"""CABS Tier 5 -- inbound settlement feed generator.

Two files, both inputs to the SETL application:

1.  ``TELCABS.SETL.CMDS.IN`` -- the inbound CMDS/RAO exchange file from the
    other RBOC. RECFM FB, LRECL 180, all DISPLAY (zoned), header / detail /
    trailer. This is the industry-format record CABSET08 reads and
    redefines three ways; the layout is declared in that program's working
    storage rather than in a copybook, so it is restated here from the
    source and the restatement is the interface contract.

2.  ``TELCABS.SETL.MPB.COUNTERPARTY`` -- the counterparty's own view of each
    meet-point circuit, as ``CABS-SETTLEMENT-RECORD`` (CABSSETL). This is
    the file that makes the L5 meet-point comparison possible: our share
    and their share have to add up, and where the two filed percentages
    disagree they will not.

Both feeds are generated from the same reference model as the usage, so the
OCNs, RAO codes and circuits resolve.

Deliberate data conditions
--------------------------
* a proportion of inbound RAO codes that resolve to no OCN (CABSET08 sends
  these to suspense; the estate's ``NORAO`` counter is what reports them)
* trailer hash totals that do not match the detail on a small number of
  files -- an inbound balancing failure is a real operational event
* exchange dates that fall in the prior year, so that CABSET08's pivot-70
  expansion and CABSET07's P2300-AGE-TEST have a year boundary to cross
* counterparty meet-point percentages that disagree with ours
"""

from __future__ import annotations

from dataclasses import dataclass
from datetime import date, timedelta
from decimal import Decimal
from pathlib import Path
from typing import Any, Dict, List, Optional, Sequence, Tuple

import gen_common as gc
from gen_common import DeterministicRandom, FixedRecordWriter, HashTotals, Layout, RecordBuilder
from gen_reference import CircuitRef, ReferenceData

__all__ = ["CMDS_LRECL", "generate_cmds_inbound", "generate_counterparty_mpb", "generate_settlement"]

#: The industry exchange record is 180 bytes, fixed, and entirely DISPLAY.
#: Restated from CABSET07/CABSET08 working storage (WS-CMDS-RECORD).
CMDS_LRECL = 180


@dataclass
class _CmdsStats:
    header: int = 0
    detail: int = 0
    trailer: int = 0
    unresolvable_rao: int = 0
    prior_year: int = 0
    trailer_mismatch: bool = False

    def as_dict(self) -> Dict[str, Any]:
        return dict(self.__dict__)


def _zoned(value: Any, digits: int, scale: int = 0, signed: bool = False) -> bytes:
    return gc.pack_zoned(value, digits, scale, signed)


def _cmds_header(from_rao: str, create: date, sequence: int) -> bytes:
    """``WS-CMDS-HEADER``: type, sending RAO, create date, file sequence."""
    body = (
        gc.to_ebcdic("HD", 2)
        + gc.to_ebcdic(from_rao, 3)
        + _zoned(gc.date_to_yyddd(create), 5)
        + _zoned(sequence, 6)
    )
    return body + gc.to_ebcdic("", CMDS_LRECL - len(body))


def _cmds_detail(
    from_rao: str,
    to_rao: str,
    exchange: date,
    ocn: str,
    period: int,
    mou: Decimal,
    amount: Decimal,
    direction: str,
) -> bytes:
    """``WS-CMDS-DETAIL``. Note MOU and amount are *zoned*, signed with a
    trailing overpunch -- not COMP-3. The exchange format predates the
    internal packed layouts and has never been renegotiated."""
    body = (
        gc.to_ebcdic("DT", 2)
        + gc.to_ebcdic(from_rao, 3)
        + gc.to_ebcdic(to_rao, 3)
        + _zoned(gc.date_to_yyddd(exchange), 5)
        + gc.to_ebcdic(ocn, 4)
        + _zoned(period, 6)
        + _zoned(mou, 17, 2, signed=True)
        + _zoned(amount, 15, 2, signed=True)
        + gc.to_ebcdic(direction, 1)
    )
    return body + gc.to_ebcdic("", CMDS_LRECL - len(body))


def _cmds_trailer(count: int, hash_amount: Decimal, hash_mou: Decimal) -> bytes:
    """``WS-CMDS-TRAILER``: the figures CABSET08 balances the detail against."""
    body = (
        gc.to_ebcdic("TR", 2)
        + _zoned(count, 9)
        + _zoned(hash_amount, 17, 2, signed=True)
        + _zoned(hash_mou, 17, 2, signed=True)
    )
    return body + gc.to_ebcdic("", CMDS_LRECL - len(body))


def generate_cmds_inbound(
    path: Path,
    rng: DeterministicRandom,
    reference: ReferenceData,
    settle_period: int,
    exchange_date: date,
    detail_count: int,
    our_rao: str = "001",
    corrupt_trailer: bool = False,
) -> Dict[str, Any]:
    """Write one inbound CMDS/RAO exchange file (HD / DT... / TR)."""
    stats = _CmdsStats()
    counterparties = [c for c in reference.carriers if c.type in ("C", "L", "W", "I")]
    raos = sorted({c.cmds_rao for c in counterparties})
    from_rao = rng.choice(raos)

    details: List[bytes] = []
    total_mou = Decimal("0.00")
    total_amt = Decimal("0.00")

    for _ in range(detail_count):
        carrier = rng.choice(counterparties)
        ocn = carrier.ocn
        to_rao = our_rao
        rao = carrier.cmds_rao
        if rng.random() < 0.03:
            # ~3% of inbound messages carry an RAO the cross-reference does
            # not know. CABSET08 counts these in WS-NORAO-CNT and suspends
            # them; the counter is on the register and nowhere else.
            rao = "%03d" % rng.randint(900, 999)
            ocn = "    "
            stats.unresolvable_rao += 1
        when = exchange_date
        if rng.random() < 0.08:
            when = date(exchange_date.year - 1, 12, rng.randint(20, 31))
            stats.prior_year += 1
        mou = rng.decimal("0.00", "9500000.00", 2)
        amount = (mou * rng.decimal("0.00050", "0.00900", 5)).quantize(Decimal("0.01"))
        direction = "R" if rng.random() < 0.55 else "P"
        details.append(
            _cmds_detail(rao if rao != carrier.cmds_rao else from_rao, to_rao, when, ocn, settle_period, mou, amount, direction)
        )
        total_mou = gc.CABS_CONTEXT.add(total_mou, mou)
        total_amt = gc.CABS_CONTEXT.add(total_amt, amount)
        stats.detail += 1

    if corrupt_trailer:
        # An inbound file whose trailer does not agree with its own detail.
        # CABSET08 must reject the file; this exists so that path is exercised.
        total_amt = gc.CABS_CONTEXT.add(total_amt, Decimal("13.37"))
        stats.trailer_mismatch = True

    with FixedRecordWriter(path, CMDS_LRECL) as writer:
        writer.write(_cmds_header(from_rao, exchange_date, 1))
        stats.header = 1
        for record in details:
            writer.write(record)
        writer.write(_cmds_trailer(len(details), total_amt, total_mou))
        stats.trailer = 1
    entry = writer.close()
    entry.update(
        {
            "dsn": "TELCABS.SETL.CMDS.IN",
            "dd": "CMDSIN",
            "copybook": "WS-CMDS-RECORD (declared in CABSET07/CABSET08 working storage)",
            "settle_period": settle_period,
            "exchange_date": exchange_date.isoformat(),
            "hash_amount": str(total_amt),
            "hash_minutes": str(total_mou),
            "conditions": stats.as_dict(),
        }
    )
    return entry


def generate_counterparty_mpb(
    path: Path,
    layout: Layout,
    rng: DeterministicRandom,
    reference: ReferenceData,
    settle_period: int,
    exchange_date: date,
    max_records: Optional[int] = None,
) -> Tuple[Dict[str, Any], int]:
    """Write the counterparty's own meet-point settlement view.

    One ``CABS-SETTLEMENT-RECORD`` per meet-point circuit, stating *their*
    percentage and *their* share. Where the two filed percentages do not sum
    to 100.00000, this file and ours will not reconcile -- which is exactly
    what the L5 comparison is for.
    """
    lrecl = layout.lrecl
    mpb_circuits: List[CircuitRef] = [c for c in reference.circuits if c.mpb]
    if max_records is not None:
        mpb_circuits = mpb_circuits[:max_records]
    disagreements = 0
    hashes = HashTotals()
    builder = RecordBuilder(layout, lrecl)

    with FixedRecordWriter(path, lrecl) as writer:
        for seq, circuit in enumerate(mpb_circuits, 1):
            our_pct = Decimal(circuit.our_pct)
            their_pct = Decimal(circuit.other_pct)
            variance = Decimal("100.00000") - (our_pct + their_pct)
            if variance != 0:
                disagreements += 1

            total_mou = rng.decimal("0.00", "4500000.00", 2)
            billable = total_mou
            rate = rng.decimal("0.00050", "0.00900", 5)
            gross = (billable * rate).quantize(Decimal("0.00001"))
            their_share = (gross * their_pct / Decimal(100)).quantize(Decimal("0.00001"))
            our_share = (gross - their_share).quantize(Decimal("0.00001"))

            b = builder.reset()
            b.set("ST-SETTLE-TYPE", "M")
            b.set("ST-COUNTERPARTY-OCN", circuit.other_ocn)
            b.set("ST-SETTLE-PERIOD", settle_period)
            b.set("ST-SEQ", seq)
            b.set("ST-TOTAL-MOU", total_mou)
            b.set("ST-BILLABLE-MOU", billable)
            b.set("ST-CAPPED-MOU", Decimal("0.00"))
            b.set("ST-RATE-APPLIED", rate)
            # Their file states their percentage as the primary one; ours is
            # the complement they believe applies.
            b.set("ST-OUR-PCT", their_pct)
            b.set("ST-THEIR-PCT", our_pct)
            b.set("ST-PCT-VARIANCE", variance)
            b.set("ST-TRUNK-GRP", circuit.trunk_grp)
            b.set("ST-CIRCUIT-ID", circuit.circuit_id)
            b.set("ST-GROSS-AMT", gross)
            b.set("ST-OUR-SHARE", their_share)
            b.set("ST-THEIR-SHARE", our_share)
            b.set("ST-NET-DUE", their_share.quantize(Decimal("0.01")))
            b.set("ST-ROUND-RESIDUE", (their_share - their_share.quantize(Decimal("0.01"))))
            b.set("ST-DIRECTION", "P")
            b.set("ST-DISPUTE-SW", "Y" if variance != 0 and rng.random() < 0.20 else "N")
            b.set("ST-EXCH-YYDDD", gc.date_to_yyddd(exchange_date))
            b.set("ST-RAO-CODE", "%03d" % rng.randint(1, 999))
            writer.write(b.build())
            hashes.add(minutes=total_mou, amount=gross, seq=seq, ocn=circuit.other_ocn)

    entry = writer.close()
    entry.update(
        {
            "dsn": "TELCABS.SETL.MPB.COUNTERPARTY",
            "dd": "MPBCPIN",
            "copybook": "CABSSETL",
            "settle_period": settle_period,
            "meet_point_circuits": len(mpb_circuits),
            "percentages_not_summing_to_100": disagreements,
        }
    )
    entry.update(hashes.as_dict())
    return entry, disagreements


def generate_settlement(
    outdir: Path | str,
    layouts: Dict[str, Layout],
    rng: DeterministicRandom,
    reference: ReferenceData,
    settle_period: int,
    exchange_date: date,
    cmds_details: int = 5_000,
    cmds_files: int = 1,
) -> List[Dict[str, Any]]:
    """Generate the whole inbound settlement feed set."""
    outdir = Path(outdir)
    outdir.mkdir(parents=True, exist_ok=True)
    entries: List[Dict[str, Any]] = []

    for generation in range(1, cmds_files + 1):
        path = outdir / gc.gdg_name("TELCABS.SETL.CMDS.IN", generation)
        entry = generate_cmds_inbound(
            path=path,
            rng=rng.substream("cmds/%d" % generation),
            reference=reference,
            settle_period=settle_period,
            exchange_date=exchange_date,
            detail_count=cmds_details,
            # One file in every eight arrives with a trailer that does not
            # agree with its own detail.
            corrupt_trailer=(generation % 8 == 0),
        )
        entry["generation"] = generation
        entries.append(entry)

    mpb_entry, _ = generate_counterparty_mpb(
        path=outdir / "TELCABS.SETL.MPB.COUNTERPARTY.dat",
        layout=layouts["CABSSETL"],
        rng=rng.substream("mpb"),
        reference=reference,
        settle_period=settle_period,
        exchange_date=exchange_date,
    )
    entries.append(mpb_entry)
    return entries
