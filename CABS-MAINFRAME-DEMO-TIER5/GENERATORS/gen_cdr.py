"""CABS Tier 5 -- wholesale access usage (CDR) generator.

Produces ``TELCABS.CABS.USAGE.RAW`` in EBCDIC, RECFM FB, LRECL 200, one
generation per day, GDG-named. This is the file DD ``RAWIN`` on CABING01
reads.

Every record is a ``CABS-CDR-RECORD`` from the frozen CABSCDR copybook. The
96-byte ``CD-VARIANT-AREA`` is written through one of the three REDEFINES
overlays according to ``CD-REC-TYPE``, exactly as the estate expects and
exactly as it will trap any program that reads the wrong one.

What the generator deliberately produces
----------------------------------------
=================================  =========  =================================
Condition                          Share      Why it is there
=================================  =========  =================================
Clean records                       ~93%      the ordinary case
Suspect edit status ('1'-'5')        ~4.5%    CABING01/CABING05 edit routing
Fatal edit status ('6'-'9')          ~2.5%    exercises the fatal edit routing
Duplicate CD-SEQ-NBR within a key    ~0.3%    CABING03 duplicate detection
Connect date in the prior cycle      ~1.5%    CABING08 carry-forward handling
Connect date after the cycle end     ~0.4%    cycle-boundary rejection
Ambiguous CD-REC-TYPE '03'/'05'      ~6%      the overlapping 88-levels
Records exactly on a band boundary  n probes  exercises band selection at an edge
=================================  =========  =================================

Band-boundary probes are recorded in ``boundary_probes.json`` next to the
usage files. A record placed at exactly 100,000.00 charged minutes sits on a
band edge, and which side of the edge an implementation puts it on is a
decision the code makes rather than one the data states. The probe list lets
the harness assert on those specific records rather than hoping a random
sample lands on an edge.
"""

from __future__ import annotations

import json
from dataclasses import dataclass, field as dc_field
from datetime import date, timedelta
from decimal import Decimal
from pathlib import Path
from typing import Any, Dict, Iterable, List, Optional, Sequence, Tuple

import gen_common as gc
from gen_common import DeterministicRandom, FixedRecordWriter, HashTotals, Layout, RecordBuilder
from gen_reference import STATES, CarrierRef, ReferenceData

__all__ = ["CycleWindow", "UsageMix", "generate_usage_day", "generate_usage"]

#: Band boundaries that fit inside CD-VC-CHG-MIN (PIC S9(07)V9(02)).
_PROBE_BOUNDARIES = [50_000, 100_000, 250_000, 500_000, 1_000_000, 2_500_000, 5_000_000]

#: Record types and the variant overlay each one is written through.
#: '03' is both CD-VOICE-MOU and CD-DATA-SVC; '05' is both CD-DATA-SVC and
#: CD-SPECIAL-ACC. The overlapping 88-levels are in the frozen copybook and
#: the generator honours them rather than tidying them away.
_REC_TYPE_WEIGHTS: List[Tuple[str, str, int]] = [
    ("01", "voice", 30),
    ("02", "voice", 22),
    ("03", "ambiguous_voice_data", 6),
    ("04", "data", 12),
    ("05", "ambiguous_data_spcl", 4),
    ("06", "special", 12),
    ("07", "special", 8),
    ("08", "voice", 6),
]

_VOICE_ELEMENTS = ["ORIGAC", "TERMAC", "LTRANS", "TANSW ", "CCLINE"]
_DATA_ELEMENTS = ["DATASV", "DATATR"]
_SPECIAL_ELEMENTS = ["UNELEM", "MPBCHG"]
_RECIP_ELEMENTS = ["RECIPC"]

_COS_VALUES = ["EF  ", "AF31", "AF21", "BE  ", "CS5 "]
_USOC_VALUES = ["TSGXX", "WALXX", "1LSPX", "DS1XX", "OC3XX", "NRDS1"]


@dataclass
class CycleWindow:
    """The billing cycle the run sits inside.

    Records dated before ``start`` are prior-cycle stragglers that CABING08
    carries forward; records dated after ``end`` are early arrivals that the
    cycle-boundary edit rejects.
    """

    start: date
    end: date

    def contains(self, when: date) -> bool:
        return self.start <= when <= self.end


@dataclass
class UsageMix:
    """Tunable proportions. Every value is parts-per-thousand of records."""

    suspect_ppt: int = 45
    fatal_ppt: int = 25
    duplicate_seq_ppt: int = 3
    prior_cycle_ppt: int = 15
    future_cycle_ppt: int = 4
    unknown_ocn_ppt: int = 2
    bad_date_ppt: int = 3
    non_numeric_ppt: int = 2
    zero_minutes_ppt: int = 4
    #: How many exact-band-boundary probe records to place per day.
    boundary_probes: int = 40


@dataclass
class _DayStats:
    records: int = 0
    voice: int = 0
    data: int = 0
    special: int = 0
    suspect: int = 0
    fatal: int = 0
    duplicates: int = 0
    prior_cycle: int = 0
    future_cycle: int = 0
    unknown_ocn: int = 0
    corrupt_packed: int = 0
    boundary_probes: int = 0
    ambiguous_type: int = 0

    def as_dict(self) -> Dict[str, int]:
        return {k: v for k, v in self.__dict__.items()}


def _hhmmss(rng: DeterministicRandom) -> int:
    """A connect time weighted to the business day, as HHMMSS."""
    hour = rng.choices(
        list(range(24)),
        [8, 5, 4, 3, 3, 5, 14, 34, 62, 78, 86, 84, 76, 82, 88, 84, 72, 60, 48, 40, 33, 26, 19, 12],
    )[0]
    return hour * 10000 + rng.randint(0, 59) * 100 + rng.randint(0, 59)


def _add_seconds(yyddd: str, hhmmss: int, seconds: int) -> Tuple[str, int]:
    """Advance a YYDDD + HHMMSS pair, rolling the date correctly."""
    base = gc.yyddd_to_date(yyddd)
    if base is None:
        return yyddd, hhmmss
    h, m, s = hhmmss // 10000, (hhmmss // 100) % 100, hhmmss % 100
    total = h * 3600 + m * 60 + s + seconds
    days, rem = divmod(total, 86400)
    end = base + timedelta(days=days)
    return gc.date_to_yyddd(end), (rem // 3600) * 10000 + ((rem % 3600) // 60) * 100 + rem % 60


def _pick_state(rng: DeterministicRandom) -> Tuple[str, List[int], List[int]]:
    return rng.choice(STATES)


def _npanxx(rng: DeterministicRandom, npas: Sequence[int]) -> int:
    return rng.choice(npas) * 1000 + rng.randint(200, 999)


def _end_office(rng: DeterministicRandom, state: str) -> str:
    consonants = "BCDFGHJKLMNPRSTVWZ"
    vowels = "AEIOU"
    return "%s%s%s%s%s%s%s" % (
        rng.choice(consonants), rng.choice(vowels), rng.choice(consonants), rng.choice(vowels),
        state, rng.choice(["MA", "DS", "CG", "XA"]), "%02d%s" % (rng.randint(0, 99), rng.choice("TWXKQ")),
    )


def generate_usage_day(
    path: Path,
    layout: Layout,
    rng: DeterministicRandom,
    reference: ReferenceData,
    day: date,
    cycle: CycleWindow,
    record_count: int,
    mix: UsageMix,
    seq_base: int = 1,
) -> Tuple[Dict[str, Any], List[Dict[str, Any]]]:
    """Write one day's usage generation.

    Returns the manifest entry for the file and the list of band-boundary
    probes placed in it.
    """
    lrecl = layout.lrecl
    billed = reference.billed_carriers()
    settlement = reference.settlement_carriers()
    circuits = reference.circuits
    stats = _DayStats()
    hashes = HashTotals()
    probes: List[Dict[str, Any]] = []
    load_yyddd = gc.date_to_yyddd(day)
    day_yyddd = gc.date_to_yyddd(day)

    # Sequence numbers are assigned per OCN in arrival order, which is what
    # CABING03 assumes when it looks for gaps and duplicates.
    seq_by_ocn: Dict[str, int] = {}
    recent_seq: List[Tuple[str, str, int]] = []  # (ocn, ban, seq) for duplicate injection

    # Choose which record ordinals get the boundary probes up front, so the
    # probe placement does not depend on how the main loop branches.
    probe_ordinals = set()
    if mix.boundary_probes and record_count > mix.boundary_probes:
        probe_ordinals = set(
            rng.substream("probe-slots").sample(range(record_count), mix.boundary_probes)
        )

    rt = rng.substream("rectype")
    rc = rng.substream("carrier")
    rd = rng.substream("detail")
    re_ = rng.substream("errors")

    rec_type_population = [(t, f) for t, f, _w in _REC_TYPE_WEIGHTS]
    rec_type_weights = [w for _t, _f, w in _REC_TYPE_WEIGHTS]
    builder = RecordBuilder(layout, lrecl)

    with FixedRecordWriter(path, lrecl) as writer:
        for ordinal in range(record_count):
            rec_type, family = rt.choices(rec_type_population, rec_type_weights)[0]
            if family == "ambiguous_voice_data":
                stats.ambiguous_type += 1
                family = "voice" if rt.random() < 0.5 else "data"
            elif family == "ambiguous_data_spcl":
                stats.ambiguous_type += 1
                family = "data" if rt.random() < 0.5 else "special"

            carrier = rc.choice(settlement if rec_type == "08" else billed)
            ban = rc.choice(carrier.bans)
            ocn = carrier.ocn

            roll = re_.randint(1, 1000)
            unknown_ocn = roll <= mix.unknown_ocn_ppt
            if unknown_ocn:
                ocn = "ZZ%02d" % re_.randint(1, 99)
                stats.unknown_ocn += 1

            seq = seq_by_ocn.get(carrier.ocn, seq_base)
            seq_by_ocn[carrier.ocn] = seq + 1
            if recent_seq and re_.randint(1, 1000) <= mix.duplicate_seq_ppt:
                dup_ocn, dup_ban, dup_seq = re_.choice(recent_seq[-200:])
                ocn, ban, seq = dup_ocn, dup_ban, dup_seq
                stats.duplicates += 1
            else:
                recent_seq.append((ocn, ban, seq))
                if len(recent_seq) > 400:
                    del recent_seq[:200]

            # --- date placement -------------------------------------------
            conn_yyddd = day_yyddd
            date_roll = re_.randint(1, 1000)
            if date_roll <= mix.prior_cycle_ppt:
                stragglers = re_.randint(1, 20)
                conn_yyddd = gc.date_to_yyddd(cycle.start - timedelta(days=stragglers))
                stats.prior_cycle += 1
            elif date_roll <= mix.prior_cycle_ppt + mix.future_cycle_ppt:
                conn_yyddd = gc.date_to_yyddd(cycle.end + timedelta(days=re_.randint(1, 5)))
                stats.future_cycle += 1

            conn_time = _hhmmss(rd)
            duration = rd.choices(
                [rd.randint(6, 120), rd.randint(120, 900), rd.randint(900, 5400), rd.randint(5400, 28800)],
                [55, 30, 12, 3],
            )[0]
            disc_yyddd, disc_time = _add_seconds(conn_yyddd, conn_time, duration)

            bad_date = re_.randint(1, 1000) <= mix.bad_date_ppt

            # --- edit status ----------------------------------------------
            status_roll = re_.randint(1, 1000)
            if status_roll <= mix.fatal_ppt:
                edit_status = str(re_.randint(6, 9))
                stats.fatal += 1
            elif status_roll <= mix.fatal_ppt + mix.suspect_ppt:
                edit_status = str(re_.randint(1, 5))
                stats.suspect += 1
            else:
                edit_status = "0"

            b = builder.reset()
            b.set("CD-OCN", ocn)
            b.set("CD-BAN", ban)
            b.set("CD-SEQ-NBR", seq)
            b.set("CD-REC-TYPE", rec_type)
            b.set("CD-CONN-YY", int(conn_yyddd[:2]))
            b.set("CD-CONN-DDD", 999 if bad_date else int(conn_yyddd[2:]))
            b.set("CD-CONN-HHMMSS", conn_time)
            b.set("CD-DISC-YYDDD", disc_yyddd)
            b.set("CD-DISC-HHMMSS", disc_time)
            b.set("CD-SRC-SYSTEM", "EMI1")
            b.set("CD-LOAD-YYDDD", load_yyddd)
            b.set("CD-EDIT-STATUS", edit_status)

            state_cd, latas, npas = _pick_state(rd)
            juris = rd.choices(["I", "S", "L", "X"], [52, 33, 12, 3])[0]
            b.set("CD-JURIS-CD", juris)

            minutes = Decimal(0)
            probe = None

            if family == "voice":
                elem = (
                    rd.choice(_RECIP_ELEMENTS)
                    if rec_type == "08"
                    else rd.choice(_VOICE_ELEMENTS)
                )
                b.set("CD-USAGE-TYPE", rd.choice("MVR"))
                b.set("CD-RATE-ELEM", elem)
                b.set("CD-VC-ORIG-NPANXX", _npanxx(rd, npas))
                b.set("CD-VC-TERM-NPANXX", _npanxx(rd, npas))
                b.set("CD-VC-ORIG-LATA", rd.choice(latas))
                b.set("CD-VC-TERM-LATA", rd.choice(latas) if juris != "L" else rd.choice(latas))
                conv = (Decimal(duration) / Decimal(60)).quantize(Decimal("0.01"))
                charged = conv.to_integral_value(rounding="ROUND_CEILING").quantize(Decimal("0.01"))
                if ordinal in probe_ordinals:
                    boundary = rd.choice(_PROBE_BOUNDARIES)
                    charged = Decimal(boundary).quantize(Decimal("0.01"))
                    conv = charged
                    elem = rd.choice(["ORIGAC", "TERMAC", "TANSW "])
                    b.set("CD-RATE-ELEM", elem)
                    probe = {
                        "day": day.isoformat(),
                        "ocn": ocn,
                        "ban": ban,
                        "seq": seq,
                        "rate_elem": elem.strip(),
                        "boundary_qty": str(charged),
                        "expected_band_under_correct_logic": "the band whose FROM equals this quantity",
                                            }
                    probes.append(probe)
                    stats.boundary_probes += 1
                if re_.randint(1, 1000) <= mix.zero_minutes_ppt:
                    conv = Decimal("0.00")
                    charged = Decimal("0.00")
                b.set("CD-VC-CONV-MIN", conv)
                b.set("CD-VC-CHG-MIN", charged)
                tandem = "Y" if (elem.strip() == "TANSW" or rd.random() < 0.30) else "N"
                b.set("CD-VC-TANDEM-IND", tandem)
                b.set("CD-VC-TRUNK-GRP", rd.choice(circuits).trunk_grp if circuits else "T0001   ")
                b.set("CD-VC-CIC", carrier.cic)
                b.set("CD-VC-END-OFFICE", _end_office(rd, state_cd))
                minutes = charged
                stats.voice += 1

            elif family == "data":
                circuit = rd.choice(circuits)
                b.set("CD-USAGE-TYPE", rd.choice("DFP"))
                b.set("CD-RATE-ELEM", rd.choice(_DATA_ELEMENTS))
                b.set("CD-DT-CIRCUIT-ID", circuit.circuit_id)
                b.set("CD-DT-BANDWIDTH", rd.choice([1544, 3088, 6176, 44736, 155520, 622080]))
                b.set("CD-DT-OCTETS-IN", rd.randint(0, 9_000_000_000_000))
                b.set("CD-DT-OCTETS-OUT", rd.randint(0, 9_000_000_000_000))
                b.set("CD-DT-CoS", rd.choice(_COS_VALUES))
                b.set("CD-DT-A-LOC", circuit.a_clli)
                b.set("CD-DT-Z-LOC", circuit.z_clli)
                stats.data += 1

            else:  # special access
                circuit = rd.choice(circuits)
                b.set("CD-USAGE-TYPE", rd.choice("SAU"))
                b.set("CD-RATE-ELEM", rd.choice(_SPECIAL_ELEMENTS))
                b.set("CD-SP-CIRCUIT-ID", circuit.circuit_id)
                b.set("CD-SP-USOC", rd.choice(_USOC_VALUES))
                b.set("CD-SP-QTY", rd.randint(1, 240))
                b.set("CD-SP-TERM-MONTHS", rd.choice([12, 24, 36, 60]))
                b.set("CD-SP-MPB-IND", "Y" if circuit.mpb else "N")
                b.set("CD-SP-MPB-PCT", Decimal(circuit.our_pct))
                b.set("CD-SP-OTHER-LEC", circuit.other_ocn)
                stats.special += 1

            record = b.build()

            # A small number of records carry genuinely corrupt packed data --
            # the S0C7 that CABING01's edit suite exists to intercept.
            if re_.randint(1, 1000) <= mix.non_numeric_ppt:
                f = layout.field("CD-SEQ-NBR")
                record = (
                    record[: f.offset]
                    + bytes([0x4B, 0x5C]) + record[f.offset + 2 : f.offset + f.length]
                    + record[f.offset + f.length :]
                )
                stats.corrupt_packed += 1

            writer.write(record)
            hashes.add(minutes=minutes, amount=0, seq=seq, ocn=ocn)
            stats.records += 1

    entry = writer.close()
    entry.update(
        {
            "dsn": "TELCABS.CABS.USAGE.RAW",
            "dd": "RAWIN",
            "copybook": "CABSCDR",
            "cycle_date": day.isoformat(),
            "cycle_yyddd": day_yyddd,
        }
    )
    entry.update(hashes.as_dict())
    entry["conditions"] = stats.as_dict()
    return entry, probes


def generate_usage(
    outdir: Path | str,
    layout: Layout,
    rng: DeterministicRandom,
    reference: ReferenceData,
    days: Sequence[date],
    cycle: CycleWindow,
    records_per_day: int,
    mix: Optional[UsageMix] = None,
    gdg_base: str = "TELCABS.CABS.USAGE.RAW",
) -> Tuple[List[Dict[str, Any]], List[Dict[str, Any]]]:
    """Generate one GDG generation per day."""
    outdir = Path(outdir)
    outdir.mkdir(parents=True, exist_ok=True)
    mix = mix or UsageMix()
    entries: List[Dict[str, Any]] = []
    all_probes: List[Dict[str, Any]] = []
    for generation, day in enumerate(days, 1):
        path = outdir / gc.gdg_name(gdg_base, generation)
        entry, probes = generate_usage_day(
            path=path,
            layout=layout,
            rng=rng.substream("usage/%s" % day.isoformat()),
            reference=reference,
            day=day,
            cycle=cycle,
            record_count=records_per_day,
            mix=mix,
            seq_base=1 + (generation - 1) * 1_000_000,
        )
        entry["generation"] = generation
        entries.append(entry)
        all_probes.extend(probes)

    probe_path = outdir / "boundary_probes.json"
    probe_path.write_text(json.dumps(all_probes, indent=1), encoding="utf-8")
    return entries, all_probes
