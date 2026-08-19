"""CABS Tier 5 -- reference (master file) generator.

Produces the four VSAM-bound master files the batch estate reads:

===========  ==========================  ==========  ==========================
DD           DSN                         Copybook    Approx. records
===========  ==========================  ==========  ==========================
CARRMST      TELCABS.CABS.CARRIER        CABSCARR    450 OCNs
RATEMST      TELCABS.CABS.RATE           CABSRATE    1,200 rate records
FCTRMST      TELCABS.CABS.FACTOR         CABSFCTR    ~2,400 PIU/PLU filings
CIRCMST      TELCABS.CABS.CIRCUIT        CABSCIRC    8,000 circuits/trunk groups
===========  ==========================  ==========  ==========================

Reference data is *not* scaled by ``--profile``. A carrier's estate has
roughly the same number of OCNs and circuits whether you are looking at one
day or a hundred million records; only usage scales. The profile is accepted
and recorded so the manifest is self-describing, and STRESS/TARGET widen the
circuit inventory modestly so that per-circuit usage stays plausible.

Deliberate data conditions
--------------------------
These are properties of the *data*, not seeded code defects. They exist so
the estate's edit suite, its jurisdictional logic and its settlement
processes have something real to react to:

* meet-point percentage pairs that do not sum to 100.00000
* PIU/PLU quarterly restatement filings, some with a window that crosses a
  year boundary and some with a zero prior factor
* banded rates whose boundaries are round tariff numbers, exported so the
  CDR generator can place usage exactly on them
* five-decimal fractional-cent rates and a mixture of RT-ROUND-RULE values
  on rate records that are otherwise siblings
* a small proportion of expired, future-dated and disputed records
"""

from __future__ import annotations

import json
from dataclasses import asdict, dataclass
from datetime import date, timedelta
from decimal import Decimal
from pathlib import Path
from typing import Any, Dict, List, Optional, Sequence, Tuple

import gen_common as gc
from gen_common import DeterministicRandom, FixedRecordWriter, Layout, RecordBuilder

__all__ = [
    "STATES",
    "RATE_ELEMENTS",
    "CarrierRef",
    "RateRef",
    "CircuitRef",
    "ReferenceData",
    "generate_reference",
]

# ---------------------------------------------------------------------------
# Geography
# ---------------------------------------------------------------------------

#: (state, [LATA], [NPA]) -- LATA numbers and NPAs are the real ones, which
#: matters because the estate's jurisdiction tables are keyed on them.
STATES: List[Tuple[str, List[int], List[int]]] = [
    ("NY", [132, 133, 134], [212, 315, 516, 585, 607, 631, 716, 718, 845, 914]),
    ("NJ", [224], [201, 609, 732, 856, 908, 973]),
    ("PA", [226, 228, 234], [215, 412, 570, 610, 717, 724, 814]),
    ("MD", [232, 236], [301, 410, 443]),
    ("VA", [236, 240, 244, 246, 252], [434, 540, 703, 757, 804]),
    ("NC", [320, 324, 326], [252, 336, 704, 828, 910, 919]),
    ("SC", [328], [803, 843, 864]),
    ("GA", [420, 424, 426, 428], [229, 404, 478, 706, 770, 912]),
    ("FL", [452, 458, 460, 464, 470], [239, 305, 352, 407, 561, 727, 813, 850, 904, 954]),
    ("AL", [476, 477], [205, 251, 256, 334]),
    ("TN", [468, 470, 486], [423, 615, 731, 865, 901, 931]),
    ("KY", [462, 464, 466], [270, 502, 606, 859]),
    ("OH", [320, 322, 324, 325], [216, 330, 419, 440, 513, 614, 740, 937]),
    ("MI", [340, 344, 346, 348], [231, 248, 313, 517, 586, 616, 734, 810, 906, 989]),
    ("IN", [330, 332, 334, 336, 338], [219, 260, 317, 574, 765, 812]),
    ("IL", [358, 360, 362, 364, 366], [217, 309, 312, 618, 630, 708, 773, 815, 847]),
    ("WI", [350, 352, 354, 356], [262, 414, 608, 715, 920]),
    ("MN", [620, 624, 626, 628], [218, 320, 507, 612, 651, 763, 952]),
    ("MO", [520, 521, 522, 524, 526], [314, 417, 573, 636, 660, 816]),
    ("TX", [552, 554, 556, 558, 560, 562, 564, 566], [210, 214, 254, 281, 409, 512, 713, 806, 817, 903, 915, 956]),
    ("CO", [656, 658], [303, 719, 720, 970]),
    ("AZ", [666, 668], [480, 520, 602, 623, 928]),
    ("CA", [720, 722, 724, 726, 728, 730, 732, 736, 738, 740], [209, 213, 310, 408, 415, 510, 530, 559, 619, 626, 707, 714, 805, 818, 831, 909, 916, 925, 949]),
    ("WA", [674, 676], [206, 253, 360, 425, 509]),
    ("OR", [672], [503, 541, 971]),
    ("MA", [128], [413, 508, 617, 781, 978]),
    ("CT", [920, 921], [203, 860]),
    ("LA", [486, 490, 492], [225, 318, 337, 504, 985]),
    ("OK", [536, 538], [405, 580, 918]),
    ("KS", [532, 534], [316, 620, 785, 913]),
]

#: The five switched-access rate elements CABRAT03 rates, plus the data,
#: settlement and unbundled elements the other rating programs handle.
RATE_ELEMENTS: List[Tuple[str, str]] = [
    ("ORIGAC", "ORIGINATING ACCESS"),
    ("TERMAC", "TERMINATING ACCESS"),
    ("LTRANS", "LOCAL TRANSPORT"),
    ("TANSW ", "TANDEM SWITCHING"),
    ("CCLINE", "CARRIER COMMON LINE"),
    ("DATASV", "DATA SERVICE"),
    ("DATATR", "DATA TRANSPORT"),
    ("RECIPC", "RECIPROCAL COMPENSATION"),
    ("UNELEM", "UNBUNDLED ELEMENT"),
    ("MPBCHG", "MEET POINT BILLED CHARGE"),
]

#: Band boundaries used in access tariffs. Round numbers, because a volume
#: commitment is negotiated to hit them exactly -- which is why real traffic
#: clusters on the edges and a uniform sample does not.
BAND_BOUNDARIES: List[int] = [
    0, 50_000, 100_000, 250_000, 500_000, 1_000_000,
    2_500_000, 5_000_000, 10_000_000, 25_000_000, 999_999_999,
]

_IXC_NAMES = [
    "AMERIDIAL", "TRANSCONTINENTAL", "NATIONAL FIBER", "MERIDIAN LONG DISTANCE",
    "PACIFIC GATEWAY", "ATLANTIC TRUNK", "GLOBAL CROSSPOINT", "UNITED CARRIER",
    "CENTURION NETWORK", "APEX TELECOM", "SUMMIT COMMUNICATIONS", "VANGUARD IXC",
]
_CLEC_NAMES = [
    "METRO ACCESS", "CITYLINK", "URBAN FIBER", "COMPETITIVE VOICE", "NEXTGEN LOCAL",
    "ALTERNATE ACCESS", "PIONEER LOCAL", "BROADREACH", "CLEARPATH", "FIRSTMILE",
    "OPENLINE", "TELEPORT", "DIGITAL EXCHANGE", "LOCAL CHOICE", "STARGATE COMM",
]
_ILEC_NAMES = [
    "VALLEY TELEPHONE", "RURAL BELL", "HERITAGE TELEPHONE", "FARMERS MUTUAL TEL",
    "PIONEER TELEPHONE", "TRI-COUNTY TELEPHONE", "LAKESHORE TELEPHONE",
    "MOUNTAIN STATES TEL", "PRAIRIE TELEPHONE", "GULF COAST TELEPHONE",
]
_WIRELESS_NAMES = [
    "CELLULAR ONE", "MOBILE NETWORKS", "AIRWAVE WIRELESS", "PCS PARTNERS",
    "SPECTRUM MOBILE", "COASTAL CELLULAR", "HIGHLAND WIRELESS", "OMNIPOINT MOBILE",
]
_RESELLER_NAMES = [
    "BUDGET DIAL", "VALUE LONG DISTANCE", "PREPAID CONNECT", "SAVER TELECOM",
    "DISCOUNT DIALING", "EASYCALL", "THRIFTLINE", "PENNYWISE COMM",
]
_NAME_SUFFIXES = ["INC", "LLC", "CORP", "LP", "CO", "GROUP INC", "HOLDINGS LLC", "OF AMERICA INC"]

_CARRIER_MIX: List[Tuple[str, int, List[str]]] = [
    ("I", 40, _IXC_NAMES),
    ("C", 180, _CLEC_NAMES),
    ("L", 60, _ILEC_NAMES),
    ("W", 70, _WIRELESS_NAMES),
    ("R", 100, _RESELLER_NAMES),
]


# ---------------------------------------------------------------------------
# In-memory reference model (the sidecar the usage generator consumes)
# ---------------------------------------------------------------------------


@dataclass
class CarrierRef:
    ocn: str
    name: str
    acna: str
    cic: int
    type: str
    bill_cycle: int
    cmds_rao: str
    default_piu: str
    default_plu: str
    recip_rate: str
    isp_cap_mou: str
    mpb_eligible: bool
    active: bool
    bans: List[str]


@dataclass
class RateRef:
    tariff_cd: str
    rate_elem: str
    juris_cd: str
    state_cd: str
    eff_yyddd: str
    initial_rate: str
    addl_rate: str
    round_rule: str
    round_pos: int
    bands: List[Tuple[int, int, str]]


@dataclass
class CircuitRef:
    circuit_id: str
    trunk_grp: str
    ocn: str
    ban: str
    service_type: str
    a_clli: str
    z_clli: str
    a_lata: int
    z_lata: int
    state_cd: str
    mpb: bool
    our_pct: str
    other_ocn: str
    other_pct: str


@dataclass
class ReferenceData:
    """Everything the usage and settlement generators need to stay
    referentially consistent with the master files."""

    carriers: List[CarrierRef]
    rates: List[RateRef]
    circuits: List[CircuitRef]
    band_boundaries: List[int]
    states: List[str]

    def carriers_by_type(self, type_code: str) -> List[CarrierRef]:
        return [c for c in self.carriers if c.type == type_code]

    def billed_carriers(self) -> List[CarrierRef]:
        """CR-BILLED-PARTY: IXC, CLEC or reseller."""
        return [c for c in self.carriers if c.type in ("I", "C", "R")]

    def settlement_carriers(self) -> List[CarrierRef]:
        """CR-SETTLEMENT-PTY: CLEC, ILEC or wireless."""
        return [c for c in self.carriers if c.type in ("C", "L", "W")]

    def to_json(self) -> Dict[str, Any]:
        return {
            "carriers": [asdict(c) for c in self.carriers],
            "rates": [asdict(r) for r in self.rates],
            "circuits": [asdict(c) for c in self.circuits],
            "band_boundaries": self.band_boundaries,
            "states": self.states,
        }

    @classmethod
    def from_json(cls, payload: Dict[str, Any]) -> "ReferenceData":
        return cls(
            carriers=[CarrierRef(**c) for c in payload["carriers"]],
            rates=[
                RateRef(**{**r, "bands": [tuple(b) for b in r["bands"]]}) for r in payload["rates"]
            ],
            circuits=[CircuitRef(**c) for c in payload["circuits"]],
            band_boundaries=payload["band_boundaries"],
            states=payload["states"],
        )


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _ocn_pool(rng: DeterministicRandom, count: int) -> List[str]:
    """Unique four-character OCNs: mostly numeric, some alphanumeric.

    Real OCNs are four characters and mix the two forms; the estate's own
    hash routine (CABHASH) treats them differently, which is why the mix
    matters for control-total reconciliation."""
    pool: List[str] = []
    seen = set()
    letters = "ABCDEFGHJKLMNPQRSTUVWXYZ"
    while len(pool) < count:
        if rng.random() < 0.72:
            candidate = "%04d" % rng.randint(100, 9999)
        else:
            candidate = "%s%03d" % (rng.choice(letters), rng.randint(1, 999))
        if candidate in seen:
            continue
        seen.add(candidate)
        pool.append(candidate)
    return pool


def _make_ban(rng: DeterministicRandom, npa: int, ocn: str) -> str:
    """A 13-character Billing Account Number: NPA + service letter + 9 digits."""
    return "%03d%s%09d" % (npa, rng.choice("ABCDEFGHJKLMNP"), rng.randint(1, 999_999_999))


def _make_clli(rng: DeterministicRandom, state: str) -> str:
    """An 11-character CLLI: 4-char place, 2-char state, 2-char building,
    3-char entity."""
    consonants = "BCDFGHJKLMNPRSTVWZ"
    vowels = "AEIOU"
    place = (
        rng.choice(consonants) + rng.choice(vowels) + rng.choice(consonants) + rng.choice(vowels)
    )
    return "%s%s%s%s" % (
        place,
        state,
        rng.choice(["MA", "DS", "CG", "XA", "RS"]),
        "%02d%s" % (rng.randint(0, 99), rng.choice("TWXKQ")),
    )


def _quarter_start_yyddd(cycle: date, quarters_back: int) -> str:
    """YYDDD of the start of a calendar quarter, counted back from ``cycle``."""
    quarter = (cycle.month - 1) // 3
    year = cycle.year
    quarter -= quarters_back
    while quarter < 0:
        quarter += 4
        year -= 1
    return gc.date_to_yyddd(date(year, quarter * 3 + 1, 1))


# ---------------------------------------------------------------------------
# Carrier master
# ---------------------------------------------------------------------------


def _build_carriers(rng: DeterministicRandom, cycle: date, bans_per_carrier: int) -> List[CarrierRef]:
    total = sum(count for _, count, _ in _CARRIER_MIX)
    ocns = _ocn_pool(rng, total)
    carriers: List[CarrierRef] = []
    used_names: set = set()
    idx = 0
    for type_code, count, name_pool in _CARRIER_MIX:
        for n in range(count):
            ocn = ocns[idx]
            idx += 1
            base = name_pool[n % len(name_pool)]
            name = "%s %s" % (base, rng.choice(_NAME_SUFFIXES))
            if name in used_names:
                name = "%s %s %d" % (base, rng.choice(_NAME_SUFFIXES), n)
            used_names.add(name)

            npa = rng.choice(rng.choice(STATES)[2])
            bans = [_make_ban(rng, npa, ocn) for _ in range(bans_per_carrier)]

            if type_code == "I":
                piu, plu = rng.decimal("70.00000", "100.00000", 5), rng.decimal("0.00000", "20.00000", 5)
                cic = rng.randint(100, 9999)
            elif type_code == "W":
                piu, plu = rng.decimal("20.00000", "60.00000", 5), rng.decimal("30.00000", "80.00000", 5)
                cic = 0
            elif type_code == "L":
                piu, plu = rng.decimal("5.00000", "35.00000", 5), rng.decimal("60.00000", "95.00000", 5)
                cic = 0
            else:
                piu, plu = rng.decimal("40.00000", "90.00000", 5), rng.decimal("5.00000", "50.00000", 5)
                cic = rng.randint(100, 9999) if type_code == "R" else 0

            carriers.append(
                CarrierRef(
                    ocn=ocn,
                    name=name[:40],
                    acna="".join(rng.choice("ABCDEFGHJKLMNPQRSTUVWXYZ") for _ in range(3)),
                    cic=cic,
                    type=type_code,
                    bill_cycle=rng.randint(1, 20),
                    cmds_rao="%03d" % rng.randint(1, 999),
                    default_piu=str(piu),
                    default_plu=str(plu),
                    recip_rate=str(rng.decimal("0.00010", "0.00950", 5)),
                    isp_cap_mou=str(rng.randint(0, 50_000_000) if type_code == "C" else 0),
                    mpb_eligible=type_code in ("C", "L") and rng.random() < 0.75,
                    # ~4% of the master is inactive or expired: the edit suite
                    # and CABOCNVL exist to catch usage that still arrives for
                    # these OCNs, and it does arrive.
                    active=rng.random() > 0.04,
                    bans=bans,
                )
            )
    return carriers


def _write_carriers(path: Path, layout: Layout, carriers: Sequence[CarrierRef], cycle: date) -> Dict[str, Any]:
    lrecl = layout.lrecl
    hashes = gc.HashTotals()
    with FixedRecordWriter(path, lrecl) as writer:
        for c in carriers:
            b = RecordBuilder(layout, lrecl)
            b.set("CR-OCN", c.ocn)
            b.set("CR-NAME", c.name)
            b.set("CR-ACNA", c.acna)
            b.set("CR-CIC", c.cic)
            b.set("CR-TYPE", c.type)
            b.set("CR-BILL-CYCLE", c.bill_cycle)
            b.set("CR-BILL-MEDIA", "P" if c.type in ("I", "C") else "E")
            b.set("CR-CURRENCY", "USD")
            b.set("CR-TERMS-DAYS", 30)
            b.set("CR-CREDIT-LIMIT", Decimal("5000000.00"))
            b.set("CR-DEFAULT-PIU", Decimal(c.default_piu))
            b.set("CR-DEFAULT-PLU", Decimal(c.default_plu))
            b.set("CR-FACTOR-SRC", "C" if c.type == "I" else "T")
            b.set("CR-RECIP-COMP-ELIG", "Y" if c.type in ("C", "L", "W") else "N")
            b.set("CR-RECIP-RATE", Decimal(c.recip_rate))
            b.set("CR-ISP-CAP-MOU", Decimal(c.isp_cap_mou))
            b.set("CR-CMDS-RAO", c.cmds_rao)
            b.set("CR-MPB-ELIGIBLE", "Y" if c.mpb_eligible else "N")
            b.set("CR-ACTIVE-SW", "Y" if c.active else "N")
            b.set("CR-EFF-YYDDD", gc.date_to_yyddd(date(cycle.year - 6, 1, 1)))
            b.set("CR-EXP-YYDDD", "99365" if c.active else gc.date_to_yyddd(cycle - timedelta(days=90)))
            writer.write(b.build())
            hashes.add(ocn=c.ocn)
    result = writer.close()
    result.update({"dsn": "TELCABS.CABS.CARRIER", "dd": "CARRMST", "copybook": "CABSCARR"})
    result.update(hashes.as_dict())
    return result


# ---------------------------------------------------------------------------
# Rate table
# ---------------------------------------------------------------------------


def _build_rates(rng: DeterministicRandom, cycle: date, target_count: int) -> List[RateRef]:
    rates: List[RateRef] = []
    state_codes = [s[0] for s in STATES]
    tariffs_interstate = ["FCC1", "FCC2"]
    round_rules = ["U", "E", "T", "C"]

    # Interstate: one record per element per effective date, no state.
    eff_dates = [
        gc.date_to_yyddd(date(cycle.year - 2, 1, 1)),
        gc.date_to_yyddd(date(cycle.year - 1, 7, 1)),
        gc.date_to_yyddd(date(cycle.year, 1, 1)),
    ]
    for tariff in tariffs_interstate:
        for elem, _ in RATE_ELEMENTS:
            for eff in eff_dates:
                rates.append(_one_rate(rng, tariff, elem, "I", "  ", eff, round_rules))

    # Intrastate and local: per state, per element, per effective date.
    # De-duplicated on the VSAM key as we go -- a KSDS cannot hold two records
    # with the same key and IDCAMS REPRO would reject the load.
    unique: Dict[Tuple[str, str, str, str, str], RateRef] = {}
    for r in rates:
        unique[(r.tariff_cd, r.rate_elem, r.juris_cd, r.state_cd, r.eff_yyddd)] = r

    attempts = 0
    while len(unique) < target_count and attempts < target_count * 50:
        attempts += 1
        state = rng.choice(state_codes)
        elem, _ = rng.choice(RATE_ELEMENTS)
        juris = rng.choices(["S", "L"], [7, 3])[0]
        tariff = "%s%02d" % (state, rng.randint(1, 3))
        eff = rng.choice(eff_dates)
        r = _one_rate(rng, tariff, elem, juris, state, eff, round_rules)
        unique.setdefault((r.tariff_cd, r.rate_elem, r.juris_cd, r.state_cd, r.eff_yyddd), r)

    return sorted(
        unique.values(),
        key=lambda r: (r.tariff_cd, r.rate_elem, r.juris_cd, r.state_cd, r.eff_yyddd),
    )


def _one_rate(
    rng: DeterministicRandom,
    tariff: str,
    elem: str,
    juris: str,
    state: str,
    eff: str,
    round_rules: Sequence[str],
) -> RateRef:
    """One rate record, with a banded rate structure on the volume-sensitive
    elements."""
    if elem in ("ORIGAC", "TERMAC"):
        initial = rng.decimal("0.00150", "0.02500", 5)
    elif elem == "TANSW ":
        initial = rng.decimal("0.00030", "0.00480", 5)
    elif elem == "CCLINE":
        initial = rng.decimal("0.00500", "0.04500", 5)
    elif elem in ("DATASV", "DATATR"):
        initial = rng.decimal("12.50000", "985.00000", 5)
    elif elem == "RECIPC":
        initial = rng.decimal("0.00010", "0.00700", 5)
    else:
        initial = rng.decimal("0.00050", "0.09999", 5)

    banded = elem in ("ORIGAC", "TERMAC", "LTRANS", "TANSW ", "CCLINE") and rng.random() < 0.55
    bands: List[Tuple[int, int, str]] = []
    if banded:
        n = rng.randint(3, 8)
        cuts = sorted(rng.sample(BAND_BOUNDARIES[1:-1], min(n - 1, len(BAND_BOUNDARIES) - 2)))
        edges = [0] + cuts + [999_999_999]
        rate = initial
        for i in range(len(edges) - 1):
            bands.append((edges[i], edges[i + 1] - 1 if i + 1 < len(edges) - 1 else edges[i + 1], str(rate)))
            # Volume discount: each band shades the rate down.
            rate = (rate * Decimal("0.94")).quantize(Decimal("0.00001"))
    else:
        bands.append((0, 999_999_999, str(initial)))

    return RateRef(
        tariff_cd=tariff,
        rate_elem=elem,
        juris_cd=juris,
        state_cd=state if state.strip() else "  ",
        eff_yyddd=eff,
        initial_rate=str(initial),
        addl_rate=str((initial * Decimal("0.85")).quantize(Decimal("0.00001"))),
        # The rounding rule is per-record, not global. Sibling records for the
        # same element in different states can and do disagree -- CONVENTIONS.md
        # says rounding is driven by RT-ROUND-RULE, not by the verb in the code.
        round_rule=rng.choices(round_rules, [55, 25, 15, 5])[0],
        round_pos=rng.choices([2, 5], [80, 20])[0],
        bands=bands,
    )


def _write_rates(path: Path, layout: Layout, rates: Sequence[RateRef]) -> Dict[str, Any]:
    # Written at the maximum ODO length. IDCAMS DEFINE uses
    # RECORDSIZE(avg max) and REPRO loads from this fixed image; RT-BAND-CNT
    # governs how many band slots are meaningful and the rest are binary zero.
    lrecl = layout.computed_length
    with FixedRecordWriter(path, lrecl) as writer:
        for r in rates:
            b = RecordBuilder(layout, lrecl, fill=0x00)
            b.set("RT-TARIFF-CD", r.tariff_cd)
            b.set("RT-RATE-ELEM", r.rate_elem)
            b.set("RT-JURIS-CD", r.juris_cd)
            b.set("RT-STATE-CD", r.state_cd)
            b.set("RT-EFF-YYDDD", r.eff_yyddd)
            b.set("RT-INITIAL-RATE", Decimal(r.initial_rate))
            b.set("RT-ADDL-RATE", Decimal(r.addl_rate))
            b.set("RT-SETUP-CHG", Decimal("0.00000"))
            b.set("RT-MIN-CHG", Decimal("0.00"))
            b.set("RT-MAX-CHG", Decimal("99999999.99"))
            b.set("RT-ROUND-RULE", r.round_rule)
            b.set("RT-ROUND-POS", r.round_pos)
            b.set("RT-INIT-PERIOD", 60)
            b.set("RT-ADDL-PERIOD", 60)
            b.set("RT-DISC-ELIGIBLE", "Y")
            b.set("RT-EXP-YYDDD", "99365")
            b.set("RT-BAND-CNT", len(r.bands))
            for i, (frm, thru, rate) in enumerate(r.bands, 1):
                b.set("RT-BAND-FROM", frm, index=i)
                b.set("RT-BAND-THRU", thru, index=i)
                b.set("RT-BAND-RATE", Decimal(rate), index=i)
                b.set("RT-BAND-PCT", Decimal("100.00000"), index=i)
            writer.write(b.build())
    result = writer.close()
    result.update(
        {
            "dsn": "TELCABS.CABS.RATE",
            "dd": "RATEMST",
            "copybook": "CABSRATE",
            "note": "written at maximum OCCURS DEPENDING ON length; RT-BAND-CNT governs",
        }
    )
    return result


# ---------------------------------------------------------------------------
# PIU / PLU factors
# ---------------------------------------------------------------------------


def _write_factors(
    path: Path,
    layout: Layout,
    rng: DeterministicRandom,
    carriers: Sequence[CarrierRef],
    cycle: date,
) -> Tuple[Dict[str, Any], int]:
    """Quarterly PIU/PLU filings, including a restatement set.

    Factors arrive quarterly and are applied retroactively to the prior
    quarter (CABSFCTR header). The restatement subset is what CABJUR07
    processes.
    """
    lrecl = layout.lrecl
    restatement_count = 0
    cur_q = _quarter_start_yyddd(cycle, 0)
    prev_q = _quarter_start_yyddd(cycle, 1)
    prev_q2 = _quarter_start_yyddd(cycle, 2)

    with FixedRecordWriter(path, lrecl) as writer:
        for c in carriers:
            if c.type not in ("I", "C", "R", "W"):
                continue
            states = rng.sample(STATES, rng.randint(1, 4))
            for state_cd, latas, _ in states:
                lata = rng.choice(latas)
                for eff, is_current in ((prev_q2, False), (prev_q, False), (cur_q, True)):
                    piu = rng.decimal("0.00000", "100.00000", 5)
                    plu = rng.decimal("0.00000", "100.00000", 5)
                    restate = is_current and rng.random() < 0.35
                    b = RecordBuilder(layout, lrecl)
                    b.set("FC-OCN", c.ocn)
                    b.set("FC-STATE-CD", state_cd)
                    b.set("FC-LATA", lata)
                    b.set("FC-EFF-YYDDD", eff)
                    b.set("FC-PIU", piu)
                    b.set("FC-PLU", plu)
                    b.set("FC-PSU", rng.decimal("0.00000", "100.00000", 5))
                    b.set(
                        "FC-SOURCE",
                        rng.choices(["C", "S", "D", "X"], [70, 15, 12, 3])[0],
                    )
                    b.set("FC-RESTATE-SW", "Y" if restate else "N")
                    if restate:
                        restatement_count += 1
                        # 1 in 4 restatement windows crosses a year boundary,
                        # so the window arithmetic has a year end to cross.
                        if rng.random() < 0.25:
                            from_yyddd = gc.date_to_yyddd(date(cycle.year - 1, 10, 1))
                            thru_yyddd = gc.date_to_yyddd(date(cycle.year, 1, 15))
                        else:
                            from_yyddd = prev_q
                            thru_yyddd = gc.yyddd_add_days(prev_q, 89) or prev_q
                        b.set("FC-RESTATE-FROM-YYDDD", from_yyddd)
                        b.set("FC-RESTATE-THRU-YYDDD", thru_yyddd)
                        # 1 in 8 restatements carries no prior factor at all,
                        # so the "no prior basis" path has something to run on.
                        if rng.random() < 0.125:
                            b.set("FC-PRIOR-PIU", Decimal("0.00000"))
                            b.set("FC-PRIOR-PLU", Decimal("0.00000"))
                        else:
                            b.set("FC-PRIOR-PIU", rng.decimal("0.00000", "100.00000", 5))
                            b.set("FC-PRIOR-PLU", rng.decimal("0.00000", "100.00000", 5))
                    else:
                        b.set("FC-RESTATE-FROM-YYDDD", "00000")
                        b.set("FC-RESTATE-THRU-YYDDD", "00000")
                        b.set("FC-PRIOR-PIU", Decimal("0.00000"))
                        b.set("FC-PRIOR-PLU", Decimal("0.00000"))
                    b.set("FC-RECV-YYDDD", gc.yyddd_add_days(eff, -rng.randint(5, 40)) or eff)
                    writer.write(b.build())
    result = writer.close()
    result.update(
        {
            "dsn": "TELCABS.CABS.FACTOR",
            "dd": "FCTRMST",
            "copybook": "CABSFCTR",
            "restatement_records": restatement_count,
        }
    )
    return result, restatement_count


# ---------------------------------------------------------------------------
# Circuit / trunk inventory
# ---------------------------------------------------------------------------


def _build_circuits(
    rng: DeterministicRandom, carriers: Sequence[CarrierRef], count: int
) -> List[CircuitRef]:
    circuits: List[CircuitRef] = []
    billed = [c for c in carriers if c.type in ("I", "C", "R")]
    mpb_partners = [c.ocn for c in carriers if c.type in ("C", "L")]
    service_types = ["SW", "SP", "UN", "IC"]
    for n in range(count):
        carrier = rng.choice(billed)
        state_cd, latas, _npas = rng.choice(STATES)
        a_lata = rng.choice(latas)
        z_lata = rng.choice(latas) if rng.random() < 0.25 else a_lata
        service = rng.choices(service_types, [55, 25, 12, 8])[0]
        mpb = service in ("SW", "IC") and rng.random() < 0.30

        our_pct = Decimal("0.00000")
        other_pct = Decimal("0.00000")
        other_ocn = "    "
        if mpb:
            our_pct = rng.decimal("10.00000", "90.00000", 5)
            other_ocn = rng.choice(mpb_partners)
            roll = rng.random()
            if roll < 0.06:
                # The two LECs file independently; ~6% of pairs do not sum to
                # 100.00000, which is the condition the meet-point settlement
                # programs exist to resolve.
                drift = rng.decimal("0.00001", "3.50000", 5)
                other_pct = Decimal("100.00000") - our_pct + (drift if roll < 0.03 else -drift)
            else:
                other_pct = Decimal("100.00000") - our_pct

        circuits.append(
            CircuitRef(
                circuit_id="%02d/%s/%s/%06d" % (
                    rng.randint(10, 99),
                    rng.choice(["HCGS", "DHEC", "T1", "DS3", "OC3", "VGPL"]),
                    state_cd,
                    n + 1,
                ),
                trunk_grp="%s%04d" % (rng.choice("ABCDFGHKMNPRSTW"), rng.randint(1, 9999)),
                ocn=carrier.ocn,
                ban=rng.choice(carrier.bans),
                service_type=service,
                a_clli=_make_clli(rng, state_cd),
                z_clli=_make_clli(rng, state_cd),
                a_lata=a_lata,
                z_lata=z_lata,
                state_cd=state_cd,
                mpb=mpb,
                our_pct=str(our_pct),
                other_ocn=other_ocn,
                other_pct=str(other_pct),
            )
        )
    return circuits


def _write_circuits(
    path: Path, layout: Layout, rng: DeterministicRandom, circuits: Sequence[CircuitRef], cycle: date
) -> Tuple[Dict[str, Any], int]:
    lrecl = layout.lrecl
    bad_pct = 0
    with FixedRecordWriter(path, lrecl) as writer:
        for c in circuits:
            if c.mpb and Decimal(c.our_pct) + Decimal(c.other_pct) != Decimal("100.00000"):
                bad_pct += 1
            b = RecordBuilder(layout, lrecl)
            b.set("CI-CIRCUIT-ID", c.circuit_id)
            b.set("CI-TRUNK-GRP", c.trunk_grp)
            b.set("CI-OCN", c.ocn)
            b.set("CI-BAN", c.ban)
            b.set("CI-USOC", rng.choice(["TSGXX", "WALXX", "1LSPX", "DS1XX", "OC3XX"]))
            b.set("CI-SERVICE-TYPE", c.service_type)
            b.set("CI-A-CLLI", c.a_clli)
            b.set("CI-Z-CLLI", c.z_clli)
            b.set("CI-A-LATA", c.a_lata)
            b.set("CI-Z-LATA", c.z_lata)
            b.set("CI-STATE-CD", c.state_cd)
            b.set("CI-MPB-SW", "Y" if c.mpb else "N")
            b.set("CI-MPB-OUR-PCT", Decimal(c.our_pct))
            b.set("CI-MPB-OTHER-OCN", c.other_ocn)
            b.set("CI-MPB-OTHER-PCT", Decimal(c.other_pct))
            b.set("CI-INSTALL-YYDDD", gc.date_to_yyddd(cycle - timedelta(days=rng.randint(60, 4000))))
            b.set("CI-TERM-MONTHS", rng.choice([12, 24, 36, 60]))
            b.set("CI-DISC-YYDDD", "00000")
            b.set("CI-STATUS", rng.choices(["A", "P", "D"], [92, 5, 3])[0])
            writer.write(b.build())
    result = writer.close()
    result.update(
        {
            "dsn": "TELCABS.CABS.CIRCUIT",
            "dd": "CIRCMST",
            "copybook": "CABSCIRC",
            "meet_point_circuits": sum(1 for c in circuits if c.mpb),
            "meet_point_pct_not_100": bad_pct,
        }
    )
    return result, bad_pct


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------


def generate_reference(
    outdir: Path | str,
    layouts: Dict[str, Layout],
    rng: DeterministicRandom,
    cycle: date,
    profile: str = "DAILY",
    carrier_count: Optional[int] = None,
    rate_count: int = 1200,
    circuit_count: Optional[int] = None,
    bans_per_carrier: int = 3,
) -> Tuple[ReferenceData, List[Dict[str, Any]]]:
    """Generate all four master files. Returns the in-memory reference model
    and the manifest entries for the files written."""
    outdir = Path(outdir)
    outdir.mkdir(parents=True, exist_ok=True)

    if circuit_count is None:
        circuit_count = {"SMOKE": 2_000, "DAILY": 8_000, "STRESS": 20_000, "TARGET": 60_000}.get(
            profile, 8_000
        )

    carriers = _build_carriers(rng.substream("carrier"), cycle, bans_per_carrier)
    if carrier_count is not None and carrier_count < len(carriers):
        carriers = carriers[:carrier_count]
    rates = _build_rates(rng.substream("rate"), cycle, rate_count)
    circuits = _build_circuits(rng.substream("circuit"), carriers, circuit_count)

    manifest: List[Dict[str, Any]] = []
    manifest.append(_write_carriers(outdir / "TELCABS.CABS.CARRIER.dat", layouts["CABSCARR"], carriers, cycle))
    manifest.append(_write_rates(outdir / "TELCABS.CABS.RATE.dat", layouts["CABSRATE"], rates))
    factor_entry, _ = _write_factors(
        outdir / "TELCABS.CABS.FACTOR.dat", layouts["CABSFCTR"], rng.substream("factor"), carriers, cycle
    )
    manifest.append(factor_entry)
    circuit_entry, _ = _write_circuits(
        outdir / "TELCABS.CABS.CIRCUIT.dat", layouts["CABSCIRC"], rng.substream("circwrite"), circuits, cycle
    )
    manifest.append(circuit_entry)

    reference = ReferenceData(
        carriers=carriers,
        rates=rates,
        circuits=circuits,
        band_boundaries=BAND_BOUNDARIES,
        states=[s[0] for s in STATES],
    )
    sidecar = outdir / "reference_index.json"
    sidecar.write_text(json.dumps(reference.to_json(), indent=1), encoding="utf-8")
    manifest.append(
        {
            "file": sidecar.name,
            "path": str(sidecar),
            "records": len(carriers) + len(rates) + len(circuits),
            "note": "ASCII sidecar: the reference model in canonical decimal-string form, "
            "consumed by gen_cdr/gen_settlement and by the harness when it needs to "
            "resolve a key without reading the VSAM image",
        }
    )
    return reference, manifest
