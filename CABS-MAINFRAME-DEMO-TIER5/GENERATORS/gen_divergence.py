"""CABS Tier 5 -- candidate-side divergence injector.

Until a real target implementation exists, the comparison harness needs a
*candidate* estate to compare the legacy estate against. This module makes
one, by applying named, documented behaviour differences to generated usage
files.

Each divergence is written to mimic what a target implementation would do if
it either (a) reproduced a seeded legacy defect faithfully, (b) silently
normalised one away, or (c) introduced something of its own. That is exactly
the three-way verdict the harness has to make:

===================  ===========================================  ===========================
Divergence           Mimics                                       Expected harness verdict
===================  ===========================================  ===========================
``band-boundary``    target puts an exact boundary quantity in     DIVERGENT-BY-DESIGN
                     the other band
``tandem-rounding``  target rounds tandem minutes where legacy     DIVERGENT-BY-DESIGN
                     truncates
``fatal-excluded``   target excludes fatal-status records from     DIVERGENT-BY-DESIGN
                     the billable stream; legacy does not
``cycle-window``     target keeps prior-cycle records the legacy   DIVERGENT-BY-DESIGN
                     window arithmetic drops
``element-overflow`` shapes the input so that a bill detail line   DIVERGENT-BY-DESIGN
                     ends up carrying 29-40 rate elements
``unseeded-drift``   a difference with no seeded defect behind it  DIVERGENT
``record-loss``      records missing from the candidate            DIVERGENT (L1)
===================  ===========================================  ===========================

Which seeded defect each of the first five is expected to attribute to is
*not* recorded here. It lives in ``SEALED/defect_placements.json`` and is
loaded from there when the sealed inputs are present; with ``SEALED/``
withheld this module reports ``null`` for every mode and the harness has to
work the attribution out for itself.

``element-overflow`` is not like the others and the difference matters
-----------------------------------------------------------------------
Every other mode here mimics a *target behaviour* and is therefore applied to
the candidate generation only: the legacy is generated with ``--divergence
none`` and the two files then differ in the way a target implementation would
make them differ.

``element-overflow`` shapes the **input** instead. What it stands for happens
downstream of the usage file, in the step that assembles a bill detail line
out of rated element records, and there is nothing on a CDR for an injector to
change — a correct target and the legacy read byte-identical usage and part
company later. So this mode does not mimic anything. It manufactures the
*precondition*: a population of accounts whose rated elements will fold into a
single bill detail line carrying 29 to 40 occurrences, which ordinary traffic
and every profile below STRESS essentially never produce.

**It must therefore be applied to both generations, not one.** Run it with the
same seed on the legacy side and on the candidate side. Applying it to one
side only makes the two usage files differ, which produces a large L1/L2
finding about the generator and tells you nothing about the billing step. The
returned report lists it under ``shapes_input`` for exactly this reason.

One further honesty note. This mode gets the shape onto the usage file. Whether
the resulting bill detail line actually reaches 29 occurrences depends on two
things this module does not control: the ingest edit, which may route some of
the shaped records to suspense before they are rated, and the ``MAXELM``
control card, which decides where the assembly step opens a continuation line.
Check the assembly step's printed register for the element distribution after
the batch has run rather than assuming the shape survived.

``unseeded-drift`` and ``record-loss`` are there on purpose. A harness that
classifies everything as DIVERGENT-BY-DESIGN is not a harness, it is a
rubber stamp; these two prove it still says no.

Nothing here is applied unless ``generate.py --divergence`` asks for it. The
default is ``none`` and the legacy estate is produced untouched.
"""

from __future__ import annotations

from decimal import Decimal, ROUND_DOWN, ROUND_HALF_UP
from pathlib import Path
from typing import Any, Dict, List, Optional, Sequence

import gen_common as gc
from gen_common import DeterministicRandom, Layout

__all__ = ["DIVERGENCES", "apply_divergence"]

DIVERGENCES = (
    "band-boundary",
    "tandem-rounding",
    "fatal-excluded",
    "cycle-window",
    "element-overflow",
    "unseeded-drift",
    "record-loss",
)

#: Modes that shape the input rather than mimic a target behaviour, and which
#: must therefore be applied to both generations with the same seed. See the
#: module docstring.
SHAPES_INPUT = ("element-overflow",)

#: Record types the special access and unbundled element rating steps handle.
#: Those two steps carry the CDR sequence number straight onto the rated
#: element record as its line sequence, and the assembly step groups on that
#: line sequence -- so records sharing a sequence number within an account
#: land on one bill detail line, one occurrence each.
_OVERFLOW_REC_TYPES = ("05", "06", "07")

#: How many occurrences the shaped lines are aimed at. The low end is one past
#: the last position a 1204-byte image of a CABSBILL record reproduces whole
#: ((1204 - 127) // 38 = 28); the high end is the copybook maximum.
_OVERFLOW_MIN_ELEMENTS = 29
_OVERFLOW_MAX_ELEMENTS = 40

#: Accounts to shape per usage file.
_OVERFLOW_ACCOUNTS_PER_FILE = 3

def _expected_attribution() -> Dict[str, Optional[str]]:
    """Which seeded defect, if any, each divergence should be traceable to.

    Read from ``SEALED/defect_placements.json`` rather than held here, so
    that withholding ``SEALED/`` withholds the mapping too. When the sealed
    file is absent every mode reports ``None`` and the manifest says so.
    """
    out: Dict[str, Optional[str]] = {n: None for n in DIVERGENCES}
    sealed = Path(__file__).resolve().parent.parent / "SEALED" / "defect_placements.json"
    try:
        import json as _json
        table = _json.loads(sealed.read_text(encoding="utf-8")).get(
            "divergence_mode_attribution", {})
    except (OSError, ValueError):
        return out
    for name in DIVERGENCES:
        if name in table:
            out[name] = table[name]
    return out


def _shape_element_overflow(
    data: bytearray,
    lrecl: int,
    plan: Dict[str, Any],
    rng: DeterministicRandom,
) -> Tuple[int, List[Dict[str, Any]]]:
    """Give a few accounts a bill detail line with 29 to 40 occurrences.

    The mechanism is the sequence number. Special access and unbundled element
    rating move the CDR sequence number onto the rated element record as its
    line sequence, and bill detail assembly groups on account, period, section
    and that line sequence, appending one occurrence per record in the group.
    Putting n records of an account onto one sequence number therefore asks for
    a line of n occurrences.

    The records are also forced to a clean edit status, because a record the
    ingest edit routes to suspense never reaches rating and would quietly
    shrink the line the shape was aiming for.

    Returns the number of records changed and a description of every account
    shaped, which the caller puts in the report so the harness can assert on
    those accounts rather than hoping a sample lands on one.
    """
    f_ocn = plan["CD-OCN"]
    f_ban = plan["CD-BAN"]
    f_seq = plan["CD-SEQ-NBR"]
    f_type = plan["CD-REC-TYPE"]
    f_status = plan["CD-EDIT-STATUS"]

    def text(base: int, field: Any) -> str:
        start = base + field[1]
        return gc.from_ebcdic(bytes(data[start : start + field[2]]))

    by_account: Dict[Tuple[str, str], List[int]] = {}
    pool: List[int] = []
    total = len(data) // lrecl
    for i in range(total):
        base = i * lrecl
        if text(base, f_type) not in _OVERFLOW_REC_TYPES:
            continue
        pool.append(base)
        by_account.setdefault((text(base, f_ocn), text(base, f_ban)), []).append(base)

    if not by_account:
        return 0, []

    # Busiest accounts first, so the shaped line sits on an account that was
    # already heavy rather than on one invented for the purpose. A line reaches
    # this many occurrences in real life because the account is large.
    ranked = sorted(by_account, key=lambda k: (-len(by_account[k]), k))
    picked = ranked[: _OVERFLOW_ACCOUNTS_PER_FILE]

    changed = 0
    used: set = set()
    placed: List[Dict[str, Any]] = []
    size_rng = rng.substream("overflow-size")
    for ocn, ban in picked:
        target = size_rng.randint(_OVERFLOW_MIN_ELEMENTS, _OVERFLOW_MAX_ELEMENTS)
        chosen = [b for b in by_account[(ocn, ban)] if b not in used][:target]
        # An ordinary day's traffic does not put thirty of anything on one
        # account, so the shortfall is made up by moving other accounts'
        # records of the same kind onto this one. The record keeps its own
        # content; only the account it is billed to changes.
        if len(chosen) < target:
            spare = [b for b in pool if b not in used and b not in chosen]
            chosen += spare[: target - len(chosen)]
        if len(chosen) < _OVERFLOW_MIN_ELEMENTS:
            continue
        used.update(chosen)
        head = chosen[0]
        shared = bytes(data[head + f_seq[1] : head + f_seq[1] + f_seq[2]])
        seq_value = gc.unpack_comp3(shared, f_seq[4], strict=False)
        for base in chosen:
            data[base + f_ocn[1] : base + f_ocn[1] + f_ocn[2]] = gc.to_ebcdic(ocn, f_ocn[2])
            data[base + f_ban[1] : base + f_ban[1] + f_ban[2]] = gc.to_ebcdic(ban, f_ban[2])
            data[base + f_seq[1] : base + f_seq[1] + f_seq[2]] = shared
            data[base + f_status[1] : base + f_status[1] + f_status[2]] = gc.to_ebcdic(
                "0", f_status[2]
            )
            changed += 1
        placed.append(
            {
                "ocn": ocn.strip(),
                "ban": ban.strip(),
                "seq": str(seq_value),
                "records_on_one_sequence": len(chosen),
                "occurrences_expected_on_the_line": len(chosen),
                "why": (
                    "one bill detail line is expected to carry %d occurrences, which is past "
                    "the %d a 1204-byte image of the record reproduces whole"
                    % (len(chosen), (1204 - 127) // 38)
                ),
            }
        )
    return changed, placed


def _parse_spec(spec: str) -> List[str]:
    if spec.strip().lower() == "all":
        return list(DIVERGENCES)
    names = [s.strip() for s in spec.split(",") if s.strip()]
    unknown = [n for n in names if n not in DIVERGENCES]
    if unknown:
        raise SystemExit(
            "unknown divergence(s) %s; choose from %s or 'all'"
            % (", ".join(unknown), ", ".join(DIVERGENCES))
        )
    return names


def apply_divergence(
    usage_files: Sequence[Path],
    layout: Layout,
    spec: str,
    rng: DeterministicRandom,
    probes: Optional[Sequence[Dict[str, Any]]] = None,
    cycle_start_yyddd: Optional[str] = None,
) -> Dict[str, Any]:
    """Rewrite ``usage_files`` in place with the named divergences applied.

    Returns a report naming what was applied, how many records each touched,
    and what the harness ought to conclude about each -- which is the
    scorecard a blind run is graded against afterwards.
    """
    names = _parse_spec(spec)
    lrecl = layout.lrecl
    plan = gc.encoding_plan(layout)

    f_chg = plan["CD-VC-CHG-MIN"]
    f_conv = plan["CD-VC-CONV-MIN"]
    f_elem = plan["CD-RATE-ELEM"]
    f_status = plan["CD-EDIT-STATUS"]
    f_type = plan["CD-REC-TYPE"]
    f_tandem = plan["CD-VC-TANDEM-IND"]
    f_conn_yy = plan["CD-CONN-YY"]
    f_conn_ddd = plan["CD-CONN-DDD"]
    f_src = plan["CD-SRC-SYSTEM"]

    applied: Dict[str, int] = {name: 0 for name in names}
    overflow_accounts: List[Dict[str, Any]] = []
    probe_keys = {(p["ocn"], p["ban"], p["seq"]) for p in (probes or [])}
    cycle_start_abs = gc.yyddd_to_abs(cycle_start_yyddd) if cycle_start_yyddd else None

    for path in usage_files:
        data = bytearray(path.read_bytes())

        # Input shaping runs first and on the whole file, because it decides
        # which records exist to be shaped before any per-record rule looks at
        # them. It is also the one thing here that has to be done identically
        # on both sides.
        if "element-overflow" in names:
            changed, placed = _shape_element_overflow(
                data, lrecl, plan, rng.substream("element-overflow:%s" % path.name)
            )
            applied["element-overflow"] += changed
            for entry in placed:
                entry["file"] = path.name
            overflow_accounts.extend(placed)

        total = len(data) // lrecl
        keep: List[int] = []
        for i in range(total):
            base = i * lrecl
            view = data[base : base + lrecl]
            drop = False

            rate_elem = gc.from_ebcdic(bytes(view[f_elem[1] : f_elem[1] + f_elem[2]]))
            edit_status = gc.from_ebcdic(bytes(view[f_status[1] : f_status[1] + f_status[2]]))
            rec_type = gc.from_ebcdic(bytes(view[f_type[1] : f_type[1] + f_type[2]]))
            is_voice = rec_type in ("01", "02", "03", "08")

            if "fatal-excluded" in names and edit_status in ("6", "7", "8", "9"):
                # The target routes a fatal record to suspense and stops
                # processing it. Whether the legacy does the same is what the
                # comparison is for.
                drop = True
                applied["fatal-excluded"] += 1

            if not drop and "record-loss" in names and rng.random() < 0.0002:
                # No defect behind this: records simply did not arrive.
                drop = True
                applied["record-loss"] += 1

            if not drop and is_voice:
                chg = gc.unpack_comp3(
                    bytes(view[f_chg[1] : f_chg[1] + f_chg[2]]), f_chg[4], strict=False
                )
                new_chg = chg

                if "band-boundary" in names and chg in (
                    Decimal("50000.00"), Decimal("100000.00"), Decimal("250000.00"),
                    Decimal("500000.00"), Decimal("1000000.00"), Decimal("2500000.00"),
                    Decimal("5000000.00"),
                ):
                    # The candidate treats the boundary as inclusive of the
                    # higher band. Represented here by nudging the quantity
                    # one hundredth above the edge, which is the quantity at
                    # which any two implementations must agree.
                    new_chg = chg + Decimal("0.01")
                    applied["band-boundary"] += 1

                tandem = gc.from_ebcdic(bytes(view[f_tandem[1] : f_tandem[1] + f_tandem[2]]))
                if "tandem-rounding" in names and (rate_elem.strip() == "TANSW" or tandem == "Y"):
                    conv = gc.unpack_comp3(
                        bytes(view[f_conv[1] : f_conv[1] + f_conv[2]]), f_conv[4], strict=False
                    )
                    rounded = conv.quantize(Decimal("1"), rounding=ROUND_HALF_UP).quantize(Decimal("0.01"))
                    truncated = conv.quantize(Decimal("1"), rounding=ROUND_DOWN).quantize(Decimal("0.01"))
                    if rounded != truncated and new_chg == chg:
                        new_chg = rounded
                        applied["tandem-rounding"] += 1

                if "unseeded-drift" in names and rng.random() < 0.0005 and new_chg == chg:
                    # A one-cent movement with nothing behind it.
                    new_chg = chg + Decimal("0.01")
                    applied["unseeded-drift"] += 1

                if new_chg != chg:
                    packed = gc.pack_comp3(new_chg, f_chg[3], f_chg[4], f_chg[5])
                    view[f_chg[1] : f_chg[1] + f_chg[2]] = packed

            if not drop and "cycle-window" in names and cycle_start_abs is not None:
                yy = int(gc.unpack_zoned(bytes(view[f_conn_yy[1] : f_conn_yy[1] + f_conn_yy[2]])))
                ddd = int(gc.unpack_zoned(bytes(view[f_conn_ddd[1] : f_conn_ddd[1] + f_conn_ddd[2]])))
                conn_abs = gc.yyddd_to_abs("%02d%03d" % (yy, ddd))
                if conn_abs is not None and conn_abs < cycle_start_abs:
                    # Usage dated before the cycle start. The candidate keeps
                    # these and marks the source system so the difference is
                    # attributable rather than anonymous; whether the legacy
                    # keeps them is what the comparison is for.
                    view[f_src[1] : f_src[1] + f_src[2]] = gc.to_ebcdic("EMI2", f_src[2])
                    applied["cycle-window"] += 1

            if not drop:
                keep.append(base)
                data[base : base + lrecl] = view

        if len(keep) == total:
            path.write_bytes(bytes(data))
        else:
            out = bytearray()
            for base in keep:
                out += data[base : base + lrecl]
            path.write_bytes(bytes(out))

    report: Dict[str, Any] = {
        "spec": spec,
        "applied": applied,
        "expected_attribution": {n: v for n, v in _expected_attribution().items()
                                 if n in names},
        "shapes_input": [n for n in names if n in SHAPES_INPUT],
        "note": (
            "This is a stand-in for a target implementation. The harness must reach its "
            "verdicts from the answer key and the comparison, never from this report; the "
            "report is the scorecard the blind run is marked against afterwards."
        ),
    }
    if report["shapes_input"]:
        report["shapes_input_note"] = (
            "The modes listed in shapes_input do not mimic a target behaviour. They change "
            "the shape of the input so that a condition further down the batch has something "
            "to act on, and they must be applied to BOTH generations with the same seed. "
            "Applying them to one side only produces a large record-level and field-level "
            "finding about the generator and says nothing about the process under test."
        )
    if overflow_accounts:
        report["element_overflow_accounts"] = overflow_accounts
        report["element_overflow_note"] = (
            "These accounts have been given a bill detail line that should carry more "
            "occurrences than a fixed image of the record would hold. Assert on them by name "
            "rather than sampling; nothing below the STRESS profile produces this shape on "
            "its own. Whether the line actually reaches the target count depends on the "
            "ingest edit and on the MAXELM control card, so read the assembly step's printed "
            "register before concluding that the shape did not survive."
        )
    return report
