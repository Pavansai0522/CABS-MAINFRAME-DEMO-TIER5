"""CABS Tier 5 -- the canonical interchange form.

Comparing an EBCDIC packed-decimal record to a UTF-8 JSON record byte by
byte is meaningless: they disagree everywhere and agree nowhere, and the
disagreement carries no information about whether the *business figure* is
the same. Comparing them by decoding the mainframe side into whatever the
target happens to produce is worse, because it makes the target's format the
definition of correct and quietly hides every precision loss on the way.

So both sides normalise into a third form that neither of them owns.

The canonical form
------------------
One newline-delimited JSON object per record::

    {
      "_schema": "cabs.canonical.v1",
      "_layout": "CABSCDR",
      "_record": "CABS-CDR-RECORD",
      "_side": "legacy",
      "_file": "TELCABS.CABS.USAGE.RAW.G0001V00.dat",
      "_ordinal": 0,
      "_variant": "CD-VOICE-DETAIL",
      "_key": "0288|813G1234567X|1",
      "f": {
        "CD-OCN":        {"t":"str", "pic":"X(04)",        "u":"DISPLAY", "o":0,  "l":4, "v":"0288"},
        "CD-SEQ-NBR":    {"t":"dec", "pic":"9(09)",        "u":"COMP-3",  "o":17, "l":5, "v":"1", "s":0},
        "CD-VC-CHG-MIN": {"t":"dec", "pic":"S9(07)V9(02)", "u":"COMP-3",  "o":77, "l":5, "v":"123.45", "s":2},
        "CD-CONN-YYDDD": {"t":"jul", "pic":"9(05)",        "u":"DISPLAY", "o":32, "l":5, "v":"24272", "iso":"2024-09-28"}
      }
    }

Rules, all of them deliberate:

* **EBCDIC to UTF-8.** cp037 in, Python string out. Trailing EBCDIC spaces
  are preserved, not stripped: a 13-byte BAN holding a 12-character value
  really does have a space in position 13, and a target that strips it has
  changed the key.
* **COMP-3 to a decimal string with the scale preserved.** Never a float,
  never a lossy conversion. ``"0.00"`` and ``"0"`` stay distinguishable
  because the declared scale is carried alongside in ``"s"``. A five-decimal
  rate is emitted with five decimals whatever its value.
* **Zoned decimal to a decimal string**, same rules.
* **YYDDD retained raw** in ``"v"`` **and** normalised to ISO in ``"iso"``.
  The raw value is authoritative for comparison; the ISO value is for
  humans and for date arithmetic. Where the raw value is not a real date
  -- 24366 in a non-leap year, 00000, day 999 -- ``"iso"`` is ``null`` and
  the raw value still compares.
* **Every field carries its declared PIC and USAGE**, so precision is
  auditable from the canonical file alone without going back to the
  copybook.
* **REDEFINES overlays are resolved to exactly one variant per record.**
  Decoding all three CABSCDR variants would produce three contradictory
  readings of the same 96 bytes. The contract says which field selects the
  variant; the chosen variant is recorded in ``_variant``.

The schema layer (offsets, lengths, PIC, USAGE) is taken from the frozen
copybooks by the parser in ``GENERATORS/gen_common.py``. The copybooks are
the only schema source in the whole toolchain, which is why neither the
generator nor the harness can drift from the data architecture.
"""

from __future__ import annotations

import json
import sys
from dataclasses import dataclass
from decimal import Decimal
from pathlib import Path
from typing import Any, Dict, Iterable, Iterator, List, Optional, Sequence, Tuple

# The copybook parser and the packed-decimal codec live with the generators;
# they are shared deliberately so there is exactly one implementation of the
# data architecture in the toolchain. test_canonical.py cross-checks the
# decoder against a second, independently written implementation.
_GENERATORS = Path(__file__).resolve().parent.parent / "GENERATORS"
if str(_GENERATORS) not in sys.path:
    sys.path.insert(0, str(_GENERATORS))

import gen_common as gc  # noqa: E402
from gen_common import Field, Layout  # noqa: E402

__all__ = [
    "SCHEMA",
    "CanonicalRecord",
    "VariantRule",
    "DatasetSpec",
    "canonicalise_record",
    "canonicalise_file",
    "write_ndjson",
    "read_ndjson",
    "load_layouts",
]

SCHEMA = "cabs.canonical.v1"

#: Types used in the ``"t"`` slot.
T_STRING = "str"
T_DECIMAL = "dec"
T_JULIAN = "jul"
T_RAW = "raw"


# ---------------------------------------------------------------------------
# Specifications (populated from the contract file)
# ---------------------------------------------------------------------------


@dataclass
class VariantRule:
    """How to choose which REDEFINES overlay to decode."""

    field: str
    mapping: Dict[str, str]
    default: Optional[str] = None

    def choose(self, values: Dict[str, Any]) -> Optional[str]:
        raw = values.get(self.field)
        if raw is None:
            return self.default
        return self.mapping.get(str(raw).strip(), self.default)


@dataclass
class DatasetSpec:
    """One dataset in the comparison contract."""

    name: str
    pattern: str
    layout: str
    levels: List[str]
    key: List[str]
    recfm: str = "FB"
    lrecl: Optional[int] = None
    variant_rule: Optional[VariantRule] = None
    ignore_fields: List[str] = None  # type: ignore[assignment]
    tolerances: Dict[str, Dict[str, str]] = None  # type: ignore[assignment]
    #: Fields carried into a variance's context so a record-level finding can
    #: later be attributed. Without them an L1 "missing record" is just a key
    #: and nothing can say *why* it might be missing.
    witness_fields: List[str] = None  # type: ignore[assignment]
    description: str = ""

    def __post_init__(self) -> None:
        if self.ignore_fields is None:
            self.ignore_fields = []
        if self.tolerances is None:
            self.tolerances = {}
        if self.witness_fields is None:
            self.witness_fields = []

    @classmethod
    def from_json(cls, payload: Dict[str, Any]) -> "DatasetSpec":
        vr = payload.get("variant_rule")
        return cls(
            name=payload["name"],
            pattern=payload["pattern"],
            layout=payload["layout"],
            levels=list(payload.get("levels", ["L1", "L2"])),
            key=list(payload["key"]),
            recfm=payload.get("recfm", "FB"),
            lrecl=payload.get("lrecl"),
            variant_rule=(
                VariantRule(vr["field"], dict(vr["map"]), vr.get("default")) if vr else None
            ),
            ignore_fields=list(payload.get("ignore_fields", [])),
            tolerances=dict(payload.get("tolerances", {})),
            witness_fields=list(payload.get("witness_fields", [])),
            description=payload.get("description", ""),
        )


@dataclass
class CanonicalRecord:
    """One canonicalised record. ``fields`` is the ``"f"`` map above."""

    layout: str
    record_name: str
    side: str
    file: str
    ordinal: int
    key: str
    variant: Optional[str]
    fields: Dict[str, Dict[str, Any]]

    def value(self, name: str) -> Optional[str]:
        entry = self.fields.get(name)
        return None if entry is None else entry.get("v")

    def decimal(self, name: str) -> Optional[Decimal]:
        entry = self.fields.get(name)
        if entry is None or entry.get("t") != T_DECIMAL:
            return None
        return Decimal(entry["v"])

    def to_json(self) -> Dict[str, Any]:
        return {
            "_schema": SCHEMA,
            "_layout": self.layout,
            "_record": self.record_name,
            "_side": self.side,
            "_file": self.file,
            "_ordinal": self.ordinal,
            "_variant": self.variant,
            "_key": self.key,
            "f": self.fields,
        }

    @classmethod
    def from_json(cls, payload: Dict[str, Any]) -> "CanonicalRecord":
        return cls(
            layout=payload["_layout"],
            record_name=payload.get("_record", ""),
            side=payload.get("_side", ""),
            file=payload.get("_file", ""),
            ordinal=payload.get("_ordinal", 0),
            key=payload["_key"],
            variant=payload.get("_variant"),
            fields=payload["f"],
        )


# ---------------------------------------------------------------------------
# Field-level canonicalisation
# ---------------------------------------------------------------------------


def _is_julian(field: Field) -> bool:
    """A field is a Julian date if it is named YYDDD and is five positions."""
    name = field.name.upper()
    return name.endswith("YYDDD") and (
        (field.pic is not None and field.pic.display_length == 5) or field.length == 5
    )


def _canonical_field(
    field: Field,
    chunk: bytes,
    index: int = 1,
    strict: bool = False,
) -> Dict[str, Any]:
    """Turn one field's bytes into its canonical entry.

    Decoding is always strict: a non-decimal nibble in a packed field is the
    Python equivalent of an S0C7 and must not be quietly read as a zero. The
    ``strict`` flag decides only what happens next -- ``False`` (the default)
    records the failure as a raw-hex finding and carries on, ``True`` aborts
    the canonicalisation. One unreadable record must not take down a
    comparison of half a million.
    """
    entry: Dict[str, Any] = {
        "pic": field.pic.raw if field.pic else "GROUP",
        "u": field.usage,
        "o": field.offset_for(index),
        "l": field.length,
    }
    try:
        if field.usage == "COMP-3":
            value = gc.unpack_comp3(chunk, field.scale, field.digits, strict=True)
            entry["t"] = T_DECIMAL
            entry["s"] = field.scale
            entry["v"] = str(value)
        elif field.pic is not None and field.pic.is_numeric:
            if _is_julian(field):
                raw = gc.from_ebcdic(chunk)
                entry["t"] = T_JULIAN
                entry["v"] = raw
                parsed = gc.yyddd_to_date(raw)
                entry["iso"] = parsed.isoformat() if parsed else None
            else:
                value = gc.unpack_zoned(chunk, field.scale, field.signed)
                entry["t"] = T_DECIMAL
                entry["s"] = field.scale
                entry["v"] = str(value)
        else:
            entry["t"] = T_STRING
            entry["v"] = gc.from_ebcdic(chunk)
    except ValueError as exc:
        if strict:
            raise
        # An undecodable field is a finding, not a crash. It is emitted as
        # raw hex so the comparison can still say "these bytes differ" and
        # the report can say why they could not be read.
        entry["t"] = T_RAW
        entry["v"] = chunk.hex()
        entry["error"] = str(exc)
    return entry


def _julian_groups(layout: Layout) -> List[Field]:
    """Group items that spell a YYDDD across two elementary fields.

    ``CD-CONN-YYDDD`` is a group of ``CD-CONN-YY`` and ``CD-CONN-DDD``. The
    two halves compare correctly on their own, but the date only means
    anything as a whole, so the whole is emitted as well.
    """
    out = []
    for name in layout.order:
        f = layout.fields[name]
        if f.is_group and f.name.upper().endswith("YYDDD") and f.length == 5:
            out.append(f)
    return out


# ---------------------------------------------------------------------------
# Record-level canonicalisation
# ---------------------------------------------------------------------------


def canonicalise_record(
    layout: Layout,
    data: bytes,
    spec: DatasetSpec,
    side: str,
    file_name: str,
    ordinal: int,
    strict: bool = False,
) -> CanonicalRecord:
    """Canonicalise one record according to ``spec``."""
    fields: Dict[str, Dict[str, Any]] = {}
    ignore = {n.upper() for n in spec.ignore_fields}

    # Pass 1: the base record, no REDEFINES overlays.
    base_values: Dict[str, Any] = {}
    for name in layout.order:
        f = layout.fields[name]
        if f.is_group or f.pic is None or layout._under_redefines(f):
            continue
        if name in ignore:
            continue
        occurrences = 1
        if f.occurs_owner:
            owner = layout.fields[f.occurs_owner]
            occurrences = owner.occurs_max
            if owner.odo_field and owner.odo_field in base_values:
                try:
                    occurrences = int(Decimal(str(base_values[owner.odo_field])))
                except Exception:
                    occurrences = owner.occurs_min or 1
        for idx in range(1, occurrences + 1):
            off = f.offset_for(idx)
            chunk = data[off : off + f.length]
            if len(chunk) < f.length:
                break
            key_name = name if occurrences == 1 and not f.occurs_owner else "%s(%d)" % (name, idx)
            entry = _canonical_field(f, chunk, idx, strict=strict)
            fields[key_name] = entry
            if idx == 1:
                base_values[name] = entry.get("v")

    # Pass 2: the group-level Julian dates.
    for g in _julian_groups(layout):
        if g.name in ignore or g.name in fields:
            continue
        chunk = data[g.offset : g.offset + g.length]
        if len(chunk) != g.length:
            continue
        raw = gc.from_ebcdic(chunk)
        parsed = gc.yyddd_to_date(raw)
        fields[g.name] = {
            "t": T_JULIAN,
            "pic": "9(05)",
            "u": "DISPLAY",
            "o": g.offset,
            "l": g.length,
            "v": raw,
            "iso": parsed.isoformat() if parsed else None,
        }

    # Pass 3: exactly one REDEFINES overlay, chosen by the contract.
    variant: Optional[str] = None
    if spec.variant_rule is not None:
        variant = spec.variant_rule.choose(base_values)
    elif layout.variant_groups():
        variant = None  # no rule: overlays are not decoded at all
    if variant:
        overlay = layout.fields.get(variant.upper())
        if overlay is not None:
            for f in _elementary_under(layout, overlay):
                if f.name in ignore:
                    continue
                chunk = data[f.offset : f.offset + f.length]
                if len(chunk) != f.length:
                    continue
                fields[f.name] = _canonical_field(f, chunk, strict=strict)

    key = "|".join(_key_component(fields, k) for k in spec.key)
    return CanonicalRecord(
        layout=layout.name,
        record_name=layout.record_name,
        side=side,
        file=file_name,
        ordinal=ordinal,
        key=key,
        variant=variant,
        fields=fields,
    )


def _key_component(fields: Dict[str, Dict[str, Any]], name: str) -> str:
    entry = fields.get(name.upper())
    if entry is None:
        return "<missing:%s>" % name
    return str(entry.get("v", ""))


def _elementary_under(layout: Layout, group: Field) -> List[Field]:
    out: List[Field] = []
    stack = list(group.children)
    while stack:
        f = stack.pop(0)
        if f.children:
            stack = list(f.children) + stack
        elif f.pic is not None:
            out.append(f)
    return out


# ---------------------------------------------------------------------------
# File-level canonicalisation
# ---------------------------------------------------------------------------


def _iter_records(path: Path, lrecl: int, recfm: str) -> Iterator[bytes]:
    """Stream records from a fixed or variable-length dataset.

    Variable-length files are read with the IBM RDW convention: a 4-byte
    record descriptor word whose first halfword is the record length
    *including* the RDW itself.
    """
    if recfm.upper().startswith("V"):
        with path.open("rb") as fh:
            while True:
                rdw = fh.read(4)
                if not rdw:
                    return
                if len(rdw) < 4:
                    raise ValueError("%s ends with a partial RDW" % path.name)
                length = int.from_bytes(rdw[:2], "big")
                if length < 4:
                    raise ValueError("%s has an RDW of %d bytes" % (path.name, length))
                body = fh.read(length - 4)
                if len(body) != length - 4:
                    raise ValueError("%s ends with a truncated record" % path.name)
                yield body
    else:
        yield from gc.iter_fixed_records(path, lrecl)


def canonicalise_file(
    path: Path | str,
    layout: Layout,
    spec: DatasetSpec,
    side: str,
    strict: bool = False,
    limit: Optional[int] = None,
) -> Iterator[CanonicalRecord]:
    """Canonicalise every record in one dataset."""
    path = Path(path)
    lrecl = spec.lrecl or layout.lrecl
    for ordinal, data in enumerate(_iter_records(path, lrecl, spec.recfm)):
        if limit is not None and ordinal >= limit:
            return
        yield canonicalise_record(layout, data, spec, side, path.name, ordinal, strict=strict)


def write_ndjson(records: Iterable[CanonicalRecord], path: Path | str) -> int:
    """Write canonical records as newline-delimited JSON. Returns the count."""
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    count = 0
    with path.open("w", encoding="utf-8") as fh:
        for record in records:
            fh.write(json.dumps(record.to_json(), separators=(",", ":"), ensure_ascii=False))
            fh.write("\n")
            count += 1
    return count


def read_ndjson(path: Path | str) -> Iterator[CanonicalRecord]:
    """Read canonical records back."""
    path = Path(path)
    with path.open("r", encoding="utf-8") as fh:
        for line in fh:
            line = line.strip()
            if line:
                yield CanonicalRecord.from_json(json.loads(line))


def load_layouts(copybook_dir: Path | str) -> Dict[str, Layout]:
    """Parse the frozen copybooks. The only schema source in the toolchain."""
    return gc.load_layouts(copybook_dir, quiet=True)


# ---------------------------------------------------------------------------
# CLI -- canonicalise a single dataset, for inspection
# ---------------------------------------------------------------------------


def _main(argv: Optional[Sequence[str]] = None) -> int:
    import argparse

    p = argparse.ArgumentParser(
        description="Canonicalise one CABS dataset into newline-delimited JSON."
    )
    p.add_argument("input", help="EBCDIC dataset to read")
    p.add_argument("--layout", required=True, help="copybook member, e.g. CABSCDR")
    p.add_argument("--key", nargs="+", required=True, help="key field names")
    p.add_argument("--recfm", default="FB")
    p.add_argument("--lrecl", type=int, default=None)
    p.add_argument("--side", default="legacy")
    p.add_argument("--variant-field", default=None)
    p.add_argument(
        "--variant-map",
        default=None,
        help='JSON object mapping the variant field value to an overlay name',
    )
    p.add_argument("--out", default="-", help="output file, or - for stdout")
    p.add_argument("--limit", type=int, default=None)
    p.add_argument(
        "--copybooks",
        default=str(Path(__file__).resolve().parent.parent / "COPYBOOKS"),
    )
    args = p.parse_args(argv)

    layouts = load_layouts(args.copybooks)
    layout = layouts[args.layout.upper()]
    rule = None
    if args.variant_field:
        rule = VariantRule(args.variant_field.upper(), json.loads(args.variant_map or "{}"))
    spec = DatasetSpec(
        name=Path(args.input).name,
        pattern=Path(args.input).name,
        layout=args.layout.upper(),
        levels=["L1", "L2"],
        key=[k.upper() for k in args.key],
        recfm=args.recfm,
        lrecl=args.lrecl,
        variant_rule=rule,
    )
    records = canonicalise_file(args.input, layout, spec, args.side, limit=args.limit)
    if args.out == "-":
        for record in records:
            print(json.dumps(record.to_json(), separators=(",", ":"), ensure_ascii=False))
        return 0
    count = write_ndjson(records, args.out)
    print("%d records -> %s" % (count, args.out))
    return 0


if __name__ == "__main__":
    raise SystemExit(_main())
