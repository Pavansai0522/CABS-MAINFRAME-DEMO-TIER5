"""CABS Tier 5 -- the staged L1-L5 comparison engine.

A bill-to-bill comparison that only looks at the final invoice tells you
*that* the two sides disagree. It does not tell you *where* they started
disagreeing, and on a wholesale access bill the answer is almost never the
last step. So the comparison is staged, and every stage runs at every
process boundary:

=====  ============  =========================================================
Level  Name          What it asserts
=====  ============  =========================================================
L1     Record        after canonicalisation, the same records are present,
                     once each, keyed per the contract -- no missing, no
                     extra, no duplicates
L2     Field         every field agrees, typed, with per-field tolerance.
                     Decimal comparison is **exact by default**; a tolerance
                     applies only where the contract declares one, and the
                     declared scale is compared as well as the value
L3     Control       the balancing equation holds on both sides for every
                     process, the hash totals chain correctly from one
                     process to the next, and the written volume of the
                     variable-length bill detail -- records, declared
                     occurrences, declared bytes and a payload digest --
                     agrees. The first three can agree while the fourth does
                     not, and that combination is the whole point of
                     asserting all four
L4     Bill          invoice header, per-carrier, per-BAN, per-rate-element
                     and per-line-item money agree, to the penny; and each
                     side's detail lines are separately asserted to own the
                     occurrences they carry and to add up to the totals
                     printed on and above them
L5     Settlement    per-counterparty per-period settlement agrees, including
                     meet-point splits and PIU/PLU restatements
=====  ============  =========================================================

Memory
------
The engine works on canonicalised NDJSON, and indexes it by key to a byte
offset rather than holding records in memory. A 500,000-record generation
costs roughly 50 MB of index. At TARGET volume (100,000,000 records) build
the index over a sampled key range; ``--sample`` on ``run_compare.py`` does
this and the report says so, because a sampled L2 is a different claim from
a complete one and must never be presented as the same thing.
"""

from __future__ import annotations

import hashlib
import json
from collections import defaultdict
from dataclasses import asdict, dataclass, field as dc_field
from decimal import Decimal
from pathlib import Path
from typing import Any, Dict, Iterable, Iterator, List, Optional, Sequence, Set, Tuple

from canonical import CanonicalRecord, DatasetSpec, T_DECIMAL, T_JULIAN

__all__ = [
    "Variance",
    "LevelResult",
    "ComparisonResult",
    "NdjsonIndex",
    "compare_l1",
    "compare_l2",
    "compare_l3",
    "compare_l4",
    "compare_l5",
    "run_levels",
    "BILL_DETAIL_FIXED_BYTES",
    "BILL_ELEMENT_BYTES",
    "BILL_SORT_IMAGE_BYTES",
    "BILL_ELEMENT_BOUNDARY",
    "elements_carried_by",
    "bill_detail_written_volume",
]

LEVELS = ("L1", "L2", "L3", "L4", "L5")

# ---------------------------------------------------------------------------
# Bill detail geometry
# ---------------------------------------------------------------------------
#
# CABSBILL is the estate's only variable-length business record. Its shape is
# arithmetic, not opinion, and several assertions below depend on getting the
# arithmetic right, so it is written down once here and asserted against the
# parsed copybook in the harness tests rather than trusted.
#
#   fixed portion  13 + 6 + 2 + 4 + 4 + 1 + 2 + 60 + 8 + 10 + 8 + 6 + 3 = 127
#   one occurrence  6 + 8 + 6 + 9 + 1 + 8                               =  38
#   longest record  127 + 40 x 38                                       = 1647
#
# The count field that governs the record length sits at bytes 125-127, inside
# the fixed portion. Any operation that keeps the fixed portion therefore keeps
# a length declaration that the occurrences may no longer justify.

#: Bytes before the first BD-ELEMENT occurrence.
BILL_DETAIL_FIXED_BYTES = 127

#: Bytes in one BD-ELEMENT occurrence.
BILL_ELEMENT_BYTES = 38


def elements_carried_by(image_bytes: int) -> int:
    """How many *whole* occurrences fit inside a fixed image of this width.

    Flattening a variable-length record into a fixed field is the one
    operation in this estate that can lose business data without losing a
    record, so the harness needs to be able to say where the cut falls for
    any given width rather than for one width it happens to know about.
    """
    return max(0, (image_bytes - BILL_DETAIL_FIXED_BYTES) // BILL_ELEMENT_BYTES)


#: The width of the fixed image the bill sequencing step builds for its own
#: internal sort. It is narrower than the record it is imaging.
BILL_SORT_IMAGE_BYTES = 1204

#: 1204 - 127 = 1077 bytes of occurrence space; 1077 // 38 = 28. Position 28
#: is therefore the last one such an image reproduces whole: the 13 bytes left
#: over hold the code and part of the quantity of the 29th and nothing else.
BILL_ELEMENT_BOUNDARY = elements_carried_by(BILL_SORT_IMAGE_BYTES)

LEVEL_NAMES = {
    "L1": "Record",
    "L2": "Field",
    "L3": "Control",
    "L4": "Bill",
    "L5": "Settlement",
}


# ---------------------------------------------------------------------------
# Variance model
# ---------------------------------------------------------------------------


@dataclass
class Variance:
    """One difference between the legacy side and the candidate side.

    A variance is a *fact*. It carries no verdict; classification is
    ``verdict.py``'s job and happens later, from a separately loaded answer
    key. Keeping the two apart is what makes a blind run possible.
    """

    level: str
    kind: str
    dataset: str
    layout: str
    key: str
    field: Optional[str] = None
    legacy: Optional[str] = None
    candidate: Optional[str] = None
    delta: Optional[str] = None
    context: Dict[str, Any] = dc_field(default_factory=dict)

    def signature(self) -> str:
        """A stable short form used for grouping in the report."""
        return "%s/%s/%s/%s" % (self.level, self.kind, self.layout, self.field or "-")

    def to_json(self) -> Dict[str, Any]:
        return asdict(self)


@dataclass
class LevelResult:
    level: str
    name: str
    ran: bool = False
    skipped_reason: Optional[str] = None
    records_compared: int = 0
    fields_compared: int = 0
    variances: List[Variance] = dc_field(default_factory=list)
    stats: Dict[str, Any] = dc_field(default_factory=dict)

    @property
    def clean(self) -> bool:
        return self.ran and not self.variances

    def to_json(self) -> Dict[str, Any]:
        return {
            "level": self.level,
            "name": self.name,
            "ran": self.ran,
            "skipped_reason": self.skipped_reason,
            "records_compared": self.records_compared,
            "fields_compared": self.fields_compared,
            "variance_count": len(self.variances),
            "stats": self.stats,
            "variances": [v.to_json() for v in self.variances],
        }


@dataclass
class ComparisonResult:
    levels: Dict[str, LevelResult] = dc_field(default_factory=dict)
    datasets: List[Dict[str, Any]] = dc_field(default_factory=list)

    def all_variances(self) -> List[Variance]:
        out: List[Variance] = []
        for level in LEVELS:
            if level in self.levels:
                out.extend(self.levels[level].variances)
        return out

    def to_json(self) -> Dict[str, Any]:
        return {
            "levels": {k: v.to_json() for k, v in self.levels.items()},
            "datasets": self.datasets,
        }


# ---------------------------------------------------------------------------
# Keyed index over a canonical NDJSON file
# ---------------------------------------------------------------------------


class NdjsonIndex:
    """Key -> [(ordinal, byte offset)] over a canonical NDJSON file.

    Holding offsets rather than records is what lets the engine compare a
    half-million-record generation without holding it in memory.
    """

    def __init__(self, path: Path | str) -> None:
        self.path = Path(path)
        self.by_key: Dict[str, List[Tuple[int, int]]] = defaultdict(list)
        self.count = 0
        self._fh = None
        self._build()

    def _build(self) -> None:
        offset = 0
        with self.path.open("rb") as fh:
            for line in fh:
                if line.strip():
                    # Only the key is parsed during indexing. Full parsing of
                    # every record on both sides would double the work for
                    # records L1 is about to discard anyway.
                    key = _extract_key(line)
                    self.by_key[key].append((self.count, offset))
                    self.count += 1
                offset += len(line)

    def keys(self) -> Set[str]:
        return set(self.by_key)

    def open(self) -> None:
        if self._fh is None:
            self._fh = self.path.open("rb")

    def close(self) -> None:
        if self._fh is not None:
            self._fh.close()
            self._fh = None

    def read(self, offset: int) -> CanonicalRecord:
        self.open()
        assert self._fh is not None
        self._fh.seek(offset)
        line = self._fh.readline()
        return CanonicalRecord.from_json(json.loads(line))

    def records_for(self, key: str) -> List[CanonicalRecord]:
        return [self.read(off) for _ordinal, off in self.by_key.get(key, [])]

    def __enter__(self) -> "NdjsonIndex":
        self.open()
        return self

    def __exit__(self, *exc: Any) -> None:
        self.close()


_KEY_MARK = b'"_key":"'


def _extract_key(line: bytes) -> str:
    """Pull ``_key`` out of a canonical line without parsing the whole object."""
    start = line.find(_KEY_MARK)
    if start < 0:
        return json.loads(line)["_key"]
    start += len(_KEY_MARK)
    end = line.find(b'"', start)
    if end < 0:
        return json.loads(line)["_key"]
    return line[start:end].decode("utf-8")


# ---------------------------------------------------------------------------
# L1 -- Record
# ---------------------------------------------------------------------------


def compare_l1(
    legacy: NdjsonIndex,
    candidate: NdjsonIndex,
    spec: DatasetSpec,
    max_reported: int = 500,
) -> LevelResult:
    """Missing, extra and duplicated records after canonicalisation."""
    result = LevelResult("L1", LEVEL_NAMES["L1"], ran=True)
    legacy_keys = legacy.keys()
    candidate_keys = candidate.keys()
    witness = [w.upper() for w in spec.witness_fields]

    def witness_for(index: "NdjsonIndex", key: str) -> Dict[str, Any]:
        """Discriminating field values from the record behind a key.

        An L1 finding that says only "this key is missing" cannot be
        attributed to anything. Carrying the contract's witness fields is
        what lets a later stage say "and it was a fatal-status record".
        """
        if not witness:
            return {}
        entries = index.by_key.get(key) or []
        if not entries:
            return {}
        record = index.read(entries[0][1])
        return {name: record.value(name) for name in witness if record.value(name) is not None}

    missing = legacy_keys - candidate_keys
    extra = candidate_keys - legacy_keys
    common = legacy_keys & candidate_keys

    legacy.open()
    candidate.open()
    try:
        for key in sorted(missing)[:max_reported]:
            result.variances.append(
                Variance(
                    level="L1",
                    kind="missing_record",
                    dataset=spec.name,
                    layout=spec.layout,
                    key=key,
                    legacy=str(len(legacy.by_key[key])),
                    candidate="0",
                    context={"key_fields": spec.key, "witness": witness_for(legacy, key)},
                )
            )
        for key in sorted(extra)[:max_reported]:
            result.variances.append(
                Variance(
                    level="L1",
                    kind="extra_record",
                    dataset=spec.name,
                    layout=spec.layout,
                    key=key,
                    legacy="0",
                    candidate=str(len(candidate.by_key[key])),
                    context={"key_fields": spec.key, "witness": witness_for(candidate, key)},
                )
            )
    finally:
        legacy.close()
        candidate.close()

    dup_legacy = 0
    dup_candidate = 0
    count_mismatch = 0
    # Sorted, not set order: a variance list whose order changes between runs
    # cannot be diffed, and diffing a blind run against an attributed one is
    # how the harness demonstrates it was not tuned to the answer.
    for key in sorted(common):
        ln = len(legacy.by_key[key])
        cn = len(candidate.by_key[key])
        if ln > 1:
            dup_legacy += ln - 1
        if cn > 1:
            dup_candidate += cn - 1
        if ln != cn and count_mismatch < max_reported:
            count_mismatch += 1
            result.variances.append(
                Variance(
                    level="L1",
                    kind="duplicate_count_mismatch",
                    dataset=spec.name,
                    layout=spec.layout,
                    key=key,
                    legacy=str(ln),
                    candidate=str(cn),
                    delta=str(cn - ln),
                    context={"key_fields": spec.key},
                )
            )

    result.records_compared = legacy.count
    result.stats = {
        "legacy_records": legacy.count,
        "candidate_records": candidate.count,
        "legacy_distinct_keys": len(legacy_keys),
        "candidate_distinct_keys": len(candidate_keys),
        "missing_in_candidate": len(missing),
        "extra_in_candidate": len(extra),
        "duplicate_keys_legacy": dup_legacy,
        "duplicate_keys_candidate": dup_candidate,
        "reported_cap": max_reported,
        "truncated": len(missing) > max_reported or len(extra) > max_reported,
    }
    return result


# ---------------------------------------------------------------------------
# L2 -- Field
# ---------------------------------------------------------------------------


def _tolerance_for(spec: DatasetSpec, field: str) -> Optional[Decimal]:
    """Absolute tolerance for a field, or ``None`` meaning exact.

    Exact is the default. A tolerance exists only where the contract says
    so, and the contract has to say why -- a tolerance introduced to make a
    comparison pass is how a defect becomes permanent -- a tolerance wide
    enough to absorb a precision mismatch will absorb it for decades.
    """
    entry = spec.tolerances.get(field) or spec.tolerances.get(field.upper())
    if not entry:
        return None
    if "abs" not in entry:
        return None
    return Decimal(str(entry["abs"]))


def compare_l2(
    legacy: NdjsonIndex,
    candidate: NdjsonIndex,
    spec: DatasetSpec,
    max_reported: int = 2000,
) -> LevelResult:
    """Typed field-by-field comparison of every record present on both sides."""
    result = LevelResult("L2", LEVEL_NAMES["L2"], ran=True)
    ignore = {n.upper() for n in spec.ignore_fields}
    common = legacy.keys() & candidate.keys()
    reported = 0
    field_hits: Dict[str, int] = defaultdict(int)
    penny_totals: Dict[str, Decimal] = defaultdict(lambda: Decimal(0))

    legacy.open()
    candidate.open()
    try:
        for key in sorted(common):
            l_recs = legacy.records_for(key)
            c_recs = candidate.records_for(key)
            for l_rec, c_rec in zip(l_recs, c_recs):
                result.records_compared += 1
                names = set(l_rec.fields) | set(c_rec.fields)
                for name in sorted(names):
                    if name.upper() in ignore:
                        continue
                    l_entry = l_rec.fields.get(name)
                    c_entry = c_rec.fields.get(name)
                    result.fields_compared += 1
                    variance = _compare_field(spec, key, name, l_entry, c_entry)
                    if variance is None:
                        continue
                    if spec.witness_fields:
                        variance.context["witness"] = {
                            w.upper(): l_rec.value(w.upper())
                            for w in spec.witness_fields
                            if l_rec.value(w.upper()) is not None
                        }
                    field_hits[name] += 1
                    if variance.delta is not None:
                        try:
                            penny_totals[name] += Decimal(variance.delta)
                        except Exception:
                            pass
                    if reported < max_reported:
                        result.variances.append(variance)
                        reported += 1
    finally:
        legacy.close()
        candidate.close()

    result.stats = {
        "keys_compared": len(common),
        "fields_with_variances": dict(sorted(field_hits.items(), key=lambda kv: -kv[1])),
        "net_delta_by_field": {k: str(v) for k, v in sorted(penny_totals.items())},
        "reported_cap": max_reported,
        "truncated": sum(field_hits.values()) > max_reported,
        "total_field_variances": sum(field_hits.values()),
    }
    return result


def _compare_field(
    spec: DatasetSpec,
    key: str,
    name: str,
    l_entry: Optional[Dict[str, Any]],
    c_entry: Optional[Dict[str, Any]],
) -> Optional[Variance]:
    if l_entry is None or c_entry is None:
        return Variance(
            level="L2",
            kind="field_absent",
            dataset=spec.name,
            layout=spec.layout,
            key=key,
            field=name,
            legacy=None if l_entry is None else l_entry.get("v"),
            candidate=None if c_entry is None else c_entry.get("v"),
            context={"note": "field present on one side only"},
        )

    l_type = l_entry.get("t")
    c_type = c_entry.get("t")
    if l_type != c_type:
        return Variance(
            level="L2",
            kind="type_mismatch",
            dataset=spec.name,
            layout=spec.layout,
            key=key,
            field=name,
            legacy="%s:%s" % (l_type, l_entry.get("v")),
            candidate="%s:%s" % (c_type, c_entry.get("v")),
            context={"legacy_pic": l_entry.get("pic"), "candidate_pic": c_entry.get("pic")},
        )

    if l_type == T_DECIMAL:
        l_scale = l_entry.get("s")
        c_scale = c_entry.get("s")
        l_val = Decimal(l_entry["v"])
        c_val = Decimal(c_entry["v"])
        if l_scale != c_scale:
            # A scale change is a finding in its own right even when the
            # values happen to be equal today: it is a declared loss of
            # precision that will bite on some later value.
            return Variance(
                level="L2",
                kind="scale_mismatch",
                dataset=spec.name,
                layout=spec.layout,
                key=key,
                field=name,
                legacy="%s (scale %s)" % (l_val, l_scale),
                candidate="%s (scale %s)" % (c_val, c_scale),
                delta=str(c_val - l_val),
                context={"legacy_pic": l_entry.get("pic"), "candidate_pic": c_entry.get("pic")},
            )
        if l_val == c_val:
            return None
        delta = c_val - l_val
        tolerance = _tolerance_for(spec, name)
        if tolerance is not None and abs(delta) <= tolerance:
            return None
        return Variance(
            level="L2",
            kind="decimal_value",
            dataset=spec.name,
            layout=spec.layout,
            key=key,
            field=name,
            legacy=str(l_val),
            candidate=str(c_val),
            delta=str(delta),
            context={
                "pic": l_entry.get("pic"),
                "usage": l_entry.get("u"),
                "scale": l_scale,
                "tolerance": str(tolerance) if tolerance is not None else "exact",
            },
        )

    if l_type == T_JULIAN:
        if l_entry.get("v") == c_entry.get("v"):
            return None
        return Variance(
            level="L2",
            kind="julian_date",
            dataset=spec.name,
            layout=spec.layout,
            key=key,
            field=name,
            legacy="%s (%s)" % (l_entry.get("v"), l_entry.get("iso")),
            candidate="%s (%s)" % (c_entry.get("v"), c_entry.get("iso")),
            context={
                "legacy_iso": l_entry.get("iso"),
                "candidate_iso": c_entry.get("iso"),
                "note": "raw YYDDD is authoritative; ISO is derived",
            },
        )

    if l_entry.get("v") == c_entry.get("v"):
        return None
    return Variance(
        level="L2",
        kind="string_value" if l_type != "raw" else "undecodable_bytes",
        dataset=spec.name,
        layout=spec.layout,
        key=key,
        field=name,
        legacy=l_entry.get("v"),
        candidate=c_entry.get("v"),
        context={"pic": l_entry.get("pic"), "usage": l_entry.get("u")},
    )


# ---------------------------------------------------------------------------
# L3 -- Control
# ---------------------------------------------------------------------------

_BALANCE_TERMS = ("CT-WRITTEN", "CT-REJECTED", "CT-SUMMARISED", "CT-CARRIED-FWD")
_HASH_FIELDS = ("CT-HASH-MINUTES", "CT-HASH-AMOUNT", "CT-HASH-SEQ", "CT-HASH-OCN")


def bill_detail_written_volume(records: Iterable[CanonicalRecord]) -> Dict[str, Any]:
    """What a bill detail dataset weighs, as the access method would see it.

    Three figures and a digest. The record count and the occurrence count are
    the two things the estate's own control record could in principle carry;
    the byte figure is what those two imply, because a CABSBILL record is
    written at the length its own count field declares -- 127 bytes plus 38
    for every occurrence it says it has. The digest is over the occurrence
    payload itself.

    Asserting all four is the point. The first three can agree exactly while
    the fourth does not, and when that happens the two sides wrote the same
    number of records, of the same declared lengths, containing different
    charges -- which no counter in ``CABSCTL`` is capable of noticing.
    """
    records_seen = 0
    elements = 0
    byte_volume = 0
    above_boundary = 0
    digest = hashlib.sha256()
    for rec in records:
        count = min(_elem_count(rec), 40)
        records_seen += 1
        elements += count
        byte_volume += BILL_DETAIL_FIXED_BYTES + count * BILL_ELEMENT_BYTES
        if count > BILL_ELEMENT_BOUNDARY:
            above_boundary += 1
        digest.update(rec.key.encode("utf-8"))
        digest.update(b"\x00")
        for index in range(1, count + 1):
            signature = _element_signature(rec, index)
            digest.update(("|".join(signature) if signature else "").encode("utf-8"))
            digest.update(b"\x00")
    return {
        "records": records_seen,
        "declared_elements": elements,
        "declared_bytes": byte_volume,
        "records_above_element_boundary": above_boundary,
        "element_payload_sha256": digest.hexdigest(),
    }


def compare_l3(
    legacy_records: Sequence[CanonicalRecord],
    candidate_records: Sequence[CanonicalRecord],
    chain: Sequence[Dict[str, Any]],
    dataset_name: str = "TELCABS.CABS.CONTROL",
    legacy_written_volume: Optional[Dict[str, Any]] = None,
    candidate_written_volume: Optional[Dict[str, Any]] = None,
    volume_dataset: str = "TELCABS.CABS.BILLDTL.SEQ",
) -> LevelResult:
    """Verify the balancing equation and the hash chain.

    ``CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARISED + CT-CARRIED-FWD``
    must hold on every control record on **both** sides -- an out-of-balance
    legacy run is a finding about the legacy, not an excuse to skip the
    check. The chain then asserts that each process read what its
    predecessor wrote, and that the hash totals the contract declares as
    carried are actually carried.
    """
    result = LevelResult("L3", LEVEL_NAMES["L3"], ran=True)

    def check_balance(records: Sequence[CanonicalRecord], side: str) -> int:
        broken = 0
        for rec in records:
            read = rec.decimal("CT-READ")
            if read is None:
                continue
            terms = [rec.decimal(t) or Decimal(0) for t in _BALANCE_TERMS]
            total = sum(terms, Decimal(0))
            declared = (rec.value("CT-BAL-IND") or " ").strip()
            if read != total:
                broken += 1
                result.variances.append(
                    Variance(
                        level="L3",
                        kind="balance_equation_failed",
                        dataset=dataset_name,
                        layout="CABSCTL",
                        key=rec.key,
                        field="CT-READ",
                        legacy=str(read) if side == "legacy" else None,
                        candidate=str(read) if side == "candidate" else None,
                        delta=str(total - read),
                        context={
                            "side": side,
                            "equation": "CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARISED + CT-CARRIED-FWD",
                            "terms": {t: str(rec.decimal(t)) for t in _BALANCE_TERMS},
                            "sum_of_terms": str(total),
                            "CT-BAL-IND": declared,
                            "note": (
                                "the record already declares itself out of balance"
                                if declared == "O"
                                else "the record declares itself in balance and is not"
                            ),
                        },
                    )
                )
        return broken

    legacy_broken = check_balance(legacy_records, "legacy")
    candidate_broken = check_balance(candidate_records, "candidate")

    # Per-process comparison of counts and hash totals.
    l_by_key = {r.key: r for r in legacy_records}
    c_by_key = {r.key: r for r in candidate_records}
    for key in sorted(set(l_by_key) & set(c_by_key)):
        l_rec, c_rec = l_by_key[key], c_by_key[key]
        result.records_compared += 1
        for name in ("CT-READ",) + _BALANCE_TERMS + _HASH_FIELDS:
            l_val, c_val = l_rec.decimal(name), c_rec.decimal(name)
            result.fields_compared += 1
            if l_val is None or c_val is None or l_val == c_val:
                continue
            result.variances.append(
                Variance(
                    level="L3",
                    kind="control_total",
                    dataset=dataset_name,
                    layout="CABSCTL",
                    key=key,
                    field=name,
                    legacy=str(l_val),
                    candidate=str(c_val),
                    delta=str(c_val - l_val),
                    context={
                        "process": (l_rec.value("CT-PROCESS-ID") or "").strip(),
                        "step": (l_rec.value("CT-STEP-SEQ") or "").strip(),
                    },
                )
            )

    # Hash and count chaining between processes.
    chain_breaks = 0
    for side, records in (("legacy", legacy_records), ("candidate", candidate_records)):
        by_process: Dict[str, CanonicalRecord] = {}
        for rec in records:
            pid = (rec.value("CT-PROCESS-ID") or "").strip()
            if pid:
                by_process.setdefault(pid, rec)
        for edge in chain:
            src, dst = edge["from"], edge["to"]
            if src not in by_process or dst not in by_process:
                continue
            s_rec, d_rec = by_process[src], by_process[dst]
            expected = s_rec.decimal(edge.get("source_field", "CT-WRITTEN"))
            actual = d_rec.decimal(edge.get("target_field", "CT-READ"))
            if expected is None or actual is None:
                continue
            if expected != actual:
                chain_breaks += 1
                result.variances.append(
                    Variance(
                        level="L3",
                        kind="chain_break",
                        dataset=dataset_name,
                        layout="CABSCTL",
                        key="%s->%s" % (src, dst),
                        field=edge.get("target_field", "CT-READ"),
                        legacy=str(expected) if side == "legacy" else None,
                        candidate=str(actual) if side == "candidate" else None,
                        delta=str(actual - expected),
                        context={
                            "side": side,
                            "from_process": src,
                            "to_process": dst,
                            "note": edge.get("note", ""),
                        },
                    )
                )
            for hash_field in edge.get("carried_hashes", []):
                s_hash, d_hash = s_rec.decimal(hash_field), d_rec.decimal(hash_field)
                if s_hash is None or d_hash is None or s_hash == d_hash:
                    continue
                chain_breaks += 1
                result.variances.append(
                    Variance(
                        level="L3",
                        kind="hash_chain_break",
                        dataset=dataset_name,
                        layout="CABSCTL",
                        key="%s->%s" % (src, dst),
                        field=hash_field,
                        legacy=str(s_hash) if side == "legacy" else None,
                        candidate=str(d_hash) if side == "candidate" else None,
                        delta=str(d_hash - s_hash),
                        context={"side": side, "from_process": src, "to_process": dst},
                    )
                )

    # Written volume of the variable-length detail the billing steps produce.
    # Supplied by the caller when the datasets exist; this level runs without
    # it, and says so, rather than pretending the assertion was made.
    volume_stats: Dict[str, Any] = {"written_volume_compared": False}
    if legacy_written_volume is not None and candidate_written_volume is not None:
        volume_stats = {
            "written_volume_compared": True,
            "legacy_written_volume": dict(legacy_written_volume),
            "candidate_written_volume": dict(candidate_written_volume),
        }
        pairs = (
            ("written_record_count", "records"),
            ("written_element_count", "declared_elements"),
            ("written_byte_volume", "declared_bytes"),
        )
        agreement: Dict[str, bool] = {}
        for kind, key in pairs:
            left = legacy_written_volume.get(key)
            right = candidate_written_volume.get(key)
            agreement[key] = left == right
            if left is None or right is None or left == right:
                continue
            result.variances.append(
                Variance(
                    level="L3",
                    kind=kind,
                    dataset=volume_dataset,
                    layout="CABSBILL",
                    key=volume_dataset,
                    field=key,
                    legacy=str(left),
                    candidate=str(right),
                    delta=str(right - left),
                    context={
                        "note": (
                            "a variable-length record is written at the length its own count "
                            "field declares, so this figure moves when the counts move"
                        ),
                    },
                )
            )
        left_digest = legacy_written_volume.get("element_payload_sha256")
        right_digest = candidate_written_volume.get("element_payload_sha256")
        if left_digest and right_digest and left_digest != right_digest:
            counters_agree = (
                agreement.get("records", False)
                and agreement.get("declared_elements", False)
                and agreement.get("declared_bytes", False)
            )
            result.variances.append(
                Variance(
                    level="L3",
                    kind="written_content_digest",
                    dataset=volume_dataset,
                    layout="CABSBILL",
                    key=volume_dataset,
                    field="element_payload_sha256",
                    legacy=left_digest,
                    candidate=right_digest,
                    delta=None,
                    context={
                        "counters_agree": counters_agree,
                        "note": (
                            "the two sides wrote the same number of records, the same declared "
                            "occurrence count and the same number of bytes, and not the same "
                            "bytes -- nothing in CABSCTL can express that difference"
                            if counters_agree
                            else "content and volume both differ; read the volume findings first"
                        ),
                        "witness": {
                            "byte_volume_agrees": "Y" if agreement.get("declared_bytes") else "N",
                            "element_count_agrees": (
                                "Y" if agreement.get("declared_elements") else "N"
                            ),
                            "record_count_agrees": "Y" if agreement.get("records") else "N",
                            "records_above_element_boundary": str(
                                max(
                                    int(legacy_written_volume.get("records_above_element_boundary", 0)),
                                    int(candidate_written_volume.get("records_above_element_boundary", 0)),
                                )
                            ),
                        },
                    },
                )
            )

    result.stats = {
        "legacy_control_records": len(legacy_records),
        "candidate_control_records": len(candidate_records),
        "legacy_out_of_balance": legacy_broken,
        "candidate_out_of_balance": candidate_broken,
        "chain_edges_checked": len(chain),
        "chain_breaks": chain_breaks,
    }
    result.stats.update(volume_stats)
    return result


# ---------------------------------------------------------------------------
# L4 -- Bill
# ---------------------------------------------------------------------------


def _aggregate(
    records: Iterable[CanonicalRecord],
    group_fields: Sequence[str],
    money_fields: Sequence[str],
) -> Dict[str, Dict[str, Decimal]]:
    totals: Dict[str, Dict[str, Decimal]] = defaultdict(lambda: defaultdict(lambda: Decimal(0)))
    for rec in records:
        group = "|".join((rec.value(f) or "").strip() for f in group_fields)
        bucket = totals[group]
        for money in money_fields:
            value = rec.decimal(money)
            if value is not None:
                bucket[money] += value
        bucket["_count"] += Decimal(1)
    return {k: dict(v) for k, v in totals.items()}


def _diff_aggregates(
    level: str,
    kind: str,
    dataset: str,
    layout: str,
    legacy: Dict[str, Dict[str, Decimal]],
    candidate: Dict[str, Dict[str, Decimal]],
    grouping: str,
    max_reported: int,
) -> Tuple[List[Variance], Decimal]:
    variances: List[Variance] = []
    net = Decimal(0)
    for group in sorted(set(legacy) | set(candidate)):
        l_bucket = legacy.get(group, {})
        c_bucket = candidate.get(group, {})
        for money in sorted(set(l_bucket) | set(c_bucket)):
            l_val = l_bucket.get(money, Decimal(0))
            c_val = c_bucket.get(money, Decimal(0))
            if l_val == c_val:
                continue
            delta = c_val - l_val
            if money != "_count":
                net += delta
            if len(variances) < max_reported:
                variances.append(
                    Variance(
                        level=level,
                        kind=kind,
                        dataset=dataset,
                        layout=layout,
                        key=group,
                        field=money,
                        legacy=str(l_val),
                        candidate=str(c_val),
                        delta=str(delta),
                        context={"grouping": grouping},
                    )
                )
    return variances, net


# ---------------------------------------------------------------------------
# L4 -- per-line-item occurrence assertions
# ---------------------------------------------------------------------------
#
# The three assertions below are made on each side *independently*, like the
# meet-point split checks at L5, because they are properties a bill detail
# file has to satisfy on its own account. A comparison that only diffs the two
# sides cannot make them: if both sides carry the same shape of error the diff
# is clean and the bill is still wrong, and if only one side carries it the
# diff says "these numbers differ" without saying which side to believe.


def _elem_count(rec: CanonicalRecord) -> int:
    value = rec.decimal("BD-ELEM-CNT")
    try:
        return int(value) if value is not None else 0
    except (TypeError, ValueError):
        return 0


def _element_signature(rec: CanonicalRecord, index: int) -> Optional[Tuple[str, str, str, str]]:
    """Code, quantity, rate and amount of one occurrence, or ``None``.

    Four fields rather than one: a rate element code on its own repeats all
    over a bill quite legitimately, and an amount on its own repeats whenever
    two lines happen to cost the same. The four together identify a specific
    charge.
    """
    code = rec.value("BD-EL-RATE-ELEM(%d)" % index)
    if code is None:
        return None

    def part(name: str) -> str:
        value = rec.decimal("%s(%d)" % (name, index))
        return "" if value is None else str(value)

    return (code.strip(), part("BD-EL-QTY"), part("BD-EL-RATE"), part("BD-EL-AMOUNT"))


def _element_sum(rec: CanonicalRecord, first: int, last: int) -> Optional[Decimal]:
    """Sum of BD-EL-AMOUNT over ``first``..``last``, or ``None`` if incomplete."""
    total = Decimal(0)
    for index in range(first, last + 1):
        value = rec.decimal("BD-EL-AMOUNT(%d)" % index)
        if value is None:
            return None
        total += value
    return total


def _line_items_by_account(
    records: Iterable[CanonicalRecord],
) -> Dict[Tuple[str, str], List[Tuple[Decimal, CanonicalRecord]]]:
    """Detail lines grouped by BAN and bill period, in line-sequence order."""
    groups: Dict[Tuple[str, str], List[Tuple[Decimal, CanonicalRecord]]] = defaultdict(list)
    for rec in records:
        ban = (rec.value("BD-BAN") or "").strip()
        period = (rec.value("BD-BILL-PERIOD") or "").strip()
        seq = rec.decimal("BD-LINE-SEQ")
        groups[(ban, period)].append((seq if seq is not None else Decimal(0), rec))
    for rows in groups.values():
        rows.sort(key=lambda row: row[0])
    return groups


def _occurrence_residue_check(
    records: Sequence[CanonicalRecord],
    side: str,
    boundary: int,
    max_reported: int,
) -> Tuple[List[Variance], Dict[str, Any]]:
    """Do the occurrences past ``boundary`` belong to the line that carries them?

    A line that declares more occurrences than a fixed image of the record
    would hold is a line whose tail could have been supplied by whatever last
    occupied that storage. The one thing that makes such a tail *detectable*
    rather than merely suspicious is where it came from: the bill is ordered
    on account, period, section print order and line sequence, so the storage
    a long line's tail would pick up belongs to the line beside it. An
    occurrence past the boundary that is identical to one on an adjacent line
    is therefore the observable to test for.

    The same test is run at and below the boundary **on the flagged lines
    themselves** and reported separately. Some accounts legitimately repeat a
    charge on consecutive lines, and if a line repeats its neighbour inside
    the part of it that no image could have damaged, then the repetition
    outside that part is a property of the data and proves nothing. The two
    counts have to be read together, which is why both are carried on the
    finding.

    The control is taken from the long lines and not from every line on the
    account, because the repetition is symmetric: a short neighbour whose
    occurrences reappear in a long line's tail would otherwise be counted as
    evidence against the very finding it is evidence for.
    """
    variances: List[Variance] = []
    lines_above = 0
    lines_flagged = 0
    within_boundary_repeats = 0
    occurrences_flagged = 0

    for (ban, period), rows in sorted(_line_items_by_account(records).items()):
        prepared: List[Tuple[Decimal, CanonicalRecord, int, List[Optional[Tuple[str, str, str, str]]]]] = []
        for seq, rec in rows:
            count = min(_elem_count(rec), 40)
            prepared.append((seq, rec, count, [_element_signature(rec, i) for i in range(1, count + 1)]))

        for position, (seq, rec, count, signatures) in enumerate(prepared):
            neighbours: Set[Tuple[str, str, str, str]] = set()
            for other in (position - 1, position + 1):
                if 0 <= other < len(prepared):
                    neighbours.update(s for s in prepared[other][3] if s is not None)
            if not neighbours:
                continue

            if count <= boundary:
                continue
            below = [
                i + 1
                for i in range(0, boundary)
                if signatures[i] is not None and signatures[i] in neighbours
            ]
            within_boundary_repeats += len(below)
            lines_above += 1
            above = [
                i + 1
                for i in range(boundary, count)
                if signatures[i] is not None and signatures[i] in neighbours
            ]
            if not above:
                continue
            lines_flagged += 1
            occurrences_flagged += len(above)
            if len(variances) >= max_reported:
                continue
            variances.append(
                Variance(
                    level="L4",
                    kind="bill_line_element_residue",
                    dataset="CABSBILL",
                    layout="CABSBILL",
                    key="%s|%s|%s" % (ban, period, seq),
                    field="BD-EL-RATE-ELEM(%d)" % above[0],
                    legacy=str(len(above)) if side == "legacy" else None,
                    candidate=str(len(above)) if side == "candidate" else None,
                    delta=str(len(above)),
                    context={
                        "side": side,
                        "boundary": boundary,
                        "occurrences_repeating_a_neighbour": above,
                        "section": (rec.value("BD-SECTION") or "").strip(),
                        "witness": {
                            "BD-ELEM-CNT": str(count),
                            "BD-SECTION": (rec.value("BD-SECTION") or "").strip(),
                            "duplicates_within_boundary": str(len(below)),
                        },
                    },
                )
            )

    stats = {
        "lines_above_element_boundary": lines_above,
        "lines_repeating_a_neighbour_above_boundary": lines_flagged,
        "occurrences_repeating_a_neighbour_above_boundary": occurrences_flagged,
        # counted on the long lines only -- see the note in the docstring
        "occurrences_repeating_a_neighbour_within_boundary": within_boundary_repeats,
    }
    return variances, stats


def _occurrence_sum_check(
    records: Sequence[CanonicalRecord],
    side: str,
    boundary: int,
    max_reported: int,
) -> Tuple[List[Variance], Dict[str, Any]]:
    """Does a line's own total agree with the occurrences printed beneath it?

    The total is in the fixed portion of the record and is computed before
    anything reorders the file; the occurrences are not. So where the two
    disagree the total is the side to believe, and the interesting question is
    whether the disagreement is confined to lines that declare more
    occurrences than a fixed image would hold. Lines at or below the boundary
    are checked in exactly the same way and counted separately: if they fail
    too, the disagreement is something else and this finding must not be read
    as if it were confined.
    """
    variances: List[Variance] = []
    checked = 0
    above_boundary = 0
    mismatches_above = 0
    mismatches_within = 0
    pending: List[Tuple[str, Decimal, Decimal, Decimal, int, Optional[Decimal]]] = []

    for rec in records:
        count = min(_elem_count(rec), 40)
        if count < 1:
            continue
        declared = rec.decimal("BD-TOT-AMOUNT")
        summed = _element_sum(rec, 1, count)
        if declared is None or summed is None:
            continue
        checked += 1
        if summed == declared:
            if count > boundary:
                above_boundary += 1
            continue
        if count <= boundary:
            mismatches_within += 1
            continue
        above_boundary += 1
        mismatches_above += 1
        head = _element_sum(rec, 1, min(count, boundary)) or Decimal(0)
        pending.append(
            (
                "%s|%s|%s"
                % (
                    (rec.value("BD-BAN") or "").strip(),
                    (rec.value("BD-BILL-PERIOD") or "").strip(),
                    rec.decimal("BD-LINE-SEQ"),
                ),
                declared,
                summed,
                head,
                count,
                rec.decimal("BD-TOT-ROUNDED"),
            )
        )

    for key, declared, summed, head, count, rounded in pending[:max_reported]:
        variances.append(
            Variance(
                level="L4",
                kind="bill_line_element_sum_mismatch",
                dataset="CABSBILL",
                layout="CABSBILL",
                key=key,
                field="BD-TOT-AMOUNT",
                legacy=str(declared) if side == "legacy" else None,
                candidate=str(declared) if side == "candidate" else None,
                delta=str(summed - declared),
                context={
                    "side": side,
                    "boundary": boundary,
                    "sum_of_occurrences": str(summed),
                    "sum_within_boundary": str(head),
                    "sum_beyond_boundary": str(summed - head),
                    "line_total_rounded": str(rounded) if rounded is not None else None,
                    "witness": {
                        "BD-ELEM-CNT": str(count),
                        "mismatches_within_boundary": str(mismatches_within),
                    },
                },
            )
        )

    stats = {
        "lines_checked_for_occurrence_sum": checked,
        "lines_above_boundary_checked": above_boundary,
        "occurrence_sum_mismatches_above_boundary": mismatches_above,
        "occurrence_sum_mismatches_within_boundary": mismatches_within,
    }
    return variances, stats


def _detail_to_header_check(
    headers: Sequence[CanonicalRecord],
    details: Sequence[CanonicalRecord],
    side: str,
    boundary: int,
    max_reported: int,
) -> Tuple[List[Variance], Dict[str, Any]]:
    """Does an account's detail still add up to the figure on its header?

    Three quantities per account: the sum of the occurrence amounts, the sum
    of the line totals, and the accumulated figure the header carries. All
    three are meant to be the same number. When the first disagrees with the
    other two the finding records whether the whole of the residual is carried
    by lines above the boundary, because that is what turns "the detail does
    not add up" into a statement about which occurrences are responsible.
    """
    variances: List[Variance] = []
    header_amount: Dict[Tuple[str, str], Decimal] = {}
    for rec in headers:
        ban = (rec.value("BH-BAN") or "").strip()
        period = (rec.value("BH-BILL-PERIOD") or "").strip()
        value = rec.decimal("BH-HASH-AMOUNT")
        if value is not None:
            header_amount[(ban, period)] = value

    accounts_checked = 0
    accounts_failed = 0
    accounts_confined = 0

    for (ban, period), rows in sorted(_line_items_by_account(details).items()):
        occ_total = Decimal(0)
        line_total = Decimal(0)
        residual_above = Decimal(0)
        residual_within = Decimal(0)
        lines_above = 0
        complete = True
        for _seq, rec in rows:
            count = min(_elem_count(rec), 40)
            declared = rec.decimal("BD-TOT-AMOUNT")
            summed = _element_sum(rec, 1, count) if count >= 1 else Decimal(0)
            if declared is None or summed is None:
                complete = False
                break
            occ_total += summed
            line_total += declared
            if count > boundary:
                lines_above += 1
                residual_above += declared - summed
            else:
                residual_within += declared - summed
        if not complete:
            continue
        accounts_checked += 1
        declared_header = header_amount.get((ban, period))
        residual = line_total - occ_total
        if residual == 0 and (declared_header is None or declared_header == line_total):
            continue
        accounts_failed += 1
        confined = residual != 0 and residual_within == 0 and residual_above == residual
        if confined:
            accounts_confined += 1
        if len(variances) >= max_reported:
            continue
        variances.append(
            Variance(
                level="L4",
                kind="bill_detail_does_not_sum_to_header",
                dataset="CABSBILL",
                layout="CABSBILL",
                key="%s|%s" % (ban, period),
                field="BD-EL-AMOUNT",
                legacy=str(occ_total) if side == "legacy" else None,
                candidate=str(occ_total) if side == "candidate" else None,
                delta=str(residual),
                context={
                    "side": side,
                    "boundary": boundary,
                    "sum_of_line_totals": str(line_total),
                    "header_accumulated_amount": (
                        str(declared_header) if declared_header is not None else None
                    ),
                    "residual_on_lines_above_boundary": str(residual_above),
                    "residual_on_lines_within_boundary": str(residual_within),
                    "lines_above_boundary": lines_above,
                    "witness": {
                        "residual_carried_by_lines_above_boundary": "Y" if confined else "N",
                        "lines_above_boundary": str(lines_above),
                    },
                },
            )
        )

    stats = {
        "accounts_checked_detail_to_header": accounts_checked,
        "accounts_detail_does_not_sum_to_header": accounts_failed,
        "accounts_whose_residual_is_confined_above_boundary": accounts_confined,
    }
    return variances, stats


def compare_l4(
    legacy_headers: Sequence[CanonicalRecord],
    candidate_headers: Sequence[CanonicalRecord],
    legacy_details: Sequence[CanonicalRecord],
    candidate_details: Sequence[CanonicalRecord],
    config: Dict[str, Any],
    max_reported: int = 500,
) -> LevelResult:
    """Invoice header, per-carrier, per-BAN, per-rate-element, per-line-item."""
    result = LevelResult("L4", LEVEL_NAMES["L4"], ran=True)
    header_money = config.get(
        "header_money_fields",
        [
            "BH-PRIOR-BAL", "BH-PAYMENTS", "BH-ADJUSTMENTS", "BH-CURR-USAGE",
            "BH-CURR-RECURRING", "BH-CURR-NONRECUR", "BH-RESTATEMENT",
            "BH-SETTLEMENT-NET", "BH-TAX", "BH-TOTAL-DUE",
            "BH-INTERSTATE-AMT", "BH-INTRASTATE-AMT", "BH-LOCAL-AMT",
        ],
    )
    detail_money = config.get(
        "detail_money_fields",
        ["BD-TOT-MINUTES", "BD-TOT-AMOUNT", "BD-TOT-ROUNDED", "BD-ROUND-DELTA"],
    )
    net_total = Decimal(0)

    groupings: List[Tuple[str, Sequence[str], Sequence[CanonicalRecord], Sequence[CanonicalRecord], Sequence[str], str]] = [
        ("invoice", ["BH-BAN", "BH-BILL-PERIOD"], legacy_headers, candidate_headers, header_money, "CABSBHDR"),
        ("carrier", ["BH-OCN"], legacy_headers, candidate_headers, header_money, "CABSBHDR"),
        ("ban", ["BH-BAN"], legacy_headers, candidate_headers, header_money, "CABSBHDR"),
        ("line_item", ["BD-BAN", "BD-BILL-PERIOD", "BD-SECTION", "BD-LINE-SEQ"], legacy_details, candidate_details, detail_money, "CABSBILL"),
        ("rate_element", ["BD-EL-RATE-ELEM(1)"], legacy_details, candidate_details, ["BD-EL-AMOUNT(1)", "BD-EL-QTY(1)"], "CABSBILL"),
    ]

    for name, group_fields, l_recs, c_recs, money_fields, layout in groupings:
        if not l_recs and not c_recs:
            continue
        l_agg = _aggregate(l_recs, group_fields, money_fields)
        c_agg = _aggregate(c_recs, group_fields, money_fields)
        variances, net = _diff_aggregates(
            "L4", "bill_%s" % name, layout, layout, l_agg, c_agg, "+".join(group_fields), max_reported
        )
        result.variances.extend(variances)
        net_total += net
        result.stats["%s_groups" % name] = len(set(l_agg) | set(c_agg))

    # Per-line-item occurrence assertions, made on each side on its own terms.
    # A fixed image of a CABSBILL record only reproduces so many occurrences
    # whole, and the count field that governs the record's written length is
    # not one of the things it loses -- so a line can leave a program still
    # declaring occurrences the program no longer has. These three assertions
    # are what makes that visible; the aggregate diffs above cannot see it,
    # because the line totals they add up are in the part of the record that
    # survives.
    boundary = int(config.get("element_boundary", BILL_ELEMENT_BOUNDARY))
    result.stats["element_boundary"] = boundary
    for side, headers, details in (
        ("legacy", legacy_headers, legacy_details),
        ("candidate", candidate_headers, candidate_details),
    ):
        if not details:
            continue
        for checker in (_occurrence_residue_check, _occurrence_sum_check):
            variances, stats = checker(details, side, boundary, max_reported)
            result.variances.extend(variances)
            result.stats.update({"%s_%s" % (side, k): v for k, v in stats.items()})
        variances, stats = _detail_to_header_check(headers, details, side, boundary, max_reported)
        result.variances.extend(variances)
        result.stats.update({"%s_%s" % (side, k): v for k, v in stats.items()})

    result.records_compared = len(legacy_headers) + len(legacy_details)
    result.stats.update(
        {
            "legacy_headers": len(legacy_headers),
            "candidate_headers": len(candidate_headers),
            "legacy_detail_lines": len(legacy_details),
            "candidate_detail_lines": len(candidate_details),
            "net_money_delta": str(net_total),
        }
    )
    return result


# ---------------------------------------------------------------------------
# L5 -- Settlement
# ---------------------------------------------------------------------------


def compare_l5(
    legacy: Sequence[CanonicalRecord],
    candidate: Sequence[CanonicalRecord],
    config: Dict[str, Any],
    max_reported: int = 500,
) -> LevelResult:
    """Per-counterparty per-period settlement, meet-point splits, restatements."""
    result = LevelResult("L5", LEVEL_NAMES["L5"], ran=True)
    money = config.get(
        "money_fields",
        ["ST-TOTAL-MOU", "ST-BILLABLE-MOU", "ST-CAPPED-MOU", "ST-GROSS-AMT",
         "ST-OUR-SHARE", "ST-THEIR-SHARE", "ST-NET-DUE", "ST-ROUND-RESIDUE"],
    )

    l_agg = _aggregate(legacy, ["ST-COUNTERPARTY-OCN", "ST-SETTLE-PERIOD"], money)
    c_agg = _aggregate(candidate, ["ST-COUNTERPARTY-OCN", "ST-SETTLE-PERIOD"], money)
    variances, net = _diff_aggregates(
        "L5", "settlement_counterparty_period", "CABSSETL", "CABSSETL",
        l_agg, c_agg, "ST-COUNTERPARTY-OCN+ST-SETTLE-PERIOD", max_reported,
    )
    result.variances.extend(variances)

    l_kind = _aggregate(legacy, ["ST-SETTLE-TYPE"], money)
    c_kind = _aggregate(candidate, ["ST-SETTLE-TYPE"], money)
    kind_variances, _ = _diff_aggregates(
        "L5", "settlement_by_kind", "CABSSETL", "CABSSETL",
        l_kind, c_kind, "ST-SETTLE-TYPE", max_reported,
    )
    result.variances.extend(kind_variances)

    # Meet-point split integrity, checked on each side independently: the
    # two filed percentages must sum to 100.00000 and the two shares must
    # sum to the gross. CABSET01 makes the second true by construction even
    # when the first is false, which is why both have to be asserted.
    hundred = Decimal("100.00000")
    split_failures = {"legacy": 0, "candidate": 0}
    pct_failures = {"legacy": 0, "candidate": 0}
    reported = 0
    for side, records in (("legacy", legacy), ("candidate", candidate)):
        for rec in records:
            if (rec.value("ST-SETTLE-TYPE") or "").strip() != "M":
                continue
            our_pct = rec.decimal("ST-OUR-PCT")
            their_pct = rec.decimal("ST-THEIR-PCT")
            gross = rec.decimal("ST-GROSS-AMT")
            our_share = rec.decimal("ST-OUR-SHARE")
            their_share = rec.decimal("ST-THEIR-SHARE")
            if our_pct is not None and their_pct is not None and our_pct + their_pct != hundred:
                pct_failures[side] += 1
                if reported < max_reported:
                    reported += 1
                    result.variances.append(
                        Variance(
                            level="L5",
                            kind="meet_point_percentages_do_not_sum_to_100",
                            dataset="CABSSETL",
                            layout="CABSSETL",
                            key=rec.key,
                            field="ST-OUR-PCT+ST-THEIR-PCT",
                            legacy=str(our_pct + their_pct) if side == "legacy" else None,
                            candidate=str(our_pct + their_pct) if side == "candidate" else None,
                            delta=str(our_pct + their_pct - hundred),
                            context={
                                "side": side,
                                "our_pct": str(our_pct),
                                "their_pct": str(their_pct),
                                "circuit": (rec.value("ST-CIRCUIT-ID") or "").strip(),
                            },
                        )
                    )
            if (
                gross is not None
                and our_share is not None
                and their_share is not None
                and our_share + their_share != gross
            ):
                split_failures[side] += 1
                if reported < max_reported:
                    reported += 1
                    result.variances.append(
                        Variance(
                            level="L5",
                            kind="meet_point_split_does_not_reconcile",
                            dataset="CABSSETL",
                            layout="CABSSETL",
                            key=rec.key,
                            field="ST-OUR-SHARE+ST-THEIR-SHARE",
                            legacy=str(our_share + their_share) if side == "legacy" else None,
                            candidate=str(our_share + their_share) if side == "candidate" else None,
                            delta=str(our_share + their_share - gross),
                            context={"side": side, "gross": str(gross)},
                        )
                    )

    result.records_compared = len(legacy)
    result.stats = {
        "legacy_settlement_records": len(legacy),
        "candidate_settlement_records": len(candidate),
        "counterparty_periods": len(set(l_agg) | set(c_agg)),
        "net_money_delta": str(net),
        "percentage_pairs_not_100": pct_failures,
        "splits_not_reconciling": split_failures,
    }
    return result


# ---------------------------------------------------------------------------
# Driver
# ---------------------------------------------------------------------------


def run_levels(
    levels: Sequence[str],
    pairs: Sequence[Tuple[DatasetSpec, Path, Path]],
    control: Optional[Tuple[List[CanonicalRecord], List[CanonicalRecord], List[Dict[str, Any]]]] = None,
    bill: Optional[Dict[str, Any]] = None,
    settlement: Optional[Dict[str, Any]] = None,
    max_reported: int = 2000,
) -> ComparisonResult:
    """Run the requested levels over the canonicalised datasets."""
    result = ComparisonResult()
    for level in LEVELS:
        result.levels[level] = LevelResult(level, LEVEL_NAMES[level])

    for spec, legacy_path, candidate_path in pairs:
        legacy = NdjsonIndex(legacy_path)
        candidate = NdjsonIndex(candidate_path)
        result.datasets.append(
            {
                "dataset": spec.name,
                "layout": spec.layout,
                "key": spec.key,
                "levels": spec.levels,
                "legacy_records": legacy.count,
                "candidate_records": candidate.count,
            }
        )
        if "L1" in levels and "L1" in spec.levels:
            partial = compare_l1(legacy, candidate, spec, max_reported=max_reported)
            _merge(result.levels["L1"], partial)
        if "L2" in levels and "L2" in spec.levels:
            partial = compare_l2(legacy, candidate, spec, max_reported=max_reported)
            _merge(result.levels["L2"], partial)

    if "L3" in levels:
        if control is None:
            result.levels["L3"].skipped_reason = (
                "no control dataset (CABSCTL) found on both sides; L3 cannot run"
            )
        else:
            legacy_ctl, candidate_ctl, chain = control
            # The written volume of the bill detail is a control-level fact and
            # is asserted here when the datasets exist, because the balancing
            # equation and the hash chain both hold over a file whose records
            # are the right length and the wrong content.
            legacy_volume = candidate_volume = None
            if bill and bill.get("legacy_details") and bill.get("candidate_details"):
                legacy_volume = bill_detail_written_volume(bill["legacy_details"])
                candidate_volume = bill_detail_written_volume(bill["candidate_details"])
            _merge(
                result.levels["L3"],
                compare_l3(
                    legacy_ctl,
                    candidate_ctl,
                    chain,
                    legacy_written_volume=legacy_volume,
                    candidate_written_volume=candidate_volume,
                ),
            )

    if "L4" in levels:
        if not bill:
            result.levels["L4"].skipped_reason = (
                "no bill header/detail datasets (CABSBHDR/CABSBILL) found on both sides; "
                "L4 runs after the billing steps have been executed"
            )
        else:
            _merge(
                result.levels["L4"],
                compare_l4(
                    bill.get("legacy_headers", []),
                    bill.get("candidate_headers", []),
                    bill.get("legacy_details", []),
                    bill.get("candidate_details", []),
                    bill.get("config", {}),
                    max_reported=max_reported,
                ),
            )

    if "L5" in levels:
        if not settlement:
            result.levels["L5"].skipped_reason = (
                "no settlement dataset (CABSSETL) found on both sides; L5 runs after the "
                "settlement steps have been executed"
            )
        else:
            _merge(
                result.levels["L5"],
                compare_l5(
                    settlement.get("legacy", []),
                    settlement.get("candidate", []),
                    settlement.get("config", {}),
                    max_reported=max_reported,
                ),
            )

    return result


def _merge(target: LevelResult, partial: LevelResult) -> None:
    target.ran = target.ran or partial.ran
    target.records_compared += partial.records_compared
    target.fields_compared += partial.fields_compared
    target.variances.extend(partial.variances)
    for k, v in partial.stats.items():
        if k in target.stats and isinstance(v, int) and isinstance(target.stats[k], int):
            target.stats[k] += v
        elif k in target.stats and isinstance(v, dict) and isinstance(target.stats[k], dict):
            merged = dict(target.stats[k])
            for kk, vv in v.items():
                if isinstance(vv, int) and isinstance(merged.get(kk), int):
                    merged[kk] += vv
                else:
                    merged[kk] = vv
            target.stats[k] = merged
        else:
            target.stats[k] = v
