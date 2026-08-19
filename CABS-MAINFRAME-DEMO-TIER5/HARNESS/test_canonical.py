"""Unit tests for the canonical interchange form and the comparison engine.

Run with::

    python3 -m unittest discover -s HARNESS -v
    # or
    python3 HARNESS/test_canonical.py

The decoding tests matter most. If the canonical form loses precision, the
comparison is measuring the canonicaliser rather than the two systems, and
every verdict it produces is worthless. They therefore check:

1.  That the canonical form round-trips a record built field by field, with
    the **declared scale preserved** -- ``0.00`` stays ``0.00`` and a
    five-decimal rate stays five decimals.
2.  That the canonical decoder agrees with a **second, independently
    written** COMP-3 implementation (``_reference_comp3`` below), which
    decodes nibble by nibble with integer shifts.
3.  That exactly one REDEFINES overlay is decoded, chosen by the contract's
    variant rule, and that the other two are absent -- because decoding all
    three produces three contradictory readings of the same 96 bytes.
4.  That a corrupt packed field becomes a *finding* (``t: "raw"`` plus an
    error) rather than an exception, so one bad record cannot take down a
    comparison of half a million.
"""

from __future__ import annotations

import json
import sys
import tempfile
import unittest
from decimal import Decimal
from pathlib import Path

_HERE = Path(__file__).resolve().parent
if str(_HERE) not in sys.path:
    sys.path.insert(0, str(_HERE))

import canonical
import compare
from canonical import DatasetSpec, VariantRule, canonicalise_record, canonicalise_file, read_ndjson, write_ndjson
from compare import NdjsonIndex, Variance, compare_l1, compare_l2, compare_l3, compare_l5
from verdict import BlindRunError, DIVERGENT, DIVERGENT_BY_DESIGN, MATCH, VerdictEngine

import gen_common as gc  # via canonical's path shim

ROOT = _HERE.parent
COPYBOOKS = _HERE.parent / "COPYBOOKS"
SEALED = _HERE.parent / "SEALED"
SIGNATURES = _HERE / "defect_signatures.json"
CONTRACT = _HERE / "contracts" / "compare_contract.json"


# ---------------------------------------------------------------------------
# An independent COMP-3 decoder, written differently on purpose.
# ---------------------------------------------------------------------------


def _reference_comp3(data: bytes, scale: int) -> Decimal:
    """Decode packed decimal with integer shifts and no string handling."""
    magnitude = 0
    sign = 0x0C
    for i, byte in enumerate(data):
        high, low = byte >> 4, byte & 0x0F
        magnitude = magnitude * 10 + high
        if i == len(data) - 1:
            sign = low
        else:
            magnitude = magnitude * 10 + low
    value = Decimal(magnitude).scaleb(-scale).quantize(Decimal(1).scaleb(-scale))
    return -value if sign in (0x0B, 0x0D) else value


CDR_SPEC = DatasetSpec(
    name="TEST.CDR",
    pattern="*.dat",
    layout="CABSCDR",
    levels=["L1", "L2"],
    key=["CD-OCN", "CD-BAN", "CD-SEQ-NBR"],
    recfm="FB",
    lrecl=200,
    variant_rule=VariantRule(
        "CD-REC-TYPE",
        {
            "01": "CD-VOICE-DETAIL", "02": "CD-VOICE-DETAIL", "03": "CD-VOICE-DETAIL",
            "04": "CD-DATA-DETAIL", "05": "CD-SPCL-DETAIL", "06": "CD-SPCL-DETAIL",
            "07": "CD-SPCL-DETAIL", "08": "CD-VOICE-DETAIL",
        },
        "CD-VOICE-DETAIL",
    ),
    ignore_fields=["CD-VARIANT-AREA", "CD-FILLER", "CD-VC-FILLER", "CD-DT-FILLER", "CD-SP-FILLER"],
    witness_fields=["CD-REC-TYPE", "CD-RATE-ELEM", "CD-EDIT-STATUS", "CD-VC-TANDEM-IND", "CD-CONN-YYDDD"],
)


def _voice_record(layouts, **overrides) -> bytes:
    layout = layouts["CABSCDR"]
    b = gc.RecordBuilder(layout)
    values = {
        "CD-OCN": "0288",
        "CD-BAN": "813G1234567X",
        "CD-SEQ-NBR": 42,
        "CD-REC-TYPE": "01",
        "CD-USAGE-TYPE": "M",
        "CD-JURIS-CD": "I",
        "CD-RATE-ELEM": "TANSW ",
        "CD-CONN-YY": 24,
        "CD-CONN-DDD": 274,
        "CD-CONN-HHMMSS": 143005,
        "CD-DISC-YYDDD": "24274",
        "CD-DISC-HHMMSS": 143512,
        "CD-VC-ORIG-NPANXX": 813555,
        "CD-VC-TERM-NPANXX": 404555,
        "CD-VC-ORIG-LATA": 460,
        "CD-VC-TERM-LATA": 426,
        "CD-VC-CONV-MIN": Decimal("100000.00"),
        "CD-VC-CHG-MIN": Decimal("100000.00"),
        "CD-VC-TANDEM-IND": "Y",
        "CD-VC-TRUNK-GRP": "T0001   ",
        "CD-VC-CIC": 288,
        "CD-VC-END-OFFICE": "TAMPFLXA01T",
        "CD-SRC-SYSTEM": "EMI1",
        "CD-LOAD-YYDDD": "24274",
        "CD-EDIT-STATUS": "0",
    }
    values.update(overrides)
    for name, value in values.items():
        b.set(name, value)
    return b.build()


class TestCanonicalDecoding(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.layouts = canonical.load_layouts(COPYBOOKS)

    def _canon(self, data, spec=CDR_SPEC):
        return canonicalise_record(self.layouts["CABSCDR"], data, spec, "legacy", "t.dat", 0)

    def test_schema_and_key(self):
        record = self._canon(_voice_record(self.layouts))
        payload = record.to_json()
        self.assertEqual(payload["_schema"], canonical.SCHEMA)
        self.assertEqual(payload["_layout"], "CABSCDR")
        self.assertEqual(record.key, "0288|813G1234567X |42")

    def test_ebcdic_becomes_utf8(self):
        record = self._canon(_voice_record(self.layouts))
        self.assertEqual(record.value("CD-OCN"), "0288")
        self.assertEqual(record.value("CD-RATE-ELEM"), "TANSW ")

    def test_trailing_spaces_are_preserved_not_stripped(self):
        # A 13-byte BAN holding 12 characters really does have a space in
        # position 13. A target that strips it has changed the key.
        record = self._canon(_voice_record(self.layouts))
        self.assertEqual(record.value("CD-BAN"), "813G1234567X ")
        self.assertEqual(len(record.value("CD-BAN")), 13)

    def test_comp3_becomes_a_decimal_string_with_the_scale_preserved(self):
        record = self._canon(_voice_record(self.layouts, **{"CD-VC-CHG-MIN": Decimal("0.00")}))
        entry = record.fields["CD-VC-CHG-MIN"]
        self.assertEqual(entry["t"], "dec")
        self.assertEqual(entry["s"], 2)
        self.assertEqual(entry["v"], "0.00")  # not "0"
        self.assertEqual(entry["pic"], "S9(07)V9(02)")
        self.assertEqual(entry["u"], "COMP-3")

    def test_five_decimal_scale_survives(self):
        # A special-access record: CD-SP-MPB-PCT is S9(03)V9(05) COMP-3 and
        # overlays the same 96 bytes the voice variant uses.
        record = self._canon(
            _voice_record(
                self.layouts,
                **{"CD-REC-TYPE": "06", "CD-SP-MPB-PCT": Decimal("33.33000")},
            )
        )
        entry = record.fields["CD-SP-MPB-PCT"]
        self.assertEqual(entry["s"], 5)
        self.assertEqual(entry["pic"], "S9(03)V9(05)")
        self.assertEqual(entry["v"], "33.33000")  # trailing zeros retained

    def test_no_float_anywhere_in_the_canonical_form(self):
        record = self._canon(_voice_record(self.layouts))
        for name, entry in record.fields.items():
            self.assertNotIsInstance(entry["v"], float, "%s is a float" % name)
            self.assertIsInstance(entry["v"], str, "%s is not a string" % name)

    def test_every_field_carries_its_declared_pic_and_usage(self):
        record = self._canon(_voice_record(self.layouts))
        for name, entry in record.fields.items():
            self.assertIn("pic", entry, name)
            self.assertIn("u", entry, name)
            self.assertIn("o", entry, name)
            self.assertIn("l", entry, name)

    def test_yyddd_is_kept_raw_alongside_an_iso_date(self):
        record = self._canon(_voice_record(self.layouts))
        entry = record.fields["CD-CONN-YYDDD"]
        self.assertEqual(entry["t"], "jul")
        self.assertEqual(entry["v"], "24274")  # raw, authoritative
        self.assertEqual(entry["iso"], "2024-09-30")  # derived

    def test_impossible_yyddd_keeps_the_raw_value_and_nulls_the_iso(self):
        record = self._canon(_voice_record(self.layouts, **{"CD-DISC-YYDDD": "25366"}))
        entry = record.fields["CD-DISC-YYDDD"]
        self.assertEqual(entry["v"], "25366")
        self.assertIsNone(entry["iso"])

    def test_decoder_agrees_with_an_independent_implementation(self):
        layout = self.layouts["CABSCDR"]
        for value in ("0.00", "1.23", "-1.23", "100000.00", "-9999999.99", "0.01"):
            data = _voice_record(self.layouts, **{"CD-VC-CHG-MIN": Decimal(value)})
            field = layout.field("CD-VC-CHG-MIN")
            chunk = data[field.offset : field.offset + field.length]
            record = self._canon(data)
            self.assertEqual(
                Decimal(record.value("CD-VC-CHG-MIN")),
                _reference_comp3(chunk, 2),
                "canonical decoder disagrees with the reference for %s" % value,
            )

    def test_exactly_one_variant_is_decoded(self):
        record = self._canon(_voice_record(self.layouts))
        self.assertEqual(record.variant, "CD-VOICE-DETAIL")
        self.assertIn("CD-VC-CHG-MIN", record.fields)
        self.assertNotIn("CD-DT-OCTETS-IN", record.fields)
        self.assertNotIn("CD-SP-USOC", record.fields)

    def test_the_variant_rule_selects_the_overlay(self):
        for rec_type, expected, present, absent in (
            ("04", "CD-DATA-DETAIL", "CD-DT-OCTETS-IN", "CD-VC-CHG-MIN"),
            ("06", "CD-SPCL-DETAIL", "CD-SP-USOC", "CD-VC-CHG-MIN"),
            ("02", "CD-VOICE-DETAIL", "CD-VC-CHG-MIN", "CD-DT-OCTETS-IN"),
        ):
            record = self._canon(_voice_record(self.layouts, **{"CD-REC-TYPE": rec_type}))
            self.assertEqual(record.variant, expected)
            self.assertIn(present, record.fields)
            self.assertNotIn(absent, record.fields)

    def test_corrupt_packed_data_is_a_finding_not_an_exception(self):
        layout = self.layouts["CABSCDR"]
        data = bytearray(_voice_record(self.layouts))
        field = layout.field("CD-VC-CHG-MIN")
        data[field.offset : field.offset + 2] = b"\x4b\x5c"  # not decimal nibbles
        record = self._canon(bytes(data))
        entry = record.fields["CD-VC-CHG-MIN"]
        self.assertEqual(entry["t"], "raw")
        self.assertIn("error", entry)
        self.assertEqual(entry["v"], bytes(data[field.offset : field.offset + field.length]).hex())

    def test_ignored_fields_are_not_emitted(self):
        record = self._canon(_voice_record(self.layouts))
        self.assertNotIn("CD-VARIANT-AREA", record.fields)
        self.assertNotIn("CD-FILLER", record.fields)


class TestNdjsonRoundTrip(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.layouts = canonical.load_layouts(COPYBOOKS)

    def test_write_and_read(self):
        with tempfile.TemporaryDirectory() as tmp:
            dat = Path(tmp) / "usage.dat"
            with gc.FixedRecordWriter(dat, 200) as writer:
                for seq in range(1, 6):
                    writer.write(_voice_record(self.layouts, **{"CD-SEQ-NBR": seq}))
            writer.close()
            nd = Path(tmp) / "usage.ndjson"
            count = write_ndjson(
                canonicalise_file(dat, self.layouts["CABSCDR"], CDR_SPEC, "legacy"), nd
            )
            self.assertEqual(count, 5)
            back = list(read_ndjson(nd))
            self.assertEqual(len(back), 5)
            self.assertEqual(back[2].value("CD-SEQ-NBR"), "3")
            self.assertEqual(back[0].side, "legacy")

    def test_index_finds_records_by_key(self):
        with tempfile.TemporaryDirectory() as tmp:
            dat = Path(tmp) / "usage.dat"
            with gc.FixedRecordWriter(dat, 200) as writer:
                for seq in (1, 2, 2, 3):
                    writer.write(_voice_record(self.layouts, **{"CD-SEQ-NBR": seq}))
            writer.close()
            nd = Path(tmp) / "usage.ndjson"
            write_ndjson(canonicalise_file(dat, self.layouts["CABSCDR"], CDR_SPEC, "legacy"), nd)
            index = NdjsonIndex(nd)
            self.assertEqual(index.count, 4)
            self.assertEqual(len(index.keys()), 3)
            self.assertEqual(len(index.by_key["0288|813G1234567X |2"]), 2)
            with index:
                records = index.records_for("0288|813G1234567X |3")
            self.assertEqual(len(records), 1)


class TestComparison(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.layouts = canonical.load_layouts(COPYBOOKS)

    def _build(self, tmp, name, specs):
        dat = Path(tmp) / ("%s.dat" % name)
        with gc.FixedRecordWriter(dat, 200) as writer:
            for overrides in specs:
                writer.write(_voice_record(self.layouts, **overrides))
        writer.close()
        nd = Path(tmp) / ("%s.ndjson" % name)
        write_ndjson(canonicalise_file(dat, self.layouts["CABSCDR"], CDR_SPEC, name), nd)
        return NdjsonIndex(nd)

    def test_l1_identical_files_match(self):
        with tempfile.TemporaryDirectory() as tmp:
            rows = [{"CD-SEQ-NBR": n} for n in range(1, 11)]
            result = compare_l1(self._build(tmp, "legacy", rows), self._build(tmp, "cand", rows), CDR_SPEC)
            self.assertTrue(result.clean)
            self.assertEqual(result.stats["missing_in_candidate"], 0)

    def test_l1_detects_missing_extra_and_duplicates(self):
        with tempfile.TemporaryDirectory() as tmp:
            legacy = self._build(tmp, "legacy", [{"CD-SEQ-NBR": n} for n in (1, 2, 3, 3)])
            cand = self._build(tmp, "cand", [{"CD-SEQ-NBR": n} for n in (1, 3, 4)])
            result = compare_l1(legacy, cand, CDR_SPEC)
            kinds = {v.kind for v in result.variances}
            self.assertIn("missing_record", kinds)
            self.assertIn("extra_record", kinds)
            self.assertIn("duplicate_count_mismatch", kinds)
            self.assertEqual(result.stats["duplicate_keys_legacy"], 1)

    def test_l1_carries_witness_fields(self):
        with tempfile.TemporaryDirectory() as tmp:
            legacy = self._build(tmp, "legacy", [{"CD-SEQ-NBR": 1, "CD-EDIT-STATUS": "8"}])
            cand = self._build(tmp, "cand", [{"CD-SEQ-NBR": 2}])
            result = compare_l1(legacy, cand, CDR_SPEC)
            missing = [v for v in result.variances if v.kind == "missing_record"]
            self.assertEqual(missing[0].context["witness"]["CD-EDIT-STATUS"], "8")

    def test_l2_decimal_comparison_is_exact_by_default(self):
        with tempfile.TemporaryDirectory() as tmp:
            legacy = self._build(tmp, "legacy", [{"CD-VC-CHG-MIN": Decimal("100.00")}])
            cand = self._build(tmp, "cand", [{"CD-VC-CHG-MIN": Decimal("100.01")}])
            result = compare_l2(legacy, cand, CDR_SPEC)
            hits = [v for v in result.variances if v.field == "CD-VC-CHG-MIN"]
            self.assertEqual(len(hits), 1)
            self.assertEqual(hits[0].delta, "0.01")
            self.assertEqual(hits[0].context["tolerance"], "exact")

    def test_l2_tolerance_applies_only_where_the_contract_declares_it(self):
        spec = DatasetSpec(
            **{**CDR_SPEC.__dict__, "tolerances": {"CD-VC-CHG-MIN": {"abs": "0.05"}}}
        )
        with tempfile.TemporaryDirectory() as tmp:
            legacy = self._build(tmp, "legacy", [{"CD-VC-CHG-MIN": Decimal("100.00")}])
            cand = self._build(tmp, "cand", [{"CD-VC-CHG-MIN": Decimal("100.04")}])
            self.assertEqual(len(compare_l2(legacy, cand, spec).variances), 0)
            cand2 = self._build(tmp, "cand2", [{"CD-VC-CHG-MIN": Decimal("100.06")}])
            self.assertEqual(len(compare_l2(legacy, cand2, spec).variances), 1)

    def test_l2_reports_a_julian_difference_as_a_date(self):
        with tempfile.TemporaryDirectory() as tmp:
            legacy = self._build(tmp, "legacy", [{"CD-DISC-YYDDD": "24274"}])
            cand = self._build(tmp, "cand", [{"CD-DISC-YYDDD": "24275"}])
            result = compare_l2(legacy, cand, CDR_SPEC)
            hits = [v for v in result.variances if v.field == "CD-DISC-YYDDD"]
            self.assertEqual(hits[0].kind, "julian_date")
            self.assertIn("2024-09-30", hits[0].legacy)


class TestControlLevel(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.layouts = canonical.load_layouts(COPYBOOKS)

    def _control(self, tmp, name, rows):
        layout = self.layouts["CABSCTL"]
        dat = Path(tmp) / ("%s.dat" % name)
        with gc.FixedRecordWriter(dat, layout.lrecl) as writer:
            for row in rows:
                b = gc.RecordBuilder(layout)
                b.set("CT-RUN-ID", "R240930001")
                b.set("CT-PROCESS-ID", row["process"])
                b.set("CT-STEP-SEQ", row.get("step", 1))
                b.set("CT-READ", row["read"])
                b.set("CT-WRITTEN", row["written"])
                b.set("CT-REJECTED", row.get("rejected", 0))
                b.set("CT-SUMMARISED", row.get("summarised", 0))
                b.set("CT-CARRIED-FWD", row.get("cfwd", 0))
                b.set("CT-HASH-SEQ", row.get("hash_seq", 0))
                b.set("CT-BAL-IND", row.get("bal", "B"))
                writer.write(b.build())
        writer.close()
        spec = DatasetSpec(
            name="CONTROL", pattern="*.dat", layout="CABSCTL", levels=["L3"],
            key=["CT-RUN-ID", "CT-PROCESS-ID", "CT-STEP-SEQ"], lrecl=layout.lrecl,
        )
        return list(canonicalise_file(dat, layout, spec, name))

    def test_balanced_run_is_clean(self):
        with tempfile.TemporaryDirectory() as tmp:
            rows = [{"process": "CABING01", "read": 1000, "written": 950, "rejected": 50}]
            result = compare_l3(self._control(tmp, "l", rows), self._control(tmp, "c", rows), [])
            self.assertTrue(result.clean)
            self.assertEqual(result.stats["legacy_out_of_balance"], 0)

    def test_balancing_equation_failure_is_reported_on_both_sides(self):
        with tempfile.TemporaryDirectory() as tmp:
            # 1000 read, but 950 written + 50 rejected + 50 double-counted
            rows = [{"process": "CABING05", "read": 1000, "written": 1000, "rejected": 50}]
            result = compare_l3(self._control(tmp, "l", rows), self._control(tmp, "c", rows), [])
            failures = [v for v in result.variances if v.kind == "balance_equation_failed"]
            self.assertEqual(len(failures), 2)  # once per side
            self.assertEqual(failures[0].delta, "50")
            self.assertIn("declares itself in balance and is not", failures[0].context["note"])

    def test_chain_break_between_processes(self):
        with tempfile.TemporaryDirectory() as tmp:
            rows = [
                {"process": "CABING01", "read": 1000, "written": 950, "rejected": 50, "step": 1},
                {"process": "CABING02", "read": 900, "written": 900, "step": 2},
            ]
            chain = [{"from": "CABING01", "to": "CABING02", "source_field": "CT-WRITTEN", "target_field": "CT-READ"}]
            result = compare_l3(self._control(tmp, "l", rows), self._control(tmp, "c", rows), chain)
            breaks = [v for v in result.variances if v.kind == "chain_break"]
            self.assertEqual(len(breaks), 2)  # once per side
            self.assertEqual(breaks[0].delta, "-50")


class TestVerdict(unittest.TestCase):
    def _variance(self, **overrides):
        base = dict(
            level="L2", kind="decimal_value", dataset="USAGE.RAW", layout="CABSCDR",
            key="0288|BAN|1", field="CD-VC-CHG-MIN", legacy="100000.00",
            candidate="100000.01", delta="0.01",
            context={"scale": 2, "witness": {"CD-VC-TANDEM-IND": "N", "CD-RATE-ELEM": "ORIGAC"}},
        )
        base.update(overrides)
        return Variance(**base)

    def _engine(self, blind=False):
        engine = VerdictEngine(blind=blind)
        if not blind:
            engine.load_answer_keys(SEALED)
            engine.load_signatures(SIGNATURES)
            contract = json.loads(CONTRACT.read_text(encoding="utf-8"))
            params = contract["parameters"]
            engine.set_substitutions(
                cycle_start=params["cycle_start"],
                cycle_end=params["cycle_end"],
                band_boundaries=params["band_boundaries"],
                probe_keys=set(),
            )
        return engine

    def test_blind_refuses_the_answer_key(self):
        engine = VerdictEngine(blind=True)
        with self.assertRaises(BlindRunError):
            engine.load_answer_keys(SEALED)

    def test_blind_refuses_the_signatures_too(self):
        engine = VerdictEngine(blind=True)
        with self.assertRaises(BlindRunError):
            engine.load_signatures(SIGNATURES)

    def test_blind_classifies_everything_as_divergent(self):
        engine = VerdictEngine(blind=True)
        attributions = engine.classify([self._variance()])
        self.assertEqual(attributions[0].verdict, DIVERGENT)
        self.assertIsNone(attributions[0].defect_id)

    def test_band_boundary_is_attributed(self):
        # The expected id is read from the signature file rather than written
        # here, so this test asserts that attribution happens and is stable —
        # not which defect it lands on. That mapping stays in SEALED/.
        engine = self._engine()
        attribution = engine.classify([self._variance(legacy="100000.00")])[0]
        self.assertEqual(attribution.verdict, DIVERGENT_BY_DESIGN)
        self.assertIsNotNone(attribution.defect_id)
        self._boundary_defect = attribution.defect_id

    def test_band_boundary_matches_regardless_of_declared_scale(self):
        # The canonical form preserves scale, so the boundary arrives as
        # "100000.00" not "100000". The signature must still match.
        engine = self._engine()
        expected = engine.classify([self._variance(legacy="100000.00")])[0].defect_id
        self.assertIsNotNone(expected)
        for value in ("100000.00", "100000", "100000.000"):
            attribution = engine.classify([self._variance(legacy=value)])[0]
            self.assertEqual(attribution.defect_id, expected, "failed for %s" % value)

    def test_tandem_rounding_is_attributed(self):
        engine = self._engine()
        attribution = engine.classify(
            [
                self._variance(
                    legacy="123.45",
                    candidate="123.46",
                    context={"scale": 2, "witness": {"CD-VC-TANDEM-IND": "Y"}},
                )
            ]
        )[0]
        self.assertEqual(attribution.verdict, DIVERGENT_BY_DESIGN)
        self.assertIsNotNone(attribution.defect_id)

    def test_fatal_status_missing_record_is_attributed(self):
        engine = self._engine()
        attribution = engine.classify(
            [
                self._variance(
                    level="L1", kind="missing_record", field=None, delta=None,
                    context={"witness": {"CD-EDIT-STATUS": "7"}},
                )
            ]
        )[0]
        self.assertEqual(attribution.verdict, DIVERGENT_BY_DESIGN)
        self.assertIsNotNone(attribution.defect_id)

    def test_an_unattributable_variance_stays_divergent(self):
        engine = self._engine()
        attribution = engine.classify(
            [
                self._variance(
                    legacy="12.34", candidate="12.35",
                    context={"scale": 2, "witness": {"CD-VC-TANDEM-IND": "N", "CD-RATE-ELEM": "ORIGAC"}},
                )
            ]
        )[0]
        self.assertEqual(attribution.verdict, DIVERGENT)
        self.assertIsNone(attribution.defect_id)

    def test_score_reports_detected_and_missed(self):
        engine = self._engine()
        attributions = engine.classify([self._variance(legacy="500000.00")])
        score = engine.score(attributions)
        self.assertEqual(score["overall_verdict"], DIVERGENT_BY_DESIGN)
        detected = set(score["seeded_defects_detected"])
        missed = set(score["seeded_defects_missed"])
        self.assertEqual(len(detected), 1)
        self.assertFalse(detected & missed)
        self.assertEqual(len(detected) + len(missed), len(engine.defects))
        self.assertEqual(score["counts"][DIVERGENT], 0)

    def test_a_single_divergent_dominates_the_overall_verdict(self):
        engine = self._engine()
        attributions = engine.classify(
            [self._variance(legacy="500000.00"), self._variance(legacy="12.34", candidate="99.99")]
        )
        self.assertEqual(engine.score(attributions)["overall_verdict"], DIVERGENT)

    def test_answer_keys_are_recorded_as_signed_inputs(self):
        engine = self._engine()
        score = engine.score(engine.classify([]))
        self.assertTrue(score["signed_inputs"])
        for entry in score["signed_inputs"]:
            self.assertEqual(len(entry["sha256"]), 64)


class TestBillDetailGeometry(unittest.TestCase):
    """The occurrence arithmetic the L3 and L4 assertions rest on.

    ``compare.py`` writes down how wide a CABSBILL record's fixed portion is
    and how wide one occurrence is, because the assertions need to know where
    a fixed image of the record stops reproducing it. Written-down constants
    rot, so they are checked against the copybook itself rather than trusted.
    """

    @classmethod
    def setUpClass(cls):
        cls.layout = gc.load_layouts(ROOT / "COPYBOOKS", quiet=True)["CABSBILL"]

    def test_the_fixed_portion_matches_the_copybook(self):
        first = self.layout.field("BD-EL-RATE-ELEM").offset
        self.assertEqual(first, compare.BILL_DETAIL_FIXED_BYTES)

    def test_one_occurrence_matches_the_copybook(self):
        element = self.layout.field("BD-ELEMENT")
        self.assertEqual(element.length, compare.BILL_ELEMENT_BYTES)

    def test_the_longest_record_is_the_declared_maximum(self):
        longest = (
            compare.BILL_DETAIL_FIXED_BYTES + 40 * compare.BILL_ELEMENT_BYTES
        )
        self.assertEqual(longest, 1647)
        self.assertEqual(self.layout.expected_declared_lrecl, 1651)

    def test_a_1204_byte_image_reproduces_28_occurrences_and_no_more(self):
        self.assertEqual(compare.elements_carried_by(1204), 28)
        self.assertEqual(compare.BILL_ELEMENT_BOUNDARY, 28)
        # 13 bytes over: the 29th occurrence's code and part of its quantity.
        spare = 1204 - compare.BILL_DETAIL_FIXED_BYTES - 28 * compare.BILL_ELEMENT_BYTES
        self.assertEqual(spare, 13)
        self.assertLess(spare, compare.BILL_ELEMENT_BYTES)

    def test_a_full_width_image_loses_nothing(self):
        self.assertEqual(compare.elements_carried_by(1647), 40)


def _detail(ban, period, section, seq, elements, total=None):
    """A synthetic canonicalised CABSBILL line.

    ``elements`` is a list of ``(code, qty, rate, amount)``. ``total`` defaults
    to the sum of the amounts, which is what a line that has not been through
    anything looks like.
    """
    fields = {
        "BD-BAN": {"t": "str", "v": ban},
        "BD-BILL-PERIOD": {"t": "dec", "v": period, "s": 0},
        "BD-SECTION": {"t": "str", "v": section},
        "BD-LINE-SEQ": {"t": "dec", "v": str(seq), "s": 0},
        "BD-ELEM-CNT": {"t": "dec", "v": str(len(elements)), "s": 0},
    }
    amount_total = sum(Decimal(e[3]) for e in elements)
    fields["BD-TOT-AMOUNT"] = {
        "t": "dec",
        "v": str(total if total is not None else amount_total),
        "s": 5,
    }
    fields["BD-TOT-ROUNDED"] = {"t": "dec", "v": "0.00", "s": 2}
    for i, (code, qty, rate, amount) in enumerate(elements, start=1):
        fields["BD-EL-RATE-ELEM(%d)" % i] = {"t": "str", "v": code}
        fields["BD-EL-QTY(%d)" % i] = {"t": "dec", "v": qty, "s": 2}
        fields["BD-EL-RATE(%d)" % i] = {"t": "dec", "v": rate, "s": 5}
        fields["BD-EL-AMOUNT(%d)" % i] = {"t": "dec", "v": amount, "s": 5}
    return canonical.CanonicalRecord(
        layout="CABSBILL",
        record_name="CABS-BILL-DETAIL",
        side="legacy",
        file="BILLDTL",
        ordinal=seq,
        key="%s|%s|%s|%s" % (ban, period, section, seq),
        variant=None,
        fields=fields,
    )


def _occurrences(prefix, count, start=1):
    return [
        ("%s%03d" % (prefix, i), "%d.00" % (10 + i), "0.01000", "%d.00000" % (100 + i))
        for i in range(start, start + count)
    ]


class TestBillDetailOccurrences(unittest.TestCase):
    """L4's per-line-item occurrence assertions.

    Two things have to be true of an assertion before it is worth anything:
    it has to fire on the shape it is looking for, and it has to stay quiet on
    a clean file. Both are asserted here against a hand-built fixture, because
    no real bill detail exists until the batch estate has been executed --
    ``L4`` reports NOT RUN on a generated-data-only run for exactly that
    reason. When a Hercules run does produce bill detail, these assertions are
    the ones that read it; until then this fixture is the proof they work.
    """

    BAN = "0001234567890"
    PERIOD = "202601"

    def _clean(self):
        """Two adjacent lines, each with its own occurrences, both short."""
        return [
            _detail(self.BAN, self.PERIOD, "U1", 1, _occurrences("AAA", 20)),
            _detail(self.BAN, self.PERIOD, "U1", 2, _occurrences("BBB", 24)),
        ]

    def _long_and_clean(self):
        """A line past the boundary whose occurrences are all its own."""
        return [
            _detail(self.BAN, self.PERIOD, "U1", 1, _occurrences("AAA", 20)),
            _detail(self.BAN, self.PERIOD, "U1", 2, _occurrences("BBB", 35)),
        ]

    def _truncated(self):
        """A line past the boundary whose tail came from the line before it.

        Occurrences 1-28 are its own. 29-35 are copies of the neighbour's, and
        the line total still reflects the occurrences it used to carry.
        """
        neighbour = _occurrences("AAA", 20)
        own = _occurrences("BBB", 35)
        true_total = sum(Decimal(e[3]) for e in own)
        residue = own[:28] + neighbour[:7]
        return [
            _detail(self.BAN, self.PERIOD, "U1", 1, neighbour),
            _detail(self.BAN, self.PERIOD, "U1", 2, residue, total=str(true_total)),
        ]

    # -- residue ---------------------------------------------------------
    def _residue(self, records):
        return compare._occurrence_residue_check(
            records, "legacy", compare.BILL_ELEMENT_BOUNDARY, 500
        )

    def test_residue_assertion_is_silent_on_clean_short_lines(self):
        variances, stats = self._residue(self._clean())
        self.assertEqual(variances, [])
        self.assertEqual(stats["lines_above_element_boundary"], 0)

    def test_residue_assertion_is_silent_on_a_long_line_that_owns_its_tail(self):
        variances, stats = self._residue(self._long_and_clean())
        self.assertEqual(variances, [])
        self.assertEqual(stats["lines_above_element_boundary"], 1)
        self.assertEqual(stats["lines_repeating_a_neighbour_above_boundary"], 0)

    def test_residue_assertion_fires_when_the_tail_repeats_a_neighbour(self):
        variances, stats = self._residue(self._truncated())
        self.assertEqual(len(variances), 1)
        v = variances[0]
        self.assertEqual(v.level, "L4")
        self.assertEqual(v.kind, "bill_line_element_residue")
        self.assertEqual(v.layout, "CABSBILL")
        self.assertEqual(v.context["occurrences_repeating_a_neighbour"],
                         list(range(29, 36)))
        self.assertEqual(v.context["witness"]["BD-ELEM-CNT"], "35")
        self.assertEqual(v.context["witness"]["duplicates_within_boundary"], "0")
        self.assertEqual(stats["occurrences_repeating_a_neighbour_above_boundary"], 7)
        self.assertEqual(stats["occurrences_repeating_a_neighbour_within_boundary"], 0)

    def test_the_boundary_is_the_28th_occurrence_and_not_the_29th(self):
        """A repeat at position 28 is within the boundary and is not the finding."""
        neighbour = _occurrences("AAA", 20)
        own = _occurrences("BBB", 27) + [neighbour[0]] + _occurrences("CCC", 7, start=40)
        records = [
            _detail(self.BAN, self.PERIOD, "U1", 1, neighbour),
            _detail(self.BAN, self.PERIOD, "U1", 2, own),
        ]
        variances, stats = self._residue(records)
        self.assertEqual(variances, [])
        self.assertEqual(stats["occurrences_repeating_a_neighbour_within_boundary"], 1)

    def test_the_assertion_is_attributable_by_the_signature_file(self):
        """The fixture's finding must survive the machine-readable rules."""
        variances, _ = self._residue(self._truncated())
        engine = VerdictEngine()
        engine.load_answer_keys(SEALED)
        engine.load_signatures(SIGNATURES)
        attributions = engine.classify(variances)
        self.assertEqual([a.verdict for a in attributions], [DIVERGENT_BY_DESIGN])
        self.assertEqual([a.defect_id for a in attributions], ["D12"])

    # -- line total vs its own occurrences --------------------------------
    def _sums(self, records):
        return compare._occurrence_sum_check(
            records, "legacy", compare.BILL_ELEMENT_BOUNDARY, 500
        )

    def test_sum_assertion_is_silent_when_a_line_adds_up(self):
        variances, stats = self._sums(self._clean() + self._long_and_clean())
        self.assertEqual(variances, [])
        self.assertEqual(stats["occurrence_sum_mismatches_above_boundary"], 0)
        self.assertEqual(stats["occurrence_sum_mismatches_within_boundary"], 0)

    def test_sum_assertion_fires_only_on_lines_past_the_boundary(self):
        variances, stats = self._sums(self._truncated())
        self.assertEqual(len(variances), 1)
        v = variances[0]
        self.assertEqual(v.kind, "bill_line_element_sum_mismatch")
        self.assertEqual(v.field, "BD-TOT-AMOUNT")
        self.assertEqual(v.context["witness"]["BD-ELEM-CNT"], "35")
        self.assertEqual(v.context["witness"]["mismatches_within_boundary"], "0")
        self.assertEqual(stats["occurrence_sum_mismatches_above_boundary"], 1)
        self.assertEqual(stats["occurrence_sum_mismatches_within_boundary"], 0)

    def test_a_short_line_that_does_not_add_up_is_counted_not_attributed(self):
        """A mismatch below the boundary weakens the attribution and must show."""
        records = [
            _detail(self.BAN, self.PERIOD, "U1", 1, _occurrences("AAA", 10), total="1.00000"),
        ]
        variances, stats = self._sums(records)
        self.assertEqual(variances, [])
        self.assertEqual(stats["occurrence_sum_mismatches_within_boundary"], 1)

    # -- detail to header --------------------------------------------------
    def _header(self, amount):
        return canonical.CanonicalRecord(
            layout="CABSBHDR", record_name="CABS-BILL-HEADER", side="legacy",
            file="BHDR", ordinal=1, key="%s|%s" % (self.BAN, self.PERIOD),
            variant=None,
            fields={
                "BH-BAN": {"t": "str", "v": self.BAN},
                "BH-BILL-PERIOD": {"t": "dec", "v": self.PERIOD, "s": 0},
                "BH-HASH-AMOUNT": {"t": "dec", "v": amount, "s": 5},
            },
        )

    def test_detail_sums_to_header_on_a_clean_account(self):
        details = self._clean()
        total = sum(d.decimal("BD-TOT-AMOUNT") for d in details)
        variances, stats = compare._detail_to_header_check(
            [self._header(str(total))], details, "legacy",
            compare.BILL_ELEMENT_BOUNDARY, 500,
        )
        self.assertEqual(variances, [])
        self.assertEqual(stats["accounts_detail_does_not_sum_to_header"], 0)

    def test_detail_does_not_sum_to_header_and_the_residual_is_confined(self):
        details = self._truncated()
        total = sum(d.decimal("BD-TOT-AMOUNT") for d in details)
        variances, stats = compare._detail_to_header_check(
            [self._header(str(total))], details, "legacy",
            compare.BILL_ELEMENT_BOUNDARY, 500,
        )
        self.assertEqual(len(variances), 1)
        v = variances[0]
        self.assertEqual(v.kind, "bill_detail_does_not_sum_to_header")
        self.assertEqual(v.context["witness"]["residual_carried_by_lines_above_boundary"], "Y")
        self.assertEqual(Decimal(v.context["residual_on_lines_within_boundary"]), Decimal(0))
        self.assertEqual(stats["accounts_whose_residual_is_confined_above_boundary"], 1)

    # -- L3 written volume -------------------------------------------------
    def test_control_level_sees_the_same_volume_and_different_content(self):
        """Every counter agrees; only the payload digest moves.

        This is the shape that makes the control level useless as a parity
        oracle for this kind of failure, so it is asserted rather than
        described.
        """
        clean = compare.bill_detail_written_volume(self._long_and_clean())
        dirty = compare.bill_detail_written_volume(self._truncated())
        self.assertEqual(clean["records"], dirty["records"])
        self.assertEqual(clean["declared_elements"], dirty["declared_elements"])
        self.assertEqual(clean["declared_bytes"], dirty["declared_bytes"])
        self.assertEqual(clean["records_above_element_boundary"], 1)
        self.assertNotEqual(
            clean["element_payload_sha256"], dirty["element_payload_sha256"]
        )

        result = compare_l3([], [], [], legacy_written_volume=clean,
                            candidate_written_volume=dirty)
        kinds = [v.kind for v in result.variances]
        self.assertEqual(kinds, ["written_content_digest"])
        witness = result.variances[0].context["witness"]
        self.assertEqual(witness["byte_volume_agrees"], "Y")
        self.assertEqual(witness["element_count_agrees"], "Y")
        self.assertEqual(witness["records_above_element_boundary"], "1")
        self.assertTrue(result.variances[0].context["counters_agree"])

    def test_the_written_byte_figure_is_the_declared_record_length(self):
        volume = compare.bill_detail_written_volume(
            [_detail(self.BAN, self.PERIOD, "U1", 1, _occurrences("AAA", 35))]
        )
        self.assertEqual(volume["declared_bytes"], 127 + 35 * 38)


class TestContract(unittest.TestCase):
    def test_contract_parses_and_declares_no_silent_tolerances(self):
        contract = json.loads(CONTRACT.read_text(encoding="utf-8"))
        for dataset in contract["datasets"]:
            spec = DatasetSpec.from_json(dataset)
            self.assertTrue(spec.key, "%s has no key" % spec.name)
            for field, tolerance in spec.tolerances.items():
                self.assertIn(
                    "reason",
                    tolerance,
                    "tolerance on %s.%s must carry a reason" % (spec.name, field),
                )

    def test_every_signature_names_a_defect_in_the_answer_key(self):
        engine = VerdictEngine()
        engine.load_answer_keys(SEALED)
        engine.load_signatures(SIGNATURES)
        for rule in engine.rules:
            self.assertIn(rule.defect, engine.defects, "signature %r names an unknown defect" % rule.name)

    def test_every_answer_key_defect_has_at_least_one_signature(self):
        engine = VerdictEngine()
        engine.load_answer_keys(SEALED)
        engine.load_signatures(SIGNATURES)
        covered = {rule.defect for rule in engine.rules}
        missing = sorted(set(engine.defects) - covered)
        self.assertEqual(missing, [], "no signature rule for %s" % missing)


if __name__ == "__main__":
    unittest.main(verbosity=2)
