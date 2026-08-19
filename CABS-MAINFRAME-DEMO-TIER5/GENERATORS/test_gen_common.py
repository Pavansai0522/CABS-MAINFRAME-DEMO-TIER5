"""Unit tests for the CABS mainframe data primitives.

Run with::

    python3 -m unittest discover -s GENERATORS -v
    # or
    python3 GENERATORS/test_gen_common.py

The COMP-3 tests are the important ones. If packed decimal is wrong, every
figure the generator produces is wrong and every comparison the harness
makes is meaningless. They therefore check three things independently:

1.  **Known byte patterns** taken from the architecture definition, not from
    this implementation -- so the test would fail if the implementation were
    rewritten incorrectly.
2.  **Round-trip identity** across odd and even digit counts, positive,
    negative and zero, scales 0 through 5, and every sign nibble.
3.  **Agreement with a second, independently written implementation**
    (``_reference_unpack_comp3`` below, which decodes nibble by nibble from
    first principles rather than via ``bytes.hex()``).
"""

from __future__ import annotations

import tempfile
import unittest
from datetime import date
from decimal import Decimal, ROUND_DOWN, ROUND_HALF_UP
from pathlib import Path

import gen_common as gc

COPYBOOKS = Path(__file__).resolve().parent.parent / "COPYBOOKS"


# ---------------------------------------------------------------------------
# An independent reference implementation, deliberately written differently.
# ---------------------------------------------------------------------------


def _reference_unpack_comp3(data: bytes, scale: int) -> Decimal:
    """Decode COMP-3 nibble by nibble using integer arithmetic only."""
    digits = 0
    for index, byte in enumerate(data):
        high = (byte >> 4) & 0x0F
        low = byte & 0x0F
        if index == len(data) - 1:
            digits = digits * 10 + high
            sign = low
        else:
            digits = digits * 10 + high
            digits = digits * 10 + low
    negative = sign in (0x0B, 0x0D)
    value = Decimal(digits).scaleb(-scale).quantize(Decimal(1).scaleb(-scale))
    return -value if negative else value


def _reference_pack_comp3(value: Decimal, digits: int, scale: int, sign_nibble: int) -> bytes:
    """Build COMP-3 bytes by shifting nibbles, without string formatting."""
    unscaled = int(value.scaleb(scale).to_integral_value())
    magnitude = abs(unscaled)
    nbytes = digits // 2 + 1
    nibbles = [sign_nibble]
    for _ in range(nbytes * 2 - 1):
        nibbles.append(magnitude % 10)
        magnitude //= 10
    nibbles.reverse()
    out = bytearray()
    for i in range(0, len(nibbles), 2):
        out.append((nibbles[i] << 4) | nibbles[i + 1])
    return bytes(out)


class TestComp3KnownPatterns(unittest.TestCase):
    """Byte patterns stated by the packed-decimal architecture, not derived
    from the implementation under test."""

    def test_unsigned_odd_digits(self):
        # PIC 9(05) COMP-3 -> 3 bytes, sign nibble F
        self.assertEqual(gc.pack_comp3(12345, 5, 0, signed=False).hex(), "12345f")

    def test_signed_positive_odd_digits(self):
        self.assertEqual(gc.pack_comp3(12345, 5, 0, signed=True).hex(), "12345c")

    def test_signed_negative_odd_digits(self):
        self.assertEqual(gc.pack_comp3(-12345, 5, 0, signed=True).hex(), "12345d")

    def test_even_digits_have_a_leading_zero_nibble(self):
        # PIC S9(06) COMP-3 -> 4 bytes: 7 digit nibbles + sign, so one pad
        self.assertEqual(gc.pack_comp3(123456, 6, 0).hex(), "0123456c")
        self.assertEqual(gc.pack_comp3(-123456, 6, 0).hex(), "0123456d")

    def test_odd_digits_use_every_nibble(self):
        self.assertEqual(gc.pack_comp3(1234567, 7, 0).hex(), "1234567c")

    def test_implied_decimal_point_occupies_no_nibble(self):
        # PIC S9(07)V9(02) COMP-3: 9 digits -> 5 bytes
        packed = gc.pack_comp3(Decimal("123.45"), 9, 2)
        self.assertEqual(packed.hex(), "000012345c")
        self.assertEqual(len(packed), 5)

    def test_five_decimal_rate(self):
        # PIC S9(05)V9(05) COMP-3: 10 digits -> 6 bytes -> 11 digit nibbles
        # plus the sign nibble. Fractional-cent access rates live here.
        packed = gc.pack_comp3(Decimal("0.00921"), 10, 5)
        self.assertEqual(len(packed), 6)
        self.assertEqual(packed.hex(), "00000000921c")
        self.assertEqual(gc.unpack_comp3(packed, 5), Decimal("0.00921"))

    def test_zero_is_positive_signed(self):
        self.assertEqual(gc.pack_comp3(0, 9, 2).hex(), "000000000c")

    def test_field_lengths_match_the_copybooks(self):
        cases = {9: 5, 11: 6, 15: 8, 17: 9, 18: 10, 5: 3, 8: 5, 10: 6, 2: 2, 7: 4}
        for digits, expected in cases.items():
            self.assertEqual(gc.comp3_length(digits), expected, "digits=%d" % digits)


class TestComp3SignNibbles(unittest.TestCase):
    def test_c_is_positive(self):
        self.assertEqual(gc.unpack_comp3(bytes.fromhex("12345c"), 0), Decimal(12345))

    def test_d_is_negative(self):
        self.assertEqual(gc.unpack_comp3(bytes.fromhex("12345d"), 0), Decimal(-12345))

    def test_f_is_positive_unsigned(self):
        self.assertEqual(gc.unpack_comp3(bytes.fromhex("12345f"), 0), Decimal(12345))

    def test_a_and_e_are_positive(self):
        self.assertEqual(gc.unpack_comp3(bytes.fromhex("12345a"), 0), Decimal(12345))
        self.assertEqual(gc.unpack_comp3(bytes.fromhex("12345e"), 0), Decimal(12345))

    def test_b_is_negative(self):
        self.assertEqual(gc.unpack_comp3(bytes.fromhex("12345b"), 0), Decimal(-12345))

    def test_corrupt_digit_nibble_raises_in_strict_mode(self):
        with self.assertRaises(ValueError):
            gc.unpack_comp3(bytes.fromhex("1a345c"), 0, strict=True)

    def test_corrupt_digit_nibble_tolerated_when_not_strict(self):
        self.assertEqual(gc.unpack_comp3(bytes.fromhex("1a345c"), 0, strict=False), Decimal(10345))


class TestComp3RoundTrip(unittest.TestCase):
    def test_exhaustive_small_round_trip(self):
        for digits in range(1, 19):
            for scale in range(0, min(digits, 6)):
                limit = 10 ** (digits - scale) - 1
                samples = [0, 1, 7, limit if limit < 10**9 else 10**9 - 1]
                for magnitude in samples:
                    value = Decimal(magnitude).scaleb(0)
                    for sign in (1, -1):
                        dec = (Decimal(sign) * value).quantize(Decimal(1).scaleb(-scale))
                        packed = gc.pack_comp3(dec, digits, scale, signed=True)
                        self.assertEqual(len(packed), gc.comp3_length(digits))
                        got = gc.unpack_comp3(packed, scale, digits)
                        self.assertEqual(got, dec, "digits=%d scale=%d value=%s" % (digits, scale, dec))
                        self.assertEqual(
                            -got.as_tuple().exponent,
                            scale,
                            "scale must be preserved, not normalised away",
                        )

    def test_agrees_with_independent_reference(self):
        cases = [
            (Decimal("0"), 9, 2),
            (Decimal("0.01"), 9, 2),
            (Decimal("-0.01"), 9, 2),
            (Decimal("99999.99999"), 10, 5),
            (Decimal("-99999.99999"), 10, 5),
            (Decimal("1234567890123.45678"), 18, 5),
            (Decimal("-1234567890123.45678"), 18, 5),
            (Decimal("12345678901234567"), 17, 0),
            (Decimal("-999999999999999"), 15, 0),
            (Decimal("7"), 1, 0),
            (Decimal("-7"), 1, 0),
        ]
        for value, digits, scale in cases:
            packed = gc.pack_comp3(value, digits, scale)
            sign_nibble = 0x0D if value < 0 else 0x0C
            expected = _reference_pack_comp3(value, digits, scale, sign_nibble)
            self.assertEqual(packed, expected, "pack mismatch for %s" % value)
            self.assertEqual(
                gc.unpack_comp3(packed, scale),
                _reference_unpack_comp3(packed, scale),
                "unpack mismatch for %s" % value,
            )

    def test_overflow_is_refused(self):
        with self.assertRaises(ValueError):
            gc.pack_comp3(1000, 3, 0)

    def test_negative_into_unsigned_is_refused(self):
        with self.assertRaises(ValueError):
            gc.pack_comp3(-1, 5, 0, signed=False)

    def test_float_is_refused_everywhere(self):
        with self.assertRaises(TypeError):
            gc.pack_comp3(1.5, 9, 2)
        with self.assertRaises(TypeError):
            gc.pack_zoned(1.5, 9, 2)

    def test_rounding_is_explicit(self):
        half_up = gc.pack_comp3(Decimal("1.005"), 9, 2, rounding=ROUND_HALF_UP)
        truncated = gc.pack_comp3(Decimal("1.005"), 9, 2, rounding=ROUND_DOWN)
        self.assertEqual(gc.unpack_comp3(half_up, 2), Decimal("1.01"))
        self.assertEqual(gc.unpack_comp3(truncated, 2), Decimal("1.00"))


class TestZonedDecimal(unittest.TestCase):
    def test_unsigned_digits_are_ebcdic(self):
        self.assertEqual(gc.pack_zoned(12345, 5).hex(), "f1f2f3f4f5")

    def test_leading_zeros_are_written(self):
        self.assertEqual(gc.pack_zoned(42, 5).hex(), "f0f0f0f4f2")

    def test_signed_positive_overpunch(self):
        self.assertEqual(gc.pack_zoned(12345, 5, signed=True).hex(), "f1f2f3f4c5")

    def test_signed_negative_overpunch(self):
        self.assertEqual(gc.pack_zoned(-12345, 5, signed=True).hex(), "f1f2f3f4d5")

    def test_round_trip_with_scale(self):
        packed = gc.pack_zoned(Decimal("123.45"), 5, 2)
        self.assertEqual(gc.unpack_zoned(packed, 2), Decimal("123.45"))

    def test_yyddd_is_five_zoned_digits(self):
        packed = gc.pack_zoned(24366, 5)
        self.assertEqual(len(packed), 5)
        self.assertEqual(gc.from_ebcdic(packed), "24366")


class TestEbcdic(unittest.TestCase):
    def test_space_is_0x40(self):
        self.assertEqual(gc.to_ebcdic("", 3), b"\x40\x40\x40")

    def test_pad_and_truncate(self):
        self.assertEqual(gc.from_ebcdic(gc.to_ebcdic("AB", 4)), "AB  ")
        self.assertEqual(gc.from_ebcdic(gc.to_ebcdic("ABCDE", 3)), "ABC")

    def test_round_trip(self):
        text = "OCN 0288 / BAN 813G1234567X"
        self.assertEqual(gc.from_ebcdic(gc.to_ebcdic(text)), text)

    def test_digits_are_f0_series(self):
        self.assertEqual(gc.to_ebcdic("0123456789").hex(), "f0f1f2f3f4f5f6f7f8f9")


class TestJulianDates(unittest.TestCase):
    def test_render(self):
        self.assertEqual(gc.date_to_yyddd(date(2024, 1, 1)), "24001")
        self.assertEqual(gc.date_to_yyddd(date(2024, 12, 31)), "24366")  # leap year
        self.assertEqual(gc.date_to_yyddd(date(2025, 12, 31)), "25365")

    def test_pivot_70(self):
        self.assertEqual(gc.yyddd_to_date("99001").year, 1999)
        self.assertEqual(gc.yyddd_to_date("70001").year, 1970)
        self.assertEqual(gc.yyddd_to_date("69001").year, 2069)
        self.assertEqual(gc.yyddd_to_date("24001").year, 2024)

    def test_invalid_dates_return_none(self):
        self.assertIsNone(gc.yyddd_to_date("00000"))
        self.assertIsNone(gc.yyddd_to_date("24367"))
        self.assertIsNone(gc.yyddd_to_date("     "))
        self.assertIsNone(gc.yyddd_to_date("2536"))

    @staticmethod
    def _naive_add(yyddd: str, days: int) -> str:
        """The estate's own arithmetic: add days to the YYDDD integer.

        Adding a day count straight to the packed YYDDD, which is what the
        naive form of this arithmetic looks like in COBOL.
        """
        return "%05d" % (int(yyddd) + days)

    def test_cross_year_addition_rolls_the_year(self):
        self.assertEqual(gc.yyddd_add_days("24365", 3), "25002")
        self.assertEqual(gc.yyddd_add_days("24300", 92), "25026")
        self.assertEqual(gc.yyddd_add_days("25365", 1), "26001")

    def test_correct_arithmetic_diverges_from_the_estates_arithmetic(self):
        # Where the naive addition stays inside the year it happens to agree;
        # where it crosses a year end it produces an impossible YYDDD.
        self.assertEqual(gc.yyddd_add_days("24274", 92), self._naive_add("24274", 92))  # 24366, leap
        for yyddd, days, naive in (("24365", 3, "24368"), ("24300", 92, "24392"), ("25365", 1, "25366")):
            self.assertEqual(self._naive_add(yyddd, days), naive)
            self.assertNotEqual(gc.yyddd_add_days(yyddd, days), naive)
            self.assertIsNone(gc.yyddd_to_date(naive), "%s is not a real date" % naive)

    def test_absolute_day_round_trip(self):
        for yyddd in ("24001", "24366", "25001", "99365", "00001"):
            abs_day = gc.yyddd_to_abs(yyddd)
            self.assertIsNotNone(abs_day)
            self.assertEqual(gc.abs_to_yyddd(abs_day), yyddd)


class TestDeterminism(unittest.TestCase):
    def test_same_seed_same_values(self):
        a = gc.DeterministicRandom(20260815).substream("cdr")
        b = gc.DeterministicRandom(20260815).substream("cdr")
        self.assertEqual([a.randint(0, 10**9) for _ in range(50)], [b.randint(0, 10**9) for _ in range(50)])

    def test_substreams_are_independent(self):
        root_a = gc.DeterministicRandom(7)
        root_b = gc.DeterministicRandom(7)
        for _ in range(1000):
            root_a.substream("carrier").randint(0, 100)
        self.assertEqual(root_a.substream("rate").randint(0, 10**9), root_b.substream("rate").randint(0, 10**9))

    def test_different_seed_different_values(self):
        a = gc.DeterministicRandom(1).substream("x")
        b = gc.DeterministicRandom(2).substream("x")
        self.assertNotEqual([a.randint(0, 10**9) for _ in range(10)], [b.randint(0, 10**9) for _ in range(10)])

    def test_decimal_draw_has_exact_scale(self):
        rng = gc.DeterministicRandom(99).substream("rates")
        for _ in range(200):
            value = rng.decimal("0.00001", "0.09999", 5)
            self.assertEqual(-value.as_tuple().exponent, 5)
            self.assertLessEqual(value, Decimal("0.09999"))
            self.assertGreaterEqual(value, Decimal("0.00001"))


class TestCopybookParser(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.layouts = gc.load_layouts(COPYBOOKS, quiet=True)

    def test_all_principal_members_parse(self):
        for member in (
            "CABSCDR",
            "CABSCTL",
            "CABSCARR",
            "CABSRATE",
            "CABSFCTR",
            "CABSCIRC",
            "CABSBHDR",
            "CABSBILL",
            "CABSSETL",
        ):
            self.assertIn(member, self.layouts)

    def test_cdr_offsets(self):
        cdr = self.layouts["CABSCDR"]
        self.assertEqual(cdr.field("CD-OCN").offset, 0)
        self.assertEqual(cdr.field("CD-BAN").offset, 4)
        self.assertEqual(cdr.field("CD-SEQ-NBR").offset, 17)
        self.assertEqual(cdr.field("CD-SEQ-NBR").length, 5)  # 9(09) COMP-3
        self.assertEqual(cdr.field("CD-VARIANT-AREA").offset, 54)
        self.assertEqual(cdr.field("CD-VARIANT-AREA").length, 96)
        self.assertEqual(cdr.field("CD-SRC-SYSTEM").offset, 150)

    def test_all_three_variants_start_at_the_same_offset(self):
        cdr = self.layouts["CABSCDR"]
        base = cdr.field("CD-VARIANT-AREA").offset
        for first in ("CD-VC-ORIG-NPANXX", "CD-DT-CIRCUIT-ID", "CD-SP-CIRCUIT-ID"):
            self.assertEqual(cdr.field(first).offset, base)

    def test_variant_length_disagreements_are_reported_not_repaired(self):
        # CABSCDR's three usage variants each occupy exactly the 96 bytes of
        # CD-VARIANT-AREA, so the parser has nothing to report against them.
        cdr = self.layouts["CABSCDR"]
        joined = " ".join(cdr.diagnostics)
        self.assertNotIn("CD-VOICE-DETAIL REDEFINES CD-VARIANT-AREA", joined)
        self.assertNotIn("CD-DATA-DETAIL REDEFINES CD-VARIANT-AREA", joined)
        self.assertNotIn("CD-SPCL-DETAIL REDEFINES CD-VARIANT-AREA", joined)
        target = cdr.field("CD-VARIANT-AREA")
        self.assertEqual(target.length, 96)
        for variant in ("CD-VOICE-DETAIL", "CD-DATA-DETAIL", "CD-SPCL-DETAIL"):
            f = cdr.field(variant)
            self.assertEqual(f.redefines, "CD-VARIANT-AREA")
            self.assertEqual(f.length, target.length)   # reported, not resized
            self.assertEqual(f.offset, target.offset)

    def test_declared_lrecl_wins_over_computed_length(self):
        # CD-KEY 22 + CD-RECORD-CTL 10 + CD-DATE-TIME 22 + CD-VARIANT-AREA 96
        # + CD-AUDIT 50 = 200, which is what the header declares.
        cdr = self.layouts["CABSCDR"]
        self.assertEqual(cdr.computed_length, 200)
        self.assertEqual(cdr.lrecl, 200)

    # ---------------------------------------------------------------
    # RDW arithmetic on RECFM V/VB layouts
    # ---------------------------------------------------------------

    def test_recfm_vb_lrecl_accounts_for_the_rdw(self):
        """A VB copybook agrees with its DCB at computed + 4, not at equality.

        ``CABSBILL`` is the estate's only RECFM VB copybook. Its data
        maximum is 127 fixed bytes plus 40 ``BD-ELEMENT`` occurrences of 38
        bytes = 1,647. QSAM prefixes a 4-byte Record Descriptor Word to each
        V/VB record, so every LRECL quoted against the bill detail datasets
        -- the JCL DCBs, the sort ``RECORD TYPE=V`` card -- is 1,651. The two
        numbers are consistent, and the parser must not report them as a
        4-byte disagreement.
        """
        bill = self.layouts["CABSBILL"]
        self.assertEqual(bill.recfm_declared, "VB")
        self.assertTrue(bill.has_rdw)
        self.assertEqual(bill.computed_length, 1647)  # 127 + 40 * 38
        self.assertEqual(gc.RDW_LENGTH, 4)
        self.assertEqual(bill.expected_declared_lrecl, 1651)
        self.assertEqual(bill.lrecl_declared, 1651)
        self.assertEqual(
            [d for d in bill.diagnostics if "LRECL" in d],
            [],
            "declared LRECL 1651 == computed 1647 + 4 byte RDW, so there is "
            "nothing to report",
        )

    def test_fixed_layouts_carry_no_rdw_allowance(self):
        """RECFM F/FB has no RDW, so declared and computed must match exactly."""
        cdr = self.layouts["CABSCDR"]
        self.assertEqual(cdr.recfm_declared, "FB")
        self.assertFalse(cdr.has_rdw)
        self.assertEqual(cdr.expected_declared_lrecl, cdr.computed_length)
        self.assertEqual(cdr.expected_declared_lrecl, 200)

    def test_odo_without_declared_recfm_gets_no_rdw_allowance(self):
        """An OCCURS DEPENDING ON table alone does not imply a V/VB record.

        ``CABSRATE`` carries ``RT-BAND OCCURS 1 TO 24 DEPENDING ON``, so it
        is variable in shape, but its header declares no RECFM. Without a
        declared V/VB there is no RDW to allow for.
        """
        rate = self.layouts["CABSRATE"]
        self.assertTrue(rate.odo_fields)
        self.assertTrue(rate.is_variable)
        self.assertFalse(rate.has_rdw)
        self.assertEqual(rate.expected_declared_lrecl, rate.computed_length)

    def test_rdw_discrepancy_is_still_reported_when_the_sum_does_not_hold(self):
        """The check reports only when declared != computed + 4 on a V/VB file."""
        body = [
            "       01  DEMO-VB-RECORD.",
            "           05  DV-COUNT                PIC 9(03).",
            "           05  DV-ELEMENT OCCURS 1 TO 40 TIMES",
            "                    DEPENDING ON DV-COUNT.",
            "               10  DV-BODY             PIC X(38).",
        ]

        def parse(declared):
            header = "      * DEMOVB - SYNTHETIC.  RECFM VB  LRECL %d." % declared
            with tempfile.TemporaryDirectory() as tmp:
                p = Path(tmp) / "DEMOVB.cpy"
                p.write_text("\n".join([header] + body) + "\n", encoding="utf-8")
                return gc.parse_copybook(p)

        # 3 + 40 * 38 = 1523 data bytes, so the DCB should say 1527.
        bad = parse(1204)
        self.assertEqual(bad.computed_length, 1523)
        self.assertTrue(bad.has_rdw)
        self.assertEqual(bad.expected_declared_lrecl, 1527)
        lrecl_diags = [d for d in bad.diagnostics if "LRECL" in d]
        self.assertEqual(len(lrecl_diags), 1, bad.diagnostics)
        self.assertIn("RDW", lrecl_diags[0])
        self.assertIn("1527", lrecl_diags[0])
        self.assertIn("-323", lrecl_diags[0])

        # Declared exactly one RDW above the data length: nothing to report.
        self.assertEqual([d for d in parse(1527).diagnostics if "LRECL" in d], [])

        # Declared equal to the data length: on a VB file that is 4 bytes short,
        # and must still be reported even though the naive check would pass.
        equal = parse(1523)
        equal_diags = [d for d in equal.diagnostics if "LRECL" in d]
        self.assertEqual(len(equal_diags), 1, equal.diagnostics)
        self.assertIn("-4", equal_diags[0])

    def test_control_record_balancing_fields_present(self):
        ctl = self.layouts["CABSCTL"]
        for name in (
            "CT-READ",
            "CT-WRITTEN",
            "CT-REJECTED",
            "CT-SUMMARISED",
            "CT-CARRIED-FWD",
            "CT-HASH-MINUTES",
            "CT-HASH-AMOUNT",
            "CT-HASH-SEQ",
            "CT-HASH-OCN",
        ):
            f = ctl.field(name)
            self.assertTrue(f.is_comp3, "%s should be COMP-3" % name)

    def test_occurs_depending_on_is_modelled(self):
        rate = self.layouts["CABSRATE"]
        band = rate.field("RT-BAND")
        self.assertEqual(band.occurs_min, 1)
        self.assertEqual(band.occurs_max, 24)
        self.assertEqual(band.odo_field, "RT-BAND-CNT")
        stride = rate.field("RT-BAND-FROM").occurs_stride
        self.assertEqual(stride, band.length)
        self.assertEqual(
            rate.field("RT-BAND-FROM").offset_for(2) - rate.field("RT-BAND-FROM").offset_for(1),
            stride,
        )

    def test_88_levels_are_captured(self):
        cdr = self.layouts["CABSCDR"]
        conds = cdr.field("CD-EDIT-STATUS").conditions
        self.assertIn("CD-FATAL", conds)
        self.assertIn("CD-CLEAN", conds)

    def test_level_66_renames_is_an_alias(self):
        dw = self.layouts["CABSDATE"]
        alias = dw.field("DW-BP-YYMM")
        self.assertEqual(alias.offset, dw.field("DW-BP-YY").offset)
        self.assertEqual(alias.length, 4)


class TestRecordBuilder(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.layouts = gc.load_layouts(COPYBOOKS, quiet=True)

    def test_build_and_decode_voice_cdr(self):
        cdr = self.layouts["CABSCDR"]
        b = gc.RecordBuilder(cdr)
        b.set("CD-OCN", "0288")
        b.set("CD-BAN", "813G1234567X")
        b.set("CD-SEQ-NBR", 123456789)
        b.set("CD-REC-TYPE", "01")
        b.set("CD-JURIS-CD", "I")
        b.set("CD-RATE-ELEM", "ORIGAC")
        b.set("CD-CONN-YY", 24)
        b.set("CD-CONN-DDD", 274)
        b.set("CD-VC-CONV-MIN", Decimal("12.30"))
        b.set("CD-VC-CHG-MIN", Decimal("13.00"))
        b.set("CD-VC-CIC", 288)
        b.set("CD-EDIT-STATUS", "0")
        record = b.build()
        self.assertEqual(len(record), 200)

        decoded = gc.decode_record(cdr, record, variant="CD-VOICE-DETAIL")
        self.assertEqual(decoded["CD-OCN"], "0288")
        self.assertEqual(decoded["CD-BAN"], "813G1234567X ")
        self.assertEqual(decoded["CD-SEQ-NBR"], Decimal(123456789))
        self.assertEqual(decoded["CD-VC-CONV-MIN"], Decimal("12.30"))
        self.assertEqual(decoded["CD-VC-CIC"], Decimal(288))
        # the data variant is not decoded unless asked for
        self.assertNotIn("CD-DT-OCTETS-IN", decoded)

    def test_variant_area_is_genuinely_shared(self):
        cdr = self.layouts["CABSCDR"]
        b = gc.RecordBuilder(cdr)
        b.set("CD-DT-CIRCUIT-ID", "101/T1/ATLNGAMA/001 ")
        record = b.build()
        voice = gc.decode_record(cdr, record, variant="CD-VOICE-DETAIL")
        # Reading the voice variant off a data record produces a reading, not
        # an error -- which is precisely the trap CABSCDR warns about.
        self.assertIn("CD-VC-TRUNK-GRP", voice)

    def test_overflow_is_refused_at_field_level(self):
        cdr = self.layouts["CABSCDR"]
        b = gc.RecordBuilder(cdr)
        with self.assertRaises(ValueError):
            b.set("CD-SEQ-NBR", 10**10)

    def test_control_record_round_trip(self):
        ctl = self.layouts["CABSCTL"]
        b = gc.RecordBuilder(ctl)
        b.set("CT-RUN-ID", "R20240930001")
        b.set("CT-PROCESS-ID", "CABING05")
        b.set("CT-STEP-SEQ", 10)
        b.set("CT-READ", 500000)
        b.set("CT-WRITTEN", 499100)
        b.set("CT-REJECTED", 900)
        b.set("CT-SUMMARISED", 0)
        b.set("CT-CARRIED-FWD", 0)
        b.set("CT-HASH-AMOUNT", Decimal("-12345.67891"))
        record = b.build()
        self.assertEqual(len(record), 180)
        d = gc.decode_record(ctl, record)
        self.assertEqual(d["CT-READ"], Decimal(500000))
        self.assertEqual(d["CT-HASH-AMOUNT"], Decimal("-12345.67891"))
        self.assertEqual(
            d["CT-READ"],
            d["CT-WRITTEN"] + d["CT-REJECTED"] + d["CT-SUMMARISED"] + d["CT-CARRIED-FWD"],
        )

    def test_occurs_indexing(self):
        rate = self.layouts["CABSRATE"]
        b = gc.RecordBuilder(rate, lrecl=rate.computed_length)
        b.set("RT-BAND-CNT", 3)
        for i, (frm, thru, rt) in enumerate(
            [(0, 99999, "0.00921"), (100000, 499999, "0.00885"), (500000, 999999999, "0.00812")], 1
        ):
            b.set("RT-BAND-FROM", frm, index=i)
            b.set("RT-BAND-THRU", thru, index=i)
            b.set("RT-BAND-RATE", Decimal(rt), index=i)
        record = b.build()
        d = gc.decode_record(rate, record)
        self.assertEqual(d["RT-BAND-FROM(2)"], Decimal(100000))
        self.assertEqual(d["RT-BAND-RATE(3)"], Decimal("0.00812"))


class TestHashTotals(unittest.TestCase):
    def test_accumulation_is_decimal_and_ordered(self):
        h = gc.HashTotals()
        for _ in range(3):
            h.add(minutes=Decimal("0.10"), amount=Decimal("0.00001"), seq=1, ocn="0288")
        self.assertEqual(h.minutes, Decimal("0.30"))
        self.assertEqual(h.amount, Decimal("0.00003"))
        self.assertEqual(h.seq, Decimal(3))
        self.assertEqual(h.ocn, Decimal(288 * 3))
        self.assertEqual(h.records, 3)

    def test_alphanumeric_ocn_hashes_by_code_point(self):
        self.assertEqual(gc.HashTotals.ocn_numeric("ABCD"), sum(gc.to_ebcdic("ABCD")))


if __name__ == "__main__":
    unittest.main(verbosity=2)
