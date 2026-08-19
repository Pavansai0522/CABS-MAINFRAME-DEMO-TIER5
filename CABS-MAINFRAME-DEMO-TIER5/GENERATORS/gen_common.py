"""CABS Tier 5 -- shared mainframe data primitives for the generator suite.

This module is the single place in the Python tooling where a COBOL data
declaration is turned into bytes, and where bytes are turned back into a
value. Everything else -- the reference generator, the CDR generator, the
settlement generator, the comparison harness -- builds on it.

Four things live here:

1.  **EBCDIC (cp037) encoding.** The mainframe estate is EBCDIC. Every
    character field written by this suite is encoded with cp037 and padded
    with the EBCDIC space (0x40), not the ASCII space (0x20).

2.  **COMP-3 packed decimal and zoned decimal.** Both are implemented from
    the architecture definition, with an *explicit* declared scale. Values
    are ``decimal.Decimal`` on the way in and on the way out. There is no
    float anywhere in this module, and none is permitted downstream.

3.  **YYDDD Julian date handling.** The estate stores dates as a two-digit
    year plus a day-of-year. The pivot the estate uses is 70 (see the
    inline pivot-70 complexity in CABING04/CABING08). Conversion to and
    from an absolute day number is provided so that date arithmetic can be
    done correctly -- which is not the same as saying the estate always
    does it that way.

4.  **A COBOL copybook parser.** ``COPYBOOKS/*.cpy`` is frozen and is the
    authoritative data architecture. Rather than restate the layouts in
    Python (which would drift), this module parses the copybooks directly:
    levels, PIC clauses, USAGE, REDEFINES, OCCURS DEPENDING ON and 88-level
    condition names. Offsets and lengths are computed from the declaration.

    The parser is deliberately tolerant and *reports* rather than repairs.
    Several of the frozen copybooks contain REDEFINES variants whose
    lengths do not agree with the area they redefine, and record totals
    that fall short of the LRECL stated in the header comment. Those are
    surfaced as ``Layout.diagnostics`` and must not be silently corrected
    (CONVENTIONS.md: "if a layout is wrong, report it, do not change it").

Determinism
-----------
``DeterministicRandom`` derives every sub-stream from a master seed via
BLAKE2b, so a given ``--seed`` reproduces byte-identical output regardless
of how many sub-streams are drawn or in what order the modules run.
"""

from __future__ import annotations

import hashlib
import re
from dataclasses import dataclass, field as dc_field
from datetime import date, timedelta
from decimal import (
    Context,
    Decimal,
    DivisionByZero,
    InvalidOperation,
    Overflow,
    ROUND_HALF_EVEN,
    ROUND_HALF_UP,
    localcontext,
)
from pathlib import Path
from random import Random
from typing import Any, Dict, Iterable, Iterator, List, Optional, Sequence, Tuple

__all__ = [
    "CABS_CONTEXT",
    "EBCDIC_CODEC",
    "EBCDIC_SPACE",
    "to_ebcdic",
    "from_ebcdic",
    "pack_comp3",
    "unpack_comp3",
    "comp3_length",
    "pack_zoned",
    "unpack_zoned",
    "date_to_yyddd",
    "yyddd_to_date",
    "yyddd_to_abs",
    "abs_to_yyddd",
    "yyddd_add_days",
    "DeterministicRandom",
    "PictureSpec",
    "parse_picture",
    "Field",
    "Layout",
    "parse_copybook",
    "load_layouts",
    "RecordBuilder",
    "decode_record",
    "FixedRecordWriter",
    "HashTotals",
]

# ---------------------------------------------------------------------------
# Decimal context
# ---------------------------------------------------------------------------

#: The arithmetic context used by every module in this suite. Precision is
#: wide enough for CT-HASH-AMOUNT (S9(13)V9(05)) accumulated over a hundred
#: million records. Traps are on: a silent NaN in a billing pipeline is worse
#: than an abend.
CABS_CONTEXT: Context = Context(
    prec=60,
    rounding=ROUND_HALF_EVEN,
    traps=[InvalidOperation, DivisionByZero, Overflow],
)

#: Pre-built quantum Decimals, ``_QUANTUM[n] == Decimal("1E-n")``. Building
#: these on every pack call is the single hottest cost in the generator.
_QUANTUM: List[Decimal] = [Decimal(1).scaleb(-n) for n in range(0, 20)]

EBCDIC_CODEC = "cp037"
EBCDIC_SPACE = 0x40
EBCDIC_ZERO = 0xF0

#: Length of the Record Descriptor Word that QSAM/BSAM prefixes to every
#: RECFM V or VB record: a 2-byte binary length followed by 2 reserved bytes.
#: The length in the RDW counts itself, so an LRECL quoted against a V/VB
#: dataset is *data bytes + 4*. A copybook never describes the RDW, so a
#: copybook that computes to N data bytes belongs on a dataset declared
#: LRECL N+4. Comparing the two numbers directly reports a false 4-byte
#: disagreement on every variable-length file in the estate.
RDW_LENGTH = 4

#: ASCII '0'-'9' (0x30-0x39) to EBCDIC zoned '0'-'9' (0xF0-0xF9), applied with
#: ``bytes.translate`` so the conversion runs at C speed.
_ASCII_DIGIT_TO_ZONED = bytes(
    (EBCDIC_ZERO | (i - 0x30)) if 0x30 <= i <= 0x39 else i for i in range(256)
)


def _dec(value: Any) -> Decimal:
    """Coerce to Decimal without ever going through float.

    Raises on float input rather than accepting it, because accepting a
    float here is exactly how fractional-cent rates get corrupted.
    """
    if isinstance(value, Decimal):
        return value
    if isinstance(value, int):
        return Decimal(value)
    if isinstance(value, str):
        return Decimal(value)
    if isinstance(value, float):
        raise TypeError(
            "float is not accepted: money and rates in this estate carry five "
            "decimal places and must be Decimal or str, got %r" % (value,)
        )
    raise TypeError("cannot convert %r (%s) to Decimal" % (value, type(value).__name__))


# ---------------------------------------------------------------------------
# EBCDIC
# ---------------------------------------------------------------------------


def to_ebcdic(text: str, length: Optional[int] = None, pad: str = " ") -> bytes:
    """Encode ``text`` as cp037, optionally padded/truncated to ``length``.

    Padding uses the EBCDIC representation of ``pad`` (0x40 for a space).
    Truncation is silent and left-justified, matching a COBOL ``MOVE`` of a
    longer alphanumeric into a shorter PIC X field.
    """
    raw = text.encode(EBCDIC_CODEC, errors="replace")
    if length is None:
        return raw
    if len(raw) >= length:
        return raw[:length]
    return raw + pad.encode(EBCDIC_CODEC) * (length - len(raw))


def from_ebcdic(data: bytes) -> str:
    """Decode cp037 bytes to a Python string."""
    return data.decode(EBCDIC_CODEC)


# ---------------------------------------------------------------------------
# COMP-3 packed decimal
# ---------------------------------------------------------------------------

#: Sign nibbles that mean "negative". Everything else (A, C, E, F) is positive.
_NEGATIVE_SIGN_NIBBLES = frozenset("bdBD")
_VALID_SIGN_NIBBLES = frozenset("abcdefABCDEF")


def comp3_length(digits: int) -> int:
    """Bytes occupied by a ``PIC S9(digits)`` (or ``9(digits)``) COMP-3 field.

    One nibble per digit plus one sign nibble, rounded up to a whole byte::

        digits 9  -> 5 bytes    digits 11 -> 6 bytes
        digits 15 -> 8 bytes    digits 18 -> 10 bytes
    """
    if digits < 1:
        raise ValueError("COMP-3 needs at least one digit")
    return digits // 2 + 1


def pack_comp3(
    value: Any,
    digits: int,
    scale: int = 0,
    signed: bool = True,
    rounding: str = ROUND_HALF_UP,
) -> bytes:
    """Pack a value into COMP-3 with an explicit declared scale.

    Parameters
    ----------
    value:
        ``Decimal``, ``int`` or numeric ``str``. Floats are rejected.
    digits:
        Total digit positions declared, including those after the implied
        decimal point. ``PIC S9(07)V9(02)`` is ``digits=9, scale=2``.
    scale:
        Number of digits after the implied decimal point.
    signed:
        ``True`` for ``PIC S9...`` -- sign nibble C (positive) or D
        (negative). ``False`` for unsigned ``PIC 9...`` -- sign nibble F.
    rounding:
        Applied only when ``value`` carries more decimal places than
        ``scale``. Defaults to ROUND_HALF_UP, matching COBOL
        ``COMPUTE ... ROUNDED``. Callers reproducing a truncating COMPUTE
        must pass ``ROUND_DOWN`` explicitly.

    Returns
    -------
    bytes of length ``comp3_length(digits)``.
    """
    if scale == 0 and type(value) is int:
        # Fast path for the many integer-valued fields (sequence numbers,
        # counts, YYDDD). Identical result, no Decimal round trip.
        unscaled = value
    else:
        d = _dec(value)
        q = d.quantize(_QUANTUM[scale], rounding=rounding, context=CABS_CONTEXT)
        unscaled = int(q.scaleb(scale, context=CABS_CONTEXT).to_integral_value())

    negative = unscaled < 0
    if negative and not signed:
        raise ValueError("negative value %s cannot be stored in unsigned PIC 9" % (value,))

    magnitude = abs(unscaled)
    digit_str = str(magnitude)
    if len(digit_str) > digits:
        raise ValueError(
            "value %s needs %d digits, field declares %d" % (value, len(digit_str), digits)
        )

    nbytes = comp3_length(digits)
    nibbles = nbytes * 2 - 1  # every nibble except the sign nibble
    if signed:
        sign_nibble = "d" if negative else "c"
    else:
        sign_nibble = "f"
    return bytes.fromhex(digit_str.rjust(nibbles, "0") + sign_nibble)


def unpack_comp3(
    data: bytes,
    scale: int = 0,
    digits: Optional[int] = None,
    strict: bool = True,
) -> Decimal:
    """Unpack COMP-3 bytes into a ``Decimal`` carrying the declared scale.

    The returned Decimal has exactly ``scale`` decimal places -- the scale
    is preserved rather than normalised away, so ``0.00`` and ``0`` remain
    distinguishable and a five-decimal rate never silently becomes a
    two-decimal one.

    Parameters
    ----------
    strict:
        When ``True`` a non-decimal digit nibble raises ``ValueError`` --
        the Python analogue of an S0C7 data exception. When ``False`` the
        offending nibble is treated as zero and processing continues, which
        is what some of the estate's own recovery paths do.
    """
    if not data:
        raise ValueError("empty COMP-3 field")
    hex_str = data.hex()
    sign_nibble = hex_str[-1]
    digit_nibbles = hex_str[:-1]

    if sign_nibble not in _VALID_SIGN_NIBBLES and not sign_nibble.isdigit():
        raise ValueError("invalid COMP-3 sign nibble %r" % sign_nibble)

    if not digit_nibbles.isdigit():
        if strict:
            raise ValueError(
                "invalid COMP-3 digit nibbles %r (S0C7 equivalent)" % digit_nibbles
            )
        digit_nibbles = "".join(c if c.isdigit() else "0" for c in digit_nibbles)

    if digits is not None and len(data) != comp3_length(digits):
        raise ValueError(
            "field length %d does not match PIC 9(%d) COMP-3 (expected %d)"
            % (len(data), digits, comp3_length(digits))
        )

    magnitude = int(digit_nibbles) if digit_nibbles else 0
    result = Decimal(magnitude).scaleb(-scale, context=CABS_CONTEXT).quantize(
        _QUANTUM[scale], context=CABS_CONTEXT
    )
    if sign_nibble in _NEGATIVE_SIGN_NIBBLES:
        result = -result
    return result


# ---------------------------------------------------------------------------
# Zoned decimal (DISPLAY numeric)
# ---------------------------------------------------------------------------


def pack_zoned(
    value: Any,
    digits: int,
    scale: int = 0,
    signed: bool = False,
    rounding: str = ROUND_HALF_UP,
) -> bytes:
    """Encode a value as EBCDIC zoned decimal (``PIC 9(n)`` DISPLAY).

    One byte per digit. Unsigned digits are 0xF0-0xF9. A signed field
    carries a trailing overpunch: 0xC0|digit for positive, 0xD0|digit for
    negative.
    """
    if scale == 0 and type(value) is int:
        unscaled = value  # fast path, see pack_comp3
    elif scale == 0 and type(value) is str and value.isdigit():
        unscaled = int(value)  # YYDDD and similar arrive as digit strings
    else:
        d = _dec(value)
        q = d.quantize(_QUANTUM[scale], rounding=rounding, context=CABS_CONTEXT)
        unscaled = int(q.scaleb(scale, context=CABS_CONTEXT).to_integral_value())

    negative = unscaled < 0
    if negative and not signed:
        raise ValueError("negative value %s cannot be stored in unsigned zoned field" % (value,))
    digit_str = str(abs(unscaled)).rjust(digits, "0")
    if len(digit_str) > digits:
        raise ValueError("value %s exceeds PIC 9(%d)" % (value, digits))

    out = bytearray(digit_str.encode("ascii").translate(_ASCII_DIGIT_TO_ZONED))
    if signed:
        out[-1] = (0xD0 if negative else 0xC0) | (out[-1] & 0x0F)
    return bytes(out)


def unpack_zoned(data: bytes, scale: int = 0, signed: bool = False) -> Decimal:
    """Decode EBCDIC zoned decimal into a ``Decimal`` with the declared scale."""
    if not data:
        raise ValueError("empty zoned field")
    digits: List[str] = []
    negative = False
    for i, byte in enumerate(data):
        low = byte & 0x0F
        high = byte >> 4
        if low > 9:
            raise ValueError("invalid zoned digit nibble 0x%X at offset %d" % (low, i))
        if i == len(data) - 1 and high in (0x0D, 0x0B):
            negative = True
        digits.append(str(low))
    result = Decimal("".join(digits)).scaleb(-scale, context=CABS_CONTEXT).quantize(
        _QUANTUM[scale], context=CABS_CONTEXT
    )
    if negative:
        result = -result
    if negative and not signed:
        # A negative overpunch in a field declared unsigned is a real data
        # condition in this estate; report the value, let the caller decide.
        pass
    return result


# ---------------------------------------------------------------------------
# YYDDD Julian dates
# ---------------------------------------------------------------------------

#: The estate's two-digit-year pivot. Years 70-99 are 19xx, 00-69 are 20xx.
#: This literal appears inline in CABING04, CABING08 and CABJUR07.
DEFAULT_PIVOT = 70


def date_to_yyddd(value: date, pivot: int = DEFAULT_PIVOT) -> str:
    """Render a date as a 5-character YYDDD string."""
    del pivot  # rendering never needs the pivot; only interpretation does
    return "%02d%03d" % (value.year % 100, value.timetuple().tm_yday)


def yyddd_to_date(yyddd: str, pivot: int = DEFAULT_PIVOT) -> Optional[date]:
    """Interpret a YYDDD string as a real date, or ``None`` if it is not one.

    ``00000`` (the estate's low-values date) and any day-of-year outside
    1..366 return ``None`` rather than raising, because the generator
    deliberately produces invalid dates for the edit suite to catch.
    """
    s = str(yyddd).strip()
    if len(s) != 5 or not s.isdigit():
        return None
    yy = int(s[:2])
    ddd = int(s[2:])
    if ddd < 1 or ddd > 366:
        return None
    year = 1900 + yy if yy >= pivot else 2000 + yy
    try:
        result = date(year, 1, 1) + timedelta(days=ddd - 1)
    except (ValueError, OverflowError):
        return None
    if result.year != year:
        # Day 366 of a non-leap year. Adding a day count to a packed YYDDD
        # produces values like this; they are not dates.
        return None
    return result


def yyddd_to_abs(yyddd: str, pivot: int = DEFAULT_PIVOT) -> Optional[int]:
    """Absolute day number (proleptic Gregorian ordinal) for a YYDDD value.

    Day arithmetic done on an absolute day number rolls the year; day
    arithmetic done on the packed YYDDD itself does not.
    """
    d = yyddd_to_date(yyddd, pivot)
    return None if d is None else d.toordinal()


def abs_to_yyddd(ordinal: int, pivot: int = DEFAULT_PIVOT) -> str:
    """Inverse of :func:`yyddd_to_abs`."""
    return date_to_yyddd(date.fromordinal(ordinal), pivot)


def yyddd_add_days(yyddd: str, days: int, pivot: int = DEFAULT_PIVOT) -> Optional[str]:
    """Add ``days`` to a YYDDD value *with* year rollover.

    Contrast with the estate's ``COMPUTE WS-WA-THRU-N = WS-WA-FROM-N +
    WS-RS-SPAN-DAYS``, which adds to the packed YYDDD integer and produces
    24366, 24368 and similar impossible values.
    """
    base = yyddd_to_abs(yyddd, pivot)
    if base is None:
        return None
    return abs_to_yyddd(base + days, pivot)


# ---------------------------------------------------------------------------
# Deterministic randomness
# ---------------------------------------------------------------------------


class DeterministicRandom:
    """A seeded RNG that hands out reproducible, independent sub-streams.

    Each sub-stream is seeded from BLAKE2b(master_seed || name), so the
    values drawn by ``rng.substream("carrier")`` do not depend on how many
    values ``rng.substream("cdr")`` has drawn. That property is what makes
    ``--profile SMOKE`` a genuine subset-shaped sample of ``--profile
    DAILY`` rather than a different universe.
    """

    def __init__(self, seed: int, name: str = "root") -> None:
        self.seed = int(seed)
        self.name = name
        self._random = Random(self._derive(seed, name))
        self._children: Dict[str, "DeterministicRandom"] = {}

    @staticmethod
    def _derive(seed: int, name: str) -> int:
        digest = hashlib.blake2b(
            ("%d|%s" % (seed, name)).encode("utf-8"), digest_size=16
        ).digest()
        return int.from_bytes(digest, "big")

    def substream(self, name: str) -> "DeterministicRandom":
        """Return (and cache) a named independent sub-stream."""
        if name not in self._children:
            self._children[name] = DeterministicRandom(self.seed, "%s/%s" % (self.name, name))
        return self._children[name]

    # -- thin, explicit delegation; no __getattr__ magic ------------------
    def randint(self, a: int, b: int) -> int:
        return self._random.randint(a, b)

    def choice(self, seq: Sequence[Any]) -> Any:
        return self._random.choice(seq)

    def choices(self, population: Sequence[Any], weights: Sequence[int], k: int = 1) -> List[Any]:
        return self._random.choices(population, weights=weights, k=k)

    def random(self) -> float:
        """Uniform float in [0,1). Used ONLY for branch selection, never for money."""
        return self._random.random()

    def shuffle(self, seq: List[Any]) -> None:
        self._random.shuffle(seq)

    def sample(self, population: Sequence[Any], k: int) -> List[Any]:
        return self._random.sample(list(population), k)

    def decimal(self, low: str, high: str, scale: int) -> Decimal:
        """Uniform Decimal in [low, high] with exactly ``scale`` places.

        Drawn as an integer over the scaled range, so no float ever touches
        a monetary or rate value.
        """
        ctx = CABS_CONTEXT
        lo = int(_dec(low).scaleb(scale, context=ctx).to_integral_value())
        hi = int(_dec(high).scaleb(scale, context=ctx).to_integral_value())
        n = self._random.randint(lo, hi)
        return Decimal(n).scaleb(-scale, context=ctx).quantize(_QUANTUM[scale], context=ctx)


# ---------------------------------------------------------------------------
# PIC clause parsing
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class PictureSpec:
    """A parsed PIC clause."""

    raw: str
    category: str  # "alphanumeric" | "numeric" | "edited"
    digits: int  # total digit positions (numeric only)
    scale: int  # digits after the implied decimal point
    signed: bool
    display_length: int  # bytes when USAGE IS DISPLAY

    @property
    def is_numeric(self) -> bool:
        return self.category == "numeric"

    @property
    def is_edited(self) -> bool:
        return self.category == "edited"


#: Every PIC symbol that appears anywhere in the frozen copybooks, including
#: the report-editing pictures in CABSPRNT and CABSRT04.
_PIC_TOKEN = re.compile(r"(CR|DB|[9AXSVZP,./*+\-$B0])(?:\((\d+)\))?", re.IGNORECASE)

#: Symbols that make a picture *edited* rather than plain numeric.
_EDIT_SYMBOLS = frozenset(",./*+-$B0")


def parse_picture(pic: str) -> PictureSpec:
    """Parse a COBOL PIC clause into a :class:`PictureSpec`.

    Handles the forms present in the frozen copybooks:
    ``X(nn)``, ``9(nn)``, ``S9(nn)``, ``S9(nn)V9(mm)``, ``9(nn)V9(mm)`` and
    the report-editing pictures ``ZZ,ZZZ,ZZ9.99``, ``-9(9)``, ``9(9)CR``.

    Edited pictures are sized (so offsets stay right) but are not treated as
    values to be compared numerically -- an edited field is presentation,
    and the harness compares the underlying figure, not its rendering.
    """
    text = pic.strip().upper()
    signed = False
    seen_v = False
    int_digits = 0
    dec_digits = 0
    alpha_chars = 0
    edit_chars = 0
    edited = False
    pos = 0
    for match in _PIC_TOKEN.finditer(text):
        if match.start() != pos:
            raise ValueError("unparsable PIC clause %r near offset %d" % (pic, pos))
        pos = match.end()
        symbol = match.group(1).upper()
        count = int(match.group(2)) if match.group(2) else 1
        if symbol == "S":
            signed = True
        elif symbol == "V":
            seen_v = True
        elif symbol == "9":
            if seen_v:
                dec_digits += count
            else:
                int_digits += count
        elif symbol in ("X", "A"):
            alpha_chars += count
        elif symbol == "Z":
            edited = True
            int_digits += count
        elif symbol in ("CR", "DB"):
            edited = True
            signed = True
            edit_chars += 2 * count
        elif symbol in _EDIT_SYMBOLS:
            edited = True
            if symbol == ".":
                seen_v = True
            edit_chars += count
        elif symbol == "P":
            raise ValueError("PIC P (scaling position) is not used in this estate: %r" % pic)
    if pos != len(text):
        raise ValueError("unparsable PIC clause %r" % pic)

    if alpha_chars and (int_digits or dec_digits):
        raise ValueError("mixed alphanumeric/numeric PIC not supported: %r" % pic)

    if alpha_chars:
        return PictureSpec(pic.strip(), "alphanumeric", 0, 0, False, alpha_chars)

    digits = int_digits + dec_digits
    if digits == 0 and edit_chars == 0:
        raise ValueError("PIC clause declares no positions: %r" % pic)
    if edited:
        return PictureSpec(pic.strip(), "edited", digits, dec_digits, signed, digits + edit_chars)
    return PictureSpec(pic.strip(), "numeric", digits, dec_digits, signed, digits)


# ---------------------------------------------------------------------------
# Copybook model
# ---------------------------------------------------------------------------


@dataclass
class Field:
    """One data item from a copybook, with its computed position."""

    name: str
    level: int
    pic: Optional[PictureSpec] = None
    usage: str = "DISPLAY"  # DISPLAY | COMP-3
    offset: int = 0  # byte offset from the start of the 01 record
    length: int = 0  # byte length of one occurrence
    redefines: Optional[str] = None
    occurs_min: int = 0
    occurs_max: int = 0
    odo_field: Optional[str] = None
    parent: Optional[str] = None
    children: List["Field"] = dc_field(default_factory=list)
    conditions: Dict[str, List[str]] = dc_field(default_factory=dict)  # 88-levels
    #: For a field inside an OCCURS group: the stride between occurrences.
    occurs_stride: int = 0
    occurs_base: int = 0
    occurs_owner: Optional[str] = None

    @property
    def is_group(self) -> bool:
        return bool(self.children)

    @property
    def is_comp3(self) -> bool:
        return self.usage == "COMP-3"

    @property
    def digits(self) -> int:
        return self.pic.digits if self.pic else 0

    @property
    def scale(self) -> int:
        return self.pic.scale if self.pic else 0

    @property
    def signed(self) -> bool:
        return self.pic.signed if self.pic else False

    @property
    def pic_text(self) -> str:
        base = self.pic.raw if self.pic else "GROUP"
        return base + (" COMP-3" if self.is_comp3 else "")

    def offset_for(self, index: int = 1) -> int:
        """Byte offset of occurrence ``index`` (1-based) of this field."""
        if self.occurs_stride == 0:
            if index != 1:
                raise ValueError("%s is not inside an OCCURS group" % self.name)
            return self.offset
        if index < 1:
            raise ValueError("OCCURS index is 1-based")
        return self.offset + (index - 1) * self.occurs_stride


@dataclass
class Layout:
    """A parsed 01-level record layout."""

    name: str  # copybook member name, e.g. CABSCDR
    record_name: str  # 01-level name, e.g. CABS-CDR-RECORD
    root: Field
    fields: Dict[str, Field] = dc_field(default_factory=dict)
    order: List[str] = dc_field(default_factory=list)
    lrecl_declared: Optional[int] = None
    recfm_declared: Optional[str] = None
    diagnostics: List[str] = dc_field(default_factory=list)

    @property
    def computed_length(self) -> int:
        """Maximum length: every OCCURS DEPENDING ON expanded to its maximum."""
        return self.root.length

    @property
    def odo_fields(self) -> List[Field]:
        """Groups declared ``OCCURS ... DEPENDING ON``."""
        return [self.fields[n] for n in self.order if self.fields[n].odo_field]

    @property
    def is_variable(self) -> bool:
        """True for RECFM V/VB layouts, or any layout carrying an ODO table."""
        return bool(self.recfm_declared and self.recfm_declared.startswith("V")) or bool(
            self.odo_fields
        )

    @property
    def has_rdw(self) -> bool:
        """True where the *declared* RECFM is V/VB, so records carry an RDW.

        An ODO table alone does not put an RDW on a record -- a table can sit
        inside an FB record written at its maximum length. Only a declared
        RECFM of V or VB does, which is why this is deliberately narrower
        than :attr:`is_variable`.
        """
        return bool(self.recfm_declared and self.recfm_declared.upper().startswith("V"))

    @property
    def expected_declared_lrecl(self) -> int:
        """What the header comment, the JCL DCB and IDCAMS should all say.

        A copybook describes the *data* portion of a record only. For RECFM
        V/VB the access method prefixes each record with a 4-byte Record
        Descriptor Word, and every LRECL quoted against such a dataset is the
        physical length -- data plus RDW. So the copybook and the DCB agree
        when ``declared LRECL == maximum computed data length + 4``, not when
        the two numbers are equal. For RECFM F/FB there is no RDW and the two
        numbers should match exactly.
        """
        return self.root.length + (RDW_LENGTH if self.has_rdw else 0)

    @property
    def lrecl(self) -> int:
        """The length fixed-format records are actually written at.

        The header comment wins where it exists, because that is what the
        JCL DCB and the IDCAMS RECORDSIZE say. Where the computed length is
        shorter, records are padded; a diagnostic records the difference.
        For variable-length layouts the maximum computed length is used and
        each record is written at its natural length.
        """
        if self.is_variable:
            return self.root.length
        return self.lrecl_declared or self.root.length

    def length_for(self, odo_counts: Optional[Dict[str, int]] = None) -> int:
        """Natural length of a record given its OCCURS DEPENDING ON counts."""
        if not self.odo_fields:
            return self.lrecl
        total = self.root.length
        for g in self.odo_fields:
            used = (odo_counts or {}).get(g.name, g.occurs_min)
            total -= (g.occurs_max - used) * g.length
        return total

    def field(self, name: str) -> Field:
        try:
            return self.fields[name.upper()]
        except KeyError:
            raise KeyError("%s has no field %r" % (self.name, name)) from None

    def elementary_fields(self, include_redefines: bool = True) -> List[Field]:
        out = []
        for name in self.order:
            f = self.fields[name]
            if f.is_group:
                continue
            if not include_redefines and self._under_redefines(f):
                continue
            out.append(f)
        return out

    def _under_redefines(self, f: Field) -> bool:
        cur: Optional[Field] = f
        while cur is not None:
            if cur.redefines:
                return True
            cur = self.fields.get(cur.parent) if cur.parent else None
        return False

    def variant_groups(self) -> List[Field]:
        """Top-level REDEFINES groups, e.g. the three CABSCDR variants."""
        return [self.fields[n] for n in self.order if self.fields[n].redefines]


_COMMENT_INDICATOR = frozenset("*/")


def _copybook_statements(text: str) -> Tuple[List[str], List[str]]:
    """Split fixed-format copybook source into statements and comment lines."""
    statements: List[str] = []
    comments: List[str] = []
    buffer = ""
    for raw_line in text.splitlines():
        line = raw_line.rstrip("\n\r")
        indicator = line[6] if len(line) > 6 else " "
        code = line[7:72] if len(line) > 7 else ""
        if indicator in _COMMENT_INDICATOR:
            comments.append((line[7:72] if len(line) > 7 else "").rstrip())
            continue
        if not code.strip():
            continue
        buffer += " " + code.strip()
        while "." in buffer:
            head, buffer = buffer.split(".", 1)
            if head.strip():
                statements.append(" ".join(head.split()))
    if buffer.strip():
        statements.append(" ".join(buffer.split()))
    return statements, comments


_LRECL_RE = re.compile(r"LRECL\s+(\d{3,5})", re.IGNORECASE)
_RECFM_RE = re.compile(r"RECFM\s+([A-Z]{1,3})", re.IGNORECASE)
_LEVEL_RE = re.compile(r"^(\d{2})\s+(\S+)(.*)$")
_OCCURS_RE = re.compile(
    r"OCCURS\s+(?:(\d+)\s+TO\s+)?(\d+)\s+TIMES(?:\s+DEPENDING\s+ON\s+(\S+))?", re.IGNORECASE
)
_REDEFINES_RE = re.compile(r"REDEFINES\s+(\S+)", re.IGNORECASE)
_PIC_RE = re.compile(r"\bPIC(?:TURE)?(?:\s+IS)?\s+(\S+)", re.IGNORECASE)
_USAGE_RE = re.compile(r"\b(COMP-3|COMPUTATIONAL-3|PACKED-DECIMAL|COMP|BINARY|DISPLAY)\b", re.IGNORECASE)
_VALUE_RE = re.compile(r"VALUES?\s+(?:IS\s+|ARE\s+)?(.*)$", re.IGNORECASE)


def parse_copybook(path: Path | str, name: Optional[str] = None) -> Layout:
    """Parse the first 01-level record in a ``.cpy`` member.

    See :func:`parse_copybook_all` for members that declare several.
    """
    return parse_copybook_all(path, name)[0]


def parse_copybook_all(path: Path | str, name: Optional[str] = None) -> List[Layout]:
    """Parse every 01-level record in a ``.cpy`` member into a :class:`Layout`.

    Offsets and lengths are computed from the declaration. Nothing is
    inferred or corrected; disagreements are appended to
    ``Layout.diagnostics``.

    Level 66 RENAMES items are recorded as zero-space aliases spanning the
    bytes they rename. Level 88 condition names are attached to the item
    they qualify.
    """
    path = Path(path)
    text = path.read_text(encoding="utf-8", errors="replace")
    member = (name or path.stem).upper()
    statements, comments = _copybook_statements(text)

    lrecl: Optional[int] = None
    recfm: Optional[str] = None
    for c in comments:
        if lrecl is None:
            m = _LRECL_RE.search(c)
            if m:
                lrecl = int(m.group(1))
        if recfm is None:
            m = _RECFM_RE.search(c)
            if m:
                recfm = m.group(1).upper()

    records: List[Tuple[Field, Dict[str, Field], List[str], List[str]]] = []
    root: Optional[Field] = None
    fields: Dict[str, Field] = {}
    order: List[str] = []
    stack: List[Field] = []
    last_elementary: Optional[Field] = None
    diagnostics: List[str] = []
    renames: List[Tuple[str, str, str]] = []

    for stmt in statements:
        m = _LEVEL_RE.match(stmt)
        if not m:
            continue
        level = int(m.group(1))
        fname = m.group(2).upper().rstrip(".")
        rest = m.group(3) or ""

        if level == 66:
            rn = re.search(r"RENAMES\s+(\S+)(?:\s+(?:THRU|THROUGH)\s+(\S+))?", rest, re.IGNORECASE)
            if rn:
                renames.append(
                    (fname, rn.group(1).upper().rstrip("."), (rn.group(2) or rn.group(1)).upper().rstrip("."))
                )
            continue

        if level == 88:
            if last_elementary is None:
                diagnostics.append("88-level %s has no parent item" % fname)
                continue
            vm = _VALUE_RE.search(rest)
            values: List[str] = []
            if vm:
                values = [
                    quoted if quoted else bare
                    for quoted, bare in re.findall(r"'([^']*)'|\b(\d+)\b", vm.group(1))
                ]
            last_elementary.conditions[fname] = values
            continue

        f = Field(name=fname, level=level)

        rm = _REDEFINES_RE.search(rest)
        if rm:
            f.redefines = rm.group(1).upper()

        om = _OCCURS_RE.search(rest)
        if om:
            f.occurs_min = int(om.group(1)) if om.group(1) else int(om.group(2))
            f.occurs_max = int(om.group(2))
            f.odo_field = om.group(3).upper() if om.group(3) else None

        pm = _PIC_RE.search(rest)
        if pm:
            f.pic = parse_picture(pm.group(1).rstrip("."))

        um = _USAGE_RE.search(rest)
        if um:
            usage = um.group(1).upper()
            if usage in ("COMP-3", "COMPUTATIONAL-3", "PACKED-DECIMAL"):
                f.usage = "COMP-3"
            elif usage in ("COMP", "BINARY"):
                f.usage = "COMP"
            else:
                f.usage = "DISPLAY"

        # attach to tree
        while stack and stack[-1].level >= level:
            stack.pop()
        if stack:
            f.parent = stack[-1].name
            stack[-1].children.append(f)
        else:
            if root is not None:
                records.append((root, fields, order, diagnostics))
                fields, order, diagnostics = {}, [], []
            root = f

        if fname in fields:
            diagnostics.append("duplicate field name %s" % fname)
        fields[fname] = f
        order.append(fname)
        stack.append(f)
        if f.pic is not None:
            last_elementary = f

    if root is None:
        raise ValueError("%s contains no 01-level record" % path)
    records.append((root, fields, order, diagnostics))

    layouts: List[Layout] = []
    for idx, (rec_root, rec_fields, rec_order, rec_diags) in enumerate(records):
        # The LRECL/RECFM in the header comment describes the member's
        # principal record only. Subsequent 01-levels in the same member are
        # working-storage areas and carry no DCB of their own.
        layouts.append(
            _finalise_layout(
                member,
                rec_root,
                rec_fields,
                rec_order,
                rec_diags,
                renames,
                lrecl if idx == 0 else None,
                recfm if idx == 0 else None,
            )
        )
    return layouts


def _finalise_layout(
    member: str,
    root: Field,
    fields: Dict[str, Field],
    order: List[str],
    diagnostics: List[str],
    renames: List[Tuple[str, str, str]],
    lrecl: Optional[int],
    recfm: Optional[str],
) -> Layout:
    """Size, position and package one 01-level record."""
    _size_field(root, fields, diagnostics)
    _assign_offsets(root, fields, 0, 0, 0, None)

    for alias, first, last in renames:
        if first in fields and last in fields:
            start = fields[first]
            end = fields[last]
            f = Field(name=alias, level=66, offset=start.offset, parent=root.name)
            f.length = end.offset + end.length - start.offset
            f.redefines = first  # an alias occupies no new space
            fields[alias] = f
            order.append(alias)

    odo_groups = [f for f in fields.values() if f.odo_field]
    odo_names = ", ".join(g.odo_field or "?" for g in odo_groups)
    has_rdw = bool(recfm and recfm.upper().startswith("V"))
    if lrecl is not None:
        if has_rdw:
            # RECFM V/VB. The copybook describes data bytes only; the LRECL on
            # the DCB is the physical length and carries the 4-byte RDW. The
            # two therefore agree at declared == computed + RDW_LENGTH, and
            # only a departure from that is worth reporting.
            expected = root.length + RDW_LENGTH
            if lrecl != expected:
                diagnostics.append(
                    "declared LRECL %d for RECFM %s against a maximum computed "
                    "data length of %d; %d + %d byte RDW = %d was expected, so "
                    "the declared LRECL is %+d bytes out%s"
                    % (
                        lrecl,
                        recfm,
                        root.length,
                        root.length,
                        RDW_LENGTH,
                        expected,
                        lrecl - expected,
                        (
                            " (the record is variable, OCCURS DEPENDING ON %s, "
                            "and is written at its natural length)" % odo_names
                        )
                        if odo_groups
                        else "",
                    )
                )
        elif root.length != lrecl:
            if odo_groups:
                diagnostics.append(
                    "declared LRECL %d against a maximum computed length of %d; the "
                    "record is variable (OCCURS DEPENDING ON %s) and is written at "
                    "its natural length" % (lrecl, root.length, odo_names)
                )
            else:
                diagnostics.append(
                    "declared LRECL %d does not equal computed record length %d "
                    "(difference %+d bytes); records are written at the declared LRECL "
                    "and the shortfall is filled with EBCDIC spaces"
                    % (lrecl, root.length, lrecl - root.length)
                )

    layout = Layout(
        name=member,
        record_name=root.name,
        root=root,
        fields=fields,
        order=order,
        lrecl_declared=lrecl,
        recfm_declared=recfm,
        diagnostics=diagnostics,
    )
    return layout


def _elementary_length(f: Field) -> int:
    if f.pic is None:
        raise ValueError("%s has neither PIC nor children" % f.name)
    if f.usage == "COMP-3":
        return comp3_length(f.pic.digits)
    if f.usage == "COMP":
        # Not present in the frozen copybooks; sized per IBM halfword/fullword.
        return 2 if f.pic.digits <= 4 else (4 if f.pic.digits <= 9 else 8)
    return f.pic.display_length


def _size_field(f: Field, fields: Dict[str, Field], diagnostics: List[str]) -> int:
    """Bottom-up length computation. Returns the length of one occurrence."""
    if f.children:
        total = 0
        for child in f.children:
            child_len = _size_field(child, fields, diagnostics)
            if child.redefines:
                target = fields.get(child.redefines)
                if target is None:
                    diagnostics.append(
                        "%s REDEFINES unknown item %s" % (child.name, child.redefines)
                    )
                elif child.length != target.length:
                    diagnostics.append(
                        "%s REDEFINES %s but is %d bytes against %d "
                        "(difference %+d); the overlay is honoured at the "
                        "redefined item's offset and the surplus/shortfall is "
                        "reported, not corrected"
                        % (
                            child.name,
                            child.redefines,
                            child.length,
                            target.length,
                            child.length - target.length,
                        )
                    )
                continue  # a REDEFINES overlay does not advance the group
            occurrences = child.occurs_max or 1
            total += child_len * occurrences
        f.length = total
    else:
        f.length = _elementary_length(f)
    return f.length


def _assign_offsets(
    f: Field,
    fields: Dict[str, Field],
    base: int,
    stride: int,
    occ_base: int,
    owner: Optional[str],
) -> None:
    f.offset = base
    f.occurs_stride = stride
    f.occurs_base = occ_base
    f.occurs_owner = owner
    cursor = base
    for child in f.children:
        if child.redefines:
            target = fields.get(child.redefines)
            child_base = target.offset if target is not None else cursor
            _assign_offsets(child, fields, child_base, stride, occ_base, owner)
            continue
        if child.occurs_max:
            _assign_offsets(child, fields, cursor, child.length, cursor, child.name)
            cursor += child.length * child.occurs_max
        else:
            _assign_offsets(child, fields, cursor, stride, occ_base, owner)
            cursor += child.length


def load_layouts(copybook_dir: Path | str, quiet: bool = False) -> Dict[str, Layout]:
    """Parse every ``.cpy`` in a directory.

    The principal record of member ``CABSCDR`` is registered under
    ``CABSCDR``; every 01-level (including the principal) is additionally
    registered under its own record name, so ``CABS-SUSPENSE-RECORD`` in
    CABSERR is reachable without knowing which member it lives in.
    """
    copybook_dir = Path(copybook_dir)
    layouts: Dict[str, Layout] = {}
    for cpy in sorted(copybook_dir.glob("*.cpy")):
        try:
            parsed = parse_copybook_all(cpy)
        except Exception as exc:  # pragma: no cover - defensive
            if not quiet:
                print("WARNING: could not parse %s: %s" % (cpy.name, exc))
            continue
        layouts[cpy.stem.upper()] = parsed[0]
        for lay in parsed:
            layouts.setdefault(lay.record_name, lay)
    return layouts


# ---------------------------------------------------------------------------
# Record building and decoding
# ---------------------------------------------------------------------------


#: Encoding kinds, resolved once per layout instead of per field write.
_KIND_COMP3 = 0
_KIND_ZONED = 1
_KIND_CHAR = 2


def encoding_plan(layout: Layout) -> Dict[str, Tuple[int, int, int, int, int, bool, int]]:
    """Flatten a layout into ``name -> (kind, offset, length, digits, scale,
    signed, stride)``.

    Built once and cached on the layout. At a hundred million records the
    per-write attribute lookups this removes are worth roughly a third of
    total generation time.
    """
    cached = getattr(layout, "_encoding_plan", None)
    if cached is not None:
        return cached
    plan: Dict[str, Tuple[int, int, int, int, int, bool, int]] = {}
    for name in layout.order:
        f = layout.fields[name]
        if f.is_group or f.pic is None:
            continue
        if f.usage == "COMP-3":
            kind = _KIND_COMP3
        elif f.pic.is_numeric:
            kind = _KIND_ZONED
        else:
            kind = _KIND_CHAR
        plan[name] = (kind, f.offset, f.length, f.pic.digits, f.pic.scale, f.pic.signed, f.occurs_stride)
    object.__setattr__(layout, "_encoding_plan", plan)
    return plan


class RecordBuilder:
    """Assemble one fixed-length record for a :class:`Layout`.

    Fields are written by name. Unset bytes keep the fill character, which
    defaults to the EBCDIC space -- the same thing a COBOL ``MOVE SPACES TO
    record`` leaves behind.

    Reuse one builder across records via :meth:`reset`; that is the fast
    path and it is what the volume generators do.
    """

    __slots__ = ("layout", "lrecl", "_plan", "_blank", "_buffer", "_written", "_track")

    def __init__(
        self,
        layout: Layout,
        lrecl: Optional[int] = None,
        fill: int = EBCDIC_SPACE,
        track: bool = False,
    ) -> None:
        self.layout = layout
        self.lrecl = lrecl or layout.lrecl
        self._plan = encoding_plan(layout)
        self._blank = bytes([fill]) * self.lrecl
        self._buffer = bytearray(self._blank)
        self._written: Dict[str, Any] = {}
        self._track = track

    def reset(self, fill: Optional[int] = None) -> "RecordBuilder":
        """Blank the buffer for the next record without reallocating."""
        if fill is not None:
            self._blank = bytes([fill]) * self.lrecl
        self._buffer[:] = self._blank
        if self._written:
            self._written.clear()
        return self

    def set(
        self,
        name: str,
        value: Any,
        index: int = 1,
        rounding: str = ROUND_HALF_UP,
    ) -> "RecordBuilder":
        """Encode ``value`` into field ``name`` (occurrence ``index``)."""
        try:
            kind, offset, length, digits, scale, signed, stride = self._plan[name]
        except KeyError:
            entry = self._plan.get(name.upper())
            if entry is None:
                f = self.layout.field(name)  # raises KeyError with the layout name
                raise ValueError(
                    "%s is a group item or has no PIC; set its elementary fields" % f.name
                ) from None
            kind, offset, length, digits, scale, signed, stride = entry
        if index != 1:
            if stride == 0:
                raise ValueError("%s is not inside an OCCURS group" % name)
            offset += (index - 1) * stride
        if kind == _KIND_COMP3:
            data = pack_comp3(value, digits, scale, signed, rounding=rounding)
        elif kind == _KIND_ZONED:
            data = pack_zoned(value, digits, scale, signed, rounding=rounding)
        else:
            data = to_ebcdic(value if isinstance(value, str) else str(value), length)
        end = offset + len(data)
        if end > self.lrecl:
            raise ValueError(
                "field %s at offset %d length %d overflows LRECL %d"
                % (name, offset, len(data), self.lrecl)
            )
        self._buffer[offset:end] = data
        if self._track:
            self._written["%s(%d)" % (name, index) if index != 1 else name] = value
        return self

    def set_raw(self, name: str, data: bytes, index: int = 1) -> "RecordBuilder":
        """Write raw bytes over a field -- used to inject corrupt data."""
        f = self.layout.field(name)
        offset = f.offset_for(index)
        self._buffer[offset : offset + len(data)] = data
        return self

    def build(self) -> bytes:
        return bytes(self._buffer)

    @property
    def written(self) -> Dict[str, Any]:
        return dict(self._written)


def decode_record(
    layout: Layout,
    data: bytes,
    variant: Optional[str] = None,
    strict: bool = False,
) -> Dict[str, Any]:
    """Decode a record into ``{field_name: value}``.

    Group items are skipped. REDEFINES overlays are skipped unless the
    overlay group's name is passed as ``variant``, because decoding all
    three CABSCDR variants at once produces three contradictory readings of
    the same 96 bytes -- which is exactly the trap the copybook warns about.
    """
    out: Dict[str, Any] = {}
    for name in layout.order:
        f = layout.fields[name]
        if f.is_group:
            continue
        if layout._under_redefines(f):
            owner = f
            while owner is not None and not owner.redefines:
                owner = layout.fields.get(owner.parent) if owner.parent else None
            if owner is None or (variant or "").upper() != owner.name:
                continue
        occurrences = 1
        if f.occurs_owner:
            occurrences = layout.fields[f.occurs_owner].occurs_max
        for idx in range(1, occurrences + 1):
            off = f.offset_for(idx)
            chunk = data[off : off + f.length]
            if len(chunk) < f.length:
                if strict:
                    raise ValueError("record too short for %s" % name)
                break
            key = name if occurrences == 1 else "%s(%d)" % (name, idx)
            try:
                if f.usage == "COMP-3":
                    out[key] = unpack_comp3(chunk, f.scale, f.digits, strict=strict)
                elif f.pic is not None and f.pic.is_numeric:
                    out[key] = unpack_zoned(chunk, f.scale, f.signed)
                else:
                    out[key] = from_ebcdic(chunk)
            except ValueError:
                if strict:
                    raise
                out[key] = None
    return out


# ---------------------------------------------------------------------------
# Output
# ---------------------------------------------------------------------------


class FixedRecordWriter:
    """Write fixed-length records and accumulate the totals the run manifest
    needs: record count, byte count and a SHA-256 of the file."""

    def __init__(self, path: Path | str, lrecl: int) -> None:
        self.path = Path(path)
        self.lrecl = lrecl
        self.path.parent.mkdir(parents=True, exist_ok=True)
        self._fh = self.path.open("wb")
        self._hash = hashlib.sha256()
        self.records = 0
        self.bytes_written = 0

    def write(self, record: bytes) -> None:
        if len(record) != self.lrecl:
            raise ValueError(
                "record length %d != LRECL %d for %s" % (len(record), self.lrecl, self.path.name)
            )
        self._fh.write(record)
        self._hash.update(record)
        self.records += 1
        self.bytes_written += len(record)

    def close(self) -> Dict[str, Any]:
        self._fh.close()
        return {
            "file": self.path.name,
            "path": str(self.path),
            "lrecl": self.lrecl,
            "recfm": "FB",
            "records": self.records,
            "bytes": self.bytes_written,
            "sha256": self._hash.hexdigest(),
        }

    def __enter__(self) -> "FixedRecordWriter":
        return self

    def __exit__(self, *exc: Any) -> None:
        if not self._fh.closed:
            self._fh.close()


class HashTotals:
    """The four CABSCTL hash totals, accumulated in Decimal.

    CT-HASH-MINUTES  S9(15)V9(02)
    CT-HASH-AMOUNT   S9(13)V9(05)
    CT-HASH-SEQ      S9(17)
    CT-HASH-OCN      S9(15)

    Accumulation order is arrival order, per CONVENTIONS.md: "Accumulate in
    the order records arrive. Never re-sequence before summing."
    """

    def __init__(self) -> None:
        self.minutes = Decimal("0.00")
        self.amount = Decimal("0.00000")
        self.seq = Decimal(0)
        self.ocn = Decimal(0)
        self.records = 0

    def add(
        self,
        minutes: Any = 0,
        amount: Any = 0,
        seq: int = 0,
        ocn: str | int = 0,
    ) -> None:
        ctx = CABS_CONTEXT
        self.minutes = ctx.add(self.minutes, _dec(minutes).quantize(_QUANTUM[2], context=ctx))
        self.amount = ctx.add(self.amount, _dec(amount).quantize(_QUANTUM[5], context=ctx))
        self.seq = ctx.add(self.seq, Decimal(int(seq)))
        self.ocn = ctx.add(self.ocn, Decimal(self.ocn_numeric(ocn)))
        self.records += 1

    @staticmethod
    def ocn_numeric(ocn: str | int) -> int:
        """OCN hash contribution: numeric OCNs sum as numbers, alphanumeric
        OCNs sum as the total of their EBCDIC code points -- which is what
        CABHASH does."""
        if isinstance(ocn, int):
            return ocn
        s = str(ocn).strip()
        if not s:
            return 0
        if s.isdigit():
            return int(s)
        return sum(to_ebcdic(s))

    def as_dict(self) -> Dict[str, str]:
        return {
            "hash_records": self.records,
            "hash_minutes": str(self.minutes),
            "hash_amount": str(self.amount),
            "hash_seq": str(self.seq),
            "hash_ocn": str(self.ocn),
        }


def iter_fixed_records(path: Path | str, lrecl: int) -> Iterator[bytes]:
    """Stream fixed-length records from a file."""
    path = Path(path)
    with path.open("rb") as fh:
        while True:
            chunk = fh.read(lrecl)
            if not chunk:
                return
            if len(chunk) != lrecl:
                raise ValueError(
                    "%s ends with a partial record of %d bytes (LRECL %d)"
                    % (path.name, len(chunk), lrecl)
                )
            yield chunk


def gdg_name(base: str, generation: int, version: int = 0, suffix: str = ".dat") -> str:
    """Filesystem name for a GDG generation, e.g.
    ``TELCABS.CABS.USAGE.RAW.G0003V00.dat``."""
    return "%s.G%04dV%02d%s" % (base, generation, version, suffix)
