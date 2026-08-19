# CABS Tier 5 — Canonical Interchange Specification

**The form both sides normalise into, and the rules that get them there.**

Reference implementation: [`HARNESS/canonical.py`](../HARNESS/canonical.py).
Schema identifier: `cabs.canonical.v1`.
Schema source: the frozen members of [`COPYBOOKS/`](../COPYBOOKS), parsed by
`GENERATORS/gen_common.py` — the only copybook parser in the toolchain, shared
deliberately between the data generators and the comparison harness so that
neither can drift from the data architecture.

---

## 1. Why a direct comparison is meaningless

A legacy CABS record is EBCDIC cp037 with packed-decimal money fields. A
candidate transformation produces UTF-8 — JSON, Parquet, a relational row, it
does not matter which. Put the two side by side, compare them byte for byte, and
they disagree in every byte position and agree in none.

That result carries no information. `X'100000000C'` and `"100000.00"` are the
same business figure and share not one byte. A byte comparison reports that pair
as different, and reports a genuine one-cent error as different in exactly the
same way, with the same weight. It cannot distinguish *encoded differently* from
*wrong*, which is the only distinction the exercise exists to make.

### And the obvious repair is worse

The repair everyone reaches for first is: decode the mainframe side into whatever
the target produces, then compare in the target's form.

Do not do this. It makes the target's format the definition of correct.

Concretely, in this estate:

- Money is `PIC S9(nn)V9(05) COMP-3`. Rates carry **five** decimals and
  fractional cents are normal and material — `CABBIL09` deliberately keeps
  `BH-HASH-AMOUNT` at five places while moving the same accumulators into
  two-decimal header fields. If the target stores money as a 64-bit float and the
  comparison is performed in the target's form, the legacy value is rounded to
  match on the way in and the loss of the fifth decimal place becomes invisible.
  The comparison reports parity on a figure that has already been damaged.
- A `PIC X(13)` BAN holding a twelve-character value really does have a space in
  position 13. That space is part of the key. A target that strips trailing
  whitespace has changed the key — and if the legacy side is normalised by the
  target's string rules, the change is erased before anything looks at it.
- `CD-CONN-YYDDD` can hold `24366` in a non-leap year, or `00000`, or day `999`.
  These are not dates. A target that parses dates on the way in either rejects
  the record or silently repairs it. Either way the original value stops existing
  and cannot be compared.

**Rule: neither side's native format is the reference.** Both sides normalise
into a third form that neither of them owns, and nothing is compared before that
point. The legacy is not the reference form and the target is not the reference
form. The canonical form is, and it belongs to the validation, not to either
implementation.

---

## 2. The form

Newline-delimited JSON, one object per record. Every value is a string, an
integer or null — so the same object model writes to Parquet unchanged, and the
field map is a fixed schema per layout.

```json
{"_schema":"cabs.canonical.v1","_layout":"CABSCDR","_record":"CABS-CDR-RECORD",
 "_side":"legacy","_file":"TELCABS.CABS.USAGE.RAW.G0001V00.dat","_ordinal":0,
 "_variant":"CD-VOICE-DETAIL","_key":"0288|813G1234567X |42",
 "f":{
   "CD-OCN":        {"t":"str","pic":"X(04)",        "u":"DISPLAY","o":0, "l":4, "v":"0288"},
   "CD-BAN":        {"t":"str","pic":"X(13)",        "u":"DISPLAY","o":4, "l":13,"v":"813G1234567X "},
   "CD-SEQ-NBR":    {"t":"dec","pic":"9(09)",        "u":"COMP-3", "o":17,"l":5, "v":"42","s":0},
   "CD-VC-CHG-MIN": {"t":"dec","pic":"S9(07)V9(02)", "u":"COMP-3", "o":77,"l":5, "v":"100000.00","s":2},
   "CD-CONN-YYDDD": {"t":"jul","pic":"9(05)",        "u":"DISPLAY","o":32,"l":5, "v":"24274","iso":"2024-09-30"}
 }}
```

### Envelope fields

| Key | Meaning |
|---|---|
| `_schema` | `cabs.canonical.v1`. A change to any rule in section 3, 4 or 5 changes this. |
| `_layout` | The copybook member the schema came from, e.g. `CABSCDR`. |
| `_record` | The 01-level name inside that member. |
| `_side` | `legacy` or `candidate`. Carried for provenance only; never used in a comparison decision. |
| `_file` | The physical file the record came from. |
| `_ordinal` | Physical position in that file, zero-based. **Diagnostic only. `_ordinal` is never a key** — see section 6. |
| `_variant` | Which REDEFINES overlay was decoded, or null. See section 5. |
| `_key` | The business key, pipe-joined, built from the field list the compare contract declares for this dataset. |
| `f` | The field map. |

### Per-field entry

| Key | Meaning |
|---|---|
| `t` | `str`, `dec`, `jul`, or `raw`. |
| `pic` | The declared PICTURE clause, verbatim, or `GROUP`. |
| `u` | The declared USAGE — `DISPLAY`, `COMP-3`, `COMP`. |
| `o` | Byte offset within the record. |
| `l` | Byte length within the record. |
| `v` | The value, always a string. |
| `s` | Declared decimal scale. Present on `dec` only. |
| `iso` | ISO-8601 form. Present on `jul` only. Null where the raw value is not a real date. |
| `error` | Present on `raw` only: why the field could not be decoded. |

Carrying `pic`, `u`, `o` and `l` on every field is not redundancy. It makes
precision **auditable from the canonical file alone**, without going back to the
copybook. A reviewer can see that a field was declared `S9(07)V9(05)` and is now
arriving with `"s": 2`, and does not have to take anybody's word for it.

---

## 3. The normalisation rules

Every rule below is deliberate, and each one exists because the obvious
alternative loses something.

### 3.1 EBCDIC cp037 → UTF-8

One text encoding, chosen explicitly and named in this document, not inherited
from whatever the reading platform happens to default to.

**Trailing spaces are preserved and never stripped.** A 13-byte BAN holding a
12-character value has a space in position 13. That space is part of the key. A
target that trims it has changed the key, and the canonical form must be able to
say so.

Low-values, high-values and any byte with no cp037 character meaning survive the
round trip; they are not silently replaced.

### 3.2 COMP-3 (packed decimal) → decimal string, with the declared scale preserved

- The value is emitted as a **string**, never a float. A binary float cannot hold
  a five-decimal fractional-cent rate exactly, and this estate's rates carry five
  decimals throughout (`CONVENTIONS.md`: *all money `PIC S9(nn)V9(05) COMP-3`*).
- The **declared** scale — not the scale the value happens to need — is carried in
  `"s"`. So:
  - `0.00` and `0` remain distinguishable, because one declares `s: 2` and the
    other `s: 0`;
  - a five-decimal rate is emitted with five decimal places whatever its current
    value, so `1.50000` never quietly becomes `1.5`;
  - **a five-decimal field silently becoming a two-decimal field is a finding at
    L2 even when today's values happen to agree.** The scale is compared as well
    as the value. This is the single most important consequence of the rule, and
    it is why the scale is stored rather than inferred.
- Sign is decoded from the low-order nibble per the IBM convention (`C`/`F`
  positive, `D` negative) and rendered as a leading `-`.
- **Decoding is strict.** A non-decimal nibble in a packed field is the Python
  equivalent of an S0C7 and must not be read as a zero. See 3.7 for what happens
  next.

### 3.3 Zoned decimal (DISPLAY numeric) → decimal string, same rules

Signed and unsigned zoned fields decode by the same rules and carry the same
`"s"`. The CMDS/RAO industry exchange format handled by `CABSET07` and `CABSET08`
is entirely zoned, and it must compare against packed fields carrying the same
business figure without either side being converted into the other's
representation first.

### 3.4 YYDDD retained raw, alongside ISO

`"t": "jul"` fields carry **both**:

- `"v"` — the raw five characters, exactly as they sit in the record. **This is
  the authoritative value for comparison.**
- `"iso"` — the ISO-8601 date, or `null`.

`"iso"` is `null`, and the record is still perfectly comparable, when the raw
value is `24366` in a non-leap year, `00000`, day `999`, or any value whose
two-digit year lands on the wrong side of a century pivot.

That last case is why the rule has this shape. The estate's pivot is `70`,
declared once in `COPYBOOKS/CABSDATE.cpy` as `DW-PIVOT-YY` and repeated inline in
seven places — and the emergency copy of the assembler date module
(`HLASM/EMERG/CABDATCV.asm`) uses `68`, so the same source-level `CALL 'CABDTCNV'`
resolves `68` to 1968 in the settlement jobs and 2068 in the rating jobs
depending only on STEPLIB order. **If the canonical form stored only the ISO
date, that divergence would be normalised away before anything could see it.**
Storing the raw value keeps the two-digit year visible and lets the comparison
report a difference in the raw characters even where both sides produced a
plausible ISO date.

Group-level YYDDD fields are emitted as well as their halves. `CD-CONN-YYDDD` is
a group of `CD-CONN-YY` and `CD-CONN-DDD`; the halves compare correctly on their
own, but the date only means anything as a whole, so the whole is emitted too
(`_julian_groups` in `canonical.py`).

### 3.5 Every field carries its declared PIC and USAGE

Stated in section 2; restated here because it is a rule, not a convenience.
Precision must be auditable from the canonical file alone.

### 3.6 Group items and filler

Group items are not emitted as values — only their elementary fields are, plus
the group-level Julian dates of 3.4. `FILLER` and the variant-area group items
named in the compare contract's `ignore_fields` are excluded from field
comparison because the chosen overlay already covers those bytes. They are still
compared as bytes at L1 through the record hash, so a change inside a filler area
is not invisible; it is simply not reported as a field difference.

### 3.7 An undecodable field is a finding, not a crash

A packed field with a bad nibble becomes `{"t":"raw","v":"<hex>","error":"..."}`
and the run continues. One unreadable record must not take down a comparison of
half a million, and the failure is itself information: the comparison can still
say *these bytes differ* and the report can say *and here is why they could not
be read*. Strict mode aborts instead, for use when a single bad record should
stop a controlled run.

---

## 4. Variable-length records and OCCURS DEPENDING ON

The estate's bill detail record `CABSBILL` carries 1 to 40 `BD-ELEMENT`
occurrences under `OCCURS DEPENDING ON BD-ELEM-CNT`, VB with LRECL 1651.
`CABBIL02` produces it; `CABBIL03`, `CABBIL09`, `CABBIL11`, `CABFMT01`,
`CABFMT03`, `CABFMT07` and `CABRPT06` consume it.

### Physical reading

Variable-length datasets are read with the IBM **RDW** convention: a four-byte
record descriptor word whose first halfword is the record length *including the
RDW itself*. The RDW is consumed, not emitted — it is an artefact of the access
method, not a business field. A partial RDW or a truncated body raises; it is not
padded out.

### Canonical representation of the occurrences

The occurrence count is read from the ODO field **first**, out of the base
record, and then exactly that many occurrences are decoded. Each occurrence is
emitted as its own field entry, subscripted:

```json
"BD-ELEM-CNT":     {"t":"dec","pic":"9(02)",       "u":"COMP-3","o":40, "l":2,"v":"3","s":0},
"BD-EL-CODE(1)":   {"t":"str","pic":"X(06)",       "u":"DISPLAY","o":44,"l":6,"v":"OA0100"},
"BD-EL-AMOUNT(1)": {"t":"dec","pic":"S9(09)V9(05)","u":"COMP-3","o":56, "l":8,"v":"1234.56789","s":5},
"BD-EL-CODE(2)":   {"t":"str","pic":"X(06)",       "u":"DISPLAY","o":73,"l":6,"v":"TA0200"},
"BD-EL-AMOUNT(2)": {"t":"dec","pic":"S9(09)V9(05)","u":"COMP-3","o":85, "l":8,"v":"98.00000","s":5},
"BD-EL-CODE(3)":   {"t":"str","pic":"X(06)",       "u":"DISPLAY","o":102,"l":6,"v":"CC0300"},
"BD-EL-AMOUNT(3)": {"t":"dec","pic":"S9(09)V9(05)","u":"COMP-3","o":114,"l":8,"v":"0.50000","s":5}
```

Consequences, all of them intended:

- **The element count is itself a compared field.** A candidate that produces the
  right money on the right elements but a different `BD-ELEM-CNT` fails, and the
  failure names the field.
- **A missing occurrence is a missing field, not a null.** `BD-EL-AMOUNT(4)`
  present on one side and absent on the other is an L2 field-set difference. It
  is not silently treated as zero.
- **Occurrence order is preserved and compared.** Elements 1..n are positional in
  the source record; the subscript carries that position. Re-ordering them is a
  difference.
- Each occurrence carries its **own** `o` and `l`, computed for that subscript, so
  the offsets are genuinely different per occurrence and are auditable.
- Where a record is shorter than a declared occurrence would require, decoding of
  that occurrence stops rather than reading past the end of the record. The
  shortfall shows up as absent fields.
- **Flattening to a fixed-width image is a behaviour change, not an
  implementation detail.** The estate does exactly this in two places —
  `CABRAT10 P5200-MOVE-TO-FIXED` moves the VB 1647 group into `PIC X(500)`, and
  `CABRPT06 P4000-BUILD-EXTRACT` moves it into `PIC X(400)` — and in both cases
  everything past the fixed length is silently lost while the key fields,
  restated over the top, keep the record looking correct. The canonical form
  makes the loss visible, because the occurrences that did not survive are simply
  absent fields.

A target that stores the elements as a child table, a JSON array or repeated
columns is free to do so. What it is not free to do is change the count, the
order, the scale or the set of occurrences — and the canonical form is where that
is checked.

---

## 5. REDEFINES: exactly one overlay per record

`CABSCDR` overlays the same 96 bytes three ways — `CD-VOICE-DETAIL`,
`CD-DATA-DETAIL`, `CD-SPCL-DETAIL`. Decoding all three produces three
contradictory readings of the same bytes: one offset is simultaneously a packed
minute count, a service code and a circuit identifier. Emitting all three would
make every record disagree with itself.

**Rule: exactly one overlay is decoded per record, and the contract says which.**

The compare contract declares a `variant_rule` per dataset: the discriminating
field, and a map from its value to an overlay name.

```json
"variant_rule": {
  "field": "CD-REC-TYPE",
  "map": {"01":"CD-VOICE-DETAIL","02":"CD-VOICE-DETAIL","03":"CD-VOICE-DETAIL",
          "04":"CD-DATA-DETAIL","05":"CD-SPCL-DETAIL","06":"CD-SPCL-DETAIL",
          "07":"CD-SPCL-DETAIL","08":"CD-VOICE-DETAIL"},
  "default": "CD-VOICE-DETAIL"
}
```

The chosen overlay is recorded in `_variant` on every record, so which reading was
taken is on the face of the data and does not have to be reconstructed.

Three further rules:

1. **The same rule is applied to both sides.** The discrimination is a property of
   the validation, not of either implementation. A candidate that decides the
   variant differently is not permitted to have its own answer; if the rule is
   wrong, the contract is changed and both sides are re-canonicalised.
2. **No rule means no overlay.** If a dataset has variant groups and the contract
   declares no `variant_rule`, `_variant` is null and no overlay is decoded at
   all. The base record still compares. The alternative — guessing — is worse than
   an incomplete comparison, because a guess is indistinguishable from a fact once
   it is written down.
3. **The ambiguity is recorded, not hidden.** In the frozen copybook the 88-levels
   genuinely overlap: `CD-REC-TYPE` `'03'` satisfies both `CD-VOICE-MOU` and
   `CD-DATA-SVC`, and `'05'` satisfies both `CD-DATA-SVC` and `CD-SPECIAL-ACC`.
   The map above resolves that the same way for both sides, which is the only way
   the comparison can mean anything. **That the ambiguity exists at all is a
   finding for the modernization**, and the compare contract carries a note saying
   so rather than quietly disposing of it. Construct 6 — *named value ranges
   quietly overlap* — is exactly this, and the estate carries 17 instances.

---

## 6. Rules that follow from the form

- **Record position is never a key.** `_ordinal` exists for diagnostics. Keys are
  the business key fields declared per dataset in the compare contract. This holds
  even for the append-only files — `CABING07`'s `AUDLOG` and `CABING08`'s
  `CFWOUT` are `OPEN EXTEND` with no key and their physical order carries meaning
  (construct 18). That is a **finding about the legacy**, to be reported, not a
  licence to make position a key in the comparison.
- **Decimal comparison is exact by default.** A tolerance exists only where the
  compare contract declares one, and the contract requires a `reason` beside it;
  `HARNESS/test_canonical.py` fails the build if a tolerance is declared without
  one. Copying the estate's own tolerances would copy the estate's blindness:
  more than one control in this estate absorbs a precision mismatch under a
  tolerance that was widened to stop it failing, and has therefore reported a
  clean result for decades. The compare contract inherits none of them.
- **Scale is compared as well as value** (3.2).
- **The field set is compared, not just the field values.** An extra field, a
  missing field and a renamed field are all differences.
- **The canonical form is versioned.** Any change to a rule in section 3, 4 or 5
  changes `_schema`, because a comparison run under two different sets of rules is
  not one comparison.

---

## 7. What the canonical form does *not* do

Worth being explicit, because a specification that appears to promise more than it
delivers is worse than one that promises less.

- It does not decide whether a difference matters. It makes differences visible
  and precise; the verdict layer (`HARNESS/verdict.py`) and, ultimately, a person
  decide what to do about them.
- It does not repair bad data. A day-999 date, a bad packed nibble and a negative
  zero all survive into the canonical form as what they are.
- It does not know the business key. The compare contract does — and where the
  contract does not know either, `CONTRACTS/contracts.yaml` records the key as
  `UNDETERMINED`. **609 dataset keys in the register are in exactly that
  position.** A dataset with no declared key can still be compared at L1 on the
  record hash, but the comparison cannot tell a missing record from a moved one.
- It does not cross application or record-type boundaries. The 180-byte CMDS
  industry exchange record, declared in `CABSET07`/`CABSET08` working storage
  rather than in a copybook, has no `CABSSETL` layout, is deliberately excluded
  from the settlement dataset pattern, and is reconciled by its own header and
  trailer hash totals instead.

---

## 8. Using it

```bash
cd HARNESS

# canonicalise one dataset and look at five records
python3 canonical.py ../DATA/legacy/USAGE/TELCABS.CABS.USAGE.RAW.G0001V00.dat \
    --layout CABSCDR --key CD-OCN CD-BAN CD-SEQ-NBR \
    --variant-field CD-REC-TYPE \
    --variant-map '{"01":"CD-VOICE-DETAIL","04":"CD-DATA-DETAIL","06":"CD-SPCL-DETAIL"}' \
    --limit 5

# the 39 tests that hold these rules in place
python3 -m unittest test_canonical -v
```

`test_canonical.py` cross-checks the packed-decimal decoder against a second,
independently written implementation, so a bug in the codec has to be made twice
before it can pass.

---

## 9. Cross-references

| Document | What it holds |
|---|---|
| [`HARNESS/canonical.py`](../HARNESS/canonical.py) | The reference implementation of every rule above. |
| [`HARNESS/_README.md`](../HARNESS/_README.md) | The L1–L5 staged comparison and the three-way verdict that consume this form. |
| [`HARNESS/contracts/compare_contract.json`](../HARNESS/contracts/compare_contract.json) | Per-dataset keys, variant rules, ignore lists, witness fields and tolerances. An input to the comparison; it contains no answers. |
| [`CONTRACTS/contracts.yaml`](contracts.yaml) | The Process Contract Register — what each process consumes, produces, counts and balances. |
| [`CONTRACTS/_README.md`](_README.md) | The contract-first principle and how to score a candidate transformation. |
| [`COPYBOOKS/`](../COPYBOOKS) | The frozen data architecture. The only schema source in the toolchain. |
| [`CONVENTIONS.md`](../CONVENTIONS.md) | Money arithmetic, the five-decimal rule, and the mandatory balancing equation. |
