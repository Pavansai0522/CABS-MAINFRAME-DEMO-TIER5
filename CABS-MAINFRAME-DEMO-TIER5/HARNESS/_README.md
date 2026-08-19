# CABS Tier 5 — Bill-to-Bill Comparison Harness

A staged L1–L5 comparison with a three-way verdict, run at every process
boundary, against a canonical form that neither side owns.

Python 3.9+. No third-party dependencies. Shares the copybook parser and the
packed-decimal codec with `../GENERATORS/gen_common.py`, because the frozen
copybooks must be the only schema source in the toolchain.

---

## 1. Quick start

```bash
cd HARNESS

python3 -m unittest test_canonical -v        # 39 tests

python3 run_compare.py \
    --legacy    ../DATA/legacy \
    --candidate ../DATA/candidate \
    --contracts contracts/compare_contract.json \
    --probes    ../DATA/legacy/USAGE/boundary_probes.json \
    --out       ../DATA/attributed_run

# blind run: the answer key and the signatures are refused
python3 run_compare.py --legacy L --candidate C --blind --out ../DATA/blind_run

# one level only
python3 run_compare.py --legacy L --candidate C --level L3 --out ../DATA/compare_l3
```

Exit codes: `0` MATCH or DIVERGENT-BY-DESIGN only; `1` at least one
DIVERGENT; `2` the harness could not run.

---

## 2. Why a canonical form, and why neither side owns it

Comparing an EBCDIC packed-decimal record to a UTF-8 JSON record byte by
byte is meaningless. They disagree everywhere and agree nowhere, and the
disagreement carries no information about whether the *business figure* is
the same. `X'100000000C'` and `"100000.00"` are the same number and share
not one byte.

The obvious repair — decode the mainframe side into whatever the target
produces and compare there — is worse. It makes the target's format the
definition of correct. Every precision loss on the way in becomes invisible,
because the reference has already been rounded to match. If the target
stores money as a 64-bit float, a comparison performed in the target's form
will happily report parity on a figure that has already lost the fifth
decimal place.

So **both sides normalise into a third form that neither of them owns**, and
nothing is compared before that point.

### The rules

| Rule | Why |
|---|---|
| EBCDIC cp037 → UTF-8 | one text encoding, chosen explicitly |
| Trailing spaces preserved, never stripped | a 13-byte BAN holding 12 characters really does have a space in position 13; a target that strips it has changed the key |
| COMP-3 → decimal **string**, never a float | a float cannot hold a five-decimal fractional-cent rate exactly |
| Declared scale preserved and carried in `"s"` | `0.00` and `0` stay distinguishable; a five-decimal rate stays five decimals whatever its value |
| Zoned decimal → decimal string, same rules | the CMDS exchange format is entirely zoned |
| YYDDD kept **raw** in `"v"`, ISO in `"iso"` | the raw value is authoritative for comparison; `24366` in a non-leap year has no ISO form and `"iso"` is `null`, but the raw value still compares |
| Every field carries its declared PIC, USAGE, offset and length | precision is auditable from the canonical file alone, without going back to the copybook |
| Exactly **one** REDEFINES overlay decoded per record | decoding all three CABSCDR variants produces three contradictory readings of the same 96 bytes |
| An undecodable packed field becomes `t: "raw"` plus an error, not an exception | one bad record must not take down a comparison of half a million; the failure is a finding |

### The form

Newline-delimited JSON, one object per record:

```json
{"_schema":"cabs.canonical.v1","_layout":"CABSCDR","_record":"CABS-CDR-RECORD",
 "_side":"legacy","_file":"TELCABS.CABS.USAGE.RAW.G0001V00.dat","_ordinal":0,
 "_variant":"CD-VOICE-DETAIL","_key":"0288|813G1234567X |42",
 "f":{
   "CD-OCN":       {"t":"str","pic":"X(04)","u":"DISPLAY","o":0,"l":4,"v":"0288"},
   "CD-SEQ-NBR":   {"t":"dec","pic":"9(09)","u":"COMP-3","o":17,"l":5,"v":"42","s":0},
   "CD-VC-CHG-MIN":{"t":"dec","pic":"S9(07)V9(02)","u":"COMP-3","o":77,"l":5,"v":"100000.00","s":2},
   "CD-CONN-YYDDD":{"t":"jul","pic":"9(05)","u":"DISPLAY","o":32,"l":5,"v":"24274","iso":"2024-09-30"}
 }}
```

Parquet-compatible: every value is a string, an integer or null, and the
field map is a fixed schema per layout. `canonical.py` emits NDJSON; a
Parquet writer over the same dicts needs no change to the model.

Canonicalise one dataset for inspection:

```bash
python3 canonical.py ../DATA/legacy/USAGE/TELCABS.CABS.USAGE.RAW.G0001V00.dat \
    --layout CABSCDR --key CD-OCN CD-BAN CD-SEQ-NBR \
    --variant-field CD-REC-TYPE \
    --variant-map '{"01":"CD-VOICE-DETAIL","04":"CD-DATA-DETAIL","06":"CD-SPCL-DETAIL"}' \
    --limit 5
```

---

## 3. The five levels

A comparison that only looks at the final invoice tells you *that* the two
sides disagree. On a wholesale access bill the place they started
disagreeing is almost never the last step, so the comparison is staged and
every stage runs at every process boundary.

| Level | Name | Asserts |
|---|---|---|
| **L1** | Record | after canonicalisation, the same records are present, once each, keyed per the contract — no missing, no extra, no duplicate-count mismatch |
| **L2** | Field | every field agrees, typed, with per-field tolerance. Decimal comparison is **exact by default**. The declared **scale** is compared as well as the value, so a five-decimal field silently becoming two-decimal is a finding even when today's values happen to agree |
| **L3** | Control | `CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARISED + CT-CARRIED-FWD` holds on **both** sides for every process, the hash totals chain correctly between processes, and the written volume of the variable-length bill detail — records, declared occurrences, declared bytes and a payload digest — agrees |
| **L4** | Bill | invoice header, per-carrier, per-BAN, per-rate-element and per-line-item money agree, to the penny; and each side's detail lines are separately asserted to own the occurrences they carry and to add up to the totals printed on and above them |
| **L5** | Settlement | per-counterparty per-period settlement agrees, including meet-point splits and PIU/PLU restatements |

### L2: tolerance is a contract decision, never a convenience

Exact is the default. A tolerance exists only where
`contracts/compare_contract.json` declares one, and the contract requires a
`reason` alongside it — `test_canonical.py` fails the build if a tolerance is
declared without one.

This is not pedantry. More than one control in this estate compares two
figures that are carried at different precisions and absorbs the difference
under a tolerance that was widened, years ago, to stop the control failing
every cycle. Those controls have reported clean ever since. A harness that
copies the estate's tolerances copies the estate's blindness — so this one
declares its own, with a reason beside each.

### L3: an out-of-balance legacy is a finding, not an exemption

The balancing equation is asserted on **both** sides independently. If the
legacy run does not balance, that is reported. `CT-BAL-IND = 'O'` being
routine in operations is not a reason to skip the check — it is the reason
to make it.

The chain is declared in the contract as a list of edges:

```json
{"from": "CABING05", "to": "CABING06", "source_field": "CT-WRITTEN", "target_field": "CT-READ",
 "note": "CABING05 writes the clean stream. If the count on either side of this edge is inflated the edge breaks, which is the point of declaring it."}
```

One edge deliberately spans a gap: `CABRAT09 → CABBIL01`. The summary sort
sits between those two and **writes no control record at all**. A divergence
that arises inside that gap has no control record on either side to expose it.
The contract records the gap rather than pretending the chain is continuous.

### L3: counters can agree over a file that is wrong

Alongside the balancing equation the level compares the **written volume** of
the bill detail: how many records, how many occurrences those records declare,
how many bytes those declarations imply, and a digest of the occurrence
payload. Three of those four are the sort of figure a control record could in
principle carry, and the reason for asserting all four is what happens when
only the digest moves — the two sides wrote the same number of records, of the
same declared lengths, and not the same charges. No counter in `CABSCTL` can
express that, which is exactly why the harness has to.

The volume assertion runs only when bill detail exists on both sides. When it
does not, the level's stats say `written_volume_compared: false` rather than
leaving the reader to assume it was checked.

### L4: three assertions each side makes on its own

The aggregate diffs compare legacy against candidate. Three further assertions
are made on each side *independently*, in the same spirit as the meet-point
checks below, because they are properties a bill detail file has to satisfy
whoever produced it:

* **occurrences past the image boundary** — a `CABSBILL` record is 127 fixed
  bytes plus 38 per occurrence, so a fixed image of it *n* bytes wide
  reproduces `(n - 127) // 38` occurrences whole and no more. Where a line
  declares more occurrences than that and the surplus ones repeat a charge
  from the line beside it in line-sequence order, the surplus did not come
  from the line that is carrying it. The same test is run on the intact part
  of the same line and reported separately: if a line repeats its neighbour
  inside the part no image could have damaged, the repetition outside it
  proves nothing.
* **line total against its own occurrences** — the total is in the fixed part
  of the record and is computed early, so where the two disagree the total is
  the side to believe. Lines at or below the boundary are checked identically
  and counted separately, because the finding is only worth anything if the
  disagreement is confined to the long lines.
* **detail against header** — occurrence amounts, line totals and the
  accumulated figure on the account header are all meant to be the same
  number. When they are not, the finding records whether the whole residual is
  carried by the long lines.

`compare.py` derives the boundary rather than hard-coding it, and
`test_canonical.py` asserts the geometry against the copybook so that the
arithmetic cannot rot away from the record it describes.

### L5: both meet-point assertions are needed

* `ST-OUR-PCT + ST-THEIR-PCT = 100.00000`
* `ST-OUR-SHARE + ST-THEIR-SHARE = ST-GROSS-AMT`

The split reconciling is **not** evidence that the percentages are right. A
split can be made to reconcile by moving the residual into one side of it,
after which the second assertion passes on its own however wrong the first
one is. Both are asserted for that reason.

---

## 4. The three-way verdict

| Verdict | Meaning | Scored |
|---|---|---|
| `MATCH` | the two sides agree | — |
| `DIVERGENT-BY-DESIGN` | the variance traces to a known seeded defect in the legacy | **positively** — the transform found something real |
| `DIVERGENT` | a variance with nothing behind it | **blocks** |

A two-way pass/fail is the wrong instrument. It has one way to describe a
difference, so it forces every real finding into the same bucket as every
bug, and the standard response is to widen a tolerance until the test goes
quiet.

`DIVERGENT-BY-DESIGN` is not a pass. It means the candidate has surfaced a
defect the legacy has been carrying, and the correct response is a **business
decision about the defect** — revenue assurance, regulatory, restatement —
not a code change to silence the harness. Several of the seeded defects
cannot be "fixed" during a migration without changing what carriers are
billed, which is a rate change in substance.

### Attribution is a separate, auditable input

`verdict.py` is the only module that touches `../SEALED/answer_key_*.json`.
Attribution runs on the variance list *after* the comparison, never inside
it. The rules live in `defect_signatures.json` as JSON predicates, so they
can be reviewed by someone who does not read Python:

```json
{"defect": "Dnn",
 "match": {"levels": ["L2"], "kinds": ["decimal_value"], "layouts": ["CABSCDR"],
           "fields": ["<field>"], "legacy": {"in_set": "$<parameter>"}},
 "evidence": "<why this shape of variance points at this defect>"}
```

The shapes above are illustrative. The real rules, with the real defect ids,
fields and evidence, are in `defect_signatures.json`, which is withheld from a
blind run for the same reason the answer key is.

Supported operators: `eq`, `ne`, `in`, `not_in`, `in_set` (decimal-valued),
`present`, `regex`, `lt`/`le`/`gt`/`ge`, `abs_ge`, `abs_le`. Values beginning
`$` are substituted from the contract's `parameters` block
(`$cycle_start`, `$band_boundaries`, `$probe_keys`).

Rules can only match on what the comparison recorded. That is why the
contract declares **witness fields** per dataset — an L1 finding that says
only "this key is missing" cannot be attributed to anything, so the
discriminating field values (`CD-EDIT-STATUS`, `CD-VC-TANDEM-IND`,
`CD-CONN-YYDDD`, …) travel with the variance.

Every signature names a defect that must exist in the answer key, and every
answer-key defect must have at least one signature. Both are asserted by
`test_canonical.py`.

---

## 5. Running a blind test

The point of `--blind` is to demonstrate that the comparison engine has not
been tuned to the answer.

```bash
# 1. blind. The answer key and the signature file are both refused.
python3 run_compare.py --legacy L --candidate C --blind --out ../DATA/blind
#    -> every variance is DIVERGENT, no attribution, no defect list

# 2. attributed. Same inputs, same comparison, answer key applied afterwards.
python3 run_compare.py --legacy L --candidate C --out ../DATA/attributed

# 3. the difference between the two reports is the score.
diff <(jq -r '.variances[].variance | [.level,.kind,.key,.field] | @tsv' ../DATA/blind/comparison_report.json) \
     <(jq -r '.variances[].variance | [.level,.kind,.key,.field] | @tsv' ../DATA/attributed/comparison_report.json)
#    -> no output: the variance list is identical. Only the verdicts changed.
```

**Both** the answer key and `defect_signatures.json` are withheld. The
signature file names the seeded defects and describes their fingerprints;
handing it to a blind run would leak the answer as surely as the key.
`verdict.py` raises `BlindRunError` on either.

A blind run always exits `1` if there is any variance at all, because
without the key nothing can be classified as by-design. That is correct
behaviour, not a bug: a blind run is a measurement, not a gate.

---

## 6. The contract file

`contracts/compare_contract.json`. It is an **input** to the comparison and
contains no answers.

Per dataset:

| Key | Meaning |
|---|---|
| `pattern` | filename glob matched on both sides |
| `layout` | copybook member; the schema comes from `../COPYBOOKS` |
| `recfm` / `lrecl` | `FB`, or `VB` with a 4-byte RDW |
| `levels` | which levels this dataset participates in |
| `key` | the business key. Record position is never a key |
| `variant_rule` | which field selects the REDEFINES overlay, and the mapping |
| `ignore_fields` | filler and the variant-area group item, which the overlay already covers |
| `witness_fields` | carried into variance context so findings can be attributed |
| `tolerances` | per field, `{"abs": "...", "reason": "..."}`. Absent means exact |

Plus `l3.process_chain`, `l4` money-field lists and groupings, `l5` money
fields and assertions, and a `parameters` block the signature substitutions
draw on.

The `USAGE.RAW` variant rule carries this note, and it is the honest one:

> `CD-REC-TYPE` `'03'` satisfies both `CD-VOICE-MOU` and `CD-DATA-SVC`, and
> `'05'` satisfies both `CD-DATA-SVC` and `CD-SPECIAL-ACC`. The 88-levels
> overlap in the frozen copybook. This mapping resolves the ambiguity the
> same way for both sides, which is the only way the comparison can be
> meaningful; that the ambiguity exists at all is a finding for the
> modernization, recorded here rather than hidden.

---

## 7. Output

`comparison_report.json` — the machine-readable record: run metadata, overall
verdict, score, money totals, per-level results and stats, variance groups,
and every variance with its attribution.

`comparison_summary.txt` — the readable version:

```
LVL  NAME        VERDICT                   RECORDS     FIELDS VARIANCE
L1   Record      DIVERGENT                   8,373          0      193
L2   Field       DIVERGENT                   8,180    202,679      173
L3   Control     MATCH                           1          9        0
L4   Bill        NOT RUN                         0          0        0
     not run: no bill header/detail datasets found on both sides; L4 runs
     after the billing steps have been executed
L5   Settlement  DIVERGENT-BY-DESIGN           372          0       44
```

Three things the summary is deliberately blunt about:

1. **Penny-level totals, signed and net** — not just counts. Ten thousand
   variances netting to zero and ten thousand netting to −$400 are entirely
   different situations. Money totals cover **scaled** fields only;
   differences on unscaled identifiers (NPA-NXX, LATA, CIC, HHMMSS) are
   counted separately, because a difference of 97,270 on `CD-VC-ORIG-NPANXX`
   is a different NPA-NXX, not ninety-seven thousand dollars.
2. **Which seeded defects were detected and which were missed** — a missed
   defect is a statement about the harness, not about the candidate. Missed
   defects are listed with their `detectable_by` classification, so
   "`Dnn … (parity_test_at_volume)`" reads as "this needs the STRESS
   profile", not as "this is fine".
3. **What did not run.** A level skipped because its data did not exist is
   not a level that passed, and the summary says `NOT RUN`, never `MATCH`.

---

## 8. Scale

The engine indexes canonical NDJSON by key to a **byte offset** and seeks,
rather than holding records in memory. A 500,000-record generation costs
roughly 50 MB of index.

At `TARGET` volume (100,000,000 records) a complete L2 index is about 10 GB.
Build it over a sampled key range instead — and note that a sampled L2 is a
different claim from a complete one and must never be presented as the same
thing. L1, L3, L4 and L5 remain complete at any volume, because L1 holds only
keys and the other three work on aggregates.

**Two of the twelve require volume, and they require different volumes.**
Part of the sort-exit layer only behaves differently once the merge spills
across more than one `SORTWK`; generate at `STRESS` or larger for it. The
other needs a bill detail line carrying more rate elements than an ordinary
account produces, which is a property of the shape of the data rather than of
how much of it there is — `GENERATORS/gen_divergence.py` has a mode that
places the shape deliberately, and it has to be applied to both sides. Without
the right input the harness will report either defect as missed and be right
to.

The missed-defect list carries a `detectable_by` classification for this
reason. `parity_test_at_volume` reads as *this needs the STRESS profile*;
`parity_test_at_element_volume` reads as *this needs the shape, not the
size*.

---

## 9. What the harness cannot tell you yet

The comparison runs on whatever datasets exist on both sides. Before the
COBOL estate has been executed under Hercules, only the generated inputs
exist, so:

* **L1 and L2** run fully on the usage and settlement datasets.
* **L3** runs against the generation control records the generator writes
  (process ID `GENUSAGE`), which anchors the chain but exercises only its
  first edge.
* **L4** does not run — there are no `CABSBHDR` or `CABSBILL` datasets until
  the BILLCALC family has been executed.
* **L5** runs on the counterparty meet-point view only; the
  reciprocal-compensation and ISP-cap comparisons need settlement output that
  does not exist until the SETTLE family has been executed.

That is why `run_compare.py` reports `NOT RUN` with a reason rather than
`MATCH`, and why the seeded-defect score names what was missed. Seven of the
twelve seeded defects are only observable after the batch estate has run.

---

## 10. Tests

```bash
python3 -m unittest test_canonical -v
```

39 tests, in five groups:

* **Canonical decoding** — schema and key formation; EBCDIC to UTF-8;
  trailing spaces preserved; COMP-3 to a decimal string with the scale
  preserved (`0.00` stays `0.00`, `33.33000` keeps five places); no float
  anywhere in the form; every field carrying its PIC, USAGE, offset and
  length; YYDDD raw plus ISO, with `iso: null` for `25366`; exactly one
  variant decoded and the other two absent; corrupt packed data becoming a
  finding rather than an exception.
* **An independent cross-check** — `_reference_comp3` decodes nibble by
  nibble with integer shifts and no string handling, and must agree with the
  canonicaliser on every tested value.
* **NDJSON round-trip and keyed indexing**, including duplicate keys.
* **L1/L2/L3 behaviour** — identical files match; missing, extra and
  duplicate records are each detected; witness fields are carried; decimal
  comparison is exact by default; a declared tolerance applies at its
  boundary and not beyond it; a Julian difference is reported as a date; the
  balancing equation failure is reported on both sides; a chain break is
  detected.
* **Verdict and contract integrity** — `--blind` raises on both the answer
  key and the signatures; blind classifies everything DIVERGENT; a boundary
  value attributes regardless of its declared scale; an unattributable
  variance stays DIVERGENT; one DIVERGENT dominates the overall verdict;
  every signature names a defect in the answer key and every answer-key
  defect has a signature; every declared tolerance carries a reason.
