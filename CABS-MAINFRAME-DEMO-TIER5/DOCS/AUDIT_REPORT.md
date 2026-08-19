# CABS Tier 5 — Independent Conformance Audit

**Auditor:** independent agent, no involvement in the build
**Date:** 2026-08-15
**Artefact:** `CABS-MAINFRAME-DEMO-TIER5`
**Method:** every figure below was produced by running greps, counts and purpose-written Python
against the source tree. No manifest, README or prior agent report was accepted as evidence.
Where a manifest and the source disagree, the source is reported.

---

## 1. Verdict table

| # | Spec item | Verdict | Evidence |
|---|---|---|---|
| A1 | Estate size 350–500 files | **FAIL** | 527 mainframe source files (`.cbl` 248, `.jcl` 167, `.ctl` 35, `.cpy` 23, `.prc` 18, `.exec` 6, `.bms` 6, `.dbd` 6, `.psb` 6, `.asm` 5, `.pli` 4, `.ddl` 2, `.sql` 1). 5.4% over the ceiling. 638 files in the tree overall. |
| A2 | Estate size 120,000–180,000 LOC | **FAIL** | 194,920 mainframe LOC counted directly. 8.3% over the ceiling. The README's own (understated) figure of 184,067 is already over. |
| A3 | COBOL LOC distribution matches real-estate calibration | **FAIL** | Actual vs required: 0–200 **0.8%** (req 8%), 200–500 **40.7%** (43%), 500–1000 **43.5%** (29%), 1000–2000 **11.7%** (10%), 2000–4000 **2.4%** (8%), 4000+ **0.8%** (1%). The curve is collapsed into the middle: almost no small programs, too few large ones. |
| B1 | No `EVALUATE` in runnable core | **FAIL** | 2 `EVALUATE` + 2 `END-EVALUATE`: `BATCH/JURIS/CABJUR10.cbl:565,579`; `BATCH/SETTLE/CABSET13.cbl:551,562`. |
| B2 | No scope terminators in runnable core | **FAIL** | 26 `END-IF` and 3 `END-READ` in 3 files: `CABJUR10.cbl` (6+1), `CABSET13.cbl` (8+1), `CABSET12.cbl` (12+1). |
| B3 | No `INITIALIZE`, no inline `PERFORM`, no `END-PERFORM` | **PASS** | 0 occurrences across all 207 runnable-core programs. |
| B4 | No reference modification `(pos:len)` | **PASS** | 0 occurrences in the runnable core (27 in the exempt Enterprise-COBOL layers, permitted). |
| B5 | Nothing past column 72 | **PARTIAL** | 0 **code** lines exceed 72 — clean. But 318 **comment** lines run to 73–78, and `BATCH/REPORT/CABRPT01.cbl:15` runs to 82. Harmless to the compiler (cols 73–80 are the sequence area) but a literal breach of the convention. |
| B6 | `P0000-MAINLINE`, `P8000-CONTROL`, `CTLOUT`, `COPY CABSWRK` in every batch program | **PASS** | All 207 runnable-core programs carry all four, except the 12 subprograms in `BATCH/COMMON/` — which `CONVENTIONS.md` documents as the exception. `CTLOUT` present in 207/207. |
| **C** | **No comment names its own construct or admits a defect** | **FAIL (critical)** | **514 comment lines matching `COMPLEXITY nn` across 117 files.** Plus `DEAD CODE` ×5, `DEFECT` ×6, `DORMANT FEATURE` ×9, `SEEDED DEFECT D8` ×1, `SEE THE SEALED ANSWER KEY` ×2, `NEVER FIRES` ×2, `DELIBERATE/DELIBERATELY` ×23. Detail in §2.1. |
| D | All 27 constructs present and traceable | **PASS** | `CONTRACTS/complexity_placement.json` records all 27, 1,613 placements. Every one of the 1,613 named paragraphs **exists in its named file** (0 missing, verified programmatically). Independently confirmed in source by construct-specific grep, including the hard ones: 5×`66 RENAMES`, 4× internal `SORT`, 72× `OCCURS DEPENDING ON`, 6× nested `COPY` inside a copybook, 12× dead code (spot-checked 5, all genuinely 0-reference), `CABING12.cbl` genuinely orphaned (0 JCL references, 0 CALLs). Minor: 24 placement `file` values do not resolve as paths (21 are PROC names written without `.prc`/`.jcl`, 3 are comma-lists/globs). |
| E1 | `CABSCDR` = 200 | **PASS** | Computed 200. |
| E2 | `CABSCTL` = 180 | **PASS** | Computed 180. |
| E3 | `CABSBHDR` = 400 | **PASS** | Computed 400. |
| E4 | `CABSPRNT` = 133 incl. carriage control | **PASS** | Computed 133 (1 + 132). |
| E5 | `CABSBILL` VB max 1204 | **FAIL** | Computed maximum **1,647** bytes (127 fixed + 40 × 38 elements). Declared LRECL is 1204 in 11 JCL/PROC members and in `CONTRACTS/contracts.json`. `CABRAT03.cbl:2515` explicitly permits `BD-ELEM-CNT` up to 40. Any record above 28 elements exceeds the DCB. |
| E6 | No REDEFINES longer than its target | **FAIL** | Copybooks are clean (8 REDEFINES, all ≤ target). But `BATCH/INGEST/CABING08.cbl:93` — `CF-VARIANT-VOICE` (`S9(07)V9(02) COMP-3` = 5 + `X(92)` = **97**) REDEFINES `CF-VARIANT-AREA` `X(96)`. **This is a hard compile error.** |
| F | 5–8 seeded defects, documented in `SEALED/` | **PASS** | 11 defects across 4 answer-key files. All 11 files and paragraphs exist. All 11 verified genuinely implemented in code (§2.4). Over the band, but explicitly documented — acceptable per spec. |
| G | Answer key contained | **FAIL (critical)** | `HUB/index.html` publishes, for every defect D1–D11, the id, file **and the answer key's own one-line `construct` description**. `README.md:525` explains D6 in full. `BATCH/*/_MANIFEST.md` label programs "**DEFECT D2**". `CONTRACTS/complexity_placement.json` lists every defect's file and paragraph. `HARNESS/defect_signatures.json` contains two verbatim fragments of `description`. Detail in §2.2. |
| H | No Java written | **PASS** | 0 `.java`, 0 `.jar`, no `pom.xml`, no `build.gradle`, no Spring artefacts, no `import java.` anywhere. |
| I | All 11 deliverables present and non-trivial | **PASS** | §3. Both `.xlsx` and both `.docx` open and contain real content. |
| J | Toolchain works | **PARTIAL** | `HARNESS/test_canonical.py` 39/39 **OK**. `GENERATORS/test_gen_common.py` **56/58 — 2 FAILURES** (§2.6). SMOKE generator runs clean: 62,711 records, 9 files, 11.2 MB, 8.0 s. Full L1–L5 harness runs end to end. |
| K | Hub integrity | **PASS** | `HUB/index.html` (1.72 MB) parses, 202 tags; embedded `DATA` object is valid JSON (1.64 MB); `HUB/build_hub.py` regenerates it **byte-identical**. D3 graph present, 710 nodes / 3,141 edges. |
| L | Manifests, register, matrix and hub agree | **FAIL** | README claims 530 files / 184,067 mainframe LOC. Actual: 527 / 194,920. Hub claims 618 files / 743,947 lines. README claims COBOL = 217 files / 168,062 lines; actual 248 / 178,464. README claims SORTEXIT = 10 files, 8 exits, 2,253 ln; actual 29 files, 27 programs, 8,159 ln. README claims JCL = 176 files; actual 194. Detail in §2.5. |

**Counts: 12 PASS · 3 PARTIAL · 9 FAIL.**

---

## 2. Findings, ranked by severity

### 2.1 CRITICAL — the anti-giveaway rule is comprehensively broken (514+ hits, 117 files)

This is the single most damaging finding. The estate cannot be used as a blind test in its current
state. Any reader — human or LLM — is told, in the comment immediately above the code, exactly
which construct they are looking at.

Counted on comment lines only:

| Banned term | Hits | Files |
|---|---:|---:|
| `COMPLEXITY nn` | **514** | 117 |
| `VARIANT` | 26 | — (legitimate domain usage, see note) |
| `DELIBERATE` / `DELIBERATELY` | 23 | 20 |
| `DORMANT` | 9 | 6 |
| `DEFECT` | 6 | 6 |
| `DEAD CODE` | 5 | 3 |
| `ANSWER KEY` | 2 | 2 |
| `NEVER FIRES` | 2 | 2 |
| `INTENTIONAL` | 2 | 2 |
| `SEEDED` | 1 | 1 |

Worst individual lines:

```
JCL/CABRAT6R.jcl:6      //* SEEDED DEFECT D8 (P3200-SELECT-BAND) CAN MANIFEST HERE - IF   *
JCL/CABRAT01.jcl:137    //* STEP080 - CABRAT06 BANDED RATES.  DEFECT D8 LIVES IN
JCL/CABRAT01.jcl:138    //* P3200-SELECT-BAND - SEE THE SEALED ANSWER KEY, NOT THIS JCL.
JCL/CABRAT01.jcl:110    //* THE AUTO-REPRICE LOOP (P6100) IS DEAD CODE - ITS ENABLING
JCL/CABRAT5R.jcl:6      //* ELEMENT RATING).  THE AUTO-REPRICE LOOP (P6100) NEVER FIRES   *
BATCH/JURIS/CABJUR02.cbl:749   * THIS PARAGRAPH.  COMPLEXITY 26 - DEAD CODE.
BATCH/SETTLE/CABSET09.cbl:868  * CONTROL CARD NO LONGER SUPPLIES.  COMPLEXITY 26 - DEAD CODE.
BATCH/JURIS/CABJUR06.cbl:715   * COMPLEXITY 27 - DORMANT FEATURE.
BATCH/SETTLE/CABSET06.cbl:540  * COMPLEXITY 27 - DORMANT FEATURE.
JCL/CABJ1500.jcl:10     //* PASSED Y SINCE JUNE 2011.  COMPLEXITY 27 - DORMANT FEATURE.
BATCH/RATING/CABRAT04.cbl:603  * ROUNDED - THE DELIBERATE INCONSISTENCY WITH P2500 ABOVE.
BATCH/RATING/CABRAT05.cbl:489  * ROUNDED - DELIBERATE INCONSISTENCY WITH P2600 ABOVE.
BATCH/JURIS/CABJUR04.cbl:795   * DIVERGENCE IS DELIBERATE AND IS RECORDED IN THE PLACEMENT
```

The `COMPLEXITY nn` hits break down as 64 JCL/PROC members and 53 COBOL programs, worst offenders
`BATCH/BILLCALC/CABBIL02.cbl` (15), `BATCH/JURIS/CABJUR07.cbl` (12), `BATCH/SETTLE/CABSET09.cbl`,
`CABSET01.cbl`, `CABSET07.cbl` (11 each).

**Note on false positives.** `VARIANT` is used throughout in its legitimate business sense
(`CD-VARIANT-AREA` is a real record-layout name, and the three usage variants are the data
architecture). Those 26 hits are *not* giveaways and should not be scrubbed. `DELIBERATE` is not on
the banned list literally, but 23 comments use it to announce that a behaviour was engineered
rather than inherited — same effect, and they should go.

**Remediation:** strip every `COMPLEXITY nn` token, every `DEAD CODE` / `DORMANT FEATURE` /
`DEFECT` / `SEEDED` / `ANSWER KEY` / `NEVER FIRES` reference, and reword the `DELIBERATE` comments
into ordinary maintenance prose. The construct-to-file mapping already lives in
`CONTRACTS/complexity_placement.json` — the source comments do not need to duplicate it.

### 2.2 CRITICAL — answer-key containment has failed in five separate places

The strict test in the brief (do `description`, `business_impact` or `correct_modernization_response`
strings appear outside `SEALED/`) yields **two literal leaks**:

```
HARNESS/defect_signatures.json  ← D3 description fragment:
   "rogram already contains a correct absolute-day conversion (P…"
HARNESS/defect_signatures.json  ← D2 description fragment:
   "the only element that multiplies by a third operand (the tan…"
```

That understates the problem. The containment intent has failed far more broadly:

1. **`HUB/index.html`** — `build_hub.py` reads `SEALED/` "under field restriction
   `('id','file','construct','detectable_by')`" and reports "0 leaked fields". But the `construct`
   field *is* the defect statement. The hub therefore publishes, for all 11:
   > `{"id":"D4","file":"BATCH/SETTLE/CABSET01.cbl","construct":"Silent absorption of a meet-point percentage residual into our own share instead of raising an exception",...}`

   Anyone opening the hub has the complete answer sheet. The field-restriction logic in
   `build_hub.py` is the root cause and needs `construct` removed.
2. **`CONTRACTS/complexity_placement.json`** (unsealed) carries `seeded_defects[]` with `id`, `file`
   and `paragraph` for all 11 — and its own note says the descriptions "must not be copied here",
   which shows the author understood the risk but stopped one field short.
3. **`README.md:525`** gives D6 away completely: *"Seeded defect D6 is a five-cent tolerance
   introduced into `CABBIL11` in 2001 to stop the account-level proof failing every cycle… The
   tolerance is one-sided as well as too wide."* Lines 66, 71 and 93 name which programs carry D3,
   D4, D5 and D7.
4. **`BATCH/*/_MANIFEST.md`** — e.g. `BATCH/RATING/_MANIFEST.md:32`: `` `CABRAT03.cbl` | **DEFECT D2** ``.
   Seven manifests do this.
5. **JCL comments** (see §2.1) name D8's file and paragraph.

A blind run is impossible until `SEALED/` is the *only* place any of this appears.

### 2.3 HIGH — 506 hard compile errors caused by column-72 truncation

A late formatting pass appears to have trimmed source to column 72 without checking what was cut.
The result is two classes of unconditional compile failure.

**(a) 501 data-description entries lost their terminating period.** 497 of them sit at exactly 72
characters. 24 programs affected; worst are `BATCH/SETTLE/CABSET05.cbl` (210),
`BATCH/JURIS/CABJUR07.cbl` (194), `BATCH/SETTLE/CABSET07.cbl` (11), then `CABJUR01/02/08`,
`CABSET09/12` (10 each).

```
BATCH/SETTLE/CABSET02.cbl:159   10  WS-CYCLE-YY   PIC 9(02)  VALUE 0     ← 72 chars, no period
BATCH/SETTLE/CABSET02.cbl:160   10  WS-CYCLE-DDD  PIC 9(03)  VALUE 0     ← 72 chars, no period
BATCH/SETTLE/CABSET02.cbl:161   05  WS-BILL-PERIOD PIC 9(06) VALUE 0.    ← 69 chars, fine
```

**(b) 5 unterminated alphanumeric literals** — the closing quote *and* the period were both cut:

```
BATCH/JURIS/CABJUR07.cbl:797  VALUE 'OCN  ST J USE-DT   BASE-MOU     PRIOR-PIU  NEW   (72, no close quote)
BATCH/SETTLE/CABSET05.cbl:643 VALUE 'CAP           BILL-MOU        RATE     NET-DUE   (72, no close quote)
BATCH/JURIS/CABJUR02.cbl:288  VALUE 'OCN  ST LATA EFF-DT   PIU        PLU        SR
BATCH/SETTLE/CABSET01.cbl:907 VALUE '…CT-ID           OCN  OTHER OUR-PCT OTH-PC
BATCH/SETTLE/CABSET02.cbl:326 VALUE '…CT-ID           TRK-GRP  OCN  OTHER OUR-P
```

Neither class is a seeded defect — neither is in any answer key, and both are compiler-fatal
rather than behaviour-changing. They are build damage. **The estate as shipped will not compile.**

**Remediation:** shorten the field-name/PIC column spacing on the affected lines so the terminating
period (and closing quote) fit inside column 72, then re-run a "does every data entry end in a
period" check as a build gate.

### 2.4 HIGH — one REDEFINES is longer than the item it redefines

```
BATCH/INGEST/CABING08.cbl:92   05  CF-VARIANT-AREA             PIC X(96).
BATCH/INGEST/CABING08.cbl:93   05  CF-VARIANT-VOICE REDEFINES CF-VARIANT-AREA.
BATCH/INGEST/CABING08.cbl:94       10  CF-VC-CHG-MIN   PIC S9(07)V9(02) COMP-3.   ← 5 bytes
BATCH/INGEST/CABING08.cbl:95       10  CF-VC-FILLER    PIC X(92).                 ← 92 bytes
                                                                       total = 97 > 96
```

`S9(07)V9(02)` is 9 digits, packed = `(9÷2)+1` = 5 bytes. `CF-VC-FILLER` should be `X(91)`. The
sibling `CF-VARIANT-DATA` at line 96 is correct (8 + 88 = 96), which confirms the intent.
This is the exact failure class the brief flagged as "previously wrong". The **copybooks are now
clean** — all 8 copybook REDEFINES are within their targets — but the program-level one was missed.

### 2.5 MEDIUM — `CABSBILL` cannot fit its declared DCB

`CABS-BILL-DETAIL` computes to a maximum of **1,647** bytes:

| Part | Bytes |
|---|---:|
| `BD-KEY` (13+6+2+4) | 25 |
| `BD-OCN`/`JURIS`/`STATE`/`DESCRIPTION` | 67 |
| `BD-TOTALS` (8+10+8+6) | 32 |
| `BD-ELEM-CNT` | 3 |
| `BD-ELEMENT` × 40 @ 38 bytes | 1,520 |
| **Total** | **1,647** |

Every DD that carries it declares `RECFM=VB,LRECL=1204` (11 members incl. `CABRAT01.jcl:93,215`,
`JCL/PROCS/CABPBDTL.prc:29`, `CABS4150.jcl:35`), i.e. 1,200 data bytes — enough for 28 elements,
not 40. `CABRAT03.cbl:2515` explicitly builds up to 40. At volume this is an SB37/record-length
failure.

To the build's credit, `GENERATORS/generate.py` **reports** this at run time:
> `CABSBILL declared LRECL 1204 against a maximum computed length of 1647; the record is variable
> (OCCURS DEPENDING ON BD-ELEM-CNT) and is written at its natural length`

So it is a known, surfaced inconsistency rather than a silent one. But it is not recorded in any
answer key and not reconciled in the contract register, so a candidate transformation has no way to
tell whether it is a deliberate trap or a build error. **Decide and document which it is.**

Related, lower impact: `CABSSETL` computes to **186** bytes while all 8 DDs that carry it
(`SETLIN`, `SETLOUT`, `NETOUT`, `RECAGG`, `CMDSOUT`, `SETLADD`, `WIRESET`, plus the header comments)
declare `LRECL=180`. Because every FD in the estate uses a generic `PIC X(nnn)` record and the
copybook lives in WORKING-STORAGE, this truncates only the tail of `ST-FILLER` (all real fields end
at byte 146) — harmless in practice, but the copybook and the physical record disagree.

### 2.6 MEDIUM — three DB2 programs sit inside the runnable core

`BATCH/JURIS/CABJUR10.cbl`, `BATCH/SETTLE/CABSET12.cbl` and `BATCH/SETTLE/CABSET13.cbl` all contain
`EXEC SQL` (5, 4 and 4 statements) and are written in Enterprise COBOL — `EVALUATE`, `END-IF`,
`END-READ`. `CONVENTIONS.md` grants the Enterprise-COBOL exemption **by folder**
(`BATCH/CONTROL/`, `ONLINE/`, `DB2/`), and these three are in neither. They also cannot run on
TK4-/MVS 3.8j. Either move them under `BATCH/CONTROL/` (or `DB2/`) and mark them reference-only, or
widen the exemption in `CONVENTIONS.md`. Right now the convention says one thing and the tree does
another. No other program in the runnable core has a single scope terminator — the discipline
elsewhere is genuinely good.

### 2.7 MEDIUM — README is materially wrong about the size of the estate

| Measure | README | Hub | Actual (audited) |
|---|---:|---:|---:|
| Mainframe source files | 479 (subtotal) / 530 (total) | 618 (whole tree) | **527** |
| Mainframe LOC | 184,067 | 743,947 (whole tree incl. data) | **194,920** |
| COBOL `.cbl` files | 217 | 248 | **248** |
| COBOL LOC | 168,062 | 178,464 | **178,464** |
| SORTEXIT | 10 files, 8 exits, 2,253 ln | 29 files, 8,432 ln | **29 files, 27 programs, 8,159 ln** |
| JCL folder | 176 files | 194 | **194** |
| Sort control cards `.ctl` | 18 files, 273 ln | — | **35 files, 724 ln** |
| HUB/ | "0 — reserved" | 2 files, 1.85 MB | **2 files** |

The 48-file / 10,853-line gap reconciles exactly: SORTEXIT gained 19 COBOL members after the README
was written, and `JCL/CTLCARDS/MVT/` (17 extra control cards) is not in the README tree at all. The
hub is correct; the README was never regenerated. This matters because the README's headline
"530 source files, ~184,000 lines" is the number a reader will quote — and even *that* figure
breaches the 180K ceiling.

### 2.8 LOW — two stale unit tests fail

```
FAIL: test_declared_lrecl_wins_over_computed_length (TestCopybookParser)
      GENERATORS/test_gen_common.py:367
      self.assertEqual(cdr.computed_length, 188)
      AssertionError: 200 != 188

FAIL: test_variant_length_disagreements_are_reported_not_repaired (TestCopybookParser)
      GENERATORS/test_gen_common.py:361
      AssertionError: 'CD-VOICE-DETAIL REDEFINES CD-VARIANT-AREA' not found in ''
```

Both are assertions that `CABSCDR` is *broken* — 188 bytes with mismatched variants. The copybook
was subsequently repaired (it now computes to exactly 200, all three variants exactly 96) and the
tests were not updated. This is evidence the earlier copybook defect was genuinely fixed, but a
green test suite is a delivery gate and this one is red: **56/58**.

`HARNESS/test_canonical.py` is **39/39 OK**, including the blind-mode guards
(`test_blind_refuses_the_answer_key`, `test_blind_refuses_the_signatures_too`).

### 2.9 LOW — the parser inside `gen_common.py` mis-sizes edited PICs

`generate.py` reports `PC-BODY-A REDEFINES PC-BODY but is 118 bytes against 132`. The correct value
is **131** (70 + 16 + 7 + 18 + 20). The direction is right (shorter than its target, therefore
legal) so nothing breaks, but the copybook-diagnostic figures it prints cannot be trusted to the
byte. Numeric-edited PIC characters (`Z , . -`) are being under-counted.

### 2.10 LOW — build scratch shipped in the deliverable

`DATA/_scratch_delete_me/` contains 6 zero-byte `.ndjson` files under
`canonical/legacy/` and `canonical/candidate/`. A directory literally named "delete me" should not
be in a client artefact. `DATA/` is also 70 MB of generated output committed alongside the source,
which inflates the hub's 618-file / 743,947-line estate figure and confuses every count in §2.7.

### 2.11 INFORMATIONAL — the COBOL size curve does not look like a real estate

The required calibration was 8 / 43 / 29 / 10 / 8 / 1 percent. Actual: 0.8 / 40.7 / 43.5 / 11.7 /
2.4 / 0.8. Only 2 of 248 programs are under 200 lines, and only 6 are in the 2,000–4,000 band.
Real estates have a long tail of tiny copy-and-tweak modules and a handful of 3,000-line monsters;
this one has neither. The 95-program generated `BATCH/UTIL` tier is clustered at 500–1,000 lines
and is what pulls the distribution off. A modernization-effort estimator calibrated on this shape
would mis-price a real estate.

---

## 3. Deliverables check (spec item I)

| # | Deliverable | Present | Substance |
|---|---|---|---|
| 1 | The estate | ✔ | 527 mainframe source files, 194,920 LOC, 11 technologies |
| 2 | Python generators, scale-parameterised | ✔ | `GENERATORS/` 8 files. `--profile {SMOKE,DAILY,STRESS,TARGET}` = 50K / 500K / 2M / 100M CDRs/day, matching spec. Deterministic (`seed`), worker-independent sharding. SMOKE run: 62,711 records / 11.2 MB / 8.0 s |
| 3 | Process Contract Register | ✔ | `DOCS/Process_Contract_Register.xlsx` — 5 sheets: Read me (25 rows), Register (**247 processes** × 34 cols), Datasets (205 × 16), Balancing Rules (247 × 10), Undetermined Items (1,964 × 5). Genuinely authored from the legacy side; the "UNDETERMINED" honesty rule is real and used 1,964 times |
| 4 | Canonical interchange specification | ✔ | `CONTRACTS/canonical_interchange_spec.md`, 20.7 KB |
| 5 | L1–L5 comparison harness, three-way verdict | ✔ | Ran end to end. L1 DIVERGENT, L2 DIVERGENT, L3 MATCH, L4 NOT RUN (honestly reported: no bill datasets exist because nothing has been executed on Hercules), L5 DIVERGENT-BY-DESIGN. Verdict counts: 2,433,794 MATCH / 2,027 DIVERGENT-BY-DESIGN / 2,524 DIVERGENT |
| 6 | Golden baseline + sealed key as separate signed input | ✔ | `DATA/legacy` (42 MB) vs `DATA/candidate` (22 MB). All 5 signed inputs SHA-256'd in the report. `--blind` mode implemented and unit-tested. Self-reported detection rate **5/11** (D1, D2, D3, D4, D8 found; D5, D6, D7, D9, D10, D11 missed) — reported honestly rather than hidden |
| 7 | 27-complexity traceability matrix | ✔ | `DOCS/Complexity_Traceability_Matrix.xlsx` — Read me (19), Summary (31 × 10), Placements (**1,613** × 7), Seeded Defects (13 × 4) |
| 8 | Hercules run book | ✔ | `DOCS/HERCULES_RUNBOOK.md`, 97 KB |
| 9 | CAST Imaging guide + expected findings | ✔ | `DOCS/CAST_IMAGING_GUIDE.md`, 73 KB, 14 expected-finding rows, 10 blind-spot sections |
| 10 | Interactive knowledge graph (D3, dark) | ✔ | `HUB/index.html` 1.72 MB, D3 present, 710 nodes / 3,141 edges, regenerates byte-identical |
| 11 | Target-state analysis ×2 | ✔ | `Target_State_Language_Recommendation.docx` (109 paras, 10 tables, ~3,588 words) and `CAST_vs_AWS_Transform_Assessment.docx` (197 paras, 8 tables, ~6,338 words). Both open cleanly in python-docx and contain real analytical prose, not placeholders |

All eleven exist and none is a stub. **PASS.**

---

## 4. What I could not verify

- **That the estate compiles.** No OS/VS COBOL or Enterprise COBOL compiler is available in this
  environment. §2.3 and §2.4 identify 506 constructs that a compiler *will* reject on inspection of
  the language rules, but I cannot produce compiler output. Conversely I cannot certify that the
  other 242 programs compile clean — there may be further errors I did not pattern-match.
- **That anything runs under Hercules/TK4-.** No emulator, no MVS 3.8j image, no load libraries.
  The run book was read, not executed. L4 of the harness is `NOT RUN` for exactly this reason, so
  the bill-level and settlement-level parity behaviour of the seeded defects is unproven end to end.
- **Runtime behaviour of the seeded defects.** I verified all 11 are *present in the code* and that
  the described mechanism is genuinely what the code does. I could not confirm the *magnitude* of
  the divergence each produces, because that needs the legacy programs to execute. The harness's
  own 5/11 detection rate is self-reported.
- **CAST Imaging findings.** The expected-findings table in `DOCS/CAST_IMAGING_GUIDE.md` is a claim
  about what CAST will detect. No CAST instance was available. Unverified.
- **IMS/DB2/CICS/MQ correctness.** These are reference-only by design and were checked for presence
  and structure only — DBD/PSB syntax, DDL validity and BMS mapset correctness were not validated
  against their respective generators.
- **The 1,613 placements semantically.** I verified every placement's file and paragraph exists,
  and I independently confirmed at least one placement per construct plus 5 dead-code claims by
  reading the code. I did not read all 1,613.
- **Whether the `CABSBILL` 1204/1647 clash is intentional.** It is surfaced by the generator but
  absent from every answer key. Only the build team can say which it is.
- **`.docx` visual fidelity.** Content and structure were read programmatically; I did not render
  them to confirm they look right.

---

## 5. Overall verdict

**FAIL — do not release as a blind test asset. Two blocking defect classes, both fixable.**

The engineering underneath is strong and, in places, unusually honest. The process contract
register with 1,964 explicit `UNDETERMINED` entries, the harness that refuses the answer key in
blind mode and reports its own 5-of-11 detection rate, the byte-identical hub regeneration, the
1,613 placements every one of which resolves to a real paragraph in a real file, and the fact that
`CONVENTIONS.md` discipline holds across 204 of 207 runnable-core programs with zero
`INITIALIZE`, zero inline `PERFORM`, zero reference modification and zero over-length code lines —
that is genuine, verifiable quality. The eleven deliverables all exist and all have real substance.
No Java was written. The copybook architecture, which the brief flagged as previously wrong, is
now correct: `CABSCDR` 200, `CABSCTL` 180, `CABSBHDR` 400, `CABSPRNT` 133, and every copybook
REDEFINES fits inside its target.

But two things make the artefact unfit for its stated purpose today:

1. **It gives away the answers.** 514 comments say `COMPLEXITY nn`. Five comments say `DEAD CODE`.
   Two point at the sealed answer key by name. `HUB/index.html` publishes a plain-English
   description of all eleven seeded defects next to their file names. `README.md` explains D6 in a
   paragraph. This is not a blind test; it is an open-book one.
2. **It does not compile.** 501 data entries lost their terminating period to a column-72 trim, 5
   literals lost their closing quote, and one REDEFINES is a byte longer than its target. That is
   506 unconditional compile errors in 25 files, none of them a seeded defect.

Secondary: the estate is 5% over the file ceiling and 8% over the LOC ceiling; the COBOL size
distribution does not match the required real-estate calibration; the README understates the estate
by 48 files and 10,853 lines; and two generator unit tests are red.

None of this is architectural. All of it is a remediation pass — a scrub script for the comments, a
re-flow of the affected source lines, a one-character fix in `CABING08.cbl`, a `construct` field
removed from `build_hub.py`, and a README regeneration. Fix those and this becomes the asset it was
specified to be.

---

*Every count in this report was reproduced independently against the source tree on 2026-08-15.
No manifest, README or prior report was treated as evidence.*
