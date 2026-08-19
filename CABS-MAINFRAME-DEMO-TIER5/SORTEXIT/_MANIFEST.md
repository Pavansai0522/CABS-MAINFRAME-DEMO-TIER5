# SORTEXIT — MANIFEST

OS/360 Sort/Merge **E15 (input)** and **E35 (output)** user exit routines, referenced only by the
`MODS=` operand of the sort control cards in `JCL/CTLCARDS/`. Enterprise COBOL, LE-enabled,
link-edited into `TELCABS.COMMON.LOADLIB`.

Read `_README.md` first — it explains the E15/E35 mechanism, the register-1 parameter list, the
return-code protocol, and why static analysis does not see any of this.

**27 programs, 8,159 lines. Plus `_README.md` (135 lines).**

Two groups, written twenty years apart in estate time and in two different passes of this build:

- **`CABSE*` — 8 modules, 2,253 lines.** The original exits, referenced by `MODS=` on the DFSORT
  control cards in `JCL/CTLCARDS/`.
- **`CABSX*` — 19 modules, 5,906 lines.** The exits referenced by `MODS` on the MVT-era control
  cards in `JCL/CTLCARDS/MVT/`. Four of them (`CABSXJUR`, `CABSXZIP`, `CABSXBST`, `CABSXLST`) were
  already named by the DFSORT cards `CABSRT11`, `CABSRT12` and `CABSRT15` but had no source in the
  tree. The other fifteen exist because a rule that a DFSORT card expressed declaratively had to
  be moved somewhere when the card was restated for a sort product that has no `INCLUDE`, `OMIT`,
  `SUM`, `INREC`, `OUTREC`, `OUTFIL` or `FIELDS=COPY`.

Read `JCL/CTLCARDS/MVT/_README.md` for the card-to-exit mapping and the modernization argument.

## Group 1 — `CABSE*`, the DFSORT-card exits

| File | Lines | Purpose | Complexities carried | Runnable? |
|---|---:|---|---|---|
| `CABSE15A.cbl` | 269 | E15 for `CABSRT04`. Reformats the 200-byte CDR into the rating work layout so the control-card sort key positions line up; drops records with a blank or zero rate element code and records outside type 01-08. | **14** (sort-exit logic invisible to the call graph). Filter and reformat rules exist only here. | REFERENCE-ONLY |
| `CABSE15B.cbl` | 275 | E15 for the settlement aggregation sorts (`CABS2400`, `CABS2800`). Loads a 1200-row carrier-type table from DD `CARRTYPE` on first entry and drops every record for a non-settlement-party, unknown, or out-of-term carrier. | **14**. Also carries the only settlement-eligibility test in the aggregation path — `CABSET04` does not repeat it. | REFERENCE-ONLY |
| `CABSE15C.cbl` | 218 | E15 for the jurisdictional split sorts (`CABJ1300`, `CABJ1500`, `CABJ1900`). Drops local traffic, **rewrites an indeterminate jurisdiction to 'I' in place**, drops intrastate traffic outside a hardcoded 45-state territory table. | **14**. Territory list and the indeterminate-to-interstate default are constants in source, maintained by recompile. | REFERENCE-ONLY |
| `CABSE15D.cbl` | 188 | E15 for the bill-detail and summary presorts. Suppresses lines below 0.01 minutes AND $0.005; exempts setup, credit and make-up lines. Accumulates the suppressed value and reports it to SYSOUT only. | **14**. The suppressed value never reaches a control record, so a run that suppresses material value still balances. | REFERENCE-ONLY |
| `CABSE35A.cbl` | 180 | E35 for `CABSRT04`. Overlays bytes 189-200 with the 12-byte "rating control prefix" (run stamp, sort work-dataset ordinal, string sequence, exit version). No copybook describes this area. | **14**. Produces the `RC-WORK-ORD` field that `CABSE35B` depends on — a cross-sort coupling with no documentation anywhere. | REFERENCE-ONLY |
| `CABSE35B.cbl` | 429 | E35 for `CABSRT07`. Applies `RT-ROUND-RULE` (half-up / half-even / truncate / up-always) to the **summed** amount after `SUM FIELDS`, accumulates the fractional-cent residue and releases it as one adjustment record at end of merge. | **14**. A reviewer reading `CABRAT09` alone concludes the summary is unrounded — it is not. | REFERENCE-ONLY |
| `CABSE35C.cbl` | 381 | E35 for the usage roll-up sorts (`CABJ1600`, `CABRAT9R`). Full control-break summarisation: deletes every detail record, emits one total per OCN / BAN / bill period / jurisdiction. Credits and setup charges held in separate buckets. | **14**. The rate element was removed from the control group in 2012 — a grouping change made in a sort exit with no matching change in any COBOL module. | REFERENCE-ONLY |
| `CABSE35D.cbl` | 313 | E35 for the CMDS outbound exchange sort (`CABS2600`). Cuts the industry-format header (record code 01) and trailer (record code 99) per receiving RAO, including the detail count and amount hash the counterparty balances against. | **14**. `CABSET07` never sees these records and cannot reproduce them. Header carries CCYYDDD, trailer still carries YYDDD. | REFERENCE-ONLY |

## Group 2 — `CABSX*`, the MVT-card exits

Every module in this group holds a business rule that used to be readable as text on a control
card and is now compiled object code. The "rule moved from" column names the DFSORT statement it
replaced. All nineteen are **REFERENCE-ONLY** for the same reasons as group 1.

### 2a. Named by the DFSORT cards but never sourced until now

| File | Lines | Type | Pairs with | Purpose | Complexities carried |
|---|---:|---|---|---|---|
| `CABSXJUR.cbl` | 350 | E15 | `CABSRT11` | Variable-length bill detail. Reads the record length through parameter word two, deletes anything under 108 bytes, and where the jurisdiction byte is blank derives it from the two LATAs and two end states (`S`/`S`/`I`/`X`), then forces `I` when either LATA is in a ten-entry straddling table. | **14**. The straddling-LATA list and the indeterminate default are constants in source. Short-record tolerance moved here from `OPTION VLSHRT`. |
| `CABSXZIP.cbl` | 264 | E15 | `CABSRT12` | Detects document starts from the ASA carriage-control byte, pulls the ZIP off the invoice header line — the only line in the stream that carries it — and overlays bytes 1-20 of every line with a sort key of ZIP plus document ordinal plus line ordinal. | **14**. Line order within a document moved here from `OPTION EQUALS`. The 20 displaced bytes are assumed to be the formatter's leading blanks. |
| `CABSXBST.cbl` | 350 | E35 | `CABSRT12` | Strips the 20-byte key, restores the carriage control, buffers up to 400 lines of the current document and releases the whole document only if its first line restored to carriage control `7`. Partially formatted invoices are kept out of the mail run this way. | **14**. Nothing in the FORMAT family is aware the burst rule exists. Documents over 400 lines are released unconditionally. |
| `CABSXLST.cbl` | 350 | E35 | `CABSRT15` | Keeps one control record per process and step. On an equal key decides the survivor by `CT-RERUN-NBR`, then `CT-CYCLE-YYDDD` expanded on the pivot-70 rule, then arrival order. | **14**. "Keep the last attempt" moved here from `OPTION EQUALS` — without the option the sort no longer preserves input order within equal keys, so the tie-break is now an explicit rule in object code. |

### 2b. Written to carry rules moved off the DFSORT cards

| File | Lines | Type | Pairs with | Rule moved from | Purpose |
|---|---:|---|---|---|---|
| `CABSXSRC.cbl` | 313 | E15 | `CABSRT01` | `INCLUDE COND=` | Keeps record types 01-08 and only source system codes 03, 05, 07, held as `FILLER` literals. A new source system is now a recompile and a relink; its records are dropped silently with no suspense entry until then. |
| `CABSXEDT.cbl` | 263 | E15 | `CABSRT02` | `OMIT COND=` | Deletes records with a fatal edit status of 6 through 9, which were already diverted to suspense upstream. Non-digit status bytes are admitted and counted. |
| `CABSXMIN.cbl` | 320 | E35 | `CABSRT03` | `SUM FIELDS=(120,7,PD)` | Control-break summarisation on OCN / BAN / circuit / USOC adding packed conversation minutes. The estate's only deduplication-by-summing rule; no INGEST program knows it happens. |
| `CABSXRTY.cbl` | 251 | E35 | `CABSRT05` | `OUTREC FIELDS=` | Rebuilds the record and zeroes the retry count at 165-167 on every record that passes through. No rating module resets that field itself. |
| `CABSXCYC.cbl` | 312 | E35 | `CABSRT06` | `INCLUDE COND=` on a merge | Keeps only the current bill period. The cycle literal was hand-maintained on the card every period and is now a `FILLER` in source. Also checks the three merge inputs are really in ascending order and counts descents, which the sort does not do. |
| `CABSXSUM.cbl` | 400 | E35 | `CABSRT07` | `SUM FIELDS=(160,7,PD)` plus the chained `CABSE35B` | Both rules in one module, because one card cannot chain two E35 exits: control break on OCN / BAN / jurisdiction summing the packed amount, then `H`/`E`/`T`/`U`/blank rounding of the group total, residue accumulated and released as one adjustment record at end of merge. |
| `CABSXBAL.cbl` | 272 | E15 | `CABSRT08` | `INCLUDE COND=(40,1,CH,NE,C'B')` | Keeps only control records that did not balance. `COPY CABSCTL.` in LINKAGE, so the test is on the named `CT-BAL-IND` and its 88 levels rather than on one byte at a fixed offset — a widening of the original test. |
| `CABSXACC.cbl` | 318 | E15 | `CABSRT09` | `INCLUDE COND=(60,1,CH,NE,C'C')` | Deletes closed accounts before the bill trigger sees them, and bands the dropped balances by close age. The trigger still carries its skip-reason S2 test and still prints the counter. |
| `CABSXDTL.cbl` | 320 | E35 | `CABSRT10` | `SUM FIELDS=(37,8,PD,45,8,PD)` | Control break on BAN / period / section / line sequence / element sequence, adding quantity and amount. No COBOL program in the estate knows duplicate element sequences are possible. |
| `CABSXEDI.cbl` | 303 | E15 | `CABSRT13` | `FIELDS=COPY` + `INCLUDE COND=` | The four live trading-partner codes as an `OCCURS 8` table of `FILLER` literals with four spare slots. A partner going live is a recompile; a partner removed without one keeps receiving segments. |
| `CABSXTAP.cbl` | 320 | E35 | `CABSRT14` | `OUTFIL OMIT=` | Deletes zero-amount detail records from the tape despatch, labels and trailers exempt. Captures the upstream trailer hash as it passes and reports the difference, which is the tolerance the bureau agreed in 1997. |
| `CABSXDUP.cbl` | 320 | E35 | `CABSRT16` | `SUM FIELDS=NONE` + `OPTION EQUALS` | Exact-duplicate elimination across four suspense generations with the run id deliberately excluded from the comparison. Expands each run id's `YYDDD` on the pivot-70 rule and keeps the oldest, so the ageing report reports first suspension rather than most recent. |
| `CABSXLDG.cbl` | 299 | E15 | `CABSRT17` | `INREC FIELDS=(24,2,1,400)` | Moves the two-byte ledger company off the front of the invoice number to the front of the record, 400 bytes in and 402 out, updating the length word. The ledger company is not a field on the header and never was. |
| `CABSXLDR.cbl` | 270 | E35 | `CABSRT17` | `OUTREC FIELDS=(3,400)` | Strips the two-byte prefix back off so the month-end close program reads the layout it has always read, and validates the prefix against bytes 24-25 of the restored record. |
| `CABSXOOB.cbl` | 311 | E15 | `CABSRT18` | `INCLUDE COND=(74,11,...)` | Keeps only bill-proof records whose result text reads `OUT OF BAL `. The test is on the text written upstream, not on the difference field, so a record inside the upstream tolerance carries `IN BALANCE ` and is not selected whatever its difference field says. |

All nineteen carry complexity **14** (sort-exit logic invisible to the call graph). Eleven of them
additionally carry a rule that exists in no COBOL program in the estate and, after the move, in no
control card either.

## Runnable vs reference-only

All twenty-seven are **REFERENCE-ONLY**. Three reasons:

1. They are written in **Enterprise COBOL** (pointers, `SET ADDRESS OF`, `SET ... TO ADDRESS OF`,
   scope terminators). The runnable batch estate targets **OS/VS COBOL 1974** on Hercules
   TK4-/MVS 3.8j, which has none of those. Per `CONVENTIONS.md`, the reference layer is
   deliberately a second vintage.
2. The MVS 3.8j `SORT` program supports `MODS=` and the E15/E35 protocol, but the exits would
   have to be link-edited as OS/VS COBOL or assembler subroutines with a static save-area
   convention, not LE-enabled modules.
3. `CABSE15B` opens a QSAM file (`CARRTYPE`) from inside a sort exit. That works, but requires
   the DD to be allocated on the sort step, which the current `CABSRT` control-card members do
   not do.

## Documentation drift (deliberate, do not "fix")

- `JCL/CTLCARDS/CABSRT04.ctl` describes `CABSE15A` as **"(ASSEMBLER, CABSE15A)"**. The module was
  recoded in COBOL in 2004 (see its revision history). The control card was never updated. This
  is authentic decay and is left in place.
- `CABSE35A` calls its output area a **"prefix"** although since the 1994 fixed-length change it
  sits in the trailing filler. The name was kept.
- `CABSE15C`'s territory table is a 45-entry `REDEFINES` over three `PIC X(30)` literals. Adding a
  state requires a source change, a recompile and a relink into `TELCABS.COMMON.LOADLIB`.

## Seeded defects

The estate carries twelve deliberately seeded defects. Which programs carry them, in which paragraphs, what each one does, what it costs and what the correct modernization response is are recorded **only** in `SEALED/` — see `SEALED/defect_placements.json` and `SEALED/answer_key_*.json`. This manifest does not say whether this family carries one.

## Modernization note

Any target-state design must extract, from these **twenty-seven** modules **and** the
**thirty-five** control-card members across `JCL/CTLCARDS/` and `JCL/CTLCARDS/MVT/`, the complete
set of filter, transform, grouping and rounding rules and restate them as explicit pipeline
stages.

A large number of the rules are constants in source that were never written down as requirements
and must be re-specified, not translated:

- the 45-state territory list in `CABSE15C` and the indeterminate-to-interstate default
- the 65,000 string limit in `CABSE35A`
- the zero-suppression thresholds in `CABSE15D`
- the 2012 grouping change in `CABSE35C`
- the ten-entry straddling-LATA table in `CABSXJUR`
- the approved source-system list in `CABSXSRC`
- the four trading-partner codes in `CABSXEDI`
- the hand-maintained cycle literal in `CABSXCYC`
- the round-rule alphabet in `CABSXSUM` and `CABSE35B`
- the 400-line document buffer limit in `CABSXBST`
- the tie-break precedence in `CABSXLST` and `CABSXDUP`

The enumeration problem matters more than any individual rule. Nothing in the estate names these
modules in a `CALL`. The only path that reaches them runs JCL step to `SYSIN` DD to control-card
member to `MODS` operand to load-module name to source member — four indirections, three of them
text in a data file rather than a language construct. **Enumerate the exits from the `MODS`
operands before any code analysis begins**; the control cards are the only index that exists.

See `JCL/CTLCARDS/MVT/_README.md` for the full argument and the card-to-exit mapping.
