# CABS Tier 5 — COMMON family manifest

Shared **called subprograms**. Every module here is invoked by a static `CALL 'literal'` from one
or more of the other batch families. None of them is a job step; none appears in any JCL.

Target: **OS/VS COBOL 1974**, Hercules TK4- / MVS 3.8j. Link-edited into `TELCABS.COMMON.LOADLIB`.
Complexity placements are authoritative in `CONTRACTS/complexity_placement.json`.

**12 programs, 4,496 lines.**

## Why this directory exists

An estate scan found twelve statically-called subprogram names with no source anywhere in the
tree. Every one of the 519 call sites below was compiling against a name that resolved at
link-edit time to a load module nobody held source for. Nothing in the batch estate links or runs
without these twelve. They were reconstructed from their call sites — the operand lists, the
operand `PIC` clauses, the return-code tests the callers make, and the fields the callers read
back after the call.

## Standards exception — CABS-STD-041

`CONVENTIONS.md` mandates `P0000-MAINLINE` / `P8000-CONTROL` and a `CABS-CONTROL-RECORD` written
to DD `CTLOUT` in **every batch program**. That rule governs job steps. The modules in this
directory are subprograms entered by `CALL` inside another program's run unit: they have no step
of their own, no `CTLOUT` allocation, and no balance to strike. Each carries the line

```
      * CONTROL     : NONE - SUBPROGRAMS DO NOT WRITE CTLOUT,         *
      *               CABS-STD-041                                    *
```

in its header block, and a sentence in the prose block confirming the calling program's balancing
equation is unaffected. `CABCTLWR` is the one that comes closest to writing a control record, and
it writes to a **separate** DD (`CTLAUX`) that no balancing process reads — the caller still
writes its own `CTLOUT` in its own `P8000-CONTROL`.

## The twelve modules

| Module | Lines | Purpose | Call sites | Families that call it |
|---|--:|---|--:|---|
| `CABPARMR.cbl` | 550 | Run parameter card reader. Accepts the SYSIN card, classifies it as positional or keyword, validates it, substitutes defaults from `PARMCTL`, and rebuilds it in place in the positional layout every caller redefines. | **141** | UTIL 101, CTC 21, RATING 10, INGEST 9 |
| `CABDTCNV.cbl` | 378 | Julian to Gregorian date conversion. `CCYYDDD` in, `CCYYMMDD` out, full leap-year rule, cumulative-days walk, pivot-70 windowing for the 5-digit callers. | **136** | UTIL 95, CTC 21, INGEST 13, RATING 7 |
| `CABHASH.cbl` | 273 | Control hash total contribution. Weighted positional hash of a 4-byte key added into the caller's `S9(15) COMP-3` accumulator, which becomes `CT-HASH-OCN`. | **120** | UTIL 79, INGEST 15, RATING 14, CTC 12 |
| `CABERRWR.cbl` | 459 | Suspense and error log writer. Accepts either of the two staging layouts in use, normalises to one 300-byte record, classifies the `Ennn` code, derives severity, writes `ERRLOG`. | **69** | SETTLE 13, BILLCALC 12, JURIS 11, FORMAT 9, REPORT 8, RATING 7, INGEST 7, CONTROL 2 |
| `CABEDITF.cbl` | 320 | Field edit and normalise. Byte classification, case folding, low/high-value replacement, left justification, overall class derivation. | **11** | UTIL 11 |
| `CABOCNVL.cbl` | 549 | OCN validation and effectivity. Format edit against the three industry OCN shapes, `SEARCH ALL` of the loaded carrier table, effective/expiry window test, 8-entry MRU cache. | **10** | INGEST 7, RATING 3 |
| `CABSEQCK.cbl` | 317 | Ascending sequence check across calls. Eight independent streams selected by the key's first byte; duplicates and descents counted per stream. | **10** | UTIL 10 |
| `CABCTLWR.cbl` | 313 | Auxiliary control record writer. Accumulates counts by action tag and writes a `CABS-CONTROL-RECORD` image to `CTLAUX`, striking the balancing equation for that record only. | **9** | UTIL 9 |
| `CABFMTR.cbl` | 320 | Display formatter for the audit reports. Numeric suppression, trailing-sign relocation, `YYDDD` expansion to `DDD/YY`, text folding. | **6** | UTIL 6 |
| `CABRTFMT.cbl` | 353 | Rate rounding and format. The estate's single rounding authority — six rule bytes plus a default, sign-correct, residue accumulated per rule. | **3** | RATING 3 |
| `CABTBLLU.cbl` | 320 | Reference table lookup. Multi-table `TBLREF` load, `SEARCH ALL` on table id plus code, description returned in place, 12-row embedded seed fallback. | **3** | UTIL 3 |
| `CABCIRCL.cbl` | 344 | Circuit inventory lookup. VSAM KSDS random read of `CIRCMST` with a 40-entry wrap-round cache and two derivation fix-ups the rating modules depend on. | **2** | RATING 2 |

**519 call sites in total.**

## Interfaces

Signatures are fixed by the existing call sites and must not be changed.

| Module | `PROCEDURE DIVISION USING` | Operand pictures |
|---|---|---|
| `CABPARMR` | `LK-PARM-CARD LK-PARM-RC` | `X(80)` / `9(04)` |
| `CABDTCNV` | `LK-DT-CCYYDDD LK-DT-GREG-DATE LK-DT-RC` | `9(07)` / group `9(04)+9(02)+9(02)` / `9(04)` |
| `CABHASH` | `LK-HS-FIELD LK-HS-ACCUM` | `X(04)` / `S9(15) COMP-3` |
| `CABERRWR` | `LK-EW-AREA LK-EW-RC` | `X(255)` redefined two ways / `9(04)` |
| `CABEDITF` | `LK-EF-FIELD LK-EF-RC` | `X(08)` / `9(04)` |
| `CABOCNVL` | `LK-OV-OCN LK-OV-VALID-SW` | `X(04)` / `X(01)` |
| `CABSEQCK` | `LK-SQ-KEY LK-SQ-RC` | `X(08)` / `9(04)` |
| `CABCTLWR` | `LK-CW-TAG LK-CW-RC` | `X(08)` / `9(04)` |
| `CABFMTR` | `LK-FM-FIELD LK-FM-RC` | `X(08)` / `9(04)` |
| `CABRTFMT` | `LK-RF-AMOUNT-IN LK-RF-RULE-IN LK-RF-AMOUNT-OUT LK-RF-RC` | `S9(13)V9(05) COMP-3` / `X(01)` / `S9(13)V9(02) COMP-3` / `9(04)` |
| `CABTBLLU` | `LK-TL-CODE LK-TL-RC` | `X(08)` / `9(04)` |
| `CABCIRCL` | `LK-CL-CIRCUIT-ID CABS-CIRCUIT-RECORD LK-CL-RC` | `X(20)` / `COPY CABSCIRC` / `9(04)` |

`CABHASH` and `CABOCNVL` have no return-code operand in their dominant signature; they reply in
the `RETURN-CODE` special register instead.

## Operand-length variance across call sites

The declared length of the first operand is not uniform across the call sites of the five UTIL
utilities. The shortest field any caller stages is eight bytes, so `CABEDITF`, `CABSEQCK`,
`CABCTLWR`, `CABFMTR` and `CABTBLLU` each declare `PIC X(08)` and address no more than eight
bytes. Callers that stage `X(10)` through `X(30)` present their leading eight.

`CABHASH` is documented as a four-byte interface. A minority of call sites hand it a wider
alphanumeric field or a packed field; only the leading four bytes contribute to the accumulator.

`CABDTCNV` is a seven-digit interface. Eight call sites still stage a five-digit `YYDDD` field;
the module detects that from the leading two digits and applies the pivot of 70, returning
`0004` so the caller can see it happened.

`CABERRWR` and `CABOCNVL` are each called with three different operand counts across the estate.
The modules declare the dominant arity; the surplus operands on the longer calls are not
addressed.

## DD names introduced

These modules open datasets of their own. A job step that calls them needs the DD allocated, or
the module falls back and returns the code that says so.

| DD | DSN | Opened by | Fallback if absent |
|---|---|---|---|
| `PARMCTL` | `TELCABS.CABS.PARMCTL` | `CABPARMR` | RC 0024, caller's own card used as presented |
| `ERRLOG` | `TELCABS.CABS.ERRLOG` | `CABERRWR` | RC 0016, nothing logged, caller's suspense write unaffected |
| `CARRTAB` | `TELCABS.CABS.CARRTAB` | `CABOCNVL` | RC 16, embedded 20-row seed table used |
| `CTLAUX` | `TELCABS.CABS.CTLAUX` | `CABCTLWR` | RC 0020, nothing written |
| `TBLREF` | `TELCABS.CABS.TBLREF` | `CABTBLLU` | RC 0016, embedded 12-row seed table used |
| `CIRCMST` | `TELCABS.CABS.CIRCMST` (VSAM KSDS) | `CABCIRCL` | RC 0012, hard status displayed |

## Return codes

**`CABPARMR`** — 0000 accepted as presented · 0004 fields defaulted from `PARMCTL` · 0008 keyword
form converted to positional · 0012 cycle not numeric or day-of-year outside 001-366 · 0016 no
card and no default row · 0020 tariff code not in the live table · 0024 `PARMCTL` unavailable.

**`CABDTCNV`** — 0000 converted · 0004 five-digit `YYDDD`, pivot of 70 applied · 0008 day-of-year
out of range for the year · 0012 year outside 1900-2099 · 0016 input not numeric · 0020 input
zero, Gregorian date returned as zeroes.

**`CABHASH`** (`RETURN-CODE`) — 0 all four bytes recognised, no fold · 4 one or more bytes not in
the collating table · 8 the accumulator was folded on the 10^15 modulus.

**`CABERRWR`** — 0000 logged · 0004 severity derived from the code · 0008 code not in the table,
logged as unclassified · 0012 layout probe defaulted · 0016 `ERRLOG` unavailable · 0020 tallies
displayed and log closed.

**`CABEDITF`** — 0000 clean · 0004 case folded · 0008 low/high-values blanked · 0012
left-justified · 0016 all spaces · 0020 unaccepted byte left as it stands · 0024 tallies.

**`CABOCNVL`** (`RETURN-CODE`) — 0 valid and effective · 4 found but outside its window · 8 format
edit failed · 12 not in the carrier table · 16 `CARRTAB` unavailable, seed table in use.

**`CABSEQCK`** — 0000 ascending · 0004 duplicate · 0008 descent · 0012 first key on this stream ·
0016 new stream allocated · 0020 stream table full, folded into stream 8 · 0024 tallies.

**`CABCTLWR`** — 0000 action taken · 0004 defaulted to accumulate · 0008 value not numeric, taken
as zero · 0012 written and balanced · 0016 written and out of balance · 0020 `CTLAUX`
unavailable · 0024 tallies displayed and log closed.

**`CABFMTR`** — 0000 numeric formatted · 0004 signed numeric formatted · 0008 date expanded ·
0012 text left-justified and folded · 0016 all spaces, unchanged · 0020 unclassified byte,
unchanged · 0024 tallies.

**`CABRTFMT`** — 0000 rounded per the rule · 0004 rule blank or unrecognised, half-up applied ·
0008 result overflowed the output field · 0012 rule `F` truncated because whole cents were zero ·
0016 residue accumulators displayed and reset.

**`CABTBLLU`** — 0000 found on `TBLREF` · 0004 found in the embedded seed · 0008 not found,
operand unchanged · 0012 staged table id not on the file · 0016 `TBLREF` unavailable · 0020 the
500-row table filled and later rows were not loaded · 0024 tallies.

**`CABCIRCL`** — 0000 read from the master · 0004 satisfied from cache · 0008 not found · 0012
VSAM hard status · 0016 circuit id was spaces, no read attempted.

## Behaviour that matters to a reader of the calling programs

- **All twelve keep state in `WORKING-STORAGE` between calls.** A subprogram stays loaded for the
  life of the run unit, so counters, loaded tables, caches and previous-key save areas persist
  from one `CALL` to the next. `CABSEQCK` has no function at all without it; `CABPARMR`,
  `CABOCNVL`, `CABTBLLU` and `CABCIRCL` load their reference data exactly once because of it.
- **`CABRTFMT` is the only place the rate-table round rule is interpreted.** A reader of the
  rating modules sees `COMPUTE` statements that variously round and truncate, and a `CALL` to
  `CABRTFMT`. What the rule byte actually means is here.
- **`CABCTLWR` writes a control record that no balancing process reads.** `CTLAUX` is not
  `CTLOUT`. A run can be in balance on `CTLOUT` and out of balance on `CTLAUX` and nothing
  reports it.
- **The residue accumulators in `CABRTFMT` and the drop counters in `CABEDITF`, `CABOCNVL`,
  `CABSEQCK` and `CABTBLLU` reach SYSOUT and nowhere else.** They are not carried into any
  control record, so a run that rounds away material value or folds a hash still balances.
- **`CABPARMR` normalises the caller's card in place.** Every calling program redefines its own
  80-byte card and reads `PC1-` fields straight out after the `CALL`. The keyword-to-positional
  conversion therefore happens invisibly from the caller's point of view.

## Not reconstructed, and why

Two further sets of static call targets remain unresolved in the tree, correctly:

| Target | Call sites | Why there is no source |
|---|--:|---|
| `CBLTDLI` | 36 | The IMS DB/DC language interface module, supplied by IBM with IMS. Resolved at link-edit from `IMS.RESLIB`. It is not application source and never has been. |
| `MQCONN` `MQOPEN` `MQPUT` `MQGET` `MQCLOSE` `MQCMIT` `MQDISC` | 12 | The WebSphere MQ call-level interface stubs, supplied by IBM. Resolved from `SCSQLOAD`. Same position. |

Dynamic call targets (`CALL identifier`) also remain unresolved, deliberately — see
`CONTRACTS/complexity_placement.json`, construct 2. Those eleven call sites in `CABRAT02`,
`CABRAT08`, `CABRAT10`, `CABRAT13`, `CABJUR03`, `CABJUR04`, `CABSET01` and `CABSET07` take their
target from a table lookup or a work field at run time and are not statically resolvable by
design. Closing them would remove the construct.

## Modernization note

These twelve modules are where a great deal of the estate's actual behaviour lives, and none of
it is visible from a reading of the calling programs. Four specific things must be re-specified
rather than translated:

1. **The rounding rules in `CABRTFMT`.** Six rule bytes, one of them (`F`) added for a 2005 tariff
   filing. The rate table carries the byte; only this module knows what it means.
2. **The pivot of 70 in `CABDTCNV`.** One of seven places the pivot is hardcoded. The other six
   must be found and changed with it.
3. **The hash algorithm in `CABHASH`.** The whole estate's control totals balance against it. Any
   target implementation must reproduce the weighting, the collating table and the fold modulus
   exactly, or every historical control record becomes unverifiable.
4. **The embedded seed tables** in `CABOCNVL` (20 OCNs), `CABTBLLU` (12 reference rows) and
   `CABEDITF` (the accepted punctuation set). These are constants in source that were never
   written down as requirements.
