# CABS Tier 5 — Build Conventions (MANDATORY, do not deviate)

## Compiler target
Runnable batch estate = **OS/VS COBOL (1974 standard)** for Hercules TK4-/MVS 3.8j.
- NO `EVALUATE` — use nested IF or GO TO ... DEPENDING ON
- NO reference modification `(pos:len)` — use INSPECT / UNSTRING / subscripted OCCURS walks
- NO `INITIALIZE`, NO inline `PERFORM`, NO `END-IF`/`END-PERFORM` scope terminators
- Use `PERFORM para THRU para-EXIT`, periods terminate statements
- Fixed format: cols 1-6 sequence (leave blank), col 7 indicator, cols 8-11 Area A, 12-72 Area B
- Comments: `*` in col 7

Part of the estate is **Enterprise COBOL** instead — scope terminators and `EVALUATE` are
permitted there. Two vintages, deliberate. The exempt set is enumerated below and is the only
place the exemption is defined.

## Enterprise COBOL exemption — the authoritative list

Everything named in the table below compiles under **Enterprise COBOL**. Everything not named in
it is OS/VS COBOL and the restrictions above apply without exception. `BUILDER/verify_syntax.py`
check 5 reads this table directly, so an auditor can tell a declared exemption from a violation
without reading any program: a hit inside the exempt set is reported as *exempt*, a hit anywhere
else is reported as a *violation* and fails the build gate.

A trailing `/` means the whole folder including anything added to it later. Any other entry is a
single file. Every exempt program also carries `COMPILER    : ENTERPRISE COBOL` in its header
block, so the declaration is visible in the source as well as here.

<!-- ENTERPRISE-COBOL-EXEMPTION -->

| Path | Scope | Why it is exempt |
|---|---|---|
| `SORTEXIT/` | folder | OS SORT/MERGE E15 input and E35 output exits. They are link-edited into the sort utility's address space, not into the batch load modules, and are built with the Enterprise COBOL compiler that the sort exit interface requires. They are not part of the TK4- runnable batch chain. |
| `ONLINE/` | folder | CICS command-level programs. They go through the CICS translator before the compiler, and the translator emits Enterprise COBOL. |
| `BATCH/CONTROL/` | folder | Reference-only control and reconciliation layer. Not scheduled in any production job stream and not built for MVS 3.8j. |
| `DB2/` | folder | DB2 reference material. Reserved — the folder currently holds no COBOL. |
| `BATCH/JURIS/CABJUR10.cbl` | file | Carries `EXEC SQL`. Goes through the DB2 precompiler and is then compiled with Enterprise COBOL. Cannot run on TK4-/MVS 3.8j and is reference-only. |
| `BATCH/SETTLE/CABSET12.cbl` | file | Carries `EXEC SQL`. Goes through the DB2 precompiler and is then compiled with Enterprise COBOL. Cannot run on TK4-/MVS 3.8j and is reference-only. |
| `BATCH/SETTLE/CABSET13.cbl` | file | Carries `EXEC SQL`. Goes through the DB2 precompiler and is then compiled with Enterprise COBOL. Cannot run on TK4-/MVS 3.8j and is reference-only. |

<!-- END ENTERPRISE-COBOL-EXEMPTION -->

To add an exemption, add a row here first. Adding one to a program without adding it here will
fail the gate; adding it here without adding the `COMPILER` header line will leave the source and
the convention disagreeing, which is the condition this table exists to prevent.

## Naming
- Programs: `CAB` + 5 chars, e.g. `CABING01`, `CABRAT01`. Max 8.
- Paragraphs: `Pnnnn-VERB-NOUN` and matching `Pnnnn-EXIT`. e.g. `P1000-INIT`, `P1000-EXIT`
- Sections: `Snnn-NAME SECTION.`
- Working storage: `WS-`, linkage `LK-`, file records use the copybook prefix
- Datasets: `TELCABS.<APP>.<NAME>` — apps are `CABS` (access billing) and `SETL` (settlement)
- JCL jobs: `CAB` + 5, PROCs `CABP` + 4

## Mandatory structure of every batch program
```
P0000-MAINLINE.       PERFORM P1000-INIT THRU P1000-EXIT.
                      PERFORM P2000-PROCESS THRU P2000-EXIT UNTIL WS-EOF.
                      PERFORM P8000-CONTROL THRU P8000-EXIT.
                      PERFORM P9000-TERM THRU P9000-EXIT.
```
- **P8000-CONTROL is NOT optional.** Every program writes a `CABS-CONTROL-RECORD` to DD `CTLOUT`
  populating CT-READ / CT-WRITTEN / CT-REJECTED / CT-SUMMARISED / CT-CARRIED-FWD and the four hash
  totals, then evaluates the balancing equation and sets CT-BAL-IND.
- Balancing equation: `CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARISED + CT-CARRIED-FWD`
- Every program COPYs `CABSWRK` (which nests CABSERR, CABSDATE, CABSCTL).

## Header comment block — mandatory on every program
```
*****************************************************************
* CABxxxnn - <one line purpose>                                 *
* APPLICATION : CABS | SETL                                     *
* INPUTS      : DDNAME  DSN                          COPYBOOK   *
* OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
* CONTROL     : CTLOUT                               CABSCTL    *
* BALANCE     : <the equation this program must satisfy>        *
* RESTART     : RESTARTABLE FROM CT-RESTART-KEY | FULL RERUN    *
* REVISION HISTORY                                              *
*   V1.00  <yyyy-mm-dd>  <initials>  <note>                     *
*****************************************************************
```
Revision histories must span 1987-2019 with realistic stale entries. Some must be WRONG or
describe behaviour that no longer exists — that is deliberate.

## Money arithmetic — read carefully
- All money `PIC S9(nn)V9(05) COMP-3`. Rates carry 5 decimals. Fractional cents are normal.
- Rounding is taken from `RT-ROUND-RULE` on the rate record, NOT from a global convention.
- **Deliberate inconsistency is required**: some programs `COMPUTE ... ROUNDED`, others truncate
  on the same field. Do not "fix" this. Record every instance in the placement plan.
- Accumulate in the order records arrive. Never re-sequence before summing.

## Complexity placement
Do not invent complexity placements. Implement ONLY what
`CONTRACTS/complexity_placement.json` assigns to your files. If you need to add one, add it to
that file so the traceability matrix stays authoritative.

## Files you must not edit
`COPYBOOKS/*` — the data architecture is frozen. If a layout is wrong, report it, do not change it.
