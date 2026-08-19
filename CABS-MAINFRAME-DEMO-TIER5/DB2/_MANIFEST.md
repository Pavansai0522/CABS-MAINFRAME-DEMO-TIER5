# DB2/ — Manifest

All members in this folder are **REFERENCE-ONLY**. MVS 3.8j / TK4- has no DB2 subsystem, so none of
this is runnable under the Hercules estate. The DDL, GRANTs and utility JCL describe the DB2 for
z/OS subsystem (`DSNP`) that the SETL and CABS DB2-precompiled programs (`BATCH/SETTLE/CABSET12.cbl`,
`BATCH/SETTLE/CABSET13.cbl`, `BATCH/JURIS/CABJUR10.cbl`) already assume exists, and that the CICS and
PL/I reference layers assume for rate/factor maintenance and invoicing.

| File | Lines | Purpose | Complexities carried | RUNNABLE / REFERENCE-ONLY |
|---|---|---|---|---|
| `CABSTBL.ddl` | 332 | STOGROUP, DATABASE, TABLESPACE and CREATE TABLE for the seven CABSDB01 tables (CABSRTHS, CABSFCHS, SETLTRAN, SETLPERIOD, CABSADJ, CABSINVR, CABSAUDT) | none placed here | REFERENCE-ONLY |
| `CABSIDX.ddl` | 117 | Unique index (= primary key) for each of the seven tables, plus two non-unique secondary indexes | none placed here | REFERENCE-ONLY |
| `DCLRTHS.cpy` | 68 | DCLGEN host structure for CABSRTHS | none placed here | REFERENCE-ONLY |
| `DCLFCHS.cpy` | 65 | DCLGEN host structure for CABSFCHS | none placed here | REFERENCE-ONLY |
| `DCLSETT.cpy` | 69 | DCLGEN host structure for SETLTRAN | none placed here | REFERENCE-ONLY |
| `DCLINVR.cpy` | 74 | DCLGEN host structure for CABSINVR | none placed here | REFERENCE-ONLY |
| `DCLAUDT.cpy` | 52 | DCLGEN host structure for CABSAUDT | none placed here | REFERENCE-ONLY |
| `CABSBIND.jcl` | 111 | BIND PACKAGE + BIND PLAN for CABSPLAN (CABS application) and SETLPLAN (SETL application) | none placed here | REFERENCE-ONLY |
| `CABSGRNT.sql` | 109 | GRANT authority to CABSBAT, SETLBAT, CABSONL, plus a temporary PUBLIC grant | none placed here | REFERENCE-ONLY |
| `CABSRUNS.jcl` | 126 | RUNSTATS and REORG utility job (DSNUTILB) for all seven tablespaces | none placed here | REFERENCE-ONLY |

No complexity from `CONTRACTS/complexity_placement.json` is placed in `DB2/`. The scope owner for the
27-complexity plan is the INGEST and RATING COBOL families and their JCL; this folder is data
architecture and DBA tooling that those families do not read, plus the DB2 side of the SETL/CABS
programs that already exist in `BATCH/SETTLE/` and `BATCH/JURIS/`.

## Who reads and writes these tables

- **`BATCH/SETTLE/CABSET12.cbl`** (paragraphs `P3500-CLOSE-DB2` / `P3600-INSERT-DB2`) reads and writes
  **`SETLPERIOD`** — the settlement period status row, updated or inserted on period close, and the
  first half of the program's uncoordinated two-store pattern (the second store is the VSAM
  `CLOSEMST` KSDS `TELCABS.SETL.CLOSE`).
- **`BATCH/SETTLE/CABSET13.cbl`** (paragraphs `P3000-POST-DB2` / `P3200-UPDATE-ROW`) reads and writes
  **`SETLTRAN`** — the settlement ledger row for each meet-point, reciprocal compensation or CMDS/RAO
  transaction. Column names and types here are fixed by that program's `WS-HV-` host variable group;
  this DDL and `DCLSETT.cpy` were built to match it exactly, not the other way around.
- **`BATCH/JURIS/CABJUR10.cbl`** (paragraphs `P3000-POST-DB2` / `P3100-UPDATE-ROW`) reads and writes
  **`CABSADJ`** — the restatement adjustment posting row, insert-or-update keyed on
  BAN/BILL_PERIOD/SECTION_CD/LINE_SEQ, with the duplicate-key tolerance documented in that program's
  header comment.

None of the three programs above touch `CABSRTHS`, `CABSFCHS`, `CABSINVR` or `CABSAUDT`. Those four
tables belong to the CICS and PL/I layers of the estate:

- **`CABSRTHS`** (rate history) and **`CABSFCHS`** (factor history) are maintained by the on-line rate
  and factor maintenance CICS transactions and by the batch-side rate table utility `PLI/CABRTMNT.pli`.
  The CICS transactions write the before/after image to `CABSAUDT`; `CABRTMNT.pli` writes its own
  control record to a VSAM file rather than to `CABSAUDT` (see `PLI/_MANIFEST.md`).
- **`CABSINVR`** (invoice register) is populated by the on-line bill inquiry CICS transaction from the
  bill header summary (`CABSBHDR`) written by the billing batch stream; no batch COBOL program in this
  estate inserts into it directly.
- **`CABSAUDT`** (audit trail) is written by the CICS rate and factor maintenance transactions for
  every row-level change to `CABSRTHS` and `CABSFCHS`.

## Notes on period-authentic construction

- `CABSTBL.ddl` mirrors the frozen VSAM copybooks field-for-field where a VSAM record maps to a DB2
  table (`CABSRATE`→`CABSRTHS`, `CABSFCTR`→`CABSFCHS`, `CABSSETL`→`SETLTRAN`, `CABSBHDR`→`CABSINVR`),
  less the `RT-BAND` OCCURS DEPENDING ON table on `CABSRATE`, which DB2 cannot represent as a column
  and which stays VSAM-only.
- A comment on `CABSRTHS` in `CABSTBL.ddl` records that a FIELDPROC/EDITPROC for the five-decimal rate
  columns was evaluated in 2005 and not adopted; no FIELDPROC or EDITPROC is actually attached to any
  table.
- `XINVR02` in `CABSIDX.ddl` carries a comment stating it was added in 2006 for a report (`RPTCAB22`)
  that was retired in 2009. The index itself was not dropped.
- `CABSGRNT.sql` carries a `GRANT SELECT ... TO PUBLIC` on `CABSRTHS`, with a comment dating it to the
  1998 FCC Part 61 rate filing audit and describing it as temporary pending fieldwork close.
- `CABSBIND.jcl` binds `CABSPLAN` before `SETLPLAN` because the `SETLPLAN` package list references a
  package bound in the `CABSCOL` collection during the first step; running the steps out of order on a
  freshly initialized subsystem fails the second BIND PLAN.
