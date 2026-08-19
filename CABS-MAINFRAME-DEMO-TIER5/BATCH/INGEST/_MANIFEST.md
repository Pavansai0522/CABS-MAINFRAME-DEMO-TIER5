# CABS Tier 5 - INGEST family manifest

Wholesale Carrier Access Billing System (CABS). Target: OS/VS COBOL 1974, Hercules TK4- / MVS 3.8j.
Complexity placements are authoritative in `CONTRACTS/complexity_placement.json`.
Seeded defects are recorded in `SEALED/` and are **not** annotated in the source, in this manifest or anywhere else outside `SEALED/`.

**12 programs, 7,825 lines, 1,440 comment lines (18%).**

| Program | Lines | Cmt | Cmt% | Purpose | Principal datasets |
|---|--:|--:|--:|---|---|
| `CABING01.cbl` | 2165 | 648 | 30% | Raw EMI access usage edit and format validation | RAWIN -> EDTOUT, SUSOUT |
| `CABING02.cbl` | 526 | 75 | 14% | OCN and BAN validation against the carrier master | EDTIN + CARRMST -> VLDOUT, SUSOUT |
| `CABING03.cbl` | 388 | 59 | 15% | Duplicate and out-of-order sequence detection | VLDIN -> DUPOUT, SUSOUT |
| `CABING04.cbl` | 418 | 56 | 13% | YYDDD connect and disconnect date validation | DUPIN -> DTVOUT, SUSOUT |
| `CABING05.cbl` | 612 | 110 | 18% | Usage type split into voice, data and special streams | DTVIN -> VOCOUT, DATOUT, SPCOUT, CLNOUT, SUSOUT |
| `CABING06.cbl` | 572 | 53 | 9% | Circuit and trunk group resolution | CLNIN + CIRCMST -> ENROUT, SUSOUT |
| `CABING07.cbl` | 372 | 40 | 11% | Suspense consolidation and audit logging | SUSIN -> SUSOUT, AUDLOG, RPTOUT |
| `CABING08.cbl` | 517 | 80 | 15% | Cycle-boundary carry-forward handler | CLNIN + CFWIN -> CFWOUT, ENROUT, SUSOUT |
| `CABING09.cbl` | 822 | 113 | 14% | Daily consolidation and volume reporting | VOCIN + DATIN + SPCIN -> CONOUT, RPTOUT |
| `CABING10.cbl` | 539 | 82 | 15% | Jurisdictional pre-edit and indeterminate resolution | ENRIN + FCTRIN + CARRMST -> JUROUT, SUSOUT |
| `CABING11.cbl` | 473 | 64 | 14% | Suspense recycle and reinjection | SUSIN -> EDTOUT, RCYOUT, SUSOUT |
| `CABING12.cbl` | 421 | 60 | 14% | EMI 42-XX record bridge (legacy pre-divestiture feed) | BRGIN -> BRGOUT |

## Complexities carried, by program

| Program | Complexities present (numbers refer to the 27-construct catalogue) |
|---|---|
| `CABING01.cbl` | 1 fall-through (P6400-EDIT-VARIANT-SPCL), 3 hidden error handler (P9900-FATAL-EXIT, GO TO from 3 sites), 4 REDEFINES, 5 RENAMES (WS-BILL-KEY), 6 overlapping 88s (WS-EDT-SEVERITY on '3'), 19 inline pivot 70, 20 STRING/UNSTRING, 23 INSPECT + byte-table walk |
| `CABING02.cbl` | 4 REDEFINES, 23 character scanning (BAN as 13-cell table, hardcoded segment positions) |
| `CABING03.cbl` | 1 fall-through (P3200-CHECK-SEQ-GAP falls into P3300-CHECK-SEQ-DUP), 4 REDEFINES |
| `CABING04.cbl` | 19 two-digit year (DW-PIVOT-YY plus literal 70 inline x4, real leap-year math), 4 REDEFINES |
| `CABING05.cbl` | 1 fall-through (P4300-SPLIT-FATAL), 6 overlapping 88s (CD-VOICE-MOU/CD-DATA-SVC both '03'; WS-STREAM-CLASS), 4 REDEFINES |
| `CABING06.cbl` | 27 dormant feature (WS-MPB-AUTO-SW = 'N', MPB auto-split intact and unreachable), 23 CLLI character scanning, 6 REDEFINES |
| `CABING07.cbl` | 3 hidden error handler (P9950-SUSPENSE-FAILURE, GO TO from 3 sites), 18 append writes (AUDLOG OPEN EXTEND), 20 STRING/UNSTRING, 3 REDEFINES |
| `CABING08.cbl` | 18 append writes (CFWOUT OPEN EXTEND, physical order = cycle sequence), 5 RENAMES (WS-CYCLE-STAMP), 19 inline pivot 70, 3 REDEFINES |
| `CABING09.cbl` | 1 fall-through (P5100-CONSOL-VOICE), 7 variable-length records (RECORD IS VARYING on DATIN; ODO summary table), 26 dead code (P6600-NEGATIVE-MOU-ADJ, impossible guard), 5 REDEFINES |
| `CABING10.cbl` | 6 overlapping 88s (WS-JUR-CLASS on 'X'), 4 REDEFINES, mixed ROUNDED/truncating COMPUTE on the juris split |
| `CABING11.cbl` | 1 fall-through (P4200-RECYCLE-WARN falls into P4300-RECYCLE-ERROR), 3 REDEFINES |
| `CABING12.cbl` | 26 dead code - ORPHANED PROGRAM, referenced by no JCL member in the estate; 23 character scanning of the EMI header, 5 REDEFINES |
