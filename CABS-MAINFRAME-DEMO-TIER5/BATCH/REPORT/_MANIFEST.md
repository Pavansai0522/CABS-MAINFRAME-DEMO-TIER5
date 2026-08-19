# REPORTING AND CLOSE FAMILY - MANIFEST

The control layer. `CABRPT01` consumes the `CABS-CONTROL-RECORD` that every process in the estate writes and proves the cycle end to end; the other seven report on revenue, suspense, factors, settlement, rate elements, unbilled usage and the period close.

Application code `CABS`. OS/VS COBOL 1974 throughout. Every program writes its own `CABS-CONTROL-RECORD` to DD `CTLOUT` in `P8000-CONTROL`.

**8 programs, 8,617 lines, 1,609 comment lines (19%).**

| Program | Lines | Cmt | Purpose | Principal datasets |
|---|--:|--:|---|---|
| `CABRPT01.cbl` | 2027 | 322 | **DAILY BALANCING REPORT — the monster.** Reads every control record in the cycle, reproves the balancing equation for each one, walks a 63-entry process chain table proving that what one process wrote is what the next one read, checks hash continuity, step sequence and missing processes, summarises the invoice-level proof and prints the verdict line operations read before releasing the print stream | CTLIN + PROOFIN -> BALOUT |
| `CABRPT02.cbl` | 971 | 183 | Revenue by carrier, state and jurisdiction for the ledger and the separations filing | BDTLIN + CARRMST -> REVOUT |
| `CABRPT03.cbl` | 916 | 178 | Suspense ageing — five bands, tally by error code, listing of anything older than the operator limit | SUSIN -> AGEOUT |
| `CABRPT04.cbl` | 934 | 182 | PIU/PLU factor exception report and column-by-column edit of the quarterly filing cards | FCTIN + CRDIN -> EXCOUT |
| `CABRPT05.cbl` | 908 | 180 | Settlement position by counterparty across all three settlement kinds | SETLIN + CARRMST -> POSOUT |
| `CABRPT06.cbl` | 879 | 188 | Rate element usage study and the flat extract the tariff analysts load onto the workstation | BDTLIN -> STUDYEX, STUDYRP |
| `CABRPT07.cbl` | 910 | 166 | Unbilled usage report — rated usage that did not reach an invoice, classified by reason, with the value | RATIN + TRIGIN -> UNBOUT |
| `CABRPT08.cbl` | 1072 | 210 | Month end close report and ledger posting — ten ledger lines proved against each other, control evidence for the whole period, and the close record the general ledger interface picks up | BHDRIN + CTLIN -> CLSOUT, CLOSEMS |

## Complexities carried, by program

| Program | Complexities present |
|---|---|
| `CABRPT01.cbl` | **03 HIDDEN ERROR HANDLER** — `P9970-CONTROL-FAILURE` at the physical bottom, reached by `GO TO` from `P2000`, `P3100`, `P3200`, `P4300` and `P5400`; **24 SORT INSIDE A PROGRAM** — the eligibility rule that decides which control records belong to the report lives in `S310-SORT-INPUT` and the equation proof lives in `S320-SORT-OUTPUT`; **22 PRINT CONTROL** — the report is burst by family and each page group carries `'4'`; 04 redefines (6); 09 job parameters — EXPPRC differs by cycle type and has no default |
| `CABRPT02.cbl` | **21 ROUNDING** — the rounded figure is recomputed from the 5dp accumulator at every level rather than adding the lower level rounded figures, which is the opposite convention to `CABRPT05`; **07 VARIABLE-LENGTH RECORDS** — reads the VB detail; 04 redefines (6); 09 job parameters (report level has no default) |
| `CABRPT03.cbl` | **01 FALL-THROUGH** — `P4200-AGE-BUCKET-60` has no exit paragraph; it ends at `P4200-DONE` and drops into `P4300-AGE-BUCKET-90`, so the 31-to-60 band test is always followed by the 61-to-90 band test whether the caller wanted it or not; 04 redefines (7) — the run-id prefix is read through a redefine of the age work area; 09 job parameters (oldest-days limit) |
| `CABRPT04.cbl` | **23 CHARACTER SCANNING** — `P4200-SCAN-CARD` and `P4210-ONE-COLUMN` walk the 80-column filing card position by position with INSPECT TALLYING and REPLACING and an implicit assumption about where each field starts; 04 redefines (7) — two views of the card; 09 job parameters (quarter, variance limit) |
| `CABRPT05.cbl` | **26 DEAD CODE** — `P6100-INTEREST-PROJECTION` and `P6110-ONE-INTEREST` implement the 1999 tariff review interest projection in full and are never PERFORMed; **21 ROUNDING** — the position is accumulated at 5dp and shown by a MOVE into a 2dp field, truncating what `CABRPT02` rounds; **16 CROSS-APPLICATION ACCESS** — reads `TELCABS.SETL.SETTLE.ALL`; 04 redefines (6) |
| `CABRPT06.cbl` | **07 VARIABLE-LENGTH RECORDS — SILENT TRUNCATION.** `P4000-BUILD-EXTRACT` moves the whole variable-length `CABS-BILL-DETAIL` group into `WS-STUDY-AREA PIC X(400)`. The source group is up to 1647 bytes, so everything past byte 400 is lost. The key fields are then restated over the top, which is why the record still looks correct; **06 CONDITION RANGES** — `WS-RS-DETAIL` VALUE 1 THRU 4 and `WS-RS-STUDY` VALUE 4 THRU 7; both true at level 4 and `P3000` tests detail first; 04 redefines (6) |
| `CABRPT07.cbl` | 04 redefines (7); 09 job parameters (age limit); reason derivation depends on the skip code the trigger process wrote |
| `CABRPT08.cbl` | **20 STRING ASSEMBLY** — `P4200-BUILD-CLOSE-KEY` assembles the 30-byte close key from six pieces and the ledger interface parses it by position; **15 FILE DEFINITION JOBS** — `TELCABS.CABS.CLOSEMST` is created only by `VSAM/CABVDEF6`; **10 GENERATION FILES** — the control file is concatenated across five weekly generations; 04 redefines (6); 09 job parameters — close period, ledger company and sign-off initials all have no default |

## JCL that drives this family

| Member | Purpose | Complexities |
|---|---|---|
| `JCL/CABS6000.jcl` | Daily balancing report | 09 EXPPRC no default, 11 CABPRPTB, 13, 14 CABSRT15 rerun consolidation |
| `JCL/CABS6100.jcl` | Revenue report and rate element study | 11 same PROC twice, 13 six DUMMY overrides, 09 |
| `JCL/CABS6200.jcl` | Suspense ageing and factor exceptions | 10 four-generation concatenation, 11, 13, 09 |
| `JCL/CABS6300.jcl` | Settlement position and unbilled usage | 10 SETL.SETTLE.ALL at (-1), 11, 13, 16 |
| `JCL/CABS6400.jcl` | Month end close report | 09 CLSPER/LEDGCO/SIGNOF, 10 five-generation control concatenation, 11 CABPCLOS, 15 CLOSEMST |
| `JCL/CABS7000.jcl` | **Month end close stream** — submits the bill calculation, format and report jobs through the internal reader, runs the balancing report in line so its return code governs the rest, then runs the close and the archive | 09 RESTART symbolic on the job card has no default, 10 GDG throughout, 11, 13, 12 |
| `JCL/PROCS/CABPRPTB.prc` | Reporting fragment | 11 used by CABS6000/6100/6200/6300 and CABS7000, 12 five-library STEPLIB, 09 |
| `JCL/PROCS/CABPCLOS.prc` | Month end close fragment | 11 used by CABS6400 and CABS7000, 09, 15 |
| `JCL/CTLCARDS/CABSRT15.ctl` | Control file consolidation | 14 the keep-the-last-rerun rule depends on OPTION EQUALS and an E35 exit |
| `JCL/CTLCARDS/CABSRT16.ctl` | Suspense consolidation | 14 the duplicate test deliberately excludes the run id, so the reported age is the age of the first suspension |
| `JCL/CTLCARDS/CABSRT18.ctl` | Out of balance listing | 14 the INCLUDE tests the result text, not the difference field, so anything inside the program tolerance is invisible here too |

The estate carries twelve deliberately seeded defects. Which programs carry them, in which paragraphs, what each one does, what it costs and what the correct modernization response is are recorded **only** in `SEALED/` — see `SEALED/defect_placements.json` and `SEALED/answer_key_*.json`. This manifest does not say whether this family carries one.
