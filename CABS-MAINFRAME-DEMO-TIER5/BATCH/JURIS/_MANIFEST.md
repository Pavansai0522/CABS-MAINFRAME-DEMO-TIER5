# JURISDICTION FAMILY - MANIFEST

The PIU/PLU engine. Determines interstate / intrastate / local jurisdiction for every rated usage record, splits minutes and revenue by the filed factors, falls back to defaults where a carrier files nothing, and reprices already-billed usage when a new factor arrives.

Application code `CABS`. OS/VS COBOL 1974 except `CABJUR10`, which is Enterprise COBOL with the DB2 precompiler (noted in its header).

| Program | Lines | Purpose | Complexities carried |
|---|---:|---|---|
| `CABJUR01.cbl` | 973 | PIU/PLU factor table load - reads the quarterly carrier filing, validates it, adds or replaces the factor on the KSDS and carries the superseded factor forward into FC-PRIOR-PIU. | 04 redefines (7); 09 job parameters; 19 two-digit year (pivot 70 in P2300); 25 Julian dates (P4000 absolute-day conversion, P4100 leap year) |
| `CABJUR02.cbl` | 929 | Factor validation and dispute quarantine - range edits PIU/PLU to 0-100, quarantines FC-DISPUTED factors, ages disputes and prints the validation register. | 04 redefines (6); 09 job parameters; 26 DEAD CODE - P5000-PSU-BAND-CHECK is compiled, is never PERFORMed and has not been called since V2.01 in 1999 |
| `CABJUR03.cbl` | 1058 | Jurisdiction determination - derives interstate / intrastate / local from originating and terminating LATA, falling back to the NPANXX table, then calls the state override module. | 01 FALL-THROUGH - P3300-SET-INTERSTATE has no exit and drops into P3400-ACCUM-JURIS; callers PERFORM P3300 THRU P3400-EXIT; 02 DYNAMIC CALL - CABJX + state suffix built at run time; 04 redefines (11); 09 job parameters |
| `CABJUR04.cbl` | 996 | PIU application - splits chargeable minutes and money between interstate and intrastate at the filed factor. Interstate MOU = MOU * PIU / 100; intrastate is derived by subtraction so the halves always add back. | 02 DYNAMIC CALL - CABRT + jurisdiction suffix; 04 redefines (9); 09 job parameters; 21 ROUNDING - the jurisdictional amount is COMPUTE ... ROUNDED here and truncated in CABJUR05 |
| `CABJUR05.cbl` | 888 | PLU application - splits the intrastate minutes into local and toll at the filed PLU and prices both halves. | 04 redefines (8); 09 job parameters; 21 ROUNDING - the same computation CABJUR04 rounds is truncated here; V2.02 aligned them in 2000 and V2.03 backed it out in 2002 |
| `CABJUR06.cbl` | 933 | Default factor fallback - derives a factor for any carrier that has filed nothing, from the carrier master, then the 1998 tariff table, then the 50 per cent industry fallback. | 04 redefines (6); 09 job parameters; 27 DORMANT FEATURE - P5000-STUDY-DERIVED is performed on every record and returns immediately; the traffic-study feed was decommissioned in June 2011 and no production JCL has passed the switch since |
| `CABJUR07.cbl` | 4075 | RETROACTIVE FACTOR RESTATEMENT - the monster. Reads prior-period priced usage and the billed detail history, resolves the new and the superseded factor, reprices the usage under both, computes the delta at five decimal places, apportions it across the billed rate elements, accumulates per carrier/state/jurisdiction buckets and raises adjustment records. Also restates the PLU split and special access meet-point percentages. | 01 FALL-THROUGH (P2700 into P2750); 04 redefines (21); 09 job parameters - RSTFROM and RSTWIN have no default anywhere; 19 two-digit year (P6500, P2800-MERGER-XREF); 21 ROUNDING - the delta is ROUNDED at five places; 25 Julian dates - P6500 absolute day, P6510 leap year, P6520 day addition with year rollover, P6530 date difference, P6540 Julian to Gregorian. |
| `CABJUR08.cbl` | 994 | Restatement reversal - backs out an adjustment raised by CABJUR07, in full or as a percentage, and checks that the reversal nets to zero against the original. | 04 redefines (7); 09 job parameters; 21 ROUNDING - the reversal truncates the amount CABJUR07 rounded, so a full reversal does not always net to zero; 25 Julian dates (P4200-PERIOD-TEST across a year boundary) |
| `CABJUR09.cbl` | 920 | Jurisdictional revenue summary - accumulates minutes and money by carrier, state and jurisdiction for the general ledger and brings in the settlement net position. | 04 redefines (7); 09 job parameters; 16 CROSS-APPLICATION ACCESS - this CABS program reads TELCABS.SETL.SETTLE.MASTER, owned by the settlement application; 21 ROUNDING - the ledger amount is ROUNDED here and truncated in CABJUR11 |
| `CABJUR10.cbl` | 857 | Restatement adjustment posting - inserts the adjustment into the CABSADJ DB2 table and updates the VSAM balance master. Enterprise COBOL with the DB2 precompiler. | 04 redefines (7); 09 job parameters; 17 TWO STORES ONE UPDATE - DB2 INSERT/UPDATE and VSAM REWRITE in the same logical unit of work with no coordination; the COMMIT covers DB2 only and the 2011 note claiming two-phase commit was never implemented |
| `CABJUR11.cbl` | 850 | Jurisdiction exception and audit report - reads the suspense file and the run control file, tallies by error code and flags any out-of-balance process. | 04 redefines (5); 09 job parameters; 21 ROUNDING - truncates the summary amount CABJUR09 rounds |

**Total: 11 programs, 13473 lines of COBOL.**

## JCL that drives this family

| Member | Purpose | Complexities |
|---|---|---|
| `JCL/CABJ1000.jcl` | Quarterly factor load | 09 scheduler symbolics, 10 GDG(0), 13 DD override |
| `JCL/CABJ1100.jcl` | Factor validation rerun | 11 same PROC different values, 12 four-library STEPLIB, 13 overrides |
| `JCL/CABJ1200.jcl` | Jurisdiction determination | 11, 12 five-library STEPLIB with CABJXCAL in two of them, 13 |
| `JCL/CABJ1300.jcl` | PIU application | 10 GDG(0), 11, 13 |
| `JCL/CABJ1400.jcl` | PLU application | 09 rates arrive as symbolics, 11, 13 |
| `JCL/CABJ1500.jcl` | Default factor derivation | 12 four-library STEPLIB, 27 dormant switch |
| `JCL/CABJ1600.jcl` | Quarterly restatement | 09 RSTFRM/RSTWIN no default, 10 reads (-1) and (-3), 13 |
| `JCL/CABJ1650.jcl` | Restatement simulation | 11 same PROC as CABJ1600, 13 ADJOUT to DUMMY |
| `JCL/CABJ1700.jcl` | Restatement reversal | 10 reads (-1), 12, 09 reversal run id |
| `JCL/CABJ1800.jcl` | Jurisdictional summary | 16 DD on TELCABS.SETL.SETTLE.MASTER, 12 |
| `JCL/CABJ1900.jcl` | Restatement posting to DB2 | 17, 11 CABPDB2P, 12 DB2 library concatenation |
| `JCL/CABJ2000.jcl` | Jurisdiction exception report | 10 GDG(0) |
| `JCL/PROCS/CABPFCTL.jcl` | Factor load fragment | 11 used by CABJ1000 and CABJ1100 |
| `JCL/PROCS/CABPJURS.jcl` | Generic jurisdiction step | 11 used by CABJ1200/1300/1400, 12 EMERG ahead of production |
| `JCL/PROCS/CABPREST.jcl` | Restatement fragment | 11 used by CABJ1600 and CABJ1650, 09, 10 |
| `JCL/PROCS/CABPDB2P.jcl` | DB2 posting fragment | 11 used by CABJ1900, CABS3100 and CABS3200, 17 |

## Seeded defects

The estate carries twelve deliberately seeded defects. Which programs carry them, in which paragraphs, what each one does, what it costs and what the correct modernization response is are recorded **only** in `SEALED/` — see `SEALED/defect_placements.json` and `SEALED/answer_key_*.json`. This manifest does not say whether this family carries one.
