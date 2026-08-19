# SETTLEMENT FAMILY - MANIFEST

Inter-carrier settlement in three kinds - meet-point billing, reciprocal compensation and CMDS/RAO exchange - plus netting, disputes, statements, period close and posting. Everything writes `CABSSETL` records.

Application code `SETL`. OS/VS COBOL 1974 except `CABSET12` and `CABSET13`, which are Enterprise COBOL with the DB2 precompiler (noted in their headers).

| Program | Lines | Purpose | Complexities carried |
|---|---:|---|---|
| `CABSET01.cbl` | 2131 | MEET POINT BILLING SETTLEMENT CALCULATION - the second monster. Reads the validated meet-point circuits, matches them to the CABS billed detail, walks the rate elements for meet-point eligible revenue, resolves the two billing percentages, splits the gross between the two LECs and writes CABSSETL records plus a percentage variance file. | 02 DYNAMIC CALL - CABMP + region suffix (regional residual rules, 2013); 04 redefines (16); 09 job parameters - the settlement period has no default; 16 CROSS-APPLICATION ACCESS - reads TELCABS.CABS.BILLDTL directly; 21 ROUNDING - our share is truncated while their share is ROUNDED. |
| `CABSET02.cbl` | 858 | Meet point circuit extract and eligibility - selects meet-point eligible circuits from the settlement circuit inventory and attaches the usage carried on the billing usage file. | 04 redefines (7); 09 job parameters; 16 CROSS-APPLICATION ACCESS - reads TELCABS.CABS.CDR.PLU, owned by the billing application |
| `CABSET03.cbl` | 811 | Meet point percentage validation and variance - range-edits both percentages, computes the variance from 100 and reports it. Every circuit is released whatever the variance. | 04 redefines (6); 09 job parameters |
| `CABSET04.cbl` | 973 | Reciprocal compensation MOU aggregation - aggregates terminating minutes by counterparty and splits them into ISP-bound and voice, excluding transit minutes. | 01 FALL-THROUGH - P2300-CLASSIFY has no exit and drops into P2400-TRANSIT-TEST; 04 redefines (8); 09 job parameters |
| `CABSET05.cbl` | 2057 | RECIPROCAL COMPENSATION CALCULATION WITH ISP CAP - resolves the negotiated rate and the ISP-bound cap from the interconnection agreement table, applies both, writes the settlement record and the capped-minute evidence file sent to the CLEC. | 04 redefines (14); 09 job parameters - the cap override has no default and zero means "use the agreement cap"; 16 CROSS-APPLICATION ACCESS - reads TELCABS.CABS.CDR.RECIP; 19 two-digit year (P5400 agreement effective date, pivot 70); 21 ROUNDING - the net due is ROUNDED and CABSET11 truncates it. |
| `CABSET06.cbl` | 857 | Wireless termination settlement - intraMTA at the reciprocal rate, interMTA at access rates. | 04 redefines (7); 09 job parameters; 27 DORMANT FEATURE - the whole program has been inert since July 2011 when the arrangement moved to bill-and-keep. P1400-FEATURE-CHECK reads a switch that no production JCL has set to Y since that date; job CABS2450 still runs every month and produces an empty settlement file |
| `CABSET07.cbl` | 1107 | CMDS RAO outbound exchange production - selects settlement records old enough to exchange, translates the counterparty to an RAO code and formats the 180-byte industry record with header and trailer. | 02 DYNAMIC CALL - CABRA + region suffix, five regional formatters never harmonised; 04 redefines (11); 09 job parameters - the exchange date has no default; 25 Julian dates (P2300-AGE-TEST across a year boundary) |
| `CABSET08.cbl` | 996 | CMDS RAO inbound exchange consumption - validates header, detail and trailer, translates the sending RAO to an OCN, inverts the direction and balances against the trailer hash totals. | 04 redefines (9); 09 job parameters; 19 TWO-DIGIT YEAR - the inbound exchange date is a YYDDD expanded with the hardcoded pivot of 70 in P2400-DETAIL-EDIT |
| `CABSET09.cbl` | 1053 | Settlement netting by counterparty - accumulates receivables and payables per counterparty across all three settlement kinds and derives the net position and the due date. | 04 redefines (8); 09 job parameters; 16 CROSS-APPLICATION ACCESS - reads TELCABS.CABS.CARRIER for the carrier name and payment terms; 21 ROUNDING - the net is ROUNDED here and truncated by CABSET11; 25 Julian dates (P4300-ADD-DAYS with year rollover); 26 DEAD CODE - P5000-INTEREST-CALC is never PERFORMed, suspended in April 1999 pending a tariff review that never concluded |
| `CABSET10.cbl` | 918 | Settlement dispute handler - applies raise, resolve and withdraw transactions against the settlement master in place and logs every action. | 04 redefines (8) - three views of the dispute transaction, one per action code; 09 job parameters |
| `CABSET11.cbl` | 874 | Settlement statement generator - produces the printed statement and remittance advice for each counterparty, carrying the invoice number from the billing application. | 04 redefines (6); 09 job parameters; 16 CROSS-APPLICATION ACCESS - reads TELCABS.CABS.BILLHDR for the invoice number; 21 ROUNDING - truncates the net that CABSET09 rounds, so the statement and the net file can differ by a cent |
| `CABSET12.cbl` | 949 | Settlement period close - closes the period in the SETLPERIOD DB2 table and in the VSAM close file. Enterprise COBOL with the DB2 precompiler. | 04 redefines (8); 09 job parameters; 17 TWO STORES ONE UPDATE - DB2 UPDATE/INSERT and VSAM WRITE/REWRITE with no coordination; 19 two-digit year (close date expanded with pivot 70); 25 Julian dates (P5500-ADD-DAYS, P5700 Julian to Gregorian for the DB2 DATE column) |
| `CABSET13.cbl` | 804 | Settlement posting - inserts into the SETLTRAN DB2 reporting table and writes the VSAM settlement master. Enterprise COBOL with the DB2 precompiler. | 04 redefines (7); 09 job parameters; 17 TWO STORES ONE UPDATE - the insert-count and the VSAM-count are compared at end of run and a divergence only produces a message; the resync utility was written in 2012 and has never been scheduled |

**Total: 13 programs, 14388 lines of COBOL.**

## JCL that drives this family

| Member | Purpose | Complexities |
|---|---|---|
| `JCL/CABS2100.jcl` | Meet point circuit extract | 11 CABPMPBX, 16 DD on TELCABS.CABS.CDR.PLU, 13 |
| `JCL/CABS2200.jcl` | Meet point settlement | 11 CABPSETL, 12 five-library STEPLIB, 13, 16 BILLDTL |
| `JCL/CABS2300.jcl` | Meet point percentage validation | 11 same PROC as CABS2100 different values, 13 DUMMY overrides |
| `JCL/CABS2400.jcl` | Reciprocal comp aggregation | 11 CABPRECP, 10 GDG(0) |
| `JCL/CABS2450.jcl` | Wireless termination settlement | 27 dormant - runs monthly, does nothing since 2011, 12 |
| `JCL/CABS2500.jcl` | Reciprocal comp calculation | 09 CAPOVR no default, 11, 13, 16 CDR.RECIP |
| `JCL/CABS2600.jcl` | CMDS outbound exchange | 09 EXCHDT no default, 11 CABPCMDS, 13 |
| `JCL/CABS2700.jcl` | CMDS inbound exchange | 11 same PROC opposite direction, 13 four DD overrides |
| `JCL/CABS2800.jcl` | Settlement netting | 11 CABPNETT, 16 DD on TELCABS.CABS.CARRIER |
| `JCL/CABS2900.jcl` | Dispute handler | 12 four-library STEPLIB |
| `JCL/CABS3000.jcl` | Settlement statements | 11 same PROC as CABS2800, 13, 16 BILLHDR |
| `JCL/CABS3100.jcl` | Settlement period close | 17, 11 CABPDB2P, 13 five overrides |
| `JCL/CABS3200.jcl` | Settlement posting | 17, 12 six-library STEPLIB, 13 |
| `JCL/PROCS/CABPMPBX.jcl` | Meet point extract fragment | 11 used by CABS2100 and CABS2300, 16 |
| `JCL/PROCS/CABPSETL.jcl` | Settlement calculation fragment | 11, 12 EMERG then SETL then CABS libraries |
| `JCL/PROCS/CABPRECP.jcl` | Reciprocal comp fragment | 11 used by CABS2400 and CABS2500, 09 |
| `JCL/PROCS/CABPCMDS.jcl` | CMDS exchange fragment | 11 used by CABS2600 and CABS2700, 13 |
| `JCL/PROCS/CABPNETT.jcl` | Netting and statement fragment | 11 used by CABS2800 and CABS3000, 16 |

## Seeded defects

The estate carries twelve deliberately seeded defects. Which programs carry them, in which paragraphs, what each one does, what it costs and what the correct modernization response is are recorded **only** in `SEALED/` — see `SEALED/defect_placements.json` and `SEALED/answer_key_*.json`. This manifest does not say whether this family carries one.
