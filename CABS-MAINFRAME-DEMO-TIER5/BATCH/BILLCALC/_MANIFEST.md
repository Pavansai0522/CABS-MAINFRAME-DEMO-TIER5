# BILL CALCULATION FAMILY - MANIFEST

Assembles the invoice. Takes rated and jurisdictionalised usage plus the inter-carrier settlement position and produces the variable-length bill detail, the invoice header, the tax, the pre-bill audit, the account-level balancing proof and the final invoice number.

Application code `CABS`. OS/VS COBOL 1974 throughout — no `EVALUATE`, no reference modification, no scope terminators. Every program writes `CABS-CONTROL-RECORD` to DD `CTLOUT` in `P8000-CONTROL`.

**12 programs, 13,816 lines, 2,835 comment lines (21%).**

| Program | Lines | Cmt | Purpose | Principal datasets |
|---|--:|--:|---|---|
| `CABBIL01.cbl` | 1221 | 248 | Bill trigger and cycle selection — decides which accounts bill on this cycle date, writes the trigger file every later step is driven from, and writes every account that did not bill with a reason code | ACCTIN + CARRMST -> TRIGOUT, SKPOUT |
| `CABBIL02.cbl` | 2312 | 435 | **BILL DETAIL LINE ASSEMBLY — the monster.** Folds rated element records into variable-length `CABSBILL` records carrying 1 to 40 rate elements, derives the bill section from the element prefix, assembles the 60-byte printed description from six fragments, rounds the line total and opens a continuation line at element 41 | RATIN + TRIGIN -> BDTLOUT (VB 1651), SUSPOUT |
| `CABBIL03.cbl` | 919 | 211 | Bill section sequencing and suppression — internal SORT; the zero-value suppression rule lives in the input procedure and the renumbering in the output procedure | BDTLIN -> BDTLSEQ |
| `CABBIL04.cbl` | 1119 | 230 | Prior balance and payment application — creates the bill header, applies cash oldest-first or newest-first, maintains the five ageing buckets | BALIN + PAYIN -> BHDROUT |
| `CABBIL05.cbl` | 1127 | 228 | Adjustment and restatement application — credits, debits, dispute awards and the factor restatement adjustments raised by `CABJUR07`; builds the printed narrative | BHDRIN + ADJIN -> BHDROUT |
| `CABBIL06.cbl` | 956 | 204 | Settlement netting into the invoice — brings the counterparty net position onto the header | BHDRIN + SETLIN -> BHDROUT |
| `CABBIL07.cbl` | 1080 | 223 | Tax and surcharge calculation — federal excise, federal surcharge, state sales tax with compounding, local surcharge and E911; each component rounded separately | BHDRIN + TAXMST -> BHDROUT |
| `CABBIL08.cbl` | 893 | 194 | Minimum and maximum bill enforcement — contract make-up charge and excess credit; computes the invoice total due | BHDRIN + MMXIN -> BHDROUT |
| `CABBIL09.cbl` | 1006 | 208 | Invoice header creation and jurisdiction split — accumulates the detail into the header, derives the interstate/intrastate/local split and sets `BH-HASH-AMOUNT` at five decimal places | BDTLIN + BHDRIN -> BHDROUT |
| `CABBIL10.cbl` | 1101 | 223 | Pre-bill audit and hold-bill processing — five tests; a held invoice is not numbered and not printed | BHDRIN + PRIORIN + HOLDMST -> BHDROUT, HOLDOUT |
| `CABBIL11.cbl` | 1033 | 206 | Bill-level balancing — proves that the bill detail adds up to the invoice header for every account and writes the proof file `CABRPT01` consumes | BDTLIN + BHDRIN -> PROOFOUT |
| `CABBIL12.cbl` | 1049 | 225 | Final invoice numbering — 14-byte number assembled piece by piece with a modulus-eleven check character | BHDRIN + INVCTL -> BHDROUT, INVCTL |

## Complexities carried, by program

| Program | Complexities present (numbers refer to the 27-construct catalogue) |
|---|---|
| `CABBIL01.cbl` | 04 redefines (6); 09 job parameters — FORCE switch, OCN range and due-days all arrive as symbolics with no default; 22 print control; 23 reason-table walk |
| `CABBIL02.cbl` | **03 HIDDEN ERROR HANDLER** — `P9990-DETAIL-FAILURE` at the physical bottom, reached by `GO TO` from `P3000`, `P4400` and `P5600`; **07 VARIABLE-LENGTH RECORDS** — the ODO producer, 1 to 40 `BD-ELEMENT` occurrences; **20 STRING ASSEMBLY** — `P5400` plus five fragment paragraphs; **21 ROUNDING** — `BD-TOT-ROUNDED` computed `ROUNDED`, and `CABBIL11` truncates the same quantity; **23 CHARACTER SCANNING** — `P5210` prefix build, `P5500` INSPECT TALLYING/REPLACING plus a reverse subscripted walk; 04 redefines (8); 09 job parameters (MAXELM has no default) |
| `CABBIL03.cbl` | **06 CONDITION RANGES** — `WS-SC-USAGE-SECTION` VALUE 'U1' THRU 'U3' and `WS-SC-CHARGE-SECTION` VALUE 'U2' THRU 'C4'; both are true for U2 and U3 and `P4200` tests usage first; **07 VARIABLE-LENGTH RECORDS** — ODO walk in `P4000`; **24 SORT INSIDE A PROGRAM** — the eligibility rule is in `S310-SORT-INPUT` and the renumbering in `S320-SORT-OUTPUT`; 04 redefines (6) |
| `CABBIL04.cbl` | **26 DEAD CODE** — `P6200-DEFERRED-PAYMENT-PLAN` and `P6210-PLAN-SCHEDULE` are compiled and never PERFORMed; the arrangement was written for the 1995 settlement programme; 04 redefines (7); 09 job parameters (ageing base date) |
| `CABBIL05.cbl` | **01 FALL-THROUGH** — `P3400-APPLY-CREDIT-ADJ` has no exit paragraph and drops into `P3500-APPLY-DEBIT-ADJ`; callers use `PERFORM P3400 THRU P3500-EXIT` for a credit and `PERFORM P3500 THRU P3500-EXIT` for a debit; **20 STRING ASSEMBLY** — `P4600-BUILD-ADJ-TEXT` from four pieces; 04 redefines (6); 09 job parameters (MAXADJ, delegated authority) ; 21 rounding — adjustments `ROUNDED` into the 2dp header field |
| `CABBIL06.cbl` | **27 DORMANT FEATURE** — `P4800-CMDS-RESIDUAL-NET` implements the 1994 CMDS residual pool share and is behind `WS-PE-CMDS-RESID-SW`; the JCL substitutes N and has done since the agreement lapsed; **16 CROSS-APPLICATION ACCESS** — reads `TELCABS.SETL.NET`, owned by the settlement application; **21 ROUNDING** — the net is `ROUNDED` here and truncated by `CABBIL09`; 04 redefines (6); 09 job parameters (settlement period) |
| `CABBIL07.cbl` | **06 CONDITION RANGES** — `WS-TX-FEDERAL` VALUE 'FE' THRU 'FS' and `WS-TX-SURCHARGE` VALUE 'FS' THRU 'SU'; both true for FS and `P3400-SELECT-BASE` tests federal first, so an FS component is computed on the usage base rather than the charge base; **15 FILE DEFINITION JOBS** — `TELCABS.CABS.TAXRATE` is created only by `VSAM/CABVDEF1`; **21 ROUNDING** — each of the five components is `ROUNDED` separately and the rounded parts are added, which is not the same as rounding the sum; 04 redefines (6) |
| `CABBIL08.cbl` | **21 ROUNDING** — the make-up charge is a MOVE from a 5dp field into a 2dp field and therefore truncates, where the same style of computation is `ROUNDED` in `CABBIL05` and `CABBIL07`; 04 redefines (8) — two live card layouts; 09 job parameters (tariff minimum and maximum) |
| `CABBIL09.cbl` | **21 ROUNDING** — the 5dp accumulators are MOVEd into the 2dp header fields (truncation) while `BH-HASH-AMOUNT` keeps all five places; **07 VARIABLE-LENGTH RECORDS** — reads the VB detail and walks the ODO; 04 redefines (6); 09 job parameters (CDR factor) |
| `CABBIL10.cbl` | **15 FILE DEFINITION JOBS** — `TELCABS.CABS.HOLDRSN` is created only by `VSAM/CABVDEF3`; **10 GENERATION FILES** — reads the final header file at `(-1)` for the bill-to-bill variance test; 04 redefines (6); 09 job parameters (variance percent, delegated authority limit). |
| `CABBIL11.cbl` | **07 VARIABLE-LENGTH RECORDS** — ODO walk in `P3200`; **21 ROUNDING** — sums `BD-TOT-ROUNDED`, the 2dp figure `CABBIL02` produced with `ROUNDED`; 04 redefines (6). |
| `CABBIL12.cbl` | **20 STRING ASSEMBLY** — `P3200-BUILD-INVOICE-NBR` from five pieces with a pointer; **23 CHARACTER SCANNING** — `P3400-CHECK-CHARACTER` walks thirteen characters and builds a modulus-eleven weighted sum through two subscripted table searches; **15 FILE DEFINITION JOBS** — `TELCABS.CABS.INVCTL` is created only by `VSAM/CABVDEF2`; 04 redefines (6); 09 job parameters (prefix has no default) |

## JCL that drives this family

| Member | Purpose | Complexities |
|---|---|---|
| `JCL/CABS4000.jcl` | Bill trigger and cycle selection | 09 FORCSW/OCNFRM/OCNTHR no default, 11 CABPBILL, 13 four DD overrides, 14 CABSRT09 |
| `JCL/CABS4100.jcl` | Bill detail line assembly | 09 MAXELM no default, 11 CABPBDTL, 14 CABSRT10 summarisation rule |
| `JCL/CABS4150.jcl` | Bill section sequencing | 11 same PROC as CABS4100, 13 five DD overrides including three DUMMY |
| `JCL/CABS4200.jcl` | Prior balance and payments | 11 CABPBILL, 13, 09 AGEBAS |
| `JCL/CABS4250.jcl` | Adjustment application | 10 concatenated GDG (0) and (-1), 11 CABPBADJ, 13 SETLIN to DUMMY |
| `JCL/CABS4300.jcl` | Settlement netting | 10 reads SETL.NET at (-1), 11 same PROC as CABS4250, 16 |
| `JCL/CABS4350.jcl` | Minimum and maximum | 09 MINBIL/MAXBIL, 11, 13 |
| `JCL/CABS4400.jcl` | Tax calculation | 09 TAXDT/FEDRAT, 11, 13, 15 TAXRATE |
| `JCL/CABS4450.jcl` | Invoice header creation | 11, 13 three DD overrides |
| `JCL/CABS4500.jcl` | Pre-bill audit and hold | 10 PRIORIN at (-1), 11, 13, 15 HOLDRSN |
| `JCL/CABS4550.jcl` | Bill level balancing | 11 CABPBHDR, 13 four DUMMY overrides, 09 TOLER |
| `JCL/CABS4600.jcl` | Final invoice numbering | 11 same PROC as CABS4550, 13, 15 INVCTL, 09 PREFIX |
| `JCL/PROCS/CABPBILL.prc` | Generic bill calculation step | 11, 12 five-library STEPLIB with EMERG first |
| `JCL/PROCS/CABPBDTL.prc` | Detail assembly and sequencing | 11 used by CABS4100 and CABS4150, 09 |
| `JCL/PROCS/CABPBADJ.prc` | Adjustment and netting | 11 used by CABS4250 and CABS4300, 16 |
| `JCL/PROCS/CABPBHDR.prc` | Audit, balance and numbering | 11 used by CABS4500, CABS4550 and CABS4600, 10 |

## Seeded defects

The estate carries twelve deliberately seeded defects. Which programs carry them, in which paragraphs, what each one does, what it costs and what the correct modernization response is are recorded **only** in `SEALED/` — see `SEALED/defect_placements.json` and `SEALED/answer_key_*.json`. This manifest does not say whether this family carries one.
