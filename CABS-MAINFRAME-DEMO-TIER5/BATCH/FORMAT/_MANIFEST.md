# BILL FORMAT AND PRINT FAMILY - MANIFEST

Turns the numbered invoice into print, tape and EDI output. The family owns `CABSPRNT` and therefore owns the carriage control character in column one, which carries business meaning to the burst and insert machinery as well as printer positioning.

Application code `CABS`. OS/VS COBOL 1974 throughout. Every program writes `CABS-CONTROL-RECORD` to DD `CTLOUT` in `P8000-CONTROL`.

**9 programs, 8,081 lines, 1,694 comment lines (21%).**

| Program | Lines | Cmt | Purpose | Principal datasets |
|---|--:|--:|---|---|
| `CABFMT01.cbl` | 1086 | 231 | Invoice page and section formatting — owns the carriage control assignment and writes one print control record per invoice for the mailroom | BDTLIN + BHDRIN -> PRTOUT, PRTCTL |
| `CABFMT02.cbl` | 870 | 188 | Heading and continuation page injection — six-line block on the first page of an invoice, short block with the continuation wording after it | PRTIN -> PRTOUT |
| `CABFMT03.cbl` | 850 | 188 | Amount and quantity editing — five `PIC Z` / `PIC -` edit patterns, the CR-tag form for two carriers, and one line per rate element under the summary line | BDTLIN -> PRTOUT |
| `CABFMT04.cbl` | 745 | 166 | Section and invoice total lines — breaks are found from the carriage control character, not from a data field | PRTIN -> PRTOUT |
| `CABFMT05.cbl` | 890 | 184 | Invoice summary page builder — the nine component lines in tariff order plus the jurisdictional split, proved against the total due | BHDRIN -> PRTOUT |
| `CABFMT06.cbl` | 999 | 191 | EDI 811 flat file production — the estate has no translator, so the ISA/BIG/N1/IT1/TDS/CTT/IEA segments are assembled here | BDTLIN + BHDRIN -> EDIOUT |
| `CABFMT07.cbl` | 964 | 192 | Tape and media extract — four record types distinguished by byte one, with a label and trailer carrying the count and hash for despatch reconciliation | BDTLIN + BHDRIN -> MEDOUT |
| `CABFMT08.cbl` | 831 | 175 | Print control file writer — counts every carriage control character and writes one control record per physical document | PRTIN -> DOCCTL |
| `CABFMT09.cbl` | 846 | 179 | Bill message insert page — the marketing insert at the back of the bill | BHDRIN + MSGIN -> PRTOUT |

## Complexities carried, by program

| Program | Complexities present |
|---|---|
| `CABFMT01.cbl` | **03 HIDDEN ERROR HANDLER** — `P9980-PRINT-FAILURE` at the physical bottom, reached by `GO TO` from `P2100`, `P3400`, `P3600` and `P4000`; **22 PRINT CONTROL** — `'7'` is both skip-to-channel-7 and start-of-invoice, `'4'` is both skip-to-channel-4 and start-of-bill-section, and the section count on the print control record is what the mailroom reconciles against; **07 VARIABLE-LENGTH RECORDS** — reads the VB detail; 04 redefines (6); 09 job parameters (lines per page has no default). |
| `CABFMT02.cbl` | **20 STRING ASSEMBLY** — `P3200-BUILD-TITLE` from five pieces with a pointer, `P4000` and `P4100` build the period and due-date text; **22 PRINT CONTROL** — the boundary is detected from the incoming carriage control and the triggering line is demoted to single spacing unless it is a section break; 04 redefines (6); 09 job parameters (company name has no default) |
| `CABFMT03.cbl` | **22 PRINT CONTROL** — `'+'` overprint suppress puts the first element line on the same physical line as the summary in the compressed form; **23 CHARACTER SCANNING** — `P3600-SCAN-EDITED` uses INSPECT TALLYING for leading blanks, separators and suppressed zeros and then walks the edited field a character at a time; **07 VARIABLE-LENGTH RECORDS** — ODO walk in `P3500`; 04 redefines (7); 09 job parameters (edit style) |
| `CABFMT04.cbl` | **01 FALL-THROUGH** — `P3200-SECTION-BREAK-MINOR` has no exit paragraph; it ends at `P3200-CLEAR` and drops into `P3300-SECTION-BREAK-MAJOR`. A section break uses `PERFORM P3200 THRU P3200-CLEAR`; an invoice break uses `PERFORM P3200 THRU P3300-EXIT`; **22 PRINT CONTROL**; 04 redefines (6) |
| `CABFMT05.cbl` | **06 CONDITION RANGES** — `WS-SP-SUMMARY` VALUE 'S' THRU 'R' and `WS-SP-RECAP` VALUE 'R' THRU 'Z'; both are true for style R and `P3000` tests summary first; **21 ROUNDING** — the tax line is shown to the whole dollar by a MOVE into an integer field, truncating the figure `CABBIL07` rounded to the cent; **22 PRINT CONTROL** — the summary page carries `'7'` because it is a separate document merged by the inserter; 04 redefines (6) |
| `CABFMT06.cbl` | **20 STRING ASSEMBLY** — every EDI segment is built by `STRING` with a pointer; **23 CHARACTER SCANNING** — `P4400-SCAN-DELIMITERS` uses INSPECT TALLYING and REPLACING to find and neutralise a separator inside a free-text description, and `P4200` UNSTRINGs the description into three elements; 04 redefines (7); 09 job parameters (EDI version and sender id have no default) |
| `CABFMT07.cbl` | **26 DEAD CODE** — `P5400-MICROFICHE-BLOCK` blocks the extract into fiche frames and writes a frame index; it is compiled, complete and never PERFORMed — the bureau contract ended in 2001; **07 VARIABLE-LENGTH RECORDS** — reads the VB detail and moves the first ten elements into a fixed 240-byte area; 04 redefines (8); 09 job parameters (volume serial) |
| `CABFMT08.cbl` | **22 PRINT CONTROL** — the whole program exists to count and describe the carriage control characters; an unrecognised character is counted and reported because it stops the burster mid-run; 04 redefines (6); 09 job parameters (form id) |
| `CABFMT09.cbl` | **27 DORMANT FEATURE** — the entire program is inert. `P1400-FEATURE-CHECK` requires both the insert switch and a campaign code; `CABS5500` STEP020 substitutes N and a blank campaign, and has done since the last campaign in 2009. The job runs every cycle and produces an empty print file; 04 redefines (7); 22 print control |

## JCL that drives this family

| Member | Purpose | Complexities |
|---|---|---|
| `JCL/CABS5000.jcl` | Page format and heading injection | 11 CABPFMTP twice, 13 five DD overrides, 09 LINPGE/COMPNY |
| `JCL/CABS5100.jcl` | Amount editing and totals | 11 same PROC twice, 13, 09 EDSTYL |
| `JCL/CABS5200.jcl` | Summary pages | 11, 13 two DUMMY overrides, 09 SUMSTL |
| `JCL/CABS5300.jcl` | EDI 811 interchange | 09 EDIVER/SENDID, 11 CABPFMTM, 14 CABSRT13 partner filter |
| `JCL/CABS5400.jcl` | Tape and media extract | 09 VOLSER, 11 same PROC, 13 MEDOUT overridden onto tape |
| `JCL/CABS5500.jcl` | Print control and message insert | 11 same PROC twice, 13 six DUMMY overrides, 27 dormant |
| `JCL/PROCS/CABPFMTP.prc` | Print formatting fragment | 11 used by CABS5000/5100/5200, 12 five-library STEPLIB, 09 |
| `JCL/PROCS/CABPFMTM.prc` | Media, EDI and control fragment | 11 used by CABS5300/5400/5500, 09 |
| `JCL/CTLCARDS/CABSRT12.ctl` | Print stream mail sort | 14 E15 and E35 exits; the suppression of partially formatted invoices exists only in the exit |
| `JCL/CTLCARDS/CABSRT14.ctl` | Media extract despatch sort | 14 OUTFIL OMIT drops zero-value details that the trailer hash still includes |

## Seeded defects

The estate carries twelve deliberately seeded defects. Which programs carry them, in which paragraphs, what each one does, what it costs and what the correct modernization response is are recorded **only** in `SEALED/` — see `SEALED/defect_placements.json` and `SEALED/answer_key_*.json`. This manifest does not say whether this family carries one.
