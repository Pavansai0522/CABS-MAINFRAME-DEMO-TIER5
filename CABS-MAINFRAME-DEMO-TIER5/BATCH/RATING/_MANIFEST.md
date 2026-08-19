# CABS Tier 5 - RATING family manifest

Wholesale Carrier Access Billing System (CABS). Target: OS/VS COBOL 1974, Hercules TK4- / MVS 3.8j.
Complexity placements are authoritative in `CONTRACTS/complexity_placement.json`.
Seeded defects are recorded in `SEALED/` and are **not** annotated in the source, in this manifest or anywhere else outside `SEALED/`.

**14 programs, 15,416 lines, 2,827 comment lines (18%).**

| Program | Lines | Cmt | Cmt% | Purpose | Principal datasets |
|---|--:|--:|--:|---|---|
| `CABRAT01.cbl` | 637 | 104 | 16% | Rate table load and internal table build | RATEMST -> RTBLOUT, RPTOUT |
| `CABRAT02.cbl` | 2373 | 373 | 16% | Access rating driver and rate element dispatcher | RATIN + RATEMST -> RATOUT, BDTLOUT, SUSOUT |
| `CABRAT03.cbl` | 4290 | 1173 | 27% | Switched access rating - five rate elements per call | RATIN + RATEMST -> RATOUT, BDTLOUT, RPTOUT |
| `CABRAT04.cbl` | 870 | 124 | 14% | Special access fixed-rate application with proration | SPCIN + CIRCMST + RATEMST -> RATOUT, BDTLOUT |
| `CABRAT05.cbl` | 768 | 105 | 14% | Unbundled network element (UNE) rating | UNEIN + RATEMST -> RATOUT, SUSOUT |
| `CABRAT06.cbl` | 708 | 93 | 13% | Banded and volume rate selection | RATIN + RTBLIN -> BNDOUT |
| `CABRAT07.cbl` | 807 | 99 | 12% | Minimum and maximum charge application | RATIN -> RATOUT, MMXOUT |
| `CABRAT08.cbl` | 878 | 117 | 13% | Rate override handler | OVRIN + RATIN -> OVROUT |
| `CABRAT09.cbl` | 841 | 115 | 14% | Rating summary and multi-level aggregation | RATIN -> SUMOUT, RPTOUT |
| `CABRAT10.cbl` | 793 | 140 | 18% | Bill detail line construction (variable-length) | RATIN -> BDTLOUT, BDTLFIX |
| `CABRAT11.cbl` | 616 | 98 | 16% | Reciprocal compensation rating with ISP minute cap | RECIN + CARRMST -> RATOUT, SUSOUT |
| `CABRAT12.cbl` | 639 | 83 | 13% | Rating retry and positional audit trail | RATIN + AUDLOG -> RTRYOUT, AUDLOG, SUSOUT |
| `CABRAT13.cbl` | 465 | 85 | 18% | Operator services access rating | RATIN -> OPROUT |
| `CABRAT14.cbl` | 731 | 118 | 16% | Rate table audit and effective-date sweep | RATEMST + CTLIN -> RPTOUT |

## Complexities carried, by program

| Program | Complexities present (numbers refer to the 27-construct catalogue) |
|---|---|
| `CABRAT01.cbl` | 7 OCCURS DEPENDING ON producer (R2-RATE-TABLE + R3-BAND-POOL flattening), 8 nested copybooks (CABSRT01>02>03>04), 4 REDEFINES |
| `CABRAT02.cbl` | 2 dynamic call x4 (P4100 target from R2-EN-MODULE-SFX table lookup - statically unresolvable), 6 overlapping 88s (R1-ANY-LIVE-MODE/R1-ANY-TEST-MODE on 'L'), 7 variable-length BD-ELEMENT ODO, 19 inline pivot 70, 20 STRING/UNSTRING, 5 REDEFINES |
| `CABRAT03.cbl` | 2 dynamic call (P7200), 3 hidden error handler (P9990-RATE-FAILURE, GO TO from 3 sites), 5 RENAMES (WS-TARIFF-LOOKUP), 6 overlapping 88s (WS-EC-SWITCHED/WS-EC-TRANSPORT on 'TANSW '), 19 inline pivot 70, 20 STRING assembly, 24 internal SORT with INPUT/OUTPUT PROCEDURE, 11 REDEFINES |
| `CABRAT04.cbl` | 6 REDEFINES, mixed ROUNDED (proration) / truncating (term discount) COMPUTE |
| `CABRAT05.cbl` | 26 dead code (P6100-UNE-LOOP-REPRICE behind WS-LOOP-REPRICE-SW, never moved to), 4 REDEFINES |
| `CABRAT06.cbl` | 6 overlapping 88s (WS-BM-STEPPED/WS-BM-GRADUATED on 'G'), 7 ODO band walk, 3 REDEFINES |
| `CABRAT07.cbl` | Two-level break (circuit within BAN), make-up and credit lines, mixed ROUNDED/truncating COMPUTE, 4 REDEFINES |
| `CABRAT08.cbl` | 2 dynamic call (P3400-INVOKE-OVERRIDE-PGM), 20 UNSTRING of the 80-byte card + STRING audit key, 23 INSPECT + 80-cell column walk, 19 inline pivot 70, 5 REDEFINES |
| `CABRAT09.cbl` | 24 internal SORT with INPUT/OUTPUT PROCEDURE (eligibility and suppression rules hidden in the procedures), 5 RENAMES (WS-SUMM-GROUP), 6 overlapping 88s (WS-SL-DETAIL/WS-SL-GROUP on 2), 2 REDEFINES |
| `CABRAT10.cbl` | 7 SILENT TRUNCATION (P5200-MOVE-TO-FIXED moves VB 1647 group to PIC X(500)), 2 dynamic call (P4500-CALL-FORMATTER), 20 STRING assembly across 3 paragraphs, 23 INSPECT REPLACING/TALLYING + reverse byte walk, 4 REDEFINES |
| `CABRAT11.cbl` | Cap-straddle split arithmetic, per-carrier accumulator OCCURS 150, 3 REDEFINES |
| `CABRAT12.cbl` | 18 append writes (AUDLOG OPEN EXTEND; attempt count derived from physical position, not a stored field), 2 REDEFINES |
| `CABRAT13.cbl` | 27 DORMANT FEATURE - entire rating path behind R1-OPR-SVC-SW = 'N', never moved to; tariff withdrawn 1998, ~170 lines intact and unreachable; 2 dynamic call (P3300-CALL-OPR-MODULE) also unreachable, 2 REDEFINES |
| `CABRAT14.cbl` | 19 two-digit year (literal 70 inline x2 plus DW-PIVOT-YY; gap detection depends on the pivot), 20 STRING assembly, 5 REDEFINES |
