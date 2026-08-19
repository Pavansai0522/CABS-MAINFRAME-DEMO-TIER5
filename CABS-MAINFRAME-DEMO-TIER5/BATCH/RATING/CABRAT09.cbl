      ******************************************************************
      * CABRAT09 - RATING SUMMARY AND AGGREGATION                      *
      * APPLICATION : CABS                                             *
      * INPUTS      : DDNAME  DSN                     COPYBOOK         *
      *               RATIN   TELCABS.CABS.RATED(0)    (LOCAL)         *
      * OUTPUTS     : DDNAME  DSN                     COPYBOOK         *
      *               SUMOUT  TELCABS.CABS.SUMMARY(+1) (LOCAL)         *
      *               RPTOUT  SYSOUT CLASS A            CABSPRNT       *
      * CONTROL     : CTLOUT                          CABSCTL          *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +             *
      *               CT-SUMMARISED + CT-CARRIED-FWD                   *
      * RESTART     : FULL RERUN                                       *
      * REVISION HISTORY                                               *
      *   V1.00 1991-02-18 R.T.WHEELER  INITIAL - OCN TOTALS ONLY,     *
      *                    NO SORT, SINGLE PASS IN OCN ORDER           *
      *   V1.03 1995-07-09 D.OKONKWO    BAN AND JURISDICTION BREAK     *
      *                    LEVELS ADDED, INTERNAL SORT INTRODUCED      *
      *   V1.06 1999-11-30 J.M.CASTILLO ELEMENT AND STATE TOTAL        *
      *                    TABLES ADDED FOR THE SUMMARY REPORT         *
      *   V1.09 2004-05-14 P.NAIR       LOW-VALUE THRESHOLD ADDED -    *
      *                    SUB-CENT AMOUNTS FOLD TO A RESIDUAL LINE    *
      *   V1.12 2009-09-21 A.BUKOWSKI   DISPUTED-CARRIER FOLD-IN       *
      *                    ADDED TO THE SAME RESIDUAL RULE             *
      *   V1.15 2014-01-30 S.MARCHETTI  ZERO-MINUTE SUPPRESSION        *
      *                    ADDED PER RECONCILIATION FINDING RC-77      *
      *   V1.18 2018-10-02 T.VANCE      RECOMPILE ONLY - LE V8         *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABRAT09.
       AUTHOR. TELCABS APPLICATIONS - RATING TEAM.
      ******************************************************************
      * RATING SUMMARY AND AGGREGATION.  SORTS RATIN INTO OCN/BAN/     *
      * JURISDICTION/ELEMENT SEQUENCE AND WRITES ONE SUMOUT ROW PER    *
      * GROUP, PLUS A MULTI-LEVEL PRINTED REPORT WITH SEPARATE         *
      * RUN-WIDE ELEMENT AND STATE TOTAL TABLES.                       *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RATIN ASSIGN TO UT-S-RATIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT SUMOUT ASSIGN TO UT-S-SUMOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT CTLOUT ASSIGN TO UT-S-CTLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CONTROL.
           SELECT RPTOUT ASSIGN TO UT-S-RPTOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT SORTWK ASSIGN TO SORTWK1.
       DATA DIVISION.
       FILE SECTION.
      * RATIN - RATED-ELEMENT INPUT.  SAME 200-BYTE LOCAL SHAPE        *
      * USED ACROSS THE CABRAT07/08/09 FAMILY.                         *
       FD  RATIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       01  CABS-RATIN-RECORD.
           05  RI-OCN                      PIC X(04).
           05  RI-BAN                      PIC X(13).
           05  RI-BILL-PERIOD              PIC 9(06).
           05  RI-SECTION                  PIC X(02).
           05  RI-SEQ-NBR                  PIC 9(09) COMP-3.
           05  RI-CIRCUIT-ID               PIC X(20).
           05  RI-JURIS-CD                 PIC X(01).
           05  RI-STATE-CD                 PIC X(02).
           05  RI-RATE-ELEM                PIC X(06).
           05  RI-QTY                      PIC S9(13)V9(02)
               COMP-3.
           05  RI-RATE                     PIC S9(05)V9(05)
               COMP-3.
           05  RI-AMOUNT                   PIC S9(11)V9(05)
               COMP-3.
           05  RI-ROUND-RULE               PIC X(01).
           05  RI-SRC-PROCESS              PIC X(08).
           05  RI-LINE-TYPE                PIC X(01).
           05  RI-DESCRIPTION              PIC X(60).
           05  RI-AUTH-REF                 PIC X(20).
           05  RI-FILLER                   PIC X(28).
      * SUMOUT - ONE ROW PER OCN/BAN/JURISDICTION/ELEMENT GROUP,       *
      * PLUS ONE TRAILING RESIDUAL ROW.  LOCAL LAYOUT, FB 200.         *
       FD  SUMOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       01  CABS-SUMOUT-RECORD.
           05  SO-OCN                      PIC X(04).
           05  SO-BAN                      PIC X(13).
           05  SO-JURIS-CD                 PIC X(01).
           05  SO-RATE-ELEM                PIC X(06).
           05  SO-STATE-CD                 PIC X(02).
           05  SO-BILL-PERIOD              PIC 9(06).
           05  SO-REC-COUNT                PIC S9(07) COMP-3.
           05  SO-TOT-MINUTES              PIC S9(13)V9(02)
               COMP-3.
           05  SO-GROSS-AMOUNT             PIC S9(13)V9(05)
               COMP-3.
           05  SO-ROUND-RESIDUE            PIC S9(07)V9(05)
               COMP-3.
           05  SO-SUMM-LEVEL               PIC 9(01).
           05  SO-RESIDUAL-SW              PIC X(01).
           05  SO-FILLER                   PIC X(133).
      * CTLOUT - RUN CONTROL / BALANCING RECORD.                       *
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD             PIC X(180).
      * RPTOUT - MULTI-LEVEL PRINTED SUMMARY REPORT.                   *
       FD  RPTOUT
           RECORDING MODE IS FA
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       COPY CABSPRNT.
      * SORTWK - ONE RECORD PER ELIGIBLE RATED ELEMENT.                *
      * WS-SUMM-GROUP RENAMES THE OCN/BAN/JURIS SPAN SO P4530          *
      * CAN TEST "DID ANY OF THE OUTER THREE LEVELS CHANGE" IN ONE     *
      * COMPARE, WHILE THE REAL ELEMENTARY ITEMS REMAIN WS-SM-*.       *
       SD  SORTWK
           RECORD CONTAINS 60 CHARACTERS.
       01  WS-SORT-RECORD.
           05  WS-SM-OCN                   PIC X(04).
           05  WS-SM-BAN                   PIC X(13).
           05  WS-SM-JURIS                 PIC X(01).
           66  WS-SUMM-GROUP RENAMES WS-SM-OCN THRU WS-SM-JURIS.
           05  WS-SM-RATE-ELEM             PIC X(06).
           05  WS-SM-STATE-CD              PIC X(02).
           05  WS-SM-BILL-PERIOD           PIC 9(06).
           05  WS-SM-QTY                   PIC S9(13)V9(02)
               COMP-3.
           05  WS-SM-AMOUNT                PIC S9(11)V9(05)
               COMP-3.
           05  WS-SM-ROUND-RULE            PIC X(01).
           05  WS-SM-FILLER                PIC X(10).
       WORKING-STORAGE SECTION.
      * STANDARD SHARED WORKING STORAGE - SEE CABSWRK.                 *
       COPY CABSWRK.
      * RATING FAMILY CONTROL BLOCKS - ONE COPY PULLS ALL FOUR.        *
       COPY CABSRT01.
      * PROGRAM CONSTANTS / SYSIN PARM CARD.                           *
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE
               'CABRAT09'.
           05  WS-LOW-VALUE-THRESHOLD     PIC S9(03)V9(02)
               COMP-3 VALUE 0.01.
       01  WS-PARM-CARD                PIC X(80).
       01  WS-PARM-CARD-R1 REDEFINES WS-PARM-CARD.
           05  PC1-CYCLE-YYDDD             PIC 9(05).
           05  PC1-BILL-PERIOD             PIC 9(06).
           05  PC1-TARIFF-CD               PIC X(04).
           05  PC1-RUN-ID                  PIC X(12).
           05  PC1-FILLER                  PIC X(53).
      * DISPUTED-CARRIER SEED TABLE - EMBEDDED LITERAL, NO DEDICATED   *
      * FILE.  SAME PATTERN CABRAT03 USES FOR ITS V AND H TABLE.       *
       01  WS-DISPUTE-OCN-SEED.
           05  FILLER PIC X(24) VALUE
               '009100984471520366907788'.
       01  WS-DISPUTE-SEED-R REDEFINES WS-DISPUTE-OCN-SEED.
           05  WS-DS-ENTRY OCCURS 6 TIMES PIC X(04).
       01  WS-DS-X                     PIC S9(02) COMP-3
           VALUE 0.
       01  WS-DS-FOUND-SW              PIC X(01) VALUE 'N'.
           88  WS-DS-FOUND                 VALUE 'Y'.
      * PRIOR-KEY SAVE AREA FOR THE FOUR-LEVEL BREAK.  WS-PK-SUMM-     *
      * GROUP-SAVE IS THE SAVE AREA COMPARED AGAINST WS-SUMM-GROUP.    *
       01  WS-PREV-KEY-SAVE.
           05  WS-PK-OCN                   PIC X(04).
           05  WS-PK-BAN                   PIC X(13).
           05  WS-PK-JURIS                 PIC X(01).
           05  WS-PK-SUMM-GROUP-SAVE       PIC X(18).
           05  WS-PK-ELEM                  PIC X(06).
           05  WS-PK-STATE                 PIC X(02).
           05  WS-PK-BILL-PERIOD           PIC 9(06).
           05  WS-PK-ROUND-RULE            PIC X(01).
           05  WS-PK-FIRST-SW              PIC X(01) VALUE 'Y'.
               88  WS-PK-FIRST-RECORD          VALUE 'Y'.
           05  WS-BREAK-LEVEL-FOUND        PIC 9(01) VALUE 0.
           05  WS-QUICK-DIFF-SW            PIC X(01) VALUE 'N'.
      * CABS-STD-006: OVERLAPPING 88-LEVELS.  WS-SL-DETAIL AND         *
      * WS-SL-GROUP ARE BOTH TRUE FOR VALUE 2.  P8250-PRINT-GROUP-     *
      * LINE TESTS WS-SL-DETAIL FIRST, SO A BAN- OR JURISDICTION-      *
      * LEVEL CLOSE (WS-SUMM-LEVEL = 2) ALWAYS PRINTS AS A PLAIN       *
      * DETAIL-CONTINUATION LINE, NEVER AS A GROUP SUBTOTAL LINE.      *
      * THIS HAS BEEN TRUE SINCE V1.15 AND NO ONE HAS ASKED FOR IT     *
      * TO CHANGE.                                                     *
       01  WS-SUMM-LEVEL-AREA.
           05  WS-SUMM-LEVEL               PIC 9(01) VALUE 1.
               88  WS-SL-DETAIL                VALUE 1 2.
               88  WS-SL-GROUP                 VALUE 2 3.
      * OPEN-GROUP ACCUMULATOR - THE CURRENT OCN/BAN/JURIS/ELEMENT     *
      * COMBINATION, RESET AT EVERY BREAK.                             *
       01  WS-GROUP-ACCUM.
           05  WS-GRP-REC-CNT              PIC S9(07) COMP-3
               VALUE 0.
           05  WS-GRP-MINUTES              PIC S9(13)V9(02)
               COMP-3 VALUE 0.
           05  WS-GRP-AMOUNT               PIC S9(13)V9(05)
               COMP-3 VALUE 0.
           05  WS-GRP-ROUND-RESIDUE        PIC S9(07)V9(05)
               COMP-3 VALUE 0.
      * RUN-WIDE ELEMENT AND STATE TOTAL TABLES - INSERT-ON-MISS,      *
      * LINEAR SEARCH, NOT TIED TO THE SORT.                           *
       01  WS-ELEM-TOTALS-TABLE.
           05  WS-ET-CNT                   PIC 9(03) VALUE 0.
           05  WS-ET-ENTRY OCCURS 1 TO 50 TIMES
               DEPENDING ON WS-ET-CNT
               INDEXED BY WS-ET-X.
               10  WS-ET-RATE-ELEM             PIC X(06).
               10  WS-ET-REC-CNT               PIC S9(09) COMP-3
                   VALUE 0.
               10  WS-ET-MINUTES               PIC S9(13)V9(02)
                   COMP-3 VALUE 0.
               10  WS-ET-AMOUNT                PIC S9(13)V9(05)
                   COMP-3 VALUE 0.
       01  WS-STATE-TOTALS-TABLE.
           05  WS-ST-CNT                   PIC 9(03) VALUE 0.
           05  WS-ST-ENTRY OCCURS 1 TO 60 TIMES
               DEPENDING ON WS-ST-CNT
               INDEXED BY WS-ST-X.
               10  WS-ST-STATE-CD              PIC X(02).
               10  WS-ST-REC-CNT               PIC S9(09) COMP-3
                   VALUE 0.
               10  WS-ST-MINUTES               PIC S9(13)V9(02)
                   COMP-3 VALUE 0.
               10  WS-ST-AMOUNT                PIC S9(13)V9(05)
                   COMP-3 VALUE 0.
       01  WS-TABLE-SEARCH-WORK.
           05  WS-TBL-FOUND-SW             PIC X(01) VALUE 'N'.
               88  WS-TBL-FOUND                VALUE 'Y'.
      * RESIDUAL BUCKET - LOW-VALUE AND DISPUTED-CARRIER RECORDS       *
      * FOLD IN HERE INSTEAD OF BEING SUMMARISED INDIVIDUALLY.  THE    *
      * RULE LIVES ONLY IN P4030 - NOWHERE ELSE IN THE PROGRAM.        *
       01  WS-RESIDUAL-BUCKET.
           05  WS-RB-REC-CNT               PIC S9(09) COMP-3
               VALUE 0.
           05  WS-RB-MINUTES               PIC S9(13)V9(02)
               COMP-3 VALUE 0.
           05  WS-RB-AMOUNT                PIC S9(13)V9(05)
               COMP-3 VALUE 0.
           05  WS-RB-LOW-VALUE-CNT         PIC S9(09) COMP-3
               VALUE 0.
           05  WS-RB-DISPUTE-CNT           PIC S9(09) COMP-3
               VALUE 0.
      * SORT CONTROL SWITCHES AND MISC RUN COUNTERS.                   *
       01  WS-SORT-CONTROL.
           05  WS-SORT-EOF-SW              PIC X(01) VALUE 'N'.
               88  WS-SORT-EOF                 VALUE 'Y'.
           05  WS-SORT-INPUT-EOF-SW        PIC X(01) VALUE 'N'.
               88  WS-SORT-INPUT-EOF           VALUE 'Y'.
       01  WS-MISC-COUNTERS.
           05  WS-MC-RELEASED              PIC S9(09) COMP-3
               VALUE 0.
           05  WS-MC-RETURNED              PIC S9(09) COMP-3
               VALUE 0.
           05  WS-MC-GROUPS-WRITTEN        PIC S9(09) COMP-3
               VALUE 0.
           05  WS-MC-GROUPS-SUPPRESSED     PIC S9(09) COMP-3
               VALUE 0.
           05  WS-MC-OCN-BREAKS            PIC S9(09) COMP-3
               VALUE 0.
      * ABEND WORK, CALL RETURN CODES.                                 *
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9909.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-HASH                  PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
      * REPORT PAGE/LINE CONTROL AND EDIT WORK.                        *
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(05) COMP-3
               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(05) COMP-3
               VALUE 0.
           05  WS-RPT-LINES-PER-PAGE       PIC S9(03) COMP-3
               VALUE 55.
      * GROUP-CLOSE SUPPRESSION SWITCH - P4540.                        *
       01  WS-SUPPRESS-WORK.
           05  WS-MC-SUPPRESS-SW           PIC X(01) VALUE 'N'.
      * ELIGIBILITY-TEST RESULT SWITCH - P4030.                        *
       01  WS-ELIGIBILITY-WORK.
           05  WS-CE-ELIGIBLE-SW           PIC X(01) VALUE 'Y'.
               88  WS-CE-ELIGIBLE              VALUE 'Y'.
       PROCEDURE DIVISION.
      * P0000-MAINLINE - P2000-PROCESS IS A SINGLE SORT VERB, SO THE   *
      * UNTIL WS-EOF LOOP RUNS EXACTLY ONCE.                           *
       P0000-MAINLINE.
           PERFORM P1000-INIT THRU P1000-EXIT.
           PERFORM P2000-PROCESS THRU P2000-EXIT UNTIL WS-EOF.
           PERFORM P8000-CONTROL THRU P8000-EXIT.
           PERFORM P9000-TERM THRU P9000-EXIT.
           STOP RUN.
      * S100-INITIALISATION SECTION                                    *
       S100-INITIALISATION SECTION.
       P1000-INIT.
           PERFORM P1100-OPEN-FILES THRU P1100-EXIT.
           PERFORM P1200-READ-PARM THRU P1200-EXIT.
           PERFORM P1700-INIT-COUNTERS THRU P1700-EXIT.
       P1000-EXIT.
           EXIT.
       P1100-OPEN-FILES.
           OPEN INPUT RATIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RATIN OPEN FAILED' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SUMOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'SUMOUT OPEN FAILED' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CTLOUT OPEN FAILED' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RPTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RPTOUT OPEN FAILED' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
       P1100-EXIT.
           EXIT.
       P1200-READ-PARM.
           MOVE SPACES TO WS-PARM-CARD.
           ACCEPT WS-PARM-CARD FROM SYSIN.
           CALL 'CABPARMR' USING WS-PARM-CARD WS-RC-PARMR.
           MOVE PC1-CYCLE-YYDDD TO R1-CYCLE-YYDDD.
           MOVE PC1-BILL-PERIOD TO R1-BILL-PERIOD.
           MOVE PC1-TARIFF-CD TO R1-TARIFF-CD.
           MOVE PC1-RUN-ID TO R1-RUN-ID.
           MOVE PC1-CYCLE-YYDDD TO DW-CURRENT-YYDDD.
           CALL 'CABDTCNV' USING DW-CURRENT-YYDDD DW-GREG-DATE
               WS-RC-DTCNV.
       P1200-EXIT.
           EXIT.
       P1700-INIT-COUNTERS.
           MOVE 0 TO WS-READ-CNT WS-WRITE-CNT WS-REJECT-CNT
               WS-SUMM-CNT WS-CFWD-CNT.
           MOVE 0 TO WS-ACC-MINUTES WS-ACC-AMOUNT WS-ACC-SEQ-HASH
               WS-ACC-OCN-HASH.
           MOVE 0 TO WS-MC-RELEASED WS-MC-RETURNED
               WS-MC-GROUPS-WRITTEN WS-MC-GROUPS-SUPPRESSED
               WS-MC-OCN-BREAKS.
       P1700-EXIT.
           EXIT.
      * S200-MAIN-PROCESS SECTION - THE SORT VERB ITSELF.              *
       S200-MAIN-PROCESS SECTION.
       P2000-PROCESS.
           SORT SORTWK
               ON ASCENDING KEY WS-SM-OCN WS-SM-BAN WS-SM-JURIS
                   WS-SM-RATE-ELEM
               INPUT PROCEDURE IS S400-SUMM-INPUT
               OUTPUT PROCEDURE IS S450-SUMM-OUTPUT.
           MOVE 'Y' TO WS-EOF-SW.
       P2000-EXIT.
           EXIT.
      * S400-SUMM-INPUT SECTION - THE SORT INPUT PROCEDURE.  READS     *
      * ALL OF RATIN AND RELEASES ONE SORTWK ROW PER ELIGIBLE          *
      * RECORD.  THE SUMMARISATION ELIGIBILITY RULE (P4030) LIVES      *
      * ONLY HERE - NOWHERE ELSE IN THE PROGRAM KNOWS IT.              *
       S400-SUMM-INPUT SECTION.
       P4000-SUMM-INPUT-CTL.
           MOVE 'N' TO WS-SORT-INPUT-EOF-SW.
           PERFORM P4010-READ-RATIN THRU P4010-EXIT.
           PERFORM P4020-PROCESS-ONE-RECORD THRU P4020-EXIT
               UNTIL WS-SORT-INPUT-EOF.
           GO TO P4900-SUMM-INPUT-DONE.
       P4010-READ-RATIN.
           READ RATIN
               AT END MOVE 'Y' TO WS-SORT-INPUT-EOF-SW.
           IF NOT WS-SORT-INPUT-EOF
               ADD 1 TO WS-READ-CNT
               CALL 'CABHASH' USING RI-OCN WS-ACC-OCN-HASH
                   ON EXCEPTION MOVE 9999 TO WS-RC-HASH
                   NOT ON EXCEPTION MOVE 0 TO WS-RC-HASH
               ADD RI-QTY TO WS-ACC-MINUTES
               ADD RI-AMOUNT TO WS-ACC-AMOUNT
               ADD RI-SEQ-NBR TO WS-ACC-SEQ-HASH.
       P4010-EXIT.
           EXIT.
       P4020-PROCESS-ONE-RECORD.
           PERFORM P4030-CHECK-ELIGIBILITY THRU P4030-EXIT.
           IF WS-CE-ELIGIBLE
               PERFORM P4040-RELEASE-DETAIL THRU P4040-EXIT
           ELSE
               PERFORM P4050-ACCUM-RESIDUAL THRU P4050-EXIT.
           PERFORM P4010-READ-RATIN THRU P4010-EXIT.
       P4020-EXIT.
           EXIT.
      * P4030 - THE ELIGIBILITY TEST.  BELOW THE 0.01 THRESHOLD OR     *
      * A DISPUTED CARRIER MEANS "FOLD, DO NOT SUMMARISE".             *
       P4030-CHECK-ELIGIBILITY.
           MOVE 'Y' TO WS-CE-ELIGIBLE-SW.
           IF RI-AMOUNT < WS-LOW-VALUE-THRESHOLD
               MOVE 'N' TO WS-CE-ELIGIBLE-SW
               ADD 1 TO WS-RB-LOW-VALUE-CNT.
           PERFORM P4035-CHECK-DISPUTE-OCN THRU P4035-EXIT.
           IF WS-DS-FOUND
               MOVE 'N' TO WS-CE-ELIGIBLE-SW
               ADD 1 TO WS-RB-DISPUTE-CNT.
       P4030-EXIT.
           EXIT.
       P4035-CHECK-DISPUTE-OCN.
           MOVE 'N' TO WS-DS-FOUND-SW.
           PERFORM P4037-TEST-ONE-DISPUTE-OCN THRU P4037-EXIT
               VARYING WS-DS-X FROM 1 BY 1
                   UNTIL WS-DS-X > 6 OR WS-DS-FOUND.
       P4035-EXIT.
           EXIT.
       P4037-TEST-ONE-DISPUTE-OCN.
           IF RI-OCN = WS-DS-ENTRY (WS-DS-X)
               MOVE 'Y' TO WS-DS-FOUND-SW.
       P4037-EXIT.
           EXIT.
       P4040-RELEASE-DETAIL.
           MOVE RI-OCN TO WS-SM-OCN.
           MOVE RI-BAN TO WS-SM-BAN.
           MOVE RI-JURIS-CD TO WS-SM-JURIS.
           MOVE RI-RATE-ELEM TO WS-SM-RATE-ELEM.
           MOVE RI-STATE-CD TO WS-SM-STATE-CD.
           MOVE RI-BILL-PERIOD TO WS-SM-BILL-PERIOD.
           MOVE RI-QTY TO WS-SM-QTY.
           MOVE RI-AMOUNT TO WS-SM-AMOUNT.
           MOVE RI-ROUND-RULE TO WS-SM-ROUND-RULE.
           MOVE SPACES TO WS-SM-FILLER.
           RELEASE WS-SORT-RECORD.
           ADD 1 TO WS-MC-RELEASED.
       P4040-EXIT.
           EXIT.
       P4050-ACCUM-RESIDUAL.
           ADD 1 TO WS-RB-REC-CNT.
           ADD RI-QTY TO WS-RB-MINUTES.
           ADD RI-AMOUNT TO WS-RB-AMOUNT.
           ADD 1 TO WS-SUMM-CNT.
       P4050-EXIT.
           EXIT.
       P4900-SUMM-INPUT-DONE.
           DISPLAY 'CABRAT09 - SORT INPUT COMPLETE - READ '
               WS-READ-CNT.
      * S450-SUMM-OUTPUT SECTION - THE SORT OUTPUT PROCEDURE.  THE     *
      * FOUR-LEVEL BREAK (OCN/BAN/JURISDICTION/ELEMENT) AND THE        *
      * ZERO-MINUTE SUPPRESSION RULE BOTH LIVE ONLY HERE.              *
       S450-SUMM-OUTPUT SECTION.
       P4500-SUMM-OUTPUT-CTL.
           MOVE 'N' TO WS-SORT-EOF-SW.
           PERFORM P4510-RETURN-NEXT THRU P4510-EXIT.
           PERFORM P4520-PROCESS-RETURNED-REC THRU P4520-EXIT
               UNTIL WS-SORT-EOF.
           IF NOT WS-PK-FIRST-RECORD
               PERFORM P4540-CLOSE-ELEM-GROUP THRU P4540-EXIT.
           PERFORM P4590-WRITE-RESIDUAL-LINE THRU P4590-EXIT.
           GO TO P4990-SUMM-OUTPUT-DONE.
       P4510-RETURN-NEXT.
           RETURN SORTWK
               AT END MOVE 'Y' TO WS-SORT-EOF-SW.
           IF NOT WS-SORT-EOF
               ADD 1 TO WS-MC-RETURNED.
       P4510-EXIT.
           EXIT.
       P4520-PROCESS-RETURNED-REC.
           PERFORM P4530-DETECT-BREAK-LEVEL THRU P4530-EXIT.
           IF WS-BREAK-LEVEL-FOUND > 0 AND NOT WS-PK-FIRST-RECORD
               PERFORM P4540-CLOSE-ELEM-GROUP THRU P4540-EXIT.
           IF WS-BREAK-LEVEL-FOUND > 0
               PERFORM P4550-START-NEW-GROUP THRU P4550-EXIT.
           PERFORM P4560-ACCUM-ELEMENT THRU P4560-EXIT.
           PERFORM P4570-ACCUM-ELEM-TABLE THRU P4570-EXIT.
           PERFORM P4580-ACCUM-STATE-TABLE THRU P4580-EXIT.
           MOVE WS-SUMM-GROUP TO WS-PK-SUMM-GROUP-SAVE.
           MOVE WS-SM-OCN TO WS-PK-OCN.
           MOVE WS-SM-BAN TO WS-PK-BAN.
           MOVE WS-SM-JURIS TO WS-PK-JURIS.
           MOVE WS-SM-RATE-ELEM TO WS-PK-ELEM.
           MOVE 'N' TO WS-PK-FIRST-SW.
           PERFORM P4510-RETURN-NEXT THRU P4510-EXIT.
       P4520-EXIT.
           EXIT.
      * P4530 - CABS-STD-005.  THE FAST PATH COMPARES WS-SUMM-GROUP    *
      * (THE 66-LEVEL RENAMES SPANNING OCN THRU JURIS) AGAINST THE     *
      * SAVE AREA IN ONE TEST; ONLY IF THAT DIFFERS DOES P4535 DRILL   *
      * DOWN TO FIND WHICH OF THE THREE OUTER LEVELS ACTUALLY BROKE.   *
      * SEQUENTIAL FLAG STYLE, NOT NESTED IF/ELSE, SO EVERY BRANCH     *
      * STAYS UNAMBIGUOUS UNDER THE 1974 PERIOD RULES.                 *
       P4530-DETECT-BREAK-LEVEL.
           MOVE 0 TO WS-BREAK-LEVEL-FOUND.
           MOVE 'N' TO WS-QUICK-DIFF-SW.
           IF WS-PK-FIRST-RECORD
               MOVE 1 TO WS-BREAK-LEVEL-FOUND.
           IF NOT WS-PK-FIRST-RECORD AND
                   WS-SUMM-GROUP NOT = WS-PK-SUMM-GROUP-SAVE
               MOVE 'Y' TO WS-QUICK-DIFF-SW.
           IF WS-QUICK-DIFF-SW = 'Y'
               PERFORM P4535-FIND-OUTER-LEVEL THRU P4535-EXIT.
           IF NOT WS-PK-FIRST-RECORD AND WS-QUICK-DIFF-SW = 'N' AND
                   WS-SM-RATE-ELEM NOT = WS-PK-ELEM
               MOVE 4 TO WS-BREAK-LEVEL-FOUND.
       P4530-EXIT.
           EXIT.
       P4535-FIND-OUTER-LEVEL.
           IF WS-SM-OCN NOT = WS-PK-OCN
               MOVE 1 TO WS-BREAK-LEVEL-FOUND
               ADD 1 TO WS-MC-OCN-BREAKS
           ELSE
               IF WS-SM-BAN NOT = WS-PK-BAN
                   MOVE 2 TO WS-BREAK-LEVEL-FOUND
               ELSE
                   MOVE 3 TO WS-BREAK-LEVEL-FOUND.
       P4535-EXIT.
           EXIT.
      * P4540 - CLOSES THE JUST-ENDED ELEMENT GROUP.  SETS WS-SUMM-    *
      * LEVEL FROM THE BREAK LEVEL THAT TRIGGERED THE CLOSE, THEN      *
      * WRITES SUMOUT (UNLESS THE ZERO-MINUTE/NON-ZERO-AMOUNT          *
      * SUPPRESSION RULE APPLIES) AND THE REPORT LINE.                 *
       P4540-CLOSE-ELEM-GROUP.
           MOVE 1 TO WS-SUMM-LEVEL.
           IF WS-BREAK-LEVEL-FOUND = 2 OR WS-BREAK-LEVEL-FOUND = 3
               MOVE 2 TO WS-SUMM-LEVEL.
           IF WS-BREAK-LEVEL-FOUND = 1
               MOVE 3 TO WS-SUMM-LEVEL.
           MOVE 'N' TO WS-MC-SUPPRESS-SW.
           IF WS-GRP-MINUTES = 0 AND WS-GRP-AMOUNT NOT = 0
               MOVE 'Y' TO WS-MC-SUPPRESS-SW
               ADD 1 TO WS-MC-GROUPS-SUPPRESSED.
           IF WS-MC-SUPPRESS-SW = 'N'
               MOVE SPACES TO CABS-SUMOUT-RECORD
               MOVE WS-PK-OCN TO SO-OCN
               MOVE WS-PK-BAN TO SO-BAN
               MOVE WS-PK-JURIS TO SO-JURIS-CD
               MOVE WS-PK-ELEM TO SO-RATE-ELEM
               MOVE WS-PK-STATE TO SO-STATE-CD
               MOVE WS-PK-BILL-PERIOD TO SO-BILL-PERIOD
               MOVE WS-GRP-REC-CNT TO SO-REC-COUNT
               MOVE WS-GRP-MINUTES TO SO-TOT-MINUTES
               MOVE WS-GRP-AMOUNT TO SO-GROSS-AMOUNT
               MOVE WS-GRP-ROUND-RESIDUE TO SO-ROUND-RESIDUE
               MOVE WS-SUMM-LEVEL TO SO-SUMM-LEVEL
               MOVE 'N' TO SO-RESIDUAL-SW
               WRITE CABS-SUMOUT-RECORD
               ADD 1 TO WS-WRITE-CNT
               ADD 1 TO WS-MC-GROUPS-WRITTEN.
           PERFORM P4545-PRINT-GROUP-LINE THRU P4545-EXIT.
       P4540-EXIT.
           EXIT.
      * P4545 - CABS-STD-006.  WS-SL-DETAIL IS TESTED FIRST, SO A      *
      * WS-SUMM-LEVEL OF 2 (A BAN- OR JURISDICTION-LEVEL CLOSE)        *
      * ALWAYS PRINTS AS PLAIN DETAIL, NEVER AS A GROUP SUBTOTAL -     *
      * SEE THE WS-SUMM-LEVEL-AREA COMMENT IN WORKING-STORAGE.         *
       P4545-PRINT-GROUP-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           IF WS-SL-DETAIL
               MOVE WS-PK-BAN TO PC-AMT-DESC
           ELSE
               IF WS-SL-GROUP
                   MOVE 'SUBTOTAL' TO PC-AMT-DESC
               ELSE
                   MOVE WS-PK-BAN TO PC-AMT-DESC.
           MOVE WS-GRP-MINUTES TO PC-AMT-QTY.
           MOVE WS-GRP-AMOUNT TO PC-AMT-VALUE.
           WRITE CABS-PRINT-LINE.
       P4545-EXIT.
           EXIT.
      * P4550-START-NEW-GROUP - RESET THE OPEN-GROUP ACCUMULATOR.      *
      * IF THE BREAK WAS LEVEL 1 (NEW OCN) THE REPORT GETS A NEW       *
      * SECTION HEADING.                                               *
       P4550-START-NEW-GROUP.
           MOVE 0 TO WS-GRP-REC-CNT.
           MOVE 0 TO WS-GRP-MINUTES.
           MOVE 0 TO WS-GRP-AMOUNT.
           MOVE 0 TO WS-GRP-ROUND-RESIDUE.
           MOVE WS-SM-STATE-CD TO WS-PK-STATE.
           MOVE WS-SM-BILL-PERIOD TO WS-PK-BILL-PERIOD.
           MOVE WS-SM-ROUND-RULE TO WS-PK-ROUND-RULE.
           IF WS-BREAK-LEVEL-FOUND = 1
               PERFORM P4555-PRINT-OCN-SECTION THRU P4555-EXIT.
       P4550-EXIT.
           EXIT.
       P4555-PRINT-OCN-SECTION.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '4' TO PC-CC.
           MOVE WS-SM-OCN TO PC-COL-001-020.
           WRITE CABS-PRINT-LINE.
       P4555-EXIT.
           EXIT.
       P4560-ACCUM-ELEMENT.
           ADD 1 TO WS-GRP-REC-CNT.
           ADD WS-SM-QTY TO WS-GRP-MINUTES.
           ADD WS-SM-AMOUNT TO WS-GRP-AMOUNT.
           ADD 1 TO WS-SUMM-CNT.
       P4560-EXIT.
           EXIT.
      * P4570/P4575 - RUN-WIDE ELEMENT TOTALS, INSERT-ON-MISS.         *
       P4570-ACCUM-ELEM-TABLE.
           MOVE 'N' TO WS-TBL-FOUND-SW.
           PERFORM P4575-SEARCH-ELEM-ONE THRU P4575-EXIT
               VARYING WS-ET-X FROM 1 BY 1
                   UNTIL WS-ET-X > WS-ET-CNT OR WS-TBL-FOUND.
           IF WS-TBL-FOUND
               ADD 1 TO WS-ET-REC-CNT (WS-ET-X)
               ADD WS-SM-QTY TO WS-ET-MINUTES (WS-ET-X)
               ADD WS-SM-AMOUNT TO WS-ET-AMOUNT (WS-ET-X)
           ELSE
               IF WS-ET-CNT < 50
                   ADD 1 TO WS-ET-CNT
                   MOVE WS-SM-RATE-ELEM TO WS-ET-RATE-ELEM (WS-ET-CNT)
                   MOVE 1 TO WS-ET-REC-CNT (WS-ET-CNT)
                   MOVE WS-SM-QTY TO WS-ET-MINUTES (WS-ET-CNT)
                   MOVE WS-SM-AMOUNT TO WS-ET-AMOUNT (WS-ET-CNT).
       P4570-EXIT.
           EXIT.
       P4575-SEARCH-ELEM-ONE.
           IF WS-ET-RATE-ELEM (WS-ET-X) = WS-SM-RATE-ELEM
               MOVE 'Y' TO WS-TBL-FOUND-SW.
       P4575-EXIT.
           EXIT.
      * P4580/P4585 - RUN-WIDE STATE TOTALS, INSERT-ON-MISS.           *
       P4580-ACCUM-STATE-TABLE.
           MOVE 'N' TO WS-TBL-FOUND-SW.
           PERFORM P4585-SEARCH-STATE-ONE THRU P4585-EXIT
               VARYING WS-ST-X FROM 1 BY 1
                   UNTIL WS-ST-X > WS-ST-CNT OR WS-TBL-FOUND.
           IF WS-TBL-FOUND
               ADD 1 TO WS-ST-REC-CNT (WS-ST-X)
               ADD WS-SM-QTY TO WS-ST-MINUTES (WS-ST-X)
               ADD WS-SM-AMOUNT TO WS-ST-AMOUNT (WS-ST-X)
           ELSE
               IF WS-ST-CNT < 60
                   ADD 1 TO WS-ST-CNT
                   MOVE WS-SM-STATE-CD TO WS-ST-STATE-CD (WS-ST-CNT)
                   MOVE 1 TO WS-ST-REC-CNT (WS-ST-CNT)
                   MOVE WS-SM-QTY TO WS-ST-MINUTES (WS-ST-CNT)
                   MOVE WS-SM-AMOUNT TO WS-ST-AMOUNT (WS-ST-CNT).
       P4580-EXIT.
           EXIT.
       P4585-SEARCH-STATE-ONE.
           IF WS-ST-STATE-CD (WS-ST-X) = WS-SM-STATE-CD
               MOVE 'Y' TO WS-TBL-FOUND-SW.
       P4585-EXIT.
           EXIT.
      * P4590 - THE FOLDED RESIDUAL, WRITTEN AS ONE TRAILING SUMOUT    *
      * ROW ONLY IF ANYTHING WAS ACTUALLY FOLDED.                      *
       P4590-WRITE-RESIDUAL-LINE.
           IF WS-RB-REC-CNT > 0
               MOVE SPACES TO CABS-SUMOUT-RECORD
               MOVE 'ALL ' TO SO-OCN
               MOVE SPACES TO SO-BAN
               MOVE 'X' TO SO-JURIS-CD
               MOVE 'RESIDL' TO SO-RATE-ELEM
               MOVE SPACES TO SO-STATE-CD
               MOVE R1-BILL-PERIOD TO SO-BILL-PERIOD
               MOVE WS-RB-REC-CNT TO SO-REC-COUNT
               MOVE WS-RB-MINUTES TO SO-TOT-MINUTES
               MOVE WS-RB-AMOUNT TO SO-GROSS-AMOUNT
               MOVE 0 TO SO-ROUND-RESIDUE
               MOVE 9 TO SO-SUMM-LEVEL
               MOVE 'Y' TO SO-RESIDUAL-SW
               WRITE CABS-SUMOUT-RECORD
               ADD 1 TO WS-WRITE-CNT.
       P4590-EXIT.
           EXIT.
       P4990-SUMM-OUTPUT-DONE.
           DISPLAY 'CABRAT09 - SORT OUTPUT COMPLETE - GROUPS '
               WS-MC-GROUPS-WRITTEN.
      * S800-CONTROL-BALANCE SECTION - MANDATORY CONTROL STEP PLUS     *
      * THE ELEMENT AND STATE TOTAL SECTIONS OF THE REPORT.            *
       S800-CONTROL-BALANCE SECTION.
       P8000-CONTROL.
           PERFORM P8100-BUILD-REPORT-HEADER THRU P8100-EXIT.
           PERFORM P8200-PRINT-ELEMENT-TOTALS THRU P8200-EXIT.
           PERFORM P8300-PRINT-STATE-TOTALS THRU P8300-EXIT.
           PERFORM P8400-BUILD-CONTROL-REC THRU P8400-EXIT.
           PERFORM P8500-CHECK-BALANCE THRU P8500-EXIT.
           PERFORM P8600-WRITE-CONTROL-REC THRU P8600-EXIT.
       P8000-EXIT.
           EXIT.
       P8100-BUILD-REPORT-HEADER.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '1' TO PC-CC.
           MOVE 'CABRAT09 - RATING SUMMARY AND AGGREGATION' TO PC-TEXT.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE R1-RUN-ID TO PC-TEXT.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS READ' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'GROUPS WRITTEN' TO PC-COL-001-020.
           MOVE WS-MC-GROUPS-WRITTEN TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'GROUPS SUPPRESSED' TO PC-COL-001-020.
           MOVE WS-MC-GROUPS-SUPPRESSED TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RESIDUAL - LOW VALUE' TO PC-COL-001-020.
           MOVE WS-RB-LOW-VALUE-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RESIDUAL - DISPUTED OCN' TO PC-COL-001-020.
           MOVE WS-RB-DISPUTE-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           WRITE CABS-PRINT-LINE.
       P8100-EXIT.
           EXIT.
      * P8200/P8210 - WALKS THE RUN-WIDE ELEMENT TOTALS TABLE.  PC-    *
      * NEW-SECTION MARKS THE START OF THIS PART OF THE REPORT.        *
       P8200-PRINT-ELEMENT-TOTALS.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '4' TO PC-CC.
           MOVE 'ELEMENT TOTALS' TO PC-TEXT.
           WRITE CABS-PRINT-LINE.
           IF WS-ET-CNT > 0
               PERFORM P8210-PRINT-ONE-ELEM THRU P8210-EXIT
                   VARYING WS-ET-X FROM 1 BY 1
                   UNTIL WS-ET-X > WS-ET-CNT.
       P8200-EXIT.
           EXIT.
       P8210-PRINT-ONE-ELEM.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-ET-RATE-ELEM (WS-ET-X) TO PC-AMT-DESC.
           MOVE WS-ET-MINUTES (WS-ET-X) TO PC-AMT-QTY.
           MOVE WS-ET-AMOUNT (WS-ET-X) TO PC-AMT-VALUE.
           WRITE CABS-PRINT-LINE.
       P8210-EXIT.
           EXIT.
      * P8300/P8310 - WALKS THE RUN-WIDE STATE TOTALS TABLE.           *
       P8300-PRINT-STATE-TOTALS.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '4' TO PC-CC.
           MOVE 'STATE TOTALS' TO PC-TEXT.
           WRITE CABS-PRINT-LINE.
           IF WS-ST-CNT > 0
               PERFORM P8310-PRINT-ONE-STATE THRU P8310-EXIT
                   VARYING WS-ST-X FROM 1 BY 1
                   UNTIL WS-ST-X > WS-ST-CNT.
       P8300-EXIT.
           EXIT.
       P8310-PRINT-ONE-STATE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-ST-STATE-CD (WS-ST-X) TO PC-AMT-DESC.
           MOVE WS-ST-MINUTES (WS-ST-X) TO PC-AMT-QTY.
           MOVE WS-ST-AMOUNT (WS-ST-X) TO PC-AMT-VALUE.
           WRITE CABS-PRINT-LINE.
       P8310-EXIT.
           EXIT.
      * P8400 - THIS IS A PURE SUMMARISER: EVERY RECORD READ ENDS UP   *
      * EITHER IN A RATE-ELEMENT GROUP OR THE RESIDUAL BUCKET, NEVER   *
      * WRITTEN 1:1.  CT-WRITTEN IS THEREFORE ALWAYS ZERO AND          *
      * CT-SUMMARISED CARRIES THE FULL READ COUNT.                     *
       P8400-BUILD-CONTROL-REC.
           MOVE R1-RUN-ID TO CT-RUN-ID.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE 1 TO CT-STEP-SEQ.
           MOVE R1-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE R1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE SPACES TO CT-JOBNAME.
           MOVE SPACES TO CT-STEPNAME.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE 0 TO CT-WRITTEN.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE SPACES TO CT-FILLER.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-ABEND-CD.
       P8400-EXIT.
           EXIT.
       P8500-CHECK-BALANCE.
           IF CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARISED +
               CT-CARRIED-FWD
               MOVE 'B' TO CT-BAL-IND
           ELSE
               MOVE 'O' TO CT-BAL-IND.
       P8500-EXIT.
           EXIT.
       P8600-WRITE-CONTROL-REC.
           MOVE CABS-CONTROL-RECORD TO CABS-CTLOUT-RECORD.
           WRITE CABS-CTLOUT-RECORD.
       P8600-EXIT.
           EXIT.
      ******************************************************************
      * S900-TERMINATION SECTION.                                      *
      ******************************************************************
       S900-TERMINATION SECTION.
       P9000-TERM.
           CLOSE RATIN.
           CLOSE SUMOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABRAT09 - RUN COMPLETE'.
           DISPLAY '  READ       = ' WS-READ-CNT.
           DISPLAY '  GROUPS OUT = ' WS-MC-GROUPS-WRITTEN.
           DISPLAY '  SUPPRESSED = ' WS-MC-GROUPS-SUPPRESSED.
           DISPLAY '  RESIDUAL   = ' WS-RB-REC-CNT.
       P9000-EXIT.
           EXIT.
       P9900-FATAL-OPEN.
           MOVE WS-PGM-NAME TO WS-AB-PGM.
           MOVE 9901 TO WS-AB-USER-CODE.
           DISPLAY 'CABRAT09 FATAL OPEN - ' WS-AB-REASON.
           CALL 'CABABEND' USING WS-AB-PGM WS-AB-PARA WS-AB-REASON
               WS-AB-USER-CODE.
           STOP RUN.
       P9900-EXIT.
           EXIT.
