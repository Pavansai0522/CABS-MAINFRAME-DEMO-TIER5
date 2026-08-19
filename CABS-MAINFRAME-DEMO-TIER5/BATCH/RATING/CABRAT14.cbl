      *****************************************************************
      * CABRAT14 - RATE TABLE AUDIT AND EFFECTIVE-DATE SWEEP           *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               RATEMST TELCABS.CABS.RATE (VSAM KSDS) CABSRATE  *
      *               CTLIN   TELCABS.CABS.CONTROL(0)       CABSCTL   *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               RPTOUT  SYSOUT CLASS A                CABSPRNT  *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +             *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN                                      *
      * REVISION HISTORY                                              *
      *   V1.00  1990-05-14  D.OKONKWO    INITIAL RELEASE - LISTS     *
      *                      RATE VERSIONS IN EFFECTIVE-DATE ORDER    *
      *   V1.02  1993-09-08  R.T.WHEELER  GAP AND OVERLAP DETECTION   *
      *                      ADDED BETWEEN CONSECUTIVE VERSIONS       *
      *   V1.05  1997-02-27  J.M.CASTILLO EXPIRING-WITHIN-60-DAYS     *
      *                      FLAG ADDED FOR THE REGULATORY TEAM       *
      *   V1.07  1999-12-01  P.NAIR       Y2K REMEDIATION - CENTURY   *
      *                      PIVOT LOGIC ADDED TO THE DATE COMPARES   *
      *   V1.09  2004-06-19  A.BUKOWSKI   BAND TABLE CONTIGUITY AND   *
      *                      MONOTONICITY CHECKS ADDED                *
      *   V1.11  2009-10-30  S.MARCHETTI  PRIOR-RUN RECONCILIATION    *
      *                      ADDED - READS LAST CABRAT14 CTLOUT ROW   *
      *   V1.13  2015-04-08  M.HOLLIS     ZERO/NEGATIVE RATE FLAG     *
      *                      ADDED AFTER AUDIT FINDING 2015-031       *
      *   V1.14  2019-09-17  G.PRZYBYLSKI RECOMPILE ONLY - CABSRATE   *
      *                      BAND TABLE WIDENED TO 24, NOT AN ISSUE   *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABRAT14.
       AUTHOR. TELCABS APPLICATIONS - RATING TEAM.
      *****************************************************************
      * BROWSES RATEMST GROUPED BY TARIFF/ELEM/JURIS/STATE, AUDITS *
      * GAPS, OVERLAPS, NEAR EXPIRY, BAD BANDS, ZERO/NEG RATES.    *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           C01 IS TO-NEW-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RATEMST ASSIGN TO DA-RATEMST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS RT-KEY
               FILE STATUS IS WS-FS-TABLE.
           SELECT CTLIN ASSIGN TO UT-S-CTLIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CTLIN.
           SELECT RPTOUT ASSIGN TO UT-S-RPTOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT CTLOUT ASSIGN TO UT-S-CTLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      *****************************************************************
      * RATEMST - VSAM KSDS, FULL BROWSE, KEY ORDER = AUDIT ORDER. *
      *****************************************************************
       FD  RATEMST
           LABEL RECORDS ARE STANDARD.
       COPY CABSRATE.
      *****************************************************************
      * CTLIN - PRIOR CTLOUT GENERATION, SHARED ACROSS PROGRAMS.   *
      *****************************************************************
       FD  CTLIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLIN-RECORD                 PIC X(180).
      *****************************************************************
      * RPTOUT - PAGINATED TARIFF AUDIT REPORT.                       *
      *****************************************************************
       FD  RPTOUT
           RECORDING MODE IS FA
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       COPY CABSPRNT.
      *****************************************************************
      * CTLOUT - RUN CONTROL / BALANCING RECORD.                      *
      *****************************************************************
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD                PIC X(180).
       WORKING-STORAGE SECTION.
       COPY CABSWRK.
       COPY CABSRT01.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                   PIC X(08) VALUE 'CABRAT14'.
           05  WS-PGM-VERSION                PIC X(05) VALUE 'V1.14'.
           05  WS-EXPIRY-WINDOW-DAYS         PIC S9(03) COMP-3
                                                            VALUE 60.
       01  WS-PARM-CARD                      PIC X(80).
       01  WS-PARM-CARD-R1 REDEFINES WS-PARM-CARD.
           05  PC1-CYCLE-YYDDD               PIC 9(05).
           05  PC1-BILL-PERIOD                PIC 9(06).
           05  PC1-RUN-ID                     PIC X(12).
           05  PC1-FILLER                     PIC X(57).
       01  WS-EXTRA-SWITCHES.
           05  WS-FS-CTLIN                   PIC X(02) VALUE '00'.
           05  WS-CTLIN-EOF-SW               PIC X(01) VALUE 'N'.
               88  WS-CTLIN-EOF                 VALUE 'Y'.
      *****************************************************************
      * GROUP-BREAK KEY - RT-KEY WITHOUT THE EFFECTIVE DATE.  A       *
      * CHANGE HERE STARTS A NEW RATE ELEMENT GROUP AND SUB-HEADING.  *
      *****************************************************************
       01  WS-GROUP-KEY-CURR.
           05  WS-GK-TARIFF                  PIC X(04).
           05  WS-GK-ELEM                    PIC X(06).
           05  WS-GK-JURIS                   PIC X(01).
           05  WS-GK-STATE                   PIC X(02).
       01  WS-GROUP-KEY-SAVE.
           05  WS-GS-TARIFF                  PIC X(04).
           05  WS-GS-ELEM                    PIC X(06).
           05  WS-GS-JURIS                   PIC X(01).
           05  WS-GS-STATE                   PIC X(02).
       01  WS-GROUP-WORK.
           05  WS-GW-VERSION-SEQ             PIC 9(03) VALUE 0.
           05  WS-GW-GROUP-CNT               PIC 9(05) VALUE 0.
       01  WS-PREVIOUS-ROW.
           05  WS-PR-EFF-YYDDD               PIC 9(05).
           05  WS-PR-EXP-YYDDD               PIC 9(05).
      *****************************************************************
      * CENTURY-RESOLUTION WORK - THREE AREAS, THREE COMPARES.     *
      *****************************************************************
       01  WS-EFF-CENTURY-WORK.
           05  WS-CV-YYDDD                   PIC 9(05).
           05  WS-CV-YYDDD-R REDEFINES WS-CV-YYDDD.
               10  WS-CV-YY                   PIC 9(02).
               10  WS-CV-DDD                  PIC 9(03).
           05  WS-CV-CCYY                    PIC 9(04).
           05  WS-CV-ABS                     PIC 9(07).
       01  WS-EXP-CENTURY-WORK.
           05  WS-CX-YYDDD                   PIC 9(05).
           05  WS-CX-YYDDD-R REDEFINES WS-CX-YYDDD.
               10  WS-CX-YY                   PIC 9(02).
               10  WS-CX-DDD                  PIC 9(03).
           05  WS-CX-CCYY                    PIC 9(04).
           05  WS-CX-ABS                     PIC 9(07).
       01  WS-EXPIRY-CENTURY-WORK.
           05  WS-CT-YYDDD                   PIC 9(05).
           05  WS-CT-YYDDD-R REDEFINES WS-CT-YYDDD.
               10  WS-CT-YY                   PIC 9(02).
               10  WS-CT-DDD                  PIC 9(03).
           05  WS-CT-CCYY                    PIC 9(04).
           05  WS-CT-ABS                     PIC 9(07).
           05  WS-TODAY-YYDDD                PIC 9(05).
           05  WS-TODAY-YYDDD-R REDEFINES WS-TODAY-YYDDD.
               10  WS-TODAY-YY                PIC 9(02).
               10  WS-TODAY-DDD               PIC 9(03).
           05  WS-TODAY-CCYY                 PIC 9(04).
           05  WS-TODAY-ABS                  PIC 9(07).
           05  WS-DAYS-TO-EXPIRY             PIC S9(07) COMP-3.
       01  WS-ROW-FLAGS.
           05  WS-RF-GAP-SW                  PIC X(01) VALUE 'N'.
               88  WS-RF-GAP                    VALUE 'Y'.
           05  WS-RF-OVERLAP-SW              PIC X(01) VALUE 'N'.
               88  WS-RF-OVERLAP                VALUE 'Y'.
           05  WS-RF-EXPIRING-SW             PIC X(01) VALUE 'N'.
               88  WS-RF-EXPIRING               VALUE 'Y'.
           05  WS-RF-ZERO-RATE-SW            PIC X(01) VALUE 'N'.
               88  WS-RF-ZERO-RATE              VALUE 'Y'.
           05  WS-RF-BAND-BAD-SW             PIC X(01) VALUE 'N'.
               88  WS-RF-BAND-BAD               VALUE 'Y'.
           05  WS-RF-ANY-FLAG-SW             PIC X(01) VALUE 'N'.
               88  WS-RF-ANY-FLAG               VALUE 'Y'.
       01  WS-BAND-CHECK-WORK.
           05  WS-BC-SUB                     PIC 9(02) VALUE 0.
           05  WS-BC-PREV-THRU               PIC S9(11) COMP-3.
       01  WS-FLAG-TEXT-WORK.
           05  WS-FT-TEXT                    PIC X(60).
           05  WS-FT-FRAG1                   PIC X(10).
           05  WS-FT-FRAG2                   PIC X(10).
           05  WS-FT-FRAG3                   PIC X(10).
           05  WS-FT-FRAG4                   PIC X(10).
           05  WS-FT-FRAG5                   PIC X(10).
      *****************************************************************
      * REPORT HEADING WORK - ASSEMBLED FROM FIVE FRAGMENTS/RUN.   *
      *****************************************************************
       01  WS-HEADING-WORK.
           05  WS-HD-TEXT                    PIC X(80).
           05  WS-HD-FRAG1                   PIC X(20).
           05  WS-HD-FRAG2                   PIC X(10).
           05  WS-HD-FRAG3                   PIC X(08).
           05  WS-HD-FRAG4                   PIC X(12).
           05  WS-HD-FRAG5                   PIC X(12).
       01  WS-EDIT-FIELDS.
           05  WS-ED-RATE                    PIC Z.ZZZZ9.
           05  WS-ED-CYCLE                   PIC 9(05).
           05  WS-ED-EFF-YYDDD               PIC 9(05).
           05  WS-ED-EXP-YYDDD                PIC 9(05).
           05  WS-ED-VERSION                  PIC ZZ9.
       01  WS-RPT-CONTROL.
           05  WS-RPT-PAGE-NBR               PIC 9(04) VALUE 0.
           05  WS-RPT-LINE-NBR               PIC 9(03) VALUE 0.
           05  WS-RPT-LINES-PER-PAGE         PIC 9(03) VALUE 55.
       01  WS-RECONCILE-WORK.
           05  WS-RC-PRIOR-CNT               PIC S9(11) COMP-3
                                                            VALUE 0.
           05  WS-RC-PRIOR-FOUND-SW          PIC X(01) VALUE 'N'.
               88  WS-RC-PRIOR-FOUND            VALUE 'Y'.
           05  WS-RC-DELTA                   PIC S9(11) COMP-3
                                                            VALUE 0.
       01  WS-CALL-RC-AREA.
           05  WS-RC-PARMR                   PIC 9(04).
           05  WS-RC-HASH                    PIC 9(04).
       01  WS-HASH-CALL-WORK.
           05  WS-HC-SEQ-IN                  PIC S9(17)       COMP-3.
       01  WS-MISC-COUNTERS.
           05  WS-MC-GAP-CNT                 PIC S9(09) COMP-3 VALUE 0.
           05  WS-MC-OVERLAP-CNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-MC-EXPIRING-CNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-MC-ZERO-RATE-CNT           PIC S9(09) COMP-3 VALUE 0.
           05  WS-MC-BAND-BAD-CNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-MC-ELEM-CNT                PIC S9(09) COMP-3 VALUE 0.
           05  WS-MC-CHECKPOINT-QUOT         PIC S9(09) COMP-3 VALUE 0.
           05  WS-MC-CHECKPOINT-REM          PIC S9(09) COMP-3 VALUE 0.
       01  WS-ABEND-WORK.
           05  WS-AB-PARA                    PIC X(30).
           05  WS-AB-REASON                  PIC X(60).
       PROCEDURE DIVISION.
       P0000-MAINLINE.
           PERFORM P1000-INIT THRU P1000-EXIT.
           PERFORM P2000-PROCESS THRU P2000-EXIT UNTIL WS-EOF.
           PERFORM P8000-CONTROL THRU P8000-EXIT.
           PERFORM P9000-TERM THRU P9000-EXIT.
           STOP RUN.
      *****************************************************************
      * S100-INITIALISATION SECTION                                   *
      *****************************************************************
       S100-INITIALISATION SECTION.
       P1000-INIT.
           PERFORM P1100-OPEN-FILES THRU P1100-EXIT.
           PERFORM P1200-READ-PARM THRU P1200-EXIT.
           PERFORM P1300-READ-PRIOR-CONTROL THRU P1300-EXIT.
           PERFORM P1400-INIT-COUNTERS THRU P1400-EXIT.
           PERFORM P5000-PRINT-REPORT-HEADER THRU P5000-EXIT.
           MOVE LOW-VALUES TO RT-KEY.
           START RATEMST KEY NOT LESS THAN RT-KEY
               INVALID KEY MOVE 'Y' TO WS-EOF-SW.
           IF NOT WS-EOF
               PERFORM P2100-READ-RATEMST THRU P2100-EXIT.
       P1000-EXIT.
           EXIT.
       P1100-OPEN-FILES.
           MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA.
           OPEN INPUT RATEMST.
           IF WS-FS-TABLE NOT = '00'
               MOVE 'RATEMST OPEN FAILED' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN INPUT CTLIN.
           IF WS-FS-CTLIN NOT = '00'
               MOVE 'CTLIN OPEN FAILED' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RPTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'RPTOUT OPEN FAILED' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'CTLOUT OPEN FAILED' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
       P1100-EXIT.
           EXIT.
       P1200-READ-PARM.
           MOVE SPACES TO WS-PARM-CARD.
           ACCEPT WS-PARM-CARD FROM SYSIN.
           CALL 'CABPARMR' USING WS-PARM-CARD WS-RC-PARMR.
           MOVE PC1-CYCLE-YYDDD TO R1-CYCLE-YYDDD.
           MOVE PC1-BILL-PERIOD TO R1-BILL-PERIOD.
           MOVE PC1-RUN-ID TO R1-RUN-ID.
       P1200-EXIT.
           EXIT.
      *****************************************************************
      * P1300-READ-PRIOR-CONTROL - KEEPS LAST CABRAT14 CTLIN ROW.  *
      *****************************************************************
       P1300-READ-PRIOR-CONTROL.
           PERFORM P1310-READ-CTLIN THRU P1310-EXIT
               UNTIL WS-CTLIN-EOF.
       P1300-EXIT.
           EXIT.
       P1310-READ-CTLIN.
           READ CTLIN
               AT END MOVE 'Y' TO WS-CTLIN-EOF-SW.
           IF NOT WS-CTLIN-EOF
               MOVE CABS-CTLIN-RECORD TO CABS-CONTROL-RECORD
               IF CT-PROCESS-ID = WS-PGM-NAME
                   MOVE CT-HASH-SEQ TO WS-RC-PRIOR-CNT
                   MOVE 'Y' TO WS-RC-PRIOR-FOUND-SW.
       P1310-EXIT.
           EXIT.
       P1400-INIT-COUNTERS.
           MOVE 0 TO WS-READ-CNT.
           MOVE 0 TO WS-WRITE-CNT.
           MOVE 0 TO WS-REJECT-CNT.
           MOVE 0 TO WS-SUMM-CNT.
           MOVE 0 TO WS-CFWD-CNT.
           MOVE 0 TO WS-ACC-MINUTES.
           MOVE 0 TO WS-ACC-AMOUNT.
           MOVE 0 TO WS-ACC-SEQ-HASH.
           MOVE 0 TO WS-ACC-OCN-HASH.
           MOVE SPACES TO WS-GROUP-KEY-SAVE.
       P1400-EXIT.
           EXIT.
       P9900-FATAL-OPEN.
           MOVE 'B037' TO CT-ABEND-CD.
           CALL 'CABABEND' USING WS-AB-PARA WS-AB-REASON
               CT-ABEND-CD.
       P9900-EXIT.
           EXIT.
      *****************************************************************
      * S200-MAIN-PROCESS SECTION                                     *
      *****************************************************************
       S200-MAIN-PROCESS SECTION.
       P2000-PROCESS.
           PERFORM P2110-CHECK-GROUP-BREAK THRU P2110-EXIT.
           IF WS-GK-TARIFF NOT = WS-GS-TARIFF OR
                   WS-GK-ELEM NOT = WS-GS-ELEM OR
                   WS-GK-JURIS NOT = WS-GS-JURIS OR
                   WS-GK-STATE NOT = WS-GS-STATE
               PERFORM P2200-START-NEW-GROUP THRU P2200-EXIT
           ELSE
               ADD 1 TO WS-GW-VERSION-SEQ
               PERFORM P2400-CHECK-GAP-OVERLAP THRU P2400-EXIT.
           PERFORM P2300-VALIDATE-ROW THRU P2300-EXIT.
           PERFORM P2500-CHECK-EXPIRING-SOON THRU P2500-EXIT.
           PERFORM P2600-CHECK-BAND-TABLE THRU P2600-EXIT.
           PERFORM P2700-PRINT-DETAIL-LINE THRU P2700-EXIT.
           PERFORM P2900-SAVE-PREVIOUS THRU P2900-EXIT.
           PERFORM P2100-READ-RATEMST THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-RATEMST.
           READ RATEMST NEXT RECORD
               AT END MOVE 'Y' TO WS-EOF-SW.
           IF NOT WS-EOF
               ADD 1 TO WS-READ-CNT
               ADD 1 TO WS-SUMM-CNT
               PERFORM P2105-CHECKPOINT-DISPLAY THRU P2105-EXIT.
       P2100-EXIT.
           EXIT.
       P2105-CHECKPOINT-DISPLAY.
           DIVIDE WS-READ-CNT BY 25000 GIVING WS-MC-CHECKPOINT-QUOT
               REMAINDER WS-MC-CHECKPOINT-REM.
           IF WS-MC-CHECKPOINT-REM = 0
               DISPLAY 'CABRAT14 - ' WS-READ-CNT ' ROWS READ'.
       P2105-EXIT.
           EXIT.
       P2110-CHECK-GROUP-BREAK.
           MOVE RT-TARIFF-CD TO WS-GK-TARIFF.
           MOVE RT-RATE-ELEM TO WS-GK-ELEM.
           MOVE RT-JURIS-CD TO WS-GK-JURIS.
           MOVE RT-STATE-CD TO WS-GK-STATE.
       P2110-EXIT.
           EXIT.
       P2200-START-NEW-GROUP.
           MOVE WS-GK-TARIFF TO WS-GS-TARIFF.
           MOVE WS-GK-ELEM TO WS-GS-ELEM.
           MOVE WS-GK-JURIS TO WS-GS-JURIS.
           MOVE WS-GK-STATE TO WS-GS-STATE.
           MOVE 1 TO WS-GW-VERSION-SEQ.
           ADD 1 TO WS-GW-GROUP-CNT.
           ADD 1 TO WS-MC-ELEM-CNT.
           PERFORM P5100-CHECK-PAGE-BREAK THRU P5100-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '0' TO PC-CC.
           STRING 'ELEMENT ' DELIMITED BY SIZE
                  RT-RATE-ELEM DELIMITED BY SIZE
                  ' JURIS '   DELIMITED BY SIZE
                  RT-JURIS-CD DELIMITED BY SIZE
                  ' STATE '   DELIMITED BY SIZE
                  RT-STATE-CD DELIMITED BY SIZE
               INTO PC-TEXT.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P2200-EXIT.
           EXIT.
       P2300-VALIDATE-ROW.
           MOVE 'N' TO WS-RF-ZERO-RATE-SW.
           IF RT-INITIAL-RATE NOT > 0
               MOVE 'Y' TO WS-RF-ZERO-RATE-SW
               ADD 1 TO WS-MC-ZERO-RATE-CNT.
       P2300-EXIT.
           EXIT.
      *****************************************************************
      * S400-DATE-AUDIT SECTION - P4210/P4230 HARDCODE 70 INLINE;  *
      * P4220 USES DW-PIVOT-YY, SO GAP DETECTION DEPENDS ON IT.    *
      *****************************************************************
       S400-DATE-AUDIT SECTION.
      *****************************************************************
      * P2400-CHECK-GAP-OVERLAP - PREVIOUS EXPIRY VS CURRENT EFF.  *
      *****************************************************************
       P2400-CHECK-GAP-OVERLAP.
           MOVE 'N' TO WS-RF-GAP-SW.
           MOVE 'N' TO WS-RF-OVERLAP-SW.
           MOVE RT-EFF-YYDDD TO WS-CV-YYDDD.
           PERFORM P4210-RESOLVE-EFF-CENTURY THRU P4210-EXIT.
           MOVE WS-PR-EXP-YYDDD TO WS-CX-YYDDD.
           PERFORM P4220-RESOLVE-EXP-CENTURY THRU P4220-EXIT.
           IF WS-CV-ABS > WS-CX-ABS + 1
               MOVE 'Y' TO WS-RF-GAP-SW
               ADD 1 TO WS-MC-GAP-CNT.
           IF WS-CV-ABS NOT > WS-CX-ABS
               MOVE 'Y' TO WS-RF-OVERLAP-SW
               ADD 1 TO WS-MC-OVERLAP-CNT.
       P2400-EXIT.
           EXIT.
      *****************************************************************
      * P4210-RESOLVE-EFF-CENTURY - CENTURY BREAK HARDCODED 70.    *
      *****************************************************************
       P4210-RESOLVE-EFF-CENTURY.
           IF WS-CV-YY < 70
               COMPUTE WS-CV-CCYY = 2000 + WS-CV-YY
           ELSE
               COMPUTE WS-CV-CCYY = 1900 + WS-CV-YY.
           COMPUTE WS-CV-ABS = WS-CV-CCYY * 1000 + WS-CV-DDD.
       P4210-EXIT.
           EXIT.
      *****************************************************************
      * P4220-RESOLVE-EXP-CENTURY - USES DW-PIVOT-YY, NOT A LITERAL*
      *****************************************************************
       P4220-RESOLVE-EXP-CENTURY.
           IF WS-CX-YY < DW-PIVOT-YY
               COMPUTE WS-CX-CCYY = 2000 + WS-CX-YY
           ELSE
               COMPUTE WS-CX-CCYY = 1900 + WS-CX-YY.
           COMPUTE WS-CX-ABS = WS-CX-CCYY * 1000 + WS-CX-DDD.
       P4220-EXIT.
           EXIT.
      *****************************************************************
      * P2500-CHECK-EXPIRING-SOON - 60-DAY WINDOW, HARDCODES 70.   *
      *****************************************************************
       P2500-CHECK-EXPIRING-SOON.
           MOVE 'N' TO WS-RF-EXPIRING-SW.
           MOVE RT-EXP-YYDDD TO WS-CT-YYDDD.
           MOVE R1-CYCLE-YYDDD TO WS-TODAY-YYDDD.
           PERFORM P4230-RESOLVE-EXPIRY-CENTURY THRU P4230-EXIT.
           COMPUTE WS-DAYS-TO-EXPIRY = WS-CT-ABS - WS-TODAY-ABS.
           IF WS-DAYS-TO-EXPIRY NOT > WS-EXPIRY-WINDOW-DAYS AND
                   WS-DAYS-TO-EXPIRY NOT < 0
               MOVE 'Y' TO WS-RF-EXPIRING-SW
               ADD 1 TO WS-MC-EXPIRING-CNT.
       P2500-EXIT.
           EXIT.
       P4230-RESOLVE-EXPIRY-CENTURY.
           IF WS-CT-YY < 70
               COMPUTE WS-CT-CCYY = 2000 + WS-CT-YY
           ELSE
               COMPUTE WS-CT-CCYY = 1900 + WS-CT-YY.
           COMPUTE WS-CT-ABS = WS-CT-CCYY * 1000 + WS-CT-DDD.
           IF WS-TODAY-YY < 70
               COMPUTE WS-TODAY-CCYY = 2000 + WS-TODAY-YY
           ELSE
               COMPUTE WS-TODAY-CCYY = 1900 + WS-TODAY-YY.
           COMPUTE WS-TODAY-ABS = WS-TODAY-CCYY * 1000 + WS-TODAY-DDD.
       P4230-EXIT.
           EXIT.
      *****************************************************************
      * P2600-CHECK-BAND-TABLE - MONOTONIC + CONTIGUOUS CHECK.     *
      *****************************************************************
       P2600-CHECK-BAND-TABLE.
           MOVE 'N' TO WS-RF-BAND-BAD-SW.
           IF RT-BAND-CNT > 0
               MOVE 0 TO WS-BC-PREV-THRU
               PERFORM P2610-CHECK-ONE-BAND THRU P2610-EXIT
                   VARYING WS-BC-SUB FROM 1 BY 1
                   UNTIL WS-BC-SUB > RT-BAND-CNT.
           IF WS-RF-BAND-BAD
               ADD 1 TO WS-MC-BAND-BAD-CNT.
       P2600-EXIT.
           EXIT.
       P2610-CHECK-ONE-BAND.
           IF RT-BAND-THRU (WS-BC-SUB) NOT > RT-BAND-FROM (WS-BC-SUB)
               MOVE 'Y' TO WS-RF-BAND-BAD-SW.
           IF WS-BC-SUB > 1
               IF RT-BAND-FROM (WS-BC-SUB) NOT = WS-BC-PREV-THRU + 1
                   MOVE 'Y' TO WS-RF-BAND-BAD-SW.
           MOVE RT-BAND-THRU (WS-BC-SUB) TO WS-BC-PREV-THRU.
       P2610-EXIT.
           EXIT.
      *****************************************************************
      * P2700-PRINT-DETAIL-LINE - ONE LINE/VERSION, FLAGS VIA P2800*
      *****************************************************************
       P2700-PRINT-DETAIL-LINE.
           MOVE 'N' TO WS-RF-ANY-FLAG-SW.
           IF WS-RF-GAP OR WS-RF-OVERLAP OR WS-RF-EXPIRING OR
                   WS-RF-ZERO-RATE OR WS-RF-BAND-BAD
               MOVE 'Y' TO WS-RF-ANY-FLAG-SW
               PERFORM P2800-BUILD-FLAG-TEXT THRU P2800-EXIT.
           PERFORM P5100-CHECK-PAGE-BREAK THRU P5100-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-GW-VERSION-SEQ TO WS-ED-VERSION.
           MOVE WS-ED-VERSION TO PC-COL-001-020.
           MOVE RT-EFF-YYDDD TO WS-ED-EFF-YYDDD.
           MOVE WS-ED-EFF-YYDDD TO PC-COL-021-060.
           MOVE RT-EXP-YYDDD TO WS-ED-EXP-YYDDD.
           MOVE WS-ED-EXP-YYDDD TO PC-COL-061-090.
           IF WS-RF-ANY-FLAG
               MOVE WS-FT-TEXT TO PC-COL-091-132
           ELSE
               MOVE 'OK' TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
           ADD RT-INITIAL-RATE TO WS-ACC-AMOUNT.
           MOVE RT-EFF-YYDDD TO WS-HC-SEQ-IN.
           CALL 'CABHASH' USING RT-INITIAL-RATE WS-HC-SEQ-IN
               WS-RC-HASH.
       P2700-EXIT.
           EXIT.
      *****************************************************************
      * P2800-BUILD-FLAG-TEXT - FIVE FRAGMENTS, STRUNG TOGETHER.   *
      *****************************************************************
       P2800-BUILD-FLAG-TEXT.
           MOVE SPACES TO WS-FT-FRAG1 WS-FT-FRAG2 WS-FT-FRAG3
               WS-FT-FRAG4 WS-FT-FRAG5.
           IF WS-RF-GAP
               MOVE 'GAP '     TO WS-FT-FRAG1.
           IF WS-RF-OVERLAP
               MOVE 'OVERLAP ' TO WS-FT-FRAG2.
           IF WS-RF-EXPIRING
               MOVE 'EXPIRING ' TO WS-FT-FRAG3.
           IF WS-RF-ZERO-RATE
               MOVE 'ZERORATE ' TO WS-FT-FRAG4.
           IF WS-RF-BAND-BAD
               MOVE 'BADBAND '  TO WS-FT-FRAG5.
           MOVE SPACES TO WS-FT-TEXT.
           STRING WS-FT-FRAG1 DELIMITED BY SIZE
                  WS-FT-FRAG2 DELIMITED BY SIZE
                  WS-FT-FRAG3 DELIMITED BY SIZE
                  WS-FT-FRAG4 DELIMITED BY SIZE
                  WS-FT-FRAG5 DELIMITED BY SIZE
               INTO WS-FT-TEXT.
       P2800-EXIT.
           EXIT.
       P2900-SAVE-PREVIOUS.
           MOVE RT-EFF-YYDDD TO WS-PR-EFF-YYDDD.
           MOVE RT-EXP-YYDDD TO WS-PR-EXP-YYDDD.
       P2900-EXIT.
           EXIT.
      *****************************************************************
      * S500-REPORT SECTION.                                          *
      *****************************************************************
       S500-REPORT SECTION.
      *****************************************************************
      * P5000-PRINT-REPORT-HEADER - FIVE FRAGMENTS VIA STRING.     *
      *****************************************************************
       P5000-PRINT-REPORT-HEADER.
           ADD 1 TO WS-RPT-PAGE-NBR.
           MOVE 1 TO WS-RPT-LINE-NBR.
           MOVE R1-CYCLE-YYDDD TO WS-ED-CYCLE.
           MOVE SPACES TO WS-HD-TEXT.
           STRING 'CABS TARIFF AUDIT REPORT'  DELIMITED BY SIZE
                  ' RUN='                     DELIMITED BY SIZE
                  R1-RUN-ID                   DELIMITED BY SIZE
                  ' CYCLE='                    DELIMITED BY SIZE
                  WS-ED-CYCLE                  DELIMITED BY SIZE
               INTO WS-HD-TEXT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '1' TO PC-CC.
           MOVE WS-HD-TEXT TO PC-TEXT.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'VERSION   EFF-YYDDD   EXP-YYDDD   FLAGS' TO PC-TEXT.
           WRITE CABS-PRINT-LINE.
           ADD 2 TO WS-RPT-LINE-NBR.
       P5000-EXIT.
           EXIT.
       P5100-CHECK-PAGE-BREAK.
           IF WS-RPT-LINE-NBR NOT < WS-RPT-LINES-PER-PAGE
               PERFORM P5000-PRINT-REPORT-HEADER THRU P5000-EXIT.
       P5100-EXIT.
           EXIT.
      *****************************************************************
      * P5200-PRINT-SUMMARY - END-OF-REPORT TOTALS AND THE PRIOR-RUN  *
      * RECONCILIATION FROM P1300.                                    *
      *****************************************************************
       P5200-PRINT-SUMMARY.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '1' TO PC-CC.
           MOVE 'TARIFF AUDIT SUMMARY' TO PC-TEXT.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  RATE VERSIONS READ' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  DISTINCT ELEMENTS' TO PC-COL-001-020.
           MOVE WS-MC-ELEM-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  GAPS' TO PC-COL-001-020.
           MOVE WS-MC-GAP-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  OVERLAPS' TO PC-COL-001-020.
           MOVE WS-MC-OVERLAP-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  EXPIRING WITHIN 60 DAYS' TO PC-COL-001-020.
           MOVE WS-MC-EXPIRING-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  ZERO OR NEGATIVE RATE' TO PC-COL-001-020.
           MOVE WS-MC-ZERO-RATE-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  BAD BAND TABLES' TO PC-COL-001-020.
           MOVE WS-MC-BAND-BAD-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           PERFORM P5210-PRINT-RECONCILIATION THRU P5210-EXIT.
       P5200-EXIT.
           EXIT.
       P5210-PRINT-RECONCILIATION.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  PRIOR RUN COUNT' TO PC-COL-001-020.
           IF WS-RC-PRIOR-FOUND
               MOVE WS-RC-PRIOR-CNT TO PC-COL-021-060
           ELSE
               MOVE 'NONE FOUND' TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           IF WS-RC-PRIOR-FOUND
               COMPUTE WS-RC-DELTA = WS-MC-ELEM-CNT - WS-RC-PRIOR-CNT
               MOVE SPACES TO CABS-PRINT-LINE
               MOVE ' ' TO PC-CC
               MOVE '  DELTA VS PRIOR RUN' TO PC-COL-001-020
               MOVE WS-RC-DELTA TO PC-COL-021-060
               WRITE CABS-PRINT-LINE.
       P5210-EXIT.
           EXIT.
      *****************************************************************
      * S800-CONTROL-BALANCE SECTION - CT-SUMMARISED IS THE FULL      *
      * READ COUNT SINCE EVERY ROW IS SUMMARISED INTO THE REPORT;     *
      * THIS PROGRAM NEVER WRITES OR REJECTS A RATED RECORD.          *
      *****************************************************************
       S800-CONTROL-BALANCE SECTION.
       P8000-CONTROL.
           PERFORM P5200-PRINT-SUMMARY THRU P5200-EXIT.
           PERFORM P8100-BUILD-CONTROL-REC THRU P8100-EXIT.
           PERFORM P8200-CHECK-BALANCE THRU P8200-EXIT.
           PERFORM P8300-WRITE-CONTROL-REC THRU P8300-EXIT.
       P8000-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE R1-RUN-ID TO CT-RUN-ID.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE 1 TO CT-STEP-SEQ.
           MOVE R1-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE R1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE SPACES TO CT-JOBNAME.
           MOVE SPACES TO CT-STEPNAME.
           MOVE WS-READ-CNT TO CT-READ.
      * CT-WRITTEN CARRIES THE DISTINCT-ELEMENT COUNT, NOT A RECORD   *
      * COUNT - THIS IS WHAT P1300 READS BACK NEXT RUN FOR THE        *
      * RECONCILIATION, A REPURPOSING OF THE FIELD LOCAL TO THIS      *
      * PROGRAM (SEE CABSCTL - NOTHING THERE FORBIDS IT).             *
      *****************************************************************
           MOVE 0 TO CT-WRITTEN.
           MOVE 0 TO CT-REJECTED.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE 0 TO CT-CARRIED-FWD.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
      *****************************************************************
      * CT-HASH-SEQ CARRIES THE ELEMENT COUNT HERE INSTEAD - THIS     *
      * PROGRAM HAS NO PER-RECORD SEQUENCE TO HASH, SO THE FIELD IS   *
      * REPURPOSED FOR THE RECONCILIATION COUNT P1300 READS BACK.     *
      *****************************************************************
           MOVE WS-MC-ELEM-CNT TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE SPACES TO CT-FILLER.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-ABEND-CD.
       P8100-EXIT.
           EXIT.
       P8200-CHECK-BALANCE.
           IF CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARISED +
                   CT-CARRIED-FWD
               MOVE 'B' TO CT-BAL-IND
           ELSE
               MOVE 'O' TO CT-BAL-IND.
       P8200-EXIT.
           EXIT.
       P8300-WRITE-CONTROL-REC.
           MOVE CABS-CONTROL-RECORD TO CABS-CTLOUT-RECORD.
           WRITE CABS-CTLOUT-RECORD.
       P8300-EXIT.
           EXIT.
      *****************************************************************
      * S900-TERMINATION SECTION.                                     *
      *****************************************************************
       S900-TERMINATION SECTION.
       P9000-TERM.
           CLOSE RATEMST.
           CLOSE CTLIN.
           CLOSE RPTOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABRAT14 - RUN COMPLETE'.
           DISPLAY '  READ        = ' WS-READ-CNT.
           DISPLAY '  ELEMENTS    = ' WS-MC-ELEM-CNT.
           DISPLAY '  GAPS        = ' WS-MC-GAP-CNT.
           DISPLAY '  OVERLAPS    = ' WS-MC-OVERLAP-CNT.
           DISPLAY '  EXPIRING    = ' WS-MC-EXPIRING-CNT.
           DISPLAY '  ZERO RATE   = ' WS-MC-ZERO-RATE-CNT.
           DISPLAY '  BAD BAND    = ' WS-MC-BAND-BAD-CNT.
       P9000-EXIT.
           EXIT.
