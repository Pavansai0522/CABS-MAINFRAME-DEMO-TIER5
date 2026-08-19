      *****************************************************************
      * CABURT09 - BAND TABLE MAINTENANCE                             *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               OVRIN   TELCABS.CABS.OVRIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               ELMOUT  TELCABS.CABS.ELMOUT         (LOCAL)     *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * REVISION HISTORY                                              *
      *   V1.00  2000-02-20  G.PRZYBYLSKI INITIAL RELEASE             *
      *   V1.04  2011-07-28  C.ADEYEMI    PRINT LINE WIDENED TO 133   *
      *   V1.05  2014-01-22  B.R.HALVORSEN HASH TOTAL ADDED TO THE    *
      *                      CONTROL RECORD                           *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABURT09.
       AUTHOR. TELCABS APPLICATIONS - RATE TABLE MAINTENANCE.
      *****************************************************************
      * BAND TABLE MAINTENANCE. THE STEP RUNS ONCE PER BILL CYCLE AND *
      * IS RERUN FROM THE TOP IF IT FAILS.                            *
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE  *
      * RESET INSIDE THE LOOP.                                        *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           C01 IS TO-NEW-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OVRIN ASSIGN TO UT-S-OVRIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT ELMOUT ASSIGN TO UT-S-ELMOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT CTLOUT ASSIGN TO UT-S-CTLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      * OVRIN - PERMANENT DATASET HELD ON DASD.
       FD  OVRIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-AC-IN-RECORD.
           05  IA-ACCOUNT                  PIC X(10).
           05  IA-SOURCE                   PIC 9(03).
           05  IA-MEDIA                    PIC 9(07).
           05  IA-ACCOUNT2                 PIC 9(03).
           05  IA-SOURCE2                  PIC 9(05).
           05  IA-MEDIA2                   PIC S9(09) COMP-3.
           05  IA-CENTRE                   PIC X(16).
           05  IA-LEVEL                    PIC S9(05) COMP-3.
           05  AC-FILL-01                  PIC X(28).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-AC-VIEW1 REDEFINES CABS-AC-IN-RECORD.
           05  R0A-SEGMENT                 PIC S9(07) COMP-3.
           05  R0A-SOURCE                  PIC S9(09) COMP-3.
           05  R0A-INVOICE                 PIC 9(06).
           05  R0A-SEQ                     PIC X(08).
           05  R0A-INVOICE2                PIC S9(09) COMP-3.
           05  R0A-BAN                     PIC X(16).
           05  R0A-STATE                   PIC 9(04).
           05  R0A-REST                    PIC X(32).
      * ELMOUT - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  ELMOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-AC-OUT-RECORD.
           05  OA-TARGET                   PIC X(10).
           05  OA-STATE                    PIC S9(13)V9(02) COMP-3.
           05  OA-ACCOUNT                  PIC 9(06).
           05  OA-OCN                      PIC S9(13)V9(02) COMP-3.
           05  OA-REGION                   PIC X(08).
           05  OA-TARGET2                  PIC S9(09) COMP-3.
           05  OA-LEVEL                    PIC S9(15) COMP-3.
           05  OA-CYCLE                    PIC S9(13)V9(02) COMP-3.
           05  OA-BAN                      PIC 9(09).
           05  OA-CLASS                    PIC 9(07).
           05  AC-FILL-02                  PIC X(3).
      * CTLOUT - WORK FILE, DELETED AT STEP END.
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD              PIC X(180).
       WORKING-STORAGE SECTION.
      * STANDARD SHARED WORKING STORAGE. SEE CABSWRK.
       COPY CABSWRK.
      * SHARED LAYOUT PULLED IN FOR THE TARIFF SIDE.
       COPY CABSRATE.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABURT09'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V3.27'.
           05  WS-TABLE-LIMIT              PIC S9(04) COMP-3
                                         VALUE 80.
      * SYSIN PARM CARD. POSITIONAL LAYOUT ONLY.
       01  WS-PARM-CARD                    PIC X(80).
       01  WS-PARM-CARD-R1 REDEFINES WS-PARM-CARD.
           05  PC1-REC-ID                  PIC X(02).
           05  PC1-RUN-ID                  PIC X(12).
           05  PC1-CYCLE-YYDDD             PIC 9(05).
           05  PC1-BILL-PERIOD             PIC 9(06).
           05  PC1-JOBNAME                 PIC X(08).
           05  PC1-STEPNAME                PIC X(08).
           05  PC1-OPT-ONE                 PIC X(01).
           05  PC1-OPT-TWO                 PIC X(01).
           05  PC1-FILLER                  PIC X(37).
       01  WS-PARM-CARD-R2 REDEFINES WS-PARM-CARD.
           05  PC2-LEAD                    PIC X(14).
           05  PC2-CYCLE-VIEW.
               10  PC2-CV-YY                   PIC 9(02).
               10  PC2-CV-DDD                  PIC 9(03).
           05  PC2-REST                    PIC X(61).
       01  WS-COUNT-AREA.
           05  WS-AC-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AC-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AC-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AC-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AC-CNT-05                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-AC-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AC-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AC-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-AC-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AC-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AC-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AC-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-AC-TXT-01                PIC X(26) VALUE SPACES.
           05  WS-AC-TXT-02                PIC X(08) VALUE SPACES.
           05  WS-AC-TXT-03                PIC X(30) VALUE SPACES.
           05  WS-AC-TXT-04                PIC X(26) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-AC-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-AC-ON-01                 VALUE 'Y'.
               88  WS-AC-OFF-01                VALUE 'N'.
           05  WS-AC-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-AC-ON-02                 VALUE 'Y'.
               88  WS-AC-OFF-02                VALUE 'N'.
           05  WS-AC-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-AC-ON-03                 VALUE 'Y'.
               88  WS-AC-OFF-03                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-AC-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-AC-SUB-02                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. LOADED ONCE AT INITIALISATION AND SEARCHED WITH
      * A SUBSCRIPTED WALK.
       01  WS-AC-TABLE.
           05  WS-AC-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-AC-TB-ENTRY OCCURS 80 TIMES
                                       INDEXED BY WS-AC-IX.
               10  WS-AC-TB-KEY                PIC X(10).
               10  WS-AC-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-AC-TB-TXT                PIC X(30).
               10  WS-AC-TB-EFF                PIC 9(05).
               10  WS-AC-TB-EXP                PIC 9(05).
       01  WS-AC-WORK-GROUP-1.
           05  WS-AC-G1-CIRCUIT            PIC S9(11)V9(02) COMP-3.
           05  WS-AC-G1-CYCLE              PIC S9(11)V9(02) COMP-3.
           05  WS-AC-G1-GROUP              PIC 9(05).
           05  WS-AC-G1-ELEM               PIC 9(05).
           05  WS-AC-G1-CLASS              PIC S9(09) COMP-3.
           05  WS-AC-G1-STATE              PIC S9(09) COMP-3.
           05  WS-AC-G1-CIRCUIT            PIC X(20).
           05  WS-AC-G1-TARIFF             PIC S9(11)V9(02) COMP-3.
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 58.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABURT09 - BAND TABLE MAINTENANCE'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-AC-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-AC-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9935.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-AC-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-AC-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
       PROCEDURE DIVISION.
      * P0000-MAINLINE - MANDATORY CABS BATCH SHAPE. ONE PASS OF
      * P2000-PROCESS CONSUMES ONE INPUT RECORD.
       P0000-MAINLINE.
           PERFORM P1000-INIT THRU P1000-EXIT.
           PERFORM P2000-PROCESS THRU P2000-EXIT UNTIL WS-EOF.
           PERFORM P8000-CONTROL THRU P8000-EXIT.
           PERFORM P9000-TERM THRU P9000-EXIT.
           STOP RUN.
      * S100-INITIALISATION SECTION
       S100-INITIALISATION SECTION.
       P1000-INIT.
           PERFORM P1100-OPEN-FILES THRU P1100-EXIT.
           PERFORM P1200-READ-PARM THRU P1200-EXIT.
           PERFORM P1300-LOAD-TABLE THRU P1300-EXIT.
           PERFORM P1400-PRIME-READ THRU P1400-EXIT.
       P1000-EXIT.
           EXIT.
       P1100-OPEN-FILES.
           OPEN INPUT OVRIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OVRIN NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT ELMOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'ELMOUT NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CTLOUT NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
       P1100-EXIT.
           EXIT.
      * P1200-READ-PARM - THE CYCLE DATE ARRIVES AS TWO DIGITS AND IS
      * PIVOTED ON DW-PIVOT-YY BEFORE ANY DATE MATH.
       P1200-READ-PARM.
           MOVE SPACES TO WS-PARM-CARD.
           ACCEPT WS-PARM-CARD FROM SYSIN.
           CALL 'CABPARMR' USING WS-PARM-CARD WS-RC-PARMR.
           MOVE PC1-CYCLE-YYDDD TO WS-AC-CYCLE-YYDDD.
           COMPUTE WS-AC-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-AC-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-AC-CNT-02.
           MOVE 0 TO WS-AC-CNT-05.
           MOVE 0 TO WS-AC-CNT-03.
       P1200-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-AC-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-AC-TAB-CNT NOT < 80
               MOVE 'Y' TO WS-AC-SW-01
               ADD 1 TO WS-AC-CNT-04
           ELSE
               ADD 1 TO WS-AC-TAB-CNT
               SET WS-AC-IX TO WS-AC-TAB-CNT
               MOVE IA-ACCOUNT2 TO WS-AC-TB-KEY (WS-AC-IX)
               MOVE 0 TO WS-AC-TB-VAL (WS-AC-IX)
               MOVE SPACES TO WS-AC-TB-TXT (WS-AC-IX)
               ADD 1 TO WS-AC-CNT-03.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ OVRIN
               AT END MOVE 'Y' TO WS-AC-SW-01.
       P1320-EXIT.
           EXIT.
       P1400-PRIME-READ.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P1400-EXIT.
           EXIT.
       P9900-FATAL-OPEN.
           MOVE WS-PGM-NAME TO WS-AB-PGM.
           CALL 'CABABEND' USING WS-AB-PGM WS-AB-PARA WS-AB-REASON
               WS-AB-USER-CODE WS-RC-ABEND.
       P9900-EXIT.
           EXIT.
      * S200-MAIN-PROCESSING SECTION - ONE PASS PER INPUT RECORD.
       S200-MAIN-PROCESSING SECTION.
       P2000-PROCESS.
           ADD 1 TO WS-READ-CNT.
           PERFORM P2200-RESOLVE-BAND THRU P2200-RESOLVE-BAND-EXIT.
           PERFORM P2300-EDIT-TARIFF THRU P2300-EDIT-TARIFF-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ OVRIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P2200-RESOLVE-BAND.
           MOVE 'Y' TO WS-AC-SW-03.
           IF IA-SOURCE2 < 7
               MOVE 'N' TO WS-AC-SW-03
               ADD 1 TO WS-AC-CNT-04.
           IF IA-SOURCE2 > 1533
               MOVE 'N' TO WS-AC-SW-03
               ADD 1 TO WS-AC-CNT-01.
           MOVE 'N' TO WS-AC-SW-03.
           IF WS-AC-TAB-CNT > 0
               PERFORM P270-COMPARE-TARIFF THRU P270-COMPARE-TARIFF-EXIT
               VARYING WS-AC-SUB-01 FROM 1 BY 1
               UNTIL WS-AC-SUB-01 > WS-AC-TAB-CNT
               OR WS-AC-SW-03 = 'Y'.
       P2200-RESOLVE-BAND-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P2300-EDIT-TARIFF.
           MOVE WS-AC-AMT-03 TO WS-AC-AMT-02.
           IF WS-AC-AMT-02 < 0
               COMPUTE WS-AC-AMT-02 = 0 - WS-AC-AMT-03.
       P2300-EDIT-TARIFF-EXIT.
           EXIT.
       P270-COMPARE-TARIFF.
           SET WS-AC-IX TO WS-AC-SUB-02.
           IF WS-AC-TB-KEY (WS-AC-IX) = IA-MEDIA
               MOVE 'Y' TO WS-AC-SW-02
               MOVE WS-AC-TB-VAL (WS-AC-IX) TO WS-AC-QTY-03
               MOVE WS-AC-SUB-02 TO WS-AC-SUB-01.
       P270-COMPARE-TARIFF-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-WRITE-BAND.
           MOVE SPACES TO CABS-AC-OUT-RECORD.
           MOVE IA-ACCOUNT TO OA-TARGET.
           MOVE IA-SOURCE2 TO OA-STATE.
           MOVE IA-CENTRE TO OA-ACCOUNT.
           MOVE IA-SOURCE TO OA-OCN.
           MOVE IA-ACCOUNT TO OA-REGION.
           MOVE IA-MEDIA TO OA-TARGET2.
           MOVE IA-SOURCE2 TO OA-LEVEL.
           MOVE IA-ACCOUNT2 TO OA-CYCLE.
           MOVE IA-LEVEL TO OA-BAN.
           WRITE CABS-AC-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P3100-WRITE-BAND-EXIT.
           EXIT.
       P3200-EMIT-OVERRIDE.
           CALL 'CABHASH' USING IA-CENTRE WS-ACC-OCN-HASH.
           ADD WS-AC-CNT-02 TO WS-ACC-SEQ-HASH.
       P3200-EMIT-OVERRIDE-EXIT.
           EXIT.
      * S800-CONTROL SECTION - THE MANDATORY CABS CONTROL BOUNDARY.
       S800-CONTROL SECTION.
       P8000-CONTROL.
           PERFORM P8100-BUILD-CONTROL-REC THRU P8100-EXIT.
           PERFORM P8200-CHECK-BALANCE THRU P8200-EXIT.
           PERFORM P8300-WRITE-CONTROL-REC THRU P8300-EXIT.
       P8000-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE 8 TO CT-STEP-SEQ.
           MOVE WS-AC-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE WS-AC-TXT-03 TO CT-RESTART-KEY.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE SPACES TO CT-FILLER.
           MOVE 0 TO CT-RC.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
       P8100-EXIT.
           EXIT.
      * P8200-CHECK-BALANCE - THE EQUATION IS TESTED AS IT STANDS AND
      * THE RETURN CODE IS SET FROM THE RESULT SO THE SCHEDULER CAN
      * SEE IT.
       P8200-CHECK-BALANCE.
           IF CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARISED +
                       CT-CARRIED-FWD
               MOVE 'B' TO CT-BAL-IND
           ELSE
               MOVE 'O' TO CT-BAL-IND.
           IF CT-OUT-OF-BAL
               MOVE 0004 TO CT-RC.
       P8200-EXIT.
           EXIT.
       P8300-WRITE-CONTROL-REC.
           MOVE CABS-CONTROL-RECORD TO CABS-CTLOUT-RECORD.
           WRITE CABS-CTLOUT-RECORD.
       P8300-EXIT.
           EXIT.
      * S900-TERMINATION SECTION.
       S900-TERMINATION SECTION.
       P9000-TERM.
           CLOSE OVRIN.
           CLOSE ELMOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABURT09 - END OF RUN'.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  AC-CNT-03 = ' WS-AC-CNT-03.
           DISPLAY '  AC-CNT-01 = ' WS-AC-CNT-01.
       P9000-EXIT.
           EXIT.
