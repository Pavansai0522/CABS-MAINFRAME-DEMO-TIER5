      *****************************************************************
      * CABURT16 - TARIFF CODE TABLE REFRESH                          *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               MNTIN   TELCABS.CABS.MNTIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               TAROUT  TELCABS.CABS.TAROUT         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - THE OUTPUT IS REBUILT EACH RUN     *
      * REVISION HISTORY                                              *
      *   V1.00  1991-06-13  P.NAIR       INITIAL RELEASE             *
      *   V1.04  1997-07-23  W.J.MCALLISTER CONTROL RECORD ADDED PER  *
      *                      CABS-STD-002                             *
      *   V1.07  2004-06-08  B.R.HALVORSEN JOB PARAMETER MADE         *
      *                      MANDATORY                                *
      *   V1.09  2019-04-05  B.R.HALVORSEN JOB PARAMETER MADE         *
      *                      MANDATORY                                *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABURT16.
       AUTHOR. TELCABS APPLICATIONS - RATE TABLE MAINTENANCE.
      *****************************************************************
      * TARIFF CODE TABLE REFRESH. THE STEP IS DRIVEN ENTIRELY FROM   *
      * THE SYSIN PARM CARD AND THE DD ALLOCATIONS IN THE JOB.        *
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND   *
      * THERE NEVER HAS BEEN.                                         *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           C01 IS TO-NEW-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MNTIN ASSIGN TO UT-S-MNTIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT TAROUT ASSIGN TO UT-S-TAROUT
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
       DATA DIVISION.
       FILE SECTION.
      * MNTIN - WORK FILE, DELETED AT STEP END.
       FD  MNTIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 90 CHARACTERS.
       01  CABS-DB-IN-RECORD.
           05  ID-CODE                     PIC X(02).
           05  ID-PERIOD                   PIC 9(06).
           05  ID-ACCOUNT                  PIC 9(06).
           05  ID-CARRIER                  PIC X(02).
           05  ID-CARRIER2                 PIC S9(09) COMP-3.
           05  ID-MEDIA                    PIC S9(13)V9(02) COMP-3.
           05  ID-INVOICE                  PIC S9(05) COMP-3.
           05  ID-GROUP                    PIC X(03).
           05  ID-ELEM                     PIC X(03).
           05  ID-REGION                   PIC X(08).
           05  ID-OCN                      PIC S9(09)V9(05) COMP-3.
           05  ID-BAND                     PIC S9(07) COMP-3.
           05  ID-SEQ                      PIC X(04).
           05  ID-CIRCUIT                  PIC X(20).
           05  DB-FILL-01                  PIC X(8).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-DB-VIEW1 REDEFINES CABS-DB-IN-RECORD.
           05  R0D-CODE                    PIC S9(13)V9(02) COMP-3.
           05  R0D-ACCOUNT                 PIC 9(06).
           05  R0D-ACCOUNT2                PIC S9(15) COMP-3.
           05  R0D-CODE2                   PIC X(10).
           05  R0D-TARGET                  PIC S9(05) COMP-3.
           05  R0D-REST                    PIC X(55).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-DB-VIEW2 REDEFINES CABS-DB-IN-RECORD.
           05  R1D-TARIFF                  PIC S9(13)V9(02) COMP-3.
           05  R1D-GROUP                   PIC S9(07)V9(02) COMP-3.
           05  R1D-INVOICE                 PIC S9(07) COMP-3.
           05  R1D-SOURCE                  PIC S9(07)V9(05) COMP-3.
           05  R1D-REST                    PIC X(66).
      * TAROUT - PERMANENT DATASET HELD ON DASD.
       FD  TAROUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-DB-OUT-RECORD.
           05  OD-ACCOUNT                  PIC X(10).
           05  OD-STATUS                   PIC X(13).
           05  OD-SEQ                      PIC S9(13) COMP-3.
           05  OD-TARGET                   PIC S9(07) COMP-3.
           05  OD-STATE                    PIC 9(05).
           05  OD-OCN                      PIC S9(09)V9(02) COMP-3.
           05  OD-ELEM                     PIC X(16).
           05  DB-FILL-02                  PIC X(19).
      * CTLOUT - WORK FILE, DELETED AT STEP END.
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD              PIC X(180).
      * RPTOUT - WORK FILE, DELETED AT STEP END.
       FD  RPTOUT
           RECORDING MODE IS FA
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       COPY CABSPRNT.
       WORKING-STORAGE SECTION.
      * STANDARD SHARED WORKING STORAGE. SEE CABSWRK.
       COPY CABSWRK.
      * SHARED LAYOUT PULLED IN FOR THE TARIFF SIDE.
       COPY CABSCOMM.
      * SHARED LAYOUT PULLED IN FOR THE BAND SIDE.
       COPY CABSRT01.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABURT16'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V2.05'.
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
       01  WS-COUNT-AREA.
           05  WS-DB-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DB-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DB-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DB-CNT-04                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-DB-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DB-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DB-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-DB-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DB-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DB-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-DB-TXT-01                PIC X(16) VALUE SPACES.
           05  WS-DB-TXT-02                PIC X(08) VALUE SPACES.
           05  WS-DB-TXT-03                PIC X(16) VALUE SPACES.
           05  WS-DB-TXT-04                PIC X(08) VALUE SPACES.
           05  WS-DB-TXT-05                PIC X(12) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-DB-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-DB-ON-01                 VALUE 'Y'.
               88  WS-DB-OFF-01                VALUE 'N'.
           05  WS-DB-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-DB-ON-02                 VALUE 'Y'.
               88  WS-DB-OFF-02                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-DB-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DB-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DB-SUB-03                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. A ROW THAT DOES NOT FIT IS COUNTED AND DROPPED
      * - THE STEP DOES NOT FAIL.
       01  WS-DB-TABLE.
           05  WS-DB-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-DB-TB-ENTRY OCCURS 80 TIMES
                                       INDEXED BY WS-DB-IX.
               10  WS-DB-TB-KEY                PIC X(13).
               10  WS-DB-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-DB-TB-TXT                PIC X(30).
               10  WS-DB-TB-EFF                PIC 9(05).
               10  WS-DB-TB-EXP                PIC 9(05).
       01  WS-DB-WORK-GROUP-1.
           05  WS-DB-G1-CIRCUIT            PIC X(20).
           05  WS-DB-G1-MEDIA              PIC 9(05).
           05  WS-DB-G1-CARRIER            PIC S9(09) COMP-3.
           05  WS-DB-G1-LEVEL              PIC X(10).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 58.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABURT16 - TARIFF CODE TABLE REFRESH'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-DB-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-DB-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9937.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-DB-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-DB-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           PERFORM P1400-PRIME-READ THRU P1400-EXIT.
       P1000-EXIT.
           EXIT.
       P1100-OPEN-FILES.
           OPEN INPUT MNTIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON MNTIN - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT TAROUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON TAROUT - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON CTLOUT - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RPTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON RPTOUT - CHECK THE ALLOCATION' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-DB-CYCLE-YYDDD.
           COMPUTE WS-DB-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-DB-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-DB-CNT-02.
           MOVE 0 TO WS-DB-CNT-03.
           MOVE 0 TO WS-DB-CNT-04.
       P1200-EXIT.
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
           PERFORM P2200-EXPAND-BAND THRU P2200-EXPAND-BAND-EXIT.
           IF WS-DB-ON-02
               PERFORM P2300-SELECT-ROW THRU P2300-SELECT-ROW-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ MNTIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-EXPAND-BAND.
           CALL 'CABHASH' USING ID-GROUP WS-ACC-OCN-HASH.
           ADD WS-DB-CNT-03 TO WS-ACC-SEQ-HASH.
       P2200-EXPAND-BAND-EXIT.
           EXIT.
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT
      * PRECEDES THIS PROGRAM IN THE JOB.
       P2300-SELECT-ROW.
           ADD ID-MEDIA TO WS-DB-QTY-02.
           COMPUTE WS-DB-AMT-01 ROUNDED = WS-DB-QTY-02 * WS-DB-QTY-03.
           ADD WS-DB-AMT-01 TO WS-DB-AMT-03.
       P2300-SELECT-ROW-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-WRITE-KEY.
           MOVE SPACES TO CABS-DB-OUT-RECORD.
           MOVE ID-SEQ TO OD-ACCOUNT.
           MOVE ID-BAND TO OD-STATUS.
           MOVE ID-CIRCUIT TO OD-SEQ.
           MOVE ID-ELEM TO OD-TARGET.
           MOVE ID-REGION TO OD-STATE.
           MOVE ID-OCN TO OD-OCN.
           WRITE CABS-DB-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P3100-WRITE-KEY-EXIT.
           EXIT.
      * S800-CONTROL SECTION - THE MANDATORY CABS CONTROL BOUNDARY.
       S800-CONTROL SECTION.
       P8000-CONTROL.
           PERFORM P8010-PRINT-AUDIT-REPORT THRU P8010-EXIT.
           PERFORM P8100-BUILD-CONTROL-REC THRU P8100-EXIT.
           PERFORM P8200-CHECK-BALANCE THRU P8200-EXIT.
           PERFORM P8300-WRITE-CONTROL-REC THRU P8300-EXIT.
       P8000-EXIT.
           EXIT.
       P8010-PRINT-AUDIT-REPORT.
           ADD 1 TO WS-RPT-PAGE-NBR.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '1' TO PC-CC.
           MOVE WS-RPT-TITLE1 TO PC-TEXT.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-RPT-TITLE2 TO PC-TEXT.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS READ' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO WS-DB-CNT-EDIT.
           MOVE WS-DB-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS WRITTEN' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-DB-CNT-EDIT.
           MOVE WS-DB-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS REJECTED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-DB-CNT-EDIT.
           MOVE WS-DB-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS SUMMARISED' TO PC-COL-001-020.
           MOVE WS-SUMM-CNT TO WS-DB-CNT-EDIT.
           MOVE WS-DB-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS CARRIED FWD' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-DB-CNT-EDIT.
           MOVE WS-DB-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-DB-CNT-01 TO WS-DB-CNT-EDIT.
           MOVE WS-DB-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 02' TO PC-COL-001-020.
           MOVE WS-DB-CNT-02 TO WS-DB-CNT-EDIT.
           MOVE WS-DB-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 03' TO PC-COL-001-020.
           MOVE WS-DB-CNT-03 TO WS-DB-CNT-EDIT.
           MOVE WS-DB-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE WS-DB-TXT-03 TO CT-RESTART-KEY.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-FILLER.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE 9 TO CT-STEP-SEQ.
           MOVE WS-DB-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
       P8100-EXIT.
           EXIT.
      * P8200-CHECK-BALANCE - A RECORD THAT WAS HELD FOR THE NEXT
      * CYCLE IS BROUGHT INTO THE CARRIED FORWARD SIDE BEFORE THE
      * EQUATION IS TESTED.
       P8200-CHECK-BALANCE.
           ADD WS-DB-CNT-01 TO CT-CARRIED-FWD.
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
      * S900-TERMINATION SECTION.
       S900-TERMINATION SECTION.
       P9000-TERM.
           CLOSE MNTIN.
           CLOSE TAROUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABURT16 - NORMAL END OF JOB'.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  DB-CNT-03 = ' WS-DB-CNT-03.
           DISPLAY '  DB-CNT-02 = ' WS-DB-CNT-02.
       P9000-EXIT.
           EXIT.
