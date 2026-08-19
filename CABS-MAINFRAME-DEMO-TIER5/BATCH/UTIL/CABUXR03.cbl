      *****************************************************************
      * CABUXR03 - RATE ELEMENT TO TARIFF CROSS REFERENCE             *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               XRFIN   TELCABS.CABS.XRFIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               LNKOUT  TELCABS.CABS.LNKOUT         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * REVISION HISTORY                                              *
      *   V1.00  1990-09-10  W.J.MCALLISTER INITIAL RELEASE           *
      *   V1.01  1995-05-20  R.T.WHEELER  CENTURY PIVOT APPLIED TO THE*
      *                      CYCLE DATE                               *
      *   V1.03  2006-10-09  C.ADEYEMI    CENTURY PIVOT APPLIED TO THE*
      *                      CYCLE DATE                               *
      *   V1.04  2016-12-07  K.O.BRIEN    EFFECTIVE DATE FILTER ADDED *
      *                      PER AUDIT FINDING                        *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUXR03.
       AUTHOR. TELCABS APPLICATIONS - CROSS REFERENCE REPORT.
      *****************************************************************
      * RATE ELEMENT TO TARIFF CROSS REFERENCE. THE STEP RUNS ONCE PER*
      * BILL CYCLE AND IS RERUN FROM THE TOP IF IT FAILS.             *
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES     *
      * RATHER THAN LOW VALUES.                                       *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           C01 IS TO-NEW-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT XRFIN ASSIGN TO UT-S-XRFIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT LNKOUT ASSIGN TO UT-S-LNKOUT
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
      * XRFIN - PERMANENT DATASET HELD ON DASD.
       FD  XRFIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 110 CHARACTERS.
       01  CABS-BN-IN-RECORD.
           05  IB-LEVEL                    PIC X(08).
           05  IB-TYPE                     PIC 9(06).
           05  IB-INVOICE                  PIC S9(15) COMP-3.
           05  IB-JURIS                    PIC S9(11)V9(05) COMP-3.
           05  IB-TARIFF                   PIC X(08).
           05  IB-CODE                     PIC S9(11) COMP-3.
           05  IB-CARRIER                  PIC X(20).
           05  IB-STATE                    PIC S9(13) COMP-3.
           05  IB-CARRIER2                 PIC X(03).
           05  IB-JURIS2                   PIC S9(11)V9(02) COMP-3.
           05  IB-JURIS3                   PIC X(10).
           05  IB-LEVEL2                   PIC 9(02).
           05  IB-GROUP                    PIC S9(13) COMP-3.
           05  BN-FILL-01                  PIC X(9).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-BN-VIEW1 REDEFINES CABS-BN-IN-RECORD.
           05  R0B-INVOICE                 PIC X(03).
           05  R0B-MEDIA                   PIC X(10).
           05  R0B-SEGMENT                 PIC S9(15) COMP-3.
           05  R0B-STATUS                  PIC S9(11)V9(05) COMP-3.
           05  R0B-BAND                    PIC S9(09)V9(02) COMP-3.
           05  R0B-INVOICE2                PIC X(02).
           05  R0B-ELEM                    PIC X(02).
           05  R0B-OCN                     PIC S9(09)V9(02) COMP-3.
           05  R0B-REST                    PIC X(64).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-BN-VIEW2 REDEFINES CABS-BN-IN-RECORD.
           05  R1B-OCN                     PIC X(10).
           05  R1B-STATE                   PIC X(06).
           05  R1B-SEGMENT                 PIC 9(04).
           05  R1B-INVOICE                 PIC X(20).
           05  R1B-REST                    PIC X(70).
      * LNKOUT - WORK FILE, DELETED AT STEP END.
       FD  LNKOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-BN-OUT-RECORD.
           05  OB-OCN                      PIC X(02).
           05  OB-CYCLE                    PIC 9(05).
           05  OB-LEVEL                    PIC S9(11)V9(02) COMP-3.
           05  OB-JURIS                    PIC X(08).
           05  OB-TARGET                   PIC X(04).
           05  OB-BAND                     PIC 9(04).
           05  OB-BAN                      PIC 9(06).
           05  OB-STATE                    PIC S9(07)V9(02) COMP-3.
           05  OB-OCN2                     PIC X(04).
           05  OB-BAND2                    PIC 9(02).
           05  OB-ACCOUNT                  PIC S9(07)V9(02) COMP-3.
           05  OB-SEQ                      PIC X(04).
           05  BN-FILL-02                  PIC X(24).
      * CTLOUT - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD              PIC X(180).
      * RPTOUT - CATALOGUED GENERATION DATA GROUP.
       FD  RPTOUT
           RECORDING MODE IS FA
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       COPY CABSPRNT.
       WORKING-STORAGE SECTION.
      * STANDARD SHARED WORKING STORAGE. SEE CABSWRK.
       COPY CABSWRK.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUXR03'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V1.23'.
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
           05  WS-BN-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BN-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BN-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BN-CNT-04                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-BN-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BN-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-BN-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BN-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-BN-TXT-01                PIC X(20) VALUE SPACES.
           05  WS-BN-TXT-02                PIC X(30) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-BN-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-BN-ON-01                 VALUE 'Y'.
               88  WS-BN-OFF-01                VALUE 'N'.
           05  WS-BN-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-BN-ON-02                 VALUE 'Y'.
               88  WS-BN-OFF-02                VALUE 'N'.
           05  WS-BN-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-BN-ON-03                 VALUE 'Y'.
               88  WS-BN-OFF-03                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-BN-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-BN-SUB-02                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. LOADED ONCE AT INITIALISATION AND SEARCHED WITH
      * A SUBSCRIPTED WALK.
       01  WS-BN-TABLE.
           05  WS-BN-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-BN-TB-ENTRY OCCURS 80 TIMES
                                       INDEXED BY WS-BN-IX.
               10  WS-BN-TB-KEY                PIC X(06).
               10  WS-BN-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-BN-TB-TXT                PIC X(40).
               10  WS-BN-TB-EFF                PIC 9(05).
               10  WS-BN-TB-EXP                PIC 9(05).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 58.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUXR03 - RATE ELEMENT TO TARIFF CROSS REFERENCE'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-BN-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-BN-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9983.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-BN-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-BN-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT XRFIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'XRFIN NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT LNKOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'LNKOUT NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CTLOUT NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RPTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RPTOUT NOT AVAILABLE - OPEN REJECTED' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-BN-CYCLE-YYDDD.
           COMPUTE WS-BN-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-BN-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-BN-CNT-03.
           MOVE 0 TO WS-BN-CNT-04.
           MOVE 0 TO WS-BN-CNT-01.
       P1200-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-BN-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-BN-TAB-CNT NOT < 80
               MOVE 'Y' TO WS-BN-SW-01
               ADD 1 TO WS-BN-CNT-02
           ELSE
               ADD 1 TO WS-BN-TAB-CNT
               SET WS-BN-IX TO WS-BN-TAB-CNT
               MOVE IB-INVOICE TO WS-BN-TB-KEY (WS-BN-IX)
               MOVE 0 TO WS-BN-TB-VAL (WS-BN-IX)
               MOVE SPACES TO WS-BN-TB-TXT (WS-BN-IX)
               ADD 1 TO WS-BN-CNT-01.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ XRFIN
               AT END MOVE 'Y' TO WS-BN-SW-01.
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
           PERFORM P2200-EXPAND-REFERENCE THRU
               P2200-EXPAND-REFERENCE-EXIT.
           PERFORM P2300-APPLY-LINK THRU P2300-APPLY-LINK-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ XRFIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-EXPAND-REFERENCE.
           IF WS-BN-AMT-01 NOT = 0
               COMPUTE WS-BN-QTY-01 = WS-BN-AMT-02 * 100 / WS-BN-AMT-01
           ELSE
               MOVE 0 TO WS-BN-QTY-01.
           MOVE 'N' TO WS-BN-SW-01.
           IF WS-BN-TAB-CNT > 0
               PERFORM P250-COMPARE-GROUP THRU P250-COMPARE-GROUP-EXIT
               VARYING WS-BN-SUB-02 FROM 1 BY 1
               UNTIL WS-BN-SUB-02 > WS-BN-TAB-CNT
               OR WS-BN-SW-01 = 'Y'.
       P2200-EXPAND-REFERENCE-EXIT.
           EXIT.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P2300-APPLY-LINK.
           MOVE 'N' TO WS-BN-SW-01.
           IF WS-BN-TXT-01 NOT = WS-BN-TXT-01
               MOVE 'Y' TO WS-BN-SW-01
               MOVE WS-BN-TXT-01 TO WS-BN-TXT-01
               ADD 1 TO WS-BN-CNT-04.
       P2300-APPLY-LINK-EXIT.
           EXIT.
       P250-COMPARE-GROUP.
           SET WS-BN-IX TO WS-BN-SUB-02.
           IF WS-BN-TB-KEY (WS-BN-IX) = IB-STATE
               MOVE 'Y' TO WS-BN-SW-02
               MOVE WS-BN-TB-VAL (WS-BN-IX) TO WS-BN-QTY-02
               MOVE WS-BN-SUB-02 TO WS-BN-SUB-01.
       P250-COMPARE-GROUP-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P3100-WRITE-LINK.
           MOVE SPACES TO WS-BN-TXT-02.
           STRING IB-GROUP DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IB-JURIS2 DELIMITED BY SIZE
               INTO WS-BN-TXT-02.
       P3100-WRITE-LINK-EXIT.
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
           MOVE 'READ FROM INPUT' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO WS-BN-CNT-EDIT.
           MOVE WS-BN-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'WRITTEN TO OUTPUT' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-BN-CNT-EDIT.
           MOVE WS-BN-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'REJECTED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-BN-CNT-EDIT.
           MOVE WS-BN-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'ROLLED INTO SUMMARY' TO PC-COL-001-020.
           MOVE WS-SUMM-CNT TO WS-BN-CNT-EDIT.
           MOVE WS-BN-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'CARRIED FORWARD' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-BN-CNT-EDIT.
           MOVE WS-BN-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-BN-CNT-01 TO WS-BN-CNT-EDIT.
           MOVE WS-BN-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-FILLER.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE 7 TO CT-STEP-SEQ.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE WS-BN-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
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
           CLOSE XRFIN.
           CLOSE LNKOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABUXR03 - RUN COMPLETE'.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  BN-CNT-02 = ' WS-BN-CNT-02.
           DISPLAY '  BN-CNT-03 = ' WS-BN-CNT-03.
           DISPLAY '  BN-CNT-04 = ' WS-BN-CNT-04.
       P9000-EXIT.
           EXIT.
