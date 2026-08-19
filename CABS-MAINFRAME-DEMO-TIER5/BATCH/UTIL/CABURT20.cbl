      *****************************************************************
      * CABURT20 - TARIFF CODE TABLE REFRESH                          *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               MNTIN   TELCABS.CABS.MNTIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               RATOUT  TELCABS.CABS.RATOUT         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - NOTHING IS UPDATED IN PLACE        *
      * REVISION HISTORY                                              *
      *   V1.00  1987-04-24  J.M.CASTILLO INITIAL RELEASE             *
      *   V1.02  1992-09-16  C.ADEYEMI    HASH TOTAL ADDED TO THE     *
      *                      CONTROL RECORD                           *
      *   V1.04  2011-05-16  W.J.MCALLISTER JOB PARAMETER MADE        *
      *                      MANDATORY                                *
      *   V1.08  2019-12-15  K.O.BRIEN    RESTART KEY WRITTEN SO A    *
      *                      RERUN CAN POSITION                       *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABURT20.
       AUTHOR. TELCABS APPLICATIONS - RATE TABLE MAINTENANCE.
      *****************************************************************
      * TARIFF CODE TABLE REFRESH. THE STEP RUNS ONCE PER BILL CYCLE  *
      * AND IS RERUN FROM THE TOP IF IT FAILS.                        *
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE   *
      * MORE AT END OF FILE.                                          *
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
           SELECT RATOUT ASSIGN TO UT-S-RATOUT
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
      * MNTIN - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  MNTIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-DR-IN-RECORD.
           05  ID-ELEM                     PIC S9(09)V9(05) COMP-3.
           05  ID-JURIS                    PIC S9(11) COMP-3.
           05  ID-LEVEL                    PIC X(04).
           05  ID-BAND                     PIC S9(09)V9(05) COMP-3.
           05  ID-CENTRE                   PIC 9(06).
           05  ID-TYPE                     PIC X(16).
           05  ID-ELEM2                    PIC X(16).
           05  ID-SOURCE                   PIC 9(06).
           05  ID-TARGET                   PIC 9(03).
           05  DR-FILL-01                  PIC X(7).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-DR-VIEW1 REDEFINES CABS-DR-IN-RECORD.
           05  R0D-CODE                    PIC S9(07)V9(02) COMP-3.
           05  R0D-CARRIER                 PIC 9(05).
           05  R0D-GROUP                   PIC S9(09)V9(02) COMP-3.
           05  R0D-CIRCUIT                 PIC S9(07) COMP-3.
           05  R0D-TARIFF                  PIC 9(04).
           05  R0D-TARGET                  PIC X(20).
           05  R0D-CODE2                   PIC X(02).
           05  R0D-REST                    PIC X(34).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-DR-VIEW2 REDEFINES CABS-DR-IN-RECORD.
           05  R1D-TYPE                    PIC S9(13)V9(05) COMP-3.
           05  R1D-STATUS                  PIC S9(09)V9(05) COMP-3.
           05  R1D-ACCOUNT                 PIC X(13).
           05  R1D-CENTRE                  PIC S9(05) COMP-3.
           05  R1D-CLASS                   PIC S9(05) COMP-3.
           05  R1D-CLASS2                  PIC S9(09)V9(05) COMP-3.
           05  R1D-REST                    PIC X(35).
      * RATOUT - WORK FILE, DELETED AT STEP END.
       FD  RATOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-DR-OUT-RECORD.
           05  OD-GROUP                    PIC S9(11) COMP-3.
           05  OD-CIRCUIT                  PIC 9(09).
           05  OD-CENTRE                   PIC S9(09) COMP-3.
           05  OD-PERIOD                   PIC 9(02).
           05  OD-ELEM                     PIC S9(09)V9(02) COMP-3.
           05  OD-REGION                   PIC 9(05).
           05  OD-CARRIER                  PIC 9(04).
           05  OD-SOURCE                   PIC S9(09) COMP-3.
           05  OD-CLASS                    PIC X(03).
           05  OD-TARGET                   PIC X(06).
           05  DR-FILL-02                  PIC X(29).
      * CTLOUT - WORK FILE, DELETED AT STEP END.
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD              PIC X(180).
      * RPTOUT - SEQUENTIAL, BLOCKED BY THE SYSTEM.
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
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABURT20'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V3.09'.
           05  WS-TABLE-LIMIT              PIC S9(04) COMP-3
                                         VALUE 100.
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
           05  WS-DR-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DR-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DR-CNT-03                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-DR-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DR-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DR-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-DR-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DR-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DR-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DR-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-DR-TXT-01                PIC X(10) VALUE SPACES.
           05  WS-DR-TXT-02                PIC X(30) VALUE SPACES.
           05  WS-DR-TXT-03                PIC X(10) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-DR-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-DR-ON-01                 VALUE 'Y'.
               88  WS-DR-OFF-01                VALUE 'N'.
           05  WS-DR-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-DR-ON-02                 VALUE 'Y'.
               88  WS-DR-OFF-02                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-DR-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DR-SUB-02                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. THE LIMIT WAS RAISED WHEN THE SOUTHEAST CENTRES
      * WERE MERGED AND HAS NOT BEEN REVIEWED SINCE.
       01  WS-DR-TABLE.
           05  WS-DR-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-DR-TB-ENTRY OCCURS 100 TIMES
                                       INDEXED BY WS-DR-IX.
               10  WS-DR-TB-KEY                PIC X(06).
               10  WS-DR-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-DR-TB-TXT                PIC X(20).
               10  WS-DR-TB-EFF                PIC 9(05).
               10  WS-DR-TB-EXP                PIC 9(05).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 60.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABURT20 - TARIFF CODE TABLE REFRESH'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-DR-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-DR-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9986.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-DR-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-DR-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
               MOVE 'MNTIN OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               DISPLAY 'MNTIN FILE STATUS = ' WS-FS-INPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RATOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RATOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               DISPLAY 'RATOUT FILE STATUS = ' WS-FS-OUTPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CTLOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               DISPLAY 'CTLOUT FILE STATUS = ' WS-FS-CONTROL
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RPTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RPTOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               DISPLAY 'RPTOUT FILE STATUS = ' WS-FS-OUTPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
       P1100-EXIT.
           EXIT.
      * P1200-READ-PARM - THE CYCLE DATE ARRIVES AS TWO DIGITS AND IS
      * PIVOTED ON DW-PIVOT-YY BEFORE ANY DATE MATH.
       P1200-READ-PARM.
           MOVE SPACES TO WS-PARM-CARD.
           ACCEPT WS-PARM-CARD FROM SYSIN.
           CALL 'CABPARMR' USING WS-PARM-CARD WS-RC-PARMR.
           MOVE PC1-CYCLE-YYDDD TO WS-DR-CYCLE-YYDDD.
           COMPUTE WS-DR-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-DR-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-DR-CNT-03.
           MOVE 0 TO WS-DR-CNT-02.
           MOVE 0 TO WS-DR-CNT-01.
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
           PERFORM P2200-BUILD-ROW THRU P2200-BUILD-ROW-EXIT.
           PERFORM P2300-SPLIT-ROW THRU P2300-SPLIT-ROW-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ MNTIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-BUILD-ROW.
           CALL 'CABSEQCK' USING WS-DR-TXT-03 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-DR-CNT-01.
       P2200-BUILD-ROW-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P2300-SPLIT-ROW.
           MOVE 'Y' TO WS-DR-SW-02.
           IF ID-CENTRE < 12
               MOVE 'N' TO WS-DR-SW-02
               ADD 1 TO WS-DR-CNT-02.
           IF ID-CENTRE > 2386
               MOVE 'N' TO WS-DR-SW-02
               ADD 1 TO WS-DR-CNT-01.
       P2300-SPLIT-ROW-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-EMIT-TARIFF.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-DR-TXT-01 TO PC-COL-001-020.
           MOVE WS-DR-TXT-02 TO PC-COL-021-060.
           MOVE WS-DR-AMT-04 TO WS-DR-AMT-EDIT.
           MOVE WS-DR-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P3100-EMIT-TARIFF-EXIT.
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
           MOVE 'RECORDS CARRIED FWD' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-DR-CNT-EDIT.
           MOVE WS-DR-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS READ' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO WS-DR-CNT-EDIT.
           MOVE WS-DR-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS REJECTED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-DR-CNT-EDIT.
           MOVE WS-DR-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS SUMMARISED' TO PC-COL-001-020.
           MOVE WS-SUMM-CNT TO WS-DR-CNT-EDIT.
           MOVE WS-DR-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS WRITTEN' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-DR-CNT-EDIT.
           MOVE WS-DR-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-DR-CNT-01 TO WS-DR-CNT-EDIT.
           MOVE WS-DR-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE WS-DR-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE 6 TO CT-STEP-SEQ.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-DR-TXT-03 TO CT-RESTART-KEY.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE SPACES TO CT-FILLER.
           MOVE WS-DR-CNT-01 TO CT-RC.
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
           CLOSE MNTIN.
           CLOSE RATOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABURT20 - STEP COMPLETE'.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  DR-CNT-01 = ' WS-DR-CNT-01.
           DISPLAY '  DR-CNT-03 = ' WS-DR-CNT-03.
           DISPLAY '  DR-CNT-02 = ' WS-DR-CNT-02.
       P9000-EXIT.
           EXIT.
