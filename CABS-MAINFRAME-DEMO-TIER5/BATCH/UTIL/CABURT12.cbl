      *****************************************************************
      * CABURT12 - RATE ELEMENT DESCRIPTION MAINTENANCE               *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               RATIN   TELCABS.CABS.RATIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               TBLOUT  TELCABS.CABS.TBLOUT         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - NOTHING IS UPDATED IN PLACE        *
      * REVISION HISTORY                                              *
      *   V1.00  1989-03-16  B.R.HALVORSEN INITIAL RELEASE            *
      *   V1.01  2006-08-13  M.DELACROIX  PARM CARD EXTENDED,         *
      *                      POSITIONS 40 THROUGH 48                  *
      *   V1.04  2013-12-28  A.BUKOWSKI   RESTART KEY WRITTEN SO A    *
      *                      RERUN CAN POSITION                       *
      *   V1.08  2017-06-24  C.ADEYEMI    REPORT PAGINATION CORRECTED *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABURT12.
       AUTHOR. TELCABS APPLICATIONS - RATE TABLE MAINTENANCE.
      *****************************************************************
      * RATE ELEMENT DESCRIPTION MAINTENANCE. THE STEP IS DRIVEN      *
      * ENTIRELY FROM THE SYSIN PARM CARD AND THE DD ALLOCATIONS IN   *
      * THE JOB.                                                      *
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF  *
      * ZERO IS OPEN ENDED.                                           *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           C01 IS TO-NEW-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RATIN ASSIGN TO UT-S-RATIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT TBLOUT ASSIGN TO UT-S-TBLOUT
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
      * RATIN - PERMANENT DATASET HELD ON DASD.
       FD  RATIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 100 CHARACTERS.
       01  CABS-AX-IN-RECORD.
           05  IA-CODE                     PIC X(02).
           05  IA-CARRIER                  PIC S9(11)V9(02) COMP-3.
           05  IA-LEVEL                    PIC 9(05).
           05  IA-CARRIER2                 PIC S9(07)V9(02) COMP-3.
           05  IA-STATE                    PIC 9(02).
           05  IA-OCN                      PIC S9(09)V9(02) COMP-3.
           05  IA-LEVEL2                   PIC X(16).
           05  IA-SOURCE                   PIC X(08).
           05  IA-TARIFF                   PIC X(20).
           05  IA-ELEM                     PIC X(20).
           05  IA-CARRIER3                 PIC X(02).
           05  IA-CYCLE                    PIC S9(07) COMP-3.
           05  AX-FILL-01                  PIC X(3).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-AX-VIEW1 REDEFINES CABS-AX-IN-RECORD.
           05  R0A-TARGET                  PIC 9(09).
           05  R0A-TYPE                    PIC X(16).
           05  R0A-TYPE2                   PIC S9(07)V9(02) COMP-3.
           05  R0A-JURIS                   PIC S9(13) COMP-3.
           05  R0A-TARIFF                  PIC S9(07)V9(02) COMP-3.
           05  R0A-CENTRE                  PIC S9(13) COMP-3.
           05  R0A-REST                    PIC X(51).
      * TBLOUT - WORK FILE, DELETED AT STEP END.
       FD  TBLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 110 CHARACTERS.
       01  CABS-AX-OUT-RECORD.
           05  OA-BAND                     PIC 9(09).
           05  OA-CENTRE                   PIC X(04).
           05  OA-CYCLE                    PIC S9(15) COMP-3.
           05  OA-LEVEL                    PIC 9(07).
           05  OA-STATUS                   PIC S9(09) COMP-3.
           05  OA-TARIFF                   PIC S9(05) COMP-3.
           05  OA-ACCOUNT                  PIC X(16).
           05  OA-TARIFF2                  PIC S9(09)V9(02) COMP-3.
           05  OA-CODE                     PIC X(16).
           05  OA-PERIOD                   PIC X(20).
           05  OA-CYCLE2                   PIC 9(07).
           05  AX-FILL-02                  PIC X(9).
      * CTLOUT - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD              PIC X(180).
      * RPTOUT - PERMANENT DATASET HELD ON DASD.
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
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABURT12'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V2.20'.
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
       01  WS-COUNT-AREA.
           05  WS-AX-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AX-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AX-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AX-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AX-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AX-CNT-06                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-AX-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AX-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AX-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-AX-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AX-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AX-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-AX-TXT-01                PIC X(20) VALUE SPACES.
           05  WS-AX-TXT-02                PIC X(08) VALUE SPACES.
           05  WS-AX-TXT-03                PIC X(08) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-AX-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-AX-ON-01                 VALUE 'Y'.
               88  WS-AX-OFF-01                VALUE 'N'.
           05  WS-AX-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-AX-ON-02                 VALUE 'Y'.
               88  WS-AX-OFF-02                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-AX-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-AX-SUB-02                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. A ROW THAT DOES NOT FIT IS COUNTED AND DROPPED
      * - THE STEP DOES NOT FAIL.
       01  WS-AX-TABLE.
           05  WS-AX-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-AX-TB-ENTRY OCCURS 100 TIMES
                                       INDEXED BY WS-AX-IX.
               10  WS-AX-TB-KEY                PIC X(13).
               10  WS-AX-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-AX-TB-TXT                PIC X(20).
               10  WS-AX-TB-EFF                PIC 9(05).
               10  WS-AX-TB-EXP                PIC 9(05).
       01  WS-AX-WORK-GROUP-1.
           05  WS-AX-G1-CLASS              PIC S9(09) COMP-3.
           05  WS-AX-G1-JURIS              PIC X(10).
           05  WS-AX-G1-CYCLE              PIC 9(05).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 50.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABURT12 - RATE ELEMENT DESCRIPTION MAINTENANCE'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-AX-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-AX-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9913.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-AX-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-AX-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT RATIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RATIN OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               DISPLAY 'RATIN FILE STATUS = ' WS-FS-INPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT TBLOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'TBLOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               DISPLAY 'TBLOUT FILE STATUS = ' WS-FS-OUTPUT
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
           MOVE PC1-CYCLE-YYDDD TO WS-AX-CYCLE-YYDDD.
           COMPUTE WS-AX-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-AX-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-AX-CNT-03.
           MOVE 0 TO WS-AX-CNT-04.
           MOVE 0 TO WS-AX-CNT-05.
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
           PERFORM P2200-BUILD-WINDOW THRU P2200-BUILD-WINDOW-EXIT.
           PERFORM P2300-CHECK-WINDOW THRU P2300-CHECK-WINDOW-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ RATIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-BUILD-WINDOW.
           CALL 'CABHASH' USING WS-AX-TXT-02 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-AX-CNT-02.
       P2200-BUILD-WINDOW-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P2300-CHECK-WINDOW.
           MOVE 0 TO WS-AX-QTY-02.
           MOVE 0 TO WS-AX-QTY-01.
           MOVE 0 TO WS-AX-QTY-03.
           MOVE 0 TO WS-AX-AMT-02.
       P2300-CHECK-WINDOW-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-FORMAT-DESCRIPTION.
           MOVE IA-CARRIER TO WS-AX-TXT-02.
           MOVE IA-CYCLE TO WS-AX-TXT-03.
           MOVE IA-ELEM TO WS-AX-TXT-02.
           ADD 1 TO WS-AX-CNT-01.
       P3100-FORMAT-DESCRIPTION-EXIT.
           EXIT.
       P3200-POST-ELEMENT.
           MOVE 0 TO WS-AX-QTY-01.
           MOVE 0 TO WS-AX-QTY-02.
           MOVE 0 TO WS-AX-QTY-03.
           MOVE 0 TO WS-AX-AMT-01.
       P3200-POST-ELEMENT-EXIT.
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
           MOVE 'INPUT RECORDS' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO WS-AX-CNT-EDIT.
           MOVE WS-AX-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'OUTPUT RECORDS' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-AX-CNT-EDIT.
           MOVE WS-AX-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'SUSPENDED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-AX-CNT-EDIT.
           MOVE WS-AX-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'SUMMARISED' TO PC-COL-001-020.
           MOVE WS-SUMM-CNT TO WS-AX-CNT-EDIT.
           MOVE WS-AX-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'HELD FOR NEXT RUN' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-AX-CNT-EDIT.
           MOVE WS-AX-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-AX-CNT-01 TO WS-AX-CNT-EDIT.
           MOVE WS-AX-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE WS-AX-CNT-01 TO CT-RC.
           MOVE SPACES TO CT-FILLER.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE WS-AX-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE 2 TO CT-STEP-SEQ.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
       P8100-EXIT.
           EXIT.
      * P8200-CHECK-BALANCE - A RECORD THAT WAS HELD FOR THE NEXT
      * CYCLE IS BROUGHT INTO THE CARRIED FORWARD SIDE BEFORE THE
      * EQUATION IS TESTED.
       P8200-CHECK-BALANCE.
           ADD WS-AX-CNT-06 TO CT-CARRIED-FWD.
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
           CLOSE RATIN.
           CLOSE TBLOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABURT12 - STEP COMPLETE'.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  AX-CNT-06 = ' WS-AX-CNT-06.
           DISPLAY '  AX-CNT-05 = ' WS-AX-CNT-05.
           DISPLAY '  AX-CNT-01 = ' WS-AX-CNT-01.
       P9000-EXIT.
           EXIT.
