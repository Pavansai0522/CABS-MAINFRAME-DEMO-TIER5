      *****************************************************************
      * CABUEX17 - USAGE EXTRACT BY JURISDICTION                      *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               SELIN   TELCABS.CABS.SELIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               CIROUT  TELCABS.CABS.CIROUT         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - THE OUTPUT IS REBUILT EACH RUN     *
      * REVISION HISTORY                                              *
      *   V1.00  1989-01-09  T.YAMASHITA  INITIAL RELEASE             *
      *   V1.02  1993-11-27  R.T.WHEELER  EFFECTIVE DATE FILTER ADDED *
      *                      PER AUDIT FINDING                        *
      *   V1.05  1997-09-01  C.ADEYEMI    SECOND OUTPUT FILE ADDED FOR*
      *                      THE FACTOR STUDY                         *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUEX17.
       AUTHOR. TELCABS APPLICATIONS - DATASET EXTRACT.
      *****************************************************************
      * USAGE EXTRACT BY JURISDICTION. THE STEP IS SCHEDULED MONTHLY  *
      * AND ALSO RUN ON DEMAND WHEN A CENTRE ASKS FOR IT.             *
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE     *
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.                      *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           C01 IS TO-NEW-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SELIN ASSIGN TO UT-S-SELIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT CIROUT ASSIGN TO UT-S-CIROUT
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
      * SELIN - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  SELIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-EA-IN-RECORD.
           05  IE-SEQ                      PIC S9(07) COMP-3.
           05  IE-STATUS                   PIC S9(09)V9(05) COMP-3.
           05  IE-CYCLE                    PIC X(06).
           05  IE-PERIOD                   PIC 9(05).
           05  IE-CARRIER                  PIC X(02).
           05  IE-CIRCUIT                  PIC 9(05).
           05  IE-CODE                     PIC 9(07).
           05  IE-CODE2                    PIC S9(13) COMP-3.
           05  EA-FILL-01                  PIC X(36).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-EA-VIEW1 REDEFINES CABS-EA-IN-RECORD.
           05  R0E-CLASS                   PIC X(08).
           05  R0E-SEQ                     PIC X(03).
           05  R0E-JURIS                   PIC X(13).
           05  R0E-CIRCUIT                 PIC X(16).
           05  R0E-GROUP                   PIC X(06).
           05  R0E-BAN                     PIC 9(07).
           05  R0E-REST                    PIC X(27).
      * CIROUT - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  CIROUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-EA-OUT-RECORD.
           05  OE-CLASS                    PIC X(08).
           05  OE-CARRIER                  PIC S9(11)V9(02) COMP-3.
           05  OE-ELEM                     PIC X(08).
           05  OE-PERIOD                   PIC S9(11)V9(02) COMP-3.
           05  OE-TARIFF                   PIC X(03).
           05  OE-SEQ                      PIC S9(13)V9(02) COMP-3.
           05  OE-SEQ2                     PIC 9(04).
           05  EA-FILL-02                  PIC X(35).
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
      * SHARED LAYOUT PULLED IN FOR THE SELECTION SIDE.
       COPY CABSCIRC.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUEX17'.
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
           05  WS-EA-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-EA-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-EA-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-EA-CNT-04                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-EA-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-EA-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-EA-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-EA-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-EA-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-EA-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-EA-TXT-01                PIC X(10) VALUE SPACES.
           05  WS-EA-TXT-02                PIC X(08) VALUE SPACES.
           05  WS-EA-TXT-03                PIC X(30) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-EA-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-EA-ON-01                 VALUE 'Y'.
               88  WS-EA-OFF-01                VALUE 'N'.
           05  WS-EA-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-EA-ON-02                 VALUE 'Y'.
               88  WS-EA-OFF-02                VALUE 'N'.
           05  WS-EA-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-EA-ON-03                 VALUE 'Y'.
               88  WS-EA-OFF-03                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-EA-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-EA-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-EA-SUB-03                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. THE LIMIT WAS RAISED WHEN THE SOUTHEAST CENTRES
      * WERE MERGED AND HAS NOT BEEN REVIEWED SINCE.
       01  WS-EA-TABLE.
           05  WS-EA-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-EA-TB-ENTRY OCCURS 80 TIMES
                                       INDEXED BY WS-EA-IX.
               10  WS-EA-TB-KEY                PIC X(04).
               10  WS-EA-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-EA-TB-TXT                PIC X(20).
               10  WS-EA-TB-EFF                PIC 9(05).
               10  WS-EA-TB-EXP                PIC 9(05).
       01  WS-EA-WORK-GROUP-1.
           05  WS-EA-G1-OCN                PIC S9(09) COMP-3.
           05  WS-EA-G1-TARIFF             PIC 9(05).
           05  WS-EA-G1-CLASS              PIC X(10).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 55.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUEX17 - USAGE EXTRACT BY JURISDICTION'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-EA-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-EA-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9916.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-EA-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-EA-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT SELIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'SELIN COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CIROUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CIROUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CTLOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RPTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RPTOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-EA-CYCLE-YYDDD.
           COMPUTE WS-EA-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-EA-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-EA-CNT-04.
           MOVE 0 TO WS-EA-CNT-03.
           MOVE 0 TO WS-EA-CNT-01.
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
           PERFORM P2200-EXPAND-RANGE THRU P2200-EXPAND-RANGE-EXIT.
           IF WS-EA-ON-03
               PERFORM P2300-APPLY-FILTER THRU P2300-APPLY-FILTER-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ SELIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P2200-EXPAND-RANGE.
           MOVE 'Y' TO WS-EA-SW-03.
           IF IE-CODE < 16
               MOVE 'N' TO WS-EA-SW-03
               ADD 1 TO WS-EA-CNT-01.
           IF IE-CODE > 3559
               MOVE 'N' TO WS-EA-SW-03
               ADD 1 TO WS-EA-CNT-04.
       P2200-EXPAND-RANGE-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P2300-APPLY-FILTER.
           MOVE WS-EA-AMT-01 TO WS-EA-AMT-02.
           IF WS-EA-AMT-02 < 0
               COMPUTE WS-EA-AMT-02 = 0 - WS-EA-AMT-01.
       P2300-APPLY-FILTER-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P3100-CLOSE-OFF-RANGE.
           CALL 'CABHASH' USING IE-STATUS WS-ACC-OCN-HASH.
           ADD WS-EA-CNT-04 TO WS-ACC-SEQ-HASH.
       P3100-CLOSE-OFF-RANGE-EXIT.
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
           MOVE 'DETAIL SUSPENDED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-EA-CNT-EDIT.
           MOVE WS-EA-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL CARRIED FWD' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-EA-CNT-EDIT.
           MOVE WS-EA-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL OUT' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-EA-CNT-EDIT.
           MOVE WS-EA-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL SUMMARISED' TO PC-COL-001-020.
           MOVE WS-SUMM-CNT TO WS-EA-CNT-EDIT.
           MOVE WS-EA-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL IN' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO WS-EA-CNT-EDIT.
           MOVE WS-EA-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-EA-CNT-01 TO WS-EA-CNT-EDIT.
           MOVE WS-EA-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 02' TO PC-COL-001-020.
           MOVE WS-EA-CNT-02 TO WS-EA-CNT-EDIT.
           MOVE WS-EA-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 03' TO PC-COL-001-020.
           MOVE WS-EA-CNT-03 TO WS-EA-CNT-EDIT.
           MOVE WS-EA-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-EA-TXT-01 TO CT-RESTART-KEY.
           MOVE SPACES TO CT-FILLER.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE 3 TO CT-STEP-SEQ.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE WS-EA-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
       P8100-EXIT.
           EXIT.
      * P8200-CHECK-BALANCE - A RECORD THAT WAS HELD FOR THE NEXT
      * CYCLE IS BROUGHT INTO THE CARRIED FORWARD SIDE BEFORE THE
      * EQUATION IS TESTED.
       P8200-CHECK-BALANCE.
           ADD WS-EA-CNT-01 TO CT-CARRIED-FWD.
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
           CLOSE SELIN.
           CLOSE CIROUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABUEX17 - NORMAL END OF JOB'.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  EA-CNT-01 = ' WS-EA-CNT-01.
           DISPLAY '  EA-CNT-02 = ' WS-EA-CNT-02.
           DISPLAY '  EA-CNT-03 = ' WS-EA-CNT-03.
       P9000-EXIT.
           EXIT.
