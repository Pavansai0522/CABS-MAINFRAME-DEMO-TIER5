      *****************************************************************
      * CABUEX22 - SUSPENSE EXTRACT FOR THE RECYCLE JOB               *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               MSTIN   TELCABS.CABS.MSTIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               DROPOUT TELCABS.CABS.DROPOU         (LOCAL)     *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * REVISION HISTORY                                              *
      *   V1.00  1988-08-14  R.T.WHEELER  INITIAL RELEASE             *
      *   V1.04  2006-06-10  K.O.BRIEN    OCCURS RAISED AFTER THE     *
      *                      FEBRUARY OVERFLOW                        *
      *   V1.06  2011-10-13  W.J.MCALLISTER REPORT PAGINATION         *
      *                      CORRECTED                                *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUEX22.
       AUTHOR. TELCABS APPLICATIONS - DATASET EXTRACT.
      *****************************************************************
      * SUSPENSE EXTRACT FOR THE RECYCLE JOB. THE STEP RUNS ONCE PER  *
      * BILL CYCLE AND IS RERUN FROM THE TOP IF IT FAILS.             *
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
           SELECT MSTIN ASSIGN TO UT-S-MSTIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT DROPOUT ASSIGN TO UT-S-DROPOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT CTLOUT ASSIGN TO UT-S-CTLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      * MSTIN - CATALOGUED GENERATION DATA GROUP.
       FD  MSTIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-DL-IN-RECORD.
           05  ID-GROUP                    PIC 9(06).
           05  ID-CYCLE                    PIC X(08).
           05  ID-JURIS                    PIC X(04).
           05  ID-CLASS                    PIC X(03).
           05  ID-TYPE                     PIC X(02).
           05  ID-INVOICE                  PIC X(03).
           05  ID-ACCOUNT                  PIC S9(09) COMP-3.
           05  ID-CIRCUIT                  PIC S9(13) COMP-3.
           05  ID-TARGET                   PIC 9(05).
           05  ID-CODE                     PIC X(04).
           05  ID-TARIFF                   PIC S9(11)V9(02) COMP-3.
           05  ID-TARIFF2                  PIC S9(13)V9(05) COMP-3.
           05  ID-CLASS2                   PIC X(03).
           05  DL-FILL-01                  PIC X(13).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-DL-VIEW1 REDEFINES CABS-DL-IN-RECORD.
           05  R0D-ELEM                    PIC S9(09)V9(05) COMP-3.
           05  R0D-BAN                     PIC 9(03).
           05  R0D-ELEM2                   PIC S9(15) COMP-3.
           05  R0D-REGION                  PIC 9(04).
           05  R0D-ELEM3                   PIC X(16).
           05  R0D-BAND                    PIC S9(11)V9(02) COMP-3.
           05  R0D-REST                    PIC X(34).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-DL-VIEW2 REDEFINES CABS-DL-IN-RECORD.
           05  R1D-ACCOUNT                 PIC X(06).
           05  R1D-PERIOD                  PIC S9(09)V9(02) COMP-3.
           05  R1D-BAN                     PIC S9(07) COMP-3.
           05  R1D-TYPE                    PIC S9(07)V9(05) COMP-3.
           05  R1D-CENTRE                  PIC X(02).
           05  R1D-BAND                    PIC X(03).
           05  R1D-JURIS                   PIC X(13).
           05  R1D-REST                    PIC X(39).
      * DROPOUT - WORK FILE, DELETED AT STEP END.
       FD  DROPOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-DL-OUT-RECORD.
           05  OD-SEQ                      PIC S9(11) COMP-3.
           05  OD-TYPE                     PIC S9(13) COMP-3.
           05  OD-LEVEL                    PIC X(04).
           05  OD-CYCLE                    PIC S9(15) COMP-3.
           05  OD-INVOICE                  PIC S9(09)V9(05) COMP-3.
           05  OD-ACCOUNT                  PIC S9(11)V9(02) COMP-3.
           05  OD-CYCLE2                   PIC X(08).
           05  OD-SEGMENT                  PIC S9(11) COMP-3.
           05  OD-OCN                      PIC S9(11)V9(02) COMP-3.
           05  OD-CIRCUIT                  PIC X(03).
           05  DL-FILL-02                  PIC X(16).
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
      * SHARED LAYOUT PULLED IN FOR THE SELECTION SIDE.
       COPY CABSCDR.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUEX22'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V1.30'.
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
           05  WS-DL-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DL-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DL-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DL-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DL-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DL-CNT-06                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-DL-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DL-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DL-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-DL-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DL-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DL-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DL-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-DL-TXT-01                PIC X(20) VALUE SPACES.
           05  WS-DL-TXT-02                PIC X(20) VALUE SPACES.
           05  WS-DL-TXT-03                PIC X(20) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-DL-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-DL-ON-01                 VALUE 'Y'.
               88  WS-DL-OFF-01                VALUE 'N'.
           05  WS-DL-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-DL-ON-02                 VALUE 'Y'.
               88  WS-DL-OFF-02                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-DL-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DL-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DL-SUB-03                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. LOADED ONCE AT INITIALISATION AND SEARCHED WITH
      * A SUBSCRIPTED WALK.
       01  WS-DL-TABLE.
           05  WS-DL-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-DL-TB-ENTRY OCCURS 80 TIMES
                                       INDEXED BY WS-DL-IX.
               10  WS-DL-TB-KEY                PIC X(06).
               10  WS-DL-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-DL-TB-TXT                PIC X(40).
               10  WS-DL-TB-EFF                PIC 9(05).
               10  WS-DL-TB-EXP                PIC 9(05).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 60.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUEX22 - SUSPENSE EXTRACT FOR THE RECYCLE JOB'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-DL-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-DL-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9985.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-DL-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-DL-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT MSTIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'MSTIN OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT DROPOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'DROPOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CTLOUT OPEN FAILED - FILE STATUS BAD' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-DL-CYCLE-YYDDD.
           COMPUTE WS-DL-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-DL-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-DL-CNT-04.
           MOVE 0 TO WS-DL-CNT-02.
           MOVE 0 TO WS-DL-CNT-03.
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
           IF WS-DL-ON-02
               PERFORM P2200-CONVERT-MASTER THRU
                   P2200-CONVERT-MASTER-EXIT.
           PERFORM P2300-SPLIT-SUBSET THRU P2300-SPLIT-SUBSET-EXIT.
           PERFORM P2400-EXPAND-SUBSET THRU P2400-EXPAND-SUBSET-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ MSTIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-CONVERT-MASTER.
           ADD ID-CIRCUIT TO WS-DL-QTY-03.
           COMPUTE WS-DL-AMT-04 = WS-DL-QTY-03 * WS-DL-QTY-02.
           ADD WS-DL-AMT-04 TO WS-DL-AMT-04.
       P2200-CONVERT-MASTER-EXIT.
           EXIT.
       P2300-SPLIT-SUBSET.
           MOVE ID-TYPE TO WS-DL-TXT-02.
           MOVE ID-CIRCUIT TO WS-DL-TXT-03.
           MOVE ID-CIRCUIT TO WS-DL-TXT-02.
           ADD 1 TO WS-DL-CNT-02.
       P2300-SPLIT-SUBSET-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P2400-EXPAND-SUBSET.
           MOVE 0 TO WS-DL-CNT-04.
           INSPECT WS-DL-TXT-02 TALLYING WS-DL-CNT-04
               FOR ALL SPACES.
           INSPECT WS-DL-TXT-02 REPLACING ALL LOW-VALUES BY SPACES.
       P2400-EXPAND-SUBSET-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-FORMAT-SELECTION.
           MOVE SPACES TO CABS-DL-OUT-RECORD.
           MOVE ID-TYPE TO OD-SEQ.
           MOVE ID-ACCOUNT TO OD-TYPE.
           MOVE ID-JURIS TO OD-LEVEL.
           MOVE ID-TARGET TO OD-CYCLE.
           MOVE ID-GROUP TO OD-INVOICE.
           MOVE ID-ACCOUNT TO OD-ACCOUNT.
           MOVE ID-CLASS2 TO OD-CYCLE2.
           WRITE CABS-DL-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P3100-FORMAT-SELECTION-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P3200-FORMAT-EXTRACT.
           CALL 'CABHASH' USING ID-CODE WS-ACC-OCN-HASH.
           ADD WS-DL-CNT-01 TO WS-ACC-SEQ-HASH.
       P3200-FORMAT-EXTRACT-EXIT.
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
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE SPACES TO CT-FILLER.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE WS-DL-CNT-04 TO CT-RC.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE 5 TO CT-STEP-SEQ.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE WS-DL-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
       P8100-EXIT.
           EXIT.
      * P8200-CHECK-BALANCE - EVERY RECORD READ IS EITHER WRITTEN,
      * REJECTED, SUMMARISED OR CARRIED FORWARD.
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
      * S900-TERMINATION SECTION.
       S900-TERMINATION SECTION.
       P9000-TERM.
           CLOSE MSTIN.
           CLOSE DROPOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABUEX22 - STEP COMPLETE'.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  DL-CNT-01 = ' WS-DL-CNT-01.
           DISPLAY '  DL-CNT-05 = ' WS-DL-CNT-05.
           DISPLAY '  DL-CNT-06 = ' WS-DL-CNT-06.
           DISPLAY '  DL-CNT-04 = ' WS-DL-CNT-04.
       P9000-EXIT.
           EXIT.
