      *****************************************************************
      * CABUEX04 - BILLED ACCOUNT EXTRACT                             *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               MSTIN   TELCABS.CABS.MSTIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               SELOUT  TELCABS.CABS.SELOUT         (LOCAL)     *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * REVISION HISTORY                                              *
      *   V1.00  1987-11-28  D.OKONKWO    INITIAL RELEASE             *
      *   V1.04  2009-06-09  L.FERREIRA   CARRIER TYPE BROUGHT ONTO   *
      *                      THE EXTRACT                              *
      *   V1.05  2010-09-19  B.R.HALVORSEN RETIRED THE SECOND SORT    *
      *                      STEP - DONE IN PROGRAM                   *
      *   V1.08  2014-01-09  C.ADEYEMI    RECOMPILE ONLY - COPYBOOK   *
      *                      CHANGE UPSTREAM                          *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUEX04.
       AUTHOR. TELCABS APPLICATIONS - DATASET EXTRACT.
      *****************************************************************
      * BILLED ACCOUNT EXTRACT. THE STEP RUNS ONCE PER BILL CYCLE AND *
      * IS RERUN FROM THE TOP IF IT FAILS.                            *
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT     *
      * PRECEDES THIS PROGRAM IN THE JOB.                             *
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
           SELECT SELOUT ASSIGN TO UT-S-SELOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT CTLOUT ASSIGN TO UT-S-CTLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      * MSTIN - PERMANENT DATASET HELD ON DASD.
       FD  MSTIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-DS-IN-RECORD.
           05  ID-CIRCUIT                  PIC S9(07) COMP-3.
           05  ID-STATUS                   PIC S9(11) COMP-3.
           05  ID-REGION                   PIC X(06).
           05  ID-JURIS                    PIC 9(03).
           05  ID-SOURCE                   PIC X(13).
           05  ID-SOURCE2                  PIC S9(07) COMP-3.
           05  ID-TARGET                   PIC S9(09)V9(05) COMP-3.
           05  ID-GROUP                    PIC S9(11)V9(05) COMP-3.
           05  ID-MEDIA                    PIC X(04).
           05  ID-CENTRE                   PIC S9(11)V9(05) COMP-3.
           05  ID-BAN                      PIC S9(09)V9(02) COMP-3.
           05  DS-FILL-01                  PIC X(8).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-DS-VIEW1 REDEFINES CABS-DS-IN-RECORD.
           05  R0D-LEVEL                   PIC X(02).
           05  R0D-REGION                  PIC S9(09) COMP-3.
           05  R0D-STATE                   PIC S9(13)V9(02) COMP-3.
           05  R0D-SOURCE                  PIC 9(02).
           05  R0D-PERIOD                  PIC S9(13)V9(05) COMP-3.
           05  R0D-CYCLE                   PIC S9(13)V9(02) COMP-3.
           05  R0D-BAN                     PIC S9(05) COMP-3.
           05  R0D-SEGMENT                 PIC 9(09).
           05  R0D-REST                    PIC X(33).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-DS-VIEW2 REDEFINES CABS-DS-IN-RECORD.
           05  R1D-CYCLE                   PIC X(16).
           05  R1D-TYPE                    PIC X(08).
           05  R1D-ELEM                    PIC 9(02).
           05  R1D-CARRIER                 PIC X(06).
           05  R1D-CIRCUIT                 PIC X(02).
           05  R1D-REST                    PIC X(46).
      * SELOUT - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  SELOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 120 CHARACTERS.
       01  CABS-DS-OUT-RECORD.
           05  OD-CODE                     PIC X(20).
           05  OD-LEVEL                    PIC X(20).
           05  OD-OCN                      PIC S9(15) COMP-3.
           05  OD-STATE                    PIC X(10).
           05  OD-CODE2                    PIC 9(06).
           05  OD-CIRCUIT                  PIC S9(05) COMP-3.
           05  OD-TARGET                   PIC X(13).
           05  OD-CARRIER                  PIC X(13).
           05  OD-BAND                     PIC 9(02).
           05  OD-STATUS                   PIC S9(09) COMP-3.
           05  OD-INVOICE                  PIC S9(13)V9(02) COMP-3.
           05  OD-CYCLE                    PIC S9(05) COMP-3.
           05  DS-FILL-02                  PIC X(9).
      * CTLOUT - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD              PIC X(180).
       WORKING-STORAGE SECTION.
      * STANDARD SHARED WORKING STORAGE. SEE CABSWRK.
       COPY CABSWRK.
      * SHARED LAYOUT PULLED IN FOR THE MASTER SIDE.
       COPY CABSCIRC.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUEX04'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V2.04'.
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
           05  WS-DS-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DS-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DS-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DS-CNT-04                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-DS-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DS-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DS-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DS-QTY-04                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-DS-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DS-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-DS-TXT-01                PIC X(12) VALUE SPACES.
           05  WS-DS-TXT-02                PIC X(26) VALUE SPACES.
           05  WS-DS-TXT-03                PIC X(10) VALUE SPACES.
           05  WS-DS-TXT-04                PIC X(20) VALUE SPACES.
           05  WS-DS-TXT-05                PIC X(10) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-DS-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-DS-ON-01                 VALUE 'Y'.
               88  WS-DS-OFF-01                VALUE 'N'.
           05  WS-DS-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-DS-ON-02                 VALUE 'Y'.
               88  WS-DS-OFF-02                VALUE 'N'.
           05  WS-DS-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-DS-ON-03                 VALUE 'Y'.
               88  WS-DS-OFF-03                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-DS-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DS-SUB-02                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. A ROW THAT DOES NOT FIT IS COUNTED AND DROPPED
      * - THE STEP DOES NOT FAIL.
       01  WS-DS-TABLE.
           05  WS-DS-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-DS-TB-ENTRY OCCURS 100 TIMES
                                       INDEXED BY WS-DS-IX.
               10  WS-DS-TB-KEY                PIC X(08).
               10  WS-DS-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-DS-TB-TXT                PIC X(40).
               10  WS-DS-TB-EFF                PIC 9(05).
               10  WS-DS-TB-EXP                PIC 9(05).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 55.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUEX04 - BILLED ACCOUNT EXTRACT'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-DS-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-DS-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9912.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-DS-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-DS-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT MSTIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON MSTIN - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SELOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON SELOUT - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON CTLOUT - CHECK THE ALLOCATION' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-DS-CYCLE-YYDDD.
           COMPUTE WS-DS-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-DS-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-DS-CNT-03.
           MOVE 0 TO WS-DS-CNT-01.
           MOVE 0 TO WS-DS-CNT-04.
       P1200-EXIT.
           EXIT.
      * A FAILURE HERE SUSPENDS THE RECORD, IT DOES NOT STOP THE STEP.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-DS-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-DS-TAB-CNT NOT < 100
               MOVE 'Y' TO WS-DS-SW-01
               ADD 1 TO WS-DS-CNT-01
           ELSE
               ADD 1 TO WS-DS-TAB-CNT
               SET WS-DS-IX TO WS-DS-TAB-CNT
               MOVE ID-JURIS TO WS-DS-TB-KEY (WS-DS-IX)
               MOVE 0 TO WS-DS-TB-VAL (WS-DS-IX)
               MOVE SPACES TO WS-DS-TB-TXT (WS-DS-IX)
               ADD 1 TO WS-DS-CNT-03.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ MSTIN
               AT END MOVE 'Y' TO WS-DS-SW-01.
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
           PERFORM P2200-MATCH-SELECTION THRU
               P2200-MATCH-SELECTION-EXIT.
           IF WS-DS-ON-03
               PERFORM P2300-SELECT-SUBSET THRU
                   P2300-SELECT-SUBSET-EXIT.
           IF WS-DS-ON-01
               PERFORM P2400-VALIDATE-MASTER THRU
                   P2400-VALIDATE-MASTER-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ MSTIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-MATCH-SELECTION.
           IF WS-DS-AMT-02 < 18
               MOVE 18 TO WS-DS-AMT-02
               ADD 1 TO WS-DS-CNT-01.
           IF WS-DS-AMT-02 > 13184
               MOVE 13184 TO WS-DS-AMT-02
               ADD 1 TO WS-DS-CNT-01.
           MOVE 'N' TO WS-DS-SW-01.
           IF WS-DS-TAB-CNT > 0
               PERFORM P280-COMPARE-FILTER THRU P280-COMPARE-FILTER-EXIT
               VARYING WS-DS-SUB-02 FROM 1 BY 1
               UNTIL WS-DS-SUB-02 > WS-DS-TAB-CNT
               OR WS-DS-SW-01 = 'Y'.
       P2200-MATCH-SELECTION-EXIT.
           EXIT.
       P2300-SELECT-SUBSET.
           CALL 'CABHASH' USING ID-STATUS WS-ACC-OCN-HASH.
           ADD WS-DS-CNT-02 TO WS-ACC-SEQ-HASH.
       P2300-SELECT-SUBSET-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P2400-VALIDATE-MASTER.
           CALL 'CABCTLWR' USING WS-DS-TXT-02 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-DS-CNT-01.
       P2400-VALIDATE-MASTER-EXIT.
           EXIT.
       P280-COMPARE-FILTER.
           SET WS-DS-IX TO WS-DS-SUB-02.
           IF WS-DS-TB-KEY (WS-DS-IX) = ID-CENTRE
               MOVE 'Y' TO WS-DS-SW-02
               MOVE WS-DS-TB-VAL (WS-DS-IX) TO WS-DS-QTY-04
               MOVE WS-DS-SUB-02 TO WS-DS-SUB-02.
       P280-COMPARE-FILTER-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-STAGE-SELECTION.
           MOVE ID-GROUP TO WS-DS-TXT-01.
           MOVE ID-REGION TO WS-DS-TXT-05.
           ADD 1 TO WS-DS-CNT-01.
       P3100-STAGE-SELECTION-EXIT.
           EXIT.
       P3200-WRITE-FILTER.
           MOVE SPACES TO CABS-DS-OUT-RECORD.
           MOVE ID-JURIS TO OD-CODE.
           MOVE ID-SOURCE TO OD-LEVEL.
           MOVE ID-BAN TO OD-OCN.
           MOVE ID-GROUP TO OD-STATE.
           MOVE ID-MEDIA TO OD-CODE2.
           MOVE ID-CENTRE TO OD-CIRCUIT.
           MOVE ID-REGION TO OD-TARGET.
           MOVE ID-TARGET TO OD-CARRIER.
           MOVE ID-SOURCE2 TO OD-BAND.
           WRITE CABS-DS-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P3200-WRITE-FILTER-EXIT.
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
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE 3 TO CT-STEP-SEQ.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE WS-DS-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE SPACES TO CT-FILLER.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE 0 TO CT-RC.
           MOVE WS-DS-TXT-04 TO CT-RESTART-KEY.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
       P8100-EXIT.
           EXIT.
      * P8200-CHECK-BALANCE - THE REPORT LINES ARE NOT RECORDS, SO THE
      * WRITTEN COUNT IS ZEROED BEFORE THE EQUATION IS TESTED.
       P8200-CHECK-BALANCE.
           MOVE 0 TO CT-WRITTEN.
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
           CLOSE SELOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABUEX04 - STEP COMPLETE'.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  DS-CNT-01 = ' WS-DS-CNT-01.
           DISPLAY '  DS-CNT-02 = ' WS-DS-CNT-02.
           DISPLAY '  DS-CNT-03 = ' WS-DS-CNT-03.
       P9000-EXIT.
           EXIT.
