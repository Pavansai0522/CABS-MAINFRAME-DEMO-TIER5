      *****************************************************************
      * CABUXR02 - CARRIER TO BILLING ACCOUNT CROSS REFERENCE         *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               SUSIN   TELCABS.CABS.SUSIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               PAIROUT TELCABS.CABS.PAIROU         (LOCAL)     *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - NOTHING IS UPDATED IN PLACE        *
      * REVISION HISTORY                                              *
      *   V1.00  1990-08-03  G.PRZYBYLSKI INITIAL RELEASE             *
      *   V1.01  2008-12-10  R.T.WHEELER  CONTROL RECORD ADDED PER    *
      *                      CABS-STD-002                             *
      *   V1.02  2009-12-07  L.FERREIRA   EFFECTIVE DATE FILTER ADDED *
      *                      PER AUDIT FINDING                        *
      *   V1.06  2017-10-21  K.O.BRIEN    JOB PARAMETER MADE MANDATORY*
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUXR02.
       AUTHOR. TELCABS APPLICATIONS - CROSS REFERENCE REPORT.
      *****************************************************************
      * CARRIER TO BILLING ACCOUNT CROSS REFERENCE. THE STEP RUNS ONCE*
      * PER BILL CYCLE AND IS RERUN FROM THE TOP IF IT FAILS.         *
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
           SELECT SUSIN ASSIGN TO UT-S-SUSIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT PAIROUT ASSIGN TO UT-S-PAIROUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT CTLOUT ASSIGN TO UT-S-CTLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      * SUSIN - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  SUSIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-BJ-IN-RECORD.
           05  IB-ELEM                     PIC X(20).
           05  IB-SEGMENT                  PIC X(16).
           05  IB-STATUS                   PIC X(06).
           05  IB-CENTRE                   PIC X(03).
           05  IB-TARGET                   PIC 9(07).
           05  IB-TARGET2                  PIC X(02).
           05  IB-CODE                     PIC S9(07)V9(02) COMP-3.
           05  IB-JURIS                    PIC X(10).
           05  IB-CARRIER                  PIC 9(09).
           05  BJ-FILL-01                  PIC X(2).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-BJ-VIEW1 REDEFINES CABS-BJ-IN-RECORD.
           05  R0B-MEDIA                   PIC 9(04).
           05  R0B-GROUP                   PIC X(06).
           05  R0B-CODE                    PIC X(06).
           05  R0B-GROUP2                  PIC S9(11) COMP-3.
           05  R0B-JURIS                   PIC X(08).
           05  R0B-LEVEL                   PIC S9(15) COMP-3.
           05  R0B-CYCLE                   PIC 9(02).
           05  R0B-CYCLE2                  PIC S9(07) COMP-3.
           05  R0B-REST                    PIC X(36).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-BJ-VIEW2 REDEFINES CABS-BJ-IN-RECORD.
           05  R1B-JURIS                   PIC X(13).
           05  R1B-CENTRE                  PIC X(13).
           05  R1B-CYCLE                   PIC 9(02).
           05  R1B-MEDIA                   PIC S9(09)V9(02) COMP-3.
           05  R1B-REGION                  PIC X(03).
           05  R1B-CIRCUIT                 PIC 9(02).
           05  R1B-REST                    PIC X(41).
      * PAIROUT - WORK FILE, DELETED AT STEP END.
       FD  PAIROUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 90 CHARACTERS.
       01  CABS-BJ-OUT-RECORD.
           05  OB-TARIFF                   PIC X(04).
           05  OB-CIRCUIT                  PIC S9(11) COMP-3.
           05  OB-CLASS                    PIC 9(03).
           05  OB-GROUP                    PIC 9(07).
           05  OB-TARIFF2                  PIC S9(11)V9(02) COMP-3.
           05  OB-CARRIER                  PIC S9(05) COMP-3.
           05  OB-OCN                      PIC S9(09)V9(05) COMP-3.
           05  OB-TARGET                   PIC 9(05).
           05  OB-GROUP2                   PIC X(13).
           05  OB-TYPE                     PIC X(02).
           05  OB-LEVEL                    PIC X(03).
           05  OB-REGION                   PIC X(20).
           05  BJ-FILL-02                  PIC X(9).
      * CTLOUT - PERMANENT DATASET HELD ON DASD.
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD              PIC X(180).
       WORKING-STORAGE SECTION.
      * STANDARD SHARED WORKING STORAGE. SEE CABSWRK.
       COPY CABSWRK.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUXR02'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V3.24'.
           05  WS-TABLE-LIMIT              PIC S9(04) COMP-3
                                         VALUE 120.
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
           05  WS-BJ-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BJ-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BJ-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BJ-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BJ-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BJ-CNT-06                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-BJ-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BJ-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-BJ-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BJ-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-BJ-TXT-01                PIC X(20) VALUE SPACES.
           05  WS-BJ-TXT-02                PIC X(12) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-BJ-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-BJ-ON-01                 VALUE 'Y'.
               88  WS-BJ-OFF-01                VALUE 'N'.
           05  WS-BJ-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-BJ-ON-02                 VALUE 'Y'.
               88  WS-BJ-OFF-02                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-BJ-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-BJ-SUB-02                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. THE LIMIT WAS RAISED WHEN THE SOUTHEAST CENTRES
      * WERE MERGED AND HAS NOT BEEN REVIEWED SINCE.
       01  WS-BJ-TABLE.
           05  WS-BJ-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-BJ-TB-ENTRY OCCURS 120 TIMES
                                       INDEXED BY WS-BJ-IX.
               10  WS-BJ-TB-KEY                PIC X(13).
               10  WS-BJ-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-BJ-TB-TXT                PIC X(20).
               10  WS-BJ-TB-EFF                PIC 9(05).
               10  WS-BJ-TB-EXP                PIC 9(05).
       01  WS-BJ-WORK-GROUP-1.
           05  WS-BJ-G1-TYPE               PIC S9(09) COMP-3.
           05  WS-BJ-G1-CYCLE              PIC 9(05).
           05  WS-BJ-G1-BAND               PIC 9(05).
           05  WS-BJ-G1-ELEM               PIC 9(05).
           05  WS-BJ-G1-JURIS              PIC S9(11)V9(02) COMP-3.
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 55.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUXR02 - CARRIER TO BILLING ACCOUNT CROSS REFER'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-BJ-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-BJ-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9980.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-BJ-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-BJ-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT SUSIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'SUSIN NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT PAIROUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'PAIROUT NOT AVAILABLE - OPEN REJECTED' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-BJ-CYCLE-YYDDD.
           COMPUTE WS-BJ-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-BJ-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-BJ-CNT-06.
           MOVE 0 TO WS-BJ-CNT-03.
           MOVE 0 TO WS-BJ-CNT-05.
       P1200-EXIT.
           EXIT.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-BJ-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-BJ-TAB-CNT NOT < 120
               MOVE 'Y' TO WS-BJ-SW-01
               ADD 1 TO WS-BJ-CNT-03
           ELSE
               ADD 1 TO WS-BJ-TAB-CNT
               SET WS-BJ-IX TO WS-BJ-TAB-CNT
               MOVE IB-STATUS TO WS-BJ-TB-KEY (WS-BJ-IX)
               MOVE 0 TO WS-BJ-TB-VAL (WS-BJ-IX)
               MOVE SPACES TO WS-BJ-TB-TXT (WS-BJ-IX)
               ADD 1 TO WS-BJ-CNT-04.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ SUSIN
               AT END MOVE 'Y' TO WS-BJ-SW-01.
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
           PERFORM P2200-CHECK-REFERENCE THRU
               P2200-CHECK-REFERENCE-EXIT.
           IF WS-BJ-ON-01
               PERFORM P2300-SELECT-GROUP THRU P2300-SELECT-GROUP-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ SUSIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * A FAILURE HERE SUSPENDS THE RECORD, IT DOES NOT STOP THE STEP.
       P2200-CHECK-REFERENCE.
           MOVE 0 TO WS-BJ-QTY-01.
           MOVE 0 TO WS-BJ-QTY-02.
           MOVE 0 TO WS-BJ-AMT-01.
           MOVE 0 TO WS-BJ-AMT-02.
           MOVE 'N' TO WS-BJ-SW-01.
           IF WS-BJ-TAB-CNT > 0
               PERFORM P260-COMPARE-SIDE THRU P260-COMPARE-SIDE-EXIT
               VARYING WS-BJ-SUB-01 FROM 1 BY 1
               UNTIL WS-BJ-SUB-01 > WS-BJ-TAB-CNT
               OR WS-BJ-SW-01 = 'Y'.
       P2200-CHECK-REFERENCE-EXIT.
           EXIT.
       P2300-SELECT-GROUP.
           ADD IB-CODE TO WS-BJ-QTY-02.
           COMPUTE WS-BJ-AMT-01 ROUNDED = WS-BJ-QTY-02 * WS-BJ-QTY-02.
           ADD WS-BJ-AMT-01 TO WS-BJ-AMT-02.
       P2300-SELECT-GROUP-EXIT.
           EXIT.
       P260-COMPARE-SIDE.
           SET WS-BJ-IX TO WS-BJ-SUB-01.
           IF WS-BJ-TB-KEY (WS-BJ-IX) = IB-CENTRE
               MOVE 'Y' TO WS-BJ-SW-01
               MOVE WS-BJ-TB-VAL (WS-BJ-IX) TO WS-BJ-QTY-02
               MOVE WS-BJ-SUB-01 TO WS-BJ-SUB-02.
       P260-COMPARE-SIDE-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P3100-FORMAT-PAIR.
           MOVE IB-TARGET TO WS-BJ-TXT-01.
           MOVE IB-STATUS TO WS-BJ-TXT-02.
           MOVE IB-TARGET TO WS-BJ-TXT-02.
           MOVE IB-SEGMENT TO WS-BJ-TXT-02.
           ADD 1 TO WS-BJ-CNT-06.
       P3100-FORMAT-PAIR-EXIT.
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
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE WS-BJ-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE 7 TO CT-STEP-SEQ.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE SPACES TO CT-FILLER.
           MOVE 0 TO CT-RC.
       P8100-EXIT.
           EXIT.
      * P8200-CHECK-BALANCE - A RECORD THAT WAS HELD FOR THE NEXT
      * CYCLE IS BROUGHT INTO THE CARRIED FORWARD SIDE BEFORE THE
      * EQUATION IS TESTED.
       P8200-CHECK-BALANCE.
           ADD WS-BJ-CNT-03 TO CT-CARRIED-FWD.
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
           CLOSE SUSIN.
           CLOSE PAIROUT.
           CLOSE CTLOUT.
           DISPLAY 'CABUXR02 - NORMAL END OF JOB'.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  BJ-CNT-06 = ' WS-BJ-CNT-06.
           DISPLAY '  BJ-CNT-04 = ' WS-BJ-CNT-04.
       P9000-EXIT.
           EXIT.
