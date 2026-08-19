      *****************************************************************
      * CABURT18 - RATE TABLE EFFECTIVE DATE ROLL                     *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               CTLIN   TELCABS.CABS.CTLIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               AUDOUT  TELCABS.CABS.AUDOUT         (LOCAL)     *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - NOTHING IS UPDATED IN PLACE        *
      * REVISION HISTORY                                              *
      *   V1.00  1994-09-26  R.T.WHEELER  INITIAL RELEASE             *
      *   V1.04  1997-10-18  J.M.CASTILLO CARRIER TYPE BROUGHT ONTO   *
      *                      THE EXTRACT                              *
      *   V1.08  2011-05-28  A.BUKOWSKI   RETIRED THE SECOND SORT STEP*
      *                      - DONE IN PROGRAM                        *
      *   V1.09  2019-08-02  D.OKONKWO    RETIRED THE SECOND SORT STEP*
      *                      - DONE IN PROGRAM                        *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABURT18.
       AUTHOR. TELCABS APPLICATIONS - RATE TABLE MAINTENANCE.
      *****************************************************************
      * RATE TABLE EFFECTIVE DATE ROLL. THIS STEP IS SCHEDULED INSIDE *
      * THE NIGHTLY ACCESS BILLING STREAM AND HAS NO INTERACTIVE ENTRY*
      * POINT.                                                        *
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
           SELECT CTLIN ASSIGN TO UT-S-CTLIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT AUDOUT ASSIGN TO UT-S-AUDOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT CTLOUT ASSIGN TO UT-S-CTLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      * CTLIN - WORK FILE, DELETED AT STEP END.
       FD  CTLIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-BV-IN-RECORD.
           05  IB-ACCOUNT                  PIC S9(09)V9(02) COMP-3.
           05  IB-ACCOUNT2                 PIC S9(07) COMP-3.
           05  IB-ELEM                     PIC S9(11) COMP-3.
           05  IB-CODE                     PIC S9(11) COMP-3.
           05  IB-SEQ                      PIC S9(11) COMP-3.
           05  IB-STATUS                   PIC S9(11) COMP-3.
           05  IB-PERIOD                   PIC X(13).
           05  IB-BAND                     PIC S9(15) COMP-3.
           05  IB-SOURCE                   PIC X(02).
           05  IB-SEQ2                     PIC X(06).
           05  IB-ELEM2                    PIC 9(05).
           05  BV-FILL-01                  PIC X(12).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-BV-VIEW1 REDEFINES CABS-BV-IN-RECORD.
           05  R0B-INVOICE                 PIC S9(11)V9(02) COMP-3.
           05  R0B-TARIFF                  PIC S9(15) COMP-3.
           05  R0B-INVOICE2                PIC 9(04).
           05  R0B-REGION                  PIC 9(02).
           05  R0B-CODE                    PIC S9(15) COMP-3.
           05  R0B-INVOICE3                PIC X(16).
           05  R0B-REST                    PIC X(35).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-BV-VIEW2 REDEFINES CABS-BV-IN-RECORD.
           05  R1B-LEVEL                   PIC X(10).
           05  R1B-PERIOD                  PIC S9(11)V9(05) COMP-3.
           05  R1B-GROUP                   PIC 9(04).
           05  R1B-CLASS                   PIC X(20).
           05  R1B-BAN                     PIC 9(04).
           05  R1B-TYPE                    PIC S9(09) COMP-3.
           05  R1B-SOURCE                  PIC S9(09) COMP-3.
           05  R1B-BAN2                    PIC 9(09).
           05  R1B-REST                    PIC X(14).
      * AUDOUT - CATALOGUED GENERATION DATA GROUP.
       FD  AUDOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-BV-OUT-RECORD.
           05  OB-LEVEL                    PIC 9(02).
           05  OB-STATE                    PIC 9(05).
           05  OB-ACCOUNT                  PIC S9(15) COMP-3.
           05  OB-OCN                      PIC X(02).
           05  OB-TYPE                     PIC S9(07) COMP-3.
           05  OB-REGION                   PIC 9(07).
           05  OB-SEGMENT                  PIC S9(11)V9(02) COMP-3.
           05  OB-LEVEL2                   PIC X(04).
           05  BV-FILL-02                  PIC X(41).
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
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABURT18'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V1.20'.
           05  WS-TABLE-LIMIT              PIC S9(04) COMP-3
                                         VALUE 50.
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
           05  WS-BV-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BV-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BV-CNT-03                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-BV-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BV-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-BV-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BV-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BV-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BV-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-BV-TXT-01                PIC X(08) VALUE SPACES.
           05  WS-BV-TXT-02                PIC X(26) VALUE SPACES.
           05  WS-BV-TXT-03                PIC X(16) VALUE SPACES.
           05  WS-BV-TXT-04                PIC X(30) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-BV-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-BV-ON-01                 VALUE 'Y'.
               88  WS-BV-OFF-01                VALUE 'N'.
           05  WS-BV-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-BV-ON-02                 VALUE 'Y'.
               88  WS-BV-OFF-02                VALUE 'N'.
           05  WS-BV-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-BV-ON-03                 VALUE 'Y'.
               88  WS-BV-OFF-03                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-BV-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-BV-SUB-02                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. THE LIMIT WAS RAISED WHEN THE SOUTHEAST CENTRES
      * WERE MERGED AND HAS NOT BEEN REVIEWED SINCE.
       01  WS-BV-TABLE.
           05  WS-BV-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-BV-TB-ENTRY OCCURS 50 TIMES
                                       INDEXED BY WS-BV-IX.
               10  WS-BV-TB-KEY                PIC X(13).
               10  WS-BV-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-BV-TB-TXT                PIC X(30).
               10  WS-BV-TB-EFF                PIC 9(05).
               10  WS-BV-TB-EXP                PIC 9(05).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 55.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABURT18 - RATE TABLE EFFECTIVE DATE ROLL'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-BV-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-BV-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9939.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-BV-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-BV-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT CTLIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF CTLIN' TO
                   WS-AB-REASON
               DISPLAY 'CTLIN FILE STATUS = ' WS-FS-INPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT AUDOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF AUDOUT' TO
                   WS-AB-REASON
               DISPLAY 'AUDOUT FILE STATUS = ' WS-FS-OUTPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF CTLOUT' TO
                   WS-AB-REASON
               DISPLAY 'CTLOUT FILE STATUS = ' WS-FS-CONTROL
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
       P1100-EXIT.
           EXIT.
      * P1200-READ-PARM - THE CYCLE DATE ARRIVES AS TWO DIGITS AND IS
      * PIVOTED ON DW-PIVOT-YY BEFORE ANY DATE MATH.
       P1200-READ-PARM.
           MOVE SPACES TO WS-PARM-CARD.
           ACCEPT WS-PARM-CARD FROM SYSIN.
           CALL 'CABPARMR' USING WS-PARM-CARD WS-RC-PARMR.
           MOVE PC1-CYCLE-YYDDD TO WS-BV-CYCLE-YYDDD.
           COMPUTE WS-BV-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-BV-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-BV-CNT-02.
           MOVE 0 TO WS-BV-CNT-03.
           MOVE 0 TO WS-BV-CNT-01.
       P1200-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-BV-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-BV-TAB-CNT NOT < 50
               MOVE 'Y' TO WS-BV-SW-01
               ADD 1 TO WS-BV-CNT-01
           ELSE
               ADD 1 TO WS-BV-TAB-CNT
               SET WS-BV-IX TO WS-BV-TAB-CNT
               MOVE IB-STATUS TO WS-BV-TB-KEY (WS-BV-IX)
               MOVE 0 TO WS-BV-TB-VAL (WS-BV-IX)
               MOVE SPACES TO WS-BV-TB-TXT (WS-BV-IX)
               ADD 1 TO WS-BV-CNT-01.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ CTLIN
               AT END MOVE 'Y' TO WS-BV-SW-01.
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
           IF WS-BV-ON-01
               PERFORM P2200-SPLIT-WINDOW THRU P2200-SPLIT-WINDOW-EXIT.
           PERFORM P2300-BUILD-WINDOW THRU P2300-BUILD-WINDOW-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ CTLIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P2200-SPLIT-WINDOW.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-BV-TXT-02 TO PC-COL-001-020.
           MOVE WS-BV-TXT-04 TO PC-COL-021-060.
           MOVE WS-BV-AMT-04 TO WS-BV-AMT-EDIT.
           MOVE WS-BV-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
           MOVE 'N' TO WS-BV-SW-02.
           IF WS-BV-TAB-CNT > 0
               PERFORM P270-COMPARE-WINDOW THRU P270-COMPARE-WINDOW-EXIT
               VARYING WS-BV-SUB-01 FROM 1 BY 1
               UNTIL WS-BV-SUB-01 > WS-BV-TAB-CNT
               OR WS-BV-SW-02 = 'Y'.
       P2200-SPLIT-WINDOW-EXIT.
           EXIT.
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT
      * PRECEDES THIS PROGRAM IN THE JOB.
       P2300-BUILD-WINDOW.
           IF WS-BV-AMT-04 < 40
               MOVE 40 TO WS-BV-AMT-04
               ADD 1 TO WS-BV-CNT-02.
           IF WS-BV-AMT-04 > 95403
               MOVE 95403 TO WS-BV-AMT-04
               ADD 1 TO WS-BV-CNT-01.
       P2300-BUILD-WINDOW-EXIT.
           EXIT.
       P270-COMPARE-WINDOW.
           SET WS-BV-IX TO WS-BV-SUB-02.
           IF WS-BV-TB-KEY (WS-BV-IX) = IB-SEQ
               MOVE 'Y' TO WS-BV-SW-02
               MOVE WS-BV-TB-VAL (WS-BV-IX) TO WS-BV-QTY-01
               MOVE WS-BV-SUB-02 TO WS-BV-SUB-01.
       P270-COMPARE-WINDOW-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-CLOSE-OFF-TARIFF.
           CALL 'CABHASH' USING IB-ACCOUNT2 WS-ACC-OCN-HASH.
           ADD WS-BV-CNT-02 TO WS-ACC-SEQ-HASH.
       P3100-CLOSE-OFF-TARIFF-EXIT.
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
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE WS-BV-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE 5 TO CT-STEP-SEQ.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
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
           CLOSE CTLIN.
           CLOSE AUDOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABURT18 - RUN COMPLETE'.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  BV-CNT-03 = ' WS-BV-CNT-03.
           DISPLAY '  BV-CNT-01 = ' WS-BV-CNT-01.
           DISPLAY '  BV-CNT-02 = ' WS-BV-CNT-02.
       P9000-EXIT.
           EXIT.
