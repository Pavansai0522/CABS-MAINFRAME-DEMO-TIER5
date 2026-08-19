      *****************************************************************
      * CABUEX13 - CIRCUIT INVENTORY EXTRACT                          *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               CIRIN   TELCABS.CABS.CIRIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               SELOUT  TELCABS.CABS.SELOUT         (LOCAL)     *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - THE OUTPUT IS REBUILT EACH RUN     *
      * REVISION HISTORY                                              *
      *   V1.00  1989-09-28  A.BUKOWSKI   INITIAL RELEASE             *
      *   V1.02  1998-03-02  W.J.MCALLISTER PARM CARD EXTENDED,       *
      *                      POSITIONS 40 THROUGH 48                  *
      *   V1.06  1999-07-13  W.J.MCALLISTER REPORT PAGINATION         *
      *                      CORRECTED                                *
      *   V1.08  2011-07-18  L.FERREIRA   CARRIER TYPE BROUGHT ONTO   *
      *                      THE EXTRACT                              *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUEX13.
       AUTHOR. TELCABS APPLICATIONS - DATASET EXTRACT.
      *****************************************************************
      * CIRCUIT INVENTORY EXTRACT. THE STEP RUNS ONCE PER BILL CYCLE  *
      * AND IS RERUN FROM THE TOP IF IT FAILS.                        *
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
           SELECT CIRIN ASSIGN TO UT-S-CIRIN
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
      * CIRIN - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  CIRIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 120 CHARACTERS.
       01  CABS-CD-IN-RECORD.
           05  IC-JURIS                    PIC S9(07) COMP-3.
           05  IC-BAN                      PIC X(16).
           05  IC-INVOICE                  PIC X(16).
           05  IC-OCN                      PIC X(16).
           05  IC-BAN2                     PIC S9(07)V9(02) COMP-3.
           05  IC-CLASS                    PIC S9(05) COMP-3.
           05  IC-SOURCE                   PIC S9(13) COMP-3.
           05  IC-LEVEL                    PIC S9(11) COMP-3.
           05  IC-CYCLE                    PIC S9(07) COMP-3.
           05  IC-STATUS                   PIC X(16).
           05  IC-PERIOD                   PIC 9(09).
           05  IC-LEVEL2                   PIC S9(13) COMP-3.
           05  IC-OCN2                     PIC X(04).
           05  IC-SOURCE2                  PIC S9(09) COMP-3.
           05  CD-FILL-01                  PIC X(2).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-CD-VIEW1 REDEFINES CABS-CD-IN-RECORD.
           05  R0C-SEGMENT                 PIC X(20).
           05  R0C-OCN                     PIC 9(02).
           05  R0C-CYCLE                   PIC S9(13) COMP-3.
           05  R0C-ELEM                    PIC S9(15) COMP-3.
           05  R0C-MEDIA                   PIC S9(11) COMP-3.
           05  R0C-CENTRE                  PIC X(03).
           05  R0C-JURIS                   PIC S9(07) COMP-3.
           05  R0C-TARIFF                  PIC 9(04).
           05  R0C-MEDIA2                  PIC 9(09).
           05  R0C-REST                    PIC X(57).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-CD-VIEW2 REDEFINES CABS-CD-IN-RECORD.
           05  R1C-TARGET                  PIC S9(13) COMP-3.
           05  R1C-PERIOD                  PIC 9(09).
           05  R1C-ACCOUNT                 PIC 9(02).
           05  R1C-INVOICE                 PIC S9(13) COMP-3.
           05  R1C-SEQ                     PIC S9(07) COMP-3.
           05  R1C-CYCLE                   PIC X(20).
           05  R1C-INVOICE2                PIC X(03).
           05  R1C-REST                    PIC X(68).
      * SELOUT - WORK FILE, DELETED AT STEP END.
       FD  SELOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-CD-OUT-RECORD.
           05  OC-MEDIA                    PIC X(04).
           05  OC-ACCOUNT                  PIC 9(02).
           05  OC-MEDIA2                   PIC S9(13) COMP-3.
           05  OC-CYCLE                    PIC X(03).
           05  OC-BAN                      PIC X(03).
           05  OC-BAND                     PIC X(04).
           05  OC-INVOICE                  PIC 9(07).
           05  CD-FILL-02                  PIC X(50).
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
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUEX13'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V1.26'.
           05  WS-TABLE-LIMIT              PIC S9(04) COMP-3
                                         VALUE 150.
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
           05  WS-CD-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CD-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CD-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CD-CNT-04                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-CD-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CD-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-CD-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CD-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CD-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CD-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-CD-TXT-01                PIC X(12) VALUE SPACES.
           05  WS-CD-TXT-02                PIC X(08) VALUE SPACES.
           05  WS-CD-TXT-03                PIC X(30) VALUE SPACES.
           05  WS-CD-TXT-04                PIC X(08) VALUE SPACES.
           05  WS-CD-TXT-05                PIC X(12) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-CD-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-CD-ON-01                 VALUE 'Y'.
               88  WS-CD-OFF-01                VALUE 'N'.
           05  WS-CD-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-CD-ON-02                 VALUE 'Y'.
               88  WS-CD-OFF-02                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-CD-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CD-SUB-02                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. THE LIMIT WAS RAISED WHEN THE SOUTHEAST CENTRES
      * WERE MERGED AND HAS NOT BEEN REVIEWED SINCE.
       01  WS-CD-TABLE.
           05  WS-CD-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-CD-TB-ENTRY OCCURS 150 TIMES
                                       INDEXED BY WS-CD-IX.
               10  WS-CD-TB-KEY                PIC X(10).
               10  WS-CD-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-CD-TB-TXT                PIC X(30).
               10  WS-CD-TB-EFF                PIC 9(05).
               10  WS-CD-TB-EXP                PIC 9(05).
       01  WS-CD-WORK-GROUP-1.
           05  WS-CD-G1-CENTRE             PIC X(20).
           05  WS-CD-G1-SEQ                PIC 9(07).
           05  WS-CD-G1-STATE              PIC 9(07).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 58.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUEX13 - CIRCUIT INVENTORY EXTRACT'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-CD-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-CD-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9951.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-CD-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-CD-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT CIRIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CIRIN NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               DISPLAY 'CIRIN FILE STATUS = ' WS-FS-INPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SELOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'SELOUT NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               DISPLAY 'SELOUT FILE STATUS = ' WS-FS-OUTPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CTLOUT NOT AVAILABLE - OPEN REJECTED' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-CD-CYCLE-YYDDD.
           COMPUTE WS-CD-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-CD-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-CD-CNT-02.
           MOVE 0 TO WS-CD-CNT-01.
           MOVE 0 TO WS-CD-CNT-04.
       P1200-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-CD-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-CD-TAB-CNT NOT < 150
               MOVE 'Y' TO WS-CD-SW-01
               ADD 1 TO WS-CD-CNT-02
           ELSE
               ADD 1 TO WS-CD-TAB-CNT
               SET WS-CD-IX TO WS-CD-TAB-CNT
               MOVE IC-OCN TO WS-CD-TB-KEY (WS-CD-IX)
               MOVE 0 TO WS-CD-TB-VAL (WS-CD-IX)
               MOVE SPACES TO WS-CD-TB-TXT (WS-CD-IX)
               ADD 1 TO WS-CD-CNT-04.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ CIRIN
               AT END MOVE 'Y' TO WS-CD-SW-01.
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
           IF WS-CD-ON-02
               PERFORM P2200-BUILD-MASTER THRU P2200-BUILD-MASTER-EXIT.
           PERFORM P2300-SPLIT-MASTER THRU P2300-SPLIT-MASTER-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ CIRIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P2200-BUILD-MASTER.
           MOVE 0 TO WS-CD-CNT-01.
           INSPECT WS-CD-TXT-03 TALLYING WS-CD-CNT-01
               FOR ALL SPACES.
           INSPECT WS-CD-TXT-03 REPLACING ALL LOW-VALUES BY SPACES.
           MOVE 'N' TO WS-CD-SW-02.
           IF WS-CD-TAB-CNT > 0
               PERFORM P280-COMPARE-RANGE THRU P280-COMPARE-RANGE-EXIT
               VARYING WS-CD-SUB-02 FROM 1 BY 1
               UNTIL WS-CD-SUB-02 > WS-CD-TAB-CNT
               OR WS-CD-SW-02 = 'Y'.
       P2200-BUILD-MASTER-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P2300-SPLIT-MASTER.
           IF IC-BAN = 'S'
               ADD 1 TO WS-CD-CNT-01
           ELSE
               IF IC-BAN = 'C'
                   ADD 1 TO WS-CD-CNT-04
               ELSE
                   IF IC-BAN = 'B'
                       ADD 1 TO WS-CD-CNT-01
                   ELSE
                       ADD 1 TO WS-CD-CNT-03.
       P2300-SPLIT-MASTER-EXIT.
           EXIT.
       P280-COMPARE-RANGE.
           SET WS-CD-IX TO WS-CD-SUB-01.
           IF WS-CD-TB-KEY (WS-CD-IX) = IC-STATUS
               MOVE 'Y' TO WS-CD-SW-01
               MOVE WS-CD-TB-VAL (WS-CD-IX) TO WS-CD-QTY-01
               MOVE WS-CD-SUB-01 TO WS-CD-SUB-02.
       P280-COMPARE-RANGE-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P3100-EMIT-RANGE.
           CALL 'CABHASH' USING IC-LEVEL2 WS-ACC-OCN-HASH.
           ADD WS-CD-CNT-04 TO WS-ACC-SEQ-HASH.
       P3100-EMIT-RANGE-EXIT.
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
           MOVE WS-CD-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE 3 TO CT-STEP-SEQ.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-FILLER.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE WS-CD-TXT-04 TO CT-RESTART-KEY.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
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
           CLOSE CIRIN.
           CLOSE SELOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABUEX13 - END OF RUN'.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  CD-CNT-04 = ' WS-CD-CNT-04.
           DISPLAY '  CD-CNT-01 = ' WS-CD-CNT-01.
       P9000-EXIT.
           EXIT.
