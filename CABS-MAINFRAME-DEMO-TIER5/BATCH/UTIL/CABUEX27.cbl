      *****************************************************************
      * CABUEX27 - CARRIER EXTRACT FOR THE SETTLEMENT FEED            *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               USGIN   TELCABS.CABS.USGIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               GLOUT   TELCABS.CABS.GLOUT          (LOCAL)     *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * REVISION HISTORY                                              *
      *   V1.00  1991-11-05  K.O.BRIEN    INITIAL RELEASE             *
      *   V1.03  1992-01-23  D.OKONKWO    OCCURS RAISED AFTER THE     *
      *                      FEBRUARY OVERFLOW                        *
      *   V1.04  2000-11-03  W.J.MCALLISTER ROUNDING RULE TAKEN FROM  *
      *                      THE RATE ROW                             *
      *   V1.07  2007-01-11  A.BUKOWSKI   CENTURY PIVOT APPLIED TO THE*
      *                      CYCLE DATE                               *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUEX27.
       AUTHOR. TELCABS APPLICATIONS - DATASET EXTRACT.
      *****************************************************************
      * CARRIER EXTRACT FOR THE SETTLEMENT FEED. THIS STEP IS         *
      * SCHEDULED INSIDE THE NIGHTLY ACCESS BILLING STREAM AND HAS NO *
      * INTERACTIVE ENTRY POINT.                                      *
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
           SELECT USGIN ASSIGN TO UT-S-USGIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT GLOUT ASSIGN TO UT-S-GLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT CTLOUT ASSIGN TO UT-S-CTLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      * USGIN - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  USGIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 130 CHARACTERS.
       01  CABS-CM-IN-RECORD.
           05  IC-CYCLE                    PIC X(20).
           05  IC-CENTRE                   PIC X(16).
           05  IC-CIRCUIT                  PIC X(16).
           05  IC-SOURCE                   PIC X(20).
           05  IC-PERIOD                   PIC X(13).
           05  IC-ACCOUNT                  PIC X(03).
           05  IC-SOURCE2                  PIC 9(07).
           05  IC-SEQ                      PIC X(16).
           05  IC-BAN                      PIC 9(03).
           05  IC-CYCLE2                   PIC S9(05) COMP-3.
           05  IC-MEDIA                    PIC S9(09)V9(02) COMP-3.
           05  CM-FILL-01                  PIC X(7).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-CM-VIEW1 REDEFINES CABS-CM-IN-RECORD.
           05  R0C-MEDIA                   PIC S9(15) COMP-3.
           05  R0C-STATE                   PIC S9(07) COMP-3.
           05  R0C-CIRCUIT                 PIC 9(07).
           05  R0C-SOURCE                  PIC X(04).
           05  R0C-OCN                     PIC 9(07).
           05  R0C-MEDIA2                  PIC S9(11) COMP-3.
           05  R0C-BAN                     PIC S9(11)V9(02) COMP-3.
           05  R0C-CYCLE                   PIC S9(15) COMP-3.
           05  R0C-BAN2                    PIC X(13).
           05  R0C-REST                    PIC X(66).
      * GLOUT - CATALOGUED GENERATION DATA GROUP.
       FD  GLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 90 CHARACTERS.
       01  CABS-CM-OUT-RECORD.
           05  OC-ACCOUNT                  PIC X(02).
           05  OC-STATUS                   PIC S9(09)V9(05) COMP-3.
           05  OC-CARRIER                  PIC X(10).
           05  OC-STATUS2                  PIC 9(05).
           05  OC-LEVEL                    PIC X(08).
           05  OC-TYPE                     PIC X(02).
           05  OC-BAND                     PIC S9(07) COMP-3.
           05  OC-BAN                      PIC S9(11)V9(02) COMP-3.
           05  OC-CLASS                    PIC S9(11) COMP-3.
           05  OC-OCN                      PIC S9(11)V9(05) COMP-3.
           05  OC-INVOICE                  PIC X(13).
           05  OC-TARIFF                   PIC S9(11) COMP-3.
           05  CM-FILL-02                  PIC X(10).
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
      * SHARED LAYOUT PULLED IN FOR THE EXTRACT SIDE.
       COPY CABSCIRC.
      * SHARED LAYOUT PULLED IN FOR THE RANGE SIDE.
       COPY CABSCARR.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUEX27'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V3.27'.
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
       01  WS-COUNT-AREA.
           05  WS-CM-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CM-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CM-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CM-CNT-04                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-CM-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CM-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-CM-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CM-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CM-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-CM-TXT-01                PIC X(10) VALUE SPACES.
           05  WS-CM-TXT-02                PIC X(30) VALUE SPACES.
           05  WS-CM-TXT-03                PIC X(12) VALUE SPACES.
           05  WS-CM-TXT-04                PIC X(26) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-CM-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-CM-ON-01                 VALUE 'Y'.
               88  WS-CM-OFF-01                VALUE 'N'.
           05  WS-CM-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-CM-ON-02                 VALUE 'Y'.
               88  WS-CM-OFF-02                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-CM-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CM-SUB-02                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. LOADED ONCE AT INITIALISATION AND SEARCHED WITH
      * A SUBSCRIPTED WALK.
       01  WS-CM-TABLE.
           05  WS-CM-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-CM-TB-ENTRY OCCURS 150 TIMES
                                       INDEXED BY WS-CM-IX.
               10  WS-CM-TB-KEY                PIC X(06).
               10  WS-CM-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-CM-TB-TXT                PIC X(20).
               10  WS-CM-TB-EFF                PIC 9(05).
               10  WS-CM-TB-EXP                PIC 9(05).
       01  WS-CM-WORK-GROUP-1.
           05  WS-CM-G1-INVOICE            PIC 9(05).
           05  WS-CM-G1-STATUS             PIC X(10).
           05  WS-CM-G1-STATE              PIC X(20).
           05  WS-CM-G1-BAN                PIC X(10).
           05  WS-CM-G1-CODE               PIC X(10).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 58.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUEX27 - CARRIER EXTRACT FOR THE SETTLEMENT FEE'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-CM-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-CM-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9920.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-CM-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-CM-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT USGIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'USGIN OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT GLOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'GLOUT OPEN FAILED - FILE STATUS BAD' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-CM-CYCLE-YYDDD.
           COMPUTE WS-CM-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-CM-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-CM-CNT-04.
           MOVE 0 TO WS-CM-CNT-01.
           MOVE 0 TO WS-CM-CNT-03.
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
           PERFORM P2200-CONVERT-SUBSET THRU P2200-CONVERT-SUBSET-EXIT.
           IF WS-CM-ON-01
               PERFORM P2300-SPLIT-MASTER THRU P2300-SPLIT-MASTER-EXIT.
           IF WS-CM-ON-01
               PERFORM P2400-VALIDATE-SUBSET THRU
                   P2400-VALIDATE-SUBSET-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ USGIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-CONVERT-SUBSET.
           MOVE 'Y' TO WS-CM-SW-02.
           IF IC-CYCLE2 < 33
               MOVE 'N' TO WS-CM-SW-02
               ADD 1 TO WS-CM-CNT-02.
           IF IC-CYCLE2 > 1943
               MOVE 'N' TO WS-CM-SW-02
               ADD 1 TO WS-CM-CNT-04.
       P2200-CONVERT-SUBSET-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P2300-SPLIT-MASTER.
           IF WS-CM-AMT-02 NOT = 0
               COMPUTE WS-CM-QTY-02 = WS-CM-AMT-03 * 100 / WS-CM-AMT-02
           ELSE
               MOVE 0 TO WS-CM-QTY-02.
       P2300-SPLIT-MASTER-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P2400-VALIDATE-SUBSET.
           MOVE 0 TO WS-CM-CNT-04.
           INSPECT WS-CM-TXT-01 TALLYING WS-CM-CNT-04
               FOR ALL SPACES.
           INSPECT WS-CM-TXT-01 REPLACING ALL LOW-VALUES BY SPACES.
       P2400-VALIDATE-SUBSET-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P3100-FORMAT-RANGE.
           ADD IC-MEDIA TO WS-CM-QTY-01.
           COMPUTE WS-CM-AMT-02 ROUNDED = WS-CM-QTY-01 * WS-CM-QTY-01.
           ADD WS-CM-AMT-02 TO WS-CM-AMT-03.
       P3100-FORMAT-RANGE-EXIT.
           EXIT.
       P3200-CLOSE-OFF-CANDIDATE.
           MOVE 0 TO WS-CM-QTY-01.
           MOVE 0 TO WS-CM-QTY-02.
           MOVE 0 TO WS-CM-AMT-02.
       P3200-CLOSE-OFF-CANDIDATE-EXIT.
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
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE 7 TO CT-STEP-SEQ.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE WS-CM-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE SPACES TO CT-FILLER.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE 0 TO CT-RC.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
       P8100-EXIT.
           EXIT.
      * P8200-CHECK-BALANCE - A RECORD THAT WAS HELD FOR THE NEXT
      * CYCLE IS BROUGHT INTO THE CARRIED FORWARD SIDE BEFORE THE
      * EQUATION IS TESTED.
       P8200-CHECK-BALANCE.
           ADD WS-CM-CNT-03 TO CT-CARRIED-FWD.
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
           CLOSE USGIN.
           CLOSE GLOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABUEX27 - STEP COMPLETE'.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  CM-CNT-01 = ' WS-CM-CNT-01.
           DISPLAY '  CM-CNT-03 = ' WS-CM-CNT-03.
           DISPLAY '  CM-CNT-04 = ' WS-CM-CNT-04.
       P9000-EXIT.
           EXIT.
