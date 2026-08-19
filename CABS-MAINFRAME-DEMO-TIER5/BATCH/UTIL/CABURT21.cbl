      *****************************************************************
      * CABURT21 - RATE ELEMENT DESCRIPTION MAINTENANCE               *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               TARIN   TELCABS.CABS.TARIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               TAROUT  TELCABS.CABS.TAROUT         (LOCAL)     *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * REVISION HISTORY                                              *
      *   V1.00  1992-07-02  B.R.HALVORSEN INITIAL RELEASE            *
      *   V1.04  1996-11-20  T.YAMASHITA  TABLE LIMIT RAISED FOR THE  *
      *                      SOUTHEAST CENTRES                        *
      *   V1.06  1997-02-21  R.T.WHEELER  RESTART KEY WRITTEN SO A    *
      *                      RERUN CAN POSITION                       *
      *   V1.07  2005-05-05  C.ADEYEMI    EFFECTIVE DATE FILTER ADDED *
      *                      PER AUDIT FINDING                        *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABURT21.
       AUTHOR. TELCABS APPLICATIONS - RATE TABLE MAINTENANCE.
      *****************************************************************
      * RATE ELEMENT DESCRIPTION MAINTENANCE. THE STEP RUNS ONCE PER  *
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
           SELECT TARIN ASSIGN TO UT-S-TARIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT TAROUT ASSIGN TO UT-S-TAROUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT SUSOUT ASSIGN TO UT-S-SUSOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-SUSPENSE.
           SELECT CTLOUT ASSIGN TO UT-S-CTLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      * TARIN - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  TARIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-EH-IN-RECORD.
           05  IE-TARIFF                   PIC X(08).
           05  IE-SEGMENT                  PIC S9(11)V9(05) COMP-3.
           05  IE-ACCOUNT                  PIC S9(15) COMP-3.
           05  IE-CARRIER                  PIC S9(13)V9(02) COMP-3.
           05  IE-CENTRE                   PIC 9(04).
           05  IE-SEGMENT2                 PIC S9(09) COMP-3.
           05  IE-PERIOD                   PIC 9(07).
           05  IE-INVOICE                  PIC X(06).
           05  EH-FILL-01                  PIC X(25).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-EH-VIEW1 REDEFINES CABS-EH-IN-RECORD.
           05  R0E-CIRCUIT                 PIC X(08).
           05  R0E-BAN                     PIC S9(09)V9(02) COMP-3.
           05  R0E-TARIFF                  PIC X(08).
           05  R0E-BAN2                    PIC S9(07) COMP-3.
           05  R0E-GROUP                   PIC X(04).
           05  R0E-REST                    PIC X(50).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-EH-VIEW2 REDEFINES CABS-EH-IN-RECORD.
           05  R1E-SEGMENT                 PIC X(16).
           05  R1E-JURIS                   PIC X(16).
           05  R1E-SEGMENT2                PIC S9(07)V9(02) COMP-3.
           05  R1E-ACCOUNT                 PIC S9(05) COMP-3.
           05  R1E-REGION                  PIC S9(09)V9(02) COMP-3.
           05  R1E-STATE                   PIC X(16).
           05  R1E-REST                    PIC X(18).
      * TAROUT - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  TAROUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-EH-OUT-RECORD.
           05  OE-SOURCE                   PIC S9(15) COMP-3.
           05  OE-CLASS                    PIC 9(06).
           05  OE-CLASS2                   PIC X(08).
           05  OE-REGION                   PIC S9(07) COMP-3.
           05  OE-LEVEL                    PIC S9(09)V9(02) COMP-3.
           05  OE-CYCLE                    PIC 9(07).
           05  OE-CYCLE2                   PIC X(03).
           05  OE-CLASS3                   PIC X(13).
           05  OE-PERIOD                   PIC X(06).
           05  OE-MEDIA                    PIC S9(13)V9(02) COMP-3.
           05  OE-SOURCE2                  PIC 9(02).
           05  OE-CENTRE                   PIC 9(02).
           05  EH-FILL-02                  PIC X(7).
      * SUSOUT - CATALOGUED GENERATION DATA GROUP.
       FD  SUSOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 300 CHARACTERS.
       01  CABS-SUSOUT-RECORD              PIC X(300).
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
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABURT21'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V3.12'.
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
       01  WS-COUNT-AREA.
           05  WS-EH-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-EH-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-EH-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-EH-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-EH-CNT-05                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-EH-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-EH-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-EH-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-EH-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-EH-TXT-01                PIC X(10) VALUE SPACES.
           05  WS-EH-TXT-02                PIC X(12) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-EH-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-EH-ON-01                 VALUE 'Y'.
               88  WS-EH-OFF-01                VALUE 'N'.
           05  WS-EH-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-EH-ON-02                 VALUE 'Y'.
               88  WS-EH-OFF-02                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-EH-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-EH-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-EH-SUB-03                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. LOADED ONCE AT INITIALISATION AND SEARCHED WITH
      * A SUBSCRIPTED WALK.
       01  WS-EH-TABLE.
           05  WS-EH-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-EH-TB-ENTRY OCCURS 50 TIMES
                                       INDEXED BY WS-EH-IX.
               10  WS-EH-TB-KEY                PIC X(04).
               10  WS-EH-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-EH-TB-TXT                PIC X(20).
               10  WS-EH-TB-EFF                PIC 9(05).
               10  WS-EH-TB-EXP                PIC 9(05).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 50.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABURT21 - RATE ELEMENT DESCRIPTION MAINTENANCE'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-EH-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-EH-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9987.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-EH-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-EH-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT TARIN.
           IF WS-FS-INPUT NOT = '00'
               DISPLAY 'TARIN FILE STATUS = ' WS-FS-INPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'TARIN NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT TAROUT.
           IF WS-FS-OUTPUT NOT = '00'
               DISPLAY 'TAROUT FILE STATUS = ' WS-FS-OUTPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'TAROUT NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SUSOUT.
           IF WS-FS-SUSPENSE NOT = '00'
               DISPLAY 'SUSOUT FILE STATUS = ' WS-FS-SUSPENSE
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'SUSOUT NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               DISPLAY 'CTLOUT FILE STATUS = ' WS-FS-CONTROL
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
           MOVE PC1-CYCLE-YYDDD TO WS-EH-CYCLE-YYDDD.
           COMPUTE WS-EH-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-EH-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-EH-CNT-02.
           MOVE 0 TO WS-EH-CNT-04.
           MOVE 0 TO WS-EH-CNT-03.
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
           IF WS-EH-ON-01
               PERFORM P2200-CHECK-KEY THRU P2200-CHECK-KEY-EXIT.
           PERFORM P2300-BUILD-ROW THRU P2300-BUILD-ROW-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ TARIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P2200-CHECK-KEY.
           MOVE SPACES TO WS-EH-TXT-01.
           STRING IE-CARRIER DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IE-PERIOD DELIMITED BY SIZE
               INTO WS-EH-TXT-01.
       P2200-CHECK-KEY-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P2300-BUILD-ROW.
           MOVE 0 TO WS-EH-CNT-02.
           INSPECT WS-EH-TXT-02 TALLYING WS-EH-CNT-02
               FOR ALL SPACES.
           INSPECT WS-EH-TXT-02 REPLACING ALL LOW-VALUES BY SPACES.
       P2300-BUILD-ROW-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-STAGE-WINDOW.
           MOVE SPACES TO WS-EH-TXT-02.
           STRING IE-TARIFF DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IE-PERIOD DELIMITED BY SIZE
               INTO WS-EH-TXT-02.
       P3100-STAGE-WINDOW-EXIT.
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
           MOVE 0 TO CT-RERUN-NBR.
           MOVE 2 TO CT-STEP-SEQ.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE WS-EH-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE SPACES TO CT-FILLER.
           MOVE 0 TO CT-RC.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-READ-CNT TO CT-READ.
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
           CLOSE TARIN.
           CLOSE TAROUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABURT21 - END OF RUN'.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  EH-CNT-03 = ' WS-EH-CNT-03.
           DISPLAY '  EH-CNT-02 = ' WS-EH-CNT-02.
           DISPLAY '  EH-CNT-01 = ' WS-EH-CNT-01.
       P9000-EXIT.
           EXIT.
