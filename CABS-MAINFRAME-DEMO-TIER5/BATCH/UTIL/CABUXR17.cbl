      *****************************************************************
      * CABUXR17 - CONTROL RECORD CROSS REFERENCE                     *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               XRFIN   TELCABS.CABS.XRFIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               LNKOUT  TELCABS.CABS.LNKOUT         (LOCAL)     *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - NOTHING IS UPDATED IN PLACE        *
      * REVISION HISTORY                                              *
      *   V1.00  1993-06-07  G.PRZYBYLSKI INITIAL RELEASE             *
      *   V1.03  1994-02-15  C.ADEYEMI    HASH TOTAL ADDED TO THE     *
      *                      CONTROL RECORD                           *
      *   V1.04  1998-07-15  R.T.WHEELER  RETIRED THE SECOND SORT STEP*
      *                      - DONE IN PROGRAM                        *
      *   V1.05  2014-08-24  C.ADEYEMI    BLOCK SIZE SET TO ZERO -    *
      *                      SYSTEM DETERMINED                        *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUXR17.
       AUTHOR. TELCABS APPLICATIONS - CROSS REFERENCE REPORT.
      *****************************************************************
      * CONTROL RECORD CROSS REFERENCE. THE STEP RUNS ONCE PER BILL   *
      * CYCLE AND IS RERUN FROM THE TOP IF IT FAILS.                  *
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND   *
      * ARE NOT PART OF THE BALANCE.                                  *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           C01 IS TO-NEW-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT XRFIN ASSIGN TO UT-S-XRFIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT LNKOUT ASSIGN TO UT-S-LNKOUT
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
      * XRFIN - PERMANENT DATASET HELD ON DASD.
       FD  XRFIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-AA-IN-RECORD.
           05  IA-STATUS                   PIC X(04).
           05  IA-OCN                      PIC X(04).
           05  IA-LEVEL                    PIC X(08).
           05  IA-STATUS2                  PIC 9(09).
           05  IA-SEQ                      PIC X(08).
           05  IA-ACCOUNT                  PIC 9(05).
           05  IA-SEQ2                     PIC 9(03).
           05  IA-CLASS                    PIC S9(11)V9(02) COMP-3.
           05  IA-SEGMENT                  PIC X(16).
           05  IA-CIRCUIT                  PIC X(02).
           05  AA-FILL-01                  PIC X(14).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-AA-VIEW1 REDEFINES CABS-AA-IN-RECORD.
           05  R0A-TARGET                  PIC 9(04).
           05  R0A-ACCOUNT                 PIC S9(11) COMP-3.
           05  R0A-PERIOD                  PIC 9(04).
           05  R0A-BAND                    PIC S9(07) COMP-3.
           05  R0A-GROUP                   PIC X(02).
           05  R0A-REGION                  PIC S9(13) COMP-3.
           05  R0A-ACCOUNT2                PIC 9(04).
           05  R0A-PERIOD2                 PIC X(16).
           05  R0A-REST                    PIC X(33).
      * LNKOUT - CATALOGUED GENERATION DATA GROUP.
       FD  LNKOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-AA-OUT-RECORD.
           05  OA-OCN                      PIC 9(09).
           05  OA-ACCOUNT                  PIC S9(09)V9(02) COMP-3.
           05  OA-INVOICE                  PIC S9(13) COMP-3.
           05  OA-OCN2                     PIC S9(07)V9(02) COMP-3.
           05  OA-CLASS                    PIC X(13).
           05  OA-ELEM                     PIC X(06).
           05  OA-MEDIA                    PIC X(06).
           05  OA-TARIFF                   PIC 9(03).
           05  OA-CYCLE                    PIC X(06).
           05  OA-CODE                     PIC X(13).
           05  AA-FILL-02                  PIC X(6).
      * SUSOUT - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  SUSOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 300 CHARACTERS.
       01  CABS-SUSOUT-RECORD              PIC X(300).
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
      * SHARED LAYOUT PULLED IN FOR THE ORPHAN SIDE.
       COPY CABSCARR.
      * SHARED LAYOUT PULLED IN FOR THE PAIR SIDE.
       COPY CABSCIRC.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUXR17'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V3.13'.
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
           05  WS-AA-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AA-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AA-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AA-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AA-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AA-CNT-06                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-AA-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AA-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-AA-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AA-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AA-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-AA-TXT-01                PIC X(16) VALUE SPACES.
           05  WS-AA-TXT-02                PIC X(20) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-AA-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-AA-ON-01                 VALUE 'Y'.
               88  WS-AA-OFF-01                VALUE 'N'.
           05  WS-AA-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-AA-ON-02                 VALUE 'Y'.
               88  WS-AA-OFF-02                VALUE 'N'.
           05  WS-AA-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-AA-ON-03                 VALUE 'Y'.
               88  WS-AA-OFF-03                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-AA-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-AA-SUB-02                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. A ROW THAT DOES NOT FIT IS COUNTED AND DROPPED
      * - THE STEP DOES NOT FAIL.
       01  WS-AA-TABLE.
           05  WS-AA-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-AA-TB-ENTRY OCCURS 100 TIMES
                                       INDEXED BY WS-AA-IX.
               10  WS-AA-TB-KEY                PIC X(10).
               10  WS-AA-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-AA-TB-TXT                PIC X(30).
               10  WS-AA-TB-EFF                PIC 9(05).
               10  WS-AA-TB-EXP                PIC 9(05).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 50.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUXR17 - CONTROL RECORD CROSS REFERENCE'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-AA-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-AA-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9925.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-AA-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-AA-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT XRFIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'XRFIN COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT LNKOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'LNKOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SUSOUT.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'SUSOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CTLOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-AA-CYCLE-YYDDD.
           COMPUTE WS-AA-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-AA-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-AA-CNT-06.
           MOVE 0 TO WS-AA-CNT-01.
           MOVE 0 TO WS-AA-CNT-04.
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
           PERFORM P2200-SELECT-GROUP THRU P2200-SELECT-GROUP-EXIT.
           PERFORM P2300-CHECK-MATCH THRU P2300-CHECK-MATCH-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ XRFIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-SELECT-GROUP.
           CALL 'CABHASH' USING IA-SEGMENT WS-ACC-OCN-HASH.
           ADD WS-AA-CNT-06 TO WS-ACC-SEQ-HASH.
       P2200-SELECT-GROUP-EXIT.
           EXIT.
       P2300-CHECK-MATCH.
           MOVE SPACES TO WS-AA-TXT-01.
           STRING IA-ACCOUNT DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IA-CIRCUIT DELIMITED BY SIZE
               INTO WS-AA-TXT-01.
       P2300-CHECK-MATCH-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
      * A FAILURE HERE SUSPENDS THE RECORD, IT DOES NOT STOP THE STEP.
       P3100-WRITE-GROUP.
           ADD IA-CLASS TO WS-AA-QTY-01.
           COMPUTE WS-AA-AMT-03 ROUNDED = WS-AA-QTY-01 * WS-AA-QTY-01.
           ADD WS-AA-AMT-03 TO WS-AA-AMT-01.
       P3100-WRITE-GROUP-EXIT.
           EXIT.
       P3200-FORMAT-ORPHAN.
           MOVE IA-ACCOUNT TO WS-AA-TXT-01.
           MOVE IA-STATUS2 TO WS-AA-TXT-02.
           MOVE IA-SEQ TO WS-AA-TXT-02.
           ADD 1 TO WS-AA-CNT-03.
       P3200-FORMAT-ORPHAN-EXIT.
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
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE 9 TO CT-STEP-SEQ.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE WS-AA-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE SPACES TO CT-FILLER.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE 0 TO CT-RC.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-READ-CNT TO CT-READ.
       P8100-EXIT.
           EXIT.
      * P8200-CHECK-BALANCE - A RECORD THAT WAS HELD FOR THE NEXT
      * CYCLE IS BROUGHT INTO THE CARRIED FORWARD SIDE BEFORE THE
      * EQUATION IS TESTED.
       P8200-CHECK-BALANCE.
           ADD WS-AA-CNT-02 TO CT-CARRIED-FWD.
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
           CLOSE XRFIN.
           CLOSE LNKOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABUXR17 - NORMAL END OF JOB'.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  AA-CNT-04 = ' WS-AA-CNT-04.
           DISPLAY '  AA-CNT-06 = ' WS-AA-CNT-06.
           DISPLAY '  AA-CNT-05 = ' WS-AA-CNT-05.
       P9000-EXIT.
           EXIT.
