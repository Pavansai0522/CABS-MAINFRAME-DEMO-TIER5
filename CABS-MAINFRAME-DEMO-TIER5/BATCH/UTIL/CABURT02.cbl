      *****************************************************************
      * CABURT02 - RATE TABLE EFFECTIVE DATE ROLL                     *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               RATIN   TELCABS.CABS.RATIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               TAROUT  TELCABS.CABS.TAROUT         (LOCAL)     *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * REVISION HISTORY                                              *
      *   V1.00  1989-10-08  G.PRZYBYLSKI INITIAL RELEASE             *
      *   V1.01  2007-02-16  A.BUKOWSKI   HASH TOTAL ADDED TO THE     *
      *                      CONTROL RECORD                           *
      *   V1.05  2015-05-25  W.J.MCALLISTER JOB PARAMETER MADE        *
      *                      MANDATORY                                *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABURT02.
       AUTHOR. TELCABS APPLICATIONS - RATE TABLE MAINTENANCE.
      *****************************************************************
      * RATE TABLE EFFECTIVE DATE ROLL. THE STEP RUNS ONCE PER BILL   *
      * CYCLE AND IS RERUN FROM THE TOP IF IT FAILS.                  *
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS*
      * BUILT ON THE SAME ORDER.                                      *
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
      * RATIN - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  RATIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-DP-IN-RECORD.
           05  ID-PERIOD                   PIC X(02).
           05  ID-TARIFF                   PIC S9(09)V9(05) COMP-3.
           05  ID-BAND                     PIC 9(07).
           05  ID-BAN                      PIC X(02).
           05  ID-CENTRE                   PIC S9(11) COMP-3.
           05  ID-STATUS                   PIC S9(05) COMP-3.
           05  ID-PERIOD2                  PIC S9(13) COMP-3.
           05  ID-BAN2                     PIC 9(05).
           05  DP-FILL-01                  PIC X(40).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-DP-VIEW1 REDEFINES CABS-DP-IN-RECORD.
           05  R0D-CIRCUIT                 PIC X(04).
           05  R0D-CODE                    PIC S9(15) COMP-3.
           05  R0D-STATE                   PIC X(10).
           05  R0D-TARIFF                  PIC 9(06).
           05  R0D-SOURCE                  PIC X(06).
           05  R0D-SOURCE2                 PIC X(20).
           05  R0D-ACCOUNT                 PIC X(13).
           05  R0D-REST                    PIC X(13).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-DP-VIEW2 REDEFINES CABS-DP-IN-RECORD.
           05  R1D-CYCLE                   PIC X(04).
           05  R1D-ACCOUNT                 PIC S9(13) COMP-3.
           05  R1D-ACCOUNT2                PIC S9(07)V9(02) COMP-3.
           05  R1D-REGION                  PIC 9(06).
           05  R1D-GROUP                   PIC X(02).
           05  R1D-PERIOD                  PIC X(13).
           05  R1D-REST                    PIC X(43).
      * TAROUT - CATALOGUED GENERATION DATA GROUP.
       FD  TAROUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-DP-OUT-RECORD.
           05  OD-CYCLE                    PIC 9(02).
           05  OD-STATE                    PIC X(06).
           05  OD-JURIS                    PIC X(06).
           05  OD-BAND                     PIC S9(11) COMP-3.
           05  OD-SEGMENT                  PIC 9(03).
           05  OD-PERIOD                   PIC 9(06).
           05  OD-CENTRE                   PIC S9(11)V9(05) COMP-3.
           05  OD-BAND2                    PIC 9(04).
           05  OD-INVOICE                  PIC S9(13) COMP-3.
           05  OD-BAND3                    PIC S9(13)V9(02) COMP-3.
           05  DP-FILL-02                  PIC X(23).
      * SUSOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
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
      * SHARED LAYOUT PULLED IN FOR THE TARIFF SIDE.
       COPY CABSRATE.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABURT02'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V2.21'.
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
           05  WS-DP-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DP-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DP-CNT-03                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-DP-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DP-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DP-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DP-QTY-04                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-DP-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DP-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DP-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DP-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-DP-TXT-01                PIC X(12) VALUE SPACES.
           05  WS-DP-TXT-02                PIC X(26) VALUE SPACES.
           05  WS-DP-TXT-03                PIC X(16) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-DP-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-DP-ON-01                 VALUE 'Y'.
               88  WS-DP-OFF-01                VALUE 'N'.
           05  WS-DP-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-DP-ON-02                 VALUE 'Y'.
               88  WS-DP-OFF-02                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-DP-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DP-SUB-02                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. LOADED ONCE AT INITIALISATION AND SEARCHED WITH
      * A SUBSCRIPTED WALK.
       01  WS-DP-TABLE.
           05  WS-DP-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-DP-TB-ENTRY OCCURS 100 TIMES
                                       INDEXED BY WS-DP-IX.
               10  WS-DP-TB-KEY                PIC X(10).
               10  WS-DP-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-DP-TB-TXT                PIC X(40).
               10  WS-DP-TB-EFF                PIC 9(05).
               10  WS-DP-TB-EXP                PIC 9(05).
       01  WS-DP-WORK-GROUP-1.
           05  WS-DP-G1-CARRIER            PIC X(10).
           05  WS-DP-G1-BAND               PIC 9(05).
           05  WS-DP-G1-REGION             PIC X(20).
           05  WS-DP-G1-CENTRE             PIC X(10).
           05  WS-DP-G1-CIRCUIT            PIC X(20).
           05  WS-DP-G1-CARRIER            PIC S9(09) COMP-3.
           05  WS-DP-G1-PERIOD             PIC S9(09) COMP-3.
           05  WS-DP-G1-GROUP              PIC S9(09) COMP-3.
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 50.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABURT02 - RATE TABLE EFFECTIVE DATE ROLL'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-DP-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-DP-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9954.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-DP-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-DP-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
               MOVE 'OPEN FAILED ON RATIN - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT TAROUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON TAROUT - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SUSOUT.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON SUSOUT - CHECK THE ALLOCATION' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-DP-CYCLE-YYDDD.
           COMPUTE WS-DP-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-DP-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-DP-CNT-03.
           MOVE 0 TO WS-DP-CNT-02.
           MOVE 0 TO WS-DP-CNT-01.
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
           PERFORM P2200-DERIVE-DESCRIPTION THRU
               P2200-DERIVE-DESCRIPTION-EXIT.
           PERFORM P2300-APPLY-TARIFF THRU P2300-APPLY-TARIFF-EXIT.
           PERFORM P2400-CHECK-ELEMENT THRU P2400-CHECK-ELEMENT-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ RATIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-DERIVE-DESCRIPTION.
           MOVE 'N' TO WS-DP-SW-01.
           IF WS-DP-TXT-02 NOT = WS-DP-TXT-03
               MOVE 'Y' TO WS-DP-SW-01
               MOVE WS-DP-TXT-02 TO WS-DP-TXT-03
               ADD 1 TO WS-DP-CNT-02.
       P2200-DERIVE-DESCRIPTION-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P2300-APPLY-TARIFF.
           MOVE 'Y' TO WS-DP-SW-02.
           IF ID-BAN2 < 32
               MOVE 'N' TO WS-DP-SW-02
               ADD 1 TO WS-DP-CNT-01.
           IF ID-BAN2 > 7627
               MOVE 'N' TO WS-DP-SW-02
               ADD 1 TO WS-DP-CNT-02.
       P2300-APPLY-TARIFF-EXIT.
           EXIT.
       P2400-CHECK-ELEMENT.
           CALL 'CABHASH' USING ID-PERIOD2 WS-ACC-OCN-HASH.
           ADD WS-DP-CNT-02 TO WS-ACC-SEQ-HASH.
       P2400-CHECK-ELEMENT-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-EMIT-WINDOW.
           MOVE ID-BAN2 TO WS-DP-TXT-01.
           MOVE ID-BAN2 TO WS-DP-TXT-01.
           MOVE ID-CENTRE TO WS-DP-TXT-02.
           MOVE ID-BAN TO WS-DP-TXT-02.
           ADD 1 TO WS-DP-CNT-01.
       P3100-EMIT-WINDOW-EXIT.
           EXIT.
       P3200-POST-TARIFF.
           ADD ID-TARIFF TO WS-DP-QTY-04.
           COMPUTE WS-DP-AMT-04 ROUNDED = WS-DP-QTY-04 * WS-DP-QTY-02.
           ADD WS-DP-AMT-04 TO WS-DP-AMT-01.
       P3200-POST-TARIFF-EXIT.
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
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE 1 TO CT-STEP-SEQ.
           MOVE WS-DP-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE WS-DP-TXT-01 TO CT-RESTART-KEY.
           MOVE SPACES TO CT-FILLER.
           MOVE 0 TO CT-RC.
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
           CLOSE RATIN.
           CLOSE TAROUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABURT02 - END OF RUN'.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  DP-CNT-01 = ' WS-DP-CNT-01.
           DISPLAY '  DP-CNT-03 = ' WS-DP-CNT-03.
       P9000-EXIT.
           EXIT.
