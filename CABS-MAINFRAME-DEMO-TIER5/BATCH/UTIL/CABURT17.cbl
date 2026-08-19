      *****************************************************************
      * CABURT17 - JURISDICTION TABLE MAINTENANCE                     *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               TBLIN   TELCABS.CABS.TBLIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               TAROUT  TELCABS.CABS.TAROUT         (LOCAL)     *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - THE OUTPUT IS REBUILT EACH RUN     *
      * REVISION HISTORY                                              *
      *   V1.00  1998-05-11  B.R.HALVORSEN INITIAL RELEASE            *
      *   V1.02  2008-04-20  L.FERREIRA   PARM CARD EXTENDED,         *
      *                      POSITIONS 40 THROUGH 48                  *
      *   V1.05  2016-11-10  A.BUKOWSKI   TABLE LIMIT RAISED FOR THE  *
      *                      SOUTHEAST CENTRES                        *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABURT17.
       AUTHOR. TELCABS APPLICATIONS - RATE TABLE MAINTENANCE.
      *****************************************************************
      * JURISDICTION TABLE MAINTENANCE. THIS STEP IS SCHEDULED INSIDE *
      * THE NIGHTLY ACCESS BILLING STREAM AND HAS NO INTERACTIVE ENTRY*
      * POINT.                                                        *
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL*
      * CHARACTER CARRIES MEANING DOWNSTREAM.                         *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           C01 IS TO-NEW-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TBLIN ASSIGN TO UT-S-TBLIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT TAROUT ASSIGN TO UT-S-TAROUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT CTLOUT ASSIGN TO UT-S-CTLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      * TBLIN - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  TBLIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-BX-IN-RECORD.
           05  IB-BAND                     PIC X(02).
           05  IB-OCN                      PIC 9(04).
           05  IB-JURIS                    PIC S9(05) COMP-3.
           05  IB-STATUS                   PIC S9(11)V9(02) COMP-3.
           05  IB-LEVEL                    PIC X(13).
           05  IB-ACCOUNT                  PIC S9(11) COMP-3.
           05  IB-SEQ                      PIC X(13).
           05  IB-CIRCUIT                  PIC 9(02).
           05  BX-FILL-01                  PIC X(30).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-BX-VIEW1 REDEFINES CABS-BX-IN-RECORD.
           05  R0B-GROUP                   PIC S9(13)V9(02) COMP-3.
           05  R0B-BAND                    PIC X(08).
           05  R0B-SOURCE                  PIC 9(07).
           05  R0B-BAND2                   PIC X(03).
           05  R0B-SOURCE2                 PIC X(13).
           05  R0B-REST                    PIC X(41).
      * TAROUT - CATALOGUED GENERATION DATA GROUP.
       FD  TAROUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 90 CHARACTERS.
       01  CABS-BX-OUT-RECORD.
           05  OB-BAND                     PIC X(10).
           05  OB-MEDIA                    PIC S9(07)V9(05) COMP-3.
           05  OB-CARRIER                  PIC X(08).
           05  OB-TARIFF                   PIC S9(11) COMP-3.
           05  OB-SOURCE                   PIC 9(04).
           05  OB-REGION                   PIC 9(07).
           05  OB-STATUS                   PIC 9(03).
           05  OB-LEVEL                    PIC S9(13)V9(05) COMP-3.
           05  OB-CIRCUIT                  PIC S9(07)V9(02) COMP-3.
           05  OB-LEVEL2                   PIC 9(02).
           05  OB-CYCLE                    PIC 9(07).
           05  OB-TYPE                     PIC X(20).
           05  BX-FILL-02                  PIC X(1).
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
       COPY CABSCOMM.
      * SHARED LAYOUT PULLED IN FOR THE ELEMENT SIDE.
       COPY CABSRT01.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABURT17'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V3.19'.
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
           05  WS-BX-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BX-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BX-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BX-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BX-CNT-05                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-BX-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BX-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BX-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-BX-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BX-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BX-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-BX-TXT-01                PIC X(10) VALUE SPACES.
           05  WS-BX-TXT-02                PIC X(10) VALUE SPACES.
           05  WS-BX-TXT-03                PIC X(08) VALUE SPACES.
           05  WS-BX-TXT-04                PIC X(10) VALUE SPACES.
           05  WS-BX-TXT-05                PIC X(26) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-BX-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-BX-ON-01                 VALUE 'Y'.
               88  WS-BX-OFF-01                VALUE 'N'.
           05  WS-BX-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-BX-ON-02                 VALUE 'Y'.
               88  WS-BX-OFF-02                VALUE 'N'.
           05  WS-BX-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-BX-ON-03                 VALUE 'Y'.
               88  WS-BX-OFF-03                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-BX-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-BX-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-BX-SUB-03                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. A ROW THAT DOES NOT FIT IS COUNTED AND DROPPED
      * - THE STEP DOES NOT FAIL.
       01  WS-BX-TABLE.
           05  WS-BX-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-BX-TB-ENTRY OCCURS 150 TIMES
                                       INDEXED BY WS-BX-IX.
               10  WS-BX-TB-KEY                PIC X(06).
               10  WS-BX-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-BX-TB-TXT                PIC X(30).
               10  WS-BX-TB-EFF                PIC 9(05).
               10  WS-BX-TB-EXP                PIC 9(05).
       01  WS-BX-WORK-GROUP-1.
           05  WS-BX-G1-PERIOD             PIC X(10).
           05  WS-BX-G1-CARRIER            PIC 9(05).
           05  WS-BX-G1-CARRIER            PIC S9(11)V9(02) COMP-3.
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 60.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABURT17 - JURISDICTION TABLE MAINTENANCE'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-BX-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-BX-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9973.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-BX-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-BX-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT TBLIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'TBLIN COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT TAROUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'TAROUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-BX-CYCLE-YYDDD.
           COMPUTE WS-BX-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-BX-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-BX-CNT-03.
           MOVE 0 TO WS-BX-CNT-05.
           MOVE 0 TO WS-BX-CNT-02.
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
           IF WS-BX-ON-01
               PERFORM P2200-DERIVE-ROW THRU P2200-DERIVE-ROW-EXIT.
           PERFORM P2300-EXPAND-OVERRIDE THRU
               P2300-EXPAND-OVERRIDE-EXIT.
           IF WS-BX-ON-03
               PERFORM P2400-DERIVE-BAND THRU P2400-DERIVE-BAND-EXIT.
           PERFORM P2500-SELECT-OVERRIDE THRU
               P2500-SELECT-OVERRIDE-EXIT.
           PERFORM P2600-EDIT-TARIFF THRU P2600-EDIT-TARIFF-EXIT.
           IF WS-BX-ON-02
               PERFORM P2700-CONVERT-KEY THRU P2700-CONVERT-KEY-EXIT.
           PERFORM P2800-EDIT-KEY THRU P2800-EDIT-KEY-EXIT.
           IF WS-BX-ON-02
               PERFORM P2900-CHECK-OVERRIDE THRU
                   P2900-CHECK-OVERRIDE-EXIT.
           PERFORM P21000-VALIDATE-TARIFF THRU
               P21000-VALIDATE-TARIFF-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ TBLIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-DERIVE-ROW.
           MOVE SPACES TO CABS-BX-OUT-RECORD.
           MOVE IB-ACCOUNT TO OB-BAND.
           MOVE IB-STATUS TO OB-MEDIA.
           MOVE IB-CIRCUIT TO OB-CARRIER.
           MOVE IB-LEVEL TO OB-TARIFF.
           MOVE IB-BAND TO OB-SOURCE.
           MOVE IB-STATUS TO OB-REGION.
           MOVE IB-CIRCUIT TO OB-STATUS.
           MOVE IB-CIRCUIT TO OB-LEVEL.
           WRITE CABS-BX-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P2200-DERIVE-ROW-EXIT.
           EXIT.
       P2300-EXPAND-OVERRIDE.
           ADD IB-ACCOUNT TO WS-BX-QTY-02.
           COMPUTE WS-BX-AMT-01 ROUNDED = WS-BX-QTY-02 * WS-BX-QTY-02.
           ADD WS-BX-AMT-01 TO WS-BX-AMT-02.
       P2300-EXPAND-OVERRIDE-EXIT.
           EXIT.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P2400-DERIVE-BAND.
           UNSTRING WS-BX-TXT-02 DELIMITED BY '/'
               INTO WS-BX-TXT-05
               WS-BX-TXT-03
               TALLYING IN WS-BX-CNT-05.
       P2400-DERIVE-BAND-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P2500-SELECT-OVERRIDE.
           IF WS-BX-AMT-02 NOT = 0
               COMPUTE WS-BX-QTY-03 = WS-BX-AMT-03 * 100 / WS-BX-AMT-02
           ELSE
               MOVE 0 TO WS-BX-QTY-03.
       P2500-SELECT-OVERRIDE-EXIT.
           EXIT.
       P2600-EDIT-TARIFF.
           IF WS-BX-AMT-02 < 5
               MOVE 5 TO WS-BX-AMT-02
               ADD 1 TO WS-BX-CNT-02.
           IF WS-BX-AMT-02 > 18553
               MOVE 18553 TO WS-BX-AMT-02
               ADD 1 TO WS-BX-CNT-04.
       P2600-EDIT-TARIFF-EXIT.
           EXIT.
       P2700-CONVERT-KEY.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-BX-TXT-05 TO PC-COL-001-020.
           MOVE WS-BX-TXT-03 TO PC-COL-021-060.
           MOVE WS-BX-AMT-01 TO WS-BX-AMT-EDIT.
           MOVE WS-BX-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P2700-CONVERT-KEY-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P2800-EDIT-KEY.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-OCN-UNKNOWN TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-BX-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
       P2800-EDIT-KEY-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P2900-CHECK-OVERRIDE.
           MOVE SPACES TO WS-BX-TXT-04.
           STRING IB-JURIS DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IB-BAND DELIMITED BY SIZE
               INTO WS-BX-TXT-04.
       P2900-CHECK-OVERRIDE-EXIT.
           EXIT.
       P21000-VALIDATE-TARIFF.
           MOVE IB-LEVEL TO WS-BX-TXT-05.
           MOVE IB-OCN TO WS-BX-TXT-03.
           ADD 1 TO WS-BX-CNT-04.
       P21000-VALIDATE-TARIFF-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P3100-EMIT-KEY.
           MOVE 0 TO WS-BX-QTY-02.
           MOVE 0 TO WS-BX-QTY-03.
           MOVE 0 TO WS-BX-QTY-01.
           MOVE 0 TO WS-BX-AMT-03.
       P3100-EMIT-KEY-EXIT.
           EXIT.
       P3200-STAGE-ROW.
           MOVE IB-BAND TO WS-BX-TXT-02.
           MOVE IB-BAND TO WS-BX-TXT-05.
           MOVE IB-JURIS TO WS-BX-TXT-04.
           MOVE IB-CIRCUIT TO WS-BX-TXT-03.
           ADD 1 TO WS-BX-CNT-04.
       P3200-STAGE-ROW-EXIT.
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
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE WS-BX-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE 9 TO CT-STEP-SEQ.
           MOVE SPACES TO CT-FILLER.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE WS-BX-TXT-01 TO CT-RESTART-KEY.
           MOVE 0 TO CT-RC.
       P8100-EXIT.
           EXIT.
      * P8200-CHECK-BALANCE - A RECORD THAT WAS HELD FOR THE NEXT
      * CYCLE IS BROUGHT INTO THE CARRIED FORWARD SIDE BEFORE THE
      * EQUATION IS TESTED.
       P8200-CHECK-BALANCE.
           ADD WS-BX-CNT-04 TO CT-CARRIED-FWD.
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
           CLOSE TBLIN.
           CLOSE TAROUT.
           CLOSE CTLOUT.
           DISPLAY 'CABURT17 - END OF RUN'.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  BX-CNT-02 = ' WS-BX-CNT-02.
           DISPLAY '  BX-CNT-04 = ' WS-BX-CNT-04.
       P9000-EXIT.
           EXIT.
