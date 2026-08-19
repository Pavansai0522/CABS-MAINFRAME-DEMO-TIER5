      *****************************************************************
      * CABURT11 - RATE ELEMENT DESCRIPTION MAINTENANCE               *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               OVRIN   TELCABS.CABS.OVRIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               BNDOUT  TELCABS.CABS.BNDOUT         (LOCAL)     *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - NOTHING IS UPDATED IN PLACE        *
      * REVISION HISTORY                                              *
      *   V1.00  1994-09-28  G.PRZYBYLSKI INITIAL RELEASE             *
      *   V1.04  1995-03-19  S.MARCHETTI  HASH TOTAL ADDED TO THE     *
      *                      CONTROL RECORD                           *
      *   V1.08  2017-12-21  J.M.CASTILLO SUSPENSE WRITE ADDED -      *
      *                      RECORDS WERE BEING DROPPED               *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABURT11.
       AUTHOR. TELCABS APPLICATIONS - RATE TABLE MAINTENANCE.
      *****************************************************************
      * RATE ELEMENT DESCRIPTION MAINTENANCE. THIS STEP IS SCHEDULED  *
      * INSIDE THE NIGHTLY ACCESS BILLING STREAM AND HAS NO           *
      * INTERACTIVE ENTRY POINT.                                      *
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
           SELECT OVRIN ASSIGN TO UT-S-OVRIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT BNDOUT ASSIGN TO UT-S-BNDOUT
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
      * OVRIN - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  OVRIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 90 CHARACTERS.
       01  CABS-DJ-IN-RECORD.
           05  ID-LEVEL                    PIC X(16).
           05  ID-TYPE                     PIC X(13).
           05  ID-INVOICE                  PIC S9(07) COMP-3.
           05  ID-INVOICE2                 PIC S9(15) COMP-3.
           05  ID-SEGMENT                  PIC S9(09)V9(02) COMP-3.
           05  ID-STATE                    PIC 9(07).
           05  ID-CYCLE                    PIC 9(03).
           05  ID-STATUS                   PIC S9(13) COMP-3.
           05  ID-ELEM                     PIC 9(09).
           05  ID-GROUP                    PIC S9(07)V9(02) COMP-3.
           05  ID-ACCOUNT                  PIC 9(04).
           05  DJ-FILL-01                  PIC X(8).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-DJ-VIEW1 REDEFINES CABS-DJ-IN-RECORD.
           05  R0D-PERIOD                  PIC 9(07).
           05  R0D-MEDIA                   PIC X(04).
           05  R0D-CIRCUIT                 PIC X(06).
           05  R0D-STATE                   PIC 9(04).
           05  R0D-SEQ                     PIC X(02).
           05  R0D-STATUS                  PIC S9(15) COMP-3.
           05  R0D-JURIS                   PIC 9(09).
           05  R0D-REST                    PIC X(50).
      * BNDOUT - CATALOGUED GENERATION DATA GROUP.
       FD  BNDOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 90 CHARACTERS.
       01  CABS-DJ-OUT-RECORD.
           05  OD-BAND                     PIC S9(11) COMP-3.
           05  OD-REGION                   PIC X(04).
           05  OD-SOURCE                   PIC X(16).
           05  OD-REGION2                  PIC X(20).
           05  OD-BAND2                    PIC X(08).
           05  OD-CARRIER                  PIC S9(05) COMP-3.
           05  OD-BAND3                    PIC 9(02).
           05  OD-INVOICE                  PIC S9(11)V9(02) COMP-3.
           05  OD-INVOICE2                 PIC S9(09)V9(02) COMP-3.
           05  OD-TYPE                     PIC S9(13)V9(02) COMP-3.
           05  OD-BAND4                    PIC X(03).
           05  DJ-FILL-02                  PIC X(7).
      * SUSOUT - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  SUSOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 300 CHARACTERS.
       01  CABS-SUSOUT-RECORD              PIC X(300).
      * CTLOUT - CATALOGUED GENERATION DATA GROUP.
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD              PIC X(180).
       WORKING-STORAGE SECTION.
      * STANDARD SHARED WORKING STORAGE. SEE CABSWRK.
       COPY CABSWRK.
      * SHARED LAYOUT PULLED IN FOR THE ROW SIDE.
       COPY CABSCOMM.
      * SHARED LAYOUT PULLED IN FOR THE OVERRIDE SIDE.
       COPY CABSRATE.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABURT11'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V2.16'.
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
           05  WS-DJ-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DJ-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DJ-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DJ-CNT-04                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-DJ-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DJ-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DJ-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DJ-QTY-04                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-DJ-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DJ-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-DJ-TXT-01                PIC X(26) VALUE SPACES.
           05  WS-DJ-TXT-02                PIC X(08) VALUE SPACES.
           05  WS-DJ-TXT-03                PIC X(08) VALUE SPACES.
           05  WS-DJ-TXT-04                PIC X(16) VALUE SPACES.
           05  WS-DJ-TXT-05                PIC X(30) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-DJ-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-DJ-ON-01                 VALUE 'Y'.
               88  WS-DJ-OFF-01                VALUE 'N'.
           05  WS-DJ-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-DJ-ON-02                 VALUE 'Y'.
               88  WS-DJ-OFF-02                VALUE 'N'.
           05  WS-DJ-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-DJ-ON-03                 VALUE 'Y'.
               88  WS-DJ-OFF-03                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-DJ-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DJ-SUB-02                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. LOADED ONCE AT INITIALISATION AND SEARCHED WITH
      * A SUBSCRIPTED WALK.
       01  WS-DJ-TABLE.
           05  WS-DJ-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-DJ-TB-ENTRY OCCURS 150 TIMES
                                       INDEXED BY WS-DJ-IX.
               10  WS-DJ-TB-KEY                PIC X(08).
               10  WS-DJ-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-DJ-TB-TXT                PIC X(40).
               10  WS-DJ-TB-EFF                PIC 9(05).
               10  WS-DJ-TB-EXP                PIC 9(05).
       01  WS-DJ-WORK-GROUP-1.
           05  WS-DJ-G1-CLASS              PIC S9(09) COMP-3.
           05  WS-DJ-G1-SEQ                PIC S9(09) COMP-3.
           05  WS-DJ-G1-SEGMENT            PIC X(20).
           05  WS-DJ-G1-INVOICE            PIC 9(05).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 50.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABURT11 - RATE ELEMENT DESCRIPTION MAINTENANCE'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-DJ-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-DJ-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9984.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-DJ-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-DJ-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT OVRIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON OVRIN - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT BNDOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON BNDOUT - CHECK THE ALLOCATION' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-DJ-CYCLE-YYDDD.
           COMPUTE WS-DJ-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-DJ-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-DJ-CNT-03.
           MOVE 0 TO WS-DJ-CNT-04.
           MOVE 0 TO WS-DJ-CNT-01.
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
           PERFORM P2200-SPLIT-KEY THRU P2200-SPLIT-KEY-EXIT.
           PERFORM P2300-SPLIT-WINDOW THRU P2300-SPLIT-WINDOW-EXIT.
           PERFORM P2400-SELECT-KEY THRU P2400-SELECT-KEY-EXIT.
           IF WS-DJ-ON-03
               PERFORM P2500-DERIVE-ELEMENT THRU
                   P2500-DERIVE-ELEMENT-EXIT.
           PERFORM P2600-SPLIT-KEY THRU P2600-SPLIT-KEY-EXIT.
           IF WS-DJ-ON-02
               PERFORM P2700-APPLY-TARIFF THRU P2700-APPLY-TARIFF-EXIT.
           IF WS-DJ-ON-01
               PERFORM P2800-CONVERT-ELEMENT THRU
                   P2800-CONVERT-ELEMENT-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ OVRIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P2200-SPLIT-KEY.
           MOVE ID-STATUS TO WS-DJ-TXT-05.
           MOVE ID-SEGMENT TO WS-DJ-TXT-05.
           ADD 1 TO WS-DJ-CNT-03.
       P2200-SPLIT-KEY-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P2300-SPLIT-WINDOW.
           CALL 'CABHASH' USING ID-STATUS WS-ACC-OCN-HASH.
           ADD WS-DJ-CNT-03 TO WS-ACC-SEQ-HASH.
       P2300-SPLIT-WINDOW-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P2400-SELECT-KEY.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-OCN-UNKNOWN TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-DJ-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
       P2400-SELECT-KEY-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P2500-DERIVE-ELEMENT.
           MOVE 0 TO WS-DJ-QTY-03.
           MOVE 0 TO WS-DJ-QTY-01.
           MOVE 0 TO WS-DJ-QTY-04.
           MOVE 0 TO WS-DJ-AMT-02.
       P2500-DERIVE-ELEMENT-EXIT.
           EXIT.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P2600-SPLIT-KEY.
           MOVE SPACES TO CABS-DJ-OUT-RECORD.
           MOVE ID-CYCLE TO OD-BAND.
           MOVE ID-INVOICE TO OD-REGION.
           MOVE ID-ELEM TO OD-SOURCE.
           MOVE ID-GROUP TO OD-REGION2.
           MOVE ID-GROUP TO OD-BAND2.
           MOVE ID-SEGMENT TO OD-CARRIER.
           MOVE ID-LEVEL TO OD-BAND3.
           MOVE ID-INVOICE2 TO OD-INVOICE.
           WRITE CABS-DJ-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P2600-SPLIT-KEY-EXIT.
           EXIT.
       P2700-APPLY-TARIFF.
           IF ID-ACCOUNT = 'E'
               ADD 1 TO WS-DJ-CNT-04
           ELSE
               IF ID-ACCOUNT = 'E'
                   ADD 1 TO WS-DJ-CNT-01
               ELSE
                   IF ID-ACCOUNT = 'C'
                       ADD 1 TO WS-DJ-CNT-04
                   ELSE
                       ADD 1 TO WS-DJ-CNT-03.
       P2700-APPLY-TARIFF-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P2800-CONVERT-ELEMENT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-DJ-TXT-05 TO PC-COL-001-020.
           MOVE WS-DJ-TXT-01 TO PC-COL-021-060.
           MOVE WS-DJ-AMT-01 TO WS-DJ-AMT-EDIT.
           MOVE WS-DJ-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P2800-CONVERT-ELEMENT-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-WRITE-OVERRIDE.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-OCN-UNKNOWN TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-DJ-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
           MOVE CABS-SUSPENSE-RECORD TO CABS-SUSOUT-RECORD.
           WRITE CABS-SUSOUT-RECORD.
       P3100-WRITE-OVERRIDE-EXIT.
           EXIT.
       P3200-POST-BAND.
           MOVE SPACES TO WS-DJ-TXT-03.
           STRING ID-LEVEL DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               ID-ACCOUNT DELIMITED BY SIZE
               INTO WS-DJ-TXT-03.
       P3200-POST-BAND-EXIT.
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
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE 4 TO CT-STEP-SEQ.
           MOVE WS-DJ-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE WS-DJ-TXT-01 TO CT-RESTART-KEY.
           MOVE SPACES TO CT-ABEND-CD.
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
           CLOSE OVRIN.
           CLOSE BNDOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABURT11 - NORMAL END OF JOB'.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  DJ-CNT-04 = ' WS-DJ-CNT-04.
           DISPLAY '  DJ-CNT-03 = ' WS-DJ-CNT-03.
       P9000-EXIT.
           EXIT.
