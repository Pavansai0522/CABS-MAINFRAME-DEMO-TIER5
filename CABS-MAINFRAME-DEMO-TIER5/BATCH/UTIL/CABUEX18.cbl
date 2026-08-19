      *****************************************************************
      * CABUEX18 - USAGE EXTRACT BY JURISDICTION                      *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               USGIN   TELCABS.CABS.USGIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               DROPOUT TELCABS.CABS.DROPOU         (LOCAL)     *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - NOTHING IS UPDATED IN PLACE        *
      * REVISION HISTORY                                              *
      *   V1.00  1988-10-11  L.FERREIRA   INITIAL RELEASE             *
      *   V1.01  1989-05-18  T.YAMASHITA  SUSPENSE WRITE ADDED -      *
      *                      RECORDS WERE BEING DROPPED               *
      *   V1.05  1995-11-15  S.MARCHETTI  ROUNDING RULE TAKEN FROM THE*
      *                      RATE ROW                                 *
      *   V1.06  2010-01-26  D.OKONKWO    PRINT LINE WIDENED TO 133   *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUEX18.
       AUTHOR. TELCABS APPLICATIONS - DATASET EXTRACT.
      *****************************************************************
      * USAGE EXTRACT BY JURISDICTION. THE STEP RUNS ONCE PER BILL    *
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
           SELECT USGIN ASSIGN TO UT-S-USGIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT DROPOUT ASSIGN TO UT-S-DROPOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT CTLOUT ASSIGN TO UT-S-CTLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      * USGIN - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  USGIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 100 CHARACTERS.
       01  CABS-DQ-IN-RECORD.
           05  ID-TARGET                   PIC S9(13)V9(02) COMP-3.
           05  ID-SOURCE                   PIC X(03).
           05  ID-CENTRE                   PIC S9(05) COMP-3.
           05  ID-CIRCUIT                  PIC 9(03).
           05  ID-STATUS                   PIC 9(03).
           05  ID-BAN                      PIC S9(13)V9(02) COMP-3.
           05  ID-STATUS2                  PIC 9(04).
           05  ID-GROUP                    PIC X(08).
           05  ID-TYPE                     PIC X(02).
           05  ID-CLASS                    PIC X(20).
           05  ID-TARGET2                  PIC X(10).
           05  ID-CENTRE2                  PIC X(06).
           05  ID-INVOICE                  PIC X(16).
           05  DQ-FILL-01                  PIC X(6).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-DQ-VIEW1 REDEFINES CABS-DQ-IN-RECORD.
           05  R0D-BAND                    PIC S9(07)V9(02) COMP-3.
           05  R0D-CLASS                   PIC X(08).
           05  R0D-TYPE                    PIC X(16).
           05  R0D-CYCLE                   PIC S9(05) COMP-3.
           05  R0D-GROUP                   PIC 9(06).
           05  R0D-BAND2                   PIC X(03).
           05  R0D-CODE                    PIC X(13).
           05  R0D-GROUP2                  PIC S9(11)V9(02) COMP-3.
           05  R0D-REST                    PIC X(39).
      * DROPOUT - WORK FILE, DELETED AT STEP END.
       FD  DROPOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-DQ-OUT-RECORD.
           05  OD-CIRCUIT                  PIC X(20).
           05  OD-SOURCE                   PIC S9(13)V9(05) COMP-3.
           05  OD-ACCOUNT                  PIC X(16).
           05  OD-LEVEL                    PIC 9(02).
           05  OD-CENTRE                   PIC S9(07)V9(02) COMP-3.
           05  OD-CIRCUIT2                 PIC S9(11) COMP-3.
           05  OD-CIRCUIT3                 PIC X(10).
           05  DQ-FILL-02                  PIC X(11).
      * CTLOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD              PIC X(180).
       WORKING-STORAGE SECTION.
      * STANDARD SHARED WORKING STORAGE. SEE CABSWRK.
       COPY CABSWRK.
      * SHARED LAYOUT PULLED IN FOR THE SUBSET SIDE.
       COPY CABSCARR.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUEX18'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V1.21'.
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
           05  WS-DQ-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DQ-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DQ-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DQ-CNT-04                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-DQ-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DQ-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DQ-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-DQ-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DQ-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DQ-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-DQ-TXT-01                PIC X(30) VALUE SPACES.
           05  WS-DQ-TXT-02                PIC X(16) VALUE SPACES.
           05  WS-DQ-TXT-03                PIC X(08) VALUE SPACES.
           05  WS-DQ-TXT-04                PIC X(16) VALUE SPACES.
           05  WS-DQ-TXT-05                PIC X(08) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-DQ-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-DQ-ON-01                 VALUE 'Y'.
               88  WS-DQ-OFF-01                VALUE 'N'.
           05  WS-DQ-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-DQ-ON-02                 VALUE 'Y'.
               88  WS-DQ-OFF-02                VALUE 'N'.
           05  WS-DQ-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-DQ-ON-03                 VALUE 'Y'.
               88  WS-DQ-OFF-03                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-DQ-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DQ-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DQ-SUB-03                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. A ROW THAT DOES NOT FIT IS COUNTED AND DROPPED
      * - THE STEP DOES NOT FAIL.
       01  WS-DQ-TABLE.
           05  WS-DQ-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-DQ-TB-ENTRY OCCURS 150 TIMES
                                       INDEXED BY WS-DQ-IX.
               10  WS-DQ-TB-KEY                PIC X(10).
               10  WS-DQ-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-DQ-TB-TXT                PIC X(40).
               10  WS-DQ-TB-EFF                PIC 9(05).
               10  WS-DQ-TB-EXP                PIC 9(05).
       01  WS-DQ-WORK-GROUP-1.
           05  WS-DQ-G1-CENTRE             PIC S9(09) COMP-3.
           05  WS-DQ-G1-CENTRE             PIC 9(05).
           05  WS-DQ-G1-TARIFF             PIC S9(11)V9(02) COMP-3.
           05  WS-DQ-G1-CODE               PIC X(10).
           05  WS-DQ-G1-BAND               PIC S9(09) COMP-3.
           05  WS-DQ-G1-CODE               PIC 9(05).
           05  WS-DQ-G1-CLASS              PIC 9(05).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 60.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUEX18 - USAGE EXTRACT BY JURISDICTION'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-DQ-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-DQ-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9969.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-DQ-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-DQ-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
               DISPLAY 'USGIN FILE STATUS = ' WS-FS-INPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON USGIN - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT DROPOUT.
           IF WS-FS-OUTPUT NOT = '00'
               DISPLAY 'DROPOUT FILE STATUS = ' WS-FS-OUTPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON DROPOUT - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               DISPLAY 'CTLOUT FILE STATUS = ' WS-FS-CONTROL
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
           MOVE PC1-CYCLE-YYDDD TO WS-DQ-CYCLE-YYDDD.
           COMPUTE WS-DQ-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-DQ-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-DQ-CNT-03.
           MOVE 0 TO WS-DQ-CNT-01.
           MOVE 0 TO WS-DQ-CNT-04.
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
           IF WS-DQ-ON-02
               PERFORM P2200-DERIVE-RANGE THRU P2200-DERIVE-RANGE-EXIT.
           PERFORM P2300-BUILD-CANDIDATE THRU
               P2300-BUILD-CANDIDATE-EXIT.
           IF WS-DQ-ON-01
               PERFORM P2400-MATCH-RANGE THRU P2400-MATCH-RANGE-EXIT.
           PERFORM P2500-EXPAND-RANGE THRU P2500-EXPAND-RANGE-EXIT.
           PERFORM P2600-SELECT-SUBSET THRU P2600-SELECT-SUBSET-EXIT.
           PERFORM P2700-CHECK-EXTRACT THRU P2700-CHECK-EXTRACT-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ USGIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-DERIVE-RANGE.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-DUP-SEQ TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-DQ-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
       P2200-DERIVE-RANGE-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P2300-BUILD-CANDIDATE.
           UNSTRING WS-DQ-TXT-01 DELIMITED BY '/'
               INTO WS-DQ-TXT-05
               WS-DQ-TXT-04
               TALLYING IN WS-DQ-CNT-01.
       P2300-BUILD-CANDIDATE-EXIT.
           EXIT.
       P2400-MATCH-RANGE.
           MOVE 0 TO WS-DQ-QTY-02.
           MOVE 0 TO WS-DQ-QTY-01.
           MOVE 0 TO WS-DQ-AMT-03.
           MOVE 0 TO WS-DQ-AMT-01.
       P2400-MATCH-RANGE-EXIT.
           EXIT.
       P2500-EXPAND-RANGE.
           IF ID-CLASS = 'E'
               ADD 1 TO WS-DQ-CNT-02
           ELSE
               IF ID-CLASS = 'S'
                   ADD 1 TO WS-DQ-CNT-04
               ELSE
                   IF ID-CLASS = 'X'
                       ADD 1 TO WS-DQ-CNT-03
                   ELSE
                       ADD 1 TO WS-DQ-CNT-02.
       P2500-EXPAND-RANGE-EXIT.
           EXIT.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P2600-SELECT-SUBSET.
           MOVE WS-DQ-AMT-03 TO WS-DQ-AMT-01.
           IF WS-DQ-AMT-01 < 0
               COMPUTE WS-DQ-AMT-01 = 0 - WS-DQ-AMT-03.
       P2600-SELECT-SUBSET-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P2700-CHECK-EXTRACT.
           MOVE SPACES TO CABS-DQ-OUT-RECORD.
           MOVE ID-SOURCE TO OD-CIRCUIT.
           MOVE ID-TARGET TO OD-SOURCE.
           MOVE ID-STATUS2 TO OD-ACCOUNT.
           MOVE ID-TYPE TO OD-LEVEL.
           MOVE ID-TARGET2 TO OD-CENTRE.
           MOVE ID-CLASS TO OD-CIRCUIT2.
           MOVE ID-STATUS2 TO OD-CIRCUIT3.
           WRITE CABS-DQ-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P2700-CHECK-EXTRACT-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-CLOSE-OFF-RANGE.
           MOVE ID-GROUP TO WS-DQ-TXT-01.
           MOVE ID-CIRCUIT TO WS-DQ-TXT-01.
           MOVE ID-BAN TO WS-DQ-TXT-03.
           MOVE ID-INVOICE TO WS-DQ-TXT-04.
           ADD 1 TO WS-DQ-CNT-02.
       P3100-CLOSE-OFF-RANGE-EXIT.
           EXIT.
       P3200-STAGE-SUBSET.
           ADD ID-CENTRE TO WS-DQ-QTY-02.
           COMPUTE WS-DQ-AMT-02 = WS-DQ-QTY-02 * WS-DQ-QTY-03.
           ADD WS-DQ-AMT-02 TO WS-DQ-AMT-01.
       P3200-STAGE-SUBSET-EXIT.
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
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE 1 TO CT-STEP-SEQ.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE WS-DQ-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE WS-DQ-TXT-02 TO CT-RESTART-KEY.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-FILLER.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
       P8100-EXIT.
           EXIT.
      * P8200-CHECK-BALANCE - EVERY RECORD READ IS EITHER WRITTEN,
      * REJECTED, SUMMARISED OR CARRIED FORWARD.
       P8200-CHECK-BALANCE.
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
           CLOSE DROPOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABUEX18 - END OF RUN'.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  DQ-CNT-02 = ' WS-DQ-CNT-02.
           DISPLAY '  DQ-CNT-03 = ' WS-DQ-CNT-03.
           DISPLAY '  DQ-CNT-04 = ' WS-DQ-CNT-04.
           DISPLAY '  DQ-CNT-01 = ' WS-DQ-CNT-01.
       P9000-EXIT.
           EXIT.
