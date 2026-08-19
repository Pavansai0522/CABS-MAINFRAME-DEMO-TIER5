      *****************************************************************
      * CABURT08 - RATE TABLE EXPIRY SWEEP                            *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               MNTIN   TELCABS.CABS.MNTIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               AUDOUT  TELCABS.CABS.AUDOUT         (LOCAL)     *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - THE OUTPUT IS REBUILT EACH RUN     *
      * REVISION HISTORY                                              *
      *   V1.00  1995-05-08  B.R.HALVORSEN INITIAL RELEASE            *
      *   V1.02  2010-10-03  T.YAMASHITA  RESTART KEY WRITTEN SO A    *
      *                      RERUN CAN POSITION                       *
      *   V1.06  2013-09-14  B.R.HALVORSEN PARM CARD EXTENDED,        *
      *                      POSITIONS 40 THROUGH 48                  *
      *   V1.09  2016-06-01  P.NAIR       BLOCK SIZE SET TO ZERO -    *
      *                      SYSTEM DETERMINED                        *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABURT08.
       AUTHOR. TELCABS APPLICATIONS - RATE TABLE MAINTENANCE.
      *****************************************************************
      * RATE TABLE EXPIRY SWEEP. THE STEP IS DRIVEN ENTIRELY FROM THE *
      * SYSIN PARM CARD AND THE DD ALLOCATIONS IN THE JOB.            *
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
           SELECT MNTIN ASSIGN TO UT-S-MNTIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT AUDOUT ASSIGN TO UT-S-AUDOUT
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
      * MNTIN - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  MNTIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-DK-IN-RECORD.
           05  ID-CLASS                    PIC S9(07)V9(02) COMP-3.
           05  ID-TARGET                   PIC X(10).
           05  ID-JURIS                    PIC X(20).
           05  ID-INVOICE                  PIC X(08).
           05  ID-OCN                      PIC 9(07).
           05  ID-CENTRE                   PIC X(02).
           05  ID-OCN2                     PIC 9(07).
           05  ID-TARIFF                   PIC S9(15) COMP-3.
           05  ID-TARGET2                  PIC X(06).
           05  DK-FILL-01                  PIC X(7).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-DK-VIEW1 REDEFINES CABS-DK-IN-RECORD.
           05  R0D-CARRIER                 PIC X(20).
           05  R0D-GROUP                   PIC X(02).
           05  R0D-SEQ                     PIC X(08).
           05  R0D-STATUS                  PIC X(10).
           05  R0D-REST                    PIC X(40).
      * AUDOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  AUDOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-DK-OUT-RECORD.
           05  OD-CIRCUIT                  PIC 9(06).
           05  OD-GROUP                    PIC X(20).
           05  OD-CODE                     PIC S9(05) COMP-3.
           05  OD-LEVEL                    PIC 9(04).
           05  OD-CYCLE                    PIC 9(06).
           05  OD-STATE                    PIC X(16).
           05  OD-ELEM                     PIC S9(13) COMP-3.
           05  OD-INVOICE                  PIC X(03).
           05  OD-CLASS                    PIC S9(09)V9(05) COMP-3.
           05  DK-FILL-02                  PIC X(7).
      * SUSOUT - WORK FILE, DELETED AT STEP END.
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
      * SHARED LAYOUT PULLED IN FOR THE DESCRIPTION SIDE.
       COPY CABSRATE.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABURT08'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V1.09'.
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
           05  WS-DK-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DK-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DK-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DK-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DK-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DK-CNT-06                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-DK-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DK-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DK-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DK-QTY-04                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-DK-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DK-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-DK-TXT-01                PIC X(16) VALUE SPACES.
           05  WS-DK-TXT-02                PIC X(26) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-DK-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-DK-ON-01                 VALUE 'Y'.
               88  WS-DK-OFF-01                VALUE 'N'.
           05  WS-DK-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-DK-ON-02                 VALUE 'Y'.
               88  WS-DK-OFF-02                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-DK-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DK-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DK-SUB-03                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. A ROW THAT DOES NOT FIT IS COUNTED AND DROPPED
      * - THE STEP DOES NOT FAIL.
       01  WS-DK-TABLE.
           05  WS-DK-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-DK-TB-ENTRY OCCURS 50 TIMES
                                       INDEXED BY WS-DK-IX.
               10  WS-DK-TB-KEY                PIC X(06).
               10  WS-DK-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-DK-TB-TXT                PIC X(30).
               10  WS-DK-TB-EFF                PIC 9(05).
               10  WS-DK-TB-EXP                PIC 9(05).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 58.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABURT08 - RATE TABLE EXPIRY SWEEP'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-DK-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-DK-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9950.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-DK-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-DK-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT MNTIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'MNTIN NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT AUDOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'AUDOUT NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SUSOUT.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'SUSOUT NOT AVAILABLE - OPEN REJECTED' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-DK-CYCLE-YYDDD.
           COMPUTE WS-DK-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-DK-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-DK-CNT-02.
           MOVE 0 TO WS-DK-CNT-04.
           MOVE 0 TO WS-DK-CNT-06.
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
           PERFORM P2200-APPLY-ELEMENT THRU P2200-APPLY-ELEMENT-EXIT.
           IF WS-DK-ON-01
               PERFORM P2300-SELECT-KEY THRU P2300-SELECT-KEY-EXIT.
           PERFORM P2400-EXPAND-DESCRIPTION THRU
               P2400-EXPAND-DESCRIPTION-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ MNTIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-APPLY-ELEMENT.
           ADD ID-CLASS TO WS-DK-QTY-04.
           COMPUTE WS-DK-AMT-02 = WS-DK-QTY-04 * WS-DK-QTY-03.
           ADD WS-DK-AMT-02 TO WS-DK-AMT-02.
       P2200-APPLY-ELEMENT-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P2300-SELECT-KEY.
           CALL 'CABSEQCK' USING WS-DK-TXT-02 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-DK-CNT-04.
       P2300-SELECT-KEY-EXIT.
           EXIT.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P2400-EXPAND-DESCRIPTION.
           MOVE ID-TARGET2 TO WS-DK-TXT-01.
           MOVE ID-TARIFF TO WS-DK-TXT-01.
           MOVE ID-CLASS TO WS-DK-TXT-01.
           ADD 1 TO WS-DK-CNT-05.
       P2400-EXPAND-DESCRIPTION-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P3100-CLOSE-OFF-BAND.
           MOVE SPACES TO WS-DK-TXT-02.
           STRING ID-OCN2 DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               ID-JURIS DELIMITED BY SIZE
               INTO WS-DK-TXT-02.
       P3100-CLOSE-OFF-BAND-EXIT.
           EXIT.
       P3200-FORMAT-ELEMENT.
           CALL 'CABHASH' USING ID-TARGET2 WS-ACC-OCN-HASH.
           ADD WS-DK-CNT-04 TO WS-ACC-SEQ-HASH.
       P3200-FORMAT-ELEMENT-EXIT.
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
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE 7 TO CT-STEP-SEQ.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE WS-DK-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE WS-DK-CNT-01 TO CT-RC.
           MOVE WS-DK-TXT-02 TO CT-RESTART-KEY.
           MOVE SPACES TO CT-FILLER.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
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
           CLOSE MNTIN.
           CLOSE AUDOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABURT08 - RUN COMPLETE'.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  DK-CNT-06 = ' WS-DK-CNT-06.
           DISPLAY '  DK-CNT-04 = ' WS-DK-CNT-04.
           DISPLAY '  DK-CNT-05 = ' WS-DK-CNT-05.
       P9000-EXIT.
           EXIT.
