      *****************************************************************
      * CABUCV11 - LEGACY LAYOUT DOWN CONVERSION                      *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               SRCIN   TELCABS.CABS.SRCIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               TGTOUT  TELCABS.CABS.TGTOUT         (LOCAL)     *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - THE OUTPUT IS REBUILT EACH RUN     *
      * REVISION HISTORY                                              *
      *   V1.00  1988-02-09  M.DELACROIX  INITIAL RELEASE             *
      *   V1.03  1998-01-06  W.J.MCALLISTER CENTURY PIVOT APPLIED TO  *
      *                      THE CYCLE DATE                           *
      *   V1.06  2010-01-28  L.FERREIRA   CONTROL RECORD ADDED PER    *
      *                      CABS-STD-002                             *
      *   V1.10  2018-02-10  R.T.WHEELER  RECOMPILE ONLY - COPYBOOK   *
      *                      CHANGE UPSTREAM                          *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUCV11.
       AUTHOR. TELCABS APPLICATIONS - FILE CONVERSION.
      *****************************************************************
      * LEGACY LAYOUT DOWN CONVERSION. THIS STEP IS SCHEDULED INSIDE  *
      * THE NIGHTLY ACCESS BILLING STREAM AND HAS NO INTERACTIVE ENTRY*
      * POINT.                                                        *
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
           SELECT SRCIN ASSIGN TO UT-S-SRCIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT TGTOUT ASSIGN TO UT-S-TGTOUT
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
      * SRCIN - CATALOGUED GENERATION DATA GROUP.
       FD  SRCIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-DY-IN-RECORD.
           05  ID-CARRIER                  PIC S9(07) COMP-3.
           05  ID-BAN                      PIC X(13).
           05  ID-TARIFF                   PIC S9(15) COMP-3.
           05  ID-TYPE                     PIC X(06).
           05  ID-SOURCE                   PIC 9(06).
           05  ID-SEGMENT                  PIC X(16).
           05  ID-CARRIER2                 PIC S9(09)V9(02) COMP-3.
           05  ID-SEQ                      PIC 9(07).
           05  DY-FILL-01                  PIC X(14).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-DY-VIEW1 REDEFINES CABS-DY-IN-RECORD.
           05  R0D-GROUP                   PIC S9(07)V9(05) COMP-3.
           05  R0D-SOURCE                  PIC S9(09)V9(02) COMP-3.
           05  R0D-STATUS                  PIC 9(04).
           05  R0D-BAN                     PIC 9(02).
           05  R0D-JURIS                   PIC X(06).
           05  R0D-CIRCUIT                 PIC X(03).
           05  R0D-ELEM                    PIC X(08).
           05  R0D-REST                    PIC X(44).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-DY-VIEW2 REDEFINES CABS-DY-IN-RECORD.
           05  R1D-ACCOUNT                 PIC X(06).
           05  R1D-BAN                     PIC 9(09).
           05  R1D-BAND                    PIC X(06).
           05  R1D-SOURCE                  PIC X(03).
           05  R1D-REST                    PIC X(56).
      * TGTOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  TGTOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 110 CHARACTERS.
       01  CABS-DY-OUT-RECORD.
           05  OD-TYPE                     PIC S9(05) COMP-3.
           05  OD-CYCLE                    PIC S9(07) COMP-3.
           05  OD-SEGMENT                  PIC 9(06).
           05  OD-ELEM                     PIC X(03).
           05  OD-REGION                   PIC X(10).
           05  OD-GROUP                    PIC X(20).
           05  OD-SEGMENT2                 PIC S9(13) COMP-3.
           05  OD-CARRIER                  PIC S9(11) COMP-3.
           05  OD-TARGET                   PIC 9(03).
           05  OD-TYPE2                    PIC X(16).
           05  OD-STATE                    PIC X(06).
           05  OD-GROUP2                   PIC X(20).
           05  DY-FILL-02                  PIC X(6).
      * SUSOUT - CATALOGUED GENERATION DATA GROUP.
       FD  SUSOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 300 CHARACTERS.
       01  CABS-SUSOUT-RECORD              PIC X(300).
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
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUCV11'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V3.16'.
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
           05  WS-DY-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DY-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DY-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DY-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DY-CNT-05                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-DY-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DY-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DY-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DY-QTY-04                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-DY-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DY-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DY-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DY-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-DY-TXT-01                PIC X(12) VALUE SPACES.
           05  WS-DY-TXT-02                PIC X(16) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-DY-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-DY-ON-01                 VALUE 'Y'.
               88  WS-DY-OFF-01                VALUE 'N'.
           05  WS-DY-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-DY-ON-02                 VALUE 'Y'.
               88  WS-DY-OFF-02                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-DY-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DY-SUB-02                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. A ROW THAT DOES NOT FIT IS COUNTED AND DROPPED
      * - THE STEP DOES NOT FAIL.
       01  WS-DY-TABLE.
           05  WS-DY-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-DY-TB-ENTRY OCCURS 150 TIMES
                                       INDEXED BY WS-DY-IX.
               10  WS-DY-TB-KEY                PIC X(13).
               10  WS-DY-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-DY-TB-TXT                PIC X(30).
               10  WS-DY-TB-EFF                PIC 9(05).
               10  WS-DY-TB-EXP                PIC 9(05).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 60.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUCV11 - LEGACY LAYOUT DOWN CONVERSION'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-DY-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-DY-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
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
           05  WS-DY-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-DY-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT SRCIN.
           IF WS-FS-INPUT NOT = '00'
               DISPLAY 'SRCIN FILE STATUS = ' WS-FS-INPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'SRCIN OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT TGTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               DISPLAY 'TGTOUT FILE STATUS = ' WS-FS-OUTPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'TGTOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SUSOUT.
           IF WS-FS-SUSPENSE NOT = '00'
               DISPLAY 'SUSOUT FILE STATUS = ' WS-FS-SUSPENSE
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'SUSOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               DISPLAY 'CTLOUT FILE STATUS = ' WS-FS-CONTROL
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
           MOVE PC1-CYCLE-YYDDD TO WS-DY-CYCLE-YYDDD.
           COMPUTE WS-DY-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-DY-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-DY-CNT-03.
           MOVE 0 TO WS-DY-CNT-05.
           MOVE 0 TO WS-DY-CNT-01.
       P1200-EXIT.
           EXIT.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-DY-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-DY-TAB-CNT NOT < 150
               MOVE 'Y' TO WS-DY-SW-01
               ADD 1 TO WS-DY-CNT-04
           ELSE
               ADD 1 TO WS-DY-TAB-CNT
               SET WS-DY-IX TO WS-DY-TAB-CNT
               MOVE ID-SEQ TO WS-DY-TB-KEY (WS-DY-IX)
               MOVE 0 TO WS-DY-TB-VAL (WS-DY-IX)
               MOVE SPACES TO WS-DY-TB-TXT (WS-DY-IX)
               ADD 1 TO WS-DY-CNT-01.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ SRCIN
               AT END MOVE 'Y' TO WS-DY-SW-01.
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
           IF WS-DY-ON-02
               PERFORM P2200-EDIT-PACKED THRU P2200-EDIT-PACKED-EXIT.
           PERFORM P2300-EDIT-CENTURY THRU P2300-EDIT-CENTURY-EXIT.
           PERFORM P2400-EDIT-CENTURY THRU P2400-EDIT-CENTURY-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ SRCIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P2200-EDIT-PACKED.
           UNSTRING WS-DY-TXT-01 DELIMITED BY '/'
               INTO WS-DY-TXT-01
               WS-DY-TXT-01
               TALLYING IN WS-DY-CNT-05.
           MOVE 'N' TO WS-DY-SW-01.
           IF WS-DY-TAB-CNT > 0
               PERFORM P250-COMPARE-LAYOUT THRU P250-COMPARE-LAYOUT-EXIT
               VARYING WS-DY-SUB-01 FROM 1 BY 1
               UNTIL WS-DY-SUB-01 > WS-DY-TAB-CNT
               OR WS-DY-SW-01 = 'Y'.
       P2200-EDIT-PACKED-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P2300-EDIT-CENTURY.
           CALL 'CABCTLWR' USING WS-DY-TXT-02 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-DY-CNT-05.
       P2300-EDIT-CENTURY-EXIT.
           EXIT.
       P2400-EDIT-CENTURY.
           IF WS-DY-AMT-02 NOT = 0
               COMPUTE WS-DY-QTY-03 = WS-DY-AMT-04 * 100 / WS-DY-AMT-02
           ELSE
               MOVE 0 TO WS-DY-QTY-03.
       P2400-EDIT-CENTURY-EXIT.
           EXIT.
       P250-COMPARE-LAYOUT.
           SET WS-DY-IX TO WS-DY-SUB-02.
           IF WS-DY-TB-KEY (WS-DY-IX) = ID-SEQ
               MOVE 'Y' TO WS-DY-SW-01
               MOVE WS-DY-TB-VAL (WS-DY-IX) TO WS-DY-QTY-01
               MOVE WS-DY-SUB-02 TO WS-DY-SUB-02.
       P250-COMPARE-LAYOUT-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-FORMAT-PACKED.
           MOVE ID-BAN TO WS-DY-TXT-01.
           MOVE ID-SEQ TO WS-DY-TXT-01.
           MOVE ID-SEGMENT TO WS-DY-TXT-02.
           MOVE ID-SEGMENT TO WS-DY-TXT-02.
           ADD 1 TO WS-DY-CNT-05.
       P3100-FORMAT-PACKED-EXIT.
           EXIT.
       P3200-RELEASE-FIELD.
           CALL 'CABHASH' USING ID-TYPE WS-ACC-OCN-HASH.
           ADD WS-DY-CNT-02 TO WS-ACC-SEQ-HASH.
       P3200-RELEASE-FIELD-EXIT.
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
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE WS-DY-CNT-01 TO CT-RC.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE SPACES TO CT-FILLER.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE WS-DY-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE 5 TO CT-STEP-SEQ.
       P8100-EXIT.
           EXIT.
      * P8200-CHECK-BALANCE - A RECORD THAT WAS HELD FOR THE NEXT
      * CYCLE IS BROUGHT INTO THE CARRIED FORWARD SIDE BEFORE THE
      * EQUATION IS TESTED.
       P8200-CHECK-BALANCE.
           ADD WS-DY-CNT-01 TO CT-CARRIED-FWD.
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
           CLOSE SRCIN.
           CLOSE TGTOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABUCV11 - RUN COMPLETE'.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  DY-CNT-02 = ' WS-DY-CNT-02.
           DISPLAY '  DY-CNT-03 = ' WS-DY-CNT-03.
           DISPLAY '  DY-CNT-01 = ' WS-DY-CNT-01.
           DISPLAY '  DY-CNT-04 = ' WS-DY-CNT-04.
       P9000-EXIT.
           EXIT.
