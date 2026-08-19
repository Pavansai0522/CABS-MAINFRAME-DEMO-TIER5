      *****************************************************************
      * CABUCV15 - LEGACY LAYOUT DOWN CONVERSION                      *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               LEGIN   TELCABS.CABS.LEGIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               DSPOUT  TELCABS.CABS.DSPOUT         (LOCAL)     *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * REVISION HISTORY                                              *
      *   V1.00  2001-05-26  B.R.HALVORSEN INITIAL RELEASE            *
      *   V1.01  2003-12-23  C.ADEYEMI    BLOCK SIZE SET TO ZERO -    *
      *                      SYSTEM DETERMINED                        *
      *   V1.04  2012-05-22  P.NAIR       JOB PARAMETER MADE MANDATORY*
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUCV15.
       AUTHOR. TELCABS APPLICATIONS - FILE CONVERSION.
      *****************************************************************
      * LEGACY LAYOUT DOWN CONVERSION. THIS STEP IS SCHEDULED INSIDE  *
      * THE NIGHTLY ACCESS BILLING STREAM AND HAS NO INTERACTIVE ENTRY*
      * POINT.                                                        *
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
           SELECT LEGIN ASSIGN TO UT-S-LEGIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT DSPOUT ASSIGN TO UT-S-DSPOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT CTLOUT ASSIGN TO UT-S-CTLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      * LEGIN - WORK FILE, DELETED AT STEP END.
       FD  LEGIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 90 CHARACTERS.
       01  CABS-AB-IN-RECORD.
           05  IA-STATE                    PIC S9(11)V9(02) COMP-3.
           05  IA-SEQ                      PIC S9(13)V9(02) COMP-3.
           05  IA-OCN                      PIC X(20).
           05  IA-BAND                     PIC S9(11)V9(05) COMP-3.
           05  IA-MEDIA                    PIC 9(05).
           05  IA-CIRCUIT                  PIC X(03).
           05  IA-LEVEL                    PIC X(06).
           05  IA-JURIS                    PIC S9(13)V9(02) COMP-3.
           05  IA-SEQ2                     PIC S9(07)V9(02) COMP-3.
           05  IA-CLASS                    PIC X(08).
           05  IA-STATUS                   PIC S9(09) COMP-3.
           05  AB-FILL-01                  PIC X(6).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-AB-VIEW1 REDEFINES CABS-AB-IN-RECORD.
           05  R0A-TARIFF                  PIC 9(06).
           05  R0A-ELEM                    PIC X(03).
           05  R0A-SEQ                     PIC S9(11) COMP-3.
           05  R0A-BAN                     PIC S9(07)V9(02) COMP-3.
           05  R0A-TYPE                    PIC X(03).
           05  R0A-STATUS                  PIC X(20).
           05  R0A-STATUS2                 PIC S9(09) COMP-3.
           05  R0A-LEVEL                   PIC X(16).
           05  R0A-REST                    PIC X(26).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-AB-VIEW2 REDEFINES CABS-AB-IN-RECORD.
           05  R1A-INVOICE                 PIC 9(05).
           05  R1A-CIRCUIT                 PIC S9(13) COMP-3.
           05  R1A-BAND                    PIC 9(02).
           05  R1A-CIRCUIT2                PIC S9(11) COMP-3.
           05  R1A-BAND2                   PIC X(08).
           05  R1A-INVOICE2                PIC X(02).
           05  R1A-JURIS                   PIC 9(07).
           05  R1A-ACCOUNT                 PIC 9(04).
           05  R1A-CARRIER                 PIC 9(04).
           05  R1A-REST                    PIC X(45).
      * DSPOUT - PERMANENT DATASET HELD ON DASD.
       FD  DSPOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 100 CHARACTERS.
       01  CABS-AB-OUT-RECORD.
           05  OA-ACCOUNT                  PIC S9(11)V9(02) COMP-3.
           05  OA-BAN                      PIC S9(09) COMP-3.
           05  OA-STATE                    PIC X(16).
           05  OA-INVOICE                  PIC X(20).
           05  OA-OCN                      PIC X(13).
           05  OA-STATE2                   PIC S9(15) COMP-3.
           05  OA-SOURCE                   PIC 9(02).
           05  OA-CIRCUIT                  PIC X(04).
           05  OA-STATUS                   PIC X(10).
           05  OA-CIRCUIT2                 PIC S9(13)V9(02) COMP-3.
           05  AB-FILL-02                  PIC X(7).
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
      * SHARED LAYOUT PULLED IN FOR THE SIGN SIDE.
       COPY CABSCDR.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUCV15'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V1.15'.
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
           05  WS-AB-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AB-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AB-CNT-03                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-AB-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AB-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AB-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-AB-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AB-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AB-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-AB-TXT-01                PIC X(30) VALUE SPACES.
           05  WS-AB-TXT-02                PIC X(30) VALUE SPACES.
           05  WS-AB-TXT-03                PIC X(20) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-AB-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-AB-ON-01                 VALUE 'Y'.
               88  WS-AB-OFF-01                VALUE 'N'.
           05  WS-AB-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-AB-ON-02                 VALUE 'Y'.
               88  WS-AB-OFF-02                VALUE 'N'.
           05  WS-AB-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-AB-ON-03                 VALUE 'Y'.
               88  WS-AB-OFF-03                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-AB-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-AB-SUB-02                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. A ROW THAT DOES NOT FIT IS COUNTED AND DROPPED
      * - THE STEP DOES NOT FAIL.
       01  WS-AB-TABLE.
           05  WS-AB-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-AB-TB-ENTRY OCCURS 150 TIMES
                                       INDEXED BY WS-AB-IX.
               10  WS-AB-TB-KEY                PIC X(10).
               10  WS-AB-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-AB-TB-TXT                PIC X(20).
               10  WS-AB-TB-EFF                PIC 9(05).
               10  WS-AB-TB-EXP                PIC 9(05).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 60.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUCV15 - LEGACY LAYOUT DOWN CONVERSION'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-AB-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-AB-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9928.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-AB-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-AB-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT LEGIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF LEGIN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT DSPOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF DSPOUT' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF CTLOUT' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-AB-CYCLE-YYDDD.
           COMPUTE WS-AB-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-AB-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-AB-CNT-02.
           MOVE 0 TO WS-AB-CNT-03.
           MOVE 0 TO WS-AB-CNT-01.
       P1200-EXIT.
           EXIT.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-AB-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-AB-TAB-CNT NOT < 150
               MOVE 'Y' TO WS-AB-SW-01
               ADD 1 TO WS-AB-CNT-02
           ELSE
               ADD 1 TO WS-AB-TAB-CNT
               SET WS-AB-IX TO WS-AB-TAB-CNT
               MOVE IA-MEDIA TO WS-AB-TB-KEY (WS-AB-IX)
               MOVE 0 TO WS-AB-TB-VAL (WS-AB-IX)
               MOVE SPACES TO WS-AB-TB-TXT (WS-AB-IX)
               ADD 1 TO WS-AB-CNT-01.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ LEGIN
               AT END MOVE 'Y' TO WS-AB-SW-01.
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
           PERFORM P2200-MATCH-RECORD THRU P2200-MATCH-RECORD-EXIT.
           IF WS-AB-ON-01
               PERFORM P2300-MATCH-SIGN THRU P2300-MATCH-SIGN-EXIT.
           PERFORM P2400-EDIT-SIGN THRU P2400-EDIT-SIGN-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ LEGIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-MATCH-RECORD.
           UNSTRING WS-AB-TXT-02 DELIMITED BY '/'
               INTO WS-AB-TXT-03
               WS-AB-TXT-02
               TALLYING IN WS-AB-CNT-01.
           MOVE 'N' TO WS-AB-SW-02.
           IF WS-AB-TAB-CNT > 0
               PERFORM P250-COMPARE-ZONE THRU P250-COMPARE-ZONE-EXIT
               VARYING WS-AB-SUB-02 FROM 1 BY 1
               UNTIL WS-AB-SUB-02 > WS-AB-TAB-CNT
               OR WS-AB-SW-02 = 'Y'.
       P2200-MATCH-RECORD-EXIT.
           EXIT.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P2300-MATCH-SIGN.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-BAN-UNKNOWN TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-AB-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
       P2300-MATCH-SIGN-EXIT.
           EXIT.
       P2400-EDIT-SIGN.
           ADD IA-BAND TO WS-AB-QTY-01.
           COMPUTE WS-AB-AMT-01 = WS-AB-QTY-01 * WS-AB-QTY-01.
           ADD WS-AB-AMT-01 TO WS-AB-AMT-03.
       P2400-EDIT-SIGN-EXIT.
           EXIT.
       P250-COMPARE-ZONE.
           SET WS-AB-IX TO WS-AB-SUB-01.
           IF WS-AB-TB-KEY (WS-AB-IX) = IA-SEQ2
               MOVE 'Y' TO WS-AB-SW-02
               MOVE WS-AB-TB-VAL (WS-AB-IX) TO WS-AB-QTY-01
               MOVE WS-AB-SUB-01 TO WS-AB-SUB-01.
       P250-COMPARE-ZONE-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-CLOSE-OFF-FIELD.
           CALL 'CABHASH' USING IA-SEQ WS-ACC-OCN-HASH.
           ADD WS-AB-CNT-03 TO WS-ACC-SEQ-HASH.
       P3100-CLOSE-OFF-FIELD-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P3200-WRITE-ZONE.
           ADD IA-JURIS TO WS-AB-QTY-03.
           COMPUTE WS-AB-AMT-01 ROUNDED = WS-AB-QTY-03 * WS-AB-QTY-01.
           ADD WS-AB-AMT-01 TO WS-AB-AMT-02.
       P3200-WRITE-ZONE-EXIT.
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
           MOVE WS-AB-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE 7 TO CT-STEP-SEQ.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE SPACES TO CT-FILLER.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-READ-CNT TO CT-READ.
       P8100-EXIT.
           EXIT.
      * P8200-CHECK-BALANCE - A RECORD THAT WAS HELD FOR THE NEXT
      * CYCLE IS BROUGHT INTO THE CARRIED FORWARD SIDE BEFORE THE
      * EQUATION IS TESTED.
       P8200-CHECK-BALANCE.
           ADD WS-AB-CNT-02 TO CT-CARRIED-FWD.
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
           CLOSE LEGIN.
           CLOSE DSPOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABUCV15 - NORMAL END OF JOB'.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  AB-CNT-01 = ' WS-AB-CNT-01.
           DISPLAY '  AB-CNT-03 = ' WS-AB-CNT-03.
           DISPLAY '  AB-CNT-02 = ' WS-AB-CNT-02.
       P9000-EXIT.
           EXIT.
