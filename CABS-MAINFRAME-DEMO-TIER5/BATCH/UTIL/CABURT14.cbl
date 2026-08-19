      *****************************************************************
      * CABURT14 - RATE OVERRIDE TABLE LOAD                           *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               RATIN   TELCABS.CABS.RATIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               RATOUT  TELCABS.CABS.RATOUT         (LOCAL)     *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - THE OUTPUT IS REBUILT EACH RUN     *
      * REVISION HISTORY                                              *
      *   V1.00  1989-06-02  T.YAMASHITA  INITIAL RELEASE             *
      *   V1.04  2000-05-24  W.J.MCALLISTER PARM CARD EXTENDED,       *
      *                      POSITIONS 40 THROUGH 48                  *
      *   V1.06  2002-03-01  A.BUKOWSKI   OCCURS RAISED AFTER THE     *
      *                      FEBRUARY OVERFLOW                        *
      *   V1.08  2003-10-13  P.NAIR       REGION SIZE REDUCED - TABLE *
      *                      MOVED OUT OF WORKING STORAGE             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABURT14.
       AUTHOR. TELCABS APPLICATIONS - RATE TABLE MAINTENANCE.
      *****************************************************************
      * RATE OVERRIDE TABLE LOAD. THIS STEP IS SCHEDULED INSIDE THE   *
      * NIGHTLY ACCESS BILLING STREAM AND HAS NO INTERACTIVE ENTRY    *
      * POINT.                                                        *
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
           SELECT RATIN ASSIGN TO UT-S-RATIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT RATOUT ASSIGN TO UT-S-RATOUT
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
      * RATIN - WORK FILE, DELETED AT STEP END.
       FD  RATIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-AU-IN-RECORD.
           05  IA-TYPE                     PIC S9(09)V9(02) COMP-3.
           05  IA-CODE                     PIC S9(07) COMP-3.
           05  IA-REGION                   PIC S9(07)V9(02) COMP-3.
           05  IA-TARGET                   PIC S9(13)V9(02) COMP-3.
           05  IA-TARGET2                  PIC 9(02).
           05  IA-JURIS                    PIC X(08).
           05  IA-REGION2                  PIC S9(07) COMP-3.
           05  IA-JURIS2                   PIC S9(05) COMP-3.
           05  IA-SEQ                      PIC 9(05).
           05  AU-FILL-01                  PIC X(35).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-AU-VIEW1 REDEFINES CABS-AU-IN-RECORD.
           05  R0A-ELEM                    PIC S9(07)V9(05) COMP-3.
           05  R0A-SEGMENT                 PIC X(08).
           05  R0A-CENTRE                  PIC S9(13)V9(02) COMP-3.
           05  R0A-LEVEL                   PIC X(04).
           05  R0A-BAN                     PIC X(16).
           05  R0A-OCN                     PIC S9(07) COMP-3.
           05  R0A-REST                    PIC X(33).
      * RATOUT - CATALOGUED GENERATION DATA GROUP.
       FD  RATOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-AU-OUT-RECORD.
           05  OA-CLASS                    PIC X(20).
           05  OA-TYPE                     PIC 9(02).
           05  OA-ACCOUNT                  PIC 9(06).
           05  OA-CENTRE                   PIC S9(11)V9(02) COMP-3.
           05  OA-BAN                      PIC S9(11)V9(02) COMP-3.
           05  OA-INVOICE                  PIC S9(13) COMP-3.
           05  OA-ACCOUNT2                 PIC 9(09).
           05  AU-FILL-02                  PIC X(22).
      * SUSOUT - SEQUENTIAL, BLOCKED BY THE SYSTEM.
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
      * SHARED LAYOUT PULLED IN FOR THE OVERRIDE SIDE.
       COPY CABSRATE.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABURT14'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V3.16'.
           05  WS-TABLE-LIMIT              PIC S9(04) COMP-3
                                         VALUE 120.
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
           05  WS-AU-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AU-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AU-CNT-03                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-AU-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AU-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AU-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-AU-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AU-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AU-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AU-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-AU-TXT-01                PIC X(26) VALUE SPACES.
           05  WS-AU-TXT-02                PIC X(30) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-AU-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-AU-ON-01                 VALUE 'Y'.
               88  WS-AU-OFF-01                VALUE 'N'.
           05  WS-AU-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-AU-ON-02                 VALUE 'Y'.
               88  WS-AU-OFF-02                VALUE 'N'.
           05  WS-AU-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-AU-ON-03                 VALUE 'Y'.
               88  WS-AU-OFF-03                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-AU-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-AU-SUB-02                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. THE LIMIT WAS RAISED WHEN THE SOUTHEAST CENTRES
      * WERE MERGED AND HAS NOT BEEN REVIEWED SINCE.
       01  WS-AU-TABLE.
           05  WS-AU-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-AU-TB-ENTRY OCCURS 120 TIMES
                                       INDEXED BY WS-AU-IX.
               10  WS-AU-TB-KEY                PIC X(08).
               10  WS-AU-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-AU-TB-TXT                PIC X(20).
               10  WS-AU-TB-EFF                PIC 9(05).
               10  WS-AU-TB-EXP                PIC 9(05).
       01  WS-AU-WORK-GROUP-1.
           05  WS-AU-G1-OCN                PIC X(20).
           05  WS-AU-G1-TARIFF             PIC S9(09) COMP-3.
           05  WS-AU-G1-OCN                PIC X(20).
           05  WS-AU-G1-LEVEL              PIC 9(07).
           05  WS-AU-G1-ELEM               PIC S9(09) COMP-3.
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 58.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABURT14 - RATE OVERRIDE TABLE LOAD'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-AU-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-AU-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9964.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-AU-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-AU-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
               MOVE 'RATIN NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RATOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RATOUT NOT AVAILABLE - OPEN REJECTED' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-AU-CYCLE-YYDDD.
           COMPUTE WS-AU-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-AU-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-AU-CNT-01.
           MOVE 0 TO WS-AU-CNT-02.
           MOVE 0 TO WS-AU-CNT-03.
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
           PERFORM P2200-RESOLVE-ELEMENT THRU
               P2200-RESOLVE-ELEMENT-EXIT.
           PERFORM P2300-CHECK-ROW THRU P2300-CHECK-ROW-EXIT.
           PERFORM P2400-CONVERT-OVERRIDE THRU
               P2400-CONVERT-OVERRIDE-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ RATIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P2200-RESOLVE-ELEMENT.
           MOVE 0 TO WS-AU-QTY-01.
           MOVE 0 TO WS-AU-QTY-03.
           MOVE 0 TO WS-AU-AMT-03.
           MOVE 0 TO WS-AU-AMT-02.
       P2200-RESOLVE-ELEMENT-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P2300-CHECK-ROW.
           ADD IA-JURIS2 TO WS-AU-QTY-03.
           COMPUTE WS-AU-AMT-01 ROUNDED = WS-AU-QTY-03 * WS-AU-QTY-02.
           ADD WS-AU-AMT-01 TO WS-AU-AMT-02.
       P2300-CHECK-ROW-EXIT.
           EXIT.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P2400-CONVERT-OVERRIDE.
           MOVE 'Y' TO WS-AU-SW-03.
           IF IA-TARGET < 35
               MOVE 'N' TO WS-AU-SW-03
               ADD 1 TO WS-AU-CNT-03.
           IF IA-TARGET > 5942
               MOVE 'N' TO WS-AU-SW-03
               ADD 1 TO WS-AU-CNT-02.
       P2400-CONVERT-OVERRIDE-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P3100-RELEASE-OVERRIDE.
           MOVE SPACES TO CABS-AU-OUT-RECORD.
           MOVE IA-TARGET TO OA-CLASS.
           MOVE IA-TARGET2 TO OA-TYPE.
           MOVE IA-TARGET2 TO OA-ACCOUNT.
           MOVE IA-JURIS2 TO OA-CENTRE.
           MOVE IA-TARGET TO OA-BAN.
           WRITE CABS-AU-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P3100-RELEASE-OVERRIDE-EXIT.
           EXIT.
       P3200-STAGE-DESCRIPTION.
           MOVE IA-CODE TO WS-AU-TXT-02.
           MOVE IA-TARGET2 TO WS-AU-TXT-02.
           MOVE IA-TYPE TO WS-AU-TXT-02.
           MOVE IA-SEQ TO WS-AU-TXT-02.
           ADD 1 TO WS-AU-CNT-03.
       P3200-STAGE-DESCRIPTION-EXIT.
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
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE SPACES TO CT-FILLER.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE WS-AU-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE 9 TO CT-STEP-SEQ.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
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
           CLOSE RATIN.
           CLOSE RATOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABURT14 - RUN COMPLETE'.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  AU-CNT-03 = ' WS-AU-CNT-03.
           DISPLAY '  AU-CNT-01 = ' WS-AU-CNT-01.
           DISPLAY '  AU-CNT-02 = ' WS-AU-CNT-02.
       P9000-EXIT.
           EXIT.
