      *****************************************************************
      * CABUCV17 - LEGACY LAYOUT DOWN CONVERSION                      *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               EMIIN   TELCABS.CABS.EMIIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               DSPOUT  TELCABS.CABS.DSPOUT         (LOCAL)     *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * REVISION HISTORY                                              *
      *   V1.00  1988-03-21  C.ADEYEMI    INITIAL RELEASE             *
      *   V1.03  1993-08-09  B.R.HALVORSEN HASH TOTAL ADDED TO THE    *
      *                      CONTROL RECORD                           *
      *   V1.05  2005-02-24  P.NAIR       JOB PARAMETER MADE MANDATORY*
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUCV17.
       AUTHOR. TELCABS APPLICATIONS - FILE CONVERSION.
      *****************************************************************
      * LEGACY LAYOUT DOWN CONVERSION. THE STEP IS DRIVEN ENTIRELY    *
      * FROM THE SYSIN PARM CARD AND THE DD ALLOCATIONS IN THE JOB.   *
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
           SELECT EMIIN ASSIGN TO UT-S-EMIIN
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
      * EMIIN - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  EMIIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-EG-IN-RECORD.
           05  IE-LEVEL                    PIC 9(07).
           05  IE-CARRIER                  PIC S9(07)V9(02) COMP-3.
           05  IE-ACCOUNT                  PIC X(06).
           05  IE-BAND                     PIC X(03).
           05  IE-ELEM                     PIC S9(09)V9(02) COMP-3.
           05  IE-CYCLE                    PIC S9(07) COMP-3.
           05  IE-CODE                     PIC S9(11)V9(02) COMP-3.
           05  IE-CYCLE2                   PIC S9(15) COMP-3.
           05  IE-ELEM2                    PIC X(20).
           05  EG-FILL-01                  PIC X(14).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-EG-VIEW1 REDEFINES CABS-EG-IN-RECORD.
           05  R0E-CLASS                   PIC S9(05) COMP-3.
           05  R0E-REGION                  PIC X(20).
           05  R0E-STATE                   PIC X(16).
           05  R0E-TYPE                    PIC X(03).
           05  R0E-REST                    PIC X(38).
      * DSPOUT - WORK FILE, DELETED AT STEP END.
       FD  DSPOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-EG-OUT-RECORD.
           05  OE-PERIOD                   PIC S9(15) COMP-3.
           05  OE-CODE                     PIC X(16).
           05  OE-ELEM                     PIC S9(13)V9(02) COMP-3.
           05  OE-REGION                   PIC X(10).
           05  OE-ACCOUNT                  PIC S9(13)V9(02) COMP-3.
           05  OE-TARIFF                   PIC S9(09) COMP-3.
           05  OE-CYCLE                    PIC 9(05).
           05  OE-PERIOD2                  PIC S9(11)V9(02) COMP-3.
           05  EG-FILL-02                  PIC X(13).
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
      * SHARED LAYOUT PULLED IN FOR THE PACKED SIDE.
       COPY CABSSETL.
      * SHARED LAYOUT PULLED IN FOR THE ZONE SIDE.
       COPY CABSCDR.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUCV17'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V1.15'.
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
       01  WS-COUNT-AREA.
           05  WS-EG-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-EG-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-EG-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-EG-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-EG-CNT-05                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-EG-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-EG-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-EG-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-EG-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-EG-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-EG-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-EG-TXT-01                PIC X(10) VALUE SPACES.
           05  WS-EG-TXT-02                PIC X(26) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-EG-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-EG-ON-01                 VALUE 'Y'.
               88  WS-EG-OFF-01                VALUE 'N'.
           05  WS-EG-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-EG-ON-02                 VALUE 'Y'.
               88  WS-EG-OFF-02                VALUE 'N'.
           05  WS-EG-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-EG-ON-03                 VALUE 'Y'.
               88  WS-EG-OFF-03                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-EG-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-EG-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-EG-SUB-03                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. A ROW THAT DOES NOT FIT IS COUNTED AND DROPPED
      * - THE STEP DOES NOT FAIL.
       01  WS-EG-TABLE.
           05  WS-EG-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-EG-TB-ENTRY OCCURS 120 TIMES
                                       INDEXED BY WS-EG-IX.
               10  WS-EG-TB-KEY                PIC X(04).
               10  WS-EG-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-EG-TB-TXT                PIC X(30).
               10  WS-EG-TB-EFF                PIC 9(05).
               10  WS-EG-TB-EXP                PIC 9(05).
       01  WS-EG-WORK-GROUP-1.
           05  WS-EG-G1-SEQ                PIC 9(05).
           05  WS-EG-G1-TYPE               PIC X(20).
           05  WS-EG-G1-CLASS              PIC 9(05).
           05  WS-EG-G1-STATE              PIC S9(11)V9(02) COMP-3.
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 50.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUCV17 - LEGACY LAYOUT DOWN CONVERSION'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-EG-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-EG-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9921.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-EG-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-EG-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT EMIIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'EMIIN NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT DSPOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'DSPOUT NOT AVAILABLE - OPEN REJECTED' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-EG-CYCLE-YYDDD.
           COMPUTE WS-EG-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-EG-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-EG-CNT-03.
           MOVE 0 TO WS-EG-CNT-04.
           MOVE 0 TO WS-EG-CNT-01.
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
           PERFORM P2200-VALIDATE-CENTURY THRU
               P2200-VALIDATE-CENTURY-EXIT.
           IF WS-EG-ON-01
               PERFORM P2300-EXPAND-PACKED THRU
                   P2300-EXPAND-PACKED-EXIT.
           PERFORM P2400-EDIT-RECORD THRU P2400-EDIT-RECORD-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ EMIIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P2200-VALIDATE-CENTURY.
           IF WS-EG-AMT-01 < 23
               MOVE 23 TO WS-EG-AMT-01
               ADD 1 TO WS-EG-CNT-02.
           IF WS-EG-AMT-01 > 83484
               MOVE 83484 TO WS-EG-AMT-01
               ADD 1 TO WS-EG-CNT-01.
       P2200-VALIDATE-CENTURY-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P2300-EXPAND-PACKED.
           ADD IE-CARRIER TO WS-EG-QTY-02.
           COMPUTE WS-EG-AMT-03 ROUNDED = WS-EG-QTY-02 * WS-EG-QTY-01.
           ADD WS-EG-AMT-03 TO WS-EG-AMT-03.
       P2300-EXPAND-PACKED-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P2400-EDIT-RECORD.
           MOVE 0 TO WS-EG-QTY-03.
           MOVE 0 TO WS-EG-QTY-01.
           MOVE 0 TO WS-EG-AMT-01.
           MOVE 0 TO WS-EG-AMT-02.
       P2400-EDIT-RECORD-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-CLOSE-OFF-CENTURY.
           MOVE IE-CODE TO WS-EG-TXT-01.
           MOVE IE-ELEM2 TO WS-EG-TXT-01.
           ADD 1 TO WS-EG-CNT-01.
       P3100-CLOSE-OFF-CENTURY-EXIT.
           EXIT.
      * A FAILURE HERE SUSPENDS THE RECORD, IT DOES NOT STOP THE STEP.
       P3200-RELEASE-PACKED.
           MOVE SPACES TO WS-EG-TXT-01.
           STRING IE-CARRIER DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IE-ELEM DELIMITED BY SIZE
               INTO WS-EG-TXT-01.
       P3200-RELEASE-PACKED-EXIT.
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
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE 5 TO CT-STEP-SEQ.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE WS-EG-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE SPACES TO CT-FILLER.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-RESTART-KEY.
       P8100-EXIT.
           EXIT.
      * P8200-CHECK-BALANCE - A RECORD THAT WAS HELD FOR THE NEXT
      * CYCLE IS BROUGHT INTO THE CARRIED FORWARD SIDE BEFORE THE
      * EQUATION IS TESTED.
       P8200-CHECK-BALANCE.
           ADD WS-EG-CNT-05 TO CT-CARRIED-FWD.
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
           CLOSE EMIIN.
           CLOSE DSPOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABUCV17 - END OF RUN'.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  EG-CNT-05 = ' WS-EG-CNT-05.
           DISPLAY '  EG-CNT-01 = ' WS-EG-CNT-01.
           DISPLAY '  EG-CNT-03 = ' WS-EG-CNT-03.
           DISPLAY '  EG-CNT-02 = ' WS-EG-CNT-02.
       P9000-EXIT.
           EXIT.
