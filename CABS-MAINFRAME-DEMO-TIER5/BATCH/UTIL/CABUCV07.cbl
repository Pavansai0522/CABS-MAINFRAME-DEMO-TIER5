      *****************************************************************
      * CABUCV07 - CODE PAGE AND SIGN CONVERSION                      *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               CNVIN   TELCABS.CABS.CNVIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               REJOUT  TELCABS.CABS.REJOUT         (LOCAL)     *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - NOTHING IS UPDATED IN PLACE        *
      * REVISION HISTORY                                              *
      *   V1.00  1994-05-23  B.R.HALVORSEN INITIAL RELEASE            *
      *   V1.02  1997-06-13  K.O.BRIEN    EFFECTIVE DATE FILTER ADDED *
      *                      PER AUDIT FINDING                        *
      *   V1.05  2003-09-05  M.DELACROIX  BLOCK SIZE SET TO ZERO -    *
      *                      SYSTEM DETERMINED                        *
      *   V1.08  2004-08-24  K.O.BRIEN    RETIRED THE SECOND SORT STEP*
      *                      - DONE IN PROGRAM                        *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUCV07.
       AUTHOR. TELCABS APPLICATIONS - FILE CONVERSION.
      *****************************************************************
      * CODE PAGE AND SIGN CONVERSION. THE STEP RUNS ONCE PER BILL    *
      * CYCLE AND IS RERUN FROM THE TOP IF IT FAILS.                  *
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
           SELECT CNVIN ASSIGN TO UT-S-CNVIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT REJOUT ASSIGN TO UT-S-REJOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT CTLOUT ASSIGN TO UT-S-CTLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      * CNVIN - CATALOGUED GENERATION DATA GROUP.
       FD  CNVIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 110 CHARACTERS.
       01  CABS-BR-IN-RECORD.
           05  IB-PERIOD                   PIC X(08).
           05  IB-CIRCUIT                  PIC X(13).
           05  IB-REGION                   PIC S9(11)V9(02) COMP-3.
           05  IB-CLASS                    PIC S9(07) COMP-3.
           05  IB-TYPE                     PIC 9(04).
           05  IB-REGION2                  PIC X(08).
           05  IB-BAN                      PIC X(16).
           05  IB-PERIOD2                  PIC S9(15) COMP-3.
           05  IB-TARIFF                   PIC X(20).
           05  IB-STATE                    PIC S9(07)V9(05) COMP-3.
           05  IB-REGION3                  PIC X(10).
           05  BR-FILL-01                  PIC X(5).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-BR-VIEW1 REDEFINES CABS-BR-IN-RECORD.
           05  R0B-CENTRE                  PIC S9(05) COMP-3.
           05  R0B-CENTRE2                 PIC S9(13)V9(05) COMP-3.
           05  R0B-STATUS                  PIC X(03).
           05  R0B-SOURCE                  PIC X(10).
           05  R0B-GROUP                   PIC X(03).
           05  R0B-ACCOUNT                 PIC S9(15) COMP-3.
           05  R0B-REST                    PIC X(73).
      * REJOUT - PERMANENT DATASET HELD ON DASD.
       FD  REJOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-BR-OUT-RECORD.
           05  OB-OCN                      PIC S9(13) COMP-3.
           05  OB-ELEM                     PIC X(04).
           05  OB-SOURCE                   PIC X(13).
           05  OB-ELEM2                    PIC X(04).
           05  OB-STATE                    PIC X(04).
           05  OB-INVOICE                  PIC S9(09) COMP-3.
           05  OB-MEDIA                    PIC X(13).
           05  OB-STATE2                   PIC X(13).
           05  OB-BAND                     PIC 9(05).
           05  OB-JURIS                    PIC S9(09) COMP-3.
           05  BR-FILL-02                  PIC X(7).
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
      * SHARED LAYOUT PULLED IN FOR THE SIGN SIDE.
       COPY CABSBILL.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUCV07'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V1.27'.
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
           05  WS-BR-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BR-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BR-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BR-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BR-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BR-CNT-06                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-BR-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BR-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-BR-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BR-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BR-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BR-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-BR-TXT-01                PIC X(20) VALUE SPACES.
           05  WS-BR-TXT-02                PIC X(26) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-BR-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-BR-ON-01                 VALUE 'Y'.
               88  WS-BR-OFF-01                VALUE 'N'.
           05  WS-BR-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-BR-ON-02                 VALUE 'Y'.
               88  WS-BR-OFF-02                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-BR-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-BR-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-BR-SUB-03                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. LOADED ONCE AT INITIALISATION AND SEARCHED WITH
      * A SUBSCRIPTED WALK.
       01  WS-BR-TABLE.
           05  WS-BR-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-BR-TB-ENTRY OCCURS 100 TIMES
                                       INDEXED BY WS-BR-IX.
               10  WS-BR-TB-KEY                PIC X(04).
               10  WS-BR-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-BR-TB-TXT                PIC X(20).
               10  WS-BR-TB-EFF                PIC 9(05).
               10  WS-BR-TB-EXP                PIC 9(05).
       01  WS-BR-WORK-GROUP-1.
           05  WS-BR-G1-JURIS              PIC 9(05).
           05  WS-BR-G1-OCN                PIC S9(11)V9(02) COMP-3.
           05  WS-BR-G1-SEQ                PIC 9(05).
           05  WS-BR-G1-LEVEL              PIC 9(07).
           05  WS-BR-G1-CENTRE             PIC 9(05).
           05  WS-BR-G1-INVOICE            PIC S9(11)V9(02) COMP-3.
           05  WS-BR-G1-BAN                PIC 9(07).
           05  WS-BR-G1-TARGET             PIC S9(11)V9(02) COMP-3.
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 50.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUCV07 - CODE PAGE AND SIGN CONVERSION'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-BR-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-BR-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9926.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-BR-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-BR-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT CNVIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF CNVIN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT REJOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF REJOUT' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-BR-CYCLE-YYDDD.
           COMPUTE WS-BR-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-BR-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-BR-CNT-04.
           MOVE 0 TO WS-BR-CNT-06.
           MOVE 0 TO WS-BR-CNT-01.
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
           IF WS-BR-ON-01
               PERFORM P2200-CONVERT-LAYOUT THRU
                   P2200-CONVERT-LAYOUT-EXIT.
           IF WS-BR-ON-01
               PERFORM P2300-DERIVE-CENTURY THRU
                   P2300-DERIVE-CENTURY-EXIT.
           PERFORM P2400-BUILD-RECORD THRU P2400-BUILD-RECORD-EXIT.
           PERFORM P2500-SPLIT-RECORD THRU P2500-SPLIT-RECORD-EXIT.
           IF WS-BR-ON-02
               PERFORM P2600-CHECK-PACKED THRU P2600-CHECK-PACKED-EXIT.
           PERFORM P2700-CONVERT-LAYOUT THRU P2700-CONVERT-LAYOUT-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ CNVIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P2200-CONVERT-LAYOUT.
           MOVE 0 TO WS-BR-CNT-06.
           INSPECT WS-BR-TXT-01 TALLYING WS-BR-CNT-06
               FOR ALL SPACES.
           INSPECT WS-BR-TXT-01 REPLACING ALL LOW-VALUES BY SPACES.
       P2200-CONVERT-LAYOUT-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P2300-DERIVE-CENTURY.
           MOVE SPACES TO WS-BR-TXT-02.
           STRING IB-PERIOD DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IB-REGION2 DELIMITED BY SIZE
               INTO WS-BR-TXT-02.
       P2300-DERIVE-CENTURY-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P2400-BUILD-RECORD.
           CALL 'CABCTLWR' USING WS-BR-TXT-01 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-BR-CNT-05.
       P2400-BUILD-RECORD-EXIT.
           EXIT.
       P2500-SPLIT-RECORD.
           IF WS-BR-AMT-02 < 12
               MOVE 12 TO WS-BR-AMT-02
               ADD 1 TO WS-BR-CNT-03.
           IF WS-BR-AMT-02 > 8639
               MOVE 8639 TO WS-BR-AMT-02
               ADD 1 TO WS-BR-CNT-04.
       P2500-SPLIT-RECORD-EXIT.
           EXIT.
       P2600-CHECK-PACKED.
           MOVE 'Y' TO WS-BR-SW-02.
           IF IB-CLASS < 36
               MOVE 'N' TO WS-BR-SW-02
               ADD 1 TO WS-BR-CNT-05.
           IF IB-CLASS > 1940
               MOVE 'N' TO WS-BR-SW-02
               ADD 1 TO WS-BR-CNT-01.
       P2600-CHECK-PACKED-EXIT.
           EXIT.
       P2700-CONVERT-LAYOUT.
           MOVE 'N' TO WS-BR-SW-01.
           IF WS-BR-TXT-02 NOT = WS-BR-TXT-01
               MOVE 'Y' TO WS-BR-SW-01
               MOVE WS-BR-TXT-02 TO WS-BR-TXT-01
               ADD 1 TO WS-BR-CNT-05.
       P2700-CONVERT-LAYOUT-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
      * A FAILURE HERE SUSPENDS THE RECORD, IT DOES NOT STOP THE STEP.
       P3100-FORMAT-CENTURY.
           ADD IB-REGION TO WS-BR-QTY-02.
           COMPUTE WS-BR-AMT-02 = WS-BR-QTY-02 * WS-BR-QTY-01.
           ADD WS-BR-AMT-02 TO WS-BR-AMT-01.
       P3100-FORMAT-CENTURY-EXIT.
           EXIT.
       P3200-EMIT-CENTURY.
           MOVE SPACES TO WS-BR-TXT-02.
           STRING IB-PERIOD DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IB-TYPE DELIMITED BY SIZE
               INTO WS-BR-TXT-02.
       P3200-EMIT-CENTURY-EXIT.
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
           MOVE 4 TO CT-STEP-SEQ.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE WS-BR-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-BR-TXT-02 TO CT-RESTART-KEY.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-FILLER.
           MOVE SPACES TO CT-ABEND-CD.
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
           CLOSE CNVIN.
           CLOSE REJOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABUCV07 - STEP COMPLETE'.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  BR-CNT-04 = ' WS-BR-CNT-04.
           DISPLAY '  BR-CNT-01 = ' WS-BR-CNT-01.
           DISPLAY '  BR-CNT-06 = ' WS-BR-CNT-06.
           DISPLAY '  BR-CNT-02 = ' WS-BR-CNT-02.
       P9000-EXIT.
           EXIT.
