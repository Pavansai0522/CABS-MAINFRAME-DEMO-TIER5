      *****************************************************************
      * CABUEX24 - SUSPENSE EXTRACT FOR THE RECYCLE JOB               *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               CIRIN   TELCABS.CABS.CIRIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               CAROUT  TELCABS.CABS.CAROUT         (LOCAL)     *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - NOTHING IS UPDATED IN PLACE        *
      * REVISION HISTORY                                              *
      *   V1.00  1990-11-02  K.O.BRIEN    INITIAL RELEASE             *
      *   V1.02  1992-06-26  R.T.WHEELER  HASH TOTAL ADDED TO THE     *
      *                      CONTROL RECORD                           *
      *   V1.06  1996-12-13  R.T.WHEELER  SECOND OUTPUT FILE ADDED FOR*
      *                      THE FACTOR STUDY                         *
      *   V1.10  2010-02-15  C.ADEYEMI    CENTURY PIVOT APPLIED TO THE*
      *                      CYCLE DATE                               *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUEX24.
       AUTHOR. TELCABS APPLICATIONS - DATASET EXTRACT.
      *****************************************************************
      * SUSPENSE EXTRACT FOR THE RECYCLE JOB. THIS STEP IS SCHEDULED  *
      * INSIDE THE NIGHTLY ACCESS BILLING STREAM AND HAS NO           *
      * INTERACTIVE ENTRY POINT.                                      *
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE   *
      * MORE AT END OF FILE.                                          *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           C01 IS TO-NEW-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CIRIN ASSIGN TO UT-S-CIRIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT CAROUT ASSIGN TO UT-S-CAROUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT CTLOUT ASSIGN TO UT-S-CTLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      * CIRIN - WORK FILE, DELETED AT STEP END.
       FD  CIRIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-AS-IN-RECORD.
           05  IA-TARIFF                   PIC S9(11) COMP-3.
           05  IA-OCN                      PIC 9(05).
           05  IA-STATE                    PIC S9(07) COMP-3.
           05  IA-REGION                   PIC S9(13)V9(02) COMP-3.
           05  IA-GROUP                    PIC X(10).
           05  IA-GROUP2                   PIC 9(05).
           05  IA-BAN                      PIC 9(07).
           05  IA-SEGMENT                  PIC S9(13) COMP-3.
           05  IA-TARIFF2                  PIC X(10).
           05  IA-CODE                     PIC 9(07).
           05  IA-CARRIER                  PIC 9(09).
           05  AS-FILL-01                  PIC X(2).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-AS-VIEW1 REDEFINES CABS-AS-IN-RECORD.
           05  R0A-INVOICE                 PIC S9(13)V9(02) COMP-3.
           05  R0A-STATUS                  PIC S9(09) COMP-3.
           05  R0A-LEVEL                   PIC S9(09) COMP-3.
           05  R0A-LEVEL2                  PIC X(16).
           05  R0A-LEVEL3                  PIC X(10).
           05  R0A-TARIFF                  PIC 9(03).
           05  R0A-CODE                    PIC S9(11) COMP-3.
           05  R0A-REST                    PIC X(27).
      * CAROUT - PERMANENT DATASET HELD ON DASD.
       FD  CAROUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-AS-OUT-RECORD.
           05  OA-CLASS                    PIC S9(13) COMP-3.
           05  OA-SEGMENT                  PIC X(20).
           05  OA-PERIOD                   PIC S9(11)V9(02) COMP-3.
           05  OA-OCN                      PIC 9(09).
           05  OA-CENTRE                   PIC X(03).
           05  OA-TARIFF                   PIC X(08).
           05  OA-STATE                    PIC S9(15) COMP-3.
           05  AS-FILL-02                  PIC X(18).
      * CTLOUT - PERMANENT DATASET HELD ON DASD.
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD              PIC X(180).
       WORKING-STORAGE SECTION.
      * STANDARD SHARED WORKING STORAGE. SEE CABSWRK.
       COPY CABSWRK.
      * SHARED LAYOUT PULLED IN FOR THE RANGE SIDE.
       COPY CABSFCTR.
      * SHARED LAYOUT PULLED IN FOR THE EXTRACT SIDE.
       COPY CABSCDR.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUEX24'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V3.11'.
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
           05  WS-AS-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AS-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AS-CNT-03                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-AS-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AS-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AS-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AS-QTY-04                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-AS-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AS-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-AS-TXT-01                PIC X(12) VALUE SPACES.
           05  WS-AS-TXT-02                PIC X(30) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-AS-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-AS-ON-01                 VALUE 'Y'.
               88  WS-AS-OFF-01                VALUE 'N'.
           05  WS-AS-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-AS-ON-02                 VALUE 'Y'.
               88  WS-AS-OFF-02                VALUE 'N'.
           05  WS-AS-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-AS-ON-03                 VALUE 'Y'.
               88  WS-AS-OFF-03                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-AS-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-AS-SUB-02                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. THE LIMIT WAS RAISED WHEN THE SOUTHEAST CENTRES
      * WERE MERGED AND HAS NOT BEEN REVIEWED SINCE.
       01  WS-AS-TABLE.
           05  WS-AS-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-AS-TB-ENTRY OCCURS 100 TIMES
                                       INDEXED BY WS-AS-IX.
               10  WS-AS-TB-KEY                PIC X(10).
               10  WS-AS-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-AS-TB-TXT                PIC X(40).
               10  WS-AS-TB-EFF                PIC 9(05).
               10  WS-AS-TB-EXP                PIC 9(05).
       01  WS-AS-WORK-GROUP-1.
           05  WS-AS-G1-INVOICE            PIC X(20).
           05  WS-AS-G1-LEVEL              PIC 9(05).
           05  WS-AS-G1-ACCOUNT            PIC S9(11)V9(02) COMP-3.
           05  WS-AS-G1-JURIS              PIC S9(11)V9(02) COMP-3.
           05  WS-AS-G1-TARGET             PIC X(20).
           05  WS-AS-G1-BAND               PIC S9(11)V9(02) COMP-3.
           05  WS-AS-G1-TARIFF             PIC X(20).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 50.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUEX24 - SUSPENSE EXTRACT FOR THE RECYCLE JOB'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-AS-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-AS-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9983.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-AS-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-AS-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT CIRIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CIRIN OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CAROUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CAROUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
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
           MOVE PC1-CYCLE-YYDDD TO WS-AS-CYCLE-YYDDD.
           COMPUTE WS-AS-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-AS-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-AS-CNT-01.
           MOVE 0 TO WS-AS-CNT-02.
           MOVE 0 TO WS-AS-CNT-03.
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
           PERFORM P2200-VALIDATE-FILTER THRU
               P2200-VALIDATE-FILTER-EXIT.
           PERFORM P2300-APPLY-CANDIDATE THRU
               P2300-APPLY-CANDIDATE-EXIT.
           PERFORM P2400-SELECT-SELECTION THRU
               P2400-SELECT-SELECTION-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ CIRIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P2200-VALIDATE-FILTER.
           MOVE WS-AS-AMT-01 TO WS-AS-AMT-01.
           IF WS-AS-AMT-01 < 0
               COMPUTE WS-AS-AMT-01 = 0 - WS-AS-AMT-01.
       P2200-VALIDATE-FILTER-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P2300-APPLY-CANDIDATE.
           MOVE 0 TO WS-AS-CNT-03.
           INSPECT WS-AS-TXT-02 TALLYING WS-AS-CNT-03
               FOR ALL SPACES.
           INSPECT WS-AS-TXT-02 REPLACING ALL LOW-VALUES BY SPACES.
       P2300-APPLY-CANDIDATE-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P2400-SELECT-SELECTION.
           UNSTRING WS-AS-TXT-01 DELIMITED BY '/'
               INTO WS-AS-TXT-02
               WS-AS-TXT-02
               TALLYING IN WS-AS-CNT-01.
       P2400-SELECT-SELECTION-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P3100-RELEASE-SELECTION.
           MOVE SPACES TO CABS-AS-OUT-RECORD.
           MOVE IA-TARIFF2 TO OA-CLASS.
           MOVE IA-OCN TO OA-SEGMENT.
           MOVE IA-CODE TO OA-PERIOD.
           MOVE IA-STATE TO OA-OCN.
           MOVE IA-CODE TO OA-CENTRE.
           MOVE IA-GROUP TO OA-TARIFF.
           WRITE CABS-AS-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P3100-RELEASE-SELECTION-EXIT.
           EXIT.
       P3200-WRITE-EXTRACT.
           MOVE IA-CARRIER TO WS-AS-TXT-01.
           MOVE IA-BAN TO WS-AS-TXT-01.
           MOVE IA-CARRIER TO WS-AS-TXT-02.
           ADD 1 TO WS-AS-CNT-02.
       P3200-WRITE-EXTRACT-EXIT.
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
           MOVE WS-AS-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE 1 TO CT-STEP-SEQ.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-FILLER.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
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
           CLOSE CIRIN.
           CLOSE CAROUT.
           CLOSE CTLOUT.
           DISPLAY 'CABUEX24 - END OF RUN'.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  AS-CNT-02 = ' WS-AS-CNT-02.
           DISPLAY '  AS-CNT-01 = ' WS-AS-CNT-01.
           DISPLAY '  AS-CNT-03 = ' WS-AS-CNT-03.
       P9000-EXIT.
           EXIT.
