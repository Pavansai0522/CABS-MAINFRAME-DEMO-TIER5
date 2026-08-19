      *****************************************************************
      * CABUEX03 - BILLED ACCOUNT EXTRACT                             *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               CARIN   TELCABS.CABS.CARIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               CIROUT  TELCABS.CABS.CIROUT         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - NOTHING IS UPDATED IN PLACE        *
      * REVISION HISTORY                                              *
      *   V1.00  1992-01-02  S.MARCHETTI  INITIAL RELEASE             *
      *   V1.01  1994-11-20  S.MARCHETTI  RETIRED THE SECOND SORT STEP*
      *                      - DONE IN PROGRAM                        *
      *   V1.04  2003-01-23  W.J.MCALLISTER JOB PARAMETER MADE        *
      *                      MANDATORY                                *
      *   V1.08  2016-10-23  T.YAMASHITA  SUSPENSE WRITE ADDED -      *
      *                      RECORDS WERE BEING DROPPED               *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUEX03.
       AUTHOR. TELCABS APPLICATIONS - DATASET EXTRACT.
      *****************************************************************
      * BILLED ACCOUNT EXTRACT. THE STEP IS DRIVEN ENTIRELY FROM THE  *
      * SYSIN PARM CARD AND THE DD ALLOCATIONS IN THE JOB.            *
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
           SELECT CARIN ASSIGN TO UT-S-CARIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT CIROUT ASSIGN TO UT-S-CIROUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT CTLOUT ASSIGN TO UT-S-CTLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CONTROL.
           SELECT RPTOUT ASSIGN TO UT-S-RPTOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
       DATA DIVISION.
       FILE SECTION.
      * CARIN - CATALOGUED GENERATION DATA GROUP.
       FD  CARIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-BY-IN-RECORD.
           05  IB-TARGET                   PIC S9(05) COMP-3.
           05  IB-TYPE                     PIC S9(13)V9(02) COMP-3.
           05  IB-CARRIER                  PIC 9(06).
           05  IB-BAND                     PIC S9(13) COMP-3.
           05  IB-STATUS                   PIC X(02).
           05  IB-ACCOUNT                  PIC 9(05).
           05  IB-SEQ                      PIC 9(04).
           05  IB-SEGMENT                  PIC X(13).
           05  IB-SEQ2                     PIC X(02).
           05  IB-SEGMENT2                 PIC X(03).
           05  BY-FILL-01                  PIC X(27).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-BY-VIEW1 REDEFINES CABS-BY-IN-RECORD.
           05  R0B-CARRIER                 PIC S9(15) COMP-3.
           05  R0B-PERIOD                  PIC X(13).
           05  R0B-ACCOUNT                 PIC S9(11)V9(02) COMP-3.
           05  R0B-CODE                    PIC X(03).
           05  R0B-TYPE                    PIC 9(06).
           05  R0B-SEQ                     PIC S9(11) COMP-3.
           05  R0B-REST                    PIC X(37).
      * CIROUT - CATALOGUED GENERATION DATA GROUP.
       FD  CIROUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 100 CHARACTERS.
       01  CABS-BY-OUT-RECORD.
           05  OB-GROUP                    PIC X(08).
           05  OB-CLASS                    PIC X(20).
           05  OB-CIRCUIT                  PIC S9(11) COMP-3.
           05  OB-MEDIA                    PIC X(16).
           05  OB-CLASS2                   PIC X(10).
           05  OB-CIRCUIT2                 PIC 9(09).
           05  OB-TYPE                     PIC 9(07).
           05  OB-SOURCE                   PIC 9(06).
           05  OB-PERIOD                   PIC X(06).
           05  OB-TARIFF                   PIC S9(05) COMP-3.
           05  OB-BAN                      PIC X(03).
           05  OB-REGION                   PIC 9(02).
           05  BY-FILL-02                  PIC X(4).
      * CTLOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD              PIC X(180).
      * RPTOUT - CATALOGUED GENERATION DATA GROUP.
       FD  RPTOUT
           RECORDING MODE IS FA
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       COPY CABSPRNT.
       WORKING-STORAGE SECTION.
      * STANDARD SHARED WORKING STORAGE. SEE CABSWRK.
       COPY CABSWRK.
      * SHARED LAYOUT PULLED IN FOR THE EXTRACT SIDE.
       COPY CABSCIRC.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUEX03'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V3.21'.
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
           05  WS-BY-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BY-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BY-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BY-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BY-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BY-CNT-06                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-BY-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BY-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-BY-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BY-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BY-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BY-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-BY-TXT-01                PIC X(10) VALUE SPACES.
           05  WS-BY-TXT-02                PIC X(20) VALUE SPACES.
           05  WS-BY-TXT-03                PIC X(10) VALUE SPACES.
           05  WS-BY-TXT-04                PIC X(08) VALUE SPACES.
           05  WS-BY-TXT-05                PIC X(08) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-BY-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-BY-ON-01                 VALUE 'Y'.
               88  WS-BY-OFF-01                VALUE 'N'.
           05  WS-BY-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-BY-ON-02                 VALUE 'Y'.
               88  WS-BY-OFF-02                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-BY-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-BY-SUB-02                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. LOADED ONCE AT INITIALISATION AND SEARCHED WITH
      * A SUBSCRIPTED WALK.
       01  WS-BY-TABLE.
           05  WS-BY-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-BY-TB-ENTRY OCCURS 150 TIMES
                                       INDEXED BY WS-BY-IX.
               10  WS-BY-TB-KEY                PIC X(08).
               10  WS-BY-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-BY-TB-TXT                PIC X(30).
               10  WS-BY-TB-EFF                PIC 9(05).
               10  WS-BY-TB-EXP                PIC 9(05).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 55.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUEX03 - BILLED ACCOUNT EXTRACT'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-BY-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-BY-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9961.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-BY-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-BY-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT CARIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CARIN NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CIROUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CIROUT NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CTLOUT NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RPTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RPTOUT NOT AVAILABLE - OPEN REJECTED' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-BY-CYCLE-YYDDD.
           COMPUTE WS-BY-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-BY-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-BY-CNT-03.
           MOVE 0 TO WS-BY-CNT-01.
           MOVE 0 TO WS-BY-CNT-02.
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
           PERFORM P2200-EXPAND-FILTER THRU P2200-EXPAND-FILTER-EXIT.
           PERFORM P2300-DERIVE-SELECTION THRU
               P2300-DERIVE-SELECTION-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ CARIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P2200-EXPAND-FILTER.
           MOVE SPACES TO WS-BY-TXT-05.
           STRING IB-SEQ DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IB-ACCOUNT DELIMITED BY SIZE
               INTO WS-BY-TXT-05.
       P2200-EXPAND-FILTER-EXIT.
           EXIT.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P2300-DERIVE-SELECTION.
           MOVE IB-TYPE TO WS-BY-TXT-03.
           MOVE IB-ACCOUNT TO WS-BY-TXT-01.
           ADD 1 TO WS-BY-CNT-05.
       P2300-DERIVE-SELECTION-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-FORMAT-SUBSET.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-BY-TXT-04 TO PC-COL-001-020.
           MOVE WS-BY-TXT-01 TO PC-COL-021-060.
           MOVE WS-BY-AMT-04 TO WS-BY-AMT-EDIT.
           MOVE WS-BY-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P3100-FORMAT-SUBSET-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P3200-STAGE-SELECTION.
           MOVE SPACES TO CABS-BY-OUT-RECORD.
           MOVE IB-STATUS TO OB-GROUP.
           MOVE IB-STATUS TO OB-CLASS.
           MOVE IB-CARRIER TO OB-CIRCUIT.
           MOVE IB-BAND TO OB-MEDIA.
           MOVE IB-SEQ TO OB-CLASS2.
           MOVE IB-CARRIER TO OB-CIRCUIT2.
           MOVE IB-SEQ TO OB-TYPE.
           MOVE IB-BAND TO OB-SOURCE.
           MOVE IB-ACCOUNT TO OB-PERIOD.
           WRITE CABS-BY-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P3200-STAGE-SELECTION-EXIT.
           EXIT.
      * S800-CONTROL SECTION - THE MANDATORY CABS CONTROL BOUNDARY.
       S800-CONTROL SECTION.
       P8000-CONTROL.
           PERFORM P8010-PRINT-AUDIT-REPORT THRU P8010-EXIT.
           PERFORM P8100-BUILD-CONTROL-REC THRU P8100-EXIT.
           PERFORM P8200-CHECK-BALANCE THRU P8200-EXIT.
           PERFORM P8300-WRITE-CONTROL-REC THRU P8300-EXIT.
       P8000-EXIT.
           EXIT.
       P8010-PRINT-AUDIT-REPORT.
           ADD 1 TO WS-RPT-PAGE-NBR.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '1' TO PC-CC.
           MOVE WS-RPT-TITLE1 TO PC-TEXT.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-RPT-TITLE2 TO PC-TEXT.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL IN' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO WS-BY-CNT-EDIT.
           MOVE WS-BY-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL OUT' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-BY-CNT-EDIT.
           MOVE WS-BY-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL SUSPENDED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-BY-CNT-EDIT.
           MOVE WS-BY-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL SUMMARISED' TO PC-COL-001-020.
           MOVE WS-SUMM-CNT TO WS-BY-CNT-EDIT.
           MOVE WS-BY-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL CARRIED FWD' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-BY-CNT-EDIT.
           MOVE WS-BY-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-BY-CNT-01 TO WS-BY-CNT-EDIT.
           MOVE WS-BY-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE SPACES TO CT-FILLER.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE WS-BY-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE 8 TO CT-STEP-SEQ.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
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
           CLOSE CARIN.
           CLOSE CIROUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABUEX03 - NORMAL END OF JOB'.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  BY-CNT-02 = ' WS-BY-CNT-02.
           DISPLAY '  BY-CNT-01 = ' WS-BY-CNT-01.
           DISPLAY '  BY-CNT-03 = ' WS-BY-CNT-03.
       P9000-EXIT.
           EXIT.
