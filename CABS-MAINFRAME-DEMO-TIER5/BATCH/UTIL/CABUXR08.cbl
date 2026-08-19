      *****************************************************************
      * CABUXR08 - JURISDICTION TO STATE CROSS REFERENCE              *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               REFIN   TELCABS.CABS.REFIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               ORPOUT  TELCABS.CABS.ORPOUT         (LOCAL)     *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * REVISION HISTORY                                              *
      *   V1.00  1989-07-21  T.YAMASHITA  INITIAL RELEASE             *
      *   V1.04  1992-04-20  K.O.BRIEN    BLOCK SIZE SET TO ZERO -    *
      *                      SYSTEM DETERMINED                        *
      *   V1.05  2005-03-06  J.M.CASTILLO EFFECTIVE DATE FILTER ADDED *
      *                      PER AUDIT FINDING                        *
      *   V1.09  2011-05-25  A.BUKOWSKI   JOB PARAMETER MADE MANDATORY*
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUXR08.
       AUTHOR. TELCABS APPLICATIONS - CROSS REFERENCE REPORT.
      *****************************************************************
      * JURISDICTION TO STATE CROSS REFERENCE. THE STEP IS SCHEDULED  *
      * MONTHLY AND ALSO RUN ON DEMAND WHEN A CENTRE ASKS FOR IT.     *
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO   *
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.                     *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           C01 IS TO-NEW-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REFIN ASSIGN TO UT-S-REFIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT ORPOUT ASSIGN TO UT-S-ORPOUT
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
      * REFIN - PERMANENT DATASET HELD ON DASD.
       FD  REFIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-CG-IN-RECORD.
           05  IC-BAND                     PIC X(02).
           05  IC-CYCLE                    PIC S9(07) COMP-3.
           05  IC-MEDIA                    PIC S9(13)V9(02) COMP-3.
           05  IC-CENTRE                   PIC S9(11)V9(02) COMP-3.
           05  IC-TARIFF                   PIC S9(11)V9(02) COMP-3.
           05  IC-CENTRE2                  PIC S9(15) COMP-3.
           05  IC-LEVEL                    PIC S9(15) COMP-3.
           05  IC-ELEM                     PIC X(16).
           05  CG-FILL-01                  PIC X(20).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-CG-VIEW1 REDEFINES CABS-CG-IN-RECORD.
           05  R0C-INVOICE                 PIC 9(07).
           05  R0C-CYCLE                   PIC X(03).
           05  R0C-JURIS                   PIC S9(13) COMP-3.
           05  R0C-JURIS2                  PIC 9(06).
           05  R0C-CODE                    PIC S9(07)V9(02) COMP-3.
           05  R0C-ELEM                    PIC X(06).
           05  R0C-MEDIA                   PIC X(04).
           05  R0C-REST                    PIC X(42).
      * ORPOUT - PERMANENT DATASET HELD ON DASD.
       FD  ORPOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 90 CHARACTERS.
       01  CABS-CG-OUT-RECORD.
           05  OC-MEDIA                    PIC 9(05).
           05  OC-PERIOD                   PIC X(13).
           05  OC-CYCLE                    PIC S9(11)V9(05) COMP-3.
           05  OC-ELEM                     PIC 9(06).
           05  OC-STATUS                   PIC X(16).
           05  OC-CODE                     PIC S9(05) COMP-3.
           05  OC-JURIS                    PIC X(03).
           05  OC-PERIOD2                  PIC S9(11) COMP-3.
           05  OC-OCN                      PIC 9(05).
           05  OC-INVOICE                  PIC S9(11) COMP-3.
           05  OC-STATUS2                  PIC X(08).
           05  OC-MEDIA2                   PIC S9(15) COMP-3.
           05  CG-FILL-02                  PIC X(2).
      * SUSOUT - PERMANENT DATASET HELD ON DASD.
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
      * SHARED LAYOUT PULLED IN FOR THE ORPHAN SIDE.
       COPY CABSCIRC.
      * SHARED LAYOUT PULLED IN FOR THE REFERENCE SIDE.
       COPY CABSBHDR.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUXR08'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V2.05'.
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
           05  WS-CG-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CG-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CG-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CG-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CG-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CG-CNT-06                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-CG-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CG-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CG-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-CG-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CG-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CG-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CG-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-CG-TXT-01                PIC X(10) VALUE SPACES.
           05  WS-CG-TXT-02                PIC X(16) VALUE SPACES.
           05  WS-CG-TXT-03                PIC X(10) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-CG-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-CG-ON-01                 VALUE 'Y'.
               88  WS-CG-OFF-01                VALUE 'N'.
           05  WS-CG-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-CG-ON-02                 VALUE 'Y'.
               88  WS-CG-OFF-02                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-CG-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CG-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CG-SUB-03                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. A ROW THAT DOES NOT FIT IS COUNTED AND DROPPED
      * - THE STEP DOES NOT FAIL.
       01  WS-CG-TABLE.
           05  WS-CG-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-CG-TB-ENTRY OCCURS 120 TIMES
                                       INDEXED BY WS-CG-IX.
               10  WS-CG-TB-KEY                PIC X(10).
               10  WS-CG-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-CG-TB-TXT                PIC X(20).
               10  WS-CG-TB-EFF                PIC 9(05).
               10  WS-CG-TB-EXP                PIC 9(05).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 58.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUXR08 - JURISDICTION TO STATE CROSS REFERENCE'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-CG-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-CG-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9934.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-CG-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-CG-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT REFIN.
           IF WS-FS-INPUT NOT = '00'
               DISPLAY 'REFIN FILE STATUS = ' WS-FS-INPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'REFIN COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT ORPOUT.
           IF WS-FS-OUTPUT NOT = '00'
               DISPLAY 'ORPOUT FILE STATUS = ' WS-FS-OUTPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'ORPOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SUSOUT.
           IF WS-FS-SUSPENSE NOT = '00'
               DISPLAY 'SUSOUT FILE STATUS = ' WS-FS-SUSPENSE
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'SUSOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               DISPLAY 'CTLOUT FILE STATUS = ' WS-FS-CONTROL
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CTLOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-CG-CYCLE-YYDDD.
           COMPUTE WS-CG-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-CG-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-CG-CNT-02.
           MOVE 0 TO WS-CG-CNT-06.
           MOVE 0 TO WS-CG-CNT-04.
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
           PERFORM P2200-CONVERT-MATCH THRU P2200-CONVERT-MATCH-EXIT.
           PERFORM P2300-APPLY-PAIR THRU P2300-APPLY-PAIR-EXIT.
           PERFORM P2400-SELECT-PAIR THRU P2400-SELECT-PAIR-EXIT.
           PERFORM P2500-EXPAND-ORPHAN THRU P2500-EXPAND-ORPHAN-EXIT.
           PERFORM P2600-APPLY-MATCH THRU P2600-APPLY-MATCH-EXIT.
           PERFORM P2700-SPLIT-GROUP THRU P2700-SPLIT-GROUP-EXIT.
           IF WS-CG-ON-02
               PERFORM P2800-DERIVE-LINK THRU P2800-DERIVE-LINK-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ REFIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-CONVERT-MATCH.
           MOVE WS-CG-AMT-01 TO WS-CG-AMT-03.
           IF WS-CG-AMT-03 < 0
               COMPUTE WS-CG-AMT-03 = 0 - WS-CG-AMT-01.
       P2200-CONVERT-MATCH-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P2300-APPLY-PAIR.
           UNSTRING WS-CG-TXT-01 DELIMITED BY '/'
               INTO WS-CG-TXT-03
               WS-CG-TXT-01
               TALLYING IN WS-CG-CNT-05.
       P2300-APPLY-PAIR-EXIT.
           EXIT.
      * A FAILURE HERE SUSPENDS THE RECORD, IT DOES NOT STOP THE STEP.
       P2400-SELECT-PAIR.
           MOVE IC-TARIFF TO WS-CG-TXT-03.
           MOVE IC-MEDIA TO WS-CG-TXT-03.
           MOVE IC-TARIFF TO WS-CG-TXT-01.
           ADD 1 TO WS-CG-CNT-06.
       P2400-SELECT-PAIR-EXIT.
           EXIT.
       P2500-EXPAND-ORPHAN.
           MOVE 0 TO WS-CG-QTY-02.
           MOVE 0 TO WS-CG-QTY-03.
           MOVE 0 TO WS-CG-AMT-01.
           MOVE 0 TO WS-CG-AMT-02.
       P2500-EXPAND-ORPHAN-EXIT.
           EXIT.
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT
      * PRECEDES THIS PROGRAM IN THE JOB.
       P2600-APPLY-MATCH.
           IF WS-CG-AMT-02 NOT = 0
               COMPUTE WS-CG-QTY-02 = WS-CG-AMT-01 * 100 / WS-CG-AMT-02
           ELSE
               MOVE 0 TO WS-CG-QTY-02.
       P2600-APPLY-MATCH-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P2700-SPLIT-GROUP.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-CG-TXT-02 TO PC-COL-001-020.
           MOVE WS-CG-TXT-01 TO PC-COL-021-060.
           MOVE WS-CG-AMT-04 TO WS-CG-AMT-EDIT.
           MOVE WS-CG-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P2700-SPLIT-GROUP-EXIT.
           EXIT.
       P2800-DERIVE-LINK.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-BAN-UNKNOWN TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-CG-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
       P2800-DERIVE-LINK-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-FORMAT-ORPHAN.
           ADD IC-CENTRE TO WS-CG-QTY-01.
           COMPUTE WS-CG-AMT-02 ROUNDED = WS-CG-QTY-01 * WS-CG-QTY-03.
           ADD WS-CG-AMT-02 TO WS-CG-AMT-04.
       P3100-FORMAT-ORPHAN-EXIT.
           EXIT.
       P3200-STAGE-PAIR.
           MOVE SPACES TO WS-CG-TXT-01.
           STRING IC-TARIFF DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IC-BAND DELIMITED BY SIZE
               INTO WS-CG-TXT-01.
       P3200-STAGE-PAIR-EXIT.
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
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE WS-CG-TXT-03 TO CT-RESTART-KEY.
           MOVE SPACES TO CT-FILLER.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE 4 TO CT-STEP-SEQ.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE WS-CG-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
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
           CLOSE REFIN.
           CLOSE ORPOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABUXR08 - END OF RUN'.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  CG-CNT-06 = ' WS-CG-CNT-06.
           DISPLAY '  CG-CNT-05 = ' WS-CG-CNT-05.
           DISPLAY '  CG-CNT-02 = ' WS-CG-CNT-02.
       P9000-EXIT.
           EXIT.
