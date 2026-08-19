      *****************************************************************
      * CABUEX12 - BILLED ACCOUNT EXTRACT                             *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               CARIN   TELCABS.CABS.CARIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               SELOUT  TELCABS.CABS.SELOUT         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * REVISION HISTORY                                              *
      *   V1.00  1988-01-22  J.M.CASTILLO INITIAL RELEASE             *
      *   V1.03  2000-05-07  K.O.BRIEN    PRINT LINE WIDENED TO 133   *
      *   V1.04  2013-03-28  S.MARCHETTI  JOB PARAMETER MADE MANDATORY*
      *   V1.08  2017-01-16  L.FERREIRA   ROUNDING RULE TAKEN FROM THE*
      *                      RATE ROW                                 *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUEX12.
       AUTHOR. TELCABS APPLICATIONS - DATASET EXTRACT.
      *****************************************************************
      * BILLED ACCOUNT EXTRACT. THE STEP RUNS ONCE PER BILL CYCLE AND *
      * IS RERUN FROM THE TOP IF IT FAILS.                            *
      * A FAILURE HERE SUSPENDS THE RECORD, IT DOES NOT STOP THE STEP.*
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
           SELECT SELOUT ASSIGN TO UT-S-SELOUT
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
      * CARIN - WORK FILE, DELETED AT STEP END.
       FD  CARIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 110 CHARACTERS.
       01  CABS-AQ-IN-RECORD.
           05  IA-ELEM                     PIC X(16).
           05  IA-CENTRE                   PIC X(10).
           05  IA-GROUP                    PIC X(20).
           05  IA-BAND                     PIC 9(03).
           05  IA-CODE                     PIC X(20).
           05  IA-SOURCE                   PIC S9(11)V9(02) COMP-3.
           05  IA-SOURCE2                  PIC S9(07) COMP-3.
           05  IA-ELEM2                    PIC 9(04).
           05  IA-LEVEL                    PIC X(06).
           05  IA-STATUS                   PIC X(16).
           05  AQ-FILL-01                  PIC X(4).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-AQ-VIEW1 REDEFINES CABS-AQ-IN-RECORD.
           05  R0A-CIRCUIT                 PIC S9(13)V9(05) COMP-3.
           05  R0A-CYCLE                   PIC X(06).
           05  R0A-SEQ                     PIC X(10).
           05  R0A-MEDIA                   PIC 9(09).
           05  R0A-TARGET                  PIC 9(03).
           05  R0A-OCN                     PIC 9(07).
           05  R0A-REST                    PIC X(65).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-AQ-VIEW2 REDEFINES CABS-AQ-IN-RECORD.
           05  R1A-TARIFF                  PIC X(06).
           05  R1A-CLASS                   PIC X(02).
           05  R1A-CYCLE                   PIC S9(09) COMP-3.
           05  R1A-CODE                    PIC S9(13)V9(02) COMP-3.
           05  R1A-CARRIER                 PIC S9(05) COMP-3.
           05  R1A-TYPE                    PIC S9(09)V9(02) COMP-3.
           05  R1A-REST                    PIC X(80).
      * SELOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  SELOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 90 CHARACTERS.
       01  CABS-AQ-OUT-RECORD.
           05  OA-ACCOUNT                  PIC X(13).
           05  OA-STATE                    PIC 9(04).
           05  OA-CODE                     PIC X(16).
           05  OA-REGION                   PIC X(20).
           05  OA-JURIS                    PIC 9(02).
           05  OA-LEVEL                    PIC X(04).
           05  OA-ELEM                     PIC X(03).
           05  OA-ELEM2                    PIC X(20).
           05  OA-BAND                     PIC S9(13) COMP-3.
           05  AQ-FILL-02                  PIC X(1).
      * CTLOUT - PERMANENT DATASET HELD ON DASD.
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD              PIC X(180).
      * RPTOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  RPTOUT
           RECORDING MODE IS FA
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       COPY CABSPRNT.
       WORKING-STORAGE SECTION.
      * STANDARD SHARED WORKING STORAGE. SEE CABSWRK.
       COPY CABSWRK.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUEX12'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V2.27'.
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
           05  WS-AQ-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AQ-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AQ-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AQ-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AQ-CNT-05                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-AQ-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AQ-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-AQ-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AQ-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-AQ-TXT-01                PIC X(20) VALUE SPACES.
           05  WS-AQ-TXT-02                PIC X(30) VALUE SPACES.
           05  WS-AQ-TXT-03                PIC X(20) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-AQ-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-AQ-ON-01                 VALUE 'Y'.
               88  WS-AQ-OFF-01                VALUE 'N'.
           05  WS-AQ-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-AQ-ON-02                 VALUE 'Y'.
               88  WS-AQ-OFF-02                VALUE 'N'.
           05  WS-AQ-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-AQ-ON-03                 VALUE 'Y'.
               88  WS-AQ-OFF-03                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-AQ-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-AQ-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-AQ-SUB-03                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. A ROW THAT DOES NOT FIT IS COUNTED AND DROPPED
      * - THE STEP DOES NOT FAIL.
       01  WS-AQ-TABLE.
           05  WS-AQ-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-AQ-TB-ENTRY OCCURS 120 TIMES
                                       INDEXED BY WS-AQ-IX.
               10  WS-AQ-TB-KEY                PIC X(13).
               10  WS-AQ-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-AQ-TB-TXT                PIC X(20).
               10  WS-AQ-TB-EFF                PIC 9(05).
               10  WS-AQ-TB-EXP                PIC 9(05).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 55.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUEX12 - BILLED ACCOUNT EXTRACT'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-AQ-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-AQ-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9929.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-AQ-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-AQ-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT CARIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CARIN NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SELOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'SELOUT NOT AVAILABLE - OPEN REJECTED' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-AQ-CYCLE-YYDDD.
           COMPUTE WS-AQ-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-AQ-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-AQ-CNT-03.
           MOVE 0 TO WS-AQ-CNT-01.
           MOVE 0 TO WS-AQ-CNT-02.
       P1200-EXIT.
           EXIT.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-AQ-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-AQ-TAB-CNT NOT < 120
               MOVE 'Y' TO WS-AQ-SW-01
               ADD 1 TO WS-AQ-CNT-05
           ELSE
               ADD 1 TO WS-AQ-TAB-CNT
               SET WS-AQ-IX TO WS-AQ-TAB-CNT
               MOVE IA-LEVEL TO WS-AQ-TB-KEY (WS-AQ-IX)
               MOVE 0 TO WS-AQ-TB-VAL (WS-AQ-IX)
               MOVE SPACES TO WS-AQ-TB-TXT (WS-AQ-IX)
               ADD 1 TO WS-AQ-CNT-02.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ CARIN
               AT END MOVE 'Y' TO WS-AQ-SW-01.
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
           IF WS-AQ-ON-01
               PERFORM P2200-DERIVE-MASTER THRU
                   P2200-DERIVE-MASTER-EXIT.
           PERFORM P2300-RESOLVE-EXTRACT THRU
               P2300-RESOLVE-EXTRACT-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ CARIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-DERIVE-MASTER.
           MOVE 'Y' TO WS-AQ-SW-03.
           IF IA-SOURCE2 < 25
               MOVE 'N' TO WS-AQ-SW-03
               ADD 1 TO WS-AQ-CNT-02.
           IF IA-SOURCE2 > 1784
               MOVE 'N' TO WS-AQ-SW-03
               ADD 1 TO WS-AQ-CNT-01.
           MOVE 'N' TO WS-AQ-SW-01.
           IF WS-AQ-TAB-CNT > 0
               PERFORM P270-COMPARE-EXTRACT THRU
                   P270-COMPARE-EXTRACT-EXIT
               VARYING WS-AQ-SUB-01 FROM 1 BY 1
               UNTIL WS-AQ-SUB-01 > WS-AQ-TAB-CNT
               OR WS-AQ-SW-01 = 'Y'.
       P2200-DERIVE-MASTER-EXIT.
           EXIT.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P2300-RESOLVE-EXTRACT.
           MOVE 0 TO WS-AQ-QTY-02.
           MOVE 0 TO WS-AQ-QTY-01.
           MOVE 0 TO WS-AQ-AMT-02.
       P2300-RESOLVE-EXTRACT-EXIT.
           EXIT.
       P270-COMPARE-EXTRACT.
           SET WS-AQ-IX TO WS-AQ-SUB-02.
           IF WS-AQ-TB-KEY (WS-AQ-IX) = IA-GROUP
               MOVE 'Y' TO WS-AQ-SW-01
               MOVE WS-AQ-TB-VAL (WS-AQ-IX) TO WS-AQ-QTY-01
               MOVE WS-AQ-SUB-02 TO WS-AQ-SUB-03.
       P270-COMPARE-EXTRACT-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P3100-RELEASE-MASTER.
           MOVE IA-STATUS TO WS-AQ-TXT-02.
           MOVE IA-ELEM TO WS-AQ-TXT-02.
           MOVE IA-LEVEL TO WS-AQ-TXT-01.
           MOVE IA-SOURCE TO WS-AQ-TXT-02.
           ADD 1 TO WS-AQ-CNT-02.
       P3100-RELEASE-MASTER-EXIT.
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
           MOVE WS-READ-CNT TO WS-AQ-CNT-EDIT.
           MOVE WS-AQ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL OUT' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-AQ-CNT-EDIT.
           MOVE WS-AQ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL SUSPENDED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-AQ-CNT-EDIT.
           MOVE WS-AQ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL SUMMARISED' TO PC-COL-001-020.
           MOVE WS-SUMM-CNT TO WS-AQ-CNT-EDIT.
           MOVE WS-AQ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL CARRIED FWD' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-AQ-CNT-EDIT.
           MOVE WS-AQ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-AQ-CNT-01 TO WS-AQ-CNT-EDIT.
           MOVE WS-AQ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 02' TO PC-COL-001-020.
           MOVE WS-AQ-CNT-02 TO WS-AQ-CNT-EDIT.
           MOVE WS-AQ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 03' TO PC-COL-001-020.
           MOVE WS-AQ-CNT-03 TO WS-AQ-CNT-EDIT.
           MOVE WS-AQ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE SPACES TO CT-FILLER.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE 2 TO CT-STEP-SEQ.
           MOVE WS-AQ-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
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
           CLOSE CARIN.
           CLOSE SELOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABUEX12 - NORMAL END OF JOB'.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  AQ-CNT-04 = ' WS-AQ-CNT-04.
           DISPLAY '  AQ-CNT-05 = ' WS-AQ-CNT-05.
           DISPLAY '  AQ-CNT-03 = ' WS-AQ-CNT-03.
       P9000-EXIT.
           EXIT.
