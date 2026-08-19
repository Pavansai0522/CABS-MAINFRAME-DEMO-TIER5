      *****************************************************************
      * CABUCV03 - CODE PAGE AND SIGN CONVERSION                      *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               EMIIN   TELCABS.CABS.EMIIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               NEWOUT  TELCABS.CABS.NEWOUT         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - THE OUTPUT IS REBUILT EACH RUN     *
      * REVISION HISTORY                                              *
      *   V1.00  1993-10-18  S.MARCHETTI  INITIAL RELEASE             *
      *   V1.01  2003-09-17  S.MARCHETTI  RESTART KEY WRITTEN SO A    *
      *                      RERUN CAN POSITION                       *
      *   V1.03  2005-02-20  J.M.CASTILLO RECOMPILE ONLY - COPYBOOK   *
      *                      CHANGE UPSTREAM                          *
      *   V1.06  2016-11-16  T.YAMASHITA  OCCURS RAISED AFTER THE     *
      *                      FEBRUARY OVERFLOW                        *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUCV03.
       AUTHOR. TELCABS APPLICATIONS - FILE CONVERSION.
      *****************************************************************
      * CODE PAGE AND SIGN CONVERSION. THIS STEP IS SCHEDULED INSIDE  *
      * THE NIGHTLY ACCESS BILLING STREAM AND HAS NO INTERACTIVE ENTRY*
      * POINT.                                                        *
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL*
      * CHARACTER CARRIES MEANING DOWNSTREAM.                         *
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
           SELECT NEWOUT ASSIGN TO UT-S-NEWOUT
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
           SELECT RPTOUT ASSIGN TO UT-S-RPTOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
       DATA DIVISION.
       FILE SECTION.
      * EMIIN - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  EMIIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 100 CHARACTERS.
       01  CABS-BK-IN-RECORD.
           05  IB-STATE                    PIC S9(15) COMP-3.
           05  IB-SOURCE                   PIC S9(09)V9(05) COMP-3.
           05  IB-INVOICE                  PIC S9(09)V9(05) COMP-3.
           05  IB-REGION                   PIC X(20).
           05  IB-ACCOUNT                  PIC X(10).
           05  IB-CLASS                    PIC S9(07) COMP-3.
           05  IB-CARRIER                  PIC X(20).
           05  IB-REGION2                  PIC 9(03).
           05  IB-MEDIA                    PIC X(10).
           05  IB-CODE                     PIC S9(07) COMP-3.
           05  BK-FILL-01                  PIC X(5).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-BK-VIEW1 REDEFINES CABS-BK-IN-RECORD.
           05  R0B-SOURCE                  PIC X(06).
           05  R0B-TARIFF                  PIC X(03).
           05  R0B-JURIS                   PIC S9(07)V9(05) COMP-3.
           05  R0B-REGION                  PIC X(02).
           05  R0B-CIRCUIT                 PIC X(20).
           05  R0B-REST                    PIC X(62).
      * NEWOUT - CATALOGUED GENERATION DATA GROUP.
       FD  NEWOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-BK-OUT-RECORD.
           05  OB-MEDIA                    PIC 9(02).
           05  OB-JURIS                    PIC S9(11)V9(05) COMP-3.
           05  OB-ELEM                     PIC 9(04).
           05  OB-JURIS2                   PIC X(04).
           05  OB-JURIS3                   PIC X(06).
           05  OB-TARIFF                   PIC S9(07)V9(05) COMP-3.
           05  OB-GROUP                    PIC X(03).
           05  OB-LEVEL                    PIC X(16).
           05  OB-GROUP2                   PIC S9(13)V9(02) COMP-3.
           05  OB-JURIS4                   PIC S9(07)V9(05) COMP-3.
           05  BK-FILL-02                  PIC X(14).
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
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUCV03'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V2.01'.
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
           05  WS-BK-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BK-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BK-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BK-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BK-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BK-CNT-06                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-BK-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BK-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BK-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-BK-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BK-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BK-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-BK-TXT-01                PIC X(16) VALUE SPACES.
           05  WS-BK-TXT-02                PIC X(12) VALUE SPACES.
           05  WS-BK-TXT-03                PIC X(26) VALUE SPACES.
           05  WS-BK-TXT-04                PIC X(30) VALUE SPACES.
           05  WS-BK-TXT-05                PIC X(26) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-BK-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-BK-ON-01                 VALUE 'Y'.
               88  WS-BK-OFF-01                VALUE 'N'.
           05  WS-BK-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-BK-ON-02                 VALUE 'Y'.
               88  WS-BK-OFF-02                VALUE 'N'.
           05  WS-BK-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-BK-ON-03                 VALUE 'Y'.
               88  WS-BK-OFF-03                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-BK-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-BK-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-BK-SUB-03                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. A ROW THAT DOES NOT FIT IS COUNTED AND DROPPED
      * - THE STEP DOES NOT FAIL.
       01  WS-BK-TABLE.
           05  WS-BK-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-BK-TB-ENTRY OCCURS 120 TIMES
                                       INDEXED BY WS-BK-IX.
               10  WS-BK-TB-KEY                PIC X(04).
               10  WS-BK-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-BK-TB-TXT                PIC X(40).
               10  WS-BK-TB-EFF                PIC 9(05).
               10  WS-BK-TB-EXP                PIC 9(05).
       01  WS-BK-WORK-GROUP-1.
           05  WS-BK-G1-JURIS              PIC 9(05).
           05  WS-BK-G1-MEDIA              PIC X(10).
           05  WS-BK-G1-ELEM               PIC 9(07).
           05  WS-BK-G1-TARIFF             PIC X(10).
           05  WS-BK-G1-SEQ                PIC X(10).
           05  WS-BK-G1-JURIS              PIC S9(09) COMP-3.
           05  WS-BK-G1-JURIS              PIC 9(07).
           05  WS-BK-G1-CODE               PIC 9(05).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 58.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUCV03 - CODE PAGE AND SIGN CONVERSION'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-BK-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-BK-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9939.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-BK-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-BK-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT EMIIN.
           IF WS-FS-INPUT NOT = '00'
               DISPLAY 'EMIIN FILE STATUS = ' WS-FS-INPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON EMIIN - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT NEWOUT.
           IF WS-FS-OUTPUT NOT = '00'
               DISPLAY 'NEWOUT FILE STATUS = ' WS-FS-OUTPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON NEWOUT - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SUSOUT.
           IF WS-FS-SUSPENSE NOT = '00'
               DISPLAY 'SUSOUT FILE STATUS = ' WS-FS-SUSPENSE
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON SUSOUT - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               DISPLAY 'CTLOUT FILE STATUS = ' WS-FS-CONTROL
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON CTLOUT - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RPTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               DISPLAY 'RPTOUT FILE STATUS = ' WS-FS-OUTPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON RPTOUT - CHECK THE ALLOCATION' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-BK-CYCLE-YYDDD.
           COMPUTE WS-BK-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-BK-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-BK-CNT-06.
           MOVE 0 TO WS-BK-CNT-04.
           MOVE 0 TO WS-BK-CNT-03.
       P1200-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-BK-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-BK-TAB-CNT NOT < 120
               MOVE 'Y' TO WS-BK-SW-01
               ADD 1 TO WS-BK-CNT-06
           ELSE
               ADD 1 TO WS-BK-TAB-CNT
               SET WS-BK-IX TO WS-BK-TAB-CNT
               MOVE IB-STATE TO WS-BK-TB-KEY (WS-BK-IX)
               MOVE 0 TO WS-BK-TB-VAL (WS-BK-IX)
               MOVE SPACES TO WS-BK-TB-TXT (WS-BK-IX)
               ADD 1 TO WS-BK-CNT-02.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ EMIIN
               AT END MOVE 'Y' TO WS-BK-SW-01.
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
           IF WS-BK-ON-01
               PERFORM P2200-EDIT-CENTURY THRU P2200-EDIT-CENTURY-EXIT.
           PERFORM P2300-SELECT-RECORD THRU P2300-SELECT-RECORD-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ EMIIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-EDIT-CENTURY.
           MOVE SPACES TO CABS-BK-OUT-RECORD.
           MOVE IB-CODE TO OB-MEDIA.
           MOVE IB-REGION TO OB-JURIS.
           MOVE IB-STATE TO OB-ELEM.
           MOVE IB-INVOICE TO OB-JURIS2.
           MOVE IB-REGION2 TO OB-JURIS3.
           MOVE IB-SOURCE TO OB-TARIFF.
           WRITE CABS-BK-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
           MOVE 'N' TO WS-BK-SW-03.
           IF WS-BK-TAB-CNT > 0
               PERFORM P270-COMPARE-LAYOUT THRU P270-COMPARE-LAYOUT-EXIT
               VARYING WS-BK-SUB-03 FROM 1 BY 1
               UNTIL WS-BK-SUB-03 > WS-BK-TAB-CNT
               OR WS-BK-SW-03 = 'Y'.
       P2200-EDIT-CENTURY-EXIT.
           EXIT.
      * A FAILURE HERE SUSPENDS THE RECORD, IT DOES NOT STOP THE STEP.
       P2300-SELECT-RECORD.
           CALL 'CABHASH' USING IB-SOURCE WS-ACC-OCN-HASH.
           ADD WS-BK-CNT-06 TO WS-ACC-SEQ-HASH.
       P2300-SELECT-RECORD-EXIT.
           EXIT.
       P270-COMPARE-LAYOUT.
           SET WS-BK-IX TO WS-BK-SUB-03.
           IF WS-BK-TB-KEY (WS-BK-IX) = IB-SOURCE
               MOVE 'Y' TO WS-BK-SW-02
               MOVE WS-BK-TB-VAL (WS-BK-IX) TO WS-BK-QTY-03
               MOVE WS-BK-SUB-03 TO WS-BK-SUB-03.
       P270-COMPARE-LAYOUT-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-STAGE-LAYOUT.
           MOVE SPACES TO WS-BK-TXT-04.
           STRING IB-MEDIA DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IB-CODE DELIMITED BY SIZE
               INTO WS-BK-TXT-04.
       P3100-STAGE-LAYOUT-EXIT.
           EXIT.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P3200-STAGE-CENTURY.
           MOVE IB-REGION2 TO WS-BK-TXT-02.
           MOVE IB-REGION2 TO WS-BK-TXT-04.
           MOVE IB-REGION TO WS-BK-TXT-01.
           ADD 1 TO WS-BK-CNT-02.
       P3200-STAGE-CENTURY-EXIT.
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
           MOVE 'INPUT RECORDS' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO WS-BK-CNT-EDIT.
           MOVE WS-BK-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'OUTPUT RECORDS' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-BK-CNT-EDIT.
           MOVE WS-BK-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'SUMMARISED' TO PC-COL-001-020.
           MOVE WS-SUMM-CNT TO WS-BK-CNT-EDIT.
           MOVE WS-BK-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'SUSPENDED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-BK-CNT-EDIT.
           MOVE WS-BK-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'HELD FOR NEXT RUN' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-BK-CNT-EDIT.
           MOVE WS-BK-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-BK-CNT-01 TO WS-BK-CNT-EDIT.
           MOVE WS-BK-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE 6 TO CT-STEP-SEQ.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE WS-BK-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-FILLER.
           MOVE SPACES TO CT-RESTART-KEY.
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
           CLOSE EMIIN.
           CLOSE NEWOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABUCV03 - END OF RUN'.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  BK-CNT-05 = ' WS-BK-CNT-05.
           DISPLAY '  BK-CNT-01 = ' WS-BK-CNT-01.
           DISPLAY '  BK-CNT-03 = ' WS-BK-CNT-03.
       P9000-EXIT.
           EXIT.
