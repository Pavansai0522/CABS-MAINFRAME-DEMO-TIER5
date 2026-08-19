      *****************************************************************
      * CABUXR19 - ACCOUNT TO INVOICE CROSS REFERENCE                 *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               SUSIN   TELCABS.CABS.SUSIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               ORPOUT  TELCABS.CABS.ORPOUT         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - THE OUTPUT IS REBUILT EACH RUN     *
      * REVISION HISTORY                                              *
      *   V1.00  2009-11-01  B.R.HALVORSEN INITIAL RELEASE            *
      *   V1.03  2014-10-28  S.MARCHETTI  PARM CARD EXTENDED,         *
      *                      POSITIONS 40 THROUGH 48                  *
      *   V1.06  2019-05-21  A.BUKOWSKI   REPORT PAGINATION CORRECTED *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUXR19.
       AUTHOR. TELCABS APPLICATIONS - CROSS REFERENCE REPORT.
      *****************************************************************
      * ACCOUNT TO INVOICE CROSS REFERENCE. THE STEP IS SCHEDULED     *
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
           SELECT SUSIN ASSIGN TO UT-S-SUSIN
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
           SELECT RPTOUT ASSIGN TO UT-S-RPTOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
       DATA DIVISION.
       FILE SECTION.
      * SUSIN - WORK FILE, DELETED AT STEP END.
       FD  SUSIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-BH-IN-RECORD.
           05  IB-SEGMENT                  PIC S9(15) COMP-3.
           05  IB-CLASS                    PIC 9(03).
           05  IB-CYCLE                    PIC 9(05).
           05  IB-ACCOUNT                  PIC 9(04).
           05  IB-MEDIA                    PIC S9(09) COMP-3.
           05  IB-TARIFF                   PIC X(03).
           05  IB-ELEM                     PIC 9(04).
           05  IB-PERIOD                   PIC S9(09) COMP-3.
           05  IB-STATE                    PIC X(02).
           05  IB-MEDIA2                   PIC S9(07)V9(02) COMP-3.
           05  IB-SOURCE                   PIC S9(13) COMP-3.
           05  IB-CYCLE2                   PIC S9(11)V9(02) COMP-3.
           05  IB-ELEM2                    PIC 9(02).
           05  BH-FILL-01                  PIC X(20).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-BH-VIEW1 REDEFINES CABS-BH-IN-RECORD.
           05  R0B-STATUS                  PIC X(02).
           05  R0B-GROUP                   PIC X(06).
           05  R0B-SEQ                     PIC X(04).
           05  R0B-CODE                    PIC S9(13)V9(05) COMP-3.
           05  R0B-SOURCE                  PIC X(16).
           05  R0B-REST                    PIC X(42).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-BH-VIEW2 REDEFINES CABS-BH-IN-RECORD.
           05  R1B-OCN                     PIC 9(09).
           05  R1B-INVOICE                 PIC S9(15) COMP-3.
           05  R1B-SEQ                     PIC 9(02).
           05  R1B-MEDIA                   PIC S9(07)V9(05) COMP-3.
           05  R1B-LEVEL                   PIC S9(05) COMP-3.
           05  R1B-BAN                     PIC S9(11) COMP-3.
           05  R1B-REGION                  PIC S9(11)V9(05) COMP-3.
           05  R1B-BAND                    PIC 9(09).
           05  R1B-INVOICE2                PIC 9(02).
           05  R1B-REST                    PIC X(25).
      * ORPOUT - CATALOGUED GENERATION DATA GROUP.
       FD  ORPOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-BH-OUT-RECORD.
           05  OB-ACCOUNT                  PIC S9(11) COMP-3.
           05  OB-TARIFF                   PIC S9(11) COMP-3.
           05  OB-GROUP                    PIC X(04).
           05  OB-TYPE                     PIC 9(09).
           05  OB-GROUP2                   PIC X(04).
           05  OB-BAND                     PIC 9(02).
           05  OB-PERIOD                   PIC 9(05).
           05  OB-SEQ                      PIC S9(13) COMP-3.
           05  OB-OCN                      PIC S9(15) COMP-3.
           05  OB-BAND2                    PIC S9(13)V9(05) COMP-3.
           05  OB-TARIFF2                  PIC S9(07) COMP-3.
           05  OB-BAN                      PIC S9(13) COMP-3.
           05  BH-FILL-02                  PIC X(8).
      * SUSOUT - CATALOGUED GENERATION DATA GROUP.
       FD  SUSOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 300 CHARACTERS.
       01  CABS-SUSOUT-RECORD              PIC X(300).
      * CTLOUT - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD              PIC X(180).
      * RPTOUT - PERMANENT DATASET HELD ON DASD.
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
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUXR19'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V1.18'.
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
           05  WS-BH-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BH-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BH-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BH-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BH-CNT-05                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-BH-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BH-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BH-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-BH-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BH-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BH-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BH-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-BH-TXT-01                PIC X(26) VALUE SPACES.
           05  WS-BH-TXT-02                PIC X(20) VALUE SPACES.
           05  WS-BH-TXT-03                PIC X(20) VALUE SPACES.
           05  WS-BH-TXT-04                PIC X(16) VALUE SPACES.
           05  WS-BH-TXT-05                PIC X(10) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-BH-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-BH-ON-01                 VALUE 'Y'.
               88  WS-BH-OFF-01                VALUE 'N'.
           05  WS-BH-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-BH-ON-02                 VALUE 'Y'.
               88  WS-BH-OFF-02                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-BH-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-BH-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-BH-SUB-03                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. A ROW THAT DOES NOT FIT IS COUNTED AND DROPPED
      * - THE STEP DOES NOT FAIL.
       01  WS-BH-TABLE.
           05  WS-BH-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-BH-TB-ENTRY OCCURS 120 TIMES
                                       INDEXED BY WS-BH-IX.
               10  WS-BH-TB-KEY                PIC X(10).
               10  WS-BH-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-BH-TB-TXT                PIC X(40).
               10  WS-BH-TB-EFF                PIC 9(05).
               10  WS-BH-TB-EXP                PIC 9(05).
       01  WS-BH-WORK-GROUP-1.
           05  WS-BH-G1-JURIS              PIC S9(09) COMP-3.
           05  WS-BH-G1-OCN                PIC X(20).
           05  WS-BH-G1-ELEM               PIC 9(05).
           05  WS-BH-G1-CARRIER            PIC X(10).
           05  WS-BH-G1-GROUP              PIC X(10).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 60.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUXR19 - ACCOUNT TO INVOICE CROSS REFERENCE'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-BH-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-BH-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
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
           05  WS-BH-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-BH-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT SUSIN.
           IF WS-FS-INPUT NOT = '00'
               DISPLAY 'SUSIN FILE STATUS = ' WS-FS-INPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'SUSIN NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT ORPOUT.
           IF WS-FS-OUTPUT NOT = '00'
               DISPLAY 'ORPOUT FILE STATUS = ' WS-FS-OUTPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'ORPOUT NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SUSOUT.
           IF WS-FS-SUSPENSE NOT = '00'
               DISPLAY 'SUSOUT FILE STATUS = ' WS-FS-SUSPENSE
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'SUSOUT NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               DISPLAY 'CTLOUT FILE STATUS = ' WS-FS-CONTROL
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CTLOUT NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RPTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               DISPLAY 'RPTOUT FILE STATUS = ' WS-FS-OUTPUT
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
           MOVE PC1-CYCLE-YYDDD TO WS-BH-CYCLE-YYDDD.
           COMPUTE WS-BH-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-BH-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-BH-CNT-05.
           MOVE 0 TO WS-BH-CNT-04.
           MOVE 0 TO WS-BH-CNT-01.
       P1200-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-BH-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-BH-TAB-CNT NOT < 120
               MOVE 'Y' TO WS-BH-SW-01
               ADD 1 TO WS-BH-CNT-01
           ELSE
               ADD 1 TO WS-BH-TAB-CNT
               SET WS-BH-IX TO WS-BH-TAB-CNT
               MOVE IB-MEDIA2 TO WS-BH-TB-KEY (WS-BH-IX)
               MOVE 0 TO WS-BH-TB-VAL (WS-BH-IX)
               MOVE SPACES TO WS-BH-TB-TXT (WS-BH-IX)
               ADD 1 TO WS-BH-CNT-02.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ SUSIN
               AT END MOVE 'Y' TO WS-BH-SW-01.
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
           PERFORM P2200-CHECK-ORPHAN THRU P2200-CHECK-ORPHAN-EXIT.
           IF WS-BH-ON-02
               PERFORM P2300-SPLIT-GROUP THRU P2300-SPLIT-GROUP-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ SUSIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-CHECK-ORPHAN.
           MOVE IB-PERIOD TO WS-BH-TXT-02.
           MOVE IB-ELEM TO WS-BH-TXT-04.
           MOVE IB-STATE TO WS-BH-TXT-04.
           ADD 1 TO WS-BH-CNT-04.
           MOVE 'N' TO WS-BH-SW-02.
           IF WS-BH-TAB-CNT > 0
               PERFORM P250-COMPARE-SIDE THRU P250-COMPARE-SIDE-EXIT
               VARYING WS-BH-SUB-03 FROM 1 BY 1
               UNTIL WS-BH-SUB-03 > WS-BH-TAB-CNT
               OR WS-BH-SW-02 = 'Y'.
       P2200-CHECK-ORPHAN-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P2300-SPLIT-GROUP.
           MOVE 0 TO WS-BH-QTY-03.
           MOVE 0 TO WS-BH-QTY-01.
           MOVE 0 TO WS-BH-AMT-02.
           MOVE 0 TO WS-BH-AMT-04.
       P2300-SPLIT-GROUP-EXIT.
           EXIT.
       P250-COMPARE-SIDE.
           SET WS-BH-IX TO WS-BH-SUB-03.
           IF WS-BH-TB-KEY (WS-BH-IX) = IB-CYCLE
               MOVE 'Y' TO WS-BH-SW-02
               MOVE WS-BH-TB-VAL (WS-BH-IX) TO WS-BH-QTY-02
               MOVE WS-BH-SUB-03 TO WS-BH-SUB-01.
       P250-COMPARE-SIDE-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P3100-RELEASE-PAIR.
           CALL 'CABHASH' USING IB-MEDIA2 WS-ACC-OCN-HASH.
           ADD WS-BH-CNT-01 TO WS-ACC-SEQ-HASH.
       P3100-RELEASE-PAIR-EXIT.
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
           MOVE 'RECORDS SUMMARISED' TO PC-COL-001-020.
           MOVE WS-SUMM-CNT TO WS-BH-CNT-EDIT.
           MOVE WS-BH-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS CARRIED FWD' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-BH-CNT-EDIT.
           MOVE WS-BH-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS REJECTED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-BH-CNT-EDIT.
           MOVE WS-BH-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS READ' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO WS-BH-CNT-EDIT.
           MOVE WS-BH-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS WRITTEN' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-BH-CNT-EDIT.
           MOVE WS-BH-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-BH-CNT-01 TO WS-BH-CNT-EDIT.
           MOVE WS-BH-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 02' TO PC-COL-001-020.
           MOVE WS-BH-CNT-02 TO WS-BH-CNT-EDIT.
           MOVE WS-BH-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 03' TO PC-COL-001-020.
           MOVE WS-BH-CNT-03 TO WS-BH-CNT-EDIT.
           MOVE WS-BH-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-FILLER.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE WS-BH-TXT-03 TO CT-RESTART-KEY.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE WS-BH-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE 7 TO CT-STEP-SEQ.
       P8100-EXIT.
           EXIT.
      * P8200-CHECK-BALANCE - THE EQUATION IS TESTED AS IT STANDS AND
      * THE RETURN CODE IS SET FROM THE RESULT SO THE SCHEDULER CAN
      * SEE IT.
       P8200-CHECK-BALANCE.
           IF CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARISED +
                       CT-CARRIED-FWD
               MOVE 'B' TO CT-BAL-IND
           ELSE
               MOVE 'O' TO CT-BAL-IND.
           IF CT-OUT-OF-BAL
               MOVE 0004 TO CT-RC.
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
           CLOSE SUSIN.
           CLOSE ORPOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABUXR19 - RUN COMPLETE'.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  BH-CNT-02 = ' WS-BH-CNT-02.
           DISPLAY '  BH-CNT-03 = ' WS-BH-CNT-03.
           DISPLAY '  BH-CNT-04 = ' WS-BH-CNT-04.
           DISPLAY '  BH-CNT-05 = ' WS-BH-CNT-05.
       P9000-EXIT.
           EXIT.
