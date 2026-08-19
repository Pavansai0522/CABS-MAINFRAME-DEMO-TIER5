      *****************************************************************
      * CABUXR07 - RATE ELEMENT TO TARIFF CROSS REFERENCE             *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               SUSIN   TELCABS.CABS.SUSIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               LNKOUT  TELCABS.CABS.LNKOUT         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - THE OUTPUT IS REBUILT EACH RUN     *
      * REVISION HISTORY                                              *
      *   V1.00  1990-06-02  B.R.HALVORSEN INITIAL RELEASE            *
      *   V1.04  1995-07-21  A.BUKOWSKI   PARM CARD EXTENDED,         *
      *                      POSITIONS 40 THROUGH 48                  *
      *   V1.06  2017-03-23  T.YAMASHITA  SECOND OUTPUT FILE ADDED FOR*
      *                      THE FACTOR STUDY                         *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUXR07.
       AUTHOR. TELCABS APPLICATIONS - CROSS REFERENCE REPORT.
      *****************************************************************
      * RATE ELEMENT TO TARIFF CROSS REFERENCE. THE STEP IS SCHEDULED *
      * MONTHLY AND ALSO RUN ON DEMAND WHEN A CENTRE ASKS FOR IT.     *
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
           SELECT SUSIN ASSIGN TO UT-S-SUSIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT LNKOUT ASSIGN TO UT-S-LNKOUT
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
      * SUSIN - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  SUSIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 130 CHARACTERS.
       01  CABS-ED-IN-RECORD.
           05  IE-OCN                      PIC X(20).
           05  IE-LEVEL                    PIC S9(07) COMP-3.
           05  IE-CYCLE                    PIC S9(11) COMP-3.
           05  IE-REGION                   PIC X(10).
           05  IE-OCN2                     PIC X(04).
           05  IE-JURIS                    PIC X(13).
           05  IE-SOURCE                   PIC S9(13)V9(02) COMP-3.
           05  IE-ELEM                     PIC X(16).
           05  IE-ACCOUNT                  PIC X(04).
           05  IE-GROUP                    PIC X(10).
           05  IE-CYCLE2                   PIC 9(09).
           05  IE-REGION2                  PIC S9(09)V9(02) COMP-3.
           05  IE-REGION3                  PIC S9(09)V9(02) COMP-3.
           05  IE-CARRIER                  PIC S9(07) COMP-3.
           05  ED-FILL-01                  PIC X(10).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-ED-VIEW1 REDEFINES CABS-ED-IN-RECORD.
           05  R0E-CLASS                   PIC X(03).
           05  R0E-REGION                  PIC S9(09) COMP-3.
           05  R0E-REGION2                 PIC S9(11) COMP-3.
           05  R0E-STATE                   PIC X(08).
           05  R0E-GROUP                   PIC X(06).
           05  R0E-CLASS2                  PIC X(16).
           05  R0E-REST                    PIC X(86).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-ED-VIEW2 REDEFINES CABS-ED-IN-RECORD.
           05  R1E-CENTRE                  PIC S9(11)V9(02) COMP-3.
           05  R1E-STATE                   PIC X(08).
           05  R1E-CODE                    PIC S9(07)V9(05) COMP-3.
           05  R1E-REGION                  PIC S9(05) COMP-3.
           05  R1E-MEDIA                   PIC S9(07) COMP-3.
           05  R1E-TARGET                  PIC X(04).
           05  R1E-STATE2                  PIC X(16).
           05  R1E-REST                    PIC X(81).
      * LNKOUT - PERMANENT DATASET HELD ON DASD.
       FD  LNKOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-ED-OUT-RECORD.
           05  OE-ACCOUNT                  PIC S9(13)V9(02) COMP-3.
           05  OE-BAND                     PIC S9(07) COMP-3.
           05  OE-BAND2                    PIC S9(07)V9(02) COMP-3.
           05  OE-PERIOD                   PIC S9(07) COMP-3.
           05  OE-BAND3                    PIC 9(04).
           05  OE-MEDIA                    PIC X(02).
           05  OE-PERIOD2                  PIC S9(09)V9(02) COMP-3.
           05  OE-CIRCUIT                  PIC X(16).
           05  OE-INVOICE                  PIC 9(03).
           05  OE-SOURCE                   PIC 9(04).
           05  ED-FILL-02                  PIC X(24).
      * SUSOUT - WORK FILE, DELETED AT STEP END.
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
      * RPTOUT - SEQUENTIAL, BLOCKED BY THE SYSTEM.
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
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUXR07'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V1.17'.
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
           05  WS-ED-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-ED-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-ED-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-ED-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-ED-CNT-05                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-ED-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-ED-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-ED-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-ED-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-ED-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-ED-TXT-01                PIC X(12) VALUE SPACES.
           05  WS-ED-TXT-02                PIC X(30) VALUE SPACES.
           05  WS-ED-TXT-03                PIC X(16) VALUE SPACES.
           05  WS-ED-TXT-04                PIC X(08) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-ED-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-ED-ON-01                 VALUE 'Y'.
               88  WS-ED-OFF-01                VALUE 'N'.
           05  WS-ED-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-ED-ON-02                 VALUE 'Y'.
               88  WS-ED-OFF-02                VALUE 'N'.
           05  WS-ED-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-ED-ON-03                 VALUE 'Y'.
               88  WS-ED-OFF-03                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-ED-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-ED-SUB-02                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. A ROW THAT DOES NOT FIT IS COUNTED AND DROPPED
      * - THE STEP DOES NOT FAIL.
       01  WS-ED-TABLE.
           05  WS-ED-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-ED-TB-ENTRY OCCURS 120 TIMES
                                       INDEXED BY WS-ED-IX.
               10  WS-ED-TB-KEY                PIC X(08).
               10  WS-ED-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-ED-TB-TXT                PIC X(30).
               10  WS-ED-TB-EFF                PIC 9(05).
               10  WS-ED-TB-EXP                PIC 9(05).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 58.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUXR07 - RATE ELEMENT TO TARIFF CROSS REFERENCE'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-ED-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-ED-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9967.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-ED-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-ED-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'SUSIN COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               DISPLAY 'SUSIN FILE STATUS = ' WS-FS-INPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT LNKOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'LNKOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               DISPLAY 'LNKOUT FILE STATUS = ' WS-FS-OUTPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SUSOUT.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'SUSOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               DISPLAY 'SUSOUT FILE STATUS = ' WS-FS-SUSPENSE
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CTLOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               DISPLAY 'CTLOUT FILE STATUS = ' WS-FS-CONTROL
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RPTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RPTOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               DISPLAY 'RPTOUT FILE STATUS = ' WS-FS-OUTPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
       P1100-EXIT.
           EXIT.
      * P1200-READ-PARM - THE CYCLE DATE ARRIVES AS TWO DIGITS AND IS
      * PIVOTED ON DW-PIVOT-YY BEFORE ANY DATE MATH.
       P1200-READ-PARM.
           MOVE SPACES TO WS-PARM-CARD.
           ACCEPT WS-PARM-CARD FROM SYSIN.
           CALL 'CABPARMR' USING WS-PARM-CARD WS-RC-PARMR.
           MOVE PC1-CYCLE-YYDDD TO WS-ED-CYCLE-YYDDD.
           COMPUTE WS-ED-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-ED-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-ED-CNT-04.
           MOVE 0 TO WS-ED-CNT-03.
           MOVE 0 TO WS-ED-CNT-05.
       P1200-EXIT.
           EXIT.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-ED-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-ED-TAB-CNT NOT < 120
               MOVE 'Y' TO WS-ED-SW-01
               ADD 1 TO WS-ED-CNT-02
           ELSE
               ADD 1 TO WS-ED-TAB-CNT
               SET WS-ED-IX TO WS-ED-TAB-CNT
               MOVE IE-OCN TO WS-ED-TB-KEY (WS-ED-IX)
               MOVE 0 TO WS-ED-TB-VAL (WS-ED-IX)
               MOVE SPACES TO WS-ED-TB-TXT (WS-ED-IX)
               ADD 1 TO WS-ED-CNT-01.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ SUSIN
               AT END MOVE 'Y' TO WS-ED-SW-01.
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
           PERFORM P2200-DERIVE-SIDE THRU P2200-DERIVE-SIDE-EXIT.
           IF WS-ED-ON-02
               PERFORM P2300-APPLY-MATCH THRU P2300-APPLY-MATCH-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ SUSIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-DERIVE-SIDE.
           MOVE 0 TO WS-ED-QTY-03.
           MOVE 0 TO WS-ED-QTY-01.
           MOVE 0 TO WS-ED-AMT-01.
           MOVE 0 TO WS-ED-AMT-02.
           MOVE 'N' TO WS-ED-SW-02.
           IF WS-ED-TAB-CNT > 0
               PERFORM P250-COMPARE-REFERENCE THRU
                   P250-COMPARE-REFERENCE-EXIT
               VARYING WS-ED-SUB-01 FROM 1 BY 1
               UNTIL WS-ED-SUB-01 > WS-ED-TAB-CNT
               OR WS-ED-SW-02 = 'Y'.
       P2200-DERIVE-SIDE-EXIT.
           EXIT.
       P2300-APPLY-MATCH.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-DATE-INVALID TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-ED-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
       P2300-APPLY-MATCH-EXIT.
           EXIT.
       P250-COMPARE-REFERENCE.
           SET WS-ED-IX TO WS-ED-SUB-01.
           IF WS-ED-TB-KEY (WS-ED-IX) = IE-OCN2
               MOVE 'Y' TO WS-ED-SW-01
               MOVE WS-ED-TB-VAL (WS-ED-IX) TO WS-ED-QTY-02
               MOVE WS-ED-SUB-01 TO WS-ED-SUB-01.
       P250-COMPARE-REFERENCE-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P3100-EMIT-MATCH.
           MOVE IE-OCN TO WS-ED-TXT-04.
           MOVE IE-CYCLE2 TO WS-ED-TXT-02.
           MOVE IE-JURIS TO WS-ED-TXT-02.
           ADD 1 TO WS-ED-CNT-02.
       P3100-EMIT-MATCH-EXIT.
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
           MOVE 'SUSPENDED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-ED-CNT-EDIT.
           MOVE WS-ED-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'INPUT RECORDS' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO WS-ED-CNT-EDIT.
           MOVE WS-ED-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'HELD FOR NEXT RUN' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-ED-CNT-EDIT.
           MOVE WS-ED-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'SUMMARISED' TO PC-COL-001-020.
           MOVE WS-SUMM-CNT TO WS-ED-CNT-EDIT.
           MOVE WS-ED-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'OUTPUT RECORDS' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-ED-CNT-EDIT.
           MOVE WS-ED-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-ED-CNT-01 TO WS-ED-CNT-EDIT.
           MOVE WS-ED-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 02' TO PC-COL-001-020.
           MOVE WS-ED-CNT-02 TO WS-ED-CNT-EDIT.
           MOVE WS-ED-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE 4 TO CT-STEP-SEQ.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE WS-ED-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE WS-ED-TXT-01 TO CT-RESTART-KEY.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE SPACES TO CT-FILLER.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
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
           CLOSE LNKOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABUXR07 - STEP COMPLETE'.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  ED-CNT-01 = ' WS-ED-CNT-01.
           DISPLAY '  ED-CNT-03 = ' WS-ED-CNT-03.
       P9000-EXIT.
           EXIT.
