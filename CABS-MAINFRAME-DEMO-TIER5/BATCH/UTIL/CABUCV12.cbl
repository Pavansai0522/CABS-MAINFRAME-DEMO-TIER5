      *****************************************************************
      * CABUCV12 - CODE PAGE AND SIGN CONVERSION                      *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               OLDIN   TELCABS.CABS.OLDIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               CNVOUT  TELCABS.CABS.CNVOUT         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - THE OUTPUT IS REBUILT EACH RUN     *
      * REVISION HISTORY                                              *
      *   V1.00  1989-05-28  A.BUKOWSKI   INITIAL RELEASE             *
      *   V1.01  2012-08-19  C.ADEYEMI    SECOND OUTPUT FILE ADDED FOR*
      *                      THE FACTOR STUDY                         *
      *   V1.03  2014-01-23  P.NAIR       SECOND OUTPUT FILE ADDED FOR*
      *                      THE FACTOR STUDY                         *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUCV12.
       AUTHOR. TELCABS APPLICATIONS - FILE CONVERSION.
      *****************************************************************
      * CODE PAGE AND SIGN CONVERSION. THE STEP RUNS ONCE PER BILL    *
      * CYCLE AND IS RERUN FROM THE TOP IF IT FAILS.                  *
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND   *
      * THERE NEVER HAS BEEN.                                         *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           C01 IS TO-NEW-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OLDIN ASSIGN TO UT-S-OLDIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT CNVOUT ASSIGN TO UT-S-CNVOUT
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
      * OLDIN - CATALOGUED GENERATION DATA GROUP.
       FD  OLDIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-AR-IN-RECORD.
           05  IA-SEGMENT                  PIC 9(04).
           05  IA-SOURCE                   PIC X(20).
           05  IA-CENTRE                   PIC S9(13) COMP-3.
           05  IA-CENTRE2                  PIC 9(09).
           05  IA-CODE                     PIC S9(13) COMP-3.
           05  IA-JURIS                    PIC X(16).
           05  IA-BAND                     PIC 9(02).
           05  IA-GROUP                    PIC S9(11)V9(02) COMP-3.
           05  IA-REGION                   PIC X(02).
           05  AR-FILL-01                  PIC X(6).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-AR-VIEW1 REDEFINES CABS-AR-IN-RECORD.
           05  R0A-SEQ                     PIC S9(09) COMP-3.
           05  R0A-INVOICE                 PIC 9(09).
           05  R0A-STATE                   PIC S9(13) COMP-3.
           05  R0A-CYCLE                   PIC S9(11)V9(05) COMP-3.
           05  R0A-TARIFF                  PIC S9(09) COMP-3.
           05  R0A-OCN                     PIC X(08).
           05  R0A-REST                    PIC X(37).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-AR-VIEW2 REDEFINES CABS-AR-IN-RECORD.
           05  R1A-GROUP                   PIC S9(09) COMP-3.
           05  R1A-CLASS                   PIC X(16).
           05  R1A-CLASS2                  PIC S9(05) COMP-3.
           05  R1A-LEVEL                   PIC X(16).
           05  R1A-OCN                     PIC X(16).
           05  R1A-GROUP2                  PIC S9(11) COMP-3.
           05  R1A-BAN                     PIC X(16).
           05  R1A-REST                    PIC X(2).
      * CNVOUT - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  CNVOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-AR-OUT-RECORD.
           05  OA-MEDIA                    PIC S9(05) COMP-3.
           05  OA-BAND                     PIC X(08).
           05  OA-INVOICE                  PIC 9(07).
           05  OA-CENTRE                   PIC X(13).
           05  OA-TYPE                     PIC S9(07)V9(05) COMP-3.
           05  OA-SEGMENT                  PIC S9(09)V9(02) COMP-3.
           05  OA-TARGET                   PIC 9(03).
           05  OA-TARGET2                  PIC X(04).
           05  OA-GROUP                    PIC S9(09) COMP-3.
           05  OA-ACCOUNT                  PIC X(16).
           05  AR-FILL-02                  PIC X(8).
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
      * SHARED LAYOUT PULLED IN FOR THE SIGN SIDE.
       COPY CABSCDR.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUCV12'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V1.14'.
           05  WS-TABLE-LIMIT              PIC S9(04) COMP-3
                                         VALUE 50.
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
           05  WS-AR-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AR-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AR-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AR-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AR-CNT-05                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-AR-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AR-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AR-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AR-QTY-04                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-AR-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AR-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-AR-TXT-01                PIC X(10) VALUE SPACES.
           05  WS-AR-TXT-02                PIC X(08) VALUE SPACES.
           05  WS-AR-TXT-03                PIC X(10) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-AR-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-AR-ON-01                 VALUE 'Y'.
               88  WS-AR-OFF-01                VALUE 'N'.
           05  WS-AR-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-AR-ON-02                 VALUE 'Y'.
               88  WS-AR-OFF-02                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-AR-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-AR-SUB-02                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. LOADED ONCE AT INITIALISATION AND SEARCHED WITH
      * A SUBSCRIPTED WALK.
       01  WS-AR-TABLE.
           05  WS-AR-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-AR-TB-ENTRY OCCURS 50 TIMES
                                       INDEXED BY WS-AR-IX.
               10  WS-AR-TB-KEY                PIC X(13).
               10  WS-AR-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-AR-TB-TXT                PIC X(40).
               10  WS-AR-TB-EFF                PIC 9(05).
               10  WS-AR-TB-EXP                PIC 9(05).
       01  WS-AR-WORK-GROUP-1.
           05  WS-AR-G1-ACCOUNT            PIC X(20).
           05  WS-AR-G1-GROUP              PIC S9(09) COMP-3.
           05  WS-AR-G1-LEVEL              PIC 9(07).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 60.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUCV12 - CODE PAGE AND SIGN CONVERSION'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-AR-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-AR-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9944.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-AR-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-AR-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT OLDIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF OLDIN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CNVOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF CNVOUT' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SUSOUT.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF SUSOUT' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF CTLOUT' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RPTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF RPTOUT' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-AR-CYCLE-YYDDD.
           COMPUTE WS-AR-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-AR-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-AR-CNT-05.
           MOVE 0 TO WS-AR-CNT-04.
           MOVE 0 TO WS-AR-CNT-01.
       P1200-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-AR-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-AR-TAB-CNT NOT < 50
               MOVE 'Y' TO WS-AR-SW-01
               ADD 1 TO WS-AR-CNT-01
           ELSE
               ADD 1 TO WS-AR-TAB-CNT
               SET WS-AR-IX TO WS-AR-TAB-CNT
               MOVE IA-SOURCE TO WS-AR-TB-KEY (WS-AR-IX)
               MOVE 0 TO WS-AR-TB-VAL (WS-AR-IX)
               MOVE SPACES TO WS-AR-TB-TXT (WS-AR-IX)
               ADD 1 TO WS-AR-CNT-03.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ OLDIN
               AT END MOVE 'Y' TO WS-AR-SW-01.
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
           PERFORM P2200-SPLIT-FIELD THRU P2200-SPLIT-FIELD-EXIT.
           IF WS-AR-ON-02
               PERFORM P2300-DERIVE-LAYOUT THRU
                   P2300-DERIVE-LAYOUT-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ OLDIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P2200-SPLIT-FIELD.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-OCN-UNKNOWN TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-AR-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
           MOVE 'N' TO WS-AR-SW-02.
           IF WS-AR-TAB-CNT > 0
               PERFORM P260-COMPARE-LAYOUT THRU P260-COMPARE-LAYOUT-EXIT
               VARYING WS-AR-SUB-02 FROM 1 BY 1
               UNTIL WS-AR-SUB-02 > WS-AR-TAB-CNT
               OR WS-AR-SW-02 = 'Y'.
       P2200-SPLIT-FIELD-EXIT.
           EXIT.
      * A FAILURE HERE SUSPENDS THE RECORD, IT DOES NOT STOP THE STEP.
       P2300-DERIVE-LAYOUT.
           MOVE 'Y' TO WS-AR-SW-01.
           IF IA-CODE < 32
               MOVE 'N' TO WS-AR-SW-01
               ADD 1 TO WS-AR-CNT-05.
           IF IA-CODE > 1607
               MOVE 'N' TO WS-AR-SW-01
               ADD 1 TO WS-AR-CNT-03.
       P2300-DERIVE-LAYOUT-EXIT.
           EXIT.
       P260-COMPARE-LAYOUT.
           SET WS-AR-IX TO WS-AR-SUB-02.
           IF WS-AR-TB-KEY (WS-AR-IX) = IA-JURIS
               MOVE 'Y' TO WS-AR-SW-02
               MOVE WS-AR-TB-VAL (WS-AR-IX) TO WS-AR-QTY-04
               MOVE WS-AR-SUB-02 TO WS-AR-SUB-02.
       P260-COMPARE-LAYOUT-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-RELEASE-ZONE.
           MOVE SPACES TO WS-AR-TXT-01.
           STRING IA-SEGMENT DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IA-REGION DELIMITED BY SIZE
               INTO WS-AR-TXT-01.
       P3100-RELEASE-ZONE-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P3200-WRITE-RECORD.
           MOVE IA-SOURCE TO WS-AR-TXT-02.
           MOVE IA-SOURCE TO WS-AR-TXT-01.
           MOVE IA-CENTRE2 TO WS-AR-TXT-02.
           MOVE IA-SOURCE TO WS-AR-TXT-02.
           ADD 1 TO WS-AR-CNT-05.
       P3200-WRITE-RECORD-EXIT.
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
           MOVE 'RECORDS WRITTEN' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-AR-CNT-EDIT.
           MOVE WS-AR-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS SUMMARISED' TO PC-COL-001-020.
           MOVE WS-SUMM-CNT TO WS-AR-CNT-EDIT.
           MOVE WS-AR-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS REJECTED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-AR-CNT-EDIT.
           MOVE WS-AR-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS READ' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO WS-AR-CNT-EDIT.
           MOVE WS-AR-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS CARRIED FWD' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-AR-CNT-EDIT.
           MOVE WS-AR-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-AR-CNT-01 TO WS-AR-CNT-EDIT.
           MOVE WS-AR-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE WS-AR-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE 3 TO CT-STEP-SEQ.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE WS-AR-TXT-03 TO CT-RESTART-KEY.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-FILLER.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
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
           CLOSE OLDIN.
           CLOSE CNVOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABUCV12 - END OF RUN'.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  AR-CNT-04 = ' WS-AR-CNT-04.
           DISPLAY '  AR-CNT-03 = ' WS-AR-CNT-03.
           DISPLAY '  AR-CNT-01 = ' WS-AR-CNT-01.
           DISPLAY '  AR-CNT-02 = ' WS-AR-CNT-02.
       P9000-EXIT.
           EXIT.
