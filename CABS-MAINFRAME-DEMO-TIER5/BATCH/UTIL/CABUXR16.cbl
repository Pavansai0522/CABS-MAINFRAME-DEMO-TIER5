      *****************************************************************
      * CABUXR16 - ACCOUNT TO INVOICE CROSS REFERENCE                 *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               INVIN   TELCABS.CABS.INVIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               XRFOUT  TELCABS.CABS.XRFOUT         (LOCAL)     *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - THE OUTPUT IS REBUILT EACH RUN     *
      * REVISION HISTORY                                              *
      *   V1.00  1987-08-22  S.MARCHETTI  INITIAL RELEASE             *
      *   V1.02  1997-01-21  A.BUKOWSKI   SECOND OUTPUT FILE ADDED FOR*
      *                      THE FACTOR STUDY                         *
      *   V1.04  2000-12-28  S.MARCHETTI  EFFECTIVE DATE FILTER ADDED *
      *                      PER AUDIT FINDING                        *
      *   V1.08  2013-11-09  W.J.MCALLISTER RECOMPILE ONLY - COPYBOOK *
      *                      CHANGE UPSTREAM                          *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUXR16.
       AUTHOR. TELCABS APPLICATIONS - CROSS REFERENCE REPORT.
      *****************************************************************
      * ACCOUNT TO INVOICE CROSS REFERENCE. THE STEP RUNS ONCE PER    *
      * BILL CYCLE AND IS RERUN FROM THE TOP IF IT FAILS.             *
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
           SELECT INVIN ASSIGN TO UT-S-INVIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT XRFOUT ASSIGN TO UT-S-XRFOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT CTLOUT ASSIGN TO UT-S-CTLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      * INVIN - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  INVIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 90 CHARACTERS.
       01  CABS-BQ-IN-RECORD.
           05  IB-STATUS                   PIC X(20).
           05  IB-SOURCE                   PIC X(16).
           05  IB-PERIOD                   PIC S9(07)V9(02) COMP-3.
           05  IB-GROUP                    PIC X(13).
           05  IB-PERIOD2                  PIC S9(11) COMP-3.
           05  IB-TYPE                     PIC 9(02).
           05  IB-JURIS                    PIC X(08).
           05  IB-CARRIER                  PIC X(13).
           05  BQ-FILL-01                  PIC X(7).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-BQ-VIEW1 REDEFINES CABS-BQ-IN-RECORD.
           05  R0B-STATE                   PIC X(02).
           05  R0B-CARRIER                 PIC X(02).
           05  R0B-GROUP                   PIC S9(13) COMP-3.
           05  R0B-STATE2                  PIC S9(13) COMP-3.
           05  R0B-CLASS                   PIC S9(11)V9(02) COMP-3.
           05  R0B-JURIS                   PIC X(20).
           05  R0B-REST                    PIC X(45).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-BQ-VIEW2 REDEFINES CABS-BQ-IN-RECORD.
           05  R1B-GROUP                   PIC X(06).
           05  R1B-LEVEL                   PIC X(08).
           05  R1B-JURIS                   PIC X(06).
           05  R1B-SEGMENT                 PIC S9(09) COMP-3.
           05  R1B-REST                    PIC X(65).
      * XRFOUT - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  XRFOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 90 CHARACTERS.
       01  CABS-BQ-OUT-RECORD.
           05  OB-ELEM                     PIC S9(13)V9(02) COMP-3.
           05  OB-LEVEL                    PIC X(06).
           05  OB-PERIOD                   PIC S9(07) COMP-3.
           05  OB-SOURCE                   PIC X(20).
           05  OB-BAN                      PIC X(13).
           05  OB-TARIFF                   PIC X(10).
           05  OB-MEDIA                    PIC S9(13) COMP-3.
           05  OB-BAN2                     PIC X(06).
           05  OB-CENTRE                   PIC 9(06).
           05  BQ-FILL-02                  PIC X(10).
      * CTLOUT - WORK FILE, DELETED AT STEP END.
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD              PIC X(180).
       WORKING-STORAGE SECTION.
      * STANDARD SHARED WORKING STORAGE. SEE CABSWRK.
       COPY CABSWRK.
      * SHARED LAYOUT PULLED IN FOR THE REFERENCE SIDE.
       COPY CABSCIRC.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUXR16'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V3.16'.
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
       01  WS-PARM-CARD-R2 REDEFINES WS-PARM-CARD.
           05  PC2-LEAD                    PIC X(14).
           05  PC2-CYCLE-VIEW.
               10  PC2-CV-YY                   PIC 9(02).
               10  PC2-CV-DDD                  PIC 9(03).
           05  PC2-REST                    PIC X(61).
       01  WS-COUNT-AREA.
           05  WS-BQ-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BQ-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BQ-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BQ-CNT-04                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-BQ-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BQ-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BQ-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-BQ-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BQ-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-BQ-TXT-01                PIC X(12) VALUE SPACES.
           05  WS-BQ-TXT-02                PIC X(08) VALUE SPACES.
           05  WS-BQ-TXT-03                PIC X(12) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-BQ-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-BQ-ON-01                 VALUE 'Y'.
               88  WS-BQ-OFF-01                VALUE 'N'.
           05  WS-BQ-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-BQ-ON-02                 VALUE 'Y'.
               88  WS-BQ-OFF-02                VALUE 'N'.
           05  WS-BQ-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-BQ-ON-03                 VALUE 'Y'.
               88  WS-BQ-OFF-03                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-BQ-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-BQ-SUB-02                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. A ROW THAT DOES NOT FIT IS COUNTED AND DROPPED
      * - THE STEP DOES NOT FAIL.
       01  WS-BQ-TABLE.
           05  WS-BQ-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-BQ-TB-ENTRY OCCURS 50 TIMES
                                       INDEXED BY WS-BQ-IX.
               10  WS-BQ-TB-KEY                PIC X(06).
               10  WS-BQ-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-BQ-TB-TXT                PIC X(40).
               10  WS-BQ-TB-EFF                PIC 9(05).
               10  WS-BQ-TB-EXP                PIC 9(05).
       01  WS-BQ-WORK-GROUP-1.
           05  WS-BQ-G1-ACCOUNT            PIC 9(07).
           05  WS-BQ-G1-REGION             PIC 9(07).
           05  WS-BQ-G1-SEQ                PIC X(20).
           05  WS-BQ-G1-CODE               PIC 9(07).
           05  WS-BQ-G1-ELEM               PIC S9(09) COMP-3.
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 50.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUXR16 - ACCOUNT TO INVOICE CROSS REFERENCE'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-BQ-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-BQ-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9975.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-BQ-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-BQ-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT INVIN.
           IF WS-FS-INPUT NOT = '00'
               DISPLAY 'INVIN FILE STATUS = ' WS-FS-INPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF INVIN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT XRFOUT.
           IF WS-FS-OUTPUT NOT = '00'
               DISPLAY 'XRFOUT FILE STATUS = ' WS-FS-OUTPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF XRFOUT' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               DISPLAY 'CTLOUT FILE STATUS = ' WS-FS-CONTROL
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
           MOVE PC1-CYCLE-YYDDD TO WS-BQ-CYCLE-YYDDD.
           COMPUTE WS-BQ-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-BQ-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-BQ-CNT-01.
           MOVE 0 TO WS-BQ-CNT-03.
           MOVE 0 TO WS-BQ-CNT-02.
       P1200-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-BQ-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-BQ-TAB-CNT NOT < 50
               MOVE 'Y' TO WS-BQ-SW-01
               ADD 1 TO WS-BQ-CNT-03
           ELSE
               ADD 1 TO WS-BQ-TAB-CNT
               SET WS-BQ-IX TO WS-BQ-TAB-CNT
               MOVE IB-SOURCE TO WS-BQ-TB-KEY (WS-BQ-IX)
               MOVE 0 TO WS-BQ-TB-VAL (WS-BQ-IX)
               MOVE SPACES TO WS-BQ-TB-TXT (WS-BQ-IX)
               ADD 1 TO WS-BQ-CNT-01.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ INVIN
               AT END MOVE 'Y' TO WS-BQ-SW-01.
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
           PERFORM P2200-APPLY-REFERENCE THRU
               P2200-APPLY-REFERENCE-EXIT.
           PERFORM P2300-APPLY-REFERENCE THRU
               P2300-APPLY-REFERENCE-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ INVIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P2200-APPLY-REFERENCE.
           MOVE WS-BQ-AMT-01 TO WS-BQ-AMT-02.
           IF WS-BQ-AMT-02 < 0
               COMPUTE WS-BQ-AMT-02 = 0 - WS-BQ-AMT-01.
           MOVE 'N' TO WS-BQ-SW-02.
           IF WS-BQ-TAB-CNT > 0
               PERFORM P260-COMPARE-GROUP THRU P260-COMPARE-GROUP-EXIT
               VARYING WS-BQ-SUB-01 FROM 1 BY 1
               UNTIL WS-BQ-SUB-01 > WS-BQ-TAB-CNT
               OR WS-BQ-SW-02 = 'Y'.
       P2200-APPLY-REFERENCE-EXIT.
           EXIT.
       P2300-APPLY-REFERENCE.
           MOVE SPACES TO CABS-BQ-OUT-RECORD.
           MOVE IB-SOURCE TO OB-ELEM.
           MOVE IB-TYPE TO OB-LEVEL.
           MOVE IB-TYPE TO OB-PERIOD.
           MOVE IB-TYPE TO OB-SOURCE.
           WRITE CABS-BQ-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P2300-APPLY-REFERENCE-EXIT.
           EXIT.
       P260-COMPARE-GROUP.
           SET WS-BQ-IX TO WS-BQ-SUB-02.
           IF WS-BQ-TB-KEY (WS-BQ-IX) = IB-GROUP
               MOVE 'Y' TO WS-BQ-SW-01
               MOVE WS-BQ-TB-VAL (WS-BQ-IX) TO WS-BQ-QTY-01
               MOVE WS-BQ-SUB-02 TO WS-BQ-SUB-01.
       P260-COMPARE-GROUP-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-POST-ORPHAN.
           MOVE SPACES TO WS-BQ-TXT-01.
           STRING IB-CARRIER DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IB-TYPE DELIMITED BY SIZE
               INTO WS-BQ-TXT-01.
       P3100-POST-ORPHAN-EXIT.
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
           MOVE 6 TO CT-STEP-SEQ.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE WS-BQ-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE SPACES TO CT-FILLER.
           MOVE WS-BQ-TXT-01 TO CT-RESTART-KEY.
           MOVE 0 TO CT-RC.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
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
           CLOSE INVIN.
           CLOSE XRFOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABUXR16 - RUN COMPLETE'.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  BQ-CNT-02 = ' WS-BQ-CNT-02.
           DISPLAY '  BQ-CNT-03 = ' WS-BQ-CNT-03.
           DISPLAY '  BQ-CNT-04 = ' WS-BQ-CNT-04.
       P9000-EXIT.
           EXIT.
