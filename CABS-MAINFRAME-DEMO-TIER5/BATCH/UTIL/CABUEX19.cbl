      *****************************************************************
      * CABUEX19 - ADJUSTMENT EXTRACT FOR THE GENERAL LEDGER          *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               CIRIN   TELCABS.CABS.CIRIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               CIROUT  TELCABS.CABS.CIROUT         (LOCAL)     *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - NOTHING IS UPDATED IN PLACE        *
      * REVISION HISTORY                                              *
      *   V1.00  1988-03-02  D.OKONKWO    INITIAL RELEASE             *
      *   V1.01  1994-10-14  C.ADEYEMI    JOB PARAMETER MADE MANDATORY*
      *   V1.03  2006-10-05  C.ADEYEMI    RESTART KEY WRITTEN SO A    *
      *                      RERUN CAN POSITION                       *
      *   V1.04  2008-03-25  G.PRZYBYLSKI CARRIER TYPE BROUGHT ONTO   *
      *                      THE EXTRACT                              *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUEX19.
       AUTHOR. TELCABS APPLICATIONS - DATASET EXTRACT.
      *****************************************************************
      * ADJUSTMENT EXTRACT FOR THE GENERAL LEDGER. THE STEP IS        *
      * SCHEDULED MONTHLY AND ALSO RUN ON DEMAND WHEN A CENTRE ASKS   *
      * FOR IT.                                                       *
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
           SELECT CIROUT ASSIGN TO UT-S-CIROUT
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
      * CIRIN - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  CIRIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 90 CHARACTERS.
       01  CABS-AG-IN-RECORD.
           05  IA-TARIFF                   PIC 9(03).
           05  IA-CIRCUIT                  PIC X(08).
           05  IA-INVOICE                  PIC X(04).
           05  IA-MEDIA                    PIC S9(11)V9(05) COMP-3.
           05  IA-CENTRE                   PIC S9(15) COMP-3.
           05  IA-PERIOD                   PIC X(08).
           05  IA-INVOICE2                 PIC S9(07)V9(02) COMP-3.
           05  IA-CARRIER                  PIC X(08).
           05  IA-JURIS                    PIC S9(13)V9(02) COMP-3.
           05  IA-CLASS                    PIC S9(05) COMP-3.
           05  IA-CYCLE                    PIC S9(11)V9(02) COMP-3.
           05  IA-SOURCE                   PIC S9(09)V9(05) COMP-3.
           05  IA-CLASS2                   PIC S9(11) COMP-3.
           05  IA-CARRIER2                 PIC S9(07) COMP-3.
           05  AG-FILL-01                  PIC X(1).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-AG-VIEW1 REDEFINES CABS-AG-IN-RECORD.
           05  R0A-REGION                  PIC X(16).
           05  R0A-JURIS                   PIC X(08).
           05  R0A-JURIS2                  PIC X(08).
           05  R0A-CODE                    PIC X(10).
           05  R0A-ACCOUNT                 PIC X(10).
           05  R0A-STATE                   PIC X(02).
           05  R0A-REST                    PIC X(36).
      * CIROUT - WORK FILE, DELETED AT STEP END.
       FD  CIROUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-AG-OUT-RECORD.
           05  OA-CYCLE                    PIC S9(11) COMP-3.
           05  OA-SEGMENT                  PIC 9(09).
           05  OA-GROUP                    PIC S9(05) COMP-3.
           05  OA-TYPE                     PIC X(04).
           05  OA-BAN                      PIC S9(05) COMP-3.
           05  OA-TARGET                   PIC X(03).
           05  OA-TARGET2                  PIC X(04).
           05  AG-FILL-02                  PIC X(48).
      * SUSOUT - CATALOGUED GENERATION DATA GROUP.
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
      * SHARED LAYOUT PULLED IN FOR THE SELECTION SIDE.
       COPY CABSCDR.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUEX19'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V2.23'.
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
           05  WS-AG-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AG-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AG-CNT-03                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-AG-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AG-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AG-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-AG-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AG-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AG-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-AG-TXT-01                PIC X(10) VALUE SPACES.
           05  WS-AG-TXT-02                PIC X(20) VALUE SPACES.
           05  WS-AG-TXT-03                PIC X(08) VALUE SPACES.
           05  WS-AG-TXT-04                PIC X(08) VALUE SPACES.
           05  WS-AG-TXT-05                PIC X(12) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-AG-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-AG-ON-01                 VALUE 'Y'.
               88  WS-AG-OFF-01                VALUE 'N'.
           05  WS-AG-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-AG-ON-02                 VALUE 'Y'.
               88  WS-AG-OFF-02                VALUE 'N'.
           05  WS-AG-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-AG-ON-03                 VALUE 'Y'.
               88  WS-AG-OFF-03                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-AG-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-AG-SUB-02                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. A ROW THAT DOES NOT FIT IS COUNTED AND DROPPED
      * - THE STEP DOES NOT FAIL.
       01  WS-AG-TABLE.
           05  WS-AG-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-AG-TB-ENTRY OCCURS 120 TIMES
                                       INDEXED BY WS-AG-IX.
               10  WS-AG-TB-KEY                PIC X(13).
               10  WS-AG-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-AG-TB-TXT                PIC X(20).
               10  WS-AG-TB-EFF                PIC 9(05).
               10  WS-AG-TB-EXP                PIC 9(05).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 50.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUEX19 - ADJUSTMENT EXTRACT FOR THE GENERAL LED'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-AG-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-AG-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9984.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-AG-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-AG-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT CIRIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CIRIN NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CIROUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CIROUT NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SUSOUT.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'SUSOUT NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CTLOUT NOT AVAILABLE - OPEN REJECTED' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-AG-CYCLE-YYDDD.
           COMPUTE WS-AG-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-AG-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-AG-CNT-01.
           MOVE 0 TO WS-AG-CNT-02.
           MOVE 0 TO WS-AG-CNT-03.
       P1200-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-AG-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-AG-TAB-CNT NOT < 120
               MOVE 'Y' TO WS-AG-SW-01
               ADD 1 TO WS-AG-CNT-03
           ELSE
               ADD 1 TO WS-AG-TAB-CNT
               SET WS-AG-IX TO WS-AG-TAB-CNT
               MOVE IA-INVOICE2 TO WS-AG-TB-KEY (WS-AG-IX)
               MOVE 0 TO WS-AG-TB-VAL (WS-AG-IX)
               MOVE SPACES TO WS-AG-TB-TXT (WS-AG-IX)
               ADD 1 TO WS-AG-CNT-02.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ CIRIN
               AT END MOVE 'Y' TO WS-AG-SW-01.
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
           PERFORM P2200-RESOLVE-SUBSET THRU P2200-RESOLVE-SUBSET-EXIT.
           IF WS-AG-ON-01
               PERFORM P2300-CHECK-MASTER THRU P2300-CHECK-MASTER-EXIT.
           PERFORM P2400-RESOLVE-RANGE THRU P2400-RESOLVE-RANGE-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ CIRIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * A FAILURE HERE SUSPENDS THE RECORD, IT DOES NOT STOP THE STEP.
       P2200-RESOLVE-SUBSET.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-AG-TXT-03 TO PC-COL-001-020.
           MOVE WS-AG-TXT-01 TO PC-COL-021-060.
           MOVE WS-AG-AMT-03 TO WS-AG-AMT-EDIT.
           MOVE WS-AG-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
           MOVE 'N' TO WS-AG-SW-02.
           IF WS-AG-TAB-CNT > 0
               PERFORM P250-COMPARE-SUBSET THRU P250-COMPARE-SUBSET-EXIT
               VARYING WS-AG-SUB-01 FROM 1 BY 1
               UNTIL WS-AG-SUB-01 > WS-AG-TAB-CNT
               OR WS-AG-SW-02 = 'Y'.
       P2200-RESOLVE-SUBSET-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P2300-CHECK-MASTER.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-OCN-UNKNOWN TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-AG-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
       P2300-CHECK-MASTER-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P2400-RESOLVE-RANGE.
           IF WS-AG-AMT-03 NOT = 0
               COMPUTE WS-AG-QTY-02 = WS-AG-AMT-02 * 100 / WS-AG-AMT-03
           ELSE
               MOVE 0 TO WS-AG-QTY-02.
       P2400-RESOLVE-RANGE-EXIT.
           EXIT.
       P250-COMPARE-SUBSET.
           SET WS-AG-IX TO WS-AG-SUB-01.
           IF WS-AG-TB-KEY (WS-AG-IX) = IA-JURIS
               MOVE 'Y' TO WS-AG-SW-01
               MOVE WS-AG-TB-VAL (WS-AG-IX) TO WS-AG-QTY-02
               MOVE WS-AG-SUB-01 TO WS-AG-SUB-01.
       P250-COMPARE-SUBSET-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-WRITE-EXTRACT.
           MOVE IA-INVOICE2 TO WS-AG-TXT-05.
           MOVE IA-CARRIER2 TO WS-AG-TXT-05.
           MOVE IA-SOURCE TO WS-AG-TXT-05.
           MOVE IA-INVOICE TO WS-AG-TXT-02.
           ADD 1 TO WS-AG-CNT-03.
       P3100-WRITE-EXTRACT-EXIT.
           EXIT.
       P3200-RELEASE-SUBSET.
           MOVE SPACES TO CABS-AG-OUT-RECORD.
           MOVE IA-TARIFF TO OA-CYCLE.
           MOVE IA-TARIFF TO OA-SEGMENT.
           MOVE IA-INVOICE2 TO OA-GROUP.
           MOVE IA-CLASS TO OA-TYPE.
           MOVE IA-INVOICE TO OA-BAN.
           WRITE CABS-AG-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P3200-RELEASE-SUBSET-EXIT.
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
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE SPACES TO CT-FILLER.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE WS-AG-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE 3 TO CT-STEP-SEQ.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
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
           CLOSE CIRIN.
           CLOSE CIROUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABUEX19 - STEP COMPLETE'.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  AG-CNT-03 = ' WS-AG-CNT-03.
           DISPLAY '  AG-CNT-01 = ' WS-AG-CNT-01.
       P9000-EXIT.
           EXIT.
