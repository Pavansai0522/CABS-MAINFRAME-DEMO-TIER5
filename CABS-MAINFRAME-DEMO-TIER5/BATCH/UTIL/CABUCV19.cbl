      *****************************************************************
      * CABUCV19 - PACKED TO DISPLAY CONVERSION                       *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               CNVIN   TELCABS.CABS.CNVIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               REJOUT  TELCABS.CABS.REJOUT         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - NOTHING IS UPDATED IN PLACE        *
      * REVISION HISTORY                                              *
      *   V1.00  1993-03-03  B.R.HALVORSEN INITIAL RELEASE            *
      *   V1.04  1994-10-15  A.BUKOWSKI   RECOMPILE ONLY - COPYBOOK   *
      *                      CHANGE UPSTREAM                          *
      *   V1.05  2013-11-07  L.FERREIRA   HASH TOTAL ADDED TO THE     *
      *                      CONTROL RECORD                           *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUCV19.
       AUTHOR. TELCABS APPLICATIONS - FILE CONVERSION.
      *****************************************************************
      * PACKED TO DISPLAY CONVERSION. THE STEP IS SCHEDULED MONTHLY   *
      * AND ALSO RUN ON DEMAND WHEN A CENTRE ASKS FOR IT.             *
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT     *
      * PRECEDES THIS PROGRAM IN THE JOB.                             *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           C01 IS TO-NEW-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CNVIN ASSIGN TO UT-S-CNVIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT REJOUT ASSIGN TO UT-S-REJOUT
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
      * CNVIN - CATALOGUED GENERATION DATA GROUP.
       FD  CNVIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-AL-IN-RECORD.
           05  IA-STATUS                   PIC X(16).
           05  IA-BAN                      PIC S9(05) COMP-3.
           05  IA-CLASS                    PIC X(03).
           05  IA-SEQ                      PIC 9(03).
           05  IA-CLASS2                   PIC X(02).
           05  IA-STATUS2                  PIC X(04).
           05  IA-PERIOD                   PIC S9(05) COMP-3.
           05  IA-TARGET                   PIC 9(04).
           05  IA-CIRCUIT                  PIC X(04).
           05  IA-INVOICE                  PIC 9(06).
           05  IA-LEVEL                    PIC 9(06).
           05  AL-FILL-01                  PIC X(26).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-AL-VIEW1 REDEFINES CABS-AL-IN-RECORD.
           05  R0A-TARIFF                  PIC X(10).
           05  R0A-LEVEL                   PIC S9(11)V9(02) COMP-3.
           05  R0A-CODE                    PIC S9(07) COMP-3.
           05  R0A-JURIS                   PIC 9(05).
           05  R0A-GROUP                   PIC S9(09)V9(05) COMP-3.
           05  R0A-REST                    PIC X(46).
      * REJOUT - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  REJOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-AL-OUT-RECORD.
           05  OA-CENTRE                   PIC X(13).
           05  OA-GROUP                    PIC 9(06).
           05  OA-TARGET                   PIC X(04).
           05  OA-TARGET2                  PIC S9(07) COMP-3.
           05  OA-TARIFF                   PIC X(10).
           05  OA-CARRIER                  PIC S9(13) COMP-3.
           05  OA-INVOICE                  PIC S9(13)V9(02) COMP-3.
           05  OA-GROUP2                   PIC S9(09) COMP-3.
           05  AL-FILL-02                  PIC X(23).
      * SUSOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
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
      * RPTOUT - WORK FILE, DELETED AT STEP END.
       FD  RPTOUT
           RECORDING MODE IS FA
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       COPY CABSPRNT.
       WORKING-STORAGE SECTION.
      * STANDARD SHARED WORKING STORAGE. SEE CABSWRK.
       COPY CABSWRK.
      * SHARED LAYOUT PULLED IN FOR THE ZONE SIDE.
       COPY CABSBILL.
      * SHARED LAYOUT PULLED IN FOR THE PACKED SIDE.
       COPY CABSCOMM.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUCV19'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V1.15'.
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
       01  WS-PARM-CARD-R2 REDEFINES WS-PARM-CARD.
           05  PC2-LEAD                    PIC X(14).
           05  PC2-CYCLE-VIEW.
               10  PC2-CV-YY                   PIC 9(02).
               10  PC2-CV-DDD                  PIC 9(03).
           05  PC2-REST                    PIC X(61).
       01  WS-COUNT-AREA.
           05  WS-AL-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AL-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AL-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AL-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AL-CNT-05                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-AL-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AL-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AL-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-AL-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AL-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AL-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-AL-TXT-01                PIC X(30) VALUE SPACES.
           05  WS-AL-TXT-02                PIC X(10) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-AL-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-AL-ON-01                 VALUE 'Y'.
               88  WS-AL-OFF-01                VALUE 'N'.
           05  WS-AL-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-AL-ON-02                 VALUE 'Y'.
               88  WS-AL-OFF-02                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-AL-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-AL-SUB-02                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. LOADED ONCE AT INITIALISATION AND SEARCHED WITH
      * A SUBSCRIPTED WALK.
       01  WS-AL-TABLE.
           05  WS-AL-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-AL-TB-ENTRY OCCURS 100 TIMES
                                       INDEXED BY WS-AL-IX.
               10  WS-AL-TB-KEY                PIC X(04).
               10  WS-AL-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-AL-TB-TXT                PIC X(30).
               10  WS-AL-TB-EFF                PIC 9(05).
               10  WS-AL-TB-EXP                PIC 9(05).
       01  WS-AL-WORK-GROUP-1.
           05  WS-AL-G1-SEGMENT            PIC X(20).
           05  WS-AL-G1-CENTRE             PIC X(20).
           05  WS-AL-G1-GROUP              PIC 9(07).
           05  WS-AL-G1-CYCLE              PIC 9(05).
           05  WS-AL-G1-TYPE               PIC X(20).
           05  WS-AL-G1-CYCLE              PIC X(20).
           05  WS-AL-G1-INVOICE            PIC S9(11)V9(02) COMP-3.
           05  WS-AL-G1-CLASS              PIC S9(09) COMP-3.
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 60.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUCV19 - PACKED TO DISPLAY CONVERSION'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-AL-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-AL-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9981.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-AL-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-AL-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT CNVIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON CNVIN - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT REJOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON REJOUT - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SUSOUT.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON SUSOUT - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON CTLOUT - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RPTOUT.
           IF WS-FS-OUTPUT NOT = '00'
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
           MOVE PC1-CYCLE-YYDDD TO WS-AL-CYCLE-YYDDD.
           COMPUTE WS-AL-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-AL-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-AL-CNT-04.
           MOVE 0 TO WS-AL-CNT-05.
           MOVE 0 TO WS-AL-CNT-01.
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
           PERFORM P2200-SELECT-RECORD THRU P2200-SELECT-RECORD-EXIT.
           PERFORM P2300-SELECT-CENTURY THRU P2300-SELECT-CENTURY-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ CNVIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P2200-SELECT-RECORD.
           MOVE SPACES TO CABS-AL-OUT-RECORD.
           MOVE IA-CLASS2 TO OA-CENTRE.
           MOVE IA-CIRCUIT TO OA-GROUP.
           MOVE IA-CLASS TO OA-TARGET.
           MOVE IA-TARGET TO OA-TARGET2.
           MOVE IA-SEQ TO OA-TARIFF.
           MOVE IA-SEQ TO OA-CARRIER.
           MOVE IA-CLASS2 TO OA-INVOICE.
           MOVE IA-STATUS TO OA-GROUP2.
           WRITE CABS-AL-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P2200-SELECT-RECORD-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P2300-SELECT-CENTURY.
           MOVE 0 TO WS-AL-CNT-03.
           INSPECT WS-AL-TXT-02 TALLYING WS-AL-CNT-03
               FOR ALL SPACES.
           INSPECT WS-AL-TXT-02 REPLACING ALL LOW-VALUES BY SPACES.
       P2300-SELECT-CENTURY-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-CLOSE-OFF-PACKED.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-OCN-UNKNOWN TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-AL-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
           MOVE CABS-SUSPENSE-RECORD TO CABS-SUSOUT-RECORD.
           WRITE CABS-SUSOUT-RECORD.
       P3100-CLOSE-OFF-PACKED-EXIT.
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
           MOVE WS-SUMM-CNT TO WS-AL-CNT-EDIT.
           MOVE WS-AL-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS CARRIED FWD' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-AL-CNT-EDIT.
           MOVE WS-AL-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS WRITTEN' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-AL-CNT-EDIT.
           MOVE WS-AL-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS REJECTED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-AL-CNT-EDIT.
           MOVE WS-AL-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS READ' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO WS-AL-CNT-EDIT.
           MOVE WS-AL-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-AL-CNT-01 TO WS-AL-CNT-EDIT.
           MOVE WS-AL-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 02' TO PC-COL-001-020.
           MOVE WS-AL-CNT-02 TO WS-AL-CNT-EDIT.
           MOVE WS-AL-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE 5 TO CT-STEP-SEQ.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE WS-AL-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE WS-AL-CNT-05 TO CT-RC.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE SPACES TO CT-FILLER.
           MOVE SPACES TO CT-ABEND-CD.
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
           CLOSE CNVIN.
           CLOSE REJOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABUCV19 - NORMAL END OF JOB'.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  AL-CNT-05 = ' WS-AL-CNT-05.
           DISPLAY '  AL-CNT-02 = ' WS-AL-CNT-02.
       P9000-EXIT.
           EXIT.
