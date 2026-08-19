      *****************************************************************
      * CABUEX06 - CONTROL RECORD EXTRACT                             *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               ADJIN   TELCABS.CABS.ADJIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               CIROUT  TELCABS.CABS.CIROUT         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * REVISION HISTORY                                              *
      *   V1.00  1990-12-16  K.O.BRIEN    INITIAL RELEASE             *
      *   V1.04  2003-02-23  A.BUKOWSKI   CARRIER TYPE BROUGHT ONTO   *
      *                      THE EXTRACT                              *
      *   V1.07  2004-11-26  K.O.BRIEN    PRINT LINE WIDENED TO 133   *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUEX06.
       AUTHOR. TELCABS APPLICATIONS - DATASET EXTRACT.
      *****************************************************************
      * CONTROL RECORD EXTRACT. THE STEP IS DRIVEN ENTIRELY FROM THE  *
      * SYSIN PARM CARD AND THE DD ALLOCATIONS IN THE JOB.            *
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
           SELECT ADJIN ASSIGN TO UT-S-ADJIN
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
      * ADJIN - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  ADJIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-DF-IN-RECORD.
           05  ID-GROUP                    PIC X(10).
           05  ID-SOURCE                   PIC X(08).
           05  ID-CODE                     PIC X(06).
           05  ID-ACCOUNT                  PIC S9(09)V9(02) COMP-3.
           05  ID-STATE                    PIC 9(07).
           05  ID-GROUP2                   PIC X(16).
           05  ID-BAN                      PIC X(06).
           05  ID-LEVEL                    PIC 9(02).
           05  ID-GROUP3                   PIC X(02).
           05  ID-CODE2                    PIC X(16).
           05  DF-FILL-01                  PIC X(1).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-DF-VIEW1 REDEFINES CABS-DF-IN-RECORD.
           05  R0D-STATE                   PIC S9(13)V9(05) COMP-3.
           05  R0D-GROUP                   PIC 9(03).
           05  R0D-JURIS                   PIC 9(07).
           05  R0D-ACCOUNT                 PIC X(03).
           05  R0D-CLASS                   PIC X(13).
           05  R0D-INVOICE                 PIC X(04).
           05  R0D-REST                    PIC X(40).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-DF-VIEW2 REDEFINES CABS-DF-IN-RECORD.
           05  R1D-GROUP                   PIC X(16).
           05  R1D-TARGET                  PIC X(06).
           05  R1D-STATUS                  PIC X(03).
           05  R1D-SOURCE                  PIC S9(07) COMP-3.
           05  R1D-SEGMENT                 PIC S9(13) COMP-3.
           05  R1D-JURIS                   PIC X(08).
           05  R1D-REST                    PIC X(36).
      * CIROUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  CIROUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-DF-OUT-RECORD.
           05  OD-OCN                      PIC 9(03).
           05  OD-MEDIA                    PIC X(02).
           05  OD-BAND                     PIC X(03).
           05  OD-BAND2                    PIC S9(15) COMP-3.
           05  OD-REGION                   PIC S9(13)V9(05) COMP-3.
           05  OD-CIRCUIT                  PIC S9(11) COMP-3.
           05  OD-SOURCE                   PIC S9(13) COMP-3.
           05  OD-BAND3                    PIC S9(07) COMP-3.
           05  OD-JURIS                    PIC X(20).
           05  DF-FILL-02                  PIC X(17).
      * CTLOUT - WORK FILE, DELETED AT STEP END.
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
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUEX06'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V3.24'.
           05  WS-TABLE-LIMIT              PIC S9(04) COMP-3
                                         VALUE 80.
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
           05  WS-DF-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DF-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DF-CNT-03                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-DF-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DF-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DF-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-DF-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DF-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-DF-TXT-01                PIC X(20) VALUE SPACES.
           05  WS-DF-TXT-02                PIC X(12) VALUE SPACES.
           05  WS-DF-TXT-03                PIC X(08) VALUE SPACES.
           05  WS-DF-TXT-04                PIC X(30) VALUE SPACES.
           05  WS-DF-TXT-05                PIC X(30) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-DF-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-DF-ON-01                 VALUE 'Y'.
               88  WS-DF-OFF-01                VALUE 'N'.
           05  WS-DF-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-DF-ON-02                 VALUE 'Y'.
               88  WS-DF-OFF-02                VALUE 'N'.
           05  WS-DF-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-DF-ON-03                 VALUE 'Y'.
               88  WS-DF-OFF-03                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-DF-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DF-SUB-02                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. THE LIMIT WAS RAISED WHEN THE SOUTHEAST CENTRES
      * WERE MERGED AND HAS NOT BEEN REVIEWED SINCE.
       01  WS-DF-TABLE.
           05  WS-DF-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-DF-TB-ENTRY OCCURS 80 TIMES
                                       INDEXED BY WS-DF-IX.
               10  WS-DF-TB-KEY                PIC X(10).
               10  WS-DF-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-DF-TB-TXT                PIC X(30).
               10  WS-DF-TB-EFF                PIC 9(05).
               10  WS-DF-TB-EXP                PIC 9(05).
       01  WS-DF-WORK-GROUP-1.
           05  WS-DF-G1-MEDIA              PIC S9(09) COMP-3.
           05  WS-DF-G1-ACCOUNT            PIC X(10).
           05  WS-DF-G1-ACCOUNT            PIC X(20).
           05  WS-DF-G1-CODE               PIC X(10).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 55.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUEX06 - CONTROL RECORD EXTRACT'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-DF-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-DF-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9954.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-DF-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-DF-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT ADJIN.
           IF WS-FS-INPUT NOT = '00'
               DISPLAY 'ADJIN FILE STATUS = ' WS-FS-INPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'ADJIN OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CIROUT.
           IF WS-FS-OUTPUT NOT = '00'
               DISPLAY 'CIROUT FILE STATUS = ' WS-FS-OUTPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CIROUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               DISPLAY 'CTLOUT FILE STATUS = ' WS-FS-CONTROL
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CTLOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RPTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               DISPLAY 'RPTOUT FILE STATUS = ' WS-FS-OUTPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RPTOUT OPEN FAILED - FILE STATUS BAD' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-DF-CYCLE-YYDDD.
           COMPUTE WS-DF-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-DF-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-DF-CNT-01.
           MOVE 0 TO WS-DF-CNT-02.
           MOVE 0 TO WS-DF-CNT-03.
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
           IF WS-DF-ON-03
               PERFORM P2200-CONVERT-MASTER THRU
                   P2200-CONVERT-MASTER-EXIT.
           PERFORM P2300-SPLIT-SELECTION THRU
               P2300-SPLIT-SELECTION-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ ADJIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P2200-CONVERT-MASTER.
           MOVE 'N' TO WS-DF-SW-03.
           IF WS-DF-TXT-03 NOT = WS-DF-TXT-04
               MOVE 'Y' TO WS-DF-SW-03
               MOVE WS-DF-TXT-03 TO WS-DF-TXT-04
               ADD 1 TO WS-DF-CNT-02.
       P2200-CONVERT-MASTER-EXIT.
           EXIT.
       P2300-SPLIT-SELECTION.
           MOVE 0 TO WS-DF-CNT-03.
           INSPECT WS-DF-TXT-01 TALLYING WS-DF-CNT-03
               FOR ALL SPACES.
           INSPECT WS-DF-TXT-01 REPLACING ALL LOW-VALUES BY SPACES.
       P2300-SPLIT-SELECTION-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P3100-FORMAT-RANGE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-DF-TXT-02 TO PC-COL-001-020.
           MOVE WS-DF-TXT-04 TO PC-COL-021-060.
           MOVE WS-DF-AMT-01 TO WS-DF-AMT-EDIT.
           MOVE WS-DF-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P3100-FORMAT-RANGE-EXIT.
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
           MOVE WS-READ-CNT TO WS-DF-CNT-EDIT.
           MOVE WS-DF-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL OUT' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-DF-CNT-EDIT.
           MOVE WS-DF-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL SUSPENDED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-DF-CNT-EDIT.
           MOVE WS-DF-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL SUMMARISED' TO PC-COL-001-020.
           MOVE WS-SUMM-CNT TO WS-DF-CNT-EDIT.
           MOVE WS-DF-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL CARRIED FWD' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-DF-CNT-EDIT.
           MOVE WS-DF-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-DF-CNT-01 TO WS-DF-CNT-EDIT.
           MOVE WS-DF-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE 0 TO CT-RC.
           MOVE WS-DF-TXT-02 TO CT-RESTART-KEY.
           MOVE SPACES TO CT-FILLER.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE 2 TO CT-STEP-SEQ.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE WS-DF-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
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
           CLOSE ADJIN.
           CLOSE CIROUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABUEX06 - RUN COMPLETE'.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  DF-CNT-02 = ' WS-DF-CNT-02.
           DISPLAY '  DF-CNT-01 = ' WS-DF-CNT-01.
           DISPLAY '  DF-CNT-03 = ' WS-DF-CNT-03.
       P9000-EXIT.
           EXIT.
