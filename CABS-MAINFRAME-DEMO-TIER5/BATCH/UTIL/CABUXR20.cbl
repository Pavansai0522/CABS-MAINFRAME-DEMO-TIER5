      *****************************************************************
      * CABUXR20 - ORPHAN KEY CROSS REFERENCE                         *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               SUSIN   TELCABS.CABS.SUSIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               PAIROUT TELCABS.CABS.PAIROU         (LOCAL)     *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - THE OUTPUT IS REBUILT EACH RUN     *
      * REVISION HISTORY                                              *
      *   V1.00  1990-09-12  W.J.MCALLISTER INITIAL RELEASE           *
      *   V1.01  2006-12-24  W.J.MCALLISTER REGION SIZE REDUCED -     *
      *                      TABLE MOVED OUT OF WORKING STORAGE       *
      *   V1.03  2010-05-25  B.R.HALVORSEN OCCURS RAISED AFTER THE    *
      *                      FEBRUARY OVERFLOW                        *
      *   V1.06  2017-02-09  S.MARCHETTI  REPORT PAGINATION CORRECTED *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUXR20.
       AUTHOR. TELCABS APPLICATIONS - CROSS REFERENCE REPORT.
      *****************************************************************
      * ORPHAN KEY CROSS REFERENCE. THIS STEP IS SCHEDULED INSIDE THE *
      * NIGHTLY ACCESS BILLING STREAM AND HAS NO INTERACTIVE ENTRY    *
      * POINT.                                                        *
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS*
      * BUILT ON THE SAME ORDER.                                      *
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
           SELECT PAIROUT ASSIGN TO UT-S-PAIROUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT CTLOUT ASSIGN TO UT-S-CTLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      * SUSIN - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  SUSIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 100 CHARACTERS.
       01  CABS-EC-IN-RECORD.
           05  IE-CENTRE                   PIC S9(07)V9(02) COMP-3.
           05  IE-REGION                   PIC S9(13)V9(02) COMP-3.
           05  IE-INVOICE                  PIC S9(09)V9(02) COMP-3.
           05  IE-TYPE                     PIC 9(07).
           05  IE-CODE                     PIC 9(09).
           05  IE-CIRCUIT                  PIC 9(06).
           05  IE-BAND                     PIC S9(13)V9(02) COMP-3.
           05  IE-SEQ                      PIC X(13).
           05  IE-STATE                    PIC S9(07) COMP-3.
           05  IE-SEQ2                     PIC X(08).
           05  IE-LEVEL                    PIC S9(13)V9(05) COMP-3.
           05  IE-INVOICE2                 PIC S9(11)V9(05) COMP-3.
           05  EC-FILL-01                  PIC X(7).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-EC-VIEW1 REDEFINES CABS-EC-IN-RECORD.
           05  R0E-STATE                   PIC S9(15) COMP-3.
           05  R0E-TARIFF                  PIC S9(11)V9(02) COMP-3.
           05  R0E-CYCLE                   PIC S9(11)V9(02) COMP-3.
           05  R0E-STATUS                  PIC S9(09)V9(05) COMP-3.
           05  R0E-REST                    PIC X(70).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-EC-VIEW2 REDEFINES CABS-EC-IN-RECORD.
           05  R1E-CARRIER                 PIC S9(11) COMP-3.
           05  R1E-SEGMENT                 PIC X(10).
           05  R1E-PERIOD                  PIC X(08).
           05  R1E-STATUS                  PIC 9(05).
           05  R1E-STATUS2                 PIC S9(13) COMP-3.
           05  R1E-INVOICE                 PIC X(03).
           05  R1E-REST                    PIC X(61).
      * PAIROUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  PAIROUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-EC-OUT-RECORD.
           05  OE-TARGET                   PIC S9(09) COMP-3.
           05  OE-PERIOD                   PIC X(03).
           05  OE-TARGET2                  PIC X(03).
           05  OE-BAN                      PIC 9(06).
           05  OE-CARRIER                  PIC X(08).
           05  OE-PERIOD2                  PIC S9(09)V9(02) COMP-3.
           05  OE-STATE                    PIC 9(06).
           05  OE-BAND                     PIC X(16).
           05  OE-CODE                     PIC X(02).
           05  OE-MEDIA                    PIC X(06).
           05  EC-FILL-02                  PIC X(19).
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
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUXR20'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V2.25'.
           05  WS-TABLE-LIMIT              PIC S9(04) COMP-3
                                         VALUE 150.
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
           05  WS-EC-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-EC-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-EC-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-EC-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-EC-CNT-05                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-EC-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-EC-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-EC-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-EC-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-EC-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-EC-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-EC-TXT-01                PIC X(16) VALUE SPACES.
           05  WS-EC-TXT-02                PIC X(12) VALUE SPACES.
           05  WS-EC-TXT-03                PIC X(30) VALUE SPACES.
           05  WS-EC-TXT-04                PIC X(20) VALUE SPACES.
           05  WS-EC-TXT-05                PIC X(30) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-EC-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-EC-ON-01                 VALUE 'Y'.
               88  WS-EC-OFF-01                VALUE 'N'.
           05  WS-EC-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-EC-ON-02                 VALUE 'Y'.
               88  WS-EC-OFF-02                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-EC-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-EC-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-EC-SUB-03                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. LOADED ONCE AT INITIALISATION AND SEARCHED WITH
      * A SUBSCRIPTED WALK.
       01  WS-EC-TABLE.
           05  WS-EC-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-EC-TB-ENTRY OCCURS 150 TIMES
                                       INDEXED BY WS-EC-IX.
               10  WS-EC-TB-KEY                PIC X(13).
               10  WS-EC-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-EC-TB-TXT                PIC X(30).
               10  WS-EC-TB-EFF                PIC 9(05).
               10  WS-EC-TB-EXP                PIC 9(05).
       01  WS-EC-WORK-GROUP-1.
           05  WS-EC-G1-MEDIA              PIC X(20).
           05  WS-EC-G1-BAND               PIC S9(11)V9(02) COMP-3.
           05  WS-EC-G1-CYCLE              PIC 9(07).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 60.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUXR20 - ORPHAN KEY CROSS REFERENCE'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-EC-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-EC-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9945.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-EC-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-EC-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT SUSIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF SUSIN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT PAIROUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF PAIROUT' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
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
           MOVE PC1-CYCLE-YYDDD TO WS-EC-CYCLE-YYDDD.
           COMPUTE WS-EC-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-EC-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-EC-CNT-05.
           MOVE 0 TO WS-EC-CNT-01.
           MOVE 0 TO WS-EC-CNT-03.
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
           PERFORM P2200-SELECT-MATCH THRU P2200-SELECT-MATCH-EXIT.
           PERFORM P2300-EDIT-ORPHAN THRU P2300-EDIT-ORPHAN-EXIT.
           PERFORM P2400-APPLY-MATCH THRU P2400-APPLY-MATCH-EXIT.
           PERFORM P2500-BUILD-SIDE THRU P2500-BUILD-SIDE-EXIT.
           PERFORM P2600-EXPAND-LINK THRU P2600-EXPAND-LINK-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ SUSIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P2200-SELECT-MATCH.
           CALL 'CABHASH' USING IE-REGION WS-ACC-OCN-HASH.
           ADD WS-EC-CNT-01 TO WS-ACC-SEQ-HASH.
       P2200-SELECT-MATCH-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P2300-EDIT-ORPHAN.
           IF WS-EC-AMT-02 < 28
               MOVE 28 TO WS-EC-AMT-02
               ADD 1 TO WS-EC-CNT-02.
           IF WS-EC-AMT-02 > 58145
               MOVE 58145 TO WS-EC-AMT-02
               ADD 1 TO WS-EC-CNT-05.
       P2300-EDIT-ORPHAN-EXIT.
           EXIT.
       P2400-APPLY-MATCH.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-DUP-SEQ TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-EC-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
       P2400-APPLY-MATCH-EXIT.
           EXIT.
       P2500-BUILD-SIDE.
           MOVE 'Y' TO WS-EC-SW-01.
           IF IE-CENTRE < 29
               MOVE 'N' TO WS-EC-SW-01
               ADD 1 TO WS-EC-CNT-03.
           IF IE-CENTRE > 1704
               MOVE 'N' TO WS-EC-SW-01
               ADD 1 TO WS-EC-CNT-01.
       P2500-BUILD-SIDE-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P2600-EXPAND-LINK.
           MOVE SPACES TO CABS-EC-OUT-RECORD.
           MOVE IE-LEVEL TO OE-TARGET.
           MOVE IE-LEVEL TO OE-PERIOD.
           MOVE IE-CODE TO OE-TARGET2.
           MOVE IE-SEQ TO OE-BAN.
           MOVE IE-STATE TO OE-CARRIER.
           WRITE CABS-EC-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P2600-EXPAND-LINK-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-POST-SIDE.
           CALL 'CABHASH' USING IE-CIRCUIT WS-ACC-OCN-HASH.
           ADD WS-EC-CNT-05 TO WS-ACC-SEQ-HASH.
       P3100-POST-SIDE-EXIT.
           EXIT.
       P3200-FORMAT-GROUP.
           ADD IE-INVOICE2 TO WS-EC-QTY-01.
           COMPUTE WS-EC-AMT-03 = WS-EC-QTY-01 * WS-EC-QTY-02.
           ADD WS-EC-AMT-03 TO WS-EC-AMT-04.
       P3200-FORMAT-GROUP-EXIT.
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
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE SPACES TO CT-FILLER.
           MOVE WS-EC-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE 4 TO CT-STEP-SEQ.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
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
           CLOSE SUSIN.
           CLOSE PAIROUT.
           CLOSE CTLOUT.
           DISPLAY 'CABUXR20 - RUN COMPLETE'.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  EC-CNT-01 = ' WS-EC-CNT-01.
           DISPLAY '  EC-CNT-04 = ' WS-EC-CNT-04.
       P9000-EXIT.
           EXIT.
