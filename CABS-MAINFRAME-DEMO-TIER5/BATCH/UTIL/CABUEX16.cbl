      *****************************************************************
      * CABUEX16 - BILLED ACCOUNT EXTRACT                             *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               EXTIN   TELCABS.CABS.EXTIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               EXTOUT  TELCABS.CABS.EXTOUT         (LOCAL)     *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * REVISION HISTORY                                              *
      *   V1.00  1994-08-23  B.R.HALVORSEN INITIAL RELEASE            *
      *   V1.04  2005-06-25  B.R.HALVORSEN HASH TOTAL ADDED TO THE    *
      *                      CONTROL RECORD                           *
      *   V1.05  2018-04-07  M.DELACROIX  EFFECTIVE DATE FILTER ADDED *
      *                      PER AUDIT FINDING                        *
      *   V1.07  2019-03-11  K.O.BRIEN    RECOMPILE ONLY - COPYBOOK   *
      *                      CHANGE UPSTREAM                          *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUEX16.
       AUTHOR. TELCABS APPLICATIONS - DATASET EXTRACT.
      *****************************************************************
      * BILLED ACCOUNT EXTRACT. THIS STEP IS SCHEDULED INSIDE THE     *
      * NIGHTLY ACCESS BILLING STREAM AND HAS NO INTERACTIVE ENTRY    *
      * POINT.                                                        *
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
           SELECT EXTIN ASSIGN TO UT-S-EXTIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT EXTOUT ASSIGN TO UT-S-EXTOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT CTLOUT ASSIGN TO UT-S-CTLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      * EXTIN - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  EXTIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 90 CHARACTERS.
       01  CABS-CZ-IN-RECORD.
           05  IC-TYPE                     PIC S9(13)V9(02) COMP-3.
           05  IC-BAN                      PIC S9(13)V9(05) COMP-3.
           05  IC-INVOICE                  PIC X(10).
           05  IC-CYCLE                    PIC X(03).
           05  IC-CYCLE2                   PIC S9(11)V9(05) COMP-3.
           05  IC-OCN                      PIC S9(13)V9(02) COMP-3.
           05  IC-CYCLE3                   PIC X(03).
           05  IC-STATUS                   PIC 9(04).
           05  IC-STATUS2                  PIC X(16).
           05  IC-OCN2                     PIC S9(11)V9(02) COMP-3.
           05  IC-TYPE2                    PIC 9(02).
           05  CZ-FILL-01                  PIC X(10).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-CZ-VIEW1 REDEFINES CABS-CZ-IN-RECORD.
           05  R0C-REGION                  PIC X(20).
           05  R0C-JURIS                   PIC X(20).
           05  R0C-REGION2                 PIC S9(13)V9(02) COMP-3.
           05  R0C-CLASS                   PIC X(03).
           05  R0C-REST                    PIC X(39).
      * EXTOUT - CATALOGUED GENERATION DATA GROUP.
       FD  EXTOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 100 CHARACTERS.
       01  CABS-CZ-OUT-RECORD.
           05  OC-TYPE                     PIC S9(15) COMP-3.
           05  OC-ACCOUNT                  PIC X(16).
           05  OC-OCN                      PIC S9(07) COMP-3.
           05  OC-CENTRE                   PIC S9(11)V9(02) COMP-3.
           05  OC-STATUS                   PIC X(02).
           05  OC-JURIS                    PIC X(08).
           05  OC-CIRCUIT                  PIC X(04).
           05  OC-TYPE2                    PIC S9(07)V9(02) COMP-3.
           05  OC-MEDIA                    PIC 9(06).
           05  OC-BAND                     PIC X(16).
           05  OC-TARGET                   PIC 9(09).
           05  OC-BAN                      PIC X(08).
           05  CZ-FILL-02                  PIC X(7).
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
      * SHARED LAYOUT PULLED IN FOR THE MASTER SIDE.
       COPY CABSCIRC.
      * SHARED LAYOUT PULLED IN FOR THE MASTER SIDE.
       COPY CABSCDR.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUEX16'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V3.28'.
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
       01  WS-COUNT-AREA.
           05  WS-CZ-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CZ-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CZ-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CZ-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CZ-CNT-05                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-CZ-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CZ-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CZ-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-CZ-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CZ-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-CZ-TXT-01                PIC X(16) VALUE SPACES.
           05  WS-CZ-TXT-02                PIC X(12) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-CZ-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-CZ-ON-01                 VALUE 'Y'.
               88  WS-CZ-OFF-01                VALUE 'N'.
           05  WS-CZ-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-CZ-ON-02                 VALUE 'Y'.
               88  WS-CZ-OFF-02                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-CZ-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CZ-SUB-02                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. THE LIMIT WAS RAISED WHEN THE SOUTHEAST CENTRES
      * WERE MERGED AND HAS NOT BEEN REVIEWED SINCE.
       01  WS-CZ-TABLE.
           05  WS-CZ-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-CZ-TB-ENTRY OCCURS 150 TIMES
                                       INDEXED BY WS-CZ-IX.
               10  WS-CZ-TB-KEY                PIC X(13).
               10  WS-CZ-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-CZ-TB-TXT                PIC X(30).
               10  WS-CZ-TB-EFF                PIC 9(05).
               10  WS-CZ-TB-EXP                PIC 9(05).
       01  WS-CZ-WORK-GROUP-1.
           05  WS-CZ-G1-CENTRE             PIC 9(07).
           05  WS-CZ-G1-CYCLE              PIC X(10).
           05  WS-CZ-G1-CODE               PIC S9(11)V9(02) COMP-3.
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 50.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUEX16 - BILLED ACCOUNT EXTRACT'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-CZ-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-CZ-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9947.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-CZ-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-CZ-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT EXTIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF EXTIN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT EXTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF EXTOUT' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-CZ-CYCLE-YYDDD.
           COMPUTE WS-CZ-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-CZ-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-CZ-CNT-02.
           MOVE 0 TO WS-CZ-CNT-01.
           MOVE 0 TO WS-CZ-CNT-03.
       P1200-EXIT.
           EXIT.
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT
      * PRECEDES THIS PROGRAM IN THE JOB.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-CZ-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-CZ-TAB-CNT NOT < 150
               MOVE 'Y' TO WS-CZ-SW-01
               ADD 1 TO WS-CZ-CNT-01
           ELSE
               ADD 1 TO WS-CZ-TAB-CNT
               SET WS-CZ-IX TO WS-CZ-TAB-CNT
               MOVE IC-STATUS2 TO WS-CZ-TB-KEY (WS-CZ-IX)
               MOVE 0 TO WS-CZ-TB-VAL (WS-CZ-IX)
               MOVE SPACES TO WS-CZ-TB-TXT (WS-CZ-IX)
               ADD 1 TO WS-CZ-CNT-04.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ EXTIN
               AT END MOVE 'Y' TO WS-CZ-SW-01.
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
           IF WS-CZ-ON-02
               PERFORM P2200-CONVERT-EXTRACT THRU
                   P2200-CONVERT-EXTRACT-EXIT.
           PERFORM P2300-CONVERT-CANDIDATE THRU
               P2300-CONVERT-CANDIDATE-EXIT.
           PERFORM P2400-MATCH-SUBSET THRU P2400-MATCH-SUBSET-EXIT.
           PERFORM P2500-DERIVE-FILTER THRU P2500-DERIVE-FILTER-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ EXTIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P2200-CONVERT-EXTRACT.
           MOVE 0 TO WS-CZ-CNT-04.
           INSPECT WS-CZ-TXT-01 TALLYING WS-CZ-CNT-04
               FOR ALL SPACES.
           INSPECT WS-CZ-TXT-01 REPLACING ALL LOW-VALUES BY SPACES.
           MOVE 'N' TO WS-CZ-SW-02.
           IF WS-CZ-TAB-CNT > 0
               PERFORM P280-COMPARE-SUBSET THRU P280-COMPARE-SUBSET-EXIT
               VARYING WS-CZ-SUB-01 FROM 1 BY 1
               UNTIL WS-CZ-SUB-01 > WS-CZ-TAB-CNT
               OR WS-CZ-SW-02 = 'Y'.
       P2200-CONVERT-EXTRACT-EXIT.
           EXIT.
      * A FAILURE HERE SUSPENDS THE RECORD, IT DOES NOT STOP THE STEP.
       P2300-CONVERT-CANDIDATE.
           MOVE IC-INVOICE TO WS-CZ-TXT-01.
           MOVE IC-OCN TO WS-CZ-TXT-02.
           ADD 1 TO WS-CZ-CNT-01.
       P2300-CONVERT-CANDIDATE-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P2400-MATCH-SUBSET.
           IF WS-CZ-AMT-02 NOT = 0
               COMPUTE WS-CZ-QTY-02 = WS-CZ-AMT-01 * 100 / WS-CZ-AMT-02
           ELSE
               MOVE 0 TO WS-CZ-QTY-02.
       P2400-MATCH-SUBSET-EXIT.
           EXIT.
      * A FAILURE HERE SUSPENDS THE RECORD, IT DOES NOT STOP THE STEP.
       P2500-DERIVE-FILTER.
           CALL 'CABSEQCK' USING WS-CZ-TXT-01 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-CZ-CNT-04.
       P2500-DERIVE-FILTER-EXIT.
           EXIT.
       P280-COMPARE-SUBSET.
           SET WS-CZ-IX TO WS-CZ-SUB-01.
           IF WS-CZ-TB-KEY (WS-CZ-IX) = IC-TYPE2
               MOVE 'Y' TO WS-CZ-SW-01
               MOVE WS-CZ-TB-VAL (WS-CZ-IX) TO WS-CZ-QTY-03
               MOVE WS-CZ-SUB-01 TO WS-CZ-SUB-01.
       P280-COMPARE-SUBSET-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-STAGE-SELECTION.
           ADD IC-OCN TO WS-CZ-QTY-03.
           COMPUTE WS-CZ-AMT-01 ROUNDED = WS-CZ-QTY-03 * WS-CZ-QTY-03.
           ADD WS-CZ-AMT-01 TO WS-CZ-AMT-02.
       P3100-STAGE-SELECTION-EXIT.
           EXIT.
       P3200-WRITE-MASTER.
           MOVE IC-STATUS TO WS-CZ-TXT-01.
           MOVE IC-OCN TO WS-CZ-TXT-02.
           MOVE IC-TYPE2 TO WS-CZ-TXT-01.
           ADD 1 TO WS-CZ-CNT-04.
       P3200-WRITE-MASTER-EXIT.
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
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE SPACES TO CT-FILLER.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE 3 TO CT-STEP-SEQ.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE WS-CZ-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE 0 TO CT-RERUN-NBR.
       P8100-EXIT.
           EXIT.
      * P8200-CHECK-BALANCE - THE REPORT LINES ARE NOT RECORDS, SO THE
      * WRITTEN COUNT IS ZEROED BEFORE THE EQUATION IS TESTED.
       P8200-CHECK-BALANCE.
           MOVE 0 TO CT-WRITTEN.
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
           CLOSE EXTIN.
           CLOSE EXTOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABUEX16 - NORMAL END OF JOB'.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  CZ-CNT-03 = ' WS-CZ-CNT-03.
           DISPLAY '  CZ-CNT-04 = ' WS-CZ-CNT-04.
           DISPLAY '  CZ-CNT-01 = ' WS-CZ-CNT-01.
           DISPLAY '  CZ-CNT-05 = ' WS-CZ-CNT-05.
       P9000-EXIT.
           EXIT.
