      *****************************************************************
      * CABURT07 - RATE TABLE ROW ADD AND CHANGE                      *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               MNTIN   TELCABS.CABS.MNTIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               AUDOUT  TELCABS.CABS.AUDOUT         (LOCAL)     *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * REVISION HISTORY                                              *
      *   V1.00  1993-06-22  L.FERREIRA   INITIAL RELEASE             *
      *   V1.04  1997-11-13  J.M.CASTILLO CONTROL RECORD ADDED PER    *
      *                      CABS-STD-002                             *
      *   V1.05  1999-09-11  T.YAMASHITA  HASH TOTAL ADDED TO THE     *
      *                      CONTROL RECORD                           *
      *   V1.07  2017-07-06  M.DELACROIX  EFFECTIVE DATE FILTER ADDED *
      *                      PER AUDIT FINDING                        *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABURT07.
       AUTHOR. TELCABS APPLICATIONS - RATE TABLE MAINTENANCE.
      *****************************************************************
      * RATE TABLE ROW ADD AND CHANGE. THE STEP IS DRIVEN ENTIRELY    *
      * FROM THE SYSIN PARM CARD AND THE DD ALLOCATIONS IN THE JOB.   *
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
           SELECT MNTIN ASSIGN TO UT-S-MNTIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT AUDOUT ASSIGN TO UT-S-AUDOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT CTLOUT ASSIGN TO UT-S-CTLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      * MNTIN - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  MNTIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-CS-IN-RECORD.
           05  IC-CENTRE                   PIC S9(15) COMP-3.
           05  IC-PERIOD                   PIC X(10).
           05  IC-CARRIER                  PIC X(03).
           05  IC-ACCOUNT                  PIC 9(07).
           05  IC-LEVEL                    PIC X(13).
           05  IC-OCN                      PIC X(03).
           05  IC-CLASS                    PIC X(03).
           05  IC-SOURCE                   PIC S9(09)V9(02) COMP-3.
           05  IC-CYCLE                    PIC X(04).
           05  IC-LEVEL2                   PIC 9(09).
           05  CS-FILL-01                  PIC X(14).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-CS-VIEW1 REDEFINES CABS-CS-IN-RECORD.
           05  R0C-LEVEL                   PIC S9(11)V9(02) COMP-3.
           05  R0C-STATUS                  PIC S9(13)V9(02) COMP-3.
           05  R0C-ELEM                    PIC X(03).
           05  R0C-CENTRE                  PIC X(16).
           05  R0C-BAND                    PIC X(02).
           05  R0C-REST                    PIC X(44).
      * AUDOUT - CATALOGUED GENERATION DATA GROUP.
       FD  AUDOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 90 CHARACTERS.
       01  CABS-CS-OUT-RECORD.
           05  OC-CYCLE                    PIC X(04).
           05  OC-TARGET                   PIC X(08).
           05  OC-BAND                     PIC X(20).
           05  OC-PERIOD                   PIC X(13).
           05  OC-INVOICE                  PIC S9(09) COMP-3.
           05  OC-PERIOD2                  PIC S9(09) COMP-3.
           05  OC-SEGMENT                  PIC X(06).
           05  OC-CLASS                    PIC 9(02).
           05  OC-INVOICE2                 PIC S9(09) COMP-3.
           05  OC-GROUP                    PIC S9(13)V9(02) COMP-3.
           05  OC-BAND2                    PIC X(06).
           05  OC-TARGET2                  PIC 9(03).
           05  CS-FILL-02                  PIC X(5).
      * CTLOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD              PIC X(180).
       WORKING-STORAGE SECTION.
      * STANDARD SHARED WORKING STORAGE. SEE CABSWRK.
       COPY CABSWRK.
      * SHARED LAYOUT PULLED IN FOR THE KEY SIDE.
       COPY CABSRATE.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABURT07'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V1.07'.
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
           05  WS-CS-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CS-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CS-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CS-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CS-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CS-CNT-06                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-CS-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CS-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CS-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-CS-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CS-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CS-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CS-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-CS-TXT-01                PIC X(16) VALUE SPACES.
           05  WS-CS-TXT-02                PIC X(12) VALUE SPACES.
           05  WS-CS-TXT-03                PIC X(16) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-CS-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-CS-ON-01                 VALUE 'Y'.
               88  WS-CS-OFF-01                VALUE 'N'.
           05  WS-CS-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-CS-ON-02                 VALUE 'Y'.
               88  WS-CS-OFF-02                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-CS-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CS-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CS-SUB-03                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. LOADED ONCE AT INITIALISATION AND SEARCHED WITH
      * A SUBSCRIPTED WALK.
       01  WS-CS-TABLE.
           05  WS-CS-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-CS-TB-ENTRY OCCURS 150 TIMES
                                       INDEXED BY WS-CS-IX.
               10  WS-CS-TB-KEY                PIC X(06).
               10  WS-CS-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-CS-TB-TXT                PIC X(30).
               10  WS-CS-TB-EFF                PIC 9(05).
               10  WS-CS-TB-EXP                PIC 9(05).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 58.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABURT07 - RATE TABLE ROW ADD AND CHANGE'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-CS-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-CS-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9930.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-CS-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-CS-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT MNTIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON MNTIN - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               DISPLAY 'MNTIN FILE STATUS = ' WS-FS-INPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT AUDOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON AUDOUT - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               DISPLAY 'AUDOUT FILE STATUS = ' WS-FS-OUTPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON CTLOUT - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               DISPLAY 'CTLOUT FILE STATUS = ' WS-FS-CONTROL
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
       P1100-EXIT.
           EXIT.
      * P1200-READ-PARM - THE CYCLE DATE ARRIVES AS TWO DIGITS AND IS
      * PIVOTED ON DW-PIVOT-YY BEFORE ANY DATE MATH.
       P1200-READ-PARM.
           MOVE SPACES TO WS-PARM-CARD.
           ACCEPT WS-PARM-CARD FROM SYSIN.
           CALL 'CABPARMR' USING WS-PARM-CARD WS-RC-PARMR.
           MOVE PC1-CYCLE-YYDDD TO WS-CS-CYCLE-YYDDD.
           COMPUTE WS-CS-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-CS-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-CS-CNT-05.
           MOVE 0 TO WS-CS-CNT-03.
           MOVE 0 TO WS-CS-CNT-02.
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
           PERFORM P2200-APPLY-OVERRIDE THRU P2200-APPLY-OVERRIDE-EXIT.
           PERFORM P2300-BUILD-TARIFF THRU P2300-BUILD-TARIFF-EXIT.
           PERFORM P2400-SPLIT-DESCRIPTION THRU
               P2400-SPLIT-DESCRIPTION-EXIT.
           PERFORM P2500-EDIT-ROW THRU P2500-EDIT-ROW-EXIT.
           PERFORM P2600-APPLY-ELEMENT THRU P2600-APPLY-ELEMENT-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ MNTIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-APPLY-OVERRIDE.
           MOVE SPACES TO WS-CS-TXT-01.
           STRING IC-OCN DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IC-OCN DELIMITED BY SIZE
               INTO WS-CS-TXT-01.
       P2200-APPLY-OVERRIDE-EXIT.
           EXIT.
       P2300-BUILD-TARIFF.
           MOVE WS-CS-AMT-02 TO WS-CS-AMT-03.
           IF WS-CS-AMT-03 < 0
               COMPUTE WS-CS-AMT-03 = 0 - WS-CS-AMT-02.
       P2300-BUILD-TARIFF-EXIT.
           EXIT.
       P2400-SPLIT-DESCRIPTION.
           MOVE 'N' TO WS-CS-SW-02.
           IF WS-CS-TXT-01 NOT = WS-CS-TXT-02
               MOVE 'Y' TO WS-CS-SW-02
               MOVE WS-CS-TXT-01 TO WS-CS-TXT-02
               ADD 1 TO WS-CS-CNT-04.
       P2400-SPLIT-DESCRIPTION-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P2500-EDIT-ROW.
           IF IC-CENTRE = 'X'
               ADD 1 TO WS-CS-CNT-03
           ELSE
               IF IC-CENTRE = 'C'
                   ADD 1 TO WS-CS-CNT-04
               ELSE
                   IF IC-CENTRE = 'B'
                       ADD 1 TO WS-CS-CNT-05
                   ELSE
                       ADD 1 TO WS-CS-CNT-04.
       P2500-EDIT-ROW-EXIT.
           EXIT.
       P2600-APPLY-ELEMENT.
           IF WS-CS-AMT-04 < 49
               MOVE 49 TO WS-CS-AMT-04
               ADD 1 TO WS-CS-CNT-03.
           IF WS-CS-AMT-04 > 13532
               MOVE 13532 TO WS-CS-AMT-04
               ADD 1 TO WS-CS-CNT-05.
       P2600-APPLY-ELEMENT-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-STAGE-KEY.
           MOVE SPACES TO CABS-CS-OUT-RECORD.
           MOVE IC-SOURCE TO OC-CYCLE.
           MOVE IC-CARRIER TO OC-TARGET.
           MOVE IC-ACCOUNT TO OC-BAND.
           MOVE IC-CENTRE TO OC-PERIOD.
           MOVE IC-CLASS TO OC-INVOICE.
           MOVE IC-CENTRE TO OC-PERIOD2.
           MOVE IC-CENTRE TO OC-SEGMENT.
           MOVE IC-PERIOD TO OC-CLASS.
           MOVE IC-PERIOD TO OC-INVOICE2.
           WRITE CABS-CS-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P3100-STAGE-KEY-EXIT.
           EXIT.
       P3200-STAGE-BAND.
           MOVE 0 TO WS-CS-QTY-03.
           MOVE 0 TO WS-CS-QTY-02.
           MOVE 0 TO WS-CS-AMT-04.
       P3200-STAGE-BAND-EXIT.
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
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE 8 TO CT-STEP-SEQ.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE WS-CS-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE WS-CS-CNT-03 TO CT-RC.
           MOVE SPACES TO CT-FILLER.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE SPACES TO CT-RESTART-KEY.
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
           CLOSE MNTIN.
           CLOSE AUDOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABURT07 - END OF RUN'.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  CS-CNT-05 = ' WS-CS-CNT-05.
           DISPLAY '  CS-CNT-01 = ' WS-CS-CNT-01.
           DISPLAY '  CS-CNT-06 = ' WS-CS-CNT-06.
           DISPLAY '  CS-CNT-04 = ' WS-CS-CNT-04.
       P9000-EXIT.
           EXIT.
