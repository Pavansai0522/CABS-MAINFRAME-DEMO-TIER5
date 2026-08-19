      *****************************************************************
      * CABUEX26 - FACTOR STUDY EXTRACT                               *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               USGIN   TELCABS.CABS.USGIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               FEEDOUT TELCABS.CABS.FEEDOU         (LOCAL)     *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - NOTHING IS UPDATED IN PLACE        *
      * REVISION HISTORY                                              *
      *   V1.00  1995-03-24  J.M.CASTILLO INITIAL RELEASE             *
      *   V1.02  2001-11-15  B.R.HALVORSEN CONTROL RECORD ADDED PER   *
      *                      CABS-STD-002                             *
      *   V1.05  2002-01-05  T.YAMASHITA  JOB PARAMETER MADE MANDATORY*
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUEX26.
       AUTHOR. TELCABS APPLICATIONS - DATASET EXTRACT.
      *****************************************************************
      * FACTOR STUDY EXTRACT. THE STEP RUNS ONCE PER BILL CYCLE AND IS*
      * RERUN FROM THE TOP IF IT FAILS.                               *
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
           SELECT USGIN ASSIGN TO UT-S-USGIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT FEEDOUT ASSIGN TO UT-S-FEEDOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT CTLOUT ASSIGN TO UT-S-CTLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      * USGIN - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  USGIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 100 CHARACTERS.
       01  CABS-AT-IN-RECORD.
           05  IA-CENTRE                   PIC X(08).
           05  IA-STATUS                   PIC S9(15) COMP-3.
           05  IA-CIRCUIT                  PIC X(10).
           05  IA-CODE                     PIC 9(09).
           05  IA-CENTRE2                  PIC X(06).
           05  IA-TARGET                   PIC S9(09) COMP-3.
           05  IA-TARGET2                  PIC S9(13)V9(02) COMP-3.
           05  IA-TYPE                     PIC X(16).
           05  IA-REGION                   PIC X(02).
           05  IA-CODE2                    PIC X(06).
           05  IA-PERIOD                   PIC 9(02).
           05  IA-GROUP                    PIC S9(13)V9(05) COMP-3.
           05  IA-CIRCUIT2                 PIC 9(02).
           05  AT-FILL-01                  PIC X(8).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-AT-VIEW1 REDEFINES CABS-AT-IN-RECORD.
           05  R0A-INVOICE                 PIC X(04).
           05  R0A-TARGET                  PIC X(08).
           05  R0A-ELEM                    PIC S9(15) COMP-3.
           05  R0A-STATE                   PIC X(16).
           05  R0A-REGION                  PIC S9(09)V9(05) COMP-3.
           05  R0A-ACCOUNT                 PIC 9(09).
           05  R0A-SEQ                     PIC X(03).
           05  R0A-TYPE                    PIC S9(15) COMP-3.
           05  R0A-CLASS                   PIC X(20).
           05  R0A-REST                    PIC X(16).
      * FEEDOUT - CATALOGUED GENERATION DATA GROUP.
       FD  FEEDOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 90 CHARACTERS.
       01  CABS-AT-OUT-RECORD.
           05  OA-BAN                      PIC X(02).
           05  OA-ELEM                     PIC S9(11)V9(05) COMP-3.
           05  OA-TYPE                     PIC X(03).
           05  OA-REGION                   PIC X(08).
           05  OA-CIRCUIT                  PIC X(16).
           05  OA-CYCLE                    PIC 9(06).
           05  OA-SEQ                      PIC X(04).
           05  OA-SEGMENT                  PIC 9(06).
           05  OA-ACCOUNT                  PIC S9(15) COMP-3.
           05  OA-GROUP                    PIC S9(09)V9(02) COMP-3.
           05  OA-BAND                     PIC S9(09) COMP-3.
           05  OA-CENTRE                   PIC S9(15) COMP-3.
           05  AT-FILL-02                  PIC X(9).
      * CTLOUT - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD              PIC X(180).
       WORKING-STORAGE SECTION.
      * STANDARD SHARED WORKING STORAGE. SEE CABSWRK.
       COPY CABSWRK.
      * SHARED LAYOUT PULLED IN FOR THE FILTER SIDE.
       COPY CABSCIRC.
      * SHARED LAYOUT PULLED IN FOR THE EXTRACT SIDE.
       COPY CABSCDR.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUEX26'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V1.16'.
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
       01  WS-COUNT-AREA.
           05  WS-AT-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AT-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AT-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AT-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AT-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AT-CNT-06                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-AT-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AT-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AT-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AT-QTY-04                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-AT-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AT-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-AT-TXT-01                PIC X(20) VALUE SPACES.
           05  WS-AT-TXT-02                PIC X(26) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-AT-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-AT-ON-01                 VALUE 'Y'.
               88  WS-AT-OFF-01                VALUE 'N'.
           05  WS-AT-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-AT-ON-02                 VALUE 'Y'.
               88  WS-AT-OFF-02                VALUE 'N'.
           05  WS-AT-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-AT-ON-03                 VALUE 'Y'.
               88  WS-AT-OFF-03                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-AT-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-AT-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-AT-SUB-03                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. A ROW THAT DOES NOT FIT IS COUNTED AND DROPPED
      * - THE STEP DOES NOT FAIL.
       01  WS-AT-TABLE.
           05  WS-AT-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-AT-TB-ENTRY OCCURS 100 TIMES
                                       INDEXED BY WS-AT-IX.
               10  WS-AT-TB-KEY                PIC X(10).
               10  WS-AT-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-AT-TB-TXT                PIC X(30).
               10  WS-AT-TB-EFF                PIC 9(05).
               10  WS-AT-TB-EXP                PIC 9(05).
       01  WS-AT-WORK-GROUP-1.
           05  WS-AT-G1-BAND               PIC S9(11)V9(02) COMP-3.
           05  WS-AT-G1-MEDIA              PIC 9(05).
           05  WS-AT-G1-TARIFF             PIC X(10).
           05  WS-AT-G1-TARIFF             PIC X(20).
           05  WS-AT-G1-BAND               PIC S9(09) COMP-3.
           05  WS-AT-G1-STATUS             PIC X(20).
           05  WS-AT-G1-CODE               PIC X(20).
           05  WS-AT-G1-TYPE               PIC 9(07).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 50.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUEX26 - FACTOR STUDY EXTRACT'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-AT-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-AT-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9959.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-AT-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-AT-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT USGIN.
           IF WS-FS-INPUT NOT = '00'
               DISPLAY 'USGIN FILE STATUS = ' WS-FS-INPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON USGIN - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT FEEDOUT.
           IF WS-FS-OUTPUT NOT = '00'
               DISPLAY 'FEEDOUT FILE STATUS = ' WS-FS-OUTPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON FEEDOUT - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               DISPLAY 'CTLOUT FILE STATUS = ' WS-FS-CONTROL
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON CTLOUT - CHECK THE ALLOCATION' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-AT-CYCLE-YYDDD.
           COMPUTE WS-AT-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-AT-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-AT-CNT-02.
           MOVE 0 TO WS-AT-CNT-05.
           MOVE 0 TO WS-AT-CNT-01.
       P1200-EXIT.
           EXIT.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-AT-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-AT-TAB-CNT NOT < 100
               MOVE 'Y' TO WS-AT-SW-01
               ADD 1 TO WS-AT-CNT-03
           ELSE
               ADD 1 TO WS-AT-TAB-CNT
               SET WS-AT-IX TO WS-AT-TAB-CNT
               MOVE IA-TYPE TO WS-AT-TB-KEY (WS-AT-IX)
               MOVE 0 TO WS-AT-TB-VAL (WS-AT-IX)
               MOVE SPACES TO WS-AT-TB-TXT (WS-AT-IX)
               ADD 1 TO WS-AT-CNT-04.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ USGIN
               AT END MOVE 'Y' TO WS-AT-SW-01.
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
           IF WS-AT-ON-01
               PERFORM P2200-EDIT-SUBSET THRU P2200-EDIT-SUBSET-EXIT.
           PERFORM P2300-RESOLVE-SELECTION THRU
               P2300-RESOLVE-SELECTION-EXIT.
           IF WS-AT-ON-01
               PERFORM P2400-VALIDATE-SUBSET THRU
                   P2400-VALIDATE-SUBSET-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ USGIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P2200-EDIT-SUBSET.
           CALL 'CABTBLLU' USING WS-AT-TXT-01 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-AT-CNT-04.
           MOVE 'N' TO WS-AT-SW-03.
           IF WS-AT-TAB-CNT > 0
               PERFORM P250-COMPARE-FILTER THRU P250-COMPARE-FILTER-EXIT
               VARYING WS-AT-SUB-03 FROM 1 BY 1
               UNTIL WS-AT-SUB-03 > WS-AT-TAB-CNT
               OR WS-AT-SW-03 = 'Y'.
       P2200-EDIT-SUBSET-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P2300-RESOLVE-SELECTION.
           UNSTRING WS-AT-TXT-02 DELIMITED BY '/'
               INTO WS-AT-TXT-02
               WS-AT-TXT-02
               TALLYING IN WS-AT-CNT-04.
       P2300-RESOLVE-SELECTION-EXIT.
           EXIT.
      * A FAILURE HERE SUSPENDS THE RECORD, IT DOES NOT STOP THE STEP.
       P2400-VALIDATE-SUBSET.
           MOVE SPACES TO CABS-AT-OUT-RECORD.
           MOVE IA-TARGET2 TO OA-BAN.
           MOVE IA-TYPE TO OA-ELEM.
           MOVE IA-TARGET2 TO OA-TYPE.
           MOVE IA-CODE TO OA-REGION.
           WRITE CABS-AT-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P2400-VALIDATE-SUBSET-EXIT.
           EXIT.
       P250-COMPARE-FILTER.
           SET WS-AT-IX TO WS-AT-SUB-03.
           IF WS-AT-TB-KEY (WS-AT-IX) = IA-TARGET2
               MOVE 'Y' TO WS-AT-SW-03
               MOVE WS-AT-TB-VAL (WS-AT-IX) TO WS-AT-QTY-02
               MOVE WS-AT-SUB-03 TO WS-AT-SUB-03.
       P250-COMPARE-FILTER-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-FORMAT-FILTER.
           MOVE SPACES TO CABS-AT-OUT-RECORD.
           MOVE IA-CODE2 TO OA-BAN.
           MOVE IA-CENTRE2 TO OA-ELEM.
           MOVE IA-CENTRE TO OA-TYPE.
           MOVE IA-CIRCUIT2 TO OA-REGION.
           WRITE CABS-AT-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P3100-FORMAT-FILTER-EXIT.
           EXIT.
       P3200-RELEASE-RANGE.
           MOVE SPACES TO WS-AT-TXT-01.
           STRING IA-GROUP DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IA-CIRCUIT2 DELIMITED BY SIZE
               INTO WS-AT-TXT-01.
       P3200-RELEASE-RANGE-EXIT.
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
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE 3 TO CT-STEP-SEQ.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE WS-AT-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE WS-AT-TXT-02 TO CT-RESTART-KEY.
           MOVE SPACES TO CT-FILLER.
           MOVE WS-AT-CNT-01 TO CT-RC.
       P8100-EXIT.
           EXIT.
      * P8200-CHECK-BALANCE - A RECORD THAT WAS HELD FOR THE NEXT
      * CYCLE IS BROUGHT INTO THE CARRIED FORWARD SIDE BEFORE THE
      * EQUATION IS TESTED.
       P8200-CHECK-BALANCE.
           ADD WS-AT-CNT-02 TO CT-CARRIED-FWD.
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
           CLOSE USGIN.
           CLOSE FEEDOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABUEX26 - RUN COMPLETE'.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  AT-CNT-01 = ' WS-AT-CNT-01.
           DISPLAY '  AT-CNT-04 = ' WS-AT-CNT-04.
       P9000-EXIT.
           EXIT.
