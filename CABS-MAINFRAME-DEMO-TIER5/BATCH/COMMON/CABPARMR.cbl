      *****************************************************************
      * CABPARMR - RUN PARAMETER CARD READER                          *
      * APPLICATION : CABS                                            *
      * INVOKED BY  : CALL FROM A CABS BATCH PROGRAM AFTER THE SYSIN  *
      *               CARD HAS BEEN ACCEPTED                          *
      * INPUTS      : LK-PARM-CARD  X(80) AS PRESENTED BY THE CALLER  *
      *               PARMCTL  TELCABS.CABS.PARMCTL   FB 80           *
      * OUTPUTS     : LK-PARM-CARD  X(80) NORMALISED IN PLACE         *
      *               LK-PARM-RC    9(04) RETURN CODE                 *
      * CONTROL     : NONE - SUBPROGRAMS DO NOT WRITE CTLOUT,         *
      *               CABS-STD-041                                    *
      * BALANCE     : NONE - THE CALLING PROGRAM BALANCE IS NOT       *
      *               AFFECTED BY THIS MODULE                         *
      * RESTART     : NONE - THE MODULE CARRIES NO RECOVERABLE STATE  *
      * REVISION HISTORY                                              *
      *   V1.00  1987-03-16  R.T.WHEELER   INITIAL RELEASE, WRITTEN   *
      *                      FOR CABURT02 ONLY                        *
      *   V1.01  1991-08-02  D.OKONKWO     TARIFF CODE EDIT ADDED     *
      *   V1.03  1996-06-24  A.BUKOWSKI    KEYWORD CARD PARSER ADDED  *
      *                      ALONGSIDE THE POSITIONAL FORM            *
      *   V1.04  1998-11-09  L.FERREIRA    CYCLE YEAR PIVOTED AT 70   *
      *                      BEFORE ANY DATE MATH, CABS-STD-018       *
      *   V1.05  2001-04-30  M.HAAS        PARMCTL DEFAULT TABLE      *
      *                      LOADED ON THE FIRST CALL OF A RUN UNIT   *
      *   V1.06  2007-02-12  J.CALLAGHAN   OPERATIONS SUMMARY CARD    *
      *                      99999 IN COLUMNS 1 THROUGH 5             *
      *   V1.07  2013-01-18  G.PETRAKIS    RAO PREFIX EDIT MOVED OUT  *
      *                      TO CABRAOCK                              *
      *   V1.08  2019-05-20  P.NAIR        RECOMPILE ONLY             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABPARMR.
       AUTHOR. TELCABS APPLICATIONS - COMMON SUBPROGRAMS.
      *****************************************************************
      * THE MODULE TAKES THE 80 BYTE RUN PARAMETER CARD THAT THE      *
      * CALLER HAS ALREADY READ FROM SYSIN, NORMALISES IT TO THE      *
      * POSITIONAL LAYOUT AND HANDS IT BACK IN THE SAME STORAGE.      *
      * EVERY CALLER REDEFINES ITS OWN CARD, SO THE CORRECTED FIELDS  *
      * ARE VISIBLE THROUGH THAT REDEFINES WITHOUT A FURTHER OPERAND. *
      * THE NORMALISED CARD IS CYCLE YYDDD IN 1 THROUGH 5, BILL       *
      * PERIOD IN 6 THROUGH 11, TARIFF CODE IN 12 THROUGH 15, RUN ID  *
      * IN 16 THROUGH 27, PROCESS ID IN 69 THROUGH 76 AND THE REST    *
      * RESERVED.                                                     *
      *                                                               *
      * THIS IS A SUBPROGRAM. IT WRITES NO CONTROL RECORD AND THE     *
      * CALLING PROGRAM BALANCE IS UNAFFECTED BY IT.                  *
      *                                                               *
      * RETURN CODES IN LK-PARM-RC                                    *
      *   0000  CARD ACCEPTED AS PRESENTED                            *
      *   0004  ONE OR MORE FIELDS DEFAULTED FROM PARMCTL             *
      *   0008  KEYWORD FORM ACCEPTED AND CONVERTED TO POSITIONAL     *
      *   0012  CYCLE NOT NUMERIC OR DAY OF YEAR OUTSIDE 001 TO 366   *
      *   0016  NO CARD PRESENTED AND NO DEFAULT ROW FOUND            *
      *   0020  TARIFF CODE NOT IN THE LIVE TABLE - THE RUN GOES ON   *
      *         WITH THE CALLER OWN DEFAULT                           *
      *   0024  PARMCTL COULD NOT BE OPENED, DEFAULTS UNAVAILABLE     *
      * WHERE MORE THAN ONE HOLDS, THE CODE RETURNED IS THE FIRST OF  *
      * 0016, 0024, 0012, 0020, 0004, 0008, 0000.                     *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PARMCTL ASSIGN TO UT-S-PARMCTL
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-PARMCTL.
       DATA DIVISION.
       FILE SECTION.
      * PARMCTL - THE ESTATE DEFAULT CARD DATASET. OPENED, LOADED AND
      * CLOSED ON THE FIRST CALL OF A RUN UNIT AND NOT TOUCHED AGAIN.
       FD  PARMCTL
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-PARMCTL-RECORD.
           05  PT-PROCESS-ID               PIC X(08).
           05  PT-CYCLE-YYDDD              PIC X(05).
           05  PT-BILL-PERIOD              PIC X(06).
           05  PT-TARIFF-CD                PIC X(04).
           05  PT-RUN-ID                   PIC X(12).
           05  PT-DEFAULT-SW               PIC X(01).
           05  PT-FILLER                   PIC X(44).
       WORKING-STORAGE SECTION.
      * WORKING-STORAGE OF A CALLED PROGRAM SURVIVES FROM ONE CALL TO
      * THE NEXT WITHIN A RUN UNIT. THE DEFAULT TABLE, THE FIRST CALL
      * SWITCH AND EVERY COUNTER BELOW RELY ON THAT.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABPARMR'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V1.08'.
           05  WS-CTL-LIMIT                PIC S9(04) COMP-3 VALUE 120.
           05  WS-TARIFF-LIMIT             PIC S9(04) COMP-3 VALUE 12.
       01  WS-FS-PARMCTL                   PIC X(02) VALUE '00'.
       01  WS-FIRST-CALL-SW                PIC X(01) VALUE 'Y'.
           88  WS-FIRST-CALL                   VALUE 'Y'.
      * IN CORE DEFAULT TABLE. THE CONTROL DATASET IS MAINTAINED BY
      * HAND AND IS NOT HELD IN PROCESS ID ORDER, SO THE TABLE IS
      * WALKED WITH A SUBSCRIPT RATHER THAN SEARCHED.
       01  WS-CTL-TABLE.
           05  WS-CTL-CNT                  PIC S9(04) COMP-3 VALUE 0.
           05  WS-CTL-ENTRY OCCURS 120 TIMES.
               10  WS-CTL-PROCESS-ID       PIC X(08).
               10  WS-CTL-CYCLE-YYDDD      PIC X(05).
               10  WS-CTL-BILL-PERIOD      PIC X(06).
               10  WS-CTL-TARIFF-CD        PIC X(04).
               10  WS-CTL-RUN-ID           PIC X(12).
               10  WS-CTL-DEFAULT-SW       PIC X(01).
      * THE LIVE TARIFF LIST. ROWS ARE ADDED WHEN A TARIFF IS FILED
      * AND ARE LEFT IN PLACE AFTER IT IS WITHDRAWN.
       01  WS-TARIFF-CONSTANTS.
           05  FILLER                      PIC X(24) VALUE
               'FCC1FCC2FCC4SA01SA02SA03'.
           05  FILLER                      PIC X(24) VALUE
               'SW01SW02ST01ST02UNE1MPB1'.
       01  WS-TARIFF-TABLE REDEFINES WS-TARIFF-CONSTANTS.
           05  WS-TARIFF-ENTRY OCCURS 12 TIMES PIC X(04).
      * THE CARD AS THE MODULE HOLDS IT WHILE IT WORKS ON IT. THE
      * FIELDS ARE ALPHANUMERIC SO THAT A BLANK OR A KEYWORD CARD CAN
      * BE EXAMINED BEFORE ANY NUMERIC EDIT IS ATTEMPTED.
       01  WS-CARD                         PIC X(80).
       01  WS-CARD-POS REDEFINES WS-CARD.
           05  WC-CYCLE-YYDDD              PIC X(05).
           05  WC-BILL-PERIOD              PIC X(06).
           05  WC-TARIFF-CD                PIC X(04).
           05  WC-RUN-ID                   PIC X(12).
           05  WC-RESERVED-1               PIC X(41).
           05  WC-PROCESS-ID               PIC X(08).
           05  WC-RESERVED-2               PIC X(04).
      * KEYWORD WORK AREAS. THE CARD IS SPLIT ON COMMA AND THEN THE
      * PAIR IS SPLIT ON THE EQUALS SIGN.
       01  WS-KW-TABLE.
           05  WS-KW-PAIR OCCURS 8 TIMES.
               10  WS-KW-TEXT              PIC X(30).
       01  WS-KW-NAME                      PIC X(10) VALUE SPACES.
       01  WS-KW-VALUE                     PIC X(20) VALUE SPACES.
       01  WS-KW-CNT                       PIC S9(04) COMP-3 VALUE 0.
       01  WS-SAVE-PROCESS-ID              PIC X(08) VALUE SPACES.
       01  WS-EQ-CNT                       PIC S9(04) COMP-3 VALUE 0.
       01  WS-DIGIT-CNT                    PIC S9(04) COMP-3 VALUE 0.
       01  WS-FILL-41                      PIC X(41) VALUE SPACES.
       01  WS-FILL-04                      PIC X(04) VALUE SPACES.
       01  WS-CYCLE-TEST                   PIC X(05) VALUE SPACES.
       01  WS-CYCLE-PARTS REDEFINES WS-CYCLE-TEST.
           05  WS-CY-YY                    PIC 9(02).
           05  WS-CY-DDD                   PIC 9(03).
       01  WS-PERIOD-TEST                  PIC X(06) VALUE SPACES.
       01  WS-SUBSCRIPT-AREA.
           05  WS-SUB-01                   PIC S9(04) COMP-3 VALUE 0.
           05  WS-SUB-02                   PIC S9(04) COMP-3 VALUE 0.
           05  WS-SUB-03                   PIC S9(04) COMP-3 VALUE 0.
           05  WS-SUB-04                   PIC S9(04) COMP-3 VALUE 0.
           05  WS-ROW-SUB                  PIC S9(04) COMP-3 VALUE 1.
       01  WS-SWITCH-AREA.
           05  WS-CTL-AVAIL-SW             PIC X(01) VALUE 'N'.
               88  WS-CTL-AVAIL                VALUE 'Y'.
           05  WS-CTL-EOF-SW               PIC X(01) VALUE 'N'.
               88  WS-CTL-EOF                  VALUE 'Y'.
           05  WS-CTL-FULL-SW              PIC X(01) VALUE 'N'.
               88  WS-CTL-FULL                 VALUE 'Y'.
           05  WS-SUMMARY-SW               PIC X(01) VALUE 'N'.
               88  WS-SUMMARY-REQ              VALUE 'Y'.
           05  WS-KEYWORD-SW               PIC X(01) VALUE 'N'.
               88  WS-KEYWORD-FORM             VALUE 'Y'.
           05  WS-BLANK-CARD-SW            PIC X(01) VALUE 'N'.
               88  WS-BLANK-CARD               VALUE 'Y'.
           05  WS-CYCLE-BAD-SW             PIC X(01) VALUE 'N'.
               88  WS-CYCLE-BAD                VALUE 'Y'.
           05  WS-TARIFF-BAD-SW            PIC X(01) VALUE 'N'.
               88  WS-TARIFF-BAD               VALUE 'Y'.
           05  WS-TARIFF-FOUND-SW          PIC X(01) VALUE 'N'.
               88  WS-TARIFF-FOUND             VALUE 'Y'.
           05  WS-NEED-CYCLE-SW            PIC X(01) VALUE 'N'.
               88  WS-NEED-CYCLE               VALUE 'Y'.
           05  WS-NEED-PERIOD-SW           PIC X(01) VALUE 'N'.
               88  WS-NEED-PERIOD              VALUE 'Y'.
           05  WS-NEED-TARIFF-SW           PIC X(01) VALUE 'N'.
               88  WS-NEED-TARIFF              VALUE 'Y'.
           05  WS-NEED-RUNID-SW            PIC X(01) VALUE 'N'.
               88  WS-NEED-RUNID               VALUE 'Y'.
           05  WS-DEFAULTED-SW             PIC X(01) VALUE 'N'.
               88  WS-DEFAULTED                VALUE 'Y'.
           05  WS-ROW-FOUND-SW             PIC X(01) VALUE 'N'.
               88  WS-ROW-FOUND                VALUE 'Y'.
       01  WS-COUNT-AREA.
           05  WS-CNT-CARDS                PIC S9(07) COMP-3 VALUE 0.
           05  WS-CNT-KEYWORD              PIC S9(07) COMP-3 VALUE 0.
           05  WS-CNT-POSITIONAL           PIC S9(07) COMP-3 VALUE 0.
           05  WS-CNT-DEFAULTED            PIC S9(07) COMP-3 VALUE 0.
           05  WS-CNT-REJECTED             PIC S9(07) COMP-3 VALUE 0.
           05  WS-CNT-UNKNOWN-KW           PIC S9(07) COMP-3 VALUE 0.
           05  WS-CNT-NO-CTL               PIC S9(07) COMP-3 VALUE 0.
       LINKAGE SECTION.
       01  LK-PARM-CARD                    PIC X(80).
       01  LK-PARM-RC                      PIC 9(04).
       PROCEDURE DIVISION USING LK-PARM-CARD LK-PARM-RC.
      * ONE PASS PER CALL. THE PARMCTL LOAD IS DRIVEN OFF
      * WS-FIRST-CALL-SW AND THEREFORE HAPPENS ONCE PER RUN UNIT.
       P0000-ENTRY.
           MOVE 0 TO LK-PARM-RC.
           MOVE 'N' TO WS-SUMMARY-SW.
           IF WS-FIRST-CALL
               PERFORM P1000-LOAD-PARMCTL THRU P1000-EXIT.
           MOVE LK-PARM-CARD TO WS-CARD.
           IF WC-CYCLE-YYDDD = '99999'
               MOVE 'Y' TO WS-SUMMARY-SW.
           IF WS-SUMMARY-REQ
               PERFORM P9500-DISPLAY-TOTALS THRU P9500-EXIT
           ELSE
               PERFORM P2000-CLASSIFY-CARD THRU P2000-EXIT
               PERFORM P3000-VALIDATE-CARD THRU P3000-EXIT
               PERFORM P4000-APPLY-DEFAULTS THRU P4000-EXIT
               PERFORM P5000-REBUILD-CARD THRU P5000-EXIT.
           GOBACK.
      * S100-CONTROL-LOAD SECTION - PARMCTL IS READ ONCE PER RUN UNIT.
       S100-CONTROL-LOAD SECTION.
       P1000-LOAD-PARMCTL.
           MOVE 'N' TO WS-FIRST-CALL-SW.
           MOVE 0 TO WS-CTL-CNT.
           MOVE 'N' TO WS-CTL-EOF-SW.
           MOVE 'N' TO WS-CTL-FULL-SW.
           PERFORM P1100-OPEN-PARMCTL THRU P1100-EXIT.
           IF WS-CTL-AVAIL
               PERFORM P1200-READ-PARMCTL THRU P1200-EXIT
               PERFORM P1300-STORE-ROW THRU P1300-EXIT
                   UNTIL WS-CTL-EOF OR WS-CTL-FULL
               PERFORM P1400-CLOSE-PARMCTL THRU P1400-EXIT.
       P1000-EXIT.
           EXIT.
       P1100-OPEN-PARMCTL.
           MOVE 'Y' TO WS-CTL-AVAIL-SW.
           OPEN INPUT PARMCTL.
           IF WS-FS-PARMCTL NOT = '00'
               MOVE 'N' TO WS-CTL-AVAIL-SW
               ADD 1 TO WS-CNT-NO-CTL
               DISPLAY 'CABPARMR - PARMCTL OPEN STATUS '
                   WS-FS-PARMCTL.
       P1100-EXIT.
           EXIT.
       P1200-READ-PARMCTL.
           READ PARMCTL
               AT END MOVE 'Y' TO WS-CTL-EOF-SW.
           IF WS-FS-PARMCTL NOT = '00' AND
              WS-FS-PARMCTL NOT = '10'
               MOVE 'Y' TO WS-CTL-EOF-SW
               DISPLAY 'CABPARMR - PARMCTL READ STATUS '
                   WS-FS-PARMCTL.
       P1200-EXIT.
           EXIT.
       P1300-STORE-ROW.
           ADD 1 TO WS-CTL-CNT.
           MOVE WS-CTL-CNT TO WS-SUB-04.
           MOVE PT-PROCESS-ID TO WS-CTL-PROCESS-ID (WS-SUB-04).
           MOVE PT-CYCLE-YYDDD TO WS-CTL-CYCLE-YYDDD (WS-SUB-04).
           MOVE PT-BILL-PERIOD TO WS-CTL-BILL-PERIOD (WS-SUB-04).
           MOVE PT-TARIFF-CD TO WS-CTL-TARIFF-CD (WS-SUB-04).
           MOVE PT-RUN-ID TO WS-CTL-RUN-ID (WS-SUB-04).
           MOVE PT-DEFAULT-SW TO WS-CTL-DEFAULT-SW (WS-SUB-04).
           IF WS-CTL-CNT NOT < WS-CTL-LIMIT
               MOVE 'Y' TO WS-CTL-FULL-SW.
           PERFORM P1200-READ-PARMCTL THRU P1200-EXIT.
       P1300-EXIT.
           EXIT.
       P1400-CLOSE-PARMCTL.
           CLOSE PARMCTL.
           IF WS-FS-PARMCTL NOT = '00'
               DISPLAY 'CABPARMR - PARMCTL CLOSE STATUS '
                   WS-FS-PARMCTL.
       P1400-EXIT.
           EXIT.
      * S200-CLASSIFICATION SECTION - THE TWO FORMS ARE TOLD APART BY
      * THE PRESENCE OF AN EQUALS SIGN. POSITIONAL CARRIES NONE.
       S200-CLASSIFICATION SECTION.
       P2000-CLASSIFY-CARD.
           ADD 1 TO WS-CNT-CARDS.
           PERFORM P2050-RESET-SWITCHES THRU P2050-EXIT.
           IF WS-CARD = SPACES
               MOVE 'Y' TO WS-BLANK-CARD-SW.
           MOVE 0 TO WS-EQ-CNT.
           INSPECT WS-CARD TALLYING WS-EQ-CNT FOR ALL '='.
           IF WS-EQ-CNT > 0
               MOVE 'Y' TO WS-KEYWORD-SW
               ADD 1 TO WS-CNT-KEYWORD
               PERFORM P2100-SPLIT-KEYWORDS THRU P2100-EXIT
           ELSE
               ADD 1 TO WS-CNT-POSITIONAL.
       P2000-EXIT.
           EXIT.
       P2050-RESET-SWITCHES.
           MOVE 'N' TO WS-KEYWORD-SW.
           MOVE 'N' TO WS-BLANK-CARD-SW.
           MOVE 'N' TO WS-CYCLE-BAD-SW.
           MOVE 'N' TO WS-TARIFF-BAD-SW.
           MOVE 'N' TO WS-TARIFF-FOUND-SW.
           MOVE 'N' TO WS-NEED-CYCLE-SW.
           MOVE 'N' TO WS-NEED-PERIOD-SW.
           MOVE 'N' TO WS-NEED-TARIFF-SW.
           MOVE 'N' TO WS-NEED-RUNID-SW.
           MOVE 'N' TO WS-DEFAULTED-SW.
           MOVE 'N' TO WS-ROW-FOUND-SW.
           MOVE 1 TO WS-ROW-SUB.
       P2050-EXIT.
           EXIT.
      * THE PROCESS ID IN COLUMNS 69 THROUGH 76 IS CARRIED FORWARD SO
      * THAT A KEYWORD CARD PUNCHED OVER A POSITIONAL ONE STILL
      * MATCHES THE DEFAULT TABLE.
       P2100-SPLIT-KEYWORDS.
           MOVE SPACES TO WS-KW-TABLE.
           MOVE 0 TO WS-KW-CNT.
           UNSTRING WS-CARD DELIMITED BY ','
               INTO WS-KW-TEXT (1) WS-KW-TEXT (2) WS-KW-TEXT (3)
                    WS-KW-TEXT (4) WS-KW-TEXT (5) WS-KW-TEXT (6)
                    WS-KW-TEXT (7) WS-KW-TEXT (8)
               TALLYING IN WS-KW-CNT.
           MOVE WC-PROCESS-ID TO WS-SAVE-PROCESS-ID.
           MOVE SPACES TO WS-CARD.
           MOVE WS-SAVE-PROCESS-ID TO WC-PROCESS-ID.
           IF WS-KW-CNT > 8
               MOVE 8 TO WS-KW-CNT.
           PERFORM P2200-APPLY-PAIR THRU P2200-EXIT
               VARYING WS-SUB-03 FROM 1 BY 1
               UNTIL WS-SUB-03 > WS-KW-CNT.
       P2100-EXIT.
           EXIT.
       P2200-APPLY-PAIR.
           MOVE SPACES TO WS-KW-NAME.
           MOVE SPACES TO WS-KW-VALUE.
           UNSTRING WS-KW-TEXT (WS-SUB-03) DELIMITED BY '='
               INTO WS-KW-NAME WS-KW-VALUE.
           IF WS-KW-NAME = 'CYCLE'
               MOVE WS-KW-VALUE TO WC-CYCLE-YYDDD
           ELSE
               IF WS-KW-NAME = 'PERIOD'
                   MOVE WS-KW-VALUE TO WC-BILL-PERIOD
               ELSE
                   IF WS-KW-NAME = 'TARIFF'
                       MOVE WS-KW-VALUE TO WC-TARIFF-CD
                   ELSE
                       IF WS-KW-NAME = 'RUNID'
                           MOVE WS-KW-VALUE TO WC-RUN-ID
                       ELSE
                           IF WS-KW-NAME = 'PROCID'
                               MOVE WS-KW-VALUE TO WC-PROCESS-ID
                           ELSE
                               ADD 1 TO WS-CNT-UNKNOWN-KW.
       P2200-EXIT.
           EXIT.
      * S300-VALIDATION SECTION - ONE PARAGRAPH PER FIELD.
       S300-VALIDATION SECTION.
       P3000-VALIDATE-CARD.
           PERFORM P3100-EDIT-CYCLE THRU P3100-EXIT.
           PERFORM P3200-EDIT-PERIOD THRU P3200-EXIT.
           PERFORM P3300-EDIT-TARIFF THRU P3300-EXIT.
           PERFORM P3400-EDIT-RUN-ID THRU P3400-EXIT.
       P3000-EXIT.
           EXIT.
       P3100-EDIT-CYCLE.
           IF WC-CYCLE-YYDDD = SPACES
               MOVE 'Y' TO WS-NEED-CYCLE-SW
           ELSE
               PERFORM P3110-TEST-CYCLE THRU P3110-EXIT.
       P3100-EXIT.
           EXIT.
      * INSPECT COUNTS THE DIGITS SO A CARD PUNCHED WITH EMBEDDED
      * BLANKS IS REJECTED RATHER THAN PASSED ON.
       P3110-TEST-CYCLE.
           MOVE WC-CYCLE-YYDDD TO WS-CYCLE-TEST.
           MOVE 0 TO WS-DIGIT-CNT.
           INSPECT WS-CYCLE-TEST TALLYING WS-DIGIT-CNT
               FOR ALL '0' ALL '1' ALL '2' ALL '3' ALL '4'
                   ALL '5' ALL '6' ALL '7' ALL '8' ALL '9'.
           IF WS-DIGIT-CNT NOT = 5
               MOVE 'Y' TO WS-CYCLE-BAD-SW
               ADD 1 TO WS-CNT-REJECTED
           ELSE
               PERFORM P3120-TEST-DAY THRU P3120-EXIT.
       P3110-EXIT.
           EXIT.
      * DAY 366 IS ACCEPTED HERE. THE LEAP TEST BELONGS TO CABDTCNV.
       P3120-TEST-DAY.
           IF WS-CY-DDD < 1 OR WS-CY-DDD > 366
               MOVE 'Y' TO WS-CYCLE-BAD-SW
               ADD 1 TO WS-CNT-REJECTED.
       P3120-EXIT.
           EXIT.
       P3200-EDIT-PERIOD.
           IF WC-BILL-PERIOD = SPACES
               MOVE 'Y' TO WS-NEED-PERIOD-SW
           ELSE
               MOVE WC-BILL-PERIOD TO WS-PERIOD-TEST
               IF WS-PERIOD-TEST NOT NUMERIC
                   MOVE 'Y' TO WS-NEED-PERIOD-SW.
       P3200-EXIT.
           EXIT.
       P3300-EDIT-TARIFF.
           IF WC-TARIFF-CD = SPACES
               MOVE 'Y' TO WS-NEED-TARIFF-SW
           ELSE
               PERFORM P3310-SCAN-TARIFF THRU P3310-EXIT.
       P3300-EXIT.
           EXIT.
      * TWELVE ROWS WALKED WITH A SUBSCRIPT. THE LIST IS IN FILING
      * ORDER, NOT CODE ORDER, SO SEARCH ALL CANNOT BE USED.
       P3310-SCAN-TARIFF.
           MOVE 'N' TO WS-TARIFF-FOUND-SW.
           PERFORM P3320-COMPARE-TARIFF THRU P3320-EXIT
               VARYING WS-SUB-01 FROM 1 BY 1
               UNTIL WS-SUB-01 > WS-TARIFF-LIMIT
                  OR WS-TARIFF-FOUND.
           IF NOT WS-TARIFF-FOUND
               MOVE 'Y' TO WS-TARIFF-BAD-SW.
       P3310-EXIT.
           EXIT.
       P3320-COMPARE-TARIFF.
           IF WC-TARIFF-CD = WS-TARIFF-ENTRY (WS-SUB-01)
               MOVE 'Y' TO WS-TARIFF-FOUND-SW.
       P3320-EXIT.
           EXIT.
       P3400-EDIT-RUN-ID.
           IF WC-RUN-ID = SPACES
               MOVE 'Y' TO WS-NEED-RUNID-SW.
       P3400-EXIT.
           EXIT.
      * S400-DEFAULTS SECTION - PARMCTL SUBSTITUTION.
       S400-DEFAULTS SECTION.
       P4000-APPLY-DEFAULTS.
           IF WS-NEED-CYCLE OR WS-NEED-PERIOD OR WS-NEED-TARIFF
              OR WS-NEED-RUNID
               PERFORM P4100-LOCATE-ROW THRU P4100-EXIT.
           IF WS-ROW-FOUND
               PERFORM P4200-SUBSTITUTE THRU P4200-EXIT.
       P4000-EXIT.
           EXIT.
      * THE PROCESS ID IN COLUMNS 69 THROUGH 76 SELECTS THE ROW. A
      * NUMBER OF CALLERS LEAVE THOSE COLUMNS BLANK AND TAKE THE
      * FIRST ROW FLAGGED D, WHICH IS THE ESTATE DEFAULT ROW.
       P4100-LOCATE-ROW.
           MOVE 'N' TO WS-ROW-FOUND-SW.
           MOVE 1 TO WS-ROW-SUB.
           IF WS-CTL-AVAIL
               PERFORM P4110-MATCH-PROCESS THRU P4110-EXIT.
           IF WS-CTL-AVAIL
               IF NOT WS-ROW-FOUND
                   PERFORM P4120-TAKE-ESTATE-ROW THRU P4120-EXIT.
       P4100-EXIT.
           EXIT.
       P4110-MATCH-PROCESS.
           IF WC-PROCESS-ID NOT = SPACES
               PERFORM P4115-COMPARE-PROCESS THRU P4115-EXIT
                   VARYING WS-SUB-02 FROM 1 BY 1
                   UNTIL WS-SUB-02 > WS-CTL-CNT
                      OR WS-ROW-FOUND.
       P4110-EXIT.
           EXIT.
       P4115-COMPARE-PROCESS.
           IF WC-PROCESS-ID = WS-CTL-PROCESS-ID (WS-SUB-02)
               MOVE 'Y' TO WS-ROW-FOUND-SW
               MOVE WS-SUB-02 TO WS-ROW-SUB.
       P4115-EXIT.
           EXIT.
       P4120-TAKE-ESTATE-ROW.
           PERFORM P4125-COMPARE-DEFAULT THRU P4125-EXIT
               VARYING WS-SUB-02 FROM 1 BY 1
               UNTIL WS-SUB-02 > WS-CTL-CNT
                  OR WS-ROW-FOUND.
       P4120-EXIT.
           EXIT.
       P4125-COMPARE-DEFAULT.
           IF WS-CTL-DEFAULT-SW (WS-SUB-02) = 'D'
               MOVE 'Y' TO WS-ROW-FOUND-SW
               MOVE WS-SUB-02 TO WS-ROW-SUB.
       P4125-EXIT.
           EXIT.
      * ONLY BLANK FIELDS ARE TAKEN FROM THE ROW. A CYCLE TAKEN FROM
      * PARMCTL IS EDITED AGAIN BEFORE THE CARD IS BUILT.
       P4200-SUBSTITUTE.
           IF WS-NEED-CYCLE
               MOVE WS-CTL-CYCLE-YYDDD (WS-ROW-SUB) TO WC-CYCLE-YYDDD
               MOVE 'Y' TO WS-DEFAULTED-SW
               ADD 1 TO WS-CNT-DEFAULTED.
           IF WS-NEED-PERIOD
               MOVE WS-CTL-BILL-PERIOD (WS-ROW-SUB) TO WC-BILL-PERIOD
               MOVE 'Y' TO WS-DEFAULTED-SW
               ADD 1 TO WS-CNT-DEFAULTED.
           IF WS-NEED-TARIFF
               MOVE WS-CTL-TARIFF-CD (WS-ROW-SUB) TO WC-TARIFF-CD
               MOVE 'Y' TO WS-DEFAULTED-SW
               ADD 1 TO WS-CNT-DEFAULTED.
           IF WS-NEED-RUNID
               MOVE WS-CTL-RUN-ID (WS-ROW-SUB) TO WC-RUN-ID
               MOVE 'Y' TO WS-DEFAULTED-SW
               ADD 1 TO WS-CNT-DEFAULTED.
           IF WS-NEED-CYCLE
               PERFORM P3110-TEST-CYCLE THRU P3110-EXIT.
       P4200-EXIT.
           EXIT.
      * S500-REBUILD SECTION - THE CARD GOES BACK POSITIONAL. A FIELD
      * STILL BLANK IS ZERO FILLED FOR THE CALLER NUMERIC REDEFINES.
       S500-REBUILD SECTION.
       P5000-REBUILD-CARD.
           IF WC-CYCLE-YYDDD = SPACES
               MOVE '00000' TO WC-CYCLE-YYDDD.
           IF WC-BILL-PERIOD = SPACES
               MOVE '000000' TO WC-BILL-PERIOD.
           MOVE SPACES TO LK-PARM-CARD.
           STRING WC-CYCLE-YYDDD DELIMITED BY SIZE
                  WC-BILL-PERIOD DELIMITED BY SIZE
                  WC-TARIFF-CD DELIMITED BY SIZE
                  WC-RUN-ID DELIMITED BY SIZE
                  WS-FILL-41 DELIMITED BY SIZE
                  WC-PROCESS-ID DELIMITED BY SIZE
                  WS-FILL-04 DELIMITED BY SIZE
               INTO LK-PARM-CARD.
           PERFORM P5100-SET-RETURN-CODE THRU P5100-EXIT.
       P5000-EXIT.
           EXIT.
       P5100-SET-RETURN-CODE.
           MOVE 0 TO LK-PARM-RC.
           IF WS-KEYWORD-FORM
               MOVE 8 TO LK-PARM-RC.
           IF WS-DEFAULTED
               MOVE 4 TO LK-PARM-RC.
           IF WS-TARIFF-BAD
               MOVE 20 TO LK-PARM-RC.
           IF WS-CYCLE-BAD
               MOVE 12 TO LK-PARM-RC.
           IF WS-BLANK-CARD AND NOT WS-CTL-AVAIL
               MOVE 24 TO LK-PARM-RC.
           IF WS-BLANK-CARD AND NOT WS-ROW-FOUND
               MOVE 16 TO LK-PARM-RC.
       P5100-EXIT.
           EXIT.
      * S900-TERMINATION SECTION - A CARD OF 99999 IN COLUMNS 1
      * THROUGH 5 IS PUNCHED BY THE JOB AT END OF STEP AND ASKS FOR
      * THE TOTALS. NO CARD IS BUILT AND THE RETURN CODE STAYS ZERO.
       S900-TERMINATION SECTION.
       P9500-DISPLAY-TOTALS.
           DISPLAY 'CABPARMR ' WS-PGM-VERSION ' - PARM CARD TOTALS'.
           DISPLAY '  CARDS READ        = ' WS-CNT-CARDS.
           DISPLAY '  POSITIONAL FORM   = ' WS-CNT-POSITIONAL.
           DISPLAY '  KEYWORD FORM      = ' WS-CNT-KEYWORD.
           DISPLAY '  FIELDS DEFAULTED  = ' WS-CNT-DEFAULTED.
           DISPLAY '  CARDS REJECTED    = ' WS-CNT-REJECTED.
           DISPLAY '  KEYWORDS IGNORED  = ' WS-CNT-UNKNOWN-KW.
           DISPLAY '  PARMCTL NOT OPEN  = ' WS-CNT-NO-CTL.
           DISPLAY '  PARMCTL ROWS HELD = ' WS-CTL-CNT.
       P9500-EXIT.
           EXIT.
