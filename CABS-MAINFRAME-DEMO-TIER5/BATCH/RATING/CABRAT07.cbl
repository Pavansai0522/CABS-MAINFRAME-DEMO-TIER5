      ******************************************************************
      * CABRAT07 - MINIMUM AND MAXIMUM CHARGE APPLICATION              *
      * APPLICATION : CABS                                             *
      * INPUTS      : DDNAME  DSN                     COPYBOOK         *
      *               RATIN   TELCABS.CABS.RATED(0)    (LOCAL)         *
      *               RATEMST TELCABS.CABS.RATE (VSAM) CABSRATE        *
      * OUTPUTS     : DDNAME  DSN                     COPYBOOK         *
      *               RATOUT  TELCABS.CABS.RATED(+1)   (LOCAL)         *
      *               MMXOUT  TELCABS.CABS.RATED.MINMAX(+1)(LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A            CABSPRNT       *
      * CONTROL     : CTLOUT                          CABSCTL          *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +             *
      *               CT-SUMMARISED + CT-CARRIED-FWD                   *
      * RESTART     : FULL RERUN                                       *
      * REVISION HISTORY                                               *
      *   V1.00 1990-05-14 R.T.WHEELER  MAXIMUM (CEILING) TEST         *
      *                    ONLY, SPECIAL ACCESS ELEMENTS               *
      *   V1.02 1993-11-09 D.OKONKWO    MINIMUM (FLOOR) TEST           *
      *                    ADDED, PER CIRCUIT PER MONTH                *
      *   V1.05 1997-03-22 J.M.CASTILLO EXTENDED TO ALL SWITCHED       *
      *                    ACCESS ELEMENTS                             *
      *   V1.08 2000-08-17 P.NAIR       MAKE-UP NOW A SEPARATE         *
      *                    LINE, NOT AN IN-PLACE RESTATEMENT           *
      *   V1.11 2004-02-05 A.BUKOWSKI   MMXOUT AUDIT TRAIL ADDED       *
      *   V1.14 2008-06-30 S.MARCHETTI  RATE TABLE PRELOAD             *
      *                    REPLACED PER-RECORD RANDOM READ             *
      *   V1.17 2013-01-18 L.FERREIRA   CREDIT ROUNDING CHANGED        *
      *                    TO TRUNCATE PER FD-118                      *
      *   V1.19 2019-10-08 G.PRZYBYLSKI RECOMPILE ONLY - LE V8         *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABRAT07.
       AUTHOR. TELCABS APPLICATIONS - RATING TEAM.
      ******************************************************************
      * MINIMUM/MAXIMUM CHARGE APPLICATION.  MINIMUM IS TESTED PER     *
      * CIRCUIT PER MONTH (INNER BREAK); MAXIMUM PER BAN PER RATE      *
      * ELEMENT PER MONTH (OUTER BREAK).  RATIN MUST ARRIVE SORTED     *
      * OCN/BAN/RATE-ELEM/CIRCUIT-ID SO THE TWO BREAKS NEST.           *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RATIN ASSIGN TO UT-S-RATIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT RATEMST ASSIGN TO DA-RATEMST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS RT-KEY
               FILE STATUS IS WS-FS-TABLE.
           SELECT RATOUT ASSIGN TO UT-S-RATOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT MMXOUT ASSIGN TO UT-S-MMXOUT
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
      * RATIN - RATED-ELEMENT INPUT.  LOCAL LAYOUT, SHARED SHAPE       *
      * WITH RATOUT/MMXOUT (SEE THOSE FDs FOR WHY).                    *
       FD  RATIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       01  CABS-RATIN-RECORD.
           05  RI-OCN                      PIC X(04).
           05  RI-BAN                      PIC X(13).
           05  RI-BILL-PERIOD              PIC 9(06).
           05  RI-SECTION                  PIC X(02).
           05  RI-SEQ-NBR                  PIC 9(09) COMP-3.
           05  RI-CIRCUIT-ID               PIC X(20).
           05  RI-JURIS-CD                 PIC X(01).
           05  RI-STATE-CD                 PIC X(02).
           05  RI-RATE-ELEM                PIC X(06).
           05  RI-QTY                      PIC S9(13)V9(02)
               COMP-3.
           05  RI-RATE                     PIC S9(05)V9(05)
               COMP-3.
           05  RI-AMOUNT                   PIC S9(11)V9(05)
               COMP-3.
           05  RI-ROUND-RULE               PIC X(01).
           05  RI-SRC-PROCESS              PIC X(08).
           05  RI-LINE-TYPE                PIC X(01).
           05  RI-DESCRIPTION              PIC X(60).
           05  RI-AUTH-REF                 PIC X(20).
           05  RI-FILLER                   PIC X(28).
      * RATEMST - ACCESS RATE TABLE, VSAM KSDS.  BROWSED IN FULL AT    *
      * INIT INTO THE R2 TABLE - SEE P1300.                            *
       FD  RATEMST
           LABEL RECORDS ARE STANDARD.
       COPY CABSRATE.
      * RATOUT - PASS-THROUGH PLUS MAKE-UP/CREDIT LINES.  SAME 200-    *
      * BYTE SHAPE AS RATIN, REDECLARED - A GROUP MOVE BETWEEN THIS    *
      * AND CABS-MMXOUT-RECORD (SEE P7100/P7200) THEREFORE WORKS.      *
       FD  RATOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       01  CABS-RATOUT-RECORD.
           05  RO-OCN                      PIC X(04).
           05  RO-BAN                      PIC X(13).
           05  RO-BILL-PERIOD              PIC 9(06).
           05  RO-SECTION                  PIC X(02).
           05  RO-SEQ-NBR                  PIC 9(09) COMP-3.
           05  RO-CIRCUIT-ID               PIC X(20).
           05  RO-JURIS-CD                 PIC X(01).
           05  RO-STATE-CD                 PIC X(02).
           05  RO-RATE-ELEM                PIC X(06).
           05  RO-QTY                      PIC S9(13)V9(02)
               COMP-3.
           05  RO-RATE                     PIC S9(05)V9(05)
               COMP-3.
           05  RO-AMOUNT                   PIC S9(11)V9(05)
               COMP-3.
           05  RO-ROUND-RULE               PIC X(01).
           05  RO-SRC-PROCESS              PIC X(08).
           05  RO-LINE-TYPE                PIC X(01).
           05  RO-DESCRIPTION              PIC X(60).
           05  RO-AUTH-REF                 PIC X(20).
           05  RO-FILLER                   PIC X(28).
      * MMXOUT - MIN/MAX ADJUSTMENT AUDIT TRAIL, MIRROR OF THE         *
      * RATOUT ADJUSTMENT LINES.                                       *
       FD  MMXOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       01  CABS-MMXOUT-RECORD.
           05  RM-OCN                      PIC X(04).
           05  RM-BAN                      PIC X(13).
           05  RM-BILL-PERIOD              PIC 9(06).
           05  RM-SECTION                  PIC X(02).
           05  RM-SEQ-NBR                  PIC 9(09) COMP-3.
           05  RM-CIRCUIT-ID               PIC X(20).
           05  RM-JURIS-CD                 PIC X(01).
           05  RM-STATE-CD                 PIC X(02).
           05  RM-RATE-ELEM                PIC X(06).
           05  RM-QTY                      PIC S9(13)V9(02)
               COMP-3.
           05  RM-RATE                     PIC S9(05)V9(05)
               COMP-3.
           05  RM-AMOUNT                   PIC S9(11)V9(05)
               COMP-3.
           05  RM-ROUND-RULE               PIC X(01).
           05  RM-SRC-PROCESS              PIC X(08).
           05  RM-LINE-TYPE                PIC X(01).
           05  RM-DESCRIPTION              PIC X(60).
           05  RM-AUTH-REF                 PIC X(20).
           05  RM-FILLER                   PIC X(28).
      * CTLOUT - RUN CONTROL / BALANCING RECORD.                       *
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD             PIC X(180).
      * RPTOUT - PRINT REPORT.                                         *
       FD  RPTOUT
           RECORDING MODE IS FA
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       COPY CABSPRNT.
       WORKING-STORAGE SECTION.
      * STANDARD SHARED WORKING STORAGE - SEE CABSWRK.                 *
       COPY CABSWRK.
      * RATING FAMILY CONTROL BLOCKS - ONE COPY PULLS ALL FOUR.        *
       COPY CABSRT01.
      * PROGRAM CONSTANTS / LINE-TYPE LITERALS / SYSIN PARM CARD.      *
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE
               'CABRAT07'.
           05  WS-LT-DETAIL                PIC X(01) VALUE 'D'.
           05  WS-LT-MAKEUP                PIC X(01) VALUE 'M'.
           05  WS-LT-CREDIT                PIC X(01) VALUE 'C'.
       01  WS-PARM-CARD                PIC X(80).
       01  WS-PARM-CARD-R1 REDEFINES WS-PARM-CARD.
           05  PC1-CYCLE-YYDDD             PIC 9(05).
           05  PC1-BILL-PERIOD             PIC 9(06).
           05  PC1-TARIFF-CD               PIC X(04).
           05  PC1-RUN-ID                  PIC X(12).
           05  PC1-FILLER                  PIC X(53).
      * LEVEL-BREAK KEYS.  OUTER = BAN/ELEM DRIVES THE MAXIMUM         *
      * TEST; INNER = BAN/ELEM/CIRCUIT DRIVES THE MINIMUM TEST.        *
      * THE FLAT REDEFINES LET P2200 COMPARE EACH GROUP IN ONE TEST.   *
       01  WS-OUTER-BREAK-KEY.
           05  WS-OBK-BAN                  PIC X(13).
           05  WS-OBK-RATE-ELEM            PIC X(06).
       01  WS-OUTER-BREAK-FLAT REDEFINES WS-OUTER-BREAK-KEY.
           05  WS-OBK-FLAT-VIEW            PIC X(19).
       01  WS-INNER-BREAK-KEY.
           05  WS-IBK-BAN                  PIC X(13).
           05  WS-IBK-RATE-ELEM            PIC X(06).
           05  WS-IBK-CIRCUIT-ID           PIC X(20).
       01  WS-INNER-BREAK-FLAT REDEFINES WS-INNER-BREAK-KEY.
           05  WS-IBK-FLAT-VIEW            PIC X(39).
       01  WS-PREV-KEY-SAVE.
           05  WS-PK-OUTER-KEY             PIC X(19).
           05  WS-PK-INNER-KEY             PIC X(39).
           05  WS-PK-FIRST-SW              PIC X(01) VALUE 'Y'.
               88  WS-PK-FIRST-RECORD          VALUE 'Y'.
           05  WS-BRK-INNER-SW             PIC X(01) VALUE 'N'.
           05  WS-BRK-OUTER-SW             PIC X(01) VALUE 'N'.
      * CIRCUIT ACCUMULATOR (INNER/MINIMUM) AND ELEMENT ACCUMULATOR    *
      * (OUTER/MAXIMUM).  ELEMENT ACCUMULATES THE POST-MINIMUM         *
      * ADJUSTED TOTAL FOR EACH CLOSED CIRCUIT - SEE P2300.            *
       01  WS-CIRCUIT-ACCUM.
           05  WS-CCT-AMOUNT               PIC S9(11)V9(05)
               COMP-3 VALUE 0.
           05  WS-CCT-ADJUSTED-TOTAL       PIC S9(11)V9(05)
               COMP-3 VALUE 0.
           05  WS-CCT-MIN-CHG              PIC S9(07)V9(02)
               COMP-3 VALUE 0.
           05  WS-CCT-SAVE-OCN             PIC X(04).
           05  WS-CCT-SAVE-BAN             PIC X(13).
           05  WS-CCT-SAVE-CIRCUIT         PIC X(20).
           05  WS-CCT-SAVE-ELEM            PIC X(06).
           05  WS-CCT-SAVE-JURIS           PIC X(01).
           05  WS-CCT-SAVE-STATE           PIC X(02).
           05  WS-CCT-SAVE-SECTION         PIC X(02).
           05  WS-CCT-SAVE-BILL-PERIOD     PIC 9(06).
           05  WS-CCT-SAVE-ROUND-RULE      PIC X(01).
       01  WS-ELEMENT-ACCUM.
           05  WS-ELM-AMOUNT               PIC S9(13)V9(05)
               COMP-3 VALUE 0.
           05  WS-ELM-MAX-CHG              PIC S9(11)V9(02)
               COMP-3 VALUE 0.
           05  WS-ELM-SAVE-OCN             PIC X(04).
           05  WS-ELM-SAVE-BAN             PIC X(13).
           05  WS-ELM-SAVE-ELEM            PIC X(06).
           05  WS-ELM-SAVE-JURIS           PIC X(01).
           05  WS-ELM-SAVE-STATE           PIC X(02).
           05  WS-ELM-SAVE-SECTION         PIC X(02).
           05  WS-ELM-SAVE-BILL-PERIOD     PIC 9(06).
           05  WS-ELM-SAVE-ROUND-RULE      PIC X(01).
      * MIN/MAX LOOKUP KEY, RESULT, AND THE TABLE SEARCH WORK USED     *
      * BY P4000/P4100/P4110.  MODE X = EXACT STATE, G = GENERIC.      *
       01  WS-LOOKUP-WORK.
           05  WS-VL-TARIFF                PIC X(04).
           05  WS-VL-ELEM                  PIC X(06).
           05  WS-VL-JURIS                 PIC X(01).
           05  WS-VL-STATE                 PIC X(02).
           05  WS-SEL-FOUND-SW             PIC X(01) VALUE 'N'.
               88  WS-SEL-FOUND                VALUE 'Y'.
           05  WS-SEL-MIN-CHG              PIC S9(07)V9(02)
               COMP-3.
           05  WS-SEL-MAX-CHG              PIC S9(11)V9(02)
               COMP-3.
           05  WS-TS-SUB                   PIC S9(04) COMP-3
               VALUE 0.
           05  WS-TS-MODE                  PIC X(01) VALUE 'X'.
           05  WS-TS-MATCH-SW              PIC X(01) VALUE 'N'.
      * MAKE-UP (ROUNDED) AND CREDIT (TRUNCATED) WORK - THE TWO RULES  *
      * ARE SET BY CABS-STD-041, SEE CONVENTIONS.MD.                   *
       01  WS-MAKEUP-CREDIT-WORK.
           05  WS-MU-AMOUNT                PIC S9(07)V9(02)
               COMP-3 VALUE 0.
           05  WS-MU-TOTAL                 PIC S9(11)V9(02)
               COMP-3 VALUE 0.
           05  WS-CR-AMOUNT                PIC S9(11)V9(02)
               COMP-3 VALUE 0.
           05  WS-CR-TOTAL                 PIC S9(13)V9(02)
               COMP-3 VALUE 0.
           05  WS-MU-LINE-SEQ              PIC S9(07) COMP-3
               VALUE 0.
      * MISC RUN COUNTERS, ABEND WORK, CALL RETURN CODES, VSAM         *
      * BROWSE EOF SWITCH.                                             *
       01  WS-MISC-COUNTERS.
           05  WS-MC-PASSTHRU-CNT          PIC S9(09) COMP-3
               VALUE 0.
           05  WS-MC-MAKEUP-CNT            PIC S9(09) COMP-3
               VALUE 0.
           05  WS-MC-CREDIT-CNT            PIC S9(09) COMP-3
               VALUE 0.
           05  WS-MC-CIRCUITS-TESTED       PIC S9(09) COMP-3
               VALUE 0.
           05  WS-MC-ELEMENTS-TESTED       PIC S9(09) COMP-3
               VALUE 0.
           05  WS-MC-RATE-NOT-FOUND        PIC S9(09) COMP-3
               VALUE 0.
           05  WS-MC-TABLE-ROWS-LOADED     PIC S9(05) COMP-3
               VALUE 0.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9907.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-HASH                  PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
       01  WS-VSAM-BROWSE-WORK.
           05  WS-VB-EOF-SW                PIC X(01) VALUE 'N'.
               88  WS-VB-EOF                   VALUE 'Y'.
       PROCEDURE DIVISION.
       P0000-MAINLINE.
           PERFORM P1000-INIT THRU P1000-EXIT.
           PERFORM P2000-PROCESS THRU P2000-EXIT UNTIL WS-EOF.
           PERFORM P8000-CONTROL THRU P8000-EXIT.
           PERFORM P9000-TERM THRU P9000-EXIT.
           STOP RUN.
      * S100-INITIALISATION SECTION                                    *
       S100-INITIALISATION SECTION.
       P1000-INIT.
           PERFORM P1100-OPEN-FILES THRU P1100-EXIT.
           PERFORM P1200-READ-PARM THRU P1200-EXIT.
           PERFORM P1300-LOAD-RATE-TABLE THRU P1300-EXIT.
           PERFORM P1700-INIT-COUNTERS THRU P1700-EXIT.
           PERFORM P2100-READ-RATIN THRU P2100-EXIT.
       P1000-EXIT.
           EXIT.
       P1100-OPEN-FILES.
           OPEN INPUT RATIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RATIN OPEN FAILED' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN INPUT RATEMST.
           IF WS-FS-TABLE NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RATEMST OPEN FAILED' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RATOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RATOUT OPEN FAILED' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT MMXOUT.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'MMXOUT OPEN FAILED' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CTLOUT OPEN FAILED' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RPTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RPTOUT OPEN FAILED' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
       P1100-EXIT.
           EXIT.
       P1200-READ-PARM.
           MOVE SPACES TO WS-PARM-CARD.
           ACCEPT WS-PARM-CARD FROM SYSIN.
           CALL 'CABPARMR' USING WS-PARM-CARD WS-RC-PARMR.
           MOVE PC1-CYCLE-YYDDD TO R1-CYCLE-YYDDD.
           MOVE PC1-BILL-PERIOD TO R1-BILL-PERIOD.
           MOVE PC1-TARIFF-CD TO R1-TARIFF-CD.
           MOVE PC1-RUN-ID TO R1-RUN-ID.
           MOVE PC1-CYCLE-YYDDD TO DW-CURRENT-YYDDD.
           CALL 'CABDTCNV' USING DW-CURRENT-YYDDD DW-GREG-DATE
               WS-RC-DTCNV.
       P1200-EXIT.
           EXIT.
      * P1300/P1310 - FULL BROWSE OF RATEMST INTO THE R2 TABLE.        *
      * P4100/P4110 SEARCH IT LATER - NO RANDOM VSAM READS DURING      *
      * MAIN PROCESSING.                                               *
       P1300-LOAD-RATE-TABLE.
           MOVE 0 TO R2-ENTRY-CNT.
           MOVE LOW-VALUES TO RT-KEY.
           START RATEMST KEY NOT LESS THAN RT-KEY
               INVALID KEY MOVE 'Y' TO WS-VB-EOF-SW.
           IF NOT WS-VB-EOF
               READ RATEMST NEXT RECORD
                   AT END MOVE 'Y' TO WS-VB-EOF-SW.
           PERFORM P1310-LOAD-ONE-ROW THRU P1310-EXIT
               UNTIL WS-VB-EOF.
       P1300-EXIT.
           EXIT.
       P1310-LOAD-ONE-ROW.
           IF R2-ENTRY-CNT < 600
               ADD 1 TO R2-ENTRY-CNT
               MOVE RT-KEY TO R2-EN-KEY (R2-ENTRY-CNT)
               MOVE RT-INITIAL-RATE TO R2-EN-INITIAL (R2-ENTRY-CNT)
               MOVE RT-ADDL-RATE TO R2-EN-ADDL (R2-ENTRY-CNT)
               MOVE RT-SETUP-CHG TO R2-EN-SETUP (R2-ENTRY-CNT)
               MOVE RT-MIN-CHG TO R2-EN-MIN-CHG (R2-ENTRY-CNT)
               MOVE RT-MAX-CHG TO R2-EN-MAX-CHG (R2-ENTRY-CNT)
               MOVE RT-ROUND-RULE TO R2-EN-ROUND-RULE (R2-ENTRY-CNT)
               MOVE RT-EXP-YYDDD TO R2-EN-EXP-YYDDD (R2-ENTRY-CNT)
               ADD 1 TO WS-MC-TABLE-ROWS-LOADED
           ELSE
               MOVE 'Y' TO R2-TABLE-FULL-SW.
           READ RATEMST NEXT RECORD
               AT END MOVE 'Y' TO WS-VB-EOF-SW.
       P1310-EXIT.
           EXIT.
       P1700-INIT-COUNTERS.
           MOVE 0 TO WS-READ-CNT WS-WRITE-CNT WS-REJECT-CNT
               WS-SUMM-CNT WS-CFWD-CNT.
           MOVE 0 TO WS-ACC-MINUTES WS-ACC-AMOUNT WS-ACC-SEQ-HASH
               WS-ACC-OCN-HASH.
           MOVE 0 TO WS-MC-PASSTHRU-CNT WS-MC-MAKEUP-CNT
               WS-MC-CREDIT-CNT WS-MC-CIRCUITS-TESTED
               WS-MC-ELEMENTS-TESTED WS-MC-RATE-NOT-FOUND.
           MOVE 0 TO WS-MU-TOTAL WS-CR-TOTAL WS-MU-LINE-SEQ.
       P1700-EXIT.
           EXIT.
      * S200-MAIN-PROCESS SECTION - READ-AHEAD LOOP.  LAST GROUP IN    *
      * THE FILE IS CLOSED BY P2900-FINAL-FLUSH FROM P8000-CONTROL.    *
       S200-MAIN-PROCESS SECTION.
       P2000-PROCESS.
           PERFORM P2200-DETECT-BREAKS THRU P2200-EXIT.
           PERFORM P2100-READ-RATIN THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-RATIN.
           READ RATIN
               AT END MOVE 'Y' TO WS-EOF-SW.
           IF NOT WS-EOF
               ADD 1 TO WS-READ-CNT
               CALL 'CABHASH' USING RI-OCN WS-ACC-OCN-HASH
                   ON EXCEPTION MOVE 9999 TO WS-RC-HASH
                   NOT ON EXCEPTION MOVE 0 TO WS-RC-HASH
               ADD RI-QTY TO WS-ACC-MINUTES
               ADD RI-AMOUNT TO WS-ACC-AMOUNT
               ADD RI-SEQ-NBR TO WS-ACC-SEQ-HASH.
       P2100-EXIT.
           EXIT.
      * P2200-DETECT-BREAKS - SEQUENTIAL FLAG STYLE, NOT NESTED        *
      * IF/ELSE, SO EVERY BRANCH STAYS UNAMBIGUOUS UNDER THE 1974      *
      * PERIOD RULES.  INNER BREAK CLOSES THE CIRCUIT (MINIMUM         *
      * TEST) BEFORE THE OUTER BREAK CLOSES THE ELEMENT (MAXIMUM       *
      * TEST), SO THE ADJUSTED CIRCUIT TOTAL FEEDS THE MAXIMUM.        *
       P2200-DETECT-BREAKS.
           MOVE RI-BAN TO WS-OBK-BAN.
           MOVE RI-RATE-ELEM TO WS-OBK-RATE-ELEM.
           MOVE RI-BAN TO WS-IBK-BAN.
           MOVE RI-RATE-ELEM TO WS-IBK-RATE-ELEM.
           MOVE RI-CIRCUIT-ID TO WS-IBK-CIRCUIT-ID.
           MOVE 'N' TO WS-BRK-INNER-SW.
           MOVE 'N' TO WS-BRK-OUTER-SW.
           IF WS-PK-FIRST-RECORD
               MOVE 'Y' TO WS-BRK-INNER-SW.
           IF NOT WS-PK-FIRST-RECORD AND
                   WS-IBK-FLAT-VIEW NOT = WS-PK-INNER-KEY
               MOVE 'Y' TO WS-BRK-INNER-SW.
           IF NOT WS-PK-FIRST-RECORD AND
                   WS-OBK-FLAT-VIEW NOT = WS-PK-OUTER-KEY
               MOVE 'Y' TO WS-BRK-OUTER-SW.
           IF WS-BRK-INNER-SW = 'Y' AND NOT WS-PK-FIRST-RECORD
               PERFORM P2300-CIRCUIT-BREAK THRU P2300-EXIT.
           IF WS-BRK-OUTER-SW = 'Y'
               PERFORM P2400-ELEMENT-BREAK THRU P2400-EXIT.
           IF WS-BRK-OUTER-SW = 'Y'
               PERFORM P2610-START-ELEMENT THRU P2610-EXIT.
           IF WS-BRK-INNER-SW = 'Y'
               PERFORM P2510-START-CIRCUIT THRU P2510-EXIT.
           PERFORM P7000-WRITE-PASSTHROUGH THRU P7000-EXIT.
           PERFORM P2520-ACCUM-CIRCUIT THRU P2520-EXIT.
           MOVE WS-OBK-FLAT-VIEW TO WS-PK-OUTER-KEY.
           MOVE WS-IBK-FLAT-VIEW TO WS-PK-INNER-KEY.
           MOVE 'N' TO WS-PK-FIRST-SW.
       P2200-EXIT.
           EXIT.
      * P2300-CIRCUIT-BREAK - MINIMUM CHARGE TEST.  BELOW RT-MIN-CHG   *
      * GENERATES A ROUNDED MAKE-UP LINE.  RATIN RECORDS THEMSELVES    *
      * ARE NEVER CHANGED.                                             *
       P2300-CIRCUIT-BREAK.
           MOVE WS-CCT-AMOUNT TO WS-CCT-ADJUSTED-TOTAL.
           MOVE WS-CCT-SAVE-ELEM TO WS-VL-ELEM.
           MOVE WS-CCT-SAVE-JURIS TO WS-VL-JURIS.
           MOVE WS-CCT-SAVE-STATE TO WS-VL-STATE.
           PERFORM P4000-RESOLVE-MIN-MAX THRU P4000-EXIT.
           IF WS-SEL-FOUND
               MOVE WS-SEL-MIN-CHG TO WS-CCT-MIN-CHG
           ELSE
               MOVE 0 TO WS-CCT-MIN-CHG
               ADD 1 TO WS-MC-RATE-NOT-FOUND.
           ADD 1 TO WS-MC-CIRCUITS-TESTED.
           IF WS-CCT-MIN-CHG > 0 AND WS-CCT-AMOUNT < WS-CCT-MIN-CHG
               COMPUTE WS-MU-AMOUNT ROUNDED =
                   WS-CCT-MIN-CHG - WS-CCT-AMOUNT
               PERFORM P7100-WRITE-MAKEUP-LINE THRU P7100-EXIT
               ADD WS-MU-AMOUNT TO WS-CCT-ADJUSTED-TOTAL.
           ADD WS-CCT-ADJUSTED-TOTAL TO WS-ELM-AMOUNT.
       P2300-EXIT.
           EXIT.
      * P2400-ELEMENT-BREAK - MAXIMUM CHARGE TEST.  ABOVE RT-MAX-CHG   *
      * GENERATES A TRUNCATED CREDIT LINE (NOT ROUNDED - FD-118).      *
       P2400-ELEMENT-BREAK.
           MOVE WS-ELM-SAVE-ELEM TO WS-VL-ELEM.
           MOVE WS-ELM-SAVE-JURIS TO WS-VL-JURIS.
           MOVE WS-ELM-SAVE-STATE TO WS-VL-STATE.
           PERFORM P4000-RESOLVE-MIN-MAX THRU P4000-EXIT.
           IF WS-SEL-FOUND
               MOVE WS-SEL-MAX-CHG TO WS-ELM-MAX-CHG
           ELSE
               MOVE 0 TO WS-ELM-MAX-CHG.
           ADD 1 TO WS-MC-ELEMENTS-TESTED.
           IF WS-ELM-MAX-CHG > 0 AND WS-ELM-AMOUNT > WS-ELM-MAX-CHG
               COMPUTE WS-CR-AMOUNT =
                   WS-ELM-AMOUNT - WS-ELM-MAX-CHG
               PERFORM P7200-WRITE-CREDIT-LINE THRU P7200-EXIT.
       P2400-EXIT.
           EXIT.
       P2510-START-CIRCUIT.
           MOVE 0 TO WS-CCT-AMOUNT.
           MOVE RI-OCN TO WS-CCT-SAVE-OCN.
           MOVE RI-BAN TO WS-CCT-SAVE-BAN.
           MOVE RI-CIRCUIT-ID TO WS-CCT-SAVE-CIRCUIT.
           MOVE RI-RATE-ELEM TO WS-CCT-SAVE-ELEM.
           MOVE RI-JURIS-CD TO WS-CCT-SAVE-JURIS.
           MOVE RI-STATE-CD TO WS-CCT-SAVE-STATE.
           MOVE RI-SECTION TO WS-CCT-SAVE-SECTION.
           MOVE RI-BILL-PERIOD TO WS-CCT-SAVE-BILL-PERIOD.
           MOVE RI-ROUND-RULE TO WS-CCT-SAVE-ROUND-RULE.
       P2510-EXIT.
           EXIT.
       P2520-ACCUM-CIRCUIT.
           ADD RI-AMOUNT TO WS-CCT-AMOUNT.
       P2520-EXIT.
           EXIT.
       P2610-START-ELEMENT.
           MOVE 0 TO WS-ELM-AMOUNT.
           MOVE RI-OCN TO WS-ELM-SAVE-OCN.
           MOVE RI-BAN TO WS-ELM-SAVE-BAN.
           MOVE RI-RATE-ELEM TO WS-ELM-SAVE-ELEM.
           MOVE RI-JURIS-CD TO WS-ELM-SAVE-JURIS.
           MOVE RI-STATE-CD TO WS-ELM-SAVE-STATE.
           MOVE RI-SECTION TO WS-ELM-SAVE-SECTION.
           MOVE RI-BILL-PERIOD TO WS-ELM-SAVE-BILL-PERIOD.
           MOVE RI-ROUND-RULE TO WS-ELM-SAVE-ROUND-RULE.
       P2610-EXIT.
           EXIT.
      * P2900-FINAL-FLUSH - THE LAST GROUP NEVER TRIGGERS A KEY        *
      * CHANGE, SO ITS BREAKS ARE FORCED HERE FROM P8000-CONTROL.      *
       P2900-FINAL-FLUSH.
           IF NOT WS-PK-FIRST-RECORD
               PERFORM P2300-CIRCUIT-BREAK THRU P2300-EXIT
               PERFORM P2400-ELEMENT-BREAK THRU P2400-EXIT.
       P2900-EXIT.
           EXIT.
      * S400-RATE-RESOLUTION SECTION - LINEAR SEARCH OF R2-ENTRY,      *
      * EXACT STATE FIRST THEN JURISDICTION-GENERIC FALLBACK.          *
       S400-RATE-RESOLUTION SECTION.
       P4000-RESOLVE-MIN-MAX.
           MOVE 'N' TO WS-SEL-FOUND-SW.
           MOVE 0 TO WS-SEL-MIN-CHG.
           MOVE 0 TO WS-SEL-MAX-CHG.
           MOVE R1-TARIFF-CD TO WS-VL-TARIFF.
           MOVE 'X' TO WS-TS-MODE.
           PERFORM P4100-SEARCH-RATE-TABLE THRU P4100-EXIT.
           IF NOT WS-SEL-FOUND
               MOVE 'G' TO WS-TS-MODE
               PERFORM P4100-SEARCH-RATE-TABLE THRU P4100-EXIT.
       P4000-EXIT.
           EXIT.
      * ONE PASS OVER R2-ENTRY IN THE CURRENT WS-TS-MODE.              *
       P4100-SEARCH-RATE-TABLE.
           PERFORM P4110-TEST-ONE-ROW THRU P4110-EXIT
               VARYING WS-TS-SUB FROM 1 BY 1
               UNTIL WS-TS-SUB > R2-ENTRY-CNT OR WS-SEL-FOUND.
       P4100-EXIT.
           EXIT.
      * P4110 - THREE FLAT CONDITIONS, NOT NESTED, SO THE PERIOD       *
      * RULES STAY UNAMBIGUOUS.                                        *
       P4110-TEST-ONE-ROW.
           MOVE 'N' TO WS-TS-MATCH-SW.
           IF R2-EN-TARIFF (WS-TS-SUB) = WS-VL-TARIFF AND
                   R2-EN-ELEM (WS-TS-SUB) = WS-VL-ELEM AND
                   R2-EN-JURIS (WS-TS-SUB) = WS-VL-JURIS AND
                   WS-TS-MODE = 'X' AND
                   R2-EN-STATE (WS-TS-SUB) = WS-VL-STATE
               MOVE 'Y' TO WS-TS-MATCH-SW.
           IF R2-EN-TARIFF (WS-TS-SUB) = WS-VL-TARIFF AND
                   R2-EN-ELEM (WS-TS-SUB) = WS-VL-ELEM AND
                   R2-EN-JURIS (WS-TS-SUB) = WS-VL-JURIS AND
                   WS-TS-MODE = 'G' AND
                   R2-EN-STATE (WS-TS-SUB) = SPACES
               MOVE 'Y' TO WS-TS-MATCH-SW.
           IF WS-TS-MATCH-SW = 'Y'
               MOVE 'Y' TO WS-SEL-FOUND-SW
               MOVE R2-EN-MIN-CHG (WS-TS-SUB) TO WS-SEL-MIN-CHG
               MOVE R2-EN-MAX-CHG (WS-TS-SUB) TO WS-SEL-MAX-CHG.
       P4110-EXIT.
           EXIT.
      * P7000/P7100/P7200 - OUTPUT.  EVERY RATIN RECORD IS COPIED      *
      * UNCHANGED TO RATOUT; ADJUSTMENTS ARE ALWAYS SEPARATE LINES.    *
       P7000-WRITE-PASSTHROUGH.
           MOVE SPACES TO CABS-RATOUT-RECORD.
           MOVE RI-OCN TO RO-OCN.
           MOVE RI-BAN TO RO-BAN.
           MOVE RI-BILL-PERIOD TO RO-BILL-PERIOD.
           MOVE RI-SECTION TO RO-SECTION.
           MOVE RI-SEQ-NBR TO RO-SEQ-NBR.
           MOVE RI-CIRCUIT-ID TO RO-CIRCUIT-ID.
           MOVE RI-JURIS-CD TO RO-JURIS-CD.
           MOVE RI-STATE-CD TO RO-STATE-CD.
           MOVE RI-RATE-ELEM TO RO-RATE-ELEM.
           MOVE RI-QTY TO RO-QTY.
           MOVE RI-RATE TO RO-RATE.
           MOVE RI-AMOUNT TO RO-AMOUNT.
           MOVE RI-ROUND-RULE TO RO-ROUND-RULE.
           MOVE RI-SRC-PROCESS TO RO-SRC-PROCESS.
           MOVE WS-LT-DETAIL TO RO-LINE-TYPE.
           MOVE RI-DESCRIPTION TO RO-DESCRIPTION.
           MOVE RI-AUTH-REF TO RO-AUTH-REF.
           WRITE CABS-RATOUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
           ADD 1 TO WS-MC-PASSTHRU-CNT.
       P7000-EXIT.
           EXIT.
      * P7100 - NOT COUNTED IN WS-WRITE-CNT (SEE P8400) - THE          *
      * BALANCING EQUATION ONLY COVERS THE 1:1 PASS-THROUGH STREAM.    *
       P7100-WRITE-MAKEUP-LINE.
           ADD 1 TO WS-MU-LINE-SEQ.
           ADD WS-MU-AMOUNT TO WS-MU-TOTAL.
           MOVE SPACES TO CABS-RATOUT-RECORD.
           MOVE WS-CCT-SAVE-OCN TO RO-OCN.
           MOVE WS-CCT-SAVE-BAN TO RO-BAN.
           MOVE WS-CCT-SAVE-BILL-PERIOD TO RO-BILL-PERIOD.
           MOVE WS-CCT-SAVE-SECTION TO RO-SECTION.
           MOVE WS-MU-LINE-SEQ TO RO-SEQ-NBR.
           MOVE WS-CCT-SAVE-CIRCUIT TO RO-CIRCUIT-ID.
           MOVE WS-CCT-SAVE-JURIS TO RO-JURIS-CD.
           MOVE WS-CCT-SAVE-STATE TO RO-STATE-CD.
           MOVE WS-CCT-SAVE-ELEM TO RO-RATE-ELEM.
           MOVE 0 TO RO-QTY.
           MOVE 0 TO RO-RATE.
           MOVE WS-MU-AMOUNT TO RO-AMOUNT.
           MOVE WS-CCT-SAVE-ROUND-RULE TO RO-ROUND-RULE.
           MOVE WS-PGM-NAME TO RO-SRC-PROCESS.
           MOVE WS-LT-MAKEUP TO RO-LINE-TYPE.
           MOVE 'MINIMUM CHARGE MAKE-UP - CIRCUIT FLOOR' TO
               RO-DESCRIPTION.
           MOVE SPACES TO RO-AUTH-REF.
           WRITE CABS-RATOUT-RECORD.
           ADD 1 TO WS-MC-MAKEUP-CNT.
           MOVE CABS-RATOUT-RECORD TO CABS-MMXOUT-RECORD.
           WRITE CABS-MMXOUT-RECORD.
       P7100-EXIT.
           EXIT.
       P7200-WRITE-CREDIT-LINE.
           ADD 1 TO WS-MU-LINE-SEQ.
           ADD WS-CR-AMOUNT TO WS-CR-TOTAL.
           MOVE SPACES TO CABS-RATOUT-RECORD.
           MOVE WS-ELM-SAVE-OCN TO RO-OCN.
           MOVE WS-ELM-SAVE-BAN TO RO-BAN.
           MOVE WS-ELM-SAVE-BILL-PERIOD TO RO-BILL-PERIOD.
           MOVE WS-ELM-SAVE-SECTION TO RO-SECTION.
           MOVE WS-MU-LINE-SEQ TO RO-SEQ-NBR.
           MOVE SPACES TO RO-CIRCUIT-ID.
           MOVE WS-ELM-SAVE-JURIS TO RO-JURIS-CD.
           MOVE WS-ELM-SAVE-STATE TO RO-STATE-CD.
           MOVE WS-ELM-SAVE-ELEM TO RO-RATE-ELEM.
           MOVE 0 TO RO-QTY.
           MOVE 0 TO RO-RATE.
           COMPUTE RO-AMOUNT = ZERO - WS-CR-AMOUNT.
           MOVE WS-ELM-SAVE-ROUND-RULE TO RO-ROUND-RULE.
           MOVE WS-PGM-NAME TO RO-SRC-PROCESS.
           MOVE WS-LT-CREDIT TO RO-LINE-TYPE.
           MOVE 'MAXIMUM CHARGE CREDIT - BAN/ELEMENT CEILING' TO
               RO-DESCRIPTION.
           MOVE SPACES TO RO-AUTH-REF.
           WRITE CABS-RATOUT-RECORD.
           ADD 1 TO WS-MC-CREDIT-CNT.
           MOVE CABS-RATOUT-RECORD TO CABS-MMXOUT-RECORD.
           WRITE CABS-MMXOUT-RECORD.
       P7200-EXIT.
           EXIT.
      * S800-CONTROL-BALANCE SECTION - MANDATORY CONTROL STEP PLUS     *
      * THE RUN SUMMARY REPORT ON RPTOUT.                              *
       S800-CONTROL-BALANCE SECTION.
       P8000-CONTROL.
           PERFORM P2900-FINAL-FLUSH THRU P2900-EXIT.
           PERFORM P8100-BUILD-REPORT THRU P8100-EXIT.
           PERFORM P8400-BUILD-CONTROL-REC THRU P8400-EXIT.
           PERFORM P8500-CHECK-BALANCE THRU P8500-EXIT.
           PERFORM P8600-WRITE-CONTROL-REC THRU P8600-EXIT.
       P8000-EXIT.
           EXIT.
       P8100-BUILD-REPORT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '1' TO PC-CC.
           MOVE 'CABRAT07 - MIN/MAX CHARGE APPLICATION' TO PC-TEXT.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE R1-RUN-ID TO PC-TEXT.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS READ' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'CIRCUITS TESTED' TO PC-COL-001-020.
           MOVE WS-MC-CIRCUITS-TESTED TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'ELEMENTS TESTED' TO PC-COL-001-020.
           MOVE WS-MC-ELEMENTS-TESTED TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RATE NOT FOUND' TO PC-COL-001-020.
           MOVE WS-MC-RATE-NOT-FOUND TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '4' TO PC-CC.
           MOVE 'MINIMUM CHARGE MAKE-UP' TO PC-AMT-DESC.
           MOVE WS-MC-MAKEUP-CNT TO PC-AMT-QTY.
           MOVE WS-MU-TOTAL TO PC-AMT-VALUE.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'MAXIMUM CHARGE CREDIT' TO PC-AMT-DESC.
           MOVE WS-MC-CREDIT-CNT TO PC-AMT-QTY.
           MOVE WS-CR-TOTAL TO PC-AMT-VALUE.
           WRITE CABS-PRINT-LINE.
       P8100-EXIT.
           EXIT.
      * P8400 - CT-WRITTEN COVERS ONLY THE 1:1 PASS-THROUGH STREAM,    *
      * SO CT-READ = CT-WRITTEN ALWAYS HOLDS FOR THIS PROGRAM - THE    *
      * MAKE-UP/CREDIT LINES ARE EXTRA OUTPUT, TRACKED SEPARATELY.     *
       P8400-BUILD-CONTROL-REC.
           MOVE R1-RUN-ID TO CT-RUN-ID.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE 1 TO CT-STEP-SEQ.
           MOVE R1-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE R1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE SPACES TO CT-JOBNAME.
           MOVE SPACES TO CT-STEPNAME.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE SPACES TO CT-FILLER.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-ABEND-CD.
       P8400-EXIT.
           EXIT.
      * THE MANDATORY BALANCING TEST.                                  *
       P8500-CHECK-BALANCE.
           IF CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARISED +
               CT-CARRIED-FWD
               MOVE 'B' TO CT-BAL-IND
           ELSE
               MOVE 'O' TO CT-BAL-IND.
       P8500-EXIT.
           EXIT.
      * ONE WRITE OF THE STANDARD 180-BYTE CONTROL RECORD.             *
       P8600-WRITE-CONTROL-REC.
           MOVE CABS-CONTROL-RECORD TO CABS-CTLOUT-RECORD.
           WRITE CABS-CTLOUT-RECORD.
       P8600-EXIT.
           EXIT.
      *****************************************************************
      * S900-TERMINATION SECTION.                                      *
      *****************************************************************
       S900-TERMINATION SECTION.
       P9000-TERM.
           CLOSE RATIN.
           CLOSE RATEMST.
           CLOSE RATOUT.
           CLOSE MMXOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABRAT07 - RUN COMPLETE'.
           DISPLAY '  READ       = ' WS-READ-CNT.
           DISPLAY '  WRITTEN    = ' WS-WRITE-CNT.
           DISPLAY '  MAKE-UP    = ' WS-MC-MAKEUP-CNT.
           DISPLAY '  CREDIT     = ' WS-MC-CREDIT-CNT.
       P9000-EXIT.
           EXIT.
       P9900-FATAL-OPEN.
           MOVE WS-PGM-NAME TO WS-AB-PGM.
           MOVE 9901 TO WS-AB-USER-CODE.
           DISPLAY 'CABRAT07 FATAL OPEN - ' WS-AB-REASON.
           CALL 'CABABEND' USING WS-AB-PGM WS-AB-PARA WS-AB-REASON
               WS-AB-USER-CODE.
           STOP RUN.
       P9900-EXIT.
           EXIT.
