      *****************************************************************
      * CABRAT12 - RATING RETRY AND AUDIT                             *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               RATIN   TELCABS.CABS.RATED(0)         (LOCAL)   *
      *               RATEMST TELCABS.CABS.RATE (VSAM KSDS) CABSRATE  *
      *               AUDLOG  TELCABS.CABS.AUDIT.LOG (READ) (LOCAL)   *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               RTRYOUT TELCABS.CABS.RATED.RETRY(+1)  (LOCAL)   *
      *               SUSOUT  TELCABS.CABS.USAGE.SUSPENSE(+1)CABSERR  *
      *               AUDLOG  TELCABS.CABS.AUDIT.LOG (EXTEND)(LOCAL)  *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +             *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN                                      *
      * REVISION HISTORY                                              *
      *   V1.00  2001-04-09  P.NAIR       INITIAL RELEASE - SINGLE    *
      *                      RETRY, NO AUDIT TRAIL                    *
      *   V1.02  2003-11-30  A.BUKOWSKI   AUDLOG ADDED - APPEND ONLY, *
      *                      ATTEMPT COUNT READ BACK POSITIONALLY     *
      *   V1.05  2007-05-17  S.MARCHETTI  THREE-ATTEMPT LIMIT ADDED,  *
      *                      4TH+ RETRY WAS LOOPING SOME KEYS FOREVER *
      *   V2.00  2016-03-21  M.HOLLIS     CARRIED-FWD NOW INCLUDES    *
      *                      FAILED-BUT-NOT-ABANDONED ATTEMPTS FOR    *
      *                      THE BALANCING EQUATION, WAS OMITTED      *
      *   V2.02  2019-08-13  G.PRZYBYLSKI RECOMPILE ONLY - CABSCTL    *
      *                      RESTART KEY FIELD WIDENED, UNUSED HERE   *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABRAT12.
       AUTHOR. TELCABS APPLICATIONS - RATING TEAM.
      *****************************************************************
      * RETRIES UNRATED RATIN RECORDS AGAINST A REFRESHED RATEMST. *
      * A KEY GETS AT MOST 3 ATTEMPTS, TRACKED BY PHYSICAL POSITION*
      * IN AUDLOG, NOT A STORED COUNT.  PASS 1 (P1300) READS ALL   *
      * OF AUDLOG TO ESTABLISH EACH KEY'S COUNT BEFORE PASS 2      *
      * (P2000) TOUCHES RATIN.  DO NOT REORDER OR COMPACT AUDLOG - *
      * ITS ORDER IS THE ONLY ATTEMPT-SEQUENCE RECORD IN THE ESTATE*
      *****************************************************************
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
           SELECT AUDLOG ASSIGN TO UT-S-AUDLOG
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-AUDLOG.
           SELECT RTRYOUT ASSIGN TO UT-S-RTRYOUT
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
      *****************************************************************
      * RATIN - RATED PASS-THROUGH; CANDIDATES PER P2110.          *
      *****************************************************************
       FD  RATIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       01  CABS-RETRY-CANDIDATE-RECORD.
           05  RC-KEY.
               10  RC-OCN                   PIC X(04).
               10  RC-BAN                   PIC X(13).
               10  RC-SEQ-NBR               PIC 9(09) COMP-3.
           05  RC-RATE-ELEM                 PIC X(06).
           05  RC-JURIS-CD                  PIC X(01).
           05  RC-STATE-CD                  PIC X(02).
           05  RC-RATE-STATUS               PIC X(01).
               88  RC-RATED                  VALUE 'R'.
               88  RC-SUSPENDED               VALUE 'S'.
           05  RC-QTY                       PIC S9(13)V9(02) COMP-3.
           05  RC-AMOUNT                    PIC S9(11)V9(05) COMP-3.
           05  RC-CYCLE-YYDDD               PIC 9(05).
           05  RC-FILLER                    PIC X(150).
      *****************************************************************
      * RATEMST - REFRESHED RATE TABLE, VSAM KSDS, RANDOM/BROWSE.     *
      *****************************************************************
       FD  RATEMST
           LABEL RECORDS ARE STANDARD.
       COPY CABSRATE.
      *****************************************************************
      * AUDLOG - UNKEYED SEQUENTIAL, INPUT PASS 1, EXTEND PASS 2.  *
      *****************************************************************
       FD  AUDLOG
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 300 CHARACTERS.
       01  CABS-AUDIT-LOG-RECORD.
           05  AL-KEY.
               10  AL-OCN                   PIC X(04).
               10  AL-BAN                   PIC X(13).
               10  AL-SEQ-NBR               PIC 9(09) COMP-3.
               10  AL-RATE-ELEM             PIC X(06).
           05  AL-ATTEMPT-NBR                PIC 9(01).
           05  AL-ATTEMPT-YYDDD              PIC 9(05).
           05  AL-RESULT-CD                  PIC X(01).
               88  AL-SUCCESS                 VALUE 'S'.
               88  AL-FAILED                  VALUE 'F'.
               88  AL-ABANDONED               VALUE 'A'.
           05  AL-AMOUNT                     PIC S9(11)V9(05) COMP-3.
           05  AL-RUN-ID                     PIC X(12).
           05  AL-FILLER                     PIC X(244).
      *****************************************************************
      * RTRYOUT - SUCCESSFULLY RE-RATED RECORDS.                      *
      *****************************************************************
       FD  RTRYOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       01  CABS-RTRYOUT-RECORD.
           05  RY-OCN                        PIC X(04).
           05  RY-BAN                        PIC X(13).
           05  RY-SEQ-NBR                    PIC 9(09) COMP-3.
           05  RY-RATE-ELEM                  PIC X(06).
           05  RY-QTY                        PIC S9(13)V9(02) COMP-3.
           05  RY-RATE                       PIC S9(05)V9(05) COMP-3.
           05  RY-AMOUNT                     PIC S9(11)V9(05) COMP-3.
           05  RY-ATTEMPT-NBR                PIC 9(01).
           05  RY-FILLER                     PIC X(154).
      *****************************************************************
      * SUSOUT - ABANDONED AFTER THE THIRD FAILED ATTEMPT.            *
      *****************************************************************
       FD  SUSOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 300 CHARACTERS.
       01  CABS-SUSPENSE-RECORD-FD.
           05  FD-SU-ERR-CODE                PIC X(04).
           05  FD-SU-ERR-SEVERITY             PIC X(01).
           05  FD-SU-DETECT-PGM               PIC X(08).
           05  FD-SU-DETECT-PARA              PIC X(30).
           05  FD-SU-RUN-ID                   PIC X(12).
           05  FD-SU-ORIG-RECORD              PIC X(200).
           05  FD-SU-FILLER                   PIC X(45).
      *****************************************************************
      * CTLOUT - RUN CONTROL / BALANCING RECORD.                      *
      *****************************************************************
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD                PIC X(180).
       WORKING-STORAGE SECTION.
       COPY CABSWRK.
       COPY CABSRT01.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                   PIC X(08) VALUE 'CABRAT12'.
           05  WS-PGM-VERSION                PIC X(05) VALUE 'V2.02'.
           05  WS-MAX-ATTEMPTS                PIC 9(01) VALUE 3.
       01  WS-PARM-CARD                      PIC X(80).
       01  WS-PARM-CARD-R1 REDEFINES WS-PARM-CARD.
           05  PC1-CYCLE-YYDDD               PIC 9(05).
           05  PC1-BILL-PERIOD                PIC 9(06).
           05  PC1-RUN-ID                     PIC X(12).
           05  PC1-FILLER                     PIC X(57).
       01  WS-EXTRA-SWITCHES.
           05  WS-FS-AUDLOG                  PIC X(02) VALUE '00'.
           05  WS-AL-EOF-SW                  PIC X(01) VALUE 'N'.
               88  WS-AL-EOF                  VALUE 'Y'.
      *****************************************************************
      * PASS-1 ATTEMPT TABLE - ONE SLOT PER DISTINCT KEY IN AUDLOG.*
      *****************************************************************
       01  WS-ATTEMPT-TABLE.
           05  WS-AT-CNT                     PIC 9(04) VALUE 0.
           05  WS-AT-ENTRY OCCURS 2000 TIMES INDEXED BY WS-AT-X.
               10  WS-AT-KEY.
                   15  WS-AT-OCN              PIC X(04).
                   15  WS-AT-BAN              PIC X(13).
                   15  WS-AT-SEQ-NBR          PIC 9(09) COMP-3.
                   15  WS-AT-RATE-ELEM        PIC X(06).
               10  WS-AT-COUNT                PIC 9(01) VALUE 0.
       01  WS-ATTEMPT-SEARCH-WORK.
           05  WS-AS-FOUND-SW                PIC X(01) VALUE 'N'.
               88  WS-AS-FOUND                 VALUE 'Y'.
           05  WS-AS-TABLE-FULL-SW           PIC X(01) VALUE 'N'.
               88  WS-AS-TABLE-FULL            VALUE 'Y'.
           05  WS-AS-CURRENT-COUNT           PIC 9(01) VALUE 0.
       01  WS-RETRY-WORK.
           05  WS-RW-CANDIDATE-SW            PIC X(01) VALUE 'N'.
               88  WS-RW-CANDIDATE             VALUE 'Y'.
           05  WS-RW-FOUND-RATE-SW           PIC X(01) VALUE 'N'.
               88  WS-RW-FOUND-RATE            VALUE 'Y'.
           05  WS-RW-RATE                    PIC S9(05)V9(05) COMP-3
                                                            VALUE 0.
           05  WS-RW-AMOUNT                  PIC S9(11)V9(05) COMP-3
                                                            VALUE 0.
       01  WS-RTFMT-CALL-WORK.
           05  WS-RTFMT-RATE-IN              PIC S9(13)V9(05) COMP-3.
           05  WS-RTFMT-RULE-IN              PIC X(01).
           05  WS-RTFMT-OUT                  PIC S9(13)V9(02) COMP-3.
       01  WS-QTY-DISPLAY                    PIC S9(13)V9(02).
       01  WS-QTY-DISPLAY-R REDEFINES WS-QTY-DISPLAY.
           05  WS-QD-WHOLE                   PIC 9(11).
           05  WS-QD-FRACTION                PIC 9(02).
       01  WS-CALL-RC-AREA.
           05  WS-RC-PARMR                   PIC 9(04).
           05  WS-RC-RTFMT                   PIC 9(04).
           05  WS-RC-HASH                    PIC 9(04).
           05  WS-RC-ERRWR                   PIC 9(04).
       01  WS-HASH-CALL-WORK.
           05  WS-HC-MINUTES-IN              PIC S9(15)V9(02) COMP-3.
           05  WS-HC-AMOUNT-IN               PIC S9(13)V9(05) COMP-3.
           05  WS-HC-SEQ-IN                  PIC S9(17)       COMP-3.
       01  WS-MISC-COUNTERS.
           05  WS-MC-CANDIDATES-CNT          PIC S9(09) COMP-3 VALUE 0.
           05  WS-MC-SUCCESS-CNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-MC-FAILED-CNT              PIC S9(09) COMP-3 VALUE 0.
           05  WS-MC-ABANDONED-CNT           PIC S9(09) COMP-3 VALUE 0.
           05  WS-MC-PASSTHRU-CNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-MC-TABLE-FULL-CNT          PIC S9(09) COMP-3 VALUE 0.
           05  WS-MC-CHECKPOINT-QUOT         PIC S9(09) COMP-3 VALUE 0.
           05  WS-MC-CHECKPOINT-REM          PIC S9(09) COMP-3 VALUE 0.
       01  WS-ABEND-WORK.
           05  WS-AB-PARA                    PIC X(30).
           05  WS-AB-REASON                  PIC X(60).
       PROCEDURE DIVISION.
       P0000-MAINLINE.
           PERFORM P1000-INIT THRU P1000-EXIT.
           PERFORM P2000-PROCESS THRU P2000-EXIT UNTIL WS-EOF.
           PERFORM P8000-CONTROL THRU P8000-EXIT.
           PERFORM P9000-TERM THRU P9000-EXIT.
           STOP RUN.
      *****************************************************************
      * S100-INITIALISATION SECTION - PASS 1 THEN PASS 2 OPENS.    *
      *****************************************************************
       S100-INITIALISATION SECTION.
       P1000-INIT.
           PERFORM P1100-OPEN-AUDLOG-PASS1 THRU P1100-EXIT.
           PERFORM P1300-BUILD-ATTEMPT-TABLE THRU P1300-EXIT
               UNTIL WS-AL-EOF.
           PERFORM P1350-CLOSE-AUDLOG-PASS1 THRU P1350-EXIT.
           PERFORM P1400-OPEN-FILES-PASS2 THRU P1400-EXIT.
           PERFORM P1500-READ-PARM THRU P1500-EXIT.
           PERFORM P1600-INIT-COUNTERS THRU P1600-EXIT.
           PERFORM P2100-READ-RATIN THRU P2100-EXIT.
       P1000-EXIT.
           EXIT.
       P1100-OPEN-AUDLOG-PASS1.
           OPEN INPUT AUDLOG.
           IF WS-FS-AUDLOG NOT = '00'
               MOVE 'P1100-OPEN-AUDLOG-PASS1' TO WS-AB-PARA
               MOVE 'AUDLOG OPEN INPUT FAILED - PASS 1' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           PERFORM P1110-READ-AUDLOG THRU P1110-EXIT.
       P1100-EXIT.
           EXIT.
       P1110-READ-AUDLOG.
           READ AUDLOG
               AT END MOVE 'Y' TO WS-AL-EOF-SW.
       P1110-EXIT.
           EXIT.
       P1300-BUILD-ATTEMPT-TABLE.
           PERFORM P1310-TALLY-ONE-RECORD THRU P1310-EXIT.
           PERFORM P1110-READ-AUDLOG THRU P1110-EXIT.
       P1300-EXIT.
           EXIT.
      *****************************************************************
      * P1310-TALLY-ONE-RECORD - COUNT = PHYSICAL RECORDS SEEN.    *
      *****************************************************************
       P1310-TALLY-ONE-RECORD.
           MOVE 'N' TO WS-AS-FOUND-SW.
           IF WS-AT-CNT > 0
               PERFORM P1320-SEARCH-ONE-SLOT THRU P1320-EXIT
                   VARYING WS-AT-X FROM 1 BY 1
                   UNTIL WS-AT-X > WS-AT-CNT OR WS-AS-FOUND.
           IF WS-AS-FOUND
               ADD 1 TO WS-AT-COUNT (WS-AT-X)
           ELSE
               IF WS-AT-CNT < 2000
                   ADD 1 TO WS-AT-CNT
                   SET WS-AT-X TO WS-AT-CNT
                   MOVE AL-OCN TO WS-AT-OCN (WS-AT-X)
                   MOVE AL-BAN TO WS-AT-BAN (WS-AT-X)
                   MOVE AL-SEQ-NBR TO WS-AT-SEQ-NBR (WS-AT-X)
                   MOVE AL-RATE-ELEM TO WS-AT-RATE-ELEM (WS-AT-X)
                   MOVE 1 TO WS-AT-COUNT (WS-AT-X)
               ELSE
                   ADD 1 TO WS-MC-TABLE-FULL-CNT.
       P1310-EXIT.
           EXIT.
       P1320-SEARCH-ONE-SLOT.
           IF WS-AT-OCN (WS-AT-X) = AL-OCN AND
                   WS-AT-BAN (WS-AT-X) = AL-BAN AND
                   WS-AT-SEQ-NBR (WS-AT-X) = AL-SEQ-NBR AND
                   WS-AT-RATE-ELEM (WS-AT-X) = AL-RATE-ELEM
               MOVE 'Y' TO WS-AS-FOUND-SW.
       P1320-EXIT.
           EXIT.
       P1350-CLOSE-AUDLOG-PASS1.
           CLOSE AUDLOG.
       P1350-EXIT.
           EXIT.
       P1400-OPEN-FILES-PASS2.
           MOVE 'P1400-OPEN-FILES-PASS2' TO WS-AB-PARA.
           OPEN INPUT RATIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'RATIN OPEN FAILED' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN INPUT RATEMST.
           IF WS-FS-TABLE NOT = '00'
               MOVE 'RATEMST OPEN FAILED' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RTRYOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'RTRYOUT OPEN FAILED' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SUSOUT.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 'SUSOUT OPEN FAILED' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'CTLOUT OPEN FAILED' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN EXTEND AUDLOG.
           IF WS-FS-AUDLOG NOT = '00'
               MOVE 'AUDLOG OPEN EXTEND FAILED' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
       P1400-EXIT.
           EXIT.
       P1500-READ-PARM.
           MOVE SPACES TO WS-PARM-CARD.
           ACCEPT WS-PARM-CARD FROM SYSIN.
           CALL 'CABPARMR' USING WS-PARM-CARD WS-RC-PARMR.
           MOVE PC1-CYCLE-YYDDD TO R1-CYCLE-YYDDD.
           MOVE PC1-BILL-PERIOD TO R1-BILL-PERIOD.
           MOVE PC1-RUN-ID TO R1-RUN-ID.
       P1500-EXIT.
           EXIT.
       P1600-INIT-COUNTERS.
           MOVE 0 TO WS-READ-CNT.
           MOVE 0 TO WS-WRITE-CNT.
           MOVE 0 TO WS-REJECT-CNT.
           MOVE 0 TO WS-SUMM-CNT.
           MOVE 0 TO WS-CFWD-CNT.
           MOVE 0 TO WS-ACC-MINUTES.
           MOVE 0 TO WS-ACC-AMOUNT.
           MOVE 0 TO WS-ACC-SEQ-HASH.
           MOVE 0 TO WS-ACC-OCN-HASH.
           MOVE 0 TO WS-MC-CANDIDATES-CNT.
           MOVE 0 TO WS-MC-SUCCESS-CNT.
           MOVE 0 TO WS-MC-FAILED-CNT.
           MOVE 0 TO WS-MC-ABANDONED-CNT.
           MOVE 0 TO WS-MC-PASSTHRU-CNT.
       P1600-EXIT.
           EXIT.
       P9900-FATAL-OPEN.
           MOVE 'B037' TO CT-ABEND-CD.
           CALL 'CABABEND' USING WS-AB-PARA WS-AB-REASON
               CT-ABEND-CD.
       P9900-EXIT.
           EXIT.
      *****************************************************************
      * S200-MAIN-PROCESS SECTION                                     *
      *****************************************************************
       S200-MAIN-PROCESS SECTION.
       P2000-PROCESS.
           PERFORM P2110-CHECK-CANDIDATE THRU P2110-EXIT.
           IF WS-RW-CANDIDATE
               ADD 1 TO WS-MC-CANDIDATES-CNT
               PERFORM P2200-LOOKUP-ATTEMPT-COUNT THRU P2200-EXIT
               IF WS-AS-CURRENT-COUNT NOT < WS-MAX-ATTEMPTS
                   PERFORM P2900-ALREADY-ABANDONED THRU P2900-EXIT
               ELSE
                   PERFORM P2400-RETRY-RATE THRU P2400-EXIT
                   IF WS-RW-FOUND-RATE
                       PERFORM P2500-WRITE-RETRY-SUCCESS THRU
                           P2500-EXIT
                   ELSE
                       IF WS-AS-CURRENT-COUNT + 1 NOT <
                               WS-MAX-ATTEMPTS
                           PERFORM P2700-ABANDON-RECORD THRU
                               P2700-EXIT
                       ELSE
                           PERFORM P2600-LOG-FAILED-ATTEMPT THRU
                               P2600-EXIT
           ELSE
               ADD 1 TO WS-MC-PASSTHRU-CNT.
           PERFORM P2100-READ-RATIN THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-RATIN.
           READ RATIN
               AT END MOVE 'Y' TO WS-EOF-SW.
           IF NOT WS-EOF
               ADD 1 TO WS-READ-CNT
               PERFORM P2105-CHECKPOINT-DISPLAY THRU P2105-EXIT.
       P2100-EXIT.
           EXIT.
       P2105-CHECKPOINT-DISPLAY.
           DIVIDE WS-READ-CNT BY 25000 GIVING WS-MC-CHECKPOINT-QUOT
               REMAINDER WS-MC-CHECKPOINT-REM.
           IF WS-MC-CHECKPOINT-REM = 0
               DISPLAY 'CABRAT12 - ' WS-READ-CNT ' RECORDS READ'.
       P2105-EXIT.
           EXIT.
      *****************************************************************
      * P2110-CHECK-CANDIDATE - SUSPENDED, OR ZERO AMT + QTY > 0.  *
      *****************************************************************
       P2110-CHECK-CANDIDATE.
           MOVE 'N' TO WS-RW-CANDIDATE-SW.
           IF RC-SUSPENDED
               MOVE 'Y' TO WS-RW-CANDIDATE-SW.
           IF RC-RATED AND RC-AMOUNT = 0 AND RC-QTY > 0
               MOVE 'Y' TO WS-RW-CANDIDATE-SW.
       P2110-EXIT.
           EXIT.
       P2200-LOOKUP-ATTEMPT-COUNT.
           MOVE 'N' TO WS-AS-FOUND-SW.
           MOVE 0 TO WS-AS-CURRENT-COUNT.
           IF WS-AT-CNT > 0
               PERFORM P2210-SEARCH-ONE-SLOT THRU P2210-EXIT
                   VARYING WS-AT-X FROM 1 BY 1
                   UNTIL WS-AT-X > WS-AT-CNT OR WS-AS-FOUND.
       P2200-EXIT.
           EXIT.
       P2210-SEARCH-ONE-SLOT.
           IF WS-AT-OCN (WS-AT-X) = RC-OCN AND
                   WS-AT-BAN (WS-AT-X) = RC-BAN AND
                   WS-AT-SEQ-NBR (WS-AT-X) = RC-SEQ-NBR AND
                   WS-AT-RATE-ELEM (WS-AT-X) = RC-RATE-ELEM
               MOVE 'Y' TO WS-AS-FOUND-SW
               MOVE WS-AT-COUNT (WS-AT-X) TO WS-AS-CURRENT-COUNT.
       P2210-EXIT.
           EXIT.
      *****************************************************************
      * P2400-RETRY-RATE - LOOKS UP THE REFRESHED RATEMST.         *
      *****************************************************************
       P2400-RETRY-RATE.
           MOVE 'N' TO WS-RW-FOUND-RATE-SW.
           MOVE LOW-VALUES TO RT-KEY.
           MOVE 'FCC1' TO RT-TARIFF-CD.
           MOVE RC-RATE-ELEM TO RT-RATE-ELEM.
           MOVE RC-JURIS-CD TO RT-JURIS-CD.
           MOVE RC-STATE-CD TO RT-STATE-CD.
           START RATEMST KEY NOT LESS THAN RT-KEY
               INVALID KEY MOVE 'N' TO WS-RW-FOUND-RATE-SW
               NOT INVALID KEY PERFORM P2410-READ-MATCHING-RATE
                   THRU P2410-EXIT.
           IF WS-RW-FOUND-RATE
               PERFORM P2420-COMPUTE-RETRY-CHARGE THRU P2420-EXIT.
       P2400-EXIT.
           EXIT.
       P2410-READ-MATCHING-RATE.
           READ RATEMST NEXT RECORD
               AT END MOVE 'N' TO WS-RW-FOUND-RATE-SW.
           IF WS-FS-TABLE = '00'
               IF RT-TARIFF-CD = 'FCC1' AND
                       RT-RATE-ELEM = RC-RATE-ELEM AND
                       RT-JURIS-CD = RC-JURIS-CD AND
                       RT-STATE-CD = RC-STATE-CD
                   MOVE 'Y' TO WS-RW-FOUND-RATE-SW
               ELSE
                   MOVE 'N' TO WS-RW-FOUND-RATE-SW.
       P2410-EXIT.
           EXIT.
       P2420-COMPUTE-RETRY-CHARGE.
           MOVE RT-INITIAL-RATE TO WS-RW-RATE.
           COMPUTE WS-RTFMT-RATE-IN = RC-QTY * WS-RW-RATE.
           MOVE RT-ROUND-RULE TO WS-RTFMT-RULE-IN.
           CALL 'CABRTFMT' USING WS-RTFMT-RATE-IN WS-RTFMT-RULE-IN
               WS-RTFMT-OUT WS-RC-RTFMT.
           MOVE WS-RTFMT-OUT TO WS-RW-AMOUNT.
       P2420-EXIT.
           EXIT.
       P2500-WRITE-RETRY-SUCCESS.
           MOVE RC-OCN TO RY-OCN.
           MOVE RC-BAN TO RY-BAN.
           MOVE RC-SEQ-NBR TO RY-SEQ-NBR.
           MOVE RC-RATE-ELEM TO RY-RATE-ELEM.
           MOVE RC-QTY TO RY-QTY.
           MOVE WS-RW-RATE TO RY-RATE.
           MOVE WS-RW-AMOUNT TO RY-AMOUNT.
           COMPUTE RY-ATTEMPT-NBR = WS-AS-CURRENT-COUNT + 1.
           WRITE CABS-RTRYOUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
           ADD 1 TO WS-MC-SUCCESS-CNT.
           MOVE 'S' TO AL-RESULT-CD.
           MOVE RY-ATTEMPT-NBR TO AL-ATTEMPT-NBR.
           MOVE WS-RW-AMOUNT TO AL-AMOUNT.
           PERFORM P2510-WRITE-AUDIT-ENTRY THRU P2510-EXIT.
           MOVE RC-QTY TO WS-HC-MINUTES-IN.
           MOVE WS-RW-AMOUNT TO WS-HC-AMOUNT-IN.
           MOVE RC-SEQ-NBR TO WS-HC-SEQ-IN.
           CALL 'CABHASH' USING WS-HC-MINUTES-IN WS-HC-AMOUNT-IN
               WS-HC-SEQ-IN WS-RC-HASH.
           ADD WS-HC-MINUTES-IN TO WS-ACC-MINUTES.
           ADD WS-HC-AMOUNT-IN TO WS-ACC-AMOUNT.
           ADD WS-HC-SEQ-IN TO WS-ACC-SEQ-HASH.
       P2500-EXIT.
           EXIT.
      *****************************************************************
      * P2510-WRITE-AUDIT-ENTRY - APPENDS TO AUDLOG (CALLER SETS   *
      * AL-RESULT-CD / AL-ATTEMPT-NBR / AL-AMOUNT FIRST).          *
      *****************************************************************
       P2510-WRITE-AUDIT-ENTRY.
           MOVE RC-OCN TO AL-OCN.
           MOVE RC-BAN TO AL-BAN.
           MOVE RC-SEQ-NBR TO AL-SEQ-NBR.
           MOVE RC-RATE-ELEM TO AL-RATE-ELEM.
           MOVE R1-CYCLE-YYDDD TO AL-ATTEMPT-YYDDD.
           MOVE R1-RUN-ID TO AL-RUN-ID.
           MOVE SPACES TO AL-FILLER.
           WRITE CABS-AUDIT-LOG-RECORD.
       P2510-EXIT.
           EXIT.
       P2600-LOG-FAILED-ATTEMPT.
           MOVE 'F' TO AL-RESULT-CD.
           COMPUTE AL-ATTEMPT-NBR = WS-AS-CURRENT-COUNT + 1.
           MOVE 0 TO AL-AMOUNT.
           PERFORM P2510-WRITE-AUDIT-ENTRY THRU P2510-EXIT.
           ADD 1 TO WS-MC-FAILED-CNT.
       P2600-EXIT.
           EXIT.
       P2700-ABANDON-RECORD.
           MOVE 'A' TO AL-RESULT-CD.
           COMPUTE AL-ATTEMPT-NBR = WS-AS-CURRENT-COUNT + 1.
           MOVE 0 TO AL-AMOUNT.
           PERFORM P2510-WRITE-AUDIT-ENTRY THRU P2510-EXIT.
           ADD 1 TO WS-MC-ABANDONED-CNT.
           PERFORM P2800-WRITE-SUSPENSE THRU P2800-EXIT.
       P2700-EXIT.
           EXIT.
       P2800-WRITE-SUSPENSE.
           MOVE SPACES TO CABS-SUSPENSE-RECORD-FD.
           MOVE EC-RATE-NOT-FOUND TO FD-SU-ERR-CODE.
           MOVE 'E' TO FD-SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO FD-SU-DETECT-PGM.
           MOVE 'P2000-PROCESS' TO FD-SU-DETECT-PARA.
           MOVE R1-RUN-ID TO FD-SU-RUN-ID.
           MOVE CABS-RETRY-CANDIDATE-RECORD TO FD-SU-ORIG-RECORD.
           WRITE CABS-SUSPENSE-RECORD-FD.
           ADD 1 TO WS-REJECT-CNT.
           CALL 'CABERRWR' USING FD-SU-ERR-CODE FD-SU-DETECT-PGM
               FD-SU-DETECT-PARA FD-SU-RUN-ID
               ON EXCEPTION
                   MOVE 9999 TO WS-RC-ERRWR
               NOT ON EXCEPTION
                   MOVE 0 TO WS-RC-ERRWR.
       P2800-EXIT.
           EXIT.
       P2900-ALREADY-ABANDONED.
           PERFORM P2800-WRITE-SUSPENSE THRU P2800-EXIT.
       P2900-EXIT.
           EXIT.
      *****************************************************************
      * S800-CONTROL-BALANCE SECTION - CT-CARRIED-FWD = PASSTHRU + *
      * FAILED-NOT-YET-ABANDONED (BOTH DEFERRED TO A LATER RUN).   *
      *****************************************************************
       S800-CONTROL-BALANCE SECTION.
       P8000-CONTROL.
           PERFORM P8100-BUILD-CONTROL-REC THRU P8100-EXIT.
           PERFORM P8200-CHECK-BALANCE THRU P8200-EXIT.
           PERFORM P8300-WRITE-CONTROL-REC THRU P8300-EXIT.
       P8000-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
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
           MOVE 0 TO CT-SUMMARISED.
           COMPUTE CT-CARRIED-FWD =
               WS-MC-PASSTHRU-CNT + WS-MC-FAILED-CNT.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE SPACES TO CT-FILLER.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-ABEND-CD.
       P8100-EXIT.
           EXIT.
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
      *****************************************************************
      * S900-TERMINATION SECTION.                                     *
      *****************************************************************
       S900-TERMINATION SECTION.
       P9000-TERM.
           CLOSE RATIN.
           CLOSE RATEMST.
           CLOSE RTRYOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           CLOSE AUDLOG.
           DISPLAY 'CABRAT12 - RUN COMPLETE'.
           DISPLAY '  READ        = ' WS-READ-CNT.
           DISPLAY '  CANDIDATES  = ' WS-MC-CANDIDATES-CNT.
           DISPLAY '  SUCCESS     = ' WS-MC-SUCCESS-CNT.
           DISPLAY '  FAILED      = ' WS-MC-FAILED-CNT.
           DISPLAY '  ABANDONED   = ' WS-MC-ABANDONED-CNT.
       P9000-EXIT.
           EXIT.
