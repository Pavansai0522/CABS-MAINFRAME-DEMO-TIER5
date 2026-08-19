      *****************************************************************
      * CABCTC02 - CARRIER RATE RECONCILIATION AND VARIANCE EXTRACT   *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               SUMIN   TELCABS.CABS.CTCSUMM(0)      (LOCAL)    *
      *               RATIN   TELCABS.CABS.RATSUMM(0)      (LOCAL)    *
      *               RTBLIN  TELCABS.CABS.RATETBL(0)      (LOCAL)    *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               RECOUT  TELCABS.CABS.CTCRECN(+1)     (LOCAL)    *
      *               SUSOUT  TELCABS.CABS.CTC.SUSP(+1)    CABSERR    *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - BOTH INPUT SIDES ARE READ FROM     *
      *               THE TOP AND NOTHING IS UPDATED IN PLACE         *
      * REVISION HISTORY                                              *
      *   V2.00  1990-04-17  R.T.WHEELER  INITIAL RELEASE - REPLACED  *
      *                      THE MANUAL RECONCILIATION THE REGIONAL   *
      *                      CENTRES WERE DOING ON PAPER              *
      *   V2.03  1993-11-29  D.OKONKWO    RATE TABLE READ FROM THE    *
      *                      FLATTENED EXTRACT INSTEAD OF THE KSDS    *
      *   V2.05  1997-05-13  J.M.CASTILLO BAND RATES BROUGHT INTO THE *
      *                      EXPECTED CHARGE - MILEAGE BANDED         *
      *                      ELEMENTS WERE RECONCILING AS VARIANCES   *
      *   V2.08  2002-09-04  P.NAIR       ONE-SIDED TOLERANCE ADDED SO
      *                      THE PENNY DRIFT STOPPED FILLING THE      *
      *                      EXCEPTION REPORT                         *
      *   V2.11  2008-01-22  A.BUKOWSKI   MATCH-MERGE REWRITTEN - THE *
      *                      OLD READ-AHEAD DROPPED THE LAST GROUP ON *
      *                      AN UNMATCHED RATED SIDE                  *
      *   V2.14  2018-06-25  L.FERREIRA   TARIFF MINIMUM AND          *
      *                      MAXIMUM COLLARED ONTO THE EXPECTED       *
      *                      AMOUNT BEFORE THE COMPARISON             *
      *   V2.15  2019-10-02  M.DELACROIX  CROSS CENTRE KEY CHECK      *
      *                      ADDED AT THIS CENTRE ONLY AFTER THE      *
      *                      SEPTEMBER DUPLICATE INVOICE INCIDENT     *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABCTC02.
       AUTHOR. TELCABS APPLICATIONS - CARRIER TRAFFIC.
      *****************************************************************
      * RATE RECONCILIATION.  A TWO-SIDED MATCH-MERGE OF THE CARRIER  *
      * TRAFFIC SUMMARY PRODUCED BY CABCTC01 AGAINST THE RATED        *
      * SUMMARY PRODUCED BY THE RATING SUITE, BOTH IN OCN / BAN /     *
      * RATE ELEMENT SEQUENCE.  FOR EVERY MATCHED PAIR THE EXPECTED   *
      * CHARGE IS REBUILT FROM THE FLATTENED RATE TABLE AND SET       *
      * AGAINST WHAT WAS ACTUALLY RATED.  THE DIFFERENCE IS           *
      * CLASSIFIED AND WRITTEN TO THE RECONCILIATION EXTRACT.         *
      *                                                               *
      * THE TOLERANCE IS ONE SIDED.  A RATED AMOUNT BELOW THE         *
      * EXPECTED AMOUNT INSIDE THE TOLERANCE IS PASSED AS AGREED.     *
      * THAT IS WHAT WAS ASKED FOR IN 2002 AND IT HAS NOT BEEN        *
      * REVISITED SINCE.                                              *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           C01 IS TO-NEW-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SUMIN ASSIGN TO UT-S-SUMIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT RATIN ASSIGN TO UT-S-RATIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT RTBLIN ASSIGN TO UT-S-RTBLIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-TABLE.
           SELECT RECOUT ASSIGN TO UT-S-RECOUT
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
      * SUMIN - CARRIER TRAFFIC SUMMARY FROM CABCTC01.
       FD  SUMIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       01  CABS-CTCS-RECORD.
           05  CS-KEY.
               10  CS-OCN                  PIC X(04).
               10  CS-BAN                  PIC X(13).
               10  CS-RATE-ELEM            PIC X(06).
           05  CS-CONTEXT.
               10  CS-CYCLE-YYDDD          PIC 9(05).
               10  CS-SITE-CD              PIC X(04).
               10  CS-CARRIER-TYPE         PIC X(01).
               10  CS-STATE-CD             PIC X(02).
           05  CS-COUNTS.
               10  CS-DETAIL-CNT           PIC S9(09) COMP-3.
               10  CS-SUSPECT-CNT          PIC S9(05) COMP-3.
           05  CS-MINUTES.
               10  CS-MOU-INTERSTATE       PIC S9(13)V9(02) COMP-3.
               10  CS-MOU-INTRASTATE       PIC S9(13)V9(02) COMP-3.
               10  CS-MOU-LOCAL            PIC S9(13)V9(02) COMP-3.
               10  CS-MOU-TOTAL            PIC S9(13)V9(02) COMP-3.
           05  CS-HASH.
               10  CS-HASH-SEQ             PIC S9(17) COMP-3.
               10  CS-HASH-OCN             PIC S9(15) COMP-3.
           05  CS-FILLER                   PIC X(108).
      * RATIN - RATED SUMMARY FROM THE RATING SUITE.  SAME KEY.
       FD  RATIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       01  CABS-RATED-RECORD.
           05  RS-KEY.
               10  RS-OCN                  PIC X(04).
               10  RS-BAN                  PIC X(13).
               10  RS-RATE-ELEM            PIC X(06).
           05  RS-CONTEXT.
               10  RS-CYCLE-YYDDD          PIC 9(05).
               10  RS-TARIFF-CD            PIC X(04).
               10  RS-JURIS-CD             PIC X(01).
               10  RS-STATE-CD             PIC X(02).
               10  RS-EFF-YYDDD            PIC 9(05).
           05  RS-QUANTITY.
               10  RS-RATED-MOU            PIC S9(13)V9(02) COMP-3.
               10  RS-RATED-CNT            PIC S9(09) COMP-3.
           05  RS-AMOUNTS.
               10  RS-RATED-AMOUNT         PIC S9(13)V9(05) COMP-3.
               10  RS-SETUP-AMOUNT         PIC S9(09)V9(05) COMP-3.
               10  RS-ROUND-RULE           PIC X(01).
           05  RS-FILLER                   PIC X(122).
      * RTBLIN - FLATTENED RATE TABLE EXTRACT FROM CABRAT01.
       FD  RTBLIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       01  CABS-RTBL-RECORD.
           05  RB-REC-TYPE                 PIC X(01).
           05  RB-DETAIL                   PIC X(199).
       01  CABS-RTBL-ENTRY-REC REDEFINES CABS-RTBL-RECORD.
           05  RB-EN-REC-TYPE              PIC X(01).
           05  RB-EN-TARIFF                PIC X(04).
           05  RB-EN-ELEM                  PIC X(06).
           05  RB-EN-JURIS                 PIC X(01).
           05  RB-EN-STATE                 PIC X(02).
           05  RB-EN-EFF-YYDDD             PIC 9(05).
           05  RB-EN-EXP-YYDDD             PIC 9(05).
           05  RB-EN-INITIAL               PIC S9(05)V9(05).
           05  RB-EN-ADDL                  PIC S9(05)V9(05).
           05  RB-EN-SETUP                 PIC S9(07)V9(05).
           05  RB-EN-MIN-CHG               PIC S9(07)V9(02).
           05  RB-EN-MAX-CHG               PIC S9(11)V9(02).
           05  RB-EN-ROUND-RULE            PIC X(01).
           05  RB-EN-ROUND-POS             PIC 9(01).
           05  RB-EN-INIT-PERIOD           PIC 9(04).
           05  RB-EN-ADDL-PERIOD           PIC 9(04).
           05  RB-EN-BAND-CNT              PIC 9(02).
           05  RB-EN-BAND-OFFSET           PIC 9(04).
           05  RB-EN-MODULE-SFX            PIC X(02).
           05  RB-EN-FILLER                PIC X(104).
       01  CABS-RTBL-BAND-REC REDEFINES CABS-RTBL-RECORD.
           05  RB-BD-REC-TYPE              PIC X(01).
           05  RB-BD-TARIFF                PIC X(04).
           05  RB-BD-ELEM                  PIC X(06).
           05  RB-BD-JURIS                 PIC X(01).
           05  RB-BD-STATE                 PIC X(02).
           05  RB-BD-EFF-YYDDD             PIC 9(05).
           05  RB-BD-BAND-SEQ              PIC 9(02).
           05  RB-BD-BAND-FROM             PIC S9(11).
           05  RB-BD-BAND-THRU             PIC S9(11).
           05  RB-BD-BAND-RATE             PIC S9(05)V9(05).
           05  RB-BD-BAND-PCT              PIC S9(03)V9(05).
           05  RB-BD-FILLER                PIC X(139).
      * RECOUT - ONE RECONCILIATION RECORD PER MATCHED OR UNMATCHED
      * KEY.  READ BY THE REGIONAL VARIANCE REPORT.
       FD  RECOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 250 CHARACTERS.
       01  CABS-RECON-RECORD.
           05  RC-KEY.
               10  RC-OCN                  PIC X(04).
               10  RC-BAN                  PIC X(13).
               10  RC-RATE-ELEM            PIC X(06).
           05  RC-CONTEXT.
               10  RC-CYCLE-YYDDD          PIC 9(05).
               10  RC-SITE-CD              PIC X(04).
               10  RC-TARIFF-CD            PIC X(04).
               10  RC-JURIS-CD             PIC X(01).
               10  RC-STATE-CD             PIC X(02).
           05  RC-SIDES.
               10  RC-SIDE-IND             PIC X(01).
                   88  RC-BOTH-SIDES       VALUE 'B'.
                   88  RC-SUMMARY-ONLY     VALUE 'S'.
                   88  RC-RATED-ONLY       VALUE 'R'.
               10  RC-SUM-MOU              PIC S9(13)V9(02) COMP-3.
               10  RC-RAT-MOU              PIC S9(13)V9(02) COMP-3.
               10  RC-MOU-DIFF             PIC S9(13)V9(02) COMP-3.
           05  RC-MONEY.
               10  RC-EXPECTED-AMT         PIC S9(13)V9(05) COMP-3.
               10  RC-RATED-AMT            PIC S9(13)V9(05) COMP-3.
               10  RC-VARIANCE-AMT         PIC S9(13)V9(05) COMP-3.
               10  RC-TOLERANCE-USED       PIC S9(05)V9(02) COMP-3.
           05  RC-CLASS.
               10  RC-CLASS-CD             PIC X(02).
                   88  RC-AGREED           VALUE 'AG'.
                   88  RC-UNDER-RATED      VALUE 'UR'.
                   88  RC-OVER-RATED       VALUE 'OR'.
                   88  RC-NO-RATE-ROW      VALUE 'NR'.
                   88  RC-ONE-SIDED        VALUE 'OS'.
               10  RC-CLASS-TEXT           PIC X(30).
           05  RC-FILLER                   PIC X(131).
      * SUSOUT - SUSPENSE.
       FD  SUSOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 300 CHARACTERS.
       01  CABS-SUSOUT-RECORD              PIC X(300).
      * CTLOUT - RUN CONTROL / BALANCING RECORD.
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD              PIC X(180).
      * RPTOUT - RECONCILIATION EXCEPTION REPORT.
       FD  RPTOUT
           RECORDING MODE IS FA
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       COPY CABSPRNT.
       WORKING-STORAGE SECTION.
      * STANDARD SHARED WORKING STORAGE.  SEE CABSWRK.
       COPY CABSWRK.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABCTC02'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V2.15'.
           05  WS-MAX-RATE-ENTRIES         PIC S9(04) COMP-3
                                                        VALUE 600.
           05  WS-MAX-RATE-BANDS           PIC S9(04) COMP-3
                                                        VALUE 2400.
      * SITE INSTALLATION CONSTANTS.  SET AT INSTALL TIME FROM THE
      * REGIONAL BILLING CENTRE STANDARDS SHEET.
       01  WS-SITE-CONSTANTS.
           05  WS-VARIANCE-TOLERANCE       PIC S9(05)V9(02) COMP-3
                                                  VALUE 0.05.
           05  WS-RETRO-CUTOFF-YYDDD       PIC 9(05) VALUE 09001.
      * SYSIN PARM CARD - POSITIONAL LAYOUT ONLY.
       01  WS-PARM-CARD                    PIC X(80).
       01  WS-PARM-CARD-R1 REDEFINES WS-PARM-CARD.
           05  PC1-REC-ID                   PIC X(02).
           05  PC1-RUN-ID                   PIC X(12).
           05  PC1-CYCLE-YYDDD              PIC 9(05).
           05  PC1-BILL-PERIOD              PIC 9(06).
           05  PC1-SITE-CD                  PIC X(04).
           05  PC1-JOBNAME                  PIC X(08).
           05  PC1-STEPNAME                 PIC X(08).
           05  PC1-OPT-REPORT               PIC X(01).
           05  PC1-OPT-RETRO                PIC X(01).
           05  PC1-FILLER                   PIC X(33).
       01  WS-PARM-CARD-R2 REDEFINES WS-PARM-CARD.
           05  PC2-LEAD                     PIC X(14).
           05  PC2-CYCLE-VIEW.
               10  PC2-CV-YY                PIC 9(02).
               10  PC2-CV-DDD               PIC 9(03).
           05  PC2-REST                     PIC X(61).
      * IN-CORE RATE TABLE REBUILT FROM THE FLATTENED EXTRACT.
       01  WS-RATE-TABLE.
           05  WS-RT-ENTRY-CNT             PIC S9(04) COMP-3 VALUE 0.
           05  WS-RT-FULL-SW               PIC X(01) VALUE 'N'.
               88  WS-RT-TABLE-FULL        VALUE 'Y'.
           05  WS-RT-ENTRY OCCURS 600 TIMES
                                           INDEXED BY WS-RX.
               10  WS-RT-TARIFF            PIC X(04).
               10  WS-RT-ELEM              PIC X(06).
               10  WS-RT-JURIS             PIC X(01).
               10  WS-RT-STATE             PIC X(02).
               10  WS-RT-EFF-YYDDD         PIC 9(05).
               10  WS-RT-EXP-YYDDD         PIC 9(05).
               10  WS-RT-INITIAL           PIC S9(05)V9(05) COMP-3.
               10  WS-RT-ADDL              PIC S9(05)V9(05) COMP-3.
               10  WS-RT-SETUP             PIC S9(07)V9(05) COMP-3.
               10  WS-RT-MIN-CHG           PIC S9(07)V9(02) COMP-3.
               10  WS-RT-MAX-CHG           PIC S9(11)V9(02) COMP-3.
               10  WS-RT-ROUND-RULE        PIC X(01).
               10  WS-RT-BAND-CNT          PIC 9(02).
               10  WS-RT-BAND-OFFSET       PIC 9(04).
       01  WS-BAND-POOL.
           05  WS-BP-POOL-CNT              PIC S9(04) COMP-3 VALUE 0.
           05  WS-BP-ENTRY OCCURS 2400 TIMES
                                           INDEXED BY WS-BX.
               10  WS-BP-FROM              PIC S9(11) COMP-3.
               10  WS-BP-THRU              PIC S9(11) COMP-3.
               10  WS-BP-RATE              PIC S9(05)V9(05) COMP-3.
               10  WS-BP-PCT               PIC S9(03)V9(05) COMP-3.
      * MATCH-MERGE CONTROL.  BOTH SIDES CARRY THEIR OWN END OF
      * FILE SWITCH - THE SHARED WS-EOF IS ONLY SET WHEN BOTH ARE
      * EXHAUSTED.
       01  WS-MERGE-CONTROL.
           05  WS-SUM-EOF-SW               PIC X(01) VALUE 'N'.
               88  WS-SUM-EOF              VALUE 'Y'.
               88  WS-SUM-NOT-EOF          VALUE 'N'.
           05  WS-RAT-EOF-SW               PIC X(01) VALUE 'N'.
               88  WS-RAT-EOF              VALUE 'Y'.
               88  WS-RAT-NOT-EOF          VALUE 'N'.
           05  WS-MERGE-ACTION             PIC X(01) VALUE SPACES.
               88  WS-TAKE-BOTH            VALUE 'B'.
               88  WS-TAKE-SUMMARY         VALUE 'S'.
               88  WS-TAKE-RATED           VALUE 'R'.
           05  WS-SUM-KEY-HOLD             PIC X(23) VALUE HIGH-VALUES.
           05  WS-RAT-KEY-HOLD             PIC X(23) VALUE HIGH-VALUES.
      * RATE LOOKUP WORK.
       01  WS-RATE-LOOKUP-WORK.
           05  WS-RL-FOUND-SW              PIC X(01) VALUE 'N'.
               88  WS-RL-FOUND             VALUE 'Y'.
               88  WS-RL-NOT-FOUND         VALUE 'N'.
           05  WS-RL-SUB                   PIC S9(04) COMP-3 VALUE 0.
           05  WS-RL-HIT-SUB               PIC S9(04) COMP-3 VALUE 0.
           05  WS-RL-INITIAL               PIC S9(05)V9(05) COMP-3
                                                            VALUE 0.
           05  WS-RL-ADDL                  PIC S9(05)V9(05) COMP-3
                                                            VALUE 0.
           05  WS-RL-SETUP                 PIC S9(07)V9(05) COMP-3
                                                            VALUE 0.
           05  WS-RL-ROUND-RULE            PIC X(01) VALUE SPACES.
           05  WS-RL-BAND-CNT              PIC 9(02) VALUE 0.
           05  WS-RL-BAND-OFFSET           PIC 9(04) VALUE 0.
      * EXPECTED CHARGE WORK.
       01  WS-EXPECTED-WORK.
           05  WS-EW-BILLABLE-MOU          PIC S9(13)V9(02) COMP-3
                                                            VALUE 0.
           05  WS-EW-INITIAL-AMT           PIC S9(13)V9(05) COMP-3
                                                            VALUE 0.
           05  WS-EW-ADDL-AMT              PIC S9(13)V9(05) COMP-3
                                                            VALUE 0.
           05  WS-EW-SETUP-AMT             PIC S9(13)V9(05) COMP-3
                                                            VALUE 0.
           05  WS-EW-BAND-AMT              PIC S9(13)V9(05) COMP-3
                                                            VALUE 0.
           05  WS-EW-TOTAL-AMT             PIC S9(13)V9(05) COMP-3
                                                            VALUE 0.
           05  WS-EW-VARIANCE              PIC S9(13)V9(05) COMP-3
                                                            VALUE 0.
           05  WS-EW-ABS-VARIANCE          PIC S9(13)V9(05) COMP-3
                                                            VALUE 0.
       01  WS-BAND-WALK-WORK.
           05  WS-BW-SUB                   PIC S9(04) COMP-3 VALUE 0.
           05  WS-BW-POOL-SUB              PIC S9(04) COMP-3 VALUE 0.
           05  WS-BW-REMAINING             PIC S9(13)V9(02) COMP-3
                                                            VALUE 0.
           05  WS-BW-IN-BAND               PIC S9(13)V9(02) COMP-3
                                                            VALUE 0.
      * COUNTERS FOR THE EXCEPTION REPORT.
       01  WS-CLASS-COUNTERS.
           05  WS-CC-AGREED                PIC S9(07) COMP-3 VALUE 0.
           05  WS-CC-UNDER                 PIC S9(07) COMP-3 VALUE 0.
           05  WS-CC-OVER                  PIC S9(07) COMP-3 VALUE 0.
           05  WS-CC-NO-RATE               PIC S9(07) COMP-3 VALUE 0.
           05  WS-CC-SUM-ONLY              PIC S9(07) COMP-3 VALUE 0.
           05  WS-CC-RAT-ONLY              PIC S9(07) COMP-3 VALUE 0.
           05  WS-CC-RETRO-SKIP            PIC S9(07) COMP-3 VALUE 0.
       01  WS-VALUE-COUNTERS.
           05  WS-VC-EXPECTED-TOT          PIC S9(15)V9(05) COMP-3
                                                            VALUE 0.
           05  WS-VC-RATED-TOT             PIC S9(15)V9(05) COMP-3
                                                            VALUE 0.
           05  WS-VC-VARIANCE-TOT          PIC S9(15)V9(05) COMP-3
                                                            VALUE 0.
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3 VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3 VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3 VALUE 55.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
               'CABCTC02 - CARRIER RATE RECONCILIATION EXCEPTIONS'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
               'TELCABS WHOLESALE ACCESS BILLING - REGIONAL CENTRE'.
           05  WS-RPT-AMT-EDIT             PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-RPT-CNT-EDIT             PIC ZZZ,ZZZ,ZZ9.
       01  WS-CLASS-TEXT-TABLE.
           05  FILLER  PIC X(30) VALUE 'AGREED WITHIN TOLERANCE'.
           05  FILLER  PIC X(30) VALUE 'RATED BELOW EXPECTED'.
           05  FILLER  PIC X(30) VALUE 'RATED ABOVE EXPECTED'.
           05  FILLER  PIC X(30) VALUE 'NO EFFECTIVE RATE ROW'.
           05  FILLER  PIC X(30) VALUE 'ONE SIDED KEY'.
       01  WS-CLASS-TEXT-R REDEFINES WS-CLASS-TEXT-TABLE.
           05  WS-CT-TEXT OCCURS 5 TIMES   PIC X(30).
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9904.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-HASH                  PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-CV-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-CV-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
       01  WS-RTBL-LOAD-WORK.
           05  WS-RL-EOF-SW                PIC X(01) VALUE 'N'.
               88  WS-RTBL-EOF             VALUE 'Y'.
           05  WS-RL-ENTRY-ROWS            PIC S9(05) COMP-3 VALUE 0.
           05  WS-RL-BAND-ROWS             PIC S9(05) COMP-3 VALUE 0.
           05  WS-RL-DROPPED-ROWS          PIC S9(05) COMP-3 VALUE 0.
       PROCEDURE DIVISION.
      * P0000-MAINLINE - MANDATORY CABS BATCH SHAPE.  ONE PASS OF
      * P2000-PROCESS ADVANCES ONE OR BOTH SIDES OF THE MERGE.
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
           PERFORM P1300-LOAD-RATE-TABLE THRU P1300-EXIT.
           PERFORM P1400-PRIME-READS THRU P1400-EXIT.
       P1000-EXIT.
           EXIT.
       P1100-OPEN-FILES.
           OPEN INPUT SUMIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'SUMIN OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN INPUT RATIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RATIN OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN INPUT RTBLIN.
           IF WS-FS-TABLE NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RTBLIN OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RECOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RECOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SUSOUT.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'SUSOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CTLOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RPTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RPTOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
       P1100-EXIT.
           EXIT.
       P1200-READ-PARM.
           MOVE SPACES TO WS-PARM-CARD.
           ACCEPT WS-PARM-CARD FROM SYSIN.
           CALL 'CABPARMR' USING WS-PARM-CARD WS-RC-PARMR.
           MOVE PC1-CYCLE-YYDDD TO WS-CV-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE WS-CV-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE WS-CV-CYCLE-CCYYDDD =
                   20000000 + PC1-CYCLE-YYDDD.
           CALL 'CABDTCNV' USING WS-CV-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-CC-AGREED WS-CC-UNDER WS-CC-OVER.
           MOVE 0 TO WS-CC-NO-RATE WS-CC-SUM-ONLY WS-CC-RAT-ONLY.
           MOVE 0 TO WS-CC-RETRO-SKIP.
           MOVE 0 TO WS-VC-EXPECTED-TOT WS-VC-RATED-TOT.
           MOVE 0 TO WS-VC-VARIANCE-TOT.
       P1200-EXIT.
           EXIT.
      * P1300-LOAD-RATE-TABLE - REBUILDS THE ENTRY TABLE AND THE
      * BAND POOL FROM THE FLATTENED EXTRACT.  THE BAND OFFSET
      * WRITTEN BY CABRAT01 IS TAKEN AT FACE VALUE.
       P1300-LOAD-RATE-TABLE.
           PERFORM P1330-READ-NEXT-RTBL THRU P1330-EXIT.
           PERFORM P1310-STORE-RTBL-ROW THRU P1310-EXIT
               UNTIL WS-RTBL-EOF.
       P1300-EXIT.
           EXIT.
       P1310-STORE-RTBL-ROW.
           IF RB-REC-TYPE = 'E'
               PERFORM P1320-STORE-ENTRY-ROW THRU P1320-EXIT
           ELSE
               IF RB-REC-TYPE = 'B'
                   PERFORM P1325-STORE-BAND-ROW THRU P1325-EXIT
               ELSE
                   ADD 1 TO WS-RL-DROPPED-ROWS.
           PERFORM P1330-READ-NEXT-RTBL THRU P1330-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-STORE-ENTRY-ROW.
           IF WS-RT-ENTRY-CNT NOT < WS-MAX-RATE-ENTRIES
               MOVE 'Y' TO WS-RT-FULL-SW
               ADD 1 TO WS-RL-DROPPED-ROWS
           ELSE
               ADD 1 TO WS-RT-ENTRY-CNT
               SET WS-RX TO WS-RT-ENTRY-CNT
               MOVE RB-EN-TARIFF TO WS-RT-TARIFF (WS-RX)
               MOVE RB-EN-ELEM TO WS-RT-ELEM (WS-RX)
               MOVE RB-EN-JURIS TO WS-RT-JURIS (WS-RX)
               MOVE RB-EN-STATE TO WS-RT-STATE (WS-RX)
               MOVE RB-EN-EFF-YYDDD TO WS-RT-EFF-YYDDD (WS-RX)
               MOVE RB-EN-EXP-YYDDD TO WS-RT-EXP-YYDDD (WS-RX)
               MOVE RB-EN-INITIAL TO WS-RT-INITIAL (WS-RX)
               MOVE RB-EN-ADDL TO WS-RT-ADDL (WS-RX)
               MOVE RB-EN-SETUP TO WS-RT-SETUP (WS-RX)
               MOVE RB-EN-MIN-CHG TO WS-RT-MIN-CHG (WS-RX)
               MOVE RB-EN-MAX-CHG TO WS-RT-MAX-CHG (WS-RX)
               MOVE RB-EN-ROUND-RULE TO WS-RT-ROUND-RULE (WS-RX)
               MOVE RB-EN-BAND-CNT TO WS-RT-BAND-CNT (WS-RX)
               MOVE RB-EN-BAND-OFFSET TO WS-RT-BAND-OFFSET (WS-RX)
               ADD 1 TO WS-RL-ENTRY-ROWS.
       P1320-EXIT.
           EXIT.
       P1325-STORE-BAND-ROW.
           IF WS-BP-POOL-CNT NOT < WS-MAX-RATE-BANDS
               ADD 1 TO WS-RL-DROPPED-ROWS
           ELSE
               ADD 1 TO WS-BP-POOL-CNT
               SET WS-BX TO WS-BP-POOL-CNT
               MOVE RB-BD-BAND-FROM TO WS-BP-FROM (WS-BX)
               MOVE RB-BD-BAND-THRU TO WS-BP-THRU (WS-BX)
               MOVE RB-BD-BAND-RATE TO WS-BP-RATE (WS-BX)
               MOVE RB-BD-BAND-PCT TO WS-BP-PCT (WS-BX)
               ADD 1 TO WS-RL-BAND-ROWS.
       P1325-EXIT.
           EXIT.
       P1330-READ-NEXT-RTBL.
           READ RTBLIN
               AT END MOVE 'Y' TO WS-RL-EOF-SW.
       P1330-EXIT.
           EXIT.
       P1400-PRIME-READS.
           PERFORM P2100-READ-SUMMARY THRU P2100-EXIT.
           PERFORM P2110-READ-RATED THRU P2110-EXIT.
           IF WS-SUM-EOF AND WS-RAT-EOF
               MOVE 'Y' TO WS-EOF-SW.
       P1400-EXIT.
           EXIT.
       P9900-FATAL-OPEN.
           MOVE WS-PGM-NAME TO WS-AB-PGM.
           CALL 'CABABEND' USING WS-AB-PGM WS-AB-PARA WS-AB-REASON
               WS-AB-USER-CODE WS-RC-ABEND.
       P9900-EXIT.
           EXIT.
      * S200-MERGE SECTION - THE TWO-SIDED MATCH.
       S200-MERGE SECTION.
       P2000-PROCESS.
           PERFORM P2200-COMPARE-KEYS THRU P2200-EXIT.
           IF WS-TAKE-BOTH
               PERFORM P2300-MATCHED-PAIR THRU P2300-EXIT
           ELSE
               IF WS-TAKE-SUMMARY
                   PERFORM P2400-SUMMARY-ONLY THRU P2400-EXIT
               ELSE
                   PERFORM P2500-RATED-ONLY THRU P2500-EXIT.
           IF WS-SUM-EOF AND WS-RAT-EOF
               MOVE 'Y' TO WS-EOF-SW.
       P2000-EXIT.
           EXIT.
       P2100-READ-SUMMARY.
           IF NOT WS-SUM-EOF
               READ SUMIN
                   AT END MOVE 'Y' TO WS-SUM-EOF-SW.
           IF WS-SUM-EOF
               MOVE HIGH-VALUES TO WS-SUM-KEY-HOLD
           ELSE
               MOVE CS-KEY TO WS-SUM-KEY-HOLD
               ADD 1 TO WS-READ-CNT.
       P2100-EXIT.
           EXIT.
       P2110-READ-RATED.
           IF NOT WS-RAT-EOF
               READ RATIN
                   AT END MOVE 'Y' TO WS-RAT-EOF-SW.
           IF WS-RAT-EOF
               MOVE HIGH-VALUES TO WS-RAT-KEY-HOLD
           ELSE
               MOVE RS-KEY TO WS-RAT-KEY-HOLD
               ADD 1 TO WS-READ-CNT.
       P2110-EXIT.
           EXIT.
       P2200-COMPARE-KEYS.
           IF WS-SUM-KEY-HOLD = WS-RAT-KEY-HOLD
               MOVE 'B' TO WS-MERGE-ACTION
           ELSE
               IF WS-SUM-KEY-HOLD < WS-RAT-KEY-HOLD
                   MOVE 'S' TO WS-MERGE-ACTION
               ELSE
                   MOVE 'R' TO WS-MERGE-ACTION.
       P2200-EXIT.
           EXIT.
      * P2300-MATCHED-PAIR - BOTH SIDES PRESENT.  THE EXPECTED
      * CHARGE IS REBUILT AND COMPARED, THEN BOTH SIDES ADVANCE.
       P2300-MATCHED-PAIR.
           PERFORM P3000-RECONCILE THRU P3000-EXIT.
           PERFORM P2100-READ-SUMMARY THRU P2100-EXIT.
           PERFORM P2110-READ-RATED THRU P2110-EXIT.
       P2300-EXIT.
           EXIT.
      * P2400-SUMMARY-ONLY - TRAFFIC WAS CONSOLIDATED BUT NEVER
      * RATED.  THAT IS ALWAYS AN EXCEPTION AND IS ALWAYS REPORTED.
       P2400-SUMMARY-ONLY.
           MOVE SPACES TO CABS-RECON-RECORD.
           MOVE CS-OCN TO RC-OCN.
           MOVE CS-BAN TO RC-BAN.
           MOVE CS-RATE-ELEM TO RC-RATE-ELEM.
           MOVE CS-CYCLE-YYDDD TO RC-CYCLE-YYDDD.
           MOVE CS-SITE-CD TO RC-SITE-CD.
           MOVE SPACES TO RC-TARIFF-CD.
           MOVE SPACES TO RC-JURIS-CD.
           MOVE CS-STATE-CD TO RC-STATE-CD.
           MOVE 'S' TO RC-SIDE-IND.
           MOVE CS-MOU-TOTAL TO RC-SUM-MOU.
           MOVE 0 TO RC-RAT-MOU.
           MOVE CS-MOU-TOTAL TO RC-MOU-DIFF.
           MOVE 0 TO RC-EXPECTED-AMT RC-RATED-AMT RC-VARIANCE-AMT.
           MOVE WS-VARIANCE-TOLERANCE TO RC-TOLERANCE-USED.
           MOVE 'OS' TO RC-CLASS-CD.
           MOVE WS-CT-TEXT (5) TO RC-CLASS-TEXT.
           MOVE SPACES TO RC-FILLER.
           WRITE CABS-RECON-RECORD.
           ADD 1 TO WS-WRITE-CNT.
           ADD 1 TO WS-CC-SUM-ONLY.
           PERFORM P7000-PRINT-EXCEPTION THRU P7000-EXIT.
           PERFORM P2100-READ-SUMMARY THRU P2100-EXIT.
       P2400-EXIT.
           EXIT.
      * P2500-RATED-ONLY - RATED WITH NO CONSOLIDATED TRAFFIC
      * BEHIND IT.  USUALLY A RERATE OF A PRIOR PERIOD.
       P2500-RATED-ONLY.
           MOVE SPACES TO CABS-RECON-RECORD.
           MOVE RS-OCN TO RC-OCN.
           MOVE RS-BAN TO RC-BAN.
           MOVE RS-RATE-ELEM TO RC-RATE-ELEM.
           MOVE RS-CYCLE-YYDDD TO RC-CYCLE-YYDDD.
           MOVE PC1-SITE-CD TO RC-SITE-CD.
           MOVE RS-TARIFF-CD TO RC-TARIFF-CD.
           MOVE RS-JURIS-CD TO RC-JURIS-CD.
           MOVE RS-STATE-CD TO RC-STATE-CD.
           MOVE 'R' TO RC-SIDE-IND.
           MOVE 0 TO RC-SUM-MOU.
           MOVE RS-RATED-MOU TO RC-RAT-MOU.
           COMPUTE RC-MOU-DIFF = 0 - RS-RATED-MOU.
           MOVE 0 TO RC-EXPECTED-AMT.
           MOVE RS-RATED-AMOUNT TO RC-RATED-AMT.
           COMPUTE RC-VARIANCE-AMT = 0 - RS-RATED-AMOUNT.
           MOVE WS-VARIANCE-TOLERANCE TO RC-TOLERANCE-USED.
           MOVE 'OS' TO RC-CLASS-CD.
           MOVE WS-CT-TEXT (5) TO RC-CLASS-TEXT.
           MOVE SPACES TO RC-FILLER.
           IF RS-EFF-YYDDD < WS-RETRO-CUTOFF-YYDDD
               ADD 1 TO WS-CC-RETRO-SKIP
               ADD 1 TO WS-CFWD-CNT
           ELSE
               WRITE CABS-RECON-RECORD
               ADD 1 TO WS-WRITE-CNT
               ADD 1 TO WS-CC-RAT-ONLY
               PERFORM P7000-PRINT-EXCEPTION THRU P7000-EXIT.
           PERFORM P2110-READ-RATED THRU P2110-EXIT.
       P2500-EXIT.
           EXIT.
      * S300-RECONCILIATION SECTION
       S300-RECONCILIATION SECTION.
       P3000-RECONCILE.
           PERFORM P3100-LOOKUP-RATE THRU P3100-EXIT.
           IF WS-RL-FOUND
               PERFORM P3200-COMPUTE-EXPECTED THRU P3200-EXIT
               PERFORM P3300-COMPARE-AMOUNTS THRU P3300-EXIT
           ELSE
               MOVE 0 TO WS-EW-TOTAL-AMT
               MOVE 'NR' TO RC-CLASS-CD
               ADD 1 TO WS-CC-NO-RATE.
           PERFORM P3500-BUILD-RECON-REC THRU P3500-EXIT.
           PERFORM P3600-WRITE-RECON THRU P3600-EXIT.
           ADD 1 TO WS-SUMM-CNT.
       P3000-EXIT.
           EXIT.
      * P3100-LOOKUP-RATE - SUBSCRIPTED WALK.  THE FIRST ROW WHOSE
      * ELEMENT, JURISDICTION AND STATE AGREE AND WHOSE EFFECTIVE
      * WINDOW COVERS THE CYCLE DATE IS TAKEN.  THE TABLE IS NOT
      * SORTED BY EFFECTIVE DATE WITHIN KEY, SO WHERE TWO ROWS
      * BOTH COVER THE CYCLE THE ONE LOADED FIRST WINS.
       P3100-LOOKUP-RATE.
           MOVE 'N' TO WS-RL-FOUND-SW.
           MOVE 0 TO WS-RL-HIT-SUB.
           IF WS-RT-ENTRY-CNT > 0
               PERFORM P3110-COMPARE-RATE-ROW THRU P3110-EXIT
                   VARYING WS-RL-SUB FROM 1 BY 1
                   UNTIL WS-RL-SUB > WS-RT-ENTRY-CNT
                   OR WS-RL-FOUND.
           IF WS-RL-FOUND
               SET WS-RX TO WS-RL-HIT-SUB
               MOVE WS-RT-INITIAL (WS-RX) TO WS-RL-INITIAL
               MOVE WS-RT-ADDL (WS-RX) TO WS-RL-ADDL
               MOVE WS-RT-SETUP (WS-RX) TO WS-RL-SETUP
               MOVE WS-RT-ROUND-RULE (WS-RX) TO WS-RL-ROUND-RULE
               MOVE WS-RT-BAND-CNT (WS-RX) TO WS-RL-BAND-CNT
               MOVE WS-RT-BAND-OFFSET (WS-RX) TO WS-RL-BAND-OFFSET.
       P3100-EXIT.
           EXIT.
       P3110-COMPARE-RATE-ROW.
           SET WS-RX TO WS-RL-SUB.
           IF WS-RT-ELEM (WS-RX) = RS-RATE-ELEM
               IF WS-RT-JURIS (WS-RX) = RS-JURIS-CD
                   IF WS-RT-STATE (WS-RX) = RS-STATE-CD
                       PERFORM P3120-CHECK-RATE-WINDOW THRU
                           P3120-EXIT.
       P3110-EXIT.
           EXIT.
       P3120-CHECK-RATE-WINDOW.
           IF WS-RT-EFF-YYDDD (WS-RX) NOT > WS-CV-CYCLE-YYDDD
               IF WS-RT-EXP-YYDDD (WS-RX) = 0 OR
                       WS-RT-EXP-YYDDD (WS-RX) NOT <
                       WS-CV-CYCLE-YYDDD
                   MOVE 'Y' TO WS-RL-FOUND-SW
                   MOVE WS-RL-SUB TO WS-RL-HIT-SUB.
       P3120-EXIT.
           EXIT.
      * P3200-COMPUTE-EXPECTED - THE INITIAL PERIOD IS PRICED AT
      * THE INITIAL RATE AND EVERYTHING BEYOND IT AT THE ADDITIONAL
      * RATE.  THE SETUP CHARGE IS APPLIED PER RATED OCCURRENCE.
       P3200-COMPUTE-EXPECTED.
           MOVE 0 TO WS-EW-INITIAL-AMT WS-EW-ADDL-AMT.
           MOVE 0 TO WS-EW-SETUP-AMT WS-EW-BAND-AMT.
           MOVE 0 TO WS-EW-TOTAL-AMT.
           MOVE CS-MOU-TOTAL TO WS-EW-BILLABLE-MOU.
           IF WS-EW-BILLABLE-MOU > 0
               COMPUTE WS-EW-INITIAL-AMT =
                   WS-EW-BILLABLE-MOU * WS-RL-INITIAL.
           IF WS-EW-BILLABLE-MOU > 1
               COMPUTE WS-EW-ADDL-AMT =
                   (WS-EW-BILLABLE-MOU - 1) * WS-RL-ADDL.
           IF RS-RATED-CNT > 0
               COMPUTE WS-EW-SETUP-AMT = RS-RATED-CNT * WS-RL-SETUP.
           IF WS-RL-BAND-CNT > 0
               PERFORM P3700-APPLY-BAND-RATES THRU P3700-EXIT.
           COMPUTE WS-EW-TOTAL-AMT = WS-EW-INITIAL-AMT +
               WS-EW-ADDL-AMT + WS-EW-SETUP-AMT + WS-EW-BAND-AMT.
           PERFORM P3750-COLLAR-EXPECTED THRU P3750-EXIT.
       P3200-EXIT.
           EXIT.
      * P3300-COMPARE-AMOUNTS - THE TOLERANCE IS ONE SIDED.  A
      * RATED AMOUNT BELOW EXPECTED BY LESS THAN THE TOLERANCE IS
      * AGREED.  A RATED AMOUNT ABOVE EXPECTED IS REPORTED AT ANY
      * SIZE.
       P3300-COMPARE-AMOUNTS.
           COMPUTE WS-EW-VARIANCE =
               RS-RATED-AMOUNT - WS-EW-TOTAL-AMT.
           MOVE WS-EW-VARIANCE TO WS-EW-ABS-VARIANCE.
           IF WS-EW-ABS-VARIANCE < 0
               COMPUTE WS-EW-ABS-VARIANCE = 0 - WS-EW-VARIANCE.
           IF WS-EW-VARIANCE = 0
               MOVE 'AG' TO RC-CLASS-CD
               ADD 1 TO WS-CC-AGREED
           ELSE
               IF WS-EW-VARIANCE < 0 AND
                       WS-EW-ABS-VARIANCE NOT >
                       WS-VARIANCE-TOLERANCE
                   MOVE 'AG' TO RC-CLASS-CD
                   ADD 1 TO WS-CC-AGREED
               ELSE
                   PERFORM P3400-CLASSIFY-VARIANCE THRU P3400-EXIT.
           ADD WS-EW-TOTAL-AMT TO WS-VC-EXPECTED-TOT.
           ADD RS-RATED-AMOUNT TO WS-VC-RATED-TOT.
           ADD WS-EW-VARIANCE TO WS-VC-VARIANCE-TOT.
       P3300-EXIT.
           EXIT.
       P3400-CLASSIFY-VARIANCE.
           IF WS-EW-VARIANCE < 0
               MOVE 'UR' TO RC-CLASS-CD
               ADD 1 TO WS-CC-UNDER
           ELSE
               MOVE 'OR' TO RC-CLASS-CD
               ADD 1 TO WS-CC-OVER.
           PERFORM P7000-PRINT-EXCEPTION THRU P7000-EXIT.
       P3400-EXIT.
           EXIT.
       P3500-BUILD-RECON-REC.
           MOVE SPACES TO RC-FILLER.
           MOVE CS-OCN TO RC-OCN.
           MOVE CS-BAN TO RC-BAN.
           MOVE CS-RATE-ELEM TO RC-RATE-ELEM.
           MOVE CS-CYCLE-YYDDD TO RC-CYCLE-YYDDD.
           MOVE CS-SITE-CD TO RC-SITE-CD.
           MOVE RS-TARIFF-CD TO RC-TARIFF-CD.
           MOVE RS-JURIS-CD TO RC-JURIS-CD.
           MOVE RS-STATE-CD TO RC-STATE-CD.
           MOVE 'B' TO RC-SIDE-IND.
           MOVE CS-MOU-TOTAL TO RC-SUM-MOU.
           MOVE RS-RATED-MOU TO RC-RAT-MOU.
           COMPUTE RC-MOU-DIFF = CS-MOU-TOTAL - RS-RATED-MOU.
           MOVE WS-EW-TOTAL-AMT TO RC-EXPECTED-AMT.
           MOVE RS-RATED-AMOUNT TO RC-RATED-AMT.
           MOVE WS-EW-VARIANCE TO RC-VARIANCE-AMT.
           MOVE WS-VARIANCE-TOLERANCE TO RC-TOLERANCE-USED.
           PERFORM P3510-SET-CLASS-TEXT THRU P3510-EXIT.
           PERFORM P3800-CHECK-CROSS-CENTRE THRU P3800-EXIT.
       P3500-EXIT.
           EXIT.
       P3510-SET-CLASS-TEXT.
           IF RC-AGREED
               MOVE WS-CT-TEXT (1) TO RC-CLASS-TEXT.
           IF RC-UNDER-RATED
               MOVE WS-CT-TEXT (2) TO RC-CLASS-TEXT.
           IF RC-OVER-RATED
               MOVE WS-CT-TEXT (3) TO RC-CLASS-TEXT.
           IF RC-NO-RATE-ROW
               MOVE WS-CT-TEXT (4) TO RC-CLASS-TEXT.
       P3510-EXIT.
           EXIT.
       P3600-WRITE-RECON.
           WRITE CABS-RECON-RECORD.
           ADD 1 TO WS-WRITE-CNT.
           ADD CS-HASH-SEQ TO WS-ACC-SEQ-HASH.
           ADD CS-HASH-OCN TO WS-ACC-OCN-HASH.
           ADD CS-MOU-TOTAL TO WS-ACC-MINUTES.
           ADD WS-EW-TOTAL-AMT TO WS-ACC-AMOUNT.
       P3600-EXIT.
           EXIT.
      * P3700-APPLY-BAND-RATES - WALKS THE BAND SLICE BELONGING TO
      * THE RATE ROW AND PRICES THE BILLABLE MINUTES BAND BY BAND.
      * ANYTHING LEFT OVER ABOVE THE LAST BAND IS PRICED AT THE
      * ADDITIONAL RATE, WHICH IS WHAT THE TARIFF SAYS HAPPENS.
       P3700-APPLY-BAND-RATES.
           MOVE 0 TO WS-EW-BAND-AMT.
           MOVE WS-EW-BILLABLE-MOU TO WS-BW-REMAINING.
           PERFORM P3710-PRICE-ONE-BAND THRU P3710-EXIT
               VARYING WS-BW-SUB FROM 1 BY 1
               UNTIL WS-BW-SUB > WS-RL-BAND-CNT
               OR WS-BW-REMAINING NOT > 0.
           IF WS-BW-REMAINING > 0
               COMPUTE WS-EW-BAND-AMT = WS-EW-BAND-AMT +
                   WS-BW-REMAINING * WS-RL-ADDL.
           MOVE 0 TO WS-EW-INITIAL-AMT.
           MOVE 0 TO WS-EW-ADDL-AMT.
       P3700-EXIT.
           EXIT.
       P3710-PRICE-ONE-BAND.
           COMPUTE WS-BW-POOL-SUB =
               WS-RL-BAND-OFFSET + WS-BW-SUB - 1.
           IF WS-BW-POOL-SUB > 0 AND WS-BW-POOL-SUB NOT >
                   WS-BP-POOL-CNT
               SET WS-BX TO WS-BW-POOL-SUB
               PERFORM P3720-TAKE-BAND-SLICE THRU P3720-EXIT.
       P3710-EXIT.
           EXIT.
       P3720-TAKE-BAND-SLICE.
           COMPUTE WS-BW-IN-BAND =
               WS-BP-THRU (WS-BX) - WS-BP-FROM (WS-BX) + 1.
           IF WS-BW-IN-BAND > WS-BW-REMAINING
               MOVE WS-BW-REMAINING TO WS-BW-IN-BAND.
           IF WS-BW-IN-BAND > 0
               COMPUTE WS-EW-BAND-AMT = WS-EW-BAND-AMT +
                   WS-BW-IN-BAND * WS-BP-RATE (WS-BX)
               SUBTRACT WS-BW-IN-BAND FROM WS-BW-REMAINING.
       P3720-EXIT.
           EXIT.
      * P3750-COLLAR-EXPECTED - THE TARIFF MINIMUM AND MAXIMUM ARE
      * PROPERTIES OF THE RATE ROW AND THE RATING SUITE APPLIES
      * THEM, SO THE EXPECTED AMOUNT HAS TO CARRY THEM TOO OR
      * EVERY COLLARED ELEMENT RECONCILES AS A VARIANCE.
       P3750-COLLAR-EXPECTED.
           SET WS-RX TO WS-RL-HIT-SUB.
           IF WS-RT-MIN-CHG (WS-RX) > 0
               IF WS-EW-TOTAL-AMT < WS-RT-MIN-CHG (WS-RX)
                   MOVE WS-RT-MIN-CHG (WS-RX) TO WS-EW-TOTAL-AMT.
           IF WS-RT-MAX-CHG (WS-RX) > 0
               IF WS-EW-TOTAL-AMT > WS-RT-MAX-CHG (WS-RX)
                   MOVE WS-RT-MAX-CHG (WS-RX) TO WS-EW-TOTAL-AMT.
       P3750-EXIT.
           EXIT.
      * P3800-CHECK-CROSS-CENTRE - THE SUMMARY SIDE CARRIES THE
      * CENTRE THAT CONSOLIDATED IT.  A SUMMARY RAISED BY ANOTHER
      * CENTRE HAS NO BUSINESS RECONCILING HERE AND IS MARKED SO
      * THE VARIANCE REPORT DOES NOT COUNT IT TWICE.
       P3800-CHECK-CROSS-CENTRE.
           IF CS-SITE-CD NOT = PC1-SITE-CD
               MOVE 'OS' TO RC-CLASS-CD
               MOVE WS-CT-TEXT (5) TO RC-CLASS-TEXT
               ADD 1 TO WS-CFWD-CNT.
       P3800-EXIT.
           EXIT.
      * S700-REPORTING SECTION
       S700-REPORTING SECTION.
       P7000-PRINT-EXCEPTION.
           IF PC1-OPT-REPORT NOT = 'N'
               PERFORM P7100-PRINT-DETAIL THRU P7100-EXIT.
       P7000-EXIT.
           EXIT.
       P7100-PRINT-DETAIL.
           IF WS-RPT-LINE-NBR > WS-RPT-MAX-LINES
               PERFORM P7200-PRINT-PAGE-HEAD THRU P7200-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE RC-OCN TO PC-COL-001-020.
           MOVE RC-BAN TO PC-COL-021-060.
           MOVE RC-CLASS-TEXT TO PC-COL-061-090.
           MOVE RC-VARIANCE-AMT TO WS-RPT-AMT-EDIT.
           MOVE WS-RPT-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P7100-EXIT.
           EXIT.
       P7200-PRINT-PAGE-HEAD.
           ADD 1 TO WS-RPT-PAGE-NBR.
           MOVE 0 TO WS-RPT-LINE-NBR.
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
           MOVE 'OCN' TO PC-COL-001-020.
           MOVE 'BILLING ACCOUNT NUMBER' TO PC-COL-021-060.
           MOVE 'CLASSIFICATION' TO PC-COL-061-090.
           MOVE 'VARIANCE' TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           WRITE CABS-PRINT-LINE.
       P7200-EXIT.
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
           MOVE 'RECONCILIATION TOTALS' TO PC-TEXT.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'AGREED' TO PC-COL-001-020.
           MOVE WS-CC-AGREED TO WS-RPT-CNT-EDIT.
           MOVE WS-RPT-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RATED BELOW EXPECTED' TO PC-COL-001-020.
           MOVE WS-CC-UNDER TO WS-RPT-CNT-EDIT.
           MOVE WS-RPT-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RATED ABOVE EXPECTED' TO PC-COL-001-020.
           MOVE WS-CC-OVER TO WS-RPT-CNT-EDIT.
           MOVE WS-RPT-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'NO EFFECTIVE RATE' TO PC-COL-001-020.
           MOVE WS-CC-NO-RATE TO WS-RPT-CNT-EDIT.
           MOVE WS-RPT-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'SUMMARY SIDE ONLY' TO PC-COL-001-020.
           MOVE WS-CC-SUM-ONLY TO WS-RPT-CNT-EDIT.
           MOVE WS-RPT-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RATED SIDE ONLY' TO PC-COL-001-020.
           MOVE WS-CC-RAT-ONLY TO WS-RPT-CNT-EDIT.
           MOVE WS-RPT-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'HELD - BEFORE CUTOFF' TO PC-COL-001-020.
           MOVE WS-CC-RETRO-SKIP TO WS-RPT-CNT-EDIT.
           MOVE WS-RPT-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'EXPECTED VALUE' TO PC-COL-001-020.
           MOVE WS-VC-EXPECTED-TOT TO WS-RPT-AMT-EDIT.
           MOVE WS-RPT-AMT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RATED VALUE' TO PC-COL-001-020.
           MOVE WS-VC-RATED-TOT TO WS-RPT-AMT-EDIT.
           MOVE WS-RPT-AMT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'NET VARIANCE' TO PC-COL-001-020.
           MOVE WS-VC-VARIANCE-TOT TO WS-RPT-AMT-EDIT.
           MOVE WS-RPT-AMT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           IF WS-RT-TABLE-FULL
               MOVE 'RATE TABLE FILLED - SOME ROWS NOT LOADED' TO
                   PC-TEXT
           ELSE
               MOVE 'RATE TABLE LOADED IN FULL' TO PC-TEXT.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE 2 TO CT-STEP-SEQ.
           MOVE WS-CV-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
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
       P8100-EXIT.
           EXIT.
      * P8200-CHECK-BALANCE - A MATCHED PAIR CONSUMES TWO INPUT
      * RECORDS AND PRODUCES ONE OUTPUT RECORD, SO THE SUMMARISED
      * COUNT IS DOUBLED BEFORE THE EQUATION IS TESTED AND THE
      * WRITTEN COUNT IS ZEROED.
       P8200-CHECK-BALANCE.
           COMPUTE CT-SUMMARISED = WS-SUMM-CNT * 2.
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
           CLOSE SUMIN.
           CLOSE RATIN.
           CLOSE RTBLIN.
           CLOSE RECOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABCTC02 - RUN COMPLETE'.
           DISPLAY '  RECORDS READ = ' WS-READ-CNT.
           DISPLAY '  RECON OUT    = ' WS-WRITE-CNT.
           DISPLAY '  MATCHED      = ' WS-SUMM-CNT.
           DISPLAY '  AGREED       = ' WS-CC-AGREED.
           DISPLAY '  UNDER RATED  = ' WS-CC-UNDER.
           DISPLAY '  OVER RATED   = ' WS-CC-OVER.
           DISPLAY '  NO RATE ROW  = ' WS-CC-NO-RATE.
           DISPLAY '  SUM ONLY     = ' WS-CC-SUM-ONLY.
           DISPLAY '  RAT ONLY     = ' WS-CC-RAT-ONLY.
           DISPLAY '  RATE ENTRIES = ' WS-RL-ENTRY-ROWS.
           DISPLAY '  RATE BANDS   = ' WS-RL-BAND-ROWS.
       P9000-EXIT.
           EXIT.
