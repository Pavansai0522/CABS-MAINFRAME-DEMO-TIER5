      *****************************************************************
      * CABRAT06 - BANDED / VOLUME RATE SELECTION                     *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               RATIN   TELCABS.CABS.RATED(0)         (LOCAL)   *
      *               RTBLIN  TELCABS.CABS.RATETBL(0)        (LOCAL)  *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               BNDOUT  TELCABS.CABS.RATED.BANDED(+1)  (LOCAL)  *
      *               SUSOUT  TELCABS.CABS.USAGE.SUSPENSE(+1)CABSERR  *
      *               RPTOUT  SYSOUT CLASS A                CABSPRNT  *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +             *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - RTBLIN IS RELOADED EVERY RUN       *
      * REVISION HISTORY                                              *
      *   V1.00  1988-07-08  R.T.WHEELER  INITIAL RELEASE - STEPPED   *
      *                      BANDING ONLY, ONE BAND PER ELEMENT       *
      *   V1.03  1993-02-19  D.OKONKWO    MULTI-BAND TABLES SUPPORTED *
      *   V1.06  1998-10-05  J.M.CASTILLO GRADUATED (TRANCHE) BANDING *
      *                      MODE ADDED FOR THE VOLUME DISCOUNT       *
      *                      TARIFF FILING                            *
      *   V1.08  2004-04-27  P.NAIR       RTBLIN REPLACED A DIRECT    *
      *                      RATEMST BROWSE - THIS PROGRAM NO LONGER  *
      *                      OPENS THE VSAM RATE FILE AT ALL          *
      *   V1.10  2009-11-11  A.BUKOWSKI   BAND CONTAINMENT LOGIC      *
      *                      REWRITTEN FOR CLARITY DURING THE Y2010   *
      *                      RATE ENGINE REFRESH                      *
      *   V1.12  2015-06-30  K.ADEYEMI    BAND-MODE DISPATCH REORDERED*
      *                      SO STEPPED IS CHECKED BEFORE GRADUATED   *
      *   V1.13  2019-05-09  G.PRZYBYLSKI RECOMPILE ONLY - CABSRT04   *
      *                      FIELD WIDTH CHANGE                       *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABRAT06.
       AUTHOR. TELCABS APPLICATIONS - RATING TEAM.
      *****************************************************************
      * FOR ELEMENTS FLAGGED AS VOLUME-BANDED (THE LOADED RATE TABLE  *
      * ROW CARRIES ONE OR MORE BANDS) THIS PROGRAM LOOKS UP THE      *
      * APPLICABLE BAND FOR THE RECORD'S ACCUMULATED QUANTITY AND     *
      * RE-RATES IT.  THREE BAND MODES ARE SUPPORTED - STEPPED (WHOLE *
      * VOLUME AT ONE BAND'S RATE), GRADUATED (EACH TRANCHE AT ITS    *
      * OWN RATE, SUMMED) AND FLAT (PASS THROUGH UNCHANGED).  THE     *
      * RATE TABLE AND BAND POOL ARE LOADED FROM RTBLIN, THE FLAT     *
      * EXTRACT CABRAT01 WRITES - NOT FROM RATEMST DIRECTLY.  COPIES  *
      * CABSRT01 LIKE EVERY RATING PROGRAM; UNLIKE MOST OF THE        *
      * FAMILY THIS ONE ACTUALLY USES ALL FOUR NESTED LEVELS.         *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           C01 IS TO-NEW-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RATIN ASSIGN TO UT-S-RATIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT RTBLIN ASSIGN TO UT-S-RTBLIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-TABLE.
           SELECT BNDOUT ASSIGN TO UT-S-BNDOUT
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
      * RATIN - ALREADY-RATED CDR PASS-THROUGH, CANDIDATE FOR BANDING.
       FD  RATIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       01  CABS-RATIN-RECORD.
           05  RI-OCN                      PIC X(04).
           05  RI-BAN                      PIC X(13).
           05  RI-SEQ-NBR                  PIC 9(09) COMP-3.
           05  RI-RATE-ELEM                PIC X(06).
           05  RI-JURIS-CD                 PIC X(01).
           05  RI-STATE-CD                 PIC X(02).
           05  RI-QTY                      PIC S9(13)V9(02) COMP-3.
           05  RI-RATE                     PIC S9(05)V9(05) COMP-3.
           05  RI-AMOUNT                   PIC S9(11)V9(05) COMP-3.
           05  RI-BAND-MODE                PIC X(01).
           05  RI-CYCLE-YYDDD              PIC 9(05).
           05  RI-FILLER                   PIC X(129).
      * RTBLIN - FLATTENED RATE TABLE EXTRACT FROM CABRAT01.  SAME
      * 'E'/'B' LAYOUT CABRAT01 WRITES TO RTBLOUT.
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
      * BNDOUT - RE-RATED / BANDED OUTPUT.
       FD  BNDOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       01  CABS-BANDED-RECORD.
           05  BN-OCN                      PIC X(04).
           05  BN-BAN                      PIC X(13).
           05  BN-SEQ-NBR                  PIC 9(09) COMP-3.
           05  BN-RATE-ELEM                PIC X(06).
           05  BN-JURIS-CD                 PIC X(01).
           05  BN-STATE-CD                 PIC X(02).
           05  BN-QTY                      PIC S9(13)V9(02) COMP-3.
           05  BN-BAND-MODE                PIC X(01).
           05  BN-ORIG-RATE                PIC S9(05)V9(05) COMP-3.
           05  BN-ORIG-AMOUNT              PIC S9(11)V9(05) COMP-3.
           05  BN-BANDED-RATE              PIC S9(05)V9(05) COMP-3.
           05  BN-BANDED-AMOUNT            PIC S9(11)V9(05) COMP-3.
           05  BN-BAND-APPLIED-SW          PIC X(01).
               88  BN-BAND-APPLIED         VALUE 'Y'.
           05  BN-RATE-STATUS              PIC X(01).
               88  BN-RATED                VALUE 'R'.
               88  BN-SUSPENDED             VALUE 'S'.
           05  BN-CYCLE-YYDDD              PIC 9(05).
           05  BN-FILLER                   PIC X(113).
      * SUSOUT - REJECTED / SUSPENDED USAGE.
       FD  SUSOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 300 CHARACTERS.
       01  CABS-SUSPENSE-RECORD-FD.
           05  FD-SU-ERR-CODE              PIC X(04).
           05  FD-SU-ERR-SEVERITY          PIC X(01).
           05  FD-SU-DETECT-PGM            PIC X(08).
           05  FD-SU-DETECT-PARA           PIC X(30).
           05  FD-SU-RUN-ID                PIC X(12).
           05  FD-SU-ORIG-RECORD           PIC X(200).
           05  FD-SU-FILLER                PIC X(45).
      * CTLOUT - RUN CONTROL / BALANCING RECORD.
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD              PIC X(180).
      * RPTOUT - EXCEPTION AND CONTROL SUMMARY REPORT.
       FD  RPTOUT
           RECORDING MODE IS FA
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       COPY CABSPRNT.
       WORKING-STORAGE SECTION.
      * STANDARD SHARED WORKING STORAGE.  SEE CABSWRK.
       COPY CABSWRK.
      * RATING FAMILY CONTROL BLOCKS.  THIS PROGRAM ACTUALLY USES ALL
      * FOUR NESTED LEVELS - R1 RUN CONTROL, R2 RATE TABLE, R3 BAND
      * POOL AND WORK, R4 ROUNDING.
       COPY CABSRT01.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABRAT06'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V1.13'.
      * SYSIN PARM CARD.
       01  WS-PARM-CARD                    PIC X(80).
       01  WS-PARM-CARD-R1 REDEFINES WS-PARM-CARD.
           05  PC1-CYCLE-YYDDD              PIC 9(05).
           05  PC1-BILL-PERIOD              PIC 9(06).
           05  PC1-RUN-ID                   PIC X(12).
           05  PC1-FILLER                   PIC X(57).
       01  WS-RTBL-LOAD-WORK.
           05  WS-RTBL-EOF-SW               PIC X(01) VALUE 'N'.
               88  WS-RTBL-EOF              VALUE 'Y'.
      * ELEMENT SEARCH WORK - LINEAR SCAN OF THE LOADED R2 TABLE BY
      * ELEMENT / JURISDICTION / STATE.
       01  WS-ELEMENT-SEARCH-WORK.
           05  WS-EL-FOUND-SW               PIC X(01) VALUE 'N'.
               88  WS-EL-FOUND              VALUE 'Y'.
           05  WS-EL-FOUND-SUB              PIC S9(04) COMP-3 VALUE 0.
      * BAND-MODE WORK - OVERLAPPING 88 LEVELS.  WS-BM-STEPPED AND
      * WS-BM-GRADUATED ARE BOTH TRUE FOR 'G'.  P3000 TESTS STEPPED
      * FIRST, SO A GRADUATED-FLAGGED RECORD IS RATED STEPPED - THIS
      * HAS BEEN TRUE SINCE V1.12 AND NO ONE HAS ASKED FOR IT BACK.
      * CABSRT03's R3-BW-STEPPED / R3-BW-GRADUATED CARRY THE SAME
      * OVERLAP AND ARE ALSO SET BELOW, FOR THE SAME RECORD.
       01  WS-BAND-MODE-WORK.
           05  WS-BAND-MODE                 PIC X(01) VALUE 'S'.
               88  WS-BM-STEPPED            VALUE 'S' 'G'.
               88  WS-BM-GRADUATED          VALUE 'G' 'T'.
               88  WS-BM-FLAT               VALUE 'F'.
           05  WS-BAND-QTY                  PIC S9(13)V9(02) COMP-3
                                                             VALUE 0.
           05  WS-BW-SUB                    PIC S9(04) COMP-3 VALUE 0.
           05  WS-BW-POOL-SUB               PIC S9(04) COMP-3 VALUE 0.
       01  WS-TRANCHE-WORK.
           05  WS-TRANCHE-QTY               PIC S9(13)V9(02) COMP-3
                                                             VALUE 0.
           05  WS-TRANCHE-AMT               PIC S9(11)V9(05) COMP-3
                                                             VALUE 0.
       01  WS-RESULT-WORK.
           05  WS-BANDED-RATE               PIC S9(05)V9(05) COMP-3
                                                             VALUE 0.
           05  WS-BANDED-AMOUNT             PIC S9(11)V9(05) COMP-3
                                                             VALUE 0.
           05  WS-BAND-APPLIED-SW           PIC X(01) VALUE 'N'.
               88  WS-BAND-WAS-APPLIED      VALUE 'Y'.
      * VALIDATION SWITCH.
       01  WS-VALIDATION-WORK.
           05  WS-VALID-SW                  PIC X(01) VALUE 'Y'.
               88  WS-RECORD-VALID          VALUE 'Y'.
           05  WS-CURRENT-ERR-CODE          PIC X(04) VALUE SPACES.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                    PIC X(08).
           05  WS-AB-PARA                   PIC X(30).
           05  WS-AB-REASON                 PIC X(60).
           05  WS-AB-USER-CODE              PIC 9(04) VALUE 9906.
       01  WS-EXT-CALL-RC.
           05  WS-RC-ERRWR                  PIC 9(04) VALUE 0.
           05  WS-RC-HASH                   PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                  PIC 9(04) VALUE 0.
       01  WS-HASH-WORK.
           05  WS-HW-OCN                    PIC S9(15) COMP-3 VALUE 0.
           05  WS-HW-SEQ                    PIC S9(17) COMP-3 VALUE 0.
           05  WS-HW-MINUTES                PIC S9(15)V9(02) COMP-3
                                                             VALUE 0.
           05  WS-HW-AMOUNT                 PIC S9(13)V9(05) COMP-3
                                                             VALUE 0.
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR              PIC S9(03) COMP-3 VALUE 0.
           05  WS-RPT-TITLE1                 PIC X(60) VALUE
               'CABRAT06 - BANDED / VOLUME RATE SELECTION'.
       01  WS-MISC-COUNTERS.
           05  WS-MC-ELEMENT-NOT-FOUND       PIC S9(07) COMP-3
                                                             VALUE 0.
           05  WS-MC-BANDED-STEPPED          PIC S9(07) COMP-3
                                                             VALUE 0.
           05  WS-MC-BANDED-GRADUATED        PIC S9(07) COMP-3
                                                             VALUE 0.
           05  WS-MC-PASS-THROUGH            PIC S9(07) COMP-3
                                                             VALUE 0.
           05  WS-TL-RATE-ROWS-LOADED        PIC S9(05) COMP-3
                                                             VALUE 0.
           05  WS-TL-BAND-ROWS-LOADED        PIC S9(05) COMP-3
                                                             VALUE 0.
       PROCEDURE DIVISION.
      * P0000-MAINLINE - MANDATORY CABS BATCH SHAPE.
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
           PERFORM P1600-READ-FIRST-RATIN THRU P1600-EXIT.
       P1000-EXIT.
           EXIT.
      * P1100-OPEN-FILES - THE ABEND REASON IS JUST THE DDNAME.
       P1100-OPEN-FILES.
           OPEN INPUT RATIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'RATIN' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN INPUT RTBLIN.
           IF WS-FS-TABLE NOT = '00'
               MOVE 'RTBLIN' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT BNDOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'BNDOUT' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SUSOUT.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 'SUSOUT' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'CTLOUT' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RPTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'RPTOUT' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
       P1100-EXIT.
           EXIT.
       P1200-READ-PARM.
           MOVE SPACES TO WS-PARM-CARD.
           ACCEPT WS-PARM-CARD FROM SYSIN.
           MOVE PC1-RUN-ID TO R1-RUN-ID.
           MOVE PC1-CYCLE-YYDDD TO R1-CYCLE-YYDDD.
           MOVE PC1-BILL-PERIOD TO R1-BILL-PERIOD.
       P1200-EXIT.
           EXIT.
      *****************************************************************
      * P1300-LOAD-RATE-TABLE - REBUILDS THE R2 / R3 ODO              *
      * TABLES FROM THE FLAT RTBLIN EXTRACT.  THE DEPENDING-ON ITEMS  *
      * (R2-ENTRY-CNT, R3-POOL-CNT) ARE INCREMENTED AND THE INDEXES   *
      * SET FROM THEM BEFORE ANY R2-ENTRY OR R3-POOL-ENTRY REFERENCE  *
      * - THE SAME RULE CABRAT01 FOLLOWS WHEN IT BUILDS THE TABLES.   *
      *****************************************************************
       P1300-LOAD-RATE-TABLE.
           MOVE 0 TO R2-ENTRY-CNT.
           MOVE 0 TO R3-POOL-CNT.
           PERFORM P1310-READ-RTBLIN THRU P1310-EXIT.
           PERFORM P1320-LOAD-ONE-RTBL-REC THRU P1320-EXIT
               UNTIL WS-RTBL-EOF.
       P1300-EXIT.
           EXIT.
       P1310-READ-RTBLIN.
           READ RTBLIN
               AT END MOVE 'Y' TO WS-RTBL-EOF-SW.
       P1310-EXIT.
           EXIT.
       P1320-LOAD-ONE-RTBL-REC.
           IF RB-REC-TYPE = 'E'
               PERFORM P1330-LOAD-ENTRY-ROW THRU P1330-EXIT
           ELSE
               IF RB-REC-TYPE = 'B'
                   PERFORM P1340-LOAD-BAND-ROW THRU P1340-EXIT.
           PERFORM P1310-READ-RTBLIN THRU P1310-EXIT.
       P1320-EXIT.
           EXIT.
       P1330-LOAD-ENTRY-ROW.
           IF R2-ENTRY-CNT < 600
               ADD 1 TO R2-ENTRY-CNT
               SET R2-EX TO R2-ENTRY-CNT
               MOVE RB-EN-TARIFF TO R2-EN-TARIFF (R2-EX)
               MOVE RB-EN-ELEM TO R2-EN-ELEM (R2-EX)
               MOVE RB-EN-JURIS TO R2-EN-JURIS (R2-EX)
               MOVE RB-EN-STATE TO R2-EN-STATE (R2-EX)
               MOVE RB-EN-EFF-YYDDD TO R2-EN-EFF-YYDDD (R2-EX)
               MOVE RB-EN-EXP-YYDDD TO R2-EN-EXP-YYDDD (R2-EX)
               MOVE RB-EN-INITIAL TO R2-EN-INITIAL (R2-EX)
               MOVE RB-EN-ADDL TO R2-EN-ADDL (R2-EX)
               MOVE RB-EN-ROUND-RULE TO R2-EN-ROUND-RULE (R2-EX)
               MOVE RB-EN-ROUND-POS TO R2-EN-ROUND-POS (R2-EX)
               MOVE RB-EN-BAND-CNT TO R2-EN-BAND-CNT (R2-EX)
               MOVE RB-EN-BAND-OFFSET TO R2-EN-BAND-OFFSET (R2-EX)
               ADD 1 TO WS-TL-RATE-ROWS-LOADED.
       P1330-EXIT.
           EXIT.
       P1340-LOAD-BAND-ROW.
           IF R3-POOL-CNT < 2400
               ADD 1 TO R3-POOL-CNT
               SET R3-PX TO R3-POOL-CNT
               MOVE RB-BD-BAND-FROM TO R3-PL-FROM (R3-PX)
               MOVE RB-BD-BAND-THRU TO R3-PL-THRU (R3-PX)
               MOVE RB-BD-BAND-RATE TO R3-PL-RATE (R3-PX)
               MOVE RB-BD-BAND-PCT TO R3-PL-PCT (R3-PX)
               ADD 1 TO WS-TL-BAND-ROWS-LOADED.
       P1340-EXIT.
           EXIT.
       P1600-READ-FIRST-RATIN.
           PERFORM P2100-READ-RATIN THRU P2100-EXIT.
       P1600-EXIT.
           EXIT.
       P9900-FATAL-OPEN.
           MOVE WS-PGM-NAME TO WS-AB-PGM.
           MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA.
           CALL 'CABABEND' USING WS-AB-PGM WS-AB-PARA WS-AB-REASON
               WS-AB-USER-CODE WS-RC-ABEND.
       P9900-EXIT.
           EXIT.
      * S200-MAIN-PROCESS SECTION - ONE PASS PER RATIN RECORD.
       S200-MAIN-PROCESS SECTION.
       P2000-PROCESS.
           ADD 1 TO WS-READ-CNT.
           MOVE 'Y' TO WS-VALID-SW.
           MOVE 'N' TO WS-BAND-APPLIED-SW.
           PERFORM P2300-FIND-ELEMENT THRU P2300-EXIT.
           IF WS-RECORD-VALID
               PERFORM P3000-APPLY-BANDING THRU P3000-EXIT
               PERFORM P3500-WRITE-BANDED-OUTPUT THRU P3500-EXIT
           ELSE
               PERFORM P3900-REJECT-TO-SUSPENSE THRU P3900-EXIT.
           PERFORM P2100-READ-RATIN THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-RATIN.
           IF NOT WS-EOF
               READ RATIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * P2300-FIND-ELEMENT - LINEAR SEARCH OF THE LOADED RATE TABLE.
       P2300-FIND-ELEMENT.
           MOVE 'N' TO WS-EL-FOUND-SW.
           IF R2-ENTRY-CNT > 0
               PERFORM P2310-SEARCH-ONE-ELEMENT THRU P2310-EXIT
                   VARYING R2-EX FROM 1 BY 1
                   UNTIL R2-EX > R2-ENTRY-CNT OR WS-EL-FOUND.
           IF WS-EL-FOUND
               SET R2-EX TO WS-EL-FOUND-SUB
           ELSE
               MOVE 'N' TO WS-VALID-SW
               MOVE EC-RATE-NOT-FOUND TO WS-CURRENT-ERR-CODE
               ADD 1 TO WS-MC-ELEMENT-NOT-FOUND.
       P2300-EXIT.
           EXIT.
       P2310-SEARCH-ONE-ELEMENT.
           IF R2-EN-ELEM (R2-EX) = RI-RATE-ELEM AND
                   R2-EN-JURIS (R2-EX) = RI-JURIS-CD AND
                   R2-EN-STATE (R2-EX) = RI-STATE-CD
               MOVE 'Y' TO WS-EL-FOUND-SW
               SET WS-EL-FOUND-SUB TO R2-EX.
       P2310-EXIT.
           EXIT.
      *****************************************************************
      * S300-BANDING SECTION.                                         *
      *****************************************************************
       S300-BANDING SECTION.
      * P3000-APPLY-BANDING - AN ELEMENT WITH NO BANDS ON ITS LOADED
      * RATE ROW PASSES THROUGH UNCHANGED.  OTHERWISE DISPATCH ON
      * BAND MODE - STEPPED IS TESTED FIRST (SEE THE WS-BAND-MODE
      * OVERLAP NOTE IN WORKING-STORAGE).
       P3000-APPLY-BANDING.
           MOVE RI-BAND-MODE TO WS-BAND-MODE.
           MOVE RI-BAND-MODE TO R3-BW-MODE.
           MOVE RI-QTY TO WS-BAND-QTY.
           IF R2-EN-BAND-CNT (R2-EX) = 0
               PERFORM P3400-RATE-UNBANDED THRU P3400-EXIT
           ELSE
               IF WS-BM-STEPPED
                   PERFORM P3100-RATE-STEPPED THRU P3100-EXIT
                   ADD 1 TO WS-MC-BANDED-STEPPED
               ELSE
                   IF WS-BM-GRADUATED
                       PERFORM P3300-RATE-GRADUATED THRU P3300-EXIT
                       ADD 1 TO WS-MC-BANDED-GRADUATED
                   ELSE
                       PERFORM P3400-RATE-UNBANDED THRU P3400-EXIT.
       P3000-EXIT.
           EXIT.
      * P3100-RATE-STEPPED - THE WHOLE QUANTITY IS BILLED AT THE ONE
      * BAND RATE SELECTED BY P3200-SELECT-BAND.
       P3100-RATE-STEPPED.
           PERFORM P3200-SELECT-BAND THRU P3200-EXIT.
           IF R3-BW-FOUND
               COMPUTE WS-BANDED-AMOUNT ROUNDED =
                   RI-QTY * R3-BW-SEL-RATE
               MOVE R3-BW-SEL-RATE TO WS-BANDED-RATE
               MOVE 'Y' TO WS-BAND-APPLIED-SW
           ELSE
               MOVE RI-RATE TO WS-BANDED-RATE
               MOVE RI-AMOUNT TO WS-BANDED-AMOUNT.
       P3100-EXIT.
           EXIT.
       P3200-SELECT-BAND.
           MOVE 'N' TO R3-BW-FOUND-SW.
           PERFORM P3210-TEST-ONE-BAND THRU P3210-EXIT
               VARYING WS-BW-SUB FROM 1 BY 1
               UNTIL WS-BW-SUB > R2-EN-BAND-CNT (R2-EX).
       P3200-EXIT.
           EXIT.
       P3210-TEST-ONE-BAND.
           COMPUTE WS-BW-POOL-SUB =
               R2-EN-BAND-OFFSET (R2-EX) + WS-BW-SUB - 1.
           SET R3-PX TO WS-BW-POOL-SUB.
           IF WS-BAND-QTY > R3-PL-FROM (R3-PX)
              AND WS-BAND-QTY NOT > R3-PL-THRU (R3-PX)
              MOVE R3-PL-RATE (R3-PX) TO R3-BW-SEL-RATE
              MOVE 'Y' TO R3-BW-FOUND-SW
              GO TO P3200-EXIT.
       P3210-EXIT.
           EXIT.
      * P3300-RATE-GRADUATED - EACH BAND TRANCHE CROSSED BY THE
      * QUANTITY IS BILLED AT ITS OWN RATE AND SUMMED.
       P3300-RATE-GRADUATED.
           MOVE 0 TO WS-BANDED-AMOUNT.
           PERFORM P3310-APPLY-ONE-TRANCHE THRU P3310-EXIT
               VARYING WS-BW-SUB FROM 1 BY 1
               UNTIL WS-BW-SUB > R2-EN-BAND-CNT (R2-EX).
           MOVE R3-PL-RATE (R3-PX) TO WS-BANDED-RATE.
           MOVE 'Y' TO WS-BAND-APPLIED-SW.
       P3300-EXIT.
           EXIT.
       P3310-APPLY-ONE-TRANCHE.
           COMPUTE WS-BW-POOL-SUB =
               R2-EN-BAND-OFFSET (R2-EX) + WS-BW-SUB - 1.
           SET R3-PX TO WS-BW-POOL-SUB.
           IF WS-BAND-QTY > R3-PL-FROM (R3-PX)
               IF WS-BAND-QTY NOT > R3-PL-THRU (R3-PX)
                   COMPUTE WS-TRANCHE-QTY =
                       WS-BAND-QTY - R3-PL-FROM (R3-PX)
               ELSE
                   COMPUTE WS-TRANCHE-QTY =
                       R3-PL-THRU (R3-PX) - R3-PL-FROM (R3-PX)
               COMPUTE WS-TRANCHE-AMT ROUNDED =
                   WS-TRANCHE-QTY * R3-PL-RATE (R3-PX)
               ADD WS-TRANCHE-AMT TO WS-BANDED-AMOUNT.
       P3310-EXIT.
           EXIT.
       P3400-RATE-UNBANDED.
           MOVE RI-RATE TO WS-BANDED-RATE.
           MOVE RI-AMOUNT TO WS-BANDED-AMOUNT.
           ADD 1 TO WS-MC-PASS-THROUGH.
       P3400-EXIT.
           EXIT.
      * S350-OUTPUT SECTION.
       S350-OUTPUT SECTION.
       P3500-WRITE-BANDED-OUTPUT.
           MOVE RI-OCN TO BN-OCN.
           MOVE RI-BAN TO BN-BAN.
           MOVE RI-SEQ-NBR TO BN-SEQ-NBR.
           MOVE RI-RATE-ELEM TO BN-RATE-ELEM.
           MOVE RI-JURIS-CD TO BN-JURIS-CD.
           MOVE RI-STATE-CD TO BN-STATE-CD.
           MOVE RI-QTY TO BN-QTY.
           MOVE RI-BAND-MODE TO BN-BAND-MODE.
           MOVE RI-RATE TO BN-ORIG-RATE.
           MOVE RI-AMOUNT TO BN-ORIG-AMOUNT.
           MOVE WS-BANDED-RATE TO BN-BANDED-RATE.
           MOVE WS-BANDED-AMOUNT TO BN-BANDED-AMOUNT.
           MOVE WS-BAND-APPLIED-SW TO BN-BAND-APPLIED-SW.
           MOVE 'R' TO BN-RATE-STATUS.
           MOVE RI-CYCLE-YYDDD TO BN-CYCLE-YYDDD.
           MOVE SPACES TO BN-FILLER.
           WRITE CABS-BANDED-RECORD.
           ADD 1 TO WS-WRITE-CNT.
           CALL 'CABHASH' USING RI-OCN WS-HW-OCN.
           ADD RI-SEQ-NBR TO WS-HW-SEQ.
           ADD RI-QTY TO WS-HW-MINUTES.
           ADD WS-BANDED-AMOUNT TO WS-HW-AMOUNT.
       P3500-EXIT.
           EXIT.
      * P3900-REJECT-TO-SUSPENSE - ELEMENT NOT FOUND ON THE LOADED
      * RATE TABLE LANDS HERE.
       P3900-REJECT-TO-SUSPENSE.
           MOVE WS-CURRENT-ERR-CODE TO FD-SU-ERR-CODE.
           MOVE 'E' TO FD-SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO FD-SU-DETECT-PGM.
           MOVE 'P2300-FIND-ELEMENT' TO FD-SU-DETECT-PARA.
           MOVE R1-RUN-ID TO FD-SU-RUN-ID.
           MOVE CABS-RATIN-RECORD TO FD-SU-ORIG-RECORD.
           MOVE SPACES TO FD-SU-FILLER.
           CALL 'CABERRWR' USING FD-SU-ERR-CODE FD-SU-DETECT-PGM
               FD-SU-RUN-ID WS-RC-ERRWR.
           WRITE CABS-SUSPENSE-RECORD-FD.
           ADD 1 TO WS-REJECT-CNT.
       P3900-EXIT.
           EXIT.
      * S800-CONTROL-BALANCE SECTION.
       S800-CONTROL-BALANCE SECTION.
       P8000-CONTROL.
           PERFORM P8010-PRINT-REPORT THRU P8010-EXIT.
           PERFORM P8100-BUILD-CONTROL-REC THRU P8100-EXIT.
           PERFORM P8200-CHECK-BALANCE THRU P8200-EXIT.
           PERFORM P8300-WRITE-CONTROL-REC THRU P8300-EXIT.
       P8000-EXIT.
           EXIT.
       P8010-PRINT-REPORT.
           ADD 1 TO WS-RPT-PAGE-NBR.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '1' TO PC-CC.
           MOVE WS-RPT-TITLE1 TO PC-TEXT.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RATE ROWS LOADED' TO PC-COL-001-020.
           MOVE WS-TL-RATE-ROWS-LOADED TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'BAND ROWS LOADED' TO PC-COL-001-020.
           MOVE WS-TL-BAND-ROWS-LOADED TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'READ' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'WRITTEN' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'REJECTED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'ELEMENT NOT FOUND' TO PC-COL-001-020.
           MOVE WS-MC-ELEMENT-NOT-FOUND TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'BANDED STEPPED' TO PC-COL-001-020.
           MOVE WS-MC-BANDED-STEPPED TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'BANDED GRADUATED' TO PC-COL-001-020.
           MOVE WS-MC-BANDED-GRADUATED TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'PASS THROUGH UNBANDED' TO PC-COL-001-020.
           MOVE WS-MC-PASS-THROUGH TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
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
           MOVE 0 TO CT-CARRIED-FWD.
           MOVE WS-HW-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-HW-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-HW-SEQ TO CT-HASH-SEQ.
           MOVE WS-HW-OCN TO CT-HASH-OCN.
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
      * S900-TERMINATION SECTION.
       S900-TERMINATION SECTION.
       P9000-TERM.
           CLOSE RATIN.
           CLOSE RTBLIN.
           CLOSE BNDOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABRAT06 - RUN COMPLETE'.
           DISPLAY '  READ        = ' WS-READ-CNT.
           DISPLAY '  WRITTEN     = ' WS-WRITE-CNT.
           DISPLAY '  REJECTED    = ' WS-REJECT-CNT.
           DISPLAY '  STEPPED     = ' WS-MC-BANDED-STEPPED.
           DISPLAY '  GRADUATED   = ' WS-MC-BANDED-GRADUATED.
       P9000-EXIT.
           EXIT.
