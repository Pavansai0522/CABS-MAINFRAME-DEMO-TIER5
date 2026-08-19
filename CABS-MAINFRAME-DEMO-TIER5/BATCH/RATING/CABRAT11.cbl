      *****************************************************************
      * CABRAT11 - RECIPROCAL COMPENSATION RATING                     *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               RECIN   TELCABS.CABS.USAGE.CLEAN(0)  CABSCDR    *
      *               CARRMST TELCABS.CABS.CARRIER (VSAM KSDS)CABSCARR*
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               RATOUT  TELCABS.CABS.RATED(+1)        (LOCAL)   *
      *               SUSOUT  TELCABS.CABS.USAGE.SUSPENSE(+1)CABSERR  *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +             *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN                                      *
      * REVISION HISTORY                                              *
      *   V1.00  1997-03-18  D.OKONKWO    INITIAL - FLAT RATE, NO CAP*
      *   V1.02  1999-11-09  J.M.CASTILLO ISP REMAND CAP ADDED PER    *
      *                      FCC ORDER - CR-ISP-CAP-MOU ENFORCED      *
      *   V1.05  2002-06-25  P.NAIR       PERIOD-TO-DATE ACCUMULATOR  *
      *                      MOVED IN-STORAGE, WAS A VSAM WORK FILE   *
      *   V1.08  2006-01-14  A.BUKOWSKI   SETTLEMENT DIRECTION FIELD  *
      *                      ADDED TO RATOUT FOR THE NEW GL FEED      *
      *   V2.00  2010-08-02  S.MARCHETTI  CARRIER TABLE WIDENED TO    *
      *                      150 ENTRIES - 100 WAS OVERFLOWING        *
      *   V2.03  2019-06-11  G.PRZYBYLSKI RESTART LOGIC REMOVED - THE *
      *                      PERIOD ACCUMULATOR CANNOT SAFELY RESUME  *
      *                      MID-CYCLE, SO A FAILED RUN IS RERUN COLD *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABRAT11.
       AUTHOR. TELCABS APPLICATIONS - RATING TEAM.
      *****************************************************************
      * RATES TYPE 08 (CD-RECIP-COMP) LOCAL TRAFFIC.  ENFORCES THE  *
      * FCC ISP REMAND ORDER MINUTE CAP - A CDR CAN STRADDLE IT.    *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RECIN ASSIGN TO UT-S-RECIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT CARRMST ASSIGN TO DA-CARRMST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CR-KEY
               FILE STATUS IS WS-FS-TABLE.
           SELECT RATOUT ASSIGN TO UT-S-RATOUT
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
      * RECIN - CLEANED USAGE, TYPE '08' ONLY (SEE P2110).         *
      *****************************************************************
       FD  RECIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       COPY CABSCDR.
      *****************************************************************
      * CARRMST - CARRIER MASTER, VSAM KSDS, RANDOM READ PER OCN.     *
      *****************************************************************
       FD  CARRMST
           LABEL RECORDS ARE STANDARD.
       COPY CABSCARR.
      *****************************************************************
      * RATOUT - RATED RECORD, LOCAL LAYOUT, MINUTE SPLIT + GL DIR.*
      *****************************************************************
       FD  RATOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       01  CABS-RATOUT-RECORD.
           05  RO-OCN                       PIC X(04).
           05  RO-BAN                       PIC X(13).
           05  RO-SEQ-NBR                   PIC 9(09) COMP-3.
           05  RO-RATE-STATUS               PIC X(01).
               88  RO-RATED                  VALUE 'R'.
               88  RO-SUSPENDED               VALUE 'S'.
           05  RO-DIRECTION                 PIC X(01).
               88  RO-PAYABLE                 VALUE 'P'.
               88  RO-RECEIVABLE               VALUE 'R'.
           05  RO-CYCLE-YYDDD               PIC 9(05).
           05  RO-RECORD-MIN                PIC S9(07)V9(02) COMP-3.
           05  RO-BILLABLE-MIN              PIC S9(07)V9(02) COMP-3.
           05  RO-CAPPED-MIN                PIC S9(07)V9(02) COMP-3.
           05  RO-RATE                      PIC S9(05)V9(05) COMP-3.
           05  RO-AMOUNT                    PIC S9(11)V9(05) COMP-3.
           05  RO-PTD-MINUTES               PIC S9(13)V9(02) COMP-3.
           05  RO-FILLER                    PIC X(133).
      *****************************************************************
      * SUSOUT - HARD REJECTS AND INFORMATIONAL CAP-EXCEEDED ENTRIES. *
      *****************************************************************
       FD  SUSOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 300 CHARACTERS.
       01  CABS-SUSPENSE-RECORD-FD.
           05  FD-SU-ERR-CODE               PIC X(04).
           05  FD-SU-ERR-SEVERITY           PIC X(01).
           05  FD-SU-DETECT-PGM             PIC X(08).
           05  FD-SU-DETECT-PARA            PIC X(30).
           05  FD-SU-RUN-ID                 PIC X(12).
           05  FD-SU-ORIG-RECORD            PIC X(200).
           05  FD-SU-FILLER                 PIC X(45).
      *****************************************************************
      * CTLOUT - RUN CONTROL / BALANCING RECORD.                      *
      *****************************************************************
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD               PIC X(180).
       WORKING-STORAGE SECTION.
       COPY CABSWRK.
       COPY CABSRT01.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                  PIC X(08) VALUE 'CABRAT11'.
           05  WS-PGM-VERSION               PIC X(05) VALUE 'V2.03'.
           05  WS-MAX-CARRIERS              PIC 9(03) VALUE 150.
       01  WS-PARM-CARD                     PIC X(80).
       01  WS-PARM-CARD-R1 REDEFINES WS-PARM-CARD.
           05  PC1-CYCLE-YYDDD              PIC 9(05).
           05  PC1-BILL-PERIOD              PIC 9(06).
           05  PC1-RUN-ID                   PIC X(12).
           05  PC1-FILLER                   PIC X(57).
      *****************************************************************
      * PER-CARRIER PTD MINUTE ACCUMULATOR - IN STORAGE, ONE STEP. *
      *****************************************************************
       01  WS-CARRIER-ACCUM-TABLE.
           05  WS-CA-CNT                    PIC 9(03) VALUE 0.
           05  WS-CA-ENTRY OCCURS 150 TIMES INDEXED BY WS-CA-X.
               10  WS-CA-OCN                 PIC X(04).
               10  WS-CA-PTD-MINUTES         PIC S9(13)V9(02) COMP-3
                                                            VALUE 0.
               10  WS-CA-CAP-HIT-SW          PIC X(01) VALUE 'N'.
                   88  WS-CA-CAP-HIT          VALUE 'Y'.
       01  WS-CARRIER-SEARCH-WORK.
           05  WS-CS-FOUND-SW               PIC X(01) VALUE 'N'.
               88  WS-CS-FOUND               VALUE 'Y'.
           05  WS-CS-TABLE-FULL-SW          PIC X(01) VALUE 'N'.
               88  WS-CS-TABLE-FULL          VALUE 'Y'.
      *****************************************************************
      * CAP-STRADDLE SPLIT WORK - REAL ARITHMETIC, NO SHORTCUTS.      *
      *****************************************************************
       01  WS-CAP-SPLIT-WORK.
           05  WS-RR-RECORD-MIN             PIC S9(07)V9(02) COMP-3
                                                            VALUE 0.
           05  WS-RR-ACC-BEFORE             PIC S9(13)V9(02) COMP-3
                                                            VALUE 0.
           05  WS-RR-HEADROOM               PIC S9(13)V9(02) COMP-3
                                                            VALUE 0.
           05  WS-RR-BILLABLE-MIN           PIC S9(07)V9(02) COMP-3
                                                            VALUE 0.
           05  WS-RR-CAPPED-MIN             PIC S9(07)V9(02) COMP-3
                                                            VALUE 0.
           05  WS-RR-AMOUNT                 PIC S9(11)V9(05) COMP-3
                                                            VALUE 0.
       01  WS-MIN-WORK                      PIC S9(13)V9(02).
       01  WS-MIN-WORK-R REDEFINES WS-MIN-WORK.
           05  WS-MW-WHOLE                  PIC 9(11).
           05  WS-MW-FRACTION               PIC 9(02).
       01  WS-AMT-DISPLAY                   PIC S9(11)V9(05).
       01  WS-AMT-DISPLAY-R REDEFINES WS-AMT-DISPLAY.
           05  WS-AD-WHOLE                  PIC 9(11).
           05  WS-AD-FRACTION               PIC 9(05).
       01  WS-DIRECTION-WORK.
           05  WS-DW-DIRECTION              PIC X(01) VALUE 'P'.
       01  WS-OCN-VALIDATE-WORK.
           05  WS-OV-VALID-SW               PIC X(01) VALUE 'Y'.
               88  WS-OV-VALID                VALUE 'Y'.
           05  WS-OV-RC                     PIC 9(04).
       01  WS-CALL-RC-AREA.
           05  WS-RC-PARMR                  PIC 9(04).
           05  WS-RC-OCNVL                  PIC 9(04).
           05  WS-RC-HASH                   PIC 9(04).
           05  WS-RC-ERRWR                  PIC 9(04).
       01  WS-HASH-CALL-WORK.
           05  WS-HC-MINUTES-IN             PIC S9(15)V9(02) COMP-3.
           05  WS-HC-AMOUNT-IN              PIC S9(13)V9(05) COMP-3.
           05  WS-HC-SEQ-IN                 PIC S9(17)       COMP-3.
       01  WS-MISC-COUNTERS.
           05  WS-MC-CAP-EXCEEDED-CNT       PIC S9(09) COMP-3 VALUE 0.
           05  WS-MC-RATE-NOT-FOUND-CNT     PIC S9(09) COMP-3 VALUE 0.
           05  WS-MC-CARRIER-NOT-FOUND-CNT  PIC S9(09) COMP-3 VALUE 0.
           05  WS-MC-INELIGIBLE-CNT         PIC S9(09) COMP-3 VALUE 0.
           05  WS-MC-TABLE-FULL-CNT         PIC S9(09) COMP-3 VALUE 0.
           05  WS-MC-PAYABLE-CNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-MC-RECEIVABLE-CNT         PIC S9(09) COMP-3 VALUE 0.
           05  WS-MC-CHECKPOINT-QUOT        PIC S9(09) COMP-3 VALUE 0.
           05  WS-MC-CHECKPOINT-REM         PIC S9(09) COMP-3 VALUE 0.
       01  WS-ABEND-WORK.
           05  WS-AB-PARA                   PIC X(30).
           05  WS-AB-REASON                 PIC X(60).
       PROCEDURE DIVISION.
       P0000-MAINLINE.
           PERFORM P1000-INIT THRU P1000-EXIT.
           PERFORM P2000-PROCESS THRU P2000-EXIT UNTIL WS-EOF.
           PERFORM P8000-CONTROL THRU P8000-EXIT.
           PERFORM P9000-TERM THRU P9000-EXIT.
           STOP RUN.
      *****************************************************************
      * S100-INITIALISATION SECTION                                   *
      *****************************************************************
       S100-INITIALISATION SECTION.
       P1000-INIT.
           PERFORM P1100-OPEN-FILES THRU P1100-EXIT.
           PERFORM P1200-READ-PARM THRU P1200-EXIT.
           PERFORM P1400-INIT-COUNTERS THRU P1400-EXIT.
           PERFORM P2100-READ-RECIN THRU P2100-EXIT.
       P1000-EXIT.
           EXIT.
       P1100-OPEN-FILES.
           OPEN INPUT RECIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RECIN OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN INPUT CARRMST.
           IF WS-FS-TABLE NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CARRMST OPEN FAILED - VSAM STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RATOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RATOUT OPEN FAILED - FILE STATUS BAD' TO
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
       P1100-EXIT.
           EXIT.
       P1200-READ-PARM.
           MOVE SPACES TO WS-PARM-CARD.
           ACCEPT WS-PARM-CARD FROM SYSIN.
           CALL 'CABPARMR' USING WS-PARM-CARD WS-RC-PARMR.
           MOVE PC1-CYCLE-YYDDD TO R1-CYCLE-YYDDD.
           MOVE PC1-BILL-PERIOD TO R1-BILL-PERIOD.
           MOVE PC1-RUN-ID TO R1-RUN-ID.
       P1200-EXIT.
           EXIT.
       P1400-INIT-COUNTERS.
           MOVE 0 TO WS-READ-CNT.
           MOVE 0 TO WS-WRITE-CNT.
           MOVE 0 TO WS-REJECT-CNT.
           MOVE 0 TO WS-SUMM-CNT.
           MOVE 0 TO WS-CFWD-CNT.
           MOVE 0 TO WS-ACC-MINUTES.
           MOVE 0 TO WS-ACC-AMOUNT.
           MOVE 0 TO WS-ACC-SEQ-HASH.
           MOVE 0 TO WS-ACC-OCN-HASH.
           MOVE 0 TO WS-MC-CAP-EXCEEDED-CNT.
           MOVE 0 TO WS-MC-RATE-NOT-FOUND-CNT.
           MOVE 0 TO WS-MC-CARRIER-NOT-FOUND-CNT.
           MOVE 0 TO WS-MC-INELIGIBLE-CNT.
           MOVE 0 TO WS-MC-TABLE-FULL-CNT.
           MOVE 0 TO WS-MC-PAYABLE-CNT.
           MOVE 0 TO WS-MC-RECEIVABLE-CNT.
       P1400-EXIT.
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
           PERFORM P2110-VALIDATE-REC-TYPE THRU P2110-EXIT.
           IF WS-ERROR-SW = 'Y'
               PERFORM P2800-REJECT-TO-SUSPENSE THRU P2800-EXIT
           ELSE
               PERFORM P2200-LOOKUP-CARRIER THRU P2200-EXIT.
           PERFORM P2100-READ-RECIN THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-RECIN.
           READ RECIN
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
               DISPLAY 'CABRAT11 - ' WS-READ-CNT ' RECORDS READ'.
       P2105-EXIT.
           EXIT.
      *****************************************************************
      * P2110-VALIDATE-REC-TYPE - BELT-AND-BRACES TYPE-08 CHECK.   *
      *****************************************************************
       P2110-VALIDATE-REC-TYPE.
           MOVE 'N' TO WS-ERROR-SW.
           MOVE SPACES TO SU-ERR-CODE.
           IF NOT CD-VALID-TYPE
               MOVE 'Y' TO WS-ERROR-SW
               MOVE EC-DATE-INVALID TO SU-ERR-CODE.
           IF WS-ERROR-SW = 'N' AND NOT CD-RECIP-COMP
               MOVE 'Y' TO WS-ERROR-SW
               MOVE EC-DATE-INVALID TO SU-ERR-CODE.
           IF WS-ERROR-SW = 'N' AND CD-FATAL
               MOVE 'Y' TO WS-ERROR-SW
               MOVE EC-DATE-INVALID TO SU-ERR-CODE.
       P2110-EXIT.
           EXIT.
      *****************************************************************
      * P2200-LOOKUP-CARRIER - RANDOM READ, NOT FOUND = REJECT.    *
      *****************************************************************
       P2200-LOOKUP-CARRIER.
           MOVE CD-OCN TO CR-OCN.
           CALL 'CABOCNVL' USING CD-OCN WS-OV-VALID-SW WS-OV-RC.
           IF NOT WS-OV-VALID
               MOVE 'Y' TO WS-ERROR-SW
               MOVE EC-OCN-UNKNOWN TO SU-ERR-CODE
               ADD 1 TO WS-MC-CARRIER-NOT-FOUND-CNT
               PERFORM P2800-REJECT-TO-SUSPENSE THRU P2800-EXIT
           ELSE
               READ CARRMST
                   INVALID KEY MOVE 'N' TO WS-CS-FOUND-SW
                   NOT INVALID KEY MOVE 'Y' TO WS-CS-FOUND-SW.
           IF WS-ERROR-SW = 'N'
               IF WS-CS-FOUND-SW = 'N'
                   MOVE 'Y' TO WS-ERROR-SW
                   MOVE EC-OCN-UNKNOWN TO SU-ERR-CODE
                   ADD 1 TO WS-MC-CARRIER-NOT-FOUND-CNT
                   PERFORM P2800-REJECT-TO-SUSPENSE THRU P2800-EXIT
               ELSE
                   PERFORM P2210-CHECK-CARRIER-STATUS THRU
                       P2210-EXIT.
       P2200-EXIT.
           EXIT.
       P2210-CHECK-CARRIER-STATUS.
           IF CR-ACTIVE-SW NOT = 'Y'
               MOVE 'Y' TO WS-ERROR-SW
               MOVE EC-TERM-EXPIRED TO SU-ERR-CODE
               ADD 1 TO WS-MC-CARRIER-NOT-FOUND-CNT
               PERFORM P2800-REJECT-TO-SUSPENSE THRU P2800-EXIT
           ELSE
               PERFORM P2300-CHECK-ELIGIBILITY THRU P2300-EXIT.
       P2210-EXIT.
           EXIT.
      *****************************************************************
      * P2300-CHECK-ELIGIBILITY - ELIG FLAG + NON-ZERO RATE REQ'D. *
      *****************************************************************
       P2300-CHECK-ELIGIBILITY.
           IF CR-RECIP-COMP-ELIG NOT = 'Y'
               MOVE 'Y' TO WS-ERROR-SW
               MOVE EC-RATE-NOT-FOUND TO SU-ERR-CODE
               ADD 1 TO WS-MC-INELIGIBLE-CNT
               PERFORM P2800-REJECT-TO-SUSPENSE THRU P2800-EXIT
           ELSE
               IF CR-RECIP-RATE = 0
                   MOVE 'Y' TO WS-ERROR-SW
                   MOVE EC-RATE-NOT-FOUND TO SU-ERR-CODE
                   ADD 1 TO WS-MC-RATE-NOT-FOUND-CNT
                   PERFORM P2800-REJECT-TO-SUSPENSE THRU P2800-EXIT
               ELSE
                   PERFORM P2400-RATE-RECORD THRU P2400-EXIT.
       P2300-EXIT.
           EXIT.
      *****************************************************************
      * P2400-RATE-RECORD - CAP SPLIT + CHARGE. MINUTES COME OFF   *
      * CD-VC-CHG-MIN - TYPE 08 IS LOCAL VOICE, CD-VOICE-DETAIL.   *
      *****************************************************************
       P2400-RATE-RECORD.
           PERFORM P2410-FIND-OR-ADD-CARRIER THRU P2410-EXIT.
           IF WS-CS-TABLE-FULL
               ADD 1 TO WS-MC-TABLE-FULL-CNT
               MOVE CD-VC-CHG-MIN TO WS-RR-RECORD-MIN
               MOVE CD-VC-CHG-MIN TO WS-RR-BILLABLE-MIN
               MOVE 0 TO WS-RR-CAPPED-MIN
           ELSE
               PERFORM P2500-APPLY-CAP-SPLIT THRU P2500-EXIT.
           PERFORM P2510-COMPUTE-CHARGE THRU P2510-EXIT.
           PERFORM P2520-SET-SETTLEMENT-DIRECTION THRU P2520-EXIT.
           PERFORM P2700-WRITE-RATOUT THRU P2700-EXIT.
           IF WS-RR-CAPPED-MIN > 0
               ADD 1 TO WS-MC-CAP-EXCEEDED-CNT
               PERFORM P2820-CAP-EXCEEDED-NOTE THRU P2820-EXIT.
       P2400-EXIT.
           EXIT.
      *****************************************************************
      * P2410-FIND-OR-ADD-CARRIER - LINEAR SEARCH, 150 ENTRIES.    *
      *****************************************************************
       P2410-FIND-OR-ADD-CARRIER.
           MOVE 'N' TO WS-CS-FOUND-SW.
           MOVE 'N' TO WS-CS-TABLE-FULL-SW.
           IF WS-CA-CNT > 0
               PERFORM P2415-SEARCH-ONE-SLOT THRU P2415-EXIT
                   VARYING WS-CA-X FROM 1 BY 1
                   UNTIL WS-CA-X > WS-CA-CNT OR WS-CS-FOUND.
           IF NOT WS-CS-FOUND
               IF WS-CA-CNT < WS-MAX-CARRIERS
                   ADD 1 TO WS-CA-CNT
                   SET WS-CA-X TO WS-CA-CNT
                   MOVE CD-OCN TO WS-CA-OCN (WS-CA-X)
                   MOVE 0 TO WS-CA-PTD-MINUTES (WS-CA-X)
                   MOVE 'N' TO WS-CA-CAP-HIT-SW (WS-CA-X)
               ELSE
                   MOVE 'Y' TO WS-CS-TABLE-FULL-SW.
       P2410-EXIT.
           EXIT.
       P2415-SEARCH-ONE-SLOT.
           IF WS-CA-OCN (WS-CA-X) = CD-OCN
               MOVE 'Y' TO WS-CS-FOUND-SW.
       P2415-EXIT.
           EXIT.
      *****************************************************************
      * P2500-APPLY-CAP-SPLIT - OVER-CAP / UNDER-CAP / STRADDLE.   *
      * ACCUMULATOR TAKES ALL MINUTES - THE CAP IS ON TRAFFIC.     *
      *****************************************************************
       P2500-APPLY-CAP-SPLIT.
           MOVE CD-VC-CHG-MIN TO WS-RR-RECORD-MIN.
           MOVE WS-CA-PTD-MINUTES (WS-CA-X) TO WS-RR-ACC-BEFORE.
           IF WS-RR-ACC-BEFORE NOT < CR-ISP-CAP-MOU
               MOVE 0 TO WS-RR-BILLABLE-MIN
               MOVE WS-RR-RECORD-MIN TO WS-RR-CAPPED-MIN
           ELSE
               COMPUTE WS-RR-HEADROOM =
                   CR-ISP-CAP-MOU - WS-RR-ACC-BEFORE
               IF WS-RR-RECORD-MIN NOT > WS-RR-HEADROOM
                   MOVE WS-RR-RECORD-MIN TO WS-RR-BILLABLE-MIN
                   MOVE 0 TO WS-RR-CAPPED-MIN
               ELSE
                   MOVE WS-RR-HEADROOM TO WS-RR-BILLABLE-MIN
                   COMPUTE WS-RR-CAPPED-MIN =
                       WS-RR-RECORD-MIN - WS-RR-HEADROOM.
           ADD WS-RR-RECORD-MIN TO WS-CA-PTD-MINUTES (WS-CA-X).
           IF WS-RR-CAPPED-MIN > 0
               MOVE 'Y' TO WS-CA-CAP-HIT-SW (WS-CA-X).
       P2500-EXIT.
           EXIT.
      *****************************************************************
      * P2510-COMPUTE-CHARGE - FIXED ROUNDED HALF-UP; CARRMST HAS  *
      * NO ROUND-RULE FIELD, UNLIKE THE RATE-TABLE PROGRAMS.       *
      *****************************************************************
       P2510-COMPUTE-CHARGE.
           COMPUTE WS-RR-AMOUNT ROUNDED =
               WS-RR-BILLABLE-MIN * CR-RECIP-RATE.
       P2510-EXIT.
           EXIT.
      *****************************************************************
      * P2520-SET-SETTLEMENT-DIRECTION - LOCAL CONVENTION: 'O' IN  *
      * CD-USAGE-TYPE = WE ORIGINATED = PAYABLE; ELSE RECEIVABLE.  *
      *****************************************************************
       P2520-SET-SETTLEMENT-DIRECTION.
           IF CD-USAGE-TYPE = 'O'
               MOVE 'P' TO WS-DW-DIRECTION
               ADD 1 TO WS-MC-PAYABLE-CNT
           ELSE
               MOVE 'R' TO WS-DW-DIRECTION
               ADD 1 TO WS-MC-RECEIVABLE-CNT.
       P2520-EXIT.
           EXIT.
       P2700-WRITE-RATOUT.
           MOVE SPACES TO CABS-RATOUT-RECORD.
           MOVE CD-OCN TO RO-OCN.
           MOVE CD-BAN TO RO-BAN.
           MOVE CD-SEQ-NBR TO RO-SEQ-NBR.
           MOVE 'R' TO RO-RATE-STATUS.
           MOVE WS-DW-DIRECTION TO RO-DIRECTION.
           MOVE R1-CYCLE-YYDDD TO RO-CYCLE-YYDDD.
           MOVE WS-RR-RECORD-MIN TO RO-RECORD-MIN.
           MOVE WS-RR-BILLABLE-MIN TO RO-BILLABLE-MIN.
           MOVE WS-RR-CAPPED-MIN TO RO-CAPPED-MIN.
           MOVE CR-RECIP-RATE TO RO-RATE.
           MOVE WS-RR-AMOUNT TO RO-AMOUNT.
           MOVE WS-CA-PTD-MINUTES (WS-CA-X) TO RO-PTD-MINUTES.
           WRITE CABS-RATOUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
           MOVE WS-RR-RECORD-MIN TO WS-HC-MINUTES-IN.
           MOVE WS-RR-AMOUNT TO WS-HC-AMOUNT-IN.
           MOVE CD-SEQ-NBR TO WS-HC-SEQ-IN.
           CALL 'CABHASH' USING WS-HC-MINUTES-IN WS-HC-AMOUNT-IN
               WS-HC-SEQ-IN WS-RC-HASH.
           ADD WS-HC-MINUTES-IN TO WS-ACC-MINUTES.
           ADD WS-HC-AMOUNT-IN TO WS-ACC-AMOUNT.
           ADD WS-HC-SEQ-IN TO WS-ACC-SEQ-HASH.
       P2700-EXIT.
           EXIT.
      *****************************************************************
      * P2800-REJECT-TO-SUSPENSE - HARD REJECT, NO RATOUT WRITE.   *
      *****************************************************************
       P2800-REJECT-TO-SUSPENSE.
           MOVE SPACES TO CABS-SUSPENSE-RECORD-FD.
           MOVE SU-ERR-CODE TO FD-SU-ERR-CODE.
           MOVE 'E' TO FD-SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO FD-SU-DETECT-PGM.
           MOVE 'P2000-PROCESS' TO FD-SU-DETECT-PARA.
           MOVE R1-RUN-ID TO FD-SU-RUN-ID.
           MOVE CABS-CDR-RECORD TO FD-SU-ORIG-RECORD.
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
      *****************************************************************
      * P2820-CAP-EXCEEDED-NOTE - INFORMATIONAL, NOT A REJECT.     *
      *****************************************************************
       P2820-CAP-EXCEEDED-NOTE.
           MOVE SPACES TO CABS-SUSPENSE-RECORD-FD.
           MOVE EC-RECIP-CAP-EXCEEDED TO FD-SU-ERR-CODE.
           MOVE 'W' TO FD-SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO FD-SU-DETECT-PGM.
           MOVE 'P2500-APPLY-CAP-SPLIT' TO FD-SU-DETECT-PARA.
           MOVE R1-RUN-ID TO FD-SU-RUN-ID.
           MOVE CABS-CDR-RECORD TO FD-SU-ORIG-RECORD.
           WRITE CABS-SUSPENSE-RECORD-FD.
       P2820-EXIT.
           EXIT.
      *****************************************************************
      * S800-CONTROL-BALANCE SECTION - THE MANDATORY CONTROL STEP.    *
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
           MOVE 0 TO CT-CARRIED-FWD.
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
           CLOSE RECIN.
           CLOSE CARRMST.
           CLOSE RATOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABRAT11 - RUN COMPLETE'.
           DISPLAY '  READ         = ' WS-READ-CNT.
           DISPLAY '  WRITTEN      = ' WS-WRITE-CNT.
           DISPLAY '  REJECTED     = ' WS-REJECT-CNT.
           DISPLAY '  CAP EXCEEDED = ' WS-MC-CAP-EXCEEDED-CNT.
           DISPLAY '  PAYABLE      = ' WS-MC-PAYABLE-CNT.
           DISPLAY '  RECEIVABLE   = ' WS-MC-RECEIVABLE-CNT.
       P9000-EXIT.
           EXIT.
