      *****************************************************************
      * CABRAT13 - OPERATOR SERVICES ACCESS RATING                    *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               RATIN   TELCABS.CABS.USAGE.VOICE(0)  CABSCDR    *
      *               RATEMST TELCABS.CABS.RATE (VSAM KSDS) CABSRATE  *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               RATOUT  TELCABS.CABS.RATED(+1)        (LOCAL)   *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +             *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN                                      *
      * REVISION HISTORY                                              *
      *   V1.00  1989-02-06  R.T.WHEELER  INITIAL RELEASE - BLV, EI,  *
      *                      OPERATOR-ASSISTED AND DA RATING          *
      *   V1.03  1991-07-15  D.OKONKWO    DAY/EVE/NIGHT-WEEKEND TIME  *
      *                      BAND TABLE ADDED PER TARIFF REVISION 4   *
      *   V1.05  1994-01-20  R.T.WHEELER  DYNAMIC CALL PATH ADDED FOR *
      *                      CARRIER-SPECIFIC OPERATOR SVC OVERRIDES  *
      *   V1.08  1998-09-01  J.M.CASTILLO OPERATOR SERVICES TARIFF    *
      *                      WITHDRAWN - R1-OPR-SVC-SW DEFAULTED OFF  *
      * PENDING REMOVAL AT NEXT RELEASE           *                   *
      *   V1.09  2002-03-11  P.NAIR       Y2K / CENTURY REVIEW - NO   *
      *                      IMPACT, RATING PATH NOT EXERCISED        *
      *   V1.11  2013-05-02  T.VANCE      RECOMPILE ONLY - LE V8      *
      *                      MIGRATION, NO SOURCE CHANGE REQUIRED     *
      *   V1.12  2019-11-07  G.PRZYBYLSKI RECOMPILE ONLY - CABSCTL    *
      *                      RESTART KEY WIDENED, UNUSED HERE         *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABRAT13.
       AUTHOR. TELCABS APPLICATIONS - RATING TEAM.
      *****************************************************************
      * RATES OPERATOR-HANDLED ACCESS TRAFFIC (BLV/EI/OACC/DA).    *
      * GATED BY R1-OPR-SVC-SW (CABSRT01) FROM THE RUN CONTROL.    *
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
           SELECT RATOUT ASSIGN TO UT-S-RATOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT CTLOUT ASSIGN TO UT-S-CTLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      *****************************************************************
      * RATIN - VOICE USAGE; OPR SVC CALLS CARRY USAGE-TYPE 'O'.   *
      *****************************************************************
       FD  RATIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       COPY CABSCDR.
      *****************************************************************
      * RATEMST - OPR SVC ROWS: TARIFF 'OPSV', BAND IN STATE-CD.   *
      *****************************************************************
       FD  RATEMST
           LABEL RECORDS ARE STANDARD.
       COPY CABSRATE.
      *****************************************************************
      * RATOUT - RATED OUTPUT.  WRITTEN BY P3900.                     *
      *****************************************************************
       FD  RATOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       01  CABS-RATOUT-RECORD.
           05  RO-OCN                        PIC X(04).
           05  RO-BAN                        PIC X(13).
           05  RO-SEQ-NBR                    PIC 9(09) COMP-3.
           05  RO-RATE-ELEM                  PIC X(06).
           05  RO-TIME-BAND                  PIC X(02).
           05  RO-AMOUNT                     PIC S9(11)V9(05) COMP-3.
           05  RO-FILLER                     PIC X(160).
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
           05  WS-PGM-NAME                   PIC X(08) VALUE 'CABRAT13'.
           05  WS-PGM-VERSION                PIC X(05) VALUE 'V1.12'.
       01  WS-PARM-CARD                      PIC X(80).
       01  WS-PARM-CARD-R1 REDEFINES WS-PARM-CARD.
           05  PC1-CYCLE-YYDDD               PIC 9(05).
           05  PC1-BILL-PERIOD                PIC 9(06).
           05  PC1-RUN-ID                     PIC X(12).
           05  PC1-FILLER                     PIC X(57).
      *****************************************************************
      * TIME-OF-DAY WORK - HHMM VIA REDEFINE, NO REFERENCE MOD.    *
      *****************************************************************
       01  WS-TIME-WORK                      PIC 9(06).
       01  WS-TIME-WORK-R REDEFINES WS-TIME-WORK.
           05  WS-TIME-HH                    PIC 9(02).
           05  WS-TIME-MM                    PIC 9(02).
           05  WS-TIME-SS                    PIC 9(02).
       01  WS-TIME-HHMM                      PIC 9(04).
      *****************************************************************
      * TIME-BAND TABLE - DAY/EVE/NW, EACH CARRYING A CALL SUFFIX. *
      *****************************************************************
       01  WS-TIME-BAND-TABLE.
           05  WS-TB-CNT                     PIC 9(01) VALUE 3.
           05  WS-TB-ENTRY OCCURS 3 TIMES INDEXED BY WS-TB-X.
               10  WS-TB-CODE                 PIC X(02).
               10  WS-TB-FROM-HHMM             PIC 9(04).
               10  WS-TB-THRU-HHMM             PIC 9(04).
               10  WS-TB-MODULE-SFX            PIC X(02).
       01  WS-TIME-BAND-SEARCH-WORK.
           05  WS-TBS-FOUND-SW               PIC X(01) VALUE 'N'.
               88  WS-TBS-FOUND                 VALUE 'Y'.
       01  WS-OS-DISPATCH-WORK.
           05  WS-OS-DISPATCH-IDX            PIC 9(01) VALUE 0.
           05  WS-OS-FOUND-SW                PIC X(01) VALUE 'N'.
               88  WS-OS-FOUND                  VALUE 'Y'.
           05  WS-OS-PERCALL-RATE            PIC S9(05)V9(05) COMP-3
                                                            VALUE 0.
           05  WS-OS-PERMIN-RATE             PIC S9(05)V9(05) COMP-3
                                                            VALUE 0.
           05  WS-OS-AMOUNT                  PIC S9(11)V9(05) COMP-3
                                                            VALUE 0.
       01  WS-OS-CALL-TARGET.
           05  WS-OS-TGT-PREFIX              PIC X(06).
           05  WS-OS-TGT-SUFFIX              PIC X(02).
       01  WS-OS-CALL-RESULT.
           05  WS-OS-RC                      PIC 9(04).
       01  WS-CALL-RC-AREA.
           05  WS-RC-PARMR                   PIC 9(04).
           05  WS-RC-HASH                    PIC 9(04).
       01  WS-HASH-CALL-WORK.
           05  WS-HC-AMOUNT-IN               PIC S9(13)V9(05) COMP-3.
           05  WS-HC-SEQ-IN                  PIC S9(17)       COMP-3.
       01  WS-MISC-COUNTERS.
           05  WS-MC-INACTIVE-CNT            PIC S9(09) COMP-3 VALUE 0.
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
      * S100-INITIALISATION SECTION                                   *
      *****************************************************************
       S100-INITIALISATION SECTION.
       P1000-INIT.
           PERFORM P1100-OPEN-FILES THRU P1100-EXIT.
           PERFORM P1200-READ-PARM THRU P1200-EXIT.
           PERFORM P1300-LOAD-TIME-BAND-TABLE THRU P1300-EXIT.
           PERFORM P1400-INIT-COUNTERS THRU P1400-EXIT.
           PERFORM P2100-READ-RATIN THRU P2100-EXIT.
       P1000-EXIT.
           EXIT.
       P1100-OPEN-FILES.
           MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA.
           OPEN INPUT RATIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'RATIN OPEN FAILED' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN INPUT RATEMST.
           IF WS-FS-TABLE NOT = '00'
               MOVE 'RATEMST OPEN FAILED' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RATOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'RATOUT OPEN FAILED' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'CTLOUT OPEN FAILED' TO WS-AB-REASON
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
      *****************************************************************
      * P1300-LOAD-TIME-BAND-TABLE - LOADED REGARDLESS OF THE SW.  *
      *****************************************************************
       P1300-LOAD-TIME-BAND-TABLE.
           MOVE 'DA' TO WS-TB-CODE (1).
           MOVE 0800 TO WS-TB-FROM-HHMM (1).
           MOVE 1659 TO WS-TB-THRU-HHMM (1).
           MOVE 'OS' TO WS-TB-MODULE-SFX (1).
           MOVE 'EV' TO WS-TB-CODE (2).
           MOVE 1700 TO WS-TB-FROM-HHMM (2).
           MOVE 2259 TO WS-TB-THRU-HHMM (2).
           MOVE 'OS' TO WS-TB-MODULE-SFX (2).
           MOVE 'NW' TO WS-TB-CODE (3).
           MOVE 2300 TO WS-TB-FROM-HHMM (3).
           MOVE 0759 TO WS-TB-THRU-HHMM (3).
           MOVE 'OS' TO WS-TB-MODULE-SFX (3).
       P1300-EXIT.
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
           MOVE 0 TO WS-MC-INACTIVE-CNT.
       P1400-EXIT.
           EXIT.
       P9900-FATAL-OPEN.
           MOVE 'B037' TO CT-ABEND-CD.
           CALL 'CABABEND' USING WS-AB-PARA WS-AB-REASON
               CT-ABEND-CD.
       P9900-EXIT.
           EXIT.
      *****************************************************************
      * S200-MAIN-PROCESS SECTION - DISPATCHES TO P3000.           *
      *****************************************************************
       S200-MAIN-PROCESS SECTION.
       P2000-PROCESS.
           IF R1-OPR-SVC-ACTIVE
               PERFORM P3000-RATE-OPERATOR-SERVICES THRU P3000-EXIT
           ELSE
               ADD 1 TO WS-MC-INACTIVE-CNT.
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
               DISPLAY 'CABRAT13 - ' WS-READ-CNT ' RECORDS READ'.
       P2105-EXIT.
           EXIT.
      *****************************************************************
      * S300-OPERATOR-SERVICES SECTION - TOD BANDED RATING.        *
      * INTACT, CORRECT-LOOKING, REACHABLE ONLY IF THE SW FIRES.   *
      *****************************************************************
       S300-OPERATOR-SERVICES SECTION.
       P3000-RATE-OPERATOR-SERVICES.
           PERFORM P3100-DETERMINE-TIME-BAND THRU P3100-EXIT.
           PERFORM P3200-DISPATCH-ELEMENT THRU P3200-EXIT.
           PERFORM P3900-WRITE-RATOUT THRU P3900-EXIT.
       P3000-EXIT.
           EXIT.
      *****************************************************************
      * P3100-DETERMINE-TIME-BAND - FROM CD-CONN-HHMMSS, NO REF MOD*
      *****************************************************************
       P3100-DETERMINE-TIME-BAND.
           MOVE CD-CONN-HHMMSS TO WS-TIME-WORK.
           COMPUTE WS-TIME-HHMM = WS-TIME-HH * 100 + WS-TIME-MM.
           MOVE 'N' TO WS-TBS-FOUND-SW.
           PERFORM P3110-CHECK-ONE-BAND THRU P3110-EXIT
               VARYING WS-TB-X FROM 1 BY 1
               UNTIL WS-TB-X > WS-TB-CNT OR WS-TBS-FOUND.
           IF NOT WS-TBS-FOUND
               SET WS-TB-X TO 3.
       P3100-EXIT.
           EXIT.
      *****************************************************************
      * P3110-CHECK-ONE-BAND - NW WRAPS MIDNIGHT, NEEDS THE OR LEG.*
      *****************************************************************
       P3110-CHECK-ONE-BAND.
           IF WS-TB-CODE (WS-TB-X) = 'NW'
               IF WS-TIME-HHMM NOT < WS-TB-FROM-HHMM (WS-TB-X) OR
                       WS-TIME-HHMM NOT > WS-TB-THRU-HHMM (WS-TB-X)
                   MOVE 'Y' TO WS-TBS-FOUND-SW
           ELSE
               IF WS-TIME-HHMM NOT < WS-TB-FROM-HHMM (WS-TB-X) AND
                       WS-TIME-HHMM NOT > WS-TB-THRU-HHMM (WS-TB-X)
                   MOVE 'Y' TO WS-TBS-FOUND-SW.
       P3110-EXIT.
           EXIT.
      *****************************************************************
      * P3200-DISPATCH-ELEMENT - GO TO DEPENDING ON, 5 ELEMENTS.   *
      *****************************************************************
       P3200-DISPATCH-ELEMENT.
           MOVE 1 TO WS-OS-DISPATCH-IDX.
           IF CD-RATE-ELEM = 'EMRINT'
               MOVE 2 TO WS-OS-DISPATCH-IDX.
           IF CD-RATE-ELEM = 'OPRCC '
               MOVE 3 TO WS-OS-DISPATCH-IDX.
           IF CD-RATE-ELEM = 'DAACC '
               MOVE 4 TO WS-OS-DISPATCH-IDX.
           IF CD-RATE-ELEM = 'DACC  '
               MOVE 5 TO WS-OS-DISPATCH-IDX.
           GO TO P3210-RATE-BLV P3220-RATE-EMRINT P3230-RATE-OPRCC
               P3240-RATE-DAACC P3250-RATE-DACC
               DEPENDING ON WS-OS-DISPATCH-IDX.
       P3210-RATE-BLV.
           PERFORM P3400-LOOKUP-RATE THRU P3400-EXIT.
           MOVE WS-OS-PERCALL-RATE TO WS-OS-AMOUNT.
           GO TO P3200-EXIT.
       P3220-RATE-EMRINT.
           PERFORM P3400-LOOKUP-RATE THRU P3400-EXIT.
           MOVE WS-OS-PERCALL-RATE TO WS-OS-AMOUNT.
           GO TO P3200-EXIT.
       P3230-RATE-OPRCC.
           PERFORM P3400-LOOKUP-RATE THRU P3400-EXIT.
           COMPUTE WS-OS-AMOUNT = WS-OS-PERCALL-RATE +
               (CD-VC-CHG-MIN * WS-OS-PERMIN-RATE).
           PERFORM P3300-CALL-OPR-MODULE THRU P3300-EXIT.
           GO TO P3200-EXIT.
       P3240-RATE-DAACC.
           PERFORM P3400-LOOKUP-RATE THRU P3400-EXIT.
           MOVE WS-OS-PERCALL-RATE TO WS-OS-AMOUNT.
           GO TO P3200-EXIT.
       P3250-RATE-DACC.
           PERFORM P3400-LOOKUP-RATE THRU P3400-EXIT.
           COMPUTE WS-OS-AMOUNT = WS-OS-PERCALL-RATE +
               (CD-VC-CHG-MIN * WS-OS-PERMIN-RATE).
           PERFORM P3300-CALL-OPR-MODULE THRU P3300-EXIT.
       P3200-EXIT.
           EXIT.
      *****************************************************************
      * P3300-CALL-OPR-MODULE - DYNAMIC CALL TO THE TOD MODULE.    *
      *****************************************************************
       P3300-CALL-OPR-MODULE.
           MOVE R1-CALL-PREFIX TO WS-OS-TGT-PREFIX.
           MOVE WS-TB-MODULE-SFX (WS-TB-X) TO WS-OS-TGT-SUFFIX.
           CALL WS-OS-CALL-TARGET USING CABS-CDR-RECORD WS-OS-AMOUNT
               WS-OS-RC.
       P3300-EXIT.
           EXIT.
      *****************************************************************
      * P3400-LOOKUP-RATE - RATEMST ROW FOR ELEMENT/TIME BAND.     *
      *****************************************************************
       P3400-LOOKUP-RATE.
           MOVE 'OPSV' TO RT-TARIFF-CD.
           MOVE CD-RATE-ELEM TO RT-RATE-ELEM.
           MOVE 'X' TO RT-JURIS-CD.
           MOVE WS-TB-CODE (WS-TB-X) TO RT-STATE-CD.
           MOVE R1-CYCLE-YYDDD TO RT-EFF-YYDDD.
           READ RATEMST
               INVALID KEY MOVE 'N' TO WS-OS-FOUND-SW
               NOT INVALID KEY MOVE 'Y' TO WS-OS-FOUND-SW.
           IF WS-OS-FOUND
               MOVE RT-INITIAL-RATE TO WS-OS-PERCALL-RATE
               MOVE RT-ADDL-RATE TO WS-OS-PERMIN-RATE
           ELSE
               MOVE 0 TO WS-OS-PERCALL-RATE
               MOVE 0 TO WS-OS-PERMIN-RATE.
       P3400-EXIT.
           EXIT.
       P3900-WRITE-RATOUT.
           MOVE SPACES TO CABS-RATOUT-RECORD.
           MOVE CD-OCN TO RO-OCN.
           MOVE CD-BAN TO RO-BAN.
           MOVE CD-SEQ-NBR TO RO-SEQ-NBR.
           MOVE CD-RATE-ELEM TO RO-RATE-ELEM.
           MOVE WS-TB-CODE (WS-TB-X) TO RO-TIME-BAND.
           MOVE WS-OS-AMOUNT TO RO-AMOUNT.
           WRITE CABS-RATOUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
           MOVE WS-OS-AMOUNT TO WS-HC-AMOUNT-IN.
           MOVE CD-SEQ-NBR TO WS-HC-SEQ-IN.
           CALL 'CABHASH' USING WS-HC-AMOUNT-IN WS-HC-SEQ-IN
               WS-RC-HASH.
           ADD WS-HC-AMOUNT-IN TO WS-ACC-AMOUNT.
           ADD WS-HC-SEQ-IN TO WS-ACC-SEQ-HASH.
       P3900-EXIT.
           EXIT.
      *****************************************************************
      * S800-CONTROL-BALANCE SECTION - CT-WRITTEN STAYS ZERO.      *
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
           MOVE 0 TO CT-REJECTED.
           MOVE 0 TO CT-SUMMARISED.
           MOVE WS-MC-INACTIVE-CNT TO CT-CARRIED-FWD.
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
           CLOSE RATOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABRAT13 - RUN COMPLETE'.
           DISPLAY '  READ        = ' WS-READ-CNT.
           DISPLAY '  WRITTEN     = ' WS-WRITE-CNT.
           DISPLAY '  INACTIVE    = ' WS-MC-INACTIVE-CNT.
       P9000-EXIT.
           EXIT.
