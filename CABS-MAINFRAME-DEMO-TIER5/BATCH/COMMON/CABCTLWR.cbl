      *****************************************************************
      * CABCTLWR - AUXILIARY CONTROL RECORD WRITER                    *
      * APPLICATION : CABS                                            *
      * INVOKED BY  : CALL FROM THE BATCH UTILITY FAMILY              *
      * INPUTS      : LK-CW-TAG   X(08) STEP TAG AND STAGED VALUE     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               CTLAUX  TELCABS.CABS.CTLAUX          CABSCTL    *
      *               LK-CW-RC    9(04) RETURN CODE                   *
      * CONTROL     : NONE - SUBPROGRAMS DO NOT WRITE CTLOUT,         *
      *               CABS-STD-041                                    *
      * BALANCE     : THE AUXILIARY RECORD CARRIES ITS OWN TEST OF    *
      *               CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - CTLAUX IS OPENED OUTPUT EACH RUN   *
      * REVISION HISTORY                                              *
      *   V1.00  1993-01-19  B.R.HALVORSEN INITIAL RELEASE            *
      *   V1.01  1995-06-30  P.NAIR        FIVE COUNTERS SELECTED BY  *
      *                      THE THIRD BYTE OF THE TAG                *
      *   V1.02  1998-04-07  D.OKONKWO     BALANCING TEST ADDED AND   *
      *                      CT-BAL-IND SET                           *
      *   V1.03  2002-09-12  L.FERREIRA    RESET ACTION ADDED FOR THE *
      *                      MULTI CYCLE UTILITIES                    *
      *   V1.04  2009-05-21  M.HAAS        STEP SEQUENCE STEPPED BY   *
      *                      THE MODULE INSTEAD OF THE CALLER         *
      *   V1.05  2016-02-08  T.YAMASHITA   BLOCK SIZE SYSTEM          *
      *                      DETERMINED                               *
      *   V1.06  2019-07-15  J.CALLAGHAN   TALLY AND CLOSE FROM THE   *
      *                      SENTINEL CALL                            *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABCTLWR.
       AUTHOR. TELCABS APPLICATIONS - COMMON SUBROUTINE GROUP.
      *****************************************************************
      * WRITES A SUPPLEMENTARY CONTROL RECORD TO DD CTLAUX. THIS IS   *
      * AN ADDITIONAL LOG AND NOT A REPLACEMENT FOR ANYTHING. THE     *
      * CALLING PROGRAM'S P8000-CONTROL STILL BUILDS AND WRITES ITS   *
      * OWN CTLOUT RECORD EXACTLY AS IT ALWAYS HAS, AND THE CALLER'S  *
      * BALANCING EQUATION IS UNAFFECTED BY ANYTHING THIS MODULE      *
      * DOES.                                                         *
      * THE INTERFACE IS EIGHT BYTES. CALLERS THAT STAGE A LONGER     *
      * FIELD PRESENT ITS LEADING EIGHT BYTES.                        *
      * TAG LAYOUT                                                    *
      *   BYTES 1-2  ACTION   OP OPEN THE LOG                         *
      *                       AC ACCUMULATE                           *
      *                       WR WRITE THE ACCUMULATED RECORD         *
      *                       RS RESET THE ACCUMULATORS               *
      *   BYTE  3    COUNTER  R READ  W WRITTEN  X REJECTED           *
      *                       S SUMMARISED  C CARRIED FORWARD         *
      *   BYTES 4-8  VALUE    FIVE DISPLAY DIGITS                     *
      * ANY OTHER ACTION IS TREATED AS AC AND STEPS A COUNTER. ON AN  *
      * OP CALL BYTES 3-8 ARE THE RUN IDENTIFIER AND NOT A VALUE.     *
      * THE ACCUMULATORS, THE STEP SEQUENCE AND THE OPEN SWITCH ARE   *
      * HELD IN WORKING-STORAGE AND SURVIVE FROM ONE CALL TO THE NEXT *
      * FOR THE LIFE OF THE RUN UNIT. THAT IS WHAT MAKES A RUN OF AC  *
      * CALLS FOLLOWED BY ONE WR CALL WORK.                           *
      * RETURN CODES                                                  *
      *   0000  ACTION TAKEN                                          *
      *   0004  ACTION DEFAULTED TO ACCUMULATE                        *
      *   0008  VALUE NOT NUMERIC, TREATED AS ZERO                    *
      *   0012  RECORD WRITTEN AND IT BALANCED                        *
      *   0016  RECORD WRITTEN AND IT DID NOT BALANCE                 *
      *   0020  CTLAUX COULD NOT BE OPENED, NOTHING WRITTEN           *
      *   0024  TALLIES DISPLAYED AND THE LOG CLOSED                  *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CTLAUX ASSIGN TO UT-S-CTLAUX
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CTLAUX.
       DATA DIVISION.
       FILE SECTION.
      * CTLAUX - FB 180, BLOCKED BY THE SYSTEM.
       FD  CTLAUX
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLAUX-RECORD          PIC X(180).
       WORKING-STORAGE SECTION.
      * THE STANDARD CONTROL RECORD LAYOUT.
       COPY CABSCTL.
       01  WS-CW-CONSTANTS.
           05  WS-CW-PGM-NAME    PIC X(08) VALUE 'CABCTLWR'.
           05  WS-CW-VERSION     PIC X(05) VALUE 'V1.06'.
           05  WS-CW-SENTINEL    PIC X(08) VALUE '*END    '.
       01  WS-FS-CTLAUX          PIC X(02) VALUE '00'.
      * THE TAG IS TAKEN APART THROUGH REDEFINES. NOTHING IN THIS
      * MODULE ADDRESSES A BYTE BY POSITION.
       01  WS-CW-TAG-AREA.
           05  WS-CW-TAG         PIC X(08).
       01  WS-CW-TAG-R REDEFINES WS-CW-TAG-AREA.
           05  WS-CW-ACTION      PIC X(02).
           05  WS-CW-CTR-ID      PIC X(01).
           05  WS-CW-VALUE       PIC X(05).
       01  WS-CW-TAG-N REDEFINES WS-CW-TAG-AREA.
           05  FILLER            PIC X(03).
           05  WS-CW-VALUE-N     PIC 9(05).
       01  WS-CW-TAG-S REDEFINES WS-CW-TAG-AREA.
           05  FILLER            PIC X(02).
           05  WS-CW-TAG-SUFFIX  PIC X(06).
      * THE ACCUMULATORS. THESE ARE THE MODULE'S MEMORY BETWEEN CALLS
      * AND ARE CLEARED ONLY BY AN RS ACTION.
       01  WS-CW-ACCUM-AREA.
           05  WS-CW-CT-READ     PIC S9(11) COMP-3 VALUE 0.
           05  WS-CW-CT-WRIT     PIC S9(11) COMP-3 VALUE 0.
           05  WS-CW-CT-REJ      PIC S9(11) COMP-3 VALUE 0.
           05  WS-CW-CT-SUM      PIC S9(11) COMP-3 VALUE 0.
           05  WS-CW-CT-CFWD     PIC S9(11) COMP-3 VALUE 0.
           05  WS-CW-VALUE-W     PIC S9(05) COMP-3 VALUE 0.
           05  WS-CW-STAGE-RUN   PIC X(06) VALUE SPACES.
           05  WS-CW-STEP-SEQ    PIC S9(03) COMP-3 VALUE 0.
           05  WS-CW-CYCLE-YYDDD PIC 9(05) VALUE 0.
           05  WS-CW-BILL-PERIOD PIC 9(06) VALUE 0.
       01  WS-CW-COUNT-AREA.
           05  WS-CW-CALL-CNT    PIC S9(09) COMP-3 VALUE 0.
           05  WS-CW-OPEN-CNT    PIC S9(09) COMP-3 VALUE 0.
           05  WS-CW-ACC-CNT     PIC S9(09) COMP-3 VALUE 0.
           05  WS-CW-WRITE-CNT   PIC S9(09) COMP-3 VALUE 0.
           05  WS-CW-RESET-CNT   PIC S9(09) COMP-3 VALUE 0.
           05  WS-CW-DEFAULT-CNT PIC S9(09) COMP-3 VALUE 0.
           05  WS-CW-NONNUM-CNT  PIC S9(09) COMP-3 VALUE 0.
           05  WS-CW-UNKCTR-CNT  PIC S9(09) COMP-3 VALUE 0.
           05  WS-CW-OOB-CNT     PIC S9(09) COMP-3 VALUE 0.
           05  WS-CW-IOERR-CNT   PIC S9(09) COMP-3 VALUE 0.
           05  WS-CW-TALLY-CNT   PIC S9(09) COMP-3 VALUE 0.
       01  WS-CW-SWITCH-AREA.
           05  WS-CW-OPEN-SW     PIC X(01) VALUE 'N'.
           05  WS-CW-FAIL-SW     PIC X(01) VALUE 'N'.
           05  WS-CW-DEFAULT-SW  PIC X(01) VALUE 'N'.
           05  WS-CW-CNT-ED      PIC ZZZ,ZZZ,ZZ9.
           05  WS-CW-ACC-ED      PIC ZZZ,ZZZ,ZZZ,ZZ9-.
       LINKAGE SECTION.
       01  LK-CW-TAG                   PIC X(08).
       01  LK-CW-RC                    PIC 9(04).
       PROCEDURE DIVISION USING LK-CW-TAG LK-CW-RC.
      * A NESTED IF CHAIN SELECTS THE ACTION. THE FALL THROUGH LIMB
      * IS ACCUMULATE, WHICH IS WHAT THE OLDEST CALLERS RELIED ON
      * WHEN THEY PASSED A BARE STEP NAME.
       P0000-CONTROL-WRITE.
           ADD 1 TO WS-CW-CALL-CNT.
           MOVE 0 TO LK-CW-RC.
           MOVE LK-CW-TAG TO WS-CW-TAG.
           IF WS-CW-TAG = WS-CW-SENTINEL
               PERFORM P7000-TALLY THRU P7000-EXIT
               MOVE 24 TO LK-CW-RC
               GO TO P0000-EXIT.
           MOVE 'N' TO WS-CW-DEFAULT-SW.
           IF WS-CW-ACTION = 'OP'
               PERFORM P1000-OPEN-LOG THRU P1000-EXIT
           ELSE IF WS-CW-ACTION = 'AC'
               PERFORM P2000-ACCUMULATE THRU P2000-EXIT
           ELSE IF WS-CW-ACTION = 'WR'
               PERFORM P3000-WRITE-AUX THRU P3000-EXIT
           ELSE IF WS-CW-ACTION = 'RS'
               PERFORM P4000-RESET THRU P4000-EXIT
           ELSE
               MOVE 'Y' TO WS-CW-DEFAULT-SW
               ADD 1 TO WS-CW-DEFAULT-CNT
               PERFORM P2000-ACCUMULATE THRU P2000-EXIT.
           IF WS-CW-DEFAULT-SW = 'Y' AND LK-CW-RC = 0
               MOVE 4 TO LK-CW-RC.
       P0000-EXIT.
           GOBACK.
      * S100-OPEN SECTION - THE FIRST OP CALL OF THE RUN UNIT OPENS
      * THE LOG AND STAGES THE RUN IDENTIFIER. LATER OP CALLS ARE
      * ACCEPTED AND LEAVE THE STAGED VALUES ALONE.
       S100-OPEN SECTION.
       P1000-OPEN-LOG.
           ADD 1 TO WS-CW-OPEN-CNT.
           IF WS-CW-OPEN-SW = 'Y'
               MOVE 0 TO LK-CW-RC
               GO TO P1000-EXIT.
           IF WS-CW-FAIL-SW = 'Y'
               MOVE 20 TO LK-CW-RC
               GO TO P1000-EXIT.
           MOVE WS-CW-TAG-SUFFIX TO WS-CW-STAGE-RUN.
           MOVE 0 TO WS-CW-STEP-SEQ.
           OPEN OUTPUT CTLAUX.
           IF WS-FS-CTLAUX NOT = '00'
               MOVE 'Y' TO WS-CW-FAIL-SW
               ADD 1 TO WS-CW-IOERR-CNT
               MOVE 20 TO LK-CW-RC
               GO TO P1000-EXIT.
           MOVE 'Y' TO WS-CW-OPEN-SW.
           MOVE 0 TO LK-CW-RC.
       P1000-EXIT.
           EXIT.
      * S200-ACCUMULATE SECTION - THE VALUE IS CONVERTED THROUGH THE
      * NUMERIC REDEFINES OF THE TAG. A VALUE THAT IS NOT ALL DIGITS
      * IS TAKEN AS ZERO AND THE CALL IS STILL COUNTED. A COUNTER
      * LETTER THAT IS NOT ONE OF THE FIVE IS CARRIED ON THE READ
      * ACCUMULATOR AND COUNTED SEPARATELY FOR THE TALLY.
       S200-ACCUMULATE SECTION.
       P2000-ACCUMULATE.
           ADD 1 TO WS-CW-ACC-CNT.
           MOVE 0 TO WS-CW-VALUE-W.
           IF WS-CW-VALUE IS NUMERIC
               MOVE WS-CW-VALUE-N TO WS-CW-VALUE-W
               MOVE 0 TO LK-CW-RC
           ELSE
               ADD 1 TO WS-CW-NONNUM-CNT
               MOVE 8 TO LK-CW-RC.
           IF WS-CW-CTR-ID = 'R'
               ADD WS-CW-VALUE-W TO WS-CW-CT-READ
           ELSE IF WS-CW-CTR-ID = 'W'
               ADD WS-CW-VALUE-W TO WS-CW-CT-WRIT
           ELSE IF WS-CW-CTR-ID = 'X'
               ADD WS-CW-VALUE-W TO WS-CW-CT-REJ
           ELSE IF WS-CW-CTR-ID = 'S'
               ADD WS-CW-VALUE-W TO WS-CW-CT-SUM
           ELSE IF WS-CW-CTR-ID = 'C'
               ADD WS-CW-VALUE-W TO WS-CW-CT-CFWD
           ELSE
               ADD WS-CW-VALUE-W TO WS-CW-CT-READ
               ADD 1 TO WS-CW-UNKCTR-CNT.
       P2000-EXIT.
           EXIT.
      * S300-WRITE SECTION - THE FOUR HASH TOTALS ARE LEFT AT ZERO.
      * THIS MODULE HAS NEVER BEEN GIVEN THE HASH ACCUMULATORS, SO
      * THE AUXILIARY RECORD CARRIES COUNTS ONLY. AN AUXILIARY
      * RECORD THAT DOES NOT BALANCE IS WRITTEN WITH CT-BAL-IND SET
      * TO O AND THE STEP CARRIES ON.
       S300-WRITE SECTION.
       P3000-WRITE-AUX.
           IF WS-CW-OPEN-SW NOT = 'Y'
               MOVE 20 TO LK-CW-RC
               GO TO P3000-EXIT.
           ADD 1 TO WS-CW-STEP-SEQ.
           MOVE SPACES TO CT-RUN-ID.
           MOVE WS-CW-STAGE-RUN TO CT-RUN-ID.
           MOVE WS-CW-PGM-NAME TO CT-PROCESS-ID.
           MOVE WS-CW-STEP-SEQ TO CT-STEP-SEQ.
           MOVE WS-CW-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE WS-CW-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE SPACES TO CT-JOBNAME.
           MOVE WS-CW-STAGE-RUN TO CT-STEPNAME.
           MOVE WS-CW-CT-READ TO CT-READ.
           MOVE WS-CW-CT-WRIT TO CT-WRITTEN.
           MOVE WS-CW-CT-REJ TO CT-REJECTED.
           MOVE WS-CW-CT-SUM TO CT-SUMMARISED.
           MOVE WS-CW-CT-CFWD TO CT-CARRIED-FWD.
           MOVE 0 TO CT-HASH-MINUTES CT-HASH-AMOUNT.
           MOVE 0 TO CT-HASH-SEQ CT-HASH-OCN.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-ABEND-CD CT-RESTART-KEY CT-FILLER.
           IF CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARISED +
                       CT-CARRIED-FWD
               MOVE 'B' TO CT-BAL-IND
               MOVE 12 TO LK-CW-RC
           ELSE
               MOVE 'O' TO CT-BAL-IND
               MOVE 16 TO LK-CW-RC
               ADD 1 TO WS-CW-OOB-CNT.
           MOVE CABS-CONTROL-RECORD TO CABS-CTLAUX-RECORD.
           WRITE CABS-CTLAUX-RECORD.
           IF WS-FS-CTLAUX NOT = '00'
               ADD 1 TO WS-CW-IOERR-CNT
               MOVE 20 TO LK-CW-RC
           ELSE
               ADD 1 TO WS-CW-WRITE-CNT.
       P3000-EXIT.
           EXIT.
      * S400-RESET SECTION - THE STEP SEQUENCE IS NOT RESET. IT
      * NUMBERS THE AUXILIARY RECORDS ACROSS THE WHOLE RUN UNIT.
       S400-RESET SECTION.
       P4000-RESET.
           ADD 1 TO WS-CW-RESET-CNT.
           MOVE 0 TO WS-CW-CT-READ WS-CW-CT-WRIT WS-CW-CT-REJ.
           MOVE 0 TO WS-CW-CT-SUM WS-CW-CT-CFWD.
           MOVE 0 TO LK-CW-RC.
       P4000-EXIT.
           EXIT.
      * S700-TALLY SECTION
       S700-TALLY SECTION.
       P7000-TALLY.
           ADD 1 TO WS-CW-TALLY-CNT.
           MOVE WS-CW-CALL-CNT TO WS-CW-CNT-ED.
           DISPLAY 'CABCTLWR ' WS-CW-VERSION ' - CTLAUX TALLY'.
           DISPLAY '  CALLS          = ' WS-CW-CNT-ED.
           DISPLAY '  OPEN CALLS     = ' WS-CW-OPEN-CNT.
           DISPLAY '  ACCUMULATES    = ' WS-CW-ACC-CNT.
           DISPLAY '  RECORDS PUT    = ' WS-CW-WRITE-CNT.
           DISPLAY '  RESETS         = ' WS-CW-RESET-CNT.
           DISPLAY '  DEFAULTED      = ' WS-CW-DEFAULT-CNT.
           DISPLAY '  VALUE NOT NUM  = ' WS-CW-NONNUM-CNT.
           DISPLAY '  COUNTER UNKNWN = ' WS-CW-UNKCTR-CNT.
           DISPLAY '  OUT OF BALANCE = ' WS-CW-OOB-CNT.
           DISPLAY '  IO STATUS BAD  = ' WS-CW-IOERR-CNT.
           MOVE WS-CW-CT-READ TO WS-CW-ACC-ED.
           DISPLAY '  ACC READ       = ' WS-CW-ACC-ED.
           MOVE WS-CW-CT-WRIT TO WS-CW-ACC-ED.
           DISPLAY '  ACC WRITTEN    = ' WS-CW-ACC-ED.
           MOVE WS-CW-CT-REJ TO WS-CW-ACC-ED.
           DISPLAY '  ACC REJECTED   = ' WS-CW-ACC-ED.
           MOVE WS-CW-CT-SUM TO WS-CW-ACC-ED.
           DISPLAY '  ACC SUMMARISED = ' WS-CW-ACC-ED.
           MOVE WS-CW-CT-CFWD TO WS-CW-ACC-ED.
           DISPLAY '  ACC CARRIED    = ' WS-CW-ACC-ED.
           DISPLAY '  STEP SEQUENCE  = ' WS-CW-STEP-SEQ.
           IF WS-CW-OPEN-SW = 'Y'
               CLOSE CTLAUX
               MOVE 'N' TO WS-CW-OPEN-SW
               IF WS-FS-CTLAUX NOT = '00'
                   ADD 1 TO WS-CW-IOERR-CNT
                   DISPLAY '  CTLAUX CLOSE   = ' WS-FS-CTLAUX.
       P7000-EXIT.
           EXIT.
