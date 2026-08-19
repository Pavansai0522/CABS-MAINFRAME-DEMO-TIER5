      *****************************************************************
      * CABHASH  - CONTROL HASH TOTAL CONTRIBUTION                    *
      * APPLICATION : CABS                                            *
      * INVOKED BY  : CALL FROM A CABS BATCH PROGRAM ONCE PER RECORD  *
      *               AS THE OCN IS TAKEN OFF THE RECORD              *
      * INPUTS      : LK-HS-FIELD   X(04) THE FIELD TO CONTRIBUTE     *
      *               LK-HS-ACCUM   S9(15) COMP-3 THE ACCUMULATOR     *
      * OUTPUTS     : LK-HS-ACCUM   S9(15) COMP-3 UPDATED IN PLACE    *
      *               RETURN-CODE   SET BY THE MODULE, SEE BELOW      *
      * CONTROL     : NONE - SUBPROGRAMS DO NOT WRITE CTLOUT,         *
      *               CABS-STD-041                                    *
      * BALANCE     : THE ACCUMULATOR THIS MODULE BUILDS IS MOVED TO  *
      *               CT-HASH-OCN BY THE CALLER. THE CALLING PROGRAM  *
      *               RECORD BALANCE IS NOT AFFECTED BY THIS MODULE.  *
      * RESTART     : NONE - THE ACCUMULATOR BELONGS TO THE CALLER    *
      * REVISION HISTORY                                              *
      *   V1.00  1989-06-12  R.T.WHEELER   INITIAL RELEASE. WEIGHTS   *
      *                      ARE POWERS OF 37                         *
      *   V1.01  1992-11-05  D.OKONKWO     CALL COUNTER ADDED         *
      *   V1.02  1995-08-21  M.HAAS        FOLDING ADDED. THE         *
      *                      SETTLEMENT RUNS NOW CARRY ENOUGH         *
      *                      RECORDS TO PASS 9(15)                    *
      *   V1.03  1998-04-17  L.FERREIRA    SENTINEL FIELD *END ASKS   *
      *                      FOR THE TOTALS AT END OF JOB             *
      *   V1.04  2002-09-24  E.KOWALCZYK   COLLATING TABLE EXTENDED   *
      *                      WITH SPACE HYPHEN SLASH POINT AMPERSAND  *
      *   V1.05  2009-01-15  T.YAMASHITA   RECOMPILE ONLY, NO SOURCE  *
      *                      CHANGE                                   *
      *   V1.06  2014-06-02  J.CALLAGHAN   RECOMPILE ONLY, NO SOURCE  *
      *                      CHANGE                                   *
      *   V1.07  2019-02-11  P.NAIR        RECOMPILE ONLY, NO SOURCE  *
      *                      CHANGE                                   *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABHASH.
       AUTHOR. TELCABS APPLICATIONS - COMMON SUBPROGRAMS.
      *****************************************************************
      * THE MODULE TAKES ONE FOUR BYTE FIELD, TURNS IT INTO A         *
      * WEIGHTED NUMBER AND ADDS THAT NUMBER TO THE ACCUMULATOR THE   *
      * CALLER PASSES. THE CALLER MOVES THE ACCUMULATOR TO            *
      * CT-HASH-OCN IN ITS OWN P8000-CONTROL BEFORE THE CONTROL       *
      * RECORD IS WRITTEN, SO THE VALUE THIS MODULE BUILDS IS THE     *
      * OCN HASH THE WHOLE ESTATE BALANCES ON.                        *
      *                                                               *
      * THE DOCUMENTED INTERFACE IS A FOUR BYTE ALPHANUMERIC FIELD.   *
      * SOME CALL SITES PASS A WIDER FIELD OR A PACKED FIELD. THE     *
      * MOVE TAKES THE LEADING FOUR BYTES AND ONLY THOSE FOUR BYTES   *
      * CONTRIBUTE.                                                   *
      *                                                               *
      * THIS IS A SUBPROGRAM. IT WRITES NO CONTROL RECORD AND THE     *
      * CALLING PROGRAM BALANCE IS UNAFFECTED BY IT.                  *
      *                                                               *
      * THE DOMINANT INTERFACE CARRIES NO RETURN CODE OPERAND. THE    *
      * MODULE SETS RETURN-CODE INSTEAD                               *
      *   0  ALL FOUR BYTES RECOGNISED AND NO FOLD                    *
      *   4  ONE OR MORE BYTES NOT IN THE COLLATING TABLE             *
      *   8  A FOLD OCCURRED ON THIS CALL                             *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * WORKING-STORAGE OF A CALLED PROGRAM SURVIVES FROM ONE CALL TO
      * THE NEXT WITHIN A RUN UNIT. THE CALL COUNT, THE UNRECOGNISED
      * BYTE COUNT AND THE FOLD COUNT ALL RELY ON THAT.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABHASH '.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V1.07'.
           05  WS-SENTINEL                 PIC X(04) VALUE '*END'.
           05  WS-COLL-LIMIT               PIC S9(04) COMP-3
                                                  VALUE 41.
      * THE MODULUS IS THE LARGEST PRIME BELOW TEN TO THE FIFTEENTH.
      * SUBTRACTING IT LEAVES THE ACCUMULATOR INSIDE THE PACKED
      * FIELD AND KEEPS THE RESIDUE COMPARABLE BETWEEN RUNS.
       01  WS-HS-MODULUS                   PIC S9(15) COMP-3
                                     VALUE 999999999999937.
      * THE FIELD AS THE MODULE HOLDS IT, AND THE SAME STORAGE SEEN
      * ONE BYTE AT A TIME.
       01  WS-HS-WORK                      PIC X(04) VALUE SPACES.
       01  WS-HS-BYTES REDEFINES WS-HS-WORK.
           05  WS-HS-BYTE OCCURS 4 TIMES   PIC X(01).
      * THE COLLATING TABLE. POSITION IN THIS TABLE IS THE ORDINAL.
      * ROWS 1 THROUGH 36 ARE THE OCN ALPHABET. ROWS 37 THROUGH 41
      * WERE ADDED IN 2002 FOR THE PUNCTUATION THAT ARRIVES ON
      * TRUNK GROUP AND USOC FIELDS.
       01  WS-COLL-CONSTANTS.
           05  FILLER                      PIC X(18) VALUE
               '0123456789ABCDEFGH'.
           05  FILLER                      PIC X(18) VALUE
               'IJKLMNOPQRSTUVWXYZ'.
           05  FILLER                      PIC X(05) VALUE ' -/.&'.
       01  WS-COLL-TABLE REDEFINES WS-COLL-CONSTANTS.
           05  WS-COLL-CHAR OCCURS 41 TIMES
                                           PIC X(01).
      * POSITION WEIGHTS. THE OCN ALPHABET IS THIRTY SIX SYMBOLS
      * WIDE AND 37 IS THE PRIME ONE LARGER, WHICH SPREADS FOUR
      * CHARACTER KEYS EVENLY ACROSS THE ACCUMULATOR. THE ROWS ARE
      * 37 TO THE POWER ZERO, ONE, TWO AND THREE. CHOSEN 1989.
       01  WS-WEIGHT-CONSTANTS.
           05  FILLER                      PIC X(24) VALUE
               '000001000037001369050653'.
       01  WS-WEIGHT-TABLE REDEFINES WS-WEIGHT-CONSTANTS.
           05  WS-WEIGHT OCCURS 4 TIMES    PIC 9(06).
       01  WS-WORK-AREA.
           05  WS-ORDINAL                  PIC S9(04) COMP-3 VALUE 41.
           05  WS-BYTE-SUB                 PIC S9(04) COMP-3 VALUE 1.
           05  WS-COLL-SUB                 PIC S9(04) COMP-3 VALUE 1.
           05  WS-BYTE-VALUE               PIC S9(09) COMP-3 VALUE 0.
           05  WS-WEIGHTED-SUM             PIC S9(11) COMP-3 VALUE 0.
           05  WS-DIGIT-CNT                PIC S9(04) COMP-3 VALUE 0.
           05  WS-HS-PEAK                  PIC S9(15) COMP-3 VALUE 0.
       01  WS-SWITCH-AREA.
           05  WS-FOUND-SW                 PIC X(01) VALUE 'N'.
               88  WS-FOUND                    VALUE 'Y'.
           05  WS-UNKNOWN-SW               PIC X(01) VALUE 'N'.
               88  WS-UNKNOWN                  VALUE 'Y'.
           05  WS-FOLD-SW                  PIC X(01) VALUE 'N'.
               88  WS-FOLD                     VALUE 'Y'.
       01  WS-COUNT-AREA.
           05  WS-CNT-CALLS                PIC S9(11) COMP-3 VALUE 0.
           05  WS-CNT-UNKNOWN              PIC S9(11) COMP-3 VALUE 0.
           05  WS-CNT-FOLDS                PIC S9(11) COMP-3 VALUE 0.
           05  WS-CNT-SENTINEL             PIC S9(05) COMP-3 VALUE 0.
           05  WS-CNT-FORM-NNNN            PIC S9(11) COMP-3 VALUE 0.
           05  WS-CNT-FORM-ANNN            PIC S9(11) COMP-3 VALUE 0.
           05  WS-CNT-FORM-AANN            PIC S9(11) COMP-3 VALUE 0.
           05  WS-CNT-FORM-OTHER           PIC S9(11) COMP-3 VALUE 0.
       01  WS-EDIT-AREA.
           05  WS-CNT-EDIT                 PIC ZZZ,ZZZ,ZZZ,ZZ9.
           05  WS-ACC-EDIT                 PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9-.
       LINKAGE SECTION.
       01  LK-HS-FIELD                     PIC X(04).
       01  LK-HS-ACCUM                     PIC S9(15) COMP-3.
       PROCEDURE DIVISION USING LK-HS-FIELD LK-HS-ACCUM.
      * P0000-ENTRY - ONE CONTRIBUTION PER CALL. THE SENTINEL FIELD
      * *END IS PUNCHED BY THE CONTROL GROUP AT END OF JOB AND ASKS
      * FOR THE TOTALS. NOTHING IS ADDED TO THE ACCUMULATOR ON THAT
      * CALL.
       P0000-ENTRY.
           MOVE 0 TO RETURN-CODE.
           MOVE LK-HS-FIELD TO WS-HS-WORK.
           IF WS-HS-WORK = WS-SENTINEL
               ADD 1 TO WS-CNT-SENTINEL
               PERFORM P9000-DISPLAY-TOTALS THRU P9000-EXIT
               GO TO P0000-RETURN.
           ADD 1 TO WS-CNT-CALLS.
           MOVE 'N' TO WS-UNKNOWN-SW.
           MOVE 'N' TO WS-FOLD-SW.
           MOVE 0 TO WS-WEIGHTED-SUM.
           PERFORM P1000-CLASSIFY-FIELD THRU P1000-EXIT.
           PERFORM P2000-WEIGH-BYTE THRU P2000-EXIT
               VARYING WS-BYTE-SUB FROM 1 BY 1
               UNTIL WS-BYTE-SUB > 4.
           PERFORM P3000-ACCUMULATE THRU P3000-EXIT.
           PERFORM P3100-TRACK-PEAK THRU P3100-EXIT.
           PERFORM P4000-SET-RETURN-CODE THRU P4000-EXIT.
       P0000-RETURN.
           GOBACK.
      * S100-FIELD-FORM SECTION - THE SHAPE OF THE FIELD IS TALLIED.
       S100-FIELD-FORM SECTION.
      * AN OCN IS FILED IN ONE OF THREE FORMS. FOUR DIGITS, ONE
      * ALPHA FOLLOWED BY THREE DIGITS, OR TWO ALPHA FOLLOWED BY
      * TWO DIGITS. THE DIGIT COUNT TELLS THE THREE APART AND THE
      * TALLY IS SHOWN ON THE CONTROL GROUP PROOF SHEET. ANYTHING
      * ELSE IS COUNTED SEPARATELY AND STILL CONTRIBUTES TO THE
      * HASH IN THE NORMAL WAY.
       P1000-CLASSIFY-FIELD.
           MOVE 0 TO WS-DIGIT-CNT.
           INSPECT WS-HS-WORK TALLYING WS-DIGIT-CNT
               FOR ALL '0' ALL '1' ALL '2' ALL '3' ALL '4'
                   ALL '5' ALL '6' ALL '7' ALL '8' ALL '9'.
           IF WS-DIGIT-CNT = 4
               ADD 1 TO WS-CNT-FORM-NNNN
           ELSE
               IF WS-DIGIT-CNT = 3
                   ADD 1 TO WS-CNT-FORM-ANNN
               ELSE
                   IF WS-DIGIT-CNT = 2
                       ADD 1 TO WS-CNT-FORM-AANN
                   ELSE
                       ADD 1 TO WS-CNT-FORM-OTHER.
       P1000-EXIT.
           EXIT.
      * S200-WEIGHTING SECTION - ONE PASS PER BYTE.
       S200-WEIGHTING SECTION.
      * A BYTE THAT IS NOT IN THE TABLE TAKES ORDINAL 41 AND STEPS
      * ITS OWN COUNTER. LOW VALUES AND THE NIBBLES OF A PACKED
      * FIELD ARRIVE THAT WAY.
       P2000-WEIGH-BYTE.
           MOVE 41 TO WS-ORDINAL.
           MOVE 'N' TO WS-FOUND-SW.
           PERFORM P2100-SCAN-COLLATE THRU P2100-EXIT
               VARYING WS-COLL-SUB FROM 1 BY 1
               UNTIL WS-COLL-SUB > WS-COLL-LIMIT
                  OR WS-FOUND.
           IF NOT WS-FOUND
               MOVE 41 TO WS-ORDINAL
               MOVE 'Y' TO WS-UNKNOWN-SW
               ADD 1 TO WS-CNT-UNKNOWN.
           COMPUTE WS-BYTE-VALUE =
               WS-ORDINAL * WS-WEIGHT (WS-BYTE-SUB).
           ADD WS-BYTE-VALUE TO WS-WEIGHTED-SUM.
       P2000-EXIT.
           EXIT.
      * THE TABLE IS FORTY ONE ROWS AND IS WALKED WITH A SUBSCRIPT.
      * IT IS HELD IN COLLATING ORDER FOR THE ALPHABET BUT THE FIVE
      * ROWS ADDED IN 2002 SIT AT THE END, SO IT IS NOT IN ASCENDING
      * ORDER AND CANNOT BE SEARCHED WITH SEARCH ALL.
       P2100-SCAN-COLLATE.
           IF WS-HS-BYTE (WS-BYTE-SUB) = WS-COLL-CHAR (WS-COLL-SUB)
               MOVE 'Y' TO WS-FOUND-SW
               MOVE WS-COLL-SUB TO WS-ORDINAL.
       P2100-EXIT.
           EXIT.
      * S300-ACCUMULATION SECTION.
       S300-ACCUMULATION SECTION.
      * THE CONTRIBUTION IS ADDED IN THE ORDER THE CALLS ARRIVE. THE
      * SEQUENCE IS NEVER CHANGED, SO A RERUN THAT READS THE SAME
      * FILE IN THE SAME ORDER PRODUCES THE SAME TOTAL. WHEN THE
      * ACCUMULATOR REACHES THE MODULUS IT IS FOLDED BACK BY ONE
      * MODULUS AND THE FOLD COUNTER IS STEPPED.
       P3000-ACCUMULATE.
           ADD WS-WEIGHTED-SUM TO LK-HS-ACCUM.
           IF LK-HS-ACCUM NOT < WS-HS-MODULUS
               SUBTRACT WS-HS-MODULUS FROM LK-HS-ACCUM
               MOVE 'Y' TO WS-FOLD-SW
               ADD 1 TO WS-CNT-FOLDS.
       P3000-EXIT.
           EXIT.
      * THE HIGH WATER MARK IS KEPT SO THAT THE CONTROL GROUP CAN
      * SEE HOW CLOSE A RUN CAME TO THE MODULUS BEFORE IT FOLDED.
       P3100-TRACK-PEAK.
           IF LK-HS-ACCUM > WS-HS-PEAK
               MOVE LK-HS-ACCUM TO WS-HS-PEAK.
       P3100-EXIT.
           EXIT.
      * S400-RETURN SECTION.
       S400-RETURN SECTION.
      * A FOLD IS REPORTED IN PREFERENCE TO AN UNRECOGNISED BYTE.
       P4000-SET-RETURN-CODE.
           MOVE 0 TO RETURN-CODE.
           IF WS-UNKNOWN
               MOVE 4 TO RETURN-CODE.
           IF WS-FOLD
               MOVE 8 TO RETURN-CODE.
       P4000-EXIT.
           EXIT.
      * S900-TERMINATION SECTION.
       S900-TERMINATION SECTION.
       P9000-DISPLAY-TOTALS.
           DISPLAY 'CABHASH  ' WS-PGM-VERSION ' - HASH TOTALS'.
           MOVE WS-CNT-CALLS TO WS-CNT-EDIT.
           DISPLAY '  FIELDS HASHED     = ' WS-CNT-EDIT.
           MOVE WS-CNT-UNKNOWN TO WS-CNT-EDIT.
           DISPLAY '  BYTES UNRECOGNSD  = ' WS-CNT-EDIT.
           MOVE WS-CNT-FOLDS TO WS-CNT-EDIT.
           DISPLAY '  FOLDS APPLIED     = ' WS-CNT-EDIT.
           MOVE WS-CNT-FORM-NNNN TO WS-CNT-EDIT.
           DISPLAY '  OCN FORM NNNN     = ' WS-CNT-EDIT.
           MOVE WS-CNT-FORM-ANNN TO WS-CNT-EDIT.
           DISPLAY '  OCN FORM ANNN     = ' WS-CNT-EDIT.
           MOVE WS-CNT-FORM-AANN TO WS-CNT-EDIT.
           DISPLAY '  OCN FORM AANN     = ' WS-CNT-EDIT.
           MOVE WS-CNT-FORM-OTHER TO WS-CNT-EDIT.
           DISPLAY '  OTHER FORM        = ' WS-CNT-EDIT.
           MOVE WS-HS-PEAK TO WS-ACC-EDIT.
           DISPLAY '  ACCUMULATOR PEAK  = ' WS-ACC-EDIT.
           MOVE LK-HS-ACCUM TO WS-ACC-EDIT.
           DISPLAY '  ACCUMULATOR NOW   = ' WS-ACC-EDIT.
       P9000-EXIT.
           EXIT.
