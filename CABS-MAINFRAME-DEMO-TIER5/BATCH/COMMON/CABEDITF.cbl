      *****************************************************************
      * CABEDITF - FIELD EDIT AND NORMALISE                           *
      * APPLICATION : CABS                                            *
      * INVOKED BY  : CALL FROM THE BATCH UTILITY FAMILY              *
      * INPUTS      : LK-EF-FIELD X(08) WORK FIELD TO BE EDITED       *
      * OUTPUTS     : LK-EF-FIELD X(08) EDITED IN PLACE               *
      *               LK-EF-RC    9(04) RETURN CODE                   *
      * CONTROL     : NONE - SUBPROGRAMS DO NOT WRITE CTLOUT,         *
      *               CABS-STD-041                                    *
      * BALANCE     : NONE - NO RECORDS ARE READ OR WRITTEN HERE      *
      * RESTART     : NONE - NO POSITION IS HELD ACROSS A RESTART     *
      * REVISION HISTORY                                              *
      *   V1.00  1988-03-14  R.T.WHEELER   INITIAL RELEASE            *
      *   V1.01  1990-07-02  D.OKONKWO     TRAILING SPACE COUNT ADDED *
      *   V1.02  1992-11-30  P.NAIR        LOWER CASE FOLDED TO UPPER *
      *   V1.03  1996-04-18  A.BUKOWSKI    SHAPE SWITCH HELD          *
      *   V1.04  2001-08-27  L.FERREIRA    PUNCTUATION SET WIDENED TO *
      *                      HYPHEN SLASH PERIOD AND AMPERSAND        *
      *   V1.05  2007-02-09  M.HAAS        LEFT JUSTIFY WALK IN       *
      *   V1.06  2014-05-19  T.YAMASHITA   RECOMPILE ONLY             *
      *   V1.07  2019-01-23  J.CALLAGHAN   TALLY FROM THE SENTINEL    *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABEDITF.
       AUTHOR. TELCABS APPLICATIONS - COMMON SUBROUTINE GROUP.
      *****************************************************************
      * THE SHARED ALPHANUMERIC FIELD EDITOR. A WORK FIELD IS PASSED  *
      * IN AND COMES BACK NORMALISED IN PLACE, READY TO BE USED AS A  *
      * KEY, A TABLE ARGUMENT OR A PRINT ITEM.                        *
      * THE INTERFACE IS EIGHT BYTES. CALLERS THAT STAGE A LONGER     *
      * FIELD PRESENT ITS LEADING EIGHT BYTES. THE CALLING PROGRAM'S  *
      * BALANCING EQUATION IS UNAFFECTED BY ANYTHING THIS MODULE      *
      * DOES. WORKING-STORAGE HERE SURVIVES FROM ONE CALL TO THE NEXT *
      * FOR THE LIFE OF THE RUN UNIT, AND THE TALLY COUNTERS AND THE  *
      * SHAPE SWITCH DEPEND ON THAT.                                  *
      * RETURN CODES                                                  *
      *   0000  CLEAN - NOTHING WAS CHANGED                           *
      *   0004  LOWER CASE WAS FOLDED TO UPPER                        *
      *   0008  LOW-VALUES OR HIGH-VALUES REPLACED BY SPACES          *
      *   0012  THE FIELD WAS LEFT JUSTIFIED                          *
      *   0016  THE FIELD IS ALL SPACES                               *
      *   0020  AN UNACCEPTED BYTE WAS FOUND AND LEFT AS IT STANDS    *
      *   0024  TALLIES DISPLAYED                                     *
      * TESTED IN THE ORDER 0016 0020 0012 0008 0004, FIRST TO HOLD.  *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-EF-CONSTANTS.
           05  WS-EF-VERSION     PIC X(05) VALUE 'V1.07'.
           05  WS-EF-SENTINEL    PIC X(08) VALUE '*END    '.
           05  WS-EF-MAX-BYTE    PIC S9(04) COMP-3 VALUE 8.
           05  WS-EF-MAX-CLASS   PIC S9(04) COMP-3 VALUE 67.
      * NO BYTE IS ADDRESSED BY POSITION - ONLY THROUGH THE REDEFINES.
       01  WS-EF-WORK-AREA.
           05  WS-EF-WORK        PIC X(08).
       01  WS-EF-WORK-R REDEFINES WS-EF-WORK-AREA.
           05  WS-EF-BYTE OCCURS 8 TIMES PIC X(01).
       01  WS-EF-CLASS-AREA.
           05  WS-EF-BCLS OCCURS 8 TIMES PIC X(01).
      * THE EIGHT WAY CLASS TABLE.  1 DIGIT  2 UPPER ALPHA  3 SPACE
      * 4 LOW-VALUE  5 HIGH-VALUE  6 ACCEPTED PUNCTUATION
      * 7 LOWER ALPHA  8 EVERYTHING ELSE. LOW-VALUE AND HIGH-VALUE
      * CANNOT BE CARRIED IN A LITERAL SO THEY ARE TESTED FIRST.
       01  WS-EF-CHR-LIT.
           05  FILLER  PIC X(10) VALUE '0123456789'.
           05  FILLER  PIC X(26) VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
           05  FILLER  PIC X(26) VALUE 'abcdefghijklmnopqrstuvwxyz'.
           05  FILLER  PIC X(05) VALUE '-/.& '.
       01  WS-EF-CHR-TAB REDEFINES WS-EF-CHR-LIT.
           05  WS-EF-CHR OCCURS 67 TIMES PIC X(01).
       01  WS-EF-CLS-LIT.
           05  FILLER  PIC X(10) VALUE '1111111111'.
           05  FILLER  PIC X(26) VALUE '22222222222222222222222222'.
           05  FILLER  PIC X(26) VALUE '77777777777777777777777777'.
           05  FILLER  PIC X(05) VALUE '66663'.
       01  WS-EF-CLS-TAB REDEFINES WS-EF-CLS-LIT.
           05  WS-EF-CLS OCCURS 67 TIMES PIC X(01).
      * INSPECT ON THIS COMPILER OFFERS TALLYING AND REPLACING ONLY,
      * SO THE FOLD TO UPPER CASE IS A TWENTY SIX ENTRY WALK.
       01  WS-EF-LOW-LIT.
           05  FILLER  PIC X(26) VALUE 'abcdefghijklmnopqrstuvwxyz'.
       01  WS-EF-LOW-TAB REDEFINES WS-EF-LOW-LIT.
           05  WS-EF-LOW-CHR OCCURS 26 TIMES PIC X(01).
       01  WS-EF-UPP-LIT.
           05  FILLER  PIC X(26) VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
       01  WS-EF-UPP-TAB REDEFINES WS-EF-UPP-LIT.
           05  WS-EF-UPP-CHR OCCURS 26 TIMES PIC X(01).
       01  WS-EF-COUNT-AREA.
           05  WS-EF-CALL-CNT    PIC S9(09) COMP-3 VALUE 0.
           05  WS-EF-CLEAN-CNT   PIC S9(09) COMP-3 VALUE 0.
           05  WS-EF-FOLD-CNT    PIC S9(09) COMP-3 VALUE 0.
           05  WS-EF-BINARY-CNT  PIC S9(09) COMP-3 VALUE 0.
           05  WS-EF-JUST-CNT    PIC S9(09) COMP-3 VALUE 0.
           05  WS-EF-BLANK-CNT   PIC S9(09) COMP-3 VALUE 0.
           05  WS-EF-SPECIAL-CNT PIC S9(09) COMP-3 VALUE 0.
           05  WS-EF-ALLNUM-CNT  PIC S9(09) COMP-3 VALUE 0.
           05  WS-EF-ALLALF-CNT  PIC S9(09) COMP-3 VALUE 0.
           05  WS-EF-MIXED-CNT   PIC S9(09) COMP-3 VALUE 0.
           05  WS-EF-SHIFT-TOT   PIC S9(09) COMP-3 VALUE 0.
           05  WS-EF-TRAIL-TOT   PIC S9(09) COMP-3 VALUE 0.
           05  WS-EF-TALLY-CNT   PIC S9(09) COMP-3 VALUE 0.
       01  WS-EF-SWITCH-AREA.
           05  WS-EF-SW-FOLD     PIC X(01) VALUE 'N'.
           05  WS-EF-SW-BIN      PIC X(01) VALUE 'N'.
           05  WS-EF-SW-SPEC     PIC X(01) VALUE 'N'.
           05  WS-EF-SW-JUST     PIC X(01) VALUE 'N'.
           05  WS-EF-SW-BLANK    PIC X(01) VALUE 'N'.
           05  WS-EF-SW-FOUND    PIC X(01) VALUE 'N'.
      * SHAPE OF THE LAST FIELD.  N NUMERIC  A ALPHA  M MIXED  B BLANK
           05  WS-EF-OVERALL     PIC X(01) VALUE ' '.
       01  WS-EF-WORK-COUNTS.
           05  WS-EF-DIGITS      PIC S9(04) COMP-3 VALUE 0.
           05  WS-EF-UPPERS      PIC S9(04) COMP-3 VALUE 0.
           05  WS-EF-SPACES      PIC S9(04) COMP-3 VALUE 0.
           05  WS-EF-PUNCTS      PIC S9(04) COMP-3 VALUE 0.
           05  WS-EF-OTHERS      PIC S9(04) COMP-3 VALUE 0.
           05  WS-EF-FIRST-NB    PIC S9(04) COMP-3 VALUE 0.
           05  WS-EF-LAST-NB     PIC S9(04) COMP-3 VALUE 0.
           05  WS-EF-SHIFT       PIC S9(04) COMP-3 VALUE 0.
           05  WS-EF-SUB         PIC S9(04) COMP-3 VALUE 0.
           05  WS-EF-SUB2        PIC S9(04) COMP-3 VALUE 0.
           05  WS-EF-SUB3        PIC S9(04) COMP-3 VALUE 0.
           05  WS-EF-CNT-ED      PIC ZZZ,ZZZ,ZZ9.
       LINKAGE SECTION.
       01  LK-EF-FIELD                 PIC X(08).
       01  LK-EF-RC                    PIC 9(04).
       PROCEDURE DIVISION USING LK-EF-FIELD LK-EF-RC.
       P0000-EDIT-FIELD.
           ADD 1 TO WS-EF-CALL-CNT.
           MOVE 0 TO LK-EF-RC.
           IF LK-EF-FIELD = WS-EF-SENTINEL
               PERFORM P7000-TALLY THRU P7000-EXIT
               MOVE 24 TO LK-EF-RC
               GO TO P0000-EXIT.
           MOVE 'N' TO WS-EF-SW-FOLD WS-EF-SW-BIN WS-EF-SW-SPEC
                       WS-EF-SW-JUST WS-EF-SW-BLANK.
           MOVE SPACES TO WS-EF-CLASS-AREA.
           MOVE 0 TO WS-EF-SHIFT.
           MOVE LK-EF-FIELD TO WS-EF-WORK.
           PERFORM P1100-CLASSIFY-BYTE THRU P1100-EXIT
               VARYING WS-EF-SUB FROM 1 BY 1
               UNTIL WS-EF-SUB > WS-EF-MAX-BYTE.
           PERFORM P2000-NORMALISE THRU P2000-EXIT.
           PERFORM P3000-DERIVE-SHAPE THRU P3000-EXIT.
           PERFORM P4000-LEFT-JUSTIFY THRU P4000-EXIT.
           MOVE WS-EF-WORK TO LK-EF-FIELD.
           PERFORM P6000-SET-RETURN-CODE THRU P6000-EXIT.
       P0000-EXIT.
           GOBACK.
      * S100 - THE CLASS TABLE IS NOT IN ORDER SO IT IS WALKED.
       S100-CLASSIFICATION SECTION.
       P1100-CLASSIFY-BYTE.
           IF WS-EF-BYTE (WS-EF-SUB) = LOW-VALUES
               MOVE '4' TO WS-EF-BCLS (WS-EF-SUB)
           ELSE IF WS-EF-BYTE (WS-EF-SUB) = HIGH-VALUES
               MOVE '5' TO WS-EF-BCLS (WS-EF-SUB)
           ELSE
               MOVE '8' TO WS-EF-BCLS (WS-EF-SUB)
               MOVE 'N' TO WS-EF-SW-FOUND
               PERFORM P1300-MATCH-CLASS THRU P1300-EXIT
                   VARYING WS-EF-SUB2 FROM 1 BY 1
                   UNTIL WS-EF-SUB2 > WS-EF-MAX-CLASS
                      OR WS-EF-SW-FOUND = 'Y'.
       P1100-EXIT.
           EXIT.
       P1300-MATCH-CLASS.
           IF WS-EF-CHR (WS-EF-SUB2) = WS-EF-BYTE (WS-EF-SUB)
               MOVE WS-EF-CLS (WS-EF-SUB2) TO WS-EF-BCLS (WS-EF-SUB)
               MOVE 'Y' TO WS-EF-SW-FOUND.
       P1300-EXIT.
           EXIT.
      * S200-NORMALISATION SECTION
       S200-NORMALISATION SECTION.
       P2000-NORMALISE.
           MOVE 0 TO WS-EF-DIGITS WS-EF-UPPERS WS-EF-SPACES
                     WS-EF-PUNCTS WS-EF-OTHERS.
           PERFORM P2100-NORMALISE-BYTE THRU P2100-EXIT
               VARYING WS-EF-SUB FROM 1 BY 1
               UNTIL WS-EF-SUB > WS-EF-MAX-BYTE.
           IF WS-EF-SW-BIN = 'Y'
               ADD 1 TO WS-EF-BINARY-CNT.
           IF WS-EF-SW-FOLD = 'Y'
               ADD 1 TO WS-EF-FOLD-CNT.
           IF WS-EF-SW-SPEC = 'Y'
               ADD 1 TO WS-EF-SPECIAL-CNT.
       P2000-EXIT.
           EXIT.
      * AN UNACCEPTED BYTE IS COUNTED AND LEFT WHERE IT IS.
       P2100-NORMALISE-BYTE.
           IF WS-EF-BCLS (WS-EF-SUB) = '4'
                   OR WS-EF-BCLS (WS-EF-SUB) = '5'
               MOVE SPACE TO WS-EF-BYTE (WS-EF-SUB)
               MOVE '3' TO WS-EF-BCLS (WS-EF-SUB)
               MOVE 'Y' TO WS-EF-SW-BIN.
           IF WS-EF-BCLS (WS-EF-SUB) = '7'
               MOVE 'N' TO WS-EF-SW-FOUND
               PERFORM P2300-FOLD-TRY THRU P2300-EXIT
                   VARYING WS-EF-SUB3 FROM 1 BY 1
                   UNTIL WS-EF-SUB3 > 26
                      OR WS-EF-SW-FOUND = 'Y'
               MOVE '2' TO WS-EF-BCLS (WS-EF-SUB)
               MOVE 'Y' TO WS-EF-SW-FOLD.
           IF WS-EF-BCLS (WS-EF-SUB) = '1'
               ADD 1 TO WS-EF-DIGITS
           ELSE IF WS-EF-BCLS (WS-EF-SUB) = '2'
               ADD 1 TO WS-EF-UPPERS
           ELSE IF WS-EF-BCLS (WS-EF-SUB) = '3'
               ADD 1 TO WS-EF-SPACES
           ELSE IF WS-EF-BCLS (WS-EF-SUB) = '6'
               ADD 1 TO WS-EF-PUNCTS
           ELSE
               ADD 1 TO WS-EF-OTHERS
               MOVE 'Y' TO WS-EF-SW-SPEC.
       P2100-EXIT.
           EXIT.
       P2300-FOLD-TRY.
           IF WS-EF-LOW-CHR (WS-EF-SUB3) = WS-EF-BYTE (WS-EF-SUB)
               MOVE WS-EF-UPP-CHR (WS-EF-SUB3) TO
                   WS-EF-BYTE (WS-EF-SUB)
               MOVE 'Y' TO WS-EF-SW-FOUND.
       P2300-EXIT.
           EXIT.
      * S300-SHAPE SECTION
       S300-SHAPE SECTION.
       P3000-DERIVE-SHAPE.
           MOVE 'M' TO WS-EF-OVERALL.
           IF WS-EF-SPACES = 8
               MOVE 'B' TO WS-EF-OVERALL
               MOVE 'Y' TO WS-EF-SW-BLANK
               ADD 1 TO WS-EF-BLANK-CNT
           ELSE IF WS-EF-DIGITS + WS-EF-SPACES = 8
               MOVE 'N' TO WS-EF-OVERALL
               ADD 1 TO WS-EF-ALLNUM-CNT
           ELSE IF WS-EF-UPPERS + WS-EF-SPACES = 8
               MOVE 'A' TO WS-EF-OVERALL
               ADD 1 TO WS-EF-ALLALF-CNT
           ELSE
               ADD 1 TO WS-EF-MIXED-CNT.
       P3000-EXIT.
           EXIT.
      * S400 - A FIELD OF ALL SPACES HAS NO FIRST NON SPACE BYTE, SO
      * THE SHIFT IS NOT ENTERED AND THE FIELD GOES BACK AS IT CAME.
      * SPACES INSIDE A RUN ARE LEFT ALONE. ONLY THE TAIL IS COUNTED.
       S400-JUSTIFICATION SECTION.
       P4000-LEFT-JUSTIFY.
           MOVE 0 TO WS-EF-FIRST-NB.
           MOVE 0 TO WS-EF-LAST-NB.
           PERFORM P4100-FIND-BOUNDS THRU P4100-EXIT
               VARYING WS-EF-SUB FROM 1 BY 1
               UNTIL WS-EF-SUB > WS-EF-MAX-BYTE.
           IF WS-EF-FIRST-NB > 1
               COMPUTE WS-EF-SHIFT = WS-EF-FIRST-NB - 1
               PERFORM P4200-SHIFT-BYTE THRU P4200-EXIT
                   VARYING WS-EF-SUB FROM 1 BY 1
                   UNTIL WS-EF-SUB > WS-EF-MAX-BYTE
               COMPUTE WS-EF-LAST-NB = WS-EF-LAST-NB - WS-EF-SHIFT
               ADD WS-EF-SHIFT TO WS-EF-SHIFT-TOT
               ADD 1 TO WS-EF-JUST-CNT
               MOVE 'Y' TO WS-EF-SW-JUST.
           COMPUTE WS-EF-SUB2 = WS-EF-MAX-BYTE - WS-EF-LAST-NB.
           ADD WS-EF-SUB2 TO WS-EF-TRAIL-TOT.
       P4000-EXIT.
           EXIT.
       P4100-FIND-BOUNDS.
           IF WS-EF-BYTE (WS-EF-SUB) NOT = SPACE
               MOVE WS-EF-SUB TO WS-EF-LAST-NB
               IF WS-EF-FIRST-NB = 0
                   MOVE WS-EF-SUB TO WS-EF-FIRST-NB.
       P4100-EXIT.
           EXIT.
      * THE WALK RUNS LOW TO HIGH SO THE SOURCE BYTE IS ALWAYS TO THE
      * RIGHT OF THE RECEIVING BYTE AND IS STILL INTACT WHEN MOVED.
       P4200-SHIFT-BYTE.
           COMPUTE WS-EF-SUB2 = WS-EF-SUB + WS-EF-SHIFT.
           IF WS-EF-SUB2 > WS-EF-MAX-BYTE
               MOVE SPACE TO WS-EF-BYTE (WS-EF-SUB)
           ELSE
               MOVE WS-EF-BYTE (WS-EF-SUB2) TO WS-EF-BYTE (WS-EF-SUB).
       P4200-EXIT.
           EXIT.
      * S600-RETURN SECTION
       S600-RETURN SECTION.
       P6000-SET-RETURN-CODE.
           IF WS-EF-SW-BLANK = 'Y'
               MOVE 16 TO LK-EF-RC
           ELSE IF WS-EF-SW-SPEC = 'Y'
               MOVE 20 TO LK-EF-RC
           ELSE IF WS-EF-SW-JUST = 'Y'
               MOVE 12 TO LK-EF-RC
           ELSE IF WS-EF-SW-BIN = 'Y'
               MOVE 8 TO LK-EF-RC
           ELSE IF WS-EF-SW-FOLD = 'Y'
               MOVE 4 TO LK-EF-RC
           ELSE
               MOVE 0 TO LK-EF-RC
               ADD 1 TO WS-EF-CLEAN-CNT.
       P6000-EXIT.
           EXIT.
      * S700-TALLY SECTION - ACCUMULATED ACROSS THE WHOLE STEP.
       S700-TALLY SECTION.
       P7000-TALLY.
           ADD 1 TO WS-EF-TALLY-CNT.
           MOVE WS-EF-CALL-CNT TO WS-EF-CNT-ED.
           DISPLAY 'CABEDITF ' WS-EF-VERSION ' - FIELD EDIT TALLY'.
           DISPLAY '  CALLS          = ' WS-EF-CNT-ED.
           DISPLAY '  CLEAN          = ' WS-EF-CLEAN-CNT.
           DISPLAY '  CASE FOLDED    = ' WS-EF-FOLD-CNT.
           DISPLAY '  BINARY BLANKED = ' WS-EF-BINARY-CNT.
           DISPLAY '  LEFT JUSTIFIED = ' WS-EF-JUST-CNT.
           DISPLAY '  ALL SPACES     = ' WS-EF-BLANK-CNT.
           DISPLAY '  UNACCEPTED     = ' WS-EF-SPECIAL-CNT.
           DISPLAY '  SHAPE NUMERIC  = ' WS-EF-ALLNUM-CNT.
           DISPLAY '  SHAPE ALPHA    = ' WS-EF-ALLALF-CNT.
           DISPLAY '  TRAILING SPACE = ' WS-EF-TRAIL-TOT.
       P7000-EXIT.
           EXIT.
