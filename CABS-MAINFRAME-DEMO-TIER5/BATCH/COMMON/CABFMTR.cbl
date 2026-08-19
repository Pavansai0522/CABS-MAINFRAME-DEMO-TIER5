      *****************************************************************
      * CABFMTR  - DISPLAY FORMATTER FOR THE AUDIT REPORTS            *
      * APPLICATION : CABS                                            *
      * INVOKED BY  : CALL FROM THE BATCH UTILITY FAMILY              *
      * INPUTS      : LK-FM-FIELD X(08) WORK FIELD TO BE FORMATTED    *
      * OUTPUTS     : LK-FM-FIELD X(08) FORMATTED IN PLACE            *
      *               LK-FM-RC    9(04) RETURN CODE                   *
      * CONTROL     : NONE - SUBPROGRAMS DO NOT WRITE CTLOUT,         *
      *               CABS-STD-041                                    *
      * BALANCE     : NONE - NO RECORDS ARE READ OR WRITTEN HERE      *
      * RESTART     : NONE - NO POSITION IS HELD ACROSS A RESTART     *
      * REVISION HISTORY                                              *
      *   V1.00  1990-02-05  D.OKONKWO     INITIAL RELEASE            *
      *   V1.01  1993-08-17  R.T.WHEELER   ZERO SUPPRESSION EDIT      *
      *   V1.02  1995-10-11  P.NAIR        SIGNED VARIANT ADDED       *
      *   V1.03  1999-06-29  A.BUKOWSKI    YYDDD DAY OF YEAR FIRST    *
      *   V1.04  2003-03-24  L.FERREIRA    REPORT HEADING ALTERED     *
      *   V1.05  2008-12-02  M.HAAS        TEXT FOLD WALK IN          *
      *   V1.07  2019-04-11  S.MBEKI       TALLY FROM THE SENTINEL    *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABFMTR.
       AUTHOR. TELCABS APPLICATIONS - COMMON SUBROUTINE GROUP.
      *****************************************************************
      * TURNS AN EIGHT BYTE WORK FIELD INTO A PRINT READY FORM FOR    *
      * THE AUDIT REPORTS. THE FIELD IS EXAMINED BYTE BY BYTE AND ONE *
      * OF FOUR FORMATS IS APPLIED IN PLACE. THE INTERFACE IS EIGHT   *
      * BYTES. CALLERS THAT STAGE A LONGER FIELD PRESENT ITS LEADING  *
      * EIGHT BYTES. THE CALLING PROGRAM'S BALANCING EQUATION IS      *
      * UNAFFECTED BY ANYTHING THIS MODULE DOES. THE COUNTERS LIVE IN *
      * WORKING-STORAGE AND SURVIVE FROM ONE CALL TO THE NEXT FOR THE *
      * LIFE OF THE RUN UNIT, WHICH IS WHAT THE SENTINEL REPORTS ON.  *
      * THE FORMS ARE TESTED IN THIS ORDER - ALL SPACES, AN           *
      * UNCLASSIFIED BYTE, YYDDD, EIGHT DIGITS, DIGITS WITH ONE       *
      * TRAILING SIGN, AND FINALLY TEXT.                              *
      * RETURN CODES                                                  *
      *   0000  NUMERIC FORMATTED                                     *
      *   0004  SIGNED NUMERIC FORMATTED                              *
      *   0008  DATE EXPANDED                                         *
      *   0012  TEXT LEFT JUSTIFIED AND FOLDED                        *
      *   0016  FIELD WAS ALL SPACES, RETURNED UNCHANGED              *
      *   0020  A BYTE THE FORMATTER DOES NOT CLASSIFY - UNCHANGED    *
      *   0024  TALLIES DISPLAYED                                     *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-FM-CONSTANTS.
           05  WS-FM-VERSION     PIC X(05) VALUE 'V1.07'.
           05  WS-FM-SENTINEL    PIC X(08) VALUE '*END    '.
           05  WS-FM-MAX-BYTE    PIC S9(04) COMP-3 VALUE 8.
           05  WS-FM-MAX-CLASS   PIC S9(04) COMP-3 VALUE 68.
      * VIEWED AS BYTES, AS A YYDDD DATE, AND AS EIGHT DIGITS.
       01  WS-FM-WORK-AREA.
           05  WS-FM-WORK        PIC X(08).
       01  WS-FM-WORK-R REDEFINES WS-FM-WORK-AREA.
           05  WS-FM-BYTE OCCURS 8 TIMES PIC X(01).
       01  WS-FM-WORK-D REDEFINES WS-FM-WORK-AREA.
           05  WS-FM-DT-YY       PIC 9(02).
           05  WS-FM-DT-DDD      PIC 9(03).
           05  FILLER            PIC X(03).
       01  WS-FM-WORK-N REDEFINES WS-FM-WORK-AREA.
           05  WS-FM-NUM         PIC 9(08).
       01  WS-FM-CLASS-AREA.
           05  WS-FM-BCLS OCCURS 8 TIMES PIC X(01).
      * THE CLASS TABLE.  1 DIGIT  2 UPPER  3 LOWER  4 SPACE
      * 5 SIGN CHARACTER  6 PUNCTUATION  8 EVERYTHING ELSE
       01  WS-FM-CHR-LIT.
           05  FILLER  PIC X(10) VALUE '0123456789'.
           05  FILLER  PIC X(26) VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
           05  FILLER  PIC X(26) VALUE 'abcdefghijklmnopqrstuvwxyz'.
           05  FILLER  PIC X(06) VALUE ' +-/.&'.
       01  WS-FM-CHR-TAB REDEFINES WS-FM-CHR-LIT.
           05  WS-FM-CHR OCCURS 68 TIMES PIC X(01).
       01  WS-FM-CLS-LIT.
           05  FILLER  PIC X(10) VALUE '1111111111'.
           05  FILLER  PIC X(26) VALUE '22222222222222222222222222'.
           05  FILLER  PIC X(26) VALUE '33333333333333333333333333'.
           05  FILLER  PIC X(06) VALUE '455666'.
       01  WS-FM-CLS-TAB REDEFINES WS-FM-CLS-LIT.
           05  WS-FM-CLS OCCURS 68 TIMES PIC X(01).
      * INSPECT HERE HAS TALLYING AND REPLACING ONLY - HENCE THE WALK.
       01  WS-FM-LOW-LIT.
           05  FILLER  PIC X(26) VALUE 'abcdefghijklmnopqrstuvwxyz'.
       01  WS-FM-LOW-TAB REDEFINES WS-FM-LOW-LIT.
           05  WS-FM-LOW-CHR OCCURS 26 TIMES PIC X(01).
       01  WS-FM-UPP-LIT.
           05  FILLER  PIC X(26) VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
       01  WS-FM-UPP-TAB REDEFINES WS-FM-UPP-LIT.
           05  WS-FM-UPP-CHR OCCURS 26 TIMES PIC X(01).
       01  WS-FM-EDIT-AREA.
           05  WS-FM-EDIT        PIC ZZZZZZZ9.
       01  WS-FM-EDIT-R REDEFINES WS-FM-EDIT-AREA.
           05  WS-FM-EDIT-X      PIC X(08).
      * THE DAY OF YEAR IS FIRST - THE 1990 REPORT WAS LAID OUT SO.
       01  WS-FM-DATE-OUT.
           05  WS-FM-DO-DDD      PIC 9(03).
           05  FILLER            PIC X(01) VALUE '/'.
           05  WS-FM-DO-YY       PIC 9(02).
           05  FILLER            PIC X(02) VALUE SPACES.
       01  WS-FM-OUT-AREA.
           05  WS-FM-OUT         PIC X(08).
       01  WS-FM-OUT-R REDEFINES WS-FM-OUT-AREA.
           05  WS-FM-OUT-BYTE OCCURS 8 TIMES PIC X(01).
       01  WS-FM-DIG-AREA.
           05  WS-FM-DIG-CHR OCCURS 8 TIMES PIC X(01).
       01  WS-FM-COUNT-AREA.
           05  WS-FM-CALL-CNT    PIC S9(09) COMP-3 VALUE 0.
           05  WS-FM-NUM-CNT     PIC S9(09) COMP-3 VALUE 0.
           05  WS-FM-SGN-CNT     PIC S9(09) COMP-3 VALUE 0.
           05  WS-FM-DATE-CNT    PIC S9(09) COMP-3 VALUE 0.
           05  WS-FM-TEXT-CNT    PIC S9(09) COMP-3 VALUE 0.
           05  WS-FM-BLANK-CNT   PIC S9(09) COMP-3 VALUE 0.
           05  WS-FM-UNKN-CNT    PIC S9(09) COMP-3 VALUE 0.
           05  WS-FM-FOLD-CNT    PIC S9(09) COMP-3 VALUE 0.
           05  WS-FM-TALLY-CNT   PIC S9(09) COMP-3 VALUE 0.
       01  WS-FM-WORK-COUNTS.
           05  WS-FM-DIGITS      PIC S9(04) COMP-3 VALUE 0.
           05  WS-FM-ALPHAS      PIC S9(04) COMP-3 VALUE 0.
           05  WS-FM-SPACES      PIC S9(04) COMP-3 VALUE 0.
           05  WS-FM-SIGNS       PIC S9(04) COMP-3 VALUE 0.
           05  WS-FM-OTHERS      PIC S9(04) COMP-3 VALUE 0.
           05  WS-FM-DIG-CNT     PIC S9(04) COMP-3 VALUE 0.
           05  WS-FM-FIRST-NB    PIC S9(04) COMP-3 VALUE 0.
           05  WS-FM-LAST-NB     PIC S9(04) COMP-3 VALUE 0.
           05  WS-FM-SIGN-POS    PIC S9(04) COMP-3 VALUE 0.
           05  WS-FM-SHIFT       PIC S9(04) COMP-3 VALUE 0.
           05  WS-FM-SUB         PIC S9(04) COMP-3 VALUE 0.
           05  WS-FM-SUB2        PIC S9(04) COMP-3 VALUE 0.
           05  WS-FM-SUB3        PIC S9(04) COMP-3 VALUE 0.
           05  WS-FM-SW-FOUND    PIC X(01) VALUE 'N'.
           05  WS-FM-SW-DATE     PIC X(01) VALUE 'N'.
           05  WS-FM-SIGN-CHR    PIC X(01) VALUE ' '.
           05  WS-FM-CNT-ED      PIC ZZZ,ZZZ,ZZ9.
       LINKAGE SECTION.
       01  LK-FM-FIELD                 PIC X(08).
       01  LK-FM-RC                    PIC 9(04).
       PROCEDURE DIVISION USING LK-FM-FIELD LK-FM-RC.
       P0000-FORMAT-FIELD.
           ADD 1 TO WS-FM-CALL-CNT.
           MOVE 0 TO LK-FM-RC.
           IF LK-FM-FIELD = WS-FM-SENTINEL
               PERFORM P7000-TALLY THRU P7000-EXIT
               MOVE 24 TO LK-FM-RC
               GO TO P0000-EXIT.
           MOVE LK-FM-FIELD TO WS-FM-WORK.
           MOVE SPACES TO WS-FM-CLASS-AREA.
           MOVE 0 TO WS-FM-DIGITS WS-FM-ALPHAS WS-FM-SPACES
                     WS-FM-SIGNS WS-FM-OTHERS.
           MOVE 0 TO WS-FM-FIRST-NB WS-FM-LAST-NB WS-FM-SIGN-POS.
           MOVE SPACE TO WS-FM-SIGN-CHR.
           PERFORM P1100-CLASSIFY-BYTE THRU P1100-EXIT
               VARYING WS-FM-SUB FROM 1 BY 1
               UNTIL WS-FM-SUB > WS-FM-MAX-BYTE.
           PERFORM P2000-SELECT-FORMAT THRU P2000-EXIT.
       P0000-EXIT.
           GOBACK.
      * S100 - THE CLASS TABLE IS NOT IN ORDER SO IT IS WALKED.
       S100-CLASSIFICATION SECTION.
       P1100-CLASSIFY-BYTE.
           MOVE '8' TO WS-FM-BCLS (WS-FM-SUB).
           MOVE 'N' TO WS-FM-SW-FOUND.
           PERFORM P1200-MATCH-CLASS THRU P1200-EXIT
               VARYING WS-FM-SUB2 FROM 1 BY 1
               UNTIL WS-FM-SUB2 > WS-FM-MAX-CLASS
                  OR WS-FM-SW-FOUND = 'Y'.
           IF WS-FM-BCLS (WS-FM-SUB) = '1'
               ADD 1 TO WS-FM-DIGITS
           ELSE IF WS-FM-BCLS (WS-FM-SUB) = '2'
                   OR WS-FM-BCLS (WS-FM-SUB) = '3'
               ADD 1 TO WS-FM-ALPHAS
           ELSE IF WS-FM-BCLS (WS-FM-SUB) = '4'
               ADD 1 TO WS-FM-SPACES
           ELSE IF WS-FM-BCLS (WS-FM-SUB) = '5'
               ADD 1 TO WS-FM-SIGNS
               MOVE WS-FM-SUB TO WS-FM-SIGN-POS
               MOVE WS-FM-BYTE (WS-FM-SUB) TO WS-FM-SIGN-CHR
           ELSE IF WS-FM-BCLS (WS-FM-SUB) = '6'
               ADD 1 TO WS-FM-ALPHAS
           ELSE
               ADD 1 TO WS-FM-OTHERS.
           IF WS-FM-BCLS (WS-FM-SUB) NOT = '4'
               MOVE WS-FM-SUB TO WS-FM-LAST-NB
               IF WS-FM-FIRST-NB = 0
                   MOVE WS-FM-SUB TO WS-FM-FIRST-NB.
       P1100-EXIT.
           EXIT.
       P1200-MATCH-CLASS.
           IF WS-FM-CHR (WS-FM-SUB2) = WS-FM-BYTE (WS-FM-SUB)
               MOVE WS-FM-CLS (WS-FM-SUB2) TO WS-FM-BCLS (WS-FM-SUB)
               MOVE 'Y' TO WS-FM-SW-FOUND.
       P1200-EXIT.
           EXIT.
      * S200 - A YYDDD DATE FILLS THE FIRST FIVE BYTES AND LEAVES THE
      * LAST THREE AS SPACES. DAY OF YEAR 001 TO 366, SO LEAP PASSES.
       S200-SELECTION SECTION.
       P2000-SELECT-FORMAT.
           MOVE 'N' TO WS-FM-SW-DATE.
           IF WS-FM-BCLS (1) = '1' AND WS-FM-BCLS (2) = '1'
                   AND WS-FM-BCLS (3) = '1' AND WS-FM-BCLS (4) = '1'
                   AND WS-FM-BCLS (5) = '1' AND WS-FM-BCLS (6) = '4'
                   AND WS-FM-BCLS (7) = '4' AND WS-FM-BCLS (8) = '4'
               IF WS-FM-DT-DDD > 0 AND WS-FM-DT-DDD < 367
                   MOVE 'Y' TO WS-FM-SW-DATE.
           IF WS-FM-SPACES = 8
               ADD 1 TO WS-FM-BLANK-CNT
               MOVE 16 TO LK-FM-RC
               GO TO P2000-EXIT.
           IF WS-FM-OTHERS > 0
               ADD 1 TO WS-FM-UNKN-CNT
               MOVE 20 TO LK-FM-RC
               GO TO P2000-EXIT.
           IF WS-FM-SW-DATE = 'Y'
               PERFORM P3000-FORMAT-DATE THRU P3000-EXIT
               GO TO P2000-EXIT.
           IF WS-FM-DIGITS = 8
               PERFORM P4000-FORMAT-NUMERIC THRU P4000-EXIT
               GO TO P2000-EXIT.
           IF WS-FM-SIGNS = 1 AND WS-FM-SIGN-POS = WS-FM-LAST-NB
                   AND WS-FM-ALPHAS = 0 AND WS-FM-DIGITS > 0
               PERFORM P5000-FORMAT-SIGNED THRU P5000-EXIT
               GO TO P2000-EXIT.
           PERFORM P6000-FORMAT-TEXT THRU P6000-EXIT.
       P2000-EXIT.
           EXIT.
       P3000-FORMAT-DATE.
           MOVE WS-FM-DT-DDD TO WS-FM-DO-DDD.
           MOVE WS-FM-DT-YY TO WS-FM-DO-YY.
           MOVE WS-FM-DATE-OUT TO LK-FM-FIELD.
           ADD 1 TO WS-FM-DATE-CNT.
           MOVE 8 TO LK-FM-RC.
       P3000-EXIT.
           EXIT.
      * LEADING ZEROS ARE SUPPRESSED BY THE EDIT PICTURE.
       P4000-FORMAT-NUMERIC.
           MOVE WS-FM-NUM TO WS-FM-EDIT.
           MOVE WS-FM-EDIT-X TO LK-FM-FIELD.
           ADD 1 TO WS-FM-NUM-CNT.
           MOVE 0 TO LK-FM-RC.
       P4000-EXIT.
           EXIT.
      * THE TRAILING SIGN IS CARRIED TO BYTE ONE, DIGITS TO THE RIGHT.
       P5000-FORMAT-SIGNED.
           MOVE 0 TO WS-FM-DIG-CNT.
           PERFORM P5100-GATHER-DIGIT THRU P5100-EXIT
               VARYING WS-FM-SUB FROM 1 BY 1
               UNTIL WS-FM-SUB > WS-FM-MAX-BYTE.
           MOVE SPACES TO WS-FM-OUT.
           MOVE WS-FM-SIGN-CHR TO WS-FM-OUT-BYTE (1).
           MOVE WS-FM-MAX-BYTE TO WS-FM-SUB2.
           PERFORM P5200-PLACE-DIGIT THRU P5200-EXIT
               VARYING WS-FM-SUB FROM WS-FM-DIG-CNT BY -1
               UNTIL WS-FM-SUB < 1.
           MOVE WS-FM-OUT TO LK-FM-FIELD.
           ADD 1 TO WS-FM-SGN-CNT.
           MOVE 4 TO LK-FM-RC.
       P5000-EXIT.
           EXIT.
       P5100-GATHER-DIGIT.
           IF WS-FM-BCLS (WS-FM-SUB) = '1'
               ADD 1 TO WS-FM-DIG-CNT
               MOVE WS-FM-BYTE (WS-FM-SUB) TO
                   WS-FM-DIG-CHR (WS-FM-DIG-CNT).
       P5100-EXIT.
           EXIT.
       P5200-PLACE-DIGIT.
           IF WS-FM-SUB2 > 1
               MOVE WS-FM-DIG-CHR (WS-FM-SUB) TO
                   WS-FM-OUT-BYTE (WS-FM-SUB2)
               SUBTRACT 1 FROM WS-FM-SUB2.
       P5200-EXIT.
           EXIT.
      * S600 - THE SHIFT WALK RUNS LOW TO HIGH SO THE SOURCE BYTE IS
      * ALWAYS TO THE RIGHT OF THE RECEIVER AND STILL INTACT.
       S600-TEXT SECTION.
       P6000-FORMAT-TEXT.
           IF WS-FM-FIRST-NB > 1
               COMPUTE WS-FM-SHIFT = WS-FM-FIRST-NB - 1
               PERFORM P6100-SHIFT-BYTE THRU P6100-EXIT
                   VARYING WS-FM-SUB FROM 1 BY 1
                   UNTIL WS-FM-SUB > WS-FM-MAX-BYTE.
           PERFORM P6300-FOLD-TRY THRU P6300-EXIT
               VARYING WS-FM-SUB FROM 1 BY 1
               UNTIL WS-FM-SUB > WS-FM-MAX-BYTE
               AFTER WS-FM-SUB3 FROM 1 BY 1
               UNTIL WS-FM-SUB3 > 26.
           MOVE WS-FM-WORK TO LK-FM-FIELD.
           ADD 1 TO WS-FM-TEXT-CNT.
           MOVE 12 TO LK-FM-RC.
       P6000-EXIT.
           EXIT.
       P6100-SHIFT-BYTE.
           COMPUTE WS-FM-SUB2 = WS-FM-SUB + WS-FM-SHIFT.
           IF WS-FM-SUB2 > WS-FM-MAX-BYTE
               MOVE SPACE TO WS-FM-BYTE (WS-FM-SUB)
           ELSE
               MOVE WS-FM-BYTE (WS-FM-SUB2) TO WS-FM-BYTE (WS-FM-SUB).
       P6100-EXIT.
           EXIT.
       P6300-FOLD-TRY.
           IF WS-FM-LOW-CHR (WS-FM-SUB3) = WS-FM-BYTE (WS-FM-SUB)
               MOVE WS-FM-UPP-CHR (WS-FM-SUB3) TO
                   WS-FM-BYTE (WS-FM-SUB)
               ADD 1 TO WS-FM-FOLD-CNT.
       P6300-EXIT.
           EXIT.
      * S700-TALLY SECTION
       S700-TALLY SECTION.
       P7000-TALLY.
           ADD 1 TO WS-FM-TALLY-CNT.
           MOVE WS-FM-CALL-CNT TO WS-FM-CNT-ED.
           DISPLAY 'CABFMTR  ' WS-FM-VERSION ' - FORMAT TALLY'.
           DISPLAY '  CALLS          = ' WS-FM-CNT-ED.
           DISPLAY '  NUMERIC        = ' WS-FM-NUM-CNT.
           DISPLAY '  UNCLASSIFIED   = ' WS-FM-UNKN-CNT.
       P7000-EXIT.
           EXIT.
