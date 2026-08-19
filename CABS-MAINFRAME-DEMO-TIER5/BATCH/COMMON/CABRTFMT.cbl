      *****************************************************************
      * CABRTFMT - RATE ROUNDING AND FORMAT                           *
      * APPLICATION : CABS                                            *
      * INVOKED BY  : CALL FROM THE RATING PROGRAMS CABRAT03,         *
      *               CABRAT10 AND CABRAT12                           *
      * INPUTS      : LK-RF-AMOUNT-IN  S9(13)V9(05) COMP-3            *
      *               LK-RF-RULE-IN    RT-ROUND-RULE OFF THE RATE ROW *
      * OUTPUTS     : LK-RF-AMOUNT-OUT S9(13)V9(02) COMP-3            *
      *               LK-RF-RC         CONDITION OF THE ROUNDING      *
      * CONTROL     : NONE - SUBPROGRAMS DO NOT WRITE CTLOUT,         *
      *               CABS-STD-041                                    *
      * BALANCE     : NONE - THE CALLING PROGRAM RECORD COUNTS ARE    *
      *               NOT TOUCHED BY THIS MODULE                      *
      * RESTART     : NONE - NO FILE IS OPENED                        *
      * REVISION HISTORY                                              *
      *   V1.00  1989-02-06  R.T.WHEELER   INITIAL RELEASE            *
      *   V1.02  1992-06-30  D.OKONKWO     HALF EVEN RULE ADDED FOR   *
      *                      THE SETTLEMENT ELEMENTS                  *
      *   V2.00  1998-05-18  A.BUKOWSKI    SIGN TESTED FIRST AND THE  *
      *                      RULES APPLIED TO THE MAGNITUDE           *
      *   V2.03  2001-09-11  M.HAAS        SIZE ERROR TRAPPED ON THE  *
      *                      OUTPUT MOVE                              *
      *   V2.05  2005-04-01  E.KOWALCZYK   RULE F ADDED FOR THE       *
      *                      TARIFF FILING OF THAT YEAR               *
      *   V2.08  2010-11-22  L.FERREIRA    RESIDUE ACCUMULATED BY     *
      *                      RULE BYTE ACROSS THE RUN                 *
      *   V2.10  2016-01-25  P.NAIR        PER RULE CALL COUNTS ON    *
      *                      THE RESIDUE DISPLAY                      *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABRTFMT.
       AUTHOR. TELCABS APPLICATIONS - RATING.
      *****************************************************************
      * THIS MODULE IS THE ROUNDING AUTHORITY FOR THE APPLICATION.    *
      * EVERY AMOUNT THAT MOVES FROM FIVE DECIMAL PLACES TO TWO GOES  *
      * THROUGH IT, AND THE RULE BYTE OFF THE RATE ROW DECIDES HOW.   *
      * IT OPENS NO FILE AND KEEPS NO COUNTS FOR THE CALLER, SO THE   *
      * CALLING PROGRAM BALANCE EQUATION IS UNAFFECTED BY THIS        *
      * MODULE.                                                       *
      *                                                               *
      * THE 1998 REWRITE TESTS THE SIGN FIRST AND APPLIES THE RULE    *
      * TO THE MAGNITUDE, WHICH IS WHAT MADE THE CREDITS BEHAVE THE   *
      * WAY THE RULE NAMES READ - UP MEANS UP IN MAGNITUDE AND DOWN   *
      * MEANS DOWN IN MAGNITUDE ON A NEGATIVE AMOUNT AS WELL AS ON A  *
      * POSITIVE ONE.                                                 *
      *                                                               *
      * THE RESIDUE ACCUMULATORS HELD IN WORKING STORAGE ARE THE      *
      * RECORD OF WHAT ROUNDING TOOK OUT OF A RUN.  THEY ARE          *
      * DISPLAYED AND RESET WHEN THE RULE BYTE IS AN ASTERISK.        *
      *                                                               *
      * RETURN CODES SET IN LK-RF-RC                                  *
      *   0000  ROUNDED PER THE RULE                                  *
      *   0004  RULE BYTE BLANK OR UNRECOGNISED - HALF UP APPLIED     *
      *   0008  RESULT OVERFLOWED THE OUTPUT FIELD - OUTPUT IS ZERO   *
      *   0012  RULE F TRUNCATED - THE WHOLE CENTS PART WAS ZERO      *
      *   0016  RESIDUE ACCUMULATORS DISPLAYED AND RESET              *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * WORKING STORAGE IN A CALLED SUBPROGRAM SURVIVES FROM ONE CALL
      * TO THE NEXT INSIDE ONE RUN UNIT.  THE RESIDUE ACCUMULATORS
      * AND THE COUNTERS DEPEND ON THAT.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABRTFMT'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V2.10'.
           05  WS-RULE-MAX                 PIC S9(04) COMP-3 VALUE 7.
       01  WS-SWITCH-AREA.
           05  WS-FIRST-SW                 PIC X(01) VALUE 'Y'.
               88  WS-FIRST-CALL               VALUE 'Y'.
           05  WS-NEG-SW                   PIC X(01) VALUE 'N'.
               88  WS-AMOUNT-NEGATIVE          VALUE 'Y'.
           05  WS-DEFAULTED-SW             PIC X(01) VALUE 'N'.
               88  WS-RULE-DEFAULTED           VALUE 'Y'.
           05  WS-F-FELL-SW                PIC X(01) VALUE 'N'.
               88  WS-F-FELL-THROUGH           VALUE 'Y'.
           05  WS-OVERFLOW-SW              PIC X(01) VALUE 'N'.
               88  WS-OVERFLOW                 VALUE 'Y'.
       01  WS-COUNT-AREA.
           05  WS-CALL-CNT                 PIC S9(09) COMP-3 VALUE 0.
           05  WS-DEFAULT-CNT              PIC S9(09) COMP-3 VALUE 0.
           05  WS-F-FELL-CNT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-OVERFLOW-CNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-NEGATIVE-CNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-ACC-OVER-CNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-TRUNC-CNT                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DOWN-CNT                 PIC S9(09) COMP-3 VALUE 0.
       01  WS-SUBSCRIPT-AREA.
           05  WS-SUB-01                   PIC S9(04) COMP-3 VALUE 0.
           05  WS-RULE-SUB                 PIC S9(04) COMP-3 VALUE 7.
       01  WS-ARITHMETIC-AREA.
           05  WS-ABS-IN                   PIC S9(13)V9(05) COMP-3
                                               VALUE 0.
           05  WS-SCALED                   PIC S9(18) COMP-3 VALUE 0.
           05  WS-CENTS                    PIC S9(15) COMP-3 VALUE 0.
           05  WS-RESIDUE                  PIC S9(04) COMP-3 VALUE 0.
           05  WS-EVEN-QUOT                PIC S9(15) COMP-3 VALUE 0.
           05  WS-EVEN-REM                 PIC S9(02) COMP-3 VALUE 0.
           05  WS-SIGN-MULT                PIC S9(01) COMP-3 VALUE 1.
           05  WS-OUT-WORK                 PIC S9(13)V9(02) COMP-3
                                               VALUE 0.
           05  WS-DELTA                    PIC S9(13)V9(05) COMP-3
                                               VALUE 0.
       01  WS-EDIT-AREA.
           05  WS-CNT-EDIT                 PIC ZZZ,ZZZ,ZZ9.
           05  WS-AMT-EDIT                 PIC -ZZZ,ZZZ,ZZZ,ZZ9.99999.
      * THE SEVEN RULE SLOTS.  THE SEVENTH CARRIES EVERY RULE BYTE
      * THAT IS NOT ONE OF THE SIX NAMED RULES.
       01  WS-RULE-BYTES                   PIC X(07) VALUE 'HETUDF?'.
       01  WS-RULE-TAB REDEFINES WS-RULE-BYTES.
           05  WS-RB-CHAR OCCURS 7 TIMES   PIC X(01).
      * NO VALUE CLAUSE IS PERMITTED UNDER AN OCCURS SO THE
      * ACCUMULATORS ARE CLEARED ON THE FIRST CALL.
       01  WS-RESIDUE-TABLE.
           05  WS-RS-ENTRY OCCURS 7 TIMES.
               10  WS-RS-RESIDUE           PIC S9(13)V9(05) COMP-3.
               10  WS-RS-CALLS             PIC S9(09) COMP-3.
       LINKAGE SECTION.
       01  LK-RF-AMOUNT-IN                 PIC S9(13)V9(05) COMP-3.
       01  LK-RF-RULE-IN                   PIC X(01).
       01  LK-RF-AMOUNT-OUT                PIC S9(13)V9(02) COMP-3.
       01  LK-RF-RC                        PIC 9(04).
       PROCEDURE DIVISION USING LK-RF-AMOUNT-IN LK-RF-RULE-IN
           LK-RF-AMOUNT-OUT LK-RF-RC.
      * P0000-ENTRY - ONE PASS PER CALL.
       P0000-ENTRY.
           MOVE 0 TO LK-RF-RC.
           ADD 1 TO WS-CALL-CNT.
           MOVE 'N' TO WS-OVERFLOW-SW.
           IF WS-FIRST-CALL
               PERFORM P1000-FIRST-CALL THRU P1000-EXIT.
           IF LK-RF-RULE-IN = '*'
               PERFORM P8000-DISPLAY-RESIDUE THRU P8000-EXIT
               MOVE 16 TO LK-RF-RC
               GO TO P0000-RETURN.
           PERFORM P2000-SPLIT-AMOUNT THRU P2000-EXIT.
           PERFORM P3000-APPLY-RULE THRU P3000-EXIT.
           PERFORM P4000-BUILD-OUTPUT THRU P4000-EXIT.
           PERFORM P5000-ACCUMULATE THRU P5000-EXIT.
       P0000-RETURN.
           GOBACK.
      * S100-INITIALISATION SECTION
       S100-INITIALISATION SECTION.
       P1000-FIRST-CALL.
           MOVE 'N' TO WS-FIRST-SW.
           PERFORM P1100-CLEAR-SLOT THRU P1100-EXIT
               VARYING WS-SUB-01 FROM 1 BY 1
               UNTIL WS-SUB-01 > WS-RULE-MAX.
       P1000-EXIT.
           EXIT.
       P1100-CLEAR-SLOT.
           MOVE 0 TO WS-RS-RESIDUE (WS-SUB-01).
           MOVE 0 TO WS-RS-CALLS (WS-SUB-01).
       P1100-EXIT.
           EXIT.
      * S200-SPLIT SECTION - THE SIGN IS TAKEN OFF FIRST AND EVERY
      * RULE IS APPLIED TO THE MAGNITUDE.  THE FIVE DECIMAL INPUT IS
      * SCALED TO A WHOLE NUMBER AND DIVIDED BY A THOUSAND, WHICH
      * LEAVES THE WHOLE CENTS IN THE QUOTIENT AND THE THREE FURTHER
      * DECIMAL PLACES IN THE REMAINDER.
       S200-SPLIT SECTION.
       P2000-SPLIT-AMOUNT.
           MOVE 'N' TO WS-NEG-SW.
           MOVE 1 TO WS-SIGN-MULT.
           IF LK-RF-AMOUNT-IN < 0
               MOVE 'Y' TO WS-NEG-SW
               MOVE -1 TO WS-SIGN-MULT
               ADD 1 TO WS-NEGATIVE-CNT
               COMPUTE WS-ABS-IN = LK-RF-AMOUNT-IN * -1
           ELSE
               MOVE LK-RF-AMOUNT-IN TO WS-ABS-IN.
           COMPUTE WS-SCALED = WS-ABS-IN * 100000.
           DIVIDE 1000 INTO WS-SCALED GIVING WS-CENTS
               REMAINDER WS-RESIDUE.
       P2000-EXIT.
           EXIT.
      * S300-RULES SECTION
       S300-RULES SECTION.
       P3000-APPLY-RULE.
           MOVE 'N' TO WS-DEFAULTED-SW.
           MOVE 'N' TO WS-F-FELL-SW.
           PERFORM P3100-RULE-INDEX THRU P3100-EXIT.
           IF LK-RF-RULE-IN = 'H'
               PERFORM P3200-HALF-UP THRU P3200-EXIT
           ELSE
               IF LK-RF-RULE-IN = 'E'
                   PERFORM P3300-HALF-EVEN THRU P3300-EXIT
               ELSE
                   IF LK-RF-RULE-IN = 'T'
                       PERFORM P3400-TRUNCATE THRU P3400-EXIT
                   ELSE
                       IF LK-RF-RULE-IN = 'U'
                           PERFORM P3500-ALWAYS-UP THRU P3500-EXIT
                       ELSE
                           IF LK-RF-RULE-IN = 'D'
                               PERFORM P3600-ALWAYS-DOWN THRU
                                   P3600-EXIT
                           ELSE
                               IF LK-RF-RULE-IN = 'F'
                                   PERFORM P3700-FILED-RULE THRU
                                       P3700-EXIT
                               ELSE
                                   PERFORM P3800-DEFAULT THRU
                                       P3800-EXIT.
       P3000-EXIT.
           EXIT.
       P3100-RULE-INDEX.
           MOVE 7 TO WS-RULE-SUB.
           PERFORM P3110-MATCH-RULE THRU P3110-EXIT
               VARYING WS-SUB-01 FROM 1 BY 1
               UNTIL WS-SUB-01 > 6.
       P3100-EXIT.
           EXIT.
       P3110-MATCH-RULE.
           IF WS-RB-CHAR (WS-SUB-01) = LK-RF-RULE-IN
               MOVE WS-SUB-01 TO WS-RULE-SUB.
       P3110-EXIT.
           EXIT.
      * P3200-HALF-UP - A RESIDUE OF FIVE HUNDRED OR MORE STEPS THE
      * MAGNITUDE UP, WHICH IS AWAY FROM ZERO ON EITHER SIGN.
       P3200-HALF-UP.
           IF WS-RESIDUE NOT < 500
               ADD 1 TO WS-CENTS.
       P3200-EXIT.
           EXIT.
      * P3300-HALF-EVEN - A RESIDUE OF EXACTLY FIVE HUNDRED GOES TO
      * THE EVEN CENT.  ANYTHING ELSE BEHAVES AS HALF UP.
       P3300-HALF-EVEN.
           IF WS-RESIDUE > 500
               ADD 1 TO WS-CENTS
               GO TO P3300-EXIT.
           IF WS-RESIDUE < 500
               GO TO P3300-EXIT.
           DIVIDE 2 INTO WS-CENTS GIVING WS-EVEN-QUOT
               REMAINDER WS-EVEN-REM.
           IF WS-EVEN-REM NOT = 0
               ADD 1 TO WS-CENTS.
       P3300-EXIT.
           EXIT.
      * P3400-TRUNCATE - THE RESIDUE IS DISCARDED AND THE MAGNITUDE
      * STANDS, WHICH IS TOWARD ZERO ON EITHER SIGN.
       P3400-TRUNCATE.
           ADD 1 TO WS-TRUNC-CNT.
       P3400-EXIT.
           EXIT.
      * P3500-ALWAYS-UP - ANY RESIDUE AT ALL STEPS THE MAGNITUDE UP.
       P3500-ALWAYS-UP.
           IF WS-RESIDUE > 0
               ADD 1 TO WS-CENTS.
       P3500-EXIT.
           EXIT.
      * P3600-ALWAYS-DOWN - THE MAGNITUDE NEVER STEPS UP.
       P3600-ALWAYS-DOWN.
           ADD 1 TO WS-DOWN-CNT.
       P3600-EXIT.
           EXIT.
      * P3700-FILED-RULE - THE RULE THE 2005 TARIFF FILING BROUGHT
      * IN.  HALF UP ONLY WHERE THERE IS A WHOLE CENT TO ROUND ON,
      * AND TRUNCATION FOR THE SUB CENT ELEMENTS.
       P3700-FILED-RULE.
           IF WS-CENTS > 0
               PERFORM P3200-HALF-UP THRU P3200-EXIT
           ELSE
               MOVE 'Y' TO WS-F-FELL-SW
               ADD 1 TO WS-F-FELL-CNT
               PERFORM P3400-TRUNCATE THRU P3400-EXIT.
       P3700-EXIT.
           EXIT.
       P3800-DEFAULT.
           MOVE 'Y' TO WS-DEFAULTED-SW.
           ADD 1 TO WS-DEFAULT-CNT.
           PERFORM P3200-HALF-UP THRU P3200-EXIT.
       P3800-EXIT.
           EXIT.
      * S400-OUTPUT SECTION
       S400-OUTPUT SECTION.
       P4000-BUILD-OUTPUT.
           MOVE 0 TO WS-OUT-WORK.
           COMPUTE WS-OUT-WORK = WS-CENTS / 100
               ON SIZE ERROR
                   MOVE 'Y' TO WS-OVERFLOW-SW.
           IF WS-OVERFLOW
               GO TO P4000-OVERFLOW.
           COMPUTE LK-RF-AMOUNT-OUT = WS-OUT-WORK * WS-SIGN-MULT
               ON SIZE ERROR
                   MOVE 'Y' TO WS-OVERFLOW-SW.
           IF WS-OVERFLOW
               GO TO P4000-OVERFLOW.
           PERFORM P4100-SET-RETURN-CODE THRU P4100-EXIT.
           GO TO P4000-EXIT.
       P4000-OVERFLOW.
           MOVE 0 TO LK-RF-AMOUNT-OUT.
           ADD 1 TO WS-OVERFLOW-CNT.
           MOVE 8 TO LK-RF-RC.
       P4000-EXIT.
           EXIT.
       P4100-SET-RETURN-CODE.
           MOVE 0 TO LK-RF-RC.
           IF WS-RULE-DEFAULTED
               MOVE 4 TO LK-RF-RC.
           IF WS-F-FELL-THROUGH
               MOVE 12 TO LK-RF-RC.
       P4100-EXIT.
           EXIT.
      * S500-RESIDUE SECTION - WHAT THE ROUNDING TOOK OUT IS HELD
      * BY RULE BYTE FOR THE LIFE OF THE RUN UNIT.
       S500-RESIDUE SECTION.
       P5000-ACCUMULATE.
           IF WS-OVERFLOW
               GO TO P5000-EXIT.
           COMPUTE WS-DELTA = LK-RF-AMOUNT-IN - LK-RF-AMOUNT-OUT.
           ADD WS-DELTA TO WS-RS-RESIDUE (WS-RULE-SUB)
               ON SIZE ERROR
                   ADD 1 TO WS-ACC-OVER-CNT.
           ADD 1 TO WS-RS-CALLS (WS-RULE-SUB).
       P5000-EXIT.
           EXIT.
      * S800-DISPLAY SECTION - DRIVEN BY A RULE BYTE OF ASTERISK.
      * THE ACCUMULATORS ARE RESET AFTER THEY ARE DISPLAYED.
       S800-DISPLAY SECTION.
       P8000-DISPLAY-RESIDUE.
           DISPLAY 'CABRTFMT ' WS-PGM-VERSION ' - RESIDUE SUMMARY'.
           MOVE WS-CALL-CNT TO WS-CNT-EDIT.
           DISPLAY '  CALLS RECEIVED   = ' WS-CNT-EDIT.
           MOVE WS-NEGATIVE-CNT TO WS-CNT-EDIT.
           DISPLAY '  NEGATIVE AMOUNTS = ' WS-CNT-EDIT.
           MOVE WS-DEFAULT-CNT TO WS-CNT-EDIT.
           DISPLAY '  RULE DEFAULTED   = ' WS-CNT-EDIT.
           MOVE WS-F-FELL-CNT TO WS-CNT-EDIT.
           DISPLAY '  RULE F TRUNCATED = ' WS-CNT-EDIT.
           MOVE WS-OVERFLOW-CNT TO WS-CNT-EDIT.
           DISPLAY '  OUTPUT OVERFLOWS = ' WS-CNT-EDIT.
           MOVE WS-ACC-OVER-CNT TO WS-CNT-EDIT.
           DISPLAY '  ACCUM OVERFLOWS  = ' WS-CNT-EDIT.
           DISPLAY '  RESIDUE BY RULE'.
           PERFORM P8100-DISPLAY-ONE THRU P8100-EXIT
               VARYING WS-SUB-01 FROM 1 BY 1
               UNTIL WS-SUB-01 > WS-RULE-MAX.
           PERFORM P1100-CLEAR-SLOT THRU P1100-EXIT
               VARYING WS-SUB-01 FROM 1 BY 1
               UNTIL WS-SUB-01 > WS-RULE-MAX.
       P8000-EXIT.
           EXIT.
       P8100-DISPLAY-ONE.
           MOVE WS-RS-RESIDUE (WS-SUB-01) TO WS-AMT-EDIT.
           MOVE WS-RS-CALLS (WS-SUB-01) TO WS-CNT-EDIT.
           DISPLAY '    RULE ' WS-RB-CHAR (WS-SUB-01) '  '
               WS-AMT-EDIT '  ' WS-CNT-EDIT.
       P8100-EXIT.
           EXIT.
