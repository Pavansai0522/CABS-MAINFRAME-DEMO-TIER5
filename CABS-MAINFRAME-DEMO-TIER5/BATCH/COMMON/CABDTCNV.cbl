      *****************************************************************
      * CABDTCNV - JULIAN TO GREGORIAN DATE CONVERSION                *
      * APPLICATION : CABS                                            *
      * INVOKED BY  : CALL FROM A CABS BATCH PROGRAM, NORMALLY AFTER  *
      *               THE CYCLE DATE HAS BEEN TAKEN OFF THE PARM CARD *
      * INPUTS      : LK-DT-CCYYDDD    9(07) CENTURY JULIAN DATE      *
      * OUTPUTS     : LK-DT-GREG-DATE  CCYY MM DD                     *
      *               LK-DT-RC         9(04) RETURN CODE              *
      * CONTROL     : NONE - SUBPROGRAMS DO NOT WRITE CTLOUT,         *
      *               CABS-STD-041                                    *
      * BALANCE     : NONE - THE CALLING PROGRAM BALANCE IS NOT       *
      *               AFFECTED BY THIS MODULE                         *
      * RESTART     : NONE - THE MODULE CARRIES NO RECOVERABLE STATE  *
      * REVISION HISTORY                                              *
      *   V0.09  1987-01-27  R.T.WHEELER   ASSEMBLER PREDECESSOR      *
      *                      CABDTC00 PLACED IN THE LOAD LIBRARY      *
      *   V1.00  1993-04-19  S.MBEKI       RECODED IN COBOL FROM      *
      *                      CABDTC00, SAME ANSWERS                   *
      *   V1.02  1995-09-08  D.OKONKWO     DAY OF YEAR EDIT SPLIT     *
      *                      FROM THE YEAR EDIT                       *
      *   V1.03  1997-10-14  L.FERREIRA    Y2K REMEDIATION. THE       *
      *                      OPERAND IS NOW SEVEN DIGITS CCYYDDD AND  *
      *                      NO LONGER FIVE DIGITS YYDDD              *
      *   V1.04  1999-02-22  A.BUKOWSKI    FIVE DIGIT INPUT STILL     *
      *                      ACCEPTED AND PIVOTED AT 70               *
      *   V1.05  2004-07-06  T.YAMASHITA   DAY OF WEEK ADDED FOR THE  *
      *                      BILL PERIOD CALENDAR                     *
      *   V1.06  2011-03-30  B.R.HALVORSEN CALL COUNTERS AND THE      *
      *                      SUMMARY DISPLAY ADDED                    *
      *   V1.07  2018-08-13  P.NAIR        RECOMPILE ONLY, NO SOURCE  *
      *                      CHANGE                                   *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABDTCNV.
       AUTHOR. TELCABS APPLICATIONS - COMMON SUBPROGRAMS.
      *****************************************************************
      * THE MODULE TAKES A CENTURY JULIAN DATE AND RETURNS THE        *
      * GREGORIAN EQUIVALENT IN THE CABSDATE LAYOUT. THE CALLER       *
      * NORMALLY BUILDS THE OPERAND WITH                              *
      *   COMPUTE WS-XX-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.   *
      * AND PASSES DW-GREG-DATE OUT OF CABSDATE AS THE SECOND         *
      * OPERAND.                                                      *
      *                                                               *
      * THIS IS A SUBPROGRAM. IT WRITES NO CONTROL RECORD AND THE     *
      * CALLING PROGRAM BALANCE IS UNAFFECTED BY IT.                  *
      *                                                               *
      * RETURN CODES IN LK-DT-RC                                      *
      *   0000  CONVERTED                                             *
      *   0004  INPUT WAS A FIVE DIGIT YYDDD AND THE PIVOT OF 70      *
      *         WAS APPLIED                                           *
      *   0008  DAY OF YEAR OUT OF RANGE FOR THE YEAR. DAY 366 IN A   *
      *         NON LEAP YEAR IS THE COMMON CASE                      *
      *   0012  YEAR OUTSIDE 1900 THROUGH 2099                        *
      *   0016  INPUT NOT NUMERIC                                     *
      *   0020  INPUT WAS ZERO. THE CALLER GETS A GREGORIAN DATE OF   *
      *         ZEROES BACK                                           *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * WORKING-STORAGE OF A CALLED PROGRAM SURVIVES FROM ONE CALL TO
      * THE NEXT WITHIN A RUN UNIT. THE COUNTERS AND THE DAY OF WEEK
      * BELOW RELY ON THAT.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABDTCNV'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V1.07'.
           05  WS-YEAR-LOW                 PIC 9(04) VALUE 1900.
           05  WS-YEAR-HIGH                PIC 9(04) VALUE 2099.
      * THE INPUT IS HELD AS CHARACTERS SO THAT IT CAN BE EDITED
      * BEFORE ANY ARITHMETIC IS ATTEMPTED ON IT.
       01  WS-IN-CHAR                      PIC X(07) VALUE SPACES.
       01  WS-IN-PARTS REDEFINES WS-IN-CHAR.
           05  WS-IN-CC-X                  PIC X(02).
           05  WS-IN-CC REDEFINES WS-IN-CC-X
                                           PIC 9(02).
           05  WS-IN-YYDDD-X.
               10  WS-IN-YY                PIC 9(02).
               10  WS-IN-DDD               PIC 9(03).
       01  WS-YYDDD-CHAR                   PIC X(05) VALUE '00000'.
       01  WS-YYDDD-PARTS REDEFINES WS-YYDDD-CHAR.
           05  WS-PV-YY                    PIC 9(02).
           05  WS-PV-DDD                   PIC 9(03).
      * CUMULATIVE DAYS BEFORE THE FIRST OF EACH MONTH. THE FIRST
      * ROW IS ZERO BECAUSE NOTHING PRECEDES JANUARY.
       01  WS-CUM-NONLEAP-C.
           05  FILLER                      PIC X(18) VALUE
               '000031059090120151'.
           05  FILLER                      PIC X(18) VALUE
               '181212243273304334'.
       01  WS-CUM-NONLEAP REDEFINES WS-CUM-NONLEAP-C.
           05  WS-CUM-NL OCCURS 12 TIMES   PIC 9(03).
       01  WS-CUM-LEAP-C.
           05  FILLER                      PIC X(18) VALUE
               '000031060091121152'.
           05  FILLER                      PIC X(18) VALUE
               '182213244274305335'.
       01  WS-CUM-LEAP REDEFINES WS-CUM-LEAP-C.
           05  WS-CUM-LP OCCURS 12 TIMES   PIC 9(03).
      * DAY OF WEEK NAMES IN THE ORDER ZELLER PRODUCES THEM. THE
      * REMAINDER OF ZERO IS SATURDAY.
       01  WS-DOW-CONSTANTS.
           05  FILLER                      PIC X(21) VALUE
               'SATSUNMONTUEWEDTHUFRI'.
       01  WS-DOW-TABLE REDEFINES WS-DOW-CONSTANTS.
           05  WS-DOW-NAME OCCURS 7 TIMES  PIC X(03).
       01  WS-WORK-AREA.
           05  WS-WORK-CCYY                PIC 9(04) VALUE 0.
           05  WS-WORK-DDD                 PIC 9(03) VALUE 0.
           05  WS-DAYS-IN-YEAR             PIC 9(03) VALUE 365.
           05  WS-MM                       PIC 9(02) VALUE 0.
           05  WS-DD                       PIC 9(02) VALUE 0.
           05  WS-SUB                      PIC S9(04) COMP-3 VALUE 0.
           05  WS-DIGIT-CNT                PIC S9(04) COMP-3 VALUE 0.
       01  WS-DIVIDE-AREA.
           05  WS-QUOTIENT                 PIC 9(06) VALUE 0.
           05  WS-REM-004                  PIC 9(04) VALUE 0.
           05  WS-REM-100                  PIC 9(04) VALUE 0.
           05  WS-REM-400                  PIC 9(04) VALUE 0.
      * ZELLER CONGRUENCE WORK. THE DAY OF WEEK IS HELD HERE AND IS
      * DISPLAYED WITH THE SUMMARY. THE INTERFACE CARRIES THREE
      * OPERANDS AND THE FIELD IS NOT RETURNED TO THE CALLER.
       01  WS-ZELLER-AREA.
           05  WS-Z-Q                      PIC 9(02) VALUE 0.
           05  WS-Z-M                      PIC 9(02) VALUE 0.
           05  WS-Z-YEAR                   PIC 9(04) VALUE 0.
           05  WS-Z-J                      PIC 9(02) VALUE 0.
           05  WS-Z-K                      PIC 9(02) VALUE 0.
           05  WS-Z-T1                     PIC 9(04) VALUE 0.
           05  WS-Z-T2                     PIC 9(04) VALUE 0.
           05  WS-Z-T3                     PIC 9(04) VALUE 0.
           05  WS-Z-T4                     PIC 9(04) VALUE 0.
           05  WS-Z-T5                     PIC 9(04) VALUE 0.
           05  WS-Z-SUM                    PIC 9(06) VALUE 0.
           05  WS-Z-H                      PIC 9(02) VALUE 0.
           05  WS-Z-SUB                    PIC S9(04) COMP-3 VALUE 1.
           05  WS-DAY-OF-WEEK              PIC X(03) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-INPUT-OK-SW              PIC X(01) VALUE 'Y'.
               88  WS-INPUT-OK                 VALUE 'Y'.
           05  WS-LEAP-SW                  PIC X(01) VALUE 'N'.
               88  WS-LEAP                     VALUE 'Y'.
       01  WS-COUNT-AREA.
           05  WS-CNT-CALLS                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CNT-CONVERTED            PIC S9(09) COMP-3 VALUE 0.
           05  WS-CNT-PIVOTED              PIC S9(09) COMP-3 VALUE 0.
           05  WS-CNT-BAD-DAY              PIC S9(09) COMP-3 VALUE 0.
           05  WS-CNT-BAD-YEAR             PIC S9(09) COMP-3 VALUE 0.
           05  WS-CNT-NOT-NUMERIC          PIC S9(09) COMP-3 VALUE 0.
           05  WS-CNT-ZERO-INPUT           PIC S9(09) COMP-3 VALUE 0.
           05  WS-CNT-LEAP-YEARS           PIC S9(09) COMP-3 VALUE 0.
       01  WS-EDIT-AREA.
           05  WS-CNT-EDIT                 PIC ZZZ,ZZZ,ZZ9.
       LINKAGE SECTION.
       01  LK-DT-CCYYDDD                   PIC 9(07).
       01  LK-DT-GREG-DATE.
           05  LK-DT-GR-CCYY               PIC 9(04).
           05  LK-DT-GR-MM                 PIC 9(02).
           05  LK-DT-GR-DD                 PIC 9(02).
       01  LK-DT-RC                        PIC 9(04).
       PROCEDURE DIVISION USING LK-DT-CCYYDDD LK-DT-GREG-DATE
           LK-DT-RC.
      * P0000-ENTRY - ONE CONVERSION PER CALL. EACH STAGE IS SKIPPED
      * ONCE THE INPUT HAS FAILED AN EDIT.
       P0000-ENTRY.
           ADD 1 TO WS-CNT-CALLS.
           MOVE 0 TO LK-DT-RC.
           MOVE 'Y' TO WS-INPUT-OK-SW.
           MOVE 'N' TO WS-LEAP-SW.
           MOVE LK-DT-CCYYDDD TO WS-IN-CHAR.
           IF WS-IN-CHAR = ALL '0'
               PERFORM P7000-ZERO-INPUT THRU P7000-EXIT
               GO TO P0000-RETURN.
           PERFORM P1000-EDIT-INPUT THRU P1000-EXIT.
           IF WS-INPUT-OK
               PERFORM P2000-RESOLVE-YEAR THRU P2000-EXIT.
           IF WS-INPUT-OK
               PERFORM P3000-SET-LEAP THRU P3000-EXIT.
           IF WS-INPUT-OK
               PERFORM P4000-EDIT-DAY THRU P4000-EXIT.
           IF WS-INPUT-OK
               PERFORM P5000-WALK-TABLE THRU P5000-EXIT.
           IF WS-INPUT-OK
               PERFORM P6000-DAY-OF-WEEK THRU P6000-EXIT.
           IF NOT WS-INPUT-OK
               PERFORM P7100-ZERO-RESULT THRU P7100-EXIT.
       P0000-RETURN.
           GOBACK.
      * S100-EDIT SECTION - THE INPUT IS PROVED BEFORE IT IS USED.
       S100-EDIT SECTION.
      * SEVEN DIGITS ARE COUNTED WITH INSPECT. A FIELD CARRYING
      * BLANKS OR A SIGN OVERPUNCH FAILS HERE AND GOES NO FURTHER.
       P1000-EDIT-INPUT.
           MOVE 0 TO WS-DIGIT-CNT.
           INSPECT WS-IN-CHAR TALLYING WS-DIGIT-CNT
               FOR ALL '0' ALL '1' ALL '2' ALL '3' ALL '4'
                   ALL '5' ALL '6' ALL '7' ALL '8' ALL '9'.
           IF WS-DIGIT-CNT NOT = 7
               MOVE 'N' TO WS-INPUT-OK-SW
               MOVE 16 TO LK-DT-RC
               ADD 1 TO WS-CNT-NOT-NUMERIC.
       P1000-EXIT.
           EXIT.
      * S200-YEAR SECTION - CENTURY RESOLUTION AND RANGE.
       S200-YEAR SECTION.
       P2000-RESOLVE-YEAR.
           IF WS-IN-CC-X = '19' OR WS-IN-CC-X = '20'
               PERFORM P2050-TAKE-CENTURY THRU P2050-EXIT
           ELSE
               PERFORM P2100-APPLY-PIVOT THRU P2100-EXIT.
           IF WS-WORK-CCYY < WS-YEAR-LOW
              OR WS-WORK-CCYY > WS-YEAR-HIGH
               MOVE 'N' TO WS-INPUT-OK-SW
               MOVE 12 TO LK-DT-RC
               ADD 1 TO WS-CNT-BAD-YEAR.
       P2000-EXIT.
           EXIT.
       P2050-TAKE-CENTURY.
           COMPUTE WS-WORK-CCYY = (WS-IN-CC * 100) + WS-IN-YY.
           MOVE WS-IN-DDD TO WS-WORK-DDD.
       P2050-EXIT.
           EXIT.
      * A NUMBER OF PLACES IN THE ESTATE STILL CARRY THE FIVE DIGIT
      * YYDDD FORM. WHEN THE LEADING TWO DIGITS ARE NEITHER 19 NOR
      * 20 THE FIELD IS READ AS YYDDD SHIFTED RIGHT AND THE YEAR IS
      * PIVOTED. THE PIVOT IS 70. A YEAR OF LESS THAN 70 IS IN THE
      * TWENTY FIRST CENTURY. THE SAME LITERAL IS CARRIED IN SIX
      * OTHER PLACES IN THE ESTATE.
       P2100-APPLY-PIVOT.
           MOVE WS-IN-YYDDD-X TO WS-YYDDD-CHAR.
           IF WS-PV-YY < 70
               COMPUTE WS-WORK-CCYY = 2000 + WS-PV-YY
           ELSE
               COMPUTE WS-WORK-CCYY = 1900 + WS-PV-YY.
           MOVE WS-PV-DDD TO WS-WORK-DDD.
           MOVE 4 TO LK-DT-RC.
           ADD 1 TO WS-CNT-PIVOTED.
       P2100-EXIT.
           EXIT.
      * S300-LEAP SECTION - THE FULL GREGORIAN RULE.
       S300-LEAP SECTION.
      * A YEAR DIVISIBLE BY FOUR IS A LEAP YEAR UNLESS IT IS ALSO
      * DIVISIBLE BY ONE HUNDRED, AND THEN IT IS A LEAP YEAR ONLY
      * WHEN IT IS DIVISIBLE BY FOUR HUNDRED. 2000 IS A LEAP YEAR
      * AND 1900 IS NOT.
       P3000-SET-LEAP.
           MOVE 'N' TO WS-LEAP-SW.
           DIVIDE WS-WORK-CCYY BY 4 GIVING WS-QUOTIENT
               REMAINDER WS-REM-004.
           DIVIDE WS-WORK-CCYY BY 100 GIVING WS-QUOTIENT
               REMAINDER WS-REM-100.
           DIVIDE WS-WORK-CCYY BY 400 GIVING WS-QUOTIENT
               REMAINDER WS-REM-400.
           IF WS-REM-004 = 0
               IF WS-REM-100 NOT = 0
                   MOVE 'Y' TO WS-LEAP-SW
               ELSE
                   IF WS-REM-400 = 0
                       MOVE 'Y' TO WS-LEAP-SW.
           MOVE 365 TO WS-DAYS-IN-YEAR.
           IF WS-LEAP
               MOVE 366 TO WS-DAYS-IN-YEAR
               ADD 1 TO WS-CNT-LEAP-YEARS.
       P3000-EXIT.
           EXIT.
      * S400-DAY SECTION - RANGE OF THE DAY OF YEAR.
       S400-DAY SECTION.
       P4000-EDIT-DAY.
           IF WS-WORK-DDD < 1
              OR WS-WORK-DDD > WS-DAYS-IN-YEAR
               MOVE 'N' TO WS-INPUT-OK-SW
               MOVE 8 TO LK-DT-RC
               ADD 1 TO WS-CNT-BAD-DAY.
       P4000-EXIT.
           EXIT.
      * S500-CONVERSION SECTION - DAY OF YEAR TO MONTH AND DAY.
       S500-CONVERSION SECTION.
      * THE CUMULATIVE TABLE IS TWELVE ROWS AND IS WALKED FROM ROW
      * ONE TO ROW TWELVE WITH A SUBSCRIPT. TWELVE COMPARES COST
      * LESS THAN SETTING UP AN INDEX AND A SEARCH, SO NO SEARCH IS
      * USED HERE. THE LAST ROW THAT SATISFIES THE TEST IS THE
      * MONTH.
       P5000-WALK-TABLE.
           MOVE 0 TO WS-MM.
           MOVE 0 TO WS-DD.
           IF WS-LEAP
               PERFORM P5200-WALK-LEAP THRU P5200-EXIT
                   VARYING WS-SUB FROM 1 BY 1
                   UNTIL WS-SUB > 12
           ELSE
               PERFORM P5100-WALK-NONLEAP THRU P5100-EXIT
                   VARYING WS-SUB FROM 1 BY 1
                   UNTIL WS-SUB > 12.
           MOVE WS-WORK-CCYY TO LK-DT-GR-CCYY.
           MOVE WS-MM TO LK-DT-GR-MM.
           MOVE WS-DD TO LK-DT-GR-DD.
           ADD 1 TO WS-CNT-CONVERTED.
       P5000-EXIT.
           EXIT.
       P5100-WALK-NONLEAP.
           IF WS-CUM-NL (WS-SUB) < WS-WORK-DDD
               MOVE WS-SUB TO WS-MM
               COMPUTE WS-DD = WS-WORK-DDD - WS-CUM-NL (WS-SUB).
       P5100-EXIT.
           EXIT.
       P5200-WALK-LEAP.
           IF WS-CUM-LP (WS-SUB) < WS-WORK-DDD
               MOVE WS-SUB TO WS-MM
               COMPUTE WS-DD = WS-WORK-DDD - WS-CUM-LP (WS-SUB).
       P5200-EXIT.
           EXIT.
      * S600-CALENDAR SECTION - DAY OF WEEK.
       S600-CALENDAR SECTION.
      * ZELLER CONGRUENCE. JANUARY AND FEBRUARY ARE TREATED AS THE
      * THIRTEENTH AND FOURTEENTH MONTHS OF THE PRECEDING YEAR. THE
      * RESULT IS HELD IN WS-DAY-OF-WEEK FOR THE BILL PERIOD
      * CALENDAR AND IS SHOWN WITH THE SUMMARY.
       P6000-DAY-OF-WEEK.
           MOVE WS-MM TO WS-Z-M.
           MOVE WS-WORK-CCYY TO WS-Z-YEAR.
           MOVE WS-DD TO WS-Z-Q.
           IF WS-Z-M < 3
               ADD 12 TO WS-Z-M
               SUBTRACT 1 FROM WS-Z-YEAR.
           DIVIDE WS-Z-YEAR BY 100 GIVING WS-Z-J
               REMAINDER WS-Z-K.
           COMPUTE WS-Z-T1 = 13 * (WS-Z-M + 1).
           DIVIDE WS-Z-T1 BY 5 GIVING WS-Z-T2.
           DIVIDE WS-Z-K BY 4 GIVING WS-Z-T3.
           DIVIDE WS-Z-J BY 4 GIVING WS-Z-T4.
           COMPUTE WS-Z-SUM = WS-Z-Q + WS-Z-T2 + WS-Z-K
               + WS-Z-T3 + WS-Z-T4 + (5 * WS-Z-J).
           DIVIDE WS-Z-SUM BY 7 GIVING WS-Z-T5
               REMAINDER WS-Z-H.
           COMPUTE WS-Z-SUB = WS-Z-H + 1.
           MOVE WS-DOW-NAME (WS-Z-SUB) TO WS-DAY-OF-WEEK.
       P6000-EXIT.
           EXIT.
      * S700-EXCEPTION SECTION - ZERO INPUT AND FAILED EDITS.
       S700-EXCEPTION SECTION.
      * AN INPUT OF ALL ZEROES IS THE END OF JOB CONVENTION. THE
      * TOTALS ARE SHOWN AND A GREGORIAN DATE OF ZEROES IS RETURNED.
       P7000-ZERO-INPUT.
           ADD 1 TO WS-CNT-ZERO-INPUT.
           MOVE 20 TO LK-DT-RC.
           PERFORM P7100-ZERO-RESULT THRU P7100-EXIT.
           PERFORM P9500-DISPLAY-TOTALS THRU P9500-EXIT.
       P7000-EXIT.
           EXIT.
       P7100-ZERO-RESULT.
           MOVE 0 TO LK-DT-GR-CCYY.
           MOVE 0 TO LK-DT-GR-MM.
           MOVE 0 TO LK-DT-GR-DD.
       P7100-EXIT.
           EXIT.
      * S900-TERMINATION SECTION.
       S900-TERMINATION SECTION.
       P9500-DISPLAY-TOTALS.
           DISPLAY 'CABDTCNV ' WS-PGM-VERSION ' - DATE CONVERSION'.
           MOVE WS-CNT-CALLS TO WS-CNT-EDIT.
           DISPLAY '  CALLS TAKEN       = ' WS-CNT-EDIT.
           MOVE WS-CNT-CONVERTED TO WS-CNT-EDIT.
           DISPLAY '  DATES CONVERTED   = ' WS-CNT-EDIT.
           MOVE WS-CNT-PIVOTED TO WS-CNT-EDIT.
           DISPLAY '  PIVOT APPLIED     = ' WS-CNT-EDIT.
           MOVE WS-CNT-BAD-DAY TO WS-CNT-EDIT.
           DISPLAY '  DAY OUT OF RANGE  = ' WS-CNT-EDIT.
           MOVE WS-CNT-BAD-YEAR TO WS-CNT-EDIT.
           DISPLAY '  YEAR OUT OF RANGE = ' WS-CNT-EDIT.
           MOVE WS-CNT-NOT-NUMERIC TO WS-CNT-EDIT.
           DISPLAY '  INPUT NOT NUMERIC = ' WS-CNT-EDIT.
           MOVE WS-CNT-LEAP-YEARS TO WS-CNT-EDIT.
           DISPLAY '  LEAP YEARS SEEN   = ' WS-CNT-EDIT.
           DISPLAY '  LAST DAY OF WEEK  = ' WS-DAY-OF-WEEK.
       P9500-EXIT.
           EXIT.
