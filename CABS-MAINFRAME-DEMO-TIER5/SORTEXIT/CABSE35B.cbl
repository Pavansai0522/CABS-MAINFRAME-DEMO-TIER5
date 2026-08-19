       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABSE35B.
      *****************************************************************
      * CABSE35B - SORT E35 OUTPUT EXIT - SUMMARY ROUNDING AND        *
      *            FRACTIONAL CENT RESIDUE                            *
      * APPLICATION : CABS                                            *
      * COMPILER    : ENTERPRISE COBOL                                *
      * INVOKED BY  : OS SORT/MERGE, MODS=(E35=(CABSE35B,4096))       *
      *               ON CONTROL CARD MEMBER JCL/CTLCARDS/CABSRT07    *
      * INPUTS      : ONE 200 BYTE SUMMARY RECORD PER ENTRY, AFTER    *
      *               THE SUM FIELDS ON THE CONTROL CARD HAS ALREADY  *
      *               COLLAPSED THE DUPLICATE KEYS                    *
      * OUTPUTS     : THE SAME RECORD WITH THE SUMMED AMOUNT ROUNDED  *
      *               PLUS ONE RESIDUE ADJUSTMENT RECORD AT THE END   *
      * CONTROL     : NONE - EXITS DO NOT WRITE CTLOUT, CABS-STD-041  *
      * BALANCE     : RECORDS OUT = RECORDS IN + WS-RESIDUE-RECS      *
      *               SUM OF ROUNDED AMOUNTS + RESIDUE = SUM OF RAW   *
      * RESTART     : NOT RESTARTABLE - RERUN THE WHOLE SORT STEP     *
      *                                                               *
      * LINKAGE CONVENTION                                            *
      *   REGISTER 1 ADDRESSES A THREE WORD PARAMETER LIST.  WORD     *
      *   ONE IS THE ADDRESS OF THE RECORD LEAVING THE FINAL MERGE,   *
      *   OR BINARY ZERO WHEN THE MERGE IS EXHAUSTED.  WORD TWO IS    *
      *   THE ADDRESS OF THE PREVIOUSLY WRITTEN RECORD.  WORD THREE   *
      *   ADDRESSES THE LENGTH HALFWORD.  THE REPLY IS PLACED IN      *
      *   RETURN-CODE -                                               *
      *     00  NO MORE RECORDS TO INSERT - TAKE THE NEXT ONE         *
      *     04  DELETE THE RECORD                                     *
      *     08  WRITE THE RECORD ADDRESSED BY WORD ONE, THEN ENTER    *
      *         THIS EXIT AGAIN WITH THE SAME INPUT RECORD            *
      *     12  DO NOT ENTER THIS EXIT AGAIN                          *
      *     16  TERMINATE THE SORT                                    *
      *   TO INSERT A RECORD THE EXIT PLACES ITS OWN ADDRESS IN WORD  *
      *   ONE AND REPLIES 08.  SORT WRITES THAT RECORD AND RE-ENTERS  *
      *   THE EXIT, WHICH THEN REPLIES 00 TO RELEASE THE MERGE.       *
      *                                                               *
      * WHY THE ROUNDING IS DONE HERE                                 *
      *   THE SUM FIELDS ON CABSRT07 COLLAPSES THE RATE ELEMENT       *
      *   LINES TO A SINGLE LINE FOR EACH BAN AND EACH JURISDICTION.  *
      *   ELEMENT LINES CARRY FIVE DECIMAL PLACES BECAUSE SWITCHED    *
      *   ACCESS RATES DO.  IF EACH LINE WERE ROUNDED BEFORE THE SUM  *
      *   THE SUMMARY WOULD DRIFT FROM THE DETAIL BY UP TO HALF A     *
      *   CENT PER LINE.  ROUNDING AFTER THE SUM, HERE, IS THE ONLY   *
      *   PLACE THAT DRIFT CAN BE AVOIDED - SEE CABS-STD-019.  THE    *
      *   FRACTION LOST BY THE ROUNDING IS ACCUMULATED AND RELEASED   *
      *   AS ONE ADJUSTMENT RECORD, WHICH IS WHAT THE 1991 TARIFF     *
      *   REQUIRES.                                                   *
      *                                                               *
      * REVISION HISTORY                                              *
      *   V1.00  1991-07-22  R.T.WHEELER   INITIAL - ASSEMBLER F      *
      *   V1.02  1993-11-30  D.OKONKWO     HALF EVEN RULE ADDED       *
      *   V1.06  1998-04-14  J.M.CASTILLO  RESIDUE RECORD ADDED       *
      *   V2.00  2005-09-06  P.NAIR        RECODED IN COBOL FOR LE    *
      *   V2.02  2008-06-17  A.BUKOWSKI    WORK ORDINAL HONOURED ON   *
      *                                    THE RESIDUE ACCUMULATOR    *
      *   V2.05  2016-11-23  M.HAAS        ROUND UP ALWAYS RULE       *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-MODULE-IDENT.
           05  FILLER                  PIC X(08) VALUE 'CABSE35B'.
           05  FILLER                  PIC X(08) VALUE ' V2.05  '.
       01  WS-SWITCHES.
           05  WS-FIRST-ENTRY-SW       PIC X(01) VALUE 'Y'.
               88  WS-FIRST-ENTRY      VALUE 'Y'.
           05  WS-RESIDUE-SENT-SW      PIC X(01) VALUE 'N'.
               88  WS-RESIDUE-SENT     VALUE 'Y'.
           05  WS-MERGE-DONE-SW        PIC X(01) VALUE 'N'.
               88  WS-MERGE-DONE       VALUE 'Y'.
       01  WS-COUNTERS.
           05  WS-ENTRY-CNT            PIC S9(11) COMP-3 VALUE 0.
           05  WS-ROUNDED-CNT          PIC S9(11) COMP-3 VALUE 0.
           05  WS-EXACT-CNT            PIC S9(11) COMP-3 VALUE 0.
           05  WS-RESIDUE-RECS         PIC S9(05) COMP-3 VALUE 0.
           05  WS-BOUNDARY-CNT         PIC S9(05) COMP-3 VALUE 0.
       01  WS-ROUND-WORK.
           05  WS-RAW-AMT              PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-ROUNDED-AMT          PIC S9(13)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BACK-TO-FIVE         PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-RESIDUE              PIC S9(05)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-WORK-9               PIC S9(13)V9(05) VALUE 0.
           05  WS-WORK-9-R REDEFINES WS-WORK-9.
               10  WS-WK-SIGN-AREA     PIC 9(13).
               10  WS-WK-FRACTION      PIC 9(05).
           05  WS-WK-FRAC-R REDEFINES WS-WORK-9.
               10  FILLER              PIC 9(13).
               10  WS-WK-F1            PIC 9(01).
               10  WS-WK-F2            PIC 9(01).
               10  WS-WK-F3            PIC 9(01).
               10  WS-WK-F45           PIC 9(02).
           05  WS-LAST-CENT-DIGIT      PIC 9(01) VALUE 0.
           05  WS-NEG-SW               PIC X(01) VALUE 'N'.
               88  WS-NEGATIVE         VALUE 'Y'.
      *
      * THE RESIDUE ACCUMULATOR.  EVERY SUMMARY LINE GIVES UP A
      * FRACTION WHEN IT IS ROUNDED TO TWO PLACES.  THOSE FRACTIONS
      * ARE HELD HERE AND RELEASED AS ONE ADJUSTMENT RECORD AT THE
      * END OF THE MERGE SO THE INVOICE TOTAL STILL AGREES WITH THE
      * UNROUNDED DETAIL.
      *
       01  WS-RESIDUE-CONTROL.
           05  WS-RESIDUE-ACC          PIC S9(09)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-RESIDUE-LINES        PIC S9(09) COMP-3 VALUE 0.
           05  WS-RESIDUE-DISCARDED    PIC S9(09)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-LAST-WORK-ORD        PIC 9(02) VALUE 00.
           05  WS-LAST-OCN             PIC X(04) VALUE SPACES.
           05  WS-LAST-BAN             PIC X(13) VALUE SPACES.
           05  WS-LAST-PERIOD          PIC 9(06) VALUE 0.
           05  WS-LAST-JURIS           PIC X(01) VALUE SPACE.
      *
      * THE ADJUSTMENT RECORD RELEASED AT THE END OF THE MERGE.  IT
      * CARRIES THE RESERVED SECTION CODE 99 AND THE ELEMENT CODE
      * RESIDU SO THE BILL FORMATTER PRINTS IT ON THE ROUNDING LINE.
      *
       01  WS-RESIDUE-RECORD.
           05  WS-RR-REC-TYPE          PIC X(02) VALUE '09'.
           05  WS-RR-USAGE-TYPE        PIC X(01) VALUE 'A'.
           05  WS-RR-FILLER-1          PIC X(01) VALUE SPACE.
           05  WS-RR-OCN               PIC X(04) VALUE SPACES.
           05  WS-RR-BAN               PIC X(13) VALUE SPACES.
           05  WS-RR-SEQ               PIC 9(09) COMP-3 VALUE 999999999.
           05  WS-RR-FILLER-2          PIC X(09) VALUE SPACES.
           05  WS-RR-JURIS-CD          PIC X(01) VALUE SPACE.
           05  WS-RR-STATE-CD          PIC X(02) VALUE SPACES.
           05  WS-RR-SECTION           PIC X(02) VALUE '99'.
           05  WS-RR-FILLER-3          PIC X(05) VALUE SPACES.
           05  WS-RR-RATE-ELEM         PIC X(06) VALUE 'RESIDU'.
           05  WS-RR-LINE-CLASS        PIC X(01) VALUE 'R'.
           05  WS-RR-BILL-PERIOD       PIC 9(06) VALUE 0.
           05  WS-RR-FILLER-4          PIC X(13) VALUE SPACES.
           05  WS-RR-MINUTES           PIC S9(13)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-RR-AMOUNT            PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-RR-ROUNDED           PIC S9(13)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-RR-LINE-COUNT        PIC S9(09) COMP-3 VALUE 0.
           05  WS-RR-FILLER-5          PIC X(97) VALUE SPACES.
           05  WS-RR-CTL-PREFIX.
               10  WS-RC-RUN-STAMP     PIC 9(05) VALUE 0.
               10  WS-RC-WORK-ORD      PIC 9(02) VALUE 0.
               10  WS-RC-STRING-SEQ    PIC 9(04) VALUE 0.
               10  WS-RC-EXIT-VER      PIC X(01) VALUE 'B'.
      *
       LINKAGE SECTION.
       01  LK-PARM-LIST.
           05  LK-RECORD-PTR           POINTER.
           05  LK-PREV-PTR             POINTER.
           05  LK-LENGTH-PTR           POINTER.
       01  LK-SORT-RECORD.
           05  LK-SM-REC-TYPE          PIC X(02).
           05  LK-SM-USAGE-TYPE        PIC X(01).
           05  LK-SM-FILLER-1          PIC X(01).
           05  LK-SM-OCN               PIC X(04).
           05  LK-SM-BAN               PIC X(13).
           05  LK-SM-SEQ               PIC 9(09) COMP-3.
           05  LK-SM-FILLER-2          PIC X(09).
           05  LK-SM-JURIS-CD          PIC X(01).
           05  LK-SM-STATE-CD          PIC X(02).
           05  LK-SM-SECTION           PIC X(02).
           05  LK-SM-FILLER-3          PIC X(05).
           05  LK-SM-RATE-ELEM         PIC X(06).
           05  LK-SM-LINE-CLASS        PIC X(01).
           05  LK-SM-BILL-PERIOD       PIC 9(06).
           05  LK-SM-ROUND-RULE        PIC X(01).
               88  LK-SM-HALF-UP       VALUE 'U'.
               88  LK-SM-HALF-EVEN     VALUE 'E'.
               88  LK-SM-TRUNCATE      VALUE 'T'.
               88  LK-SM-UP-ALWAYS     VALUE 'C'.
           05  LK-SM-ROUND-POS         PIC 9(01).
           05  LK-SM-FILLER-4          PIC X(11).
           05  LK-SM-MINUTES           PIC S9(13)V9(02) COMP-3.
           05  LK-SM-AMOUNT            PIC S9(13)V9(05) COMP-3.
           05  LK-SM-ROUNDED           PIC S9(13)V9(02) COMP-3.
           05  LK-SM-FILLER-5          PIC X(101).
           05  LK-SM-CTL-PREFIX.
               10  LK-RC-RUN-STAMP     PIC 9(05).
               10  LK-RC-WORK-ORD      PIC 9(02).
               10  LK-RC-STRING-SEQ    PIC 9(04).
               10  LK-RC-EXIT-VER      PIC X(01).
      *
       PROCEDURE DIVISION USING LK-PARM-LIST.
       P0000-MAINLINE.
           MOVE ZERO TO RETURN-CODE.
           IF WS-FIRST-ENTRY
               PERFORM P1000-INIT THRU P1000-EXIT
           END-IF.
           IF LK-RECORD-PTR = NULL
               PERFORM P8000-END-OF-MERGE THRU P8000-EXIT
               GOBACK
           END-IF.
           SET ADDRESS OF LK-SORT-RECORD TO LK-RECORD-PTR.
           ADD 1 TO WS-ENTRY-CNT.
           PERFORM P2000-WORK-BOUNDARY THRU P2000-EXIT.
           PERFORM P3000-APPLY-ROUNDING THRU P3000-EXIT.
           PERFORM P4000-ACCUM-RESIDUE THRU P4000-EXIT.
           PERFORM P5000-HOLD-CONTEXT THRU P5000-EXIT.
           MOVE 8 TO RETURN-CODE.
           GOBACK.

       P1000-INIT.
           MOVE 'N' TO WS-FIRST-ENTRY-SW.
           MOVE ZERO TO WS-RESIDUE-ACC.
           MOVE ZERO TO WS-RESIDUE-LINES.
           MOVE 00 TO WS-LAST-WORK-ORD.
           DISPLAY 'CABSE35B ENTERED - SUMMARY ROUNDING'.

       P1000-EXIT.
           EXIT.

       P2000-WORK-BOUNDARY.
      * THE RATING CONTROL PREFIX CARRIES THE ORDINAL OF THE SORT
      * WORK DATA SET THE RECORD CAME OUT OF.  A CHANGE OF ORDINAL
      * MEANS THE MERGE HAS MOVED ON TO THE NEXT WORK DATA SET.
      * THE ACCUMULATOR IS RESTARTED AT THAT POINT.  THE MERGE
      * RE-PRESENTS THE HIGH KEY OF THE PREVIOUS STRING AS THE LOW
      * KEY OF THE NEXT ONE, AND WITHOUT THE RESTART THE FRACTION
      * ON THAT KEY WOULD BE COUNTED IN BOTH STRINGS AND THE
      * ADJUSTMENT WOULD BE OVERSTATED.  ADDED 2008 AFTER THE
      * MARCH CYCLE OVERSTATED THE ROUNDING LINE.
           IF LK-RC-WORK-ORD = WS-LAST-WORK-ORD
               GO TO P2000-EXIT
           END-IF.
           IF WS-LAST-WORK-ORD = 00
               MOVE LK-RC-WORK-ORD TO WS-LAST-WORK-ORD
               GO TO P2000-EXIT
           END-IF.
           ADD 1 TO WS-BOUNDARY-CNT.
           PERFORM P2400-RESTART-RESIDUE THRU P2400-EXIT.
           MOVE LK-RC-WORK-ORD TO WS-LAST-WORK-ORD.

       P2000-EXIT.
           EXIT.

       P2400-RESTART-RESIDUE.
      * RESTART THE ACCUMULATOR FOR THE NEW WORK DATA SET.  THE
      * VALUE HELD SO FAR IS KEPT IN A SEPARATE FIELD FOR THE
      * MESSAGE DATA SET ONLY.
           ADD WS-RESIDUE-ACC TO WS-RESIDUE-DISCARDED.
           MOVE ZERO TO WS-RESIDUE-ACC.
           MOVE ZERO TO WS-RESIDUE-LINES.

       P2400-EXIT.
           EXIT.

       P3000-APPLY-ROUNDING.
      * THE RULE COMES OFF THE RECORD, NOT OFF A GLOBAL SETTING.
      * CABRAT09 CARRIES THE RATE TABLE RULE FORWARD ONTO EVERY
      * SUMMARY LINE FOR EXACTLY THIS PURPOSE.
           MOVE LK-SM-AMOUNT TO WS-RAW-AMT.
           MOVE 'N' TO WS-NEG-SW.
           IF WS-RAW-AMT < ZERO
               MOVE 'Y' TO WS-NEG-SW
               COMPUTE WS-RAW-AMT = WS-RAW-AMT * -1
           END-IF.
           MOVE WS-RAW-AMT TO WS-WORK-9.
           MOVE WS-WK-F3 TO WS-LAST-CENT-DIGIT.
           IF LK-SM-TRUNCATE
               PERFORM P3300-TRUNCATE THRU P3300-EXIT
               GO TO P3000-STORE
           END-IF.
           IF LK-SM-UP-ALWAYS
               PERFORM P3400-UP-ALWAYS THRU P3400-EXIT
               GO TO P3000-STORE
           END-IF.
           IF LK-SM-HALF-EVEN
               PERFORM P3200-HALF-EVEN THRU P3200-EXIT
               GO TO P3000-STORE
           END-IF.
           PERFORM P3100-HALF-UP THRU P3100-EXIT.

       P3000-STORE.
           IF WS-NEGATIVE
               COMPUTE WS-ROUNDED-AMT = WS-ROUNDED-AMT * -1
               COMPUTE WS-RAW-AMT = WS-RAW-AMT * -1
           END-IF.
           MOVE WS-ROUNDED-AMT TO LK-SM-ROUNDED.
           COMPUTE WS-BACK-TO-FIVE = WS-ROUNDED-AMT.
           COMPUTE WS-RESIDUE = WS-RAW-AMT - WS-BACK-TO-FIVE.
           IF WS-RESIDUE = ZERO
               ADD 1 TO WS-EXACT-CNT
           ELSE
               ADD 1 TO WS-ROUNDED-CNT
           END-IF.

       P3000-EXIT.
           EXIT.

       P3100-HALF-UP.
      * THE COMMON CASE.  A THIRD DECIMAL OF FIVE OR MORE CARRIES
      * INTO THE CENT.
           COMPUTE WS-ROUNDED-AMT ROUNDED = WS-RAW-AMT.

       P3100-EXIT.
           EXIT.

       P3200-HALF-EVEN.
      * BANKERS ROUNDING.  APPLIED ONLY WHERE THE THIRD DECIMAL IS
      * EXACTLY FIVE AND NOTHING FOLLOWS IT; EVERY OTHER CASE FALLS
      * BACK TO HALF UP.  REQUIRED BY THE 1993 INTERCONNECTION
      * AGREEMENTS FOR THE THREE LARGEST COUNTERPARTIES.
           IF WS-WK-F3 NOT = 5
               COMPUTE WS-ROUNDED-AMT ROUNDED = WS-RAW-AMT
               GO TO P3200-EXIT
           END-IF.
           IF WS-WK-F45 NOT = ZERO
               COMPUTE WS-ROUNDED-AMT ROUNDED = WS-RAW-AMT
               GO TO P3200-EXIT
           END-IF.
           MOVE WS-WK-F2 TO WS-LAST-CENT-DIGIT.
           DIVIDE WS-LAST-CENT-DIGIT BY 2
                  GIVING WS-LAST-CENT-DIGIT
                  REMAINDER WS-LAST-CENT-DIGIT.
           IF WS-LAST-CENT-DIGIT = ZERO
               COMPUTE WS-ROUNDED-AMT = WS-RAW-AMT
           ELSE
               COMPUTE WS-ROUNDED-AMT ROUNDED = WS-RAW-AMT
           END-IF.

       P3200-EXIT.
           EXIT.

       P3300-TRUNCATE.
      * DROP EVERYTHING BEYOND THE CENT.  THE WHOLE FRACTION GOES
      * TO THE ACCUMULATOR.
           COMPUTE WS-ROUNDED-AMT = WS-RAW-AMT.

       P3300-EXIT.
           EXIT.

       P3400-UP-ALWAYS.
      * ALWAYS ROUND AWAY FROM ZERO.  USED ON THE TWO SPECIAL
      * ACCESS TARIFFS THAT REQUIRE THE CARRIER TO BE CHARGED THE
      * NEXT WHOLE CENT.  ADDED 2016.
           COMPUTE WS-ROUNDED-AMT = WS-RAW-AMT.
           MOVE WS-RAW-AMT TO WS-WORK-9.
           IF WS-WK-F3 NOT = ZERO OR WS-WK-F45 NOT = ZERO
               COMPUTE WS-ROUNDED-AMT = WS-ROUNDED-AMT + 0.01
           END-IF.

       P3400-EXIT.
           EXIT.

       P4000-ACCUM-RESIDUE.
      * ADD THE FRACTION GIVEN UP BY THIS LINE.  ACCUMULATION IS IN
      * THE ORDER THE RECORDS ARRIVE - NOTHING IS RESEQUENCED.
           IF WS-RESIDUE = ZERO
               GO TO P4000-EXIT
           END-IF.
           ADD WS-RESIDUE TO WS-RESIDUE-ACC.
           ADD 1 TO WS-RESIDUE-LINES.

       P4000-EXIT.
           EXIT.

       P5000-HOLD-CONTEXT.
      * THE ADJUSTMENT RECORD HAS TO CARRY A KEY.  IT IS GIVEN THE
      * KEY OF THE LAST SUMMARY LINE SEEN, WHICH PUTS IT ON THE
      * LAST ACCOUNT PROCESSED.
           MOVE LK-SM-OCN         TO WS-LAST-OCN.
           MOVE LK-SM-BAN         TO WS-LAST-BAN.
           MOVE LK-SM-BILL-PERIOD TO WS-LAST-PERIOD.
           MOVE LK-SM-JURIS-CD    TO WS-LAST-JURIS.

       P5000-EXIT.
           EXIT.

       P8000-END-OF-MERGE.
      * THE MERGE IS EXHAUSTED.  RELEASE THE ACCUMULATED RESIDUE AS
      * ONE ADJUSTMENT RECORD, THEN REPLY ZERO ON THE FOLLOWING
      * ENTRY SO SORT CAN CLOSE SORTOUT.
           MOVE 'Y' TO WS-MERGE-DONE-SW.
           IF WS-RESIDUE-SENT
               PERFORM P8600-REPORT THRU P8600-EXIT
               MOVE ZERO TO RETURN-CODE
               GO TO P8000-EXIT
           END-IF.
           IF WS-RESIDUE-ACC = ZERO
               MOVE 'Y' TO WS-RESIDUE-SENT-SW
               PERFORM P8600-REPORT THRU P8600-EXIT
               MOVE ZERO TO RETURN-CODE
               GO TO P8000-EXIT
           END-IF.
           PERFORM P8200-BUILD-RESIDUE THRU P8200-EXIT.
           MOVE 'Y' TO WS-RESIDUE-SENT-SW.
           SET LK-RECORD-PTR TO ADDRESS OF WS-RESIDUE-RECORD.
           MOVE 8 TO RETURN-CODE.

       P8000-EXIT.
           EXIT.

       P8200-BUILD-RESIDUE.
      * THE ADJUSTMENT IS ROUNDED TO THE CENT ON THE WAY OUT.  WHAT
      * IS LEFT AFTER THAT ROUNDING IS BELOW HALF A CENT ON THE
      * WHOLE RUN AND IS NOT CARRIED FORWARD.
           MOVE WS-LAST-OCN      TO WS-RR-OCN.
           MOVE WS-LAST-BAN      TO WS-RR-BAN.
           MOVE WS-LAST-PERIOD   TO WS-RR-BILL-PERIOD.
           MOVE WS-LAST-JURIS    TO WS-RR-JURIS-CD.
           MOVE WS-RESIDUE-ACC   TO WS-RR-AMOUNT.
           COMPUTE WS-RR-ROUNDED ROUNDED = WS-RESIDUE-ACC.
           MOVE WS-RESIDUE-LINES TO WS-RR-LINE-COUNT.
           MOVE ZERO             TO WS-RR-MINUTES.
           MOVE WS-LAST-WORK-ORD TO WS-RC-WORK-ORD.
           ADD 1 TO WS-RESIDUE-RECS.

       P8200-EXIT.
           EXIT.

       P8600-REPORT.
           DISPLAY 'CABSE35B ENTRIES     ' WS-ENTRY-CNT.
           DISPLAY 'CABSE35B ROUNDED     ' WS-ROUNDED-CNT.
           DISPLAY 'CABSE35B EXACT       ' WS-EXACT-CNT.
           DISPLAY 'CABSE35B RESIDUE ACC ' WS-RESIDUE-ACC.
           DISPLAY 'CABSE35B RESIDUE REC ' WS-RESIDUE-RECS.
           DISPLAY 'CABSE35B BOUNDARIES  ' WS-BOUNDARY-CNT.
           DISPLAY 'CABSE35B PRIOR STRNG ' WS-RESIDUE-DISCARDED.

       P8600-EXIT.
           EXIT.
