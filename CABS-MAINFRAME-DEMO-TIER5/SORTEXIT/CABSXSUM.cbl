       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABSXSUM.
      *****************************************************************
      * CABSXSUM - SORT E35 OUTPUT EXIT - SUMMARY CONTROL BREAK,      *
      *            ROUNDING AND FRACTIONAL CENT RESIDUE               *
      * APPLICATION : CABS                                            *
      * COMPILER    : ENTERPRISE COBOL                                *
      * INVOKED BY  : OS SORT/MERGE, MODS STATEMENT ON CONTROL CARD   *
      *               MEMBER JCL/CTLCARDS/MVT/CABSRT07 -              *
      *               E35=(CABSXSUM,8192,SORTEXIT,N)                  *
      * INPUTS      : ONE 200 BYTE SUMMARY RECORD PER ENTRY, IN       *
      *               OCN, BAN AND JURISDICTION ORDER                 *
      * OUTPUTS     : ONE RECORD PER CONTROL GROUP PLUS ONE RESIDUE   *
      *               ADJUSTMENT RECORD AT END OF MERGE               *
      * CONTROL     : NONE - EXITS DO NOT WRITE CTLOUT, CABS-STD-041  *
      * BALANCE     : SUM OF ROUNDED AMOUNTS + RESIDUE = SUM OF RAW   *
      * RESTART     : NOT RESTARTABLE - RERUN THE WHOLE SORT STEP     *
      *                                                               *
      * LINKAGE CONVENTION                                            *
      *   REGISTER 1 ADDRESSES A THREE WORD PARAMETER LIST - THE      *
      *   RECORD LEAVING THE FINAL MERGE OR BINARY ZERO AT THE END,   *
      *   THE RECORD LAST WRITTEN, AND THE LENGTH HALFWORD.           *
      *   RETURN-CODE CARRIES THE REPLY - 00 TAKE THE NEXT, 04        *
      *   DELETE, 08 WRITE THE RECORD ADDRESSED BY WORD ONE AND       *
      *   ENTER AGAIN WITH THE SAME INPUT, 12 DO NOT ENTER AGAIN,     *
      *   16 TERMINATE.  WORKING STORAGE PERSISTS.                    *
      *                                                               *
      * WHAT THIS EXIT DECIDES                                        *
      *   TWO RULES MEET HERE.  A CONTROL CARD CAN NAME ONE E35       *
      *   ONLY, SO THE SUMMARISATION THAT SAT ON THE CARD AND THE     *
      *   ROUNDING THAT SAT IN CABSE35B ARE BOTH CARRIED HERE.        *
      *   RECORDS SHARING AN OCN, A BAN AND A JURISDICTION            *
      *   COLLAPSE INTO ONE LINE AND THEIR PACKED AMOUNT AT           *
      *   COLUMN 160, SEVEN BYTES, IS ADDED.  THE RATE                *
      *   TABLE ROUND RULE IS THEN APPLIED TO THE GROUP TOTAL,        *
      *   AFTER THE ADDITION AND NOT BEFORE IT - ROUNDING EACH        *
      *   LINE FIRST WOULD LET THE SUMMARY DRIFT FROM THE DETAIL      *
      *   BY HALF A CENT A LINE, SEE CABS-STD-019.  THE RULES ARE     *
      *   H HALF UP, E HALF EVEN, T TRUNCATE AND U UP ALWAYS; A       *
      *   BLANK BYTE IS TREATED AS HALF UP.  THE FRACTION GIVEN UP    *
      *   IS RELEASED AS ONE ADJUSTMENT RECORD AT END OF MERGE,       *
      *   AS THE 1991 TARIFF REQUIRES.                                *
      *                                                               *
      * REVISION HISTORY                                              *
      *   V1.00  1987-11-16  R.T.WHEELER   INITIAL - ASSEMBLER F      *
      *   V1.02  1993-11-30  D.OKONKWO     HALF EVEN RULE ADDED       *
      *   V2.00  2005-09-06  P.NAIR        RECODED IN COBOL FOR LE    *
      *   V2.05  2016-11-23  M.HAAS        ROUND UP ALWAYS RULE       *
      *   V2.07  2019-02-11  J.CALLAGHAN   SUMMARISATION JOINED       *
      *                                    TO THE ROUNDING HERE       *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
      * THIS EXIT WRITES NO CONTROL RECORD.  THE BALANCE OF
      * CABRAT09, WHICH READS SORTOUT, IS UNAFFECTED BY THIS
      * MODULE - IT REPORTS WHAT IT READS AS ITS OWN CT-READ.
       01  WS-MODULE-IDENT.
           05  FILLER              PIC X(16) VALUE 'CABSXSUM V2.07  '.
       01  WS-COUNTERS.
           05  WS-ENTRY-CNT            PIC S9(11) COMP-3 VALUE 0.
           05  WS-GROUP-CNT            PIC S9(11) COMP-3 VALUE 0.
           05  WS-ABSORBED-CNT         PIC S9(11) COMP-3 VALUE 0.
           05  WS-ROUNDED-CNT          PIC S9(11) COMP-3 VALUE 0.
           05  WS-EXACT-CNT            PIC S9(11) COMP-3 VALUE 0.
           05  WS-RULE-OTHER           PIC S9(09) COMP-3 VALUE 0.
           05  WS-RESIDUE-RECS         PIC S9(05) COMP-3 VALUE 0.
           05  WS-AMT-IN               PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-AMT-OUT              PIC S9(15)V9(02) COMP-3 VALUE 0.
       01  WS-SWITCHES.
           05  WS-FIRST-ENTRY-SW       PIC X(01) VALUE 'Y'.
               88  WS-FIRST-ENTRY      VALUE 'Y'.
           05  WS-GROUP-OPEN-SW        PIC X(01) VALUE 'N'.
               88  WS-GROUP-OPEN       VALUE 'Y'.
           05  WS-PENDING-SW           PIC X(01) VALUE 'N'.
               88  WS-PENDING          VALUE 'Y'.
           05  WS-MERGE-DONE-SW        PIC X(01) VALUE 'N'.
               88  WS-MERGE-DONE       VALUE 'Y'.
           05  WS-NEG-SW               PIC X(01) VALUE 'N'.
               88  WS-NEGATIVE         VALUE 'Y'.
       01  WS-BREAK-CONTROL.
           05  WS-EOM-STATE            PIC 9(01) VALUE 0.
           05  WS-GRP-AMT              PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-GRP-MIN              PIC S9(11)V9(02) COMP-3 VALUE 0.
           05  WS-HOLD-KEY.
               10  WS-HK-OCN           PIC X(04) VALUE SPACES.
               10  WS-HK-BAN           PIC X(13) VALUE SPACES.
               10  WS-HK-JURIS         PIC X(01) VALUE SPACE.
           05  WS-THIS-KEY.
               10  WS-TK-OCN           PIC X(04) VALUE SPACES.
               10  WS-TK-BAN           PIC X(13) VALUE SPACES.
               10  WS-TK-JURIS         PIC X(01) VALUE SPACE.
           05  WS-NEXT-KEY             PIC X(18) VALUE SPACES.
      *
      * THE ROUND RULE FOR THE OPEN GROUP, TAKEN FROM ITS FIRST
      * LINE.  CABRAT09 CARRIES THE RATE TABLE RULE ONTO EVERY
      * LINE, SO THE LINES OF ONE GROUP NORMALLY AGREE.
       01  WS-ROUND-CONTROL.
           05  WS-GRP-RULE             PIC X(01) VALUE SPACE.
               88  WS-RULE-HALF-UP     VALUE 'H'.
               88  WS-RULE-HALF-EVEN   VALUE 'E'.
               88  WS-RULE-TRUNCATE    VALUE 'T'.
               88  WS-RULE-UP-ALWAYS   VALUE 'U'.
       01  WS-ROUND-WORK.
           05  WS-RAW-AMT              PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-ROUNDED-AMT          PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-RESIDUE              PIC S9(05)V9(05) COMP-3 VALUE 0.
           05  WS-WORK-9               PIC S9(13)V9(05) VALUE 0.
           05  WS-WK-FRAC-R REDEFINES WS-WORK-9.
               10  FILLER              PIC 9(14).
               10  WS-WK-F2            PIC 9(01).
               10  WS-WK-F3            PIC 9(01).
               10  WS-WK-F45           PIC 9(02).
           05  WS-CENT-DIGIT           PIC 9(01) VALUE 0.
       01  WS-RESIDUE-CONTROL.
           05  WS-RESIDUE-ACC          PIC S9(09)V9(05) COMP-3 VALUE 0.
           05  WS-RESIDUE-LINES        PIC S9(09) COMP-3 VALUE 0.
           05  WS-LAST-OCN             PIC X(04) VALUE SPACES.
           05  WS-LAST-BAN             PIC X(13) VALUE SPACES.
           05  WS-LAST-JURIS           PIC X(01) VALUE SPACE.
           05  WS-LAST-PERIOD          PIC 9(06) VALUE 0.
      *
      * THE HELD GROUP IMAGE AND A COPY OF THE RECORD THAT CAUSED
      * THE BREAK.  A REPLY OF EIGHT MAKES SORT WRITE THE IMAGE
      * AND ENTER AGAIN, SO THE BREAKING RECORD IS COPIED FIRST.
       01  WS-HOLD-RECORD.
           05  WS-HD-LEAD-1            PIC X(46).
           05  WS-HD-RULE              PIC X(01).
           05  WS-HD-LEAD-2            PIC X(105).
           05  WS-HD-MINUTES           PIC S9(11)V9(02) COMP-3.
           05  WS-HD-AMOUNT            PIC S9(08)V9(05) COMP-3.
           05  WS-HD-ROUNDED           PIC S9(11)V9(02) COMP-3.
           05  WS-HD-TAIL              PIC X(27).
       01  WS-NEXT-RECORD              PIC X(200).
      *
      * THE ADJUSTMENT RECORD.  IT CARRIES SECTION CODE 99 AND
      * ELEMENT CODE RESIDU FOR THE BILL FORMATTER.
       01  WS-RESIDUE-RECORD.
           05  FILLER                  PIC X(04) VALUE '09A '.
           05  WS-RR-OCN               PIC X(04) VALUE SPACES.
           05  WS-RR-BAN               PIC X(13) VALUE SPACES.
           05  FILLER    PIC 9(09) COMP-3 VALUE 999999999.
           05  FILLER                  PIC X(13) VALUE 'RESIDU'.
           05  WS-RR-JURIS-CD          PIC X(01) VALUE SPACE.
           05  WS-RR-BILL-PERIOD       PIC 9(06) VALUE 0.
           05  FILLER                  PIC X(05) VALUE 'T299 '.
           05  FILLER                  PIC X(101) VALUE SPACES.
           05  WS-RR-MINUTES           PIC S9(11)V9(02) COMP-3 VALUE 0.
           05  WS-RR-AMOUNT            PIC S9(08)V9(05) COMP-3 VALUE 0.
           05  WS-RR-ROUNDED           PIC S9(11)V9(02) COMP-3 VALUE 0.
           05  WS-RR-LINE-COUNT        PIC S9(09) COMP-3 VALUE 0.
           05  FILLER                  PIC X(22) VALUE
                                       '                     S'.
       LINKAGE SECTION.
       01  LK-PARM-LIST.
           05  LK-RECORD-PTR           POINTER.
           05  LK-PREV-PTR             POINTER.
           05  LK-LENGTH-PTR           POINTER.
       01  LK-SORT-RECORD.
           05  LK-SM-REC-TYPE          PIC X(02).
           05  LK-SM-FILLER-1          PIC X(02).
           05  LK-SM-OCN               PIC X(04).
           05  LK-SM-BAN               PIC X(13).
           05  LK-SM-SEQ               PIC 9(09) COMP-3.
           05  LK-SM-FILLER-2          PIC X(13).
           05  LK-SM-JURIS-CD          PIC X(01).
           05  LK-SM-BILL-PERIOD       PIC 9(06).
           05  LK-SM-ROUND-RULE        PIC X(01).
           05  LK-SM-FILLER-3          PIC X(04).
           05  LK-SM-FILLER-4          PIC X(101).
           05  LK-SM-MINUTES           PIC S9(11)V9(02) COMP-3.
           05  LK-SM-AMOUNT            PIC S9(08)V9(05) COMP-3.
           05  LK-SM-ROUNDED           PIC S9(11)V9(02) COMP-3.
           05  LK-SM-FILLER-5          PIC X(15).
           05  LK-SM-CTL-PREFIX        PIC X(12).
       PROCEDURE DIVISION USING LK-PARM-LIST.
       P0000-MAINLINE.
           MOVE ZERO TO RETURN-CODE.
           IF WS-FIRST-ENTRY
               PERFORM P1000-INIT THRU P1000-EXIT
           END-IF.
           IF WS-MERGE-DONE
               PERFORM P8000-END-OF-MERGE THRU P8000-EXIT
               GOBACK
           END-IF.
           IF WS-PENDING
               PERFORM P7000-RESUME-BREAK THRU P7000-EXIT
               GOBACK
           END-IF.
           IF LK-RECORD-PTR = NULL
               MOVE 'Y' TO WS-MERGE-DONE-SW
               PERFORM P8000-END-OF-MERGE THRU P8000-EXIT
               GOBACK
           END-IF.
           SET ADDRESS OF LK-SORT-RECORD TO LK-RECORD-PTR.
           ADD 1 TO WS-ENTRY-CNT.
           PERFORM P3000-TEST-BREAK THRU P3000-EXIT.
           GOBACK.

       P1000-INIT.
           MOVE 'N' TO WS-FIRST-ENTRY-SW.
           DISPLAY 'CABSXSUM ENTERED - SUMMARY AND ROUNDING'.

       P1000-EXIT.
           EXIT.

       P3000-TEST-BREAK.
      * THE CONTROL GROUP IS OCN, BAN AND JURISDICTION.  THE RATE
      * ELEMENT IS NOT PART OF IT, SO RATED LINES COLLAPSE.
           MOVE LK-SM-OCN      TO WS-TK-OCN.
           MOVE LK-SM-BAN      TO WS-TK-BAN.
           MOVE LK-SM-JURIS-CD TO WS-TK-JURIS.
           ADD LK-SM-AMOUNT TO WS-AMT-IN.
           IF NOT WS-GROUP-OPEN
               MOVE LK-SORT-RECORD   TO WS-HOLD-RECORD
               MOVE WS-THIS-KEY      TO WS-HOLD-KEY
               MOVE LK-SM-AMOUNT     TO WS-GRP-AMT
               MOVE LK-SM-MINUTES    TO WS-GRP-MIN
               MOVE LK-SM-ROUND-RULE TO WS-GRP-RULE
               MOVE 'Y' TO WS-GROUP-OPEN-SW
               MOVE 4 TO RETURN-CODE
               GO TO P3000-EXIT
           END-IF.
           IF WS-THIS-KEY = WS-HOLD-KEY
               ADD LK-SM-AMOUNT  TO WS-GRP-AMT
               ADD LK-SM-MINUTES TO WS-GRP-MIN
               IF LK-SM-ROUND-RULE NOT = WS-GRP-RULE
                   ADD 1 TO WS-RULE-OTHER
               END-IF
               ADD 1 TO WS-ABSORBED-CNT
               MOVE 4 TO RETURN-CODE
               GO TO P3000-EXIT
           END-IF.
           MOVE LK-SORT-RECORD TO WS-NEXT-RECORD.
           MOVE WS-THIS-KEY    TO WS-NEXT-KEY.
           MOVE 'Y' TO WS-PENDING-SW.
           PERFORM P5000-RELEASE-GROUP THRU P5000-EXIT.

       P3000-EXIT.
           EXIT.

       P5000-RELEASE-GROUP.
      * ROUND THE GROUP TOTAL, TAKE THE FRACTION IT GIVES UP INTO
      * THE ACCUMULATOR AND HAND THE IMAGE TO SORT.
           PERFORM P6000-APPLY-ROUNDING THRU P6000-EXIT.
           IF WS-RESIDUE NOT = ZERO
               ADD WS-RESIDUE TO WS-RESIDUE-ACC
               ADD 1 TO WS-RESIDUE-LINES
           END-IF.
           MOVE WS-GRP-MIN     TO WS-HD-MINUTES.
           MOVE WS-GRP-AMT     TO WS-HD-AMOUNT.
           MOVE WS-ROUNDED-AMT TO WS-HD-ROUNDED.
           ADD WS-ROUNDED-AMT TO WS-AMT-OUT.
           MOVE WS-HK-OCN         TO WS-LAST-OCN.
           MOVE WS-HK-BAN         TO WS-LAST-BAN.
           MOVE WS-HK-JURIS       TO WS-LAST-JURIS.
           MOVE LK-SM-BILL-PERIOD TO WS-LAST-PERIOD.
           ADD 1 TO WS-GROUP-CNT.
           SET LK-RECORD-PTR TO ADDRESS OF WS-HOLD-RECORD.
           MOVE 8 TO RETURN-CODE.

       P5000-EXIT.
           EXIT.

       P6000-APPLY-ROUNDING.
      * THE RULE COMES OFF THE RECORD, NOT A GLOBAL SETTING, AND
      * THE ARITHMETIC IS DONE ON THE POSITIVE VALUE.
           MOVE WS-GRP-AMT TO WS-RAW-AMT.
           MOVE 'N' TO WS-NEG-SW.
           IF WS-RAW-AMT < ZERO
               MOVE 'Y' TO WS-NEG-SW
               COMPUTE WS-RAW-AMT = WS-RAW-AMT * -1
           END-IF.
           MOVE WS-RAW-AMT TO WS-WORK-9.
           EVALUATE TRUE
               WHEN WS-RULE-TRUNCATE
      *            THE WHOLE FRACTION GOES TO THE ACCUMULATOR
                   COMPUTE WS-ROUNDED-AMT = WS-RAW-AMT
               WHEN WS-RULE-UP-ALWAYS
      *            AWAY FROM ZERO, FOR THE TWO SPECIAL ACCESS
      *            TARIFFS THAT CHARGE THE NEXT WHOLE CENT
                   COMPUTE WS-ROUNDED-AMT = WS-RAW-AMT
                   IF WS-WK-F3 NOT = ZERO OR WS-WK-F45 NOT = ZERO
                       COMPUTE WS-ROUNDED-AMT =
                               WS-ROUNDED-AMT + 0.01
                   END-IF
               WHEN WS-RULE-HALF-EVEN
                   PERFORM P6200-HALF-EVEN THRU P6200-EXIT
               WHEN OTHER
      *            H AND BLANK BOTH CARRY A THIRD DECIMAL OF
      *            FIVE OR MORE INTO THE CENT
                   COMPUTE WS-ROUNDED-AMT ROUNDED = WS-RAW-AMT
           END-EVALUATE.
           IF WS-NEGATIVE
               COMPUTE WS-ROUNDED-AMT = WS-ROUNDED-AMT * -1
               COMPUTE WS-RAW-AMT = WS-RAW-AMT * -1
           END-IF.
           COMPUTE WS-RESIDUE = WS-RAW-AMT - WS-ROUNDED-AMT.
           IF WS-RESIDUE = ZERO
               ADD 1 TO WS-EXACT-CNT
           ELSE
               ADD 1 TO WS-ROUNDED-CNT
           END-IF.

       P6000-EXIT.
           EXIT.

       P6200-HALF-EVEN.
      * BANKERS ROUNDING.  APPLIED ONLY WHERE THE THIRD DECIMAL
      * IS FIVE AND NOTHING FOLLOWS IT; EVERY OTHER CASE FALLS
      * BACK TO HALF UP.  THE 1993 AGREEMENTS REQUIRE IT.
           IF WS-WK-F3 NOT = 5 OR WS-WK-F45 NOT = ZERO
               COMPUTE WS-ROUNDED-AMT ROUNDED = WS-RAW-AMT
               GO TO P6200-EXIT
           END-IF.
           MOVE WS-WK-F2 TO WS-CENT-DIGIT.
           DIVIDE WS-CENT-DIGIT BY 2
                  GIVING WS-CENT-DIGIT
                  REMAINDER WS-CENT-DIGIT.
           IF WS-CENT-DIGIT = ZERO
               COMPUTE WS-ROUNDED-AMT = WS-RAW-AMT
           ELSE
               COMPUTE WS-ROUNDED-AMT ROUNDED = WS-RAW-AMT
           END-IF.

       P6200-EXIT.
           EXIT.

       P7000-RESUME-BREAK.
      * SORT HAS WRITTEN THE IMAGE AND HAS ENTERED AGAIN WITH THE
      * SAME INPUT.  THE NEW GROUP IS OPENED FROM THE COPY AND
      * THAT RECORD IS NOT COUNTED AGAIN.
           MOVE 'N' TO WS-PENDING-SW.
           MOVE WS-NEXT-RECORD TO WS-HOLD-RECORD.
           MOVE WS-NEXT-KEY    TO WS-HOLD-KEY.
           MOVE WS-HD-AMOUNT   TO WS-GRP-AMT.
           MOVE WS-HD-MINUTES  TO WS-GRP-MIN.
           MOVE WS-HD-RULE     TO WS-GRP-RULE.
           MOVE 'Y' TO WS-GROUP-OPEN-SW.
           MOVE 4 TO RETURN-CODE.

       P7000-EXIT.
           EXIT.

       P8000-END-OF-MERGE.
      * THE MERGE IS EXHAUSTED.  THE LAST GROUP IS RELEASED FIRST,
      * THE RESIDUE NEXT, AND ONLY THEN IS ZERO REPLIED.
           IF WS-EOM-STATE = 0
               MOVE 1 TO WS-EOM-STATE
               IF WS-GROUP-OPEN
                   MOVE 'N' TO WS-GROUP-OPEN-SW
                   PERFORM P5000-RELEASE-GROUP THRU P5000-EXIT
                   GO TO P8000-EXIT
               END-IF
           END-IF.
           IF WS-EOM-STATE < 2
               MOVE 2 TO WS-EOM-STATE
               IF WS-RESIDUE-ACC NOT = ZERO
                   MOVE WS-LAST-OCN      TO WS-RR-OCN
                   MOVE WS-LAST-BAN      TO WS-RR-BAN
                   MOVE WS-LAST-JURIS    TO WS-RR-JURIS-CD
                   MOVE WS-LAST-PERIOD   TO WS-RR-BILL-PERIOD
                   MOVE WS-RESIDUE-ACC   TO WS-RR-AMOUNT
                   COMPUTE WS-RR-ROUNDED ROUNDED = WS-RESIDUE-ACC
                   MOVE WS-RESIDUE-LINES TO WS-RR-LINE-COUNT
                   MOVE ZERO             TO WS-RR-MINUTES
                   ADD WS-RR-ROUNDED TO WS-AMT-OUT
                   ADD 1 TO WS-RESIDUE-RECS
                   SET LK-RECORD-PTR TO
                       ADDRESS OF WS-RESIDUE-RECORD
                   MOVE 8 TO RETURN-CODE
                   GO TO P8000-EXIT
               END-IF
           END-IF.
           MOVE 3 TO WS-EOM-STATE.
           PERFORM P8600-REPORT THRU P8600-EXIT.
           MOVE ZERO TO RETURN-CODE.

       P8000-EXIT.
           EXIT.

       P8600-REPORT.
      * AMOUNT IN AND AMOUNT OUT DIFFER BY THE FRACTION LEFT
      * BELOW HALF A CENT ON THE WHOLE RUN.
           DISPLAY 'CABSXSUM ENTRIES     ' WS-ENTRY-CNT.
           DISPLAY 'CABSXSUM GROUPS OUT  ' WS-GROUP-CNT.
           DISPLAY 'CABSXSUM ABSORBED    ' WS-ABSORBED-CNT.
           DISPLAY 'CABSXSUM ROUNDED     ' WS-ROUNDED-CNT.
           DISPLAY 'CABSXSUM EXACT       ' WS-EXACT-CNT.
           DISPLAY 'CABSXSUM RESIDUE ACC ' WS-RESIDUE-ACC.
           DISPLAY 'CABSXSUM RULE OTHER  ' WS-RULE-OTHER.
           DISPLAY 'CABSXSUM RESIDUE REC ' WS-RESIDUE-RECS.
           DISPLAY 'CABSXSUM AMOUNT IN   ' WS-AMT-IN.
           DISPLAY 'CABSXSUM AMOUNT OUT  ' WS-AMT-OUT.

       P8600-EXIT.
           EXIT.
