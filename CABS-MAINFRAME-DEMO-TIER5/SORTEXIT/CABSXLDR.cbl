       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABSXLDR.
      *****************************************************************
      * CABSXLDR - SORT E35 OUTPUT EXIT - LEDGER PREFIX REMOVAL       *
      * APPLICATION : CABS                                            *
      * COMPILER    : ENTERPRISE COBOL                                *
      * INVOKED BY  : OS SORT/MERGE, MODS E35=(CABSXLDR,4096)         *
      *               ON CONTROL CARD MEMBER                          *
      *               JCL/CTLCARDS/MVT/CABSRT17                       *
      * INPUTS      : ONE 402 BYTE RECORD PER ENTRY LEAVING THE       *
      *               FINAL MERGE, PREFIXED BY CABSXLDG               *
      * OUTPUTS     : THE ORIGINAL 400 BYTE INVOICE HEADER, WITH      *
      *               THE TWO BYTE PREFIX TAKEN BACK OFF              *
      * CONTROL     : NONE - EXITS DO NOT WRITE CTLOUT, CABS-STD-041  *
      * BALANCE     : MERGE OUT = RECORDS WRITTEN, NOTHING DROPPED    *
      * RESTART     : NOT RESTARTABLE - RERUN THE WHOLE SORT STEP     *
      *                                                               *
      * LINKAGE CONVENTION                                            *
      *   REGISTER 1 ADDRESSES A THREE WORD PARAMETER LIST.  WORD     *
      *   ONE IS THE ADDRESS OF THE RECORD LEAVING THE FINAL MERGE,   *
      *   OR BINARY ZERO WHEN THE MERGE IS EXHAUSTED.  WORD TWO IS    *
      *   THE ADDRESS OF THE RECORD MOST RECENTLY WRITTEN TO          *
      *   SORTOUT.  WORD THREE ADDRESSES THE LENGTH HALFWORD.  THE    *
      *   REPLY IS PLACED IN RETURN-CODE -                            *
      *     00  NO MORE RECORDS TO INSERT - TAKE THE NEXT ONE         *
      *     04  DELETE THE RECORD - DO NOT WRITE IT TO SORTOUT        *
      *     08  WRITE THE RECORD ADDRESSED BY WORD ONE                *
      *     12  DO NOT ENTER THIS EXIT AGAIN                          *
      *     16  TERMINATE THE SORT                                    *
      *   THE LENGTH HALFWORD ADDRESSED BY WORD THREE IS SET TO 400   *
      *   BEFORE THE REPLY OF 08 IS GIVEN, BECAUSE THE RECORD BEING   *
      *   WRITTEN IS TWO BYTES SHORTER THAN THE ONE THAT CAME OUT     *
      *   OF THE MERGE.  WORKING STORAGE PERSISTS FOR THE LIFE OF     *
      *   THE SORT STEP.                                              *
      *                                                               *
      * WHAT THIS EXIT DECIDES                                        *
      *   CABSXLDG PUT THE TWO BYTE LEDGER COMPANY IN FRONT OF THE    *
      *   INVOICE HEADER SO THE SORT COULD KEY ON IT.  THIS EXIT      *
      *   TAKES THOSE TWO BYTES BACK OFF SO THE MONTH END CLOSE       *
      *   PROGRAM READS THE ORIGINAL 400 BYTE LAYOUT IT HAS ALWAYS    *
      *   READ.  THE REMOVAL WAS CARRIED AS AN OUTREC OPERAND ON      *
      *   THE CONTROL CARD UNTIL THE STEP WAS MOVED TO A SORT THAT    *
      *   HAS NO OUTREC STATEMENT.                                    *
      *   THE PREFIX IS SET AGAINST BYTES 24 AND 25 OF THE RESTORED   *
      *   RECORD BEFORE IT IS DISCARDED.  THOSE TWO BYTES ARE WHERE   *
      *   THE PREFIX CAME FROM, SO THE TWO AGREE ON EVERY RECORD      *
      *   THAT PASSED THROUGH CABSXLDG WITH AN ASSIGNED INVOICE       *
      *   NUMBER.  A HEADER WHOSE INVOICE NUMBER WAS BLANK CARRIES    *
      *   THE SUBSTITUTED 00 IN THE PREFIX AND SPACES AT 24, SO IT    *
      *   IS COUNTED AS A DIFFERENCE.  THE COUNT GOES TO SYSOUT.      *
      *   THE CALLING PROGRAM'S BALANCE IS UNAFFECTED BY THIS         *
      *   MODULE - A SORT STEP WRITES NO CONTROL RECORD OF ITS OWN.   *
      *                                                               *
      * REVISION HISTORY                                              *
      *   V1.00  1992-08-17  D.OKONKWO     INITIAL - ASSEMBLER F      *
      *   V1.03  1997-03-11  J.CALLAGHAN   PREFIX CHECKED AGAINST     *
      *                                    THE INVOICE NUMBER         *
      *   V1.05  2001-05-29  B.R.HALVORSEN LEDGER RUN TOTALS ADDED    *
      *   V2.00  2007-06-08  P.NAIR        RECODED IN COBOL FOR LE    *
      *   V2.02  2014-04-15  G.PETRAKIS    LENGTH WORD SET FROM A     *
      *                                    CONSTANT RATHER THAN A     *
      *                                    COMPUTED VALUE             *
      *   V2.04  2018-10-16  M.HAAS        RECOMPILE ONLY - LE V6.2   *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-MODULE-IDENT.
           05  FILLER                  PIC X(08) VALUE 'CABSXLDR'.
           05  FILLER                  PIC X(08) VALUE ' V2.04  '.
       01  WS-SWITCHES.
           05  WS-FIRST-ENTRY-SW       PIC X(01) VALUE 'Y'.
               88  WS-FIRST-ENTRY      VALUE 'Y'.
           05  WS-EOM-SEEN-SW          PIC X(01) VALUE 'N'.
               88  WS-EOM-SEEN         VALUE 'Y'.
       01  WS-COUNTERS.
           05  WS-ENTRY-CNT            PIC S9(11) COMP-3 VALUE 0.
           05  WS-WRITTEN-CNT          PIC S9(11) COMP-3 VALUE 0.
           05  WS-PREFIX-AGREES        PIC S9(11) COMP-3 VALUE 0.
           05  WS-PREFIX-DIFFERS       PIC S9(09) COMP-3 VALUE 0.
           05  WS-PREFIX-WAS-BLANK     PIC S9(09) COMP-3 VALUE 0.
           05  WS-LEDGER-BREAKS        PIC S9(07) COMP-3 VALUE 0.
           05  WS-PERIOD-BREAKS        PIC S9(07) COMP-3 VALUE 0.
           05  WS-ORDER-REVERSED       PIC S9(07) COMP-3 VALUE 0.
       01  WS-VALUES.
           05  WS-TOTAL-DUE            PIC S9(15)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CURR-CHARGE          PIC S9(15)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-LEDGER-DUE           PIC S9(13)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-WORK-FIELDS.
           05  WS-THIS-PREFIX          PIC X(02) VALUE SPACES.
           05  WS-BODY-PREFIX          PIC X(02) VALUE SPACES.
           05  WS-LAST-PREFIX          PIC X(02) VALUE SPACES.
           05  WS-LAST-PERIOD          PIC 9(06) VALUE 0.
           05  WS-BLANK-TALLY          PIC S9(04) COMP VALUE 0.
      *
      * THE 400 BYTE WORK RECORD.  IT IS THE AREA HANDED BACK TO
      * SORT, SO IT MUST STAY ADDRESSABLE BETWEEN ENTRIES.  THE
      * INVOICE NUMBER IS NAMED INSIDE IT SO THE PREFIX CHECK CAN
      * BE MADE ON THE RESTORED LAYOUT RATHER THAN ON THE PREFIXED
      * ONE.
      *
       01  WS-OUT-RECORD.
           05  WS-OR-HEAD              PIC X(23).
           05  WS-OR-INVOICE-NBR.
               10  WS-OR-LEDGER-CO     PIC X(02).
               10  WS-OR-INV-SERIAL    PIC X(10).
           05  WS-OR-TAIL              PIC X(365).
      *
       LINKAGE SECTION.
       01  LK-PARM-LIST.
           05  LK-RECORD-PTR           POINTER.
           05  LK-PREV-PTR             POINTER.
           05  LK-LENGTH-PTR           POINTER.
       01  LK-LENGTH-WORD.
           05  LK-REC-LENGTH           PIC S9(04) COMP.
      *
      * A HAND MAINTAINED VIEW OF THE 402 BYTE RECORD AS IT LEAVES
      * THE MERGE.  THE FIRST TWO BYTES ARE THE PREFIX CABSXLDG
      * PUT THERE.  EVERY NAMED FIELD AFTER THAT SITS TWO BYTES
      * RIGHT OF WHERE IT SITS ON THE FILE THE CLOSE READS.
      *
       01  LK-SORT-RECORD.
           05  LK-LR-PREFIX            PIC X(02).
           05  LK-LR-BODY              PIC X(400).
       01  LK-BODY-VIEW.
           05  LK-IH-BAN               PIC X(13).
           05  LK-IH-INV-TYPE          PIC X(01).
           05  LK-IH-BILL-PERIOD       PIC 9(06).
           05  LK-IH-STATE-CD          PIC X(02).
           05  LK-IH-MEDIA-CD          PIC X(01).
           05  LK-IH-INVOICE-NBR.
               10  LK-IH-LEDGER-CO     PIC X(02).
               10  LK-IH-INV-SERIAL    PIC X(10).
           05  LK-IH-OCN               PIC X(04).
           05  LK-IH-RAO               PIC 9(03).
           05  LK-IH-CUST-NAME         PIC X(30).
           05  LK-IH-INV-YYDDD         PIC 9(05).
           05  LK-IH-DUE-YYDDD         PIC 9(05).
           05  LK-IH-CURR-CHG          PIC S9(11)V9(02) COMP-3.
           05  LK-IH-PRIOR-BAL         PIC S9(11)V9(02) COMP-3.
           05  LK-IH-PAYMENTS          PIC S9(11)V9(02) COMP-3.
           05  LK-IH-ADJUSTMENTS       PIC S9(11)V9(02) COMP-3.
           05  LK-IH-TAX-AMT           PIC S9(09)V9(02) COMP-3.
           05  LK-IH-TOTAL-DUE         PIC S9(11)V9(02) COMP-3.
           05  LK-IH-TAIL              PIC X(277).
      *
       PROCEDURE DIVISION USING LK-PARM-LIST.
       P0000-MAINLINE.
           MOVE ZERO TO RETURN-CODE.
           IF WS-FIRST-ENTRY
               MOVE 'N' TO WS-FIRST-ENTRY-SW
               DISPLAY 'CABSXLDR ENTERED - LEDGER PREFIX REMOVAL'
           END-IF.
           IF LK-RECORD-PTR = NULL
               PERFORM P8000-END-OF-MERGE THRU P8000-EXIT
               GOBACK
           END-IF.
           SET ADDRESS OF LK-SORT-RECORD TO LK-RECORD-PTR.
           SET ADDRESS OF LK-LENGTH-WORD TO LK-LENGTH-PTR.
           ADD 1 TO WS-ENTRY-CNT.
           PERFORM P2000-STRIP-PREFIX THRU P2000-EXIT.
           PERFORM P3000-CHECK-PREFIX THRU P3000-EXIT.
           PERFORM P4000-ACCUMULATE THRU P4000-EXIT.
           PERFORM P5000-RETURN-RECORD THRU P5000-EXIT.
           GOBACK.

       P2000-STRIP-PREFIX.
      * MOVE THE 400 BYTE BODY OUT WHOLE.  THE TWO PREFIX BYTES ARE
      * KEPT IN WORKING STORAGE FOR THE CHECK AND ARE THEN
      * DISCARDED - THEY ARE NOT WRITTEN.
           MOVE LK-LR-PREFIX TO WS-THIS-PREFIX.
           MOVE LK-LR-BODY   TO WS-OUT-RECORD.
           SET ADDRESS OF LK-BODY-VIEW TO ADDRESS OF WS-OUT-RECORD.
           MOVE WS-OR-LEDGER-CO TO WS-BODY-PREFIX.

       P2000-EXIT.
           EXIT.

       P3000-CHECK-PREFIX.
      * THE PREFIX AND BYTES 24 AND 25 OF THE RESTORED RECORD MUST
      * BE THE SAME TWO CHARACTERS.  A DIFFERENCE MEANS THE RECORD
      * REACHING THIS EXIT IS NOT THE ONE CABSXLDG BUILT.  THE
      * SUBSTITUTED 00 ON AN UNASSIGNED INVOICE NUMBER IS COUNTED
      * SEPARATELY BECAUSE IT IS A DIFFERENCE BY DESIGN.
           MOVE ZERO TO WS-BLANK-TALLY.
           INSPECT WS-BODY-PREFIX TALLYING WS-BLANK-TALLY
                   FOR ALL SPACE.
           IF WS-BLANK-TALLY = 2 AND WS-THIS-PREFIX = '00'
               ADD 1 TO WS-PREFIX-WAS-BLANK
               GO TO P3000-EXIT
           END-IF.
           IF WS-THIS-PREFIX = WS-BODY-PREFIX
               ADD 1 TO WS-PREFIX-AGREES
           ELSE
               ADD 1 TO WS-PREFIX-DIFFERS
               DISPLAY 'CABSXLDR PREFIX ' WS-THIS-PREFIX
                       ' INVOICE ' WS-OR-INVOICE-NBR
           END-IF.

       P3000-EXIT.
           EXIT.

       P4000-ACCUMULATE.
      * RUN AND PER LEDGER TOTALS FOR THE CLOSE.  A CHANGE OF
      * PREFIX CLOSES A LEDGER RUN AND ITS TOTAL IS PRINTED THERE
      * AND THEN, BECAUSE NOTHING DOWNSTREAM OF THIS SORT REPORTS
      * AT THAT LEVEL.
           ADD LK-IH-TOTAL-DUE TO WS-TOTAL-DUE.
           ADD LK-IH-CURR-CHG  TO WS-CURR-CHARGE.
           IF WS-THIS-PREFIX NOT = WS-LAST-PREFIX
               IF WS-LEDGER-BREAKS > ZERO
                   DISPLAY 'CABSXLDR LEDGER ' WS-LAST-PREFIX
                           ' DUE ' WS-LEDGER-DUE
               END-IF
               IF WS-THIS-PREFIX < WS-LAST-PREFIX
                   ADD 1 TO WS-ORDER-REVERSED
               END-IF
               ADD 1 TO WS-LEDGER-BREAKS
               MOVE ZERO TO WS-LEDGER-DUE
               MOVE WS-THIS-PREFIX TO WS-LAST-PREFIX
           END-IF.
           ADD LK-IH-TOTAL-DUE TO WS-LEDGER-DUE.
           IF LK-IH-BILL-PERIOD NOT = WS-LAST-PERIOD
               ADD 1 TO WS-PERIOD-BREAKS
               MOVE LK-IH-BILL-PERIOD TO WS-LAST-PERIOD
           END-IF.

       P4000-EXIT.
           EXIT.

       P5000-RETURN-RECORD.
      * POINT WORD ONE AT THE 400 BYTE WORK AREA AND SET THE LENGTH
      * HALFWORD ADDRESSED BY WORD THREE TO 400 BEFORE REPLYING 08.
      * THE CONTROL CARD CARRIES RECORD TYPE=F,LENGTH=(402) FOR THE
      * SORT ITSELF - THE SHORTENING HAPPENS HERE AND NOWHERE ELSE.
           MOVE 400 TO LK-REC-LENGTH.
           SET LK-RECORD-PTR TO ADDRESS OF WS-OUT-RECORD.
           ADD 1 TO WS-WRITTEN-CNT.
           MOVE 8 TO RETURN-CODE.

       P5000-EXIT.
           EXIT.

       P8000-END-OF-MERGE.
      * PRINT THE LAST LEDGER RUN AND THE RUN TOTALS, THEN REPLY
      * ZERO SO SORT CAN CLOSE SORTOUT.  NOTHING IS INSERTED HERE.
           MOVE 'Y' TO WS-EOM-SEEN-SW.
           IF WS-LEDGER-BREAKS > ZERO
               DISPLAY 'CABSXLDR LEDGER ' WS-LAST-PREFIX
                       ' DUE ' WS-LEDGER-DUE
           END-IF.
           DISPLAY 'CABSXLDR ENTRIES     ' WS-ENTRY-CNT.
           DISPLAY 'CABSXLDR WRITTEN     ' WS-WRITTEN-CNT.
           DISPLAY 'CABSXLDR PREFIX SAME ' WS-PREFIX-AGREES.
           DISPLAY 'CABSXLDR PREFIX DIFF ' WS-PREFIX-DIFFERS.
           DISPLAY 'CABSXLDR PREFIX WAS 0' WS-PREFIX-WAS-BLANK.
           DISPLAY 'CABSXLDR LEDGER RUNS ' WS-LEDGER-BREAKS.
           DISPLAY 'CABSXLDR PERIOD RUNS ' WS-PERIOD-BREAKS.
           DISPLAY 'CABSXLDR LEDGER BACK ' WS-ORDER-REVERSED.
           DISPLAY 'CABSXLDR TOTAL DUE   ' WS-TOTAL-DUE.
           DISPLAY 'CABSXLDR CURR CHARGE ' WS-CURR-CHARGE.
           MOVE ZERO TO RETURN-CODE.

       P8000-EXIT.
           EXIT.
