       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABSE35C.
      *****************************************************************
      * CABSE35C - SORT E35 OUTPUT EXIT - USAGE SUMMARISATION         *
      * APPLICATION : CABS                                            *
      * COMPILER    : ENTERPRISE COBOL                                *
      * INVOKED BY  : OS SORT/MERGE, MODS=(E35=(CABSE35C,8192))       *
      *               ON THE USAGE ROLL UP SORT STEPS OF CABJ1600     *
      *               AND CABRAT9R                                    *
      * INPUTS      : ONE 200 BYTE RATED RECORD PER ENTRY, IN         *
      *               OCN / BAN / JURISDICTION / ELEMENT ORDER        *
      * OUTPUTS     : ONE TOTAL RECORD PER CONTROL GROUP.  THE        *
      *               DETAIL RECORDS THEMSELVES ARE NOT WRITTEN.      *
      * CONTROL     : NONE - EXITS DO NOT WRITE CTLOUT, CABS-STD-041  *
      * BALANCE     : SUM OF TOTAL RECORDS = SUM OF DETAIL READ       *
      * RESTART     : NOT RESTARTABLE - RERUN THE WHOLE SORT STEP     *
      *                                                               *
      * LINKAGE CONVENTION                                            *
      *   REGISTER 1 ADDRESSES A THREE WORD PARAMETER LIST.  WORD     *
      *   ONE IS THE ADDRESS OF THE RECORD LEAVING THE FINAL MERGE    *
      *   OR BINARY ZERO AT END OF MERGE.  WORD TWO ADDRESSES THE     *
      *   LAST RECORD WRITTEN.  WORD THREE ADDRESSES THE LENGTH.      *
      *   THE REPLY IS PLACED IN RETURN-CODE - 00 TAKE THE NEXT       *
      *   RECORD, 04 DELETE, 08 WRITE THE RECORD ADDRESSED BY WORD    *
      *   ONE AND RE-ENTER, 12 NO FURTHER ENTRY, 16 TERMINATE.        *
      *   AN EXIT THAT SUMMARISES MUST REPLY 04 FOR EVERY DETAIL      *
      *   RECORD AND 08 WITH ITS OWN AREA AT EACH CONTROL BREAK,      *
      *   THEN 04 AGAIN TO SUPPRESS THE DETAIL THAT CAUSED THE        *
      *   BREAK.  THAT TWO STEP REPLY IS WHY THE BREAK IS HELD IN     *
      *   WS-PENDING-BREAK-SW ACROSS ENTRIES.                         *
      *                                                               *
      * WHAT THIS EXIT DECIDES                                        *
      *   THE GROUP IS OCN, BAN, BILL PERIOD AND JURISDICTION.  THE   *
      *  ELEMENT CODE IS NOT PART OF THE GROUP PER CABS-STD-036 - THE *
      *   ROLL UP IS TO THE JURISDICTIONAL LINE THE INVOICE PRINTS,   *
      *   NOT TO THE RATE ELEMENT.  MINUTES ARE ADDED IN ARRIVAL      *
      *   ORDER.  A LINE FLAGGED AS A CREDIT IS ACCUMULATED INTO A    *
      *   SEPARATE BUCKET AND SHOWN SEPARATELY ON THE TOTAL RECORD    *
      *   BECAUSE THE BILL FORMAT REQUIRES CREDITS TO PRINT BELOW     *
      *   THE USAGE LINE.  NONE OF THIS IS RESTATED IN ANY COBOL      *
      *   MODULE - CABRAT09 CONSUMES THE TOTAL RECORDS AS IF THEY     *
      *   HAD ALWAYS BEEN AT THAT LEVEL.                              *
      *                                                               *
      * REVISION HISTORY                                              *
      *   V1.00  1994-04-11  D.OKONKWO     INITIAL - ASSEMBLER F      *
      *   V1.03  1997-10-02  J.M.CASTILLO  CREDIT BUCKET SPLIT OUT    *
      *   V2.00  2006-07-25  P.NAIR        RECODED IN COBOL FOR LE    *
      *   V2.03  2012-02-29  A.BUKOWSKI    ELEMENT DROPPED FROM THE   *
      *                                    CONTROL GROUP              *
      *   V2.04  2019-01-15  M.HAAS        RECOMPILE ONLY - LE V6.2   *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-MODULE-IDENT.
           05  FILLER                  PIC X(08) VALUE 'CABSE35C'.
           05  FILLER                  PIC X(08) VALUE ' V2.04  '.
       01  WS-SWITCHES.
           05  WS-FIRST-ENTRY-SW       PIC X(01) VALUE 'Y'.
               88  WS-FIRST-ENTRY      VALUE 'Y'.
           05  WS-PENDING-BREAK-SW     PIC X(01) VALUE 'N'.
               88  WS-PENDING-BREAK    VALUE 'Y'.
           05  WS-GROUP-OPEN-SW        PIC X(01) VALUE 'N'.
               88  WS-GROUP-OPEN       VALUE 'Y'.
           05  WS-FINAL-SENT-SW        PIC X(01) VALUE 'N'.
               88  WS-FINAL-SENT       VALUE 'Y'.
       01  WS-COUNTERS.
           05  WS-ENTRY-CNT            PIC S9(11) COMP-3 VALUE 0.
           05  WS-DETAIL-CNT           PIC S9(11) COMP-3 VALUE 0.
           05  WS-GROUP-CNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-CREDIT-CNT           PIC S9(09) COMP-3 VALUE 0.
           05  WS-TOTAL-WRITTEN        PIC S9(09) COMP-3 VALUE 0.
       01  WS-GRAND-TOTALS.
           05  WS-GT-MINUTES           PIC S9(15)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-GT-AMOUNT            PIC S9(15)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-GT-CREDIT            PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
      *
      * THE CONTROL GROUP AND ITS ACCUMULATORS.
      *
       01  WS-CONTROL-GROUP.
           05  WS-CG-OCN               PIC X(04) VALUE SPACES.
           05  WS-CG-BAN               PIC X(13) VALUE SPACES.
           05  WS-CG-BILL-PERIOD       PIC 9(06) VALUE 0.
           05  WS-CG-JURIS-CD          PIC X(01) VALUE SPACE.
       01  WS-INCOMING-GROUP.
           05  WS-IG-OCN               PIC X(04) VALUE SPACES.
           05  WS-IG-BAN               PIC X(13) VALUE SPACES.
           05  WS-IG-BILL-PERIOD       PIC 9(06) VALUE 0.
           05  WS-IG-JURIS-CD          PIC X(01) VALUE SPACE.
       01  WS-GROUP-ACCUMS.
           05  WS-GA-MINUTES           PIC S9(13)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-GA-AMOUNT            PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-GA-CREDIT-AMT        PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-GA-SETUP-AMT         PIC S9(11)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-GA-LINE-CNT          PIC S9(07) COMP-3 VALUE 0.
           05  WS-GA-ELEM-CNT          PIC S9(03) COMP-3 VALUE 0.
           05  WS-GA-STATE-CD          PIC X(02) VALUE SPACES.
           05  WS-GA-FIRST-ELEM        PIC X(06) VALUE SPACES.
      *
      * THE TOTAL RECORD.  SECTION 50 IS THE JURISDICTIONAL SUMMARY
      * SECTION ON THE INVOICE.
      *
       01  WS-TOTAL-RECORD.
           05  WS-TR-REC-TYPE          PIC X(02) VALUE '20'.
           05  WS-TR-USAGE-TYPE        PIC X(01) VALUE 'T'.
           05  WS-TR-FILLER-1          PIC X(01) VALUE SPACE.
           05  WS-TR-OCN               PIC X(04) VALUE SPACES.
           05  WS-TR-BAN               PIC X(13) VALUE SPACES.
           05  WS-TR-SEQ               PIC 9(09) COMP-3 VALUE 0.
           05  WS-TR-FILLER-2          PIC X(09) VALUE SPACES.
           05  WS-TR-JURIS-CD          PIC X(01) VALUE SPACE.
           05  WS-TR-STATE-CD          PIC X(02) VALUE SPACES.
           05  WS-TR-SECTION           PIC X(02) VALUE '50'.
           05  WS-TR-FILLER-3          PIC X(05) VALUE SPACES.
           05  WS-TR-RATE-ELEM         PIC X(06) VALUE 'TOTUSG'.
           05  WS-TR-LINE-CLASS        PIC X(01) VALUE 'T'.
           05  WS-TR-BILL-PERIOD       PIC 9(06) VALUE 0.
           05  WS-TR-ROUND-RULE        PIC X(01) VALUE 'U'.
           05  WS-TR-ROUND-POS         PIC 9(01) VALUE 2.
           05  WS-TR-FIRST-ELEM        PIC X(06) VALUE SPACES.
           05  WS-TR-FILLER-4          PIC X(05) VALUE SPACES.
           05  WS-TR-MINUTES           PIC S9(13)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-TR-AMOUNT            PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-TR-ROUNDED           PIC S9(13)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-TR-CREDIT-AMT        PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-TR-SETUP-AMT         PIC S9(11)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-TR-LINE-CNT          PIC S9(07) COMP-3 VALUE 0.
           05  WS-TR-ELEM-CNT          PIC S9(03) COMP-3 VALUE 0.
           05  WS-TR-FILLER-5          PIC X(83) VALUE SPACES.
           05  WS-TR-CTL-PREFIX.
               10  WS-TC-RUN-STAMP     PIC 9(05) VALUE 0.
               10  WS-TC-WORK-ORD      PIC 9(02) VALUE 0.
               10  WS-TC-STRING-SEQ    PIC 9(04) VALUE 0.
               10  WS-TC-EXIT-VER      PIC X(01) VALUE 'C'.
      *
       LINKAGE SECTION.
       01  LK-PARM-LIST.
           05  LK-RECORD-PTR           POINTER.
           05  LK-PREV-PTR             POINTER.
           05  LK-LENGTH-PTR           POINTER.
       01  LK-SORT-RECORD.
           05  LK-DT-REC-TYPE          PIC X(02).
           05  LK-DT-USAGE-TYPE        PIC X(01).
           05  LK-DT-FILLER-1          PIC X(01).
           05  LK-DT-OCN               PIC X(04).
           05  LK-DT-BAN               PIC X(13).
           05  LK-DT-SEQ               PIC 9(09) COMP-3.
           05  LK-DT-FILLER-2          PIC X(09).
           05  LK-DT-JURIS-CD          PIC X(01).
           05  LK-DT-STATE-CD          PIC X(02).
           05  LK-DT-SECTION           PIC X(02).
           05  LK-DT-FILLER-3          PIC X(05).
           05  LK-DT-RATE-ELEM         PIC X(06).
           05  LK-DT-LINE-CLASS        PIC X(01).
               88  LK-DT-USAGE-LINE    VALUE 'U'.
               88  LK-DT-SETUP-LINE    VALUE 'S'.
               88  LK-DT-CREDIT-LINE   VALUE 'C'.
               88  LK-DT-MAKEUP-LINE   VALUE 'M'.
           05  LK-DT-BILL-PERIOD       PIC 9(06).
           05  LK-DT-FILLER-4          PIC X(13).
           05  LK-DT-MINUTES           PIC S9(13)V9(02) COMP-3.
           05  LK-DT-AMOUNT            PIC S9(13)V9(05) COMP-3.
           05  LK-DT-TAIL              PIC X(101).
           05  LK-DT-CTL-PREFIX.
               10  LK-RC-RUN-STAMP     PIC 9(05).
               10  LK-RC-WORK-ORD      PIC 9(02).
               10  LK-RC-STRING-SEQ    PIC 9(04).
               10  LK-RC-EXIT-VER      PIC X(01).
      *
       PROCEDURE DIVISION USING LK-PARM-LIST.
       P0000-MAINLINE.
           MOVE ZERO TO RETURN-CODE.
           IF WS-FIRST-ENTRY
               MOVE 'N' TO WS-FIRST-ENTRY-SW
               DISPLAY 'CABSE35C ENTERED - USAGE SUMMARISATION'
           END-IF.
           IF LK-RECORD-PTR = NULL
               PERFORM P8000-END-OF-MERGE THRU P8000-EXIT
               GOBACK
           END-IF.
           SET ADDRESS OF LK-SORT-RECORD TO LK-RECORD-PTR.
           IF WS-PENDING-BREAK
               PERFORM P5000-CLEAR-BREAK THRU P5000-EXIT
               GOBACK
           END-IF.
           ADD 1 TO WS-ENTRY-CNT.
           PERFORM P2000-TEST-BREAK THRU P2000-EXIT.
           IF WS-PENDING-BREAK
               GOBACK
           END-IF.
           PERFORM P3000-ACCUMULATE THRU P3000-EXIT.
           MOVE 4 TO RETURN-CODE.
           GOBACK.

       P2000-TEST-BREAK.
      * A CHANGE IN ANY OF THE FOUR GROUP FIELDS CLOSES THE GROUP.
      * THE INPUT IS PRESENTED IN THAT ORDER BY THE SORT KEY ON THE
      * CONTROL CARD - IF THE KEY IS EVER CHANGED THE GROUPING
      * SILENTLY CHANGES WITH IT.
           MOVE LK-DT-OCN         TO WS-IG-OCN.
           MOVE LK-DT-BAN         TO WS-IG-BAN.
           MOVE LK-DT-BILL-PERIOD TO WS-IG-BILL-PERIOD.
           MOVE LK-DT-JURIS-CD    TO WS-IG-JURIS-CD.
           IF WS-GROUP-OPEN
               CONTINUE
           ELSE
               PERFORM P2600-OPEN-GROUP THRU P2600-EXIT
               GO TO P2000-EXIT
           END-IF.
           IF WS-IG-OCN = WS-CG-OCN
              AND WS-IG-BAN = WS-CG-BAN
              AND WS-IG-BILL-PERIOD = WS-CG-BILL-PERIOD
              AND WS-IG-JURIS-CD = WS-CG-JURIS-CD
               GO TO P2000-EXIT
           END-IF.
           PERFORM P4000-RELEASE-TOTAL THRU P4000-EXIT.

       P2000-EXIT.
           EXIT.

       P2600-OPEN-GROUP.
           MOVE WS-IG-OCN         TO WS-CG-OCN.
           MOVE WS-IG-BAN         TO WS-CG-BAN.
           MOVE WS-IG-BILL-PERIOD TO WS-CG-BILL-PERIOD.
           MOVE WS-IG-JURIS-CD    TO WS-CG-JURIS-CD.
           MOVE LK-DT-STATE-CD    TO WS-GA-STATE-CD.
           MOVE LK-DT-RATE-ELEM   TO WS-GA-FIRST-ELEM.
           MOVE ZERO TO WS-GA-MINUTES.
           MOVE ZERO TO WS-GA-AMOUNT.
           MOVE ZERO TO WS-GA-CREDIT-AMT.
           MOVE ZERO TO WS-GA-SETUP-AMT.
           MOVE ZERO TO WS-GA-LINE-CNT.
           MOVE ZERO TO WS-GA-ELEM-CNT.
           MOVE 'Y' TO WS-GROUP-OPEN-SW.
           ADD 1 TO WS-GROUP-CNT.

       P2600-EXIT.
           EXIT.

       P3000-ACCUMULATE.
      * CREDITS AND SETUP CHARGES ARE HELD SEPARATELY.  BOTH ARE
      * STILL ADDED INTO THE GROUP AMOUNT - THE SEPARATE BUCKETS
      * ARE FOR PRINTING, NOT FOR ARITHMETIC.
           ADD 1 TO WS-DETAIL-CNT.
           ADD 1 TO WS-GA-LINE-CNT.
           IF LK-DT-RATE-ELEM NOT = WS-GA-FIRST-ELEM
               ADD 1 TO WS-GA-ELEM-CNT
               MOVE LK-DT-RATE-ELEM TO WS-GA-FIRST-ELEM
           END-IF.
           ADD LK-DT-MINUTES TO WS-GA-MINUTES.
           ADD LK-DT-AMOUNT  TO WS-GA-AMOUNT.
           IF LK-DT-CREDIT-LINE OR LK-DT-MAKEUP-LINE
               ADD LK-DT-AMOUNT TO WS-GA-CREDIT-AMT
               ADD 1 TO WS-CREDIT-CNT
           END-IF.
           IF LK-DT-SETUP-LINE
               ADD LK-DT-AMOUNT TO WS-GA-SETUP-AMT
           END-IF.
           ADD LK-DT-MINUTES TO WS-GT-MINUTES.
           ADD LK-DT-AMOUNT  TO WS-GT-AMOUNT.

       P3000-EXIT.
           EXIT.

       P4000-RELEASE-TOTAL.
      * BUILD THE TOTAL RECORD FOR THE GROUP JUST CLOSED AND HAND
      * IT TO SORT.  THE DETAIL RECORD THAT CAUSED THE BREAK HAS
      * NOT BEEN ACCUMULATED YET AND IS DEALT WITH ON THE NEXT
      * ENTRY, WHICH IS WHY THE BREAK IS HELD.
           MOVE WS-CG-OCN         TO WS-TR-OCN.
           MOVE WS-CG-BAN         TO WS-TR-BAN.
           MOVE WS-CG-BILL-PERIOD TO WS-TR-BILL-PERIOD.
           MOVE WS-CG-JURIS-CD    TO WS-TR-JURIS-CD.
           MOVE WS-GA-STATE-CD    TO WS-TR-STATE-CD.
           MOVE WS-GA-FIRST-ELEM  TO WS-TR-FIRST-ELEM.
           MOVE WS-GA-MINUTES     TO WS-TR-MINUTES.
           MOVE WS-GA-AMOUNT      TO WS-TR-AMOUNT.
           COMPUTE WS-TR-ROUNDED ROUNDED = WS-GA-AMOUNT.
           MOVE WS-GA-CREDIT-AMT  TO WS-TR-CREDIT-AMT.
           MOVE WS-GA-SETUP-AMT   TO WS-TR-SETUP-AMT.
           MOVE WS-GA-LINE-CNT    TO WS-TR-LINE-CNT.
           MOVE WS-GA-ELEM-CNT    TO WS-TR-ELEM-CNT.
           ADD 1 TO WS-TR-SEQ.
           MOVE LK-RC-RUN-STAMP   TO WS-TC-RUN-STAMP.
           MOVE LK-RC-WORK-ORD    TO WS-TC-WORK-ORD.
           ADD WS-GA-CREDIT-AMT   TO WS-GT-CREDIT.
           ADD 1 TO WS-TOTAL-WRITTEN.
           MOVE 'Y' TO WS-PENDING-BREAK-SW.
           MOVE 'N' TO WS-GROUP-OPEN-SW.
           SET LK-RECORD-PTR TO ADDRESS OF WS-TOTAL-RECORD.
           MOVE 8 TO RETURN-CODE.

       P4000-EXIT.
           EXIT.

       P5000-CLEAR-BREAK.
      * SORT HAS WRITTEN THE TOTAL RECORD AND IS PRESENTING THE
      * SAME DETAIL RECORD AGAIN.  OPEN THE NEW GROUP WITH IT AND
      * SUPPRESS IT.
           MOVE 'N' TO WS-PENDING-BREAK-SW.
           MOVE LK-DT-OCN         TO WS-IG-OCN.
           MOVE LK-DT-BAN         TO WS-IG-BAN.
           MOVE LK-DT-BILL-PERIOD TO WS-IG-BILL-PERIOD.
           MOVE LK-DT-JURIS-CD    TO WS-IG-JURIS-CD.
           PERFORM P2600-OPEN-GROUP THRU P2600-EXIT.
           PERFORM P3000-ACCUMULATE THRU P3000-EXIT.
           MOVE 4 TO RETURN-CODE.

       P5000-EXIT.
           EXIT.

       P8000-END-OF-MERGE.
      * RELEASE THE LAST GROUP, THEN REPLY ZERO SO SORT CAN CLOSE
      * SORTOUT.
           IF WS-FINAL-SENT
               PERFORM P8600-REPORT THRU P8600-EXIT
               MOVE ZERO TO RETURN-CODE
               GO TO P8000-EXIT
           END-IF.
           IF WS-GROUP-OPEN
               PERFORM P8200-FINAL-TOTAL THRU P8200-EXIT
               MOVE 'Y' TO WS-FINAL-SENT-SW
               SET LK-RECORD-PTR TO ADDRESS OF WS-TOTAL-RECORD
               MOVE 8 TO RETURN-CODE
               GO TO P8000-EXIT
           END-IF.
           MOVE 'Y' TO WS-FINAL-SENT-SW.
           PERFORM P8600-REPORT THRU P8600-EXIT.
           MOVE ZERO TO RETURN-CODE.

       P8000-EXIT.
           EXIT.

       P8200-FINAL-TOTAL.
           MOVE WS-CG-OCN         TO WS-TR-OCN.
           MOVE WS-CG-BAN         TO WS-TR-BAN.
           MOVE WS-CG-BILL-PERIOD TO WS-TR-BILL-PERIOD.
           MOVE WS-CG-JURIS-CD    TO WS-TR-JURIS-CD.
           MOVE WS-GA-STATE-CD    TO WS-TR-STATE-CD.
           MOVE WS-GA-FIRST-ELEM  TO WS-TR-FIRST-ELEM.
           MOVE WS-GA-MINUTES     TO WS-TR-MINUTES.
           MOVE WS-GA-AMOUNT      TO WS-TR-AMOUNT.
           COMPUTE WS-TR-ROUNDED ROUNDED = WS-GA-AMOUNT.
           MOVE WS-GA-CREDIT-AMT  TO WS-TR-CREDIT-AMT.
           MOVE WS-GA-SETUP-AMT   TO WS-TR-SETUP-AMT.
           MOVE WS-GA-LINE-CNT    TO WS-TR-LINE-CNT.
           MOVE WS-GA-ELEM-CNT    TO WS-TR-ELEM-CNT.
           ADD 1 TO WS-TR-SEQ.
           ADD WS-GA-CREDIT-AMT   TO WS-GT-CREDIT.
           ADD 1 TO WS-TOTAL-WRITTEN.
           MOVE 'N' TO WS-GROUP-OPEN-SW.

       P8200-EXIT.
           EXIT.

       P8600-REPORT.
           DISPLAY 'CABSE35C ENTRIES     ' WS-ENTRY-CNT.
           DISPLAY 'CABSE35C DETAIL IN   ' WS-DETAIL-CNT.
           DISPLAY 'CABSE35C GROUPS      ' WS-GROUP-CNT.
           DISPLAY 'CABSE35C TOTALS OUT  ' WS-TOTAL-WRITTEN.
           DISPLAY 'CABSE35C CREDIT LNS  ' WS-CREDIT-CNT.
           DISPLAY 'CABSE35C GRAND MOU   ' WS-GT-MINUTES.
           DISPLAY 'CABSE35C GRAND AMT   ' WS-GT-AMOUNT.
           DISPLAY 'CABSE35C GRAND CRED  ' WS-GT-CREDIT.

       P8600-EXIT.
           EXIT.
