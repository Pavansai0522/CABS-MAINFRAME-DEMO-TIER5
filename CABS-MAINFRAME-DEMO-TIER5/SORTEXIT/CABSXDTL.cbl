       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABSXDTL.
      *****************************************************************
      * CABSXDTL - SORT E35 OUTPUT EXIT - BILL DETAIL SUMMARISATION   *
      * APPLICATION : CABS                                            *
      * COMPILER    : ENTERPRISE COBOL                                *
      * INVOKED BY  : OS SORT/MERGE, MODS E35=(CABSXDTL,8192)         *
      *               ON CONTROL CARD MEMBER                          *
      *               JCL/CTLCARDS/MVT/CABSRT10                       *
      * INPUTS      : ONE 200 BYTE RATED RECORD PER ENTRY, IN BAN /   *
      *               BILL PERIOD / SECTION / LINE SEQUENCE /         *
      *               ELEMENT SEQUENCE ORDER                          *
      * OUTPUTS     : ONE RECORD PER CONTROL GROUP, CARRYING THE      *
      *               ADDED QUANTITY AND ADDED AMOUNT                 *
      * CONTROL     : NONE - EXITS DO NOT WRITE CTLOUT, CABS-STD-041  *
      * BALANCE     : SUM OF RELEASED AMOUNTS = SUM OF AMOUNTS READ   *
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
      *   AN EXIT THAT SUMMARISES MUST REPLY 04 FOR EVERY RECORD IT   *
      *   ABSORBS AND 08 WITH ITS OWN AREA AT EACH CONTROL BREAK,     *
      *   THEN 04 AGAIN FOR THE RECORD THAT CAUSED THE BREAK.  THAT   *
      *   TWO STEP REPLY IS WHY THE BREAK IS HELD IN                  *
      *   WS-PENDING-BREAK-SW ACROSS ENTRIES.  WORKING STORAGE        *
      *   PERSISTS FOR THE LIFE OF THE SORT STEP.                     *
      *                                                               *
      * WHAT THIS EXIT DECIDES                                        *
      *   THE CONTROL GROUP IS BAN, BILL PERIOD, SECTION, LINE        *
      *   SEQUENCE AND ELEMENT SEQUENCE - THE SAME FIVE FIELDS THE    *
      *   SORT KEY IS BUILT FROM.  TWO RATED RECORDS THAT AGREE ON    *
      *   ALL FIVE ARE COLLAPSED INTO ONE, ADDING THE PACKED          *
      *   QUANTITY AT 37 AND THE PACKED AMOUNT AT 45.  THE RULE WAS   *
      *   CARRIED AS A SUM FIELDS OPERAND ON THE CONTROL CARD UNTIL   *
      *   THE STEP WAS MOVED TO A SORT THAT HAS NO SUM STATEMENT.     *
      *   THAT DEDUPLICATION RULE EXISTS NOWHERE ELSE.  NO COBOL      *
      *   PROGRAM IN THE ESTATE KNOWS THAT DUPLICATE ELEMENT          *
      *   SEQUENCES ARE POSSIBLE, AND CABBIL02 CONSUMES WHAT COMES    *
      *   OUT OF THIS SORT AS IF ONE RECORD PER ELEMENT SEQUENCE      *
      *   HAD ALWAYS BEEN WRITTEN UPSTREAM.                           *
      *   THE SURVIVING RECORD IS THE FIRST ONE OF THE GROUP TO       *
      *   REACH THIS EXIT, SO EVERY NON ADDED FIELD - USOC, RATE,     *
      *   CIRCUIT AND TEXT - IS TAKEN FROM THAT FIRST RECORD.         *
      *   THE CALLING PROGRAM'S BALANCE IS UNAFFECTED BY THIS         *
      *   MODULE - A SORT STEP WRITES NO CONTROL RECORD OF ITS OWN.   *
      *                                                               *
      * REVISION HISTORY                                              *
      *   V1.00  1989-09-12  R.T.WHEELER   INITIAL - ASSEMBLER F      *
      *   V1.02  1992-06-30  D.OKONKWO     SECTION ADDED TO GROUP     *
      *   V1.06  1998-04-14  E.KOWALCZYK   QUANTITY WIDENED TO 8      *
      *   V2.00  2006-01-23  P.NAIR        RECODED IN COBOL FOR LE    *
      *   V2.02  2011-10-07  A.BUKOWSKI    ELEMENT SEQUENCE ADDED     *
      *                                    TO THE CONTROL GROUP       *
      *   V2.04  2016-05-19  G.PETRAKIS    ABSORBED COUNT REPORTED    *
      *   V2.05  2019-03-08  M.HAAS        RECOMPILE ONLY - LE V6.2   *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-MODULE-IDENT.
           05  FILLER                  PIC X(08) VALUE 'CABSXDTL'.
           05  FILLER                  PIC X(08) VALUE ' V2.05  '.
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
           05  WS-GROUP-CNT            PIC S9(11) COMP-3 VALUE 0.
           05  WS-RELEASED-CNT         PIC S9(11) COMP-3 VALUE 0.
           05  WS-ABSORBED-CNT         PIC S9(11) COMP-3 VALUE 0.
       01  WS-GRAND-TOTALS.
           05  WS-GT-QUANTITY          PIC S9(15)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-GT-AMOUNT            PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
      *
      * THE CONTROL GROUP.  THE FIVE FIELDS ARE HELD SEPARATELY
      * FROM THE HELD RECORD SO THAT THE COMPARE IS ON THE GROUP
      * ALONE AND NOT ON ANY FIELD THAT IS BEING ADDED INTO.
      *
       01  WS-CONTROL-GROUP.
           05  WS-CG-BAN               PIC X(13) VALUE SPACES.
           05  WS-CG-BILL-PERIOD       PIC 9(06) VALUE 0.
           05  WS-CG-SECTION           PIC X(02) VALUE SPACES.
           05  WS-CG-LINE-SEQ          PIC S9(07) COMP-3 VALUE 0.
           05  WS-CG-ELEM-SEQ          PIC X(02) VALUE SPACES.
       01  WS-INCOMING-GROUP.
           05  WS-IG-BAN               PIC X(13) VALUE SPACES.
           05  WS-IG-BILL-PERIOD       PIC 9(06) VALUE 0.
           05  WS-IG-SECTION           PIC X(02) VALUE SPACES.
           05  WS-IG-LINE-SEQ          PIC S9(07) COMP-3 VALUE 0.
           05  WS-IG-ELEM-SEQ          PIC X(02) VALUE SPACES.
       01  WS-GROUP-WORK.
           05  WS-GW-MEMBER-CNT        PIC S9(05) COMP-3 VALUE 0.
           05  WS-GW-QUANTITY          PIC S9(13)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-GW-AMOUNT            PIC S9(10)V9(05) COMP-3
                                                  VALUE 0.
      *
      * THE HELD RECORD.  THIS IS THE AREA SORT IS GIVEN AT THE
      * BREAK, SO IT MUST STAY ADDRESSABLE BETWEEN ENTRIES.  IT IS
      * A COPY OF THE FIRST RECORD OF THE GROUP WITH THE TWO ADDED
      * FIELDS OVERLAID JUST BEFORE IT IS RELEASED.
      *
       01  WS-HELD-RECORD.
           05  WS-HR-HEAD              PIC X(36).
           05  WS-HR-QUANTITY          PIC S9(13)V9(02) COMP-3.
           05  WS-HR-AMOUNT            PIC S9(10)V9(05) COMP-3.
           05  WS-HR-TAIL              PIC X(148).
      *
       LINKAGE SECTION.
       01  LK-PARM-LIST.
           05  LK-RECORD-PTR           POINTER.
           05  LK-PREV-PTR             POINTER.
           05  LK-LENGTH-PTR           POINTER.
      *
      * A HAND MAINTAINED VIEW OF THE 200 BYTE RATED DETAIL RECORD.
      * THE POSITIONS QUOTED ON THE CONTROL CARD ARE THE POSITIONS
      * IN THIS VIEW - 1,13 BAN, 14,6 PERIOD, 20,2 SECTION, 22,4
      * PACKED LINE SEQUENCE, 35,2 ELEMENT SEQUENCE, 37,8 PACKED
      * QUANTITY AND 45,8 PACKED AMOUNT.
      *
       01  LK-SORT-RECORD.
           05  LK-DT-BAN               PIC X(13).
           05  LK-DT-BILL-PERIOD       PIC 9(06).
           05  LK-DT-SECTION           PIC X(02).
           05  LK-DT-LINE-SEQ          PIC S9(07) COMP-3.
           05  LK-DT-USOC              PIC X(05).
           05  LK-DT-JURIS-CD          PIC X(01).
           05  LK-DT-LINE-CLASS        PIC X(01).
               88  LK-DT-USAGE-LINE    VALUE 'U'.
               88  LK-DT-CREDIT-LINE   VALUE 'C'.
           05  LK-DT-FILLER-1          PIC X(02).
           05  LK-DT-ELEM-SEQ          PIC X(02).
           05  LK-DT-QUANTITY          PIC S9(13)V9(02) COMP-3.
           05  LK-DT-AMOUNT            PIC S9(10)V9(05) COMP-3.
           05  LK-DT-RATE-ELEM         PIC X(06).
           05  LK-DT-RATE              PIC S9(05)V9(05) COMP-3.
           05  LK-DT-OCN               PIC X(04).
           05  LK-DT-TAIL              PIC X(136).
      *
       PROCEDURE DIVISION USING LK-PARM-LIST.
       P0000-MAINLINE.
           MOVE ZERO TO RETURN-CODE.
           IF WS-FIRST-ENTRY
               MOVE 'N' TO WS-FIRST-ENTRY-SW
               DISPLAY 'CABSXDTL ENTERED - DETAIL SUMMARISATION'
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
           PERFORM P3000-ABSORB THRU P3000-EXIT.
           MOVE 4 TO RETURN-CODE.
           GOBACK.

       P2000-TEST-BREAK.
      * A CHANGE IN ANY OF THE FIVE GROUP FIELDS CLOSES THE GROUP.
      * THE INPUT IS PRESENTED IN THAT ORDER BY THE SORT KEY ON THE
      * CONTROL CARD - IF THE KEY IS EVER CHANGED THE GROUPING
      * CHANGES WITH IT.
           MOVE LK-DT-BAN         TO WS-IG-BAN.
           MOVE LK-DT-BILL-PERIOD TO WS-IG-BILL-PERIOD.
           MOVE LK-DT-SECTION     TO WS-IG-SECTION.
           MOVE LK-DT-LINE-SEQ    TO WS-IG-LINE-SEQ.
           MOVE LK-DT-ELEM-SEQ    TO WS-IG-ELEM-SEQ.
           IF WS-GROUP-OPEN
               CONTINUE
           ELSE
               PERFORM P2600-OPEN-GROUP THRU P2600-EXIT
               GO TO P2000-EXIT
           END-IF.
           IF WS-IG-BAN = WS-CG-BAN
              AND WS-IG-BILL-PERIOD = WS-CG-BILL-PERIOD
              AND WS-IG-SECTION = WS-CG-SECTION
              AND WS-IG-LINE-SEQ = WS-CG-LINE-SEQ
              AND WS-IG-ELEM-SEQ = WS-CG-ELEM-SEQ
               GO TO P2000-EXIT
           END-IF.
           PERFORM P4000-RELEASE-GROUP THRU P4000-EXIT.

       P2000-EXIT.
           EXIT.

       P2600-OPEN-GROUP.
      * THE FIRST RECORD OF A GROUP IS COPIED WHOLE INTO THE HELD
      * AREA.  EVERY FIELD OUTSIDE THE TWO ADDED ONES IS TAKEN FROM
      * IT AND IS NEVER LOOKED AT AGAIN FOR THIS GROUP.
           MOVE WS-IG-BAN         TO WS-CG-BAN.
           MOVE WS-IG-BILL-PERIOD TO WS-CG-BILL-PERIOD.
           MOVE WS-IG-SECTION     TO WS-CG-SECTION.
           MOVE WS-IG-LINE-SEQ    TO WS-CG-LINE-SEQ.
           MOVE WS-IG-ELEM-SEQ    TO WS-CG-ELEM-SEQ.
           MOVE LK-SORT-RECORD    TO WS-HELD-RECORD.
           MOVE ZERO TO WS-GW-QUANTITY.
           MOVE ZERO TO WS-GW-AMOUNT.
           MOVE ZERO TO WS-GW-MEMBER-CNT.
           MOVE 'Y' TO WS-GROUP-OPEN-SW.
           ADD 1 TO WS-GROUP-CNT.

       P2600-EXIT.
           EXIT.

       P3000-ABSORB.
      * ADD THE PACKED QUANTITY AND THE PACKED AMOUNT INTO THE
      * GROUP.  THE SECOND AND ANY LATER MEMBER OF A GROUP IS A
      * DUPLICATE ELEMENT SEQUENCE AND IS COUNTED SEPARATELY SO
      * THE VOLUME IS VISIBLE ON SYSOUT.
           ADD 1 TO WS-DETAIL-CNT.
           ADD 1 TO WS-GW-MEMBER-CNT.
           ADD LK-DT-QUANTITY TO WS-GW-QUANTITY.
           ADD LK-DT-AMOUNT   TO WS-GW-AMOUNT.
           ADD LK-DT-QUANTITY TO WS-GT-QUANTITY.
           ADD LK-DT-AMOUNT   TO WS-GT-AMOUNT.
           IF WS-GW-MEMBER-CNT > 1
               ADD 1 TO WS-ABSORBED-CNT
           END-IF.

       P3000-EXIT.
           EXIT.

       P4000-RELEASE-GROUP.
      * OVERLAY THE TWO ADDED FIELDS ON THE HELD RECORD AND HAND IT
      * TO SORT.  THE RECORD THAT CAUSED THE BREAK HAS NOT BEEN
      * ABSORBED YET AND IS DEALT WITH ON THE NEXT ENTRY, WHICH IS
      * WHY THE BREAK IS HELD.
           MOVE WS-GW-QUANTITY TO WS-HR-QUANTITY.
           MOVE WS-GW-AMOUNT   TO WS-HR-AMOUNT.
           ADD 1 TO WS-RELEASED-CNT.
           MOVE 'Y' TO WS-PENDING-BREAK-SW.
           MOVE 'N' TO WS-GROUP-OPEN-SW.
           SET LK-RECORD-PTR TO ADDRESS OF WS-HELD-RECORD.
           MOVE 8 TO RETURN-CODE.

       P4000-EXIT.
           EXIT.

       P5000-CLEAR-BREAK.
      * SORT HAS WRITTEN THE HELD RECORD AND IS PRESENTING THE SAME
      * INPUT RECORD AGAIN.  OPEN THE NEW GROUP WITH IT AND ABSORB
      * IT, THEN SUPPRESS IT.
           MOVE 'N' TO WS-PENDING-BREAK-SW.
           MOVE LK-DT-BAN         TO WS-IG-BAN.
           MOVE LK-DT-BILL-PERIOD TO WS-IG-BILL-PERIOD.
           MOVE LK-DT-SECTION     TO WS-IG-SECTION.
           MOVE LK-DT-LINE-SEQ    TO WS-IG-LINE-SEQ.
           MOVE LK-DT-ELEM-SEQ    TO WS-IG-ELEM-SEQ.
           PERFORM P2600-OPEN-GROUP THRU P2600-EXIT.
           PERFORM P3000-ABSORB THRU P3000-EXIT.
           MOVE 4 TO RETURN-CODE.

       P5000-EXIT.
           EXIT.

       P8000-END-OF-MERGE.
      * RELEASE THE LAST GROUP, THEN REPLY ZERO SO SORT CAN CLOSE
      * SORTOUT.  A GROUP THAT REACHES THIS PATH AND IS NOT
      * RELEASED IS NOT WRITTEN ANYWHERE.
           IF WS-FINAL-SENT
               PERFORM P8600-REPORT THRU P8600-EXIT
               MOVE ZERO TO RETURN-CODE
               GO TO P8000-EXIT
           END-IF.
           IF WS-GROUP-OPEN
               MOVE WS-GW-QUANTITY TO WS-HR-QUANTITY
               MOVE WS-GW-AMOUNT   TO WS-HR-AMOUNT
               ADD 1 TO WS-RELEASED-CNT
               MOVE 'N' TO WS-GROUP-OPEN-SW
               MOVE 'Y' TO WS-FINAL-SENT-SW
               SET LK-RECORD-PTR TO ADDRESS OF WS-HELD-RECORD
               MOVE 8 TO RETURN-CODE
               GO TO P8000-EXIT
           END-IF.
           MOVE 'Y' TO WS-FINAL-SENT-SW.
           PERFORM P8600-REPORT THRU P8600-EXIT.
           MOVE ZERO TO RETURN-CODE.

       P8000-EXIT.
           EXIT.

       P8600-REPORT.
           DISPLAY 'CABSXDTL ENTRIES     ' WS-ENTRY-CNT.
           DISPLAY 'CABSXDTL DETAIL IN   ' WS-DETAIL-CNT.
           DISPLAY 'CABSXDTL GROUPS      ' WS-GROUP-CNT.
           DISPLAY 'CABSXDTL RELEASED    ' WS-RELEASED-CNT.
           DISPLAY 'CABSXDTL ABSORBED    ' WS-ABSORBED-CNT.
           DISPLAY 'CABSXDTL TOTAL QTY   ' WS-GT-QUANTITY.
           DISPLAY 'CABSXDTL TOTAL AMT   ' WS-GT-AMOUNT.

       P8600-EXIT.
           EXIT.
