       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABSXRTY.
      *****************************************************************
      * CABSXRTY - SORT E35 OUTPUT EXIT - RATED DETAIL REFORMAT AND   *
      *            RETRY COUNT RESET                                  *
      * APPLICATION : CABS                                            *
      * COMPILER    : ENTERPRISE COBOL                                *
      * INVOKED BY  : OS SORT/MERGE, MODS STATEMENT ON CONTROL CARD   *
      *               MEMBER JCL/CTLCARDS/MVT/CABSRT05 -              *
      *               E35=(CABSXRTY,4096,SORTEXIT,N)                  *
      * INPUTS      : ONE 200 BYTE RATED DETAIL RECORD PER ENTRY FROM *
      *               THE FINAL MERGE                                 *
      * OUTPUTS     : THE SAME RECORD REFORMATTED, WITH COLUMNS 165   *
      *               THROUGH 167 SET TO ZEROS                        *
      * CONTROL     : NONE - EXITS DO NOT WRITE CTLOUT, CABS-STD-041  *
      * BALANCE     : RECORDS IN = RECORDS OUT                        *
      * RESTART     : NOT RESTARTABLE - RERUN THE WHOLE SORT STEP     *
      *                                                               *
      * LINKAGE CONVENTION                                            *
      *   REGISTER 1 ADDRESSES A THREE WORD PARAMETER LIST.  WORD     *
      *   ONE IS THE ADDRESS OF THE RECORD LEAVING THE FINAL MERGE,   *
      *   OR BINARY ZERO WHEN THE MERGE IS EXHAUSTED.  WORD TWO IS    *
      *   THE ADDRESS OF THE RECORD JUST WRITTEN TO SORTOUT.  WORD    *
      *   THREE ADDRESSES THE LENGTH HALFWORD.  THE REPLY IS          *
      *   PLACED IN RETURN-CODE -                                     *
      *     00  NO MORE RECORDS TO INSERT - TAKE THE NEXT ONE         *
      *     04  DELETE THE RECORD                                     *
      *     08  WRITE THE RECORD ADDRESSED BY WORD ONE                *
      *     12  DO NOT ENTER THIS EXIT AGAIN                          *
      *     16  TERMINATE THE SORT.  WORKING STORAGE PERSISTS FOR     *
      *         THE LIFE OF THE SORT STEP.                            *
      *                                                               *
      * WHAT THIS EXIT DECIDES                                        *
      *   THE CARD CARRIED THE REFORMAT UNTIL THE MOVE TO THE MVT     *
      *   CONTROL CARD FORMAT.  IT IS NOW HERE.  THE RECORD IS        *
      *   REBUILT AS ITS LEADING 164 BYTES, A THREE BYTE ZONED        *
      *   FIELD AND ITS TRAILING 33 BYTES, WHICH RETURNS IT AT 200    *
      *   BYTES.  THE THREE BYTE FIELD IS THE RETRY COUNT IN          *
      *   COLUMNS 165 THROUGH 167 AND IT IS SET TO ZEROS ON EVERY     *
      *   RECORD THAT PASSES THROUGH, SO A RERUN DOES NOT INHERIT     *
      *   THE RETRY COUNT LEFT BY THE ATTEMPT THAT FAILED.  NO        *
      *   MODULE IN THE CABRAT FAMILY SETS THAT FIELD BACK.           *
      *                                                               *
      * REVISION HISTORY                                              *
      *   V1.00  1989-06-05  R.T.WHEELER   INITIAL - ASSEMBLER F      *
      *   V1.03  1993-01-18  D.OKONKWO     SECTION AND LINE ADDED     *
      *                                    TO THE SORT KEY            *
      *   V2.00  2006-02-27  P.NAIR        RECODED IN COBOL FOR LE    *
      *   V2.02  2012-07-09  A.BUKOWSKI    RETRY COUNT SET TO ZERO    *
      *                                    ON EVERY RECORD            *
      *   V2.03  2016-09-14  T.YAMASHITA   LENGTH HALFWORD CHECKED    *
      *   V2.05  2019-02-11  J.CALLAGHAN   REFORMAT MOVED OFF THE     *
      *                                    CONTROL CARD               *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
      * THIS EXIT WRITES NO CONTROL RECORD.  THE BALANCE OF
      * CABRAT10, WHICH READS SORTOUT, IS UNAFFECTED BY THIS
      * MODULE - IT REPORTS WHAT IT READS AS ITS OWN CT-READ.
      *
      * COUNTERS SURVIVE FROM ONE ENTRY TO THE NEXT.  THE SORT
      * STEP LOADS THIS MODULE ONCE AND HOLDS IT FOR THE PASS.
      *
       01  WS-MODULE-IDENT.
           05  FILLER                  PIC X(08) VALUE 'CABSXRTY'.
           05  FILLER                  PIC X(08) VALUE ' V2.05  '.
       01  WS-COUNTERS.
           05  WS-ENTRY-CNT            PIC S9(11) COMP-3 VALUE 0.
           05  WS-RESET-CNT            PIC S9(11) COMP-3 VALUE 0.
           05  WS-STALE-CNT            PIC S9(11) COMP-3 VALUE 0.
           05  WS-RETRY-BAD-CNT        PIC S9(09) COMP-3 VALUE 0.
           05  WS-LEN-ODD-CNT          PIC S9(09) COMP-3 VALUE 0.
           05  WS-BAN-CNT              PIC S9(09) COMP-3 VALUE 0.
           05  WS-RETRY-SUM            PIC S9(11) COMP-3 VALUE 0.
           05  WS-RETRY-HIGH           PIC S9(03) COMP-3 VALUE 0.
           05  WS-AMT-TOTAL            PIC S9(15)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-SWITCHES.
           05  WS-FIRST-ENTRY-SW       PIC X(01) VALUE 'Y'.
               88  WS-FIRST-ENTRY      VALUE 'Y'.
           05  WS-MERGE-DONE-SW        PIC X(01) VALUE 'N'.
               88  WS-MERGE-DONE       VALUE 'Y'.
       01  WS-WORK-FIELDS.
           05  WS-RETRY-TEST           PIC X(03) VALUE SPACES.
           05  WS-RETRY-NUM REDEFINES WS-RETRY-TEST
                                       PIC 9(03).
           05  WS-LAST-BAN             PIC X(13) VALUE SPACES.
           05  WS-EXPECT-LEN           PIC S9(04) COMP VALUE 200.
      *
      * THE REFORMAT WORK AREA.  THE RECORD IS REBUILT HERE AND
      * THEN MOVED BACK OVER THE INPUT AREA.  THE THREE MIDDLE
      * BYTES ARE THE ONLY PART OF THE RECORD THIS EXIT CHANGES.
      *
       01  WS-RATED-WORK.
           05  WS-RW-LEAD              PIC X(164).
           05  WS-RW-RETRY             PIC 9(03).
           05  WS-RW-TAIL              PIC X(33).
      *
      * TWO VIEWS OF THE SAME TWO HUNDRED BYTES ARE ADDRESSED ON
      * EVERY ENTRY.  THE NAMED VIEW IS USED FOR THE FIELDS THIS
      * EXIT READS AND THE BLOCK VIEW IS USED FOR THE REBUILD.
      *
       LINKAGE SECTION.
       01  LK-PARM-LIST.
           05  LK-RECORD-PTR           POINTER.
           05  LK-PREV-PTR             POINTER.
           05  LK-LENGTH-PTR           POINTER.
       01  LK-LENGTH                   PIC S9(04) COMP.
       01  LK-SORT-RECORD.
           05  LK-RT-REC-TYPE          PIC X(02).
           05  LK-RT-USAGE-TYPE        PIC X(01).
           05  LK-RT-FILLER-1          PIC X(01).
           05  LK-RT-OCN               PIC X(04).
           05  LK-RT-BAN               PIC X(13).
           05  LK-RT-SEQ-NBR           PIC 9(09) COMP-3.
           05  LK-RT-RATE-ELEM         PIC X(06).
           05  LK-RT-STATE-CD          PIC X(02).
           05  LK-RT-CONN-YYDDD        PIC 9(05).
           05  LK-RT-JURIS-CD          PIC X(01).
           05  LK-RT-BILL-PERIOD       PIC 9(06).
           05  LK-RT-CYCLE-YYDDD       PIC 9(05).
           05  LK-RT-TRUNK-GRP         PIC X(08).
           05  LK-RT-CIRCUIT-ID        PIC X(10).
           05  LK-RT-USOC              PIC X(05).
           05  LK-RT-FILLER-2          PIC X(14).
           05  LK-RT-CHG-MIN           PIC S9(11)V9(02) COMP-3.
           05  LK-RT-RATE              PIC S9(09)V9(05) COMP-3.
           05  LK-RT-AMOUNT            PIC S9(13)V9(05) COMP-3.
           05  LK-RT-FILLER-3          PIC X(36).
           05  LK-RT-SECTION           PIC X(03).
           05  LK-RT-LINE-NBR          PIC X(04).
           05  LK-RT-FILLER-4          PIC X(08).
           05  LK-RT-RETRY-COUNT       PIC X(03).
           05  LK-RT-TAIL              PIC X(33).
       01  LK-SORT-IMAGE.
           05  LK-IM-LEAD              PIC X(164).
           05  LK-IM-RETRY             PIC X(03).
           05  LK-IM-TAIL              PIC X(33).
      *
       PROCEDURE DIVISION USING LK-PARM-LIST.
       P0000-MAINLINE.
           MOVE ZERO TO RETURN-CODE.
           IF WS-FIRST-ENTRY
               PERFORM P1000-INIT THRU P1000-EXIT
           END-IF.
           IF LK-RECORD-PTR = NULL
               MOVE 'Y' TO WS-MERGE-DONE-SW
               PERFORM P8000-END-OF-MERGE THRU P8000-EXIT
               GOBACK
           END-IF.
           SET ADDRESS OF LK-SORT-RECORD TO LK-RECORD-PTR.
           SET ADDRESS OF LK-SORT-IMAGE  TO LK-RECORD-PTR.
           ADD 1 TO WS-ENTRY-CNT.
           PERFORM P2000-CHECK-LENGTH THRU P2000-EXIT.
           PERFORM P3000-EXAMINE-RETRY THRU P3000-EXIT.
           PERFORM P4000-REFORMAT THRU P4000-EXIT.
           PERFORM P5000-TRACK-ACCOUNT THRU P5000-EXIT.
           MOVE 8 TO RETURN-CODE.
           GOBACK.

       P1000-INIT.
           MOVE 'N' TO WS-FIRST-ENTRY-SW.
           MOVE SPACES TO WS-LAST-BAN.
           DISPLAY 'CABSXRTY ENTERED - RATED DETAIL REFORMAT'.

       P1000-EXIT.
           EXIT.

       P2000-CHECK-LENGTH.
      * WORD THREE ADDRESSES THE LENGTH HALFWORD.  THE CARD FIXES
      * THE RECORD AT TWO HUNDRED BYTES, SO THE HALFWORD IS READ
      * ONLY AS A CHECK ON THE LINKAGE AND IS NEVER ALTERED.
           SET ADDRESS OF LK-LENGTH TO LK-LENGTH-PTR.
           IF LK-LENGTH NOT = WS-EXPECT-LEN
               ADD 1 TO WS-LEN-ODD-CNT
           END-IF.

       P2000-EXIT.
           EXIT.

       P3000-EXAMINE-RETRY.
      * READ THE INBOUND RETRY COUNT BEFORE IT IS REPLACED.  A
      * NON ZERO VALUE HERE MEANS THE RECORD CARRIES THE COUNT
      * FROM THE ATTEMPT THAT DID NOT FINISH.  THE VALUES ARE
      * TALLIED FOR THE MESSAGE DATA SET AND NOWHERE ELSE.
           MOVE LK-RT-RETRY-COUNT TO WS-RETRY-TEST.
           IF WS-RETRY-TEST NOT NUMERIC
               ADD 1 TO WS-RETRY-BAD-CNT
               GO TO P3000-EXIT
           END-IF.
           IF WS-RETRY-NUM = ZERO
               GO TO P3000-EXIT
           END-IF.
           ADD 1 TO WS-STALE-CNT.
           ADD WS-RETRY-NUM TO WS-RETRY-SUM.
           IF WS-RETRY-NUM > WS-RETRY-HIGH
               MOVE WS-RETRY-NUM TO WS-RETRY-HIGH
           END-IF.

       P3000-EXIT.
           EXIT.

       P4000-REFORMAT.
      * REBUILD THE RECORD FROM ITS THREE PIECES.  THE LEADING
      * ONE HUNDRED AND SIXTY FOUR BYTES AND THE TRAILING THIRTY
      * THREE ARE CARRIED ACROSS UNCHANGED.  THE MIDDLE THREE
      * BYTES ARE REPLACED BY A ZONED ZERO, WHICH IS THE RESET.
           MOVE LK-IM-LEAD TO WS-RW-LEAD.
           MOVE LK-IM-TAIL TO WS-RW-TAIL.
           MOVE ZERO       TO WS-RW-RETRY.
           MOVE WS-RATED-WORK TO LK-SORT-IMAGE.
           ADD 1 TO WS-RESET-CNT.
           ADD LK-RT-AMOUNT TO WS-AMT-TOTAL.

       P4000-EXIT.
           EXIT.

       P5000-TRACK-ACCOUNT.
      * THE BAN IS THE HIGH ORDER SORT KEY, SO EVERY CHANGE OF
      * BAN IS A NEW ACCOUNT.  THE COUNT IS REPORTED SO THE RUN
      * SHEET SHOWS HOW MANY ACCOUNTS THE BUILD STEP WILL SEE.
           IF LK-RT-BAN = WS-LAST-BAN
               GO TO P5000-EXIT
           END-IF.
           ADD 1 TO WS-BAN-CNT.
           MOVE LK-RT-BAN TO WS-LAST-BAN.

       P5000-EXIT.
           EXIT.

       P8000-END-OF-MERGE.
      * A NULL RECORD ADDRESS MEANS THE MERGE IS EXHAUSTED.  THE
      * EXIT HAS NOTHING TO INSERT, SO IT REPLIES ZERO AND
      * WRITES ITS TALLIES TO THE MESSAGE DATA SET.
           DISPLAY 'CABSXRTY ENTRIES     ' WS-ENTRY-CNT.
           DISPLAY 'CABSXRTY RESET       ' WS-RESET-CNT.
           DISPLAY 'CABSXRTY WAS NON ZERO' WS-STALE-CNT.
           DISPLAY 'CABSXRTY HIGHEST     ' WS-RETRY-HIGH.
           DISPLAY 'CABSXRTY RETRY SUM   ' WS-RETRY-SUM.
           DISPLAY 'CABSXRTY NON NUMERIC ' WS-RETRY-BAD-CNT.
           DISPLAY 'CABSXRTY LENGTH OTHER' WS-LEN-ODD-CNT.
           DISPLAY 'CABSXRTY ACCOUNTS    ' WS-BAN-CNT.
           DISPLAY 'CABSXRTY AMOUNT      ' WS-AMT-TOTAL.
           MOVE ZERO TO RETURN-CODE.

       P8000-EXIT.
           EXIT.
