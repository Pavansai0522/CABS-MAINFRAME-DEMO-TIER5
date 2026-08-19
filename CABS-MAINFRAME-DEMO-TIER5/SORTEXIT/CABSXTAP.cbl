       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABSXTAP.
      *****************************************************************
      * CABSXTAP - SORT E35 OUTPUT EXIT - TAPE DESPATCH SELECTION     *
      * APPLICATION : CABS                                            *
      * COMPILER    : ENTERPRISE COBOL                                *
      * INVOKED BY  : OS SORT/MERGE, MODS E35=(CABSXTAP,8192)         *
      *               ON CONTROL CARD MEMBER                          *
      *               JCL/CTLCARDS/MVT/CABSRT14                       *
      * INPUTS      : ONE 400 BYTE MEDIA RECORD PER ENTRY LEAVING     *
      *               THE FINAL MERGE, IN TYPE / CARRIER / PERIOD     *
      *               ORDER                                           *
      * OUTPUTS     : THE SAME RECORD UNCHANGED, OR A DELETE REPLY    *
      * CONTROL     : NONE - EXITS DO NOT WRITE CTLOUT, CABS-STD-041  *
      * BALANCE     : MERGE OUT = RECORDS WRITTEN + WS-DROP-TOTAL     *
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
      *   THE EXIT IS ENTERED A FINAL TIME WITH A NULL RECORD         *
      *   ADDRESS.  WORKING STORAGE PERSISTS FOR THE LIFE OF THE      *
      *   SORT STEP AND EVERY ACCUMULATOR HERE RELIES ON THAT.        *
      *                                                               *
      * WHAT THIS EXIT DECIDES                                        *
      *   A DETAIL RECORD - TYPE BYTE D AT POSITION 1 - WHOSE         *
      *   PACKED AMOUNT AT POSITION 375 FOR EIGHT BYTES IS ZERO IS    *
      *   NOT WRITTEN TO THE TAPE.  LABEL AND TRAILER RECORDS ARE     *
      *   NEVER DELETED, WHATEVER THEIR AMOUNT FIELD HOLDS.  THE      *
      *   RULE WAS CARRIED AS AN OUTFIL OMIT OPERAND ON THE CONTROL   *
      *   CARD UNTIL THE STEP WAS MOVED TO A SORT THAT HAS NO         *
      *   OUTFIL STATEMENT.                                           *
      *   THE TRAILER HASH WRITTEN BY CABFMT07 IS BUILT BEFORE THIS   *
      *   SORT RUNS AND STILL INCLUDES THE DROPPED RECORDS, SO THE    *
      *   BUREAU RECONCILIATION IS RUN TO A TOLERANCE AGREED IN       *
      *   1997.  THE TALLY BELOW IS THE ONLY PLACE THE SIZE OF THAT   *
      *   DIFFERENCE IS RECORDED.                                     *
      *   THE CALLING PROGRAM'S BALANCE IS UNAFFECTED BY THIS         *
      *   MODULE - A SORT STEP WRITES NO CONTROL RECORD OF ITS OWN.   *
      *                                                               *
      * REVISION HISTORY                                              *
      *   V1.00  1991-01-30  D.OKONKWO     INITIAL - ASSEMBLER F      *
      *   V1.03  1997-05-06  B.R.HALVORSEN ZERO VALUE DETAIL LINES    *
      *                                    TAKEN OFF THE VOLUME       *
      *   V1.05  2001-02-13  J.CALLAGHAN   LABEL RECORDS EXEMPTED     *
      *   V2.00  2006-11-27  P.NAIR        RECODED IN COBOL FOR LE    *
      *   V2.02  2010-04-09  E.KOWALCZYK   TRAILER HASH CAPTURED      *
      *   V2.04  2015-08-31  G.PETRAKIS    MINUTE TALLY SPLIT OUT     *
      *   V2.05  2019-02-06  M.HAAS        RECOMPILE ONLY - LE V6.2   *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-MODULE-IDENT.
           05  FILLER                  PIC X(08) VALUE 'CABSXTAP'.
           05  FILLER                  PIC X(08) VALUE ' V2.05  '.
       01  WS-SWITCHES.
           05  WS-FIRST-ENTRY-SW       PIC X(01) VALUE 'Y'.
               88  WS-FIRST-ENTRY      VALUE 'Y'.
           05  WS-TRAILER-SEEN-SW      PIC X(01) VALUE 'N'.
               88  WS-TRAILER-SEEN     VALUE 'Y'.
       01  WS-COUNTERS.
           05  WS-ENTRY-CNT            PIC S9(11) COMP-3 VALUE 0.
           05  WS-WRITTEN-CNT          PIC S9(11) COMP-3 VALUE 0.
           05  WS-DROP-TOTAL           PIC S9(11) COMP-3 VALUE 0.
           05  WS-LABEL-CNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-DETAIL-IN            PIC S9(11) COMP-3 VALUE 0.
           05  WS-DETAIL-OUT           PIC S9(11) COMP-3 VALUE 0.
           05  WS-TRAILER-CNT          PIC S9(09) COMP-3 VALUE 0.
           05  WS-OTHER-TYPE-CNT       PIC S9(09) COMP-3 VALUE 0.
           05  WS-DROP-WITH-MOU        PIC S9(09) COMP-3 VALUE 0.
           05  WS-DROP-WITH-TAX        PIC S9(09) COMP-3 VALUE 0.
           05  WS-CARRIER-CNT          PIC S9(07) COMP-3 VALUE 0.
      *
      * WHAT WAS DROPPED.  THE AMOUNT TALLY ADDS THE AMOUNT FIELD
      * OF EVERY DELETED RECORD AND IS THEREFORE ZERO BY THE TEST
      * THAT SELECTED THEM.  IT IS STILL PRINTED, BECAUSE A NON
      * ZERO FIGURE THERE WOULD MEAN THE PACKED FIELD AT 375 IS NOT
      * THE FIELD THIS VIEW SAYS IT IS.  THE QUANTITY AND MINUTE
      * TALLIES ARE THE ONES THAT CARRY WEIGHT.
      *
       01  WS-DROP-VALUES.
           05  WS-DV-AMOUNT            PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DV-QUANTITY          PIC S9(15)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DV-MINUTES           PIC S9(15)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DV-TAX               PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-KEPT-VALUES.
           05  WS-KV-AMOUNT            PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-KV-QUANTITY          PIC S9(15)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-KV-MINUTES           PIC S9(15)V9(02) COMP-3
                                                  VALUE 0.
      *
      * THE TRAILER RECORD CARRIES THE COUNT AND THE AMOUNT HASH
      * THE BUREAU BALANCES AGAINST.  THEY ARE CAPTURED AS THE
      * TRAILER PASSES THROUGH AND COMPARED WITH WHAT WAS ACTUALLY
      * WRITTEN, SO THE OPERATIONS LOG SHOWS THE DIFFERENCE THE
      * TOLERANCE HAS TO ABSORB.
      *
       01  WS-TRAILER-HASH.
           05  WS-TH-DETAIL-CNT        PIC S9(11) COMP-3 VALUE 0.
           05  WS-TH-AMOUNT            PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-TH-CNT-DIFF          PIC S9(11) COMP-3 VALUE 0.
           05  WS-TH-AMT-DIFF          PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-WORK-FIELDS.
           05  WS-LAST-CARRIER         PIC X(14) VALUE SPACES.
      *
       LINKAGE SECTION.
       01  LK-PARM-LIST.
           05  LK-RECORD-PTR           POINTER.
           05  LK-PREV-PTR             POINTER.
           05  LK-LENGTH-PTR           POINTER.
      *
      * A HAND MAINTAINED VIEW OF THE 400 BYTE MEDIA RECORD.  THE
      * POSITIONS THE CONTROL CARD QUOTES ARE THE POSITIONS IN THIS
      * VIEW - 1,1 TYPE BYTE, 15,14 CARRIER KEY, 29,6 BILL PERIOD
      * AND 375,8 PACKED AMOUNT.
      *
       01  LK-SORT-RECORD.
           05  LK-MT-REC-TYPE          PIC X(01).
               88  LK-MT-LABEL         VALUE 'L'.
               88  LK-MT-DETAIL        VALUE 'D'.
               88  LK-MT-TRAILER       VALUE 'T'.
           05  LK-MT-FILLER-1          PIC X(13).
           05  LK-MT-CARRIER-KEY       PIC X(14).
           05  LK-MT-BILL-PERIOD       PIC X(06).
           05  LK-MT-BAN               PIC X(13).
           05  LK-MT-OCN               PIC X(04).
           05  LK-MT-RAO               PIC 9(03).
           05  LK-MT-INVOICE-NBR       PIC X(12).
           05  LK-MT-CIRCUIT-ID        PIC X(20).
           05  LK-MT-USOC              PIC X(05).
           05  LK-MT-RATE-ELEM         PIC X(06).
           05  LK-MT-JURIS-CD          PIC X(01).
           05  LK-MT-TEXT              PIC X(60).
           05  LK-MT-EMI-AREA          PIC X(180).
           05  LK-MT-QUANTITY          PIC S9(13)V9(02) COMP-3.
           05  LK-MT-MINUTES           PIC S9(13)V9(02) COMP-3.
           05  LK-MT-TAX-AMT           PIC S9(10)V9(05) COMP-3.
           05  LK-MT-FILLER-2          PIC X(12).
           05  LK-MT-AMOUNT            PIC S9(10)V9(05) COMP-3.
           05  LK-MT-TAIL              PIC X(18).
      *
      * THE TRAILER USES THE SAME BYTES FOR ITS HASH FIELDS.  THE
      * COUNT SITS WHERE THE QUANTITY SITS ON A DETAIL RECORD AND
      * THE HASH AMOUNT SITS WHERE THE AMOUNT SITS.
      *
       01  LK-TRAILER-RECORD.
           05  LK-TR-REC-TYPE          PIC X(01).
           05  LK-TR-FILLER-1          PIC X(13).
           05  LK-TR-CARRIER-KEY       PIC X(14).
           05  LK-TR-BILL-PERIOD       PIC X(06).
           05  LK-TR-FILLER-2          PIC X(304).
           05  LK-TR-DETAIL-CNT        PIC S9(13)V9(02) COMP-3.
           05  LK-TR-FILLER-3          PIC X(28).
           05  LK-TR-HASH-AMOUNT       PIC S9(10)V9(05) COMP-3.
           05  LK-TR-FILLER-4          PIC X(18).
      *
       PROCEDURE DIVISION USING LK-PARM-LIST.
       P0000-MAINLINE.
           MOVE ZERO TO RETURN-CODE.
           IF WS-FIRST-ENTRY
               MOVE 'N' TO WS-FIRST-ENTRY-SW
               DISPLAY 'CABSXTAP ENTERED - TAPE DESPATCH SELECTION'
           END-IF.
           IF LK-RECORD-PTR = NULL
               PERFORM P8000-END-OF-MERGE THRU P8000-EXIT
               GOBACK
           END-IF.
           SET ADDRESS OF LK-SORT-RECORD TO LK-RECORD-PTR.
           ADD 1 TO WS-ENTRY-CNT.
           PERFORM P2000-SCREEN-RECORD THRU P2000-EXIT.
           IF RETURN-CODE = 4
               GOBACK
           END-IF.
           PERFORM P3000-COUNT-WRITTEN THRU P3000-EXIT.
           MOVE ZERO TO RETURN-CODE.
           GOBACK.

       P2000-SCREEN-RECORD.
      * ONLY A DETAIL RECORD IS ELIGIBLE FOR DELETION.  THE LABEL
      * AT THE FRONT AND THE TRAILER AT THE BACK OF EACH CARRIER
      * GROUP ARE PASSED THROUGH WHATEVER THEY CARRY, BECAUSE THE
      * BUREAU READS THE VOLUME BY THOSE MARKERS.
           EVALUATE TRUE
               WHEN LK-MT-LABEL
                   ADD 1 TO WS-LABEL-CNT
                   PERFORM P2200-STEP-CARRIER THRU P2200-EXIT
               WHEN LK-MT-TRAILER
                   ADD 1 TO WS-TRAILER-CNT
                   PERFORM P2600-CAPTURE-HASH THRU P2600-EXIT
               WHEN LK-MT-DETAIL
                   ADD 1 TO WS-DETAIL-IN
                   PERFORM P2800-TEST-AMOUNT THRU P2800-EXIT
               WHEN OTHER
                   ADD 1 TO WS-OTHER-TYPE-CNT
           END-EVALUATE.

       P2000-EXIT.
           EXIT.

       P2200-STEP-CARRIER.
      * ONE LABEL PER CARRIER GROUP.  A CHANGE OF CARRIER KEY ON
      * THE LABEL IS COUNTED SO THE LOG SHOWS HOW MANY GROUPS THE
      * VOLUME HAS TO BE SPLIT INTO AT THE BUREAU.
           IF LK-MT-CARRIER-KEY NOT = WS-LAST-CARRIER
               ADD 1 TO WS-CARRIER-CNT
               MOVE LK-MT-CARRIER-KEY TO WS-LAST-CARRIER
           END-IF.

       P2200-EXIT.
           EXIT.

       P2600-CAPTURE-HASH.
      * THE TRAILER HASH IS ACCUMULATED ACROSS ALL CARRIER GROUPS
      * ON THE VOLUME.  IT IS TAKEN FROM THE RECORD AS BUILT BY
      * CABFMT07, WHICH COUNTED EVERY DETAIL RECORD IT WROTE.
           SET ADDRESS OF LK-TRAILER-RECORD TO LK-RECORD-PTR.
           ADD LK-TR-DETAIL-CNT   TO WS-TH-DETAIL-CNT.
           ADD LK-TR-HASH-AMOUNT  TO WS-TH-AMOUNT.
           MOVE 'Y' TO WS-TRAILER-SEEN-SW.

       P2600-EXIT.
           EXIT.

       P2800-TEST-AMOUNT.
      * THE AMOUNT AT 375 IS THE BILLED VALUE OF THE LINE.  A ZERO
      * THERE MEANS THE LINE PRICED TO NOTHING AND THE BUREAU DOES
      * NOT PRINT IT, SO IT IS NOT SENT.
           IF LK-MT-AMOUNT NOT = ZERO
               GO TO P2800-EXIT
           END-IF.
           ADD 1 TO WS-DROP-TOTAL.
           ADD LK-MT-AMOUNT   TO WS-DV-AMOUNT.
           ADD LK-MT-QUANTITY TO WS-DV-QUANTITY.
           ADD LK-MT-MINUTES  TO WS-DV-MINUTES.
           ADD LK-MT-TAX-AMT  TO WS-DV-TAX.
           IF LK-MT-MINUTES NOT = ZERO
               ADD 1 TO WS-DROP-WITH-MOU
           END-IF.
           IF LK-MT-TAX-AMT NOT = ZERO
               ADD 1 TO WS-DROP-WITH-TAX
           END-IF.
           MOVE 4 TO RETURN-CODE.

       P2800-EXIT.
           EXIT.

       P3000-COUNT-WRITTEN.
           ADD 1 TO WS-WRITTEN-CNT.
           IF LK-MT-DETAIL
               ADD 1 TO WS-DETAIL-OUT
               ADD LK-MT-AMOUNT   TO WS-KV-AMOUNT
               ADD LK-MT-QUANTITY TO WS-KV-QUANTITY
               ADD LK-MT-MINUTES  TO WS-KV-MINUTES
           END-IF.

       P3000-EXIT.
           EXIT.

       P8000-END-OF-MERGE.
      * WORK OUT THE DIFFERENCE BETWEEN WHAT THE TRAILER SAYS IS ON
      * THE VOLUME AND WHAT WAS ACTUALLY WRITTEN, THEN REPLY ZERO
      * SO SORT CAN CLOSE SORTOUT.  NOTHING IS INSERTED HERE - THE
      * TRAILER RECORDS ARE PART OF THE DATA AND HAVE ALREADY GONE
      * OUT AS THEY STOOD.
           COMPUTE WS-TH-CNT-DIFF =
                   WS-TH-DETAIL-CNT - WS-DETAIL-OUT.
           COMPUTE WS-TH-AMT-DIFF =
                   WS-TH-AMOUNT - WS-KV-AMOUNT.
           PERFORM P8600-REPORT THRU P8600-EXIT.
           MOVE ZERO TO RETURN-CODE.

       P8000-EXIT.
           EXIT.

       P8600-REPORT.
           DISPLAY 'CABSXTAP ENTRIES     ' WS-ENTRY-CNT.
           DISPLAY 'CABSXTAP WRITTEN     ' WS-WRITTEN-CNT.
           DISPLAY 'CABSXTAP LABELS      ' WS-LABEL-CNT.
           DISPLAY 'CABSXTAP TRAILERS    ' WS-TRAILER-CNT.
           DISPLAY 'CABSXTAP CARRIER GRPS' WS-CARRIER-CNT.
           DISPLAY 'CABSXTAP OTHER TYPE  ' WS-OTHER-TYPE-CNT.
           DISPLAY 'CABSXTAP DETAIL IN   ' WS-DETAIL-IN.
           DISPLAY 'CABSXTAP DETAIL OUT  ' WS-DETAIL-OUT.
           DISPLAY 'CABSXTAP DROPPED     ' WS-DROP-TOTAL.
           DISPLAY 'CABSXTAP DROP AMT    ' WS-DV-AMOUNT.
           DISPLAY 'CABSXTAP DROP QTY    ' WS-DV-QUANTITY.
           DISPLAY 'CABSXTAP DROP MOU    ' WS-DV-MINUTES.
           DISPLAY 'CABSXTAP DROP TAX    ' WS-DV-TAX.
           DISPLAY 'CABSXTAP DROP W MOU  ' WS-DROP-WITH-MOU.
           DISPLAY 'CABSXTAP DROP W TAX  ' WS-DROP-WITH-TAX.
           DISPLAY 'CABSXTAP KEPT AMT    ' WS-KV-AMOUNT.
           DISPLAY 'CABSXTAP KEPT QTY    ' WS-KV-QUANTITY.
           DISPLAY 'CABSXTAP KEPT MOU    ' WS-KV-MINUTES.
           DISPLAY 'CABSXTAP TRLR COUNT  ' WS-TH-DETAIL-CNT.
           DISPLAY 'CABSXTAP TRLR AMOUNT ' WS-TH-AMOUNT.
           DISPLAY 'CABSXTAP TRLR CNT DIF' WS-TH-CNT-DIFF.
           DISPLAY 'CABSXTAP TRLR AMT DIF' WS-TH-AMT-DIFF.

       P8600-EXIT.
           EXIT.
