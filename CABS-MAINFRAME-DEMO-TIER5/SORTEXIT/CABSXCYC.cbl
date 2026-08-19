       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABSXCYC.
      *****************************************************************
      * CABSXCYC - MERGE E35 OUTPUT EXIT - BILLING CYCLE SELECTION    *
      *            AND MERGE INPUT ORDER CHECK                        *
      * APPLICATION : CABS                                            *
      * COMPILER    : ENTERPRISE COBOL                                *
      * INVOKED BY  : OS SORT/MERGE, MODS STATEMENT ON CONTROL CARD   *
      *               MEMBER JCL/CTLCARDS/MVT/CABSRT06 -              *
      *               E35=(CABSXCYC,4096,SORTEXIT,N)                  *
      * INPUTS      : ONE 200 BYTE RATED RECORD PER ENTRY FROM THE    *
      *               FINAL MERGE OF THE THREE ELEMENT TYPE STREAMS   *
      * OUTPUTS     : THE SAME RECORD UNCHANGED, OR A DELETE REPLY    *
      * CONTROL     : NONE - EXITS DO NOT WRITE CTLOUT, CABS-STD-041  *
      * BALANCE     : RECORDS IN = WS-KEPT-CNT + WS-DROP-CNT          *
      * RESTART     : NOT RESTARTABLE - RERUN THE WHOLE MERGE STEP    *
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
      *     16  TERMINATE THE MERGE.  WORKING STORAGE PERSISTS FOR    *
      *         THE LIFE OF THE STEP.                                 *
      *                                                               *
      * WHAT THIS EXIT DECIDES                                        *
      *   THE CARD CARRIED THE CYCLE SELECTION UNTIL THE MOVE TO      *
      *   THE MVT CONTROL CARD FORMAT.  IT IS NOW HERE.  ONLY         *
      *   RECORDS WHOSE BILL PERIOD IN COLUMNS 180 THROUGH 185        *
      *   MATCHES WS-CYCLE-KEEP ARE WRITTEN TO SORTOUT.  THAT         *
      *   VALUE WAS TYPED ONTO THE CONTROL CARD ON THE FIRST          *
      *   WORKING DAY OF EACH BILL PERIOD.  IT IS NOW A LITERAL IN    *
      *   THIS SOURCE, SO THE SAME MAINTENANCE IS A RECOMPILE AND     *
      *   A RELINK OF THIS MODULE ON THE FIRST WORKING DAY OF EACH    *
      *   BILL PERIOD.                                                *
      *   THE EXIT ALSO COMPARES EACH RECORD'S BAN AND SEQUENCE       *
      *   WITH THE ONE BEFORE IT.  SORT DOES NOT VALIDATE THAT        *
      *   MERGE INPUTS ARRIVE IN KEY ORDER, SO A DESCENDING KEY       *
      *   HERE IS THE ONLY SIGN THAT ONE OF THE THREE INPUTS WAS      *
      *   PRESENTED OUT OF ORDER.  THE COUNT IS WRITTEN TO THE        *
      *   MESSAGE DATA SET AT END OF MERGE.                           *
      *                                                               *
      * REVISION HISTORY                                              *
      *   V1.00  1990-03-12  R.T.WHEELER   INITIAL - ASSEMBLER F      *
      *   V1.05  1995-07-25  D.OKONKWO     UNE STREAM ADDED AS THE    *
      *                                    THIRD MERGE INPUT          *
      *   V2.00  2006-02-27  P.NAIR        RECODED IN COBOL FOR LE    *
      *   V2.02  2009-11-30  A.BUKOWSKI    CYCLE SELECTION ADDED      *
      *                                    AFTER THE GDG POINTER      *
      *                                    LEFT TWO CYCLES IN ONE     *
      *                                    GENERATION                 *
      *   V2.04  2015-04-16  L.FERREIRA    ORDER CHECK ADDED          *
      *   V2.06  2019-02-11  J.CALLAGHAN   CYCLE LITERAL MOVED OFF    *
      *                                    THE CONTROL CARD           *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
      * THIS EXIT WRITES NO CONTROL RECORD.  THE BALANCE OF
      * CABRAT11, WHICH READS SORTOUT, IS UNAFFECTED BY THIS
      * MODULE - IT REPORTS WHAT IT READS AS ITS OWN CT-READ.
      *
      * THE PREVIOUS KEY AND THE COUNTERS SURVIVE FROM ONE ENTRY
      * TO THE NEXT.  THE STEP LOADS THIS MODULE ONCE.
      *
       01  WS-MODULE-IDENT.
           05  FILLER                  PIC X(08) VALUE 'CABSXCYC'.
           05  FILLER                  PIC X(08) VALUE ' V2.06  '.
      *
      * THE BILL PERIOD THIS RUN ADMITS.  OPERATIONS UPDATES THE
      * LITERAL ON THE FIRST WORKING DAY OF EACH BILL PERIOD AND
      * THE MODULE IS RECOMPILED AND RELINKED BEFORE THE FIRST
      * RATING JOB OF THAT PERIOD IS SUBMITTED.
      *
       01  WS-CYCLE-LITERALS.
           05  FILLER                  PIC X(06) VALUE '202608'.
       01  WS-CYCLE-TABLE REDEFINES WS-CYCLE-LITERALS.
           05  WS-CYCLE-KEEP           PIC X(06).
       01  WS-COUNTERS.
           05  WS-ENTRY-CNT            PIC S9(11) COMP-3 VALUE 0.
           05  WS-KEPT-CNT             PIC S9(11) COMP-3 VALUE 0.
           05  WS-DROP-CNT             PIC S9(11) COMP-3 VALUE 0.
           05  WS-DESCENT-CNT          PIC S9(09) COMP-3 VALUE 0.
           05  WS-BLANK-PERIOD-CNT     PIC S9(09) COMP-3 VALUE 0.
           05  WS-UNKNOWN-STREAM       PIC S9(09) COMP-3 VALUE 0.
           05  WS-AMT-KEPT             PIC S9(15)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AMT-DROPPED          PIC S9(15)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-SWITCHES.
           05  WS-FIRST-ENTRY-SW       PIC X(01) VALUE 'Y'.
               88  WS-FIRST-ENTRY      VALUE 'Y'.
           05  WS-PREV-SET-SW          PIC X(01) VALUE 'N'.
               88  WS-PREV-SET         VALUE 'Y'.
           05  WS-STREAM-FOUND-SW      PIC X(01) VALUE 'N'.
               88  WS-STREAM-FOUND     VALUE 'Y'.
      *
      * THE THREE MERGE INPUTS.  THE CODES ARE HELD IN THE ORDER
      * THE STREAMS ARE NAMED ON THE MERGE STEP AND ARE WALKED
      * WITH A SEQUENTIAL SEARCH RATHER THAN SEARCH ALL.
      *
       01  WS-STREAM-LITERALS.
           05  FILLER  PIC X(14) VALUE 'SWASWITCHED   '.
           05  FILLER  PIC X(14) VALUE 'SPASPECIAL    '.
           05  FILLER  PIC X(14) VALUE 'UNEUNBUNDLED  '.
       01  WS-STREAM-TABLE REDEFINES WS-STREAM-LITERALS.
           05  WS-STREAM-ENTRY OCCURS 3 TIMES INDEXED BY WS-EX.
               10  WS-STREAM-CD        PIC X(03).
               10  WS-STREAM-NAME      PIC X(11).
       01  WS-STREAM-COUNTS.
           05  WS-STREAM-KEPT OCCURS 3 TIMES PIC S9(11) COMP-3
                                                  VALUE 0.
           05  WS-STREAM-DROP OCCURS 3 TIMES PIC S9(11) COMP-3
                                                  VALUE 0.
       01  WS-STREAM-MAX               PIC S9(04) COMP VALUE 3.
       01  WS-ORDER-CONTROL.
           05  WS-THIS-KEY.
               10  WS-TK-BAN           PIC X(13) VALUE SPACES.
               10  WS-TK-SEQ           PIC X(09) VALUE SPACES.
           05  WS-PREV-KEY.
               10  WS-PK-BAN           PIC X(13) VALUE SPACES.
               10  WS-PK-SEQ           PIC X(09) VALUE SPACES.
           05  WS-WARN-LIMIT           PIC S9(04) COMP VALUE 10.
           05  WS-WARN-SENT            PIC S9(04) COMP VALUE 0.
           05  WS-SX                   PIC S9(04) COMP VALUE 0.
      *
       LINKAGE SECTION.
       01  LK-PARM-LIST.
           05  LK-RECORD-PTR           POINTER.
           05  LK-PREV-PTR             POINTER.
           05  LK-LENGTH-PTR           POINTER.
       01  LK-SORT-RECORD.
           05  LK-MG-REC-TYPE          PIC X(02).
           05  LK-MG-USAGE-TYPE        PIC X(01).
           05  LK-MG-FILLER-1          PIC X(01).
           05  LK-MG-OCN               PIC X(04).
           05  LK-MG-BAN               PIC X(13).
           05  LK-MG-SEQ-KEY           PIC X(09).
           05  LK-MG-CIC               PIC X(04).
           05  LK-MG-CONN-YYDDD        PIC 9(05).
           05  LK-MG-JURIS-CD          PIC X(01).
           05  LK-MG-STATE-CD          PIC X(02).
           05  LK-MG-RATE-ELEM         PIC X(06).
           05  LK-MG-ELEM-TYPE         PIC X(03).
           05  LK-MG-TRUNK-GRP         PIC X(08).
           05  LK-MG-CIRCUIT-ID        PIC X(10).
           05  LK-MG-USOC              PIC X(05).
           05  LK-MG-FILLER-2          PIC X(30).
           05  LK-MG-AMOUNT            PIC S9(13)V9(05) COMP-3.
           05  LK-MG-CHG-MIN           PIC S9(11)V9(02) COMP-3.
           05  LK-MG-FILLER-3          PIC X(58).
           05  LK-MG-BILL-PERIOD       PIC X(06).
           05  LK-MG-TAIL              PIC X(15).
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
           PERFORM P2000-CHECK-ORDER THRU P2000-EXIT.
           PERFORM P3000-FIND-STREAM THRU P3000-EXIT.
           PERFORM P4000-APPLY-CYCLE THRU P4000-EXIT.
           GOBACK.

       P1000-INIT.
           MOVE 'N' TO WS-FIRST-ENTRY-SW.
           MOVE SPACES TO WS-PREV-KEY.
           MOVE 'N' TO WS-PREV-SET-SW.
           DISPLAY 'CABSXCYC ENTERED - CYCLE ' WS-CYCLE-KEEP.

       P1000-EXIT.
           EXIT.

       P2000-CHECK-ORDER.
      * THE MERGE KEY IS BAN THEN SEQUENCE.  A KEY LOWER THAN THE
      * ONE BEFORE IT MEANS ONE OF THE THREE INPUTS WAS NOT IN
      * BAN AND SEQUENCE ORDER WHEN THE STEP STARTED.  THE CHECK
      * IS MADE ON EVERY RECORD, INCLUDING THE ONES THE CYCLE
      * TEST LATER REMOVES, SO THE COUNT COVERS THE WHOLE MERGE.
           MOVE LK-MG-BAN     TO WS-TK-BAN.
           MOVE LK-MG-SEQ-KEY TO WS-TK-SEQ.
           IF NOT WS-PREV-SET
               MOVE 'Y' TO WS-PREV-SET-SW
               MOVE WS-THIS-KEY TO WS-PREV-KEY
               GO TO P2000-EXIT
           END-IF.
           IF WS-THIS-KEY < WS-PREV-KEY
               ADD 1 TO WS-DESCENT-CNT
               PERFORM P2400-WARN THRU P2400-EXIT
           END-IF.
           MOVE WS-THIS-KEY TO WS-PREV-KEY.

       P2000-EXIT.
           EXIT.

       P2400-WARN.
      * THE FIRST TEN DESCENDING KEYS ARE NAMED ON THE MESSAGE
      * DATA SET SO OPERATIONS CAN IDENTIFY WHICH INPUT TO
      * RESEQUENCE.  THE REST ARE COUNTED ONLY.
           IF WS-WARN-SENT NOT < WS-WARN-LIMIT
               GO TO P2400-EXIT
           END-IF.
           ADD 1 TO WS-WARN-SENT.
           DISPLAY 'CABSXCYC ORDER       '
                   WS-PREV-KEY ' BEFORE ' WS-THIS-KEY.

       P2400-EXIT.
           EXIT.

       P3000-FIND-STREAM.
      * COLUMNS 49 THROUGH 51 CARRY THE ELEMENT TYPE STAMPED BY
      * THE RATING STEP THAT CUT THE RECORD.  IT IS USED HERE
      * ONLY TO SPLIT THE TALLIES BY MERGE INPUT.
           MOVE 'N' TO WS-STREAM-FOUND-SW.
           SET WS-EX TO 1.
           SEARCH WS-STREAM-ENTRY
               AT END
                   ADD 1 TO WS-UNKNOWN-STREAM
               WHEN WS-STREAM-CD (WS-EX) = LK-MG-ELEM-TYPE
                   MOVE 'Y' TO WS-STREAM-FOUND-SW
           END-SEARCH.

       P3000-EXIT.
           EXIT.

       P4000-APPLY-CYCLE.
      * THE ONE RULE THIS EXIT ENFORCES.  A BILL PERIOD THAT DOES
      * NOT MATCH THE LITERAL IS REMOVED BEFORE SORTOUT IS
      * WRITTEN.  A BLANK BILL PERIOD IS REMOVED WITH THE REST
      * AND IS COUNTED ON ITS OWN LINE.
           IF LK-MG-BILL-PERIOD = SPACES
               ADD 1 TO WS-BLANK-PERIOD-CNT
           END-IF.
           IF LK-MG-BILL-PERIOD = WS-CYCLE-KEEP
               PERFORM P4200-KEEP THRU P4200-EXIT
               GO TO P4000-EXIT
           END-IF.
           PERFORM P4400-DROP THRU P4400-EXIT.

       P4000-EXIT.
           EXIT.

       P4200-KEEP.
           ADD 1 TO WS-KEPT-CNT.
           ADD LK-MG-AMOUNT TO WS-AMT-KEPT.
           IF WS-STREAM-FOUND
               ADD 1 TO WS-STREAM-KEPT (WS-EX)
           END-IF.
           MOVE ZERO TO RETURN-CODE.

       P4200-EXIT.
           EXIT.

       P4400-DROP.
      * THE AMOUNT ON A REMOVED RECORD IS ACCUMULATED FOR THE
      * MESSAGE DATA SET ONLY.  NOTHING DOWNSTREAM IS TOLD THAT
      * A RECORD FOR ANOTHER BILL PERIOD WAS PRESENT.
           ADD 1 TO WS-DROP-CNT.
           ADD LK-MG-AMOUNT TO WS-AMT-DROPPED.
           IF WS-STREAM-FOUND
               ADD 1 TO WS-STREAM-DROP (WS-EX)
           END-IF.
           MOVE 4 TO RETURN-CODE.

       P4400-EXIT.
           EXIT.

       P8000-END-OF-MERGE.
      * A NULL RECORD ADDRESS MEANS THE MERGE IS EXHAUSTED.  THE
      * EXIT HAS NOTHING TO INSERT, SO IT REPLIES ZERO AND
      * WRITES ITS TALLIES TO THE MESSAGE DATA SET.
           DISPLAY 'CABSXCYC ENTRIES     ' WS-ENTRY-CNT.
           DISPLAY 'CABSXCYC CYCLE KEPT  ' WS-CYCLE-KEEP.
           DISPLAY 'CABSXCYC KEPT        ' WS-KEPT-CNT.
           DISPLAY 'CABSXCYC REMOVED     ' WS-DROP-CNT.
           DISPLAY 'CABSXCYC BLANK PERIOD' WS-BLANK-PERIOD-CNT.
           DISPLAY 'CABSXCYC ORDER FAULTS' WS-DESCENT-CNT.
           DISPLAY 'CABSXCYC UNKNOWN TYPE' WS-UNKNOWN-STREAM.
           DISPLAY 'CABSXCYC AMOUNT KEPT ' WS-AMT-KEPT.
           DISPLAY 'CABSXCYC AMOUNT OUT  ' WS-AMT-DROPPED.
           PERFORM P8200-STREAM-LINE THRU P8200-EXIT
                   VARYING WS-EX FROM 1 BY 1
                   UNTIL WS-EX > WS-STREAM-MAX.
           MOVE ZERO TO RETURN-CODE.

       P8000-EXIT.
           EXIT.

       P8200-STREAM-LINE.
           DISPLAY 'CABSXCYC STREAM      '
                   WS-STREAM-CD (WS-EX) ' '
                   WS-STREAM-KEPT (WS-EX) ' '
                   WS-STREAM-DROP (WS-EX).

       P8200-EXIT.
           EXIT.
