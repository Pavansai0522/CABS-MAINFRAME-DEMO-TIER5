       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABSE35D.
      *****************************************************************
      * CABSE35D - SORT E35 OUTPUT EXIT - CMDS EXCHANGE HEADER AND    *
      *            TRAILER GENERATION                                 *
      * APPLICATION : SETL                                            *
      * COMPILER    : ENTERPRISE COBOL                                *
      * INVOKED BY  : OS SORT/MERGE, MODS=(E35=(CABSE35D,4096))       *
      *               ON THE OUTBOUND EXCHANGE SORT STEP OF CABS2600  *
      * INPUTS      : ONE 180 BYTE INDUSTRY FORMAT RECORD PER ENTRY   *
      * OUTPUTS     : THE SAME RECORDS WITH A HEADER RECORD IN FRONT  *
      *               OF EACH RAO GROUP AND A TRAILER BEHIND IT       *
      * CONTROL     : NONE - EXITS DO NOT WRITE CTLOUT, CABS-STD-041  *
      * BALANCE     : DETAIL COUNT ON THE TRAILER = RECORDS IN GROUP  *
      *               HASH ON THE TRAILER = SUM OF THE NET AMOUNTS    *
      * RESTART     : NOT RESTARTABLE - RERUN THE WHOLE SORT STEP     *
      *                                                               *
      * LINKAGE CONVENTION                                            *
      *   REGISTER 1 ADDRESSES A THREE WORD PARAMETER LIST.  WORD     *
      *   ONE IS THE ADDRESS OF THE RECORD LEAVING THE FINAL MERGE    *
      *   OR BINARY ZERO AT END OF MERGE.  WORD TWO ADDRESSES THE     *
      *   LAST RECORD WRITTEN.  WORD THREE ADDRESSES THE LENGTH.      *
      *   RETURN-CODE - 00 TAKE THE NEXT RECORD, 04 DELETE, 08 WRITE  *
      *   THE RECORD ADDRESSED BY WORD ONE AND RE-ENTER WITH THE      *
      *   SAME INPUT, 12 NO FURTHER ENTRY, 16 TERMINATE.              *
      *   AN EXIT THAT INSERTS AHEAD OF A RECORD MUST HOLD ITS OWN    *
      *   STATE ACROSS THE RE-ENTRY - SEE WS-INSERT-STATE.            *
      *                                                               *
      * WHY THIS IS NOT IN CABSET07                                   *
      *   THE INDUSTRY EXCHANGE FORMAT REQUIRES ONE HEADER AND ONE    *
      *   TRAILER PER RECEIVING RAO, WITH THE DETAIL COUNT AND THE    *
      *   AMOUNT HASH ON THE TRAILER.  CABSET07 WRITES ITS OUTPUT     *
      *   IN SETTLEMENT KEY ORDER AND DOES NOT KNOW WHICH RAO EACH    *
      *   RECORD WILL LAND IN UNTIL THE SORT HAS GROUPED THEM.  THE   *
      *   HEADERS AND TRAILERS ARE THEREFORE CUT HERE, AFTER THE      *
      *   GROUPING, AND CABSET07 NEVER SEES THEM - SEE CABS-STD-052.  *
      *                                                               *
      * REVISION HISTORY                                              *
      *   V1.00  1989-02-27  R.T.WHEELER   INITIAL - ASSEMBLER F      *
      *   V1.04  1993-05-19  D.OKONKWO     HASH WIDENED TO 15 DIGITS  *
      *   V1.07  2000-08-31  J.M.CASTILLO  Y2K - DATE TO CCYYDDD ON   *
      *                                    THE HEADER ONLY            *
      *   V2.00  2007-10-08  P.NAIR        RECODED IN COBOL FOR LE    *
      *   V2.02  2013-03-25  A.BUKOWSKI    TRAILER SEQUENCE RESET     *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-MODULE-IDENT.
           05  FILLER                  PIC X(08) VALUE 'CABSE35D'.
           05  FILLER                  PIC X(08) VALUE ' V2.02  '.
       01  WS-SWITCHES.
           05  WS-FIRST-ENTRY-SW       PIC X(01) VALUE 'Y'.
               88  WS-FIRST-ENTRY      VALUE 'Y'.
           05  WS-GROUP-OPEN-SW        PIC X(01) VALUE 'N'.
               88  WS-GROUP-OPEN       VALUE 'Y'.
           05  WS-FINAL-STATE-SW       PIC X(01) VALUE '0'.
               88  WS-FINAL-NOT-DONE   VALUE '0'.
               88  WS-FINAL-TRAILER    VALUE '1'.
               88  WS-FINAL-DONE       VALUE '2'.
       01  WS-INSERT-STATE             PIC X(01) VALUE ' '.
           88  WS-NO-INSERT            VALUE ' '.
           88  WS-TRAILER-PENDING      VALUE 'T'.
           88  WS-HEADER-PENDING       VALUE 'H'.
       01  WS-COUNTERS.
           05  WS-ENTRY-CNT            PIC S9(11) COMP-3 VALUE 0.
           05  WS-DETAIL-CNT           PIC S9(11) COMP-3 VALUE 0.
           05  WS-HEADER-CNT           PIC S9(05) COMP-3 VALUE 0.
           05  WS-TRAILER-CNT          PIC S9(05) COMP-3 VALUE 0.
           05  WS-RAO-CNT              PIC S9(05) COMP-3 VALUE 0.
       01  WS-GROUP-WORK.
           05  WS-GW-RAO               PIC X(03) VALUE SPACES.
           05  WS-GW-PREV-RAO          PIC X(03) VALUE SPACES.
           05  WS-GW-DETAIL-CNT        PIC S9(09) COMP-3 VALUE 0.
           05  WS-GW-HASH-AMT          PIC S9(15)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-GW-HASH-OCN          PIC S9(15) COMP-3 VALUE 0.
           05  WS-GW-SEQ               PIC 9(07) VALUE 0.
           05  WS-GW-OCN-NUM           PIC 9(09) VALUE 0.
           05  WS-GW-OCN-X             PIC X(04) VALUE SPACES.
           05  WS-GW-OCN-N REDEFINES WS-GW-OCN-X.
               10  WS-GW-OCN-DIGITS    PIC 9(04).
       01  WS-DATE-WORK.
           05  WS-DW-CURRENT           PIC 9(07) VALUE 0.
           05  WS-DW-CURRENT-R REDEFINES WS-DW-CURRENT.
               10  WS-DW-CCYY          PIC 9(04).
               10  WS-DW-DDD           PIC 9(03).
      *
      * THE HEADER RECORD.  RECORD CODE 01 IN THE INDUSTRY FORMAT.
      * THE DATE ON THE HEADER CARRIES THE CENTURY.  THE DATE ON
      * THE TRAILER DOES NOT - THE 2000 CHANGE COVERED THE HEADER
      * ONLY, ON THE GROUNDS THAT THE RECEIVING SIDE MATCHES ON THE
      * HEADER DATE.
      *
       01  WS-HEADER-RECORD.
           05  WS-HR-REC-CODE          PIC X(02) VALUE '01'.
           05  WS-HR-FROM-RAO          PIC X(03) VALUE SPACES.
           05  WS-HR-TO-RAO            PIC X(03) VALUE SPACES.
           05  WS-HR-EXCH-CCYYDDD      PIC 9(07) VALUE 0.
           05  WS-HR-CYCLE             PIC 9(02) VALUE 0.
           05  WS-HR-FILE-SEQ          PIC 9(03) VALUE 0.
           05  WS-HR-FORMAT-VER        PIC X(02) VALUE '04'.
           05  WS-HR-SENDER-NAME       PIC X(30) VALUE
               'TELCABS ACCESS BILLING        '.
           05  WS-HR-FILLER            PIC X(128) VALUE SPACES.
      *
      * THE TRAILER RECORD.  RECORD CODE 99.
      *
       01  WS-TRAILER-RECORD.
           05  WS-TL-REC-CODE          PIC X(02) VALUE '99'.
           05  WS-TL-FROM-RAO          PIC X(03) VALUE SPACES.
           05  WS-TL-TO-RAO            PIC X(03) VALUE SPACES.
           05  WS-TL-EXCH-YYDDD        PIC 9(05) VALUE 0.
           05  WS-TL-DETAIL-CNT        PIC 9(09) VALUE 0.
           05  WS-TL-HASH-AMT          PIC S9(15)V9(02) VALUE 0.
           05  WS-TL-HASH-OCN          PIC 9(15) VALUE 0.
           05  WS-TL-LAST-SEQ          PIC 9(07) VALUE 0.
           05  WS-TL-FILLER            PIC X(118) VALUE SPACES.
      *
       LINKAGE SECTION.
       01  LK-PARM-LIST.
           05  LK-RECORD-PTR           POINTER.
           05  LK-PREV-PTR             POINTER.
           05  LK-LENGTH-PTR           POINTER.
       01  LK-SORT-RECORD.
           05  LK-EX-REC-CODE          PIC X(02).
           05  LK-EX-FROM-RAO          PIC X(03).
           05  LK-EX-TO-RAO            PIC X(03).
           05  LK-EX-SEQ               PIC 9(07).
           05  LK-EX-SETTLE-TYPE       PIC X(01).
           05  LK-EX-COUNTERPARTY      PIC X(04).
           05  LK-EX-SETTLE-PERIOD     PIC 9(06).
           05  LK-EX-DIRECTION         PIC X(01).
           05  LK-EX-TOTAL-MOU         PIC 9(13)V9(02).
           05  LK-EX-RATE              PIC 9(05)V9(05).
           05  LK-EX-NET-AMT           PIC S9(13)V9(02).
           05  LK-EX-EXCH-YYDDD        PIC 9(05).
           05  LK-EX-TAIL              PIC X(100).
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
           IF WS-TRAILER-PENDING
               PERFORM P5000-AFTER-TRAILER THRU P5000-EXIT
               GOBACK
           END-IF.
           IF WS-HEADER-PENDING
               PERFORM P5400-AFTER-HEADER THRU P5400-EXIT
               GOBACK
           END-IF.
           ADD 1 TO WS-ENTRY-CNT.
           PERFORM P2000-TEST-RAO-BREAK THRU P2000-EXIT.
           IF RETURN-CODE = 8
               GOBACK
           END-IF.
           PERFORM P3000-COUNT-DETAIL THRU P3000-EXIT.
           MOVE ZERO TO RETURN-CODE.
           GOBACK.

       P1000-INIT.
           MOVE 'N' TO WS-FIRST-ENTRY-SW.
           ACCEPT WS-DW-CURRENT FROM DAY.
           MOVE SPACES TO WS-GW-PREV-RAO.
           DISPLAY 'CABSE35D ENTERED - CMDS EXCHANGE FRAMING'.

       P1000-EXIT.
           EXIT.

       P2000-TEST-RAO-BREAK.
      * A CHANGE OF RECEIVING RAO CLOSES THE PREVIOUS GROUP AND
      * OPENS A NEW ONE.  THE TRAILER GOES OUT FIRST, THEN THE
      * HEADER, THEN THE DETAIL RECORD THAT CAUSED THE BREAK.
           MOVE LK-EX-TO-RAO TO WS-GW-RAO.
           IF WS-GROUP-OPEN AND WS-GW-RAO = WS-GW-PREV-RAO
               GO TO P2000-EXIT
           END-IF.
           IF WS-GROUP-OPEN
               PERFORM P4000-BUILD-TRAILER THRU P4000-EXIT
               SET WS-TRAILER-PENDING TO TRUE
               SET LK-RECORD-PTR TO ADDRESS OF WS-TRAILER-RECORD
               MOVE 8 TO RETURN-CODE
               GO TO P2000-EXIT
           END-IF.
           PERFORM P4400-BUILD-HEADER THRU P4400-EXIT.
           SET WS-HEADER-PENDING TO TRUE.
           SET LK-RECORD-PTR TO ADDRESS OF WS-HEADER-RECORD.
           MOVE 8 TO RETURN-CODE.

       P2000-EXIT.
           EXIT.

       P3000-COUNT-DETAIL.
      * ACCUMULATE THE TRAILER CONTROLS.  THE OCN HASH IS THE SUM
      * OF THE NUMERIC VALUE OF THE COUNTERPARTY CODES.  AN OCN
      * THAT IS NOT ALL NUMERIC CONTRIBUTES ZERO, WHICH IS WHY THE
      * RECEIVING SIDE ONLY EVER COMPARES THE AMOUNT HASH.
           ADD 1 TO WS-DETAIL-CNT.
           ADD 1 TO WS-GW-DETAIL-CNT.
           ADD LK-EX-NET-AMT TO WS-GW-HASH-AMT.
           MOVE LK-EX-COUNTERPARTY TO WS-GW-OCN-X.
           IF WS-GW-OCN-X IS NUMERIC
               MOVE WS-GW-OCN-DIGITS TO WS-GW-OCN-NUM
               ADD WS-GW-OCN-NUM TO WS-GW-HASH-OCN
           END-IF.
           MOVE LK-EX-SEQ TO WS-GW-SEQ.

       P3000-EXIT.
           EXIT.

       P4000-BUILD-TRAILER.
           MOVE WS-GW-PREV-RAO    TO WS-TL-TO-RAO.
           MOVE 'TCB'             TO WS-TL-FROM-RAO.
           MOVE WS-GW-DETAIL-CNT  TO WS-TL-DETAIL-CNT.
           MOVE WS-GW-HASH-AMT    TO WS-TL-HASH-AMT.
           MOVE WS-GW-HASH-OCN    TO WS-TL-HASH-OCN.
           MOVE WS-GW-SEQ         TO WS-TL-LAST-SEQ.
           MOVE LK-EX-EXCH-YYDDD  TO WS-TL-EXCH-YYDDD.
           ADD 1 TO WS-TRAILER-CNT.

       P4000-EXIT.
           EXIT.

       P4400-BUILD-HEADER.
           MOVE WS-GW-RAO         TO WS-HR-TO-RAO.
           MOVE 'TCB'             TO WS-HR-FROM-RAO.
           MOVE WS-DW-CURRENT     TO WS-HR-EXCH-CCYYDDD.
           ADD 1 TO WS-HEADER-CNT.
           MOVE WS-HEADER-CNT     TO WS-HR-FILE-SEQ.
           MOVE ZERO TO WS-GW-DETAIL-CNT.
           MOVE ZERO TO WS-GW-HASH-AMT.
           MOVE ZERO TO WS-GW-HASH-OCN.
           MOVE ZERO TO WS-GW-SEQ.
           MOVE WS-GW-RAO TO WS-GW-PREV-RAO.
           MOVE 'Y' TO WS-GROUP-OPEN-SW.
           ADD 1 TO WS-RAO-CNT.

       P4400-EXIT.
           EXIT.

       P5000-AFTER-TRAILER.
      * THE TRAILER HAS BEEN WRITTEN.  THE SAME DETAIL RECORD IS
      * PRESENTED AGAIN.  NOW CUT THE HEADER FOR THE NEW GROUP.
           SET WS-NO-INSERT TO TRUE.
           MOVE LK-EX-TO-RAO TO WS-GW-RAO.
           PERFORM P4400-BUILD-HEADER THRU P4400-EXIT.
           SET WS-HEADER-PENDING TO TRUE.
           SET LK-RECORD-PTR TO ADDRESS OF WS-HEADER-RECORD.
           MOVE 8 TO RETURN-CODE.

       P5000-EXIT.
           EXIT.

       P5400-AFTER-HEADER.
      * THE HEADER HAS BEEN WRITTEN.  LET THE DETAIL RECORD THROUGH
      * AND COUNT IT.
           SET WS-NO-INSERT TO TRUE.
           PERFORM P3000-COUNT-DETAIL THRU P3000-EXIT.
           MOVE ZERO TO RETURN-CODE.

       P5400-EXIT.
           EXIT.

       P8000-END-OF-MERGE.
      * CUT THE TRAILER FOR THE LAST GROUP, THEN RELEASE.
           IF WS-FINAL-DONE
               MOVE ZERO TO RETURN-CODE
               GO TO P8000-EXIT
           END-IF.
           IF WS-FINAL-TRAILER
               SET WS-FINAL-DONE TO TRUE
               PERFORM P8600-REPORT THRU P8600-EXIT
               MOVE ZERO TO RETURN-CODE
               GO TO P8000-EXIT
           END-IF.
           IF WS-GROUP-OPEN
               MOVE WS-GW-PREV-RAO    TO WS-TL-TO-RAO
               MOVE 'TCB'             TO WS-TL-FROM-RAO
               MOVE WS-GW-DETAIL-CNT  TO WS-TL-DETAIL-CNT
               MOVE WS-GW-HASH-AMT    TO WS-TL-HASH-AMT
               MOVE WS-GW-HASH-OCN    TO WS-TL-HASH-OCN
               MOVE WS-GW-SEQ         TO WS-TL-LAST-SEQ
               ADD 1 TO WS-TRAILER-CNT
               SET WS-FINAL-TRAILER TO TRUE
               SET LK-RECORD-PTR TO ADDRESS OF WS-TRAILER-RECORD
               MOVE 8 TO RETURN-CODE
               GO TO P8000-EXIT
           END-IF.
           SET WS-FINAL-DONE TO TRUE.
           PERFORM P8600-REPORT THRU P8600-EXIT.
           MOVE ZERO TO RETURN-CODE.

       P8000-EXIT.
           EXIT.

       P8600-REPORT.
           DISPLAY 'CABSE35D ENTRIES     ' WS-ENTRY-CNT.
           DISPLAY 'CABSE35D DETAIL      ' WS-DETAIL-CNT.
           DISPLAY 'CABSE35D RAO GROUPS  ' WS-RAO-CNT.
           DISPLAY 'CABSE35D HEADERS     ' WS-HEADER-CNT.
           DISPLAY 'CABSE35D TRAILERS    ' WS-TRAILER-CNT.

       P8600-EXIT.
           EXIT.
