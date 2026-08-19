       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABSE15B.
      *****************************************************************
      * CABSE15B - SORT E15 INPUT EXIT - CARRIER TYPE SELECTION       *
      * APPLICATION : SETL                                            *
      * COMPILER    : ENTERPRISE COBOL                                *
      * INVOKED BY  : OS SORT/MERGE, MODS=(E15=(CABSE15B,8192))       *
      *               ON THE SETTLEMENT AGGREGATION SORT STEPS OF     *
      *               CABS2400 AND CABS2800                           *
      * INPUTS      : ONE 200 BYTE SORTIN RECORD PER ENTRY, CABSCDR   *
      *               CARRTYPE  TELCABS.SETL.CARRTYPE   FB 080        *
      * OUTPUTS     : THE SAME RECORD, OR NOTHING                     *
      * CONTROL     : NONE - EXITS DO NOT WRITE CTLOUT, CABS-STD-041  *
      * BALANCE     : SORTIN = SORTOUT + WS-DROP-TOTAL                *
      * RESTART     : NOT RESTARTABLE - RERUN THE WHOLE SORT STEP     *
      *                                                               *
      * LINKAGE CONVENTION                                            *
      *   REGISTER 1 ADDRESSES A TWO WORD PARAMETER LIST.  WORD ONE   *
      *   IS THE ADDRESS OF THE INPUT RECORD OR BINARY ZERO AT END    *
      *   OF INPUT.  WORD TWO ADDRESSES THE LENGTH HALFWORD.  THE     *
      *   REPLY IS PLACED IN RETURN-CODE -                            *
      *     00  PASS THE RECORD THROUGH UNCHANGED                     *
      *     04  DELETE THE RECORD                                     *
      *     08  RETURN THE ALTERED RECORD                             *
      *     12  DO NOT ENTER THIS EXIT AGAIN                          *
      *     16  TERMINATE THE SORT                                    *
      *   THE MODULE STAYS RESIDENT FOR THE WHOLE SORT STEP, SO THE   *
      *   CARRIER TYPE TABLE IS LOADED ON THE FIRST ENTRY ONLY AND    *
      *   IS STILL ADDRESSABLE ON EVERY LATER ENTRY.                  *
      *                                                               *
      * REVISION HISTORY                                              *
      *   V1.00  1993-01-19  D.OKONKWO     INITIAL - ASSEMBLER F      *
      *   V1.02  1996-07-08  J.M.CASTILLO  WIRELESS ADDED TO THE      *
      *                                    SETTLEMENT PARTY SET       *
      *   V2.00  2005-03-14  P.NAIR        RECODED IN COBOL FOR LE    *
      *   V2.02  2011-10-30  A.BUKOWSKI    RESELLER PASS THROUGH      *
      *   V2.03  2017-04-12  M.HAAS        TABLE RAISED TO 1200 ROWS  *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CARRIER-TYPE-FILE ASSIGN TO CARRTYPE
                  FILE STATUS IS WS-FS-TABLE.
       DATA DIVISION.
       FILE SECTION.
       FD  CARRIER-TYPE-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  CARRIER-TYPE-CARD.
           05  CTC-OCN                 PIC X(04).
           05  CTC-TYPE                PIC X(01).
           05  CTC-SETTLE-ELIG         PIC X(01).
           05  CTC-EFF-YYDDD           PIC 9(05).
           05  CTC-EXP-YYDDD           PIC 9(05).
           05  CTC-NAME                PIC X(40).
           05  CTC-FILLER              PIC X(24).
       WORKING-STORAGE SECTION.
       01  WS-MODULE-IDENT.
           05  FILLER                  PIC X(08) VALUE 'CABSE15B'.
           05  FILLER                  PIC X(08) VALUE ' V2.03  '.
       01  WS-FS-TABLE                 PIC X(02) VALUE '00'.
       01  WS-SWITCHES.
           05  WS-FIRST-ENTRY-SW       PIC X(01) VALUE 'Y'.
               88  WS-FIRST-ENTRY      VALUE 'Y'.
           05  WS-TABLE-EOF-SW         PIC X(01) VALUE 'N'.
               88  WS-TABLE-EOF        VALUE 'Y'.
           05  WS-FOUND-SW             PIC X(01) VALUE 'N'.
               88  WS-FOUND            VALUE 'Y'.
               88  WS-NOT-FOUND        VALUE 'N'.
       01  WS-COUNTERS.
           05  WS-ENTRY-CNT            PIC S9(11) COMP-3 VALUE 0.
           05  WS-KEPT-CNT             PIC S9(11) COMP-3 VALUE 0.
           05  WS-DROP-TOTAL           PIC S9(11) COMP-3 VALUE 0.
           05  WS-DROP-NOT-SETTLE      PIC S9(11) COMP-3 VALUE 0.
           05  WS-DROP-UNKNOWN-OCN     PIC S9(11) COMP-3 VALUE 0.
           05  WS-DROP-EXPIRED         PIC S9(11) COMP-3 VALUE 0.
           05  WS-TABLE-CNT            PIC S9(04) COMP  VALUE 0.
      *
      * THE CARRIER TYPE TABLE.  THIS IS THE ONLY PLACE IN THE
      * SETTLEMENT STREAM WHERE A CARRIER IS TESTED FOR SETTLEMENT
      * ELIGIBILITY BEFORE THE MINUTES ARE AGGREGATED.  THE CARRIER
      * MASTER CARRIES THE SAME INDICATOR BUT THE AGGREGATION STEP
      * DOES NOT READ IT - SEE THE 1993 DESIGN NOTE ON CABS-STD-028,
      * WHICH KEPT THE VSAM MASTER OUT OF THE SORT STEP FOR REGION
      * REASONS.
      *
       01  WS-CARRIER-TABLE.
           05  WS-CT-ENTRY OCCURS 1200 TIMES
                    ASCENDING KEY IS WS-CT-OCN
                    INDEXED BY WS-CX.
               10  WS-CT-OCN           PIC X(04).
               10  WS-CT-TYPE          PIC X(01).
                   88  WS-CT-IXC       VALUE 'I'.
                   88  WS-CT-CLEC      VALUE 'C'.
                   88  WS-CT-ILEC      VALUE 'L'.
                   88  WS-CT-WIRELESS  VALUE 'W'.
                   88  WS-CT-RESELLER  VALUE 'R'.
               10  WS-CT-SETTLE-ELIG   PIC X(01).
               10  WS-CT-EFF-YYDDD     PIC 9(05).
               10  WS-CT-EXP-YYDDD     PIC 9(05).
       01  WS-WORK-FIELDS.
           05  WS-TEST-OCN             PIC X(04).
           05  WS-TEST-YYDDD           PIC 9(05).
           05  WS-HELD-TYPE            PIC X(01).
           05  WS-HELD-ELIG            PIC X(01).
      *
       LINKAGE SECTION.
       01  LK-PARM-LIST.
           05  LK-RECORD-PTR           POINTER.
           05  LK-LENGTH-PTR           POINTER.
       01  LK-SORT-RECORD.
           05  LK-CD-REC-TYPE          PIC X(02).
           05  LK-CD-USAGE-TYPE        PIC X(01).
           05  LK-CD-JURIS-CD          PIC X(01).
           05  LK-CD-OCN               PIC X(04).
           05  LK-CD-BAN               PIC X(13).
           05  LK-CD-SEQ-NBR           PIC 9(09) COMP-3.
           05  LK-CD-RATE-ELEM         PIC X(06).
           05  LK-CD-STATE-CD          PIC X(02).
           05  LK-CD-CONN-YYDDD        PIC 9(05).
           05  LK-CD-TAIL              PIC X(156).
      *
       PROCEDURE DIVISION USING LK-PARM-LIST.
       P0000-MAINLINE.
           MOVE ZERO TO RETURN-CODE.
           IF WS-FIRST-ENTRY
               PERFORM P1000-LOAD-TABLE THRU P1000-EXIT
           END-IF.
           IF LK-RECORD-PTR = NULL
               PERFORM P8000-END-OF-INPUT THRU P8000-EXIT
               GOBACK
           END-IF.
           SET ADDRESS OF LK-SORT-RECORD TO LK-RECORD-PTR.
           ADD 1 TO WS-ENTRY-CNT.
           PERFORM P2000-SELECT THRU P2000-EXIT.
           GOBACK.

       P1000-LOAD-TABLE.
      * LOADED ONCE PER SORT STEP.  THE FILE IS PRESORTED BY OCN SO
      * THE SEARCH ALL BELOW IS VALID.  IF THE FILE IS EVER LEFT
      * UNSORTED THE BINARY SEARCH WILL MISS ROWS AND THE MINUTES
      * FOR THOSE CARRIERS WILL NOT REACH THE AGGREGATION.
           MOVE 'N' TO WS-FIRST-ENTRY-SW.
           MOVE ZERO TO WS-TABLE-CNT.
           OPEN INPUT CARRIER-TYPE-FILE.
           IF WS-FS-TABLE NOT = '00'
               DISPLAY 'CABSE15B OPEN FAILED ON CARRTYPE '
                       WS-FS-TABLE
               MOVE 16 TO RETURN-CODE
               GO TO P1000-EXIT
           END-IF.
           PERFORM P1100-READ-CARD THRU P1100-EXIT
               UNTIL WS-TABLE-EOF
                  OR WS-TABLE-CNT NOT < 1200.
           CLOSE CARRIER-TYPE-FILE.
           DISPLAY 'CABSE15B TABLE ROWS  ' WS-TABLE-CNT.

       P1000-EXIT.
           EXIT.

       P1100-READ-CARD.
           READ CARRIER-TYPE-FILE
               AT END
                   MOVE 'Y' TO WS-TABLE-EOF-SW
                   GO TO P1100-EXIT
           END-READ.
           IF CTC-OCN = SPACES
               GO TO P1100-EXIT
           END-IF.
           ADD 1 TO WS-TABLE-CNT.
           SET WS-CX TO WS-TABLE-CNT.
           MOVE CTC-OCN         TO WS-CT-OCN (WS-CX).
           MOVE CTC-TYPE        TO WS-CT-TYPE (WS-CX).
           MOVE CTC-SETTLE-ELIG TO WS-CT-SETTLE-ELIG (WS-CX).
           MOVE CTC-EFF-YYDDD   TO WS-CT-EFF-YYDDD (WS-CX).
           MOVE CTC-EXP-YYDDD   TO WS-CT-EXP-YYDDD (WS-CX).

       P1100-EXIT.
           EXIT.

       P2000-SELECT.
      * ONLY MINUTES EXCHANGED WITH A SETTLEMENT PARTY BELONG IN THE
      * AGGREGATION.  AN IXC IS BILLED FOR ACCESS AND IS NOT SETTLED
      * WITH, SO ITS USAGE IS TAKEN OUT HERE.  A RESELLER IS PASSED
      * THROUGH BECAUSE THE UNDERLYING CLEC IS SETTLED WITH ON THE
      * RESELLER'S BEHALF - SEE THE 2011 NOTE.
           MOVE LK-CD-OCN TO WS-TEST-OCN.
           MOVE LK-CD-CONN-YYDDD TO WS-TEST-YYDDD.
           SET WS-CX TO 1.
           MOVE 'N' TO WS-FOUND-SW.
           SEARCH ALL WS-CT-ENTRY
               AT END
                   CONTINUE
               WHEN WS-CT-OCN (WS-CX) = WS-TEST-OCN
                   MOVE 'Y' TO WS-FOUND-SW
                   MOVE WS-CT-TYPE (WS-CX) TO WS-HELD-TYPE
                   MOVE WS-CT-SETTLE-ELIG (WS-CX) TO WS-HELD-ELIG
           END-SEARCH.
           IF WS-NOT-FOUND
               ADD 1 TO WS-DROP-UNKNOWN-OCN
               ADD 1 TO WS-DROP-TOTAL
               MOVE 4 TO RETURN-CODE
               GO TO P2000-EXIT
           END-IF.
           PERFORM P2200-EFFECTIVITY THRU P2200-EXIT.
           IF RETURN-CODE = 4
               GO TO P2000-EXIT
           END-IF.
           PERFORM P2400-TYPE-TEST THRU P2400-EXIT.

       P2000-EXIT.
           EXIT.

       P2200-EFFECTIVITY.
      * A CARRIER WHOSE INTERCONNECTION AGREEMENT HAD NOT STARTED,
      * OR HAD ALREADY ENDED, ON THE CONNECT DATE OF THE CALL IS NOT
      * SETTLED WITH FOR THAT CALL.  THE EXPIRY OF 99999 IS THE OPEN
      * ENDED VALUE AND IS SET ON THE MAJORITY OF ROWS.
           SET WS-CX TO WS-CX.
           IF WS-TEST-YYDDD < WS-CT-EFF-YYDDD (WS-CX)
               ADD 1 TO WS-DROP-EXPIRED
               ADD 1 TO WS-DROP-TOTAL
               MOVE 4 TO RETURN-CODE
               GO TO P2200-EXIT
           END-IF.
           IF WS-CT-EXP-YYDDD (WS-CX) NOT = 99999
               IF WS-TEST-YYDDD > WS-CT-EXP-YYDDD (WS-CX)
                   ADD 1 TO WS-DROP-EXPIRED
                   ADD 1 TO WS-DROP-TOTAL
                   MOVE 4 TO RETURN-CODE
               END-IF
           END-IF.

       P2200-EXIT.
           EXIT.

       P2400-TYPE-TEST.
      * THE SETTLEMENT PARTY SET IS CLEC, ILEC AND WIRELESS.  THE
      * ELIGIBILITY BYTE ON THE ROW OVERRIDES THE TYPE WHEN IT IS
      * SET TO N - THAT IS HOW A CARRIER IN DISPUTE IS TAKEN OUT OF
      * THE SETTLEMENT RUN WITHOUT CHANGING THE CARRIER MASTER.
           IF WS-HELD-ELIG = 'N'
               ADD 1 TO WS-DROP-NOT-SETTLE
               ADD 1 TO WS-DROP-TOTAL
               MOVE 4 TO RETURN-CODE
               GO TO P2400-EXIT
           END-IF.
           IF WS-HELD-TYPE = 'C' OR 'L' OR 'W' OR 'R'
               ADD 1 TO WS-KEPT-CNT
               MOVE ZERO TO RETURN-CODE
           ELSE
               ADD 1 TO WS-DROP-NOT-SETTLE
               ADD 1 TO WS-DROP-TOTAL
               MOVE 4 TO RETURN-CODE
           END-IF.

       P2400-EXIT.
           EXIT.

       P8000-END-OF-INPUT.
      * SORTIN IS EXHAUSTED.  NOTHING IS INSERTED.  THE TALLIES GO
      * TO THE MESSAGE DATA SET AND NOWHERE ELSE.
           DISPLAY 'CABSE15B ENTRIES     ' WS-ENTRY-CNT.
           DISPLAY 'CABSE15B KEPT        ' WS-KEPT-CNT.
           DISPLAY 'CABSE15B NOT SETTLED ' WS-DROP-NOT-SETTLE.
           DISPLAY 'CABSE15B UNKNOWN OCN ' WS-DROP-UNKNOWN-OCN.
           DISPLAY 'CABSE15B OUT OF TERM ' WS-DROP-EXPIRED.
           DISPLAY 'CABSE15B DROPPED     ' WS-DROP-TOTAL.
           MOVE ZERO TO RETURN-CODE.

       P8000-EXIT.
           EXIT.
