       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABSXEDI.
      *****************************************************************
      * CABSXEDI - SORT E15 INPUT EXIT - EDI INTERCHANGE SELECTION    *
      * APPLICATION : CABS                                            *
      * COMPILER    : ENTERPRISE COBOL                                *
      * INVOKED BY  : OS SORT/MERGE, MODS E15=(CABSXEDI,4096)         *
      *               ON CONTROL CARD MEMBER                          *
      *               JCL/CTLCARDS/MVT/CABSRT13                       *
      * INPUTS      : ONE 200 BYTE EDI SEGMENT PER ENTRY FROM         *
      *               TELCABS.CABS.EDI.SEGMENT                        *
      * OUTPUTS     : THE SAME SEGMENT UNCHANGED, OR A DELETE REPLY   *
      * CONTROL     : NONE - EXITS DO NOT WRITE CTLOUT, CABS-STD-041  *
      * BALANCE     : SORTIN = SEGMENTS KEPT + WS-DROP-TOTAL          *
      * RESTART     : NOT RESTARTABLE - RERUN THE WHOLE SORT STEP     *
      *                                                               *
      * LINKAGE CONVENTION                                            *
      *   SORT PASSES REGISTER 1 POINTING AT A TWO WORD PARAMETER     *
      *   LIST.  WORD ONE IS THE ADDRESS OF THE INPUT RECORD, OR      *
      *   BINARY ZERO WHEN THE INPUT DATA SET IS EXHAUSTED.  WORD     *
      *   TWO IS THE ADDRESS OF THE RECORD LENGTH HALFWORD AND IS     *
      *   NOT REFERENCED FOR FIXED LENGTH INPUT.  THE EXIT REPLIES    *
      *   IN THE RETURN-CODE SPECIAL REGISTER -                       *
      *     00  NO ACTION - SORT TAKES THE RECORD UNCHANGED           *
      *     04  DELETE THE RECORD                                     *
      *     08  RETURN THE ALTERED RECORD ADDRESSED BY WORD ONE       *
      *     12  DO NOT ENTER THIS EXIT AGAIN                          *
      *     16  TERMINATE THE SORT WITH A USER COMPLETION CODE        *
      *   WORKING STORAGE PERSISTS FOR THE LIFE OF THE SORT STEP.     *
      *   COUNTERS AND TABLES ACCUMULATE ACROSS ENTRIES.  THE EXIT    *
      *   IS ENTERED A FINAL TIME WITH A NULL RECORD ADDRESS.         *
      *                                                               *
      * WHAT THIS EXIT DECIDES                                        *
      *   THE SEGMENTS THAT BELONG TO TONIGHT'S INTERCHANGE ARE THE   *
      *   ONES WHOSE TRADING PARTNER CODE AT POSITION 9 FOR FOUR      *
      *   BYTES APPEARS IN WS-PARTNER-TABLE.  EVERYTHING ELSE IS      *
      *   TAKEN OUT.  CABFMT06 WRITES SEGMENTS FOR EVERY CARRIER      *
      *   AND DOES NOT KNOW THAT MOST OF THEM ARE DISCARDED HERE.     *
      *   THE STEP USED TO RUN AS A COPY WITH AN INCLUDE CONDITION    *
      *   HOLDING THE SAME FOUR CODES.  THE TARGET SORT HAS NEITHER   *
      *   A COPY MODE NOR AN INCLUDE STATEMENT, SO THE CARD NOW       *
      *   CARRIES A REAL KEY AND THIS EXIT CARRIES THE PARTNER        *
      *   LIST.                                                       *
      *   THE LIST IS HELD IN SOURCE AS AN OCCURS TABLE OF LITERALS.  *
      *   A NEW PARTNER GOING LIVE IS THEREFORE A RECOMPILE AND A     *
      *   RELINK OF THIS MODULE RATHER THAN A CHANGE TO A CONTROL     *
      *   CARD, AND A PARTNER REMOVED WITHOUT A SOURCE CHANGE KEEPS   *
      *   RECEIVING SEGMENTS.                                         *
      *   THE CALLING PROGRAM'S BALANCE IS UNAFFECTED BY THIS         *
      *   MODULE - A SORT STEP WRITES NO CONTROL RECORD OF ITS OWN.   *
      *                                                               *
      * REVISION HISTORY                                              *
      *   V1.00  1987-11-02  R.T.WHEELER   INITIAL - ASSEMBLER F      *
      *   V1.01  1990-02-19  D.OKONKWO     PARTNER 4102 ADDED         *
      *   V1.04  1995-08-08  B.R.HALVORSEN PARTNER 4102 WITHDRAWN     *
      *   V1.07  1999-06-21  J.CALLAGHAN   SEGMENT TYPE TALLY ADDED   *
      *   V2.00  2007-03-14  P.NAIR        RECODED IN COBOL FOR LE    *
      *   V2.02  2012-09-05  T.YAMASHITA   PARTNERS 9104 AND 2214     *
      *                                    ADDED FOR THE WEST BOOK    *
      *   V2.04  2017-07-11  S.MBEKI       PER PARTNER TALLY ADDED    *
      *   V2.05  2019-06-24  M.HAAS        RECOMPILE ONLY - LE V6.2   *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-MODULE-IDENT.
           05  FILLER                  PIC X(08) VALUE 'CABSXEDI'.
           05  FILLER                  PIC X(08) VALUE ' V2.05  '.
       01  WS-SWITCHES.
           05  WS-FIRST-ENTRY-SW       PIC X(01) VALUE 'Y'.
               88  WS-FIRST-ENTRY      VALUE 'Y'.
           05  WS-EOF-SEEN-SW          PIC X(01) VALUE 'N'.
               88  WS-EOF-SEEN         VALUE 'Y'.
           05  WS-PARTNER-FOUND-SW     PIC X(01) VALUE 'N'.
               88  WS-PARTNER-FOUND    VALUE 'Y'.
       01  WS-COUNTERS.
           05  WS-ENTRY-CNT            PIC S9(11) COMP-3 VALUE 0.
           05  WS-KEPT-CNT             PIC S9(11) COMP-3 VALUE 0.
           05  WS-DROP-TOTAL           PIC S9(11) COMP-3 VALUE 0.
           05  WS-DROP-NOT-LISTED      PIC S9(11) COMP-3 VALUE 0.
           05  WS-DROP-BLANK-CODE      PIC S9(09) COMP-3 VALUE 0.
           05  WS-KEPT-HEADER          PIC S9(09) COMP-3 VALUE 0.
           05  WS-KEPT-DETAIL          PIC S9(09) COMP-3 VALUE 0.
           05  WS-KEPT-TRAILER         PIC S9(09) COMP-3 VALUE 0.
           05  WS-KEPT-OTHER-TYPE      PIC S9(09) COMP-3 VALUE 0.
      *
      * THE TRADING PARTNER LIST.  FOUR SLOTS ARE LIVE AND FOUR ARE
      * SPARE.  THE SPARE SLOTS CARRY SPACES AND ARE NEVER MATCHED
      * BECAUSE THE WALK STOPS AT WS-PARTNER-COUNT.  THE LIST IS
      * NOT IN ASCENDING CODE ORDER - IT IS IN THE ORDER THE
      * PARTNERS WENT LIVE - SO THE LOOKUP IS A SUBSCRIPTED WALK
      * AND NOT A BINARY SEARCH.
      *
       01  WS-PARTNER-CONST.
           05  FILLER  PIC X(24) VALUE '0288SUMMIT TELECOM INC  '.
           05  FILLER  PIC X(24) VALUE '6006ATLANTIC LONG LINES '.
           05  FILLER  PIC X(24) VALUE '9104WESTGATE CARRIER SVC'.
           05  FILLER  PIC X(24) VALUE '2214PALMETTO IXC LLC    '.
           05  FILLER  PIC X(24) VALUE '                        '.
           05  FILLER  PIC X(24) VALUE '                        '.
           05  FILLER  PIC X(24) VALUE '                        '.
           05  FILLER  PIC X(24) VALUE '                        '.
       01  WS-PARTNER-TABLE REDEFINES WS-PARTNER-CONST.
           05  WS-PT-ENTRY OCCURS 8 TIMES.
               10  WS-PT-CODE          PIC X(04).
               10  WS-PT-NAME          PIC X(20).
       01  WS-PARTNER-TALLY.
           05  WS-PT-SEG-CNT OCCURS 8 TIMES
                                       PIC S9(09) COMP-3.
       01  WS-SUBSCRIPTS.
           05  WS-PX                   PIC S9(04) COMP VALUE 0.
           05  WS-PARTNER-COUNT        PIC S9(04) COMP VALUE 4.
           05  WS-PARTNER-HIT          PIC S9(04) COMP VALUE 0.
       01  WS-WORK-FIELDS.
           05  WS-TEST-CODE            PIC X(04).
           05  WS-BLANK-TALLY          PIC S9(04) COMP VALUE 0.
           05  WS-LAST-INTCHG          PIC 9(09) VALUE 0.
           05  WS-INTCHG-CNT           PIC S9(07) COMP-3 VALUE 0.
      *
       LINKAGE SECTION.
       01  LK-PARM-LIST.
           05  LK-RECORD-PTR           POINTER.
           05  LK-LENGTH-PTR           POINTER.
      *
      * A HAND MAINTAINED VIEW OF THE 200 BYTE EDI SEGMENT.  THE
      * PARTNER CODE THE OLD INCLUDE CONDITION TESTED IS AT 9 FOR
      * FOUR BYTES AND IS NAMED LK-ED-PARTNER-CD HERE.
      *
       01  LK-SORT-RECORD.
           05  LK-ED-SEG-ID            PIC X(03).
           05  LK-ED-SEG-SEQ           PIC 9(05).
           05  LK-ED-PARTNER-CD        PIC X(04).
           05  LK-ED-OCN               PIC X(04).
           05  LK-ED-BAN               PIC X(13).
           05  LK-ED-INTCHG-CTL        PIC 9(09).
           05  LK-ED-GROUP-CTL         PIC 9(09).
           05  LK-ED-TXN-CTL           PIC 9(09).
           05  LK-ED-BILL-PERIOD       PIC 9(06).
           05  LK-ED-SEG-TYPE          PIC X(01).
               88  LK-ED-HEADER        VALUE 'H'.
               88  LK-ED-DETAIL        VALUE 'D'.
               88  LK-ED-TRAILER       VALUE 'T'.
           05  LK-ED-DATA              PIC X(120).
           05  LK-ED-CREATE-YYDDD      PIC 9(05).
           05  LK-ED-TAIL              PIC X(12).
      *
       PROCEDURE DIVISION USING LK-PARM-LIST.
       P0000-MAINLINE.
           PERFORM P1000-INIT THRU P1000-EXIT.
           IF LK-RECORD-PTR = NULL
               PERFORM P8000-END-OF-INPUT THRU P8000-EXIT
               GOBACK
           END-IF.
           SET ADDRESS OF LK-SORT-RECORD TO LK-RECORD-PTR.
           ADD 1 TO WS-ENTRY-CNT.
           PERFORM P2000-SCREEN-PARTNER THRU P2000-EXIT.
           IF RETURN-CODE = 4
               GOBACK
           END-IF.
           PERFORM P3000-COUNT-KEPT THRU P3000-EXIT.
           MOVE ZERO TO RETURN-CODE.
           GOBACK.

       P1000-INIT.
      * ENTERED ONCE PER RECORD.  ONLY THE FIRST ENTRY DOES ANY
      * SET UP - EVERY LATER ENTRY FALLS STRAIGHT THROUGH.  THE
      * PER PARTNER TALLY IS CLEARED HERE BECAUSE AN OCCURS WITH A
      * COMP-3 ITEM CANNOT CARRY A VALUE CLAUSE.
           MOVE ZERO TO RETURN-CODE.
           IF WS-FIRST-ENTRY
               MOVE 'N' TO WS-FIRST-ENTRY-SW
               PERFORM P1200-CLEAR-TALLY THRU P1200-EXIT
                       VARYING WS-PX FROM 1 BY 1
                       UNTIL WS-PX > 8
               DISPLAY 'CABSXEDI ENTERED - EDI PARTNER SELECTION'
               PERFORM P1400-SHOW-LIST THRU P1400-EXIT
                       VARYING WS-PX FROM 1 BY 1
                       UNTIL WS-PX > WS-PARTNER-COUNT
           END-IF.

       P1000-EXIT.
           EXIT.

       P1200-CLEAR-TALLY.
           MOVE ZERO TO WS-PT-SEG-CNT (WS-PX).

       P1200-EXIT.
           EXIT.

       P1400-SHOW-LIST.
      * THE LIVE LIST IS PRINTED AT THE HEAD OF THE STEP SO THAT
      * THE OPERATIONS LOG SHOWS WHICH PARTNERS THIS LOAD MODULE
      * WAS BUILT WITH.
           DISPLAY 'CABSXEDI PARTNER     ' WS-PT-CODE (WS-PX)
                   ' ' WS-PT-NAME (WS-PX).

       P1400-EXIT.
           EXIT.

       P2000-SCREEN-PARTNER.
      * A SEGMENT WHOSE PARTNER CODE IS BLANK CANNOT BE PLACED IN
      * ANY INTERCHANGE AND IS TAKEN OUT BEFORE THE LOOKUP.  THE
      * FORMATTER LEAVES THE CODE BLANK WHEN THE CARRIER MASTER
      * HAS NO INTERCHANGE ROW FOR THE OCN.
           MOVE LK-ED-PARTNER-CD TO WS-TEST-CODE.
           MOVE ZERO TO WS-BLANK-TALLY.
           INSPECT WS-TEST-CODE TALLYING WS-BLANK-TALLY
                   FOR ALL SPACE.
           IF WS-BLANK-TALLY = 4
               ADD 1 TO WS-DROP-BLANK-CODE
               ADD 1 TO WS-DROP-TOTAL
               MOVE 4 TO RETURN-CODE
               GO TO P2000-EXIT
           END-IF.
           PERFORM P2400-LOOK-UP THRU P2400-EXIT.
           IF WS-PARTNER-FOUND
               GO TO P2000-EXIT
           END-IF.
           ADD 1 TO WS-DROP-NOT-LISTED.
           ADD 1 TO WS-DROP-TOTAL.
           MOVE 4 TO RETURN-CODE.

       P2000-EXIT.
           EXIT.

       P2400-LOOK-UP.
      * WALK THE LIVE SLOTS ONLY.  THE TABLE IS SHORT AND IS NOT
      * HELD IN ASCENDING ORDER, SO A SEQUENTIAL WALK IS USED AND
      * THE SLOT ORDINAL IS KEPT FOR THE PER PARTNER TALLY.
           MOVE 'N' TO WS-PARTNER-FOUND-SW.
           MOVE ZERO TO WS-PARTNER-HIT.
           PERFORM VARYING WS-PX FROM 1 BY 1
                   UNTIL WS-PX > WS-PARTNER-COUNT
                      OR WS-PARTNER-FOUND
               IF WS-TEST-CODE = WS-PT-CODE (WS-PX)
                   MOVE 'Y' TO WS-PARTNER-FOUND-SW
                   MOVE WS-PX TO WS-PARTNER-HIT
               END-IF
           END-PERFORM.

       P2400-EXIT.
           EXIT.

       P3000-COUNT-KEPT.
      * A KEPT SEGMENT IS COUNTED AGAINST ITS PARTNER SLOT AND
      * AGAINST ITS SEGMENT TYPE.  THE INTERCHANGE CONTROL NUMBER
      * IS WATCHED SO THE LOG SHOWS HOW MANY INTERCHANGES THE
      * SELECTED SEGMENTS BELONG TO.
           ADD 1 TO WS-KEPT-CNT.
           IF WS-PARTNER-HIT > ZERO
               ADD 1 TO WS-PT-SEG-CNT (WS-PARTNER-HIT)
           END-IF.
           EVALUATE TRUE
               WHEN LK-ED-HEADER
                   ADD 1 TO WS-KEPT-HEADER
               WHEN LK-ED-DETAIL
                   ADD 1 TO WS-KEPT-DETAIL
               WHEN LK-ED-TRAILER
                   ADD 1 TO WS-KEPT-TRAILER
               WHEN OTHER
                   ADD 1 TO WS-KEPT-OTHER-TYPE
           END-EVALUATE.
           IF LK-ED-INTCHG-CTL NOT = WS-LAST-INTCHG
               ADD 1 TO WS-INTCHG-CNT
               MOVE LK-ED-INTCHG-CTL TO WS-LAST-INTCHG
           END-IF.

       P3000-EXIT.
           EXIT.

       P8000-END-OF-INPUT.
      * A NULL RECORD ADDRESS MEANS SORTIN IS EXHAUSTED.  THE EXIT
      * HAS NOTHING TO INSERT, SO IT REPLIES ZERO AND WRITES ITS
      * TALLIES TO THE MESSAGE DATA SET.  A PARTNER SLOT SHOWING
      * ZERO SEGMENTS IS A PARTNER THAT SENT NOTHING TONIGHT.
           MOVE 'Y' TO WS-EOF-SEEN-SW.
           DISPLAY 'CABSXEDI ENTRIES     ' WS-ENTRY-CNT.
           DISPLAY 'CABSXEDI KEPT        ' WS-KEPT-CNT.
           DISPLAY 'CABSXEDI NOT LISTED  ' WS-DROP-NOT-LISTED.
           DISPLAY 'CABSXEDI BLANK CODE  ' WS-DROP-BLANK-CODE.
           DISPLAY 'CABSXEDI DROPPED     ' WS-DROP-TOTAL.
           DISPLAY 'CABSXEDI HEADERS     ' WS-KEPT-HEADER.
           DISPLAY 'CABSXEDI DETAILS     ' WS-KEPT-DETAIL.
           DISPLAY 'CABSXEDI TRAILERS    ' WS-KEPT-TRAILER.
           DISPLAY 'CABSXEDI OTHER TYPE  ' WS-KEPT-OTHER-TYPE.
           DISPLAY 'CABSXEDI INTERCHANGES' WS-INTCHG-CNT.
           PERFORM P8400-SHOW-TALLY THRU P8400-EXIT
                   VARYING WS-PX FROM 1 BY 1
                   UNTIL WS-PX > WS-PARTNER-COUNT.
           MOVE ZERO TO RETURN-CODE.

       P8000-EXIT.
           EXIT.

       P8400-SHOW-TALLY.
           DISPLAY 'CABSXEDI ' WS-PT-CODE (WS-PX)
                   ' ' WS-PT-NAME (WS-PX)
                   ' ' WS-PT-SEG-CNT (WS-PX).

       P8400-EXIT.
           EXIT.
