       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABSXSRC.
      *****************************************************************
      * CABSXSRC - SORT E15 INPUT EXIT - INTAKE SOURCE SELECTION      *
      * APPLICATION : CABS                                            *
      * COMPILER    : ENTERPRISE COBOL                                *
      * INVOKED BY  : OS SORT/MERGE, MODS STATEMENT ON CONTROL CARD   *
      *               MEMBER JCL/CTLCARDS/MVT/CABSRT01 -              *
      *               E15=(CABSXSRC,4096,SORTEXIT,N)                  *
      * INPUTS      : ONE 200 BYTE SORTIN RECORD PER ENTRY FROM       *
      *               TELCABS.CABS.USAGE.INTAKE                       *
      * OUTPUTS     : THE SAME RECORD UNCHANGED, OR A DELETE REPLY    *
      * CONTROL     : NONE - EXITS DO NOT WRITE CTLOUT, CABS-STD-041  *
      * BALANCE     : SORTIN = WS-KEPT-CNT + WS-DROP-TOTAL            *
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
      *   THE MODULE IS LOADED ONCE AND ENTERED ONCE PER RECORD, SO   *
      *   COUNTERS AND TABLES ACCUMULATE ACROSS ENTRIES.  THE EXIT    *
      *   IS ENTERED A FINAL TIME WITH A NULL RECORD ADDRESS.         *
      *                                                               *
      * WHAT THIS EXIT DECIDES                                        *
      *   THE PRESORT CARD CARRIED THE RECORD SELECTION UNTIL THE     *
      *   MOVE TO THE MVT CONTROL CARD FORMAT.  THE SELECTION IS      *
      *   NOW HERE AND ONLY HERE.  A RECORD IS KEPT WHEN ITS          *
      *   RECORD TYPE IN COLUMNS 1 AND 2 IS 01 THROUGH 08 AND ITS     *
      *   SOURCE SYSTEM CODE IN COLUMNS 45 AND 46 IS ON THE           *
      *   APPROVED LIST - 03 EMI GATEWAY, 05 CRIS FEED AND 07         *
      *   MEDIATION.  EVERY OTHER RECORD IS DELETED WITHOUT A         *
      *   SUSPENSE RECORD AND WITHOUT A MESSAGE.  THE LIST IS         *
      *   HELD AS LITERALS IN THIS SOURCE, SO ONBOARDING A NEW        *
      *   SOURCE SYSTEM IS A RECOMPILE AND A RELINK OF THIS           *
      *   MODULE AND NOT A CHANGE TO A CONTROL CARD.                  *
      *                                                               *
      * REVISION HISTORY                                              *
      *   V1.00  1987-04-13  R.T.WHEELER   INITIAL - ASSEMBLER F      *
      *   V1.02  1990-08-27  D.OKONKWO     TYPE 07 AND 08 ADDED       *
      *   V1.05  1994-11-02  P.NAIR        CRIS FEED CODE 05 ADDED    *
      *   V1.09  1999-02-15  A.BUKOWSKI    BLANK SOURCE COUNTED       *
      *                                    SEPARATELY FROM UNKNOWN    *
      *   V2.00  2005-06-30  L.FERREIRA    RECODED IN COBOL FOR LE    *
      *   V2.03  2011-10-11  T.YAMASHITA   MEDIATION CODE 07 ADDED    *
      *   V2.05  2016-05-24  M.HAAS        KEPT PERCENTAGE ON THE     *
      *                                    MESSAGE DATA SET           *
      *   V2.06  2019-01-29  J.CALLAGHAN   SELECTION TAKEN OFF THE    *
      *                                    CONTROL CARD INTO HERE     *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
      * THIS EXIT WRITES NO CONTROL RECORD.  THE BALANCE OF THE
      * PROGRAM THAT READS SORTOUT IS UNAFFECTED BY THIS MODULE -
      * IT COUNTS WHAT IT READS AND REPORTS THAT AS ITS OWN
      * CT-READ.
      *
      * COUNTERS AND TABLES SURVIVE FROM ONE ENTRY TO THE NEXT.
      * THE SORT STEP LOADS THIS MODULE ONCE AND HOLDS IT FOR THE
      * WHOLE PASS.
      *
       01  WS-MODULE-IDENT.
           05  FILLER                  PIC X(08) VALUE 'CABSXSRC'.
           05  FILLER                  PIC X(08) VALUE ' V2.06  '.
       01  WS-COUNTERS.
           05  WS-ENTRY-CNT            PIC S9(11) COMP-3 VALUE 0.
           05  WS-KEPT-CNT             PIC S9(11) COMP-3 VALUE 0.
           05  WS-DROP-TOTAL           PIC S9(11) COMP-3 VALUE 0.
           05  WS-DROP-BAD-TYPE        PIC S9(11) COMP-3 VALUE 0.
           05  WS-DROP-BAD-SRC         PIC S9(11) COMP-3 VALUE 0.
           05  WS-DROP-BLANK-SRC       PIC S9(11) COMP-3 VALUE 0.
           05  WS-MIN-DROPPED          PIC S9(13)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-SWITCHES.
           05  WS-FIRST-ENTRY-SW       PIC X(01) VALUE 'Y'.
               88  WS-FIRST-ENTRY      VALUE 'Y'.
           05  WS-EOF-SEEN-SW          PIC X(01) VALUE 'N'.
               88  WS-EOF-SEEN         VALUE 'Y'.
           05  WS-SRC-FOUND-SW         PIC X(01) VALUE 'N'.
               88  WS-SRC-FOUND        VALUE 'Y'.
      *
      * THE APPROVED SOURCE SYSTEM LIST.  THE ENTRIES ARE HELD IN
      * THE ORDER OPERATIONS ONBOARDED THE FEEDS AND NOT IN
      * ASCENDING CODE ORDER, SO THE TABLE IS WALKED WITH A
      * SEQUENTIAL SEARCH RATHER THAN SEARCH ALL.  A FOURTH FEED
      * MEANS A FOURTH LITERAL, A HIGHER OCCURS, A HIGHER
      * WS-SRC-MAX, A RECOMPILE AND A RELINK.
      *
       01  WS-SRC-LITERALS.
           05  FILLER  PIC X(22) VALUE '03EMI GATEWAY         '.
           05  FILLER  PIC X(22) VALUE '05CRIS FEED           '.
           05  FILLER  PIC X(22) VALUE '07MEDIATION           '.
       01  WS-SRC-TABLE REDEFINES WS-SRC-LITERALS.
           05  WS-SRC-ENTRY OCCURS 3 TIMES INDEXED BY WS-SX.
               10  WS-SRC-CODE         PIC X(02).
               10  WS-SRC-NAME         PIC X(20).
       01  WS-SRC-COUNTS.
           05  WS-SRC-KEPT OCCURS 3 TIMES PIC S9(11) COMP-3
                                                  VALUE 0.
       01  WS-SRC-MAX                  PIC S9(04) COMP VALUE 3.
       01  WS-WORK-FIELDS.
           05  WS-SRC-TEST             PIC X(02) VALUE SPACES.
           05  WS-TYPE-TEST            PIC X(02) VALUE SPACES.
           05  WS-BLANK-TALLY          PIC S9(04) COMP VALUE 0.
           05  WS-KEPT-PCT             PIC S9(05)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DX                   PIC S9(04) COMP VALUE 0.
      *
      * THE INTAKE LAYOUT.  THIS IS A HAND MAINTAINED VIEW OF THE
      * SAME TWO HUNDRED BYTES THE INTAKE PROGRAMS READ.  THE
      * COLUMNS THAT MATTER TO THIS EXIT ARE 1 AND 2 FOR THE
      * RECORD TYPE AND 45 AND 46 FOR THE SOURCE SYSTEM CODE.
      *
       LINKAGE SECTION.
       01  LK-PARM-LIST.
           05  LK-RECORD-PTR           POINTER.
           05  LK-LENGTH-PTR           POINTER.
       01  LK-SORT-RECORD.
           05  LK-IN-REC-TYPE          PIC X(02).
           05  LK-IN-USAGE-TYPE        PIC X(01).
           05  LK-IN-FILLER-1          PIC X(01).
           05  LK-IN-OCN               PIC X(04).
           05  LK-IN-BAN               PIC X(13).
           05  LK-IN-SEQ-NBR           PIC 9(09) COMP-3.
           05  LK-IN-RAO               PIC X(03).
           05  LK-IN-EDIT-STATUS       PIC X(01).
           05  LK-IN-CIC               PIC 9(04).
           05  LK-IN-CONN-YYDDD        PIC 9(05).
           05  LK-IN-JURIS-CD          PIC X(01).
           05  LK-IN-STATE-CD          PIC X(02).
           05  LK-IN-FILLER-2          PIC X(02).
           05  LK-IN-SRC-SYSTEM        PIC X(02).
           05  LK-IN-LOAD-YYDDD        PIC 9(05).
           05  LK-IN-TRUNK-GRP         PIC X(08).
           05  LK-IN-CIRCUIT-ID        PIC X(10).
           05  LK-IN-USOC              PIC X(05).
           05  LK-IN-FILLER-3          PIC X(45).
           05  LK-IN-CONV-MIN          PIC S9(11)V9(02) COMP-3.
           05  LK-IN-TAIL              PIC X(74).
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
           PERFORM P2000-TEST-REC-TYPE THRU P2000-EXIT.
           IF RETURN-CODE = 4
               GOBACK
           END-IF.
           PERFORM P3000-TEST-SOURCE THRU P3000-EXIT.
           IF RETURN-CODE = 4
               GOBACK
           END-IF.
           PERFORM P4000-KEEP-RECORD THRU P4000-EXIT.
           GOBACK.

       P1000-INIT.
      * ENTERED ONCE PER RECORD.  ONLY THE FIRST ENTRY DOES ANY
      * SET UP - EVERY LATER ENTRY FALLS STRAIGHT THROUGH.
           MOVE ZERO TO RETURN-CODE.
           IF WS-FIRST-ENTRY
               MOVE 'N' TO WS-FIRST-ENTRY-SW
               DISPLAY 'CABSXSRC ENTERED - INTAKE SOURCE SELECTION'
               PERFORM P1200-SHOW-LIST THRU P1200-EXIT
                       VARYING WS-SX FROM 1 BY 1
                       UNTIL WS-SX > WS-SRC-MAX
           END-IF.

       P1000-EXIT.
           EXIT.

       P1200-SHOW-LIST.
      * THE APPROVED LIST IS WRITTEN TO THE MESSAGE DATA SET ON
      * THE FIRST ENTRY SO THE RUN SHEET SHOWS WHICH FEEDS WERE
      * ADMITTED BY THE LEVEL OF THE MODULE THAT WAS LINKED.
           DISPLAY 'CABSXSRC APPROVED    '
                   WS-SRC-CODE (WS-SX) ' '
                   WS-SRC-NAME (WS-SX).

       P1200-EXIT.
           EXIT.

       P2000-TEST-REC-TYPE.
      * RECORD TYPES OUTSIDE 01 THROUGH 08 ARE NOT PRICED BY ANY
      * RATING MODULE.  A TYPE THAT IS NOT TWO DIGITS IS TREATED
      * THE SAME WAY AS A TYPE OUTSIDE THE RANGE.
           MOVE LK-IN-REC-TYPE TO WS-TYPE-TEST.
           IF WS-TYPE-TEST NOT NUMERIC
               PERFORM P2400-COUNT-MINUTES THRU P2400-EXIT
               ADD 1 TO WS-DROP-BAD-TYPE
               ADD 1 TO WS-DROP-TOTAL
               MOVE 4 TO RETURN-CODE
               GO TO P2000-EXIT
           END-IF.
           IF WS-TYPE-TEST < '01' OR WS-TYPE-TEST > '08'
               PERFORM P2400-COUNT-MINUTES THRU P2400-EXIT
               ADD 1 TO WS-DROP-BAD-TYPE
               ADD 1 TO WS-DROP-TOTAL
               MOVE 4 TO RETURN-CODE
           END-IF.

       P2000-EXIT.
           EXIT.

       P2400-COUNT-MINUTES.
      * THE MINUTES ON A DELETED RECORD ARE ACCUMULATED FOR THE
      * MESSAGE DATA SET ONLY.  THEY ARE NOT CARRIED INTO ANY
      * CONTROL RECORD AND NO PROGRAM DOWNSTREAM READS THEM.
           ADD LK-IN-CONV-MIN TO WS-MIN-DROPPED.

       P2400-EXIT.
           EXIT.

       P3000-TEST-SOURCE.
      * COLUMNS 45 AND 46 CARRY THE CODE OF THE SYSTEM THAT CUT
      * THE RECORD.  A BLANK CODE MEANS THE FEED DID NOT STAMP
      * ONE AND IS COUNTED ON ITS OWN LINE, BUT IT IS REMOVED
      * ALONGSIDE AN UNKNOWN CODE.
           MOVE LK-IN-SRC-SYSTEM TO WS-SRC-TEST.
           MOVE ZERO TO WS-BLANK-TALLY.
           INSPECT WS-SRC-TEST TALLYING WS-BLANK-TALLY
                   FOR ALL SPACE.
           IF WS-BLANK-TALLY = 2
               PERFORM P2400-COUNT-MINUTES THRU P2400-EXIT
               ADD 1 TO WS-DROP-BLANK-SRC
               ADD 1 TO WS-DROP-TOTAL
               MOVE 4 TO RETURN-CODE
               GO TO P3000-EXIT
           END-IF.
           MOVE 'N' TO WS-SRC-FOUND-SW.
           SET WS-SX TO 1.
           SEARCH WS-SRC-ENTRY
               AT END
                   CONTINUE
               WHEN WS-SRC-CODE (WS-SX) = WS-SRC-TEST
                   MOVE 'Y' TO WS-SRC-FOUND-SW
           END-SEARCH.
           IF NOT WS-SRC-FOUND
               PERFORM P2400-COUNT-MINUTES THRU P2400-EXIT
               ADD 1 TO WS-DROP-BAD-SRC
               ADD 1 TO WS-DROP-TOTAL
               MOVE 4 TO RETURN-CODE
           END-IF.

       P3000-EXIT.
           EXIT.

       P4000-KEEP-RECORD.
      * THE RECORD IS ADMITTED UNCHANGED.  THE SORT KEY POSITIONS
      * ON THE CONTROL CARD ALREADY LINE UP WITH THIS LAYOUT, SO
      * THERE IS NOTHING TO REARRANGE AND THE REPLY IS ZERO
      * RATHER THAN EIGHT.
           ADD 1 TO WS-KEPT-CNT.
           ADD 1 TO WS-SRC-KEPT (WS-SX).
           MOVE ZERO TO RETURN-CODE.

       P4000-EXIT.
           EXIT.

       P8000-END-OF-INPUT.
      * A NULL RECORD ADDRESS MEANS SORTIN IS EXHAUSTED.  THE EXIT
      * HAS NOTHING TO INSERT, SO IT REPLIES ZERO AND WRITES ITS
      * TALLIES TO THE MESSAGE DATA SET.  THESE COUNTS ARE THE
      * ONLY RECORD OF WHAT THE EXIT REMOVED - THEY ARE NOT
      * CARRIED INTO ANY CONTROL RECORD.
           MOVE 'Y' TO WS-EOF-SEEN-SW.
           MOVE ZERO TO WS-KEPT-PCT.
           IF WS-ENTRY-CNT > ZERO
               COMPUTE WS-KEPT-PCT ROUNDED =
                       (WS-KEPT-CNT * 100) / WS-ENTRY-CNT
           END-IF.
           DISPLAY 'CABSXSRC ENTRIES     ' WS-ENTRY-CNT.
           DISPLAY 'CABSXSRC KEPT        ' WS-KEPT-CNT.
           DISPLAY 'CABSXSRC BAD TYPE    ' WS-DROP-BAD-TYPE.
           DISPLAY 'CABSXSRC BAD SOURCE  ' WS-DROP-BAD-SRC.
           DISPLAY 'CABSXSRC BLANK SOURCE' WS-DROP-BLANK-SRC.
           DISPLAY 'CABSXSRC DROPPED     ' WS-DROP-TOTAL.
           DISPLAY 'CABSXSRC MINUTES OUT ' WS-MIN-DROPPED.
           DISPLAY 'CABSXSRC KEPT PCT    ' WS-KEPT-PCT.
           PERFORM P8200-SOURCE-LINE THRU P8200-EXIT
                   VARYING WS-SX FROM 1 BY 1
                   UNTIL WS-SX > WS-SRC-MAX.
           MOVE ZERO TO RETURN-CODE.

       P8000-EXIT.
           EXIT.

       P8200-SOURCE-LINE.
      * ONE LINE PER APPROVED FEED.  A FEED THAT READS ZERO HAS
      * SENT NOTHING THIS CYCLE.
           DISPLAY 'CABSXSRC KEPT FOR    '
                   WS-SRC-CODE (WS-SX) ' '
                   WS-SRC-KEPT (WS-SX).

       P8200-EXIT.
           EXIT.
