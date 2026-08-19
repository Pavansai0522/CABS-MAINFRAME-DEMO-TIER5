       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABSXOOB.
      *****************************************************************
      * CABSXOOB - SORT E15 INPUT EXIT - OUT OF BALANCE SELECTION     *
      * APPLICATION : CABS                                            *
      * COMPILER    : ENTERPRISE COBOL                                *
      * INVOKED BY  : OS SORT/MERGE, MODS E15=(CABSXOOB,4096)         *
      *               ON CONTROL CARD MEMBER                          *
      *               JCL/CTLCARDS/MVT/CABSRT18                       *
      * INPUTS      : ONE 120 BYTE BILL PROOF RECORD PER ENTRY FROM   *
      *               TELCABS.CABS.BILL.PROOF                         *
      * OUTPUTS     : THE SAME RECORD UNCHANGED, OR A DELETE REPLY    *
      * CONTROL     : NONE - EXITS DO NOT WRITE CTLOUT, CABS-STD-041  *
      * BALANCE     : SORTIN = RECORDS SELECTED + WS-DROP-TOTAL       *
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
      *   COUNTERS AND BANDS ACCUMULATE ACROSS ENTRIES.  THE EXIT     *
      *   IS ENTERED A FINAL TIME WITH A NULL RECORD ADDRESS.         *
      *                                                               *
      * WHAT THIS EXIT DECIDES                                        *
      *   A BILL PROOF RECORD IS SELECTED FOR THE OUT OF BALANCE      *
      *   LISTING WHEN ITS RESULT TEXT AT POSITION 74 FOR ELEVEN      *
      *   BYTES READS OUT OF BAL FOLLOWED BY ONE SPACE.  EVERYTHING   *
      *   ELSE IS TAKEN OUT.  THE RULE WAS CARRIED AS AN INCLUDE      *
      *   CONDITION ON THE CONTROL CARD UNTIL THE STEP WAS MOVED TO   *
      *   A SORT THAT HAS NO INCLUDE STATEMENT.  THE COMPARE IS       *
      *   BYTE FOR BYTE AND THE TRAILING SPACE IS PART OF IT, EXACT   *
      *   AS THE CARD HAD IT.                                         *
      *   THE TEST IS ON THE RESULT TEXT CABBIL11 WROTE, NOT ON THE   *
      *   DIFFERENCE FIELD.  A RECORD WHOSE DIFFERENCE IS INSIDE      *
      *   THE TOLERANCE CABBIL11 APPLIED CARRIES THE TEXT IN          *
      *   BALANCE AND IS NOT SELECTED HERE, WHATEVER THE DIFFERENCE   *
      *   FIELD ACTUALLY SAYS.  THE COUNT OF RECORDS IN THAT          *
      *   POSITION IS PRINTED BELOW SO THE SIZE OF IT IS ON THE       *
      *   OPERATIONS LOG.                                             *
      *   THE SELECTED RECORDS ARE ALSO BANDED BY THE SIZE OF THE     *
      *   DIFFERENCE, BECAUSE THE CONTROL TEAM WORK THE LISTING       *
      *   FROM THE TOP AND THE BANDS SHOW HOW FAR DOWN IT IS WORTH    *
      *   READING.                                                    *
      *   THE CALLING PROGRAM'S BALANCE IS UNAFFECTED BY THIS         *
      *   MODULE - A SORT STEP WRITES NO CONTROL RECORD OF ITS OWN.   *
      *                                                               *
      * REVISION HISTORY                                              *
      *   V1.00  1989-02-08  R.T.WHEELER   INITIAL - ASSEMBLER F      *
      *   V1.02  1994-10-24  D.OKONKWO     RESULT TEXT WIDENED TO     *
      *                                    ELEVEN BYTES               *
      *   V1.05  1999-12-06  E.KOWALCZYK   DESCENDING KEY ON THE      *
      *                                    PACKED DIFFERENCE          *
      *   V2.00  2006-03-30  P.NAIR        RECODED IN COBOL FOR LE    *
      *   V2.02  2011-07-18  A.BUKOWSKI    DIFFERENCE BANDS ADDED     *
      *   V2.03  2016-02-29  T.YAMASHITA   TOLERANCE FIELD READ AND   *
      *                                    REPORTED, NOT TESTED       *
      *   V2.05  2019-08-13  M.HAAS        RECOMPILE ONLY - LE V6.2   *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-MODULE-IDENT.
           05  FILLER                  PIC X(08) VALUE 'CABSXOOB'.
           05  FILLER                  PIC X(08) VALUE ' V2.05  '.
       01  WS-SWITCHES.
           05  WS-FIRST-ENTRY-SW       PIC X(01) VALUE 'Y'.
               88  WS-FIRST-ENTRY      VALUE 'Y'.
           05  WS-EOF-SEEN-SW          PIC X(01) VALUE 'N'.
               88  WS-EOF-SEEN         VALUE 'Y'.
      *
      * THE TEXT THE SELECTION IS MADE ON.  BOTH LITERALS ARE
      * ELEVEN BYTES AND ARE HELD HERE RATHER THAN CODED INLINE, SO
      * THAT THE TRAILING SPACE IN EACH ONE IS VISIBLE IN THE
      * SOURCE.
      *
       01  WS-RESULT-LITERALS.
           05  WS-RL-OUT-OF-BAL        PIC X(11)
                                       VALUE 'OUT OF BAL '.
           05  WS-RL-IN-BALANCE        PIC X(11)
                                       VALUE 'IN BALANCE '.
           05  WS-RL-NOT-PROVED        PIC X(11)
                                       VALUE 'NOT PROVED '.
       01  WS-COUNTERS.
           05  WS-ENTRY-CNT            PIC S9(11) COMP-3 VALUE 0.
           05  WS-KEPT-CNT             PIC S9(11) COMP-3 VALUE 0.
           05  WS-DROP-TOTAL           PIC S9(11) COMP-3 VALUE 0.
           05  WS-DROP-IN-BALANCE      PIC S9(11) COMP-3 VALUE 0.
           05  WS-DROP-NOT-PROVED      PIC S9(09) COMP-3 VALUE 0.
           05  WS-DROP-OTHER-TEXT      PIC S9(09) COMP-3 VALUE 0.
           05  WS-DROP-DIFF-NONZERO    PIC S9(09) COMP-3 VALUE 0.
           05  WS-KEPT-DIFF-ZERO       PIC S9(09) COMP-3 VALUE 0.
           05  WS-KEPT-INSIDE-TOL      PIC S9(09) COMP-3 VALUE 0.
           05  WS-KEPT-NEGATIVE        PIC S9(09) COMP-3 VALUE 0.
      *
      * THE DIFFERENCE BANDS.  THE BOUNDARIES ARE HELD IN SOURCE
      * AND HAVE NOT MOVED SINCE THEY WERE PUT IN.
      *
       01  WS-DIFF-BANDS.
           05  WS-DB-UNDER-1           PIC S9(09) COMP-3 VALUE 0.
           05  WS-DB-1-99              PIC S9(09) COMP-3 VALUE 0.
           05  WS-DB-100-999           PIC S9(09) COMP-3 VALUE 0.
           05  WS-DB-1000-9999         PIC S9(09) COMP-3 VALUE 0.
           05  WS-DB-10000-UP          PIC S9(09) COMP-3 VALUE 0.
       01  WS-BAND-LIMITS.
           05  WS-BL-LOW               PIC S9(09)V9(02) COMP-3
                                                  VALUE 1.
           05  WS-BL-MID               PIC S9(09)V9(02) COMP-3
                                                  VALUE 100.
           05  WS-BL-HIGH              PIC S9(09)V9(02) COMP-3
                                                  VALUE 1000.
           05  WS-BL-TOP               PIC S9(09)V9(02) COMP-3
                                                  VALUE 10000.
       01  WS-VALUES.
           05  WS-KEPT-DIFF-SIGNED     PIC S9(15)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-KEPT-DIFF-ABS        PIC S9(15)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DROP-DIFF-ABS        PIC S9(15)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-LARGEST-DIFF         PIC S9(13)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-WORK-FIELDS.
           05  WS-TEST-TEXT            PIC X(11) VALUE SPACES.
           05  WS-ABS-DIFF             PIC S9(13)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-LARGEST-BAN          PIC X(13) VALUE SPACES.
      *
       LINKAGE SECTION.
       01  LK-PARM-LIST.
           05  LK-RECORD-PTR           POINTER.
           05  LK-LENGTH-PTR           POINTER.
      *
      * A HAND MAINTAINED VIEW OF THE 120 BYTE BILL PROOF RECORD.
      * THE POSITIONS THE CONTROL CARD QUOTES ARE THE POSITIONS IN
      * THIS VIEW - 66,8 THE PACKED DIFFERENCE THE SORT KEYS ON IN
      * DESCENDING ORDER, AND 74,11 THE RESULT TEXT.
      *
       01  LK-SORT-RECORD.
           05  LK-BP-BAN               PIC X(13).
           05  LK-BP-BILL-PERIOD       PIC 9(06).
           05  LK-BP-OCN               PIC X(04).
           05  LK-BP-RAO               PIC 9(03).
           05  LK-BP-STATE-CD          PIC X(02).
           05  LK-BP-PROOF-TYPE        PIC X(01).
           05  LK-BP-DETAIL-AMT        PIC S9(13)V9(02) COMP-3.
           05  LK-BP-HEADER-AMT        PIC S9(13)V9(02) COMP-3.
           05  LK-BP-TAX-AMT           PIC S9(13)V9(02) COMP-3.
           05  LK-BP-ADJ-AMT           PIC S9(13)V9(02) COMP-3.
           05  LK-BP-LINE-CNT          PIC S9(07) COMP-3.
           05  LK-BP-DIFF-AMT          PIC S9(13)V9(02) COMP-3.
           05  LK-BP-RESULT-TEXT       PIC X(11).
           05  LK-BP-PROOF-YYDDD       PIC 9(05).
           05  LK-BP-TOLERANCE         PIC S9(09)V9(02) COMP-3.
           05  LK-BP-DETECT-PGM        PIC X(08).
           05  LK-BP-RUN-ID            PIC X(12).
           05  LK-BP-FILLER            PIC X(05).
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
           PERFORM P2000-SCREEN-RESULT THRU P2000-EXIT.
           IF RETURN-CODE = 4
               GOBACK
           END-IF.
           PERFORM P3000-BAND-SELECTED THRU P3000-EXIT.
           MOVE ZERO TO RETURN-CODE.
           GOBACK.

       P1000-INIT.
      * ENTERED ONCE PER RECORD.  ONLY THE FIRST ENTRY DOES ANY
      * SET UP - EVERY LATER ENTRY FALLS STRAIGHT THROUGH.
           MOVE ZERO TO RETURN-CODE.
           IF WS-FIRST-ENTRY
               MOVE 'N' TO WS-FIRST-ENTRY-SW
               DISPLAY 'CABSXOOB ENTERED - OUT OF BALANCE SELECT'
           END-IF.

       P1000-EXIT.
           EXIT.

       P2000-SCREEN-RESULT.
      * THE ONLY TEST IS ON THE RESULT TEXT.  THE DIFFERENCE FIELD
      * IS READ AND COUNTED BUT TAKES NO PART IN THE SELECTION.
           MOVE LK-BP-RESULT-TEXT TO WS-TEST-TEXT.
           IF WS-TEST-TEXT = WS-RL-OUT-OF-BAL
               ADD 1 TO WS-KEPT-CNT
               GO TO P2000-EXIT
           END-IF.
           PERFORM P2400-COUNT-DROPPED THRU P2400-EXIT.
           MOVE 4 TO RETURN-CODE.

       P2000-EXIT.
           EXIT.

       P2400-COUNT-DROPPED.
      * A RECORD THAT IS NOT SELECTED IS STILL WORTH COUNTING.  THE
      * IN BALANCE FIGURE WITH A NON ZERO DIFFERENCE IS THE ONE
      * THAT SHOWS HOW MUCH THE UPSTREAM TOLERANCE IS ABSORBING.
           ADD 1 TO WS-DROP-TOTAL.
           MOVE LK-BP-DIFF-AMT TO WS-ABS-DIFF.
           IF WS-ABS-DIFF < ZERO
               COMPUTE WS-ABS-DIFF = WS-ABS-DIFF * -1
           END-IF.
           ADD WS-ABS-DIFF TO WS-DROP-DIFF-ABS.
           EVALUATE WS-TEST-TEXT
               WHEN WS-RL-IN-BALANCE
                   ADD 1 TO WS-DROP-IN-BALANCE
               WHEN WS-RL-NOT-PROVED
                   ADD 1 TO WS-DROP-NOT-PROVED
               WHEN OTHER
                   ADD 1 TO WS-DROP-OTHER-TEXT
           END-EVALUATE.
           IF LK-BP-DIFF-AMT NOT = ZERO
               ADD 1 TO WS-DROP-DIFF-NONZERO
           END-IF.

       P2400-EXIT.
           EXIT.

       P3000-BAND-SELECTED.
      * BAND THE SELECTED RECORD BY THE SIZE OF ITS DIFFERENCE AND
      * KEEP THE LARGEST ONE SEEN, WHICH IS THE RECORD THAT WILL
      * PRINT FIRST ON THE LISTING BECAUSE THE SORT KEY ON THE
      * PACKED DIFFERENCE IS DESCENDING.
           MOVE LK-BP-DIFF-AMT TO WS-ABS-DIFF.
           IF WS-ABS-DIFF < ZERO
               COMPUTE WS-ABS-DIFF = WS-ABS-DIFF * -1
               ADD 1 TO WS-KEPT-NEGATIVE
           END-IF.
           ADD LK-BP-DIFF-AMT TO WS-KEPT-DIFF-SIGNED.
           ADD WS-ABS-DIFF     TO WS-KEPT-DIFF-ABS.
           IF WS-ABS-DIFF > WS-LARGEST-DIFF
               MOVE WS-ABS-DIFF TO WS-LARGEST-DIFF
               MOVE LK-BP-BAN   TO WS-LARGEST-BAN
           END-IF.
           IF LK-BP-DIFF-AMT = ZERO
               ADD 1 TO WS-KEPT-DIFF-ZERO
           END-IF.
           IF WS-ABS-DIFF NOT > LK-BP-TOLERANCE
               ADD 1 TO WS-KEPT-INSIDE-TOL
           END-IF.
           PERFORM P3400-PLACE-BAND THRU P3400-EXIT.

       P3000-EXIT.
           EXIT.

       P3400-PLACE-BAND.
           EVALUATE TRUE
               WHEN WS-ABS-DIFF < WS-BL-LOW
                   ADD 1 TO WS-DB-UNDER-1
               WHEN WS-ABS-DIFF < WS-BL-MID
                   ADD 1 TO WS-DB-1-99
               WHEN WS-ABS-DIFF < WS-BL-HIGH
                   ADD 1 TO WS-DB-100-999
               WHEN WS-ABS-DIFF < WS-BL-TOP
                   ADD 1 TO WS-DB-1000-9999
               WHEN OTHER
                   ADD 1 TO WS-DB-10000-UP
           END-EVALUATE.

       P3400-EXIT.
           EXIT.

       P8000-END-OF-INPUT.
      * A NULL RECORD ADDRESS MEANS SORTIN IS EXHAUSTED.  THE EXIT
      * HAS NOTHING TO INSERT, SO IT REPLIES ZERO AND WRITES ITS
      * TALLIES TO THE MESSAGE DATA SET.  THESE COUNTS ARE THE ONLY
      * RECORD OF WHAT THE EXIT REMOVED - THEY ARE NOT CARRIED INTO
      * ANY CONTROL RECORD.
           MOVE 'Y' TO WS-EOF-SEEN-SW.
           DISPLAY 'CABSXOOB ENTRIES     ' WS-ENTRY-CNT.
           DISPLAY 'CABSXOOB SELECTED    ' WS-KEPT-CNT.
           DISPLAY 'CABSXOOB DROPPED     ' WS-DROP-TOTAL.
           DISPLAY 'CABSXOOB IN BALANCE  ' WS-DROP-IN-BALANCE.
           DISPLAY 'CABSXOOB NOT PROVED  ' WS-DROP-NOT-PROVED.
           DISPLAY 'CABSXOOB OTHER TEXT  ' WS-DROP-OTHER-TEXT.
           DISPLAY 'CABSXOOB DROP W DIFF ' WS-DROP-DIFF-NONZERO.
           DISPLAY 'CABSXOOB DROP DIFF   ' WS-DROP-DIFF-ABS.
           DISPLAY 'CABSXOOB SEL DIFF ZER' WS-KEPT-DIFF-ZERO.
           DISPLAY 'CABSXOOB SEL IN TOL  ' WS-KEPT-INSIDE-TOL.
           DISPLAY 'CABSXOOB SEL NEGATIVE' WS-KEPT-NEGATIVE.
           DISPLAY 'CABSXOOB SEL DIFF NET' WS-KEPT-DIFF-SIGNED.
           DISPLAY 'CABSXOOB SEL DIFF ABS' WS-KEPT-DIFF-ABS.
           DISPLAY 'CABSXOOB BAND UNDER 1' WS-DB-UNDER-1.
           DISPLAY 'CABSXOOB BAND 1-99   ' WS-DB-1-99.
           DISPLAY 'CABSXOOB BAND 100-999' WS-DB-100-999.
           DISPLAY 'CABSXOOB BAND 1K-9K  ' WS-DB-1000-9999.
           DISPLAY 'CABSXOOB BAND 10K UP ' WS-DB-10000-UP.
           DISPLAY 'CABSXOOB LARGEST DIFF' WS-LARGEST-DIFF.
           DISPLAY 'CABSXOOB LARGEST BAN ' WS-LARGEST-BAN.
           MOVE ZERO TO RETURN-CODE.

       P8000-EXIT.
           EXIT.
