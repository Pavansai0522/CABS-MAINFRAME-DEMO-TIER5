       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABSXMIN.
      *****************************************************************
      * CABSXMIN - SORT E35 OUTPUT EXIT - MINUTE SUMMARISATION        *
      * APPLICATION : CABS                                            *
      * COMPILER    : ENTERPRISE COBOL                                *
      * INVOKED BY  : OS SORT/MERGE, MODS STATEMENT ON CONTROL CARD   *
      *               MEMBER JCL/CTLCARDS/MVT/CABSRT03 -              *
      *               E35=(CABSXMIN,8192,SORTEXIT,N)                  *
      * INPUTS      : ONE 200 BYTE RECORD PER ENTRY FROM THE FINAL    *
      *               MERGE, IN OCN, BAN, CIRCUIT AND USOC ORDER      *
      * OUTPUTS     : ONE 200 BYTE RECORD PER CONTROL GROUP, CARRYING *
      *               THE GROUP TOTAL AT COLUMN 120                   *
      * CONTROL     : NONE - EXITS DO NOT WRITE CTLOUT, CABS-STD-041  *
      * BALANCE     : RECORDS IN = RECORDS OUT + WS-ABSORBED-CNT      *
      *               SUM OF RELEASED MINUTES = SUM OF INPUT MINUTES  *
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
      *     04  DELETE THE RECORD - DO NOT WRITE IT TO SORTOUT        *
      *     08  WRITE THE RECORD ADDRESSED BY WORD ONE, THEN ENTER    *
      *         THIS EXIT AGAIN WITH THE SAME INPUT RECORD            *
      *     12  DO NOT ENTER THIS EXIT AGAIN                          *
      *     16  TERMINATE THE SORT.  WORKING STORAGE PERSISTS FOR     *
      *         THE LIFE OF THE SORT STEP.                            *
      *                                                               *
      * WHAT THIS EXIT DECIDES                                        *
      *   THE CARD CARRIED THE SUMMARISATION UNTIL THE MOVE TO THE    *
      *   MVT CONTROL CARD FORMAT.  THE RULE IS NOW HERE.  RECORDS    *
      *   SHARING AN OCN, A BAN, A CIRCUIT AND A USOC COLLAPSE        *
      *   INTO ONE RECORD AND THEIR PACKED MINUTES FIELD AT           *
      *   COLUMN 120, SEVEN BYTES, IS ADDED.  THE FIRST RECORD OF     *
      *   THE GROUP IS THE IMAGE THAT SURVIVES.  CABING03 MATCHES     *
      *   EXACT SEQUENCE NUMBERS AND WILL NOT SEE THESE, AND NO       *
      *   CABING PROGRAM IS TOLD THAT THE COLLAPSE HAPPENED.          *
      *                                                               *
      * REVISION HISTORY                                              *
      *   V1.00  1988-02-22  R.T.WHEELER   INITIAL - ASSEMBLER F      *
      *   V1.04  1992-10-06  D.OKONKWO     USOC ADDED TO THE GROUP    *
      *   V1.08  1997-05-13  P.NAIR        CYCLE DATE DROPPED FROM    *
      *                                    THE GROUP                  *
      *   V2.00  2005-11-14  L.FERREIRA    RECODED IN COBOL FOR LE    *
      *   V2.02  2010-04-27  E.KOWALCZYK   OVERFLOW COUNTED           *
      *   V2.05  2019-02-11  J.CALLAGHAN   SUMMARISATION MOVED OFF    *
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
      * CABING05, WHICH READS SORTOUT, IS UNAFFECTED BY THIS
      * MODULE - IT REPORTS WHAT IT READS AS ITS OWN CT-READ.
      *
      * THE HELD IMAGE AND THE ACCUMULATOR SURVIVE FROM ONE ENTRY
      * TO THE NEXT.  THE SORT STEP LOADS THIS MODULE ONCE.
      *
       01  WS-MODULE-IDENT.
           05  FILLER                  PIC X(08) VALUE 'CABSXMIN'.
           05  FILLER                  PIC X(08) VALUE ' V2.05  '.
       01  WS-COUNTERS.
           05  WS-ENTRY-CNT            PIC S9(11) COMP-3 VALUE 0.
           05  WS-GROUP-CNT            PIC S9(11) COMP-3 VALUE 0.
           05  WS-ABSORBED-CNT         PIC S9(11) COMP-3 VALUE 0.
           05  WS-COLLAPSED-GRP        PIC S9(11) COMP-3 VALUE 0.
           05  WS-OVERFLOW-CNT         PIC S9(09) COMP-3 VALUE 0.
           05  WS-NEG-MIN-CNT          PIC S9(09) COMP-3 VALUE 0.
           05  WS-MAX-GROUP            PIC S9(07) COMP-3 VALUE 0.
           05  WS-MIN-IN               PIC S9(15)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-MIN-RELEASED         PIC S9(15)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-SWITCHES.
           05  WS-FIRST-ENTRY-SW       PIC X(01) VALUE 'Y'.
               88  WS-FIRST-ENTRY      VALUE 'Y'.
           05  WS-GROUP-OPEN-SW        PIC X(01) VALUE 'N'.
               88  WS-GROUP-OPEN       VALUE 'Y'.
           05  WS-PENDING-SW           PIC X(01) VALUE 'N'.
               88  WS-PENDING          VALUE 'Y'.
           05  WS-MERGE-DONE-SW        PIC X(01) VALUE 'N'.
               88  WS-MERGE-DONE       VALUE 'Y'.
       01  WS-BREAK-CONTROL.
           05  WS-EOM-STATE            PIC 9(01) VALUE 0.
           05  WS-GRP-MIN              PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-GRP-COUNT            PIC S9(07) COMP-3 VALUE 0.
           05  WS-HOLD-KEY.
               10  WS-HK-OCN           PIC X(04) VALUE SPACES.
               10  WS-HK-BAN           PIC X(13) VALUE SPACES.
               10  WS-HK-CIRCUIT       PIC X(10) VALUE SPACES.
               10  WS-HK-USOC          PIC X(05) VALUE SPACES.
           05  WS-THIS-KEY.
               10  WS-TK-OCN           PIC X(04) VALUE SPACES.
               10  WS-TK-BAN           PIC X(13) VALUE SPACES.
               10  WS-TK-CIRCUIT       PIC X(10) VALUE SPACES.
               10  WS-TK-USOC          PIC X(05) VALUE SPACES.
           05  WS-NEXT-KEY             PIC X(32) VALUE SPACES.
      *
      * THE HELD IMAGE.  A GROUP IS OPENED BY COPYING ITS FIRST
      * RECORD HERE.  THE MINUTES FIELD IS REPLACED BY THE GROUP
      * TOTAL IMMEDIATELY BEFORE THE IMAGE GOES BACK TO SORT.
      *
       01  WS-HOLD-RECORD.
           05  WS-HD-LEAD              PIC X(119).
           05  WS-HD-CONV-MIN          PIC S9(11)V9(02) COMP-3.
           05  WS-HD-TAIL              PIC X(74).
      *
      * THE RECORD THAT CAUSED THE BREAK.  A REPLY OF EIGHT MAKES
      * SORT WRITE THE IMAGE AND ENTER THIS EXIT AGAIN, SO THE
      * BREAKING RECORD IS COPIED HERE FIRST AND WORD ONE IS NOT
      * REFERENCED ON THE RE-ENTRY.
      *
       01  WS-NEXT-RECORD.
           05  WS-NX-LEAD              PIC X(119).
           05  WS-NX-CONV-MIN          PIC S9(11)V9(02) COMP-3.
           05  WS-NX-TAIL              PIC X(74).
      *
       LINKAGE SECTION.
       01  LK-PARM-LIST.
           05  LK-RECORD-PTR           POINTER.
           05  LK-PREV-PTR             POINTER.
           05  LK-LENGTH-PTR           POINTER.
       01  LK-SORT-RECORD.
           05  LK-MN-REC-TYPE          PIC X(02).
           05  LK-MN-USAGE-TYPE        PIC X(01).
           05  LK-MN-FILLER-1          PIC X(01).
           05  LK-MN-OCN               PIC X(04).
           05  LK-MN-BAN               PIC X(13).
           05  LK-MN-SEQ-NBR           PIC 9(09) COMP-3.
           05  LK-MN-RAO               PIC X(03).
           05  LK-MN-EDIT-STATUS       PIC X(01).
           05  LK-MN-CIC               PIC 9(04).
           05  LK-MN-CONN-YYDDD        PIC 9(05).
           05  LK-MN-JURIS-CD          PIC X(01).
           05  LK-MN-STATE-CD          PIC X(02).
           05  LK-MN-FILLER-2          PIC X(02).
           05  LK-MN-SRC-SYSTEM        PIC X(02).
           05  LK-MN-LOAD-YYDDD        PIC 9(05).
           05  LK-MN-TRUNK-GRP         PIC X(08).
           05  LK-MN-CIRCUIT-ID        PIC X(10).
           05  LK-MN-USOC              PIC X(05).
           05  LK-MN-FILLER-3          PIC X(45).
           05  LK-MN-CONV-MIN          PIC S9(11)V9(02) COMP-3.
           05  LK-MN-TAIL              PIC X(74).
      *
       PROCEDURE DIVISION USING LK-PARM-LIST.
       P0000-MAINLINE.
           MOVE ZERO TO RETURN-CODE.
           IF WS-FIRST-ENTRY
               PERFORM P1000-INIT THRU P1000-EXIT
           END-IF.
           IF WS-MERGE-DONE
               PERFORM P8000-END-OF-MERGE THRU P8000-EXIT
               GOBACK
           END-IF.
           IF WS-PENDING
               PERFORM P5000-RESUME-BREAK THRU P5000-EXIT
               GOBACK
           END-IF.
           IF LK-RECORD-PTR = NULL
               MOVE 'Y' TO WS-MERGE-DONE-SW
               PERFORM P8000-END-OF-MERGE THRU P8000-EXIT
               GOBACK
           END-IF.
           SET ADDRESS OF LK-SORT-RECORD TO LK-RECORD-PTR.
           ADD 1 TO WS-ENTRY-CNT.
           PERFORM P2000-BUILD-KEY THRU P2000-EXIT.
           PERFORM P3000-TEST-BREAK THRU P3000-EXIT.
           GOBACK.

       P1000-INIT.
           MOVE 'N' TO WS-FIRST-ENTRY-SW.
           MOVE ZERO TO WS-GRP-MIN.
           MOVE ZERO TO WS-GRP-COUNT.
           MOVE SPACES TO WS-HOLD-KEY.
           DISPLAY 'CABSXMIN ENTERED - OCN BAN CIRCUIT USOC'.

       P1000-EXIT.
           EXIT.

       P2000-BUILD-KEY.
      * BECAUSE THE MERGE DELIVERS ASCENDING KEYS, ANY CHANGE IN
      * THIS KEY IS A GENUINE END OF GROUP.
           MOVE LK-MN-OCN        TO WS-TK-OCN.
           MOVE LK-MN-BAN        TO WS-TK-BAN.
           MOVE LK-MN-CIRCUIT-ID TO WS-TK-CIRCUIT.
           MOVE LK-MN-USOC       TO WS-TK-USOC.
           ADD LK-MN-CONV-MIN TO WS-MIN-IN.
           IF LK-MN-CONV-MIN < ZERO
               ADD 1 TO WS-NEG-MIN-CNT
           END-IF.

       P2000-EXIT.
           EXIT.

       P3000-TEST-BREAK.
      * NO GROUP OPEN - OPEN ONE.  SAME KEY - TAKE THE MINUTES
      * AND DELETE THE RECORD.  NEW KEY - RELEASE THE IMAGE.
           IF NOT WS-GROUP-OPEN
               PERFORM P4000-OPEN-GROUP THRU P4000-EXIT
               MOVE 4 TO RETURN-CODE
               GO TO P3000-EXIT
           END-IF.
           IF WS-THIS-KEY = WS-HOLD-KEY
               PERFORM P4200-ABSORB THRU P4200-EXIT
               MOVE 4 TO RETURN-CODE
               GO TO P3000-EXIT
           END-IF.
           MOVE LK-SORT-RECORD TO WS-NEXT-RECORD.
           MOVE WS-THIS-KEY    TO WS-NEXT-KEY.
           MOVE 'Y' TO WS-PENDING-SW.
           PERFORM P4400-RELEASE-GROUP THRU P4400-EXIT.

       P3000-EXIT.
           EXIT.

       P4000-OPEN-GROUP.
      * THE FIRST RECORD OF A GROUP BECOMES THE IMAGE.  EVERY
      * FIELD OUTSIDE THE MINUTES KEEPS THAT RECORD'S VALUE.
           MOVE LK-SORT-RECORD TO WS-HOLD-RECORD.
           MOVE WS-THIS-KEY    TO WS-HOLD-KEY.
           MOVE LK-MN-CONV-MIN TO WS-GRP-MIN.
           MOVE 1 TO WS-GRP-COUNT.
           MOVE 'Y' TO WS-GROUP-OPEN-SW.

       P4000-EXIT.
           EXIT.

       P4200-ABSORB.
      * TAKE THE MINUTES AND THROW THE RECORD AWAY.  A TOTAL THAT
      * WILL NOT FIT ELEVEN DIGITS IS LEFT AS IT STANDS AND THE
      * RECORD IS COUNTED ON THE OVERFLOW LINE.
           ADD LK-MN-CONV-MIN TO WS-GRP-MIN
               ON SIZE ERROR
                   ADD 1 TO WS-OVERFLOW-CNT
           END-ADD.
           ADD 1 TO WS-GRP-COUNT.
           ADD 1 TO WS-ABSORBED-CNT.

       P4200-EXIT.
           EXIT.

       P4400-RELEASE-GROUP.
      * STAMP THE GROUP TOTAL INTO THE HELD IMAGE AND HAND IT TO
      * SORT.  WORD ONE POINTS AT WORKING STORAGE, WHICH IS WHY
      * THE MODS STATEMENT ASKS FOR EIGHT THOUSAND BYTES.
           MOVE WS-GRP-MIN TO WS-HD-CONV-MIN.
           ADD WS-GRP-MIN TO WS-MIN-RELEASED.
           ADD 1 TO WS-GROUP-CNT.
           IF WS-GRP-COUNT > WS-MAX-GROUP
               MOVE WS-GRP-COUNT TO WS-MAX-GROUP
           END-IF.
           IF WS-GRP-COUNT > 1
               ADD 1 TO WS-COLLAPSED-GRP
           END-IF.
           SET LK-RECORD-PTR TO ADDRESS OF WS-HOLD-RECORD.
           MOVE 8 TO RETURN-CODE.

       P4400-EXIT.
           EXIT.

       P5000-RESUME-BREAK.
      * SORT HAS WRITTEN THE IMAGE AND HAS ENTERED THE EXIT AGAIN
      * WITH THE SAME INPUT RECORD.  THE NEW GROUP IS OPENED FROM
      * THE COPY TAKEN AT THE BREAK.  THE RECORD WAS COUNTED ON
      * THAT ENTRY AND IS NOT COUNTED AGAIN HERE.
           MOVE 'N' TO WS-PENDING-SW.
           MOVE WS-NEXT-RECORD TO WS-HOLD-RECORD.
           MOVE WS-NEXT-KEY    TO WS-HOLD-KEY.
           MOVE WS-NX-CONV-MIN TO WS-GRP-MIN.
           MOVE 1 TO WS-GRP-COUNT.
           MOVE 'Y' TO WS-GROUP-OPEN-SW.
           MOVE 4 TO RETURN-CODE.

       P5000-EXIT.
           EXIT.

       P8000-END-OF-MERGE.
      * A NULL RECORD ADDRESS MEANS THE MERGE IS EXHAUSTED.  THE
      * LAST GROUP IS RELEASED HERE - THIS IS THE ONLY POINT AT
      * WHICH IT CAN REACH SORTOUT.
           IF WS-EOM-STATE = 0
               MOVE 1 TO WS-EOM-STATE
               IF WS-GROUP-OPEN
                   MOVE 'N' TO WS-GROUP-OPEN-SW
                   PERFORM P4400-RELEASE-GROUP THRU P4400-EXIT
                   GO TO P8000-EXIT
               END-IF
           END-IF.
           MOVE 2 TO WS-EOM-STATE.
           PERFORM P8600-REPORT THRU P8600-EXIT.
           MOVE ZERO TO RETURN-CODE.

       P8000-EXIT.
           EXIT.

       P8600-REPORT.
      * THE TALLIES GO TO THE MESSAGE DATA SET.  NOTHING
      * DOWNSTREAM CAN TELL A COLLAPSED RECORD FROM ANY OTHER.
           DISPLAY 'CABSXMIN ENTRIES     ' WS-ENTRY-CNT.
           DISPLAY 'CABSXMIN GROUPS OUT  ' WS-GROUP-CNT.
           DISPLAY 'CABSXMIN ABSORBED    ' WS-ABSORBED-CNT.
           DISPLAY 'CABSXMIN MULTI GROUPS' WS-COLLAPSED-GRP.
           DISPLAY 'CABSXMIN LARGEST GRP ' WS-MAX-GROUP.
           DISPLAY 'CABSXMIN MINUTES IN  ' WS-MIN-IN.
           DISPLAY 'CABSXMIN MINUTES OUT ' WS-MIN-RELEASED.
           DISPLAY 'CABSXMIN OVERFLOWS   ' WS-OVERFLOW-CNT.
           DISPLAY 'CABSXMIN NEGATIVE MIN' WS-NEG-MIN-CNT.

       P8600-EXIT.
           EXIT.
