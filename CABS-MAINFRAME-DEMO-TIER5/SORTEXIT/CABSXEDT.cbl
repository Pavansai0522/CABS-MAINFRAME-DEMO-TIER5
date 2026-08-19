       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABSXEDT.
      *****************************************************************
      * CABSXEDT - SORT E15 INPUT EXIT - FATAL EDIT STATUS REMOVAL    *
      * APPLICATION : CABS                                            *
      * COMPILER    : ENTERPRISE COBOL                                *
      * INVOKED BY  : OS SORT/MERGE, MODS STATEMENT ON CONTROL CARD   *
      *               MEMBER JCL/CTLCARDS/MVT/CABSRT02 -              *
      *               E15=(CABSXEDT,4096,SORTEXIT,N)                  *
      * INPUTS      : ONE 200 BYTE SORTIN RECORD PER ENTRY FROM       *
      *               TELCABS.CABS.USAGE.EDITED                       *
      * OUTPUTS     : THE SAME RECORD UNCHANGED, OR A DELETE REPLY    *
      * CONTROL     : NONE - EXITS DO NOT WRITE CTLOUT, CABS-STD-041  *
      * BALANCE     : SORTIN = WS-KEPT-CNT + WS-DROP-CNT              *
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
      *   THE COUNTERS ACCUMULATE ACROSS ENTRIES.  THE EXIT IS        *
      *   ENTERED A FINAL TIME WITH A NULL RECORD ADDRESS.            *
      *                                                               *
      * WHAT THIS EXIT DECIDES                                        *
      *   THE CARD CARRIED THE OMIT UNTIL THE MOVE TO THE MVT         *
      *   CONTROL CARD FORMAT.  THE RULE IS NOW HERE.  A RECORD       *
      *   WHOSE EDIT STATUS BYTE IN COLUMN 30 IS 6 THROUGH 9 IS       *
      *   DELETED BEFORE IT REACHES CABING02.  THOSE RECORDS WERE     *
      *   ALREADY DIVERTED TO SUSPENSE BY CABING01, AND VALIDATING    *
      *   THEM A SECOND TIME WOULD SUSPEND THEM TWICE AND LEAVE       *
      *   THE CONTROL TOTALS SHORT BY THE DUPLICATED COUNT.  A        *
      *   STATUS BYTE THAT IS NOT A DIGIT IS KEPT AND COUNTED ON      *
      *   ITS OWN LINE, WHICH IS WHAT THE OMIT DID.                   *
      *                                                               *
      * REVISION HISTORY                                              *
      *   V1.00  1987-09-08  R.T.WHEELER   INITIAL - ASSEMBLER F      *
      *   V1.03  1991-06-19  D.OKONKWO     STATUS 8 AND 9 ADDED TO    *
      *                                    THE FATAL RANGE            *
      *   V1.07  1996-03-04  P.NAIR        STATUS PROFILE WRITTEN     *
      *                                    TO THE MESSAGE DATA SET    *
      *   V2.00  2005-06-30  L.FERREIRA    RECODED IN COBOL FOR LE    *
      *   V2.02  2009-07-21  S.MBEKI       MINUTES REMOVED TALLIED    *
      *   V2.04  2014-12-03  B.R.HALVORSEN NON DIGIT STATUS COUNTED   *
      *   V2.06  2019-01-29  J.CALLAGHAN   OMIT TAKEN OFF THE         *
      *                                    CONTROL CARD INTO HERE     *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
      * THIS EXIT WRITES NO CONTROL RECORD.  THE BALANCE OF
      * CABING02, WHICH READS SORTOUT, IS UNAFFECTED BY THIS
      * MODULE - IT COUNTS WHAT IT READS AND REPORTS THAT AS ITS
      * OWN CT-READ.
      *
      * COUNTERS SURVIVE FROM ONE ENTRY TO THE NEXT.  THE SORT
      * STEP LOADS THIS MODULE ONCE AND HOLDS IT FOR THE WHOLE
      * PASS.
      *
       01  WS-MODULE-IDENT.
           05  FILLER                  PIC X(08) VALUE 'CABSXEDT'.
           05  FILLER                  PIC X(08) VALUE ' V2.06  '.
       01  WS-COUNTERS.
           05  WS-ENTRY-CNT            PIC S9(11) COMP-3 VALUE 0.
           05  WS-KEPT-CNT             PIC S9(11) COMP-3 VALUE 0.
           05  WS-DROP-CNT             PIC S9(11) COMP-3 VALUE 0.
           05  WS-NON-DIGIT-CNT        PIC S9(11) COMP-3 VALUE 0.
           05  WS-MIN-REMOVED          PIC S9(13)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-MIN-KEPT             PIC S9(13)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-SWITCHES.
           05  WS-FIRST-ENTRY-SW       PIC X(01) VALUE 'Y'.
               88  WS-FIRST-ENTRY      VALUE 'Y'.
           05  WS-EOF-SEEN-SW          PIC X(01) VALUE 'N'.
               88  WS-EOF-SEEN         VALUE 'Y'.
      *
      * THE STATUS PROFILE.  ONE SLOT PER DIGIT.  SLOT ONE HOLDS
      * STATUS ZERO, SO THE SUBSCRIPT IS THE DIGIT PLUS ONE.  THE
      * PROFILE IS PRINTED AT END OF INPUT AND IS THE ONLY PLACE
      * THE SHAPE OF A CYCLE'S EDIT RESULTS IS VISIBLE.
      *
       01  WS-STATUS-PROFILE.
           05  WS-STATUS-CNT OCCURS 10 TIMES PIC S9(11) COMP-3
                                                  VALUE 0.
       01  WS-STATUS-MAX               PIC S9(04) COMP VALUE 10.
       01  WS-WORK-FIELDS.
           05  WS-STATUS-TEST          PIC X(01) VALUE SPACE.
               88  WS-STATUS-FATAL     VALUE '6' THRU '9'.
               88  WS-STATUS-CLEAN     VALUE '0' THRU '5'.
           05  WS-STATUS-NUM REDEFINES WS-STATUS-TEST
                                       PIC 9(01).
           05  WS-STATUS-SUB           PIC S9(04) COMP VALUE 0.
           05  WS-DROP-PCT             PIC S9(05)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-PX                   PIC S9(04) COMP VALUE 0.
           05  WS-SHOW-DIGIT           PIC 9(01) VALUE 0.
      *
      * THE EDITED USAGE LAYOUT.  THIS IS A HAND MAINTAINED VIEW
      * OF THE SAME TWO HUNDRED BYTES CABING02 READS.  THE COLUMN
      * THAT MATTERS TO THIS EXIT IS 30, THE EDIT STATUS BYTE THE
      * INTAKE EDITS STAMPED.
      *
       LINKAGE SECTION.
       01  LK-PARM-LIST.
           05  LK-RECORD-PTR           POINTER.
           05  LK-LENGTH-PTR           POINTER.
       01  LK-SORT-RECORD.
           05  LK-ED-REC-TYPE          PIC X(02).
           05  LK-ED-USAGE-TYPE        PIC X(01).
           05  LK-ED-FILLER-1          PIC X(01).
           05  LK-ED-OCN               PIC X(04).
           05  LK-ED-BAN               PIC X(13).
           05  LK-ED-SEQ-NBR           PIC 9(09) COMP-3.
           05  LK-ED-RAO               PIC X(03).
           05  LK-ED-EDIT-STATUS       PIC X(01).
           05  LK-ED-CIC               PIC 9(04).
           05  LK-ED-CONN-YYDDD        PIC 9(05).
           05  LK-ED-JURIS-CD          PIC X(01).
           05  LK-ED-STATE-CD          PIC X(02).
           05  LK-ED-FILLER-2          PIC X(02).
           05  LK-ED-SRC-SYSTEM        PIC X(02).
           05  LK-ED-LOAD-YYDDD        PIC 9(05).
           05  LK-ED-TRUNK-GRP         PIC X(08).
           05  LK-ED-CIRCUIT-ID        PIC X(10).
           05  LK-ED-USOC              PIC X(05).
           05  LK-ED-FILLER-3          PIC X(45).
           05  LK-ED-CONV-MIN          PIC S9(11)V9(02) COMP-3.
           05  LK-ED-TAIL              PIC X(74).
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
           PERFORM P2000-PROFILE-STATUS THRU P2000-EXIT.
           PERFORM P3000-APPLY-RULE THRU P3000-EXIT.
           GOBACK.

       P1000-INIT.
      * ENTERED ONCE PER RECORD.  ONLY THE FIRST ENTRY DOES ANY
      * SET UP - EVERY LATER ENTRY FALLS STRAIGHT THROUGH.
           MOVE ZERO TO RETURN-CODE.
           IF WS-FIRST-ENTRY
               MOVE 'N' TO WS-FIRST-ENTRY-SW
               DISPLAY 'CABSXEDT ENTERED - FATAL EDIT REMOVAL'
               DISPLAY 'CABSXEDT FATAL RANGE 6 THROUGH 9'
           END-IF.

       P1000-EXIT.
           EXIT.

       P2000-PROFILE-STATUS.
      * COUNT THE RECORD AGAINST ITS STATUS DIGIT BEFORE ANY
      * DECISION IS TAKEN, SO THE PROFILE AT END OF INPUT COVERS
      * EVERY RECORD THAT ENTERED THE STEP AND NOT ONLY THE ONES
      * THAT SURVIVED IT.
           MOVE LK-ED-EDIT-STATUS TO WS-STATUS-TEST.
           IF WS-STATUS-TEST NOT NUMERIC
               ADD 1 TO WS-NON-DIGIT-CNT
               MOVE ZERO TO WS-STATUS-SUB
               GO TO P2000-EXIT
           END-IF.
           COMPUTE WS-STATUS-SUB = WS-STATUS-NUM + 1.
           ADD 1 TO WS-STATUS-CNT (WS-STATUS-SUB).

       P2000-EXIT.
           EXIT.

       P3000-APPLY-RULE.
      * STATUS 6 THROUGH 9 IS THE FATAL BAND.  CABING01 HAS
      * ALREADY WRITTEN THOSE RECORDS TO SUSPENSE AND SET THE
      * REJECT COUNT ON ITS OWN CONTROL RECORD.  LETTING THEM
      * THROUGH WOULD PUT THEM THROUGH THE OCN AND BAN EDITS A
      * SECOND TIME AND SUSPEND THEM AGAIN UNDER A SECOND ERROR
      * CODE, AND THE TWO SUSPENSE ENTRIES WOULD BOTH BE COUNTED
      * WHILE ONLY ONE RECORD WAS READ.
           IF WS-STATUS-FATAL
               PERFORM P3200-REMOVE THRU P3200-EXIT
               GO TO P3000-EXIT
           END-IF.
           PERFORM P3400-ADMIT THRU P3400-EXIT.

       P3000-EXIT.
           EXIT.

       P3200-REMOVE.
      * THE MINUTES ON A REMOVED RECORD ARE ACCUMULATED FOR THE
      * MESSAGE DATA SET ONLY.  THEY ARE NOT CARRIED INTO ANY
      * CONTROL RECORD, AND NO SUSPENSE RECORD IS CUT HERE
      * BECAUSE ONE ALREADY EXISTS FROM THE INTAKE STEP.
           ADD LK-ED-CONV-MIN TO WS-MIN-REMOVED.
           ADD 1 TO WS-DROP-CNT.
           MOVE 4 TO RETURN-CODE.

       P3200-EXIT.
           EXIT.

       P3400-ADMIT.
      * THE RECORD IS ADMITTED UNCHANGED.  A STATUS BYTE THAT IS
      * NOT A DIGIT REACHES THIS PARAGRAPH AS WELL - THE RULE
      * TESTS A RANGE, NOT A CLASS, AND ANYTHING OUTSIDE THE
      * RANGE IS ADMITTED.
           ADD LK-ED-CONV-MIN TO WS-MIN-KEPT.
           ADD 1 TO WS-KEPT-CNT.
           MOVE ZERO TO RETURN-CODE.

       P3400-EXIT.
           EXIT.

       P8000-END-OF-INPUT.
      * A NULL RECORD ADDRESS MEANS SORTIN IS EXHAUSTED.  THE EXIT
      * HAS NOTHING TO INSERT, SO IT REPLIES ZERO AND WRITES ITS
      * TALLIES TO THE MESSAGE DATA SET.  THESE COUNTS ARE THE
      * ONLY RECORD OF WHAT THE EXIT REMOVED - THEY ARE NOT
      * CARRIED INTO ANY CONTROL RECORD.
           MOVE 'Y' TO WS-EOF-SEEN-SW.
           MOVE ZERO TO WS-DROP-PCT.
           IF WS-ENTRY-CNT > ZERO
               COMPUTE WS-DROP-PCT ROUNDED =
                       (WS-DROP-CNT * 100) / WS-ENTRY-CNT
           END-IF.
           DISPLAY 'CABSXEDT ENTRIES     ' WS-ENTRY-CNT.
           DISPLAY 'CABSXEDT KEPT        ' WS-KEPT-CNT.
           DISPLAY 'CABSXEDT REMOVED     ' WS-DROP-CNT.
           DISPLAY 'CABSXEDT NON DIGIT   ' WS-NON-DIGIT-CNT.
           DISPLAY 'CABSXEDT MINUTES OUT ' WS-MIN-REMOVED.
           DISPLAY 'CABSXEDT MINUTES KEPT' WS-MIN-KEPT.
           DISPLAY 'CABSXEDT REMOVED PCT ' WS-DROP-PCT.
           PERFORM P8200-PROFILE-LINE THRU P8200-EXIT
                   VARYING WS-PX FROM 1 BY 1
                   UNTIL WS-PX > WS-STATUS-MAX.
           MOVE ZERO TO RETURN-CODE.

       P8000-EXIT.
           EXIT.

       P8200-PROFILE-LINE.
      * ONE LINE PER STATUS DIGIT.  SLOTS SEVEN THROUGH TEN ARE
      * THE FATAL BAND AND THEIR TOTAL EQUALS WS-DROP-CNT.
           COMPUTE WS-SHOW-DIGIT = WS-PX - 1.
           DISPLAY 'CABSXEDT STATUS      '
                   WS-SHOW-DIGIT ' '
                   WS-STATUS-CNT (WS-PX).

       P8200-EXIT.
           EXIT.
