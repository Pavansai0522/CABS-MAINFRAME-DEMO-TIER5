       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABSE15A.
      *****************************************************************
      * CABSE15A - SORT E15 INPUT EXIT - RATING PRESORT REFORMAT      *
      * APPLICATION : CABS                                            *
      * COMPILER    : ENTERPRISE COBOL                                *
      * INVOKED BY  : OS SORT/MERGE, MODS=(E15=(CABSE15A,4096))       *
      *               ON CONTROL CARD MEMBER JCL/CTLCARDS/CABSRT04    *
      * INPUTS      : ONE 200 BYTE SORTIN RECORD PER ENTRY, CABSCDR   *
      * OUTPUTS     : ONE 200 BYTE RECORD RETURNED TO SORT            *
      * CONTROL     : NONE - EXITS DO NOT WRITE CTLOUT, CABS-STD-041  *
      * BALANCE     : SORTIN = RECORDS RETURNED + WS-DROP-TOTAL       *
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
      * REVISION HISTORY                                              *
      *   V1.00  1991-05-14  R.T.WHEELER   INITIAL - ASSEMBLER F      *
      *   V1.03  1994-02-28  D.OKONKWO     TRUNK GROUP MOVED LEFT     *
      *   V2.00  2004-08-02  P.NAIR        RECODED IN COBOL FOR LE    *
      *   V2.01  2007-11-19  A.BUKOWSKI    ADDED CIC CARRY FORWARD    *
      *   V2.04  2013-06-05  L.FERREIRA    ELEMENT CODE EDIT ADDED    *
      *   V2.05  2018-09-27  M.HAAS        RECOMPILE ONLY - LE V6.2   *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
      * COUNTERS SURVIVE FROM ONE ENTRY TO THE NEXT.  THE SORT STEP
      * LOADS THIS MODULE ONCE AND HOLDS IT FOR THE WHOLE PASS.
      *
       01  WS-MODULE-IDENT.
           05  FILLER                  PIC X(08) VALUE 'CABSE15A'.
           05  FILLER                  PIC X(08) VALUE ' V2.05  '.
       01  WS-COUNTERS.
           05  WS-ENTRY-CNT            PIC S9(11) COMP-3 VALUE 0.
           05  WS-RETURN-CNT           PIC S9(11) COMP-3 VALUE 0.
           05  WS-DROP-TOTAL           PIC S9(11) COMP-3 VALUE 0.
           05  WS-DROP-NO-ELEM         PIC S9(11) COMP-3 VALUE 0.
           05  WS-DROP-BAD-TYPE        PIC S9(11) COMP-3 VALUE 0.
           05  WS-CIC-CARRIED          PIC S9(11) COMP-3 VALUE 0.
       01  WS-SWITCHES.
           05  WS-FIRST-ENTRY-SW       PIC X(01) VALUE 'Y'.
               88  WS-FIRST-ENTRY      VALUE 'Y'.
           05  WS-EOF-SEEN-SW          PIC X(01) VALUE 'N'.
               88  WS-EOF-SEEN         VALUE 'Y'.
      *
      * THE RATING WORK LAYOUT.  THE FIRST FORTY EIGHT BYTES ARE
      * REARRANGED SO THE SORT KEY IS CONTIGUOUS - THE CONTROL CARD
      * SORTS ON 5,4 40,1 AND 50,3 AND THOSE POSITIONS ONLY LINE UP
      * AFTER THIS EXIT HAS RUN.
      *
       01  WS-RATING-WORK.
           05  WS-RW-REC-TYPE          PIC X(02).
           05  WS-RW-USAGE-TYPE        PIC X(01).
           05  WS-RW-FILLER-1          PIC X(01).
           05  WS-RW-OCN               PIC X(04).
           05  WS-RW-BAN               PIC X(13).
           05  WS-RW-SEQ               PIC 9(09) COMP-3.
           05  WS-RW-FILLER-2          PIC X(09).
           05  WS-RW-JURIS-CD          PIC X(01).
           05  WS-RW-STATE-CD          PIC X(02).
           05  WS-RW-FILLER-3          PIC X(07).
           05  WS-RW-RATE-ELEM         PIC X(06).
           05  WS-RW-CONN-YYDDD        PIC 9(05).
           05  WS-RW-CONN-HHMMSS       PIC 9(06).
           05  WS-RW-DISC-YYDDD        PIC 9(05).
           05  WS-RW-CIC               PIC 9(04).
           05  WS-RW-TRUNK-GRP         PIC X(08).
           05  WS-RW-CONV-MIN          PIC S9(07)V9(02) COMP-3.
           05  WS-RW-CHG-MIN           PIC S9(07)V9(02) COMP-3.
           05  WS-RW-VARIANT           PIC X(96).
           05  WS-RW-SRC-SYSTEM        PIC X(04).
           05  WS-RW-LOAD-YYDDD        PIC 9(05).
           05  WS-RW-EDIT-STATUS       PIC X(01).
           05  WS-RW-FILLER-9          PIC X(06).
       01  WS-ELEM-TEST                PIC X(06).
       01  WS-BLANK-TALLY              PIC S9(04) COMP VALUE 0.
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
           05  LK-CD-CONN-HHMMSS       PIC 9(06).
           05  LK-CD-DISC-YYDDD        PIC 9(05).
           05  LK-CD-DISC-HHMMSS       PIC 9(06).
           05  LK-CD-VARIANT           PIC X(96).
           05  LK-CD-VC-DETAIL REDEFINES LK-CD-VARIANT.
               10  LK-VC-ORIG-NPANXX   PIC 9(06).
               10  LK-VC-TERM-NPANXX   PIC 9(06).
               10  LK-VC-ORIG-LATA     PIC 9(03).
               10  LK-VC-TERM-LATA     PIC 9(03).
               10  LK-VC-CONV-MIN      PIC S9(07)V9(02) COMP-3.
               10  LK-VC-CHG-MIN       PIC S9(07)V9(02) COMP-3.
               10  LK-VC-TANDEM-IND    PIC X(01).
               10  LK-VC-TRUNK-GRP     PIC X(08).
               10  LK-VC-CIC           PIC 9(04).
               10  LK-VC-END-OFFICE    PIC X(11).
               10  LK-VC-FILLER        PIC X(43).
           05  LK-CD-SRC-SYSTEM        PIC X(04).
           05  LK-CD-LOAD-YYDDD        PIC 9(05).
           05  LK-CD-EDIT-STATUS       PIC X(01).
           05  LK-CD-TAIL              PIC X(33).
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
           PERFORM P2000-SCREEN-RECORD THRU P2000-EXIT.
           IF RETURN-CODE = 4
               GOBACK
           END-IF.
           PERFORM P3000-REFORMAT THRU P3000-EXIT.
           PERFORM P4000-RETURN-RECORD THRU P4000-EXIT.
           GOBACK.

       P1000-INIT.
      * ENTERED ONCE PER RECORD.  ONLY THE FIRST ENTRY DOES ANY
      * SET UP - EVERY LATER ENTRY FALLS STRAIGHT THROUGH.
           MOVE ZERO TO RETURN-CODE.
           IF WS-FIRST-ENTRY
               MOVE 'N' TO WS-FIRST-ENTRY-SW
               DISPLAY 'CABSE15A ENTERED - RATING PRESORT REFORMAT'
           END-IF.

       P1000-EXIT.
           EXIT.

       P2000-SCREEN-RECORD.
      * THE INTAKE IS SUPPOSED TO RESOLVE A RATE ELEMENT CODE ON
      * EVERY RECORD BEFORE THE CLEAN FILE IS CUT.  A HANDFUL EACH
      * CYCLE ARRIVE WITHOUT ONE BECAUSE THE ELEMENT TABLE HAS NO
      * ROW FOR THE TRUNK GROUP.  THOSE CANNOT BE PRICED AND ARE
      * TAKEN OUT HERE SO THE RATING DISPATCHER IS NOT LEFT
      * SEARCHING THE RATE TABLE FOR A KEY OF SPACES.
           MOVE LK-CD-RATE-ELEM TO WS-ELEM-TEST.
           MOVE ZERO TO WS-BLANK-TALLY.
           INSPECT WS-ELEM-TEST TALLYING WS-BLANK-TALLY
                   FOR ALL SPACE.
           IF WS-BLANK-TALLY = 6
               ADD 1 TO WS-DROP-NO-ELEM
               ADD 1 TO WS-DROP-TOTAL
               MOVE 4 TO RETURN-CODE
               GO TO P2000-EXIT
           END-IF.
           IF WS-ELEM-TEST = ALL '0'
               ADD 1 TO WS-DROP-NO-ELEM
               ADD 1 TO WS-DROP-TOTAL
               MOVE 4 TO RETURN-CODE
               GO TO P2000-EXIT
           END-IF.
      * RECORD TYPES OUTSIDE 01 THROUGH 08 ARE NOT PRICED BY ANY
      * CURRENT RATING MODULE.  THE INCLUDE ON CABSRT01 REMOVES
      * MOST OF THEM BUT THAT CARD IS NOT USED ON EVERY PATH INTO
      * THIS SORT, SO THE TEST IS REPEATED HERE.
           IF LK-CD-REC-TYPE < '01' OR LK-CD-REC-TYPE > '08'
               ADD 1 TO WS-DROP-BAD-TYPE
               ADD 1 TO WS-DROP-TOTAL
               MOVE 4 TO RETURN-CODE
           END-IF.

       P2000-EXIT.
           EXIT.

       P3000-REFORMAT.
      * BUILD THE RATING WORK LAYOUT.  THE SORT KEY POSITIONS ON
      * CABSRT04 ARE 5,4 (OCN), 40,1 (JURISDICTION) AND 50,3 (THE
      * FIRST THREE BYTES OF THE ELEMENT CODE).  NONE OF THOSE
      * POSITIONS HOLD THE INTENDED FIELD IN THE INBOUND LAYOUT.
           MOVE SPACES TO WS-RATING-WORK.
           MOVE LK-CD-REC-TYPE     TO WS-RW-REC-TYPE.
           MOVE LK-CD-USAGE-TYPE   TO WS-RW-USAGE-TYPE.
           MOVE LK-CD-OCN          TO WS-RW-OCN.
           MOVE LK-CD-BAN          TO WS-RW-BAN.
           MOVE LK-CD-SEQ-NBR      TO WS-RW-SEQ.
           MOVE LK-CD-JURIS-CD     TO WS-RW-JURIS-CD.
           MOVE LK-CD-STATE-CD     TO WS-RW-STATE-CD.
           MOVE LK-CD-RATE-ELEM    TO WS-RW-RATE-ELEM.
           MOVE LK-CD-CONN-YYDDD   TO WS-RW-CONN-YYDDD.
           MOVE LK-CD-CONN-HHMMSS  TO WS-RW-CONN-HHMMSS.
           MOVE LK-CD-DISC-YYDDD   TO WS-RW-DISC-YYDDD.
           MOVE LK-CD-VARIANT      TO WS-RW-VARIANT.
           MOVE LK-CD-SRC-SYSTEM   TO WS-RW-SRC-SYSTEM.
           MOVE LK-CD-LOAD-YYDDD   TO WS-RW-LOAD-YYDDD.
           MOVE LK-CD-EDIT-STATUS  TO WS-RW-EDIT-STATUS.
           PERFORM P3200-CARRY-VOICE THRU P3200-EXIT.

       P3000-EXIT.
           EXIT.

       P3200-CARRY-VOICE.
      * ONLY THE VOICE VARIANT CARRIES MINUTES, A TRUNK GROUP AND A
      * CIC IN FIXED POSITIONS.  FOR THE OTHER TWO VARIANTS THOSE
      * FIELDS ARE LEFT AT THEIR INITIAL VALUES AND THE RATING
      * MODULE PICKS THEM OUT OF THE VARIANT AREA ITSELF.
           MOVE ZERO TO WS-RW-CONV-MIN.
           MOVE ZERO TO WS-RW-CHG-MIN.
           MOVE ZERO TO WS-RW-CIC.
           MOVE SPACES TO WS-RW-TRUNK-GRP.
           IF LK-CD-REC-TYPE = '01' OR '02' OR '03'
               MOVE LK-VC-CONV-MIN   TO WS-RW-CONV-MIN
               MOVE LK-VC-CHG-MIN    TO WS-RW-CHG-MIN
               MOVE LK-VC-TRUNK-GRP  TO WS-RW-TRUNK-GRP
               MOVE LK-VC-CIC        TO WS-RW-CIC
               ADD 1 TO WS-CIC-CARRIED
           END-IF.

       P3200-EXIT.
           EXIT.

       P4000-RETURN-RECORD.
      * OVERLAY THE INPUT AREA IN PLACE AND TELL SORT TO TAKE THE
      * ALTERED RECORD.  THE LENGTH IS UNCHANGED SO WORD TWO OF THE
      * PARAMETER LIST IS LEFT ALONE.
           MOVE WS-RATING-WORK TO LK-SORT-RECORD.
           ADD 1 TO WS-RETURN-CNT.
           MOVE 8 TO RETURN-CODE.

       P4000-EXIT.
           EXIT.

       P8000-END-OF-INPUT.
      * A NULL RECORD ADDRESS MEANS SORTIN IS EXHAUSTED.  THE EXIT
      * HAS NOTHING TO INSERT, SO IT REPLIES ZERO AND WRITES ITS
      * TALLIES TO THE MESSAGE DATA SET.  THESE COUNTS ARE THE ONLY
      * RECORD OF WHAT THE EXIT REMOVED - THEY ARE NOT CARRIED INTO
      * ANY CONTROL RECORD.
           MOVE 'Y' TO WS-EOF-SEEN-SW.
           DISPLAY 'CABSE15A ENTRIES     ' WS-ENTRY-CNT.
           DISPLAY 'CABSE15A RETURNED    ' WS-RETURN-CNT.
           DISPLAY 'CABSE15A NO ELEMENT  ' WS-DROP-NO-ELEM.
           DISPLAY 'CABSE15A BAD TYPE    ' WS-DROP-BAD-TYPE.
           DISPLAY 'CABSE15A DROPPED     ' WS-DROP-TOTAL.
           MOVE ZERO TO RETURN-CODE.

       P8000-EXIT.
           EXIT.
