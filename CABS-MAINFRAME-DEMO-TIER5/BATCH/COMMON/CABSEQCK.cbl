      *****************************************************************
      * CABSEQCK - ASCENDING SEQUENCE CHECK                           *
      * APPLICATION : CABS                                            *
      * INVOKED BY  : CALL FROM THE BATCH UTILITY FAMILY              *
      * INPUTS      : LK-SQ-KEY   X(08) KEY FROM A SORTED FILE        *
      * OUTPUTS     : LK-SQ-RC    9(04) RETURN CODE                   *
      * CONTROL     : NONE - SUBPROGRAMS DO NOT WRITE CTLOUT,         *
      *               CABS-STD-041                                    *
      * BALANCE     : NONE - NO RECORDS ARE READ OR WRITTEN HERE      *
      * RESTART     : NONE - THE STREAM TABLE IS REBUILT EACH RUN     *
      * REVISION HISTORY                                              *
      *   V1.00  1987-06-22  R.T.WHEELER   INITIAL RELEASE            *
      *   V1.01  1991-02-11  S.MBEKI       DUPLICATE KEY GIVEN ITS    *
      *                      OWN RETURN CODE                          *
      *   V1.02  1994-09-05  D.OKONKWO     EIGHT INDEPENDENT SEQUENCE *
      *                      STREAMS ADDED                            *
      *   V1.03  1999-03-17  A.BUKOWSKI    STREAM TAG TAKEN FROM THE  *
      *                      FIRST BYTE OF THE KEY                    *
      *   V1.04  2007-11-08  M.HAAS        FIRST THREE DESCENTS PER   *
      *                      STREAM CAPTURED FOR THE TALLY            *
      *   V1.05  2012-04-26  G.PETRAKIS    PREVIOUS BUT ONE KEY HELD  *
      *                      FOR THE OPERATIONS PRINT                 *
      *   V1.06  2018-10-02  E.KOWALCZYK   TALLY DISPLAY DRIVEN FROM  *
      *                      THE SENTINEL CALL                        *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABSEQCK.
       AUTHOR. TELCABS APPLICATIONS - COMMON SUBROUTINE GROUP.
      *****************************************************************
      * CONFIRMS THAT A KEY ARRIVING FROM A SORTED FILE REALLY IS     *
      * ASCENDING. THE WHOLE VALUE OF THE MODULE IS THE MEMORY IT     *
      * KEEPS BETWEEN CALLS.                                          *
      *                                                               *
      * THE PREVIOUS KEY, THE PREVIOUS BUT ONE KEY, THE PER STREAM    *
      * COUNTERS AND THE FIRST CALL SWITCH ALL LIVE IN WORKING-       *
      * STORAGE AND SURVIVE FROM ONE CALL TO THE NEXT BECAUSE THE     *
      * MODULE STAYS LOADED FOR THE LIFE OF THE RUN UNIT. THERE IS    *
      * NOTHING ELSE HOLDING THE COMPARISON TOGETHER.                 *
      *                                                               *
      * THE INTERFACE IS EIGHT BYTES. CALLERS THAT STAGE A LONGER     *
      * FIELD PRESENT ITS LEADING EIGHT BYTES.                        *
      *                                                               *
      * SEVERAL UTILITY PROGRAMS CHECK TWO OR THREE FILES IN ONE      *
      * PASS, SO EIGHT INDEPENDENT STREAMS ARE HELD AND SELECTED BY   *
      * THE FIRST BYTE OF THE KEY. A FIRST BYTE NOT YET SEEN TAKES    *
      * THE NEXT FREE SLOT. ONCE ALL EIGHT SLOTS ARE IN USE, THE      *
      * NINTH AND EVERY LATER FIRST BYTE VALUE SHARES SLOT 8 WITH     *
      * WHATEVER IS ALREADY THERE. THAT IS HOW THE MODULE BEHAVES.    *
      *                                                               *
      * THE CALLING PROGRAM'S BALANCING EQUATION IS UNAFFECTED BY     *
      * ANYTHING THIS MODULE DOES.                                    *
      *                                                               *
      * RETURN CODES                                                  *
      *   0000  ASCENDING - HIGHER THAN THE PREVIOUS KEY              *
      *   0004  EQUAL TO THE PREVIOUS KEY                             *
      *   0008  LOWER THAN THE PREVIOUS KEY                           *
      *   0012  FIRST KEY ON THIS STREAM, NOTHING TO COMPARE          *
      *   0016  A NEW STREAM WAS ALLOCATED                            *
      *   0020  THE STREAM TABLE WAS FULL, KEY FOLDED INTO STREAM 8   *
      *   0024  TALLIES DISPLAYED                                     *
      * 0016 AND 0020 ARE SET AFTER THE COMPARISON AND OVERRIDE THE   *
      * COMPARISON CODE. THE PER STREAM COUNTERS ARE STEPPED EITHER   *
      * WAY.                                                          *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-SQ-CONSTANTS.
           05  WS-SQ-PGM-NAME          PIC X(08) VALUE 'CABSEQCK'.
           05  WS-SQ-VERSION           PIC X(05) VALUE 'V1.06'.
           05  WS-SQ-SENTINEL          PIC X(08) VALUE '*END    '.
           05  WS-SQ-MAX-STREAM        PIC S9(04) COMP-3 VALUE 8.
           05  WS-SQ-MAX-CAP           PIC S9(04) COMP-3 VALUE 3.
      * THE INCOMING KEY AND ITS FIRST BYTE. THE FIRST BYTE IS TAKEN
      * THROUGH A REDEFINES SO NO PART OF THE KEY IS ADDRESSED BY
      * POSITION.
       01  WS-SQ-KEY-AREA.
           05  WS-SQ-KEY               PIC X(08).
       01  WS-SQ-KEY-R REDEFINES WS-SQ-KEY-AREA.
           05  WS-SQ-KEY-B1            PIC X(01).
           05  WS-SQ-KEY-REST          PIC X(07).
      * THE STREAM TABLE. NO VALUE CLAUSES ARE CARRIED UNDER THE
      * OCCURS, SO THE TABLE IS CLEARED ON THE FIRST CALL OF THE RUN
      * UNIT AND THEN LEFT TO ACCUMULATE.
       01  WS-SQ-STREAM-TABLE.
           05  WS-SQ-STREAM-CNT        PIC S9(04) COMP-3 VALUE 0.
           05  WS-SQ-STREAM OCCURS 8 TIMES.
               10  WS-SQ-ST-TAG        PIC X(01).
               10  WS-SQ-ST-USED       PIC X(01).
               10  WS-SQ-ST-FIRST      PIC X(01).
               10  WS-SQ-ST-PREV       PIC X(08).
               10  WS-SQ-ST-PREV2      PIC X(08).
               10  WS-SQ-ST-SEEN       PIC S9(09) COMP-3.
               10  WS-SQ-ST-DUP        PIC S9(09) COMP-3.
               10  WS-SQ-ST-DESC       PIC S9(09) COMP-3.
               10  WS-SQ-ST-BIG-HI     PIC X(08).
               10  WS-SQ-ST-BIG-LO     PIC X(08).
               10  WS-SQ-ST-CAP-CNT    PIC S9(04) COMP-3.
               10  WS-SQ-ST-CAP OCCURS 3 TIMES.
                   15  WS-SQ-CAP-PREV  PIC X(08).
                   15  WS-SQ-CAP-CURR  PIC X(08).
       01  WS-SQ-COUNT-AREA.
           05  WS-SQ-CALL-CNT          PIC S9(09) COMP-3 VALUE 0.
           05  WS-SQ-ASC-CNT           PIC S9(09) COMP-3 VALUE 0.
           05  WS-SQ-DUP-CNT           PIC S9(09) COMP-3 VALUE 0.
           05  WS-SQ-DESC-CNT          PIC S9(09) COMP-3 VALUE 0.
           05  WS-SQ-FIRST-CNT         PIC S9(09) COMP-3 VALUE 0.
           05  WS-SQ-NEW-CNT           PIC S9(09) COMP-3 VALUE 0.
           05  WS-SQ-FOLD-CNT          PIC S9(09) COMP-3 VALUE 0.
           05  WS-SQ-TALLY-CNT         PIC S9(09) COMP-3 VALUE 0.
       01  WS-SQ-SWITCH-AREA.
           05  WS-SQ-INIT-SW           PIC X(01) VALUE 'N'.
           05  WS-SQ-SW-FOUND          PIC X(01) VALUE 'N'.
           05  WS-SQ-SW-NEW            PIC X(01) VALUE 'N'.
           05  WS-SQ-SW-FOLD           PIC X(01) VALUE 'N'.
       01  WS-SQ-WORK-AREA.
           05  WS-SQ-SLOT              PIC S9(04) COMP-3 VALUE 0.
           05  WS-SQ-SUB               PIC S9(04) COMP-3 VALUE 0.
           05  WS-SQ-SUB2              PIC S9(04) COMP-3 VALUE 0.
           05  WS-SQ-SUB3              PIC S9(04) COMP-3 VALUE 0.
           05  WS-SQ-SLOT-ED           PIC Z9.
           05  WS-SQ-CNT-ED            PIC ZZZ,ZZZ,ZZ9.
       LINKAGE SECTION.
       01  LK-SQ-KEY                   PIC X(08).
       01  LK-SQ-RC                    PIC 9(04).
       PROCEDURE DIVISION USING LK-SQ-KEY LK-SQ-RC.
       P0000-SEQUENCE-CHECK.
           ADD 1 TO WS-SQ-CALL-CNT.
           MOVE 0 TO LK-SQ-RC.
           IF WS-SQ-INIT-SW = 'N'
               PERFORM P1000-INIT-TABLE THRU P1000-EXIT.
           IF LK-SQ-KEY = WS-SQ-SENTINEL
               PERFORM P7000-TALLY THRU P7000-EXIT
               MOVE 24 TO LK-SQ-RC
               GO TO P0000-EXIT.
           MOVE 'N' TO WS-SQ-SW-NEW.
           MOVE 'N' TO WS-SQ-SW-FOLD.
           MOVE LK-SQ-KEY TO WS-SQ-KEY.
           PERFORM P2000-FIND-STREAM THRU P2000-EXIT.
           PERFORM P3000-COMPARE-KEY THRU P3000-EXIT.
           PERFORM P4000-OVERRIDE-RC THRU P4000-EXIT.
       P0000-EXIT.
           GOBACK.
      * S100-INITIALISATION SECTION
       S100-INITIALISATION SECTION.
      * RUN ONCE PER RUN UNIT. AFTER THIS THE TABLE IS THE MODULE'S
      * MEMORY AND IS NEVER CLEARED AGAIN.
       P1000-INIT-TABLE.
           MOVE 'Y' TO WS-SQ-INIT-SW.
           MOVE 0 TO WS-SQ-STREAM-CNT.
           PERFORM P1100-CLEAR-STREAM THRU P1100-EXIT
               VARYING WS-SQ-SUB FROM 1 BY 1
               UNTIL WS-SQ-SUB > WS-SQ-MAX-STREAM.
       P1000-EXIT.
           EXIT.
       P1100-CLEAR-STREAM.
           MOVE SPACES TO WS-SQ-ST-TAG (WS-SQ-SUB).
           MOVE 'N' TO WS-SQ-ST-USED (WS-SQ-SUB).
           MOVE 'Y' TO WS-SQ-ST-FIRST (WS-SQ-SUB).
           MOVE SPACES TO WS-SQ-ST-PREV (WS-SQ-SUB).
           MOVE SPACES TO WS-SQ-ST-PREV2 (WS-SQ-SUB).
           MOVE 0 TO WS-SQ-ST-SEEN (WS-SQ-SUB).
           MOVE 0 TO WS-SQ-ST-DUP (WS-SQ-SUB).
           MOVE 0 TO WS-SQ-ST-DESC (WS-SQ-SUB).
           MOVE SPACES TO WS-SQ-ST-BIG-HI (WS-SQ-SUB).
           MOVE SPACES TO WS-SQ-ST-BIG-LO (WS-SQ-SUB).
           MOVE 0 TO WS-SQ-ST-CAP-CNT (WS-SQ-SUB).
           PERFORM P1200-CLEAR-CAPTURE THRU P1200-EXIT
               VARYING WS-SQ-SUB2 FROM 1 BY 1
               UNTIL WS-SQ-SUB2 > WS-SQ-MAX-CAP.
       P1100-EXIT.
           EXIT.
       P1200-CLEAR-CAPTURE.
           MOVE SPACES TO WS-SQ-CAP-PREV (WS-SQ-SUB, WS-SQ-SUB2).
           MOVE SPACES TO WS-SQ-CAP-CURR (WS-SQ-SUB, WS-SQ-SUB2).
       P1200-EXIT.
           EXIT.
      * S200-STREAM-SELECTION SECTION
       S200-STREAM-SELECTION SECTION.
      * THE TAGS ARE NOT IN ORDER, SO THE TABLE IS WALKED SERIALLY
      * FROM SLOT ONE TO THE LAST SLOT ALLOCATED.
       P2000-FIND-STREAM.
           MOVE 0 TO WS-SQ-SLOT.
           MOVE 'N' TO WS-SQ-SW-FOUND.
           PERFORM P2100-MATCH-TAG THRU P2100-EXIT
               VARYING WS-SQ-SUB FROM 1 BY 1
               UNTIL WS-SQ-SUB > WS-SQ-STREAM-CNT
                  OR WS-SQ-SW-FOUND = 'Y'.
           IF WS-SQ-SW-FOUND = 'N'
               PERFORM P2200-ALLOCATE-STREAM THRU P2200-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-MATCH-TAG.
           IF WS-SQ-ST-TAG (WS-SQ-SUB) = WS-SQ-KEY-B1
               MOVE WS-SQ-SUB TO WS-SQ-SLOT
               MOVE 'Y' TO WS-SQ-SW-FOUND.
       P2100-EXIT.
           EXIT.
      * WITH ALL EIGHT SLOTS IN USE THE KEY IS FOLDED INTO STREAM 8
      * AND COMPARED AGAINST WHATEVER STREAM 8 IS ALREADY HOLDING.
       P2200-ALLOCATE-STREAM.
           IF WS-SQ-STREAM-CNT < WS-SQ-MAX-STREAM
               ADD 1 TO WS-SQ-STREAM-CNT
               MOVE WS-SQ-STREAM-CNT TO WS-SQ-SLOT
               MOVE WS-SQ-KEY-B1 TO WS-SQ-ST-TAG (WS-SQ-SLOT)
               MOVE 'Y' TO WS-SQ-ST-USED (WS-SQ-SLOT)
               MOVE 'Y' TO WS-SQ-SW-NEW
               ADD 1 TO WS-SQ-NEW-CNT
           ELSE
               MOVE WS-SQ-MAX-STREAM TO WS-SQ-SLOT
               MOVE 'Y' TO WS-SQ-SW-FOLD
               ADD 1 TO WS-SQ-FOLD-CNT.
       P2200-EXIT.
           EXIT.
      * S300-COMPARISON SECTION
       S300-COMPARISON SECTION.
       P3000-COMPARE-KEY.
           ADD 1 TO WS-SQ-ST-SEEN (WS-SQ-SLOT).
           IF WS-SQ-ST-FIRST (WS-SQ-SLOT) = 'Y'
               MOVE 'N' TO WS-SQ-ST-FIRST (WS-SQ-SLOT)
               MOVE 12 TO LK-SQ-RC
               ADD 1 TO WS-SQ-FIRST-CNT
           ELSE
           IF WS-SQ-KEY > WS-SQ-ST-PREV (WS-SQ-SLOT)
               MOVE 0 TO LK-SQ-RC
               ADD 1 TO WS-SQ-ASC-CNT
           ELSE
           IF WS-SQ-KEY = WS-SQ-ST-PREV (WS-SQ-SLOT)
               MOVE 4 TO LK-SQ-RC
               ADD 1 TO WS-SQ-DUP-CNT
               ADD 1 TO WS-SQ-ST-DUP (WS-SQ-SLOT)
           ELSE
               MOVE 8 TO LK-SQ-RC
               ADD 1 TO WS-SQ-DESC-CNT
               ADD 1 TO WS-SQ-ST-DESC (WS-SQ-SLOT)
               PERFORM P3100-CAPTURE-DESCENT THRU P3100-EXIT.
           MOVE WS-SQ-ST-PREV (WS-SQ-SLOT) TO
               WS-SQ-ST-PREV2 (WS-SQ-SLOT).
           MOVE WS-SQ-KEY TO WS-SQ-ST-PREV (WS-SQ-SLOT).
       P3000-EXIT.
           EXIT.
      * THE FIRST THREE DESCENTS ON EACH STREAM ARE HELD AS THE PAIR
      * OF KEYS INVOLVED. THE LARGEST DESCENT IS THE PAIR WHOSE
      * INCOMING KEY SORTED LOWEST ON A PLAIN CHARACTER COMPARISON.
       P3100-CAPTURE-DESCENT.
           IF WS-SQ-ST-CAP-CNT (WS-SQ-SLOT) < WS-SQ-MAX-CAP
               ADD 1 TO WS-SQ-ST-CAP-CNT (WS-SQ-SLOT)
               MOVE WS-SQ-ST-CAP-CNT (WS-SQ-SLOT) TO WS-SQ-SUB2
               MOVE WS-SQ-ST-PREV (WS-SQ-SLOT) TO
                   WS-SQ-CAP-PREV (WS-SQ-SLOT, WS-SQ-SUB2)
               MOVE WS-SQ-KEY TO
                   WS-SQ-CAP-CURR (WS-SQ-SLOT, WS-SQ-SUB2).
           IF WS-SQ-ST-BIG-LO (WS-SQ-SLOT) = SPACES
                   OR WS-SQ-KEY < WS-SQ-ST-BIG-LO (WS-SQ-SLOT)
               MOVE WS-SQ-ST-PREV (WS-SQ-SLOT) TO
                   WS-SQ-ST-BIG-HI (WS-SQ-SLOT)
               MOVE WS-SQ-KEY TO WS-SQ-ST-BIG-LO (WS-SQ-SLOT).
       P3100-EXIT.
           EXIT.
      * S400-RETURN SECTION
       S400-RETURN SECTION.
       P4000-OVERRIDE-RC.
           IF WS-SQ-SW-FOLD = 'Y'
               MOVE 20 TO LK-SQ-RC
           ELSE
           IF WS-SQ-SW-NEW = 'Y'
               MOVE 16 TO LK-SQ-RC.
       P4000-EXIT.
           EXIT.
      * S700-TALLY SECTION
       S700-TALLY SECTION.
      * EVERYTHING DISPLAYED HERE WAS ACCUMULATED ACROSS THE WHOLE
      * STEP IN WORKING-STORAGE.
       P7000-TALLY.
           ADD 1 TO WS-SQ-TALLY-CNT.
           MOVE WS-SQ-CALL-CNT TO WS-SQ-CNT-ED.
           DISPLAY 'CABSEQCK ' WS-SQ-VERSION ' - SEQUENCE TALLY'.
           DISPLAY '  CALLS          = ' WS-SQ-CNT-ED.
           DISPLAY '  ASCENDING      = ' WS-SQ-ASC-CNT.
           DISPLAY '  DUPLICATES     = ' WS-SQ-DUP-CNT.
           DISPLAY '  DESCENTS       = ' WS-SQ-DESC-CNT.
           DISPLAY '  FIRST ON STRM  = ' WS-SQ-FIRST-CNT.
           DISPLAY '  STREAMS OPENED = ' WS-SQ-NEW-CNT.
           DISPLAY '  FOLDED TO 8    = ' WS-SQ-FOLD-CNT.
           DISPLAY '  STREAMS IN USE = ' WS-SQ-STREAM-CNT.
           PERFORM P7100-TALLY-STREAM THRU P7100-EXIT
               VARYING WS-SQ-SUB FROM 1 BY 1
               UNTIL WS-SQ-SUB > WS-SQ-STREAM-CNT.
       P7000-EXIT.
           EXIT.
       P7100-TALLY-STREAM.
           MOVE WS-SQ-SUB TO WS-SQ-SLOT-ED.
           DISPLAY '  STREAM ' WS-SQ-SLOT-ED
               ' TAG ' WS-SQ-ST-TAG (WS-SQ-SUB).
           DISPLAY '    KEYS SEEN  = ' WS-SQ-ST-SEEN (WS-SQ-SUB).
           DISPLAY '    DUPLICATES = ' WS-SQ-ST-DUP (WS-SQ-SUB).
           DISPLAY '    DESCENTS   = ' WS-SQ-ST-DESC (WS-SQ-SUB).
           DISPLAY '    LAST KEY   = ' WS-SQ-ST-PREV (WS-SQ-SUB).
           DISPLAY '    PRIOR KEY  = ' WS-SQ-ST-PREV2 (WS-SQ-SUB).
           IF WS-SQ-ST-DESC (WS-SQ-SUB) > 0
               DISPLAY '    LARGEST    = '
                   WS-SQ-ST-BIG-HI (WS-SQ-SUB)
                   ' THEN ' WS-SQ-ST-BIG-LO (WS-SQ-SUB)
               PERFORM P7200-TALLY-CAPTURE THRU P7200-EXIT
                   VARYING WS-SQ-SUB2 FROM 1 BY 1
                   UNTIL WS-SQ-SUB2 > WS-SQ-ST-CAP-CNT (WS-SQ-SUB).
       P7100-EXIT.
           EXIT.
       P7200-TALLY-CAPTURE.
           DISPLAY '    DESCENT    = '
               WS-SQ-CAP-PREV (WS-SQ-SUB, WS-SQ-SUB2)
               ' THEN ' WS-SQ-CAP-CURR (WS-SQ-SUB, WS-SQ-SUB2).
       P7200-EXIT.
           EXIT.
