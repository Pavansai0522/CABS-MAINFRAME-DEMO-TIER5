       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABSXLST.
      *****************************************************************
      * CABSXLST - SORT E35 OUTPUT EXIT - KEEP THE LAST CONTROL       *
      *            RECORD PER PROCESS AND STEP                        *
      * APPLICATION : CABS                                            *
      * COMPILER    : ENTERPRISE COBOL                                *
      * INVOKED BY  : OS SORT/MERGE, MODS E35=(CABSXLST,2048,         *
      *               SORTEXIT,N) ON CONTROL CARD MEMBER              *
      *               JCL/CTLCARDS/CABSRT15                           *
      * INPUTS      : ONE 180 BYTE CABS-CONTROL-RECORD IMAGE PER      *
      *               ENTRY, LEAVING THE FINAL MERGE                  *
      * OUTPUTS     : ONE RECORD PER PROCESS AND STEP - THE LATEST    *
      *               ATTEMPT.  THE EARLIER ATTEMPTS ARE DELETED.     *
      * CONTROL     : NONE - EXITS DO NOT WRITE CTLOUT, CABS-STD-041  *
      * BALANCE     : RECORDS IN = RELEASED + SUPERSEDED              *
      * RESTART     : NOT RESTARTABLE - RERUN THE WHOLE SORT STEP     *
      *                                                               *
      * LINKAGE CONVENTION                                            *
      *   REGISTER 1 ADDRESSES A THREE WORD PARAMETER LIST.  WORD     *
      *   ONE IS THE ADDRESS OF THE RECORD LEAVING THE FINAL MERGE    *
      *   OR BINARY ZERO AT END OF MERGE.  WORD TWO ADDRESSES THE     *
      *   RECORD MOST RECENTLY WRITTEN TO SORTOUT.  WORD THREE        *
      *   ADDRESSES THE LENGTH HALFWORD AND IS NOT REFERENCED ON A    *
      *   FIXED FILE.  RETURN-CODE - 00 TAKE THE NEXT RECORD, 04      *
      *   DELETE, 08 WRITE THE RECORD ADDRESSED BY WORD ONE AND       *
      *   RE-ENTER WITH THE SAME INPUT, 12 NO FURTHER ENTRY, 16       *
      *   TERMINATE.  WORKING STORAGE HOLDS THE SURVIVOR BETWEEN      *
      *   ENTRIES FOR THE LIFE OF THE MERGE.                          *
      *                                                               *
      * THE SORT KEY                                                  *
      *   CABSRT15 SORTS ON 13,8 AND 21,3.  GIVEN CT-RUN-ID AS THE    *
      *   FIRST TWELVE BYTES OF CABSCTL, 13,8 IS CT-PROCESS-ID AND    *
      *   21,3 IS CT-STEP-SEQ.  THE SAME PROCESS AND STEP APPEAR      *
      *   MANY TIMES - THE FILE IS WRITTEN WITH DISP MOD.             *
      *                                                               *
      * WHICH OF TWO EQUAL KEYED RECORDS IS THE LATER ONE             *
      *   THIS MODULE DOES NOT RELY ON THE ORDER IN WHICH TWO         *
      *   RECORDS WITH THE SAME KEY LEAVE THE MERGE.  IT COMPARES     *
      *   CT-RERUN-NBR, WHICH THE RESTART PROCEDURE STEPS EVERY TIME  *
      *   A PROCESS IS RESUBMITTED, AND TAKES THE HIGHER ONE.  WHEN   *
      *   THOSE ARE EQUAL IT COMPARES CT-CYCLE-YYDDD ON A PIVOT YEAR  *
      *   OF 70 AND TAKES THE LATER CYCLE.  ONLY WHEN BOTH AGREE      *
      *   DOES IT FALL BACK ON ARRIVAL ORDER AND KEEP THE ONE THAT    *
      *   CAME SECOND.  THAT RULE IS THE REASON THIS MODULE EXISTS -  *
      *   IT IS HELD HERE AND NOT ON THE CONTROL CARD.                *
      *                                                               *
      * REVISION HISTORY                                              *
      *   V1.00  1987-11-16  R.T.WHEELER   INITIAL - ASSEMBLER F      *
      *   V1.03  1993-01-28  D.OKONKWO     RERUN NUMBER COMPARED      *
      *   V1.06  1999-10-12  J.M.CASTILLO  CYCLE DATE TIE BREAK       *
      *   V2.00  2005-06-13  P.NAIR        RECODED IN COBOL FOR LE    *
      *   V2.02  2011-03-30  G.PETRAKIS    PIVOT 70 ON THE CYCLE      *
      *   V2.04  2016-08-19  T.YAMASHITA   ARRIVAL TIE COUNTED        *
      *   V2.05  2019-05-07  M.HAAS        RECOMPILE ONLY - LE V6.3   *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * THE MODULE IS LOADED ONCE FOR THE WHOLE MERGE.  THE HELD
      * RECORD AND EVERY COUNTER BELOW CARRY ACROSS ENTRIES.
       01  WS-MODULE-IDENT.
           05  FILLER                  PIC X(08) VALUE 'CABSXLST'.
           05  FILLER                  PIC X(08) VALUE ' V2.05  '.
       01  WS-SWITCHES.
           05  WS-FIRST-ENTRY-SW       PIC X(01) VALUE 'Y'.
               88  WS-FIRST-ENTRY      VALUE 'Y'.
           05  WS-HOLDING-SW           PIC X(01) VALUE 'N'.
               88  WS-HOLDING          VALUE 'Y'.
           05  WS-RELEASE-SW           PIC X(01) VALUE 'N'.
               88  WS-RELEASE-PENDING  VALUE 'Y'.
           05  WS-FINAL-SW             PIC X(01) VALUE '0'.
               88  WS-FINAL-NOT-DONE   VALUE '0'.
               88  WS-FINAL-DONE       VALUE '1'.
           05  WS-NEW-WINS-SW          PIC X(01) VALUE 'N'.
               88  WS-NEW-WINS         VALUE 'Y'.
       01  WS-COUNTERS.
           05  WS-ENTRY-CNT            PIC S9(11) COMP-3 VALUE 0.
           05  WS-RELEASED-CNT         PIC S9(11) COMP-3 VALUE 0.
           05  WS-SUPERSEDED-CNT       PIC S9(11) COMP-3 VALUE 0.
           05  WS-TIE-RERUN-CNT        PIC S9(09) COMP-3 VALUE 0.
           05  WS-TIE-CYCLE-CNT        PIC S9(09) COMP-3 VALUE 0.
           05  WS-TIE-ARRIVAL-CNT      PIC S9(09) COMP-3 VALUE 0.
           05  WS-PREV-CONFIRM-CNT     PIC S9(11) COMP-3 VALUE 0.
      *
      * THE SURVIVING RECORD.  THE 180 BYTES ARE HELD WHOLE SO IT
      * CAN GO BACK UNCHANGED.  LAYOUT FROM CABSCTL.
       01  WS-HELD-AREA                PIC X(180) VALUE SPACES.
       01  WS-HELD-RECORD REDEFINES WS-HELD-AREA.
           05  WS-HD-RUN-ID            PIC X(12).
           05  WS-HD-PROCESS-ID        PIC X(08).
           05  WS-HD-STEP-SEQ          PIC 9(03).
           05  WS-HD-CYCLE-X           PIC X(05).
           05  WS-HD-CYCLE-YYDDD REDEFINES WS-HD-CYCLE-X
                                       PIC 9(05).
           05  WS-HD-BILL-PERIOD       PIC 9(06).
           05  WS-HD-RERUN-X           PIC X(02).
           05  WS-HD-RERUN-NBR REDEFINES WS-HD-RERUN-X
                                       PIC 9(02).
           05  WS-HD-TAIL              PIC X(144).
       01  WS-RELEASE-AREA             PIC X(180) VALUE SPACES.
      * CYCLE DATE WORK.  DATE COMPARISON IS ON YYDDD WITH A PIVOT
      * YEAR OF 70 - A YY UNDER 70 IS 20XX.
       01  WS-DATE-WORK.
           05  WS-DW-PIVOT-YY          PIC 9(02) VALUE 70.
           05  WS-DW-YY                PIC 9(02) VALUE 0.
           05  WS-DW-DDD               PIC 9(03) VALUE 0.
           05  WS-DW-CCYY              PIC 9(04) VALUE 0.
           05  WS-DW-EXPANDED          PIC 9(07) VALUE 0.
           05  WS-DW-HELD-CCYYDDD      PIC 9(07) VALUE 0.
           05  WS-DW-NEW-CCYYDDD       PIC 9(07) VALUE 0.
           05  WS-DW-SPLIT             PIC 9(05) VALUE 0.
           05  WS-DW-SPLIT-R REDEFINES WS-DW-SPLIT.
               10  WS-DW-S-YY          PIC 9(02).
               10  WS-DW-S-DDD         PIC 9(03).
       01  WS-COMPARE-WORK.
           05  WS-CW-HELD-RERUN        PIC 9(02) VALUE 0.
           05  WS-CW-NEW-RERUN         PIC 9(02) VALUE 0.
       LINKAGE SECTION.
       01  LK-PARM-LIST.
           05  LK-RECORD-PTR           POINTER.
           05  LK-PREV-PTR             POINTER.
           05  LK-LENGTH-PTR           POINTER.
      * THE RECORD LEAVING THE MERGE, LAID OUT FROM CABSCTL.  THE
      * COUNTS AND HASHES PASS THROUGH UNTOUCHED IN THE TAIL.
       01  LK-CONTROL-RECORD.
           05  LK-CT-RUN-ID            PIC X(12).
           05  LK-CT-PROCESS-ID        PIC X(08).
           05  LK-CT-STEP-SEQ          PIC 9(03).
           05  LK-CT-CYCLE-X           PIC X(05).
           05  LK-CT-CYCLE-YYDDD REDEFINES LK-CT-CYCLE-X
                                       PIC 9(05).
           05  LK-CT-BILL-PERIOD       PIC 9(06).
           05  LK-CT-RERUN-X           PIC X(02).
           05  LK-CT-RERUN-NBR REDEFINES LK-CT-RERUN-X
                                       PIC 9(02).
           05  LK-CT-TAIL              PIC X(144).
       01  LK-CONTROL-IMAGE            PIC X(180).
      * WORD TWO OF THE PARAMETER LIST.  ONLY THE KEY IS READ.
       01  LK-PREV-RECORD.
           05  LK-PV-RUN-ID            PIC X(12).
           05  LK-PV-PROCESS-ID        PIC X(08).
           05  LK-PV-STEP-SEQ          PIC 9(03).
           05  LK-PV-TAIL              PIC X(157).
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
           SET ADDRESS OF LK-CONTROL-RECORD TO LK-RECORD-PTR.
           SET ADDRESS OF LK-CONTROL-IMAGE  TO LK-RECORD-PTR.
           IF WS-RELEASE-PENDING
               PERFORM P6000-AFTER-RELEASE THRU P6000-EXIT
               GOBACK
           END-IF.
           ADD 1 TO WS-ENTRY-CNT.
           PERFORM P2000-TEST-KEY THRU P2000-EXIT.
           GOBACK.

       P1000-INIT.
           MOVE 'N' TO WS-FIRST-ENTRY-SW.
           DISPLAY 'CABSXLST ENTERED - KEEP LAST PER PROCESS'.

       P1000-EXIT.
           EXIT.

       P2000-TEST-KEY.
      * NOTHING HELD YET - TAKE THE RECORD AND DELETE THE COPY THE
      * SORT IS OFFERING.  EVERY RECORD THAT REACHES SORTOUT IS
      * WRITTEN FROM THE HELD AREA.
           IF NOT WS-HOLDING
               PERFORM P4000-TAKE-RECORD THRU P4000-EXIT
               MOVE 4 TO RETURN-CODE
               GO TO P2000-EXIT
           END-IF.
           IF LK-CT-PROCESS-ID = WS-HD-PROCESS-ID
             AND LK-CT-STEP-SEQ = WS-HD-STEP-SEQ
               PERFORM P3000-CHOOSE-LATER THRU P3000-EXIT
               MOVE 4 TO RETURN-CODE
               GO TO P2000-EXIT
           END-IF.
      * THE KEY HAS CHANGED.  WHATEVER IS HELD IS THE SURVIVOR FOR
      * THE PREVIOUS PROCESS AND STEP AND IS WRITTEN NOW.
           PERFORM P5000-RELEASE-HELD THRU P5000-EXIT.

       P2000-EXIT.
           EXIT.

       P3000-CHOOSE-LATER.
      * TWO RECORDS FOR ONE PROCESS AND STEP.  THE EARLIER ATTEMPT
      * IS DROPPED.  RERUN NUMBER FIRST, THEN CYCLE, THEN ARRIVAL.
           MOVE 'N' TO WS-NEW-WINS-SW.
           MOVE ZERO TO WS-CW-HELD-RERUN.
           MOVE ZERO TO WS-CW-NEW-RERUN.
           IF WS-HD-RERUN-X IS NUMERIC
               MOVE WS-HD-RERUN-NBR TO WS-CW-HELD-RERUN
           END-IF.
           IF LK-CT-RERUN-X IS NUMERIC
               MOVE LK-CT-RERUN-NBR TO WS-CW-NEW-RERUN
           END-IF.
           IF WS-CW-NEW-RERUN > WS-CW-HELD-RERUN
               MOVE 'Y' TO WS-NEW-WINS-SW
               ADD 1 TO WS-TIE-RERUN-CNT
               GO TO P3000-DECIDE
           END-IF.
           IF WS-CW-NEW-RERUN < WS-CW-HELD-RERUN
               ADD 1 TO WS-TIE-RERUN-CNT
               GO TO P3000-DECIDE
           END-IF.
           PERFORM P3400-COMPARE-CYCLE THRU P3400-EXIT.
           IF WS-DW-NEW-CCYYDDD > WS-DW-HELD-CCYYDDD
               MOVE 'Y' TO WS-NEW-WINS-SW
               ADD 1 TO WS-TIE-CYCLE-CNT
               GO TO P3000-DECIDE
           END-IF.
           IF WS-DW-NEW-CCYYDDD < WS-DW-HELD-CCYYDDD
               ADD 1 TO WS-TIE-CYCLE-CNT
               GO TO P3000-DECIDE
           END-IF.
      * THE RERUN NUMBER AND THE CYCLE DATE BOTH AGREE.  THE
      * RECORD THAT ARRIVED SECOND IS TAKEN AS THE LATER ONE.
           MOVE 'Y' TO WS-NEW-WINS-SW.
           ADD 1 TO WS-TIE-ARRIVAL-CNT.

       P3000-DECIDE.
           ADD 1 TO WS-SUPERSEDED-CNT.
           IF WS-NEW-WINS
               PERFORM P4000-TAKE-RECORD THRU P4000-EXIT
           END-IF.

       P3000-EXIT.
           EXIT.

       P3400-COMPARE-CYCLE.
      * EXPAND BOTH CYCLE DATES ON THE PIVOT AND COMPARE AS
      * CCYYDDD.  A NON NUMERIC CYCLE EXPANDS TO ZERO.
           MOVE ZERO TO WS-DW-HELD-CCYYDDD.
           MOVE ZERO TO WS-DW-NEW-CCYYDDD.
           IF WS-HD-CYCLE-X IS NUMERIC
               MOVE WS-HD-CYCLE-YYDDD TO WS-DW-SPLIT
               PERFORM P3600-EXPAND-CYCLE THRU P3600-EXIT
               MOVE WS-DW-EXPANDED TO WS-DW-HELD-CCYYDDD
           END-IF.
           IF LK-CT-CYCLE-X IS NUMERIC
               MOVE LK-CT-CYCLE-YYDDD TO WS-DW-SPLIT
               PERFORM P3600-EXPAND-CYCLE THRU P3600-EXIT
               MOVE WS-DW-EXPANDED TO WS-DW-NEW-CCYYDDD
           END-IF.

       P3400-EXIT.
           EXIT.

       P3600-EXPAND-CYCLE.
           MOVE WS-DW-S-YY  TO WS-DW-YY.
           MOVE WS-DW-S-DDD TO WS-DW-DDD.
           IF WS-DW-YY < WS-DW-PIVOT-YY
               COMPUTE WS-DW-CCYY = 2000 + WS-DW-YY
           ELSE
               COMPUTE WS-DW-CCYY = 1900 + WS-DW-YY
           END-IF.
           COMPUTE WS-DW-EXPANDED = (WS-DW-CCYY * 1000) + WS-DW-DDD.

       P3600-EXIT.
           EXIT.

       P4000-TAKE-RECORD.
      * COPY THE WHOLE 180 BYTES.  THE SORT BUFFER IS REUSED BEFORE
      * THE NEXT ENTRY, SO A COPY IS TAKEN AND NOT AN ADDRESS.
           MOVE LK-CONTROL-IMAGE TO WS-HELD-AREA.
           MOVE 'Y' TO WS-HOLDING-SW.

       P4000-EXIT.
           EXIT.

       P5000-RELEASE-HELD.
      * WRITE THE HELD RECORD BY POINTING WORD ONE AT IT AND
      * REPLYING 08.  THE RELEASE AREA IS A SEPARATE COPY SO THE
      * HELD AREA CAN BE OVERWRITTEN ON THE RE-ENTRY.
           MOVE WS-HELD-AREA TO WS-RELEASE-AREA.
           SET LK-RECORD-PTR TO ADDRESS OF WS-RELEASE-AREA.
           MOVE 'Y' TO WS-RELEASE-SW.
           MOVE 'N' TO WS-HOLDING-SW.
           ADD 1 TO WS-RELEASED-CNT.
           MOVE 8 TO RETURN-CODE.

       P5000-EXIT.
           EXIT.

       P6000-AFTER-RELEASE.
      * THE HELD RECORD HAS BEEN WRITTEN AND THE SAME INPUT IS
      * PRESENTED AGAIN.  WORD TWO ADDRESSES THE RECORD JUST
      * WRITTEN, WHICH UNDER THIS PROTOCOL IS THE ONE THIS EXIT
      * INSERTED.  IT IS READ ONLY TO CONFIRM THE RELEASE REACHED
      * SORTOUT - THE DECISION USES THE HELD COPY.
           MOVE 'N' TO WS-RELEASE-SW.
           IF LK-PREV-PTR NOT = NULL
               SET ADDRESS OF LK-PREV-RECORD TO LK-PREV-PTR
               IF LK-PV-PROCESS-ID NOT = LK-CT-PROCESS-ID
                   ADD 1 TO WS-PREV-CONFIRM-CNT
               END-IF
           END-IF.
           PERFORM P4000-TAKE-RECORD THRU P4000-EXIT.
           MOVE 4 TO RETURN-CODE.

       P6000-EXIT.
           EXIT.

       P8000-END-OF-MERGE.
      * THE LAST PROCESS AND STEP HAS NO FOLLOWING KEY TO CLOSE IT,
      * SO THE RECORD STILL HELD IS WRITTEN HERE.  THE SORT ENTERS
      * AGAIN WITH A NULL ADDRESS AFTER AN 08.
           IF WS-FINAL-DONE
               MOVE ZERO TO RETURN-CODE
               GO TO P8000-EXIT
           END-IF.
           IF WS-HOLDING
               MOVE WS-HELD-AREA TO WS-RELEASE-AREA
               SET LK-RECORD-PTR TO ADDRESS OF WS-RELEASE-AREA
               MOVE 'N' TO WS-HOLDING-SW
               ADD 1 TO WS-RELEASED-CNT
               MOVE 8 TO RETURN-CODE
               GO TO P8000-EXIT
           END-IF.
           MOVE '1' TO WS-FINAL-SW.
           PERFORM P8600-REPORT THRU P8600-EXIT.
           MOVE ZERO TO RETURN-CODE.

       P8000-EXIT.
           EXIT.

       P8600-REPORT.
      * THE TIE COUNTS ARE THE ONLY PLACE THE RUN RECORDS HOW THE
      * SURVIVING ATTEMPT WAS PICKED.
           DISPLAY 'CABSXLST ENTRIES     ' WS-ENTRY-CNT.
           DISPLAY 'CABSXLST RELEASED    ' WS-RELEASED-CNT.
           DISPLAY 'CABSXLST SUPERSEDED  ' WS-SUPERSEDED-CNT.
           DISPLAY 'CABSXLST BY RERUN    ' WS-TIE-RERUN-CNT.
           DISPLAY 'CABSXLST BY CYCLE    ' WS-TIE-CYCLE-CNT.
           DISPLAY 'CABSXLST BY ARRIVAL  ' WS-TIE-ARRIVAL-CNT.
           DISPLAY 'CABSXLST PRIOR SEEN  ' WS-PREV-CONFIRM-CNT.

       P8600-EXIT.
           EXIT.
