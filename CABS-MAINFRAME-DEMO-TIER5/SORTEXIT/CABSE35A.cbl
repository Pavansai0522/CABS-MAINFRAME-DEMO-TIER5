       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABSE35A.
      *****************************************************************
      * CABSE35A - SORT E35 OUTPUT EXIT - RATING CONTROL PREFIX       *
      * APPLICATION : CABS                                            *
      * COMPILER    : ENTERPRISE COBOL                                *
      * INVOKED BY  : OS SORT/MERGE, MODS=(E35=(CABSE35A,4096))       *
      *               ON CONTROL CARD MEMBER JCL/CTLCARDS/CABSRT04    *
      * INPUTS      : ONE 200 BYTE RECORD FROM THE FINAL MERGE        *
      * OUTPUTS     : THE SAME RECORD WITH BYTES 189 THROUGH 200      *
      *               OVERLAID BY THE RATING CONTROL PREFIX           *
      * CONTROL     : NONE - EXITS DO NOT WRITE CTLOUT, CABS-STD-041  *
      * BALANCE     : RECORDS IN = RECORDS OUT                        *
      * RESTART     : NOT RESTARTABLE - RERUN THE WHOLE SORT STEP     *
      *                                                               *
      * LINKAGE CONVENTION                                            *
      *   REGISTER 1 ADDRESSES A THREE WORD PARAMETER LIST.  WORD     *
      *   ONE IS THE ADDRESS OF THE RECORD LEAVING THE FINAL MERGE,   *
      *   OR BINARY ZERO WHEN THE MERGE IS EXHAUSTED.  WORD TWO IS    *
      *   THE ADDRESS OF THE RECORD JUST WRITTEN TO SORTOUT AND IS    *
      *   USED ONLY BY EXITS THAT NEED TO COMPARE ADJACENT RECORDS.   *
      *   WORD THREE ADDRESSES THE LENGTH HALFWORD.  THE REPLY IS     *
      *   PLACED IN RETURN-CODE -                                     *
      *     00  NO MORE RECORDS TO INSERT - TAKE THE NEXT ONE         *
      *     04  DELETE THE RECORD - DO NOT WRITE IT TO SORTOUT        *
      *     08  WRITE THE RECORD ADDRESSED BY WORD ONE                *
      *     12  DO NOT ENTER THIS EXIT AGAIN                          *
      *     16  TERMINATE THE SORT                                    *
      *   THE EXIT IS ENTERED A FINAL TIME WITH A NULL RECORD         *
      *   ADDRESS SO IT CAN INSERT TRAILING RECORDS.  WORKING         *
      *   STORAGE PERSISTS FOR THE LIFE OF THE SORT STEP.             *
      *                                                               *
      * THE PREFIX IS CALLED A PREFIX FOR HISTORICAL REASONS.  IN     *
      * THE 1990 LAYOUT IT SAT IN FRONT OF THE RECORD.  WHEN THE      *
      * RECORD WAS FIXED AT 200 BYTES IN 1994 IT WAS MOVED INTO THE   *
      * TRAILING FILLER AND THE NAME WAS KEPT.                        *
      *                                                               *
      * REVISION HISTORY                                              *
      *   V1.00  1990-10-04  R.T.WHEELER   INITIAL - ASSEMBLER F      *
      *   V1.04  1994-06-15  D.OKONKWO     PREFIX MOVED TO TRAILER    *
      *   V2.00  2004-08-02  P.NAIR        RECODED IN COBOL FOR LE    *
      *   V2.02  2008-05-21  A.BUKOWSKI    WORK ORDINAL STAMPED       *
      *   V2.04  2015-03-09  L.FERREIRA    STRING LIMIT TO 65000      *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-MODULE-IDENT.
           05  FILLER                  PIC X(08) VALUE 'CABSE35A'.
           05  FILLER                  PIC X(08) VALUE ' V2.04  '.
       01  WS-SWITCHES.
           05  WS-FIRST-ENTRY-SW       PIC X(01) VALUE 'Y'.
               88  WS-FIRST-ENTRY      VALUE 'Y'.
       01  WS-COUNTERS.
           05  WS-ENTRY-CNT            PIC S9(11) COMP-3 VALUE 0.
           05  WS-STAMPED-CNT          PIC S9(11) COMP-3 VALUE 0.
           05  WS-STRING-CNT           PIC S9(09) COMP-3 VALUE 0.
      *
      * THE SORT HOLDS ONE STRING PER WORK DATA SET AT THE REGION
      * SIZE THE RATING JOBS RUN WITH.  THE ORDINAL BELOW STEPS
      * EVERY TIME THAT MANY RECORDS HAVE PASSED THROUGH, WHICH IS
      * HOW THE DOWNSTREAM EXITS KNOW WHICH WORK DATA SET A RECORD
      * CAME OUT OF.  THE LIMIT WAS RAISED FROM 40000 TO 65000 WHEN
      * THE REGION WENT TO 0M IN 2015.
      *
       01  WS-STRING-CONTROL.
           05  WS-STRING-LIMIT         PIC S9(09) COMP-3
                                                  VALUE 65000.
           05  WS-WORK-ORDINAL         PIC 9(02) VALUE 01.
           05  WS-STRING-SEQ           PIC 9(04) VALUE 0.
       01  WS-RUN-STAMP.
           05  WS-RS-CYCLE-YYDDD       PIC 9(05) VALUE 0.
       01  WS-DATE-WORK.
           05  WS-DW-CURRENT           PIC 9(07) VALUE 0.
           05  WS-DW-CURRENT-R REDEFINES WS-DW-CURRENT.
               10  WS-DW-CCYY          PIC 9(04).
               10  WS-DW-DDD           PIC 9(03).
           05  WS-DW-YY                PIC 9(02) VALUE 0.
       01  WS-PREFIX-AREA.
           05  WS-PX-RUN-STAMP         PIC 9(05) VALUE 0.
           05  WS-PX-WORK-ORD          PIC 9(02) VALUE 01.
           05  WS-PX-STRING-SEQ        PIC 9(04) VALUE 0.
           05  WS-PX-EXIT-VER          PIC X(01) VALUE 'D'.
      *
       LINKAGE SECTION.
       01  LK-PARM-LIST.
           05  LK-RECORD-PTR           POINTER.
           05  LK-PREV-PTR             POINTER.
           05  LK-LENGTH-PTR           POINTER.
       01  LK-SORT-RECORD.
           05  LK-RR-HEAD              PIC X(188).
           05  LK-RR-CTL-PREFIX.
               10  LK-RC-RUN-STAMP     PIC 9(05).
               10  LK-RC-WORK-ORD      PIC 9(02).
               10  LK-RC-STRING-SEQ    PIC 9(04).
               10  LK-RC-EXIT-VER      PIC X(01).
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
           ADD 1 TO WS-ENTRY-CNT.
           PERFORM P2000-STEP-ORDINAL THRU P2000-EXIT.
           PERFORM P3000-STAMP-PREFIX THRU P3000-EXIT.
           MOVE 8 TO RETURN-CODE.
           GOBACK.

       P1000-INIT.
      * THE CYCLE STAMP IS TAKEN FROM THE SYSTEM DATE.  AN EXIT HAS
      * NO SYSIN OF ITS OWN, SO THERE IS NOWHERE ELSE TO GET IT.
      * A SORT RUN AFTER MIDNIGHT ON THE LAST NIGHT OF THE CYCLE
      * WILL THEREFORE STAMP THE FOLLOWING DAY.
           MOVE 'N' TO WS-FIRST-ENTRY-SW.
           ACCEPT WS-DW-CURRENT FROM DAY.
           MOVE WS-DW-CCYY TO WS-DW-YY.
           COMPUTE WS-DW-YY = WS-DW-CCYY - 2000.
           IF WS-DW-CCYY < 2000
               COMPUTE WS-DW-YY = WS-DW-CCYY - 1900
           END-IF.
           COMPUTE WS-RS-CYCLE-YYDDD = (WS-DW-YY * 1000) + WS-DW-DDD.
           MOVE WS-RS-CYCLE-YYDDD TO WS-PX-RUN-STAMP.
           MOVE 01 TO WS-WORK-ORDINAL.
           MOVE ZERO TO WS-STRING-CNT.
           MOVE ZERO TO WS-STRING-SEQ.
           DISPLAY 'CABSE35A ENTERED - STAMP ' WS-RS-CYCLE-YYDDD.

       P1000-EXIT.
           EXIT.

       P2000-STEP-ORDINAL.
      * COUNT THE RECORDS LEAVING THE MERGE.  EVERY TIME THE STRING
      * LIMIT IS REACHED THE ORDINAL IS STEPPED AND THE STRING
      * SEQUENCE IS RESTARTED.  A RUN THAT FITS IN ONE WORK DATA
      * SET NEVER STEPS PAST 01.
           ADD 1 TO WS-STRING-CNT.
           ADD 1 TO WS-STRING-SEQ.
           IF WS-STRING-CNT NOT < WS-STRING-LIMIT
               ADD 1 TO WS-WORK-ORDINAL
               MOVE ZERO TO WS-STRING-CNT
               MOVE ZERO TO WS-STRING-SEQ
               DISPLAY 'CABSE35A WORK ORDINAL NOW '
                       WS-WORK-ORDINAL
           END-IF.
           IF WS-WORK-ORDINAL > 99
               MOVE 99 TO WS-WORK-ORDINAL
           END-IF.

       P2000-EXIT.
           EXIT.

       P3000-STAMP-PREFIX.
      * OVERLAY THE TRAILING TWELVE BYTES.  THE RATING MODULES DO
      * NOT REFERENCE THIS AREA AND NEITHER DOES ANY COPYBOOK, SO
      * THE OVERLAY IS SAFE ON THE CURRENT LAYOUT.
           MOVE WS-PX-RUN-STAMP  TO LK-RC-RUN-STAMP.
           MOVE WS-WORK-ORDINAL  TO LK-RC-WORK-ORD.
           MOVE WS-STRING-SEQ    TO LK-RC-STRING-SEQ.
           MOVE WS-PX-EXIT-VER   TO LK-RC-EXIT-VER.
           ADD 1 TO WS-STAMPED-CNT.

       P3000-EXIT.
           EXIT.

       P8000-END-OF-MERGE.
           DISPLAY 'CABSE35A ENTRIES     ' WS-ENTRY-CNT.
           DISPLAY 'CABSE35A STAMPED     ' WS-STAMPED-CNT.
           DISPLAY 'CABSE35A ORDINALS    ' WS-WORK-ORDINAL.
           MOVE ZERO TO RETURN-CODE.

       P8000-EXIT.
           EXIT.
