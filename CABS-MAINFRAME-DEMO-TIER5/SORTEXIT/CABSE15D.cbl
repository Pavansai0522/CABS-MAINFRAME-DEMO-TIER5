       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABSE15D.
      *****************************************************************
      * CABSE15D - SORT E15 INPUT EXIT - ZERO USAGE SUPPRESSION       *
      * APPLICATION : CABS                                            *
      * COMPILER    : ENTERPRISE COBOL                                *
      * INVOKED BY  : OS SORT/MERGE, MODS=(E15=(CABSE15D,4096))       *
      *               ON THE BILL DETAIL PRESORT STEPS OF CABRAT10    *
      *               AND ON THE SUMMARY PRESORT OF CABRAT09          *
      * INPUTS      : ONE 200 BYTE SORTIN RECORD PER ENTRY            *
      * OUTPUTS     : THE SAME RECORD, OR NOTHING                     *
      * CONTROL     : NONE - EXITS DO NOT WRITE CTLOUT, CABS-STD-041  *
      * BALANCE     : SORTIN = SORTOUT + WS-DROP-TOTAL                *
      * RESTART     : NOT RESTARTABLE - RERUN THE WHOLE SORT STEP     *
      *                                                               *
      * LINKAGE CONVENTION                                            *
      *   REGISTER 1 ADDRESSES A TWO WORD PARAMETER LIST.  WORD ONE   *
      *   IS THE ADDRESS OF THE INPUT RECORD OR BINARY ZERO AT END    *
      *   OF INPUT.  WORD TWO ADDRESSES THE LENGTH HALFWORD.  THE     *
      *   REPLY IS PLACED IN RETURN-CODE - 00 PASS, 04 DELETE,        *
      *   08 RETURN ALTERED, 12 NO FURTHER ENTRY, 16 TERMINATE.       *
      *                                                               *
      * A LINE OF ZERO MINUTES AND ZERO AMOUNT ADDS NOTHING TO AN     *
      * INVOICE BUT DOES ADD A PRINTED LINE AND A DETAIL RECORD.      *
      * THE 1995 BILL FORMAT REVIEW ASKED FOR THEM TO BE SUPPRESSED   *
      * AND THIS IS WHERE THAT WAS DONE.  THE THRESHOLD BELOW IS      *
      * NOT ZERO BUT HALF A CENT - A RESIDUAL BELOW THAT CANNOT       *
      * PRINT AND IS TREATED THE SAME WAY.                            *
      *                                                               *
      * REVISION HISTORY                                              *
      *   V1.00  1995-11-08  J.M.CASTILLO  INITIAL - ASSEMBLER F      *
      *   V1.02  1999-04-26  D.OKONKWO     THRESHOLD RAISED TO .005   *
      *   V2.00  2007-02-13  P.NAIR        RECODED IN COBOL FOR LE    *
      *   V2.01  2010-12-02  A.BUKOWSKI    SETUP CHARGE EXEMPTED      *
      *   V2.02  2016-08-19  M.HAAS        RECOMPILE ONLY - LE V6     *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-MODULE-IDENT.
           05  FILLER                  PIC X(08) VALUE 'CABSE15D'.
           05  FILLER                  PIC X(08) VALUE ' V2.02  '.
       01  WS-SWITCHES.
           05  WS-FIRST-ENTRY-SW       PIC X(01) VALUE 'Y'.
               88  WS-FIRST-ENTRY      VALUE 'Y'.
       01  WS-THRESHOLDS.
           05  WS-MIN-MINUTES          PIC S9(07)V9(02) COMP-3
                                                  VALUE 0.01.
           05  WS-MIN-AMOUNT           PIC S9(11)V9(05) COMP-3
                                                  VALUE 0.00500.
       01  WS-COUNTERS.
           05  WS-ENTRY-CNT            PIC S9(11) COMP-3 VALUE 0.
           05  WS-KEPT-CNT             PIC S9(11) COMP-3 VALUE 0.
           05  WS-DROP-TOTAL           PIC S9(11) COMP-3 VALUE 0.
           05  WS-DROP-ZERO-MOU        PIC S9(11) COMP-3 VALUE 0.
           05  WS-DROP-ZERO-AMT        PIC S9(11) COMP-3 VALUE 0.
           05  WS-KEPT-SETUP           PIC S9(11) COMP-3 VALUE 0.
           05  WS-KEPT-CREDIT          PIC S9(11) COMP-3 VALUE 0.
       01  WS-ACCUMS.
      * THE SUPPRESSED VALUE IS ACCUMULATED SO THE OPERATOR CAN SEE
      * WHAT WAS TAKEN OUT.  IT IS NOT ADDED BACK ANYWHERE.
           05  WS-SUPPRESSED-MOU       PIC S9(15)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-SUPPRESSED-AMT       PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-WORK-FIELDS.
           05  WS-ABS-MINUTES          PIC S9(07)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-ABS-AMOUNT           PIC S9(11)V9(05) COMP-3
                                                  VALUE 0.
      *
       LINKAGE SECTION.
       01  LK-PARM-LIST.
           05  LK-RECORD-PTR           POINTER.
           05  LK-LENGTH-PTR           POINTER.
       01  LK-SORT-RECORD.
           05  LK-RR-REC-TYPE          PIC X(02).
           05  LK-RR-USAGE-TYPE        PIC X(01).
           05  LK-RR-FILLER-1          PIC X(01).
           05  LK-RR-OCN               PIC X(04).
           05  LK-RR-BAN               PIC X(13).
           05  LK-RR-SEQ               PIC 9(09) COMP-3.
           05  LK-RR-FILLER-2          PIC X(09).
           05  LK-RR-JURIS-CD          PIC X(01).
           05  LK-RR-STATE-CD          PIC X(02).
           05  LK-RR-FILLER-3          PIC X(07).
           05  LK-RR-RATE-ELEM         PIC X(06).
           05  LK-RR-LINE-CLASS        PIC X(01).
               88  LK-RR-USAGE-LINE    VALUE 'U'.
               88  LK-RR-SETUP-LINE    VALUE 'S'.
               88  LK-RR-CREDIT-LINE   VALUE 'C'.
               88  LK-RR-MAKEUP-LINE   VALUE 'M'.
           05  LK-RR-FILLER-4          PIC X(19).
           05  LK-RR-CHG-MINUTES       PIC S9(07)V9(02) COMP-3.
           05  LK-RR-RATE              PIC S9(05)V9(05) COMP-3.
           05  LK-RR-AMOUNT            PIC S9(11)V9(05) COMP-3.
           05  LK-RR-TAIL              PIC X(110).
      *
       PROCEDURE DIVISION USING LK-PARM-LIST.
       P0000-MAINLINE.
           MOVE ZERO TO RETURN-CODE.
           IF WS-FIRST-ENTRY
               MOVE 'N' TO WS-FIRST-ENTRY-SW
               DISPLAY 'CABSE15D ENTERED - ZERO USAGE SUPPRESSION'
           END-IF.
           IF LK-RECORD-PTR = NULL
               PERFORM P8000-END-OF-INPUT THRU P8000-EXIT
               GOBACK
           END-IF.
           SET ADDRESS OF LK-SORT-RECORD TO LK-RECORD-PTR.
           ADD 1 TO WS-ENTRY-CNT.
           PERFORM P2000-EXEMPT-TEST THRU P2000-EXIT.
           GOBACK.

       P2000-EXEMPT-TEST.
      * A SETUP CHARGE LINE CARRIES NO MINUTES BY DESIGN AND MUST
      * SURVIVE.  A CREDIT LINE MAY CARRY A NEGATIVE AMOUNT AND NO
      * MINUTES AND MUST ALSO SURVIVE.  BOTH WERE BEING LOST UNTIL
      * THE 2010 CHANGE.
           IF LK-RR-SETUP-LINE
               ADD 1 TO WS-KEPT-SETUP
               ADD 1 TO WS-KEPT-CNT
               GO TO P2000-EXIT
           END-IF.
           IF LK-RR-CREDIT-LINE OR LK-RR-MAKEUP-LINE
               ADD 1 TO WS-KEPT-CREDIT
               ADD 1 TO WS-KEPT-CNT
               GO TO P2000-EXIT
           END-IF.
           PERFORM P2400-THRESHOLD-TEST THRU P2400-EXIT.

       P2000-EXIT.
           EXIT.

       P2400-THRESHOLD-TEST.
      * BOTH TESTS MUST FAIL BEFORE THE RECORD IS TAKEN OUT.  A LINE
      * WITH MINUTES BUT NO CHARGE IS STILL PRINTED - THE CUSTOMER
      * IS ENTITLED TO SEE THE USAGE EVEN WHERE IT IS UNPRICED.
           MOVE LK-RR-CHG-MINUTES TO WS-ABS-MINUTES.
           IF WS-ABS-MINUTES < ZERO
               COMPUTE WS-ABS-MINUTES = WS-ABS-MINUTES * -1
           END-IF.
           MOVE LK-RR-AMOUNT TO WS-ABS-AMOUNT.
           IF WS-ABS-AMOUNT < ZERO
               COMPUTE WS-ABS-AMOUNT = WS-ABS-AMOUNT * -1
           END-IF.
           IF WS-ABS-MINUTES NOT < WS-MIN-MINUTES
               ADD 1 TO WS-KEPT-CNT
               GO TO P2400-EXIT
           END-IF.
           IF WS-ABS-AMOUNT NOT < WS-MIN-AMOUNT
               ADD 1 TO WS-KEPT-CNT
               GO TO P2400-EXIT
           END-IF.
           IF WS-ABS-MINUTES < WS-MIN-MINUTES
               ADD 1 TO WS-DROP-ZERO-MOU
           END-IF.
           IF WS-ABS-AMOUNT < WS-MIN-AMOUNT
               ADD 1 TO WS-DROP-ZERO-AMT
           END-IF.
           ADD LK-RR-CHG-MINUTES TO WS-SUPPRESSED-MOU.
           ADD LK-RR-AMOUNT TO WS-SUPPRESSED-AMT.
           ADD 1 TO WS-DROP-TOTAL.
           MOVE 4 TO RETURN-CODE.

       P2400-EXIT.
           EXIT.

       P8000-END-OF-INPUT.
      * THE SUPPRESSED VALUE IS REPORTED HERE AND NOWHERE ELSE.  IT
      * IS NOT ADDED TO A CONTROL RECORD AND IT DOES NOT APPEAR ON
      * ANY BALANCING REPORT, SO A RUN THAT SUPPRESSES MATERIAL
      * VALUE STILL RECONCILES.
           DISPLAY 'CABSE15D ENTRIES     ' WS-ENTRY-CNT.
           DISPLAY 'CABSE15D KEPT        ' WS-KEPT-CNT.
           DISPLAY 'CABSE15D SETUP KEPT  ' WS-KEPT-SETUP.
           DISPLAY 'CABSE15D CREDIT KEPT ' WS-KEPT-CREDIT.
           DISPLAY 'CABSE15D ZERO MOU    ' WS-DROP-ZERO-MOU.
           DISPLAY 'CABSE15D ZERO AMT    ' WS-DROP-ZERO-AMT.
           DISPLAY 'CABSE15D DROPPED     ' WS-DROP-TOTAL.
           DISPLAY 'CABSE15D SUPP MOU    ' WS-SUPPRESSED-MOU.
           DISPLAY 'CABSE15D SUPP AMT    ' WS-SUPPRESSED-AMT.
           MOVE ZERO TO RETURN-CODE.

       P8000-EXIT.
           EXIT.
