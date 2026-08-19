       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABSE15C.
      *****************************************************************
      * CABSE15C - SORT E15 INPUT EXIT - JURISDICTION INCLUSION       *
      * APPLICATION : CABS                                            *
      * COMPILER    : ENTERPRISE COBOL                                *
      * INVOKED BY  : OS SORT/MERGE, MODS=(E15=(CABSE15C,4096))       *
      *               ON THE JURISDICTIONAL SPLIT SORT STEPS OF       *
      *               CABJ1300, CABJ1500 AND CABJ1900                 *
      * INPUTS      : ONE 200 BYTE SORTIN RECORD PER ENTRY, CABSCDR   *
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
      *   WORKING STORAGE PERSISTS ACROSS ENTRIES FOR THE LIFE OF     *
      *   THE SORT STEP.                                              *
      *                                                               *
      * THE STATE TABLE BELOW IS THE OPERATING TERRITORY.  IT IS      *
      * MAINTAINED IN SOURCE AND REQUIRES A RECOMPILE AND A RELINK    *
      * OF THIS MODULE INTO TELCABS.COMMON.LOADLIB - SEE THE          *
      * OPERATIONS NOTE UNDER CABS-STD-033.                           *
      *                                                               *
      * REVISION HISTORY                                              *
      *   V1.00  1992-09-30  R.T.WHEELER   INITIAL - ASSEMBLER F      *
      *   V1.05  1997-03-17  D.OKONKWO     LOCAL TRAFFIC EXCLUDED     *
      *   V1.09  2001-06-22  J.M.CASTILLO  ADDED WV AND KY            *
      *   V2.00  2006-01-30  P.NAIR        RECODED IN COBOL FOR LE    *
      *   V2.01  2009-08-11  A.BUKOWSKI    INDETERMINATE TO INTER     *
      *   V2.03  2014-05-06  L.FERREIRA    LATA 220 SPLIT HANDLED     *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-MODULE-IDENT.
           05  FILLER                  PIC X(08) VALUE 'CABSE15C'.
           05  FILLER                  PIC X(08) VALUE ' V2.03  '.
       01  WS-SWITCHES.
           05  WS-FIRST-ENTRY-SW       PIC X(01) VALUE 'Y'.
               88  WS-FIRST-ENTRY      VALUE 'Y'.
           05  WS-STATE-FOUND-SW       PIC X(01) VALUE 'N'.
               88  WS-STATE-FOUND      VALUE 'Y'.
               88  WS-STATE-NOT-FOUND  VALUE 'N'.
       01  WS-COUNTERS.
           05  WS-ENTRY-CNT            PIC S9(11) COMP-3 VALUE 0.
           05  WS-KEPT-INTER           PIC S9(11) COMP-3 VALUE 0.
           05  WS-KEPT-INTRA           PIC S9(11) COMP-3 VALUE 0.
           05  WS-DROP-TOTAL           PIC S9(11) COMP-3 VALUE 0.
           05  WS-DROP-LOCAL           PIC S9(11) COMP-3 VALUE 0.
           05  WS-DROP-OUT-TERR        PIC S9(11) COMP-3 VALUE 0.
           05  WS-FORCED-INTER         PIC S9(11) COMP-3 VALUE 0.
      *
      * OPERATING TERRITORY.  A RECORD CARRYING A STATE CODE THAT IS
      * NOT IN THIS LIST IS NOT OURS TO BILL AND IS TAKEN OUT BEFORE
      * THE JURISDICTIONAL SPLIT SEES IT.  NOTHING DOWNSTREAM
      * REPEATS THIS TEST.
      *
       01  WS-TERRITORY-CONST.
           05  FILLER  PIC X(30) VALUE 'ALARCTDCDEFLGAIAILINKSKYLAMAMD'.
           05  FILLER  PIC X(30) VALUE 'MEMIMNMOMSNCNENHNJNMNVNYOHOKOR'.
           05  FILLER  PIC X(30) VALUE 'PARISCSDTNTXUTVAVTWAWIWVWY    '.
       01  WS-TERRITORY-TABLE REDEFINES WS-TERRITORY-CONST.
           05  WS-TT-STATE OCCURS 45 TIMES
                    INDEXED BY WS-TX          PIC X(02).
      *
      * LATA 220 WAS SPLIT IN 2014.  USAGE CARRYING THE OLD LATA ON
      * AN INTRASTATE RECORD IS RECLASSIFIED RATHER THAN DROPPED,
      * BECAUSE THE TWO HALVES NOW SIT IN DIFFERENT STATES.
      *
       01  WS-SPLIT-LATA-TABLE.
           05  FILLER                  PIC 9(03) VALUE 220.
           05  FILLER                  PIC 9(03) VALUE 224.
           05  FILLER                  PIC 9(03) VALUE 466.
           05  FILLER                  PIC 9(03) VALUE 938.
       01  WS-SPLIT-LATA-R REDEFINES WS-SPLIT-LATA-TABLE.
           05  WS-SL-LATA OCCURS 4 TIMES
                    INDEXED BY WS-SX          PIC 9(03).
       01  WS-WORK-FIELDS.
           05  WS-TEST-STATE           PIC X(02).
           05  WS-TEST-JURIS           PIC X(01).
           05  WS-TEST-LATA            PIC 9(03).
      *
       LINKAGE SECTION.
       01  LK-PARM-LIST.
           05  LK-RECORD-PTR           POINTER.
           05  LK-LENGTH-PTR           POINTER.
       01  LK-SORT-RECORD.
           05  LK-CD-REC-TYPE          PIC X(02).
           05  LK-CD-USAGE-TYPE        PIC X(01).
           05  LK-CD-JURIS-CD          PIC X(01).
               88  LK-INTERSTATE       VALUE 'I'.
               88  LK-INTRASTATE       VALUE 'S'.
               88  LK-LOCAL            VALUE 'L'.
               88  LK-INDETERMINATE    VALUE 'X' ' '.
           05  LK-CD-OCN               PIC X(04).
           05  LK-CD-BAN               PIC X(13).
           05  LK-CD-SEQ-NBR           PIC 9(09) COMP-3.
           05  LK-CD-RATE-ELEM         PIC X(06).
           05  LK-CD-STATE-CD          PIC X(02).
           05  LK-CD-ORIG-LATA         PIC 9(03).
           05  LK-CD-TERM-LATA         PIC 9(03).
           05  LK-CD-TAIL              PIC X(150).
      *
       PROCEDURE DIVISION USING LK-PARM-LIST.
       P0000-MAINLINE.
           MOVE ZERO TO RETURN-CODE.
           IF WS-FIRST-ENTRY
               MOVE 'N' TO WS-FIRST-ENTRY-SW
               DISPLAY 'CABSE15C ENTERED - JURISDICTION SELECTION'
           END-IF.
           IF LK-RECORD-PTR = NULL
               PERFORM P8000-END-OF-INPUT THRU P8000-EXIT
               GOBACK
           END-IF.
           SET ADDRESS OF LK-SORT-RECORD TO LK-RECORD-PTR.
           ADD 1 TO WS-ENTRY-CNT.
           MOVE LK-CD-STATE-CD  TO WS-TEST-STATE.
           MOVE LK-CD-JURIS-CD  TO WS-TEST-JURIS.
           MOVE LK-CD-ORIG-LATA TO WS-TEST-LATA.
           PERFORM P2000-RESOLVE-JURIS THRU P2000-EXIT.
           IF RETURN-CODE = 4
               GOBACK
           END-IF.
           PERFORM P3000-TERRITORY-TEST THRU P3000-EXIT.
           GOBACK.

       P2000-RESOLVE-JURIS.
      * LOCAL TRAFFIC IS NOT ACCESS TRAFFIC.  IT IS SETTLED UNDER
      * RECIPROCAL COMPENSATION AND MUST NOT REACH THE ACCESS
      * JURISDICTIONAL SPLIT, WHERE IT WOULD BE PRICED TWICE.
           IF LK-LOCAL
               ADD 1 TO WS-DROP-LOCAL
               ADD 1 TO WS-DROP-TOTAL
               MOVE 4 TO RETURN-CODE
               GO TO P2000-EXIT
           END-IF.
      * A RECORD THAT REACHED THIS POINT WITHOUT A RESOLVED
      * JURISDICTION IS TREATED AS INTERSTATE.  THE 2009 CHANGE
      * FOLLOWED THE TARIFF POSITION THAT UNIDENTIFIED TRAFFIC
      * DEFAULTS TO THE INTERSTATE RATE.  THE RECORD IS ALTERED IN
      * PLACE SO THE DOWNSTREAM SPLIT SEES A RESOLVED VALUE.
           IF LK-INDETERMINATE
               MOVE 'I' TO LK-CD-JURIS-CD
               MOVE 'I' TO WS-TEST-JURIS
               ADD 1 TO WS-FORCED-INTER
               MOVE 8 TO RETURN-CODE
           END-IF.

       P2000-EXIT.
           EXIT.

       P3000-TERRITORY-TEST.
      * INTERSTATE TRAFFIC IS BILLED WHEREVER IT ORIGINATES, SO THE
      * TERRITORY TEST IS ONLY APPLIED TO INTRASTATE.  THAT IS THE
      * 1997 POSITION AND IT HAS NOT BEEN REVISITED.
           IF WS-TEST-JURIS = 'I'
               ADD 1 TO WS-KEPT-INTER
               GO TO P3000-EXIT
           END-IF.
           MOVE 'N' TO WS-STATE-FOUND-SW.
           SET WS-TX TO 1.
           SEARCH WS-TT-STATE
               AT END
                   CONTINUE
               WHEN WS-TT-STATE (WS-TX) = WS-TEST-STATE
                   MOVE 'Y' TO WS-STATE-FOUND-SW
           END-SEARCH.
           IF WS-STATE-NOT-FOUND
               PERFORM P3400-SPLIT-LATA-TEST THRU P3400-EXIT
           END-IF.
           IF WS-STATE-FOUND
               ADD 1 TO WS-KEPT-INTRA
           ELSE
               ADD 1 TO WS-DROP-OUT-TERR
               ADD 1 TO WS-DROP-TOTAL
               MOVE 4 TO RETURN-CODE
           END-IF.

       P3000-EXIT.
           EXIT.

       P3400-SPLIT-LATA-TEST.
      * A STATE CODE WE DO NOT RECOGNISE MAY STILL BE OURS IF THE
      * ORIGINATING LATA IS ONE OF THE FOUR THAT WERE REDRAWN.  THE
      * RECORD IS RETAINED AND THE STATE CODE IS LEFT ALONE - THE
      * RESTATEMENT PROCESS CORRECTS IT LATER FROM THE CIRCUIT
      * INVENTORY.
           SET WS-SX TO 1.
           SEARCH WS-SL-LATA
               AT END
                   CONTINUE
               WHEN WS-SL-LATA (WS-SX) = WS-TEST-LATA
                   MOVE 'Y' TO WS-STATE-FOUND-SW
           END-SEARCH.

       P3400-EXIT.
           EXIT.

       P8000-END-OF-INPUT.
           DISPLAY 'CABSE15C ENTRIES     ' WS-ENTRY-CNT.
           DISPLAY 'CABSE15C INTERSTATE  ' WS-KEPT-INTER.
           DISPLAY 'CABSE15C INTRASTATE  ' WS-KEPT-INTRA.
           DISPLAY 'CABSE15C FORCED INTR ' WS-FORCED-INTER.
           DISPLAY 'CABSE15C LOCAL OUT   ' WS-DROP-LOCAL.
           DISPLAY 'CABSE15C OUT OF TERR ' WS-DROP-OUT-TERR.
           DISPLAY 'CABSE15C DROPPED     ' WS-DROP-TOTAL.
           MOVE ZERO TO RETURN-CODE.

       P8000-EXIT.
           EXIT.
