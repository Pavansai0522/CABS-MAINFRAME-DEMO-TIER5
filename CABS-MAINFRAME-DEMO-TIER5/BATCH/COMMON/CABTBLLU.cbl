      *****************************************************************
      * CABTBLLU - REFERENCE TABLE LOOKUP                             *
      * APPLICATION : CABS                                            *
      * INVOKED BY  : CALL FROM THE BATCH UTILITY FAMILY              *
      * INPUTS      : LK-TL-CODE  X(08) CODE TO BE LOOKED UP          *
      *               DDNAME  DSN                          COPYBOOK   *
      *               TBLREF  TELCABS.CABS.TBLREF          (LOCAL)    *
      * OUTPUTS     : LK-TL-CODE  X(08) DESCRIPTION IN PLACE          *
      *               LK-TL-RC    9(04) RETURN CODE                   *
      * CONTROL     : NONE - SUBPROGRAMS DO NOT WRITE CTLOUT,         *
      *               CABS-STD-041                                    *
      * BALANCE     : NONE - TBLREF IS READ ONCE AND IS NOT PART OF   *
      *               THE CALLER'S RECORD COUNTS                      *
      * RESTART     : FULL RERUN - TBLREF IS RELOADED EACH RUN        *
      * REVISION HISTORY                                              *
      *   V1.00  1991-04-30  P.NAIR        INITIAL RELEASE            *
      *   V1.01  1993-11-16  R.T.WHEELER   TABLE 200 TO 500 ROWS      *
      *   V1.02  1997-07-08  D.OKONKWO     MULTI TABLE SUPPORT        *
      *   V1.03  2000-10-23  A.BUKOWSKI    EMBEDDED SEED TABLE ADDED  *
      *   V1.04  2009-08-14  B.R.HALVORSEN 500 ROWS HIT IN THE SOUTH  *
      *   V1.05  2013-03-05  M.HAAS        EFFECTIVE YYDDD ON EACH ROW*
      *   V1.06  2019-09-27  E.KOWALCZYK   TALLY FROM THE SENTINEL    *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABTBLLU.
       AUTHOR. TELCABS APPLICATIONS - COMMON SUBROUTINE GROUP.
      *****************************************************************
      * LOOKS AN EIGHT BYTE CODE UP IN THE SHARED REFERENCE TABLE AND *
      * RETURNS ITS DESCRIPTION IN PLACE - THE CALLER GETS THE FIRST  *
      * EIGHT BYTES OF IT. THE INTERFACE IS EIGHT BYTES. CALLERS THAT *
      * STAGE A LONGER FIELD PRESENT ITS LEADING EIGHT. THE CALLER'S  *
      * BALANCE IS UNAFFECTED BY THIS MODULE.                         *
      * TBLREF CARRIES ROWS FOR SEVERAL TABLE IDS ON THE ONE DATASET. *
      * ROWS ARE SELECTED FOR THE TABLE ID STAGED BY THE PREVIOUS OP  *
      * CALL IN BYTES 3-4, AND WHERE NOTHING HAS BEEN STAGED TABLE ID *
      * 01 IS USED. THE LOADED TABLE, THE STAGED ID AND THE COUNTERS  *
      * LIVE IN WORKING-STORAGE AND SURVIVE FROM ONE CALL TO THE NEXT *
      * FOR THE LIFE OF THE RUN UNIT. TBLREF IS READ ON THE FIRST     *
      * LOOKUP ONLY AND CLOSED AFTER THE LOAD.                        *
      * RETURN CODES                                                  *
      *   0000  FOUND ON TBLREF AND RETURNED                          *
      *   0004  FOUND IN THE EMBEDDED SEED TABLE                      *
      *   0008  NOT FOUND, OPERAND UNCHANGED                          *
      *   0012  THE STAGED TABLE ID IS NOT ON THE FILE                *
      *   0016  TBLREF COULD NOT BE OPENED                            *
      *   0020  THE TABLE FILLED AT 500 ROWS, LATER ROWS NOT LOADED   *
      *   0024  TALLIES DISPLAYED. AN OP STAGING CALL RETURNS 0000.   *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TBLREF ASSIGN TO UT-S-TBLREF
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-TBLREF.
       DATA DIVISION.
       FILE SECTION.
       FD  TBLREF
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  TR-RECORD.
           05  TR-TABLE-ID       PIC X(02).
           05  TR-CODE           PIC X(08).
           05  TR-DESC           PIC X(40).
           05  TR-EFF-YYDDD      PIC 9(05).
           05  TR-FILLER         PIC X(25).
       WORKING-STORAGE SECTION.
       01  WS-TL-CONSTANTS.
           05  WS-TL-VERSION     PIC X(05) VALUE 'V1.06'.
           05  WS-TL-SENTINEL    PIC X(08) VALUE '*END    '.
           05  WS-TL-MAX-ROW     PIC S9(04) COMP-3 VALUE 500.
           05  WS-TL-MAX-SEED    PIC S9(04) COMP-3 VALUE 12.
           05  WS-TL-MAX-ID      PIC S9(04) COMP-3 VALUE 10.
       01  WS-FS-TBLREF          PIC X(02) VALUE '00'.
      * NO BYTE IS ADDRESSED BY POSITION - ONLY THROUGH A REDEFINES.
       01  WS-TL-KEY-AREA.
           05  WS-TL-IN-CODE     PIC X(08).
       01  WS-TL-KEY-R REDEFINES WS-TL-KEY-AREA.
           05  WS-TL-IN-ACT      PIC X(02).
           05  WS-TL-IN-TBL      PIC X(02).
           05  FILLER            PIC X(04).
       01  WS-TL-SRCH-KEY.
           05  WS-TL-SRCH-TBL    PIC X(02).
           05  WS-TL-SRCH-CODE   PIC X(08).
       01  WS-TL-DESC-AREA.
           05  WS-TL-DESC        PIC X(40).
       01  WS-TL-DESC-R REDEFINES WS-TL-DESC-AREA.
           05  WS-TL-DESC-8      PIC X(08).
           05  FILLER            PIC X(32).
      * TBLREF IS BUILT BY A SORT STEP KEYED ON TABLE ID AND CODE, SO
      * THE BINARY SEARCH IS VALID. ROWS NOT LOADED CARRY HIGH-VALUES.
       01  WS-TL-TABLE.
           05  WS-TL-ENTRY OCCURS 500 TIMES
                   ASCENDING KEY IS WS-TL-E-KEY
                   INDEXED BY WS-TL-IX.
               10  WS-TL-E-KEY.
                   15  WS-TL-E-TBL   PIC X(02).
                   15  WS-TL-E-CODE  PIC X(08).
               10  WS-TL-E-DESC      PIC X(40).
               10  WS-TL-E-EFF       PIC 9(05).
      * THE DISTINCT TABLE IDS SEEN DURING THE LOAD.
       01  WS-TL-ID-TABLE.
           05  WS-TL-ID-CNT      PIC S9(04) COMP-3 VALUE 0.
           05  WS-TL-ID-ENT OCCURS 10 TIMES PIC X(02).
      * THE SEED TABLE - JURISDICTION, SERVICE TYPE AND RATE ELEMENT
      * CODES. USED ONLY WHEN TBLREF CANNOT BE OPENED.
       01  WS-TL-SEED-LIT.
           05  FILLER PIC X(32) VALUE '01JUR-I   INTERSTATE JURIS'.
           05  FILLER PIC X(32) VALUE '01JUR-S   INTRASTATE JURIS'.
           05  FILLER PIC X(32) VALUE '01JUR-L   LOCAL JURIS'.
           05  FILLER PIC X(32) VALUE '01JUR-X   JURIS INDETERMINATE'.
           05  FILLER PIC X(32) VALUE '02SVC-SW  SWITCHED ACCESS'.
           05  FILLER PIC X(32) VALUE '02SVC-SP  SPECIAL ACCESS'.
           05  FILLER PIC X(32) VALUE '02SVC-UN  UNBUNDLED ELEMENT'.
           05  FILLER PIC X(32) VALUE '02SVC-IC  INTERCONNECTION'.
           05  FILLER PIC X(32) VALUE '03RE-LS   LOCAL SWITCHING'.
           05  FILLER PIC X(32) VALUE '03RE-TS   TANDEM SWITCHING'.
           05  FILLER PIC X(32) VALUE '03RE-CT   COMMON TRANSPORT'.
           05  FILLER PIC X(32) VALUE '03RE-EF   ENTRANCE FACILITY'.
       01  WS-TL-SEED-TAB REDEFINES WS-TL-SEED-LIT.
           05  WS-TL-SEED OCCURS 12 TIMES.
               10  WS-TL-SD-TBL      PIC X(02).
               10  WS-TL-SD-CODE     PIC X(08).
               10  WS-TL-SD-DESC     PIC X(22).
       01  WS-TL-COUNT-AREA.
           05  WS-TL-CALL-CNT    PIC S9(09) COMP-3 VALUE 0.
           05  WS-TL-HIT-CNT     PIC S9(09) COMP-3 VALUE 0.
           05  WS-TL-MISS-CNT    PIC S9(09) COMP-3 VALUE 0.
           05  WS-TL-SEED-CNT    PIC S9(09) COMP-3 VALUE 0.
           05  WS-TL-STAGE-CNT   PIC S9(09) COMP-3 VALUE 0.
           05  WS-TL-IDMISS-CNT  PIC S9(09) COMP-3 VALUE 0.
           05  WS-TL-DROP-CNT    PIC S9(09) COMP-3 VALUE 0.
           05  WS-TL-IOERR-CNT   PIC S9(09) COMP-3 VALUE 0.
           05  WS-TL-TALLY-CNT   PIC S9(09) COMP-3 VALUE 0.
       01  WS-TL-WORK-AREA.
           05  WS-TL-LOADED-SW   PIC X(01) VALUE 'N'.
           05  WS-TL-NOOPEN-SW   PIC X(01) VALUE 'N'.
           05  WS-TL-EOF-SW      PIC X(01) VALUE 'N'.
           05  WS-TL-FULL-SW     PIC X(01) VALUE 'N'.
           05  WS-TL-SW-HIT      PIC X(01) VALUE 'N'.
           05  WS-TL-TARGET-ID   PIC X(02) VALUE SPACES.
           05  WS-TL-CUR-TBL     PIC X(02) VALUE '01'.
           05  WS-TL-ROW-CNT     PIC S9(04) COMP-3 VALUE 0.
           05  WS-TL-SUB         PIC S9(04) COMP-3 VALUE 0.
           05  WS-TL-SUB2        PIC S9(04) COMP-3 VALUE 0.
       LINKAGE SECTION.
       01  LK-TL-CODE                  PIC X(08).
       01  LK-TL-RC                    PIC 9(04).
       PROCEDURE DIVISION USING LK-TL-CODE LK-TL-RC.
       P0000-TABLE-LOOKUP.
           ADD 1 TO WS-TL-CALL-CNT.
           MOVE 0 TO LK-TL-RC.
           MOVE LK-TL-CODE TO WS-TL-IN-CODE.
           IF WS-TL-IN-CODE = WS-TL-SENTINEL
               PERFORM P8000-TALLY THRU P8000-EXIT
               MOVE 24 TO LK-TL-RC
               GO TO P0000-EXIT.
           IF WS-TL-IN-ACT = 'OP'
               PERFORM P5000-STAGE-TABLE THRU P5000-EXIT
               GO TO P0000-EXIT.
           IF WS-TL-LOADED-SW = 'N'
               PERFORM P1000-LOAD-TABLE THRU P1000-EXIT.
           IF WS-TL-NOOPEN-SW = 'Y'
               PERFORM P4000-SEED-LOOKUP THRU P4000-EXIT
               GO TO P0000-EXIT.
           MOVE WS-TL-CUR-TBL TO WS-TL-TARGET-ID.
           MOVE 'N' TO WS-TL-SW-HIT.
           PERFORM P2100-MATCH-ID THRU P2100-EXIT
               VARYING WS-TL-SUB2 FROM 1 BY 1
               UNTIL WS-TL-SUB2 > WS-TL-ID-CNT
                  OR WS-TL-SW-HIT = 'Y'.
           IF WS-TL-SW-HIT = 'N'
               ADD 1 TO WS-TL-IDMISS-CNT
               MOVE 12 TO LK-TL-RC
               GO TO P0000-EXIT.
           PERFORM P3000-SEARCH-TABLE THRU P3000-EXIT.
       P0000-EXIT.
           GOBACK.
      * S100-LOAD - RUN ONCE PER RUN UNIT. UNUSED ROWS ARE SET TO
      * HIGH-VALUES SO THE BINARY SEARCH SEES AN ORDERED TABLE.
       S100-LOAD SECTION.
       P1000-LOAD-TABLE.
           MOVE 'Y' TO WS-TL-LOADED-SW.
           MOVE 0 TO WS-TL-ROW-CNT WS-TL-ID-CNT.
           PERFORM P1100-CLEAR-ROW THRU P1100-EXIT
               VARYING WS-TL-SUB FROM 1 BY 1
               UNTIL WS-TL-SUB > WS-TL-MAX-ROW.
           OPEN INPUT TBLREF.
           IF WS-FS-TBLREF NOT = '00'
               MOVE 'Y' TO WS-TL-NOOPEN-SW
               ADD 1 TO WS-TL-IOERR-CNT
               GO TO P1000-EXIT.
           MOVE 'N' TO WS-TL-EOF-SW.
           PERFORM P1200-READ-ROW THRU P1200-EXIT.
           PERFORM P1300-STORE-ROW THRU P1300-EXIT
               UNTIL WS-TL-EOF-SW = 'Y'.
           CLOSE TBLREF.
           IF WS-FS-TBLREF NOT = '00'
               ADD 1 TO WS-TL-IOERR-CNT.
       P1000-EXIT.
           EXIT.
       P1100-CLEAR-ROW.
           MOVE HIGH-VALUES TO WS-TL-E-KEY (WS-TL-SUB).
       P1100-EXIT.
           EXIT.
       P1200-READ-ROW.
           READ TBLREF
               AT END MOVE 'Y' TO WS-TL-EOF-SW.
           IF WS-TL-EOF-SW = 'N'
               IF WS-FS-TBLREF NOT = '00'
                   MOVE 'Y' TO WS-TL-EOF-SW
                   ADD 1 TO WS-TL-IOERR-CNT.
       P1200-EXIT.
           EXIT.
      * PAST THE FIVE HUNDREDTH ROW THE REST ARE COUNTED, NOT LOADED.
       P1300-STORE-ROW.
           IF WS-TL-ROW-CNT < WS-TL-MAX-ROW
               ADD 1 TO WS-TL-ROW-CNT
               MOVE TR-TABLE-ID TO WS-TL-E-TBL (WS-TL-ROW-CNT)
               MOVE TR-CODE TO WS-TL-E-CODE (WS-TL-ROW-CNT)
               MOVE TR-DESC TO WS-TL-E-DESC (WS-TL-ROW-CNT)
               MOVE TR-EFF-YYDDD TO WS-TL-E-EFF (WS-TL-ROW-CNT)
               PERFORM P1400-NOTE-TABLE-ID THRU P1400-EXIT
           ELSE
               MOVE 'Y' TO WS-TL-FULL-SW
               ADD 1 TO WS-TL-DROP-CNT.
           PERFORM P1200-READ-ROW THRU P1200-EXIT.
       P1300-EXIT.
           EXIT.
       P1400-NOTE-TABLE-ID.
           MOVE TR-TABLE-ID TO WS-TL-TARGET-ID.
           MOVE 'N' TO WS-TL-SW-HIT.
           PERFORM P2100-MATCH-ID THRU P2100-EXIT
               VARYING WS-TL-SUB2 FROM 1 BY 1
               UNTIL WS-TL-SUB2 > WS-TL-ID-CNT
                  OR WS-TL-SW-HIT = 'Y'.
           IF WS-TL-SW-HIT = 'N' AND WS-TL-ID-CNT < WS-TL-MAX-ID
               ADD 1 TO WS-TL-ID-CNT
               MOVE TR-TABLE-ID TO WS-TL-ID-ENT (WS-TL-ID-CNT).
       P1400-EXIT.
           EXIT.
       P2100-MATCH-ID.
           IF WS-TL-ID-ENT (WS-TL-SUB2) = WS-TL-TARGET-ID
               MOVE 'Y' TO WS-TL-SW-HIT.
       P2100-EXIT.
           EXIT.
      * S200-SEARCH SECTION
       S200-SEARCH SECTION.
       P3000-SEARCH-TABLE.
           MOVE WS-TL-CUR-TBL TO WS-TL-SRCH-TBL.
           MOVE WS-TL-IN-CODE TO WS-TL-SRCH-CODE.
           MOVE 'N' TO WS-TL-SW-HIT.
           MOVE SPACES TO WS-TL-DESC.
           SEARCH ALL WS-TL-ENTRY
               AT END
                   MOVE 'N' TO WS-TL-SW-HIT
               WHEN WS-TL-E-KEY (WS-TL-IX) = WS-TL-SRCH-KEY
                   MOVE 'Y' TO WS-TL-SW-HIT
                   MOVE WS-TL-E-DESC (WS-TL-IX) TO WS-TL-DESC.
           IF WS-TL-SW-HIT = 'Y'
               MOVE WS-TL-DESC-8 TO LK-TL-CODE
               ADD 1 TO WS-TL-HIT-CNT
               MOVE 0 TO LK-TL-RC
           ELSE
               ADD 1 TO WS-TL-MISS-CNT
               MOVE 8 TO LK-TL-RC.
           IF WS-TL-SW-HIT = 'N' AND WS-TL-FULL-SW = 'Y'
               MOVE 20 TO LK-TL-RC.
       P3000-EXIT.
           EXIT.
      * S400-SEED - THE SEED ROWS ARE WALKED, NOT SEARCHED.
       S400-SEED SECTION.
       P4000-SEED-LOOKUP.
           MOVE 'N' TO WS-TL-SW-HIT.
           MOVE SPACES TO WS-TL-DESC.
           PERFORM P4100-MATCH-SEED THRU P4100-EXIT
               VARYING WS-TL-SUB FROM 1 BY 1
               UNTIL WS-TL-SUB > WS-TL-MAX-SEED
                  OR WS-TL-SW-HIT = 'Y'.
           IF WS-TL-SW-HIT = 'Y'
               MOVE WS-TL-DESC-8 TO LK-TL-CODE
               ADD 1 TO WS-TL-SEED-CNT
               MOVE 4 TO LK-TL-RC
           ELSE
               ADD 1 TO WS-TL-MISS-CNT
               MOVE 16 TO LK-TL-RC.
       P4000-EXIT.
           EXIT.
       P4100-MATCH-SEED.
           IF WS-TL-SD-TBL (WS-TL-SUB) = WS-TL-CUR-TBL
               IF WS-TL-SD-CODE (WS-TL-SUB) = WS-TL-IN-CODE
                   MOVE WS-TL-SD-DESC (WS-TL-SUB) TO WS-TL-DESC
                   MOVE 'Y' TO WS-TL-SW-HIT.
       P4100-EXIT.
           EXIT.
      * AN OP CALL SETS THE TABLE ID. A BLANK ID LEAVES 01 IN PLACE.
       P5000-STAGE-TABLE.
           ADD 1 TO WS-TL-STAGE-CNT.
           IF WS-TL-IN-TBL NOT = SPACES
               MOVE WS-TL-IN-TBL TO WS-TL-CUR-TBL.
           MOVE 0 TO LK-TL-RC.
       P5000-EXIT.
           EXIT.
      * S800-TALLY SECTION
       S800-TALLY SECTION.
       P8000-TALLY.
           ADD 1 TO WS-TL-TALLY-CNT.
           DISPLAY 'CABTBLLU ' WS-TL-VERSION ' - LOOKUP TALLY'.
           DISPLAY '  CALLS          = ' WS-TL-CALL-CNT.
           DISPLAY '  ROWS LOADED    = ' WS-TL-ROW-CNT.
           DISPLAY '  HITS           = ' WS-TL-HIT-CNT.
           DISPLAY '  MISSES         = ' WS-TL-MISS-CNT.
           DISPLAY '  IO STATUS BAD  = ' WS-TL-IOERR-CNT.
       P8000-EXIT.
           EXIT.
