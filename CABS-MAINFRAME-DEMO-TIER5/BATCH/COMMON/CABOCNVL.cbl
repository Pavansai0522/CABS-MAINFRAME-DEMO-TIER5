      *****************************************************************
      * CABOCNVL - OCN VALIDATION AND EFFECTIVITY                     *
      * APPLICATION : CABS                                            *
      * INVOKED BY  : CALL FROM INGEST AND RATING PROGRAMS            *
      * INPUTS      : LK-OV-OCN       FOUR CHARACTER OCN              *
      *               DDNAME  DSN                          COPYBOOK   *
      *               CARRTAB TELCABS.CABS.CARRTAB        (LOCAL)     *
      * OUTPUTS     : LK-OV-VALID-SW  VALIDITY BYTE                   *
      *               RETURN-CODE     CONDITION OF THE LOOKUP         *
      * CONTROL     : NONE - SUBPROGRAMS DO NOT WRITE CTLOUT,         *
      *               CABS-STD-041                                    *
      * BALANCE     : NONE - THE CALLING PROGRAM RECORD COUNTS ARE    *
      *               NOT TOUCHED BY THIS MODULE                      *
      * RESTART     : NONE - CARRTAB IS RE-READ ON EVERY RUN          *
      * REVISION HISTORY                                              *
      *   V1.00  1990-06-11  D.OKONKWO     INITIAL RELEASE            *
      *   V1.03  1993-09-27  R.T.WHEELER   EMBEDDED SEED TABLE USED   *
      *                      WHEN CARRTAB IS SHORT OR ABSENT          *
      *   V1.06  1995-03-02  S.MBEKI       EXPIRY OF ZERO IS OPEN END *
      *   V2.00  1997-08-15  A.BUKOWSKI    SECOND OPERAND SET TO THE  *
      *                      CHARACTER ZERO FOR BOTH READER STYLES    *
      *   V2.02  1999-12-06  E.KOWALCZYK   PIVOT OF SEVENTY APPLIED   *
      *                      TO THE TWO DIGIT YEAR COMPARISONS        *
      *   V2.05  2006-04-24  L.FERREIRA    EIGHT ENTRY MOST RECENTLY  *
      *                      USED CACHE AHEAD OF THE BINARY SEARCH    *
      *   V2.07  2012-02-13  P.NAIR        TABLE RAISED TO 1500 ROWS  *
      *   V2.09  2018-07-30  J.CALLAGHAN   SETTLEMENT SWITCH LOADED   *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABOCNVL.
       AUTHOR. TELCABS APPLICATIONS - CARRIER MASTER.
      *****************************************************************
      * THE MODULE ANSWERS ONE QUESTION - IS THIS OCN A CARRIER WE    *
      * KNOW AND IS IT EFFECTIVE FOR THE CYCLE BEING PROCESSED.  IT   *
      * WRITES NO FILE AND KEEPS NO COUNTS FOR THE CALLER, SO THE     *
      * CALLING PROGRAM BALANCE EQUATION IS UNAFFECTED BY IT.         *
      *                                                               *
      * THE SECOND OPERAND IS READ TWO WAYS ACROSS THE ESTATE.  SOME  *
      * CALL SITES TEST IT AS A SWITCH FOR Y OR N AND SOME TEST IT    *
      * AS A NUMERIC RETURN CODE WHERE ZERO MEANS VALID.  THE 1997    *
      * CHANGE SETTLED ON THE COMPROMISE OF MOVING THE CHARACTER      *
      * ZERO WHEN THE OCN IS GOOD - THAT BYTE READS AS ZERO FOR THE   *
      * NUMERIC SITES AND IS NOT Y FOR THE SWITCH SITES.  AN OCN      *
      * THAT DOES NOT PASS RETURNS THE CHARACTER N.                   *
      *                                                               *
      * RETURN CODES SET IN THE RETURN-CODE SPECIAL REGISTER          *
      *   0   VALID AND EFFECTIVE                                     *
      *   4   FOUND BUT OUTSIDE ITS EFFECTIVE WINDOW                  *
      *   8   FORMAT EDIT FAILED                                      *
      *  12   NOT IN THE CARRIER TABLE                                *
      *  16   CARRTAB NOT AVAILABLE - THE EMBEDDED SEED WAS USED      *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CARRTAB ASSIGN TO UT-S-CARRTAB
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CARRTAB.
       DATA DIVISION.
       FILE SECTION.
      * CARRTAB - FIXED BLOCKED 80, BUILT BY A SORT STEP IN THE JOB.
       FD  CARRTAB
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CARRTAB-RECORD.
           05  CB-IN-OCN                   PIC X(04).
           05  CB-IN-NAME                  PIC X(40).
           05  CB-IN-EFF-YYDDD             PIC 9(05).
           05  CB-IN-EXP-YYDDD             PIC 9(05).
           05  CB-IN-SETTLE-SW             PIC X(01).
           05  CB-IN-FILLER                PIC X(25).
       WORKING-STORAGE SECTION.
      * WORKING STORAGE IN A CALLED SUBPROGRAM SURVIVES FROM ONE CALL
      * TO THE NEXT INSIDE ONE RUN UNIT.  THE CARRIER TABLE, THE
      * CYCLE DATE AND THE CACHE ALL DEPEND ON THAT.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABOCNVL'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V2.09'.
           05  WS-TABLE-MAX                PIC S9(04) COMP-3
                                               VALUE 1500.
           05  WS-SEED-MAX                 PIC S9(04) COMP-3 VALUE 20.
           05  WS-CACHE-MAX                PIC S9(04) COMP-3 VALUE 8.
           05  WS-MIN-ROWS                 PIC S9(04) COMP-3 VALUE 20.
           05  WS-PIVOT-YY                 PIC 9(02) VALUE 70.
       01  WS-FS-CARRTAB                   PIC X(02) VALUE '  '.
       01  WS-SWITCH-AREA.
           05  WS-FIRST-SW                 PIC X(01) VALUE 'Y'.
               88  WS-FIRST-CALL               VALUE 'Y'.
           05  WS-SEED-SW                  PIC X(01) VALUE 'N'.
               88  WS-SEED-IN-USE              VALUE 'Y'.
           05  WS-EOF-SW                   PIC X(01) VALUE 'N'.
               88  WS-CARR-EOF                 VALUE 'Y'.
           05  WS-FMT-OK-SW                PIC X(01) VALUE 'N'.
               88  WS-FMT-OK                   VALUE 'Y'.
           05  WS-FOUND-SW                 PIC X(01) VALUE 'N'.
               88  WS-OCN-FOUND                VALUE 'Y'.
           05  WS-CACHE-HIT-SW             PIC X(01) VALUE 'N'.
               88  WS-CACHE-HIT                VALUE 'Y'.
           05  WS-EFFECTIVE-SW             PIC X(01) VALUE 'N'.
               88  WS-EFFECTIVE                VALUE 'Y'.
       01  WS-COUNT-AREA.
           05  WS-CALL-CNT                 PIC S9(09) COMP-3 VALUE 0.
           05  WS-VALID-CNT                PIC S9(09) COMP-3 VALUE 0.
           05  WS-WINDOW-CNT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-FORMAT-CNT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-NOTFOUND-CNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-CACHE-HIT-CNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-SEARCH-CNT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-ROWS-DROPPED             PIC S9(09) COMP-3 VALUE 0.
       01  WS-SUBSCRIPT-AREA.
           05  WS-SUB-01                   PIC S9(04) COMP-3 VALUE 0.
           05  WS-SUB-02                   PIC S9(04) COMP-3 VALUE 0.
           05  WS-CACHE-SUB                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CACHE-PTR                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CT-COUNT                 PIC S9(04) COMP-3 VALUE 0.
       01  WS-EDIT-AREA.
           05  WS-CNT-EDIT                 PIC ZZZ,ZZZ,ZZ9.
      * FORMAT EDIT WORK.  THE FOUR BYTES ARE WALKED ONE POSITION AT
      * A TIME AND A SHAPE STRING OF 9, A OR QUERY IS BUILT.  THE
      * THREE INDUSTRY SHAPES ARE FOUR DIGITS, ONE ALPHA AND THREE
      * DIGITS, OR TWO ALPHA AND TWO DIGITS.
       01  WS-OCN-WORK                     PIC X(04) VALUE SPACES.
       01  WS-OCN-CHARS REDEFINES WS-OCN-WORK.
           05  WS-OC-CHAR OCCURS 4 TIMES   PIC X(01).
       01  WS-SHAPE                        PIC X(04) VALUE SPACES.
       01  WS-SHAPE-CHARS REDEFINES WS-SHAPE.
           05  WS-SH-CHAR OCCURS 4 TIMES   PIC X(01).
       01  WS-INSPECT-AREA.
           05  WS-ONE-CHAR                 PIC X(01) VALUE SPACE.
           05  WS-DIG-TALLY                PIC S9(04) COMP-3 VALUE 0.
           05  WS-ALF-TALLY                PIC S9(04) COMP-3 VALUE 0.
      * CYCLE DATE STAGED ON THE FIRST CALL.  THE PARM CARD IS
      * POSITIONAL.  THE CALLING PROGRAMS READ THEIR OWN CARD IN
      * THEIR INITIALISATION, WHICH RUNS BEFORE THE FIRST CALL
      * REACHES THIS MODULE.
       01  WS-PARM-CARD                    PIC X(80) VALUE SPACES.
       01  WS-PARM-CARD-R1 REDEFINES WS-PARM-CARD.
           05  PC1-REC-ID                  PIC X(02).
           05  PC1-RUN-ID                  PIC X(12).
           05  PC1-CYCLE-YYDDD             PIC 9(05).
           05  PC1-BILL-PERIOD             PIC 9(06).
           05  PC1-REST                    PIC X(55).
       01  WS-TODAY-AREA.
           05  WS-TODAY-YYMMDD             PIC 9(06) VALUE 0.
       01  WS-TODAY-PARTS REDEFINES WS-TODAY-AREA.
           05  WS-TD-YY                    PIC 9(02).
           05  WS-TD-MM                    PIC 9(02).
           05  WS-TD-DD                    PIC 9(02).
       01  WS-MONTH-LIT.
           05  FILLER  PIC X(36) VALUE
               '000031059090120151181212243273304334'.
       01  WS-MONTH-TAB REDEFINES WS-MONTH-LIT.
           05  WS-MO-ENTRY OCCURS 12 TIMES.
               10  WS-MO-CUM               PIC 9(03).
       01  WS-DATE-WORK.
           05  WS-CYCLE-YYDDD              PIC 9(05) VALUE 0.
           05  WS-WORK-YYDDD               PIC 9(05) VALUE 0.
           05  WS-CYCLE-CCYYDDD            PIC 9(07) VALUE 0.
           05  WS-EFF-CCYYDDD              PIC 9(07) VALUE 0.
           05  WS-EXP-CCYYDDD              PIC 9(07) VALUE 0.
           05  WS-WORK-CCYYDDD             PIC 9(07) VALUE 0.
           05  WS-CCYY                     PIC 9(04) VALUE 0.
           05  WS-DDD                      PIC 9(03) VALUE 0.
           05  WS-QUOT                     PIC 9(05) VALUE 0.
           05  WS-REM                      PIC 9(02) VALUE 0.
       01  WS-YYDDD-SPLIT.
           05  WS-SP-YYDDD                 PIC 9(05) VALUE 0.
       01  WS-YYDDD-PARTS REDEFINES WS-YYDDD-SPLIT.
           05  WS-SP-YY                    PIC 9(02).
           05  WS-SP-DDD                   PIC 9(03).
      * THE LOADED CARRIER TABLE.  CARRTAB IS BUILT BY A SORT STEP
      * SO THE ROWS ARRIVE ASCENDING ON OCN AND THE BINARY SEARCH IS
      * SAFE HERE.  THE UTILITY PROGRAMS THAT READ THE SAME CARRIER
      * DATA BEFORE THE SORT WALK IT SERIALLY INSTEAD.  UNUSED ROWS
      * ARE SET TO HIGH VALUES SO THEY SORT ABOVE EVERY REAL OCN.
       01  WS-CARRIER-TABLE.
           05  WS-CT-ENTRY OCCURS 1500 TIMES
                   ASCENDING KEY IS WS-CT-OCN
                                       INDEXED BY WS-CT-IX.
               10  WS-CT-OCN               PIC X(04).
               10  WS-CT-NAME              PIC X(20).
               10  WS-CT-EFF               PIC 9(05).
               10  WS-CT-EXP               PIC 9(05).
               10  WS-CT-SETTLE            PIC X(01).
      * EMBEDDED SEED ADDED IN 1993.  IT CARRIES THE CARRIERS THAT
      * APPEAR ON EVERY CYCLE, IN THE COLLATING ORDER THE SORT STEP
      * PRODUCES, SO IT LOADS STRAIGHT INTO THE TABLE ABOVE.
       01  WS-SEED-LIT.
           05  FILLER  PIC X(27) VALUE 'AB12ACCESS INTGR9900100000Y'.
           05  FILLER  PIC X(27) VALUE 'A001PAETEC CORP 9800100000Y'.
           05  FILLER  PIC X(27) VALUE 'A115US LEC CORP 9700100000Y'.
           05  FILLER  PIC X(27) VALUE 'B220BROADWING   0000100000Y'.
           05  FILLER  PIC X(27) VALUE 'CD34CAVALIER TEL9812000000Y'.
           05  FILLER  PIC X(27) VALUE 'C333CBEYOND LLC 0100100000Y'.
           05  FILLER  PIC X(27) VALUE 'D404DELTACOM INC9600100000Y'.
           05  FILLER  PIC X(27) VALUE 'EF56ELECTRIC LTW9506000000Y'.
           05  FILLER  PIC X(27) VALUE 'E512EARTHLINK BZ0200100000Y'.
           05  FILLER  PIC X(27) VALUE 'GH78GRANDE COMM 9918000000Y'.
           05  FILLER  PIC X(27) VALUE '0222MCI TELECOM 8500100000N'.
           05  FILLER  PIC X(27) VALUE '0288AT&T COMM   8400100000N'.
           05  FILLER  PIC X(27) VALUE '0333SPRINT COMM 8600100000N'.
           05  FILLER  PIC X(27) VALUE '0432GLOBAL CROSS9500111365N'.
           05  FILLER  PIC X(27) VALUE '1234ALLNET COMM 8700193365N'.
           05  FILLER  PIC X(27) VALUE '2119QWEST COMM  9800100000N'.
           05  FILLER  PIC X(27) VALUE '5119VERIZON BUS 0600100000N'.
           05  FILLER  PIC X(27) VALUE '6010LEVEL 3 COMM9900100000N'.
           05  FILLER  PIC X(27) VALUE '7223XO COMM     0012000000N'.
           05  FILLER  PIC X(27) VALUE '9206TW TELECOM  0100100000Y'.
       01  WS-SEED-TAB REDEFINES WS-SEED-LIT.
           05  WS-SD-ENTRY OCCURS 20 TIMES.
               10  WS-SD-OCN               PIC X(04).
               10  WS-SD-NAME              PIC X(12).
               10  WS-SD-EFF               PIC 9(05).
               10  WS-SD-EXP               PIC 9(05).
               10  WS-SD-SETTLE            PIC X(01).
      * EIGHT ENTRY MOST RECENTLY USED CACHE ADDED IN 2006.  THE
      * USAGE FILES ARRIVE IN CARRIER ORDER SO A CONTIGUOUS RUN OF
      * ONE OCN IS ANSWERED WITHOUT RE-ENTERING THE BIG TABLE.
       01  WS-CACHE-TABLE.
           05  WS-MR-ENTRY OCCURS 8 TIMES.
               10  WS-MR-OCN               PIC X(04).
               10  WS-MR-EFF               PIC 9(05).
               10  WS-MR-EXP               PIC 9(05).
               10  WS-MR-SETTLE            PIC X(01).
       01  WS-FOUND-ROW.
           05  WS-FND-EFF                  PIC 9(05) VALUE 0.
           05  WS-FND-EXP                  PIC 9(05) VALUE 0.
           05  WS-FND-SETTLE               PIC X(01) VALUE SPACE.
       LINKAGE SECTION.
      * FIRST OPERAND.  SOME CALL SITES PASS A GROUP WHOSE LEADING
      * FOUR BYTES ARE THE OCN, SO A FOUR BYTE ITEM ADDRESSES
      * CORRECTLY IN EVERY CASE.
       01  LK-OV-OCN                       PIC X(04).
       01  LK-OV-VALID-SW                  PIC X(01).
       PROCEDURE DIVISION USING LK-OV-OCN LK-OV-VALID-SW.
      * P0000-ENTRY - ONE PASS PER CALL.
       P0000-ENTRY.
           MOVE 0 TO RETURN-CODE.
           ADD 1 TO WS-CALL-CNT.
           IF WS-FIRST-CALL
               PERFORM P1000-FIRST-CALL THRU P1000-EXIT.
           MOVE LK-OV-OCN TO WS-OCN-WORK.
           IF WS-OCN-WORK = '*END'
               PERFORM P8000-SUMMARY THRU P8000-EXIT
               GO TO P0000-RETURN.
           PERFORM P2000-FORMAT-EDIT THRU P2000-EXIT.
           IF NOT WS-FMT-OK
               MOVE 'N' TO LK-OV-VALID-SW
               ADD 1 TO WS-FORMAT-CNT
               MOVE 8 TO RETURN-CODE
               GO TO P0000-RETURN.
           PERFORM P3000-LOOK-IN-CACHE THRU P3000-EXIT.
           IF NOT WS-CACHE-HIT
               PERFORM P4000-SEARCH-TABLE THRU P4000-EXIT.
           IF NOT WS-OCN-FOUND
               MOVE 'N' TO LK-OV-VALID-SW
               ADD 1 TO WS-NOTFOUND-CNT
               MOVE 12 TO RETURN-CODE
               GO TO P0000-SEED-TEST.
           PERFORM P5000-TEST-EFFECTIVITY THRU P5000-EXIT.
           IF WS-EFFECTIVE
               MOVE '0' TO LK-OV-VALID-SW
               ADD 1 TO WS-VALID-CNT
               MOVE 0 TO RETURN-CODE
           ELSE
               MOVE 'N' TO LK-OV-VALID-SW
               ADD 1 TO WS-WINDOW-CNT
               MOVE 4 TO RETURN-CODE.
      * WHILE THE MODULE IS RUNNING ON THE EMBEDDED SEED THE
      * SIXTEEN IS RETURNED SO THE CALLER KNOWS THE ANSWER CAME
      * FROM THE SEED AND NOT FROM CARRTAB.  THE VALIDITY BYTE IS
      * STILL SET FROM THE SEED LOOKUP.
       P0000-SEED-TEST.
           IF WS-SEED-IN-USE
               MOVE 16 TO RETURN-CODE.
       P0000-RETURN.
           GOBACK.
      * S100-INITIALISATION SECTION
       S100-INITIALISATION SECTION.
       P1000-FIRST-CALL.
           MOVE 'N' TO WS-FIRST-SW.
           PERFORM P1050-CLEAR-TABLE THRU P1050-EXIT.
           PERFORM P1080-CLEAR-CACHE THRU P1080-EXIT.
           PERFORM P1100-STAGE-CYCLE THRU P1100-EXIT.
           PERFORM P1200-LOAD-TABLE THRU P1200-EXIT.
       P1000-EXIT.
           EXIT.
       P1050-CLEAR-TABLE.
           MOVE 0 TO WS-CT-COUNT.
           PERFORM P1055-CLEAR-ROW THRU P1055-EXIT
               VARYING WS-SUB-01 FROM 1 BY 1
               UNTIL WS-SUB-01 > WS-TABLE-MAX.
       P1050-EXIT.
           EXIT.
       P1055-CLEAR-ROW.
           MOVE HIGH-VALUES TO WS-CT-ENTRY (WS-SUB-01).
       P1055-EXIT.
           EXIT.
       P1080-CLEAR-CACHE.
           MOVE 1 TO WS-CACHE-PTR.
           PERFORM P1085-CLEAR-SLOT THRU P1085-EXIT
               VARYING WS-SUB-01 FROM 1 BY 1
               UNTIL WS-SUB-01 > WS-CACHE-MAX.
       P1080-EXIT.
           EXIT.
       P1085-CLEAR-SLOT.
           MOVE SPACES TO WS-MR-ENTRY (WS-SUB-01).
       P1085-EXIT.
           EXIT.
      * P1100-STAGE-CYCLE - THE CYCLE DATE IS TAKEN ONCE.  WHEN THE
      * CARD IS NOT POSITIONED THE SYSTEM DATE IS CONVERTED INSTEAD.
       P1100-STAGE-CYCLE.
           MOVE SPACES TO WS-PARM-CARD.
           ACCEPT WS-PARM-CARD FROM SYSIN.
           IF PC1-CYCLE-YYDDD IS NUMERIC AND PC1-CYCLE-YYDDD > 0
               MOVE PC1-CYCLE-YYDDD TO WS-CYCLE-YYDDD
           ELSE
               PERFORM P1150-CONVERT-TODAY THRU P1150-EXIT.
           MOVE WS-CYCLE-YYDDD TO WS-WORK-YYDDD.
           PERFORM P5100-PIVOT-YYDDD THRU P5100-EXIT.
           MOVE WS-WORK-CCYYDDD TO WS-CYCLE-CCYYDDD.
       P1100-EXIT.
           EXIT.
      * P1150-CONVERT-TODAY - GREGORIAN TO YYDDD.  FEBRUARY CARRIES
      * THE EXTRA DAY WHEN THE YEAR DIVIDES BY FOUR, WHICH HOLDS FOR
      * EVERY YEAR THE PIVOT OF SEVENTY CAN PRODUCE.
       P1150-CONVERT-TODAY.
           ACCEPT WS-TODAY-YYMMDD FROM DATE.
           MOVE WS-TD-YY TO WS-SP-YY.
           IF WS-TD-YY < WS-PIVOT-YY
               COMPUTE WS-CCYY = 2000 + WS-TD-YY
           ELSE
               COMPUTE WS-CCYY = 1900 + WS-TD-YY.
           MOVE 0 TO WS-DDD.
           IF WS-TD-MM > 0 AND WS-TD-MM < 13
               COMPUTE WS-DDD = WS-MO-CUM (WS-TD-MM) + WS-TD-DD.
           DIVIDE 4 INTO WS-CCYY GIVING WS-QUOT REMAINDER WS-REM.
           IF WS-REM = 0 AND WS-TD-MM > 2
               ADD 1 TO WS-DDD.
           COMPUTE WS-CYCLE-YYDDD = WS-TD-YY * 1000 + WS-DDD.
       P1150-EXIT.
           EXIT.
      * P1200-LOAD-TABLE - CARRTAB IS READ ONCE AND CLOSED AGAIN.
      * ROWS BEYOND THE TABLE LIMIT ARE COUNTED AND DROPPED.
       P1200-LOAD-TABLE.
           OPEN INPUT CARRTAB.
           IF WS-FS-CARRTAB NOT = '00'
               DISPLAY 'CABOCNVL - CARRTAB OPEN ' WS-FS-CARRTAB
               PERFORM P1300-LOAD-SEED THRU P1300-EXIT
               GO TO P1200-EXIT.
           PERFORM P1210-READ-CARRTAB THRU P1210-EXIT.
           PERFORM P1220-STORE-ROW THRU P1220-EXIT
               UNTIL WS-CARR-EOF.
           CLOSE CARRTAB.
           IF WS-FS-CARRTAB NOT = '00'
               DISPLAY 'CABOCNVL - CARRTAB CLOSE ' WS-FS-CARRTAB.
           IF WS-CT-COUNT < WS-MIN-ROWS
               PERFORM P1300-LOAD-SEED THRU P1300-EXIT.
       P1200-EXIT.
           EXIT.
       P1210-READ-CARRTAB.
           READ CARRTAB
               AT END MOVE 'Y' TO WS-EOF-SW.
           IF WS-FS-CARRTAB NOT = '00' AND WS-FS-CARRTAB NOT = '10'
               DISPLAY 'CABOCNVL - CARRTAB READ ' WS-FS-CARRTAB
               MOVE 'Y' TO WS-EOF-SW.
       P1210-EXIT.
           EXIT.
       P1220-STORE-ROW.
           IF WS-CT-COUNT NOT < WS-TABLE-MAX
               ADD 1 TO WS-ROWS-DROPPED
               GO TO P1220-READ-NEXT.
           ADD 1 TO WS-CT-COUNT.
           MOVE CB-IN-OCN TO WS-CT-OCN (WS-CT-COUNT).
           MOVE CB-IN-NAME TO WS-CT-NAME (WS-CT-COUNT).
           MOVE CB-IN-EFF-YYDDD TO WS-CT-EFF (WS-CT-COUNT).
           MOVE CB-IN-EXP-YYDDD TO WS-CT-EXP (WS-CT-COUNT).
           MOVE CB-IN-SETTLE-SW TO WS-CT-SETTLE (WS-CT-COUNT).
       P1220-READ-NEXT.
           PERFORM P1210-READ-CARRTAB THRU P1210-EXIT.
       P1220-EXIT.
           EXIT.
       P1300-LOAD-SEED.
           MOVE 'Y' TO WS-SEED-SW.
           PERFORM P1050-CLEAR-TABLE THRU P1050-EXIT.
           PERFORM P1310-SEED-ROW THRU P1310-EXIT
               VARYING WS-SUB-02 FROM 1 BY 1
               UNTIL WS-SUB-02 > WS-SEED-MAX.
           DISPLAY 'CABOCNVL - EMBEDDED SEED TABLE IN USE'.
       P1300-EXIT.
           EXIT.
       P1310-SEED-ROW.
           ADD 1 TO WS-CT-COUNT.
           MOVE WS-SD-OCN (WS-SUB-02) TO WS-CT-OCN (WS-CT-COUNT).
           MOVE WS-SD-NAME (WS-SUB-02) TO WS-CT-NAME (WS-CT-COUNT).
           MOVE WS-SD-EFF (WS-SUB-02) TO WS-CT-EFF (WS-CT-COUNT).
           MOVE WS-SD-EXP (WS-SUB-02) TO WS-CT-EXP (WS-CT-COUNT).
           MOVE WS-SD-SETTLE (WS-SUB-02) TO
               WS-CT-SETTLE (WS-CT-COUNT).
       P1310-EXIT.
           EXIT.
      * S200-FORMAT-EDIT SECTION - NO FILE IS TOUCHED HERE.  AN OCN
      * THAT DOES NOT TAKE ONE OF THE THREE INDUSTRY SHAPES IS
      * REJECTED BEFORE THE TABLE IS ENTERED.
       S200-FORMAT-EDIT SECTION.
       P2000-FORMAT-EDIT.
           MOVE SPACES TO WS-SHAPE.
           PERFORM P2100-CLASSIFY-CHAR THRU P2100-EXIT
               VARYING WS-SUB-01 FROM 1 BY 1
               UNTIL WS-SUB-01 > 4.
           IF WS-SHAPE = '9999' OR WS-SHAPE = 'A999' OR
              WS-SHAPE = 'AA99'
               MOVE 'Y' TO WS-FMT-OK-SW
           ELSE
               MOVE 'N' TO WS-FMT-OK-SW.
       P2000-EXIT.
           EXIT.
       P2100-CLASSIFY-CHAR.
           MOVE WS-OC-CHAR (WS-SUB-01) TO WS-ONE-CHAR.
           MOVE 0 TO WS-DIG-TALLY.
           INSPECT WS-ONE-CHAR TALLYING WS-DIG-TALLY
               FOR ALL '0' ALL '1' ALL '2' ALL '3' ALL '4'
               ALL '5' ALL '6' ALL '7' ALL '8' ALL '9'.
           MOVE 0 TO WS-ALF-TALLY.
           INSPECT WS-ONE-CHAR TALLYING WS-ALF-TALLY
               FOR ALL 'A' ALL 'B' ALL 'C' ALL 'D' ALL 'E' ALL 'F'
               ALL 'G' ALL 'H' ALL 'I' ALL 'J' ALL 'K' ALL 'L'
               ALL 'M' ALL 'N' ALL 'O' ALL 'P' ALL 'Q' ALL 'R'
               ALL 'S' ALL 'T' ALL 'U' ALL 'V' ALL 'W' ALL 'X'
               ALL 'Y' ALL 'Z'.
           IF WS-DIG-TALLY > 0
               MOVE '9' TO WS-SH-CHAR (WS-SUB-01)
           ELSE
               IF WS-ALF-TALLY > 0
                   MOVE 'A' TO WS-SH-CHAR (WS-SUB-01)
               ELSE
                   MOVE '?' TO WS-SH-CHAR (WS-SUB-01).
       P2100-EXIT.
           EXIT.
      * S300-CACHE SECTION - THE EIGHT SLOTS ARE WALKED SERIALLY.
      * EIGHT COMPARES COST LESS THAN THE BIG TABLE FOR A RUN OF ONE
      * CARRIER.
       S300-CACHE SECTION.
       P3000-LOOK-IN-CACHE.
           MOVE 'N' TO WS-CACHE-HIT-SW.
           MOVE 'N' TO WS-FOUND-SW.
           MOVE 1 TO WS-CACHE-SUB.
           PERFORM P3100-TEST-SLOT THRU P3100-EXIT
               UNTIL WS-CACHE-SUB > WS-CACHE-MAX OR WS-CACHE-HIT.
           IF WS-CACHE-HIT
               ADD 1 TO WS-CACHE-HIT-CNT
               MOVE 'Y' TO WS-FOUND-SW.
       P3000-EXIT.
           EXIT.
       P3100-TEST-SLOT.
           IF WS-MR-OCN (WS-CACHE-SUB) = WS-OCN-WORK
               MOVE 'Y' TO WS-CACHE-HIT-SW
               MOVE WS-MR-EFF (WS-CACHE-SUB) TO WS-FND-EFF
               MOVE WS-MR-EXP (WS-CACHE-SUB) TO WS-FND-EXP
               MOVE WS-MR-SETTLE (WS-CACHE-SUB) TO WS-FND-SETTLE
           ELSE
               ADD 1 TO WS-CACHE-SUB.
       P3100-EXIT.
           EXIT.
      * P3200-ADD-TO-CACHE - THE OLDEST SLOT IS REPLACED THROUGH A
      * WRAP ROUND SLOT INDEX.
       P3200-ADD-TO-CACHE.
           IF WS-CACHE-PTR < 1 OR WS-CACHE-PTR > WS-CACHE-MAX
               MOVE 1 TO WS-CACHE-PTR.
           MOVE WS-OCN-WORK TO WS-MR-OCN (WS-CACHE-PTR).
           MOVE WS-FND-EFF TO WS-MR-EFF (WS-CACHE-PTR).
           MOVE WS-FND-EXP TO WS-MR-EXP (WS-CACHE-PTR).
           MOVE WS-FND-SETTLE TO WS-MR-SETTLE (WS-CACHE-PTR).
           ADD 1 TO WS-CACHE-PTR.
           IF WS-CACHE-PTR > WS-CACHE-MAX
               MOVE 1 TO WS-CACHE-PTR.
       P3200-EXIT.
           EXIT.
      * S400-TABLE-SEARCH SECTION
       S400-TABLE-SEARCH SECTION.
       P4000-SEARCH-TABLE.
           ADD 1 TO WS-SEARCH-CNT.
           MOVE 'N' TO WS-FOUND-SW.
           MOVE 0 TO WS-FND-EFF.
           MOVE 0 TO WS-FND-EXP.
           MOVE SPACE TO WS-FND-SETTLE.
           SEARCH ALL WS-CT-ENTRY
               AT END
                   MOVE 'N' TO WS-FOUND-SW
               WHEN WS-CT-OCN (WS-CT-IX) = WS-OCN-WORK
                   MOVE 'Y' TO WS-FOUND-SW
                   MOVE WS-CT-EFF (WS-CT-IX) TO WS-FND-EFF
                   MOVE WS-CT-EXP (WS-CT-IX) TO WS-FND-EXP
                   MOVE WS-CT-SETTLE (WS-CT-IX) TO WS-FND-SETTLE.
           IF WS-OCN-FOUND
               PERFORM P3200-ADD-TO-CACHE THRU P3200-EXIT.
       P4000-EXIT.
           EXIT.
      * S500-EFFECTIVITY SECTION - THE WINDOW IS INCLUSIVE AT BOTH
      * ENDS AND AN EXPIRY OF ZERO IS OPEN ENDED.
       S500-EFFECTIVITY SECTION.
       P5000-TEST-EFFECTIVITY.
           MOVE 'N' TO WS-EFFECTIVE-SW.
           MOVE WS-FND-EFF TO WS-WORK-YYDDD.
           PERFORM P5100-PIVOT-YYDDD THRU P5100-EXIT.
           MOVE WS-WORK-CCYYDDD TO WS-EFF-CCYYDDD.
           MOVE WS-FND-EXP TO WS-WORK-YYDDD.
           IF WS-FND-EXP = 0
               MOVE 9999999 TO WS-EXP-CCYYDDD
           ELSE
               PERFORM P5100-PIVOT-YYDDD THRU P5100-EXIT
               MOVE WS-WORK-CCYYDDD TO WS-EXP-CCYYDDD.
           IF WS-CYCLE-CCYYDDD NOT < WS-EFF-CCYYDDD
               IF WS-CYCLE-CCYYDDD NOT > WS-EXP-CCYYDDD
                   MOVE 'Y' TO WS-EFFECTIVE-SW.
       P5000-EXIT.
           EXIT.
      * P5100-PIVOT-YYDDD - ESTATE PIVOT OF SEVENTY.  A TWO DIGIT
      * YEAR BELOW SEVENTY IS TWENTY HUNDREDS.
       P5100-PIVOT-YYDDD.
           MOVE WS-WORK-YYDDD TO WS-SP-YYDDD.
           IF WS-SP-YY < WS-PIVOT-YY
               COMPUTE WS-CCYY = 2000 + WS-SP-YY
           ELSE
               COMPUTE WS-CCYY = 1900 + WS-SP-YY.
           COMPUTE WS-WORK-CCYYDDD = WS-CCYY * 1000 + WS-SP-DDD.
       P5100-EXIT.
           EXIT.
      * S800-SUMMARY SECTION - DRIVEN BY AN OCN OF *END.
       S800-SUMMARY SECTION.
       P8000-SUMMARY.
           DISPLAY 'CABOCNVL ' WS-PGM-VERSION ' - OCN EDIT SUMMARY'.
           DISPLAY '  CALLS RECEIVED   = ' WS-CALL-CNT.
           DISPLAY '  VALID EFFECTIVE  = ' WS-VALID-CNT.
           DISPLAY '  FORMAT REJECTED  = ' WS-FORMAT-CNT.
           DISPLAY '  NOT IN TABLE     = ' WS-NOTFOUND-CNT.
           DISPLAY '  CACHE HITS       = ' WS-CACHE-HIT-CNT.
           DISPLAY '  CYCLE CCYYDDD    = ' WS-CYCLE-CCYYDDD
               '  SEED IN USE = ' WS-SEED-SW.
           MOVE '0' TO LK-OV-VALID-SW.
           MOVE 0 TO RETURN-CODE.
       P8000-EXIT.
           EXIT.
