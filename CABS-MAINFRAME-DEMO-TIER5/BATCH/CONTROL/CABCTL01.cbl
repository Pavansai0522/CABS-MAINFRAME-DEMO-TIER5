       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABCTL01.
      *****************************************************************
      * CABCTL01 - CARRIER DATABASE EXTRACT AND PROFILE LISTING       *
      * APPLICATION : CABS                                            *
      * COMPILER    : ENTERPRISE COBOL                                *
      * INPUTS      : IMS PCB 1  CABCARDB  PSB CABCT01P  PROCOPT G    *
      *               PARMIN  INSTREAM SYSIN PARM CARD      FB 080    *
      * OUTPUTS     : CAREXT  TELCABS.CABS.CARRIER.EXTRACT(+1) FB 400 *
      *               RPTOUT  SYSOUT PRINT                  FBA 133   *
      * CONTROL     : CTLOUT                                CABSCTL   *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED               *
      *                       + CT-SUMMARISED + CT-CARRIED-FWD        *
      * RESTART     : FULL RERUN - THE EXTRACT IS CUT FROM SCRATCH    *
      *                                                               *
      * THE CARRIER PROFILE LIVES IN IMS AND THE CARRIER MASTER       *
      * LIVES IN VSAM.  THE TWO WERE MEANT TO BE MERGED WHEN THE      *
      * 1994 CONVERSION COMPLETED.  THE MERGE WAS DEFERRED AND THE    *
      * EXTRACT THIS PROGRAM CUTS IS WHAT THE DOWNSTREAM SYSTEMS      *
      * READ INSTEAD - SEE CABS-STD-021.                              *
      *                                                               *
      * REVISION HISTORY                                              *
      *   V1.00  1991-02-18  D.OKONKWO     INITIAL                    *
      *   V1.04  1994-09-07  J.M.CASTILLO  AGREEMENT SEGMENT ADDED    *
      *   V1.08  1999-05-25  P.NAIR        FACTOR SEGMENT WALKED IN   *
      *                                    EFFECTIVE DATE ORDER       *
      *   V2.00  2006-03-02  A.BUKOWSKI    RECOMPILED FOR IMS V9      *
      *   V2.02  2013-10-29  L.FERREIRA    TYPE FILTER FROM PARM      *
      *   V2.03  2018-06-11  M.HAAS        RECOMPILE ONLY - IMS V14   *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PARM-FILE     ASSIGN TO PARMIN
                  FILE STATUS IS WS-FS-INPUT.
           SELECT EXTRACT-FILE  ASSIGN TO CAREXT
                  FILE STATUS IS WS-FS-OUTPUT.
           SELECT CONTROL-FILE  ASSIGN TO CTLOUT
                  FILE STATUS IS WS-FS-CONTROL.
           SELECT REPORT-FILE   ASSIGN TO RPTOUT.
       DATA DIVISION.
       FILE SECTION.
       FD  PARM-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE OMITTED.
       01  PARM-CARD                   PIC X(80).
       FD  EXTRACT-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  EXTRACT-RECORD              PIC X(400).
       FD  CONTROL-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  CONTROL-RECORD              PIC X(180).
       FD  REPORT-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE OMITTED.
       01  REPORT-LINE                 PIC X(133).
       WORKING-STORAGE SECTION.
       01  WS-PGM-NAME                 PIC X(08) VALUE 'CABCTL01'.
       01  WS-PARA-NAME                PIC X(30) VALUE SPACES.
       COPY CABSWRK.
       COPY CABSCARR.
       COPY CABSPRNT.
      *
      * DL/I FUNCTION CODES.  FOUR BYTES, TRAILING BLANKS SIGNIFICANT.
      *
       01  WS-DLI-FUNCTIONS.
           05  DLI-GU                  PIC X(04) VALUE 'GU  '.
           05  DLI-GHU                 PIC X(04) VALUE 'GHU '.
           05  DLI-GN                  PIC X(04) VALUE 'GN  '.
           05  DLI-GHN                 PIC X(04) VALUE 'GHN '.
           05  DLI-GNP                 PIC X(04) VALUE 'GNP '.
           05  DLI-GHNP                PIC X(04) VALUE 'GHNP'.
           05  DLI-ISRT                PIC X(04) VALUE 'ISRT'.
           05  DLI-REPL                PIC X(04) VALUE 'REPL'.
           05  DLI-DLET                PIC X(04) VALUE 'DLET'.
      *
      * SEGMENT SEARCH ARGUMENTS.
      *
       01  SSA-CARRSEG-U.
           05  FILLER                  PIC X(08) VALUE 'CARRSEG '.
           05  FILLER                  PIC X(01) VALUE SPACE.
       01  SSA-CARRSEG-Q.
           05  FILLER                  PIC X(08) VALUE 'CARRSEG '.
           05  FILLER                  PIC X(01) VALUE '('.
           05  FILLER                  PIC X(08) VALUE 'CROCN   '.
           05  FILLER                  PIC X(02) VALUE ' ='.
           05  SSA-CS-OCN              PIC X(04) VALUE SPACES.
           05  FILLER                  PIC X(01) VALUE ')'.
       01  SSA-CARRBILL-U.
           05  FILLER                  PIC X(08) VALUE 'CARRBILL'.
           05  FILLER                  PIC X(01) VALUE SPACE.
       01  SSA-CARRFACT-U.
           05  FILLER                  PIC X(08) VALUE 'CARRFACT'.
           05  FILLER                  PIC X(01) VALUE SPACE.
       01  SSA-CARRFACT-Q.
           05  FILLER                  PIC X(08) VALUE 'CARRFACT'.
           05  FILLER                  PIC X(01) VALUE '('.
           05  FILLER                  PIC X(08) VALUE 'CRFEFFDT'.
           05  FILLER                  PIC X(02) VALUE '>='.
           05  SSA-CF-EFFDT            PIC 9(05) VALUE ZERO.
           05  FILLER                  PIC X(01) VALUE ')'.
       01  SSA-CARRAGMT-U.
           05  FILLER                  PIC X(08) VALUE 'CARRAGMT'.
           05  FILLER                  PIC X(01) VALUE SPACE.
      *
      * SEGMENT I/O AREAS.
      *
       01  WS-CARRSEG-IO.
           05  CS-OCN                  PIC X(04).
           05  CS-NAME                 PIC X(40).
           05  CS-ACNA                 PIC X(03).
           05  CS-TYPE                 PIC X(01).
               88  CS-IXC              VALUE 'I'.
               88  CS-CLEC             VALUE 'C'.
               88  CS-ILEC             VALUE 'L'.
               88  CS-WIRELESS         VALUE 'W'.
               88  CS-RESELLER         VALUE 'R'.
           05  CS-CIC                  PIC 9(04).
           05  CS-PARENT-OCN           PIC X(04).
           05  CS-STATUS               PIC X(01).
           05  CS-EFF-YYDDD            PIC 9(05).
           05  CS-EXP-YYDDD            PIC 9(05).
           05  CS-CONTACT              PIC X(40).
           05  CS-PHONE                PIC X(12).
           05  CS-UPD-YYDDD            PIC 9(05).
           05  CS-UPD-USER             PIC X(08).
           05  CS-FILLER               PIC X(48).
       01  WS-CARRBILL-IO.
           05  CB-CYCLE                PIC 9(02).
           05  CB-MEDIA                PIC X(01).
           05  CB-CURRENCY             PIC X(03).
           05  CB-TERMS-DAYS           PIC 9(03).
           05  CB-CREDIT-LIMIT         PIC S9(11)V9(02) COMP-3.
           05  CB-INVOICE-PREFIX       PIC X(04).
           05  CB-REMIT-ADDR           PIC X(35).
           05  CB-FILLER               PIC X(07).
       01  WS-CARRFACT-IO.
           05  CF-EFF-YYDDD            PIC 9(05).
           05  CF-STATE-CD             PIC X(02).
           05  CF-LATA                 PIC 9(03).
           05  CF-PIU                  PIC S9(03)V9(05) COMP-3.
           05  CF-PLU                  PIC S9(03)V9(05) COMP-3.
           05  CF-PSU                  PIC S9(03)V9(05) COMP-3.
           05  CF-SOURCE               PIC X(01).
           05  CF-RESTATE-SW           PIC X(01).
           05  CF-RECV-YYDDD           PIC 9(05).
           05  CF-FILLER               PIC X(57).
       01  WS-CARRAGMT-IO.
           05  CA-AGMT-ID              PIC X(08).
           05  CA-AGMT-TYPE            PIC X(02).
           05  CA-EFF-YYDDD            PIC 9(05).
           05  CA-EXP-YYDDD            PIC 9(05).
           05  CA-RECIP-RATE           PIC S9(05)V9(05) COMP-3.
           05  CA-ISP-CAP-MOU          PIC S9(13) COMP-3.
           05  CA-MPB-ELIG             PIC X(01).
           05  CA-STATE-CD             PIC X(02).
           05  CA-FILLER               PIC X(85).
      *
      * THE EXTRACT RECORD.  ONE PER CARRIER, FLATTENING THE FOUR
      * SEGMENTS INTO 400 BYTES.  ONLY THE FIRST THREE FACTOR ROWS
      * AND THE FIRST TWO AGREEMENTS FIT.
      *
       01  WS-EXTRACT-AREA.
           05  WE-OCN                  PIC X(04).
           05  WE-NAME                 PIC X(40).
           05  WE-ACNA                 PIC X(03).
           05  WE-TYPE                 PIC X(01).
           05  WE-CIC                  PIC 9(04).
           05  WE-STATUS               PIC X(01).
           05  WE-EFF-YYDDD            PIC 9(05).
           05  WE-EXP-YYDDD            PIC 9(05).
           05  WE-BILL-CYCLE           PIC 9(02).
           05  WE-BILL-MEDIA           PIC X(01).
           05  WE-TERMS-DAYS           PIC 9(03).
           05  WE-CREDIT-LIMIT         PIC S9(11)V9(02) COMP-3.
           05  WE-FACTOR-CNT           PIC 9(02).
           05  WE-FACTOR OCCURS 3 TIMES
                    INDEXED BY WE-FX.
               10  WE-FC-EFF-YYDDD     PIC 9(05).
               10  WE-FC-STATE         PIC X(02).
               10  WE-FC-LATA          PIC 9(03).
               10  WE-FC-PIU           PIC S9(03)V9(05) COMP-3.
               10  WE-FC-PLU           PIC S9(03)V9(05) COMP-3.
               10  WE-FC-SOURCE        PIC X(01).
           05  WE-AGMT-CNT             PIC 9(02).
           05  WE-AGMT OCCURS 2 TIMES
                    INDEXED BY WE-AX.
               10  WE-AG-ID            PIC X(08).
               10  WE-AG-TYPE          PIC X(02).
               10  WE-AG-EFF-YYDDD     PIC 9(05).
               10  WE-AG-RECIP-RATE    PIC S9(05)V9(05) COMP-3.
               10  WE-AG-MPB-ELIG      PIC X(01).
           05  WE-EXTRACT-YYDDD        PIC 9(05).
           05  WE-SRC-PGM              PIC X(08).
           05  WE-FILLER               PIC X(213).
      *
      * PARM CARD.  THE TYPE FILTER IS OPTIONAL - SPACES MEANS ALL.
      *
       01  WS-PARM-AREA.
           05  WP-RUN-ID               PIC X(12).
           05  WP-CYCLE-YYDDD          PIC 9(05).
           05  WP-BILL-PERIOD          PIC 9(06).
           05  WP-TYPE-FILTER          PIC X(01).
           05  WP-EFF-FROM             PIC 9(05).
           05  WP-FILLER               PIC X(51).
      *
       01  WS-SWITCHES-LOCAL.
           05  WS-DB-EOF-SW            PIC X(01) VALUE 'N'.
               88  WS-DB-EOF           VALUE 'Y'.
           05  WS-CHILD-EOF-SW         PIC X(01) VALUE 'N'.
               88  WS-CHILD-EOF        VALUE 'Y'.
           05  WS-SELECT-SW            PIC X(01) VALUE 'Y'.
               88  WS-SELECTED         VALUE 'Y'.
       01  WS-DLI-STATUS               PIC X(02) VALUE SPACES.
           88  WS-DLI-OK               VALUE '  '.
           88  WS-DLI-NOT-FOUND        VALUE 'GE'.
           88  WS-DLI-END-OF-DB        VALUE 'GB'.
           88  WS-DLI-NO-MORE-CHILD    VALUE 'GE' 'GB' 'GA' 'GK'.
       01  WS-LOCAL-COUNTS.
           05  WS-CARR-CNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-FACT-CNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-AGMT-CNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-BILL-CNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-FACT-DROPPED         PIC S9(09) COMP-3 VALUE 0.
           05  WS-AGMT-DROPPED         PIC S9(09) COMP-3 VALUE 0.
       01  WS-PAGE-CONTROL.
           05  WS-LINE-CNT             PIC 9(03) VALUE 99.
           05  WS-PAGE-CNT             PIC 9(05) VALUE 0.
       01  WS-EDIT-FIELDS.
           05  WS-ED-PIU               PIC ZZ9.99999.
           05  WS-ED-PLU               PIC ZZ9.99999.
           05  WS-ED-COUNT             PIC ZZZ,ZZZ,ZZ9.
      *
       LINKAGE SECTION.
       01  DB-PCB-CARR.
           05  DBP-DBD-NAME            PIC X(08).
           05  DBP-SEG-LEVEL           PIC X(02).
           05  DBP-STATUS-CODE         PIC X(02).
           05  DBP-PROC-OPTIONS        PIC X(04).
           05  DBP-RESERVED            PIC S9(05) COMP.
           05  DBP-SEG-NAME-FB         PIC X(08).
           05  DBP-KEY-LENGTH          PIC S9(05) COMP.
           05  DBP-NUMB-SENS-SEGS      PIC S9(05) COMP.
           05  DBP-KEY-FB              PIC X(30).
      *
       PROCEDURE DIVISION USING DB-PCB-CARR.
       P0000-MAINLINE.
           PERFORM P1000-INIT THRU P1000-EXIT.
           PERFORM P2000-PROCESS THRU P2000-EXIT
               UNTIL WS-DB-EOF.
           PERFORM P8000-CONTROL THRU P8000-EXIT.
           PERFORM P9000-TERM THRU P9000-EXIT.
           GOBACK.

      *****************************************************************
      * S100-INITIALISATION                                           *
      *****************************************************************
       S100-INITIALISATION SECTION.

       P1000-INIT.
           MOVE 'P1000-INIT' TO WS-PARA-NAME.
           OPEN INPUT PARM-FILE.
           OPEN OUTPUT EXTRACT-FILE.
           OPEN OUTPUT CONTROL-FILE.
           OPEN OUTPUT REPORT-FILE.
           PERFORM P1200-READ-PARM THRU P1200-EXIT.
           MOVE ZERO TO WS-READ-CNT.
           MOVE ZERO TO WS-WRITE-CNT.
           MOVE ZERO TO WS-REJECT-CNT.
           MOVE ZERO TO WS-SUMM-CNT.
           MOVE ZERO TO WS-CFWD-CNT.
           PERFORM P1400-FIRST-ROOT THRU P1400-EXIT.

       P1000-EXIT.
           EXIT.

       P1200-READ-PARM.
      * ONE CARD.  A MISSING CARD IS FATAL - THERE IS NO DEFAULT FOR
      * THE RUN IDENTIFIER AND THE CONTROL RECORD CANNOT BE KEYED
      * WITHOUT IT.
           MOVE SPACES TO PARM-CARD.
           READ PARM-FILE
               AT END
                   MOVE 'NO PARM CARD SUPPLIED' TO PC-TEXT
                   PERFORM P9600-PRINT THRU P9600-EXIT
                   MOVE 8 TO RETURN-CODE
                   GOBACK
           END-READ.
           MOVE PARM-CARD TO WS-PARM-AREA.
           MOVE WP-CYCLE-YYDDD TO DW-CURRENT-YYDDD.

       P1200-EXIT.
           EXIT.

       P1400-FIRST-ROOT.
      * UNQUALIFIED GU POSITIONS ON THE FIRST ROOT IN PHYSICAL
      * SEQUENCE.  UNDER HDAM THAT IS RANDOMISER ORDER, NOT KEY
      * ORDER, SO THE EXTRACT IS NOT PRODUCED IN OCN SEQUENCE.
      * THE DOWNSTREAM SORT PUTS IT RIGHT.
           MOVE 'P1400-FIRST-ROOT' TO WS-PARA-NAME.
           CALL 'CBLTDLI' USING DLI-GU
                                DB-PCB-CARR
                                WS-CARRSEG-IO
                                SSA-CARRSEG-U.
           MOVE DBP-STATUS-CODE TO WS-DLI-STATUS.
           IF WS-DLI-OK
               ADD 1 TO WS-READ-CNT
               ADD 1 TO WS-CARR-CNT
           ELSE
               IF WS-DLI-END-OF-DB
                   MOVE 'Y' TO WS-DB-EOF-SW
               ELSE
                   PERFORM P9500-DLI-ERROR THRU P9500-EXIT
               END-IF
           END-IF.

       P1400-EXIT.
           EXIT.

      *****************************************************************
      * S200-MAIN-PROCESS                                             *
      *****************************************************************
       S200-MAIN-PROCESS SECTION.

       P2000-PROCESS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2200-SELECT-TEST THRU P2200-EXIT.
           IF WS-SELECTED
               PERFORM P3000-BUILD-EXTRACT THRU P3000-EXIT
               PERFORM P6000-WRITE-EXTRACT THRU P6000-EXIT
           ELSE
               ADD 1 TO WS-REJECT-CNT
           END-IF.
           PERFORM P2800-NEXT-ROOT THRU P2800-EXIT.

       P2000-EXIT.
           EXIT.

       P2200-SELECT-TEST.
      * THE TYPE FILTER ON THE PARM CARD RESTRICTS THE EXTRACT TO ONE
      * CARRIER TYPE.  SPACES MEANS EVERY TYPE.  AN EXPIRED CARRIER
      * IS STILL EXTRACTED BECAUSE THE SETTLEMENT HISTORY REPORTS
      * NEED THE NAME LONG AFTER THE AGREEMENT HAS ENDED.
           MOVE 'Y' TO WS-SELECT-SW.
           IF WP-TYPE-FILTER NOT = SPACE
               IF CS-TYPE NOT = WP-TYPE-FILTER
                   MOVE 'N' TO WS-SELECT-SW
               END-IF
           END-IF.

       P2200-EXIT.
           EXIT.

       P2800-NEXT-ROOT.
      * GN WITH AN UNQUALIFIED ROOT SSA STEPS TO THE NEXT ROOT AND
      * SKIPS ANY DEPENDENT SEGMENTS LEFT UNREAD.
           MOVE 'P2800-NEXT-ROOT' TO WS-PARA-NAME.
           CALL 'CBLTDLI' USING DLI-GN
                                DB-PCB-CARR
                                WS-CARRSEG-IO
                                SSA-CARRSEG-U.
           MOVE DBP-STATUS-CODE TO WS-DLI-STATUS.
           IF WS-DLI-OK
               ADD 1 TO WS-READ-CNT
               ADD 1 TO WS-CARR-CNT
               GO TO P2800-EXIT
           END-IF.
           IF WS-DLI-END-OF-DB
               MOVE 'Y' TO WS-DB-EOF-SW
               GO TO P2800-EXIT
           END-IF.
           PERFORM P9500-DLI-ERROR THRU P9500-EXIT.

       P2800-EXIT.
           EXIT.

      *****************************************************************
      * S300-SEGMENT-WALK                                             *
      *****************************************************************
       S300-SEGMENT-WALK SECTION.

       P3000-BUILD-EXTRACT.
           MOVE 'P3000-BUILD-EXTRACT' TO WS-PARA-NAME.
           MOVE SPACES TO WS-EXTRACT-AREA.
           MOVE ZERO TO WE-FACTOR-CNT.
           MOVE ZERO TO WE-AGMT-CNT.
           MOVE CS-OCN         TO WE-OCN.
           MOVE CS-NAME        TO WE-NAME.
           MOVE CS-ACNA        TO WE-ACNA.
           MOVE CS-TYPE        TO WE-TYPE.
           MOVE CS-CIC         TO WE-CIC.
           MOVE CS-STATUS      TO WE-STATUS.
           MOVE CS-EFF-YYDDD   TO WE-EFF-YYDDD.
           MOVE CS-EXP-YYDDD   TO WE-EXP-YYDDD.
           MOVE ZERO           TO WE-BILL-CYCLE.
           MOVE SPACE          TO WE-BILL-MEDIA.
           MOVE ZERO           TO WE-TERMS-DAYS.
           MOVE ZERO           TO WE-CREDIT-LIMIT.
           MOVE WP-CYCLE-YYDDD TO WE-EXTRACT-YYDDD.
           MOVE WS-PGM-NAME    TO WE-SRC-PGM.
           MOVE CS-OCN         TO SSA-CS-OCN.
           PERFORM P3200-GET-BILLING THRU P3200-EXIT.
           PERFORM P3400-GET-FACTORS THRU P3400-EXIT.
           PERFORM P3600-GET-AGREEMENTS THRU P3600-EXIT.

       P3000-EXIT.
           EXIT.

       P3200-GET-BILLING.
      * ONE BILLING SEGMENT PER CARRIER IN PRACTICE, THOUGH THE
      * DATABASE ALLOWS MORE THAN ONE.  THE FIRST IS TAKEN.
           MOVE 'P3200-GET-BILLING' TO WS-PARA-NAME.
           CALL 'CBLTDLI' USING DLI-GNP
                                DB-PCB-CARR
                                WS-CARRBILL-IO
                                SSA-CARRBILL-U.
           MOVE DBP-STATUS-CODE TO WS-DLI-STATUS.
           IF WS-DLI-OK
               MOVE CB-CYCLE        TO WE-BILL-CYCLE
               MOVE CB-MEDIA        TO WE-BILL-MEDIA
               MOVE CB-TERMS-DAYS   TO WE-TERMS-DAYS
               MOVE CB-CREDIT-LIMIT TO WE-CREDIT-LIMIT
               ADD 1 TO WS-BILL-CNT
               GO TO P3200-EXIT
           END-IF.
           IF WS-DLI-NO-MORE-CHILD
               GO TO P3200-EXIT
           END-IF.
           PERFORM P9500-DLI-ERROR THRU P9500-EXIT.

       P3200-EXIT.
           EXIT.

       P3400-GET-FACTORS.
      * WALK THE FACTOR SEGMENTS FROM THE EFFECTIVE DATE ON THE PARM
      * CARD FORWARD.  ONLY THE FIRST THREE ARE CARRIED ON THE
      * EXTRACT.  A CARRIER WITH MORE THAN THREE ACTIVE STATE OR
      * LATA COMBINATIONS IS TRUNCATED AND THE COUNT OF THOSE IS
      * REPORTED AT END OF RUN.
           MOVE 'P3400-GET-FACTORS' TO WS-PARA-NAME.
           MOVE WP-EFF-FROM TO SSA-CF-EFFDT.
           MOVE 'N' TO WS-CHILD-EOF-SW.
           SET WE-FX TO 1.
           PERFORM P3450-NEXT-FACTOR THRU P3450-EXIT
               UNTIL WS-CHILD-EOF.

       P3400-EXIT.
           EXIT.

       P3450-NEXT-FACTOR.
           CALL 'CBLTDLI' USING DLI-GNP
                                DB-PCB-CARR
                                WS-CARRFACT-IO
                                SSA-CARRFACT-Q.
           MOVE DBP-STATUS-CODE TO WS-DLI-STATUS.
           IF WS-DLI-NO-MORE-CHILD
               MOVE 'Y' TO WS-CHILD-EOF-SW
               GO TO P3450-EXIT
           END-IF.
           IF NOT WS-DLI-OK
               PERFORM P9500-DLI-ERROR THRU P9500-EXIT
               MOVE 'Y' TO WS-CHILD-EOF-SW
               GO TO P3450-EXIT
           END-IF.
           ADD 1 TO WS-FACT-CNT.
           IF WE-FACTOR-CNT NOT < 3
               ADD 1 TO WS-FACT-DROPPED
               GO TO P3450-EXIT
           END-IF.
           ADD 1 TO WE-FACTOR-CNT.
           SET WE-FX TO WE-FACTOR-CNT.
           MOVE CF-EFF-YYDDD TO WE-FC-EFF-YYDDD (WE-FX).
           MOVE CF-STATE-CD  TO WE-FC-STATE (WE-FX).
           MOVE CF-LATA      TO WE-FC-LATA (WE-FX).
           MOVE CF-PIU       TO WE-FC-PIU (WE-FX).
           MOVE CF-PLU       TO WE-FC-PLU (WE-FX).
           MOVE CF-SOURCE    TO WE-FC-SOURCE (WE-FX).

       P3450-EXIT.
           EXIT.

       P3600-GET-AGREEMENTS.
      * TWO AGREEMENTS FIT ON THE EXTRACT.  THE MOST RECENT PAIR
      * WINS BECAUSE THE SEGMENTS ARE STORED IN AGREEMENT IDENTIFIER
      * ORDER AND THE IDENTIFIERS ARE ISSUED IN SEQUENCE.
           MOVE 'P3600-GET-AGREEMENTS' TO WS-PARA-NAME.
           MOVE 'N' TO WS-CHILD-EOF-SW.
           SET WE-AX TO 1.
           PERFORM P3650-NEXT-AGMT THRU P3650-EXIT
               UNTIL WS-CHILD-EOF.

       P3600-EXIT.
           EXIT.

       P3650-NEXT-AGMT.
           CALL 'CBLTDLI' USING DLI-GNP
                                DB-PCB-CARR
                                WS-CARRAGMT-IO
                                SSA-CARRAGMT-U.
           MOVE DBP-STATUS-CODE TO WS-DLI-STATUS.
           IF WS-DLI-NO-MORE-CHILD
               MOVE 'Y' TO WS-CHILD-EOF-SW
               GO TO P3650-EXIT
           END-IF.
           IF NOT WS-DLI-OK
               PERFORM P9500-DLI-ERROR THRU P9500-EXIT
               MOVE 'Y' TO WS-CHILD-EOF-SW
               GO TO P3650-EXIT
           END-IF.
           ADD 1 TO WS-AGMT-CNT.
           IF WE-AGMT-CNT NOT < 2
               ADD 1 TO WS-AGMT-DROPPED
               GO TO P3650-EXIT
           END-IF.
           ADD 1 TO WE-AGMT-CNT.
           SET WE-AX TO WE-AGMT-CNT.
           MOVE CA-AGMT-ID    TO WE-AG-ID (WE-AX).
           MOVE CA-AGMT-TYPE  TO WE-AG-TYPE (WE-AX).
           MOVE CA-EFF-YYDDD  TO WE-AG-EFF-YYDDD (WE-AX).
           MOVE CA-RECIP-RATE TO WE-AG-RECIP-RATE (WE-AX).
           MOVE CA-MPB-ELIG   TO WE-AG-MPB-ELIG (WE-AX).

       P3650-EXIT.
           EXIT.

      *****************************************************************
      * S600-OUTPUT                                                   *
      *****************************************************************
       S600-OUTPUT SECTION.

       P6000-WRITE-EXTRACT.
           MOVE 'P6000-WRITE-EXTRACT' TO WS-PARA-NAME.
           MOVE WS-EXTRACT-AREA TO EXTRACT-RECORD.
           WRITE EXTRACT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
           PERFORM P6400-PRINT-DETAIL THRU P6400-EXIT.

       P6000-EXIT.
           EXIT.

       P6400-PRINT-DETAIL.
           IF WS-LINE-CNT > 55
               PERFORM P6600-HEADINGS THRU P6600-EXIT
           END-IF.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WE-OCN TO PC-COL-001-020.
           MOVE WE-NAME TO PC-COL-021-060.
           MOVE SPACES TO PC-COL-061-090.
           MOVE WE-TYPE TO PC-COL-061-090.
           MOVE WE-FACTOR-CNT TO WS-ED-COUNT.
           MOVE WS-ED-COUNT TO PC-COL-091-132.
           MOVE CABS-PRINT-LINE TO REPORT-LINE.
           WRITE REPORT-LINE.
           ADD 1 TO WS-LINE-CNT.

       P6400-EXIT.
           EXIT.

       P6600-HEADINGS.
           ADD 1 TO WS-PAGE-CNT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '1' TO PC-CC.
           MOVE 'CABCTL01' TO PC-COL-001-020.
           MOVE 'CARRIER PROFILE EXTRACT' TO PC-COL-021-060.
           MOVE CABS-PRINT-LINE TO REPORT-LINE.
           WRITE REPORT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '0' TO PC-CC.
           MOVE 'OCN' TO PC-COL-001-020.
           MOVE 'CARRIER NAME' TO PC-COL-021-060.
           MOVE 'TYPE' TO PC-COL-061-090.
           MOVE 'FACTOR ROWS' TO PC-COL-091-132.
           MOVE CABS-PRINT-LINE TO REPORT-LINE.
           WRITE REPORT-LINE.
           MOVE 3 TO WS-LINE-CNT.

       P6600-EXIT.
           EXIT.

      *****************************************************************
      * S800-CONTROL-AND-TERMINATION                                  *
      *****************************************************************
       S800-CONTROL SECTION.

       P8000-CONTROL.
      * MANDATORY.  THE FACTOR AND AGREEMENT ROWS THAT DID NOT FIT
      * ON THE EXTRACT ARE COUNTED AS SUMMARISED, WHICH IS HOW THE
      * BALANCING EQUATION IS SATISFIED WHEN A CARRIER CARRIES MORE
      * DETAIL THAN THE RECORD HOLDS.
           MOVE 'P8000-CONTROL' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-CONTROL-RECORD.
           MOVE WP-RUN-ID        TO CT-RUN-ID.
           MOVE WS-PGM-NAME      TO CT-PROCESS-ID.
           MOVE 010              TO CT-STEP-SEQ.
           MOVE WP-CYCLE-YYDDD   TO CT-CYCLE-YYDDD.
           MOVE WP-BILL-PERIOD   TO CT-BILL-PERIOD.
           MOVE ZERO             TO CT-RERUN-NBR.
           MOVE 'CABJ2100'       TO CT-JOBNAME.
           MOVE 'IMSSTEP'        TO CT-STEPNAME.
           MOVE WS-READ-CNT      TO CT-READ.
           MOVE WS-WRITE-CNT     TO CT-WRITTEN.
           MOVE WS-REJECT-CNT    TO CT-REJECTED.
           COMPUTE WS-SUMM-CNT = WS-FACT-DROPPED + WS-AGMT-DROPPED.
           MOVE ZERO             TO CT-SUMMARISED.
           MOVE WS-CFWD-CNT      TO CT-CARRIED-FWD.
           MOVE ZERO             TO CT-HASH-MINUTES.
           MOVE ZERO             TO CT-HASH-AMOUNT.
           MOVE WS-ACC-SEQ-HASH  TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH  TO CT-HASH-OCN.
           PERFORM P8400-BALANCE THRU P8400-EXIT.
           MOVE ZERO             TO CT-RC.
           MOVE SPACES           TO CT-ABEND-CD.
           MOVE CS-OCN           TO CT-RESTART-KEY.
           MOVE CABS-CONTROL-RECORD TO CONTROL-RECORD.
           WRITE CONTROL-RECORD.

       P8000-EXIT.
           EXIT.

       P8400-BALANCE.
           COMPUTE WS-ACC-SEQ-HASH = CT-WRITTEN + CT-REJECTED
                                   + CT-SUMMARISED + CT-CARRIED-FWD.
           IF WS-ACC-SEQ-HASH = CT-READ
               SET CT-IN-BALANCE TO TRUE
           ELSE
               SET CT-OUT-OF-BAL TO TRUE
           END-IF.

       P8400-EXIT.
           EXIT.

       P9000-TERM.
           MOVE 'P9000-TERM' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '0' TO PC-CC.
           MOVE 'CARRIERS READ' TO PC-COL-001-020.
           MOVE WS-CARR-CNT TO WS-ED-COUNT.
           MOVE WS-ED-COUNT TO PC-COL-021-060.
           MOVE CABS-PRINT-LINE TO REPORT-LINE.
           WRITE REPORT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'FACTOR ROWS LOST' TO PC-COL-001-020.
           MOVE WS-FACT-DROPPED TO WS-ED-COUNT.
           MOVE WS-ED-COUNT TO PC-COL-021-060.
           MOVE CABS-PRINT-LINE TO REPORT-LINE.
           WRITE REPORT-LINE.
           CLOSE PARM-FILE.
           CLOSE EXTRACT-FILE.
           CLOSE CONTROL-FILE.
           CLOSE REPORT-FILE.

       P9000-EXIT.
           EXIT.

       P9500-DLI-ERROR.
      * ANY STATUS OTHER THAN THE EXPECTED ONES IS FATAL.  THE
      * EXTRACT IS INCOMPLETE AT THAT POINT AND MUST NOT BE USED.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '0' TO PC-CC.
           MOVE 'DLI STATUS' TO PC-COL-001-020.
           MOVE WS-DLI-STATUS TO PC-COL-021-060.
           MOVE WS-PARA-NAME TO PC-COL-061-090.
           MOVE DBP-SEG-NAME-FB TO PC-COL-091-132.
           MOVE CABS-PRINT-LINE TO REPORT-LINE.
           WRITE REPORT-LINE.
           MOVE 3010 TO CT-RC.
           MOVE 12 TO RETURN-CODE.
           MOVE 'Y' TO WS-DB-EOF-SW.
           MOVE 'Y' TO WS-CHILD-EOF-SW.

       P9500-EXIT.
           EXIT.

       P9600-PRINT.
           MOVE CABS-PRINT-LINE TO REPORT-LINE.
           WRITE REPORT-LINE.

       P9600-EXIT.
           EXIT.
