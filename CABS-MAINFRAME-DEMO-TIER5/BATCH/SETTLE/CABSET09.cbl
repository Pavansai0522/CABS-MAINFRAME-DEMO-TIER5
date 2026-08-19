      *****************************************************************
      * CABSET09 - SETTLEMENT NETTING BY COUNTERPARTY                 *
      * APPLICATION : SETL                                            *
      * INPUTS      : SETLIN   TELCABS.SETL.SETTLE.ALL(0)     CABSSETL*
      * INPUTS      : CARRMAST TELCABS.CABS.CARRIER           CABSCARR*
      * INPUTS      : SYSIN    INSTREAM                       NONE    *
      * OUTPUTS     : NETOUT   TELCABS.SETL.NET(+1)           CABSSETL*
      * OUTPUTS     : REPORT   SYSOUT                         CABSPRNT*
      * CONTROL     : CTLOUT                         CABSCTL          *
      * BALANCE     : CT-READ = CT-SUMMARISED + CT-REJECTED           *
      *               RECEIVABLES LESS PAYABLES = NET DUE PER PARTY   *
      * RESTART     : FULL RERUN                                      *
      * STANDARDS   : CODED TO CABS-STD-041 (MONEY FIELDS) AND        *
      *               CABS-STD-063 (PRINT CONTROL). REVIEWED AT THE   *
      *               2013 APPLICATION AUDIT. WAIVERS, IF ANY, ARE    *
      *               FILED WITH THE APPLICATION FOLDER.              *
      * REVISION HISTORY                                              *
      *   V1.00  1989-08-07  D.OKONKWO     INITIAL                    *
      *   V1.04  1993-05-19  D.OKONKWO     AGEING BUCKETS ADDED       *
      *   V1.08  1995-10-04  J.M.CASTILLO  INTEREST CALC ADDED        *
      *   V2.00  1997-06-23  J.M.CASTILLO  Y2K REVIEW - NO IMPACT     *
      *   V2.01  1999-04-08  P.NAIR        INTEREST CALC SUSPENDED    *
      *   V2.04  2005-07-15  P.NAIR        NET BY PARTY NOT BY TYPE   *
      *   V2.06  2012-12-11  A.BUKOWSKI    CROSS APP CARRIER READ     *
      *   V2.08  2019-07-02  M.OYELARAN    RECOMPILE ONLY             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CABSET09.
       AUTHOR.        D.OKONKWO.
       DATE-WRITTEN.  1989-08-07.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
               C01 IS TOP-OF-PAGE
               C04 IS NEW-SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * ALL SETTLEMENT RECORDS - MPB, RECIP AND CMDS
           SELECT SETTLE-IN-FILE
               ASSIGN TO UT-S-SETLIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
      * OWNED BY THE CABS APPLICATION - CROSS APP READ
           SELECT CARRIER-MASTER
               ASSIGN TO DA-I-CARRMAST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CRM-KEY
               FILE STATUS IS WS-FS-TABLE.
      * NET POSITION PER COUNTERPARTY - FEEDS THE STATEMENT
           SELECT NET-OUT-FILE
               ASSIGN TO UT-S-NETOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
      * SYSIN - RUN CONTROL CARD, NO DEFAULTS SUPPLIED
           SELECT PARM-FILE
               ASSIGN TO UT-S-SYSIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-TABLE.
      * RUN CONTROL - BALANCING RECORD, GDG PLUS ONE
           SELECT CONTROL-FILE
               ASSIGN TO UT-S-CTLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CONTROL.
      * SUSPENSE - REJECTED RECORDS WITH ERROR CODE
           SELECT SUSPENSE-FILE
               ASSIGN TO UT-S-SUSPOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-SUSPENSE.
      * PRINTED REPORT - ASA CARRIAGE CONTROL COL 1
           SELECT PRINT-FILE
               ASSIGN TO UT-S-REPORT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-TABLE.

       DATA DIVISION.
       FILE SECTION.
       FD  SETTLE-IN-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 180 CHARACTERS
               DATA RECORD IS STI-RECORD.
       01  STI-RECORD              PIC X(180).

       FD  CARRIER-MASTER
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 120 CHARACTERS
               DATA RECORD IS CRM-RECORD.
       01  CRM-RECORD.
           05  CRM-KEY                 PIC X(04).
           05  CRM-DATA                PIC X(116).

       FD  NET-OUT-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 180 CHARACTERS
               DATA RECORD IS NTO-RECORD.
       01  NTO-RECORD              PIC X(180).

       FD  PARM-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 80 CHARACTERS
               DATA RECORD IS PRM-RECORD.
       01  PRM-RECORD              PIC X(80).

       FD  CONTROL-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 180 CHARACTERS
               DATA RECORD IS CTL-RECORD.
       01  CTL-RECORD              PIC X(180).

       FD  SUSPENSE-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 300 CHARACTERS
               DATA RECORD IS SUS-RECORD.
       01  SUS-RECORD              PIC X(300).

       FD  PRINT-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 133 CHARACTERS
               DATA RECORD IS PRT-RECORD.
       01  PRT-RECORD              PIC X(133).

       WORKING-STORAGE SECTION.

      * PROGRAM IDENTIFICATION - MOVED TO THE CONTROL RECORD AND TO
      * EVERY SUSPENSE RECORD RAISED BY THIS MODULE.
       01  WS-PROGRAM-IDENT.
           05  WS-PGM-NAME         PIC X(08)         VALUE 'CABSET09'.
           05  WS-PGM-VERSION        PIC X(05)           VALUE 'V2.08'.
           05  WS-PGM-APPL           PIC X(04)           VALUE 'SETL'.
           05  WS-PGM-COMPILE      PIC X(08)         VALUE '20190702'.
           05  WS-PARA-NAME          PIC X(30)           VALUE SPACES.

      * RUN CONTEXT.  POPULATED FROM THE SYSIN CARD AND FROM THE
      * JCL SYMBOLICS THAT THE SCHEDULER SUBSTITUTES AT SUBMISSION.
      * NONE OF THESE HAVE DEFAULTS.
       01  WS-RUN-CONTEXT.
           05  WS-RUN-ID             PIC X(12)           VALUE SPACES.
           05  WS-CYCLE-YYDDD.
               10  WS-CYCLE-YY             PIC 9(02)            VALUE 0.
               10  WS-CYCLE-DDD            PIC 9(03)            VALUE 0.
           05  WS-BILL-PERIOD          PIC 9(06)             VALUE 0.
           05  WS-RERUN-NBR            PIC 9(02)             VALUE 0.
           05  WS-JOBNAME            PIC X(08)           VALUE SPACES.
           05  WS-STEPNAME           PIC X(08)           VALUE SPACES.
           05  WS-RETURN-CODE          PIC 9(04)             VALUE 0.
           05  WS-BAL-CHECK            PIC S9(11) COMP-3     VALUE 0.
           05  WS-ERR-CODE           PIC X(04)           VALUE SPACES.
           05  WS-ERR-SEVERITY         PIC X(01)             VALUE 'E'.
           05  WS-RESTART-KEY        PIC X(26)           VALUE SPACES.
           05  WS-JW-QUOT              PIC S9(07) COMP-3     VALUE 0.
           05  WS-SUB-RC               PIC S9(04) COMP       VALUE 0.
           05  WS-GREG-CYCLE           PIC 9(08)             VALUE 0.

       COPY CABSWRK.

       COPY CABSSETL.

       COPY CABSCARR.

       COPY CABSPRNT.

      * ACCEPT AREAS AND SPARE WORK FIELDS.
       01  WS-ACCEPT-AREAS.
           05  WS-ACCEPT-DATE          PIC 9(06)             VALUE 0.
           05  WS-ACCEPT-TIME          PIC 9(08)             VALUE 0.
       01  WS-AD-WORK.
           05  WS-AD-YY                PIC 9(02).
           05  WS-AD-MM                PIC 9(02).
           05  WS-AD-DD                PIC 9(02).
       01  WS-AD-ALT REDEFINES WS-AD-WORK.
           05  WS-AD-YYMM              PIC 9(04).
           05  WS-AD-DAY               PIC 9(02).

      * SYSIN CONTROL CARD.  READ AS 80 BYTES THEN REDEFINED THREE
      * WAYS.  THE CARD TYPE IN COLUMNS 1-2 DECIDES WHICH REDEFINE
      * IS VALID.  NOTHING IN THE PROGRAM ENFORCES THAT AGREEMENT.
      * LAYOUT HELD IN THE APPLICATION FOLDER, NOT IN A COPYBOOK.
       01  WS-PARM-CARD.
           05  WS-PC-TYPE            PIC X(02)           VALUE SPACES.
           05  WS-PC-REST            PIC X(78)           VALUE SPACES.
       01  WS-PARM-RUN REDEFINES WS-PARM-CARD.
           05  FILLER                  PIC X(02).
           05  WS-PC-RUN-ID            PIC X(12).
           05  WS-PC-CYCLE.
               10  WS-PC-CYCLE-YY          PIC 9(02).
               10  WS-PC-CYCLE-DDD         PIC 9(03).
           05  WS-PC-BILL-PERIOD       PIC 9(06).
           05  WS-PC-RERUN             PIC 9(02).
           05  WS-PC-JOBNAME           PIC X(08).
           05  WS-PC-STEPNAME          PIC X(08).
           05  WS-PC-OPT1              PIC X(01).
           05  WS-PC-OPT2              PIC X(01).
           05  WS-PC-EXTRA             PIC X(35).
       01  WS-PARM-EXT REDEFINES WS-PARM-CARD.
           05  FILLER                  PIC X(45).
           05  WS-PE-NET-PERIOD        PIC 9(06).
           05  WS-PE-INCL-DISP         PIC X(01).
           05  WS-PE-INT-RATE          PIC 9(03)V9(05).
           05  WS-PE-FILLER            PIC X(20).
       01  WS-PARM-OLD REDEFINES WS-PARM-CARD.
      * THE 1989 CARD.  TWO REGIONS NEVER CONVERTED THEIR JCL AND
      * THE EDIT STILL ACCEPTS IT.  THE FIELDS ARE IN A DIFFERENT
      * ORDER AND THE CYCLE DATE IS SIX DIGITS YYMMDD, NOT YYDDD.
           05  FILLER                  PIC X(02).
           05  WS-PO-CYCLE             PIC 9(05).
           05  WS-PO-RUN-ID            PIC X(12).
           05  WS-PO-YYMMDD            PIC 9(06).
           05  WS-PO-NETPER            PIC 9(06).
           05  FILLER                  PIC X(20).

      * LOCAL SWITCHES.  THE STANDARD ONES ARRIVE VIA CABSWRK.
       01  WS-LOCAL-SWITCHES.
           05  WS-PARM-EOF-SW          PIC X(01)             VALUE 'N'.
                   88  WS-PARM-EOF             VALUE 'Y'.
           05  WS-CARR-FOUND-SW        PIC X(01)             VALUE 'N'.
                   88  WS-CARR-FOUND           VALUE 'Y'.
           05  WS-INT-ACTIVE-SW        PIC X(01)             VALUE 'N'.
                   88  WS-INT-ACTIVE           VALUE 'Y'.

      * SUBSCRIPTS AND INDEX WORK FIELDS.
       01  WS-SUBSCRIPTS.
           05  WS-SUB1                 PIC S9(05) COMP-3     VALUE 0.
           05  WS-SUB2                 PIC S9(05) COMP-3     VALUE 0.
           05  WS-SUB3                 PIC S9(05) COMP-3     VALUE 0.

      * NETTING TABLE.  ONE ENTRY PER COUNTERPARTY.  RECEIVABLES
      * AND PAYABLES ARE HELD SEPARATELY AND NETTED AT THE END -
      * THE GROSS FIGURES ARE NEEDED FOR THE LEDGER EVEN THOUGH
      * ONLY THE NET IS SETTLED IN CASH.
       01  WS-NET-TABLE.
           05  WS-NT-ENTRY OCCURS 400 TIMES
                   INDEXED BY WS-NT-IX.
               10  WS-NT-OCN               PIC X(04).
               10  WS-NT-NAME              PIC X(40).
               10  WS-NT-RECV              PIC S9(13)V9(02) COMP-3.
               10  WS-NT-PAY               PIC S9(13)V9(02) COMP-3.
               10  WS-NT-NET               PIC S9(13)V9(02) COMP-3.
               10  WS-NT-MPB               PIC S9(13)V9(02) COMP-3.
               10  WS-NT-RECIP             PIC S9(13)V9(02) COMP-3.
               10  WS-NT-CMDS              PIC S9(13)V9(02) COMP-3.
               10  WS-NT-COUNT             PIC S9(09) COMP-3.
               10  WS-NT-TERMS             PIC 9(03).
               10  WS-NT-OLDEST            PIC 9(05).
       01  WS-NET-CTL.
           05  WS-NT-USED              PIC S9(05) COMP-3     VALUE 0.
           05  WS-NT-MAX               PIC S9(05) COMP-3     VALUE 400.
           05  WS-NT-HIT               PIC S9(05) COMP-3     VALUE 0.
           05  WS-NT-FOUND-SW          PIC X(01)             VALUE 'N'.
                   88  WS-NT-FOUND              VALUE 'Y'.

      * NETTING TOTALS.
       01  WS-NET-TOTALS.
           05  WS-TOT-RECV             PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-TOT-PAY              PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-TOT-NET              PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-DISP-CNT             PIC S9(09) COMP-3     VALUE 0.
           05  WS-INT-AMOUNT           PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-AGE-DAYS             PIC S9(05) COMP-3     VALUE 0.
           05  WS-WORK-AMT             PIC S9(13)V9(02) COMP-3 VALUE 0.

      * NET OUTPUT RECORD WITH TWO REDEFINES.
       01  WS-NET-OUT.
           05  WS-NO-OCN             PIC X(04)           VALUE SPACES.
           05  WS-NO-PERIOD            PIC 9(06)             VALUE 0.
           05  WS-NO-RECV              PIC S9(13)V9(02)      VALUE 0.
           05  WS-NO-PAY               PIC S9(13)V9(02)      VALUE 0.
           05  WS-NO-NET               PIC S9(13)V9(02)      VALUE 0.
           05  WS-NO-DIRECTION       PIC X(01)           VALUE SPACES.
           05  WS-NO-TERMS             PIC 9(03)             VALUE 0.
           05  WS-NO-DUE-YYDDD         PIC 9(05)             VALUE 0.
           05  WS-NO-FILLER          PIC X(106)          VALUE SPACES.
       01  WS-NET-OUT-K REDEFINES WS-NET-OUT.
           05  WS-NK-KEY               PIC X(10).
           05  WS-NK-REST              PIC X(170).
       01  WS-NET-OUT-A REDEFINES WS-NET-OUT.
           05  WS-NA-HEAD              PIC X(10).
           05  WS-NA-AMOUNTS           PIC X(45).
           05  WS-NA-TAIL              PIC X(125).

      * JULIAN DATE WORK AREA - LOCAL TO CABSET09.  THE SHARED AREA IN
      * CABSDATE IS USED FOR THE CYCLE DATE ONLY.  THIS ONE CARRIES
      * THE WINDOW ENDPOINTS.
       01  WS-JULIAN-WORK.
           05  WS-JW-FROM.
               10  WS-JW-FROM-YY           PIC 9(02)            VALUE 0.
               10  WS-JW-FROM-DDD          PIC 9(03)            VALUE 0.
           05  WS-JW-THRU.
               10  WS-JW-THRU-YY           PIC 9(02)            VALUE 0.
               10  WS-JW-THRU-DDD          PIC 9(03)            VALUE 0.
           05  WS-JW-TEST.
               10  WS-JW-TEST-YY           PIC 9(02)            VALUE 0.
               10  WS-JW-TEST-DDD          PIC 9(03)            VALUE 0.
           05  WS-JW-HOLD.
               10  WS-JW-HOLD-YY           PIC 9(02)            VALUE 0.
               10  WS-JW-HOLD-DDD          PIC 9(03)            VALUE 0.
           05  WS-JW-CCYY              PIC 9(04)             VALUE 0.
           05  WS-JW-DAYS-IN-YR        PIC 9(03)             VALUE 365.
           05  WS-JW-ABS-FROM          PIC S9(07) COMP-3     VALUE 0.
           05  WS-JW-ABS-THRU          PIC S9(07) COMP-3     VALUE 0.
           05  WS-JW-ABS-TEST          PIC S9(07) COMP-3     VALUE 0.
           05  WS-JW-SPAN-DAYS         PIC S9(07) COMP-3     VALUE 0.
           05  WS-JW-REM               PIC S9(05) COMP-3     VALUE 0.
           05  WS-JW-LEAP-SW           PIC X(01)             VALUE 'N'.
                   88  WS-JW-LEAP              VALUE 'Y'.

      * ABEND COMMUNICATION AREA.  PASSED TO CABABEND WHICH ISSUES
      * A USER ABEND WITH THE CODE IN WS-AB-CODE.
       01  WS-ABEND-AREA.
           05  WS-AB-CODE              PIC 9(04) COMP        VALUE 0.
           05  WS-AB-PGM             PIC X(08)           VALUE SPACES.
           05  WS-AB-PARA            PIC X(30)           VALUE SPACES.
           05  WS-AB-TEXT            PIC X(60)           VALUE SPACES.
           05  WS-AB-KEY             PIC X(26)           VALUE SPACES.

      * PARAMETER AREA FOR CABDATCV - THE SHARED DATE CONVERSION
      * SUBROUTINE.  CABDATCV IS 1988 VINTAGE AND STILL PIVOTS ON
      * 70 INTERNALLY.
       01  WS-DATE-PARM.
           05  WS-DP-FUNCTION        PIC X(02)           VALUE SPACES.
           05  WS-DP-YYDDD             PIC 9(05)             VALUE 0.
           05  WS-DP-CCYYMMDD          PIC 9(08)             VALUE 0.
           05  WS-DP-DAYS              PIC S9(07) COMP-3     VALUE 0.
           05  WS-DP-RC                PIC 9(02)             VALUE 0.

      * DAYS BEFORE MONTH TABLE - NON LEAP AND LEAP.  USED BY THE
      * YYDDD TO GREGORIAN CONVERSION.
       01  WS-MONTH-CONST.
           05  FILLER              PIC X(48)
                   VALUE '000031059090120151181212243273304334'.
           05  FILLER              PIC X(48)
                   VALUE '000031060091121152182213244274305335'.
       01  WS-MONTH-TABLE REDEFINES WS-MONTH-CONST.
           05  WS-MT-YEAR-TYPE OCCURS 2 TIMES.
               10  WS-MT-MONTH OCCURS 12 TIMES
                       INDEXED BY WS-MT-IX.
                   15  WS-MT-DAYS-BEFORE       PIC 9(03).
               10  FILLER                  PIC X(12).

      * PRINT LINE BUILD AREAS.  CABSPRNT SUPPLIES THE OUTPUT LINE;
      * THESE ARE THE ASSEMBLY AREAS.
       01  WS-PAGE-CONTROL.
           05  WS-PAGE-NBR             PIC 9(05) COMP-3      VALUE 0.
           05  WS-LINE-CNT             PIC 9(03) COMP-3      VALUE 99.
           05  WS-MAX-LINES            PIC 9(03) COMP-3      VALUE 58.
       01  WS-HEAD-1.
           05  FILLER                  PIC X(01)             VALUE '1'.
           05  FILLER              PIC X(08)         VALUE 'TELCABS '.
           05  FILLER              PIC X(52)
                   VALUE 'INTER CARRIER SETTLEMENT NETTING'.
           05  FILLER                PIC X(06)           VALUE 'PAGE  '.
           05  WS-H1-PAGE              PIC ZZZZ9.
           05  FILLER                PIC X(60)           VALUE SPACES.
       01  WS-HEAD-2.
           05  FILLER                  PIC X(01)             VALUE ' '.
           05  FILLER              PIC X(10)         VALUE 'RUN ID   '.
           05  WS-H2-RUNID             PIC X(12).
           05  FILLER              PIC X(10)         VALUE '  CYCLE  '.
           05  WS-H2-CYCLE             PIC 9(05).
           05  FILLER              PIC X(10)         VALUE '  PGM    '.
           05  WS-H2-PGM               PIC X(08).
           05  FILLER                PIC X(75)           VALUE SPACES.
       01  WS-HEAD-3.
           05  FILLER                  PIC X(01)             VALUE ' '.
           05  FILLER              PIC X(48)
                  VALUE 'OCN  CARRIER-NAME                  RECEIVABLE'.
           05  FILLER              PIC X(40)
                   VALUE '  PAYABLE        NET            D  TERMS'.
       01  WS-HEAD-4.
           05  FILLER                  PIC X(01)             VALUE ' '.
           05  FILLER                PIC X(131)          VALUE ALL '-'.

      * DETAIL LINE WS-DETAIL-1.
       01  WS-DETAIL-1.
           05  FILLER                  PIC X(01)             VALUE ' '.
           05  WS-D1-OCN               PIC X(04).
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-NAME              PIC X(30).
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-RECV              PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-PAY               PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-NET               PIC ZZZ,ZZZ,ZZ9.99-.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-DIR               PIC X(01).
           05  FILLER                PIC X(02)           VALUE SPACES.
           05  WS-D1-TERMS             PIC ZZ9.

      * MESSAGE TEXT TABLE.  ADDRESSED BY ERROR CODE SEQUENCE, NOT
      * BY THE CODE ITSELF - INSERTING A CODE IN THE MIDDLE SHIFTS
      * EVERY MESSAGE.  THIS HAS HAPPENED TWICE (1994 AND 2007).
       01  WS-MSG-CONST.
           05  FILLER              PIC X(44)
                   VALUE 'NETTED                                      '.
           05  FILLER              PIC X(44)
                   VALUE 'DISPUTED - EXCLUDED FROM NETTING            '.
           05  FILLER              PIC X(44)
                   VALUE 'CARRIER NOT ON THE CABS MASTER              '.
           05  FILLER              PIC X(44)
                   VALUE 'NET POSITION IS A RECEIVABLE                '.
           05  FILLER              PIC X(44)
                   VALUE 'NET POSITION IS A PAYABLE                   '.
           05  FILLER              PIC X(44)
                   VALUE 'NET POSITION IS ZERO                        '.
           05  FILLER              PIC X(44)
                   VALUE 'NETTING TABLE FULL                          '.
           05  FILLER              PIC X(44)
                   VALUE 'INTEREST CALCULATION SUSPENDED 1999         '.
           05  FILLER              PIC X(44)
                   VALUE 'DUE DATE DERIVED FROM PAYMENT TERMS         '.
           05  FILLER              PIC X(44)
                   VALUE 'END OF NETTING RUN                          '.
       01  WS-MSG-TABLE REDEFINES WS-MSG-CONST.
           05  WS-MSG-ENTRY OCCURS 10 TIMES
                   INDEXED BY WS-MSG-IX.
               10  WS-MSG-TEXT             PIC X(44).

      * LAYOUTS COPIED FOR THE CROSS REFERENCE AND VALIDATION
      * ROUTINES.  NOT EVERY FIELD IN EVERY LAYOUT IS USED BY
      * THIS MODULE - THE COPY IS HERE BECAUSE THE LAYOUT WAS
      * NEEDED AT SOME POINT AND REMOVING A COPY MEMBER FORCES
      * A FULL REGRESSION UNDER CABS-STD-009.
       COPY CABSRATE.

       PROCEDURE DIVISION.


      *****************************************************************
      * S000-MAINLINE                                                 *
      * DRIVER.  STRUCTURE IS MANDATED BY CABS-STD-001.               *
      *****************************************************************
       S000-MAINLINE SECTION.

       P0000-MAINLINE.
      * THE FOUR PERFORMS BELOW ARE THE ONLY STATEMENTS PERMITTED IN
      * THIS PARAGRAPH.  DO NOT ADD LOGIC HERE - ADD IT TO P1000 OR
      * P2000 AND LET THE STRUCTURE STAND.
           PERFORM P1000-INIT     THRU P1000-EXIT.
           PERFORM P2000-PROCESS  THRU P2000-EXIT
               UNTIL WS-EOF.
           PERFORM P8000-CONTROL  THRU P8000-EXIT.
           PERFORM P9000-TERM     THRU P9000-EXIT.
           STOP RUN.


      *****************************************************************
      * S100-INITIALISATION                                           *
      * OPEN, READ THE CONTROL CARD, PRIME THE WORK AREAS.            *
      *****************************************************************
       S100-INITIALISATION SECTION.

       P1000-INIT.
      * NOTHING IS DEFAULTED.  IF THE SCHEDULER DID NOT SUPPLY A
      * CYCLE DATE THE STEP ABENDS - IT DOES NOT ASSUME TODAY.
           MOVE 'P1000-INIT' TO WS-PARA-NAME.
           ACCEPT WS-ACCEPT-DATE FROM DATE.
           ACCEPT WS-ACCEPT-TIME FROM TIME.
           OPEN INPUT  SETTLE-IN-FILE
                       CARRIER-MASTER
                       PARM-FILE
           OPEN OUTPUT NET-OUT-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 5901 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-SETLIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 5902 TO WS-AB-CODE
               MOVE 'OPEN FAILED DA-I-CARRMAST' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 5903 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-NETOUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 4901 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-SYSIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 4801 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-CTLOUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 4802 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-SUSPOUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 4803 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-REPORT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           MOVE WS-ACCEPT-DATE         TO WS-AD-WORK.
           MOVE WS-AD-YY               TO DW-CUR-YY.
           PERFORM P1100-READ-PARM THRU P1100-EXIT.
           PERFORM P1200-EDIT-PARM THRU P1200-EXIT.
           MOVE ZERO TO WS-TOT-RECV WS-TOT-PAY
                        WS-TOT-NET WS-DISP-CNT
                        WS-NT-USED.
           PERFORM P6000-HEADING THRU P6000-EXIT.
           MOVE WS-PC-RUN-ID           TO WS-RUN-ID.
           MOVE WS-PC-CYCLE            TO WS-CYCLE-YYDDD.
           MOVE WS-PC-BILL-PERIOD      TO WS-BILL-PERIOD.
           MOVE WS-PC-RERUN            TO WS-RERUN-NBR.
           MOVE WS-PC-JOBNAME          TO WS-JOBNAME.
           MOVE WS-PC-STEPNAME         TO WS-STEPNAME.
           MOVE WS-CYCLE-YYDDD         TO DW-CURRENT-YYDDD.
           MOVE 'JG' TO WS-DP-FUNCTION.
           MOVE WS-CYCLE-YYDDD         TO WS-DP-YYDDD.
           CALL 'CABDATCV' USING WS-DATE-PARM.
           IF WS-DP-RC NOT = ZERO
               MOVE 3908 TO WS-AB-CODE
               MOVE 'CYCLE DATE CONVERSION FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           MOVE WS-DP-CCYYMMDD         TO WS-GREG-CYCLE.
           DISPLAY WS-PGM-NAME ' STARTED - RUN ' WS-RUN-ID.
           DISPLAY '  CYCLE YYDDD  ' WS-CYCLE-YYDDD.
           DISPLAY '  BILL PERIOD  ' WS-BILL-PERIOD.

       P1000-EXIT.
           EXIT.

       P1100-READ-PARM.
      * THE SYSIN CARD CARRIES THE VALUES THE SCHEDULER SUBSTITUTED
      * INTO THE JCL AT SUBMISSION TIME.  THERE ARE NO DEFAULTS - AN
      * ABSENT CARD IS A FATAL ERROR, NOT A DEFAULTED RUN.
      * PARAMETER HANDLING PER CABS-STD-022.
           MOVE 'P1100-READ-PARM' TO WS-PARA-NAME.
           MOVE SPACES TO WS-PARM-CARD.
           READ PARM-FILE INTO WS-PARM-CARD
               AT END
                   MOVE 'Y' TO WS-PARM-EOF-SW.
           IF WS-PARM-EOF
               MOVE 3901 TO WS-AB-CODE
               MOVE 'NO SYSIN CONTROL CARD SUPPLIED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PC-TYPE NOT = 'RN' AND WS-PC-TYPE NOT = 'R2'
               MOVE 3902 TO WS-AB-CODE
               MOVE 'SYSIN CARD TYPE NOT RN OR R2' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PC-TYPE = 'R2'
               MOVE WS-PO-RUN-ID           TO WS-PC-RUN-ID
               MOVE WS-PO-CYCLE            TO WS-PC-CYCLE.

       P1100-EXIT.
           EXIT.

       P1200-EDIT-PARM.
      * EDIT THE CONTROL CARD.  EVERY FIELD IS MANDATORY.  THE 1989
      * CARD FORMAT IS STILL ACCEPTED VIA THE WS-PARM-OLD REDEFINE.
           MOVE 'P1200-EDIT-PARM' TO WS-PARA-NAME.
           IF WS-PC-CYCLE NOT NUMERIC
               MOVE 3903 TO WS-AB-CODE
               MOVE 'CYCLE DATE NOT NUMERIC' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PC-CYCLE-DDD < 001 OR WS-PC-CYCLE-DDD > 366
               MOVE 3904 TO WS-AB-CODE
               MOVE 'CYCLE DAY OF YEAR OUT OF RANGE' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PC-BILL-PERIOD NOT NUMERIC
               MOVE 3905 TO WS-AB-CODE
               MOVE 'BILL PERIOD NOT NUMERIC' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PC-RERUN NOT NUMERIC
               MOVE ZERO TO WS-PC-RERUN.
           IF WS-PE-NET-PERIOD NOT NUMERIC
               MOVE WS-PC-BILL-PERIOD TO WS-PE-NET-PERIOD.
           IF WS-PE-INCL-DISP NOT = 'Y'
               MOVE 'N' TO WS-PE-INCL-DISP.
           IF WS-PE-INT-RATE NOT NUMERIC
               MOVE ZERO TO WS-PE-INT-RATE.

       P1200-EXIT.
           EXIT.


      *****************************************************************
      * S200-NETTING                                                  *
      * ACCUMULATE RECEIVABLE AND PAYABLE.                            *
      *****************************************************************
       S200-NETTING SECTION.

       P2000-PROCESS.
      * ONE SETTLEMENT RECORD PER PASS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ THRU P2100-EXIT.
           IF WS-EOF
               GO TO P2000-EXIT.
           MOVE STI-RECORD TO CABS-SETTLEMENT-RECORD.
           MOVE ST-KEY TO WS-RESTART-KEY.
           IF ST-DISPUTE-SW = 'Y' AND WS-PE-INCL-DISP NOT = 'Y'
               ADD 1 TO WS-DISP-CNT
               ADD 1 TO WS-SUMM-CNT
               GO TO P2000-EXIT.
           IF ST-SETTLE-PERIOD NOT = WS-PE-NET-PERIOD
               ADD 1 TO WS-SUMM-CNT
               GO TO P2000-EXIT.
           PERFORM P2200-FIND-PARTY THRU P2200-EXIT.
           IF NOT WS-NT-FOUND
               GO TO P2000-EXIT.
           PERFORM P3000-ACCUMULATE THRU P3000-EXIT.
           ADD 1 TO WS-SUMM-CNT.

       P2000-EXIT.
           EXIT.

       P2100-READ.
      * SEQUENTIAL READ OF THE SETTLEMENT FILE.
           READ SETTLE-IN-FILE
               AT END
                   MOVE 'Y' TO WS-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00'
               MOVE 3590 TO WS-AB-CODE
               MOVE 'READ ERROR UT-S-SETLIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-READ-CNT.

       P2100-EXIT.
           EXIT.

       P2200-FIND-PARTY.
      * FIND OR CREATE THE COUNTERPARTY ENTRY.  THE CARRIER NAME
      * AND PAYMENT TERMS COME FROM THE CABS CARRIER MASTER, WHICH
      * BELONGS TO THE BILLING APPLICATION.  THE SETTLEMENT
      * APPLICATION HAS ITS OWN CARRIER FILE BUT IT DOES NOT CARRY
      * PAYMENT TERMS, SO THIS READ CROSSES THE BOUNDARY.
      * ACCESS IS RECORDED IN THE DATASET REGISTER.
           MOVE 'P2200-FIND-PARTY' TO WS-PARA-NAME.
           MOVE 'N' TO WS-NT-FOUND-SW.
           MOVE 1 TO WS-SUB1.
           PERFORM P2250-PARTY-COMPARE THRU P2250-EXIT
               UNTIL WS-SUB1 > WS-NT-USED
                  OR WS-NT-FOUND.
           IF WS-NT-FOUND
               GO TO P2200-EXIT.
           IF WS-NT-USED NOT < WS-NT-MAX
               MOVE EC-OUT-OF-BALANCE TO WS-ERR-CODE
               MOVE 'E' TO WS-ERR-SEVERITY
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               GO TO P2200-EXIT.
           ADD 1 TO WS-NT-USED.
           MOVE WS-NT-USED TO WS-NT-HIT.
           SET WS-NT-IX TO WS-NT-HIT.
           MOVE ST-COUNTERPARTY-OCN TO WS-NT-OCN (WS-NT-IX).
           MOVE ZERO TO WS-NT-RECV (WS-NT-IX).
           MOVE ZERO TO WS-NT-PAY (WS-NT-IX).
           MOVE ZERO TO WS-NT-NET (WS-NT-IX).
           MOVE ZERO TO WS-NT-MPB (WS-NT-IX).
           MOVE ZERO TO WS-NT-RECIP (WS-NT-IX).
           MOVE ZERO TO WS-NT-CMDS (WS-NT-IX).
           MOVE ZERO TO WS-NT-COUNT (WS-NT-IX).
           PERFORM P2300-CARRIER-READ THRU P2300-EXIT.
           MOVE 'Y' TO WS-NT-FOUND-SW.

       P2200-EXIT.
           EXIT.

       P2250-PARTY-COMPARE.
      * ONE TABLE COMPARE.
           IF WS-NT-OCN (WS-SUB1) = ST-COUNTERPARTY-OCN
               MOVE 'Y' TO WS-NT-FOUND-SW
               MOVE WS-SUB1 TO WS-NT-HIT
               GO TO P2250-EXIT.
           ADD 1 TO WS-SUB1.

       P2250-EXIT.
           EXIT.

       P2300-CARRIER-READ.
      * READ THE CABS CARRIER MASTER FOR THE NAME AND TERMS.
           MOVE 'N' TO WS-CARR-FOUND-SW.
           MOVE ST-COUNTERPARTY-OCN TO CRM-KEY.
           READ CARRIER-MASTER
               INVALID KEY
                   MOVE 'CARRIER NOT ON CABS MASTER'
                        TO WS-NT-NAME (WS-NT-IX)
                   MOVE 030 TO WS-NT-TERMS (WS-NT-IX)
                   GO TO P2300-EXIT.
           MOVE CRM-RECORD TO CABS-CARRIER-RECORD.
           MOVE 'Y' TO WS-CARR-FOUND-SW.
           MOVE CR-NAME TO WS-NT-NAME (WS-NT-IX).
           MOVE CR-TERMS-DAYS TO WS-NT-TERMS (WS-NT-IX).

       P2300-EXIT.
           EXIT.


      *****************************************************************
      * S300-ACCUMULATE                                               *
      * ADD THE RECORD TO ITS BUCKET.                                 *
      *****************************************************************
       S300-ACCUMULATE SECTION.

       P3000-ACCUMULATE.
      * A RECEIVABLE IS MONEY THE COUNTERPARTY OWES US, A PAYABLE
      * IS MONEY WE OWE THEM.  BOTH ARE HELD GROSS.
           SET WS-NT-IX TO WS-NT-HIT.
           MOVE ST-NET-DUE TO WS-WORK-AMT.
           IF ST-RECEIVABLE
               ADD WS-WORK-AMT TO WS-NT-RECV (WS-NT-IX)
               ADD WS-WORK-AMT TO WS-TOT-RECV
           ELSE
               ADD WS-WORK-AMT TO WS-NT-PAY (WS-NT-IX)
               ADD WS-WORK-AMT TO WS-TOT-PAY.
           IF ST-MEET-POINT
               ADD WS-WORK-AMT TO WS-NT-MPB (WS-NT-IX).
           IF ST-RECIP-COMP
               ADD WS-WORK-AMT TO WS-NT-RECIP (WS-NT-IX).
           IF ST-CMDS-RAO
               ADD WS-WORK-AMT TO WS-NT-CMDS (WS-NT-IX).
           ADD 1 TO WS-NT-COUNT (WS-NT-IX).
           ADD WS-WORK-AMT TO WS-ACC-AMOUNT.
           ADD ST-TOTAL-MOU TO WS-ACC-MINUTES.
           IF WS-NT-OLDEST (WS-NT-IX) = ZERO
               MOVE ST-EXCH-YYDDD TO WS-NT-OLDEST (WS-NT-IX).

       P3000-EXIT.
           EXIT.


      *****************************************************************
      * S400-NET-AND-WRITE                                            *
      * NET AND WRITE EACH PARTY.                                     *
      *****************************************************************
       S400-NET-AND-WRITE SECTION.

       P4000-NET-PARTIES.
      * NET EACH COUNTERPARTY AT END OF FILE.
           MOVE 'P4000-NET-PARTIES' TO WS-PARA-NAME.
           MOVE 1 TO WS-SUB2.
           PERFORM P4100-NET-ONE THRU P4100-EXIT
               UNTIL WS-SUB2 > WS-NT-USED.

       P4000-EXIT.
           EXIT.

       P4100-NET-ONE.
      * NET ONE COUNTERPARTY AND WRITE THE RECORD.  THE NET IS
      * ROUNDED TO TWO PLACES.  CABSET11 TRUNCATES THE SAME FIGURE
      * WHEN IT PRINTS THE STATEMENT, SO THE STATEMENT AND THE NET
      * FILE CAN DIFFER BY A CENT.
           SET WS-NT-IX TO WS-SUB2.
           COMPUTE WS-NT-NET (WS-NT-IX) ROUNDED =
                   WS-NT-RECV (WS-NT-IX) - WS-NT-PAY (WS-NT-IX).
           ADD WS-NT-NET (WS-NT-IX) TO WS-TOT-NET.
           PERFORM P4200-DUE-DATE THRU P4200-EXIT.
           MOVE SPACES TO WS-NET-OUT.
           MOVE WS-NT-OCN (WS-NT-IX) TO WS-NO-OCN.
           MOVE WS-PE-NET-PERIOD TO WS-NO-PERIOD.
           MOVE WS-NT-RECV (WS-NT-IX) TO WS-NO-RECV.
           MOVE WS-NT-PAY (WS-NT-IX) TO WS-NO-PAY.
           MOVE WS-NT-NET (WS-NT-IX) TO WS-NO-NET.
           MOVE WS-NT-TERMS (WS-NT-IX) TO WS-NO-TERMS.
           IF WS-NT-NET (WS-NT-IX) > ZERO
               MOVE 'R' TO WS-NO-DIRECTION
           ELSE
               MOVE 'P' TO WS-NO-DIRECTION.
           MOVE WS-NET-OUT TO NTO-RECORD.
           WRITE NTO-RECORD.
           ADD 1 TO WS-WRITE-CNT.
           SUBTRACT 1 FROM WS-SUMM-CNT.
           PERFORM P6100-DETAIL THRU P6100-EXIT.
           ADD 1 TO WS-SUB2.

       P4100-EXIT.
           EXIT.

       P4200-DUE-DATE.
      * DERIVE THE DUE DATE BY ADDING THE PAYMENT TERMS IN DAYS TO
      * THE CYCLE DATE.  THE ADDITION ROLLS THE YEAR PROPERLY, SO A
      * THIRTY DAY TERM APPLIED ON DAY 350 GIVES DAY 015 OF THE
      * FOLLOWING YEAR AND NOT DAY 380.
           MOVE WS-CYCLE-YYDDD TO WS-JW-HOLD.
           MOVE WS-NT-TERMS (WS-NT-IX) TO WS-JW-SPAN-DAYS.
           PERFORM P4300-ADD-DAYS THRU P4300-EXIT.
           MOVE WS-JW-HOLD TO WS-NO-DUE-YYDDD.

       P4200-EXIT.
           EXIT.

       P4300-ADD-DAYS.
      * ADD A NUMBER OF DAYS TO A YYDDD WITH YEAR ROLLOVER.
           MOVE WS-JW-HOLD TO WS-JW-TEST.
           PERFORM P4400-LEAP-YEAR THRU P4400-EXIT.
           COMPUTE WS-JW-TEST-DDD =
                   WS-JW-TEST-DDD + WS-JW-SPAN-DAYS.
           PERFORM P4350-ROLL-YEAR THRU P4350-EXIT
               UNTIL WS-JW-TEST-DDD NOT > WS-JW-DAYS-IN-YR.
           MOVE WS-JW-TEST TO WS-JW-HOLD.

       P4300-EXIT.
           EXIT.

       P4350-ROLL-YEAR.
      * ROLL ONE YEAR FORWARD.
           SUBTRACT WS-JW-DAYS-IN-YR FROM WS-JW-TEST-DDD.
           ADD 1 TO WS-JW-TEST-YY.
           IF WS-JW-TEST-YY > 99
               MOVE ZERO TO WS-JW-TEST-YY.
           PERFORM P4400-LEAP-YEAR THRU P4400-EXIT.

       P4350-EXIT.
           EXIT.

       P4400-LEAP-YEAR.
      * GREGORIAN LEAP YEAR TEST.
           IF WS-JW-TEST-YY < 70
               COMPUTE WS-JW-CCYY = 2000 + WS-JW-TEST-YY
           ELSE
               COMPUTE WS-JW-CCYY = 1900 + WS-JW-TEST-YY.
           MOVE 'N' TO WS-JW-LEAP-SW.
           DIVIDE WS-JW-CCYY BY 4 GIVING WS-JW-QUOT
               REMAINDER WS-JW-REM.
           IF WS-JW-REM = 0
               MOVE 'Y' TO WS-JW-LEAP-SW.
           DIVIDE WS-JW-CCYY BY 100 GIVING WS-JW-QUOT
               REMAINDER WS-JW-REM.
           IF WS-JW-REM = 0
               MOVE 'N' TO WS-JW-LEAP-SW.
           DIVIDE WS-JW-CCYY BY 400 GIVING WS-JW-QUOT
               REMAINDER WS-JW-REM.
           IF WS-JW-REM = 0
               MOVE 'Y' TO WS-JW-LEAP-SW.
           IF WS-JW-LEAP-SW = 'Y'
               MOVE 366 TO WS-JW-DAYS-IN-YR
           ELSE
               MOVE 365 TO WS-JW-DAYS-IN-YR.

       P4400-EXIT.
           EXIT.


      *****************************************************************
      * S500-INTEREST                                                 *
      * LATE PAYMENT INTEREST.  SUSPENDED SINCE 1999.                 *
      *****************************************************************
       S500-INTEREST SECTION.

       P5000-INTEREST-CALC.
      * THE INTEREST CALCULATION ON OVERDUE SETTLEMENT BALANCES WAS
      * PUT UNDER TARIFF REVIEW IN APRIL 1999.  IT IS RETAINED PER
      * CABS-STD-022 AND THE RATE IT USES IS SUPPLIED ON THE CONTROL
      * CARD BY THE SETTLEMENT GROUP WHEN THE REVIEW CONCLUDES.
      * THE AGEING BUCKETS BELOW ARE UNAFFECTED EITHER WAY.
           MOVE 'P5000-INTEREST-CALC' TO WS-PARA-NAME.
           SET WS-NT-IX TO WS-SUB3.
           MOVE WS-NT-OLDEST (WS-NT-IX) TO WS-JW-FROM.
           MOVE WS-CYCLE-YYDDD TO WS-JW-THRU.
           COMPUTE WS-AGE-DAYS =
                   WS-JW-THRU-DDD - WS-JW-FROM-DDD.
           IF WS-AGE-DAYS NOT > WS-NT-TERMS (WS-NT-IX)
               GO TO P5000-EXIT.
           COMPUTE WS-INT-AMOUNT ROUNDED =
                   WS-NT-NET (WS-NT-IX) * WS-PE-INT-RATE
                 * WS-AGE-DAYS / 36500.
           ADD WS-INT-AMOUNT TO WS-NT-RECV (WS-NT-IX).

       P5000-EXIT.
           EXIT.


      *****************************************************************
      * S600-REPORT                                                   *
      * NETTING REGISTER.                                             *
      *****************************************************************
       S600-REPORT SECTION.

       P6000-HEADING.
      * PAGE HEADINGS.
           ADD 1 TO WS-PAGE-NBR.
           MOVE WS-PAGE-NBR            TO WS-H1-PAGE.
           MOVE WS-RUN-ID              TO WS-H2-RUNID.
           MOVE WS-CYCLE-YYDDD         TO WS-H2-CYCLE.
           MOVE WS-PGM-NAME            TO WS-H2-PGM.
           WRITE PRT-RECORD FROM WS-HEAD-1 AFTER ADVANCING PAGE.
           WRITE PRT-RECORD FROM WS-HEAD-2 AFTER ADVANCING 1 LINES.
           WRITE PRT-RECORD FROM WS-HEAD-3 AFTER ADVANCING 2 LINES.
           WRITE PRT-RECORD FROM WS-HEAD-4 AFTER ADVANCING 1 LINES.
           MOVE 6 TO WS-LINE-CNT.

       P6000-EXIT.
           EXIT.

       P6100-DETAIL.
      * ONE LINE PER COUNTERPARTY.
           IF WS-LINE-CNT > WS-MAX-LINES
               PERFORM P6000-HEADING THRU P6000-EXIT.
           MOVE WS-NT-OCN (WS-NT-IX) TO WS-D1-OCN.
           MOVE WS-NT-NAME (WS-NT-IX) TO WS-D1-NAME.
           MOVE WS-NT-RECV (WS-NT-IX) TO WS-D1-RECV.
           MOVE WS-NT-PAY (WS-NT-IX) TO WS-D1-PAY.
           MOVE WS-NT-NET (WS-NT-IX) TO WS-D1-NET.
           MOVE WS-NO-DIRECTION TO WS-D1-DIR.
           MOVE WS-NT-TERMS (WS-NT-IX) TO WS-D1-TERMS.
           WRITE PRT-RECORD FROM WS-DETAIL-1 AFTER ADVANCING 1 LINES.
           ADD 1 TO WS-LINE-CNT.

       P6100-EXIT.
           EXIT.


      *****************************************************************
      * S800-CONTROL                                                  *
      * BALANCING AND SUSPENSE.  P8000 IS NOT OPTIONAL.               *
      *****************************************************************
       S800-CONTROL SECTION.

       P7000-SUSPEND.
      * WRITE A SUSPENSE RECORD.  THE CALLER SETS WS-ERR-CODE AND
      * WS-ERR-SEVERITY BEFORE PERFORMING THIS PARAGRAPH.
           MOVE SPACES                 TO CABS-SUSPENSE-RECORD.
           MOVE WS-ERR-CODE            TO SU-ERR-CODE.
           MOVE WS-ERR-SEVERITY        TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME            TO SU-DETECT-PGM.
           MOVE WS-PARA-NAME           TO SU-DETECT-PARA.
           MOVE WS-RUN-ID              TO SU-RUN-ID.
           MOVE CABS-SETTLEMENT-RECORD TO SU-ORIG-RECORD.
           CALL 'CABERRWR' USING CABS-SUSPENSE-RECORD
                                  WS-SUB-RC.
           WRITE SUS-RECORD FROM CABS-SUSPENSE-RECORD.
           ADD 1 TO WS-REJECT-CNT.
           MOVE 'Y' TO WS-ERROR-SW.

       P7000-EXIT.
           EXIT.

       P8000-CONTROL.
      * MANDATORY CONTROL RECORD.  THE BALANCING EQUATION IS
      *   CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARISED
      *           + CT-CARRIED-FWD
      * A FAILURE HERE SETS CT-OUT-OF-BAL AND RC 0008.  THE NIGHTLY
      * CONTROL REPORT (CABCTL02) READS EVERY CONTROL RECORD AND
      * HALTS THE CYCLE ON ANY OUT OF BALANCE PROCESS.
           MOVE SPACES                 TO CABS-CONTROL-RECORD.
           MOVE WS-RUN-ID              TO CT-RUN-ID.
           MOVE WS-PGM-NAME            TO CT-PROCESS-ID.
           MOVE 280                    TO CT-STEP-SEQ.
           MOVE WS-CYCLE-YYDDD         TO CT-CYCLE-YYDDD.
           MOVE WS-BILL-PERIOD         TO CT-BILL-PERIOD.
           MOVE WS-RERUN-NBR           TO CT-RERUN-NBR.
           MOVE WS-JOBNAME             TO CT-JOBNAME.
           MOVE WS-STEPNAME            TO CT-STEPNAME.
           MOVE WS-READ-CNT            TO CT-READ.
           MOVE WS-WRITE-CNT           TO CT-WRITTEN.
           MOVE WS-REJECT-CNT          TO CT-REJECTED.
           MOVE WS-SUMM-CNT            TO CT-SUMMARISED.
           MOVE WS-CFWD-CNT            TO CT-CARRIED-FWD.
           MOVE WS-ACC-MINUTES         TO CT-HASH-MINUTES.
           MOVE WS-ACC-AMOUNT          TO CT-HASH-AMOUNT.
           MOVE WS-ACC-SEQ-HASH        TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH        TO CT-HASH-OCN.
           COMPUTE WS-BAL-CHECK =
                   WS-WRITE-CNT + WS-REJECT-CNT
                 + WS-SUMM-CNT  + WS-CFWD-CNT.
           IF WS-BAL-CHECK = WS-READ-CNT
               MOVE 'B' TO CT-BAL-IND
           ELSE
               MOVE 'O' TO CT-BAL-IND
               MOVE EC-OUT-OF-BALANCE TO WS-ERR-CODE
               MOVE 0008 TO WS-RETURN-CODE
               PERFORM P7000-SUSPEND THRU P7000-EXIT.
           MOVE WS-RETURN-CODE         TO CT-RC.
           MOVE WS-RESTART-KEY         TO CT-RESTART-KEY.
           CALL 'CABHASH ' USING CT-HASH-TOTALS
                                  WS-SUB-RC.
           WRITE CTL-RECORD FROM CABS-CONTROL-RECORD.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 3801 TO WS-AB-CODE
               MOVE 'CONTROL RECORD WRITE FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.

       P8000-EXIT.
           EXIT.


      *****************************************************************
      * S900-TERMINATION                                              *
      * CLOSE DOWN, DISPLAY TOTALS, SET RETURN CODE.                  *
      *****************************************************************
       S900-TERMINATION SECTION.

       P9000-TERM.
      * DISPLAY THE RUN TOTALS TO THE JOB LOG.  OPERATIONS TRANSCRIBE
      * THESE INTO THE NIGHTLY BALANCING SHEET BY HAND - THE FORMAT
      * MUST NOT CHANGE WITHOUT NOTIFYING THE DATA CENTRE.
           DISPLAY '--------------------------------------------'.
           DISPLAY WS-PGM-NAME ' V' WS-PGM-VERSION ' RUN ' WS-RUN-ID.
           PERFORM P4000-NET-PARTIES THRU P4000-EXIT.
           DISPLAY 'PARTIES NETTED   ' WS-NT-USED.
           DISPLAY 'RECEIVABLE TOTAL ' WS-TOT-RECV.
           DISPLAY 'PAYABLE TOTAL    ' WS-TOT-PAY.
           DISPLAY 'NET POSITION     ' WS-TOT-NET.
           DISPLAY 'DISPUTED HELD    ' WS-DISP-CNT.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE SETTLE-IN-FILE
                 CARRIER-MASTER
                 NET-OUT-FILE
                 PARM-FILE
                 CONTROL-FILE
                 SUSPENSE-FILE
                 PRINT-FILE
           .
           MOVE WS-RETURN-CODE TO RETURN-CODE.

       P9000-EXIT.
           EXIT.

       P9500-ABEND.
      * UNRECOVERABLE ERROR.  CABABEND ISSUES A USER ABEND SO THAT
      * THE STEP FAILS VISIBLY RATHER THAN COMPLETING WITH BAD DATA.
           MOVE WS-PGM-NAME            TO WS-AB-PGM.
           MOVE WS-PARA-NAME           TO WS-AB-PARA.
           MOVE WS-RESTART-KEY         TO WS-AB-KEY.
           DISPLAY '*** ABEND ' WS-AB-CODE ' IN ' WS-AB-PGM.
           DISPLAY '*** PARAGRAPH ' WS-AB-PARA.
           DISPLAY '*** ' WS-AB-TEXT.
           DISPLAY '*** LAST KEY ' WS-AB-KEY.
           CALL 'CABABEND' USING WS-ABEND-AREA.
           MOVE 16 TO WS-RETURN-CODE.
           STOP RUN.

       P9500-EXIT.
           EXIT.

