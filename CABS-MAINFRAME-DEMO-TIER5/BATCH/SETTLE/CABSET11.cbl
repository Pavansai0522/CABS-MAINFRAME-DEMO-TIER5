      *****************************************************************
      * CABSET11 - SETTLEMENT STATEMENT GENERATOR                     *
      * APPLICATION : SETL                                            *
      * INPUTS      : NETIN    TELCABS.SETL.NET(0)            CABSSETL*
      * INPUTS      : BILLHDR  TELCABS.CABS.BILLHDR           CABSBHDR*
      * INPUTS      : SYSIN    INSTREAM                       NONE    *
      * OUTPUTS     : STMTOUT  TELCABS.SETL.STATEMENT(+1)     CABSPRNT*
      * OUTPUTS     : REPORT   SYSOUT                         CABSPRNT*
      * CONTROL     : CTLOUT                         CABSCTL          *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED              *
      *               ONE STATEMENT PER NET POSITION RECORD           *
      * RESTART     : FULL RERUN                                      *
      * STANDARDS   : CODED TO CABS-STD-014 (RECORD LAYOUTS) AND      *
      *               CABS-STD-058 (DATE HANDLING). LAST STANDARDS    *
      *               REVIEW 2016. NO WAIVERS ON FILE FOR THIS MODULE.*
      *               RECOMPILE ONLY CHANGES DO NOT REQUIRE A NEW     *
      *               DESIGN NOTE.                                    *
      * REVISION HISTORY                                              *
      *   V1.00  1990-11-26  D.OKONKWO     INITIAL                    *
      *   V1.05  1994-07-13  J.M.CASTILLO  INVOICE NUMBER ADDED       *
      *   V2.00  1997-09-30  J.M.CASTILLO  Y2K REVIEW - NO IMPACT     *
      *   V2.03  2003-01-21  P.NAIR        REMITTANCE ADVICE ADDED    *
      *   V2.05  2009-10-06  A.BUKOWSKI    PDF HANDOFF - BACKED OUT   *
      *   V2.07  2016-11-24  L.FERREIRA    RECOMPILE ONLY             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CABSET11.
       AUTHOR.        D.OKONKWO.
       DATE-WRITTEN.  1990-11-26.
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
      * NET POSITION PER COUNTERPARTY FROM CABSET09
           SELECT NET-IN-FILE
               ASSIGN TO UT-S-NETIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
      * OWNED BY THE CABS APPLICATION - CROSS APP READ
           SELECT BILL-HEADER-FILE
               ASSIGN TO DA-I-BILLHDR
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS BHF-KEY
               FILE STATUS IS WS-FS-TABLE.
      * PRINTED SETTLEMENT STATEMENTS - ASA CARRIAGE CONTROL
           SELECT STATEMENT-FILE
               ASSIGN TO UT-S-STMTOUT
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
       FD  NET-IN-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 180 CHARACTERS
               DATA RECORD IS NTI-RECORD.
       01  NTI-RECORD              PIC X(180).

       FD  BILL-HEADER-FILE
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 400 CHARACTERS
               DATA RECORD IS BHF-RECORD.
       01  BHF-RECORD.
           05  BHF-KEY                 PIC X(19).
           05  BHF-DATA                PIC X(381).

       FD  STATEMENT-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 133 CHARACTERS
               DATA RECORD IS STO-RECORD.
       01  STO-RECORD              PIC X(133).

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
           05  WS-PGM-NAME         PIC X(08)         VALUE 'CABSET11'.
           05  WS-PGM-VERSION        PIC X(05)           VALUE 'V2.07'.
           05  WS-PGM-APPL           PIC X(04)           VALUE 'SETL'.
           05  WS-PGM-COMPILE      PIC X(08)         VALUE '20161124'.
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

       COPY CABSBHDR.

       COPY CABSPRNT.

       COPY CABSCARR.

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
      * LAYOUT AGREED WITH THE CARRIER GATEWAY TEAM, CR-3318.
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
           05  WS-PE-STMT-YYDDD        PIC 9(05).
           05  WS-PE-SUPP-ZERO         PIC X(01).
           05  WS-PE-FILLER            PIC X(29).
       01  WS-PARM-OLD REDEFINES WS-PARM-CARD.
      * THE 1989 CARD.  TWO REGIONS NEVER CONVERTED THEIR JCL AND
      * THE EDIT STILL ACCEPTS IT.  THE FIELDS ARE IN A DIFFERENT
      * ORDER AND THE CYCLE DATE IS SIX DIGITS YYMMDD, NOT YYDDD.
           05  FILLER                  PIC X(02).
           05  WS-PO-CYCLE             PIC 9(05).
           05  WS-PO-RUN-ID            PIC X(12).
           05  WS-PO-YYMMDD            PIC 9(06).
           05  WS-PO-STMT              PIC 9(05).
           05  FILLER                  PIC X(20).

      * LOCAL SWITCHES.  THE STANDARD ONES ARRIVE VIA CABSWRK.
       01  WS-LOCAL-SWITCHES.
           05  WS-PARM-EOF-SW          PIC X(01)             VALUE 'N'.
                   88  WS-PARM-EOF             VALUE 'Y'.
           05  WS-INV-FOUND-SW         PIC X(01)             VALUE 'N'.
                   88  WS-INV-FOUND            VALUE 'Y'.
           05  WS-SUPPRESS-SW          PIC X(01)             VALUE 'N'.
                   88  WS-SUPPRESSED           VALUE 'Y'.

      * SUBSCRIPTS AND INDEX WORK FIELDS.
       01  WS-SUBSCRIPTS.
           05  WS-SUB1                 PIC S9(05) COMP-3     VALUE 0.
           05  WS-SUB2                 PIC S9(05) COMP-3     VALUE 0.

      * THE NET POSITION RECORD AS WRITTEN BY CABSET09.  DECLARED
      * HERE AND THERE - THERE IS STILL NO COPYBOOK FOR IT.
       01  WS-NET-IN.
           05  WS-NI-OCN             PIC X(04)           VALUE SPACES.
           05  WS-NI-PERIOD            PIC 9(06)             VALUE 0.
           05  WS-NI-RECV              PIC S9(13)V9(02)      VALUE 0.
           05  WS-NI-PAY               PIC S9(13)V9(02)      VALUE 0.
           05  WS-NI-NET               PIC S9(13)V9(02)      VALUE 0.
           05  WS-NI-DIRECTION       PIC X(01)           VALUE SPACES.
           05  WS-NI-TERMS             PIC 9(03)             VALUE 0.
           05  WS-NI-DUE-YYDDD         PIC 9(05)             VALUE 0.
           05  WS-NI-FILLER          PIC X(106)          VALUE SPACES.
       01  WS-NET-IN-K REDEFINES WS-NET-IN.
           05  WS-NIK-KEY              PIC X(10).
           05  WS-NIK-REST             PIC X(170).

      * STATEMENT WORK AREA.
       01  WS-STMT-WORK.
           05  WS-SW-NET               PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-SW-TRUNC             PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-SW-RECV              PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-SW-PAY               PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-SW-DIFF              PIC S9(05)V9(05) COMP-3 VALUE 0.

      * STATEMENT TOTALS.  THE TRUNCATION LOSS IS THE SUM OF THE
      * FRACTIONS THROWN AWAY BY THE STATEMENT ROUNDING.  IT IS
      * DISPLAYED AT END OF RUN AND POSTED NOWHERE.
       01  WS-STMT-TOTALS.
           05  WS-STMT-CNT             PIC S9(09) COMP-3     VALUE 0.
           05  WS-INV-CNT              PIC S9(09) COMP-3     VALUE 0.
           05  WS-ZERO-CNT             PIC S9(09) COMP-3     VALUE 0.
           05  WS-TOT-STMT-AMT         PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-TOT-TRUNC-LOSS       PIC S9(09)V9(05) COMP-3 VALUE 0.

      * STATEMENT PAGE LAYOUTS.  THE STATEMENT IS A PRINTED
      * DOCUMENT SENT TO THE COUNTERPARTY CARRIER.
       01  WS-STMT-HEAD.
           05  FILLER                  PIC X(01)             VALUE '1'.
           05  FILLER  PIC X(30)  VALUE 'INTER CARRIER SETTLEMENT'.
           05  FILLER            PIC X(12)       VALUE '  STATEMENT '.
           05  WS-SH-PERIOD            PIC 9(06).
           05  FILLER                PIC X(84)           VALUE SPACES.
       01  WS-STMT-PARTY.
           05  FILLER                  PIC X(01)             VALUE ' '.
           05  FILLER              PIC X(10)         VALUE 'CARRIER   '.
           05  WS-SP-OCN               PIC X(04).
           05  FILLER                PIC X(02)           VALUE SPACES.
           05  WS-SP-NAME              PIC X(40).
           05  FILLER                PIC X(76)           VALUE SPACES.
       01  WS-STMT-LINE.
           05  FILLER                  PIC X(01)             VALUE ' '.
           05  FILLER                PIC X(04)           VALUE SPACES.
           05  WS-SL-DESC              PIC X(40).
           05  FILLER                PIC X(04)           VALUE SPACES.
           05  WS-SL-AMOUNT            PIC ZZZ,ZZZ,ZZZ,ZZ9.99-.
           05  FILLER                PIC X(62)           VALUE SPACES.
       01  WS-STMT-REMIT.
           05  FILLER                  PIC X(01)             VALUE '-'.
           05  FILLER  PIC X(20)  VALUE 'REMITTANCE DUE BY   '.
           05  WS-SR-DUE               PIC 9(05).
           05  FILLER            PIC X(14)       VALUE '   INVOICE    '.
           05  WS-SR-INVOICE           PIC X(14).
           05  FILLER                PIC X(79)           VALUE SPACES.

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
                   VALUE 'SETTLEMENT STATEMENT CONTROL REGISTER'.
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
           05  FILLER              PIC X(45)
                  VALUE 'OCN  PERIOD  RECEIVABLE      PAYABLE         '.
           05  FILLER  PIC X(26)  VALUE 'NET             D  INVOICE'.
       01  WS-HEAD-4.
           05  FILLER                  PIC X(01)             VALUE ' '.
           05  FILLER                PIC X(131)          VALUE ALL '-'.

      * DETAIL LINE WS-DETAIL-1.
       01  WS-DETAIL-1.
           05  FILLER                  PIC X(01)             VALUE ' '.
           05  WS-D1-OCN               PIC X(04).
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-PERIOD            PIC 9(06).
           05  FILLER                PIC X(02)           VALUE SPACES.
           05  WS-D1-RECV              PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-PAY               PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-NET               PIC ZZZ,ZZZ,ZZ9.99-.
           05  FILLER                PIC X(02)           VALUE SPACES.
           05  WS-D1-DIR               PIC X(01).
           05  FILLER                PIC X(02)           VALUE SPACES.
           05  WS-D1-INVOICE           PIC X(14).

      * MESSAGE TEXT TABLE.  ADDRESSED BY ERROR CODE SEQUENCE, NOT
      * BY THE CODE ITSELF - INSERTING A CODE IN THE MIDDLE SHIFTS
      * EVERY MESSAGE.  THIS HAS HAPPENED TWICE (1994 AND 2007).
       01  WS-MSG-CONST.
           05  FILLER              PIC X(44)
                   VALUE 'STATEMENT PRODUCED                          '.
           05  FILLER              PIC X(44)
                   VALUE 'ZERO NET - STATEMENT SUPPRESSED             '.
           05  FILLER              PIC X(44)
                   VALUE 'INVOICE NUMBER TAKEN FROM CABS              '.
           05  FILLER              PIC X(44)
                   VALUE 'NO BILL HEADER - INVOICE LEFT BLANK         '.
           05  FILLER              PIC X(44)
                   VALUE 'RECEIVABLE STATEMENT                        '.
           05  FILLER              PIC X(44)
                   VALUE 'PAYABLE STATEMENT                           '.
           05  FILLER              PIC X(44)
                   VALUE 'NET TRUNCATED FOR THE STATEMENT             '.
           05  FILLER              PIC X(44)
                   VALUE 'REMITTANCE ADVICE ATTACHED                  '.
           05  FILLER              PIC X(44)
                   VALUE 'DUE DATE FROM THE NET POSITION RECORD       '.
           05  FILLER              PIC X(44)
                   VALUE 'END OF STATEMENT RUN                        '.
       01  WS-MSG-TABLE REDEFINES WS-MSG-CONST.
           05  WS-MSG-ENTRY OCCURS 10 TIMES
                   INDEXED BY WS-MSG-IX.
               10  WS-MSG-TEXT             PIC X(44).

      * BILL HEADER KEY BUILD AREA.  THE KEY LAYOUT BELONGS TO THE
      * CABS APPLICATION AND IS REPRODUCED HERE.
       01  WS-BH-KEY-BUILD.
           05  WS-BK-OCN-PART        PIC X(13)           VALUE SPACES.
           05  WS-BK-PERIOD-PART       PIC 9(06)             VALUE 0.
       01  WS-INVOICE-AREA.
           05  WS-INVOICE-HOLD       PIC X(14)           VALUE SPACES.

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
           OPEN INPUT  NET-IN-FILE
                       BILL-HEADER-FILE
                       PARM-FILE
           OPEN OUTPUT STATEMENT-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 6101 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-NETIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 6102 TO WS-AB-CODE
               MOVE 'OPEN FAILED DA-I-BILLHDR' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 6103 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-STMTOUT' TO WS-AB-TEXT
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
           MOVE ZERO TO WS-STMT-CNT WS-INV-CNT
                        WS-ZERO-CNT WS-TOT-STMT-AMT
                        WS-TOT-TRUNC-LOSS.
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
      * VALUES ARE SUPPLIED BY THE SCHEDULER PER CABS-STD-022.
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
           IF WS-PE-STMT-YYDDD NOT NUMERIC
               MOVE WS-PC-CYCLE TO WS-PE-STMT-YYDDD.
           IF WS-PE-SUPP-ZERO NOT = 'N'
               MOVE 'Y' TO WS-PE-SUPP-ZERO.

       P1200-EXIT.
           EXIT.


      *****************************************************************
      * S200-STATEMENT                                                *
      * PRODUCE ONE STATEMENT PER PARTY.                              *
      *****************************************************************
       S200-STATEMENT SECTION.

       P2000-PROCESS.
      * ONE NET POSITION RECORD PER PASS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ THRU P2100-EXIT.
           IF WS-EOF
               GO TO P2000-EXIT.
           MOVE NTI-RECORD TO WS-NET-IN.
           MOVE WS-NIK-KEY TO WS-RESTART-KEY.
           MOVE 'N' TO WS-SUPPRESS-SW.
           IF WS-NI-NET = ZERO
               IF WS-PE-SUPP-ZERO = 'Y'
                   ADD 1 TO WS-ZERO-CNT
                   ADD 1 TO WS-REJECT-CNT
                   GO TO P2000-EXIT.
           PERFORM P2200-ROUND-NET THRU P2200-EXIT.
           PERFORM P2400-GET-INVOICE THRU P2400-EXIT.
           PERFORM P3000-PRINT-STATEMENT THRU P3000-EXIT.
           PERFORM P6100-DETAIL THRU P6100-EXIT.
           ADD 1 TO WS-WRITE-CNT.
           ADD 1 TO WS-STMT-CNT.

       P2000-EXIT.
           EXIT.

       P2100-READ.
      * SEQUENTIAL READ OF THE NET FILE.
           READ NET-IN-FILE
               AT END
                   MOVE 'Y' TO WS-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00'
               MOVE 3610 TO WS-AB-CODE
               MOVE 'READ ERROR UT-S-NETIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-READ-CNT.

       P2100-EXIT.
           EXIT.

       P2200-ROUND-NET.
      * THE NET AMOUNT IS MOVED TO A TWO PLACE FIELD WITHOUT A
      * ROUNDED CLAUSE, SO ANY FRACTION IS TRUNCATED.  CABSET09
      * ROUNDS THE SAME FIGURE WHEN IT WRITES THE NET FILE.  THE
      * STATEMENT AND THE NET FILE THEREFORE DISAGREE BY UP TO A
      * CENT PER CARRIER PER MONTH.
           MOVE 'P2200-ROUND-NET' TO WS-PARA-NAME.
           MOVE WS-NI-NET TO WS-SW-NET.
           MOVE WS-NI-RECV TO WS-SW-RECV.
           MOVE WS-NI-PAY TO WS-SW-PAY.
           COMPUTE WS-SW-TRUNC = WS-SW-RECV - WS-SW-PAY.
           COMPUTE WS-SW-DIFF = WS-SW-NET - WS-SW-TRUNC.
           ADD WS-SW-DIFF TO WS-TOT-TRUNC-LOSS.
           ADD WS-SW-TRUNC TO WS-TOT-STMT-AMT.
           ADD WS-SW-TRUNC TO WS-ACC-AMOUNT.

       P2200-EXIT.
           EXIT.

       P2400-GET-INVOICE.
      * THE STATEMENT CARRIES THE INVOICE NUMBER FROM THE BILLING
      * APPLICATION SO THAT THE CARRIER CAN TIE THE SETTLEMENT TO
      * THE ACCESS BILL.  THE BILL HEADER FILE BELONGS TO CABS AND
      * IS READ DIRECTLY.
           MOVE 'P2400-GET-INVOICE' TO WS-PARA-NAME.
           MOVE 'N' TO WS-INV-FOUND-SW.
           MOVE SPACES TO WS-INVOICE-HOLD.
           MOVE SPACES TO WS-BH-KEY-BUILD.
           MOVE WS-NI-OCN TO WS-BK-OCN-PART.
           MOVE WS-NI-PERIOD TO WS-BK-PERIOD-PART.
           MOVE WS-BH-KEY-BUILD TO BHF-KEY.
           READ BILL-HEADER-FILE
               INVALID KEY
                   GO TO P2400-EXIT.
           MOVE BHF-RECORD TO CABS-BILL-HEADER.
           MOVE BH-INVOICE-NBR TO WS-INVOICE-HOLD.
           MOVE 'Y' TO WS-INV-FOUND-SW.
           ADD 1 TO WS-INV-CNT.

       P2400-EXIT.
           EXIT.


      *****************************************************************
      * S300-PRINT                                                    *
      * PRINT THE STATEMENT PAGES.                                    *
      *****************************************************************
       S300-PRINT SECTION.

       P3000-PRINT-STATEMENT.
      * ONE STATEMENT PER COUNTERPARTY.  THE PAGE STARTS ON A NEW
      * SHEET AND THE REMITTANCE ADVICE IS ON THE SAME PAGE - THE
      * 2009 CHANGE TO SPLIT THEM ONTO SEPARATE PAGES WAS BACKED
      * OUT AFTER THE FIRST RUN.
           MOVE 'P3000-PRINT-STATEMENT' TO WS-PARA-NAME.
           MOVE WS-NI-PERIOD TO WS-SH-PERIOD.
           MOVE WS-STMT-HEAD TO STO-RECORD.
           WRITE STO-RECORD.
           MOVE WS-NI-OCN TO WS-SP-OCN.
           MOVE SPACES TO WS-SP-NAME.
           MOVE WS-STMT-PARTY TO STO-RECORD.
           WRITE STO-RECORD.
           PERFORM P3100-PRINT-LINES THRU P3100-EXIT.
           PERFORM P3200-PRINT-REMIT THRU P3200-EXIT.

       P3000-EXIT.
           EXIT.

       P3100-PRINT-LINES.
      * THE THREE MONEY LINES - RECEIVABLE, PAYABLE AND NET.
           MOVE 'AMOUNTS DUE TO US' TO WS-SL-DESC.
           MOVE WS-SW-RECV TO WS-SL-AMOUNT.
           MOVE WS-STMT-LINE TO STO-RECORD.
           WRITE STO-RECORD.
           MOVE 'AMOUNTS DUE TO YOU' TO WS-SL-DESC.
           MOVE WS-SW-PAY TO WS-SL-AMOUNT.
           MOVE WS-STMT-LINE TO STO-RECORD.
           WRITE STO-RECORD.
           MOVE 'NET SETTLEMENT POSITION' TO WS-SL-DESC.
           MOVE WS-SW-TRUNC TO WS-SL-AMOUNT.
           MOVE WS-STMT-LINE TO STO-RECORD.
           WRITE STO-RECORD.

       P3100-EXIT.
           EXIT.

       P3200-PRINT-REMIT.
      * THE REMITTANCE ADVICE LINE.
           MOVE WS-NI-DUE-YYDDD TO WS-SR-DUE.
           MOVE WS-INVOICE-HOLD TO WS-SR-INVOICE.
           MOVE WS-STMT-REMIT TO STO-RECORD.
           WRITE STO-RECORD.

       P3200-EXIT.
           EXIT.


      *****************************************************************
      * S600-REPORT                                                   *
      * CONTROL REGISTER.                                             *
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
      * ONE LINE PER STATEMENT.
           IF WS-LINE-CNT > WS-MAX-LINES
               PERFORM P6000-HEADING THRU P6000-EXIT.
           MOVE WS-NI-OCN TO WS-D1-OCN.
           MOVE WS-NI-PERIOD TO WS-D1-PERIOD.
           MOVE WS-SW-RECV TO WS-D1-RECV.
           MOVE WS-SW-PAY TO WS-D1-PAY.
           MOVE WS-SW-TRUNC TO WS-D1-NET.
           MOVE WS-NI-DIRECTION TO WS-D1-DIR.
           MOVE WS-INVOICE-HOLD TO WS-D1-INVOICE.
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
           MOVE WS-NET-IN TO SU-ORIG-RECORD.
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
           MOVE 300                    TO CT-STEP-SEQ.
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
           DISPLAY 'STATEMENTS       ' WS-STMT-CNT.
           DISPLAY 'INVOICE MATCHED  ' WS-INV-CNT.
           DISPLAY 'ZERO SUPPRESSED  ' WS-ZERO-CNT.
           DISPLAY 'STATEMENT TOTAL  ' WS-TOT-STMT-AMT.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE NET-IN-FILE
                 BILL-HEADER-FILE
                 STATEMENT-FILE
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

