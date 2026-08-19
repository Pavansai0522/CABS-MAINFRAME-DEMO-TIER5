      *****************************************************************
      * CABJUR06 - DEFAULT FACTOR FALLBACK AND DERIVATION             *
      * APPLICATION : CABS                                            *
      * INPUTS      : CARRMAST TELCABS.CABS.CARRIER           CABSCARR*
      * INPUTS      : FCTRMAST TELCABS.CABS.FACTOR            CABSFCTR*
      * INPUTS      : SYSIN    INSTREAM                       NONE    *
      * OUTPUTS     : FCTRDEF  TELCABS.CABS.FACTOR.DEF(+1)    CABSFCTR*
      * OUTPUTS     : REPORT   SYSOUT                         CABSPRNT*
      * CONTROL     : CTLOUT                         CABSCTL          *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARIS*
      *               SUMMARISED = CARRIERS THAT ALREADY HAVE A FACTOR*
      * RESTART     : FULL RERUN                                      *
      * STANDARDS   : CODED TO CABS-STD-022 (CONTROL CARDS) AND       *
      *               CABS-STD-026 (GENERATION FILES). REVIEWED AT THE*
      *               2016 STANDARDS SWEEP. NO WAIVERS ON FILE.       *
      * REVISION HISTORY                                              *
      *   V1.00  1994-09-12  J.M.CASTILLO  INITIAL                    *
      *   V1.02  1995-06-27  J.M.CASTILLO  STUDY DERIVED PSU          *
      *   V2.00  1996-07-08  J.M.CASTILLO  Y2K REVIEW - NO IMPACT     *
      *   V2.01  2001-01-19  P.NAIR        TARIFF DEFAULT TABLE       *
      *   V2.04  2006-03-24  A.BUKOWSKI    WIRELESS OCN EXCLUDED      *
      *   V2.06  2011-06-30  A.BUKOWSKI    PSU STUDY PATH DISABLED    *
      *   V2.07  2014-02-11  L.FERREIRA    REPORT ONLY CHANGE         *
      *   V2.08  2018-09-28  M.OYELARAN    RECOMPILE ONLY             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CABJUR06.
       AUTHOR.        J.M.CASTILLO.
       DATE-WRITTEN.  1994-09-12.
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
      * CARRIER MASTER READ IN OCN SEQUENCE
           SELECT CARRIER-MASTER
               ASSIGN TO DA-I-CARRMAST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS CRM-KEY
               FILE STATUS IS WS-FS-INPUT.
      * FACTOR MASTER - TESTED FOR AN EXISTING FILING
           SELECT FACTOR-MASTER
               ASSIGN TO DA-I-FCTRMAST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS FCM-KEY
               FILE STATUS IS WS-FS-TABLE.
      * DERIVED DEFAULT FACTORS - MERGED BACK BY CABJ1500
           SELECT FACTOR-DEF-FILE
               ASSIGN TO UT-S-FCTRDEF
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
       FD  CARRIER-MASTER
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 120 CHARACTERS
               DATA RECORD IS CRM-RECORD.
       01  CRM-RECORD.
           05  CRM-KEY                 PIC X(04).
           05  CRM-DATA                PIC X(116).

       FD  FACTOR-MASTER
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 76 CHARACTERS
               DATA RECORD IS FCM-RECORD.
       01  FCM-RECORD.
           05  FCM-KEY                 PIC X(14).
           05  FCM-DATA                PIC X(62).

       FD  FACTOR-DEF-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 76 CHARACTERS
               DATA RECORD IS FDF-RECORD.
       01  FDF-RECORD              PIC X(76).

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
           05  WS-PGM-NAME         PIC X(08)         VALUE 'CABJUR06'.
           05  WS-PGM-VERSION        PIC X(05)           VALUE 'V2.08'.
           05  WS-PGM-APPL           PIC X(04)           VALUE 'CABS'.
           05  WS-PGM-COMPILE      PIC X(08)         VALUE '20180928'.
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

       COPY CABSCARR.

       COPY CABSFCTR.

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
           05  WS-PE-EFF-YYDDD         PIC 9(05).
           05  WS-PE-STUDY-SW          PIC X(01).
           05  WS-PE-FILLER            PIC X(29).
       01  WS-PARM-OLD REDEFINES WS-PARM-CARD.
      * THE 1989 CARD.  TWO REGIONS NEVER CONVERTED THEIR JCL AND
      * THE EDIT STILL ACCEPTS IT.  THE FIELDS ARE IN A DIFFERENT
      * ORDER AND THE CYCLE DATE IS SIX DIGITS YYMMDD, NOT YYDDD.
           05  FILLER                  PIC X(02).
           05  WS-PO-CYCLE             PIC 9(05).
           05  WS-PO-RUN-ID            PIC X(12).
           05  WS-PO-YYMMDD            PIC 9(06).
           05  WS-PO-EFF               PIC 9(05).
           05  FILLER                  PIC X(20).

      * LOCAL SWITCHES.  THE STANDARD ONES ARRIVE VIA CABSWRK.
       01  WS-LOCAL-SWITCHES.
           05  WS-PARM-EOF-SW          PIC X(01)             VALUE 'N'.
                   88  WS-PARM-EOF             VALUE 'Y'.
           05  WS-FILED-SW             PIC X(01)             VALUE 'N'.
                   88  WS-ALREADY-FILED        VALUE 'Y'.
           05  WS-STUDY-SW             PIC X(01)             VALUE 'N'.
                   88  WS-STUDY-ENABLED        VALUE 'Y'.
           05  WS-ELIGIBLE-SW          PIC X(01)             VALUE 'Y'.
                   88  WS-ELIGIBLE             VALUE 'Y'.

      * SUBSCRIPTS AND INDEX WORK FIELDS.
       01  WS-SUBSCRIPTS.
           05  WS-SUB1                 PIC S9(05) COMP-3     VALUE 0.
           05  WS-SUB2                 PIC S9(05) COMP-3     VALUE 0.

      * DERIVATION STATISTICS.
       01  WS-DEF-COUNTERS.
           05  WS-DEF-CNT              PIC S9(09) COMP-3     VALUE 0.
           05  WS-FILED-CNT            PIC S9(09) COMP-3     VALUE 0.
           05  WS-STUDY-CNT            PIC S9(09) COMP-3     VALUE 0.
           05  WS-EXCL-CNT             PIC S9(09) COMP-3     VALUE 0.

      * DEFAULT DERIVATION WORK AREA.
       01  WS-DEF-WORK.
           05  WS-DW-PIU               PIC S9(03)V9(05) COMP-3 VALUE 0.
           05  WS-DW-PLU               PIC S9(03)V9(05) COMP-3 VALUE 0.
           05  WS-DW-PSU               PIC S9(03)V9(05) COMP-3 VALUE 0.
           05  WS-DW-STUDY-MOU         PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-DW-STUDY-IS          PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-DW-STUDY-PCT         PIC S9(03)V9(05) COMP-3 VALUE 0.

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
                   VALUE 'CARRIER DEFAULT FACTOR DERIVATION'.
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
                  VALUE 'OCN  TYPE  DEFAULT-PIU  DEFAULT-PLU  SOURCE  '.
           05  FILLER              PIC X(11)        VALUE 'DISPOSITION'.
       01  WS-HEAD-4.
           05  FILLER                  PIC X(01)             VALUE ' '.
           05  FILLER                PIC X(131)          VALUE ALL '-'.

      * DETAIL LINE WS-DETAIL-1.
       01  WS-DETAIL-1.
           05  FILLER                  PIC X(01)             VALUE ' '.
           05  WS-D1-OCN               PIC X(04).
           05  FILLER                PIC X(02)           VALUE SPACES.
           05  WS-D1-TYPE              PIC X(01).
           05  FILLER                PIC X(05)           VALUE SPACES.
           05  WS-D1-PIU               PIC ZZ9.99999.
           05  FILLER                PIC X(02)           VALUE SPACES.
           05  WS-D1-PLU               PIC ZZ9.99999.
           05  FILLER                PIC X(03)           VALUE SPACES.
           05  WS-D1-SRC               PIC X(01).
           05  FILLER                PIC X(06)           VALUE SPACES.
           05  WS-D1-DISPO             PIC X(40).

      * TARIFF DEFAULT FACTOR TABLE BY OCN.  USED ONLY WHEN THE
      * CARRIER HAS SUPPLIED NOTHING AND THE CARRIER MASTER DEFAULT
      * IS ALSO ZERO.  THE VALUES ARE THE 1998 TARIFF DEFAULTS AND
      * HAVE NEVER BEEN REFRESHED.  SEE OPEN ITEM CABS-1998-0044.
       01  WS-OCNDEF-CONST.
           05  FILLER  PIC X(21)  VALUE '1052R0330000003500000'.
           05  FILLER  PIC X(21)  VALUE '1524R0880000002000000'.
           05  FILLER  PIC X(21)  VALUE '1612W0150000003500000'.
           05  FILLER  PIC X(21)  VALUE '1762L0330000000000000'.
           05  FILLER  PIC X(21)  VALUE '1827W0250000000000000'.
           05  FILLER  PIC X(21)  VALUE '1876C0330000003500000'.
           05  FILLER  PIC X(21)  VALUE '1884C1000000000500000'.
           05  FILLER  PIC X(21)  VALUE '2348C0150000003500000'.
           05  FILLER  PIC X(21)  VALUE '2412W0150000000000000'.
           05  FILLER  PIC X(21)  VALUE '2621L0330000000500000'.
           05  FILLER  PIC X(21)  VALUE '2677R0880000001200000'.
           05  FILLER  PIC X(21)  VALUE '2851I0650000000000000'.
           05  FILLER  PIC X(21)  VALUE '2866L0250000000000000'.
           05  FILLER  PIC X(21)  VALUE '3161L0500000001200000'.
           05  FILLER  PIC X(21)  VALUE '3781L0150000000000000'.
           05  FILLER  PIC X(21)  VALUE '4332I0650000000500000'.
           05  FILLER  PIC X(21)  VALUE '4399C0150000000000000'.
           05  FILLER  PIC X(21)  VALUE '4596I0650000000000000'.
           05  FILLER  PIC X(21)  VALUE '4780L0750000004800000'.
           05  FILLER  PIC X(21)  VALUE '4856W0500000001200000'.
           05  FILLER  PIC X(21)  VALUE '4868R0330000003500000'.
           05  FILLER  PIC X(21)  VALUE '5074R0650000000000000'.
           05  FILLER  PIC X(21)  VALUE '5722I0750000004800000'.
           05  FILLER  PIC X(21)  VALUE '6001L0500000003500000'.
           05  FILLER  PIC X(21)  VALUE '6247C0650000001200000'.
           05  FILLER  PIC X(21)  VALUE '6283W0880000000500000'.
           05  FILLER  PIC X(21)  VALUE '6306I1000000000000000'.
           05  FILLER  PIC X(21)  VALUE '6576I0000000004800000'.
           05  FILLER  PIC X(21)  VALUE '6593W0250000003500000'.
           05  FILLER  PIC X(21)  VALUE '6851C0650000001200000'.
           05  FILLER  PIC X(21)  VALUE '7026W0250000000000000'.
           05  FILLER  PIC X(21)  VALUE '7195L0250000003500000'.
           05  FILLER  PIC X(21)  VALUE '7728R0880000000000000'.
           05  FILLER  PIC X(21)  VALUE '7969C0000000003500000'.
           05  FILLER  PIC X(21)  VALUE '8344W0880000003500000'.
           05  FILLER  PIC X(21)  VALUE '8400I0330000000500000'.
           05  FILLER  PIC X(21)  VALUE '8481I0000000000000000'.
           05  FILLER  PIC X(21)  VALUE '8626R0750000002000000'.
           05  FILLER  PIC X(21)  VALUE '8765I0150000001200000'.
           05  FILLER  PIC X(21)  VALUE '8832C0750000002000000'.
           05  FILLER  PIC X(21)  VALUE '8854W0150000004800000'.
           05  FILLER  PIC X(21)  VALUE '8901L0330000000000000'.
           05  FILLER  PIC X(21)  VALUE '9326R0500000004800000'.
           05  FILLER  PIC X(21)  VALUE '9538I0150000002000000'.
           05  FILLER  PIC X(21)  VALUE '9539C0750000000000000'.
           05  FILLER  PIC X(21)  VALUE '9822C1000000003500000'.
           05  FILLER  PIC X(21)  VALUE '9842R0150000001200000'.
           05  FILLER  PIC X(21)  VALUE '9986L0150000000500000'.
       01  WS-OCNDEF-TABLE REDEFINES WS-OCNDEF-CONST.
           05  WS-WS-OD-ENTRY OCCURS 48 TIMES
                   INDEXED BY WS-OD-IX.
               10  WS-OD-OCN               PIC X(04).
               10  WS-OD-TYPE              PIC X(01).
               10  WS-OD-PIU               PIC 9(03)V9(05).
               10  WS-OD-PLU               PIC 9(03)V9(05).

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

      * MESSAGE TEXT TABLE.  ADDRESSED BY ERROR CODE SEQUENCE, NOT
      * BY THE CODE ITSELF - INSERTING A CODE IN THE MIDDLE SHIFTS
      * EVERY MESSAGE.  THIS HAS HAPPENED TWICE (1994 AND 2007).
       01  WS-MSG-CONST.
           05  FILLER              PIC X(44)
                   VALUE 'FACTOR ALREADY FILED - NO DEFAULT NEEDED    '.
           05  FILLER              PIC X(44)
                   VALUE 'DEFAULT TAKEN FROM CARRIER MASTER           '.
           05  FILLER              PIC X(44)
                   VALUE 'DEFAULT TAKEN FROM TARIFF TABLE             '.
           05  FILLER              PIC X(44)
                   VALUE 'INDUSTRY FALLBACK FIFTY PERCENT APPLIED     '.
           05  FILLER              PIC X(44)
                   VALUE 'WIRELESS CARRIER - EXCLUDED SINCE 2006      '.
           05  FILLER              PIC X(44)
                   VALUE 'CARRIER INACTIVE - EXCLUDED                 '.
           05  FILLER              PIC X(44)
                   VALUE 'STUDY DERIVED PATH DISABLED 2011            '.
           05  FILLER              PIC X(44)
                   VALUE 'STUDY DERIVED FACTOR CALCULATED             '.
           05  FILLER              PIC X(44)
                   VALUE 'DEFAULT WRITTEN TO THE DERIVATION FILE      '.
           05  FILLER              PIC X(44)
                   VALUE 'EFFECTIVE DATE TAKEN FROM CONTROL CARD      '.
       01  WS-MSG-TABLE REDEFINES WS-MSG-CONST.
           05  WS-MSG-ENTRY OCCURS 10 TIMES
                   INDEXED BY WS-MSG-IX.
               10  WS-MSG-TEXT             PIC X(44).

      * FACTOR MASTER KEY BUILD AREA.
       01  WS-FCTR-KEY.
           05  WS-FK-OCN             PIC X(04)           VALUE SPACES.
           05  WS-FK-STATE           PIC X(02)           VALUE SPACES.
           05  WS-FK-LATA              PIC 9(03)             VALUE 0.
           05  WS-FK-EFF               PIC 9(05)             VALUE 0.
       01  WS-DERIVE-CTL.
           05  WS-DEF-SOURCE           PIC X(01)             VALUE 'D'.

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
           OPEN INPUT  CARRIER-MASTER
                       FACTOR-MASTER
                       PARM-FILE
           OPEN OUTPUT FACTOR-DEF-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 4601 TO WS-AB-CODE
               MOVE 'OPEN FAILED DA-I-CARRMAST' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 4602 TO WS-AB-CODE
               MOVE 'OPEN FAILED DA-I-FCTRMAST' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 4603 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-FCTRDEF' TO WS-AB-TEXT
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
           MOVE ZERO TO WS-DEF-CNT WS-FILED-CNT
                        WS-STUDY-CNT WS-EXCL-CNT.
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
      * THE SUBMISSION STANDARD IS CABS-STD-022 - NOTHING IS DEFAULTED.
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
           IF WS-PE-EFF-YYDDD NOT NUMERIC
               MOVE WS-PC-CYCLE TO WS-PE-EFF-YYDDD.

       P1200-EXIT.
           EXIT.


      *****************************************************************
      * S200-DERIVATION                                               *
      * DECIDE WHETHER EACH CARRIER NEEDS A DEFAULT FACTOR.           *
      *****************************************************************
       S200-DERIVATION SECTION.

       P2000-PROCESS.
      * ONE CARRIER PER PASS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ THRU P2100-EXIT.
           IF WS-EOF
               GO TO P2000-EXIT.
           MOVE CRM-RECORD TO CABS-CARRIER-RECORD.
           MOVE CR-OCN TO WS-RESTART-KEY.
           MOVE SPACES TO WS-D1-DISPO.
           MOVE 'Y' TO WS-ELIGIBLE-SW.
           PERFORM P2200-ELIGIBILITY THRU P2200-EXIT.
           IF NOT WS-ELIGIBLE
               ADD 1 TO WS-EXCL-CNT
               ADD 1 TO WS-SUMM-CNT
               PERFORM P6100-DETAIL THRU P6100-EXIT
               GO TO P2000-EXIT.
           PERFORM P2300-TEST-FILED THRU P2300-EXIT.
           IF WS-ALREADY-FILED
               ADD 1 TO WS-FILED-CNT
               ADD 1 TO WS-SUMM-CNT
               MOVE WS-MSG-TEXT (1) TO WS-D1-DISPO
               PERFORM P6100-DETAIL THRU P6100-EXIT
               GO TO P2000-EXIT.
           PERFORM P2400-DERIVE-DEFAULT THRU P2400-EXIT.
           PERFORM P5000-STUDY-DERIVED THRU P5000-EXIT.
           PERFORM P3000-WRITE-DEFAULT THRU P3000-EXIT.
           PERFORM P6100-DETAIL THRU P6100-EXIT.

       P2000-EXIT.
           EXIT.

       P2100-READ.
      * SEQUENTIAL PASS OF THE CARRIER MASTER.
           READ CARRIER-MASTER
               AT END
                   MOVE 'Y' TO WS-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00'
               MOVE 3460 TO WS-AB-CODE
               MOVE 'READ ERROR DA-I-CARRMAST' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-READ-CNT.

       P2100-EXIT.
           EXIT.

       P2200-ELIGIBILITY.
      * WIRELESS CARRIERS WERE EXCLUDED IN 2006 WHEN THE TERMINATING
      * COMPENSATION ARRANGEMENT MOVED TO A SEPARATE AGREEMENT.  AN
      * INACTIVE CARRIER IS EXCLUDED TOO.
           IF CR-WIRELESS
               MOVE 'N' TO WS-ELIGIBLE-SW
               MOVE WS-MSG-TEXT (5) TO WS-D1-DISPO
               GO TO P2200-EXIT.
           IF CR-ACTIVE-SW NOT = 'A'
               MOVE 'N' TO WS-ELIGIBLE-SW
               MOVE WS-MSG-TEXT (6) TO WS-D1-DISPO
               GO TO P2200-EXIT.
           IF NOT CR-BILLED-PARTY
               MOVE 'N' TO WS-ELIGIBLE-SW.

       P2200-EXIT.
           EXIT.

       P2300-TEST-FILED.
      * A CARRIER THAT HAS FILED A FACTOR FOR THE EFFECTIVE DATE ON
      * THE CONTROL CARD DOES NOT NEED A DEFAULT.  THE KEY IS BUILT
      * WITH A BLANK STATE AND ZERO LATA - THE CARRIER LEVEL FILING.
           MOVE SPACES TO WS-FCTR-KEY.
           MOVE CR-OCN TO WS-FK-OCN.
           MOVE SPACES TO WS-FK-STATE.
           MOVE ZERO TO WS-FK-LATA.
           MOVE WS-PE-EFF-YYDDD TO WS-FK-EFF.
           MOVE WS-FCTR-KEY TO FCM-KEY.
           MOVE 'N' TO WS-FILED-SW.
           READ FACTOR-MASTER
               INVALID KEY
                   GO TO P2300-EXIT.
           MOVE 'Y' TO WS-FILED-SW.

       P2300-EXIT.
           EXIT.

       P2400-DERIVE-DEFAULT.
      * THE CARRIER MASTER DEFAULT IS PREFERRED.  IF IT IS ZERO THE
      * 1998 TARIFF TABLE IS SEARCHED.  IF THAT FAILS TOO THE FIFTY
      * PERCENT INDUSTRY FALLBACK IS USED - A NUMBER THAT HAS NO
      * TARIFF BASIS AND HAS SURVIVED FOUR AUDITS.
           MOVE CR-DEFAULT-PIU TO WS-DW-PIU.
           MOVE CR-DEFAULT-PLU TO WS-DW-PLU.
           IF WS-DW-PIU NOT = ZERO
               MOVE WS-MSG-TEXT (2) TO WS-D1-DISPO
               MOVE 'C' TO WS-DEF-SOURCE
               GO TO P2400-EXIT.
           SET WS-OD-IX TO 1.
           SEARCH WS-OD-ENTRY
               AT END
                   MOVE 050.00000 TO WS-DW-PIU
                   MOVE 000.00000 TO WS-DW-PLU
                   MOVE 'D' TO WS-DEF-SOURCE
                   MOVE WS-MSG-TEXT (4) TO WS-D1-DISPO
                   GO TO P2400-EXIT
               WHEN WS-OD-OCN (WS-OD-IX) = CR-OCN
                   MOVE WS-OD-PIU (WS-OD-IX) TO WS-DW-PIU
                   MOVE WS-OD-PLU (WS-OD-IX) TO WS-DW-PLU
                   MOVE 'T' TO WS-DEF-SOURCE
                   MOVE WS-MSG-TEXT (3) TO WS-D1-DISPO.

       P2400-EXIT.
           EXIT.


      *****************************************************************
      * S500-STUDY-DERIVED                                            *
      * TRAFFIC STUDY DERIVATION.  SEE CABS-STD-022.                  *
      *****************************************************************
       S500-STUDY-DERIVED SECTION.

       P5000-STUDY-DERIVED.
      * THE STUDY DERIVED PATH CALCULATES A PSU FROM THE PRIOR TWO
      * QUARTERS OF ACTUAL TRAFFIC.  THE TRAFFIC STUDY FEED WAS
      * RESTRUCTURED IN JUNE 2011 AND THE PATH WAS PLACED UNDER THE
      * STUDY SWITCH ON THE CONTROL CARD AT THAT TIME.  THE SWITCH
      * IS OWNED BY THE FACTOR ADMINISTRATION GROUP AND IS SET PER
      * CABS-STD-022; IT IS NOT DEFAULTED BY THE PROGRAM.  THE
      * DISPOSITION MESSAGE IS WRITTEN EITHER WAY.
           MOVE 'P5000-STUDY-DERIVED' TO WS-PARA-NAME.
           IF WS-PE-STUDY-SW NOT = 'Y'
               MOVE WS-MSG-TEXT (7) TO WS-D1-DISPO
               GO TO P5000-EXIT.
           MOVE ZERO TO WS-DW-STUDY-MOU.
           MOVE ZERO TO WS-DW-STUDY-IS.
           MOVE CR-DEFAULT-PIU TO WS-DW-STUDY-PCT.
           IF WS-DW-STUDY-MOU = ZERO
               GO TO P5000-EXIT.
           COMPUTE WS-DW-STUDY-PCT ROUNDED =
                   (WS-DW-STUDY-IS / WS-DW-STUDY-MOU) * 100.
           MOVE WS-DW-STUDY-PCT TO WS-DW-PIU.
           MOVE 'S' TO WS-DEF-SOURCE.
           ADD 1 TO WS-STUDY-CNT.
           MOVE WS-MSG-TEXT (8) TO WS-D1-DISPO.

       P5000-EXIT.
           EXIT.


      *****************************************************************
      * S300-OUTPUT                                                   *
      * WRITE THE DERIVED FACTOR RECORD.                              *
      *****************************************************************
       S300-OUTPUT SECTION.

       P3000-WRITE-DEFAULT.
      * BUILD A FACTOR RECORD FROM THE DERIVED VALUES.  RESTATEMENT
      * IS NEVER FLAGGED ON A DERIVED FACTOR - A DEFAULT IS NOT A
      * FILING AND CANNOT SUPPORT A RETROACTIVE ADJUSTMENT.
           MOVE SPACES TO CABS-FACTOR-RECORD.
           MOVE CR-OCN TO FC-OCN.
           MOVE SPACES TO FC-STATE-CD.
           MOVE ZERO TO FC-LATA.
           MOVE WS-PE-EFF-YYDDD TO FC-EFF-YYDDD.
           MOVE WS-DW-PIU TO FC-PIU.
           MOVE WS-DW-PLU TO FC-PLU.
           MOVE ZERO TO FC-PSU.
           MOVE WS-DEF-SOURCE TO FC-SOURCE.
           MOVE 'N' TO FC-RESTATE-SW.
           MOVE ZERO TO FC-RESTATE-FROM-YYDDD.
           MOVE ZERO TO FC-RESTATE-THRU-YYDDD.
           MOVE ZERO TO FC-PRIOR-PIU.
           MOVE ZERO TO FC-PRIOR-PLU.
           MOVE WS-CYCLE-YYDDD TO FC-RECV-YYDDD.
           MOVE CABS-FACTOR-RECORD TO FDF-RECORD.
           WRITE FDF-RECORD.
           ADD 1 TO WS-WRITE-CNT.
           ADD 1 TO WS-DEF-CNT.

       P3000-EXIT.
           EXIT.


      *****************************************************************
      * S600-REPORT                                                   *
      * DERIVATION REGISTER.                                          *
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
      * ONE LINE PER CARRIER.
           IF WS-LINE-CNT > WS-MAX-LINES
               PERFORM P6000-HEADING THRU P6000-EXIT.
           MOVE CR-OCN TO WS-D1-OCN.
           MOVE CR-TYPE TO WS-D1-TYPE.
           MOVE WS-DW-PIU TO WS-D1-PIU.
           MOVE WS-DW-PLU TO WS-D1-PLU.
           MOVE WS-DEF-SOURCE TO WS-D1-SRC.
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
           MOVE CABS-CARRIER-RECORD TO SU-ORIG-RECORD.
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
           MOVE 060                    TO CT-STEP-SEQ.
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
           DISPLAY 'CARRIERS READ    ' WS-READ-CNT.
           DISPLAY 'DEFAULTS WRITTEN ' WS-DEF-CNT.
           DISPLAY 'ALREADY FILED    ' WS-FILED-CNT.
           DISPLAY 'STUDY DERIVED    ' WS-STUDY-CNT.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE CARRIER-MASTER
                 FACTOR-MASTER
                 FACTOR-DEF-FILE
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

