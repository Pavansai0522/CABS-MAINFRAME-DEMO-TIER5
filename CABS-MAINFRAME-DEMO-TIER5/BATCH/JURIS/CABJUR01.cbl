      *****************************************************************
      * CABJUR01 - PIU/PLU FACTOR TABLE LOAD                          *
      * APPLICATION : CABS                                            *
      * INPUTS      : FCTRIN   TELCABS.CABS.FACTOR.IN(0)      CABSFCTR*
      * INPUTS      : CARRMAST TELCABS.CABS.CARRIER           CABSCARR*
      * INPUTS      : SYSIN    INSTREAM                       NONE    *
      * OUTPUTS     : FCTRMAST TELCABS.CABS.FACTOR            CABSFCTR*
      * OUTPUTS     : SUSPOUT  TELCABS.CABS.SUSPENSE(+1)      CABSERR *
      * CONTROL     : CTLOUT                         CABSCTL          *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED + CT-CARRIED-*
      *               CARRIED FWD = FACTORS DATED AFTER THE CYCLE DATE*
      * RESTART     : FULL RERUN - THE VSAM LOAD IS IDEMPOTENT BY KEY *
      * STANDARDS   : CODED TO CABS-STD-022 (CONTROL CARDS) AND       *
      *               CABS-STD-026 (GENERATION FILES).                *
      * REVISION HISTORY                                              *
      *   V1.00  1987-04-02  R.T.WHEELER   INITIAL - PIU ONLY         *
      *   V1.03  1989-11-27  D.OKONKWO     PLU ADDED FOR LOCAL        *
      *   V1.07  1992-06-15  D.OKONKWO     DISPUTED FACTOR HOLD       *
      *   V2.00  1996-02-01  J.M.CASTILLO  Y2K - PIVOT 70 APPLIED     *
      *   V2.02  1998-09-14  P.NAIR        PSU FIELD LOADED           *
      *   V2.05  2003-05-08  P.NAIR        DUP KEY NOW REWRITES       *
      *   V2.06  2007-01-22  A.BUKOWSKI    REMOVED CARRIER EDIT       *
      *   V2.08  2011-10-03  A.BUKOWSKI    PSU LOAD SUPPRESSED        *
      *   V2.09  2016-07-19  L.FERREIRA    RECOMPILE ONLY LE V6       *
      *   V2.09  2019-03-11  M.OYELARAN    NO CODE CHANGE - AUDIT     *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CABJUR01.
       AUTHOR.        R.T.WHEELER.
       DATE-WRITTEN.  1987-04-02.
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
      * QUARTERLY FACTOR FEED - GDG ZERO IS TODAYS ARRIVAL
           SELECT FACTOR-IN-FILE
               ASSIGN TO UT-S-FCTRIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
      * FACTOR MASTER KSDS - KEYED OCN/STATE/LATA/EFFDATE
           SELECT FACTOR-MASTER
               ASSIGN TO DA-I-FCTRMAST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS FCM-KEY
               FILE STATUS IS WS-FS-OUTPUT.
      * CARRIER MASTER - SUPPLIES THE DEFAULT FACTORS
           SELECT CARRIER-MASTER
               ASSIGN TO DA-I-CARRMAST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CRM-KEY
               FILE STATUS IS WS-FS-TABLE.
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

       DATA DIVISION.
       FILE SECTION.
       FD  FACTOR-IN-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 76 CHARACTERS
               DATA RECORD IS FCI-RECORD.
       01  FCI-RECORD              PIC X(76).

       FD  FACTOR-MASTER
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 76 CHARACTERS
               DATA RECORD IS FCM-RECORD.
       01  FCM-RECORD.
           05  FCM-KEY                 PIC X(14).
           05  FCM-DATA                PIC X(62).

       FD  CARRIER-MASTER
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 120 CHARACTERS
               DATA RECORD IS CRM-RECORD.
       01  CRM-RECORD.
           05  CRM-KEY                 PIC X(04).
           05  CRM-DATA                PIC X(116).

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

       WORKING-STORAGE SECTION.

      * PROGRAM IDENTIFICATION - MOVED TO THE CONTROL RECORD AND TO
      * EVERY SUSPENSE RECORD RAISED BY THIS MODULE.
       01  WS-PROGRAM-IDENT.
           05  WS-PGM-NAME         PIC X(08)         VALUE 'CABJUR01'.
           05  WS-PGM-VERSION        PIC X(05)           VALUE 'V2.09'.
           05  WS-PGM-APPL           PIC X(04)           VALUE 'CABS'.
           05  WS-PGM-COMPILE      PIC X(08)         VALUE '20190311'.
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

       COPY CABSFCTR.

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
      * CARD LAYOUT FROZEN UNDER CABS-STD-014 SINCE THE 1994 REWRITE.
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
           05  WS-PE-FACTOR-EFF        PIC 9(05).
           05  WS-PE-LOAD-MODE         PIC X(01).
           05  WS-PE-FILLER            PIC X(29).
       01  WS-PARM-OLD REDEFINES WS-PARM-CARD.
      * THE 1989 CARD.  TWO REGIONS NEVER CONVERTED THEIR JCL AND
      * THE EDIT STILL ACCEPTS IT.  THE FIELDS ARE IN A DIFFERENT
      * ORDER AND THE CYCLE DATE IS SIX DIGITS YYMMDD, NOT YYDDD.
           05  FILLER                  PIC X(02).
           05  WS-PO-CYCLE             PIC 9(05).
           05  WS-PO-RUN-ID            PIC X(12).
           05  WS-PO-YYMMDD            PIC 9(06).
           05  WS-PO-MODE              PIC X(01).
           05  FILLER                  PIC X(20).

      * LOCAL SWITCHES.  THE STANDARD ONES ARRIVE VIA CABSWRK.
       01  WS-LOCAL-SWITCHES.
           05  WS-PARM-EOF-SW          PIC X(01)             VALUE 'N'.
                   88  WS-PARM-EOF             VALUE 'Y'.
           05  WS-DUP-KEY-SW           PIC X(01)             VALUE 'N'.
                   88  WS-DUP-KEY              VALUE 'Y'.
           05  WS-CARR-FOUND-SW        PIC X(01)             VALUE 'N'.
                   88  WS-CARR-FOUND           VALUE 'Y'.
           05  WS-FACTOR-OK-SW         PIC X(01)             VALUE 'Y'.
                   88  WS-FACTOR-OK            VALUE 'Y'.
                   88  WS-FACTOR-BAD           VALUE 'N'.
           05  WS-FUTURE-SW            PIC X(01)             VALUE 'N'.
                   88  WS-FUTURE-DATED         VALUE 'Y'.

      * SUBSCRIPTS AND INDEX WORK FIELDS.
       01  WS-SUBSCRIPTS.
           05  WS-SUB1                 PIC S9(05) COMP-3     VALUE 0.
           05  WS-SUB2                 PIC S9(05) COMP-3     VALUE 0.
           05  WS-SUB3                 PIC S9(05) COMP-3     VALUE 0.
           05  WS-IDX                  PIC S9(05) COMP-3     VALUE 0.

      * LOAD STATISTICS.  THESE DO NOT PARTICIPATE IN THE BALANCE
      * EQUATION - THEY ARE FOR THE OPERATIONS SHEET ONLY.
       01  WS-LOAD-COUNTERS.
           05  WS-ADD-CNT              PIC S9(09) COMP-3     VALUE 0.
           05  WS-REP-CNT              PIC S9(09) COMP-3     VALUE 0.
           05  WS-DEF-CNT              PIC S9(09) COMP-3     VALUE 0.
           05  WS-DISP-CNT             PIC S9(09) COMP-3     VALUE 0.
           05  WS-FUT-CNT              PIC S9(09) COMP-3     VALUE 0.
           05  WS-CARR-MISS-CNT        PIC S9(09) COMP-3     VALUE 0.

      * FACTOR WORK AREA.  ALL FACTORS ARE HELD AS A PERCENTAGE
      * IN THE RANGE 0.00000 THROUGH 100.00000.  THE FIVE DECIMAL
      * PLACES MATTER - CARRIERS FILE FACTORS TO FIVE PLACES AND
      * THE FCC ORDER REQUIRES THEM TO BE APPLIED AS FILED.
       01  WS-FACTOR-WORK.
           05  WS-FW-PIU               PIC S9(03)V9(05) COMP-3 VALUE 0.
           05  WS-FW-PLU               PIC S9(03)V9(05) COMP-3 VALUE 0.
           05  WS-FW-PSU               PIC S9(03)V9(05) COMP-3 VALUE 0.
           05  WS-FW-SUM               PIC S9(05)V9(05) COMP-3 VALUE 0.
           05  WS-FW-PRIOR-PIU         PIC S9(03)V9(05) COMP-3 VALUE 0.
           05  WS-FW-PRIOR-PLU         PIC S9(03)V9(05) COMP-3 VALUE 0.

      * JULIAN DATE WORK AREA - LOCAL TO CABJUR01.  THE SHARED AREA IN
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

      * PARAMETER AREA FOR CABDATCV - THE SHARED DATE CONVERSION
      * SUBROUTINE.  CABDATCV IS 1988 VINTAGE AND STILL PIVOTS ON
      * 70 INTERNALLY.
       01  WS-DATE-PARM.
           05  WS-DP-FUNCTION        PIC X(02)           VALUE SPACES.
           05  WS-DP-YYDDD             PIC 9(05)             VALUE 0.
           05  WS-DP-CCYYMMDD          PIC 9(08)             VALUE 0.
           05  WS-DP-DAYS              PIC S9(07) COMP-3     VALUE 0.
           05  WS-DP-RC                PIC 9(02)             VALUE 0.

      * ABEND COMMUNICATION AREA.  PASSED TO CABABEND WHICH ISSUES
      * A USER ABEND WITH THE CODE IN WS-AB-CODE.
       01  WS-ABEND-AREA.
           05  WS-AB-CODE              PIC 9(04) COMP        VALUE 0.
           05  WS-AB-PGM             PIC X(08)           VALUE SPACES.
           05  WS-AB-PARA            PIC X(30)           VALUE SPACES.
           05  WS-AB-TEXT            PIC X(60)           VALUE SPACES.
           05  WS-AB-KEY             PIC X(26)           VALUE SPACES.

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

      * MESSAGE TEXT TABLE.  ADDRESSED BY ERROR CODE SEQUENCE, NOT
      * BY THE CODE ITSELF - INSERTING A CODE IN THE MIDDLE SHIFTS
      * EVERY MESSAGE.  THIS HAS HAPPENED TWICE (1994 AND 2007).
       01  WS-MSG-CONST.
           05  FILLER              PIC X(44)
                   VALUE 'FACTOR LOADED FROM CARRIER FILING           '.
           05  FILLER              PIC X(44)
                   VALUE 'FACTOR DEFAULTED FROM CARRIER MASTER        '.
           05  FILLER              PIC X(44)
                   VALUE 'FACTOR DEFAULTED FROM TARIFF TABLE          '.
           05  FILLER              PIC X(44)
                   VALUE 'FACTOR DISPUTED - HELD FOR REVIEW           '.
           05  FILLER              PIC X(44)
                   VALUE 'FACTOR EFFECTIVE DATE IN THE FUTURE         '.
           05  FILLER              PIC X(44)
                   VALUE 'CARRIER NOT ON MASTER - RECORD REJECTED     '.
           05  FILLER              PIC X(44)
                   VALUE 'PIU OUT OF RANGE 0 THRU 100                 '.
           05  FILLER              PIC X(44)
                   VALUE 'PLU OUT OF RANGE 0 THRU 100                 '.
           05  FILLER              PIC X(44)
                   VALUE 'DUPLICATE KEY - EXISTING RECORD REPLACED    '.
           05  FILLER              PIC X(44)
                   VALUE 'EFFECTIVE DATE NOT A VALID JULIAN DATE      '.
       01  WS-MSG-TABLE REDEFINES WS-MSG-CONST.
           05  WS-MSG-ENTRY OCCURS 10 TIMES
                   INDEXED BY WS-MSG-IX.
               10  WS-MSG-TEXT             PIC X(44).

      * SAVE AREA FOR THE FACTOR RECORD BEING REPLACED.  ONLY THE
      * TWO FACTOR FIELDS ARE TAKEN FORWARD.
       01  WS-OLD-FACTOR.
           05  WS-OF-KEY               PIC X(14).
           05  WS-OF-PIU               PIC S9(03)V9(05) COMP-3.
           05  WS-OF-PLU               PIC S9(03)V9(05) COMP-3.
           05  WS-OF-PSU               PIC S9(03)V9(05) COMP-3.
           05  WS-OF-REST              PIC X(47).

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

      * LAYOUTS COPIED FOR THE CROSS REFERENCE AND VALIDATION
      * ROUTINES.  NOT EVERY FIELD IN EVERY LAYOUT IS USED BY
      * THIS MODULE - THE COPY IS HERE BECAUSE THE LAYOUT WAS
      * NEEDED AT SOME POINT AND REMOVING A COPY MEMBER FORCES
      * A FULL REGRESSION UNDER CABS-STD-009.
       COPY CABSRATE.
       COPY CABSCIRC.

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
      * OPEN, READ THE CONTROL CARD, PRIME THE DATE WORK AREA.        *
      *****************************************************************
       S100-INITIALISATION SECTION.

       P1000-INIT.
      * NOTHING IS DEFAULTED HERE.  IF THE SCHEDULER DID NOT SUPPLY
      * A CYCLE DATE THE STEP ABENDS - IT DOES NOT ASSUME TODAY.
           MOVE 'P1000-INIT' TO WS-PARA-NAME.
           ACCEPT WS-ACCEPT-DATE FROM DATE.
           ACCEPT WS-ACCEPT-TIME FROM TIME.
           OPEN INPUT  FACTOR-IN-FILE
                       CARRIER-MASTER
                       PARM-FILE
           OPEN OUTPUT CONTROL-FILE
                       SUSPENSE-FILE
           OPEN I-O    FACTOR-MASTER
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 4101 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-FCTRIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 4102 TO WS-AB-CODE
               MOVE 'OPEN FAILED DA-I-FCTRMAST' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 4103 TO WS-AB-CODE
               MOVE 'OPEN FAILED DA-I-CARRMAST' TO WS-AB-TEXT
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
           MOVE WS-ACCEPT-DATE         TO WS-AD-WORK.
           MOVE WS-AD-YY               TO DW-CUR-YY.
           PERFORM P1100-READ-PARM THRU P1100-EXIT.
           PERFORM P1200-EDIT-PARM THRU P1200-EXIT.
           MOVE ZERO TO WS-ADD-CNT WS-REP-CNT WS-DEF-CNT
                        WS-DISP-CNT WS-FUT-CNT WS-CARR-MISS-CNT.
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
           IF WS-PE-FACTOR-EFF NOT NUMERIC
               MOVE ZERO TO WS-PE-FACTOR-EFF.
           IF WS-PE-LOAD-MODE NOT = 'F' AND WS-PE-LOAD-MODE NOT = 'I'
               MOVE 'I' TO WS-PE-LOAD-MODE.

       P1200-EXIT.
           EXIT.


      *****************************************************************
      * S200-FACTOR-LOAD                                              *
      * READ THE FILING, VALIDATE IT, PUT IT ON THE MASTER.           *
      *****************************************************************
       S200-FACTOR-LOAD SECTION.

       P2000-PROCESS.
      * ONE FACTOR FILING RECORD PER PASS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ THRU P2100-EXIT.
           IF WS-EOF
               GO TO P2000-EXIT.
           MOVE FCI-RECORD TO CABS-FACTOR-RECORD.
           MOVE FC-KEY TO WS-RESTART-KEY.
           MOVE 'Y' TO WS-FACTOR-OK-SW.
           MOVE 'N' TO WS-FUTURE-SW.
           PERFORM P2200-EDIT-FACTOR THRU P2200-EXIT.
           IF WS-FACTOR-BAD
               GO TO P2000-EXIT.
           PERFORM P2300-CHECK-EFF-DATE THRU P2300-EXIT.
           IF WS-FUTURE-DATED
               ADD 1 TO WS-CFWD-CNT
               ADD 1 TO WS-FUT-CNT
               GO TO P2000-EXIT.
           PERFORM P2400-RESOLVE-DEFAULT THRU P2400-EXIT.
           PERFORM P2500-CARRIER-LOOKUP THRU P2500-EXIT.
           IF NOT WS-CARR-FOUND
               MOVE EC-OCN-UNKNOWN TO WS-ERR-CODE
               MOVE 'E' TO WS-ERR-SEVERITY
               ADD 1 TO WS-CARR-MISS-CNT
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               GO TO P2000-EXIT.
           PERFORM P3000-WRITE-MASTER THRU P3000-EXIT.
           ADD 1 TO WS-ACC-OCN-HASH.
           ADD FC-PIU TO WS-ACC-AMOUNT.

       P2000-EXIT.
           EXIT.

       P2100-READ.
      * SEQUENTIAL READ OF THE QUARTERLY FILING.
           READ FACTOR-IN-FILE
               AT END
                   MOVE 'Y' TO WS-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00'
               MOVE 3410 TO WS-AB-CODE
               MOVE 'READ ERROR UT-S-FCTRIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-READ-CNT.

       P2100-EXIT.
           EXIT.

       P2200-EDIT-FACTOR.
      * RANGE EDIT.  A FACTOR OUTSIDE 0 THRU 100 IS NOT A FACTOR.
      * THE FCC FILING IS A PERCENTAGE; ANYTHING ELSE IS A KEYING
      * ERROR AT THE CARRIER AND MUST NOT BE APPLIED TO REVENUE.
           MOVE 'P2200-EDIT-FACTOR' TO WS-PARA-NAME.
           IF FC-OCN = SPACES OR FC-OCN = LOW-VALUES
               MOVE EC-OCN-UNKNOWN TO WS-ERR-CODE
               MOVE 'N' TO WS-FACTOR-OK-SW
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               GO TO P2200-EXIT.
           IF FC-PIU < 0 OR FC-PIU > 100
               MOVE EC-PIU-OUT-OF-RANGE TO WS-ERR-CODE
               MOVE 'E' TO WS-ERR-SEVERITY
               MOVE 'N' TO WS-FACTOR-OK-SW
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               GO TO P2200-EXIT.
           IF FC-PLU < 0 OR FC-PLU > 100
               MOVE EC-PIU-OUT-OF-RANGE TO WS-ERR-CODE
               MOVE 'E' TO WS-ERR-SEVERITY
               MOVE 'N' TO WS-FACTOR-OK-SW
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               GO TO P2200-EXIT.
           COMPUTE WS-FW-SUM = FC-PIU + FC-PLU.
           IF WS-FW-SUM > 100.00000
               MOVE EC-FACTOR-MISSING TO WS-ERR-CODE
               MOVE 'W' TO WS-ERR-SEVERITY
               PERFORM P7000-SUSPEND THRU P7000-EXIT.
           IF FC-DISPUTED
               ADD 1 TO WS-DISP-CNT
               MOVE 'X' TO FC-SOURCE.
           IF FC-LATA NOT NUMERIC
               MOVE ZERO TO FC-LATA.

       P2200-EXIT.
           EXIT.

       P2300-CHECK-EFF-DATE.
      * THE EFFECTIVE DATE IS A JULIAN YYDDD.  A FILING DATED AFTER
      * THE CYCLE DATE IS CARRIED FORWARD, NOT LOADED - IT WOULD
      * REPRICE USAGE THAT HAS NOT HAPPENED YET.
      * THE PIVOT OF 70 IS THE ESTATE STANDARD AND IS CODED HERE AND
      * IN SIX OTHER PLACES.  SEE CABS-STD-058.
           MOVE 'P2300-CHECK-EFF-DATE' TO WS-PARA-NAME.
           MOVE FC-EFF-YYDDD TO WS-JW-TEST.
           IF WS-JW-TEST-DDD < 001 OR WS-JW-TEST-DDD > 366
               MOVE EC-DATE-INVALID TO WS-ERR-CODE
               MOVE 'N' TO WS-FACTOR-OK-SW
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               GO TO P2300-EXIT.
           PERFORM P4100-LEAP-YEAR THRU P4100-EXIT.
           IF WS-JW-TEST-DDD > WS-JW-DAYS-IN-YR
               MOVE EC-DATE-INVALID TO WS-ERR-CODE
               MOVE 'N' TO WS-FACTOR-OK-SW
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               GO TO P2300-EXIT.
           PERFORM P4000-JULIAN-TO-ABS THRU P4000-EXIT.
           MOVE WS-JW-ABS-TEST TO WS-JW-ABS-FROM.
           MOVE WS-CYCLE-YYDDD TO WS-JW-TEST.
           PERFORM P4000-JULIAN-TO-ABS THRU P4000-EXIT.
           IF WS-JW-ABS-FROM > WS-JW-ABS-TEST
               MOVE 'Y' TO WS-FUTURE-SW.

       P2300-EXIT.
           EXIT.

       P2400-RESOLVE-DEFAULT.
      * A CARRIER THAT FILES NOTHING GETS THE TARIFF DEFAULT.  ZERO
      * IS NOT A FILED FACTOR - IT IS THE ABSENCE OF ONE.  THE ONLY
      * WAY TO FILE A GENUINE ZERO IS SOURCE C WITH FC-PIU ZERO,
      * WHICH THIS PARAGRAPH CANNOT DISTINGUISH.  KNOWN SINCE 1993.
           MOVE 'P2400-RESOLVE-DEFAULT' TO WS-PARA-NAME.
           MOVE FC-PIU TO WS-FW-PIU.
           MOVE FC-PLU TO WS-FW-PLU.
           IF FC-PIU = ZERO AND FC-FROM-CARRIER
               MOVE 'D' TO FC-SOURCE
               ADD 1 TO WS-DEF-CNT
               PERFORM P2450-TARIFF-DEFAULT THRU P2450-EXIT.
           IF FC-PLU = ZERO AND NOT FC-DISPUTED
               MOVE ZERO TO FC-PLU.

       P2400-EXIT.
           EXIT.

       P2450-TARIFF-DEFAULT.
      * SEARCH THE 1998 TARIFF DEFAULT TABLE.  IF THE OCN IS NOT
      * THERE THE INDUSTRY FALLBACK OF 50 PERCENT IS USED.
           SET WS-OD-IX TO 1.
           SEARCH WS-OD-ENTRY
               AT END
                   MOVE 050.00000 TO FC-PIU
                   MOVE 000.00000 TO FC-PLU
                   GO TO P2450-EXIT
               WHEN WS-OD-OCN (WS-OD-IX) = FC-OCN
                   MOVE WS-OD-PIU (WS-OD-IX) TO FC-PIU
                   MOVE WS-OD-PLU (WS-OD-IX) TO FC-PLU.

       P2450-EXIT.
           EXIT.

       P2500-CARRIER-LOOKUP.
      * RANDOM READ OF THE CARRIER MASTER.  THE 2007 CHANGE NOTE IN
      * THE REVISION HISTORY SAYS THIS EDIT WAS REMOVED.  IT WAS NOT.
           MOVE 'P2500-CARRIER-LOOKUP' TO WS-PARA-NAME.
           MOVE 'N' TO WS-CARR-FOUND-SW.
           MOVE FC-OCN TO CRM-KEY.
           READ CARRIER-MASTER
               INVALID KEY
                   GO TO P2500-EXIT.
           MOVE CRM-RECORD TO CABS-CARRIER-RECORD.
           MOVE 'Y' TO WS-CARR-FOUND-SW.
           IF CR-DEFAULT-PIU NOT = ZERO AND FC-FROM-DEFAULT
               MOVE CR-DEFAULT-PIU TO FC-PIU
               MOVE CR-DEFAULT-PLU TO FC-PLU.

       P2500-EXIT.
           EXIT.


      *****************************************************************
      * S300-MASTER-UPDATE                                            *
      * ADD OR REPLACE THE FACTOR ON THE KSDS.                        *
      *****************************************************************
       S300-MASTER-UPDATE SECTION.

       P3000-WRITE-MASTER.
      * THE 2003 CHANGE MADE A DUPLICATE KEY A REWRITE RATHER THAN A
      * REJECT.  THAT IS WHY A RERUN OF THIS STEP IS SAFE.
           MOVE 'P3000-WRITE-MASTER' TO WS-PARA-NAME.
           MOVE CABS-FACTOR-RECORD TO FCM-RECORD.
           MOVE FC-KEY TO FCM-KEY.
           MOVE 'N' TO WS-DUP-KEY-SW.
           WRITE FCM-RECORD
               INVALID KEY
                   MOVE 'Y' TO WS-DUP-KEY-SW.
           IF WS-DUP-KEY
               PERFORM P3100-REWRITE-MASTER THRU P3100-EXIT
           ELSE
               ADD 1 TO WS-ADD-CNT
               ADD 1 TO WS-WRITE-CNT.

       P3000-EXIT.
           EXIT.

       P3100-REWRITE-MASTER.
      * REPLACE THE EXISTING FACTOR.  THE PRIOR VALUES ARE CARRIED
      * INTO FC-PRIOR-PIU AND FC-PRIOR-PLU SO THAT CABJUR07 CAN
      * RESTATE AGAINST THEM LATER.  IF THIS PARAGRAPH DOES NOT RUN
      * THE PRIOR FACTOR IS ZERO AND THE RESTATEMENT HAS NO BASIS.
           MOVE FC-KEY TO FCM-KEY.
           READ FACTOR-MASTER
               INVALID KEY
                   MOVE 4104 TO WS-AB-CODE
                   MOVE 'DUP KEY THEN NOT FOUND' TO WS-AB-TEXT
                   PERFORM P9500-ABEND THRU P9500-EXIT.
           MOVE FCM-RECORD TO WS-OLD-FACTOR.
           MOVE WS-OF-PIU TO FC-PRIOR-PIU.
           MOVE WS-OF-PLU TO FC-PRIOR-PLU.
           MOVE CABS-FACTOR-RECORD TO FCM-RECORD.
           MOVE FC-KEY TO FCM-KEY.
           REWRITE FCM-RECORD
               INVALID KEY
                   MOVE 4105 TO WS-AB-CODE
                   MOVE 'REWRITE FAILED ON FACTOR MASTER' TO WS-AB-TEXT
                   PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-REP-CNT.
           ADD 1 TO WS-WRITE-CNT.

       P3100-EXIT.
           EXIT.


      *****************************************************************
      * S400-DATE-ROUTINES                                            *
      * JULIAN DATE SUPPORT.                                          *
      *****************************************************************
       S400-DATE-ROUTINES SECTION.

       P4000-JULIAN-TO-ABS.
      * CONVERT A YYDDD INTO AN ABSOLUTE DAY NUMBER SO THAT DATES
      * EITHER SIDE OF A YEAR BOUNDARY CAN BE COMPARED.  A PLAIN
      * NUMERIC COMPARE OF TWO YYDDD VALUES IS ONLY SAFE INSIDE ONE
      * YEAR - SEE THE NOTE IN CABJUR07 WHERE IT IS NOT.
           MOVE WS-JW-TEST TO WS-JW-TEST.
           IF WS-JW-TEST-YY < 70
               COMPUTE WS-JW-CCYY = 2000 + WS-JW-TEST-YY
           ELSE
               COMPUTE WS-JW-CCYY = 1900 + WS-JW-TEST-YY.
           COMPUTE WS-JW-ABS-TEST = ((WS-JW-CCYY - 1900) * 365)
                 + ((WS-JW-CCYY - 1901) / 4)
                 + WS-JW-TEST-DDD.

       P4000-EXIT.
           EXIT.

       P4100-LEAP-YEAR.
      * GREGORIAN LEAP RULE.  DIVISIBLE BY 4, NOT BY 100, UNLESS BY
      * 400.  THE 1987 VERSION TESTED ONLY THE DIVISION BY 4 AND WAS
      * CORRECTED IN 1996 - 2000 WAS A LEAP YEAR EITHER WAY SO THE
      * ORIGINAL ERROR NEVER SURFACED.
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

       P4100-EXIT.
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
           MOVE CABS-FACTOR-RECORD TO SU-ORIG-RECORD.
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
           MOVE 010                    TO CT-STEP-SEQ.
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
           DISPLAY 'FACTORS ADDED    ' WS-ADD-CNT.
           DISPLAY 'FACTORS REPLACED ' WS-REP-CNT.
           DISPLAY 'DEFAULTED        ' WS-DEF-CNT.
           DISPLAY 'DISPUTED HELD    ' WS-DISP-CNT.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE FACTOR-IN-FILE
                 FACTOR-MASTER
                 CARRIER-MASTER
                 PARM-FILE
                 CONTROL-FILE
                 SUSPENSE-FILE
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

