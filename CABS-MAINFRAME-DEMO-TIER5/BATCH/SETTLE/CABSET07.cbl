      *****************************************************************
      * CABSET07 - CMDS RAO OUTBOUND EXCHANGE PRODUCTION              *
      * APPLICATION : SETL                                            *
      * INPUTS      : SETLIN   TELCABS.SETL.SETTLE.DAILY(0)   CABSSETL*
      * INPUTS      : CARRMAST TELCABS.SETL.CARRIER           CABSCARR*
      * INPUTS      : SYSIN    INSTREAM                       NONE    *
      * OUTPUTS     : CMDSOUT  TELCABS.SETL.CMDS.OUT(+1)      CABSSETL*
      * OUTPUTS     : REPORT   SYSOUT                         CABSPRNT*
      * CONTROL     : CTLOUT                         CABSCTL          *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED + CT-CARRIED-*
      *               CARRIED FWD = RECORDS HELD FOR THE NEXT EXCHANGE*
      * RESTART     : FULL RERUN - THE EXCHANGE FILE IS REBUILT WHOLE *
      * STANDARDS   : CODED TO CABS-STD-041 (MONEY FIELDS) AND        *
      *               CABS-STD-063 (PRINT CONTROL).                   *
      * REVISION HISTORY                                              *
      *   V1.00  1987-11-30  R.T.WHEELER   INITIAL - CMDS DAILY       *
      *   V1.03  1990-06-05  D.OKONKWO     RAO FORMATTER SPLIT OUT    *
      *   V1.07  1994-02-17  D.OKONKWO     REGIONAL FORMATS ADDED     *
      *   V2.00  1997-01-28  J.M.CASTILLO  Y2K - EXCH DATE YYDDD      *
      *   V2.02  2002-11-12  P.NAIR        CUT OFF DAY FROM CARD      *
      *   V2.05  2010-08-24  A.BUKOWSKI    FTP HANDOFF REPLACED NDM   *
      *   V2.07  2018-01-09  M.OYELARAN    RECOMPILE ONLY             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CABSET07.
       AUTHOR.        R.T.WHEELER.
       DATE-WRITTEN.  1987-11-30.
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
      * TODAYS SETTLEMENT RECORDS AWAITING EXCHANGE
           SELECT SETTLE-IN-FILE
               ASSIGN TO UT-S-SETLIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
      * CARRIER MASTER - SUPPLIES THE RAO CODE
           SELECT CARRIER-MASTER
               ASSIGN TO DA-I-CARRMAST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CRM-KEY
               FILE STATUS IS WS-FS-TABLE.
      * OUTBOUND CMDS EXCHANGE FILE - SENT TO THE OTHER RBOC
           SELECT CMDS-OUT-FILE
               ASSIGN TO UT-S-CMDSOUT
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

       FD  CMDS-OUT-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 180 CHARACTERS
               DATA RECORD IS CMO-RECORD.
       01  CMO-RECORD              PIC X(180).

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
           05  WS-PGM-NAME         PIC X(08)         VALUE 'CABSET07'.
           05  WS-PGM-VERSION        PIC X(05)           VALUE 'V2.07'.
           05  WS-PGM-APPL           PIC X(04)           VALUE 'SETL'.
           05  WS-PGM-COMPILE      PIC X(08)         VALUE '20180109'.
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
           05  WS-PE-EXCH-YYDDD        PIC 9(05).
           05  WS-PE-CUTOFF-DAY        PIC 9(03).
           05  WS-PE-REGION            PIC X(01).
           05  WS-PE-FILLER            PIC X(26).
       01  WS-PARM-OLD REDEFINES WS-PARM-CARD.
      * THE 1989 CARD.  TWO REGIONS NEVER CONVERTED THEIR JCL AND
      * THE EDIT STILL ACCEPTS IT.  THE FIELDS ARE IN A DIFFERENT
      * ORDER AND THE CYCLE DATE IS SIX DIGITS YYMMDD, NOT YYDDD.
           05  FILLER                  PIC X(02).
           05  WS-PO-CYCLE             PIC 9(05).
           05  WS-PO-RUN-ID            PIC X(12).
           05  WS-PO-YYMMDD            PIC 9(06).
           05  WS-PO-EXCH              PIC 9(05).
           05  FILLER                  PIC X(20).

      * LOCAL SWITCHES.  THE STANDARD ONES ARRIVE VIA CABSWRK.
       01  WS-LOCAL-SWITCHES.
           05  WS-PARM-EOF-SW          PIC X(01)             VALUE 'N'.
                   88  WS-PARM-EOF             VALUE 'Y'.
           05  WS-RAO-FOUND-SW         PIC X(01)             VALUE 'N'.
                   88  WS-RAO-FOUND            VALUE 'Y'.
           05  WS-HOLD-SW              PIC X(01)             VALUE 'N'.
                   88  WS-HELD                 VALUE 'Y'.

      * SUBSCRIPTS AND INDEX WORK FIELDS.
       01  WS-SUBSCRIPTS.
           05  WS-SUB1                 PIC S9(05) COMP-3     VALUE 0.
           05  WS-SUB2                 PIC S9(05) COMP-3     VALUE 0.

      * THE RAO FORMATTER IS REGION SPECIFIC.  EACH RBOC REGION
      * AGREED ITS OWN RECORD FORMAT IN 1987 AND THE FIVE HAVE
      * NEVER BEEN HARMONISED.  THE MODULE NAME IS ASSEMBLED
      * FROM THE REGION CODE HELD ON THE RAO CROSS REFERENCE.
      * MODULE NAMING FOLLOWS CABS-STD-031 (TARIFF SUFFIXES).
       01  WS-CALL-AREA.
           05  WS-CALL-PGM.
               10  WS-CP-STEM          PIC X(05)         VALUE 'CABRA'.
               10  WS-CP-SUFFIX        PIC X(03)         VALUE SPACES.
           05  WS-CALL-RC              PIC S9(04) COMP       VALUE 0.
           05  WS-CALL-COUNT           PIC S9(07) COMP-3     VALUE 0.
           05  WS-CALL-SUFFIX-TAB.
               10  FILLER              PIC X(05)         VALUE 'AOAM'.
               10  FILLER              PIC X(05)         VALUE 'BOBM'.
               10  FILLER              PIC X(05)         VALUE 'COCM'.
               10  FILLER              PIC X(05)         VALUE 'DODM'.
               10  FILLER              PIC X(05)         VALUE 'EOEM'.
           05  WS-CALL-SUFFIX-R REDEFINES WS-CALL-SUFFIX-TAB.
               10  WS-CS-ENT OCCURS 5 TIMES
                   INDEXED BY WS-CS-IX.
                   15  WS-CS-KEY               PIC X(02).
                   15  WS-CS-SUF               PIC X(03).

      * EXCHANGE WORK AREA.
       01  WS-EXCHANGE-WORK.
           05  WS-EW-AMOUNT            PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-EW-MOU               PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-EW-AGE-DAYS          PIC S9(05) COMP-3     VALUE 0.
           05  WS-EW-NET               PIC S9(13)V9(02) COMP-3 VALUE 0.

      * EXCHANGE TOTALS.
       01  WS-EXCHANGE-TOTALS.
           05  WS-EXCH-CNT             PIC S9(09) COMP-3     VALUE 0.
           05  WS-HELD-CNT             PIC S9(09) COMP-3     VALUE 0.
           05  WS-NORAO-CNT            PIC S9(09) COMP-3     VALUE 0.
           05  WS-TOT-EXCH-AMT         PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-TOT-EXCH-MOU         PIC S9(15)V9(02) COMP-3 VALUE 0.

      * THE CMDS EXCHANGE RECORD.  THE INDUSTRY FORMAT IS FIXED
      * AT 180 BYTES AND IS REDEFINED THREE WAYS - ONE PER RECORD
      * KIND.
       01  WS-CMDS-RECORD.
           05  WS-CM-REC-TYPE        PIC X(02)           VALUE SPACES.
           05  WS-CM-BODY            PIC X(178)          VALUE SPACES.
       01  WS-CMDS-DETAIL REDEFINES WS-CMDS-RECORD.
           05  WS-CD-TYPE              PIC X(02).
           05  WS-CD-FROM-RAO          PIC X(03).
           05  WS-CD-TO-RAO            PIC X(03).
           05  WS-CD-EXCH-YYDDD        PIC 9(05).
           05  WS-CD-OCN               PIC X(04).
           05  WS-CD-PERIOD            PIC 9(06).
           05  WS-CD-MOU               PIC S9(15)V9(02).
           05  WS-CD-AMOUNT            PIC S9(13)V9(02).
           05  WS-CD-DIRECTION         PIC X(01).
           05  WS-CD-FILLER            PIC X(122).
       01  WS-CMDS-HEADER REDEFINES WS-CMDS-RECORD.
           05  WS-CH-TYPE              PIC X(02).
           05  WS-CH-FROM-RAO          PIC X(03).
           05  WS-CH-CREATE-YYDDD      PIC 9(05).
           05  WS-CH-SEQ               PIC 9(06).
           05  WS-CH-FILLER            PIC X(164).
       01  WS-CMDS-TRAILER REDEFINES WS-CMDS-RECORD.
           05  WS-CT-TYPE              PIC X(02).
           05  WS-CT-COUNT             PIC 9(09).
           05  WS-CT-HASH-AMT          PIC S9(15)V9(02).
           05  WS-CT-HASH-MOU          PIC S9(15)V9(02).
           05  WS-CT-FILLER            PIC X(135).

      * JULIAN DATE WORK AREA - LOCAL TO CABSET07.  THE SHARED AREA IN
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

      * CMDS RAO TO OCN CROSS REFERENCE.  THE REVENUE ACCOUNTING
      * OFFICE CODE IS THE ONLY KEY THE INTER-RBOC EXCHANGE CARRIES,
      * SO EVERY INBOUND MESSAGE HAS TO BE TRANSLATED HERE BEFORE IT
      * CAN BE MATCHED TO A CARRIER.  UNMATCHED RAOS GO TO SUSPENSE.
       01  WS-RAOXRF-CONST.
           05  FILLER              PIC X(08)         VALUE '1621902B'.
           05  FILLER              PIC X(08)         VALUE '1987280E'.
           05  FILLER              PIC X(08)         VALUE '2274560A'.
           05  FILLER              PIC X(08)         VALUE '2851096A'.
           05  FILLER              PIC X(08)         VALUE '3207666A'.
           05  FILLER              PIC X(08)         VALUE '3655973C'.
           05  FILLER              PIC X(08)         VALUE '3751698E'.
           05  FILLER              PIC X(08)         VALUE '3888936A'.
           05  FILLER              PIC X(08)         VALUE '4178577C'.
           05  FILLER              PIC X(08)         VALUE '5114414E'.
           05  FILLER              PIC X(08)         VALUE '5269394C'.
           05  FILLER              PIC X(08)         VALUE '5482914A'.
           05  FILLER              PIC X(08)         VALUE '5504159D'.
           05  FILLER              PIC X(08)         VALUE '5861471E'.
           05  FILLER              PIC X(08)         VALUE '5926220E'.
           05  FILLER              PIC X(08)         VALUE '5958449A'.
           05  FILLER              PIC X(08)         VALUE '5961282B'.
           05  FILLER              PIC X(08)         VALUE '6377051B'.
           05  FILLER              PIC X(08)         VALUE '6715105D'.
           05  FILLER              PIC X(08)         VALUE '6883345B'.
           05  FILLER              PIC X(08)         VALUE '7478695C'.
           05  FILLER              PIC X(08)         VALUE '7523403D'.
           05  FILLER              PIC X(08)         VALUE '7532663D'.
           05  FILLER              PIC X(08)         VALUE '7879964E'.
           05  FILLER              PIC X(08)         VALUE '8582252D'.
           05  FILLER              PIC X(08)         VALUE '8845571C'.
           05  FILLER              PIC X(08)         VALUE '9037628C'.
           05  FILLER              PIC X(08)         VALUE '9152038B'.
           05  FILLER              PIC X(08)         VALUE '9171759B'.
           05  FILLER              PIC X(08)         VALUE '9235544C'.
           05  FILLER              PIC X(08)         VALUE '9283426D'.
           05  FILLER              PIC X(08)         VALUE '9373690A'.
           05  FILLER              PIC X(08)         VALUE '9623029E'.
           05  FILLER              PIC X(08)         VALUE '9687104A'.
           05  FILLER              PIC X(08)         VALUE '9746331B'.
           05  FILLER              PIC X(08)         VALUE '9899972D'.
       01  WS-RAOXRF-TABLE REDEFINES WS-RAOXRF-CONST.
           05  WS-WS-RA-ENTRY OCCURS 36 TIMES
                   INDEXED BY WS-RA-IX.
               10  WS-RA-RAO               PIC X(03).
               10  WS-RA-OCN               PIC X(04).
               10  WS-RA-REGION            PIC X(01).

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
                   VALUE 'CMDS OUTBOUND EXCHANGE REGISTER'.
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
           05  FILLER              PIC X(47)
                   VALUE 'OCN  RAO REG EXCH-DT  PERIOD  MINUTES'.
           05  FILLER              PIC X(31)
                   VALUE 'AMOUNT           D  DISPOSITION'.
       01  WS-HEAD-4.
           05  FILLER                  PIC X(01)             VALUE ' '.
           05  FILLER                PIC X(131)          VALUE ALL '-'.

      * DETAIL LINE WS-DETAIL-1.
       01  WS-DETAIL-1.
           05  FILLER                  PIC X(01)             VALUE ' '.
           05  WS-D1-OCN               PIC X(04).
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-RAO               PIC X(03).
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-REG               PIC X(01).
           05  FILLER                PIC X(03)           VALUE SPACES.
           05  WS-D1-EXCH              PIC 9(05).
           05  FILLER                PIC X(02)           VALUE SPACES.
           05  WS-D1-PERIOD            PIC 9(06).
           05  FILLER                PIC X(02)           VALUE SPACES.
           05  WS-D1-MOU               PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-AMT               PIC ZZZ,ZZZ,ZZ9.99-.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-DIR               PIC X(01).
           05  FILLER                PIC X(02)           VALUE SPACES.
           05  WS-D1-DISPO             PIC X(24).

      * MESSAGE TEXT TABLE.  ADDRESSED BY ERROR CODE SEQUENCE, NOT
      * BY THE CODE ITSELF - INSERTING A CODE IN THE MIDDLE SHIFTS
      * EVERY MESSAGE.  THIS HAS HAPPENED TWICE (1994 AND 2007).
       01  WS-MSG-CONST.
           05  FILLER              PIC X(44)
                   VALUE 'EXCHANGED TO THE OTHER RBOC                 '.
           05  FILLER              PIC X(44)
                   VALUE 'HELD - BEFORE THE CUT OFF DAY               '.
           05  FILLER              PIC X(44)
                   VALUE 'RAO CODE NOT ON THE CROSS REFERENCE         '.
           05  FILLER              PIC X(44)
                   VALUE 'RAO CODE NOT ON THE CARRIER MASTER          '.
           05  FILLER              PIC X(44)
                   VALUE 'REGIONAL FORMATTER CALLED                   '.
           05  FILLER              PIC X(44)
                   VALUE 'REGIONAL FORMATTER RETURNED NON ZERO        '.
           05  FILLER              PIC X(44)
                   VALUE 'DISPUTED RECORD - NOT EXCHANGED             '.
           05  FILLER              PIC X(44)
                   VALUE 'ZERO AMOUNT - NOT EXCHANGED                 '.
           05  FILLER              PIC X(44)
                   VALUE 'EXCHANGE DATE TAKEN FROM CONTROL CARD       '.
           05  FILLER              PIC X(44)
                   VALUE 'END OF EXCHANGE RUN                         '.
       01  WS-MSG-TABLE REDEFINES WS-MSG-CONST.
           05  WS-MSG-ENTRY OCCURS 10 TIMES
                   INDEXED BY WS-MSG-IX.
               10  WS-MSG-TEXT             PIC X(44).

      * RAO AND REGION HOLD FIELDS.
       01  WS-RAO-WORK.
           05  WS-RAO-HOLD           PIC X(03)           VALUE SPACES.
           05  WS-REGION-HOLD          PIC X(01)             VALUE 'A'.

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
           OPEN OUTPUT CMDS-OUT-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 5701 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-SETLIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 5702 TO WS-AB-CODE
               MOVE 'OPEN FAILED DA-I-CARRMAST' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 5703 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-CMDSOUT' TO WS-AB-TEXT
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
           MOVE ZERO TO WS-EXCH-CNT WS-HELD-CNT
                        WS-NORAO-CNT WS-TOT-EXCH-AMT
                        WS-TOT-EXCH-MOU.
           PERFORM P6000-HEADING THRU P6000-EXIT.
           PERFORM P1400-WRITE-HEADER THRU P1400-EXIT.
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
           IF WS-PE-EXCH-YYDDD NOT NUMERIC
               MOVE WS-PC-CYCLE TO WS-PE-EXCH-YYDDD.
           IF WS-PE-CUTOFF-DAY NOT NUMERIC
               MOVE 005 TO WS-PE-CUTOFF-DAY.
           IF WS-PE-REGION = SPACES
               MOVE 'A' TO WS-PE-REGION.

       P1200-EXIT.
           EXIT.

       P1400-WRITE-HEADER.
      * THE EXCHANGE FILE OPENS WITH A HEADER RECORD CARRYING THE
      * ORIGINATING RAO AND THE CREATION DATE.  THE RECEIVING RBOC
      * REJECTS THE WHOLE FILE IF THE HEADER IS ABSENT OR IF THE
      * CREATION DATE IS NOT TODAY IN THEIR TIME ZONE - WHICH IS
      * WHY THE JOB MUST NOT RUN AFTER 2200 EASTERN.
           MOVE SPACES TO WS-CMDS-RECORD.
           MOVE 'HD' TO WS-CH-TYPE.
           MOVE WS-PE-REGION TO WS-CH-FROM-RAO.
           MOVE WS-PE-EXCH-YYDDD TO WS-CH-CREATE-YYDDD.
           MOVE 000001 TO WS-CH-SEQ.
           MOVE WS-CMDS-RECORD TO CMO-RECORD.
           WRITE CMO-RECORD.

       P1400-EXIT.
           EXIT.


      *****************************************************************
      * S200-EXCHANGE                                                 *
      * SELECT AND FORMAT EXCHANGE RECORDS.                           *
      *****************************************************************
       S200-EXCHANGE SECTION.

       P2000-PROCESS.
      * ONE SETTLEMENT RECORD PER PASS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ THRU P2100-EXIT.
           IF WS-EOF
               GO TO P2000-EXIT.
           MOVE STI-RECORD TO CABS-SETTLEMENT-RECORD.
           MOVE ST-KEY TO WS-RESTART-KEY.
           MOVE SPACES TO WS-D1-DISPO.
           IF ST-DISPUTE-SW = 'Y'
               ADD 1 TO WS-CFWD-CNT
               MOVE WS-MSG-TEXT (7) TO WS-D1-DISPO
               PERFORM P6100-DETAIL THRU P6100-EXIT
               GO TO P2000-EXIT.
           IF ST-NET-DUE = ZERO
               ADD 1 TO WS-SUMM-CNT
               MOVE WS-MSG-TEXT (8) TO WS-D1-DISPO
               GO TO P2000-EXIT.
           PERFORM P2300-AGE-TEST THRU P2300-EXIT.
           IF WS-HELD
               ADD 1 TO WS-CFWD-CNT
               ADD 1 TO WS-HELD-CNT
               PERFORM P6100-DETAIL THRU P6100-EXIT
               GO TO P2000-EXIT.
           PERFORM P2400-RAO-LOOKUP THRU P2400-EXIT.
           IF NOT WS-RAO-FOUND
               ADD 1 TO WS-NORAO-CNT
               MOVE EC-OCN-UNKNOWN TO WS-ERR-CODE
               MOVE 'E' TO WS-ERR-SEVERITY
               MOVE WS-MSG-TEXT (3) TO WS-D1-DISPO
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               PERFORM P6100-DETAIL THRU P6100-EXIT
               GO TO P2000-EXIT.
           PERFORM P3000-BUILD-CMDS THRU P3000-EXIT.
           PERFORM P3200-CALL-FORMATTER THRU P3200-EXIT.
           PERFORM P3400-WRITE-CMDS THRU P3400-EXIT.
           PERFORM P6100-DETAIL THRU P6100-EXIT.
           ADD 1 TO WS-WRITE-CNT.
           ADD 1 TO WS-EXCH-CNT.

       P2000-EXIT.
           EXIT.

       P2100-READ.
      * SEQUENTIAL READ OF THE DAILY SETTLEMENT.
           READ SETTLE-IN-FILE
               AT END
                   MOVE 'Y' TO WS-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00'
               MOVE 3570 TO WS-AB-CODE
               MOVE 'READ ERROR UT-S-SETLIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-READ-CNT.

       P2100-EXIT.
           EXIT.

       P2300-AGE-TEST.
      * A SETTLEMENT RECORD IS NOT EXCHANGED UNTIL IT IS AT LEAST
      * THE CUT OFF NUMBER OF DAYS OLD - THE OTHER RBOC WILL NOT
      * ACCEPT AN EXCHANGE FOR A PERIOD THEY HAVE NOT CLOSED.  THE
      * AGE IS COMPUTED THROUGH ABSOLUTE DAY NUMBERS SO THAT A
      * RECORD DATED IN DECEMBER AND EXCHANGED IN JANUARY IS AGED
      * CORRECTLY.
           MOVE 'P2300-AGE-TEST' TO WS-PARA-NAME.
           MOVE 'N' TO WS-HOLD-SW.
           IF ST-EXCH-YYDDD = ZERO
               GO TO P2300-EXIT.
           MOVE ST-EXCH-YYDDD TO WS-JW-TEST.
           PERFORM P4000-JULIAN-TO-ABS THRU P4000-EXIT.
           MOVE WS-JW-ABS-TEST TO WS-JW-ABS-FROM.
           MOVE WS-PE-EXCH-YYDDD TO WS-JW-TEST.
           PERFORM P4000-JULIAN-TO-ABS THRU P4000-EXIT.
           MOVE WS-JW-ABS-TEST TO WS-JW-ABS-THRU.
           COMPUTE WS-EW-AGE-DAYS =
                   WS-JW-ABS-THRU - WS-JW-ABS-FROM.
           IF WS-EW-AGE-DAYS < WS-PE-CUTOFF-DAY
               MOVE 'Y' TO WS-HOLD-SW
               MOVE WS-MSG-TEXT (2) TO WS-D1-DISPO.

       P2300-EXIT.
           EXIT.

       P2400-RAO-LOOKUP.
      * THE RAO CODE IS THE ONLY KEY THE INTER RBOC EXCHANGE
      * CARRIES.  IT IS TAKEN FROM THE CARRIER MASTER WHEN IT IS
      * THERE AND FROM THE CROSS REFERENCE TABLE WHEN IT IS NOT.
           MOVE 'P2400-RAO-LOOKUP' TO WS-PARA-NAME.
           MOVE 'N' TO WS-RAO-FOUND-SW.
           MOVE SPACES TO WS-RAO-HOLD.
           MOVE ST-COUNTERPARTY-OCN TO CRM-KEY.
           READ CARRIER-MASTER
               INVALID KEY
                   GO TO P2450-TABLE-LOOKUP.
           MOVE CRM-RECORD TO CABS-CARRIER-RECORD.
           IF CR-CMDS-RAO NOT = SPACES
               MOVE CR-CMDS-RAO TO WS-RAO-HOLD
               MOVE 'Y' TO WS-RAO-FOUND-SW
               GO TO P2400-EXIT.

       P2400-EXIT.
           EXIT.

       P2450-TABLE-LOOKUP.
      * FALL BACK TO THE RAO CROSS REFERENCE TABLE.  THE REGION
      * CODE FROM THIS TABLE DRIVES THE DYNAMIC CALL BELOW.
           SET WS-RA-IX TO 1.
           SEARCH WS-RA-ENTRY
               AT END
                   GO TO P2450-EXIT
               WHEN WS-RA-OCN (WS-RA-IX) = ST-COUNTERPARTY-OCN
                   MOVE WS-RA-RAO (WS-RA-IX) TO WS-RAO-HOLD
                   MOVE WS-RA-REGION (WS-RA-IX) TO WS-REGION-HOLD
                   MOVE 'Y' TO WS-RAO-FOUND-SW.

       P2450-EXIT.
           EXIT.


      *****************************************************************
      * S300-FORMAT                                                   *
      * BUILD AND FORMAT THE EXCHANGE RECORD.                         *
      *****************************************************************
       S300-FORMAT SECTION.

       P3000-BUILD-CMDS.
      * BUILD THE INDUSTRY FORMAT DETAIL RECORD.  THE AMOUNT IS
      * CARRIED AT TWO DECIMAL PLACES - THE INDUSTRY FORMAT HAS NO
      * ROOM FOR FIVE, WHICH IS WHERE THE SETTLEMENT PRECISION IS
      * LOST ON THE WAY OUT.
           MOVE SPACES TO WS-CMDS-RECORD.
           MOVE 'DT' TO WS-CD-TYPE.
           MOVE WS-PE-REGION TO WS-CD-FROM-RAO.
           MOVE WS-RAO-HOLD TO WS-CD-TO-RAO.
           MOVE WS-PE-EXCH-YYDDD TO WS-CD-EXCH-YYDDD.
           MOVE ST-COUNTERPARTY-OCN TO WS-CD-OCN.
           MOVE ST-SETTLE-PERIOD TO WS-CD-PERIOD.
           MOVE ST-TOTAL-MOU TO WS-CD-MOU.
           MOVE ST-NET-DUE TO WS-CD-AMOUNT.
           MOVE ST-DIRECTION TO WS-CD-DIRECTION.
           ADD ST-NET-DUE TO WS-TOT-EXCH-AMT.
           ADD ST-TOTAL-MOU TO WS-TOT-EXCH-MOU.
           ADD ST-NET-DUE TO WS-ACC-AMOUNT.
           ADD ST-TOTAL-MOU TO WS-ACC-MINUTES.

       P3000-EXIT.
           EXIT.

       P3200-CALL-FORMATTER.
      * CALL THE REGIONAL FORMATTER.  THE MODULE NAME IS BUILT FROM
      * THE REGION CODE AT RUN TIME AND THE LOAD MODULE THAT ACTUALLY
      * RUNS DEPENDS ON THE STEPLIB CONCATENATION IN THE JOB.
      * MODULE NAMING FOLLOWS CABS-STD-031 (TARIFF SUFFIXES).
           MOVE 'P3200-CALL-FORMATTER' TO WS-PARA-NAME.
           MOVE SPACES TO WS-CP-SUFFIX.
           SET WS-CS-IX TO 1.
           SEARCH WS-CS-ENT
               AT END
                   MOVE 'OAM' TO WS-CP-SUFFIX
               WHEN WS-CS-KEY (WS-CS-IX) = WS-REGION-HOLD
                   MOVE WS-CS-SUF (WS-CS-IX) TO WS-CP-SUFFIX.
           ADD 1 TO WS-CALL-COUNT.
           CALL WS-CALL-PGM USING WS-CMDS-RECORD
                                  WS-CALL-RC.
           IF WS-CALL-RC NOT = ZERO
               MOVE WS-MSG-TEXT (6) TO WS-D1-DISPO
               MOVE EC-OUT-OF-BALANCE TO WS-ERR-CODE
               MOVE 'W' TO WS-ERR-SEVERITY
           ELSE
               MOVE WS-MSG-TEXT (5) TO WS-D1-DISPO.

       P3200-EXIT.
           EXIT.

       P3400-WRITE-CMDS.
      * WRITE THE FORMATTED RECORD TO THE EXCHANGE FILE.
           MOVE WS-CMDS-RECORD TO CMO-RECORD.
           WRITE CMO-RECORD.
           MOVE WS-MSG-TEXT (1) TO WS-D1-DISPO.

       P3400-EXIT.
           EXIT.

       P3600-WRITE-TRAILER.
      * THE TRAILER CARRIES THE COUNT AND THE TWO HASH TOTALS.  THE
      * RECEIVING RBOC BALANCES ON THESE AND REJECTS THE FILE IF
      * THEY DO NOT AGREE WITH WHAT THEY COUNT.
           MOVE SPACES TO WS-CMDS-RECORD.
           MOVE 'TR' TO WS-CT-TYPE.
           MOVE WS-EXCH-CNT TO WS-CT-COUNT.
           MOVE WS-TOT-EXCH-AMT TO WS-CT-HASH-AMT.
           MOVE WS-TOT-EXCH-MOU TO WS-CT-HASH-MOU.
           MOVE WS-CMDS-RECORD TO CMO-RECORD.
           WRITE CMO-RECORD.

       P3600-EXIT.
           EXIT.


      *****************************************************************
      * S400-DATE-ROUTINES                                            *
      * JULIAN SUPPORT.                                               *
      *****************************************************************
       S400-DATE-ROUTINES SECTION.

       P4000-JULIAN-TO-ABS.
      * CONVERT YYDDD TO AN ABSOLUTE DAY NUMBER.
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

       P4100-EXIT.
           EXIT.


      *****************************************************************
      * S600-REPORT                                                   *
      * EXCHANGE REGISTER.                                            *
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
      * ONE LINE PER SETTLEMENT RECORD.
           IF WS-LINE-CNT > WS-MAX-LINES
               PERFORM P6000-HEADING THRU P6000-EXIT.
           MOVE ST-COUNTERPARTY-OCN TO WS-D1-OCN.
           MOVE WS-RAO-HOLD TO WS-D1-RAO.
           MOVE WS-REGION-HOLD TO WS-D1-REG.
           MOVE WS-PE-EXCH-YYDDD TO WS-D1-EXCH.
           MOVE ST-SETTLE-PERIOD TO WS-D1-PERIOD.
           MOVE ST-TOTAL-MOU TO WS-D1-MOU.
           MOVE ST-NET-DUE TO WS-D1-AMT.
           MOVE ST-DIRECTION TO WS-D1-DIR.
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
           MOVE 260                    TO CT-STEP-SEQ.
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
           PERFORM P3600-WRITE-TRAILER THRU P3600-EXIT.
           DISPLAY 'EXCHANGED        ' WS-EXCH-CNT.
           DISPLAY 'HELD FOR NEXT    ' WS-HELD-CNT.
           DISPLAY 'RAO NOT FOUND    ' WS-NORAO-CNT.
           DISPLAY 'EXCHANGE AMOUNT  ' WS-TOT-EXCH-AMT.
           DISPLAY 'FORMATTER CALLS  ' WS-CALL-COUNT.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE SETTLE-IN-FILE
                 CARRIER-MASTER
                 CMDS-OUT-FILE
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

