      *****************************************************************
      * CABSET08 - CMDS RAO INBOUND EXCHANGE CONSUMPTION              *
      * APPLICATION : SETL                                            *
      * INPUTS      : CMDSIN   TELCABS.SETL.CMDS.IN(0)        CABSSETL*
      * INPUTS      : CARRMAST TELCABS.SETL.CARRIER           CABSCARR*
      * INPUTS      : SYSIN    INSTREAM                       NONE    *
      * OUTPUTS     : SETLADD  TELCABS.SETL.SETTLE.ADD(+1)    CABSSETL*
      * OUTPUTS     : REPORT   SYSOUT                         CABSPRNT*
      * CONTROL     : CTLOUT                         CABSCTL          *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARIS*
      *               SUMMARISED = HEADER AND TRAILER RECORDS         *
      *               THE TRAILER HASH MUST AGREE WITH WHAT WAS READ  *
      * RESTART     : FULL RERUN - THE INBOUND FILE IS NOT CONSUMED   *
      * STANDARDS   : CODED TO CABS-STD-014 (RECORD LAYOUTS) AND      *
      *               CABS-STD-041 (MONEY FIELDS). REVIEWED AT THE    *
      *               1994 REWRITE BASELINE AND NOT REOPENED SINCE.   *
      * REVISION HISTORY                                              *
      *   V1.00  1988-01-18  R.T.WHEELER   INITIAL                    *
      *   V1.05  1992-04-23  D.OKONKWO     TRAILER BALANCE ADDED      *
      *   V2.00  1997-02-11  J.M.CASTILLO  Y2K - PIVOT 70 APPLIED     *
      *   V2.03  2003-08-14  P.NAIR        UNKNOWN RAO TO SUSPENSE    *
      *   V2.05  2009-06-18  A.BUKOWSKI    DUPLICATE FILE DETECTION   *
      *   V2.07  2015-12-01  L.FERREIRA    RECOMPILE ONLY             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CABSET08.
       AUTHOR.        R.T.WHEELER.
       DATE-WRITTEN.  1988-01-18.
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
      * INBOUND CMDS EXCHANGE FILE FROM THE OTHER RBOC
           SELECT CMDS-IN-FILE
               ASSIGN TO UT-S-CMDSIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
      * CARRIER MASTER - RAO TO OCN TRANSLATION
           SELECT CARRIER-MASTER
               ASSIGN TO DA-I-CARRMAST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CRM-KEY
               FILE STATUS IS WS-FS-TABLE.
      * SETTLEMENT RECORDS DERIVED FROM THE INBOUND EXCHANGE
           SELECT SETTLE-ADD-FILE
               ASSIGN TO UT-S-SETLADD
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
       FD  CMDS-IN-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 180 CHARACTERS
               DATA RECORD IS CMI-RECORD.
       01  CMI-RECORD              PIC X(180).

       FD  CARRIER-MASTER
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 120 CHARACTERS
               DATA RECORD IS CRM-RECORD.
       01  CRM-RECORD.
           05  CRM-KEY                 PIC X(04).
           05  CRM-DATA                PIC X(116).

       FD  SETTLE-ADD-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 180 CHARACTERS
               DATA RECORD IS SAD-RECORD.
       01  SAD-RECORD              PIC X(180).

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
           05  WS-PGM-NAME         PIC X(08)         VALUE 'CABSET08'.
           05  WS-PGM-VERSION        PIC X(05)           VALUE 'V2.07'.
           05  WS-PGM-APPL           PIC X(04)           VALUE 'SETL'.
           05  WS-PGM-COMPILE      PIC X(08)         VALUE '20151201'.
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
           05  WS-PE-EXPECT-RAO        PIC X(03).
           05  WS-PE-FILE-SEQ          PIC 9(06).
           05  WS-PE-FILLER            PIC X(26).
       01  WS-PARM-OLD REDEFINES WS-PARM-CARD.
      * THE 1989 CARD.  TWO REGIONS NEVER CONVERTED THEIR JCL AND
      * THE EDIT STILL ACCEPTS IT.  THE FIELDS ARE IN A DIFFERENT
      * ORDER AND THE CYCLE DATE IS SIX DIGITS YYMMDD, NOT YYDDD.
           05  FILLER                  PIC X(02).
           05  WS-PO-CYCLE             PIC 9(05).
           05  WS-PO-RUN-ID            PIC X(12).
           05  WS-PO-YYMMDD            PIC 9(06).
           05  WS-PO-RAO               PIC X(03).
           05  FILLER                  PIC X(20).

      * LOCAL SWITCHES.  THE STANDARD ONES ARRIVE VIA CABSWRK.
       01  WS-LOCAL-SWITCHES.
           05  WS-PARM-EOF-SW          PIC X(01)             VALUE 'N'.
                   88  WS-PARM-EOF             VALUE 'Y'.
           05  WS-RAO-FOUND-SW         PIC X(01)             VALUE 'N'.
                   88  WS-RAO-FOUND            VALUE 'Y'.
           05  WS-HEADER-SW            PIC X(01)             VALUE 'N'.
                   88  WS-HEADER-SEEN          VALUE 'Y'.
           05  WS-TRAILER-SW           PIC X(01)             VALUE 'N'.
                   88  WS-TRAILER-SEEN         VALUE 'Y'.

      * SUBSCRIPTS AND INDEX WORK FIELDS.
       01  WS-SUBSCRIPTS.
           05  WS-SUB1                 PIC S9(05) COMP-3     VALUE 0.
           05  WS-SUB2                 PIC S9(05) COMP-3     VALUE 0.

      * THE INBOUND CMDS RECORD.  SAME 180 BYTE INDUSTRY FORMAT
      * AS THE OUTBOUND FILE, REDEFINED THE SAME THREE WAYS.
       01  WS-CMDS-RECORD.
           05  WS-CM-REC-TYPE        PIC X(02)           VALUE SPACES.
           05  WS-CM-BODY            PIC X(178)          VALUE SPACES.
       01  WS-CMDS-DETAIL REDEFINES WS-CMDS-RECORD.
           05  WS-CD-TYPE              PIC X(02).
           05  WS-CD-FROM-RAO          PIC X(03).
           05  WS-CD-TO-RAO            PIC X(03).
           05  WS-CD-EXCH-YYDDD.
               10  WS-CD-EXCH-YY           PIC 9(02).
               10  WS-CD-EXCH-DDD          PIC 9(03).
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

      * INBOUND WORK AREA.  THE EXCHANGE DATE ARRIVES AS A TWO
      * DIGIT YEAR AND MUST BE EXPANDED BEFORE IT CAN BE
      * COMPARED WITH ANYTHING.
       01  WS-INBOUND-WORK.
           05  WS-IW-AMOUNT            PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-IW-MOU               PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-IW-CCYY              PIC 9(04)             VALUE 0.
           05  WS-IW-EXCH-CCYYDDD      PIC 9(07)             VALUE 0.
           05  WS-IW-CYCLE-CCYY        PIC 9(04)             VALUE 0.

      * INBOUND TOTALS AND THE TRAILER FIGURES TO BALANCE THEM.
       01  WS-INBOUND-TOTALS.
           05  WS-DET-CNT              PIC S9(09) COMP-3     VALUE 0.
           05  WS-NORAO-CNT            PIC S9(09) COMP-3     VALUE 0.
           05  WS-HASH-AMT             PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-HASH-MOU             PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-TRL-COUNT            PIC S9(09) COMP-3     VALUE 0.
           05  WS-TRL-HASH-AMT         PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-TRL-HASH-MOU         PIC S9(15)V9(02) COMP-3 VALUE 0.

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
                   VALUE 'CMDS INBOUND EXCHANGE REGISTER'.
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
                   VALUE 'TYP FROM TO   EXCH-DT  OCN  PERIOD  MINUTES'.
           05  FILLER              PIC X(33)
                   VALUE '     AMOUNT           DISPOSITION'.
       01  WS-HEAD-4.
           05  FILLER                  PIC X(01)             VALUE ' '.
           05  FILLER                PIC X(131)          VALUE ALL '-'.

      * DETAIL LINE WS-DETAIL-1.
       01  WS-DETAIL-1.
           05  FILLER                  PIC X(01)             VALUE ' '.
           05  WS-D1-TYPE              PIC X(02).
           05  FILLER                PIC X(02)           VALUE SPACES.
           05  WS-D1-FROM              PIC X(03).
           05  FILLER                PIC X(02)           VALUE SPACES.
           05  WS-D1-TO                PIC X(03).
           05  FILLER                PIC X(02)           VALUE SPACES.
           05  WS-D1-EXCH              PIC 9(05).
           05  FILLER                PIC X(02)           VALUE SPACES.
           05  WS-D1-OCN               PIC X(04).
           05  FILLER                PIC X(02)           VALUE SPACES.
           05  WS-D1-PERIOD            PIC 9(06).
           05  FILLER                PIC X(02)           VALUE SPACES.
           05  WS-D1-MOU               PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-AMT               PIC ZZZ,ZZZ,ZZ9.99-.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-DISPO             PIC X(20).

      * MESSAGE TEXT TABLE.  ADDRESSED BY ERROR CODE SEQUENCE, NOT
      * BY THE CODE ITSELF - INSERTING A CODE IN THE MIDDLE SHIFTS
      * EVERY MESSAGE.  THIS HAS HAPPENED TWICE (1994 AND 2007).
       01  WS-MSG-CONST.
           05  FILLER              PIC X(44)
                   VALUE 'DETAIL RECORD ACCEPTED                      '.
           05  FILLER              PIC X(44)
                   VALUE 'HEADER RECORD                               '.
           05  FILLER              PIC X(44)
                   VALUE 'TRAILER RECORD                              '.
           05  FILLER              PIC X(44)
                   VALUE 'RECORD TYPE NOT RECOGNISED                  '.
           05  FILLER              PIC X(44)
                   VALUE 'RAO NOT TRANSLATED TO AN OCN                '.
           05  FILLER              PIC X(44)
                   VALUE 'EXCHANGE DATE IN THE FUTURE                 '.
           05  FILLER              PIC X(44)
                   VALUE 'FILE SEQUENCE ALREADY PROCESSED             '.
           05  FILLER              PIC X(44)
                   VALUE 'TRAILER HASH DOES NOT AGREE                 '.
           05  FILLER              PIC X(44)
                   VALUE 'HEADER RAO NOT THE EXPECTED ONE             '.
           05  FILLER              PIC X(44)
                   VALUE 'END OF INBOUND EXCHANGE RUN                 '.
       01  WS-MSG-TABLE REDEFINES WS-MSG-CONST.
           05  WS-MSG-ENTRY OCCURS 10 TIMES
                   INDEXED BY WS-MSG-IX.
               10  WS-MSG-TEXT             PIC X(44).

      * HOLD FIELDS AND THE EXPANDED CYCLE DATE.
       01  WS-INBOUND-HOLD.
           05  WS-OCN-HOLD           PIC X(04)           VALUE SPACES.
           05  WS-SEQ-NBR              PIC 9(09) COMP-3      VALUE 0.
           05  WS-BAL-IND              PIC X(01)             VALUE ' '.
           05  WS-IW-CCYY-CYCLE-DDD    PIC 9(07)             VALUE 0.

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
           OPEN INPUT  CMDS-IN-FILE
                       CARRIER-MASTER
                       PARM-FILE
           OPEN OUTPUT SETTLE-ADD-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 5801 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-CMDSIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 5802 TO WS-AB-CODE
               MOVE 'OPEN FAILED DA-I-CARRMAST' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 5803 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-SETLADD' TO WS-AB-TEXT
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
           MOVE ZERO TO WS-DET-CNT WS-NORAO-CNT
                        WS-HASH-AMT WS-HASH-MOU
                        WS-TRL-COUNT WS-TRL-HASH-AMT.
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
           IF WS-PE-FILE-SEQ NOT NUMERIC
               MOVE ZERO TO WS-PE-FILE-SEQ.

       P1200-EXIT.
           EXIT.


      *****************************************************************
      * S200-INBOUND                                                  *
      * CONSUME THE INBOUND EXCHANGE FILE.                            *
      *****************************************************************
       S200-INBOUND SECTION.

       P2000-PROCESS.
      * ONE INBOUND RECORD PER PASS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ THRU P2100-EXIT.
           IF WS-EOF
               PERFORM P5000-BALANCE-CHECK THRU P5000-EXIT
               GO TO P2000-EXIT.
           MOVE CMI-RECORD TO WS-CMDS-RECORD.
           MOVE SPACES TO WS-D1-DISPO.
           IF WS-CM-REC-TYPE = 'HD'
               PERFORM P2200-HEADER THRU P2200-EXIT
               ADD 1 TO WS-SUMM-CNT
               GO TO P2000-EXIT.
           IF WS-CM-REC-TYPE = 'TR'
               PERFORM P2300-TRAILER THRU P2300-EXIT
               ADD 1 TO WS-SUMM-CNT
               GO TO P2000-EXIT.
           IF WS-CM-REC-TYPE NOT = 'DT'
               MOVE EC-DATE-INVALID TO WS-ERR-CODE
               MOVE 'E' TO WS-ERR-SEVERITY
               MOVE WS-MSG-TEXT (4) TO WS-D1-DISPO
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               GO TO P2000-EXIT.
           PERFORM P2400-DETAIL-EDIT THRU P2400-EXIT.
           IF WS-ERROR-FOUND
               MOVE 'N' TO WS-ERROR-SW
               GO TO P2000-EXIT.
           PERFORM P3000-BUILD-SETTLE THRU P3000-EXIT.
           PERFORM P6100-DETAIL THRU P6100-EXIT.
           ADD 1 TO WS-WRITE-CNT.
           ADD 1 TO WS-DET-CNT.

       P2000-EXIT.
           EXIT.

       P2100-READ.
      * SEQUENTIAL READ OF THE INBOUND FILE.
           READ CMDS-IN-FILE
               AT END
                   MOVE 'Y' TO WS-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00'
               MOVE 3580 TO WS-AB-CODE
               MOVE 'READ ERROR UT-S-CMDSIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-READ-CNT.

       P2100-EXIT.
           EXIT.

       P2200-HEADER.
      * THE HEADER CARRIES THE SENDING RAO AND A FILE SEQUENCE.  A
      * SEQUENCE THAT HAS ALREADY BEEN PROCESSED MEANS THE SAME
      * FILE HAS ARRIVED TWICE, WHICH HAPPENED IN 2009 AND DOUBLE
      * SETTLED AN ENTIRE REGION FOR A MONTH.
           MOVE 'P2200-HEADER' TO WS-PARA-NAME.
           MOVE 'Y' TO WS-HEADER-SW.
           MOVE WS-MSG-TEXT (2) TO WS-D1-DISPO.
           IF WS-PE-EXPECT-RAO NOT = SPACES
               IF WS-CH-FROM-RAO NOT = WS-PE-EXPECT-RAO
                   MOVE 5810 TO WS-AB-CODE
                   MOVE 'HEADER RAO NOT AS EXPECTED' TO WS-AB-TEXT
                   PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PE-FILE-SEQ NOT = ZERO
               IF WS-CH-SEQ NOT > WS-PE-FILE-SEQ
                   MOVE 5811 TO WS-AB-CODE
                   MOVE 'FILE SEQUENCE ALREADY PROCESSED' TO WS-AB-TEXT
                   PERFORM P9500-ABEND THRU P9500-EXIT.
           PERFORM P6100-DETAIL THRU P6100-EXIT.

       P2200-EXIT.
           EXIT.

       P2300-TRAILER.
      * THE TRAILER CARRIES THE COUNT AND THE TWO HASH TOTALS.
           MOVE 'Y' TO WS-TRAILER-SW.
           MOVE WS-CT-COUNT TO WS-TRL-COUNT.
           MOVE WS-CT-HASH-AMT TO WS-TRL-HASH-AMT.
           MOVE WS-CT-HASH-MOU TO WS-TRL-HASH-MOU.
           MOVE WS-MSG-TEXT (3) TO WS-D1-DISPO.
           PERFORM P6100-DETAIL THRU P6100-EXIT.

       P2300-EXIT.
           EXIT.

       P2400-DETAIL-EDIT.
      * EDIT THE DETAIL RECORD.  THE EXCHANGE DATE ARRIVES AS A TWO
      * DIGIT YEAR JULIAN AND IS EXPANDED HERE WITH THE PIVOT OF
      * 70 - A YEAR BELOW 70 IS TWENTY FIRST CENTURY.  THE SAME
      * PIVOT IS CODED IN SIX OTHER PLACES.  AN EXCHANGE DATE IN
      * THE FUTURE IS REJECTED.
           MOVE 'P2400-DETAIL-EDIT' TO WS-PARA-NAME.
           IF WS-CD-EXCH-YY < 70
               COMPUTE WS-IW-CCYY = 2000 + WS-CD-EXCH-YY
           ELSE
               COMPUTE WS-IW-CCYY = 1900 + WS-CD-EXCH-YY.
           COMPUTE WS-IW-EXCH-CCYYDDD =
                   (WS-IW-CCYY * 1000) + WS-CD-EXCH-DDD.
           IF WS-CYCLE-YY < 70
               COMPUTE WS-IW-CYCLE-CCYY = 2000 + WS-CYCLE-YY
           ELSE
               COMPUTE WS-IW-CYCLE-CCYY = 1900 + WS-CYCLE-YY.
           COMPUTE WS-IW-CCYY-CYCLE-DDD =
                   (WS-IW-CYCLE-CCYY * 1000) + WS-CYCLE-DDD.
           IF WS-IW-EXCH-CCYYDDD > WS-IW-CCYY-CYCLE-DDD
               MOVE EC-DATE-INVALID TO WS-ERR-CODE
               MOVE 'E' TO WS-ERR-SEVERITY
               MOVE WS-MSG-TEXT (6) TO WS-D1-DISPO
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               MOVE 'Y' TO WS-ERROR-SW
               GO TO P2400-EXIT.
           PERFORM P2500-RAO-TRANSLATE THRU P2500-EXIT.
           IF NOT WS-RAO-FOUND
               ADD 1 TO WS-NORAO-CNT
               MOVE EC-OCN-UNKNOWN TO WS-ERR-CODE
               MOVE 'E' TO WS-ERR-SEVERITY
               MOVE WS-MSG-TEXT (5) TO WS-D1-DISPO
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               MOVE 'Y' TO WS-ERROR-SW
               GO TO P2400-EXIT.
           MOVE WS-CD-AMOUNT TO WS-IW-AMOUNT.
           MOVE WS-CD-MOU TO WS-IW-MOU.
           ADD WS-IW-AMOUNT TO WS-HASH-AMT.
           ADD WS-IW-MOU TO WS-HASH-MOU.
           ADD WS-IW-AMOUNT TO WS-ACC-AMOUNT.
           ADD WS-IW-MOU TO WS-ACC-MINUTES.

       P2400-EXIT.
           EXIT.

       P2500-RAO-TRANSLATE.
      * TRANSLATE THE SENDING RAO INTO AN OCN.  THE INBOUND FILE
      * CARRIES AN OCN AS WELL, BUT IT IS THE OTHER RBOCS VIEW OF
      * THE OCN AND HAS NOT ALWAYS AGREED WITH OURS.
           MOVE 'N' TO WS-RAO-FOUND-SW.
           MOVE SPACES TO WS-OCN-HOLD.
           SET WS-RA-IX TO 1.
           SEARCH WS-RA-ENTRY
               AT END
                   GO TO P2500-EXIT
               WHEN WS-RA-RAO (WS-RA-IX) = WS-CD-FROM-RAO
                   MOVE WS-RA-OCN (WS-RA-IX) TO WS-OCN-HOLD
                   MOVE 'Y' TO WS-RAO-FOUND-SW.

       P2500-EXIT.
           EXIT.


      *****************************************************************
      * S300-BUILD                                                    *
      * BUILD THE SETTLEMENT RECORD.                                  *
      *****************************************************************
       S300-BUILD SECTION.

       P3000-BUILD-SETTLE.
      * TURN THE INBOUND EXCHANGE RECORD INTO A SETTLEMENT RECORD
      * OF TYPE C.  THE DIRECTION IS INVERTED - WHAT THE SENDER
      * CALLS A PAYABLE IS OUR RECEIVABLE.
           MOVE SPACES TO CABS-SETTLEMENT-RECORD.
           MOVE 'C' TO ST-SETTLE-TYPE.
           MOVE WS-OCN-HOLD TO ST-COUNTERPARTY-OCN.
           MOVE WS-CD-PERIOD TO ST-SETTLE-PERIOD.
           ADD 1 TO WS-SEQ-NBR.
           MOVE WS-SEQ-NBR TO ST-SEQ.
           MOVE WS-IW-MOU TO ST-TOTAL-MOU.
           MOVE WS-IW-MOU TO ST-BILLABLE-MOU.
           MOVE ZERO TO ST-CAPPED-MOU.
           MOVE WS-IW-AMOUNT TO ST-GROSS-AMT.
           MOVE WS-IW-AMOUNT TO ST-NET-DUE.
           IF WS-CD-DIRECTION = 'P'
               MOVE 'R' TO ST-DIRECTION
           ELSE
               MOVE 'P' TO ST-DIRECTION.
           MOVE 'N' TO ST-DISPUTE-SW.
           MOVE WS-CD-EXCH-YYDDD TO ST-EXCH-YYDDD.
           MOVE WS-CD-FROM-RAO TO ST-RAO-CODE.
           MOVE CABS-SETTLEMENT-RECORD TO SAD-RECORD.
           WRITE SAD-RECORD.

       P3000-EXIT.
           EXIT.


      *****************************************************************
      * S500-BALANCE                                                  *
      * TRAILER BALANCE CHECK.                                        *
      *****************************************************************
       S500-BALANCE SECTION.

       P5000-BALANCE-CHECK.
      * THE COUNT AND HASH TOTALS READ MUST AGREE WITH THE TRAILER.
      * A DISAGREEMENT MEANS THE FILE WAS TRUNCATED IN TRANSIT AND
      * THE WHOLE EXCHANGE MUST BE REJECTED AND REQUESTED AGAIN.
           MOVE 'P5000-BALANCE-CHECK' TO WS-PARA-NAME.
           IF NOT WS-TRAILER-SEEN
               MOVE 5820 TO WS-AB-CODE
               MOVE 'NO TRAILER ON INBOUND FILE' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-DET-CNT NOT = WS-TRL-COUNT
               MOVE 5821 TO WS-AB-CODE
               MOVE 'TRAILER COUNT DOES NOT AGREE' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-HASH-AMT NOT = WS-TRL-HASH-AMT
               MOVE 5822 TO WS-AB-CODE
               MOVE 'TRAILER HASH AMOUNT DISAGREES' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           MOVE 'B' TO WS-BAL-IND.

       P5000-EXIT.
           EXIT.


      *****************************************************************
      * S600-REPORT                                                   *
      * INBOUND REGISTER.                                             *
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
      * ONE LINE PER INBOUND RECORD.
           IF WS-LINE-CNT > WS-MAX-LINES
               PERFORM P6000-HEADING THRU P6000-EXIT.
           MOVE WS-CM-REC-TYPE TO WS-D1-TYPE.
           MOVE WS-CD-FROM-RAO TO WS-D1-FROM.
           MOVE WS-CD-TO-RAO TO WS-D1-TO.
           MOVE WS-CD-EXCH-YYDDD TO WS-D1-EXCH.
           MOVE WS-OCN-HOLD TO WS-D1-OCN.
           MOVE WS-CD-PERIOD TO WS-D1-PERIOD.
           MOVE WS-IW-MOU TO WS-D1-MOU.
           MOVE WS-IW-AMOUNT TO WS-D1-AMT.
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
           MOVE WS-CMDS-RECORD TO SU-ORIG-RECORD.
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
           MOVE 270                    TO CT-STEP-SEQ.
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
           DISPLAY 'DETAIL RECORDS   ' WS-DET-CNT.
           DISPLAY 'TRAILER COUNT    ' WS-TRL-COUNT.
           DISPLAY 'HASH AMOUNT READ ' WS-HASH-AMT.
           DISPLAY 'HASH AMOUNT TRLR ' WS-TRL-HASH-AMT.
           DISPLAY 'UNKNOWN RAO      ' WS-NORAO-CNT.
           DISPLAY 'BALANCE INDICATOR' WS-BAL-IND.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE CMDS-IN-FILE
                 CARRIER-MASTER
                 SETTLE-ADD-FILE
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

