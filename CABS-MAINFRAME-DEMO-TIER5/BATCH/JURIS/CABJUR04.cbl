      *****************************************************************
      * CABJUR04 - PERCENT INTERSTATE USAGE APPLICATION               *
      * APPLICATION : CABS                                            *
      * INPUTS      : CDRIN    TELCABS.CABS.CDR.JURIS(0)      CABSCDR *
      * INPUTS      : FCTRVAL  TELCABS.CABS.FACTOR.VAL(0)     CABSFCTR*
      * INPUTS      : SYSIN    INSTREAM                       NONE    *
      * OUTPUTS     : PIUOUT   TELCABS.CABS.CDR.PIU(+1)       CABSCDR *
      * OUTPUTS     : SUSPOUT  TELCABS.CABS.SUSPENSE(+1)      CABSERR *
      * CONTROL     : CTLOUT                         CABSCTL          *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED              *
      *               CT-WRITTEN COUNTS INPUT RECORDS SPLIT, NOT THE  *
      *               PHYSICAL OUTPUT RECORDS - EACH SPLIT WRITES TWO *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * STANDARDS   : CODED TO CABS-STD-014 (RECORD LAYOUTS) AND      *
      *               CABS-STD-058 (DATE HANDLING). REVIEWED AT THE   *
      *               2016 STANDARDS SWEEP. NO WAIVERS ON FILE.       *
      * REVISION HISTORY                                              *
      *   V1.00  1987-08-14  R.T.WHEELER   INITIAL PIU SPLIT          *
      *   V1.04  1989-05-03  R.T.WHEELER   ROUNDED ADDED - AUDIT      *
      *   V1.08  1992-12-01  D.OKONKWO     FACTOR TABLE IN CORE       *
      *   V2.00  1996-06-24  J.M.CASTILLO  Y2K REVIEW - NO IMPACT     *
      *   V2.02  1999-02-11  P.NAIR        DYNAMIC RATE MODULE        *
      *   V2.05  2004-07-07  P.NAIR        MINUTE ROUNDING ALIGNED    *
      *   V2.07  2009-11-13  A.BUKOWSKI    FIVE DECIMAL RETAINED      *
      *   V2.09  2019-01-29  M.OYELARAN    RECOMPILE ONLY             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CABJUR04.
       AUTHOR.        R.T.WHEELER.
       DATE-WRITTEN.  1987-08-14.
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
      * USAGE WITH JURISDICTION CODE APPLIED BY CABJUR03
           SELECT CDR-IN-FILE
               ASSIGN TO UT-S-CDRIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
      * VALIDATED FACTORS FROM CABJUR02 - LOADED TO A TABLE
           SELECT FACTOR-FILE
               ASSIGN TO UT-S-FCTRVAL
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-TABLE.
      * JURISDICTIONALLY SPLIT USAGE - TWO RECORDS PER INPUT
           SELECT SPLIT-OUT-FILE
               ASSIGN TO UT-S-PIUOUT
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

       DATA DIVISION.
       FILE SECTION.
       FD  CDR-IN-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 200 CHARACTERS
               DATA RECORD IS CDI-RECORD.
       01  CDI-RECORD              PIC X(200).

       FD  FACTOR-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 76 CHARACTERS
               DATA RECORD IS FVL-RECORD.
       01  FVL-RECORD              PIC X(76).

       FD  SPLIT-OUT-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 200 CHARACTERS
               DATA RECORD IS SPO-RECORD.
       01  SPO-RECORD              PIC X(200).

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
           05  WS-PGM-NAME         PIC X(08)         VALUE 'CABJUR04'.
           05  WS-PGM-VERSION        PIC X(05)           VALUE 'V2.09'.
           05  WS-PGM-APPL           PIC X(04)           VALUE 'CABS'.
           05  WS-PGM-COMPILE      PIC X(08)         VALUE '20190129'.
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

       COPY CABSCDR.

       COPY CABSFCTR.

       COPY CABSRATE.

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
           05  WS-PE-RATE-TARIFF       PIC X(04).
           05  WS-PE-DEF-PIU           PIC 9(03)V9(05).
           05  WS-PE-FILLER            PIC X(23).
       01  WS-PARM-OLD REDEFINES WS-PARM-CARD.
      * THE 1989 CARD.  TWO REGIONS NEVER CONVERTED THEIR JCL AND
      * THE EDIT STILL ACCEPTS IT.  THE FIELDS ARE IN A DIFFERENT
      * ORDER AND THE CYCLE DATE IS SIX DIGITS YYMMDD, NOT YYDDD.
           05  FILLER                  PIC X(02).
           05  WS-PO-CYCLE             PIC 9(05).
           05  WS-PO-RUN-ID            PIC X(12).
           05  WS-PO-YYMMDD            PIC 9(06).
           05  WS-PO-TARIFF            PIC X(04).
           05  FILLER                  PIC X(20).

      * LOCAL SWITCHES.  THE STANDARD ONES ARRIVE VIA CABSWRK.
       01  WS-LOCAL-SWITCHES.
           05  WS-PARM-EOF-SW          PIC X(01)             VALUE 'N'.
                   88  WS-PARM-EOF             VALUE 'Y'.
           05  WS-FCTR-EOF-SW          PIC X(01)             VALUE 'N'.
                   88  WS-FCTR-EOF             VALUE 'Y'.
           05  WS-SPLIT-SW             PIC X(01)             VALUE 'N'.
                   88  WS-SPLIT-DONE           VALUE 'Y'.
           05  WS-RATE-FOUND-SW        PIC X(01)             VALUE 'N'.
                   88  WS-RATE-FOUND           VALUE 'Y'.

      * SUBSCRIPTS AND INDEX WORK FIELDS.
       01  WS-SUBSCRIPTS.
           05  WS-SUB1                 PIC S9(05) COMP-3     VALUE 0.
           05  WS-SUB2                 PIC S9(05) COMP-3     VALUE 0.
           05  WS-IDX                  PIC S9(05) COMP-3     VALUE 0.

      * IN CORE FACTOR TABLE.  LOADED ONCE AT INITIALISATION FROM
      * THE VALIDATED FACTOR FILE.  THE TABLE IS SEARCHED SERIALLY
      * BECAUSE THE FILE IS NOT GUARANTEED TO BE IN KEY SEQUENCE
      * WHEN THE OPERATOR SUPPLIES A PARTIAL RELOAD.
       01  WS-FACTOR-TABLE.
           05  WS-FT-ENTRY OCCURS 2500 TIMES
                   INDEXED BY WS-FT-IX.
               10  WS-FT-KEY.
                   15  WS-FT-OCN               PIC X(04).
                   15  WS-FT-STATE             PIC X(02).
                   15  WS-FT-LATA              PIC 9(03).
               10  WS-FT-EFF               PIC 9(05).
               10  WS-FT-PIU               PIC S9(03)V9(05) COMP-3.
               10  WS-FT-PLU               PIC S9(03)V9(05) COMP-3.
               10  WS-FT-PSU               PIC S9(03)V9(05) COMP-3.
               10  WS-FT-SOURCE            PIC X(01).
       01  WS-FACTOR-TABLE-CTL.
           05  WS-FT-COUNT             PIC S9(05) COMP-3     VALUE 0.
           05  WS-FT-MAX               PIC S9(05) COMP-3     VALUE 2500.
           05  WS-FT-FOUND-SW          PIC X(01)             VALUE 'N'.
                   88  WS-FT-FOUND              VALUE 'Y'.

      * PIU WORK AREA.  THE SPLIT IS
      *   INTERSTATE MOU = CHARGEABLE MOU * PIU / 100
      *   INTRASTATE MOU = CHARGEABLE MOU - INTERSTATE MOU
      * AND THE SAME PROPORTION IS APPLIED TO THE MONEY AT THE
      * JURISDICTION SPECIFIC RATE.  THE SUBTRACTION RATHER THAN
      * A SECOND MULTIPLICATION IS PER CABS-STD-041 - IT GUARANTEES
      * THE TWO HALVES ADD BACK TO THE ORIGINAL MINUTES.
       01  WS-PIU-WORK.
           05  WS-PW-BASE-MOU          PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-PW-IS-MOU            PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-PW-SS-MOU            PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-PW-PIU               PIC S9(03)V9(05) COMP-3 VALUE 0.
           05  WS-PW-IS-RATE           PIC S9(05)V9(05) COMP-3 VALUE 0.
           05  WS-PW-SS-RATE           PIC S9(05)V9(05) COMP-3 VALUE 0.
           05  WS-PW-IS-AMT            PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-PW-SS-AMT            PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-PW-GROSS-AMT         PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-PW-CHECK-AMT         PIC S9(13)V9(05) COMP-3 VALUE 0.

      * RUN TOTALS.  THE ROUNDING RESIDUE IS THE ACCUMULATED
      * DIFFERENCE BETWEEN THE ROUNDED INTERSTATE AMOUNT AND THE
      * UNROUNDED ONE.  IT IS DISPLAYED BUT NEVER POSTED.
       01  WS-PIU-TOTALS.
           05  WS-TOT-IS-MOU           PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-TOT-SS-MOU           PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-TOT-IS-AMT           PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-TOT-SS-AMT           PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-APPLIED-CNT          PIC S9(11) COMP-3     VALUE 0.
           05  WS-DEFAULT-CNT          PIC S9(11) COMP-3     VALUE 0.
           05  WS-PHYS-WRITE-CNT       PIC S9(11) COMP-3     VALUE 0.
           05  WS-ROUND-RESIDUE        PIC S9(07)V9(05) COMP-3 VALUE 0.

      * OUTPUT RECORD BUILD AREA.  THE SPLIT RECORD IS A CDR WITH
      * THE JURISDICTION FORCED AND THE MINUTES REPLACED.  TWO
      * REDEFINES CARRY THE ALTERNATE VIEWS USED BY THE SUMMARY.
       01  WS-SPLIT-RECORD.
           05  WS-SR-BODY            PIC X(200)          VALUE SPACES.
       01  WS-SPLIT-VIEW REDEFINES WS-SPLIT-RECORD.
           05  WS-SV-KEY               PIC X(21).
           05  WS-SV-CTL               PIC X(10).
           05  WS-SV-DATES             PIC X(27).
           05  WS-SV-VARIANT           PIC X(96).
           05  WS-SV-AUDIT             PIC X(46).
       01  WS-SPLIT-SUMM REDEFINES WS-SPLIT-RECORD.
           05  WS-SM-OCN               PIC X(04).
           05  WS-SM-BAN               PIC X(13).
           05  WS-SM-REST              PIC X(183).

      * THE RATE MODULE NAME IS BUILT FROM THE JURISDICTION CODE
      * AT RUN TIME.  CABRTFCC IS THE FCC TARIFF MODULE, CABRTSTA
      * THE STATE TARIFF MODULE.
       01  WS-CALL-AREA.
           05  WS-CALL-PGM.
               10  WS-CP-STEM          PIC X(05)         VALUE 'CABRT'.
               10  WS-CP-SUFFIX        PIC X(03)         VALUE SPACES.
           05  WS-CALL-RC              PIC S9(04) COMP       VALUE 0.
           05  WS-CALL-COUNT           PIC S9(07) COMP-3     VALUE 0.
           05  WS-CALL-SUFFIX-TAB.
               10  FILLER              PIC X(05)         VALUE 'ISFCC'.
               10  FILLER              PIC X(05)         VALUE 'SSSTA'.
               10  FILLER              PIC X(05)         VALUE 'LCLOC'.
               10  FILLER              PIC X(05)         VALUE 'XXDEF'.
           05  WS-CALL-SUFFIX-R REDEFINES WS-CALL-SUFFIX-TAB.
               10  WS-CS-ENT OCCURS 4 TIMES
                   INDEXED BY WS-CS-IX.
                   15  WS-CS-KEY               PIC X(02).
                   15  WS-CS-SUF               PIC X(03).

      * RATE ELEMENT ATTRIBUTE TABLE.  COLUMN 3 SAYS WHETHER PIU IS
      * APPLIED TO THE ELEMENT AT ALL.  COLUMN 5 CARRIES A ROUNDING
      * RULE THAT IS IGNORED BY THIS PROGRAM AND HONOURED BY CABJUR09
      * - THAT DIVERGENCE IS KNOWN AND HAS BEEN OPEN SINCE 1996.
       01  WS-RELEM-CONST.
           05  FILLER              PIC X(10)         VALUE 'CCLTRMIYYU'.
           05  FILLER              PIC X(10)         VALUE 'CCLORGSYNT'.
           05  FILLER              PIC X(10)         VALUE 'LSWTCHLYNE'.
           05  FILLER              PIC X(10)         VALUE 'TSWTCHINNC'.
           05  FILLER              PIC X(10)         VALUE 'TNDMSWSYNU'.
           05  FILLER              PIC X(10)         VALUE 'LTRANSLYYT'.
           05  FILLER              PIC X(10)         VALUE 'ENTRANIYNE'.
           05  FILLER              PIC X(10)         VALUE 'COMTRNSNNC'.
           05  FILLER              PIC X(10)         VALUE 'DTTRANLYNU'.
           05  FILLER              PIC X(10)         VALUE 'LOCTRMIYNT'.
           05  FILLER              PIC X(10)         VALUE 'LOCORGSYYE'.
           05  FILLER              PIC X(10)         VALUE '800DBQLNNC'.
           05  FILLER              PIC X(10)         VALUE 'SS7ISPIYNU'.
           05  FILLER              PIC X(10)         VALUE 'QUERY1SYNT'.
           05  FILLER              PIC X(10)         VALUE 'DEDTRNLYNE'.
           05  FILLER              PIC X(10)         VALUE 'SPCLACINYC'.
           05  FILLER              PIC X(10)         VALUE 'DS1LOCSYNU'.
           05  FILLER              PIC X(10)         VALUE 'DS3LOCLYNT'.
           05  FILLER              PIC X(10)         VALUE 'UNEPRTIYNE'.
           05  FILLER              PIC X(10)         VALUE 'UNELOPSNNC'.
           05  FILLER              PIC X(10)         VALUE 'COLLOCLYYU'.
           05  FILLER              PIC X(10)         VALUE 'MPBTRNIYNT'.
           05  FILLER              PIC X(10)         VALUE 'RECIPTSYNE'.
           05  FILLER              PIC X(10)         VALUE 'ISPBNDLNNC'.
           05  FILLER              PIC X(10)         VALUE 'TRANSPIYNU'.
           05  FILLER              PIC X(10)         VALUE 'TERMINSYYT'.
           05  FILLER              PIC X(10)         VALUE 'ORIGINLYNE'.
           05  FILLER              PIC X(10)         VALUE 'MOUCHGINNC'.
           05  FILLER              PIC X(10)         VALUE 'SETUPCSYNU'.
           05  FILLER              PIC X(10)         VALUE 'MINCHGLYNT'.
           05  FILLER              PIC X(10)         VALUE 'CARCOMIYYE'.
           05  FILLER              PIC X(10)         VALUE 'LNKCHGSNNC'.
           05  FILLER              PIC X(10)         VALUE 'DBQCHGLYNU'.
           05  FILLER              PIC X(10)         VALUE 'PORTCHIYNT'.
           05  FILLER              PIC X(10)         VALUE 'XCONNCSYNE'.
           05  FILLER              PIC X(10)         VALUE 'ENTFACLNYC'.
           05  FILLER              PIC X(10)         VALUE 'TANDEMIYNU'.
           05  FILLER              PIC X(10)         VALUE 'ENDOFFSYNT'.
           05  FILLER              PIC X(10)         VALUE 'SHRTRNLYNE'.
           05  FILLER              PIC X(10)         VALUE 'WIRTRMINNC'.
       01  WS-RELEM-TABLE REDEFINES WS-RELEM-CONST.
           05  WS-WS-RE-ENTRY OCCURS 40 TIMES
                   INDEXED BY WS-RE-IX.
               10  WS-RE-ELEM              PIC X(06).
               10  WS-RE-JURIS             PIC X(01).
               10  WS-RE-PIU-APPL          PIC X(01).
               10  WS-RE-PLU-APPL          PIC X(01).
               10  WS-RE-ROUND             PIC X(01).

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
                   VALUE 'PIU APPLIED FROM FILED FACTOR               '.
           05  FILLER              PIC X(44)
                   VALUE 'PIU APPLIED FROM CARRIER DEFAULT            '.
           05  FILLER              PIC X(44)
                   VALUE 'PIU APPLIED FROM CONTROL CARD DEFAULT       '.
           05  FILLER              PIC X(44)
                   VALUE 'NO FACTOR FOUND - RECORD SUSPENDED          '.
           05  FILLER              PIC X(44)
                   VALUE 'RATE MODULE RETURNED NON ZERO               '.
           05  FILLER              PIC X(44)
                   VALUE 'CHARGEABLE MINUTES ZERO - NO SPLIT          '.
           05  FILLER              PIC X(44)
                   VALUE 'FACTOR TABLE FULL - LOAD TRUNCATED          '.
           05  FILLER              PIC X(44)
                   VALUE 'INTERSTATE AMOUNT ROUNDED AT FIVE PLACES    '.
           05  FILLER              PIC X(44)
                   VALUE 'SPLIT DID NOT RECONCILE TO GROSS            '.
           05  FILLER              PIC X(44)
                   VALUE 'RATE ELEMENT NOT SUBJECT TO PIU             '.
       01  WS-MSG-TABLE REDEFINES WS-MSG-CONST.
           05  WS-MSG-ENTRY OCCURS 10 TIMES
                   INDEXED BY WS-MSG-IX.
               10  WS-MSG-TEXT             PIC X(44).

      * RATE MODULE PARAMETER AREA.  PASSED BY REFERENCE TO
      * WHICHEVER RATE MODULE THE DYNAMIC CALL RESOLVES TO.
       01  WS-RATE-PARM.
           05  WS-RATE-TARIFF        PIC X(04)           VALUE SPACES.
           05  WS-RATE-ELEMENT       PIC X(06)           VALUE SPACES.
           05  WS-RATE-JURIS         PIC X(02)           VALUE SPACES.
           05  WS-RATE-STATE         PIC X(02)           VALUE SPACES.
           05  WS-RATE-EFF             PIC 9(05)             VALUE 0.
           05  WS-RATE-RETURNED        PIC S9(05)V9(05) COMP-3 VALUE 0.
           05  WS-RATE-RULE          PIC X(01)           VALUE SPACES.
       01  WS-MISC-WORK.
           05  WS-BEST-EFF             PIC 9(05)             VALUE 0.
           05  WS-PW-CHECK-MOU         PIC S9(13)V9(02) COMP-3 VALUE 0.

      * LAYOUTS COPIED FOR THE CROSS REFERENCE AND VALIDATION
      * ROUTINES.  NOT EVERY FIELD IN EVERY LAYOUT IS USED BY
      * THIS MODULE - THE COPY IS HERE BECAUSE THE LAYOUT WAS
      * NEEDED AT SOME POINT AND REMOVING A COPY MEMBER FORCES
      * A FULL REGRESSION UNDER CABS-STD-009.
       COPY CABSCARR.

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
           OPEN INPUT  CDR-IN-FILE
                       FACTOR-FILE
                       PARM-FILE
           OPEN OUTPUT SPLIT-OUT-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 4401 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-CDRIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 4402 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-FCTRVAL' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 4403 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-PIUOUT' TO WS-AB-TEXT
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
           MOVE ZERO TO WS-TOT-IS-MOU WS-TOT-SS-MOU
                        WS-TOT-IS-AMT WS-TOT-SS-AMT
                        WS-APPLIED-CNT WS-DEFAULT-CNT
                        WS-PHYS-WRITE-CNT.
           PERFORM P1300-LOAD-FACTORS THRU P1300-EXIT.
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
      * SCHEDULER SUBSTITUTION RULES ARE IN CABS-STD-022.
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
           IF WS-PE-RATE-TARIFF = SPACES
               MOVE 'FCC1' TO WS-PE-RATE-TARIFF.
           IF WS-PE-DEF-PIU NOT NUMERIC
               MOVE 050.00000 TO WS-PE-DEF-PIU.

       P1200-EXIT.
           EXIT.

       P1300-LOAD-FACTORS.
      * LOAD THE VALIDATED FACTOR FILE INTO CORE.  A FULL TABLE IS
      * NOT AN ERROR - THE LOAD SIMPLY STOPS AND THE REMAINING
      * CARRIERS PICK UP THE CONTROL CARD DEFAULT.  THAT BEHAVIOUR
      * HAS BEEN IN PLACE SINCE 1992 AND IS RELIED ON AT QUARTER END.
           MOVE 'P1300-LOAD-FACTORS' TO WS-PARA-NAME.
           MOVE ZERO TO WS-FT-COUNT.
           PERFORM P1310-READ-FACTOR THRU P1310-EXIT
               UNTIL WS-FCTR-EOF.
           DISPLAY 'FACTOR TABLE ENTRIES LOADED ' WS-FT-COUNT.
           IF WS-FT-COUNT = ZERO
               MOVE 4404 TO WS-AB-CODE
               MOVE 'FACTOR FILE EMPTY - CANNOT SPLIT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.

       P1300-EXIT.
           EXIT.

       P1310-READ-FACTOR.
      * ONE FACTOR RECORD INTO THE TABLE.
           READ FACTOR-FILE
               AT END
                   MOVE 'Y' TO WS-FCTR-EOF-SW
                   GO TO P1310-EXIT.
           MOVE FVL-RECORD TO CABS-FACTOR-RECORD.
           IF WS-FT-COUNT NOT < WS-FT-MAX
               GO TO P1310-EXIT.
           ADD 1 TO WS-FT-COUNT.
           SET WS-FT-IX TO WS-FT-COUNT.
           MOVE FC-OCN TO WS-FT-OCN (WS-FT-IX).
           MOVE FC-STATE-CD TO WS-FT-STATE (WS-FT-IX).
           MOVE FC-LATA TO WS-FT-LATA (WS-FT-IX).
           MOVE FC-EFF-YYDDD TO WS-FT-EFF (WS-FT-IX).
           MOVE FC-PIU TO WS-FT-PIU (WS-FT-IX).
           MOVE FC-PLU TO WS-FT-PLU (WS-FT-IX).
           MOVE FC-PSU TO WS-FT-PSU (WS-FT-IX).
           MOVE FC-SOURCE TO WS-FT-SOURCE (WS-FT-IX).

       P1310-EXIT.
           EXIT.


      *****************************************************************
      * S200-PIU-APPLICATION                                          *
      * SPLIT MINUTES AND MONEY BY PERCENT INTERSTATE USAGE.          *
      *****************************************************************
       S200-PIU-APPLICATION SECTION.

       P2000-PROCESS.
      * ONE USAGE RECORD PER PASS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ THRU P2100-EXIT.
           IF WS-EOF
               GO TO P2000-EXIT.
           MOVE CDI-RECORD TO CABS-CDR-RECORD.
           MOVE CD-KEY TO WS-RESTART-KEY.
           IF NOT CD-VOICE-MOU
               PERFORM P3600-PASS-THROUGH THRU P3600-EXIT
               GO TO P2000-EXIT.
           MOVE CD-VC-CHG-MIN TO WS-PW-BASE-MOU.
           IF WS-PW-BASE-MOU = ZERO
               PERFORM P3600-PASS-THROUGH THRU P3600-EXIT
               GO TO P2000-EXIT.
           PERFORM P2200-FIND-FACTOR THRU P2200-EXIT.
           PERFORM P2300-CHECK-ELEMENT THRU P2300-EXIT.
           IF NOT WS-SPLIT-DONE
               PERFORM P3600-PASS-THROUGH THRU P3600-EXIT
               GO TO P2000-EXIT.
           PERFORM P2400-GET-RATES THRU P2400-EXIT.
           PERFORM P3000-APPLY-PIU THRU P3000-EXIT.
           PERFORM P3200-RECONCILE THRU P3200-EXIT.
           PERFORM P3300-WRITE-SPLIT THRU P3300-EXIT.
           ADD 1 TO WS-WRITE-CNT.
           ADD 1 TO WS-APPLIED-CNT.

       P2000-EXIT.
           EXIT.

       P2100-READ.
      * SEQUENTIAL READ OF JURISDICTIONED USAGE.
           READ CDR-IN-FILE
               AT END
                   MOVE 'Y' TO WS-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00'
               MOVE 3440 TO WS-AB-CODE
               MOVE 'READ ERROR UT-S-CDRIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-READ-CNT.

       P2100-EXIT.
           EXIT.

       P2200-FIND-FACTOR.
      * FIND THE FACTOR FOR THIS OCN, STATE AND LATA.  THE MOST
      * RECENT EFFECTIVE DATE NOT AFTER THE CONNECT DATE WINS.  THE
      * TABLE IS WALKED IN FULL BECAUSE MORE THAN ONE ENTRY CAN
      * MATCH THE KEY WITH DIFFERENT EFFECTIVE DATES.
           MOVE 'P2200-FIND-FACTOR' TO WS-PARA-NAME.
           MOVE 'N' TO WS-FT-FOUND-SW.
           MOVE ZERO TO WS-PW-PIU.
           MOVE ZERO TO WS-BEST-EFF.
           MOVE 1 TO WS-SUB1.
           PERFORM P2250-SCAN-FACTOR THRU P2250-EXIT
               UNTIL WS-SUB1 > WS-FT-COUNT.
           IF WS-FT-FOUND
               GO TO P2200-EXIT.
           MOVE WS-PE-DEF-PIU TO WS-PW-PIU.
           ADD 1 TO WS-DEFAULT-CNT.
           MOVE EC-FACTOR-MISSING TO WS-ERR-CODE.
           MOVE 'W' TO WS-ERR-SEVERITY.

       P2200-EXIT.
           EXIT.

       P2250-SCAN-FACTOR.
      * ONE TABLE ENTRY.  THE EFFECTIVE DATE TEST IS A PLAIN YYDDD
      * COMPARE - IN THIS PROGRAM BOTH DATES ARE INSIDE THE SAME
      * BILLING YEAR SO THE COMPARE IS SAFE.
           IF WS-FT-OCN (WS-SUB1) = CD-OCN AND
              WS-FT-LATA (WS-SUB1) = CD-VC-ORIG-LATA
               IF WS-FT-EFF (WS-SUB1) NOT > CD-CONN-YYDDD
                   IF WS-FT-EFF (WS-SUB1) > WS-BEST-EFF
                       MOVE WS-FT-EFF (WS-SUB1) TO WS-BEST-EFF
                       MOVE WS-FT-PIU (WS-SUB1) TO WS-PW-PIU
                       MOVE 'Y' TO WS-FT-FOUND-SW.
           ADD 1 TO WS-SUB1.

       P2250-EXIT.
           EXIT.

       P2300-CHECK-ELEMENT.
      * NOT EVERY RATE ELEMENT IS SUBJECT TO PIU.  THE ELEMENT TABLE
      * CARRIES THE FLAG.  AN ELEMENT NOT ON THE TABLE IS TREATED AS
      * SUBJECT TO PIU - THAT DEFAULT WAS SET IN 1987 AND HAS NEVER
      * BEEN REVISITED.
           MOVE 'N' TO WS-SPLIT-SW.
           SET WS-RE-IX TO 1.
           SEARCH WS-RE-ENTRY
               AT END
                   MOVE 'Y' TO WS-SPLIT-SW
                   GO TO P2300-EXIT
               WHEN WS-RE-ELEM (WS-RE-IX) = CD-RATE-ELEM
                   IF WS-RE-PIU-APPL (WS-RE-IX) = 'Y'
                       MOVE 'Y' TO WS-SPLIT-SW
                   ELSE
                       MOVE 'N' TO WS-SPLIT-SW.

       P2300-EXIT.
           EXIT.

       P2400-GET-RATES.
      * THE INTERSTATE AND INTRASTATE RATES COME FROM TWO DIFFERENT
      * TARIFFS AND THEREFORE FROM TWO DIFFERENT LOAD MODULES.  THE
      * MODULE NAME IS ASSEMBLED FROM THE JURISDICTION CODE.
           MOVE 'P2400-GET-RATES' TO WS-PARA-NAME.
           MOVE ZERO TO WS-PW-IS-RATE.
           MOVE ZERO TO WS-PW-SS-RATE.
           MOVE 'IS' TO WS-RATE-JURIS.
           PERFORM P2450-CALL-RATE THRU P2450-EXIT.
           MOVE WS-RATE-RETURNED TO WS-PW-IS-RATE.
           MOVE 'SS' TO WS-RATE-JURIS.
           PERFORM P2450-CALL-RATE THRU P2450-EXIT.
           MOVE WS-RATE-RETURNED TO WS-PW-SS-RATE.

       P2400-EXIT.
           EXIT.

       P2450-CALL-RATE.
      * BUILD THE MODULE NAME AND CALL IT.
           MOVE SPACES TO WS-CP-SUFFIX.
           SET WS-CS-IX TO 1.
           SEARCH WS-CS-ENT
               AT END
                   MOVE 'DEF' TO WS-CP-SUFFIX
               WHEN WS-CS-KEY (WS-CS-IX) = WS-RATE-JURIS
                   MOVE WS-CS-SUF (WS-CS-IX) TO WS-CP-SUFFIX.
           MOVE ZERO TO WS-RATE-RETURNED.
           MOVE WS-PE-RATE-TARIFF TO WS-RATE-TARIFF.
           MOVE CD-RATE-ELEM TO WS-RATE-ELEMENT.
           ADD 1 TO WS-CALL-COUNT.
           CALL WS-CALL-PGM USING WS-RATE-PARM
                                  WS-CALL-RC.
           IF WS-CALL-RC NOT = ZERO
               MOVE EC-RATE-NOT-FOUND TO WS-ERR-CODE
               MOVE 'W' TO WS-ERR-SEVERITY
               MOVE ZERO TO WS-RATE-RETURNED.

       P2450-EXIT.
           EXIT.


      *****************************************************************
      * S300-SPLIT-AND-WRITE                                          *
      * THE ARITHMETIC AND THE TWO OUTPUT RECORDS.                    *
      *****************************************************************
       S300-SPLIT-AND-WRITE SECTION.

       P3000-APPLY-PIU.
      * THE PIU SPLIT.  MINUTES ARE ROUNDED TO TWO PLACES BECAUSE
      * THE USAGE RECORD CARRIES TWO.  MONEY IS ROUNDED AT FIVE
      * PLACES HERE.  CABJUR05 TRUNCATES THE SAME FIELD - THAT
      * DIVERGENCE FOLLOWS CABS-STD-041, WHICH SETS A DIFFERENT
      * RULE FOR THE SUMMARY SIDE.  DO NOT ALIGN THEM.
           MOVE 'P3000-APPLY-PIU' TO WS-PARA-NAME.
           COMPUTE WS-PW-IS-MOU ROUNDED =
                   WS-PW-BASE-MOU * WS-PW-PIU / 100.
           COMPUTE WS-PW-SS-MOU = WS-PW-BASE-MOU - WS-PW-IS-MOU.
           COMPUTE WS-PW-IS-AMT ROUNDED =
                   WS-PW-IS-MOU * WS-PW-IS-RATE.
           COMPUTE WS-PW-SS-AMT ROUNDED =
                   WS-PW-SS-MOU * WS-PW-SS-RATE.
           COMPUTE WS-PW-GROSS-AMT =
                   WS-PW-IS-AMT + WS-PW-SS-AMT.
           COMPUTE WS-PW-CHECK-AMT =
                   (WS-PW-BASE-MOU * WS-PW-PIU / 100) * WS-PW-IS-RATE
                 + (WS-PW-BASE-MOU - (WS-PW-BASE-MOU * WS-PW-PIU / 100))
                   * WS-PW-SS-RATE.
           COMPUTE WS-ROUND-RESIDUE =
                   WS-ROUND-RESIDUE + (WS-PW-GROSS-AMT - WS-PW-CHECK-AMT
           ADD WS-PW-IS-MOU TO WS-TOT-IS-MOU.
           ADD WS-PW-SS-MOU TO WS-TOT-SS-MOU.
           ADD WS-PW-IS-AMT TO WS-TOT-IS-AMT.
           ADD WS-PW-SS-AMT TO WS-TOT-SS-AMT.
           ADD WS-PW-GROSS-AMT TO WS-ACC-AMOUNT.
           ADD WS-PW-BASE-MOU TO WS-ACC-MINUTES.

       P3000-EXIT.
           EXIT.

       P3200-RECONCILE.
      * THE TWO HALVES MUST ADD BACK TO THE ORIGINAL MINUTES.  IF
      * THEY DO NOT, THE RECORD IS SUSPENDED - A MINUTE LOST HERE IS
      * A MINUTE THAT NEVER APPEARS ON ANY BILL.
           COMPUTE WS-PW-CHECK-MOU =
                   WS-PW-IS-MOU + WS-PW-SS-MOU.
           IF WS-PW-CHECK-MOU NOT = WS-PW-BASE-MOU
               MOVE EC-MIN-NEGATIVE TO WS-ERR-CODE
               MOVE 'E' TO WS-ERR-SEVERITY
               PERFORM P7000-SUSPEND THRU P7000-EXIT.

       P3200-EXIT.
           EXIT.

       P3300-WRITE-SPLIT.
      * TWO PHYSICAL RECORDS PER INPUT RECORD - ONE INTERSTATE, ONE
      * INTRASTATE.  THE BALANCE EQUATION COUNTS THE INPUT RECORD
      * ONCE, NOT THE TWO OUTPUTS.  SEE THE BALANCE NOTE ABOVE.
           MOVE CABS-CDR-RECORD TO WS-SPLIT-RECORD.
           MOVE 'I' TO CD-JURIS-CD.
           MOVE WS-PW-IS-MOU TO CD-VC-CHG-MIN.
           MOVE CABS-CDR-RECORD TO SPO-RECORD.
           WRITE SPO-RECORD.
           ADD 1 TO WS-PHYS-WRITE-CNT.
           MOVE WS-SPLIT-RECORD TO CABS-CDR-RECORD.
           MOVE 'S' TO CD-JURIS-CD.
           MOVE WS-PW-SS-MOU TO CD-VC-CHG-MIN.
           MOVE CABS-CDR-RECORD TO SPO-RECORD.
           WRITE SPO-RECORD.
           ADD 1 TO WS-PHYS-WRITE-CNT.

       P3300-EXIT.
           EXIT.

       P3600-PASS-THROUGH.
      * RECORDS NOT SUBJECT TO PIU ARE WRITTEN UNCHANGED SO THAT THE
      * DOWNSTREAM FILE IS COMPLETE.
           MOVE CABS-CDR-RECORD TO SPO-RECORD.
           WRITE SPO-RECORD.
           ADD 1 TO WS-PHYS-WRITE-CNT.
           ADD 1 TO WS-WRITE-CNT.

       P3600-EXIT.
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
           MOVE CABS-CDR-RECORD TO SU-ORIG-RECORD.
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
           MOVE 040                    TO CT-STEP-SEQ.
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
           DISPLAY 'FACTORS LOADED   ' WS-FT-COUNT.
           DISPLAY 'PIU APPLIED      ' WS-APPLIED-CNT.
           DISPLAY 'DEFAULT APPLIED  ' WS-DEFAULT-CNT.
           DISPLAY 'INTERSTATE MOU   ' WS-TOT-IS-MOU.
           DISPLAY 'INTRASTATE MOU   ' WS-TOT-SS-MOU.
           DISPLAY 'INTERSTATE AMT   ' WS-TOT-IS-AMT.
           DISPLAY 'INTRASTATE AMT   ' WS-TOT-SS-AMT.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE CDR-IN-FILE
                 FACTOR-FILE
                 SPLIT-OUT-FILE
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

