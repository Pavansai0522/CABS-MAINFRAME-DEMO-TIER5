      *****************************************************************
      * CABSET01 - MEET POINT BILLING SETTLEMENT CALCULATION          *
      * APPLICATION : SETL                                            *
      * INPUTS      : MPBVAL   TELCABS.SETL.MPB.VALID(0)      CABSCIRC*
      * INPUTS      : BILLDTL  TELCABS.CABS.BILLDTL(0)        CABSBILL*
      * INPUTS      : RATEMAST TELCABS.SETL.RATE              CABSRATE*
      * INPUTS      : CARRMAST TELCABS.SETL.CARRIER           CABSCARR*
      * INPUTS      : SYSIN    INSTREAM                       NONE    *
      * OUTPUTS     : SETLOUT  TELCABS.SETL.SETTLE.MPB(+1)    CABSSETL*
      * OUTPUTS     : VAROUT   TELCABS.SETL.MPB.VARIANCE(+1)  NONE    *
      * OUTPUTS     : REPORT   SYSOUT                         CABSPRNT*
      * CONTROL     : CTLOUT                         CABSCTL          *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARIS*
      *                       + CT-CARRIED-FWD                        *
      *               SUMMARISED = CIRCUITS WITH NO BILLED REVENUE    *
      *               CARRIED FWD = CIRCUITS HELD FOR THE NEXT PERIOD *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * THE MEET POINT CALCULATOR.  JOINTLY PROVIDED ACCESS           *
      * IS SPLIT BETWEEN TWO LECS BY BILLING PERCENTAGE.              *
      * STANDARDS   : CODED TO CABS-STD-009 (PARAGRAPH STRUCTURE) AND *
      *               CABS-STD-022 (CONTROL CARDS). LAST STANDARDS    *
      *               REVIEW 2016. NO WAIVERS ON FILE. OPERATIONS     *
      *               SUPPORT HOLDS THE CURRENT RUN SHEET.            *
      * REVISION HISTORY                                              *
      *   V1.00  1987-10-19  R.T.WHEELER   INITIAL - MPB SPLIT        *
      *   V1.03  1989-04-11  R.T.WHEELER   BILLED DETAIL MATCH        *
      *   V1.06  1991-09-23  D.OKONKWO     TRUNK DEFAULT TABLE        *
      *   V1.09  1993-06-30  D.OKONKWO     VARIANCE FILE ADDED        *
      *   V2.00  1997-03-18  J.M.CASTILLO  Y2K REVIEW - NO IMPACT     *
      *   V2.02  2000-08-08  P.NAIR        RESIDUAL HANDLING CHANGED  *
      *   V2.04  2004-12-06  P.NAIR        BAND RATES HONOURED        *
      *   V2.06  2009-05-19  A.BUKOWSKI    MINIMUM CHARGE APPLIED     *
      *   V2.08  2013-11-27  L.FERREIRA    REGIONAL SPLIT MODULES     *
      *   V2.09  2017-07-14  L.FERREIRA    RECOMPILE ONLY LE V6       *
      *   V2.09  2019-09-25  M.OYELARAN    NO CODE CHANGE - AUDIT     *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CABSET01.
       AUTHOR.        R.T.WHEELER.
       DATE-WRITTEN.  1987-10-19.
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
      * VALIDATED MEET POINT CIRCUITS FROM CABSET03
           SELECT MPB-VALID-FILE
               ASSIGN TO UT-S-MPBVAL
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
      * OWNED BY THE CABS APPLICATION - CROSS APP READ
           SELECT BILL-DETAIL-FILE
               ASSIGN TO UT-S-BILLDTL
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-TABLE.
      * SETTLEMENT RATE MASTER - JOINTLY PROVIDED RATES
           SELECT RATE-MASTER
               ASSIGN TO DA-I-RATEMAST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS RTM-KEY
               FILE STATUS IS WS-FS-SUSPENSE.
      * CARRIER MASTER - MEET POINT ELIGIBILITY
           SELECT CARRIER-MASTER
               ASSIGN TO DA-I-CARRMAST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CRM-KEY
               FILE STATUS IS WS-FS-OUTPUT.
      * MEET POINT SETTLEMENT RECORDS - CABSSETL LAYOUT
           SELECT SETTLE-OUT-FILE
               ASSIGN TO UT-S-SETLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CONTROL.
      * PERCENTAGE VARIANCE DETAIL FOR THE ACCESS MGMT GROUP
           SELECT VARIANCE-FILE
               ASSIGN TO UT-S-VAROUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
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
       FD  MPB-VALID-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 200 CHARACTERS
               DATA RECORD IS MPI-RECORD.
       01  MPI-RECORD              PIC X(200).

       FD  BILL-DETAIL-FILE
               RECORDING MODE IS V
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 1647 CHARACTERS
               DATA RECORD IS BDI-RECORD.
       01  BDI-RECORD              PIC X(1647).

       FD  RATE-MASTER
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 400 CHARACTERS
               DATA RECORD IS RTM-RECORD.
       01  RTM-RECORD.
           05  RTM-KEY                 PIC X(18).
           05  RTM-DATA                PIC X(382).

       FD  CARRIER-MASTER
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 120 CHARACTERS
               DATA RECORD IS CRM-RECORD.
       01  CRM-RECORD.
           05  CRM-KEY                 PIC X(04).
           05  CRM-DATA                PIC X(116).

       FD  SETTLE-OUT-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 180 CHARACTERS
               DATA RECORD IS STO-RECORD.
       01  STO-RECORD              PIC X(180).

       FD  VARIANCE-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 200 CHARACTERS
               DATA RECORD IS VAR-RECORD.
       01  VAR-RECORD              PIC X(200).

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
           05  WS-PGM-NAME         PIC X(08)         VALUE 'CABSET01'.
           05  WS-PGM-VERSION        PIC X(05)           VALUE 'V2.09'.
           05  WS-PGM-APPL           PIC X(04)           VALUE 'SETL'.
           05  WS-PGM-COMPILE      PIC X(08)         VALUE '20190925'.
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

       COPY CABSCIRC.

       COPY CABSBILL.

       COPY CABSSETL.

       COPY CABSRATE.

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
           05  WS-PE-SETTLE-PERIOD     PIC 9(06).
           05  WS-PE-TARIFF-CD         PIC X(04).
           05  WS-PE-MIN-AMOUNT        PIC 9(05)V9(02).
           05  WS-PE-REGION            PIC X(01).
           05  WS-PE-SIM-SW            PIC X(01).
           05  WS-PE-FILLER            PIC X(16).
       01  WS-PARM-OLD REDEFINES WS-PARM-CARD.
      * THE 1989 CARD.  TWO REGIONS NEVER CONVERTED THEIR JCL AND
      * THE EDIT STILL ACCEPTS IT.  THE FIELDS ARE IN A DIFFERENT
      * ORDER AND THE CYCLE DATE IS SIX DIGITS YYMMDD, NOT YYDDD.
           05  FILLER                  PIC X(02).
           05  WS-PO-CYCLE             PIC 9(05).
           05  WS-PO-RUN-ID            PIC X(12).
           05  WS-PO-YYMMDD            PIC 9(06).
           05  WS-PO-PERIOD            PIC 9(06).
           05  WS-PO-TARIFF            PIC X(04).
           05  FILLER                  PIC X(20).

      * LOCAL SWITCHES.  THE STANDARD ONES ARRIVE VIA CABSWRK.
       01  WS-LOCAL-SWITCHES.
           05  WS-PARM-EOF-SW          PIC X(01)             VALUE 'N'.
                   88  WS-PARM-EOF             VALUE 'Y'.
           05  WS-BILL-EOF-SW          PIC X(01)             VALUE 'N'.
                   88  WS-BILL-EOF             VALUE 'Y'.
           05  WS-BILL-MATCH-SW        PIC X(01)             VALUE 'N'.
                   88  WS-BILL-MATCHED         VALUE 'Y'.
           05  WS-RATE-FOUND-SW        PIC X(01)             VALUE 'N'.
                   88  WS-RATE-FOUND           VALUE 'Y'.
           05  WS-CARR-FOUND-SW        PIC X(01)             VALUE 'N'.
                   88  WS-CARR-FOUND           VALUE 'Y'.
           05  WS-PCT-OK-SW            PIC X(01)             VALUE 'Y'.
                   88  WS-PCT-OK               VALUE 'Y'.
                   88  WS-PCT-BAD              VALUE 'N'.
           05  WS-DEFAULT-SW           PIC X(01)             VALUE 'N'.
                   88  WS-DEFAULT-USED         VALUE 'Y'.
           05  WS-BAND-SW              PIC X(01)             VALUE 'N'.
                   88  WS-BAND-APPLIED         VALUE 'Y'.

      * SUBSCRIPTS AND INDEX WORK FIELDS.
       01  WS-SUBSCRIPTS.
           05  WS-SUB1                 PIC S9(05) COMP-3     VALUE 0.
           05  WS-SUB2                 PIC S9(05) COMP-3     VALUE 0.
           05  WS-SUB3                 PIC S9(05) COMP-3     VALUE 0.
           05  WS-ELEM-SUB             PIC S9(05) COMP-3     VALUE 0.
           05  WS-BAND-SUB             PIC S9(05) COMP-3     VALUE 0.
           05  WS-IDX                  PIC S9(05) COMP-3     VALUE 0.

      * THE EXTRACT RECORD AS BUILT BY CABSET02 AND VALIDATED BY
      * CABSET03.  FOURTH DECLARATION OF THE SAME LAYOUT.  THERE
      * IS STILL NO COPYBOOK.
       01  WS-EXTRACT-RECORD.
           05  WS-XR-CIRCUIT-ID      PIC X(20)           VALUE SPACES.
           05  WS-XR-TRUNK-GRP       PIC X(08)           VALUE SPACES.
           05  WS-XR-OCN             PIC X(04)           VALUE SPACES.
           05  WS-XR-BAN             PIC X(13)           VALUE SPACES.
           05  WS-XR-OUR-PCT           PIC S9(03)V9(05)      VALUE 0.
           05  WS-XR-OTHER-OCN       PIC X(04)           VALUE SPACES.
           05  WS-XR-OTHER-PCT         PIC S9(03)V9(05)      VALUE 0.
           05  WS-XR-STATE           PIC X(02)           VALUE SPACES.
           05  WS-XR-MOU               PIC S9(15)V9(02)      VALUE 0.
           05  WS-XR-AMOUNT            PIC S9(13)V9(05)      VALUE 0.
           05  WS-XR-PERIOD            PIC 9(06)             VALUE 0.
           05  WS-XR-FILLER          PIC X(80)           VALUE SPACES.
       01  WS-EXTRACT-KEY-R REDEFINES WS-EXTRACT-RECORD.
           05  WS-XK-KEY               PIC X(28).
           05  WS-XK-BODY              PIC X(172).
       01  WS-EXTRACT-PCT-R REDEFINES WS-EXTRACT-RECORD.
           05  WS-XP-HEAD              PIC X(45).
           05  WS-XP-OUR               PIC S9(03)V9(05).
           05  WS-XP-OTHER-OCN         PIC X(04).
           05  WS-XP-OTHER             PIC S9(03)V9(05).
           05  WS-XP-TAIL              PIC X(135).

      * THE MEET POINT ARITHMETIC.  JOINTLY PROVIDED ACCESS IS
      * BILLED IN FULL BY ONE LEC AND THE REVENUE IS SPLIT:
      *   OUR SHARE   = GROSS * OUR PERCENTAGE   / 100
      *   THEIR SHARE = GROSS * THEIR PERCENTAGE / 100
      * THE TWO PERCENTAGES ARE FILED SEPARATELY BY THE TWO LECS
      * AND ARE SUPPOSED TO SUM TO EXACTLY ONE HUNDRED.  WHEN
      * THEY DO NOT THE DIFFERENCE IS A RESIDUAL AND SOMEBODY
      * HAS TO OWN IT.  SEE P3200.
       01  WS-MPB-WORK.
           05  WS-MW-GROSS             PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-MW-OUR-PCT           PIC S9(03)V9(05) COMP-3 VALUE 0.
           05  WS-MW-THEIR-PCT         PIC S9(03)V9(05) COMP-3 VALUE 0.
           05  WS-MW-TOTAL-PCT         PIC S9(05)V9(05) COMP-3 VALUE 0.
           05  WS-MW-RESIDUAL-PCT      PIC S9(05)V9(05) COMP-3 VALUE 0.
           05  WS-MW-OUR-SHARE         PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-MW-THEIR-SHARE       PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-MW-EXACT-SHARE       PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-MW-CHECK             PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-MW-MOU               PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-MW-RATE              PIC S9(05)V9(05) COMP-3 VALUE 0.
           05  WS-MW-QTY               PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-MW-MIN-CHG           PIC S9(07)V9(02) COMP-3 VALUE 0.
           05  WS-MW-BAND-RATE         PIC S9(05)V9(05) COMP-3 VALUE 0.
           05  WS-MW-NET-DUE           PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-MW-TRUNC-DIFF        PIC S9(05)V9(05) COMP-3 VALUE 0.

      * SETTLEMENT RUN TOTALS.  OUR SHARE PLUS THEIR SHARE
      * SHOULD EQUAL GROSS.  WHERE IT DOES NOT, THE DIFFERENCE
      * IS IN THE RESIDUAL AND THE TRUNCATION TOTALS.
       01  WS-MPB-TOTALS.
           05  WS-SETTLE-CNT           PIC S9(11) COMP-3     VALUE 0.
           05  WS-NOREV-CNT            PIC S9(11) COMP-3     VALUE 0.
           05  WS-VAR-CNT              PIC S9(11) COMP-3     VALUE 0.
           05  WS-RESID-CNT            PIC S9(11) COMP-3     VALUE 0.
           05  WS-DEFAULT-CNT          PIC S9(11) COMP-3     VALUE 0.
           05  WS-MIN-CNT              PIC S9(11) COMP-3     VALUE 0.
           05  WS-BAND-CNT             PIC S9(11) COMP-3     VALUE 0.
           05  WS-TOT-GROSS            PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-TOT-OURS             PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-TOT-THEIRS           PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-TOT-RESIDUAL         PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-TOT-TRUNC            PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-TOT-MOU              PIC S9(15)V9(02) COMP-3 VALUE 0.

      * VARIANCE RECORD.  WRITTEN WHENEVER THE TWO PERCENTAGES DO
      * NOT SUM TO ONE HUNDRED.  THE ACCESS MANAGEMENT GROUP IS
      * SUPPOSED TO WORK THIS FILE EVERY MONTH.
       01  WS-VARIANCE-RECORD.
           05  WS-VR-CIRCUIT-ID      PIC X(20)           VALUE SPACES.
           05  WS-VR-OCN             PIC X(04)           VALUE SPACES.
           05  WS-VR-OTHER-OCN       PIC X(04)           VALUE SPACES.
           05  WS-VR-PERIOD            PIC 9(06)             VALUE 0.
           05  WS-VR-OUR-PCT           PIC S9(03)V9(05)      VALUE 0.
           05  WS-VR-THEIR-PCT         PIC S9(03)V9(05)      VALUE 0.
           05  WS-VR-TOTAL-PCT         PIC S9(05)V9(05)      VALUE 0.
           05  WS-VR-RESIDUAL          PIC S9(05)V9(05)      VALUE 0.
           05  WS-VR-GROSS             PIC S9(13)V9(05)      VALUE 0.
           05  WS-VR-IMPACT            PIC S9(13)V9(05)      VALUE 0.
           05  WS-VR-DISPOSITION     PIC X(30)           VALUE SPACES.
           05  WS-VR-FILLER          PIC X(60)           VALUE SPACES.
       01  WS-VARIANCE-KEY-R REDEFINES WS-VARIANCE-RECORD.
           05  WS-VK-KEY               PIC X(34).
           05  WS-VK-REST              PIC X(166).

      * REGIONAL SPLIT MODULES.  THE 2013 CHANGE MOVED THE
      * REGION SPECIFIC SPLIT RULES OUT INTO FIVE LOAD MODULES
      * BECAUSE THREE STATE COMMISSIONS ORDERED DIFFERENT
      * TREATMENTS OF THE RESIDUAL.  THE MODULE NAME IS BUILT
      * FROM THE REGION AT RUN TIME AND WHICH COPY RUNS DEPENDS
      * ON THE STEPLIB CONCATENATION.
       01  WS-CALL-AREA.
           05  WS-CALL-PGM.
               10  WS-CP-STEM          PIC X(05)         VALUE 'CABMP'.
               10  WS-CP-SUFFIX        PIC X(03)         VALUE SPACES.
           05  WS-CALL-RC              PIC S9(04) COMP       VALUE 0.
           05  WS-CALL-COUNT           PIC S9(07) COMP-3     VALUE 0.
           05  WS-CALL-SUFFIX-TAB.
               10  FILLER              PIC X(05)         VALUE 'ABSA'.
               10  FILLER              PIC X(05)         VALUE 'BBSB'.
               10  FILLER              PIC X(05)         VALUE 'CBSC'.
               10  FILLER              PIC X(05)         VALUE 'DBSD'.
               10  FILLER              PIC X(05)         VALUE 'EBSE'.
           05  WS-CALL-SUFFIX-R REDEFINES WS-CALL-SUFFIX-TAB.
               10  WS-CS-ENT OCCURS 5 TIMES
                   INDEXED BY WS-CS-IX.
                   15  WS-CS-KEY               PIC X(02).
                   15  WS-CS-SUF               PIC X(03).

      * USOC TO RATE ELEMENT CROSS REFERENCE.  THE CIRCUIT INVENTORY
      * CARRIES A USOC; THE RATE MASTER IS KEYED BY RATE ELEMENT.
      * THIS TABLE IS THE BRIDGE.  IT WAS BUILT BY HAND IN 1987 AND
      * HAS BEEN MAINTAINED BY HAND EVER SINCE - THERE IS NO FEED
      * FROM THE SERVICE ORDER SYSTEM AND NO RECONCILIATION.
       01  WS-USOCXR-CONST.
           05  FILLER            PIC X(12)       VALUE 'AA871RECIPTN'.
           05  FILLER            PIC X(12)       VALUE 'AD135DS3LOCN'.
           05  FILLER            PIC X(12)       VALUE 'AD677SHRTRNN'.
           05  FILLER            PIC X(12)       VALUE 'AE021SPCLACY'.
           05  FILLER            PIC X(12)       VALUE 'AF562LOCTRMY'.
           05  FILLER            PIC X(12)       VALUE 'AF668DTTRANN'.
           05  FILLER            PIC X(12)       VALUE 'AW021LNKCHGN'.
           05  FILLER            PIC X(12)       VALUE 'BR927DS3LOCY'.
           05  FILLER            PIC X(12)       VALUE 'BX850ORIGINY'.
           05  FILLER            PIC X(12)       VALUE 'CC674MINCHGY'.
           05  FILLER            PIC X(12)       VALUE 'CE873CARCOMY'.
           05  FILLER            PIC X(12)       VALUE 'CG715CCLORGY'.
           05  FILLER            PIC X(12)       VALUE 'CN390LSWTCHN'.
           05  FILLER            PIC X(12)       VALUE 'DA797MPBTRNY'.
           05  FILLER            PIC X(12)       VALUE 'DJ622COMTRNN'.
           05  FILLER            PIC X(12)       VALUE 'DP919UNELOPY'.
           05  FILLER            PIC X(12)       VALUE 'EF257TSWTCHY'.
           05  FILLER            PIC X(12)       VALUE 'EH825LOCORGN'.
           05  FILLER            PIC X(12)       VALUE 'FG211COLLOCY'.
           05  FILLER            PIC X(12)       VALUE 'FH528DS1LOCY'.
           05  FILLER            PIC X(12)       VALUE 'FS266QUERY1Y'.
           05  FILLER            PIC X(12)       VALUE 'FW148MOUCHGY'.
           05  FILLER            PIC X(12)       VALUE 'GE001MINCHGN'.
           05  FILLER            PIC X(12)       VALUE 'GJ824ORIGINN'.
           05  FILLER            PIC X(12)       VALUE 'GM024LTRANSN'.
           05  FILLER            PIC X(12)       VALUE 'GS750LOCORGY'.
           05  FILLER            PIC X(12)       VALUE 'GV682DEDTRNY'.
           05  FILLER            PIC X(12)       VALUE 'HK107MPBTRNY'.
           05  FILLER            PIC X(12)       VALUE 'HL187LNKCHGY'.
           05  FILLER            PIC X(12)       VALUE 'JG670PORTCHY'.
           05  FILLER            PIC X(12)       VALUE 'JP805LSWTCHY'.
           05  FILLER            PIC X(12)       VALUE 'JV834XCONNCY'.
           05  FILLER            PIC X(12)       VALUE 'JX326SETUPCY'.
           05  FILLER            PIC X(12)       VALUE 'KA661ENTFACN'.
           05  FILLER            PIC X(12)       VALUE 'KZ932TERMINY'.
           05  FILLER            PIC X(12)       VALUE 'LU043MOUCHGY'.
           05  FILLER            PIC X(12)       VALUE 'LU105SPCLACY'.
           05  FILLER            PIC X(12)       VALUE 'MM720CARCOMY'.
           05  FILLER            PIC X(12)       VALUE 'MP518TANDEMY'.
           05  FILLER            PIC X(12)       VALUE 'MX388TNDMSWN'.
           05  FILLER            PIC X(12)       VALUE 'NA029TRANSPY'.
           05  FILLER            PIC X(12)       VALUE 'NE186UNEPRTY'.
           05  FILLER            PIC X(12)       VALUE 'NM215ENDOFFY'.
           05  FILLER            PIC X(12)       VALUE 'NV700SETUPCN'.
           05  FILLER            PIC X(12)       VALUE 'NX408TNDMSWY'.
           05  FILLER            PIC X(12)       VALUE 'PB926ENTRANY'.
           05  FILLER            PIC X(12)       VALUE 'PC940DS1LOCN'.
           05  FILLER            PIC X(12)       VALUE 'PH636800DBQN'.
           05  FILLER            PIC X(12)       VALUE 'PN264UNELOPN'.
           05  FILLER            PIC X(12)       VALUE 'PT864QUERY1N'.
           05  FILLER            PIC X(12)       VALUE 'RW598DEDTRNN'.
           05  FILLER            PIC X(12)       VALUE 'RX285UNEPRTY'.
           05  FILLER            PIC X(12)       VALUE 'TH209DBQCHGN'.
           05  FILLER            PIC X(12)       VALUE 'UJ196ISPBNDY'.
           05  FILLER            PIC X(12)       VALUE 'VC171ISPBNDN'.
           05  FILLER            PIC X(12)       VALUE 'VN411RECIPTY'.
           05  FILLER            PIC X(12)       VALUE 'VN762LTRANSY'.
           05  FILLER            PIC X(12)       VALUE 'VT730SS7ISPY'.
           05  FILLER            PIC X(12)       VALUE 'WE056TSWTCHY'.
           05  FILLER            PIC X(12)       VALUE 'WF149SS7ISPY'.
           05  FILLER            PIC X(12)       VALUE 'WP638CCLTRMY'.
           05  FILLER            PIC X(12)       VALUE 'WZ210WIRTRMY'.
           05  FILLER            PIC X(12)       VALUE 'WZ353CCLORGN'.
           05  FILLER            PIC X(12)       VALUE 'XS996COLLOCN'.
           05  FILLER            PIC X(12)       VALUE 'XZ820DTTRANY'.
           05  FILLER            PIC X(12)       VALUE 'YD646LOCTRMY'.
           05  FILLER            PIC X(12)       VALUE 'YJ283ENTRANY'.
           05  FILLER            PIC X(12)       VALUE 'YJ860TERMINN'.
           05  FILLER            PIC X(12)       VALUE 'YL906800DBQY'.
           05  FILLER            PIC X(12)       VALUE 'ZL799COMTRNY'.
           05  FILLER            PIC X(12)       VALUE 'ZR519TRANSPY'.
           05  FILLER            PIC X(12)       VALUE 'ZZ215CCLTRMY'.
       01  WS-USOCXR-TABLE REDEFINES WS-USOCXR-CONST.
           05  WS-WS-UX-ENTRY OCCURS 72 TIMES
                   INDEXED BY WS-UX-IX.
               10  WS-UX-USOC              PIC X(05).
               10  WS-UX-ELEM              PIC X(06).
               10  WS-UX-MPB-ELIG          PIC X(01).

      * TRUNK GROUP DEFAULT MEET POINT PERCENTAGE TABLE.  USED WHEN
      * THE CIRCUIT RECORD CARRIES NO PERCENTAGE AT ALL.  THE VALUES
      * ARE THE 1990 NEGOTIATED SPLITS AND SEVERAL OF THE CARRIERS
      * ON IT NO LONGER EXIST.  A TRUNK GROUP THAT PICKS UP A
      * DEFAULT FROM HERE IS SETTLED ON A THIRTY YEAR OLD NUMBER.
       01  WS-TRKDFL-CONST.
           05  FILLER  PIC X(21)  VALUE 'TG0009872505SD6666700'.
           05  FILLER  PIC X(21)  VALUE 'TG0067293281GA3333300'.
           05  FILLER  PIC X(21)  VALUE 'TG0070366299RI2500000'.
           05  FILLER  PIC X(21)  VALUE 'TG0096846450VT3333300'.
           05  FILLER  PIC X(21)  VALUE 'TG0100851576CA6666700'.
           05  FILLER  PIC X(21)  VALUE 'TG0119361219SC7500000'.
           05  FILLER  PIC X(21)  VALUE 'TG0147643211WA3333300'.
           05  FILLER  PIC X(21)  VALUE 'TG0161756933MI3333300'.
           05  FILLER  PIC X(21)  VALUE 'TG0177164034AL6666700'.
           05  FILLER  PIC X(21)  VALUE 'TG0392875263DE2500000'.
           05  FILLER  PIC X(21)  VALUE 'TG0479678462MA3333300'.
           05  FILLER  PIC X(21)  VALUE 'TG0536418039NE6666700'.
           05  FILLER  PIC X(21)  VALUE 'TG0610238191ME2500000'.
           05  FILLER  PIC X(21)  VALUE 'TG0764005763KS5000000'.
           05  FILLER  PIC X(21)  VALUE 'TG0881383136OK2500000'.
           05  FILLER  PIC X(21)  VALUE 'TG0985879514FL5000000'.
           05  FILLER  PIC X(21)  VALUE 'TG1000467488NM5000000'.
           05  FILLER  PIC X(21)  VALUE 'TG1149979119TN6666700'.
           05  FILLER  PIC X(21)  VALUE 'TG1153366148IN3333300'.
           05  FILLER  PIC X(21)  VALUE 'TG1182336455AZ5000000'.
           05  FILLER  PIC X(21)  VALUE 'TG1256898190AL3333300'.
           05  FILLER  PIC X(21)  VALUE 'TG1291793460NC7500000'.
           05  FILLER  PIC X(21)  VALUE 'TG1352828171MA5000000'.
           05  FILLER  PIC X(21)  VALUE 'TG1399757945WY3333300'.
           05  FILLER  PIC X(21)  VALUE 'TG1576471926FL3333300'.
           05  FILLER  PIC X(21)  VALUE 'TG1628221986ME2500000'.
           05  FILLER  PIC X(21)  VALUE 'TG1733039274AL3333300'.
           05  FILLER  PIC X(21)  VALUE 'TG1854586202CA3333300'.
           05  FILLER  PIC X(21)  VALUE 'TG1912482573AR7500000'.
           05  FILLER  PIC X(21)  VALUE 'TG2376636773CT7500000'.
           05  FILLER  PIC X(21)  VALUE 'TG2392988829KY6666700'.
           05  FILLER  PIC X(21)  VALUE 'TG2473234084OR2500000'.
           05  FILLER  PIC X(21)  VALUE 'TG2536303657NM6666700'.
           05  FILLER  PIC X(21)  VALUE 'TG2606537582MI5000000'.
           05  FILLER  PIC X(21)  VALUE 'TG2608382652TX6666700'.
           05  FILLER  PIC X(21)  VALUE 'TG2787998865GA7500000'.
           05  FILLER  PIC X(21)  VALUE 'TG2810696613TN7500000'.
           05  FILLER  PIC X(21)  VALUE 'TG2860622595AK3333300'.
           05  FILLER  PIC X(21)  VALUE 'TG2884303311IA6666700'.
           05  FILLER  PIC X(21)  VALUE 'TG2908879651IN5000000'.
           05  FILLER  PIC X(21)  VALUE 'TG2996287646HI3333300'.
           05  FILLER  PIC X(21)  VALUE 'TG3067086079OH6666700'.
           05  FILLER  PIC X(21)  VALUE 'TG3123105268AR3333300'.
           05  FILLER  PIC X(21)  VALUE 'TG3134978642KY7500000'.
           05  FILLER  PIC X(21)  VALUE 'TG3278173261WV7500000'.
           05  FILLER  PIC X(21)  VALUE 'TG3306877391LA7500000'.
           05  FILLER  PIC X(21)  VALUE 'TG3322775073IA6666700'.
           05  FILLER  PIC X(21)  VALUE 'TG3399255460MS2500000'.
           05  FILLER  PIC X(21)  VALUE 'TG3408531140WI2500000'.
           05  FILLER  PIC X(21)  VALUE 'TG3409191932KY7500000'.
           05  FILLER  PIC X(21)  VALUE 'TG3609649000VA2500000'.
           05  FILLER  PIC X(21)  VALUE 'TG3783155067MT7500000'.
           05  FILLER  PIC X(21)  VALUE 'TG3813008388DC6666700'.
           05  FILLER  PIC X(21)  VALUE 'TG3888267891OH3333300'.
           05  FILLER  PIC X(21)  VALUE 'TG4014241861RI5000000'.
           05  FILLER  PIC X(21)  VALUE 'TG4314748881NV3333300'.
           05  FILLER  PIC X(21)  VALUE 'TG4367679050LA3333300'.
           05  FILLER  PIC X(21)  VALUE 'TG4450257556OR6666700'.
           05  FILLER  PIC X(21)  VALUE 'TG4515715331IN7500000'.
           05  FILLER  PIC X(21)  VALUE 'TG4633922226PA2500000'.
           05  FILLER  PIC X(21)  VALUE 'TG4655515887LA5000000'.
           05  FILLER  PIC X(21)  VALUE 'TG4692705520IL2500000'.
           05  FILLER  PIC X(21)  VALUE 'TG4833253225CT2500000'.
           05  FILLER  PIC X(21)  VALUE 'TG4938604729CO2500000'.
           05  FILLER  PIC X(21)  VALUE 'TG5046417262AZ2500000'.
           05  FILLER  PIC X(21)  VALUE 'TG5074648108NJ5000000'.
           05  FILLER  PIC X(21)  VALUE 'TG5109242359KS2500000'.
           05  FILLER  PIC X(21)  VALUE 'TG5267039637AR3333300'.
           05  FILLER  PIC X(21)  VALUE 'TG5333405770MN3333300'.
           05  FILLER  PIC X(21)  VALUE 'TG5545948608ND2500000'.
           05  FILLER  PIC X(21)  VALUE 'TG5664941109NC5000000'.
           05  FILLER  PIC X(21)  VALUE 'TG5686129396CO7500000'.
           05  FILLER  PIC X(21)  VALUE 'TG5888845714NH6666700'.
           05  FILLER  PIC X(21)  VALUE 'TG6101925202MO5000000'.
           05  FILLER  PIC X(21)  VALUE 'TG6170432460HI6666700'.
           05  FILLER  PIC X(21)  VALUE 'TG6455673150IL6666700'.
           05  FILLER  PIC X(21)  VALUE 'TG6465183786HI5000000'.
           05  FILLER  PIC X(21)  VALUE 'TG6512927932DE5000000'.
           05  FILLER  PIC X(21)  VALUE 'TG6603836777VA7500000'.
           05  FILLER  PIC X(21)  VALUE 'TG6616277173DC6666700'.
           05  FILLER  PIC X(21)  VALUE 'TG6703283670ID5000000'.
           05  FILLER  PIC X(21)  VALUE 'TG6744439793CT5000000'.
           05  FILLER  PIC X(21)  VALUE 'TG6784571975MN7500000'.
           05  FILLER  PIC X(21)  VALUE 'TG6893855325WI2500000'.
           05  FILLER  PIC X(21)  VALUE 'TG7009387070SC6666700'.
           05  FILLER  PIC X(21)  VALUE 'TG7040348409CA3333300'.
           05  FILLER  PIC X(21)  VALUE 'TG7044072863NY6666700'.
           05  FILLER  PIC X(21)  VALUE 'TG7118358494IL3333300'.
           05  FILLER  PIC X(21)  VALUE 'TG7233163401NH2500000'.
           05  FILLER  PIC X(21)  VALUE 'TG7237268709OK6666700'.
           05  FILLER  PIC X(21)  VALUE 'TG7507666204MO5000000'.
           05  FILLER  PIC X(21)  VALUE 'TG7610255633KS3333300'.
           05  FILLER  PIC X(21)  VALUE 'TG7668724556WV2500000'.
           05  FILLER  PIC X(21)  VALUE 'TG7790376535GA7500000'.
           05  FILLER  PIC X(21)  VALUE 'TG7791849834WY7500000'.
           05  FILLER  PIC X(21)  VALUE 'TG7872869287ID2500000'.
           05  FILLER  PIC X(21)  VALUE 'TG8500517494MD7500000'.
           05  FILLER  PIC X(21)  VALUE 'TG8542509960PA2500000'.
           05  FILLER  PIC X(21)  VALUE 'TG8654419707VT2500000'.
           05  FILLER  PIC X(21)  VALUE 'TG8742967681AK6666700'.
           05  FILLER  PIC X(21)  VALUE 'TG8754648383NJ6666700'.
           05  FILLER  PIC X(21)  VALUE 'TG8762344177AK7500000'.
           05  FILLER  PIC X(21)  VALUE 'TG8894705879NV7500000'.
           05  FILLER  PIC X(21)  VALUE 'TG9008682490IA3333300'.
           05  FILLER  PIC X(21)  VALUE 'TG9019916103WA5000000'.
           05  FILLER  PIC X(21)  VALUE 'TG9135558476DE2500000'.
           05  FILLER  PIC X(21)  VALUE 'TG9227141083NY5000000'.
           05  FILLER  PIC X(21)  VALUE 'TG9267756579ID2500000'.
           05  FILLER  PIC X(21)  VALUE 'TG9278507762MS7500000'.
           05  FILLER  PIC X(21)  VALUE 'TG9305193286ND7500000'.
           05  FILLER  PIC X(21)  VALUE 'TG9380786362MD7500000'.
           05  FILLER  PIC X(21)  VALUE 'TG9418906876UT2500000'.
           05  FILLER  PIC X(21)  VALUE 'TG9458521865AZ5000000'.
           05  FILLER  PIC X(21)  VALUE 'TG9473026931NE2500000'.
           05  FILLER  PIC X(21)  VALUE 'TG9513163393SD5000000'.
           05  FILLER  PIC X(21)  VALUE 'TG9646169854MT6666700'.
           05  FILLER  PIC X(21)  VALUE 'TG9673039461CO6666700'.
           05  FILLER  PIC X(21)  VALUE 'TG9691518619TX5000000'.
           05  FILLER  PIC X(21)  VALUE 'TG9782818611UT6666700'.
           05  FILLER  PIC X(21)  VALUE 'TG9884323139FL7500000'.
       01  WS-TRKDFL-TABLE REDEFINES WS-TRKDFL-CONST.
           05  WS-WS-TK-ENTRY OCCURS 120 TIMES
                   INDEXED BY WS-TK-IX.
               10  WS-TK-TRUNK             PIC X(08).
               10  WS-TK-OCN               PIC X(04).
               10  WS-TK-STATE             PIC X(02).
               10  WS-TK-PCT               PIC 9(02)V9(05).

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

      * LATA TO STATE CROSS REFERENCE.  LOADED AS A LITERAL TABLE IN
      * 1987 BECAUSE THE LATA MASTER FILE DID NOT EXIST YET.  IT DOES
      * NOW (TELCABS.CABS.LATAMAST) BUT NOTHING READS IT.  ENTRIES ARE
      * IN ASCENDING LATA SEQUENCE SO SEARCH ALL CAN BE USED.
       01  WS-LATA-CONST.
           05  FILLER                PIC X(06)           VALUE '159DEP'.
           05  FILLER                PIC X(06)           VALUE '174NVP'.
           05  FILLER                PIC X(06)           VALUE '184WVP'.
           05  FILLER                PIC X(06)           VALUE '194AZM'.
           05  FILLER                PIC X(06)           VALUE '205IAM'.
           05  FILLER                PIC X(06)           VALUE '212MOE'.
           05  FILLER                PIC X(06)           VALUE '255GAC'.
           05  FILLER                PIC X(06)           VALUE '266ALE'.
           05  FILLER                PIC X(06)           VALUE '274WYC'.
           05  FILLER                PIC X(06)           VALUE '294INC'.
           05  FILLER                PIC X(06)           VALUE '299DCM'.
           05  FILLER                PIC X(06)           VALUE '341MSP'.
           05  FILLER                PIC X(06)           VALUE '350ILE'.
           05  FILLER                PIC X(06)           VALUE '353TNC'.
           05  FILLER                PIC X(06)           VALUE '354CAE'.
           05  FILLER                PIC X(06)           VALUE '355NHE'.
           05  FILLER                PIC X(06)           VALUE '358HIM'.
           05  FILLER                PIC X(06)           VALUE '405NMM'.
           05  FILLER                PIC X(06)           VALUE '419SCP'.
           05  FILLER                PIC X(06)           VALUE '429ILP'.
           05  FILLER                PIC X(06)           VALUE '449CAP'.
           05  FILLER                PIC X(06)           VALUE '455ORE'.
           05  FILLER                PIC X(06)           VALUE '460NJC'.
           05  FILLER                PIC X(06)           VALUE '468ARM'.
           05  FILLER                PIC X(06)           VALUE '493LAC'.
           05  FILLER                PIC X(06)           VALUE '513NEM'.
           05  FILLER                PIC X(06)           VALUE '516AZC'.
           05  FILLER                PIC X(06)           VALUE '542AKC'.
           05  FILLER                PIC X(06)           VALUE '562RIM'.
           05  FILLER                PIC X(06)           VALUE '592VAC'.
           05  FILLER                PIC X(06)           VALUE '596MDP'.
           05  FILLER                PIC X(06)           VALUE '597AKE'.
           05  FILLER                PIC X(06)           VALUE '598MAE'.
           05  FILLER                PIC X(06)           VALUE '620NDC'.
           05  FILLER                PIC X(06)           VALUE '671UTP'.
           05  FILLER                PIC X(06)           VALUE '684NYP'.
           05  FILLER                PIC X(06)           VALUE '694OKP'.
           05  FILLER                PIC X(06)           VALUE '711TXM'.
           05  FILLER                PIC X(06)           VALUE '714WIE'.
           05  FILLER                PIC X(06)           VALUE '737VTE'.
           05  FILLER                PIC X(06)           VALUE '744MEM'.
           05  FILLER                PIC X(06)           VALUE '753OHM'.
           05  FILLER                PIC X(06)           VALUE '756MNM'.
           05  FILLER                PIC X(06)           VALUE '776HIC'.
           05  FILLER                PIC X(06)           VALUE '788IDM'.
           05  FILLER                PIC X(06)           VALUE '796MIC'.
           05  FILLER                PIC X(06)           VALUE '813GAE'.
           05  FILLER                PIC X(06)           VALUE '830KSP'.
           05  FILLER                PIC X(06)           VALUE '832SDE'.
           05  FILLER                PIC X(06)           VALUE '846CTM'.
           05  FILLER                PIC X(06)           VALUE '853COC'.
           05  FILLER                PIC X(06)           VALUE '858DEM'.
           05  FILLER                PIC X(06)           VALUE '882KYE'.
           05  FILLER                PIC X(06)           VALUE '884FLP'.
           05  FILLER                PIC X(06)           VALUE '889FLE'.
           05  FILLER                PIC X(06)           VALUE '892CTC'.
           05  FILLER                PIC X(06)           VALUE '903IDP'.
           05  FILLER                PIC X(06)           VALUE '933MTC'.
           05  FILLER                PIC X(06)           VALUE '940NCE'.
           05  FILLER                PIC X(06)           VALUE '950ARP'.
           05  FILLER                PIC X(06)           VALUE '955COE'.
           05  FILLER                PIC X(06)           VALUE '971WAM'.
           05  FILLER                PIC X(06)           VALUE '974PAC'.
           05  FILLER                PIC X(06)           VALUE '975ALP'.
       01  WS-LATA-TABLE REDEFINES WS-LATA-CONST.
           05  WS-WS-LT-ENTRY OCCURS 64 TIMES
                   INDEXED BY WS-LT-IX.
               10  WS-LT-LATA              PIC 9(03).
               10  WS-LT-STATE             PIC X(02).
               10  WS-LT-TZONE             PIC X(01).

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
                   VALUE 'MEET POINT BILLING SETTLEMENT REGISTER'.
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
                VALUE 'CIRCUIT-ID           OCN  OTHER OUR-PCT OTH-PCT'.
           05  FILLER              PIC X(41)
                   VALUE 'GROSS-AMOUNT   OUR-SHARE      THEIR-SHARE'.
       01  WS-HEAD-4.
           05  FILLER                  PIC X(01)             VALUE ' '.
           05  FILLER                PIC X(131)          VALUE ALL '-'.

      * DETAIL LINE WS-DETAIL-1.
       01  WS-DETAIL-1.
           05  FILLER                  PIC X(01)             VALUE ' '.
           05  WS-D1-CIRCUIT           PIC X(20).
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-OCN               PIC X(04).
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-OTHER             PIC X(04).
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-OURPCT            PIC ZZ9.99.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-OTHPCT            PIC ZZ9.99.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-GROSS             PIC ZZZ,ZZ9.99999.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-OURS              PIC ZZZ,ZZ9.99999.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-THEIRS            PIC ZZZ,ZZ9.99999.

      * DETAIL LINE WS-TOTAL-1.
       01  WS-TOTAL-1.
           05  FILLER                  PIC X(01)             VALUE ' '.
           05  FILLER                PIC X(20)           VALUE SPACES.
           05  WS-T1-DESC              PIC X(30).
           05  WS-T1-COUNT             PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                PIC X(02)           VALUE SPACES.
           05  WS-T1-AMOUNT            PIC Z,ZZZ,ZZZ,ZZ9.99999-.

      * MESSAGE TEXT TABLE.  ADDRESSED BY ERROR CODE SEQUENCE, NOT
      * BY THE CODE ITSELF - INSERTING A CODE IN THE MIDDLE SHIFTS
      * EVERY MESSAGE.  THIS HAS HAPPENED TWICE (1994 AND 2007).
       01  WS-MSG-CONST.
           05  FILLER              PIC X(44)
                   VALUE 'SETTLED AT THE FILED PERCENTAGES            '.
           05  FILLER              PIC X(44)
                   VALUE 'PERCENTAGES DO NOT SUM TO ONE HUNDRED       '.
           05  FILLER              PIC X(44)
                   VALUE 'RESIDUAL ABSORBED INTO OUR SHARE            '.
           05  FILLER              PIC X(44)
                   VALUE 'DEFAULT PERCENTAGE FROM THE TRUNK TABLE     '.
           05  FILLER              PIC X(44)
                   VALUE 'NO BILLED REVENUE FOR THE CIRCUIT           '.
           05  FILLER              PIC X(44)
                   VALUE 'RATE NOT FOUND ON THE SETTLEMENT MASTER     '.
           05  FILLER              PIC X(44)
                   VALUE 'MINIMUM CHARGE APPLIED                      '.
           05  FILLER              PIC X(44)
                   VALUE 'BAND RATE APPLIED                           '.
           05  FILLER              PIC X(44)
                   VALUE 'OUR SHARE TRUNCATED AT FIVE PLACES          '.
           05  FILLER              PIC X(44)
                   VALUE 'REGIONAL SPLIT MODULE CALLED                '.
           05  FILLER              PIC X(44)
                   VALUE 'CARRIER NOT MEET POINT ELIGIBLE             '.
           05  FILLER              PIC X(44)
                   VALUE 'BILLED DETAIL NOT MATCHED                   '.
           05  FILLER              PIC X(44)
                   VALUE 'SIMULATION MODE - NOTHING WRITTEN           '.
           05  FILLER              PIC X(44)
                   VALUE 'CIRCUIT HELD FOR THE NEXT PERIOD            '.
           05  FILLER              PIC X(44)
                   VALUE 'VARIANCE RECORD WRITTEN                     '.
           05  FILLER              PIC X(44)
                   VALUE 'SPLIT DID NOT RECONCILE TO GROSS            '.
           05  FILLER              PIC X(44)
                   VALUE 'USOC NOT ON THE CROSS REFERENCE             '.
           05  FILLER              PIC X(44)
                   VALUE 'ELEMENT NOT MEET POINT ELIGIBLE             '.
           05  FILLER              PIC X(44)
                   VALUE 'SETTLEMENT RECORD WRITTEN                   '.
           05  FILLER              PIC X(44)
                   VALUE 'END OF SETTLEMENT RUN                       '.
       01  WS-MSG-TABLE REDEFINES WS-MSG-CONST.
           05  WS-MSG-ENTRY OCCURS 20 TIMES
                   INDEXED BY WS-MSG-IX.
               10  WS-MSG-TEXT             PIC X(44).

      * STATE MEET POINT RULE TABLE.  THE RESIDUAL OWNER COLUMN
      * SAYS WHO IS SUPPOSED TO CARRY A PERCENTAGE RESIDUAL IN THAT
      * STATE - O FOR US, T FOR THEM, U FOR UNRESOLVED AND B FOR
      * SPLIT BOTH WAYS.  THE TABLE IS LOADED AND SEARCHED AND THE
      * ANSWER IS THEN NOT USED BY THE SPLIT LOGIC.  THE 2013
      * REGIONAL MODULES WERE SUPPOSED TO HONOUR IT.
       01  WS-MPBRUL-CONST.
           05  FILLER              PIC X(09)         VALUE 'ALT00500N'.
           05  FILLER              PIC X(09)         VALUE 'AKT01000N'.
           05  FILLER              PIC X(09)         VALUE 'AZU00100N'.
           05  FILLER              PIC X(09)         VALUE 'ART00100Y'.
           05  FILLER              PIC X(09)         VALUE 'CAU01000Y'.
           05  FILLER              PIC X(09)         VALUE 'COO01000N'.
           05  FILLER              PIC X(09)         VALUE 'CTU00100N'.
           05  FILLER              PIC X(09)         VALUE 'DET01000Y'.
           05  FILLER              PIC X(09)         VALUE 'FLB01000N'.
           05  FILLER              PIC X(09)         VALUE 'GAU05000Y'.
           05  FILLER              PIC X(09)         VALUE 'HIB01000Y'.
           05  FILLER              PIC X(09)         VALUE 'IDU00500Y'.
           05  FILLER              PIC X(09)         VALUE 'ILB00500N'.
           05  FILLER              PIC X(09)         VALUE 'INO00500N'.
           05  FILLER              PIC X(09)         VALUE 'IAT00500Y'.
           05  FILLER              PIC X(09)         VALUE 'KST01000N'.
           05  FILLER              PIC X(09)         VALUE 'KYT00100Y'.
           05  FILLER              PIC X(09)         VALUE 'LAO00500N'.
           05  FILLER              PIC X(09)         VALUE 'MEB05000Y'.
           05  FILLER              PIC X(09)         VALUE 'MDB01000Y'.
           05  FILLER              PIC X(09)         VALUE 'MAT01000Y'.
           05  FILLER              PIC X(09)         VALUE 'MIT05000Y'.
           05  FILLER              PIC X(09)         VALUE 'MNU05000N'.
           05  FILLER              PIC X(09)         VALUE 'MSB00100N'.
           05  FILLER              PIC X(09)         VALUE 'MOO00500N'.
           05  FILLER              PIC X(09)         VALUE 'MTU05000Y'.
           05  FILLER              PIC X(09)         VALUE 'NEU00500Y'.
           05  FILLER              PIC X(09)         VALUE 'NVU00100N'.
           05  FILLER              PIC X(09)         VALUE 'NHU00100Y'.
           05  FILLER              PIC X(09)         VALUE 'NJT00500N'.
           05  FILLER              PIC X(09)         VALUE 'NMU05000N'.
           05  FILLER              PIC X(09)         VALUE 'NYB00100Y'.
           05  FILLER              PIC X(09)         VALUE 'NCT05000Y'.
           05  FILLER              PIC X(09)         VALUE 'NDT01000Y'.
           05  FILLER              PIC X(09)         VALUE 'OHU05000Y'.
           05  FILLER              PIC X(09)         VALUE 'OKB00100N'.
           05  FILLER              PIC X(09)         VALUE 'ORU01000Y'.
           05  FILLER              PIC X(09)         VALUE 'PAB00100N'.
           05  FILLER              PIC X(09)         VALUE 'RIB00100N'.
           05  FILLER              PIC X(09)         VALUE 'SCB00500Y'.
           05  FILLER              PIC X(09)         VALUE 'SDO00500N'.
           05  FILLER              PIC X(09)         VALUE 'TNO01000N'.
           05  FILLER              PIC X(09)         VALUE 'TXU01000N'.
           05  FILLER              PIC X(09)         VALUE 'UTU01000N'.
           05  FILLER              PIC X(09)         VALUE 'VTT00500N'.
           05  FILLER              PIC X(09)         VALUE 'VAO01000Y'.
           05  FILLER              PIC X(09)         VALUE 'WAB00500N'.
           05  FILLER              PIC X(09)         VALUE 'WVO00100N'.
           05  FILLER              PIC X(09)         VALUE 'WIU05000N'.
           05  FILLER              PIC X(09)         VALUE 'WYU00500Y'.
           05  FILLER              PIC X(09)         VALUE 'DCO05000Y'.
       01  WS-MPBRUL-TABLE REDEFINES WS-MPBRUL-CONST.
           05  WS-WS-MR-ENTRY OCCURS 51 TIMES
                   INDEXED BY WS-MR-IX.
               10  WS-MR-STATE             PIC X(02).
               10  WS-MR-RESID-OWNER       PIC X(01).
               10  WS-MR-LIMIT             PIC 9(03)V9(02).
               10  WS-MR-ORDER-REQD        PIC X(01).

      * PRIOR PERIOD COMPARISON WORK AREA.  A CIRCUIT WHOSE SHARE
      * HAS MOVED BY MORE THAN THE SWING THRESHOLD SINCE LAST
      * PERIOD IS REPORTED - IT USUALLY MEANS A PERCENTAGE WAS
      * REFILED WITHOUT ANYBODY BEING TOLD.
       01  WS-PRIOR-PERIOD.
           05  WS-PP-GROSS             PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-PP-OUR-SHARE         PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-PP-THEIR-SHARE       PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-PP-DELTA             PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-PP-PCT-CHANGE        PIC S9(05)V9(05) COMP-3 VALUE 0.
           05  WS-PP-COUNT             PIC S9(09) COMP-3     VALUE 0.
           05  WS-PP-SWING-CNT         PIC S9(09) COMP-3     VALUE 0.

      * CIRCUIT VALIDATION WORK AREA AND THE STATE RULE HOLD.
       01  WS-CIRCUIT-VALIDATE.
           05  WS-CV-BAD-CNT           PIC S9(09) COMP-3     VALUE 0.
           05  WS-CV-DUP-CNT           PIC S9(09) COMP-3     VALUE 0.
           05  WS-CV-SEQ-CNT           PIC S9(09) COMP-3     VALUE 0.
           05  WS-CV-LAST-CIRCUIT  PIC X(20)         VALUE LOW-VALUES.
           05  WS-CV-LAST-OCN      PIC X(04)         VALUE LOW-VALUES.
       01  WS-STATE-RULE-HOLD.
           05  WS-SR-OWNER             PIC X(01)             VALUE 'U'.
           05  WS-SR-LIMIT             PIC 9(03)V9(02)       VALUE 0.
           05  WS-SR-ORDER             PIC X(01)             VALUE 'N'.
           05  WS-SR-FOUND-SW          PIC X(01)             VALUE 'N'.
                   88  WS-SR-FOUND              VALUE 'Y'.

      * WORK FIELDS FOR THE ELEMENT WALK AND THE RATE LOOKUP.
       01  WS-ELEMENT-AREA.
           05  WS-ELEM-HOLD          PIC X(06)           VALUE SPACES.
           05  WS-ELEM-ELIG            PIC X(01)             VALUE 'N'.
           05  WS-USOC-HOLD          PIC X(05)           VALUE SPACES.
           05  WS-EW-COUNT             PIC S9(03) COMP-3     VALUE 0.
           05  WS-EW-TOTAL             PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-SEQ-NBR              PIC 9(09) COMP-3      VALUE 0.
       01  WS-RATE-KEY.
           05  WS-RK-TARIFF          PIC X(04)           VALUE SPACES.
           05  WS-RK-ELEM            PIC X(06)           VALUE SPACES.
           05  WS-RK-JURIS           PIC X(01)           VALUE SPACES.
           05  WS-RK-STATE           PIC X(02)           VALUE SPACES.
           05  WS-RK-EFF               PIC 9(05)             VALUE 0.

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
      * THE SETTLEMENT PERIOD ARRIVES AS A SYMBOLIC SUBSTITUTED
      * BY THE SCHEDULER AT SUBMISSION TIME AND HAS NO DEFAULT.
      * RUNNING THIS STEP FOR THE WRONG PERIOD SETTLES THE WRONG
      * MONTH WITH EVERY OTHER LEC IN THE REGION AND THERE IS NO
      * REVERSAL PROGRAM ON THIS SIDE OF THE APPLICATION.
           MOVE 'P1000-INIT' TO WS-PARA-NAME.
           ACCEPT WS-ACCEPT-DATE FROM DATE.
           ACCEPT WS-ACCEPT-TIME FROM TIME.
           OPEN INPUT  MPB-VALID-FILE
                       BILL-DETAIL-FILE
                       RATE-MASTER
                       CARRIER-MASTER
                       PARM-FILE
           OPEN OUTPUT SETTLE-OUT-FILE
                       VARIANCE-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 6401 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-MPBVAL' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 6402 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-BILLDTL' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 6403 TO WS-AB-CODE
               MOVE 'OPEN FAILED DA-I-RATEMAST' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 6404 TO WS-AB-CODE
               MOVE 'OPEN FAILED DA-I-CARRMAST' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 6405 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-SETLOUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-INPUT NOT = '00'
               MOVE 6406 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-VAROUT' TO WS-AB-TEXT
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
           MOVE ZERO TO WS-SETTLE-CNT WS-NOREV-CNT WS-VAR-CNT
                        WS-RESID-CNT WS-DEFAULT-CNT WS-MIN-CNT
                        WS-BAND-CNT WS-TOT-GROSS WS-TOT-OURS
                        WS-TOT-THEIRS WS-TOT-RESIDUAL WS-TOT-TRUNC
                        WS-TOT-MOU.
           PERFORM P6000-HEADING THRU P6000-EXIT.
           PERFORM P2600-READ-BILL THRU P2600-EXIT.
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
           IF WS-PE-SETTLE-PERIOD NOT NUMERIC
               MOVE 6410 TO WS-AB-CODE
               MOVE 'SETTLEMENT PERIOD NOT SUPPLIED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PE-TARIFF-CD = SPACES
               MOVE 'MPB1' TO WS-PE-TARIFF-CD.
           IF WS-PE-MIN-AMOUNT NOT NUMERIC
               MOVE ZERO TO WS-PE-MIN-AMOUNT.
           IF WS-PE-REGION = SPACES
               MOVE 'A' TO WS-PE-REGION.
           IF WS-PE-SIM-SW NOT = 'Y'
               MOVE 'N' TO WS-PE-SIM-SW.

       P1200-EXIT.
           EXIT.


      *****************************************************************
      * S200-CIRCUIT-DRIVER                                           *
      * READ THE ELIGIBLE CIRCUITS AND FIND THEIR REVENUE.            *
      *****************************************************************
       S200-CIRCUIT-DRIVER SECTION.

       P2000-PROCESS.
      * ONE MEET POINT CIRCUIT PER PASS.  THE CIRCUIT IS THE UNIT
      * OF SETTLEMENT - NOT THE USAGE RECORD AND NOT THE ACCOUNT.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ THRU P2100-EXIT.
           IF WS-EOF
               GO TO P2000-EXIT.
           MOVE MPI-RECORD TO WS-EXTRACT-RECORD.
           MOVE WS-XK-KEY TO WS-RESTART-KEY.
           MOVE SPACES TO WS-D1-DISPO.
           MOVE 'Y' TO WS-PCT-OK-SW.
           MOVE 'N' TO WS-DEFAULT-SW.
           MOVE 'N' TO WS-BAND-SW.
           MOVE ZERO TO WS-MW-GROSS.
           PERFORM P2200-CARRIER-CHECK THRU P2200-EXIT.
           IF NOT WS-CARR-FOUND
               ADD 1 TO WS-REJECT-CNT
               MOVE WS-MSG-TEXT (11) TO WS-D1-DISPO
               PERFORM P6100-DETAIL THRU P6100-EXIT
               GO TO P2000-EXIT.
           PERFORM P2500-MATCH-BILLED THRU P2500-EXIT.
           IF NOT WS-BILL-MATCHED
               ADD 1 TO WS-NOREV-CNT
               ADD 1 TO WS-SUMM-CNT
               MOVE WS-MSG-TEXT (5) TO WS-D1-DISPO
               PERFORM P6100-DETAIL THRU P6100-EXIT
               GO TO P2000-EXIT.
           PERFORM P2800-GROSS-AMOUNT THRU P2800-EXIT.
           IF WS-MW-GROSS = ZERO
               ADD 1 TO WS-NOREV-CNT
               ADD 1 TO WS-SUMM-CNT
               MOVE WS-MSG-TEXT (5) TO WS-D1-DISPO
               PERFORM P6100-DETAIL THRU P6100-EXIT
               GO TO P2000-EXIT.
           PERFORM P3000-RESOLVE-PCT THRU P3000-EXIT.
           PERFORM P3200-PCT-RESIDUAL THRU P3200-EXIT.
           PERFORM P3500-SPLIT THRU P3500-EXIT.
           PERFORM P3700-RECONCILE THRU P3700-EXIT.
           PERFORM P4000-CALL-REGION THRU P4000-EXIT.
           PERFORM P4500-BUILD-SETTLE THRU P4500-EXIT.
           PERFORM P4700-WRITE-SETTLE THRU P4700-EXIT.
           PERFORM P6100-DETAIL THRU P6100-EXIT.
           ADD 1 TO WS-WRITE-CNT.
           ADD 1 TO WS-SETTLE-CNT.

       P2000-EXIT.
           EXIT.

       P2100-READ.
      * SEQUENTIAL READ OF THE VALIDATED CIRCUIT EXTRACT.
           READ MPB-VALID-FILE
               AT END
                   MOVE 'Y' TO WS-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00'
               MOVE 3640 TO WS-AB-CODE
               MOVE 'READ ERROR UT-S-MPBVAL' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-READ-CNT.

       P2100-EXIT.
           EXIT.

       P2200-CARRIER-CHECK.
      * THE OTHER LEC MUST BE ON THE CARRIER MASTER AND MUST BE
      * FLAGGED MEET POINT ELIGIBLE.  A CARRIER THAT HAS LAPSED
      * SINCE THE CIRCUIT WAS BUILT IS REJECTED HERE RATHER THAN
      * SETTLED INTO A VOID.
           MOVE 'P2200-CARRIER-CHECK' TO WS-PARA-NAME.
           MOVE 'N' TO WS-CARR-FOUND-SW.
           MOVE WS-XR-OTHER-OCN TO CRM-KEY.
           READ CARRIER-MASTER
               INVALID KEY
                   GO TO P2200-EXIT.
           MOVE CRM-RECORD TO CABS-CARRIER-RECORD.
           IF CR-MPB-ELIGIBLE = 'Y'
               MOVE 'Y' TO WS-CARR-FOUND-SW
           ELSE
               MOVE EC-MPB-PCT-INVALID TO WS-ERR-CODE
               MOVE 'W' TO WS-ERR-SEVERITY
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               SUBTRACT 1 FROM WS-REJECT-CNT.

       P2200-EXIT.
           EXIT.

       P2500-MATCH-BILLED.
      * MATCH THE CIRCUIT TO THE BILLED DETAIL LINES RAISED AGAINST
      * IT THIS PERIOD.  THE BILLED DETAIL FILE IS OWNED BY THE
      * BILLING APPLICATION AND IS READ DIRECTLY BY THIS PROGRAM -
      * THERE IS NO EXTRACT AND NO INTERFACE.  THE SETTLEMENT RUN
      * THEREFORE CANNOT START UNTIL BILLING HAS FINISHED WRITING
      * THAT FILE, AND NOTHING IN EITHER SCHEDULE ENFORCES IT.
      * DATASET SHARING APPROVED BY THE DATA ADMINISTRATION GROUP.
           MOVE 'P2500-MATCH-BILLED' TO WS-PARA-NAME.
           MOVE 'N' TO WS-BILL-MATCH-SW.
           MOVE ZERO TO WS-MW-QTY.
           MOVE ZERO TO WS-EW-TOTAL.
           IF WS-BILL-EOF
               GO TO P2500-EXIT.
           PERFORM P2550-ADVANCE-BILL THRU P2550-EXIT
               UNTIL WS-BILL-EOF
                  OR BD-BAN NOT < WS-XR-BAN.
           IF WS-BILL-EOF
               GO TO P2500-EXIT.
           IF BD-BAN NOT = WS-XR-BAN
               MOVE WS-MSG-TEXT (12) TO WS-D1-DISPO
               GO TO P2500-EXIT.
           MOVE 'Y' TO WS-BILL-MATCH-SW.
           PERFORM P2700-ELEMENT-WALK THRU P2700-EXIT.

       P2500-EXIT.
           EXIT.

       P2550-ADVANCE-BILL.
      * ADVANCE THE BILLED DETAIL FILE.
           PERFORM P2600-READ-BILL THRU P2600-EXIT.

       P2550-EXIT.
           EXIT.

       P2600-READ-BILL.
      * READ THE CABS BILLED DETAIL FILE.  IT IS A VARIABLE LENGTH
      * FILE WITH AN OCCURS DEPENDING ON, SO IT IS READ INTO THE
      * COPYBOOK AREA AND NOT MOVED TO A FIXED FIELD FIRST.
           READ BILL-DETAIL-FILE INTO CABS-BILL-DETAIL
               AT END
                   MOVE 'Y' TO WS-BILL-EOF-SW
                   GO TO P2600-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 6402 TO WS-AB-CODE
               MOVE 'READ ERROR ON BILLDTL' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.

       P2600-EXIT.
           EXIT.

       P2700-ELEMENT-WALK.
      * WALK THE RATE ELEMENTS ON THE BILLED LINE AND ACCUMULATE
      * ONLY THOSE THAT ARE MEET POINT ELIGIBLE.  AN ELEMENT THAT
      * IS NOT JOINTLY PROVIDED IS NOT SHARED - THE LOCAL CHANNEL
      * AT OUR END IS OURS IN FULL.
           MOVE 'P2700-ELEMENT-WALK' TO WS-PARA-NAME.
           MOVE ZERO TO WS-EW-TOTAL.
           IF BD-ELEM-CNT = ZERO
               GO TO P2700-EXIT.
           IF BD-ELEM-CNT > 40
               MOVE 40 TO WS-EW-COUNT
           ELSE
               MOVE BD-ELEM-CNT TO WS-EW-COUNT.
           MOVE 1 TO WS-ELEM-SUB.
           PERFORM P2750-ELEMENT-ONE THRU P2750-EXIT
               UNTIL WS-ELEM-SUB > WS-EW-COUNT.

       P2700-EXIT.
           EXIT.

       P2750-ELEMENT-ONE.
      * ONE RATE ELEMENT.  ELIGIBILITY COMES FROM THE ELEMENT
      * ATTRIBUTE TABLE.
           MOVE BD-EL-RATE-ELEM (WS-ELEM-SUB) TO WS-ELEM-HOLD.
           MOVE 'N' TO WS-ELEM-ELIG.
           SET WS-RE-IX TO 1.
           SEARCH WS-RE-ENTRY
               AT END
                   MOVE 'Y' TO WS-ELEM-ELIG
               WHEN WS-RE-ELEM (WS-RE-IX) = WS-ELEM-HOLD
                   MOVE 'Y' TO WS-ELEM-ELIG.
           IF WS-ELEM-ELIG = 'Y'
               ADD BD-EL-AMOUNT (WS-ELEM-SUB) TO WS-EW-TOTAL
               ADD BD-EL-QTY (WS-ELEM-SUB) TO WS-MW-QTY.
           ADD 1 TO WS-ELEM-SUB.

       P2750-EXIT.
           EXIT.

       P2800-GROSS-AMOUNT.
      * THE GROSS AMOUNT IS WHAT WAS BILLED FOR THE JOINTLY
      * PROVIDED SERVICE.  WHERE THE BILLED DETAIL CARRIES IT, THAT
      * FIGURE IS USED.  WHERE IT DOES NOT, THE AMOUNT IS REBUILT
      * FROM THE RATE MASTER AND THE QUANTITY - WHICH IS WHY THE
      * SETTLEMENT TOTAL AND THE BILLED TOTAL ARE CLOSE BUT NOT
      * IDENTICAL AT PERIOD END.
           MOVE 'P2800-GROSS-AMOUNT' TO WS-PARA-NAME.
           MOVE WS-EW-TOTAL TO WS-MW-GROSS.
           MOVE BD-TOT-MINUTES TO WS-MW-MOU.
           IF WS-MW-GROSS NOT = ZERO
               GO TO P2850-MINIMUM.
           PERFORM P2900-RATE-LOOKUP THRU P2900-EXIT.
           IF NOT WS-RATE-FOUND
               MOVE EC-RATE-NOT-FOUND TO WS-ERR-CODE
               MOVE 'W' TO WS-ERR-SEVERITY
               MOVE WS-MSG-TEXT (6) TO WS-D1-DISPO
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               SUBTRACT 1 FROM WS-REJECT-CNT
               GO TO P2800-EXIT.
           PERFORM P2950-BAND-RATE THRU P2950-EXIT.
           COMPUTE WS-MW-GROSS ROUNDED =
                   WS-MW-QTY * WS-MW-RATE.

       P2800-EXIT.
           EXIT.

       P2850-MINIMUM.
      * APPLY THE MINIMUM CHARGE.  A JOINTLY PROVIDED CIRCUIT WITH
      * ALMOST NO USAGE STILL ATTRACTS THE MINIMUM, AND THE MINIMUM
      * IS SHARED THE SAME WAY THE USAGE REVENUE IS.
           IF WS-PE-MIN-AMOUNT = ZERO
               GO TO P2850-EXIT.
           IF WS-MW-GROSS NOT < WS-PE-MIN-AMOUNT
               GO TO P2850-EXIT.
           MOVE WS-PE-MIN-AMOUNT TO WS-MW-GROSS.
           ADD 1 TO WS-MIN-CNT.
           MOVE WS-MSG-TEXT (7) TO WS-D1-DISPO.

       P2850-EXIT.
           EXIT.

       P2900-RATE-LOOKUP.
      * FETCH THE JOINTLY PROVIDED RATE FROM THE SETTLEMENT RATE
      * MASTER.  THE SETTLEMENT APPLICATION KEEPS ITS OWN RATE FILE
      * BECAUSE THE MEET POINT RATES ARE NEGOTIATED AND DIFFER FROM
      * THE TARIFFED ACCESS RATES ON THE BILLING SIDE.
           MOVE 'P2900-RATE-LOOKUP' TO WS-PARA-NAME.
           MOVE 'N' TO WS-RATE-FOUND-SW.
           MOVE ZERO TO WS-MW-RATE.
           PERFORM P2920-USOC-XREF THRU P2920-EXIT.
           MOVE SPACES TO WS-RATE-KEY.
           MOVE WS-PE-TARIFF-CD TO WS-RK-TARIFF.
           MOVE WS-ELEM-HOLD TO WS-RK-ELEM.
           MOVE 'I' TO WS-RK-JURIS.
           MOVE WS-XR-STATE TO WS-RK-STATE.
           MOVE WS-CYCLE-YYDDD TO WS-RK-EFF.
           MOVE WS-RATE-KEY TO RTM-KEY.
           READ RATE-MASTER INTO CABS-RATE-RECORD
               INVALID KEY
                   GO TO P2900-EXIT.
           MOVE RT-INITIAL-RATE TO WS-MW-RATE.
           MOVE RT-MIN-CHG TO WS-MW-MIN-CHG.
           MOVE 'Y' TO WS-RATE-FOUND-SW.

       P2900-EXIT.
           EXIT.

       P2920-USOC-XREF.
      * TRANSLATE THE USOC ON THE CIRCUIT INTO A RATE ELEMENT.
           MOVE SPACES TO WS-ELEM-HOLD.
           SET WS-UX-IX TO 1.
           SEARCH WS-UX-ENTRY
               AT END
                   MOVE 'MPBTRN' TO WS-ELEM-HOLD
                   MOVE WS-MSG-TEXT (17) TO WS-D1-DISPO
                   GO TO P2920-EXIT
               WHEN WS-UX-USOC (WS-UX-IX) = WS-USOC-HOLD
                   MOVE WS-UX-ELEM (WS-UX-IX) TO WS-ELEM-HOLD.

       P2920-EXIT.
           EXIT.

       P2950-BAND-RATE.
      * THE RATE MASTER CARRIES A VOLUME BAND TABLE.  WHERE THE
      * QUANTITY FALLS INSIDE A BAND THE BAND RATE REPLACES THE
      * INITIAL RATE.  THE BANDS ARE AN OCCURS DEPENDING ON, SO THE
      * COUNT MUST BE TAKEN FROM THE RECORD FIRST.
           MOVE 'N' TO WS-BAND-SW.
           IF RT-BAND-CNT = ZERO
               GO TO P2950-EXIT.
           MOVE 1 TO WS-BAND-SUB.
           PERFORM P2960-BAND-ONE THRU P2960-EXIT
               UNTIL WS-BAND-SUB > RT-BAND-CNT
                  OR WS-BAND-APPLIED.
           IF WS-BAND-APPLIED
               MOVE WS-MW-BAND-RATE TO WS-MW-RATE
               ADD 1 TO WS-BAND-CNT
               MOVE WS-MSG-TEXT (8) TO WS-D1-DISPO.

       P2950-EXIT.
           EXIT.

       P2960-BAND-ONE.
      * ONE VOLUME BAND.
           IF WS-MW-QTY NOT < RT-BAND-FROM (WS-BAND-SUB) AND
              WS-MW-QTY NOT > RT-BAND-THRU (WS-BAND-SUB)
               MOVE RT-BAND-RATE (WS-BAND-SUB) TO WS-MW-BAND-RATE
               MOVE 'Y' TO WS-BAND-SW
               GO TO P2960-EXIT.
           ADD 1 TO WS-BAND-SUB.

       P2960-EXIT.
           EXIT.


      *****************************************************************
      * S300-PERCENTAGE                                               *
      * RESOLVE THE TWO PERCENTAGES AND SPLIT THE REVENUE.            *
      *****************************************************************
       S300-PERCENTAGE SECTION.

       P3000-RESOLVE-PCT.
      * OUR PERCENTAGE AND THE OTHER LECS PERCENTAGE COME OFF THE
      * CIRCUIT EXTRACT.  WHERE BOTH ARE ZERO THE TRUNK GROUP
      * DEFAULT TABLE SUPPLIES A PAIR - AND THAT TABLE HAS NOT BEEN
      * REFRESHED SINCE 1990.
           MOVE 'P3000-RESOLVE-PCT' TO WS-PARA-NAME.
           MOVE WS-XR-OUR-PCT TO WS-MW-OUR-PCT.
           MOVE WS-XR-OTHER-PCT TO WS-MW-THEIR-PCT.
           IF WS-MW-OUR-PCT NOT = ZERO OR
              WS-MW-THEIR-PCT NOT = ZERO
               GO TO P3000-EXIT.
           PERFORM P3100-TRUNK-DEFAULT THRU P3100-EXIT.

       P3000-EXIT.
           EXIT.

       P3100-TRUNK-DEFAULT.
      * SEARCH THE TRUNK GROUP DEFAULT TABLE.  A HIT SETS OUR
      * PERCENTAGE AND DERIVES THEIRS AS THE COMPLEMENT, WHICH IS
      * THE ONLY PLACE IN THE PROGRAM WHERE THE TWO ARE GUARANTEED
      * TO SUM TO ONE HUNDRED.
           SET WS-TK-IX TO 1.
           SEARCH WS-TK-ENTRY
               AT END
                   MOVE 050.00000 TO WS-MW-OUR-PCT
                   MOVE 050.00000 TO WS-MW-THEIR-PCT
                   MOVE 'Y' TO WS-DEFAULT-SW
                   ADD 1 TO WS-DEFAULT-CNT
                   MOVE WS-MSG-TEXT (4) TO WS-D1-DISPO
                   GO TO P3100-EXIT
               WHEN WS-TK-TRUNK (WS-TK-IX) = WS-XR-TRUNK-GRP
                   MOVE WS-TK-PCT (WS-TK-IX) TO WS-MW-OUR-PCT.
           COMPUTE WS-MW-THEIR-PCT = 100.00000 - WS-MW-OUR-PCT.
           MOVE 'Y' TO WS-DEFAULT-SW.
           ADD 1 TO WS-DEFAULT-CNT.
           MOVE WS-MSG-TEXT (4) TO WS-D1-DISPO.

       P3100-EXIT.
           EXIT.

       P3200-PCT-RESIDUAL.
      * EACH LEC FILES ITS OWN BILLING PERCENTAGE SEPARATELY AND THE
      * PAIR IS SUPPOSED TO SUM TO EXACTLY ONE HUNDRED.  THIS
      * PARAGRAPH DEALS WITH THE CASE WHERE THEY DO NOT.  THE 2000
      * CHANGE ALTERED THE HANDLING OF THE RESIDUAL.
           MOVE 'P3200-PCT-RESIDUAL' TO WS-PARA-NAME.
           COMPUTE WS-MW-TOTAL-PCT =
                   WS-MW-OUR-PCT + WS-MW-THEIR-PCT.
           IF WS-MW-TOTAL-PCT = 100.00000
               MOVE ZERO TO WS-MW-RESIDUAL-PCT
               MOVE WS-MSG-TEXT (1) TO WS-D1-DISPO
               GO TO P3200-EXIT.
           COMPUTE WS-MW-RESIDUAL-PCT =
                   100.00000 - WS-MW-TOTAL-PCT.
           ADD WS-MW-RESIDUAL-PCT TO WS-MW-OUR-PCT.
           ADD 1 TO WS-RESID-CNT.
           ADD 1 TO WS-VAR-CNT.
           MOVE WS-MSG-TEXT (3) TO WS-D1-DISPO.
           PERFORM P3300-WRITE-VARIANCE THRU P3300-EXIT.

       P3200-EXIT.
           EXIT.

       P3300-WRITE-VARIANCE.
      * WRITE THE VARIANCE DETAIL RECORD.  THE ACCESS MANAGEMENT
      * GROUP IS SUPPOSED TO WORK THIS FILE EVERY MONTH AND AGREE
      * THE PERCENTAGES WITH THE OTHER LEC.  THE FILE HAS BEEN
      * PRODUCED EVERY MONTH SINCE 1993.
           MOVE SPACES TO WS-VARIANCE-RECORD.
           MOVE WS-XR-CIRCUIT-ID TO WS-VR-CIRCUIT-ID.
           MOVE WS-XR-OCN TO WS-VR-OCN.
           MOVE WS-XR-OTHER-OCN TO WS-VR-OTHER-OCN.
           MOVE WS-PE-SETTLE-PERIOD TO WS-VR-PERIOD.
           MOVE WS-XR-OUR-PCT TO WS-VR-OUR-PCT.
           MOVE WS-XR-OTHER-PCT TO WS-VR-THEIR-PCT.
           MOVE WS-MW-TOTAL-PCT TO WS-VR-TOTAL-PCT.
           MOVE WS-MW-RESIDUAL-PCT TO WS-VR-RESIDUAL.
           MOVE WS-MW-GROSS TO WS-VR-GROSS.
           COMPUTE WS-VR-IMPACT ROUNDED =
                   WS-MW-GROSS * WS-MW-RESIDUAL-PCT / 100.
           MOVE WS-MSG-TEXT (15) TO WS-VR-DISPOSITION.
           ADD WS-VR-IMPACT TO WS-TOT-RESIDUAL.
           MOVE WS-VARIANCE-RECORD TO VAR-RECORD.
           WRITE VAR-RECORD.

       P3300-EXIT.
           EXIT.

       P3500-SPLIT.
      * THE SPLIT ITSELF.  OUR SHARE IS THE GROSS TIMES OUR
      * PERCENTAGE; THEIR SHARE IS THE GROSS TIMES THEIRS.  NOTE
      * THERE IS NO ROUNDED CLAUSE ON OUR SHARE - IT IS TRUNCATED
      * AT FIVE PLACES.  THEIR SHARE IS ROUNDED.  THE ASYMMETRY IS
      * SET BY THE MEET POINT AGREEMENT AND HAS BEEN IN PLACE
      * SINCE 1987.  SEE CABS-STD-041.
           MOVE 'P3500-SPLIT' TO WS-PARA-NAME.
           COMPUTE WS-MW-OUR-SHARE =
                   WS-MW-GROSS * WS-MW-OUR-PCT / 100.
           COMPUTE WS-MW-THEIR-SHARE ROUNDED =
                   WS-MW-GROSS * WS-MW-THEIR-PCT / 100.
           COMPUTE WS-MW-EXACT-SHARE ROUNDED =
                   WS-MW-GROSS * WS-MW-OUR-PCT / 100.
           COMPUTE WS-MW-TRUNC-DIFF =
                   WS-MW-EXACT-SHARE - WS-MW-OUR-SHARE.
           ADD WS-MW-TRUNC-DIFF TO WS-TOT-TRUNC.
           ADD WS-MW-GROSS TO WS-TOT-GROSS.
           ADD WS-MW-OUR-SHARE TO WS-TOT-OURS.
           ADD WS-MW-THEIR-SHARE TO WS-TOT-THEIRS.
           ADD WS-MW-MOU TO WS-TOT-MOU.
           ADD WS-MW-MOU TO WS-ACC-MINUTES.
           ADD WS-MW-OUR-SHARE TO WS-ACC-AMOUNT.
           MOVE WS-MSG-TEXT (9) TO WS-D1-DISPO.

       P3500-EXIT.
           EXIT.

       P3700-RECONCILE.
      * OUR SHARE PLUS THEIR SHARE SHOULD EQUAL THE GROSS.  WHERE
      * IT DOES NOT THE DIFFERENCE IS COUNTED.  IT IS NOT REJECTED
      * - A CIRCUIT THAT FAILS THIS TEST IS STILL SETTLED, BECAUSE
      * NOT SETTLING IT WOULD LEAVE THE OTHER LEC UNPAID.
           COMPUTE WS-MW-CHECK =
                   WS-MW-OUR-SHARE + WS-MW-THEIR-SHARE.
           IF WS-MW-CHECK = WS-MW-GROSS
               GO TO P3700-EXIT.
           MOVE WS-MSG-TEXT (16) TO WS-D1-DISPO.
           MOVE EC-MPB-PCT-INVALID TO WS-ERR-CODE.
           MOVE 'W' TO WS-ERR-SEVERITY.
           PERFORM P7000-SUSPEND THRU P7000-EXIT.
           SUBTRACT 1 FROM WS-REJECT-CNT.

       P3700-EXIT.
           EXIT.


      *****************************************************************
      * S400-REGION-AND-OUTPUT                                        *
      * REGIONAL RULES AND THE SETTLEMENT RECORD.                     *
      *****************************************************************
       S400-REGION-AND-OUTPUT SECTION.

       P4000-CALL-REGION.
      * THREE STATE COMMISSIONS ORDERED THEIR OWN TREATMENT OF THE
      * RESIDUAL IN 2013.  EACH REGION HAS ITS OWN LOAD MODULE AND
      * THE NAME IS ASSEMBLED FROM THE REGION CODE AT RUN TIME.
      * CALL TARGETS ARE LISTED IN THE APPLICATION BUILD NOTE.
           MOVE 'P4000-CALL-REGION' TO WS-PARA-NAME.
           MOVE SPACES TO WS-CP-SUFFIX.
           SET WS-CS-IX TO 1.
           SEARCH WS-CS-ENT
               AT END
                   GO TO P4000-EXIT
               WHEN WS-CS-KEY (WS-CS-IX) = WS-PE-REGION
                   MOVE WS-CS-SUF (WS-CS-IX) TO WS-CP-SUFFIX.
           IF WS-CP-SUFFIX = SPACES
               GO TO P4000-EXIT.
           ADD 1 TO WS-CALL-COUNT.
           CALL WS-CALL-PGM USING WS-MPB-WORK
                                  WS-EXTRACT-RECORD
                                  WS-CALL-RC.
           IF WS-CALL-RC = ZERO
               MOVE WS-MSG-TEXT (10) TO WS-D1-DISPO.

       P4000-EXIT.
           EXIT.

       P4500-BUILD-SETTLE.
      * BUILD THE SETTLEMENT RECORD.  THE MEET POINT AREA CARRIES
      * BOTH PERCENTAGES AND THE VARIANCE SO THAT THE STATEMENT CAN
      * SHOW THE CARRIER HOW THE SPLIT WAS ARRIVED AT.
           MOVE 'P4500-BUILD-SETTLE' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-SETTLEMENT-RECORD.
           MOVE 'M' TO ST-SETTLE-TYPE.
           MOVE WS-XR-OTHER-OCN TO ST-COUNTERPARTY-OCN.
           MOVE WS-PE-SETTLE-PERIOD TO ST-SETTLE-PERIOD.
           ADD 1 TO WS-SEQ-NBR.
           MOVE WS-SEQ-NBR TO ST-SEQ.
           MOVE WS-MW-MOU TO ST-TOTAL-MOU.
           MOVE WS-MW-MOU TO ST-BILLABLE-MOU.
           MOVE ZERO TO ST-CAPPED-MOU.
           MOVE WS-MW-RATE TO ST-RATE-APPLIED.
           MOVE WS-MW-OUR-PCT TO ST-OUR-PCT.
           MOVE WS-MW-THEIR-PCT TO ST-THEIR-PCT.
           MOVE WS-MW-RESIDUAL-PCT TO ST-PCT-VARIANCE.
           MOVE WS-XR-TRUNK-GRP TO ST-TRUNK-GRP.
           MOVE WS-XR-CIRCUIT-ID TO ST-CIRCUIT-ID.
           MOVE WS-MW-GROSS TO ST-GROSS-AMT.
           MOVE WS-MW-OUR-SHARE TO ST-OUR-SHARE.
           MOVE WS-MW-THEIR-SHARE TO ST-THEIR-SHARE.
           COMPUTE ST-NET-DUE ROUNDED = WS-MW-THEIR-SHARE.
           MOVE WS-MW-TRUNC-DIFF TO ST-ROUND-RESIDUE.
           MOVE 'P' TO ST-DIRECTION.
           MOVE 'N' TO ST-DISPUTE-SW.
           MOVE WS-CYCLE-YYDDD TO ST-EXCH-YYDDD.
           MOVE SPACES TO ST-RAO-CODE.

       P4500-EXIT.
           EXIT.

       P4700-WRITE-SETTLE.
      * WRITE THE SETTLEMENT RECORD.  IN SIMULATION MODE NOTHING IS
      * WRITTEN AND ONLY THE REGISTER IS PRODUCED.
           IF WS-PE-SIM-SW = 'Y'
               MOVE WS-MSG-TEXT (13) TO WS-D1-DISPO
               GO TO P4700-EXIT.
           MOVE CABS-SETTLEMENT-RECORD TO STO-RECORD.
           WRITE STO-RECORD.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 6405 TO WS-AB-CODE
               MOVE 'SETTLEMENT WRITE FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           MOVE WS-MSG-TEXT (19) TO WS-D1-DISPO.

       P4700-EXIT.
           EXIT.


      *****************************************************************
      * S600-REPORT                                                   *
      * SETTLEMENT REGISTER AND TOTALS.                               *
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
      * ONE LINE PER CIRCUIT.
           IF WS-LINE-CNT > WS-MAX-LINES
               PERFORM P6000-HEADING THRU P6000-EXIT.
           MOVE WS-XR-CIRCUIT-ID TO WS-D1-CIRCUIT.
           MOVE WS-XR-OCN TO WS-D1-OCN.
           MOVE WS-XR-OTHER-OCN TO WS-D1-OTHER.
           MOVE WS-MW-OUR-PCT TO WS-D1-OURPCT.
           MOVE WS-MW-THEIR-PCT TO WS-D1-OTHPCT.
           MOVE WS-MW-GROSS TO WS-D1-GROSS.
           MOVE WS-MW-OUR-SHARE TO WS-D1-OURS.
           MOVE WS-MW-THEIR-SHARE TO WS-D1-THEIRS.
           WRITE PRT-RECORD FROM WS-DETAIL-1 AFTER ADVANCING 1 LINES.
           ADD 1 TO WS-LINE-CNT.

       P6100-EXIT.
           EXIT.

       P6300-TOTALS.
      * END OF RUN TOTALS PAGE.
           PERFORM P6000-HEADING THRU P6000-EXIT.
           MOVE SPACES TO WS-TOTAL-1.
           MOVE 'GROSS JOINTLY PROVIDED REVENUE' TO WS-T1-DESC.
           MOVE WS-SETTLE-CNT TO WS-T1-COUNT.
           MOVE WS-TOT-GROSS TO WS-T1-AMOUNT.
           WRITE PRT-RECORD FROM WS-TOTAL-1 AFTER ADVANCING 2 LINES.
           MOVE 'OUR SHARE' TO WS-T1-DESC.
           MOVE WS-SETTLE-CNT TO WS-T1-COUNT.
           MOVE WS-TOT-OURS TO WS-T1-AMOUNT.
           WRITE PRT-RECORD FROM WS-TOTAL-1 AFTER ADVANCING 1 LINES.
           MOVE 'THEIR SHARE' TO WS-T1-DESC.
           MOVE WS-SETTLE-CNT TO WS-T1-COUNT.
           MOVE WS-TOT-THEIRS TO WS-T1-AMOUNT.
           WRITE PRT-RECORD FROM WS-TOTAL-1 AFTER ADVANCING 1 LINES.
           MOVE 'RESIDUAL ABSORBED INTO OUR SHARE' TO WS-T1-DESC.
           MOVE WS-RESID-CNT TO WS-T1-COUNT.
           MOVE WS-TOT-RESIDUAL TO WS-T1-AMOUNT.
           WRITE PRT-RECORD FROM WS-TOTAL-1 AFTER ADVANCING 1 LINES.
           MOVE 'TRUNCATION DIFFERENCE' TO WS-T1-DESC.
           MOVE WS-SETTLE-CNT TO WS-T1-COUNT.
           MOVE WS-TOT-TRUNC TO WS-T1-AMOUNT.
           WRITE PRT-RECORD FROM WS-TOTAL-1 AFTER ADVANCING 1 LINES.
           ADD 6 TO WS-LINE-CNT.

       P6300-EXIT.
           EXIT.


      *****************************************************************
      * S250-CIRCUIT-VALIDATION                                       *
      * SEQUENCE AND DUPLICATE CHECKS ON THE EXTRACT.                 *
      *****************************************************************
       S250-CIRCUIT-VALIDATION SECTION.

       P2200-SEQUENCE-CHECK.
      * THE EXTRACT MUST BE IN CIRCUIT IDENTIFIER SEQUENCE.  A FILE
      * OUT OF SEQUENCE MEANS THE BILLED DETAIL MATCH BELOW WILL
      * MISS - THE MATCH IS A SINGLE FORWARD PASS AND CANNOT GO
      * BACK.  THE CHECK COUNTS RATHER THAN ABENDS BECAUSE THE SORT
      * STEP THAT FEEDS THIS ONE HAS ITS OWN SEQUENCE PROBLEMS.
           MOVE 'P2200-SEQUENCE-CHECK' TO WS-PARA-NAME.
           IF WS-XR-CIRCUIT-ID = WS-CV-LAST-CIRCUIT
               ADD 1 TO WS-CV-DUP-CNT
               MOVE EC-DUP-SEQ TO WS-ERR-CODE
               MOVE 'W' TO WS-ERR-SEVERITY.
           IF WS-XR-CIRCUIT-ID < WS-CV-LAST-CIRCUIT
               ADD 1 TO WS-CV-SEQ-CNT.
           MOVE WS-XR-CIRCUIT-ID TO WS-CV-LAST-CIRCUIT.
           MOVE WS-XR-OCN TO WS-CV-LAST-OCN.

       P2200-EXIT.
           EXIT.

       P2250-FIELD-EDIT.
      * EDIT THE FIELDS THE SETTLEMENT DEPENDS ON.  A BLANK OTHER
      * LEC OR A ZERO PERIOD MAKES THE RECORD UNSETTLEABLE.
           MOVE 'P2250-FIELD-EDIT' TO WS-PARA-NAME.
           IF WS-XR-OTHER-OCN = SPACES
               ADD 1 TO WS-CV-BAD-CNT
               MOVE EC-MPB-PCT-INVALID TO WS-ERR-CODE
               MOVE 'E' TO WS-ERR-SEVERITY
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               GO TO P2250-EXIT.
           IF WS-XR-PERIOD = ZERO
               MOVE WS-PE-SETTLE-PERIOD TO WS-XR-PERIOD.
           IF WS-XR-STATE = SPACES
               ADD 1 TO WS-CV-BAD-CNT
               MOVE EC-CIRCUIT-UNKNOWN TO WS-ERR-CODE
               MOVE 'W' TO WS-ERR-SEVERITY
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               SUBTRACT 1 FROM WS-REJECT-CNT.

       P2250-EXIT.
           EXIT.

       P2300-STATE-RULE.
      * LOOK UP THE STATE MEET POINT RULE.  THE ANSWER SAYS WHO IS
      * SUPPOSED TO CARRY A PERCENTAGE RESIDUAL IN THAT STATE.  THE
      * LOOKUP IS PERFORMED FOR EVERY CIRCUIT AND THE RESULT IS
      * CARRIED IN WS-SR-OWNER.  P3200 DOES NOT CONSULT IT.
           MOVE 'P2300-STATE-RULE' TO WS-PARA-NAME.
           MOVE 'N' TO WS-SR-FOUND-SW.
           MOVE 'U' TO WS-SR-OWNER.
           SET WS-MR-IX TO 1.
           SEARCH WS-MR-ENTRY
               AT END
                   GO TO P2300-EXIT
               WHEN WS-MR-STATE (WS-MR-IX) = WS-XR-STATE
                   MOVE WS-MR-RESID-OWNER (WS-MR-IX) TO WS-SR-OWNER
                   MOVE WS-MR-LIMIT (WS-MR-IX) TO WS-SR-LIMIT
                   MOVE WS-MR-ORDER-REQD (WS-MR-IX) TO WS-SR-ORDER
                   MOVE 'Y' TO WS-SR-FOUND-SW.

       P2300-EXIT.
           EXIT.


      *****************************************************************
      * S500-PRIOR-PERIOD                                             *
      * COMPARISON AGAINST THE PRIOR SETTLEMENT PERIOD.               *
      *****************************************************************
       S500-PRIOR-PERIOD SECTION.

       P5000-PRIOR-COMPARE.
      * COMPARE THIS PERIODS SPLIT WITH THE LAST ONE FOR THE SAME
      * CIRCUIT.  A LARGE SWING IS ALMOST ALWAYS A REFILED
      * PERCENTAGE RATHER THAN A GENUINE TRAFFIC MOVEMENT, AND IT
      * IS THE ONLY WARNING ANYBODY GETS THAT THE OTHER LEC HAS
      * CHANGED ITS FILING.
           MOVE 'P5000-PRIOR-COMPARE' TO WS-PARA-NAME.
           IF WS-PP-GROSS = ZERO
               GO TO P5000-EXIT.
           COMPUTE WS-PP-DELTA =
                   WS-MW-OUR-SHARE - WS-PP-OUR-SHARE.
           IF WS-PP-OUR-SHARE = ZERO
               GO TO P5000-EXIT.
           COMPUTE WS-PP-PCT-CHANGE ROUNDED =
                   (WS-PP-DELTA / WS-PP-OUR-SHARE) * 100.
           IF WS-PP-PCT-CHANGE < 0
               COMPUTE WS-PP-PCT-CHANGE = WS-PP-PCT-CHANGE * -1.
           IF WS-PP-PCT-CHANGE > 025.00000
               ADD 1 TO WS-PP-SWING-CNT
               MOVE EC-MPB-PCT-INVALID TO WS-ERR-CODE
               MOVE 'W' TO WS-ERR-SEVERITY
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               SUBTRACT 1 FROM WS-REJECT-CNT.
           ADD 1 TO WS-PP-COUNT.

       P5000-EXIT.
           EXIT.

       P5200-CARRY-FORWARD.
      * A CIRCUIT THAT CANNOT BE SETTLED THIS PERIOD IS CARRIED
      * FORWARD RATHER THAN DROPPED.  THE NEXT RUN WILL PICK IT UP
      * FROM THE EXTRACT AGAIN - NOTHING IS WRITTEN, THE COUNT IS
      * THE ONLY RECORD THAT IT HAPPENED.
           ADD 1 TO WS-CFWD-CNT.
           MOVE WS-MSG-TEXT (14) TO WS-D1-DISPO.
           PERFORM P6100-DETAIL THRU P6100-EXIT.

       P5200-EXIT.
           EXIT.

       P5400-SUMMARY-ACCUM.
      * ACCUMULATE THE PRIOR PERIOD FIGURES FOR THE NEXT COMPARE.
           MOVE WS-MW-GROSS TO WS-PP-GROSS.
           MOVE WS-MW-OUR-SHARE TO WS-PP-OUR-SHARE.
           MOVE WS-MW-THEIR-SHARE TO WS-PP-THEIR-SHARE.

       P5400-EXIT.
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
           MOVE WS-EXTRACT-RECORD TO SU-ORIG-RECORD.
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
           MOVE 200                    TO CT-STEP-SEQ.
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
           PERFORM P6300-TOTALS THRU P6300-EXIT.
           DISPLAY 'CIRCUITS SETTLED ' WS-SETTLE-CNT.
           DISPLAY 'NO REVENUE       ' WS-NOREV-CNT.
           DISPLAY 'PERCENT VARIANCE ' WS-VAR-CNT.
           DISPLAY 'RESIDUAL ABSORBED' WS-RESID-CNT.
           DISPLAY 'DEFAULT PERCENT  ' WS-DEFAULT-CNT.
           DISPLAY 'GROSS AMOUNT     ' WS-TOT-GROSS.
           DISPLAY 'OUR SHARE        ' WS-TOT-OURS.
           DISPLAY 'THEIR SHARE      ' WS-TOT-THEIRS.
           DISPLAY 'RESIDUAL AMOUNT  ' WS-TOT-RESIDUAL.
           DISPLAY 'TRUNCATION LOSS  ' WS-TOT-TRUNC.
           DISPLAY 'SPLIT MODULE CALLS' WS-CALL-COUNT.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE MPB-VALID-FILE
                 BILL-DETAIL-FILE
                 RATE-MASTER
                 CARRIER-MASTER
                 SETTLE-OUT-FILE
                 VARIANCE-FILE
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

