      *****************************************************************
      * CABJUR07 - RETROACTIVE FACTOR RESTATEMENT AND REPRICING       *
      * APPLICATION : CABS                                            *
      * INPUTS      : USGHIST  TELCABS.CABS.CDR.PLU(-1)       CABSCDR *
      * INPUTS      : BILLHIST TELCABS.CABS.BILLDTL.HIST(-3)  CABSBILL*
      * INPUTS      : FCTRVAL  TELCABS.CABS.FACTOR.VAL(0)     CABSFCTR*
      * INPUTS      : RATEMAST TELCABS.CABS.RATE              CABSRATE*
      * INPUTS      : SYSIN    INSTREAM                       NONE    *
      * OUTPUTS     : ADJOUT   TELCABS.CABS.RESTATE.ADJ(+1)   CABSBILL*
      * OUTPUTS     : AUDTOUT  TELCABS.CABS.RESTATE.AUDIT(+1) NONE    *
      * OUTPUTS     : REPORT   SYSOUT                         CABSPRNT*
      * CONTROL     : CTLOUT                         CABSCTL          *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARIS*
      *                       + CT-CARRIED-FWD                        *
      *               SUMMARISED = USAGE INSIDE THE WINDOW WITH A ZERO*
      *               DELTA.  CARRIED FWD = USAGE OUTSIDE THE WINDOW. *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY - SEE P1600     *
      * THIS IS THE HARDEST MODULE IN THE APPLICATION.  READ          *
      * THE NOTES IN S300 AND S500 BEFORE CHANGING ANYTHING.          *
      * STANDARDS   : CODED TO CABS-STD-014 (RECORD LAYOUTS) AND      *
      *               CABS-STD-058 (DATE HANDLING).                   *
      * REVISION HISTORY                                              *
      *   V1.00  1988-11-04  D.OKONKWO     INITIAL - PIU ONLY         *
      *   V1.02  1989-07-18  D.OKONKWO     ELEMENT APPORTIONMENT      *
      *   V1.05  1991-02-26  R.T.WHEELER   BUCKET TABLE ADDED         *
      *   V1.09  1993-08-30  D.OKONKWO     BUCKET LIMIT 200           *
      *   V2.00  1996-08-12  J.M.CASTILLO  Y2K - WINDOW REVIEWED      *
      *   V2.01  1997-05-06  J.M.CASTILLO  CROSS YEAR WINDOW FIXED    *
      *   V2.03  2000-12-14  P.NAIR        MATERIALITY TEST ADDED     *
      *   V2.05  2003-09-09  P.NAIR        PRIOR FACTOR FALLBACK      *
      *   V2.06  2006-06-01  A.BUKOWSKI    AUDIT TRAIL FILE ADDED     *
      *   V2.07  2010-04-20  A.BUKOWSKI    REASON CODE TABLE          *
      *   V2.08  2013-07-25  L.FERREIRA    DELTA ROUNDED AT FIVE      *
      *   V2.09  2017-11-08  L.FERREIRA    RECOMPILE ONLY LE V6       *
      *   V2.09  2019-06-17  M.OYELARAN    NO CODE CHANGE - AUDIT     *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CABJUR07.
       AUTHOR.        D.OKONKWO.
       DATE-WRITTEN.  1988-11-04.
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
      * PRIOR PERIOD PRICED USAGE - GDG MINUS ONE.  WHICH
           SELECT USAGE-HIST-FILE
               ASSIGN TO UT-S-USGHIST
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
      * BILLED DETAIL AS ISSUED - GDG MINUS THREE.  THE
           SELECT BILL-HIST-FILE
               ASSIGN TO UT-S-BILLHIST
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-TABLE.
      * NEW FACTORS CARRYING THE RESTATEMENT WINDOW
           SELECT FACTOR-FILE
               ASSIGN TO UT-S-FCTRVAL
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-SUSPENSE.
      * RATE MASTER - THE RATES THAT WERE IN EFFECT THEN
           SELECT RATE-MASTER
               ASSIGN TO DA-I-RATEMAST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS RTM-KEY
               FILE STATUS IS WS-FS-OUTPUT.
      * RESTATEMENT ADJUSTMENTS - POSTED BY CABJUR10
           SELECT ADJUST-OUT-FILE
               ASSIGN TO UT-S-ADJOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CONTROL.
      * AUDIT TRAIL - ONE RECORD PER REPRICED USAGE RECORD
           SELECT AUDIT-OUT-FILE
               ASSIGN TO UT-S-AUDTOUT
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
       FD  USAGE-HIST-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 200 CHARACTERS
               DATA RECORD IS UHI-RECORD.
       01  UHI-RECORD              PIC X(200).

       FD  BILL-HIST-FILE
               RECORDING MODE IS V
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 1647 CHARACTERS
               DATA RECORD IS BHI-RECORD.
       01  BHI-RECORD              PIC X(1647).

       FD  FACTOR-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 76 CHARACTERS
               DATA RECORD IS FVL-RECORD.
       01  FVL-RECORD              PIC X(76).

       FD  RATE-MASTER
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 400 CHARACTERS
               DATA RECORD IS RTM-RECORD.
       01  RTM-RECORD.
           05  RTM-KEY                 PIC X(18).
           05  RTM-DATA                PIC X(382).

       FD  ADJUST-OUT-FILE
               RECORDING MODE IS V
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 1647 CHARACTERS
               DATA RECORD IS ADO-RECORD.
       01  ADO-RECORD              PIC X(1647).

       FD  AUDIT-OUT-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 250 CHARACTERS
               DATA RECORD IS AUD-RECORD.
       01  AUD-RECORD              PIC X(250).

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
           05  WS-PGM-NAME         PIC X(08)         VALUE 'CABJUR07'.
           05  WS-PGM-VERSION        PIC X(05)           VALUE 'V2.09'.
           05  WS-PGM-APPL           PIC X(04)           VALUE 'CABS'.
           05  WS-PGM-COMPILE      PIC X(08)         VALUE '20190617'.
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

       COPY CABSBILL.

       COPY CABSRATE.

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
           05  WS-PE-WINDOW-DAYS       PIC 9(03).
           05  WS-PE-REASON-CD         PIC X(02).
           05  WS-PE-MATERIALITY       PIC 9(05)V9(02).
           05  WS-PE-OCN-SELECT        PIC X(04).
           05  WS-PE-SIM-SW            PIC X(01).
           05  WS-PE-FILLER            PIC X(18).
       01  WS-PARM-OLD REDEFINES WS-PARM-CARD.
      * THE 1989 CARD.  TWO REGIONS NEVER CONVERTED THEIR JCL AND
      * THE EDIT STILL ACCEPTS IT.  THE FIELDS ARE IN A DIFFERENT
      * ORDER AND THE CYCLE DATE IS SIX DIGITS YYMMDD, NOT YYDDD.
           05  FILLER                  PIC X(02).
           05  WS-PO-CYCLE             PIC 9(05).
           05  WS-PO-RUN-ID            PIC X(12).
           05  WS-PO-YYMMDD            PIC 9(06).
           05  WS-PO-WINDOW            PIC 9(03).
           05  WS-PO-REASON            PIC X(02).
           05  FILLER                  PIC X(20).

      * LOCAL SWITCHES.  THE STANDARD ONES ARRIVE VIA CABSWRK.
       01  WS-LOCAL-SWITCHES.
           05  WS-PARM-EOF-SW          PIC X(01)             VALUE 'N'.
                   88  WS-PARM-EOF             VALUE 'Y'.
           05  WS-FCTR-EOF-SW          PIC X(01)             VALUE 'N'.
                   88  WS-FCTR-EOF             VALUE 'Y'.
           05  WS-BILL-EOF-SW          PIC X(01)             VALUE 'N'.
                   88  WS-BILL-EOF             VALUE 'Y'.
           05  WS-IN-WINDOW-SW         PIC X(01)             VALUE 'N'.
                   88  WS-IN-WINDOW            VALUE 'Y'.
           05  WS-FACTOR-PAIR-SW       PIC X(01)             VALUE 'N'.
                   88  WS-PAIR-OK              VALUE 'Y'.
                   88  WS-PAIR-BAD             VALUE 'N'.
           05  WS-BREAK-SW             PIC X(01)             VALUE 'N'.
                   88  WS-BREAK-DETECTED       VALUE 'Y'.
           05  WS-MATERIAL-SW          PIC X(01)             VALUE 'N'.
                   88  WS-MATERIAL             VALUE 'Y'.
           05  WS-BILL-MATCH-SW        PIC X(01)             VALUE 'N'.
                   88  WS-BILL-MATCHED         VALUE 'Y'.
           05  WS-RESTART-DONE-SW      PIC X(01)             VALUE 'N'.
                   88  WS-RESTART-DONE         VALUE 'Y'.

      * SUBSCRIPTS AND INDEX WORK FIELDS.
       01  WS-SUBSCRIPTS.
           05  WS-SUB1                 PIC S9(05) COMP-3     VALUE 0.
           05  WS-SUB2                 PIC S9(05) COMP-3     VALUE 0.
           05  WS-SUB3                 PIC S9(05) COMP-3     VALUE 0.
           05  WS-SUB4                 PIC S9(05) COMP-3     VALUE 0.
           05  WS-IDX                  PIC S9(05) COMP-3     VALUE 0.
           05  WS-DEPTH                PIC S9(05) COMP-3     VALUE 0.
           05  WS-ELEM-SUB             PIC S9(05) COMP-3     VALUE 0.

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

      * RESTATEMENT ACCUMULATION BUCKETS.  ONE BUCKET PER CARRIER,
      * STATE AND JURISDICTION COMBINATION SEEN IN THE WINDOW.  THE
      * BUCKETS ARE FLUSHED TO THE ADJUSTMENT FILE AT EVERY CARRIER
      * BREAK AND AT END OF FILE.  TWO HUNDRED IS THE 1993 LIMIT -
      * THE TABLE HAS NEVER OVERFLOWED IN PRODUCTION BUT THE CHECK
      * IS THERE BECAUSE IT DID OVERFLOW DURING PARALLEL RUNNING.
       01  WS-BUCKET-TABLE.
           05  WS-BK-ENTRY OCCURS 200 TIMES
                   INDEXED BY WS-BK-IX.
               10  WS-BK-KEY.
                   15  WS-BK-OCN               PIC X(04).
                   15  WS-BK-STATE             PIC X(02).
                   15  WS-BK-JURIS             PIC X(01).
               10  WS-BK-MOU               PIC S9(15)V9(02) COMP-3.
               10  WS-BK-PRIOR-AMT         PIC S9(13)V9(05) COMP-3.
               10  WS-BK-NEW-AMT           PIC S9(13)V9(05) COMP-3.
               10  WS-BK-DELTA             PIC S9(13)V9(05) COMP-3.
               10  WS-BK-COUNT             PIC S9(09) COMP-3.
               10  WS-BK-PRIOR-PIU         PIC S9(03)V9(05) COMP-3.
               10  WS-BK-NEW-PIU           PIC S9(03)V9(05) COMP-3.
               10  WS-BK-REASON            PIC X(02).
       01  WS-BUCKET-CTL.
           05  WS-BK-COUNT-USED        PIC S9(05) COMP-3     VALUE 0.
           05  WS-BK-MAX               PIC S9(05) COMP-3     VALUE 200.
           05  WS-BK-FOUND-SW          PIC X(01)             VALUE 'N'.
                   88  WS-BK-FOUND              VALUE 'Y'.
           05  WS-BK-HIT               PIC S9(05) COMP-3     VALUE 0.

      * PER ELEMENT WORK TABLE.  A BILLED DETAIL LINE CARRIES UP TO
      * FORTY RATE ELEMENTS AND THE DELTA HAS TO BE APPORTIONED
      * ACROSS THEM IN PROPORTION TO THE ORIGINAL BILLED AMOUNT,
      * OTHERWISE THE ADJUSTMENT CANNOT BE TIED BACK TO A LINE.
       01  WS-ELEMENT-WORK.
           05  WS-EW-ENTRY OCCURS 40 TIMES
                   INDEXED BY WS-EW-IX.
               10  WS-EW-ELEM              PIC X(06).
               10  WS-EW-QTY               PIC S9(13)V9(02) COMP-3.
               10  WS-EW-RATE              PIC S9(05)V9(05) COMP-3.
               10  WS-EW-ORIG-AMT          PIC S9(11)V9(05) COMP-3.
               10  WS-EW-NEW-AMT           PIC S9(11)V9(05) COMP-3.
               10  WS-EW-DELTA             PIC S9(11)V9(05) COMP-3.
               10  WS-EW-SHARE             PIC S9(03)V9(07) COMP-3.
               10  WS-EW-ROUND             PIC X(01).
               10  WS-EW-PIU-APPL          PIC X(01).
       01  WS-ELEMENT-CTL.
           05  WS-EW-COUNT             PIC S9(03) COMP-3     VALUE 0.
           05  WS-EW-TOT-ORIG          PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-EW-TOT-NEW           PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-EW-TOT-DELTA         PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-EW-RESIDUE           PIC S9(07)V9(05) COMP-3 VALUE 0.

      * ADJUSTMENT REASON CODES.  THE CODE IS WRITTEN ONTO EVERY
      * ADJUSTMENT RECORD AND APPEARS ON THE CARRIER STATEMENT.
      * CODES 16 THROUGH 19 BELONG TO THE SETTLEMENT APPLICATION
      * AND ARE CARRIED HERE ONLY SO THAT THE PRINTED REGISTER CAN
      * DESCRIBE AN ADJUSTMENT RAISED BY EITHER APPLICATION.
       01  WS-REASON-CONST.
           05  FILLER              PIC X(40)
                   VALUE '01FACTOR REVISED BY CARRIER FILING      '.
           05  FILLER              PIC X(40)
                   VALUE '02FACTOR REVISED BY COMMISSION ORDER    '.
           05  FILLER              PIC X(40)
                   VALUE '03FACTOR CORRECTED - KEYING ERROR       '.
           05  FILLER              PIC X(40)
                   VALUE '04DISPUTE RESOLVED IN CARRIER FAVOUR    '.
           05  FILLER              PIC X(40)
                   VALUE '05DISPUTE RESOLVED IN COMPANY FAVOUR    '.
           05  FILLER              PIC X(40)
                   VALUE '06DEFAULT REPLACED BY FILED FACTOR      '.
           05  FILLER              PIC X(40)
                   VALUE '07STUDY DERIVED FACTOR SUPERSEDED       '.
           05  FILLER              PIC X(40)
                   VALUE '08TARIFF DEFAULT REVISED                '.
           05  FILLER              PIC X(40)
                   VALUE '09PRIOR PERIOD TRUE UP                  '.
           05  FILLER              PIC X(40)
                   VALUE '10AUDIT ADJUSTMENT                      '.
           05  FILLER              PIC X(40)
                   VALUE '11MERGER - OCN CONSOLIDATION            '.
           05  FILLER              PIC X(40)
                   VALUE '12JURISDICTION RECLASSIFIED             '.
           05  FILLER              PIC X(40)
                   VALUE '13LATA BOUNDARY CHANGE                  '.
           05  FILLER              PIC X(40)
                   VALUE '14RATE ELEMENT RETIRED                  '.
           05  FILLER              PIC X(40)
                   VALUE '15BILLING SYSTEM CONVERSION             '.
           05  FILLER              PIC X(40)
                   VALUE '16MEET POINT PERCENTAGE REVISED         '.
           05  FILLER              PIC X(40)
                   VALUE '17RECIPROCAL RATE REVISED               '.
           05  FILLER              PIC X(40)
                   VALUE '18ISP CAP REVISED                       '.
           05  FILLER              PIC X(40)
                   VALUE '19CMDS EXCHANGE CORRECTION              '.
           05  FILLER              PIC X(40)
                   VALUE '20MANUAL ADJUSTMENT REQUEST             '.
           05  FILLER              PIC X(40)
                   VALUE '21WITHDRAWN FILING                      '.
           05  FILLER              PIC X(40)
                   VALUE '22LATE FILING ACCEPTED                  '.
           05  FILLER              PIC X(40)
                   VALUE '23REGULATORY SETTLEMENT                 '.
           05  FILLER              PIC X(40)
                   VALUE '24REASON NOT SUPPLIED                   '.
       01  WS-REASON-TABLE REDEFINES WS-REASON-CONST.
           05  WS-WS-RS-ENTRY OCCURS 24 TIMES
                   INDEXED BY WS-RS-IX.
               10  WS-RS-CODE              PIC X(02).
               10  WS-RS-TEXT              PIC X(38).

      * THE RESTATEMENT WINDOW.  THE FROM DATE COMES FROM THE NEW
      * FACTOR RECORD.  THE THRU DATE IS DERIVED FROM IT USING THE
      * WINDOW LENGTH ON THE CONTROL CARD, THEN CAPPED BY THE THRU
      * DATE ON THE FACTOR RECORD IF THAT IS EARLIER.  BOTH ARE
      * HELD AS YYDDD.
       01  WS-RESTATE-WINDOW.
           05  WS-RS-FROM-YYDDD.
               10  WS-RS-FROM-YY           PIC 9(02)            VALUE 0.
               10  WS-RS-FROM-DDD          PIC 9(03)            VALUE 0.
           05  WS-RS-THRU-YYDDD.
               10  WS-RS-THRU-YY           PIC 9(02)            VALUE 0.
               10  WS-RS-THRU-DDD          PIC 9(03)            VALUE 0.
           05  WS-RS-SPAN-DAYS         PIC S9(05) COMP-3     VALUE 0.
           05  WS-RS-FROM-ABS          PIC S9(07) COMP-3     VALUE 0.
           05  WS-RS-THRU-ABS          PIC S9(07) COMP-3     VALUE 0.
           05  WS-RS-USE-ABS           PIC S9(07) COMP-3     VALUE 0.
           05  WS-RS-USE-YYDDD         PIC 9(05)             VALUE 0.
       01  WS-WINDOW-ALT REDEFINES WS-RESTATE-WINDOW.
           05  WS-WA-FROM-N            PIC 9(05).
           05  WS-WA-THRU-N            PIC 9(05).
           05  WS-WA-REST              PIC X(22).

      * THE CORE REPRICING WORK AREA.  THE ARITHMETIC IS
      *   PRIOR IS MOU = BASE MOU * PRIOR PIU / 100
      *   PRIOR SS MOU = BASE MOU - PRIOR IS MOU
      *   NEW   IS MOU = BASE MOU * NEW   PIU / 100
      *   NEW   SS MOU = BASE MOU - NEW   IS MOU
      *   PRIOR AMT    = PRIOR IS MOU * IS RATE
      *                + PRIOR SS MOU * SS RATE
      *   NEW   AMT    = NEW   IS MOU * IS RATE
      *                + NEW   SS MOU * SS RATE
      *   DELTA        = NEW AMT - PRIOR AMT
      * THE DELTA IS CARRIED AT FIVE DECIMAL PLACES ALL THE WAY
      * TO THE ADJUSTMENT RECORD.  IT IS ROUNDED TO TWO ONLY
      * WHEN THE BILL IS FORMATTED, IN CABFMT03, NOT HERE.
       01  WS-RESTATE-WORK.
           05  WS-RW-BASE-MOU          PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-RW-PRIOR-PIU         PIC S9(03)V9(05) COMP-3 VALUE 0.
           05  WS-RW-NEW-PIU           PIC S9(03)V9(05) COMP-3 VALUE 0.
           05  WS-RW-PRIOR-PLU         PIC S9(03)V9(05) COMP-3 VALUE 0.
           05  WS-RW-NEW-PLU           PIC S9(03)V9(05) COMP-3 VALUE 0.
           05  WS-RW-PR-IS-MOU         PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-RW-PR-SS-MOU         PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-RW-NW-IS-MOU         PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-RW-NW-SS-MOU         PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-RW-IS-RATE           PIC S9(05)V9(05) COMP-3 VALUE 0.
           05  WS-RW-SS-RATE           PIC S9(05)V9(05) COMP-3 VALUE 0.
           05  WS-RW-PRIOR-AMT         PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-RW-NEW-AMT           PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-RW-DELTA             PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-RW-DELTA-ABS         PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-RW-BILLED-AMT        PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-RW-VARIANCE          PIC S9(13)V9(05) COMP-3 VALUE 0.

      * RUN TOTALS.  DEBIT AND CREDIT ARE SEPARATED BECAUSE THE
      * GENERAL LEDGER INTERFACE POSTS THEM TO DIFFERENT ACCOUNTS
      * AND A NET FIGURE CANNOT BE SPLIT AFTER THE FACT.
       01  WS-RESTATE-TOTALS.
           05  WS-TOT-PRIOR-AMT        PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-TOT-NEW-AMT          PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-TOT-DELTA-AMT        PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-TOT-DEBIT-AMT        PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-TOT-CREDIT-AMT       PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-TOT-MOU              PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-INWIN-CNT            PIC S9(11) COMP-3     VALUE 0.
           05  WS-OUTWIN-CNT           PIC S9(11) COMP-3     VALUE 0.
           05  WS-REPRICE-CNT          PIC S9(11) COMP-3     VALUE 0.
           05  WS-ZERODELTA-CNT        PIC S9(11) COMP-3     VALUE 0.
           05  WS-NOBASIS-CNT          PIC S9(11) COMP-3     VALUE 0.
           05  WS-ADJUST-CNT           PIC S9(11) COMP-3     VALUE 0.
           05  WS-IMMATERIAL-CNT       PIC S9(11) COMP-3     VALUE 0.
           05  WS-BILLMATCH-CNT        PIC S9(11) COMP-3     VALUE 0.

      * CONTROL BREAK SAVE AREA.  THE USAGE FILE IS IN OCN, BAN,
      * SEQUENCE ORDER.  A CHANGE OF OCN FLUSHES THE BUCKETS.
       01  WS-SAVE-KEYS.
           05  WS-SV-OCN             PIC X(04)           VALUE SPACES.
           05  WS-SV-BAN             PIC X(13)           VALUE SPACES.
           05  WS-SV-STATE           PIC X(02)           VALUE SPACES.
           05  WS-SV-JURIS           PIC X(01)           VALUE SPACES.
           05  WS-SV-SEQ               PIC 9(09)             VALUE 0.
           05  WS-SV-FIRST-SW          PIC X(01)             VALUE 'Y'.
                   88  WS-SV-FIRST              VALUE 'Y'.
       01  WS-SAVE-AREAS.
           05  WS-SAVE-CDR           PIC X(200)          VALUE SPACES.
           05  WS-SAVE-BILL          PIC X(1204)         VALUE SPACES.
       01  WS-SAVE-BILL-R REDEFINES WS-SAVE-AREAS.
           05  WS-SB-CDR-AREA          PIC X(200).
           05  WS-SB-KEY               PIC X(28).
           05  WS-SB-REST              PIC X(1176).

      * ADJUSTMENT RECORD BUILD AREA.  THE ADJUSTMENT IS A BILL
      * DETAIL LINE WITH A NEGATIVE OR POSITIVE AMOUNT AND A
      * SECTION CODE OF RS.  IT IS BUILT HERE AND WRITTEN BY
      * P6100.  TWO REDEFINES GIVE THE POSTING PROGRAM AND THE
      * AUDIT WRITER THEIR OWN VIEWS OF THE SAME BYTES.
       01  WS-ADJUST-RECORD.
           05  WS-AR-BODY            PIC X(1204)         VALUE SPACES.
       01  WS-ADJUST-VIEW REDEFINES WS-ADJUST-RECORD.
           05  WS-AV-BAN               PIC X(13).
           05  WS-AV-PERIOD            PIC 9(06).
           05  WS-AV-SECTION           PIC X(02).
           05  WS-AV-REST              PIC X(1183).
       01  WS-ADJUST-POST REDEFINES WS-ADJUST-RECORD.
           05  WS-AP-KEY               PIC X(28).
           05  WS-AP-OCN               PIC X(04).
           05  WS-AP-JURIS             PIC X(01).
           05  WS-AP-STATE             PIC X(02).
           05  WS-AP-DESC              PIC X(60).
           05  WS-AP-REST              PIC X(1109).
       01  WS-AUDIT-RECORD.
           05  WS-AU-RUN-ID          PIC X(12)           VALUE SPACES.
           05  WS-AU-OCN             PIC X(04)           VALUE SPACES.
           05  WS-AU-BAN             PIC X(13)           VALUE SPACES.
           05  WS-AU-SEQ               PIC 9(09)             VALUE 0.
           05  WS-AU-USE-YYDDD         PIC 9(05)             VALUE 0.
           05  WS-AU-BASE-MOU          PIC S9(13)V9(02)      VALUE 0.
           05  WS-AU-PRIOR-PIU         PIC S9(03)V9(05)      VALUE 0.
           05  WS-AU-NEW-PIU           PIC S9(03)V9(05)      VALUE 0.
           05  WS-AU-PRIOR-AMT         PIC S9(13)V9(05)      VALUE 0.
           05  WS-AU-NEW-AMT           PIC S9(13)V9(05)      VALUE 0.
           05  WS-AU-DELTA             PIC S9(13)V9(05)      VALUE 0.
           05  WS-AU-REASON          PIC X(02)           VALUE SPACES.
           05  WS-AU-FILLER          PIC X(100)          VALUE SPACES.

      * JULIAN DATE WORK AREA - LOCAL TO CABJUR07.  THE SHARED AREA IN
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
                   VALUE 'RETROACTIVE FACTOR RESTATEMENT REGISTER'.
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
           05  FILLER              PIC X(50)
             VALUE 'OCN  ST J USE-DT   BASE-MOU     PRIOR-PIU  NEW-PIU'.
           05  FILLER              PIC X(37)
                   VALUE '   PRIOR-AMT      NEW-AMT       DELTA'.
       01  WS-HEAD-4.
           05  FILLER                  PIC X(01)             VALUE ' '.
           05  FILLER                PIC X(131)          VALUE ALL '-'.

      * DETAIL LINE WS-DETAIL-1.
       01  WS-DETAIL-1.
           05  FILLER                  PIC X(01)             VALUE ' '.
           05  WS-D1-OCN               PIC X(04).
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-STATE             PIC X(02).
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-JURIS             PIC X(01).
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-USEDT             PIC 9(05).
           05  FILLER                PIC X(02)           VALUE SPACES.
           05  WS-D1-MOU               PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-PRIOR-PIU         PIC ZZ9.99999.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-NEW-PIU           PIC ZZ9.99999.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-PRIOR-AMT         PIC ZZZ,ZZ9.99999.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-NEW-AMT           PIC ZZZ,ZZ9.99999.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-DELTA             PIC ZZZ,ZZ9.99999-.

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
                   VALUE 'USAGE INSIDE THE RESTATEMENT WINDOW         '.
           05  FILLER              PIC X(44)
                   VALUE 'USAGE OUTSIDE THE WINDOW - CARRIED FWD      '.
           05  FILLER              PIC X(44)
                   VALUE 'NO NEW FACTOR FOR THIS CARRIER              '.
           05  FILLER              PIC X(44)
                   VALUE 'NO PRIOR FACTOR - NO RESTATEMENT BASIS      '.
           05  FILLER              PIC X(44)
                   VALUE 'PRIOR AND NEW FACTOR IDENTICAL              '.
           05  FILLER              PIC X(44)
                   VALUE 'DELTA BELOW THE MATERIALITY THRESHOLD       '.
           05  FILLER              PIC X(44)
                   VALUE 'ADJUSTMENT RAISED - DEBIT                   '.
           05  FILLER              PIC X(44)
                   VALUE 'ADJUSTMENT RAISED - CREDIT                  '.
           05  FILLER              PIC X(44)
                   VALUE 'BILLED DETAIL NOT MATCHED - ESTIMATED       '.
           05  FILLER              PIC X(44)
                   VALUE 'BUCKET TABLE FULL - CARRIER SUMMARISED      '.
           05  FILLER              PIC X(44)
                   VALUE 'RATE NOT ON MASTER FOR THE USAGE DATE       '.
           05  FILLER              PIC X(44)
                   VALUE 'SIMULATION MODE - NOTHING WRITTEN           '.
           05  FILLER              PIC X(44)
                   VALUE 'WINDOW SPANS A YEAR BOUNDARY                '.
           05  FILLER              PIC X(44)
                   VALUE 'ELEMENT APPORTIONMENT RESIDUE APPLIED       '.
           05  FILLER              PIC X(44)
                   VALUE 'REPRICED AT FIVE DECIMAL PLACES             '.
           05  FILLER              PIC X(44)
                   VALUE 'USAGE RECORD NOT VOICE - IGNORED            '.
           05  FILLER              PIC X(44)
                   VALUE 'CARRIER NOT SELECTED BY CONTROL CARD        '.
           05  FILLER              PIC X(44)
                   VALUE 'PRIOR FACTOR TAKEN FROM CURRENT FACTOR      '.
           05  FILLER              PIC X(44)
                   VALUE 'DELTA SIGN REVERSED FOR CREDIT POSTING      '.
           05  FILLER              PIC X(44)
                   VALUE 'END OF RESTATEMENT RUN                      '.
       01  WS-MSG-TABLE REDEFINES WS-MSG-CONST.
           05  WS-MSG-ENTRY OCCURS 20 TIMES
                   INDEXED BY WS-MSG-IX.
               10  WS-MSG-TEXT             PIC X(44).

      * STATE RESTATEMENT RULE TABLE.  SOME COMMISSIONS REQUIRE AN
      * ORDER BEFORE A RETROACTIVE ADJUSTMENT MAY BE RAISED AND MOST
      * IMPOSE A LIMIT ON HOW FAR BACK ONE MAY REACH.  THE LIMITS
      * WERE LAST CONFIRMED IN 2004.  THE ACCESS MANAGEMENT GROUP
      * MAINTAINS A SPREADSHEET THAT DISAGREES WITH THIS TABLE IN
      * FOUR STATES.  NOBODY HAS RECONCILED THEM.
       01  WS-STRULE-CONST.
           05  FILLER              PIC X(10)         VALUE 'ALN0600025'.
           05  FILLER              PIC X(10)         VALUE 'AKN2400001'.
           05  FILLER              PIC X(10)         VALUE 'AZY1201000'.
           05  FILLER              PIC X(10)         VALUE 'ARN3601000'.
           05  FILLER              PIC X(10)         VALUE 'CAY1200025'.
           05  FILLER              PIC X(10)         VALUE 'CON0001000'.
           05  FILLER              PIC X(10)         VALUE 'CTY2400001'.
           05  FILLER              PIC X(10)         VALUE 'DEY0600100'.
           05  FILLER              PIC X(10)         VALUE 'FLY3600001'.
           05  FILLER              PIC X(10)         VALUE 'GAY1200001'.
           05  FILLER              PIC X(10)         VALUE 'HIN2401000'.
           05  FILLER              PIC X(10)         VALUE 'IDN1200001'.
           05  FILLER              PIC X(10)         VALUE 'ILN3601000'.
           05  FILLER              PIC X(10)         VALUE 'INY2400100'.
           05  FILLER              PIC X(10)         VALUE 'IAN1200025'.
           05  FILLER              PIC X(10)         VALUE 'KSY2400001'.
           05  FILLER              PIC X(10)         VALUE 'KYY2401000'.
           05  FILLER              PIC X(10)         VALUE 'LAN3600001'.
           05  FILLER              PIC X(10)         VALUE 'MEY1200025'.
           05  FILLER              PIC X(10)         VALUE 'MDY1200100'.
           05  FILLER              PIC X(10)         VALUE 'MAY1200001'.
           05  FILLER              PIC X(10)         VALUE 'MIY0600100'.
           05  FILLER              PIC X(10)         VALUE 'MNY2400001'.
           05  FILLER              PIC X(10)         VALUE 'MSN3600100'.
           05  FILLER              PIC X(10)         VALUE 'MON1200100'.
           05  FILLER              PIC X(10)         VALUE 'MTN2400100'.
           05  FILLER              PIC X(10)         VALUE 'NEY0600001'.
           05  FILLER              PIC X(10)         VALUE 'NVN1200025'.
           05  FILLER              PIC X(10)         VALUE 'NHN2401000'.
           05  FILLER              PIC X(10)         VALUE 'NJN0000025'.
           05  FILLER              PIC X(10)         VALUE 'NMN0000025'.
           05  FILLER              PIC X(10)         VALUE 'NYN3601000'.
           05  FILLER              PIC X(10)         VALUE 'NCN3600100'.
           05  FILLER              PIC X(10)         VALUE 'NDY3600025'.
           05  FILLER              PIC X(10)         VALUE 'OHN0600025'.
           05  FILLER              PIC X(10)         VALUE 'OKY0000100'.
           05  FILLER              PIC X(10)         VALUE 'ORY3600100'.
           05  FILLER              PIC X(10)         VALUE 'PAN3600025'.
           05  FILLER              PIC X(10)         VALUE 'RIN0601000'.
           05  FILLER              PIC X(10)         VALUE 'SCN3600100'.
           05  FILLER              PIC X(10)         VALUE 'SDN0000100'.
           05  FILLER              PIC X(10)         VALUE 'TNN3600001'.
           05  FILLER              PIC X(10)         VALUE 'TXY3601000'.
           05  FILLER              PIC X(10)         VALUE 'UTY0001000'.
           05  FILLER              PIC X(10)         VALUE 'VTY1200100'.
           05  FILLER              PIC X(10)         VALUE 'VAY1200025'.
           05  FILLER              PIC X(10)         VALUE 'WAN2400025'.
           05  FILLER              PIC X(10)         VALUE 'WVY2400025'.
           05  FILLER              PIC X(10)         VALUE 'WIN3601000'.
           05  FILLER              PIC X(10)         VALUE 'WYN1200001'.
           05  FILLER              PIC X(10)         VALUE 'DCY0600025'.
       01  WS-STRULE-TABLE REDEFINES WS-STRULE-CONST.
           05  WS-WS-SR-ENTRY OCCURS 51 TIMES
                   INDEXED BY WS-SR-IX.
               10  WS-SR-STATE             PIC X(02).
               10  WS-SR-ORDER-REQD        PIC X(01).
               10  WS-SR-MONTH-LIMIT       PIC 9(02).
               10  WS-SR-MATERIALITY       PIC 9(03)V9(02).

      * FALLBACK RATE MATRIX.  USED WHEN THE RATE MASTER HAS NO
      * RECORD FOR THE USAGE DATE - WHICH HAPPENS FOR ANY ELEMENT
      * RETIRED BETWEEN THE BILLING RUN AND THE RESTATEMENT RUN.
      * THESE RATES WERE COPIED FROM THE 1991 TARIFF AND HAVE NEVER
      * BEEN UPDATED.  A RESTATEMENT THAT FALLS BACK TO THIS TABLE
      * IS WRONG BY WHATEVER THE RATE HAS MOVED SINCE 1991.
       01  WS-RTDFLT-CONST.
           05  FILLER            PIC X(12)       VALUE 'CCLTRMI36174'.
           05  FILLER            PIC X(12)       VALUE 'CCLTRMS16749'.
           05  FILLER            PIC X(12)       VALUE 'CCLORGI97687'.
           05  FILLER            PIC X(12)       VALUE 'CCLORGS98889'.
           05  FILLER            PIC X(12)       VALUE 'LSWTCHI94132'.
           05  FILLER            PIC X(12)       VALUE 'LSWTCHS03074'.
           05  FILLER            PIC X(12)       VALUE 'TSWTCHI02244'.
           05  FILLER            PIC X(12)       VALUE 'TSWTCHS81965'.
           05  FILLER            PIC X(12)       VALUE 'TNDMSWI19360'.
           05  FILLER            PIC X(12)       VALUE 'TNDMSWS79144'.
           05  FILLER            PIC X(12)       VALUE 'LTRANSI16874'.
           05  FILLER            PIC X(12)       VALUE 'LTRANSS79813'.
           05  FILLER            PIC X(12)       VALUE 'ENTRANI81440'.
           05  FILLER            PIC X(12)       VALUE 'ENTRANS27826'.
           05  FILLER            PIC X(12)       VALUE 'COMTRNI39458'.
           05  FILLER            PIC X(12)       VALUE 'COMTRNS61695'.
           05  FILLER            PIC X(12)       VALUE 'DTTRANI28815'.
           05  FILLER            PIC X(12)       VALUE 'DTTRANS19193'.
           05  FILLER            PIC X(12)       VALUE 'LOCTRMI86381'.
           05  FILLER            PIC X(12)       VALUE 'LOCTRMS90571'.
           05  FILLER            PIC X(12)       VALUE 'LOCORGI74873'.
           05  FILLER            PIC X(12)       VALUE 'LOCORGS75908'.
           05  FILLER            PIC X(12)       VALUE '800DBQI84378'.
           05  FILLER            PIC X(12)       VALUE '800DBQS87626'.
           05  FILLER            PIC X(12)       VALUE 'SS7ISPI16689'.
           05  FILLER            PIC X(12)       VALUE 'SS7ISPS32474'.
           05  FILLER            PIC X(12)       VALUE 'QUERY1I48494'.
           05  FILLER            PIC X(12)       VALUE 'QUERY1S10507'.
           05  FILLER            PIC X(12)       VALUE 'DEDTRNI40975'.
           05  FILLER            PIC X(12)       VALUE 'DEDTRNS38614'.
           05  FILLER            PIC X(12)       VALUE 'SPCLACI61036'.
           05  FILLER            PIC X(12)       VALUE 'SPCLACS81496'.
           05  FILLER            PIC X(12)       VALUE 'DS1LOCI15991'.
           05  FILLER            PIC X(12)       VALUE 'DS1LOCS40751'.
           05  FILLER            PIC X(12)       VALUE 'DS3LOCI41220'.
           05  FILLER            PIC X(12)       VALUE 'DS3LOCS56025'.
           05  FILLER            PIC X(12)       VALUE 'UNEPRTI75967'.
           05  FILLER            PIC X(12)       VALUE 'UNEPRTS81263'.
           05  FILLER            PIC X(12)       VALUE 'UNELOPI19204'.
           05  FILLER            PIC X(12)       VALUE 'UNELOPS79839'.
           05  FILLER            PIC X(12)       VALUE 'COLLOCI22473'.
           05  FILLER            PIC X(12)       VALUE 'COLLOCS48866'.
           05  FILLER            PIC X(12)       VALUE 'MPBTRNI03811'.
           05  FILLER            PIC X(12)       VALUE 'MPBTRNS30754'.
           05  FILLER            PIC X(12)       VALUE 'RECIPTI06861'.
           05  FILLER            PIC X(12)       VALUE 'RECIPTS78490'.
           05  FILLER            PIC X(12)       VALUE 'ISPBNDI66589'.
           05  FILLER            PIC X(12)       VALUE 'ISPBNDS69657'.
           05  FILLER            PIC X(12)       VALUE 'TRANSPI17687'.
           05  FILLER            PIC X(12)       VALUE 'TRANSPS33885'.
           05  FILLER            PIC X(12)       VALUE 'TERMINI78422'.
           05  FILLER            PIC X(12)       VALUE 'TERMINS02683'.
           05  FILLER            PIC X(12)       VALUE 'ORIGINI75762'.
           05  FILLER            PIC X(12)       VALUE 'ORIGINS71905'.
           05  FILLER            PIC X(12)       VALUE 'MOUCHGI98642'.
           05  FILLER            PIC X(12)       VALUE 'MOUCHGS06815'.
           05  FILLER            PIC X(12)       VALUE 'SETUPCI76652'.
           05  FILLER            PIC X(12)       VALUE 'SETUPCS32671'.
           05  FILLER            PIC X(12)       VALUE 'MINCHGI38746'.
           05  FILLER            PIC X(12)       VALUE 'MINCHGS32128'.
           05  FILLER            PIC X(12)       VALUE 'CARCOMI55904'.
           05  FILLER            PIC X(12)       VALUE 'CARCOMS29075'.
           05  FILLER            PIC X(12)       VALUE 'LNKCHGI79675'.
           05  FILLER            PIC X(12)       VALUE 'LNKCHGS83542'.
           05  FILLER            PIC X(12)       VALUE 'DBQCHGI10230'.
           05  FILLER            PIC X(12)       VALUE 'DBQCHGS83206'.
           05  FILLER            PIC X(12)       VALUE 'PORTCHI25761'.
           05  FILLER            PIC X(12)       VALUE 'PORTCHS62093'.
           05  FILLER            PIC X(12)       VALUE 'XCONNCI69806'.
           05  FILLER            PIC X(12)       VALUE 'XCONNCS54382'.
           05  FILLER            PIC X(12)       VALUE 'ENTFACI39427'.
           05  FILLER            PIC X(12)       VALUE 'ENTFACS67273'.
           05  FILLER            PIC X(12)       VALUE 'TANDEMI96341'.
           05  FILLER            PIC X(12)       VALUE 'TANDEMS09230'.
           05  FILLER            PIC X(12)       VALUE 'ENDOFFI99547'.
           05  FILLER            PIC X(12)       VALUE 'ENDOFFS56004'.
           05  FILLER            PIC X(12)       VALUE 'SHRTRNI22524'.
           05  FILLER            PIC X(12)       VALUE 'SHRTRNS46685'.
           05  FILLER            PIC X(12)       VALUE 'WIRTRMI11449'.
           05  FILLER            PIC X(12)       VALUE 'WIRTRMS50474'.
       01  WS-RTDFLT-TABLE REDEFINES WS-RTDFLT-CONST.
           05  WS-WS-RD-ENTRY OCCURS 80 TIMES
                   INDEXED BY WS-RD-IX.
               10  WS-RD-ELEM              PIC X(06).
               10  WS-RD-JURIS             PIC X(01).
               10  WS-RD-RATE              PIC 9(00)V9(05).

      * OCN MERGER CROSS REFERENCE.  A RESTATEMENT REACHING BACK
      * ACROSS A MERGER HAS TO RAISE THE ADJUSTMENT AGAINST THE
      * SURVIVING OCN, NOT THE ONE THAT WAS BILLED AT THE TIME.
      * THE YEAR IS TWO DIGITS AND IS COMPARED AGAINST THE USAGE
      * YEAR WITH THE SAME PIVOT OF 70 USED EVERYWHERE ELSE.
      * DATE HANDLING PER CABS-STD-058 - REVIEWED AT Y2K.
       01  WS-MERGER-CONST.
           05  FILLER              PIC X(10)         VALUE '1038839006'.
           05  FILLER              PIC X(10)         VALUE '1374420601'.
           05  FILLER              PIC X(10)         VALUE '1447100899'.
           05  FILLER              PIC X(10)         VALUE '1904274203'.
           05  FILLER              PIC X(10)         VALUE '2053319697'.
           05  FILLER              PIC X(10)         VALUE '2068343797'.
           05  FILLER              PIC X(10)         VALUE '2238943099'.
           05  FILLER              PIC X(10)         VALUE '2278371511'.
           05  FILLER              PIC X(10)         VALUE '2447370811'.
           05  FILLER              PIC X(10)         VALUE '2466284111'.
           05  FILLER              PIC X(10)         VALUE '2561784901'.
           05  FILLER              PIC X(10)         VALUE '2779914506'.
           05  FILLER              PIC X(10)         VALUE '2801799497'.
           05  FILLER              PIC X(10)         VALUE '3270151501'.
           05  FILLER              PIC X(10)         VALUE '3342187999'.
           05  FILLER              PIC X(10)         VALUE '3476952514'.
           05  FILLER              PIC X(10)         VALUE '3652950118'.
           05  FILLER              PIC X(10)         VALUE '4311375501'.
           05  FILLER              PIC X(10)         VALUE '4371239699'.
           05  FILLER              PIC X(10)         VALUE '4414324903'.
           05  FILLER              PIC X(10)         VALUE '4589790597'.
           05  FILLER              PIC X(10)         VALUE '4642614899'.
           05  FILLER              PIC X(10)         VALUE '5069334497'.
           05  FILLER              PIC X(10)         VALUE '5150530503'.
           05  FILLER              PIC X(10)         VALUE '5165551606'.
           05  FILLER              PIC X(10)         VALUE '5216722501'.
           05  FILLER              PIC X(10)         VALUE '5253643518'.
           05  FILLER              PIC X(10)         VALUE '5274900799'.
           05  FILLER              PIC X(10)         VALUE '5298977314'.
           05  FILLER              PIC X(10)         VALUE '5308482401'.
           05  FILLER              PIC X(10)         VALUE '5627507803'.
           05  FILLER              PIC X(10)         VALUE '5657192514'.
           05  FILLER              PIC X(10)         VALUE '5695922197'.
           05  FILLER              PIC X(10)         VALUE '5971631014'.
           05  FILLER              PIC X(10)         VALUE '5989845506'.
           05  FILLER              PIC X(10)         VALUE '6660706518'.
           05  FILLER              PIC X(10)         VALUE '6968721914'.
           05  FILLER              PIC X(10)         VALUE '6973373011'.
           05  FILLER              PIC X(10)         VALUE '7065227506'.
           05  FILLER              PIC X(10)         VALUE '7511437499'.
           05  FILLER              PIC X(10)         VALUE '7546737318'.
           05  FILLER              PIC X(10)         VALUE '7604745901'.
           05  FILLER              PIC X(10)         VALUE '7615619403'.
           05  FILLER              PIC X(10)         VALUE '7685548501'.
           05  FILLER              PIC X(10)         VALUE '7747799497'.
           05  FILLER              PIC X(10)         VALUE '7766406311'.
           05  FILLER              PIC X(10)         VALUE '8109563199'.
           05  FILLER              PIC X(10)         VALUE '8291784603'.
           05  FILLER              PIC X(10)         VALUE '8563942903'.
           05  FILLER              PIC X(10)         VALUE '8670576811'.
           05  FILLER              PIC X(10)         VALUE '8753427603'.
           05  FILLER              PIC X(10)         VALUE '8907282206'.
           05  FILLER              PIC X(10)         VALUE '9034833901'.
           05  FILLER              PIC X(10)         VALUE '9042271918'.
           05  FILLER              PIC X(10)         VALUE '9349760006'.
           05  FILLER              PIC X(10)         VALUE '9413947799'.
           05  FILLER              PIC X(10)         VALUE '9571675603'.
           05  FILLER              PIC X(10)         VALUE '9629430403'.
           05  FILLER              PIC X(10)         VALUE '9723793518'.
           05  FILLER              PIC X(10)         VALUE '9927995111'.
       01  WS-MERGER-TABLE REDEFINES WS-MERGER-CONST.
           05  WS-WS-MG-ENTRY OCCURS 60 TIMES
                   INDEXED BY WS-MG-IX.
               10  WS-MG-OLD-OCN           PIC X(04).
               10  WS-MG-NEW-OCN           PIC X(04).
               10  WS-MG-YY                PIC 9(02).

      * VOLUME BAND TABLE.  A RESTATEMENT THAT CROSSES A VOLUME BAND
      * BOUNDARY CHANGES THE EFFECTIVE RATE AS WELL AS THE FACTOR.
      * THIS TABLE IS LOADED AND SEARCHED BUT THE BAND ADJUSTMENT
      * ITSELF WAS NEVER FINISHED - SEE P4600.
       01  WS-VOLBAND-CONST.
           05  FILLER  PIC X(23)  VALUE '00000000000000999928186'.
           05  FILLER  PIC X(23)  VALUE '00001000000001999997304'.
           05  FILLER  PIC X(23)  VALUE '00002000000002999997381'.
           05  FILLER  PIC X(23)  VALUE '00003000000003999957573'.
           05  FILLER  PIC X(23)  VALUE '00004000000004999932622'.
           05  FILLER  PIC X(23)  VALUE '00005000000005999981054'.
           05  FILLER  PIC X(23)  VALUE '00006000000006999948019'.
           05  FILLER  PIC X(23)  VALUE '00007000000007999966013'.
           05  FILLER  PIC X(23)  VALUE '00008000000008999992889'.
           05  FILLER  PIC X(23)  VALUE '00009000000009999973550'.
           05  FILLER  PIC X(23)  VALUE '00010000000010999955707'.
           05  FILLER  PIC X(23)  VALUE '00011000000011999951967'.
           05  FILLER  PIC X(23)  VALUE '00012000000012999914841'.
           05  FILLER  PIC X(23)  VALUE '00013000000013999987374'.
           05  FILLER  PIC X(23)  VALUE '00014000000014999917660'.
           05  FILLER  PIC X(23)  VALUE '00015000000015999950282'.
           05  FILLER  PIC X(23)  VALUE '00016000000016999980343'.
           05  FILLER  PIC X(23)  VALUE '00017000000017999954933'.
           05  FILLER  PIC X(23)  VALUE '00018000000018999910097'.
           05  FILLER  PIC X(23)  VALUE '00019000000019999950294'.
           05  FILLER  PIC X(23)  VALUE '00020000000020999966935'.
           05  FILLER  PIC X(23)  VALUE '00021000000021999928595'.
           05  FILLER  PIC X(23)  VALUE '00022000000022999927119'.
           05  FILLER  PIC X(23)  VALUE '00023000000023999947618'.
           05  FILLER  PIC X(23)  VALUE '00024000000024999910167'.
           05  FILLER  PIC X(23)  VALUE '00025000000025999965426'.
           05  FILLER  PIC X(23)  VALUE '00026000000026999963502'.
           05  FILLER  PIC X(23)  VALUE '00027000000027999956051'.
           05  FILLER  PIC X(23)  VALUE '00028000000028999968863'.
           05  FILLER  PIC X(23)  VALUE '00029000000029999909282'.
           05  FILLER  PIC X(23)  VALUE '00030000000030999906250'.
           05  FILLER  PIC X(23)  VALUE '00031000000031999927997'.
           05  FILLER  PIC X(23)  VALUE '00032000000032999963739'.
           05  FILLER  PIC X(23)  VALUE '00033000000033999942289'.
           05  FILLER  PIC X(23)  VALUE '00034000000034999963480'.
           05  FILLER  PIC X(23)  VALUE '00035000000035999922964'.
           05  FILLER  PIC X(23)  VALUE '00036000000036999984034'.
           05  FILLER  PIC X(23)  VALUE '00037000000037999920148'.
           05  FILLER  PIC X(23)  VALUE '00038000000038999957807'.
           05  FILLER  PIC X(23)  VALUE '00039000000039999903474'.
           05  FILLER  PIC X(23)  VALUE '00040000000040999945365'.
           05  FILLER  PIC X(23)  VALUE '00041000000041999959597'.
           05  FILLER  PIC X(23)  VALUE '00042000000042999971894'.
           05  FILLER  PIC X(23)  VALUE '00043000000043999975425'.
           05  FILLER  PIC X(23)  VALUE '00044000000044999908834'.
           05  FILLER  PIC X(23)  VALUE '00045000000045999973416'.
           05  FILLER  PIC X(23)  VALUE '00046000000046999994819'.
           05  FILLER  PIC X(23)  VALUE '00047000000047999956621'.
           05  FILLER  PIC X(23)  VALUE '00048000000048999945354'.
           05  FILLER  PIC X(23)  VALUE '00049000000049999991269'.
           05  FILLER  PIC X(23)  VALUE '00050000000050999991745'.
           05  FILLER  PIC X(23)  VALUE '00051000000051999917761'.
           05  FILLER  PIC X(23)  VALUE '00052000000052999900501'.
           05  FILLER  PIC X(23)  VALUE '00053000000053999980652'.
           05  FILLER  PIC X(23)  VALUE '00054000000054999964931'.
           05  FILLER  PIC X(23)  VALUE '00055000000055999947584'.
           05  FILLER  PIC X(23)  VALUE '00056000000056999991722'.
           05  FILLER  PIC X(23)  VALUE '00057000000057999987832'.
           05  FILLER  PIC X(23)  VALUE '00058000000058999961915'.
           05  FILLER  PIC X(23)  VALUE '00059000000059999919311'.
           05  FILLER  PIC X(23)  VALUE '00060000000060999970708'.
           05  FILLER  PIC X(23)  VALUE '00061000000061999922960'.
           05  FILLER  PIC X(23)  VALUE '00062000000062999986344'.
           05  FILLER  PIC X(23)  VALUE '00063000000063999999718'.
           05  FILLER  PIC X(23)  VALUE '00064000000064999971803'.
           05  FILLER  PIC X(23)  VALUE '00065000000065999945950'.
           05  FILLER  PIC X(23)  VALUE '00066000000066999990188'.
           05  FILLER  PIC X(23)  VALUE '00067000000067999973784'.
           05  FILLER  PIC X(23)  VALUE '00068000000068999931345'.
           05  FILLER  PIC X(23)  VALUE '00069000000069999903571'.
           05  FILLER  PIC X(23)  VALUE '00070000000070999940080'.
           05  FILLER  PIC X(23)  VALUE '00071000000071999903794'.
           05  FILLER  PIC X(23)  VALUE '00072000000072999914325'.
           05  FILLER  PIC X(23)  VALUE '00073000000073999913661'.
           05  FILLER  PIC X(23)  VALUE '00074000000074999971435'.
           05  FILLER  PIC X(23)  VALUE '00075000000075999988138'.
           05  FILLER  PIC X(23)  VALUE '00076000000076999910341'.
           05  FILLER  PIC X(23)  VALUE '00077000000077999995955'.
           05  FILLER  PIC X(23)  VALUE '00078000000078999956630'.
           05  FILLER  PIC X(23)  VALUE '00079000000079999957343'.
           05  FILLER  PIC X(23)  VALUE '00080000000080999954549'.
           05  FILLER  PIC X(23)  VALUE '00081000000081999913724'.
           05  FILLER  PIC X(23)  VALUE '00082000000082999914568'.
           05  FILLER  PIC X(23)  VALUE '00083000000083999910433'.
           05  FILLER  PIC X(23)  VALUE '00084000000084999977147'.
           05  FILLER  PIC X(23)  VALUE '00085000000085999912583'.
           05  FILLER  PIC X(23)  VALUE '00086000000086999999206'.
           05  FILLER  PIC X(23)  VALUE '00087000000087999954937'.
           05  FILLER  PIC X(23)  VALUE '00088000000088999956870'.
           05  FILLER  PIC X(23)  VALUE '00089000000089999923024'.
           05  FILLER  PIC X(23)  VALUE '00090000000090999927532'.
           05  FILLER  PIC X(23)  VALUE '00091000000091999988137'.
           05  FILLER  PIC X(23)  VALUE '00092000000092999941808'.
           05  FILLER  PIC X(23)  VALUE '00093000000093999975636'.
           05  FILLER  PIC X(23)  VALUE '00094000000094999990916'.
           05  FILLER  PIC X(23)  VALUE '00095000000095999934653'.
       01  WS-VOLBAND-TABLE REDEFINES WS-VOLBAND-CONST.
           05  WS-WS-VB-ENTRY OCCURS 96 TIMES
                   INDEXED BY WS-VB-IX.
               10  WS-VB-FROM              PIC 9(09).
               10  WS-VB-THRU              PIC 9(09).
               10  WS-VB-PCT               PIC 9(00)V9(05).

      * PLU RESTATEMENT WORK AREA.  A FACTOR FILING REVISES BOTH THE
      * PIU AND THE PLU.  THE PLU RESTATEMENT REPRICES THE LOCAL AND
      * TOLL SPLIT OF THE INTRASTATE MINUTES AND PRODUCES ITS OWN
      * DELTA, WHICH IS ADDED TO THE SAME BUCKET AS THE PIU DELTA
      * BECAUSE THE CARRIER SEES ONE ADJUSTMENT LINE, NOT TWO.
       01  WS-PLU-RESTATE.
           05  WS-PR-BASE-MOU          PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-PR-PR-LC-MOU         PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-PR-PR-TL-MOU         PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-PR-NW-LC-MOU         PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-PR-NW-TL-MOU         PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-PR-LC-RATE           PIC S9(05)V9(05) COMP-3 VALUE 0.
           05  WS-PR-TL-RATE           PIC S9(05)V9(05) COMP-3 VALUE 0.
           05  WS-PR-PRIOR-AMT         PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-PR-NEW-AMT           PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-PR-DELTA             PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-PR-TOT-DELTA         PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-PR-COUNT             PIC S9(11) COMP-3     VALUE 0.

      * PER STATE SUMMARY TABLE FOR THE END OF RUN SUMMARY PAGE.
       01  WS-SUMMARY-TABLE.
           05  WS-SM-ENTRY OCCURS 60 TIMES
                   INDEXED BY WS-SM-IX.
               10  WS-SM-STATE             PIC X(02).
               10  WS-SM-MOU               PIC S9(15)V9(02) COMP-3.
               10  WS-SM-PRIOR             PIC S9(15)V9(05) COMP-3.
               10  WS-SM-NEW               PIC S9(15)V9(05) COMP-3.
               10  WS-SM-DELTA             PIC S9(15)V9(05) COMP-3.
               10  WS-SM-COUNT             PIC S9(11) COMP-3.
       01  WS-SUMMARY-CTL.
           05  WS-SM-USED              PIC S9(03) COMP-3     VALUE 0.
           05  WS-SM-HIT               PIC S9(03) COMP-3     VALUE 0.
           05  WS-SM-FOUND-SW          PIC X(01)             VALUE 'N'.
                   88  WS-SM-FOUND              VALUE 'Y'.
       01  WS-SEQUENCE-CHECK.
           05  WS-SQ-LAST-OCN      PIC X(04)         VALUE LOW-VALUES.
           05  WS-SQ-LAST-BAN      PIC X(13)         VALUE LOW-VALUES.
           05  WS-SQ-LAST-SEQ          PIC 9(09)             VALUE 0.
           05  WS-SQ-OUT-CNT           PIC S9(09) COMP-3     VALUE 0.
           05  WS-SQ-DUP-CNT           PIC S9(09) COMP-3     VALUE 0.
       01  WS-MERGER-WORK.
           05  WS-MW-BILLED-OCN      PIC X(04)           VALUE SPACES.
           05  WS-MW-SURVIVE-OCN     PIC X(04)           VALUE SPACES.
           05  WS-MW-MERGE-YY          PIC 9(02)             VALUE 0.
           05  WS-MW-USE-YY            PIC 9(02)             VALUE 0.
           05  WS-MW-MERGE-CCYY        PIC 9(04)             VALUE 0.
           05  WS-MW-USE-CCYY          PIC 9(04)             VALUE 0.
           05  WS-MW-APPLIED-CNT       PIC S9(09) COMP-3     VALUE 0.

      * CARRIER IDENTIFICATION CODE TO OCN CROSS REFERENCE.  THE
      * USAGE RECORD CARRIES THE CIC THAT THE SWITCH RECORDED.  THE
      * BILL IS RAISED AGAINST THE OCN.  A CIC THAT IS NOT ON THIS
      * TABLE CANNOT BE BILLED AND THE USAGE GOES TO SUSPENSE - THE
      * SINGLE LARGEST SOURCE OF SUSPENDED USAGE IN THE ESTATE.
       01  WS-CICXRF-CONST.
           05  FILLER              PIC X(09)         VALUE '01779186C'.
           05  FILLER              PIC X(09)         VALUE '02475038I'.
           05  FILLER              PIC X(09)         VALUE '02806962L'.
           05  FILLER              PIC X(09)         VALUE '03263762I'.
           05  FILLER              PIC X(09)         VALUE '03367794C'.
           05  FILLER              PIC X(09)         VALUE '04292420L'.
           05  FILLER              PIC X(09)         VALUE '04496044R'.
           05  FILLER              PIC X(09)         VALUE '05075915R'.
           05  FILLER              PIC X(09)         VALUE '05891421C'.
           05  FILLER              PIC X(09)         VALUE '06583898R'.
           05  FILLER              PIC X(09)         VALUE '08166878C'.
           05  FILLER              PIC X(09)         VALUE '09711784C'.
           05  FILLER              PIC X(09)         VALUE '11337266C'.
           05  FILLER              PIC X(09)         VALUE '11356152C'.
           05  FILLER              PIC X(09)         VALUE '11664512L'.
           05  FILLER              PIC X(09)         VALUE '11797732R'.
           05  FILLER              PIC X(09)         VALUE '12046451R'.
           05  FILLER              PIC X(09)         VALUE '12158422L'.
           05  FILLER              PIC X(09)         VALUE '13408320I'.
           05  FILLER              PIC X(09)         VALUE '13513493L'.
           05  FILLER              PIC X(09)         VALUE '13712814L'.
           05  FILLER              PIC X(09)         VALUE '14495898L'.
           05  FILLER              PIC X(09)         VALUE '15509126L'.
           05  FILLER              PIC X(09)         VALUE '16323821L'.
           05  FILLER              PIC X(09)         VALUE '17587827I'.
           05  FILLER              PIC X(09)         VALUE '17612758R'.
           05  FILLER              PIC X(09)         VALUE '17788950L'.
           05  FILLER              PIC X(09)         VALUE '17976433L'.
           05  FILLER              PIC X(09)         VALUE '18037862L'.
           05  FILLER              PIC X(09)         VALUE '18461986C'.
           05  FILLER              PIC X(09)         VALUE '18598103L'.
           05  FILLER              PIC X(09)         VALUE '19263888C'.
           05  FILLER              PIC X(09)         VALUE '19467783I'.
           05  FILLER              PIC X(09)         VALUE '19625737I'.
           05  FILLER              PIC X(09)         VALUE '19664697R'.
           05  FILLER              PIC X(09)         VALUE '19876002I'.
           05  FILLER              PIC X(09)         VALUE '20388499I'.
           05  FILLER              PIC X(09)         VALUE '22656396R'.
           05  FILLER              PIC X(09)         VALUE '22802729R'.
           05  FILLER              PIC X(09)         VALUE '23822441L'.
           05  FILLER              PIC X(09)         VALUE '23962511C'.
           05  FILLER              PIC X(09)         VALUE '24367802I'.
           05  FILLER              PIC X(09)         VALUE '24862202L'.
           05  FILLER              PIC X(09)         VALUE '26717886C'.
           05  FILLER              PIC X(09)         VALUE '26957529L'.
           05  FILLER              PIC X(09)         VALUE '27126534C'.
           05  FILLER              PIC X(09)         VALUE '27555005L'.
           05  FILLER              PIC X(09)         VALUE '28065174I'.
           05  FILLER              PIC X(09)         VALUE '28093387I'.
           05  FILLER              PIC X(09)         VALUE '28759656L'.
           05  FILLER              PIC X(09)         VALUE '28761125R'.
           05  FILLER              PIC X(09)         VALUE '29186733I'.
           05  FILLER              PIC X(09)         VALUE '29587219C'.
           05  FILLER              PIC X(09)         VALUE '29952508L'.
           05  FILLER              PIC X(09)         VALUE '30358768C'.
           05  FILLER              PIC X(09)         VALUE '31402436R'.
           05  FILLER              PIC X(09)         VALUE '32255662R'.
           05  FILLER              PIC X(09)         VALUE '32692824R'.
           05  FILLER              PIC X(09)         VALUE '33186544C'.
           05  FILLER              PIC X(09)         VALUE '33428894I'.
           05  FILLER              PIC X(09)         VALUE '35636427R'.
           05  FILLER              PIC X(09)         VALUE '36834984L'.
           05  FILLER              PIC X(09)         VALUE '37433279C'.
           05  FILLER              PIC X(09)         VALUE '38182183L'.
           05  FILLER              PIC X(09)         VALUE '38539960L'.
           05  FILLER              PIC X(09)         VALUE '38907621R'.
           05  FILLER              PIC X(09)         VALUE '39243965R'.
           05  FILLER              PIC X(09)         VALUE '39267670C'.
           05  FILLER              PIC X(09)         VALUE '40112398I'.
           05  FILLER              PIC X(09)         VALUE '41809269I'.
           05  FILLER              PIC X(09)         VALUE '42055481L'.
           05  FILLER              PIC X(09)         VALUE '42272821R'.
           05  FILLER              PIC X(09)         VALUE '44285682C'.
           05  FILLER              PIC X(09)         VALUE '44319911L'.
           05  FILLER              PIC X(09)         VALUE '44409233I'.
           05  FILLER              PIC X(09)         VALUE '44453379C'.
           05  FILLER              PIC X(09)         VALUE '44965226I'.
           05  FILLER              PIC X(09)         VALUE '45387085L'.
           05  FILLER              PIC X(09)         VALUE '45945783I'.
           05  FILLER              PIC X(09)         VALUE '46055912R'.
           05  FILLER              PIC X(09)         VALUE '46305160I'.
           05  FILLER              PIC X(09)         VALUE '46607905I'.
           05  FILLER              PIC X(09)         VALUE '47053063R'.
           05  FILLER              PIC X(09)         VALUE '47674587I'.
           05  FILLER              PIC X(09)         VALUE '48036724R'.
           05  FILLER              PIC X(09)         VALUE '48864797C'.
           05  FILLER              PIC X(09)         VALUE '48932187C'.
           05  FILLER              PIC X(09)         VALUE '49337255C'.
           05  FILLER              PIC X(09)         VALUE '49421954I'.
           05  FILLER              PIC X(09)         VALUE '50169942L'.
           05  FILLER              PIC X(09)         VALUE '51045280R'.
           05  FILLER              PIC X(09)         VALUE '51416896R'.
           05  FILLER              PIC X(09)         VALUE '51859548C'.
           05  FILLER              PIC X(09)         VALUE '52262619R'.
           05  FILLER              PIC X(09)         VALUE '52486795L'.
           05  FILLER              PIC X(09)         VALUE '52749314C'.
           05  FILLER              PIC X(09)         VALUE '54414956R'.
           05  FILLER              PIC X(09)         VALUE '54942367L'.
           05  FILLER              PIC X(09)         VALUE '55349063L'.
           05  FILLER              PIC X(09)         VALUE '55629475C'.
           05  FILLER              PIC X(09)         VALUE '56122097R'.
           05  FILLER              PIC X(09)         VALUE '56334081C'.
           05  FILLER              PIC X(09)         VALUE '58712324C'.
           05  FILLER              PIC X(09)         VALUE '58802282I'.
           05  FILLER              PIC X(09)         VALUE '58848822I'.
           05  FILLER              PIC X(09)         VALUE '59294683L'.
           05  FILLER              PIC X(09)         VALUE '59791852R'.
           05  FILLER              PIC X(09)         VALUE '59807727C'.
           05  FILLER              PIC X(09)         VALUE '59829747I'.
           05  FILLER              PIC X(09)         VALUE '61722356R'.
           05  FILLER              PIC X(09)         VALUE '63196020R'.
           05  FILLER              PIC X(09)         VALUE '63281535L'.
           05  FILLER              PIC X(09)         VALUE '63772017I'.
           05  FILLER              PIC X(09)         VALUE '64171180L'.
           05  FILLER              PIC X(09)         VALUE '64692680I'.
           05  FILLER              PIC X(09)         VALUE '65595395I'.
           05  FILLER              PIC X(09)         VALUE '65676329C'.
           05  FILLER              PIC X(09)         VALUE '65979811I'.
           05  FILLER              PIC X(09)         VALUE '67024222I'.
           05  FILLER              PIC X(09)         VALUE '67387901R'.
           05  FILLER              PIC X(09)         VALUE '67613705I'.
           05  FILLER              PIC X(09)         VALUE '68067660C'.
           05  FILLER              PIC X(09)         VALUE '68564420C'.
           05  FILLER              PIC X(09)         VALUE '68773121I'.
           05  FILLER              PIC X(09)         VALUE '68857866C'.
           05  FILLER              PIC X(09)         VALUE '69257387I'.
           05  FILLER              PIC X(09)         VALUE '71361457I'.
           05  FILLER              PIC X(09)         VALUE '73332935R'.
           05  FILLER              PIC X(09)         VALUE '73461931I'.
           05  FILLER              PIC X(09)         VALUE '73838100C'.
           05  FILLER              PIC X(09)         VALUE '74857532R'.
           05  FILLER              PIC X(09)         VALUE '75816220I'.
           05  FILLER              PIC X(09)         VALUE '76756832I'.
           05  FILLER              PIC X(09)         VALUE '76781997L'.
           05  FILLER              PIC X(09)         VALUE '76921810C'.
           05  FILLER              PIC X(09)         VALUE '77614327R'.
           05  FILLER              PIC X(09)         VALUE '77761749L'.
           05  FILLER              PIC X(09)         VALUE '78586911R'.
           05  FILLER              PIC X(09)         VALUE '78641164C'.
           05  FILLER              PIC X(09)         VALUE '79002202R'.
           05  FILLER              PIC X(09)         VALUE '79045474I'.
           05  FILLER              PIC X(09)         VALUE '80428773C'.
           05  FILLER              PIC X(09)         VALUE '80557551R'.
           05  FILLER              PIC X(09)         VALUE '81156000L'.
           05  FILLER              PIC X(09)         VALUE '82134753L'.
           05  FILLER              PIC X(09)         VALUE '82606795C'.
           05  FILLER              PIC X(09)         VALUE '82795819L'.
           05  FILLER              PIC X(09)         VALUE '83295487C'.
           05  FILLER              PIC X(09)         VALUE '83567833L'.
           05  FILLER              PIC X(09)         VALUE '83859633R'.
           05  FILLER              PIC X(09)         VALUE '83992653I'.
           05  FILLER              PIC X(09)         VALUE '84117548L'.
           05  FILLER              PIC X(09)         VALUE '84245250I'.
           05  FILLER              PIC X(09)         VALUE '84268027C'.
           05  FILLER              PIC X(09)         VALUE '85102131C'.
           05  FILLER              PIC X(09)         VALUE '85491034C'.
           05  FILLER              PIC X(09)         VALUE '85902714L'.
           05  FILLER              PIC X(09)         VALUE '86024654R'.
           05  FILLER              PIC X(09)         VALUE '86654017L'.
           05  FILLER              PIC X(09)         VALUE '86723479L'.
           05  FILLER              PIC X(09)         VALUE '88186425R'.
           05  FILLER              PIC X(09)         VALUE '88362562C'.
           05  FILLER              PIC X(09)         VALUE '89269175C'.
           05  FILLER              PIC X(09)         VALUE '89293750C'.
           05  FILLER              PIC X(09)         VALUE '89403786I'.
           05  FILLER              PIC X(09)         VALUE '89494441R'.
           05  FILLER              PIC X(09)         VALUE '89598310C'.
           05  FILLER              PIC X(09)         VALUE '89625433I'.
           05  FILLER              PIC X(09)         VALUE '90007502L'.
           05  FILLER              PIC X(09)         VALUE '90153856L'.
           05  FILLER              PIC X(09)         VALUE '90242244I'.
           05  FILLER              PIC X(09)         VALUE '90481419R'.
           05  FILLER              PIC X(09)         VALUE '90626991R'.
           05  FILLER              PIC X(09)         VALUE '91787884I'.
           05  FILLER              PIC X(09)         VALUE '91805901L'.
           05  FILLER              PIC X(09)         VALUE '92003108R'.
           05  FILLER              PIC X(09)         VALUE '92503722R'.
           05  FILLER              PIC X(09)         VALUE '92578641I'.
           05  FILLER              PIC X(09)         VALUE '93764540I'.
           05  FILLER              PIC X(09)         VALUE '94311556R'.
           05  FILLER              PIC X(09)         VALUE '94594362L'.
           05  FILLER              PIC X(09)         VALUE '94734541L'.
           05  FILLER              PIC X(09)         VALUE '96157818C'.
           05  FILLER              PIC X(09)         VALUE '96812446I'.
           05  FILLER              PIC X(09)         VALUE '97007468R'.
           05  FILLER              PIC X(09)         VALUE '97649058I'.
           05  FILLER              PIC X(09)         VALUE '97795940C'.
           05  FILLER              PIC X(09)         VALUE '97952222R'.
           05  FILLER              PIC X(09)         VALUE '98442376C'.
           05  FILLER              PIC X(09)         VALUE '98798811R'.
       01  WS-CICXRF-TABLE REDEFINES WS-CICXRF-CONST.
           05  WS-WS-CX-ENTRY OCCURS 190 TIMES
                   INDEXED BY WS-CX-IX.
               10  WS-CX-CIC               PIC 9(04).
               10  WS-CX-OCN               PIC X(04).
               10  WS-CX-TYPE              PIC X(01).

      * RATE ELEMENT DESCRIPTIONS AS THEY PRINT ON THE BILL.  THE
      * TEXT IS TARIFF LANGUAGE AND MAY NOT BE CHANGED WITHOUT A
      * TARIFF FILING.  THE LAST ENTRY IS FOR AN ELEMENT THAT HAS
      * NOT BEEN BILLED SINCE 2011.
       01  WS-ELEMDS-CONST.
           05  FILLER              PIC X(40)
                   VALUE 'CCLTRMCARRIER COMMON LINE TERMINATING   '.
           05  FILLER              PIC X(40)
                   VALUE 'CCLORGCARRIER COMMON LINE ORIGINATING   '.
           05  FILLER              PIC X(40)
                   VALUE 'LSWTCHLOCAL SWITCHING                   '.
           05  FILLER              PIC X(40)
                   VALUE 'TSWTCHTANDEM SWITCHING                  '.
           05  FILLER              PIC X(40)
                   VALUE 'TNDMSWTANDEM SWITCHED TRANSPORT         '.
           05  FILLER              PIC X(40)
                   VALUE 'LTRANSLOCAL TRANSPORT                   '.
           05  FILLER              PIC X(40)
                   VALUE 'ENTRANENTRANCE FACILITY                 '.
           05  FILLER              PIC X(40)
                   VALUE 'COMTRNCOMMON TRANSPORT                  '.
           05  FILLER              PIC X(40)
                   VALUE 'DTTRANDEDICATED TRANSPORT               '.
           05  FILLER              PIC X(40)
                   VALUE 'LOCTRMLOCAL TERMINATION                 '.
           05  FILLER              PIC X(40)
                   VALUE 'LOCORGLOCAL ORIGINATION                 '.
           05  FILLER              PIC X(40)
                   VALUE '800DBQTOLL FREE DATABASE QUERY          '.
           05  FILLER              PIC X(40)
                   VALUE 'SS7ISPSS7 ISUP SIGNALLING               '.
           05  FILLER              PIC X(40)
                   VALUE 'QUERY1DATABASE QUERY BASIC              '.
           05  FILLER              PIC X(40)
                   VALUE 'DEDTRNDEDICATED TRANSPORT MILEAGE       '.
           05  FILLER              PIC X(40)
                   VALUE 'SPCLACSPECIAL ACCESS CHANNEL            '.
           05  FILLER              PIC X(40)
                   VALUE 'DS1LOCDS1 LOCAL CHANNEL                 '.
           05  FILLER              PIC X(40)
                   VALUE 'DS3LOCDS3 LOCAL CHANNEL                 '.
           05  FILLER              PIC X(40)
                   VALUE 'UNEPRTUNBUNDLED PORT                    '.
           05  FILLER              PIC X(40)
                   VALUE 'UNELOPUNBUNDLED LOOP                    '.
           05  FILLER              PIC X(40)
                   VALUE 'COLLOCCOLLOCATION CHARGE                '.
           05  FILLER              PIC X(40)
                   VALUE 'MPBTRNMEET POINT TRANSPORT              '.
           05  FILLER              PIC X(40)
                   VALUE 'RECIPTRECIPROCAL TERMINATION            '.
           05  FILLER              PIC X(40)
                   VALUE 'ISPBNDISP BOUND TERMINATION             '.
           05  FILLER              PIC X(40)
                   VALUE 'TRANSPTRANSPORT INTEROFFICE             '.
           05  FILLER              PIC X(40)
                   VALUE 'TERMINTERMINATION SWITCHED              '.
           05  FILLER              PIC X(40)
                   VALUE 'ORIGINORIGINATION SWITCHED              '.
           05  FILLER              PIC X(40)
                   VALUE 'MOUCHGMINUTE OF USE CHARGE              '.
           05  FILLER              PIC X(40)
                   VALUE 'SETUPCCALL SETUP CHARGE                 '.
           05  FILLER              PIC X(40)
                   VALUE 'MINCHGMINIMUM MONTHLY CHARGE            '.
           05  FILLER              PIC X(40)
                   VALUE 'CARCOMCARRIER COMMON CHARGE             '.
           05  FILLER              PIC X(40)
                   VALUE 'LNKCHGLINK CHARGE                       '.
           05  FILLER              PIC X(40)
                   VALUE 'DBQCHGDATABASE QUERY CHARGE             '.
           05  FILLER              PIC X(40)
                   VALUE 'PORTCHNUMBER PORTABILITY CHARGE         '.
           05  FILLER              PIC X(40)
                   VALUE 'XCONNCCROSS CONNECT CHARGE              '.
           05  FILLER              PIC X(40)
                   VALUE 'ENTFACENTRANCE FACILITY MILEAGE         '.
           05  FILLER              PIC X(40)
                   VALUE 'TANDEMTANDEM SWITCHED FACILITY          '.
           05  FILLER              PIC X(40)
                   VALUE 'ENDOFFEND OFFICE SWITCHING              '.
           05  FILLER              PIC X(40)
                   VALUE 'SHRTRNSHARED TRANSPORT                  '.
           05  FILLER              PIC X(40)
                   VALUE 'WIRTRMWIRELESS TERMINATION              '.
       01  WS-ELEMDS-TABLE REDEFINES WS-ELEMDS-CONST.
           05  WS-WS-ED-ENTRY OCCURS 40 TIMES
                   INDEXED BY WS-ED-IX.
               10  WS-ED-ELEM              PIC X(06).
               10  WS-ED-DESC              PIC X(34).

      * LATA PAIR OVERRIDE MATRIX.  A SMALL NUMBER OF LATA PAIRS DO
      * NOT FOLLOW THE STATE BOUNDARY RULE - CORRIDOR ARRANGEMENTS,
      * THE NEW YORK NEW JERSEY CORRIDOR BEING THE OLDEST.  WHEN A
      * PAIR APPEARS HERE THE JURISDICTION IN THIS TABLE WINS OVER
      * THE ONE DERIVED FROM THE STATE CODES.  THE TABLE IS SEARCHED
      * SERIALLY - IT IS NOT IN ANY PARTICULAR ORDER.
       01  WS-LATAMX-CONST.
           05  FILLER                PIC X(07)          VALUE '674898I'.
           05  FILLER                PIC X(07)          VALUE '611719S'.
           05  FILLER                PIC X(07)          VALUE '793499S'.
           05  FILLER                PIC X(07)          VALUE '629516I'.
           05  FILLER                PIC X(07)          VALUE '270911S'.
           05  FILLER                PIC X(07)          VALUE '233437L'.
           05  FILLER                PIC X(07)          VALUE '456761I'.
           05  FILLER                PIC X(07)          VALUE '213865L'.
           05  FILLER                PIC X(07)          VALUE '405723S'.
           05  FILLER                PIC X(07)          VALUE '222841S'.
           05  FILLER                PIC X(07)          VALUE '953968S'.
           05  FILLER                PIC X(07)          VALUE '528679L'.
           05  FILLER                PIC X(07)          VALUE '801894I'.
           05  FILLER                PIC X(07)          VALUE '719761I'.
           05  FILLER                PIC X(07)          VALUE '427684L'.
           05  FILLER                PIC X(07)          VALUE '547905L'.
           05  FILLER                PIC X(07)          VALUE '614660S'.
           05  FILLER                PIC X(07)          VALUE '530486I'.
           05  FILLER                PIC X(07)          VALUE '514152L'.
           05  FILLER                PIC X(07)          VALUE '439701I'.
           05  FILLER                PIC X(07)          VALUE '531135S'.
           05  FILLER                PIC X(07)          VALUE '440631L'.
           05  FILLER                PIC X(07)          VALUE '486224L'.
           05  FILLER                PIC X(07)          VALUE '670314I'.
           05  FILLER                PIC X(07)          VALUE '695565I'.
           05  FILLER                PIC X(07)          VALUE '709254S'.
           05  FILLER                PIC X(07)          VALUE '774795S'.
           05  FILLER                PIC X(07)          VALUE '695842I'.
           05  FILLER                PIC X(07)          VALUE '266485S'.
           05  FILLER                PIC X(07)          VALUE '794297S'.
           05  FILLER                PIC X(07)          VALUE '248152L'.
           05  FILLER                PIC X(07)          VALUE '438805L'.
           05  FILLER                PIC X(07)          VALUE '128765L'.
           05  FILLER                PIC X(07)          VALUE '881956S'.
           05  FILLER                PIC X(07)          VALUE '886638L'.
           05  FILLER                PIC X(07)          VALUE '510304S'.
           05  FILLER                PIC X(07)          VALUE '851517I'.
           05  FILLER                PIC X(07)          VALUE '186475L'.
           05  FILLER                PIC X(07)          VALUE '314760L'.
           05  FILLER                PIC X(07)          VALUE '172407L'.
           05  FILLER                PIC X(07)          VALUE '275538L'.
           05  FILLER                PIC X(07)          VALUE '491381L'.
           05  FILLER                PIC X(07)          VALUE '340528L'.
           05  FILLER                PIC X(07)          VALUE '847951L'.
           05  FILLER                PIC X(07)          VALUE '489931I'.
           05  FILLER                PIC X(07)          VALUE '650654L'.
           05  FILLER                PIC X(07)          VALUE '281804L'.
           05  FILLER                PIC X(07)          VALUE '697407I'.
           05  FILLER                PIC X(07)          VALUE '725795L'.
           05  FILLER                PIC X(07)          VALUE '430521L'.
           05  FILLER                PIC X(07)          VALUE '343305L'.
           05  FILLER                PIC X(07)          VALUE '664625S'.
           05  FILLER                PIC X(07)          VALUE '890969I'.
           05  FILLER                PIC X(07)          VALUE '526556I'.
           05  FILLER                PIC X(07)          VALUE '870480L'.
           05  FILLER                PIC X(07)          VALUE '748370S'.
           05  FILLER                PIC X(07)          VALUE '183751I'.
           05  FILLER                PIC X(07)          VALUE '762809I'.
           05  FILLER                PIC X(07)          VALUE '758491L'.
           05  FILLER                PIC X(07)          VALUE '509599L'.
           05  FILLER                PIC X(07)          VALUE '521683S'.
           05  FILLER                PIC X(07)          VALUE '412806I'.
           05  FILLER                PIC X(07)          VALUE '759758S'.
           05  FILLER                PIC X(07)          VALUE '478752I'.
           05  FILLER                PIC X(07)          VALUE '956366S'.
           05  FILLER                PIC X(07)          VALUE '861248I'.
           05  FILLER                PIC X(07)          VALUE '554454S'.
           05  FILLER                PIC X(07)          VALUE '291440I'.
           05  FILLER                PIC X(07)          VALUE '864693I'.
           05  FILLER                PIC X(07)          VALUE '755146I'.
           05  FILLER                PIC X(07)          VALUE '122398L'.
           05  FILLER                PIC X(07)          VALUE '541429L'.
           05  FILLER                PIC X(07)          VALUE '714230I'.
           05  FILLER                PIC X(07)          VALUE '165654I'.
           05  FILLER                PIC X(07)          VALUE '246214I'.
           05  FILLER                PIC X(07)          VALUE '917814S'.
           05  FILLER                PIC X(07)          VALUE '747563I'.
           05  FILLER                PIC X(07)          VALUE '846367S'.
           05  FILLER                PIC X(07)          VALUE '450262S'.
           05  FILLER                PIC X(07)          VALUE '831563L'.
           05  FILLER                PIC X(07)          VALUE '890956L'.
           05  FILLER                PIC X(07)          VALUE '397513I'.
           05  FILLER                PIC X(07)          VALUE '126260I'.
           05  FILLER                PIC X(07)          VALUE '720896I'.
           05  FILLER                PIC X(07)          VALUE '883713L'.
           05  FILLER                PIC X(07)          VALUE '755855L'.
           05  FILLER                PIC X(07)          VALUE '408967L'.
           05  FILLER                PIC X(07)          VALUE '900715L'.
           05  FILLER                PIC X(07)          VALUE '443722L'.
           05  FILLER                PIC X(07)          VALUE '921623L'.
           05  FILLER                PIC X(07)          VALUE '143705S'.
           05  FILLER                PIC X(07)          VALUE '969138S'.
           05  FILLER                PIC X(07)          VALUE '672856L'.
           05  FILLER                PIC X(07)          VALUE '522391S'.
           05  FILLER                PIC X(07)          VALUE '787390S'.
           05  FILLER                PIC X(07)          VALUE '181123L'.
           05  FILLER                PIC X(07)          VALUE '916810L'.
           05  FILLER                PIC X(07)          VALUE '760673L'.
           05  FILLER                PIC X(07)          VALUE '843269L'.
           05  FILLER                PIC X(07)          VALUE '350926S'.
           05  FILLER                PIC X(07)          VALUE '306506L'.
           05  FILLER                PIC X(07)          VALUE '316407L'.
           05  FILLER                PIC X(07)          VALUE '834394I'.
           05  FILLER                PIC X(07)          VALUE '273629I'.
           05  FILLER                PIC X(07)          VALUE '826765L'.
           05  FILLER                PIC X(07)          VALUE '289588I'.
           05  FILLER                PIC X(07)          VALUE '458513L'.
           05  FILLER                PIC X(07)          VALUE '718355S'.
           05  FILLER                PIC X(07)          VALUE '406738S'.
           05  FILLER                PIC X(07)          VALUE '848281S'.
           05  FILLER                PIC X(07)          VALUE '624520I'.
           05  FILLER                PIC X(07)          VALUE '316130S'.
           05  FILLER                PIC X(07)          VALUE '300920S'.
           05  FILLER                PIC X(07)          VALUE '717690I'.
           05  FILLER                PIC X(07)          VALUE '786928S'.
           05  FILLER                PIC X(07)          VALUE '621420L'.
           05  FILLER                PIC X(07)          VALUE '907366S'.
           05  FILLER                PIC X(07)          VALUE '172858L'.
           05  FILLER                PIC X(07)          VALUE '899775S'.
           05  FILLER                PIC X(07)          VALUE '546718S'.
           05  FILLER                PIC X(07)          VALUE '557765L'.
           05  FILLER                PIC X(07)          VALUE '734617S'.
           05  FILLER                PIC X(07)          VALUE '441608I'.
           05  FILLER                PIC X(07)          VALUE '213296L'.
           05  FILLER                PIC X(07)          VALUE '166751I'.
           05  FILLER                PIC X(07)          VALUE '789582S'.
           05  FILLER                PIC X(07)          VALUE '236752L'.
           05  FILLER                PIC X(07)          VALUE '388245I'.
           05  FILLER                PIC X(07)          VALUE '623943I'.
           05  FILLER                PIC X(07)          VALUE '733246I'.
           05  FILLER                PIC X(07)          VALUE '357976S'.
           05  FILLER                PIC X(07)          VALUE '634385S'.
           05  FILLER                PIC X(07)          VALUE '859385I'.
           05  FILLER                PIC X(07)          VALUE '304213S'.
           05  FILLER                PIC X(07)          VALUE '908680S'.
           05  FILLER                PIC X(07)          VALUE '450867I'.
           05  FILLER                PIC X(07)          VALUE '726572L'.
           05  FILLER                PIC X(07)          VALUE '506221L'.
           05  FILLER                PIC X(07)          VALUE '318927L'.
           05  FILLER                PIC X(07)          VALUE '569180S'.
           05  FILLER                PIC X(07)          VALUE '673352L'.
           05  FILLER                PIC X(07)          VALUE '576696S'.
           05  FILLER                PIC X(07)          VALUE '627901I'.
           05  FILLER                PIC X(07)          VALUE '787358I'.
           05  FILLER                PIC X(07)          VALUE '286198S'.
           05  FILLER                PIC X(07)          VALUE '873848S'.
           05  FILLER                PIC X(07)          VALUE '217560I'.
           05  FILLER                PIC X(07)          VALUE '271135I'.
           05  FILLER                PIC X(07)          VALUE '325853L'.
           05  FILLER                PIC X(07)          VALUE '814884L'.
           05  FILLER                PIC X(07)          VALUE '443854S'.
           05  FILLER                PIC X(07)          VALUE '477513S'.
           05  FILLER                PIC X(07)          VALUE '961185I'.
           05  FILLER                PIC X(07)          VALUE '911445I'.
           05  FILLER                PIC X(07)          VALUE '433513L'.
           05  FILLER                PIC X(07)          VALUE '323725L'.
           05  FILLER                PIC X(07)          VALUE '222491S'.
           05  FILLER                PIC X(07)          VALUE '494187S'.
           05  FILLER                PIC X(07)          VALUE '383135I'.
           05  FILLER                PIC X(07)          VALUE '917874I'.
           05  FILLER                PIC X(07)          VALUE '122309L'.
           05  FILLER                PIC X(07)          VALUE '265788S'.
           05  FILLER                PIC X(07)          VALUE '527217L'.
           05  FILLER                PIC X(07)          VALUE '673818L'.
           05  FILLER                PIC X(07)          VALUE '680742I'.
           05  FILLER                PIC X(07)          VALUE '976790S'.
           05  FILLER                PIC X(07)          VALUE '866729S'.
           05  FILLER                PIC X(07)          VALUE '626849I'.
           05  FILLER                PIC X(07)          VALUE '350126L'.
           05  FILLER                PIC X(07)          VALUE '490517I'.
           05  FILLER                PIC X(07)          VALUE '148320S'.
           05  FILLER                PIC X(07)          VALUE '472912S'.
           05  FILLER                PIC X(07)          VALUE '907931I'.
           05  FILLER                PIC X(07)          VALUE '502819I'.
           05  FILLER                PIC X(07)          VALUE '762757L'.
           05  FILLER                PIC X(07)          VALUE '530417L'.
           05  FILLER                PIC X(07)          VALUE '781368S'.
           05  FILLER                PIC X(07)          VALUE '425973I'.
           05  FILLER                PIC X(07)          VALUE '828812L'.
           05  FILLER                PIC X(07)          VALUE '688219L'.
       01  WS-LATAMX-TABLE REDEFINES WS-LATAMX-CONST.
           05  WS-WS-LM-ENTRY OCCURS 180 TIMES
                   INDEXED BY WS-LM-IX.
               10  WS-LM-A-LATA            PIC 9(03).
               10  WS-LM-Z-LATA            PIC 9(03).
               10  WS-LM-JURIS             PIC X(01).

      * SPECIAL ACCESS RESTATEMENT WORK AREA.  SPECIAL ACCESS DOES
      * NOT CARRY MINUTES - IT IS A MONTHLY RECURRING CHARGE WITH A
      * MEET POINT PERCENTAGE.  A REVISED MEET POINT PERCENTAGE
      * RESTATES IT THE SAME WAY A REVISED PIU RESTATES USAGE, AND
      * THE SETTLEMENT APPLICATION RAISES THE MATCHING ENTRY.
       01  WS-SPECIAL-RESTATE.
           05  WS-SA-QTY               PIC S9(09)V9(02) COMP-3 VALUE 0.
           05  WS-SA-MPB-PCT           PIC S9(03)V9(05) COMP-3 VALUE 0.
           05  WS-SA-PRIOR-AMT         PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-SA-NEW-AMT           PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-SA-DELTA             PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-SA-TOT-DELTA         PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-SA-COUNT             PIC S9(11) COMP-3     VALUE 0.
           05  WS-SA-RATE              PIC S9(07)V9(05) COMP-3 VALUE 0.

      * EXCEPTION REGISTER COUNTS.  ONE COUNT PER MESSAGE TABLE
      * ENTRY.  THE REGISTER PRINTS EVERY NON ZERO COUNT.
       01  WS-EXCEPTION-COUNTS.
           05  WS-XC-ENTRY OCCURS 20 TIMES
                   INDEXED BY WS-XC-IX.
               10  WS-XC-COUNT             PIC S9(09) COMP-3.
       01  WS-EXCEPTION-CTL.
           05  WS-XC-SUB               PIC S9(03) COMP-3     VALUE 1.
           05  WS-XC-TOTAL             PIC S9(11) COMP-3     VALUE 0.
       01  WS-CIC-WORK.
           05  WS-CW-CIC               PIC 9(04)             VALUE 0.
           05  WS-CW-OCN             PIC X(04)           VALUE SPACES.
           05  WS-CW-FOUND-SW          PIC X(01)             VALUE 'N'.
                   88  WS-CW-FOUND              VALUE 'Y'.
           05  WS-CW-MISMATCH-CNT      PIC S9(09) COMP-3     VALUE 0.

      * HOLD FIELDS FOR THE STATE RULE AND BAND SEARCHES.
       01  WS-RULE-HOLD.
           05  WS-SR-HOLD-LIM          PIC 9(02)             VALUE 0.
           05  WS-SR-HOLD-ORD          PIC X(01)             VALUE 'N'.
           05  WS-SR-HOLD-MONTHS       PIC S9(05) COMP-3     VALUE 0.
           05  WS-SR-BARRED-CNT        PIC S9(09) COMP-3     VALUE 0.
           05  WS-VB-HOLD-PCT          PIC S9(03)V9(05) COMP-3 VALUE 0.

      * ELEMENT DESCRIPTION HOLD AREA AND SPECIAL ACCESS PERCENT.
       01  WS-DESC-HOLD.
           05  WS-ED-HOLD            PIC X(34)           VALUE SPACES.
           05  WS-SA-NEW-PCT           PIC S9(03)V9(05) COMP-3 VALUE 0.

      * BILLING ACCOUNT NUMBER TO CARRIER AND STATE CROSS REFERENCE.
      * THE STATE CODE ON THIS TABLE IS WHAT DECIDES WHICH FACTOR
      * APPLIES - THE USAGE RECORD DOES NOT CARRY A STATE.  THE
      * TABLE IS A SNAPSHOT TAKEN IN 2006 AND MAINTAINED BY HAND
      * SINCE.  A BAN OPENED AFTER 2006 THAT IS NOT ON IT PICKS UP
      * A BLANK STATE AND THEREFORE THE CARRIER LEVEL FACTOR.
       01  WS-BANXRF-CONST.
           05  FILLER  PIC X(19)  VALUE '11414881732593759OH'.
           05  FILLER  PIC X(19)  VALUE '11671994130626188MA'.
           05  FILLER  PIC X(19)  VALUE '11688450731739613NH'.
           05  FILLER  PIC X(19)  VALUE '12081739507065307AR'.
           05  FILLER  PIC X(19)  VALUE '12084168018084395MA'.
           05  FILLER  PIC X(19)  VALUE '12255344412146055ID'.
           05  FILLER  PIC X(19)  VALUE '12506498791711263MT'.
           05  FILLER  PIC X(19)  VALUE '13263278952693713KY'.
           05  FILLER  PIC X(19)  VALUE '13449603284264370OH'.
           05  FILLER  PIC X(19)  VALUE '14101818618943907MT'.
           05  FILLER  PIC X(19)  VALUE '14229896830664248OK'.
           05  FILLER  PIC X(19)  VALUE '16585856217593048FL'.
           05  FILLER  PIC X(19)  VALUE '17219056188554147VT'.
           05  FILLER  PIC X(19)  VALUE '17949781381911838NE'.
           05  FILLER  PIC X(19)  VALUE '18141539124121892AK'.
           05  FILLER  PIC X(19)  VALUE '19282357149881289MA'.
           05  FILLER  PIC X(19)  VALUE '19739974193683421VA'.
           05  FILLER  PIC X(19)  VALUE '19948063093102169IA'.
           05  FILLER  PIC X(19)  VALUE '19982437983582746NH'.
           05  FILLER  PIC X(19)  VALUE '20760888217545785OR'.
           05  FILLER  PIC X(19)  VALUE '21142462442778994DE'.
           05  FILLER  PIC X(19)  VALUE '21151157253865220DE'.
           05  FILLER  PIC X(19)  VALUE '21317467269297357AL'.
           05  FILLER  PIC X(19)  VALUE '21391952578945170AR'.
           05  FILLER  PIC X(19)  VALUE '21930123452238030NH'.
           05  FILLER  PIC X(19)  VALUE '21966370853571530MI'.
           05  FILLER  PIC X(19)  VALUE '22041767408491443SC'.
           05  FILLER  PIC X(19)  VALUE '22876828454851377SC'.
           05  FILLER  PIC X(19)  VALUE '23741843523436361NM'.
           05  FILLER  PIC X(19)  VALUE '23742642394103187HI'.
           05  FILLER  PIC X(19)  VALUE '24103279494906977ND'.
           05  FILLER  PIC X(19)  VALUE '24300591300061929IN'.
           05  FILLER  PIC X(19)  VALUE '24487400849796496WV'.
           05  FILLER  PIC X(19)  VALUE '24507089825405829NM'.
           05  FILLER  PIC X(19)  VALUE '24850442047892573MO'.
           05  FILLER  PIC X(19)  VALUE '25218996602071160CA'.
           05  FILLER  PIC X(19)  VALUE '25787774617243482OK'.
           05  FILLER  PIC X(19)  VALUE '25898666372527202TX'.
           05  FILLER  PIC X(19)  VALUE '26021926696222403LA'.
           05  FILLER  PIC X(19)  VALUE '27540947657837845IA'.
           05  FILLER  PIC X(19)  VALUE '27845922040043265KS'.
           05  FILLER  PIC X(19)  VALUE '27895500520851009KS'.
           05  FILLER  PIC X(19)  VALUE '27902743687868431FL'.
           05  FILLER  PIC X(19)  VALUE '27965346502796081TN'.
           05  FILLER  PIC X(19)  VALUE '28581537045172588SC'.
           05  FILLER  PIC X(19)  VALUE '30505237082597004UT'.
           05  FILLER  PIC X(19)  VALUE '30794383581102461AK'.
           05  FILLER  PIC X(19)  VALUE '31034157329504196ME'.
           05  FILLER  PIC X(19)  VALUE '31129640483052868CT'.
           05  FILLER  PIC X(19)  VALUE '32011176833734843NJ'.
           05  FILLER  PIC X(19)  VALUE '32619001662858158ME'.
           05  FILLER  PIC X(19)  VALUE '32693415289449693WA'.
           05  FILLER  PIC X(19)  VALUE '33177700022743320MI'.
           05  FILLER  PIC X(19)  VALUE '33595480500801717AK'.
           05  FILLER  PIC X(19)  VALUE '34256451831604141ND'.
           05  FILLER  PIC X(19)  VALUE '34962246340498486MS'.
           05  FILLER  PIC X(19)  VALUE '35181781884573605NM'.
           05  FILLER  PIC X(19)  VALUE '35540253324755001AK'.
           05  FILLER  PIC X(19)  VALUE '37044421275005469PA'.
           05  FILLER  PIC X(19)  VALUE '37198199623904623WY'.
           05  FILLER  PIC X(19)  VALUE '37275453421224404NC'.
           05  FILLER  PIC X(19)  VALUE '37342887140633899ID'.
           05  FILLER  PIC X(19)  VALUE '37389794651957760NE'.
           05  FILLER  PIC X(19)  VALUE '37455867242033857MT'.
           05  FILLER  PIC X(19)  VALUE '37811424781202295WA'.
           05  FILLER  PIC X(19)  VALUE '38204880083236759NC'.
           05  FILLER  PIC X(19)  VALUE '39852780866946374AL'.
           05  FILLER  PIC X(19)  VALUE '40152398951314323IN'.
           05  FILLER  PIC X(19)  VALUE '40645560690735780MN'.
           05  FILLER  PIC X(19)  VALUE '40818689944264177GA'.
           05  FILLER  PIC X(19)  VALUE '40823204010387951MO'.
           05  FILLER  PIC X(19)  VALUE '41106366706621480MD'.
           05  FILLER  PIC X(19)  VALUE '41638859910454625SD'.
           05  FILLER  PIC X(19)  VALUE '42189730809434701OR'.
           05  FILLER  PIC X(19)  VALUE '42853269488127757KY'.
           05  FILLER  PIC X(19)  VALUE '43221931970212799NV'.
           05  FILLER  PIC X(19)  VALUE '43558664957595036VA'.
           05  FILLER  PIC X(19)  VALUE '43691780832528859UT'.
           05  FILLER  PIC X(19)  VALUE '43832437752771765NY'.
           05  FILLER  PIC X(19)  VALUE '44014758191615077MI'.
           05  FILLER  PIC X(19)  VALUE '44064587364525552HI'.
           05  FILLER  PIC X(19)  VALUE '44952615076456182NC'.
           05  FILLER  PIC X(19)  VALUE '45014907591371714MD'.
           05  FILLER  PIC X(19)  VALUE '45035895750513528GA'.
           05  FILLER  PIC X(19)  VALUE '45559109942077341CT'.
           05  FILLER  PIC X(19)  VALUE '45715970093881410UT'.
           05  FILLER  PIC X(19)  VALUE '45763466307581804IN'.
           05  FILLER  PIC X(19)  VALUE '45953812377325857ID'.
           05  FILLER  PIC X(19)  VALUE '46264741564196343MD'.
           05  FILLER  PIC X(19)  VALUE '46810551188948802VT'.
           05  FILLER  PIC X(19)  VALUE '46833873921158312MS'.
           05  FILLER  PIC X(19)  VALUE '47233991376861513SD'.
           05  FILLER  PIC X(19)  VALUE '47838706387754894CO'.
           05  FILLER  PIC X(19)  VALUE '47902385698414414CT'.
           05  FILLER  PIC X(19)  VALUE '47984209203258110MN'.
           05  FILLER  PIC X(19)  VALUE '48251494840295573ME'.
           05  FILLER  PIC X(19)  VALUE '48392643877485741UT'.
           05  FILLER  PIC X(19)  VALUE '48628629896723674AZ'.
           05  FILLER  PIC X(19)  VALUE '48720258297984025CA'.
           05  FILLER  PIC X(19)  VALUE '48770232613546413AZ'.
           05  FILLER  PIC X(19)  VALUE '49048406938676649MA'.
           05  FILLER  PIC X(19)  VALUE '49245595886503424IL'.
           05  FILLER  PIC X(19)  VALUE '49350921071683497TX'.
           05  FILLER  PIC X(19)  VALUE '49478260736487002CO'.
           05  FILLER  PIC X(19)  VALUE '50177487002512410WY'.
           05  FILLER  PIC X(19)  VALUE '50465352302243550IL'.
           05  FILLER  PIC X(19)  VALUE '50496930173327651NM'.
           05  FILLER  PIC X(19)  VALUE '51229995291002200NC'.
           05  FILLER  PIC X(19)  VALUE '51700762555144006IL'.
           05  FILLER  PIC X(19)  VALUE '51809252915608546MI'.
           05  FILLER  PIC X(19)  VALUE '51965537269062842RI'.
           05  FILLER  PIC X(19)  VALUE '53167515043883313DC'.
           05  FILLER  PIC X(19)  VALUE '53439492857063898WV'.
           05  FILLER  PIC X(19)  VALUE '54157089908824622PA'.
           05  FILLER  PIC X(19)  VALUE '54544590913517658VA'.
           05  FILLER  PIC X(19)  VALUE '54621086032589190KS'.
           05  FILLER  PIC X(19)  VALUE '55185288635023522ND'.
           05  FILLER  PIC X(19)  VALUE '55292437457466255CA'.
           05  FILLER  PIC X(19)  VALUE '55574016094255335RI'.
           05  FILLER  PIC X(19)  VALUE '56053700877658494TN'.
           05  FILLER  PIC X(19)  VALUE '56055603975002765IN'.
           05  FILLER  PIC X(19)  VALUE '56676466575962743MD'.
           05  FILLER  PIC X(19)  VALUE '56856063829787069MA'.
           05  FILLER  PIC X(19)  VALUE '56994082058444934KY'.
           05  FILLER  PIC X(19)  VALUE '57135051196194685OR'.
           05  FILLER  PIC X(19)  VALUE '57285278335858907NJ'.
           05  FILLER  PIC X(19)  VALUE '57407753547889311AL'.
           05  FILLER  PIC X(19)  VALUE '58076840477052420NV'.
           05  FILLER  PIC X(19)  VALUE '58082328685227379OK'.
           05  FILLER  PIC X(19)  VALUE '58336929918017917MO'.
           05  FILLER  PIC X(19)  VALUE '59220199178628091OH'.
           05  FILLER  PIC X(19)  VALUE '59407480944209338VA'.
           05  FILLER  PIC X(19)  VALUE '59912527886853185CA'.
           05  FILLER  PIC X(19)  VALUE '60291426001939995KY'.
           05  FILLER  PIC X(19)  VALUE '60633189185578338HI'.
           05  FILLER  PIC X(19)  VALUE '60785339949945639RI'.
           05  FILLER  PIC X(19)  VALUE '61263836781542549ND'.
           05  FILLER  PIC X(19)  VALUE '62183305857088780AL'.
           05  FILLER  PIC X(19)  VALUE '62685541187078870CO'.
           05  FILLER  PIC X(19)  VALUE '62878004552082268DE'.
           05  FILLER  PIC X(19)  VALUE '62929895564214098IL'.
           05  FILLER  PIC X(19)  VALUE '62949124434472008SD'.
           05  FILLER  PIC X(19)  VALUE '64010247435005600PA'.
           05  FILLER  PIC X(19)  VALUE '64331702378547151NY'.
           05  FILLER  PIC X(19)  VALUE '64479949841733875VT'.
           05  FILLER  PIC X(19)  VALUE '65293172808513747ID'.
           05  FILLER  PIC X(19)  VALUE '65508907648443923WY'.
           05  FILLER  PIC X(19)  VALUE '66901527655687459NY'.
           05  FILLER  PIC X(19)  VALUE '67377128092627931VT'.
           05  FILLER  PIC X(19)  VALUE '68993932634172258CO'.
           05  FILLER  PIC X(19)  VALUE '69791000450771571NH'.
           05  FILLER  PIC X(19)  VALUE '70914753611173061HI'.
           05  FILLER  PIC X(19)  VALUE '70978720580102506OH'.
           05  FILLER  PIC X(19)  VALUE '71444612822641895GA'.
           05  FILLER  PIC X(19)  VALUE '72121093432493681NJ'.
           05  FILLER  PIC X(19)  VALUE '72721960187552339AL'.
           05  FILLER  PIC X(19)  VALUE '72984382208809724MO'.
           05  FILLER  PIC X(19)  VALUE '73026618634414290MS'.
           05  FILLER  PIC X(19)  VALUE '73428072519577591MT'.
           05  FILLER  PIC X(19)  VALUE '73513913340966258NC'.
           05  FILLER  PIC X(19)  VALUE '73668906086096251DC'.
           05  FILLER  PIC X(19)  VALUE '73885403474523545AR'.
           05  FILLER  PIC X(19)  VALUE '73907910105002129WI'.
           05  FILLER  PIC X(19)  VALUE '74302144088808856IL'.
           05  FILLER  PIC X(19)  VALUE '74894085409718638MO'.
           05  FILLER  PIC X(19)  VALUE '75112885084147483ID'.
           05  FILLER  PIC X(19)  VALUE '75477435671549778TX'.
           05  FILLER  PIC X(19)  VALUE '76203036153352626NH'.
           05  FILLER  PIC X(19)  VALUE '76696363088823163NM'.
           05  FILLER  PIC X(19)  VALUE '76799284321517110NV'.
           05  FILLER  PIC X(19)  VALUE '76818530671412762LA'.
           05  FILLER  PIC X(19)  VALUE '77268560802122978WV'.
           05  FILLER  PIC X(19)  VALUE '77450273434619131CT'.
           05  FILLER  PIC X(19)  VALUE '77634879299879491TN'.
           05  FILLER  PIC X(19)  VALUE '77826572811092271IA'.
           05  FILLER  PIC X(19)  VALUE '77850836927756249WY'.
           05  FILLER  PIC X(19)  VALUE '78111462202436285MN'.
           05  FILLER  PIC X(19)  VALUE '78267704733733918TN'.
           05  FILLER  PIC X(19)  VALUE '78876260932437599LA'.
           05  FILLER  PIC X(19)  VALUE '79315954153809027RI'.
           05  FILLER  PIC X(19)  VALUE '80196052154262090MN'.
           05  FILLER  PIC X(19)  VALUE '80401382864174186KS'.
           05  FILLER  PIC X(19)  VALUE '80987601123693189IA'.
           05  FILLER  PIC X(19)  VALUE '81005996876864446WV'.
           05  FILLER  PIC X(19)  VALUE '81255827219065310NE'.
           05  FILLER  PIC X(19)  VALUE '82339833399826938PA'.
           05  FILLER  PIC X(19)  VALUE '82492411084882309DC'.
           05  FILLER  PIC X(19)  VALUE '82853557806862521IA'.
           05  FILLER  PIC X(19)  VALUE '84863063087629639MT'.
           05  FILLER  PIC X(19)  VALUE '85277993416649181FL'.
           05  FILLER  PIC X(19)  VALUE '85677956820372225MD'.
           05  FILLER  PIC X(19)  VALUE '85803892992226645MS'.
           05  FILLER  PIC X(19)  VALUE '85889769312195859GA'.
           05  FILLER  PIC X(19)  VALUE '86895380210887366TX'.
           05  FILLER  PIC X(19)  VALUE '87423446160414048FL'.
           05  FILLER  PIC X(19)  VALUE '88095787099255429DC'.
           05  FILLER  PIC X(19)  VALUE '88187026408252748AR'.
           05  FILLER  PIC X(19)  VALUE '88360582730067430FL'.
           05  FILLER  PIC X(19)  VALUE '88404311963186257KY'.
           05  FILLER  PIC X(19)  VALUE '88695972188758569MS'.
           05  FILLER  PIC X(19)  VALUE '88940005778121474DE'.
           05  FILLER  PIC X(19)  VALUE '89393045840539982MN'.
           05  FILLER  PIC X(19)  VALUE '89410570259643458ND'.
           05  FILLER  PIC X(19)  VALUE '89771575843801393CT'.
           05  FILLER  PIC X(19)  VALUE '89905056498596426WA'.
           05  FILLER  PIC X(19)  VALUE '90031834476877427OK'.
           05  FILLER  PIC X(19)  VALUE '90202141451481002AK'.
           05  FILLER  PIC X(19)  VALUE '90612532380719560AZ'.
           05  FILLER  PIC X(19)  VALUE '90888167558946712OR'.
           05  FILLER  PIC X(19)  VALUE '91011806500466644NE'.
           05  FILLER  PIC X(19)  VALUE '91118978999736637NY'.
           05  FILLER  PIC X(19)  VALUE '92064170163023650NJ'.
           05  FILLER  PIC X(19)  VALUE '92686881251241194DE'.
           05  FILLER  PIC X(19)  VALUE '92753157543331876OK'.
           05  FILLER  PIC X(19)  VALUE '92846007651644347WA'.
           05  FILLER  PIC X(19)  VALUE '93352894798864816NV'.
           05  FILLER  PIC X(19)  VALUE '93555512174588193OH'.
           05  FILLER  PIC X(19)  VALUE '93746620638844520AZ'.
           05  FILLER  PIC X(19)  VALUE '93767497596062757WI'.
           05  FILLER  PIC X(19)  VALUE '93880419601683579CO'.
           05  FILLER  PIC X(19)  VALUE '94043301762303097SD'.
           05  FILLER  PIC X(19)  VALUE '94162635390104242NJ'.
           05  FILLER  PIC X(19)  VALUE '94275285045302224LA'.
           05  FILLER  PIC X(19)  VALUE '94483580123771163NV'.
           05  FILLER  PIC X(19)  VALUE '94684582076318064NY'.
           05  FILLER  PIC X(19)  VALUE '95351841193567548WI'.
           05  FILLER  PIC X(19)  VALUE '95623560814161890AR'.
           05  FILLER  PIC X(19)  VALUE '95740478760831599CA'.
           05  FILLER  PIC X(19)  VALUE '96193812391084264ME'.
           05  FILLER  PIC X(19)  VALUE '96374730524458813HI'.
           05  FILLER  PIC X(19)  VALUE '96507605891138352LA'.
           05  FILLER  PIC X(19)  VALUE '96519665459082496ME'.
           05  FILLER  PIC X(19)  VALUE '97487216381861910SC'.
           05  FILLER  PIC X(19)  VALUE '98305334976528380WI'.
           05  FILLER  PIC X(19)  VALUE '98429921695333075IN'.
           05  FILLER  PIC X(19)  VALUE '99223373755619014NE'.
           05  FILLER  PIC X(19)  VALUE '99309462800396930MI'.
           05  FILLER  PIC X(19)  VALUE '99607632177878747KS'.
           05  FILLER  PIC X(19)  VALUE '99638416312004757AZ'.
           05  FILLER  PIC X(19)  VALUE '99658136802581566GA'.
       01  WS-BANXRF-TABLE REDEFINES WS-BANXRF-CONST.
           05  WS-WS-BX-ENTRY OCCURS 240 TIMES
                   INDEXED BY WS-BX-IX.
               10  WS-BX-BAN               PIC X(13).
               10  WS-BX-OCN               PIC X(04).
               10  WS-BX-STATE             PIC X(02).

      * BILLED DETAIL VALIDATION WORK AREA.
       01  WS-BILL-VALIDATE.
           05  WS-BV-ELEM-TOTAL        PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-BV-LINE-TOTAL        PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-BV-VARIANCE          PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-BV-BAD-CNT           PIC S9(09) COMP-3     VALUE 0.
           05  WS-BV-ZERO-CNT          PIC S9(09) COMP-3     VALUE 0.
           05  WS-BV-TRUNC-CNT         PIC S9(09) COMP-3     VALUE 0.
           05  WS-BV-STATE-HIT       PIC X(02)           VALUE SPACES.
           05  WS-BV-FOUND-SW          PIC X(01)             VALUE 'N'.
                   88  WS-BV-FOUND              VALUE 'Y'.

      * LATA SEARCH ARGUMENT FOR THE STATE CROSS CHECK.
       01  WS-LATA-SEARCH-AREA.
           05  WS-LT-SEARCH-X          PIC 9(03)             VALUE 0.
           05  WS-LT-RESULT-X        PIC X(02)           VALUE SPACES.

      * MISCELLANEOUS WORK FIELDS.
       01  WS-MISC-WORK.
           05  WS-BEST-EFF             PIC 9(05)             VALUE 0.
           05  WS-FC-THRU-HOLD         PIC 9(05)             VALUE 0.
           05  WS-LINE-SEQ             PIC 9(07) COMP-3      VALUE 0.
           05  WS-MSG-SUB              PIC S9(03) COMP-3     VALUE 1.
           05  WS-D1-DISPO-X         PIC X(44)           VALUE SPACES.
           05  WS-PE-RATE-TARIFF-X   PIC X(04)           VALUE 'FCC1'.
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
      * THE RESTATEMENT WINDOW ENDPOINTS ARE THE MOST IMPORTANT
      * PARAMETERS THIS PROGRAM TAKES AND NEITHER OF THEM HAS A
      * DEFAULT.  THE WINDOW LENGTH ARRIVES ON THE CONTROL CARD
      * AS A SYMBOLIC SUBSTITUTED BY THE SCHEDULER AT SUBMISSION
      * TIME.  RUNNING THIS STEP WITH THE WRONG WINDOW RESTATES
      * THE WRONG QUARTER AND THE ONLY WAY BACK IS CABJUR08.
           MOVE 'P1000-INIT' TO WS-PARA-NAME.
           ACCEPT WS-ACCEPT-DATE FROM DATE.
           ACCEPT WS-ACCEPT-TIME FROM TIME.
           OPEN INPUT  USAGE-HIST-FILE
                       BILL-HIST-FILE
                       FACTOR-FILE
                       RATE-MASTER
                       PARM-FILE
           OPEN OUTPUT ADJUST-OUT-FILE
                       AUDIT-OUT-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 4701 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-USGHIST' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 4702 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-BILLHIST' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 4703 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-FCTRVAL' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 4704 TO WS-AB-CODE
               MOVE 'OPEN FAILED DA-I-RATEMAST' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 4705 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-ADJOUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-INPUT NOT = '00'
               MOVE 4706 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-AUDTOUT' TO WS-AB-TEXT
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
           MOVE ZERO TO WS-TOT-PRIOR-AMT WS-TOT-NEW-AMT
                        WS-TOT-DELTA-AMT WS-TOT-DEBIT-AMT
                        WS-TOT-CREDIT-AMT WS-TOT-MOU
                        WS-INWIN-CNT WS-OUTWIN-CNT WS-REPRICE-CNT
                        WS-ZERODELTA-CNT WS-NOBASIS-CNT
                        WS-ADJUST-CNT WS-IMMATERIAL-CNT
                        WS-BILLMATCH-CNT.
           MOVE ZERO TO WS-BK-COUNT-USED.
           PERFORM P1300-LOAD-FACTORS THRU P1300-EXIT.
           PERFORM P1400-BUILD-WINDOW THRU P1400-EXIT.
           PERFORM P1600-RESTART-CHECK THRU P1600-EXIT.
           PERFORM P2600-READ-BILL THRU P2600-EXIT.
           PERFORM P7100-HEADING THRU P7100-EXIT.
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
           IF WS-PE-WINDOW-DAYS NOT NUMERIC
               MOVE 092 TO WS-PE-WINDOW-DAYS.
           IF WS-PE-WINDOW-DAYS = ZERO
               MOVE 092 TO WS-PE-WINDOW-DAYS.
           IF WS-PE-REASON-CD = SPACES
               MOVE '01' TO WS-PE-REASON-CD.
           IF WS-PE-MATERIALITY NOT NUMERIC
               MOVE 00000.01 TO WS-PE-MATERIALITY.
           IF WS-PE-SIM-SW NOT = 'Y'
               MOVE 'N' TO WS-PE-SIM-SW.

       P1200-EXIT.
           EXIT.

       P1300-LOAD-FACTORS.
      * LOAD THE NEW FACTORS INTO CORE.  ONLY FACTORS FLAGGED FOR
      * RESTATEMENT ARE OF ANY INTEREST HERE, BUT THE WHOLE FILE IS
      * LOADED BECAUSE THE PRIOR FACTOR FOR ONE CARRIER CAN SIT ON
      * A RECORD THAT IS NOT ITSELF FLAGGED.
           MOVE 'P1300-LOAD-FACTORS' TO WS-PARA-NAME.
           MOVE ZERO TO WS-FT-COUNT.
           PERFORM P1310-READ-FACTOR THRU P1310-EXIT
               UNTIL WS-FCTR-EOF.
           DISPLAY 'FACTOR ENTRIES LOADED ' WS-FT-COUNT.
           IF WS-FT-COUNT = ZERO
               MOVE 4710 TO WS-AB-CODE
               MOVE 'NO FACTORS - NOTHING TO RESTATE' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.

       P1300-EXIT.
           EXIT.

       P1310-READ-FACTOR.
      * ONE FACTOR RECORD.  THE FIRST RECORD FLAGGED FOR RESTATEMENT
      * SUPPLIES THE WINDOW FOR THE WHOLE RUN.  THAT IS A 1988
      * ASSUMPTION - ONE RESTATEMENT PER RUN - AND IT IS STILL HOW
      * THE QUARTERLY JOB IS SCHEDULED.
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
           IF FC-RESTATE-REQD AND WS-RS-FROM-YYDDD = ZERO
               MOVE FC-RESTATE-FROM-YYDDD TO WS-RS-FROM-YYDDD
               MOVE FC-RESTATE-THRU-YYDDD TO WS-FC-THRU-HOLD
               MOVE FC-PRIOR-PIU TO WS-RW-PRIOR-PIU
               MOVE FC-PRIOR-PLU TO WS-RW-PRIOR-PLU.

       P1310-EXIT.
           EXIT.

       P1400-BUILD-WINDOW.
      * DERIVE THE END OF THE RESTATEMENT WINDOW.  THE FACTOR RECORD
      * CARRIES A FROM DATE AND THE CONTROL CARD CARRIES THE NUMBER
      * OF DAYS THE QUARTER RUNS FOR.  THE THRU DATE ON THE FACTOR
      * RECORD CAPS THE WINDOW WHEN IT IS THE EARLIER OF THE TWO.
           MOVE 'P1400-BUILD-WINDOW' TO WS-PARA-NAME.
           IF WS-RS-FROM-YYDDD = ZERO
               MOVE 4711 TO WS-AB-CODE
               MOVE 'NO RESTATEMENT WINDOW ON ANY FACTOR' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           MOVE WS-PE-WINDOW-DAYS TO WS-RS-SPAN-DAYS.
           COMPUTE WS-WA-THRU-N = WS-WA-FROM-N + WS-RS-SPAN-DAYS.
           IF WS-FC-THRU-HOLD NOT = ZERO
               IF WS-FC-THRU-HOLD < WS-WA-THRU-N
                   MOVE WS-FC-THRU-HOLD TO WS-WA-THRU-N.
           MOVE WS-RS-FROM-YYDDD TO WS-JW-TEST.
           PERFORM P6500-JULIAN-TO-ABS THRU P6500-EXIT.
           MOVE WS-JW-ABS-TEST TO WS-RS-FROM-ABS.
           MOVE WS-RS-THRU-YYDDD TO WS-JW-TEST.
           PERFORM P6500-JULIAN-TO-ABS THRU P6500-EXIT.
           MOVE WS-JW-ABS-TEST TO WS-RS-THRU-ABS.
           IF WS-RS-FROM-YY NOT = WS-RS-THRU-YY
               DISPLAY WS-MSG-TEXT (13).
           DISPLAY 'RESTATEMENT WINDOW ' WS-RS-FROM-YYDDD
                   ' THROUGH ' WS-RS-THRU-YYDDD.
           DISPLAY 'WINDOW SPAN DAYS   ' WS-RS-SPAN-DAYS.

       P1400-EXIT.
           EXIT.

       P1600-RESTART-CHECK.
      * IF THE PRIOR RUN ABENDED THE OPERATOR SUPPLIES THE RESTART
      * KEY ON A SECOND CONTROL CARD.  USAGE RECORDS BELOW THAT KEY
      * ARE SKIPPED.  THE BUCKETS ARE NOT REBUILT, SO A RESTART
      * PRODUCES CARRIER LEVEL ADJUSTMENTS ONLY FROM THE RESTART
      * POINT FORWARD.  THAT IS ACCEPTED - THE ALTERNATIVE WAS A
      * CHECKPOINT DATASET AND IT WAS NEVER BUILT.
           MOVE 'P1600-RESTART-CHECK' TO WS-PARA-NAME.
           IF WS-PC-TYPE NOT = 'R2'
               MOVE 'Y' TO WS-RESTART-DONE-SW
               GO TO P1600-EXIT.
           MOVE 'Y' TO WS-RESTART-SW.
           MOVE WS-PO-RUN-ID TO WS-RESTART-KEY.
           DISPLAY 'RESTART REQUESTED FROM KEY ' WS-RESTART-KEY.

       P1600-EXIT.
           EXIT.


      *****************************************************************
      * S200-PRIOR-PERIOD                                             *
      * READ THE PRIOR PERIOD USAGE AND SELECT THE WINDOW.            *
      *****************************************************************
       S200-PRIOR-PERIOD SECTION.

       P2000-PROCESS.
      * ONE PRIOR PERIOD USAGE RECORD PER PASS.  THE DRIVING FILE IS
      * THE PRICED USAGE FILE FROM THE QUARTER BEING RESTATED, NOT
      * THE CURRENT CYCLE.  IT IS READ THROUGH THE MINUS ONE
      * GENERATION OF THE PLU FILE - WHICH GENERATION THAT ACTUALLY
      * IS DEPENDS ON HOW MANY TIMES THE CYCLE HAS RUN THIS MONTH.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ THRU P2100-EXIT.
           IF WS-EOF
               PERFORM P6300-BUCKET-FLUSH THRU P6300-EXIT
               GO TO P2000-EXIT.
           MOVE UHI-RECORD TO CABS-CDR-RECORD.
           MOVE CD-KEY TO WS-RESTART-KEY.
           IF WS-RESTARTING AND NOT WS-RESTART-DONE
               PERFORM P2050-SKIP-TO-RESTART THRU P2050-EXIT
               GO TO P2000-EXIT.
           PERFORM P2200-VALIDATE-USAGE THRU P2200-EXIT.
           IF WS-ERROR-FOUND
               MOVE 'N' TO WS-ERROR-SW
               GO TO P2000-EXIT.
           PERFORM P2400-CONTROL-BREAK THRU P2400-EXIT.
           PERFORM P2300-WINDOW-TEST THRU P2300-EXIT.
           IF NOT WS-IN-WINDOW
               ADD 1 TO WS-OUTWIN-CNT
               ADD 1 TO WS-CFWD-CNT
               GO TO P2000-EXIT.
           ADD 1 TO WS-INWIN-CNT.
           PERFORM P2500-MATCH-BILLED THRU P2500-EXIT.
           PERFORM P3000-RESOLVE-FACTORS THRU P3000-EXIT.
           IF WS-PAIR-BAD
               ADD 1 TO WS-SUMM-CNT
               GO TO P2000-EXIT.
           PERFORM P4000-REPRICE THRU P4000-EXIT.
           PERFORM P5000-COMPUTE-DELTA THRU P5000-EXIT.
           PERFORM P5300-MATERIALITY THRU P5300-EXIT.
           IF NOT WS-MATERIAL
               ADD 1 TO WS-IMMATERIAL-CNT
               ADD 1 TO WS-SUMM-CNT
               GO TO P2000-EXIT.
           PERFORM P5200-ACCUM-BUCKET THRU P5200-EXIT.
           PERFORM P6200-AUDIT-TRAIL THRU P6200-EXIT.
           PERFORM P7200-DETAIL THRU P7200-EXIT.
           ADD 1 TO WS-REPRICE-CNT.
           ADD 1 TO WS-WRITE-CNT.

       P2000-EXIT.
           EXIT.

       P2100-READ.
      * SEQUENTIAL READ OF THE PRIOR PERIOD USAGE FILE.
           READ USAGE-HIST-FILE
               AT END
                   MOVE 'Y' TO WS-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00'
               MOVE 3470 TO WS-AB-CODE
               MOVE 'READ ERROR UT-S-USGHIST' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-READ-CNT.

       P2100-EXIT.
           EXIT.

       P2050-SKIP-TO-RESTART.
      * SKIP FORWARD TO THE RESTART KEY.  ONCE THE KEY IS REACHED
      * NORMAL PROCESSING RESUMES ON THE NEXT RECORD.
           IF CD-KEY < WS-RESTART-KEY
               ADD 1 TO WS-CFWD-CNT
               GO TO P2050-EXIT.
           MOVE 'Y' TO WS-RESTART-DONE-SW.
           DISPLAY 'RESTART POINT REACHED AT ' CD-KEY.

       P2050-EXIT.
           EXIT.

       P2200-VALIDATE-USAGE.
      * ONLY VOICE MINUTES CARRY A JURISDICTIONAL FACTOR.  DATA AND
      * SPECIAL ACCESS RECORDS ARE COUNTED AND IGNORED.  A RECORD
      * WITH ZERO CHARGEABLE MINUTES CANNOT PRODUCE A DELTA.
           MOVE 'P2200-VALIDATE-USAGE' TO WS-PARA-NAME.
           IF NOT CD-VOICE-MOU
               MOVE 'Y' TO WS-ERROR-SW
               ADD 1 TO WS-CFWD-CNT
               GO TO P2200-EXIT.
           IF CD-VC-CHG-MIN = ZERO
               MOVE 'Y' TO WS-ERROR-SW
               ADD 1 TO WS-SUMM-CNT
               GO TO P2200-EXIT.
           IF CD-VC-CHG-MIN < ZERO
               MOVE EC-MIN-NEGATIVE TO WS-ERR-CODE
               MOVE 'E' TO WS-ERR-SEVERITY
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               MOVE 'Y' TO WS-ERROR-SW
               GO TO P2200-EXIT.
           IF WS-PE-OCN-SELECT NOT = SPACES
               IF CD-OCN NOT = WS-PE-OCN-SELECT
                   MOVE 'Y' TO WS-ERROR-SW
                   ADD 1 TO WS-CFWD-CNT
                   GO TO P2200-EXIT.
           MOVE CD-VC-CHG-MIN TO WS-RW-BASE-MOU.
           MOVE CD-CONN-YYDDD TO WS-RS-USE-YYDDD.

       P2200-EXIT.
           EXIT.

       P2300-WINDOW-TEST.
      * DECIDE WHETHER THIS USAGE RECORD FALLS INSIDE THE WINDOW.
      * THE CONNECT DATE ON THE RECORD IS COMPARED AGAINST THE TWO
      * ENDPOINTS.  USAGE OUTSIDE THE WINDOW IS CARRIED FORWARD AND
      * IS NOT RESTATED - IT WAS BILLED UNDER A DIFFERENT FACTOR.
           MOVE 'P2300-WINDOW-TEST' TO WS-PARA-NAME.
           MOVE 'N' TO WS-IN-WINDOW-SW.
           IF WS-RS-USE-YYDDD < WS-RS-FROM-YYDDD
               GO TO P2300-EXIT.
           IF WS-RS-USE-YYDDD > WS-RS-THRU-YYDDD
               GO TO P2300-EXIT.
           MOVE 'Y' TO WS-IN-WINDOW-SW.

       P2300-EXIT.
           EXIT.

       P2400-CONTROL-BREAK.
      * THE USAGE FILE IS IN OCN AND BAN SEQUENCE.  A CHANGE OF OCN
      * FLUSHES THE ACCUMULATED BUCKETS TO THE ADJUSTMENT FILE.
           MOVE 'P2400-CONTROL-BREAK' TO WS-PARA-NAME.
           IF WS-SV-FIRST
               MOVE 'N' TO WS-SV-FIRST-SW
               PERFORM P2450-SAVE-KEYS THRU P2450-EXIT
               GO TO P2400-EXIT.
           IF CD-OCN = WS-SV-OCN
               GO TO P2400-EXIT.
           PERFORM P6300-BUCKET-FLUSH THRU P6300-EXIT.
           MOVE ZERO TO WS-BK-COUNT-USED.
           PERFORM P2450-SAVE-KEYS THRU P2450-EXIT.

       P2400-EXIT.
           EXIT.

       P2450-SAVE-KEYS.
      * REMEMBER THE CURRENT KEY VALUES.
           MOVE CD-OCN TO WS-SV-OCN.
           MOVE CD-BAN TO WS-SV-BAN.
           MOVE CD-JURIS-CD TO WS-SV-JURIS.
           MOVE CD-SEQ-NBR TO WS-SV-SEQ.

       P2450-EXIT.
           EXIT.

       P2500-MATCH-BILLED.
      * MATCH THE USAGE RECORD TO THE BILLED DETAIL LINE IT WAS
      * SUMMARISED INTO.  THE MATCH IS ON BAN AND SECTION.  A USAGE
      * RECORD WITH NO MATCHING BILLED LINE IS STILL RESTATED - THE
      * PRIOR AMOUNT IS THEN AN ESTIMATE FROM THE RATE MASTER RATHER
      * THAN THE AMOUNT ACTUALLY BILLED.  THAT ESTIMATE IS WHY THE
      * RESTATEMENT TOTAL AND THE BILLED TOTAL NEVER TIE EXACTLY.
           MOVE 'P2500-MATCH-BILLED' TO WS-PARA-NAME.
           MOVE 'N' TO WS-BILL-MATCH-SW.
           MOVE ZERO TO WS-RW-BILLED-AMT.
           IF WS-BILL-EOF
               GO TO P2500-EXIT.
           PERFORM P2550-ADVANCE-BILL THRU P2550-EXIT
               UNTIL WS-BILL-EOF
                  OR BD-BAN NOT < CD-BAN.
           IF WS-BILL-EOF
               GO TO P2500-EXIT.
           IF BD-BAN NOT = CD-BAN
               MOVE WS-MSG-TEXT (9) TO WS-D1-DISPO-X
               GO TO P2500-EXIT.
           MOVE 'Y' TO WS-BILL-MATCH-SW.
           ADD 1 TO WS-BILLMATCH-CNT.
           MOVE BD-TOT-AMOUNT TO WS-RW-BILLED-AMT.
           MOVE BD-ELEM-CNT TO WS-EW-COUNT.
           PERFORM P4300-ELEMENT-LOAD THRU P4300-EXIT.

       P2500-EXIT.
           EXIT.

       P2550-ADVANCE-BILL.
      * ADVANCE THE BILLED DETAIL FILE UNTIL IT REACHES OR PASSES
      * THE CURRENT USAGE BAN.
           PERFORM P2600-READ-BILL THRU P2600-EXIT.

       P2550-EXIT.
           EXIT.

       P2600-READ-BILL.
      * READ THE BILLED DETAIL HISTORY FILE.  THIS IS A VARIABLE
      * LENGTH FILE WITH AN OCCURS DEPENDING ON - MOVING IT TO A
      * FIXED AREA TRUNCATES IT, WHICH IS WHY IT IS READ INTO THE
      * COPYBOOK AREA DIRECTLY.
           READ BILL-HIST-FILE INTO CABS-BILL-DETAIL
               AT END
                   MOVE 'Y' TO WS-BILL-EOF-SW
                   GO TO P2600-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 4702 TO WS-AB-CODE
               MOVE 'READ ERROR ON BILLHIST' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.

       P2600-EXIT.
           EXIT.


      *****************************************************************
      * S300-FACTOR-RESOLUTION                                        *
      * FIND THE NEW FACTOR AND THE FACTOR IT REPLACES.               *
      *****************************************************************
       S300-FACTOR-RESOLUTION SECTION.

       P3000-RESOLVE-FACTORS.
      * RESOLVE BOTH ENDS OF THE RESTATEMENT.  THE NEW FACTOR IS THE
      * ONE THAT ARRIVED THIS QUARTER.  THE PRIOR FACTOR IS THE ONE
      * THE USAGE WAS ORIGINALLY BILLED UNDER.  WITHOUT BOTH THERE
      * IS NO DELTA TO COMPUTE AND NO ADJUSTMENT TO RAISE.
           MOVE 'P3000-RESOLVE-FACTORS' TO WS-PARA-NAME.
           MOVE 'Y' TO WS-FACTOR-PAIR-SW.
           MOVE ZERO TO WS-RW-NEW-PIU.
           MOVE ZERO TO WS-RW-PRIOR-PIU.
           PERFORM P3100-FIND-NEW THRU P3100-EXIT.
           IF WS-PAIR-BAD
               GO TO P3000-EXIT.
           PERFORM P3200-FIND-PRIOR THRU P3200-EXIT.
           IF WS-PAIR-BAD
               GO TO P3000-EXIT.
           PERFORM P3300-VALIDATE-PAIR THRU P3300-EXIT.

       P3000-EXIT.
           EXIT.

       P3100-FIND-NEW.
      * THE NEW FACTOR IS THE ENTRY FOR THIS OCN AND LATA WITH THE
      * LATEST EFFECTIVE DATE THAT IS NOT AFTER THE WINDOW FROM
      * DATE.  THE TABLE IS WALKED IN FULL - IT IS NOT SORTED BY
      * EFFECTIVE DATE WITHIN KEY.
           MOVE 'P3100-FIND-NEW' TO WS-PARA-NAME.
           MOVE 'N' TO WS-FT-FOUND-SW.
           MOVE ZERO TO WS-BEST-EFF.
           MOVE 1 TO WS-SUB1.
           PERFORM P3150-SCAN-NEW THRU P3150-EXIT
               UNTIL WS-SUB1 > WS-FT-COUNT.
           IF WS-FT-FOUND
               GO TO P3100-EXIT.
           MOVE EC-FACTOR-MISSING TO WS-ERR-CODE.
           MOVE 'W' TO WS-ERR-SEVERITY.
           MOVE 'N' TO WS-FACTOR-PAIR-SW.

       P3100-EXIT.
           EXIT.

       P3150-SCAN-NEW.
      * ONE FACTOR TABLE ENTRY.
           IF WS-FT-OCN (WS-SUB1) = CD-OCN AND
              WS-FT-LATA (WS-SUB1) = CD-VC-ORIG-LATA
               IF WS-FT-EFF (WS-SUB1) NOT > WS-RS-FROM-YYDDD
                   IF WS-FT-EFF (WS-SUB1) NOT < WS-BEST-EFF
                       MOVE WS-FT-EFF (WS-SUB1) TO WS-BEST-EFF
                       MOVE WS-FT-PIU (WS-SUB1) TO WS-RW-NEW-PIU
                       MOVE WS-FT-PLU (WS-SUB1) TO WS-RW-NEW-PLU
                       MOVE 'Y' TO WS-FT-FOUND-SW.
           ADD 1 TO WS-SUB1.

       P3150-EXIT.
           EXIT.

       P3200-FIND-PRIOR.
      * THE PRIOR FACTOR IS CARRIED ON THE NEW FACTOR RECORD BY
      * CABJUR01 WHEN IT REPLACES AN EXISTING FILING.  IT IS THE
      * BASIS THE ORIGINAL BILL WAS RAISED ON.
           MOVE 'P3200-FIND-PRIOR' TO WS-PARA-NAME.
           MOVE ZERO TO WS-RW-PRIOR-PIU.
           MOVE 1 TO WS-SUB2.
           PERFORM P3250-SCAN-PRIOR THRU P3250-EXIT
               UNTIL WS-SUB2 > WS-FT-COUNT.
           IF WS-RW-PRIOR-PIU = ZERO
               MOVE WS-RW-NEW-PIU TO WS-RW-PRIOR-PIU
               ADD 1 TO WS-NOBASIS-CNT.
           IF WS-RW-PRIOR-PLU = ZERO
               MOVE WS-RW-NEW-PLU TO WS-RW-PRIOR-PLU.

       P3200-EXIT.
           EXIT.

       P3250-SCAN-PRIOR.
      * LOCATE THE SUPERSEDED FILING FOR THE SAME KEY.  THE PRIOR
      * FACTOR IS HELD IN FC-PRIOR-PIU ON THE REPLACING RECORD.
           IF WS-FT-OCN (WS-SUB2) = CD-OCN AND
              WS-FT-LATA (WS-SUB2) = CD-VC-ORIG-LATA
               IF WS-FT-EFF (WS-SUB2) < WS-RS-FROM-YYDDD
                   MOVE WS-FT-PIU (WS-SUB2) TO WS-RW-PRIOR-PIU
                   MOVE WS-FT-PLU (WS-SUB2) TO WS-RW-PRIOR-PLU.
           ADD 1 TO WS-SUB2.

       P3250-EXIT.
           EXIT.

       P3300-VALIDATE-PAIR.
      * BOTH FACTORS MUST BE IN RANGE AND THEY MUST DIFFER.  AN
      * IDENTICAL PAIR PRODUCES A ZERO DELTA AND NO ADJUSTMENT - THE
      * RECORD IS COUNTED AS SUMMARISED SO THAT THE BALANCE HOLDS.
           MOVE 'P3300-VALIDATE-PAIR' TO WS-PARA-NAME.
           IF WS-RW-NEW-PIU < 0 OR WS-RW-NEW-PIU > 100.00000
               MOVE EC-PIU-OUT-OF-RANGE TO WS-ERR-CODE
               MOVE 'E' TO WS-ERR-SEVERITY
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               MOVE 'N' TO WS-FACTOR-PAIR-SW
               GO TO P3300-EXIT.
           IF WS-RW-PRIOR-PIU < 0 OR WS-RW-PRIOR-PIU > 100.00000
               MOVE EC-PIU-OUT-OF-RANGE TO WS-ERR-CODE
               MOVE 'E' TO WS-ERR-SEVERITY
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               MOVE 'N' TO WS-FACTOR-PAIR-SW
               GO TO P3300-EXIT.
           IF WS-RW-NEW-PIU = WS-RW-PRIOR-PIU
               ADD 1 TO WS-ZERODELTA-CNT
               MOVE 'N' TO WS-FACTOR-PAIR-SW.

       P3300-EXIT.
           EXIT.


      *****************************************************************
      * S400-REPRICE                                                  *
      * REPRICE THE USAGE UNDER BOTH FACTORS.                         *
      *****************************************************************
       S400-REPRICE SECTION.

       P4000-REPRICE.
      * REPRICE THE RECORD TWICE - ONCE UNDER THE FACTOR IT WAS
      * BILLED WITH AND ONCE UNDER THE FACTOR THAT NOW APPLIES.
      * THE RATES USED MUST BE THE RATES THAT WERE IN EFFECT ON THE
      * USAGE DATE, NOT TODAYS RATES - OTHERWISE A RATE CHANGE IN
      * THE INTERVENING QUARTER WOULD LEAK INTO THE FACTOR DELTA.
           MOVE 'P4000-REPRICE' TO WS-PARA-NAME.
           PERFORM P4400-RATE-LOOKUP THRU P4400-EXIT.
           PERFORM P4100-SPLIT-PRIOR THRU P4100-EXIT.
           PERFORM P4200-SPLIT-NEW THRU P4200-EXIT.
           IF WS-BILL-MATCHED
               PERFORM P4500-APPORTION THRU P4500-EXIT.

       P4000-EXIT.
           EXIT.

       P4100-SPLIT-PRIOR.
      * SPLIT THE MINUTES AND PRICE THEM UNDER THE PRIOR FACTOR.
      * THE INTRASTATE HALF IS DERIVED BY SUBTRACTION SO THAT THE
      * TWO HALVES ALWAYS ADD BACK TO THE BASE MINUTES.
           COMPUTE WS-RW-PR-IS-MOU ROUNDED =
                   WS-RW-BASE-MOU * WS-RW-PRIOR-PIU / 100.
           COMPUTE WS-RW-PR-SS-MOU =
                   WS-RW-BASE-MOU - WS-RW-PR-IS-MOU.
           COMPUTE WS-RW-PRIOR-AMT ROUNDED =
                   (WS-RW-PR-IS-MOU * WS-RW-IS-RATE)
                 + (WS-RW-PR-SS-MOU * WS-RW-SS-RATE).
           IF WS-BILL-MATCHED AND WS-RW-BILLED-AMT NOT = ZERO
               COMPUTE WS-RW-VARIANCE =
                       WS-RW-PRIOR-AMT - WS-RW-BILLED-AMT.

       P4100-EXIT.
           EXIT.

       P4200-SPLIT-NEW.
      * THE SAME SPLIT UNDER THE NEW FACTOR.
           COMPUTE WS-RW-NW-IS-MOU ROUNDED =
                   WS-RW-BASE-MOU * WS-RW-NEW-PIU / 100.
           COMPUTE WS-RW-NW-SS-MOU =
                   WS-RW-BASE-MOU - WS-RW-NW-IS-MOU.
           COMPUTE WS-RW-NEW-AMT ROUNDED =
                   (WS-RW-NW-IS-MOU * WS-RW-IS-RATE)
                 + (WS-RW-NW-SS-MOU * WS-RW-SS-RATE).

       P4200-EXIT.
           EXIT.

       P4300-ELEMENT-LOAD.
      * COPY THE BILLED RATE ELEMENTS INTO THE WORK TABLE.  THE
      * BILLED LINE CAN CARRY UP TO FORTY OF THEM AND THE OCCURS
      * DEPENDING ON MEANS THE COUNT MUST BE TAKEN FROM THE RECORD
      * BEFORE ANY ELEMENT IS TOUCHED.
           MOVE 'P4300-ELEMENT-LOAD' TO WS-PARA-NAME.
           MOVE ZERO TO WS-EW-TOT-ORIG.
           IF BD-ELEM-CNT = ZERO
               GO TO P4300-EXIT.
           IF BD-ELEM-CNT > 40
               MOVE 40 TO WS-EW-COUNT
           ELSE
               MOVE BD-ELEM-CNT TO WS-EW-COUNT.
           MOVE 1 TO WS-ELEM-SUB.
           PERFORM P4350-ELEMENT-COPY THRU P4350-EXIT
               UNTIL WS-ELEM-SUB > WS-EW-COUNT.

       P4300-EXIT.
           EXIT.

       P4350-ELEMENT-COPY.
      * ONE RATE ELEMENT.
           MOVE BD-EL-RATE-ELEM (WS-ELEM-SUB)
                TO WS-EW-ELEM (WS-ELEM-SUB).
           MOVE BD-EL-QTY (WS-ELEM-SUB)
                TO WS-EW-QTY (WS-ELEM-SUB).
           MOVE BD-EL-RATE (WS-ELEM-SUB)
                TO WS-EW-RATE (WS-ELEM-SUB).
           MOVE BD-EL-AMOUNT (WS-ELEM-SUB)
                TO WS-EW-ORIG-AMT (WS-ELEM-SUB).
           MOVE BD-EL-ROUND-RULE (WS-ELEM-SUB)
                TO WS-EW-ROUND (WS-ELEM-SUB).
           MOVE ZERO TO WS-EW-NEW-AMT (WS-ELEM-SUB).
           MOVE ZERO TO WS-EW-DELTA (WS-ELEM-SUB).
           ADD BD-EL-AMOUNT (WS-ELEM-SUB) TO WS-EW-TOT-ORIG.
           ADD 1 TO WS-ELEM-SUB.

       P4350-EXIT.
           EXIT.

       P4400-RATE-LOOKUP.
      * FETCH THE INTERSTATE AND INTRASTATE RATES THAT APPLIED ON
      * THE USAGE DATE.  THE RATE KEY CARRIES AN EFFECTIVE DATE SO
      * THE READ IS FOR THE HIGHEST KEY NOT GREATER THAN THE USAGE
      * DATE - WHICH VSAM CANNOT DO DIRECTLY, SO THE KEY IS BUILT
      * WITH THE USAGE DATE AND A FAILED READ FALLS BACK TO THE
      * ELEMENT TABLE DEFAULT.
           MOVE 'P4400-RATE-LOOKUP' TO WS-PARA-NAME.
           MOVE ZERO TO WS-RW-IS-RATE.
           MOVE ZERO TO WS-RW-SS-RATE.
           MOVE WS-PE-RATE-TARIFF-X TO WS-RK-TARIFF.
           MOVE CD-RATE-ELEM TO WS-RK-ELEM.
           MOVE 'I' TO WS-RK-JURIS.
           MOVE WS-SV-STATE TO WS-RK-STATE.
           MOVE WS-RS-USE-YYDDD TO WS-RK-EFF.
           MOVE WS-RATE-KEY TO RTM-KEY.
           READ RATE-MASTER INTO CABS-RATE-RECORD
               INVALID KEY
                   MOVE ZERO TO WS-RW-IS-RATE
                   GO TO P4450-INTRASTATE.
           MOVE RT-INITIAL-RATE TO WS-RW-IS-RATE.

       P4400-EXIT.
           EXIT.

       P4450-INTRASTATE.
      * THE INTRASTATE RATE FOR THE SAME ELEMENT AND DATE.
           MOVE 'S' TO WS-RK-JURIS.
           MOVE WS-RATE-KEY TO RTM-KEY.
           READ RATE-MASTER INTO CABS-RATE-RECORD
               INVALID KEY
                   MOVE ZERO TO WS-RW-SS-RATE
                   GO TO P4450-EXIT.
           MOVE RT-INITIAL-RATE TO WS-RW-SS-RATE.
           IF WS-RW-IS-RATE = ZERO AND WS-RW-SS-RATE = ZERO
               MOVE EC-RATE-NOT-FOUND TO WS-ERR-CODE
               MOVE 'W' TO WS-ERR-SEVERITY.

       P4450-EXIT.
           EXIT.

       P4500-APPORTION.
      * APPORTION THE DELTA ACROSS THE BILLED RATE ELEMENTS IN
      * PROPORTION TO THE ORIGINAL BILLED AMOUNT.  WITHOUT THIS THE
      * ADJUSTMENT CANNOT BE TIED BACK TO A BILL LINE AND THE
      * CARRIER DISPUTES IT.  THE RESIDUE FROM THE APPORTIONMENT IS
      * ADDED TO THE LARGEST ELEMENT, NOT SPREAD - SPREADING IT
      * PRODUCED FRACTIONAL CENTS THAT DID NOT ADD BACK.
           MOVE 'P4500-APPORTION' TO WS-PARA-NAME.
           IF WS-EW-COUNT = ZERO
               GO TO P4500-EXIT.
           IF WS-EW-TOT-ORIG = ZERO
               GO TO P4500-EXIT.
           MOVE ZERO TO WS-EW-TOT-DELTA.
           MOVE 1 TO WS-ELEM-SUB.
           PERFORM P4550-APPORTION-ONE THRU P4550-EXIT
               UNTIL WS-ELEM-SUB > WS-EW-COUNT.
           COMPUTE WS-EW-RESIDUE =
                   WS-RW-DELTA - WS-EW-TOT-DELTA.
           IF WS-EW-RESIDUE NOT = ZERO
               ADD WS-EW-RESIDUE TO WS-EW-DELTA (1).

       P4500-EXIT.
           EXIT.

       P4550-APPORTION-ONE.
      * ONE ELEMENT SHARE.  THE SHARE IS CARRIED TO SEVEN DECIMAL
      * PLACES SO THAT THE APPORTIONMENT OF A SMALL DELTA ACROSS
      * FORTY ELEMENTS DOES NOT COLLAPSE TO ZERO.
           COMPUTE WS-EW-SHARE (WS-ELEM-SUB) ROUNDED =
                   WS-EW-ORIG-AMT (WS-ELEM-SUB) / WS-EW-TOT-ORIG.
           COMPUTE WS-EW-DELTA (WS-ELEM-SUB) ROUNDED =
                   WS-RW-DELTA * WS-EW-SHARE (WS-ELEM-SUB).
           COMPUTE WS-EW-NEW-AMT (WS-ELEM-SUB) =
                   WS-EW-ORIG-AMT (WS-ELEM-SUB)
                 + WS-EW-DELTA (WS-ELEM-SUB).
           ADD WS-EW-DELTA (WS-ELEM-SUB) TO WS-EW-TOT-DELTA.
           ADD 1 TO WS-ELEM-SUB.

       P4550-EXIT.
           EXIT.


      *****************************************************************
      * S500-DELTA                                                    *
      * THE DELTA AND ITS ACCUMULATION.                               *
      *****************************************************************
       S500-DELTA SECTION.

       P5000-COMPUTE-DELTA.
      * THE DELTA IS THE DIFFERENCE BETWEEN WHAT THE USAGE WOULD
      * HAVE COST UNDER THE NEW FACTOR AND WHAT IT ACTUALLY COST
      * UNDER THE OLD ONE.  IT IS COMPUTED AT FIVE DECIMAL PLACES
      * AND ROUNDED AT THE FIFTH - THIS IS THE FIGURE THAT REACHES
      * THE CARRIERS BILL, SO IT IS THE FIGURE THE AUDITORS TRACE.
      * A POSITIVE DELTA IS AN UNDERBILL AND RAISES A DEBIT.
      * A NEGATIVE DELTA IS AN OVERBILL AND RAISES A CREDIT.
           MOVE 'P5000-COMPUTE-DELTA' TO WS-PARA-NAME.
           COMPUTE WS-RW-DELTA ROUNDED =
                   WS-RW-NEW-AMT - WS-RW-PRIOR-AMT.
           IF WS-RW-DELTA < ZERO
               COMPUTE WS-RW-DELTA-ABS = WS-RW-DELTA * -1
           ELSE
               MOVE WS-RW-DELTA TO WS-RW-DELTA-ABS.
           ADD WS-RW-PRIOR-AMT TO WS-TOT-PRIOR-AMT.
           ADD WS-RW-NEW-AMT TO WS-TOT-NEW-AMT.
           ADD WS-RW-DELTA TO WS-TOT-DELTA-AMT.
           ADD WS-RW-BASE-MOU TO WS-TOT-MOU.
           ADD WS-RW-BASE-MOU TO WS-ACC-MINUTES.
           ADD WS-RW-DELTA TO WS-ACC-AMOUNT.
           IF WS-RW-DELTA > ZERO
               ADD WS-RW-DELTA TO WS-TOT-DEBIT-AMT.
           IF WS-RW-DELTA < ZERO
               ADD WS-RW-DELTA TO WS-TOT-CREDIT-AMT.

       P5000-EXIT.
           EXIT.

       P5300-MATERIALITY.
      * A DELTA BELOW THE MATERIALITY THRESHOLD IS NOT WORTH THE
      * COST OF AN ADJUSTMENT LINE.  THE THRESHOLD ARRIVES ON THE
      * CONTROL CARD.  IT WAS ONE CENT FROM 2000 UNTIL 2013 AND IS
      * NOW WHATEVER THE SCHEDULER SUBSTITUTES.
           MOVE 'P5300-MATERIALITY' TO WS-PARA-NAME.
           MOVE 'Y' TO WS-MATERIAL-SW.
           IF WS-RW-DELTA-ABS < WS-PE-MATERIALITY
               MOVE 'N' TO WS-MATERIAL-SW.
           IF WS-RW-DELTA = ZERO
               MOVE 'N' TO WS-MATERIAL-SW
               ADD 1 TO WS-ZERODELTA-CNT.

       P5300-EXIT.
           EXIT.

       P5200-ACCUM-BUCKET.
      * ACCUMULATE THE DELTA INTO THE BUCKET FOR THIS CARRIER,
      * STATE AND JURISDICTION.  ONE ADJUSTMENT RECORD IS RAISED PER
      * BUCKET, NOT PER USAGE RECORD - A QUARTERLY RESTATEMENT CAN
      * TOUCH TENS OF MILLIONS OF USAGE RECORDS AND NO CARRIER WILL
      * ACCEPT AN ADJUSTMENT LINE FOR EACH ONE.
           MOVE 'P5200-ACCUM-BUCKET' TO WS-PARA-NAME.
           PERFORM P5250-FIND-BUCKET THRU P5250-EXIT.
           IF NOT WS-BK-FOUND
               PERFORM P5260-ADD-BUCKET THRU P5260-EXIT.
           IF NOT WS-BK-FOUND
               GO TO P5200-EXIT.
           SET WS-BK-IX TO WS-BK-HIT.
           ADD WS-RW-BASE-MOU TO WS-BK-MOU (WS-BK-IX).
           ADD WS-RW-PRIOR-AMT TO WS-BK-PRIOR-AMT (WS-BK-IX).
           ADD WS-RW-NEW-AMT TO WS-BK-NEW-AMT (WS-BK-IX).
           ADD WS-RW-DELTA TO WS-BK-DELTA (WS-BK-IX).
           ADD 1 TO WS-BK-COUNT (WS-BK-IX).
           MOVE WS-RW-PRIOR-PIU TO WS-BK-PRIOR-PIU (WS-BK-IX).
           MOVE WS-RW-NEW-PIU TO WS-BK-NEW-PIU (WS-BK-IX).
           MOVE WS-PE-REASON-CD TO WS-BK-REASON (WS-BK-IX).

       P5200-EXIT.
           EXIT.

       P5250-FIND-BUCKET.
      * SERIAL SEARCH OF THE BUCKET TABLE.  THE TABLE IS SMALL AND
      * IS REBUILT AT EVERY CARRIER BREAK.
           MOVE 'N' TO WS-BK-FOUND-SW.
           MOVE ZERO TO WS-BK-HIT.
           MOVE 1 TO WS-SUB3.
           PERFORM P5255-BUCKET-COMPARE THRU P5255-EXIT
               UNTIL WS-SUB3 > WS-BK-COUNT-USED
                  OR WS-BK-FOUND.

       P5250-EXIT.
           EXIT.

       P5255-BUCKET-COMPARE.
      * ONE BUCKET COMPARE.
           IF WS-BK-OCN (WS-SUB3) = CD-OCN AND
              WS-BK-STATE (WS-SUB3) = WS-SV-STATE AND
              WS-BK-JURIS (WS-SUB3) = CD-JURIS-CD
               MOVE 'Y' TO WS-BK-FOUND-SW
               MOVE WS-SUB3 TO WS-BK-HIT
               GO TO P5255-EXIT.
           ADD 1 TO WS-SUB3.

       P5255-EXIT.
           EXIT.

       P5260-ADD-BUCKET.
      * ADD A NEW BUCKET.  A FULL TABLE IS NOT AN ABEND - THE DELTA
      * IS ADDED TO THE FIRST BUCKET FOR THE CARRIER SO THAT NO
      * MONEY IS LOST, AND A WARNING IS RAISED.  THAT COMPROMISE
      * DATES FROM THE 1993 PARALLEL RUN AND HAS NEVER BEEN TIDIED.
           IF WS-BK-COUNT-USED NOT < WS-BK-MAX
               MOVE EC-OUT-OF-BALANCE TO WS-ERR-CODE
               MOVE 'W' TO WS-ERR-SEVERITY
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               SUBTRACT 1 FROM WS-REJECT-CNT
               MOVE 1 TO WS-BK-HIT
               MOVE 'Y' TO WS-BK-FOUND-SW
               GO TO P5260-EXIT.
           ADD 1 TO WS-BK-COUNT-USED.
           MOVE WS-BK-COUNT-USED TO WS-BK-HIT.
           SET WS-BK-IX TO WS-BK-HIT.
           MOVE CD-OCN TO WS-BK-OCN (WS-BK-IX).
           MOVE WS-SV-STATE TO WS-BK-STATE (WS-BK-IX).
           MOVE CD-JURIS-CD TO WS-BK-JURIS (WS-BK-IX).
           MOVE ZERO TO WS-BK-MOU (WS-BK-IX).
           MOVE ZERO TO WS-BK-PRIOR-AMT (WS-BK-IX).
           MOVE ZERO TO WS-BK-NEW-AMT (WS-BK-IX).
           MOVE ZERO TO WS-BK-DELTA (WS-BK-IX).
           MOVE ZERO TO WS-BK-COUNT (WS-BK-IX).
           MOVE 'Y' TO WS-BK-FOUND-SW.

       P5260-EXIT.
           EXIT.


      *****************************************************************
      * S600-ADJUSTMENT                                               *
      * BUILD AND WRITE THE ADJUSTMENT RECORDS.                       *
      *****************************************************************
       S600-ADJUSTMENT SECTION.

       P6000-BUILD-ADJUST.
      * BUILD ONE ADJUSTMENT DETAIL LINE FROM A BUCKET.  THE LINE
      * IS A BILL DETAIL RECORD WITH SECTION CODE RS AND A SINGLE
      * RATE ELEMENT CARRYING THE RESTATED AMOUNT.  THE DESCRIPTION
      * TEXT COMES FROM THE REASON CODE TABLE AND IS PRINTED ON THE
      * CARRIERS BILL EXACTLY AS IT APPEARS THERE.
           MOVE 'P6000-BUILD-ADJUST' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-BILL-DETAIL.
           SET WS-BK-IX TO WS-SUB4.
           MOVE WS-SV-BAN TO BD-BAN.
           MOVE WS-BILL-PERIOD TO BD-BILL-PERIOD.
           MOVE 'RS' TO BD-SECTION.
           ADD 1 TO WS-LINE-SEQ.
           MOVE WS-LINE-SEQ TO BD-LINE-SEQ.
           MOVE WS-BK-OCN (WS-BK-IX) TO BD-OCN.
           MOVE WS-BK-JURIS (WS-BK-IX) TO BD-JURIS-CD.
           MOVE WS-BK-STATE (WS-BK-IX) TO BD-STATE-CD.
           PERFORM P6050-REASON-TEXT THRU P6050-EXIT.
           MOVE WS-BK-MOU (WS-BK-IX) TO BD-TOT-MINUTES.
           MOVE WS-BK-DELTA (WS-BK-IX) TO BD-TOT-AMOUNT.
           COMPUTE BD-TOT-ROUNDED ROUNDED =
                   WS-BK-DELTA (WS-BK-IX).
           COMPUTE BD-ROUND-DELTA =
                   BD-TOT-AMOUNT - BD-TOT-ROUNDED.
           MOVE 1 TO BD-ELEM-CNT.
           MOVE 'RESTAT' TO BD-EL-RATE-ELEM (1).
           MOVE WS-BK-MOU (WS-BK-IX) TO BD-EL-QTY (1).
           MOVE ZERO TO BD-EL-RATE (1).
           MOVE WS-BK-DELTA (WS-BK-IX) TO BD-EL-AMOUNT (1).
           MOVE 'U' TO BD-EL-ROUND-RULE (1).
           MOVE 'CABJUR07' TO BD-EL-SRC-PROCESS (1).

       P6000-EXIT.
           EXIT.

       P6050-REASON-TEXT.
      * TRANSLATE THE REASON CODE INTO THE DESCRIPTION THAT PRINTS
      * ON THE BILL.  AN UNKNOWN CODE PRINTS AS REASON NOT SUPPLIED
      * RATHER THAN BLANK - A BLANK DESCRIPTION ON AN ADJUSTMENT
      * LINE IS AN AUTOMATIC CARRIER DISPUTE.
           MOVE SPACES TO BD-DESCRIPTION.
           SET WS-RS-IX TO 1.
           SEARCH WS-RS-ENTRY
               AT END
                   MOVE WS-RS-TEXT (24) TO BD-DESCRIPTION
                   GO TO P6050-EXIT
               WHEN WS-RS-CODE (WS-RS-IX) = WS-BK-REASON (WS-BK-IX)
                   MOVE WS-RS-TEXT (WS-RS-IX) TO BD-DESCRIPTION.

       P6050-EXIT.
           EXIT.

       P6100-WRITE-ADJUST.
      * WRITE THE ADJUSTMENT.  IN SIMULATION MODE NOTHING IS
      * WRITTEN - THE REGISTER IS PRODUCED AND THE FILE IS LEFT
      * EMPTY SO THAT THE ACCESS MANAGEMENT GROUP CAN SEE WHAT A
      * RESTATEMENT WOULD DO BEFORE IT IS COMMITTED.
           MOVE 'P6100-WRITE-ADJUST' TO WS-PARA-NAME.
           IF WS-PE-SIM-SW = 'Y'
               ADD 1 TO WS-ADJUST-CNT
               GO TO P6100-EXIT.
           MOVE CABS-BILL-DETAIL TO ADO-RECORD.
           WRITE ADO-RECORD.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 4705 TO WS-AB-CODE
               MOVE 'ADJUSTMENT WRITE FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-ADJUST-CNT.

       P6100-EXIT.
           EXIT.

       P6200-AUDIT-TRAIL.
      * ONE AUDIT RECORD PER REPRICED USAGE RECORD.  THIS FILE IS
      * THE ONLY PLACE THE PER RECORD ARITHMETIC IS PRESERVED - THE
      * ADJUSTMENT FILE CARRIES BUCKET TOTALS ONLY.  IT IS KEPT FOR
      * SEVEN YEARS AND IS THE FIRST THING AN AUDITOR ASKS FOR.
           MOVE 'P6200-AUDIT-TRAIL' TO WS-PARA-NAME.
           MOVE WS-RUN-ID TO WS-AU-RUN-ID.
           MOVE CD-OCN TO WS-AU-OCN.
           MOVE CD-BAN TO WS-AU-BAN.
           MOVE CD-SEQ-NBR TO WS-AU-SEQ.
           MOVE WS-RS-USE-YYDDD TO WS-AU-USE-YYDDD.
           MOVE WS-RW-BASE-MOU TO WS-AU-BASE-MOU.
           MOVE WS-RW-PRIOR-PIU TO WS-AU-PRIOR-PIU.
           MOVE WS-RW-NEW-PIU TO WS-AU-NEW-PIU.
           MOVE WS-RW-PRIOR-AMT TO WS-AU-PRIOR-AMT.
           MOVE WS-RW-NEW-AMT TO WS-AU-NEW-AMT.
           MOVE WS-RW-DELTA TO WS-AU-DELTA.
           MOVE WS-PE-REASON-CD TO WS-AU-REASON.
           MOVE WS-AUDIT-RECORD TO AUD-RECORD.
           WRITE AUD-RECORD.

       P6200-EXIT.
           EXIT.

       P6300-BUCKET-FLUSH.
      * FLUSH EVERY BUCKET TO THE ADJUSTMENT FILE.  CALLED AT EVERY
      * CARRIER BREAK AND AT END OF FILE.
           MOVE 'P6300-BUCKET-FLUSH' TO WS-PARA-NAME.
           IF WS-BK-COUNT-USED = ZERO
               GO TO P6300-EXIT.
           MOVE 1 TO WS-SUB4.
           PERFORM P6350-FLUSH-ONE THRU P6350-EXIT
               UNTIL WS-SUB4 > WS-BK-COUNT-USED.
           PERFORM P7300-TOTALS THRU P7300-EXIT.

       P6300-EXIT.
           EXIT.

       P6350-FLUSH-ONE.
      * ONE BUCKET.  A BUCKET WHOSE DELTA IS ZERO IS NOT WRITTEN.
           SET WS-BK-IX TO WS-SUB4.
           IF WS-BK-DELTA (WS-BK-IX) = ZERO
               ADD 1 TO WS-SUB4
               GO TO P6350-EXIT.
           PERFORM P6000-BUILD-ADJUST THRU P6000-EXIT.
           PERFORM P6100-WRITE-ADJUST THRU P6100-EXIT.
           ADD 1 TO WS-SUB4.

       P6350-EXIT.
           EXIT.


      *****************************************************************
      * S650-DATE-ROUTINES                                            *
      * JULIAN DATE SUPPORT.                                          *
      *****************************************************************
       S650-DATE-ROUTINES SECTION.

       P6500-JULIAN-TO-ABS.
      * CONVERT YYDDD TO AN ABSOLUTE DAY NUMBER.  THE PIVOT IS 70 -
      * A YEAR BELOW 70 IS TWENTY FIRST CENTURY.  THE SAME PIVOT IS
      * CODED IN SIX OTHER PLACES IN THE ESTATE AND IN CABDATCV.
      * CENTURY WINDOW CONFIRMED BY THE 1996 Y2K REMEDIATION.
           MOVE WS-JW-TEST TO WS-JW-TEST.
           IF WS-JW-TEST-YY < 70
               COMPUTE WS-JW-CCYY = 2000 + WS-JW-TEST-YY
           ELSE
               COMPUTE WS-JW-CCYY = 1900 + WS-JW-TEST-YY.
           COMPUTE WS-JW-ABS-TEST = ((WS-JW-CCYY - 1900) * 365)
                 + ((WS-JW-CCYY - 1901) / 4)
                 + WS-JW-TEST-DDD.

       P6500-EXIT.
           EXIT.

       P6510-LEAP-YEAR.
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

       P6510-EXIT.
           EXIT.

       P6520-ADD-DAYS.
      * ADD A NUMBER OF DAYS TO A YYDDD AND ROLL THE YEAR PROPERLY.
      * THIS PARAGRAPH EXISTS AND IS CORRECT.  IT IS USED BY THE
      * REVERSAL PROGRAM CABJUR08 THROUGH THE SHARED COPY MEMBER.
      * IT IS NOT USED BY P1400 WHEN THE WINDOW END IS DERIVED.
           MOVE WS-JW-HOLD TO WS-JW-TEST.
           PERFORM P6510-LEAP-YEAR THRU P6510-EXIT.
           COMPUTE WS-JW-TEST-DDD =
                   WS-JW-TEST-DDD + WS-JW-SPAN-DAYS.
           PERFORM P6525-ROLL-YEAR THRU P6525-EXIT
               UNTIL WS-JW-TEST-DDD NOT > WS-JW-DAYS-IN-YR.
           MOVE WS-JW-TEST TO WS-JW-HOLD.

       P6520-EXIT.
           EXIT.

       P6525-ROLL-YEAR.
      * ROLL ONE YEAR FORWARD, ALLOWING FOR THE LENGTH OF THE YEAR
      * BEING LEFT BEHIND.
           SUBTRACT WS-JW-DAYS-IN-YR FROM WS-JW-TEST-DDD.
           ADD 1 TO WS-JW-TEST-YY.
           IF WS-JW-TEST-YY > 99
               MOVE ZERO TO WS-JW-TEST-YY.
           PERFORM P6510-LEAP-YEAR THRU P6510-EXIT.

       P6525-EXIT.
           EXIT.

       P6530-DATE-DIFF.
      * DAYS BETWEEN TWO YYDDD VALUES, COMPUTED THROUGH THE
      * ABSOLUTE DAY NUMBERS SO THAT A YEAR BOUNDARY IS HANDLED.
           MOVE WS-JW-FROM TO WS-JW-TEST.
           PERFORM P6500-JULIAN-TO-ABS THRU P6500-EXIT.
           MOVE WS-JW-ABS-TEST TO WS-JW-ABS-FROM.
           MOVE WS-JW-THRU TO WS-JW-TEST.
           PERFORM P6500-JULIAN-TO-ABS THRU P6500-EXIT.
           MOVE WS-JW-ABS-TEST TO WS-JW-ABS-THRU.
           COMPUTE WS-JW-SPAN-DAYS =
                   WS-JW-ABS-THRU - WS-JW-ABS-FROM.

       P6530-EXIT.
           EXIT.

       P6540-YYDDD-TO-GREG.
      * CONVERT YYDDD TO CCYYMMDD FOR THE PRINTED REGISTER.  THE
      * MONTH TABLE CARRIES A LEAP AND A NON LEAP ROW.
           PERFORM P6510-LEAP-YEAR THRU P6510-EXIT.
           IF WS-JW-LEAP
               MOVE 2 TO WS-SUB1
           ELSE
               MOVE 1 TO WS-SUB1.
           MOVE 12 TO WS-SUB2.
           PERFORM P6545-FIND-MONTH THRU P6545-EXIT
               UNTIL WS-SUB2 < 1
                  OR WS-MT-DAYS-BEFORE (WS-SUB1, WS-SUB2)
                     < WS-JW-TEST-DDD.
           MOVE WS-JW-CCYY TO DW-GR-CCYY.
           MOVE WS-SUB2 TO DW-GR-MM.
           COMPUTE DW-GR-DD = WS-JW-TEST-DDD
                 - WS-MT-DAYS-BEFORE (WS-SUB1, WS-SUB2).

       P6540-EXIT.
           EXIT.

       P6545-FIND-MONTH.
      * STEP BACK ONE MONTH.
           SUBTRACT 1 FROM WS-SUB2.

       P6545-EXIT.
           EXIT.


      *****************************************************************
      * S700-REPORT                                                   *
      * THE PRINTED RESTATEMENT REGISTER.                             *
      *****************************************************************
       S700-REPORT SECTION.

       P7100-HEADING.
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

       P7100-EXIT.
           EXIT.

       P7200-DETAIL.
      * ONE LINE PER REPRICED USAGE RECORD.  AT FULL VOLUME THIS
      * REPORT RUNS TO THOUSANDS OF PAGES, WHICH IS WHY THE DETAIL
      * LINE IS SUPPRESSED UNLESS THE DELTA IS MATERIAL.
           IF WS-LINE-CNT > WS-MAX-LINES
               PERFORM P7100-HEADING THRU P7100-EXIT.
           MOVE CD-OCN TO WS-D1-OCN.
           MOVE WS-SV-STATE TO WS-D1-STATE.
           MOVE CD-JURIS-CD TO WS-D1-JURIS.
           MOVE WS-RS-USE-YYDDD TO WS-D1-USEDT.
           MOVE WS-RW-BASE-MOU TO WS-D1-MOU.
           MOVE WS-RW-PRIOR-PIU TO WS-D1-PRIOR-PIU.
           MOVE WS-RW-NEW-PIU TO WS-D1-NEW-PIU.
           MOVE WS-RW-PRIOR-AMT TO WS-D1-PRIOR-AMT.
           MOVE WS-RW-NEW-AMT TO WS-D1-NEW-AMT.
           MOVE WS-RW-DELTA TO WS-D1-DELTA.
           WRITE PRT-RECORD FROM WS-DETAIL-1 AFTER ADVANCING 1 LINES.
           ADD 1 TO WS-LINE-CNT.

       P7200-EXIT.
           EXIT.

       P7300-TOTALS.
      * CARRIER LEVEL TOTALS AT EVERY BREAK.
           MOVE SPACES TO WS-TOTAL-1.
           MOVE 'CARRIER TOTAL - DEBIT' TO WS-T1-DESC.
           MOVE WS-REPRICE-CNT TO WS-T1-COUNT.
           MOVE WS-TOT-DEBIT-AMT TO WS-T1-AMOUNT.
           WRITE PRT-RECORD FROM WS-TOTAL-1 AFTER ADVANCING 2 LINES.
           MOVE 'CARRIER TOTAL - CREDIT' TO WS-T1-DESC.
           MOVE WS-ADJUST-CNT TO WS-T1-COUNT.
           MOVE WS-TOT-CREDIT-AMT TO WS-T1-AMOUNT.
           WRITE PRT-RECORD FROM WS-TOTAL-1 AFTER ADVANCING 1 LINES.
           ADD 3 TO WS-LINE-CNT.

       P7300-EXIT.
           EXIT.

       P7400-EXCEPTION-LINE.
      * PRINT AN EXCEPTION MESSAGE FROM THE MESSAGE TABLE.
           MOVE SPACES TO WS-TOTAL-1.
           MOVE WS-MSG-TEXT (WS-MSG-SUB) TO WS-T1-DESC.
           WRITE PRT-RECORD FROM WS-TOTAL-1 AFTER ADVANCING 1 LINES.
           ADD 1 TO WS-LINE-CNT.

       P7400-EXIT.
           EXIT.


      *****************************************************************
      * S250-SEQUENCE-CHECK                                           *
      * SEQUENCE AND DUPLICATE DETECTION ON THE USAGE FILE.           *
      *****************************************************************
       S250-SEQUENCE-CHECK SECTION.

       P2700-SEQUENCE-CHECK.
      * THE USAGE FILE MUST BE IN OCN, BAN, SEQUENCE ORDER.  A FILE
      * OUT OF SEQUENCE PRODUCES BUCKETS THAT ARE FLUSHED EARLY AND
      * CARRIERS THAT RECEIVE TWO ADJUSTMENT LINES FOR ONE PERIOD.
      * THE CHECK COUNTS RATHER THAN ABENDS BECAUSE THE 2006 MERGE
      * OF THE TWO REGIONAL USAGE FILES IS KNOWN TO PRODUCE A SMALL
      * NUMBER OF OUT OF SEQUENCE RECORDS AT THE JOIN.
           MOVE 'P2700-SEQUENCE-CHECK' TO WS-PARA-NAME.
           IF CD-OCN < WS-SQ-LAST-OCN
               ADD 1 TO WS-SQ-OUT-CNT
               GO TO P2750-SAVE-SEQ.
           IF CD-OCN > WS-SQ-LAST-OCN
               GO TO P2750-SAVE-SEQ.
           IF CD-BAN < WS-SQ-LAST-BAN
               ADD 1 TO WS-SQ-OUT-CNT
               GO TO P2750-SAVE-SEQ.
           IF CD-BAN > WS-SQ-LAST-BAN
               GO TO P2750-SAVE-SEQ.
           IF CD-SEQ-NBR = WS-SQ-LAST-SEQ
               ADD 1 TO WS-SQ-DUP-CNT
               MOVE EC-DUP-SEQ TO WS-ERR-CODE
               MOVE 'W' TO WS-ERR-SEVERITY.
           IF CD-SEQ-NBR < WS-SQ-LAST-SEQ
               ADD 1 TO WS-SQ-OUT-CNT.

       P2750-SAVE-SEQ.
      * REMEMBER THE KEY FOR THE NEXT COMPARE.  ENTERED BY FALL
      * THROUGH FROM P2700 AND BY GO TO FROM FOUR PLACES IN IT.
           MOVE CD-OCN TO WS-SQ-LAST-OCN.
           MOVE CD-BAN TO WS-SQ-LAST-BAN.
           MOVE CD-SEQ-NBR TO WS-SQ-LAST-SEQ.

       P2750-EXIT.
           EXIT.

       P2800-MERGER-XREF.
      * A RESTATEMENT REACHING BACK ACROSS A MERGER MUST RAISE THE
      * ADJUSTMENT AGAINST THE SURVIVING OCN.  THE MERGER YEAR IS
      * HELD AS TWO DIGITS AND IS EXPANDED WITH THE PIVOT OF 70
      * BEFORE IT IS COMPARED WITH THE USAGE YEAR.  A MERGER IN
      * 1997 AND USAGE IN 2018 MUST NOT COMPARE AS 97 GREATER
      * THAN 18.
           MOVE 'P2800-MERGER-XREF' TO WS-PARA-NAME.
           MOVE CD-OCN TO WS-MW-BILLED-OCN.
           MOVE CD-OCN TO WS-MW-SURVIVE-OCN.
           SET WS-MG-IX TO 1.
           SEARCH WS-MG-ENTRY
               AT END
                   GO TO P2800-EXIT
               WHEN WS-MG-OLD-OCN (WS-MG-IX) = CD-OCN
                   MOVE WS-MG-NEW-OCN (WS-MG-IX) TO WS-MW-SURVIVE-OCN
                   MOVE WS-MG-YY (WS-MG-IX) TO WS-MW-MERGE-YY.
           MOVE CD-CONN-YY TO WS-MW-USE-YY.
           IF WS-MW-MERGE-YY < 70
               COMPUTE WS-MW-MERGE-CCYY = 2000 + WS-MW-MERGE-YY
           ELSE
               COMPUTE WS-MW-MERGE-CCYY = 1900 + WS-MW-MERGE-YY.
           IF WS-MW-USE-YY < 70
               COMPUTE WS-MW-USE-CCYY = 2000 + WS-MW-USE-YY
           ELSE
               COMPUTE WS-MW-USE-CCYY = 1900 + WS-MW-USE-YY.
           IF WS-MW-USE-CCYY < WS-MW-MERGE-CCYY
               ADD 1 TO WS-MW-APPLIED-CNT
               MOVE WS-MW-SURVIVE-OCN TO CD-OCN.

       P2800-EXIT.
           EXIT.


      *****************************************************************
      * S550-PLU-RESTATEMENT                                          *
      * RESTATE THE LOCAL AND TOLL SPLIT AS WELL.                     *
      *****************************************************************
       S550-PLU-RESTATEMENT SECTION.

       P5500-PLU-RESTATE.
      * A FACTOR FILING REVISES THE PLU AT THE SAME TIME AS THE PIU.
      * THE INTRASTATE MINUTES PRODUCED BY THE NEW PIU HAVE TO BE
      * RESPLIT BETWEEN LOCAL AND TOLL UNDER THE NEW PLU, AND THE
      * DIFFERENCE BETWEEN THAT AND WHAT WAS BILLED IS A SECOND
      * DELTA.  IT IS ADDED TO THE SAME BUCKET AS THE PIU DELTA.
           MOVE 'P5500-PLU-RESTATE' TO WS-PARA-NAME.
           IF WS-RW-PRIOR-PLU = WS-RW-NEW-PLU
               MOVE ZERO TO WS-PR-DELTA
               GO TO P5500-EXIT.
           IF NOT CD-INTRASTATE
               MOVE ZERO TO WS-PR-DELTA
               GO TO P5500-EXIT.
           PERFORM P5510-PLU-RATES THRU P5510-EXIT.
           PERFORM P5520-PLU-PRIOR THRU P5520-EXIT.
           PERFORM P5530-PLU-NEW THRU P5530-EXIT.
           PERFORM P5540-PLU-DELTA THRU P5540-EXIT.

       P5500-EXIT.
           EXIT.

       P5510-PLU-RATES.
      * LOCAL AND TOLL RATES FOR THE USAGE DATE.  THE LOCAL RATE IS
      * KEYED WITH JURISDICTION L AND THE TOLL RATE WITH S.
           MOVE ZERO TO WS-PR-LC-RATE.
           MOVE ZERO TO WS-PR-TL-RATE.
           MOVE WS-PE-RATE-TARIFF-X TO WS-RK-TARIFF.
           MOVE CD-RATE-ELEM TO WS-RK-ELEM.
           MOVE 'L' TO WS-RK-JURIS.
           MOVE WS-SV-STATE TO WS-RK-STATE.
           MOVE WS-RS-USE-YYDDD TO WS-RK-EFF.
           MOVE WS-RATE-KEY TO RTM-KEY.
           READ RATE-MASTER INTO CABS-RATE-RECORD
               INVALID KEY
                   PERFORM P4600-BAND-FALLBACK THRU P4600-EXIT
                   GO TO P5510-EXIT.
           MOVE RT-INITIAL-RATE TO WS-PR-LC-RATE.
           MOVE WS-RW-SS-RATE TO WS-PR-TL-RATE.

       P5510-EXIT.
           EXIT.

       P5520-PLU-PRIOR.
      * THE LOCAL AND TOLL SPLIT AS BILLED.
           MOVE WS-RW-NW-SS-MOU TO WS-PR-BASE-MOU.
           COMPUTE WS-PR-PR-LC-MOU ROUNDED =
                   WS-PR-BASE-MOU * WS-RW-PRIOR-PLU / 100.
           COMPUTE WS-PR-PR-TL-MOU =
                   WS-PR-BASE-MOU - WS-PR-PR-LC-MOU.
           COMPUTE WS-PR-PRIOR-AMT ROUNDED =
                   (WS-PR-PR-LC-MOU * WS-PR-LC-RATE)
                 + (WS-PR-PR-TL-MOU * WS-PR-TL-RATE).

       P5520-EXIT.
           EXIT.

       P5530-PLU-NEW.
      * THE SPLIT UNDER THE NEW PLU.
           COMPUTE WS-PR-NW-LC-MOU ROUNDED =
                   WS-PR-BASE-MOU * WS-RW-NEW-PLU / 100.
           COMPUTE WS-PR-NW-TL-MOU =
                   WS-PR-BASE-MOU - WS-PR-NW-LC-MOU.
           COMPUTE WS-PR-NEW-AMT ROUNDED =
                   (WS-PR-NW-LC-MOU * WS-PR-LC-RATE)
                 + (WS-PR-NW-TL-MOU * WS-PR-TL-RATE).

       P5530-EXIT.
           EXIT.

       P5540-PLU-DELTA.
      * THE SECOND DELTA.  IT IS ADDED INTO THE PIU DELTA BEFORE THE
      * BUCKET IS UPDATED SO THAT ONE ADJUSTMENT LINE COVERS BOTH.
           COMPUTE WS-PR-DELTA ROUNDED =
                   WS-PR-NEW-AMT - WS-PR-PRIOR-AMT.
           ADD WS-PR-DELTA TO WS-PR-TOT-DELTA.
           ADD 1 TO WS-PR-COUNT.
           ADD WS-PR-DELTA TO WS-RW-DELTA.
           IF WS-RW-DELTA < ZERO
               COMPUTE WS-RW-DELTA-ABS = WS-RW-DELTA * -1
           ELSE
               MOVE WS-RW-DELTA TO WS-RW-DELTA-ABS.

       P5540-EXIT.
           EXIT.


      *****************************************************************
      * S460-BAND-AND-STATE                                           *
      * VOLUME BAND FALLBACK AND STATE RULE TESTS.                    *
      *****************************************************************
       S460-BAND-AND-STATE SECTION.

       P4600-BAND-FALLBACK.
      * WHEN THE RATE MASTER HAS NOTHING FOR THE USAGE DATE THE
      * 1991 FALLBACK MATRIX IS USED.  THE VOLUME BAND ADJUSTMENT
      * THAT WAS SUPPOSED TO REFINE IT WAS SPECIFIED IN 2013 AND
      * NEVER FINISHED - THE BAND IS LOCATED AND THEN IGNORED.
           MOVE 'P4600-BAND-FALLBACK' TO WS-PARA-NAME.
           SET WS-RD-IX TO 1.
           SEARCH WS-RD-ENTRY
               AT END
                   MOVE ZERO TO WS-PR-LC-RATE
                   GO TO P4600-EXIT
               WHEN WS-RD-ELEM (WS-RD-IX) = CD-RATE-ELEM
                   MOVE WS-RD-RATE (WS-RD-IX) TO WS-PR-LC-RATE.
           SET WS-VB-IX TO 1.
           SEARCH WS-VB-ENTRY
               AT END
                   GO TO P4600-EXIT
               WHEN WS-VB-FROM (WS-VB-IX) NOT > WS-RW-BASE-MOU
                   MOVE WS-VB-PCT (WS-VB-IX) TO WS-VB-HOLD-PCT.

       P4600-EXIT.
           EXIT.

       P4700-STATE-RULE.
      * SOME COMMISSIONS LIMIT HOW FAR BACK A RESTATEMENT MAY REACH
      * AND SOME REQUIRE AN ORDER BEFORE ONE MAY BE RAISED AT ALL.
      * THE LIMIT IS EXPRESSED IN MONTHS AND IS COMPARED AGAINST THE
      * AGE OF THE USAGE IN DAYS DIVIDED BY THIRTY - AN APPROXIMATION
      * THAT HAS BEEN QUERIED TWICE AND LEFT ALONE BOTH TIMES.
           MOVE 'P4700-STATE-RULE' TO WS-PARA-NAME.
           MOVE 'Y' TO WS-MATERIAL-SW.
           SET WS-SR-IX TO 1.
           SEARCH WS-SR-ENTRY
               AT END
                   GO TO P4700-EXIT
               WHEN WS-SR-STATE (WS-SR-IX) = WS-SV-STATE
                   MOVE WS-SR-MONTH-LIMIT (WS-SR-IX) TO WS-SR-HOLD-LIM
                   MOVE WS-SR-ORDER-REQD (WS-SR-IX) TO WS-SR-HOLD-ORD.
           IF WS-SR-HOLD-LIM = ZERO
               GO TO P4700-EXIT.
           MOVE WS-RS-USE-YYDDD TO WS-JW-FROM.
           MOVE WS-CYCLE-YYDDD TO WS-JW-THRU.
           PERFORM P6530-DATE-DIFF THRU P6530-EXIT.
           DIVIDE WS-JW-SPAN-DAYS BY 30 GIVING WS-SR-HOLD-MONTHS.
           IF WS-SR-HOLD-MONTHS > WS-SR-HOLD-LIM
               MOVE 'N' TO WS-MATERIAL-SW
               ADD 1 TO WS-SR-BARRED-CNT.

       P4700-EXIT.
           EXIT.


      *****************************************************************
      * S770-SUMMARY                                                  *
      * PER STATE SUMMARY PAGE AT END OF RUN.                         *
      *****************************************************************
       S770-SUMMARY SECTION.

       P7700-SUMMARY-ACCUM.
      * ACCUMULATE THE STATE SUMMARY.  SIXTY ENTRIES COVERS THE
      * FIFTY STATES, THE DISTRICT AND THE FOUR TERRITORIES WITH
      * ROOM LEFT OVER FOR THE BLANK STATE THAT THE INDETERMINATE
      * RECORDS CARRY.
           MOVE 'P7700-SUMMARY-ACCUM' TO WS-PARA-NAME.
           MOVE 'N' TO WS-SM-FOUND-SW.
           MOVE 1 TO WS-SUB3.
           PERFORM P7710-SUMMARY-FIND THRU P7710-EXIT
               UNTIL WS-SUB3 > WS-SM-USED
                  OR WS-SM-FOUND.
           IF NOT WS-SM-FOUND
               IF WS-SM-USED < 60
                   ADD 1 TO WS-SM-USED
                   MOVE WS-SM-USED TO WS-SM-HIT
                   SET WS-SM-IX TO WS-SM-HIT
                   MOVE WS-SV-STATE TO WS-SM-STATE (WS-SM-IX)
                   MOVE ZERO TO WS-SM-MOU (WS-SM-IX)
                   MOVE ZERO TO WS-SM-PRIOR (WS-SM-IX)
                   MOVE ZERO TO WS-SM-NEW (WS-SM-IX)
                   MOVE ZERO TO WS-SM-DELTA (WS-SM-IX)
                   MOVE ZERO TO WS-SM-COUNT (WS-SM-IX)
                   MOVE 'Y' TO WS-SM-FOUND-SW
               ELSE
                   GO TO P7700-EXIT.
           SET WS-SM-IX TO WS-SM-HIT.
           ADD WS-RW-BASE-MOU TO WS-SM-MOU (WS-SM-IX).
           ADD WS-RW-PRIOR-AMT TO WS-SM-PRIOR (WS-SM-IX).
           ADD WS-RW-NEW-AMT TO WS-SM-NEW (WS-SM-IX).
           ADD WS-RW-DELTA TO WS-SM-DELTA (WS-SM-IX).
           ADD 1 TO WS-SM-COUNT (WS-SM-IX).

       P7700-EXIT.
           EXIT.

       P7710-SUMMARY-FIND.
      * ONE SUMMARY ENTRY COMPARE.
           IF WS-SM-STATE (WS-SUB3) = WS-SV-STATE
               MOVE 'Y' TO WS-SM-FOUND-SW
               MOVE WS-SUB3 TO WS-SM-HIT
               GO TO P7710-EXIT.
           ADD 1 TO WS-SUB3.

       P7710-EXIT.
           EXIT.

       P7800-SUMMARY-PRINT.
      * PRINT THE STATE SUMMARY PAGE.  THIS IS THE PAGE THE ACCESS
      * MANAGEMENT GROUP ACTUALLY READS - THE DETAIL PAGES ARE
      * MICROFICHED AND NEVER LOOKED AT AGAIN.
           MOVE 'P7800-SUMMARY-PRINT' TO WS-PARA-NAME.
           PERFORM P7100-HEADING THRU P7100-EXIT.
           MOVE 1 TO WS-SUB3.
           PERFORM P7810-SUMMARY-LINE THRU P7810-EXIT
               UNTIL WS-SUB3 > WS-SM-USED.

       P7800-EXIT.
           EXIT.

       P7810-SUMMARY-LINE.
      * ONE STATE SUMMARY LINE.
           SET WS-SM-IX TO WS-SUB3.
           MOVE SPACES TO WS-DETAIL-1.
           MOVE WS-SM-STATE (WS-SM-IX) TO WS-D1-STATE.
           MOVE WS-SM-MOU (WS-SM-IX) TO WS-D1-MOU.
           MOVE WS-SM-PRIOR (WS-SM-IX) TO WS-D1-PRIOR-AMT.
           MOVE WS-SM-NEW (WS-SM-IX) TO WS-D1-NEW-AMT.
           MOVE WS-SM-DELTA (WS-SM-IX) TO WS-D1-DELTA.
           WRITE PRT-RECORD FROM WS-DETAIL-1 AFTER ADVANCING 1 LINES.
           ADD 1 TO WS-LINE-CNT.
           ADD 1 TO WS-SUB3.

       P7810-EXIT.
           EXIT.


      *****************************************************************
      * S350-CARRIER-VALIDATION                                       *
      * VALIDATE THE CARRIER AND THE CIC ON THE USAGE RECORD.         *
      *****************************************************************
       S350-CARRIER-VALIDATION SECTION.

       P3500-CIC-VALIDATE.
      * THE SWITCH RECORDS A CIC.  THE BILL IS RAISED AGAINST AN
      * OCN.  WHEN THE TWO DISAGREE THE OCN ON THE RECORD IS TRUSTED
      * AND THE DISAGREEMENT IS COUNTED - CHANGING IT HERE WOULD
      * MOVE REVENUE BETWEEN CARRIERS IN A RESTATEMENT RUN, WHICH IS
      * NOT SOMETHING A RESTATEMENT IS ALLOWED TO DO.
           MOVE 'P3500-CIC-VALIDATE' TO WS-PARA-NAME.
           MOVE 'N' TO WS-CW-FOUND-SW.
           MOVE CD-VC-CIC TO WS-CW-CIC.
           IF WS-CW-CIC = ZERO
               GO TO P3500-EXIT.
           SET WS-CX-IX TO 1.
           SEARCH WS-CX-ENTRY
               AT END
                   MOVE EC-OCN-UNKNOWN TO WS-ERR-CODE
                   MOVE 'W' TO WS-ERR-SEVERITY
                   GO TO P3500-EXIT
               WHEN WS-CX-CIC (WS-CX-IX) = WS-CW-CIC
                   MOVE WS-CX-OCN (WS-CX-IX) TO WS-CW-OCN
                   MOVE 'Y' TO WS-CW-FOUND-SW.
           IF WS-CW-FOUND
               IF WS-CW-OCN NOT = CD-OCN
                   ADD 1 TO WS-CW-MISMATCH-CNT.

       P3500-EXIT.
           EXIT.

       P3600-LATA-OVERRIDE.
      * APPLY THE LATA PAIR OVERRIDE MATRIX.  A CORRIDOR PAIR IS
      * TREATED AS INTRASTATE EVEN THOUGH THE TWO LATAS SIT IN
      * DIFFERENT STATES.  THE OVERRIDE MUST BE APPLIED BEFORE THE
      * FACTOR IS RESOLVED, NOT AFTER - THE FACTOR IS KEYED BY THE
      * STATE THE OVERRIDE DECIDES.
           MOVE 'P3600-LATA-OVERRIDE' TO WS-PARA-NAME.
           MOVE 1 TO WS-SUB3.
           PERFORM P3650-LATA-COMPARE THRU P3650-EXIT
               UNTIL WS-SUB3 > 180.

       P3600-EXIT.
           EXIT.

       P3650-LATA-COMPARE.
      * ONE LATA PAIR COMPARE.  THE MATRIX IS UNSORTED SO EVERY
      * ENTRY IS EXAMINED FOR EVERY USAGE RECORD.  AT PRODUCTION
      * VOLUME THIS IS THE MOST EXPENSIVE LOOP IN THE PROGRAM AND
      * HAS BEEN RAISED AS A PERFORMANCE ITEM THREE TIMES.
           IF WS-LM-A-LATA (WS-SUB3) = CD-VC-ORIG-LATA AND
              WS-LM-Z-LATA (WS-SUB3) = CD-VC-TERM-LATA
               MOVE WS-LM-JURIS (WS-SUB3) TO CD-JURIS-CD
               MOVE 999 TO WS-SUB3
               GO TO P3650-EXIT.
           ADD 1 TO WS-SUB3.

       P3650-EXIT.
           EXIT.

       P3700-ELEMENT-DESC.
      * FETCH THE TARIFF DESCRIPTION FOR THE RATE ELEMENT SO THAT
      * THE ADJUSTMENT LINE CARRIES TARIFF LANGUAGE.
           MOVE SPACES TO WS-ED-HOLD.
           SET WS-ED-IX TO 1.
           SEARCH WS-ED-ENTRY
               AT END
                   MOVE 'UNCLASSIFIED ACCESS ELEMENT' TO WS-ED-HOLD
                   GO TO P3700-EXIT
               WHEN WS-ED-ELEM (WS-ED-IX) = CD-RATE-ELEM
                   MOVE WS-ED-DESC (WS-ED-IX) TO WS-ED-HOLD.

       P3700-EXIT.
           EXIT.


      *****************************************************************
      * S480-SPECIAL-ACCESS                                           *
      * RESTATEMENT OF SPECIAL ACCESS AND UNBUNDLED RECORDS.          *
      *****************************************************************
       S480-SPECIAL-ACCESS SECTION.

       P4800-SPECIAL-RESTATE.
      * SPECIAL ACCESS IS A RECURRING CHARGE, NOT USAGE.  WHAT GETS
      * RESTATED IS THE MEET POINT PERCENTAGE, NOT THE PIU.  THE
      * PERCENTAGE COMES OFF THE USAGE RECORD ITSELF BECAUSE THE
      * CIRCUIT INVENTORY IS OWNED BY THE SETTLEMENT APPLICATION
      * AND THIS PROGRAM MAY NOT READ IT DIRECTLY.
           MOVE 'P4800-SPECIAL-RESTATE' TO WS-PARA-NAME.
           IF NOT CD-SPECIAL-ACC
               GO TO P4800-EXIT.
           IF CD-SP-MPB-IND NOT = 'Y'
               GO TO P4800-EXIT.
           MOVE CD-SP-QTY TO WS-SA-QTY.
           MOVE CD-SP-MPB-PCT TO WS-SA-MPB-PCT.
           PERFORM P4810-SPECIAL-RATE THRU P4810-EXIT.
           PERFORM P4820-SPECIAL-PRIOR THRU P4820-EXIT.
           PERFORM P4830-SPECIAL-NEW THRU P4830-EXIT.
           PERFORM P4840-SPECIAL-DELTA THRU P4840-EXIT.

       P4800-EXIT.
           EXIT.

       P4810-SPECIAL-RATE.
      * THE RECURRING RATE FOR THE USOC ON THE USAGE DATE.
           MOVE ZERO TO WS-SA-RATE.
           MOVE WS-PE-RATE-TARIFF-X TO WS-RK-TARIFF.
           MOVE CD-RATE-ELEM TO WS-RK-ELEM.
           MOVE 'I' TO WS-RK-JURIS.
           MOVE WS-SV-STATE TO WS-RK-STATE.
           MOVE WS-RS-USE-YYDDD TO WS-RK-EFF.
           MOVE WS-RATE-KEY TO RTM-KEY.
           READ RATE-MASTER INTO CABS-RATE-RECORD
               INVALID KEY
                   MOVE ZERO TO WS-SA-RATE
                   GO TO P4810-EXIT.
           MOVE RT-SETUP-CHG TO WS-SA-RATE.
           IF WS-SA-RATE = ZERO
               MOVE RT-INITIAL-RATE TO WS-SA-RATE.

       P4810-EXIT.
           EXIT.

       P4820-SPECIAL-PRIOR.
      * THE AMOUNT AS BILLED, AT THE MEET POINT PERCENTAGE THAT WAS
      * ON THE CIRCUIT AT THE TIME.
           COMPUTE WS-SA-PRIOR-AMT ROUNDED =
                   WS-SA-QTY * WS-SA-RATE * WS-SA-MPB-PCT / 100.

       P4820-EXIT.
           EXIT.

       P4830-SPECIAL-NEW.
      * THE AMOUNT AT THE REVISED PERCENTAGE.  THE REVISED VALUE IS
      * PASSED IN THROUGH THE FACTOR RECORD PSU FIELD, WHICH IS THE
      * ONLY SPARE PERCENTAGE FIELD ON THE LAYOUT.  THAT REUSE WAS
      * MEANT TO BE TEMPORARY IN 1998.
           IF WS-RW-NEW-PLU = ZERO
               MOVE WS-SA-MPB-PCT TO WS-SA-NEW-PCT
           ELSE
               MOVE WS-RW-NEW-PLU TO WS-SA-NEW-PCT.
           COMPUTE WS-SA-NEW-AMT ROUNDED =
                   WS-SA-QTY * WS-SA-RATE * WS-SA-NEW-PCT / 100.

       P4830-EXIT.
           EXIT.

       P4840-SPECIAL-DELTA.
      * THE SPECIAL ACCESS DELTA.  IT IS ACCUMULATED SEPARATELY AND
      * REPORTED SEPARATELY BECAUSE THE SETTLEMENT APPLICATION HAS
      * TO RAISE THE OTHER HALF OF IT AGAINST THE OTHER LEC.
           COMPUTE WS-SA-DELTA ROUNDED =
                   WS-SA-NEW-AMT - WS-SA-PRIOR-AMT.
           ADD WS-SA-DELTA TO WS-SA-TOT-DELTA.
           ADD 1 TO WS-SA-COUNT.
           ADD WS-SA-DELTA TO WS-RW-DELTA.

       P4840-EXIT.
           EXIT.


      *****************************************************************
      * S780-EXCEPTION-REGISTER                                       *
      * END OF RUN EXCEPTION REGISTER.                                *
      *****************************************************************
       S780-EXCEPTION-REGISTER SECTION.

       P7900-EXCEPTION-BUMP.
      * BUMP THE COUNT FOR ONE MESSAGE TABLE ENTRY.
           IF WS-XC-SUB < 1 OR WS-XC-SUB > 20
               GO TO P7900-EXIT.
           SET WS-XC-IX TO WS-XC-SUB.
           ADD 1 TO WS-XC-COUNT (WS-XC-IX).
           ADD 1 TO WS-XC-TOTAL.

       P7900-EXIT.
           EXIT.

       P7910-EXCEPTION-PRINT.
      * PRINT EVERY NON ZERO EXCEPTION COUNT WITH ITS MESSAGE TEXT.
      * THIS PAGE IS THE ONLY EVIDENCE THAT A RESTATEMENT SILENTLY
      * SKIPPED ANYTHING.  IT IS PRINTED EVEN WHEN EVERY COUNT IS
      * ZERO SO THAT ITS ABSENCE MEANS THE STEP DID NOT FINISH.
           MOVE 'P7910-EXCEPTION-PRINT' TO WS-PARA-NAME.
           PERFORM P7100-HEADING THRU P7100-EXIT.
           MOVE 1 TO WS-SUB3.
           PERFORM P7920-EXCEPTION-LINE THRU P7920-EXIT
               UNTIL WS-SUB3 > 20.

       P7910-EXIT.
           EXIT.

       P7920-EXCEPTION-LINE.
      * ONE EXCEPTION LINE.
           SET WS-XC-IX TO WS-SUB3.
           IF WS-XC-COUNT (WS-XC-IX) = ZERO
               ADD 1 TO WS-SUB3
               GO TO P7920-EXIT.
           MOVE SPACES TO WS-TOTAL-1.
           MOVE WS-MSG-TEXT (WS-SUB3) TO WS-T1-DESC.
           MOVE WS-XC-COUNT (WS-XC-IX) TO WS-T1-COUNT.
           MOVE ZERO TO WS-T1-AMOUNT.
           WRITE PRT-RECORD FROM WS-TOTAL-1 AFTER ADVANCING 1 LINES.
           ADD 1 TO WS-LINE-CNT.
           ADD 1 TO WS-SUB3.

       P7920-EXIT.
           EXIT.


      *****************************************************************
      * S290-BILLED-VALIDATION                                        *
      * VALIDATE THE BILLED DETAIL LINE BEFORE REPRICING IT.          *
      *****************************************************************
       S290-BILLED-VALIDATION SECTION.

       P2900-VALIDATE-BILL.
      * THE BILLED DETAIL LINE IS THE BASIS THE RESTATEMENT WORKS
      * FROM.  IF ITS ELEMENTS DO NOT ADD UP TO ITS TOTAL THEN THE
      * ORIGINAL BILL WAS WRONG AND RESTATING IT WILL COMPOUND THE
      * ERROR RATHER THAN CORRECT IT.  SUCH LINES ARE COUNTED AND
      * STILL PROCESSED - STOPPING WOULD LEAVE THE CARRIER WITH AN
      * UNCORRECTED BILL, WHICH IS WORSE.
           MOVE 'P2900-VALIDATE-BILL' TO WS-PARA-NAME.
           IF NOT WS-BILL-MATCHED
               GO TO P2900-EXIT.
           MOVE ZERO TO WS-BV-ELEM-TOTAL.
           IF BD-ELEM-CNT = ZERO
               ADD 1 TO WS-BV-ZERO-CNT
               GO TO P2900-EXIT.
           MOVE 1 TO WS-ELEM-SUB.
           PERFORM P2910-ELEM-TOTAL THRU P2910-EXIT
               UNTIL WS-ELEM-SUB > WS-EW-COUNT.
           MOVE BD-TOT-AMOUNT TO WS-BV-LINE-TOTAL.
           COMPUTE WS-BV-VARIANCE =
                   WS-BV-LINE-TOTAL - WS-BV-ELEM-TOTAL.
           IF WS-BV-VARIANCE NOT = ZERO
               ADD 1 TO WS-BV-BAD-CNT.
           IF BD-ELEM-CNT > 40
               ADD 1 TO WS-BV-TRUNC-CNT.

       P2900-EXIT.
           EXIT.

       P2910-ELEM-TOTAL.
      * ADD ONE ELEMENT AMOUNT.
           ADD BD-EL-AMOUNT (WS-ELEM-SUB) TO WS-BV-ELEM-TOTAL.
           ADD 1 TO WS-ELEM-SUB.

       P2910-EXIT.
           EXIT.

       P2950-BAN-STATE.
      * DERIVE THE STATE FROM THE BILLING ACCOUNT NUMBER.  EVERY
      * FACTOR LOOKUP IN THIS PROGRAM DEPENDS ON THE STATE AND THE
      * ONLY PLACE IT CAN COME FROM IS THIS TABLE.  A MISS LEAVES
      * THE STATE BLANK, WHICH SILENTLY MOVES THE RECORD ONTO THE
      * CARRIER LEVEL FACTOR INSTEAD OF THE STATE LEVEL ONE.
           MOVE 'P2950-BAN-STATE' TO WS-PARA-NAME.
           MOVE 'N' TO WS-BV-FOUND-SW.
           MOVE SPACES TO WS-BV-STATE-HIT.
           SET WS-BX-IX TO 1.
           SEARCH WS-BX-ENTRY
               AT END
                   MOVE SPACES TO WS-SV-STATE
                   GO TO P2950-EXIT
               WHEN WS-BX-BAN (WS-BX-IX) = CD-BAN
                   MOVE WS-BX-STATE (WS-BX-IX) TO WS-BV-STATE-HIT
                   MOVE 'Y' TO WS-BV-FOUND-SW.
           IF WS-BV-FOUND
               MOVE WS-BV-STATE-HIT TO WS-SV-STATE.

       P2950-EXIT.
           EXIT.

       P2960-STATE-CHECK.
      * CROSS CHECK THE DERIVED STATE AGAINST THE LATA TABLE.  WHEN
      * THEY DISAGREE THE BAN TABLE WINS - IT IS THE BILLING VIEW
      * AND THE BILL IS WHAT IS BEING RESTATED.
           MOVE CD-VC-ORIG-LATA TO WS-LT-SEARCH-X.
           SET WS-LT-IX TO 1.
           SEARCH ALL WS-LT-ENTRY
               AT END
                   GO TO P2960-EXIT
               WHEN WS-LT-LATA (WS-LT-IX) = WS-LT-SEARCH-X
                   MOVE WS-LT-STATE (WS-LT-IX) TO WS-LT-RESULT-X.
           IF WS-LT-RESULT-X NOT = WS-SV-STATE
               IF WS-SV-STATE = SPACES
                   MOVE WS-LT-RESULT-X TO WS-SV-STATE.

       P2960-EXIT.
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
           MOVE 070                    TO CT-STEP-SEQ.
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
           DISPLAY 'WINDOW FROM YYDDD ' WS-RS-FROM-YYDDD.
           DISPLAY 'WINDOW THRU YYDDD ' WS-RS-THRU-YYDDD.
           DISPLAY 'WINDOW SPAN DAYS  ' WS-RS-SPAN-DAYS.
           DISPLAY 'IN WINDOW         ' WS-INWIN-CNT.
           DISPLAY 'OUT OF WINDOW     ' WS-OUTWIN-CNT.
           DISPLAY 'REPRICED          ' WS-REPRICE-CNT.
           DISPLAY 'ZERO DELTA        ' WS-ZERODELTA-CNT.
           DISPLAY 'NO PRIOR BASIS    ' WS-NOBASIS-CNT.
           DISPLAY 'ADJUSTMENTS       ' WS-ADJUST-CNT.
           DISPLAY 'PRIOR AMOUNT      ' WS-TOT-PRIOR-AMT.
           DISPLAY 'NEW AMOUNT        ' WS-TOT-NEW-AMT.
           DISPLAY 'NET DELTA         ' WS-TOT-DELTA-AMT.
           DISPLAY 'DEBIT DELTA       ' WS-TOT-DEBIT-AMT.
           DISPLAY 'CREDIT DELTA      ' WS-TOT-CREDIT-AMT.
           DISPLAY 'BUCKETS USED      ' WS-BK-COUNT-USED.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE USAGE-HIST-FILE
                 BILL-HIST-FILE
                 FACTOR-FILE
                 RATE-MASTER
                 ADJUST-OUT-FILE
                 AUDIT-OUT-FILE
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

