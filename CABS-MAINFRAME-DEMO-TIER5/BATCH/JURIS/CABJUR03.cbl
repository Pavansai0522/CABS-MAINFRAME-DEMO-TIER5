      *****************************************************************
      * CABJUR03 - JURISDICTION DETERMINATION FROM LATA AND NPANXX    *
      * APPLICATION : CABS                                            *
      * INPUTS      : CDRIN    TELCABS.CABS.CDR.RATED(0)      CABSCDR *
      * INPUTS      : SYSIN    INSTREAM                       NONE    *
      * OUTPUTS     : CDROUT   TELCABS.CABS.CDR.JURIS(+1)     CABSCDR *
      * OUTPUTS     : SUSPOUT  TELCABS.CABS.SUSPENSE(+1)      CABSERR *
      * CONTROL     : CTLOUT                         CABSCTL          *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED              *
      *               EVERY USAGE RECORD LEAVES WITH A JURISDICTION CO*
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * STANDARDS   : CODED TO CABS-STD-022 (CONTROL CARDS) AND       *
      *               CABS-STD-026 (GENERATION FILES).                *
      * REVISION HISTORY                                              *
      *   V1.00  1987-06-30  R.T.WHEELER   INITIAL - LATA COMPARE     *
      *   V1.05  1990-01-08  R.T.WHEELER   NPANXX FALLBACK ADDED      *
      *   V1.09  1993-03-22  D.OKONKWO     STATE OVERRIDE MODULES     *
      *   V2.00  1996-05-17  J.M.CASTILLO  Y2K REVIEW - NO IMPACT     *
      *   V2.03  2000-10-02  P.NAIR        LOCAL CALLING AREA TEST    *
      *   V2.06  2005-02-28  P.NAIR        INDETERMINATE TO CARRIER   *
      *   V2.08  2012-09-19  A.BUKOWSKI    VOIP ORIGINATION - BACKED  *
      *   V2.09  2018-04-05  M.OYELARAN    RECOMPILE ONLY             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CABJUR03.
       AUTHOR.        R.T.WHEELER.
       DATE-WRITTEN.  1987-06-30.
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
      * RATED USAGE - GDG ZERO IS THE CURRENT CYCLE EXTRACT
           SELECT CDR-IN-FILE
               ASSIGN TO UT-S-CDRIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
      * USAGE WITH JURISDICTION APPLIED - FEEDS CABJUR04
           SELECT CDR-OUT-FILE
               ASSIGN TO UT-S-CDROUT
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

       FD  CDR-OUT-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 200 CHARACTERS
               DATA RECORD IS CDO-RECORD.
       01  CDO-RECORD              PIC X(200).

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
           05  WS-PGM-NAME         PIC X(08)         VALUE 'CABJUR03'.
           05  WS-PGM-VERSION        PIC X(05)           VALUE 'V2.09'.
           05  WS-PGM-APPL           PIC X(04)           VALUE 'CABS'.
           05  WS-PGM-COMPILE      PIC X(08)         VALUE '20180405'.
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
      * NO COPYBOOK IS RAISED FOR CONTROL CARDS - SEE CABS-STD-014.
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
           05  WS-PE-JURIS-MODE        PIC X(01).
           05  WS-PE-OVERRIDE-SW       PIC X(01).
           05  WS-PE-FILLER            PIC X(33).
       01  WS-PARM-OLD REDEFINES WS-PARM-CARD.
      * THE 1989 CARD.  TWO REGIONS NEVER CONVERTED THEIR JCL AND
      * THE EDIT STILL ACCEPTS IT.  THE FIELDS ARE IN A DIFFERENT
      * ORDER AND THE CYCLE DATE IS SIX DIGITS YYMMDD, NOT YYDDD.
           05  FILLER                  PIC X(02).
           05  WS-PO-CYCLE             PIC 9(05).
           05  WS-PO-RUN-ID            PIC X(12).
           05  WS-PO-YYMMDD            PIC 9(06).
           05  WS-PO-JMODE             PIC X(01).
           05  FILLER                  PIC X(20).

      * LOCAL SWITCHES.  THE STANDARD ONES ARRIVE VIA CABSWRK.
       01  WS-LOCAL-SWITCHES.
           05  WS-PARM-EOF-SW          PIC X(01)             VALUE 'N'.
                   88  WS-PARM-EOF             VALUE 'Y'.
           05  WS-JURIS-SET-SW         PIC X(01)             VALUE 'N'.
                   88  WS-JURIS-SET            VALUE 'Y'.
           05  WS-LATA-FOUND-SW        PIC X(01)             VALUE 'N'.
                   88  WS-LATA-FOUND           VALUE 'Y'.
           05  WS-OVERRIDE-SW          PIC X(01)             VALUE 'N'.
                   88  WS-OVERRIDE-REQD        VALUE 'Y'.

      * SUBSCRIPTS AND INDEX WORK FIELDS.
       01  WS-SUBSCRIPTS.
           05  WS-SUB1                 PIC S9(05) COMP-3     VALUE 0.
           05  WS-SUB2                 PIC S9(05) COMP-3     VALUE 0.
           05  WS-IDX                  PIC S9(05) COMP-3     VALUE 0.
           05  WS-DEPTH                PIC S9(05) COMP-3     VALUE 0.

      * JURISDICTION COUNTS BY OUTCOME.
       01  WS-JURIS-COUNTERS.
           05  WS-IS-CNT               PIC S9(11) COMP-3     VALUE 0.
           05  WS-SS-CNT               PIC S9(11) COMP-3     VALUE 0.
           05  WS-LC-CNT               PIC S9(11) COMP-3     VALUE 0.
           05  WS-IX-CNT               PIC S9(11) COMP-3     VALUE 0.
           05  WS-NPA-LOOKUP-CNT       PIC S9(11) COMP-3     VALUE 0.

      * JURISDICTION DERIVATION WORK AREA.  THE ORIGINATING AND
      * TERMINATING LATA DECIDE INTERSTATE VERSUS INTRASTATE; THE
      * NPA PAIR DECIDES LOCAL WITHIN A LATA.
       01  WS-JURIS-WORK.
           05  WS-JW-ORIG-LATA         PIC 9(03)             VALUE 0.
           05  WS-JW-TERM-LATA         PIC 9(03)             VALUE 0.
           05  WS-JW-ORIG-NPA          PIC 9(03)             VALUE 0.
           05  WS-JW-TERM-NPA          PIC 9(03)             VALUE 0.
           05  WS-JW-ORIG-STATE        PIC X(02).
           05  WS-JW-TERM-STATE        PIC X(02).
           05  WS-JW-JURIS-CD          PIC X(01).
           05  WS-JW-MINUTES           PIC S9(07)V9(02) COMP-3 VALUE 0.

      * THE NPANXX PAIR IS SOMETIMES SUPPLIED AS ONE TEN DIGIT
      * NUMBER BY THE OLDER MEDIATION FEED.  THIS REDEFINE SPLITS
      * IT.
       01  WS-NPANXX-WORK.
           05  WS-NW-ORIG              PIC 9(06)             VALUE 0.
           05  WS-NW-TERM              PIC 9(06)             VALUE 0.
       01  WS-NPANXX-SPLIT REDEFINES WS-NPANXX-WORK.
           05  WS-NS-ORIG.
               10  WS-NS-O-NPA             PIC 9(03).
               10  WS-NS-O-NXX             PIC 9(03).
           05  WS-NS-TERM.
               10  WS-NS-T-NPA             PIC 9(03).
               10  WS-NS-T-NXX             PIC 9(03).
       01  WS-NPANXX-ALT REDEFINES WS-NPANXX-WORK.
           05  WS-NA-TEN-DIGIT         PIC 9(10).
           05  WS-NA-SPARE             PIC 9(02).

      * STATE OVERRIDE MODULE NAME IS BUILT AT RUN TIME FROM THE
      * ORIGINATING STATE.  THE LOAD MODULE THAT RUNS IS DECIDED
      * BY THE STEPLIB CONCATENATION, NOT BY THIS PROGRAM.
      * MODULE NAMING FOLLOWS CABS-STD-031 (TARIFF SUFFIXES).
       01  WS-CALL-AREA.
           05  WS-CALL-PGM.
               10  WS-CP-STEM          PIC X(05)         VALUE 'CABJX'.
               10  WS-CP-SUFFIX        PIC X(03)         VALUE SPACES.
           05  WS-CALL-RC              PIC S9(04) COMP       VALUE 0.
           05  WS-CALL-COUNT           PIC S9(07) COMP-3     VALUE 0.
           05  WS-CALL-SUFFIX-TAB.
               10  FILLER              PIC X(05)         VALUE 'CACAL'.
               10  FILLER              PIC X(05)         VALUE 'NYNYK'.
               10  FILLER              PIC X(05)         VALUE 'TXTEX'.
               10  FILLER              PIC X(05)         VALUE 'FLFLA'.
               10  FILLER              PIC X(05)         VALUE 'ILILL'.
               10  FILLER              PIC X(05)         VALUE 'PAPEN'.
           05  WS-CALL-SUFFIX-R REDEFINES WS-CALL-SUFFIX-TAB.
               10  WS-CS-ENT OCCURS 6 TIMES
                   INDEXED BY WS-CS-IX.
                   15  WS-CS-KEY               PIC X(02).
                   15  WS-CS-SUF               PIC X(03).

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

      * NPA-NXX TO LATA TABLE.  THIS IS A WORKING SUBSET - THE FULL
      * LERG DERIVED TABLE IS ON TELCABS.CABS.NPANXX AND IS LOADED BY
      * CABJUR01.  THE SUBSET BELOW IS THE FALLBACK USED WHEN THE
      * LOAD FAILS AND THE OPERATOR REPLIES GO TO THE WTOR.
       01  WS-NPANXX-CONST.
           05  FILLER              PIC X(09)         VALUE '203562722'.
           05  FILLER              PIC X(09)         VALUE '205692632'.
           05  FILLER              PIC X(09)         VALUE '207405455'.
           05  FILLER              PIC X(09)         VALUE '234372370'.
           05  FILLER              PIC X(09)         VALUE '242503713'.
           05  FILLER              PIC X(09)         VALUE '242633701'.
           05  FILLER              PIC X(09)         VALUE '252954925'.
           05  FILLER              PIC X(09)         VALUE '253668463'.
           05  FILLER              PIC X(09)         VALUE '277231633'.
           05  FILLER              PIC X(09)         VALUE '285736617'.
           05  FILLER              PIC X(09)         VALUE '286859793'.
           05  FILLER              PIC X(09)         VALUE '297394770'.
           05  FILLER              PIC X(09)         VALUE '306871532'.
           05  FILLER              PIC X(09)         VALUE '309322569'.
           05  FILLER              PIC X(09)         VALUE '312691689'.
           05  FILLER              PIC X(09)         VALUE '314480401'.
           05  FILLER              PIC X(09)         VALUE '324533624'.
           05  FILLER              PIC X(09)         VALUE '329773463'.
           05  FILLER              PIC X(09)         VALUE '341414880'.
           05  FILLER              PIC X(09)         VALUE '343551961'.
           05  FILLER              PIC X(09)         VALUE '350310245'.
           05  FILLER              PIC X(09)         VALUE '351998945'.
           05  FILLER              PIC X(09)         VALUE '360210559'.
           05  FILLER              PIC X(09)         VALUE '364913633'.
           05  FILLER              PIC X(09)         VALUE '377455241'.
           05  FILLER              PIC X(09)         VALUE '378456433'.
           05  FILLER              PIC X(09)         VALUE '381546595'.
           05  FILLER              PIC X(09)         VALUE '391283791'.
           05  FILLER              PIC X(09)         VALUE '399610789'.
           05  FILLER              PIC X(09)         VALUE '405973186'.
           05  FILLER              PIC X(09)         VALUE '449450579'.
           05  FILLER              PIC X(09)         VALUE '453233919'.
           05  FILLER              PIC X(09)         VALUE '472227661'.
           05  FILLER              PIC X(09)         VALUE '476204214'.
           05  FILLER              PIC X(09)         VALUE '476481317'.
           05  FILLER              PIC X(09)         VALUE '485763935'.
           05  FILLER              PIC X(09)         VALUE '489289346'.
           05  FILLER              PIC X(09)         VALUE '518985151'.
           05  FILLER              PIC X(09)         VALUE '520610652'.
           05  FILLER              PIC X(09)         VALUE '527473745'.
           05  FILLER              PIC X(09)         VALUE '528408676'.
           05  FILLER              PIC X(09)         VALUE '531214246'.
           05  FILLER              PIC X(09)         VALUE '533355505'.
           05  FILLER              PIC X(09)         VALUE '539459411'.
           05  FILLER              PIC X(09)         VALUE '542215633'.
           05  FILLER              PIC X(09)         VALUE '562986666'.
           05  FILLER              PIC X(09)         VALUE '569690493'.
           05  FILLER              PIC X(09)         VALUE '570661196'.
           05  FILLER              PIC X(09)         VALUE '571516272'.
           05  FILLER              PIC X(09)         VALUE '579931797'.
           05  FILLER              PIC X(09)         VALUE '585912137'.
           05  FILLER              PIC X(09)         VALUE '598712331'.
           05  FILLER              PIC X(09)         VALUE '600485718'.
           05  FILLER              PIC X(09)         VALUE '630938852'.
           05  FILLER              PIC X(09)         VALUE '653243796'.
           05  FILLER              PIC X(09)         VALUE '676670664'.
           05  FILLER              PIC X(09)         VALUE '678551370'.
           05  FILLER              PIC X(09)         VALUE '681396770'.
           05  FILLER              PIC X(09)         VALUE '685466759'.
           05  FILLER              PIC X(09)         VALUE '687908157'.
           05  FILLER              PIC X(09)         VALUE '694935923'.
           05  FILLER              PIC X(09)         VALUE '698261904'.
           05  FILLER              PIC X(09)         VALUE '705433795'.
           05  FILLER              PIC X(09)         VALUE '729236827'.
           05  FILLER              PIC X(09)         VALUE '746509134'.
           05  FILLER              PIC X(09)         VALUE '748315283'.
           05  FILLER              PIC X(09)         VALUE '758252276'.
           05  FILLER              PIC X(09)         VALUE '768683699'.
           05  FILLER              PIC X(09)         VALUE '772498822'.
           05  FILLER              PIC X(09)         VALUE '775522671'.
           05  FILLER              PIC X(09)         VALUE '798344677'.
           05  FILLER              PIC X(09)         VALUE '801395851'.
           05  FILLER              PIC X(09)         VALUE '820432238'.
           05  FILLER              PIC X(09)         VALUE '827790705'.
           05  FILLER              PIC X(09)         VALUE '849393496'.
           05  FILLER              PIC X(09)         VALUE '850821899'.
           05  FILLER              PIC X(09)         VALUE '852343399'.
           05  FILLER              PIC X(09)         VALUE '872775727'.
           05  FILLER              PIC X(09)         VALUE '875575283'.
           05  FILLER              PIC X(09)         VALUE '884471248'.
           05  FILLER              PIC X(09)         VALUE '887837934'.
           05  FILLER              PIC X(09)         VALUE '890842193'.
           05  FILLER              PIC X(09)         VALUE '896743300'.
           05  FILLER              PIC X(09)         VALUE '903398530'.
           05  FILLER              PIC X(09)         VALUE '912453896'.
           05  FILLER              PIC X(09)         VALUE '928740122'.
           05  FILLER              PIC X(09)         VALUE '947252962'.
           05  FILLER              PIC X(09)         VALUE '951893798'.
           05  FILLER              PIC X(09)         VALUE '969259145'.
           05  FILLER              PIC X(09)         VALUE '981836849'.
       01  WS-NPANXX-TABLE REDEFINES WS-NPANXX-CONST.
           05  WS-WS-NX-ENTRY OCCURS 90 TIMES
                   INDEXED BY WS-NX-IX.
               10  WS-NX-NPA               PIC 9(03).
               10  WS-NX-NXX               PIC 9(03).
               10  WS-NX-LATA              PIC 9(03).

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
                   VALUE 'JURISDICTION INTERSTATE                     '.
           05  FILLER              PIC X(44)
                   VALUE 'JURISDICTION INTRASTATE                     '.
           05  FILLER              PIC X(44)
                   VALUE 'JURISDICTION LOCAL                          '.
           05  FILLER              PIC X(44)
                   VALUE 'JURISDICTION INDETERMINATE                  '.
           05  FILLER              PIC X(44)
                   VALUE 'ORIGINATING LATA NOT ON TABLE               '.
           05  FILLER              PIC X(44)
                   VALUE 'TERMINATING LATA NOT ON TABLE               '.
           05  FILLER              PIC X(44)
                   VALUE 'NPANXX NOT ON TABLE - LATA ASSUMED          '.
           05  FILLER              PIC X(44)
                   VALUE 'STATE OVERRIDE MODULE CALLED                '.
           05  FILLER              PIC X(44)
                   VALUE 'STATE OVERRIDE MODULE NOT AVAILABLE         '.
           05  FILLER              PIC X(44)
                   VALUE 'USAGE TYPE NOT VOICE - RATE ELEMENT USED    '.
       01  WS-MSG-TABLE REDEFINES WS-MSG-CONST.
           05  WS-MSG-ENTRY OCCURS 10 TIMES
                   INDEXED BY WS-MSG-IX.
               10  WS-MSG-TEXT             PIC X(44).

      * SEARCH ARGUMENT AND RESULT FIELDS FOR THE TWO TABLE
      * LOOKUPS.  BOTH TABLES ARE SEARCHED MANY TIMES PER RECORD.
       01  WS-LOOKUP-AREA.
           05  WS-LT-SEARCH            PIC 9(03)             VALUE 0.
           05  WS-LT-RESULT          PIC X(02)           VALUE SPACES.
           05  WS-NX-SEARCH.
               10  WS-NXS-NPA              PIC 9(03).
               10  WS-NXS-NXX              PIC 9(03).
           05  WS-NX-RESULT            PIC 9(03)             VALUE 0.

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
                       PARM-FILE
           OPEN OUTPUT CDR-OUT-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 4301 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-CDRIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 4302 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-CDROUT' TO WS-AB-TEXT
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
           MOVE ZERO TO WS-IS-CNT WS-SS-CNT WS-LC-CNT
                        WS-IX-CNT WS-NPA-LOOKUP-CNT.
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
           IF WS-PE-JURIS-MODE NOT = 'L' AND
              WS-PE-JURIS-MODE NOT = 'N'
               MOVE 'L' TO WS-PE-JURIS-MODE.

       P1200-EXIT.
           EXIT.


      *****************************************************************
      * S200-JURISDICTION                                             *
      * DERIVE THE JURISDICTION FOR EACH USAGE RECORD.                *
      *****************************************************************
       S200-JURISDICTION SECTION.

       P2000-PROCESS.
      * ONE USAGE RECORD PER PASS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ THRU P2100-EXIT.
           IF WS-EOF
               GO TO P2000-EXIT.
           MOVE CDI-RECORD TO CABS-CDR-RECORD.
           MOVE CD-KEY TO WS-RESTART-KEY.
           MOVE 'N' TO WS-JURIS-SET-SW.
           MOVE ' ' TO WS-JW-JURIS-CD.
           IF NOT CD-VALID-TYPE
               MOVE EC-JURIS-INDET TO WS-ERR-CODE
               MOVE 'E' TO WS-ERR-SEVERITY
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               GO TO P2000-EXIT.
           IF CD-VOICE-MOU
               PERFORM P2200-VOICE-JURIS THRU P2200-EXIT
           ELSE
               PERFORM P2300-NONVOICE-JURIS THRU P2300-EXIT.
           IF NOT WS-JURIS-SET
               PERFORM P2400-INDETERMINATE THRU P2400-EXIT.
           PERFORM P3000-STATE-OVERRIDE THRU P3000-EXIT.
           PERFORM P3500-WRITE-OUT THRU P3500-EXIT.

       P2000-EXIT.
           EXIT.

       P2100-READ.
      * SEQUENTIAL READ OF RATED USAGE.
           READ CDR-IN-FILE
               AT END
                   MOVE 'Y' TO WS-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00'
               MOVE 3430 TO WS-AB-CODE
               MOVE 'READ ERROR UT-S-CDRIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-READ-CNT.

       P2100-EXIT.
           EXIT.

       P2200-VOICE-JURIS.
      * VOICE MINUTES OF USE.  THE ORIGINATING AND TERMINATING LATA
      * CARRIED ON THE RECORD ARE USED WHEN PRESENT.  WHEN THEY ARE
      * ZERO THE NPANXX PAIR IS LOOKED UP INSTEAD.  DIFFERENT STATE
      * MEANS INTERSTATE.  SAME LATA AND SAME NPA MEANS LOCAL.
           MOVE 'P2200-VOICE-JURIS' TO WS-PARA-NAME.
           MOVE CD-VC-ORIG-LATA TO WS-JW-ORIG-LATA.
           MOVE CD-VC-TERM-LATA TO WS-JW-TERM-LATA.
           MOVE CD-VC-ORIG-NPANXX TO WS-NW-ORIG.
           MOVE CD-VC-TERM-NPANXX TO WS-NW-TERM.
           MOVE CD-VC-CHG-MIN TO WS-JW-MINUTES.
           IF WS-JW-ORIG-LATA = ZERO
               MOVE WS-NW-ORIG TO WS-NX-SEARCH
               PERFORM P2250-NPA-LOOKUP THRU P2250-EXIT
               MOVE WS-NX-RESULT TO WS-JW-ORIG-LATA.
           IF WS-JW-TERM-LATA = ZERO
               MOVE WS-NW-TERM TO WS-NX-SEARCH
               PERFORM P2250-NPA-LOOKUP THRU P2250-EXIT
               MOVE WS-NX-RESULT TO WS-JW-TERM-LATA.
           MOVE WS-JW-ORIG-LATA TO WS-LT-SEARCH.
           PERFORM P2260-LATA-LOOKUP THRU P2260-EXIT.
           MOVE WS-LT-RESULT TO WS-JW-ORIG-STATE.
           MOVE WS-JW-TERM-LATA TO WS-LT-SEARCH.
           PERFORM P2260-LATA-LOOKUP THRU P2260-EXIT.
           MOVE WS-LT-RESULT TO WS-JW-TERM-STATE.
           IF WS-JW-ORIG-STATE = SPACES OR WS-JW-TERM-STATE = SPACES
               GO TO P2200-EXIT.
           IF WS-JW-ORIG-STATE NOT = WS-JW-TERM-STATE
               PERFORM P3300-SET-INTERSTATE THRU P3400-EXIT
               GO TO P2200-EXIT.
           IF WS-JW-ORIG-LATA NOT = WS-JW-TERM-LATA
               PERFORM P3300-SET-INTERSTATE THRU P3400-EXIT
               GO TO P2200-EXIT.
           MOVE WS-NS-O-NPA TO WS-JW-ORIG-NPA.
           MOVE WS-NS-T-NPA TO WS-JW-TERM-NPA.
           IF WS-JW-ORIG-NPA = WS-JW-TERM-NPA
               MOVE 'L' TO WS-JW-JURIS-CD
               ADD 1 TO WS-LC-CNT
               MOVE 'Y' TO WS-JURIS-SET-SW
           ELSE
               MOVE 'S' TO WS-JW-JURIS-CD
               ADD 1 TO WS-SS-CNT
               MOVE 'Y' TO WS-JURIS-SET-SW.

       P2200-EXIT.
           EXIT.

       P2250-NPA-LOOKUP.
      * BINARY SEARCH OF THE NPANXX SUBSET TABLE.
           MOVE ZERO TO WS-NX-RESULT.
           ADD 1 TO WS-NPA-LOOKUP-CNT.
           SET WS-NX-IX TO 1.
           SEARCH WS-NX-ENTRY
               AT END
                   MOVE ZERO TO WS-NX-RESULT
               WHEN WS-NX-NPA (WS-NX-IX) = WS-NXS-NPA AND
                    WS-NX-NXX (WS-NX-IX) = WS-NXS-NXX
                   MOVE WS-NX-LATA (WS-NX-IX) TO WS-NX-RESULT.

       P2250-EXIT.
           EXIT.

       P2260-LATA-LOOKUP.
      * THE LATA TABLE IS IN ASCENDING SEQUENCE SO SEARCH ALL IS
      * USED.  A LATA THAT IS NOT ON THE TABLE RETURNS SPACES AND
      * THE RECORD FALLS THROUGH TO THE INDETERMINATE PATH.
           MOVE SPACES TO WS-LT-RESULT.
           SEARCH ALL WS-LT-ENTRY
               AT END
                   MOVE SPACES TO WS-LT-RESULT
               WHEN WS-LT-LATA (WS-LT-IX) = WS-LT-SEARCH
                   MOVE WS-LT-STATE (WS-LT-IX) TO WS-LT-RESULT.

       P2260-EXIT.
           EXIT.

       P2300-NONVOICE-JURIS.
      * DATA AND SPECIAL ACCESS DO NOT CARRY A LATA PAIR.  THE RATE
      * ELEMENT DECIDES THE JURISDICTION FOR THOSE RECORDS.
           MOVE 'P2300-NONVOICE-JURIS' TO WS-PARA-NAME.
           SET WS-RE-IX TO 1.
           SEARCH WS-RE-ENTRY
               AT END
                   GO TO P2300-EXIT
               WHEN WS-RE-ELEM (WS-RE-IX) = CD-RATE-ELEM
                   MOVE WS-RE-JURIS (WS-RE-IX) TO WS-JW-JURIS-CD
                   MOVE 'Y' TO WS-JURIS-SET-SW.
           IF WS-JW-JURIS-CD = 'I'
               ADD 1 TO WS-IS-CNT.
           IF WS-JW-JURIS-CD = 'S'
               ADD 1 TO WS-SS-CNT.
           IF WS-JW-JURIS-CD = 'L'
               ADD 1 TO WS-LC-CNT.

       P2300-EXIT.
           EXIT.

       P2400-INDETERMINATE.
      * NOTHING DECIDED THE JURISDICTION.  THE RECORD IS MARKED X
      * AND CABJUR06 WILL APPLY THE CARRIER DEFAULT FACTOR TO IT.
           MOVE 'X' TO WS-JW-JURIS-CD.
           ADD 1 TO WS-IX-CNT.
           MOVE EC-JURIS-INDET TO WS-ERR-CODE.
           MOVE 'W' TO WS-ERR-SEVERITY.
           PERFORM P7000-SUSPEND THRU P7000-EXIT.
           SUBTRACT 1 FROM WS-REJECT-CNT.

       P2400-EXIT.
           EXIT.


      *****************************************************************
      * S300-OVERRIDE-AND-WRITE                                       *
      * STATE OVERRIDE MODULE AND OUTPUT.                             *
      *****************************************************************
       S300-OVERRIDE-AND-WRITE SECTION.

       P3000-STATE-OVERRIDE.
      * SIX STATES HAVE COMMISSION ORDERED JURISDICTION RULES THAT
      * DIFFER FROM THE FEDERAL TREATMENT.  EACH HAS ITS OWN LOAD
      * MODULE.  THE NAME IS ASSEMBLED HERE AND CALLED DYNAMICALLY,
      * SO STATIC ANALYSIS CANNOT SEE WHICH MODULE RUNS.
      * CALL TARGETS ARE LISTED IN THE APPLICATION BUILD NOTE.
           MOVE 'P3000-STATE-OVERRIDE' TO WS-PARA-NAME.
           IF WS-PE-OVERRIDE-SW NOT = 'Y'
               GO TO P3000-EXIT.
           MOVE SPACES TO WS-CP-SUFFIX.
           SET WS-CS-IX TO 1.
           SEARCH WS-CS-ENT
               AT END
                   GO TO P3000-EXIT
               WHEN WS-CS-KEY (WS-CS-IX) = WS-JW-ORIG-STATE
                   MOVE WS-CS-SUF (WS-CS-IX) TO WS-CP-SUFFIX.
           IF WS-CP-SUFFIX = SPACES
               GO TO P3000-EXIT.
           ADD 1 TO WS-CALL-COUNT.
           CALL WS-CALL-PGM USING CABS-CDR-RECORD
                                  WS-JURIS-WORK
                                  WS-CALL-RC.
           IF WS-CALL-RC = ZERO
               MOVE WS-JW-JURIS-CD TO CD-JURIS-CD.

       P3000-EXIT.
           EXIT.

       P3300-SET-INTERSTATE.
      * SETS THE INTERSTATE CODE.  THIS PARAGRAPH HAS NO EXIT OF ITS
      * OWN - CONTROL DROPS INTO P3400-ACCUM-JURIS AND THE CALLERS
      * ALL PERFORM P3300 THRU P3400-EXIT.
           MOVE 'I' TO WS-JW-JURIS-CD.
           ADD 1 TO WS-IS-CNT.
           MOVE 'Y' TO WS-JURIS-SET-SW.

       P3400-ACCUM-JURIS.
      * ACCUMULATE THE HASH TOTALS.  ENTERED BOTH BY FALL THROUGH
      * FROM P3300 AND BY DIRECT PERFORM FROM P3500.
           ADD WS-JW-MINUTES TO WS-ACC-MINUTES.
           ADD CD-SEQ-NBR TO WS-ACC-SEQ-HASH.

       P3400-EXIT.
           EXIT.

       P3500-WRITE-OUT.
      * WRITE THE USAGE RECORD WITH ITS JURISDICTION CODE.
           MOVE WS-JW-JURIS-CD TO CD-JURIS-CD.
           MOVE CABS-CDR-RECORD TO CDO-RECORD.
           WRITE CDO-RECORD.
           ADD 1 TO WS-WRITE-CNT.

       P3500-EXIT.
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
           MOVE 030                    TO CT-STEP-SEQ.
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
           DISPLAY 'INTERSTATE       ' WS-IS-CNT.
           DISPLAY 'INTRASTATE       ' WS-SS-CNT.
           DISPLAY 'LOCAL            ' WS-LC-CNT.
           DISPLAY 'INDETERMINATE    ' WS-IX-CNT.
           DISPLAY 'STATE OVERRIDES  ' WS-CALL-COUNT.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE CDR-IN-FILE
                 CDR-OUT-FILE
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

