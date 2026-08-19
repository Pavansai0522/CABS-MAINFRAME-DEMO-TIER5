      *****************************************************************
      * CABJUR05 - PERCENT LOCAL USAGE APPLICATION                    *
      * APPLICATION : CABS                                            *
      * INPUTS      : PIUIN    TELCABS.CABS.CDR.PIU(0)        CABSCDR *
      * INPUTS      : FCTRVAL  TELCABS.CABS.FACTOR.VAL(0)     CABSFCTR*
      * INPUTS      : SYSIN    INSTREAM                       NONE    *
      * OUTPUTS     : PLUOUT   TELCABS.CABS.CDR.PLU(+1)       CABSCDR *
      * OUTPUTS     : SUSPOUT  TELCABS.CABS.SUSPENSE(+1)      CABSERR *
      * CONTROL     : CTLOUT                         CABSCTL          *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED              *
      *               INTRASTATE MINUTES IN = LOCAL + TOLL MINUTES OUT*
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * STANDARDS   : CODED TO CABS-STD-058 (DATE HANDLING) AND       *
      *               CABS-STD-014 (RECORD LAYOUTS). THE MODULE IS IN *
      *               THE MONTHLY REGRESSION PACK.                    *
      * REVISION HISTORY                                              *
      *   V1.00  1989-10-16  D.OKONKWO     INITIAL PLU SPLIT          *
      *   V1.03  1991-04-09  D.OKONKWO     TOLL RESIDUAL ADDED        *
      *   V2.00  1996-06-25  J.M.CASTILLO  Y2K REVIEW - NO IMPACT     *
      *   V2.02  2000-03-30  P.NAIR        ROUNDING MADE CONSISTENT   *
      *   V2.03  2002-08-21  P.NAIR        V2.02 BACKED OUT - AUDIT   *
      *   V2.06  2008-05-14  A.BUKOWSKI    ISP BOUND FLAG PASSED      *
      *   V2.08  2015-11-02  L.FERREIRA    RECOMPILE ONLY             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CABJUR05.
       AUTHOR.        D.OKONKWO.
       DATE-WRITTEN.  1989-10-16.
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
      * PIU SPLIT USAGE FROM CABJUR04
           SELECT PIU-IN-FILE
               ASSIGN TO UT-S-PIUIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
      * VALIDATED FACTORS - PLU IS TAKEN FROM HERE
           SELECT FACTOR-FILE
               ASSIGN TO UT-S-FCTRVAL
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-TABLE.
      * LOCAL AND TOLL SPLIT USAGE - FEEDS BILLING AND SETTLE
           SELECT PLU-OUT-FILE
               ASSIGN TO UT-S-PLUOUT
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
       FD  PIU-IN-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 200 CHARACTERS
               DATA RECORD IS PII-RECORD.
       01  PII-RECORD              PIC X(200).

       FD  FACTOR-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 76 CHARACTERS
               DATA RECORD IS FVL-RECORD.
       01  FVL-RECORD              PIC X(76).

       FD  PLU-OUT-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 200 CHARACTERS
               DATA RECORD IS PLO-RECORD.
       01  PLO-RECORD              PIC X(200).

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
           05  WS-PGM-NAME         PIC X(08)         VALUE 'CABJUR05'.
           05  WS-PGM-VERSION        PIC X(05)           VALUE 'V2.08'.
           05  WS-PGM-APPL           PIC X(04)           VALUE 'CABS'.
           05  WS-PGM-COMPILE      PIC X(08)         VALUE '20151102'.
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
           05  WS-PE-LOCAL-RATE        PIC 9(05)V9(05).
           05  WS-PE-TOLL-RATE         PIC 9(05)V9(05).
           05  WS-PE-FILLER            PIC X(15).
       01  WS-PARM-OLD REDEFINES WS-PARM-CARD.
      * THE 1989 CARD.  TWO REGIONS NEVER CONVERTED THEIR JCL AND
      * THE EDIT STILL ACCEPTS IT.  THE FIELDS ARE IN A DIFFERENT
      * ORDER AND THE CYCLE DATE IS SIX DIGITS YYMMDD, NOT YYDDD.
           05  FILLER                  PIC X(02).
           05  WS-PO-CYCLE             PIC 9(05).
           05  WS-PO-RUN-ID            PIC X(12).
           05  WS-PO-YYMMDD            PIC 9(06).
           05  WS-PO-LRATE             PIC 9(05)V9(05).
           05  FILLER                  PIC X(20).

      * LOCAL SWITCHES.  THE STANDARD ONES ARRIVE VIA CABSWRK.
       01  WS-LOCAL-SWITCHES.
           05  WS-PARM-EOF-SW          PIC X(01)             VALUE 'N'.
                   88  WS-PARM-EOF             VALUE 'Y'.
           05  WS-FCTR-EOF-SW          PIC X(01)             VALUE 'N'.
                   88  WS-FCTR-EOF             VALUE 'Y'.
           05  WS-PLU-SW               PIC X(01)             VALUE 'N'.
                   88  WS-PLU-APPLIES          VALUE 'Y'.

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

      * PLU WORK AREA.  PLU IS THE PERCENTAGE OF INTRASTATE
      * MINUTES THAT ARE LOCAL RATHER THAN TOLL.  IT IS APPLIED
      * ONLY TO RECORDS THAT CABJUR04 LEFT AS INTRASTATE.
       01  WS-PLU-WORK.
           05  WS-PW-BASE-MOU          PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-PW-LC-MOU            PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-PW-TL-MOU            PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-PW-PLU               PIC S9(03)V9(05) COMP-3 VALUE 0.
           05  WS-PW-LC-RATE           PIC S9(05)V9(05) COMP-3 VALUE 0.
           05  WS-PW-TL-RATE           PIC S9(05)V9(05) COMP-3 VALUE 0.
           05  WS-PW-LC-AMT            PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-PW-TL-AMT            PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-PW-EXACT-AMT         PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-PW-CHECK-MOU         PIC S9(13)V9(02) COMP-3 VALUE 0.

      * RUN TOTALS.  WS-TRUNC-LOSS ACCUMULATES WHAT TRUNCATION
      * THREW AWAY.  IT HAS NEVER BEEN POSTED ANYWHERE.
       01  WS-PLU-TOTALS.
           05  WS-TOT-LC-MOU           PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-TOT-TL-MOU           PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-TOT-LC-AMT           PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-TOT-TL-AMT           PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-APPLIED-CNT          PIC S9(11) COMP-3     VALUE 0.
           05  WS-BYPASS-CNT           PIC S9(11) COMP-3     VALUE 0.
           05  WS-TRUNC-LOSS           PIC S9(09)V9(05) COMP-3 VALUE 0.

      * LOCAL CALLING AREA TABLE.  THREE VIEWS OF THE SAME 12
      * BYTES - THE THIRD IS USED ONLY BY THE 1991 TOLL RESIDUAL
      * LOGIC.
       01  WS-LCA-WORK.
           05  WS-LCA-ORIG           PIC X(06)           VALUE SPACES.
           05  WS-LCA-TERM           PIC X(06)           VALUE SPACES.
       01  WS-LCA-NUM REDEFINES WS-LCA-WORK.
           05  WS-LCA-ORIG-N           PIC 9(06).
           05  WS-LCA-TERM-N           PIC 9(06).
       01  WS-LCA-PAIR REDEFINES WS-LCA-WORK.
           05  WS-LCA-BOTH             PIC 9(12).

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

      * MESSAGE TEXT TABLE.  ADDRESSED BY ERROR CODE SEQUENCE, NOT
      * BY THE CODE ITSELF - INSERTING A CODE IN THE MIDDLE SHIFTS
      * EVERY MESSAGE.  THIS HAS HAPPENED TWICE (1994 AND 2007).
       01  WS-MSG-CONST.
           05  FILLER              PIC X(44)
                   VALUE 'PLU APPLIED FROM FILED FACTOR               '.
           05  FILLER              PIC X(44)
                   VALUE 'PLU ZERO - ALL MINUTES TREATED AS TOLL      '.
           05  FILLER              PIC X(44)
                   VALUE 'RECORD NOT INTRASTATE - PLU BYPASSED        '.
           05  FILLER              PIC X(44)
                   VALUE 'LOCAL RATE NOT SUPPLIED ON CONTROL CARD     '.
           05  FILLER              PIC X(44)
                   VALUE 'TOLL RESIDUAL NEGATIVE - SUSPENDED          '.
           05  FILLER              PIC X(44)
                   VALUE 'MINUTES DID NOT RECONCILE AFTER SPLIT       '.
           05  FILLER              PIC X(44)
                   VALUE 'FACTOR TABLE FULL - LOAD TRUNCATED          '.
           05  FILLER              PIC X(44)
                   VALUE 'AMOUNT TRUNCATED AT FIVE DECIMAL PLACES     '.
           05  FILLER              PIC X(44)
                   VALUE 'ISP BOUND INDICATOR CARRIED TO SETTLEMENT   '.
           05  FILLER              PIC X(44)
                   VALUE 'RATE ELEMENT NOT SUBJECT TO PLU             '.
       01  WS-MSG-TABLE REDEFINES WS-MSG-CONST.
           05  WS-MSG-ENTRY OCCURS 10 TIMES
                   INDEXED BY WS-MSG-IX.
               10  WS-MSG-TEXT             PIC X(44).

      * SAVE AREA FOR THE INPUT RECORD WHILE THE TWO OUTPUT
      * RECORDS ARE BUILT FROM IT.
       01  WS-SAVE-AREAS.
           05  WS-SAVE-CDR           PIC X(200)          VALUE SPACES.
           05  WS-BEST-EFF             PIC 9(05)             VALUE 0.

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
           OPEN INPUT  PIU-IN-FILE
                       FACTOR-FILE
                       PARM-FILE
           OPEN OUTPUT PLU-OUT-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 4501 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-PIUIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 4502 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-FCTRVAL' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 4503 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-PLUOUT' TO WS-AB-TEXT
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
           MOVE ZERO TO WS-TOT-LC-MOU WS-TOT-TL-MOU
                        WS-TOT-LC-AMT WS-TOT-TL-AMT
                        WS-APPLIED-CNT WS-BYPASS-CNT
                        WS-TRUNC-LOSS.
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
           IF WS-PE-LOCAL-RATE NOT NUMERIC
               MOVE ZERO TO WS-PE-LOCAL-RATE.
           IF WS-PE-TOLL-RATE NOT NUMERIC
               MOVE ZERO TO WS-PE-TOLL-RATE.

       P1200-EXIT.
           EXIT.

       P1300-LOAD-FACTORS.
      * LOAD THE FACTOR FILE INTO CORE - PLU ONLY IS USED HERE.
           MOVE 'P1300-LOAD-FACTORS' TO WS-PARA-NAME.
           MOVE ZERO TO WS-FT-COUNT.
           PERFORM P1310-READ-FACTOR THRU P1310-EXIT
               UNTIL WS-FCTR-EOF.
           DISPLAY 'PLU FACTOR ENTRIES LOADED ' WS-FT-COUNT.

       P1300-EXIT.
           EXIT.

       P1310-READ-FACTOR.
      * ONE TABLE ENTRY.
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
           MOVE FC-SOURCE TO WS-FT-SOURCE (WS-FT-IX).

       P1310-EXIT.
           EXIT.


      *****************************************************************
      * S200-PLU-APPLICATION                                          *
      * SPLIT INTRASTATE MINUTES INTO LOCAL AND TOLL.                 *
      *****************************************************************
       S200-PLU-APPLICATION SECTION.

       P2000-PROCESS.
      * ONE USAGE RECORD PER PASS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ THRU P2100-EXIT.
           IF WS-EOF
               GO TO P2000-EXIT.
           MOVE PII-RECORD TO CABS-CDR-RECORD.
           MOVE CD-KEY TO WS-RESTART-KEY.
           IF NOT CD-INTRASTATE
               ADD 1 TO WS-BYPASS-CNT
               PERFORM P3600-PASS-THROUGH THRU P3600-EXIT
               GO TO P2000-EXIT.
           MOVE CD-VC-CHG-MIN TO WS-PW-BASE-MOU.
           PERFORM P2200-FIND-PLU THRU P2200-EXIT.
           IF NOT WS-PLU-APPLIES
               ADD 1 TO WS-BYPASS-CNT
               PERFORM P3600-PASS-THROUGH THRU P3600-EXIT
               GO TO P2000-EXIT.
           PERFORM P2400-GET-RATES THRU P2400-EXIT.
           PERFORM P3000-APPLY-PLU THRU P3000-EXIT.
           PERFORM P3200-RECONCILE THRU P3200-EXIT.
           PERFORM P3300-WRITE-SPLIT THRU P3300-EXIT.
           ADD 1 TO WS-WRITE-CNT.
           ADD 1 TO WS-APPLIED-CNT.

       P2000-EXIT.
           EXIT.

       P2100-READ.
      * SEQUENTIAL READ OF PIU SPLIT USAGE.
           READ PIU-IN-FILE
               AT END
                   MOVE 'Y' TO WS-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00'
               MOVE 3450 TO WS-AB-CODE
               MOVE 'READ ERROR UT-S-PIUIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-READ-CNT.

       P2100-EXIT.
           EXIT.

       P2200-FIND-PLU.
      * LOCATE THE PLU FOR THE CARRIER AND LATA.  A ZERO PLU IS A
      * VALID FILING - IT MEANS NONE OF THE INTRASTATE MINUTES ARE
      * LOCAL.  THE RECORD IS STILL PROCESSED SO THAT THE TOLL
      * RESIDUAL IS WRITTEN WITH THE CORRECT RATE ELEMENT.
           MOVE 'P2200-FIND-PLU' TO WS-PARA-NAME.
           MOVE 'N' TO WS-PLU-SW.
           MOVE ZERO TO WS-PW-PLU.
           MOVE ZERO TO WS-BEST-EFF.
           MOVE 1 TO WS-SUB1.
           PERFORM P2250-SCAN-FACTOR THRU P2250-EXIT
               UNTIL WS-SUB1 > WS-FT-COUNT.
           IF NOT WS-PLU-APPLIES
               MOVE EC-FACTOR-MISSING TO WS-ERR-CODE
               MOVE 'W' TO WS-ERR-SEVERITY.

       P2200-EXIT.
           EXIT.

       P2250-SCAN-FACTOR.
      * ONE FACTOR TABLE ENTRY.
           IF WS-FT-OCN (WS-SUB1) = CD-OCN AND
              WS-FT-LATA (WS-SUB1) = CD-VC-ORIG-LATA
               IF WS-FT-EFF (WS-SUB1) NOT > CD-CONN-YYDDD
                   IF WS-FT-EFF (WS-SUB1) > WS-BEST-EFF
                       MOVE WS-FT-EFF (WS-SUB1) TO WS-BEST-EFF
                       MOVE WS-FT-PLU (WS-SUB1) TO WS-PW-PLU
                       MOVE 'Y' TO WS-PLU-SW.
           ADD 1 TO WS-SUB1.

       P2250-EXIT.
           EXIT.

       P2400-GET-RATES.
      * LOCAL AND TOLL RATES ARRIVE ON THE CONTROL CARD.  THEY USED
      * TO BE READ FROM THE RATE MASTER; THE 1991 CHANGE MOVED THEM
      * TO THE CARD SO THAT A RATE CHANGE COULD BE APPLIED WITHOUT
      * A VSAM RELOAD.  THE RATE MASTER STILL HOLDS THE OLD VALUES.
           MOVE WS-PE-LOCAL-RATE TO WS-PW-LC-RATE.
           MOVE WS-PE-TOLL-RATE TO WS-PW-TL-RATE.
           IF WS-PW-LC-RATE = ZERO
               MOVE EC-RATE-NOT-FOUND TO WS-ERR-CODE
               MOVE 'W' TO WS-ERR-SEVERITY.

       P2400-EXIT.
           EXIT.


      *****************************************************************
      * S300-SPLIT-AND-WRITE                                          *
      * THE PLU ARITHMETIC.                                           *
      *****************************************************************
       S300-SPLIT-AND-WRITE SECTION.

       P3000-APPLY-PLU.
      * THE PLU SPLIT.  NOTE THAT THERE IS NO ROUNDED CLAUSE ON THE
      * MONEY COMPUTES - THE PRODUCT IS TRUNCATED AT FIVE DECIMAL
      * PLACES.  CABJUR04 ROUNDS THE EQUIVALENT COMPUTATION.  THE
      * DIFFERENCE IS SMALL PER RECORD AND MEASURABLE PER MILLION.
      * V2.02 ALIGNED THEM IN 2000 AND V2.03 BACKED IT OUT IN 2002
      * AFTER THE STATE COMMISSION QUERIED THE RESTATED TOTALS.
           MOVE 'P3000-APPLY-PLU' TO WS-PARA-NAME.
           COMPUTE WS-PW-LC-MOU ROUNDED =
                   WS-PW-BASE-MOU * WS-PW-PLU / 100.
           COMPUTE WS-PW-TL-MOU = WS-PW-BASE-MOU - WS-PW-LC-MOU.
           COMPUTE WS-PW-LC-AMT =
                   WS-PW-LC-MOU * WS-PW-LC-RATE.
           COMPUTE WS-PW-TL-AMT =
                   WS-PW-TL-MOU * WS-PW-TL-RATE.
           COMPUTE WS-PW-EXACT-AMT ROUNDED =
                   (WS-PW-LC-MOU * WS-PW-LC-RATE)
                 + (WS-PW-TL-MOU * WS-PW-TL-RATE).
           COMPUTE WS-TRUNC-LOSS = WS-TRUNC-LOSS
                 + (WS-PW-EXACT-AMT - (WS-PW-LC-AMT + WS-PW-TL-AMT)).
           ADD WS-PW-LC-MOU TO WS-TOT-LC-MOU.
           ADD WS-PW-TL-MOU TO WS-TOT-TL-MOU.
           ADD WS-PW-LC-AMT TO WS-TOT-LC-AMT.
           ADD WS-PW-TL-AMT TO WS-TOT-TL-AMT.
           ADD WS-PW-LC-AMT TO WS-ACC-AMOUNT.
           ADD WS-PW-TL-AMT TO WS-ACC-AMOUNT.
           ADD WS-PW-BASE-MOU TO WS-ACC-MINUTES.

       P3000-EXIT.
           EXIT.

       P3200-RECONCILE.
      * LOCAL PLUS TOLL MUST EQUAL THE INTRASTATE MINUTES IN.
           COMPUTE WS-PW-CHECK-MOU = WS-PW-LC-MOU + WS-PW-TL-MOU.
           IF WS-PW-CHECK-MOU NOT = WS-PW-BASE-MOU
               MOVE EC-MIN-NEGATIVE TO WS-ERR-CODE
               MOVE 'E' TO WS-ERR-SEVERITY
               PERFORM P7000-SUSPEND THRU P7000-EXIT.
           IF WS-PW-TL-MOU < ZERO
               MOVE EC-MIN-NEGATIVE TO WS-ERR-CODE
               MOVE 'E' TO WS-ERR-SEVERITY
               PERFORM P7000-SUSPEND THRU P7000-EXIT.

       P3200-EXIT.
           EXIT.

       P3300-WRITE-SPLIT.
      * LOCAL RECORD THEN TOLL RECORD.  THE LOCAL RECORD CARRIES THE
      * ISP BOUND INDICATOR THAT THE SETTLEMENT APPLICATION READS.
           MOVE CABS-CDR-RECORD TO WS-SAVE-CDR.
           MOVE 'L' TO CD-JURIS-CD.
           MOVE WS-PW-LC-MOU TO CD-VC-CHG-MIN.
           MOVE 'L' TO CD-USAGE-TYPE.
           MOVE CABS-CDR-RECORD TO PLO-RECORD.
           WRITE PLO-RECORD.
           MOVE WS-SAVE-CDR TO CABS-CDR-RECORD.
           MOVE 'S' TO CD-JURIS-CD.
           MOVE WS-PW-TL-MOU TO CD-VC-CHG-MIN.
           MOVE CABS-CDR-RECORD TO PLO-RECORD.
           WRITE PLO-RECORD.

       P3300-EXIT.
           EXIT.

       P3600-PASS-THROUGH.
      * RECORDS OUTSIDE THE INTRASTATE JURISDICTION PASS STRAIGHT THROUG
           MOVE CABS-CDR-RECORD TO PLO-RECORD.
           WRITE PLO-RECORD.
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
           MOVE 050                    TO CT-STEP-SEQ.
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
           DISPLAY 'PLU APPLIED      ' WS-APPLIED-CNT.
           DISPLAY 'LOCAL MOU        ' WS-TOT-LC-MOU.
           DISPLAY 'TOLL MOU         ' WS-TOT-TL-MOU.
           DISPLAY 'LOCAL AMOUNT     ' WS-TOT-LC-AMT.
           DISPLAY 'TOLL AMOUNT      ' WS-TOT-TL-AMT.
           DISPLAY 'TRUNCATION LOSS  ' WS-TRUNC-LOSS.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE PIU-IN-FILE
                 FACTOR-FILE
                 PLU-OUT-FILE
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

