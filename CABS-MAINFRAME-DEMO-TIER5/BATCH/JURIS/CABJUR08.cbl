      *****************************************************************
      * CABJUR08 - RESTATEMENT REVERSAL                               *
      * APPLICATION : CABS                                            *
      * INPUTS      : ADJIN    TELCABS.CABS.RESTATE.ADJ(-1)   CABSBILL*
      * INPUTS      : AUDTIN   TELCABS.CABS.RESTATE.AUDIT(-1) NONE    *
      * INPUTS      : SYSIN    INSTREAM                       NONE    *
      * OUTPUTS     : REVOUT   TELCABS.CABS.RESTATE.REV(+1)   CABSBILL*
      * OUTPUTS     : REPORT   SYSOUT                         CABSPRNT*
      * CONTROL     : CTLOUT                         CABSCTL          *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARIS*
      *               EVERY REVERSED ADJUSTMENT MUST NET TO ZERO AGAIN*
      *               THE ADJUSTMENT IT REVERSES                      *
      * RESTART     : FULL RERUN - REVERSALS ARE KEYED AND IDEMPOTENT *
      * STANDARDS   : CODED TO CABS-STD-022 (CONTROL CARDS) AND       *
      *               CABS-STD-026 (GENERATION FILES). LAST STANDARDS *
      *               REVIEW 2016. ONE WAIVER ON FILE (W-0148, PRINT  *
      *               AREA ONLY).                                     *
      * REVISION HISTORY                                              *
      *   V1.00  1989-03-27  D.OKONKWO     INITIAL                    *
      *   V1.04  1992-11-16  R.T.WHEELER   PARTIAL REVERSAL ADDED     *
      *   V2.00  1996-09-02  J.M.CASTILLO  Y2K REVIEW - NO IMPACT     *
      *   V2.02  2001-06-12  P.NAIR        AUDIT FILE MATCH ADDED     *
      *   V2.04  2005-10-31  P.NAIR        NET TO ZERO CHECK          *
      *   V2.06  2012-03-08  A.BUKOWSKI    REASON CODE CARRIED        *
      *   V2.08  2018-12-04  M.OYELARAN    RECOMPILE ONLY             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CABJUR08.
       AUTHOR.        D.OKONKWO.
       DATE-WRITTEN.  1989-03-27.
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
      * THE ADJUSTMENTS TO BE REVERSED - GDG MINUS ONE
           SELECT ADJUST-IN-FILE
               ASSIGN TO UT-S-ADJIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
      * THE AUDIT TRAIL OF THE RUN BEING REVERSED
           SELECT AUDIT-IN-FILE
               ASSIGN TO UT-S-AUDTIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-TABLE.
      * REVERSAL ADJUSTMENTS - POSTED BY CABJUR10
           SELECT REVERSE-OUT-FILE
               ASSIGN TO UT-S-REVOUT
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
       FD  ADJUST-IN-FILE
               RECORDING MODE IS V
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 1647 CHARACTERS
               DATA RECORD IS ADI-RECORD.
       01  ADI-RECORD              PIC X(1647).

       FD  AUDIT-IN-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 250 CHARACTERS
               DATA RECORD IS AUI-RECORD.
       01  AUI-RECORD              PIC X(250).

       FD  REVERSE-OUT-FILE
               RECORDING MODE IS V
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 1647 CHARACTERS
               DATA RECORD IS RVO-RECORD.
       01  RVO-RECORD              PIC X(1647).

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
           05  WS-PGM-NAME         PIC X(08)         VALUE 'CABJUR08'.
           05  WS-PGM-VERSION        PIC X(05)           VALUE 'V2.08'.
           05  WS-PGM-APPL           PIC X(04)           VALUE 'CABS'.
           05  WS-PGM-COMPILE      PIC X(08)         VALUE '20181204'.
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

       COPY CABSBILL.

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
           05  WS-PE-REV-RUN-ID        PIC X(12).
           05  WS-PE-REV-PCT           PIC 9(03)V9(02).
           05  WS-PE-REV-REASON        PIC X(02).
           05  WS-PE-FILLER            PIC X(16).
       01  WS-PARM-OLD REDEFINES WS-PARM-CARD.
      * THE 1989 CARD.  TWO REGIONS NEVER CONVERTED THEIR JCL AND
      * THE EDIT STILL ACCEPTS IT.  THE FIELDS ARE IN A DIFFERENT
      * ORDER AND THE CYCLE DATE IS SIX DIGITS YYMMDD, NOT YYDDD.
           05  FILLER                  PIC X(02).
           05  WS-PO-CYCLE             PIC 9(05).
           05  WS-PO-RUN-ID            PIC X(12).
           05  WS-PO-YYMMDD            PIC 9(06).
           05  WS-PO-REVRUN            PIC X(12).
           05  FILLER                  PIC X(20).

      * LOCAL SWITCHES.  THE STANDARD ONES ARRIVE VIA CABSWRK.
       01  WS-LOCAL-SWITCHES.
           05  WS-PARM-EOF-SW          PIC X(01)             VALUE 'N'.
                   88  WS-PARM-EOF             VALUE 'Y'.
           05  WS-AUDIT-EOF-SW         PIC X(01)             VALUE 'N'.
                   88  WS-AUDIT-EOF            VALUE 'Y'.
           05  WS-PARTIAL-SW           PIC X(01)             VALUE 'N'.
                   88  WS-PARTIAL              VALUE 'Y'.
           05  WS-MATCH-SW             PIC X(01)             VALUE 'N'.
                   88  WS-MATCHED              VALUE 'Y'.

      * SUBSCRIPTS AND INDEX WORK FIELDS.
       01  WS-SUBSCRIPTS.
           05  WS-SUB1                 PIC S9(05) COMP-3     VALUE 0.
           05  WS-SUB2                 PIC S9(05) COMP-3     VALUE 0.
           05  WS-ELEM-SUB             PIC S9(05) COMP-3     VALUE 0.

      * REVERSAL WORK AREA.  A FULL REVERSAL IS THE ORIGINAL
      * AMOUNT WITH THE SIGN FLIPPED.  A PARTIAL REVERSAL IS A
      * PERCENTAGE OF IT - USED WHEN A DISPUTE IS SETTLED PART
      * WAY.  THE PARTIAL PERCENTAGE ARRIVES ON THE CONTROL CARD.
       01  WS-REVERSE-WORK.
           05  WS-RV-ORIG-AMT          PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-RV-REV-AMT           PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-RV-RESIDUAL          PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-RV-PCT               PIC S9(03)V9(02) COMP-3 VALUE 0.
           05  WS-RV-MOU               PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-RV-NET-CHECK         PIC S9(13)V9(05) COMP-3 VALUE 0.

      * REVERSAL RUN TOTALS.
       01  WS-REVERSE-TOTALS.
           05  WS-REV-CNT              PIC S9(11) COMP-3     VALUE 0.
           05  WS-PART-CNT             PIC S9(11) COMP-3     VALUE 0.
           05  WS-NONZERO-CNT          PIC S9(11) COMP-3     VALUE 0.
           05  WS-TOT-REV-AMT          PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-TOT-RESID-AMT        PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-TOT-ORIG-AMT         PIC S9(15)V9(05) COMP-3 VALUE 0.

      * THE AUDIT RECORD AS IT WAS WRITTEN BY CABJUR07.  THE
      * LAYOUT IS NOT IN A COPYBOOK - IT IS DECLARED IN BOTH
      * PROGRAMS AND THE TWO DECLARATIONS ARE KEPT IN STEP BY
      * HAND.  THEY DIVERGED IN 2012 AND WERE RESYNCHRONISED.
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
       01  WS-AUDIT-ALT REDEFINES WS-AUDIT-RECORD.
           05  WS-AA-KEY               PIC X(29).
           05  WS-AA-BODY              PIC X(221).

      * JULIAN DATE WORK AREA - LOCAL TO CABJUR08.  THE SHARED AREA IN
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
                   VALUE 'RESTATEMENT REVERSAL REGISTER'.
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
           05  FILLER              PIC X(41)
                   VALUE 'OCN  BAN           SECT ORIG-AMOUNT      '.
           05  FILLER              PIC X(39)
                   VALUE 'REVERSAL         RESIDUAL   DISPOSITION'.
       01  WS-HEAD-4.
           05  FILLER                  PIC X(01)             VALUE ' '.
           05  FILLER                PIC X(131)          VALUE ALL '-'.

      * DETAIL LINE WS-DETAIL-1.
       01  WS-DETAIL-1.
           05  FILLER                  PIC X(01)             VALUE ' '.
           05  WS-D1-OCN               PIC X(04).
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-BAN               PIC X(13).
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-SECT              PIC X(02).
           05  FILLER                PIC X(02)           VALUE SPACES.
           05  WS-D1-ORIG              PIC ZZZ,ZZZ,ZZ9.99999-.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-REV               PIC ZZZ,ZZZ,ZZ9.99999-.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-RESID             PIC ZZ9.99999-.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-DISPO             PIC X(24).

      * MESSAGE TEXT TABLE.  ADDRESSED BY ERROR CODE SEQUENCE, NOT
      * BY THE CODE ITSELF - INSERTING A CODE IN THE MIDDLE SHIFTS
      * EVERY MESSAGE.  THIS HAS HAPPENED TWICE (1994 AND 2007).
       01  WS-MSG-CONST.
           05  FILLER              PIC X(44)
                   VALUE 'FULL REVERSAL APPLIED                       '.
           05  FILLER              PIC X(44)
                   VALUE 'PARTIAL REVERSAL APPLIED                    '.
           05  FILLER              PIC X(44)
                   VALUE 'AUDIT RECORD NOT MATCHED                    '.
           05  FILLER              PIC X(44)
                   VALUE 'REVERSAL DOES NOT NET TO ZERO               '.
           05  FILLER              PIC X(44)
                   VALUE 'ADJUSTMENT ALREADY REVERSED                 '.
           05  FILLER              PIC X(44)
                   VALUE 'RUN ID DOES NOT MATCH CONTROL CARD          '.
           05  FILLER              PIC X(44)
                   VALUE 'REVERSAL PERCENTAGE OUT OF RANGE            '.
           05  FILLER              PIC X(44)
                   VALUE 'RESIDUAL WRITTEN OFF                        '.
           05  FILLER              PIC X(44)
                   VALUE 'REVERSAL DATED OUTSIDE THE PERIOD           '.
           05  FILLER              PIC X(44)
                   VALUE 'END OF REVERSAL RUN                         '.
       01  WS-MSG-TABLE REDEFINES WS-MSG-CONST.
           05  WS-MSG-ENTRY OCCURS 10 TIMES
                   INDEXED BY WS-MSG-IX.
               10  WS-MSG-TEXT             PIC X(44).

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
           OPEN INPUT  ADJUST-IN-FILE
                       AUDIT-IN-FILE
                       PARM-FILE
           OPEN OUTPUT REVERSE-OUT-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 4801 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-ADJIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 4802 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-AUDTIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 4803 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-REVOUT' TO WS-AB-TEXT
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
           MOVE ZERO TO WS-REV-CNT WS-PART-CNT
                        WS-NONZERO-CNT WS-TOT-REV-AMT
                        WS-TOT-RESID-AMT WS-TOT-ORIG-AMT.
           PERFORM P2600-READ-AUDIT THRU P2600-EXIT.
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
           IF WS-PE-REV-PCT NOT NUMERIC
               MOVE 100.00 TO WS-PE-REV-PCT.
           IF WS-PE-REV-PCT = ZERO
               MOVE 100.00 TO WS-PE-REV-PCT.
           IF WS-PE-REV-PCT > 100.00
               MOVE 3906 TO WS-AB-CODE
               MOVE 'REVERSAL PERCENT ABOVE 100' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PE-REV-RUN-ID = SPACES
               MOVE 3907 TO WS-AB-CODE
               MOVE 'NO RUN ID TO REVERSE' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.

       P1200-EXIT.
           EXIT.


      *****************************************************************
      * S200-REVERSAL                                                 *
      * REVERSE EACH ADJUSTMENT.                                      *
      *****************************************************************
       S200-REVERSAL SECTION.

       P2000-PROCESS.
      * ONE ADJUSTMENT RECORD PER PASS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ THRU P2100-EXIT.
           IF WS-EOF
               GO TO P2000-EXIT.
           MOVE ADI-RECORD TO CABS-BILL-DETAIL.
           MOVE BD-KEY TO WS-RESTART-KEY.
           MOVE SPACES TO WS-D1-DISPO.
           IF BD-SECTION NOT = 'RS'
               ADD 1 TO WS-SUMM-CNT
               GO TO P2000-EXIT.
           PERFORM P2500-MATCH-AUDIT THRU P2500-EXIT.
           PERFORM P3000-COMPUTE-REVERSAL THRU P3000-EXIT.
           PERFORM P3200-NET-CHECK THRU P3200-EXIT.
           PERFORM P3300-WRITE-REVERSAL THRU P3300-EXIT.
           PERFORM P6100-DETAIL THRU P6100-EXIT.
           ADD 1 TO WS-WRITE-CNT.

       P2000-EXIT.
           EXIT.

       P2100-READ.
      * SEQUENTIAL READ OF THE ADJUSTMENT FILE.
           READ ADJUST-IN-FILE
               AT END
                   MOVE 'Y' TO WS-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00'
               MOVE 3480 TO WS-AB-CODE
               MOVE 'READ ERROR UT-S-ADJIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-READ-CNT.

       P2100-EXIT.
           EXIT.

       P2500-MATCH-AUDIT.
      * MATCH THE ADJUSTMENT TO ITS AUDIT RECORDS.  THE MATCH IS ON
      * RUN ID AND BAN.  AN UNMATCHED ADJUSTMENT IS STILL REVERSED
      * BUT THE REGISTER SAYS SO - IT MEANS THE AUDIT FILE AND THE
      * ADJUSTMENT FILE CAME FROM DIFFERENT RUNS, WHICH IS ONLY
      * POSSIBLE IF THE OPERATOR SUPPLIED MISMATCHED GENERATIONS.
           MOVE 'P2500-MATCH-AUDIT' TO WS-PARA-NAME.
           MOVE 'N' TO WS-MATCH-SW.
           IF WS-AUDIT-EOF
               MOVE WS-MSG-TEXT (3) TO WS-D1-DISPO
               GO TO P2500-EXIT.
           PERFORM P2600-READ-AUDIT THRU P2600-EXIT
               UNTIL WS-AUDIT-EOF
                  OR WS-AU-BAN NOT < BD-BAN.
           IF WS-AUDIT-EOF
               MOVE WS-MSG-TEXT (3) TO WS-D1-DISPO
               GO TO P2500-EXIT.
           IF WS-AU-BAN = BD-BAN
               MOVE 'Y' TO WS-MATCH-SW
               IF WS-AU-RUN-ID NOT = WS-PE-REV-RUN-ID
                   MOVE WS-MSG-TEXT (6) TO WS-D1-DISPO.

       P2500-EXIT.
           EXIT.

       P2600-READ-AUDIT.
      * READ THE AUDIT TRAIL FILE.
           READ AUDIT-IN-FILE INTO WS-AUDIT-RECORD
               AT END
                   MOVE 'Y' TO WS-AUDIT-EOF-SW
                   GO TO P2600-EXIT.

       P2600-EXIT.
           EXIT.


      *****************************************************************
      * S300-COMPUTE                                                  *
      * THE REVERSAL ARITHMETIC.                                      *
      *****************************************************************
       S300-COMPUTE SECTION.

       P3000-COMPUTE-REVERSAL.
      * THE REVERSAL IS THE ORIGINAL AMOUNT WITH THE SIGN FLIPPED,
      * SCALED BY THE REVERSAL PERCENTAGE.  NOTE THERE IS NO ROUNDED
      * CLAUSE - THE PRODUCT IS TRUNCATED.  CABJUR07 ROUNDED THE
      * AMOUNT BEING REVERSED.  A FULL REVERSAL OF A ROUNDED FIGURE
      * BY A TRUNCATING MULTIPLY DOES NOT ALWAYS NET TO ZERO, AND
      * THE RESIDUE IS WHAT P3200 COUNTS.
           MOVE 'P3000-COMPUTE-REVERSAL' TO WS-PARA-NAME.
           MOVE BD-TOT-AMOUNT TO WS-RV-ORIG-AMT.
           MOVE BD-TOT-MINUTES TO WS-RV-MOU.
           MOVE WS-PE-REV-PCT TO WS-RV-PCT.
           COMPUTE WS-RV-REV-AMT =
                   (WS-RV-ORIG-AMT * WS-RV-PCT / 100) * -1.
           IF WS-RV-PCT < 100.00
               MOVE 'Y' TO WS-PARTIAL-SW
               ADD 1 TO WS-PART-CNT
               MOVE WS-MSG-TEXT (2) TO WS-D1-DISPO
           ELSE
               MOVE 'N' TO WS-PARTIAL-SW
               MOVE WS-MSG-TEXT (1) TO WS-D1-DISPO.
           ADD WS-RV-ORIG-AMT TO WS-TOT-ORIG-AMT.
           ADD WS-RV-REV-AMT TO WS-TOT-REV-AMT.
           ADD WS-RV-REV-AMT TO WS-ACC-AMOUNT.
           ADD WS-RV-MOU TO WS-ACC-MINUTES.

       P3000-EXIT.
           EXIT.

       P3200-NET-CHECK.
      * THE REVERSAL AND THE ORIGINAL SHOULD NET TO ZERO ON A FULL
      * REVERSAL.  WHERE THEY DO NOT, THE RESIDUE IS COUNTED AND
      * CARRIED TO THE WRITE OFF TOTAL.  IT IS NEVER POSTED.
           COMPUTE WS-RV-NET-CHECK =
                   WS-RV-ORIG-AMT + WS-RV-REV-AMT.
           IF WS-PARTIAL
               COMPUTE WS-RV-RESIDUAL =
                       WS-RV-ORIG-AMT - (WS-RV-REV-AMT * -1)
               ADD WS-RV-RESIDUAL TO WS-TOT-RESID-AMT
               GO TO P3200-EXIT.
           IF WS-RV-NET-CHECK NOT = ZERO
               ADD 1 TO WS-NONZERO-CNT
               ADD WS-RV-NET-CHECK TO WS-TOT-RESID-AMT
               MOVE WS-MSG-TEXT (4) TO WS-D1-DISPO.

       P3200-EXIT.
           EXIT.

       P3300-WRITE-REVERSAL.
      * BUILD AND WRITE THE REVERSAL RECORD.  IT IS THE ORIGINAL
      * ADJUSTMENT WITH THE AMOUNTS FLIPPED AND THE SOURCE PROCESS
      * CHANGED SO THAT IT CANNOT ITSELF BE REVERSED BY A SECOND
      * RUN OF THIS PROGRAM.
           MOVE WS-RV-REV-AMT TO BD-TOT-AMOUNT.
           COMPUTE BD-TOT-ROUNDED = WS-RV-REV-AMT.
           COMPUTE BD-ROUND-DELTA =
                   BD-TOT-AMOUNT - BD-TOT-ROUNDED.
           IF BD-ELEM-CNT > ZERO
               MOVE 1 TO WS-ELEM-SUB
               PERFORM P3350-FLIP-ELEMENT THRU P3350-EXIT
                   UNTIL WS-ELEM-SUB > BD-ELEM-CNT.
           MOVE CABS-BILL-DETAIL TO RVO-RECORD.
           WRITE RVO-RECORD.
           ADD 1 TO WS-REV-CNT.

       P3300-EXIT.
           EXIT.

       P3350-FLIP-ELEMENT.
      * FLIP THE SIGN ON ONE RATE ELEMENT.
           COMPUTE BD-EL-AMOUNT (WS-ELEM-SUB) =
                   BD-EL-AMOUNT (WS-ELEM-SUB) * -1.
           MOVE 'CABJUR08' TO BD-EL-SRC-PROCESS (WS-ELEM-SUB).
           ADD 1 TO WS-ELEM-SUB.

       P3350-EXIT.
           EXIT.


      *****************************************************************
      * S400-DATE-ROUTINES                                            *
      * JULIAN SUPPORT.                                               *
      *****************************************************************
       S400-DATE-ROUTINES SECTION.

       P4000-JULIAN-TO-ABS.
      * CONVERT YYDDD TO AN ABSOLUTE DAY NUMBER.  USED TO CHECK
      * THAT THE REVERSAL FALLS IN AN OPEN PERIOD.
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

       P4200-PERIOD-TEST.
      * A REVERSAL MAY ONLY BE RAISED INTO AN OPEN PERIOD.  THE
      * OPEN PERIOD IS THE NINETY DAYS ENDING ON THE CYCLE DATE.
      * THE SPAN IS COMPUTED THROUGH THE ABSOLUTE DAY NUMBERS SO
      * THAT A REVERSAL IN JANUARY OF A RESTATEMENT RAISED IN
      * DECEMBER IS HANDLED CORRECTLY.
           MOVE WS-AU-USE-YYDDD TO WS-JW-TEST.
           PERFORM P4000-JULIAN-TO-ABS THRU P4000-EXIT.
           MOVE WS-JW-ABS-TEST TO WS-JW-ABS-FROM.
           MOVE WS-CYCLE-YYDDD TO WS-JW-TEST.
           PERFORM P4000-JULIAN-TO-ABS THRU P4000-EXIT.
           MOVE WS-JW-ABS-TEST TO WS-JW-ABS-THRU.
           COMPUTE WS-JW-SPAN-DAYS =
                   WS-JW-ABS-THRU - WS-JW-ABS-FROM.
           IF WS-JW-SPAN-DAYS > 090
               MOVE WS-MSG-TEXT (9) TO WS-D1-DISPO.

       P4200-EXIT.
           EXIT.


      *****************************************************************
      * S600-REPORT                                                   *
      * REVERSAL REGISTER.                                            *
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
      * ONE LINE PER REVERSAL.
           IF WS-LINE-CNT > WS-MAX-LINES
               PERFORM P6000-HEADING THRU P6000-EXIT.
           MOVE BD-OCN TO WS-D1-OCN.
           MOVE BD-BAN TO WS-D1-BAN.
           MOVE BD-SECTION TO WS-D1-SECT.
           MOVE WS-RV-ORIG-AMT TO WS-D1-ORIG.
           MOVE WS-RV-REV-AMT TO WS-D1-REV.
           MOVE WS-RV-RESIDUAL TO WS-D1-RESID.
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
           MOVE CABS-BILL-DETAIL TO SU-ORIG-RECORD.
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
           MOVE 080                    TO CT-STEP-SEQ.
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
           DISPLAY 'REVERSALS WRITTEN' WS-REV-CNT.
           DISPLAY 'PARTIAL REVERSALS' WS-PART-CNT.
           DISPLAY 'NOT NET TO ZERO  ' WS-NONZERO-CNT.
           DISPLAY 'REVERSED AMOUNT  ' WS-TOT-REV-AMT.
           DISPLAY 'RESIDUAL AMOUNT  ' WS-TOT-RESID-AMT.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE ADJUST-IN-FILE
                 AUDIT-IN-FILE
                 REVERSE-OUT-FILE
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

