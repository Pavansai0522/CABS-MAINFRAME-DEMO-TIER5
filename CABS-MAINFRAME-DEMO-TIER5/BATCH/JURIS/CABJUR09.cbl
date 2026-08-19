      *****************************************************************
      * CABJUR09 - JURISDICTIONAL REVENUE SUMMARY                     *
      * APPLICATION : CABS                                            *
      * INPUTS      : PLUIN    TELCABS.CABS.CDR.PLU(0)        CABSCDR *
      * INPUTS      : SETLIN   TELCABS.SETL.SETTLE.MASTER     CABSSETL*
      * INPUTS      : SYSIN    INSTREAM                       NONE    *
      * OUTPUTS     : JURSUM   TELCABS.CABS.JURIS.SUMM(+1)    NONE    *
      * OUTPUTS     : REPORT   SYSOUT                         CABSPRNT*
      * CONTROL     : CTLOUT                         CABSCTL          *
      * BALANCE     : CT-READ = CT-SUMMARISED + CT-REJECTED           *
      *               EVERY USAGE RECORD IS SUMMARISED, NONE PASS THRO*
      * RESTART     : FULL RERUN                                      *
      * STANDARDS   : CODED TO CABS-STD-058 (DATE HANDLING) AND       *
      *               CABS-STD-014 (RECORD LAYOUTS). REVIEWED AT THE  *
      *               2013 APPLICATION AUDIT. ONE WAIVER ON FILE      *
      *               (W-0148, PRINT AREA ONLY). PARAGRAPH NUMBERING  *
      *               IS RESERVED IN HUNDREDS - DO NOT RENUMBER. THE  *
      *               MODULE IS IN THE MONTHLY REGRESSION PACK.       *
      * REVISION HISTORY                                              *
      *   V1.00  1988-02-08  R.T.WHEELER   INITIAL                    *
      *   V1.03  1990-09-14  D.OKONKWO     SETTLEMENT NET ADDED       *
      *   V2.00  1996-10-21  J.M.CASTILLO  Y2K REVIEW - NO IMPACT     *
      *   V2.02  2002-04-16  P.NAIR        STATE LEVEL SUMMARY        *
      *   V2.05  2008-08-29  A.BUKOWSKI    SETL READ MADE OPTIONAL    *
      *   V2.06  2013-05-22  L.FERREIRA    ROUNDING ALIGNED TO GL     *
      *   V2.08  2019-02-27  M.OYELARAN    RECOMPILE ONLY             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CABJUR09.
       AUTHOR.        R.T.WHEELER.
       DATE-WRITTEN.  1988-02-08.
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
      * FULLY JURISDICTIONED USAGE FROM CABJUR05
           SELECT PLU-IN-FILE
               ASSIGN TO UT-S-PLUIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
      * OWNED BY THE SETL APPLICATION - CROSS APP READ
           SELECT SETTLE-MASTER
               ASSIGN TO DA-I-SETLIN
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS STM-KEY
               FILE STATUS IS WS-FS-TABLE.
      * JURISDICTIONAL SUMMARY - FEEDS THE GENERAL LEDGER
           SELECT JURIS-SUMM-FILE
               ASSIGN TO UT-S-JURSUM
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
       FD  PLU-IN-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 200 CHARACTERS
               DATA RECORD IS PLI-RECORD.
       01  PLI-RECORD              PIC X(200).

       FD  SETTLE-MASTER
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 180 CHARACTERS
               DATA RECORD IS STM-RECORD.
       01  STM-RECORD.
           05  STM-KEY                 PIC X(20).
           05  STM-DATA                PIC X(160).

       FD  JURIS-SUMM-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 120 CHARACTERS
               DATA RECORD IS JSO-RECORD.
       01  JSO-RECORD              PIC X(120).

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
           05  WS-PGM-NAME         PIC X(08)         VALUE 'CABJUR09'.
           05  WS-PGM-VERSION        PIC X(05)           VALUE 'V2.08'.
           05  WS-PGM-APPL           PIC X(04)           VALUE 'CABS'.
           05  WS-PGM-COMPILE      PIC X(08)         VALUE '20190227'.
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

       COPY CABSSETL.

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
           05  WS-PE-SETL-SW           PIC X(01).
           05  WS-PE-SETL-PERIOD       PIC 9(06).
           05  WS-PE-FILLER            PIC X(28).
       01  WS-PARM-OLD REDEFINES WS-PARM-CARD.
      * THE 1989 CARD.  TWO REGIONS NEVER CONVERTED THEIR JCL AND
      * THE EDIT STILL ACCEPTS IT.  THE FIELDS ARE IN A DIFFERENT
      * ORDER AND THE CYCLE DATE IS SIX DIGITS YYMMDD, NOT YYDDD.
           05  FILLER                  PIC X(02).
           05  WS-PO-CYCLE             PIC 9(05).
           05  WS-PO-RUN-ID            PIC X(12).
           05  WS-PO-YYMMDD            PIC 9(06).
           05  WS-PO-SETLSW            PIC X(01).
           05  FILLER                  PIC X(20).

      * LOCAL SWITCHES.  THE STANDARD ONES ARRIVE VIA CABSWRK.
       01  WS-LOCAL-SWITCHES.
           05  WS-PARM-EOF-SW          PIC X(01)             VALUE 'N'.
                   88  WS-PARM-EOF             VALUE 'Y'.
           05  WS-SETL-FOUND-SW        PIC X(01)             VALUE 'N'.
                   88  WS-SETL-FOUND           VALUE 'Y'.
           05  WS-BREAK-SW             PIC X(01)             VALUE 'N'.
                   88  WS-BREAK-DETECTED       VALUE 'Y'.

      * SUBSCRIPTS AND INDEX WORK FIELDS.
       01  WS-SUBSCRIPTS.
           05  WS-SUB1                 PIC S9(05) COMP-3     VALUE 0.
           05  WS-SUB2                 PIC S9(05) COMP-3     VALUE 0.
           05  WS-SUB3                 PIC S9(05) COMP-3     VALUE 0.

      * SUMMARY TABLE.  ONE ENTRY PER CARRIER, STATE AND
      * JURISDICTION.  FIVE HUNDRED ENTRIES HAS BEEN ENOUGH SINCE
      * 1988 AND WAS NEARLY NOT ENOUGH AFTER THE 2011 MERGER.
       01  WS-SUMMARY-TABLE.
           05  WS-SU-ENTRY OCCURS 500 TIMES
                   INDEXED BY WS-SU-IX.
               10  WS-SU-KEY.
                   15  WS-SU-OCN               PIC X(04).
                   15  WS-SU-STATE             PIC X(02).
                   15  WS-SU-JURIS             PIC X(01).
               10  WS-SU-MOU               PIC S9(15)V9(02) COMP-3.
               10  WS-SU-AMT               PIC S9(15)V9(05) COMP-3.
               10  WS-SU-ROUNDED           PIC S9(15)V9(02) COMP-3.
               10  WS-SU-COUNT             PIC S9(11) COMP-3.
               10  WS-SU-SETL              PIC S9(13)V9(02) COMP-3.
       01  WS-SUMMARY-CTL.
           05  WS-SU-USED              PIC S9(05) COMP-3     VALUE 0.
           05  WS-SU-MAX               PIC S9(05) COMP-3     VALUE 500.
           05  WS-SU-HIT               PIC S9(05) COMP-3     VALUE 0.
           05  WS-SU-FOUND-SW          PIC X(01)             VALUE 'N'.
                   88  WS-SU-FOUND              VALUE 'Y'.

      * SUMMARY RUN TOTALS.  WS-ROUND-DIFF IS THE DIFFERENCE
      * BETWEEN THE FIVE PLACE ACCUMULATION AND THE TWO PLACE
      * FIGURE SENT TO THE LEDGER.  IT IS REPORTED AND ABSORBED.
       01  WS-SUMMARY-TOTALS.
           05  WS-TOT-IS-AMT           PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-TOT-SS-AMT           PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-TOT-LC-AMT           PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-TOT-SETL-AMT         PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-TOT-MOU              PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-SUMM-LINES           PIC S9(09) COMP-3     VALUE 0.
           05  WS-ROUND-DIFF           PIC S9(09)V9(05) COMP-3 VALUE 0.

      * AMOUNT WORK AREA WITH TWO REDEFINES USED BY THE SUMMARY
      * WRITE.
       01  WS-SUMM-OUT.
           05  WS-SO-OCN             PIC X(04)           VALUE SPACES.
           05  WS-SO-STATE           PIC X(02)           VALUE SPACES.
           05  WS-SO-JURIS           PIC X(01)           VALUE SPACES.
           05  WS-SO-PERIOD            PIC 9(06)             VALUE 0.
           05  WS-SO-MOU               PIC S9(15)V9(02)      VALUE 0.
           05  WS-SO-AMT               PIC S9(15)V9(02)      VALUE 0.
           05  WS-SO-SETL              PIC S9(13)V9(02)      VALUE 0.
           05  WS-SO-COUNT             PIC 9(11)             VALUE 0.
           05  WS-SO-FILLER          PIC X(20)           VALUE SPACES.
       01  WS-SUMM-OUT-R REDEFINES WS-SUMM-OUT.
           05  WS-SR-KEY               PIC X(13).
           05  WS-SR-BODY              PIC X(107).
       01  WS-SUMM-OUT-L REDEFINES WS-SUMM-OUT.
           05  WS-SL-LEDGER-KEY        PIC X(07).
           05  WS-SL-REST              PIC X(113).

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
                   VALUE 'JURISDICTIONAL REVENUE SUMMARY'.
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
                   VALUE 'OCN  ST J  RECORDS      MINUTES          '.
           05  FILLER              PIC X(33)
                   VALUE 'AMOUNT             SETTLEMENT NET'.
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
           05  FILLER                PIC X(02)           VALUE SPACES.
           05  WS-D1-COUNT             PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                PIC X(02)           VALUE SPACES.
           05  WS-D1-MOU               PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-AMT               PIC ZZZ,ZZZ,ZZ9.99999.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-SETL              PIC ZZZ,ZZZ,ZZ9.99-.

      * MESSAGE TEXT TABLE.  ADDRESSED BY ERROR CODE SEQUENCE, NOT
      * BY THE CODE ITSELF - INSERTING A CODE IN THE MIDDLE SHIFTS
      * EVERY MESSAGE.  THIS HAS HAPPENED TWICE (1994 AND 2007).
       01  WS-MSG-CONST.
           05  FILLER              PIC X(44)
                   VALUE 'SUMMARY LINE WRITTEN                        '.
           05  FILLER              PIC X(44)
                   VALUE 'SUMMARY TABLE FULL - RECORD DROPPED         '.
           05  FILLER              PIC X(44)
                   VALUE 'SETTLEMENT MASTER NOT READ THIS RUN         '.
           05  FILLER              PIC X(44)
                   VALUE 'SETTLEMENT RECORD NOT FOUND                 '.
           05  FILLER              PIC X(44)
                   VALUE 'SETTLEMENT NET APPLIED TO SUMMARY           '.
           05  FILLER              PIC X(44)
                   VALUE 'ROUNDING DIFFERENCE ABSORBED                '.
           05  FILLER              PIC X(44)
                   VALUE 'JURISDICTION INDETERMINATE - EXCLUDED       '.
           05  FILLER              PIC X(44)
                   VALUE 'ZERO AMOUNT SUMMARY LINE SUPPRESSED         '.
           05  FILLER              PIC X(44)
                   VALUE 'LEDGER KEY DERIVED FROM OCN AND STATE       '.
           05  FILLER              PIC X(44)
                   VALUE 'END OF SUMMARY RUN                          '.
       01  WS-MSG-TABLE REDEFINES WS-MSG-CONST.
           05  WS-MSG-ENTRY OCCURS 10 TIMES
                   INDEXED BY WS-MSG-IX.
               10  WS-MSG-TEXT             PIC X(44).

      * SETTLEMENT MASTER KEY BUILD AREA.  THE KEY LAYOUT BELONGS
      * TO THE SETL APPLICATION AND IS REPRODUCED HERE BY HAND.
       01  WS-SETL-KEY.
           05  WS-SK-TYPE            PIC X(01)           VALUE SPACES.
           05  WS-SK-OCN             PIC X(04)           VALUE SPACES.
           05  WS-SK-PERIOD            PIC 9(06)             VALUE 0.
           05  WS-SK-SEQ               PIC 9(09)             VALUE 0.

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
           OPEN INPUT  PLU-IN-FILE
                       SETTLE-MASTER
                       PARM-FILE
           OPEN OUTPUT JURIS-SUMM-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 4901 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-PLUIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 4902 TO WS-AB-CODE
               MOVE 'OPEN FAILED DA-I-SETLIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 4903 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-JURSUM' TO WS-AB-TEXT
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
           MOVE ZERO TO WS-TOT-IS-AMT WS-TOT-SS-AMT
                        WS-TOT-LC-AMT WS-TOT-SETL-AMT
                        WS-TOT-MOU WS-SUMM-LINES
                        WS-SU-USED.
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
           IF WS-PE-SETL-SW NOT = 'Y'
               MOVE 'N' TO WS-PE-SETL-SW.
           IF WS-PE-SETL-PERIOD NOT NUMERIC
               MOVE WS-PC-BILL-PERIOD TO WS-PE-SETL-PERIOD.

       P1200-EXIT.
           EXIT.


      *****************************************************************
      * S200-SUMMARISE                                                *
      * ACCUMULATE THE SUMMARY TABLE.                                 *
      *****************************************************************
       S200-SUMMARISE SECTION.

       P2000-PROCESS.
      * ONE USAGE RECORD PER PASS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ THRU P2100-EXIT.
           IF WS-EOF
               GO TO P2000-EXIT.
           MOVE PLI-RECORD TO CABS-CDR-RECORD.
           MOVE CD-KEY TO WS-RESTART-KEY.
           IF CD-INDETERMINATE
               ADD 1 TO WS-REJECT-CNT
               GO TO P2000-EXIT.
           PERFORM P2200-ACCUM THRU P2200-EXIT.
           ADD 1 TO WS-SUMM-CNT.

       P2000-EXIT.
           EXIT.

       P2100-READ.
      * SEQUENTIAL READ OF PRICED USAGE.
           READ PLU-IN-FILE
               AT END
                   MOVE 'Y' TO WS-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00'
               MOVE 3490 TO WS-AB-CODE
               MOVE 'READ ERROR UT-S-PLUIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-READ-CNT.

       P2100-EXIT.
           EXIT.

       P2200-ACCUM.
      * FIND OR CREATE THE SUMMARY ENTRY AND ADD TO IT.  THE
      * ACCUMULATION IS IN THE ORDER THE RECORDS ARRIVE - THE FILE
      * IS NOT RESEQUENCED FIRST, BY AGREEMENT, BECAUSE THE ORDER
      * AFFECTS THE FIFTH DECIMAL PLACE AND THE PARALLEL RUN
      * COMPARISON IS DONE AT FIVE PLACES.
           MOVE 'P2200-ACCUM' TO WS-PARA-NAME.
           MOVE 'N' TO WS-SU-FOUND-SW.
           MOVE 1 TO WS-SUB1.
           PERFORM P2250-FIND-ENTRY THRU P2250-EXIT
               UNTIL WS-SUB1 > WS-SU-USED
                  OR WS-SU-FOUND.
           IF NOT WS-SU-FOUND
               PERFORM P2260-ADD-ENTRY THRU P2260-EXIT.
           IF NOT WS-SU-FOUND
               GO TO P2200-EXIT.
           SET WS-SU-IX TO WS-SU-HIT.
           ADD CD-VC-CHG-MIN TO WS-SU-MOU (WS-SU-IX).
           ADD 1 TO WS-SU-COUNT (WS-SU-IX).
           ADD CD-VC-CHG-MIN TO WS-TOT-MOU.
           ADD CD-VC-CHG-MIN TO WS-ACC-MINUTES.

       P2200-EXIT.
           EXIT.

       P2250-FIND-ENTRY.
      * ONE SUMMARY ENTRY COMPARE.
           IF WS-SU-OCN (WS-SUB1) = CD-OCN AND
              WS-SU-JURIS (WS-SUB1) = CD-JURIS-CD
               MOVE 'Y' TO WS-SU-FOUND-SW
               MOVE WS-SUB1 TO WS-SU-HIT
               GO TO P2250-EXIT.
           ADD 1 TO WS-SUB1.

       P2250-EXIT.
           EXIT.

       P2260-ADD-ENTRY.
      * ADD A NEW SUMMARY ENTRY.  A FULL TABLE DROPS THE RECORD AND
      * RAISES A SUSPENSE - THE ALTERNATIVE WOULD BE TO MERGE IT
      * INTO A NEIGHBOURING ENTRY AND MISSTATE THE LEDGER.
           IF WS-SU-USED NOT < WS-SU-MAX
               MOVE EC-OUT-OF-BALANCE TO WS-ERR-CODE
               MOVE 'E' TO WS-ERR-SEVERITY
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               GO TO P2260-EXIT.
           ADD 1 TO WS-SU-USED.
           MOVE WS-SU-USED TO WS-SU-HIT.
           SET WS-SU-IX TO WS-SU-HIT.
           MOVE CD-OCN TO WS-SU-OCN (WS-SU-IX).
           MOVE SPACES TO WS-SU-STATE (WS-SU-IX).
           MOVE CD-JURIS-CD TO WS-SU-JURIS (WS-SU-IX).
           MOVE ZERO TO WS-SU-MOU (WS-SU-IX).
           MOVE ZERO TO WS-SU-AMT (WS-SU-IX).
           MOVE ZERO TO WS-SU-COUNT (WS-SU-IX).
           MOVE ZERO TO WS-SU-SETL (WS-SU-IX).
           MOVE 'Y' TO WS-SU-FOUND-SW.

       P2260-EXIT.
           EXIT.


      *****************************************************************
      * S300-SETTLEMENT-READ                                          *
      * CROSS APPLICATION READ OF THE SETL MASTER.                    *
      *****************************************************************
       S300-SETTLEMENT-READ SECTION.

       P3000-SETTLE-LOOKUP.
      * READ THE SETTLEMENT MASTER FOR THIS CARRIER AND PERIOD AND
      * BRING THE NET SETTLEMENT POSITION INTO THE JURISDICTIONAL
      * SUMMARY.  THE DATASET BELONGS TO THE SETTLEMENT APPLICATION
      * AND IS READ DIRECTLY RATHER THAN THROUGH AN INTERFACE FILE.
      * THE SETL BATCH WINDOW MUST THEREFORE FINISH BEFORE THIS STEP
      * RUNS, AND NOTHING IN EITHER SCHEDULE ENFORCES THAT.
      * THE INTERFACE AGREEMENT IS HELD BY THE APPLICATION OWNER.
           MOVE 'P3000-SETTLE-LOOKUP' TO WS-PARA-NAME.
           IF WS-PE-SETL-SW NOT = 'Y'
               GO TO P3000-EXIT.
           MOVE 'N' TO WS-SETL-FOUND-SW.
           MOVE SPACES TO WS-SETL-KEY.
           MOVE 'M' TO WS-SK-TYPE.
           MOVE WS-SU-OCN (WS-SU-IX) TO WS-SK-OCN.
           MOVE WS-PE-SETL-PERIOD TO WS-SK-PERIOD.
           MOVE WS-SETL-KEY TO STM-KEY.
           READ SETTLE-MASTER
               INVALID KEY
                   GO TO P3000-EXIT.
           MOVE STM-RECORD TO CABS-SETTLEMENT-RECORD.
           MOVE 'Y' TO WS-SETL-FOUND-SW.
           ADD ST-NET-DUE TO WS-SU-SETL (WS-SU-IX).
           ADD ST-NET-DUE TO WS-TOT-SETL-AMT.

       P3000-EXIT.
           EXIT.


      *****************************************************************
      * S400-SUMMARY-WRITE                                            *
      * WRITE THE SUMMARY FILE.                                       *
      *****************************************************************
       S400-SUMMARY-WRITE SECTION.

       P4000-WRITE-SUMMARY.
      * WRITE ONE RECORD PER SUMMARY ENTRY.  THE AMOUNT IS ROUNDED
      * TO TWO PLACES FOR THE LEDGER.  THE DIFFERENCE BETWEEN THE
      * ROUNDED FIGURE AND THE FIVE PLACE ACCUMULATION IS ABSORBED
      * HERE AND REPORTED.  CABJUR11 TRUNCATES THE SAME FIGURE.
           MOVE 'P4000-WRITE-SUMMARY' TO WS-PARA-NAME.
           MOVE 1 TO WS-SUB2.
           PERFORM P4100-WRITE-ONE THRU P4100-EXIT
               UNTIL WS-SUB2 > WS-SU-USED.

       P4000-EXIT.
           EXIT.

       P4100-WRITE-ONE.
      * ONE SUMMARY RECORD.
           SET WS-SU-IX TO WS-SUB2.
           PERFORM P3000-SETTLE-LOOKUP THRU P3000-EXIT.
           MOVE SPACES TO WS-SUMM-OUT.
           MOVE WS-SU-OCN (WS-SU-IX) TO WS-SO-OCN.
           MOVE WS-SU-STATE (WS-SU-IX) TO WS-SO-STATE.
           MOVE WS-SU-JURIS (WS-SU-IX) TO WS-SO-JURIS.
           MOVE WS-BILL-PERIOD TO WS-SO-PERIOD.
           MOVE WS-SU-MOU (WS-SU-IX) TO WS-SO-MOU.
           COMPUTE WS-SO-AMT ROUNDED = WS-SU-AMT (WS-SU-IX).
           COMPUTE WS-ROUND-DIFF = WS-ROUND-DIFF
                 + (WS-SU-AMT (WS-SU-IX) - WS-SO-AMT).
           MOVE WS-SU-SETL (WS-SU-IX) TO WS-SO-SETL.
           MOVE WS-SU-COUNT (WS-SU-IX) TO WS-SO-COUNT.
           MOVE WS-SUMM-OUT TO JSO-RECORD.
           WRITE JSO-RECORD.
           ADD 1 TO WS-SUMM-LINES.
           PERFORM P6100-DETAIL THRU P6100-EXIT.
           ADD 1 TO WS-SUB2.

       P4100-EXIT.
           EXIT.


      *****************************************************************
      * S600-REPORT                                                   *
      * SUMMARY REGISTER.                                             *
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
      * ONE LINE PER SUMMARY ENTRY.
           IF WS-LINE-CNT > WS-MAX-LINES
               PERFORM P6000-HEADING THRU P6000-EXIT.
           MOVE WS-SU-OCN (WS-SU-IX) TO WS-D1-OCN.
           MOVE WS-SU-STATE (WS-SU-IX) TO WS-D1-STATE.
           MOVE WS-SU-JURIS (WS-SU-IX) TO WS-D1-JURIS.
           MOVE WS-SU-COUNT (WS-SU-IX) TO WS-D1-COUNT.
           MOVE WS-SU-MOU (WS-SU-IX) TO WS-D1-MOU.
           MOVE WS-SU-AMT (WS-SU-IX) TO WS-D1-AMT.
           MOVE WS-SU-SETL (WS-SU-IX) TO WS-D1-SETL.
           WRITE PRT-RECORD FROM WS-DETAIL-1 AFTER ADVANCING 1 LINES.
           ADD 1 TO WS-LINE-CNT.

       P6100-EXIT.
           EXIT.

       P4900-FINAL-WRITE.
      * CALLED FROM P9000 - THE SUMMARY IS ONLY WRITTEN AT END OF
      * FILE BECAUSE THE TABLE IS NOT COMPLETE UNTIL THEN.
           PERFORM P4000-WRITE-SUMMARY THRU P4000-EXIT.

       P4900-EXIT.
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
           MOVE 090                    TO CT-STEP-SEQ.
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
           PERFORM P4900-FINAL-WRITE THRU P4900-EXIT.
           DISPLAY 'SUMMARY LINES    ' WS-SUMM-LINES.
           DISPLAY 'INTERSTATE AMT   ' WS-TOT-IS-AMT.
           DISPLAY 'INTRASTATE AMT   ' WS-TOT-SS-AMT.
           DISPLAY 'LOCAL AMT        ' WS-TOT-LC-AMT.
           DISPLAY 'SETTLEMENT NET   ' WS-TOT-SETL-AMT.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE PLU-IN-FILE
                 SETTLE-MASTER
                 JURIS-SUMM-FILE
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

