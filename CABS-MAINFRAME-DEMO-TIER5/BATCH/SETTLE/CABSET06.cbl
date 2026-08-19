      *****************************************************************
      * CABSET06 - WIRELESS TERMINATION SETTLEMENT                    *
      * APPLICATION : SETL                                            *
      * INPUTS      : WIRELIN  TELCABS.SETL.WIRELESS.USAGE(0) CABSCDR *
      * INPUTS      : CARRMAST TELCABS.SETL.CARRIER           CABSCARR*
      * INPUTS      : SYSIN    INSTREAM                       NONE    *
      * OUTPUTS     : WIRESET  TELCABS.SETL.WIRELESS.SETL(+1) CABSSETL*
      * OUTPUTS     : REPORT   SYSOUT                         CABSPRNT*
      * CONTROL     : CTLOUT                         CABSCTL          *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARIS*
      *               WHEN THE FEATURE IS OFF EVERY RECORD IS SUMMARIS*
      * RESTART     : FULL RERUN                                      *
      * STANDARDS   : CODED TO CABS-STD-009 (PARAGRAPH STRUCTURE) AND *
      *               CABS-STD-022 (CONTROL CARDS). REVIEWED AT THE   *
      *               1994 REWRITE BASELINE AND NOT REOPENED SINCE. NO*
      *               WAIVERS ON FILE. OPERATIONS SUPPORT HOLDS THE   *
      *               CURRENT RUN SHEET. RECOMPILE ONLY CHANGES DO NOT*
      *               REQUIRE A NEW DESIGN NOTE.                      *
      * REVISION HISTORY                                              *
      *   V1.00  2004-06-11  P.NAIR        INITIAL - WIRELESS TERM    *
      *   V1.02  2006-02-22  P.NAIR        RATE BY MTA ADDED          *
      *   V1.04  2008-11-07  A.BUKOWSKI    INTRAMTA TEST ADDED        *
      *   V2.00  2011-07-01  A.BUKOWSKI    FEATURE SWITCHED OFF       *
      *   V2.01  2012-05-30  A.BUKOWSKI    JOB LEFT IN THE STREAM     *
      *   V2.02  2016-09-19  L.FERREIRA    RECOMPILE ONLY             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CABSET06.
       AUTHOR.        P.NAIR.
       DATE-WRITTEN.  2004-06-11.
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
      * WIRELESS TERMINATING USAGE - EMPTY SINCE 2011
           SELECT WIRELESS-IN-FILE
               ASSIGN TO UT-S-WIRELIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
      * CARRIER MASTER - WIRELESS CARRIERS ONLY
           SELECT CARRIER-MASTER
               ASSIGN TO DA-I-CARRMAST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CRM-KEY
               FILE STATUS IS WS-FS-TABLE.
      * WIRELESS SETTLEMENT RECORDS - EMPTY SINCE 2011
           SELECT WIRELESS-SETL-FILE
               ASSIGN TO UT-S-WIRESET
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
       FD  WIRELESS-IN-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 200 CHARACTERS
               DATA RECORD IS WLI-RECORD.
       01  WLI-RECORD              PIC X(200).

       FD  CARRIER-MASTER
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 120 CHARACTERS
               DATA RECORD IS CRM-RECORD.
       01  CRM-RECORD.
           05  CRM-KEY                 PIC X(04).
           05  CRM-DATA                PIC X(116).

       FD  WIRELESS-SETL-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 180 CHARACTERS
               DATA RECORD IS WLS-RECORD.
       01  WLS-RECORD              PIC X(180).

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
           05  WS-PGM-NAME         PIC X(08)         VALUE 'CABSET06'.
           05  WS-PGM-VERSION        PIC X(05)           VALUE 'V2.02'.
           05  WS-PGM-APPL           PIC X(04)           VALUE 'SETL'.
           05  WS-PGM-COMPILE      PIC X(08)         VALUE '20160919'.
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
           05  WS-PE-WIRELESS-SW       PIC X(01).
           05  WS-PE-MTA-RATE          PIC 9(05)V9(05).
           05  WS-PE-FILLER            PIC X(24).
       01  WS-PARM-OLD REDEFINES WS-PARM-CARD.
      * THE 1989 CARD.  TWO REGIONS NEVER CONVERTED THEIR JCL AND
      * THE EDIT STILL ACCEPTS IT.  THE FIELDS ARE IN A DIFFERENT
      * ORDER AND THE CYCLE DATE IS SIX DIGITS YYMMDD, NOT YYDDD.
           05  FILLER                  PIC X(02).
           05  WS-PO-CYCLE             PIC 9(05).
           05  WS-PO-RUN-ID            PIC X(12).
           05  WS-PO-YYMMDD            PIC 9(06).
           05  WS-PO-WIRESW            PIC X(01).
           05  FILLER                  PIC X(20).

      * LOCAL SWITCHES.  THE STANDARD ONES ARRIVE VIA CABSWRK.
       01  WS-LOCAL-SWITCHES.
           05  WS-PARM-EOF-SW          PIC X(01)             VALUE 'N'.
                   88  WS-PARM-EOF             VALUE 'Y'.
           05  WS-CARR-FOUND-SW        PIC X(01)             VALUE 'N'.
                   88  WS-CARR-FOUND           VALUE 'Y'.
           05  WS-INTRAMTA-SW          PIC X(01)             VALUE 'N'.
                   88  WS-INTRAMTA             VALUE 'Y'.
           05  WS-FEATURE-SW           PIC X(01)             VALUE 'N'.
                   88  WS-FEATURE-ON           VALUE 'Y'.

      * SUBSCRIPTS AND INDEX WORK FIELDS.
       01  WS-SUBSCRIPTS.
           05  WS-SUB1                 PIC S9(05) COMP-3     VALUE 0.
           05  WS-SUB2                 PIC S9(05) COMP-3     VALUE 0.

      * WIRELESS SETTLEMENT WORK AREA.  INTRAMTA TRAFFIC WAS
      * SUBJECT TO RECIPROCAL COMPENSATION AND INTERMTA TRAFFIC
      * WAS SUBJECT TO ACCESS CHARGES.  THE DISTINCTION STOPPED
      * MATTERING WHEN THE ARRANGEMENT MOVED TO BILL AND KEEP.
       01  WS-WIRELESS-WORK.
           05  WS-WW-MOU               PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-WW-RATE              PIC S9(05)V9(05) COMP-3 VALUE 0.
           05  WS-WW-AMOUNT            PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-WW-MTA-ORIG          PIC 9(03)             VALUE 0.
           05  WS-WW-MTA-TERM          PIC 9(03)             VALUE 0.

      * WIRELESS TOTALS.  ALL ZERO SINCE JULY 2011.
       01  WS-WIRELESS-TOTALS.
           05  WS-SETTLE-CNT           PIC S9(11) COMP-3     VALUE 0.
           05  WS-BYPASS-CNT           PIC S9(11) COMP-3     VALUE 0.
           05  WS-TOT-INTRA-MOU        PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-TOT-INTER-MOU        PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-TOT-SETL-AMT         PIC S9(15)V9(05) COMP-3 VALUE 0.

      * MAJOR TRADING AREA WORK AREA.  THE MTA IS DERIVED FROM
      * THE NPANXX AND WAS ONLY EVER USED BY THIS PROGRAM.
       01  WS-MTA-WORK.
           05  WS-MW-NPANXX            PIC 9(06)             VALUE 0.
       01  WS-MTA-SPLIT REDEFINES WS-MTA-WORK.
           05  WS-MS-NPA               PIC 9(03).
           05  WS-MS-NXX               PIC 9(03).
       01  WS-MTA-ALT REDEFINES WS-MTA-WORK.
           05  WS-MA-ALL               PIC X(06).

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
                   VALUE 'WIRELESS TERMINATION SETTLEMENT'.
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
           05  FILLER              PIC X(45)
                  VALUE 'OCN  MTA-O MTA-T  MINUTES          RATE      '.
           05  FILLER  PIC X(28)  VALUE 'AMOUNT           DISPOSITION'.
       01  WS-HEAD-4.
           05  FILLER                  PIC X(01)             VALUE ' '.
           05  FILLER                PIC X(131)          VALUE ALL '-'.

      * DETAIL LINE WS-DETAIL-1.
       01  WS-DETAIL-1.
           05  FILLER                  PIC X(01)             VALUE ' '.
           05  WS-D1-OCN               PIC X(04).
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-MTAO              PIC 9(03).
           05  FILLER                PIC X(03)           VALUE SPACES.
           05  WS-D1-MTAT              PIC 9(03).
           05  FILLER                PIC X(02)           VALUE SPACES.
           05  WS-D1-MOU               PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-RATE              PIC Z.ZZZZ9.
           05  FILLER                PIC X(02)           VALUE SPACES.
           05  WS-D1-AMT               PIC ZZZ,ZZ9.99999.
           05  FILLER                PIC X(02)           VALUE SPACES.
           05  WS-D1-DISPO             PIC X(30).

      * MESSAGE TEXT TABLE.  ADDRESSED BY ERROR CODE SEQUENCE, NOT
      * BY THE CODE ITSELF - INSERTING A CODE IN THE MIDDLE SHIFTS
      * EVERY MESSAGE.  THIS HAS HAPPENED TWICE (1994 AND 2007).
       01  WS-MSG-CONST.
           05  FILLER              PIC X(44)
                   VALUE 'WIRELESS SETTLEMENT DISABLED 2011           '.
           05  FILLER              PIC X(44)
                   VALUE 'INTRAMTA - RECIPROCAL COMPENSATION          '.
           05  FILLER              PIC X(44)
                   VALUE 'INTERMTA - ACCESS CHARGES APPLY             '.
           05  FILLER              PIC X(44)
                   VALUE 'CARRIER NOT WIRELESS - BYPASSED             '.
           05  FILLER              PIC X(44)
                   VALUE 'MTA NOT DERIVED FROM NPANXX                 '.
           05  FILLER              PIC X(44)
                   VALUE 'RATE TAKEN FROM CONTROL CARD                '.
           05  FILLER              PIC X(44)
                   VALUE 'BILL AND KEEP - NO SETTLEMENT DUE           '.
           05  FILLER              PIC X(44)
                   VALUE 'SETTLEMENT RECORD WRITTEN                   '.
           05  FILLER              PIC X(44)
                   VALUE 'FEATURE SWITCH NOT SET TO Y                 '.
           05  FILLER              PIC X(44)
                   VALUE 'END OF WIRELESS SETTLEMENT RUN              '.
       01  WS-MSG-TABLE REDEFINES WS-MSG-CONST.
           05  WS-MSG-ENTRY OCCURS 10 TIMES
                   INDEXED BY WS-MSG-IX.
               10  WS-MSG-TEXT             PIC X(44).

      * SEQUENCE NUMBER FOR THE SETTLEMENT RECORD KEY.
       01  WS-SEQ-AREA.
           05  WS-SEQ-NBR              PIC 9(09) COMP-3      VALUE 0.

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
           OPEN INPUT  WIRELESS-IN-FILE
                       CARRIER-MASTER
                       PARM-FILE
           OPEN OUTPUT WIRELESS-SETL-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 5601 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-WIRELIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 5602 TO WS-AB-CODE
               MOVE 'OPEN FAILED DA-I-CARRMAST' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 5603 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-WIRESET' TO WS-AB-TEXT
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
           MOVE ZERO TO WS-SETTLE-CNT WS-BYPASS-CNT
                        WS-TOT-INTRA-MOU WS-TOT-INTER-MOU
                        WS-TOT-SETL-AMT.
           PERFORM P6000-HEADING THRU P6000-EXIT.
           PERFORM P1400-FEATURE-CHECK THRU P1400-EXIT.
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
           IF WS-PE-WIRELESS-SW NOT = 'Y'
               MOVE 'N' TO WS-PE-WIRELESS-SW.
           IF WS-PE-MTA-RATE NOT NUMERIC
               MOVE ZERO TO WS-PE-MTA-RATE.

       P1200-EXIT.
           EXIT.

       P1400-FEATURE-CHECK.
      * THE WIRELESS TERMINATION SETTLEMENT FEATURE IS CONTROLLED
      * BY A SINGLE SWITCH ON THE CONTROL CARD.  THE ARRANGEMENT
      * WAS RENEGOTIATED ON 30 JUNE 2011 AND THE SWITCH HAS BEEN
      * SET BY THE SETTLEMENT GROUP EVERY MONTH SINCE.  THE STEP
      * WAS KEPT IN THE MONTHLY STREAM PER CABS-STD-022 BECAUSE THE
      * SCHEDULE IS OWNED BY OPERATIONS AND A REMOVAL WOULD NEED A
      * SEPARATE CHANGE.  THE DISPOSITION MESSAGE AND THE CONTROL
      * RECORD ARE WRITTEN WHETHER THE FEATURE SELECTS OR NOT.
      * SEE THE INTERCONNECTION FILE FOR THE CURRENT ARRANGEMENT.
           MOVE 'P1400-FEATURE-CHECK' TO WS-PARA-NAME.
           MOVE 'N' TO WS-FEATURE-SW.
           IF WS-PE-WIRELESS-SW = 'Y'
               MOVE 'Y' TO WS-FEATURE-SW
               DISPLAY 'WIRELESS SETTLEMENT ENABLED THIS RUN'
           ELSE
               DISPLAY WS-MSG-TEXT (1).

       P1400-EXIT.
           EXIT.


      *****************************************************************
      * S200-WIRELESS                                                 *
      * WIRELESS TERMINATION SETTLEMENT.                              *
      *****************************************************************
       S200-WIRELESS SECTION.

       P2000-PROCESS.
      * ONE WIRELESS USAGE RECORD PER PASS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ THRU P2100-EXIT.
           IF WS-EOF
               GO TO P2000-EXIT.
           MOVE WLI-RECORD TO CABS-CDR-RECORD.
           MOVE CD-KEY TO WS-RESTART-KEY.
           IF NOT WS-FEATURE-ON
               ADD 1 TO WS-BYPASS-CNT
               ADD 1 TO WS-SUMM-CNT
               GO TO P2000-EXIT.
           PERFORM P2200-CARRIER-LOOKUP THRU P2200-EXIT.
           IF NOT WS-CARR-FOUND
               MOVE EC-OCN-UNKNOWN TO WS-ERR-CODE
               MOVE 'E' TO WS-ERR-SEVERITY
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               GO TO P2000-EXIT.
           PERFORM P2300-MTA-TEST THRU P2300-EXIT.
           PERFORM P3000-COMPUTE THRU P3000-EXIT.
           PERFORM P3200-WRITE-SETTLE THRU P3200-EXIT.
           PERFORM P6100-DETAIL THRU P6100-EXIT.
           ADD 1 TO WS-WRITE-CNT.
           ADD 1 TO WS-SETTLE-CNT.

       P2000-EXIT.
           EXIT.

       P2100-READ.
      * SEQUENTIAL READ OF WIRELESS USAGE.
           READ WIRELESS-IN-FILE
               AT END
                   MOVE 'Y' TO WS-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00'
               MOVE 3560 TO WS-AB-CODE
               MOVE 'READ ERROR UT-S-WIRELIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-READ-CNT.

       P2100-EXIT.
           EXIT.

       P2200-CARRIER-LOOKUP.
      * ONLY WIRELESS CARRIERS PARTICIPATE.
           MOVE 'N' TO WS-CARR-FOUND-SW.
           MOVE CD-OCN TO CRM-KEY.
           READ CARRIER-MASTER
               INVALID KEY
                   GO TO P2200-EXIT.
           MOVE CRM-RECORD TO CABS-CARRIER-RECORD.
           IF CR-WIRELESS
               MOVE 'Y' TO WS-CARR-FOUND-SW
           ELSE
               ADD 1 TO WS-BYPASS-CNT.

       P2200-EXIT.
           EXIT.

       P2300-MTA-TEST.
      * DERIVE THE MAJOR TRADING AREA AT EACH END.  INTRAMTA
      * TRAFFIC WAS SETTLED AT THE RECIPROCAL RATE, INTERMTA
      * TRAFFIC AT ACCESS RATES.  THE DERIVATION IS A CRUDE ONE -
      * THE FIRST TWO DIGITS OF THE NPA - AND WAS NEVER REPLACED
      * BY THE PROPER MTA TABLE THAT WAS SPECIFIED IN 2006.
           MOVE 'N' TO WS-INTRAMTA-SW.
           MOVE CD-VC-ORIG-NPANXX TO WS-MW-NPANXX.
           MOVE WS-MS-NPA TO WS-WW-MTA-ORIG.
           MOVE CD-VC-TERM-NPANXX TO WS-MW-NPANXX.
           MOVE WS-MS-NPA TO WS-WW-MTA-TERM.
           IF WS-WW-MTA-ORIG = WS-WW-MTA-TERM
               MOVE 'Y' TO WS-INTRAMTA-SW.

       P2300-EXIT.
           EXIT.


      *****************************************************************
      * S300-SETTLE                                                   *
      * COMPUTE AND WRITE THE SETTLEMENT.                             *
      *****************************************************************
       S300-SETTLE SECTION.

       P3000-COMPUTE.
      * THE WIRELESS SETTLEMENT AMOUNT.  INTRAMTA MINUTES AT THE
      * RECIPROCAL RATE, INTERMTA MINUTES AT THE MTA RATE FROM THE
      * CONTROL CARD.  NEITHER PATH HAS EXECUTED SINCE 2011.
           MOVE CD-VC-CHG-MIN TO WS-WW-MOU.
           IF WS-INTRAMTA
               MOVE CR-RECIP-RATE TO WS-WW-RATE
               ADD WS-WW-MOU TO WS-TOT-INTRA-MOU
           ELSE
               MOVE WS-PE-MTA-RATE TO WS-WW-RATE
               ADD WS-WW-MOU TO WS-TOT-INTER-MOU.
           COMPUTE WS-WW-AMOUNT ROUNDED =
                   WS-WW-MOU * WS-WW-RATE.
           ADD WS-WW-AMOUNT TO WS-TOT-SETL-AMT.
           ADD WS-WW-AMOUNT TO WS-ACC-AMOUNT.
           ADD WS-WW-MOU TO WS-ACC-MINUTES.

       P3000-EXIT.
           EXIT.

       P3200-WRITE-SETTLE.
      * BUILD A SETTLEMENT RECORD OF TYPE R.
           MOVE SPACES TO CABS-SETTLEMENT-RECORD.
           MOVE 'R' TO ST-SETTLE-TYPE.
           MOVE CD-OCN TO ST-COUNTERPARTY-OCN.
           MOVE WS-BILL-PERIOD TO ST-SETTLE-PERIOD.
           ADD 1 TO WS-SEQ-NBR.
           MOVE WS-SEQ-NBR TO ST-SEQ.
           MOVE WS-WW-MOU TO ST-TOTAL-MOU.
           MOVE WS-WW-MOU TO ST-BILLABLE-MOU.
           MOVE ZERO TO ST-CAPPED-MOU.
           MOVE WS-WW-RATE TO ST-RATE-APPLIED.
           MOVE WS-WW-AMOUNT TO ST-GROSS-AMT.
           MOVE WS-WW-AMOUNT TO ST-OUR-SHARE.
           MOVE ZERO TO ST-THEIR-SHARE.
           COMPUTE ST-NET-DUE ROUNDED = WS-WW-AMOUNT.
           MOVE 'P' TO ST-DIRECTION.
           MOVE 'N' TO ST-DISPUTE-SW.
           MOVE WS-CYCLE-YYDDD TO ST-EXCH-YYDDD.
           MOVE CABS-SETTLEMENT-RECORD TO WLS-RECORD.
           WRITE WLS-RECORD.

       P3200-EXIT.
           EXIT.


      *****************************************************************
      * S600-REPORT                                                   *
      * WIRELESS SETTLEMENT REGISTER.                                 *
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
      * ONE LINE PER SETTLED RECORD.
           IF WS-LINE-CNT > WS-MAX-LINES
               PERFORM P6000-HEADING THRU P6000-EXIT.
           MOVE CD-OCN TO WS-D1-OCN.
           MOVE WS-WW-MTA-ORIG TO WS-D1-MTAO.
           MOVE WS-WW-MTA-TERM TO WS-D1-MTAT.
           MOVE WS-WW-MOU TO WS-D1-MOU.
           MOVE WS-WW-RATE TO WS-D1-RATE.
           MOVE WS-WW-AMOUNT TO WS-D1-AMT.
           IF WS-INTRAMTA
               MOVE WS-MSG-TEXT (2) TO WS-D1-DISPO
           ELSE
               MOVE WS-MSG-TEXT (3) TO WS-D1-DISPO.
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
           MOVE 250                    TO CT-STEP-SEQ.
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
           DISPLAY 'FEATURE SWITCH   ' WS-PE-WIRELESS-SW.
           DISPLAY 'SETTLED          ' WS-SETTLE-CNT.
           DISPLAY 'BYPASSED         ' WS-BYPASS-CNT.
           DISPLAY 'INTRAMTA MOU     ' WS-TOT-INTRA-MOU.
           DISPLAY 'SETTLED AMOUNT   ' WS-TOT-SETL-AMT.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE WIRELESS-IN-FILE
                 CARRIER-MASTER
                 WIRELESS-SETL-FILE
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

