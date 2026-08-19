      *****************************************************************
      * CABSET02 - MEET POINT CIRCUIT EXTRACT AND ELIGIBILITY         *
      * APPLICATION : SETL                                            *
      * INPUTS      : CIRCMAST TELCABS.SETL.CIRCUIT           CABSCIRC*
      * INPUTS      : CDRIN    TELCABS.CABS.CDR.PLU(0)        CABSCDR *
      * INPUTS      : SYSIN    INSTREAM                       NONE    *
      * OUTPUTS     : MPBEXT   TELCABS.SETL.MPB.EXTRACT(+1)   CABSCIRC*
      * OUTPUTS     : REPORT   SYSOUT                         CABSPRNT*
      * CONTROL     : CTLOUT                         CABSCTL          *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARIS*
      *               SUMMARISED = CIRCUITS NOT MEET POINT ELIGIBLE   *
      * RESTART     : FULL RERUN                                      *
      * STANDARDS   : CODED TO CABS-STD-058 (DATE HANDLING) AND       *
      *               CABS-STD-014 (RECORD LAYOUTS). REVIEWED AT THE  *
      *               1996 Y2K SWEEP AND AGAIN AT THE 2016 STANDARDS  *
      *               SWEEP. NO WAIVERS ON FILE.                      *
      * REVISION HISTORY                                              *
      *   V1.00  1987-09-21  R.T.WHEELER   INITIAL                    *
      *   V1.04  1991-01-14  D.OKONKWO     USAGE MATCH ADDED          *
      *   V2.00  1996-11-19  J.M.CASTILLO  Y2K REVIEW - NO IMPACT     *
      *   V2.03  2002-09-05  P.NAIR        DISCONNECT DATE HONOURED   *
      *   V2.05  2008-01-31  A.BUKOWSKI    UNE CIRCUITS EXCLUDED      *
      *   V2.07  2017-03-09  L.FERREIRA    RECOMPILE ONLY             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CABSET02.
       AUTHOR.        R.T.WHEELER.
       DATE-WRITTEN.  1987-09-21.
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
      * CIRCUIT INVENTORY - OWNED BY SETL
           SELECT CIRCUIT-MASTER
               ASSIGN TO DA-I-CIRCMAST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS CIM-KEY
               FILE STATUS IS WS-FS-INPUT.
      * OWNED BY THE CABS APPLICATION - CROSS APP READ
           SELECT CDR-IN-FILE
               ASSIGN TO UT-S-CDRIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-TABLE.
      * ELIGIBLE MEET POINT CIRCUITS WITH USAGE ATTACHED
           SELECT MPB-EXTRACT-FILE
               ASSIGN TO UT-S-MPBEXT
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
       FD  CIRCUIT-MASTER
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 120 CHARACTERS
               DATA RECORD IS CIM-RECORD.
       01  CIM-RECORD.
           05  CIM-KEY                 PIC X(20).
           05  CIM-DATA                PIC X(100).

       FD  CDR-IN-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 200 CHARACTERS
               DATA RECORD IS CDI-RECORD.
       01  CDI-RECORD              PIC X(200).

       FD  MPB-EXTRACT-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 200 CHARACTERS
               DATA RECORD IS MPX-RECORD.
       01  MPX-RECORD              PIC X(200).

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
           05  WS-PGM-NAME         PIC X(08)         VALUE 'CABSET02'.
           05  WS-PGM-VERSION        PIC X(05)           VALUE 'V2.07'.
           05  WS-PGM-APPL           PIC X(04)           VALUE 'SETL'.
           05  WS-PGM-COMPILE      PIC X(08)         VALUE '20170309'.
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

       COPY CABSCDR.

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
           05  WS-PE-SETTLE-PERIOD     PIC 9(06).
           05  WS-PE-INCL-UNE          PIC X(01).
           05  WS-PE-FILLER            PIC X(28).
       01  WS-PARM-OLD REDEFINES WS-PARM-CARD.
      * THE 1989 CARD.  TWO REGIONS NEVER CONVERTED THEIR JCL AND
      * THE EDIT STILL ACCEPTS IT.  THE FIELDS ARE IN A DIFFERENT
      * ORDER AND THE CYCLE DATE IS SIX DIGITS YYMMDD, NOT YYDDD.
           05  FILLER                  PIC X(02).
           05  WS-PO-CYCLE             PIC 9(05).
           05  WS-PO-RUN-ID            PIC X(12).
           05  WS-PO-YYMMDD            PIC 9(06).
           05  WS-PO-PERIOD            PIC 9(06).
           05  FILLER                  PIC X(20).

      * LOCAL SWITCHES.  THE STANDARD ONES ARRIVE VIA CABSWRK.
       01  WS-LOCAL-SWITCHES.
           05  WS-PARM-EOF-SW          PIC X(01)             VALUE 'N'.
                   88  WS-PARM-EOF             VALUE 'Y'.
           05  WS-CDR-EOF-SW           PIC X(01)             VALUE 'N'.
                   88  WS-CDR-EOF              VALUE 'Y'.
           05  WS-ELIGIBLE-SW          PIC X(01)             VALUE 'Y'.
                   88  WS-ELIGIBLE             VALUE 'Y'.
           05  WS-USAGE-SW             PIC X(01)             VALUE 'N'.
                   88  WS-USAGE-FOUND          VALUE 'Y'.

      * SUBSCRIPTS AND INDEX WORK FIELDS.
       01  WS-SUBSCRIPTS.
           05  WS-SUB1                 PIC S9(05) COMP-3     VALUE 0.
           05  WS-SUB2                 PIC S9(05) COMP-3     VALUE 0.

      * EXTRACT STATISTICS.
       01  WS-EXTRACT-COUNTERS.
           05  WS-ELIG-CNT             PIC S9(09) COMP-3     VALUE 0.
           05  WS-NOTELIG-CNT          PIC S9(09) COMP-3     VALUE 0.
           05  WS-MATCH-CNT            PIC S9(09) COMP-3     VALUE 0.
           05  WS-DISC-CNT             PIC S9(09) COMP-3     VALUE 0.
           05  WS-UNE-CNT              PIC S9(09) COMP-3     VALUE 0.
           05  WS-TOT-MOU              PIC S9(15)V9(02) COMP-3 VALUE 0.

      * EXTRACT RECORD.  THE CIRCUIT RECORD WITH THE USAGE
      * TOTALS APPENDED.  TWO REDEFINES GIVE THE SETTLEMENT
      * CALCULATOR ITS VIEW.
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
           05  WS-XP-PCT-AREA          PIC X(20).
           05  WS-XP-TAIL              PIC X(135).

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
                   VALUE 'MEET POINT CIRCUIT ELIGIBILITY EXTRACT'.
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
           05  FILLER              PIC X(49)
               VALUE 'CIRCUIT-ID           TRK-GRP  OCN  OTHER OUR-PCT'.
           05  FILLER              PIC X(38)
                   VALUE 'OTHER-PCT  MINUTES         DISPOSITION'.
       01  WS-HEAD-4.
           05  FILLER                  PIC X(01)             VALUE ' '.
           05  FILLER                PIC X(131)          VALUE ALL '-'.

      * DETAIL LINE WS-DETAIL-1.
       01  WS-DETAIL-1.
           05  FILLER                  PIC X(01)             VALUE ' '.
           05  WS-D1-CIRCUIT           PIC X(20).
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-TRUNK             PIC X(08).
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-OCN               PIC X(04).
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-OTHER             PIC X(04).
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-OURPCT            PIC ZZ9.99.
           05  FILLER                PIC X(03)           VALUE SPACES.
           05  WS-D1-OTHPCT            PIC ZZ9.99.
           05  FILLER                PIC X(02)           VALUE SPACES.
           05  WS-D1-MOU               PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-DISPO             PIC X(24).

      * MESSAGE TEXT TABLE.  ADDRESSED BY ERROR CODE SEQUENCE, NOT
      * BY THE CODE ITSELF - INSERTING A CODE IN THE MIDDLE SHIFTS
      * EVERY MESSAGE.  THIS HAS HAPPENED TWICE (1994 AND 2007).
       01  WS-MSG-CONST.
           05  FILLER              PIC X(44)
                   VALUE 'ELIGIBLE - EXTRACTED                        '.
           05  FILLER              PIC X(44)
                   VALUE 'MEET POINT SWITCH NOT SET                   '.
           05  FILLER              PIC X(44)
                   VALUE 'CIRCUIT DISCONNECTED BEFORE PERIOD          '.
           05  FILLER              PIC X(44)
                   VALUE 'UNBUNDLED CIRCUIT - EXCLUDED SINCE 2008     '.
           05  FILLER              PIC X(44)
                   VALUE 'NO OTHER LEC ON THE CIRCUIT RECORD          '.
           05  FILLER              PIC X(44)
                   VALUE 'NO USAGE FOUND FOR THE CIRCUIT              '.
           05  FILLER              PIC X(44)
                   VALUE 'USAGE MATCHED FROM THE CABS USAGE FILE      '.
           05  FILLER              PIC X(44)
                   VALUE 'CIRCUIT NOT ON THE BILLING ACCOUNT          '.
           05  FILLER              PIC X(44)
                   VALUE 'PERCENTAGES BOTH ZERO                       '.
           05  FILLER              PIC X(44)
                   VALUE 'END OF EXTRACT RUN                          '.
       01  WS-MSG-TABLE REDEFINES WS-MSG-CONST.
           05  WS-MSG-ENTRY OCCURS 10 TIMES
                   INDEXED BY WS-MSG-IX.
               10  WS-MSG-TEXT             PIC X(44).

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
           OPEN INPUT  CIRCUIT-MASTER
                       CDR-IN-FILE
                       PARM-FILE
           OPEN OUTPUT MPB-EXTRACT-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 5201 TO WS-AB-CODE
               MOVE 'OPEN FAILED DA-I-CIRCMAST' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 5202 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-CDRIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 5203 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-MPBEXT' TO WS-AB-TEXT
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
           MOVE ZERO TO WS-ELIG-CNT WS-NOTELIG-CNT
                        WS-MATCH-CNT WS-DISC-CNT
                        WS-UNE-CNT WS-TOT-MOU.
           PERFORM P6000-HEADING THRU P6000-EXIT.
           PERFORM P2600-READ-CDR THRU P2600-EXIT.
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
           IF WS-PE-SETTLE-PERIOD NOT NUMERIC
               MOVE WS-PC-BILL-PERIOD TO WS-PE-SETTLE-PERIOD.
           IF WS-PE-INCL-UNE NOT = 'Y'
               MOVE 'N' TO WS-PE-INCL-UNE.

       P1200-EXIT.
           EXIT.


      *****************************************************************
      * S200-EXTRACT                                                  *
      * SELECT THE ELIGIBLE CIRCUITS.                                 *
      *****************************************************************
       S200-EXTRACT SECTION.

       P2000-PROCESS.
      * ONE CIRCUIT PER PASS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ THRU P2100-EXIT.
           IF WS-EOF
               GO TO P2000-EXIT.
           MOVE CIM-RECORD TO CABS-CIRCUIT-RECORD.
           MOVE CI-CIRCUIT-ID TO WS-RESTART-KEY.
           MOVE SPACES TO WS-D1-DISPO.
           MOVE 'Y' TO WS-ELIGIBLE-SW.
           PERFORM P2200-ELIGIBILITY THRU P2200-EXIT.
           IF NOT WS-ELIGIBLE
               ADD 1 TO WS-NOTELIG-CNT
               ADD 1 TO WS-SUMM-CNT
               PERFORM P6100-DETAIL THRU P6100-EXIT
               GO TO P2000-EXIT.
           PERFORM P2500-MATCH-USAGE THRU P2500-EXIT.
           PERFORM P3000-BUILD-EXTRACT THRU P3000-EXIT.
           PERFORM P6100-DETAIL THRU P6100-EXIT.
           ADD 1 TO WS-WRITE-CNT.
           ADD 1 TO WS-ELIG-CNT.

       P2000-EXIT.
           EXIT.

       P2100-READ.
      * SEQUENTIAL PASS OF THE CIRCUIT MASTER.
           READ CIRCUIT-MASTER
               AT END
                   MOVE 'Y' TO WS-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00'
               MOVE 3520 TO WS-AB-CODE
               MOVE 'READ ERROR DA-I-CIRCMAST' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-READ-CNT.

       P2100-EXIT.
           EXIT.

       P2200-ELIGIBILITY.
      * A CIRCUIT IS MEET POINT ELIGIBLE WHEN THE SWITCH IS SET, AN
      * OTHER LEC IS NAMED AND THE CIRCUIT WAS IN SERVICE DURING THE
      * SETTLEMENT PERIOD.  UNBUNDLED CIRCUITS WERE EXCLUDED IN 2008
      * WHEN THEY MOVED TO A SEPARATE ARRANGEMENT.
           MOVE 'P2200-ELIGIBILITY' TO WS-PARA-NAME.
           IF CI-MPB-SW NOT = 'Y'
               MOVE 'N' TO WS-ELIGIBLE-SW
               MOVE WS-MSG-TEXT (2) TO WS-D1-DISPO
               GO TO P2200-EXIT.
           IF CI-MPB-OTHER-OCN = SPACES
               MOVE 'N' TO WS-ELIGIBLE-SW
               MOVE WS-MSG-TEXT (5) TO WS-D1-DISPO
               MOVE EC-MPB-PCT-INVALID TO WS-ERR-CODE
               MOVE 'E' TO WS-ERR-SEVERITY
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               GO TO P2200-EXIT.
           IF CI-UNE AND WS-PE-INCL-UNE NOT = 'Y'
               MOVE 'N' TO WS-ELIGIBLE-SW
               ADD 1 TO WS-UNE-CNT
               MOVE WS-MSG-TEXT (4) TO WS-D1-DISPO
               GO TO P2200-EXIT.
           IF CI-DISC-YYDDD NOT = ZERO
               IF CI-DISC-YYDDD < WS-CYCLE-YYDDD
                   MOVE 'N' TO WS-ELIGIBLE-SW
                   ADD 1 TO WS-DISC-CNT
                   MOVE WS-MSG-TEXT (3) TO WS-D1-DISPO
                   GO TO P2200-EXIT.
           IF CI-MPB-OUR-PCT = ZERO AND CI-MPB-OTHER-PCT = ZERO
               MOVE 'N' TO WS-ELIGIBLE-SW
               MOVE WS-MSG-TEXT (9) TO WS-D1-DISPO.

       P2200-EXIT.
           EXIT.

       P2500-MATCH-USAGE.
      * ATTACH THE USAGE FOR THE CIRCUIT.  THE USAGE FILE BELONGS TO
      * THE BILLING APPLICATION AND IS READ DIRECTLY.  IT IS IN
      * CARRIER AND ACCOUNT ORDER, NOT CIRCUIT ORDER, SO THE MATCH
      * IS ON THE TRUNK GROUP CARRIED ON THE VOICE VARIANT.
      * READ ACCESS GRANTED UNDER THE 1991 INTERFACE AGREEMENT.
           MOVE 'P2500-MATCH-USAGE' TO WS-PARA-NAME.
           MOVE 'N' TO WS-USAGE-SW.
           MOVE ZERO TO WS-XR-MOU.
           IF WS-CDR-EOF
               MOVE WS-MSG-TEXT (6) TO WS-D1-DISPO
               GO TO P2500-EXIT.
           PERFORM P2550-SCAN-USAGE THRU P2550-EXIT
               UNTIL WS-CDR-EOF
                  OR CD-VC-TRUNK-GRP NOT < CI-TRUNK-GRP.
           IF WS-CDR-EOF
               GO TO P2500-EXIT.
           IF CD-VC-TRUNK-GRP NOT = CI-TRUNK-GRP
               MOVE WS-MSG-TEXT (6) TO WS-D1-DISPO
               GO TO P2500-EXIT.
           MOVE 'Y' TO WS-USAGE-SW.
           ADD 1 TO WS-MATCH-CNT.
           MOVE WS-MSG-TEXT (7) TO WS-D1-DISPO.

       P2500-EXIT.
           EXIT.

       P2550-SCAN-USAGE.
      * ADVANCE THE USAGE FILE AND ACCUMULATE MINUTES FOR THE TRUNK
      * GROUP BEING SETTLED.
           IF CD-VC-TRUNK-GRP = CI-TRUNK-GRP
               ADD CD-VC-CHG-MIN TO WS-XR-MOU
               ADD CD-VC-CHG-MIN TO WS-TOT-MOU.
           PERFORM P2600-READ-CDR THRU P2600-EXIT.

       P2550-EXIT.
           EXIT.

       P2600-READ-CDR.
      * READ THE BILLING APPLICATION USAGE FILE.
           READ CDR-IN-FILE INTO CABS-CDR-RECORD
               AT END
                   MOVE 'Y' TO WS-CDR-EOF-SW
                   GO TO P2600-EXIT.

       P2600-EXIT.
           EXIT.


      *****************************************************************
      * S300-OUTPUT                                                   *
      * BUILD AND WRITE THE EXTRACT RECORD.                           *
      *****************************************************************
       S300-OUTPUT SECTION.

       P3000-BUILD-EXTRACT.
      * THE EXTRACT CARRIES BOTH PERCENTAGES SO THAT THE CALCULATOR
      * DOES NOT HAVE TO READ THE CIRCUIT MASTER AGAIN.  THE TWO
      * PERCENTAGES ARE COPIED AS FILED - THEY ARE NOT VALIDATED
      * HERE.  CABSET03 VALIDATES THEM AND CABSET01 SETTLES WITH
      * WHATEVER IT IS GIVEN.
           MOVE SPACES TO WS-EXTRACT-RECORD.
           MOVE CI-CIRCUIT-ID TO WS-XR-CIRCUIT-ID.
           MOVE CI-TRUNK-GRP TO WS-XR-TRUNK-GRP.
           MOVE CI-OCN TO WS-XR-OCN.
           MOVE CI-BAN TO WS-XR-BAN.
           MOVE CI-MPB-OUR-PCT TO WS-XR-OUR-PCT.
           MOVE CI-MPB-OTHER-OCN TO WS-XR-OTHER-OCN.
           MOVE CI-MPB-OTHER-PCT TO WS-XR-OTHER-PCT.
           MOVE CI-STATE-CD TO WS-XR-STATE.
           MOVE WS-PE-SETTLE-PERIOD TO WS-XR-PERIOD.
           MOVE WS-EXTRACT-RECORD TO MPX-RECORD.
           WRITE MPX-RECORD.
           ADD WS-XR-MOU TO WS-ACC-MINUTES.

       P3000-EXIT.
           EXIT.


      *****************************************************************
      * S600-REPORT                                                   *
      * ELIGIBILITY REGISTER.                                         *
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
           MOVE CI-CIRCUIT-ID TO WS-D1-CIRCUIT.
           MOVE CI-TRUNK-GRP TO WS-D1-TRUNK.
           MOVE CI-OCN TO WS-D1-OCN.
           MOVE CI-MPB-OTHER-OCN TO WS-D1-OTHER.
           MOVE CI-MPB-OUR-PCT TO WS-D1-OURPCT.
           MOVE CI-MPB-OTHER-PCT TO WS-D1-OTHPCT.
           MOVE WS-XR-MOU TO WS-D1-MOU.
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
           MOVE CABS-CIRCUIT-RECORD TO SU-ORIG-RECORD.
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
           MOVE 210                    TO CT-STEP-SEQ.
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
           DISPLAY 'ELIGIBLE CIRCUITS' WS-ELIG-CNT.
           DISPLAY 'NOT ELIGIBLE     ' WS-NOTELIG-CNT.
           DISPLAY 'USAGE MATCHED    ' WS-MATCH-CNT.
           DISPLAY 'USAGE MINUTES    ' WS-TOT-MOU.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE CIRCUIT-MASTER
                 CDR-IN-FILE
                 MPB-EXTRACT-FILE
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

