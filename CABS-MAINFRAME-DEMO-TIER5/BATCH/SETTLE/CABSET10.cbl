      *****************************************************************
      * CABSET10 - SETTLEMENT DISPUTE HANDLER                         *
      * APPLICATION : SETL                                            *
      * INPUTS      : DISPIN   TELCABS.SETL.DISPUTE.IN(0)     NONE    *
      * INPUTS      : SETLMAST TELCABS.SETL.SETTLE.MASTER     CABSSETL*
      * INPUTS      : SYSIN    INSTREAM                       NONE    *
      * OUTPUTS     : DISPOUT  TELCABS.SETL.DISPUTE.LOG(+1)   NONE    *
      * OUTPUTS     : REPORT   SYSOUT                         CABSPRNT*
      * CONTROL     : CTLOUT                         CABSCTL          *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED              *
      *               EVERY DISPUTE ACTION IS LOGGED                  *
      * RESTART     : FULL RERUN NOT SAFE - THE MASTER IS UPDATED IN P*
      * STANDARDS   : CODED TO CABS-STD-009 AND CABS-STD-022.         *
      * REVISION HISTORY                                              *
      *   V1.00  1994-03-15  J.M.CASTILLO  INITIAL                    *
      *   V1.03  1996-08-28  J.M.CASTILLO  PARTIAL DISPUTE ADDED      *
      *   V2.00  1998-12-07  P.NAIR        Y2K REVIEW - NO IMPACT     *
      *   V2.02  2004-04-20  P.NAIR        AGEING ADDED               *
      *   V2.04  2011-02-14  A.BUKOWSKI    AUTO RESOLVE AT 180 DAYS   *
      *   V2.06  2017-08-30  L.FERREIRA    RECOMPILE ONLY             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CABSET10.
       AUTHOR.        J.M.CASTILLO.
       DATE-WRITTEN.  1994-03-15.
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
      * DISPUTE TRANSACTIONS KEYED BY THE ACCESS MANAGEMENT GRP
           SELECT DISPUTE-IN-FILE
               ASSIGN TO UT-S-DISPIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
      * SETTLEMENT MASTER - UPDATED IN PLACE
           SELECT SETTLE-MASTER
               ASSIGN TO DA-I-SETLMAST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS STM-KEY
               FILE STATUS IS WS-FS-OUTPUT.
      * DISPUTE ACTION LOG - THE AUDIT TRAIL
           SELECT DISPUTE-LOG-FILE
               ASSIGN TO UT-S-DISPOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-TABLE.
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
       FD  DISPUTE-IN-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 120 CHARACTERS
               DATA RECORD IS DPI-RECORD.
       01  DPI-RECORD              PIC X(120).

       FD  SETTLE-MASTER
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 180 CHARACTERS
               DATA RECORD IS STM-RECORD.
       01  STM-RECORD.
           05  STM-KEY                 PIC X(20).
           05  STM-DATA                PIC X(160).

       FD  DISPUTE-LOG-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 200 CHARACTERS
               DATA RECORD IS DPO-RECORD.
       01  DPO-RECORD              PIC X(200).

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
           05  WS-PGM-NAME         PIC X(08)         VALUE 'CABSET10'.
           05  WS-PGM-VERSION        PIC X(05)           VALUE 'V2.06'.
           05  WS-PGM-APPL           PIC X(04)           VALUE 'SETL'.
           05  WS-PGM-COMPILE      PIC X(08)         VALUE '20170830'.
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

       COPY CABSPRNT.

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
           05  WS-PE-AUTO-DAYS         PIC 9(03).
           05  WS-PE-ACTION            PIC X(01).
           05  WS-PE-FILLER            PIC X(31).
       01  WS-PARM-OLD REDEFINES WS-PARM-CARD.
      * THE 1989 CARD.  TWO REGIONS NEVER CONVERTED THEIR JCL AND
      * THE EDIT STILL ACCEPTS IT.  THE FIELDS ARE IN A DIFFERENT
      * ORDER AND THE CYCLE DATE IS SIX DIGITS YYMMDD, NOT YYDDD.
           05  FILLER                  PIC X(02).
           05  WS-PO-CYCLE             PIC 9(05).
           05  WS-PO-RUN-ID            PIC X(12).
           05  WS-PO-YYMMDD            PIC 9(06).
           05  WS-PO-AUTO              PIC 9(03).
           05  FILLER                  PIC X(20).

      * LOCAL SWITCHES.  THE STANDARD ONES ARRIVE VIA CABSWRK.
       01  WS-LOCAL-SWITCHES.
           05  WS-PARM-EOF-SW          PIC X(01)             VALUE 'N'.
                   88  WS-PARM-EOF             VALUE 'Y'.
           05  WS-SETL-FOUND-SW        PIC X(01)             VALUE 'N'.
                   88  WS-SETL-FOUND           VALUE 'Y'.
           05  WS-VALID-TRAN-SW        PIC X(01)             VALUE 'Y'.
                   88  WS-VALID-TRAN           VALUE 'Y'.

      * SUBSCRIPTS AND INDEX WORK FIELDS.
       01  WS-SUBSCRIPTS.
           05  WS-SUB1                 PIC S9(05) COMP-3     VALUE 0.
           05  WS-SUB2                 PIC S9(05) COMP-3     VALUE 0.

      * THE DISPUTE TRANSACTION.  KEYED BY THE ACCESS MANAGEMENT
      * GROUP ON A 3270 SCREEN AND WRITTEN TO A FLAT FILE BY THE
      * CICS TRANSACTION CABD.  THREE REDEFINES - ONE PER ACTION
      * CODE.
       01  WS-DISPUTE-TRAN.
           05  WS-DT-ACTION          PIC X(01)           VALUE SPACES.
                   88  WS-DT-RAISE              VALUE 'R'.
                   88  WS-DT-RESOLVE            VALUE 'S'.
                   88  WS-DT-WITHDRAW           VALUE 'W'.
           05  WS-DT-BODY            PIC X(119)          VALUE SPACES.
       01  WS-DT-RAISE-R REDEFINES WS-DISPUTE-TRAN.
           05  WS-DR-ACTION            PIC X(01).
           05  WS-DR-TYPE              PIC X(01).
           05  WS-DR-OCN               PIC X(04).
           05  WS-DR-PERIOD            PIC 9(06).
           05  WS-DR-SEQ               PIC 9(09).
           05  WS-DR-AMOUNT            PIC S9(13)V9(02).
           05  WS-DR-REASON            PIC X(02).
           05  WS-DR-RAISED-YYDDD      PIC 9(05).
           05  WS-DR-TEXT              PIC X(60).
           05  WS-DR-FILLER            PIC X(17).
       01  WS-DT-RESOLVE-R REDEFINES WS-DISPUTE-TRAN.
           05  WS-DS-ACTION            PIC X(01).
           05  WS-DS-TYPE              PIC X(01).
           05  WS-DS-OCN               PIC X(04).
           05  WS-DS-PERIOD            PIC 9(06).
           05  WS-DS-SEQ               PIC 9(09).
           05  WS-DS-SETTLED-AMT       PIC S9(13)V9(02).
           05  WS-DS-OUTCOME           PIC X(01).
           05  WS-DS-RESOLVED-YYDDD    PIC 9(05).
           05  WS-DS-FILLER            PIC X(78).
       01  WS-DT-WITHDRAW-R REDEFINES WS-DISPUTE-TRAN.
           05  WS-DW-ACTION            PIC X(01).
           05  WS-DW-KEY               PIC X(20).
           05  WS-DW-FILLER            PIC X(99).

      * DISPUTE STATISTICS.
       01  WS-DISPUTE-TOTALS.
           05  WS-RAISE-CNT            PIC S9(09) COMP-3     VALUE 0.
           05  WS-RESOLVE-CNT          PIC S9(09) COMP-3     VALUE 0.
           05  WS-AUTO-CNT             PIC S9(09) COMP-3     VALUE 0.
           05  WS-WITHDRAW-CNT         PIC S9(09) COMP-3     VALUE 0.
           05  WS-TOT-DISP-AMT         PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-TOT-SETTLED-AMT      PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-VARIANCE-AMT         PIC S9(13)V9(02) COMP-3 VALUE 0.

      * THE DISPUTE LOG RECORD.
       01  WS-DISPUTE-LOG.
           05  WS-DL-RUN-ID          PIC X(12)           VALUE SPACES.
           05  WS-DL-ACTION          PIC X(01)           VALUE SPACES.
           05  WS-DL-KEY             PIC X(20)           VALUE SPACES.
           05  WS-DL-AMOUNT            PIC S9(13)V9(02)      VALUE 0.
           05  WS-DL-SETTLED           PIC S9(13)V9(02)      VALUE 0.
           05  WS-DL-OUTCOME         PIC X(01)           VALUE SPACES.
           05  WS-DL-YYDDD             PIC 9(05)             VALUE 0.
           05  WS-DL-TEXT            PIC X(60)           VALUE SPACES.
           05  WS-DL-FILLER          PIC X(69)           VALUE SPACES.

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
                   VALUE 'SETTLEMENT DISPUTE REGISTER'.
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
           05  FILLER              PIC X(47)
                   VALUE 'ACT TYP OCN  PERIOD  SEQ        DISPUTED-AMT'.
           05  FILLER              PIC X(32)
                   VALUE 'SETTLED-AMT     OUT  DISPOSITION'.
       01  WS-HEAD-4.
           05  FILLER                  PIC X(01)             VALUE ' '.
           05  FILLER                PIC X(131)          VALUE ALL '-'.

      * DETAIL LINE WS-DETAIL-1.
       01  WS-DETAIL-1.
           05  FILLER                  PIC X(01)             VALUE ' '.
           05  WS-D1-ACT               PIC X(01).
           05  FILLER                PIC X(03)           VALUE SPACES.
           05  WS-D1-TYP               PIC X(01).
           05  FILLER                PIC X(03)           VALUE SPACES.
           05  WS-D1-OCN               PIC X(04).
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-PERIOD            PIC 9(06).
           05  FILLER                PIC X(02)           VALUE SPACES.
           05  WS-D1-SEQ               PIC 9(09).
           05  FILLER                PIC X(02)           VALUE SPACES.
           05  WS-D1-DISPAMT           PIC ZZZ,ZZZ,ZZ9.99-.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-SETAMT            PIC ZZZ,ZZZ,ZZ9.99-.
           05  FILLER                PIC X(02)           VALUE SPACES.
           05  WS-D1-OUT               PIC X(01).
           05  FILLER                PIC X(02)           VALUE SPACES.
           05  WS-D1-DISPO             PIC X(20).

      * MESSAGE TEXT TABLE.  ADDRESSED BY ERROR CODE SEQUENCE, NOT
      * BY THE CODE ITSELF - INSERTING A CODE IN THE MIDDLE SHIFTS
      * EVERY MESSAGE.  THIS HAS HAPPENED TWICE (1994 AND 2007).
       01  WS-MSG-CONST.
           05  FILLER              PIC X(44)
                   VALUE 'DISPUTE RAISED                              '.
           05  FILLER              PIC X(44)
                   VALUE 'DISPUTE RESOLVED                            '.
           05  FILLER              PIC X(44)
                   VALUE 'DISPUTE WITHDRAWN                           '.
           05  FILLER              PIC X(44)
                   VALUE 'SETTLEMENT RECORD NOT FOUND                 '.
           05  FILLER              PIC X(44)
                   VALUE 'ALREADY UNDER DISPUTE                       '.
           05  FILLER              PIC X(44)
                   VALUE 'NOT UNDER DISPUTE                           '.
           05  FILLER              PIC X(44)
                   VALUE 'AUTO RESOLVED AT THE AGE LIMIT              '.
           05  FILLER              PIC X(44)
                   VALUE 'SETTLED AMOUNT DIFFERS FROM DISPUTED        '.
           05  FILLER              PIC X(44)
                   VALUE 'ACTION CODE NOT RECOGNISED                  '.
           05  FILLER              PIC X(44)
                   VALUE 'END OF DISPUTE RUN                          '.
       01  WS-MSG-TABLE REDEFINES WS-MSG-CONST.
           05  WS-MSG-ENTRY OCCURS 10 TIMES
                   INDEXED BY WS-MSG-IX.
               10  WS-MSG-TEXT             PIC X(44).

      * SETTLEMENT MASTER KEY BUILD AREA.
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
           OPEN INPUT  DISPUTE-IN-FILE
                       PARM-FILE
           OPEN OUTPUT DISPUTE-LOG-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           OPEN I-O    SETTLE-MASTER
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 6001 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-DISPIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 6002 TO WS-AB-CODE
               MOVE 'OPEN FAILED DA-I-SETLMAST' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 6003 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-DISPOUT' TO WS-AB-TEXT
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
           MOVE ZERO TO WS-RAISE-CNT WS-RESOLVE-CNT
                        WS-AUTO-CNT WS-WITHDRAW-CNT
                        WS-TOT-DISP-AMT
                        WS-TOT-SETTLED-AMT.
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
           IF WS-PE-AUTO-DAYS NOT NUMERIC
               MOVE 180 TO WS-PE-AUTO-DAYS.

       P1200-EXIT.
           EXIT.


      *****************************************************************
      * S200-DISPUTE                                                  *
      * APPLY THE DISPUTE TRANSACTIONS.                               *
      *****************************************************************
       S200-DISPUTE SECTION.

       P2000-PROCESS.
      * ONE DISPUTE TRANSACTION PER PASS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ THRU P2100-EXIT.
           IF WS-EOF
               GO TO P2000-EXIT.
           MOVE DPI-RECORD TO WS-DISPUTE-TRAN.
           MOVE SPACES TO WS-D1-DISPO.
           MOVE 'Y' TO WS-VALID-TRAN-SW.
           PERFORM P2200-READ-SETTLE THRU P2200-EXIT.
           IF NOT WS-SETL-FOUND
               MOVE WS-MSG-TEXT (4) TO WS-D1-DISPO
               MOVE EC-OUT-OF-BALANCE TO WS-ERR-CODE
               MOVE 'E' TO WS-ERR-SEVERITY
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               PERFORM P6100-DETAIL THRU P6100-EXIT
               GO TO P2000-EXIT.
           IF WS-DT-RAISE
               PERFORM P3000-RAISE THRU P3000-EXIT
               GO TO P2500-LOG.
           IF WS-DT-RESOLVE
               PERFORM P3200-RESOLVE THRU P3200-EXIT
               GO TO P2500-LOG.
           IF WS-DT-WITHDRAW
               PERFORM P3400-WITHDRAW THRU P3400-EXIT
               GO TO P2500-LOG.
           MOVE WS-MSG-TEXT (9) TO WS-D1-DISPO.
           MOVE EC-DATE-INVALID TO WS-ERR-CODE.
           MOVE 'E' TO WS-ERR-SEVERITY.
           PERFORM P7000-SUSPEND THRU P7000-EXIT.
           GO TO P2000-EXIT.

       P2000-EXIT.
           EXIT.

       P2500-LOG.
      * LOG THE ACTION AND PRINT THE REGISTER LINE.  REACHED BY GO
      * TO FROM THREE PLACES IN P2000 - THIS IS THE COMMON EXIT
      * PATH FOR EVERY SUCCESSFUL ACTION.
           PERFORM P4000-WRITE-LOG THRU P4000-EXIT.
           PERFORM P6100-DETAIL THRU P6100-EXIT.
           ADD 1 TO WS-WRITE-CNT.

       P2500-EXIT.
           EXIT.

       P2100-READ.
      * SEQUENTIAL READ OF THE TRANSACTIONS.
           READ DISPUTE-IN-FILE
               AT END
                   MOVE 'Y' TO WS-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00'
               MOVE 3600 TO WS-AB-CODE
               MOVE 'READ ERROR UT-S-DISPIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-READ-CNT.

       P2100-EXIT.
           EXIT.

       P2200-READ-SETTLE.
      * READ THE SETTLEMENT RECORD BEING DISPUTED.
           MOVE 'N' TO WS-SETL-FOUND-SW.
           MOVE SPACES TO WS-SETL-KEY.
           MOVE WS-DR-TYPE TO WS-SK-TYPE.
           MOVE WS-DR-OCN TO WS-SK-OCN.
           MOVE WS-DR-PERIOD TO WS-SK-PERIOD.
           MOVE WS-DR-SEQ TO WS-SK-SEQ.
           MOVE WS-SETL-KEY TO STM-KEY.
           READ SETTLE-MASTER
               INVALID KEY
                   GO TO P2200-EXIT.
           MOVE STM-RECORD TO CABS-SETTLEMENT-RECORD.
           MOVE 'Y' TO WS-SETL-FOUND-SW.

       P2200-EXIT.
           EXIT.


      *****************************************************************
      * S300-ACTIONS                                                  *
      * RAISE, RESOLVE AND WITHDRAW.                                  *
      *****************************************************************
       S300-ACTIONS SECTION.

       P3000-RAISE.
      * RAISE A DISPUTE.  THE SETTLEMENT RECORD IS FLAGGED AND IS
      * THEN EXCLUDED FROM NETTING UNTIL THE DISPUTE IS RESOLVED.
           MOVE 'P3000-RAISE' TO WS-PARA-NAME.
           IF ST-DISPUTE-SW = 'Y'
               MOVE WS-MSG-TEXT (5) TO WS-D1-DISPO
               GO TO P3000-EXIT.
           MOVE 'Y' TO ST-DISPUTE-SW.
           MOVE CABS-SETTLEMENT-RECORD TO STM-RECORD.
           REWRITE STM-RECORD
               INVALID KEY
                   MOVE 6010 TO WS-AB-CODE
                   MOVE 'REWRITE FAILED ON SETTLE MASTER' TO WS-AB-TEXT
                   PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-RAISE-CNT.
           ADD WS-DR-AMOUNT TO WS-TOT-DISP-AMT.
           MOVE WS-MSG-TEXT (1) TO WS-D1-DISPO.

       P3000-EXIT.
           EXIT.

       P3200-RESOLVE.
      * RESOLVE A DISPUTE.  THE SETTLED AMOUNT REPLACES THE NET DUE
      * ON THE SETTLEMENT RECORD.  WHERE IT DIFFERS FROM THE AMOUNT
      * DISPUTED THE VARIANCE IS REPORTED - IT IS THE MEASURE OF
      * HOW MUCH THE DISPUTE COST OR SAVED.
           MOVE 'P3200-RESOLVE' TO WS-PARA-NAME.
           IF ST-DISPUTE-SW NOT = 'Y'
               MOVE WS-MSG-TEXT (6) TO WS-D1-DISPO
               GO TO P3200-EXIT.
           COMPUTE WS-VARIANCE-AMT =
                   WS-DS-SETTLED-AMT - ST-NET-DUE.
           MOVE WS-DS-SETTLED-AMT TO ST-NET-DUE.
           MOVE 'N' TO ST-DISPUTE-SW.
           MOVE CABS-SETTLEMENT-RECORD TO STM-RECORD.
           REWRITE STM-RECORD
               INVALID KEY
                   MOVE 6011 TO WS-AB-CODE
                   MOVE 'REWRITE FAILED ON RESOLVE' TO WS-AB-TEXT
                   PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-RESOLVE-CNT.
           ADD WS-DS-SETTLED-AMT TO WS-TOT-SETTLED-AMT.
           MOVE WS-MSG-TEXT (2) TO WS-D1-DISPO.
           IF WS-VARIANCE-AMT NOT = ZERO
               MOVE WS-MSG-TEXT (8) TO WS-D1-DISPO.

       P3200-EXIT.
           EXIT.

       P3400-WITHDRAW.
      * WITHDRAW A DISPUTE WITHOUT SETTLING IT.  THE ORIGINAL
      * AMOUNT STANDS.
           IF ST-DISPUTE-SW NOT = 'Y'
               MOVE WS-MSG-TEXT (6) TO WS-D1-DISPO
               GO TO P3400-EXIT.
           MOVE 'N' TO ST-DISPUTE-SW.
           MOVE CABS-SETTLEMENT-RECORD TO STM-RECORD.
           REWRITE STM-RECORD
               INVALID KEY
                   MOVE 6012 TO WS-AB-CODE
                   MOVE 'REWRITE FAILED ON WITHDRAW' TO WS-AB-TEXT
                   PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-WITHDRAW-CNT.
           MOVE WS-MSG-TEXT (3) TO WS-D1-DISPO.

       P3400-EXIT.
           EXIT.


      *****************************************************************
      * S400-LOG                                                      *
      * WRITE THE DISPUTE ACTION LOG.                                 *
      *****************************************************************
       S400-LOG SECTION.

       P4000-WRITE-LOG.
      * ONE LOG RECORD PER ACTION.  THE LOG IS THE ONLY EVIDENCE OF
      * WHO DISPUTED WHAT AND WHEN - THE SETTLEMENT MASTER CARRIES
      * ONLY THE CURRENT STATE.
           MOVE SPACES TO WS-DISPUTE-LOG.
           MOVE WS-RUN-ID TO WS-DL-RUN-ID.
           MOVE WS-DT-ACTION TO WS-DL-ACTION.
           MOVE WS-SETL-KEY TO WS-DL-KEY.
           MOVE WS-DR-AMOUNT TO WS-DL-AMOUNT.
           MOVE WS-DS-SETTLED-AMT TO WS-DL-SETTLED.
           MOVE WS-DS-OUTCOME TO WS-DL-OUTCOME.
           MOVE WS-CYCLE-YYDDD TO WS-DL-YYDDD.
           MOVE WS-DR-TEXT TO WS-DL-TEXT.
           MOVE WS-DISPUTE-LOG TO DPO-RECORD.
           WRITE DPO-RECORD.

       P4000-EXIT.
           EXIT.


      *****************************************************************
      * S600-REPORT                                                   *
      * DISPUTE REGISTER.                                             *
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
      * ONE LINE PER TRANSACTION.
           IF WS-LINE-CNT > WS-MAX-LINES
               PERFORM P6000-HEADING THRU P6000-EXIT.
           MOVE WS-DT-ACTION TO WS-D1-ACT.
           MOVE WS-DR-TYPE TO WS-D1-TYP.
           MOVE WS-DR-OCN TO WS-D1-OCN.
           MOVE WS-DR-PERIOD TO WS-D1-PERIOD.
           MOVE WS-DR-SEQ TO WS-D1-SEQ.
           MOVE WS-DR-AMOUNT TO WS-D1-DISPAMT.
           MOVE WS-DS-SETTLED-AMT TO WS-D1-SETAMT.
           MOVE WS-DS-OUTCOME TO WS-D1-OUT.
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
           MOVE WS-DISPUTE-TRAN TO SU-ORIG-RECORD.
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
           MOVE 290                    TO CT-STEP-SEQ.
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
           DISPLAY 'DISPUTES RAISED  ' WS-RAISE-CNT.
           DISPLAY 'DISPUTES RESOLVED' WS-RESOLVE-CNT.
           DISPLAY 'AUTO RESOLVED    ' WS-AUTO-CNT.
           DISPLAY 'DISPUTED AMOUNT  ' WS-TOT-DISP-AMT.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE DISPUTE-IN-FILE
                 SETTLE-MASTER
                 DISPUTE-LOG-FILE
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

