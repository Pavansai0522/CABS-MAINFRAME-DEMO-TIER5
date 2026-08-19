      *****************************************************************
      * CABJUR11 - JURISDICTION EXCEPTION AND AUDIT REPORT            *
      * APPLICATION : CABS                                            *
      * INPUTS      : SUSPIN   TELCABS.CABS.SUSPENSE(0)       CABSERR *
      * INPUTS      : CTLIN    TELCABS.CABS.RUNCTL(0)         CABSCTL *
      * INPUTS      : SYSIN    INSTREAM                       NONE    *
      * OUTPUTS     : REPORT   SYSOUT                         CABSPRNT*
      * OUTPUTS     : EXCOUT   TELCABS.CABS.JURIS.EXC(+1)     CABSERR *
      * CONTROL     : CTLOUT                         CABSCTL          *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-SUMMARISED            *
      *               EVERY SUSPENSE RECORD IS EITHER REPORTED OR ROLL*
      *               INTO A SUMMARY COUNT                            *
      * RESTART     : FULL RERUN                                      *
      * STANDARDS   : CODED TO CABS-STD-041 (MONEY FIELDS) AND        *
      *               CABS-STD-063 (PRINT CONTROL). THE FILED DESIGN  *
      *               NOTE IS HELD BY THE APPLICATION OWNER.          *
      * REVISION HISTORY                                              *
      *   V1.00  1995-01-23  J.M.CASTILLO  INITIAL                    *
      *   V1.02  1997-10-09  J.M.CASTILLO  CONTROL FILE READ          *
      *   V2.00  2000-05-25  P.NAIR        Y2K CLEANUP                *
      *   V2.02  2004-03-17  P.NAIR        ERROR CODE TOTALS          *
      *   V2.04  2009-09-30  A.BUKOWSKI    OUT OF BALANCE FLAGGED     *
      *   V2.06  2016-06-13  L.FERREIRA    RECOMPILE ONLY             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CABJUR11.
       AUTHOR.        J.M.CASTILLO.
       DATE-WRITTEN.  1995-01-23.
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
      * SUSPENSE RECORDS FROM EVERY JURISDICTION STEP
           SELECT SUSPENSE-IN-FILE
               ASSIGN TO UT-S-SUSPIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
      * RUN CONTROL RECORDS - THE BALANCING EVIDENCE
           SELECT CONTROL-IN-FILE
               ASSIGN TO UT-S-CTLIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-TABLE.
      * EXCEPTIONS FOR THE ACCESS MANAGEMENT WORK QUEUE
           SELECT EXCEPT-OUT-FILE
               ASSIGN TO UT-S-EXCOUT
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
       FD  SUSPENSE-IN-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 300 CHARACTERS
               DATA RECORD IS SUI-RECORD.
       01  SUI-RECORD              PIC X(300).

       FD  CONTROL-IN-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 180 CHARACTERS
               DATA RECORD IS CTI-RECORD.
       01  CTI-RECORD              PIC X(180).

       FD  EXCEPT-OUT-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 300 CHARACTERS
               DATA RECORD IS EXO-RECORD.
       01  EXO-RECORD              PIC X(300).

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
           05  WS-PGM-NAME         PIC X(08)         VALUE 'CABJUR11'.
           05  WS-PGM-VERSION        PIC X(05)           VALUE 'V2.06'.
           05  WS-PGM-APPL           PIC X(04)           VALUE 'CABS'.
           05  WS-PGM-COMPILE      PIC X(08)         VALUE '20160613'.
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

       COPY CABSCTL.

       COPY CABSPRNT.

       COPY CABSFCTR.

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
           05  WS-PE-SEVERITY          PIC X(01).
           05  WS-PE-MAX-LINES         PIC 9(05).
           05  WS-PE-FILLER            PIC X(29).
       01  WS-PARM-OLD REDEFINES WS-PARM-CARD.
      * THE 1989 CARD.  TWO REGIONS NEVER CONVERTED THEIR JCL AND
      * THE EDIT STILL ACCEPTS IT.  THE FIELDS ARE IN A DIFFERENT
      * ORDER AND THE CYCLE DATE IS SIX DIGITS YYMMDD, NOT YYDDD.
           05  FILLER                  PIC X(02).
           05  WS-PO-CYCLE             PIC 9(05).
           05  WS-PO-RUN-ID            PIC X(12).
           05  WS-PO-YYMMDD            PIC 9(06).
           05  WS-PO-SEV               PIC X(01).
           05  FILLER                  PIC X(20).

      * LOCAL SWITCHES.  THE STANDARD ONES ARRIVE VIA CABSWRK.
       01  WS-LOCAL-SWITCHES.
           05  WS-PARM-EOF-SW          PIC X(01)             VALUE 'N'.
                   88  WS-PARM-EOF             VALUE 'Y'.
           05  WS-CTL-EOF-SW           PIC X(01)             VALUE 'N'.
                   88  WS-CTL-EOF              VALUE 'Y'.
           05  WS-REPORTABLE-SW        PIC X(01)             VALUE 'Y'.
                   88  WS-REPORTABLE           VALUE 'Y'.

      * SUBSCRIPTS AND INDEX WORK FIELDS.
       01  WS-SUBSCRIPTS.
           05  WS-SUB1                 PIC S9(05) COMP-3     VALUE 0.
           05  WS-SUB2                 PIC S9(05) COMP-3     VALUE 0.
           05  WS-SUB3                 PIC S9(05) COMP-3     VALUE 0.

      * ERROR CODE TALLY.  FIFTEEN CODES ARE DEFINED IN CABSERR
      * AND THE TALLY IS ADDRESSED BY POSITION, NOT BY CODE - SEE
      * THE WARNING ON THE MESSAGE TABLE.
       01  WS-ERROR-TALLY.
           05  WS-ET-ENTRY OCCURS 15 TIMES
                   INDEXED BY WS-ET-IX.
               10  WS-ET-CODE              PIC X(04).
               10  WS-ET-COUNT             PIC S9(09) COMP-3.
               10  WS-ET-AMOUNT            PIC S9(13)V9(05) COMP-3.
       01  WS-TALLY-CTL.
           05  WS-ET-USED              PIC S9(03) COMP-3     VALUE 0.
           05  WS-ET-HIT               PIC S9(03) COMP-3     VALUE 0.
           05  WS-ET-FOUND-SW          PIC X(01)             VALUE 'N'.
                   88  WS-ET-FOUND              VALUE 'Y'.

      * EXCEPTION COUNTS.  WS-TRUNC-AMT IS THE TRUNCATED FORM OF
      * WS-TOT-EXC-AMT.  CABJUR09 ROUNDS THE EQUIVALENT FIGURE.
       01  WS-EXC-COUNTERS.
           05  WS-EXC-CNT              PIC S9(09) COMP-3     VALUE 0.
           05  WS-OOB-CNT              PIC S9(09) COMP-3     VALUE 0.
           05  WS-FATAL-CNT            PIC S9(09) COMP-3     VALUE 0.
           05  WS-WARN-CNT             PIC S9(09) COMP-3     VALUE 0.
           05  WS-CTL-CNT              PIC S9(09) COMP-3     VALUE 0.
           05  WS-TOT-EXC-AMT          PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-TRUNC-AMT            PIC S9(13)V9(02) COMP-3 VALUE 0.

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
                   VALUE 'JURISDICTION EXCEPTION REGISTER'.
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
                   VALUE 'CODE SEV PROGRAM  PARAGRAPH'.
           05  FILLER  PIC X(18)  VALUE 'COUNT       AMOUNT'.
       01  WS-HEAD-4.
           05  FILLER                  PIC X(01)             VALUE ' '.
           05  FILLER                PIC X(131)          VALUE ALL '-'.

      * DETAIL LINE WS-DETAIL-1.
       01  WS-DETAIL-1.
           05  FILLER                  PIC X(01)             VALUE ' '.
           05  WS-D1-CODE              PIC X(04).
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-SEV               PIC X(01).
           05  FILLER                PIC X(03)           VALUE SPACES.
           05  WS-D1-PGM               PIC X(08).
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-PARA              PIC X(30).
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-COUNT             PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                PIC X(02)           VALUE SPACES.
           05  WS-D1-AMOUNT            PIC ZZZ,ZZZ,ZZ9.99-.

      * DETAIL LINE WS-DETAIL-2.
       01  WS-DETAIL-2.
           05  FILLER                  PIC X(01)             VALUE ' '.
           05  WS-D2-PROCESS           PIC X(08).
           05  FILLER                PIC X(02)           VALUE SPACES.
           05  WS-D2-READ              PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                PIC X(02)           VALUE SPACES.
           05  WS-D2-WRITTEN           PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                PIC X(02)           VALUE SPACES.
           05  WS-D2-REJECTED          PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                PIC X(02)           VALUE SPACES.
           05  WS-D2-BAL               PIC X(01).
           05  FILLER                PIC X(02)           VALUE SPACES.
           05  WS-D2-TEXT              PIC X(30).

      * MESSAGE TEXT TABLE.  ADDRESSED BY ERROR CODE SEQUENCE, NOT
      * BY THE CODE ITSELF - INSERTING A CODE IN THE MIDDLE SHIFTS
      * EVERY MESSAGE.  THIS HAS HAPPENED TWICE (1994 AND 2007).
       01  WS-MSG-CONST.
           05  FILLER              PIC X(44)
                   VALUE 'EXCEPTION REPORTED                          '.
           05  FILLER              PIC X(44)
                   VALUE 'SEVERITY BELOW THE REPORTING THRESHOLD      '.
           05  FILLER              PIC X(44)
                   VALUE 'PROCESS OUT OF BALANCE                      '.
           05  FILLER              PIC X(44)
                   VALUE 'FATAL SUSPENSE - CYCLE MUST NOT PROCEED     '.
           05  FILLER              PIC X(44)
                   VALUE 'WARNING SUSPENSE - INFORMATION ONLY         '.
           05  FILLER              PIC X(44)
                   VALUE 'ERROR CODE NOT ON THE TALLY TABLE           '.
           05  FILLER              PIC X(44)
                   VALUE 'CONTROL RECORD READ                         '.
           05  FILLER              PIC X(44)
                   VALUE 'AMOUNT TRUNCATED FOR THE REGISTER           '.
           05  FILLER              PIC X(44)
                   VALUE 'NO EXCEPTIONS THIS CYCLE                    '.
           05  FILLER              PIC X(44)
                   VALUE 'END OF EXCEPTION REGISTER                   '.
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
           OPEN INPUT  SUSPENSE-IN-FILE
                       CONTROL-IN-FILE
                       PARM-FILE
           OPEN OUTPUT EXCEPT-OUT-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 5101 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-SUSPIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 5102 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-CTLIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 5103 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-EXCOUT' TO WS-AB-TEXT
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
           MOVE ZERO TO WS-EXC-CNT WS-OOB-CNT
                        WS-FATAL-CNT WS-WARN-CNT
                        WS-CTL-CNT WS-TOT-EXC-AMT.
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
           IF WS-PE-SEVERITY NOT = 'W' AND
              WS-PE-SEVERITY NOT = 'E' AND
              WS-PE-SEVERITY NOT = 'F'
               MOVE 'W' TO WS-PE-SEVERITY.
           IF WS-PE-MAX-LINES NOT NUMERIC
               MOVE 00058 TO WS-PE-MAX-LINES.

       P1200-EXIT.
           EXIT.


      *****************************************************************
      * S200-EXCEPTIONS                                               *
      * READ AND TALLY THE SUSPENSE FILE.                             *
      *****************************************************************
       S200-EXCEPTIONS SECTION.

       P2000-PROCESS.
      * ONE SUSPENSE RECORD PER PASS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ THRU P2100-EXIT.
           IF WS-EOF
               GO TO P2000-EXIT.
           MOVE SUI-RECORD TO CABS-SUSPENSE-RECORD.
           MOVE SU-ERR-CODE TO WS-RESTART-KEY.
           PERFORM P2200-SEVERITY-TEST THRU P2200-EXIT.
           IF NOT WS-REPORTABLE
               ADD 1 TO WS-SUMM-CNT
               GO TO P2000-EXIT.
           PERFORM P2300-TALLY THRU P2300-EXIT.
           PERFORM P3000-WRITE-EXCEPTION THRU P3000-EXIT.
           PERFORM P6100-DETAIL THRU P6100-EXIT.
           ADD 1 TO WS-WRITE-CNT.
           ADD 1 TO WS-EXC-CNT.

       P2000-EXIT.
           EXIT.

       P2100-READ.
      * SEQUENTIAL READ OF THE SUSPENSE FILE.
           READ SUSPENSE-IN-FILE
               AT END
                   MOVE 'Y' TO WS-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00'
               MOVE 3510 TO WS-AB-CODE
               MOVE 'READ ERROR UT-S-SUSPIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-READ-CNT.

       P2100-EXIT.
           EXIT.

       P2200-SEVERITY-TEST.
      * ONLY SUSPENSE AT OR ABOVE THE REQUESTED SEVERITY IS
      * REPORTED.  THE THRESHOLD ARRIVES ON THE CONTROL CARD.
           MOVE 'Y' TO WS-REPORTABLE-SW.
           IF SU-FATAL
               ADD 1 TO WS-FATAL-CNT
               GO TO P2200-EXIT.
           IF SU-WARN
               ADD 1 TO WS-WARN-CNT.
           IF WS-PE-SEVERITY = 'F' AND NOT SU-FATAL
               MOVE 'N' TO WS-REPORTABLE-SW.
           IF WS-PE-SEVERITY = 'E' AND SU-WARN
               MOVE 'N' TO WS-REPORTABLE-SW.

       P2200-EXIT.
           EXIT.

       P2300-TALLY.
      * TALLY BY ERROR CODE.  THE TABLE IS BUILT AS CODES ARE MET.
           MOVE 'N' TO WS-ET-FOUND-SW.
           MOVE 1 TO WS-SUB1.
           PERFORM P2350-TALLY-FIND THRU P2350-EXIT
               UNTIL WS-SUB1 > WS-ET-USED
                  OR WS-ET-FOUND.
           IF NOT WS-ET-FOUND
               IF WS-ET-USED < 15
                   ADD 1 TO WS-ET-USED
                   MOVE WS-ET-USED TO WS-ET-HIT
                   SET WS-ET-IX TO WS-ET-HIT
                   MOVE SU-ERR-CODE TO WS-ET-CODE (WS-ET-IX)
                   MOVE ZERO TO WS-ET-COUNT (WS-ET-IX)
                   MOVE ZERO TO WS-ET-AMOUNT (WS-ET-IX)
                   MOVE 'Y' TO WS-ET-FOUND-SW
               ELSE
                   GO TO P2300-EXIT.
           SET WS-ET-IX TO WS-ET-HIT.
           ADD 1 TO WS-ET-COUNT (WS-ET-IX).

       P2300-EXIT.
           EXIT.

       P2350-TALLY-FIND.
      * ONE TALLY COMPARE.
           IF WS-ET-CODE (WS-SUB1) = SU-ERR-CODE
               MOVE 'Y' TO WS-ET-FOUND-SW
               MOVE WS-SUB1 TO WS-ET-HIT
               GO TO P2350-EXIT.
           ADD 1 TO WS-SUB1.

       P2350-EXIT.
           EXIT.

       P2600-READ-CTL.
      * READ THE RUN CONTROL FILE AND FLAG ANY PROCESS THAT DID NOT
      * BALANCE.  AN OUT OF BALANCE PROCESS STOPS THE CYCLE.
           READ CONTROL-IN-FILE
               AT END
                   MOVE 'Y' TO WS-CTL-EOF-SW
                   GO TO P2600-EXIT.
           MOVE CTI-RECORD TO CABS-CONTROL-RECORD.
           ADD 1 TO WS-CTL-CNT.
           IF CT-OUT-OF-BAL
               ADD 1 TO WS-OOB-CNT
               PERFORM P6200-CTL-DETAIL THRU P6200-EXIT.

       P2600-EXIT.
           EXIT.


      *****************************************************************
      * S300-OUTPUT                                                   *
      * WRITE THE EXCEPTION WORK QUEUE FILE.                          *
      *****************************************************************
       S300-OUTPUT SECTION.

       P3000-WRITE-EXCEPTION.
      * THE EXCEPTION FILE FEEDS THE ACCESS MANAGEMENT WORK QUEUE.
      * IT IS THE SAME LAYOUT AS THE SUSPENSE RECORD.
           MOVE CABS-SUSPENSE-RECORD TO EXO-RECORD.
           WRITE EXO-RECORD.

       P3000-EXIT.
           EXIT.


      *****************************************************************
      * S600-REPORT                                                   *
      * THE EXCEPTION REGISTER.                                       *
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
      * ONE LINE PER EXCEPTION.  THE AMOUNT IS TRUNCATED TO TWO
      * PLACES.  CABJUR09 ROUNDS THE SAME FIGURE.
           IF WS-LINE-CNT > WS-MAX-LINES
               PERFORM P6000-HEADING THRU P6000-EXIT.
           MOVE SU-ERR-CODE TO WS-D1-CODE.
           MOVE SU-ERR-SEVERITY TO WS-D1-SEV.
           MOVE SU-DETECT-PGM TO WS-D1-PGM.
           MOVE SU-DETECT-PARA TO WS-D1-PARA.
           SET WS-ET-IX TO WS-ET-HIT.
           MOVE WS-ET-COUNT (WS-ET-IX) TO WS-D1-COUNT.
           COMPUTE WS-TRUNC-AMT = WS-TOT-EXC-AMT.
           MOVE WS-TRUNC-AMT TO WS-D1-AMOUNT.
           WRITE PRT-RECORD FROM WS-DETAIL-1 AFTER ADVANCING 1 LINES.
           ADD 1 TO WS-LINE-CNT.

       P6100-EXIT.
           EXIT.

       P6200-CTL-DETAIL.
      * ONE LINE PER OUT OF BALANCE PROCESS.
           IF WS-LINE-CNT > WS-MAX-LINES
               PERFORM P6000-HEADING THRU P6000-EXIT.
           MOVE CT-PROCESS-ID TO WS-D2-PROCESS.
           MOVE CT-READ TO WS-D2-READ.
           MOVE CT-WRITTEN TO WS-D2-WRITTEN.
           MOVE CT-REJECTED TO WS-D2-REJECTED.
           MOVE CT-BAL-IND TO WS-D2-BAL.
           MOVE WS-MSG-TEXT (3) TO WS-D2-TEXT.
           WRITE PRT-RECORD FROM WS-DETAIL-2 AFTER ADVANCING 1 LINES.
           ADD 1 TO WS-LINE-CNT.

       P6200-EXIT.
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
           MOVE CABS-SUSPENSE-RECORD TO SU-ORIG-RECORD.
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
           MOVE 110                    TO CT-STEP-SEQ.
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
           PERFORM P2600-READ-CTL THRU P2600-EXIT UNTIL WS-CTL-EOF.
           DISPLAY 'EXCEPTIONS       ' WS-EXC-CNT.
           DISPLAY 'OUT OF BALANCE   ' WS-OOB-CNT.
           DISPLAY 'FATAL SUSPENSE   ' WS-FATAL-CNT.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE SUSPENSE-IN-FILE
                 CONTROL-IN-FILE
                 EXCEPT-OUT-FILE
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

