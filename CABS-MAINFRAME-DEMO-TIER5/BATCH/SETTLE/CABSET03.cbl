      *****************************************************************
      * CABSET03 - MEET POINT PERCENTAGE VALIDATION AND VARIANCE      *
      * APPLICATION : SETL                                            *
      * INPUTS      : MPBEXT   TELCABS.SETL.MPB.EXTRACT(0)    CABSCIRC*
      * INPUTS      : SYSIN    INSTREAM                       NONE    *
      * OUTPUTS     : MPBVAL   TELCABS.SETL.MPB.VALID(+1)     CABSCIRC*
      * OUTPUTS     : REPORT   SYSOUT                         CABSPRNT*
      * CONTROL     : CTLOUT                         CABSCTL          *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED              *
      *               A CIRCUIT WHOSE PERCENTAGES DO NOT SUM TO 100 IS*
      *               STILL WRITTEN - THE VARIANCE IS REPORTED ONLY   *
      * RESTART     : FULL RERUN                                      *
      * STANDARDS   : CODED TO CABS-STD-058 AND CABS-STD-014.         *
      * REVISION HISTORY                                              *
      *   V1.00  1990-05-08  D.OKONKWO     INITIAL                    *
      *   V1.03  1993-12-02  D.OKONKWO     VARIANCE REPORT ADDED      *
      *   V2.00  1996-12-10  J.M.CASTILLO  Y2K REVIEW - NO IMPACT     *
      *   V2.02  2003-06-26  P.NAIR        TOLERANCE FROM CARD        *
      *   V2.04  2010-11-15  A.BUKOWSKI    REJECT ON GROSS VARIANCE   *
      *   V2.05  2014-08-07  L.FERREIRA    REJECT MADE A WARNING      *
      *   V2.06  2018-10-22  M.OYELARAN    RECOMPILE ONLY             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CABSET03.
       AUTHOR.        D.OKONKWO.
       DATE-WRITTEN.  1990-05-08.
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
      * ELIGIBLE MEET POINT CIRCUITS FROM CABSET02
           SELECT MPB-EXTRACT-FILE
               ASSIGN TO UT-S-MPBEXT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
      * VALIDATED CIRCUITS - INPUT TO CABSET01
           SELECT MPB-VALID-FILE
               ASSIGN TO UT-S-MPBVAL
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
       FD  MPB-EXTRACT-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 200 CHARACTERS
               DATA RECORD IS MPI-RECORD.
       01  MPI-RECORD              PIC X(200).

       FD  MPB-VALID-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 200 CHARACTERS
               DATA RECORD IS MPV-RECORD.
       01  MPV-RECORD              PIC X(200).

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
           05  WS-PGM-NAME         PIC X(08)         VALUE 'CABSET03'.
           05  WS-PGM-VERSION        PIC X(05)           VALUE 'V2.06'.
           05  WS-PGM-APPL           PIC X(04)           VALUE 'SETL'.
           05  WS-PGM-COMPILE      PIC X(08)         VALUE '20181022'.
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

       COPY CABSPRNT.

       COPY CABSSETL.

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
           05  WS-PE-TOLERANCE         PIC 9(03)V9(05).
           05  WS-PE-REJECT-SW         PIC X(01).
           05  WS-PE-FILLER            PIC X(26).
       01  WS-PARM-OLD REDEFINES WS-PARM-CARD.
      * THE 1989 CARD.  TWO REGIONS NEVER CONVERTED THEIR JCL AND
      * THE EDIT STILL ACCEPTS IT.  THE FIELDS ARE IN A DIFFERENT
      * ORDER AND THE CYCLE DATE IS SIX DIGITS YYMMDD, NOT YYDDD.
           05  FILLER                  PIC X(02).
           05  WS-PO-CYCLE             PIC 9(05).
           05  WS-PO-RUN-ID            PIC X(12).
           05  WS-PO-YYMMDD            PIC 9(06).
           05  WS-PO-TOL               PIC 9(03)V9(05).
           05  FILLER                  PIC X(20).

      * LOCAL SWITCHES.  THE STANDARD ONES ARRIVE VIA CABSWRK.
       01  WS-LOCAL-SWITCHES.
           05  WS-PARM-EOF-SW          PIC X(01)             VALUE 'N'.
                   88  WS-PARM-EOF             VALUE 'Y'.
           05  WS-VARIANCE-SW          PIC X(01)             VALUE 'N'.
                   88  WS-VARIANCE-FOUND       VALUE 'Y'.
           05  WS-GROSS-SW             PIC X(01)             VALUE 'N'.
                   88  WS-GROSS-VARIANCE       VALUE 'Y'.

      * SUBSCRIPTS AND INDEX WORK FIELDS.
       01  WS-SUBSCRIPTS.
           05  WS-SUB1                 PIC S9(05) COMP-3     VALUE 0.
           05  WS-SUB2                 PIC S9(05) COMP-3     VALUE 0.

      * EXTRACT RECORD AS BUILT BY CABSET02.  THE LAYOUT IS
      * DECLARED IN BOTH PROGRAMS AND IN CABSET01.  THERE IS NO
      * COPYBOOK FOR IT - THE 1987 CHANGE THAT WOULD HAVE ADDED
      * ONE WAS DEFERRED AND NEVER PICKED UP.
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
       01  WS-EXTRACT-ALT REDEFINES WS-EXTRACT-RECORD.
           05  WS-XA-KEY               PIC X(28).
           05  WS-XA-REST              PIC X(172).

      * PERCENTAGE VALIDATION WORK AREA.  THE TWO PERCENTAGES
      * SHOULD SUM TO EXACTLY 100.  THEY OFTEN DO NOT - THE
      * OTHER LEC FILES ITS PERCENTAGE INDEPENDENTLY AND THE TWO
      * ARE ONLY RECONCILED WHEN SOMEBODY LOOKS AT THIS REPORT.
       01  WS-VALIDATE-WORK.
           05  WS-VW-OUR-PCT           PIC S9(03)V9(05) COMP-3 VALUE 0.
           05  WS-VW-OTHER-PCT         PIC S9(03)V9(05) COMP-3 VALUE 0.
           05  WS-VW-TOTAL-PCT         PIC S9(05)V9(05) COMP-3 VALUE 0.
           05  WS-VW-VARIANCE          PIC S9(05)V9(05) COMP-3 VALUE 0.
           05  WS-VW-ABS-VAR           PIC S9(05)V9(05) COMP-3 VALUE 0.

      * VALIDATION TOTALS.
       01  WS-VALIDATE-TOTALS.
           05  WS-OK-CNT               PIC S9(09) COMP-3     VALUE 0.
           05  WS-VAR-CNT              PIC S9(09) COMP-3     VALUE 0.
           05  WS-GROSS-CNT            PIC S9(09) COMP-3     VALUE 0.
           05  WS-TOT-VARIANCE         PIC S9(09)V9(05) COMP-3 VALUE 0.
           05  WS-MAX-VARIANCE         PIC S9(05)V9(05) COMP-3 VALUE 0.

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
                   VALUE 'MEET POINT PERCENTAGE VARIANCE REPORT'.
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
           05  FILLER              PIC X(43)
                   VALUE 'CIRCUIT-ID           OCN  OTHER OUR-PCT    '.
           05  FILLER              PIC X(44)
                   VALUE 'OTHER-PCT  TOTAL      VARIANCE   DISPOSITION'.
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
           05  WS-D1-OURPCT            PIC ZZ9.99999.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-OTHPCT            PIC ZZ9.99999.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-TOTPCT            PIC ZZ9.99999.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-VAR               PIC ZZ9.99999-.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-DISPO             PIC X(20).

      * MESSAGE TEXT TABLE.  ADDRESSED BY ERROR CODE SEQUENCE, NOT
      * BY THE CODE ITSELF - INSERTING A CODE IN THE MIDDLE SHIFTS
      * EVERY MESSAGE.  THIS HAS HAPPENED TWICE (1994 AND 2007).
       01  WS-MSG-CONST.
           05  FILLER              PIC X(44)
                   VALUE 'PERCENTAGES SUM TO 100                      '.
           05  FILLER              PIC X(44)
                   VALUE 'VARIANCE WITHIN TOLERANCE                   '.
           05  FILLER              PIC X(44)
                   VALUE 'VARIANCE OUTSIDE TOLERANCE                  '.
           05  FILLER              PIC X(44)
                   VALUE 'GROSS VARIANCE - OVER TEN PERCENT           '.
           05  FILLER              PIC X(44)
                   VALUE 'OUR PERCENTAGE OUT OF RANGE                 '.
           05  FILLER              PIC X(44)
                   VALUE 'OTHER PERCENTAGE OUT OF RANGE               '.
           05  FILLER              PIC X(44)
                   VALUE 'BOTH PERCENTAGES ZERO                       '.
           05  FILLER              PIC X(44)
                   VALUE 'VARIANCE REPORTED NOT REJECTED              '.
           05  FILLER              PIC X(44)
                   VALUE 'CIRCUIT RELEASED TO SETTLEMENT              '.
           05  FILLER              PIC X(44)
                   VALUE 'END OF VALIDATION RUN                       '.
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
           OPEN INPUT  MPB-EXTRACT-FILE
                       PARM-FILE
           OPEN OUTPUT MPB-VALID-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 5301 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-MPBEXT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 5302 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-MPBVAL' TO WS-AB-TEXT
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
           MOVE ZERO TO WS-OK-CNT WS-VAR-CNT
                        WS-GROSS-CNT WS-TOT-VARIANCE
                        WS-MAX-VARIANCE.
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
           IF WS-PE-TOLERANCE NOT NUMERIC
               MOVE 000.00001 TO WS-PE-TOLERANCE.
           IF WS-PE-REJECT-SW NOT = 'Y'
               MOVE 'N' TO WS-PE-REJECT-SW.

       P1200-EXIT.
           EXIT.


      *****************************************************************
      * S200-VALIDATION                                               *
      * PERCENTAGE EDITS AND VARIANCE.                                *
      *****************************************************************
       S200-VALIDATION SECTION.

       P2000-PROCESS.
      * ONE EXTRACT RECORD PER PASS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ THRU P2100-EXIT.
           IF WS-EOF
               GO TO P2000-EXIT.
           MOVE MPI-RECORD TO WS-EXTRACT-RECORD.
           MOVE WS-XA-KEY TO WS-RESTART-KEY.
           MOVE SPACES TO WS-D1-DISPO.
           MOVE 'N' TO WS-VARIANCE-SW.
           MOVE 'N' TO WS-GROSS-SW.
           PERFORM P2200-RANGE-EDIT THRU P2200-EXIT.
           IF WS-ERROR-FOUND
               MOVE 'N' TO WS-ERROR-SW
               PERFORM P6100-DETAIL THRU P6100-EXIT
               GO TO P2000-EXIT.
           PERFORM P2300-VARIANCE THRU P2300-EXIT.
           PERFORM P3000-WRITE-VALID THRU P3000-EXIT.
           PERFORM P6100-DETAIL THRU P6100-EXIT.
           ADD 1 TO WS-WRITE-CNT.

       P2000-EXIT.
           EXIT.

       P2100-READ.
      * SEQUENTIAL READ OF THE EXTRACT.
           READ MPB-EXTRACT-FILE
               AT END
                   MOVE 'Y' TO WS-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00'
               MOVE 3530 TO WS-AB-CODE
               MOVE 'READ ERROR UT-S-MPBEXT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-READ-CNT.

       P2100-EXIT.
           EXIT.

       P2200-RANGE-EDIT.
      * EACH PERCENTAGE MUST SIT BETWEEN ZERO AND ONE HUNDRED.
           MOVE 'P2200-RANGE-EDIT' TO WS-PARA-NAME.
           MOVE WS-XR-OUR-PCT TO WS-VW-OUR-PCT.
           MOVE WS-XR-OTHER-PCT TO WS-VW-OTHER-PCT.
           IF WS-VW-OUR-PCT < 0 OR WS-VW-OUR-PCT > 100.00000
               MOVE EC-MPB-PCT-INVALID TO WS-ERR-CODE
               MOVE 'E' TO WS-ERR-SEVERITY
               MOVE WS-MSG-TEXT (5) TO WS-D1-DISPO
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               MOVE 'Y' TO WS-ERROR-SW
               GO TO P2200-EXIT.
           IF WS-VW-OTHER-PCT < 0 OR WS-VW-OTHER-PCT > 100.00000
               MOVE EC-MPB-PCT-INVALID TO WS-ERR-CODE
               MOVE 'E' TO WS-ERR-SEVERITY
               MOVE WS-MSG-TEXT (6) TO WS-D1-DISPO
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               MOVE 'Y' TO WS-ERROR-SW
               GO TO P2200-EXIT.
           IF WS-VW-OUR-PCT = ZERO AND WS-VW-OTHER-PCT = ZERO
               MOVE EC-MPB-PCT-INVALID TO WS-ERR-CODE
               MOVE 'W' TO WS-ERR-SEVERITY
               MOVE WS-MSG-TEXT (7) TO WS-D1-DISPO
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               MOVE 'Y' TO WS-ERROR-SW.

       P2200-EXIT.
           EXIT.

       P2300-VARIANCE.
      * COMPUTE THE VARIANCE FROM ONE HUNDRED.  A VARIANCE INSIDE
      * THE TOLERANCE IS ACCEPTED SILENTLY.  ANYTHING LARGER IS
      * REPORTED.  SINCE 2014 EVEN A GROSS VARIANCE IS ONLY A
      * WARNING - THE 2010 CHANGE THAT REJECTED THEM WAS BACKED OUT
      * BECAUSE IT HELD UP THE SETTLEMENT RUN EVERY MONTH.
           MOVE 'P2300-VARIANCE' TO WS-PARA-NAME.
           COMPUTE WS-VW-TOTAL-PCT =
                   WS-VW-OUR-PCT + WS-VW-OTHER-PCT.
           COMPUTE WS-VW-VARIANCE = 100.00000 - WS-VW-TOTAL-PCT.
           IF WS-VW-VARIANCE < ZERO
               COMPUTE WS-VW-ABS-VAR = WS-VW-VARIANCE * -1
           ELSE
               MOVE WS-VW-VARIANCE TO WS-VW-ABS-VAR.
           IF WS-VW-ABS-VAR = ZERO
               ADD 1 TO WS-OK-CNT
               MOVE WS-MSG-TEXT (1) TO WS-D1-DISPO
               GO TO P2300-EXIT.
           IF WS-VW-ABS-VAR NOT > WS-PE-TOLERANCE
               MOVE WS-MSG-TEXT (2) TO WS-D1-DISPO
               GO TO P2300-EXIT.
           MOVE 'Y' TO WS-VARIANCE-SW.
           ADD 1 TO WS-VAR-CNT.
           ADD WS-VW-ABS-VAR TO WS-TOT-VARIANCE.
           IF WS-VW-ABS-VAR > WS-MAX-VARIANCE
               MOVE WS-VW-ABS-VAR TO WS-MAX-VARIANCE.
           MOVE WS-MSG-TEXT (3) TO WS-D1-DISPO.
           IF WS-VW-ABS-VAR > 010.00000
               MOVE 'Y' TO WS-GROSS-SW
               ADD 1 TO WS-GROSS-CNT
               MOVE WS-MSG-TEXT (4) TO WS-D1-DISPO
               MOVE EC-MPB-PCT-INVALID TO WS-ERR-CODE
               MOVE 'W' TO WS-ERR-SEVERITY
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               SUBTRACT 1 FROM WS-REJECT-CNT.

       P2300-EXIT.
           EXIT.


      *****************************************************************
      * S300-OUTPUT                                                   *
      * RELEASE TO SETTLEMENT.                                        *
      *****************************************************************
       S300-OUTPUT SECTION.

       P3000-WRITE-VALID.
      * EVERY CIRCUIT IS RELEASED WHATEVER THE VARIANCE.  CABSET01
      * DECIDES WHAT TO DO WITH A PERCENTAGE PAIR THAT DOES NOT SUM
      * TO ONE HUNDRED.
           MOVE WS-EXTRACT-RECORD TO MPV-RECORD.
           WRITE MPV-RECORD.
           MOVE WS-MSG-TEXT (9) TO WS-D1-DISPO.

       P3000-EXIT.
           EXIT.


      *****************************************************************
      * S600-REPORT                                                   *
      * VARIANCE REPORT.                                              *
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
           MOVE WS-VW-OUR-PCT TO WS-D1-OURPCT.
           MOVE WS-VW-OTHER-PCT TO WS-D1-OTHPCT.
           MOVE WS-VW-TOTAL-PCT TO WS-D1-TOTPCT.
           MOVE WS-VW-VARIANCE TO WS-D1-VAR.
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
           MOVE 220                    TO CT-STEP-SEQ.
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
           DISPLAY 'PERCENT OK       ' WS-OK-CNT.
           DISPLAY 'VARIANCE FOUND   ' WS-VAR-CNT.
           DISPLAY 'GROSS VARIANCE   ' WS-GROSS-CNT.
           DISPLAY 'TOTAL VARIANCE   ' WS-TOT-VARIANCE.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE MPB-EXTRACT-FILE
                 MPB-VALID-FILE
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

