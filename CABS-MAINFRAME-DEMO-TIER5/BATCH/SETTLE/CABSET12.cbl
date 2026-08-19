      *****************************************************************
      * CABSET12 - SETTLEMENT PERIOD CLOSE                            *
      * APPLICATION : SETL                                            *
      * COMPILER    : ENTERPRISE COBOL                                *
      * INPUTS      : NETIN    TELCABS.SETL.NET(0)            CABSSETL*
      * INPUTS      : SYSIN    INSTREAM                       NONE    *
      * OUTPUTS     : CLOSEMST TELCABS.SETL.CLOSE             NONE    *
      * OUTPUTS     : DB2      SETLPERIOD TABLE (DSNSETL)     DCLGEN  *
      * CONTROL     : CTLOUT                         CABSCTL          *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED              *
      *               THE PERIOD IS CLOSED IN DB2 AND IN VSAM AND THE *
      *               TWO MUST AGREE AFTERWARDS                       *
      * RESTART     : FULL RERUN NOT SAFE - SEE P4000                 *
      * COMPILED WITH ENTERPRISE COBOL AND THE DB2                    *
      * PRECOMPILER.  SCOPE TERMINATORS PERMITTED HERE.               *
      * STANDARDS   : CODED TO CABS-STD-014 (RECORD LAYOUTS) AND      *
      *               CABS-STD-058 (DATE HANDLING).                   *
      * REVISION HISTORY                                              *
      *   V1.00  1999-10-04  P.NAIR        INITIAL                    *
      *   V1.02  2001-07-23  P.NAIR        DB2 PERIOD TABLE ADDED     *
      *   V1.05  2005-11-08  P.NAIR        VSAM CLOSE FILE ADDED      *
      *   V2.00  2010-03-15  A.BUKOWSKI    REOPEN LOGIC ADDED         *
      *   V2.02  2014-06-02  L.FERREIRA    CLOSE DATE PIVOT 70        *
      *   V2.04  2019-08-19  M.OYELARAN    RECOMPILE ONLY             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CABSET12.
       AUTHOR.        P.NAIR.
       DATE-WRITTEN.  1999-10-04.
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
      * NET POSITIONS FOR THE PERIOD BEING CLOSED
           SELECT NET-IN-FILE
               ASSIGN TO UT-S-NETIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
      * PERIOD CLOSE KSDS - THE SECOND STORE
           SELECT CLOSE-MASTER
               ASSIGN TO DA-I-CLOSEMST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CLM-KEY
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
       FD  NET-IN-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 180 CHARACTERS
               DATA RECORD IS NTI-RECORD.
       01  NTI-RECORD              PIC X(180).

       FD  CLOSE-MASTER
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 200 CHARACTERS
               DATA RECORD IS CLM-RECORD.
       01  CLM-RECORD.
           05  CLM-KEY                 PIC X(10).
           05  CLM-DATA                PIC X(190).

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
           05  WS-PGM-NAME         PIC X(08)         VALUE 'CABSET12'.
           05  WS-PGM-VERSION        PIC X(05)           VALUE 'V2.04'.
           05  WS-PGM-APPL           PIC X(04)           VALUE 'SETL'.
           05  WS-PGM-COMPILE      PIC X(08)         VALUE '20190819'.
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

      * DB2 COMMUNICATION AREA.  THIS MODULE IS COMPILED WITH THE
      * ENTERPRISE COBOL COMPILER AND THE DB2 PRECOMPILER; SCOPE
      * TERMINATORS ARE PERMITTED HERE AND NOWHERE ELSE.
       EXEC SQL INCLUDE SQLCA END-EXEC.

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
           05  WS-PE-CLOSE-PERIOD      PIC 9(06).
           05  WS-PE-REOPEN-SW         PIC X(01).
           05  WS-PE-LAG-DAYS          PIC 9(03).
           05  WS-PE-FILLER            PIC X(25).
       01  WS-PARM-OLD REDEFINES WS-PARM-CARD.
      * THE 1989 CARD.  TWO REGIONS NEVER CONVERTED THEIR JCL AND
      * THE EDIT STILL ACCEPTS IT.  THE FIELDS ARE IN A DIFFERENT
      * ORDER AND THE CYCLE DATE IS SIX DIGITS YYMMDD, NOT YYDDD.
           05  FILLER                  PIC X(02).
           05  WS-PO-CYCLE             PIC 9(05).
           05  WS-PO-RUN-ID            PIC X(12).
           05  WS-PO-YYMMDD            PIC 9(06).
           05  WS-PO-CLOSE             PIC 9(06).
           05  FILLER                  PIC X(20).

      * LOCAL SWITCHES.  THE STANDARD ONES ARRIVE VIA CABSWRK.
       01  WS-LOCAL-SWITCHES.
           05  WS-PARM-EOF-SW          PIC X(01)             VALUE 'N'.
                   88  WS-PARM-EOF             VALUE 'Y'.
           05  WS-SQL-OK-SW            PIC X(01)             VALUE 'Y'.
                   88  WS-SQL-OK               VALUE 'Y'.
                   88  WS-SQL-BAD              VALUE 'N'.
           05  WS-VSAM-FOUND-SW        PIC X(01)             VALUE 'N'.
                   88  WS-VSAM-FOUND           VALUE 'Y'.
           05  WS-REOPEN-SW            PIC X(01)             VALUE 'N'.
                   88  WS-REOPENING            VALUE 'Y'.

      * SUBSCRIPTS AND INDEX WORK FIELDS.
       01  WS-SUBSCRIPTS.
           05  WS-SUB1                 PIC S9(05) COMP-3     VALUE 0.
           05  WS-SUB2                 PIC S9(05) COMP-3     VALUE 0.

      * DB2 HOST VARIABLES FOR THE SETLPERIOD TABLE.
       01  WS-HOST-VARIABLES.
           05  WS-HV-PERIOD            PIC S9(09) COMP.
           05  WS-HV-OCN               PIC X(04).
           05  WS-HV-RECV              PIC S9(13)V9(02) COMP-3.
           05  WS-HV-PAY               PIC S9(13)V9(02) COMP-3.
           05  WS-HV-NET               PIC S9(13)V9(02) COMP-3.
           05  WS-HV-CLOSE-DATE        PIC X(10).
           05  WS-HV-STATUS            PIC X(01).
           05  WS-HV-RUN-ID            PIC X(12).
           05  WS-HV-ROWCOUNT          PIC S9(09) COMP.

      * THE NET POSITION RECORD.  THIRD DECLARATION OF THE SAME
      * LAYOUT - CABSET09, CABSET11 AND HERE.
       01  WS-NET-IN.
           05  WS-NI-OCN             PIC X(04)           VALUE SPACES.
           05  WS-NI-PERIOD            PIC 9(06)             VALUE 0.
           05  WS-NI-RECV              PIC S9(13)V9(02)      VALUE 0.
           05  WS-NI-PAY               PIC S9(13)V9(02)      VALUE 0.
           05  WS-NI-NET               PIC S9(13)V9(02)      VALUE 0.
           05  WS-NI-DIRECTION       PIC X(01)           VALUE SPACES.
           05  WS-NI-TERMS             PIC 9(03)             VALUE 0.
           05  WS-NI-DUE-YYDDD         PIC 9(05)             VALUE 0.
           05  WS-NI-FILLER          PIC X(106)          VALUE SPACES.
       01  WS-NET-IN-K REDEFINES WS-NET-IN.
           05  WS-NIK-KEY              PIC X(10).
           05  WS-NIK-REST             PIC X(170).
       01  WS-CLOSE-RECORD.
           05  WS-CR-OCN             PIC X(04)           VALUE SPACES.
           05  WS-CR-PERIOD            PIC 9(06)             VALUE 0.
           05  WS-CR-RECV              PIC S9(13)V9(02)      VALUE 0.
           05  WS-CR-PAY               PIC S9(13)V9(02)      VALUE 0.
           05  WS-CR-NET               PIC S9(13)V9(02)      VALUE 0.
           05  WS-CR-STATUS          PIC X(01)           VALUE SPACES.
           05  WS-CR-CLOSE-YYDDD       PIC 9(05)             VALUE 0.
           05  WS-CR-RUN-ID          PIC X(12)           VALUE SPACES.
           05  WS-CR-FILLER          PIC X(122)          VALUE SPACES.
       01  WS-CLOSE-KEY-R REDEFINES WS-CLOSE-RECORD.
           05  WS-CK-KEY               PIC X(10).
           05  WS-CK-REST              PIC X(190).

      * CLOSE COUNTERS.
       01  WS-CLOSE-TOTALS.
           05  WS-CLOSE-CNT            PIC S9(09) COMP-3     VALUE 0.
           05  WS-SQL-CNT              PIC S9(09) COMP-3     VALUE 0.
           05  WS-VSAM-CNT             PIC S9(09) COMP-3     VALUE 0.
           05  WS-TOT-CLOSE-AMT        PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-COMMIT-CNT           PIC S9(09) COMP-3     VALUE 0.
           05  WS-SINCE-COMMIT         PIC S9(09) COMP-3     VALUE 0.

      * JULIAN DATE WORK AREA - LOCAL TO CABSET12.  THE SHARED AREA IN
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

      * MESSAGE TEXT TABLE.  ADDRESSED BY ERROR CODE SEQUENCE, NOT
      * BY THE CODE ITSELF - INSERTING A CODE IN THE MIDDLE SHIFTS
      * EVERY MESSAGE.  THIS HAS HAPPENED TWICE (1994 AND 2007).
       01  WS-MSG-CONST.
           05  FILLER              PIC X(44)
                   VALUE 'PERIOD CLOSED FOR THIS CARRIER              '.
           05  FILLER              PIC X(44)
                   VALUE 'PERIOD REOPENED                             '.
           05  FILLER              PIC X(44)
                   VALUE 'ALREADY CLOSED - SKIPPED                    '.
           05  FILLER              PIC X(44)
                   VALUE 'DB2 ROW INSERTED                            '.
           05  FILLER              PIC X(44)
                   VALUE 'DB2 ROW UPDATED                             '.
           05  FILLER              PIC X(44)
                   VALUE 'VSAM CLOSE RECORD WRITTEN                   '.
           05  FILLER              PIC X(44)
                   VALUE 'VSAM CLOSE RECORD REWRITTEN                 '.
           05  FILLER              PIC X(44)
                   VALUE 'DB2 AND VSAM MAY NOW DISAGREE               '.
           05  FILLER              PIC X(44)
                   VALUE 'CLOSE DATE DERIVED WITH PIVOT 70            '.
           05  FILLER              PIC X(44)
                   VALUE 'END OF PERIOD CLOSE                         '.
       01  WS-MSG-TABLE REDEFINES WS-MSG-CONST.
           05  WS-MSG-ENTRY OCCURS 10 TIMES
                   INDEXED BY WS-MSG-IX.
               10  WS-MSG-TEXT             PIC X(44).

      * THE HOST DATE IN ISO FORM AND THE CLOSE DATE IN YYDDD.
       01  WS-HOST-DATE.
           05  WS-HD-CCYY              PIC 9(04)             VALUE 0.
           05  FILLER                  PIC X(01)             VALUE '-'.
           05  WS-HD-MM                PIC 9(02)             VALUE 0.
           05  FILLER                  PIC X(01)             VALUE '-'.
           05  WS-HD-DD                PIC 9(02)             VALUE 0.
       01  WS-CLOSE-DATE-AREA.
           05  WS-CLOSE-YYDDD          PIC 9(05)             VALUE 0.

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
           OPEN INPUT  NET-IN-FILE
                       PARM-FILE
           OPEN OUTPUT CONTROL-FILE
                       SUSPENSE-FILE
           OPEN I-O    CLOSE-MASTER
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 6201 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-NETIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 6202 TO WS-AB-CODE
               MOVE 'OPEN FAILED DA-I-CLOSEMST' TO WS-AB-TEXT
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
           MOVE ZERO TO WS-CLOSE-CNT WS-SQL-CNT
                        WS-VSAM-CNT WS-TOT-CLOSE-AMT
                        WS-COMMIT-CNT WS-SINCE-COMMIT.
           MOVE WS-RUN-ID TO WS-HV-RUN-ID.
           PERFORM P1400-CLOSE-DATE THRU P1400-EXIT.
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
           IF WS-PE-CLOSE-PERIOD NOT NUMERIC
               MOVE 6210 TO WS-AB-CODE
               MOVE 'CLOSE PERIOD NOT SUPPLIED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PE-REOPEN-SW NOT = 'Y'
               MOVE 'N' TO WS-PE-REOPEN-SW.
           IF WS-PE-LAG-DAYS NOT NUMERIC
               MOVE 015 TO WS-PE-LAG-DAYS.

       P1200-EXIT.
           EXIT.

       P1400-CLOSE-DATE.
      * THE CLOSE DATE IS THE CYCLE DATE PLUS THE LAG DAYS.  IT IS
      * HELD AS A YYDDD AND IS EXPANDED TO A FULL DATE FOR DB2 WITH
      * THE PIVOT OF 70.  DB2 STORES A REAL DATE; THE VSAM FILE
      * STORES THE YYDDD.  THE TWO REPRESENTATIONS OF THE SAME DAY
      * HAVE DRIFTED APART TWICE, BOTH TIMES IN JANUARY.
      * CENTURY WINDOW CONFIRMED BY THE 1996 Y2K REMEDIATION.
           MOVE 'P1400-CLOSE-DATE' TO WS-PARA-NAME.
           MOVE WS-CYCLE-YYDDD TO WS-JW-HOLD.
           MOVE WS-PE-LAG-DAYS TO WS-JW-SPAN-DAYS.
           PERFORM P5500-ADD-DAYS THRU P5500-EXIT.
           MOVE WS-JW-HOLD TO WS-CLOSE-YYDDD.
           MOVE WS-CLOSE-YYDDD TO WS-JW-TEST.
           IF WS-JW-TEST-YY < 70
               COMPUTE WS-JW-CCYY = 2000 + WS-JW-TEST-YY
           ELSE
               COMPUTE WS-JW-CCYY = 1900 + WS-JW-TEST-YY
           END-IF.
           PERFORM P5700-YYDDD-TO-GREG THRU P5700-EXIT.
           MOVE DW-GR-CCYY TO WS-HD-CCYY.
           MOVE DW-GR-MM TO WS-HD-MM.
           MOVE DW-GR-DD TO WS-HD-DD.
           MOVE WS-HOST-DATE TO WS-HV-CLOSE-DATE.
           DISPLAY 'CLOSE DATE YYDDD ' WS-CLOSE-YYDDD
                   ' GREGORIAN ' WS-HV-CLOSE-DATE.

       P1400-EXIT.
           EXIT.


      *****************************************************************
      * S200-CLOSE                                                    *
      * CLOSE EACH COUNTERPARTY POSITION.                             *
      *****************************************************************
       S200-CLOSE SECTION.

       P2000-PROCESS.
      * ONE NET POSITION PER PASS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ THRU P2100-EXIT.
           IF WS-EOF
               GO TO P2000-EXIT.
           MOVE NTI-RECORD TO WS-NET-IN.
           MOVE WS-NIK-KEY TO WS-RESTART-KEY.
           IF WS-NI-PERIOD NOT = WS-PE-CLOSE-PERIOD
               ADD 1 TO WS-REJECT-CNT
               GO TO P2000-EXIT
           END-IF.
           PERFORM P3000-BUILD-HOST THRU P3000-EXIT.
           PERFORM P3500-CLOSE-DB2 THRU P3500-EXIT.
           IF WS-SQL-BAD
               GO TO P2000-EXIT
           END-IF.
           PERFORM P4000-CLOSE-VSAM THRU P4000-EXIT.
           PERFORM P4500-COMMIT-CHECK THRU P4500-EXIT.
           ADD 1 TO WS-WRITE-CNT.
           ADD 1 TO WS-CLOSE-CNT.
           ADD WS-NI-NET TO WS-TOT-CLOSE-AMT.
           ADD WS-NI-NET TO WS-ACC-AMOUNT.

       P2000-EXIT.
           EXIT.

       P2100-READ.
      * SEQUENTIAL READ OF THE NET FILE.
           READ NET-IN-FILE
               AT END
                   MOVE 'Y' TO WS-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00'
               MOVE 3620 TO WS-AB-CODE
               MOVE 'READ ERROR UT-S-NETIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-READ-CNT.

       P2100-EXIT.
           EXIT.

       P3000-BUILD-HOST.
      * MOVE TO THE HOST VARIABLES.
           MOVE WS-NI-PERIOD TO WS-HV-PERIOD.
           MOVE WS-NI-OCN TO WS-HV-OCN.
           MOVE WS-NI-RECV TO WS-HV-RECV.
           MOVE WS-NI-PAY TO WS-HV-PAY.
           MOVE WS-NI-NET TO WS-HV-NET.
           IF WS-PE-REOPEN-SW = 'Y'
               MOVE 'O' TO WS-HV-STATUS
           ELSE
               MOVE 'C' TO WS-HV-STATUS
           END-IF.

       P3000-EXIT.
           EXIT.

       P3500-CLOSE-DB2.
      * UPDATE THE PERIOD STATUS IN DB2.  THIS IS THE FIRST OF THE
      * TWO STORES.
           MOVE 'P3500-CLOSE-DB2' TO WS-PARA-NAME.
           MOVE 'Y' TO WS-SQL-OK-SW.
           EXEC SQL
               UPDATE SETLPERIOD
                  SET PERIOD_STATUS = :WS-HV-STATUS,
                      CLOSE_DATE = :WS-HV-CLOSE-DATE,
                      NET_AMOUNT = :WS-HV-NET,
                      RUN_ID = :WS-HV-RUN-ID
                WHERE OCN = :WS-HV-OCN
                  AND SETTLE_PERIOD = :WS-HV-PERIOD
           END-EXEC.
           IF SQLCODE = 0
               ADD 1 TO WS-SQL-CNT
           ELSE
               IF SQLCODE = 100
                   PERFORM P3600-INSERT-DB2 THRU P3600-EXIT
               ELSE
                   MOVE 6220 TO WS-AB-CODE
                   MOVE 'SQL ERROR ON SETLPERIOD' TO WS-AB-TEXT
                   DISPLAY 'SQLCODE ' SQLCODE
                   PERFORM P9500-ABEND THRU P9500-EXIT
               END-IF
           END-IF.

       P3500-EXIT.
           EXIT.

       P3600-INSERT-DB2.
      * NO ROW EXISTS FOR THIS CARRIER AND PERIOD.  INSERT ONE.
           EXEC SQL
               INSERT INTO SETLPERIOD
                     (OCN, SETTLE_PERIOD, RECEIVABLE, PAYABLE,
                      NET_AMOUNT, PERIOD_STATUS, CLOSE_DATE,
                      RUN_ID)
               VALUES (:WS-HV-OCN, :WS-HV-PERIOD, :WS-HV-RECV,
                       :WS-HV-PAY, :WS-HV-NET, :WS-HV-STATUS,
                       :WS-HV-CLOSE-DATE, :WS-HV-RUN-ID)
           END-EXEC.
           IF SQLCODE = 0
               ADD 1 TO WS-SQL-CNT
           ELSE
               MOVE 6221 TO WS-AB-CODE
               MOVE 'SQL ERROR ON INSERT SETLPERIOD' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT
           END-IF.

       P3600-EXIT.
           EXIT.

       P4000-CLOSE-VSAM.
      * WRITE THE SAME CLOSE INTO THE VSAM CLOSE FILE.  THIS IS THE
      * SECOND STORE AND IT IS NOT COORDINATED WITH THE FIRST.  THE
      * DB2 UPDATE ABOVE IS INSIDE A UNIT OF WORK THAT COMMITS ON A
      * COUNT; THE VSAM WRITE BELOW IS HARDENED IMMEDIATELY.  AN
      * ABEND BETWEEN THEM LEAVES THE PERIOD CLOSED IN ONE PLACE
      * AND OPEN IN THE OTHER, AND THE ONLY WAY TO FIND OUT WHICH
      * IS TO COMPARE THEM BY HAND.
           MOVE 'P4000-CLOSE-VSAM' TO WS-PARA-NAME.
           MOVE SPACES TO WS-CLOSE-RECORD.
           MOVE WS-NI-OCN TO WS-CR-OCN.
           MOVE WS-NI-PERIOD TO WS-CR-PERIOD.
           MOVE WS-NI-RECV TO WS-CR-RECV.
           MOVE WS-NI-PAY TO WS-CR-PAY.
           MOVE WS-NI-NET TO WS-CR-NET.
           MOVE WS-HV-STATUS TO WS-CR-STATUS.
           MOVE WS-CLOSE-YYDDD TO WS-CR-CLOSE-YYDDD.
           MOVE WS-RUN-ID TO WS-CR-RUN-ID.
           MOVE WS-CK-KEY TO CLM-KEY.
           MOVE 'N' TO WS-VSAM-FOUND-SW.
           READ CLOSE-MASTER
               INVALID KEY
                   CONTINUE
           END-READ.
           IF WS-FS-OUTPUT = '00'
               MOVE 'Y' TO WS-VSAM-FOUND-SW
           END-IF.
           MOVE WS-CLOSE-RECORD TO CLM-RECORD.
           MOVE WS-CK-KEY TO CLM-KEY.
           IF WS-VSAM-FOUND
               REWRITE CLM-RECORD
                   INVALID KEY
                       MOVE 6230 TO WS-AB-CODE
                       MOVE 'REWRITE FAILED ON CLOSE KSDS'
                            TO WS-AB-TEXT
                       PERFORM P9500-ABEND THRU P9500-EXIT
               END-REWRITE
           ELSE
               WRITE CLM-RECORD
                   INVALID KEY
                       MOVE 6231 TO WS-AB-CODE
                       MOVE 'WRITE FAILED ON CLOSE KSDS'
                            TO WS-AB-TEXT
                       PERFORM P9500-ABEND THRU P9500-EXIT
               END-WRITE
           END-IF.
           ADD 1 TO WS-VSAM-CNT.

       P4000-EXIT.
           EXIT.

       P4500-COMMIT-CHECK.
      * COMMIT THE DB2 WORK EVERY HUNDRED CARRIERS.  THE VSAM
      * WRITES ARE NOT PART OF THE COMMIT SCOPE.
           ADD 1 TO WS-SINCE-COMMIT.
           IF WS-SINCE-COMMIT < 100
               GO TO P4500-EXIT
           END-IF.
           EXEC SQL
               COMMIT
           END-EXEC.
           ADD 1 TO WS-COMMIT-CNT.
           MOVE ZERO TO WS-SINCE-COMMIT.

       P4500-EXIT.
           EXIT.


      *****************************************************************
      * S550-DATE-ROUTINES                                            *
      * JULIAN SUPPORT.                                               *
      *****************************************************************
       S550-DATE-ROUTINES SECTION.

       P5500-ADD-DAYS.
      * ADD DAYS TO A YYDDD WITH YEAR ROLLOVER.
           MOVE WS-JW-HOLD TO WS-JW-TEST.
           PERFORM P5600-LEAP-YEAR THRU P5600-EXIT.
           COMPUTE WS-JW-TEST-DDD =
                   WS-JW-TEST-DDD + WS-JW-SPAN-DAYS.
           PERFORM P5550-ROLL-YEAR THRU P5550-EXIT
               UNTIL WS-JW-TEST-DDD NOT > WS-JW-DAYS-IN-YR.
           MOVE WS-JW-TEST TO WS-JW-HOLD.

       P5500-EXIT.
           EXIT.

       P5550-ROLL-YEAR.
      * ROLL ONE YEAR FORWARD.
           SUBTRACT WS-JW-DAYS-IN-YR FROM WS-JW-TEST-DDD.
           ADD 1 TO WS-JW-TEST-YY.
           IF WS-JW-TEST-YY > 99
               MOVE ZERO TO WS-JW-TEST-YY
           END-IF.
           PERFORM P5600-LEAP-YEAR THRU P5600-EXIT.

       P5550-EXIT.
           EXIT.

       P5600-LEAP-YEAR.
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

       P5600-EXIT.
           EXIT.

       P5700-YYDDD-TO-GREG.
      * CONVERT THE CLOSE DATE TO A GREGORIAN DATE FOR DB2.
           PERFORM P5600-LEAP-YEAR THRU P5600-EXIT.
           IF WS-JW-LEAP
               MOVE 2 TO WS-SUB1
           ELSE
               MOVE 1 TO WS-SUB1
           END-IF.
           MOVE 12 TO WS-SUB2.
           PERFORM P5750-FIND-MONTH THRU P5750-EXIT
               UNTIL WS-SUB2 < 1
                  OR WS-MT-DAYS-BEFORE (WS-SUB1, WS-SUB2)
                     < WS-JW-TEST-DDD.
           MOVE WS-JW-CCYY TO DW-GR-CCYY.
           MOVE WS-SUB2 TO DW-GR-MM.
           COMPUTE DW-GR-DD = WS-JW-TEST-DDD
                 - WS-MT-DAYS-BEFORE (WS-SUB1, WS-SUB2).

       P5700-EXIT.
           EXIT.

       P5750-FIND-MONTH.
      * STEP BACK ONE MONTH.
           SUBTRACT 1 FROM WS-SUB2.

       P5750-EXIT.
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
           MOVE WS-NET-IN TO SU-ORIG-RECORD.
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
           MOVE 310                    TO CT-STEP-SEQ.
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
           DISPLAY 'PARTIES CLOSED   ' WS-CLOSE-CNT.
           DISPLAY 'DB2 ROWS         ' WS-SQL-CNT.
           DISPLAY 'VSAM RECORDS     ' WS-VSAM-CNT.
           DISPLAY 'CLOSE DATE       ' WS-CLOSE-YYDDD.
           DISPLAY 'CLOSED AMOUNT    ' WS-TOT-CLOSE-AMT.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE NET-IN-FILE
                 CLOSE-MASTER
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

