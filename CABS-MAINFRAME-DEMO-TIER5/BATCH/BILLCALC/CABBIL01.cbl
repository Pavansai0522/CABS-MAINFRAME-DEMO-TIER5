      *****************************************************************
      * CABBIL01 - BILL TRIGGER AND CYCLE SELECTION                   *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               ACCTIN  TELCABS.CABS.ACCOUNT(0)           (LOCAL)*
      *               CARRMST TELCABS.CABS.CARRIER              CABSCARR*
      *               SYSIN   INSTREAM CONTROL CARD             (LOCAL)*
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               TRIGOUT TELCABS.CABS.BILLTRIG(+1)         (LOCAL)*
      *               SKPOUT  TELCABS.CABS.BILLSKIP(+1)         (LOCAL)*
      *               REPORT  SYSOUT PRINT                      CABSPRNT*
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - THE TRIGGER FILE IS REBUILT EVERY CYCLE*
      * REVISION HISTORY                                              *
      *   V1.00  1987-11-02  R.T.WHEELER  INITIAL RELEASE - CYCLE SELECTION*
      *                      DRIVEN FROM THE ACCOUNT MASTER ONLY      *
      *   V1.04  1990-06-18  D.OKONKWO    CARRIER MASTER LOOKUP ADDED SO THAT*
      *                      AN INACTIVE OCN SUPPRESSES ITS BANS      *
      *   V1.09  1994-03-07  M.J.FERRARO  LAST OCN HOLD REMOVED - THE MASTER*
      *                      IS UPDATED DURING THE BILL WINDOW        *
      *   V1.12  1996-08-21  J.M.CASTILLO Y2K REVIEW - CENTURY RULE ADDED TO*
      *                      THE FEBRUARY TEST IN P4200               *
      *   V2.00  1999-04-30  P.NAIR       MINIMUM BILL INTERVAL INTRODUCED AT*
      *                      25 DAYS AFTER THE MARCH DOUBLE BILL      *
      *   V2.02  2002-10-15  A.BUKOWSKI   OPERATOR FORCE SWITCH AND OCN RANGE*
      *                      ADDED FOR SINGLE CARRIER RE-DRIVES       *
      *   V2.05  2009-05-19  S.MARCHETTI  SKIP FILE NOW CARRIES EVERY ACCOUNT*
      *                      THAT DID NOT BILL - EQUATION BALANCES    *
      *   V2.07  2018-09-14  G.PRZYBYLSKI DUE DAYS OVERRIDE ACCEPTED FROM THE*
      *                      SCHEDULER FOR THE QUARTER END RUNS       *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABBIL01.
       AUTHOR. TELCABS APPLICATIONS - BILLING TEAM.
      *****************************************************************
      * DECIDES WHICH ACCOUNTS BILL ON THIS CYCLE DATE AND WRITES THE *
      * TRIGGER FILE THAT DRIVES EVERY LATER BILLCALC STEP.  ACCOUNTS *
      * THAT DO NOT BILL ARE WRITTEN OUT WITH A REASON CODE.          *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCT-IN-FILE ASSIGN TO UT-S-ACCTIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT CARRIER-MASTER ASSIGN TO DA-I-CARRMST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CR-KEY
               FILE STATUS IS WS-FS-TABLE.
           SELECT TRIG-OUT-FILE ASSIGN TO UT-S-TRIGOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT SKIP-OUT-FILE ASSIGN TO UT-S-SKPOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT PARM-FILE ASSIGN TO UT-S-SYSIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-TABLE.
           SELECT CONTROL-FILE ASSIGN TO UT-S-CTLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CONTROL.
           SELECT SUSPENSE-FILE ASSIGN TO UT-S-SUSPOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-SUSPENSE.
           SELECT PRINT-FILE ASSIGN TO UT-S-REPORT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-TABLE.
       DATA DIVISION.
       FILE SECTION.
      *****************************************************************
      * ACCTIN - THE NIGHTLY ACCOUNT EXTRACT FROM THE CUSTOMER        *
      * SYSTEM.  200 BYTES FB, SORTED BY BAN BY CABSRT09.             *
      *****************************************************************
       FD  ACCT-IN-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       01  CABS-ACCOUNT-RECORD.
           05  AC-KEY.
               10  AC-BAN              PIC X(13).
           05  AC-OCN                  PIC X(04).
           05  AC-NAME                 PIC X(40).
           05  AC-BILL-CYCLE           PIC 9(02).
           05  AC-STATUS               PIC X(01).
               88  AC-ACTIVE           VALUE 'A'.
               88  AC-FINAL            VALUE 'F'.
               88  AC-SUSPENDED        VALUE 'S'.
               88  AC-CLOSED           VALUE 'C'.
           05  AC-LAST-BILL-YYDDD      PIC 9(05).
           05  AC-LAST-BILL-PERIOD     PIC 9(06).
           05  AC-PRIOR-BAL            PIC S9(13)V9(02) COMP-3.
           05  AC-CREDIT-HOLD-SW       PIC X(01).
           05  AC-STATE-CD             PIC X(02).
           05  AC-MEDIA-CD             PIC X(01).
           05  AC-EST-YYDDD            PIC 9(05).
           05  AC-TERM-YYDDD           PIC 9(05).
           05  AC-FILLER               PIC X(107).
      *****************************************************************
      * CARRMST - CARRIER MASTER, VSAM KSDS, READ RANDOMLY BY OCN.    *
      *****************************************************************
       FD  CARRIER-MASTER
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 200 CHARACTERS.
       COPY CABSCARR.
      *****************************************************************
      * TRIGOUT - ONE RECORD PER ACCOUNT THAT BILLS.                  *
      *****************************************************************
       FD  TRIG-OUT-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       01  TRIG-RECORD                      PIC X(200).
      *****************************************************************
      * SKPOUT - ONE RECORD PER ACCOUNT THAT DID NOT BILL,            *
      * CARRYING THE REASON CODE IN THE TRIGGER CODE FIELD.           *
      *****************************************************************
       FD  SKIP-OUT-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       01  SKIP-RECORD                      PIC X(200).
      *****************************************************************
      * PARM-FILE - THE SYSIN CONTROL CARD.  ONE CARD, 80 BYTES.      *
      * NOTHING IN THIS PROGRAM DEFAULTS A MISSING CARD.              *
      *****************************************************************
       FD  PARM-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE OMITTED
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  PARM-RECORD                      PIC X(80).
      *****************************************************************
      * CONTROL-FILE - THE MANDATORY RUN CONTROL RECORD.  SEE CABSCTL.*
      *****************************************************************
       FD  CONTROL-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CTL-RECORD                       PIC X(180).
      *****************************************************************
      * SUSPENSE-FILE - REJECTED AND QUARANTINED RECORDS.             *
      *****************************************************************
       FD  SUSPENSE-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 300 CHARACTERS.
       01  SUS-RECORD                       PIC X(300).
      *****************************************************************
      * PRINT-FILE - THE RUN REGISTER.  FBA 133, ASA CARRIAGE CONTROL.*
      *****************************************************************
       FD  PRINT-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       01  PRT-RECORD                       PIC X(133).
       WORKING-STORAGE SECTION.
      *****************************************************************
      * PROGRAM IDENTIFICATION - MOVED TO THE CONTROL RECORD AND TO   *
      * EVERY SUSPENSE RECORD RAISED BY THIS MODULE.                  *
      *****************************************************************
       01  WS-PROGRAM-IDENT.
           05  WS-PGM-NAME             PIC X(08) VALUE 'CABBIL01'.
           05  WS-PGM-VERSION          PIC X(05) VALUE 'V2.07'.
           05  WS-PGM-APPL             PIC X(04) VALUE 'CABS'.
           05  WS-PGM-COMPILE          PIC X(08) VALUE '20180914'.
           05  WS-PARA-NAME            PIC X(30) VALUE SPACES.
      *****************************************************************
      * RUN CONTEXT.  POPULATED FROM THE SYSIN CARD AND FROM THE JCL  *
      * SYMBOLICS THE SCHEDULER SUBSTITUTES AT SUBMISSION TIME.       *
      * NONE OF THESE HAVE DEFAULTS.                                  *
      *****************************************************************
       01  WS-RUN-CONTEXT.
           05  WS-RUN-ID               PIC X(12) VALUE SPACES.
           05  WS-CYCLE-YYDDD.
               10  WS-CYCLE-YY         PIC 9(02) VALUE 0.
               10  WS-CYCLE-DDD        PIC 9(03) VALUE 0.
           05  WS-BILL-PERIOD          PIC 9(06) VALUE 0.
           05  WS-RERUN-NBR            PIC 9(02) VALUE 0.
           05  WS-JOBNAME              PIC X(08) VALUE SPACES.
           05  WS-STEPNAME             PIC X(08) VALUE SPACES.
           05  WS-RETURN-CODE          PIC 9(04) VALUE 0.
           05  WS-BAL-CHECK            PIC S9(11) COMP-3 VALUE 0.
           05  WS-ERR-CODE             PIC X(04) VALUE SPACES.
           05  WS-ERR-SEVERITY         PIC X(01) VALUE 'E'.
           05  WS-RESTART-KEY          PIC X(26) VALUE SPACES.
           05  WS-SUB-RC               PIC S9(04) COMP VALUE 0.
           05  WS-GREG-CYCLE           PIC 9(08) VALUE 0.
      *****************************************************************
      * BILL DETAIL, BILL HEADER AND PRINT LAYOUTS ARE SHARED.  THE   *
      * ACCOUNT LAYOUT IS LOCAL TO THIS PROGRAM AND IS DECLARED IN THE*
      * FILE SECTION, NOT IN A COPYBOOK - THAT WAS NEVER REGULARISED. *
      *****************************************************************
       COPY CABSWRK.

       COPY CABSCARR.

       COPY CABSPRNT.
      *****************************************************************
      * ACCEPT AREAS AND SPARE WORK FIELDS.                           *
      *****************************************************************
       01  WS-ACCEPT-AREAS.
           05  WS-ACCEPT-DATE          PIC 9(06) VALUE 0.
           05  WS-ACCEPT-TIME          PIC 9(08) VALUE 0.
       01  WS-AD-WORK.
           05  WS-AD-YY                PIC 9(02).
           05  WS-AD-MM                PIC 9(02).
           05  WS-AD-DD                PIC 9(02).
       01  WS-AD-ALT REDEFINES WS-AD-WORK.
           05  WS-AD-YYMM              PIC 9(04).
           05  WS-AD-DAY               PIC 9(02).
      *****************************************************************
      * SYSIN CONTROL CARD.  READ AS 80 BYTES THEN REDEFINED THREE    *
      * WAYS.  THE CARD TYPE IN COLUMNS 1-2 DECIDES WHICH REDEFINE IS *
      * VALID.  NOTHING IN THE PROGRAM ENFORCES THAT AGREEMENT.       *
      * NO COPYBOOK IS RAISED FOR CONTROL CARDS - SEE CABS-STD-014.   *
      *****************************************************************
       01  WS-PARM-CARD.
           05  WS-PC-TYPE              PIC X(02) VALUE SPACES.
           05  WS-PC-REST              PIC X(78) VALUE SPACES.
       01  WS-PARM-RUN REDEFINES WS-PARM-CARD.
           05  FILLER                  PIC X(02).
           05  WS-PC-RUN-ID            PIC X(12).
           05  WS-PC-CYCLE.
               10  WS-PC-CYCLE-YY      PIC 9(02).
               10  WS-PC-CYCLE-DDD     PIC 9(03).
           05  WS-PC-BILL-PERIOD       PIC 9(06).
           05  WS-PC-RERUN             PIC 9(02).
           05  WS-PC-JOBNAME           PIC X(08).
           05  WS-PC-STEPNAME          PIC X(08).
           05  WS-PC-OPT1              PIC X(01).
           05  WS-PC-OPT2              PIC X(01).
           05  WS-PC-EXTRA             PIC X(35).
       01  WS-PARM-EXT REDEFINES WS-PARM-CARD.
           05  FILLER                  PIC X(45).
           05  WS-PE-FORCE-SW          PIC X(01).
           05  WS-PE-OCN-FROM          PIC X(04).
           05  WS-PE-OCN-THRU          PIC X(04).
           05  WS-PE-DUE-DAYS          PIC 9(03).
           05  WS-PE-FILLER            PIC X(23).
      *****************************************************************
      * THE 1989 CARD.  TWO REGIONS NEVER CONVERTED THEIR JCL AND     *
      * THE EDIT STILL ACCEPTS IT.  THE FIELDS ARE IN A DIFFERENT     *
      * ORDER AND THE CYCLE DATE IS SIX DIGITS YYMMDD, NOT YYDDD.     *
      *****************************************************************
       01  WS-PARM-OLD REDEFINES WS-PARM-CARD.
           05  FILLER                  PIC X(02).
           05  WS-PO-CYCLE             PIC 9(05).
           05  WS-PO-RUN-ID            PIC X(12).
           05  WS-PO-YYMMDD            PIC 9(06).
           05  WS-PO-CYCLE-NBR         PIC 9(02).
           05  FILLER                  PIC X(53).
      *****************************************************************
      * LOCAL SWITCHES.  THE STANDARD ONES ARRIVE VIA CABSWRK.        *
      *****************************************************************
       01  WS-LOCAL-SWITCHES.
           05  WS-PARM-EOF-SW          PIC X(01) VALUE 'N'.
               88  WS-PARM-EOF         VALUE 'Y'.
           05  WS-ACCT-EOF-SW          PIC X(01) VALUE 'N'.
               88  WS-ACCT-EOF         VALUE 'Y'.
           05  WS-CARR-FOUND-SW        PIC X(01) VALUE 'N'.
               88  WS-CARR-FOUND       VALUE 'Y'.
           05  WS-BILL-SW              PIC X(01) VALUE 'N'.
               88  WS-BILL-IT          VALUE 'Y'.
           05  WS-FORCE-SW             PIC X(01) VALUE 'N'.
               88  WS-FORCING          VALUE 'Y'.
           05  WS-IN-RANGE-SW          PIC X(01) VALUE 'N'.
               88  WS-IN-RANGE         VALUE 'Y'.
      *****************************************************************
      * CYCLE WORK.  THE BILL CYCLE NUMBER IS THE DAY OF THE MONTH ON *
      * WHICH THE ACCOUNT BILLS.  CYCLE 31 ACCOUNTS BILL ON THE LAST  *
      * DAY OF ANY MONTH THAT IS SHORTER THAN 31 DAYS - THAT RULE IS  *
      * CARRIED IN P3300 AND NOWHERE ELSE.                            *
      *****************************************************************
       01  WS-CYCLE-WORK.
           05  WS-CY-DAY-OF-MONTH      PIC 9(02) VALUE 0.
           05  WS-CY-MONTH             PIC 9(02) VALUE 0.
           05  WS-CY-YEAR              PIC 9(04) VALUE 0.
           05  WS-CY-CARR-CYCLE        PIC 9(02) VALUE 0.
           05  WS-CY-DAYS-IN-MONTH     PIC 9(02) VALUE 0.
           05  WS-CY-LAST-ABS          PIC S9(07) COMP-3 VALUE 0.
           05  WS-CY-CURR-ABS          PIC S9(07) COMP-3 VALUE 0.
           05  WS-CY-DAYS-SINCE        PIC S9(05) COMP-3 VALUE 0.
           05  WS-CY-MIN-INTERVAL      PIC S9(05) COMP-3 VALUE 25.
           05  WS-CY-DUE-ABS           PIC S9(07) COMP-3 VALUE 0.
           05  WS-CY-DUE-YYDDD         PIC 9(05) VALUE 0.
       01  WS-MONTH-DAYS-TABLE.
           05  FILLER PIC X(24) VALUE
               '312831303130313130313031'.
       01  WS-MONTH-DAYS-R REDEFINES WS-MONTH-DAYS-TABLE.
           05  WS-MD-DAYS OCCURS 12 TIMES  PIC 9(02).
      *****************************************************************
      * THE TRIGGER RECORD.  200 BYTES, WRITTEN TO TELCABS.CABS.BILLTRIG*
      * AND READ BY EVERY BILLCALC PROGRAM THAT FOLLOWS.              *
      *****************************************************************
       01  WS-TRIGGER-RECORD.
           05  WS-TR-BAN               PIC X(13) VALUE SPACES.
           05  WS-TR-BILL-PERIOD       PIC 9(06) VALUE 0.
           05  WS-TR-OCN               PIC X(04) VALUE SPACES.
           05  WS-TR-CYCLE-YYDDD       PIC 9(05) VALUE 0.
           05  WS-TR-DUE-YYDDD         PIC 9(05) VALUE 0.
           05  WS-TR-STATE-CD          PIC X(02) VALUE SPACES.
           05  WS-TR-MEDIA-CD          PIC X(01) VALUE SPACES.
           05  WS-TR-TERMS-DAYS        PIC 9(03) VALUE 0.
           05  WS-TR-PRIOR-BAL         PIC S9(13)V9(02) VALUE 0.
           05  WS-TR-TRIGGER-CD        PIC X(02) VALUE SPACES.
           05  WS-TR-FIRST-BILL-SW     PIC X(01) VALUE SPACES.
           05  WS-TR-FINAL-BILL-SW     PIC X(01) VALUE SPACES.
           05  WS-TR-FILLER            PIC X(142) VALUE SPACES.
       01  WS-TRIGGER-KEYED REDEFINES WS-TRIGGER-RECORD.
           05  WS-TK-KEY               PIC X(19).
           05  WS-TK-REST              PIC X(181).
       01  WS-TRIGGER-AMT REDEFINES WS-TRIGGER-RECORD.
           05  WS-TA-HEAD              PIC X(43).
           05  WS-TA-BALANCE           PIC S9(13)V9(02).
           05  WS-TA-TAIL              PIC X(142).
      *****************************************************************
      * SKIP REASON TABLE.  ONE COUNTER PER REASON.  THE REASON TEXT IS*
      * PRINTED ON THE REGISTER IN TABLE ORDER, NOT IN CODE ORDER.    *
      *****************************************************************
       01  WS-SKIP-REASON-TABLE.
           05  FILLER PIC X(34) VALUE
               'S1CYCLE DAY DOES NOT MATCH        '.
           05  FILLER PIC X(34) VALUE
               'S2ACCOUNT CLOSED                  '.
           05  FILLER PIC X(34) VALUE
               'S3ACCOUNT SUSPENDED               '.
           05  FILLER PIC X(34) VALUE
               'S4BILLED WITHIN MINIMUM INTERVAL  '.
           05  FILLER PIC X(34) VALUE
               'S5CARRIER NOT ON CARRIER MASTER   '.
           05  FILLER PIC X(34) VALUE
               'S6CARRIER OUTSIDE OCN RANGE       '.
           05  FILLER PIC X(34) VALUE
               'S7CARRIER NOT EFFECTIVE THIS CYCLE'.
           05  FILLER PIC X(34) VALUE
               'S8ACCOUNT TERMINATED BEFORE CYCLE '.
           05  FILLER PIC X(34) VALUE
               'S9CREDIT HOLD SET ON THE ACCOUNT  '.
           05  FILLER PIC X(34) VALUE
               'SACARRIER MARKED INACTIVE         '.
       01  WS-SKIP-REASON-R REDEFINES WS-SKIP-REASON-TABLE.
           05  WS-SR-ENTRY OCCURS 10 TIMES INDEXED BY WS-SR-X.
               10  WS-SR-CODE          PIC X(02).
               10  WS-SR-TEXT          PIC X(32).
       01  WS-SKIP-COUNTS.
           05  WS-SK-COUNT OCCURS 10 TIMES PIC S9(09) COMP-3.
       01  WS-SKIP-WORK.
           05  WS-SK-CODE              PIC X(02) VALUE SPACES.
           05  WS-SK-SUB               PIC S9(03) COMP-3 VALUE 0.
           05  WS-SK-FOUND-SW          PIC X(01) VALUE 'N'.
               88  WS-SK-FOUND         VALUE 'Y'.
      *****************************************************************
      * RUN TOTALS.                                                   *
      *****************************************************************
       01  WS-RUN-TOTALS.
           05  WS-RT-TRIGGERED         PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-SKIPPED           PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-FIRST-BILL        PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-FINAL-BILL        PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-FORCED            PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-PRIOR-BAL         PIC S9(15)V9(02) COMP-3 VALUE 0.
      *****************************************************************
      * PRINT AND PAGE CONTROL.  EVERY PROGRAM IN THE FAMILY WRITES A *
      * REGISTER TO DD REPORT.                                        *
      *****************************************************************
       01  WS-REPORT-WORK.
           05  WS-PAGE-LINES           PIC S9(05) COMP-3 VALUE 0.
           05  WS-PAGE-NBR             PIC S9(05) COMP-3 VALUE 0.
           05  WS-MAX-LINES            PIC S9(05) COMP-3 VALUE 58.
           05  WS-ED-PAGE-DATE         PIC 9(08) VALUE 0.
           05  WS-ED-COUNT             PIC ZZZ,ZZZ,ZZ9.
           05  WS-ED-MONEY             PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-ED-RATE              PIC Z.ZZZZ9.
           05  WS-ED-PCT               PIC ZZ9.99.
      *****************************************************************
      * SUBSCRIPTS AND INDEX WORK FIELDS.                             *
      *****************************************************************
       01  WS-SUBSCRIPTS.
           05  WS-SUB1                 PIC S9(05) COMP-3 VALUE 0.
           05  WS-SUB2                 PIC S9(05) COMP-3 VALUE 0.
           05  WS-SUB3                 PIC S9(05) COMP-3 VALUE 0.
           05  WS-SUB4                 PIC S9(05) COMP-3 VALUE 0.
           05  WS-SAVE-SUB             PIC S9(05) COMP-3 VALUE 0.
      *****************************************************************
      * ABEND COMMUNICATION AREA.  PASSED TO CABABEND WHICH ISSUES A  *
      * USER ABEND WITH THE CODE IN WS-AB-CODE.                       *
      *****************************************************************
       01  WS-ABEND-AREA.
           05  WS-AB-CODE              PIC 9(04) COMP VALUE 0.
           05  WS-AB-PGM               PIC X(08) VALUE SPACES.
           05  WS-AB-PARA              PIC X(30) VALUE SPACES.
           05  WS-AB-TEXT              PIC X(60) VALUE SPACES.
           05  WS-AB-KEY               PIC X(26) VALUE SPACES.
      *****************************************************************
      * PARAMETER AREA FOR CABDATCV - THE SHARED DATE CONVERSION      *
      * SUBROUTINE.  CABDATCV IS 1988 VINTAGE AND STILL PIVOTS ON 70  *
      * INTERNALLY.                                                   *
      *****************************************************************
       01  WS-DATE-PARM.
           05  WS-DP-FUNCTION          PIC X(02) VALUE SPACES.
           05  WS-DP-YYDDD             PIC 9(05) VALUE 0.
           05  WS-DP-CCYYMMDD          PIC 9(08) VALUE 0.
           05  WS-DP-DAYS              PIC S9(07) COMP-3 VALUE 0.
           05  WS-DP-RC                PIC 9(02) VALUE 0.
      *****************************************************************
      * SUSPENSE WRITER PARAMETER AREA - PASSED TO CABERRWR.          *
      *****************************************************************
       01  WS-ERRW-AREA.
           05  WS-EW-PGM               PIC X(08) VALUE SPACES.
           05  WS-EW-PARA              PIC X(30) VALUE SPACES.
           05  WS-EW-CODE              PIC X(04) VALUE SPACES.
           05  WS-EW-SEV               PIC X(01) VALUE SPACES.
           05  WS-EW-RUN-ID            PIC X(12) VALUE SPACES.
           05  WS-EW-DATA              PIC X(200) VALUE SPACES.
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
      * NOTHING IS DEFAULTED.  IF THE SCHEDULER DID NOT SUPPLY A CYCLE
      * DATE THE STEP ABENDS - IT DOES NOT ASSUME TODAY.
           MOVE 'P1000-INIT' TO WS-PARA-NAME.
           ACCEPT WS-ACCEPT-DATE FROM DATE.
           ACCEPT WS-ACCEPT-TIME FROM TIME.
           OPEN INPUT  ACCT-IN-FILE
                       CARRIER-MASTER
                       PARM-FILE
           OPEN OUTPUT TRIG-OUT-FILE
                       SKIP-OUT-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 4001 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-ACCTIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 4002 TO WS-AB-CODE
               MOVE 'OPEN FAILED DA-I-CARRMST' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 4003 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-TRIGOUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 4004 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-CTLOUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 4005 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-SUSPOUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           MOVE WS-ACCEPT-DATE         TO WS-AD-WORK.
           MOVE WS-AD-YY               TO DW-CUR-YY.
           PERFORM P1100-READ-PARM THRU P1100-EXIT.
           PERFORM P1200-EDIT-PARM THRU P1200-EXIT.
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
           MOVE ZERO TO WS-RT-TRIGGERED WS-RT-SKIPPED
                        WS-RT-FIRST-BILL WS-RT-FINAL-BILL
                        WS-RT-FORCED WS-RT-PRIOR-BAL.
           PERFORM P5150-CLEAR-COUNTS THRU P5150-EXIT
               VARYING WS-SUB1 FROM 1 BY 1
               UNTIL WS-SUB1 > 10.
           PERFORM P6000-HEADING THRU P6000-EXIT.
           DISPLAY WS-PGM-NAME ' STARTED - RUN ' WS-RUN-ID.
           DISPLAY '  CYCLE YYDDD  ' WS-CYCLE-YYDDD.
           DISPLAY '  BILL PERIOD  ' WS-BILL-PERIOD.
           DISPLAY '  FORCE SWITCH ' WS-PE-FORCE-SW.
           DISPLAY '  OCN RANGE    ' WS-PE-OCN-FROM ' TO '
                   WS-PE-OCN-THRU.

       P1000-EXIT.
           EXIT.

       P1100-READ-PARM.
      * THE SYSIN CARD CARRIES THE VALUES THE SCHEDULER SUBSTITUTED INTO
      * THE JCL AT SUBMISSION TIME.  THERE ARE NO DEFAULTS - AN ABSENT
      * CARD IS A FATAL ERROR, NOT A DEFAULTED RUN.
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
      * EDIT THE CONTROL CARD.  EVERY FIELD IS MANDATORY.  THE 1989 CARD
      * FORMAT IS STILL ACCEPTED VIA THE WS-PARM-OLD REDEFINE.
      * THE FORCE SWITCH AND THE OCN RANGE ARE SUPPLIED BY THE
      * SCHEDULER AT SUBMISSION.  THERE IS NO DEFAULT IN THE JCL AND
      * NO DEFAULT HERE - AN INVALID FORCE SWITCH ABENDS THE STEP.
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
           IF WS-PE-FORCE-SW NOT = 'Y' AND WS-PE-FORCE-SW NOT = 'N'
               MOVE 4011 TO WS-AB-CODE
               MOVE 'FORCE SWITCH NOT Y OR N' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PE-DUE-DAYS NOT NUMERIC
               MOVE ZERO TO WS-PE-DUE-DAYS.
           IF WS-PE-OCN-FROM NOT = SPACES
               IF WS-PE-OCN-THRU = SPACES
                   MOVE WS-PE-OCN-FROM TO WS-PE-OCN-THRU.

       P1200-EXIT.
           EXIT.

      *****************************************************************
      * S200-MAIN PROCESSING                                          *
      * ONE PASS OF THE ACCOUNT EXTRACT.  EVERY ACCOUNT IS EITHER     *
      * TRIGGERED OR SKIPPED - NOTHING IS SILENTLY DROPPED, WHICH IS  *
      * WHAT MAKES THE BALANCING EQUATION HOLD FOR THIS STEP.         *
      *****************************************************************
       S200-MAIN-PROCESS SECTION.

       P2000-PROCESS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ-ACCOUNT THRU P2100-EXIT.
           IF WS-ACCT-EOF
               MOVE 'Y' TO WS-EOF-SW
               GO TO P2000-EXIT.
           ADD 1 TO WS-READ-CNT.
           MOVE AC-BAN                 TO WS-RESTART-KEY.
           MOVE 'N' TO WS-BILL-SW.
           MOVE SPACES TO WS-SK-CODE.
           PERFORM P3000-SELECT-ACCOUNT THRU P3000-EXIT.
           IF WS-BILL-IT
               PERFORM P3700-BUILD-TRIGGER THRU P3700-EXIT
               PERFORM P3800-WRITE-TRIGGER THRU P3800-EXIT
           ELSE
               PERFORM P3900-WRITE-SKIP THRU P3900-EXIT.

       P2000-EXIT.
           EXIT.

       P2100-READ-ACCOUNT.
      * READ THE ACCOUNT EXTRACT.  THE EXTRACT IS PRODUCED BY THE CUSTOMER
      * SYSTEM OVERNIGHT AND ARRIVES SORTED BY BAN.  THE SORT ORDER IS NOT
      * VERIFIED HERE - CABSRT09 IS RELIED ON.
           MOVE 'P2100-READ-ACCOUNT' TO WS-PARA-NAME.
           READ ACCT-IN-FILE
               AT END
                   MOVE 'Y' TO WS-ACCT-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00' AND WS-FS-INPUT NOT = '10'
               MOVE 4101 TO WS-AB-CODE
               MOVE 'ACCOUNT EXTRACT READ ERROR' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.

       P2100-EXIT.
           EXIT.

      *****************************************************************
      * S300-SELECTION                                                *
      * THE ORDER OF THE TESTS BELOW IS THE ORDER IN WHICH THE SKIP   *
      * REASONS WERE ADDED, NOT AN ORDER OF PRECEDENCE.  AN ACCOUNT THAT*
      * FAILS TWO TESTS IS REPORTED UNDER WHICHEVER ONE IT REACHES FIRST.*
      *****************************************************************
       S300-SELECTION SECTION.

       P3000-SELECT-ACCOUNT.
           MOVE 'P3000-SELECT-ACCOUNT' TO WS-PARA-NAME.
           PERFORM P3100-READ-CARRIER THRU P3100-EXIT.
           IF NOT WS-CARR-FOUND
               MOVE 'S5' TO WS-SK-CODE
               MOVE EC-OCN-UNKNOWN TO WS-ERR-CODE
               GO TO P3000-EXIT.
           PERFORM P4100-RANGE-TEST THRU P4100-EXIT.
           IF NOT WS-IN-RANGE
               MOVE 'S6' TO WS-SK-CODE
               GO TO P3000-EXIT.
           IF CR-ACTIVE-SW NOT = 'Y'
               MOVE 'SA' TO WS-SK-CODE
               GO TO P3000-EXIT.
           IF CR-EFF-YYDDD > WS-CYCLE-YYDDD
               MOVE 'S7' TO WS-SK-CODE
               GO TO P3000-EXIT.
           IF CR-EXP-YYDDD NOT = ZERO
               IF CR-EXP-YYDDD < WS-CYCLE-YYDDD
                   MOVE 'S7' TO WS-SK-CODE
                   GO TO P3000-EXIT.
           PERFORM P3400-TEST-ACCOUNT-STATUS THRU P3400-EXIT.
           IF WS-SK-CODE NOT = SPACES
               GO TO P3000-EXIT.
           PERFORM P4000-FORCE-OVERRIDE THRU P4000-EXIT.
           IF WS-FORCING
               MOVE 'Y' TO WS-BILL-SW
               ADD 1 TO WS-RT-FORCED
               PERFORM P3600-DERIVE-BILL-PERIOD THRU P3600-EXIT
               GO TO P3000-EXIT.
           PERFORM P3200-DERIVE-CYCLE-DAY THRU P3200-EXIT.
           PERFORM P3300-TEST-CYCLE-MATCH THRU P3300-EXIT.
           IF WS-SK-CODE NOT = SPACES
               GO TO P3000-EXIT.
           PERFORM P3500-TEST-LAST-BILLED THRU P3500-EXIT.
           IF WS-SK-CODE NOT = SPACES
               GO TO P3000-EXIT.
           PERFORM P3600-DERIVE-BILL-PERIOD THRU P3600-EXIT.
           MOVE 'Y' TO WS-BILL-SW.

       P3000-EXIT.
           EXIT.

       P3100-READ-CARRIER.
      * THE CARRIER MASTER IS READ RANDOMLY.  THE EXTRACT IS IN BAN ORDER
      * AND CARRIERS REPEAT, SO THE SAME OCN IS READ MANY TIMES.  A HOLD
      * OF THE LAST OCN WAS TRIED IN 1994 AND BACKED OUT WHEN THE MASTER
      * STARTED BEING UPDATED DURING THE BILL RUN.
           MOVE 'P3100-READ-CARRIER' TO WS-PARA-NAME.
           MOVE 'N' TO WS-CARR-FOUND-SW.
           MOVE AC-OCN                 TO CR-OCN.
           READ CARRIER-MASTER
               INVALID KEY
                   MOVE 'N' TO WS-CARR-FOUND-SW
                   GO TO P3100-EXIT.
           MOVE 'Y' TO WS-CARR-FOUND-SW.
           ADD 1 TO WS-ACC-OCN-HASH.

       P3100-EXIT.
           EXIT.

       P3200-DERIVE-CYCLE-DAY.
      * CONVERT THE CYCLE YYDDD TO A GREGORIAN DATE SO THAT THE DAY OF THE
      * MONTH CAN BE COMPARED WITH THE CARRIER BILL CYCLE NUMBER.
           MOVE 'P3200-DERIVE-CYCLE-DAY' TO WS-PARA-NAME.
           MOVE 'JG' TO WS-DP-FUNCTION.
           MOVE WS-CYCLE-YYDDD         TO WS-DP-YYDDD.
           CALL 'CABDATCV' USING WS-DATE-PARM.
           IF WS-DP-RC NOT = ZERO
               MOVE 4102 TO WS-AB-CODE
               MOVE 'CYCLE DATE UNCONVERTIBLE IN P3200' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           MOVE WS-DP-CCYYMMDD         TO DW-GREG-DATE.
           MOVE DW-GR-CCYY             TO WS-CY-YEAR.
           MOVE DW-GR-MM               TO WS-CY-MONTH.
           MOVE DW-GR-DD               TO WS-CY-DAY-OF-MONTH.
           MOVE WS-MD-DAYS (WS-CY-MONTH) TO WS-CY-DAYS-IN-MONTH.
           IF WS-CY-MONTH = 2
               PERFORM P4200-LEAP-TEST THRU P4200-EXIT.
           MOVE CR-BILL-CYCLE          TO WS-CY-CARR-CYCLE.

       P3200-EXIT.
           EXIT.

       P3300-TEST-CYCLE-MATCH.
      * THE ACCOUNT BILLS WHEN THE CARRIER BILL CYCLE NUMBER EQUALS THE
      * DAY OF THE MONTH.  CYCLES 29, 30 AND 31 FALL OFF THE END OF THE
      * SHORTER MONTHS AND ARE PULLED BACK ONTO THE LAST DAY.
           MOVE 'P3300-TEST-CYCLE-MATCH' TO WS-PARA-NAME.
           IF WS-CY-CARR-CYCLE = WS-CY-DAY-OF-MONTH
               GO TO P3300-EXIT.
           IF WS-CY-CARR-CYCLE > WS-CY-DAYS-IN-MONTH
               IF WS-CY-DAY-OF-MONTH = WS-CY-DAYS-IN-MONTH
                   GO TO P3300-EXIT.
           MOVE 'S1' TO WS-SK-CODE.

       P3300-EXIT.
           EXIT.

       P3400-TEST-ACCOUNT-STATUS.
      * A FINAL BILL IS PRODUCED ONCE, WHATEVER THE CYCLE DAY.  A CLOSED
      * ACCOUNT IS NEVER BILLED AGAIN.  A SUSPENDED ACCOUNT BILLS ONLY
      * WHEN THE OPERATOR SUPPLIES THE FORCE SWITCH.
           MOVE 'P3400-TEST-ACCOUNT-STATUS' TO WS-PARA-NAME.
           IF AC-CLOSED
               MOVE 'S2' TO WS-SK-CODE
               GO TO P3400-EXIT.
           IF AC-TERM-YYDDD NOT = ZERO
               IF AC-TERM-YYDDD < WS-CYCLE-YYDDD
                   IF AC-LAST-BILL-YYDDD > AC-TERM-YYDDD
                       MOVE 'S8' TO WS-SK-CODE
                       GO TO P3400-EXIT.
           IF AC-SUSPENDED
               IF WS-PE-FORCE-SW NOT = 'Y'
                   MOVE 'S3' TO WS-SK-CODE
                   GO TO P3400-EXIT.
           IF AC-CREDIT-HOLD-SW = 'Y'
               IF WS-PE-FORCE-SW NOT = 'Y'
                   MOVE 'S9' TO WS-SK-CODE
                   GO TO P3400-EXIT.
           IF AC-FINAL
               MOVE 'Y' TO WS-TR-FINAL-BILL-SW
               ADD 1 TO WS-RT-FINAL-BILL.

       P3400-EXIT.
           EXIT.

       P3500-TEST-LAST-BILLED.
      * AN ACCOUNT MUST NOT BE BILLED TWICE INSIDE THE MINIMUM INTERVAL.
      * THE COMPARISON IS DONE ON ABSOLUTE DAY NUMBERS SO THAT A CYCLE
      * SPANNING A YEAR END BEHAVES.
           MOVE 'P3500-TEST-LAST-BILLED' TO WS-PARA-NAME.
           IF AC-LAST-BILL-YYDDD = ZERO
               MOVE 'Y' TO WS-TR-FIRST-BILL-SW
               ADD 1 TO WS-RT-FIRST-BILL
               GO TO P3500-EXIT.
           MOVE 'JA' TO WS-DP-FUNCTION.
           MOVE AC-LAST-BILL-YYDDD     TO WS-DP-YYDDD.
           CALL 'CABDATCV' USING WS-DATE-PARM.
           MOVE WS-DP-DAYS             TO WS-CY-LAST-ABS.
           MOVE 'JA' TO WS-DP-FUNCTION.
           MOVE WS-CYCLE-YYDDD         TO WS-DP-YYDDD.
           CALL 'CABDATCV' USING WS-DATE-PARM.
           MOVE WS-DP-DAYS             TO WS-CY-CURR-ABS.
           COMPUTE WS-CY-DAYS-SINCE =
                   WS-CY-CURR-ABS - WS-CY-LAST-ABS.
           IF WS-CY-DAYS-SINCE < WS-CY-MIN-INTERVAL
               MOVE 'S4' TO WS-SK-CODE.

       P3500-EXIT.
           EXIT.

       P3600-DERIVE-BILL-PERIOD.
      * THE BILL PERIOD IS YYMMCC WHERE CC IS THE CARRIER BILL CYCLE.  IT
      * IS THE KEY EVERY DOWNSTREAM PROGRAM JOINS ON.  THE DUE DATE IS
      * THE CYCLE DATE PLUS THE CARRIER PAYMENT TERMS IN DAYS, OR THE
      * OPERATOR SUPPLIED DUE DAYS WHEN THAT IS NON ZERO.
           MOVE 'P3600-DERIVE-BILL-PERIOD' TO WS-PARA-NAME.
           MOVE WS-CY-YEAR             TO DW-CENTURY-WORK.
           MOVE WS-CYCLE-YY            TO DW-BP-YY.
           MOVE WS-CY-MONTH            TO DW-BP-MM.
           MOVE CR-BILL-CYCLE          TO DW-BP-CYCLE.
           MOVE DW-BILL-PERIOD         TO WS-TR-BILL-PERIOD.
           MOVE CR-TERMS-DAYS          TO WS-TR-TERMS-DAYS.
           IF WS-PE-DUE-DAYS NOT = ZERO
               MOVE WS-PE-DUE-DAYS     TO WS-TR-TERMS-DAYS.
           MOVE 'JA' TO WS-DP-FUNCTION.
           MOVE WS-CYCLE-YYDDD         TO WS-DP-YYDDD.
           CALL 'CABDATCV' USING WS-DATE-PARM.
           COMPUTE WS-CY-DUE-ABS =
                   WS-DP-DAYS + WS-TR-TERMS-DAYS.
           MOVE 'AJ' TO WS-DP-FUNCTION.
           MOVE WS-CY-DUE-ABS          TO WS-DP-DAYS.
           CALL 'CABDATCV' USING WS-DATE-PARM.
           MOVE WS-DP-YYDDD            TO WS-CY-DUE-YYDDD.

       P3600-EXIT.
           EXIT.

       P3700-BUILD-TRIGGER.
      * ASSEMBLE THE TRIGGER RECORD.  THE TRIGGER CODE TELLS THE DETAIL
      * ASSEMBLER WHICH KIND OF BILL THIS IS - REGULAR, FIRST, FINAL OR
      * OPERATOR FORCED.  ONLY ONE CODE FITS SO THE PRECEDENCE BELOW IS
      * THE PRECEDENCE THE BUSINESS GETS.
           MOVE 'P3700-BUILD-TRIGGER' TO WS-PARA-NAME.
           MOVE AC-BAN                 TO WS-TR-BAN.
           MOVE AC-OCN                 TO WS-TR-OCN.
           MOVE WS-CYCLE-YYDDD         TO WS-TR-CYCLE-YYDDD.
           MOVE WS-CY-DUE-YYDDD        TO WS-TR-DUE-YYDDD.
           MOVE AC-STATE-CD            TO WS-TR-STATE-CD.
           MOVE AC-MEDIA-CD            TO WS-TR-MEDIA-CD.
           IF AC-MEDIA-CD = SPACES
               MOVE CR-BILL-MEDIA      TO WS-TR-MEDIA-CD.
           MOVE AC-PRIOR-BAL           TO WS-TR-PRIOR-BAL.
           MOVE 'RG' TO WS-TR-TRIGGER-CD.
           IF WS-TR-FIRST-BILL-SW = 'Y'
               MOVE 'FI' TO WS-TR-TRIGGER-CD.
           IF WS-TR-FINAL-BILL-SW = 'Y'
               MOVE 'FN' TO WS-TR-TRIGGER-CD.
           IF WS-FORCING
               MOVE 'FO' TO WS-TR-TRIGGER-CD.
           ADD AC-PRIOR-BAL            TO WS-RT-PRIOR-BAL.

       P3700-EXIT.
           EXIT.

       P3800-WRITE-TRIGGER.
           MOVE 'P3800-WRITE-TRIGGER' TO WS-PARA-NAME.
           WRITE TRIG-RECORD FROM WS-TRIGGER-RECORD.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 4103 TO WS-AB-CODE
               MOVE 'TRIGGER FILE WRITE FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-WRITE-CNT.
           ADD 1 TO WS-RT-TRIGGERED.
           PERFORM P5000-REGISTER-LINE THRU P5000-EXIT.
           MOVE SPACES TO WS-TR-FIRST-BILL-SW WS-TR-FINAL-BILL-SW.
           MOVE 'N' TO WS-FORCE-SW.

       P3800-EXIT.
           EXIT.

       P3900-WRITE-SKIP.
      * SKIPPED ACCOUNTS ARE WRITTEN OUT IN FULL SO THAT THE CONTROL
      * EQUATION HOLDS AND SO THAT THE ACCOUNT TEAM CAN SEE WHY AN
      * ACCOUNT DID NOT BILL.  THEY ARE COUNTED AS CARRIED FORWARD.
           MOVE 'P3900-WRITE-SKIP' TO WS-PARA-NAME.
           MOVE SPACES TO WS-TRIGGER-RECORD.
           MOVE AC-BAN                 TO WS-TR-BAN.
           MOVE AC-OCN                 TO WS-TR-OCN.
           MOVE WS-CYCLE-YYDDD         TO WS-TR-CYCLE-YYDDD.
           MOVE AC-STATE-CD            TO WS-TR-STATE-CD.
           MOVE WS-SK-CODE             TO WS-TR-TRIGGER-CD.
           MOVE ZERO TO WS-TR-BILL-PERIOD WS-TR-DUE-YYDDD
                        WS-TR-TERMS-DAYS WS-TR-PRIOR-BAL.
           WRITE SKIP-RECORD FROM WS-TRIGGER-RECORD.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 4104 TO WS-AB-CODE
               MOVE 'SKIP FILE WRITE FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-CFWD-CNT.
           ADD 1 TO WS-RT-SKIPPED.
           PERFORM P5100-COUNT-SKIP THRU P5100-EXIT.
           IF WS-ERR-CODE NOT = SPACES
               MOVE 'W' TO WS-ERR-SEVERITY
               MOVE WS-TRIGGER-RECORD TO WS-EW-DATA
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               SUBTRACT 1 FROM WS-REJECT-CNT
               MOVE SPACES TO WS-ERR-CODE.
           MOVE SPACES TO WS-TR-FIRST-BILL-SW WS-TR-FINAL-BILL-SW.

       P3900-EXIT.
           EXIT.

      *****************************************************************
      * S400-QUALIFIERS                                               *
      * RANGE, FORCE AND LEAP YEAR TESTS.                             *
      *****************************************************************
       S400-QUALIFIERS SECTION.

       P4000-FORCE-OVERRIDE.
      * THE OPERATOR FORCE SWITCH ARRIVES ON THE SYSIN CARD.  IT IS NEVER
      * CODED IN THE PRODUCTION JCL - THE SCHEDULER SUBSTITUTES IT AND
      * THE VALUE IS DECIDED BY THE BILLING SUPERVISOR ON THE NIGHT.
      * SCHEDULER SUBSTITUTION RULES ARE IN CABS-STD-022.
           MOVE 'P4000-FORCE-OVERRIDE' TO WS-PARA-NAME.
           MOVE 'N' TO WS-FORCE-SW.
           IF WS-PE-FORCE-SW NOT = 'Y'
               GO TO P4000-EXIT.
           IF AC-CLOSED
               GO TO P4000-EXIT.
           MOVE 'Y' TO WS-FORCE-SW.

       P4000-EXIT.
           EXIT.

       P4100-RANGE-TEST.
      * THE OCN RANGE LETS OPERATIONS RE-DRIVE A SINGLE CARRIER WITHOUT
      * REBUILDING THE WHOLE TRIGGER FILE.  BOTH ENDS ARE INCLUSIVE.  AN
      * ALL BLANK RANGE MEANS EVERY CARRIER.
           MOVE 'P4100-RANGE-TEST' TO WS-PARA-NAME.
           MOVE 'Y' TO WS-IN-RANGE-SW.
           IF WS-PE-OCN-FROM = SPACES AND WS-PE-OCN-THRU = SPACES
               GO TO P4100-EXIT.
           IF AC-OCN < WS-PE-OCN-FROM
               MOVE 'N' TO WS-IN-RANGE-SW
               GO TO P4100-EXIT.
           IF AC-OCN > WS-PE-OCN-THRU
               MOVE 'N' TO WS-IN-RANGE-SW.

       P4100-EXIT.
           EXIT.

       P4200-LEAP-TEST.
      * FEBRUARY.  THE CENTURY RULE IS APPLIED IN FULL - THE 1996 REVIEW
      * FOUND THE 1987 CODE STOPPED AT THE DIVISIBLE BY FOUR TEST.
           MOVE 'P4200-LEAP-TEST' TO WS-PARA-NAME.
           MOVE 'N' TO DW-LEAP-SW.
           DIVIDE WS-CY-YEAR BY 4 GIVING WS-SUB1
               REMAINDER WS-SUB2.
           IF WS-SUB2 = ZERO
               MOVE 'Y' TO DW-LEAP-SW.
           DIVIDE WS-CY-YEAR BY 100 GIVING WS-SUB1
               REMAINDER WS-SUB2.
           IF WS-SUB2 = ZERO
               MOVE 'N' TO DW-LEAP-SW.
           DIVIDE WS-CY-YEAR BY 400 GIVING WS-SUB1
               REMAINDER WS-SUB2.
           IF WS-SUB2 = ZERO
               MOVE 'Y' TO DW-LEAP-SW.
           IF DW-IS-LEAP
               MOVE 29 TO WS-CY-DAYS-IN-MONTH.

       P4200-EXIT.
           EXIT.

      *****************************************************************
      * S500-REGISTER LINES AND COUNTERS                              *
      *****************************************************************
       S500-TALLY SECTION.

       P5000-REGISTER-LINE.
      * ONE PRINT LINE PER TRIGGERED ACCOUNT.  THE REGISTER IS THE ONLY
      * PLACE THE BILL PERIOD AND THE DUE DATE APPEAR TOGETHER BEFORE
      * THE INVOICE ITSELF IS BUILT.
           MOVE 'P5000-REGISTER-LINE' TO WS-PARA-NAME.
           IF WS-PAGE-LINES > WS-MAX-LINES
               PERFORM P6000-HEADING THRU P6000-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-TR-BAN              TO PC-COL-001-020.
           MOVE SPACES                 TO PC-COL-021-060.
           MOVE WS-TR-OCN              TO PC-COL-021-060.
           MOVE WS-TR-BILL-PERIOD      TO WS-ED-PAGE-DATE.
           MOVE WS-ED-PAGE-DATE        TO PC-COL-061-090.
           MOVE WS-TR-PRIOR-BAL        TO WS-ED-MONEY.
           MOVE WS-ED-MONEY            TO PC-COL-091-132.
           WRITE PRT-RECORD FROM CABS-PRINT-LINE.
           ADD 1 TO WS-PAGE-LINES.

       P5000-EXIT.
           EXIT.

       P5100-COUNT-SKIP.
      * WALK THE REASON TABLE AND BUMP THE MATCHING COUNTER.  A REASON
      * CODE THAT IS NOT IN THE TABLE IS COUNTED AGAINST ENTRY 1, WHICH
      * IS WHY THE CYCLE DAY COUNT IS ALWAYS THE LARGEST.
           MOVE 'P5100-COUNT-SKIP' TO WS-PARA-NAME.
           MOVE 'N' TO WS-SK-FOUND-SW.
           MOVE 1 TO WS-SK-SUB.
           PERFORM P5110-SCAN-REASON THRU P5110-EXIT
               VARYING WS-SR-X FROM 1 BY 1
               UNTIL WS-SR-X > 10 OR WS-SK-FOUND.
           ADD 1 TO WS-SK-COUNT (WS-SK-SUB).

       P5100-EXIT.
           EXIT.

       P5110-SCAN-REASON.
           IF WS-SR-CODE (WS-SR-X) = WS-SK-CODE
               MOVE 'Y' TO WS-SK-FOUND-SW
               SET WS-SUB1 TO WS-SR-X
               MOVE WS-SUB1 TO WS-SK-SUB.

       P5110-EXIT.
           EXIT.

       P5200-PRINT-SKIP-SUMMARY.
      * PRINTED ONCE AT END OF RUN.  THE ACCOUNT TEAM WORK FROM THIS PAGE
      * EVERY MORNING.
           MOVE 'P5200-PRINT-SKIP-SUMMARY' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '1' TO PC-CC.
           MOVE 'ACCOUNTS NOT TRIGGERED - REASON SUMMARY' TO PC-TEXT.
           WRITE PRT-RECORD FROM CABS-PRINT-LINE.
           PERFORM P5210-PRINT-REASON THRU P5210-EXIT
               VARYING WS-SR-X FROM 1 BY 1
               UNTIL WS-SR-X > 10.

       P5200-EXIT.
           EXIT.

       P5210-PRINT-REASON.
           SET WS-SUB1 TO WS-SR-X.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-SR-CODE (WS-SR-X)   TO PC-COL-001-020.
           MOVE WS-SR-TEXT (WS-SR-X)   TO PC-COL-021-060.
           MOVE WS-SK-COUNT (WS-SUB1)  TO WS-ED-COUNT.
           MOVE WS-ED-COUNT            TO PC-COL-061-090.
           WRITE PRT-RECORD FROM CABS-PRINT-LINE.

       P5210-EXIT.
           EXIT.

      *****************************************************************
      * S600-REGISTER                                                 *
      * THE PRINTED RUN REGISTER AND THE SUSPENSE WRITER.             *
      *****************************************************************
       S600-REGISTER SECTION.

       P6000-HEADING.
      * THE REGISTER HEADING.  OPERATIONS FILE THE PRINTED REGISTER
      * WITH THE NIGHTLY BALANCING SHEET.  THE TITLE LINE POSITION IS
      * FIXED BY THE FILING CLERKS - DO NOT RE-CENTRE IT.
           MOVE 'P6000-HEADING' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '1' TO PC-CC.
           MOVE 'CABBIL01  BILL TRIGGER AND CYCLE SELECTION REGISTER'
                                       TO PC-TEXT.
           WRITE PRT-RECORD FROM CABS-PRINT-LINE.
           ADD 1 TO WS-PAGE-LINES.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RUN ' TO PC-COL-001-020.
           MOVE WS-RUN-ID TO PC-COL-021-060.
           MOVE WS-GREG-CYCLE TO WS-ED-PAGE-DATE.
           MOVE WS-ED-PAGE-DATE TO PC-COL-061-090.
           WRITE PRT-RECORD FROM CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '0' TO PC-CC.
           MOVE 'BAN                 OCN     BILL PERIOD    PRIOR BAL'
                                       TO PC-TEXT.
           WRITE PRT-RECORD FROM CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE ALL '-' TO PC-TEXT.
           WRITE PRT-RECORD FROM CABS-PRINT-LINE.
           ADD 3 TO WS-PAGE-LINES.

       P6000-EXIT.
           EXIT.

       P7000-SUSPEND.
      * WRITE THE OFFENDING RECORD TO THE SUSPENSE FILE THROUGH CABERRWR.
      * THE SUSPENSE FILE IS RE-PRESENTED BY THE RECYCLE JOB THE NEXT
      * NIGHT.  NOTHING IN THIS PROGRAM READS IT BACK.
           MOVE WS-PGM-NAME            TO WS-EW-PGM.
           MOVE WS-PARA-NAME           TO WS-EW-PARA.
           MOVE WS-ERR-CODE            TO WS-EW-CODE.
           MOVE WS-ERR-SEVERITY        TO WS-EW-SEV.
           MOVE WS-RUN-ID              TO WS-EW-RUN-ID.
           MOVE SPACES                 TO CABS-SUSPENSE-RECORD.
           MOVE WS-EW-CODE             TO SU-ERR-CODE.
           MOVE WS-EW-SEV              TO SU-ERR-SEVERITY.
           MOVE WS-EW-PGM              TO SU-DETECT-PGM.
           MOVE WS-EW-PARA             TO SU-DETECT-PARA.
           MOVE WS-EW-RUN-ID           TO SU-RUN-ID.
           MOVE WS-EW-DATA             TO SU-ORIG-RECORD.
           CALL 'CABERRWR' USING WS-ERRW-AREA
                                 WS-SUB-RC.
           WRITE SUS-RECORD FROM CABS-SUSPENSE-RECORD.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 3802 TO WS-AB-CODE
               MOVE 'SUSPENSE WRITE FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-REJECT-CNT.

       P7000-EXIT.
           EXIT.

      *****************************************************************
      * S800-RUN-CONTROL                                              *
      * THE MANDATORY CONTROL RECORD.  CABS-STD-001 SECTION 4.        *
      *****************************************************************
       S800-RUN-CONTROL SECTION.

       P8000-CONTROL.
      * MANDATORY CONTROL RECORD.  THE BALANCING EQUATION IS
      *   CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARISED
      *           + CT-CARRIED-FWD
      * A FAILURE HERE SETS CT-OUT-OF-BAL AND RC 0008.  THE NIGHTLY
      * CONTROL REPORT (CABRPT01) READS EVERY CONTROL RECORD AND
      * HALTS THE CYCLE ON ANY OUT OF BALANCE PROCESS.
           MOVE 'P8000-CONTROL' TO WS-PARA-NAME.
           MOVE SPACES                 TO CABS-CONTROL-RECORD.
           MOVE WS-RUN-ID              TO CT-RUN-ID.
           MOVE WS-PGM-NAME            TO CT-PROCESS-ID.
           MOVE 400                    TO CT-STEP-SEQ.
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
           MOVE 'P9000-TERM' TO WS-PARA-NAME.
           DISPLAY '--------------------------------------------'.
           DISPLAY WS-PGM-NAME ' V' WS-PGM-VERSION
                   ' RUN ' WS-RUN-ID.
           PERFORM P5200-PRINT-SKIP-SUMMARY THRU P5200-EXIT.
           DISPLAY 'ACCOUNTS TRIGGERED' WS-RT-TRIGGERED.
           DISPLAY 'ACCOUNTS SKIPPED  ' WS-RT-SKIPPED.
           DISPLAY 'FIRST BILLS       ' WS-RT-FIRST-BILL.
           DISPLAY 'FINAL BILLS       ' WS-RT-FINAL-BILL.
           DISPLAY 'OPERATOR FORCED   ' WS-RT-FORCED.
           DISPLAY 'PRIOR BALANCE SUM ' WS-RT-PRIOR-BAL.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE ACCT-IN-FILE
                 CARRIER-MASTER
                 TRIG-OUT-FILE
                 SKIP-OUT-FILE
                 PARM-FILE
                 CONTROL-FILE
                 SUSPENSE-FILE
                 PRINT-FILE
           .
           MOVE WS-RETURN-CODE TO RETURN-CODE.

       P9000-EXIT.
           EXIT.

       P9500-ABEND.
      * UNRECOVERABLE ERROR.  CABABEND ISSUES A USER ABEND SO THAT THE
      * STEP FAILS VISIBLY RATHER THAN COMPLETING WITH BAD DATA.
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

       P5150-CLEAR-COUNTS.
           MOVE ZERO TO WS-SK-COUNT (WS-SUB1).

       P5150-EXIT.
           EXIT.
