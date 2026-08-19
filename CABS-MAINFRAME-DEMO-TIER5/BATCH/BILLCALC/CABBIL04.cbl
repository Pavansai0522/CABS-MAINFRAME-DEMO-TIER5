      *****************************************************************
      * CABBIL04 - PRIOR BALANCE AND PAYMENT APPLICATION              *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               BALIN   TELCABS.CABS.BALANCE(0)           (LOCAL)*
      *               PAYIN   TELCABS.CABS.PAYMENT(0)           (LOCAL)*
      *               SYSIN   INSTREAM CONTROL CARD             (LOCAL)*
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               BHDROUT TELCABS.CABS.BILLHDR(+1)          CABSBHDR*
      *               REPORT  SYSOUT PRINT                      CABSPRNT*
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY (BAN)           *
      * REVISION HISTORY                                              *
      *   V1.00  1987-12-11  R.T.WHEELER  INITIAL RELEASE - PRIOR BALANCE ONLY,*
      *                      PAYMENTS APPLIED BY A SEPARATE JOB       *
      *   V1.03  1990-01-29  D.OKONKWO    PAYMENT FEED BROUGHT INTO THIS STEP*
      *                      AND HELD IN A STORAGE TABLE              *
      *   V1.08  1993-08-16  L.HARGREAVES FIVE AGEING BUCKETS REPLACED THE*
      *                      SINGLE OUTSTANDING AMOUNT                *
      *   V1.11  1995-06-05  M.J.FERRARO  DEFERRED PAYMENT PLAN SUPPORT ADDED*
      *                      FOR THE SETTLEMENT PROGRAMME             *
      *   V1.14  1997-02-27  J.M.CASTILLO Y2K - PAYMENT AGEING NOW GOES*
      *                      THROUGH CABDATCV ABSOLUTE DAYS           *
      *   V2.00  2000-10-09  P.NAIR       APPLICATION ORDER MADE SELECTABLE -*
      *                      OLDEST FIRST OR NEWEST FIRST             *
      *   V2.04  2004-04-22  A.BUKOWSKI   REVERSALS ACCEPTED ON THE PAYMENT*
      *                      FEED UNDER METHOD CODE RV                *
      *   V2.08  2010-09-30  S.MARCHETTI  PAYMENT TABLE RAISED TO 3000 ENTRIES*
      *   V2.11  2016-08-22  G.PRZYBYLSKI UNAPPLIED CASH NOW WRITTEN TO THE*
      *                      SUSPENSE FILE FOR THE CASH TEAM          *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABBIL04.
       AUTHOR. TELCABS APPLICATIONS - BILLING TEAM.
      *****************************************************************
      * CREATES THE BILL HEADER AND CARRIES THE PRIOR BALANCE AND THE *
      * CASH RECEIVED SINCE THE LAST BILL ONTO IT.  MAINTAINS THE FIVE*
      * AGEING BUCKETS ON THE BALANCE MASTER.                         *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BAL-IN-FILE ASSIGN TO UT-S-BALIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT PAY-IN-FILE ASSIGN TO UT-S-PAYIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-TABLE.
           SELECT BHDR-OUT-FILE ASSIGN TO UT-S-BHDROUT
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
      * BALIN - ACCOUNT BALANCE MASTER EXTRACT, ONE RECORD            *
      * PER ACCOUNT WITH FIVE AGEING BUCKETS.                         *
      *****************************************************************
       FD  BAL-IN-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       01  BAL-RECORD                       PIC X(200).
      *****************************************************************
      * PAYIN - CASH APPLICATION FEED.  RECEIPTS AND                  *
      * REVERSALS SHARE THE RECORD AND DIFFER IN LAYOUT.              *
      *****************************************************************
       FD  PAY-IN-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 120 CHARACTERS.
       01  PAY-RECORD                       PIC X(120).
      *****************************************************************
      * BHDROUT - THE BILL HEADER.  THE BILL TO BILL COMPARISON       *
      * KEYS ON THIS RECORD.                                          *
      *****************************************************************
       FD  BHDR-OUT-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 400 CHARACTERS.
       01  BHDR-RECORD                  PIC X(400).
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
           05  WS-PGM-NAME             PIC X(08) VALUE 'CABBIL04'.
           05  WS-PGM-VERSION          PIC X(05) VALUE 'V2.11'.
           05  WS-PGM-APPL             PIC X(04) VALUE 'CABS'.
           05  WS-PGM-COMPILE          PIC X(08) VALUE '20160822'.
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
       COPY CABSWRK.

       COPY CABSBHDR.

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
      * LAYOUT HELD IN THE APPLICATION FOLDER, NOT IN A COPYBOOK.     *
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
           05  WS-PE-APPLY-ORDER       PIC X(01).
           05  WS-PE-UNAPPLIED-SW      PIC X(01).
           05  WS-PE-AGE-BASE          PIC 9(05).
           05  WS-PE-WRITEOFF-LIMIT    PIC 9(05)V9(02).
           05  WS-PE-FILLER            PIC X(22).
      *****************************************************************
      * THE 1989 CARD.  TWO REGIONS NEVER CONVERTED THEIR JCL AND THE *
      * EDIT STILL ACCEPTS IT.  THE FIELDS ARE IN A DIFFERENT ORDER AND*
      * THE CYCLE DATE IS SIX DIGITS YYMMDD, NOT YYDDD.               *
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
           05  WS-BAL-EOF-SW           PIC X(01) VALUE 'N'.
               88  WS-BAL-EOF          VALUE 'Y'.
           05  WS-PAY-EOF-SW           PIC X(01) VALUE 'N'.
               88  WS-PAY-EOF          VALUE 'Y'.
           05  WS-PAY-FOUND-SW         PIC X(01) VALUE 'N'.
               88  WS-PAY-FOUND        VALUE 'Y'.
           05  WS-FULLY-APPLIED-SW     PIC X(01) VALUE 'N'.
               88  WS-FULLY-APPLIED    VALUE 'Y'.
      *****************************************************************
      * THE PAYMENT TABLE.  PAYMENTS ARRIVE FROM THE CASH APPLICATION *
      * SYSTEM ON A SEPARATE FEED AND ARE HELD IN STORAGE BECAUSE THE *
      * BALANCE FILE AND THE PAYMENT FILE ARE NOT IN THE SAME ORDER.  *
      *****************************************************************
       01  WS-PAY-TABLE.
           05  WS-PY-ENTRY OCCURS 3000 TIMES INDEXED BY WS-PY-X.
               10  WS-PY-BAN           PIC X(13).
               10  WS-PY-YYDDD         PIC 9(05).
               10  WS-PY-AMOUNT        PIC S9(13)V9(02) COMP-3.
               10  WS-PY-METHOD        PIC X(02).
               10  WS-PY-REF           PIC X(16).
               10  WS-PY-APPLIED       PIC S9(13)V9(02) COMP-3.
               10  WS-PY-USED-SW       PIC X(01).
       01  WS-PAY-CTL.
           05  WS-PY-USED              PIC S9(05) COMP-3 VALUE 0.
           05  WS-PY-MAX               PIC S9(05) COMP-3 VALUE 3000.
           05  WS-PY-FIRST             PIC S9(05) COMP-3 VALUE 0.
           05  WS-PY-LAST              PIC S9(05) COMP-3 VALUE 0.
      *****************************************************************
      * THE PAYMENT INPUT RECORD.  120 BYTES, TWO REDEFINES - THE CASH*
      * SYSTEM SENDS A DIFFERENT LAYOUT FOR A REVERSAL THAN FOR A     *
      * RECEIPT AND THE METHOD CODE IS THE ONLY THING THAT SAYS WHICH.*
      *****************************************************************
       01  WS-PAY-IN.
           05  WS-PI-BAN               PIC X(13).
           05  WS-PI-YYDDD             PIC 9(05).
           05  WS-PI-AMOUNT            PIC S9(13)V9(02).
           05  WS-PI-METHOD            PIC X(02).
           05  WS-PI-REF               PIC X(16).
           05  WS-PI-FILLER            PIC X(69).
       01  WS-PAY-IN-REV REDEFINES WS-PAY-IN.
           05  WS-PR-BAN               PIC X(13).
           05  WS-PR-ORIG-YYDDD        PIC 9(05).
           05  WS-PR-AMOUNT            PIC S9(13)V9(02).
           05  WS-PR-METHOD            PIC X(02).
           05  WS-PR-ORIG-REF          PIC X(16).
           05  WS-PR-REASON            PIC X(04).
           05  WS-PR-FILLER            PIC X(65).
       01  WS-PAY-IN-K REDEFINES WS-PAY-IN.
           05  WS-PK-KEY               PIC X(18).
           05  WS-PK-REST              PIC X(102).
      *****************************************************************
      * THE BALANCE MASTER RECORD, HELD IN WORKING STORAGE WHILE THE  *
      * PAYMENTS ARE APPLIED TO IT.  FIVE AGEING BUCKETS.             *
      *****************************************************************
       01  WS-BAL-WORK.
           05  WS-BW-BAN               PIC X(13) VALUE SPACES.
           05  WS-BW-OCN               PIC X(04) VALUE SPACES.
           05  WS-BW-LAST-INV          PIC X(14) VALUE SPACES.
           05  WS-BW-TOTAL             PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-BW-BUCKET OCCURS 5 TIMES
                                       PIC S9(13)V9(02) COMP-3.
           05  WS-BW-BUCKET-AGE OCCURS 5 TIMES
                                       PIC 9(05).
       01  WS-BUCKET-NAMES.
           05  FILLER PIC X(12) VALUE 'CURRENT     '.
           05  FILLER PIC X(12) VALUE '30 DAYS     '.
           05  FILLER PIC X(12) VALUE '60 DAYS     '.
           05  FILLER PIC X(12) VALUE '90 DAYS     '.
           05  FILLER PIC X(12) VALUE 'OVER 120    '.
       01  WS-BUCKET-NAMES-R REDEFINES WS-BUCKET-NAMES.
           05  WS-BN-NAME OCCURS 5 TIMES PIC X(12).
      *****************************************************************
      * APPLICATION WORK.                                             *
      *****************************************************************
       01  WS-APPLY-WORK.
           05  WS-AW-REMAINING         PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-AW-THIS-BUCKET       PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-AW-APPLIED           PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-AW-UNAPPLIED         PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-AW-BUCKET-SUB        PIC S9(03) COMP-3 VALUE 0.
           05  WS-AW-AGE-DAYS          PIC S9(05) COMP-3 VALUE 0.
           05  WS-AW-BASE-ABS          PIC S9(07) COMP-3 VALUE 0.
           05  WS-AW-ITEM-ABS          PIC S9(07) COMP-3 VALUE 0.
      *****************************************************************
      * DEFERRED PAYMENT PLAN WORK.  THE PLAN TABLE WAS BUILT FOR THE *
      * 1995 SETTLEMENT PROGRAMME AND HAS BEEN CARRIED SINCE.         *
      *****************************************************************
       01  WS-PLAN-WORK.
           05  WS-PL-INSTALMENTS       PIC 9(02) VALUE 0.
           05  WS-PL-AMOUNT            PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-PL-PER-INSTAL        PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-PL-RESIDUE           PIC S9(05)V9(02) COMP-3 VALUE 0.
           05  WS-PL-FIRST-YYDDD       PIC 9(05) VALUE 0.
           05  WS-PL-ACTIVE-SW         PIC X(01) VALUE 'N'.
               88  WS-PL-ACTIVE        VALUE 'Y'.
       01  WS-RUN-TOTALS.
           05  WS-RT-HEADERS           PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-PAY-APPLIED       PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-PAY-UNAPPLIED     PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-PRIOR-TOTAL       PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-RT-PAY-TOTAL         PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-RT-UNAPP-TOTAL       PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-RT-BUCKET OCCURS 5 TIMES
                                       PIC S9(15)V9(02) COMP-3.
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
           OPEN INPUT  BAL-IN-FILE
                       PAY-IN-FILE
                       PARM-FILE
           OPEN OUTPUT BHDR-OUT-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 4051 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-BALIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 4052 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-PAYIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 4053 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-BHDROUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 4054 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-CTLOUT' TO WS-AB-TEXT
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
           PERFORM P4000-LOAD-PAYMENTS THRU P4000-EXIT.
           PERFORM P4050-CLEAR-TOTALS THRU P4050-EXIT
               VARYING WS-SUB1 FROM 1 BY 1
               UNTIL WS-SUB1 > 5.
           PERFORM P6000-HEADING THRU P6000-EXIT.
           DISPLAY WS-PGM-NAME ' STARTED - RUN ' WS-RUN-ID.
           DISPLAY '  APPLY ORDER   ' WS-PE-APPLY-ORDER.
           DISPLAY '  AGE BASE      ' WS-PE-AGE-BASE.

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
      * THE AGEING BASE DATE IS SUPPLIED BY THE SCHEDULER SO THAT A
      * RE-RUN AGES AGAINST THE ORIGINAL CYCLE AND NOT AGAINST THE
      * DATE THE RE-RUN HAPPENED TO BE SUBMITTED.
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
           IF WS-PE-APPLY-ORDER NOT = 'N'
               MOVE 'O' TO WS-PE-APPLY-ORDER.
           IF WS-PE-UNAPPLIED-SW NOT = 'N'
               MOVE 'Y' TO WS-PE-UNAPPLIED-SW.
           IF WS-PE-AGE-BASE NOT NUMERIC
               MOVE WS-PC-CYCLE TO WS-PE-AGE-BASE.
           IF WS-PE-AGE-BASE = ZERO
               MOVE WS-PC-CYCLE TO WS-PE-AGE-BASE.

       P1200-EXIT.
           EXIT.

      *****************************************************************
      * S200-MAIN PROCESSING                                          *
      * ONE PASS OF THE BALANCE MASTER.  ONE BILL HEADER IS WRITTEN PER*
      * ACCOUNT WITH THE PRIOR BALANCE AND THE PAYMENTS APPLIED.      *
      *****************************************************************
       S200-MAIN-PROCESS SECTION.

       P2000-PROCESS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ-BALANCE THRU P2100-EXIT.
           IF WS-BAL-EOF
               MOVE 'Y' TO WS-EOF-SW
               GO TO P2000-EXIT.
           ADD 1 TO WS-READ-CNT.
           MOVE WS-BW-BAN TO WS-RESTART-KEY.
           PERFORM P3000-BUILD-HEADER THRU P3000-EXIT.
           PERFORM P3100-FIND-PAYMENTS THRU P3100-EXIT.
           IF WS-PAY-FOUND
               PERFORM P3200-APPLY-PAYMENTS THRU P3200-EXIT.
           PERFORM P3300-AGE-BUCKETS THRU P3300-EXIT.
           PERFORM P3500-WRITE-HEADER THRU P3500-EXIT.

       P2000-EXIT.
           EXIT.

       P2100-READ-BALANCE.
      * THE BALANCE MASTER CARRIES ONE RECORD PER ACCOUNT WITH THE
      * OUTSTANDING AMOUNT SPLIT INTO FIVE AGEING BUCKETS.  THE BUCKETS
      * ARE MAINTAINED BY THIS PROGRAM AND BY NOTHING ELSE.
           MOVE 'P2100-READ-BALANCE' TO WS-PARA-NAME.
           READ BAL-IN-FILE INTO WS-BAL-WORK
               AT END
                   MOVE 'Y' TO WS-BAL-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00' AND WS-FS-INPUT NOT = '10'
               MOVE 4401 TO WS-AB-CODE
               MOVE 'BALANCE MASTER READ ERROR' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD WS-BW-TOTAL TO WS-ACC-AMOUNT.

       P2100-EXIT.
           EXIT.

      *****************************************************************
      * S300-BALANCE AND PAYMENT APPLICATION                          *
      *****************************************************************
       S300-APPLY SECTION.

       P3000-BUILD-HEADER.
      * START THE BILL HEADER.  THE REMAINING AMOUNT FIELDS ARE FILLED
      * IN BY LATER STEPS IN THE STREAM - THIS PROGRAM OWNS THE PRIOR
      * BALANCE AND THE PAYMENTS AND NOTHING ELSE.
           MOVE 'P3000-BUILD-HEADER' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-BILL-HEADER.
           MOVE WS-BW-BAN              TO BH-BAN.
           MOVE WS-BILL-PERIOD         TO BH-BILL-PERIOD.
           MOVE WS-BW-OCN              TO BH-OCN.
           MOVE SPACES                 TO BH-INVOICE-NBR.
           MOVE WS-CYCLE-YYDDD         TO BH-BILL-YYDDD.
           MOVE ZERO TO BH-PRIOR-BAL BH-PAYMENTS BH-ADJUSTMENTS
                        BH-CURR-USAGE BH-CURR-RECURRING
                        BH-CURR-NONRECUR BH-RESTATEMENT
                        BH-SETTLEMENT-NET BH-TAX BH-TOTAL-DUE.
           MOVE ZERO TO BH-INTERSTATE-AMT BH-INTRASTATE-AMT
                        BH-LOCAL-AMT BH-DETAIL-LINES
                        BH-CDR-COUNT BH-HASH-AMOUNT.
           MOVE WS-BW-TOTAL            TO BH-PRIOR-BAL.
           MOVE 'P'                    TO BH-STATUS.
           MOVE SPACES                 TO BH-HOLD-REASON.
           ADD WS-BW-TOTAL             TO WS-RT-PRIOR-TOTAL.

       P3000-EXIT.
           EXIT.

       P3100-FIND-PAYMENTS.
      * LOCATE THE FIRST AND LAST PAYMENT FOR THIS ACCOUNT.  THE TABLE
      * IS IN BAN ORDER SO THE ENTRIES FOR ONE ACCOUNT ARE CONTIGUOUS.
           MOVE 'P3100-FIND-PAYMENTS' TO WS-PARA-NAME.
           MOVE 'N' TO WS-PAY-FOUND-SW.
           MOVE ZERO TO WS-PY-FIRST WS-PY-LAST.
           PERFORM P3110-SCAN-PAYMENTS THRU P3110-EXIT
               VARYING WS-PY-X FROM 1 BY 1
               UNTIL WS-PY-X > WS-PY-USED.
           IF WS-PY-FIRST NOT = ZERO
               MOVE 'Y' TO WS-PAY-FOUND-SW.

       P3100-EXIT.
           EXIT.

       P3110-SCAN-PAYMENTS.
           IF WS-PY-BAN (WS-PY-X) NOT = WS-BW-BAN
               GO TO P3110-EXIT.
           IF WS-PY-USED-SW (WS-PY-X) = 'Y'
               GO TO P3110-EXIT.
           SET WS-SUB1 TO WS-PY-X.
           IF WS-PY-FIRST = ZERO
               MOVE WS-SUB1 TO WS-PY-FIRST.
           MOVE WS-SUB1 TO WS-PY-LAST.

       P3110-EXIT.
           EXIT.

       P3200-APPLY-PAYMENTS.
      * APPLY EACH PAYMENT AGAINST THE AGEING BUCKETS.  THE ORDER IS
      * OLDEST FIRST UNLESS THE CONTROL CARD SAYS OTHERWISE.  ANYTHING
      * LEFT OVER BECOMES UNAPPLIED CASH AND STAYS ON THE ACCOUNT AS A
      * CREDIT AGAINST THE NEXT BILL.
           MOVE 'P3200-APPLY-PAYMENTS' TO WS-PARA-NAME.
           MOVE ZERO TO WS-AW-APPLIED WS-AW-UNAPPLIED.
           PERFORM P3210-APPLY-ONE THRU P3210-EXIT
               VARYING WS-SUB2 FROM WS-PY-FIRST BY 1
               UNTIL WS-SUB2 > WS-PY-LAST.
           MOVE WS-AW-APPLIED          TO BH-PAYMENTS.
           ADD WS-AW-APPLIED           TO WS-RT-PAY-TOTAL.
           ADD WS-AW-UNAPPLIED         TO WS-RT-UNAPP-TOTAL.

       P3200-EXIT.
           EXIT.

       P3210-APPLY-ONE.
           SET WS-PY-X TO WS-SUB2.
           IF WS-PY-BAN (WS-PY-X) NOT = WS-BW-BAN
               GO TO P3210-EXIT.
           IF WS-PY-USED-SW (WS-PY-X) = 'Y'
               GO TO P3210-EXIT.
           MOVE WS-PY-AMOUNT (WS-PY-X) TO WS-AW-REMAINING.
           MOVE 'N' TO WS-FULLY-APPLIED-SW.
           IF WS-PE-APPLY-ORDER = 'N'
               PERFORM P3220-APPLY-NEWEST THRU P3220-EXIT
                   VARYING WS-AW-BUCKET-SUB FROM 1 BY 1
                   UNTIL WS-AW-BUCKET-SUB > 5 OR WS-FULLY-APPLIED
           ELSE
               PERFORM P3230-APPLY-OLDEST THRU P3230-EXIT
                   VARYING WS-AW-BUCKET-SUB FROM 5 BY -1
                   UNTIL WS-AW-BUCKET-SUB < 1 OR WS-FULLY-APPLIED.
           COMPUTE WS-AW-APPLIED =
                   WS-AW-APPLIED
                 + WS-PY-AMOUNT (WS-PY-X) - WS-AW-REMAINING.
           MOVE WS-PY-AMOUNT (WS-PY-X) TO WS-PY-APPLIED (WS-PY-X).
           MOVE 'Y' TO WS-PY-USED-SW (WS-PY-X).
           ADD 1 TO WS-RT-PAY-APPLIED.
           IF WS-AW-REMAINING > ZERO
               ADD WS-AW-REMAINING TO WS-AW-UNAPPLIED
               ADD 1 TO WS-RT-PAY-UNAPPLIED
               PERFORM P3400-UNAPPLIED-CASH THRU P3400-EXIT.

       P3210-EXIT.
           EXIT.

       P3220-APPLY-NEWEST.
           MOVE WS-BW-BUCKET (WS-AW-BUCKET-SUB) TO WS-AW-THIS-BUCKET.
           IF WS-AW-THIS-BUCKET NOT > ZERO
               GO TO P3220-EXIT.
           IF WS-AW-REMAINING NOT < WS-AW-THIS-BUCKET
               SUBTRACT WS-AW-THIS-BUCKET FROM WS-AW-REMAINING
               MOVE ZERO TO WS-BW-BUCKET (WS-AW-BUCKET-SUB)
           ELSE
               SUBTRACT WS-AW-REMAINING
                   FROM WS-BW-BUCKET (WS-AW-BUCKET-SUB)
               MOVE ZERO TO WS-AW-REMAINING
               MOVE 'Y' TO WS-FULLY-APPLIED-SW.

       P3220-EXIT.
           EXIT.

       P3230-APPLY-OLDEST.
           MOVE WS-BW-BUCKET (WS-AW-BUCKET-SUB) TO WS-AW-THIS-BUCKET.
           IF WS-AW-THIS-BUCKET NOT > ZERO
               GO TO P3230-EXIT.
           IF WS-AW-REMAINING NOT < WS-AW-THIS-BUCKET
               SUBTRACT WS-AW-THIS-BUCKET FROM WS-AW-REMAINING
               MOVE ZERO TO WS-BW-BUCKET (WS-AW-BUCKET-SUB)
           ELSE
               SUBTRACT WS-AW-REMAINING
                   FROM WS-BW-BUCKET (WS-AW-BUCKET-SUB)
               MOVE ZERO TO WS-AW-REMAINING
               MOVE 'Y' TO WS-FULLY-APPLIED-SW.

       P3230-EXIT.
           EXIT.

       P3300-AGE-BUCKETS.
      * ROLL THE AGEING BUCKETS FORWARD BY ONE PERIOD AND ACCUMULATE THE
      * RUN TOTALS.  THE OLDEST BUCKET ABSORBS WHAT ROLLS OUT OF THE
      * NINETY DAY BUCKET - NOTHING AGES OUT OF THE MASTER.
           MOVE 'P3300-AGE-BUCKETS' TO WS-PARA-NAME.
           ADD WS-BW-BUCKET (4) TO WS-BW-BUCKET (5).
           MOVE WS-BW-BUCKET (3) TO WS-BW-BUCKET (4).
           MOVE WS-BW-BUCKET (2) TO WS-BW-BUCKET (3).
           MOVE WS-BW-BUCKET (1) TO WS-BW-BUCKET (2).
           MOVE ZERO TO WS-BW-BUCKET (1).
           PERFORM P3310-ACCUM-BUCKET THRU P3310-EXIT
               VARYING WS-SUB3 FROM 1 BY 1
               UNTIL WS-SUB3 > 5.

       P3300-EXIT.
           EXIT.

       P3310-ACCUM-BUCKET.
           ADD WS-BW-BUCKET (WS-SUB3) TO WS-RT-BUCKET (WS-SUB3).

       P3310-EXIT.
           EXIT.

       P3400-UNAPPLIED-CASH.
      * CASH THAT COULD NOT BE APPLIED TO AN OPEN ITEM.  IT IS SHOWN ON
      * THE BILL AS A PAYMENT AND CARRIED AS A CREDIT BALANCE.  THE CASH
      * TEAM WORK FROM THE SUSPENSE FILE THE FOLLOWING MORNING.
           MOVE 'P3400-UNAPPLIED-CASH' TO WS-PARA-NAME.
           IF WS-PE-UNAPPLIED-SW NOT = 'Y'
               GO TO P3400-EXIT.
           SUBTRACT WS-AW-REMAINING FROM WS-BW-BUCKET (1).
           MOVE 'W' TO WS-ERR-SEVERITY.
           MOVE EC-DUP-SEQ TO WS-ERR-CODE.
           MOVE WS-PAY-IN TO WS-EW-DATA.
           PERFORM P7000-SUSPEND THRU P7000-EXIT.
           SUBTRACT 1 FROM WS-REJECT-CNT.
           MOVE SPACES TO WS-ERR-CODE.

       P3400-EXIT.
           EXIT.

       P3500-WRITE-HEADER.
           MOVE 'P3500-WRITE-HEADER' TO WS-PARA-NAME.
           COMPUTE BH-TOTAL-DUE =
                   BH-PRIOR-BAL - BH-PAYMENTS.
           WRITE BHDR-RECORD FROM CABS-BILL-HEADER.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 4402 TO WS-AB-CODE
               MOVE 'BILL HEADER WRITE FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-WRITE-CNT.
           ADD 1 TO WS-RT-HEADERS.
           PERFORM P5000-REGISTER-LINE THRU P5000-EXIT.

       P3500-EXIT.
           EXIT.

      *****************************************************************
      * S400-PAYMENT FILE LOAD                                        *
      *****************************************************************
       S400-PAYMENTS SECTION.

       P4000-LOAD-PAYMENTS.
      * LOAD THE PAYMENT FEED INTO STORAGE.  A REVERSAL CARRIES A
      * NEGATIVE AMOUNT UNDER A DIFFERENT LAYOUT AND THE METHOD CODE IS
      * THE ONLY THING THAT DISTINGUISHES THE TWO.
           MOVE 'P4000-LOAD-PAYMENTS' TO WS-PARA-NAME.
           MOVE ZERO TO WS-PY-USED.
           PERFORM P4010-READ-PAYMENT THRU P4010-EXIT
               UNTIL WS-PAY-EOF.
           DISPLAY 'PAYMENTS LOADED ' WS-PY-USED.

       P4000-EXIT.
           EXIT.

       P4010-READ-PAYMENT.
           READ PAY-IN-FILE INTO WS-PAY-IN
               AT END
                   MOVE 'Y' TO WS-PAY-EOF-SW
                   GO TO P4010-EXIT.
           IF WS-PY-USED NOT < WS-PY-MAX
               MOVE 4403 TO WS-AB-CODE
               MOVE 'PAYMENT TABLE FULL' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-PY-USED.
           SET WS-PY-X TO WS-PY-USED.
           MOVE WS-PI-BAN      TO WS-PY-BAN (WS-PY-X).
           MOVE WS-PI-YYDDD    TO WS-PY-YYDDD (WS-PY-X).
           MOVE WS-PI-METHOD   TO WS-PY-METHOD (WS-PY-X).
           MOVE WS-PI-REF      TO WS-PY-REF (WS-PY-X).
           MOVE ZERO           TO WS-PY-APPLIED (WS-PY-X).
           MOVE 'N'            TO WS-PY-USED-SW (WS-PY-X).
           IF WS-PI-METHOD = 'RV'
               COMPUTE WS-PY-AMOUNT (WS-PY-X) = WS-PR-AMOUNT * -1
           ELSE
               MOVE WS-PI-AMOUNT TO WS-PY-AMOUNT (WS-PY-X).

       P4010-EXIT.
           EXIT.

       P4100-PAYMENT-AGE.
      * HOW OLD IS THE PAYMENT.  USED BY THE REGISTER AND BY THE
      * DEFERRED PLAN ROUTINE.  THE BASE DATE COMES FROM THE CONTROL
      * CARD SO THAT A RE-RUN AGES AGAINST THE ORIGINAL CYCLE.
           MOVE 'P4100-PAYMENT-AGE' TO WS-PARA-NAME.
           MOVE 'JA' TO WS-DP-FUNCTION.
           MOVE WS-PE-AGE-BASE TO WS-DP-YYDDD.
           CALL 'CABDATCV' USING WS-DATE-PARM.
           MOVE WS-DP-DAYS TO WS-AW-BASE-ABS.
           MOVE 'JA' TO WS-DP-FUNCTION.
           MOVE WS-PY-YYDDD (WS-PY-X) TO WS-DP-YYDDD.
           CALL 'CABDATCV' USING WS-DATE-PARM.
           MOVE WS-DP-DAYS TO WS-AW-ITEM-ABS.
           COMPUTE WS-AW-AGE-DAYS =
                   WS-AW-BASE-ABS - WS-AW-ITEM-ABS.

       P4100-EXIT.
           EXIT.

      *****************************************************************
      * S500-REGISTER                                                 *
      *****************************************************************
       S500-TALLY SECTION.

       P5000-REGISTER-LINE.
           MOVE 'P5000-REGISTER-LINE' TO WS-PARA-NAME.
           IF WS-PAGE-LINES > WS-MAX-LINES
               PERFORM P6000-HEADING THRU P6000-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE BH-BAN                 TO PC-COL-001-020.
           MOVE BH-PRIOR-BAL           TO WS-ED-MONEY.
           MOVE WS-ED-MONEY            TO PC-COL-021-060.
           MOVE BH-PAYMENTS            TO WS-ED-MONEY.
           MOVE WS-ED-MONEY            TO PC-COL-061-090.
           MOVE BH-TOTAL-DUE           TO WS-ED-MONEY.
           MOVE WS-ED-MONEY            TO PC-COL-091-132.
           WRITE PRT-RECORD FROM CABS-PRINT-LINE.
           ADD 1 TO WS-PAGE-LINES.

       P5000-EXIT.
           EXIT.

       P5100-PRINT-AGEING.
      * THE AGEING RECAP AT THE FOOT OF THE REGISTER.  THE CREDIT TEAM
      * RECONCILE THIS AGAINST THE LEDGER EVERY MONTH.
           MOVE 'P5100-PRINT-AGEING' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '1' TO PC-CC.
           MOVE 'PRIOR BALANCE AGEING RECAP' TO PC-TEXT.
           WRITE PRT-RECORD FROM CABS-PRINT-LINE.
           PERFORM P5110-PRINT-BUCKET THRU P5110-EXIT
               VARYING WS-SUB1 FROM 1 BY 1
               UNTIL WS-SUB1 > 5.

       P5100-EXIT.
           EXIT.

       P5110-PRINT-BUCKET.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-BN-NAME (WS-SUB1)   TO PC-COL-001-020.
           MOVE WS-RT-BUCKET (WS-SUB1) TO WS-ED-MONEY.
           MOVE WS-ED-MONEY            TO PC-COL-021-060.
           WRITE PRT-RECORD FROM CABS-PRINT-LINE.

       P5110-EXIT.
           EXIT.

      *****************************************************************
      * S620-DEFERRED PAYMENT ARRANGEMENTS                            *
      * INSTALMENT HANDLING FOR ACCOUNTS UNDER A NEGOTIATED PLAN.     *
      *****************************************************************
       S620-PLANS SECTION.

       P6200-DEFERRED-PAYMENT-PLAN.
      * SPREAD AN AGREED BALANCE ACROSS INSTALMENTS AND POST THE FIRST
      * ONE TO THE CURRENT BUCKET.  THE RESIDUE FROM THE DIVISION IS
      * ADDED TO THE FINAL INSTALMENT SO THAT THE PLAN ADDS BACK TO THE
      * AGREED AMOUNT TO THE PENNY.
           MOVE 'P6200-DEFERRED-PAYMENT-PLAN' TO WS-PARA-NAME.
           IF WS-PL-INSTALMENTS = ZERO
               GO TO P6200-EXIT.
           COMPUTE WS-PL-PER-INSTAL =
                   WS-PL-AMOUNT / WS-PL-INSTALMENTS.
           COMPUTE WS-PL-RESIDUE =
                   WS-PL-AMOUNT
                 - (WS-PL-PER-INSTAL * WS-PL-INSTALMENTS).
           ADD WS-PL-PER-INSTAL TO WS-BW-BUCKET (1).
           ADD WS-PL-RESIDUE TO WS-BW-BUCKET (1).
           SUBTRACT WS-PL-AMOUNT FROM WS-BW-BUCKET (5).
           MOVE 'Y' TO WS-PL-ACTIVE-SW.

       P6200-EXIT.
           EXIT.

       P6210-PLAN-SCHEDULE.
      * DERIVE THE DATE OF THE FIRST INSTALMENT FROM THE CYCLE DATE AND
      * THE AGREED START OFFSET.
           MOVE 'P6210-PLAN-SCHEDULE' TO WS-PARA-NAME.
           MOVE 'JA' TO WS-DP-FUNCTION.
           MOVE WS-CYCLE-YYDDD TO WS-DP-YYDDD.
           CALL 'CABDATCV' USING WS-DATE-PARM.
           COMPUTE WS-AW-ITEM-ABS = WS-DP-DAYS + 30.
           MOVE 'AJ' TO WS-DP-FUNCTION.
           MOVE WS-AW-ITEM-ABS TO WS-DP-DAYS.
           CALL 'CABDATCV' USING WS-DATE-PARM.
           MOVE WS-DP-YYDDD TO WS-PL-FIRST-YYDDD.

       P6210-EXIT.
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
           MOVE 'CABBIL04  PRIOR BALANCE AND PAYMENT REGISTER'
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
           MOVE 'BAN                 PRIOR BALANCE   PAYMENTS      DU'
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
           MOVE 420                    TO CT-STEP-SEQ.
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
           PERFORM P5100-PRINT-AGEING THRU P5100-EXIT.
           DISPLAY 'HEADERS WRITTEN   ' WS-RT-HEADERS.
           DISPLAY 'PAYMENTS APPLIED  ' WS-RT-PAY-APPLIED.
           DISPLAY 'PAYMENTS UNAPPLIED' WS-RT-PAY-UNAPPLIED.
           DISPLAY 'PRIOR BALANCE SUM ' WS-RT-PRIOR-TOTAL.
           DISPLAY 'PAYMENT SUM       ' WS-RT-PAY-TOTAL.
           DISPLAY 'UNAPPLIED CASH    ' WS-RT-UNAPP-TOTAL.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE BAL-IN-FILE
                 PAY-IN-FILE
                 BHDR-OUT-FILE
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

       P4050-CLEAR-TOTALS.
           MOVE ZERO TO WS-RT-BUCKET (WS-SUB1).

       P4050-EXIT.
           EXIT.
