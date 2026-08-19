      *****************************************************************
      * CABRPT02 - REVENUE BY CARRIER AND JURISDICTION                *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               BDTLIN  TELCABS.CABS.BILLDTL.SEQ(0)       CABSBILL*
      *               CARRMST TELCABS.CABS.CARRIER              CABSCARR*
      *               SYSIN   INSTREAM CONTROL CARD             (LOCAL)*
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               REVOUT  SYSOUT PRINT - REVENUE REPORT     CABSPRNT*
      *               REPORT  SYSOUT PRINT - RUN REGISTER       CABSPRNT*
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN                                      *
      * REVISION HISTORY                                              *
      *   V1.00  1989-04-11  L.HARGREAVES INITIAL RELEASE - CARRIER TOTAL ONLY*
      *   V1.05  1992-01-28  M.J.FERRARO  JURISDICTION SPLIT ADDED FOR THE*
      *                      SEPARATIONS FILING                       *
      *   V1.09  1995-08-15  D.OKONKWO    STATE LEVEL ADDED - THE MATRIX WENT*
      *                      FROM A TABLE TO A SEARCHED LIST          *
      *   V2.00  2001-06-19  P.NAIR       ROUNDED FIGURE RECOMPUTED FROM THE*
      *                      FIVE PLACE ACCUMULATOR AT EVERY          *
      *                      LEVEL RATHER THAN ADDED UP               *
      *   V2.08  2017-02-14  G.PRZYBYLSKI MATRIX RAISED TO 1500 ENTRIES*
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABRPT02.
       AUTHOR. TELCABS APPLICATIONS - BILLING CONTROL TEAM.
      *****************************************************************
      * ACCUMULATES BILLED REVENUE BY CARRIER, STATE AND JURISDICTION *
      * FOR THE GENERAL LEDGER AND THE SEPARATIONS FILING.            *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BILL-DTL-IN ASSIGN TO UT-S-BDTLIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT CARRIER-MASTER ASSIGN TO DA-I-CARRMST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CR-KEY
               FILE STATUS IS WS-FS-TABLE.
           SELECT REV-OUT-FILE ASSIGN TO UT-S-REVOUT
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
      * BDTLIN - BILL DETAIL, VARIABLE LENGTH.                        *
      *****************************************************************
       FD  BILL-DTL-IN
           RECORDING MODE IS V
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD IS VARYING IN SIZE FROM 108 TO 1647
               CHARACTERS DEPENDING ON BD-ELEM-CNT.
       COPY CABSBILL.
      *****************************************************************
      * CARRMST - CARRIER MASTER, READ RANDOMLY BY OCN.               *
      *****************************************************************
       FD  CARRIER-MASTER
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 200 CHARACTERS.
       COPY CABSCARR.
      *****************************************************************
      * REVOUT - THE REVENUE REPORT.                                  *
      *****************************************************************
       FD  REV-OUT-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       01  REV-RECORD                       PIC X(133).
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
           05  WS-PGM-NAME             PIC X(08) VALUE 'CABRPT02'.
           05  WS-PGM-VERSION          PIC X(05) VALUE 'V2.08'.
           05  WS-PGM-APPL             PIC X(04) VALUE 'CABS'.
           05  WS-PGM-COMPILE          PIC X(08) VALUE '20170214'.
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
           05  WS-PE-LEVEL             PIC X(01).
           05  WS-PE-MIN-REVENUE       PIC 9(09)V9(02).
           05  WS-PE-STATE-SEL         PIC X(02).
           05  WS-PE-JURIS-SEL         PIC X(01).
           05  WS-PE-FILLER            PIC X(21).
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
           05  WS-DTL-EOF-SW           PIC X(01) VALUE 'N'.
               88  WS-DTL-EOF          VALUE 'Y'.
           05  WS-CARR-FOUND-SW        PIC X(01) VALUE 'N'.
               88  WS-CARR-FOUND       VALUE 'Y'.
           05  WS-BUCKET-FOUND-SW      PIC X(01) VALUE 'N'.
               88  WS-BUCKET-FOUND     VALUE 'Y'.
      *****************************************************************
      * THE REVENUE MATRIX.  ONE BUCKET PER CARRIER, STATE AND        *
      * JURISDICTION.  THE MATRIX IS SPARSE - MOST CARRIERS APPEAR IN *
      * THREE OR FOUR STATES - SO IT IS HELD AS A LIST AND SEARCHED,  *
      * NOT AS A THREE DIMENSIONAL TABLE.                             *
      *****************************************************************
       01  WS-REV-TABLE.
           05  WS-RV-ENTRY OCCURS 1500 TIMES INDEXED BY WS-RV-X.
               10  WS-RV-OCN           PIC X(04).
               10  WS-RV-STATE         PIC X(02).
               10  WS-RV-JURIS         PIC X(01).
               10  WS-RV-RAW           PIC S9(15)V9(05) COMP-3.
               10  WS-RV-ROUNDED       PIC S9(15)V9(02) COMP-3.
               10  WS-RV-MINUTES       PIC S9(15)V9(02) COMP-3.
               10  WS-RV-LINES         PIC S9(09) COMP-3.
               10  WS-RV-ELEMENTS      PIC S9(11) COMP-3.
       01  WS-REV-CTL.
           05  WS-RV-USED              PIC S9(05) COMP-3 VALUE 0.
           05  WS-RV-MAX               PIC S9(05) COMP-3 VALUE 1500.
           05  WS-RV-HIT               PIC S9(05) COMP-3 VALUE 0.
      *****************************************************************
      * THE JURISDICTION NAMES FOR THE REPORT.                        *
      *****************************************************************
       01  WS-JURIS-NAMES.
           05  FILLER PIC X(13) VALUE 'IINTERSTATE  '.
           05  FILLER PIC X(13) VALUE 'SINTRASTATE  '.
           05  FILLER PIC X(13) VALUE 'LLOCAL       '.
           05  FILLER PIC X(13) VALUE 'TTRANSIT     '.
           05  FILLER PIC X(13) VALUE 'XINDETERMINED'.
       01  WS-JURIS-NAMES-R REDEFINES WS-JURIS-NAMES.
           05  WS-JN-ENTRY OCCURS 5 TIMES INDEXED BY WS-JN-X.
               10  WS-JN-CODE          PIC X(01).
               10  WS-JN-NAME          PIC X(12).
      *****************************************************************
      * ROLL UP WORK.  THE REPORT ROLLS UP AT THREE LEVELS - THE      *
      * JURISDICTION WITHIN THE STATE, THE STATE WITHIN THE CARRIER AND*
      * THE CARRIER WITHIN THE RUN.  THE ROUNDED FIGURE IS COMPUTED AT*
      * EACH LEVEL FROM THE FIVE PLACE ACCUMULATOR, NOT BY ADDING THE *
      * LOWER LEVEL ROUNDED FIGURES.                                  *
      * THE ROUNDING RULE IS SET BY CABS-STD-041.                     *
      *****************************************************************
       01  WS-ROLL-WORK.
           05  WS-RW-OCN               PIC X(04) VALUE SPACES.
           05  WS-RW-STATE             PIC X(02) VALUE SPACES.
           05  WS-RW-RAW               PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-RW-ROUNDED           PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-RW-STATE-RAW         PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-RW-STATE-ROUND       PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-RW-CARR-RAW          PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-RW-CARR-ROUND        PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-RW-NAME              PIC X(40) VALUE SPACES.
           05  WS-RW-JNAME             PIC X(12) VALUE SPACES.
       01  WS-RUN-TOTALS.
           05  WS-RT-LINES             PIC S9(11) COMP-3 VALUE 0.
           05  WS-RT-BUCKETS           PIC S9(05) COMP-3 VALUE 0.
           05  WS-RT-CARRIERS          PIC S9(05) COMP-3 VALUE 0.
           05  WS-RT-SUPPRESSED        PIC S9(05) COMP-3 VALUE 0.
           05  WS-RT-RAW               PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-RT-ROUNDED           PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-RT-JURIS OCCURS 5 TIMES
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
           OPEN INPUT  BILL-DTL-IN
                       CARRIER-MASTER
                       PARM-FILE
           OPEN OUTPUT REV-OUT-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 7111 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-BDTLIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 7112 TO WS-AB-CODE
               MOVE 'OPEN FAILED DA-I-CARRMST' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 7113 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-REVOUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 7114 TO WS-AB-CODE
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
           PERFORM P4600-CLEAR-JURIS THRU P4600-EXIT
               VARYING WS-SUB1 FROM 1 BY 1
               UNTIL WS-SUB1 > 5.
           PERFORM P6000-HEADING THRU P6000-EXIT.
           DISPLAY WS-PGM-NAME ' STARTED - RUN ' WS-RUN-ID.
           DISPLAY '  LEVEL         ' WS-PE-LEVEL.
           DISPLAY '  STATE SELECT  ' WS-PE-STATE-SEL.

       P1000-EXIT.
           EXIT.

       P1100-READ-PARM.
      * THE SYSIN CARD CARRIES THE VALUES THE SCHEDULER SUBSTITUTED INTO
      * THE JCL AT SUBMISSION TIME.  THERE ARE NO DEFAULTS - AN ABSENT
      * CARD IS A FATAL ERROR, NOT A DEFAULTED RUN.
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
      * EDIT THE CONTROL CARD.  EVERY FIELD IS MANDATORY.  THE 1989 CARD
      * FORMAT IS STILL ACCEPTED VIA THE WS-PARM-OLD REDEFINE.
      * THE REPORT LEVEL IS SUPPLIED BY THE SCHEDULER.  IT HAS NO
      * DEFAULT BECAUSE THE LEDGER FEED AND THE REGULATORY FILING
      * TAKE DIFFERENT LEVELS OF DETAIL FROM THE SAME PROGRAM.
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
           IF WS-PE-LEVEL = SPACE
               MOVE 7121 TO WS-AB-CODE
               MOVE 'REPORT LEVEL NOT SUPPLIED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PE-MIN-REVENUE NOT NUMERIC
               MOVE ZERO TO WS-PE-MIN-REVENUE.

       P1200-EXIT.
           EXIT.

      *****************************************************************
      * S200-MAIN PROCESSING                                          *
      *****************************************************************
       S200-MAIN-PROCESS SECTION.

       P2000-PROCESS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ-DETAIL THRU P2100-EXIT.
           IF WS-DTL-EOF
               MOVE 'Y' TO WS-EOF-SW
               GO TO P2000-EXIT.
           ADD 1 TO WS-READ-CNT.
           MOVE BD-BAN TO WS-RESTART-KEY.
           PERFORM P3000-SELECT-LINE THRU P3000-EXIT.
           IF WS-ERR-CODE NOT = SPACES
               MOVE SPACES TO WS-ERR-CODE
               ADD 1 TO WS-CFWD-CNT
               GO TO P2000-EXIT.
           PERFORM P3200-FIND-BUCKET THRU P3200-EXIT.
           PERFORM P3300-ACCUM-BUCKET THRU P3300-EXIT.
           ADD 1 TO WS-SUMM-CNT.
           ADD 1 TO WS-RT-LINES.

       P2000-EXIT.
           EXIT.

       P2100-READ-DETAIL.
           MOVE 'P2100-READ-DETAIL' TO WS-PARA-NAME.
           READ BILL-DTL-IN
               AT END
                   MOVE 'Y' TO WS-DTL-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00' AND WS-FS-INPUT NOT = '10'
               MOVE 7101 TO WS-AB-CODE
               MOVE 'BILL DETAIL READ ERROR' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.

       P2100-EXIT.
           EXIT.

      *****************************************************************
      * S300-SELECTION AND ACCUMULATION                               *
      *****************************************************************
       S300-ACCUM SECTION.

       P3000-SELECT-LINE.
      * THE STATE AND JURISDICTION SELECTORS ON THE CONTROL CARD LET
      * THE REGULATORY TEAM RUN THE REPORT FOR A SINGLE FILING.  A
      * BLANK SELECTOR MEANS EVERYTHING.
           MOVE 'P3000-SELECT-LINE' TO WS-PARA-NAME.
           MOVE SPACES TO WS-ERR-CODE.
           IF WS-PE-STATE-SEL NOT = SPACES
               IF BD-STATE-CD NOT = WS-PE-STATE-SEL
                   MOVE EC-JURIS-INDET TO WS-ERR-CODE
                   GO TO P3000-EXIT.
           IF WS-PE-JURIS-SEL NOT = SPACE
               IF BD-JURIS-CD NOT = WS-PE-JURIS-SEL
                   MOVE EC-JURIS-INDET TO WS-ERR-CODE.

       P3000-EXIT.
           EXIT.

       P3200-FIND-BUCKET.
           MOVE 'P3200-FIND-BUCKET' TO WS-PARA-NAME.
           MOVE 'N' TO WS-BUCKET-FOUND-SW.
           MOVE ZERO TO WS-RV-HIT.
           PERFORM P3210-MATCH-BUCKET THRU P3210-EXIT
               VARYING WS-RV-X FROM 1 BY 1
               UNTIL WS-RV-X > WS-RV-USED OR WS-BUCKET-FOUND.
           IF WS-BUCKET-FOUND
               GO TO P3200-EXIT.
           IF WS-RV-USED NOT < WS-RV-MAX
               MOVE 7102 TO WS-AB-CODE
               MOVE 'REVENUE MATRIX FULL' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-RV-USED.
           MOVE WS-RV-USED TO WS-RV-HIT.
           SET WS-RV-X TO WS-RV-USED.
           MOVE BD-OCN      TO WS-RV-OCN (WS-RV-X).
           MOVE BD-STATE-CD TO WS-RV-STATE (WS-RV-X).
           MOVE BD-JURIS-CD TO WS-RV-JURIS (WS-RV-X).
           MOVE ZERO TO WS-RV-RAW (WS-RV-X)
                        WS-RV-ROUNDED (WS-RV-X)
                        WS-RV-MINUTES (WS-RV-X)
                        WS-RV-LINES (WS-RV-X)
                        WS-RV-ELEMENTS (WS-RV-X).
           ADD 1 TO WS-RT-BUCKETS.

       P3200-EXIT.
           EXIT.

       P3210-MATCH-BUCKET.
           IF WS-RV-OCN (WS-RV-X) NOT = BD-OCN
               GO TO P3210-EXIT.
           IF WS-RV-STATE (WS-RV-X) NOT = BD-STATE-CD
               GO TO P3210-EXIT.
           IF WS-RV-JURIS (WS-RV-X) NOT = BD-JURIS-CD
               GO TO P3210-EXIT.
           SET WS-SUB1 TO WS-RV-X.
           MOVE WS-SUB1 TO WS-RV-HIT.
           MOVE 'Y' TO WS-BUCKET-FOUND-SW.

       P3210-EXIT.
           EXIT.

       P3300-ACCUM-BUCKET.
      * ACCUMULATE AT FIVE PLACES AND ROUND ONCE AT THE END.  THE
      * ROUNDED FIGURE IS RECOMPUTED FROM THE RAW ACCUMULATOR EVERY
      * TIME SO THAT IT NEVER DRIFTS FROM IT.
           MOVE 'P3300-ACCUM-BUCKET' TO WS-PARA-NAME.
           SET WS-RV-X TO WS-RV-HIT.
           ADD BD-TOT-AMOUNT  TO WS-RV-RAW (WS-RV-X).
           ADD BD-TOT-MINUTES TO WS-RV-MINUTES (WS-RV-X).
           ADD 1              TO WS-RV-LINES (WS-RV-X).
           ADD BD-ELEM-CNT    TO WS-RV-ELEMENTS (WS-RV-X).
           COMPUTE WS-RV-ROUNDED (WS-RV-X) ROUNDED =
                   WS-RV-RAW (WS-RV-X).
           ADD BD-TOT-AMOUNT TO WS-RT-RAW.
           ADD BD-TOT-AMOUNT TO WS-ACC-AMOUNT.
           ADD BD-TOT-MINUTES TO WS-ACC-MINUTES.

       P3300-EXIT.
           EXIT.

      *****************************************************************
      * S400-THE PRINTED REPORT                                       *
      *****************************************************************
       S400-REPORT SECTION.

       P4000-PRINT-REPORT.
      * THE REPORT PRINTS IN CARRIER, STATE, JURISDICTION ORDER.  THE
      * MATRIX IS NOT SORTED - IT IS WALKED ONCE PER CARRIER, WHICH IS
      * SLOWER BUT AVOIDS A SECOND SORT WORK FILE ON A JOB THAT ALREADY
      * HAS THREE.
           MOVE 'P4000-PRINT-REPORT' TO WS-PARA-NAME.
           PERFORM P4100-ONE-CARRIER THRU P4100-EXIT
               VARYING WS-RV-X FROM 1 BY 1
               UNTIL WS-RV-X > WS-RV-USED.
           PERFORM P4500-RUN-TOTAL THRU P4500-EXIT.

       P4000-EXIT.
           EXIT.

       P4100-ONE-CARRIER.
           IF WS-RV-OCN (WS-RV-X) = WS-RW-OCN
               GO TO P4100-EXIT.
           MOVE WS-RV-OCN (WS-RV-X) TO WS-RW-OCN.
           MOVE ZERO TO WS-RW-CARR-RAW.
           ADD 1 TO WS-RT-CARRIERS.
           PERFORM P4200-CARRIER-NAME THRU P4200-EXIT.
           PERFORM P4300-CARRIER-PAGE THRU P4300-EXIT.
           PERFORM P4400-CARRIER-LINES THRU P4400-EXIT
               VARYING WS-SUB2 FROM 1 BY 1
               UNTIL WS-SUB2 > WS-RV-USED.
           COMPUTE WS-RW-CARR-ROUND ROUNDED = WS-RW-CARR-RAW.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '0' TO PC-CC.
           MOVE 'CARRIER TOTAL' TO PC-COL-001-020.
           MOVE WS-RW-CARR-ROUND TO WS-ED-MONEY.
           MOVE WS-ED-MONEY TO PC-COL-091-132.
           PERFORM P5000-WRITE-LINE THRU P5000-EXIT.
           ADD WS-RW-CARR-ROUND TO WS-RT-ROUNDED.

       P4100-EXIT.
           EXIT.

       P4200-CARRIER-NAME.
           MOVE 'P4200-CARRIER-NAME' TO WS-PARA-NAME.
           MOVE 'N' TO WS-CARR-FOUND-SW.
           MOVE SPACES TO WS-RW-NAME.
           MOVE WS-RW-OCN TO CR-OCN.
           READ CARRIER-MASTER
               INVALID KEY
                   MOVE 'UNKNOWN CARRIER' TO WS-RW-NAME
                   GO TO P4200-EXIT.
           MOVE 'Y' TO WS-CARR-FOUND-SW.
           MOVE CR-NAME TO WS-RW-NAME.

       P4200-EXIT.
           EXIT.

       P4300-CARRIER-PAGE.
           MOVE 'P4300-CARRIER-PAGE' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '4' TO PC-CC.
           MOVE WS-RW-OCN  TO PC-COL-001-020.
           MOVE WS-RW-NAME TO PC-COL-021-060.
           PERFORM P5000-WRITE-LINE THRU P5000-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '0' TO PC-CC.
           MOVE 'STATE  JURISDICTION' TO PC-COL-001-020.
           MOVE 'LINES        MINUTES' TO PC-COL-021-060.
           MOVE 'ELEMENTS' TO PC-COL-061-090.
           MOVE 'REVENUE' TO PC-COL-091-132.
           PERFORM P5000-WRITE-LINE THRU P5000-EXIT.

       P4300-EXIT.
           EXIT.

       P4400-CARRIER-LINES.
           SET WS-RV-X TO WS-SUB2.
           IF WS-RV-OCN (WS-RV-X) NOT = WS-RW-OCN
               GO TO P4400-EXIT.
           IF WS-RV-ROUNDED (WS-RV-X) < WS-PE-MIN-REVENUE
               ADD 1 TO WS-RT-SUPPRESSED
               GO TO P4400-EXIT.
           PERFORM P4450-JURIS-NAME THRU P4450-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-RV-STATE (WS-RV-X) TO PC-COL-001-020.
           MOVE WS-RW-JNAME           TO PC-COL-021-060.
           MOVE WS-RV-MINUTES (WS-RV-X) TO WS-ED-MONEY.
           MOVE WS-ED-MONEY           TO PC-COL-061-090.
           MOVE WS-RV-ROUNDED (WS-RV-X) TO WS-ED-MONEY.
           MOVE WS-ED-MONEY           TO PC-COL-091-132.
           PERFORM P5000-WRITE-LINE THRU P5000-EXIT.
           ADD WS-RV-RAW (WS-RV-X) TO WS-RW-CARR-RAW.
           PERFORM P4460-JURIS-TOTAL THRU P4460-EXIT.

       P4400-EXIT.
           EXIT.

       P4450-JURIS-NAME.
           MOVE 'UNKNOWN' TO WS-RW-JNAME.
           PERFORM P4455-MATCH-JURIS THRU P4455-EXIT
               VARYING WS-JN-X FROM 1 BY 1
               UNTIL WS-JN-X > 5.

       P4450-EXIT.
           EXIT.

       P4455-MATCH-JURIS.
           IF WS-JN-CODE (WS-JN-X) = WS-RV-JURIS (WS-RV-X)
               MOVE WS-JN-NAME (WS-JN-X) TO WS-RW-JNAME.

       P4455-EXIT.
           EXIT.

       P4460-JURIS-TOTAL.
           PERFORM P4465-ADD-JURIS THRU P4465-EXIT
               VARYING WS-SUB3 FROM 1 BY 1
               UNTIL WS-SUB3 > 5.

       P4460-EXIT.
           EXIT.

       P4465-ADD-JURIS.
           SET WS-JN-X TO WS-SUB3.
           IF WS-JN-CODE (WS-JN-X) = WS-RV-JURIS (WS-RV-X)
               ADD WS-RV-ROUNDED (WS-RV-X)
                   TO WS-RT-JURIS (WS-SUB3).

       P4465-EXIT.
           EXIT.

       P4500-RUN-TOTAL.
           MOVE 'P4500-RUN-TOTAL' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '4' TO PC-CC.
           MOVE 'REVENUE BY JURISDICTION - ALL CARRIERS' TO PC-TEXT.
           PERFORM P5000-WRITE-LINE THRU P5000-EXIT.
           PERFORM P4510-JURIS-LINE THRU P4510-EXIT
               VARYING WS-SUB1 FROM 1 BY 1
               UNTIL WS-SUB1 > 5.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '-' TO PC-CC.
           MOVE 'RUN TOTAL' TO PC-COL-001-020.
           MOVE WS-RT-ROUNDED TO WS-ED-MONEY.
           MOVE WS-ED-MONEY TO PC-COL-091-132.
           PERFORM P5000-WRITE-LINE THRU P5000-EXIT.

       P4500-EXIT.
           EXIT.

       P4510-JURIS-LINE.
           SET WS-JN-X TO WS-SUB1.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-JN-NAME (WS-JN-X)  TO PC-COL-001-020.
           MOVE WS-RT-JURIS (WS-SUB1) TO WS-ED-MONEY.
           MOVE WS-ED-MONEY           TO PC-COL-091-132.
           PERFORM P5000-WRITE-LINE THRU P5000-EXIT.

       P4510-EXIT.
           EXIT.

       P5000-WRITE-LINE.
           WRITE REV-RECORD FROM CABS-PRINT-LINE.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 7103 TO WS-AB-CODE
               MOVE 'REVENUE REPORT WRITE FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-WRITE-CNT.

       P5000-EXIT.
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
           MOVE 'CABRPT02  REVENUE BY CARRIER AND JURISDICTION'
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
           MOVE 'OCN     BUCKETS      LINES         REVENUE'
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
           MOVE 605                    TO CT-STEP-SEQ.
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
           PERFORM P4000-PRINT-REPORT THRU P4000-EXIT.
           DISPLAY 'DETAIL LINES READ ' WS-RT-LINES.
           DISPLAY 'MATRIX BUCKETS    ' WS-RT-BUCKETS.
           DISPLAY 'CARRIERS REPORTED ' WS-RT-CARRIERS.
           DISPLAY 'BUCKETS SUPPRESSED' WS-RT-SUPPRESSED.
           DISPLAY 'RAW REVENUE 5DP   ' WS-RT-RAW.
           DISPLAY 'ROUNDED REVENUE   ' WS-RT-ROUNDED.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE BILL-DTL-IN
                 CARRIER-MASTER
                 REV-OUT-FILE
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

       P4600-CLEAR-JURIS.
           MOVE ZERO TO WS-RT-JURIS (WS-SUB1).

       P4600-EXIT.
           EXIT.
