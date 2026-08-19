      *****************************************************************
      * CABRPT06 - RATE ELEMENT USAGE STUDY                           *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               BDTLIN  TELCABS.CABS.BILLDTL.SEQ(0)       CABSBILL*
      *               SYSIN   INSTREAM CONTROL CARD             (LOCAL)*
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               STUDYEX TELCABS.CABS.STUDY.EXTRACT(+1)    (LOCAL)*
      *               STUDYRP SYSOUT PRINT - STUDY REPORT       CABSPRNT*
      *               REPORT  SYSOUT PRINT - RUN REGISTER       CABSPRNT*
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN                                      *
      * REVISION HISTORY                                              *
      *   V1.00  1992-05-19  L.HARGREAVES INITIAL RELEASE - COUNT BY ELEMENT*
      *   V1.04  1995-09-06  M.J.FERRARO  FLAT EXTRACT ADDED FOR THE ANALYST*
      *                      WORKSTATION - FOUR HUNDRED BYTES         *
      *   V1.08  2002-03-26  A.BUKOWSKI   STUDY LEVEL INTRODUCED SO THAT A*
      *                      SUMMARY RUN DOES NOT WALK EVERY          *
      *                      ELEMENT ON EVERY LINE                    *
      *   V2.00  2006-11-21  T.VANCE      RATE RANGE ADDED - THE TARIFF TEAM*
      *                      WANTED TO SEE AN ELEMENT BILLED AT       *
      *                      TWO DIFFERENT RATES IN ONE PERIOD        *
      *   V2.02  2011-04-19  G.PRZYBYLSKI ELEMENT TABLE RAISED TO 400 ENTRIES*
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABRPT06.
       AUTHOR. TELCABS APPLICATIONS - BILLING CONTROL TEAM.
      *****************************************************************
      * TALLIES EVERY RATE ELEMENT BILLED IN THE PERIOD AND PRODUCES  *
      * THE FLAT EXTRACT THE TARIFF ANALYSTS LOAD ONTO THE            *
      * WORKSTATION FOR THE ANNUAL RATE STUDY.                        *
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
           SELECT STUDY-EX-FILE ASSIGN TO UT-S-STUDYEX
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT STUDY-RP-FILE ASSIGN TO UT-S-STUDYRP
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-SUSPENSE.
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
      * STUDYEX - THE FLAT STUDY EXTRACT.  FOUR HUNDRED               *
      * BYTES FIXED, READ BY THE ANALYST WORKSTATION.                 *
      *****************************************************************
       FD  STUDY-EX-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 400 CHARACTERS.
       01  STUDY-RECORD                     PIC X(400).
      *****************************************************************
      * STUDYRP - THE PRINTED STUDY.                                  *
      *****************************************************************
       FD  STUDY-RP-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       01  STD-RECORD                       PIC X(133).
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
           05  WS-PGM-NAME             PIC X(08) VALUE 'CABRPT06'.
           05  WS-PGM-VERSION          PIC X(05) VALUE 'V2.02'.
           05  WS-PGM-APPL             PIC X(04) VALUE 'CABS'.
           05  WS-PGM-COMPILE          PIC X(08) VALUE '20110419'.
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
      * CARD LAYOUT FROZEN UNDER CABS-STD-014 SINCE THE 1994 REWRITE. *
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
           05  WS-PE-STUDY-LEVEL       PIC 9(01).
           05  WS-PE-MIN-COUNT         PIC 9(07).
           05  WS-PE-ELEM-SEL          PIC X(06).
           05  WS-PE-EXTRACT-SW        PIC X(01).
           05  WS-PE-FILLER            PIC X(20).
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
           05  WS-ELEM-FOUND-SW        PIC X(01) VALUE 'N'.
               88  WS-ELEM-FOUND       VALUE 'Y'.
      *****************************************************************
      * THE STUDY LEVEL.  THE TWO CONDITIONS BELOW WERE ADDED IN 1995 *
      * AND 2002 AND HAVE OVERLAPPED SINCE - A LEVEL OF FOUR SATISFIES*
      * BOTH.  P3000 TESTS THE DETAIL CONDITION FIRST.                *
      * THE 88 LEVELS ARE MAINTAINED WITH THE EDIT SPECIFICATION.     *
      *****************************************************************
       01  WS-STUDY-LEVEL-WORK.
           05  WS-RS-LEVEL             PIC 9(01) VALUE 0.
               88  WS-RS-DETAIL        VALUE 1 THRU 4.
               88  WS-RS-STUDY         VALUE 4 THRU 7.
               88  WS-RS-SUMMARY       VALUE 8 THRU 9.
               88  WS-RS-ANY-LEVEL     VALUE 1 THRU 9.
      *****************************************************************
      * THE RATE ELEMENT TALLY.  ONE ENTRY PER ELEMENT CODE SEEN, WITH*
      * THE COUNT, THE QUANTITY, THE REVENUE AND THE RANGE OF RATES IT*
      * WAS BILLED AT.  THE RATE RANGE IS WHAT THE TARIFF TEAM USE TO *
      * SPOT AN ELEMENT THAT IS BEING BILLED AT MORE THAN ONE RATE IN *
      * THE SAME PERIOD.                                              *
      *****************************************************************
       01  WS-STUDY-TABLE.
           05  WS-ST-ENTRY OCCURS 400 TIMES INDEXED BY WS-ST-X.
               10  WS-ST-ELEM          PIC X(06).
               10  WS-ST-COUNT         PIC S9(11) COMP-3.
               10  WS-ST-QTY           PIC S9(15)V9(02) COMP-3.
               10  WS-ST-AMOUNT        PIC S9(15)V9(05) COMP-3.
               10  WS-ST-MIN-RATE      PIC S9(05)V9(05) COMP-3.
               10  WS-ST-MAX-RATE      PIC S9(05)V9(05) COMP-3.
               10  WS-ST-LINES         PIC S9(09) COMP-3.
       01  WS-STUDY-CTL.
           05  WS-ST-USED              PIC S9(05) COMP-3 VALUE 0.
           05  WS-ST-MAX               PIC S9(05) COMP-3 VALUE 400.
           05  WS-ST-HIT               PIC S9(05) COMP-3 VALUE 0.
      *****************************************************************
      * THE STUDY EXTRACT AREA.  FOUR HUNDRED BYTES, FIXED.  THE      *
      * STATISTICS PACKAGE ON THE ANALYST WORKSTATION READS THIS      *
      * LAYOUT AND HAS DONE SINCE 1995.                               *
      *****************************************************************
       01  WS-STUDY-AREA               PIC X(400) VALUE SPACES.
       01  WS-STUDY-AREA-R REDEFINES WS-STUDY-AREA.
           05  WS-SA-BAN               PIC X(13).
           05  WS-SA-PERIOD            PIC 9(06).
           05  WS-SA-SECTION           PIC X(02).
           05  WS-SA-OCN               PIC X(04).
           05  WS-SA-JURIS             PIC X(01).
           05  WS-SA-STATE             PIC X(02).
           05  WS-SA-ELEM-CNT          PIC 9(03).
           05  WS-SA-BODY              PIC X(369).
       01  WS-ELEM-WALK.
           05  WS-EW-COUNT             PIC 9(03) VALUE 0.
           05  WS-EW-RATE              PIC S9(05)V9(05) COMP-3 VALUE 0.
       01  WS-RUN-TOTALS.
           05  WS-RT-LINES             PIC S9(11) COMP-3 VALUE 0.
           05  WS-RT-ELEMENTS          PIC S9(11) COMP-3 VALUE 0.
           05  WS-RT-CODES             PIC S9(05) COMP-3 VALUE 0.
           05  WS-RT-EXTRACTED         PIC S9(11) COMP-3 VALUE 0.
           05  WS-RT-SUPPRESSED        PIC S9(05) COMP-3 VALUE 0.
           05  WS-RT-AMOUNT            PIC S9(15)V9(05) COMP-3 VALUE 0.
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
                       PARM-FILE
           OPEN OUTPUT STUDY-EX-FILE
                       STUDY-RP-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 7511 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-BDTLIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 7512 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-STUDYEX' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 7513 TO WS-AB-CODE
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
           PERFORM P6000-HEADING THRU P6000-EXIT.
           DISPLAY WS-PGM-NAME ' STARTED - RUN ' WS-RUN-ID.
           DISPLAY '  STUDY LEVEL   ' WS-PE-STUDY-LEVEL.
           DISPLAY '  ELEMENT SELECT' WS-PE-ELEM-SEL.

       P1000-EXIT.
           EXIT.

       P1100-READ-PARM.
      * THE SYSIN CARD CARRIES THE VALUES THE SCHEDULER SUBSTITUTED INTO
      * THE JCL AT SUBMISSION TIME.  THERE ARE NO DEFAULTS - AN ABSENT
      * CARD IS A FATAL ERROR, NOT A DEFAULTED RUN.
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
      * EDIT THE CONTROL CARD.  EVERY FIELD IS MANDATORY.  THE 1989 CARD
      * FORMAT IS STILL ACCEPTED VIA THE WS-PARM-OLD REDEFINE.
      * THE STUDY LEVEL IS SUPPLIED BY THE ANALYST THROUGH THE
      * SCHEDULER.  IT HAS NO DEFAULT AND A ZERO IS FATAL.
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
           IF WS-PE-STUDY-LEVEL NOT NUMERIC
               MOVE 7521 TO WS-AB-CODE
               MOVE 'STUDY LEVEL NOT NUMERIC' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PE-STUDY-LEVEL = ZERO
               MOVE 7522 TO WS-AB-CODE
               MOVE 'STUDY LEVEL NOT SUPPLIED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PE-MIN-COUNT NOT NUMERIC
               MOVE ZERO TO WS-PE-MIN-COUNT.
           IF WS-PE-EXTRACT-SW NOT = 'N'
               MOVE 'Y' TO WS-PE-EXTRACT-SW.

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
           ADD 1 TO WS-RT-LINES.
           MOVE BD-BAN TO WS-RESTART-KEY.
           PERFORM P3000-STUDY-LEVEL THRU P3000-EXIT.
           PERFORM P3200-WALK-ELEMENTS THRU P3200-EXIT.
           IF WS-PE-EXTRACT-SW = 'Y'
               PERFORM P4000-BUILD-EXTRACT THRU P4000-EXIT.
           ADD 1 TO WS-SUMM-CNT.

       P2000-EXIT.
           EXIT.

       P2100-READ-DETAIL.
           MOVE 'P2100-READ-DETAIL' TO WS-PARA-NAME.
           READ BILL-DTL-IN
               AT END
                   MOVE 'Y' TO WS-DTL-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00' AND WS-FS-INPUT NOT = '10'
               MOVE 7501 TO WS-AB-CODE
               MOVE 'BILL DETAIL READ ERROR' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.

       P2100-EXIT.
           EXIT.

      *****************************************************************
      * S300-THE STUDY                                                *
      *****************************************************************
       S300-STUDY SECTION.

       P3000-STUDY-LEVEL.
      * THE STUDY LEVEL DECIDES HOW MUCH OF THE LINE IS TALLIED.  THE
      * DETAIL CONDITION IS TESTED FIRST.
           MOVE 'P3000-STUDY-LEVEL' TO WS-PARA-NAME.
           MOVE WS-PE-STUDY-LEVEL TO WS-RS-LEVEL.
           MOVE BD-ELEM-CNT TO WS-EW-COUNT.
           IF WS-EW-COUNT < 1
               MOVE 1 TO WS-EW-COUNT.
           IF WS-EW-COUNT > 40
               MOVE 40 TO WS-EW-COUNT.
           IF WS-RS-DETAIL
               GO TO P3000-EXIT.
           IF WS-RS-STUDY
               MOVE 1 TO WS-EW-COUNT
               GO TO P3000-EXIT.
           MOVE 1 TO WS-EW-COUNT.

       P3000-EXIT.
           EXIT.

       P3200-WALK-ELEMENTS.
      * WALK THE OCCURS DEPENDING ON AREA AND TALLY EACH RATE ELEMENT.
      * RECORD LENGTHS ARE HELD IN THE DATASET REGISTER.
           MOVE 'P3200-WALK-ELEMENTS' TO WS-PARA-NAME.
           PERFORM P3210-ONE-ELEMENT THRU P3210-EXIT
               VARYING WS-SUB1 FROM 1 BY 1
               UNTIL WS-SUB1 > WS-EW-COUNT.

       P3200-EXIT.
           EXIT.

       P3210-ONE-ELEMENT.
           SET BD-EX TO WS-SUB1.
           IF WS-PE-ELEM-SEL NOT = SPACES
               IF BD-EL-RATE-ELEM (BD-EX) NOT = WS-PE-ELEM-SEL
                   GO TO P3210-EXIT.
           PERFORM P3300-FIND-ELEM THRU P3300-EXIT.
           SET WS-ST-X TO WS-ST-HIT.
           ADD 1                     TO WS-ST-COUNT (WS-ST-X).
           ADD BD-EL-QTY (BD-EX)     TO WS-ST-QTY (WS-ST-X).
           ADD BD-EL-AMOUNT (BD-EX)  TO WS-ST-AMOUNT (WS-ST-X).
           ADD BD-EL-AMOUNT (BD-EX)  TO WS-RT-AMOUNT.
           ADD BD-EL-AMOUNT (BD-EX)  TO WS-ACC-AMOUNT.
           MOVE BD-EL-RATE (BD-EX)   TO WS-EW-RATE.
           IF WS-EW-RATE < WS-ST-MIN-RATE (WS-ST-X)
               MOVE WS-EW-RATE TO WS-ST-MIN-RATE (WS-ST-X).
           IF WS-EW-RATE > WS-ST-MAX-RATE (WS-ST-X)
               MOVE WS-EW-RATE TO WS-ST-MAX-RATE (WS-ST-X).
           ADD 1 TO WS-RT-ELEMENTS.

       P3210-EXIT.
           EXIT.

       P3300-FIND-ELEM.
           MOVE 'N' TO WS-ELEM-FOUND-SW.
           MOVE ZERO TO WS-ST-HIT.
           PERFORM P3310-MATCH-ELEM THRU P3310-EXIT
               VARYING WS-ST-X FROM 1 BY 1
               UNTIL WS-ST-X > WS-ST-USED OR WS-ELEM-FOUND.
           IF WS-ELEM-FOUND
               GO TO P3300-EXIT.
           IF WS-ST-USED NOT < WS-ST-MAX
               MOVE WS-ST-MAX TO WS-ST-HIT
               GO TO P3300-EXIT.
           ADD 1 TO WS-ST-USED.
           MOVE WS-ST-USED TO WS-ST-HIT.
           SET WS-ST-X TO WS-ST-USED.
           MOVE BD-EL-RATE-ELEM (BD-EX) TO WS-ST-ELEM (WS-ST-X).
           MOVE ZERO TO WS-ST-COUNT (WS-ST-X)
                        WS-ST-QTY (WS-ST-X)
                        WS-ST-AMOUNT (WS-ST-X)
                        WS-ST-MAX-RATE (WS-ST-X)
                        WS-ST-LINES (WS-ST-X).
           MOVE 99999.99999 TO WS-ST-MIN-RATE (WS-ST-X).
           ADD 1 TO WS-RT-CODES.

       P3300-EXIT.
           EXIT.

       P3310-MATCH-ELEM.
           IF WS-ST-ELEM (WS-ST-X) = BD-EL-RATE-ELEM (BD-EX)
               SET WS-SUB2 TO WS-ST-X
               MOVE WS-SUB2 TO WS-ST-HIT
               MOVE 'Y' TO WS-ELEM-FOUND-SW.

       P3310-EXIT.
           EXIT.

      *****************************************************************
      * S400-THE STUDY EXTRACT                                        *
      *****************************************************************
       S400-EXTRACT SECTION.

       P4000-BUILD-EXTRACT.
      * THE STUDY EXTRACT.  THE ANALYST WORKSTATION READS A FLAT FOUR
      * HUNDRED BYTE RECORD, SO THE BILL DETAIL RECORD IS MOVED INTO
      * THE STUDY AREA AND WRITTEN OUT.  THE KEY FIELDS ARE THEN
      * RESTATED FROM THE RECORD SO THAT THE EXTRACT CARRIES THEM IN
      * THE POSITIONS THE WORKSTATION EXPECTS.
           MOVE 'P4000-BUILD-EXTRACT' TO WS-PARA-NAME.
           MOVE SPACES TO WS-STUDY-AREA.
           MOVE CABS-BILL-DETAIL TO WS-STUDY-AREA.
           MOVE BD-BAN         TO WS-SA-BAN.
           MOVE BD-BILL-PERIOD TO WS-SA-PERIOD.
           MOVE BD-SECTION     TO WS-SA-SECTION.
           MOVE BD-OCN         TO WS-SA-OCN.
           MOVE BD-JURIS-CD    TO WS-SA-JURIS.
           MOVE BD-STATE-CD    TO WS-SA-STATE.
           MOVE BD-ELEM-CNT    TO WS-SA-ELEM-CNT.
           WRITE STUDY-RECORD FROM WS-STUDY-AREA.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 7502 TO WS-AB-CODE
               MOVE 'STUDY EXTRACT WRITE FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-WRITE-CNT.
           ADD 1 TO WS-RT-EXTRACTED.

       P4000-EXIT.
           EXIT.

      *****************************************************************
      * S500-REPORT                                                   *
      *****************************************************************
       S500-REPORT SECTION.

       P5000-PRINT-STUDY.
           MOVE 'P5000-PRINT-STUDY' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '4' TO PC-CC.
           MOVE 'RATE ELEMENT USAGE STUDY' TO PC-TEXT.
           PERFORM P5200-WRITE-LINE THRU P5200-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '0' TO PC-CC.
           MOVE 'ELEMENT' TO PC-COL-001-020.
           MOVE 'OCCURRENCES    QUANTITY' TO PC-COL-021-060.
           MOVE 'MIN RATE   MAX RATE' TO PC-COL-061-090.
           MOVE 'REVENUE' TO PC-COL-091-132.
           PERFORM P5200-WRITE-LINE THRU P5200-EXIT.
           PERFORM P5100-STUDY-LINE THRU P5100-EXIT
               VARYING WS-ST-X FROM 1 BY 1
               UNTIL WS-ST-X > WS-ST-USED.

       P5000-EXIT.
           EXIT.

       P5100-STUDY-LINE.
           SET WS-SUB1 TO WS-ST-X.
           IF WS-ST-COUNT (WS-ST-X) < WS-PE-MIN-COUNT
               ADD 1 TO WS-RT-SUPPRESSED
               GO TO P5100-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-ST-ELEM (WS-ST-X)     TO PC-COL-001-020.
           MOVE WS-ST-COUNT (WS-ST-X) TO WS-ED-COUNT.
           MOVE WS-ED-COUNT              TO PC-COL-021-060.
           MOVE WS-ST-MIN-RATE (WS-ST-X) TO WS-ED-RATE.
           MOVE WS-ED-RATE               TO PC-COL-061-090.
           MOVE WS-ST-AMOUNT (WS-ST-X)   TO WS-ED-MONEY.
           MOVE WS-ED-MONEY              TO PC-COL-091-132.
           PERFORM P5200-WRITE-LINE THRU P5200-EXIT.

       P5100-EXIT.
           EXIT.

       P5200-WRITE-LINE.
           WRITE STD-RECORD FROM CABS-PRINT-LINE.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 7503 TO WS-AB-CODE
               MOVE 'STUDY REPORT WRITE FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.

       P5200-EXIT.
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
           MOVE 'CABRPT06  RATE ELEMENT USAGE STUDY'
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
           MOVE 'ELEMENT   OCCURRENCES    RATE RANGE      REVENUE'
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
           MOVE 625                    TO CT-STEP-SEQ.
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
           PERFORM P5000-PRINT-STUDY THRU P5000-EXIT.
           DISPLAY 'DETAIL LINES READ ' WS-RT-LINES.
           DISPLAY 'ELEMENTS TALLIED  ' WS-RT-ELEMENTS.
           DISPLAY 'DISTINCT CODES    ' WS-RT-CODES.
           DISPLAY 'EXTRACT RECORDS   ' WS-RT-EXTRACTED.
           DISPLAY 'CODES SUPPRESSED  ' WS-RT-SUPPRESSED.
           DISPLAY 'STUDY REVENUE     ' WS-RT-AMOUNT.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE BILL-DTL-IN
                 STUDY-EX-FILE
                 STUDY-RP-FILE
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

