      *****************************************************************
      * CABBIL09 - INVOICE HEADER CREATION AND JURISDICTION SPLIT     *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               BDTLIN  TELCABS.CABS.BILLDTL.SEQ(0)       CABSBILL*
      *               BHDRIN  TELCABS.CABS.BILLHDR.SET(0)       CABSBHDR*
      *               SYSIN   INSTREAM CONTROL CARD             (LOCAL)*
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               BHDROUT TELCABS.CABS.BILLHDR.USG(+1)      CABSBHDR*
      *               REPORT  SYSOUT PRINT                      CABSPRNT*
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-SUMMARISED + CT-CARRIED-FWD        *
      *               CT-WRITTEN IS THE HEADER COUNT, NOT THE DETAIL COUNT*
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY (BAN)           *
      * REVISION HISTORY                                              *
      *   V1.00  1988-10-04  R.T.WHEELER  INITIAL RELEASE - USAGE TOTAL ONLY*
      *   V1.05  1991-09-12  M.J.FERRARO  THREE CURRENT PERIOD FIELDS SPLIT*
      *                      OUT FROM THE SINGLE USAGE TOTAL          *
      *   V1.09  1995-01-23  L.HARGREAVES JURISDICTIONAL SPLIT ADDED FOR THE*
      *                      SEPARATIONS FILING                       *
      *   V1.13  1997-06-06  J.M.CASTILLO HASH AMOUNT CARRIED AT FIVE PLACES*
      *                      SO THE BALANCING STEP CAN CHECK IT       *
      *   V2.00  2000-02-28  P.NAIR       SECTION TO CLASS MAP TABLE ADDED -*
      *                      THE MAPPING WAS AN IF NEST BEFORE        *
      *   V2.06  2005-08-30  A.BUKOWSKI   INDETERMINATE JURISDICTION NOW*
      *                      DEFAULTS TO INTERSTATE PER TARIFF        *
      *   V2.09  2012-03-19  R.KAMINSKI   HEADER TABLE RAISED TO 2000 ENTRIES*
      *   V2.12  2017-09-28  G.PRZYBYLSKI CDR COUNT FACTOR ACCEPTED FROM THE*
      *                      CONTROL CARD FOR THE REGULATORY FEED     *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABBIL09.
       AUTHOR. TELCABS APPLICATIONS - BILLING TEAM.
      *****************************************************************
      * ACCUMULATES THE BILL DETAIL INTO THE INVOICE HEADER, DERIVES  *
      * THE JURISDICTIONAL SPLIT AND SETS THE CONTROL FIELDS THAT THE *
      * BALANCING STEP LATER CHECKS.                                  *
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
           SELECT BHDR-IN-FILE ASSIGN TO UT-S-BHDRIN
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
      * BDTLIN - SEQUENCED BILL DETAIL, VARIABLE LENGTH.              *
      *****************************************************************
       FD  BILL-DTL-IN
           RECORDING MODE IS V
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD IS VARYING IN SIZE FROM 108 TO 1647
               CHARACTERS DEPENDING ON BD-ELEM-CNT.
       COPY CABSBILL.
      *****************************************************************
      * BHDRIN - BILL HEADER WITH SETTLEMENT POSTED.                  *
      *****************************************************************
       FD  BHDR-IN-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 400 CHARACTERS.
       01  BHDR-IN-REC                      PIC X(400).
      *****************************************************************
      * BHDROUT - THE COMPLETED INVOICE HEADER.                       *
      *****************************************************************
       FD  BHDR-OUT-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 400 CHARACTERS.
       01  BHDR-OUT-REC                     PIC X(400).
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
           05  WS-PGM-NAME             PIC X(08) VALUE 'CABBIL09'.
           05  WS-PGM-VERSION          PIC X(05) VALUE 'V2.12'.
           05  WS-PGM-APPL             PIC X(04) VALUE 'CABS'.
           05  WS-PGM-COMPILE          PIC X(08) VALUE '20170928'.
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
           05  WS-PE-HASH-SW           PIC X(01).
           05  WS-PE-JURIS-SW          PIC X(01).
           05  WS-PE-CDR-FACTOR        PIC 9(05).
           05  WS-PE-SECT-CLASS        PIC X(01).
           05  WS-PE-FILLER            PIC X(27).
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
           05  WS-HDR-EOF-SW           PIC X(01) VALUE 'N'.
               88  WS-HDR-EOF          VALUE 'Y'.
           05  WS-ACCT-OPEN-SW         PIC X(01) VALUE 'N'.
               88  WS-ACCT-OPEN        VALUE 'Y'.
           05  WS-HDR-MATCH-SW         PIC X(01) VALUE 'N'.
               88  WS-HDR-MATCH        VALUE 'Y'.
      *****************************************************************
      * THE PER ACCOUNT ACCUMULATORS.  EVERYTHING IS ACCUMULATED AT FIVE*
      * DECIMAL PLACES AND MOVED INTO THE TWO PLACE HEADER FIELDS AT  *
      * THE BREAK.  THE MOVE TRUNCATES.  CABBIL02 ROUNDED THE SAME    *
      * QUANTITY WHEN IT BUILT THE DETAIL LINE.                       *
      * THE ROUNDING RULE IS SET BY CABS-STD-041.                     *
      *****************************************************************
       01  WS-ACCT-ACCUM.
           05  WS-AA-BAN               PIC X(13) VALUE SPACES.
           05  WS-AA-PERIOD            PIC 9(06) VALUE 0.
           05  WS-AA-OCN               PIC X(04) VALUE SPACES.
           05  WS-AA-USAGE             PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-AA-RECURRING         PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-AA-NONRECUR          PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-AA-SETTLEMENT        PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-AA-ADJUSTMENT        PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-AA-HASH              PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-AA-MINUTES           PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-AA-LINES             PIC S9(07) COMP-3 VALUE 0.
           05  WS-AA-ELEMENTS          PIC S9(11) COMP-3 VALUE 0.
      *****************************************************************
      * THE JURISDICTIONAL SPLIT.  THREE BUCKETS, KEYED FROM THE      *
      * JURISDICTION CODE ON THE DETAIL LINE.  ANYTHING THAT IS NOT ONE*
      * OF THE THREE IS COUNTED AS INTERSTATE - THE FILED TARIFF      *
      * DEFAULTS AN INDETERMINATE MINUTE TO INTERSTATE.               *
      *****************************************************************
       01  WS-JURIS-SPLIT.
           05  WS-JS-INTER             PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-JS-INTRA             PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-JS-LOCAL             PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-JS-INDET             PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-JS-CHECK             PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-JS-VARIANCE          PIC S9(11)V9(05) COMP-3 VALUE 0.
      *****************************************************************
      * SECTION CLASS TO HEADER FIELD MAPPING.  THE SECTION CODE ON THE*
      * DETAIL LINE DECIDES WHICH OF THE THREE CURRENT PERIOD FIELDS  *
      * THE AMOUNT LANDS IN.  THE MAPPING IS HELD HERE AND NOWHERE ELSE.*
      *****************************************************************
       01  WS-CLASS-MAP-TABLE.
           05  FILLER PIC X(03) VALUE 'U1U'.
           05  FILLER PIC X(03) VALUE 'U2U'.
           05  FILLER PIC X(03) VALUE 'U3U'.
           05  FILLER PIC X(03) VALUE 'C1R'.
           05  FILLER PIC X(03) VALUE 'C2N'.
           05  FILLER PIC X(03) VALUE 'C3R'.
           05  FILLER PIC X(03) VALUE 'C4R'.
           05  FILLER PIC X(03) VALUE 'S1S'.
           05  FILLER PIC X(03) VALUE 'S2S'.
           05  FILLER PIC X(03) VALUE 'A1A'.
           05  FILLER PIC X(03) VALUE 'T1T'.
           05  FILLER PIC X(03) VALUE 'Z1N'.
       01  WS-CLASS-MAP-R REDEFINES WS-CLASS-MAP-TABLE.
           05  WS-CM-ENTRY OCCURS 12 TIMES INDEXED BY WS-CM-X.
               10  WS-CM-SECTION       PIC X(02).
               10  WS-CM-CLASS         PIC X(01).
       01  WS-CLASS-WORK.
           05  WS-CW-CLASS             PIC X(01) VALUE SPACES.
           05  WS-CW-FOUND-SW          PIC X(01) VALUE 'N'.
               88  WS-CW-FOUND         VALUE 'Y'.
           05  WS-CW-COUNT             PIC 9(03) VALUE 0.
      *****************************************************************
      * THE HEADER TABLE.  HEADERS ARE HELD IN STORAGE SO THAT THE    *
      * DETAIL FILE CAN BE READ ONCE AND MATCHED TO THEM.             *
      *****************************************************************
       01  WS-HDR-TABLE.
           05  WS-HT-ENTRY OCCURS 2000 TIMES INDEXED BY WS-HT-X.
               10  WS-HT-BAN           PIC X(13).
               10  WS-HT-IMAGE         PIC X(400).
       01  WS-HDR-CTL.
           05  WS-HT-USED              PIC S9(05) COMP-3 VALUE 0.
           05  WS-HT-MAX               PIC S9(05) COMP-3 VALUE 2000.
           05  WS-HT-HIT               PIC S9(05) COMP-3 VALUE 0.
       01  WS-RUN-TOTALS.
           05  WS-RT-HEADERS           PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-DETAILS           PIC S9(11) COMP-3 VALUE 0.
           05  WS-RT-ORPHANS           PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-USAGE             PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-RT-INTER             PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-RT-INTRA             PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-RT-LOCAL             PIC S9(15)V9(02) COMP-3 VALUE 0.
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
                       BHDR-IN-FILE
                       PARM-FILE
           OPEN OUTPUT BHDR-OUT-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 4151 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-BDTLIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 4152 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-BHDRIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 4153 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-BHDROUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 4154 TO WS-AB-CODE
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
           PERFORM P4500-LOAD-HEADERS THRU P4500-EXIT.
           PERFORM P6000-HEADING THRU P6000-EXIT.
           DISPLAY WS-PGM-NAME ' STARTED - RUN ' WS-RUN-ID.
           DISPLAY '  JURIS SPLIT   ' WS-PE-JURIS-SW.
           DISPLAY '  CDR FACTOR    ' WS-PE-CDR-FACTOR.

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
      * THE CDR COUNT FACTOR IS SUPPLIED BY THE SCHEDULER FROM THE
      * CURRENT REGULATORY FILING.  A ZERO MEANS USE THE RAW ELEMENT
      * COUNT, NOT A COUNT OF ZERO.
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
           IF WS-PE-JURIS-SW NOT = 'N'
               MOVE 'Y' TO WS-PE-JURIS-SW.
           IF WS-PE-HASH-SW NOT = 'N'
               MOVE 'Y' TO WS-PE-HASH-SW.
           IF WS-PE-CDR-FACTOR NOT NUMERIC
               MOVE ZERO TO WS-PE-CDR-FACTOR.

       P1200-EXIT.
           EXIT.

      *****************************************************************
      * S200-MAIN PROCESSING                                          *
      * ONE PASS OF THE SEQUENCED BILL DETAIL FILE.  THE FILE IS IN BAN*
      * ORDER SO THE ACCOUNT BREAK IS A SIMPLE CHANGE OF KEY.         *
      *****************************************************************
       S200-MAIN-PROCESS SECTION.

       P2000-PROCESS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ-DETAIL THRU P2100-EXIT.
           IF WS-DTL-EOF
               PERFORM P4000-CLOSE-ACCOUNT THRU P4000-EXIT
               MOVE 'Y' TO WS-EOF-SW
               GO TO P2000-EXIT.
           ADD 1 TO WS-READ-CNT.
           MOVE BD-BAN TO WS-RESTART-KEY.
           IF WS-ACCT-OPEN
               IF BD-BAN NOT = WS-AA-BAN
                   PERFORM P4000-CLOSE-ACCOUNT THRU P4000-EXIT.
           IF NOT WS-ACCT-OPEN
               PERFORM P3000-OPEN-ACCOUNT THRU P3000-EXIT.
           PERFORM P3100-CLASSIFY-LINE THRU P3100-EXIT.
           PERFORM P3200-ACCUM-AMOUNT THRU P3200-EXIT.
           PERFORM P3300-ACCUM-JURIS THRU P3300-EXIT.
           PERFORM P3400-WALK-ELEMENTS THRU P3400-EXIT.
           ADD 1 TO WS-SUMM-CNT.
           ADD 1 TO WS-RT-DETAILS.

       P2000-EXIT.
           EXIT.

       P2100-READ-DETAIL.
           MOVE 'P2100-READ-DETAIL' TO WS-PARA-NAME.
           READ BILL-DTL-IN
               AT END
                   MOVE 'Y' TO WS-DTL-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00' AND WS-FS-INPUT NOT = '10'
               MOVE 4901 TO WS-AB-CODE
               MOVE 'BILL DETAIL READ ERROR' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.

       P2100-EXIT.
           EXIT.

      *****************************************************************
      * S300-ACCUMULATION                                             *
      *****************************************************************
       S300-ACCUM SECTION.

       P3000-OPEN-ACCOUNT.
           MOVE 'P3000-OPEN-ACCOUNT' TO WS-PARA-NAME.
           MOVE BD-BAN                 TO WS-AA-BAN.
           MOVE BD-BILL-PERIOD         TO WS-AA-PERIOD.
           MOVE BD-OCN                 TO WS-AA-OCN.
           MOVE ZERO TO WS-AA-USAGE WS-AA-RECURRING
                        WS-AA-NONRECUR WS-AA-SETTLEMENT
                        WS-AA-ADJUSTMENT WS-AA-HASH
                        WS-AA-MINUTES WS-AA-LINES WS-AA-ELEMENTS.
           MOVE ZERO TO WS-JS-INTER WS-JS-INTRA WS-JS-LOCAL
                        WS-JS-INDET.
           MOVE 'Y' TO WS-ACCT-OPEN-SW.

       P3000-EXIT.
           EXIT.

       P3100-CLASSIFY-LINE.
      * RESOLVE THE SECTION CODE TO ITS HEADER CLASS.  A SECTION THAT
      * IS NOT IN THE MAP IS TREATED AS NON RECURRING, WHICH IS WHERE
      * THE UNCLASSIFIED SECTION LANDS.
           MOVE 'P3100-CLASSIFY-LINE' TO WS-PARA-NAME.
           MOVE 'N' TO WS-CW-FOUND-SW.
           MOVE 'N' TO WS-CW-CLASS.
           PERFORM P3110-MATCH-CLASS THRU P3110-EXIT
               VARYING WS-CM-X FROM 1 BY 1
               UNTIL WS-CM-X > 12 OR WS-CW-FOUND.

       P3100-EXIT.
           EXIT.

       P3110-MATCH-CLASS.
           IF WS-CM-SECTION (WS-CM-X) = BD-SECTION
               MOVE WS-CM-CLASS (WS-CM-X) TO WS-CW-CLASS
               MOVE 'Y' TO WS-CW-FOUND-SW.

       P3110-EXIT.
           EXIT.

       P3200-ACCUM-AMOUNT.
      * ACCUMULATE THE LINE INTO THE RIGHT CURRENT PERIOD BUCKET.  THE
      * HASH ACCUMULATOR TAKES THE UNROUNDED FIVE PLACE AMOUNT FROM THE
      * DETAIL LINE - IT IS THE CONTROL FIGURE THE BALANCING STEP USES.
           MOVE 'P3200-ACCUM-AMOUNT' TO WS-PARA-NAME.
           IF WS-CW-CLASS = 'U'
               ADD BD-TOT-AMOUNT TO WS-AA-USAGE.
           IF WS-CW-CLASS = 'R'
               ADD BD-TOT-AMOUNT TO WS-AA-RECURRING.
           IF WS-CW-CLASS = 'N'
               ADD BD-TOT-AMOUNT TO WS-AA-NONRECUR.
           IF WS-CW-CLASS = 'S'
               ADD BD-TOT-AMOUNT TO WS-AA-SETTLEMENT.
           IF WS-CW-CLASS = 'A'
               ADD BD-TOT-AMOUNT TO WS-AA-ADJUSTMENT.
           ADD BD-TOT-AMOUNT  TO WS-AA-HASH.
           ADD BD-TOT-MINUTES TO WS-AA-MINUTES.
           ADD 1 TO WS-AA-LINES.
           ADD BD-TOT-AMOUNT TO WS-ACC-AMOUNT.
           ADD BD-TOT-MINUTES TO WS-ACC-MINUTES.

       P3200-EXIT.
           EXIT.

       P3300-ACCUM-JURIS.
      * THE JURISDICTIONAL SPLIT.  THE THREE BUCKETS MUST ADD BACK TO
      * THE HASH TOTAL - THE VARIANCE IS CHECKED AT THE BREAK.
           MOVE 'P3300-ACCUM-JURIS' TO WS-PARA-NAME.
           IF WS-PE-JURIS-SW NOT = 'Y'
               GO TO P3300-EXIT.
           IF BD-JURIS-CD = 'I'
               ADD BD-TOT-AMOUNT TO WS-JS-INTER
               GO TO P3300-EXIT.
           IF BD-JURIS-CD = 'S'
               ADD BD-TOT-AMOUNT TO WS-JS-INTRA
               GO TO P3300-EXIT.
           IF BD-JURIS-CD = 'L'
               ADD BD-TOT-AMOUNT TO WS-JS-LOCAL
               GO TO P3300-EXIT.
           ADD BD-TOT-AMOUNT TO WS-JS-INDET.
           ADD BD-TOT-AMOUNT TO WS-JS-INTER.

       P3300-EXIT.
           EXIT.

       P3400-WALK-ELEMENTS.
      * COUNT THE RATE ELEMENTS ON THE LINE.  THE ELEMENT COUNT IS THE
      * NEAREST THING THE HEADER HAS TO A CALL DETAIL COUNT AND IT IS
      * WHAT THE CDR COUNT FIELD IS DERIVED FROM.
           MOVE 'P3400-WALK-ELEMENTS' TO WS-PARA-NAME.
           MOVE BD-ELEM-CNT TO WS-CW-COUNT.
           IF WS-CW-COUNT < 1
               MOVE 1 TO WS-CW-COUNT.
           IF WS-CW-COUNT > 40
               MOVE 40 TO WS-CW-COUNT.
           ADD WS-CW-COUNT TO WS-AA-ELEMENTS.

       P3400-EXIT.
           EXIT.

      *****************************************************************
      * S400-ACCOUNT BREAK                                            *
      *****************************************************************
       S400-BREAK SECTION.

       P4000-CLOSE-ACCOUNT.
      * THE ACCOUNT BREAK.  MATCH THE ACCUMULATED TOTALS TO THE HEADER
      * HELD IN STORAGE AND WRITE THE COMPLETED HEADER OUT.
           MOVE 'P4000-CLOSE-ACCOUNT' TO WS-PARA-NAME.
           IF NOT WS-ACCT-OPEN
               GO TO P4000-EXIT.
           PERFORM P4100-FIND-HEADER THRU P4100-EXIT.
           IF NOT WS-HDR-MATCH
               ADD 1 TO WS-RT-ORPHANS
               ADD 1 TO WS-CFWD-CNT
               MOVE 'N' TO WS-ACCT-OPEN-SW
               GO TO P4000-EXIT.
           PERFORM P4200-POST-TOTALS THRU P4200-EXIT.
           PERFORM P4300-POST-JURIS THRU P4300-EXIT.
           PERFORM P4400-WRITE-HEADER THRU P4400-EXIT.
           MOVE 'N' TO WS-ACCT-OPEN-SW.

       P4000-EXIT.
           EXIT.

       P4100-FIND-HEADER.
           MOVE 'P4100-FIND-HEADER' TO WS-PARA-NAME.
           MOVE 'N' TO WS-HDR-MATCH-SW.
           MOVE ZERO TO WS-HT-HIT.
           PERFORM P4110-MATCH-HDR THRU P4110-EXIT
               VARYING WS-HT-X FROM 1 BY 1
               UNTIL WS-HT-X > WS-HT-USED OR WS-HDR-MATCH.
           IF WS-HDR-MATCH
               SET WS-HT-X TO WS-HT-HIT
               MOVE WS-HT-IMAGE (WS-HT-X) TO CABS-BILL-HEADER.

       P4100-EXIT.
           EXIT.

       P4110-MATCH-HDR.
           IF WS-HT-BAN (WS-HT-X) = WS-AA-BAN
               SET WS-SUB1 TO WS-HT-X
               MOVE WS-SUB1 TO WS-HT-HIT
               MOVE 'Y' TO WS-HDR-MATCH-SW.

       P4110-EXIT.
           EXIT.

       P4200-POST-TOTALS.
      * MOVE THE FIVE PLACE ACCUMULATORS INTO THE TWO PLACE HEADER
      * FIELDS.  THE HASH FIELD KEEPS ALL FIVE PLACES - IT IS THE ONLY
      * FIELD ON THE HEADER THAT DOES.
           MOVE 'P4200-POST-TOTALS' TO WS-PARA-NAME.
           MOVE WS-AA-USAGE        TO BH-CURR-USAGE.
           MOVE WS-AA-RECURRING    TO BH-CURR-RECURRING.
           MOVE WS-AA-NONRECUR     TO BH-CURR-NONRECUR.
           MOVE WS-AA-LINES        TO BH-DETAIL-LINES.
           MOVE WS-AA-HASH         TO BH-HASH-AMOUNT.
           IF WS-PE-CDR-FACTOR = ZERO
               MOVE WS-AA-ELEMENTS TO BH-CDR-COUNT
           ELSE
               COMPUTE BH-CDR-COUNT =
                       WS-AA-ELEMENTS * WS-PE-CDR-FACTOR.
           ADD BH-CURR-USAGE TO WS-RT-USAGE.
           ADD 1 TO WS-RT-HEADERS.

       P4200-EXIT.
           EXIT.

       P4300-POST-JURIS.
      * POST THE JURISDICTIONAL SPLIT AND CHECK THAT THE THREE BUCKETS
      * ADD BACK TO THE HASH TOTAL.  A VARIANCE IS SUSPENDED BUT DOES
      * NOT STOP THE BILL.
           MOVE 'P4300-POST-JURIS' TO WS-PARA-NAME.
           MOVE WS-JS-INTER TO BH-INTERSTATE-AMT.
           MOVE WS-JS-INTRA TO BH-INTRASTATE-AMT.
           MOVE WS-JS-LOCAL TO BH-LOCAL-AMT.
           COMPUTE WS-JS-CHECK =
                   WS-JS-INTER + WS-JS-INTRA + WS-JS-LOCAL.
           COMPUTE WS-JS-VARIANCE = WS-AA-HASH - WS-JS-CHECK.
           IF WS-JS-VARIANCE NOT = ZERO
               MOVE EC-JURIS-INDET TO WS-ERR-CODE
               MOVE 'W' TO WS-ERR-SEVERITY
               MOVE CABS-BILL-HEADER TO WS-EW-DATA
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               SUBTRACT 1 FROM WS-REJECT-CNT
               MOVE SPACES TO WS-ERR-CODE.
           ADD BH-INTERSTATE-AMT TO WS-RT-INTER.
           ADD BH-INTRASTATE-AMT TO WS-RT-INTRA.
           ADD BH-LOCAL-AMT      TO WS-RT-LOCAL.

       P4300-EXIT.
           EXIT.

       P4400-WRITE-HEADER.
           MOVE 'P4400-WRITE-HEADER' TO WS-PARA-NAME.
           COMPUTE BH-TOTAL-DUE =
                   BH-PRIOR-BAL - BH-PAYMENTS + BH-ADJUSTMENTS
                 + BH-CURR-USAGE + BH-CURR-RECURRING
                 + BH-CURR-NONRECUR + BH-RESTATEMENT
                 + BH-SETTLEMENT-NET + BH-TAX.
           WRITE BHDR-OUT-REC FROM CABS-BILL-HEADER.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 4902 TO WS-AB-CODE
               MOVE 'BILL HEADER WRITE FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-WRITE-CNT.
           PERFORM P5000-REGISTER-LINE THRU P5000-EXIT.

       P4400-EXIT.
           EXIT.

       P4500-LOAD-HEADERS.
           MOVE 'P4500-LOAD-HEADERS' TO WS-PARA-NAME.
           MOVE ZERO TO WS-HT-USED.
           PERFORM P4510-READ-HDR THRU P4510-EXIT
               UNTIL WS-HDR-EOF.
           DISPLAY 'HEADERS LOADED ' WS-HT-USED.

       P4500-EXIT.
           EXIT.

       P4510-READ-HDR.
           READ BHDR-IN-FILE INTO CABS-BILL-HEADER
               AT END
                   MOVE 'Y' TO WS-HDR-EOF-SW
                   GO TO P4510-EXIT.
           IF WS-HT-USED NOT < WS-HT-MAX
               MOVE 4903 TO WS-AB-CODE
               MOVE 'HEADER TABLE FULL' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-HT-USED.
           SET WS-HT-X TO WS-HT-USED.
           MOVE BH-BAN           TO WS-HT-BAN (WS-HT-X).
           MOVE CABS-BILL-HEADER TO WS-HT-IMAGE (WS-HT-X).

       P4510-EXIT.
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
           MOVE BH-BAN             TO PC-COL-001-020.
           MOVE BH-DETAIL-LINES    TO WS-ED-COUNT.
           MOVE WS-ED-COUNT        TO PC-COL-021-060.
           MOVE BH-CURR-USAGE      TO WS-ED-MONEY.
           MOVE WS-ED-MONEY        TO PC-COL-061-090.
           MOVE BH-TOTAL-DUE       TO WS-ED-MONEY.
           MOVE WS-ED-MONEY        TO PC-COL-091-132.
           WRITE PRT-RECORD FROM CABS-PRINT-LINE.
           ADD 1 TO WS-PAGE-LINES.

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
           MOVE 'CABBIL09  INVOICE HEADER CREATION REGISTER'
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
           MOVE 'BAN                 LINES        USAGE         TOTAL'
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
           MOVE 435                    TO CT-STEP-SEQ.
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
           DISPLAY 'HEADERS WRITTEN   ' WS-RT-HEADERS.
           DISPLAY 'DETAIL LINES READ ' WS-RT-DETAILS.
           DISPLAY 'ORPHAN ACCOUNTS   ' WS-RT-ORPHANS.
           DISPLAY 'USAGE TOTAL       ' WS-RT-USAGE.
           DISPLAY 'INTERSTATE TOTAL  ' WS-RT-INTER.
           DISPLAY 'INTRASTATE TOTAL  ' WS-RT-INTRA.
           DISPLAY 'LOCAL TOTAL       ' WS-RT-LOCAL.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE BILL-DTL-IN
                 BHDR-IN-FILE
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

