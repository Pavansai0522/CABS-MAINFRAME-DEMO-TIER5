      *****************************************************************
      * CABBIL08 - MINIMUM AND MAXIMUM BILL ENFORCEMENT               *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               BHDRIN  TELCABS.CABS.BILLHDR.TAX(0)       CABSBHDR*
      *               MMXIN   TELCABS.CABS.CONTRACT.MMX         (LOCAL)*
      *               SYSIN   INSTREAM CONTROL CARD             (LOCAL)*
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               BHDROUT TELCABS.CABS.BILLHDR.MMX(+1)      CABSBHDR*
      *               REPORT  SYSOUT PRINT                      CABSPRNT*
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY (BAN)           *
      * REVISION HISTORY                                              *
      *   V1.00  1989-05-03  D.OKONKWO    INITIAL RELEASE - TARIFF MINIMUM*
      *                      ONLY, NO PER CARRIER CONTRACTS           *
      *   V1.03  1992-11-30  M.J.FERRARO  CONTRACT MINIMUM CARD FILE ADDED*
      *   V1.06  1995-03-14  L.HARGREAVES CONTRACT MAXIMUM ADDED - NEW CARD*
      *                      LAYOUT, OLD ONE STILL ACCEPTED           *
      *   V2.00  2001-07-25  P.NAIR       MAKE UP CHARGE TRUNCATED SO IT CAN*
      *                      NEVER EXCEED THE TRUE SHORTFALL          *
      *   V2.03  2011-10-04  G.PRZYBYLSKI EFFECTIVE DATE HONOURED ON THE*
      *                      CONTRACT CARD                            *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABBIL08.
       AUTHOR. TELCABS APPLICATIONS - BILLING TEAM.
      *****************************************************************
      * APPLIES THE CONTRACT MINIMUM AND MAXIMUM TO THE CURRENT PERIOD*
      * ACCESS REVENUE, RAISING A MAKE UP CHARGE OR CREDITING THE     *
      * EXCESS, AND COMPUTES THE INVOICE TOTAL DUE.                   *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BHDR-IN-FILE ASSIGN TO UT-S-BHDRIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT MMX-IN-FILE ASSIGN TO UT-S-MMXIN
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
      * BHDRIN - BILL HEADER WITH TAX POSTED.                         *
      *****************************************************************
       FD  BHDR-IN-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 400 CHARACTERS.
       01  BHDR-IN-REC                      PIC X(400).
      *****************************************************************
      * MMXIN - CONTRACT MINIMUM AND MAXIMUM CARDS.  TWO              *
      * LAYOUTS, BOTH LIVE.                                           *
      *****************************************************************
       FD  MMX-IN-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  MMX-RECORD                       PIC X(80).
      *****************************************************************
      * BHDROUT - HEADER WITH THE TOTAL DUE COMPUTED.                 *
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
           05  WS-PGM-NAME             PIC X(08) VALUE 'CABBIL08'.
           05  WS-PGM-VERSION          PIC X(05) VALUE 'V2.03'.
           05  WS-PGM-APPL             PIC X(04) VALUE 'CABS'.
           05  WS-PGM-COMPILE          PIC X(08) VALUE '20111004'.
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
      * LAYOUT AGREED WITH THE CARRIER GATEWAY TEAM, CR-3318.         *
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
           05  WS-PE-MINMAX-SW         PIC X(01).
           05  WS-PE-MIN-BILL          PIC 9(07)V9(02).
           05  WS-PE-MAX-BILL          PIC 9(11)V9(02).
           05  WS-PE-MAKEUP-SW         PIC X(01).
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
           05  WS-HDR-EOF-SW           PIC X(01) VALUE 'N'.
               88  WS-HDR-EOF          VALUE 'Y'.
           05  WS-MMX-EOF-SW           PIC X(01) VALUE 'N'.
               88  WS-MMX-EOF          VALUE 'Y'.
           05  WS-MMX-FOUND-SW         PIC X(01) VALUE 'N'.
               88  WS-MMX-FOUND        VALUE 'Y'.
      *****************************************************************
      * THE PER CARRIER MINIMUM AND MAXIMUM TABLE.  THE CONTRACT LEVEL*
      * MINIMUM IS NEGOTIATED PER CARRIER AND ARRIVES ON AN 80 BYTE   *
      * CARD FILE.  A CARRIER THAT IS NOT ON THE FILE FALLS BACK TO THE*
      * TARIFF LEVEL FIGURES ON THE CONTROL CARD.                     *
      *****************************************************************
       01  WS-MMX-TABLE.
           05  WS-MX-ENTRY OCCURS 300 TIMES INDEXED BY WS-MX-X.
               10  WS-MX-OCN           PIC X(04).
               10  WS-MX-MIN           PIC S9(07)V9(02) COMP-3.
               10  WS-MX-MAX           PIC S9(11)V9(02) COMP-3.
               10  WS-MX-MAKEUP-ELEM   PIC X(06).
               10  WS-MX-EFF-YYDDD     PIC 9(05).
       01  WS-MMX-CTL.
           05  WS-MX-USED              PIC S9(05) COMP-3 VALUE 0.
           05  WS-MX-MAX-ENT           PIC S9(05) COMP-3 VALUE 300.
           05  WS-MX-HIT               PIC S9(05) COMP-3 VALUE 0.
      *****************************************************************
      * THE MINIMUM AND MAXIMUM CARD.  80 BYTES, TWO LAYOUTS - THE 1992*
      * CARD CARRIED NO MAXIMUM AND IS STILL ACCEPTED.                *
      *****************************************************************
       01  WS-MMX-CARD                 PIC X(80).
       01  WS-MMX-CARD-N REDEFINES WS-MMX-CARD.
           05  WS-MC-OCN               PIC X(04).
           05  WS-MC-MIN               PIC 9(07)V9(02).
           05  WS-MC-MAX               PIC 9(11)V9(02).
           05  WS-MC-ELEM              PIC X(06).
           05  WS-MC-EFF               PIC 9(05).
           05  WS-MC-FILLER            PIC X(43).
       01  WS-MMX-CARD-O REDEFINES WS-MMX-CARD.
           05  WS-MO-OCN               PIC X(04).
           05  WS-MO-MIN               PIC 9(07)V9(02).
           05  WS-MO-ELEM              PIC X(06).
           05  WS-MO-FILLER            PIC X(61).
      *****************************************************************
      * MINIMUM AND MAXIMUM WORK.  THE MAKE UP CHARGE IS THE DIFFERENCE*
      * BETWEEN WHAT WAS BILLED AND THE CONTRACT MINIMUM.  IT IS      *
      * TRUNCATED, NOT ROUNDED - THE 2001 REVIEW LEFT IT THAT WAY SO  *
      * THAT A MAKE UP CHARGE CAN NEVER EXCEED THE SHORTFALL.         *
      * MONEY FIELDS FOLLOW CABS-STD-041 (TARIFF ROUNDING).           *
      *****************************************************************
       01  WS-MMX-WORK.
           05  WS-MW-BILLED            PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-MW-MIN               PIC S9(07)V9(02) COMP-3 VALUE 0.
           05  WS-MW-MAX               PIC S9(11)V9(02) COMP-3 VALUE 0.
           05  WS-MW-MAKEUP            PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-MW-EXCESS            PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-MW-RAW               PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-MW-ELEM              PIC X(06) VALUE SPACES.
       01  WS-RUN-TOTALS.
           05  WS-RT-HEADERS           PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-BELOW-MIN         PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-ABOVE-MAX         PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-TARIFF-FALLBACK   PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-MAKEUP-AMT        PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-RT-EXCESS-AMT        PIC S9(15)V9(02) COMP-3 VALUE 0.
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
           OPEN INPUT  BHDR-IN-FILE
                       MMX-IN-FILE
                       PARM-FILE
           OPEN OUTPUT BHDR-OUT-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 4131 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-BHDRIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 4132 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-MMXIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 4133 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-BHDROUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 4134 TO WS-AB-CODE
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
           PERFORM P4000-LOAD-CONTRACTS THRU P4000-EXIT.
           PERFORM P6000-HEADING THRU P6000-EXIT.
           DISPLAY WS-PGM-NAME ' STARTED - RUN ' WS-RUN-ID.
           DISPLAY '  MINMAX SWITCH ' WS-PE-MINMAX-SW.
           DISPLAY '  TARIFF MINIMUM' WS-PE-MIN-BILL.
           DISPLAY '  TARIFF MAXIMUM' WS-PE-MAX-BILL.

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
      * THE TARIFF MINIMUM AND MAXIMUM ARE SUPPLIED BY THE SCHEDULER
      * FROM THE CURRENT FILING.  THEY ARE NOT CODED IN THE JCL AND
      * THEY HAVE NO DEFAULT.
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
           IF WS-PE-MINMAX-SW NOT = 'N'
               MOVE 'Y' TO WS-PE-MINMAX-SW.
           IF WS-PE-MAKEUP-SW NOT = 'N'
               MOVE 'Y' TO WS-PE-MAKEUP-SW.
           IF WS-PE-MIN-BILL NOT NUMERIC
               MOVE 4141 TO WS-AB-CODE
               MOVE 'TARIFF MINIMUM NOT NUMERIC' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PE-MAX-BILL NOT NUMERIC
               MOVE ZERO TO WS-PE-MAX-BILL.

       P1200-EXIT.
           EXIT.

      *****************************************************************
      * S200-MAIN PROCESSING                                          *
      *****************************************************************
       S200-MAIN-PROCESS SECTION.

       P2000-PROCESS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ-HEADER THRU P2100-EXIT.
           IF WS-HDR-EOF
               MOVE 'Y' TO WS-EOF-SW
               GO TO P2000-EXIT.
           ADD 1 TO WS-READ-CNT.
           MOVE BH-BAN TO WS-RESTART-KEY.
           PERFORM P3000-DERIVE-BILLED THRU P3000-EXIT.
           PERFORM P3100-FIND-CONTRACT THRU P3100-EXIT.
           PERFORM P3200-APPLY-MINIMUM THRU P3200-EXIT.
           PERFORM P3300-APPLY-MAXIMUM THRU P3300-EXIT.
           PERFORM P3400-WRITE-HEADER THRU P3400-EXIT.

       P2000-EXIT.
           EXIT.

       P2100-READ-HEADER.
           MOVE 'P2100-READ-HEADER' TO WS-PARA-NAME.
           READ BHDR-IN-FILE INTO CABS-BILL-HEADER
               AT END
                   MOVE 'Y' TO WS-HDR-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00' AND WS-FS-INPUT NOT = '10'
               MOVE 4801 TO WS-AB-CODE
               MOVE 'BILL HEADER READ ERROR' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.

       P2100-EXIT.
           EXIT.

      *****************************************************************
      * S300-MINIMUM AND MAXIMUM                                      *
      * THE MINIMUM APPLIES TO CURRENT PERIOD ACCESS REVENUE ONLY.  THE*
      * PRIOR BALANCE, THE SETTLEMENT POSITION AND THE TAX ARE ALL    *
      * OUTSIDE THE CONTRACT MINIMUM.                                 *
      *****************************************************************
       S300-MINMAX SECTION.

       P3000-DERIVE-BILLED.
           MOVE 'P3000-DERIVE-BILLED' TO WS-PARA-NAME.
           COMPUTE WS-MW-BILLED =
                   BH-CURR-USAGE + BH-CURR-RECURRING
                 + BH-CURR-NONRECUR.
           MOVE ZERO TO WS-MW-MAKEUP WS-MW-EXCESS.

       P3000-EXIT.
           EXIT.

       P3100-FIND-CONTRACT.
      * LOOK THE CARRIER UP IN THE CONTRACT TABLE.  A CARRIER THAT IS
      * NOT THERE FALLS BACK TO THE TARIFF FIGURES ON THE CONTROL CARD.
           MOVE 'P3100-FIND-CONTRACT' TO WS-PARA-NAME.
           MOVE 'N' TO WS-MMX-FOUND-SW.
           MOVE ZERO TO WS-MX-HIT.
           PERFORM P3110-MATCH-OCN THRU P3110-EXIT
               VARYING WS-MX-X FROM 1 BY 1
               UNTIL WS-MX-X > WS-MX-USED OR WS-MMX-FOUND.
           IF WS-MMX-FOUND
               SET WS-MX-X TO WS-MX-HIT
               MOVE WS-MX-MIN (WS-MX-X) TO WS-MW-MIN
               MOVE WS-MX-MAX (WS-MX-X) TO WS-MW-MAX
               MOVE WS-MX-MAKEUP-ELEM (WS-MX-X) TO WS-MW-ELEM
               GO TO P3100-EXIT.
           MOVE WS-PE-MIN-BILL TO WS-MW-MIN.
           MOVE WS-PE-MAX-BILL TO WS-MW-MAX.
           MOVE 'NRCCHG' TO WS-MW-ELEM.
           ADD 1 TO WS-RT-TARIFF-FALLBACK.

       P3100-EXIT.
           EXIT.

       P3110-MATCH-OCN.
           IF WS-MX-OCN (WS-MX-X) NOT = BH-OCN
               GO TO P3110-EXIT.
           IF WS-MX-EFF-YYDDD (WS-MX-X) > WS-CYCLE-YYDDD
               GO TO P3110-EXIT.
           SET WS-SUB1 TO WS-MX-X.
           MOVE WS-SUB1 TO WS-MX-HIT.
           MOVE 'Y' TO WS-MMX-FOUND-SW.

       P3110-EXIT.
           EXIT.

       P3200-APPLY-MINIMUM.
      * WHERE THE BILLED REVENUE FALLS SHORT OF THE CONTRACT MINIMUM A
      * MAKE UP CHARGE IS RAISED FOR THE DIFFERENCE.  THE SHORTFALL IS
      * COMPUTED AT FIVE PLACES AND MOVED INTO A TWO PLACE FIELD, WHICH
      * TRUNCATES.  A MAKE UP CHARGE THEREFORE NEVER EXCEEDS THE TRUE
      * SHORTFALL.
           MOVE 'P3200-APPLY-MINIMUM' TO WS-PARA-NAME.
           IF WS-PE-MINMAX-SW NOT = 'Y'
               GO TO P3200-EXIT.
           IF WS-MW-MIN = ZERO
               GO TO P3200-EXIT.
           IF WS-MW-BILLED NOT < WS-MW-MIN
               GO TO P3200-EXIT.
           COMPUTE WS-MW-RAW = WS-MW-MIN - WS-MW-BILLED.
           MOVE WS-MW-RAW TO WS-MW-MAKEUP.
           IF WS-PE-MAKEUP-SW NOT = 'Y'
               ADD 1 TO WS-RT-BELOW-MIN
               GO TO P3200-EXIT.
           ADD WS-MW-MAKEUP TO BH-CURR-NONRECUR.
           ADD WS-MW-MAKEUP TO WS-RT-MAKEUP-AMT.
           ADD 1 TO WS-RT-BELOW-MIN.
           PERFORM P5000-REGISTER-LINE THRU P5000-EXIT.

       P3200-EXIT.
           EXIT.

       P3300-APPLY-MAXIMUM.
      * WHERE THE BILLED REVENUE EXCEEDS THE CONTRACT MAXIMUM THE
      * EXCESS IS CREDITED BACK.  THE CREDIT IS TAKEN OUT OF USAGE
      * BECAUSE THAT IS WHERE THE EXCESS ALMOST ALWAYS ARISES.
           MOVE 'P3300-APPLY-MAXIMUM' TO WS-PARA-NAME.
           IF WS-PE-MINMAX-SW NOT = 'Y'
               GO TO P3300-EXIT.
           IF WS-MW-MAX = ZERO
               GO TO P3300-EXIT.
           IF WS-MW-BILLED NOT > WS-MW-MAX
               GO TO P3300-EXIT.
           COMPUTE WS-MW-RAW = WS-MW-BILLED - WS-MW-MAX.
           MOVE WS-MW-RAW TO WS-MW-EXCESS.
           SUBTRACT WS-MW-EXCESS FROM BH-CURR-USAGE.
           ADD WS-MW-EXCESS TO WS-RT-EXCESS-AMT.
           ADD 1 TO WS-RT-ABOVE-MAX.
           PERFORM P5000-REGISTER-LINE THRU P5000-EXIT.

       P3300-EXIT.
           EXIT.

       P3400-WRITE-HEADER.
           MOVE 'P3400-WRITE-HEADER' TO WS-PARA-NAME.
           COMPUTE BH-TOTAL-DUE =
                   BH-PRIOR-BAL - BH-PAYMENTS + BH-ADJUSTMENTS
                 + BH-CURR-USAGE + BH-CURR-RECURRING
                 + BH-CURR-NONRECUR + BH-RESTATEMENT
                 + BH-SETTLEMENT-NET + BH-TAX.
           WRITE BHDR-OUT-REC FROM CABS-BILL-HEADER.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 4802 TO WS-AB-CODE
               MOVE 'BILL HEADER WRITE FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-WRITE-CNT.
           ADD 1 TO WS-RT-HEADERS.
           ADD BH-TOTAL-DUE TO WS-ACC-AMOUNT.

       P3400-EXIT.
           EXIT.

      *****************************************************************
      * S400-CONTRACT TABLE LOAD                                      *
      *****************************************************************
       S400-SUPPORT SECTION.

       P4000-LOAD-CONTRACTS.
      * LOAD THE CONTRACT MINIMUM AND MAXIMUM CARDS.  THE 1992 CARD
      * CARRIED NO MAXIMUM AND IS RECOGNISED BY A BLANK IN THE MAXIMUM
      * POSITION.  BOTH LAYOUTS ARE STILL PRESENT ON THE LIVE FILE.
           MOVE 'P4000-LOAD-CONTRACTS' TO WS-PARA-NAME.
           MOVE ZERO TO WS-MX-USED.
           PERFORM P4010-READ-CARD THRU P4010-EXIT
               UNTIL WS-MMX-EOF.
           DISPLAY 'CONTRACT ENTRIES ' WS-MX-USED.

       P4000-EXIT.
           EXIT.

       P4010-READ-CARD.
           READ MMX-IN-FILE INTO WS-MMX-CARD
               AT END
                   MOVE 'Y' TO WS-MMX-EOF-SW
                   GO TO P4010-EXIT.
           IF WS-MMX-CARD = SPACES
               GO TO P4010-EXIT.
           IF WS-MX-USED NOT < WS-MX-MAX-ENT
               MOVE 4803 TO WS-AB-CODE
               MOVE 'CONTRACT TABLE FULL' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-MX-USED.
           SET WS-MX-X TO WS-MX-USED.
           MOVE WS-MC-OCN TO WS-MX-OCN (WS-MX-X).
           IF WS-MC-MAX NOT NUMERIC
               MOVE WS-MO-MIN  TO WS-MX-MIN (WS-MX-X)
               MOVE ZERO       TO WS-MX-MAX (WS-MX-X)
               MOVE WS-MO-ELEM TO WS-MX-MAKEUP-ELEM (WS-MX-X)
               MOVE ZERO       TO WS-MX-EFF-YYDDD (WS-MX-X)
               GO TO P4010-EXIT.
           MOVE WS-MC-MIN  TO WS-MX-MIN (WS-MX-X).
           MOVE WS-MC-MAX  TO WS-MX-MAX (WS-MX-X).
           MOVE WS-MC-ELEM TO WS-MX-MAKEUP-ELEM (WS-MX-X).
           MOVE WS-MC-EFF  TO WS-MX-EFF-YYDDD (WS-MX-X).

       P4010-EXIT.
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
           MOVE BH-BAN         TO PC-COL-001-020.
           MOVE WS-MW-BILLED   TO WS-ED-MONEY.
           MOVE WS-ED-MONEY    TO PC-COL-021-060.
           MOVE WS-MW-MAKEUP   TO WS-ED-MONEY.
           MOVE WS-ED-MONEY    TO PC-COL-061-090.
           MOVE WS-MW-EXCESS   TO WS-ED-MONEY.
           MOVE WS-ED-MONEY    TO PC-COL-091-132.
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
           MOVE 'CABBIL08  MINIMUM AND MAXIMUM BILL REGISTER'
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
           MOVE 'BAN                 BILLED          MAKE UP       EX'
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
           MOVE 445                    TO CT-STEP-SEQ.
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
           DISPLAY 'BELOW MINIMUM     ' WS-RT-BELOW-MIN.
           DISPLAY 'ABOVE MAXIMUM     ' WS-RT-ABOVE-MAX.
           DISPLAY 'TARIFF FALLBACK   ' WS-RT-TARIFF-FALLBACK.
           DISPLAY 'MAKE UP TOTAL     ' WS-RT-MAKEUP-AMT.
           DISPLAY 'EXCESS CREDITED   ' WS-RT-EXCESS-AMT.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE BHDR-IN-FILE
                 MMX-IN-FILE
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

