      *****************************************************************
      * CABBIL06 - SETTLEMENT NETTING INTO THE INVOICE                *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               BHDRIN  TELCABS.CABS.BILLHDR.ADJ(0)       CABSBHDR*
      *               SETLIN  TELCABS.SETL.NET(0)               CABSSETL*
      *               SYSIN   INSTREAM CONTROL CARD             (LOCAL)*
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               BHDROUT TELCABS.CABS.BILLHDR.SET(+1)      CABSBHDR*
      *               REPORT  SYSOUT PRINT                      CABSPRNT*
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY (BAN)           *
      * REVISION HISTORY                                              *
      *   V1.00  1990-03-26  M.J.FERRARO  INITIAL RELEASE - MEET POINT ONLY,*
      *                      ONE POSITION PER CIRCUIT                 *
      *   V1.04  1994-08-11  L.HARGREAVES CMDS RESIDUAL POOL SHARE ADDED FOR*
      *                      THE 1994 EXCHANGE AGREEMENT              *
      *   V1.07  1997-04-03  J.M.CASTILLO RECIPROCAL COMPENSATION POSITIONS*
      *                      BROUGHT IN FROM THE SAME FILE            *
      *   V2.00  2001-09-17  P.NAIR       NETTING NOW AT COUNTERPARTY LEVEL,*
      *                      NOT AT CIRCUIT LEVEL                     *
      *   V2.02  2005-11-29  A.BUKOWSKI   DISPUTED POSITIONS HELD OUT OF THE*
      *                      BILL AND SETTLED IN CASH                 *
      *   V2.05  2014-06-18  G.PRZYBYLSKI MINIMUM NET POSITION INTRODUCED -*
      *                      SMALL POSITIONS ROLL TO NEXT PERIOD      *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABBIL06.
       AUTHOR. TELCABS APPLICATIONS - BILLING TEAM.
      *****************************************************************
      * BRINGS THE INTER CARRIER SETTLEMENT NET POSITION ONTO THE BILL*
      * HEADER.  READS A DATASET OWNED BY THE SETTLEMENT APPLICATION. *
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
           SELECT SETL-IN-FILE ASSIGN TO UT-S-SETLIN
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
      * BHDRIN - BILL HEADER WITH ADJUSTMENTS POSTED.                 *
      *****************************************************************
       FD  BHDR-IN-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 400 CHARACTERS.
       01  BHDR-IN-REC                      PIC X(400).
      *****************************************************************
      * SETLIN - TELCABS.SETL.NET, OWNED BY THE SETTLEMENT            *
      * APPLICATION AND READ DIRECTLY BY THIS CABS PROGRAM.           *
      * THE INTERFACE AGREEMENT IS HELD BY THE APPLICATION OWNER.     *
      *****************************************************************
       FD  SETL-IN-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  SETL-IN-REC                      PIC X(180).
      *****************************************************************
      * BHDROUT - HEADER WITH THE SETTLEMENT NET POSTED.              *
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
           05  WS-PGM-NAME             PIC X(08) VALUE 'CABBIL06'.
           05  WS-PGM-VERSION          PIC X(05) VALUE 'V2.05'.
           05  WS-PGM-APPL             PIC X(04) VALUE 'CABS'.
           05  WS-PGM-COMPILE          PIC X(08) VALUE '20140618'.
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

       COPY CABSSETL.

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
           05  WS-PE-NET-INTO-BILL     PIC X(01).
           05  WS-PE-CMDS-RESID-SW     PIC X(01).
           05  WS-PE-SETL-PERIOD       PIC 9(06).
           05  WS-PE-MIN-NET           PIC 9(07)V9(02).
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
           05  WS-HDR-EOF-SW           PIC X(01) VALUE 'N'.
               88  WS-HDR-EOF          VALUE 'Y'.
           05  WS-SET-EOF-SW           PIC X(01) VALUE 'N'.
               88  WS-SET-EOF          VALUE 'Y'.
           05  WS-OCN-FOUND-SW         PIC X(01) VALUE 'N'.
               88  WS-OCN-FOUND        VALUE 'Y'.
           05  WS-RESID-ACTIVE-SW      PIC X(01) VALUE 'N'.
               88  WS-RESID-ACTIVE     VALUE 'Y'.
      *****************************************************************
      * THE SETTLEMENT POSITION TABLE.  ONE ENTRY PER COUNTERPARTY OCN*
      * CARRYING THE THREE SETTLEMENT KINDS SEPARATELY AND THE NET.   *
      * THE SETTLEMENT APPLICATION OWNS THE FILE THIS IS BUILT FROM.  *
      *****************************************************************
       01  WS-SETL-TABLE.
           05  WS-SL-ENTRY OCCURS 500 TIMES INDEXED BY WS-SL-X.
               10  WS-SL-OCN           PIC X(04).
               10  WS-SL-MPB           PIC S9(13)V9(05) COMP-3.
               10  WS-SL-RECIP         PIC S9(13)V9(05) COMP-3.
               10  WS-SL-CMDS          PIC S9(13)V9(05) COMP-3.
               10  WS-SL-NET           PIC S9(13)V9(05) COMP-3.
               10  WS-SL-RESIDUE       PIC S9(05)V9(05) COMP-3.
               10  WS-SL-DIRECTION     PIC X(01).
               10  WS-SL-DISPUTED      PIC X(01).
               10  WS-SL-POSTED-SW     PIC X(01).
       01  WS-SETL-CTL.
           05  WS-SL-USED              PIC S9(05) COMP-3 VALUE 0.
           05  WS-SL-MAX               PIC S9(05) COMP-3 VALUE 500.
           05  WS-SL-HIT               PIC S9(05) COMP-3 VALUE 0.
      *****************************************************************
      * NETTING WORK.  THE SETTLEMENT FILE CARRIES FIVE DECIMAL PLACES*
      * AND THE BILL HEADER CARRIES TWO.  THE CONVERSION IS DONE HERE *
      * WITH ROUNDED; CABBIL09 TRUNCATES THE SAME QUANTITY WHEN IT    *
      * ROLLS IT INTO THE INVOICE TOTAL.                              *
      * PRECISION AGREED WITH REVENUE ACCOUNTING, CR-2907.            *
      *****************************************************************
       01  WS-NET-WORK.
           05  WS-NW-RAW               PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-NW-ROUNDED           PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-NW-DELTA             PIC S9(05)V9(05) COMP-3 VALUE 0.
           05  WS-NW-RESIDUE-SUM       PIC S9(11)V9(05) COMP-3 VALUE 0.
           05  WS-NW-DIRECTION         PIC X(01) VALUE SPACES.
      *****************************************************************
      * CMDS RESIDUAL NETTING.  THE RESIDUAL POOL WAS PART OF THE 1994*
      * CMDS AGREEMENT AND IS DRIVEN BY A SWITCH ON THE CONTROL CARD. *
      *****************************************************************
       01  WS-RESID-WORK.
           05  WS-RW-POOL              PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-RW-SHARE             PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-RW-PCT               PIC S9(03)V9(05) COMP-3
                                                    VALUE 000.50000.
           05  WS-RW-COUNT             PIC S9(05) COMP-3 VALUE 0.
       01  WS-RUN-TOTALS.
           05  WS-RT-HEADERS           PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-NETTED            PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-BELOW-MIN         PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-DISPUTED          PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-NET-AMT           PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-RT-MPB-AMT           PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-RT-RECIP-AMT         PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-RT-CMDS-AMT          PIC S9(15)V9(02) COMP-3 VALUE 0.
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
                       SETL-IN-FILE
                       PARM-FILE
           OPEN OUTPUT BHDR-OUT-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 4081 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-BHDRIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 4082 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-SETLIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 4083 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-BHDROUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 4084 TO WS-AB-CODE
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
           PERFORM P4000-LOAD-SETTLEMENT THRU P4000-EXIT.
           PERFORM P6000-HEADING THRU P6000-EXIT.
           DISPLAY WS-PGM-NAME ' STARTED - RUN ' WS-RUN-ID.
           DISPLAY '  NET INTO BILL ' WS-PE-NET-INTO-BILL.
           DISPLAY '  RESIDUAL SW   ' WS-PE-CMDS-RESID-SW.
           DISPLAY '  SETTLE PERIOD ' WS-PE-SETL-PERIOD.

       P1000-EXIT.
           EXIT.

       P1100-READ-PARM.
      * THE SYSIN CARD CARRIES THE VALUES THE SCHEDULER SUBSTITUTED INTO
      * THE JCL AT SUBMISSION TIME.  THERE ARE NO DEFAULTS - AN ABSENT
      * CARD IS A FATAL ERROR, NOT A DEFAULTED RUN.
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
      * EDIT THE CONTROL CARD.  EVERY FIELD IS MANDATORY.  THE 1989 CARD
      * FORMAT IS STILL ACCEPTED VIA THE WS-PARM-OLD REDEFINE.
      * THE SETTLEMENT PERIOD IS NOT ALWAYS THE BILL PERIOD - THE
      * SETTLEMENT CYCLE RUNS A MONTH BEHIND THE BILL CYCLE.  THE
      * SCHEDULER SUPPLIES BOTH AND NEITHER HAS A DEFAULT.
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
           IF WS-PE-NET-INTO-BILL NOT = 'N'
               MOVE 'Y' TO WS-PE-NET-INTO-BILL.
           IF WS-PE-CMDS-RESID-SW NOT = 'Y'
               MOVE 'N' TO WS-PE-CMDS-RESID-SW.
           IF WS-PE-SETL-PERIOD NOT NUMERIC
               MOVE 4091 TO WS-AB-CODE
               MOVE 'SETTLEMENT PERIOD NOT NUMERIC' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PE-MIN-NET NOT NUMERIC
               MOVE ZERO TO WS-PE-MIN-NET.

       P1200-EXIT.
           EXIT.

      *****************************************************************
      * S200-MAIN PROCESSING                                          *
      * THE SETTLEMENT NET POSITION IS HELD AT COUNTERPARTY LEVEL.  IT*
      * IS APPLIED TO THE FIRST BILLING ACCOUNT ENCOUNTERED FOR THAT  *
      * COUNTERPARTY - A CARRIER WITH SEVERAL BANS SEES THE WHOLE NET *
      * ON ONE OF THEM.                                               *
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
           MOVE ZERO TO WS-NW-RAW WS-NW-ROUNDED WS-NW-DELTA.
           PERFORM P3100-FIND-SETTLEMENT THRU P3100-EXIT.
           IF WS-OCN-FOUND
               PERFORM P3200-NET-INTO-BILL THRU P3200-EXIT.
           PERFORM P3500-WRITE-HEADER THRU P3500-EXIT.

       P2000-EXIT.
           EXIT.

       P2100-READ-HEADER.
           MOVE 'P2100-READ-HEADER' TO WS-PARA-NAME.
           READ BHDR-IN-FILE INTO CABS-BILL-HEADER
               AT END
                   MOVE 'Y' TO WS-HDR-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00' AND WS-FS-INPUT NOT = '10'
               MOVE 4601 TO WS-AB-CODE
               MOVE 'BILL HEADER READ ERROR' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.

       P2100-EXIT.
           EXIT.

      *****************************************************************
      * S300-SETTLEMENT NETTING                                       *
      *****************************************************************
       S300-NETTING SECTION.

       P3100-FIND-SETTLEMENT.
      * LOCATE THE COUNTERPARTY POSITION.  A POSITION THAT HAS ALREADY
      * BEEN POSTED TO AN EARLIER ACCOUNT IS NOT POSTED AGAIN.
           MOVE 'P3100-FIND-SETTLEMENT' TO WS-PARA-NAME.
           MOVE 'N' TO WS-OCN-FOUND-SW.
           MOVE ZERO TO WS-SL-HIT.
           PERFORM P3110-SCAN-SETL THRU P3110-EXIT
               VARYING WS-SL-X FROM 1 BY 1
               UNTIL WS-SL-X > WS-SL-USED OR WS-OCN-FOUND.

       P3100-EXIT.
           EXIT.

       P3110-SCAN-SETL.
           IF WS-SL-OCN (WS-SL-X) NOT = BH-OCN
               GO TO P3110-EXIT.
           IF WS-SL-POSTED-SW (WS-SL-X) = 'Y'
               GO TO P3110-EXIT.
           SET WS-SUB1 TO WS-SL-X.
           MOVE WS-SUB1 TO WS-SL-HIT.
           MOVE 'Y' TO WS-OCN-FOUND-SW.

       P3110-EXIT.
           EXIT.

       P3200-NET-INTO-BILL.
      * POST THE NET POSITION ONTO THE BILL HEADER.  A RECEIVABLE ADDS
      * TO WHAT THE CARRIER OWES; A PAYABLE REDUCES IT.  A DISPUTED
      * POSITION IS HELD OUT OF THE BILL ENTIRELY AND SETTLED IN CASH.
           MOVE 'P3200-NET-INTO-BILL' TO WS-PARA-NAME.
           SET WS-SL-X TO WS-SL-HIT.
           IF WS-PE-NET-INTO-BILL NOT = 'Y'
               ADD 1 TO WS-CFWD-CNT
               GO TO P3200-EXIT.
           IF WS-SL-DISPUTED (WS-SL-X) = 'Y'
               ADD 1 TO WS-RT-DISPUTED
               ADD 1 TO WS-CFWD-CNT
               GO TO P3200-EXIT.
           MOVE WS-SL-NET (WS-SL-X) TO WS-NW-RAW.
           IF WS-SL-DIRECTION (WS-SL-X) = 'P'
               COMPUTE WS-NW-RAW = WS-NW-RAW * -1.
           PERFORM P3300-MIN-NET-TEST THRU P3300-EXIT.
           IF WS-ERR-CODE NOT = SPACES
               MOVE SPACES TO WS-ERR-CODE
               GO TO P3200-EXIT.
           PERFORM P3400-ROUND-NET THRU P3400-EXIT.
           ADD WS-NW-ROUNDED TO BH-SETTLEMENT-NET.
           MOVE 'Y' TO WS-SL-POSTED-SW (WS-SL-X).
           ADD 1 TO WS-RT-NETTED.
           ADD WS-NW-ROUNDED TO WS-RT-NET-AMT.
           ADD WS-SL-MPB (WS-SL-X) TO WS-RT-MPB-AMT.
           ADD WS-SL-RECIP (WS-SL-X) TO WS-RT-RECIP-AMT.
           ADD WS-SL-CMDS (WS-SL-X) TO WS-RT-CMDS-AMT.
           PERFORM P4800-CMDS-RESIDUAL-NET THRU P4800-EXIT.
           PERFORM P5000-REGISTER-LINE THRU P5000-EXIT.

       P3200-EXIT.
           EXIT.

       P3300-MIN-NET-TEST.
      * A POSITION SMALLER THAN THE MINIMUM IS NOT WORTH PUTTING ON A
      * BILL.  IT STAYS ON THE SETTLEMENT FILE AND ROLLS INTO THE NEXT
      * PERIOD.  THE MINIMUM IS SET BY THE SCHEDULER AT SUBMISSION.
           MOVE 'P3300-MIN-NET-TEST' TO WS-PARA-NAME.
           MOVE SPACES TO WS-ERR-CODE.
           IF WS-PE-MIN-NET = ZERO
               GO TO P3300-EXIT.
           IF WS-NW-RAW < WS-PE-MIN-NET
               IF WS-NW-RAW > ZERO
                   MOVE EC-OUT-OF-BALANCE TO WS-ERR-CODE
                   ADD 1 TO WS-RT-BELOW-MIN
                   ADD 1 TO WS-CFWD-CNT.

       P3300-EXIT.
           EXIT.

       P3400-ROUND-NET.
      * CONVERT THE FIVE DECIMAL SETTLEMENT FIGURE TO THE TWO DECIMAL
      * FIGURE THE BILL HEADER CAN HOLD.  THE RESIDUE IS ACCUMULATED
      * ACROSS THE RUN AND PRINTED ON THE REGISTER SO THAT THE
      * SETTLEMENT TEAM CAN RECONCILE THE PENNY.
           MOVE 'P3400-ROUND-NET' TO WS-PARA-NAME.
           COMPUTE WS-NW-ROUNDED ROUNDED = WS-NW-RAW.
           COMPUTE WS-NW-DELTA = WS-NW-RAW - WS-NW-ROUNDED.
           ADD WS-NW-DELTA TO WS-NW-RESIDUE-SUM.

       P3400-EXIT.
           EXIT.

       P3500-WRITE-HEADER.
           MOVE 'P3500-WRITE-HEADER' TO WS-PARA-NAME.
           WRITE BHDR-OUT-REC FROM CABS-BILL-HEADER.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 4602 TO WS-AB-CODE
               MOVE 'BILL HEADER WRITE FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-WRITE-CNT.
           ADD 1 TO WS-RT-HEADERS.
           ADD BH-SETTLEMENT-NET TO WS-ACC-AMOUNT.

       P3500-EXIT.
           EXIT.

      *****************************************************************
      * S400-SETTLEMENT FILE LOAD AND RESIDUAL POOL                   *
      *****************************************************************
       S400-SUPPORT SECTION.

       P4000-LOAD-SETTLEMENT.
      * LOAD THE SETTLEMENT NET FILE.  THE FILE IS PRODUCED BY CABSET09
      * AND IS ALREADY ONE RECORD PER COUNTERPARTY.  THE THREE
      * SETTLEMENT KINDS ARE SEPARATED HERE BY THE TYPE ON THE RECORD.
           MOVE 'P4000-LOAD-SETTLEMENT' TO WS-PARA-NAME.
           MOVE ZERO TO WS-SL-USED.
           PERFORM P4010-READ-SETL THRU P4010-EXIT
               UNTIL WS-SET-EOF.
           DISPLAY 'SETTLEMENT ENTRIES ' WS-SL-USED.

       P4000-EXIT.
           EXIT.

       P4010-READ-SETL.
           READ SETL-IN-FILE INTO CABS-SETTLEMENT-RECORD
               AT END
                   MOVE 'Y' TO WS-SET-EOF-SW
                   GO TO P4010-EXIT.
           IF ST-SETTLE-PERIOD NOT = WS-PE-SETL-PERIOD
               GO TO P4010-EXIT.
           PERFORM P4020-FIND-OR-ADD THRU P4020-EXIT.
           SET WS-SL-X TO WS-SL-HIT.
           IF ST-MEET-POINT
               ADD ST-NET-DUE TO WS-SL-MPB (WS-SL-X).
           IF ST-RECIP-COMP
               ADD ST-NET-DUE TO WS-SL-RECIP (WS-SL-X).
           IF ST-CMDS-RAO
               ADD ST-NET-DUE TO WS-SL-CMDS (WS-SL-X).
           ADD ST-NET-DUE TO WS-SL-NET (WS-SL-X).
           ADD ST-ROUND-RESIDUE TO WS-SL-RESIDUE (WS-SL-X).
           MOVE ST-DIRECTION TO WS-SL-DIRECTION (WS-SL-X).
           IF ST-DISPUTE-SW = 'Y'
               MOVE 'Y' TO WS-SL-DISPUTED (WS-SL-X).

       P4010-EXIT.
           EXIT.

       P4020-FIND-OR-ADD.
           MOVE 'N' TO WS-OCN-FOUND-SW.
           PERFORM P4030-MATCH-OCN THRU P4030-EXIT
               VARYING WS-SL-X FROM 1 BY 1
               UNTIL WS-SL-X > WS-SL-USED OR WS-OCN-FOUND.
           IF WS-OCN-FOUND
               GO TO P4020-EXIT.
           IF WS-SL-USED NOT < WS-SL-MAX
               MOVE 4603 TO WS-AB-CODE
               MOVE 'SETTLEMENT TABLE FULL' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-SL-USED.
           MOVE WS-SL-USED TO WS-SL-HIT.
           SET WS-SL-X TO WS-SL-USED.
           MOVE ST-COUNTERPARTY-OCN TO WS-SL-OCN (WS-SL-X).
           MOVE ZERO TO WS-SL-MPB (WS-SL-X)
                        WS-SL-RECIP (WS-SL-X)
                        WS-SL-CMDS (WS-SL-X)
                        WS-SL-NET (WS-SL-X)
                        WS-SL-RESIDUE (WS-SL-X).
           MOVE 'N' TO WS-SL-DISPUTED (WS-SL-X).
           MOVE 'N' TO WS-SL-POSTED-SW (WS-SL-X).

       P4020-EXIT.
           EXIT.

       P4030-MATCH-OCN.
           IF WS-SL-OCN (WS-SL-X) = ST-COUNTERPARTY-OCN
               SET WS-SUB1 TO WS-SL-X
               MOVE WS-SUB1 TO WS-SL-HIT
               MOVE 'Y' TO WS-OCN-FOUND-SW.

       P4030-EXIT.
           EXIT.

       P4800-CMDS-RESIDUAL-NET.
      * THE CMDS RESIDUAL POOL.  UNDER THE 1994 EXCHANGE AGREEMENT THE
      * UNALLOCATED RESIDUE FROM THE RAO EXCHANGE IS SHARED BETWEEN THE
      * TWO PARTIES AT THE AGREED PERCENTAGE AND THE SHARE IS CARRIED
      * ONTO THE BILL.  THE SHARE IS ONLY TAKEN WHEN THE OPERATOR
      * SUPPLIES THE RESIDUAL SWITCH ON THE CONTROL CARD.
           MOVE 'P4800-CMDS-RESIDUAL-NET' TO WS-PARA-NAME.
           IF WS-PE-CMDS-RESID-SW NOT = 'Y'
               GO TO P4800-EXIT.
           SET WS-SL-X TO WS-SL-HIT.
           MOVE WS-SL-RESIDUE (WS-SL-X) TO WS-RW-POOL.
           IF WS-RW-POOL = ZERO
               GO TO P4800-EXIT.
           COMPUTE WS-RW-SHARE ROUNDED =
                   (WS-RW-POOL * WS-RW-PCT) / 100.
           ADD WS-RW-SHARE TO BH-SETTLEMENT-NET.
           ADD 1 TO WS-RW-COUNT.
           MOVE 'Y' TO WS-RESID-ACTIVE-SW.

       P4800-EXIT.
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
           MOVE BH-OCN                 TO PC-COL-021-060.
           MOVE WS-SL-DIRECTION (WS-SL-X) TO PC-COL-061-090.
           MOVE WS-NW-ROUNDED          TO WS-ED-MONEY.
           MOVE WS-ED-MONEY            TO PC-COL-091-132.
           WRITE PRT-RECORD FROM CABS-PRINT-LINE.
           ADD 1 TO WS-PAGE-LINES.

       P5000-EXIT.
           EXIT.

       P5100-PRINT-RESIDUE.
           MOVE 'P5100-PRINT-RESIDUE' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '0' TO PC-CC.
           MOVE 'ROUNDING RESIDUE CARRIED THIS RUN' TO PC-COL-001-060.
           MOVE WS-NW-RESIDUE-SUM TO WS-ED-MONEY.
           MOVE WS-ED-MONEY TO PC-COL-061-090.
           WRITE PRT-RECORD FROM CABS-PRINT-LINE.

       P5100-EXIT.
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
           MOVE 'CABBIL06  SETTLEMENT NETTING REGISTER'
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
           MOVE 'BAN                 OCN     DIRECTION    NET POSTED'
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
           MOVE 430                    TO CT-STEP-SEQ.
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
           PERFORM P5100-PRINT-RESIDUE THRU P5100-EXIT.
           DISPLAY 'HEADERS WRITTEN   ' WS-RT-HEADERS.
           DISPLAY 'POSITIONS NETTED  ' WS-RT-NETTED.
           DISPLAY 'BELOW MINIMUM     ' WS-RT-BELOW-MIN.
           DISPLAY 'DISPUTED HELD     ' WS-RT-DISPUTED.
           DISPLAY 'NET POSTED TOTAL  ' WS-RT-NET-AMT.
           DISPLAY 'MEET POINT TOTAL  ' WS-RT-MPB-AMT.
           DISPLAY 'RECIP COMP TOTAL  ' WS-RT-RECIP-AMT.
           DISPLAY 'CMDS TOTAL        ' WS-RT-CMDS-AMT.
           DISPLAY 'RESIDUAL SHARES   ' WS-RW-COUNT.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE BHDR-IN-FILE
                 SETL-IN-FILE
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

