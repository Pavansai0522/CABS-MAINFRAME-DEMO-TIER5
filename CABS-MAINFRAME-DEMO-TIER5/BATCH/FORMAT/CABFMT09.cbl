      *****************************************************************
      * CABFMT09 - BILL MESSAGE INSERT PAGE                           *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               BHDRIN  TELCABS.CABS.BILLHDR.FIN(0)       CABSBHDR*
      *               MSGIN   TELCABS.CABS.BILLMSG              (LOCAL)*
      *               SYSIN   INSTREAM CONTROL CARD             (LOCAL)*
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               PRTOUT  TELCABS.CABS.PRINT.MSG(+1)        CABSPRNT*
      *               REPORT  SYSOUT PRINT                      CABSPRNT*
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN                                      *
      * REVISION HISTORY                                              *
      *   V1.00  1993-09-14  D.OKONKWO    INITIAL RELEASE - ONE MESSAGE PER*
      *                      CAMPAIGN, NO CARRIER TARGETING           *
      *   V1.03  1997-02-25  M.J.FERRARO  CARRIER TYPE TARGETING AND A DATE*
      *                      WINDOW ADDED, NEW CARD LAYOUT            *
      *   V1.06  2002-06-11  P.NAIR       MESSAGE PAGE MADE A DOCUMENT OF ITS*
      *                      OWN SO THE INSERTER CAN DIVERT IT        *
      *   V1.08  2009-02-03  G.PRZYBYLSKI RECOMPILE FOR THE 400 BYTE HEADER -*
      *                      NO FUNCTIONAL CHANGE                     *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABFMT09.
       AUTHOR. TELCABS APPLICATIONS - BILL PRINT TEAM.
      *****************************************************************
      * PRODUCES THE MESSAGE INSERT PAGE THAT MARKETING ATTACH TO THE *
      * BACK OF A CARRIER BILL WHEN A CAMPAIGN IS RUNNING.            *
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
           SELECT MSG-IN-FILE ASSIGN TO UT-S-MSGIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-TABLE.
           SELECT PRINT-STREAM ASSIGN TO UT-S-PRTOUT
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
      * BHDRIN - THE FINAL NUMBERED INVOICE HEADER.                   *
      *****************************************************************
       FD  BHDR-IN-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 400 CHARACTERS.
       01  BHDR-IN-REC                      PIC X(400).
      *****************************************************************
      * MSGIN - THE BILL MESSAGE FILE.  TWO CARD LAYOUTS,             *
      * THE 1993 ONE STILL ACCEPTED.                                  *
      *****************************************************************
       FD  MSG-IN-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 120 CHARACTERS.
       01  MSG-RECORD                       PIC X(120).
      *****************************************************************
      * PRTOUT - PRINT STREAM.  FBA 133, ASA CARRIAGE CONTROL.        *
      *****************************************************************
       FD  PRINT-STREAM
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       01  PRINT-LINE                   PIC X(133).
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
           05  WS-PGM-NAME             PIC X(08) VALUE 'CABFMT09'.
           05  WS-PGM-VERSION          PIC X(05) VALUE 'V1.08'.
           05  WS-PGM-APPL             PIC X(04) VALUE 'CABS'.
           05  WS-PGM-COMPILE          PIC X(08) VALUE '20090203'.
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
           05  WS-PE-INSERT-SW         PIC X(01).
           05  WS-PE-CAMPAIGN          PIC X(06).
           05  WS-PE-MSG-FROM          PIC 9(05).
           05  WS-PE-MSG-THRU          PIC 9(05).
           05  WS-PE-FILLER            PIC X(18).
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
           05  WS-MSG-EOF-SW           PIC X(01) VALUE 'N'.
               88  WS-MSG-EOF          VALUE 'Y'.
           05  WS-FEATURE-SW           PIC X(01) VALUE 'N'.
               88  WS-FEATURE-ON       VALUE 'Y'.
           05  WS-MSG-FOUND-SW         PIC X(01) VALUE 'N'.
               88  WS-MSG-FOUND        VALUE 'Y'.
      *****************************************************************
      * THE BILL MESSAGE TABLE.  MESSAGES ARE SELECTED BY CAMPAIGN AND*
      * BY THE CARRIER TYPE AND ARE PRINTED ON A PAGE OF THEIR OWN AT *
      * THE BACK OF THE INVOICE.                                      *
      *****************************************************************
       01  WS-MSG-TABLE.
           05  WS-MT-ENTRY OCCURS 200 TIMES INDEXED BY WS-MT-X.
               10  WS-MT-CAMPAIGN      PIC X(06).
               10  WS-MT-CARR-TYPE     PIC X(01).
               10  WS-MT-SEQ           PIC 9(02).
               10  WS-MT-FROM-YYDDD    PIC 9(05).
               10  WS-MT-THRU-YYDDD    PIC 9(05).
               10  WS-MT-TEXT          PIC X(70).
       01  WS-MSG-CTL.
           05  WS-MT-USED              PIC S9(05) COMP-3 VALUE 0.
           05  WS-MT-MAX               PIC S9(05) COMP-3 VALUE 200.
           05  WS-MT-HIT               PIC S9(05) COMP-3 VALUE 0.
           05  WS-MT-SELECTED          PIC S9(05) COMP-3 VALUE 0.
       01  WS-MSG-IN.
           05  WS-MI-CAMPAIGN          PIC X(06).
           05  WS-MI-CARR-TYPE         PIC X(01).
           05  WS-MI-SEQ               PIC 9(02).
           05  WS-MI-FROM              PIC 9(05).
           05  WS-MI-THRU              PIC 9(05).
           05  WS-MI-TEXT              PIC X(70).
           05  WS-MI-FILLER            PIC X(31).
       01  WS-MSG-IN-O REDEFINES WS-MSG-IN.
           05  WS-MO-CAMPAIGN          PIC X(06).
           05  WS-MO-SEQ               PIC 9(02).
           05  WS-MO-TEXT              PIC X(60).
           05  WS-MO-FILLER            PIC X(52).
       01  WS-MSG-WORK.
           05  WS-MW-LINE-CNT          PIC S9(03) COMP-3 VALUE 0.
           05  WS-MW-PAGE-CNT          PIC S9(09) COMP-3 VALUE 0.
           05  WS-MW-CARR-TYPE         PIC X(01) VALUE SPACES.
       01  WS-RUN-TOTALS.
           05  WS-RT-INVOICES          PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-INSERTED          PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-LINES             PIC S9(11) COMP-3 VALUE 0.
           05  WS-RT-NO-MESSAGE        PIC S9(09) COMP-3 VALUE 0.
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
       01  WS-DESC-WORK                PIC X(60) VALUE SPACES.
       01  WS-DESC-WORK-R REDEFINES WS-DESC-WORK.
           05  WS-DW-CHAR OCCURS 60 TIMES PIC X(01).
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
                       MSG-IN-FILE
                       PARM-FILE
           OPEN OUTPUT PRINT-STREAM
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 6911 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-BHDRIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 6912 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-MSGIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 6913 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-PRTOUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 6914 TO WS-AB-CODE
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
           PERFORM P4100-LOAD-MESSAGES THRU P4100-EXIT.
           PERFORM P6000-HEADING THRU P6000-EXIT.
           DISPLAY WS-PGM-NAME ' STARTED - RUN ' WS-RUN-ID.
           DISPLAY '  INSERT SWITCH ' WS-PE-INSERT-SW.
           DISPLAY '  CAMPAIGN      ' WS-PE-CAMPAIGN.

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
      * THE INSERT SWITCH AND THE CAMPAIGN CODE ARE SET BY THE
      * SCHEDULER FROM THE MARKETING CAMPAIGN CALENDAR.
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
           IF WS-PE-INSERT-SW NOT = 'Y'
               MOVE 'N' TO WS-PE-INSERT-SW.
           IF WS-PE-MSG-FROM NOT NUMERIC
               MOVE ZERO TO WS-PE-MSG-FROM.
           IF WS-PE-MSG-THRU NOT NUMERIC
               MOVE ZERO TO WS-PE-MSG-THRU.

       P1200-EXIT.
           EXIT.

      *****************************************************************
      * S200-MAIN PROCESSING                                          *
      * THE MESSAGE INSERT PAGE.  EVERY INVOICE IS EXAMINED AND, WHERE*
      * A MESSAGE APPLIES TO THE CARRIER TYPE AND THE CAMPAIGN IS     *
      * CURRENT, A MESSAGE PAGE IS PRODUCED AT THE BACK OF THE BILL.  *
      *****************************************************************
       S200-MAIN-PROCESS SECTION.

       P2000-PROCESS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ-HEADER THRU P2100-EXIT.
           IF WS-HDR-EOF
               MOVE 'Y' TO WS-EOF-SW
               GO TO P2000-EXIT.
           ADD 1 TO WS-READ-CNT.
           ADD 1 TO WS-RT-INVOICES.
           MOVE BH-BAN TO WS-RESTART-KEY.
           PERFORM P1400-FEATURE-CHECK THRU P1400-EXIT.
           IF NOT WS-FEATURE-ON
               ADD 1 TO WS-CFWD-CNT
               GO TO P2000-EXIT.
           PERFORM P3000-SELECT-MESSAGE THRU P3000-EXIT.
           IF NOT WS-MSG-FOUND
               ADD 1 TO WS-RT-NO-MESSAGE
               ADD 1 TO WS-CFWD-CNT
               GO TO P2000-EXIT.
           PERFORM P3200-BUILD-MESSAGE-PAGE THRU P3200-EXIT.
           ADD 1 TO WS-SUMM-CNT.

       P2000-EXIT.
           EXIT.

       P2100-READ-HEADER.
           MOVE 'P2100-READ-HEADER' TO WS-PARA-NAME.
           READ BHDR-IN-FILE INTO CABS-BILL-HEADER
               AT END
                   MOVE 'Y' TO WS-HDR-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00' AND WS-FS-INPUT NOT = '10'
               MOVE 6901 TO WS-AB-CODE
               MOVE 'BILL HEADER READ ERROR' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.

       P2100-EXIT.
           EXIT.

       P1400-FEATURE-CHECK.
      * THE MESSAGE INSERT SWITCH.  IT IS SUPPLIED BY THE SCHEDULER
      * FROM THE CAMPAIGN CALENDAR AND CONTROLS WHETHER ANY MESSAGE
      * PAGES ARE PRODUCED AT ALL.  MARKETING SET IT WHEN A CAMPAIGN
      * IS LIVE.
           MOVE 'P1400-FEATURE-CHECK' TO WS-PARA-NAME.
           MOVE 'N' TO WS-FEATURE-SW.
           IF WS-PE-INSERT-SW NOT = 'Y'
               GO TO P1400-EXIT.
           IF WS-PE-CAMPAIGN = SPACES
               GO TO P1400-EXIT.
           MOVE 'Y' TO WS-FEATURE-SW.

       P1400-EXIT.
           EXIT.

      *****************************************************************
      * S300-MESSAGE SELECTION AND PAGE BUILD                         *
      *****************************************************************
       S300-MESSAGE SECTION.

       P3000-SELECT-MESSAGE.
      * SELECT THE MESSAGE FOR THIS INVOICE.  THE CAMPAIGN MUST MATCH,
      * THE CARRIER TYPE MUST MATCH OR BE THE WILDCARD, AND THE CYCLE
      * DATE MUST FALL INSIDE THE MESSAGE WINDOW.
           MOVE 'P3000-SELECT-MESSAGE' TO WS-PARA-NAME.
           MOVE 'N' TO WS-MSG-FOUND-SW.
           MOVE ZERO TO WS-MT-HIT.
           PERFORM P3100-DERIVE-CARR-TYPE THRU P3100-EXIT.
           PERFORM P3050-MATCH-MESSAGE THRU P3050-EXIT
               VARYING WS-MT-X FROM 1 BY 1
               UNTIL WS-MT-X > WS-MT-USED OR WS-MSG-FOUND.

       P3000-EXIT.
           EXIT.

       P3050-MATCH-MESSAGE.
           IF WS-MT-CAMPAIGN (WS-MT-X) NOT = WS-PE-CAMPAIGN
               GO TO P3050-EXIT.
           IF WS-MT-CARR-TYPE (WS-MT-X) NOT = WS-MW-CARR-TYPE
               IF WS-MT-CARR-TYPE (WS-MT-X) NOT = '*'
                   GO TO P3050-EXIT.
           IF WS-MT-FROM-YYDDD (WS-MT-X) > WS-CYCLE-YYDDD
               GO TO P3050-EXIT.
           IF WS-MT-THRU-YYDDD (WS-MT-X) NOT = ZERO
               IF WS-MT-THRU-YYDDD (WS-MT-X) < WS-CYCLE-YYDDD
                   GO TO P3050-EXIT.
           SET WS-SUB1 TO WS-MT-X.
           MOVE WS-SUB1 TO WS-MT-HIT.
           MOVE 'Y' TO WS-MSG-FOUND-SW.

       P3050-EXIT.
           EXIT.

       P3100-DERIVE-CARR-TYPE.
      * THE CARRIER TYPE IS NOT ON THE BILL HEADER.  IT IS DERIVED FROM
      * THE FIRST CHARACTER OF THE OCN, WHICH IS THE CONVENTION THE
      * NUMBERING ADMINISTRATOR USED WHEN THE CODES WERE FIRST ISSUED.
           MOVE 'P3100-DERIVE-CARR-TYPE' TO WS-PARA-NAME.
           MOVE '*' TO WS-MW-CARR-TYPE.
           IF BH-OCN NOT = SPACES
               MOVE BH-OCN TO WS-DESC-WORK
               MOVE WS-DW-CHAR (1) TO WS-MW-CARR-TYPE.

       P3100-EXIT.
           EXIT.

       P3200-BUILD-MESSAGE-PAGE.
      * THE MESSAGE PAGE.  IT IS A DOCUMENT OF ITS OWN IN THE BURST
      * STREAM SO THAT IT CAN BE DIVERTED TO A DIFFERENT TRAY.
      * THE PRINT REGISTER IS RECONCILED BY THE MAILROOM.
           MOVE 'P3200-BUILD-MESSAGE-PAGE' TO WS-PARA-NAME.
           SET WS-MT-X TO WS-MT-HIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '7' TO PC-CC.
           MOVE BH-INVOICE-NBR TO PC-COL-001-020.
           MOVE 'IMPORTANT INFORMATION' TO PC-COL-021-060.
           PERFORM P4000-WRITE-LINE THRU P4000-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '0' TO PC-CC.
           MOVE WS-MT-TEXT (WS-MT-X) TO PC-TEXT.
           PERFORM P4000-WRITE-LINE THRU P4000-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '0' TO PC-CC.
           MOVE WS-PE-CAMPAIGN TO PC-COL-001-020.
           PERFORM P4000-WRITE-LINE THRU P4000-EXIT.
           ADD 1 TO WS-RT-INSERTED.
           ADD 1 TO WS-MW-PAGE-CNT.

       P3200-EXIT.
           EXIT.

      *****************************************************************
      * S400-OUTPUT AND TABLE LOAD                                    *
      *****************************************************************
       S400-SUPPORT SECTION.

       P4000-WRITE-LINE.
           MOVE 'P4000-WRITE-LINE' TO WS-PARA-NAME.
           WRITE PRINT-LINE FROM CABS-PRINT-LINE.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 6902 TO WS-AB-CODE
               MOVE 'MESSAGE PAGE WRITE FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-WRITE-CNT.
           ADD 1 TO WS-RT-LINES.

       P4000-EXIT.
           EXIT.

       P4100-LOAD-MESSAGES.
      * LOAD THE MESSAGE FILE.  THE 1993 CARD LAYOUT CARRIED NO CARRIER
      * TYPE AND NO WINDOW AND IS STILL ACCEPTED - IT IS RECOGNISED BY
      * A NON NUMERIC IN THE FROM DATE POSITION.
           MOVE 'P4100-LOAD-MESSAGES' TO WS-PARA-NAME.
           MOVE ZERO TO WS-MT-USED.
           PERFORM P4110-READ-MSG THRU P4110-EXIT
               UNTIL WS-MSG-EOF.
           DISPLAY 'MESSAGES LOADED ' WS-MT-USED.

       P4100-EXIT.
           EXIT.

       P4110-READ-MSG.
           READ MSG-IN-FILE INTO WS-MSG-IN
               AT END
                   MOVE 'Y' TO WS-MSG-EOF-SW
                   GO TO P4110-EXIT.
           IF WS-MSG-IN = SPACES
               GO TO P4110-EXIT.
           IF WS-MT-USED NOT < WS-MT-MAX
               MOVE 6903 TO WS-AB-CODE
               MOVE 'MESSAGE TABLE FULL' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-MT-USED.
           SET WS-MT-X TO WS-MT-USED.
           MOVE WS-MI-CAMPAIGN TO WS-MT-CAMPAIGN (WS-MT-X).
           IF WS-MI-FROM NOT NUMERIC
               MOVE '*'          TO WS-MT-CARR-TYPE (WS-MT-X)
               MOVE WS-MO-SEQ    TO WS-MT-SEQ (WS-MT-X)
               MOVE ZERO         TO WS-MT-FROM-YYDDD (WS-MT-X)
               MOVE ZERO         TO WS-MT-THRU-YYDDD (WS-MT-X)
               MOVE WS-MO-TEXT   TO WS-MT-TEXT (WS-MT-X)
               GO TO P4110-EXIT.
           MOVE WS-MI-CARR-TYPE TO WS-MT-CARR-TYPE (WS-MT-X).
           MOVE WS-MI-SEQ       TO WS-MT-SEQ (WS-MT-X).
           MOVE WS-MI-FROM      TO WS-MT-FROM-YYDDD (WS-MT-X).
           MOVE WS-MI-THRU      TO WS-MT-THRU-YYDDD (WS-MT-X).
           MOVE WS-MI-TEXT      TO WS-MT-TEXT (WS-MT-X).

       P4110-EXIT.
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
           MOVE 'CABFMT09  BILL MESSAGE INSERT REGISTER'
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
           MOVE 'INVOICES     MESSAGES INSERTED   LINES'
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
           MOVE 540                    TO CT-STEP-SEQ.
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
           DISPLAY 'INVOICES EXAMINED ' WS-RT-INVOICES.
           DISPLAY 'MESSAGES INSERTED ' WS-RT-INSERTED.
           DISPLAY 'NO MESSAGE MATCH  ' WS-RT-NO-MESSAGE.
           DISPLAY 'LINES WRITTEN     ' WS-RT-LINES.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE BHDR-IN-FILE
                 MSG-IN-FILE
                 PRINT-STREAM
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

