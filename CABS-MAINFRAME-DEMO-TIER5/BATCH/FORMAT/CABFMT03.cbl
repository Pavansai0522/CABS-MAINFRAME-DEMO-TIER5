      *****************************************************************
      * CABFMT03 - AMOUNT AND QUANTITY EDITING                        *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               BDTLIN  TELCABS.CABS.BILLDTL.SEQ(0)       CABSBILL*
      *               SYSIN   INSTREAM CONTROL CARD             (LOCAL)*
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               PRTOUT  TELCABS.CABS.PRINT.EDIT(+1)       CABSPRNT*
      *               REPORT  SYSOUT PRINT                      CABSPRNT*
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN                                      *
      * REVISION HISTORY                                              *
      *   V1.00  1988-05-30  K.OYELARAN   INITIAL RELEASE - TRAILING MINUS*
      *                      PATTERN AGREED WITH THE CARRIERS         *
      *   V1.06  1991-04-08  D.OKONKWO    CR TAG FORM ADDED FOR THE TWO*
      *                      CARRIERS THAT COULD NOT READ MINUS       *
      *   V1.10  1994-07-19  M.J.FERRARO  ELEMENT DETAIL LINES ADDED UNDER*
      *                      THE SUMMARY LINE                         *
      *   V2.00  1998-12-01  J.M.CASTILLO FLOATING CURRENCY PATTERN ADDED FOR*
      *                      THE CD-ROM BILL                          *
      *   V2.09  2015-06-18  G.PRZYBYLSKI ZERO SUPPRESSION MADE OPTIONAL*
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABFMT03.
       AUTHOR. TELCABS APPLICATIONS - BILL PRINT TEAM.
      *****************************************************************
      * APPLIES THE PRINT EDIT PATTERNS TO THE MONEY AND QUANTITY     *
      * FIELDS AND PRODUCES THE SUMMARY AND ELEMENT DETAIL LINES.     *
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
           05  WS-PGM-NAME             PIC X(08) VALUE 'CABFMT03'.
           05  WS-PGM-VERSION          PIC X(05) VALUE 'V2.09'.
           05  WS-PGM-APPL             PIC X(04) VALUE 'CABS'.
           05  WS-PGM-COMPILE          PIC X(08) VALUE '20150618'.
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
           05  WS-PE-EDIT-STYLE        PIC X(01).
           05  WS-PE-CR-SW             PIC X(01).
           05  WS-PE-ELEM-DETAIL-SW    PIC X(01).
           05  WS-PE-ZERO-SUPP-SW      PIC X(01).
           05  WS-PE-FILLER            PIC X(31).
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
           05  WS-NEGATIVE-SW          PIC X(01) VALUE 'N'.
               88  WS-NEGATIVE         VALUE 'Y'.
      *****************************************************************
      * THE EDIT PATTERNS.  FIVE OF THEM, SELECTED BY THE EDIT STYLE ON*
      * THE CONTROL CARD AND BY THE SIGN OF THE AMOUNT.  THE TRAILING *
      * MINUS PATTERN IS THE ONE THE CARRIERS AGREED TO IN 1988; THE  *
      * CR PATTERN WAS ADDED FOR THE TWO CARRIERS WHOSE RECEIVABLES   *
      * SYSTEMS COULD NOT READ A TRAILING MINUS.                      *
      *****************************************************************
       01  WS-EDIT-PATTERNS.
           05  WS-EP-AMOUNT-MINUS      PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-EP-AMOUNT-CR         PIC ZZ,ZZZ,ZZZ,ZZ9.99.
           05  WS-EP-AMOUNT-FLOAT      PIC $$$,$$$,$$9.99.
           05  WS-EP-QTY               PIC Z,ZZZ,ZZZ,ZZ9.99.
           05  WS-EP-QTY-INT           PIC ZZZ,ZZZ,ZZZ,ZZ9.
           05  WS-EP-RATE              PIC Z.ZZZZ9.
           05  WS-EP-RATE-LONG         PIC ZZZ.ZZZZ9.
           05  WS-EP-PCT               PIC ZZ9.99.
           05  WS-EP-COUNT             PIC ZZZ,ZZ9.
       01  WS-EDIT-WORK.
           05  WS-EW-AMOUNT            PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-EW-ABS-AMOUNT        PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-EW-QTY               PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-EW-RATE              PIC S9(05)V9(05) COMP-3 VALUE 0.
           05  WS-EW-CR-TAG            PIC X(02) VALUE SPACES.
           05  WS-EW-FIELD             PIC X(18) VALUE SPACES.
       01  WS-EDIT-FIELD-R REDEFINES WS-EDIT-WORK.
           05  WS-ER-PACKED            PIC X(21).
           05  WS-ER-TEXT              PIC X(20).
      *****************************************************************
      * THE SCAN AREA.  THE EDITED FIELD IS WALKED CHARACTER BY       *
      * CHARACTER TO FIND THE FIRST SIGNIFICANT DIGIT SO THAT THE     *
      * FLOATING SYMBOL CAN BE PLACED.  NO REFERENCE MODIFICATION.    *
      * EDITING RULES ARE HELD WITH THE BILL FORMAT SPECIFICATION.    *
      *****************************************************************
       01  WS-SCAN-AREA                PIC X(20) VALUE SPACES.
       01  WS-SCAN-AREA-R REDEFINES WS-SCAN-AREA.
           05  WS-SA-CHAR OCCURS 20 TIMES PIC X(01).
       01  WS-SCAN-CTL.
           05  WS-SC-SUB               PIC 9(03) VALUE 0.
           05  WS-SC-FIRST-DIGIT       PIC 9(03) VALUE 0.
           05  WS-SC-BLANKS            PIC 9(03) VALUE 0.
           05  WS-SC-ZEROS             PIC 9(03) VALUE 0.
           05  WS-SC-COMMAS            PIC 9(03) VALUE 0.
           05  WS-SC-DONE-SW           PIC X(01) VALUE 'N'.
               88  WS-SC-DONE          VALUE 'Y'.
       01  WS-ELEM-WORK.
           05  WS-EL-COUNT             PIC 9(03) VALUE 0.
           05  WS-EL-LINE-CNT          PIC S9(09) COMP-3 VALUE 0.
       01  WS-RUN-TOTALS.
           05  WS-RT-LINES             PIC S9(11) COMP-3 VALUE 0.
           05  WS-RT-ELEM-LINES        PIC S9(11) COMP-3 VALUE 0.
           05  WS-RT-CREDIT-LINES      PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-ZERO-LINES        PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-AMOUNT            PIC S9(15)V9(02) COMP-3 VALUE 0.
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
           OPEN OUTPUT PRINT-STREAM
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 6311 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-BDTLIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 6312 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-PRTOUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 6313 TO WS-AB-CODE
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
           DISPLAY '  EDIT STYLE    ' WS-PE-EDIT-STYLE.
           DISPLAY '  CR TAG SWITCH ' WS-PE-CR-SW.

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
      * THE EDIT STYLE IS SUPPLIED BY THE SCHEDULER FROM THE MEDIA
      * PROFILE.  IT HAS NO DEFAULT.
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
           IF WS-PE-EDIT-STYLE = SPACES
               MOVE 6321 TO WS-AB-CODE
               MOVE 'EDIT STYLE NOT SUPPLIED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PE-CR-SW NOT = 'Y'
               MOVE 'N' TO WS-PE-CR-SW.
           IF WS-PE-ELEM-DETAIL-SW NOT = 'N'
               MOVE 'Y' TO WS-PE-ELEM-DETAIL-SW.
           IF WS-PE-ZERO-SUPP-SW NOT = 'Y'
               MOVE 'N' TO WS-PE-ZERO-SUPP-SW.

       P1200-EXIT.
           EXIT.

      *****************************************************************
      * S200-MAIN PROCESSING                                          *
      * READ THE VARIABLE LENGTH DETAIL AND PRODUCE THE EDITED PRINT  *
      * LINES.  A LINE WITH SEVERAL RATE ELEMENTS PRODUCES A SUMMARY  *
      * LINE AND, WHEN THE CONTROL CARD ASKS FOR IT, ONE LINE PER     *
      * ELEMENT UNDERNEATH IT.                                        *
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
           PERFORM P3000-EDIT-SUMMARY THRU P3000-EXIT.
           IF WS-PE-ELEM-DETAIL-SW = 'Y'
               PERFORM P3500-EDIT-ELEMENTS THRU P3500-EXIT.

       P2000-EXIT.
           EXIT.

       P2100-READ-DETAIL.
           MOVE 'P2100-READ-DETAIL' TO WS-PARA-NAME.
           READ BILL-DTL-IN
               AT END
                   MOVE 'Y' TO WS-DTL-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00' AND WS-FS-INPUT NOT = '10'
               MOVE 6301 TO WS-AB-CODE
               MOVE 'BILL DETAIL READ ERROR' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.

       P2100-EXIT.
           EXIT.

      *****************************************************************
      * S300-EDITING                                                  *
      *****************************************************************
       S300-EDIT SECTION.

       P3000-EDIT-SUMMARY.
      * THE SUMMARY LINE FOR THE WHOLE DETAIL RECORD.  THE AMOUNT VIEW
      * OF THE PRINT LINE IS USED SO THAT THE EDITED FIELDS LAND IN THE
      * COLUMNS THE HEADING BLOCK ADVERTISES.
           MOVE 'P3000-EDIT-SUMMARY' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE BD-TOT-ROUNDED TO WS-EW-AMOUNT.
           MOVE BD-TOT-MINUTES TO WS-EW-QTY.
           PERFORM P3100-SIGN-TEST THRU P3100-EXIT.
           PERFORM P3200-EDIT-AMOUNT THRU P3200-EXIT.
           PERFORM P3300-EDIT-QUANTITY THRU P3300-EXIT.
           MOVE BD-DESCRIPTION TO PC-AMT-DESC.
           MOVE WS-EP-QTY      TO PC-AMT-QTY.
           MOVE ZERO           TO PC-AMT-RATE.
           MOVE WS-EP-AMOUNT-MINUS TO PC-AMT-VALUE.
           MOVE WS-EW-CR-TAG   TO PC-AMT-FILL.
           PERFORM P3400-WRITE-LINE THRU P3400-EXIT.
           ADD 1 TO WS-RT-LINES.
           ADD WS-EW-AMOUNT TO WS-RT-AMOUNT.
           ADD WS-EW-AMOUNT TO WS-ACC-AMOUNT.

       P3000-EXIT.
           EXIT.

       P3100-SIGN-TEST.
      * A CREDIT LINE PRINTS EITHER WITH A TRAILING MINUS OR WITH THE
      * LETTERS CR AFTER THE FIGURE, DEPENDING ON THE CONTROL CARD.
      * THE CR FORM PRINTS THE ABSOLUTE VALUE AND CARRIES THE SIGN IN
      * THE TAG, WHICH IS WHY THE ABSOLUTE VALUE IS TAKEN HERE.
           MOVE 'P3100-SIGN-TEST' TO WS-PARA-NAME.
           MOVE 'N' TO WS-NEGATIVE-SW.
           MOVE SPACES TO WS-EW-CR-TAG.
           MOVE WS-EW-AMOUNT TO WS-EW-ABS-AMOUNT.
           IF WS-EW-AMOUNT NOT < ZERO
               GO TO P3100-EXIT.
           MOVE 'Y' TO WS-NEGATIVE-SW.
           COMPUTE WS-EW-ABS-AMOUNT = WS-EW-AMOUNT * -1.
           ADD 1 TO WS-RT-CREDIT-LINES.
           IF WS-PE-CR-SW = 'Y'
               MOVE 'CR' TO WS-EW-CR-TAG.

       P3100-EXIT.
           EXIT.

       P3200-EDIT-AMOUNT.
      * MOVE THE AMOUNT INTO THE EDIT PATTERN.  THE PATTERN CHOSEN
      * DEPENDS ON THE STYLE ON THE CONTROL CARD.  THE EDITED FIELD IS
      * THEN SCANNED SO THAT LEADING BLANKS AND SEPARATORS CAN BE
      * COUNTED FOR THE FLOATING SYMBOL STYLE.
           MOVE 'P3200-EDIT-AMOUNT' TO WS-PARA-NAME.
           IF WS-PE-CR-SW = 'Y'
               MOVE WS-EW-ABS-AMOUNT TO WS-EP-AMOUNT-CR
               MOVE WS-EP-AMOUNT-CR TO WS-EP-AMOUNT-MINUS
               GO TO P3200-EDIT-SCAN.
           IF WS-PE-EDIT-STYLE = 'F'
               MOVE WS-EW-ABS-AMOUNT TO WS-EP-AMOUNT-FLOAT
               MOVE WS-EP-AMOUNT-FLOAT TO WS-EP-AMOUNT-MINUS
               GO TO P3200-EDIT-SCAN.
           MOVE WS-EW-AMOUNT TO WS-EP-AMOUNT-MINUS.

       P3200-EDIT-SCAN.
           MOVE WS-EP-AMOUNT-MINUS TO WS-SCAN-AREA.
           PERFORM P3600-SCAN-EDITED THRU P3600-EXIT.
           IF WS-PE-ZERO-SUPP-SW = 'Y'
               IF WS-EW-AMOUNT = ZERO
                   MOVE SPACES TO WS-SCAN-AREA
                   MOVE WS-SCAN-AREA TO WS-EP-AMOUNT-MINUS
                   ADD 1 TO WS-RT-ZERO-LINES.

       P3200-EXIT.
           EXIT.

       P3300-EDIT-QUANTITY.
      * THE QUANTITY.  MINUTES PRINT TO TWO PLACES; A COUNT OF
      * ELEMENTS PRINTS WHOLE.  THE DISTINCTION IS TAKEN FROM THE
      * SECTION CODE ON THE DETAIL RECORD.
           MOVE 'P3300-EDIT-QUANTITY' TO WS-PARA-NAME.
           MOVE WS-EW-QTY TO WS-EP-QTY.
           IF BD-SECTION = 'C1' OR BD-SECTION = 'C2'
               MOVE WS-EW-QTY TO WS-EP-QTY-INT
               MOVE WS-EP-QTY-INT TO WS-EP-QTY.

       P3300-EXIT.
           EXIT.

       P3400-WRITE-LINE.
           MOVE 'P3400-WRITE-LINE' TO WS-PARA-NAME.
           WRITE PRINT-LINE FROM CABS-PRINT-LINE.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 6302 TO WS-AB-CODE
               MOVE 'PRINT LINE WRITE FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-WRITE-CNT.

       P3400-EXIT.
           EXIT.

       P3500-EDIT-ELEMENTS.
      * ONE PRINT LINE PER RATE ELEMENT UNDERNEATH THE SUMMARY LINE.
      * THE ELEMENT LINES ARE OVERPRINTED WITH A PLUS IN COLUMN ONE
      * WHEN THE CONTROL CARD ASKS FOR THE COMPRESSED FORM, WHICH PUTS
      * THE ELEMENT DETAIL ON THE SAME PHYSICAL LINE AS THE SUMMARY.
      * CARRIAGE CONTROL PER CABS-STD-063 AND THE MAILROOM SPEC.
      * VARIABLE LENGTH HANDLING PER CABS-STD-019.
           MOVE 'P3500-EDIT-ELEMENTS' TO WS-PARA-NAME.
           MOVE BD-ELEM-CNT TO WS-EL-COUNT.
           IF WS-EL-COUNT < 1
               MOVE 1 TO WS-EL-COUNT.
           IF WS-EL-COUNT > 40
               MOVE 40 TO WS-EL-COUNT.
           PERFORM P3510-ONE-ELEMENT THRU P3510-EXIT
               VARYING WS-SUB1 FROM 1 BY 1
               UNTIL WS-SUB1 > WS-EL-COUNT.

       P3500-EXIT.
           EXIT.

       P3510-ONE-ELEMENT.
           SET BD-EX TO WS-SUB1.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           IF WS-SUB1 = 1
               IF WS-PE-EDIT-STYLE = 'C'
                   MOVE '+' TO PC-CC.
           MOVE BD-EL-RATE-ELEM (BD-EX) TO PC-AMT-DESC.
           MOVE BD-EL-QTY (BD-EX)       TO WS-EP-QTY.
           MOVE WS-EP-QTY               TO PC-AMT-QTY.
           MOVE BD-EL-RATE (BD-EX)      TO WS-EP-RATE.
           MOVE WS-EP-RATE              TO PC-AMT-RATE.
           MOVE BD-EL-AMOUNT (BD-EX)    TO WS-EW-AMOUNT.
           MOVE WS-EW-AMOUNT            TO WS-EP-AMOUNT-MINUS.
           MOVE WS-EP-AMOUNT-MINUS      TO PC-AMT-VALUE.
           MOVE SPACES                  TO PC-AMT-FILL.
           PERFORM P3400-WRITE-LINE THRU P3400-EXIT.
           ADD 1 TO WS-RT-ELEM-LINES.

       P3510-EXIT.
           EXIT.

       P3600-SCAN-EDITED.
      * WALK THE EDITED FIELD AND COUNT THE LEADING BLANKS, THE
      * SUPPRESSED ZEROS AND THE SEPARATORS.  THE POSITION OF THE FIRST
      * SIGNIFICANT CHARACTER IS WHERE A FLOATING SYMBOL WOULD SIT.
           MOVE 'P3600-SCAN-EDITED' TO WS-PARA-NAME.
           MOVE ZERO TO WS-SC-BLANKS WS-SC-ZEROS WS-SC-COMMAS
                        WS-SC-FIRST-DIGIT.
           INSPECT WS-SCAN-AREA
               TALLYING WS-SC-BLANKS FOR LEADING SPACE.
           INSPECT WS-SCAN-AREA
               TALLYING WS-SC-COMMAS FOR ALL ','.
           INSPECT WS-SCAN-AREA
               TALLYING WS-SC-ZEROS FOR ALL '0'.
           MOVE 'N' TO WS-SC-DONE-SW.
           MOVE 1 TO WS-SC-SUB.
           PERFORM P3610-SCAN-ONE THRU P3610-EXIT
               UNTIL WS-SC-DONE OR WS-SC-SUB > 20.
           MOVE WS-SC-SUB TO WS-SC-FIRST-DIGIT.

       P3600-EXIT.
           EXIT.

       P3610-SCAN-ONE.
           IF WS-SA-CHAR (WS-SC-SUB) NOT = SPACE
               MOVE 'Y' TO WS-SC-DONE-SW
               GO TO P3610-EXIT.
           ADD 1 TO WS-SC-SUB.

       P3610-EXIT.
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
           MOVE 'CABFMT03  AMOUNT EDITING REGISTER'
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
           MOVE 'SUMMARY LINES   ELEMENT LINES   CREDIT LINES'
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
           MOVE 510                    TO CT-STEP-SEQ.
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
           DISPLAY 'SUMMARY LINES     ' WS-RT-LINES.
           DISPLAY 'ELEMENT LINES     ' WS-RT-ELEM-LINES.
           DISPLAY 'CREDIT LINES      ' WS-RT-CREDIT-LINES.
           DISPLAY 'ZERO SUPPRESSED   ' WS-RT-ZERO-LINES.
           DISPLAY 'VALUE EDITED      ' WS-RT-AMOUNT.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE BILL-DTL-IN
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

