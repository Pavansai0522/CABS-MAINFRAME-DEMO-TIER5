      *****************************************************************
      * CABFMT02 - HEADING AND CONTINUATION PAGE INJECTION            *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               PRTIN   TELCABS.CABS.PRINT.STREAM(0)      CABSPRNT*
      *               SYSIN   INSTREAM CONTROL CARD             (LOCAL)*
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               PRTOUT  TELCABS.CABS.PRINT.HEAD(+1)       CABSPRNT*
      *               REPORT  SYSOUT PRINT                      CABSPRNT*
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN                                      *
      * REVISION HISTORY                                              *
      *   V1.00  1988-04-12  K.OYELARAN   INITIAL RELEASE - ONE HEADING LINE*
      *   V1.05  1992-02-20  D.OKONKWO    SIX LINE HEADING BLOCK INTRODUCED*
      *   V1.09  1995-10-31  M.J.FERRARO  CONTINUATION WORDING ADDED - CARRIERS*
      *                      WERE FILING PAGES OUT OF ORDER           *
      *   V2.00  1999-03-09  J.M.CASTILLO COMPANY NAME MOVED TO THE CONTROL*
      *                      CARD FOR THE SECOND OPERATING CO         *
      *   V2.06  2013-02-21  G.PRZYBYLSKI SECTION BREAK CHARACTER NO LONGER*
      *                      DEMOTED WHEN A HEADING IS INJECTED       *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABFMT02.
       AUTHOR. TELCABS APPLICATIONS - BILL PRINT TEAM.
      *****************************************************************
      * INJECTS THE HEADING BLOCK AT THE TOP OF EVERY PAGE AND THE    *
      * CONTINUATION WORDING ON EVERY PAGE AFTER THE FIRST.           *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRINT-IN-FILE ASSIGN TO UT-S-PRTIN
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
      * PRTIN - THE RAW PRINT STREAM FROM CABFMT01.                   *
      *****************************************************************
       FD  PRINT-IN-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       01  PRINT-IN-REC                     PIC X(133).
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
           05  WS-PGM-NAME             PIC X(08) VALUE 'CABFMT02'.
           05  WS-PGM-VERSION          PIC X(05) VALUE 'V2.06'.
           05  WS-PGM-APPL             PIC X(04) VALUE 'CABS'.
           05  WS-PGM-COMPILE          PIC X(08) VALUE '20130221'.
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
           05  WS-PE-LINES-PAGE        PIC 9(03).
           05  WS-PE-CONT-SW           PIC X(01).
           05  WS-PE-REMIT-SW          PIC X(01).
           05  WS-PE-COMPANY           PIC X(30).
           05  WS-PE-FILLER            PIC X(00).
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
           05  WS-PRT-EOF-SW           PIC X(01) VALUE 'N'.
               88  WS-PRT-EOF          VALUE 'Y'.
           05  WS-CONT-SW              PIC X(01) VALUE 'N'.
               88  WS-CONTINUING       VALUE 'Y'.
           05  WS-HDR-DONE-SW          PIC X(01) VALUE 'N'.
               88  WS-HDR-DONE         VALUE 'Y'.
      *****************************************************************
      * HEADING BLOCK.  SIX LINES AT THE TOP OF EVERY PAGE.  THE FIRST*
      * PAGE OF AN INVOICE CARRIES THE FULL BLOCK; EVERY PAGE AFTER IT*
      * CARRIES THE SHORT BLOCK WITH THE CONTINUATION WORDING.        *
      * PAGE CONTROL FOLLOWS CABS-STD-063.                            *
      *****************************************************************
       01  WS-HEAD-WORK.
           05  WS-HW-PAGE              PIC S9(05) COMP-3 VALUE 0.
           05  WS-HW-LINE              PIC S9(05) COMP-3 VALUE 0.
           05  WS-HW-INVOICE           PIC X(14) VALUE SPACES.
           05  WS-HW-BAN               PIC X(13) VALUE SPACES.
           05  WS-HW-PRIOR-BAN         PIC X(13) VALUE SPACES.
           05  WS-HW-PERIOD-TEXT       PIC X(09) VALUE SPACES.
           05  WS-HW-DUE-TEXT          PIC X(09) VALUE SPACES.
           05  WS-HW-CC                PIC X(01) VALUE SPACES.
      *****************************************************************
      * HEADING TEXT ASSEMBLY.  THE TITLE LINE IS BUILT FROM THE      *
      * COMPANY NAME ON THE CONTROL CARD, THE DOCUMENT WORDING AND THE*
      * BILL PERIOD.                                                  *
      *****************************************************************
       01  WS-HEAD-PIECES.
           05  WS-HP-COMPANY           PIC X(30) VALUE SPACES.
           05  WS-HP-DOCTYPE           PIC X(24) VALUE SPACES.
           05  WS-HP-PERIOD            PIC X(09) VALUE SPACES.
           05  WS-HP-PAGE-TEXT         PIC X(12) VALUE SPACES.
           05  WS-HP-CONT-TEXT         PIC X(13) VALUE SPACES.
       01  WS-HEAD-LINE                PIC X(132) VALUE SPACES.
       01  WS-HEAD-LINE-R REDEFINES WS-HEAD-LINE.
           05  WS-HL-CHAR OCCURS 132 TIMES PIC X(01).
       01  WS-HEAD-CTL.
           05  WS-HC-PTR               PIC 9(03) VALUE 1.
           05  WS-HC-LEN               PIC 9(03) VALUE 0.
           05  WS-HC-EDIT-PAGE         PIC ZZZ9.
       01  WS-MONTH-NAME-TABLE.
           05  FILLER PIC X(36) VALUE
               'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'.
       01  WS-MONTH-NAME-R REDEFINES WS-MONTH-NAME-TABLE.
           05  WS-MN-NAME OCCURS 12 TIMES PIC X(03).
       01  WS-DATE-SPLIT.
           05  WS-DS-CCYY              PIC 9(04) VALUE 0.
           05  WS-DS-MM                PIC 9(02) VALUE 0.
           05  WS-DS-DD                PIC 9(02) VALUE 0.
       01  WS-RUN-TOTALS.
           05  WS-RT-PAGES             PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-FIRST-PAGES       PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-CONT-PAGES        PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-HEAD-LINES        PIC S9(11) COMP-3 VALUE 0.
           05  WS-RT-BODY-LINES        PIC S9(11) COMP-3 VALUE 0.
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
           OPEN INPUT  PRINT-IN-FILE
                       PARM-FILE
           OPEN OUTPUT PRINT-STREAM
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 6211 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-PRTIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 6212 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-PRTOUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 6213 TO WS-AB-CODE
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
           DISPLAY '  COMPANY NAME  ' WS-PE-COMPANY.

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
      * THE COMPANY NAME IS SUPPLIED BY THE SCHEDULER.  THE SAME LOAD
      * MODULE PRINTS FOR BOTH OPERATING COMPANIES AND THERE IS NO
      * DEFAULT - AN ABSENT NAME STOPS THE STEP.
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
           IF WS-PE-COMPANY = SPACES
               MOVE 6221 TO WS-AB-CODE
               MOVE 'COMPANY NAME NOT SUPPLIED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PE-LINES-PAGE NOT NUMERIC
               MOVE 60 TO WS-PE-LINES-PAGE.
           IF WS-PE-CONT-SW NOT = 'N'
               MOVE 'Y' TO WS-PE-CONT-SW.

       P1200-EXIT.
           EXIT.

      *****************************************************************
      * S200-MAIN PROCESSING                                          *
      * READ THE RAW PRINT STREAM AND INJECT THE HEADING BLOCKS.  THE *
      * CARRIAGE CONTROL CHARACTER ON THE INCOMING LINE TELLS THIS    *
      * PROGRAM WHERE THE PAGE AND INVOICE BOUNDARIES ARE.            *
      *****************************************************************
       S200-MAIN-PROCESS SECTION.

       P2000-PROCESS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ-LINE THRU P2100-EXIT.
           IF WS-PRT-EOF
               MOVE 'Y' TO WS-EOF-SW
               GO TO P2000-EXIT.
           ADD 1 TO WS-READ-CNT.
           PERFORM P3000-BOUNDARY-TEST THRU P3000-EXIT.
           IF WS-HDR-DONE
               PERFORM P3100-EMIT-HEADING THRU P3100-EXIT.
           PERFORM P3500-EMIT-BODY THRU P3500-EXIT.

       P2000-EXIT.
           EXIT.

       P2100-READ-LINE.
           MOVE 'P2100-READ-LINE' TO WS-PARA-NAME.
           READ PRINT-IN-FILE INTO CABS-PRINT-LINE
               AT END
                   MOVE 'Y' TO WS-PRT-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00' AND WS-FS-INPUT NOT = '10'
               MOVE 6201 TO WS-AB-CODE
               MOVE 'PRINT STREAM READ ERROR' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.

       P2100-EXIT.
           EXIT.

      *****************************************************************
      * S300-BOUNDARY DETECTION AND HEADING INJECTION                 *
      *****************************************************************
       S300-HEADING SECTION.

       P3000-BOUNDARY-TEST.
      * DECIDE WHETHER THIS LINE STARTS A PAGE.  A SEVEN IS A NEW
      * INVOICE AND ALWAYS STARTS A PAGE WITH THE FULL HEADING.  A ONE
      * IS A NEW PAGE INSIDE THE SAME INVOICE AND TAKES THE SHORT
      * HEADING WITH THE CONTINUATION WORDING.  A FOUR IS A SECTION
      * BREAK AND DOES NOT ON ITS OWN START A PAGE.
           MOVE 'P3000-BOUNDARY-TEST' TO WS-PARA-NAME.
           MOVE 'N' TO WS-HDR-DONE-SW.
           MOVE PC-CC TO WS-HW-CC.
           IF PC-NEW-INVOICE
               MOVE 'N' TO WS-CONT-SW
               MOVE 1 TO WS-HW-PAGE
               MOVE 'Y' TO WS-HDR-DONE-SW
               MOVE PC-COL-001-020 TO WS-HW-INVOICE
               MOVE PC-COL-021-060 TO WS-HW-BAN
               ADD 1 TO WS-RT-FIRST-PAGES
               GO TO P3000-EXIT.
           IF PC-NEW-PAGE
               MOVE 'Y' TO WS-CONT-SW
               ADD 1 TO WS-HW-PAGE
               MOVE 'Y' TO WS-HDR-DONE-SW
               ADD 1 TO WS-RT-CONT-PAGES
               GO TO P3000-EXIT.
           IF WS-HW-LINE > WS-PE-LINES-PAGE
               MOVE 'Y' TO WS-CONT-SW
               ADD 1 TO WS-HW-PAGE
               MOVE 'Y' TO WS-HDR-DONE-SW
               ADD 1 TO WS-RT-CONT-PAGES.

       P3000-EXIT.
           EXIT.

       P3100-EMIT-HEADING.
      * WRITE THE HEADING BLOCK.  SIX LINES ON THE FIRST PAGE OF AN
      * INVOICE, FOUR ON A CONTINUATION PAGE.  THE BLOCK ALWAYS STARTS
      * ON A NEW PHYSICAL PAGE.
           MOVE 'P3100-EMIT-HEADING' TO WS-PARA-NAME.
           PERFORM P3200-BUILD-TITLE THRU P3200-EXIT.
           PERFORM P3300-WRITE-TITLE THRU P3300-EXIT.
           PERFORM P3400-WRITE-COLUMNS THRU P3400-EXIT.
           MOVE ZERO TO WS-HW-LINE.
           ADD 1 TO WS-RT-PAGES.

       P3100-EXIT.
           EXIT.

       P3200-BUILD-TITLE.
      * ASSEMBLE THE TITLE LINE.  THE COMPANY NAME ARRIVES ON THE
      * CONTROL CARD SO THAT THE SAME LOAD MODULE CAN PRINT FOR THE
      * TWO OPERATING COMPANIES.  A CONTINUATION PAGE CARRIES THE WORD
      * CONTINUED AT THE END OF THE TITLE.
           MOVE 'P3200-BUILD-TITLE' TO WS-PARA-NAME.
           MOVE WS-PE-COMPANY TO WS-HP-COMPANY.
           MOVE 'CARRIER ACCESS BILL' TO WS-HP-DOCTYPE.
           PERFORM P4000-PERIOD-TEXT THRU P4000-EXIT.
           MOVE WS-HW-PERIOD-TEXT TO WS-HP-PERIOD.
           MOVE WS-HW-PAGE TO WS-HC-EDIT-PAGE.
           MOVE SPACES TO WS-HP-PAGE-TEXT.
           STRING 'PAGE '        DELIMITED BY SIZE
                  WS-HC-EDIT-PAGE DELIMITED BY SIZE
                  INTO WS-HP-PAGE-TEXT.
           MOVE SPACES TO WS-HP-CONT-TEXT.
           IF WS-CONTINUING
               MOVE ' (CONTINUED)' TO WS-HP-CONT-TEXT.
           MOVE SPACES TO WS-HEAD-LINE.
           MOVE 1 TO WS-HC-PTR.
           STRING WS-HP-COMPANY   DELIMITED BY '  '
                  '  '            DELIMITED BY SIZE
                  WS-HP-DOCTYPE   DELIMITED BY '  '
                  '  '            DELIMITED BY SIZE
                  WS-HP-PERIOD    DELIMITED BY SIZE
                  '  '            DELIMITED BY SIZE
                  WS-HP-PAGE-TEXT DELIMITED BY SIZE
                  WS-HP-CONT-TEXT DELIMITED BY SIZE
                  INTO WS-HEAD-LINE
                  WITH POINTER WS-HC-PTR
               ON OVERFLOW
                  MOVE 132 TO WS-HC-PTR.
           COMPUTE WS-HC-LEN = WS-HC-PTR - 1.

       P3200-EXIT.
           EXIT.

       P3300-WRITE-TITLE.
           MOVE 'P3300-WRITE-TITLE' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '1' TO PC-CC.
           MOVE WS-HEAD-LINE TO PC-TEXT.
           WRITE PRINT-LINE FROM CABS-PRINT-LINE.
           ADD 1 TO WS-WRITE-CNT.
           ADD 1 TO WS-RT-HEAD-LINES.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-HW-INVOICE TO PC-COL-001-020.
           MOVE WS-HW-BAN     TO PC-COL-021-060.
           MOVE WS-HW-DUE-TEXT TO PC-COL-061-090.
           WRITE PRINT-LINE FROM CABS-PRINT-LINE.
           ADD 1 TO WS-WRITE-CNT.
           ADD 1 TO WS-RT-HEAD-LINES.

       P3300-EXIT.
           EXIT.

       P3400-WRITE-COLUMNS.
      * THE COLUMN HEADINGS.  THE POSITIONS MATCH THE AMOUNT VIEW OF
      * THE PRINT LINE AND MUST NOT BE MOVED WITHOUT MOVING THE EDIT
      * PATTERNS IN CABFMT03 AS WELL.
           MOVE 'P3400-WRITE-COLUMNS' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '0' TO PC-CC.
           MOVE 'DESCRIPTION' TO PC-COL-001-020.
           MOVE 'QUANTITY' TO PC-COL-061-090.
           MOVE 'RATE            AMOUNT' TO PC-COL-091-132.
           WRITE PRINT-LINE FROM CABS-PRINT-LINE.
           ADD 1 TO WS-WRITE-CNT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE ALL '-' TO PC-TEXT.
           WRITE PRINT-LINE FROM CABS-PRINT-LINE.
           ADD 1 TO WS-WRITE-CNT.
           ADD 2 TO WS-RT-HEAD-LINES.

       P3400-EXIT.
           EXIT.

       P3500-EMIT-BODY.
      * WRITE THE ORIGINAL LINE THROUGH.  A LINE THAT TRIGGERED A
      * HEADING BLOCK IS DEMOTED TO SINGLE SPACING SO THAT IT DOES NOT
      * THROW A SECOND PAGE IMMEDIATELY AFTER THE HEADING.  THE SECTION
      * BREAK CHARACTER IS LEFT ALONE - THE BURST PROCESS STILL NEEDS
      * TO SEE IT.
           MOVE 'P3500-EMIT-BODY' TO WS-PARA-NAME.
           IF WS-HDR-DONE
               IF NOT PC-NEW-SECTION
                   MOVE ' ' TO PC-CC.
           WRITE PRINT-LINE FROM CABS-PRINT-LINE.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 6202 TO WS-AB-CODE
               MOVE 'PRINT STREAM WRITE FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-WRITE-CNT.
           ADD 1 TO WS-HW-LINE.
           ADD 1 TO WS-RT-BODY-LINES.

       P3500-EXIT.
           EXIT.

      *****************************************************************
      * S400-DATE TEXT                                                *
      *****************************************************************
       S400-SUPPORT SECTION.

       P4000-PERIOD-TEXT.
      * THE BILL PERIOD IN MON YYYY FORM FOR THE TITLE LINE.
           MOVE 'P4000-PERIOD-TEXT' TO WS-PARA-NAME.
           MOVE 'JG' TO WS-DP-FUNCTION.
           MOVE WS-CYCLE-YYDDD TO WS-DP-YYDDD.
           CALL 'CABDATCV' USING WS-DATE-PARM.
           IF WS-DP-RC NOT = ZERO
               MOVE 'UNKNOWN  ' TO WS-HW-PERIOD-TEXT
               GO TO P4000-EXIT.
           MOVE WS-DP-CCYYMMDD TO WS-DATE-SPLIT.
           IF WS-DS-MM < 1 OR WS-DS-MM > 12
               MOVE 1 TO WS-DS-MM.
           MOVE SPACES TO WS-HW-PERIOD-TEXT.
           STRING WS-MN-NAME (WS-DS-MM) DELIMITED BY SIZE
                  ' '                   DELIMITED BY SIZE
                  WS-DS-CCYY            DELIMITED BY SIZE
                  INTO WS-HW-PERIOD-TEXT.

       P4000-EXIT.
           EXIT.

       P4100-DUE-TEXT.
      * THE DUE DATE FOR THE SECOND HEADING LINE.  TAKEN FROM THE
      * HEADER WHEN ONE IS AVAILABLE AND LEFT BLANK WHEN IT IS NOT.
           MOVE 'P4100-DUE-TEXT' TO WS-PARA-NAME.
           IF BH-DUE-YYDDD = ZERO
               MOVE SPACES TO WS-HW-DUE-TEXT
               GO TO P4100-EXIT.
           MOVE 'JG' TO WS-DP-FUNCTION.
           MOVE BH-DUE-YYDDD TO WS-DP-YYDDD.
           CALL 'CABDATCV' USING WS-DATE-PARM.
           MOVE WS-DP-CCYYMMDD TO WS-DATE-SPLIT.
           IF WS-DS-MM < 1 OR WS-DS-MM > 12
               MOVE 1 TO WS-DS-MM.
           MOVE SPACES TO WS-HW-DUE-TEXT.
           STRING WS-DS-DD              DELIMITED BY SIZE
                  ' '                   DELIMITED BY SIZE
                  WS-MN-NAME (WS-DS-MM) DELIMITED BY SIZE
                  ' '                   DELIMITED BY SIZE
                  WS-DS-CCYY            DELIMITED BY SIZE
                  INTO WS-HW-DUE-TEXT.

       P4100-EXIT.
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
           MOVE 'CABFMT02  HEADING INJECTION REGISTER'
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
           MOVE 'PAGES        FIRST PAGES     CONTINUATION PAGES'
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
           MOVE 505                    TO CT-STEP-SEQ.
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
           DISPLAY 'PAGES HEADED      ' WS-RT-PAGES.
           DISPLAY 'FIRST PAGES       ' WS-RT-FIRST-PAGES.
           DISPLAY 'CONTINUATION PAGES' WS-RT-CONT-PAGES.
           DISPLAY 'HEADING LINES     ' WS-RT-HEAD-LINES.
           DISPLAY 'BODY LINES        ' WS-RT-BODY-LINES.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE PRINT-IN-FILE
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

