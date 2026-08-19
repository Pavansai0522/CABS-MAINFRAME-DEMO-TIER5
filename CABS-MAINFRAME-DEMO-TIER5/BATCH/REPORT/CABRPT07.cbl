      *****************************************************************
      * CABRPT07 - UNBILLED USAGE REPORT                              *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               RATIN   TELCABS.CABS.RATED.JUR(0)         (LOCAL)*
      *               TRIGIN  TELCABS.CABS.BILLTRIG(0)          (LOCAL)*
      *               SYSIN   INSTREAM CONTROL CARD             (LOCAL)*
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               UNBOUT  SYSOUT PRINT - UNBILLED REPORT    CABSPRNT*
      *               REPORT  SYSOUT PRINT - RUN REGISTER       CABSPRNT*
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN                                      *
      * REVISION HISTORY                                              *
      *   V1.00  1994-01-24  D.OKONKWO    INITIAL RELEASE - COUNT ONLY*
      *   V1.05  1998-08-11  J.M.CASTILLO REASON CODES INTRODUCED     *
      *   V2.00  2003-10-06  A.BUKOWSKI   VALUE OF UNBILLED USAGE ADDED FOR*
      *                      REVENUE ASSURANCE                        *
      *   V2.03  2008-10-08  G.PRZYBYLSKI DETAIL LISTING MADE OPTIONAL*
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABRPT07.
       AUTHOR. TELCABS APPLICATIONS - BILLING CONTROL TEAM.
      *****************************************************************
      * REPORTS RATED USAGE THAT DID NOT REACH AN INVOICE THIS CYCLE, *
      * CLASSIFIED BY REASON, WITH THE VALUE OF THE UNBILLED REVENUE. *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RAT-IN-FILE ASSIGN TO UT-S-RATIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT TRG-IN-FILE ASSIGN TO UT-S-TRIGIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-TABLE.
           SELECT UNB-OUT-FILE ASSIGN TO UT-S-UNBOUT
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
      * RATIN - RATED AND JURISDICTIONALISED USAGE.                   *
      *****************************************************************
       FD  RAT-IN-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       01  CABS-RATED-DETAIL-RECORD.
           05  RD-KEY.
               10  RD-BAN              PIC X(13).
               10  RD-BILL-PERIOD      PIC 9(06).
               10  RD-SECTION          PIC X(02).
               10  RD-LINE-SEQ         PIC 9(07) COMP-3.
           05  RD-OCN                  PIC X(04).
           05  RD-JURIS-CD             PIC X(01).
           05  RD-STATE-CD             PIC X(02).
           05  RD-RATE-ELEM            PIC X(06).
           05  RD-ELEM-SEQ             PIC 9(02).
           05  RD-QTY                  PIC S9(13)V9(02) COMP-3.
           05  RD-RATE                 PIC S9(05)V9(05) COMP-3.
           05  RD-AMOUNT               PIC S9(11)V9(05) COMP-3.
           05  RD-ROUND-RULE           PIC X(01).
           05  RD-SRC-PROCESS          PIC X(08).
           05  RD-CYCLE-YYDDD          PIC 9(05).
           05  RD-FILLER               PIC X(123).
      *****************************************************************
      * TRIGIN - THE TRIGGER AND SKIP FILE FROM CABBIL01.             *
      *****************************************************************
       FD  TRG-IN-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       01  TRG-RECORD                       PIC X(200).
      *****************************************************************
      * UNBOUT - THE UNBILLED REPORT.                                 *
      *****************************************************************
       FD  UNB-OUT-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       01  UNB-RECORD                       PIC X(133).
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
           05  WS-PGM-NAME             PIC X(08) VALUE 'CABRPT07'.
           05  WS-PGM-VERSION          PIC X(05) VALUE 'V2.03'.
           05  WS-PGM-APPL             PIC X(04) VALUE 'CABS'.
           05  WS-PGM-COMPILE          PIC X(08) VALUE '20081008'.
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
           05  WS-PE-AGE-LIMIT         PIC 9(03).
           05  WS-PE-REASON-SEL        PIC X(02).
           05  WS-PE-MIN-VALUE         PIC 9(09)V9(02).
           05  WS-PE-DETAIL-SW         PIC X(01).
           05  WS-PE-FILLER            PIC X(23).
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
           05  WS-RAT-EOF-SW           PIC X(01) VALUE 'N'.
               88  WS-RAT-EOF          VALUE 'Y'.
           05  WS-TRG-EOF-SW           PIC X(01) VALUE 'N'.
               88  WS-TRG-EOF          VALUE 'Y'.
           05  WS-TRIG-FOUND-SW        PIC X(01) VALUE 'N'.
               88  WS-TRIG-FOUND       VALUE 'Y'.
      *****************************************************************
      * UNBILLED REASON CODES.  RATED USAGE THAT DID NOT REACH A BILL *
      * IS ALWAYS FOR ONE OF THESE REASONS.  THE FIGURE MATTERS TO THE*
      * REVENUE ASSURANCE TEAM - IT IS EARNED REVENUE THAT HAS NOT    *
      * BEEN INVOICED.                                                *
      *****************************************************************
       01  WS-REASON-TABLE.
           05  FILLER PIC X(34) VALUE
               'U1ACCOUNT NOT TRIGGERED THIS CYCLE'.
           05  FILLER PIC X(34) VALUE
               'U2ACCOUNT ON CREDIT HOLD          '.
           05  FILLER PIC X(34) VALUE
               'U3INVOICE HELD BY PRE BILL AUDIT  '.
           05  FILLER PIC X(34) VALUE
               'U4USAGE DATED AFTER THE CYCLE     '.
           05  FILLER PIC X(34) VALUE
               'U5RATE ELEMENT NOT ON THE TARIFF  '.
           05  FILLER PIC X(34) VALUE
               'U6ACCOUNT CLOSED BEFORE THE CYCLE '.
       01  WS-REASON-TABLE-R REDEFINES WS-REASON-TABLE.
           05  WS-RN-ENTRY OCCURS 6 TIMES INDEXED BY WS-RN-X.
               10  WS-RN-CODE          PIC X(02).
               10  WS-RN-TEXT          PIC X(32).
       01  WS-REASON-TOTALS.
           05  WS-RC-ENTRY OCCURS 6 TIMES.
               10  WS-RC-COUNT         PIC S9(11) COMP-3.
               10  WS-RC-AMOUNT        PIC S9(15)V9(05) COMP-3.
               10  WS-RC-MINUTES       PIC S9(15)V9(02) COMP-3.
       01  WS-TRIG-TABLE.
           05  WS-TT-ENTRY OCCURS 2000 TIMES INDEXED BY WS-TT-X.
               10  WS-TT-BAN           PIC X(13).
               10  WS-TT-CODE          PIC X(02).
       01  WS-TRIG-CTL.
           05  WS-TT-USED              PIC S9(05) COMP-3 VALUE 0.
           05  WS-TT-MAX               PIC S9(05) COMP-3 VALUE 2000.
           05  WS-TT-HIT               PIC S9(05) COMP-3 VALUE 0.
       01  WS-TRIGGER-IN               PIC X(200) VALUE SPACES.
       01  WS-TRIGGER-IN-R REDEFINES WS-TRIGGER-IN.
           05  WS-TI-BAN               PIC X(13).
           05  WS-TI-PERIOD            PIC 9(06).
           05  WS-TI-OCN               PIC X(04).
           05  WS-TI-REST              PIC X(154).
           05  WS-TI-CODE              PIC X(02).
           05  WS-TI-TAIL              PIC X(21).
       01  WS-UNBILL-WORK.
           05  WS-UW-REASON            PIC X(02) VALUE SPACES.
           05  WS-UW-SUB               PIC S9(03) COMP-3 VALUE 0.
           05  WS-UW-FOUND-SW          PIC X(01) VALUE 'N'.
               88  WS-UW-FOUND         VALUE 'Y'.
       01  WS-RUN-TOTALS.
           05  WS-RT-READ              PIC S9(11) COMP-3 VALUE 0.
           05  WS-RT-BILLED            PIC S9(11) COMP-3 VALUE 0.
           05  WS-RT-UNBILLED          PIC S9(11) COMP-3 VALUE 0.
           05  WS-RT-LISTED            PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-AMOUNT            PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-RT-MINUTES           PIC S9(15)V9(02) COMP-3 VALUE 0.
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
           OPEN INPUT  RAT-IN-FILE
                       TRG-IN-FILE
                       PARM-FILE
           OPEN OUTPUT UNB-OUT-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 7611 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-RATIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 7612 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-TRIGIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 7613 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-UNBOUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 7614 TO WS-AB-CODE
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
           PERFORM P4000-LOAD-TRIGGERS THRU P4000-EXIT.
           PERFORM P5400-CLEAR-REASONS THRU P5400-EXIT
               VARYING WS-SUB1 FROM 1 BY 1
               UNTIL WS-SUB1 > 6.
           PERFORM P6000-HEADING THRU P6000-EXIT.
           DISPLAY WS-PGM-NAME ' STARTED - RUN ' WS-RUN-ID.
           DISPLAY '  AGE LIMIT     ' WS-PE-AGE-LIMIT.
           DISPLAY '  MIN VALUE     ' WS-PE-MIN-VALUE.

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
      * THE AGE LIMIT COMES FROM THE REVENUE ASSURANCE STANDARD
      * THROUGH THE SCHEDULER.  IT HAS NO DEFAULT.
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
           IF WS-PE-AGE-LIMIT NOT NUMERIC
               MOVE 7621 TO WS-AB-CODE
               MOVE 'AGE LIMIT NOT NUMERIC' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PE-MIN-VALUE NOT NUMERIC
               MOVE ZERO TO WS-PE-MIN-VALUE.
           IF WS-PE-DETAIL-SW NOT = 'Y'
               MOVE 'N' TO WS-PE-DETAIL-SW.

       P1200-EXIT.
           EXIT.

      *****************************************************************
      * S200-MAIN PROCESSING                                          *
      * WALK THE RATED USAGE FILE AND REPORT EVERYTHING THAT DID NOT  *
      * REACH AN INVOICE THIS CYCLE.                                  *
      *****************************************************************
       S200-MAIN-PROCESS SECTION.

       P2000-PROCESS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ-RATED THRU P2100-EXIT.
           IF WS-RAT-EOF
               MOVE 'Y' TO WS-EOF-SW
               GO TO P2000-EXIT.
           ADD 1 TO WS-READ-CNT.
           ADD 1 TO WS-RT-READ.
           MOVE RD-BAN TO WS-RESTART-KEY.
           PERFORM P3000-CHECK-TRIGGER THRU P3000-EXIT.
           IF WS-TRIG-FOUND
               ADD 1 TO WS-RT-BILLED
               ADD 1 TO WS-CFWD-CNT
               GO TO P2000-EXIT.
           PERFORM P3200-DERIVE-REASON THRU P3200-EXIT.
           PERFORM P3400-ACCUM-UNBILLED THRU P3400-EXIT.
           IF WS-PE-DETAIL-SW = 'Y'
               PERFORM P5000-DETAIL-LINE THRU P5000-EXIT.
           ADD 1 TO WS-SUMM-CNT.

       P2000-EXIT.
           EXIT.

       P2100-READ-RATED.
           MOVE 'P2100-READ-RATED' TO WS-PARA-NAME.
           READ RAT-IN-FILE
               AT END
                   MOVE 'Y' TO WS-RAT-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00' AND WS-FS-INPUT NOT = '10'
               MOVE 7601 TO WS-AB-CODE
               MOVE 'RATED USAGE READ ERROR' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.

       P2100-EXIT.
           EXIT.

      *****************************************************************
      * S300-UNBILLED CLASSIFICATION                                  *
      *****************************************************************
       S300-CLASSIFY SECTION.

       P3000-CHECK-TRIGGER.
           MOVE 'P3000-CHECK-TRIGGER' TO WS-PARA-NAME.
           MOVE 'N' TO WS-TRIG-FOUND-SW.
           MOVE ZERO TO WS-TT-HIT.
           PERFORM P3010-MATCH-TRIG THRU P3010-EXIT
               VARYING WS-TT-X FROM 1 BY 1
               UNTIL WS-TT-X > WS-TT-USED OR WS-TRIG-FOUND.

       P3000-EXIT.
           EXIT.

       P3010-MATCH-TRIG.
           IF WS-TT-BAN (WS-TT-X) NOT = RD-BAN
               GO TO P3010-EXIT.
           SET WS-SUB1 TO WS-TT-X.
           MOVE WS-SUB1 TO WS-TT-HIT.
           IF WS-TT-CODE (WS-TT-X) = 'RG' OR
              WS-TT-CODE (WS-TT-X) = 'FI' OR
              WS-TT-CODE (WS-TT-X) = 'FN' OR
              WS-TT-CODE (WS-TT-X) = 'FO'
               MOVE 'Y' TO WS-TRIG-FOUND-SW.

       P3010-EXIT.
           EXIT.

       P3200-DERIVE-REASON.
      * WHY DID THIS USAGE NOT BILL.  THE SKIP CODE ON THE TRIGGER FILE
      * CARRIES THE ANSWER WHERE THE ACCOUNT WAS LOOKED AT AT ALL.  AN
      * ACCOUNT THAT IS NOT ON THE TRIGGER FILE AT ALL IS REPORTED
      * UNDER THE FIRST REASON.
           MOVE 'P3200-DERIVE-REASON' TO WS-PARA-NAME.
           MOVE 'U1' TO WS-UW-REASON.
           IF WS-TT-HIT = ZERO
               GO TO P3200-SUB.
           SET WS-TT-X TO WS-TT-HIT.
           IF WS-TT-CODE (WS-TT-X) = 'S9'
               MOVE 'U2' TO WS-UW-REASON.
           IF WS-TT-CODE (WS-TT-X) = 'S2'
               MOVE 'U6' TO WS-UW-REASON.
           IF RD-CYCLE-YYDDD > WS-CYCLE-YYDDD
               MOVE 'U4' TO WS-UW-REASON.
           IF RD-RATE = ZERO
               IF RD-AMOUNT = ZERO
                   MOVE 'U5' TO WS-UW-REASON.

       P3200-SUB.
           MOVE 1 TO WS-UW-SUB.
           MOVE 'N' TO WS-UW-FOUND-SW.
           PERFORM P3210-FIND-REASON THRU P3210-EXIT
               VARYING WS-RN-X FROM 1 BY 1
               UNTIL WS-RN-X > 6 OR WS-UW-FOUND.

       P3200-EXIT.
           EXIT.

       P3210-FIND-REASON.
           IF WS-RN-CODE (WS-RN-X) = WS-UW-REASON
               SET WS-SUB2 TO WS-RN-X
               MOVE WS-SUB2 TO WS-UW-SUB
               MOVE 'Y' TO WS-UW-FOUND-SW.

       P3210-EXIT.
           EXIT.

       P3400-ACCUM-UNBILLED.
           MOVE 'P3400-ACCUM-UNBILLED' TO WS-PARA-NAME.
           ADD 1          TO WS-RC-COUNT (WS-UW-SUB).
           ADD RD-AMOUNT  TO WS-RC-AMOUNT (WS-UW-SUB).
           ADD RD-QTY     TO WS-RC-MINUTES (WS-UW-SUB).
           ADD 1          TO WS-RT-UNBILLED.
           ADD RD-AMOUNT  TO WS-RT-AMOUNT.
           ADD RD-QTY     TO WS-RT-MINUTES.
           ADD RD-AMOUNT  TO WS-ACC-AMOUNT.
           ADD RD-QTY     TO WS-ACC-MINUTES.

       P3400-EXIT.
           EXIT.

      *****************************************************************
      * S400-TRIGGER LOAD                                             *
      *****************************************************************
       S400-SUPPORT SECTION.

       P4000-LOAD-TRIGGERS.
           MOVE 'P4000-LOAD-TRIGGERS' TO WS-PARA-NAME.
           MOVE ZERO TO WS-TT-USED.
           PERFORM P4010-READ-TRIG THRU P4010-EXIT
               UNTIL WS-TRG-EOF.
           DISPLAY 'TRIGGER ENTRIES ' WS-TT-USED.

       P4000-EXIT.
           EXIT.

       P4010-READ-TRIG.
           READ TRG-IN-FILE INTO WS-TRIGGER-IN
               AT END
                   MOVE 'Y' TO WS-TRG-EOF-SW
                   GO TO P4010-EXIT.
           IF WS-TT-USED NOT < WS-TT-MAX
               MOVE 7602 TO WS-AB-CODE
               MOVE 'TRIGGER TABLE FULL' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-TT-USED.
           SET WS-TT-X TO WS-TT-USED.
           MOVE WS-TI-BAN  TO WS-TT-BAN (WS-TT-X).
           MOVE WS-TI-CODE TO WS-TT-CODE (WS-TT-X).

       P4010-EXIT.
           EXIT.

      *****************************************************************
      * S500-REPORT                                                   *
      *****************************************************************
       S500-REPORT SECTION.

       P5000-DETAIL-LINE.
           MOVE 'P5000-DETAIL-LINE' TO WS-PARA-NAME.
           IF RD-AMOUNT < WS-PE-MIN-VALUE
               GO TO P5000-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE RD-BAN         TO PC-COL-001-020.
           MOVE RD-RATE-ELEM   TO PC-COL-021-060.
           MOVE WS-UW-REASON   TO PC-COL-061-090.
           MOVE RD-AMOUNT TO WS-ED-MONEY.
           MOVE WS-ED-MONEY    TO PC-COL-091-132.
           PERFORM P5300-WRITE-LINE THRU P5300-EXIT.
           ADD 1 TO WS-RT-LISTED.

       P5000-EXIT.
           EXIT.

       P5100-PRINT-SUMMARY.
           MOVE 'P5100-PRINT-SUMMARY' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '4' TO PC-CC.
           MOVE 'UNBILLED USAGE BY REASON' TO PC-TEXT.
           PERFORM P5300-WRITE-LINE THRU P5300-EXIT.
           PERFORM P5110-REASON-LINE THRU P5110-EXIT
               VARYING WS-RN-X FROM 1 BY 1
               UNTIL WS-RN-X > 6.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '-' TO PC-CC.
           MOVE 'TOTAL UNBILLED' TO PC-COL-001-020.
           MOVE WS-RT-AMOUNT TO WS-ED-MONEY.
           MOVE WS-ED-MONEY  TO PC-COL-091-132.
           PERFORM P5300-WRITE-LINE THRU P5300-EXIT.

       P5100-EXIT.
           EXIT.

       P5110-REASON-LINE.
           SET WS-SUB1 TO WS-RN-X.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-RN-CODE (WS-RN-X)   TO PC-COL-001-020.
           MOVE WS-RN-TEXT (WS-RN-X)   TO PC-COL-021-060.
           MOVE WS-RC-COUNT (WS-SUB1)  TO WS-ED-COUNT.
           MOVE WS-ED-COUNT            TO PC-COL-061-090.
           MOVE WS-RC-AMOUNT (WS-SUB1) TO WS-ED-MONEY.
           MOVE WS-ED-MONEY            TO PC-COL-091-132.
           PERFORM P5300-WRITE-LINE THRU P5300-EXIT.

       P5110-EXIT.
           EXIT.

       P5300-WRITE-LINE.
           WRITE UNB-RECORD FROM CABS-PRINT-LINE.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 7603 TO WS-AB-CODE
               MOVE 'UNBILLED REPORT WRITE FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-WRITE-CNT.

       P5300-EXIT.
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
           MOVE 'CABRPT07  UNBILLED USAGE REPORT'
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
           MOVE 'BAN                 RATE ELEMENT   REASON     VALUE'
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
           MOVE 630                    TO CT-STEP-SEQ.
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
           PERFORM P5100-PRINT-SUMMARY THRU P5100-EXIT.
           DISPLAY 'RATED RECORDS     ' WS-RT-READ.
           DISPLAY 'BILLED RECORDS    ' WS-RT-BILLED.
           DISPLAY 'UNBILLED RECORDS  ' WS-RT-UNBILLED.
           DISPLAY 'DETAIL LINES      ' WS-RT-LISTED.
           DISPLAY 'UNBILLED VALUE    ' WS-RT-AMOUNT.
           DISPLAY 'UNBILLED MINUTES  ' WS-RT-MINUTES.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE RAT-IN-FILE
                 TRG-IN-FILE
                 UNB-OUT-FILE
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

       P5400-CLEAR-REASONS.
           MOVE ZERO TO WS-RC-COUNT (WS-SUB1)
                        WS-RC-AMOUNT (WS-SUB1)
                        WS-RC-MINUTES (WS-SUB1).

       P5400-EXIT.
           EXIT.
