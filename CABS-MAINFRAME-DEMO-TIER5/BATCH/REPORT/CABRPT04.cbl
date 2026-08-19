      *****************************************************************
      * CABRPT04 - PIU AND PLU FACTOR EXCEPTION REPORT                *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               FCTIN   TELCABS.CABS.FACTOR(0)            CABSFCTR*
      *               CRDIN   TELCABS.CABS.FACTOR.CARDS         (LOCAL)*
      *               SYSIN   INSTREAM CONTROL CARD             (LOCAL)*
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               EXCOUT  SYSOUT PRINT - EXCEPTION REPORT   CABSPRNT*
      *               REPORT  SYSOUT PRINT - RUN REGISTER       CABSPRNT*
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN                                      *
      * REVISION HISTORY                                              *
      *   V1.00  1991-07-08  M.J.FERRARO  INITIAL RELEASE - RANGE EDITS ONLY*
      *   V1.05  1994-10-17  D.OKONKWO    FACTOR MOVEMENT EDIT ADDED  *
      *   V1.09  1998-04-06  J.M.CASTILLO FILING CARD COLUMN EDIT ADDED - THE*
      *                      CARRIERS TYPE THESE BY HAND              *
      *   V2.00  2004-09-13  A.BUKOWSKI   RESTATEMENT WINDOW EDIT ADDED*
      *   V2.06  2013-05-07  G.PRZYBYLSKI DISPUTED FACTOR EXCEPTION ADDED*
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABRPT04.
       AUTHOR. TELCABS APPLICATIONS - BILLING CONTROL TEAM.
      *****************************************************************
      * REPORTS EXCEPTIONS ON THE QUARTERLY PIU AND PLU FILINGS AND   *
      * EDITS THE FILING CARDS COLUMN BY COLUMN.                      *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FCT-IN-FILE ASSIGN TO UT-S-FCTIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT CRD-IN-FILE ASSIGN TO UT-S-CRDIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-TABLE.
           SELECT EXC-OUT-FILE ASSIGN TO UT-S-EXCOUT
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
      * FCTIN - THE QUARTERLY FACTOR FILE.                            *
      *****************************************************************
       FD  FCT-IN-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 100 CHARACTERS.
       01  FCT-IN-REC                       PIC X(100).
      *****************************************************************
      * CRDIN - THE FILING CARDS AS RECEIVED.                         *
      *****************************************************************
       FD  CRD-IN-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CRD-RECORD                       PIC X(80).
      *****************************************************************
      * EXCOUT - THE EXCEPTION REPORT.                                *
      *****************************************************************
       FD  EXC-OUT-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       01  EXC-RECORD                       PIC X(133).
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
           05  WS-PGM-NAME             PIC X(08) VALUE 'CABRPT04'.
           05  WS-PGM-VERSION          PIC X(05) VALUE 'V2.06'.
           05  WS-PGM-APPL             PIC X(04) VALUE 'CABS'.
           05  WS-PGM-COMPILE          PIC X(08) VALUE '20130507'.
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

       COPY CABSFCTR.

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
           05  WS-PE-QUARTER           PIC 9(05).
           05  WS-PE-VAR-LIMIT         PIC 9(03)V9(02).
           05  WS-PE-CARD-EDIT-SW      PIC X(01).
           05  WS-PE-DEFAULT-SW        PIC X(01).
           05  WS-PE-FILLER            PIC X(24).
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
           05  WS-FCT-EOF-SW           PIC X(01) VALUE 'N'.
               88  WS-FCT-EOF          VALUE 'Y'.
           05  WS-CRD-EOF-SW           PIC X(01) VALUE 'N'.
               88  WS-CRD-EOF          VALUE 'Y'.
           05  WS-EXCEPT-SW            PIC X(01) VALUE 'N'.
               88  WS-EXCEPTION        VALUE 'Y'.
      *****************************************************************
      * THE EXCEPTION TYPES.  ONE COUNTER EACH.  THE ORDER HERE IS THE*
      * ORDER THE REGULATORY TEAM WORK THEM IN.                       *
      *****************************************************************
       01  WS-EXC-TABLE.
           05  FILLER PIC X(34) VALUE
               'X1PIU OUTSIDE 0 TO 100            '.
           05  FILLER PIC X(34) VALUE
               'X2PLU OUTSIDE 0 TO 100            '.
           05  FILLER PIC X(34) VALUE
               'X3FACTOR MOVED MORE THAN THE LIMIT'.
           05  FILLER PIC X(34) VALUE
               'X4NO PRIOR FACTOR ON FILE         '.
           05  FILLER PIC X(34) VALUE
               'X5DISPUTED FACTOR STILL IN FORCE  '.
           05  FILLER PIC X(34) VALUE
               'X6DEFAULT FACTOR USED FOR A FILER '.
           05  FILLER PIC X(34) VALUE
               'X7RESTATEMENT WINDOW NOT SET      '.
           05  FILLER PIC X(34) VALUE
               'X8FILING CARD FAILED COLUMN EDIT  '.
       01  WS-EXC-TABLE-R REDEFINES WS-EXC-TABLE.
           05  WS-XT-ENTRY OCCURS 8 TIMES INDEXED BY WS-XT-X.
               10  WS-XT-CODE          PIC X(02).
               10  WS-XT-TEXT          PIC X(32).
       01  WS-EXC-COUNTS.
           05  WS-XC-COUNT OCCURS 8 TIMES PIC S9(09) COMP-3.
      *****************************************************************
      * THE QUARTERLY FILING CARD.  EIGHTY COLUMNS.  THE CARRIERS TYPE*
      * THESE AND SEND THEM IN, SO EVERY COLUMN IS EDITED BEFORE THE  *
      * FACTOR IS ACCEPTED.  THE EDIT WALKS THE CARD A COLUMN AT A TIME*
      * BECAUSE THE FIELDS ARE NOT ALWAYS WHERE THEY SHOULD BE.       *
      * EDITING RULES ARE HELD WITH THE BILL FORMAT SPECIFICATION.    *
      *****************************************************************
       01  WS-FILING-CARD              PIC X(80) VALUE SPACES.
       01  WS-FILING-CARD-R REDEFINES WS-FILING-CARD.
           05  WS-FC-COL OCCURS 80 TIMES PIC X(01).
       01  WS-FILING-CARD-F REDEFINES WS-FILING-CARD.
           05  WS-FF-OCN               PIC X(04).
           05  WS-FF-STATE             PIC X(02).
           05  WS-FF-LATA              PIC 9(03).
           05  WS-FF-PIU               PIC 9(03)V9(02).
           05  WS-FF-PLU               PIC 9(03)V9(02).
           05  WS-FF-EFF               PIC 9(05).
           05  WS-FF-SIGNATORY         PIC X(20).
           05  WS-FF-FILLER            PIC X(32).
       01  WS-CARD-SCAN.
           05  WS-CS-SUB               PIC 9(03) VALUE 0.
           05  WS-CS-BLANKS            PIC 9(03) VALUE 0.
           05  WS-CS-DIGITS            PIC 9(03) VALUE 0.
           05  WS-CS-ALPHA             PIC 9(03) VALUE 0.
           05  WS-CS-SPECIAL           PIC 9(03) VALUE 0.
           05  WS-CS-BAD-COL           PIC 9(03) VALUE 0.
           05  WS-CS-OK-SW             PIC X(01) VALUE 'Y'.
               88  WS-CS-OK            VALUE 'Y'.
      *****************************************************************
      * FACTOR COMPARISON WORK.                                       *
      *****************************************************************
       01  WS-FACT-WORK.
           05  WS-FW-PIU-MOVE          PIC S9(05)V9(05) COMP-3 VALUE 0.
           05  WS-FW-PLU-MOVE          PIC S9(05)V9(05) COMP-3 VALUE 0.
           05  WS-FW-ABS               PIC S9(05)V9(05) COMP-3 VALUE 0.
           05  WS-FW-EXC-CODE          PIC X(02) VALUE SPACES.
           05  WS-FW-SUB               PIC S9(03) COMP-3 VALUE 0.
       01  WS-RUN-TOTALS.
           05  WS-RT-FACTORS           PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-CARDS             PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-EXCEPTIONS        PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-CLEAN             PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-BAD-CARDS         PIC S9(09) COMP-3 VALUE 0.
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
           OPEN INPUT  FCT-IN-FILE
                       CRD-IN-FILE
                       PARM-FILE
           OPEN OUTPUT EXC-OUT-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 7311 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-FCTIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 7312 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-CRDIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 7313 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-EXCOUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 7314 TO WS-AB-CODE
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
           PERFORM P5300-CLEAR-EXC THRU P5300-EXIT
               VARYING WS-SUB1 FROM 1 BY 1
               UNTIL WS-SUB1 > 8.
           PERFORM P4000-EDIT-CARDS THRU P4000-EXIT.
           PERFORM P6000-HEADING THRU P6000-EXIT.
           DISPLAY WS-PGM-NAME ' STARTED - RUN ' WS-RUN-ID.
           DISPLAY '  QUARTER       ' WS-PE-QUARTER.
           DISPLAY '  VARIANCE LIMIT' WS-PE-VAR-LIMIT.

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
      * THE FILING QUARTER AND THE VARIANCE LIMIT COME FROM THE
      * REGULATORY CALENDAR THROUGH THE SCHEDULER.  NEITHER HAS A
      * DEFAULT.
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
           IF WS-PE-QUARTER NOT NUMERIC
               MOVE 7321 TO WS-AB-CODE
               MOVE 'FILING QUARTER NOT NUMERIC' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PE-VAR-LIMIT NOT NUMERIC
               MOVE 7322 TO WS-AB-CODE
               MOVE 'VARIANCE LIMIT NOT NUMERIC' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PE-CARD-EDIT-SW NOT = 'N'
               MOVE 'Y' TO WS-PE-CARD-EDIT-SW.

       P1200-EXIT.
           EXIT.

      *****************************************************************
      * S200-MAIN PROCESSING                                          *
      *****************************************************************
       S200-MAIN-PROCESS SECTION.

       P2000-PROCESS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ-FACTOR THRU P2100-EXIT.
           IF WS-FCT-EOF
               MOVE 'Y' TO WS-EOF-SW
               GO TO P2000-EXIT.
           ADD 1 TO WS-READ-CNT.
           ADD 1 TO WS-RT-FACTORS.
           MOVE FC-OCN TO WS-RESTART-KEY.
           MOVE 'N' TO WS-EXCEPT-SW.
           PERFORM P3000-RANGE-EDITS THRU P3000-EXIT.
           PERFORM P3200-MOVEMENT-EDIT THRU P3200-EXIT.
           PERFORM P3400-SOURCE-EDITS THRU P3400-EXIT.
           IF WS-EXCEPTION
               ADD 1 TO WS-RT-EXCEPTIONS
           ELSE
               ADD 1 TO WS-RT-CLEAN.
           ADD 1 TO WS-SUMM-CNT.

       P2000-EXIT.
           EXIT.

       P2100-READ-FACTOR.
           MOVE 'P2100-READ-FACTOR' TO WS-PARA-NAME.
           READ FCT-IN-FILE INTO CABS-FACTOR-RECORD
               AT END
                   MOVE 'Y' TO WS-FCT-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00' AND WS-FS-INPUT NOT = '10'
               MOVE 7301 TO WS-AB-CODE
               MOVE 'FACTOR FILE READ ERROR' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.

       P2100-EXIT.
           EXIT.

      *****************************************************************
      * S300-FACTOR EDITS                                             *
      *****************************************************************
       S300-EDITS SECTION.

       P3000-RANGE-EDITS.
           MOVE 'P3000-RANGE-EDITS' TO WS-PARA-NAME.
           IF FC-PIU < ZERO OR FC-PIU > 100
               MOVE 'X1' TO WS-FW-EXC-CODE
               PERFORM P3600-RAISE THRU P3600-EXIT.
           IF FC-PLU < ZERO OR FC-PLU > 100
               MOVE 'X2' TO WS-FW-EXC-CODE
               PERFORM P3600-RAISE THRU P3600-EXIT.

       P3000-EXIT.
           EXIT.

       P3200-MOVEMENT-EDIT.
      * HOW FAR THE FACTOR MOVED SINCE THE PRIOR FILING.  A LARGE MOVE
      * IS NOT WRONG BUT IT IS WORTH A HUMAN LOOKING AT IT - THE
      * RESTATEMENT IT DRIVES CAN BE WORTH SIX FIGURES.
           MOVE 'P3200-MOVEMENT-EDIT' TO WS-PARA-NAME.
           IF FC-PRIOR-PIU = ZERO
               MOVE 'X4' TO WS-FW-EXC-CODE
               PERFORM P3600-RAISE THRU P3600-EXIT
               GO TO P3200-EXIT.
           COMPUTE WS-FW-PIU-MOVE = FC-PIU - FC-PRIOR-PIU.
           MOVE WS-FW-PIU-MOVE TO WS-FW-ABS.
           IF WS-FW-ABS < ZERO
               COMPUTE WS-FW-ABS = WS-FW-ABS * -1.
           IF WS-FW-ABS > WS-PE-VAR-LIMIT
               MOVE 'X3' TO WS-FW-EXC-CODE
               PERFORM P3600-RAISE THRU P3600-EXIT.
           COMPUTE WS-FW-PLU-MOVE = FC-PLU - FC-PRIOR-PLU.

       P3200-EXIT.
           EXIT.

       P3400-SOURCE-EDITS.
           MOVE 'P3400-SOURCE-EDITS' TO WS-PARA-NAME.
           IF FC-DISPUTED
               MOVE 'X5' TO WS-FW-EXC-CODE
               PERFORM P3600-RAISE THRU P3600-EXIT.
           IF FC-FROM-DEFAULT
               IF WS-PE-DEFAULT-SW = 'Y'
                   MOVE 'X6' TO WS-FW-EXC-CODE
                   PERFORM P3600-RAISE THRU P3600-EXIT.
           IF FC-RESTATE-REQD
               IF FC-RESTATE-FROM-YYDDD = ZERO
                   MOVE 'X7' TO WS-FW-EXC-CODE
                   PERFORM P3600-RAISE THRU P3600-EXIT.

       P3400-EXIT.
           EXIT.

       P3600-RAISE.
           MOVE 'Y' TO WS-EXCEPT-SW.
           MOVE 8 TO WS-FW-SUB.
           PERFORM P3610-FIND-EXC THRU P3610-EXIT
               VARYING WS-XT-X FROM 1 BY 1
               UNTIL WS-XT-X > 8.
           ADD 1 TO WS-XC-COUNT (WS-FW-SUB).
           PERFORM P5000-EXCEPTION-LINE THRU P5000-EXIT.

       P3600-EXIT.
           EXIT.

       P3610-FIND-EXC.
           IF WS-XT-CODE (WS-XT-X) = WS-FW-EXC-CODE
               SET WS-SUB1 TO WS-XT-X
               MOVE WS-SUB1 TO WS-FW-SUB.

       P3610-EXIT.
           EXIT.

      *****************************************************************
      * S400-FILING CARD EDIT                                         *
      *****************************************************************
       S400-CARDS SECTION.

       P4000-EDIT-CARDS.
      * EDIT THE QUARTERLY FILING CARDS.  EVERY COLUMN IS CLASSIFIED
      * AND THE FIELD POSITIONS ARE CHECKED AGAINST WHAT THE FILING
      * INSTRUCTION SAYS.  A CARD THAT FAILS IS REPORTED AND THE
      * FACTOR IT CARRIED IS NOT APPLIED.
           MOVE 'P4000-EDIT-CARDS' TO WS-PARA-NAME.
           IF WS-PE-CARD-EDIT-SW NOT = 'Y'
               GO TO P4000-EXIT.
           PERFORM P4100-READ-CARD THRU P4100-EXIT
               UNTIL WS-CRD-EOF.

       P4000-EXIT.
           EXIT.

       P4100-READ-CARD.
           READ CRD-IN-FILE INTO WS-FILING-CARD
               AT END
                   MOVE 'Y' TO WS-CRD-EOF-SW
                   GO TO P4100-EXIT.
           IF WS-FILING-CARD = SPACES
               GO TO P4100-EXIT.
           ADD 1 TO WS-RT-CARDS.
           PERFORM P4200-SCAN-CARD THRU P4200-EXIT.
           IF NOT WS-CS-OK
               ADD 1 TO WS-RT-BAD-CARDS
               MOVE 'X8' TO WS-FW-EXC-CODE
               PERFORM P3600-RAISE THRU P3600-EXIT.

       P4100-EXIT.
           EXIT.

       P4200-SCAN-CARD.
      * WALK THE EIGHTY COLUMNS.  COLUMNS 1 TO 4 MUST BE THE OCN AND
      * MAY BE ALPHANUMERIC; 5 AND 6 MUST BE ALPHABETIC; 7 THROUGH 27
      * MUST BE NUMERIC; 28 ONWARDS IS THE SIGNATORY AND IS NOT
      * EDITED.  A SPECIAL CHARACTER ANYWHERE IN THE NUMERIC RANGE
      * FAILS THE CARD.
           MOVE 'P4200-SCAN-CARD' TO WS-PARA-NAME.
           MOVE 'Y' TO WS-CS-OK-SW.
           MOVE ZERO TO WS-CS-BLANKS WS-CS-DIGITS WS-CS-ALPHA
                        WS-CS-SPECIAL WS-CS-BAD-COL.
           INSPECT WS-FILING-CARD
               TALLYING WS-CS-BLANKS FOR ALL SPACE.
           INSPECT WS-FILING-CARD
               REPLACING ALL LOW-VALUE BY SPACE.
           PERFORM P4210-ONE-COLUMN THRU P4210-EXIT
               VARYING WS-CS-SUB FROM 7 BY 1
               UNTIL WS-CS-SUB > 27.
           IF WS-CS-SPECIAL > ZERO
               MOVE 'N' TO WS-CS-OK-SW.
           IF WS-FF-EFF NOT NUMERIC
               MOVE 'N' TO WS-CS-OK-SW.
           IF WS-FF-OCN = SPACES
               MOVE 'N' TO WS-CS-OK-SW.

       P4200-EXIT.
           EXIT.

       P4210-ONE-COLUMN.
           IF WS-FC-COL (WS-CS-SUB) NOT < '0'
               IF WS-FC-COL (WS-CS-SUB) NOT > '9'
                   ADD 1 TO WS-CS-DIGITS
                   GO TO P4210-EXIT.
           IF WS-FC-COL (WS-CS-SUB) = SPACE
               ADD 1 TO WS-CS-SPECIAL
               MOVE WS-CS-SUB TO WS-CS-BAD-COL
               GO TO P4210-EXIT.
           ADD 1 TO WS-CS-SPECIAL.
           MOVE WS-CS-SUB TO WS-CS-BAD-COL.

       P4210-EXIT.
           EXIT.

      *****************************************************************
      * S500-REPORT                                                   *
      *****************************************************************
       S500-REPORT SECTION.

       P5000-EXCEPTION-LINE.
           MOVE 'P5000-EXCEPTION-LINE' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE FC-OCN         TO PC-COL-001-020.
           MOVE FC-STATE-CD    TO PC-COL-021-060.
           MOVE WS-FW-EXC-CODE TO PC-COL-061-090.
           MOVE FC-PIU TO WS-ED-PCT.
           MOVE WS-ED-PCT      TO PC-COL-091-132.
           PERFORM P5200-WRITE-LINE THRU P5200-EXIT.

       P5000-EXIT.
           EXIT.

       P5100-PRINT-SUMMARY.
           MOVE 'P5100-PRINT-SUMMARY' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '4' TO PC-CC.
           MOVE 'FACTOR EXCEPTION SUMMARY' TO PC-TEXT.
           PERFORM P5200-WRITE-LINE THRU P5200-EXIT.
           PERFORM P5110-SUMMARY-LINE THRU P5110-EXIT
               VARYING WS-XT-X FROM 1 BY 1
               UNTIL WS-XT-X > 8.

       P5100-EXIT.
           EXIT.

       P5110-SUMMARY-LINE.
           SET WS-SUB1 TO WS-XT-X.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-XT-CODE (WS-XT-X)  TO PC-COL-001-020.
           MOVE WS-XT-TEXT (WS-XT-X)  TO PC-COL-021-060.
           MOVE WS-XC-COUNT (WS-SUB1) TO WS-ED-COUNT.
           MOVE WS-ED-COUNT           TO PC-COL-061-090.
           PERFORM P5200-WRITE-LINE THRU P5200-EXIT.

       P5110-EXIT.
           EXIT.

       P5200-WRITE-LINE.
           WRITE EXC-RECORD FROM CABS-PRINT-LINE.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 7302 TO WS-AB-CODE
               MOVE 'EXCEPTION REPORT WRITE FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-WRITE-CNT.

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
           MOVE 'CABRPT04  FACTOR EXCEPTION REPORT'
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
           MOVE 'OCN     STATE          EXCEPTION      PIU'
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
           MOVE 615                    TO CT-STEP-SEQ.
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
           DISPLAY 'FACTORS EXAMINED  ' WS-RT-FACTORS.
           DISPLAY 'FILING CARDS      ' WS-RT-CARDS.
           DISPLAY 'EXCEPTIONS RAISED ' WS-RT-EXCEPTIONS.
           DISPLAY 'CLEAN FACTORS     ' WS-RT-CLEAN.
           DISPLAY 'CARDS FAILING EDIT' WS-RT-BAD-CARDS.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE FCT-IN-FILE
                 CRD-IN-FILE
                 EXC-OUT-FILE
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

       P5300-CLEAR-EXC.
           MOVE ZERO TO WS-XC-COUNT (WS-SUB1).

       P5300-EXIT.
           EXIT.
