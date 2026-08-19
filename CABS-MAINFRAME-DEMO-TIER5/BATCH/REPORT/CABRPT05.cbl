      *****************************************************************
      * CABRPT05 - SETTLEMENT POSITION REPORT                         *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               SETLIN  TELCABS.SETL.SETTLE.ALL(0)        CABSSETL*
      *               CARRMST TELCABS.CABS.CARRIER              CABSCARR*
      *               SYSIN   INSTREAM CONTROL CARD             (LOCAL)*
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               POSOUT  SYSOUT PRINT - POSITION REPORT    CABSPRNT*
      *               REPORT  SYSOUT PRINT - RUN REGISTER       CABSPRNT*
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN                                      *
      * REVISION HISTORY                                              *
      *   V1.00  1993-06-14  M.J.FERRARO  INITIAL RELEASE - MEET POINT ONLY*
      *   V1.05  1996-09-02  D.OKONKWO    ALL THREE SETTLEMENT KINDS BROUGHT*
      *                      INTO ONE POSITION                        *
      *   V1.09  1999-12-07  J.M.CASTILLO INTEREST PROJECTION ADDED FOLLOWING*
      *                      THE TARIFF REVIEW                        *
      *   V2.00  2003-04-23  A.BUKOWSKI   DISPUTED POSITIONS FLAGGED SEPARATELY*
      *   V2.04  2010-11-22  G.PRZYBYLSKI MINIMUM POSITION SUPPRESSION ADDED*
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABRPT05.
       AUTHOR. TELCABS APPLICATIONS - BILLING CONTROL TEAM.
      *****************************************************************
      * REPORTS THE INTER CARRIER SETTLEMENT POSITION BY COUNTERPARTY *
      * ACROSS ALL THREE SETTLEMENT KINDS.  READS A DATASET OWNED BY  *
      * THE SETTLEMENT APPLICATION.                                   *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SETL-IN-FILE ASSIGN TO UT-S-SETLIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT CARRIER-MASTER ASSIGN TO DA-I-CARRMST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CR-KEY
               FILE STATUS IS WS-FS-TABLE.
           SELECT POS-OUT-FILE ASSIGN TO UT-S-POSOUT
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
      * SETLIN - THE SETTLEMENT MASTER, OWNED BY THE                  *
      * SETTLEMENT APPLICATION AND READ DIRECTLY HERE.                *
      * ACCESS IS RECORDED IN THE DATASET REGISTER.                   *
      *****************************************************************
       FD  SETL-IN-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  SETL-IN-REC                      PIC X(180).
      *****************************************************************
      * CARRMST - CARRIER MASTER, READ RANDOMLY BY OCN.               *
      *****************************************************************
       FD  CARRIER-MASTER
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 200 CHARACTERS.
       COPY CABSCARR.
      *****************************************************************
      * POSOUT - THE POSITION REPORT.                                 *
      *****************************************************************
       FD  POS-OUT-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       01  POS-RECORD                       PIC X(133).
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
           05  WS-PGM-NAME             PIC X(08) VALUE 'CABRPT05'.
           05  WS-PGM-VERSION          PIC X(05) VALUE 'V2.04'.
           05  WS-PGM-APPL             PIC X(04) VALUE 'CABS'.
           05  WS-PGM-COMPILE          PIC X(08) VALUE '20101122'.
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

       COPY CABSSETL.

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
           05  WS-PE-SETL-PERIOD       PIC 9(06).
           05  WS-PE-KIND-SEL          PIC X(01).
           05  WS-PE-MIN-POSITION      PIC 9(09)V9(02).
           05  WS-PE-DISPUTE-SW        PIC X(01).
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
           05  WS-SET-EOF-SW           PIC X(01) VALUE 'N'.
               88  WS-SET-EOF          VALUE 'Y'.
           05  WS-PARTY-FOUND-SW       PIC X(01) VALUE 'N'.
               88  WS-PARTY-FOUND      VALUE 'Y'.
      *****************************************************************
      * THE COUNTERPARTY POSITION TABLE.  RECEIVABLE AND PAYABLE ARE  *
      * HELD SEPARATELY BECAUSE THE LEDGER NEEDS THE GROSS FIGURES EVEN*
      * THOUGH ONLY THE NET IS SETTLED IN CASH.                       *
      *****************************************************************
       01  WS-POS-TABLE.
           05  WS-PS-ENTRY OCCURS 400 TIMES INDEXED BY WS-PS-X.
               10  WS-PS-OCN           PIC X(04).
               10  WS-PS-NAME          PIC X(40).
               10  WS-PS-MPB           PIC S9(15)V9(05) COMP-3.
               10  WS-PS-RECIP         PIC S9(15)V9(05) COMP-3.
               10  WS-PS-CMDS          PIC S9(15)V9(05) COMP-3.
               10  WS-PS-RECV          PIC S9(15)V9(05) COMP-3.
               10  WS-PS-PAY           PIC S9(15)V9(05) COMP-3.
               10  WS-PS-NET-RAW       PIC S9(15)V9(05) COMP-3.
               10  WS-PS-NET-SHOWN     PIC S9(15)V9(02) COMP-3.
               10  WS-PS-DISPUTED      PIC S9(09) COMP-3.
               10  WS-PS-COUNT         PIC S9(09) COMP-3.
               10  WS-PS-TERMS         PIC 9(03).
       01  WS-POS-CTL.
           05  WS-PS-USED              PIC S9(05) COMP-3 VALUE 0.
           05  WS-PS-MAX               PIC S9(05) COMP-3 VALUE 400.
           05  WS-PS-HIT               PIC S9(05) COMP-3 VALUE 0.
       01  WS-KIND-NAMES.
           05  FILLER PIC X(24) VALUE 'MMEET POINT BILLING     '.
           05  FILLER PIC X(24) VALUE 'RRECIPROCAL COMPENSATION'.
           05  FILLER PIC X(24) VALUE 'CCMDS RAO EXCHANGE      '.
       01  WS-KIND-NAMES-R REDEFINES WS-KIND-NAMES.
           05  WS-KN-ENTRY OCCURS 3 TIMES INDEXED BY WS-KN-X.
               10  WS-KN-CODE          PIC X(01).
               10  WS-KN-NAME          PIC X(23).
      *****************************************************************
      * INTEREST PROJECTION WORK.  THE 1999 TARIFF REVIEW ASKED FOR AN*
      * INTEREST FIGURE ON OVERDUE SETTLEMENT POSITIONS.              *
      *****************************************************************
       01  WS-INT-WORK.
           05  WS-IW-RATE              PIC S9(03)V9(05) COMP-3
                                                    VALUE 000.01500.
           05  WS-IW-DAYS              PIC S9(05) COMP-3 VALUE 0.
           05  WS-IW-BASE              PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-IW-AMOUNT            PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-IW-TOTAL             PIC S9(15)V9(02) COMP-3 VALUE 0.
       01  WS-RUN-TOTALS.
           05  WS-RT-RECORDS           PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-PARTIES           PIC S9(05) COMP-3 VALUE 0.
           05  WS-RT-DISPUTED          PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-SUPPRESSED        PIC S9(05) COMP-3 VALUE 0.
           05  WS-RT-RECV              PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-RT-PAY               PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-RT-NET               PIC S9(15)V9(02) COMP-3 VALUE 0.
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
           OPEN INPUT  SETL-IN-FILE
                       CARRIER-MASTER
                       PARM-FILE
           OPEN OUTPUT POS-OUT-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 7411 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-SETLIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 7412 TO WS-AB-CODE
               MOVE 'OPEN FAILED DA-I-CARRMST' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 7413 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-POSOUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 7414 TO WS-AB-CODE
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
           DISPLAY '  SETTLE PERIOD ' WS-PE-SETL-PERIOD.
           DISPLAY '  KIND SELECT   ' WS-PE-KIND-SEL.

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
      * THE SETTLEMENT PERIOD RUNS A MONTH BEHIND THE BILL PERIOD.
      * THE SCHEDULER SUPPLIES IT AND IT HAS NO DEFAULT.
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
           IF WS-PE-SETL-PERIOD NOT NUMERIC
               MOVE 7421 TO WS-AB-CODE
               MOVE 'SETTLEMENT PERIOD NOT NUMERIC' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PE-MIN-POSITION NOT NUMERIC
               MOVE ZERO TO WS-PE-MIN-POSITION.
           IF WS-PE-DISPUTE-SW NOT = 'N'
               MOVE 'Y' TO WS-PE-DISPUTE-SW.

       P1200-EXIT.
           EXIT.

      *****************************************************************
      * S200-MAIN PROCESSING                                          *
      *****************************************************************
       S200-MAIN-PROCESS SECTION.

       P2000-PROCESS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ-SETTLE THRU P2100-EXIT.
           IF WS-SET-EOF
               MOVE 'Y' TO WS-EOF-SW
               GO TO P2000-EXIT.
           ADD 1 TO WS-READ-CNT.
           MOVE ST-COUNTERPARTY-OCN TO WS-RESTART-KEY.
           IF ST-SETTLE-PERIOD NOT = WS-PE-SETL-PERIOD
               ADD 1 TO WS-CFWD-CNT
               GO TO P2000-EXIT.
           IF WS-PE-KIND-SEL NOT = SPACE
               IF ST-SETTLE-TYPE NOT = WS-PE-KIND-SEL
                   ADD 1 TO WS-CFWD-CNT
                   GO TO P2000-EXIT.
           ADD 1 TO WS-RT-RECORDS.
           PERFORM P3000-FIND-PARTY THRU P3000-EXIT.
           PERFORM P3200-ACCUM-POSITION THRU P3200-EXIT.
           ADD 1 TO WS-SUMM-CNT.

       P2000-EXIT.
           EXIT.

       P2100-READ-SETTLE.
           MOVE 'P2100-READ-SETTLE' TO WS-PARA-NAME.
           READ SETL-IN-FILE INTO CABS-SETTLEMENT-RECORD
               AT END
                   MOVE 'Y' TO WS-SET-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00' AND WS-FS-INPUT NOT = '10'
               MOVE 7401 TO WS-AB-CODE
               MOVE 'SETTLEMENT FILE READ ERROR' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.

       P2100-EXIT.
           EXIT.

      *****************************************************************
      * S300-POSITION ACCUMULATION                                    *
      *****************************************************************
       S300-POSITION SECTION.

       P3000-FIND-PARTY.
           MOVE 'P3000-FIND-PARTY' TO WS-PARA-NAME.
           MOVE 'N' TO WS-PARTY-FOUND-SW.
           MOVE ZERO TO WS-PS-HIT.
           PERFORM P3010-MATCH-PARTY THRU P3010-EXIT
               VARYING WS-PS-X FROM 1 BY 1
               UNTIL WS-PS-X > WS-PS-USED OR WS-PARTY-FOUND.
           IF WS-PARTY-FOUND
               GO TO P3000-EXIT.
           IF WS-PS-USED NOT < WS-PS-MAX
               MOVE 7402 TO WS-AB-CODE
               MOVE 'POSITION TABLE FULL' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-PS-USED.
           MOVE WS-PS-USED TO WS-PS-HIT.
           SET WS-PS-X TO WS-PS-USED.
           MOVE ST-COUNTERPARTY-OCN TO WS-PS-OCN (WS-PS-X).
           MOVE ZERO TO WS-PS-MPB (WS-PS-X)
                        WS-PS-RECIP (WS-PS-X)
                        WS-PS-CMDS (WS-PS-X)
                        WS-PS-RECV (WS-PS-X)
                        WS-PS-PAY (WS-PS-X)
                        WS-PS-NET-RAW (WS-PS-X)
                        WS-PS-NET-SHOWN (WS-PS-X)
                        WS-PS-DISPUTED (WS-PS-X)
                        WS-PS-COUNT (WS-PS-X).
           PERFORM P3100-CARRIER-NAME THRU P3100-EXIT.
           ADD 1 TO WS-RT-PARTIES.

       P3000-EXIT.
           EXIT.

       P3010-MATCH-PARTY.
           IF WS-PS-OCN (WS-PS-X) = ST-COUNTERPARTY-OCN
               SET WS-SUB1 TO WS-PS-X
               MOVE WS-SUB1 TO WS-PS-HIT
               MOVE 'Y' TO WS-PARTY-FOUND-SW.

       P3010-EXIT.
           EXIT.

       P3100-CARRIER-NAME.
           MOVE 'P3100-CARRIER-NAME' TO WS-PARA-NAME.
           MOVE ST-COUNTERPARTY-OCN TO CR-OCN.
           READ CARRIER-MASTER
               INVALID KEY
                   MOVE 'UNKNOWN COUNTERPARTY'
                                       TO WS-PS-NAME (WS-PS-X)
                   MOVE ZERO TO WS-PS-TERMS (WS-PS-X)
                   GO TO P3100-EXIT.
           MOVE CR-NAME       TO WS-PS-NAME (WS-PS-X).
           MOVE CR-TERMS-DAYS TO WS-PS-TERMS (WS-PS-X).

       P3100-EXIT.
           EXIT.

       P3200-ACCUM-POSITION.
      * ACCUMULATE THE POSITION AT FIVE DECIMAL PLACES.  THE PRINTED
      * AND LEDGER FIGURE IS TWO PLACES AND IS PRODUCED BY A MOVE, NOT
      * BY A ROUNDED COMPUTE - THE POSITION SHOWN HERE IS THEREFORE
      * NEVER MORE THAN THE POSITION HELD.
      * PRECISION AGREED WITH REVENUE ACCOUNTING, CR-2907.
           MOVE 'P3200-ACCUM-POSITION' TO WS-PARA-NAME.
           SET WS-PS-X TO WS-PS-HIT.
           IF ST-MEET-POINT
               ADD ST-NET-DUE TO WS-PS-MPB (WS-PS-X).
           IF ST-RECIP-COMP
               ADD ST-NET-DUE TO WS-PS-RECIP (WS-PS-X).
           IF ST-CMDS-RAO
               ADD ST-NET-DUE TO WS-PS-CMDS (WS-PS-X).
           IF ST-RECEIVABLE
               ADD ST-NET-DUE TO WS-PS-RECV (WS-PS-X)
           ELSE
               ADD ST-NET-DUE TO WS-PS-PAY (WS-PS-X).
           COMPUTE WS-PS-NET-RAW (WS-PS-X) =
                   WS-PS-RECV (WS-PS-X) - WS-PS-PAY (WS-PS-X).
           MOVE WS-PS-NET-RAW (WS-PS-X)
                                       TO WS-PS-NET-SHOWN (WS-PS-X).
           ADD 1 TO WS-PS-COUNT (WS-PS-X).
           IF ST-DISPUTE-SW = 'Y'
               ADD 1 TO WS-PS-DISPUTED (WS-PS-X)
               ADD 1 TO WS-RT-DISPUTED.
           ADD ST-NET-DUE TO WS-ACC-AMOUNT.

       P3200-EXIT.
           EXIT.

      *****************************************************************
      * S500-REPORT                                                   *
      *****************************************************************
       S500-REPORT SECTION.

       P5000-PRINT-POSITIONS.
           MOVE 'P5000-PRINT-POSITIONS' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '4' TO PC-CC.
           MOVE 'SETTLEMENT POSITION BY COUNTERPARTY' TO PC-TEXT.
           PERFORM P5300-WRITE-LINE THRU P5300-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '0' TO PC-CC.
           MOVE 'OCN   COUNTERPARTY' TO PC-COL-001-020.
           MOVE 'RECEIVABLE     PAYABLE' TO PC-COL-021-060.
           MOVE 'DISPUTED' TO PC-COL-061-090.
           MOVE 'NET POSITION' TO PC-COL-091-132.
           PERFORM P5300-WRITE-LINE THRU P5300-EXIT.
           PERFORM P5100-ONE-POSITION THRU P5100-EXIT
               VARYING WS-PS-X FROM 1 BY 1
               UNTIL WS-PS-X > WS-PS-USED.
           PERFORM P5200-RUN-TOTAL THRU P5200-EXIT.

       P5000-EXIT.
           EXIT.

       P5100-ONE-POSITION.
           SET WS-SUB1 TO WS-PS-X.
           IF WS-PS-NET-SHOWN (WS-PS-X) < WS-PE-MIN-POSITION
               IF WS-PS-NET-SHOWN (WS-PS-X) > ZERO
                   ADD 1 TO WS-RT-SUPPRESSED
                   GO TO P5100-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           IF WS-PS-DISPUTED (WS-PS-X) > ZERO
               MOVE '0' TO PC-CC.
           MOVE WS-PS-OCN (WS-PS-X)  TO PC-COL-001-020.
           MOVE WS-PS-NAME (WS-PS-X) TO PC-COL-021-060.
           MOVE WS-PS-RECV (WS-PS-X) TO WS-ED-MONEY.
           MOVE WS-ED-MONEY          TO PC-COL-061-090.
           MOVE WS-PS-NET-SHOWN (WS-PS-X) TO WS-ED-MONEY.
           MOVE WS-ED-MONEY          TO PC-COL-091-132.
           PERFORM P5300-WRITE-LINE THRU P5300-EXIT.
           ADD WS-PS-RECV (WS-PS-X)      TO WS-RT-RECV.
           ADD WS-PS-PAY (WS-PS-X)       TO WS-RT-PAY.
           ADD WS-PS-NET-SHOWN (WS-PS-X) TO WS-RT-NET.

       P5100-EXIT.
           EXIT.

       P5200-RUN-TOTAL.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '-' TO PC-CC.
           MOVE 'RUN POSITION' TO PC-COL-001-020.
           MOVE WS-RT-RECV TO WS-ED-MONEY.
           MOVE WS-ED-MONEY TO PC-COL-061-090.
           MOVE WS-RT-NET TO WS-ED-MONEY.
           MOVE WS-ED-MONEY TO PC-COL-091-132.
           PERFORM P5300-WRITE-LINE THRU P5300-EXIT.

       P5200-EXIT.
           EXIT.

       P5300-WRITE-LINE.
           WRITE POS-RECORD FROM CABS-PRINT-LINE.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 7403 TO WS-AB-CODE
               MOVE 'POSITION REPORT WRITE FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-WRITE-CNT.

       P5300-EXIT.
           EXIT.

      *****************************************************************
      * S610-INTEREST PROJECTION                                      *
      * THE PROJECTED INTEREST ON A SETTLEMENT POSITION THAT IS PAST  *
      * ITS DUE DATE, AT THE RATE HELD IN WORKING STORAGE.            *
      *****************************************************************
       S610-INTEREST SECTION.

       P6100-INTEREST-PROJECTION.
      * PROJECT THE INTEREST ON EVERY OVERDUE POSITION.  THE BASE IS
      * THE NET POSITION AS SHOWN, THE PERIOD IS THE NUMBER OF DAYS
      * SINCE THE DUE DATE AND THE RATE IS THE ONE THE 1999 TARIFF
      * REVIEW SETTLED ON.  THE FIGURE IS PRINTED AS A MEMORANDUM AND
      * IS NOT POSTED ANYWHERE.
           MOVE 'P6100-INTEREST-PROJECTION' TO WS-PARA-NAME.
           MOVE ZERO TO WS-IW-TOTAL.
           PERFORM P6110-ONE-INTEREST THRU P6110-EXIT
               VARYING WS-PS-X FROM 1 BY 1
               UNTIL WS-PS-X > WS-PS-USED.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '0' TO PC-CC.
           MOVE 'PROJECTED INTEREST' TO PC-COL-001-020.
           MOVE WS-IW-TOTAL TO WS-ED-MONEY.
           MOVE WS-ED-MONEY TO PC-COL-091-132.
           PERFORM P5300-WRITE-LINE THRU P5300-EXIT.

       P6100-EXIT.
           EXIT.

       P6110-ONE-INTEREST.
           SET WS-SUB1 TO WS-PS-X.
           IF WS-PS-NET-SHOWN (WS-PS-X) NOT > ZERO
               GO TO P6110-EXIT.
           MOVE WS-PS-NET-SHOWN (WS-PS-X) TO WS-IW-BASE.
           MOVE WS-PS-TERMS (WS-PS-X) TO WS-IW-DAYS.
           IF WS-IW-DAYS = ZERO
               MOVE 30 TO WS-IW-DAYS.
           COMPUTE WS-IW-AMOUNT ROUNDED =
                   (WS-IW-BASE * WS-IW-RATE * WS-IW-DAYS) / 36500.
           ADD WS-IW-AMOUNT TO WS-IW-TOTAL.

       P6110-EXIT.
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
           MOVE 'CABRPT05  SETTLEMENT POSITION REPORT'
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
           MOVE 'OCN     COUNTERPARTY        RECEIVABLE     NET'
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
           MOVE 620                    TO CT-STEP-SEQ.
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
           PERFORM P5000-PRINT-POSITIONS THRU P5000-EXIT.
           DISPLAY 'SETTLEMENT RECORDS' WS-RT-RECORDS.
           DISPLAY 'COUNTERPARTIES    ' WS-RT-PARTIES.
           DISPLAY 'DISPUTED ITEMS    ' WS-RT-DISPUTED.
           DISPLAY 'SUPPRESSED        ' WS-RT-SUPPRESSED.
           DISPLAY 'RECEIVABLE TOTAL  ' WS-RT-RECV.
           DISPLAY 'PAYABLE TOTAL     ' WS-RT-PAY.
           DISPLAY 'NET POSITION      ' WS-RT-NET.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE SETL-IN-FILE
                 CARRIER-MASTER
                 POS-OUT-FILE
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

