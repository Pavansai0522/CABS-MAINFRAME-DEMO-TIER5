      *****************************************************************
      * CABBIL11 - BILL LEVEL BALANCING - DETAIL TO HEADER PROOF      *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               BDTLIN  TELCABS.CABS.BILLDTL.SEQ(0)       CABSBILL*
      *               BHDRIN  TELCABS.CABS.BILLHDR.AUD(0)       CABSBHDR*
      *               SYSIN   INSTREAM CONTROL CARD             (LOCAL)*
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               PROOFOUTTELCABS.CABS.BILLPROOF(+1)        (LOCAL)*
      *               REPORT  SYSOUT PRINT                      CABSPRNT*
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-SUMMARISED + CT-CARRIED-FWD        *
      *               AND, PER ACCOUNT, BH-HASH-AMOUNT = SUM OF BD-TOT-ROUNDED*
      * RESTART     : FULL RERUN - THE PROOF FILE IS REBUILT EACH CYCLE*
      * REVISION HISTORY                                              *
      *   V1.00  1991-05-08  M.J.FERRARO  INITIAL RELEASE - LINE COUNT PROOF*
      *                      ONLY, NO MONEY COMPARISON                *
      *   V1.03  1994-11-22  L.HARGREAVES MONEY COMPARISON ADDED AGAINST THE*
      *                      HEADER CONTROL FIGURE                    *
      *   V1.06  1997-08-14  J.M.CASTILLO ELEMENT LEVEL PROOF ADDED AS A*
      *                      SECOND INDEPENDENT CHECK OF A LINE       *
      *   V2.00  2001-02-06  P.NAIR       TOLERANCE INTRODUCED AT FIVE CENTS*
      *                      PER THE CONTROL STANDARD - THE ZERO      *
      *                      TOLERANCE TEST WAS FAILING LARGE         *
      *                      ACCOUNTS EVERY CYCLE                     *
      *   V2.02  2006-04-11  A.BUKOWSKI   PROOF FILE WRITTEN FOR THE DAILY*
      *                      BALANCING REPORT TO CONSUME              *
      *   V2.04  2012-07-11  G.PRZYBYLSKI WORST CASE ACCOUNT NOW REPORTED AT*
      *                      END OF RUN                               *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABBIL11.
       AUTHOR. TELCABS APPLICATIONS - BILLING TEAM.
      *****************************************************************
      * PROVES THAT THE BILL DETAIL ADDS UP TO THE INVOICE HEADER FOR *
      * EVERY ACCOUNT.  AN INVOICE IS NOT NUMBERED OR SENT FOR PRINT  *
      * UNTIL THIS STEP HAS PASSED IT.                                *
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
           SELECT PROOF-OUT-FILE ASSIGN TO UT-S-PROOFOUT
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
      * BHDRIN - THE AUDITED INVOICE HEADER.                          *
      *****************************************************************
       FD  BHDR-IN-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 400 CHARACTERS.
       01  BHDR-IN-REC                      PIC X(400).
      *****************************************************************
      * PROOFOUT - ONE PROOF RECORD PER ACCOUNT.                      *
      *****************************************************************
       FD  PROOF-OUT-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 120 CHARACTERS.
       01  PROOF-RECORD                     PIC X(120).
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
           05  WS-PGM-NAME             PIC X(08) VALUE 'CABBIL11'.
           05  WS-PGM-VERSION          PIC X(05) VALUE 'V2.04'.
           05  WS-PGM-APPL             PIC X(04) VALUE 'CABS'.
           05  WS-PGM-COMPILE          PIC X(08) VALUE '20120711'.
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
           05  WS-PE-TOLERANCE         PIC 9(03)V9(02).
           05  WS-PE-ABEND-SW          PIC X(01).
           05  WS-PE-ELEM-CHECK-SW     PIC X(01).
           05  WS-PE-LIST-SW           PIC X(01).
           05  WS-PE-FILLER            PIC X(29).
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
           05  WS-BAL-SW               PIC X(01) VALUE 'Y'.
               88  WS-IN-BAL           VALUE 'Y'.
           05  WS-HDR-MATCH-SW         PIC X(01) VALUE 'N'.
               88  WS-HDR-MATCH        VALUE 'Y'.
      *****************************************************************
      * THE DETAIL SIDE OF THE PROOF.  THE ROUNDED LINE TOTAL IS THE  *
      * FIGURE THAT APPEARS ON THE PRINTED BILL, SO IT IS THE FIGURE  *
      * THIS PROGRAM ADDS UP.  THE ELEMENT AMOUNTS ARE ADDED UP AS WELL*
      * AS A SECOND, INDEPENDENT PROOF OF THE SAME LINE.              *
      *****************************************************************
       01  WS-DETAIL-SIDE.
           05  WS-DS-BAN               PIC X(13) VALUE SPACES.
           05  WS-DS-PERIOD            PIC 9(06) VALUE 0.
           05  WS-DS-SUM               PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-DS-ELEM-SUM          PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-DS-MINUTES           PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-DS-LINES             PIC S9(07) COMP-3 VALUE 0.
           05  WS-DS-ELEMS             PIC S9(11) COMP-3 VALUE 0.
           05  WS-DS-DELTA-SUM         PIC S9(11)V9(05) COMP-3 VALUE 0.
      *****************************************************************
      * THE COMPARISON.  THE HEADER CONTROL FIGURE IS COMPARED WITH THE*
      * DETAIL SIDE AND ANYTHING INSIDE THE TOLERANCE IS ACCEPTED.  THE*
      * TOLERANCE EXISTS BECAUSE THE TWO SIDES ARE CARRIED AT DIFFERENT*
      * PRECISIONS AND A LARGE ACCOUNT CAN DRIFT BY A FEW CENTS.      *
      *****************************************************************
       01  WS-COMPARE-WORK.
           05  WS-CW-HDR-AMOUNT        PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-CW-DTL-AMOUNT        PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-DIFF                 PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-CW-LINE-DIFF         PIC S9(07) COMP-3 VALUE 0.
           05  WS-CW-ELEM-DIFF         PIC S9(11) COMP-3 VALUE 0.
           05  WS-CW-TOLERANCE         PIC S9(03)V9(02) COMP-3
                                                        VALUE 0.05.
           05  WS-CW-RESULT            PIC X(11) VALUE SPACES.
       01  WS-ELEM-WALK.
           05  WS-EW-COUNT             PIC 9(03) VALUE 0.
           05  WS-EW-LINE-SUM          PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-EW-LINE-ROUNDED      PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-EW-LINE-DIFF         PIC S9(07)V9(05) COMP-3 VALUE 0.
           05  WS-EW-BAD-LINES         PIC S9(09) COMP-3 VALUE 0.
       01  WS-HDR-TABLE.
           05  WS-HT-ENTRY OCCURS 2000 TIMES INDEXED BY WS-HT-X.
               10  WS-HT-BAN           PIC X(13).
               10  WS-HT-HASH          PIC S9(15)V9(05) COMP-3.
               10  WS-HT-LINES         PIC S9(07) COMP-3.
               10  WS-HT-CDR           PIC S9(11) COMP-3.
               10  WS-HT-STATUS        PIC X(01).
       01  WS-HDR-CTL.
           05  WS-HT-USED              PIC S9(05) COMP-3 VALUE 0.
           05  WS-HT-MAX               PIC S9(05) COMP-3 VALUE 2000.
           05  WS-HT-HIT               PIC S9(05) COMP-3 VALUE 0.
       01  WS-RUN-TOTALS.
           05  WS-RT-ACCOUNTS          PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-IN-BALANCE        PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-OUT-OF-BALANCE    PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-NO-HEADER         PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-LINE-MISMATCH     PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-DTL-TOTAL         PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-RT-HDR-TOTAL         PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-RT-WORST-DIFF        PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-RT-WORST-BAN         PIC X(13) VALUE SPACES.
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
      * THE PROOF RECORD.  ONE PER ACCOUNT, READ BY CABRPT01.         *
      *****************************************************************
       01  WS-PROOF-RECORD.
           05  WS-PF-BAN               PIC X(13) VALUE SPACES.
           05  WS-PF-PERIOD            PIC 9(06) VALUE 0.
           05  WS-PF-DETAIL            PIC S9(15)V9(02) VALUE 0.
           05  WS-PF-HEADER            PIC S9(15)V9(05) VALUE 0.
           05  WS-PF-DIFF              PIC S9(13)V9(05) VALUE 0.
           05  WS-PF-LINES             PIC 9(07) VALUE 0.
           05  WS-PF-RESULT            PIC X(11) VALUE SPACES.
           05  WS-PF-DELTA             PIC S9(11)V9(05) VALUE 0.
           05  WS-PF-FILLER            PIC X(38) VALUE SPACES.
       01  WS-PROOF-RECORD-K REDEFINES WS-PROOF-RECORD.
           05  WS-PK-KEY               PIC X(19).
           05  WS-PK-REST              PIC X(101).
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
           OPEN OUTPUT PROOF-OUT-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 5111 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-BDTLIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 5112 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-BHDRIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 5113 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-PROOFOUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 5114 TO WS-AB-CODE
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
           PERFORM P4000-LOAD-HEADERS THRU P4000-EXIT.
           IF WS-PE-TOLERANCE NOT = ZERO
               MOVE WS-PE-TOLERANCE TO WS-CW-TOLERANCE.
           PERFORM P6000-HEADING THRU P6000-EXIT.
           DISPLAY WS-PGM-NAME ' STARTED - RUN ' WS-RUN-ID.
           DISPLAY '  TOLERANCE     ' WS-CW-TOLERANCE.
           DISPLAY '  ELEMENT CHECK ' WS-PE-ELEM-CHECK-SW.

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
      * THE TOLERANCE ARRIVES ON THE CARD.  A ZERO MEANS USE THE
      * STANDARD FIVE CENTS, NOT A TOLERANCE OF ZERO.
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
           IF WS-PE-TOLERANCE NOT NUMERIC
               MOVE ZERO TO WS-PE-TOLERANCE.
           IF WS-PE-ELEM-CHECK-SW NOT = 'N'
               MOVE 'Y' TO WS-PE-ELEM-CHECK-SW.
           IF WS-PE-LIST-SW NOT = 'Y'
               MOVE 'N' TO WS-PE-LIST-SW.
           IF WS-PE-ABEND-SW NOT = 'Y'
               MOVE 'N' TO WS-PE-ABEND-SW.

       P1200-EXIT.
           EXIT.

      *****************************************************************
      * S200-MAIN PROCESSING                                          *
      * ONE PASS OF THE SEQUENCED DETAIL FILE, ACCUMULATING PER ACCOUNT*
      * AND PROVING EACH ACCOUNT AT THE BREAK.                        *
      *****************************************************************
       S200-MAIN-PROCESS SECTION.

       P2000-PROCESS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ-DETAIL THRU P2100-EXIT.
           IF WS-DTL-EOF
               PERFORM P5000-CLOSE-ACCOUNT THRU P5000-EXIT
               MOVE 'Y' TO WS-EOF-SW
               GO TO P2000-EXIT.
           ADD 1 TO WS-READ-CNT.
           MOVE BD-BAN TO WS-RESTART-KEY.
           IF WS-ACCT-OPEN
               IF BD-BAN NOT = WS-DS-BAN
                   PERFORM P5000-CLOSE-ACCOUNT THRU P5000-EXIT.
           IF NOT WS-ACCT-OPEN
               PERFORM P3000-OPEN-ACCOUNT THRU P3000-EXIT.
           PERFORM P3100-ACCUM-LINE THRU P3100-EXIT.
           PERFORM P3200-WALK-ELEMENTS THRU P3200-EXIT.
           PERFORM P3300-PROVE-LINE THRU P3300-EXIT.
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
               MOVE 5101 TO WS-AB-CODE
               MOVE 'BILL DETAIL READ ERROR' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.

       P2100-EXIT.
           EXIT.

      *****************************************************************
      * S300-ACCUMULATION AND LINE LEVEL PROOF                        *
      *****************************************************************
       S300-ACCUM SECTION.

       P3000-OPEN-ACCOUNT.
           MOVE 'P3000-OPEN-ACCOUNT' TO WS-PARA-NAME.
           MOVE BD-BAN         TO WS-DS-BAN.
           MOVE BD-BILL-PERIOD TO WS-DS-PERIOD.
           MOVE ZERO TO WS-DS-SUM WS-DS-ELEM-SUM WS-DS-MINUTES
                        WS-DS-LINES WS-DS-ELEMS WS-DS-DELTA-SUM.
           MOVE 'Y' TO WS-ACCT-OPEN-SW.

       P3000-EXIT.
           EXIT.

       P3100-ACCUM-LINE.
      * THE ROUNDED LINE TOTAL IS THE BILLED FIGURE.  IT IS WHAT THE
      * CARRIER IS ASKED TO PAY AND IT IS WHAT THIS PROGRAM PROVES.
           MOVE 'P3100-ACCUM-LINE' TO WS-PARA-NAME.
           ADD BD-TOT-ROUNDED  TO WS-DS-SUM.
           ADD BD-TOT-MINUTES  TO WS-DS-MINUTES.
           ADD BD-ROUND-DELTA  TO WS-DS-DELTA-SUM.
           ADD 1               TO WS-DS-LINES.
           ADD BD-TOT-ROUNDED  TO WS-ACC-AMOUNT.
           ADD BD-TOT-MINUTES  TO WS-ACC-MINUTES.

       P3100-EXIT.
           EXIT.

       P3200-WALK-ELEMENTS.
      * WALK THE OCCURS DEPENDING ON AREA AND ADD THE ELEMENT AMOUNTS
      * UP INDEPENDENTLY.  THE ELEMENT SIDE IS CARRIED AT FIVE PLACES
      * BECAUSE THAT IS HOW THE RATING PROCESS PRODUCED IT.
      * RECORD LENGTHS ARE HELD IN THE DATASET REGISTER.
           MOVE 'P3200-WALK-ELEMENTS' TO WS-PARA-NAME.
           MOVE ZERO TO WS-EW-LINE-SUM.
           MOVE BD-ELEM-CNT TO WS-EW-COUNT.
           IF WS-EW-COUNT < 1
               MOVE 1 TO WS-EW-COUNT.
           IF WS-EW-COUNT > 40
               MOVE 40 TO WS-EW-COUNT.
           PERFORM P3210-ONE-ELEMENT THRU P3210-EXIT
               VARYING WS-SUB1 FROM 1 BY 1
               UNTIL WS-SUB1 > WS-EW-COUNT.
           ADD WS-EW-LINE-SUM TO WS-DS-ELEM-SUM.
           ADD WS-EW-COUNT    TO WS-DS-ELEMS.

       P3200-EXIT.
           EXIT.

       P3210-ONE-ELEMENT.
           SET BD-EX TO WS-SUB1.
           ADD BD-EL-AMOUNT (BD-EX) TO WS-EW-LINE-SUM.

       P3210-EXIT.
           EXIT.

       P3300-PROVE-LINE.
      * PROVE THE LINE.  THE SUM OF THE ELEMENT AMOUNTS ROUNDED TO THE
      * CENT MUST EQUAL THE ROUNDED LINE TOTAL CARRIED ON THE RECORD.
      * A LINE THAT DOES NOT PROVE IS LISTED AND COUNTED BUT DOES NOT
      * STOP THE RUN - THE ACCOUNT LEVEL PROOF IS THE CONTROLLING ONE.
           MOVE 'P3300-PROVE-LINE' TO WS-PARA-NAME.
           IF WS-PE-ELEM-CHECK-SW NOT = 'Y'
               GO TO P3300-EXIT.
           COMPUTE WS-EW-LINE-ROUNDED ROUNDED = WS-EW-LINE-SUM.
           COMPUTE WS-EW-LINE-DIFF =
                   WS-EW-LINE-ROUNDED - BD-TOT-ROUNDED.
           IF WS-EW-LINE-DIFF = ZERO
               GO TO P3300-EXIT.
           ADD 1 TO WS-EW-BAD-LINES.
           IF WS-PE-LIST-SW = 'Y'
               PERFORM P6100-PRINT-BAD-LINE THRU P6100-EXIT.

       P3300-EXIT.
           EXIT.

      *****************************************************************
      * S400-HEADER SIDE                                              *
      *****************************************************************
       S400-HEADER SECTION.

       P4000-LOAD-HEADERS.
           MOVE 'P4000-LOAD-HEADERS' TO WS-PARA-NAME.
           MOVE ZERO TO WS-HT-USED.
           PERFORM P4010-READ-HDR THRU P4010-EXIT
               UNTIL WS-HDR-EOF.
           DISPLAY 'HEADERS LOADED ' WS-HT-USED.

       P4000-EXIT.
           EXIT.

       P4010-READ-HDR.
           READ BHDR-IN-FILE INTO CABS-BILL-HEADER
               AT END
                   MOVE 'Y' TO WS-HDR-EOF-SW
                   GO TO P4010-EXIT.
           IF WS-HT-USED NOT < WS-HT-MAX
               MOVE 5102 TO WS-AB-CODE
               MOVE 'HEADER TABLE FULL' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-HT-USED.
           SET WS-HT-X TO WS-HT-USED.
           MOVE BH-BAN          TO WS-HT-BAN (WS-HT-X).
           MOVE BH-HASH-AMOUNT  TO WS-HT-HASH (WS-HT-X).
           MOVE BH-DETAIL-LINES TO WS-HT-LINES (WS-HT-X).
           MOVE BH-CDR-COUNT    TO WS-HT-CDR (WS-HT-X).
           MOVE BH-STATUS       TO WS-HT-STATUS (WS-HT-X).

       P4010-EXIT.
           EXIT.

       P4100-FIND-HEADER.
           MOVE 'P4100-FIND-HEADER' TO WS-PARA-NAME.
           MOVE 'N' TO WS-HDR-MATCH-SW.
           MOVE ZERO TO WS-HT-HIT.
           PERFORM P4110-MATCH-HDR THRU P4110-EXIT
               VARYING WS-HT-X FROM 1 BY 1
               UNTIL WS-HT-X > WS-HT-USED OR WS-HDR-MATCH.

       P4100-EXIT.
           EXIT.

       P4110-MATCH-HDR.
           IF WS-HT-BAN (WS-HT-X) = WS-DS-BAN
               SET WS-SUB1 TO WS-HT-X
               MOVE WS-SUB1 TO WS-HT-HIT
               MOVE 'Y' TO WS-HDR-MATCH-SW.

       P4110-EXIT.
           EXIT.

      *****************************************************************
      * S500-THE ACCOUNT LEVEL PROOF                                  *
      * THIS IS THE CONTROL THAT SAYS THE INVOICE ADDS UP.  IT IS THE *
      * ONLY PLACE IN THE STREAM WHERE THE DETAIL AND THE HEADER ARE  *
      * COMPARED AGAINST EACH OTHER.                                  *
      *****************************************************************
       S500-PROOF SECTION.

       P5000-CLOSE-ACCOUNT.
           MOVE 'P5000-CLOSE-ACCOUNT' TO WS-PARA-NAME.
           IF NOT WS-ACCT-OPEN
               GO TO P5000-EXIT.
           ADD 1 TO WS-RT-ACCOUNTS.
           PERFORM P4100-FIND-HEADER THRU P4100-EXIT.
           IF NOT WS-HDR-MATCH
               ADD 1 TO WS-RT-NO-HEADER
               ADD 1 TO WS-CFWD-CNT
               MOVE 'N' TO WS-ACCT-OPEN-SW
               GO TO P5000-EXIT.
           PERFORM P5100-LOAD-COMPARE THRU P5100-EXIT.
           PERFORM P5200-COMPARE-TOTALS THRU P5200-EXIT.
           PERFORM P5300-COMPARE-COUNTS THRU P5300-EXIT.
           PERFORM P5400-WRITE-PROOF THRU P5400-EXIT.
           MOVE 'N' TO WS-ACCT-OPEN-SW.

       P5000-EXIT.
           EXIT.

       P5100-LOAD-COMPARE.
           MOVE 'P5100-LOAD-COMPARE' TO WS-PARA-NAME.
           SET WS-HT-X TO WS-HT-HIT.
           MOVE WS-HT-HASH (WS-HT-X) TO WS-CW-HDR-AMOUNT.
           MOVE WS-DS-SUM            TO WS-CW-DTL-AMOUNT.
           ADD WS-DS-SUM             TO WS-RT-DTL-TOTAL.
           ADD WS-CW-HDR-AMOUNT      TO WS-RT-HDR-TOTAL.

       P5100-EXIT.
           EXIT.

       P5200-COMPARE-TOTALS.
      * THE PROOF.  THE HEADER CONTROL FIGURE MINUS THE DETAIL SIDE
      * MUST BE INSIDE THE TOLERANCE.  THE TOLERANCE IS FIVE CENTS,
      * WHICH IS THE FIGURE THE CONTROL STANDARD SETS FOR AN ACCOUNT
      * LEVEL PROOF.
           MOVE 'P5200-COMPARE-TOTALS' TO WS-PARA-NAME.
           MOVE 'Y' TO WS-BAL-SW.
           MOVE 'IN BALANCE' TO WS-CW-RESULT.
           COMPUTE WS-DIFF =
                   WS-CW-HDR-AMOUNT - WS-CW-DTL-AMOUNT.
           IF WS-DIFF > WS-CW-TOLERANCE
               MOVE 'N' TO WS-BAL-SW
               MOVE 'OUT OF BAL' TO WS-CW-RESULT
               ADD 1 TO WS-RT-OUT-OF-BALANCE
               MOVE EC-OUT-OF-BALANCE TO WS-ERR-CODE
               MOVE 'E' TO WS-ERR-SEVERITY
               MOVE 0008 TO WS-RETURN-CODE
               GO TO P5200-EXIT.
           ADD 1 TO WS-RT-IN-BALANCE.

       P5200-EXIT.
           EXIT.

       P5300-COMPARE-COUNTS.
      * THE LINE COUNT ON THE HEADER MUST MATCH THE NUMBER OF DETAIL
      * LINES ACTUALLY PRESENT.  A COUNT MISMATCH IS REPORTED BUT DOES
      * NOT ON ITS OWN PUT THE ACCOUNT OUT OF BALANCE.
           MOVE 'P5300-COMPARE-COUNTS' TO WS-PARA-NAME.
           SET WS-HT-X TO WS-HT-HIT.
           COMPUTE WS-CW-LINE-DIFF =
                   WS-HT-LINES (WS-HT-X) - WS-DS-LINES.
           IF WS-CW-LINE-DIFF NOT = ZERO
               ADD 1 TO WS-RT-LINE-MISMATCH.
           COMPUTE WS-CW-ELEM-DIFF =
                   WS-HT-CDR (WS-HT-X) - WS-DS-ELEMS.

       P5300-EXIT.
           EXIT.

       P5400-WRITE-PROOF.
      * WRITE THE PROOF RECORD AND PRINT THE LINE.  THE PROOF FILE IS
      * READ BY THE DAILY BALANCING REPORT.
           MOVE 'P5400-WRITE-PROOF' TO WS-PARA-NAME.
           MOVE SPACES TO WS-PROOF-RECORD.
           MOVE WS-DS-BAN          TO WS-PF-BAN.
           MOVE WS-DS-PERIOD       TO WS-PF-PERIOD.
           MOVE WS-CW-DTL-AMOUNT   TO WS-PF-DETAIL.
           MOVE WS-CW-HDR-AMOUNT   TO WS-PF-HEADER.
           MOVE WS-DIFF            TO WS-PF-DIFF.
           MOVE WS-DS-LINES        TO WS-PF-LINES.
           MOVE WS-CW-RESULT       TO WS-PF-RESULT.
           MOVE WS-DS-DELTA-SUM    TO WS-PF-DELTA.
           WRITE PROOF-RECORD FROM WS-PROOF-RECORD.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 5103 TO WS-AB-CODE
               MOVE 'PROOF FILE WRITE FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-WRITE-CNT.
           PERFORM P6200-PRINT-PROOF THRU P6200-EXIT.
           IF NOT WS-IN-BAL
               MOVE WS-PROOF-RECORD TO WS-EW-DATA
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               MOVE SPACES TO WS-ERR-CODE
               PERFORM P5500-WORST-CASE THRU P5500-EXIT.

       P5400-EXIT.
           EXIT.

       P5500-WORST-CASE.
           IF WS-DIFF > WS-RT-WORST-DIFF
               MOVE WS-DIFF   TO WS-RT-WORST-DIFF
               MOVE WS-DS-BAN TO WS-RT-WORST-BAN.

       P5500-EXIT.
           EXIT.

      *****************************************************************
      * S610-PROOF PRINTING                                           *
      *****************************************************************
       S610-PROOF-PRINT SECTION.

       P6100-PRINT-BAD-LINE.
           MOVE 'P6100-PRINT-BAD-LINE' TO WS-PARA-NAME.
           IF WS-PAGE-LINES > WS-MAX-LINES
               PERFORM P6000-HEADING THRU P6000-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE BD-BAN             TO PC-COL-001-020.
           MOVE BD-SECTION         TO PC-COL-021-060.
           MOVE BD-TOT-ROUNDED     TO WS-ED-MONEY.
           MOVE WS-ED-MONEY        TO PC-COL-061-090.
           MOVE WS-EW-LINE-ROUNDED TO WS-ED-MONEY.
           MOVE WS-ED-MONEY        TO PC-COL-091-132.
           WRITE PRT-RECORD FROM CABS-PRINT-LINE.
           ADD 1 TO WS-PAGE-LINES.

       P6100-EXIT.
           EXIT.

       P6200-PRINT-PROOF.
           MOVE 'P6200-PRINT-PROOF' TO WS-PARA-NAME.
           IF WS-PE-LIST-SW NOT = 'Y'
               IF WS-IN-BAL
                   GO TO P6200-EXIT.
           IF WS-PAGE-LINES > WS-MAX-LINES
               PERFORM P6000-HEADING THRU P6000-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           IF NOT WS-IN-BAL
               MOVE '0' TO PC-CC.
           MOVE WS-DS-BAN          TO PC-COL-001-020.
           MOVE WS-CW-DTL-AMOUNT   TO WS-ED-MONEY.
           MOVE WS-ED-MONEY        TO PC-COL-021-060.
           MOVE WS-CW-HDR-AMOUNT   TO WS-ED-MONEY.
           MOVE WS-ED-MONEY        TO PC-COL-061-090.
           MOVE WS-CW-RESULT       TO PC-COL-091-132.
           WRITE PRT-RECORD FROM CABS-PRINT-LINE.
           ADD 1 TO WS-PAGE-LINES.

       P6200-EXIT.
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
           MOVE 'CABBIL11  BILL LEVEL BALANCING REGISTER'
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
           MOVE 'BAN                 DETAIL SUM      HEADER SUM   RES'
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
           MOVE 455                    TO CT-STEP-SEQ.
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
           DISPLAY 'ACCOUNTS PROVED   ' WS-RT-ACCOUNTS.
           DISPLAY 'IN BALANCE        ' WS-RT-IN-BALANCE.
           DISPLAY 'OUT OF BALANCE    ' WS-RT-OUT-OF-BALANCE.
           DISPLAY 'NO HEADER FOUND   ' WS-RT-NO-HEADER.
           DISPLAY 'LINE COUNT DIFFS  ' WS-RT-LINE-MISMATCH.
           DISPLAY 'BAD ELEMENT LINES ' WS-EW-BAD-LINES.
           DISPLAY 'DETAIL SIDE TOTAL ' WS-RT-DTL-TOTAL.
           DISPLAY 'HEADER SIDE TOTAL ' WS-RT-HDR-TOTAL.
           DISPLAY 'WORST DIFFERENCE  ' WS-RT-WORST-DIFF.
           DISPLAY 'WORST ACCOUNT     ' WS-RT-WORST-BAN.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE BILL-DTL-IN
                 BHDR-IN-FILE
                 PROOF-OUT-FILE
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

