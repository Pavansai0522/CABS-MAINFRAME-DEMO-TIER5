      *****************************************************************
      * CABBIL10 - PRE BILL AUDIT AND HOLD BILL PROCESSING            *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               BHDRIN  TELCABS.CABS.BILLHDR.MMX(0)       CABSBHDR*
      *               PRIORIN TELCABS.CABS.BILLHDR.FIN(-1)      CABSBHDR*
      *               HOLDMST TELCABS.CABS.HOLDRSN              (LOCAL)*
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               BHDROUT TELCABS.CABS.BILLHDR.AUD(+1)      CABSBHDR*
      *               HOLDOUT TELCABS.CABS.BILLHOLD(+1)         CABSBHDR*
      *               REPORT  SYSOUT PRINT                      CABSPRNT*
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY (BAN)           *
      * REVISION HISTORY                                              *
      *   V1.00  1990-07-16  L.HARGREAVES INITIAL RELEASE - NEGATIVE BILL TEST*
      *                      ONLY, HELD LIST TYPED BY HAND            *
      *   V1.04  1993-04-27  M.J.FERRARO  BILL TO BILL VARIANCE TEST ADDED*
      *                      AGAINST LAST CYCLE HEADER FILE           *
      *   V1.08  1996-09-11  J.M.CASTILLO HOLD REASON MASTER INTRODUCED SO*
      *                      THE TEXT IS NOT COMPILED IN              *
      *   V2.00  2000-06-13  P.NAIR       JURISDICTIONAL SPLIT TEST ADDED*
      *                      AFTER THE 1999 SEPARATIONS QUERY         *
      *   V2.03  2004-10-19  A.BUKOWSKI   DELEGATED AUTHORITY LIMIT TEST*
      *   V2.06  2011-01-25  S.MARCHETTI  TOTAL DUE NOW REBUILT FROM THE*
      *                      COMPONENTS RATHER THAN TRUSTED           *
      *   V2.08  2016-02-15  G.PRZYBYLSKI HELD BILLS WRITTEN TO A SEPARATE*
      *                      FILE FOR THE MORNING WORK LIST           *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABBIL10.
       AUTHOR. TELCABS APPLICATIONS - BILLING TEAM.
      *****************************************************************
      * RUNS THE PRE BILL AUDIT OVER EVERY INVOICE AND HOLDS THE ONES *
      * THAT FAIL.  A HELD INVOICE IS NOT NUMBERED AND IS NOT PRINTED *
      * UNTIL THE BILLING CONTROL TEAM RELEASE IT.                    *
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
           SELECT PRIOR-IN-FILE ASSIGN TO UT-S-PRIORIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-TABLE.
           SELECT HOLD-MASTER ASSIGN TO DA-I-HOLDMST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS HR-KEY
               FILE STATUS IS WS-FS-TABLE.
           SELECT BHDR-OUT-FILE ASSIGN TO UT-S-BHDROUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT HOLD-OUT-FILE ASSIGN TO UT-S-HOLDOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-SUSPENSE.
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
      * BHDRIN - BILL HEADER WITH MINIMUM AND MAXIMUM                 *
      * ENFORCED.                                                     *
      *****************************************************************
       FD  BHDR-IN-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 400 CHARACTERS.
       01  BHDR-IN-REC                      PIC X(400).
      *****************************************************************
      * PRIORIN - LAST CYCLE FINAL HEADER FILE, READ AT               *
      * GENERATION MINUS ONE FOR THE VARIANCE TEST.                   *
      *****************************************************************
       FD  PRIOR-IN-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 400 CHARACTERS.
       01  PRIOR-RECORD                     PIC X(400).
      *****************************************************************
      * HOLDMST - HOLD REASON MASTER.  VSAM KSDS DEFINED BY AN        *
      * IDCAMS JOB.  NOTHING IN THE ESTATE WRITES IT.                 *
      *****************************************************************
       FD  HOLD-MASTER
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-HOLD-REASON-RECORD.
           05  HR-KEY.
               10  HR-CODE             PIC X(04).
           05  HR-TEXT                 PIC X(40).
           05  HR-SEVERITY             PIC X(01).
           05  HR-RELEASE-SW           PIC X(01).
           05  HR-OWNER                PIC X(08).
           05  HR-FILLER               PIC X(26).
      *****************************************************************
      * BHDROUT - EVERY HEADER, HELD OR RELEASED.                     *
      *****************************************************************
       FD  BHDR-OUT-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 400 CHARACTERS.
       01  BHDR-OUT-REC                     PIC X(400).
      *****************************************************************
      * HOLDOUT - THE HELD ONES ONLY, FOR THE MORNING                 *
      * WORK LIST.                                                    *
      *****************************************************************
       FD  HOLD-OUT-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 400 CHARACTERS.
       01  HOLD-RECORD                      PIC X(400).
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
           05  WS-PGM-NAME             PIC X(08) VALUE 'CABBIL10'.
           05  WS-PGM-VERSION          PIC X(05) VALUE 'V2.08'.
           05  WS-PGM-APPL             PIC X(04) VALUE 'CABS'.
           05  WS-PGM-COMPILE          PIC X(08) VALUE '20160215'.
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
           05  WS-PE-AUDIT-SW          PIC X(01).
           05  WS-PE-VAR-PCT           PIC 9(03)V9(02).
           05  WS-PE-MIN-LINES         PIC 9(05).
           05  WS-PE-HOLD-LIMIT        PIC 9(09)V9(02).
           05  WS-PE-FILLER            PIC X(21).
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
           05  WS-PRI-EOF-SW           PIC X(01) VALUE 'N'.
               88  WS-PRI-EOF          VALUE 'Y'.
           05  WS-HOLD-SW              PIC X(01) VALUE 'N'.
               88  WS-HOLDING          VALUE 'Y'.
           05  WS-PRIOR-FOUND-SW       PIC X(01) VALUE 'N'.
               88  WS-PRIOR-FOUND      VALUE 'Y'.
      *****************************************************************
      * THE PRE BILL AUDIT.  EIGHT TESTS.  THE FIRST ONE THAT FAILS   *
      * SETS THE HOLD REASON AND THE REMAINING TESTS ARE NOT MADE, SO *
      * AN ACCOUNT THAT FAILS TWO TESTS IS REPORTED UNDER THE FIRST.  *
      *****************************************************************
       01  WS-HOLD-WORK.
           05  WS-HB-BASIS             PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-HB-PRIOR-TOTAL       PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-HB-VARIANCE          PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-HB-VAR-PCT           PIC S9(05)V9(02) COMP-3 VALUE 0.
           05  WS-HB-REASON            PIC X(04) VALUE SPACES.
           05  WS-HB-JURIS-SUM         PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-HB-CURRENT           PIC S9(13)V9(02) COMP-3 VALUE 0.
      *****************************************************************
      * HOLD REASON TABLE.  LOADED FROM THE VSAM HOLD REASON MASTER,  *
      * WHICH IS DEFINED BY AN IDCAMS JOB AND MAINTAINED BY THE BILLING*
      * CONTROL TEAM.  NO PROGRAM IN THIS ESTATE WRITES IT.           *
      *****************************************************************
       01  WS-HOLD-TABLE.
           05  WS-HR-ENTRY OCCURS 60 TIMES INDEXED BY WS-HR-X.
               10  WS-HR-CODE          PIC X(04).
               10  WS-HR-TEXT          PIC X(40).
               10  WS-HR-SEVERITY      PIC X(01).
               10  WS-HR-RELEASE-SW    PIC X(01).
       01  WS-HOLD-CTL.
           05  WS-HR-USED              PIC S9(03) COMP-3 VALUE 0.
           05  WS-HR-MAX               PIC S9(03) COMP-3 VALUE 60.
           05  WS-HR-HIT               PIC S9(03) COMP-3 VALUE 0.
           05  WS-HR-FOUND-SW          PIC X(01) VALUE 'N'.
               88  WS-HR-FOUND         VALUE 'Y'.
      *****************************************************************
      * THE PRIOR BILL TABLE.  USED FOR THE BILL TO BILL VARIANCE TEST.*
      * THE PRIOR HEADER FILE IS THE ONE THIS PROGRAM WROTE LAST CYCLE.*
      *****************************************************************
       01  WS-PRIOR-TABLE.
           05  WS-PT-ENTRY OCCURS 2000 TIMES INDEXED BY WS-PT-X.
               10  WS-PT-BAN           PIC X(13).
               10  WS-PT-TOTAL         PIC S9(13)V9(02) COMP-3.
               10  WS-PT-LINES         PIC S9(07) COMP-3.
               10  WS-PT-STATUS        PIC X(01).
       01  WS-PRIOR-CTL.
           05  WS-PT-USED              PIC S9(05) COMP-3 VALUE 0.
           05  WS-PT-MAX               PIC S9(05) COMP-3 VALUE 2000.
           05  WS-PT-HIT               PIC S9(05) COMP-3 VALUE 0.
       01  WS-PRIOR-IN.
           05  WS-PI-IMAGE             PIC X(400).
       01  WS-RUN-TOTALS.
           05  WS-RT-HEADERS           PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-HELD              PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-RELEASED          PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-NO-PRIOR          PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-HELD-AMT          PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-RT-REASON-CNT OCCURS 8 TIMES
                                       PIC S9(09) COMP-3.
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
                       PRIOR-IN-FILE
                       HOLD-MASTER
                       PARM-FILE
           OPEN OUTPUT BHDR-OUT-FILE
                       HOLD-OUT-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 5011 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-BHDRIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 5012 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-PRIORIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 5013 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-BHDROUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 5014 TO WS-AB-CODE
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
           PERFORM P4300-LOAD-REASONS THRU P4300-EXIT.
           PERFORM P4000-LOAD-PRIOR THRU P4000-EXIT.
           PERFORM P4400-CLEAR-COUNTS THRU P4400-EXIT
               VARYING WS-SUB1 FROM 1 BY 1
               UNTIL WS-SUB1 > 8.
           PERFORM P6000-HEADING THRU P6000-EXIT.
           DISPLAY WS-PGM-NAME ' STARTED - RUN ' WS-RUN-ID.
           DISPLAY '  AUDIT SWITCH  ' WS-PE-AUDIT-SW.
           DISPLAY '  VARIANCE PCT  ' WS-PE-VAR-PCT.
           DISPLAY '  HOLD LIMIT    ' WS-PE-HOLD-LIMIT.

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
      * THE VARIANCE PERCENTAGE AND THE DELEGATED AUTHORITY LIMIT
      * ARE BOTH SUPPLIED BY THE SCHEDULER FROM THE CURRENT CONTROL
      * STANDARD.  NEITHER IS CODED IN THE JCL.
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
           IF WS-PE-AUDIT-SW NOT = 'N'
               MOVE 'Y' TO WS-PE-AUDIT-SW.
           IF WS-PE-VAR-PCT NOT NUMERIC
               MOVE 5021 TO WS-AB-CODE
               MOVE 'VARIANCE PERCENT NOT NUMERIC' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PE-MIN-LINES NOT NUMERIC
               MOVE 1 TO WS-PE-MIN-LINES.
           IF WS-PE-HOLD-LIMIT NOT NUMERIC
               MOVE ZERO TO WS-PE-HOLD-LIMIT.

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
           MOVE 'N' TO WS-HOLD-SW.
           MOVE SPACES TO WS-HB-REASON.
           PERFORM P2300-RECOMPUTE-TOTAL THRU P2300-EXIT.
           PERFORM P2400-SET-AUDIT-BASIS THRU P2400-EXIT.
           PERFORM P3000-PREBILL-AUDIT THRU P3000-EXIT.
           PERFORM P3600-APPLY-ADJ-TO-TOTAL THRU P3600-EXIT.
           PERFORM P3700-SET-STATUS THRU P3700-EXIT.
           PERFORM P3800-WRITE-HEADER THRU P3800-EXIT.

       P2000-EXIT.
           EXIT.

       P2100-READ-HEADER.
           MOVE 'P2100-READ-HEADER' TO WS-PARA-NAME.
           READ BHDR-IN-FILE INTO CABS-BILL-HEADER
               AT END
                   MOVE 'Y' TO WS-HDR-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00' AND WS-FS-INPUT NOT = '10'
               MOVE 5001 TO WS-AB-CODE
               MOVE 'BILL HEADER READ ERROR' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.

       P2100-EXIT.
           EXIT.

       P2300-RECOMPUTE-TOTAL.
      * THE TOTAL DUE IS REBUILT FROM ITS COMPONENTS HERE.  FOUR EARLIER
      * STEPS CAN EACH HAVE TOUCHED A COMPONENT AFTER THE LAST TOTAL WAS
      * STRUCK, SO THE FIGURE ARRIVING ON THE RECORD IS NOT TRUSTED.
           MOVE 'P2300-RECOMPUTE-TOTAL' TO WS-PARA-NAME.
           COMPUTE BH-TOTAL-DUE =
                   BH-PRIOR-BAL - BH-PAYMENTS
                 + BH-CURR-USAGE + BH-CURR-RECURRING
                 + BH-CURR-NONRECUR + BH-SETTLEMENT-NET
                 + BH-TAX.
           COMPUTE WS-HB-CURRENT =
                   BH-CURR-USAGE + BH-CURR-RECURRING
                 + BH-CURR-NONRECUR.

       P2300-EXIT.
           EXIT.

       P2400-SET-AUDIT-BASIS.
           MOVE 'P2400-SET-AUDIT-BASIS' TO WS-PARA-NAME.
           MOVE BH-TOTAL-DUE TO WS-HB-BASIS.
           COMPUTE WS-HB-JURIS-SUM =
                   BH-INTERSTATE-AMT + BH-INTRASTATE-AMT
                 + BH-LOCAL-AMT.

       P2400-EXIT.
           EXIT.

      *****************************************************************
      * S300-PRE BILL AUDIT                                           *
      * EIGHT TESTS RUN IN A FIXED ORDER.  AN ACCOUNT THAT FAILS ANY  *
      * ONE OF THEM IS HELD, ITS INVOICE IS NOT NUMBERED AND IT IS NOT*
      * PASSED TO THE PRINT FAMILY.  THE BILLING CONTROL TEAM WORK THE*
      * HELD LIST EVERY MORNING AND RELEASE OR CANCEL EACH ONE.       *
      *****************************************************************
       S300-AUDIT SECTION.

       P3000-PREBILL-AUDIT.
           MOVE 'P3000-PREBILL-AUDIT' TO WS-PARA-NAME.
           IF WS-PE-AUDIT-SW NOT = 'Y'
               GO TO P3000-EXIT.
           PERFORM P3100-HOLD-TEST-NEGATIVE THRU P3100-EXIT.
           IF WS-HOLDING
               GO TO P3000-EXIT.
           PERFORM P3200-HOLD-TEST-VARIANCE THRU P3200-EXIT.
           IF WS-HOLDING
               GO TO P3000-EXIT.
           PERFORM P3300-HOLD-TEST-LINES THRU P3300-EXIT.
           IF WS-HOLDING
               GO TO P3000-EXIT.
           PERFORM P3400-HOLD-TEST-JURIS THRU P3400-EXIT.
           IF WS-HOLDING
               GO TO P3000-EXIT.
           PERFORM P3500-HOLD-TEST-LIMIT THRU P3500-EXIT.

       P3000-EXIT.
           EXIT.

       P3100-HOLD-TEST-NEGATIVE.
      * A NEGATIVE INVOICE IS A REFUND AND MUST NOT BE POSTED WITHOUT A
      * HUMAN LOOKING AT IT.  THE TEST IS MADE ON THE AUDIT BASIS SET
      * IN P2400.
           MOVE 'P3100-HOLD-TEST-NEGATIVE' TO WS-PARA-NAME.
           IF WS-HB-BASIS < ZERO
               MOVE 'HNEG' TO WS-HB-REASON
               MOVE 'Y' TO WS-HOLD-SW
               ADD 1 TO WS-RT-REASON-CNT (1).

       P3100-EXIT.
           EXIT.

       P3200-HOLD-TEST-VARIANCE.
      * BILL TO BILL VARIANCE.  A BILL THAT MOVES BY MORE THAN THE
      * AGREED PERCENTAGE AGAINST THE SAME ACCOUNT LAST CYCLE IS HELD.
      * AN ACCOUNT WITH NO PRIOR BILL CANNOT BE TESTED AND IS RELEASED.
           MOVE 'P3200-HOLD-TEST-VARIANCE' TO WS-PARA-NAME.
           PERFORM P4100-FIND-PRIOR THRU P4100-EXIT.
           IF NOT WS-PRIOR-FOUND
               ADD 1 TO WS-RT-NO-PRIOR
               GO TO P3200-EXIT.
           IF WS-HB-PRIOR-TOTAL = ZERO
               GO TO P3200-EXIT.
           COMPUTE WS-HB-VARIANCE =
                   WS-HB-BASIS - WS-HB-PRIOR-TOTAL.
           IF WS-HB-VARIANCE < ZERO
               COMPUTE WS-HB-VARIANCE = WS-HB-VARIANCE * -1.
           COMPUTE WS-HB-VAR-PCT ROUNDED =
                   (WS-HB-VARIANCE * 100) / WS-HB-PRIOR-TOTAL.
           IF WS-HB-VAR-PCT > WS-PE-VAR-PCT
               MOVE 'HVAR' TO WS-HB-REASON
               MOVE 'Y' TO WS-HOLD-SW
               ADD 1 TO WS-RT-REASON-CNT (2).

       P3200-EXIT.
           EXIT.

       P3300-HOLD-TEST-LINES.
      * A BILL WITH NO DETAIL LINES AND A NON ZERO CURRENT CHARGE HAS
      * LOST ITS DETAIL SOMEWHERE IN THE STREAM.  A BILL WITH NEITHER
      * IS A LEGITIMATE ZERO BILL AND IS RELEASED.
           MOVE 'P3300-HOLD-TEST-LINES' TO WS-PARA-NAME.
           IF BH-DETAIL-LINES NOT < WS-PE-MIN-LINES
               GO TO P3300-EXIT.
           IF WS-HB-CURRENT = ZERO
               GO TO P3300-EXIT.
           MOVE 'HLIN' TO WS-HB-REASON.
           MOVE 'Y' TO WS-HOLD-SW.
           ADD 1 TO WS-RT-REASON-CNT (3).

       P3300-EXIT.
           EXIT.

       P3400-HOLD-TEST-JURIS.
      * THE THREE JURISDICTIONAL AMOUNTS MUST ADD BACK TO THE CURRENT
      * PERIOD CHARGE.  A MISMATCH MEANS THE SEPARATIONS FILING WOULD
      * BE WRONG, WHICH IS A REGULATORY EXPOSURE, NOT A BILLING ONE.
           MOVE 'P3400-HOLD-TEST-JURIS' TO WS-PARA-NAME.
           IF BH-CURR-USAGE = ZERO
               GO TO P3400-EXIT.
           COMPUTE WS-HB-VARIANCE =
                   WS-HB-JURIS-SUM - WS-HB-CURRENT.
           IF WS-HB-VARIANCE < ZERO
               COMPUTE WS-HB-VARIANCE = WS-HB-VARIANCE * -1.
           IF WS-HB-VARIANCE > 1
               MOVE 'HJUR' TO WS-HB-REASON
               MOVE 'Y' TO WS-HOLD-SW
               ADD 1 TO WS-RT-REASON-CNT (4).

       P3400-EXIT.
           EXIT.

       P3500-HOLD-TEST-LIMIT.
      * AN INVOICE ABOVE THE DELEGATED AUTHORITY LIMIT IS HELD FOR
      * MANAGEMENT REVIEW BEFORE IT LEAVES THE BUILDING.
           MOVE 'P3500-HOLD-TEST-LIMIT' TO WS-PARA-NAME.
           IF WS-PE-HOLD-LIMIT = ZERO
               GO TO P3500-EXIT.
           IF WS-HB-BASIS > WS-PE-HOLD-LIMIT
               MOVE 'HLIM' TO WS-HB-REASON
               MOVE 'Y' TO WS-HOLD-SW
               ADD 1 TO WS-RT-REASON-CNT (5).

       P3500-EXIT.
           EXIT.

       P3600-APPLY-ADJ-TO-TOTAL.
      * ADJUSTMENTS AND RESTATEMENTS ARE ROLLED INTO THE TOTAL DUE.
      * THEY ARE HELD OUT OF THE RECOMPUTATION IN P2300 SO THAT THIS
      * PERIOD'S OWN FIGURES STAND AGAINST THE DETAIL ON THEIR OWN,
      * WITHOUT AN ADJUSTMENT MOVING THEM.
           MOVE 'P3600-APPLY-ADJ-TO-TOTAL' TO WS-PARA-NAME.
           COMPUTE BH-TOTAL-DUE =
                   BH-TOTAL-DUE + BH-ADJUSTMENTS
                 + BH-RESTATEMENT.

       P3600-EXIT.
           EXIT.

       P3700-SET-STATUS.
      * SET THE STATUS AND THE HOLD REASON ON THE HEADER.  A HELD BILL
      * IS NOT NUMBERED AND IS NOT PRINTED.  A RELEASED BILL GOES
      * FORWARD AS PENDING UNTIL THE NUMBERING STEP MAKES IT FINAL.
           MOVE 'P3700-SET-STATUS' TO WS-PARA-NAME.
           IF WS-HOLDING
               MOVE 'H' TO BH-STATUS
               MOVE WS-HB-REASON TO BH-HOLD-REASON
               ADD 1 TO WS-RT-HELD
               ADD BH-TOTAL-DUE TO WS-RT-HELD-AMT
               ADD 1 TO WS-CFWD-CNT
               PERFORM P4200-LOOKUP-REASON THRU P4200-EXIT
               PERFORM P5000-REGISTER-LINE THRU P5000-EXIT
               GO TO P3700-EXIT.
           MOVE 'P' TO BH-STATUS.
           MOVE SPACES TO BH-HOLD-REASON.
           ADD 1 TO WS-RT-RELEASED.

       P3700-EXIT.
           EXIT.

       P3800-WRITE-HEADER.
           MOVE 'P3800-WRITE-HEADER' TO WS-PARA-NAME.
           IF WS-HOLDING
               WRITE HOLD-RECORD FROM CABS-BILL-HEADER
               IF WS-FS-SUSPENSE NOT = '00'
                   MOVE 5002 TO WS-AB-CODE
                   MOVE 'HELD BILL WRITE FAILED' TO WS-AB-TEXT
                   PERFORM P9500-ABEND THRU P9500-EXIT.
           WRITE BHDR-OUT-REC FROM CABS-BILL-HEADER.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 5003 TO WS-AB-CODE
               MOVE 'BILL HEADER WRITE FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-WRITE-CNT.
           ADD 1 TO WS-RT-HEADERS.
           ADD BH-TOTAL-DUE TO WS-ACC-AMOUNT.

       P3800-EXIT.
           EXIT.

      *****************************************************************
      * S400-SUPPORTING TABLES                                        *
      *****************************************************************
       S400-SUPPORT SECTION.

       P4000-LOAD-PRIOR.
      * LOAD LAST CYCLE HEADER FILE FOR THE VARIANCE TEST.
           MOVE 'P4000-LOAD-PRIOR' TO WS-PARA-NAME.
           MOVE ZERO TO WS-PT-USED.
           PERFORM P4010-READ-PRIOR THRU P4010-EXIT
               UNTIL WS-PRI-EOF.
           DISPLAY 'PRIOR BILLS LOADED ' WS-PT-USED.

       P4000-EXIT.
           EXIT.

       P4010-READ-PRIOR.
           READ PRIOR-IN-FILE INTO CABS-BILL-HEADER
               AT END
                   MOVE 'Y' TO WS-PRI-EOF-SW
                   GO TO P4010-EXIT.
           IF WS-PT-USED NOT < WS-PT-MAX
               MOVE 5004 TO WS-AB-CODE
               MOVE 'PRIOR BILL TABLE FULL' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-PT-USED.
           SET WS-PT-X TO WS-PT-USED.
           MOVE BH-BAN           TO WS-PT-BAN (WS-PT-X).
           MOVE BH-TOTAL-DUE     TO WS-PT-TOTAL (WS-PT-X).
           MOVE BH-DETAIL-LINES  TO WS-PT-LINES (WS-PT-X).
           MOVE BH-STATUS        TO WS-PT-STATUS (WS-PT-X).

       P4010-EXIT.
           EXIT.

       P4100-FIND-PRIOR.
           MOVE 'P4100-FIND-PRIOR' TO WS-PARA-NAME.
           MOVE 'N' TO WS-PRIOR-FOUND-SW.
           MOVE ZERO TO WS-HB-PRIOR-TOTAL.
           PERFORM P4110-MATCH-PRIOR THRU P4110-EXIT
               VARYING WS-PT-X FROM 1 BY 1
               UNTIL WS-PT-X > WS-PT-USED OR WS-PRIOR-FOUND.

       P4100-EXIT.
           EXIT.

       P4110-MATCH-PRIOR.
           IF WS-PT-BAN (WS-PT-X) NOT = BH-BAN
               GO TO P4110-EXIT.
           IF WS-PT-STATUS (WS-PT-X) = 'C'
               GO TO P4110-EXIT.
           MOVE WS-PT-TOTAL (WS-PT-X) TO WS-HB-PRIOR-TOTAL.
           MOVE 'Y' TO WS-PRIOR-FOUND-SW.

       P4110-EXIT.
           EXIT.

       P4200-LOOKUP-REASON.
           MOVE 'P4200-LOOKUP-REASON' TO WS-PARA-NAME.
           MOVE 'N' TO WS-HR-FOUND-SW.
           MOVE ZERO TO WS-HR-HIT.
           PERFORM P4210-MATCH-REASON THRU P4210-EXIT
               VARYING WS-HR-X FROM 1 BY 1
               UNTIL WS-HR-X > WS-HR-USED OR WS-HR-FOUND.

       P4200-EXIT.
           EXIT.

       P4210-MATCH-REASON.
           IF WS-HR-CODE (WS-HR-X) = WS-HB-REASON
               SET WS-SUB1 TO WS-HR-X
               MOVE WS-SUB1 TO WS-HR-HIT
               MOVE 'Y' TO WS-HR-FOUND-SW.

       P4210-EXIT.
           EXIT.

       P4300-LOAD-REASONS.
      * LOAD THE HOLD REASON MASTER.  A VSAM KSDS DEFINED BY AN IDCAMS
      * JOB IN THE VSAM LIBRARY AND MAINTAINED BY THE BILLING CONTROL
      * TEAM.  NO PROGRAM IN THE ESTATE WRITES IT.
      * SPACE AND SHAREOPTIONS ARE OWNED BY STORAGE MANAGEMENT.
           MOVE 'P4300-LOAD-REASONS' TO WS-PARA-NAME.
           MOVE ZERO TO WS-HR-USED.
           MOVE LOW-VALUES TO HR-KEY.
           START HOLD-MASTER KEY NOT LESS THAN HR-KEY
               INVALID KEY
                   MOVE 5005 TO WS-AB-CODE
                   MOVE 'HOLD REASON MASTER UNREADABLE'
                                       TO WS-AB-TEXT
                   PERFORM P9500-ABEND THRU P9500-EXIT.
           PERFORM P4310-READ-REASON THRU P4310-EXIT
               UNTIL WS-FS-TABLE NOT = '00'.
           DISPLAY 'HOLD REASONS LOADED ' WS-HR-USED.

       P4300-EXIT.
           EXIT.

       P4310-READ-REASON.
           READ HOLD-MASTER NEXT RECORD
               AT END
                   MOVE '10' TO WS-FS-TABLE
                   GO TO P4310-EXIT.
           IF WS-FS-TABLE NOT = '00'
               GO TO P4310-EXIT.
           IF WS-HR-USED NOT < WS-HR-MAX
               GO TO P4310-EXIT.
           ADD 1 TO WS-HR-USED.
           SET WS-HR-X TO WS-HR-USED.
           MOVE HR-CODE       TO WS-HR-CODE (WS-HR-X).
           MOVE HR-TEXT       TO WS-HR-TEXT (WS-HR-X).
           MOVE HR-SEVERITY   TO WS-HR-SEVERITY (WS-HR-X).
           MOVE HR-RELEASE-SW TO WS-HR-RELEASE-SW (WS-HR-X).

       P4310-EXIT.
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
           MOVE WS-HB-REASON           TO PC-COL-021-060.
           IF WS-HR-FOUND
               SET WS-HR-X TO WS-HR-HIT
               MOVE WS-HR-TEXT (WS-HR-X) TO PC-COL-021-060.
           MOVE WS-HB-VAR-PCT          TO WS-ED-PCT.
           MOVE WS-ED-PCT              TO PC-COL-061-090.
           MOVE BH-TOTAL-DUE           TO WS-ED-MONEY.
           MOVE WS-ED-MONEY            TO PC-COL-091-132.
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
           MOVE 'CABBIL10  PRE BILL AUDIT AND HOLD REGISTER'
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
           MOVE 'BAN                 HOLD REASON              VAR%   '
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
           MOVE 450                    TO CT-STEP-SEQ.
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
           DISPLAY 'BILLS HELD        ' WS-RT-HELD.
           DISPLAY 'BILLS RELEASED    ' WS-RT-RELEASED.
           DISPLAY 'NO PRIOR BILL     ' WS-RT-NO-PRIOR.
           DISPLAY 'HELD VALUE        ' WS-RT-HELD-AMT.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE BHDR-IN-FILE
                 PRIOR-IN-FILE
                 HOLD-MASTER
                 BHDR-OUT-FILE
                 HOLD-OUT-FILE
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

       P4400-CLEAR-COUNTS.
           MOVE ZERO TO WS-RT-REASON-CNT (WS-SUB1).

       P4400-EXIT.
           EXIT.
