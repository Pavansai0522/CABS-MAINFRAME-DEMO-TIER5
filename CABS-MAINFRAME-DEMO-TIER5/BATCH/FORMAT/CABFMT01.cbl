      *****************************************************************
      * CABFMT01 - INVOICE PAGE AND SECTION FORMATTING                *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               BDTLIN  TELCABS.CABS.BILLDTL.SEQ(0)       CABSBILL*
      *               BHDRIN  TELCABS.CABS.BILLHDR.FIN(0)       CABSBHDR*
      *               SYSIN   INSTREAM CONTROL CARD             (LOCAL)*
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               PRTOUT  TELCABS.CABS.PRINT.STREAM(+1)     CABSPRNT*
      *               PRTCTL  TELCABS.CABS.PRTCTL(+1)           (LOCAL)*
      *               REPORT  SYSOUT PRINT                      CABSPRNT*
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-SUMMARISED + CT-CARRIED-FWD        *
      *               CT-WRITTEN IS THE PRINT LINE COUNT              *
      * RESTART     : FULL RERUN - THE PRINT STREAM IS REBUILT WHOLE  *
      * REVISION HISTORY                                              *
      *   V1.00  1988-03-21  K.OYELARAN   INITIAL RELEASE - ONE PAGE PER*
      *                      INVOICE, NO SECTION BREAKS               *
      *   V1.04  1990-08-09  D.OKONKWO    SECTION BREAK ON CHANNEL FOUR ADDED*
      *                      FOR THE NEW BURSTER                      *
      *   V1.08  1993-05-18  M.J.FERRARO  INVOICE BREAK MOVED TO CHANNEL SEVEN*
      *                      SO THE INSERTER CAN SEE IT               *
      *   V1.12  1996-11-04  J.M.CASTILLO PRINT CONTROL RECORD INTRODUCED FOR*
      *                      THE MAILROOM RECONCILIATION              *
      *   V2.00  2000-09-26  P.NAIR       PAGE NUMBER NOW RESTARTS PER INVOICE*
      *   V2.06  2005-02-08  A.BUKOWSKI   HELD AND CANCELLED INVOICES ARE NOT*
      *                      FORMATTED AT ALL                         *
      *   V3.00  2011-06-14  R.KAMINSKI   SECTION NAME TABLE MOVED IN LINE TO*
      *                      MATCH THE FILED TARIFF WORDING           *
      *   V3.03  2018-04-05  G.PRZYBYLSKI LINES PER PAGE ACCEPTED FROM THE*
      *                      CONTROL CARD FOR THE A4 TRIAL            *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABFMT01.
       AUTHOR. TELCABS APPLICATIONS - BILL PRINT TEAM.
      *****************************************************************
      * TURNS THE SEQUENCED BILL DETAIL INTO THE PRINT STREAM.  OWNS  *
      * THE CARRIAGE CONTROL CHARACTER, WHICH CARRIES BUSINESS MEANING*
      * AS WELL AS PRINTER POSITIONING, AND THE PRINT CONTROL RECORD. *
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
           SELECT PRINT-STREAM ASSIGN TO UT-S-PRTOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT PRTCTL-FILE ASSIGN TO UT-S-PRTCTL
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
      * BHDRIN - THE FINAL NUMBERED INVOICE HEADER.                   *
      *****************************************************************
       FD  BHDR-IN-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 400 CHARACTERS.
       01  BHDR-IN-REC                      PIC X(400).
      *****************************************************************
      * PRTOUT - THE PRINT STREAM.  FBA 133.  COLUMN ONE IS THE       *
      * CARRIAGE CONTROL CHARACTER AND CARRIES BUSINESS MEANING.      *
      *****************************************************************
       FD  PRINT-STREAM
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       01  PRINT-LINE                   PIC X(133).
      *****************************************************************
      * PRTCTL - ONE CONTROL RECORD PER INVOICE FOR THE               *
      * BURST PROCESS AND THE MAILROOM.                               *
      *****************************************************************
       FD  PRTCTL-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 90 CHARACTERS.
       01  PRTCTL-RECORD                    PIC X(90).
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
           05  WS-PGM-NAME             PIC X(08) VALUE 'CABFMT01'.
           05  WS-PGM-VERSION          PIC X(05) VALUE 'V3.03'.
           05  WS-PGM-APPL             PIC X(04) VALUE 'CABS'.
           05  WS-PGM-COMPILE          PIC X(08) VALUE '20180405'.
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
           05  WS-PE-LINES-PAGE        PIC 9(03).
           05  WS-PE-MEDIA             PIC X(01).
           05  WS-PE-BURST-SW          PIC X(01).
           05  WS-PE-SECT-BREAK-SW     PIC X(01).
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
           05  WS-INV-OPEN-SW          PIC X(01) VALUE 'N'.
               88  WS-INV-OPEN         VALUE 'Y'.
           05  WS-HDR-MATCH-SW         PIC X(01) VALUE 'N'.
               88  WS-HDR-MATCH        VALUE 'Y'.
      *****************************************************************
      * PAGE AND SECTION CONTROL.  THE CARRIAGE CONTROL CHARACTER IN  *
      * COLUMN ONE IS NOT ONLY A PRINTER INSTRUCTION - THE BURST AND  *
      * INSERT MACHINERY READS IT TOO.  A SEVEN STARTS A NEW INVOICE  *
      * AND A FOUR STARTS A NEW BILL SECTION.  BOTH COUNTS ARE CARRIED*
      * ON THE PRINT CONTROL RECORD SO THAT THE MAILROOM CAN RECONCILE*
      * WHAT CAME OFF THE PRINTER AGAINST WHAT WAS SENT.              *
      * PAGE CONTROL FOLLOWS CABS-STD-063.                            *
      *****************************************************************
       01  WS-PAGE-CONTROL.
           05  WS-PG-LINE-CNT          PIC S9(05) COMP-3 VALUE 0.
           05  WS-PG-PAGE-CNT          PIC S9(05) COMP-3 VALUE 0.
           05  WS-PG-INV-PAGE          PIC S9(05) COMP-3 VALUE 0.
           05  WS-PG-SECTION-CNT       PIC S9(07) COMP-3 VALUE 0.
           05  WS-PG-INVOICE-CNT       PIC S9(07) COMP-3 VALUE 0.
           05  WS-PG-LINE-WRITTEN      PIC S9(11) COMP-3 VALUE 0.
           05  WS-PG-PRIOR-SECTION     PIC X(02) VALUE SPACES.
           05  WS-PG-PRIOR-BAN         PIC X(13) VALUE SPACES.
           05  WS-PG-MAX-LINES         PIC S9(03) COMP-3 VALUE 60.
           05  WS-PG-CC-SAVE           PIC X(01) VALUE SPACES.
      *****************************************************************
      * THE PRINT CONTROL RECORD.  ONE PER INVOICE.  IT CARRIES THE   *
      * PAGE, SECTION AND LINE COUNTS THAT THE BURST PROCESS USES TO  *
      * SPLIT THE PRINT STREAM AND THAT THE MAILROOM RECONCILES ON.   *
      *****************************************************************
       01  WS-PRTCTL-RECORD.
           05  WS-PR-BAN               PIC X(13) VALUE SPACES.
           05  WS-PR-INVOICE           PIC X(14) VALUE SPACES.
           05  WS-PR-PERIOD            PIC 9(06) VALUE 0.
           05  WS-PR-PAGES             PIC 9(05) VALUE 0.
           05  WS-PR-SECTIONS          PIC 9(05) VALUE 0.
           05  WS-PR-LINES             PIC 9(07) VALUE 0.
           05  WS-PR-MEDIA             PIC X(01) VALUE SPACES.
           05  WS-PR-TOTAL             PIC S9(13)V9(02) VALUE 0.
           05  WS-PR-FILLER            PIC X(24) VALUE SPACES.
       01  WS-PRTCTL-KEYED REDEFINES WS-PRTCTL-RECORD.
           05  WS-PRK-KEY              PIC X(27).
           05  WS-PRK-REST             PIC X(63).
      *****************************************************************
      * THE SECTION NAME TABLE.  THE PRINTED SECTION HEADING COMES FROM*
      * HERE AND MUST MATCH THE FILED TARIFF WORDING EXACTLY.         *
      *****************************************************************
       01  WS-SECTION-NAME-TABLE.
           05  FILLER PIC X(32) VALUE
               'U1SWITCHED ACCESS USAGE         '.
           05  FILLER PIC X(32) VALUE
               'U2ORIGINATING ACCESS USAGE      '.
           05  FILLER PIC X(32) VALUE
               'U3SPECIAL ACCESS USAGE          '.
           05  FILLER PIC X(32) VALUE
               'C1RECURRING ACCESS CHARGES      '.
           05  FILLER PIC X(32) VALUE
               'C2NON RECURRING CHARGES         '.
           05  FILLER PIC X(32) VALUE
               'C3UNBUNDLED NETWORK ELEMENTS    '.
           05  FILLER PIC X(32) VALUE
               'C4INTERCONNECTION CHARGES       '.
           05  FILLER PIC X(32) VALUE
               'S1RECIPROCAL COMPENSATION       '.
           05  FILLER PIC X(32) VALUE
               'S2MEET POINT BILLING            '.
           05  FILLER PIC X(32) VALUE
               'A1ADJUSTMENTS AND RESTATEMENT   '.
           05  FILLER PIC X(32) VALUE
               'T1TAXES AND SURCHARGES          '.
           05  FILLER PIC X(32) VALUE
               'Z1OTHER CHARGES                 '.
       01  WS-SECTION-NAME-R REDEFINES WS-SECTION-NAME-TABLE.
           05  WS-SN-ENTRY OCCURS 12 TIMES INDEXED BY WS-SN-X.
               10  WS-SN-CODE          PIC X(02).
               10  WS-SN-NAME          PIC X(30).
       01  WS-SECTION-WORK.
           05  WS-SW-NAME              PIC X(30) VALUE SPACES.
           05  WS-SW-FOUND-SW          PIC X(01) VALUE 'N'.
               88  WS-SW-FOUND         VALUE 'Y'.
      *****************************************************************
      * THE HEADER TABLE.  HEADERS ARE HELD IN STORAGE AND MATCHED TO *
      * THE DETAIL AS IT IS FORMATTED.                                *
      *****************************************************************
       01  WS-HDR-TABLE.
           05  WS-HT-ENTRY OCCURS 2000 TIMES INDEXED BY WS-HT-X.
               10  WS-HT-BAN           PIC X(13).
               10  WS-HT-IMAGE         PIC X(400).
       01  WS-HDR-CTL.
           05  WS-HT-USED              PIC S9(05) COMP-3 VALUE 0.
           05  WS-HT-MAX               PIC S9(05) COMP-3 VALUE 2000.
           05  WS-HT-HIT               PIC S9(05) COMP-3 VALUE 0.
      *****************************************************************
      * LINE BUILD WORK.                                              *
      *****************************************************************
       01  WS-LINE-WORK.
           05  WS-LW-DESC              PIC X(60) VALUE SPACES.
           05  WS-LW-QTY               PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-LW-AMOUNT            PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-LW-ELEM-CNT          PIC 9(03) VALUE 0.
           05  WS-LW-SUPPRESS-SW       PIC X(01) VALUE 'N'.
       01  WS-RUN-TOTALS.
           05  WS-RT-INVOICES          PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-PAGES             PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-SECTIONS          PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-LINES             PIC S9(11) COMP-3 VALUE 0.
           05  WS-RT-ORPHANS           PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-HELD-SKIPPED      PIC S9(09) COMP-3 VALUE 0.
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
                       BHDR-IN-FILE
                       PARM-FILE
           OPEN OUTPUT PRINT-STREAM
                       PRTCTL-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 6011 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-BDTLIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 6012 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-BHDRIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 6013 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-PRTOUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 6014 TO WS-AB-CODE
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
           PERFORM P5000-LOAD-HEADERS THRU P5000-EXIT.
           MOVE WS-PE-LINES-PAGE TO WS-PG-MAX-LINES.
           PERFORM P6000-HEADING THRU P6000-EXIT.
           DISPLAY WS-PGM-NAME ' STARTED - RUN ' WS-RUN-ID.
           DISPLAY '  LINES PER PAGE' WS-PE-LINES-PAGE.
           DISPLAY '  MEDIA CODE    ' WS-PE-MEDIA.

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
      * LINES PER PAGE IS SUPPLIED BY THE SCHEDULER FROM THE FORM
      * DEFINITION IN USE ON THE NIGHT.  IT HAS NO DEFAULT.
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
           IF WS-PE-LINES-PAGE NOT NUMERIC
               MOVE 6021 TO WS-AB-CODE
               MOVE 'LINES PER PAGE NOT NUMERIC' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PE-LINES-PAGE < 20 OR WS-PE-LINES-PAGE > 99
               MOVE 6022 TO WS-AB-CODE
               MOVE 'LINES PER PAGE OUTSIDE 20 TO 99' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PE-MEDIA = SPACES
               MOVE 'P' TO WS-PE-MEDIA.
           IF WS-PE-BURST-SW NOT = 'N'
               MOVE 'Y' TO WS-PE-BURST-SW.

       P1200-EXIT.
           EXIT.

      *****************************************************************
      * S200-MAIN PROCESSING                                          *
      * ONE PASS OF THE SEQUENCED BILL DETAIL FILE PRODUCING THE PRINT*
      * STREAM.  THE FILE IS IN BAN AND SECTION ORDER, WHICH IS THE   *
      * ORDER THE BILL PRINTS IN.                                     *
      *****************************************************************
       S200-MAIN-PROCESS SECTION.

       P2000-PROCESS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ-DETAIL THRU P2100-EXIT.
           IF WS-DTL-EOF
               PERFORM P4000-CLOSE-INVOICE THRU P4000-EXIT
               MOVE 'Y' TO WS-EOF-SW
               GO TO P2000-EXIT.
           ADD 1 TO WS-READ-CNT.
           MOVE BD-BAN TO WS-RESTART-KEY.
           IF WS-INV-OPEN
               IF BD-BAN NOT = WS-PG-PRIOR-BAN
                   PERFORM P4000-CLOSE-INVOICE THRU P4000-EXIT.
           IF NOT WS-INV-OPEN
               PERFORM P3100-START-INVOICE THRU P3100-EXIT.
           IF NOT WS-INV-OPEN
               ADD 1 TO WS-CFWD-CNT
               GO TO P2000-EXIT.
           PERFORM P3200-BUILD-DETAIL THRU P3200-EXIT.
           PERFORM P3300-ASSIGN-CARRIAGE-CONTROL THRU P3300-EXIT.
           PERFORM P3400-WRITE-PRINT-LINE THRU P3400-EXIT.
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
               MOVE 6101 TO WS-AB-CODE
               MOVE 'BILL DETAIL READ ERROR' TO WS-AB-TEXT
               GO TO P9980-PRINT-FAILURE.

       P2100-EXIT.
           EXIT.

      *****************************************************************
      * S300-INVOICE AND PAGE FORMATTING                              *
      *****************************************************************
       S300-FORMAT SECTION.

       P3100-START-INVOICE.
      * OPEN A NEW INVOICE.  THE FIRST LINE OF AN INVOICE CARRIES A
      * SEVEN IN COLUMN ONE, WHICH THE BURST MACHINERY READS AS THE
      * START OF A NEW DOCUMENT AND THE PRINTER READS AS A SKIP TO
      * CHANNEL SEVEN.  THE INVOICE BREAK ALSO OPENS THE FIRST SECTION.
           MOVE 'P3100-START-INVOICE' TO WS-PARA-NAME.
           PERFORM P5100-FIND-HEADER THRU P5100-EXIT.
           IF NOT WS-HDR-MATCH
               ADD 1 TO WS-RT-ORPHANS
               MOVE BD-BAN TO WS-PG-PRIOR-BAN
               GO TO P3100-EXIT.
           IF BH-HELD
               ADD 1 TO WS-RT-HELD-SKIPPED
               MOVE BD-BAN TO WS-PG-PRIOR-BAN
               GO TO P3100-EXIT.
           IF BH-CANCELLED
               ADD 1 TO WS-RT-HELD-SKIPPED
               MOVE BD-BAN TO WS-PG-PRIOR-BAN
               GO TO P3100-EXIT.
           MOVE 'Y' TO WS-INV-OPEN-SW.
           MOVE BD-BAN TO WS-PG-PRIOR-BAN.
           MOVE SPACES TO WS-PG-PRIOR-SECTION.
           MOVE ZERO TO WS-PG-INV-PAGE WS-PG-LINE-CNT.
           ADD 1 TO WS-PG-INVOICE-CNT.
           ADD 1 TO WS-PG-SECTION-CNT.
           ADD 1 TO WS-RT-INVOICES.
           PERFORM P3600-INVOICE-HEAD THRU P3600-EXIT.

       P3100-EXIT.
           EXIT.

       P3200-BUILD-DETAIL.
      * BUILD THE BODY OF THE PRINT LINE FROM THE DETAIL RECORD.  THE
      * AMOUNT VIEW OF THE PRINT LINE IS USED - THE REDEFINE THAT
      * CARRIES THE EDITED QUANTITY, RATE AND VALUE FIELDS.
           MOVE 'P3200-BUILD-DETAIL' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE BD-DESCRIPTION TO WS-LW-DESC.
           MOVE BD-TOT-MINUTES TO WS-LW-QTY.
           MOVE BD-TOT-ROUNDED TO WS-LW-AMOUNT.
           MOVE BD-ELEM-CNT    TO WS-LW-ELEM-CNT.
           MOVE WS-LW-DESC     TO PC-AMT-DESC.
           MOVE WS-LW-QTY      TO PC-AMT-QTY.
           MOVE ZERO           TO PC-AMT-RATE.
           IF WS-LW-ELEM-CNT = 1
               SET BD-EX TO 1
               MOVE BD-EL-RATE (BD-EX) TO PC-AMT-RATE.
           MOVE WS-LW-AMOUNT   TO PC-AMT-VALUE.
           MOVE SPACES         TO PC-AMT-FILL.
           ADD WS-LW-AMOUNT    TO WS-RT-AMOUNT.
           ADD WS-LW-AMOUNT    TO WS-ACC-AMOUNT.

       P3200-EXIT.
           EXIT.

       P3300-ASSIGN-CARRIAGE-CONTROL.
      * SET THE CARRIAGE CONTROL FOR THIS DETAIL LINE.  A CHANGE OF
      * BILL SECTION PUTS A FOUR IN COLUMN ONE, WHICH SKIPS TO CHANNEL
      * FOUR ON THE PRINTER AND TELLS THE BURST PROCESS THAT A NEW BILL
      * SECTION HAS STARTED.  A LINE INSIDE THE SAME SECTION SINGLE
      * SPACES.  THE SECTION COUNTER IS WHAT THE PRINT CONTROL RECORD
      * CARRIES TO THE MAILROOM.
           MOVE 'P3300-ASSIGN-CARRIAGE-CONTROL' TO WS-PARA-NAME.
           MOVE '4' TO PC-CC.
           ADD 1 TO WS-PG-SECTION-CNT.
           IF BD-SECTION = WS-PG-PRIOR-SECTION
               MOVE ' ' TO PC-CC
               SUBTRACT 1 FROM WS-PG-SECTION-CNT.
           MOVE BD-SECTION TO WS-PG-PRIOR-SECTION.
           IF PC-NEW-SECTION
               PERFORM P3500-SECTION-HEAD THRU P3500-EXIT.
           IF WS-PG-LINE-CNT > WS-PE-LINES-PAGE
               MOVE '1' TO PC-CC
               PERFORM P3700-PAGE-HEAD THRU P3700-EXIT.

       P3300-EXIT.
           EXIT.

       P3400-WRITE-PRINT-LINE.
           MOVE 'P3400-WRITE-PRINT-LINE' TO WS-PARA-NAME.
           WRITE PRINT-LINE FROM CABS-PRINT-LINE.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 6102 TO WS-AB-CODE
               MOVE 'PRINT STREAM WRITE FAILED' TO WS-AB-TEXT
               GO TO P9980-PRINT-FAILURE.
           ADD 1 TO WS-WRITE-CNT.
           ADD 1 TO WS-PG-LINE-CNT.
           ADD 1 TO WS-PG-LINE-WRITTEN.
           ADD 1 TO WS-RT-LINES.

       P3400-EXIT.
           EXIT.

       P3500-SECTION-HEAD.
      * PRINT THE SECTION HEADING.  THE HEADING LINE ITSELF CARRIES THE
      * SECTION BREAK CHARACTER; THE DETAIL LINE THAT FOLLOWS IT SINGLE
      * SPACES UNDERNEATH IT.
           MOVE 'P3500-SECTION-HEAD' TO WS-PARA-NAME.
           PERFORM P5200-SECTION-NAME THRU P5200-EXIT.
           MOVE SPACES TO WS-LW-DESC.
           MOVE WS-SW-NAME TO WS-LW-DESC.
           ADD 1 TO WS-RT-SECTIONS.

       P3500-EXIT.
           EXIT.

       P3600-INVOICE-HEAD.
      * THE FIRST LINE OF THE INVOICE.  SEVEN IN COLUMN ONE.  THE LINE
      * CARRIES THE INVOICE NUMBER AND THE CARRIER NAME AND IS WHAT THE
      * INSERTER READS THROUGH THE ENVELOPE WINDOW.
           MOVE 'P3600-INVOICE-HEAD' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '7' TO PC-CC.
           MOVE BH-INVOICE-NBR     TO PC-COL-001-020.
           MOVE BH-BAN             TO PC-COL-021-060.
           MOVE BH-OCN             TO PC-COL-061-090.
           MOVE BH-TOTAL-DUE       TO WS-ED-MONEY.
           MOVE WS-ED-MONEY        TO PC-COL-091-132.
           WRITE PRINT-LINE FROM CABS-PRINT-LINE.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 6103 TO WS-AB-CODE
               MOVE 'INVOICE HEAD WRITE FAILED' TO WS-AB-TEXT
               GO TO P9980-PRINT-FAILURE.
           ADD 1 TO WS-PG-LINE-CNT.
           ADD 1 TO WS-PG-LINE-WRITTEN.
           ADD 1 TO WS-PG-INV-PAGE.
           ADD 1 TO WS-PG-PAGE-CNT.
           ADD 1 TO WS-RT-PAGES.

       P3700-PAGE-HEAD.
      * A NEW PAGE INSIDE THE SAME INVOICE.  THE PAGE NUMBER RESTARTS
      * AT ONE FOR EACH INVOICE BECAUSE THE CARRIER READS PAGE ONE OF
      * SIX, NOT PAGE FOUR HUNDRED OF THE RUN.
           MOVE 'P3700-PAGE-HEAD' TO WS-PARA-NAME.
           MOVE ZERO TO WS-PG-LINE-CNT.
           ADD 1 TO WS-PG-INV-PAGE.
           ADD 1 TO WS-PG-PAGE-CNT.
           ADD 1 TO WS-RT-PAGES.

       P3700-EXIT.
           EXIT.

       P3600-EXIT.
           EXIT.

      *****************************************************************
      * S400-INVOICE BREAK                                            *
      *****************************************************************
       S400-BREAK SECTION.

       P4000-CLOSE-INVOICE.
      * CLOSE THE OPEN INVOICE AND WRITE ITS PRINT CONTROL RECORD.  THE
      * COUNTS ON THAT RECORD ARE WHAT THE BURST PROCESS AND THE
      * MAILROOM WORK FROM.
           MOVE 'P4000-CLOSE-INVOICE' TO WS-PARA-NAME.
           IF NOT WS-INV-OPEN
               GO TO P4000-EXIT.
           MOVE SPACES TO WS-PRTCTL-RECORD.
           MOVE BH-BAN             TO WS-PR-BAN.
           MOVE BH-INVOICE-NBR     TO WS-PR-INVOICE.
           MOVE BH-BILL-PERIOD     TO WS-PR-PERIOD.
           MOVE WS-PG-INV-PAGE     TO WS-PR-PAGES.
           MOVE WS-PG-SECTION-CNT  TO WS-PR-SECTIONS.
           MOVE WS-PG-LINE-WRITTEN TO WS-PR-LINES.
           MOVE WS-PE-MEDIA        TO WS-PR-MEDIA.
           MOVE BH-TOTAL-DUE       TO WS-PR-TOTAL.
           WRITE PRTCTL-RECORD FROM WS-PRTCTL-RECORD.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 6104 TO WS-AB-CODE
               MOVE 'PRINT CONTROL WRITE FAILED' TO WS-AB-TEXT
               GO TO P9980-PRINT-FAILURE.
           PERFORM P5300-REGISTER-LINE THRU P5300-EXIT.
           MOVE ZERO TO WS-PG-SECTION-CNT WS-PG-LINE-WRITTEN.
           MOVE 'N' TO WS-INV-OPEN-SW.

       P4000-EXIT.
           EXIT.

      *****************************************************************
      * S500-SUPPORT                                                  *
      *****************************************************************
       S500-SUPPORT SECTION.

       P5000-LOAD-HEADERS.
           MOVE 'P5000-LOAD-HEADERS' TO WS-PARA-NAME.
           MOVE ZERO TO WS-HT-USED.
           PERFORM P5010-READ-HDR THRU P5010-EXIT
               UNTIL WS-HDR-EOF.
           DISPLAY 'HEADERS LOADED ' WS-HT-USED.

       P5000-EXIT.
           EXIT.

       P5010-READ-HDR.
           READ BHDR-IN-FILE INTO CABS-BILL-HEADER
               AT END
                   MOVE 'Y' TO WS-HDR-EOF-SW
                   GO TO P5010-EXIT.
           IF WS-HT-USED NOT < WS-HT-MAX
               MOVE 6105 TO WS-AB-CODE
               MOVE 'HEADER TABLE FULL' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-HT-USED.
           SET WS-HT-X TO WS-HT-USED.
           MOVE BH-BAN           TO WS-HT-BAN (WS-HT-X).
           MOVE CABS-BILL-HEADER TO WS-HT-IMAGE (WS-HT-X).

       P5010-EXIT.
           EXIT.

       P5100-FIND-HEADER.
           MOVE 'P5100-FIND-HEADER' TO WS-PARA-NAME.
           MOVE 'N' TO WS-HDR-MATCH-SW.
           MOVE ZERO TO WS-HT-HIT.
           PERFORM P5110-MATCH-HDR THRU P5110-EXIT
               VARYING WS-HT-X FROM 1 BY 1
               UNTIL WS-HT-X > WS-HT-USED OR WS-HDR-MATCH.
           IF WS-HDR-MATCH
               SET WS-HT-X TO WS-HT-HIT
               MOVE WS-HT-IMAGE (WS-HT-X) TO CABS-BILL-HEADER.

       P5100-EXIT.
           EXIT.

       P5110-MATCH-HDR.
           IF WS-HT-BAN (WS-HT-X) = BD-BAN
               SET WS-SUB1 TO WS-HT-X
               MOVE WS-SUB1 TO WS-HT-HIT
               MOVE 'Y' TO WS-HDR-MATCH-SW.

       P5110-EXIT.
           EXIT.

       P5200-SECTION-NAME.
           MOVE 'P5200-SECTION-NAME' TO WS-PARA-NAME.
           MOVE 'N' TO WS-SW-FOUND-SW.
           MOVE SPACES TO WS-SW-NAME.
           PERFORM P5210-MATCH-SECTION THRU P5210-EXIT
               VARYING WS-SN-X FROM 1 BY 1
               UNTIL WS-SN-X > 12 OR WS-SW-FOUND.
           IF NOT WS-SW-FOUND
               MOVE 'OTHER CHARGES' TO WS-SW-NAME.

       P5200-EXIT.
           EXIT.

       P5210-MATCH-SECTION.
           IF WS-SN-CODE (WS-SN-X) = BD-SECTION
               MOVE WS-SN-NAME (WS-SN-X) TO WS-SW-NAME
               MOVE 'Y' TO WS-SW-FOUND-SW.

       P5210-EXIT.
           EXIT.

       P5300-REGISTER-LINE.
           MOVE 'P5300-REGISTER-LINE' TO WS-PARA-NAME.
           IF WS-PAGE-LINES > WS-MAX-LINES
               PERFORM P6000-HEADING THRU P6000-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-PR-BAN      TO PC-COL-001-020.
           MOVE WS-PR-INVOICE  TO PC-COL-021-060.
           MOVE WS-PR-PAGES    TO WS-ED-COUNT.
           MOVE WS-ED-COUNT    TO PC-COL-061-090.
           MOVE WS-PR-SECTIONS TO WS-ED-COUNT.
           MOVE WS-ED-COUNT    TO PC-COL-091-132.
           WRITE PRT-RECORD FROM CABS-PRINT-LINE.
           ADD 1 TO WS-PAGE-LINES.

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
           MOVE 'CABFMT01  INVOICE PAGE AND SECTION FORMAT REGISTER'
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
           MOVE 'BAN                 INVOICE NUMBER       PAGES   SEC'
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
           MOVE 500                    TO CT-STEP-SEQ.
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
           DISPLAY 'INVOICES FORMATTED' WS-RT-INVOICES.
           DISPLAY 'PAGES PRODUCED    ' WS-RT-PAGES.
           DISPLAY 'SECTION BREAKS    ' WS-RT-SECTIONS.
           DISPLAY 'PRINT LINES       ' WS-RT-LINES.
           DISPLAY 'ORPHAN DETAIL     ' WS-RT-ORPHANS.
           DISPLAY 'HELD NOT PRINTED  ' WS-RT-HELD-SKIPPED.
           DISPLAY 'VALUE FORMATTED   ' WS-RT-AMOUNT.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE BILL-DTL-IN
                 BHDR-IN-FILE
                 PRINT-STREAM
                 PRTCTL-FILE
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

       P9980-PRINT-FAILURE.
      * THE PRINT STREAM OR THE PRINT CONTROL FILE COULD NOT BE
      * WRITTEN.  THERE IS NO WAY TO CARRY ON - A PARTIAL PRINT STREAM
      * WOULD BE BURST AND MAILED.  THE STEP IS FAILED WITH THE FILE
      * STATUS ON THE JOB LOG SO THAT OPERATIONS CAN SEE WHICH DD
      * FAILED BEFORE THEY RESTART.
      * REACHED BY GO TO FROM P2100, P3400, P3600 AND P4000.
      * FAILURE PATH STRUCTURED PER CABS-STD-047.
           DISPLAY '*** PRINT FAILURE IN ' WS-PGM-NAME.
           DISPLAY '*** OUTPUT STATUS ' WS-FS-OUTPUT.
           DISPLAY '*** CONTROL STATUS ' WS-FS-SUSPENSE.
           DISPLAY '*** INVOICES FORMATTED ' WS-RT-INVOICES.
           DISPLAY '*** LAST BAN ' WS-PG-PRIOR-BAN.
           MOVE WS-PG-PRIOR-BAN TO WS-RESTART-KEY.
           PERFORM P9500-ABEND THRU P9500-EXIT.

       P9980-EXIT.
           EXIT.
