      *****************************************************************
      * CABFMT06 - EDI 811 FLAT FILE PRODUCTION                       *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               BDTLIN  TELCABS.CABS.BILLDTL.SEQ(0)       CABSBILL*
      *               BHDRIN  TELCABS.CABS.BILLHDR.FIN(0)       CABSBHDR*
      *               SYSIN   INSTREAM CONTROL CARD             (LOCAL)*
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               EDIOUT  TELCABS.CABS.EDI811(+1)           (LOCAL)*
      *               REPORT  SYSOUT PRINT                      CABSPRNT*
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - THE INTERCHANGE IS REBUILT WHOLE   *
      * REVISION HISTORY                                              *
      *   V1.00  1994-04-25  D.OKONKWO    INITIAL RELEASE - 811 VERSION 3020*
      *                      FOR THE FIRST TWO TRADING PARTNERS       *
      *   V1.04  1997-01-13  M.J.FERRARO  SEPARATORS MOVED TO THE CONTROL CARD*
      *   V2.00  2002-08-19  P.NAIR       DESCRIPTION NOW SCANNED FOR THE*
      *                      SEPARATOR BEFORE IT IS PLACED IN A       *
      *                      SEGMENT - ONE PARTNER REJECTED A         *
      *                      WHOLE INTERCHANGE OVER A COMMA           *
      *   V2.06  2016-03-29  G.PRZYBYLSKI SEGMENT COUNT PER INVOICE ADDED TO*
      *                      THE CTT SEGMENT                          *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABFMT06.
       AUTHOR. TELCABS APPLICATIONS - BILL PRINT TEAM.
      *****************************************************************
      * PRODUCES THE 811 CONSOLIDATED SERVICE INVOICE AS A FLAT FILE. *
      * THE ESTATE HAS NO TRANSLATOR - THE SEGMENTS ARE ASSEMBLED HERE*
      * AND THE NETWORK GATEWAY WRAPS THEM.                           *
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
           SELECT EDI-OUT-FILE ASSIGN TO UT-S-EDIOUT
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
      * BHDRIN - THE FINAL NUMBERED INVOICE HEADER.                   *
      *****************************************************************
       FD  BHDR-IN-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 400 CHARACTERS.
       01  BHDR-IN-REC                      PIC X(400).
      *****************************************************************
      * EDIOUT - ONE EDI SEGMENT PER 200 BYTE RECORD.                 *
      *****************************************************************
       FD  EDI-OUT-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       01  EDI-RECORD                       PIC X(200).
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
           05  WS-PGM-NAME             PIC X(08) VALUE 'CABFMT06'.
           05  WS-PGM-VERSION          PIC X(05) VALUE 'V2.06'.
           05  WS-PGM-APPL             PIC X(04) VALUE 'CABS'.
           05  WS-PGM-COMPILE          PIC X(08) VALUE '20160329'.
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
           05  WS-PE-EDI-VERSION       PIC X(06).
           05  WS-PE-SENDER-ID         PIC X(15).
           05  WS-PE-SEG-TERM          PIC X(01).
           05  WS-PE-ELEM-SEP          PIC X(01).
           05  WS-PE-FILLER            PIC X(12).
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
      *****************************************************************
      * THE 811 CONSOLIDATED SERVICE INVOICE.  THE ESTATE DOES NOT USE*
      * A TRANSLATOR - THE SEGMENTS ARE BUILT HERE AS FIXED 200 BYTE  *
      * RECORDS AND THE VALUE ADDED NETWORK GATEWAY WRAPS THEM.  THE  *
      * SEGMENT AND ELEMENT SEPARATORS ARRIVE ON THE CONTROL CARD     *
      * BECAUSE TWO TRADING PARTNERS USE DIFFERENT ONES.              *
      * THE PRINT LINE IS AGREED WITH THE MAILROOM SPECIFICATION.     *
      *****************************************************************
       01  WS-EDI-RECORD               PIC X(200) VALUE SPACES.
       01  WS-EDI-RECORD-R REDEFINES WS-EDI-RECORD.
           05  WS-ER-SEG-ID            PIC X(03).
           05  WS-ER-BODY              PIC X(197).
       01  WS-EDI-RECORD-C REDEFINES WS-EDI-RECORD.
           05  WS-EC-CHAR OCCURS 200 TIMES PIC X(01).
       01  WS-EDI-PIECES.
           05  WS-EP-SEG-ID            PIC X(03) VALUE SPACES.
           05  WS-EP-QUAL              PIC X(02) VALUE SPACES.
           05  WS-EP-REF               PIC X(20) VALUE SPACES.
           05  WS-EP-AMOUNT            PIC X(18) VALUE SPACES.
           05  WS-EP-QTY               PIC X(18) VALUE SPACES.
           05  WS-EP-DATE              PIC X(08) VALUE SPACES.
           05  WS-EP-DESC              PIC X(60) VALUE SPACES.
       01  WS-EDI-CTL.
           05  WS-EK-PTR               PIC 9(03) VALUE 1.
           05  WS-EK-LEN               PIC 9(03) VALUE 0.
           05  WS-EK-SEG-CNT           PIC S9(09) COMP-3 VALUE 0.
           05  WS-EK-INV-SEG           PIC S9(07) COMP-3 VALUE 0.
           05  WS-EK-SEPARATORS        PIC 9(03) VALUE 0.
           05  WS-EK-BAD-CHARS         PIC 9(03) VALUE 0.
      *****************************************************************
      * UNSTRING WORK.  THE DESCRIPTION CARRIED ON THE BILL DETAIL IS *
      * A FREE TEXT FIELD AND HAS TO BE BROKEN INTO EDI ELEMENTS AT   *
      * THE BLANKS.  A DESCRIPTION THAT CONTAINS THE ELEMENT SEPARATOR*
      * WOULD CORRUPT THE SEGMENT, SO IT IS SCANNED FIRST.            *
      * EDITING RULES ARE HELD WITH THE BILL FORMAT SPECIFICATION.    *
      *****************************************************************
       01  WS-UNSTRING-WORK.
           05  WS-UW-SOURCE            PIC X(60) VALUE SPACES.
           05  WS-UW-PART1             PIC X(20) VALUE SPACES.
           05  WS-UW-PART2             PIC X(20) VALUE SPACES.
           05  WS-UW-PART3             PIC X(20) VALUE SPACES.
           05  WS-UW-COUNT             PIC 9(02) VALUE 0.
           05  WS-UW-PTR               PIC 9(03) VALUE 1.
           05  WS-UW-DELIM             PIC X(01) VALUE SPACES.
       01  WS-EDIT-EDI.
           05  WS-EE-AMOUNT            PIC S9(13)V9(02) VALUE 0.
           05  WS-EE-QTY               PIC S9(13)V9(02) VALUE 0.
           05  WS-EE-AMT-TEXT          PIC X(18) VALUE SPACES.
       01  WS-HDR-TABLE.
           05  WS-HT-ENTRY OCCURS 2000 TIMES INDEXED BY WS-HT-X.
               10  WS-HT-BAN           PIC X(13).
               10  WS-HT-IMAGE         PIC X(400).
       01  WS-HDR-CTL.
           05  WS-HT-USED              PIC S9(05) COMP-3 VALUE 0.
           05  WS-HT-MAX               PIC S9(05) COMP-3 VALUE 2000.
           05  WS-HT-HIT               PIC S9(05) COMP-3 VALUE 0.
           05  WS-HT-FOUND-SW          PIC X(01) VALUE 'N'.
               88  WS-HT-FOUND         VALUE 'Y'.
       01  WS-RUN-TOTALS.
           05  WS-RT-INVOICES          PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-SEGMENTS          PIC S9(11) COMP-3 VALUE 0.
           05  WS-RT-DETAIL-SEG        PIC S9(11) COMP-3 VALUE 0.
           05  WS-RT-BAD-DESC          PIC S9(09) COMP-3 VALUE 0.
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
           OPEN OUTPUT EDI-OUT-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 6611 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-BDTLIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 6612 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-BHDRIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 6613 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-EDIOUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 6614 TO WS-AB-CODE
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
           PERFORM P5100-LOAD-HEADERS THRU P5100-EXIT.
           PERFORM P3000-HEADER-SEGMENT THRU P3000-EXIT.
           PERFORM P6000-HEADING THRU P6000-EXIT.
           DISPLAY WS-PGM-NAME ' STARTED - RUN ' WS-RUN-ID.
           DISPLAY '  EDI VERSION   ' WS-PE-EDI-VERSION.
           DISPLAY '  SENDER ID     ' WS-PE-SENDER-ID.

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
      * THE EDI VERSION AND THE SENDER ID ARRIVE FROM THE SCHEDULER
      * BECAUSE THEY DIFFER BY TRADING PARTNER AND CHANGE WHENEVER
      * A PARTNER MIGRATES.  NEITHER HAS A DEFAULT.
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
           IF WS-PE-EDI-VERSION = SPACES
               MOVE 6621 TO WS-AB-CODE
               MOVE 'EDI VERSION NOT SUPPLIED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PE-SENDER-ID = SPACES
               MOVE 6622 TO WS-AB-CODE
               MOVE 'EDI SENDER ID NOT SUPPLIED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PE-SEG-TERM = SPACE
               MOVE '~' TO WS-PE-SEG-TERM.
           IF WS-PE-ELEM-SEP = SPACE
               MOVE '*' TO WS-PE-ELEM-SEP.

       P1200-EXIT.
           EXIT.

      *****************************************************************
      * S200-MAIN PROCESSING                                          *
      *****************************************************************
       S200-MAIN-PROCESS SECTION.

       P2000-PROCESS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ-DETAIL THRU P2100-EXIT.
           IF WS-DTL-EOF
               PERFORM P3900-CLOSE-INVOICE THRU P3900-EXIT
               PERFORM P3950-TRAILER THRU P3950-EXIT
               MOVE 'Y' TO WS-EOF-SW
               GO TO P2000-EXIT.
           ADD 1 TO WS-READ-CNT.
           MOVE BD-BAN TO WS-RESTART-KEY.
           IF WS-INV-OPEN
               IF BD-BAN NOT = WS-HT-BAN (WS-HT-X)
                   PERFORM P3900-CLOSE-INVOICE THRU P3900-EXIT.
           IF NOT WS-INV-OPEN
               PERFORM P3100-OPEN-INVOICE THRU P3100-EXIT.
           IF NOT WS-INV-OPEN
               ADD 1 TO WS-CFWD-CNT
               GO TO P2000-EXIT.
           PERFORM P4000-DETAIL-SEGMENT THRU P4000-EXIT.
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
               MOVE 6601 TO WS-AB-CODE
               MOVE 'BILL DETAIL READ ERROR' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.

       P2100-EXIT.
           EXIT.

      *****************************************************************
      * S300-ENVELOPE AND INVOICE SEGMENTS                            *
      *****************************************************************
       S300-SEGMENTS SECTION.

       P3000-HEADER-SEGMENT.
      * THE INTERCHANGE HEADER.  ONE PER FILE.  THE SENDER ID AND THE
      * VERSION ARRIVE ON THE CONTROL CARD - THEY CHANGE WHENEVER A
      * TRADING PARTNER MIGRATES TO A NEW RELEASE.
           MOVE 'P3000-HEADER-SEGMENT' TO WS-PARA-NAME.
           MOVE SPACES TO WS-EDI-RECORD.
           MOVE 1 TO WS-EK-PTR.
           STRING 'ISA'              DELIMITED BY SIZE
                  WS-PE-ELEM-SEP     DELIMITED BY SIZE
                  WS-PE-SENDER-ID    DELIMITED BY SIZE
                  WS-PE-ELEM-SEP     DELIMITED BY SIZE
                  WS-PE-EDI-VERSION  DELIMITED BY SIZE
                  WS-PE-ELEM-SEP     DELIMITED BY SIZE
                  WS-RUN-ID          DELIMITED BY SIZE
                  WS-PE-SEG-TERM     DELIMITED BY SIZE
                  INTO WS-EDI-RECORD
                  WITH POINTER WS-EK-PTR.
           PERFORM P5000-WRITE-SEGMENT THRU P5000-EXIT.

       P3000-EXIT.
           EXIT.

       P3100-OPEN-INVOICE.
      * THE BIG SEGMENT - BEGINNING OF INVOICE.  CARRIES THE INVOICE
      * NUMBER AND THE BILL DATE.
           MOVE 'P3100-OPEN-INVOICE' TO WS-PARA-NAME.
           PERFORM P6000-FIND-HEADER THRU P6000-EXIT.
           IF NOT WS-HT-FOUND
               GO TO P3100-EXIT.
           IF NOT BH-FINAL
               GO TO P3100-EXIT.
           MOVE 'Y' TO WS-INV-OPEN-SW.
           MOVE ZERO TO WS-EK-INV-SEG.
           ADD 1 TO WS-RT-INVOICES.
           MOVE SPACES TO WS-EDI-RECORD.
           MOVE 1 TO WS-EK-PTR.
           MOVE BH-BILL-YYDDD TO WS-EP-DATE.
           STRING 'BIG'              DELIMITED BY SIZE
                  WS-PE-ELEM-SEP     DELIMITED BY SIZE
                  WS-EP-DATE         DELIMITED BY SIZE
                  WS-PE-ELEM-SEP     DELIMITED BY SIZE
                  BH-INVOICE-NBR     DELIMITED BY SIZE
                  WS-PE-ELEM-SEP     DELIMITED BY SIZE
                  BH-BAN             DELIMITED BY SIZE
                  WS-PE-SEG-TERM     DELIMITED BY SIZE
                  INTO WS-EDI-RECORD
                  WITH POINTER WS-EK-PTR.
           PERFORM P5000-WRITE-SEGMENT THRU P5000-EXIT.
           PERFORM P3200-PARTY-SEGMENT THRU P3200-EXIT.

       P3100-EXIT.
           EXIT.

       P3200-PARTY-SEGMENT.
           MOVE 'P3200-PARTY-SEGMENT' TO WS-PARA-NAME.
           MOVE SPACES TO WS-EDI-RECORD.
           MOVE 1 TO WS-EK-PTR.
           STRING 'N1 '              DELIMITED BY SIZE
                  WS-PE-ELEM-SEP     DELIMITED BY SIZE
                  'BT'               DELIMITED BY SIZE
                  WS-PE-ELEM-SEP     DELIMITED BY SIZE
                  BH-OCN             DELIMITED BY SIZE
                  WS-PE-SEG-TERM     DELIMITED BY SIZE
                  INTO WS-EDI-RECORD
                  WITH POINTER WS-EK-PTR.
           PERFORM P5000-WRITE-SEGMENT THRU P5000-EXIT.

       P3200-EXIT.
           EXIT.

       P3900-CLOSE-INVOICE.
      * THE TDS AND CTT SEGMENTS - TOTAL DUE AND THE SEGMENT COUNT FOR
      * THIS INVOICE.
           MOVE 'P3900-CLOSE-INVOICE' TO WS-PARA-NAME.
           IF NOT WS-INV-OPEN
               GO TO P3900-EXIT.
           MOVE BH-TOTAL-DUE TO WS-EE-AMOUNT.
           MOVE WS-EE-AMOUNT TO WS-EE-AMT-TEXT.
           MOVE SPACES TO WS-EDI-RECORD.
           MOVE 1 TO WS-EK-PTR.
           STRING 'TDS'              DELIMITED BY SIZE
                  WS-PE-ELEM-SEP     DELIMITED BY SIZE
                  WS-EE-AMT-TEXT     DELIMITED BY SIZE
                  WS-PE-SEG-TERM     DELIMITED BY SIZE
                  INTO WS-EDI-RECORD
                  WITH POINTER WS-EK-PTR.
           PERFORM P5000-WRITE-SEGMENT THRU P5000-EXIT.
           MOVE SPACES TO WS-EDI-RECORD.
           MOVE 1 TO WS-EK-PTR.
           MOVE WS-EK-INV-SEG TO WS-EP-QTY.
           STRING 'CTT'              DELIMITED BY SIZE
                  WS-PE-ELEM-SEP     DELIMITED BY SIZE
                  WS-EP-QTY          DELIMITED BY SIZE
                  WS-PE-SEG-TERM     DELIMITED BY SIZE
                  INTO WS-EDI-RECORD
                  WITH POINTER WS-EK-PTR.
           PERFORM P5000-WRITE-SEGMENT THRU P5000-EXIT.
           ADD BH-TOTAL-DUE TO WS-RT-AMOUNT.
           ADD BH-TOTAL-DUE TO WS-ACC-AMOUNT.
           MOVE 'N' TO WS-INV-OPEN-SW.

       P3900-EXIT.
           EXIT.

       P3950-TRAILER.
           MOVE 'P3950-TRAILER' TO WS-PARA-NAME.
           MOVE SPACES TO WS-EDI-RECORD.
           MOVE 1 TO WS-EK-PTR.
           MOVE WS-EK-SEG-CNT TO WS-EP-QTY.
           STRING 'IEA'              DELIMITED BY SIZE
                  WS-PE-ELEM-SEP     DELIMITED BY SIZE
                  WS-EP-QTY          DELIMITED BY SIZE
                  WS-PE-SEG-TERM     DELIMITED BY SIZE
                  INTO WS-EDI-RECORD
                  WITH POINTER WS-EK-PTR.
           PERFORM P5000-WRITE-SEGMENT THRU P5000-EXIT.

       P3950-EXIT.
           EXIT.

      *****************************************************************
      * S400-DETAIL SEGMENTS                                          *
      *****************************************************************
       S400-DETAIL SECTION.

       P4000-DETAIL-SEGMENT.
      * THE IT1 SEGMENT - ONE PER BILL DETAIL LINE.  THE DESCRIPTION
      * HAS TO BE BROKEN UP AND CLEANED BEFORE IT CAN GO INTO AN EDI
      * ELEMENT.
           MOVE 'P4000-DETAIL-SEGMENT' TO WS-PARA-NAME.
           MOVE BD-DESCRIPTION TO WS-UW-SOURCE.
           PERFORM P4400-SCAN-DELIMITERS THRU P4400-EXIT.
           PERFORM P4200-SPLIT-DESCRIPTION THRU P4200-EXIT.
           MOVE BD-TOT-ROUNDED TO WS-EE-AMOUNT.
           MOVE WS-EE-AMOUNT   TO WS-EE-AMT-TEXT.
           MOVE BD-TOT-MINUTES TO WS-EE-QTY.
           MOVE WS-EE-QTY      TO WS-EP-QTY.
           MOVE SPACES TO WS-EDI-RECORD.
           MOVE 1 TO WS-EK-PTR.
           STRING 'IT1'              DELIMITED BY SIZE
                  WS-PE-ELEM-SEP     DELIMITED BY SIZE
                  BD-SECTION         DELIMITED BY SIZE
                  WS-PE-ELEM-SEP     DELIMITED BY SIZE
                  WS-EP-QTY          DELIMITED BY SIZE
                  WS-PE-ELEM-SEP     DELIMITED BY SIZE
                  WS-EE-AMT-TEXT     DELIMITED BY SIZE
                  WS-PE-ELEM-SEP     DELIMITED BY SIZE
                  WS-UW-PART1        DELIMITED BY '  '
                  WS-PE-SEG-TERM     DELIMITED BY SIZE
                  INTO WS-EDI-RECORD
                  WITH POINTER WS-EK-PTR
               ON OVERFLOW
                  ADD 1 TO WS-RT-BAD-DESC.
           PERFORM P5000-WRITE-SEGMENT THRU P5000-EXIT.
           ADD 1 TO WS-RT-DETAIL-SEG.

       P4000-EXIT.
           EXIT.

       P4200-SPLIT-DESCRIPTION.
      * BREAK THE DESCRIPTION AT THE FIRST TWO BLANK RUNS.  THREE
      * PIECES ARE TAKEN; ANYTHING BEYOND THE THIRD IS DROPPED, WHICH
      * IS WHY THE ELEMENT NAME MUST BE FIRST IN THE DESCRIPTION.
           MOVE 'P4200-SPLIT-DESCRIPTION' TO WS-PARA-NAME.
           MOVE SPACES TO WS-UW-PART1 WS-UW-PART2 WS-UW-PART3.
           MOVE ZERO TO WS-UW-COUNT.
           MOVE 1 TO WS-UW-PTR.
           UNSTRING WS-UW-SOURCE
               DELIMITED BY '  '
               INTO WS-UW-PART1
                    WS-UW-PART2
                    WS-UW-PART3
               WITH POINTER WS-UW-PTR
               TALLYING IN WS-UW-COUNT
               ON OVERFLOW
                   ADD 1 TO WS-RT-BAD-DESC.

       P4200-EXIT.
           EXIT.

       P4400-SCAN-DELIMITERS.
      * A DESCRIPTION THAT CONTAINS THE ELEMENT OR SEGMENT SEPARATOR
      * WOULD BREAK THE SEGMENT ON THE TRADING PARTNER SIDE.  ANY
      * OCCURRENCE IS COUNTED AND REPLACED WITH A BLANK.
           MOVE 'P4400-SCAN-DELIMITERS' TO WS-PARA-NAME.
           MOVE ZERO TO WS-EK-SEPARATORS WS-EK-BAD-CHARS.
           INSPECT WS-UW-SOURCE
               TALLYING WS-EK-SEPARATORS FOR ALL WS-PE-ELEM-SEP.
           INSPECT WS-UW-SOURCE
               TALLYING WS-EK-BAD-CHARS FOR ALL WS-PE-SEG-TERM.
           IF WS-EK-SEPARATORS > ZERO
               INSPECT WS-UW-SOURCE
                   REPLACING ALL WS-PE-ELEM-SEP BY SPACE
               ADD 1 TO WS-RT-BAD-DESC.
           IF WS-EK-BAD-CHARS > ZERO
               INSPECT WS-UW-SOURCE
                   REPLACING ALL WS-PE-SEG-TERM BY SPACE
               ADD 1 TO WS-RT-BAD-DESC.
           INSPECT WS-UW-SOURCE
               REPLACING ALL LOW-VALUE BY SPACE.

       P4400-EXIT.
           EXIT.

      *****************************************************************
      * S500-OUTPUT AND SUPPORT                                       *
      *****************************************************************
       S500-SUPPORT SECTION.

       P5000-WRITE-SEGMENT.
           MOVE 'P5000-WRITE-SEGMENT' TO WS-PARA-NAME.
           WRITE EDI-RECORD FROM WS-EDI-RECORD.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 6602 TO WS-AB-CODE
               MOVE 'EDI FILE WRITE FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-WRITE-CNT.
           ADD 1 TO WS-EK-SEG-CNT.
           ADD 1 TO WS-EK-INV-SEG.
           ADD 1 TO WS-RT-SEGMENTS.

       P5000-EXIT.
           EXIT.

       P5100-LOAD-HEADERS.
           MOVE 'P5100-LOAD-HEADERS' TO WS-PARA-NAME.
           MOVE ZERO TO WS-HT-USED.
           PERFORM P5110-READ-HDR THRU P5110-EXIT
               UNTIL WS-HDR-EOF.
           DISPLAY 'HEADERS LOADED ' WS-HT-USED.

       P5100-EXIT.
           EXIT.

       P5110-READ-HDR.
           READ BHDR-IN-FILE INTO CABS-BILL-HEADER
               AT END
                   MOVE 'Y' TO WS-HDR-EOF-SW
                   GO TO P5110-EXIT.
           IF WS-HT-USED NOT < WS-HT-MAX
               MOVE 6603 TO WS-AB-CODE
               MOVE 'HEADER TABLE FULL' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-HT-USED.
           SET WS-HT-X TO WS-HT-USED.
           MOVE BH-BAN           TO WS-HT-BAN (WS-HT-X).
           MOVE CABS-BILL-HEADER TO WS-HT-IMAGE (WS-HT-X).

       P5110-EXIT.
           EXIT.

       P6000-FIND-HEADER.
           MOVE 'P6000-FIND-HEADER' TO WS-PARA-NAME.
           MOVE 'N' TO WS-HT-FOUND-SW.
           MOVE ZERO TO WS-HT-HIT.
           PERFORM P6010-MATCH-HDR THRU P6010-EXIT
               VARYING WS-HT-X FROM 1 BY 1
               UNTIL WS-HT-X > WS-HT-USED OR WS-HT-FOUND.
           IF WS-HT-FOUND
               SET WS-HT-X TO WS-HT-HIT
               MOVE WS-HT-IMAGE (WS-HT-X) TO CABS-BILL-HEADER.

       P6000-EXIT.
           EXIT.

       P6010-MATCH-HDR.
           IF WS-HT-BAN (WS-HT-X) = BD-BAN
               SET WS-SUB1 TO WS-HT-X
               MOVE WS-SUB1 TO WS-HT-HIT
               MOVE 'Y' TO WS-HT-FOUND-SW.

       P6010-EXIT.
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
           MOVE 'CABFMT06  EDI 811 PRODUCTION REGISTER'
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
           MOVE 'INVOICES     SEGMENTS        DETAIL SEGMENTS'
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
           MOVE 525                    TO CT-STEP-SEQ.
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
           DISPLAY 'INVOICES SENT     ' WS-RT-INVOICES.
           DISPLAY 'SEGMENTS WRITTEN  ' WS-RT-SEGMENTS.
           DISPLAY 'DETAIL SEGMENTS   ' WS-RT-DETAIL-SEG.
           DISPLAY 'DESCRIPTIONS FIXED' WS-RT-BAD-DESC.
           DISPLAY 'VALUE INTERCHANGED' WS-RT-AMOUNT.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE BILL-DTL-IN
                 BHDR-IN-FILE
                 EDI-OUT-FILE
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

