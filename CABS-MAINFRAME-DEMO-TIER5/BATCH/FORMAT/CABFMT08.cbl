      *****************************************************************
      * CABFMT08 - PRINT CONTROL FILE WRITER                          *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               PRTIN   TELCABS.CABS.PRINT.TOT(0)         CABSPRNT*
      *               SYSIN   INSTREAM CONTROL CARD             (LOCAL)*
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               DOCCTL  TELCABS.CABS.PRTCTL.DOC(+1)       (LOCAL)*
      *               REPORT  SYSOUT PRINT                      CABSPRNT*
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN                                      *
      * REVISION HISTORY                                              *
      *   V1.00  1991-03-25  D.OKONKWO    INITIAL RELEASE - DOCUMENT COUNT*
      *                      ONLY, FOR THE FIRST BURSTER              *
      *   V1.04  1994-12-14  M.J.FERRARO  CARRIAGE CONTROL RECAP ADDED AT THE*
      *                      MAILROOM REQUEST                         *
      *   V2.00  2000-05-16  P.NAIR       FORM ID AND TRAY NUMBER CARRIED ON*
      *                      THE DOCUMENT RECORD                      *
      *   V2.03  2007-10-16  G.PRZYBYLSKI PHYSICAL LINE RANGE ADDED SO THE*
      *                      BURSTER CAN SEEK TO A DOCUMENT           *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABFMT08.
       AUTHOR. TELCABS APPLICATIONS - BILL PRINT TEAM.
      *****************************************************************
      * DESCRIBES THE FINISHED PRINT STREAM FOR THE BURST AND INSERT  *
      * MACHINERY.  COUNTS EVERY CARRIAGE CONTROL CHARACTER AND WRITES*
      * ONE CONTROL RECORD PER PHYSICAL DOCUMENT.                     *
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
           SELECT DOC-CTL-FILE ASSIGN TO UT-S-DOCCTL
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
      * PRTIN - THE FINISHED PRINT STREAM.                            *
      *****************************************************************
       FD  PRINT-IN-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       01  PRINT-IN-REC                     PIC X(133).
      *****************************************************************
      * DOCCTL - ONE RECORD PER PHYSICAL DOCUMENT.                    *
      *****************************************************************
       FD  DOC-CTL-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 90 CHARACTERS.
       01  DOC-RECORD                       PIC X(90).
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
           05  WS-PGM-NAME             PIC X(08) VALUE 'CABFMT08'.
           05  WS-PGM-VERSION          PIC X(05) VALUE 'V2.03'.
           05  WS-PGM-APPL             PIC X(04) VALUE 'CABS'.
           05  WS-PGM-COMPILE          PIC X(08) VALUE '20071016'.
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
           05  WS-PE-BURST-SW          PIC X(01).
           05  WS-PE-INSERT-SW         PIC X(01).
           05  WS-PE-FORM-ID           PIC X(04).
           05  WS-PE-TRAY              PIC 9(01).
           05  WS-PE-FILLER            PIC X(28).
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
           05  WS-DOC-OPEN-SW          PIC X(01) VALUE 'N'.
               88  WS-DOC-OPEN         VALUE 'Y'.
      *****************************************************************
      * CARRIAGE CONTROL COUNTERS.  ONE PER RECOGNISED CHARACTER.  THE*
      * COUNTS ARE THE ONLY EVIDENCE THE MAILROOM HAS THAT THE PRINT  *
      * STREAM CONTAINS WHAT THE BILLING RUN SAID IT WOULD.           *
      * THE PRINT REGISTER IS RECONCILED BY THE MAILROOM.             *
      *****************************************************************
       01  WS-CC-TABLE.
           05  FILLER PIC X(21) VALUE ' SINGLE SPACE        '.
           05  FILLER PIC X(21) VALUE '0DOUBLE SPACE        '.
           05  FILLER PIC X(21) VALUE '-TRIPLE SPACE        '.
           05  FILLER PIC X(21) VALUE '1NEW PAGE            '.
           05  FILLER PIC X(21) VALUE '4NEW BILL SECTION    '.
           05  FILLER PIC X(21) VALUE '7NEW INVOICE         '.
           05  FILLER PIC X(21) VALUE '+OVERPRINT SUPPRESS  '.
           05  FILLER PIC X(21) VALUE '*UNRECOGNISED        '.
       01  WS-CC-TABLE-R REDEFINES WS-CC-TABLE.
           05  WS-CC-ENTRY OCCURS 8 TIMES INDEXED BY WS-CC-X.
               10  WS-CC-CHAR          PIC X(01).
               10  WS-CC-NAME          PIC X(20).
       01  WS-CC-COUNTS.
           05  WS-CC-COUNT OCCURS 8 TIMES PIC S9(11) COMP-3.
       01  WS-CC-WORK.
           05  WS-CW-SUB               PIC S9(03) COMP-3 VALUE 0.
           05  WS-CW-FOUND-SW          PIC X(01) VALUE 'N'.
               88  WS-CW-FOUND         VALUE 'Y'.
      *****************************************************************
      * THE DOCUMENT CONTROL RECORD.  ONE PER PHYSICAL DOCUMENT IN THE*
      * PRINT STREAM.  THE BURST PROCESS SPLITS ON THE NEW INVOICE    *
      * CHARACTER AND USES THE PAGE COUNT TO SET THE FOLDER.          *
      *****************************************************************
       01  WS-DOC-RECORD.
           05  WS-DR-SEQ               PIC 9(07) VALUE 0.
           05  WS-DR-INVOICE           PIC X(14) VALUE SPACES.
           05  WS-DR-BAN               PIC X(13) VALUE SPACES.
           05  WS-DR-FORM              PIC X(04) VALUE SPACES.
           05  WS-DR-TRAY              PIC 9(01) VALUE 0.
           05  WS-DR-PAGES             PIC 9(05) VALUE 0.
           05  WS-DR-SECTIONS          PIC 9(05) VALUE 0.
           05  WS-DR-LINES             PIC 9(07) VALUE 0.
           05  WS-DR-START-LINE        PIC 9(09) VALUE 0.
           05  WS-DR-END-LINE          PIC 9(09) VALUE 0.
           05  WS-DR-FILLER            PIC X(16) VALUE SPACES.
       01  WS-DOC-RECORD-K REDEFINES WS-DOC-RECORD.
           05  WS-DK-KEY               PIC X(21).
           05  WS-DK-REST              PIC X(69).
       01  WS-DOC-WORK.
           05  WS-DW-SEQ               PIC S9(07) COMP-3 VALUE 0.
           05  WS-DW-PAGES             PIC S9(05) COMP-3 VALUE 0.
           05  WS-DW-SECTIONS          PIC S9(05) COMP-3 VALUE 0.
           05  WS-DW-LINES             PIC S9(07) COMP-3 VALUE 0.
           05  WS-DW-START             PIC S9(09) COMP-3 VALUE 0.
           05  WS-DW-PHYS-LINE         PIC S9(09) COMP-3 VALUE 0.
       01  WS-RUN-TOTALS.
           05  WS-RT-DOCS              PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-PAGES             PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-SECTIONS          PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-LINES             PIC S9(11) COMP-3 VALUE 0.
           05  WS-RT-UNKNOWN-CC        PIC S9(09) COMP-3 VALUE 0.
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
           OPEN OUTPUT DOC-CTL-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 6811 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-PRTIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 6812 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-DOCCTL' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 6813 TO WS-AB-CODE
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
           PERFORM P5200-CLEAR-CC THRU P5200-EXIT
               VARYING WS-SUB1 FROM 1 BY 1
               UNTIL WS-SUB1 > 8.
           PERFORM P6000-HEADING THRU P6000-EXIT.
           DISPLAY WS-PGM-NAME ' STARTED - RUN ' WS-RUN-ID.
           DISPLAY '  FORM ID       ' WS-PE-FORM-ID.
           DISPLAY '  TRAY          ' WS-PE-TRAY.

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
      * THE FORM ID IS ALLOCATED BY THE PRINT ROOM ON THE NIGHT AND
      * PASSED IN BY THE SCHEDULER.  IT HAS NO DEFAULT.
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
           IF WS-PE-FORM-ID = SPACES
               MOVE 6821 TO WS-AB-CODE
               MOVE 'FORM ID NOT SUPPLIED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PE-TRAY NOT NUMERIC
               MOVE 1 TO WS-PE-TRAY.
           IF WS-PE-BURST-SW NOT = 'N'
               MOVE 'Y' TO WS-PE-BURST-SW.

       P1200-EXIT.
           EXIT.

      *****************************************************************
      * S200-MAIN PROCESSING                                          *
      * READ THE FINISHED PRINT STREAM AND WRITE THE CONTROL FILE THE *
      * BURST AND INSERT MACHINERY DRIVES FROM.  THIS PROGRAM DOES NOT*
      * CHANGE THE PRINT STREAM - IT ONLY DESCRIBES IT.               *
      *****************************************************************
       S200-MAIN-PROCESS SECTION.

       P2000-PROCESS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ-LINE THRU P2100-EXIT.
           IF WS-PRT-EOF
               PERFORM P3400-CLOSE-DOCUMENT THRU P3400-EXIT
               MOVE 'Y' TO WS-EOF-SW
               GO TO P2000-EXIT.
           ADD 1 TO WS-READ-CNT.
           ADD 1 TO WS-DW-PHYS-LINE.
           PERFORM P3000-COUNT-CC THRU P3000-EXIT.
           PERFORM P3100-DOCUMENT-BREAK THRU P3100-EXIT.
           PERFORM P3200-ACCUM-DOCUMENT THRU P3200-EXIT.
           ADD 1 TO WS-SUMM-CNT.

       P2000-EXIT.
           EXIT.

       P2100-READ-LINE.
           MOVE 'P2100-READ-LINE' TO WS-PARA-NAME.
           READ PRINT-IN-FILE INTO CABS-PRINT-LINE
               AT END
                   MOVE 'Y' TO WS-PRT-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00' AND WS-FS-INPUT NOT = '10'
               MOVE 6801 TO WS-AB-CODE
               MOVE 'PRINT STREAM READ ERROR' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.

       P2100-EXIT.
           EXIT.

      *****************************************************************
      * S300-CONTROL FILE BUILD                                       *
      *****************************************************************
       S300-CONTROL SECTION.

       P3000-COUNT-CC.
      * WALK THE CARRIAGE CONTROL TABLE AND BUMP THE MATCHING COUNTER.
      * A CHARACTER THAT IS NOT IN THE TABLE IS COUNTED AGAINST THE
      * LAST ENTRY AND REPORTED - AN UNRECOGNISED CHARACTER WILL STOP
      * THE BURSTER MID RUN.
           MOVE 'P3000-COUNT-CC' TO WS-PARA-NAME.
           MOVE 'N' TO WS-CW-FOUND-SW.
           MOVE 8 TO WS-CW-SUB.
           PERFORM P3010-MATCH-CC THRU P3010-EXIT
               VARYING WS-CC-X FROM 1 BY 1
               UNTIL WS-CC-X > 8 OR WS-CW-FOUND.
           ADD 1 TO WS-CC-COUNT (WS-CW-SUB).
           IF NOT WS-CW-FOUND
               ADD 1 TO WS-RT-UNKNOWN-CC.

       P3000-EXIT.
           EXIT.

       P3010-MATCH-CC.
           IF WS-CC-CHAR (WS-CC-X) = PC-CC
               SET WS-SUB1 TO WS-CC-X
               MOVE WS-SUB1 TO WS-CW-SUB
               MOVE 'Y' TO WS-CW-FOUND-SW.

       P3010-EXIT.
           EXIT.

       P3100-DOCUMENT-BREAK.
      * A NEW INVOICE CHARACTER CLOSES THE OPEN DOCUMENT AND OPENS THE
      * NEXT.  NOTHING ELSE STARTS A DOCUMENT - A SECTION BREAK STAYS
      * INSIDE THE SAME PHYSICAL PIECE OF MAIL.
           MOVE 'P3100-DOCUMENT-BREAK' TO WS-PARA-NAME.
           IF NOT PC-NEW-INVOICE
               GO TO P3100-EXIT.
           PERFORM P3400-CLOSE-DOCUMENT THRU P3400-EXIT.
           ADD 1 TO WS-DW-SEQ.
           MOVE ZERO TO WS-DW-PAGES WS-DW-SECTIONS WS-DW-LINES.
           MOVE WS-DW-PHYS-LINE TO WS-DW-START.
           MOVE PC-COL-001-020 TO WS-DR-INVOICE.
           MOVE PC-COL-021-060 TO WS-DR-BAN.
           MOVE 'Y' TO WS-DOC-OPEN-SW.
           ADD 1 TO WS-DW-PAGES.
           ADD 1 TO WS-DW-SECTIONS.

       P3100-EXIT.
           EXIT.

       P3200-ACCUM-DOCUMENT.
           MOVE 'P3200-ACCUM-DOCUMENT' TO WS-PARA-NAME.
           IF NOT WS-DOC-OPEN
               GO TO P3200-EXIT.
           ADD 1 TO WS-DW-LINES.
           IF PC-NEW-PAGE
               ADD 1 TO WS-DW-PAGES.
           IF PC-NEW-SECTION
               ADD 1 TO WS-DW-SECTIONS.

       P3200-EXIT.
           EXIT.

       P3400-CLOSE-DOCUMENT.
           MOVE 'P3400-CLOSE-DOCUMENT' TO WS-PARA-NAME.
           IF NOT WS-DOC-OPEN
               GO TO P3400-EXIT.
           MOVE SPACES TO WS-DOC-RECORD.
           MOVE WS-DW-SEQ          TO WS-DR-SEQ.
           MOVE WS-PE-FORM-ID      TO WS-DR-FORM.
           MOVE WS-PE-TRAY         TO WS-DR-TRAY.
           MOVE WS-DW-PAGES        TO WS-DR-PAGES.
           MOVE WS-DW-SECTIONS     TO WS-DR-SECTIONS.
           MOVE WS-DW-LINES        TO WS-DR-LINES.
           MOVE WS-DW-START        TO WS-DR-START-LINE.
           MOVE WS-DW-PHYS-LINE    TO WS-DR-END-LINE.
           WRITE DOC-RECORD FROM WS-DOC-RECORD.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 6802 TO WS-AB-CODE
               MOVE 'DOCUMENT CONTROL WRITE FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-WRITE-CNT.
           ADD 1 TO WS-RT-DOCS.
           ADD WS-DW-PAGES    TO WS-RT-PAGES.
           ADD WS-DW-SECTIONS TO WS-RT-SECTIONS.
           ADD WS-DW-LINES    TO WS-RT-LINES.
           MOVE 'N' TO WS-DOC-OPEN-SW.
           PERFORM P5000-REGISTER-LINE THRU P5000-EXIT.

       P3400-EXIT.
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
           MOVE WS-DR-INVOICE  TO PC-COL-001-020.
           MOVE WS-DR-BAN      TO PC-COL-021-060.
           MOVE WS-DR-PAGES    TO WS-ED-COUNT.
           MOVE WS-ED-COUNT    TO PC-COL-061-090.
           MOVE WS-DR-SECTIONS TO WS-ED-COUNT.
           MOVE WS-ED-COUNT    TO PC-COL-091-132.
           WRITE PRT-RECORD FROM CABS-PRINT-LINE.
           ADD 1 TO WS-PAGE-LINES.

       P5000-EXIT.
           EXIT.

       P5100-PRINT-CC-RECAP.
      * THE CARRIAGE CONTROL RECAP.  THE MAILROOM COMPARE THE NEW
      * INVOICE COUNT WITH THE NUMBER OF ENVELOPES THEY SEALED AND THE
      * NEW SECTION COUNT WITH THE NUMBER OF FOLDS THE BURSTER MADE.
           MOVE 'P5100-PRINT-CC-RECAP' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '1' TO PC-CC.
           MOVE 'CARRIAGE CONTROL RECAP' TO PC-TEXT.
           WRITE PRT-RECORD FROM CABS-PRINT-LINE.
           PERFORM P5110-PRINT-ONE-CC THRU P5110-EXIT
               VARYING WS-CC-X FROM 1 BY 1
               UNTIL WS-CC-X > 8.

       P5100-EXIT.
           EXIT.

       P5110-PRINT-ONE-CC.
           SET WS-SUB1 TO WS-CC-X.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-CC-NAME (WS-CC-X)   TO PC-COL-001-020.
           MOVE WS-CC-COUNT (WS-SUB1)  TO WS-ED-COUNT.
           MOVE WS-ED-COUNT            TO PC-COL-021-060.
           WRITE PRT-RECORD FROM CABS-PRINT-LINE.

       P5110-EXIT.
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
           MOVE 'CABFMT08  PRINT CONTROL FILE REGISTER'
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
           MOVE 'INVOICE NUMBER      BAN            PAGES    SECTIONS'
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
           MOVE 535                    TO CT-STEP-SEQ.
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
           PERFORM P5100-PRINT-CC-RECAP THRU P5100-EXIT.
           DISPLAY 'DOCUMENTS         ' WS-RT-DOCS.
           DISPLAY 'PAGES             ' WS-RT-PAGES.
           DISPLAY 'SECTION BREAKS    ' WS-RT-SECTIONS.
           DISPLAY 'PRINT LINES       ' WS-RT-LINES.
           DISPLAY 'UNRECOGNISED CC   ' WS-RT-UNKNOWN-CC.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE PRINT-IN-FILE
                 DOC-CTL-FILE
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

       P5200-CLEAR-CC.
           MOVE ZERO TO WS-CC-COUNT (WS-SUB1).

       P5200-EXIT.
           EXIT.
