      *****************************************************************
      * CABFMT07 - TAPE AND MEDIA EXTRACT                             *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               BDTLIN  TELCABS.CABS.BILLDTL.SEQ(0)       CABSBILL*
      *               BHDRIN  TELCABS.CABS.BILLHDR.FIN(0)       CABSBHDR*
      *               SYSIN   INSTREAM CONTROL CARD             (LOCAL)*
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               MEDOUT  TELCABS.CABS.MEDIA.EXTRACT(+1)    (LOCAL)*
      *               REPORT  SYSOUT PRINT                      CABSPRNT*
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - THE TAPE IS REWRITTEN WHOLE        *
      * REVISION HISTORY                                              *
      *   V1.00  1990-01-30  K.OYELARAN   INITIAL RELEASE - TAPE EXTRACT FOR*
      *                      THE FOUR LARGEST CARRIERS                *
      *   V1.05  1992-10-06  D.OKONKWO    MICROFICHE BLOCKING ADDED FOR THE*
      *                      FICHE BUREAU WITH A FRAME INDEX          *
      *   V1.09  1996-03-12  M.J.FERRARO  ELEMENT AREA ADDED - TEN ELEMENTS*
      *                      PER DETAIL RECORD                        *
      *   V2.00  2001-04-24  P.NAIR       FICHE BUREAU CONTRACT ENDED, OUTPUT*
      *                      REDIRECTED TO THE CD-ROM SUPPLIER        *
      *   V2.04  2009-07-21  G.PRZYBYLSKI LABEL AND TRAILER RECORDS ADDED FOR*
      *                      THE DESPATCH RECONCILIATION              *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABFMT07.
       AUTHOR. TELCABS APPLICATIONS - BILL PRINT TEAM.
      *****************************************************************
      * PRODUCES THE FLAT MEDIA EXTRACT TAKEN BY THE CARRIERS THAT DO *
      * NOT RECEIVE A PAPER BILL, WITH A LABEL AND A TRAILER CARRYING *
      * THE COUNT AND HASH FOR DESPATCH RECONCILIATION.               *
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
           SELECT MEDIA-OUT-FILE ASSIGN TO UT-S-MEDOUT
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
      * MEDOUT - THE MEDIA EXTRACT.  FOUR RECORD TYPES                *
      * DISTINGUISHED BY BYTE ONE.                                    *
      *****************************************************************
       FD  MEDIA-OUT-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 400 CHARACTERS.
       01  MEDIA-RECORD                     PIC X(400).
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
           05  WS-PGM-NAME             PIC X(08) VALUE 'CABFMT07'.
           05  WS-PGM-VERSION          PIC X(05) VALUE 'V2.04'.
           05  WS-PGM-APPL             PIC X(04) VALUE 'CABS'.
           05  WS-PGM-COMPILE          PIC X(08) VALUE '20090721'.
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
           05  WS-PE-MEDIA-CD          PIC X(01).
           05  WS-PE-BLOCK-SIZE        PIC 9(05).
           05  WS-PE-LABEL-TEXT        PIC X(20).
           05  WS-PE-VOLSER            PIC X(06).
           05  WS-PE-FILLER            PIC X(03).
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
           05  WS-MEDIA-ELIG-SW        PIC X(01) VALUE 'N'.
               88  WS-MEDIA-ELIG       VALUE 'Y'.
      *****************************************************************
      * THE MEDIA EXTRACT RECORD.  400 BYTES FIXED.  THE CARRIERS THAT*
      * TAKE THE BILL ON TAPE PARSE THIS LAYOUT WITH THEIR OWN CODE, SO*
      * THE POSITIONS ARE FIXED BY THE CARRIER INTERFACE AGREEMENT AND*
      * CANNOT BE CHANGED WITHOUT A JOINT RELEASE.                    *
      *****************************************************************
       01  WS-MEDIA-RECORD.
           05  WS-MR-TYPE              PIC X(01) VALUE SPACES.
           05  WS-MR-BAN               PIC X(13) VALUE SPACES.
           05  WS-MR-INVOICE           PIC X(14) VALUE SPACES.
           05  WS-MR-PERIOD            PIC 9(06) VALUE 0.
           05  WS-MR-SECTION           PIC X(02) VALUE SPACES.
           05  WS-MR-SEQ               PIC 9(07) VALUE 0.
           05  WS-MR-OCN               PIC X(04) VALUE SPACES.
           05  WS-MR-JURIS             PIC X(01) VALUE SPACES.
           05  WS-MR-STATE             PIC X(02) VALUE SPACES.
           05  WS-MR-DESC              PIC X(60) VALUE SPACES.
           05  WS-MR-MINUTES           PIC S9(13)V9(02) VALUE 0.
           05  WS-MR-AMOUNT            PIC S9(13)V9(02) VALUE 0.
           05  WS-MR-ELEM-CNT          PIC 9(03) VALUE 0.
           05  WS-MR-ELEM-AREA         PIC X(240) VALUE SPACES.
       01  WS-MEDIA-RECORD-H REDEFINES WS-MEDIA-RECORD.
           05  WS-MH-TYPE              PIC X(01).
           05  WS-MH-BAN               PIC X(13).
           05  WS-MH-INVOICE           PIC X(14).
           05  WS-MH-PERIOD            PIC 9(06).
           05  WS-MH-TOTAL-DUE         PIC S9(13)V9(02).
           05  WS-MH-PRIOR-BAL         PIC S9(13)V9(02).
           05  WS-MH-PAYMENTS          PIC S9(13)V9(02).
           05  WS-MH-TAX               PIC S9(11)V9(02).
           05  WS-MH-LINES             PIC 9(07).
           05  WS-MH-FILLER            PIC X(268).
       01  WS-MEDIA-RECORD-L REDEFINES WS-MEDIA-RECORD.
           05  WS-ML-TYPE              PIC X(01).
           05  WS-ML-LABEL             PIC X(20).
           05  WS-ML-VOLSER            PIC X(06).
           05  WS-ML-YYDDD             PIC 9(05).
           05  WS-ML-RUN-ID            PIC X(12).
           05  WS-ML-COUNT             PIC 9(09).
           05  WS-ML-HASH              PIC S9(15)V9(02).
           05  WS-ML-FILLER            PIC X(330).
      *****************************************************************
      * THE ELEMENT AREA IS BUILT FROM THE OCCURS DEPENDING ON AREA OF*
      * THE DETAIL RECORD, TEN ELEMENTS AT TWENTY FOUR BYTES EACH.  A *
      * DETAIL LINE WITH MORE THAN TEN ELEMENTS CARRIES THE FIRST TEN *
      * ON THE MEDIA RECORD AND THE TOTALS ACCOUNT FOR THE REST.      *
      *****************************************************************
       01  WS-ELEM-AREA.
           05  WS-EA-ENTRY OCCURS 10 TIMES.
               10  WS-EA-ELEM          PIC X(06).
               10  WS-EA-QTY           PIC S9(09)V9(02).
               10  WS-EA-AMOUNT        PIC S9(09)V9(02).
               10  WS-EA-RULE          PIC X(01).
       01  WS-ELEM-CTL.
           05  WS-EC-COUNT             PIC 9(03) VALUE 0.
           05  WS-EC-MOVED             PIC 9(03) VALUE 0.
           05  WS-EC-DROPPED           PIC S9(09) COMP-3 VALUE 0.
      *****************************************************************
      * MICROFICHE BLOCKING.  THE FICHE BUREAU TOOK A BLOCKED IMAGE OF*
      * THE PRINT STREAM WITH A FRAME INDEX EVERY SIXTY PAGES.        *
      *****************************************************************
       01  WS-FICHE-WORK.
           05  WS-FW-FRAME             PIC 9(05) VALUE 0.
           05  WS-FW-PAGE-IN-FRAME     PIC 9(03) VALUE 0.
           05  WS-FW-PAGES-PER-FRAME   PIC 9(03) VALUE 60.
           05  WS-FW-INDEX-KEY         PIC X(20) VALUE SPACES.
           05  WS-FW-BLOCK-CNT         PIC S9(09) COMP-3 VALUE 0.
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
           05  WS-RT-HEADERS           PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-DETAILS           PIC S9(11) COMP-3 VALUE 0.
           05  WS-RT-SKIPPED           PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-HASH              PIC S9(15)V9(02) COMP-3 VALUE 0.
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
           OPEN OUTPUT MEDIA-OUT-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 6711 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-BDTLIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 6712 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-BHDRIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 6713 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-MEDOUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 6714 TO WS-AB-CODE
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
           PERFORM P4800-LABEL THRU P4800-EXIT.
           PERFORM P6000-HEADING THRU P6000-EXIT.
           DISPLAY WS-PGM-NAME ' STARTED - RUN ' WS-RUN-ID.
           DISPLAY '  MEDIA CODE    ' WS-PE-MEDIA-CD.
           DISPLAY '  VOLUME SERIAL ' WS-PE-VOLSER.

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
      * THE VOLUME SERIAL IS ALLOCATED BY THE TAPE LIBRARY AND
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
           IF WS-PE-VOLSER = SPACES
               MOVE 6721 TO WS-AB-CODE
               MOVE 'VOLUME SERIAL NOT SUPPLIED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PE-MEDIA-CD = SPACES
               MOVE 'T' TO WS-PE-MEDIA-CD.
           IF WS-PE-BLOCK-SIZE NOT NUMERIC
               MOVE ZERO TO WS-PE-BLOCK-SIZE.

       P1200-EXIT.
           EXIT.

      *****************************************************************
      * S200-MAIN PROCESSING                                          *
      * THE TAPE EXTRACT.  A HEADER RECORD PER INVOICE FOLLOWED BY ONE*
      * DETAIL RECORD PER BILL DETAIL LINE, THEN A LABEL TRAILER.     *
      *****************************************************************
       S200-MAIN-PROCESS SECTION.

       P2000-PROCESS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ-DETAIL THRU P2100-EXIT.
           IF WS-DTL-EOF
               PERFORM P4900-TRAILER THRU P4900-EXIT
               MOVE 'Y' TO WS-EOF-SW
               GO TO P2000-EXIT.
           ADD 1 TO WS-READ-CNT.
           MOVE BD-BAN TO WS-RESTART-KEY.
           PERFORM P3000-MEDIA-ELIGIBLE THRU P3000-EXIT.
           IF NOT WS-MEDIA-ELIG
               ADD 1 TO WS-RT-SKIPPED
               ADD 1 TO WS-CFWD-CNT
               GO TO P2000-EXIT.
           PERFORM P3200-BUILD-DETAIL THRU P3200-EXIT.
           PERFORM P4000-WRITE-MEDIA THRU P4000-EXIT.
           ADD 1 TO WS-RT-DETAILS.
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
               MOVE 6701 TO WS-AB-CODE
               MOVE 'BILL DETAIL READ ERROR' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.

       P2100-EXIT.
           EXIT.

      *****************************************************************
      * S300-MEDIA RECORD BUILD                                       *
      *****************************************************************
       S300-BUILD SECTION.

       P3000-MEDIA-ELIGIBLE.
      * ONLY THE CARRIERS THAT TAKE THE BILL ON TAPE APPEAR ON THIS
      * EXTRACT.  THE MEDIA CODE IS HELD ON THE CARRIER MASTER AND IS
      * CARRIED ONTO THE HEADER BY THE TRIGGER PROCESS.
           MOVE 'P3000-MEDIA-ELIGIBLE' TO WS-PARA-NAME.
           MOVE 'N' TO WS-MEDIA-ELIG-SW.
           PERFORM P5000-FIND-HEADER THRU P5000-EXIT.
           IF NOT WS-HT-FOUND
               GO TO P3000-EXIT.
           IF NOT BH-FINAL
               GO TO P3000-EXIT.
           MOVE 'Y' TO WS-MEDIA-ELIG-SW.
           IF BD-LINE-SEQ = 1
               PERFORM P3100-BUILD-HEADER THRU P3100-EXIT.

       P3000-EXIT.
           EXIT.

       P3100-BUILD-HEADER.
      * THE INVOICE HEADER RECORD ON THE TAPE.  TYPE H.
           MOVE 'P3100-BUILD-HEADER' TO WS-PARA-NAME.
           MOVE SPACES TO WS-MEDIA-RECORD.
           MOVE 'H'                TO WS-MH-TYPE.
           MOVE BH-BAN             TO WS-MH-BAN.
           MOVE BH-INVOICE-NBR     TO WS-MH-INVOICE.
           MOVE BH-BILL-PERIOD     TO WS-MH-PERIOD.
           MOVE BH-TOTAL-DUE       TO WS-MH-TOTAL-DUE.
           MOVE BH-PRIOR-BAL       TO WS-MH-PRIOR-BAL.
           MOVE BH-PAYMENTS        TO WS-MH-PAYMENTS.
           MOVE BH-TAX             TO WS-MH-TAX.
           MOVE BH-DETAIL-LINES    TO WS-MH-LINES.
           PERFORM P4000-WRITE-MEDIA THRU P4000-EXIT.
           ADD 1 TO WS-RT-HEADERS.
           ADD BH-TOTAL-DUE TO WS-RT-HASH.

       P3100-EXIT.
           EXIT.

       P3200-BUILD-DETAIL.
      * THE DETAIL RECORD ON THE TAPE.  TYPE D.  THE ELEMENT AREA
      * CARRIES THE FIRST TEN RATE ELEMENTS.
      * THE ODO LIMIT IS AGREED WITH THE BILL PRINT VENDOR.
           MOVE 'P3200-BUILD-DETAIL' TO WS-PARA-NAME.
           MOVE SPACES TO WS-MEDIA-RECORD.
           MOVE 'D'                TO WS-MR-TYPE.
           MOVE BD-BAN             TO WS-MR-BAN.
           MOVE BH-INVOICE-NBR     TO WS-MR-INVOICE.
           MOVE BD-BILL-PERIOD     TO WS-MR-PERIOD.
           MOVE BD-SECTION         TO WS-MR-SECTION.
           MOVE BD-LINE-SEQ        TO WS-MR-SEQ.
           MOVE BD-OCN             TO WS-MR-OCN.
           MOVE BD-JURIS-CD        TO WS-MR-JURIS.
           MOVE BD-STATE-CD        TO WS-MR-STATE.
           MOVE BD-DESCRIPTION     TO WS-MR-DESC.
           MOVE BD-TOT-MINUTES     TO WS-MR-MINUTES.
           MOVE BD-TOT-ROUNDED     TO WS-MR-AMOUNT.
           MOVE BD-ELEM-CNT        TO WS-MR-ELEM-CNT.
           PERFORM P3300-BUILD-ELEMENTS THRU P3300-EXIT.
           MOVE WS-ELEM-AREA       TO WS-MR-ELEM-AREA.
           ADD BD-TOT-ROUNDED      TO WS-RT-HASH.
           ADD BD-TOT-ROUNDED      TO WS-ACC-AMOUNT.

       P3200-EXIT.
           EXIT.

       P3300-BUILD-ELEMENTS.
           MOVE 'P3300-BUILD-ELEMENTS' TO WS-PARA-NAME.
           MOVE SPACES TO WS-ELEM-AREA.
           MOVE BD-ELEM-CNT TO WS-EC-COUNT.
           IF WS-EC-COUNT > 40
               MOVE 40 TO WS-EC-COUNT.
           MOVE WS-EC-COUNT TO WS-EC-MOVED.
           IF WS-EC-MOVED > 10
               COMPUTE WS-EC-DROPPED =
                       WS-EC-DROPPED + (WS-EC-MOVED - 10)
               MOVE 10 TO WS-EC-MOVED.
           PERFORM P3310-ONE-ELEMENT THRU P3310-EXIT
               VARYING WS-SUB1 FROM 1 BY 1
               UNTIL WS-SUB1 > WS-EC-MOVED.

       P3300-EXIT.
           EXIT.

       P3310-ONE-ELEMENT.
           SET BD-EX TO WS-SUB1.
           MOVE BD-EL-RATE-ELEM (BD-EX)  TO WS-EA-ELEM (WS-SUB1).
           MOVE BD-EL-QTY (BD-EX)        TO WS-EA-QTY (WS-SUB1).
           MOVE BD-EL-AMOUNT (BD-EX)     TO WS-EA-AMOUNT (WS-SUB1).
           MOVE BD-EL-ROUND-RULE (BD-EX) TO WS-EA-RULE (WS-SUB1).

       P3310-EXIT.
           EXIT.

      *****************************************************************
      * S400-OUTPUT                                                   *
      *****************************************************************
       S400-OUTPUT SECTION.

       P4000-WRITE-MEDIA.
           MOVE 'P4000-WRITE-MEDIA' TO WS-PARA-NAME.
           WRITE MEDIA-RECORD FROM WS-MEDIA-RECORD.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 6702 TO WS-AB-CODE
               MOVE 'MEDIA EXTRACT WRITE FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-WRITE-CNT.

       P4000-EXIT.
           EXIT.

       P4800-LABEL.
      * THE LABEL RECORD AT THE FRONT OF THE TAPE.  THE BUREAU MATCH
      * THE LABEL TEXT AND THE VOLUME SERIAL AGAINST THEIR DESPATCH
      * NOTE BEFORE THEY LOAD IT.
           MOVE 'P4800-LABEL' TO WS-PARA-NAME.
           MOVE SPACES TO WS-MEDIA-RECORD.
           MOVE 'L'                TO WS-ML-TYPE.
           MOVE WS-PE-LABEL-TEXT   TO WS-ML-LABEL.
           MOVE WS-PE-VOLSER       TO WS-ML-VOLSER.
           MOVE WS-CYCLE-YYDDD     TO WS-ML-YYDDD.
           MOVE WS-RUN-ID          TO WS-ML-RUN-ID.
           MOVE ZERO               TO WS-ML-COUNT.
           MOVE ZERO               TO WS-ML-HASH.
           PERFORM P4000-WRITE-MEDIA THRU P4000-EXIT.

       P4800-EXIT.
           EXIT.

       P4900-TRAILER.
           MOVE 'P4900-TRAILER' TO WS-PARA-NAME.
           MOVE SPACES TO WS-MEDIA-RECORD.
           MOVE 'T'                TO WS-ML-TYPE.
           MOVE WS-PE-LABEL-TEXT   TO WS-ML-LABEL.
           MOVE WS-PE-VOLSER       TO WS-ML-VOLSER.
           MOVE WS-CYCLE-YYDDD     TO WS-ML-YYDDD.
           MOVE WS-RUN-ID          TO WS-ML-RUN-ID.
           MOVE WS-WRITE-CNT       TO WS-ML-COUNT.
           MOVE WS-RT-HASH         TO WS-ML-HASH.
           PERFORM P4000-WRITE-MEDIA THRU P4000-EXIT.

       P4900-EXIT.
           EXIT.

      *****************************************************************
      * S500-SUPPORT                                                  *
      *****************************************************************
       S500-SUPPORT SECTION.

       P5000-FIND-HEADER.
           MOVE 'P5000-FIND-HEADER' TO WS-PARA-NAME.
           MOVE 'N' TO WS-HT-FOUND-SW.
           MOVE ZERO TO WS-HT-HIT.
           PERFORM P5010-MATCH-HDR THRU P5010-EXIT
               VARYING WS-HT-X FROM 1 BY 1
               UNTIL WS-HT-X > WS-HT-USED OR WS-HT-FOUND.
           IF WS-HT-FOUND
               SET WS-HT-X TO WS-HT-HIT
               MOVE WS-HT-IMAGE (WS-HT-X) TO CABS-BILL-HEADER.

       P5000-EXIT.
           EXIT.

       P5010-MATCH-HDR.
           IF WS-HT-BAN (WS-HT-X) = BD-BAN
               SET WS-SUB1 TO WS-HT-X
               MOVE WS-SUB1 TO WS-HT-HIT
               MOVE 'Y' TO WS-HT-FOUND-SW.

       P5010-EXIT.
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
               MOVE 6703 TO WS-AB-CODE
               MOVE 'HEADER TABLE FULL' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-HT-USED.
           SET WS-HT-X TO WS-HT-USED.
           MOVE BH-BAN           TO WS-HT-BAN (WS-HT-X).
           MOVE CABS-BILL-HEADER TO WS-HT-IMAGE (WS-HT-X).

       P5110-EXIT.
           EXIT.

       P5400-MICROFICHE-BLOCK.
      * BLOCK THE EXTRACT INTO FICHE FRAMES AND WRITE A FRAME INDEX
      * KEY EVERY SIXTY PAGES.  THE BUREAU READ THE INDEX KEY TO FIND
      * AN ACCOUNT WITHOUT WINDING THROUGH THE WHOLE FRAME.
           MOVE 'P5400-MICROFICHE-BLOCK' TO WS-PARA-NAME.
           ADD 1 TO WS-FW-PAGE-IN-FRAME.
           IF WS-FW-PAGE-IN-FRAME < WS-FW-PAGES-PER-FRAME
               GO TO P5400-EXIT.
           ADD 1 TO WS-FW-FRAME.
           MOVE ZERO TO WS-FW-PAGE-IN-FRAME.
           MOVE SPACES TO WS-FW-INDEX-KEY.
           STRING WS-PE-VOLSER     DELIMITED BY SIZE
                  WS-FW-FRAME      DELIMITED BY SIZE
                  BD-BAN           DELIMITED BY SIZE
                  INTO WS-FW-INDEX-KEY.
           MOVE SPACES TO WS-MEDIA-RECORD.
           MOVE 'F'                TO WS-ML-TYPE.
           MOVE WS-FW-INDEX-KEY    TO WS-ML-LABEL.
           MOVE WS-PE-VOLSER       TO WS-ML-VOLSER.
           MOVE WS-FW-FRAME        TO WS-ML-COUNT.
           PERFORM P4000-WRITE-MEDIA THRU P4000-EXIT.
           ADD 1 TO WS-FW-BLOCK-CNT.

       P5400-EXIT.
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
           MOVE 'CABFMT07  MEDIA EXTRACT REGISTER'
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
           MOVE 'HEADERS      DETAIL RECORDS   SKIPPED'
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
           MOVE 530                    TO CT-STEP-SEQ.
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
           DISPLAY 'MEDIA HEADERS     ' WS-RT-HEADERS.
           DISPLAY 'MEDIA DETAILS     ' WS-RT-DETAILS.
           DISPLAY 'LINES SKIPPED     ' WS-RT-SKIPPED.
           DISPLAY 'ELEMENTS DROPPED  ' WS-EC-DROPPED.
           DISPLAY 'EXTRACT HASH      ' WS-RT-HASH.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE BILL-DTL-IN
                 BHDR-IN-FILE
                 MEDIA-OUT-FILE
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

