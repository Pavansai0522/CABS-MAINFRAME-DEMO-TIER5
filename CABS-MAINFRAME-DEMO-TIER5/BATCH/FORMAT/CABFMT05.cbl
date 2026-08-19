      *****************************************************************
      * CABFMT05 - INVOICE SUMMARY PAGE BUILDER                       *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               BHDRIN  TELCABS.CABS.BILLHDR.FIN(0)       CABSBHDR*
      *               SYSIN   INSTREAM CONTROL CARD             (LOCAL)*
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               PRTOUT  TELCABS.CABS.PRINT.SUMM(+1)       CABSPRNT*
      *               REPORT  SYSOUT PRINT                      CABSPRNT*
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN                                      *
      * REVISION HISTORY                                              *
      *   V1.00  1989-06-27  K.OYELARAN   INITIAL RELEASE - NINE LINE SUMMARY*
      *   V1.05  1993-03-16  D.OKONKWO    SHORT RECAP STYLE ADDED FOR THE TAPE*
      *                      MEDIA CUSTOMERS                          *
      *   V1.09  1996-05-21  M.J.FERRARO  SUMMARY PAGE PRODUCED AS A SEPARATE*
      *                      DOCUMENT AND MERGED BY THE INSERTER      *
      *   V2.00  1999-10-05  J.M.CASTILLO JURISDICTIONAL SPLIT SHOWN WHERE THE*
      *                      CARRIER HAS ASKED FOR IT                 *
      *   V2.07  2014-09-17  G.PRZYBYLSKI SUMMARY PAGE NOW PROVES ITSELF*
      *                      AGAINST THE TOTAL DUE                    *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABFMT05.
       AUTHOR. TELCABS APPLICATIONS - BILL PRINT TEAM.
      *****************************************************************
      * BUILDS THE SUMMARY OF CHARGES PAGE THAT SITS IN FRONT OF THE  *
      * DETAIL PAGES AND PROVES ITS COMPONENTS AGAINST THE TOTAL DUE. *
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
      * BHDRIN - THE FINAL NUMBERED INVOICE HEADER.                   *
      *****************************************************************
       FD  BHDR-IN-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 400 CHARACTERS.
       01  BHDR-IN-REC                      PIC X(400).
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
           05  WS-PGM-NAME             PIC X(08) VALUE 'CABFMT05'.
           05  WS-PGM-VERSION          PIC X(05) VALUE 'V2.07'.
           05  WS-PGM-APPL             PIC X(04) VALUE 'CABS'.
           05  WS-PGM-COMPILE          PIC X(08) VALUE '20140917'.
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
           05  WS-PE-SUMM-STYLE        PIC X(01).
           05  WS-PE-RECAP-SW          PIC X(01).
           05  WS-PE-TAX-DETAIL-SW     PIC X(01).
           05  WS-PE-JURIS-SHOW-SW     PIC X(01).
           05  WS-PE-FILLER            PIC X(31).
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
      *****************************************************************
      * SUMMARY PAGE STYLE.  THE TWO CONDITIONS BELOW WERE ADDED IN   *
      * 1993 AND 1999 AND HAVE OVERLAPPED SINCE - A STYLE OF R        *
      * SATISFIES BOTH.  P3000 TESTS THE SUMMARY CONDITION FIRST.     *
      * VALUES ARE SET BY THE TARIFF GROUP, NOT BY THIS PROGRAM.      *
      *****************************************************************
       01  WS-SUMM-STYLE-WORK.
           05  WS-SP-STYLE             PIC X(01) VALUE SPACES.
               88  WS-SP-SUMMARY       VALUE 'S' THRU 'R'.
               88  WS-SP-RECAP         VALUE 'R' THRU 'Z'.
               88  WS-SP-FULL          VALUE 'F'.
               88  WS-SP-ANY-PRINTED   VALUE 'F' THRU 'Z'.
           05  WS-SP-LINE-CNT          PIC S9(03) COMP-3 VALUE 0.
      *****************************************************************
      * SUMMARY LINE TABLE.  THE LINES ON THE SUMMARY PAGE AND THE    *
      * HEADER FIELD EACH ONE DRAWS FROM.  THE ORDER IS FIXED BY THE  *
      * TARIFF AND MUST NOT BE REARRANGED.                            *
      *****************************************************************
       01  WS-SUMM-LINE-TABLE.
           05  FILLER PIC X(34) VALUE
               'PBBALANCE FROM PREVIOUS BILL      '.
           05  FILLER PIC X(34) VALUE
               'PYPAYMENTS RECEIVED - THANK YOU   '.
           05  FILLER PIC X(34) VALUE
               'AJADJUSTMENTS                     '.
           05  FILLER PIC X(34) VALUE
               'RSPRIOR PERIOD RESTATEMENT        '.
           05  FILLER PIC X(34) VALUE
               'USCURRENT ACCESS USAGE            '.
           05  FILLER PIC X(34) VALUE
               'RCCURRENT RECURRING CHARGES       '.
           05  FILLER PIC X(34) VALUE
               'NRCURRENT NON RECURRING CHARGES   '.
           05  FILLER PIC X(34) VALUE
               'STINTER CARRIER SETTLEMENT        '.
           05  FILLER PIC X(34) VALUE
               'TXTAXES AND SURCHARGES            '.
           05  FILLER PIC X(34) VALUE
               'TDTOTAL AMOUNT DUE                '.
       01  WS-SUMM-LINE-R REDEFINES WS-SUMM-LINE-TABLE.
           05  WS-SL-ENTRY OCCURS 10 TIMES INDEXED BY WS-SL-X.
               10  WS-SL-CODE          PIC X(02).
               10  WS-SL-TEXT          PIC X(32).
       01  WS-SUMM-WORK.
           05  WS-SM-AMOUNT            PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-SM-TAX-RAW           PIC S9(11)V9(02) COMP-3 VALUE 0.
           05  WS-SM-TAX-SHOWN         PIC S9(11) COMP-3 VALUE 0.
           05  WS-SM-CHECK             PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-SM-VARIANCE          PIC S9(13)V9(02) COMP-3 VALUE 0.
       01  WS-JURIS-TEXTS.
           05  FILLER PIC X(30) VALUE '  INTERSTATE                  '.
           05  FILLER PIC X(30) VALUE '  INTRASTATE                  '.
           05  FILLER PIC X(30) VALUE '  LOCAL                       '.
       01  WS-JURIS-TEXTS-R REDEFINES WS-JURIS-TEXTS.
           05  WS-JT-TEXT OCCURS 3 TIMES PIC X(30).
       01  WS-RUN-TOTALS.
           05  WS-RT-PAGES             PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-LINES             PIC S9(11) COMP-3 VALUE 0.
           05  WS-RT-VARIANCE          PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-TOTAL             PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-RT-TAX               PIC S9(15) COMP-3 VALUE 0.
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
                       PARM-FILE
           OPEN OUTPUT PRINT-STREAM
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 6511 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-BHDRIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 6512 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-PRTOUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 6513 TO WS-AB-CODE
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
           DISPLAY '  SUMMARY STYLE ' WS-PE-SUMM-STYLE.

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
      * THE SUMMARY STYLE IS SUPPLIED BY THE SCHEDULER FROM THE
      * MEDIA PROFILE.  IT HAS NO DEFAULT.
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
           IF WS-PE-SUMM-STYLE = SPACES
               MOVE 6521 TO WS-AB-CODE
               MOVE 'SUMMARY STYLE NOT SUPPLIED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PE-TAX-DETAIL-SW NOT = 'Y'
               MOVE 'N' TO WS-PE-TAX-DETAIL-SW.
           IF WS-PE-JURIS-SHOW-SW NOT = 'Y'
               MOVE 'N' TO WS-PE-JURIS-SHOW-SW.

       P1200-EXIT.
           EXIT.

      *****************************************************************
      * S200-MAIN PROCESSING                                          *
      * ONE SUMMARY PAGE PER INVOICE.  THE PAGE IS BUILT FROM THE     *
      * INVOICE HEADER ALONE - THE DETAIL FILE IS NOT READ HERE.      *
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
           IF NOT BH-FINAL
               ADD 1 TO WS-CFWD-CNT
               GO TO P2000-EXIT.
           PERFORM P3000-BUILD-SUMMARY THRU P3000-EXIT.
           ADD 1 TO WS-SUMM-CNT.

       P2000-EXIT.
           EXIT.

       P2100-READ-HEADER.
           MOVE 'P2100-READ-HEADER' TO WS-PARA-NAME.
           READ BHDR-IN-FILE INTO CABS-BILL-HEADER
               AT END
                   MOVE 'Y' TO WS-HDR-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00' AND WS-FS-INPUT NOT = '10'
               MOVE 6501 TO WS-AB-CODE
               MOVE 'BILL HEADER READ ERROR' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.

       P2100-EXIT.
           EXIT.

      *****************************************************************
      * S300-SUMMARY PAGE                                             *
      *****************************************************************
       S300-SUMMARY SECTION.

       P3000-BUILD-SUMMARY.
      * THE SUMMARY PAGE.  THE STYLE DECIDES HOW MUCH DETAIL APPEARS.
      * THE SUMMARY CONDITION IS TESTED BEFORE THE RECAP CONDITION.
           MOVE 'P3000-BUILD-SUMMARY' TO WS-PARA-NAME.
           MOVE WS-PE-SUMM-STYLE TO WS-SP-STYLE.
           PERFORM P3100-PAGE-HEAD THRU P3100-EXIT.
           IF WS-SP-SUMMARY
               PERFORM P3200-SUMMARY-LINES THRU P3200-EXIT
               GO TO P3000-TOTALS.
           IF WS-SP-RECAP
               PERFORM P3300-RECAP-LINES THRU P3300-EXIT
               GO TO P3000-TOTALS.
           PERFORM P3200-SUMMARY-LINES THRU P3200-EXIT.

       P3000-TOTALS.
           IF WS-PE-JURIS-SHOW-SW = 'Y'
               PERFORM P3400-JURIS-LINES THRU P3400-EXIT.
           PERFORM P3500-TOTAL-LINE THRU P3500-EXIT.
           PERFORM P3600-PROVE-SUMMARY THRU P3600-EXIT.
           ADD 1 TO WS-RT-PAGES.

       P3000-EXIT.
           EXIT.

       P3100-PAGE-HEAD.
      * THE SUMMARY PAGE ALWAYS STARTS A NEW INVOICE IN THE BURST
      * STREAM BECAUSE IT IS PRODUCED AS A SEPARATE DOCUMENT AND
      * MERGED IN FRONT OF THE DETAIL PAGES BY THE INSERTER.
      * CARRIAGE CONTROL PER CABS-STD-063 AND THE MAILROOM SPEC.
           MOVE 'P3100-PAGE-HEAD' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '7' TO PC-CC.
           MOVE BH-INVOICE-NBR TO PC-COL-001-020.
           MOVE 'SUMMARY OF CHARGES' TO PC-COL-021-060.
           MOVE BH-BAN TO PC-COL-061-090.
           PERFORM P4000-WRITE-LINE THRU P4000-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '0' TO PC-CC.
           MOVE ALL '=' TO PC-TEXT.
           PERFORM P4000-WRITE-LINE THRU P4000-EXIT.

       P3100-EXIT.
           EXIT.

       P3200-SUMMARY-LINES.
      * THE NINE COMPONENT LINES IN TARIFF ORDER.
           MOVE 'P3200-SUMMARY-LINES' TO WS-PARA-NAME.
           PERFORM P3250-ONE-LINE THRU P3250-EXIT
               VARYING WS-SL-X FROM 1 BY 1
               UNTIL WS-SL-X > 9.

       P3200-EXIT.
           EXIT.

       P3250-ONE-LINE.
           SET WS-SUB1 TO WS-SL-X.
           PERFORM P3260-SELECT-AMOUNT THRU P3260-EXIT.
           IF WS-SM-AMOUNT = ZERO
               IF WS-SL-CODE (WS-SL-X) NOT = 'TX'
                   GO TO P3250-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-SL-TEXT (WS-SL-X) TO PC-AMT-DESC.
           MOVE WS-SM-AMOUNT TO PC-AMT-VALUE.
           PERFORM P4000-WRITE-LINE THRU P4000-EXIT.

       P3250-EXIT.
           EXIT.

       P3260-SELECT-AMOUNT.
      * PICK THE HEADER FIELD THIS SUMMARY LINE DRAWS FROM.  THE TAX
      * LINE IS SHOWN TO THE WHOLE DOLLAR ON THE SUMMARY PAGE - THE
      * CENTS APPEAR ONLY IN THE TAX SECTION OF THE DETAIL.  THE MOVE
      * INTO THE WHOLE DOLLAR FIELD TRUNCATES.
      * PRECISION AGREED WITH REVENUE ACCOUNTING, CR-2907.
           MOVE ZERO TO WS-SM-AMOUNT.
           IF WS-SL-CODE (WS-SL-X) = 'PB'
               MOVE BH-PRIOR-BAL TO WS-SM-AMOUNT.
           IF WS-SL-CODE (WS-SL-X) = 'PY'
               COMPUTE WS-SM-AMOUNT = BH-PAYMENTS * -1.
           IF WS-SL-CODE (WS-SL-X) = 'AJ'
               MOVE BH-ADJUSTMENTS TO WS-SM-AMOUNT.
           IF WS-SL-CODE (WS-SL-X) = 'RS'
               MOVE BH-RESTATEMENT TO WS-SM-AMOUNT.
           IF WS-SL-CODE (WS-SL-X) = 'US'
               MOVE BH-CURR-USAGE TO WS-SM-AMOUNT.
           IF WS-SL-CODE (WS-SL-X) = 'RC'
               MOVE BH-CURR-RECURRING TO WS-SM-AMOUNT.
           IF WS-SL-CODE (WS-SL-X) = 'NR'
               MOVE BH-CURR-NONRECUR TO WS-SM-AMOUNT.
           IF WS-SL-CODE (WS-SL-X) = 'ST'
               MOVE BH-SETTLEMENT-NET TO WS-SM-AMOUNT.
           IF WS-SL-CODE (WS-SL-X) = 'TX'
               MOVE BH-TAX TO WS-SM-TAX-RAW
               MOVE WS-SM-TAX-RAW TO WS-SM-TAX-SHOWN
               MOVE WS-SM-TAX-SHOWN TO WS-SM-AMOUNT
               ADD WS-SM-TAX-SHOWN TO WS-RT-TAX.

       P3260-EXIT.
           EXIT.

       P3300-RECAP-LINES.
      * THE SHORT RECAP.  THREE LINES ONLY - WHAT WAS OWED, WHAT WAS
      * PAID AND WHAT IS OWED NOW.  USED FOR THE ELECTRONIC MEDIA
      * CUSTOMERS WHO GET THE DETAIL ON TAPE.
           MOVE 'P3300-RECAP-LINES' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-SL-TEXT (1) TO PC-AMT-DESC.
           MOVE BH-PRIOR-BAL TO PC-AMT-VALUE.
           PERFORM P4000-WRITE-LINE THRU P4000-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-SL-TEXT (2) TO PC-AMT-DESC.
           COMPUTE WS-SM-AMOUNT = BH-PAYMENTS * -1.
           MOVE WS-SM-AMOUNT TO PC-AMT-VALUE.
           PERFORM P4000-WRITE-LINE THRU P4000-EXIT.

       P3300-EXIT.
           EXIT.

       P3400-JURIS-LINES.
      * THE JURISDICTIONAL SPLIT OF THE CURRENT PERIOD USAGE.  SHOWN
      * ONLY WHERE THE CARRIER HAS ASKED FOR IT.
           MOVE 'P3400-JURIS-LINES' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '0' TO PC-CC.
           MOVE 'JURISDICTIONAL SPLIT OF USAGE' TO PC-AMT-DESC.
           PERFORM P4000-WRITE-LINE THRU P4000-EXIT.
           PERFORM P3450-ONE-JURIS THRU P3450-EXIT
               VARYING WS-SUB2 FROM 1 BY 1
               UNTIL WS-SUB2 > 3.

       P3400-EXIT.
           EXIT.

       P3450-ONE-JURIS.
           MOVE ZERO TO WS-SM-AMOUNT.
           IF WS-SUB2 = 1
               MOVE BH-INTERSTATE-AMT TO WS-SM-AMOUNT.
           IF WS-SUB2 = 2
               MOVE BH-INTRASTATE-AMT TO WS-SM-AMOUNT.
           IF WS-SUB2 = 3
               MOVE BH-LOCAL-AMT TO WS-SM-AMOUNT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-JT-TEXT (WS-SUB2) TO PC-AMT-DESC.
           MOVE WS-SM-AMOUNT TO PC-AMT-VALUE.
           PERFORM P4000-WRITE-LINE THRU P4000-EXIT.

       P3450-EXIT.
           EXIT.

       P3500-TOTAL-LINE.
           MOVE 'P3500-TOTAL-LINE' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '-' TO PC-CC.
           MOVE WS-SL-TEXT (10) TO PC-AMT-DESC.
           MOVE BH-TOTAL-DUE TO PC-AMT-VALUE.
           PERFORM P4000-WRITE-LINE THRU P4000-EXIT.
           ADD BH-TOTAL-DUE TO WS-RT-TOTAL.
           ADD BH-TOTAL-DUE TO WS-ACC-AMOUNT.

       P3500-EXIT.
           EXIT.

       P3600-PROVE-SUMMARY.
      * PROVE THE SUMMARY PAGE.  THE NINE COMPONENTS MUST ADD BACK TO
      * THE TOTAL DUE.  THE TAX LINE IS SHOWN TO THE WHOLE DOLLAR, SO
      * THE PROOF IS MADE AGAINST THE UNTRUNCATED TAX FIGURE.
           MOVE 'P3600-PROVE-SUMMARY' TO WS-PARA-NAME.
           COMPUTE WS-SM-CHECK =
                   BH-PRIOR-BAL - BH-PAYMENTS + BH-ADJUSTMENTS
                 + BH-RESTATEMENT + BH-CURR-USAGE
                 + BH-CURR-RECURRING + BH-CURR-NONRECUR
                 + BH-SETTLEMENT-NET + BH-TAX.
           COMPUTE WS-SM-VARIANCE = WS-SM-CHECK - BH-TOTAL-DUE.
           IF WS-SM-VARIANCE = ZERO
               GO TO P3600-EXIT.
           ADD 1 TO WS-RT-VARIANCE.
           MOVE EC-OUT-OF-BALANCE TO WS-ERR-CODE.
           MOVE 'W' TO WS-ERR-SEVERITY.
           MOVE CABS-BILL-HEADER TO WS-EW-DATA.
           PERFORM P7000-SUSPEND THRU P7000-EXIT.
           MOVE SPACES TO WS-ERR-CODE.

       P3600-EXIT.
           EXIT.

      *****************************************************************
      * S400-OUTPUT                                                   *
      *****************************************************************
       S400-OUTPUT SECTION.

       P4000-WRITE-LINE.
           MOVE 'P4000-WRITE-LINE' TO WS-PARA-NAME.
           WRITE PRINT-LINE FROM CABS-PRINT-LINE.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 6502 TO WS-AB-CODE
               MOVE 'SUMMARY PAGE WRITE FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-WRITE-CNT.
           ADD 1 TO WS-RT-LINES.

       P4000-EXIT.
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
           MOVE 'CABFMT05  SUMMARY PAGE REGISTER'
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
           MOVE 'PAGES        LINES           VARIANCES'
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
           MOVE 520                    TO CT-STEP-SEQ.
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
           DISPLAY 'SUMMARY PAGES     ' WS-RT-PAGES.
           DISPLAY 'LINES WRITTEN     ' WS-RT-LINES.
           DISPLAY 'SUMMARY VARIANCES ' WS-RT-VARIANCE.
           DISPLAY 'TOTAL DUE SUM     ' WS-RT-TOTAL.
           DISPLAY 'TAX SHOWN WHOLE   ' WS-RT-TAX.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE BHDR-IN-FILE
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

