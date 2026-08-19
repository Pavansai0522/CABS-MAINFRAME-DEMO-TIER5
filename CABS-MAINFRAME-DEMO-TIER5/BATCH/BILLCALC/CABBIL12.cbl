      *****************************************************************
      * CABBIL12 - FINAL INVOICE NUMBERING                            *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               BHDRIN  TELCABS.CABS.BILLHDR.AUD(0)       CABSBHDR*
      *               INVCTL  TELCABS.CABS.INVCTL               (LOCAL)*
      *               SYSIN   INSTREAM CONTROL CARD             (LOCAL)*
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               BHDROUT TELCABS.CABS.BILLHDR.FIN(+1)      CABSBHDR*
      *               INVCTL  TELCABS.CABS.INVCTL               (LOCAL)*
      *               REPORT  SYSOUT PRINT                      CABSPRNT*
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - NUMBERS ARE REISSUED, CONTROL FILE UNCHANGED*
      * REVISION HISTORY                                              *
      *   V1.00  1988-01-25  R.T.WHEELER  INITIAL RELEASE - FOURTEEN BYTE*
      *                      NUMBER AGREED WITH THE CARRIERS          *
      *   V1.02  1990-04-17  D.OKONKWO    MODULUS ELEVEN CHECK CHARACTER*
      *                      ADDED AT THE CARRIERS REQUEST            *
      *   V1.06  1993-12-07  M.J.FERRARO  SEQUENCE MADE PER CARRIER RATHER*
      *                      THAN PER RUN                             *
      *   V1.09  1996-07-30  J.M.CASTILLO Y2K - THE YYMM PIECE CONFIRMED AS*
      *                      PRESENTATION ONLY, NOT USED IN ANY       *
      *                      DATE COMPARISON                          *
      *   V2.00  2002-01-14  P.NAIR       CONTROL FILE REWRITTEN AT END OF*
      *                      RUN SO A FAILED RUN REISSUES             *
      *   V2.04  2009-06-23  A.BUKOWSKI   RENUMBER SWITCH ADDED FOR THE*
      *                      CANCEL AND REISSUE PROCESS               *
      *   V2.07  2019-05-13  G.PRZYBYLSKI RECOMPILE - NO FUNCTIONAL CHANGE*
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABBIL12.
       AUTHOR. TELCABS APPLICATIONS - BILLING TEAM.
      *****************************************************************
      * ISSUES THE FINAL INVOICE NUMBER AND MAKES THE INVOICE FINAL.  *
      * THE NUMBER IS ASSEMBLED PIECE BY PIECE AND CARRIES A MODULUS  *
      * ELEVEN CHECK CHARACTER THE CARRIERS VERIFY ON ARRIVAL.        *
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
           SELECT INV-CTL-FILE ASSIGN TO DA-I-INVCTL
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS IV-KEY
               FILE STATUS IS WS-FS-TABLE.
           SELECT BHDR-OUT-FILE ASSIGN TO UT-S-BHDROUT
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
      * BHDRIN - THE AUDITED INVOICE HEADER.                          *
      *****************************************************************
       FD  BHDR-IN-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 400 CHARACTERS.
       01  BHDR-IN-REC                      PIC X(400).
      *****************************************************************
      * INVCTL - INVOICE NUMBER CONTROL.  VSAM KSDS DEFINED BY AN     *
      * IDCAMS JOB.  READ AND REWRITTEN BY THIS PROGRAM ONLY.         *
      *****************************************************************
       FD  INV-CTL-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 60 CHARACTERS.
       01  CABS-INVCTL-RECORD.
           05  IV-KEY.
               10  IV-OCN              PIC X(04).
           05  IV-LAST-SEQ             PIC 9(06).
           05  IV-LAST-YYDDD           PIC 9(05).
           05  IV-LAST-RUN             PIC X(12).
           05  IV-PREFIX               PIC X(02).
           05  IV-FILLER               PIC X(31).
      *****************************************************************
      * BHDROUT - THE FINAL NUMBERED INVOICE HEADER.                  *
      *****************************************************************
       FD  BHDR-OUT-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 400 CHARACTERS.
       01  BHDR-OUT-REC                     PIC X(400).
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
           05  WS-PGM-NAME             PIC X(08) VALUE 'CABBIL12'.
           05  WS-PGM-VERSION          PIC X(05) VALUE 'V2.07'.
           05  WS-PGM-APPL             PIC X(04) VALUE 'CABS'.
           05  WS-PGM-COMPILE          PIC X(08) VALUE '20190513'.
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
           05  WS-PE-PREFIX            PIC X(02).
           05  WS-PE-SEQ-START         PIC 9(06).
           05  WS-PE-CHECK-SW          PIC X(01).
           05  WS-PE-RENUM-SW          PIC X(01).
           05  WS-PE-FILLER            PIC X(25).
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
           05  WS-CTL-FOUND-SW         PIC X(01) VALUE 'N'.
               88  WS-CTL-FOUND        VALUE 'Y'.
           05  WS-NUMBER-SW            PIC X(01) VALUE 'N'.
               88  WS-NUMBER-IT        VALUE 'Y'.
      *****************************************************************
      * THE INVOICE NUMBER.  FOURTEEN BYTES ASSEMBLED FROM FIVE PIECES.*
      * THE FORMAT IS FIXED BY THE CARRIER INTERFACE AGREEMENT AND HAS*
      * NOT CHANGED SINCE 1988.  IT IS BUILT PIECE BY PIECE RATHER THAN*
      * BY A SINGLE MOVE BECAUSE THE SEQUENCE IS ZERO SUPPRESSED IN THE*
      * MIDDLE AND THE CHECK CHARACTER IS DERIVED FROM WHAT COMES     *
      * BEFORE IT.                                                    *
      *****************************************************************
       01  WS-INV-PIECES.
           05  WS-IP-PREFIX            PIC X(02) VALUE SPACES.
           05  WS-IP-OCN               PIC X(04) VALUE SPACES.
           05  WS-IP-YYMM              PIC 9(04) VALUE 0.
           05  WS-IP-SEQ               PIC 9(06) VALUE 0.
           05  WS-IP-SEQ-EDIT          PIC X(06) VALUE SPACES.
           05  WS-IP-CHECK             PIC X(01) VALUE SPACES.
       01  WS-INV-NUMBER               PIC X(14) VALUE SPACES.
       01  WS-INV-NUMBER-R REDEFINES WS-INV-NUMBER.
           05  WS-IN-CHAR OCCURS 14 TIMES PIC X(01).
       01  WS-INV-NUMBER-P REDEFINES WS-INV-NUMBER.
           05  WS-IN-BODY              PIC X(13).
           05  WS-IN-CHECK             PIC X(01).
       01  WS-INV-CTL.
           05  WS-IC-PTR               PIC 9(03) VALUE 1.
           05  WS-IC-LEN               PIC 9(03) VALUE 0.
      *****************************************************************
      * CHECK CHARACTER DERIVATION.  A MODULUS ELEVEN WEIGHTED SUM OVER*
      * THE THIRTEEN CHARACTERS THAT PRECEDE IT.  NON NUMERIC BYTES   *
      * CONTRIBUTE THEIR POSITION IN THE ALPHABET.  THE SCHEME WAS    *
      * AGREED WITH THE CARRIERS IN 1988 AND IS CHECKED BY THEIR      *
      * RECEIVABLES SYSTEMS ON ARRIVAL.                               *
      * EDITING RULES ARE HELD WITH THE BILL FORMAT SPECIFICATION.    *
      *****************************************************************
       01  WS-CHECK-WORK.
           05  WS-CK-SUM               PIC S9(09) COMP-3 VALUE 0.
           05  WS-CK-WEIGHT            PIC S9(03) COMP-3 VALUE 0.
           05  WS-CK-VALUE             PIC S9(03) COMP-3 VALUE 0.
           05  WS-CK-REMAINDER         PIC S9(03) COMP-3 VALUE 0.
           05  WS-CK-QUOTIENT          PIC S9(09) COMP-3 VALUE 0.
           05  WS-CK-SUB               PIC S9(03) COMP-3 VALUE 0.
           05  WS-CK-DIGIT-CNT         PIC S9(03) COMP-3 VALUE 0.
           05  WS-CK-ALPHA-CNT         PIC S9(03) COMP-3 VALUE 0.
       01  WS-CHECK-CHAR-TABLE.
           05  FILLER PIC X(11) VALUE '0123456789X'.
       01  WS-CHECK-CHAR-R REDEFINES WS-CHECK-CHAR-TABLE.
           05  WS-CC-CHAR OCCURS 11 TIMES PIC X(01).
       01  WS-ALPHA-TABLE.
           05  FILLER PIC X(26) VALUE
               'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
       01  WS-ALPHA-TABLE-R REDEFINES WS-ALPHA-TABLE.
           05  WS-AL-CHAR OCCURS 26 TIMES PIC X(01).
      *****************************************************************
      * THE INVOICE NUMBER CONTROL FILE.  ONE RECORD PER CARRIER WITH *
      * THE LAST NUMBER ISSUED.  VSAM KSDS DEFINED BY AN IDCAMS JOB IN*
      * THE VSAM LIBRARY - NOTHING ELSE IN THE ESTATE CREATES IT.     *
      * SPACE AND SHAREOPTIONS ARE OWNED BY STORAGE MANAGEMENT.       *
      *****************************************************************
       01  WS-SEQ-TABLE.
           05  WS-SQ-ENTRY OCCURS 500 TIMES INDEXED BY WS-SQ-X.
               10  WS-SQ-OCN           PIC X(04).
               10  WS-SQ-LAST          PIC 9(06).
               10  WS-SQ-ISSUED        PIC S9(07) COMP-3.
       01  WS-SEQ-CTL.
           05  WS-SQ-USED              PIC S9(05) COMP-3 VALUE 0.
           05  WS-SQ-MAX               PIC S9(05) COMP-3 VALUE 500.
           05  WS-SQ-HIT               PIC S9(05) COMP-3 VALUE 0.
       01  WS-PERIOD-WORK.
           05  WS-PW-YY                PIC 9(02) VALUE 0.
           05  WS-PW-MM                PIC 9(02) VALUE 0.
           05  WS-PW-CC                PIC 9(02) VALUE 0.
       01  WS-RUN-TOTALS.
           05  WS-RT-HEADERS           PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-NUMBERED          PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-HELD              PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-NEW-CARRIER       PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-FINAL-AMT         PIC S9(15)V9(02) COMP-3 VALUE 0.
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
           OPEN OUTPUT BHDR-OUT-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 5211 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-BHDRIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 5212 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-BHDROUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 5213 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-CTLOUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           MOVE WS-ACCEPT-DATE         TO WS-AD-WORK.
           MOVE WS-AD-YY               TO DW-CUR-YY.
           PERFORM P1100-READ-PARM THRU P1100-EXIT.
           PERFORM P1200-EDIT-PARM THRU P1200-EXIT.
           OPEN I-O    INV-CTL-FILE.
           IF WS-FS-TABLE NOT = '00'
               MOVE 5214 TO WS-AB-CODE
               MOVE 'OPEN I-O FAILED DA-I-INVCTL' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
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
           PERFORM P4000-LOAD-SEQUENCE THRU P4000-EXIT.
           PERFORM P6000-HEADING THRU P6000-EXIT.
           DISPLAY WS-PGM-NAME ' STARTED - RUN ' WS-RUN-ID.
           DISPLAY '  NUMBER PREFIX ' WS-PE-PREFIX.
           DISPLAY '  SEQ START     ' WS-PE-SEQ-START.
           DISPLAY '  CHECK CHAR SW ' WS-PE-CHECK-SW.

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
      * THE INVOICE PREFIX IS SUPPLIED BY THE SCHEDULER FROM THE
      * REGIONAL NUMBERING STANDARD.  IT IS NOT CODED IN THE JCL AND
      * AN ABSENT PREFIX STOPS THE STEP.
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
           IF WS-PE-PREFIX = SPACES
               MOVE 5221 TO WS-AB-CODE
               MOVE 'INVOICE PREFIX NOT SUPPLIED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PE-SEQ-START NOT NUMERIC
               MOVE ZERO TO WS-PE-SEQ-START.
           IF WS-PE-CHECK-SW NOT = 'N'
               MOVE 'Y' TO WS-PE-CHECK-SW.
           IF WS-PE-RENUM-SW NOT = 'Y'
               MOVE 'N' TO WS-PE-RENUM-SW.

       P1200-EXIT.
           EXIT.

      *****************************************************************
      * S200-MAIN PROCESSING                                          *
      * THE LAST STEP OF THE BILL CALCULATION STREAM.  AN INVOICE THAT*
      * REACHES THIS PROGRAM AND IS NOT HELD BECOMES FINAL AND IS GIVEN*
      * ITS NUMBER.  ONCE NUMBERED IT CANNOT BE RECALCULATED - ONLY   *
      * CANCELLED AND REISSUED.                                       *
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
           PERFORM P3000-ELIGIBILITY THRU P3000-EXIT.
           IF WS-NUMBER-IT
               PERFORM P3100-NEXT-SEQUENCE THRU P3100-EXIT
               PERFORM P3200-BUILD-INVOICE-NBR THRU P3200-EXIT
               PERFORM P3500-SET-FINAL THRU P3500-EXIT
           ELSE
               ADD 1 TO WS-RT-HELD
               ADD 1 TO WS-CFWD-CNT.
           PERFORM P3600-WRITE-HEADER THRU P3600-EXIT.

       P2000-EXIT.
           EXIT.

       P2100-READ-HEADER.
           MOVE 'P2100-READ-HEADER' TO WS-PARA-NAME.
           READ BHDR-IN-FILE INTO CABS-BILL-HEADER
               AT END
                   MOVE 'Y' TO WS-HDR-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00' AND WS-FS-INPUT NOT = '10'
               MOVE 5201 TO WS-AB-CODE
               MOVE 'BILL HEADER READ ERROR' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.

       P2100-EXIT.
           EXIT.

      *****************************************************************
      * S300-NUMBERING                                                *
      *****************************************************************
       S300-NUMBER SECTION.

       P3000-ELIGIBILITY.
      * A HELD INVOICE IS NOT NUMBERED.  A CANCELLED ONE IS NOT EITHER.
      * AN INVOICE THAT ALREADY CARRIES A NUMBER KEEPS IT UNLESS THE
      * OPERATOR ASKED FOR A RENUMBER RUN.
           MOVE 'P3000-ELIGIBILITY' TO WS-PARA-NAME.
           MOVE 'Y' TO WS-NUMBER-SW.
           IF BH-HELD
               MOVE 'N' TO WS-NUMBER-SW
               GO TO P3000-EXIT.
           IF BH-CANCELLED
               MOVE 'N' TO WS-NUMBER-SW
               GO TO P3000-EXIT.
           IF BH-INVOICE-NBR NOT = SPACES
               IF WS-PE-RENUM-SW NOT = 'Y'
                   MOVE 'N' TO WS-NUMBER-SW.

       P3000-EXIT.
           EXIT.

       P3100-NEXT-SEQUENCE.
      * TAKE THE NEXT NUMBER IN SEQUENCE FOR THIS CARRIER.  A CARRIER
      * THAT HAS NEVER BEEN BILLED STARTS AT THE VALUE SUPPLIED ON THE
      * CONTROL CARD.  THE SEQUENCE IS PER CARRIER, NOT PER ACCOUNT -
      * THE CARRIER RECEIVABLES SYSTEMS EXPECT A CONTINUOUS RANGE.
           MOVE 'P3100-NEXT-SEQUENCE' TO WS-PARA-NAME.
           MOVE 'N' TO WS-CTL-FOUND-SW.
           MOVE ZERO TO WS-SQ-HIT.
           PERFORM P3110-MATCH-OCN THRU P3110-EXIT
               VARYING WS-SQ-X FROM 1 BY 1
               UNTIL WS-SQ-X > WS-SQ-USED OR WS-CTL-FOUND.
           IF WS-CTL-FOUND
               GO TO P3105-BUMP.
           IF WS-SQ-USED NOT < WS-SQ-MAX
               MOVE 5202 TO WS-AB-CODE
               MOVE 'INVOICE SEQUENCE TABLE FULL' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-SQ-USED.
           MOVE WS-SQ-USED TO WS-SQ-HIT.
           SET WS-SQ-X TO WS-SQ-USED.
           MOVE BH-OCN TO WS-SQ-OCN (WS-SQ-X).
           MOVE WS-PE-SEQ-START TO WS-SQ-LAST (WS-SQ-X).
           MOVE ZERO TO WS-SQ-ISSUED (WS-SQ-X).
           ADD 1 TO WS-RT-NEW-CARRIER.

       P3105-BUMP.
           SET WS-SQ-X TO WS-SQ-HIT.
           ADD 1 TO WS-SQ-LAST (WS-SQ-X).
           ADD 1 TO WS-SQ-ISSUED (WS-SQ-X).
           MOVE WS-SQ-LAST (WS-SQ-X) TO WS-IP-SEQ.

       P3100-EXIT.
           EXIT.

       P3110-MATCH-OCN.
           IF WS-SQ-OCN (WS-SQ-X) = BH-OCN
               SET WS-SUB1 TO WS-SQ-X
               MOVE WS-SUB1 TO WS-SQ-HIT
               MOVE 'Y' TO WS-CTL-FOUND-SW.

       P3110-EXIT.
           EXIT.

       P3200-BUILD-INVOICE-NBR.
      * ASSEMBLE THE FOURTEEN BYTE INVOICE NUMBER FROM ITS PIECES.  THE
      * LAYOUT IS PREFIX(2) OCN(4) YYMM(4) SEQUENCE(3 LOW ORDER) AND
      * THE CHECK CHARACTER.  THE SEQUENCE IS CARRIED AT SIX DIGITS
      * INTERNALLY AND ONLY THE LOW ORDER THREE APPEAR IN THE NUMBER -
      * THE FORMAT WAS AGREED WHEN NO CARRIER BILLED MORE THAN A
      * THOUSAND ACCOUNTS IN A MONTH.
           MOVE 'P3200-BUILD-INVOICE-NBR' TO WS-PARA-NAME.
           PERFORM P3300-PERIOD-PIECE THRU P3300-EXIT.
           MOVE WS-PE-PREFIX TO WS-IP-PREFIX.
           MOVE BH-OCN TO WS-IP-OCN.
           MOVE WS-IP-SEQ TO WS-IP-SEQ-EDIT.
           MOVE SPACES TO WS-INV-NUMBER.
           MOVE 1 TO WS-IC-PTR.
           STRING WS-IP-PREFIX     DELIMITED BY SIZE
                  WS-IP-OCN        DELIMITED BY SIZE
                  WS-IP-YYMM       DELIMITED BY SIZE
                  WS-IP-SEQ-EDIT   DELIMITED BY SIZE
                  INTO WS-INV-NUMBER
                  WITH POINTER WS-IC-PTR
               ON OVERFLOW
                  MOVE 5203 TO WS-AB-CODE
                  MOVE 'INVOICE NUMBER OVERFLOW' TO WS-AB-TEXT
                  PERFORM P9500-ABEND THRU P9500-EXIT.
           COMPUTE WS-IC-LEN = WS-IC-PTR - 1.
           PERFORM P3400-CHECK-CHARACTER THRU P3400-EXIT.
           MOVE WS-IP-CHECK TO WS-IN-CHECK.

       P3200-EXIT.
           EXIT.

       P3300-PERIOD-PIECE.
      * THE FOUR DIGIT YYMM PIECE COMES FROM THE BILL PERIOD, WHICH IS
      * HELD AS YYMMCC.  THE CYCLE NUMBER IS DROPPED - TWO ACCOUNTS ON
      * DIFFERENT CYCLES IN THE SAME MONTH SHARE THE SAME PIECE AND ARE
      * DISTINGUISHED ONLY BY THE SEQUENCE.
           MOVE 'P3300-PERIOD-PIECE' TO WS-PARA-NAME.
           MOVE BH-BILL-PERIOD TO WS-PERIOD-WORK.
           MOVE WS-PW-YY TO WS-IP-YYMM.
           COMPUTE WS-IP-YYMM = (WS-PW-YY * 100) + WS-PW-MM.

       P3300-EXIT.
           EXIT.

       P3400-CHECK-CHARACTER.
      * WALK THE THIRTEEN CHARACTERS OF THE NUMBER AND BUILD A MODULUS
      * ELEVEN WEIGHTED SUM.  A DIGIT CONTRIBUTES ITS VALUE; A LETTER
      * CONTRIBUTES ITS POSITION IN THE ALPHABET.  THE WEIGHT RUNS FROM
      * FOURTEEN DOWN TO TWO.  THE REMAINDER SELECTS THE CHECK
      * CHARACTER FROM THE ELEVEN CHARACTER TABLE.
      * AGREED WITH THE BILL PRINT VENDOR IN 1997.
           MOVE 'P3400-CHECK-CHARACTER' TO WS-PARA-NAME.
           MOVE ZERO TO WS-CK-SUM WS-CK-DIGIT-CNT WS-CK-ALPHA-CNT.
           MOVE 14 TO WS-CK-WEIGHT.
           PERFORM P3410-ONE-CHARACTER THRU P3410-EXIT
               VARYING WS-CK-SUB FROM 1 BY 1
               UNTIL WS-CK-SUB > 13.
           DIVIDE WS-CK-SUM BY 11 GIVING WS-CK-QUOTIENT
               REMAINDER WS-CK-REMAINDER.
           COMPUTE WS-CK-SUB = WS-CK-REMAINDER + 1.
           IF WS-CK-SUB < 1 OR WS-CK-SUB > 11
               MOVE 11 TO WS-CK-SUB.
           MOVE WS-CC-CHAR (WS-CK-SUB) TO WS-IP-CHECK.
           IF WS-PE-CHECK-SW NOT = 'Y'
               MOVE '0' TO WS-IP-CHECK.

       P3400-EXIT.
           EXIT.

       P3410-ONE-CHARACTER.
           MOVE ZERO TO WS-CK-VALUE.
           IF WS-IN-CHAR (WS-CK-SUB) NOT < '0'
               IF WS-IN-CHAR (WS-CK-SUB) NOT > '9'
                   MOVE WS-IN-CHAR (WS-CK-SUB) TO WS-ED-PAGE-DATE
                   ADD 1 TO WS-CK-DIGIT-CNT.
           IF WS-CK-DIGIT-CNT > ZERO
               PERFORM P3420-DIGIT-VALUE THRU P3420-EXIT.
           IF WS-CK-VALUE = ZERO
               PERFORM P3430-ALPHA-VALUE THRU P3430-EXIT.
           COMPUTE WS-CK-SUM =
                   WS-CK-SUM + (WS-CK-VALUE * WS-CK-WEIGHT).
           SUBTRACT 1 FROM WS-CK-WEIGHT.

       P3410-EXIT.
           EXIT.

       P3420-DIGIT-VALUE.
           MOVE ZERO TO WS-CK-DIGIT-CNT.
           PERFORM P3425-SCAN-DIGIT THRU P3425-EXIT
               VARYING WS-SUB2 FROM 1 BY 1
               UNTIL WS-SUB2 > 10.

       P3420-EXIT.
           EXIT.

       P3425-SCAN-DIGIT.
           IF WS-CC-CHAR (WS-SUB2) = WS-IN-CHAR (WS-CK-SUB)
               COMPUTE WS-CK-VALUE = WS-SUB2 - 1.

       P3425-EXIT.
           EXIT.

       P3430-ALPHA-VALUE.
           PERFORM P3435-SCAN-ALPHA THRU P3435-EXIT
               VARYING WS-SUB3 FROM 1 BY 1
               UNTIL WS-SUB3 > 26.

       P3430-EXIT.
           EXIT.

       P3435-SCAN-ALPHA.
           IF WS-AL-CHAR (WS-SUB3) = WS-IN-CHAR (WS-CK-SUB)
               MOVE WS-SUB3 TO WS-CK-VALUE
               ADD 1 TO WS-CK-ALPHA-CNT.

       P3435-EXIT.
           EXIT.

       P3500-SET-FINAL.
      * THE INVOICE BECOMES FINAL.  THE DUE DATE IS CONFIRMED FROM THE
      * CYCLE DATE AND THE TERMS ALREADY ON THE HEADER.
           MOVE 'P3500-SET-FINAL' TO WS-PARA-NAME.
           MOVE WS-INV-NUMBER TO BH-INVOICE-NBR.
           MOVE 'F' TO BH-STATUS.
           MOVE SPACES TO BH-HOLD-REASON.
           MOVE WS-CYCLE-YYDDD TO BH-BILL-YYDDD.
           ADD 1 TO WS-RT-NUMBERED.
           ADD BH-TOTAL-DUE TO WS-RT-FINAL-AMT.
           PERFORM P5000-REGISTER-LINE THRU P5000-EXIT.

       P3500-EXIT.
           EXIT.

       P3600-WRITE-HEADER.
           MOVE 'P3600-WRITE-HEADER' TO WS-PARA-NAME.
           WRITE BHDR-OUT-REC FROM CABS-BILL-HEADER.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 5204 TO WS-AB-CODE
               MOVE 'FINAL HEADER WRITE FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-WRITE-CNT.
           ADD 1 TO WS-RT-HEADERS.
           ADD BH-TOTAL-DUE TO WS-ACC-AMOUNT.

       P3600-EXIT.
           EXIT.

      *****************************************************************
      * S400-SEQUENCE CONTROL FILE                                    *
      *****************************************************************
       S400-SUPPORT SECTION.

       P4000-LOAD-SEQUENCE.
      * LOAD THE INVOICE NUMBER CONTROL FILE.  IT IS A VSAM KSDS THAT
      * NO PROGRAM IN THE ESTATE CREATES - IT IS DEFINED BY AN IDCAMS
      * JOB AND REBUILT BY HAND WHEN A CARRIER IS ADDED.
           MOVE 'P4000-LOAD-SEQUENCE' TO WS-PARA-NAME.
           MOVE ZERO TO WS-SQ-USED.
           MOVE LOW-VALUES TO IV-KEY.
           START INV-CTL-FILE KEY NOT LESS THAN IV-KEY
               INVALID KEY
                   MOVE 5205 TO WS-AB-CODE
                   MOVE 'INVOICE CONTROL FILE UNREADABLE'
                                       TO WS-AB-TEXT
                   PERFORM P9500-ABEND THRU P9500-EXIT.
           PERFORM P4010-READ-SEQ THRU P4010-EXIT
               UNTIL WS-FS-TABLE NOT = '00'.
           DISPLAY 'SEQUENCE ENTRIES ' WS-SQ-USED.

       P4000-EXIT.
           EXIT.

       P4010-READ-SEQ.
           READ INV-CTL-FILE NEXT RECORD
               AT END
                   MOVE '10' TO WS-FS-TABLE
                   GO TO P4010-EXIT.
           IF WS-FS-TABLE NOT = '00'
               GO TO P4010-EXIT.
           IF WS-SQ-USED NOT < WS-SQ-MAX
               GO TO P4010-EXIT.
           ADD 1 TO WS-SQ-USED.
           SET WS-SQ-X TO WS-SQ-USED.
           MOVE IV-OCN      TO WS-SQ-OCN (WS-SQ-X).
           MOVE IV-LAST-SEQ TO WS-SQ-LAST (WS-SQ-X).
           MOVE ZERO        TO WS-SQ-ISSUED (WS-SQ-X).

       P4010-EXIT.
           EXIT.

       P4100-REWRITE-SEQUENCE.
      * PUT THE LAST NUMBER ISSUED BACK ON THE CONTROL FILE.  THIS IS
      * DONE ONCE AT END OF RUN, NOT AS EACH NUMBER IS TAKEN - A FAILED
      * RUN THEREFORE LEAVES THE CONTROL FILE AS IT WAS AND THE NUMBERS
      * ARE REISSUED ON THE RERUN.
           MOVE 'P4100-REWRITE-SEQUENCE' TO WS-PARA-NAME.
           PERFORM P4110-ONE-REWRITE THRU P4110-EXIT
               VARYING WS-SQ-X FROM 1 BY 1
               UNTIL WS-SQ-X > WS-SQ-USED.

       P4100-EXIT.
           EXIT.

       P4110-ONE-REWRITE.
           MOVE WS-SQ-OCN (WS-SQ-X)  TO IV-OCN.
           MOVE WS-SQ-LAST (WS-SQ-X) TO IV-LAST-SEQ.
           MOVE WS-CYCLE-YYDDD       TO IV-LAST-YYDDD.
           MOVE WS-RUN-ID            TO IV-LAST-RUN.
           REWRITE CABS-INVCTL-RECORD
               INVALID KEY
                   WRITE CABS-INVCTL-RECORD
                       INVALID KEY
                           ADD 1 TO WS-REJECT-CNT.

       P4110-EXIT.
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
           MOVE BH-BAN         TO PC-COL-001-020.
           MOVE BH-INVOICE-NBR TO PC-COL-021-060.
           MOVE BH-OCN         TO PC-COL-061-090.
           MOVE BH-TOTAL-DUE   TO WS-ED-MONEY.
           MOVE WS-ED-MONEY    TO PC-COL-091-132.
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
           MOVE 'CABBIL12  FINAL INVOICE NUMBERING REGISTER'
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
           MOVE 'BAN                 INVOICE NUMBER          OCN   TO'
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
           MOVE 460                    TO CT-STEP-SEQ.
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
           PERFORM P4100-REWRITE-SEQUENCE THRU P4100-EXIT.
           DISPLAY 'HEADERS WRITTEN   ' WS-RT-HEADERS.
           DISPLAY 'INVOICES NUMBERED ' WS-RT-NUMBERED.
           DISPLAY 'HELD NOT NUMBERED ' WS-RT-HELD.
           DISPLAY 'NEW CARRIERS      ' WS-RT-NEW-CARRIER.
           DISPLAY 'FINAL BILLED VALUE' WS-RT-FINAL-AMT.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE BHDR-IN-FILE
                 INV-CTL-FILE
                 BHDR-OUT-FILE
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

