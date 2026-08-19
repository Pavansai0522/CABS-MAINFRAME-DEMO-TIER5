      *****************************************************************
      * CABBIL07 - TAX AND SURCHARGE CALCULATION                      *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               BHDRIN  TELCABS.CABS.BILLHDR.USG(0)       CABSBHDR*
      *               TAXMST  TELCABS.CABS.TAXRATE              (LOCAL)*
      *               SYSIN   INSTREAM CONTROL CARD             (LOCAL)*
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               BHDROUT TELCABS.CABS.BILLHDR.TAX(+1)      CABSBHDR*
      *               REPORT  SYSOUT PRINT                      CABSPRNT*
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY (BAN)           *
      * REVISION HISTORY                                              *
      *   V1.00  1988-08-08  L.HARGREAVES INITIAL RELEASE - FEDERAL EXCISE*
      *                      AT A HARDCODED THREE PER CENT            *
      *   V1.04  1991-06-24  M.J.FERRARO  STATE SALES TAX ADDED, RATES HELD*
      *                      ON A NEW VSAM MASTER                     *
      *   V1.07  1994-02-09  D.OKONKWO    FEDERAL SUBSCRIBER LINE SURCHARGE*
      *                      ADDED UNDER TYPE FS                      *
      *   V1.11  1996-10-15  J.M.CASTILLO COMPOUNDING STATES HANDLED - STATE*
      *                      TAX NOW APPLIES TO THE FEDERAL TAX       *
      *   V2.00  1999-07-20  P.NAIR       E911 SURCHARGE ADDED AS A FLAT*
      *                      AMOUNT PER BILL, NOT PER LINE            *
      *   V2.05  2003-12-02  A.BUKOWSKI   MAXIMUM TAX CAP HONOURED PER STATE*
      *   V2.09  2009-03-16  T.VANCE      EACH COMPONENT NOW ROUNDED ON ITS*
      *                      OWN - THE SUM OF THE ROUNDED PARTS       *
      *                      IS WHAT THE CARRIER PAYS                 *
      *   V3.00  2013-08-27  R.KAMINSKI   DOMINANT JURISDICTION RULE ADDED -*
      *                      A BILL IS TAXED UNDER ONE ONLY           *
      *   V3.02  2018-11-19  G.PRZYBYLSKI FEDERAL RATE OVERRIDE ACCEPTED FROM*
      *                      THE CONTROL CARD FOR RATE CHANGES        *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABBIL07.
       AUTHOR. TELCABS APPLICATIONS - BILLING TEAM.
      *****************************************************************
      * COMPUTES FEDERAL EXCISE, THE FEDERAL SURCHARGE, STATE SALES   *
      * TAX, THE LOCAL SURCHARGE AND E911 AND POSTS THE TOTAL TO THE  *
      * BILL HEADER.  EACH COMPONENT IS ROUNDED SEPARATELY.           *
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
           SELECT TAX-MASTER ASSIGN TO DA-I-TAXMST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS TX-KEY
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
      * BHDRIN - BILL HEADER WITH USAGE TOTALS POSTED.                *
      *****************************************************************
       FD  BHDR-IN-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 400 CHARACTERS.
       01  BHDR-IN-REC                      PIC X(400).
      *****************************************************************
      * TAXMST - THE TAX RATE MASTER.  VSAM KSDS.  NO PROGRAM IN      *
      * THE ESTATE CREATES OR WRITES THIS FILE - IT IS DEFINED BY     *
      * AN IDCAMS JOB AND MAINTAINED THROUGH A FILE-AID PANEL.        *
      *****************************************************************
       FD  TAX-MASTER
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 100 CHARACTERS.
       01  CABS-TAX-RECORD.
           05  TX-KEY.
               10  TX-STATE-CD         PIC X(02).
               10  TX-JURIS-CD         PIC X(01).
               10  TX-TAX-TYPE         PIC X(02).
           05  TX-RATE                 PIC S9(03)V9(05) COMP-3.
           05  TX-BASIS                PIC X(01).
               88  TX-PERCENTAGE       VALUE 'P'.
               88  TX-FLAT-AMOUNT      VALUE 'F'.
           05  TX-COMPOUND-SW          PIC X(01).
           05  TX-MIN-BASE             PIC S9(09)V9(02) COMP-3.
           05  TX-MAX-TAX              PIC S9(09)V9(02) COMP-3.
           05  TX-EFF-YYDDD            PIC 9(05).
           05  TX-EXP-YYDDD            PIC 9(05).
           05  TX-DESC                 PIC X(30).
           05  TX-AUTHORITY            PIC X(20).
           05  TX-FILLER               PIC X(16).
      *****************************************************************
      * BHDROUT - HEADER WITH THE TAX FIELD POPULATED.                *
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
           05  WS-PGM-NAME             PIC X(08) VALUE 'CABBIL07'.
           05  WS-PGM-VERSION          PIC X(05) VALUE 'V3.02'.
           05  WS-PGM-APPL             PIC X(04) VALUE 'CABS'.
           05  WS-PGM-COMPILE          PIC X(08) VALUE '20181119'.
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
           05  WS-PE-TAX-SW            PIC X(01).
           05  WS-PE-FED-RATE          PIC 9(03)V9(05).
           05  WS-PE-EXEMPT-SW         PIC X(01).
           05  WS-PE-TAX-YYDDD         PIC 9(05).
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
           05  WS-TAX-FOUND-SW         PIC X(01) VALUE 'N'.
               88  WS-TAX-FOUND        VALUE 'Y'.
           05  WS-EXEMPT-SW            PIC X(01) VALUE 'N'.
               88  WS-EXEMPT           VALUE 'Y'.
      *****************************************************************
      * TAX TYPE WORK.  THE RANGES ON THE TWO CONDITIONS BELOW WERE SET*
      * WHEN FEDERAL EXCISE AND THE FEDERAL SUBSCRIBER LINE SURCHARGE *
      * WERE FILED SEPARATELY IN 1991 AND 1994.  BOTH CONDITIONS ARE  *
      * TRUE FOR TYPE FS AND BOTH ARE TESTED IN P3400.                *
      * VALUES ARE SET BY THE TARIFF GROUP, NOT BY THIS PROGRAM.      *
      *****************************************************************
       01  WS-TAX-TYPE-WORK.
           05  WS-TX-TYPE              PIC X(02) VALUE SPACES.
               88  WS-TX-FEDERAL       VALUE 'FE' THRU 'FS'.
               88  WS-TX-SURCHARGE     VALUE 'FS' THRU 'SU'.
               88  WS-TX-STATE-TAX     VALUE 'ST'.
               88  WS-TX-LOCAL-TAX     VALUE 'LO'.
               88  WS-TX-E911          VALUE 'E9'.
               88  WS-TX-ANY-STATUTORY VALUE 'E9' THRU 'SU'.
           05  WS-TX-SUB               PIC S9(03) COMP-3 VALUE 0.
      *****************************************************************
      * THE TAX RATE TABLE, LOADED FROM THE VSAM TAX MASTER AT INIT.  *
      * THE MASTER IS TELCABS.CABS.TAXRATE AND IS DEFINED BY AN IDCAMS*
      * JOB IN THE VSAM LIBRARY.  NO PROGRAM IN THE ESTATE CREATES IT.*
      *****************************************************************
       01  WS-TAX-TABLE.
           05  WS-TT-ENTRY OCCURS 400 TIMES INDEXED BY WS-TT-X.
               10  WS-TT-STATE         PIC X(02).
               10  WS-TT-JURIS         PIC X(01).
               10  WS-TT-TYPE          PIC X(02).
               10  WS-TT-RATE          PIC S9(03)V9(05) COMP-3.
               10  WS-TT-BASIS         PIC X(01).
               10  WS-TT-COMPOUND      PIC X(01).
               10  WS-TT-MIN-BASE      PIC S9(09)V9(02) COMP-3.
               10  WS-TT-MAX-TAX       PIC S9(09)V9(02) COMP-3.
               10  WS-TT-DESC          PIC X(30).
       01  WS-TAX-CTL.
           05  WS-TT-USED              PIC S9(05) COMP-3 VALUE 0.
           05  WS-TT-MAX               PIC S9(05) COMP-3 VALUE 400.
           05  WS-TT-HIT               PIC S9(05) COMP-3 VALUE 0.
      *****************************************************************
      * TAX CALCULATION WORK.  FIVE COMPONENTS ARE COMPUTED SEPARATELY*
      * AND EACH IS ROUNDED ON ITS OWN.  THE SUM OF THE ROUNDED PARTS *
      * IS WHAT THE CARRIER PAYS - IT IS NOT THE ROUNDED SUM.         *
      * THE ROUNDING RULE IS SET BY CABS-STD-041.                     *
      *****************************************************************
       01  WS-TAX-WORK.
           05  WS-TW-BASE              PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-TW-USAGE-BASE        PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-TW-CHARGE-BASE       PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-TW-COMPOUND-BASE     PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-TW-RAW               PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-TW-ROUNDED           PIC S9(11)V9(02) COMP-3 VALUE 0.
           05  WS-TW-TOTAL             PIC S9(11)V9(02) COMP-3 VALUE 0.
           05  WS-TW-CAPPED-SW         PIC X(01) VALUE 'N'.
               88  WS-TW-CAPPED        VALUE 'Y'.
       01  WS-TAX-COMPONENTS.
           05  WS-TC-ENTRY OCCURS 5 TIMES.
               10  WS-TC-TYPE          PIC X(02).
               10  WS-TC-RATE          PIC S9(03)V9(05) COMP-3.
               10  WS-TC-BASE          PIC S9(13)V9(05) COMP-3.
               10  WS-TC-AMOUNT        PIC S9(11)V9(02) COMP-3.
               10  WS-TC-DESC          PIC X(30).
       01  WS-TAX-COMP-CTL.
           05  WS-TC-USED              PIC S9(03) COMP-3 VALUE 0.
           05  WS-TC-ORDER-TABLE.
               10  FILLER PIC X(02) VALUE 'FE'.
               10  FILLER PIC X(02) VALUE 'FS'.
               10  FILLER PIC X(02) VALUE 'ST'.
               10  FILLER PIC X(02) VALUE 'LO'.
               10  FILLER PIC X(02) VALUE 'E9'.
           05  WS-TC-ORDER-R REDEFINES WS-TC-ORDER-TABLE.
               10  WS-TC-ORDER OCCURS 5 TIMES PIC X(02).
       01  WS-RUN-TOTALS.
           05  WS-RT-HEADERS           PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-TAXED             PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-EXEMPT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-NO-RATE           PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-CAPPED            PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-TAX-TOTAL         PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-RT-COMP-TOTAL OCCURS 5 TIMES
                                       PIC S9(15)V9(02) COMP-3.
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
                       TAX-MASTER
                       PARM-FILE
           OPEN OUTPUT BHDR-OUT-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 4111 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-BHDRIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 4112 TO WS-AB-CODE
               MOVE 'OPEN FAILED DA-I-TAXMST' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 4113 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-BHDROUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 4114 TO WS-AB-CODE
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
           PERFORM P4000-LOAD-TAX-MASTER THRU P4000-EXIT.
           PERFORM P4050-CLEAR-COMP THRU P4050-EXIT
               VARYING WS-SUB1 FROM 1 BY 1
               UNTIL WS-SUB1 > 5.
           PERFORM P6000-HEADING THRU P6000-EXIT.
           DISPLAY WS-PGM-NAME ' STARTED - RUN ' WS-RUN-ID.
           DISPLAY '  TAX SWITCH    ' WS-PE-TAX-SW.
           DISPLAY '  FED OVERRIDE  ' WS-PE-FED-RATE.
           DISPLAY '  TAX EFF DATE  ' WS-PE-TAX-YYDDD.

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
      * THE TAX EFFECTIVE DATE DECIDES WHICH RATES ARE SELECTED FROM
      * THE MASTER.  IT IS SUPPLIED BY THE SCHEDULER AND IS NOT
      * ALWAYS THE CYCLE DATE - A RATE CHANGE MID CYCLE IS APPLIED
      * FROM THE DATE THE TARIFF SAYS, NOT THE DATE OF THE RUN.
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
           IF WS-PE-TAX-SW NOT = 'N'
               MOVE 'Y' TO WS-PE-TAX-SW.
           IF WS-PE-FED-RATE NOT NUMERIC
               MOVE ZERO TO WS-PE-FED-RATE.
           IF WS-PE-TAX-YYDDD NOT NUMERIC
               MOVE 4121 TO WS-AB-CODE
               MOVE 'TAX EFFECTIVE DATE NOT NUMERIC' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PE-TAX-YYDDD = ZERO
               MOVE WS-PC-CYCLE TO WS-PE-TAX-YYDDD.

       P1200-EXIT.
           EXIT.

      *****************************************************************
      * S200-MAIN PROCESSING                                          *
      * ONE PASS OF THE BILL HEADER FILE.  EVERY HEADER IS TAXED AND  *
      * WRITTEN BACK OUT WITH THE TAX FIELD POPULATED.                *
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
           MOVE ZERO TO WS-TW-TOTAL WS-TC-USED.
           PERFORM P3000-DERIVE-BASE THRU P3000-EXIT.
           PERFORM P3100-EXEMPT-TEST THRU P3100-EXIT.
           IF WS-EXEMPT
               ADD 1 TO WS-RT-EXEMPT
               MOVE ZERO TO BH-TAX
               GO TO P2000-WRITE.
           PERFORM P3200-CALC-ALL-TAXES THRU P3200-EXIT.
           MOVE WS-TW-TOTAL TO BH-TAX.
           ADD 1 TO WS-RT-TAXED.
           ADD WS-TW-TOTAL TO WS-RT-TAX-TOTAL.

       P2000-WRITE.
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
               MOVE 4701 TO WS-AB-CODE
               MOVE 'BILL HEADER READ ERROR' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.

       P2100-EXIT.
           EXIT.

      *****************************************************************
      * S300-TAX CALCULATION                                          *
      * FIVE COMPONENTS.  FEDERAL EXCISE AND THE FEDERAL SURCHARGE ARE*
      * COMPUTED ON THE USAGE BASE.  STATE TAX IS COMPUTED ON THE FULL*
      * BASE AND, IN THE COMPOUNDING STATES, ON THE FEDERAL TAX AS    *
      * WELL.  LOCAL SURCHARGE FOLLOWS STATE.  E911 IS A FLAT AMOUNT. *
      *****************************************************************
       S300-TAX SECTION.

       P3000-DERIVE-BASE.
      * THE TAXABLE BASE.  SETTLEMENT AND PRIOR BALANCE ARE NOT TAXED.
      * ADJUSTMENTS ARE TAXED BECAUSE THEY REPRICE TAXABLE SERVICE, BUT
      * A RESTATEMENT IS NOT - THE ORIGINAL BILL WAS ALREADY TAXED.
           MOVE 'P3000-DERIVE-BASE' TO WS-PARA-NAME.
           COMPUTE WS-TW-USAGE-BASE = BH-CURR-USAGE.
           COMPUTE WS-TW-CHARGE-BASE =
                   BH-CURR-RECURRING + BH-CURR-NONRECUR.
           COMPUTE WS-TW-BASE =
                   WS-TW-USAGE-BASE + WS-TW-CHARGE-BASE
                 + BH-ADJUSTMENTS.
           MOVE ZERO TO WS-TW-COMPOUND-BASE.

       P3000-EXIT.
           EXIT.

       P3100-EXEMPT-TEST.
      * AN EXEMPT CARRIER PAYS NO TAX AT ALL.  THE EXEMPTION IS HELD ON
      * THE ACCOUNT AND ARRIVES ON THE HEADER IN THE HOLD REASON FIELD,
      * WHICH IS OTHERWISE UNUSED AT THIS POINT IN THE STREAM.
           MOVE 'P3100-EXEMPT-TEST' TO WS-PARA-NAME.
           MOVE 'N' TO WS-EXEMPT-SW.
           IF WS-PE-TAX-SW NOT = 'Y'
               MOVE 'Y' TO WS-EXEMPT-SW
               GO TO P3100-EXIT.
           IF BH-HOLD-REASON = 'EXMP'
               MOVE 'Y' TO WS-EXEMPT-SW
               GO TO P3100-EXIT.
           IF WS-TW-BASE NOT > ZERO
               MOVE 'Y' TO WS-EXEMPT-SW.

       P3100-EXIT.
           EXIT.

       P3200-CALC-ALL-TAXES.
      * WALK THE FIVE TAX TYPES IN FILING ORDER.  THE ORDER MATTERS -
      * THE STATE COMPUTATION USES THE FEDERAL RESULT IN THE COMPOUND
      * STATES AND THE LOCAL COMPUTATION USES THE STATE RESULT.
           MOVE 'P3200-CALC-ALL-TAXES' TO WS-PARA-NAME.
           PERFORM P3300-ONE-TAX-TYPE THRU P3300-EXIT
               VARYING WS-TX-SUB FROM 1 BY 1
               UNTIL WS-TX-SUB > 5.

       P3200-EXIT.
           EXIT.

       P3300-ONE-TAX-TYPE.
           MOVE WS-TC-ORDER (WS-TX-SUB) TO WS-TX-TYPE.
           PERFORM P3500-FIND-TAX-RATE THRU P3500-EXIT.
           IF NOT WS-TAX-FOUND
               ADD 1 TO WS-RT-NO-RATE
               GO TO P3300-EXIT.
           PERFORM P3400-SELECT-BASE THRU P3400-EXIT.
           PERFORM P3600-COMPUTE-TAX THRU P3600-EXIT.
           PERFORM P3700-POST-COMPONENT THRU P3700-EXIT.

       P3300-EXIT.
           EXIT.

       P3400-SELECT-BASE.
      * SELECT THE BASE THIS TAX TYPE APPLIES TO.  THE FEDERAL TEST IS
      * MADE BEFORE THE SURCHARGE TEST.  A TYPE THAT SATISFIES BOTH
      * CONDITIONS IS TREATED AS FEDERAL AND IS COMPUTED ON THE USAGE
      * BASE, NOT ON THE FULL BASE.
           MOVE 'P3400-SELECT-BASE' TO WS-PARA-NAME.
           MOVE WS-TW-BASE TO WS-TC-BASE (WS-TX-SUB).
           IF WS-TX-FEDERAL
               MOVE WS-TW-USAGE-BASE TO WS-TC-BASE (WS-TX-SUB)
               GO TO P3400-EXIT.
           IF WS-TX-SURCHARGE
               MOVE WS-TW-CHARGE-BASE TO WS-TC-BASE (WS-TX-SUB)
               GO TO P3400-EXIT.
           IF WS-TX-STATE-TAX
               SET WS-TT-X TO WS-TT-HIT
               IF WS-TT-COMPOUND (WS-TT-X) = 'Y'
                   COMPUTE WS-TC-BASE (WS-TX-SUB) =
                           WS-TW-BASE + WS-TW-COMPOUND-BASE.
           IF WS-TX-LOCAL-TAX
               COMPUTE WS-TC-BASE (WS-TX-SUB) =
                       WS-TW-BASE + WS-TW-COMPOUND-BASE.
           IF WS-TX-E911
               MOVE 1 TO WS-TC-BASE (WS-TX-SUB).

       P3400-EXIT.
           EXIT.

       P3500-FIND-TAX-RATE.
      * FIND THE RATE FOR THIS STATE, JURISDICTION AND TAX TYPE.  THE
      * JURISDICTION ON THE HEADER IS TAKEN FROM THE LARGEST OF THE
      * THREE JURISDICTIONAL SPLIT AMOUNTS - A BILL IS TAXED UNDER ONE
      * JURISDICTION EVEN WHEN IT CARRIES USAGE IN ALL THREE.
           MOVE 'P3500-FIND-TAX-RATE' TO WS-PARA-NAME.
           MOVE 'N' TO WS-TAX-FOUND-SW.
           MOVE ZERO TO WS-TT-HIT.
           PERFORM P3510-DOMINANT-JURIS THRU P3510-EXIT.
           PERFORM P3520-MATCH-RATE THRU P3520-EXIT
               VARYING WS-TT-X FROM 1 BY 1
               UNTIL WS-TT-X > WS-TT-USED OR WS-TAX-FOUND.

       P3500-EXIT.
           EXIT.

       P3510-DOMINANT-JURIS.
           MOVE 'I' TO WS-EW-SEV.
           IF BH-INTRASTATE-AMT > BH-INTERSTATE-AMT
               MOVE 'S' TO WS-EW-SEV.
           IF BH-LOCAL-AMT > BH-INTERSTATE-AMT
               IF BH-LOCAL-AMT > BH-INTRASTATE-AMT
                   MOVE 'L' TO WS-EW-SEV.

       P3510-EXIT.
           EXIT.

       P3520-MATCH-RATE.
           IF WS-TT-TYPE (WS-TT-X) NOT = WS-TX-TYPE
               GO TO P3520-EXIT.
           IF WS-TT-STATE (WS-TT-X) NOT = BH-HOLD-REASON
               IF WS-TT-STATE (WS-TT-X) NOT = '**'
                   GO TO P3520-EXIT.
           IF WS-TT-JURIS (WS-TT-X) NOT = WS-EW-SEV
               IF WS-TT-JURIS (WS-TT-X) NOT = '*'
                   GO TO P3520-EXIT.
           SET WS-SUB1 TO WS-TT-X.
           MOVE WS-SUB1 TO WS-TT-HIT.
           MOVE 'Y' TO WS-TAX-FOUND-SW.

       P3520-EXIT.
           EXIT.

       P3600-COMPUTE-TAX.
      * THE ARITHMETIC.  THE RATE IS A PERCENTAGE HELD TO FIVE DECIMAL
      * PLACES.  EACH COMPONENT IS ROUNDED TO THE CENT ON ITS OWN AND
      * THE ROUNDED COMPONENTS ARE ADDED TOGETHER.  ROUNDING THE SUM
      * INSTEAD WOULD GIVE A DIFFERENT ANSWER ON ABOUT ONE BILL IN SIX.
           MOVE 'P3600-COMPUTE-TAX' TO WS-PARA-NAME.
           SET WS-TT-X TO WS-TT-HIT.
           MOVE 'N' TO WS-TW-CAPPED-SW.
           MOVE WS-TT-RATE (WS-TT-X) TO WS-TC-RATE (WS-TX-SUB).
           IF WS-TC-BASE (WS-TX-SUB) < WS-TT-MIN-BASE (WS-TT-X)
               MOVE ZERO TO WS-TW-RAW
               MOVE ZERO TO WS-TW-ROUNDED
               GO TO P3600-EXIT.
           IF WS-TT-BASIS (WS-TT-X) = 'F'
               MOVE WS-TT-RATE (WS-TT-X) TO WS-TW-RAW
           ELSE
               COMPUTE WS-TW-RAW =
                       (WS-TC-BASE (WS-TX-SUB)
                        * WS-TT-RATE (WS-TT-X)) / 100.
           COMPUTE WS-TW-ROUNDED ROUNDED = WS-TW-RAW.
           IF WS-TT-MAX-TAX (WS-TT-X) NOT = ZERO
               IF WS-TW-ROUNDED > WS-TT-MAX-TAX (WS-TT-X)
                   MOVE WS-TT-MAX-TAX (WS-TT-X) TO WS-TW-ROUNDED
                   MOVE 'Y' TO WS-TW-CAPPED-SW
                   ADD 1 TO WS-RT-CAPPED.

       P3600-EXIT.
           EXIT.

       P3700-POST-COMPONENT.
      * POST THE COMPONENT AND CARRY THE FEDERAL RESULT FORWARD SO THAT
      * THE COMPOUNDING STATES CAN TAX IT.
           MOVE 'P3700-POST-COMPONENT' TO WS-PARA-NAME.
           MOVE WS-TX-TYPE TO WS-TC-TYPE (WS-TX-SUB).
           MOVE WS-TW-ROUNDED TO WS-TC-AMOUNT (WS-TX-SUB).
           SET WS-TT-X TO WS-TT-HIT.
           MOVE WS-TT-DESC (WS-TT-X) TO WS-TC-DESC (WS-TX-SUB).
           ADD WS-TW-ROUNDED TO WS-TW-TOTAL.
           ADD WS-TW-ROUNDED TO WS-RT-COMP-TOTAL (WS-TX-SUB).
           ADD 1 TO WS-TC-USED.
           IF WS-TX-FEDERAL
               ADD WS-TW-ROUNDED TO WS-TW-COMPOUND-BASE.
           PERFORM P5000-REGISTER-LINE THRU P5000-EXIT.

       P3700-EXIT.
           EXIT.

       P3800-WRITE-HEADER.
           MOVE 'P3800-WRITE-HEADER' TO WS-PARA-NAME.
           WRITE BHDR-OUT-REC FROM CABS-BILL-HEADER.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 4702 TO WS-AB-CODE
               MOVE 'BILL HEADER WRITE FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-WRITE-CNT.
           ADD 1 TO WS-RT-HEADERS.
           ADD BH-TAX TO WS-ACC-AMOUNT.

       P3800-EXIT.
           EXIT.

      *****************************************************************
      * S400-TAX MASTER LOAD                                          *
      *****************************************************************
       S400-SUPPORT SECTION.

       P4000-LOAD-TAX-MASTER.
      * LOAD THE TAX RATE MASTER.  THE FILE IS A VSAM KSDS THAT NO
      * PROGRAM IN THE ESTATE WRITES - IT IS MAINTAINED BY THE TAX
      * DEPARTMENT THROUGH A FILE-AID PANEL AND CREATED BY THE IDCAMS
      * JOB IN THE VSAM LIBRARY.
           MOVE 'P4000-LOAD-TAX-MASTER' TO WS-PARA-NAME.
           MOVE ZERO TO WS-TT-USED.
           MOVE LOW-VALUES TO TX-KEY.
           START TAX-MASTER KEY NOT LESS THAN TX-KEY
               INVALID KEY
                   MOVE 4703 TO WS-AB-CODE
                   MOVE 'TAX MASTER EMPTY OR UNREADABLE'
                                       TO WS-AB-TEXT
                   PERFORM P9500-ABEND THRU P9500-EXIT.
           PERFORM P4010-READ-TAX THRU P4010-EXIT
               UNTIL WS-FS-TABLE NOT = '00'.
           DISPLAY 'TAX RATES LOADED ' WS-TT-USED.

       P4000-EXIT.
           EXIT.

       P4010-READ-TAX.
           READ TAX-MASTER NEXT RECORD
               AT END
                   MOVE '10' TO WS-FS-TABLE
                   GO TO P4010-EXIT.
           IF WS-FS-TABLE NOT = '00'
               GO TO P4010-EXIT.
           IF TX-EFF-YYDDD > WS-PE-TAX-YYDDD
               GO TO P4010-EXIT.
           IF TX-EXP-YYDDD NOT = ZERO
               IF TX-EXP-YYDDD < WS-PE-TAX-YYDDD
                   GO TO P4010-EXIT.
           IF WS-TT-USED NOT < WS-TT-MAX
               MOVE 4704 TO WS-AB-CODE
               MOVE 'TAX RATE TABLE FULL' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-TT-USED.
           SET WS-TT-X TO WS-TT-USED.
           MOVE TX-STATE-CD    TO WS-TT-STATE (WS-TT-X).
           MOVE TX-JURIS-CD    TO WS-TT-JURIS (WS-TT-X).
           MOVE TX-TAX-TYPE    TO WS-TT-TYPE (WS-TT-X).
           MOVE TX-RATE        TO WS-TT-RATE (WS-TT-X).
           MOVE TX-BASIS       TO WS-TT-BASIS (WS-TT-X).
           MOVE TX-COMPOUND-SW TO WS-TT-COMPOUND (WS-TT-X).
           MOVE TX-MIN-BASE    TO WS-TT-MIN-BASE (WS-TT-X).
           MOVE TX-MAX-TAX     TO WS-TT-MAX-TAX (WS-TT-X).
           MOVE TX-DESC        TO WS-TT-DESC (WS-TT-X).
           IF TX-TAX-TYPE = 'FE'
               IF WS-PE-FED-RATE NOT = ZERO
                   MOVE WS-PE-FED-RATE TO WS-TT-RATE (WS-TT-X).

       P4010-EXIT.
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
           MOVE BH-BAN                     TO PC-COL-001-020.
           MOVE WS-TC-DESC (WS-TX-SUB)     TO PC-COL-021-060.
           MOVE WS-TC-RATE (WS-TX-SUB)     TO WS-ED-PCT.
           MOVE WS-ED-PCT                  TO PC-COL-061-090.
           MOVE WS-TC-AMOUNT (WS-TX-SUB)   TO WS-ED-MONEY.
           MOVE WS-ED-MONEY                TO PC-COL-091-132.
           WRITE PRT-RECORD FROM CABS-PRINT-LINE.
           ADD 1 TO WS-PAGE-LINES.

       P5000-EXIT.
           EXIT.

       P5100-PRINT-TAX-RECAP.
           MOVE 'P5100-PRINT-TAX-RECAP' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '1' TO PC-CC.
           MOVE 'TAX RECAP BY COMPONENT' TO PC-TEXT.
           WRITE PRT-RECORD FROM CABS-PRINT-LINE.
           PERFORM P5110-PRINT-COMP THRU P5110-EXIT
               VARYING WS-SUB1 FROM 1 BY 1
               UNTIL WS-SUB1 > 5.

       P5100-EXIT.
           EXIT.

       P5110-PRINT-COMP.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-TC-ORDER (WS-SUB1)      TO PC-COL-001-020.
           MOVE WS-RT-COMP-TOTAL (WS-SUB1) TO WS-ED-MONEY.
           MOVE WS-ED-MONEY                TO PC-COL-021-060.
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
           MOVE 'CABBIL07  TAX AND SURCHARGE REGISTER'
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
           MOVE 'BAN                 TAX DESCRIPTION           RATE  '
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
           MOVE 440                    TO CT-STEP-SEQ.
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
           PERFORM P5100-PRINT-TAX-RECAP THRU P5100-EXIT.
           DISPLAY 'HEADERS WRITTEN   ' WS-RT-HEADERS.
           DISPLAY 'HEADERS TAXED     ' WS-RT-TAXED.
           DISPLAY 'HEADERS EXEMPT    ' WS-RT-EXEMPT.
           DISPLAY 'NO RATE FOUND     ' WS-RT-NO-RATE.
           DISPLAY 'CAPPED COMPONENTS ' WS-RT-CAPPED.
           DISPLAY 'TAX TOTAL         ' WS-RT-TAX-TOTAL.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE BHDR-IN-FILE
                 TAX-MASTER
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

       P4050-CLEAR-COMP.
           MOVE ZERO TO WS-RT-COMP-TOTAL (WS-SUB1).
           MOVE SPACES TO WS-TC-TYPE (WS-SUB1).
           MOVE ZERO TO WS-TC-AMOUNT (WS-SUB1).

       P4050-EXIT.
           EXIT.
