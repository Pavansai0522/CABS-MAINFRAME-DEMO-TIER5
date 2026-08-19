      *****************************************************************
      * CABRPT01 - DAILY BALANCING REPORT - END TO END PROOF          *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               CTLIN   TELCABS.CABS.CONTROL(0)           CABSCTL*
      *               PROOFIN TELCABS.CABS.BILLPROOF(0)         (LOCAL)*
      *               SYSIN   INSTREAM CONTROL CARD             (LOCAL)*
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               BALOUT  SYSOUT PRINT - BALANCING REPORT   CABSPRNT*
      *               REPORT  SYSOUT PRINT - RUN REGISTER       CABSPRNT*
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARISED*
      *               + CT-CARRIED-FWD, PROVED FOR EVERY PROCESS IN THE CYCLE*
      *               AND ACROSS EVERY LINK OF THE PROCESS CHAIN      *
      * RESTART     : FULL RERUN - THE REPORT IS REPRODUCED FROM THE CONTROL FILE*
      * REVISION HISTORY                                              *
      *   V1.00  1989-01-16  L.HARGREAVES INITIAL RELEASE - ONE LINE PER*
      *                      PROCESS, NO CHAIN CHECKING               *
      *   V1.04  1991-10-22  M.J.FERRARO  PROCESS CHAIN TABLE INTRODUCED AND*
      *                      THE WRITTEN TO READ CHECK ADDED          *
      *   V1.09  1994-05-11  D.OKONKWO    SUMMARISED RULE ADDED FOR THE*
      *                      PROCESSES THAT FOLD RECORDS              *
      *   V1.14  1996-12-03  J.M.CASTILLO FAMILY RECAP PAGES ADDED AND THE*
      *                      REPORT BURST BY FAMILY                   *
      *   V2.00  2000-08-15  P.NAIR       EQUATION NOW REPROVED HERE RATHER*
      *                      THAN TRUSTING THE INDICATOR - TWO        *
      *                      PROGRAMS HAD BEEN SETTING IT             *
      *                      WITHOUT DOING THE ARITHMETIC             *
      *   V2.06  2004-02-27  A.BUKOWSKI   MISSING PROCESS CHECK ADDED AFTER*
      *                      THE JANUARY CYCLE RAN WITHOUT THE        *
      *                      JURISDICTION STEP AND STILL PRINTED      *
      *   V3.00  2009-11-06  R.KAMINSKI   INVOICE PROOF FILE FROM CABBIL11*
      *                      BROUGHT INTO THE REPORT SO THAT THE      *
      *                      MONEY IS PROVED AS WELL AS THE           *
      *                      RECORD COUNTS                            *
      *   V3.03  2014-07-18  S.MARCHETTI  CHAIN TABLE EXTENDED TO THE FORMAT*
      *                      FAMILY                                   *
      *   V3.05  2019-08-22  G.PRZYBYLSKI EXPECTED PROCESS COUNT ACCEPTED FROM*
      *                      THE CONTROL CARD                         *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABRPT01.
       AUTHOR. TELCABS APPLICATIONS - BILLING CONTROL TEAM.
      *****************************************************************
      * THE DAILY BALANCING REPORT.  READS THE CONTROL RECORD WRITTEN *
      * BY EVERY PROCESS IN THE CYCLE, REPROVES THE BALANCING EQUATION*
      * FOR EACH ONE, WALKS THE PROCESS CHAIN TO PROVE THAT WHAT ONE  *
      * PROCESS WROTE IS WHAT THE NEXT ONE READ, AND SUMMARISES THE   *
      * INVOICE LEVEL PROOF.  THE VERDICT LINE ON THE LAST PAGE IS    *
      * THE ONE OPERATIONS CHECK BEFORE THE PRINT JOBS ARE RELEASED.  *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CTL-IN-FILE ASSIGN TO UT-S-CTLIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT PROOF-IN-FILE ASSIGN TO UT-S-PROOFIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-TABLE.
           SELECT BAL-OUT-FILE ASSIGN TO UT-S-BALOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT SORT-FILE ASSIGN TO UT-S-SORTWK1.
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
      * CTLIN - EVERY CONTROL RECORD WRITTEN BY EVERY                 *
      * PROCESS IN THE CYCLE.  THE FILE IS WRITTEN WITH               *
      * DISP MOD SO SUPERSEDED RERUNS ARE STILL ON IT.                *
      *****************************************************************
       FD  CTL-IN-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CTL-IN-REC                       PIC X(180).
      *****************************************************************
      * PROOFIN - THE ACCOUNT LEVEL PROOF FROM CABBIL11.              *
      *****************************************************************
       FD  PROOF-IN-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 120 CHARACTERS.
       01  PROOF-IN-REC                     PIC X(120).
      *****************************************************************
      * BALOUT - THE BALANCING REPORT ITSELF.                         *
      *****************************************************************
       FD  BAL-OUT-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       01  BAL-RECORD                       PIC X(133).
      *****************************************************************
      * THE INTERNAL SORT.  CONTROL RECORDS ARE SORTED INTO           *
      * FAMILY, STEP AND PROCESS ORDER SO THAT THE REPORT READS       *
      * IN RUN ORDER.                                                 *
      *****************************************************************
       SD  SORT-FILE
           RECORD CONTAINS 194 CHARACTERS.
       01  SORT-RECORD.
           05  SR-KEY.
               10  SR-FAMILY           PIC X(03).
               10  SR-STEP             PIC 9(03).
               10  SR-PROCESS          PIC X(08).
           05  SR-IMAGE                PIC X(180).
       01  SORT-RECORD-R REDEFINES SORT-RECORD.
           05  SR-FULL-KEY             PIC X(14).
           05  SR-BODY                 PIC X(180).
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
           05  WS-PGM-NAME             PIC X(08) VALUE 'CABRPT01'.
           05  WS-PGM-VERSION          PIC X(05) VALUE 'V3.05'.
           05  WS-PGM-APPL             PIC X(04) VALUE 'CABS'.
           05  WS-PGM-COMPILE          PIC X(08) VALUE '20190822'.
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
           05  WS-PE-HALT-SW           PIC X(01).
           05  WS-PE-CHAIN-SW          PIC X(01).
           05  WS-PE-HASH-SW           PIC X(01).
           05  WS-PE-DETAIL-SW         PIC X(01).
           05  WS-PE-EXPECT-PROC       PIC 9(03).
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
           05  WS-CTL-EOF-SW           PIC X(01) VALUE 'N'.
               88  WS-CTL-EOF          VALUE 'Y'.
           05  WS-PRF-EOF-SW           PIC X(01) VALUE 'N'.
               88  WS-PRF-EOF          VALUE 'Y'.
           05  WS-SORT-EOF-SW          PIC X(01) VALUE 'N'.
               88  WS-SORT-EOF         VALUE 'Y'.
           05  WS-SORT-DONE-SW         PIC X(01) VALUE 'N'.
               88  WS-SORT-DONE        VALUE 'Y'.
           05  WS-RUN-BAL-SW           PIC X(01) VALUE 'Y'.
               88  WS-RUN-IN-BAL       VALUE 'Y'.
           05  WS-CHAIN-FOUND-SW       PIC X(01) VALUE 'N'.
               88  WS-CHAIN-FOUND      VALUE 'Y'.
           05  WS-PRED-FOUND-SW        PIC X(01) VALUE 'N'.
               88  WS-PRED-FOUND       VALUE 'Y'.
           05  WS-RELEASE-SW           PIC X(01) VALUE 'Y'.
               88  WS-RELEASE-IT       VALUE 'Y'.
      *****************************************************************
      * THE PROCESS CHAIN TABLE.  SIXTY THREE ENTRIES.  EACH ONE NAMES*
      * A PROCESS, ITS FAMILY, ITS STEP SEQUENCE, THE PROCESS THAT    *
      * FEEDS IT AND THE CHAIN RULE THAT SAYS WHAT MUST MATCH:        *
      *   W  THE PREDECESSOR WRITTEN COUNT MUST EQUAL THIS READ COUNT *
      *   S  THE PREDECESSOR SUMMARISED COUNT MUST EQUAL THIS READ    *
      *   N  NO CHAIN RULE - THIS PROCESS STARTS FROM A MASTER FILE OR*
      *      READS A DATASET THAT ANOTHER CYCLE PRODUCED              *
      * THIS TABLE IS THE ONLY PLACE IN THE ESTATE WHERE THE END TO END*
      * SHAPE OF THE BILLING RUN IS WRITTEN DOWN.                     *
      *****************************************************************
       01  WS-CHAIN-TABLE.
           05  FILLER PIC X(24) VALUE
               'CABING01ING100        N'.
           05  FILLER PIC X(24) VALUE
               'CABING02ING105CABING01W'.
           05  FILLER PIC X(24) VALUE
               'CABING03ING110CABING02W'.
           05  FILLER PIC X(24) VALUE
               'CABING04ING115CABING03W'.
           05  FILLER PIC X(24) VALUE
               'CABING05ING120CABING04S'.
           05  FILLER PIC X(24) VALUE
               'CABING06ING125CABING05W'.
           05  FILLER PIC X(24) VALUE
               'CABING07ING130CABING06W'.
           05  FILLER PIC X(24) VALUE
               'CABING08ING135CABING07W'.
           05  FILLER PIC X(24) VALUE
               'CABING09ING140CABING08S'.
           05  FILLER PIC X(24) VALUE
               'CABING10ING145CABING09W'.
           05  FILLER PIC X(24) VALUE
               'CABING11ING150CABING10W'.
           05  FILLER PIC X(24) VALUE
               'CABING12ING155CABING11W'.
           05  FILLER PIC X(24) VALUE
               'CABRAT01RAT200        N'.
           05  FILLER PIC X(24) VALUE
               'CABRAT02RAT205CABING12W'.
           05  FILLER PIC X(24) VALUE
               'CABRAT03RAT210CABRAT02W'.
           05  FILLER PIC X(24) VALUE
               'CABRAT04RAT215CABRAT03W'.
           05  FILLER PIC X(24) VALUE
               'CABRAT05RAT220CABRAT04W'.
           05  FILLER PIC X(24) VALUE
               'CABRAT06RAT225CABRAT05W'.
           05  FILLER PIC X(24) VALUE
               'CABRAT07RAT230CABRAT06W'.
           05  FILLER PIC X(24) VALUE
               'CABRAT08RAT235CABRAT07W'.
           05  FILLER PIC X(24) VALUE
               'CABRAT09RAT240CABRAT08S'.
           05  FILLER PIC X(24) VALUE
               'CABRAT10RAT245CABRAT09W'.
           05  FILLER PIC X(24) VALUE
               'CABRAT11RAT250CABRAT10W'.
           05  FILLER PIC X(24) VALUE
               'CABRAT12RAT255CABRAT11W'.
           05  FILLER PIC X(24) VALUE
               'CABRAT13RAT260CABRAT12W'.
           05  FILLER PIC X(24) VALUE
               'CABRAT14RAT265CABRAT13N'.
           05  FILLER PIC X(24) VALUE
               'CABJUR01JUR300        N'.
           05  FILLER PIC X(24) VALUE
               'CABJUR02JUR305CABJUR01W'.
           05  FILLER PIC X(24) VALUE
               'CABJUR03JUR310CABRAT10W'.
           05  FILLER PIC X(24) VALUE
               'CABJUR04JUR315CABJUR03W'.
           05  FILLER PIC X(24) VALUE
               'CABJUR05JUR320CABJUR04W'.
           05  FILLER PIC X(24) VALUE
               'CABJUR06JUR325CABJUR05W'.
           05  FILLER PIC X(24) VALUE
               'CABJUR07JUR330CABJUR06N'.
           05  FILLER PIC X(24) VALUE
               'CABJUR08JUR335CABJUR07W'.
           05  FILLER PIC X(24) VALUE
               'CABJUR09JUR340CABJUR05S'.
           05  FILLER PIC X(24) VALUE
               'CABJUR10JUR345CABJUR07W'.
           05  FILLER PIC X(24) VALUE
               'CABJUR11JUR350CABJUR09N'.
           05  FILLER PIC X(24) VALUE
               'CABSET01SET200CABSET02W'.
           05  FILLER PIC X(24) VALUE
               'CABSET02SET195        N'.
           05  FILLER PIC X(24) VALUE
               'CABSET05SET215CABSET04W'.
           05  FILLER PIC X(24) VALUE
               'CABSET09SET280CABSET05S'.
           05  FILLER PIC X(24) VALUE
               'CABBIL01BIL400        N'.
           05  FILLER PIC X(24) VALUE
               'CABBIL02BIL410CABJUR05S'.
           05  FILLER PIC X(24) VALUE
               'CABBIL03BIL415CABBIL02W'.
           05  FILLER PIC X(24) VALUE
               'CABBIL04BIL420        N'.
           05  FILLER PIC X(24) VALUE
               'CABBIL05BIL425CABBIL04W'.
           05  FILLER PIC X(24) VALUE
               'CABBIL06BIL430CABBIL05W'.
           05  FILLER PIC X(24) VALUE
               'CABBIL09BIL435CABBIL03S'.
           05  FILLER PIC X(24) VALUE
               'CABBIL07BIL440CABBIL09W'.
           05  FILLER PIC X(24) VALUE
               'CABBIL08BIL445CABBIL07W'.
           05  FILLER PIC X(24) VALUE
               'CABBIL10BIL450CABBIL08W'.
           05  FILLER PIC X(24) VALUE
               'CABBIL11BIL455CABBIL03S'.
           05  FILLER PIC X(24) VALUE
               'CABBIL12BIL460CABBIL10W'.
           05  FILLER PIC X(24) VALUE
               'CABFMT01FMT500CABBIL03S'.
           05  FILLER PIC X(24) VALUE
               'CABFMT02FMT505CABFMT01W'.
           05  FILLER PIC X(24) VALUE
               'CABFMT03FMT510CABBIL03N'.
           05  FILLER PIC X(24) VALUE
               'CABFMT04FMT515CABFMT03W'.
           05  FILLER PIC X(24) VALUE
               'CABFMT05FMT520CABBIL12N'.
           05  FILLER PIC X(24) VALUE
               'CABFMT06FMT525CABBIL03N'.
           05  FILLER PIC X(24) VALUE
               'CABFMT07FMT530CABBIL03N'.
           05  FILLER PIC X(24) VALUE
               'CABFMT08FMT535CABFMT04S'.
           05  FILLER PIC X(24) VALUE
               'CABFMT09FMT540CABBIL12N'.
       01  WS-CHAIN-TABLE-R REDEFINES WS-CHAIN-TABLE.
           05  WS-CT-ENTRY OCCURS 63 TIMES INDEXED BY WS-CT-X.
               10  WS-CT-PROCESS       PIC X(08).
               10  WS-CT-FAMILY        PIC X(03).
               10  WS-CT-STEP          PIC 9(03).
               10  WS-CT-PRED          PIC X(08).
               10  WS-CT-RULE          PIC X(01).
               10  WS-CT-FILLER        PIC X(01).
      *****************************************************************
      * THE FAMILY TABLE AND ITS RECAP TOTALS.                        *
      *****************************************************************
       01  WS-FAMILY-TABLE.
           05  FILLER PIC X(31) VALUE
               'INGUSAGE INGEST                '.
           05  FILLER PIC X(31) VALUE
               'RATACCESS RATING               '.
           05  FILLER PIC X(31) VALUE
               'JURJURISDICTION AND FACTORS    '.
           05  FILLER PIC X(31) VALUE
               'SETINTER CARRIER SETTLEMENT    '.
           05  FILLER PIC X(31) VALUE
               'BILBILL CALCULATION            '.
           05  FILLER PIC X(31) VALUE
               'FMTBILL FORMAT AND PRINT       '.
           05  FILLER PIC X(31) VALUE
               'RPTREPORTING AND CLOSE         '.
       01  WS-FAMILY-TABLE-R REDEFINES WS-FAMILY-TABLE.
           05  WS-FT-ENTRY OCCURS 7 TIMES INDEXED BY WS-FT-X.
               10  WS-FT-CODE          PIC X(03).
               10  WS-FT-NAME          PIC X(28).
       01  WS-FAMILY-TOTALS.
           05  WS-FM-ENTRY OCCURS 7 TIMES.
               10  WS-FM-PROCESSES     PIC S9(05) COMP-3.
               10  WS-FM-READ          PIC S9(13) COMP-3.
               10  WS-FM-WRITTEN       PIC S9(13) COMP-3.
               10  WS-FM-REJECTED      PIC S9(13) COMP-3.
               10  WS-FM-SUMMARISED    PIC S9(13) COMP-3.
               10  WS-FM-CFWD          PIC S9(13) COMP-3.
               10  WS-FM-AMOUNT        PIC S9(15)V9(05) COMP-3.
               10  WS-FM-OUT-OF-BAL    PIC S9(05) COMP-3.
       01  WS-FAMILY-WORK.
           05  WS-FR-CODE              PIC X(03) VALUE SPACES.
           05  WS-FR-SUB               PIC S9(03) COMP-3 VALUE 0.
           05  WS-FR-NAME              PIC X(28) VALUE SPACES.
      *****************************************************************
      * THE CONTROL RECORD TABLE.  EVERY CONTROL RECORD WRITTEN BY    *
      * EVERY PROCESS IN THE CYCLE IS HELD HERE SO THAT THE CHAIN CAN *
      * BE WALKED IN EITHER DIRECTION.                                *
      *****************************************************************
       01  WS-PROC-TABLE.
           05  WS-PT-ENTRY OCCURS 400 TIMES INDEXED BY WS-PT-X.
               10  WS-PT-PROCESS       PIC X(08).
               10  WS-PT-STEP          PIC 9(03).
               10  WS-PT-JOBNAME       PIC X(08).
               10  WS-PT-STEPNAME      PIC X(08).
               10  WS-PT-READ          PIC S9(11) COMP-3.
               10  WS-PT-WRITTEN       PIC S9(11) COMP-3.
               10  WS-PT-REJECTED      PIC S9(11) COMP-3.
               10  WS-PT-SUMMARISED    PIC S9(11) COMP-3.
               10  WS-PT-CFWD          PIC S9(11) COMP-3.
               10  WS-PT-HASH-MIN      PIC S9(15)V9(02) COMP-3.
               10  WS-PT-HASH-AMT      PIC S9(13)V9(05) COMP-3.
               10  WS-PT-BAL-IND       PIC X(01).
               10  WS-PT-RC            PIC 9(04).
               10  WS-PT-FAMILY        PIC X(03).
               10  WS-PT-CHAIN-IND     PIC X(01).
               10  WS-PT-RERUN         PIC 9(02).
       01  WS-PROC-CTL.
           05  WS-PT-USED              PIC S9(05) COMP-3 VALUE 0.
           05  WS-PT-MAX               PIC S9(05) COMP-3 VALUE 400.
           05  WS-PT-HIT               PIC S9(05) COMP-3 VALUE 0.
           05  WS-PT-PRED-HIT          PIC S9(05) COMP-3 VALUE 0.
      *****************************************************************
      * EQUATION AND CHAIN CHECK WORK.                                *
      *****************************************************************
       01  WS-CHECK-WORK.
           05  WS-CK-EXPECTED          PIC S9(13) COMP-3 VALUE 0.
           05  WS-CK-ACTUAL            PIC S9(13) COMP-3 VALUE 0.
           05  WS-CK-DIFF              PIC S9(13) COMP-3 VALUE 0.
           05  WS-CK-PRED-SIDE         PIC S9(13) COMP-3 VALUE 0.
           05  WS-CK-HASH-DIFF         PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-CK-RESULT            PIC X(11) VALUE SPACES.
           05  WS-CK-REASON            PIC X(30) VALUE SPACES.
      *****************************************************************
      * THE BILL PROOF FILE FROM CABBIL11.  THE ACCOUNT LEVEL PROOF IS*
      * SUMMARISED HERE SO THAT THE RUN LEVEL REPORT CAN SAY WHETHER  *
      * THE INVOICES THEMSELVES ADD UP AS WELL AS THE RECORD COUNTS.  *
      *****************************************************************
       01  WS-PROOF-IN.
           05  WS-PI-BAN               PIC X(13).
           05  WS-PI-PERIOD            PIC 9(06).
           05  WS-PI-DETAIL            PIC S9(15)V9(02).
           05  WS-PI-HEADER            PIC S9(15)V9(05).
           05  WS-PI-DIFF              PIC S9(13)V9(05).
           05  WS-PI-LINES             PIC 9(07).
           05  WS-PI-RESULT            PIC X(11).
           05  WS-PI-DELTA             PIC S9(11)V9(05).
           05  WS-PI-FILLER            PIC X(38).
       01  WS-PROOF-IN-K REDEFINES WS-PROOF-IN.
           05  WS-PK-KEY               PIC X(19).
           05  WS-PK-REST              PIC X(101).
       01  WS-PROOF-TOTALS.
           05  WS-PF-ACCOUNTS          PIC S9(09) COMP-3 VALUE 0.
           05  WS-PF-IN-BAL            PIC S9(09) COMP-3 VALUE 0.
           05  WS-PF-OUT-OF-BAL        PIC S9(09) COMP-3 VALUE 0.
           05  WS-PF-DETAIL-SUM        PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-PF-HEADER-SUM        PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-PF-DELTA-SUM         PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-PF-WORST             PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-PF-WORST-BAN         PIC X(13) VALUE SPACES.
      *****************************************************************
      * RUN LEVEL TOTALS AND THE EXCEPTION LIST.                      *
      *****************************************************************
       01  WS-RUN-TOTALS.
           05  WS-RT-PROCESSES         PIC S9(05) COMP-3 VALUE 0.
           05  WS-RT-EQN-FAILED        PIC S9(05) COMP-3 VALUE 0.
           05  WS-RT-CHAIN-FAILED      PIC S9(05) COMP-3 VALUE 0.
           05  WS-RT-HASH-FAILED       PIC S9(05) COMP-3 VALUE 0.
           05  WS-RT-MISSING           PIC S9(05) COMP-3 VALUE 0.
           05  WS-RT-RERUN             PIC S9(05) COMP-3 VALUE 0.
           05  WS-RT-RC-NONZERO        PIC S9(05) COMP-3 VALUE 0.
           05  WS-RT-READ              PIC S9(15) COMP-3 VALUE 0.
           05  WS-RT-WRITTEN           PIC S9(15) COMP-3 VALUE 0.
           05  WS-RT-REJECTED          PIC S9(15) COMP-3 VALUE 0.
           05  WS-RT-SUMMARISED        PIC S9(15) COMP-3 VALUE 0.
           05  WS-RT-CFWD              PIC S9(15) COMP-3 VALUE 0.
           05  WS-RT-AMOUNT            PIC S9(15)V9(05) COMP-3 VALUE 0.
       01  WS-EXCEPTION-TABLE.
           05  WS-EX-ENTRY OCCURS 100 TIMES INDEXED BY WS-EX-X.
               10  WS-EX-PROCESS       PIC X(08).
               10  WS-EX-STEP          PIC 9(03).
               10  WS-EX-REASON        PIC X(30).
               10  WS-EX-DIFF          PIC S9(13) COMP-3.
       01  WS-EXCEPTION-CTL.
           05  WS-EX-USED              PIC S9(03) COMP-3 VALUE 0.
           05  WS-EX-MAX               PIC S9(03) COMP-3 VALUE 100.
       01  WS-EDIT-BIG.
           05  WS-EB-COUNT             PIC ZZZ,ZZZ,ZZZ,ZZ9-.
           05  WS-EB-MONEY             PIC ZZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-EB-DIFF              PIC ZZZ,ZZZ,ZZ9-.
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
           OPEN INPUT  CTL-IN-FILE
                       PROOF-IN-FILE
                       PARM-FILE
           OPEN OUTPUT BAL-OUT-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 7011 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-CTLIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 7012 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-PROOFIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 7013 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-BALOUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 7014 TO WS-AB-CODE
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
           PERFORM P7100-CLEAR-FAMILY THRU P7100-EXIT
               VARYING WS-SUB1 FROM 1 BY 1
               UNTIL WS-SUB1 > 7.
           PERFORM P6000-HEADING THRU P6000-EXIT.
           DISPLAY WS-PGM-NAME ' STARTED - RUN ' WS-RUN-ID.
           DISPLAY '  CHAIN CHECK   ' WS-PE-CHAIN-SW.
           DISPLAY '  HALT ON FAIL  ' WS-PE-HALT-SW.
           DISPLAY '  EXPECTED PROC ' WS-PE-EXPECT-PROC.

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
      * THE EXPECTED PROCESS COUNT IS SUPPLIED BY THE SCHEDULER FROM
      * THE CYCLE DEFINITION.  IT DIFFERS BETWEEN A NORMAL CYCLE, A
      * RESTATEMENT CYCLE AND A MONTH END CYCLE, SO IT HAS NO
      * DEFAULT AND AN ABSENT VALUE STOPS THE STEP.
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
           IF WS-PE-CHAIN-SW NOT = 'N'
               MOVE 'Y' TO WS-PE-CHAIN-SW.
           IF WS-PE-HALT-SW NOT = 'N'
               MOVE 'Y' TO WS-PE-HALT-SW.
           IF WS-PE-HASH-SW NOT = 'N'
               MOVE 'Y' TO WS-PE-HASH-SW.
           IF WS-PE-EXPECT-PROC NOT NUMERIC
               MOVE 7021 TO WS-AB-CODE
               MOVE 'EXPECTED PROCESS COUNT NOT NUMERIC'
                                       TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.

       P1200-EXIT.
           EXIT.

      *****************************************************************
      * S200-MAIN PROCESSING                                          *
      * THE WHOLE REPORT IS DRIVEN FROM ONE INTERNAL SORT OF THE      *
      * CONTROL FILE.  THE ELIGIBILITY RULE - WHICH CONTROL RECORDS   *
      * BELONG TO THE CYCLE BEING REPORTED - LIVES IN THE INPUT       *
      * PROCEDURE AND NOWHERE ELSE.                                   *
      * SORT FIELDS ARE FILED WITH THE DATASET REGISTER.              *
      *****************************************************************
       S200-MAIN-PROCESS SECTION.

       P2000-PROCESS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           IF WS-SORT-DONE
               MOVE 'Y' TO WS-EOF-SW
               GO TO P2000-EXIT.
           SORT SORT-FILE
               ON ASCENDING KEY SR-FAMILY
                                SR-STEP
                                SR-PROCESS
               INPUT PROCEDURE IS S310-SORT-INPUT
               OUTPUT PROCEDURE IS S320-SORT-OUTPUT.
           IF SORT-RETURN NOT = ZERO
               MOVE 7001 TO WS-AB-CODE
               MOVE 'CONTROL RECORD SORT FAILED' TO WS-AB-TEXT
               GO TO P9970-CONTROL-FAILURE.
           MOVE 'Y' TO WS-SORT-DONE-SW.
           PERFORM P5000-CHAIN-CHECK THRU P5000-EXIT.
           PERFORM P5600-HASH-CONTINUITY THRU P5600-EXIT.
           PERFORM P5500-PROOF-SUMMARY THRU P5500-EXIT.
           PERFORM P6000-PRINT-REPORT THRU P6000-EXIT.
           MOVE 'Y' TO WS-EOF-SW.

       P2000-EXIT.
           EXIT.

      *****************************************************************
      * S310-SORT INPUT PROCEDURE                                     *
      * READ THE CONTROL FILE AND DECIDE WHAT BELONGS TO THIS CYCLE.  *
      * A CONTROL RECORD FROM AN EARLIER RERUN OF THE SAME PROCESS IS *
      * SUPERSEDED BY THE LATEST ONE AND IS NOT REPORTED.             *
      *****************************************************************
       S310-SORT-INPUT SECTION.

       P3000-INPUT-DRIVER.
           MOVE 'P3000-INPUT-DRIVER' TO WS-PARA-NAME.
           PERFORM P3100-READ-CONTROL THRU P3100-EXIT
               UNTIL WS-CTL-EOF.
           GO TO P3000-EXIT.

       P3000-EXIT.
           EXIT.

       P3100-READ-CONTROL.
           READ CTL-IN-FILE INTO CABS-CONTROL-RECORD
               AT END
                   MOVE 'Y' TO WS-CTL-EOF-SW
                   GO TO P3100-EXIT.
           IF WS-FS-INPUT NOT = '00' AND WS-FS-INPUT NOT = '10'
               MOVE 7002 TO WS-AB-CODE
               MOVE 'CONTROL FILE READ ERROR' TO WS-AB-TEXT
               GO TO P9970-CONTROL-FAILURE.
           ADD 1 TO WS-READ-CNT.
           MOVE 'Y' TO WS-RELEASE-SW.
           PERFORM P3200-ELIGIBILITY THRU P3200-EXIT.
           IF NOT WS-RELEASE-IT
               ADD 1 TO WS-CFWD-CNT
               GO TO P3100-EXIT.
           PERFORM P3300-BUILD-SORT-REC THRU P3300-EXIT.
           RELEASE SORT-RECORD.
           ADD 1 TO WS-SUMM-CNT.

       P3100-EXIT.
           EXIT.

       P3200-ELIGIBILITY.
      * THE ELIGIBILITY RULE.  A CONTROL RECORD BELONGS TO THIS REPORT
      * WHEN ITS CYCLE DATE MATCHES AND ITS RUN ID MATCHES.  RECORDS
      * FROM A SUPERSEDED RERUN ARE DROPPED HERE; THE CONTROL FILE IS
      * WRITTEN WITH DISP MOD SO EVERY ATTEMPT IS STILL ON IT.
           MOVE 'P3200-ELIGIBILITY' TO WS-PARA-NAME.
           MOVE 'Y' TO WS-RELEASE-SW.
           IF CT-CYCLE-YYDDD NOT = WS-CYCLE-YYDDD
               MOVE 'N' TO WS-RELEASE-SW
               GO TO P3200-EXIT.
           IF CT-RUN-ID NOT = WS-RUN-ID
               IF WS-PE-DETAIL-SW NOT = 'A'
                   MOVE 'N' TO WS-RELEASE-SW
                   GO TO P3200-EXIT.
           IF CT-PROCESS-ID = SPACES
               MOVE 'N' TO WS-RELEASE-SW
               MOVE 7003 TO WS-AB-CODE
               MOVE 'CONTROL RECORD WITH NO PROCESS ID'
                                       TO WS-AB-TEXT
               GO TO P9970-CONTROL-FAILURE.
           IF CT-RERUN-NBR > ZERO
               ADD 1 TO WS-RT-RERUN.

       P3200-EXIT.
           EXIT.

       P3300-BUILD-SORT-REC.
      * THE SORT KEY IS FAMILY, STEP, PROCESS SO THAT THE REPORT READS
      * IN RUN ORDER WITHIN FAMILY.  THE FAMILY IS DERIVED FROM THE
      * CHAIN TABLE, NOT FROM THE PROCESS NAME - TWO SETTLEMENT
      * PROGRAMS DO NOT FOLLOW THE NAMING CONVENTION.
           MOVE 'P3300-BUILD-SORT-REC' TO WS-PARA-NAME.
           MOVE SPACES TO SORT-RECORD.
           PERFORM P3400-DERIVE-FAMILY THRU P3400-EXIT.
           MOVE WS-FR-CODE             TO SR-FAMILY.
           MOVE CT-STEP-SEQ            TO SR-STEP.
           MOVE CT-PROCESS-ID          TO SR-PROCESS.
           MOVE CABS-CONTROL-RECORD    TO SR-IMAGE.

       P3300-EXIT.
           EXIT.

       P3400-DERIVE-FAMILY.
           MOVE 'ZZZ' TO WS-FR-CODE.
           MOVE 'N' TO WS-CHAIN-FOUND-SW.
           PERFORM P3410-MATCH-CHAIN THRU P3410-EXIT
               VARYING WS-CT-X FROM 1 BY 1
               UNTIL WS-CT-X > 63 OR WS-CHAIN-FOUND.
           IF NOT WS-CHAIN-FOUND
               ADD 1 TO WS-RT-MISSING.

       P3400-EXIT.
           EXIT.

       P3410-MATCH-CHAIN.
           IF WS-CT-PROCESS (WS-CT-X) = CT-PROCESS-ID
               MOVE WS-CT-FAMILY (WS-CT-X) TO WS-FR-CODE
               MOVE 'Y' TO WS-CHAIN-FOUND-SW.

       P3410-EXIT.
           EXIT.

      *****************************************************************
      * S320-SORT OUTPUT PROCEDURE                                    *
      * RETURN THE SORTED CONTROL RECORDS, PROVE THE BALANCING        *
      * EQUATION FOR EACH ONE AND LOAD THE PROCESS TABLE THAT THE     *
      * CHAIN CHECK LATER WALKS.                                      *
      *****************************************************************
       S320-SORT-OUTPUT SECTION.

       P4000-OUTPUT-DRIVER.
           MOVE 'P4000-OUTPUT-DRIVER' TO WS-PARA-NAME.
           MOVE ZERO TO WS-PT-USED.
           PERFORM P4100-RETURN-ONE THRU P4100-EXIT
               UNTIL WS-SORT-EOF.
           GO TO P4000-EXIT.

       P4000-EXIT.
           EXIT.

       P4100-RETURN-ONE.
           RETURN SORT-FILE
               AT END
                   MOVE 'Y' TO WS-SORT-EOF-SW
                   GO TO P4100-EXIT.
           MOVE SR-IMAGE TO CABS-CONTROL-RECORD.
           ADD 1 TO WS-RT-PROCESSES.
           PERFORM P4200-PROVE-EQUATION THRU P4200-EXIT.
           PERFORM P4300-LOAD-PROCESS THRU P4300-EXIT.
           PERFORM P4400-ACCUM-FAMILY THRU P4400-EXIT.
           PERFORM P4500-ACCUM-RUN THRU P4500-EXIT.

       P4100-EXIT.
           EXIT.

       P4200-PROVE-EQUATION.
      * PROVE THE PROCESS LEVEL BALANCING EQUATION.  THIS IS THE SAME
      * EQUATION EVERY PROCESS EVALUATES FOR ITSELF; PROVING IT AGAIN
      * HERE CATCHES A PROCESS THAT SET THE INDICATOR WITHOUT DOING
      * THE ARITHMETIC, WHICH HAS HAPPENED TWICE.
           MOVE 'P4200-PROVE-EQUATION' TO WS-PARA-NAME.
           MOVE 'IN BALANCE' TO WS-CK-RESULT.
           MOVE SPACES TO WS-CK-REASON.
           COMPUTE WS-CK-EXPECTED =
                   CT-WRITTEN + CT-REJECTED
                 + CT-SUMMARISED + CT-CARRIED-FWD.
           MOVE CT-READ TO WS-CK-ACTUAL.
           COMPUTE WS-CK-DIFF = WS-CK-ACTUAL - WS-CK-EXPECTED.
           IF WS-CK-DIFF = ZERO
               GO TO P4200-INDICATOR.
           MOVE 'OUT OF BAL' TO WS-CK-RESULT.
           MOVE 'EQUATION DOES NOT HOLD' TO WS-CK-REASON.
           ADD 1 TO WS-RT-EQN-FAILED.
           MOVE 'N' TO WS-RUN-BAL-SW.
           PERFORM P4600-ADD-EXCEPTION THRU P4600-EXIT.

       P4200-INDICATOR.
           IF CT-OUT-OF-BAL
               IF WS-CK-DIFF = ZERO
                   MOVE 'INDICATOR SET, EQUATION HOLDS'
                                       TO WS-CK-REASON
                   PERFORM P4600-ADD-EXCEPTION THRU P4600-EXIT.
           IF CT-IN-BALANCE
               IF WS-CK-DIFF NOT = ZERO
                   MOVE 'INDICATOR CLEAR, EQUATION FAILS'
                                       TO WS-CK-REASON
                   PERFORM P4600-ADD-EXCEPTION THRU P4600-EXIT.
           IF CT-RC NOT = ZERO
               ADD 1 TO WS-RT-RC-NONZERO.

       P4200-EXIT.
           EXIT.

       P4300-LOAD-PROCESS.
      * HOLD THE CONTROL RECORD IN THE PROCESS TABLE.  A PROCESS THAT
      * APPEARS TWICE - A RERUN - REPLACES THE EARLIER ENTRY SO THAT
      * THE CHAIN CHECK SEES THE FINAL POSITION ONLY.
           MOVE 'P4300-LOAD-PROCESS' TO WS-PARA-NAME.
           MOVE 'N' TO WS-CHAIN-FOUND-SW.
           MOVE ZERO TO WS-PT-HIT.
           PERFORM P4310-FIND-PROCESS THRU P4310-EXIT
               VARYING WS-PT-X FROM 1 BY 1
               UNTIL WS-PT-X > WS-PT-USED OR WS-CHAIN-FOUND.
           IF WS-CHAIN-FOUND
               GO TO P4300-STORE.
           IF WS-PT-USED NOT < WS-PT-MAX
               MOVE 7004 TO WS-AB-CODE
               MOVE 'PROCESS TABLE FULL' TO WS-AB-TEXT
               GO TO P9970-CONTROL-FAILURE.
           ADD 1 TO WS-PT-USED.
           MOVE WS-PT-USED TO WS-PT-HIT.

       P4300-STORE.
           SET WS-PT-X TO WS-PT-HIT.
           MOVE CT-PROCESS-ID     TO WS-PT-PROCESS (WS-PT-X).
           MOVE CT-STEP-SEQ       TO WS-PT-STEP (WS-PT-X).
           MOVE CT-JOBNAME        TO WS-PT-JOBNAME (WS-PT-X).
           MOVE CT-STEPNAME       TO WS-PT-STEPNAME (WS-PT-X).
           MOVE CT-READ           TO WS-PT-READ (WS-PT-X).
           MOVE CT-WRITTEN        TO WS-PT-WRITTEN (WS-PT-X).
           MOVE CT-REJECTED       TO WS-PT-REJECTED (WS-PT-X).
           MOVE CT-SUMMARISED     TO WS-PT-SUMMARISED (WS-PT-X).
           MOVE CT-CARRIED-FWD    TO WS-PT-CFWD (WS-PT-X).
           MOVE CT-HASH-MINUTES   TO WS-PT-HASH-MIN (WS-PT-X).
           MOVE CT-HASH-AMOUNT    TO WS-PT-HASH-AMT (WS-PT-X).
           MOVE CT-BAL-IND        TO WS-PT-BAL-IND (WS-PT-X).
           MOVE CT-RC             TO WS-PT-RC (WS-PT-X).
           MOVE SR-FAMILY         TO WS-PT-FAMILY (WS-PT-X).
           MOVE CT-RERUN-NBR      TO WS-PT-RERUN (WS-PT-X).
           MOVE ' '               TO WS-PT-CHAIN-IND (WS-PT-X).

       P4300-EXIT.
           EXIT.

       P4310-FIND-PROCESS.
           IF WS-PT-PROCESS (WS-PT-X) = CT-PROCESS-ID
               SET WS-SUB1 TO WS-PT-X
               MOVE WS-SUB1 TO WS-PT-HIT
               MOVE 'Y' TO WS-CHAIN-FOUND-SW.

       P4310-EXIT.
           EXIT.

       P4400-ACCUM-FAMILY.
           MOVE 'P4400-ACCUM-FAMILY' TO WS-PARA-NAME.
           MOVE 7 TO WS-FR-SUB.
           PERFORM P4410-FIND-FAMILY THRU P4410-EXIT
               VARYING WS-FT-X FROM 1 BY 1
               UNTIL WS-FT-X > 7.
           ADD 1              TO WS-FM-PROCESSES (WS-FR-SUB).
           ADD CT-READ        TO WS-FM-READ (WS-FR-SUB).
           ADD CT-WRITTEN     TO WS-FM-WRITTEN (WS-FR-SUB).
           ADD CT-REJECTED    TO WS-FM-REJECTED (WS-FR-SUB).
           ADD CT-SUMMARISED  TO WS-FM-SUMMARISED (WS-FR-SUB).
           ADD CT-CARRIED-FWD TO WS-FM-CFWD (WS-FR-SUB).
           ADD CT-HASH-AMOUNT TO WS-FM-AMOUNT (WS-FR-SUB).
           IF CT-OUT-OF-BAL
               ADD 1 TO WS-FM-OUT-OF-BAL (WS-FR-SUB).

       P4400-EXIT.
           EXIT.

       P4410-FIND-FAMILY.
           IF WS-FT-CODE (WS-FT-X) = SR-FAMILY
               SET WS-SUB2 TO WS-FT-X
               MOVE WS-SUB2 TO WS-FR-SUB.

       P4410-EXIT.
           EXIT.

       P4500-ACCUM-RUN.
           ADD CT-READ        TO WS-RT-READ.
           ADD CT-WRITTEN     TO WS-RT-WRITTEN.
           ADD CT-REJECTED    TO WS-RT-REJECTED.
           ADD CT-SUMMARISED  TO WS-RT-SUMMARISED.
           ADD CT-CARRIED-FWD TO WS-RT-CFWD.
           ADD CT-HASH-AMOUNT TO WS-RT-AMOUNT.
           ADD CT-HASH-AMOUNT TO WS-ACC-AMOUNT.
           ADD CT-HASH-MINUTES TO WS-ACC-MINUTES.

       P4500-EXIT.
           EXIT.

       P4600-ADD-EXCEPTION.
           IF WS-EX-USED NOT < WS-EX-MAX
               GO TO P4600-EXIT.
           ADD 1 TO WS-EX-USED.
           SET WS-EX-X TO WS-EX-USED.
           MOVE CT-PROCESS-ID TO WS-EX-PROCESS (WS-EX-X).
           MOVE CT-STEP-SEQ   TO WS-EX-STEP (WS-EX-X).
           MOVE WS-CK-REASON  TO WS-EX-REASON (WS-EX-X).
           MOVE WS-CK-DIFF    TO WS-EX-DIFF (WS-EX-X).

       P4600-EXIT.
           EXIT.

      *****************************************************************
      * S500-THE CHAIN CHECK                                          *
      * THE PROCESS LEVEL EQUATION ONLY PROVES THAT A PROCESS DID NOT *
      * LOSE ANYTHING OF ITS OWN.  THE CHAIN CHECK PROVES THAT WHAT   *
      * ONE PROCESS WROTE IS WHAT THE NEXT ONE READ, WHICH IS THE ONLY*
      * THING THAT PROVES THE RUN END TO END.                         *
      *****************************************************************
       S500-CHAIN SECTION.

       P5000-CHAIN-CHECK.
           MOVE 'P5000-CHAIN-CHECK' TO WS-PARA-NAME.
           IF WS-PE-CHAIN-SW NOT = 'Y'
               GO TO P5000-EXIT.
           PERFORM P5100-ONE-CHAIN THRU P5100-EXIT
               VARYING WS-CT-X FROM 1 BY 1
               UNTIL WS-CT-X > 63.
           PERFORM P5400-MISSING-CHECK THRU P5400-EXIT.

       P5000-EXIT.
           EXIT.

       P5100-ONE-CHAIN.
      * CHECK ONE LINK.  A LINK WITH RULE N IS NOT CHECKED.  A LINK
      * WHOSE PREDECESSOR DID NOT RUN IS REPORTED SEPARATELY - THAT IS
      * A DIFFERENT PROBLEM FROM A COUNT MISMATCH.
           IF WS-CT-RULE (WS-CT-X) = 'N'
               GO TO P5100-EXIT.
           PERFORM P5200-FIND-BOTH THRU P5200-EXIT.
           IF NOT WS-CHAIN-FOUND
               GO TO P5100-EXIT.
           IF NOT WS-PRED-FOUND
               MOVE 'PREDECESSOR DID NOT RUN' TO WS-CK-REASON
               PERFORM P5300-CHAIN-EXCEPTION THRU P5300-EXIT
               GO TO P5100-EXIT.
           SET WS-PT-X TO WS-PT-PRED-HIT.
           IF WS-CT-RULE (WS-CT-X) = 'W'
               MOVE WS-PT-WRITTEN (WS-PT-X) TO WS-CK-PRED-SIDE
           ELSE
               MOVE WS-PT-SUMMARISED (WS-PT-X) TO WS-CK-PRED-SIDE.
           SET WS-PT-X TO WS-PT-HIT.
           MOVE WS-PT-READ (WS-PT-X) TO WS-CK-ACTUAL.
           COMPUTE WS-CK-DIFF = WS-CK-PRED-SIDE - WS-CK-ACTUAL.
           IF WS-CK-DIFF = ZERO
               MOVE 'C' TO WS-PT-CHAIN-IND (WS-PT-X)
               GO TO P5100-EXIT.
           MOVE 'B' TO WS-PT-CHAIN-IND (WS-PT-X).
           MOVE 'CHAIN COUNT MISMATCH' TO WS-CK-REASON.
           ADD 1 TO WS-RT-CHAIN-FAILED.
           MOVE 'N' TO WS-RUN-BAL-SW.
           PERFORM P5300-CHAIN-EXCEPTION THRU P5300-EXIT.

       P5100-EXIT.
           EXIT.

       P5200-FIND-BOTH.
           MOVE 'N' TO WS-CHAIN-FOUND-SW.
           MOVE 'N' TO WS-PRED-FOUND-SW.
           MOVE ZERO TO WS-PT-HIT WS-PT-PRED-HIT.
           PERFORM P5210-SCAN-PROC THRU P5210-EXIT
               VARYING WS-PT-X FROM 1 BY 1
               UNTIL WS-PT-X > WS-PT-USED.

       P5200-EXIT.
           EXIT.

       P5210-SCAN-PROC.
           IF WS-PT-PROCESS (WS-PT-X) = WS-CT-PROCESS (WS-CT-X)
               SET WS-SUB1 TO WS-PT-X
               MOVE WS-SUB1 TO WS-PT-HIT
               MOVE 'Y' TO WS-CHAIN-FOUND-SW.
           IF WS-PT-PROCESS (WS-PT-X) = WS-CT-PRED (WS-CT-X)
               SET WS-SUB2 TO WS-PT-X
               MOVE WS-SUB2 TO WS-PT-PRED-HIT
               MOVE 'Y' TO WS-PRED-FOUND-SW.

       P5210-EXIT.
           EXIT.

       P5300-CHAIN-EXCEPTION.
           IF WS-EX-USED NOT < WS-EX-MAX
               GO TO P5300-EXIT.
           ADD 1 TO WS-EX-USED.
           SET WS-EX-X TO WS-EX-USED.
           MOVE WS-CT-PROCESS (WS-CT-X) TO WS-EX-PROCESS (WS-EX-X).
           MOVE WS-CT-STEP (WS-CT-X)    TO WS-EX-STEP (WS-EX-X).
           MOVE WS-CK-REASON            TO WS-EX-REASON (WS-EX-X).
           MOVE WS-CK-DIFF              TO WS-EX-DIFF (WS-EX-X).

       P5300-EXIT.
           EXIT.

       P5400-MISSING-CHECK.
      * EVERY PROCESS THE CHAIN TABLE EXPECTS MUST HAVE WRITTEN A
      * CONTROL RECORD.  A PROCESS THAT IS EXPECTED AND HAS NOT
      * REPORTED EITHER DID NOT RUN OR FAILED BEFORE P8000.  EITHER
      * WAY THE CYCLE CANNOT BE DECLARED COMPLETE.
           MOVE 'P5400-MISSING-CHECK' TO WS-PARA-NAME.
           IF WS-PT-USED = ZERO
               MOVE 7005 TO WS-AB-CODE
               MOVE 'NO CONTROL RECORDS FOR THIS CYCLE'
                                       TO WS-AB-TEXT
               GO TO P9970-CONTROL-FAILURE.
           PERFORM P5410-EXPECT-ONE THRU P5410-EXIT
               VARYING WS-CT-X FROM 1 BY 1
               UNTIL WS-CT-X > 63.
           IF WS-RT-MISSING > WS-PE-EXPECT-PROC
               MOVE 'N' TO WS-RUN-BAL-SW.

       P5400-EXIT.
           EXIT.

       P5410-EXPECT-ONE.
           MOVE 'N' TO WS-CHAIN-FOUND-SW.
           PERFORM P5420-LOOK-FOR THRU P5420-EXIT
               VARYING WS-PT-X FROM 1 BY 1
               UNTIL WS-PT-X > WS-PT-USED OR WS-CHAIN-FOUND.
           IF WS-CHAIN-FOUND
               GO TO P5410-EXIT.
           ADD 1 TO WS-RT-MISSING.
           MOVE 'EXPECTED PROCESS DID NOT REPORT' TO WS-CK-REASON.
           MOVE ZERO TO WS-CK-DIFF.
           PERFORM P5300-CHAIN-EXCEPTION THRU P5300-EXIT.

       P5410-EXIT.
           EXIT.

       P5420-LOOK-FOR.
           IF WS-PT-PROCESS (WS-PT-X) = WS-CT-PROCESS (WS-CT-X)
               MOVE 'Y' TO WS-CHAIN-FOUND-SW.

       P5420-EXIT.
           EXIT.

       P5500-PROOF-SUMMARY.
      * SUMMARISE THE ACCOUNT LEVEL PROOF FILE WRITTEN BY CABBIL11.
      * THE COUNTS ABOVE PROVE THAT NO RECORD WAS LOST; THIS PROVES
      * THAT THE MONEY ON THE INVOICES ADDS UP.
           MOVE 'P5500-PROOF-SUMMARY' TO WS-PARA-NAME.
           PERFORM P5510-READ-PROOF THRU P5510-EXIT
               UNTIL WS-PRF-EOF.
           DISPLAY 'PROOF RECORDS READ ' WS-PF-ACCOUNTS.

       P5500-EXIT.
           EXIT.

       P5510-READ-PROOF.
           READ PROOF-IN-FILE INTO WS-PROOF-IN
               AT END
                   MOVE 'Y' TO WS-PRF-EOF-SW
                   GO TO P5510-EXIT.
           ADD 1 TO WS-PF-ACCOUNTS.
           ADD WS-PI-DETAIL TO WS-PF-DETAIL-SUM.
           ADD WS-PI-HEADER TO WS-PF-HEADER-SUM.
           ADD WS-PI-DELTA  TO WS-PF-DELTA-SUM.
           IF WS-PI-RESULT = 'IN BALANCE'
               ADD 1 TO WS-PF-IN-BAL
           ELSE
               ADD 1 TO WS-PF-OUT-OF-BAL.
           IF WS-PI-DIFF > WS-PF-WORST
               MOVE WS-PI-DIFF TO WS-PF-WORST
               MOVE WS-PI-BAN  TO WS-PF-WORST-BAN.

       P5510-EXIT.
           EXIT.

      *****************************************************************
      * S600-THE PRINTED REPORT                                       *
      * FIVE PAGE GROUPS - THE RUN SUMMARY, THE FAMILY RECAPS, THE    *
      * CHAIN LISTING, THE EXCEPTION LIST AND THE PROOF SUMMARY.      *
      * EACH GROUP STARTS ON A NEW PAGE AND THE GROUP BOUNDARY CARRIES*
      * THE SECTION BREAK CHARACTER SO THAT THE REPORT CAN BE BURST   *
      * AND DISTRIBUTED TO DIFFERENT TEAMS.                           *
      * CARRIAGE CONTROL PER CABS-STD-063 AND THE MAILROOM SPEC.      *
      *****************************************************************
       S600-REPORT SECTION.

       P6000-PRINT-REPORT.
           MOVE 'P6000-PRINT-REPORT' TO WS-PARA-NAME.
           PERFORM P6100-RUN-SUMMARY THRU P6100-EXIT.
           PERFORM P6201-RECAP-ING THRU P6201-EXIT.
           PERFORM P6202-RECAP-RAT THRU P6202-EXIT.
           PERFORM P6203-RECAP-JUR THRU P6203-EXIT.
           PERFORM P6204-RECAP-SET THRU P6204-EXIT.
           PERFORM P6205-RECAP-BIL THRU P6205-EXIT.
           PERFORM P6206-RECAP-FMT THRU P6206-EXIT.
           PERFORM P6207-RECAP-RPT THRU P6207-EXIT.
           PERFORM P6600-CHAIN-LISTING THRU P6600-EXIT.
           PERFORM P6620-STEP-SEQUENCE-CHECK THRU P6620-EXIT.
           PERFORM P6750-RERUN-LISTING THRU P6750-EXIT.
           PERFORM P6700-EXCEPTION-LIST THRU P6700-EXIT.
           PERFORM P6850-CYCLE-TIMELINE THRU P6850-EXIT.
           PERFORM P6800-PROOF-PAGE THRU P6800-EXIT.
           PERFORM P6900-VERDICT THRU P6900-EXIT.

       P6000-EXIT.
           EXIT.

       P6100-RUN-SUMMARY.
           MOVE 'P6100-RUN-SUMMARY' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '1' TO PC-CC.
           MOVE 'DAILY BALANCING REPORT - RUN SUMMARY' TO PC-TEXT.
           PERFORM P7000-WRITE-PRINT THRU P7000-EXIT.
           PERFORM P6110-TOTAL-LINE THRU P6110-EXIT.

       P6100-EXIT.
           EXIT.

       P6110-TOTAL-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '0' TO PC-CC.
           MOVE 'PROCESSES REPORTED' TO PC-COL-001-020.
           MOVE WS-RT-PROCESSES TO WS-EB-COUNT.
           MOVE WS-EB-COUNT TO PC-COL-021-060.
           PERFORM P7000-WRITE-PRINT THRU P7000-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS READ' TO PC-COL-001-020.
           MOVE WS-RT-READ TO WS-EB-COUNT.
           MOVE WS-EB-COUNT TO PC-COL-021-060.
           PERFORM P7000-WRITE-PRINT THRU P7000-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS WRITTEN' TO PC-COL-001-020.
           MOVE WS-RT-WRITTEN TO WS-EB-COUNT.
           MOVE WS-EB-COUNT TO PC-COL-021-060.
           PERFORM P7000-WRITE-PRINT THRU P7000-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS REJECTED' TO PC-COL-001-020.
           MOVE WS-RT-REJECTED TO WS-EB-COUNT.
           MOVE WS-EB-COUNT TO PC-COL-021-060.
           PERFORM P7000-WRITE-PRINT THRU P7000-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS SUMMARISED' TO PC-COL-001-020.
           MOVE WS-RT-SUMMARISED TO WS-EB-COUNT.
           MOVE WS-EB-COUNT TO PC-COL-021-060.
           PERFORM P7000-WRITE-PRINT THRU P7000-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'CARRIED FORWARD' TO PC-COL-001-020.
           MOVE WS-RT-CFWD TO WS-EB-COUNT.
           MOVE WS-EB-COUNT TO PC-COL-021-060.
           PERFORM P7000-WRITE-PRINT THRU P7000-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '0' TO PC-CC.
           MOVE 'RUN AMOUNT HASH' TO PC-COL-001-020.
           MOVE WS-RT-AMOUNT TO WS-EB-MONEY.
           MOVE WS-EB-MONEY TO PC-COL-021-060.
           PERFORM P7000-WRITE-PRINT THRU P7000-EXIT.

       P6110-EXIT.
           EXIT.

       P6201-RECAP-ING.
      * THE USAGE INGEST RECAP.
      * ONE LINE PER PROCESS IN THIS FAMILY WITH ITS FIVE COUNTS, ITS
      * BALANCE INDICATOR AND ITS CONTRIBUTION TO THE RUN HASH.  THE
      * FAMILY IS DECLARED OUT OF BALANCE WHEN ANY PROCESS IN IT IS.
           MOVE 'P6201-RECAP-ING' TO WS-PARA-NAME.
           MOVE 'ING' TO WS-FR-CODE.
           MOVE 1  TO WS-FR-SUB.
           PERFORM P6300-FAMILY-PAGE THRU P6300-EXIT.
           PERFORM P6400-FAMILY-LINES THRU P6400-EXIT
               VARYING WS-PT-X FROM 1 BY 1
               UNTIL WS-PT-X > WS-PT-USED.
           PERFORM P6500-FAMILY-TOTAL THRU P6500-EXIT.

       P6201-EXIT.
           EXIT.

       P6202-RECAP-RAT.
      * THE ACCESS RATING RECAP.
      * ONE LINE PER PROCESS IN THIS FAMILY WITH ITS FIVE COUNTS, ITS
      * BALANCE INDICATOR AND ITS CONTRIBUTION TO THE RUN HASH.  THE
      * FAMILY IS DECLARED OUT OF BALANCE WHEN ANY PROCESS IN IT IS.
           MOVE 'P6202-RECAP-RAT' TO WS-PARA-NAME.
           MOVE 'RAT' TO WS-FR-CODE.
           MOVE 2  TO WS-FR-SUB.
           PERFORM P6300-FAMILY-PAGE THRU P6300-EXIT.
           PERFORM P6400-FAMILY-LINES THRU P6400-EXIT
               VARYING WS-PT-X FROM 1 BY 1
               UNTIL WS-PT-X > WS-PT-USED.
           PERFORM P6500-FAMILY-TOTAL THRU P6500-EXIT.

       P6202-EXIT.
           EXIT.

       P6203-RECAP-JUR.
      * THE JURISDICTION AND FACTORS RECAP.
      * ONE LINE PER PROCESS IN THIS FAMILY WITH ITS FIVE COUNTS, ITS
      * BALANCE INDICATOR AND ITS CONTRIBUTION TO THE RUN HASH.  THE
      * FAMILY IS DECLARED OUT OF BALANCE WHEN ANY PROCESS IN IT IS.
           MOVE 'P6203-RECAP-JUR' TO WS-PARA-NAME.
           MOVE 'JUR' TO WS-FR-CODE.
           MOVE 3  TO WS-FR-SUB.
           PERFORM P6300-FAMILY-PAGE THRU P6300-EXIT.
           PERFORM P6400-FAMILY-LINES THRU P6400-EXIT
               VARYING WS-PT-X FROM 1 BY 1
               UNTIL WS-PT-X > WS-PT-USED.
           PERFORM P6500-FAMILY-TOTAL THRU P6500-EXIT.

       P6203-EXIT.
           EXIT.

       P6204-RECAP-SET.
      * THE INTER CARRIER SETTLEMENT RECAP.
      * ONE LINE PER PROCESS IN THIS FAMILY WITH ITS FIVE COUNTS, ITS
      * BALANCE INDICATOR AND ITS CONTRIBUTION TO THE RUN HASH.  THE
      * FAMILY IS DECLARED OUT OF BALANCE WHEN ANY PROCESS IN IT IS.
           MOVE 'P6204-RECAP-SET' TO WS-PARA-NAME.
           MOVE 'SET' TO WS-FR-CODE.
           MOVE 4  TO WS-FR-SUB.
           PERFORM P6300-FAMILY-PAGE THRU P6300-EXIT.
           PERFORM P6400-FAMILY-LINES THRU P6400-EXIT
               VARYING WS-PT-X FROM 1 BY 1
               UNTIL WS-PT-X > WS-PT-USED.
           PERFORM P6500-FAMILY-TOTAL THRU P6500-EXIT.

       P6204-EXIT.
           EXIT.

       P6205-RECAP-BIL.
      * THE BILL CALCULATION RECAP.
      * ONE LINE PER PROCESS IN THIS FAMILY WITH ITS FIVE COUNTS, ITS
      * BALANCE INDICATOR AND ITS CONTRIBUTION TO THE RUN HASH.  THE
      * FAMILY IS DECLARED OUT OF BALANCE WHEN ANY PROCESS IN IT IS.
           MOVE 'P6205-RECAP-BIL' TO WS-PARA-NAME.
           MOVE 'BIL' TO WS-FR-CODE.
           MOVE 5  TO WS-FR-SUB.
           PERFORM P6300-FAMILY-PAGE THRU P6300-EXIT.
           PERFORM P6400-FAMILY-LINES THRU P6400-EXIT
               VARYING WS-PT-X FROM 1 BY 1
               UNTIL WS-PT-X > WS-PT-USED.
           PERFORM P6500-FAMILY-TOTAL THRU P6500-EXIT.

       P6205-EXIT.
           EXIT.

       P6206-RECAP-FMT.
      * THE BILL FORMAT AND PRINT RECAP.
      * ONE LINE PER PROCESS IN THIS FAMILY WITH ITS FIVE COUNTS, ITS
      * BALANCE INDICATOR AND ITS CONTRIBUTION TO THE RUN HASH.  THE
      * FAMILY IS DECLARED OUT OF BALANCE WHEN ANY PROCESS IN IT IS.
           MOVE 'P6206-RECAP-FMT' TO WS-PARA-NAME.
           MOVE 'FMT' TO WS-FR-CODE.
           MOVE 6  TO WS-FR-SUB.
           PERFORM P6300-FAMILY-PAGE THRU P6300-EXIT.
           PERFORM P6400-FAMILY-LINES THRU P6400-EXIT
               VARYING WS-PT-X FROM 1 BY 1
               UNTIL WS-PT-X > WS-PT-USED.
           PERFORM P6500-FAMILY-TOTAL THRU P6500-EXIT.

       P6206-EXIT.
           EXIT.

       P6207-RECAP-RPT.
      * THE REPORTING AND CLOSE RECAP.
      * ONE LINE PER PROCESS IN THIS FAMILY WITH ITS FIVE COUNTS, ITS
      * BALANCE INDICATOR AND ITS CONTRIBUTION TO THE RUN HASH.  THE
      * FAMILY IS DECLARED OUT OF BALANCE WHEN ANY PROCESS IN IT IS.
           MOVE 'P6207-RECAP-RPT' TO WS-PARA-NAME.
           MOVE 'RPT' TO WS-FR-CODE.
           MOVE 7  TO WS-FR-SUB.
           PERFORM P6300-FAMILY-PAGE THRU P6300-EXIT.
           PERFORM P6400-FAMILY-LINES THRU P6400-EXIT
               VARYING WS-PT-X FROM 1 BY 1
               UNTIL WS-PT-X > WS-PT-USED.
           PERFORM P6500-FAMILY-TOTAL THRU P6500-EXIT.

       P6207-EXIT.
           EXIT.

       P6300-FAMILY-PAGE.
           MOVE 'P6300-FAMILY-PAGE' TO WS-PARA-NAME.
           MOVE SPACES TO WS-FR-NAME.
           PERFORM P6310-FAMILY-NAME THRU P6310-EXIT
               VARYING WS-FT-X FROM 1 BY 1
               UNTIL WS-FT-X > 7.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '4' TO PC-CC.
           MOVE 'FAMILY RECAP - ' TO PC-COL-001-020.
           MOVE WS-FR-NAME TO PC-COL-021-060.
           PERFORM P7000-WRITE-PRINT THRU P7000-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '0' TO PC-CC.
           MOVE 'PROCESS   STEP' TO PC-COL-001-020.
           MOVE 'READ         WRITTEN' TO PC-COL-021-060.
           MOVE 'REJECTED     SUMMARISED' TO PC-COL-061-090.
           MOVE 'CFWD          BAL CHN' TO PC-COL-091-132.
           PERFORM P7000-WRITE-PRINT THRU P7000-EXIT.

       P6300-EXIT.
           EXIT.

       P6310-FAMILY-NAME.
           IF WS-FT-CODE (WS-FT-X) = WS-FR-CODE
               MOVE WS-FT-NAME (WS-FT-X) TO WS-FR-NAME.

       P6310-EXIT.
           EXIT.

       P6400-FAMILY-LINES.
           IF WS-PT-FAMILY (WS-PT-X) NOT = WS-FR-CODE
               GO TO P6400-EXIT.
           SET WS-SUB1 TO WS-PT-X.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           IF WS-PT-BAL-IND (WS-PT-X) = 'O'
               MOVE '0' TO PC-CC.
           MOVE WS-PT-PROCESS (WS-PT-X) TO PC-COL-001-020.
           MOVE WS-PT-READ (WS-PT-X) TO WS-EB-COUNT.
           MOVE WS-EB-COUNT TO PC-COL-021-060.
           MOVE WS-PT-WRITTEN (WS-PT-X) TO WS-EB-COUNT.
           MOVE WS-EB-COUNT TO PC-COL-061-090.
           MOVE WS-PT-CFWD (WS-PT-X) TO WS-EB-COUNT.
           MOVE WS-EB-COUNT TO PC-COL-091-132.
           PERFORM P7000-WRITE-PRINT THRU P7000-EXIT.

       P6400-EXIT.
           EXIT.

       P6500-FAMILY-TOTAL.
           MOVE 'P6500-FAMILY-TOTAL' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '0' TO PC-CC.
           MOVE 'FAMILY TOTAL' TO PC-COL-001-020.
           MOVE WS-FM-READ (WS-FR-SUB) TO WS-EB-COUNT.
           MOVE WS-EB-COUNT TO PC-COL-021-060.
           MOVE WS-FM-WRITTEN (WS-FR-SUB) TO WS-EB-COUNT.
           MOVE WS-EB-COUNT TO PC-COL-061-090.
           MOVE WS-FM-AMOUNT (WS-FR-SUB) TO WS-EB-MONEY.
           MOVE WS-EB-MONEY TO PC-COL-091-132.
           PERFORM P7000-WRITE-PRINT THRU P7000-EXIT.

       P6500-EXIT.
           EXIT.

       P6600-CHAIN-LISTING.
      * THE CHAIN LISTING.  ONE LINE PER LINK WITH THE PREDECESSOR,
      * THE RULE AND THE RESULT.  THE CONTROL TEAM SIGN THIS PAGE.
           MOVE 'P6600-CHAIN-LISTING' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '4' TO PC-CC.
           MOVE 'PROCESS CHAIN LISTING' TO PC-TEXT.
           PERFORM P7000-WRITE-PRINT THRU P7000-EXIT.
           PERFORM P6610-CHAIN-LINE THRU P6610-EXIT
               VARYING WS-CT-X FROM 1 BY 1
               UNTIL WS-CT-X > 63.

       P6600-EXIT.
           EXIT.

       P6610-CHAIN-LINE.
           IF WS-CT-RULE (WS-CT-X) = 'N'
               GO TO P6610-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-CT-PROCESS (WS-CT-X) TO PC-COL-001-020.
           MOVE WS-CT-PRED (WS-CT-X)    TO PC-COL-021-060.
           MOVE WS-CT-RULE (WS-CT-X)    TO PC-COL-061-090.
           PERFORM P7000-WRITE-PRINT THRU P7000-EXIT.

       P6610-EXIT.
           EXIT.

       P6700-EXCEPTION-LIST.
           MOVE 'P6700-EXCEPTION-LIST' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '4' TO PC-CC.
           MOVE 'BALANCING EXCEPTIONS' TO PC-TEXT.
           PERFORM P7000-WRITE-PRINT THRU P7000-EXIT.
           IF WS-EX-USED = ZERO
               MOVE SPACES TO CABS-PRINT-LINE
               MOVE '0' TO PC-CC
               MOVE 'NO EXCEPTIONS RAISED THIS CYCLE' TO PC-TEXT
               PERFORM P7000-WRITE-PRINT THRU P7000-EXIT
               GO TO P6700-EXIT.
           PERFORM P6710-EXCEPTION-LINE THRU P6710-EXIT
               VARYING WS-EX-X FROM 1 BY 1
               UNTIL WS-EX-X > WS-EX-USED.

       P6700-EXIT.
           EXIT.

       P6710-EXCEPTION-LINE.
           SET WS-SUB1 TO WS-EX-X.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-EX-PROCESS (WS-EX-X) TO PC-COL-001-020.
           MOVE WS-EX-REASON (WS-EX-X)  TO PC-COL-021-060.
           MOVE WS-EX-DIFF (WS-EX-X) TO WS-EB-DIFF.
           MOVE WS-EB-DIFF TO PC-COL-061-090.
           PERFORM P7000-WRITE-PRINT THRU P7000-EXIT.

       P6710-EXIT.
           EXIT.

       P6800-PROOF-PAGE.
           MOVE 'P6800-PROOF-PAGE' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '4' TO PC-CC.
           MOVE 'INVOICE PROOF SUMMARY' TO PC-TEXT.
           PERFORM P7000-WRITE-PRINT THRU P7000-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '0' TO PC-CC.
           MOVE 'ACCOUNTS PROVED' TO PC-COL-001-020.
           MOVE WS-PF-ACCOUNTS TO WS-EB-COUNT.
           MOVE WS-EB-COUNT TO PC-COL-021-060.
           PERFORM P7000-WRITE-PRINT THRU P7000-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'ACCOUNTS IN BALANCE' TO PC-COL-001-020.
           MOVE WS-PF-IN-BAL TO WS-EB-COUNT.
           MOVE WS-EB-COUNT TO PC-COL-021-060.
           PERFORM P7000-WRITE-PRINT THRU P7000-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'ACCOUNTS OUT OF BAL' TO PC-COL-001-020.
           MOVE WS-PF-OUT-OF-BAL TO WS-EB-COUNT.
           MOVE WS-EB-COUNT TO PC-COL-021-060.
           PERFORM P7000-WRITE-PRINT THRU P7000-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL SIDE TOTAL' TO PC-COL-001-020.
           MOVE WS-PF-DETAIL-SUM TO WS-EB-MONEY.
           MOVE WS-EB-MONEY TO PC-COL-021-060.
           PERFORM P7000-WRITE-PRINT THRU P7000-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'HEADER SIDE TOTAL' TO PC-COL-001-020.
           MOVE WS-PF-HEADER-SUM TO WS-EB-MONEY.
           MOVE WS-EB-MONEY TO PC-COL-021-060.
           PERFORM P7000-WRITE-PRINT THRU P7000-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'ROUNDING RESIDUE' TO PC-COL-001-020.
           MOVE WS-PF-DELTA-SUM TO WS-EB-MONEY.
           MOVE WS-EB-MONEY TO PC-COL-021-060.
           PERFORM P7000-WRITE-PRINT THRU P7000-EXIT.

       P6800-EXIT.
           EXIT.

       P6850-CYCLE-TIMELINE.
      * THE TIMELINE PAGE.  THE PROCESSES IN STEP SEQUENCE ORDER WITH
      * THE JOB AND STEP THAT RAN EACH ONE.  OPERATIONS USE IT TO SEE
      * WHERE A CYCLE STOPPED WITHOUT READING THE JOB LOG.
           MOVE 'P6850-CYCLE-TIMELINE' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '4' TO PC-CC.
           MOVE 'CYCLE TIMELINE IN STEP SEQUENCE ORDER' TO PC-TEXT.
           PERFORM P7000-WRITE-PRINT THRU P7000-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '0' TO PC-CC.
           MOVE 'STEP  PROCESS' TO PC-COL-001-020.
           MOVE 'JOBNAME       STEPNAME' TO PC-COL-021-060.
           MOVE 'BAL  CHAIN   RC' TO PC-COL-061-090.
           PERFORM P7000-WRITE-PRINT THRU P7000-EXIT.
           PERFORM P6860-TIMELINE-LINE THRU P6860-EXIT
               VARYING WS-CT-X FROM 1 BY 1
               UNTIL WS-CT-X > 63.

       P6850-EXIT.
           EXIT.

       P6860-TIMELINE-LINE.
           MOVE 'N' TO WS-CHAIN-FOUND-SW.
           MOVE ZERO TO WS-PT-HIT.
           PERFORM P6640-FIND-STEP THRU P6640-EXIT
               VARYING WS-PT-X FROM 1 BY 1
               UNTIL WS-PT-X > WS-PT-USED OR WS-CHAIN-FOUND.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-CT-PROCESS (WS-CT-X) TO PC-COL-001-020.
           IF NOT WS-CHAIN-FOUND
               MOVE 'DID NOT REPORT' TO PC-COL-021-060
               PERFORM P7000-WRITE-PRINT THRU P7000-EXIT
               GO TO P6860-EXIT.
           SET WS-PT-X TO WS-PT-HIT.
           MOVE WS-PT-JOBNAME (WS-PT-X)  TO PC-COL-021-060.
           MOVE WS-PT-STEPNAME (WS-PT-X) TO PC-COL-061-090.
           MOVE WS-PT-BAL-IND (WS-PT-X)  TO PC-COL-091-132.
           PERFORM P7000-WRITE-PRINT THRU P7000-EXIT.

       P6860-EXIT.
           EXIT.

       P6900-VERDICT.
      * THE VERDICT LINE.  OPERATIONS READ THIS LINE AND NOTHING ELSE
      * WHEN THEY DECIDE WHETHER TO RELEASE THE PRINT STREAM.
           MOVE 'P6900-VERDICT' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '-' TO PC-CC.
           IF WS-RUN-IN-BAL
               MOVE 'CYCLE IS IN BALANCE - RELEASE PERMITTED'
                                       TO PC-TEXT
           ELSE
               MOVE 'CYCLE IS OUT OF BALANCE - DO NOT RELEASE'
                                       TO PC-TEXT
               MOVE 0008 TO WS-RETURN-CODE.
           PERFORM P7000-WRITE-PRINT THRU P7000-EXIT.
           IF NOT WS-RUN-IN-BAL
               IF WS-PE-HALT-SW = 'Y'
                   MOVE 0012 TO WS-RETURN-CODE.

       P6900-EXIT.
           EXIT.

      *****************************************************************
      * S700-OUTPUT                                                   *
      *****************************************************************
       S700-OUTPUT SECTION.

       P7000-WRITE-PRINT.
           WRITE BAL-RECORD FROM CABS-PRINT-LINE.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 7006 TO WS-AB-CODE
               MOVE 'BALANCING REPORT WRITE FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-WRITE-CNT.

       P7000-EXIT.
           EXIT.

       P5600-HASH-CONTINUITY.
      * THE HASH CONTINUITY CHECK.  WHERE A LINK CARRIES A RULE THE
      * AMOUNT HASH OF THE PREDECESSOR SHOULD ALSO CARRY FORWARD.  IT
      * DOES NOT ALWAYS - A PROCESS THAT REPRICES CHANGES THE MONEY
      * LEGITIMATELY - SO A HASH DIFFERENCE IS REPORTED AND NOT
      * TREATED AS A FAILURE OF THE CYCLE.
           MOVE 'P5600-HASH-CONTINUITY' TO WS-PARA-NAME.
           IF WS-PE-HASH-SW NOT = 'Y'
               GO TO P5600-EXIT.
           PERFORM P5610-ONE-HASH THRU P5610-EXIT
               VARYING WS-CT-X FROM 1 BY 1
               UNTIL WS-CT-X > 63.

       P5600-EXIT.
           EXIT.

       P5610-ONE-HASH.
           IF WS-CT-RULE (WS-CT-X) = 'N'
               GO TO P5610-EXIT.
           PERFORM P5200-FIND-BOTH THRU P5200-EXIT.
           IF NOT WS-CHAIN-FOUND
               GO TO P5610-EXIT.
           IF NOT WS-PRED-FOUND
               GO TO P5610-EXIT.
           SET WS-PT-X TO WS-PT-PRED-HIT.
           MOVE WS-PT-HASH-AMT (WS-PT-X) TO WS-CK-HASH-DIFF.
           SET WS-PT-X TO WS-PT-HIT.
           COMPUTE WS-CK-HASH-DIFF =
                   WS-CK-HASH-DIFF - WS-PT-HASH-AMT (WS-PT-X).
           IF WS-CK-HASH-DIFF = ZERO
               GO TO P5610-EXIT.
           ADD 1 TO WS-RT-HASH-FAILED.
           MOVE 'AMOUNT HASH NOT CARRIED FORWARD' TO WS-CK-REASON.
           MOVE ZERO TO WS-CK-DIFF.
           PERFORM P5300-CHAIN-EXCEPTION THRU P5300-EXIT.

       P5610-EXIT.
           EXIT.

       P6620-STEP-SEQUENCE-CHECK.
      * THE STEP SEQUENCE ON A CONTROL RECORD IS SET BY THE PROGRAM,
      * NOT BY THE JCL.  A PROCESS THAT REPORTS A STEP SEQUENCE THE
      * CHAIN TABLE DOES NOT EXPECT HAS EITHER BEEN RECOMPILED WITH A
      * NEW VALUE OR IS RUNNING OUT OF ITS PLACE IN THE STREAM.
           MOVE 'P6620-STEP-SEQUENCE-CHECK' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '4' TO PC-CC.
           MOVE 'STEP SEQUENCE VERIFICATION' TO PC-TEXT.
           PERFORM P7000-WRITE-PRINT THRU P7000-EXIT.
           PERFORM P6630-ONE-STEP THRU P6630-EXIT
               VARYING WS-CT-X FROM 1 BY 1
               UNTIL WS-CT-X > 63.

       P6620-EXIT.
           EXIT.

       P6630-ONE-STEP.
           MOVE 'N' TO WS-CHAIN-FOUND-SW.
           MOVE ZERO TO WS-PT-HIT.
           PERFORM P6640-FIND-STEP THRU P6640-EXIT
               VARYING WS-PT-X FROM 1 BY 1
               UNTIL WS-PT-X > WS-PT-USED OR WS-CHAIN-FOUND.
           IF NOT WS-CHAIN-FOUND
               GO TO P6630-EXIT.
           SET WS-PT-X TO WS-PT-HIT.
           IF WS-PT-STEP (WS-PT-X) = WS-CT-STEP (WS-CT-X)
               GO TO P6630-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-CT-PROCESS (WS-CT-X) TO PC-COL-001-020.
           MOVE 'STEP SEQUENCE DIFFERS FROM TABLE' TO PC-COL-021-060.
           MOVE WS-PT-STEP (WS-PT-X) TO WS-EB-DIFF.
           MOVE WS-EB-DIFF TO PC-COL-061-090.
           PERFORM P7000-WRITE-PRINT THRU P7000-EXIT.

       P6630-EXIT.
           EXIT.

       P6640-FIND-STEP.
           IF WS-PT-PROCESS (WS-PT-X) = WS-CT-PROCESS (WS-CT-X)
               SET WS-SUB1 TO WS-PT-X
               MOVE WS-SUB1 TO WS-PT-HIT
               MOVE 'Y' TO WS-CHAIN-FOUND-SW.

       P6640-EXIT.
           EXIT.

       P6750-RERUN-LISTING.
      * THE RERUN LISTING.  ANY PROCESS THAT REPORTED A NON ZERO RERUN
      * NUMBER RAN MORE THAN ONCE THIS CYCLE.  THE CONTROL TEAM CHECK
      * THAT EACH ONE WAS AUTHORISED.
           MOVE 'P6750-RERUN-LISTING' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '4' TO PC-CC.
           MOVE 'PROCESSES RERUN THIS CYCLE' TO PC-TEXT.
           PERFORM P7000-WRITE-PRINT THRU P7000-EXIT.
           PERFORM P6760-RERUN-LINE THRU P6760-EXIT
               VARYING WS-PT-X FROM 1 BY 1
               UNTIL WS-PT-X > WS-PT-USED.

       P6750-EXIT.
           EXIT.

       P6760-RERUN-LINE.
           IF WS-PT-RERUN (WS-PT-X) = ZERO
               GO TO P6760-EXIT.
           SET WS-SUB1 TO WS-PT-X.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-PT-PROCESS (WS-PT-X)  TO PC-COL-001-020.
           MOVE WS-PT-JOBNAME (WS-PT-X)  TO PC-COL-021-060.
           MOVE WS-PT-STEPNAME (WS-PT-X) TO PC-COL-061-090.
           MOVE WS-PT-RERUN (WS-PT-X) TO WS-EB-DIFF.
           MOVE WS-EB-DIFF TO PC-COL-091-132.
           PERFORM P7000-WRITE-PRINT THRU P7000-EXIT.

       P6760-EXIT.
           EXIT.

       P7100-CLEAR-FAMILY.
           MOVE ZERO TO WS-FM-PROCESSES (WS-SUB1)
                        WS-FM-READ (WS-SUB1)
                        WS-FM-WRITTEN (WS-SUB1)
                        WS-FM-REJECTED (WS-SUB1)
                        WS-FM-SUMMARISED (WS-SUB1)
                        WS-FM-CFWD (WS-SUB1)
                        WS-FM-AMOUNT (WS-SUB1)
                        WS-FM-OUT-OF-BAL (WS-SUB1).

       P7100-EXIT.
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
           MOVE 'CABRPT01  DAILY BALANCING REPORT REGISTER'
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
           MOVE 'PROCESS   FAMILY   READ         WRITTEN      RESULT'
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
           MOVE 600                    TO CT-STEP-SEQ.
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
           DISPLAY 'PROCESSES REPORTED' WS-RT-PROCESSES.
           DISPLAY 'EQUATION FAILURES ' WS-RT-EQN-FAILED.
           DISPLAY 'CHAIN FAILURES    ' WS-RT-CHAIN-FAILED.
           DISPLAY 'MISSING PROCESSES ' WS-RT-MISSING.
           DISPLAY 'RERUN PROCESSES   ' WS-RT-RERUN.
           DISPLAY 'NON ZERO RC       ' WS-RT-RC-NONZERO.
           DISPLAY 'ACCOUNTS PROVED   ' WS-PF-ACCOUNTS.
           DISPLAY 'ACCOUNTS OUT OF B ' WS-PF-OUT-OF-BAL.
           DISPLAY 'WORST ACCOUNT     ' WS-PF-WORST-BAN.
           DISPLAY 'RUN BALANCE       ' WS-RUN-BAL-SW.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE CTL-IN-FILE
                 PROOF-IN-FILE
                 BAL-OUT-FILE
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

       P9970-CONTROL-FAILURE.
      * THE CONTROL FILE ITSELF COULD NOT BE READ OR SORTED, OR IT
      * CARRIES A RECORD THAT CANNOT BE ATTRIBUTED TO A PROCESS.  THE
      * BALANCING REPORT IS THE ONLY EVIDENCE THAT THE CYCLE COMPLETED
      * CORRECTLY, SO A REPORT THAT CANNOT BE PRODUCED IS A FAILED
      * CYCLE, NOT A FAILED REPORT.
      * REACHED BY GO TO FROM P2000, P3100, P3200, P4300 AND P5400.
      * EXIT PLACEMENT AGREED AT THE 1998 CODE REVIEW.
           DISPLAY '*** CONTROL FILE FAILURE IN ' WS-PGM-NAME.
           DISPLAY '*** INPUT STATUS ' WS-FS-INPUT.
           DISPLAY '*** PROCESSES LOADED ' WS-PT-USED.
           DISPLAY '*** LAST PROCESS ' CT-PROCESS-ID.
           MOVE CT-PROCESS-ID TO WS-RESTART-KEY.
           MOVE 0012 TO WS-RETURN-CODE.
           PERFORM P9500-ABEND THRU P9500-EXIT.

       P9970-EXIT.
           EXIT.
