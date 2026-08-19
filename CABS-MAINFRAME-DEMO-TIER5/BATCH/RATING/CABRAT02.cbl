      *****************************************************************
      * CABRAT02 - ACCESS RATING DRIVER AND RATE ELEMENT DISPATCHER   *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               RATIN   TELCABS.CABS.USAGE.CLEAN(0) CABSCDR    *
      *               RATEMST TELCABS.CABS.RATE (VSAM KSDS) CABSRATE *
      *               CARRMST TELCABS.CABS.CARRIER(VSAM KSDS)CABSCARR*
      *               PARMIN  INSTREAM SYSIN               (LOCAL)   *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               RATOUT  TELCABS.CABS.RATED(+1)        (LOCAL)   *
      *               BDTLOUT TELCABS.CABS.BILLDTL(+1)      CABSBILL  *
      *               SUSOUT  TELCABS.CABS.USAGE.SUSPENSE(+1)CABSERR  *
      *               RPTOUT  SYSOUT CLASS A                CABSPRNT  *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +             *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * REVISION HISTORY                                              *
      *   V1.00  1988-09-12  R.T.WHEELER  INITIAL RELEASE - DRIVES    *
      *                      ORIGAC AND TERMAC ONLY, ALL OTHER        *
      *                      ELEMENTS WERE RATED DIRECTLY BY CABRAT03 *
      *   V1.02  1989-11-30  R.T.WHEELER  ADDED LOCAL TRANSPORT AND   *
      *                      TANDEM SWITCHING TO THE DISPATCH LIST    *
      *   V1.05  1991-04-08  D.OKONKWO    RATE TABLE LOAD CONVERTED   *
      *                      FROM SEQUENTIAL FILE TO VSAM KSDS BROWSE *
      *   V1.09  1993-08-19  D.OKONKWO    ADDED CARRIER COMMON LINE   *
      *                      ELEMENT, FORCED LAST IN DISPATCH ORDER   *
      *   V1.14  1996-02-27  J.M.CASTILLO Y2K REMEDIATION - CENTURY   *
      *                      PIVOT AT YY=70 INTRODUCED THROUGHOUT     *
      *   V2.00  1999-06-15  J.M.CASTILLO DYNAMIC CALL DISPATCH       *
      *                      INTRODUCED, SUFFIX TAKEN FROM THE RATE   *
      *                      TABLE ROW THAT WINS THE SEARCH            *
      *   V2.03  2002-10-03  P.NAIR       ELEMENT APPLICABILITY       *
      *                      MATRIX REPLACES THE OLD FIVE-ELEMENT     *
      *                      FIXED LIST                                *
      *   V2.05  2005-01-21  P.NAIR       OPERATOR SERVICES ELEMENTS  *
      *                      NOW ROUTE THROUGH THIS DRIVER INSTEAD    *
      *                      OF BEING SKIPPED AT THE INGEST LAYER     *
      *   V2.07  2008-07-09  A.BUKOWSKI   OVERRIDE CARD DISPATCH      *
      *                      ADDED FOR EMERGENCY RATE CORRECTIONS     *
      *   V2.09  2011-12-02  A.BUKOWSKI   RETRY LOGIC ADDED, UP TO    *
      *                      THREE ATTEMPTS PER ELEMENT ON A          *
      *                      MODULE-NOT-FOUND CONDITION                *
      *   V2.11  2015-06-30  L.FERREIRA   RECOMPILE ONLY - LE V6      *
      *                      UPGRADE, NO LOGIC CHANGE                  *
      *   V2.13  2019-03-11  T.VANCE      BAND POOL OFFSET LOGIC      *
      *                      ALIGNED WITH CABRAT01 FOR CONSISTENCY    *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABRAT02.
       AUTHOR. TELCABS APPLICATIONS - RATING TEAM.
      *****************************************************************
      * THIS IS THE CONTROL PROGRAM OF THE RATING STREAM.  IT READS   *
      * THE VALIDATED CLEAN USAGE FILE, RESOLVES WHICH RATE ELEMENTS  *
      * APPLY TO EACH CDR VIA THE ELEMENT APPLICABILITY MATRIX, AND   *
      * DISPATCHES EACH ELEMENT TO THE CORRECT RATING MODULE - SOME   *
      * IN-LINE, MOST BY DYNAMIC CALL WHOSE TARGET IS ASSEMBLED AT    *
      * RUN TIME FROM THE RATE TABLE ROW THAT MATCHED THE SEARCH.     *
      * IT COLLECTS THE RETURNED AMOUNTS, BUILDS THE RATED OUTPUT     *
      * RECORD AND THE BILL DETAIL RECORD, HANDLES MODULE FAILURE     *
      * AND BALANCES.  READ CABSRT01 THROUGH CABSRT04 BEFORE TOUCHING *
      * ANY OF THE DISPATCH LOGIC IN S400.                             *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           C01 IS TO-NEW-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RATIN ASSIGN TO UT-S-RATIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT RATEMST ASSIGN TO DA-RATEMST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS RT-KEY
               FILE STATUS IS WS-FS-TABLE.
           SELECT CARRMST ASSIGN TO DA-CARRMST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CR-KEY
               FILE STATUS IS WS-FS-TABLE.
           SELECT RATOUT ASSIGN TO UT-S-RATOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT BDTLOUT ASSIGN TO UT-S-BDTLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT SUSOUT ASSIGN TO UT-S-SUSOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-SUSPENSE.
           SELECT CTLOUT ASSIGN TO UT-S-CTLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CONTROL.
           SELECT RPTOUT ASSIGN TO UT-S-RPTOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
       DATA DIVISION.
       FILE SECTION.
      * RATIN - VALIDATED, CLEANED USAGE.  ANY RECORD TYPE 01-08.     *
       FD  RATIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       COPY CABSCDR.
      * RATEMST - ACCESS RATE TABLE, VSAM KSDS, KEYED BROWSE AT INIT. *
       FD  RATEMST
           LABEL RECORDS ARE STANDARD.
       COPY CABSRATE.
      * CARRMST - CARRIER MASTER, VSAM KSDS, BROWSED INTO A CACHE AT  *
      * INIT SO THE MAIN LOOP NEVER TAKES A RANDOM I/O PER CDR.       *
       FD  CARRMST
           LABEL RECORDS ARE STANDARD.
       COPY CABSCARR.
      * RATOUT - RATED CDR PASS-THROUGH.  LOCAL LAYOUT, RATING        *
      * OUTPUT ONLY, CARRIES OVERALL RATE STATUS AND TOTAL AMOUNT.    *
       FD  RATOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       01  CABS-RATOUT-RECORD.
           05  RO-OCN                      PIC X(04).
           05  RO-BAN                      PIC X(13).
           05  RO-SEQ-NBR                  PIC 9(09) COMP-3.
           05  RO-RATE-STATUS              PIC X(01).
               88  RO-RATED                VALUE 'R'.
               88  RO-SUSPENDED            VALUE 'S'.
               88  RO-PARTIAL              VALUE 'P'.
           05  RO-CYCLE-YYDDD              PIC 9(05).
           05  RO-JURIS-CD                 PIC X(01).
           05  RO-STATE-CD                 PIC X(02).
           05  RO-REC-TYPE                 PIC X(02).
           05  RO-ELEM-COUNT               PIC 9(02).
           05  RO-DISPATCH-CLASS           PIC X(01).
           05  RO-TOT-QTY                  PIC S9(11)V9(02) COMP-3.
           05  RO-TOT-AMOUNT               PIC S9(11)V9(05) COMP-3.
           05  RO-FILLER                   PIC X(148).
      * BDTLOUT - BILL DETAIL, VARIABLE LENGTH, UP TO 40 ELEMENTS.    *
       FD  BDTLOUT
           RECORDING MODE IS V
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD IS VARYING IN SIZE FROM 108 TO 1647 CHARACTERS
               DEPENDING ON BD-ELEM-CNT.
       COPY CABSBILL.
      * SUSOUT - REJECTED / SUSPENDED USAGE.  CABS-SUSPENSE-RECORD    *
      * LAYOUT FROM CABSERR, RESTATED HERE FOR THE FD (CABSERR IS     *
      * NESTED INSIDE CABSWRK, NOT DIRECTLY COPYABLE INTO THE FD).    *
       FD  SUSOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 300 CHARACTERS.
       01  CABS-SUSPENSE-RECORD-FD.
           05  FD-SU-ERR-CODE              PIC X(04).
           05  FD-SU-ERR-SEVERITY          PIC X(01).
           05  FD-SU-DETECT-PGM            PIC X(08).
           05  FD-SU-DETECT-PARA           PIC X(30).
           05  FD-SU-RUN-ID                PIC X(12).
           05  FD-SU-ORIG-RECORD           PIC X(200).
           05  FD-SU-FILLER                PIC X(45).
      * CTLOUT - RUN CONTROL / BALANCING RECORD.                      *
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD              PIC X(180).
      * RPTOUT - PRINT REPORT, PER-ELEMENT DISPATCH SUMMARY.          *
       FD  RPTOUT
           RECORDING MODE IS FA
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       COPY CABSPRNT.
       WORKING-STORAGE SECTION.
      * STANDARD SHARED WORKING STORAGE - SWITCHES, COUNTERS, ERROR   *
      * CODES, DATE WORK, CONTROL RECORD.  SEE CABSWRK.               *
       COPY CABSWRK.
      * RATING FAMILY CONTROL BLOCKS - RUN CONTROL, RATE TABLE, BAND  *
      * POOL, ROUNDING WORK.  ONE COPY STATEMENT PULLS ALL FOUR.      *
      * EVERY RATING PROGRAM MUST COPY THIS - SEE CABSRT01 HEADER.    *
       COPY CABSRT01.
      * PROGRAM CONSTANTS AND LITERALS                                *
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                  PIC X(08) VALUE 'CABRAT02'.
           05  WS-PGM-VERSION               PIC X(05) VALUE 'V2.13'.
           05  WS-MAX-ELEMENTS-BD           PIC 9(02) VALUE 40.
           05  WS-MAX-ELEMENTS-LIST         PIC 9(02) VALUE 60.
           05  WS-SUPPRESS-THRESHOLD        PIC S9(03)V9(05) COMP-3
                                                            VALUE 0.005.
           05  WS-ELEM-CCLINE               PIC X(06) VALUE 'CCLINE'.
           05  WS-ELEM-RECIPC                PIC X(06) VALUE 'RECIPC'.
      * SYSIN PARM CARD.  BYTE 1 IS A FORMAT FLAG - 'P' MEANS THE     *
      * LEGACY FIXED POSITIONAL LAYOUT IN USE SINCE V1.00, ANYTHING   *
      * ELSE MEANS THE COMMA-SEPARATED FREE-FORM LAYOUT INTRODUCED    *
       01  WS-PARM-CARD                     PIC X(80).
       01  WS-PARM-CARD-POS REDEFINES WS-PARM-CARD.
           05  PC-POS-FORMAT-FLAG            PIC X(01).
           05  PC-POS-RUN-ID                 PIC X(12).
           05  PC-POS-CYCLE-YYDDD            PIC 9(05).
           05  PC-POS-BILL-PERIOD            PIC 9(06).
           05  PC-POS-TARIFF-CD              PIC X(04).
           05  PC-POS-MODE-SW                PIC X(01).
           05  PC-POS-RESTART-KEY            PIC X(26).
           05  PC-POS-FILLER                 PIC X(25).
       01  WS-PARM-CARD-CSV REDEFINES WS-PARM-CARD.
           05  PC-CSV-FORMAT-FLAG            PIC X(01).
           05  PC-CSV-REST                   PIC X(79).
      * UNSTRING RECEIVING FIELDS - THE COMMA-SEPARATED CARD SPLITS   *
      * INTO SIX FIELDS: RUN-ID, CYCLE, BILL PERIOD, TARIFF, MODE     *
      * AND RESTART KEY.  COUNT IN CAPTURES HOW MANY BYTES LANDED IN  *
       01  WS-PARM-UNSTRING-WORK.
           05  WS-UNS-RUN-ID                 PIC X(12).
           05  WS-UNS-RUN-ID-LEN             PIC 9(02) COMP-3.
           05  WS-UNS-CYCLE                  PIC X(05).
           05  WS-UNS-CYCLE-LEN              PIC 9(02) COMP-3.
           05  WS-UNS-BILL-PERIOD            PIC X(06).
           05  WS-UNS-BP-LEN                 PIC 9(02) COMP-3.
           05  WS-UNS-TARIFF                 PIC X(04).
           05  WS-UNS-TARIFF-LEN             PIC 9(02) COMP-3.
           05  WS-UNS-MODE                   PIC X(01).
           05  WS-UNS-MODE-LEN               PIC 9(02) COMP-3.
           05  WS-UNS-RESTART-KEY            PIC X(26).
           05  WS-UNS-RESTART-LEN            PIC 9(02) COMP-3.
           05  WS-UNS-FIELD-CNT              PIC 9(02) COMP-3 VALUE 0.
      * VALIDATED PARM VALUES - WHAT P1250-VALIDATE-PARM HANDS TO     *
      * THE REST OF THE PROGRAM ONCE BOTH LAYOUTS HAVE BEEN RESOLVED  *
      * DOWN TO ONE COMMON SET OF FIELDS.                             *
       01  WS-PARM-VALIDATED.
           05  WS-PV-RUN-ID                  PIC X(12).
           05  WS-PV-CYCLE-YYDDD             PIC 9(05).
           05  WS-PV-BILL-PERIOD             PIC 9(06).
           05  WS-PV-TARIFF-CD                PIC X(04).
           05  WS-PV-MODE-SW                 PIC X(01).
           05  WS-PV-RESTART-KEY.
               10  WS-PV-RK-OVR-FLAG          PIC X(01).
               10  WS-PV-RK-OVR-TARGET        PIC X(08).
               10  WS-PV-RK-OVR-FILTER        PIC X(06).
               10  WS-PV-RK-FILLER            PIC X(11).
           05  WS-PV-OVERRIDE-TARGET         PIC X(08).
           05  WS-PV-VALID-SW                PIC X(01) VALUE 'Y'.
               88  WS-PV-VALID                VALUE 'Y'.
               88  WS-PV-INVALID              VALUE 'N'.
      * DYNAMIC CALL TARGET - THE 8-BYTE FIELD ACTUALLY USED ON EVERY *
      * CALL STATEMENT IN S400.  REDEFINED AS A 6-BYTE PREFIX PLUS A  *
      * 2-BYTE SUFFIX SO EACH DISPATCH PARAGRAPH CAN BUILD IT FROM    *
       01  WS-DISPATCH-TARGET                PIC X(08) VALUE SPACES.
       01  WS-DISPATCH-TARGET-R REDEFINES WS-DISPATCH-TARGET.
           05  WS-DT-PREFIX                  PIC X(06).
           05  WS-DT-SUFFIX                  PIC X(02).
      * JURISDICTION-KEYED SUFFIX TABLE FOR P4200-DISPATCH-JURIS-     *
      * MODULE.  A SMALL CONSTANT TABLE, NOT LOADED FROM ANY FILE -   *
      * INTERSTATE, INTRASTATE AND LOCAL EACH ROUTE TO A DIFFERENT    *
       01  WS-JURIS-SUFFIX-TABLE.
           05  WS-JS-ENTRY OCCURS 4 TIMES INDEXED BY WS-JS-X.
               10  WS-JS-JURIS-CD             PIC X(01) VALUE SPACES.
               10  WS-JS-SUFFIX               PIC X(02) VALUE SPACES.
       01  WS-JURIS-SUFFIX-SEARCH-WORK.
           05  WS-JS-FOUND-SW                PIC X(01) VALUE 'N'.
               88  WS-JS-MATCHED               VALUE 'Y'.
           05  WS-JS-MATCH-SFX               PIC X(02) VALUE SPACES.
      * OVERRIDE DISPATCH CONTROL - P4600-DISPATCH-OVERRIDE READS THE *
      * TARGET STRAIGHT OFF THE PARM CARD RESTART-KEY FIELD WHEN THE  *
      * FIRST BYTE OF THAT FIELD IS '*' (AN EMERGENCY RATE CORRECTION *
       01  WS-OVERRIDE-CONTROL.
           05  WS-OVR-ACTIVE-SW              PIC X(01) VALUE 'N'.
               88  WS-OVR-ACTIVE              VALUE 'Y'.
           05  WS-OVR-TARGET                 PIC X(08) VALUE SPACES.
           05  WS-OVR-ELEM-FILTER            PIC X(06) VALUE SPACES.
      * RETRY DISPATCH CONTROL - P4700-DISPATCH-RETRY FALLS BACK TO A *
      * SECONDARY TARGET WHEN THE PRIMARY MODULE-NOT-FOUND CONDITION  *
      * FIRES.  THE SECONDARY TARGET IS BUILT FROM A DIFFERENT COLUMN *
       01  WS-RETRY-CONTROL.
           05  WS-RTY-ATTEMPTED-SW           PIC X(01) VALUE 'N'.
               88  WS-RTY-ATTEMPTED           VALUE 'Y'.
           05  WS-RTY-SECONDARY-SFX          PIC X(02) VALUE 'UN'.
           05  WS-RTY-SUCCESS-SW             PIC X(01) VALUE 'N'.
               88  WS-RTY-SUCCEEDED           VALUE 'Y'.
      * LINKAGE PARAMETER BLOCK - 240 BYTES, PASSED ON EVERY DYNAMIC  *
      * CALL IN S400.  THE CALLED MODULE READS THE HEADER AND QTY-IN, *
      * FILLS IN LP-AMT-OUT, LP-RETURN-CODE AND LP-RETURN-MSG, AND    *
       01  WS-LINK-PARM-BLOCK.
           05  LP-RUN-ID                     PIC X(12).
           05  LP-PROCESS-ID                 PIC X(08).
           05  LP-ELEM-CODE                  PIC X(06).
           05  LP-MODULE-TARGET              PIC X(08).
           05  LP-REC-TYPE                   PIC X(02).
           05  LP-USAGE-TYPE                 PIC X(01).
           05  LP-JURIS-CD                   PIC X(01).
           05  LP-STATE-CD                   PIC X(02).
           05  LP-OCN                        PIC X(04).
           05  LP-BAN                        PIC X(13).
           05  LP-SEQ-NBR                    PIC 9(09) COMP-3.
           05  LP-QTY-IN                     PIC S9(13)V9(02) COMP-3.
           05  LP-RATE-INITIAL                PIC S9(05)V9(05) COMP-3.
           05  LP-RATE-ADDL                   PIC S9(05)V9(05) COMP-3.
           05  LP-SETUP-CHG                   PIC S9(07)V9(05) COMP-3.
           05  LP-MIN-CHG                    PIC S9(07)V9(02) COMP-3.
           05  LP-MAX-CHG                    PIC S9(11)V9(02) COMP-3.
           05  LP-ROUND-RULE                 PIC X(01).
           05  LP-AMT-OUT                    PIC S9(13)V9(05) COMP-3.
           05  LP-RETURN-CODE                PIC 9(04).
           05  LP-RETURN-MSG                 PIC X(40).
           05  LP-RETRY-FLAG                 PIC X(01).
           05  LP-FILLER                     PIC X(84).
      * ELEMENT-TYPE VIEW OF THE SAME 240 BYTES - VOICE-STYLE RATING  *
      * MODULES EXPECT CONVERSATION AND CHARGEABLE MINUTES AND A      *
      * TANDEM HOP COUNT LAID OVER THE BACK OF THE BLOCK; THE FIELDS  *
       01  WS-LINK-PARM-TYPE-VIEW REDEFINES WS-LINK-PARM-BLOCK.
           05  LPT-HEADER                    PIC X(40).
           05  LPT-KEY-AREA                  PIC X(19).
           05  LPT-VOICE-METRICS.
               10  LPT-CONV-MIN               PIC S9(13)V9(02) COMP-3.
               10  LPT-CHG-MIN                PIC S9(13)V9(02) COMP-3.
               10  LPT-TANDEM-HOPS            PIC 9(05) COMP-3.
           05  LPT-FILLER                    PIC X(163).
      * PACKED AMOUNT REDEFINED FOR INSPECTION - WHEN A DYNAMIC CALL  *
      * RETURNS AN AMOUNT THAT FAILS THE SANITY CHECK IN P4400 (SEE   *
      * ALSO THE 1994 SOC7 INCIDENT LOG), THIS LETS P9910-MODULE-     *
       01  WS-PACKED-AMT-WORK                PIC S9(11)V9(05) COMP-3
                                                            VALUE 0.
       01  WS-PACKED-AMT-INSPECT REDEFINES WS-PACKED-AMT-WORK
                                             PIC X(09).
      * ELEMENT APPLICABILITY MATRIX - RECORD TYPE / USAGE TYPE /     *
      * JURISDICTION TO UP TO SIX RATE ELEMENTS.  CAPACITY IS 60 ROWS *
      * BUT ONLY THE ROWS LOADED BY P1550 BELOW ARE ACTIVE - THE      *
       01  WS-APPLICABILITY-MATRIX.
           05  WS-AM-LOADED-CNT              PIC 9(02) VALUE 0.
           05  WS-AM-ENTRY OCCURS 60 TIMES INDEXED BY WS-AM-X.
               10  WS-AM-KEY.
                   15  WS-AM-REC-TYPE          PIC X(02)
                                                VALUE LOW-VALUES.
                   15  WS-AM-USAGE-TYPE        PIC X(01)
                                                VALUE LOW-VALUES.
                   15  WS-AM-JURIS-CD          PIC X(01)
                                                VALUE LOW-VALUES.
               10  WS-AM-ELEM-CNT             PIC 9(01) VALUE 0.
               10  WS-AM-ELEM OCCURS 6 TIMES
                        INDEXED BY WS-AM-EX.
                   15  WS-AM-ELEM-CODE          PIC X(06)
                                                VALUE SPACES.
      * CARRIER CACHE - THE ENTIRE ACTIVE CARRMST VSAM FILE, LOADED   *
      * ONCE AT INIT SO S200 NEVER TAKES A RANDOM READ PER CDR.       *
       01  WS-CARRIER-CACHE.
           05  WS-CC-LOADED-CNT              PIC 9(04) VALUE 0.
           05  WS-CC-ENTRY OCCURS 1 TO 500 TIMES
                    DEPENDING ON WS-CC-LOADED-CNT
                    INDEXED BY WS-CC-X.
               10  WS-CC-OCN                  PIC X(04).
               10  WS-CC-TYPE                 PIC X(01).
               10  WS-CC-ACTIVE-SW            PIC X(01).
               10  WS-CC-DEFAULT-PIU          PIC S9(03)V9(05) COMP-3.
               10  WS-CC-DEFAULT-PLU          PIC S9(03)V9(05) COMP-3.
               10  WS-CC-ISP-CAP-MOU          PIC S9(13) COMP-3.
      * CARRIER CACHE LOOKUP WORK - WS-CC-X ITSELF ENDS UP POINTING   *
      * AT THE MATCHED ROW WHEN WS-CC-FOUND-SW IS 'Y' (TEST-BEFORE    *
      * PERFORM VARYING SEMANTICS), SO NO SEPARATE INDEX IS SAVED.    *
       01  WS-CARRIER-LOOKUP-WORK.
           05  WS-CC-FOUND-SW                PIC X(01) VALUE 'N'.
               88  WS-CC-ROW-FOUND             VALUE 'Y'.
      * PRIOR-RECORD KEY - USED BY P2205 TO DETECT AN ADJACENT        *
      * DUPLICATE SEQUENCE NUMBER THAT SLIPPED THROUGH INGEST.        *
       01  WS-PRIOR-KEY-WORK.
           05  WS-PRIOR-KEY-SW               PIC X(01) VALUE 'N'.
           05  WS-PRIOR-OCN                  PIC X(04) VALUE SPACES.
           05  WS-PRIOR-BAN                  PIC X(13) VALUE SPACES.
           05  WS-PRIOR-SEQ                  PIC 9(09) COMP-3 VALUE 0.
      * DATE VALIDATION WORK - RETURN SWITCH FROM CABDTCNV.           *
       01  WS-DATE-VALIDATE-WORK.
           05  WS-DT-VALID-SW                PIC X(01) VALUE 'Y'.
               88  WS-DT-VALID                  VALUE 'Y'.
      * ELEMENT LIST - THE OUTPUT OF S300, THE INPUT TO S400 AND S500.*
      * ONE ROW PER RATE ELEMENT RESOLVED FOR THE CURRENT CDR.        *
      * CAPACITY 60 IS SET ABOVE THE 40-ELEMENT LIMIT PER CABS-STD-014*
       01  WS-ELEMENT-LIST.
           05  WS-EL-CNT                     PIC 9(02) VALUE 0.
           05  WS-EL-CCL-SUB                 PIC S9(02) COMP-3 VALUE 0.
           05  WS-EL-SWAP-AREA               PIC X(43).
           05  WS-EL-ENTRY OCCURS 1 TO 60 TIMES
                    DEPENDING ON WS-EL-CNT
                    INDEXED BY WS-EL-X WS-EL-PRIOR-X WS-EL-Y.
               10  WS-EL-ELEM-CODE             PIC X(06).
               10  WS-EL-MODULE-SFX            PIC X(02).
               10  WS-EL-R2-INDEX              PIC S9(04) COMP-3.
               10  WS-EL-QTY                   PIC S9(13)V9(02) COMP-3.
               10  WS-EL-AMOUNT                PIC S9(11)V9(05) COMP-3.
               10  WS-EL-ROUND-RULE            PIC X(01).
               10  WS-EL-SRC-PROCESS           PIC X(08).
               10  WS-EL-SUPPRESS-SW           PIC X(01) VALUE 'N'.
                   88  WS-EL-SUPPRESSED         VALUE 'Y'.
               10  WS-EL-DISPATCH-METHOD       PIC X(01).
                   88  WS-EL-VIA-MAIN           VALUE '1'.
                   88  WS-EL-VIA-JURIS          VALUE '2'.
                   88  WS-EL-VIA-OVERRIDE       VALUE '3'.
                   88  WS-EL-VIA-RETRY          VALUE '4'.
               10  WS-EL-RC                    PIC 9(04).
      * EFFECTIVE-DATED RATE VIEW - INDEXES INTO R2-RATE-TABLE FOR    *
      * ROWS CURRENTLY IN EFFECT FOR THIS RUN'S CYCLE DATE.  BUILT    *
      * ONCE AT INIT BY P1600 SO EVERY SEARCH THEREAFTER SKIPS        *
       01  WS-ACTIVE-RATE-INDEX.
           05  WS-ARI-CNT                    PIC 9(04) VALUE 0.
           05  WS-ARI-ENTRY OCCURS 1 TO 600 TIMES
                    DEPENDING ON WS-ARI-CNT
                    INDEXED BY WS-ARI-X.
               10  WS-ARI-R2-INDEX             PIC S9(04) COMP-3.
      * MATRIX SEARCH WORK - STATE FOR P3100'S THREE-LEVEL FALLBACK   *
      * SEARCH OF THE ELEMENT APPLICABILITY MATRIX.                   *
       01  WS-MATRIX-SEARCH-WORK.
           05  WS-MSW-FOUND-SW               PIC X(01) VALUE 'N'.
               88  WS-MSW-FOUND                VALUE 'Y'.
           05  WS-MSW-FALLBACK-LVL           PIC X(01) VALUE ' '.
               88  WS-MSW-EXACT                 VALUE '1'.
               88  WS-MSW-JURIS-WILD            VALUE '2'.
               88  WS-MSW-RECTYPE-ONLY          VALUE '3'.
      * RATE TABLE SEARCH WORK - SAME SEQUENTIAL-SCAN-OF-ACTIVE-      *
      * ENTRIES APPROACH CABRAT03 USES, SCOPED TO WS-ACTIVE-RATE-     *
      * INDEX INSTEAD OF THE FULL R2 TABLE.  RESOLVES ONE ELEMENT'S   *
       01  WS-RATE-SEARCH-WORK.
           05  WS-RSW-FOUND-SW               PIC X(01) VALUE 'N'.
               88  WS-RSW-FOUND                VALUE 'Y'.
           05  WS-RSW-SCAN-SUB               PIC S9(04) COMP-3.
           05  WS-RSW-R2-SUB                 PIC S9(04) COMP-3.
           05  WS-RSW-FALLBACK-LVL           PIC X(01) VALUE ' '.
               88  WS-RSW-EXACT-STATE         VALUE '1'.
               88  WS-RSW-JURIS-GENERIC       VALUE '2'.
               88  WS-RSW-TARIFF-DEFAULT      VALUE '3'.
      * WORKING COPY OF CDR CONTEXT CARRIED THROUGH RATING FOR THE    *
      * RECORD CURRENTLY BEING PROCESSED.                             *
       01  WS-CDR-WORK-AREA.
           05  WS-CW-OCN                     PIC X(04).
           05  WS-CW-BAN                     PIC X(13).
           05  WS-CW-SEQ-NBR                 PIC 9(09) COMP-3.
           05  WS-CW-KEY-TEXT.
               10  WS-CW-KT-OCN                PIC X(04).
               10  WS-CW-KT-BAN                PIC X(13).
               10  WS-CW-KT-SEQ                PIC 9(09).
           05  WS-CW-REC-TYPE                PIC X(02).
           05  WS-CW-USAGE-TYPE              PIC X(01).
           05  WS-CW-JURIS-CD                PIC X(01).
           05  WS-CW-STATE-CD                PIC X(02).
           05  WS-CW-RATE-ELEM-HINT          PIC X(06).
           05  WS-CW-CONV-MIN                PIC S9(07)V9(02) COMP-3.
           05  WS-CW-VALID-SW                PIC X(01) VALUE 'Y'.
               88  WS-CW-VALID                 VALUE 'Y'.
               88  WS-CW-INVALID               VALUE 'N'.
           05  WS-CW-QTY-CLASS-IDX           PIC 9(01) VALUE 0.
      * DISPATCH CLASS - OVERLAPPING 88 LEVELS OF ITS OWN.  A CLASS   *
      * OF 'B' IS BOTH STATIC-ELIGIBLE AND DYNAMIC-ONLY.  P4000 TESTS *
      * WS-DC-DYNAMIC-ONLY FIRST, SO A 'B' ELEMENT ALWAYS GOES DOWN   *
       01  WS-DISPATCH-CLASS-WORK.
           05  WS-DISPATCH-CLASS             PIC X(01) VALUE ' '.
               88  WS-DC-STATIC-ELIGIBLE      VALUE 'S' 'B'.
               88  WS-DC-DYNAMIC-ONLY         VALUE 'B' 'D'.
               88  WS-DC-OVERRIDE-ONLY        VALUE 'O'.
      * RETURN CODE INTERPRETATION - EVERY DISPATCH PARAGRAPH IN S400 *
      * MOVES THE CALLED MODULE'S RC INTO WS-RC-CURRENT AND TESTS IT  *
      * HERE.  0000 IS CLEAN, 0004 IS A WARNING (MIN OR MAX CHARGE    *
       01  WS-RC-INTERPRET-WORK.
           05  WS-RC-CURRENT                 PIC 9(04) VALUE 0.
               88  WS-RC-OK                    VALUE 0.
               88  WS-RC-WARNING               VALUE 4.
               88  WS-RC-SOFT-REJECT           VALUE 8.
               88  WS-RC-MODULE-NOT-FOUND      VALUE 900 THRU 999.
               88  WS-RC-FATAL                  VALUE 1 THRU 3
                                                  5 THRU 7
                                                  9 THRU 899
                                                  1000 THRU 9999.
      * BILL DETAIL BUILD WORK - ACCUMULATES ELEMENT AMOUNTS INTO THE *
      * BD-ELEMENT ODO OCCURRENCE AND HOLDS THE RUNNING TOTALS BEFORE *
      * THEY ARE MOVED TO CABS-BILL-DETAIL AND ROUNDED.               *
       01  WS-BD-BUILD-WORK.
           05  WS-BD-ELIGIBLE-CNT            PIC 9(03) VALUE 0.
           05  WS-BD-BUILD-QTY               PIC S9(13)V9(02) COMP-3.
           05  WS-BD-BUILD-AMT               PIC S9(13)V9(05) COMP-3.
           05  WS-BD-RAW-TOTAL               PIC S9(13)V9(05) COMP-3.
           05  WS-BD-ROUNDED-TOTAL           PIC S9(13)V9(02) COMP-3.
           05  WS-BD-DELTA                   PIC S9(05)V9(05) COMP-3.
           05  WS-BD-OVERFLOW-SW             PIC X(01) VALUE 'N'.
               88  WS-BD-OVERFLOW              VALUE 'Y'.
           05  WS-BD-SECTION-CD              PIC X(02) VALUE 'RT'.
           05  WS-BD-LINE-SEQ-CTR            PIC 9(07) COMP-3 VALUE 0.
      * ROUNDING WORK - LOCAL TO THIS PROGRAM, USED ALONGSIDE R4-     *
      * ROUND-WORK FROM CABSRT04 WHEN THE FOUR RULE-SPECIFIC          *
      * PARAGRAPHS IN P5500 NEED SCRATCH SPACE R4 DOES NOT PROVIDE.   *
       01  WS-ROUND-LOCAL-WORK.
           05  WS-RD-RULE-INDEX              PIC 9(01) VALUE 1.
           05  WS-RL-CENTS-INT               PIC S9(15) COMP-3.
           05  WS-RL-HALF-INT                PIC S9(15) COMP-3.
           05  WS-RL-CHECK-INT               PIC S9(15) COMP-3.
      * SIX-FRAGMENT DESCRIPTION WORK AREA - BUILT PER ELEMENT AND    *
      * STRUNG TOGETHER INTO BD-DESCRIPTION (60 BYTES) BY P5600.      *
       01  WS-DESCRIPTION-FRAGMENTS.
           05  WS-DESC-FRAG1                 PIC X(16).
           05  WS-DESC-FRAG2                 PIC X(01) VALUE SPACE.
           05  WS-DESC-FRAG3                 PIC X(08).
           05  WS-DESC-FRAG4                 PIC X(01) VALUE SPACE.
           05  WS-DESC-FRAG5                 PIC X(20).
           05  WS-DESC-FRAG6                 PIC X(14).
      * EDIT MASKS FOR THE PRINTED REPORT.                            *
       01  WS-EDIT-MASKS.
           05  WS-ED-QTY                     PIC ZZ,ZZZ,ZZZ,ZZ9.99.
           05  WS-ED-AMOUNT                  PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-ED-RATE                    PIC Z.ZZZZ9.
           05  WS-ED-COUNT                   PIC ZZZ,ZZ9.
      * REPORT LAYOUT WORK - PAGE AND LINE CONTROL FOR RPTOUT.        *
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR               PIC 9(04) VALUE 0.
           05  WS-RPT-LINE-NBR               PIC 9(02) VALUE 99.
           05  WS-RPT-LINES-PER-PAGE         PIC 9(02) VALUE 55.
           05  WS-RPT-LAST-ELEM              PIC X(06) VALUE SPACES.
      * PER-ELEMENT SUMMARY TABLE - LEVEL BREAK ACCUMULATORS PRINTED  *
      * BY P8300 IN THE ELEMENT ORDER FIRST SEEN.                     *
       01  WS-ELEMENT-SUMMARY-TABLE.
           05  WS-ES-LOADED-CNT              PIC 9(02) VALUE 0.
           05  WS-ES-FOUND-SW                PIC X(01) VALUE 'N'.
               88  WS-ES-FOUND                  VALUE 'Y'.
           05  WS-ES-ENTRY OCCURS 20 TIMES INDEXED BY WS-ES-X.
               10  WS-ES-ELEM-CODE             PIC X(06).
               10  WS-ES-COUNT                 PIC 9(07) COMP-3.
               10  WS-ES-QTY-TOTAL             PIC S9(13)V9(02) COMP-3.
               10  WS-ES-AMT-TOTAL             PIC S9(13)V9(05) COMP-3.
      * PER-JURISDICTION SUMMARY - THREE FIXED ROWS (I/S/L), PRINTED  *
      * BY P8250 ALONGSIDE THE PER-ELEMENT BREAKDOWN.                 *
       01  WS-JURIS-SUMMARY-TABLE.
           05  WS-JSU-ENTRY OCCURS 3 TIMES INDEXED BY WS-JSU-X.
               10  WS-JSU-JURIS-CD             PIC X(01) VALUE SPACE.
               10  WS-JSU-COUNT                PIC 9(07) COMP-3
                                                VALUE 0.
               10  WS-JSU-AMT-TOTAL            PIC S9(13)V9(05) COMP-3
                                                VALUE 0.
      * MISCELLANEOUS EXCEPTION AND ACTIVITY COUNTERS - INFORMATIONAL *
      * ONLY, NONE FEEDS THE BALANCING EQUATION.                      *
       01  WS-MISC-COUNTERS.
           05  WS-MC-DYNAMIC-CALLS           PIC 9(07) COMP-3 VALUE 0.
           05  WS-MC-MAIN-DISPATCH           PIC 9(07) COMP-3 VALUE 0.
           05  WS-MC-JURIS-DISPATCH          PIC 9(07) COMP-3 VALUE 0.
           05  WS-MC-OVERRIDE-DISPATCH       PIC 9(07) COMP-3 VALUE 0.
           05  WS-MC-RETRY-DISPATCH          PIC 9(07) COMP-3 VALUE 0.
           05  WS-MC-RETRY-SUCCESS           PIC 9(07) COMP-3 VALUE 0.
           05  WS-MC-MODULE-NOT-FOUND        PIC 9(07) COMP-3 VALUE 0.
           05  WS-MC-ELEMENTS-SUPPRESSED     PIC 9(07) COMP-3 VALUE 0.
           05  WS-MC-ELEMENTS-DEDUPED        PIC 9(07) COMP-3 VALUE 0.
           05  WS-MC-BD-OVERFLOW-CNT         PIC 9(07) COMP-3 VALUE 0.
           05  WS-MC-MATRIX-EXACT-CNT        PIC 9(07) COMP-3 VALUE 0.
           05  WS-MC-MATRIX-FALLBACK-CNT     PIC 9(07) COMP-3 VALUE 0.
           05  WS-MC-MATRIX-MISS-CNT         PIC 9(07) COMP-3 VALUE 0.
      * HASH TOTAL WORK - LOCAL ACCUMULATORS COPIED TO CT-HASH-* IN   *
      * P8300 BEFORE THE CONTROL RECORD IS WRITTEN.                   *
       01  WS-HASH-WORK.
           05  WS-HW-MINUTES                 PIC S9(15)V9(02) COMP-3
                                                            VALUE 0.
           05  WS-HW-AMOUNT                  PIC S9(13)V9(05) COMP-3
                                                            VALUE 0.
           05  WS-HW-SEQ                     PIC S9(17)       COMP-3
                                                            VALUE 0.
           05  WS-HW-OCN                     PIC S9(15)       COMP-3
                                                            VALUE 0.
      * RESTART / SKIP WORK - WS-RESTART-KEY-SAVE ECHOES THE PARM     *
      * CARD KEY TO CT-RESTART-KEY.  WS-SKIP-SW STAYS 'Y' UNTIL THE   *
      * CURRENT KEY MATCHES OR PASSES THE RESTART KEY, AT WHICH POINT *
       01  WS-RESTART-WORK.
           05  WS-RESTART-KEY-SAVE           PIC X(26) VALUE SPACES.
           05  WS-SKIP-SW                    PIC X(01) VALUE 'N'.
               88  WS-SKIPPING                 VALUE 'Y'.
           05  WS-CURRENT-KEY-CMP            PIC X(26).
      * SUSPENSE BUILD WORK - POPULATES CABS-SUSPENSE-RECORD BEFORE   *
      * THE WRITE TO SUSOUT.                                          *
       01  WS-SUSPENSE-BUILD-WORK.
           05  WS-SB-ERR-CODE                PIC X(04).
           05  WS-SB-SEVERITY                PIC X(01).
           05  WS-SB-PARA                    PIC X(30).
      * ABEND WORK - PASSED TO CABABEND ON AN UNRECOVERABLE FAILURE.  *
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                     PIC X(08).
           05  WS-AB-PARA                    PIC X(30).
           05  WS-AB-REASON                  PIC X(60).
           05  WS-AB-USER-CODE               PIC 9(04).
      * FILE OPEN / SORT HOUSEKEEPING SWITCHES.                       *
       01  WS-HOUSEKEEPING-SWITCHES.
           05  WS-IS-OPEN-OK-SW              PIC X(01) VALUE 'N'.
           05  WS-VB-EOF-SW                  PIC X(01) VALUE 'N'.
               88  WS-VB-EOF                   VALUE 'Y'.
           05  WS-CB-EOF-SW                  PIC X(01) VALUE 'N'.
               88  WS-CB-EOF                   VALUE 'Y'.
           05  WS-WRITE-BDTLOUT-SW           PIC X(01) VALUE 'Y'.
      * RETURN CODES FROM THE STATIC SUBPROGRAMS.                     *
       01  WS-STATIC-CALL-RC.
           05  WS-RC-PARMR                   PIC 9(04) VALUE 0.
           05  WS-RC-DTCNV                   PIC 9(04) VALUE 0.
           05  WS-RC-ERRWR                   PIC 9(04) VALUE 0.
           05  WS-RC-HASH                    PIC 9(04) VALUE 0.
           05  WS-RC-OCNVL                   PIC 9(04) VALUE 0.
       PROCEDURE DIVISION.
      * P0000-MAINLINE - MANDATORY CABS BATCH SHAPE.                  *
       P0000-MAINLINE.
           PERFORM P1000-INIT THRU P1000-EXIT.
           PERFORM P2000-PROCESS THRU P2000-EXIT UNTIL WS-EOF.
           PERFORM P8000-CONTROL THRU P8000-EXIT.
           PERFORM P9000-TERM THRU P9000-EXIT.
           STOP RUN.
      * S100-INITIALISATION SECTION - OPENS SEVEN PERMANENT FILES     *
      * (RATIN, RATEMST, CARRMST, RATOUT, BDTLOUT, SUSOUT, CTLOUT ARE *
      * OPENED HERE; RPTOUT IS THE EIGHTH), READS AND VALIDATES THE   *
       S100-INITIALISATION SECTION.
       P1000-INIT.
           PERFORM P1100-OPEN-FILES THRU P1100-EXIT.
           PERFORM P1200-READ-PARM THRU P1200-EXIT.
           PERFORM P1250-VALIDATE-PARM THRU P1250-EXIT.
           PERFORM P1300-LOAD-RATE-TABLE THRU P1300-EXIT.
           PERFORM P1400-LOAD-CARRIER-CACHE THRU P1400-EXIT.
           PERFORM P1500-BUILD-APPLICABILITY-MATRIX THRU P1500-EXIT.
           PERFORM P1600-ESTABLISH-EFF-VIEW THRU P1600-EXIT.
           PERFORM P1650-LOAD-JURIS-TABLE THRU P1650-EXIT.
           PERFORM P1700-INIT-COUNTERS THRU P1700-EXIT.
           PERFORM P1800-CHECK-OVERRIDE-CARD THRU P1800-EXIT.
       P1000-EXIT.
           EXIT.
      * P1100-OPEN-FILES - A BAD OPEN ON ANY FILE THIS PROGRAM NEEDS  *
      * TO READ OR WRITE IS FATAL - THE RUN CANNOT PRODUCE A TRUSTED  *
      * BILL WITH ONE OF THE SEVEN PERMANENT FILES UNAVAILABLE.       *
       P1100-OPEN-FILES.
           OPEN OUTPUT RPTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RPTOUT OPEN FAILED' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN INPUT RATIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RATIN OPEN FAILED' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN INPUT RATEMST.
           IF WS-FS-TABLE NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RATEMST OPEN FAILED' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN INPUT CARRMST.
           IF WS-FS-TABLE NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CARRMST OPEN FAILED' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RATOUT.
           OPEN OUTPUT BDTLOUT.
           OPEN OUTPUT SUSOUT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-OUTPUT NOT = '00' OR WS-FS-SUSPENSE NOT = '00'
                   OR WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'ONE OR MORE OUTPUT FILES FAILED TO OPEN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           MOVE 'Y' TO WS-IS-OPEN-OK-SW.
       P1100-EXIT.
           EXIT.
      * P1200-READ-PARM - ACCEPTS THE SYSIN CARD, THEN TESTS BYTE 1   *
      * (VIA THE REDEFINE, NOT REFERENCE MODIFICATION) TO DECIDE      *
      * WHICH OF THE TWO LAYOUTS APPLIES.  'P' IS THE LEGACY FIXED    *
       P1200-READ-PARM.
           MOVE SPACES TO WS-PARM-CARD.
           ACCEPT WS-PARM-CARD FROM SYSIN.
           IF PC-POS-FORMAT-FLAG = 'P'
               PERFORM P1220-PARSE-POSITIONAL-CARD THRU P1220-EXIT
           ELSE
               PERFORM P1210-PARSE-CSV-CARD THRU P1210-EXIT.
       P1200-EXIT.
           EXIT.
      * P1210-PARSE-CSV-CARD - THE SIX-FIELD UNSTRING.  DELIMITED BY  *
      * ',' SPLITS RUN-ID, CYCLE, BILL PERIOD, TARIFF, MODE AND       *
      * RESTART KEY.  COUNT IN CATCHES A FIELD THAT UNSTRUNG TO ZERO  *
       P1210-PARSE-CSV-CARD.
           MOVE SPACES TO WS-UNS-RUN-ID WS-UNS-CYCLE
               WS-UNS-BILL-PERIOD WS-UNS-TARIFF WS-UNS-MODE
               WS-UNS-RESTART-KEY.
           MOVE 0 TO WS-UNS-FIELD-CNT.
           UNSTRING PC-CSV-REST DELIMITED BY ','
               INTO WS-UNS-RUN-ID    COUNT IN WS-UNS-RUN-ID-LEN
                    WS-UNS-CYCLE     COUNT IN WS-UNS-CYCLE-LEN
                    WS-UNS-BILL-PERIOD COUNT IN WS-UNS-BP-LEN
                    WS-UNS-TARIFF    COUNT IN WS-UNS-TARIFF-LEN
                    WS-UNS-MODE      COUNT IN WS-UNS-MODE-LEN
                    WS-UNS-RESTART-KEY COUNT IN WS-UNS-RESTART-LEN
               TALLYING IN WS-UNS-FIELD-CNT.
           MOVE WS-UNS-RUN-ID TO WS-PV-RUN-ID.
           MOVE WS-UNS-BILL-PERIOD TO WS-PV-BILL-PERIOD.
           MOVE WS-UNS-TARIFF TO WS-PV-TARIFF-CD.
           MOVE WS-UNS-MODE TO WS-PV-MODE-SW.
           MOVE WS-UNS-RESTART-KEY TO WS-PV-RESTART-KEY.
           IF WS-UNS-CYCLE-LEN > 0
               MOVE WS-UNS-CYCLE TO WS-PV-CYCLE-YYDDD.
       P1210-EXIT.
           EXIT.
      * P1220-PARSE-POSITIONAL-CARD - THE LEGACY LAYOUT, KEPT FOR     *
      * SITES THAT STILL SUBMIT THE PRE-1999 FIXED-COLUMN CARD.       *
       P1220-PARSE-POSITIONAL-CARD.
           MOVE PC-POS-RUN-ID TO WS-PV-RUN-ID.
           MOVE PC-POS-CYCLE-YYDDD TO WS-PV-CYCLE-YYDDD.
           MOVE PC-POS-BILL-PERIOD TO WS-PV-BILL-PERIOD.
           MOVE PC-POS-TARIFF-CD TO WS-PV-TARIFF-CD.
           MOVE PC-POS-MODE-SW TO WS-PV-MODE-SW.
           MOVE PC-POS-RESTART-KEY TO WS-PV-RESTART-KEY.
       P1220-EXIT.
           EXIT.
      * P1250-VALIDATE-PARM - COPIES THE VALIDATED PARM VALUES INTO   *
      * R1-RATING-CONTROL FOR THE REST OF THE PROGRAM AND THE RATING  *
      * MODULES IT CALLS.  A BLANK TARIFF DEFAULTS TO FCC1; A BLANK   *
       P1250-VALIDATE-PARM.
           IF WS-PV-TARIFF-CD = SPACES
               MOVE 'FCC1' TO WS-PV-TARIFF-CD.
           IF WS-PV-MODE-SW = SPACES
               MOVE 'P' TO WS-PV-MODE-SW.
           IF WS-PV-RUN-ID = SPACES
               MOVE 'CABRAT02-DFLT' TO WS-PV-RUN-ID.
           MOVE WS-PV-RUN-ID TO R1-RUN-ID.
           MOVE WS-PGM-NAME TO R1-PROCESS-ID.
           MOVE WS-PV-CYCLE-YYDDD TO R1-CYCLE-YYDDD.
           MOVE WS-PV-BILL-PERIOD TO R1-BILL-PERIOD.
           MOVE WS-PV-TARIFF-CD TO R1-TARIFF-CD.
           MOVE WS-PV-MODE-SW TO R1-MODE-SW.
           MOVE WS-PV-RESTART-KEY TO WS-RESTART-KEY-SAVE.
           IF WS-PV-RESTART-KEY NOT = SPACES
               MOVE 'Y' TO WS-RESTART-SW
               MOVE 'Y' TO WS-SKIP-SW.
           IF WS-PV-CYCLE-YYDDD = 0
               MOVE 'N' TO WS-PV-VALID-SW
               MOVE 'P1250-VALIDATE-PARM' TO WS-AB-PARA
               MOVE 'PARM CARD CYCLE-YYDDD IS ZERO' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
       P1250-EXIT.
           EXIT.
      * P1300-LOAD-RATE-TABLE - KEYED BROWSE OF THE ENTIRE RATEMST    *
      * VSAM KSDS INTO R2-RATE-TABLE, FLATTENING EACH ROW'S BAND SET  *
      * INTO R3-BAND-POOL AT THE SAME TIME WITH A RUNNING OFFSET.     *
       P1300-LOAD-RATE-TABLE.
           MOVE LOW-VALUES TO RT-KEY.
           MOVE 0 TO R2-ENTRY-CNT.
           MOVE 0 TO R3-POOL-CNT.
           MOVE 'N' TO WS-VB-EOF-SW.
           START RATEMST KEY NOT LESS THAN RT-KEY
               INVALID KEY MOVE 'Y' TO WS-VB-EOF-SW.
           PERFORM P1310-READ-NEXT-RATE THRU P1310-EXIT.
           PERFORM P1320-LOAD-RATE-ROW THRU P1320-EXIT
               UNTIL WS-VB-EOF.
       P1300-EXIT.
           EXIT.
       P1310-READ-NEXT-RATE.
           IF NOT WS-VB-EOF
               READ RATEMST NEXT RECORD
                   AT END MOVE 'Y' TO WS-VB-EOF-SW.
       P1310-EXIT.
           EXIT.
       P1320-LOAD-RATE-ROW.
           IF R2-ENTRY-CNT < 600
               ADD 1 TO R2-ENTRY-CNT
               SET R2-EX TO R2-ENTRY-CNT
               MOVE RT-KEY TO R2-EN-KEY (R2-EX)
               MOVE RT-INITIAL-RATE TO R2-EN-INITIAL (R2-EX)
               MOVE RT-ADDL-RATE TO R2-EN-ADDL (R2-EX)
               MOVE RT-SETUP-CHG TO R2-EN-SETUP (R2-EX)
               MOVE RT-MIN-CHG TO R2-EN-MIN-CHG (R2-EX)
               MOVE RT-MAX-CHG TO R2-EN-MAX-CHG (R2-EX)
               MOVE RT-ROUND-RULE TO R2-EN-ROUND-RULE (R2-EX)
               MOVE RT-ROUND-POS TO R2-EN-ROUND-POS (R2-EX)
               MOVE RT-INIT-PERIOD TO R2-EN-INIT-PERIOD (R2-EX)
               MOVE RT-ADDL-PERIOD TO R2-EN-ADDL-PERIOD (R2-EX)
               MOVE RT-EXP-YYDDD TO R2-EN-EXP-YYDDD (R2-EX)
               MOVE RT-BAND-CNT TO R2-EN-BAND-CNT (R2-EX)
               PERFORM P1330-LOAD-BAND-SET THRU P1330-EXIT
               PERFORM P1340-SET-MODULE-SUFFIX THRU P1340-EXIT
           ELSE
               MOVE 'Y' TO R2-TABLE-FULL-SW.
           PERFORM P1310-READ-NEXT-RATE THRU P1310-EXIT.
       P1320-EXIT.
           EXIT.
      * P1330-LOAD-BAND-SET - COPIES THIS RATE ROW'S BANDS INTO THE   *
      * FLAT POOL AND RECORDS THE OFFSET OF THE FIRST ONE.  A ROW     *
      * WITH ZERO BANDS GETS AN OFFSET OF ZERO AND IS NEVER SCANNED.  *
       P1330-LOAD-BAND-SET.
           IF RT-BAND-CNT > 0
               COMPUTE R2-EN-BAND-OFFSET (R2-EX) = R3-POOL-CNT + 1
               PERFORM P1335-LOAD-ONE-BAND THRU P1335-EXIT
                   VARYING RT-BX FROM 1 BY 1
                   UNTIL RT-BX > RT-BAND-CNT
           ELSE
               MOVE 0 TO R2-EN-BAND-OFFSET (R2-EX).
       P1330-EXIT.
           EXIT.
       P1335-LOAD-ONE-BAND.
           IF R3-POOL-CNT < 2400
               ADD 1 TO R3-POOL-CNT
               SET R3-PX TO R3-POOL-CNT
               MOVE RT-BAND-FROM (RT-BX) TO R3-PL-FROM (R3-PX)
               MOVE RT-BAND-THRU (RT-BX) TO R3-PL-THRU (R3-PX)
               MOVE RT-BAND-RATE (RT-BX) TO R3-PL-RATE (R3-PX)
               MOVE RT-BAND-PCT (RT-BX) TO R3-PL-PCT (R3-PX).
       P1335-EXIT.
           EXIT.
      * P1340-SET-MODULE-SUFFIX - DERIVES R2-EN-MODULE-SFX FOR THE    *
      * ROW JUST LOADED.  RATEMST ITSELF CARRIES NO SUFFIX COLUMN -   *
      * IT IS DERIVED HERE FROM THE RATE ELEMENT CODE ONE TIME AT     *
       P1340-SET-MODULE-SUFFIX.
           MOVE 'UN' TO R2-EN-MODULE-SFX (R2-EX).
           IF RT-RATE-ELEM = 'ORIGAC'
               MOVE 'OA' TO R2-EN-MODULE-SFX (R2-EX).
           IF RT-RATE-ELEM = 'TERMAC'
               MOVE 'TA' TO R2-EN-MODULE-SFX (R2-EX).
           IF RT-RATE-ELEM = 'LTRANS'
               MOVE 'LT' TO R2-EN-MODULE-SFX (R2-EX).
           IF RT-RATE-ELEM = 'TANSW '
               MOVE 'TS' TO R2-EN-MODULE-SFX (R2-EX).
           IF RT-RATE-ELEM = 'CCLINE'
               MOVE 'CC' TO R2-EN-MODULE-SFX (R2-EX).
           IF RT-RATE-ELEM = 'SPACC '
               MOVE 'SP' TO R2-EN-MODULE-SFX (R2-EX).
           IF RT-RATE-ELEM = 'UNELEM'
               MOVE 'UN' TO R2-EN-MODULE-SFX (R2-EX).
           IF RT-RATE-ELEM = 'RECIPC'
               MOVE 'CC' TO R2-EN-MODULE-SFX (R2-EX).
           IF RT-RATE-ELEM = 'DATASV'
               MOVE 'SP' TO R2-EN-MODULE-SFX (R2-EX).
           IF RT-RATE-ELEM = 'DATATR'
               MOVE 'SP' TO R2-EN-MODULE-SFX (R2-EX).
           IF RT-RATE-ELEM = 'MPBCHG'
               MOVE 'SP' TO R2-EN-MODULE-SFX (R2-EX).
       P1340-EXIT.
           EXIT.
      * P1400-LOAD-CARRIER-CACHE - BROWSES THE ENTIRE CARRMST VSAM    *
      * FILE INTO WS-CARRIER-CACHE ONCE, SO THE MAIN LOOP NEVER TAKES *
      * A RANDOM READ PER CDR.                                        *
       P1400-LOAD-CARRIER-CACHE.
           MOVE LOW-VALUES TO CR-KEY.
           MOVE 0 TO WS-CC-LOADED-CNT.
           MOVE 'N' TO WS-CB-EOF-SW.
           START CARRMST KEY NOT LESS THAN CR-KEY
               INVALID KEY MOVE 'Y' TO WS-CB-EOF-SW.
           PERFORM P1410-READ-NEXT-CARRIER THRU P1410-EXIT.
           PERFORM P1420-LOAD-CARRIER-ROW THRU P1420-EXIT
               UNTIL WS-CB-EOF.
       P1400-EXIT.
           EXIT.
       P1410-READ-NEXT-CARRIER.
           IF NOT WS-CB-EOF
               READ CARRMST NEXT RECORD
                   AT END MOVE 'Y' TO WS-CB-EOF-SW.
       P1410-EXIT.
           EXIT.
       P1420-LOAD-CARRIER-ROW.
           IF WS-CC-LOADED-CNT < 500 AND CR-ACTIVE-SW = 'Y'
               ADD 1 TO WS-CC-LOADED-CNT
               SET WS-CC-X TO WS-CC-LOADED-CNT
               MOVE CR-OCN TO WS-CC-OCN (WS-CC-X)
               MOVE CR-TYPE TO WS-CC-TYPE (WS-CC-X)
               MOVE CR-ACTIVE-SW TO WS-CC-ACTIVE-SW (WS-CC-X)
               MOVE CR-DEFAULT-PIU TO WS-CC-DEFAULT-PIU (WS-CC-X)
               MOVE CR-DEFAULT-PLU TO WS-CC-DEFAULT-PLU (WS-CC-X)
               MOVE CR-ISP-CAP-MOU TO WS-CC-ISP-CAP-MOU (WS-CC-X).
           PERFORM P1410-READ-NEXT-CARRIER THRU P1410-EXIT.
       P1420-EXIT.
           EXIT.
      * P1500-BUILD-APPLICABILITY-MATRIX - LOADS THE TWENTY ACTIVE    *
      * ROWS OF THE ELEMENT APPLICABILITY MATRIX.  NOT FILE-DRIVEN -  *
      * THIS TABLE CHANGES ONLY WHEN A NEW TARIFF INTRODUCES A NEW    *
       P1500-BUILD-APPLICABILITY-MATRIX.
           SET WS-AM-X TO 1.
           MOVE '01' TO WS-AM-REC-TYPE (WS-AM-X).
           MOVE 'I' TO WS-AM-JURIS-CD (WS-AM-X).
           MOVE 5 TO WS-AM-ELEM-CNT (WS-AM-X).
           MOVE 'ORIGAC' TO WS-AM-ELEM-CODE (WS-AM-X 1).
           MOVE 'TERMAC' TO WS-AM-ELEM-CODE (WS-AM-X 2).
           MOVE 'LTRANS' TO WS-AM-ELEM-CODE (WS-AM-X 3).
           MOVE 'TANSW ' TO WS-AM-ELEM-CODE (WS-AM-X 4).
           MOVE 'CCLINE' TO WS-AM-ELEM-CODE (WS-AM-X 5).
           SET WS-AM-X TO 2.
           MOVE '01' TO WS-AM-REC-TYPE (WS-AM-X).
           MOVE 'S' TO WS-AM-JURIS-CD (WS-AM-X).
           MOVE 5 TO WS-AM-ELEM-CNT (WS-AM-X).
           MOVE 'ORIGAC' TO WS-AM-ELEM-CODE (WS-AM-X 1).
           MOVE 'TERMAC' TO WS-AM-ELEM-CODE (WS-AM-X 2).
           MOVE 'LTRANS' TO WS-AM-ELEM-CODE (WS-AM-X 3).
           MOVE 'TANSW ' TO WS-AM-ELEM-CODE (WS-AM-X 4).
           MOVE 'CCLINE' TO WS-AM-ELEM-CODE (WS-AM-X 5).
           SET WS-AM-X TO 3.
           MOVE '01' TO WS-AM-REC-TYPE (WS-AM-X).
           MOVE 'L' TO WS-AM-JURIS-CD (WS-AM-X).
           MOVE 2 TO WS-AM-ELEM-CNT (WS-AM-X).
           MOVE 'LTRANS' TO WS-AM-ELEM-CODE (WS-AM-X 1).
           MOVE 'TANSW ' TO WS-AM-ELEM-CODE (WS-AM-X 2).
           SET WS-AM-X TO 4.
           MOVE '02' TO WS-AM-REC-TYPE (WS-AM-X).
           MOVE 'I' TO WS-AM-JURIS-CD (WS-AM-X).
           MOVE 5 TO WS-AM-ELEM-CNT (WS-AM-X).
           MOVE 'ORIGAC' TO WS-AM-ELEM-CODE (WS-AM-X 1).
           MOVE 'TERMAC' TO WS-AM-ELEM-CODE (WS-AM-X 2).
           MOVE 'LTRANS' TO WS-AM-ELEM-CODE (WS-AM-X 3).
           MOVE 'TANSW ' TO WS-AM-ELEM-CODE (WS-AM-X 4).
           MOVE 'CCLINE' TO WS-AM-ELEM-CODE (WS-AM-X 5).
           SET WS-AM-X TO 5.
           MOVE '02' TO WS-AM-REC-TYPE (WS-AM-X).
           MOVE 'S' TO WS-AM-JURIS-CD (WS-AM-X).
           MOVE 5 TO WS-AM-ELEM-CNT (WS-AM-X).
           MOVE 'ORIGAC' TO WS-AM-ELEM-CODE (WS-AM-X 1).
           MOVE 'TERMAC' TO WS-AM-ELEM-CODE (WS-AM-X 2).
           MOVE 'LTRANS' TO WS-AM-ELEM-CODE (WS-AM-X 3).
           MOVE 'TANSW ' TO WS-AM-ELEM-CODE (WS-AM-X 4).
           MOVE 'CCLINE' TO WS-AM-ELEM-CODE (WS-AM-X 5).
           SET WS-AM-X TO 6.
           MOVE '03' TO WS-AM-REC-TYPE (WS-AM-X).
           MOVE '1' TO WS-AM-USAGE-TYPE (WS-AM-X).
           MOVE 'I' TO WS-AM-JURIS-CD (WS-AM-X).
           MOVE 5 TO WS-AM-ELEM-CNT (WS-AM-X).
           MOVE 'ORIGAC' TO WS-AM-ELEM-CODE (WS-AM-X 1).
           MOVE 'TERMAC' TO WS-AM-ELEM-CODE (WS-AM-X 2).
           MOVE 'LTRANS' TO WS-AM-ELEM-CODE (WS-AM-X 3).
           MOVE 'TANSW ' TO WS-AM-ELEM-CODE (WS-AM-X 4).
           MOVE 'CCLINE' TO WS-AM-ELEM-CODE (WS-AM-X 5).
           SET WS-AM-X TO 7.
           MOVE '03' TO WS-AM-REC-TYPE (WS-AM-X).
           MOVE '1' TO WS-AM-USAGE-TYPE (WS-AM-X).
           MOVE 'S' TO WS-AM-JURIS-CD (WS-AM-X).
           MOVE 5 TO WS-AM-ELEM-CNT (WS-AM-X).
           MOVE 'ORIGAC' TO WS-AM-ELEM-CODE (WS-AM-X 1).
           MOVE 'TERMAC' TO WS-AM-ELEM-CODE (WS-AM-X 2).
           MOVE 'LTRANS' TO WS-AM-ELEM-CODE (WS-AM-X 3).
           MOVE 'TANSW ' TO WS-AM-ELEM-CODE (WS-AM-X 4).
           MOVE 'CCLINE' TO WS-AM-ELEM-CODE (WS-AM-X 5).
           SET WS-AM-X TO 8.
           MOVE '03' TO WS-AM-REC-TYPE (WS-AM-X).
           MOVE '2' TO WS-AM-USAGE-TYPE (WS-AM-X).
           MOVE 'I' TO WS-AM-JURIS-CD (WS-AM-X).
           MOVE 2 TO WS-AM-ELEM-CNT (WS-AM-X).
           MOVE 'DATASV' TO WS-AM-ELEM-CODE (WS-AM-X 1).
           MOVE 'DATATR' TO WS-AM-ELEM-CODE (WS-AM-X 2).
           SET WS-AM-X TO 9.
           MOVE '03' TO WS-AM-REC-TYPE (WS-AM-X).
           MOVE '2' TO WS-AM-USAGE-TYPE (WS-AM-X).
           MOVE 'S' TO WS-AM-JURIS-CD (WS-AM-X).
           MOVE 2 TO WS-AM-ELEM-CNT (WS-AM-X).
           MOVE 'DATASV' TO WS-AM-ELEM-CODE (WS-AM-X 1).
           MOVE 'DATATR' TO WS-AM-ELEM-CODE (WS-AM-X 2).
           SET WS-AM-X TO 10.
           MOVE '04' TO WS-AM-REC-TYPE (WS-AM-X).
           MOVE 'I' TO WS-AM-JURIS-CD (WS-AM-X).
           MOVE 2 TO WS-AM-ELEM-CNT (WS-AM-X).
           MOVE 'DATASV' TO WS-AM-ELEM-CODE (WS-AM-X 1).
           MOVE 'DATATR' TO WS-AM-ELEM-CODE (WS-AM-X 2).
           SET WS-AM-X TO 11.
           MOVE '04' TO WS-AM-REC-TYPE (WS-AM-X).
           MOVE 'S' TO WS-AM-JURIS-CD (WS-AM-X).
           MOVE 2 TO WS-AM-ELEM-CNT (WS-AM-X).
           MOVE 'DATASV' TO WS-AM-ELEM-CODE (WS-AM-X 1).
           MOVE 'DATATR' TO WS-AM-ELEM-CODE (WS-AM-X 2).
           SET WS-AM-X TO 12.
           MOVE '05' TO WS-AM-REC-TYPE (WS-AM-X).
           MOVE '2' TO WS-AM-USAGE-TYPE (WS-AM-X).
           MOVE 'I' TO WS-AM-JURIS-CD (WS-AM-X).
           MOVE 2 TO WS-AM-ELEM-CNT (WS-AM-X).
           MOVE 'DATASV' TO WS-AM-ELEM-CODE (WS-AM-X 1).
           MOVE 'DATATR' TO WS-AM-ELEM-CODE (WS-AM-X 2).
           SET WS-AM-X TO 13.
           MOVE '05' TO WS-AM-REC-TYPE (WS-AM-X).
           MOVE '3' TO WS-AM-USAGE-TYPE (WS-AM-X).
           MOVE 'I' TO WS-AM-JURIS-CD (WS-AM-X).
           MOVE 2 TO WS-AM-ELEM-CNT (WS-AM-X).
           MOVE 'SPACC ' TO WS-AM-ELEM-CODE (WS-AM-X 1).
           MOVE 'MPBCHG' TO WS-AM-ELEM-CODE (WS-AM-X 2).
           SET WS-AM-X TO 14.
           MOVE '05' TO WS-AM-REC-TYPE (WS-AM-X).
           MOVE '3' TO WS-AM-USAGE-TYPE (WS-AM-X).
           MOVE 'S' TO WS-AM-JURIS-CD (WS-AM-X).
           MOVE 2 TO WS-AM-ELEM-CNT (WS-AM-X).
           MOVE 'SPACC ' TO WS-AM-ELEM-CODE (WS-AM-X 1).
           MOVE 'MPBCHG' TO WS-AM-ELEM-CODE (WS-AM-X 2).
           SET WS-AM-X TO 15.
           MOVE '06' TO WS-AM-REC-TYPE (WS-AM-X).
           MOVE 'I' TO WS-AM-JURIS-CD (WS-AM-X).
           MOVE 2 TO WS-AM-ELEM-CNT (WS-AM-X).
           MOVE 'SPACC ' TO WS-AM-ELEM-CODE (WS-AM-X 1).
           MOVE 'MPBCHG' TO WS-AM-ELEM-CODE (WS-AM-X 2).
           SET WS-AM-X TO 16.
           MOVE '06' TO WS-AM-REC-TYPE (WS-AM-X).
           MOVE 'S' TO WS-AM-JURIS-CD (WS-AM-X).
           MOVE 2 TO WS-AM-ELEM-CNT (WS-AM-X).
           MOVE 'SPACC ' TO WS-AM-ELEM-CODE (WS-AM-X 1).
           MOVE 'MPBCHG' TO WS-AM-ELEM-CODE (WS-AM-X 2).
           SET WS-AM-X TO 17.
           MOVE '07' TO WS-AM-REC-TYPE (WS-AM-X).
           MOVE 'I' TO WS-AM-JURIS-CD (WS-AM-X).
           MOVE 1 TO WS-AM-ELEM-CNT (WS-AM-X).
           MOVE 'UNELEM' TO WS-AM-ELEM-CODE (WS-AM-X 1).
           SET WS-AM-X TO 18.
           MOVE '07' TO WS-AM-REC-TYPE (WS-AM-X).
           MOVE 'S' TO WS-AM-JURIS-CD (WS-AM-X).
           MOVE 1 TO WS-AM-ELEM-CNT (WS-AM-X).
           MOVE 'UNELEM' TO WS-AM-ELEM-CODE (WS-AM-X 1).
           SET WS-AM-X TO 19.
           MOVE '08' TO WS-AM-REC-TYPE (WS-AM-X).
           MOVE 'I' TO WS-AM-JURIS-CD (WS-AM-X).
           MOVE 1 TO WS-AM-ELEM-CNT (WS-AM-X).
           MOVE 'RECIPC' TO WS-AM-ELEM-CODE (WS-AM-X 1).
           SET WS-AM-X TO 20.
           MOVE '08' TO WS-AM-REC-TYPE (WS-AM-X).
           MOVE SPACE TO WS-AM-JURIS-CD (WS-AM-X).
           MOVE 1 TO WS-AM-ELEM-CNT (WS-AM-X).
           MOVE 'RECIPC' TO WS-AM-ELEM-CODE (WS-AM-X 1).
           MOVE 20 TO WS-AM-LOADED-CNT.
       P1500-EXIT.
           EXIT.
      * P1600-ESTABLISH-EFF-VIEW - SCANS THE RATE TABLE ONCE AND      *
      * BUILDS WS-ACTIVE-RATE-INDEX, THE LIST OF R2 SUBSCRIPTS THAT   *
      * ARE CURRENTLY EFFECTIVE FOR THIS RUN'S CYCLE DATE.  EVERY     *
       P1600-ESTABLISH-EFF-VIEW.
           MOVE 0 TO WS-ARI-CNT.
           IF R2-ENTRY-CNT > 0
               PERFORM P1610-CHECK-ONE-RATE-ROW THRU P1610-EXIT
                   VARYING R2-EX FROM 1 BY 1
                   UNTIL R2-EX > R2-ENTRY-CNT.
       P1600-EXIT.
           EXIT.
      * P1610-CHECK-ONE-RATE-ROW - A ROW IS EFFECTIVE WHEN ITS KEY    *
      * EFFECTIVE DATE IS NOT LATER THAN THE CYCLE DATE AND ITS       *
      * EXPIRY DATE IS EITHER ZERO (OPEN-ENDED) OR NOT EARLIER THAN   *
       P1610-CHECK-ONE-RATE-ROW.
           IF R2-EN-EFF-YYDDD (R2-EX) NOT > WS-PV-CYCLE-YYDDD AND
                   (R2-EN-EXP-YYDDD (R2-EX) = 0 OR
                    R2-EN-EXP-YYDDD (R2-EX) NOT < WS-PV-CYCLE-YYDDD)
               IF WS-ARI-CNT < 600
                   ADD 1 TO WS-ARI-CNT
                   SET WS-ARI-X TO WS-ARI-CNT
                   MOVE R2-EX TO WS-ARI-R2-INDEX (WS-ARI-X).
       P1610-EXIT.
           EXIT.
      * P1650-LOAD-JURIS-TABLE - THE FOUR-ROW CONSTANT USED BY         *
      * P4200-DISPATCH-JURIS-MODULE.  INTRASTATE UNBUNDLED ELEMENTS    *
      * ROUTE TO THE SPECIAL ACCESS MODULE RATHER THAN A DEDICATED     *
       P1650-LOAD-JURIS-TABLE.
           SET WS-JS-X TO 1.
           MOVE 'I' TO WS-JS-JURIS-CD (WS-JS-X).
           MOVE 'UN' TO WS-JS-SUFFIX (WS-JS-X).
           SET WS-JS-X TO 2.
           MOVE 'S' TO WS-JS-JURIS-CD (WS-JS-X).
           MOVE 'SP' TO WS-JS-SUFFIX (WS-JS-X).
           SET WS-JS-X TO 3.
           MOVE 'L' TO WS-JS-JURIS-CD (WS-JS-X).
           MOVE 'LT' TO WS-JS-SUFFIX (WS-JS-X).
           SET WS-JS-X TO 4.
           MOVE 'X' TO WS-JS-JURIS-CD (WS-JS-X).
           MOVE 'UN' TO WS-JS-SUFFIX (WS-JS-X).
       P1650-EXIT.
           EXIT.
      * P1700-INIT-COUNTERS - ZEROES EVERY WORKING ACCUMULATOR BEFORE *
      * THE MAIN LOOP STARTS.                                          *
       P1700-INIT-COUNTERS.
           MOVE 0 TO WS-READ-CNT WS-WRITE-CNT WS-REJECT-CNT
               WS-SUMM-CNT WS-CFWD-CNT.
           MOVE 0 TO WS-HW-MINUTES WS-HW-AMOUNT WS-HW-SEQ WS-HW-OCN.
           MOVE 0 TO WS-MC-DYNAMIC-CALLS WS-MC-MAIN-DISPATCH
               WS-MC-JURIS-DISPATCH WS-MC-OVERRIDE-DISPATCH
               WS-MC-RETRY-DISPATCH WS-MC-RETRY-SUCCESS
               WS-MC-MODULE-NOT-FOUND WS-MC-ELEMENTS-SUPPRESSED
               WS-MC-ELEMENTS-DEDUPED WS-MC-BD-OVERFLOW-CNT
               WS-MC-MATRIX-EXACT-CNT WS-MC-MATRIX-FALLBACK-CNT
               WS-MC-MATRIX-MISS-CNT.
           MOVE 0 TO WS-ES-LOADED-CNT.
           MOVE 0 TO WS-RPT-PAGE-NBR.
           MOVE 99 TO WS-RPT-LINE-NBR.
           MOVE 'I' TO WS-JSU-JURIS-CD (1).
           MOVE 'S' TO WS-JSU-JURIS-CD (2).
           MOVE 'L' TO WS-JSU-JURIS-CD (3).
           MOVE 0 TO WS-JSU-COUNT (1) WS-JSU-COUNT (2)
               WS-JSU-COUNT (3).
           MOVE 0 TO WS-JSU-AMT-TOTAL (1) WS-JSU-AMT-TOTAL (2)
               WS-JSU-AMT-TOTAL (3).
       P1700-EXIT.
           EXIT.
      * P1800-CHECK-OVERRIDE-CARD - AN EMERGENCY RATE CORRECTION      *
      * CARD PUTS AN ASTERISK IN THE FIRST BYTE OF THE PARM CARD'S    *
      * RESTART-KEY FIELD, FOLLOWED BY AN 8-BYTE PROGRAM NAME AND A   *
       P1800-CHECK-OVERRIDE-CARD.
           MOVE 'N' TO WS-OVR-ACTIVE-SW.
           MOVE SPACES TO WS-OVR-TARGET WS-OVR-ELEM-FILTER.
           IF WS-PV-RK-OVR-FLAG = '*'
               MOVE 'Y' TO WS-OVR-ACTIVE-SW
               MOVE WS-PV-RK-OVR-TARGET TO WS-OVR-TARGET
               MOVE WS-PV-RK-OVR-FILTER TO WS-OVR-ELEM-FILTER.
       P1800-EXIT.
           EXIT.
      * S200-MAIN-PROCESSING SECTION - ONE PASS PER RATIN RECORD.     *
      * VALIDATES THE RECORD, DETERMINES JURISDICTION AND DISPATCH    *
      * CLASS, BUILDS THE ELEMENT LIST, DISPATCHES EVERY ELEMENT ON   *
       S200-MAIN-PROCESSING SECTION.
       P2000-PROCESS.
           PERFORM P2100-READ-RATIN THRU P2100-EXIT.
           IF NOT WS-EOF
               PERFORM P2110-CHECK-RESTART-SKIP THRU P2110-EXIT
               IF WS-SKIPPING
                   PERFORM P2120-COUNT-CARRIED-FWD THRU P2120-EXIT
               ELSE
                   PERFORM P2200-VALIDATE-RECORD THRU P2200-EXIT
                   IF WS-CW-VALID
                       PERFORM P2300-DETERMINE-JURISDICTION THRU
                           P2300-EXIT
                       PERFORM P2400-BUILD-ELEMENT-LIST THRU
                           P2400-EXIT
                       PERFORM P2500-PROCESS-ELEMENT-LIST THRU
                           P2500-EXIT
                       IF WS-BD-OVERFLOW
                           MOVE EC-RATE-NOT-FOUND TO WS-SB-ERR-CODE
                           MOVE 'E' TO WS-SB-SEVERITY
                           PERFORM P2800-REJECT-TO-SUSPENSE THRU
                               P2800-EXIT
                       ELSE
                           PERFORM P2700-WRITE-RATOUT THRU
                               P2700-EXIT
                   ELSE
                       PERFORM P2800-REJECT-TO-SUSPENSE THRU
                           P2800-EXIT.
       P2000-EXIT.
           EXIT.
      * P2100-READ-RATIN.                                              *
       P2100-READ-RATIN.
           READ RATIN
               AT END MOVE 'Y' TO WS-EOF-SW.
           IF NOT WS-EOF
               ADD 1 TO WS-READ-CNT.
       P2100-EXIT.
           EXIT.
      * P2110-CHECK-RESTART-SKIP - WHILE A RESTART IS ACTIVE, EVERY    *
      * RECORD IS COMPARED AGAINST THE RESTART KEY UNTIL ONE IS FOUND  *
      * THAT IS NOT LESS THAN IT; FROM THAT POINT ON WS-SKIP-SW STAYS  *
       P2110-CHECK-RESTART-SKIP.
           IF WS-RESTART-SW = 'Y' AND WS-SKIPPING
               MOVE CD-OCN TO WS-CW-KT-OCN
               MOVE CD-BAN TO WS-CW-KT-BAN
               MOVE CD-SEQ-NBR TO WS-CW-KT-SEQ
               MOVE WS-CW-KEY-TEXT TO WS-CURRENT-KEY-CMP
               IF WS-CURRENT-KEY-CMP NOT < WS-RESTART-KEY-SAVE
                   MOVE 'N' TO WS-SKIP-SW.
       P2110-EXIT.
           EXIT.
       P2120-COUNT-CARRIED-FWD.
           ADD 1 TO WS-CFWD-CNT.
       P2120-EXIT.
           EXIT.
      * P2200-VALIDATE-RECORD - RECORD TYPE, JURISDICTION AND OCN      *
      * PRESENCE.  RATIN IS SUPPOSED TO BE CLEAN, BUT THIS DRIVER      *
      * DOES NOT TRUST THAT BLINDLY - A RECORD THAT SLIPPED THROUGH    *
       P2200-VALIDATE-RECORD.
           MOVE 'Y' TO WS-CW-VALID-SW.
           MOVE SPACES TO WS-SB-ERR-CODE.
           MOVE CD-OCN TO WS-CW-OCN.
           MOVE CD-BAN TO WS-CW-BAN.
           MOVE CD-SEQ-NBR TO WS-CW-SEQ-NBR.
           MOVE CD-REC-TYPE TO WS-CW-REC-TYPE.
           MOVE CD-USAGE-TYPE TO WS-CW-USAGE-TYPE.
           MOVE CD-JURIS-CD TO WS-CW-JURIS-CD.
           MOVE SPACES TO WS-CW-STATE-CD.
           PERFORM P2205-CHECK-DUP-SEQ THRU P2205-EXIT.
           IF WS-CW-VALID AND NOT CD-VALID-TYPE
               MOVE 'N' TO WS-CW-VALID-SW
               MOVE EC-RATE-NOT-FOUND TO WS-SB-ERR-CODE
               MOVE 'E' TO WS-SB-SEVERITY.
           IF WS-CW-VALID AND CD-INDETERMINATE
               MOVE 'N' TO WS-CW-VALID-SW
               MOVE EC-JURIS-INDET TO WS-SB-ERR-CODE
               MOVE 'E' TO WS-SB-SEVERITY.
           IF WS-CW-VALID
               CALL 'CABOCNVL' USING CD-OCN WS-RC-OCNVL
               IF WS-RC-OCNVL NOT = 0
                   MOVE 'N' TO WS-CW-VALID-SW
                   MOVE EC-OCN-UNKNOWN TO WS-SB-ERR-CODE
                   MOVE 'E' TO WS-SB-SEVERITY.
           IF WS-CW-VALID
               PERFORM P2265-FIND-CARRIER-CACHE-ROW THRU P2265-EXIT.
           IF WS-CW-VALID
               PERFORM P2270-VALIDATE-PIU THRU P2270-EXIT.
           IF WS-CW-VALID
               PERFORM P2250-DERIVE-QUANTITY THRU P2250-EXIT.
           IF WS-CW-VALID
               PERFORM P2240-VALIDATE-MINUTES THRU P2240-EXIT.
           IF WS-CW-VALID
               PERFORM P2230-VALIDATE-DATES THRU P2230-EXIT.
           IF WS-CW-VALID AND CD-RECIP-COMP
               PERFORM P2260-CHECK-RECIP-CAP THRU P2260-EXIT.
           MOVE WS-CW-OCN TO WS-PRIOR-OCN.
           MOVE WS-CW-BAN TO WS-PRIOR-BAN.
           MOVE WS-CW-SEQ-NBR TO WS-PRIOR-SEQ.
           MOVE 'Y' TO WS-PRIOR-KEY-SW.
       P2200-EXIT.
           EXIT.
      * P2205-CHECK-DUP-SEQ - CATCHES AN ADJACENT DUPLICATE OCN/BAN/   *
      * SEQ THAT SLIPPED PAST INGEST.  ONLY ADJACENT DUPLICATES ARE    *
      * CAUGHT - RATIN IS NOT SORTED, SO A DUPLICATE SEPARATED BY      *
      * OTHER RECORDS IS NOT DETECTED BY THIS DRIVER.                  *
       P2205-CHECK-DUP-SEQ.
           IF WS-PRIOR-KEY-SW = 'Y' AND WS-CW-OCN = WS-PRIOR-OCN AND
                   WS-CW-BAN = WS-PRIOR-BAN AND
                   WS-CW-SEQ-NBR = WS-PRIOR-SEQ
               MOVE 'N' TO WS-CW-VALID-SW
               MOVE EC-DUP-SEQ TO WS-SB-ERR-CODE
               MOVE 'E' TO WS-SB-SEVERITY.
       P2205-EXIT.
           EXIT.
      * P2230-VALIDATE-DATES - CABDTCNV VALIDATES THE CONNECT YYDDD   *
      * IS A REAL CALENDAR DATE UNDER THE DW-PIVOT-YY CENTURY WINDOW. *
       P2230-VALIDATE-DATES.
           CALL 'CABDTCNV' USING CD-CONN-YYDDD WS-DT-VALID-SW
               WS-RC-DTCNV.
           IF WS-RC-DTCNV NOT = 0 OR NOT WS-DT-VALID
               MOVE 'N' TO WS-CW-VALID-SW
               MOVE EC-DATE-INVALID TO WS-SB-ERR-CODE
               MOVE 'E' TO WS-SB-SEVERITY.
       P2230-EXIT.
           EXIT.
      * P2240-VALIDATE-MINUTES - A NEGATIVE DERIVED QUANTITY MEANS    *
      * EITHER A BAD CONVERSION MINUTES FIELD OR AN OCTET COUNT THAT  *
      * OVERFLOWED THE RECEIVING FIELD IN P2252.                       *
       P2240-VALIDATE-MINUTES.
           IF WS-CW-CONV-MIN < 0
               MOVE 'N' TO WS-CW-VALID-SW
               MOVE EC-MIN-NEGATIVE TO WS-SB-ERR-CODE
               MOVE 'E' TO WS-SB-SEVERITY.
       P2240-EXIT.
           EXIT.
      * P2260-CHECK-RECIP-CAP - RECIPROCAL COMPENSATION MINUTES ABOVE *
      * THE CARRIER'S CACHED ISP CAP ARE REJECTED RATHER THAN SILENTLY*
      * TRUNCATED TO THE CAP - THE CAP APPLIES AT THE BILLING LEVEL,  *
      * NOT PER CDR, SO A PER-RECORD OVERAGE IS A DATA PROBLEM.        *
       P2260-CHECK-RECIP-CAP.
           IF WS-CC-ROW-FOUND AND WS-CC-ISP-CAP-MOU (WS-CC-X) > 0
               IF WS-CW-CONV-MIN > WS-CC-ISP-CAP-MOU (WS-CC-X)
                   MOVE 'N' TO WS-CW-VALID-SW
                   MOVE EC-RECIP-CAP-EXCEEDED TO WS-SB-ERR-CODE
                   MOVE 'E' TO WS-SB-SEVERITY.
       P2260-EXIT.
           EXIT.
      * P2265-FIND-CARRIER-CACHE-ROW - LINEAR SCAN OF THE CACHE       *
      * LOADED BY P1400.  WS-CC-X POINTS AT THE MATCHED ROW ON EXIT.  *
       P2265-FIND-CARRIER-CACHE-ROW.
           MOVE 'N' TO WS-CC-FOUND-SW.
           IF WS-CC-LOADED-CNT > 0
               PERFORM P2266-SCAN-ONE-CACHE-ROW THRU P2266-EXIT
                   VARYING WS-CC-X FROM 1 BY 1
                   UNTIL WS-CC-X > WS-CC-LOADED-CNT OR
                       WS-CC-ROW-FOUND.
       P2265-EXIT.
           EXIT.
       P2266-SCAN-ONE-CACHE-ROW.
           IF WS-CC-OCN (WS-CC-X) = WS-CW-OCN
               MOVE 'Y' TO WS-CC-FOUND-SW.
       P2266-EXIT.
           EXIT.
      * P2270-VALIDATE-PIU - PERCENT INTERSTATE USAGE MUST FALL       *
      * BETWEEN 0 AND 100 PERCENT - A CARRIER-SUPPLIED FACTOR ABOVE   *
      * 100 IS A LOAD ERROR ON CARRMST, NOT SOMETHING THIS DRIVER     *
      * SHOULD RATE AGAINST.                                          *
       P2270-VALIDATE-PIU.
           IF WS-CC-ROW-FOUND AND WS-CC-DEFAULT-PIU (WS-CC-X) > 100
               MOVE 'N' TO WS-CW-VALID-SW
               MOVE EC-PIU-OUT-OF-RANGE TO WS-SB-ERR-CODE
               MOVE 'E' TO WS-SB-SEVERITY.
       P2270-EXIT.
           EXIT.
      * P2250-DERIVE-QUANTITY - THE DRIVER DOES NOT ATTEMPT THE FULL   *
      * PER-VARIANT UNIT MATH THE RATING MODULES DO - IT JUST NEEDS A  *
      * PLAUSIBLE QUANTITY TO PASS ON THE DYNAMIC CALL.  VOICE-CLASS   *
       P2250-DERIVE-QUANTITY.
           MOVE 1 TO WS-CW-QTY-CLASS-IDX.
           IF CD-DATA-SVC AND (CD-REC-TYPE = '04' OR
                   (CD-REC-TYPE = '03' AND WS-CW-USAGE-TYPE = '2') OR
                   (CD-REC-TYPE = '05' AND WS-CW-USAGE-TYPE = '2'))
               MOVE 2 TO WS-CW-QTY-CLASS-IDX.
           IF CD-SPECIAL-ACC AND NOT (CD-REC-TYPE = '05' AND
                   WS-CW-USAGE-TYPE = '2')
               MOVE 3 TO WS-CW-QTY-CLASS-IDX.
           IF CD-UNBUNDLED OR CD-RECIP-COMP
               MOVE 3 TO WS-CW-QTY-CLASS-IDX.
           GO TO P2251-VOICE-QTY P2252-DATA-QTY P2253-OTHER-QTY
               DEPENDING ON WS-CW-QTY-CLASS-IDX.
       P2251-VOICE-QTY.
           MOVE CD-VC-CHG-MIN TO WS-CW-CONV-MIN.
           GO TO P2250-EXIT.
       P2252-DATA-QTY.
           COMPUTE WS-CW-CONV-MIN = CD-DT-OCTETS-IN + CD-DT-OCTETS-OUT.
           GO TO P2250-EXIT.
       P2253-OTHER-QTY.
           MOVE CD-SP-QTY TO WS-CW-CONV-MIN.
       P2250-EXIT.
           EXIT.
      * P2300-DETERMINE-JURISDICTION - DEFAULTS AN INDETERMINATE       *
      * JURISDICTION TO INTERSTATE (THE MORE CONSERVATIVE ASSUMPTION   *
      * FOR ACCESS BILLING) AND SETS WS-DISPATCH-CLASS.  A CLASS OF    *
       P2300-DETERMINE-JURISDICTION.
           IF WS-CW-JURIS-CD = SPACE OR WS-CW-JURIS-CD = 'X'
               MOVE 'I' TO WS-CW-JURIS-CD.
           MOVE ' ' TO WS-DISPATCH-CLASS.
           IF CD-VOICE-MOU
               MOVE 'D' TO WS-DISPATCH-CLASS.
           IF CD-DATA-SVC OR CD-SPECIAL-ACC
               MOVE 'B' TO WS-DISPATCH-CLASS.
           IF CD-UNBUNDLED
               MOVE 'S' TO WS-DISPATCH-CLASS.
           IF CD-RECIP-COMP
               MOVE 'O' TO WS-DISPATCH-CLASS.
           IF WS-OVR-ACTIVE
               MOVE 'O' TO WS-DISPATCH-CLASS.
       P2300-EXIT.
           EXIT.
      * P2400-BUILD-ELEMENT-LIST - HANDS OFF TO S300.                  *
       P2400-BUILD-ELEMENT-LIST.
           PERFORM P3000-RESOLVE-ELEMENT-LIST THRU P3000-EXIT.
       P2400-EXIT.
           EXIT.
      * P2500-PROCESS-ELEMENT-LIST - DISPATCHES EVERY NON-SUPPRESSED   *
      * ELEMENT ON THE LIST, THEN HANDS OFF TO S500 TO BUILD THE BILL  *
      * DETAIL RECORD FROM WHATEVER CAME BACK.                         *
       P2500-PROCESS-ELEMENT-LIST.
           IF WS-EL-CNT > 0
               PERFORM P2510-PROCESS-ONE-ELEMENT THRU P2510-EXIT
                   VARYING WS-EL-X FROM 1 BY 1
                   UNTIL WS-EL-X > WS-EL-CNT.
           PERFORM P5000-COLLECT-AND-BUILD-DETAIL THRU P5000-EXIT.
       P2500-EXIT.
           EXIT.
       P2510-PROCESS-ONE-ELEMENT.
           IF NOT WS-EL-SUPPRESSED (WS-EL-X)
               PERFORM P4000-DISPATCH-LOOP-ONE-ELEMENT THRU
                   P4000-EXIT.
       P2510-EXIT.
           EXIT.
      * P2700-WRITE-RATOUT - THE RATED CDR PASS-THROUGH RECORD.        *
       P2700-WRITE-RATOUT.
           MOVE WS-CW-OCN TO RO-OCN.
           MOVE WS-CW-BAN TO RO-BAN.
           MOVE WS-CW-SEQ-NBR TO RO-SEQ-NBR.
           MOVE 'R' TO RO-RATE-STATUS.
           MOVE WS-PV-CYCLE-YYDDD TO RO-CYCLE-YYDDD.
           MOVE WS-CW-JURIS-CD TO RO-JURIS-CD.
           MOVE WS-CW-STATE-CD TO RO-STATE-CD.
           MOVE WS-CW-REC-TYPE TO RO-REC-TYPE.
           MOVE WS-EL-CNT TO RO-ELEM-COUNT.
           MOVE WS-DISPATCH-CLASS TO RO-DISPATCH-CLASS.
           MOVE WS-CW-CONV-MIN TO RO-TOT-QTY.
           MOVE WS-BD-RAW-TOTAL TO RO-TOT-AMOUNT.
           MOVE SPACES TO RO-FILLER.
           WRITE CABS-RATOUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
           ADD WS-CW-CONV-MIN TO WS-HW-MINUTES.
           ADD WS-BD-RAW-TOTAL TO WS-HW-AMOUNT.
           ADD WS-CW-SEQ-NBR TO WS-HW-SEQ.
           CALL 'CABHASH' USING WS-CW-OCN WS-HW-OCN.
           PERFORM P2710-UPDATE-JURIS-SUMMARY THRU P2710-EXIT.
       P2700-EXIT.
           EXIT.
      * P2710-UPDATE-JURIS-SUMMARY - THREE ROWS ONLY, SO A SIMPLE     *
      * FIXED SCAN IS CHEAPER THAN A SEARCH PARAGRAPH.                *
       P2710-UPDATE-JURIS-SUMMARY.
           PERFORM P2715-CHECK-ONE-JURIS-ROW THRU P2715-EXIT
               VARYING WS-JSU-X FROM 1 BY 1
               UNTIL WS-JSU-X > 3.
       P2710-EXIT.
           EXIT.
       P2715-CHECK-ONE-JURIS-ROW.
           IF WS-JSU-JURIS-CD (WS-JSU-X) = WS-CW-JURIS-CD
               ADD 1 TO WS-JSU-COUNT (WS-JSU-X)
               ADD WS-BD-RAW-TOTAL TO WS-JSU-AMT-TOTAL (WS-JSU-X).
       P2715-EXIT.
           EXIT.
      * P2800-REJECT-TO-SUSPENSE - USED FOR BOTH A FAILED-VALIDATION   *
      * RECORD AND A ELEMENT-LIST-OVERFLOW RECORD.  THE SUSPENSE       *
      * ERROR CODE IS SET BY THE CALLER BEFORE THIS PARAGRAPH RUNS.    *
       P2800-REJECT-TO-SUSPENSE.
           MOVE WS-SB-ERR-CODE TO SU-ERR-CODE.
           MOVE WS-SB-SEVERITY TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE 'P2200-VALIDATE-RECORD' TO SU-DETECT-PARA.
           MOVE WS-PV-RUN-ID TO SU-RUN-ID.
           MOVE CABS-CDR-RECORD TO SU-ORIG-RECORD.
           MOVE SPACES TO SU-FILLER.
           CALL 'CABERRWR' USING CABS-SUSPENSE-RECORD WS-RC-ERRWR.
           MOVE CABS-SUSPENSE-RECORD TO CABS-SUSPENSE-RECORD-FD.
           WRITE CABS-SUSPENSE-RECORD-FD.
           ADD 1 TO WS-REJECT-CNT.
       P2800-EXIT.
           EXIT.
      * S300-ELEMENT-LIST-RESOLUTION SECTION - TURNS THE APPLICABILITY *
      * MATRIX MATCH FOR THIS CDR INTO A CONCRETE, DEDUPLICATED,       *
      * ORDERED LIST OF RATE ELEMENTS, EACH WITH ITS OWN RATE-TABLE-   *
       S300-ELEMENT-LIST-RESOLUTION SECTION.
       P3000-RESOLVE-ELEMENT-LIST.
           MOVE 0 TO WS-EL-CNT.
           PERFORM P3100-SEARCH-MATRIX THRU P3100-EXIT.
           IF WS-MSW-FOUND
               PERFORM P3200-COPY-MATRIX-ELEMENTS THRU P3200-EXIT.
           PERFORM P3400-DEDUPLICATE-ELEMENTS THRU P3400-EXIT.
           PERFORM P3500-ORDER-ELEMENTS-CCL-LAST THRU P3500-EXIT.
           PERFORM P3600-SUPPRESS-UPSTREAM-RATED THRU P3600-EXIT.
       P3000-EXIT.
           EXIT.
      * P3100-SEARCH-MATRIX - THREE-LEVEL FALLBACK.  LEVEL 1 IS AN     *
      * EXACT MATCH ON RECORD TYPE, USAGE TYPE AND JURISDICTION.       *
      * LEVEL 2 DROPS THE JURISDICTION TEST.  LEVEL 3 DROPS BOTH THE   *
       P3100-SEARCH-MATRIX.
           MOVE 'N' TO WS-MSW-FOUND-SW.
           MOVE ' ' TO WS-MSW-FALLBACK-LVL.
           PERFORM P3110-EXACT-MATCH-SCAN THRU P3110-EXIT.
           IF NOT WS-MSW-FOUND
               PERFORM P3120-JURIS-WILDCARD-SCAN THRU P3120-EXIT.
           IF NOT WS-MSW-FOUND
               PERFORM P3130-RECTYPE-ONLY-SCAN THRU P3130-EXIT.
           IF NOT WS-MSW-FOUND
               ADD 1 TO WS-MC-MATRIX-MISS-CNT.
       P3100-EXIT.
           EXIT.
       P3110-EXACT-MATCH-SCAN.
           PERFORM P3115-TEST-EXACT-ROW THRU P3115-EXIT
               VARYING WS-AM-X FROM 1 BY 1
               UNTIL WS-AM-X > WS-AM-LOADED-CNT OR WS-MSW-FOUND.
           IF WS-MSW-FOUND
               MOVE '1' TO WS-MSW-FALLBACK-LVL
               ADD 1 TO WS-MC-MATRIX-EXACT-CNT.
       P3110-EXIT.
           EXIT.
       P3115-TEST-EXACT-ROW.
           IF WS-AM-REC-TYPE (WS-AM-X) = WS-CW-REC-TYPE AND
                   WS-AM-JURIS-CD (WS-AM-X) = WS-CW-JURIS-CD AND
                   (WS-AM-USAGE-TYPE (WS-AM-X) = LOW-VALUES OR
                    WS-AM-USAGE-TYPE (WS-AM-X) = WS-CW-USAGE-TYPE)
               MOVE 'Y' TO WS-MSW-FOUND-SW.
       P3115-EXIT.
           EXIT.
       P3120-JURIS-WILDCARD-SCAN.
           PERFORM P3125-TEST-NOJURIS-ROW THRU P3125-EXIT
               VARYING WS-AM-X FROM 1 BY 1
               UNTIL WS-AM-X > WS-AM-LOADED-CNT OR WS-MSW-FOUND.
           IF WS-MSW-FOUND
               MOVE '2' TO WS-MSW-FALLBACK-LVL
               ADD 1 TO WS-MC-MATRIX-FALLBACK-CNT.
       P3120-EXIT.
           EXIT.
       P3125-TEST-NOJURIS-ROW.
           IF WS-AM-REC-TYPE (WS-AM-X) = WS-CW-REC-TYPE AND
                   (WS-AM-USAGE-TYPE (WS-AM-X) = LOW-VALUES OR
                    WS-AM-USAGE-TYPE (WS-AM-X) = WS-CW-USAGE-TYPE)
               MOVE 'Y' TO WS-MSW-FOUND-SW.
       P3125-EXIT.
           EXIT.
       P3130-RECTYPE-ONLY-SCAN.
           PERFORM P3135-TEST-RECTYPE-ROW THRU P3135-EXIT
               VARYING WS-AM-X FROM 1 BY 1
               UNTIL WS-AM-X > WS-AM-LOADED-CNT OR WS-MSW-FOUND.
           IF WS-MSW-FOUND
               MOVE '3' TO WS-MSW-FALLBACK-LVL
               ADD 1 TO WS-MC-MATRIX-FALLBACK-CNT.
       P3130-EXIT.
           EXIT.
       P3135-TEST-RECTYPE-ROW.
           IF WS-AM-REC-TYPE (WS-AM-X) = WS-CW-REC-TYPE
               MOVE 'Y' TO WS-MSW-FOUND-SW.
       P3135-EXIT.
           EXIT.
      * P3200-COPY-MATRIX-ELEMENTS - THE MATCHED ROW (STILL POINTED   *
      * TO BY WS-AM-X FROM WHICHEVER SCAN FOUND IT) IS COPIED INTO     *
      * THE ELEMENT LIST, ONE ROW PER ELEMENT.                         *
       P3200-COPY-MATRIX-ELEMENTS.
           IF WS-AM-ELEM-CNT (WS-AM-X) > 0
               PERFORM P3210-ADD-ONE-ELEMENT THRU P3210-EXIT
                   VARYING WS-AM-EX FROM 1 BY 1
                   UNTIL WS-AM-EX > WS-AM-ELEM-CNT (WS-AM-X).
       P3200-EXIT.
           EXIT.
      * P3210-ADD-ONE-ELEMENT - APPENDS ONE ELEMENT TO THE LIST AND    *
      * IMMEDIATELY RESOLVES ITS RATE TABLE ROW SO THE MODULE SUFFIX   *
      * NEEDED BY S400 IS ALREADY ATTACHED.  A RATE-NOT-FOUND ON AN    *
       P3210-ADD-ONE-ELEMENT.
           IF WS-EL-CNT < 60
               ADD 1 TO WS-EL-CNT
               SET WS-EL-X TO WS-EL-CNT
               MOVE WS-AM-ELEM-CODE (WS-AM-X WS-AM-EX) TO
                   WS-EL-ELEM-CODE (WS-EL-X)
               MOVE 'N' TO WS-EL-SUPPRESS-SW (WS-EL-X)
               MOVE ' ' TO WS-EL-DISPATCH-METHOD (WS-EL-X)
               MOVE WS-CW-CONV-MIN TO WS-EL-QTY (WS-EL-X)
               MOVE 0 TO WS-EL-AMOUNT (WS-EL-X)
               MOVE WS-PGM-NAME TO WS-EL-SRC-PROCESS (WS-EL-X)
               MOVE WS-EL-ELEM-CODE (WS-EL-X) TO WS-CW-RATE-ELEM-HINT
               PERFORM P3300-SEARCH-RATE-TABLE THRU P3300-EXIT
               MOVE WS-RSW-R2-SUB TO WS-EL-R2-INDEX (WS-EL-X)
               IF WS-RSW-FOUND
                   MOVE R2-EN-MODULE-SFX (WS-RSW-R2-SUB) TO
                       WS-EL-MODULE-SFX (WS-EL-X)
                   MOVE R2-EN-ROUND-RULE (WS-RSW-R2-SUB) TO
                       WS-EL-ROUND-RULE (WS-EL-X)
               ELSE
                   MOVE 'UN' TO WS-EL-MODULE-SFX (WS-EL-X)
                   MOVE 'U' TO WS-EL-ROUND-RULE (WS-EL-X).
       P3210-EXIT.
           EXIT.
      * P3300-SEARCH-RATE-TABLE - SEQUENTIAL SCAN OF THE EFFECTIVE-    *
      * DATED VIEW (WS-ACTIVE-RATE-INDEX) FOR TARIFF + ELEMENT +       *
      * JURISDICTION + STATE.  EXACT STATE FIRST, THEN JURISDICTION-   *
       P3300-SEARCH-RATE-TABLE.
           MOVE 'N' TO WS-RSW-FOUND-SW.
           MOVE ' ' TO WS-RSW-FALLBACK-LVL.
           IF WS-ARI-CNT > 0
               PERFORM P3310-SCAN-ACTIVE-ENTRY THRU P3310-EXIT
                   VARYING WS-ARI-X FROM 1 BY 1
                   UNTIL WS-ARI-X > WS-ARI-CNT OR WS-RSW-FOUND.
       P3300-EXIT.
           EXIT.
      * P3310-SCAN-ACTIVE-ENTRY - AN ENTRY MATCHES WHEN TARIFF AND     *
      * ELEMENT ARE EQUAL, JURISDICTION IS EQUAL OR THE ROW'S          *
      * JURISDICTION IS GENERIC, AND STATE IS EQUAL OR THE ROW'S       *
       P3310-SCAN-ACTIVE-ENTRY.
           MOVE WS-ARI-R2-INDEX (WS-ARI-X) TO WS-RSW-R2-SUB.
           IF R2-EN-TARIFF (WS-RSW-R2-SUB) = WS-PV-TARIFF-CD AND
                   R2-EN-ELEM (WS-RSW-R2-SUB) = WS-CW-RATE-ELEM-HINT
               IF R2-EN-JURIS (WS-RSW-R2-SUB) = WS-CW-JURIS-CD AND
                       R2-EN-STATE (WS-RSW-R2-SUB) = WS-CW-STATE-CD
                   MOVE 'Y' TO WS-RSW-FOUND-SW
                   MOVE '1' TO WS-RSW-FALLBACK-LVL
               ELSE
                   IF R2-EN-JURIS (WS-RSW-R2-SUB) = WS-CW-JURIS-CD AND
                           R2-EN-STATE (WS-RSW-R2-SUB) = SPACES
                       MOVE 'Y' TO WS-RSW-FOUND-SW
                       MOVE '2' TO WS-RSW-FALLBACK-LVL
                   ELSE
                       IF R2-EN-JURIS (WS-RSW-R2-SUB) = SPACES AND
                               R2-EN-STATE (WS-RSW-R2-SUB) = SPACES
                           MOVE 'Y' TO WS-RSW-FOUND-SW
                           MOVE '3' TO WS-RSW-FALLBACK-LVL.
       P3310-EXIT.
           EXIT.
      * P3400-DEDUPLICATE-ELEMENTS - A CDR WHOSE JURISDICTION SITS ON  *
      * A FALLBACK BOUNDARY CAN PICK UP THE SAME ELEMENT CODE TWICE IF *
      * BOTH AN EXACT AND A FALLBACK MATRIX ROW FIRE ACROSS DIFFERENT  *
       P3400-DEDUPLICATE-ELEMENTS.
           IF WS-EL-CNT > 1
               PERFORM P3410-CHECK-ONE-ELEMENT THRU P3410-EXIT
                   VARYING WS-EL-X FROM 2 BY 1
                   UNTIL WS-EL-X > WS-EL-CNT.
       P3400-EXIT.
           EXIT.
       P3410-CHECK-ONE-ELEMENT.
           PERFORM P3415-COMPARE-EARLIER THRU P3415-EXIT
               VARYING WS-EL-PRIOR-X FROM 1 BY 1
               UNTIL WS-EL-PRIOR-X > WS-EL-X - 1
                   OR WS-EL-SUPPRESSED (WS-EL-X).
       P3410-EXIT.
           EXIT.
       P3415-COMPARE-EARLIER.
           IF NOT WS-EL-SUPPRESSED (WS-EL-PRIOR-X) AND
                   WS-EL-ELEM-CODE (WS-EL-PRIOR-X) =
                   WS-EL-ELEM-CODE (WS-EL-X)
               MOVE 'Y' TO WS-EL-SUPPRESS-SW (WS-EL-X)
               ADD 1 TO WS-MC-ELEMENTS-DEDUPED.
       P3415-EXIT.
           EXIT.
      * P3500-ORDER-ELEMENTS-CCL-LAST - CARRIER COMMON LINE MUST BE    *
      * THE LAST ELEMENT DISPATCHED SO ITS AMOUNT CAN BE COMPUTED AS   *
      * A RESIDUAL AGAINST THE OTHER FOUR SWITCHED ELEMENTS ON SOME    *
       P3500-ORDER-ELEMENTS-CCL-LAST.
           MOVE 0 TO WS-EL-CCL-SUB.
           IF WS-EL-CNT > 1
               PERFORM P3510-FIND-CCL THRU P3510-EXIT
                   VARYING WS-EL-X FROM 1 BY 1
                   UNTIL WS-EL-X > WS-EL-CNT.
           IF WS-EL-CCL-SUB > 0 AND WS-EL-CCL-SUB NOT = WS-EL-CNT
               PERFORM P3520-SWAP-TO-LAST THRU P3520-EXIT.
       P3500-EXIT.
           EXIT.
       P3510-FIND-CCL.
           IF WS-EL-ELEM-CODE (WS-EL-X) = WS-ELEM-CCLINE
               MOVE WS-EL-X TO WS-EL-CCL-SUB.
       P3510-EXIT.
           EXIT.
       P3520-SWAP-TO-LAST.
           SET WS-EL-X TO WS-EL-CCL-SUB.
           SET WS-EL-Y TO WS-EL-CNT.
           MOVE WS-EL-ENTRY (WS-EL-X) TO WS-EL-SWAP-AREA.
           MOVE WS-EL-ENTRY (WS-EL-Y) TO WS-EL-ENTRY (WS-EL-X).
           MOVE WS-EL-SWAP-AREA TO WS-EL-ENTRY (WS-EL-Y).
       P3520-EXIT.
           EXIT.
      * P3600-SUPPRESS-UPSTREAM-RATED - IF THE INBOUND CDR ALREADY     *
      * NAMES A SPECIFIC RATE ELEMENT IN CD-RATE-ELEM (SET BY A PRIOR  *
      * PARTIAL RATING PASS ON A RETRY CYCLE), EVERY OTHER ELEMENT     *
       P3600-SUPPRESS-UPSTREAM-RATED.
           IF CD-RATE-ELEM NOT = SPACES AND WS-EL-CNT > 0
               PERFORM P3610-SUPPRESS-NON-MATCH THRU P3610-EXIT
                   VARYING WS-EL-X FROM 1 BY 1
                   UNTIL WS-EL-X > WS-EL-CNT.
       P3600-EXIT.
           EXIT.
       P3610-SUPPRESS-NON-MATCH.
           IF WS-EL-ELEM-CODE (WS-EL-X) NOT = CD-RATE-ELEM
               MOVE 'Y' TO WS-EL-SUPPRESS-SW (WS-EL-X)
               ADD 1 TO WS-MC-ELEMENTS-SUPPRESSED.
       P3610-EXIT.
           EXIT.
      * S400-DISPATCH SECTION - CHOOSES BETWEEN FOUR DISPATCH PATHS    *
      * FOR THE CURRENT ELEMENT (WS-EL-X) AND INTERPRETS THE RETURN    *
      * CODE THAT COMES BACK.  P4100 IS THE PRIMARY PATH FOR MOST      *
       S400-DISPATCH SECTION.
      * P4000-DISPATCH-LOOP-ONE-ELEMENT - WS-DC-DYNAMIC-ONLY IS TESTED *
      * BEFORE WS-DC-STATIC-ELIGIBLE.  BOTH ARE TRUE FOR A DISPATCH    *
      * CLASS OF 'B' (DATA OR SPECIAL ACCESS - SEE P2300), SO A 'B'    *
       P4000-DISPATCH-LOOP-ONE-ELEMENT.
           MOVE 0 TO WS-RC-CURRENT.
           MOVE ' ' TO WS-EL-DISPATCH-METHOD (WS-EL-X).
           IF WS-OVR-ACTIVE AND
                   WS-EL-ELEM-CODE (WS-EL-X) = WS-OVR-ELEM-FILTER
               PERFORM P4600-DISPATCH-OVERRIDE THRU P4600-EXIT
           ELSE
               IF WS-DC-DYNAMIC-ONLY
                   PERFORM P4100-DISPATCH-ELEMENT THRU P4100-EXIT
               ELSE
                   IF WS-DC-STATIC-ELIGIBLE
                       PERFORM P4200-DISPATCH-JURIS-MODULE THRU
                           P4200-EXIT
                   ELSE
                       PERFORM P4100-DISPATCH-ELEMENT THRU
                           P4100-EXIT.
           PERFORM P4400-INTERPRET-RETURN-CODE THRU P4400-EXIT.
           IF WS-RC-MODULE-NOT-FOUND
               PERFORM P4500-HANDLE-MODULE-NOT-FOUND THRU P4500-EXIT.
           IF WS-RC-FATAL
               PERFORM P9910-MODULE-FAILURE THRU P9910-EXIT.
       P4000-EXIT.
           EXIT.
      * P4100-DISPATCH-ELEMENT - THE MAIN DYNAMIC CALL SITE.  THE      *
      * TARGET SUFFIX COMES FROM R2-EN-MODULE-SFX ON THE RATE TABLE    *
      * ROW THAT WON THE SEARCH IN P3300, RE-FETCHED HERE BY INDEX     *
       P4100-DISPATCH-ELEMENT.
           ADD 1 TO WS-MC-DYNAMIC-CALLS.
           ADD 1 TO WS-MC-MAIN-DISPATCH.
           MOVE '1' TO WS-EL-DISPATCH-METHOD (WS-EL-X).
           IF WS-EL-R2-INDEX (WS-EL-X) > 0
               SET R2-EX TO WS-EL-R2-INDEX (WS-EL-X)
               MOVE R2-EN-MODULE-SFX (R2-EX) TO R1-CALL-SUFFIX
           ELSE
               MOVE 'UN' TO R1-CALL-SUFFIX.
           MOVE R1-CALL-PREFIX TO WS-DT-PREFIX.
           MOVE R1-CALL-SUFFIX TO WS-DT-SUFFIX.
           PERFORM P4300-BUILD-LINKAGE-PARM THRU P4300-EXIT.
           CALL WS-DISPATCH-TARGET USING WS-LINK-PARM-BLOCK
               R1-RATING-CONTROL.
           MOVE LP-RETURN-CODE TO WS-RC-CURRENT.
           MOVE LP-AMT-OUT TO WS-EL-AMOUNT (WS-EL-X).
           PERFORM P4800-SANITY-CHECK-AMOUNT THRU P4800-EXIT.
       P4100-EXIT.
           EXIT.
      * P4200-DISPATCH-JURIS-MODULE - THE SECOND DYNAMIC CALL SITE,    *
      * USED ONLY FOR UNBUNDLED NETWORK ELEMENTS.  THE SUFFIX COMES    *
      * FROM WS-JURIS-SUFFIX-TABLE INDEXED BY CD-JURIS-CD, NOT FROM    *
       P4200-DISPATCH-JURIS-MODULE.
           ADD 1 TO WS-MC-DYNAMIC-CALLS.
           ADD 1 TO WS-MC-JURIS-DISPATCH.
           MOVE '2' TO WS-EL-DISPATCH-METHOD (WS-EL-X).
           MOVE 'N' TO WS-JS-FOUND-SW.
           PERFORM P4210-FIND-JURIS-SUFFIX THRU P4210-EXIT
               VARYING WS-JS-X FROM 1 BY 1
               UNTIL WS-JS-X > 4 OR WS-JS-MATCHED.
           MOVE R1-CALL-PREFIX TO WS-DT-PREFIX.
           IF WS-JS-MATCHED
               MOVE WS-JS-MATCH-SFX TO WS-DT-SUFFIX
           ELSE
               MOVE 'UN' TO WS-DT-SUFFIX.
           PERFORM P4300-BUILD-LINKAGE-PARM THRU P4300-EXIT.
           CALL WS-DISPATCH-TARGET USING WS-LINK-PARM-BLOCK
               R1-RATING-CONTROL.
           MOVE LP-RETURN-CODE TO WS-RC-CURRENT.
           MOVE LP-AMT-OUT TO WS-EL-AMOUNT (WS-EL-X).
           PERFORM P4800-SANITY-CHECK-AMOUNT THRU P4800-EXIT.
       P4200-EXIT.
           EXIT.
       P4210-FIND-JURIS-SUFFIX.
           IF WS-JS-JURIS-CD (WS-JS-X) = WS-CW-JURIS-CD
               MOVE 'Y' TO WS-JS-FOUND-SW
               MOVE WS-JS-SUFFIX (WS-JS-X) TO WS-JS-MATCH-SFX.
       P4210-EXIT.
           EXIT.
      * P4300-BUILD-LINKAGE-PARM - THE 240-BYTE PARAMETER BLOCK        *
      * PASSED ON EVERY DISPATCH IN THIS SECTION.                      *
       P4300-BUILD-LINKAGE-PARM.
           MOVE SPACES TO WS-LINK-PARM-BLOCK.
           MOVE WS-PV-RUN-ID TO LP-RUN-ID.
           MOVE WS-PGM-NAME TO LP-PROCESS-ID.
           MOVE WS-EL-ELEM-CODE (WS-EL-X) TO LP-ELEM-CODE.
           MOVE WS-DISPATCH-TARGET TO LP-MODULE-TARGET.
           MOVE WS-CW-REC-TYPE TO LP-REC-TYPE.
           MOVE WS-CW-USAGE-TYPE TO LP-USAGE-TYPE.
           MOVE WS-CW-JURIS-CD TO LP-JURIS-CD.
           MOVE WS-CW-STATE-CD TO LP-STATE-CD.
           MOVE WS-CW-OCN TO LP-OCN.
           MOVE WS-CW-BAN TO LP-BAN.
           MOVE WS-CW-SEQ-NBR TO LP-SEQ-NBR.
           MOVE WS-EL-QTY (WS-EL-X) TO LP-QTY-IN.
           IF WS-EL-R2-INDEX (WS-EL-X) > 0
               SET R2-EX TO WS-EL-R2-INDEX (WS-EL-X)
               MOVE R2-EN-INITIAL (R2-EX) TO LP-RATE-INITIAL
               MOVE R2-EN-ADDL (R2-EX) TO LP-RATE-ADDL
               MOVE R2-EN-SETUP (R2-EX) TO LP-SETUP-CHG
               MOVE R2-EN-MIN-CHG (R2-EX) TO LP-MIN-CHG
               MOVE R2-EN-MAX-CHG (R2-EX) TO LP-MAX-CHG
               MOVE R2-EN-ROUND-RULE (R2-EX) TO LP-ROUND-RULE
           ELSE
               MOVE 0 TO LP-RATE-INITIAL LP-RATE-ADDL LP-SETUP-CHG
                   LP-MIN-CHG LP-MAX-CHG
               MOVE 'U' TO LP-ROUND-RULE.
           MOVE 0 TO LP-AMT-OUT.
           MOVE 0 TO LP-RETURN-CODE.
           MOVE SPACES TO LP-RETURN-MSG.
           MOVE 'N' TO LP-RETRY-FLAG.
       P4300-EXIT.
           EXIT.
      * P4400-INTERPRET-RETURN-CODE - SEE WS-RC-INTERPRET-WORK FOR THE *
      * CODE RANGES.  A SOFT REJECT SUPPRESSES JUST THIS ELEMENT; A    *
      * MODULE-NOT-FOUND TRIGGERS THE RETRY PATH; ANYTHING ELSE FATAL  *
       P4400-INTERPRET-RETURN-CODE.
           MOVE WS-RC-CURRENT TO WS-EL-RC (WS-EL-X).
           IF WS-RC-SOFT-REJECT
               MOVE 'Y' TO WS-EL-SUPPRESS-SW (WS-EL-X)
               ADD 1 TO WS-MC-ELEMENTS-SUPPRESSED.
           IF WS-RC-MODULE-NOT-FOUND
               ADD 1 TO WS-MC-MODULE-NOT-FOUND.
       P4400-EXIT.
           EXIT.
      * P4500-HANDLE-MODULE-NOT-FOUND - ONE RETRY PER ELEMENT ONLY.    *
      * WS-RTY-ATTEMPTED-SW IS RESET PER CDR IN P2300 SO EVERY CDR     *
      * GETS ITS OWN RETRY BUDGET; WITHIN A CDR, A SECOND MODULE-NOT-  *
       P4500-HANDLE-MODULE-NOT-FOUND.
           IF NOT WS-RTY-ATTEMPTED
               PERFORM P4700-DISPATCH-RETRY THRU P4700-EXIT
           ELSE
               MOVE 'Y' TO WS-EL-SUPPRESS-SW (WS-EL-X)
               ADD 1 TO WS-MC-ELEMENTS-SUPPRESSED.
       P4500-EXIT.
           EXIT.
      * P4600-DISPATCH-OVERRIDE - THE THIRD DYNAMIC CALL SITE.  THE    *
      * TARGET COMES STRAIGHT OFF THE PARM CARD OVERRIDE CONTROL       *
      * FIELD (SEE P1800) WITH NO INVOLVEMENT OF THE RATE TABLE AT     *
       P4600-DISPATCH-OVERRIDE.
           ADD 1 TO WS-MC-DYNAMIC-CALLS.
           ADD 1 TO WS-MC-OVERRIDE-DISPATCH.
           MOVE '3' TO WS-EL-DISPATCH-METHOD (WS-EL-X).
           MOVE WS-OVR-TARGET TO WS-DISPATCH-TARGET.
           PERFORM P4300-BUILD-LINKAGE-PARM THRU P4300-EXIT.
           CALL WS-DISPATCH-TARGET USING WS-LINK-PARM-BLOCK
               R1-RATING-CONTROL.
           MOVE LP-RETURN-CODE TO WS-RC-CURRENT.
           MOVE LP-AMT-OUT TO WS-EL-AMOUNT (WS-EL-X).
           PERFORM P4800-SANITY-CHECK-AMOUNT THRU P4800-EXIT.
       P4600-EXIT.
           EXIT.
      * P4700-DISPATCH-RETRY - THE FOURTH DYNAMIC CALL SITE.  BUILDS   *
      * A SECONDARY TARGET FROM WS-JURIS-SUFFIX-TABLE (THE SAME        *
      * CONSTANT TABLE P4200 USES) RATHER THAN FROM THE RATE TABLE     *
       P4700-DISPATCH-RETRY.
           MOVE 'Y' TO WS-RTY-ATTEMPTED-SW.
           ADD 1 TO WS-MC-DYNAMIC-CALLS.
           ADD 1 TO WS-MC-RETRY-DISPATCH.
           MOVE '4' TO WS-EL-DISPATCH-METHOD (WS-EL-X).
           MOVE R1-CALL-PREFIX TO WS-DT-PREFIX.
           MOVE WS-RTY-SECONDARY-SFX TO WS-DT-SUFFIX.
           PERFORM P4300-BUILD-LINKAGE-PARM THRU P4300-EXIT.
           MOVE 'Y' TO LP-RETRY-FLAG.
           CALL WS-DISPATCH-TARGET USING WS-LINK-PARM-BLOCK
                   R1-RATING-CONTROL
               ON EXCEPTION
                   MOVE 9999 TO WS-RC-CURRENT
                   MOVE 0 TO WS-EL-AMOUNT (WS-EL-X)
               NOT ON EXCEPTION
                   MOVE LP-RETURN-CODE TO WS-RC-CURRENT
                   MOVE LP-AMT-OUT TO WS-EL-AMOUNT (WS-EL-X)
                   MOVE 'Y' TO WS-RTY-SUCCESS-SW
                   ADD 1 TO WS-MC-RETRY-SUCCESS.
           IF WS-RC-CURRENT = 9999
               MOVE 'Y' TO WS-EL-SUPPRESS-SW (WS-EL-X)
               ADD 1 TO WS-MC-ELEMENTS-SUPPRESSED
           ELSE
               PERFORM P4800-SANITY-CHECK-AMOUNT THRU P4800-EXIT.
       P4700-EXIT.
           EXIT.
      * P4800-SANITY-CHECK-AMOUNT - A NEGATIVE AMOUNT ON ANY ELEMENT   *
      * OTHER THAN CARRIER COMMON LINE (WHICH CAN LEGITIMATELY CARRY A *
      * VOLUME-DISCOUNT CREDIT) IS TREATED AS A CORRUPTED RETURN FROM  *
       P4800-SANITY-CHECK-AMOUNT.
           MOVE WS-EL-AMOUNT (WS-EL-X) TO WS-PACKED-AMT-WORK.
           IF WS-EL-AMOUNT (WS-EL-X) < 0 AND
                   WS-EL-ELEM-CODE (WS-EL-X) NOT = WS-ELEM-CCLINE
               DISPLAY 'CABRAT02 SUSPECT AMOUNT PACKED BYTES = '
                   WS-PACKED-AMT-INSPECT
               MOVE 0 TO WS-EL-AMOUNT (WS-EL-X).
       P4800-EXIT.
           EXIT.
      * S500-AMOUNT-COLLECTION SECTION - ACCUMULATES EVERY DISPATCHED  *
      * ELEMENT'S AMOUNT INTO THE BD-ELEMENT ODO OCCURRENCE, ROUNDS    *
      * THE RECORD TOTAL, BUILDS THE DESCRIPTION AND WRITES BDTLOUT -  *
       S500-AMOUNT-COLLECTION SECTION.
       P5000-COLLECT-AND-BUILD-DETAIL.
           MOVE 0 TO WS-BD-RAW-TOTAL.
           MOVE 0 TO BD-ELEM-CNT.
           MOVE 0 TO WS-BD-ELIGIBLE-CNT.
           MOVE 'N' TO WS-BD-OVERFLOW-SW.
           IF WS-EL-CNT > 0
               PERFORM P5100-ACCUMULATE-ELEMENT-AMOUNT THRU P5100-EXIT
                   VARYING WS-EL-X FROM 1 BY 1
                   UNTIL WS-EL-X > WS-EL-CNT.
           PERFORM P5250-CHECK-ELEMENT-OVERFLOW THRU P5250-EXIT.
           IF NOT WS-BD-OVERFLOW
               PERFORM P5300-COMPUTE-TOTALS THRU P5300-EXIT
               PERFORM P5500-APPLY-ROUND-RULE THRU P5500-EXIT
               PERFORM P5600-BUILD-DESCRIPTION THRU P5600-EXIT
               PERFORM P5700-CHECK-MODE-FOR-WRITE THRU P5700-EXIT
               IF WS-WRITE-BDTLOUT-SW = 'Y'
                   PERFORM P5750-WRITE-BDTLOUT THRU P5750-EXIT.
       P5000-EXIT.
           EXIT.
      * P5100-ACCUMULATE-ELEMENT-AMOUNT - EVERY NON-SUPPRESSED         *
      * ELEMENT COUNTS TOWARD WS-BD-ELIGIBLE-CNT (WHICH CAN EXCEED 40  *
      * BEFORE P5250 CATCHES IT); ONLY THE FIRST FORTY ARE ACTUALLY    *
       P5100-ACCUMULATE-ELEMENT-AMOUNT.
           IF NOT WS-EL-SUPPRESSED (WS-EL-X)
               ADD 1 TO WS-BD-ELIGIBLE-CNT
               IF BD-ELEM-CNT < 40
                   ADD 1 TO BD-ELEM-CNT
                   SET BD-EX TO BD-ELEM-CNT
                   PERFORM P5200-BUILD-BD-ELEMENT-OCCURRENCE THRU
                       P5200-EXIT
               MOVE WS-EL-AMOUNT (WS-EL-X) TO WS-BD-BUILD-AMT
               ADD WS-BD-BUILD-AMT TO WS-BD-RAW-TOTAL.
       P5100-EXIT.
           EXIT.
      * P5200-BUILD-BD-ELEMENT-OCCURRENCE.                             *
       P5200-BUILD-BD-ELEMENT-OCCURRENCE.
           MOVE WS-EL-ELEM-CODE (WS-EL-X) TO BD-EL-RATE-ELEM (BD-EX).
           MOVE WS-EL-QTY (WS-EL-X) TO BD-EL-QTY (BD-EX).
           IF WS-EL-R2-INDEX (WS-EL-X) > 0
               SET R2-EX TO WS-EL-R2-INDEX (WS-EL-X)
               MOVE R2-EN-INITIAL (R2-EX) TO BD-EL-RATE (BD-EX)
           ELSE
               MOVE 0 TO BD-EL-RATE (BD-EX).
           MOVE WS-EL-AMOUNT (WS-EL-X) TO BD-EL-AMOUNT (BD-EX).
           MOVE WS-EL-ROUND-RULE (WS-EL-X) TO BD-EL-ROUND-RULE (BD-EX).
           MOVE WS-EL-SRC-PROCESS (WS-EL-X) TO
               BD-EL-SRC-PROCESS (BD-EX).
           PERFORM P5220-UPDATE-ELEMENT-SUMMARY THRU P5220-EXIT.
       P5200-EXIT.
           EXIT.
      * P5220-UPDATE-ELEMENT-SUMMARY - LINEAR SEARCH OF THE TWENTY-    *
      * ROW RUN-LEVEL SUMMARY TABLE PRINTED BY P8200.  A NEW ELEMENT   *
      * CODE NOT SEEN BEFORE THIS RUN GETS A NEW ROW; THE TABLE HAS    *
       P5220-UPDATE-ELEMENT-SUMMARY.
           MOVE 'N' TO WS-ES-FOUND-SW.
           IF WS-ES-LOADED-CNT > 0
               PERFORM P5225-SCAN-ONE-SUMMARY THRU P5225-EXIT
                   VARYING WS-ES-X FROM 1 BY 1
                   UNTIL WS-ES-X > WS-ES-LOADED-CNT OR WS-ES-FOUND.
           IF NOT WS-ES-FOUND
               IF WS-ES-LOADED-CNT < 20
                   ADD 1 TO WS-ES-LOADED-CNT
                   SET WS-ES-X TO WS-ES-LOADED-CNT
                   MOVE WS-EL-ELEM-CODE (WS-EL-X) TO
                       WS-ES-ELEM-CODE (WS-ES-X)
                   MOVE 0 TO WS-ES-COUNT (WS-ES-X)
                   MOVE 0 TO WS-ES-QTY-TOTAL (WS-ES-X)
                   MOVE 0 TO WS-ES-AMT-TOTAL (WS-ES-X)
                   MOVE 'Y' TO WS-ES-FOUND-SW.
           IF WS-ES-FOUND
               ADD 1 TO WS-ES-COUNT (WS-ES-X)
               ADD WS-EL-QTY (WS-EL-X) TO WS-ES-QTY-TOTAL (WS-ES-X)
               ADD WS-EL-AMOUNT (WS-EL-X) TO WS-ES-AMT-TOTAL (WS-ES-X).
       P5220-EXIT.
           EXIT.
       P5225-SCAN-ONE-SUMMARY.
           IF WS-ES-ELEM-CODE (WS-ES-X) = WS-EL-ELEM-CODE (WS-EL-X)
               MOVE 'Y' TO WS-ES-FOUND-SW.
       P5225-EXIT.
           EXIT.
      * P5250-CHECK-ELEMENT-OVERFLOW.                                  *
       P5250-CHECK-ELEMENT-OVERFLOW.
           IF WS-BD-ELIGIBLE-CNT > 40
               MOVE 'Y' TO WS-BD-OVERFLOW-SW
               ADD 1 TO WS-MC-BD-OVERFLOW-CNT.
       P5250-EXIT.
           EXIT.
      * P5300-COMPUTE-TOTALS.                                          *
       P5300-COMPUTE-TOTALS.
           MOVE WS-BD-RAW-TOTAL TO BD-TOT-AMOUNT.
           MOVE 0 TO BD-TOT-MINUTES.
           IF BD-ELEM-CNT > 0
               PERFORM P5310-SUM-ONE-ELEM-QTY THRU P5310-EXIT
                   VARYING BD-EX FROM 1 BY 1
                   UNTIL BD-EX > BD-ELEM-CNT.
       P5300-EXIT.
           EXIT.
       P5310-SUM-ONE-ELEM-QTY.
           ADD BD-EL-QTY (BD-EX) TO BD-TOT-MINUTES.
       P5310-EXIT.
           EXIT.
      * P5400-DETERMINE-ROUND-RULE - USES THE FIRST ELEMENT'S ROUND    *
      * RULE AS THE RULE FOR THE RECORD-LEVEL TOTAL.  ORIGAC IS FIRST  *
      * ON EVERY NORMAL VOICE CDR (CCLINE IS LAST, PER P3500), SO IN   *
       P5400-DETERMINE-ROUND-RULE.
           MOVE 'U' TO R4-RULE.
           IF BD-ELEM-CNT > 0
               MOVE BD-EL-ROUND-RULE (1) TO R4-RULE.
           MOVE 2 TO R4-POS.
       P5400-EXIT.
           EXIT.
      * P5500-APPLY-ROUND-RULE - FOUR GENUINELY DIFFERENT ROUNDING     *
      * IMPLEMENTATIONS SELECTED BY GO TO ... DEPENDING ON.            *
       P5500-APPLY-ROUND-RULE.
           PERFORM P5400-DETERMINE-ROUND-RULE THRU P5400-EXIT.
           MOVE BD-TOT-AMOUNT TO R4-RAW-AMT.
           MOVE 1 TO WS-RD-RULE-INDEX.
           IF R4-RULE = 'E'
               MOVE 2 TO WS-RD-RULE-INDEX.
           IF R4-RULE = 'T'
               MOVE 3 TO WS-RD-RULE-INDEX.
           IF R4-RULE = 'C'
               MOVE 4 TO WS-RD-RULE-INDEX.
           GO TO P5510-ROUND-HALF-UP P5520-ROUND-HALF-EVEN
               P5530-ROUND-TRUNCATE P5540-ROUND-ALWAYS-UP
               DEPENDING ON WS-RD-RULE-INDEX.
      * P5510-ROUND-HALF-UP - RT-ROUND-HALF-UP ('U').  STANDARD COBOL  *
      * ROUNDED PHRASE - ROUNDS AWAY FROM ZERO AT THE MIDPOINT.        *
       P5510-ROUND-HALF-UP.
           COMPUTE R4-ROUNDED-AMT ROUNDED = R4-RAW-AMT.
           GO TO P5550-FINISH-ROUND.
      * P5520-ROUND-HALF-EVEN - RT-ROUND-HALF-EVEN ('E').  BANKER'S    *
      * ROUNDING - AT AN EXACT HALF-CENT, ROUNDS TO WHICHEVER CENT     *
      * VALUE IS EVEN.  NO INTRINSIC FUNCTION AVAILABLE IN THIS        *
       P5520-ROUND-HALF-EVEN.
           MOVE R4-RAW-AMT TO R4-ROUNDED-AMT.
           COMPUTE R4-RESIDUE = R4-RAW-AMT - R4-ROUNDED-AMT.
           IF R4-RESIDUE > 0.005
               ADD 0.01 TO R4-ROUNDED-AMT
           ELSE
               IF R4-RESIDUE = 0.005
                   COMPUTE WS-RL-CENTS-INT = R4-ROUNDED-AMT * 100
                   COMPUTE WS-RL-HALF-INT = WS-RL-CENTS-INT / 2
                   COMPUTE WS-RL-CHECK-INT = WS-RL-HALF-INT * 2
                   IF WS-RL-CHECK-INT NOT = WS-RL-CENTS-INT
                       ADD 0.01 TO R4-ROUNDED-AMT.
           GO TO P5550-FINISH-ROUND.
      * P5530-ROUND-TRUNCATE - RT-TRUNCATE ('T').  A MOVE BETWEEN TWO  *
      * NUMERIC FIELDS OF DIFFERENT SCALE ALIGNS ON THE DECIMAL POINT  *
      * AND DROPS THE LOW-ORDER DIGITS - NO ROUNDING AT ALL.           *
       P5530-ROUND-TRUNCATE.
           MOVE R4-RAW-AMT TO R4-ROUNDED-AMT.
           GO TO P5550-FINISH-ROUND.
      * P5540-ROUND-ALWAYS-UP - RT-ROUND-UP-ALWAYS ('C').  ANY NON-    *
      * ZERO RESIDUE, HOWEVER SMALL, PUSHES THE AMOUNT AWAY FROM ZERO  *
      * BY A FULL CENT - USED ON A HANDFUL OF SURCHARGE ELEMENTS       *
       P5540-ROUND-ALWAYS-UP.
           MOVE R4-RAW-AMT TO R4-ROUNDED-AMT.
           COMPUTE R4-RESIDUE = R4-RAW-AMT - R4-ROUNDED-AMT.
           IF R4-RESIDUE > 0
               ADD 0.01 TO R4-ROUNDED-AMT.
           IF R4-RESIDUE < 0
               SUBTRACT 0.01 FROM R4-ROUNDED-AMT.
       P5550-FINISH-ROUND.
           MOVE R4-ROUNDED-AMT TO BD-TOT-ROUNDED.
           COMPUTE BD-ROUND-DELTA = BD-TOT-AMOUNT - BD-TOT-ROUNDED.
       P5500-EXIT.
           EXIT.
      * P5600-BUILD-DESCRIPTION - ONE STRING STATEMENT ASSEMBLING THE  *
      * 60-BYTE BD-DESCRIPTION FROM SIX FRAGMENTS: THE ELEMENT NAME,   *
      * A SEPARATOR, THE JURISDICTION-DERIVED BAND TEXT, ANOTHER       *
       P5600-BUILD-DESCRIPTION.
           MOVE SPACES TO WS-DESC-FRAG1 WS-DESC-FRAG3 WS-DESC-FRAG5
               WS-DESC-FRAG6.
           MOVE SPACE TO WS-DESC-FRAG2 WS-DESC-FRAG4.
           IF BD-ELEM-CNT > 0
               MOVE BD-EL-RATE-ELEM (1) TO WS-DESC-FRAG1
               MOVE WS-CW-JURIS-CD TO WS-DESC-FRAG3
               MOVE BD-EL-RATE (1) TO R4-EDIT-RATE
               MOVE R4-EDIT-RATE TO WS-DESC-FRAG5.
           MOVE 'ELEM RATED' TO WS-DESC-FRAG6.
           MOVE SPACES TO BD-DESCRIPTION.
           STRING WS-DESC-FRAG1 DELIMITED BY SIZE
                  WS-DESC-FRAG2 DELIMITED BY SIZE
                  WS-DESC-FRAG3 DELIMITED BY SIZE
                  WS-DESC-FRAG4 DELIMITED BY SIZE
                  WS-DESC-FRAG5 DELIMITED BY SIZE
                  WS-DESC-FRAG6 DELIMITED BY SIZE
               INTO BD-DESCRIPTION.
       P5600-EXIT.
           EXIT.
      * P5700-CHECK-MODE-FOR-WRITE - CABSRT01 DEFINES R1-ANY-LIVE-     *
      * MODE ('P' 'L') AND R1-ANY-TEST-MODE ('L' 'S') AS OVERLAPPING   *
      * ON 'L' (PARALLEL).  THIS PARAGRAPH TESTS R1-ANY-TEST-MODE      *
       P5700-CHECK-MODE-FOR-WRITE.
           MOVE 'Y' TO WS-WRITE-BDTLOUT-SW.
           IF R1-ANY-TEST-MODE
               MOVE 'N' TO WS-WRITE-BDTLOUT-SW
           ELSE
               IF R1-ANY-LIVE-MODE
                   MOVE 'Y' TO WS-WRITE-BDTLOUT-SW.
       P5700-EXIT.
           EXIT.
      * P5750-WRITE-BDTLOUT - BD-ELEM-CNT WAS ALREADY SET BY P5100     *
      * BEFORE THIS RUNS, WHICH IS WHAT DRIVES THE VB RECORD LENGTH.   *
       P5750-WRITE-BDTLOUT.
           MOVE WS-CW-BAN TO BD-BAN.
           MOVE WS-PV-BILL-PERIOD TO BD-BILL-PERIOD.
           MOVE WS-BD-SECTION-CD TO BD-SECTION.
           ADD 1 TO WS-BD-LINE-SEQ-CTR.
           MOVE WS-BD-LINE-SEQ-CTR TO BD-LINE-SEQ.
           MOVE WS-CW-OCN TO BD-OCN.
           MOVE WS-CW-JURIS-CD TO BD-JURIS-CD.
           MOVE WS-CW-STATE-CD TO BD-STATE-CD.
           WRITE CABS-BILL-DETAIL.
       P5750-EXIT.
           EXIT.
      * S800-CONTROL-BALANCE SECTION - THE MANDATORY CONTROL STEP,     *
      * PLUS THE PRINTED REPORT (HEADER, PER-ELEMENT DISPATCH SUMMARY, *
      * EXCEPTION COUNTS AND THE CONTROL TOTALS THEMSELVES).           *
       S800-CONTROL-BALANCE SECTION.
       P8000-CONTROL.
           PERFORM P8100-BUILD-REPORT-HEADER THRU P8100-EXIT.
           PERFORM P8150-PRINT-DISPATCH-METHOD-MIX THRU P8150-EXIT.
           PERFORM P8200-PRINT-ELEMENT-SUMMARY THRU P8200-EXIT.
           PERFORM P8250-PRINT-JURIS-TOTALS THRU P8250-EXIT.
           PERFORM P8700-PRINT-EXCEPTION-SUMMARY THRU P8700-EXIT.
           PERFORM P8300-BUILD-CONTROL-REC THRU P8300-EXIT.
           PERFORM P8400-CHECK-BALANCE THRU P8400-EXIT.
           PERFORM P8600-PRINT-CONTROL-SUMMARY THRU P8600-EXIT.
           PERFORM P8500-WRITE-CONTROL-REC THRU P8500-EXIT.
       P8000-EXIT.
           EXIT.
      * P8100-BUILD-REPORT-HEADER.                                     *
       P8100-BUILD-REPORT-HEADER.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '1' TO PC-CC.
           MOVE 'CABRAT02 - ACCESS RATING DRIVER - CONTROL REPORT' TO
               PC-TEXT.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RUN ID' TO PC-COL-001-020.
           MOVE WS-PV-RUN-ID TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'CYCLE YYDDD' TO PC-COL-001-020.
           MOVE WS-PV-CYCLE-YYDDD TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'TARIFF' TO PC-COL-001-020.
           MOVE WS-PV-TARIFF-CD TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'MODE' TO PC-COL-001-020.
           MOVE WS-PV-MODE-SW TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RATE ROWS LOADED' TO PC-COL-001-020.
           MOVE R2-ENTRY-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'ACTIVE RATE ROWS' TO PC-COL-001-020.
           MOVE WS-ARI-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'CARRIERS CACHED' TO PC-COL-001-020.
           MOVE WS-CC-LOADED-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           WRITE CABS-PRINT-LINE.
       P8100-EXIT.
           EXIT.
      * P8150-PRINT-DISPATCH-METHOD-MIX - HOW MANY ELEMENTS WENT DOWN *
      * EACH OF THE FOUR DISPATCH PATHS THIS RUN, FOR OPERATIONS TO   *
      * CONFIRM AN OVERRIDE OR RETRY CARD ACTUALLY TOOK EFFECT.       *
       P8150-PRINT-DISPATCH-METHOD-MIX.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '4' TO PC-CC.
           MOVE 'DISPATCH METHOD MIX' TO PC-TEXT.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  MAIN (P4100)' TO PC-COL-001-020.
           MOVE WS-MC-MAIN-DISPATCH TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  JURIS (P4200)' TO PC-COL-001-020.
           MOVE WS-MC-JURIS-DISPATCH TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  OVERRIDE (P4600)' TO PC-COL-001-020.
           MOVE WS-MC-OVERRIDE-DISPATCH TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  RETRY (P4700)' TO PC-COL-001-020.
           MOVE WS-MC-RETRY-DISPATCH TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           WRITE CABS-PRINT-LINE.
       P8150-EXIT.
           EXIT.
      * P8200-PRINT-ELEMENT-SUMMARY - LEVEL-BREAK STYLE LISTING OF     *
      * EVERY DISTINCT ELEMENT CODE DISPATCHED THIS RUN.               *
       P8200-PRINT-ELEMENT-SUMMARY.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '4' TO PC-CC.
           MOVE 'ELEMENT DISPATCH SUMMARY' TO PC-TEXT.
           WRITE CABS-PRINT-LINE.
           IF WS-ES-LOADED-CNT > 0
               PERFORM P8210-PRINT-ONE-ELEMENT-LINE THRU P8210-EXIT
                   VARYING WS-ES-X FROM 1 BY 1
                   UNTIL WS-ES-X > WS-ES-LOADED-CNT.
       P8200-EXIT.
           EXIT.
       P8210-PRINT-ONE-ELEMENT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-ES-ELEM-CODE (WS-ES-X) TO PC-AMT-DESC.
           MOVE WS-ES-QTY-TOTAL (WS-ES-X) TO PC-AMT-QTY.
           MOVE WS-ES-AMT-TOTAL (WS-ES-X) TO PC-AMT-VALUE.
           WRITE CABS-PRINT-LINE.
       P8210-EXIT.
           EXIT.
      * P8250-PRINT-JURIS-TOTALS - THREE FIXED ROWS, I THEN S THEN L. *
       P8250-PRINT-JURIS-TOTALS.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '4' TO PC-CC.
           MOVE 'JURISDICTION TOTALS' TO PC-TEXT.
           WRITE CABS-PRINT-LINE.
           PERFORM P8260-PRINT-ONE-JURIS-LINE THRU P8260-EXIT
               VARYING WS-JSU-X FROM 1 BY 1
               UNTIL WS-JSU-X > 3.
       P8250-EXIT.
           EXIT.
       P8260-PRINT-ONE-JURIS-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-JSU-JURIS-CD (WS-JSU-X) TO PC-AMT-DESC.
           MOVE WS-JSU-COUNT (WS-JSU-X) TO PC-AMT-QTY.
           MOVE WS-JSU-AMT-TOTAL (WS-JSU-X) TO PC-AMT-VALUE.
           WRITE CABS-PRINT-LINE.
       P8260-EXIT.
           EXIT.
      * P8300-BUILD-CONTROL-REC - POPULATES CABS-CONTROL-RECORD.       *
      * CT-SUMMARISED IS ALWAYS ZERO - THIS DRIVER EITHER WRITES A     *
      * RATED RECORD OR REJECTS IT, IT DOES NOT SUMMARISE.             *
       P8300-BUILD-CONTROL-REC.
           MOVE WS-PV-RUN-ID TO CT-RUN-ID.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE 1 TO CT-STEP-SEQ.
           MOVE WS-PV-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE WS-PV-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE SPACES TO CT-JOBNAME.
           MOVE SPACES TO CT-STEPNAME.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE 0 TO CT-SUMMARISED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-HW-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-HW-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-HW-SEQ TO CT-HASH-SEQ.
           MOVE WS-HW-OCN TO CT-HASH-OCN.
           MOVE WS-RESTART-KEY-SAVE TO CT-RESTART-KEY.
           MOVE SPACES TO CT-FILLER.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-ABEND-CD.
       P8300-EXIT.
           EXIT.
      * P8400-CHECK-BALANCE - THE MANDATORY BALANCING TEST.            *
       P8400-CHECK-BALANCE.
           IF CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARISED +
                   CT-CARRIED-FWD
               MOVE 'B' TO CT-BAL-IND
           ELSE
               MOVE 'O' TO CT-BAL-IND.
       P8400-EXIT.
           EXIT.
      * P8500-WRITE-CONTROL-REC.                                       *
       P8500-WRITE-CONTROL-REC.
           MOVE CABS-CONTROL-RECORD TO CABS-CTLOUT-RECORD.
           WRITE CABS-CTLOUT-RECORD.
       P8500-EXIT.
           EXIT.
      * P8600-PRINT-CONTROL-SUMMARY.                                   *
       P8600-PRINT-CONTROL-SUMMARY.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '4' TO PC-CC.
           MOVE 'CONTROL TOTALS -' TO PC-TEXT.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  CT-READ' TO PC-COL-001-020.
           MOVE CT-READ TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  CT-WRITTEN' TO PC-COL-001-020.
           MOVE CT-WRITTEN TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  CT-REJECTED' TO PC-COL-001-020.
           MOVE CT-REJECTED TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  CT-CARRIED-FWD' TO PC-COL-001-020.
           MOVE CT-CARRIED-FWD TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  CT-BAL-IND' TO PC-COL-001-020.
           MOVE CT-BAL-IND TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           IF CT-OUT-OF-BAL
               MOVE SPACES TO CABS-PRINT-LINE
               MOVE ' ' TO PC-CC
               MOVE '  *** OUT OF BALANCE - SEE CTLOUT ***' TO
                   PC-TEXT
               WRITE CABS-PRINT-LINE.
       P8600-EXIT.
           EXIT.
      * P8700-PRINT-EXCEPTION-SUMMARY - NONE OF THESE COUNTS FEED THE  *
      * BALANCING EQUATION.  INFORMATIONAL ONLY.                       *
       P8700-PRINT-EXCEPTION-SUMMARY.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '4' TO PC-CC.
           MOVE 'EXCEPTIONS -' TO PC-COL-001-020.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  DYNAMIC CALLS' TO PC-COL-001-020.
           MOVE WS-MC-DYNAMIC-CALLS TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  MAIN DISPATCH' TO PC-COL-001-020.
           MOVE WS-MC-MAIN-DISPATCH TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  JURIS DISPATCH' TO PC-COL-001-020.
           MOVE WS-MC-JURIS-DISPATCH TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  OVERRIDE DISPATCH' TO PC-COL-001-020.
           MOVE WS-MC-OVERRIDE-DISPATCH TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  RETRY DISPATCH' TO PC-COL-001-020.
           MOVE WS-MC-RETRY-DISPATCH TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  RETRY SUCCESS' TO PC-COL-001-020.
           MOVE WS-MC-RETRY-SUCCESS TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  MODULE NOT FOUND' TO PC-COL-001-020.
           MOVE WS-MC-MODULE-NOT-FOUND TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  ELEMENTS SUPPRESSED' TO PC-COL-001-020.
           MOVE WS-MC-ELEMENTS-SUPPRESSED TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  ELEMENTS DEDUPED' TO PC-COL-001-020.
           MOVE WS-MC-ELEMENTS-DEDUPED TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  BD OVERFLOW' TO PC-COL-001-020.
           MOVE WS-MC-BD-OVERFLOW-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  MATRIX EXACT' TO PC-COL-001-020.
           MOVE WS-MC-MATRIX-EXACT-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  MATRIX FALLBACK' TO PC-COL-001-020.
           MOVE WS-MC-MATRIX-FALLBACK-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  MATRIX MISS' TO PC-COL-001-020.
           MOVE WS-MC-MATRIX-MISS-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           WRITE CABS-PRINT-LINE.
       P8700-EXIT.
           EXIT.
      * S900-TERMINATION SECTION.                                      *
       S900-TERMINATION SECTION.
       P9000-TERM.
           CLOSE RATIN.
           CLOSE RATEMST.
           CLOSE CARRMST.
           CLOSE RATOUT.
           CLOSE BDTLOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABRAT02 - RUN COMPLETE'.
           DISPLAY '  READ        = ' WS-READ-CNT.
           DISPLAY '  WRITTEN     = ' WS-WRITE-CNT.
           DISPLAY '  REJECTED    = ' WS-REJECT-CNT.
           DISPLAY '  CARRIED FWD = ' WS-CFWD-CNT.
           DISPLAY '  DYNAMIC CALLS = ' WS-MC-DYNAMIC-CALLS.
           DISPLAY '  MODULE RETRY  = ' WS-MC-RETRY-DISPATCH.
           DISPLAY '  ELEM SUPPRESS = ' WS-MC-ELEMENTS-SUPPRESSED.
           DISPLAY '  MATRIX MISSES = ' WS-MC-MATRIX-MISS-CNT.
           DISPLAY '  BAL IND      = ' CT-BAL-IND.
       P9000-EXIT.
           EXIT.
      * P9900-FATAL-OPEN - REACHED FROM P1100 AND P1250 WHEN A FILE    *
      * FAILS TO OPEN OR THE PARM CARD IS UNUSABLE.  THE RUN CANNOT    *
      * PROCEED WITHOUT ALL SEVEN PERMANENT FILES AND A VALID CYCLE.   *
       P9900-FATAL-OPEN.
           MOVE WS-PGM-NAME TO WS-AB-PGM.
           DISPLAY 'CABRAT02 FATAL - ' WS-AB-REASON.
           CALL 'CABABEND' USING WS-AB-PGM WS-AB-PARA WS-AB-REASON
               WS-AB-USER-CODE.
           STOP RUN.
       P9900-EXIT.
           EXIT.
      * P9910-MODULE-FAILURE - HIDDEN AT THE PHYSICAL BOTTOM OF THE    *
      * PROGRAM, REACHED ONLY VIA GO TO FROM P4000-DISPATCH-LOOP-ONE-  *
      * ELEMENT WHEN A DISPATCH RETURNS A FATAL RETURN CODE.  A FATAL  *
       P9910-MODULE-FAILURE.
           MOVE WS-PGM-NAME TO WS-AB-PGM.
           MOVE 'S400-DISPATCH' TO WS-AB-PARA.
           MOVE 'FATAL RETURN CODE FROM RATING MODULE - SEE SYSOUT' TO
               WS-AB-REASON.
           MOVE WS-RC-CURRENT TO WS-AB-USER-CODE.
           PERFORM P8300-BUILD-CONTROL-REC THRU P8300-EXIT.
           MOVE 'O' TO CT-BAL-IND.
           MOVE WS-RC-CURRENT TO CT-RC.
           MOVE '9910' TO CT-ABEND-CD.
           PERFORM P8500-WRITE-CONTROL-REC THRU P8500-EXIT.
           DISPLAY 'CABRAT02 FATAL - ' WS-AB-REASON.
           CALL 'CABABEND' USING WS-AB-PGM WS-AB-PARA WS-AB-REASON
               WS-AB-USER-CODE.
           STOP RUN.
       P9910-EXIT.
           EXIT.
