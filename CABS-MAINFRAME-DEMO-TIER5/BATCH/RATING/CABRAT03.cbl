      *****************************************************************
      * CABRAT03 - SWITCHED ACCESS RATING - FIVE ELEMENT RATER        *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               RATIN   TELCABS.CABS.USAGE.VOICE(0) CABSCDR    *
      *               RATEMST TELCABS.CABS.RATE (VSAM KSDS) CABSRATE *
      *               CARRMST TELCABS.CABS.CARRIER (VSAM KSDS)CABSCARR*
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
      *   V1.00  1987-04-14  R.T.WHEELER  INITIAL RELEASE - ORIGAC    *
      *                      AND TERMAC ONLY, FLAT PER-MINUTE RATE,   *
      *                      NO MILEAGE BANDING YET                   *
      *   V1.03  1988-11-02  R.T.WHEELER  ADDED LOCAL TRANSPORT WITH  *
      *                      V AND H COORDINATE MILEAGE BANDING       *
      *   V1.05  1990-02-27  D.OKONKWO    TANDEM SWITCHING ELEMENT    *
      *                      ADDED PER FCC ACCESS TARIFF NO 1         *
      *   V1.09  1992-07-19  D.OKONKWO    CARRIER COMMON LINE ELEMENT *
      *                      ADDED - INTERSTATE ONLY, PREMIUM SPLIT   *
      *   V1.10  1993-04-19  D.OKONKWO    ADDED FEATURE GROUP A/B/D   *
      *                      DETERMINATION FROM TRUNK GROUP AND       *
      *                      CIC FOR EQUAL ACCESS ROUTING - CCL       *
      *                      PREMIUM SPLIT NOW DRIVEN BY FEATURE      *
      *                      GROUP INSTEAD OF CARRIER TYPE            *
      *   V1.12  1994-03-08  J.M.CASTILLO ADDED VOLUME DISCOUNT       *
      *                      ROUTINE FOR OCN OVER 50M MOU PER MONTH   *
      *   V1.15  1996-09-30  J.M.CASTILLO Y2K REMEDIATION - CENTURY   *
      *                      WINDOW PIVOT INTRODUCED AT YY=70         *
      *   V1.17  1996-11-14  J.M.CASTILLO ADDED 8YY DATABASE QUERY    *
      *                      CHARGE (DBQURY) FOR SMS/800 LOOKUPS,     *
      *                      THIRD-PARTY DATABASE SURCHARGE AND       *
      *                      QUERY VOLUME DISCOUNT BANDING            *
      *   V1.18  1998-05-11  P.NAIR       SEQUENTIAL RATE TABLE SCAN  *
      *                      REPLACED WITH BINARY-STYLE SEARCH        *
      *   V2.00  2001-01-22  P.NAIR       REMOVED CARRIER COMMON LINE *
      *                      PREMIUM / NON-PREMIUM SPLIT - SINGLE     *
      *                      FLAT CCL RATE NOW APPLIES ALL TRAFFIC    *
      *   V2.01  2003-06-14  A.BUKOWSKI   DYNAMIC CALL OVERRIDE PATH  *
      *                      ADDED FOR RATE-TABLE-DRIVEN MODULE CALLS *
      *   V2.03  2005-10-02  A.BUKOWSKI   INTERNAL SORT INTRODUCED    *
      *                      FOR BAN LEVEL-BREAK BILL DETAIL BUILD    *
      *   V2.06  2011-02-25  S.MARCHETTI  MILEAGE SQUARE ROOT         *
      *                      CONVERGENCE TOLERANCE TIGHTENED PER      *
      *                      INTERNAL AUDIT FINDING 2010-118          *
      *   V2.09  2016-04-07  T.VANCE      TANDEM SWITCHING ELEMENT    *
      *                      NOW ROUNDS CONSISTENTLY WITH THE OTHER   *
      *                      FOUR ELEMENTS PER SOX REMEDIATION RC-44  *
      *   V2.10  2017-09-13  M.HOLLIS     ADDED SUSPENSE ROUTE FOR    *
      *                      TERMINATING ACCESS RATE-NOT-FOUND CASE   *
      *                      (SOFT REJECT INSTEAD OF ABEND)           *
      *   V2.11  2019-03-05  G.PRZYBYLSKI RATE TABLE ODO EXPANDED TO  *
      *                      600 ENTRIES, BAND POOL EXPANDED TO 2400  *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABRAT03.
       AUTHOR. TELCABS APPLICATIONS - RATING TEAM.
      *****************************************************************
      * SWITCHED ACCESS RATING.  READS VALIDATED WHOLESALE VOICE      *
      * USAGE AND APPLIES ACCESS RATES FOR FIVE SEPARATE RATE         *
      * ELEMENTS PER CALL RECORD - ORIGINATING ACCESS, TERMINATING    *
      * ACCESS, LOCAL TRANSPORT, TANDEM SWITCHING AND CARRIER COMMON  *
      * LINE.  EACH ELEMENT PRODUCES ITS OWN BILL DETAIL ELEMENT.     *
      * THIS IS THE LARGEST AND OLDEST RATING PROGRAM IN THE CABS     *
      * SUITE.  READ CABSRT01 THROUGH CABSRT04 BEFORE CHANGING IT.    *
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
           SELECT SORTWK ASSIGN TO SORTWK1.
       DATA DIVISION.
       FILE SECTION.
      *****************************************************************
      * RATIN - VALIDATED VOICE USAGE INPUT.  ONE CDR PER SWITCHED    *
      * ACCESS CALL, ALREADY EDITED BY CABING SUITE.                  *
      *****************************************************************
       FD  RATIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       COPY CABSCDR.
      *****************************************************************
      * RATEMST - ACCESS RATE TABLE, VSAM KSDS, KEYED BROWSE AT INIT. *
      *****************************************************************
       FD  RATEMST
           LABEL RECORDS ARE STANDARD.
       COPY CABSRATE.
      *****************************************************************
      * CARRMST - CARRIER MASTER, VSAM KSDS, RANDOM READ PER OCN.     *
      *****************************************************************
       FD  CARRMST
           LABEL RECORDS ARE STANDARD.
       COPY CABSCARR.
      *****************************************************************
      * RATOUT - RATED CDR PASS-THROUGH.  ONE RECORD PER INPUT CDR,   *
      * CARRYING RATE STATUS AND TOTAL RATED AMOUNT FOR AUDIT TRAIL.  *
      * NO SEPARATE COPYBOOK - LOCAL LAYOUT, RATING OUTPUT ONLY.      *
      *****************************************************************
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
               88  RO-SUSPENDED             VALUE 'S'.
           05  RO-CYCLE-YYDDD              PIC 9(05).
           05  RO-JURIS-CD                 PIC X(01).
           05  RO-STATE-CD                 PIC X(02).
           05  RO-ELEM-COUNT               PIC 9(02).
           05  RO-TOT-MINUTES              PIC S9(09)V9(02) COMP-3.
           05  RO-TOT-AMOUNT               PIC S9(11)V9(05) COMP-3.
           05  RO-FEATURE-GRP               PIC X(01).
           05  RO-FILLER                   PIC X(151).
      *****************************************************************
      * BDTLOUT - BILL DETAIL, VARIABLE LENGTH, UP TO 40 ELEMENTS.    *
      *****************************************************************
       FD  BDTLOUT
           RECORDING MODE IS V
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD IS VARYING IN SIZE FROM 108 TO 1647 CHARACTERS
               DEPENDING ON BD-ELEM-CNT.
       COPY CABSBILL.
      *****************************************************************
      * SUSOUT - REJECTED / SUSPENDED USAGE.  CARRIES CABS-SUSPENSE-  *
      * RECORD LAYOUT FROM CABSERR (COPIED DIRECTLY - CABS-STD-002    *
      * SAYS DON'T, THIS PROGRAM HAS DONE IT SINCE V1.00 REGARDLESS). *
      *****************************************************************
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
      *****************************************************************
      * CTLOUT - RUN CONTROL / BALANCING RECORD.                      *
      *****************************************************************
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD              PIC X(180).
      *****************************************************************
      * RPTOUT - PRINT REPORT, LEVEL-BREAK ON OCN AND ELEMENT.        *
      *****************************************************************
       FD  RPTOUT
           RECORDING MODE IS FA
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       COPY CABSPRNT.
      *****************************************************************
      * SORTWK - INTERMEDIATE RATED-ELEMENT SORT FILE.  ONE RECORD    *
      * PER RATED ELEMENT PER CDR, SORTED OCN / BAN / JURIS / ELEM    *
      * SO S350-SORT-OUTPUT CAN BUILD ONE CABS-BILL-DETAIL PER BAN.   *
      *****************************************************************
       SD  SORTWK
           RECORD CONTAINS 160 CHARACTERS.
       01  WS-SORT-RECORD.
           05  SR-KEY.
               10  SR-OCN                  PIC X(04).
               10  SR-BAN                  PIC X(13).
               10  SR-JURIS-CD             PIC X(01).
               10  SR-RATE-ELEM            PIC X(06).
           05  SR-STATE-CD                 PIC X(02).
           05  SR-BILL-PERIOD              PIC 9(06).
           05  SR-SECTION                  PIC X(02).
           05  SR-SEQ-NBR                  PIC 9(09) COMP-3.
           05  SR-QTY                      PIC S9(13)V9(02) COMP-3.
           05  SR-RATE                     PIC S9(05)V9(05) COMP-3.
           05  SR-AMOUNT                   PIC S9(11)V9(05) COMP-3.
           05  SR-ROUND-RULE               PIC X(01).
           05  SR-SRC-PROCESS              PIC X(08).
           05  SR-MILEAGE-BAND             PIC X(04).
           05  SR-CCL-ELIGIBLE-SW          PIC X(01).
           05  SR-DESCRIPTION              PIC X(60).
           05  SR-FILLER                   PIC X(24).
       WORKING-STORAGE SECTION.
      *****************************************************************
      * STANDARD SHARED WORKING STORAGE - SWITCHES, COUNTERS, ERROR   *
      * CODES, DATE WORK, CONTROL RECORD.  SEE CABSWRK.               *
      *****************************************************************
       COPY CABSWRK.
      *****************************************************************
      * RATING FAMILY CONTROL BLOCKS - RUN CONTROL, RATE TABLE, BAND  *
      * POOL, ROUNDING WORK.  ONE COPY STATEMENT PULLS ALL FOUR.      *
      *****************************************************************
       COPY CABSRT01.
      *****************************************************************
      * PROGRAM CONSTANTS AND LITERALS                                *
      *****************************************************************
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABRAT03'.
           05  WS-PGM-VERSION               PIC X(05) VALUE 'V2.11'.
           05  WS-ELEM-ORIGAC               PIC X(06) VALUE 'ORIGAC'.
           05  WS-ELEM-TERMAC               PIC X(06) VALUE 'TERMAC'.
           05  WS-ELEM-LTRANS               PIC X(06) VALUE 'LTRANS'.
           05  WS-ELEM-TANSW                PIC X(06) VALUE 'TANSW '.
           05  WS-ELEM-CCLINE               PIC X(06) VALUE 'CCLINE'.
           05  WS-SUPPRESS-THRESHOLD        PIC S9(03)V9(05) COMP-3
                                                            VALUE 0.005.
           05  WS-ELEM-DBQURY               PIC X(06) VALUE 'DBQURY'.
           05  WS-OWN-CIC-LOW                PIC 9(04) VALUE 0100.
           05  WS-OWN-CIC-HIGH               PIC 9(04) VALUE 0199.
           05  WS-FG-PREM-FACTOR             PIC S9(01)V9(05) COMP-3
                                                          VALUE 1.00000.
           05  WS-FG-NONPREM-FACTOR          PIC S9(01)V9(05) COMP-3
                                                          VALUE 0.85000.
           05  WS-8YY-SURCHARGE-RATE         PIC S9(03)V9(05) COMP-3
                                                          VALUE 0.02000.
      *****************************************************************
      * SYSIN PARM CARD - CONTROLS THE CYCLE.  REDEFINED TWO WAYS -   *
      * POSITIONAL (R1) FOR THE NORMAL PRODUCTION CARD AND KEYWORD    *
      * (R2) FOR THE FREE-FORM OVERRIDE CARD USED IN PARALLEL RUNS.   *
      * WHICH LAYOUT APPLIES IS DECIDED IN P1200 BY TESTING BYTE 1.   *
      *****************************************************************
       01  WS-PARM-CARD                    PIC X(80).
       01  WS-PARM-CARD-R1 REDEFINES WS-PARM-CARD.
           05  PC1-CYCLE-YYDDD              PIC 9(05).
           05  PC1-BILL-PERIOD               PIC 9(06).
           05  PC1-TARIFF-CD                PIC X(04).
           05  PC1-MODE-SW                  PIC X(01).
           05  PC1-TANDEM-SW                PIC X(01).
           05  PC1-RUN-ID                   PIC X(12).
           05  PC1-RESTART-KEY.
               10  PC1-RSK-OCN               PIC X(04).
               10  PC1-RSK-BAN               PIC X(13).
               10  PC1-RSK-SEQ               PIC 9(09).
           05  PC1-FILLER                   PIC X(25).
       01  WS-PARM-CARD-R2 REDEFINES WS-PARM-CARD.
           05  PC2-KEYWORD-SW               PIC X(01).
           05  PC2-KEYWORD                  PIC X(09).
           05  PC2-VALUE-CYCLE              PIC 9(05).
           05  PC2-VALUE-REST               PIC X(65).
      *****************************************************************
      * WORKING COPY OF CDR CONTEXT CARRIED THROUGH THE RATING PASS.  *
      *****************************************************************
       01  WS-CDR-WORK-AREA.
           05  WS-CW-OCN                    PIC X(04).
           05  WS-CW-BAN                    PIC X(13).
           05  WS-CW-SEQ-NBR                PIC 9(09) COMP-3.
           05  WS-CW-JURIS-CD               PIC X(01).
           05  WS-CW-STATE-CD               PIC X(02).
           05  WS-CW-ORIG-LATA               PIC 9(03).
           05  WS-CW-TERM-LATA               PIC 9(03).
           05  WS-CW-ORIG-LATA-X             PIC X(03).
           05  WS-CW-TERM-LATA-X             PIC X(03).
           05  WS-CW-CONV-MIN               PIC S9(07)V9(02) COMP-3.
           05  WS-CW-TANDEM-IND             PIC X(01).
           05  WS-CW-CIC                    PIC 9(04).
      *****************************************************************
      * ELEMENT CLASSIFICATION - OVERLAPPING 88 LEVELS.  WS-EC-       *
      * SWITCHED AND WS-EC-TRANSPORT ARE BOTH TRUE FOR 'TANSW '.      *
      * P2450-CLASSIFY-FOR-REPORT TESTS WS-EC-SWITCHED FIRST, SO A    *
      * TANDEM ELEMENT LANDS IN THE SWITCHED-ACCESS REPORT BUCKET     *
      * EVEN THOUGH IT IS ALSO A TRANSPORT-CLASS ELEMENT.  THIS HAS   *
      * BEEN TRUE SINCE V1.05 AND NO ONE HAS ASKED FOR IT TO CHANGE.  *
      *****************************************************************
       01  WS-ELEMENT-CLASSIFY.
           05  WS-ELEM-CLASS                PIC X(06).
               88  WS-EC-SWITCHED           VALUE 'ORIGAC' 'TERMAC'
                                                   'TANSW '.
               88  WS-EC-TRANSPORT          VALUE 'TANSW ' 'LTRANS'.
               88  WS-EC-CARRIER-LINE       VALUE 'CCLINE'.
      *****************************************************************
      * RATE KEY WORK AREA - COMPONENT FIELDS AND A FLAT 18-BYTE      *
      * VIEW OF THE SAME BYTES.  THE 66-LEVEL BELOW RENAMES THE       *
      * TARIFF-THROUGH-STATE PORTION AS WS-TARIFF-LOOKUP.  P4100      *
      * REFERS TO WS-TARIFF-LOOKUP; THE REAL FIELDS ARE THE WS-RK-*   *
      * ELEMENTARY ITEMS.  DO NOT RENAME THIS AGAIN - CABS-STD-019.   *
      *****************************************************************
       01  WS-RATE-KEY-WORK.
           05  WS-RK-TARIFF                 PIC X(04).
           05  WS-RK-ELEM                   PIC X(06).
           05  WS-RK-JURIS                  PIC X(01).
           05  WS-RK-STATE                  PIC X(02).
           05  WS-RK-EFF-YYDDD              PIC 9(05).
           66  WS-TARIFF-LOOKUP RENAMES WS-RK-TARIFF THRU WS-RK-STATE.
       01  WS-RATE-KEY-FLAT REDEFINES WS-RATE-KEY-WORK.
           05  WS-RK-FLAT-VIEW               PIC X(18).
      *****************************************************************
      * MIRROR OF THE CURRENT R2 TABLE ROW'S KEY, BUILT FRESH EACH    *
      * PASS OF THE BINARY SEARCH SO IT CAN BE COMPARED AS A GROUP    *
      * AGAINST WS-TARIFF-LOOKUP (R2-EN-KEY LIVES IN A FROZEN         *
      * COPYBOOK AND CANNOT CARRY ITS OWN 66-LEVEL RENAMES).          *
      *****************************************************************
       01  WS-CURR-TABLE-KEY.
           05  WS-CTK-TARIFF                PIC X(04).
           05  WS-CTK-ELEM                  PIC X(06).
           05  WS-CTK-JURIS                 PIC X(01).
           05  WS-CTK-STATE                 PIC X(02).
      *****************************************************************
      * BINARY-STYLE SEARCH WORK AREA FOR THE INTERNAL RATE TABLE.    *
      *****************************************************************
       01  WS-BINARY-SEARCH-WORK.
           05  WS-BS-LOW                    PIC S9(04) COMP-3.
           05  WS-BS-HIGH                   PIC S9(04) COMP-3.
           05  WS-BS-MID                    PIC S9(04) COMP-3.
           05  WS-BS-CMP-SW                 PIC X(01).
               88  WS-BS-EQUAL              VALUE 'E'.
               88  WS-BS-LOWER               VALUE 'L'.
               88  WS-BS-HIGHER              VALUE 'H'.
           05  WS-BS-DONE-SW                PIC X(01) VALUE 'N'.
               88  WS-BS-DONE               VALUE 'Y'.
           05  WS-BS-LANDED-SUB             PIC S9(04) COMP-3.
      *****************************************************************
      * RESOLVED-RATE RESULT AREA - WHAT P4000-RESOLVE-RATE HANDS     *
      * BACK TO THE ELEMENT RATING PARAGRAPHS IN S500.                *
      *****************************************************************
       01  WS-RATE-RESOLUTION-RESULT.
           05  WS-RR-FOUND-SW               PIC X(01) VALUE 'N'.
               88  WS-RR-FOUND              VALUE 'Y'.
               88  WS-RR-NOT-FOUND           VALUE 'N'.
           05  WS-RR-FALLBACK-LVL           PIC X(01).
               88  WS-RR-EXACT-STATE        VALUE '1'.
               88  WS-RR-JURIS-GENERIC      VALUE '2'.
               88  WS-RR-TARIFF-DEFAULT     VALUE '3'.
           05  WS-RR-INDEX                  PIC S9(04) COMP-3.
           05  WS-SEL-INIT-RATE             PIC S9(05)V9(05) COMP-3.
           05  WS-SEL-ADDL-RATE             PIC S9(05)V9(05) COMP-3.
           05  WS-SEL-RATE                  PIC S9(05)V9(05) COMP-3.
           05  WS-SEL-SETUP-CHG             PIC S9(07)V9(05) COMP-3.
           05  WS-SEL-MIN-CHG               PIC S9(07)V9(02) COMP-3.
           05  WS-SEL-MAX-CHG               PIC S9(11)V9(02) COMP-3.
           05  WS-SEL-ROUND-RULE            PIC X(01).
           05  WS-SEL-ROUND-POS             PIC 9(01).
           05  WS-SEL-INIT-PERIOD           PIC 9(04).
           05  WS-SEL-ADDL-PERIOD           PIC 9(04).
           05  WS-SEL-MODULE-SFX            PIC X(02).
           05  WS-SEL-BAND-CNT              PIC 9(02).
           05  WS-SEL-BAND-OFFSET           PIC 9(04).
      *****************************************************************
      * ELEMENT CONTROL TABLE - DRIVES THE PER-ELEMENT DISPATCH LOOP  *
      * IN P2400.  FIVE ENTRIES, ONE PER RATE ELEMENT, LOADED BY      *
      * P1750-BUILD-ELEMENT-TABLE AT INIT TIME (NOT FROM A FILE).     *
      *****************************************************************
       01  WS-ELEMENT-CONTROL-TABLE.
           05  WS-EC-ENTRY OCCURS 5 TIMES INDEXED BY WS-EC-X.
               10  WS-EC-ELEM-CODE          PIC X(06).
               10  WS-EC-ELEM-NAME          PIC X(16).
               10  WS-EC-COND-TANDEM-ONLY   PIC X(01).
               10  WS-EC-COND-INTERSTATE    PIC X(01).
      *****************************************************************
      * V AND H COORDINATE SEED TABLE - EMBEDDED LITERAL DATA.        *
      * THIS PROGRAM HAS NO DD FOR A V&H FILE, SO THE COORDINATE      *
      * TABLE HAS BEEN CARRIED AS A LITERAL SINCE V1.03 (1988).  THE  *
      * SAME PATTERN IS USED BY BELLCORE'S OWN NPA-NXX V&H TAPE       *
      * LOADER, WHICH IS WHERE R.T.WHEELER COPIED THE IDEA FROM.      *
      * FORMAT PER ENTRY: LATA(3) V-COORD(4) H-COORD(4) = 11 BYTES.   *
      *****************************************************************
       01  WS-VH-SEED.
           05  FILLER PIC X(770) VALUE
               '12182383912132320490741435253500615448284
      -        '14316590333839176854390671877467371219878
      -        '37645620932603244220376747912314905713924
      -        '27931321725375974628264886583232758745746
      -        '42866436480529766797827308527896303193053
      -        '92163309601430734187196462352578752763634
      -        '27347633749254575738538373759396611237924
      -        '07594099424185817794542951669611440335589
      -        '77451676373924624022610047336457522484540
      -        '19794495814980665065962772951745758771528
      -        '35693375539841748665509332537056136534907
      -        '57238276113583527767145948207983360559884
      -        '33261660325910627471684906385187874964985
      -        '99830866035847990671820144016827375897369
      -        '35005433870467866108715521182427268637756
      -        '27374799860874856569904759929493567703458
      -        '48767819732326279295945584803628651938143
      -        '54247288257646888183655774741847836970898
      -        '58624182668696758417088051694143'.
       01  WS-VH-SEED-R REDEFINES WS-VH-SEED.
           05  WS-VH-SEED-ENTRY OCCURS 70 TIMES.
               10  WS-VHS-LATA               PIC X(03).
               10  WS-VHS-V                   PIC 9(04).
               10  WS-VHS-H                   PIC 9(04).
      *****************************************************************
      * WORKING V AND H TABLE - LOADED FROM THE SEED ABOVE PLUS ANY   *
      * LATA ENTRIES DERIVED FROM CARRMST DURING P1400.  CAPACITY IS  *
      * 400 TO ALLOW FOR FUTURE LATA SPLITS WITHOUT A RECOMPILE.      *
      *****************************************************************
       01  WS-VH-TABLE.
           05  WS-VH-CNT                    PIC 9(03) VALUE 0.
           05  WS-VH-ENTRY OCCURS 1 TO 400 TIMES
                    DEPENDING ON WS-VH-CNT
                    INDEXED BY WS-VH-X.
               10  WS-VH-LATA                PIC X(03).
               10  WS-VH-VCOORD               PIC 9(04).
               10  WS-VH-HCOORD               PIC 9(04).
      *****************************************************************
      * V AND H COORDINATE PAIR FOR THE CURRENT CALL - REDEFINED SO   *
      * P6200-COMPUTE-DIFFERENCES CAN WORK ON IT NUMERICALLY WHILE    *
      * P8100-BUILD-REPORT-HEADER CAN DUMP THE SAME BYTES AS TEXT     *
      * FOR THE EXCEPTION LINE WHEN A COORDINATE LOOKUP MISSES.       *
      *****************************************************************
       01  WS-VH-COORD-PAIR.
           05  WS-VH-ORIG-V                 PIC 9(04).
           05  WS-VH-ORIG-H                 PIC 9(04).
           05  WS-VH-TERM-V                 PIC 9(04).
           05  WS-VH-TERM-H                 PIC 9(04).
       01  WS-VH-COORD-PAIR-CHAR REDEFINES WS-VH-COORD-PAIR.
           05  WS-VH-ORIG-V-X               PIC X(04).
           05  WS-VH-ORIG-H-X               PIC X(04).
           05  WS-VH-TERM-V-X               PIC X(04).
           05  WS-VH-TERM-H-X               PIC X(04).
      *****************************************************************
      * STATE / LATA CROSS-REFERENCE SEED TABLE - EMBEDDED LITERAL.   *
      * FORMAT PER ENTRY: STATE(2) LATA(3) JURIS-DEFAULT(1) = 6 BYTES *
      *****************************************************************
       01  WS-LATA-SEED.
           05  FILLER PIC X(360) VALUE
               'AL372SAR882SCA694SCO671SCT389SDE884SFL718
      -        'SGA558SIL717SIN528SKY490SLA344SMA261SMD64
      -        '1SMI625SMN213SMO893SMS168SNC232SNJ276SNY7
      -        '62SOH283SOK931SOR816SPA552STN730STX185SVA
      -        '514SWA510SWI730SAL599SAR661SCA377SCO686SC
      -        'T131SDE816SFL858SGA237SIL818SIN669SKY888S
      -        'LA393SMA907SMD776SMI468SMN234SMO420SMS565
      -        'SNC281SNJ584SNY123SOH859SOK856SOR389SPA63
      -        '2STN900STX302SVA639SWA228SWI760S'.
       01  WS-LATA-SEED-R REDEFINES WS-LATA-SEED.
           05  WS-LATA-SEED-ENTRY OCCURS 60 TIMES.
               10  WS-LTS-STATE               PIC X(02).
               10  WS-LTS-LATA                PIC X(03).
               10  WS-LTS-JURIS-DFLT          PIC X(01).
      *****************************************************************
      * WORKING STATE/LATA CROSS-REFERENCE TABLE - LOADED FROM SEED.  *
      *****************************************************************
       01  WS-LATA-XREF-TABLE.
           05  WS-LX-CNT                    PIC 9(03) VALUE 0.
           05  WS-LX-ENTRY OCCURS 1 TO 250 TIMES
                    DEPENDING ON WS-LX-CNT
                    INDEXED BY WS-LX-X.
               10  WS-LX-STATE                PIC X(02).
               10  WS-LX-LATA                 PIC X(03).
               10  WS-LX-JURIS-DFLT           PIC X(01).
      *****************************************************************
      * MILEAGE COMPUTATION WORK AREA - V AND H DIFFERENCE, SQUARED   *
      * SUM, AND A NEWTON'S-METHOD SQUARE ROOT ITERATION.  OS/VS      *
      * COBOL HAS NO SQRT FUNCTION SO THIS IS DONE LONGHAND, AS IT    *
      * HAS BEEN SINCE V1.03.                                         *
      *****************************************************************
       01  WS-MILEAGE-WORK.
           05  WS-MW-V-DIFF                  PIC S9(05) COMP-3.
           05  WS-MW-H-DIFF                  PIC S9(05) COMP-3.
           05  WS-MW-V-DIFF-SQ               PIC S9(11) COMP-3.
           05  WS-MW-H-DIFF-SQ               PIC S9(11) COMP-3.
           05  WS-MW-SUM-SQ                  PIC S9(11) COMP-3.
           05  WS-MW-RADICAND                PIC S9(11)V9(04) COMP-3.
           05  WS-MW-ROOT-EST                PIC S9(07)V9(04) COMP-3.
           05  WS-MW-ROOT-PREV               PIC S9(07)V9(04) COMP-3.
           05  WS-MW-ROOT-DELTA              PIC S9(07)V9(04) COMP-3.
           05  WS-MW-CONVERGE-TOL            PIC S9(05)V9(04) COMP-3
                                                    VALUE 0.0050.
           05  WS-MW-ITERATION-CNT           PIC S9(03) COMP-3
                                                            VALUE 0.
           05  WS-MW-MAX-ITERATIONS          PIC S9(03) COMP-3
                                                            VALUE 40.
           05  WS-MW-CONVERGED-SW            PIC X(01) VALUE 'N'.
               88  WS-MW-CONVERGED           VALUE 'Y'.
           05  WS-MW-MILES                   PIC S9(07)V9(02) COMP-3.
           05  WS-MW-VH-FOUND-SW             PIC X(01) VALUE 'N'.
               88  WS-MW-VH-FOUND            VALUE 'Y'.
           05  WS-MW-CEILING-CNT             PIC S9(09) COMP-3
                                                          VALUE 0.
      *****************************************************************
      * MILEAGE BAND SEARCH WORK - WALKS R3-POOL-ENTRY FROM THE       *
      * OFFSET CARRIED ON THE RESOLVED LTRANS RATE TABLE ROW.         *
      *****************************************************************
       01  WS-MILEAGE-BAND-WORK.
           05  WS-MB-BAND-TEXT               PIC X(04).
           05  WS-MB-FOUND-SW                PIC X(01) VALUE 'N'.
               88  WS-MB-FOUND               VALUE 'Y'.
           05  WS-MB-SUB                     PIC S9(04) COMP-3.
           05  WS-MB-SEL-RATE                PIC S9(05)V9(05) COMP-3.
           05  WS-MB-SCAN-SUB                PIC S9(04) COMP-3.
           05  WS-MB-SCAN-LIMIT              PIC S9(04) COMP-3.
           05  WS-MB-ORDINAL                  PIC 9(02).
      *****************************************************************
      * BILLABLE MINUTE CONVERSION SCRATCH - SHARED SHAPE, BUT EACH   *
      * OF THE FIVE S500 ELEMENT PARAGRAPHS DOES ITS OWN CONVERSION   *
      * INLINE RATHER THAN CALLING A COMMON ROUTINE.  THAT HAS BEEN   *
      * TRUE SINCE EACH ELEMENT WAS WRITTEN BY A DIFFERENT PROGRAMMER *
      * IN A DIFFERENT DECADE AND NO ONE HAS EVER CONSOLIDATED THEM.  *
      *****************************************************************
       01  WS-CONV-WORK.
           05  WS-CONV-SECONDS               PIC S9(07)V9(02) COMP-3.
           05  WS-CONV-SECONDS-WHOLE         PIC S9(07) COMP-3.
           05  WS-CONV-ADDL-SECONDS          PIC S9(07) COMP-3.
           05  WS-CONV-ADDL-PERIODS          PIC S9(05) COMP-3.
           05  WS-CONV-ADDL-REM              PIC S9(05) COMP-3.
           05  WS-CONV-BILL-SECONDS          PIC S9(07) COMP-3.
      *****************************************************************
      * ELEMENT RATING RESULT - SHARED BY EACH S500 PARAGRAPH JUST    *
      * BEFORE THE SORT RECORD IS BUILT AND RELEASED.                 *
      *****************************************************************
       01  WS-ELEMENT-RESULT.
           05  WS-BILLABLE-MIN               PIC S9(13)V9(02) COMP-3.
           05  WS-ELEM-AMOUNT                PIC S9(11)V9(05) COMP-3.
           05  WS-TANDEM-COUNT               PIC S9(02) COMP-3
                                                            VALUE 1.
           05  WS-CCL-PREMIUM-SW             PIC X(01) VALUE 'N'.
               88  WS-CCL-IS-PREMIUM         VALUE 'Y'.
           05  WS-MIN-MAX-APPLIED-SW         PIC X(01) VALUE ' '.
               88  WS-MIN-CHG-APPLIED        VALUE 'N'.
               88  WS-MAX-CHG-APPLIED        VALUE 'X'.
           05  WS-ELEM-APPLIES-SW            PIC X(01) VALUE 'Y'.
      *****************************************************************
      * COMPUTED AMOUNT REDEFINED TO INSPECT WHOLE DOLLARS AND        *
      * FRACTIONAL CENTS SEPARATELY - USED BY P8300 WHEN THE REPORT   *
      * FLAGS AN ELEMENT AMOUNT THAT ROUNDED TO ZERO.                 *
      *****************************************************************
       01  WS-AMT-INSPECT                    PIC S9(11)V9(05).
       01  WS-AMT-INSPECT-R REDEFINES WS-AMT-INSPECT.
           05  WS-AI-WHOLE                   PIC 9(11).
           05  WS-AI-FRACTION                PIC 9(05).
      *****************************************************************
      * BILL DESCRIPTION FRAGMENTS - ASSEMBLED BY P3700 VIA STRING.   *
      * EACH FRAGMENT IS SET IN A DIFFERENT, DISTANT PARAGRAPH - THE  *
      * ELEMENT NAME IN S500, THE JURISDICTION WORD IN S200, THE      *
      * MILEAGE BAND TEXT IN S600, AND THE RATE/PERIOD TEXT IN S350   *
      * ITSELF.  NO SINGLE PARAGRAPH SHOWS THE WHOLE PICTURE.         *
      *****************************************************************
       01  WS-DESC-FRAGMENTS.
           05  WS-DESC-FRAG1                 PIC X(16).
           05  WS-DESC-FRAG2                 PIC X(11).
           05  WS-DESC-FRAG3                 PIC X(03).
           05  WS-DESC-FRAG4                 PIC X(10).
           05  WS-DESC-FRAG5                 PIC X(08).
           05  WS-DESC-FRAG6                 PIC X(08).
           05  WS-DESC-ASSEMBLED             PIC X(60).
      *****************************************************************
      * DYNAMIC CALL TARGET WORK - SEE P7200.                         *
      *****************************************************************
       01  WS-CALL-WORK.
           05  WS-TGT-PREFIX                 PIC X(06).
           05  WS-TGT-SUFFIX                 PIC X(02).
           05  WS-CALL-USED-SW               PIC X(01) VALUE 'N'.
               88  WS-CALL-USED              VALUE 'Y'.
           05  WS-RTFMT-RATE-IN              PIC S9(05)V9(05) COMP-3.
           05  WS-RTFMT-OUT                  PIC X(15).
      *****************************************************************
      * CARRIER LOOKUP RESULT - LOADED FROM CARRMST PER OCN.  THE     *
      * CCL ELIGIBILITY TEST BELOW IS THE ONLY PLACE IN THE PROGRAM   *
      * WHERE THAT RULE IS EXPRESSED - IT IS APPLIED IN S300, NOT     *
      * HERE, WHICH IS WHY IT IS SO EASY TO MISS ON A CODE READ.      *
      *****************************************************************
       01  WS-CARRIER-WORK.
           05  WS-CR-FOUND-SW                PIC X(01) VALUE 'N'.
               88  WS-CR-FOUND               VALUE 'Y'.
           05  WS-CR-TYPE                    PIC X(01).
           05  WS-CR-CCL-ELIGIBLE-SW         PIC X(01) VALUE 'N'.
               88  WS-CR-CCL-ELIGIBLE        VALUE 'Y'.
           05  WS-CR-RECIP-ELIG              PIC X(01).
           05  WS-CR-PLU                     PIC S9(03)V9(05).
           05  WS-CR-ISP-CAP                 PIC S9(13).
      *****************************************************************
      * RECIPROCAL COMPENSATION / ISP CAP RUN TOTALS - INFORMATIONAL,  *
      * PRINTED ON THE EXCEPTION SUMMARY (P8700), NEVER FED BACK INTO  *
      * THE BALANCING EQUATION.                                        *
      *****************************************************************
       01  WS-RECIP-SETTLE-WORK.
           05  WS-RS-TOTAL-MINUTES           PIC S9(13)V9(02) COMP-3
                                                          VALUE 0.
           05  WS-RS-TOTAL-AMOUNT            PIC S9(13)V9(05) COMP-3
                                                          VALUE 0.
           05  WS-RS-CAP-EXCEEDED-CNT        PIC S9(09) COMP-3
                                                          VALUE 0.
      *****************************************************************
      * JURISDICTION DISPATCH WORK.                                   *
      *****************************************************************
       01  WS-JURIS-DISPATCH-WORK.
           05  WS-JD-JURIS-WORD              PIC X(11).
           05  WS-JD-DETERMINED-SW           PIC X(01) VALUE 'N'.
               88  WS-JD-DETERMINED          VALUE 'Y'.
           05  WS-JD-LATA-FOUND-SW           PIC X(01) VALUE 'N'.
               88  WS-JD-LATA-FOUND          VALUE 'Y'.
           05  WS-JD-FOUND-STATE             PIC X(02).
           05  WS-JD-FOUND-DEFAULT-JURIS     PIC X(01).
           05  WS-JD-NPA-FALLBACK-CNT        PIC S9(09) COMP-3
                                                          VALUE 0.
      *****************************************************************
      * NPA-NXX FALLBACK WORK - THIRD AND LAST JURISDICTION TEST, ONLY *
      * REACHED IF BOTH THE LATA CROSS-REFERENCE (P2310) AND THE PIU   *
      * CROSS-CHECK (P2330) FAILED TO DETERMINE A JURISDICTION.  THE   *
      * REDEFINES BELOW SPLIT THE SIX-DIGIT NPANXX INTO ITS THREE-     *
      * DIGIT AREA CODE WITHOUT REFERENCE MODIFICATION.                *
      *****************************************************************
       01  WS-NPA-COMPARE-WORK.
           05  WS-NPA-ORIG-FULL              PIC 9(06).
           05  WS-NPA-ORIG-R REDEFINES WS-NPA-ORIG-FULL.
               10  WS-NPA-ORIG-NPA            PIC 9(03).
               10  WS-NPA-ORIG-NXX            PIC 9(03).
           05  WS-NPA-TERM-FULL              PIC 9(06).
           05  WS-NPA-TERM-R REDEFINES WS-NPA-TERM-FULL.
               10  WS-NPA-TERM-NPA            PIC 9(03).
               10  WS-NPA-TERM-NXX            PIC 9(03).
      *****************************************************************
      * PREVIOUS-RECORD KEY - DUPLICATE SEQUENCE DETECTION.  RATIN    *
      * ARRIVES IN KEY SEQUENCE, SO AN EXACT DUPLICATE SHOWS UP AS    *
      * TWO CONSECUTIVE RECORDS WITH THE SAME OCN/BAN/SEQ.            *
      *****************************************************************
       01  WS-PREV-KEY-WORK.
           05  WS-PK-OCN                     PIC X(04) VALUE SPACES.
           05  WS-PK-BAN                     PIC X(13) VALUE SPACES.
           05  WS-PK-SEQ-NBR                 PIC 9(09) VALUE 0.
           05  WS-PK-FIRST-SW                PIC X(01) VALUE 'Y'.
               88  WS-PK-FIRST-RECORD        VALUE 'Y'.
      *****************************************************************
      * ABEND CALL WORK - PASSED TO CABABEND FROM P9990.              *
      *****************************************************************
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                     PIC X(08).
           05  WS-AB-PARA                    PIC X(30).
           05  WS-AB-REASON                  PIC X(60).
           05  WS-AB-USER-CODE               PIC 9(04) VALUE 9903.
      *****************************************************************
      * MISCELLANEOUS RUN COUNTERS - NOT PART OF THE STANDARD BLOCK.  *
      *****************************************************************
       01  WS-MISC-COUNTERS.
           05  WS-MC-ELEMENTS-RATED          PIC S9(09) COMP-3
                                                          VALUE 0.
           05  WS-MC-ELEMENTS-DROPPED-CCL    PIC S9(09) COMP-3
                                                          VALUE 0.
           05  WS-MC-ELEMENTS-SUPPRESSED     PIC S9(09) COMP-3
                                                          VALUE 0.
           05  WS-MC-SORT-RELEASED           PIC S9(09) COMP-3
                                                          VALUE 0.
           05  WS-MC-SORT-RETURNED           PIC S9(09) COMP-3
                                                          VALUE 0.
           05  WS-MC-BANS-WRITTEN            PIC S9(09) COMP-3
                                                          VALUE 0.
           05  WS-MC-DYNAMIC-CALLS           PIC S9(09) COMP-3
                                                          VALUE 0.
           05  WS-MC-RATE-NOT-FOUND-SOFT     PIC S9(09) COMP-3
                                                          VALUE 0.
           05  WS-MC-BD-TABLE-FULL           PIC S9(09) COMP-3
                                                          VALUE 0.
           05  WS-MC-VH-EXACT-CNT            PIC S9(09) COMP-3
                                                          VALUE 0.
           05  WS-MC-VH-FALLBACK-CNT         PIC S9(09) COMP-3
                                                          VALUE 0.
           05  WS-MC-ZERO-DURATION-CNT       PIC S9(09) COMP-3
                                                          VALUE 0.
           05  WS-MC-CHECKPOINT-QUOT         PIC S9(09) COMP-3
                                                          VALUE 0.
           05  WS-MC-CHECKPOINT-REM          PIC S9(09) COMP-3
                                                          VALUE 0.
           05  WS-MC-MAX-LOAD-YYDDD          PIC 9(05) VALUE 0.
           05  WS-MC-8YY-QUERIES-RATED       PIC S9(09) COMP-3
                                                          VALUE 0.
           05  WS-MC-8YY-THIRD-PARTY-CNT     PIC S9(09) COMP-3
                                                          VALUE 0.
      *****************************************************************
      * SORT CONTROL - AT-END SWITCH FOR THE RETURN LOOP IN S350.     *
      *****************************************************************
       01  WS-SORT-CONTROL.
           05  WS-SORT-EOF-SW                PIC X(01) VALUE 'N'.
               88  WS-SORT-EOF               VALUE 'Y'.
           05  WS-SORT-INPUT-EOF-SW          PIC X(01) VALUE 'N'.
               88  WS-SORT-INPUT-EOF         VALUE 'Y'.
      *****************************************************************
      * VSAM BROWSE CONTROL - RATEMST LOAD LOOP (P1300).              *
      *****************************************************************
       01  WS-VSAM-BROWSE-WORK.
           05  WS-VB-START-KEY.
               10  WS-VB-TARIFF               PIC X(04).
               10  WS-VB-ELEM                  PIC X(06).
               10  WS-VB-JURIS                 PIC X(01).
               10  WS-VB-STATE                 PIC X(02).
               10  WS-VB-EFF                   PIC 9(05).
           05  WS-VB-EOF-SW                  PIC X(01) VALUE 'N'.
               88  WS-VB-EOF                 VALUE 'Y'.
      *****************************************************************
      * CARRMST BROWSE CONTROL - NOT USED FOR A FULL TABLE LOAD, THE  *
      * CARRIER MASTER IS READ RANDOM PER OCN INSTEAD.  RETAINED FOR  *
      * SYMMETRY WITH THE RATE TABLE LOAD AND FOR A POSSIBLE FUTURE   *
      * CARRIER-TABLE PRELOAD THAT HAS BEEN "COMING SOON" SINCE 2003. *
      *****************************************************************
       01  WS-CARRMST-WORK.
           05  WS-CM-READ-SW                 PIC X(01) VALUE 'N'.
               88  WS-CM-READ-OK              VALUE 'Y'.
      *****************************************************************
      * RESTART WORK - CT-RESTART-KEY, IN THE FORM OCN(4)+BAN(13)+    *
      * SEQ(9), IS TYPED IN FROM A PRIOR RUN'S CTLOUT/RPTOUT ONTO THE *
      * PARM CARD'S PC1-RESTART-KEY FIELD FOR A RERUN.  RATIN ARRIVES *
      * IN OCN/BAN/SEQ SEQUENCE, SO A SIMPLE KEY COMPARE IS ENOUGH TO *
      * SKIP EVERYTHING ALREADY PROCESSED BY THE FAILED RUN.          *
      *****************************************************************
       01  WS-RESTART-WORK.
           05  WS-RESTART-KEY-SAVE           PIC X(26).
           05  WS-RESTART-SKIP-SW            PIC X(01) VALUE 'N'.
               88  WS-RESTART-SKIP           VALUE 'Y'.
       01  WS-RESTART-CDR-KEY.
           05  WS-RSK-OCN                    PIC X(04).
           05  WS-RSK-BAN                    PIC X(13).
           05  WS-RSK-SEQ                    PIC 9(09).
      *****************************************************************
      * ROUND-RULE DISPATCH WORK - GO TO ... DEPENDING ON USES THIS.  *
      *****************************************************************
       01  WS-ROUND-DISPATCH-WORK.
           05  WS-RD-RULE-INDEX              PIC 9(01) VALUE 0.
      *****************************************************************
      * INIT / OPEN STATUS WORK.                                      *
      *****************************************************************
       01  WS-INIT-STATUS-WORK.
           05  WS-IS-OPEN-OK-SW              PIC X(01) VALUE 'Y'.
               88  WS-IS-ALL-OPENED-OK       VALUE 'Y'.
      *****************************************************************
      * RETURN CODE AREAS FOR THE STATIC CALLS USED BY THIS PROGRAM.  *
      *****************************************************************
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                   PIC 9(04) VALUE 0.
           05  WS-RC-ERRWR                   PIC 9(04) VALUE 0.
           05  WS-RC-HASH                    PIC 9(04) VALUE 0.
           05  WS-RC-OCNVL                   PIC 9(04) VALUE 0.
           05  WS-RC-RTFMT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                   PIC 9(04) VALUE 0.
      *****************************************************************
      * TABLE LOAD COUNTERS - REPORTED ON THE CONTROL REPORT HEADER.  *
      *****************************************************************
       01  WS-TABLE-LOAD-COUNTERS.
           05  WS-TL-RATE-ROWS-LOADED        PIC S9(05) COMP-3
                                                          VALUE 0.
           05  WS-TL-BAND-ROWS-LOADED        PIC S9(05) COMP-3
                                                          VALUE 0.
           05  WS-TL-VH-ROWS-LOADED          PIC S9(05) COMP-3
                                                          VALUE 0.
           05  WS-TL-LATA-ROWS-LOADED        PIC S9(05) COMP-3
                                                          VALUE 0.
           05  WS-VH-SUB                     PIC S9(03) COMP-3
                                                          VALUE 0.
           05  WS-LX-SUB                     PIC S9(03) COMP-3
                                                          VALUE 0.
           05  WS-TL-DISC-ELIGIBLE-CNT       PIC S9(05) COMP-3
                                                          VALUE 0.
           05  WS-TL-DISC-SAVE               PIC X(01) VALUE 'N'.
           05  WS-TL-MPB-ELIGIBLE-CNT        PIC S9(05) COMP-3
                                                          VALUE 0.
      *****************************************************************
      * EFFECTIVE-DATED VIEW - THE CYCLE DATE THE RATE SEARCH FILTERS *
      * AGAINST, ESTABLISHED ONCE AT INIT FROM THE PARM CARD.         *
      *****************************************************************
       01  WS-EFFECTIVE-VIEW-WORK.
           05  WS-EV-CYCLE-YYDDD             PIC 9(05).
           05  WS-EV-CYCLE-CCYYDDD           PIC 9(07).
           05  WS-EV-CENTURY                 PIC 9(02).
      *****************************************************************
      * LOCAL DATE WORK - CENTURY DERIVATION FOR RATE TABLE EFFECTIVE *
      * AND EXPIRY DATES.  P4200 HARDCODES THE PIVOT LITERAL RATHER   *
      * THAN USING DW-PIVOT-YY FROM CABSDATE - THAT INCONSISTENCY IS  *
      * LONGSTANDING AND IS RECORDED IN THE CABS-STD REGISTER.        *
      *****************************************************************
       01  WS-DATE-WORK-LOCAL.
           05  WS-DL-EFF-CCYY                PIC 9(04).
           05  WS-DL-EXP-CCYY                 PIC 9(04).
           05  WS-DL-CONN-CCYY                PIC 9(04).
      *****************************************************************
      * HASH ACCUMULATION WORK - FEEDS THE FOUR CT-HASH-* TOTALS AT   *
      * P8400.  ACCUMULATED IN THE ORDER RECORDS ARRIVE ON RATIN.     *
      *****************************************************************
       01  WS-HASH-WORK.
           05  WS-HW-MINUTES                 PIC S9(15)V9(02) COMP-3
                                                          VALUE 0.
           05  WS-HW-AMOUNT                  PIC S9(13)V9(05) COMP-3
                                                          VALUE 0.
           05  WS-HW-SEQ                     PIC S9(17)       COMP-3
                                                          VALUE 0.
           05  WS-HW-OCN                     PIC S9(15)       COMP-3
                                                          VALUE 0.
      *****************************************************************
      * REPORT WORK - PAGE / LINE CONTROL, HEADINGS, EDIT FIELDS.     *
      *****************************************************************
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR               PIC 9(04) VALUE 0.
           05  WS-RPT-LINE-NBR               PIC 9(02) VALUE 99.
           05  WS-RPT-LINES-PER-PAGE         PIC 9(02) VALUE 55.
           05  WS-RPT-RUN-DATE               PIC X(08).
           05  WS-RPT-RUN-TIME               PIC X(08).
           05  WS-RPT-TITLE1                 PIC X(40) VALUE
               'TELCABS  -  SWITCHED ACCESS RATING'.
           05  WS-RPT-TITLE2                 PIC X(40) VALUE
               'PROGRAM CABRAT03  -  ELEMENT SUMMARY'.
      *****************************************************************
      * EDIT MASKS FOR REPORT AND SUSPENSE TEXT.                      *
      *****************************************************************
       01  WS-EDIT-MASKS.
           05  WS-ED-AMOUNT                  PIC Z,ZZZ,ZZZ,ZZ9.99-.
           05  WS-ED-MINUTES                  PIC Z,ZZZ,ZZ9.99-.
           05  WS-ED-RATE                    PIC Z.ZZZZ9.
           05  WS-ED-OCN                     PIC X(04).
           05  WS-ED-BAN                     PIC X(13).
           05  WS-ED-COUNT                   PIC ZZZ,ZZ9.
      *****************************************************************
      * PER-ELEMENT REPORT TOTALS - ACCUMULATED ACROSS THE WHOLE RUN, *
      * PRINTED AT P8300.  INDEXED SAME ORDER AS WS-ELEMENT-CONTROL.  *
      *****************************************************************
       01  WS-REPORT-TOTALS-TABLE.
           05  WS-RT-ELEM-TOT OCCURS 5 TIMES INDEXED BY WS-RT-X.
               10  WS-RT-ELEM-CODE            PIC X(06).
               10  WS-RT-ELEM-MINUTES         PIC S9(13)V9(02)
                                                  COMP-3 VALUE 0.
               10  WS-RT-ELEM-AMOUNT          PIC S9(13)V9(05)
                                                  COMP-3 VALUE 0.
               10  WS-RT-ELEM-CNT             PIC S9(09) COMP-3
                                                          VALUE 0.
               10  WS-RT-MIN-APPLIED-CNT      PIC S9(09) COMP-3
                                                          VALUE 0.
               10  WS-RT-MAX-APPLIED-CNT      PIC S9(09) COMP-3
                                                          VALUE 0.
      *****************************************************************
      * CURRENT-OCN REPORT ACCUMULATOR - RESET ON EACH OCN LEVEL      *
      * BREAK IN P8200.                                               *
      *****************************************************************
       01  WS-OCN-TOTALS.
           05  WS-OT-CNT                     PIC 9(03) VALUE 0.
           05  WS-OT-ENTRY OCCURS 1 TO 200 TIMES
                    DEPENDING ON WS-OT-CNT
                    INDEXED BY WS-OT-X.
               10  WS-OT-OCN                  PIC X(04).
               10  WS-OT-MINUTES              PIC S9(13)V9(02)
                                                  COMP-3 VALUE 0.
               10  WS-OT-AMOUNT               PIC S9(13)V9(05)
                                                  COMP-3 VALUE 0.
               10  WS-OT-CALL-CNT             PIC S9(09) COMP-3
                                                  VALUE 0.
               10  WS-OT-CARRIER-TYPE         PIC X(01).
           05  WS-OT-FOUND-SW                PIC X(01) VALUE 'N'.
               88  WS-OT-FOUND               VALUE 'Y'.
           05  WS-OT-FOUND-NUM               PIC S9(03) COMP-3
                                                          VALUE 0.
      *****************************************************************
      * DISTINCT END OFFICE TABLE - FIXED SIZE, NOT AN ODO TABLE,     *
      * BECAUSE 100 SLOTS IS FAR MORE THAN ANY REALISTIC SWITCH COUNT *
      * FOR ONE OCN'S TRAFFIC AND THE WASTED SPACE IS TRIVIAL.        *
      *****************************************************************
       01  WS-END-OFFICE-TABLE.
           05  WS-EO-CNT                     PIC 9(03) VALUE 0.
           05  WS-EO-ENTRY OCCURS 100 TIMES INDEXED BY WS-EO-X.
               10  WS-EO-CODE                 PIC X(11).
               10  WS-EO-CALL-CNT             PIC S9(09) COMP-3
                                                  VALUE 0.
           05  WS-EO-FOUND-SW                PIC X(01) VALUE 'N'.
               88  WS-EO-FOUND               VALUE 'Y'.
           05  WS-EO-FOUND-NUM               PIC S9(03) COMP-3
                                                          VALUE 0.
      *****************************************************************
      * BAN LEVEL-BREAK WORK - S350 BUILDS ONE CABS-BILL-DETAIL PER   *
      * BAN FROM THE SORTED RATED-ELEMENT RECORDS RETURNED BY SORT.   *
      *****************************************************************
       01  WS-BAN-BREAK-WORK.
           05  WS-BB-SAVE-BAN                PIC X(13) VALUE SPACES.
           05  WS-BB-SAVE-OCN                PIC X(04) VALUE SPACES.
           05  WS-BB-SAVE-JURIS              PIC X(01) VALUE SPACES.
           05  WS-BB-SAVE-STATE              PIC X(02) VALUE SPACES.
           05  WS-BB-SAVE-SECTION            PIC X(02) VALUE SPACES.
           05  WS-BB-SAVE-BILL-PERIOD        PIC 9(06) VALUE 0.
           05  WS-BB-LINE-SEQ                PIC 9(07) COMP-3
                                                          VALUE 0.
           05  WS-BB-FIRST-REC-SW            PIC X(01) VALUE 'Y'.
               88  WS-BB-FIRST-REC            VALUE 'Y'.
           05  WS-BB-TOT-MINUTES             PIC S9(13)V9(02) COMP-3
                                                          VALUE 0.
           05  WS-BB-TOT-AMOUNT              PIC S9(13)V9(05) COMP-3
                                                          VALUE 0.
           05  WS-BB-NEW-GROUP-SW            PIC X(01) VALUE 'N'.
           05  WS-BB-ELEM-FOUND-SW           PIC X(01) VALUE 'N'.
               88  WS-BB-ELEM-FOUND          VALUE 'Y'.
           05  WS-BB-FOUND-NUM               PIC S9(03) COMP-3
                                                          VALUE 0.
           05  WS-BB-SUPPRESS-SW             PIC X(01) VALUE 'N'.
      *****************************************************************
      * CCL ELIGIBILITY DROP WORK - COUNTS RELEASED VS DROPPED SO THE *
      * CONTROL REPORT CAN SHOW HOW MANY WERE FILTERED IN S300.       *
      *****************************************************************
       01  WS-CCL-ELIGIBILITY-WORK.
           05  WS-CE-CHECKED-SW              PIC X(01) VALUE 'N'.
      *****************************************************************
      * DATE-FILTER WORK FOR P4200/P4250 - EFFECTIVE AND EXPIRY DATE  *
      * CENTURY QUALIFICATION.  THIS IS WHERE THE TWO REMAINING       *
      * HARDCODED "70" PIVOT LITERALS LIVE.                           *
      *****************************************************************
       01  WS-DATE-FILTER-WORK.
           05  WS-DF-EFF-YYDDD.
               10  WS-DF-EFF-YY               PIC 9(02).
               10  WS-DF-EFF-DDD              PIC 9(03).
           05  WS-DF-EXP-YYDDD.
               10  WS-DF-EXP-YY               PIC 9(02).
               10  WS-DF-EXP-DDD              PIC 9(03).
           05  WS-DF-EFF-CCYYDDD             PIC 9(07).
           05  WS-DF-EXP-CCYYDDD             PIC 9(07).
           05  WS-DF-DATE-OK-SW              PIC X(01) VALUE 'N'.
               88  WS-DF-DATE-OK             VALUE 'Y'.
           05  WS-DF-SAVE-STATE              PIC X(02).
           05  WS-DF-SAVE-JURIS              PIC X(01).
      *****************************************************************
      * RATE TABLE VALIDATION WORK - P1900 CHECKS THE TABLE JUST      *
      * LOADED FOR ASCENDING KEY ORDER AND EXACT-KEY DUPLICATES.      *
      * THE BINARY SEARCH IN P4100 IS ONLY SAFE IF THE TABLE IS       *
      * TRULY SORTED, SO THIS IS NOT AN OPTIONAL NICETY.              *
      *****************************************************************
       01  WS-TABLE-VALIDATE-WORK.
           05  WS-TV-OUT-OF-SEQ-CNT          PIC S9(05) COMP-3
                                                          VALUE 0.
           05  WS-TV-DUP-CNT                 PIC S9(05) COMP-3
                                                          VALUE 0.
           05  WS-TV-ELEM-CNT-TABLE.
               10  WS-TV-ELEM-ENTRY OCCURS 5 TIMES
                        INDEXED BY WS-TV-X.
                   15  WS-TV-ELEM-CODE        PIC X(06).
                   15  WS-TV-ELEM-ROWS        PIC S9(05) COMP-3
                                                          VALUE 0.
      *****************************************************************
      * PIU (PERCENT INTERSTATE USAGE) JURISDICTION CROSS-CHECK WORK. *
      * WHEN THE LATA CROSS-REFERENCE CANNOT DETERMINE JURISDICTION   *
      * OUTRIGHT, THE CARRIER'S DEFAULT PIU FACTOR (CR-DEFAULT-PIU,   *
      * FROM CABSCARR) IS USED AS A SECONDARY TEST - A STANDARD       *
      * ACCESS-BILLING FALLBACK WHEN NPA-NXX/LATA DATA IS AMBIGUOUS.  *
      *****************************************************************
       01  WS-PIU-CROSS-CHECK-WORK.
           05  WS-PC-PIU-PCT                 PIC S9(03)V9(05) COMP-3.
           05  WS-PC-HIGH-THRESHOLD          PIC S9(03)V9(05) COMP-3
                                                    VALUE 95.00000.
           05  WS-PC-LOW-THRESHOLD           PIC S9(03)V9(05) COMP-3
                                                    VALUE 5.00000.
           05  WS-PC-RESULT-SW               PIC X(01) VALUE ' '.
               88  WS-PC-RESULT-INTERSTATE   VALUE 'I'.
               88  WS-PC-RESULT-INTRASTATE   VALUE 'S'.
               88  WS-PC-RESULT-INDET        VALUE ' '.
      *****************************************************************
      * REPORT EXCEPTION WORK - FALLBACK-LEVEL DISTRIBUTION AND THE   *
      * CCL / SUPPRESSION EXCEPTION DETAIL PRINTED BY P8250 AND P8700.*
      *****************************************************************
       01  WS-REPORT-EXCEPTION-WORK.
           05  WS-RE-FALLBACK-CNT.
               10  WS-RE-FB-EXACT             PIC S9(09) COMP-3
                                                          VALUE 0.
               10  WS-RE-FB-JURIS-GEN         PIC S9(09) COMP-3
                                                          VALUE 0.
               10  WS-RE-FB-TARIFF-DFLT       PIC S9(09) COMP-3
                                                          VALUE 0.
           05  WS-RE-DETAIL-LINE-CNT         PIC S9(05) COMP-3
                                                          VALUE 0.
           05  WS-RE-DETAIL-LINE-MAX         PIC S9(05) COMP-3
                                                    VALUE 50.
           05  WS-RE-TOT-MIN-APPLIED         PIC S9(09) COMP-3
                                                          VALUE 0.
           05  WS-RE-TOT-MAX-APPLIED         PIC S9(09) COMP-3
                                                          VALUE 0.
      *****************************************************************
      * CARRIER TYPE MIX WORK - TALLIES THE OCN SUMMARY TABLE BY      *
      * CR-TYPE (I/C/L/W/R) FOR P8280.  BUILT FROM WS-OT-CARRIER-     *
      * TYPE, WHICH P2460 CAPTURED FROM CARRMST AT FIRST SIGHT OF     *
      * EACH OCN.                                                     *
      *****************************************************************
       01  WS-CARRIER-TYPE-MIX-WORK.
           05  WS-CTM-IXC-CNT                PIC S9(05) COMP-3
                                                          VALUE 0.
           05  WS-CTM-CLEC-CNT               PIC S9(05) COMP-3
                                                          VALUE 0.
           05  WS-CTM-ILEC-CNT               PIC S9(05) COMP-3
                                                          VALUE 0.
           05  WS-CTM-WIRELESS-CNT           PIC S9(05) COMP-3
                                                          VALUE 0.
           05  WS-CTM-RESELLER-CNT           PIC S9(05) COMP-3
                                                          VALUE 0.
           05  WS-CTM-OTHER-CNT              PIC S9(05) COMP-3
                                                          VALUE 0.
      *****************************************************************
      * FIELDS ADDED FOR S650-FEATURE-GROUP AND S680-DATABASE-QUERY.  *
      *****************************************************************
       01  WS-FEATURE-GROUP-RESULT.
           05  WS-FG-CODE                  PIC X(01) VALUE 'A'.
               88  WS-FG-IS-A              VALUE 'A'.
               88  WS-FG-IS-B              VALUE 'B'.
               88  WS-FG-IS-C              VALUE 'C'.
               88  WS-FG-IS-D              VALUE 'D'.
           05  WS-FG-CCL-SUBJECT-SW        PIC X(01) VALUE 'N'.
               88  WS-FG-CCL-SUBJECT       VALUE 'Y'.
           05  WS-FG-PREMIUM-SW            PIC X(01) VALUE 'N'.
               88  WS-FG-IS-PREMIUM        VALUE 'Y'.
           05  WS-FG-TEXT                  PIC X(11).
      * TRUNK GROUP TO FEATURE GROUP XREF - 24 ROWS, SAME EMBEDDED
      * SEED-LITERAL PATTERN AS WS-VH-SEED.  8-BYTE TRUNK GROUP +
      * 1-BYTE FG LETTER = 9 BYTES PER ENTRY.  LOADED BY P1380.
       01  WS-TGFG-XREF-SEED.
           05  FILLER PIC X(216) VALUE
               'EAFGD001DEAFGD002DEAFGD003DEAFGD004DEAFGD0
      -        '05DEAFGD006DEAFGD007DEAFGD008DTSFGB001BTSF
      -        'GB002BTSFGB003BTSFGB004BTSFGB005BTSFGB006B
      -        'LSFGA001ALSFGA002ALSFGA003ALSFGA004ALSFGA0
      -        '05ALSFGA006AOSFGC001COSFGC002COSFGC003COSF
      -        'GC004C'.
       01  WS-TGFG-XREF-SEED-R REDEFINES WS-TGFG-XREF-SEED.
           05  WS-TGFG-SEED-ENTRY OCCURS 24 TIMES.
               10  WS-TGFG-SEED-TRUNK      PIC X(08).
               10  WS-TGFG-SEED-FG         PIC X(01).
       01  WS-TGFG-XREF-TABLE.
           05  WS-TGFG-ENTRY OCCURS 24 TIMES.
               10  WS-TGFG-TRUNK-GRP       PIC X(08).
               10  WS-TGFG-FG-CODE         PIC X(01).
       01  WS-TGFG-WORK.
           05  WS-TGFG-X                   PIC S9(02) COMP-3
                                                        VALUE 0.
           05  WS-TGFG-FOUND-SW            PIC X(01) VALUE 'N'.
               88  WS-TGFG-FOUND           VALUE 'Y'.
      * SEVEN TOLL-FREE NPAS - LOADED BY P1385.
       01  WS-8YY-NPA-TABLE.
           05  WS-8YY-NPA-ENTRY OCCURS 7 TIMES INDEXED BY WS-8YY-NX.
               10  WS-8YY-NPA-VALUE        PIC 9(03).
      * NPA EXTRACTION (VIA REDEFINES, NOT REFERENCE MODIFICATION),
      * THIRD-PARTY-DATABASE TEST, AND THE QUERY CHARGE ITSELF.
       01  WS-8YY-WORK.
           05  WS-8YY-TERM-NPANXX          PIC 9(06).
           05  WS-8YY-TERM-NPANXX-R REDEFINES WS-8YY-TERM-NPANXX.
               10  WS-8YY-TERM-NPA         PIC 9(03).
               10  WS-8YY-TERM-NXX         PIC 9(03).
           05  WS-8YY-CALL-SW              PIC X(01) VALUE 'N'.
               88  WS-8YY-IS-TOLLFREE      VALUE 'Y'.
           05  WS-8YY-THIRD-PARTY-SW       PIC X(01) VALUE 'N'.
               88  WS-8YY-THIRD-PARTY-DB   VALUE 'Y'.
           05  WS-8YY-QUERY-AMOUNT         PIC S9(09)V9(05) COMP-3.
           05  WS-8YY-SURCHARGE-AMOUNT     PIC S9(09)V9(05) COMP-3.
           05  WS-8YY-DISCOUNT-PCT         PIC S9(03)V9(05) COMP-3.
           05  WS-8YY-DESC-FRAG1           PIC X(15).
           05  WS-8YY-DESC-FRAG2           PIC X(08).
      * PER-OCN 8YY QUERY COUNT FOR THE RUN - FIND-OR-ADD, SAME
      * SHAPE AS WS-OCN-TOTALS (P2460).  USED BY P6850.
       01  WS-8YY-OCN-QUERY-TABLE.
           05  WS-8YQ-CNT                  PIC 9(02) VALUE 0.
           05  WS-8YQ-ENTRY OCCURS 1 TO 40 TIMES
                    DEPENDING ON WS-8YQ-CNT
                    INDEXED BY WS-8YQ-X.
               10  WS-8YQ-OCN              PIC X(04).
               10  WS-8YQ-QUERY-CNT        PIC S9(09) COMP-3
                                                        VALUE 0.
           05  WS-8YQ-FOUND-SW             PIC X(01) VALUE 'N'.
               88  WS-8YQ-FOUND            VALUE 'Y'.
           05  WS-8YQ-FOUND-NUM            PIC S9(03) COMP-3
                                                        VALUE 0.
      * QUERY VOLUME DISCOUNT BANDS - LOADED BY P1390, WALKED BY
      * P6850 THE SAME WAY P6410 WALKS THE MILEAGE BAND POOL.
       01  WS-8YY-DISC-BAND-TABLE.
           05  WS-8YB-ENTRY OCCURS 4 TIMES INDEXED BY WS-8YB-X.
               10  WS-8YB-FROM-QTY         PIC S9(09) COMP-3.
               10  WS-8YB-THRU-QTY         PIC S9(09) COMP-3.
               10  WS-8YB-DISC-PCT         PIC S9(03)V9(05) COMP-3.
       PROCEDURE DIVISION.
      *****************************************************************
      * P0000-MAINLINE - MANDATORY CABS BATCH SHAPE.  P2000-PROCESS   *
      * IS A SINGLE SORT VERB, SO THE UNTIL WS-EOF LOOP RUNS EXACTLY  *
      * ONCE - THE SORT'S INPUT PROCEDURE DOES THE REAL RECORD LOOP.  *
      *****************************************************************
       P0000-MAINLINE.
           PERFORM P1000-INIT THRU P1000-EXIT.
           PERFORM P2000-PROCESS THRU P2000-EXIT UNTIL WS-EOF.
           PERFORM P8000-CONTROL THRU P8000-EXIT.
           PERFORM P9000-TERM THRU P9000-EXIT.
           STOP RUN.
      *****************************************************************
      * S100-INITIALISATION SECTION                                   *
      *****************************************************************
       S100-INITIALISATION SECTION.
       P1000-INIT.
           PERFORM P1100-OPEN-FILES THRU P1100-EXIT.
           PERFORM P1200-READ-PARM THRU P1200-EXIT.
           PERFORM P1300-LOAD-RATE-TABLE THRU P1300-EXIT.
           PERFORM P1900-VALIDATE-RATE-TABLE THRU P1900-EXIT.
           PERFORM P1400-LOAD-VH-TABLE THRU P1400-EXIT.
           PERFORM P1500-LOAD-LATA-XREF THRU P1500-EXIT.
           PERFORM P1600-ESTABLISH-EFF-VIEW THRU P1600-EXIT.
           PERFORM P1650-VALIDATE-PARM THRU P1650-EXIT.
           PERFORM P1700-INIT-COUNTERS THRU P1700-EXIT.
           PERFORM P1750-BUILD-ELEMENT-TABLE THRU P1750-EXIT.
           PERFORM P1380-LOAD-TRUNK-FG-XREF THRU P1380-EXIT.
           PERFORM P1385-LOAD-8YY-NPA-TABLE THRU P1385-EXIT.
           PERFORM P1390-LOAD-8YY-DISC-BANDS THRU P1390-EXIT.
       P1000-EXIT.
           EXIT.
      *****************************************************************
      * P1100-OPEN-FILES - OPEN ALL FIVE PERMANENT FILES.  RATIN,     *
      * RATEMST AND CARRMST ARE CHECKED INDIVIDUALLY BECAUSE A BAD    *
      * OPEN ON ANY OF THE THREE MEANS THE RUN CANNOT RATE ANYTHING.  *
      *****************************************************************
       P1100-OPEN-FILES.
           OPEN INPUT RATIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RATIN OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN INPUT RATEMST.
           IF WS-FS-TABLE NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RATEMST OPEN FAILED - VSAM STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN INPUT CARRMST.
           IF WS-FS-TABLE NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CARRMST OPEN FAILED - VSAM STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RATOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RATOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT BDTLOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BDTLOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SUSOUT.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'SUSOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CTLOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RPTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RPTOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           MOVE 'Y' TO WS-IS-OPEN-OK-SW.
       P1100-EXIT.
           EXIT.
      *****************************************************************
      * P1200-READ-PARM - READS THE SYSIN CARD.  A LEADING 'K' MEANS  *
      * THE FREE-FORM KEYWORD LAYOUT (R2), ANYTHING ELSE MEANS THE    *
      * FIXED POSITIONAL LAYOUT (R1) USED SINCE V1.00.                *
      *****************************************************************
       P1200-READ-PARM.
           MOVE SPACES TO WS-PARM-CARD.
           ACCEPT WS-PARM-CARD FROM SYSIN.
           CALL 'CABPARMR' USING WS-PARM-CARD WS-RC-PARMR.
           IF PC2-KEYWORD-SW = 'K'
               PERFORM P1210-PARSE-KEYWORD-CARD THRU P1210-EXIT
           ELSE
               PERFORM P1220-PARSE-POSITIONAL-CARD THRU P1220-EXIT.
           MOVE R1-CYCLE-YYDDD TO WS-EV-CYCLE-YYDDD.
       P1200-EXIT.
           EXIT.
       P1210-PARSE-KEYWORD-CARD.
           IF PC2-KEYWORD = 'CYCLE'
               MOVE PC2-VALUE-CYCLE TO R1-CYCLE-YYDDD.
           MOVE 'FCC1' TO R1-TARIFF-CD.
           MOVE 'P' TO R1-MODE-SW.
           MOVE 'Y' TO R1-TANDEM-SW.
           MOVE 'CABRAT03-KW' TO R1-RUN-ID.
       P1210-EXIT.
           EXIT.
       P1220-PARSE-POSITIONAL-CARD.
           MOVE PC1-CYCLE-YYDDD TO R1-CYCLE-YYDDD.
           MOVE PC1-BILL-PERIOD TO R1-BILL-PERIOD.
           MOVE PC1-TARIFF-CD TO R1-TARIFF-CD.
           MOVE PC1-MODE-SW TO R1-MODE-SW.
           MOVE PC1-TANDEM-SW TO R1-TANDEM-SW.
           MOVE PC1-RUN-ID TO R1-RUN-ID.
           IF R1-TARIFF-CD = SPACES
               MOVE 'FCC1' TO R1-TARIFF-CD.
           MOVE PC1-RESTART-KEY TO WS-RESTART-KEY-SAVE.
           IF WS-RESTART-KEY-SAVE NOT = SPACES
               MOVE 'Y' TO WS-RESTART-SW.
       P1220-EXIT.
           EXIT.
      *****************************************************************
      * P1300-LOAD-RATE-TABLE - KEYED BROWSE OF THE ENTIRE RATEMST    *
      * VSAM KSDS INTO R2-RATE-TABLE, WITH EACH ROW'S BAND SET        *
      * FLATTENED INTO R3-BAND-POOL AT THE SAME TIME.                 *
      *****************************************************************
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
           MOVE R2-ENTRY-CNT TO WS-TL-RATE-ROWS-LOADED.
           MOVE R3-POOL-CNT TO WS-TL-BAND-ROWS-LOADED.
       P1300-EXIT.
           EXIT.
      *****************************************************************
      * P1900-VALIDATE-RATE-TABLE - CONFIRMS THE TABLE JUST LOADED IS *
      * TRULY IN ASCENDING KEY SEQUENCE (P4100'S BINARY SEARCH SILENT *
      * MISBEHAVES IF IT IS NOT) AND COUNTS EXACT-KEY DUPLICATE ROWS. *
      * ALSO BUILDS THE PER-ELEMENT ROW-COUNT TABLE PRINTED ON THE    *
      * CONTROL REPORT HEADER BY P8100.  NEITHER CONDITION ABENDS -   *
      * BOTH ARE LOGGED AS WARNINGS AND THE RUN CONTINUES, TRUSTING   *
      * THE VSAM KEYED BROWSE TO HAVE DELIVERED SORTED DATA.          *
      *****************************************************************
       P1900-VALIDATE-RATE-TABLE.
           MOVE 0 TO WS-TV-OUT-OF-SEQ-CNT.
           MOVE 0 TO WS-TV-DUP-CNT.
           PERFORM P1910-INIT-ELEM-CNT-TABLE THRU P1910-EXIT
               VARYING WS-TV-X FROM 1 BY 1
               UNTIL WS-TV-X > 5.
           IF R2-ENTRY-CNT > 0
               PERFORM P1920-VALIDATE-ONE-ROW THRU P1920-EXIT
                   VARYING R2-EX FROM 1 BY 1
                   UNTIL R2-EX > R2-ENTRY-CNT.
           IF WS-TV-OUT-OF-SEQ-CNT > 0
               DISPLAY 'CABRAT03 WARNING - RATE TABLE OUT OF SEQUENCE '
                   WS-TV-OUT-OF-SEQ-CNT ' TIMES'.
           IF WS-TV-DUP-CNT > 0
               DISPLAY 'CABRAT03 WARNING - RATE TABLE HAS '
                   WS-TV-DUP-CNT ' EXACT-KEY DUPLICATE ROWS'.
       P1900-EXIT.
           EXIT.
       P1910-INIT-ELEM-CNT-TABLE.
           MOVE SPACES TO WS-TV-ELEM-CODE (WS-TV-X).
           MOVE 0 TO WS-TV-ELEM-ROWS (WS-TV-X).
       P1910-EXIT.
           EXIT.
      *****************************************************************
      * P1920-VALIDATE-ONE-ROW - COMPARES THIS ROW'S FULL KEY TO THE  *
      * PRECEDING ROW'S USING RELATIVE INDEXING (R2-EX - 1) RATHER    *
      * THAN A SEPARATE SAVED-KEY FIELD.                              *
      *****************************************************************
       P1920-VALIDATE-ONE-ROW.
           IF R2-EX > 1 AND R2-EN-KEY (R2-EX) < R2-EN-KEY (R2-EX - 1)
               ADD 1 TO WS-TV-OUT-OF-SEQ-CNT.
           IF R2-EX > 1 AND R2-EN-KEY (R2-EX) = R2-EN-KEY (R2-EX - 1)
               ADD 1 TO WS-TV-DUP-CNT.
           PERFORM P1930-TALLY-ELEMENT THRU P1930-EXIT.
       P1920-EXIT.
           EXIT.
      *****************************************************************
      * P1930-TALLY-ELEMENT - FIND-OR-ADD BY ELEMENT CODE INTO THE    *
      * 5-ROW COUNT TABLE.                                            *
      *****************************************************************
       P1930-TALLY-ELEMENT.
           MOVE 'N' TO WS-CE-CHECKED-SW.
           PERFORM P1935-SEARCH-ELEM-SLOT THRU P1935-EXIT
               VARYING WS-TV-X FROM 1 BY 1
               UNTIL WS-TV-X > 5 OR WS-CE-CHECKED-SW = 'Y'.
       P1930-EXIT.
           EXIT.
       P1935-SEARCH-ELEM-SLOT.
           IF WS-TV-ELEM-CODE (WS-TV-X) = R2-EN-ELEM (R2-EX)
               ADD 1 TO WS-TV-ELEM-ROWS (WS-TV-X)
               MOVE 'Y' TO WS-CE-CHECKED-SW.
           IF WS-TV-ELEM-CODE (WS-TV-X) = SPACES AND
                   WS-CE-CHECKED-SW = 'N'
               MOVE R2-EN-ELEM (R2-EX) TO WS-TV-ELEM-CODE (WS-TV-X)
               ADD 1 TO WS-TV-ELEM-ROWS (WS-TV-X)
               MOVE 'Y' TO WS-CE-CHECKED-SW.
       P1935-EXIT.
           EXIT.
       P1310-READ-NEXT-RATE.
           IF NOT WS-VB-EOF
               READ RATEMST NEXT RECORD
                   AT END MOVE 'Y' TO WS-VB-EOF-SW.
       P1310-EXIT.
           EXIT.
      *****************************************************************
      * P1320-LOAD-RATE-ROW - ONE ROW FROM VSAM INTO R2-ENTRY, THEN   *
      * ITS BANDS INTO THE POOL, THEN READS THE NEXT ROW.  THE TABLE  *
      * IS ALREADY IN ASCENDING KEY SEQUENCE BECAUSE IT CAME OFF A    *
      * KEYED BROWSE - THAT ORDERING IS WHAT MAKES THE P4100 BINARY   *
      * SEARCH VALID LATER ON.                                        *
      *****************************************************************
       P1320-LOAD-RATE-ROW.
           IF R2-ENTRY-CNT NOT < 600
               MOVE 'Y' TO R2-TABLE-FULL-SW
               MOVE 'Y' TO WS-VB-EOF-SW
               MOVE 'N' TO WS-TL-DISC-SAVE
           ELSE
               ADD 1 TO R2-ENTRY-CNT
               SET R2-EX TO R2-ENTRY-CNT
               MOVE RT-TARIFF-CD TO R2-EN-TARIFF (R2-EX)
               MOVE RT-RATE-ELEM TO R2-EN-ELEM (R2-EX)
               MOVE RT-JURIS-CD TO R2-EN-JURIS (R2-EX)
               MOVE RT-STATE-CD TO R2-EN-STATE (R2-EX)
               MOVE RT-EFF-YYDDD TO R2-EN-EFF-YYDDD (R2-EX)
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
               MOVE SPACES TO R2-EN-MODULE-SFX (R2-EX)
               MOVE RT-DISC-ELIGIBLE TO WS-TL-DISC-SAVE
               PERFORM P1350-DERIVE-MODULE-SFX THRU P1350-EXIT
               PERFORM P1330-LOAD-BAND-ROWS THRU P1330-EXIT
               PERFORM P1310-READ-NEXT-RATE THRU P1310-EXIT.
      *****************************************************************
      * RT-DISC-ELIGIBLE IS READ AND TALLIED FOR THE TABLE LOAD        *
      * SUMMARY (P8150) BUT IS NOT CARRIED INTO R2-ENTRY - THE IN-     *
      * MEMORY TABLE LAYOUT (CABSRT02) WAS FROZEN BEFORE THE DISCOUNT  *
      * ELIGIBILITY FLAG WAS ADDED TO THE VSAM RECORD, SO THERE IS NO  *
      * R2-EN-DISC-ELIGIBLE FIELD FOR IT TO GO INTO.  NO RATING        *
      * PARAGRAPH IN S500 CAN SEE THIS FLAG - IT IS COUNT-ONLY.  THE   *
      * FLAG HAS TO BE SAVED TO WS-TL-DISC-SAVE BEFORE P1310-READ-     *
      * NEXT-RATE RUNS ABOVE, BECAUSE THAT READ OVERLAYS RT-DISC-      *
      * ELIGIBLE WITH THE NEXT ROW'S VALUE BEFORE THIS TEST WOULD      *
      * OTHERWISE SEE IT.                                              *
      *****************************************************************
           IF WS-TL-DISC-SAVE = 'Y'
               ADD 1 TO WS-TL-DISC-ELIGIBLE-CNT.
       P1320-EXIT.
           EXIT.
      *****************************************************************
      * P1330-LOAD-BAND-ROWS - FLATTENS THIS RATE ROW'S MILEAGE OR    *
      * PERCENTAGE BANDS INTO R3-BAND-POOL, RECORDING THE STARTING    *
      * OFFSET ON THE PARENT ROW SO P6400 CAN WALK JUST ITS SLICE.    *
      *****************************************************************
       P1330-LOAD-BAND-ROWS.
           ADD 1 TO R3-POOL-CNT.
           MOVE R3-POOL-CNT TO R2-EN-BAND-OFFSET (R2-EX).
           SUBTRACT 1 FROM R3-POOL-CNT.
           IF RT-BAND-CNT > 0
               PERFORM P1340-LOAD-ONE-BAND THRU P1340-EXIT
                   VARYING RT-BX FROM 1 BY 1
                   UNTIL RT-BX > RT-BAND-CNT.
       P1330-EXIT.
           EXIT.
       P1340-LOAD-ONE-BAND.
           IF R3-POOL-CNT < 2400
               ADD 1 TO R3-POOL-CNT
               SET R3-PX TO R3-POOL-CNT
               MOVE RT-BAND-FROM (RT-BX) TO R3-PL-FROM (R3-PX)
               MOVE RT-BAND-THRU (RT-BX) TO R3-PL-THRU (R3-PX)
               MOVE RT-BAND-RATE (RT-BX) TO R3-PL-RATE (R3-PX)
               MOVE RT-BAND-PCT (RT-BX) TO R3-PL-PCT (R3-PX).
       P1340-EXIT.
           EXIT.
      *****************************************************************
      * P1350-DERIVE-MODULE-SFX - THE RATEMST RECORD ITSELF CARRIES   *
      * NO MODULE-OVERRIDE FIELD, SO THE SUFFIX IS SYNTHESISED HERE   *
      * FROM THE KEY.  TODAY THE ONLY LIVE OVERRIDE IS NEW YORK       *
      * INTRASTATE TANDEM, WHICH THE PSC ORDER FROM 2003 REQUIRES TO  *
      * BE RATED BY A SEPARATE MODULE (CABRATTS) RATHER THAN INLINE.  *
      *****************************************************************
       P1350-DERIVE-MODULE-SFX.
           IF RT-RATE-ELEM = 'TANSW ' AND RT-JURIS-CD = 'S'
                   AND RT-STATE-CD = 'NY'
               MOVE 'TS' TO R2-EN-MODULE-SFX (R2-EX).
       P1350-EXIT.
           EXIT.
      *****************************************************************
      * P1400-LOAD-VH-TABLE - COPIES THE EMBEDDED V AND H LITERAL     *
      * SEED INTO THE WORKING TABLE.  A REAL LATA SPLIT WOULD BE      *
      * HANDLED BY EXTENDING THE SEED, NOT BY ADDING A NEW DD.        *
      *****************************************************************
       P1400-LOAD-VH-TABLE.
           MOVE 0 TO WS-VH-CNT.
           PERFORM P1410-LOAD-ONE-VH THRU P1410-EXIT
               VARYING WS-VH-SUB FROM 1 BY 1
               UNTIL WS-VH-SUB > 70.
           MOVE WS-VH-CNT TO WS-TL-VH-ROWS-LOADED.
       P1400-EXIT.
           EXIT.
       P1410-LOAD-ONE-VH.
           ADD 1 TO WS-VH-CNT.
           SET WS-VH-X TO WS-VH-CNT.
           MOVE WS-VHS-LATA (WS-VH-SUB) TO WS-VH-LATA (WS-VH-X).
           MOVE WS-VHS-V (WS-VH-SUB) TO WS-VH-VCOORD (WS-VH-X).
           MOVE WS-VHS-H (WS-VH-SUB) TO WS-VH-HCOORD (WS-VH-X).
       P1410-EXIT.
           EXIT.
      *****************************************************************
      * P1500-LOAD-LATA-XREF - COPIES THE EMBEDDED STATE/LATA SEED.   *
      *****************************************************************
       P1500-LOAD-LATA-XREF.
           MOVE 0 TO WS-LX-CNT.
           PERFORM P1510-LOAD-ONE-LATA THRU P1510-EXIT
               VARYING WS-LX-SUB FROM 1 BY 1
               UNTIL WS-LX-SUB > 60.
           MOVE WS-LX-CNT TO WS-TL-LATA-ROWS-LOADED.
       P1500-EXIT.
           EXIT.
       P1510-LOAD-ONE-LATA.
           ADD 1 TO WS-LX-CNT.
           SET WS-LX-X TO WS-LX-CNT.
           MOVE WS-LTS-STATE (WS-LX-SUB) TO WS-LX-STATE (WS-LX-X).
           MOVE WS-LTS-LATA (WS-LX-SUB) TO WS-LX-LATA (WS-LX-X).
           MOVE WS-LTS-JURIS-DFLT (WS-LX-SUB) TO
               WS-LX-JURIS-DFLT (WS-LX-X).
       P1510-EXIT.
           EXIT.
      *****************************************************************
      * P1600-ESTABLISH-EFF-VIEW - CONVERTS THE CYCLE YYDDD FROM THE  *
      * PARM CARD INTO A CENTURY-QUALIFIED FORM USING THE STANDARD    *
      * DW-PIVOT-YY FROM CABSDATE (THE CORRECT, MAINTAINED PATTERN -  *
      * CONTRAST WITH P4200 LATER IN THIS PROGRAM).                   *
      *****************************************************************
       P1600-ESTABLISH-EFF-VIEW.
           MOVE WS-EV-CYCLE-YYDDD TO DW-CURRENT-YYDDD.
           IF DW-CUR-YY < DW-PIVOT-YY
               MOVE 20 TO WS-EV-CENTURY
           ELSE
               MOVE 19 TO WS-EV-CENTURY.
           COMPUTE DW-CENTURY-WORK = WS-EV-CENTURY * 100 + DW-CUR-YY.
           COMPUTE WS-EV-CYCLE-CCYYDDD =
               (DW-CENTURY-WORK * 1000) + DW-CUR-DDD.
           PERFORM P1610-CONVERT-CYCLE-GREG THRU P1610-EXIT.
       P1600-EXIT.
           EXIT.
      *****************************************************************
      * P1610-CONVERT-CYCLE-GREG - CALLS THE SHARED ESTATE UTILITY     *
      * CABDTCNV TO TURN THE SEVEN-DIGIT CCYYDDD CYCLE DATE INTO A     *
      * GREGORIAN CCYY/MM/DD FOR DISPLAY ON THE REPORT HEADER (SEE     *
      * P8100).  CABDTCNV IS THE SAME JULIAN-TO-GREGORIAN UTILITY      *
      * USED BY EVERY OTHER PROGRAM IN THE ESTATE THAT HAS TO PRINT A  *
      * DATE FOR A HUMAN READER - THIS PROGRAM COULD HAVE DONE THE     *
      * MATH INLINE (THE WAY P6xxx DOES FOR MILEAGE), BUT DATE MATH    *
      * IS CENTRALISED PER CABS-STD-058 SO A FUTURE CALENDAR CORRECTION*
      * ONLY HAS TO BE MADE ONCE.  IF THE CALL TARGET IS MISSING FROM  *
      * THE LOAD LIBRARY, ON EXCEPTION LEAVES DW-GREG-DATE AT ZERO AND *
      * THE REPORT HEADER PRINTS THE RAW CCYYDDD INSTEAD.              *
      *****************************************************************
       P1610-CONVERT-CYCLE-GREG.
           MOVE 0 TO DW-GREG-DATE.
           CALL 'CABDTCNV' USING WS-EV-CYCLE-CCYYDDD DW-GREG-DATE
               ON EXCEPTION
                   MOVE 9999 TO WS-RC-DTCNV
               NOT ON EXCEPTION
                   MOVE 0 TO WS-RC-DTCNV.
       P1610-EXIT.
           EXIT.
      *****************************************************************
      * P1650-VALIDATE-PARM - SANITY CHECKS ON THE PARM CARD AFTER    *
      * P1200/P1210/P1220 HAVE PARSED IT.  ANY FAILURE HERE IS FATAL  *
      * - THERE IS NO SENSIBLE DEFAULT FOR A MISSING CYCLE DATE OR    *
      * TARIFF CODE, SO THE RUN CANNOT PROCEED.  REUSES P9900-FATAL-  *
      * OPEN AS A GENERIC FATAL-ABEND HELPER - THE NAME IS A LEFTOVER *
      * FROM WHEN IT ONLY HANDLED OPEN FAILURES.                      *
      *****************************************************************
       P1650-VALIDATE-PARM.
           IF R1-CYCLE-YYDDD = 0
               MOVE 'P1650-VALIDATE-PARM' TO WS-AB-PARA
               MOVE 'PARM CARD CYCLE DATE IS ZERO' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           IF R1-TARIFF-CD = SPACES
               MOVE 'P1650-VALIDATE-PARM' TO WS-AB-PARA
               MOVE 'PARM CARD TARIFF CODE IS BLANK' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           IF NOT R1-PRODUCTION AND NOT R1-PARALLEL AND
                   NOT R1-SIMULATION
               MOVE 'P1650-VALIDATE-PARM' TO WS-AB-PARA
               MOVE 'PARM CARD MODE SWITCH IS NOT P, L OR S' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
       P1650-EXIT.
           EXIT.
      *****************************************************************
      * P1700-INIT-COUNTERS - ZEROISE THE STANDARD AND LOCAL COUNTERS.*
      *****************************************************************
       P1700-INIT-COUNTERS.
           MOVE 0 TO WS-READ-CNT WS-WRITE-CNT WS-REJECT-CNT.
           MOVE 0 TO WS-SUMM-CNT WS-CFWD-CNT.
           MOVE 0 TO WS-ACC-MINUTES WS-ACC-AMOUNT.
           MOVE 0 TO WS-ACC-SEQ-HASH WS-ACC-OCN-HASH.
           MOVE 0 TO WS-HW-MINUTES WS-HW-AMOUNT WS-HW-SEQ WS-HW-OCN.
           MOVE 0 TO WS-MC-ELEMENTS-RATED WS-MC-ELEMENTS-DROPPED-CCL.
           MOVE 0 TO WS-MC-ELEMENTS-SUPPRESSED WS-MC-SORT-RELEASED.
           MOVE 0 TO WS-MC-SORT-RETURNED WS-MC-BANS-WRITTEN.
           MOVE 0 TO WS-MC-DYNAMIC-CALLS WS-MC-RATE-NOT-FOUND-SOFT.
           MOVE 0 TO WS-MC-BD-TABLE-FULL.
           MOVE 0 TO WS-MC-VH-EXACT-CNT.
           MOVE 0 TO WS-MC-VH-FALLBACK-CNT.
           MOVE 0 TO WS-MC-ZERO-DURATION-CNT.
           MOVE 0 TO WS-MC-8YY-QUERIES-RATED WS-MC-8YY-THIRD-PARTY-CNT.
           MOVE 0 TO WS-RPT-PAGE-NBR.
           MOVE 99 TO WS-RPT-LINE-NBR.
       P1700-EXIT.
           EXIT.
      *****************************************************************
      * P1750-BUILD-ELEMENT-TABLE - THE FIVE RATE ELEMENTS THIS       *
      * PROGRAM RATES, IN THE ORDER THEY ARE OFFERED TO EACH CDR.     *
      *****************************************************************
       P1750-BUILD-ELEMENT-TABLE.
           SET WS-EC-X TO 1.
           MOVE WS-ELEM-ORIGAC TO WS-EC-ELEM-CODE (WS-EC-X).
           MOVE 'ORIGINATING ACC' TO WS-EC-ELEM-NAME (WS-EC-X).
           MOVE 'N' TO WS-EC-COND-TANDEM-ONLY (WS-EC-X).
           MOVE 'N' TO WS-EC-COND-INTERSTATE (WS-EC-X).
           SET WS-EC-X TO 2.
           MOVE WS-ELEM-TERMAC TO WS-EC-ELEM-CODE (WS-EC-X).
           MOVE 'TERMINATING ACC' TO WS-EC-ELEM-NAME (WS-EC-X).
           MOVE 'N' TO WS-EC-COND-TANDEM-ONLY (WS-EC-X).
           MOVE 'N' TO WS-EC-COND-INTERSTATE (WS-EC-X).
           SET WS-EC-X TO 3.
           MOVE WS-ELEM-LTRANS TO WS-EC-ELEM-CODE (WS-EC-X).
           MOVE 'LOCAL TRANSPORT' TO WS-EC-ELEM-NAME (WS-EC-X).
           MOVE 'N' TO WS-EC-COND-TANDEM-ONLY (WS-EC-X).
           MOVE 'N' TO WS-EC-COND-INTERSTATE (WS-EC-X).
           SET WS-EC-X TO 4.
           MOVE WS-ELEM-TANSW TO WS-EC-ELEM-CODE (WS-EC-X).
           MOVE 'TANDEM SWITCHING' TO WS-EC-ELEM-NAME (WS-EC-X).
           MOVE 'Y' TO WS-EC-COND-TANDEM-ONLY (WS-EC-X).
           MOVE 'N' TO WS-EC-COND-INTERSTATE (WS-EC-X).
           SET WS-EC-X TO 5.
           MOVE WS-ELEM-CCLINE TO WS-EC-ELEM-CODE (WS-EC-X).
           MOVE 'CARRIER COM LINE' TO WS-EC-ELEM-NAME (WS-EC-X).
           MOVE 'N' TO WS-EC-COND-TANDEM-ONLY (WS-EC-X).
           MOVE 'Y' TO WS-EC-COND-INTERSTATE (WS-EC-X).
           SET WS-EC-X TO 1.
       P1750-EXIT.
           EXIT.
      * P1380-LOAD-TRUNK-FG-XREF - COPIES THE 24-ROW SEED TABLE INTO
      * THE WORKING XREF, SAME PATTERN AS P1410 AND P1510 ABOVE.
       P1380-LOAD-TRUNK-FG-XREF.
           PERFORM P1382-LOAD-ONE-TGFG THRU P1382-EXIT
               VARYING WS-TGFG-X FROM 1 BY 1
               UNTIL WS-TGFG-X > 24.
       P1380-EXIT.
           EXIT.
       P1382-LOAD-ONE-TGFG.
           MOVE WS-TGFG-SEED-TRUNK (WS-TGFG-X) TO
               WS-TGFG-TRUNK-GRP (WS-TGFG-X).
           MOVE WS-TGFG-SEED-FG (WS-TGFG-X) TO
               WS-TGFG-FG-CODE (WS-TGFG-X).
       P1382-EXIT.
           EXIT.
      * THE SEVEN TOLL-FREE NPAS ASSIGNED AS OF THE LAST 8YY CHANGE.
       P1385-LOAD-8YY-NPA-TABLE.
           MOVE 800 TO WS-8YY-NPA-VALUE (1).
           MOVE 888 TO WS-8YY-NPA-VALUE (2).
           MOVE 877 TO WS-8YY-NPA-VALUE (3).
           MOVE 866 TO WS-8YY-NPA-VALUE (4).
           MOVE 855 TO WS-8YY-NPA-VALUE (5).
           MOVE 844 TO WS-8YY-NPA-VALUE (6).
           MOVE 833 TO WS-8YY-NPA-VALUE (7).
       P1385-EXIT.
           EXIT.
      * QUERY VOLUME DISCOUNT BANDS - APPLIED BY P6850.
       P1390-LOAD-8YY-DISC-BANDS.
           MOVE 0 TO WS-8YB-FROM-QTY (1).
           MOVE 999 TO WS-8YB-THRU-QTY (1).
           MOVE 0 TO WS-8YB-DISC-PCT (1).
           MOVE 1000 TO WS-8YB-FROM-QTY (2).
           MOVE 4999 TO WS-8YB-THRU-QTY (2).
           MOVE 5.00000 TO WS-8YB-DISC-PCT (2).
           MOVE 5000 TO WS-8YB-FROM-QTY (3).
           MOVE 19999 TO WS-8YB-THRU-QTY (3).
           MOVE 10.00000 TO WS-8YB-DISC-PCT (3).
           MOVE 20000 TO WS-8YB-FROM-QTY (4).
           MOVE 999999999 TO WS-8YB-THRU-QTY (4).
           MOVE 15.00000 TO WS-8YB-DISC-PCT (4).
       P1390-EXIT.
           EXIT.
      *****************************************************************
      * P9900-FATAL-OPEN - ORDINARY (NOT HIDDEN) OPEN-FAILURE ABEND.  *
      *****************************************************************
       P9900-FATAL-OPEN.
           MOVE WS-PGM-NAME TO WS-AB-PGM.
           MOVE 9901 TO WS-AB-USER-CODE.
           DISPLAY 'CABRAT03 FATAL OPEN - ' WS-AB-REASON.
           CALL 'CABABEND' USING WS-AB-PGM WS-AB-PARA WS-AB-REASON
               WS-AB-USER-CODE.
           STOP RUN.
       P9900-EXIT.
           EXIT.
      *****************************************************************
      * S200-MAIN-PROCESS SECTION - READ, VALIDATE, JURISDICTION      *
      * DISPATCH, PER-ELEMENT RATING LOOP.  PERFORMED FROM S300 SORT  *
      * INPUT PROCEDURE, NOT FROM P2000-PROCESS DIRECTLY.             *
      *****************************************************************
       S200-MAIN-PROCESS SECTION.
      *****************************************************************
      * P2000-PROCESS - THE MANDATORY MAINLINE STEP.  THE ENTIRE      *
      * READ / RATE / RELEASE / RETURN / WRITE CYCLE HAPPENS INSIDE   *
      * THE SORT'S INPUT AND OUTPUT PROCEDURES, SO THIS PARAGRAPH IS  *
      * JUST THE SORT VERB ITSELF.  IT RUNS ONCE - P2000-EXIT SETS    *
      * WS-EOF SO THE MAINLINE'S UNTIL WS-EOF LOOP DOES NOT REPEAT.   *
      *****************************************************************
       P2000-PROCESS.
           SORT SORTWK
               ON ASCENDING KEY SR-OCN SR-BAN SR-JURIS-CD SR-RATE-ELEM
               INPUT PROCEDURE IS S300-SORT-INPUT
               OUTPUT PROCEDURE IS S350-SORT-OUTPUT.
           MOVE 'Y' TO WS-EOF-SW.
       P2000-EXIT.
           EXIT.
       P2100-READ-RATIN.
           READ RATIN
               AT END MOVE 'Y' TO WS-SORT-INPUT-EOF-SW.
           IF NOT WS-SORT-INPUT-EOF
               ADD 1 TO WS-READ-CNT
               PERFORM P2105-CHECKPOINT-DISPLAY THRU P2105-EXIT.
       P2100-EXIT.
           EXIT.
      *****************************************************************
      * P2105-CHECKPOINT-DISPLAY - OPERATIONAL PROGRESS MARKER EVERY  *
      * 25,000 RECORDS.  HELPS AN OPERATOR WATCHING SDSF TELL A SLOW  *
      * RUN FROM A HUNG ONE ON A MULTI-MILLION RECORD CYCLE.          *
      *****************************************************************
       P2105-CHECKPOINT-DISPLAY.
           DIVIDE WS-READ-CNT BY 25000 GIVING WS-MC-CHECKPOINT-QUOT
               REMAINDER WS-MC-CHECKPOINT-REM.
           IF WS-MC-CHECKPOINT-REM = 0
               DISPLAY 'CABRAT03 - ' WS-READ-CNT ' RECORDS READ'.
       P2105-EXIT.
           EXIT.
      *****************************************************************
      * P2202-VALIDATE-REC-TYPE - THIS PROGRAM RATES VOICE MOU        *
      * RECORDS ONLY (CD-VOICE-MOU), SINCE EVERY S500 PARAGRAPH        *
      * REFERENCES CD-VOICE-DETAIL, ONE OF THREE REDEFINITIONS OF THE  *
      * SAME 96-BYTE CD-VARIANT-AREA.  IF A DATA-SERVICE, SPECIAL-     *
      * ACCESS, UNBUNDLED OR RECIPROCAL-COMPENSATION RECORD (CD-DATA-  *
      * SVC / CD-SPECIAL-ACC / CD-UNBUNDLED / CD-RECIP-COMP) REACHES   *
      * THIS PROGRAM, READING IT AS CD-VOICE-DETAIL WOULD MISINTERPRET *
      * EVERY BYTE PAST CD-DATE-TIME - SO IT IS REJECTED HERE BEFORE   *
      * ANY VOICE-DETAIL FIELD IS TOUCHED.  NORMALLY THE UPSTREAM      *
      * EXTRACT JCL SPLITS THE FEEDS BY RECORD TYPE BEFORE RATIN IS    *
      * EVEN BUILT, SO THIS CHECK IS A BELT-AND-BRACES DEFENCE, NOT    *
      * THE PRIMARY CONTROL - IT STILL COUNTS AS A REJECT WHEN IT      *
      * FIRES, USING EC-DATE-INVALID SINCE THERE IS NO CABSERR CODE    *
      * SPECIFIC TO RECORD TYPE (THE CODE TABLE PREDATES THE DATA-     *
      * SERVICE AND SPECIAL-ACCESS RECORD TYPES BY SEVERAL YEARS).     *
      * A CD-FATAL EDIT STATUS (SET BY THE UPSTREAM EMI FEED PROCESS   *
      * WHEN A RECORD FAILED ITS OWN FORMAT EDITS) IS REJECTED THE     *
      * SAME WAY; CD-SUSPECT IS ALLOWED THROUGH UNCHANGED - THIS       *
      * PROGRAM HAS NO SEPARATE HANDLING FOR A SUSPECT RECORD, IT      *
      * SIMPLY RATES IT AND LETS ANY DOWNSTREAM FIELD-LEVEL EDIT       *
      * (NEGATIVE MINUTES, UNKNOWN OCN, AND SO ON) CATCH THE PROBLEM   *
      * ON ITS OWN MERITS.                                             *
      *****************************************************************
       P2202-VALIDATE-REC-TYPE.
           IF NOT CD-VALID-TYPE
               MOVE 'Y' TO WS-ERROR-SW
               MOVE EC-DATE-INVALID TO SU-ERR-CODE.
           IF WS-ERROR-SW = 'N' AND NOT CD-VOICE-MOU
               MOVE 'Y' TO WS-ERROR-SW
               MOVE EC-DATE-INVALID TO SU-ERR-CODE.
           IF WS-ERROR-SW = 'N' AND CD-FATAL
               MOVE 'Y' TO WS-ERROR-SW
               MOVE EC-DATE-INVALID TO SU-ERR-CODE.
       P2202-EXIT.
           EXIT.
      *****************************************************************
      * P2200-VALIDATE-CDR - FIELD-LEVEL EDITS PLUS JURISDICTION      *
      * DERIVATION.  ANY FAILURE ROUTES TO P2800-REJECT-TO-SUSPENSE   *
      * AND THE RECORD IS NOT RATED.                                  *
      *****************************************************************
       P2200-VALIDATE-CDR.
           MOVE 'N' TO WS-ERROR-SW.
           MOVE SPACES TO SU-ERR-CODE.
           PERFORM P2202-VALIDATE-REC-TYPE THRU P2202-EXIT.
      *****************************************************************
      * CD-LOAD-YYDDD IS THE DATE THE EMI EXTRACT PROCESS ACTUALLY     *
      * LOADED THIS RECORD ONTO RATIN, NOT THE CALL DATE.  THE HIGHEST *
      * VALUE SEEN ACROSS THE WHOLE RUN IS PRINTED ON THE REPORT       *
      * HEADER (P8100) AS A ROUGH "DATA AS OF" MARKER SO A REVIEWER    *
      * CAN TELL HOW STALE THE SOURCE EXTRACT WAS WHEN THIS CYCLE RAN. *
      *****************************************************************
           IF CD-LOAD-YYDDD > WS-MC-MAX-LOAD-YYDDD
               MOVE CD-LOAD-YYDDD TO WS-MC-MAX-LOAD-YYDDD.
           MOVE CD-OCN TO WS-CW-OCN.
           MOVE CD-BAN TO WS-CW-BAN.
           MOVE CD-SEQ-NBR TO WS-CW-SEQ-NBR.
           MOVE CD-JURIS-CD TO WS-CW-JURIS-CD.
           MOVE CD-VC-ORIG-LATA TO WS-CW-ORIG-LATA.
           MOVE CD-VC-TERM-LATA TO WS-CW-TERM-LATA.
      *****************************************************************
      * BILLABLE MINUTES NORMALLY COME FROM CD-VC-CONV-MIN (RAW        *
      * CONVERSATION TIME OFF THE SWITCH).  WHEN CD-VC-CHG-MIN IS      *
      * ALSO POPULATED IT REFLECTS THE SWITCH'S OWN ANSWER-SUPERVISION *
      * ADJUSTED CHARGE TIME, WHICH TAKES PRECEDENCE WHEN PRESENT -    *
      * THIS IS WHY WS-CW-CONV-MIN, NOT CD-VC-CONV-MIN DIRECTLY, IS    *
      * USED BY EVERY S500 RATING PARAGRAPH FROM HERE ON.              *
      *****************************************************************
           IF CD-VC-CHG-MIN NOT = 0
               MOVE CD-VC-CHG-MIN TO WS-CW-CONV-MIN
           ELSE
               MOVE CD-VC-CONV-MIN TO WS-CW-CONV-MIN.
           IF WS-CW-CONV-MIN = 0
               ADD 1 TO WS-MC-ZERO-DURATION-CNT.
           MOVE CD-VC-TANDEM-IND TO WS-CW-TANDEM-IND.
           MOVE CD-VC-CIC TO WS-CW-CIC.
           ADD CD-SEQ-NBR TO WS-HW-SEQ.
      *****************************************************************
      * CT-HASH-OCN IS AN ALPHANUMERIC-OCN HASH TOTAL, NOT A SUM - THE *
      * ESTATE-STANDARD CABHASH UTILITY FOLDS THE FOUR CD-OCN BYTES    *
      * INTO A NUMERIC VALUE AND ADDS IT TO THE RUNNING TOTAL PASSED   *
      * BY REFERENCE.  THIS IS THE SAME UTILITY EVERY OTHER PROCESS IN *
      * THE ESTATE USES FOR CTLOUT HASH TOTALS SO A DOWNSTREAM CONTROL *
      * COMPARE JOB CAN BE WRITTEN ONCE AGAINST A CONSISTENT ALGORITHM *
      * INSTEAD OF EACH PROGRAM INVENTING ITS OWN.                     *
      *****************************************************************
           CALL 'CABHASH' USING CD-OCN WS-HW-OCN
               ON EXCEPTION
                   MOVE 9999 TO WS-RC-HASH
               NOT ON EXCEPTION
                   MOVE 0 TO WS-RC-HASH.
           IF WS-PK-FIRST-RECORD
               MOVE 'N' TO WS-PK-FIRST-SW
           ELSE
               IF WS-PK-OCN = CD-OCN AND WS-PK-BAN = CD-BAN
                       AND WS-PK-SEQ-NBR = CD-SEQ-NBR
                   MOVE 'Y' TO WS-ERROR-SW
                   MOVE EC-DUP-SEQ TO SU-ERR-CODE.
           MOVE CD-OCN TO WS-PK-OCN.
           MOVE CD-BAN TO WS-PK-BAN.
           MOVE CD-SEQ-NBR TO WS-PK-SEQ-NBR.
           IF WS-ERROR-SW = 'N' AND CD-VC-CONV-MIN < 0
               MOVE 'Y' TO WS-ERROR-SW
               MOVE EC-MIN-NEGATIVE TO SU-ERR-CODE.
           IF WS-ERROR-SW = 'N' AND CD-BAN = SPACES
               MOVE 'Y' TO WS-ERROR-SW
               MOVE EC-BAN-UNKNOWN TO SU-ERR-CODE.
           IF WS-ERROR-SW = 'N' AND
                   (CD-CONN-DDD = 0 OR CD-CONN-DDD > 366)
               MOVE 'Y' TO WS-ERROR-SW
               MOVE EC-DATE-INVALID TO SU-ERR-CODE.
           IF WS-ERROR-SW = 'N'
      *        CENTURY DERIVATION FOR THE CONNECT DATE - HARDCODED
      *        PIVOT LITERAL, NOT DW-PIVOT-YY.  SEE P1600 FOR THE
      *        MAINTAINED VERSION OF THIS SAME TEST.
               IF CD-CONN-YY < 70
                   COMPUTE WS-DL-CONN-CCYY = 2000 + CD-CONN-YY
               ELSE
                   COMPUTE WS-DL-CONN-CCYY = 1900 + CD-CONN-YY.
           IF WS-ERROR-SW = 'N'
               PERFORM P2250-LOOKUP-CARRIER THRU P2250-EXIT.
           IF WS-ERROR-SW = 'N'
               PERFORM P2260-VALIDATE-TRUNK-CIC THRU P2260-EXIT.
           IF WS-ERROR-SW = 'N'
               PERFORM P2300-JURIS-DISPATCH THRU P2300-EXIT.
           IF WS-ERROR-SW = 'Y'
               ADD 1 TO WS-REJECT-CNT
               PERFORM P2800-REJECT-TO-SUSPENSE THRU P2800-EXIT
           ELSE
               ADD 1 TO WS-WRITE-CNT
               PERFORM P6500-DETERMINE-FEATURE-GRP THRU P6500-EXIT
               PERFORM P2400-ELEMENT-DISPATCH-LOOP THRU P2400-EXIT
               PERFORM P6800-RATE-8YY-QUERY THRU P6800-EXIT
               PERFORM P2700-WRITE-RATOUT THRU P2700-EXIT.
       P2200-EXIT.
           EXIT.
      *****************************************************************
      * P2260-VALIDATE-TRUNK-CIC - A TANDEM-SWITCHED CALL MUST CARRY  *
      * A TRUNK GROUP ID (WITHOUT ONE THE TANDEM SWITCHING ELEMENT    *
      * CANNOT BE ASSIGNED A TANDEM), AND WHEN THE CDR CARRIES A      *
      * NON-ZERO CIC IT MUST MATCH THE CARRIER MASTER'S CIC FOR THE   *
      * OCN ON THE RECORD - A MISMATCH USUALLY MEANS THE OCN AND CIC  *
      * WERE POPULATED FROM DIFFERENT SOURCE SYSTEMS UPSTREAM.        *
      *****************************************************************
       P2260-VALIDATE-TRUNK-CIC.
           IF WS-CW-TANDEM-IND = 'Y' AND CD-VC-TRUNK-GRP = SPACES
               MOVE 'Y' TO WS-ERROR-SW
               MOVE EC-CIRCUIT-UNKNOWN TO SU-ERR-CODE.
           IF WS-ERROR-SW = 'N' AND WS-CR-FOUND AND WS-CW-CIC NOT = 0
               IF WS-CW-CIC NOT = CR-CIC
                   MOVE 'Y' TO WS-ERROR-SW
                   MOVE EC-CIRCUIT-UNKNOWN TO SU-ERR-CODE.
       P2260-EXIT.
           EXIT.
      *****************************************************************
      * P2245-VALIDATE-OCN-FORMAT - CALLS THE ESTATE-STANDARD OCN     *
      * FORMAT UTILITY CABOCNVL BEFORE THE VSAM READ.  CABOCNVL KNOWS *
      * THE FULL NECA OCN NUMBERING RULES (VALID RANGES, RESERVED     *
      * BLOCKS, CHECK PATTERNS) THAT THIS PROGRAM DOES NOT NEED TO    *
      * DUPLICATE - IT JUST NEEDS TO KNOW WHETHER THE FOUR BYTES ON   *
      * THE CDR ARE EVEN WORTH LOOKING UP.  A FORMAT FAILURE HERE IS  *
      * FOLDED INTO THE SAME EC-OCN-UNKNOWN REJECT AS A GENUINE VSAM  *
      * NOT-FOUND, SINCE FROM THE SUSPENSE ANALYST'S POINT OF VIEW    *
      * BOTH MEAN "THIS OCN CANNOT BE BILLED" - THE DISTINCTION IS    *
      * ONLY VISIBLE IN THE RC CAPTURED AT WS-RC-OCNVL FOR DIAGNOSTIC *
      * PURPOSES.  IF THE UTILITY IS MISSING FROM THE LOAD LIBRARY,   *
      * ON EXCEPTION LETS THE OCN THROUGH TO THE VSAM READ ANYWAY -   *
      * A FORMAT CHECK IS AN OPTIMISATION, NOT A SUBSTITUTE FOR THE   *
      * REAL EXISTENCE CHECK THAT FOLLOWS.                            *
      *****************************************************************
       P2245-VALIDATE-OCN-FORMAT.
           MOVE 0 TO WS-RC-OCNVL.
           CALL 'CABOCNVL' USING WS-CW-OCN WS-RC-OCNVL
               ON EXCEPTION
                   MOVE 0 TO WS-RC-OCNVL.
           IF WS-RC-OCNVL NOT = 0
               MOVE 'Y' TO WS-ERROR-SW
               MOVE EC-OCN-UNKNOWN TO SU-ERR-CODE.
       P2245-EXIT.
           EXIT.
      *****************************************************************
      * P2250-LOOKUP-CARRIER - RANDOM READ OF CARRMST BY OCN.  ALSO   *
      * SETS THE CCL ELIGIBILITY FLAG THAT P3100 TESTS LATER - THE    *
      * FLAG IS SET HERE, BUT THE DECISION TO ACT ON IT IS MADE IN    *
      * THE SORT INPUT PROCEDURE, NOT HERE.                           *
      *****************************************************************
       P2250-LOOKUP-CARRIER.
           MOVE 'N' TO WS-CR-FOUND-SW.
           MOVE 'N' TO WS-CR-CCL-ELIGIBLE-SW.
           PERFORM P2245-VALIDATE-OCN-FORMAT THRU P2245-EXIT.
           MOVE WS-CW-OCN TO CR-OCN.
           READ CARRMST
               INVALID KEY MOVE 'N' TO WS-CR-FOUND-SW
               NOT INVALID KEY MOVE 'Y' TO WS-CR-FOUND-SW.
           IF NOT WS-CR-FOUND
               MOVE 'Y' TO WS-ERROR-SW
               MOVE EC-OCN-UNKNOWN TO SU-ERR-CODE.
           IF WS-CR-FOUND
               MOVE CR-TYPE TO WS-CR-TYPE
               MOVE CR-RECIP-COMP-ELIG TO WS-CR-RECIP-ELIG
               MOVE CR-DEFAULT-PLU TO WS-CR-PLU
               MOVE CR-ISP-CAP-MOU TO WS-CR-ISP-CAP.
           IF WS-CR-FOUND AND CR-BILLED-PARTY AND NOT CR-WIRELESS
               MOVE 'Y' TO WS-CR-CCL-ELIGIBLE-SW.
      *****************************************************************
      * CR-MPB-ELIGIBLE (MEET POINT BILLING) IS COUNT-ONLY, LIKE THE  *
      * RATE TABLE DISCOUNT FLAG ABOVE IN P1320 - NO S500 RATING       *
      * PARAGRAPH ACTS ON IT.  PRINTED ON P8150 ALONGSIDE THE OTHER    *
      * TABLE-LOAD COUNTS EVEN THOUGH IT IS A PER-CDR-LOOKUP TALLY,    *
      * NOT A TABLE-LOAD COUNT, BECAUSE THERE WAS NO OTHER REPORT      *
      * BLOCK IN THE ORIGINAL RPTOUT LAYOUT TO PUT IT ON.              *
      *****************************************************************
           IF WS-CR-FOUND AND CR-MPB-ELIGIBLE = 'Y'
               ADD 1 TO WS-TL-MPB-ELIGIBLE-CNT.
           IF WS-ERROR-SW = 'N' AND WS-CR-FOUND
               PERFORM P2255-VALIDATE-CARRIER-STATUS THRU P2255-EXIT.
       P2250-EXIT.
           EXIT.
      *****************************************************************
      * P2255-VALIDATE-CARRIER-STATUS - TWO ADDITIONAL CHECKS AGAINST *
      * THE CARRIER MASTER ROW, EACH USING AN ERROR CODE FROM CABSERR *
      * THAT NO OTHER PARAGRAPH IN THIS PROGRAM USES: A CARRIER       *
      * MARKED INACTIVE OR PAST ITS CR-EXP-YYDDD IS REJECTED AS       *
      * EC-TERM-EXPIRED, AND A CR-DEFAULT-PIU OUTSIDE 0-100 (A DATA   *
      * ENTRY ERROR ON THE CARRIER MASTER, NOT A CALL-LEVEL PROBLEM)  *
      * IS REJECTED AS EC-PIU-OUT-OF-RANGE BEFORE P2330 EVER GETS A   *
      * CHANCE TO USE IT.  A THIRD CHECK, ADDED LATER, CATCHES A      *
      * CARRIER MASTER ROW WHERE NEITHER FACTOR WAS EVER POPULATED -  *
      * SEE P2258 BELOW.                                              *
      *****************************************************************
       P2255-VALIDATE-CARRIER-STATUS.
           IF CR-ACTIVE-SW NOT = 'Y'
               MOVE 'Y' TO WS-ERROR-SW
               MOVE EC-TERM-EXPIRED TO SU-ERR-CODE.
           IF WS-ERROR-SW = 'N' AND CR-EXP-YYDDD NOT = 0 AND
                   CR-EXP-YYDDD < WS-EV-CYCLE-YYDDD
               MOVE 'Y' TO WS-ERROR-SW
               MOVE EC-TERM-EXPIRED TO SU-ERR-CODE.
           IF WS-ERROR-SW = 'N' AND (CR-DEFAULT-PIU < 0 OR
                   CR-DEFAULT-PIU > 100)
               MOVE 'Y' TO WS-ERROR-SW
               MOVE EC-PIU-OUT-OF-RANGE TO SU-ERR-CODE.
           IF WS-ERROR-SW = 'N'
               PERFORM P2258-CHECK-FACTOR-PRESENT THRU P2258-EXIT.
       P2255-EXIT.
           EXIT.
      *****************************************************************
      * P2258-CHECK-FACTOR-PRESENT - A CARRIER MASTER ROW WITH BOTH   *
      * CR-DEFAULT-PIU AND CR-DEFAULT-PLU AT ZERO HAS NEVER HAD ITS   *
      * JURISDICTIONAL FACTORS POPULATED BY THE FACTOR STUDY FEED -   *
      * A LEGITIMATE ALL-INTRASTATE OR ALL-INTERSTATE CARRIER WOULD   *
      * STILL SHOW A NON-ZERO VALUE ON ONE SIDE, SO ZERO ON BOTH IS   *
      * TREATED AS MISSING DATA RATHER THAN A GENUINE 0% READING.     *
      * ONLY CHECKED WHEN CR-FACTOR-SRC IS NOT 'T' (TARIFF DEFAULT),  *
      * BECAUSE A TARIFF-DEFAULT ROW IS ALLOWED TO CARRY ZEROES UNTIL *
      * A REAL FACTOR STUDY REPLACES IT.                              *
      *****************************************************************
       P2258-CHECK-FACTOR-PRESENT.
           IF NOT CR-TARIFF-DEFAULT AND CR-DEFAULT-PIU = 0 AND
                   WS-CR-PLU = 0
               MOVE 'Y' TO WS-ERROR-SW
               MOVE EC-FACTOR-MISSING TO SU-ERR-CODE.
       P2258-EXIT.
           EXIT.
      *****************************************************************
      * P2300-JURIS-DISPATCH - IF THE CDR ALREADY CARRIES A DEFINITE  *
      * JURISDICTION, KEEP IT.  OTHERWISE DERIVE IT FROM THE STATE/   *
      * LATA CROSS-REFERENCE BY COMPARING ORIGINATING AND TERMINATING *
      * LATA.  ALSO SETS THE STATE CODE AND THE JURISDICTION-WORD     *
      * FRAGMENT USED LATER BY P2510-BUILD-DESCRIPTION.               *
      *****************************************************************
       P2300-JURIS-DISPATCH.
           MOVE 'N' TO WS-JD-DETERMINED-SW.
           IF CD-INTERSTATE OR CD-INTRASTATE OR CD-LOCAL
               MOVE 'Y' TO WS-JD-DETERMINED-SW
           ELSE
               PERFORM P2310-DERIVE-JURIS THRU P2310-EXIT.
           IF NOT WS-JD-DETERMINED
               PERFORM P2330-PIU-CROSS-CHECK THRU P2330-EXIT.
           IF NOT WS-JD-DETERMINED
               PERFORM P2314-NPA-FALLBACK-JURIS THRU P2314-EXIT.
           IF WS-JD-DETERMINED
               PERFORM P2320-SET-JURIS-WORD THRU P2320-EXIT
           ELSE
               MOVE 'Y' TO WS-ERROR-SW
               MOVE EC-JURIS-INDET TO SU-ERR-CODE.
       P2300-EXIT.
           EXIT.
      *****************************************************************
      * P2314-NPA-FALLBACK-JURIS - LAST-RESORT JURISDICTION TEST.      *
      * WHEN NEITHER THE LATA CROSS-REFERENCE NOR THE CARRIER PIU      *
      * FACTOR COULD SETTLE THE QUESTION, COMPARE THE ORIGINATING AND  *
      * TERMINATING AREA CODES.  A MATCHING NPA IS TREATED AS          *
      * PRESUMPTIVELY INTRASTATE AND A DIFFERING NPA AS PRESUMPTIVELY  *
      * INTERSTATE.  THIS IS A KNOWN-IMPRECISE HEURISTIC - OVERLAY     *
      * AREA CODES AND NPA SPLITS MEAN THE SAME NPA CAN SPAN A STATE   *
      * LINE AND DIFFERENT NPAS CAN SHARE ONE - BUT IT IS STILL MORE   *
      * ACCURATE THAN LEAVING THE CALL INDETERMINATE, WHICH IS WHY IT  *
      * RUNS BEFORE THE FINAL REJECT RATHER THAN AFTER IT.  THE STATE  *
      * CODE IS LEFT BLANK BY THIS PATH - NPA ALONE IS                 *
      * NOT RELIABLE ENOUGH TO NAME A SPECIFIC STATE, ONLY TO CALL     *
      * THE JURISDICTION.  WS-JD-NPA-FALLBACK-CNT FEEDS THE FALLBACK   *
      * DISTRIBUTION BLOCK ON THE CONTROL REPORT (P8250) SO A SUDDEN   *
      * SPIKE IN THIS COUNT IS VISIBLE - IT USUALLY MEANS THE LATA     *
      * CROSS-REFERENCE TABLE HAS FALLEN BEHIND A NEW LATA SPLIT.      *
      *****************************************************************
       P2314-NPA-FALLBACK-JURIS.
           MOVE CD-VC-ORIG-NPANXX TO WS-NPA-ORIG-FULL.
           MOVE CD-VC-TERM-NPANXX TO WS-NPA-TERM-FULL.
           IF WS-NPA-ORIG-FULL = 0 OR WS-NPA-TERM-FULL = 0
               GO TO P2314-EXIT.
           IF WS-NPA-ORIG-NPA = WS-NPA-TERM-NPA
               MOVE 'S' TO WS-CW-JURIS-CD
           ELSE
               MOVE 'I' TO WS-CW-JURIS-CD.
           MOVE SPACES TO WS-CW-STATE-CD.
           MOVE 'Y' TO WS-JD-DETERMINED-SW.
           ADD 1 TO WS-JD-NPA-FALLBACK-CNT.
       P2314-EXIT.
           EXIT.
      *****************************************************************
      * P2330-PIU-CROSS-CHECK - SECONDARY JURISDICTION TEST, ONLY      *
      * REACHED WHEN THE LATA CROSS-REFERENCE COULD NOT DETERMINE     *
      * JURISDICTION AT ALL (P2310 FOUND NO MATCHING LATA ROW).  USES *
      * CR-DEFAULT-PIU (PERCENT INTERSTATE USAGE) FROM THE CARRIER    *
      * MASTER, ALREADY LOADED BY P2250.  A CARRIER WHOSE TRAFFIC IS  *
      * 95% OR MORE INTERSTATE IS TREATED AS WHOLLY INTERSTATE FOR    *
      * THIS CALL; 5% OR LESS IS TREATED AS WHOLLY INTRASTATE.        *
      * ANYTHING IN BETWEEN STAYS INDETERMINATE AND THE CALL IS       *
      * REJECTED - PIU IS A CARRIER-LEVEL AVERAGE, NOT A PER-CALL     *
      * MEASUREMENT, SO IT IS ONLY TRUSTED AT THE EXTREMES.           *
      *****************************************************************
       P2330-PIU-CROSS-CHECK.
           MOVE ' ' TO WS-PC-RESULT-SW.
           IF WS-CR-FOUND
               MOVE CR-DEFAULT-PIU TO WS-PC-PIU-PCT
               IF WS-PC-PIU-PCT NOT < WS-PC-HIGH-THRESHOLD
                   MOVE 'I' TO WS-PC-RESULT-SW.
           IF WS-CR-FOUND AND WS-PC-RESULT-SW = ' '
               IF WS-PC-PIU-PCT NOT > WS-PC-LOW-THRESHOLD
                   MOVE 'S' TO WS-PC-RESULT-SW.
           IF WS-PC-RESULT-INTERSTATE
               MOVE 'I' TO WS-CW-JURIS-CD
               MOVE SPACES TO WS-CW-STATE-CD
               MOVE 'Y' TO WS-JD-DETERMINED-SW.
           IF WS-PC-RESULT-INTRASTATE
               MOVE 'S' TO WS-CW-JURIS-CD
               MOVE SPACES TO WS-CW-STATE-CD
               MOVE 'Y' TO WS-JD-DETERMINED-SW.
       P2330-EXIT.
           EXIT.
      *****************************************************************
      * P2310-DERIVE-JURIS - SEARCHES THE STATE/LATA XREF TABLE FOR   *
      * THE ORIGINATING LATA TO GET A STATE AND A DEFAULT JURIS.  IF  *
      * THE TERMINATING LATA IS DIFFERENT FROM THE ORIGINATING LATA   *
      * AND CROSSES A STATE LINE THE CALL IS TREATED AS INTERSTATE.   *
      *****************************************************************
       P2310-DERIVE-JURIS.
           MOVE WS-CW-ORIG-LATA TO WS-CW-ORIG-LATA-X.
           MOVE WS-CW-TERM-LATA TO WS-CW-TERM-LATA-X.
           MOVE 'N' TO WS-JD-LATA-FOUND-SW.
           PERFORM P2312-SCAN-LATA-XREF THRU P2312-EXIT
               VARYING WS-LX-X FROM 1 BY 1
               UNTIL WS-LX-X > WS-LX-CNT OR WS-JD-LATA-FOUND.
           IF WS-JD-LATA-FOUND
               MOVE WS-JD-FOUND-STATE TO WS-CW-STATE-CD
               MOVE WS-JD-FOUND-DEFAULT-JURIS TO WS-CW-JURIS-CD
               IF WS-CW-ORIG-LATA-X NOT = WS-CW-TERM-LATA-X
                   MOVE 'I' TO WS-CW-JURIS-CD.
           IF WS-JD-LATA-FOUND
               MOVE 'Y' TO WS-JD-DETERMINED-SW.
       P2310-EXIT.
           EXIT.
      *****************************************************************
      * P2312-SCAN-LATA-XREF - ONE PROBE OF THE STATE/LATA TABLE.     *
      *****************************************************************
       P2312-SCAN-LATA-XREF.
           IF WS-LX-LATA (WS-LX-X) = WS-CW-ORIG-LATA-X
               MOVE WS-LX-STATE (WS-LX-X) TO WS-JD-FOUND-STATE
               MOVE WS-LX-JURIS-DFLT (WS-LX-X) TO
                   WS-JD-FOUND-DEFAULT-JURIS
               MOVE 'Y' TO WS-JD-LATA-FOUND-SW.
       P2312-EXIT.
           EXIT.
      *****************************************************************
      * P2320-SET-JURIS-WORD - THE JURISDICTION-WORD FRAGMENT USED BY *
      * P2510-BUILD-DESCRIPTION.  ONE OF THREE DISTANT PARAGRAPHS     *
      * THAT FEED THE BILL DESCRIPTION STRING.                        *
      *****************************************************************
       P2320-SET-JURIS-WORD.
           MOVE WS-CW-STATE-CD TO WS-DESC-FRAG3.
           IF CD-INTERSTATE
               MOVE 'INTERSTATE ' TO WS-JD-JURIS-WORD.
           IF CD-INTRASTATE
               MOVE 'INTRASTATE ' TO WS-JD-JURIS-WORD.
           IF CD-LOCAL
               MOVE 'LOCAL      ' TO WS-JD-JURIS-WORD.
           MOVE WS-JD-JURIS-WORD TO WS-DESC-FRAG2.
       P2320-EXIT.
           EXIT.
      *****************************************************************
      * P2400-ELEMENT-DISPATCH-LOOP - OFFERS EACH OF THE FIVE RATE    *
      * ELEMENTS TO THE CURRENT CDR IN TURN.                          *
      *****************************************************************
       P2400-ELEMENT-DISPATCH-LOOP.
           PERFORM P2410-DISPATCH-ONE-ELEMENT THRU P2410-EXIT
               VARYING WS-EC-X FROM 1 BY 1
               UNTIL WS-EC-X > 5.
           SET WS-EC-X TO 1.
       P2400-EXIT.
           EXIT.
      *****************************************************************
      * P2410-DISPATCH-ONE-ELEMENT - APPLICABILITY TEST FOR ONE       *
      * ELEMENT AGAINST THE CURRENT CDR, THEN RATE RESOLUTION.        *
      *****************************************************************
       P2410-DISPATCH-ONE-ELEMENT.
           MOVE 'Y' TO WS-ELEM-APPLIES-SW.
           IF WS-EC-COND-TANDEM-ONLY (WS-EC-X) = 'Y' AND
                   WS-CW-TANDEM-IND NOT = 'Y'
               MOVE 'N' TO WS-ELEM-APPLIES-SW.
           IF WS-EC-COND-INTERSTATE (WS-EC-X) = 'Y' AND
                   NOT CD-INTERSTATE
               MOVE 'N' TO WS-ELEM-APPLIES-SW.
           IF WS-ELEM-APPLIES-SW = 'Y'
               MOVE WS-EC-ELEM-CODE (WS-EC-X) TO WS-ELEM-CLASS
               MOVE R1-TARIFF-CD TO WS-RK-TARIFF
               MOVE WS-EC-ELEM-CODE (WS-EC-X) TO WS-RK-ELEM
               MOVE WS-CW-JURIS-CD TO WS-RK-JURIS
               MOVE WS-CW-STATE-CD TO WS-RK-STATE
               PERFORM P4000-RESOLVE-RATE THRU P4000-EXIT
               PERFORM P2420-HANDLE-RESOLUTION THRU P2420-EXIT.
       P2410-EXIT.
           EXIT.
      *****************************************************************
      * P2420-HANDLE-RESOLUTION - RATE FOUND MEANS RATE THE ELEMENT   *
      * AND RELEASE IT TO THE SORT; NOT FOUND MEANS A SOFT REJECT     *
      * FOR THAT ONE ELEMENT (THE OTHER FOUR STILL PROCEED).          *
      *****************************************************************
       P2420-HANDLE-RESOLUTION.
           IF WS-RR-FOUND
               PERFORM P7100-DETERMINE-OVERRIDE THRU P7100-EXIT
               PERFORM P2450-CLASSIFY-FOR-REPORT THRU P2450-EXIT
               PERFORM P2460-ACCUM-OCN-TOTAL THRU P2460-EXIT
               PERFORM P2470-ACCUM-END-OFFICE THRU P2470-EXIT
               PERFORM P2500-BUILD-SORT-REC THRU P2500-EXIT
               ADD 1 TO WS-MC-ELEMENTS-RATED
           ELSE
               PERFORM P2820-SOFT-REJECT-ELEMENT THRU P2820-EXIT.
       P2420-EXIT.
           EXIT.
      *****************************************************************
      * P2450-CLASSIFY-FOR-REPORT - USES THE OVERLAPPING 88-LEVELS ON *
      * WS-ELEM-CLASS.  WS-EC-SWITCHED IS TESTED FIRST, SO A TANDEM   *
      * ELEMENT ('TANSW ') LANDS IN THE SWITCHED-ACCESS REPORT ROW    *
      * EVEN THOUGH WS-EC-TRANSPORT IS ALSO TRUE FOR THE SAME VALUE.  *
      * THE REPORT ROW INDEX BELOW MUST STAY IN STEP WITH THE ORDER   *
      * ELEMENTS WERE LOADED IN P1750.                                *
      *****************************************************************
       P2450-CLASSIFY-FOR-REPORT.
           SET WS-RT-X TO WS-EC-X.
           MOVE WS-EC-ELEM-CODE (WS-EC-X) TO WS-RT-ELEM-CODE (WS-RT-X).
           IF WS-EC-SWITCHED
               ADD WS-BILLABLE-MIN TO WS-RT-ELEM-MINUTES (WS-RT-X)
           ELSE
               IF WS-EC-TRANSPORT
                   ADD WS-BILLABLE-MIN TO
                       WS-RT-ELEM-MINUTES (WS-RT-X).
           ADD WS-ELEM-AMOUNT TO WS-RT-ELEM-AMOUNT (WS-RT-X).
           ADD 1 TO WS-RT-ELEM-CNT (WS-RT-X).
           IF WS-MIN-CHG-APPLIED
               ADD 1 TO WS-RT-MIN-APPLIED-CNT (WS-RT-X).
           IF WS-MAX-CHG-APPLIED
               ADD 1 TO WS-RT-MAX-APPLIED-CNT (WS-RT-X).
       P2450-EXIT.
           EXIT.
      *****************************************************************
      * P2460-ACCUM-OCN-TOTAL - FIND-OR-ADD INTO THE 200-ROW OCN      *
      * SUMMARY TABLE PRINTED BY P8200.  SAME SEARCH-OR-APPEND        *
      * SHAPE AS P3660 IN THE SORT OUTPUT PROCEDURE.                  *
      *****************************************************************
       P2460-ACCUM-OCN-TOTAL.
           MOVE 'N' TO WS-OT-FOUND-SW.
           MOVE 0 TO WS-OT-FOUND-NUM.
           PERFORM P2462-SEARCH-OCN THRU P2462-EXIT
               VARYING WS-OT-X FROM 1 BY 1
               UNTIL WS-OT-X > WS-OT-CNT OR WS-OT-FOUND.
           IF WS-OT-FOUND
               SET WS-OT-X TO WS-OT-FOUND-NUM.
           IF NOT WS-OT-FOUND AND WS-OT-CNT < 200
               ADD 1 TO WS-OT-CNT
               SET WS-OT-X TO WS-OT-CNT
               MOVE WS-CW-OCN TO WS-OT-OCN (WS-OT-X)
               MOVE 0 TO WS-OT-MINUTES (WS-OT-X)
               MOVE 0 TO WS-OT-AMOUNT (WS-OT-X)
               MOVE 0 TO WS-OT-CALL-CNT (WS-OT-X)
               MOVE WS-CR-TYPE TO WS-OT-CARRIER-TYPE (WS-OT-X)
               MOVE 'Y' TO WS-OT-FOUND-SW.
           IF WS-OT-FOUND
               ADD WS-BILLABLE-MIN TO WS-OT-MINUTES (WS-OT-X)
               ADD WS-ELEM-AMOUNT TO WS-OT-AMOUNT (WS-OT-X)
               ADD 1 TO WS-OT-CALL-CNT (WS-OT-X).
           IF WS-CW-JURIS-CD = 'L' AND WS-CR-RECIP-ELIG = 'Y'
               PERFORM P2465-CHECK-RECIP-CAP THRU P2465-EXIT.
       P2460-EXIT.
           EXIT.
      *****************************************************************
      * P2465-CHECK-RECIP-CAP - INFORMATIONAL ONLY, NOT A REJECT.      *
      * CR-ISP-CAP-MOU IS THE MONTHLY RECIPROCAL COMPENSATION MINUTES  *
      * CAP CARRIED ON THE CARRIER MASTER FOR ISP-BOUND LOCAL TRAFFIC. *
      * ONCE THE RUNNING PER-OCN MINUTE TOTAL IN WS-OT-MINUTES PASSES  *
      * THE CAP, EVERY SUBSEQUENT LOCAL CALL FOR THAT OCN IS COUNTED   *
      * IN WS-RS-CAP-EXCEEDED-CNT AND SHOWN ON THE EXCEPTION SUMMARY   *
      * (P8700) - THE CALL ITSELF STILL RATES AND BILLS NORMALLY.      *
      * WHETHER TRAFFIC OVER THE CAP IS ACTUALLY SETTLED AT A LOWER    *
      * RATE OR NOT AT ALL IS A BILLING POLICY DECISION MADE OUTSIDE   *
      * THIS PROGRAM - CABRAT03 ONLY FLAGS THE CONDITION FOR REVIEW.   *
      *****************************************************************
       P2465-CHECK-RECIP-CAP.
           ADD WS-BILLABLE-MIN TO WS-RS-TOTAL-MINUTES.
           ADD WS-ELEM-AMOUNT TO WS-RS-TOTAL-AMOUNT.
           IF WS-CR-ISP-CAP > 0 AND
                   WS-OT-MINUTES (WS-OT-X) > WS-CR-ISP-CAP
               ADD 1 TO WS-RS-CAP-EXCEEDED-CNT.
       P2465-EXIT.
           EXIT.
       P2462-SEARCH-OCN.
           IF WS-OT-OCN (WS-OT-X) = WS-CW-OCN
               MOVE 'Y' TO WS-OT-FOUND-SW
               SET WS-OT-FOUND-NUM TO WS-OT-X.
       P2462-EXIT.
           EXIT.
      *****************************************************************
      * P2470-ACCUM-END-OFFICE - FIND-OR-ADD INTO A SMALL DISTINCT-    *
      * END-OFFICE TABLE, SAME SHAPE AS P2460 ABOVE.  CD-VC-END-OFFICE *
      * IS A CLLI-STYLE CODE IDENTIFYING THE ORIGINATING SWITCH - THE  *
      * DISTINCT COUNT PRINTED BY P8280 IS A ROUGH PROXY FOR HOW MANY  *
      * PHYSICAL SWITCHES FED THIS CYCLE'S RATIN EXTRACT, WHICH IS     *
      * USEFUL WHEN A CARRIER REPORTS A SWITCH DECOMMISSIONED AND      *
      * OPERATIONS WANTS TO CONFIRM ITS TRAFFIC HAS ACTUALLY STOPPED.  *
      * RUNS ONCE PER RATED ELEMENT, NOT ONCE PER CDR, SO THE PER-     *
      * ENTRY CALL COUNT IS AN ELEMENT COUNT, NOT A CDR COUNT - ONLY   *
      * THE DISTINCT-CODE COUNT (WS-EO-CNT) IS ACTUALLY PRINTED.       *
      *****************************************************************
       P2470-ACCUM-END-OFFICE.
           IF CD-VC-END-OFFICE = SPACES
               GO TO P2470-EXIT.
           MOVE 'N' TO WS-EO-FOUND-SW.
           MOVE 0 TO WS-EO-FOUND-NUM.
           PERFORM P2472-SEARCH-END-OFFICE THRU P2472-EXIT
               VARYING WS-EO-X FROM 1 BY 1
               UNTIL WS-EO-X > WS-EO-CNT OR WS-EO-FOUND.
           IF WS-EO-FOUND
               SET WS-EO-X TO WS-EO-FOUND-NUM.
           IF NOT WS-EO-FOUND AND WS-EO-CNT < 100
               ADD 1 TO WS-EO-CNT
               SET WS-EO-X TO WS-EO-CNT
               MOVE CD-VC-END-OFFICE TO WS-EO-CODE (WS-EO-X)
               MOVE 0 TO WS-EO-CALL-CNT (WS-EO-X)
               MOVE 'Y' TO WS-EO-FOUND-SW.
           IF WS-EO-FOUND
               ADD 1 TO WS-EO-CALL-CNT (WS-EO-X).
       P2470-EXIT.
           EXIT.
       P2472-SEARCH-END-OFFICE.
           IF WS-EO-CODE (WS-EO-X) = CD-VC-END-OFFICE
               MOVE 'Y' TO WS-EO-FOUND-SW
               SET WS-EO-FOUND-NUM TO WS-EO-X.
       P2472-EXIT.
           EXIT.
      *****************************************************************
      * P2500-BUILD-SORT-REC - ONE RATED-ELEMENT ROW, READY FOR THE   *
      * SORT.  THE ACTUAL RELEASE (AND THE CCL ELIGIBILITY GATE) IS   *
      * IN P3100, WHICH LIVES IN S300-SORT-INPUT SECTION.             *
      *****************************************************************
       P2500-BUILD-SORT-REC.
           MOVE WS-CW-OCN TO SR-OCN.
           MOVE WS-CW-BAN TO SR-BAN.
           MOVE WS-CW-JURIS-CD TO SR-JURIS-CD.
           MOVE WS-ELEM-CLASS TO SR-RATE-ELEM.
           MOVE WS-CW-STATE-CD TO SR-STATE-CD.
           MOVE R1-BILL-PERIOD TO SR-BILL-PERIOD.
           MOVE 'VC' TO SR-SECTION.
           MOVE WS-CW-SEQ-NBR TO SR-SEQ-NBR.
           MOVE WS-BILLABLE-MIN TO SR-QTY.
           MOVE WS-SEL-RATE TO SR-RATE.
           MOVE WS-ELEM-AMOUNT TO SR-AMOUNT.
           MOVE WS-SEL-ROUND-RULE TO SR-ROUND-RULE.
           MOVE WS-PGM-NAME TO SR-SRC-PROCESS.
           MOVE WS-MB-BAND-TEXT TO SR-MILEAGE-BAND.
           MOVE WS-CR-CCL-ELIGIBLE-SW TO SR-CCL-ELIGIBLE-SW.
           PERFORM P2510-BUILD-DESCRIPTION THRU P2510-EXIT.
           PERFORM P3100-RELEASE-ELEMENT-REC THRU P3100-EXIT.
       P2500-EXIT.
           EXIT.
      *****************************************************************
      * P2510-BUILD-DESCRIPTION - STRING ASSEMBLY FROM SIX FRAGMENTS  *
      * THAT WERE SET IN FIVE DIFFERENT, PHYSICALLY DISTANT           *
      * PARAGRAPHS: FRAG1 IN S700 (P7100), FRAG2 AND FRAG3 IN S200    *
      * (P2320), FRAG4 IN S600 (P6400), FRAG5 AND FRAG6 IN S500 (THE  *
      * RATING PARAGRAPH THAT ACTUALLY RAN FOR THIS ELEMENT).  NO     *
      * SINGLE PARAGRAPH IN THE PROGRAM SETS ALL SIX.                 *
      *****************************************************************
       P2510-BUILD-DESCRIPTION.
           MOVE SPACES TO WS-DESC-ASSEMBLED.
           STRING WS-DESC-FRAG1 DELIMITED BY SIZE
                  ' '          DELIMITED BY SIZE
                  WS-DESC-FRAG2 DELIMITED BY SIZE
                  WS-DESC-FRAG3 DELIMITED BY SIZE
                  ' '          DELIMITED BY SIZE
                  WS-DESC-FRAG4 DELIMITED BY SIZE
                  WS-DESC-FRAG5 DELIMITED BY SIZE
                  WS-DESC-FRAG6 DELIMITED BY SIZE
               INTO WS-DESC-ASSEMBLED.
           MOVE WS-DESC-ASSEMBLED TO SR-DESCRIPTION.
       P2510-EXIT.
           EXIT.
      *****************************************************************
      * P2700-WRITE-RATOUT - RATED CDR PASS-THROUGH AUDIT TRAIL.      *
      *****************************************************************
       P2700-WRITE-RATOUT.
           MOVE SPACES TO CABS-RATOUT-RECORD.
           MOVE WS-CW-OCN TO RO-OCN.
           MOVE WS-CW-BAN TO RO-BAN.
           MOVE WS-CW-SEQ-NBR TO RO-SEQ-NBR.
           MOVE 'R' TO RO-RATE-STATUS.
           MOVE WS-EV-CYCLE-YYDDD TO RO-CYCLE-YYDDD.
           MOVE WS-CW-JURIS-CD TO RO-JURIS-CD.
           MOVE WS-CW-STATE-CD TO RO-STATE-CD.
           MOVE 5 TO RO-ELEM-COUNT.
           MOVE WS-CW-CONV-MIN TO RO-TOT-MINUTES.
           MOVE 0 TO RO-TOT-AMOUNT.
           MOVE WS-FG-CODE TO RO-FEATURE-GRP.
           WRITE CABS-RATOUT-RECORD.
       P2700-EXIT.
           EXIT.
      *****************************************************************
      * P2800-REJECT-TO-SUSPENSE - WHOLE-CDR REJECTS FROM P2200.      *
      *****************************************************************
       P2800-REJECT-TO-SUSPENSE.
           MOVE SPACES TO CABS-SUSPENSE-RECORD-FD.
           MOVE SU-ERR-CODE TO FD-SU-ERR-CODE.
           MOVE 'E' TO FD-SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO FD-SU-DETECT-PGM.
           MOVE 'P2200-VALIDATE-CDR' TO FD-SU-DETECT-PARA.
           MOVE R1-RUN-ID TO FD-SU-RUN-ID.
           MOVE CABS-CDR-RECORD TO FD-SU-ORIG-RECORD.
           WRITE CABS-SUSPENSE-RECORD-FD.
      *****************************************************************
      * IN ADDITION TO THE LOCAL SUSOUT WRITE ABOVE, EVERY HARD REJECT *
      * IS ALSO PASSED TO CABERRWR, THE ESTATE-WIDE ERROR AUDIT TRAIL  *
      * WRITER.  CABERRWR APPENDS TO A CROSS-APPLICATION LOG THAT THE  *
      * OPERATIONS TEAM SCANS EVERY MORNING ACROSS ALL CABS PROCESSES, *
      * NOT JUST RATING - IT IS HELD SEPARATE FROM SUSOUT              *
      * BECAUSE SUSOUT IS RE-DRIVEN BACK THROUGH RATING ON THE NEXT    *
      * CYCLE BUT THE AUDIT TRAIL IS PERMANENT AND IS NEVER REPLAYED.  *
      *****************************************************************
           CALL 'CABERRWR' USING FD-SU-ERR-CODE FD-SU-DETECT-PGM
               FD-SU-DETECT-PARA FD-SU-RUN-ID
               ON EXCEPTION
                   MOVE 9999 TO WS-RC-ERRWR
               NOT ON EXCEPTION
                   MOVE 0 TO WS-RC-ERRWR.
           MOVE SPACES TO CABS-RATOUT-RECORD.
           MOVE WS-CW-OCN TO RO-OCN.
           MOVE WS-CW-BAN TO RO-BAN.
           MOVE WS-CW-SEQ-NBR TO RO-SEQ-NBR.
           MOVE 'S' TO RO-RATE-STATUS.
           WRITE CABS-RATOUT-RECORD.
       P2800-EXIT.
           EXIT.
      *****************************************************************
      * P2820-SOFT-REJECT-ELEMENT - A SINGLE RATE ELEMENT COULD NOT   *
      * BE RATED (NO MATCHING RATE TABLE ROW).  THE CDR AS A WHOLE    *
      * STILL PROCEEDS - THE OTHER APPLICABLE ELEMENTS STILL RATE.    *
      *****************************************************************
       P2820-SOFT-REJECT-ELEMENT.
           ADD 1 TO WS-MC-RATE-NOT-FOUND-SOFT.
           MOVE SPACES TO CABS-SUSPENSE-RECORD-FD.
           MOVE EC-RATE-NOT-FOUND TO FD-SU-ERR-CODE.
           MOVE 'W' TO FD-SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO FD-SU-DETECT-PGM.
           MOVE 'P4000-RESOLVE-RATE' TO FD-SU-DETECT-PARA.
           MOVE R1-RUN-ID TO FD-SU-RUN-ID.
           MOVE CABS-CDR-RECORD TO FD-SU-ORIG-RECORD.
           WRITE CABS-SUSPENSE-RECORD-FD.
       P2820-EXIT.
           EXIT.
      *****************************************************************
      * S300-SORT-INPUT SECTION - THE SORT'S INPUT PROCEDURE.  DRIVES *
      * THE READ / VALIDATE / RATE CYCLE OVER ALL OF RATIN, THEN      *
      * RELEASES ONE SORT RECORD PER RATED ELEMENT.  THE CCL          *
      * ELIGIBILITY RULE IS APPLIED IN P3100 BELOW, AND ONLY THERE.   *
      *****************************************************************
       S300-SORT-INPUT SECTION.
       P3000-SORT-INPUT-CTL.
           MOVE 'N' TO WS-SORT-INPUT-EOF-SW.
           PERFORM P2100-READ-RATIN THRU P2100-EXIT.
           PERFORM P3010-PROCESS-ONE-CDR THRU P3010-EXIT
               UNTIL WS-SORT-INPUT-EOF.
           GO TO P3900-SORT-INPUT-DONE.
      *****************************************************************
      * P3010-PROCESS-ONE-CDR - WHEN A RESTART KEY WAS SUPPLIED ON    *
      * THE PARM CARD, EVERY RECORD IS CHECKED AGAINST IT VIA P2110   *
      * UNTIL THE RESTART POINT IS PASSED, AT WHICH POINT WS-RESTART- *
      * SW IS TURNED OFF AND THE CHECK STOPS RUNNING FOR THE REST OF  *
      * THE FILE.  SKIPPED RECORDS COUNT AS CT-CARRIED-FWD, NOT AS    *
      * CT-WRITTEN OR CT-REJECTED - THEY WERE ALREADY DISPOSED OF BY  *
      * WHATEVER RUN THIS ONE IS RESTARTING.                          *
      *****************************************************************
       P3010-PROCESS-ONE-CDR.
           MOVE 'N' TO WS-RESTART-SKIP-SW.
           IF WS-RESTARTING
               PERFORM P2110-CHECK-RESTART-SKIP THRU P2110-EXIT.
           IF WS-RESTART-SKIP
               ADD 1 TO WS-CFWD-CNT
           ELSE
               PERFORM P2200-VALIDATE-CDR THRU P2200-EXIT.
           PERFORM P2100-READ-RATIN THRU P2100-EXIT.
       P3010-EXIT.
           EXIT.
      *****************************************************************
      * P2110-CHECK-RESTART-SKIP - ONCE THE CURRENT RECORD'S KEY      *
      * EXCEEDS THE SAVED RESTART KEY, RESTART MODE IS TURNED OFF     *
      * FOR GOOD - THERE IS NO NEED TO KEEP COMPARING FOR THE         *
      * REMAINDER OF A MULTI-MILLION-RECORD FILE.                     *
      *****************************************************************
       P2110-CHECK-RESTART-SKIP.
           MOVE CD-OCN TO WS-RSK-OCN.
           MOVE CD-BAN TO WS-RSK-BAN.
           MOVE CD-SEQ-NBR TO WS-RSK-SEQ.
           IF WS-RESTART-CDR-KEY NOT > WS-RESTART-KEY-SAVE
               MOVE 'Y' TO WS-RESTART-SKIP-SW
           ELSE
               MOVE 'N' TO WS-RESTART-SW
               MOVE 'N' TO WS-RESTART-SKIP-SW.
       P2110-EXIT.
           EXIT.
      *****************************************************************
      * P3100-RELEASE-ELEMENT-REC - CARRIER COMMON LINE APPLIES ONLY  *
      * TO INTERSTATE PREMIUM ACCESS TRAFFIC.  WHETHER A CARRIER'S    *
      * TRAFFIC QUALIFIES AS "PREMIUM" FOR CCL PURPOSES IS DECIDED    *
      * SOLELY BY WS-CR-CCL-ELIGIBLE-SW, SET BACK IN P2250 FROM THE   *
      * CARRIER TYPE.  IF THE CURRENT ELEMENT IS CCLINE AND THE       *
      * CARRIER IS NOT ELIGIBLE, THE ROW IS SILENTLY DROPPED HERE -   *
      * NOT REJECTED, NOT SUSPENDED, JUST NEVER RELEASED TO THE SORT. *
      * THIS IS THE ONLY PLACE IN THE PROGRAM WHERE THAT HAPPENS.     *
      *****************************************************************
       P3100-RELEASE-ELEMENT-REC.
           IF SR-RATE-ELEM = WS-ELEM-CCLINE AND
                   SR-CCL-ELIGIBLE-SW NOT = 'Y'
               ADD 1 TO WS-MC-ELEMENTS-DROPPED-CCL
           ELSE
               RELEASE WS-SORT-RECORD
               ADD 1 TO WS-MC-SORT-RELEASED.
       P3100-EXIT.
           EXIT.
       P3900-SORT-INPUT-DONE.
           DISPLAY 'CABRAT03 - SORT INPUT PROCEDURE COMPLETE - READ '
               WS-READ-CNT.
      *****************************************************************
      * S350-SORT-OUTPUT SECTION - THE SORT'S OUTPUT PROCEDURE.       *
      * RETURNS THE RATED-ELEMENT ROWS IN OCN/BAN/JURIS/ELEM ORDER,   *
      * BUILDS ONE CABS-BILL-DETAIL PER BAN/JURISDICTION GROUP, AND   *
      * SUPPRESSES ANY ELEMENT WHOSE TOTAL AMOUNT IS BELOW THE HALF-  *
      * CENT THRESHOLD.  THAT SUPPRESSION RULE LIVES ONLY HERE.       *
      *****************************************************************
       S350-SORT-OUTPUT SECTION.
       P3500-SORT-OUTPUT-CTL.
           MOVE 'N' TO WS-SORT-EOF-SW.
           MOVE 'Y' TO WS-BB-FIRST-REC-SW.
           PERFORM P3510-RETURN-NEXT THRU P3510-EXIT.
           PERFORM P3520-PROCESS-RETURNED-REC THRU P3520-EXIT
               UNTIL WS-SORT-EOF.
           IF NOT WS-BB-FIRST-REC
               PERFORM P3700-WRITE-BILL-DETAIL THRU P3700-EXIT.
           GO TO P3990-SORT-OUTPUT-DONE.
       P3510-RETURN-NEXT.
           RETURN SORTWK
               AT END MOVE 'Y' TO WS-SORT-EOF-SW.
           IF NOT WS-SORT-EOF
               ADD 1 TO WS-MC-SORT-RETURNED.
       P3510-EXIT.
           EXIT.
      *****************************************************************
      * P3520-PROCESS-RETURNED-REC - DETECTS THE BAN/JURISDICTION     *
      * LEVEL BREAK (SEQUENTIAL FLAG STYLE, NOT NESTED IF/ELSE, SO    *
      * EVERY BRANCH STAYS UNAMBIGUOUS UNDER THE 1974 PERIOD RULES).  *
      *****************************************************************
       P3520-PROCESS-RETURNED-REC.
           MOVE 'N' TO WS-BB-NEW-GROUP-SW.
           IF WS-BB-FIRST-REC
               MOVE 'Y' TO WS-BB-NEW-GROUP-SW.
           IF NOT WS-BB-FIRST-REC AND (SR-BAN NOT = WS-BB-SAVE-BAN
                   OR SR-JURIS-CD NOT = WS-BB-SAVE-JURIS)
               MOVE 'Y' TO WS-BB-NEW-GROUP-SW.
           IF WS-BB-NEW-GROUP-SW = 'Y' AND NOT WS-BB-FIRST-REC
               PERFORM P3700-WRITE-BILL-DETAIL THRU P3700-EXIT.
           IF WS-BB-NEW-GROUP-SW = 'Y'
               PERFORM P3600-START-NEW-BAN THRU P3600-EXIT.
           PERFORM P3650-ADD-ELEMENT THRU P3650-EXIT.
           PERFORM P3510-RETURN-NEXT THRU P3510-EXIT.
       P3520-EXIT.
           EXIT.
       P3600-START-NEW-BAN.
           MOVE SPACES TO CABS-BILL-DETAIL.
           MOVE 0 TO BD-ELEM-CNT.
           MOVE SR-BAN TO WS-BB-SAVE-BAN.
           MOVE SR-OCN TO WS-BB-SAVE-OCN.
           MOVE SR-JURIS-CD TO WS-BB-SAVE-JURIS.
           MOVE SR-STATE-CD TO WS-BB-SAVE-STATE.
           MOVE SR-SECTION TO WS-BB-SAVE-SECTION.
           MOVE SR-BILL-PERIOD TO WS-BB-SAVE-BILL-PERIOD.
           MOVE 0 TO WS-BB-TOT-MINUTES.
           MOVE 0 TO WS-BB-TOT-AMOUNT.
           ADD 1 TO WS-BB-LINE-SEQ.
           MOVE 'N' TO WS-BB-FIRST-REC-SW.
       P3600-EXIT.
           EXIT.
      *****************************************************************
      * P3650-ADD-ELEMENT - THE SUPPRESSION TEST.  AN ELEMENT WHOSE   *
      * AMOUNT FOR THIS RETURNED ROW IS UNDER HALF A CENT NEVER       *
      * BECOMES A BD-ELEMENT ENTRY - IT IS SIMPLY COUNTED AND         *
      * DROPPED.  NOTHING ELSEWHERE IN THE PROGRAM KNOWS THIS RULE    *
      * EXISTS.                                                       *
      *****************************************************************
       P3650-ADD-ELEMENT.
           MOVE 'N' TO WS-BB-SUPPRESS-SW.
           IF SR-AMOUNT < WS-SUPPRESS-THRESHOLD
               ADD 1 TO WS-MC-ELEMENTS-SUPPRESSED
               MOVE 'Y' TO WS-BB-SUPPRESS-SW.
           IF WS-BB-SUPPRESS-SW = 'N'
               PERFORM P3660-FIND-OR-ADD-ELEMENT THRU P3660-EXIT.
       P3650-EXIT.
           EXIT.
       P3660-FIND-OR-ADD-ELEMENT.
           MOVE 'N' TO WS-BB-ELEM-FOUND-SW.
           MOVE 0 TO WS-BB-FOUND-NUM.
           PERFORM P3670-SEARCH-ONE THRU P3670-EXIT
               VARYING BD-EX FROM 1 BY 1
               UNTIL BD-EX > BD-ELEM-CNT OR WS-BB-ELEM-FOUND.
           IF WS-BB-ELEM-FOUND
               SET BD-EX TO WS-BB-FOUND-NUM.
           IF NOT WS-BB-ELEM-FOUND AND BD-ELEM-CNT < 40
               PERFORM P3680-APPEND-NEW-ELEMENT THRU P3680-EXIT
               MOVE 'Y' TO WS-BB-ELEM-FOUND-SW.
           IF NOT WS-BB-ELEM-FOUND AND BD-ELEM-CNT NOT < 40
               ADD 1 TO WS-MC-BD-TABLE-FULL
               DISPLAY 'CABRAT03 WARNING - BD-ELEMENT TABLE FULL '
                   'FOR BAN ' WS-BB-SAVE-BAN.
           IF WS-BB-ELEM-FOUND
               ADD SR-QTY TO BD-EL-QTY (BD-EX)
               ADD SR-AMOUNT TO BD-EL-AMOUNT (BD-EX)
               ADD SR-QTY TO WS-BB-TOT-MINUTES
               ADD SR-AMOUNT TO WS-BB-TOT-AMOUNT.
       P3660-EXIT.
           EXIT.
       P3670-SEARCH-ONE.
           IF BD-EL-RATE-ELEM (BD-EX) = SR-RATE-ELEM
               MOVE 'Y' TO WS-BB-ELEM-FOUND-SW
               SET WS-BB-FOUND-NUM TO BD-EX.
       P3670-EXIT.
           EXIT.
       P3680-APPEND-NEW-ELEMENT.
           ADD 1 TO BD-ELEM-CNT.
           SET BD-EX TO BD-ELEM-CNT.
           MOVE SR-RATE-ELEM TO BD-EL-RATE-ELEM (BD-EX).
           MOVE 0 TO BD-EL-QTY (BD-EX).
           MOVE SR-RATE TO BD-EL-RATE (BD-EX).
           MOVE 0 TO BD-EL-AMOUNT (BD-EX).
           MOVE SR-ROUND-RULE TO BD-EL-ROUND-RULE (BD-EX).
           MOVE SR-SRC-PROCESS TO BD-EL-SRC-PROCESS (BD-EX).
           IF BD-ELEM-CNT = 1
               MOVE SR-DESCRIPTION TO BD-DESCRIPTION.
       P3680-EXIT.
           EXIT.
      *****************************************************************
      * P3700-WRITE-BILL-DETAIL - CLOSES OUT THE CURRENT BAN GROUP.   *
      *****************************************************************
       P3700-WRITE-BILL-DETAIL.
           MOVE WS-BB-SAVE-BAN TO BD-BAN.
           MOVE WS-BB-SAVE-BILL-PERIOD TO BD-BILL-PERIOD.
           MOVE WS-BB-SAVE-SECTION TO BD-SECTION.
           MOVE WS-BB-LINE-SEQ TO BD-LINE-SEQ.
           MOVE WS-BB-SAVE-OCN TO BD-OCN.
           MOVE WS-BB-SAVE-JURIS TO BD-JURIS-CD.
           MOVE WS-BB-SAVE-STATE TO BD-STATE-CD.
           MOVE WS-BB-TOT-MINUTES TO BD-TOT-MINUTES.
           MOVE WS-BB-TOT-AMOUNT TO BD-TOT-AMOUNT.
           MOVE WS-BB-TOT-AMOUNT TO R4-RAW-AMT.
           PERFORM P4900-ROUND-AMOUNT THRU P4900-EXIT.
           MOVE R4-ROUNDED-AMT TO BD-TOT-ROUNDED.
           MOVE R4-RESIDUE TO BD-ROUND-DELTA.
           IF BD-ELEM-CNT > 0
               WRITE CABS-BILL-DETAIL
               ADD 1 TO WS-MC-BANS-WRITTEN
               ADD WS-BB-TOT-MINUTES TO WS-HW-MINUTES
               ADD WS-BB-TOT-AMOUNT TO WS-HW-AMOUNT.
       P3700-EXIT.
           EXIT.
       P3990-SORT-OUTPUT-DONE.
           DISPLAY 'CABRAT03 - SORT OUTPUT PROCEDURE COMPLETE - BANS '
               WS-MC-BANS-WRITTEN.
      *****************************************************************
      * S400-RATE-RESOLUTION SECTION - BINARY-STYLE SEARCH OF THE     *
      * INTERNAL RATE TABLE WITH A THREE-LEVEL FALLBACK: EXACT STATE, *
      * JURISDICTION-GENERIC (STATE BLANK), THEN TARIFF DEFAULT       *
      * (JURISDICTION AND STATE BOTH BLANK).                          *
      *                                                                *
      * HISTORY NOTE (P.NAIR, 1998, ON THE ORIGINAL CHANGE THAT        *
      * INTRODUCED THIS SECTION) - THE SEQUENTIAL SCAN THIS REPLACED   *
      * WAS COSTING ROUGHLY FOUR CPU MINUTES PER MILLION CDRS ONCE     *
      * THE RATE TABLE PASSED ABOUT 150 ROWS.  THE BINARY SEARCH       *
      * BROUGHT THAT DOWN TO UNDER THIRTY SECONDS.  THE TRADE-OFF IS   *
      * THAT THE TABLE MUST BE TRUE ASCENDING KEY SEQUENCE WITH NO     *
      * GAPS IN THE COMPARISON LOGIC - SEE P1900, ADDED YEARS LATER    *
      * AFTER A 2011 INCIDENT WHERE A RATEMST REBUILD JOB LEFT THE     *
      * VSAM KSDS ALTERNATE INDEX ACTIVE FOR A FEW MINUTES AND THIS    *
      * PROGRAM SILENTLY RATED SEVERAL THOUSAND CALLS AGAINST THE      *
      * WRONG STATE BECAUSE THE BROWSE CAME BACK IN ALTERNATE-INDEX    *
      * ORDER INSTEAD OF PRIMARY-KEY ORDER.  P1900 WOULD HAVE CAUGHT   *
      * THAT IN SECONDS INSTEAD OF THE THREE DAYS IT ACTUALLY TOOK TO  *
      * NOTICE THE BAD BILLS.                                          *
      *****************************************************************
       S400-RATE-RESOLUTION SECTION.
       P4000-RESOLVE-RATE.
           MOVE 'N' TO WS-RR-FOUND-SW.
           MOVE '1' TO WS-RR-FALLBACK-LVL.
           PERFORM P4100-BINARY-SEARCH THRU P4100-EXIT.
           IF WS-BS-DONE
               PERFORM P4200-DATE-FILTER THRU P4200-EXIT.
           IF NOT WS-RR-FOUND
               PERFORM P4300-TRY-JURIS-GENERIC THRU P4300-EXIT.
           IF NOT WS-RR-FOUND
               PERFORM P4400-TRY-TARIFF-DEFAULT THRU P4400-EXIT.
           IF WS-RR-FOUND
               PERFORM P4050-TALLY-FALLBACK-LVL THRU P4050-EXIT.
       P4000-EXIT.
           EXIT.
      *****************************************************************
      * P4050-TALLY-FALLBACK-LVL - REPORT-ONLY COUNTER, PRINTED BY    *
      * P8250.  LETS THE CONTROL REPORT SHOW HOW MUCH OF THE RUN WAS  *
      * RATED ON AN EXACT STATE MATCH VERSUS A FALLBACK LEVEL.        *
      *****************************************************************
       P4050-TALLY-FALLBACK-LVL.
           IF WS-RR-EXACT-STATE
               ADD 1 TO WS-RE-FB-EXACT.
           IF WS-RR-JURIS-GENERIC
               ADD 1 TO WS-RE-FB-JURIS-GEN.
           IF WS-RR-TARIFF-DEFAULT
               ADD 1 TO WS-RE-FB-TARIFF-DFLT.
       P4050-EXIT.
           EXIT.
      *****************************************************************
      * P4100-BINARY-SEARCH - TEXTBOOK BINARY SEARCH OVER R2-ENTRY,   *
      * WHICH IS IN ASCENDING KEY SEQUENCE BECAUSE IT WAS LOADED OFF  *
      * A VSAM KEYED BROWSE IN P1300.  THE COMPARISON USES THE        *
      * WS-TARIFF-LOOKUP 66-LEVEL RENAME (TARIFF THRU STATE) AGAINST  *
      * A MIRROR OF THE CURRENT TABLE ROW'S KEY, WS-CURR-TABLE-KEY -  *
      * R2-EN-KEY ITSELF CANNOT CARRY ITS OWN RENAME BECAUSE CABSRT02 *
      * IS A FROZEN COPYBOOK.                                         *
      *****************************************************************
       P4100-BINARY-SEARCH.
           MOVE 1 TO WS-BS-LOW.
           MOVE R2-ENTRY-CNT TO WS-BS-HIGH.
           MOVE 'N' TO WS-BS-DONE-SW.
           PERFORM P4110-SEARCH-STEP THRU P4110-EXIT
               UNTIL WS-BS-LOW > WS-BS-HIGH OR WS-BS-DONE.
       P4100-EXIT.
           EXIT.
       P4110-SEARCH-STEP.
           COMPUTE WS-BS-MID = (WS-BS-LOW + WS-BS-HIGH) / 2.
           SET R2-EX TO WS-BS-MID.
           MOVE R2-EN-TARIFF (R2-EX) TO WS-CTK-TARIFF.
           MOVE R2-EN-ELEM (R2-EX) TO WS-CTK-ELEM.
           MOVE R2-EN-JURIS (R2-EX) TO WS-CTK-JURIS.
           MOVE R2-EN-STATE (R2-EX) TO WS-CTK-STATE.
           IF WS-TARIFF-LOOKUP = WS-CURR-TABLE-KEY
               MOVE 'Y' TO WS-BS-DONE-SW
               MOVE WS-BS-MID TO WS-BS-LANDED-SUB
           ELSE
               IF WS-TARIFF-LOOKUP < WS-CURR-TABLE-KEY
                   COMPUTE WS-BS-HIGH = WS-BS-MID - 1
               ELSE
                   COMPUTE WS-BS-LOW = WS-BS-MID + 1.
       P4110-EXIT.
           EXIT.
      *****************************************************************
      * P4200-DATE-FILTER - LANDED ON A KEY MATCH.  NOW TEST WHETHER  *
      * THAT ROW IS EFFECTIVE-DATED FOR THE CURRENT CYCLE.            *
      *****************************************************************
       P4200-DATE-FILTER.
           SET R2-EX TO WS-BS-LANDED-SUB.
           PERFORM P4250-TEST-EFF-EXP THRU P4250-EXIT.
           IF WS-DF-DATE-OK
               PERFORM P4230-COPY-RESULT THRU P4230-EXIT
               MOVE 'Y' TO WS-RR-FOUND-SW.
       P4200-EXIT.
           EXIT.
      *****************************************************************
      * P4230-COPY-RESULT - COPIES THE WINNING R2-ENTRY INTO THE      *
      * RESOLUTION RESULT AREA THAT S500 AND S700 READ FROM.          *
      *****************************************************************
       P4230-COPY-RESULT.
           MOVE R2-EN-INITIAL (R2-EX) TO WS-SEL-INIT-RATE.
           MOVE R2-EN-ADDL (R2-EX) TO WS-SEL-ADDL-RATE.
           MOVE R2-EN-ADDL (R2-EX) TO WS-SEL-RATE.
           MOVE R2-EN-SETUP (R2-EX) TO WS-SEL-SETUP-CHG.
           MOVE R2-EN-MIN-CHG (R2-EX) TO WS-SEL-MIN-CHG.
           MOVE R2-EN-MAX-CHG (R2-EX) TO WS-SEL-MAX-CHG.
           MOVE R2-EN-ROUND-RULE (R2-EX) TO WS-SEL-ROUND-RULE.
           MOVE R2-EN-ROUND-POS (R2-EX) TO WS-SEL-ROUND-POS.
           MOVE R2-EN-INIT-PERIOD (R2-EX) TO WS-SEL-INIT-PERIOD.
           MOVE R2-EN-ADDL-PERIOD (R2-EX) TO WS-SEL-ADDL-PERIOD.
           MOVE R2-EN-MODULE-SFX (R2-EX) TO WS-SEL-MODULE-SFX.
           MOVE R2-EN-BAND-CNT (R2-EX) TO WS-SEL-BAND-CNT.
           MOVE R2-EN-BAND-OFFSET (R2-EX) TO WS-SEL-BAND-OFFSET.
           MOVE R2-EX TO WS-RR-INDEX.
       P4230-EXIT.
           EXIT.
      *****************************************************************
      * P4250-TEST-EFF-EXP - CENTURY-QUALIFIED EFFECTIVE/EXPIRY DATE  *
      * COMPARISON AGAINST THE CYCLE DATE ESTABLISHED IN P1600.  THIS *
      * PARAGRAPH HARDCODES THE CENTURY PIVOT AS THE LITERAL 70       *
      * TWICE, INSTEAD OF REFERRING TO DW-PIVOT-YY FROM CABSDATE -    *
      * THE MAINTAINED PATTERN USED BY P1600.  BOTH HAVE MEANT THE    *
      * SAME THING SINCE THE 1996 Y2K REMEDIATION AND ALWAYS WILL,    *
      * BUT A FUTURE PIVOT CHANGE WOULD HAVE TO FIND THIS PLACE TOO.  *
      *****************************************************************
       P4250-TEST-EFF-EXP.
           MOVE 'N' TO WS-DF-DATE-OK-SW.
           MOVE R2-EN-EFF-YYDDD (R2-EX) TO WS-DF-EFF-YYDDD.
           MOVE R2-EN-EXP-YYDDD (R2-EX) TO WS-DF-EXP-YYDDD.
           IF WS-DF-EFF-YY < 70
               COMPUTE WS-DL-EFF-CCYY = 2000 + WS-DF-EFF-YY
           ELSE
               COMPUTE WS-DL-EFF-CCYY = 1900 + WS-DF-EFF-YY.
           COMPUTE WS-DF-EFF-CCYYDDD =
               (WS-DL-EFF-CCYY * 1000) + WS-DF-EFF-DDD.
           IF WS-DF-EXP-YYDDD = 0
               MOVE 9999999 TO WS-DF-EXP-CCYYDDD.
           IF WS-DF-EXP-YYDDD NOT = 0 AND WS-DF-EXP-YY < 70
               COMPUTE WS-DL-EXP-CCYY = 2000 + WS-DF-EXP-YY
               COMPUTE WS-DF-EXP-CCYYDDD =
                   (WS-DL-EXP-CCYY * 1000) + WS-DF-EXP-DDD.
           IF WS-DF-EXP-YYDDD NOT = 0 AND WS-DF-EXP-YY NOT < 70
               COMPUTE WS-DL-EXP-CCYY = 1900 + WS-DF-EXP-YY
               COMPUTE WS-DF-EXP-CCYYDDD =
                   (WS-DL-EXP-CCYY * 1000) + WS-DF-EXP-DDD.
           IF WS-DF-EFF-CCYYDDD NOT > WS-EV-CYCLE-CCYYDDD AND
                   WS-DF-EXP-CCYYDDD NOT < WS-EV-CYCLE-CCYYDDD
               MOVE 'Y' TO WS-DF-DATE-OK-SW.
       P4250-EXIT.
           EXIT.
      *****************************************************************
      * P4300-TRY-JURIS-GENERIC - DROP STATE SPECIFICITY AND RE-       *
      * SEARCH.  USED FOR TARIFF ELEMENTS THAT ARE PRICED THE SAME    *
      * ACROSS EVERY STATE IN A JURISDICTION.                         *
      *****************************************************************
       P4300-TRY-JURIS-GENERIC.
           MOVE WS-RK-STATE TO WS-DF-SAVE-STATE.
           MOVE SPACES TO WS-RK-STATE.
           PERFORM P4100-BINARY-SEARCH THRU P4100-EXIT.
           IF WS-BS-DONE
               PERFORM P4200-DATE-FILTER THRU P4200-EXIT.
           IF WS-RR-FOUND
               MOVE '2' TO WS-RR-FALLBACK-LVL.
           MOVE WS-DF-SAVE-STATE TO WS-RK-STATE.
       P4300-EXIT.
           EXIT.
      *****************************************************************
      * P4400-TRY-TARIFF-DEFAULT - DROP JURISDICTION AND STATE, KEEP  *
      * ONLY TARIFF AND ELEMENT.  THE LAST RESORT BEFORE A SOFT       *
      * REJECT (OR, FOR ORIGAC AND TANSW, A FATAL ABEND - SEE P5100   *
      * AND P5400).                                                   *
      *****************************************************************
       P4400-TRY-TARIFF-DEFAULT.
           MOVE WS-RK-STATE TO WS-DF-SAVE-STATE.
           MOVE WS-RK-JURIS TO WS-DF-SAVE-JURIS.
           MOVE SPACES TO WS-RK-STATE.
           MOVE SPACES TO WS-RK-JURIS.
           PERFORM P4100-BINARY-SEARCH THRU P4100-EXIT.
           IF WS-BS-DONE
               PERFORM P4200-DATE-FILTER THRU P4200-EXIT.
           IF WS-RR-FOUND
               MOVE '3' TO WS-RR-FALLBACK-LVL.
           MOVE WS-DF-SAVE-STATE TO WS-RK-STATE.
           MOVE WS-DF-SAVE-JURIS TO WS-RK-JURIS.
       P4400-EXIT.
           EXIT.
      *****************************************************************
      * P4900-ROUND-AMOUNT - SHARED ROUNDING UTILITY DRIVEN BY        *
      * R4-RULE (ULTIMATELY FROM RT-ROUND-RULE ON THE RATE TABLE).    *
      * CALLED WITH R4-RAW-AMT / R4-RULE ALREADY SET BY THE CALLER;   *
      * RETURNS R4-ROUNDED-AMT AND ACCUMULATES R4-RESIDUE-ACC.        *
      *****************************************************************
       P4900-ROUND-AMOUNT.
           MOVE 1 TO WS-RD-RULE-INDEX.
           IF R4-RULE = 'E'
               MOVE 2 TO WS-RD-RULE-INDEX.
           IF R4-RULE = 'T'
               MOVE 3 TO WS-RD-RULE-INDEX.
           IF R4-RULE = 'C'
               MOVE 4 TO WS-RD-RULE-INDEX.
           GO TO P4910-ROUND-UP P4920-ROUND-EVEN P4930-TRUNCATE
               P4940-ROUND-CEILING DEPENDING ON WS-RD-RULE-INDEX.
       P4910-ROUND-UP.
           COMPUTE R4-ROUNDED-AMT ROUNDED = R4-RAW-AMT.
           GO TO P4950-FINISH-ROUND.
      *    THE 'E' (HALF-TO-EVEN) RULE HAS NEVER BEEN IMPLEMENTED
      *    DIFFERENTLY FROM 'U' - THE BANKER'S ROUNDING ENHANCEMENT
      *    REQUESTED IN THE 1996 TARIFF REVIEW WAS NEVER FUNDED.
       P4920-ROUND-EVEN.
           COMPUTE R4-ROUNDED-AMT ROUNDED = R4-RAW-AMT.
           GO TO P4950-FINISH-ROUND.
       P4930-TRUNCATE.
           COMPUTE R4-ROUNDED-AMT = R4-RAW-AMT.
           GO TO P4950-FINISH-ROUND.
       P4940-ROUND-CEILING.
           COMPUTE R4-ROUNDED-AMT = R4-RAW-AMT.
           COMPUTE R4-RESIDUE = R4-RAW-AMT - R4-ROUNDED-AMT.
           IF R4-RESIDUE > 0
               ADD 0.01 TO R4-ROUNDED-AMT.
       P4950-FINISH-ROUND.
           COMPUTE R4-RESIDUE = R4-RAW-AMT - R4-ROUNDED-AMT.
           ADD R4-RESIDUE TO R4-RESIDUE-ACC.
       P4900-EXIT.
           EXIT.
      *****************************************************************
      * S500-ELEMENT-RATING SECTION - ONE PARAGRAPH GROUP PER RATE    *
      * ELEMENT.  EACH GROUP COMPUTES BILLABLE MINUTES FROM THE       *
      * INITIAL/ADDITIONAL PERIOD FIELDS ON ITS OWN, THEN APPLIES THE *
      * RESOLVED RATE, THEN TESTS MINIMUM AND MAXIMUM CHARGE.         *
      *****************************************************************
       S500-ELEMENT-RATING SECTION.
      *****************************************************************
      * P5100 GROUP - ORIGINATING ACCESS ('ORIGAC').  THE GO TO       *
      * BELOW IS ONE OF THREE PATHS INTO P9990-RATE-FAILURE.          *
      *****************************************************************
       P5100-RATE-ORIG-ACCESS.
           IF WS-RR-INDEX < 1 OR WS-RR-INDEX > R2-ENTRY-CNT
               GO TO P9990-RATE-FAILURE.
           PERFORM P5110-OA-COMPUTE-MINUTES THRU P5110-EXIT.
           PERFORM P5120-OA-APPLY-RATE THRU P5120-EXIT.
       P5100-EXIT.
           EXIT.
       P5110-OA-COMPUTE-MINUTES.
           COMPUTE WS-CONV-SECONDS = WS-CW-CONV-MIN * 60.
           IF WS-CONV-SECONDS NOT > WS-SEL-INIT-PERIOD
               MOVE WS-SEL-INIT-PERIOD TO WS-CONV-BILL-SECONDS
           ELSE
               COMPUTE WS-CONV-ADDL-SECONDS =
                   WS-CONV-SECONDS - WS-SEL-INIT-PERIOD
               DIVIDE WS-CONV-ADDL-SECONDS BY WS-SEL-ADDL-PERIOD
                   GIVING WS-CONV-ADDL-PERIODS
                   REMAINDER WS-CONV-ADDL-REM
               PERFORM P5115-OA-BUMP-PERIOD THRU P5115-EXIT
               COMPUTE WS-CONV-BILL-SECONDS = WS-SEL-INIT-PERIOD +
                   (WS-CONV-ADDL-PERIODS * WS-SEL-ADDL-PERIOD).
           COMPUTE WS-BILLABLE-MIN ROUNDED = WS-CONV-BILL-SECONDS / 60.
       P5110-EXIT.
           EXIT.
       P5115-OA-BUMP-PERIOD.
           IF WS-CONV-ADDL-REM > 0
               ADD 1 TO WS-CONV-ADDL-PERIODS.
       P5115-EXIT.
           EXIT.
       P5120-OA-APPLY-RATE.
           COMPUTE WS-ELEM-AMOUNT ROUNDED =
               WS-BILLABLE-MIN * WS-SEL-RATE.
           PERFORM P5130-OA-MIN-MAX-TEST THRU P5130-EXIT.
           MOVE WS-SEL-RATE TO R4-EDIT-RATE.
           MOVE R4-EDIT-RATE TO WS-DESC-FRAG5.
           MOVE 'INITONLY' TO WS-DESC-FRAG6.
           IF WS-CONV-ADDL-PERIODS > 0
               MOVE 'PLUSADDL' TO WS-DESC-FRAG6.
       P5120-EXIT.
           EXIT.
       P5130-OA-MIN-MAX-TEST.
           MOVE ' ' TO WS-MIN-MAX-APPLIED-SW.
           IF WS-SEL-MIN-CHG > 0 AND WS-ELEM-AMOUNT < WS-SEL-MIN-CHG
               MOVE WS-SEL-MIN-CHG TO WS-ELEM-AMOUNT
               MOVE 'N' TO WS-MIN-MAX-APPLIED-SW.
           IF WS-SEL-MAX-CHG > 0 AND WS-ELEM-AMOUNT > WS-SEL-MAX-CHG
               MOVE WS-SEL-MAX-CHG TO WS-ELEM-AMOUNT
               MOVE 'X' TO WS-MIN-MAX-APPLIED-SW.
       P5130-EXIT.
           EXIT.
      *****************************************************************
      * P5200 GROUP - TERMINATING ACCESS ('TERMAC').  A RATE-NOT-     *
      * FOUND HERE IS A SOFT REJECT (SEE P2820), NOT A HARD ABEND -   *
      * THAT BEHAVIOUR WAS ADDED IN V2.10 (SEE REVISION HISTORY).     *
      *****************************************************************
       P5200-RATE-TERM-ACCESS.
           PERFORM P5210-TA-COMPUTE-MINUTES THRU P5210-EXIT.
           PERFORM P5220-TA-APPLY-RATE THRU P5220-EXIT.
       P5200-EXIT.
           EXIT.
       P5210-TA-COMPUTE-MINUTES.
           COMPUTE WS-CONV-SECONDS = WS-CW-CONV-MIN * 60.
           IF WS-CONV-SECONDS NOT > WS-SEL-INIT-PERIOD
               MOVE WS-SEL-INIT-PERIOD TO WS-CONV-BILL-SECONDS
           ELSE
               COMPUTE WS-CONV-ADDL-SECONDS =
                   WS-CONV-SECONDS - WS-SEL-INIT-PERIOD
               DIVIDE WS-CONV-ADDL-SECONDS BY WS-SEL-ADDL-PERIOD
                   GIVING WS-CONV-ADDL-PERIODS
                   REMAINDER WS-CONV-ADDL-REM
               PERFORM P5215-TA-BUMP-PERIOD THRU P5215-EXIT
               COMPUTE WS-CONV-BILL-SECONDS = WS-SEL-INIT-PERIOD +
                   (WS-CONV-ADDL-PERIODS * WS-SEL-ADDL-PERIOD).
           COMPUTE WS-BILLABLE-MIN ROUNDED = WS-CONV-BILL-SECONDS / 60.
       P5210-EXIT.
           EXIT.
       P5215-TA-BUMP-PERIOD.
           IF WS-CONV-ADDL-REM > 0
               ADD 1 TO WS-CONV-ADDL-PERIODS.
       P5215-EXIT.
           EXIT.
       P5220-TA-APPLY-RATE.
           COMPUTE WS-ELEM-AMOUNT ROUNDED =
               WS-BILLABLE-MIN * WS-SEL-RATE.
           PERFORM P5230-TA-MIN-MAX-TEST THRU P5230-EXIT.
           MOVE WS-SEL-RATE TO R4-EDIT-RATE.
           MOVE R4-EDIT-RATE TO WS-DESC-FRAG5.
           MOVE 'INITONLY' TO WS-DESC-FRAG6.
           IF WS-CONV-ADDL-PERIODS > 0
               MOVE 'PLUSADDL' TO WS-DESC-FRAG6.
       P5220-EXIT.
           EXIT.
       P5230-TA-MIN-MAX-TEST.
           MOVE ' ' TO WS-MIN-MAX-APPLIED-SW.
           IF WS-SEL-MIN-CHG > 0 AND WS-ELEM-AMOUNT < WS-SEL-MIN-CHG
               MOVE WS-SEL-MIN-CHG TO WS-ELEM-AMOUNT
               MOVE 'N' TO WS-MIN-MAX-APPLIED-SW.
           IF WS-SEL-MAX-CHG > 0 AND WS-ELEM-AMOUNT > WS-SEL-MAX-CHG
               MOVE WS-SEL-MAX-CHG TO WS-ELEM-AMOUNT
               MOVE 'X' TO WS-MIN-MAX-APPLIED-SW.
       P5230-EXIT.
           EXIT.
      *****************************************************************
      * P5300 GROUP - LOCAL TRANSPORT ('LTRANS').  PER MINUTE PER     *
      * MILE, DISTANCE-SENSITIVE.  CALLS S600 FOR THE V AND H         *
      * MILEAGE CALCULATION AND BANDING BEFORE COMPUTING MINUTES.     *
      *****************************************************************
       P5300-RATE-LOCAL-TRANSPORT.
           PERFORM P6100-GET-VH-COORDS THRU P6100-EXIT.
           PERFORM P6300-COMPUTE-MILEAGE THRU P6300-EXIT.
           PERFORM P6400-BAND-MILEAGE THRU P6400-EXIT.
           PERFORM P5310-LT-COMPUTE-MINUTES THRU P5310-EXIT.
           PERFORM P5320-LT-APPLY-RATE THRU P5320-EXIT.
       P5300-EXIT.
           EXIT.
       P5310-LT-COMPUTE-MINUTES.
           COMPUTE WS-CONV-SECONDS = WS-CW-CONV-MIN * 60.
           IF WS-CONV-SECONDS NOT > WS-SEL-INIT-PERIOD
               MOVE WS-SEL-INIT-PERIOD TO WS-CONV-BILL-SECONDS
           ELSE
               COMPUTE WS-CONV-ADDL-SECONDS =
                   WS-CONV-SECONDS - WS-SEL-INIT-PERIOD
               DIVIDE WS-CONV-ADDL-SECONDS BY WS-SEL-ADDL-PERIOD
                   GIVING WS-CONV-ADDL-PERIODS
                   REMAINDER WS-CONV-ADDL-REM
               PERFORM P5315-LT-BUMP-PERIOD THRU P5315-EXIT
               COMPUTE WS-CONV-BILL-SECONDS = WS-SEL-INIT-PERIOD +
                   (WS-CONV-ADDL-PERIODS * WS-SEL-ADDL-PERIOD).
           COMPUTE WS-BILLABLE-MIN ROUNDED = WS-CONV-BILL-SECONDS / 60.
       P5310-EXIT.
           EXIT.
       P5315-LT-BUMP-PERIOD.
           IF WS-CONV-ADDL-REM > 0
               ADD 1 TO WS-CONV-ADDL-PERIODS.
       P5315-EXIT.
           EXIT.
      *****************************************************************
      * P5320-LT-APPLY-RATE - THE ONLY ELEMENT WHERE THE PER-MINUTE   *
      * RATE COMES FROM THE MILEAGE BAND SEARCH (WS-MB-SEL-RATE),     *
      * NOT DIRECTLY FROM WS-SEL-RATE, AND WHERE THE AMOUNT IS ALSO   *
      * SCALED BY THE MILES THEMSELVES.                               *
      *****************************************************************
       P5320-LT-APPLY-RATE.
           IF WS-MB-FOUND
               MOVE WS-MB-SEL-RATE TO WS-SEL-RATE
           ELSE
               MOVE WS-SEL-ADDL-RATE TO WS-SEL-RATE.
           COMPUTE WS-ELEM-AMOUNT ROUNDED =
               WS-BILLABLE-MIN * WS-SEL-RATE * WS-MW-MILES.
           PERFORM P5330-LT-MIN-MAX-TEST THRU P5330-EXIT.
           MOVE WS-SEL-RATE TO R4-EDIT-RATE.
           MOVE R4-EDIT-RATE TO WS-DESC-FRAG5.
           MOVE 'PERMILE ' TO WS-DESC-FRAG6.
       P5320-EXIT.
           EXIT.
       P5330-LT-MIN-MAX-TEST.
           MOVE ' ' TO WS-MIN-MAX-APPLIED-SW.
           IF WS-SEL-MIN-CHG > 0 AND WS-ELEM-AMOUNT < WS-SEL-MIN-CHG
               MOVE WS-SEL-MIN-CHG TO WS-ELEM-AMOUNT
               MOVE 'N' TO WS-MIN-MAX-APPLIED-SW.
           IF WS-SEL-MAX-CHG > 0 AND WS-ELEM-AMOUNT > WS-SEL-MAX-CHG
               MOVE WS-SEL-MAX-CHG TO WS-ELEM-AMOUNT
               MOVE 'X' TO WS-MIN-MAX-APPLIED-SW.
       P5330-EXIT.
           EXIT.
      *****************************************************************
      * P5400 GROUP - TANDEM SWITCHING ('TANSW ').  APPLIES ONLY WHEN *
      * CD-VC-TANDEM-IND IS 'Y' (ENFORCED BY P2410 BEFORE THIS GROUP  *
      * IS EVER REACHED).  A MISSING TANDEM RATE IS FATAL - TANDEM    *
      * SWITCHING CANNOT BE ESTIMATED OR DEFAULTED.                   *
      *****************************************************************
       P5400-RATE-TANDEM.
           IF WS-SEL-RATE = 0
               GO TO P9990-RATE-FAILURE.
           MOVE 1 TO WS-TANDEM-COUNT.
           PERFORM P5410-TS-COMPUTE-MINUTES THRU P5410-EXIT.
           PERFORM P5420-TS-APPLY-RATE THRU P5420-EXIT.
       P5400-EXIT.
           EXIT.
       P5410-TS-COMPUTE-MINUTES.
           COMPUTE WS-CONV-SECONDS = WS-CW-CONV-MIN * 60.
           IF WS-CONV-SECONDS NOT > WS-SEL-INIT-PERIOD
               MOVE WS-SEL-INIT-PERIOD TO WS-CONV-BILL-SECONDS
           ELSE
               COMPUTE WS-CONV-ADDL-SECONDS =
                   WS-CONV-SECONDS - WS-SEL-INIT-PERIOD
               DIVIDE WS-CONV-ADDL-SECONDS BY WS-SEL-ADDL-PERIOD
                   GIVING WS-CONV-ADDL-PERIODS
                   REMAINDER WS-CONV-ADDL-REM
               PERFORM P5415-TS-BUMP-PERIOD THRU P5415-EXIT
               COMPUTE WS-CONV-BILL-SECONDS = WS-SEL-INIT-PERIOD +
                   (WS-CONV-ADDL-PERIODS * WS-SEL-ADDL-PERIOD).
           COMPUTE WS-BILLABLE-MIN ROUNDED = WS-CONV-BILL-SECONDS / 60.
       P5410-EXIT.
           EXIT.
       P5415-TS-BUMP-PERIOD.
           IF WS-CONV-ADDL-REM > 0
               ADD 1 TO WS-CONV-ADDL-PERIODS.
       P5415-EXIT.
           EXIT.
      *****************************************************************
      * P5420-TS-APPLY-RATE - NOTE THE PLAIN COMPUTE BELOW, WITH NO   *
      * ROUNDED KEYWORD, UNLIKE THE EQUIVALENT LINE IN EVERY OTHER    *
      * ELEMENT GROUP IN THIS PROGRAM.  WS-ELEM-AMOUNT IS DEFINED     *
      * S9(11)V9(05) SO THE COMPUTE TRUNCATES AT FIVE DECIMAL PLACES  *
      * INSTEAD OF ROUNDING - THE SUB-CENT DRIFT IS SMALL PER CALL    *
      * BUT ACCUMULATES ACROSS THE FULL TANDEM VOLUME OF THE ESTATE.  *
      *****************************************************************
       P5420-TS-APPLY-RATE.
           COMPUTE WS-ELEM-AMOUNT =
               WS-BILLABLE-MIN * WS-SEL-RATE * WS-TANDEM-COUNT.
           PERFORM P5430-TS-MIN-MAX-TEST THRU P5430-EXIT.
           MOVE WS-SEL-RATE TO R4-EDIT-RATE.
           MOVE R4-EDIT-RATE TO WS-DESC-FRAG5.
           MOVE 'PERTANDM' TO WS-DESC-FRAG6.
       P5420-EXIT.
           EXIT.
       P5430-TS-MIN-MAX-TEST.
           MOVE ' ' TO WS-MIN-MAX-APPLIED-SW.
           IF WS-SEL-MIN-CHG > 0 AND WS-ELEM-AMOUNT < WS-SEL-MIN-CHG
               MOVE WS-SEL-MIN-CHG TO WS-ELEM-AMOUNT
               MOVE 'N' TO WS-MIN-MAX-APPLIED-SW.
           IF WS-SEL-MAX-CHG > 0 AND WS-ELEM-AMOUNT > WS-SEL-MAX-CHG
               MOVE WS-SEL-MAX-CHG TO WS-ELEM-AMOUNT
               MOVE 'X' TO WS-MIN-MAX-APPLIED-SW.
       P5430-EXIT.
           EXIT.
      *****************************************************************
      * P5500 GROUP - CARRIER COMMON LINE ('CCLINE').  INTERSTATE     *
      * ONLY (ENFORCED BY P2410), WITH A PREMIUM/NON-PREMIUM RATE     *
      * DISTINCTION DRIVEN BY WS-CR-CCL-ELIGIBLE-SW AND CARRIER TYPE. *
      * WHETHER THE ROW SURVIVES TO BDTLOUT AT ALL IS DECIDED LATER,  *
      * IN P3100 - THIS PARAGRAPH RATES EVERY CCLINE CANDIDATE THE    *
      * SAME WAY REGARDLESS OF ELIGIBILITY.                           *
      *                                                                *
      * NOTE ON THE PREMIUM/NON-PREMIUM SPLIT - SEE V2.00 (P.NAIR,    *
      * 2001) IN THE REVISION HISTORY ABOVE, WHICH RECORDS THAT THE   *
      * PREMIUM/NON-PREMIUM DISTINCTION WAS REMOVED IN FAVOUR OF A    *
      * SINGLE FLAT CCL RATE.  THE WS-CCL-PREMIUM-SW TEST BELOW HAS   *
      * BEEN LEFT IN PLACE BECAUSE P5520-CC-APPLY-RATE STILL BRANCHES *
      * ON IT WHEN SELECTING BETWEEN THE PREMIUM AND STANDARD ROWS OF *
      * THE CCLINE RATE TABLE ENTRY - REMOVING THE SWITCH WITHOUT     *
      * ALSO COLLAPSING THE TWO RATE ROWS WOULD HAVE LEFT THE         *
      * PREMIUM ROW ORPHANED, SO THE CODE PATH WAS NEVER ACTUALLY     *
      * TAKEN OUT.  IXC CARRIERS (WS-CR-TYPE = 'I') STILL RATE AT THE *
      * PREMIUM CCL RATE; ALL OTHER CARRIER TYPES RATE STANDARD.      *
      *****************************************************************
       P5500-RATE-CCL.
           MOVE 'N' TO WS-CCL-PREMIUM-SW.
           IF WS-CR-TYPE = 'I'
               MOVE 'Y' TO WS-CCL-PREMIUM-SW.
           PERFORM P5510-CC-COMPUTE-MINUTES THRU P5510-EXIT.
           PERFORM P5520-CC-APPLY-RATE THRU P5520-EXIT.
       P5500-EXIT.
           EXIT.
       P5510-CC-COMPUTE-MINUTES.
           COMPUTE WS-CONV-SECONDS = WS-CW-CONV-MIN * 60.
           IF WS-CONV-SECONDS NOT > WS-SEL-INIT-PERIOD
               MOVE WS-SEL-INIT-PERIOD TO WS-CONV-BILL-SECONDS
           ELSE
               COMPUTE WS-CONV-ADDL-SECONDS =
                   WS-CONV-SECONDS - WS-SEL-INIT-PERIOD
               DIVIDE WS-CONV-ADDL-SECONDS BY WS-SEL-ADDL-PERIOD
                   GIVING WS-CONV-ADDL-PERIODS
                   REMAINDER WS-CONV-ADDL-REM
               PERFORM P5515-CC-BUMP-PERIOD THRU P5515-EXIT
               COMPUTE WS-CONV-BILL-SECONDS = WS-SEL-INIT-PERIOD +
                   (WS-CONV-ADDL-PERIODS * WS-SEL-ADDL-PERIOD).
           COMPUTE WS-BILLABLE-MIN ROUNDED = WS-CONV-BILL-SECONDS / 60.
       P5510-EXIT.
           EXIT.
       P5515-CC-BUMP-PERIOD.
           IF WS-CONV-ADDL-REM > 0
               ADD 1 TO WS-CONV-ADDL-PERIODS.
       P5515-EXIT.
           EXIT.
       P5520-CC-APPLY-RATE.
           IF WS-CCL-IS-PREMIUM
               MOVE WS-SEL-ADDL-RATE TO WS-SEL-RATE
           ELSE
               MOVE WS-SEL-INIT-RATE TO WS-SEL-RATE.
           COMPUTE WS-ELEM-AMOUNT ROUNDED =
               WS-BILLABLE-MIN * WS-SEL-RATE.
           PERFORM P5530-CC-MIN-MAX-TEST THRU P5530-EXIT.
           MOVE WS-SEL-RATE TO R4-EDIT-RATE.
           MOVE R4-EDIT-RATE TO WS-DESC-FRAG5.
           MOVE 'CCLRATE ' TO WS-DESC-FRAG6.
       P5520-EXIT.
           EXIT.
       P5530-CC-MIN-MAX-TEST.
           MOVE ' ' TO WS-MIN-MAX-APPLIED-SW.
           IF WS-SEL-MIN-CHG > 0 AND WS-ELEM-AMOUNT < WS-SEL-MIN-CHG
               MOVE WS-SEL-MIN-CHG TO WS-ELEM-AMOUNT
               MOVE 'N' TO WS-MIN-MAX-APPLIED-SW.
           IF WS-SEL-MAX-CHG > 0 AND WS-ELEM-AMOUNT > WS-SEL-MAX-CHG
               MOVE WS-SEL-MAX-CHG TO WS-ELEM-AMOUNT
               MOVE 'X' TO WS-MIN-MAX-APPLIED-SW.
       P5530-EXIT.
           EXIT.
      *****************************************************************
      * S600-MILEAGE SECTION - V AND H COORDINATE LOOKUP, AIRLINE     *
      * MILEAGE BY THE STANDARD BELLCORE FORMULA, AND BANDING.        *
      * OS/VS COBOL HAS NO SQRT FUNCTION, SO THE SQUARE ROOT IS DONE  *
      * LONGHAND BY NEWTON'S METHOD - A REAL ITERATIVE APPROXIMATION, *
      * NOT A LOOKUP TABLE.                                           *
      *                                                                *
      * HISTORY NOTE (S.MARCHETTI, 2011) - INTERNAL AUDIT FINDING     *
      * 2010-118 FLAGGED THAT THE ORIGINAL CONVERGENCE TOLERANCE OF   *
      * 0.05 MILES (SET IN 1988 WHEN THIS ROUTINE WAS FIRST WRITTEN)  *
      * COULD, IN RARE CASES NEAR A MILEAGE BAND BOUNDARY, LAND ON    *
      * THE WRONG SIDE OF THE BOUNDARY AND SELECT AN ADJACENT BAND'S  *
      * RATE INSTEAD OF THE CORRECT ONE.  THE FIX WAS SIMPLY TO       *
      * TIGHTEN WS-MW-CONVERGE-TOL TO 0.005 MILES (SEE THE VALUE      *
      * CLAUSE IN WORKING STORAGE) - THE ITERATION COUNT NEEDED TO    *
      * CONVERGE BARELY CHANGED, SO THERE WAS NO MEASURABLE CPU       *
      * IMPACT.  THE MAXIMUM ITERATION CAP OF 40 (WS-MW-MAX-          *
      * ITERATIONS) HAS NEVER BEEN HIT IN PRODUCTION SINCE THIS       *
      * ROUTINE WENT LIVE - IF IT EVER IS, SOMETHING IS SERIOUSLY     *
      * WRONG WITH THE V&H DATA, NOT WITH THE MATH, WHICH IS WHY      *
      * P6300 TREATS NON-CONVERGENCE AS FATAL RATHER THAN RETRYING.   *
      *****************************************************************
       S600-MILEAGE SECTION.
      *****************************************************************
      * P6100-GET-VH-COORDS - LINEAR SCAN OF THE 70-ROW V&H TABLE FOR *
      * THE ORIGINATING AND TERMINATING LATA.  SMALL TABLE, NO NEED   *
      * FOR ANYTHING FANCIER THAN A FULL SCAN EACH WAY.               *
      *****************************************************************
       P6100-GET-VH-COORDS.
           MOVE 'N' TO WS-MW-VH-FOUND-SW.
           MOVE 0 TO WS-VH-ORIG-V.
           MOVE 0 TO WS-VH-ORIG-H.
           MOVE 0 TO WS-VH-TERM-V.
           MOVE 0 TO WS-VH-TERM-H.
           PERFORM P6110-FIND-ORIG-VH THRU P6110-EXIT
               VARYING WS-VH-X FROM 1 BY 1
               UNTIL WS-VH-X > WS-VH-CNT.
           PERFORM P6120-FIND-TERM-VH THRU P6120-EXIT
               VARYING WS-VH-X FROM 1 BY 1
               UNTIL WS-VH-X > WS-VH-CNT.
           IF WS-VH-ORIG-V > 0 AND WS-VH-TERM-V > 0
               ADD 1 TO WS-MC-VH-EXACT-CNT
           ELSE
               IF WS-VH-ORIG-V = 0
                   PERFORM P6130-FALLBACK-ORIG-VH THRU P6130-EXIT.
               IF WS-VH-TERM-V = 0
                   PERFORM P6140-FALLBACK-TERM-VH THRU P6140-EXIT.
               IF WS-VH-ORIG-V > 0 AND WS-VH-TERM-V > 0
                   ADD 1 TO WS-MC-VH-FALLBACK-CNT.
           IF WS-VH-ORIG-V > 0 AND WS-VH-TERM-V > 0
               MOVE 'Y' TO WS-MW-VH-FOUND-SW.
       P6100-EXIT.
           EXIT.
       P6110-FIND-ORIG-VH.
           IF WS-VH-LATA (WS-VH-X) = WS-CW-ORIG-LATA-X
               MOVE WS-VH-VCOORD (WS-VH-X) TO WS-VH-ORIG-V
               MOVE WS-VH-HCOORD (WS-VH-X) TO WS-VH-ORIG-H.
       P6110-EXIT.
           EXIT.
       P6120-FIND-TERM-VH.
           IF WS-VH-LATA (WS-VH-X) = WS-CW-TERM-LATA-X
               MOVE WS-VH-VCOORD (WS-VH-X) TO WS-VH-TERM-V
               MOVE WS-VH-HCOORD (WS-VH-X) TO WS-VH-TERM-H.
       P6120-EXIT.
           EXIT.
      *****************************************************************
      * P6130-FALLBACK-ORIG-VH - THE EXACT LATA WAS NOT IN THE V&H    *
      * TABLE.  RATHER THAN FAIL THE MILEAGE COMPUTATION OUTRIGHT,    *
      * FALL BACK TO ANY OTHER LATA IN THE SAME STATE (FROM THE       *
      * STATE/LATA XREF) THAT DOES HAVE A V&H ENTRY - A COARSE BUT    *
      * SERVICEABLE APPROXIMATION FOR IN-STATE MILEAGE BANDING.       *
      *****************************************************************
       P6130-FALLBACK-ORIG-VH.
           PERFORM P6132-SCAN-STATE-LATA THRU P6132-EXIT
               VARYING WS-LX-X FROM 1 BY 1
               UNTIL WS-LX-X > WS-LX-CNT OR WS-VH-ORIG-V > 0.
       P6130-EXIT.
           EXIT.
       P6132-SCAN-STATE-LATA.
           IF WS-LX-STATE (WS-LX-X) = WS-CW-STATE-CD AND
                   WS-VH-ORIG-V = 0
               PERFORM P6134-TRY-LATA-FOR-ORIG-VH THRU P6134-EXIT.
       P6132-EXIT.
           EXIT.
       P6134-TRY-LATA-FOR-ORIG-VH.
           PERFORM P6136-SCAN-VH-FOR-ORIG-LATA THRU P6136-EXIT
               VARYING WS-VH-X FROM 1 BY 1
               UNTIL WS-VH-X > WS-VH-CNT OR WS-VH-ORIG-V > 0.
       P6134-EXIT.
           EXIT.
       P6136-SCAN-VH-FOR-ORIG-LATA.
           IF WS-VH-LATA (WS-VH-X) = WS-LX-LATA (WS-LX-X) AND
                   WS-VH-ORIG-V = 0
               MOVE WS-VH-VCOORD (WS-VH-X) TO WS-VH-ORIG-V
               MOVE WS-VH-HCOORD (WS-VH-X) TO WS-VH-ORIG-H.
       P6136-EXIT.
           EXIT.
      *****************************************************************
      * P6140-FALLBACK-TERM-VH - THE MIRROR OF P6130 FOR THE           *
      * TERMINATING LATA.                                             *
      *****************************************************************
       P6140-FALLBACK-TERM-VH.
           PERFORM P6142-SCAN-STATE-LATA THRU P6142-EXIT
               VARYING WS-LX-X FROM 1 BY 1
               UNTIL WS-LX-X > WS-LX-CNT OR WS-VH-TERM-V > 0.
       P6140-EXIT.
           EXIT.
       P6142-SCAN-STATE-LATA.
           IF WS-LX-STATE (WS-LX-X) = WS-CW-STATE-CD AND
                   WS-VH-TERM-V = 0
               PERFORM P6144-TRY-LATA-FOR-TERM-VH THRU P6144-EXIT.
       P6142-EXIT.
           EXIT.
       P6144-TRY-LATA-FOR-TERM-VH.
           PERFORM P6146-SCAN-VH-FOR-TERM-LATA THRU P6146-EXIT
               VARYING WS-VH-X FROM 1 BY 1
               UNTIL WS-VH-X > WS-VH-CNT OR WS-VH-TERM-V > 0.
       P6144-EXIT.
           EXIT.
       P6146-SCAN-VH-FOR-TERM-LATA.
           IF WS-VH-LATA (WS-VH-X) = WS-LX-LATA (WS-LX-X) AND
                   WS-VH-TERM-V = 0
               MOVE WS-VH-VCOORD (WS-VH-X) TO WS-VH-TERM-V
               MOVE WS-VH-HCOORD (WS-VH-X) TO WS-VH-TERM-H.
       P6146-EXIT.
           EXIT.
      *****************************************************************
      * P6200-COMPUTE-DIFFERENCES - THE (V1-V2)**2 + (H1-H2)**2 PART  *
      * OF THE BELLCORE V AND H FORMULA, DIVIDED BY 10.               *
      *****************************************************************
       P6200-COMPUTE-DIFFERENCES.
           COMPUTE WS-MW-V-DIFF = WS-VH-ORIG-V - WS-VH-TERM-V.
           COMPUTE WS-MW-H-DIFF = WS-VH-ORIG-H - WS-VH-TERM-H.
           COMPUTE WS-MW-V-DIFF-SQ = WS-MW-V-DIFF * WS-MW-V-DIFF.
           COMPUTE WS-MW-H-DIFF-SQ = WS-MW-H-DIFF * WS-MW-H-DIFF.
           COMPUTE WS-MW-SUM-SQ = WS-MW-V-DIFF-SQ + WS-MW-H-DIFF-SQ.
           COMPUTE WS-MW-RADICAND ROUNDED = WS-MW-SUM-SQ / 10.
       P6200-EXIT.
           EXIT.
      *****************************************************************
      * P6300-COMPUTE-MILEAGE - THE SQUARE ROOT OF WS-MW-RADICAND,    *
      * BY NEWTON'S METHOD, STARTING FROM RADICAND / 2.  IF THE       *
      * ITERATION HAS NOT CONVERGED WITHIN WS-MW-MAX-ITERATIONS       *
      * PASSES, THIS IS THE THIRD AND LAST OF THE THREE GO TO PATHS   *
      * INTO P9990-RATE-FAILURE.                                      *
      *****************************************************************
       P6300-COMPUTE-MILEAGE.
           PERFORM P6200-COMPUTE-DIFFERENCES THRU P6200-EXIT.
           MOVE 0 TO WS-MW-ITERATION-CNT.
           MOVE 'N' TO WS-MW-CONVERGED-SW.
           IF WS-MW-RADICAND = 0
               MOVE 0 TO WS-MW-ROOT-EST
               MOVE 'Y' TO WS-MW-CONVERGED-SW
           ELSE
               COMPUTE WS-MW-ROOT-EST = WS-MW-RADICAND / 2
               PERFORM P6310-NEWTON-STEP THRU P6310-EXIT
                   UNTIL WS-MW-CONVERGED OR
                       WS-MW-ITERATION-CNT > WS-MW-MAX-ITERATIONS.
           IF NOT WS-MW-CONVERGED
               GO TO P9990-RATE-FAILURE.
           MOVE WS-MW-ROOT-EST TO WS-MW-MILES.
      *        SANITY CEILING - LONGEST PLAUSIBLE CONTINENTAL V&H
      *        AIRLINE DISTANCE IS WELL UNDER 3500 MILES.  A COMPUTED
      *        DISTANCE OVER THAT POINTS TO A BAD COORDINATE PAIR, NOT
      *        A REAL CALL, BUT IT STILL BANDS AND RATES NORMALLY -
      *        THIS ONLY DRIVES THE EXCEPTION COUNT ON P8700.
           IF WS-MW-MILES > 3500
               ADD 1 TO WS-MW-CEILING-CNT.
       P6300-EXIT.
           EXIT.
      *****************************************************************
      * P6310-NEWTON-STEP - ONE PASS: X(N+1) = (X(N) + R/X(N)) / 2.   *
      * CONVERGED WHEN THE ABSOLUTE STEP SIZE DROPS BELOW THE         *
      * TOLERANCE TIGHTENED IN V2.06 (SEE REVISION HISTORY).          *
      *****************************************************************
       P6310-NEWTON-STEP.
           ADD 1 TO WS-MW-ITERATION-CNT.
           MOVE WS-MW-ROOT-EST TO WS-MW-ROOT-PREV.
           COMPUTE WS-MW-ROOT-EST ROUNDED =
               (WS-MW-ROOT-PREV + (WS-MW-RADICAND / WS-MW-ROOT-PREV))
                   / 2.
           COMPUTE WS-MW-ROOT-DELTA = WS-MW-ROOT-EST - WS-MW-ROOT-PREV.
           IF WS-MW-ROOT-DELTA < 0
               COMPUTE WS-MW-ROOT-DELTA = WS-MW-ROOT-DELTA * -1.
           IF WS-MW-ROOT-DELTA < WS-MW-CONVERGE-TOL
               MOVE 'Y' TO WS-MW-CONVERGED-SW.
       P6310-EXIT.
           EXIT.
      *****************************************************************
      * P6400-BAND-MILEAGE - WALKS THIS ELEMENT'S SLICE OF THE        *
      * FLATTENED BAND POOL (R3-BAND-POOL, VIA CABSRT03) LOOKING FOR  *
      * THE BAND WHOSE FROM/THRU RANGE CONTAINS THE COMPUTED MILEAGE. *
      *****************************************************************
       P6400-BAND-MILEAGE.
           MOVE 'N' TO WS-MB-FOUND-SW.
           MOVE SPACES TO WS-MB-BAND-TEXT.
           IF WS-SEL-BAND-CNT > 0
               COMPUTE WS-MB-SCAN-LIMIT =
                   WS-SEL-BAND-OFFSET + WS-SEL-BAND-CNT - 1
               PERFORM P6410-SCAN-ONE-BAND THRU P6410-EXIT
                   VARYING WS-MB-SCAN-SUB FROM WS-SEL-BAND-OFFSET
                   BY 1
                   UNTIL WS-MB-SCAN-SUB > WS-MB-SCAN-LIMIT
                       OR WS-MB-FOUND.
           IF NOT WS-MB-FOUND
               MOVE 'OOB ' TO WS-MB-BAND-TEXT
               MOVE WS-SEL-ADDL-RATE TO WS-MB-SEL-RATE.
           MOVE WS-MB-BAND-TEXT TO WS-DESC-FRAG4.
       P6400-EXIT.
           EXIT.
       P6410-SCAN-ONE-BAND.
           IF WS-MW-MILES NOT < R3-PL-FROM (WS-MB-SCAN-SUB) AND
                   WS-MW-MILES NOT > R3-PL-THRU (WS-MB-SCAN-SUB)
               MOVE R3-PL-RATE (WS-MB-SCAN-SUB) TO WS-MB-SEL-RATE
               PERFORM P6420-FORMAT-BAND-TEXT THRU P6420-EXIT
               MOVE 'Y' TO WS-MB-FOUND-SW.
       P6410-EXIT.
           EXIT.
       P6420-FORMAT-BAND-TEXT.
           COMPUTE WS-MB-SUB = WS-MB-SCAN-SUB - WS-SEL-BAND-OFFSET
               + 1.
           MOVE WS-MB-SUB TO WS-MB-ORDINAL.
           MOVE SPACES TO WS-MB-BAND-TEXT.
           STRING 'B' DELIMITED BY SIZE
                  WS-MB-ORDINAL DELIMITED BY SIZE
               INTO WS-MB-BAND-TEXT.
       P6420-EXIT.
           EXIT.
      *****************************************************************
      * S650-FEATURE-GROUP SECTION - CLASSIFIES THE CALL INTO FEATURE  *
      * GROUP A/B/C/D FROM THE TRUNK GROUP, END OFFICE, CIC AND        *
      * TANDEM INDICATOR, THEN SETS CCL-SUBJECT AND PREMIUM/NON-       *
      * PREMIUM STATUS.  CALLED ONCE PER CDR FROM P2200, BEFORE THE    *
      * ELEMENT DISPATCH LOOP.                                         *
      *****************************************************************
       S650-FEATURE-GROUP SECTION.
       P6500-DETERMINE-FEATURE-GRP.
           MOVE 'A' TO WS-FG-CODE.
           MOVE 'N' TO WS-FG-CCL-SUBJECT-SW.
           MOVE 'N' TO WS-FG-PREMIUM-SW.
           PERFORM P6510-EQUAL-ACCESS-TEST THRU P6510-EXIT.
           IF NOT WS-FG-IS-D
               PERFORM P6520-SCAN-TRUNK-XREF THRU P6520-EXIT.
           IF NOT WS-FG-IS-D AND NOT WS-TGFG-FOUND
               PERFORM P6530-DEFAULT-BY-TANDEM THRU P6530-EXIT.
           PERFORM P6540-SET-CCL-SUBJECT THRU P6540-EXIT.
           PERFORM P6550-SET-PREMIUM-FLAG THRU P6550-EXIT.
           PERFORM P6560-FORMAT-FG-TEXT THRU P6560-EXIT.
       P6500-EXIT.
           EXIT.
      * A NON-ZERO CIC THROUGH A KNOWN END OFFICE IS EQUAL ACCESS -
      * FEATURE GROUP D - TRIED FIRST AS THE MOST DIRECT EVIDENCE.
       P6510-EQUAL-ACCESS-TEST.
           IF WS-CW-CIC NOT = 0 AND CD-VC-END-OFFICE NOT = SPACES
               MOVE 'D' TO WS-FG-CODE.
       P6510-EXIT.
           EXIT.
      * TRUNK GROUP LOOKUP AGAINST THE 24-ROW XREF LOADED BY P1380.
       P6520-SCAN-TRUNK-XREF.
           MOVE 'N' TO WS-TGFG-FOUND-SW.
           PERFORM P6522-SCAN-ONE-TGFG THRU P6522-EXIT
               VARYING WS-TGFG-X FROM 1 BY 1
               UNTIL WS-TGFG-X > 24 OR WS-TGFG-FOUND.
       P6520-EXIT.
           EXIT.
       P6522-SCAN-ONE-TGFG.
           IF WS-TGFG-TRUNK-GRP (WS-TGFG-X) = CD-VC-TRUNK-GRP
               MOVE WS-TGFG-FG-CODE (WS-TGFG-X) TO WS-FG-CODE
               MOVE 'Y' TO WS-TGFG-FOUND-SW.
       P6522-EXIT.
           EXIT.
      * NEITHER TEST SETTLED IT - TANDEM-ROUTED DEFAULTS TO LEGACY
      * TRUNK-SIDE (FGB), DIRECT DEFAULTS TO LEGACY LINE-SIDE (FGA).
       P6530-DEFAULT-BY-TANDEM.
           IF WS-CW-TANDEM-IND = 'Y'
               MOVE 'B' TO WS-FG-CODE
           ELSE
               MOVE 'A' TO WS-FG-CODE.
       P6530-EXIT.
           EXIT.
       P6540-SET-CCL-SUBJECT.
           IF WS-FG-IS-B OR WS-FG-IS-C OR WS-FG-IS-D
               MOVE 'Y' TO WS-FG-CCL-SUBJECT-SW.
       P6540-EXIT.
           EXIT.
      * FGD WITH A VALID CIC RATES PREMIUM CCL.  EVERYTHING ELSE
      * (INCLUDING AN FGD CALL WITHOUT A CIC) RATES NON-PREMIUM.
       P6550-SET-PREMIUM-FLAG.
           IF WS-FG-IS-D AND WS-CW-CIC NOT = 0
               MOVE 'Y' TO WS-FG-PREMIUM-SW.
       P6550-EXIT.
           EXIT.
       P6560-FORMAT-FG-TEXT.
           MOVE SPACES TO WS-FG-TEXT.
           STRING 'FEAT GRP ' DELIMITED BY SIZE
                  WS-FG-CODE  DELIMITED BY SIZE
               INTO WS-FG-TEXT.
       P6560-EXIT.
           EXIT.
      * CALLED FROM P7100 RIGHT AFTER CCLINE IS RATED.  STAMPS THE
      * FEATURE GROUP INTO THE DESCRIPTION FRAGMENT AND SCALES THE
      * AMOUNT BY THE DIFFERENTIAL - LAYERED ON TOP OF, NOT INSTEAD
      * OF, THE EXISTING CARRIER-TYPE-DRIVEN LOGIC IN P5500.
       P6570-APPLY-CCL-DIFFERENTIAL.
           MOVE SPACES TO WS-DESC-FRAG6.
           IF WS-FG-IS-PREMIUM
               STRING 'FG' DELIMITED BY SIZE
                      WS-FG-CODE DELIMITED BY SIZE
                      ' PREM' DELIMITED BY SIZE
                   INTO WS-DESC-FRAG6
           ELSE
               STRING 'FG' DELIMITED BY SIZE
                      WS-FG-CODE DELIMITED BY SIZE
                      ' STD ' DELIMITED BY SIZE
                   INTO WS-DESC-FRAG6.
           IF WS-FG-IS-PREMIUM
               COMPUTE WS-ELEM-AMOUNT ROUNDED =
                   WS-ELEM-AMOUNT * WS-FG-PREM-FACTOR
           ELSE
               COMPUTE WS-ELEM-AMOUNT ROUNDED =
                   WS-ELEM-AMOUNT * WS-FG-NONPREM-FACTOR.
       P6570-EXIT.
           EXIT.
      *****************************************************************
      * S680-DATABASE-QUERY SECTION - RATES THE 8YY/800 DATABASE       *
      * QUERY CHARGE (RATE ELEMENT DBQURY), A PER-QUERY CHARGE FOR     *
      * AN SMS/800 LOOKUP ON A TOLL-FREE CALL.  SIXTH RATE ELEMENT,    *
      * SEPARATE FROM THE FIVE RATED BY S500.  CALLED ONCE PER CDR     *
      * FROM P2200, AFTER THE NORMAL ELEMENT DISPATCH LOOP.            *
      *****************************************************************
       S680-DATABASE-QUERY SECTION.
       P6800-RATE-8YY-QUERY.
           MOVE 'N' TO WS-8YY-CALL-SW.
           MOVE CD-VC-TERM-NPANXX TO WS-8YY-TERM-NPANXX.
           PERFORM P6810-SCAN-8YY-NPA THRU P6810-EXIT
               VARYING WS-8YY-NX FROM 1 BY 1
               UNTIL WS-8YY-NX > 7 OR WS-8YY-IS-TOLLFREE.
           IF WS-8YY-IS-TOLLFREE
               PERFORM P6820-RESOLVE-DBQURY-RATE THRU P6820-EXIT.
           IF WS-8YY-IS-TOLLFREE AND WS-RR-FOUND
               PERFORM P6830-COMPUTE-QUERY-CHARGE THRU P6830-EXIT
               PERFORM P6840-ACCUM-OCN-QUERY-CNT THRU P6840-EXIT
               PERFORM P6850-APPLY-VOLUME-DISCOUNT THRU P6850-EXIT
               PERFORM P6860-BUILD-8YY-SORT-REC THRU P6860-EXIT.
           IF WS-8YY-IS-TOLLFREE AND NOT WS-RR-FOUND
               PERFORM P2820-SOFT-REJECT-ELEMENT THRU P2820-EXIT.
       P6800-EXIT.
           EXIT.
      * SUBSCRIPTED WALK OF THE SEVEN-ROW NPA TABLE - NPA ISOLATED BY
      * THE REDEFINES ABOVE, NOT REFERENCE MODIFICATION.
       P6810-SCAN-8YY-NPA.
           IF WS-8YY-TERM-NPA = WS-8YY-NPA-VALUE (WS-8YY-NX)
               MOVE 'Y' TO WS-8YY-CALL-SW.
       P6810-EXIT.
           EXIT.
      * REUSES THE S400 BINARY SEARCH AND THREE-LEVEL FALLBACK FOR
      * RATE ELEMENT DBQURY INSTEAD OF ONE OF THE FIVE S500 ELEMENTS.
       P6820-RESOLVE-DBQURY-RATE.
           MOVE R1-TARIFF-CD TO WS-RK-TARIFF.
           MOVE WS-ELEM-DBQURY TO WS-RK-ELEM.
           MOVE WS-CW-JURIS-CD TO WS-RK-JURIS.
           MOVE WS-CW-STATE-CD TO WS-RK-STATE.
           PERFORM P4000-RESOLVE-RATE THRU P4000-EXIT.
       P6820-EXIT.
           EXIT.
      * BASE CHARGE IS THE FLAT PER-QUERY RATE.  A CIC OUTSIDE THE
      * HOME CARRIER'S OWN BLOCK MEANS THE QUERY WENT TO A THIRD
      * PARTY'S DATABASE IMAGE, WHICH ADDS A FLAT SURCHARGE.
       P6830-COMPUTE-QUERY-CHARGE.
           MOVE WS-SEL-INIT-RATE TO WS-8YY-QUERY-AMOUNT.
           MOVE 'N' TO WS-8YY-THIRD-PARTY-SW.
           IF WS-CW-CIC < WS-OWN-CIC-LOW OR WS-CW-CIC >
                   WS-OWN-CIC-HIGH
               MOVE 'Y' TO WS-8YY-THIRD-PARTY-SW.
           MOVE 0 TO WS-8YY-SURCHARGE-AMOUNT.
           IF WS-8YY-THIRD-PARTY-DB
               MOVE WS-8YY-SURCHARGE-RATE TO WS-8YY-SURCHARGE-AMOUNT
               ADD 1 TO WS-MC-8YY-THIRD-PARTY-CNT.
           ADD WS-8YY-SURCHARGE-AMOUNT TO WS-8YY-QUERY-AMOUNT.
       P6830-EXIT.
           EXIT.
      * FIND-OR-ADD INTO THE PER-OCN QUERY COUNT, SAME SHAPE AS
      * P2460 AND P2470.  TABLE FULL MEANS NO DISCOUNT (SAFE).
       P6840-ACCUM-OCN-QUERY-CNT.
           MOVE 'N' TO WS-8YQ-FOUND-SW.
           MOVE 0 TO WS-8YQ-FOUND-NUM.
           PERFORM P6842-SEARCH-8YY-OCN THRU P6842-EXIT
               VARYING WS-8YQ-X FROM 1 BY 1
               UNTIL WS-8YQ-X > WS-8YQ-CNT OR WS-8YQ-FOUND.
           IF WS-8YQ-FOUND
               SET WS-8YQ-X TO WS-8YQ-FOUND-NUM.
           IF NOT WS-8YQ-FOUND AND WS-8YQ-CNT < 40
               ADD 1 TO WS-8YQ-CNT
               SET WS-8YQ-X TO WS-8YQ-CNT
               MOVE WS-CW-OCN TO WS-8YQ-OCN (WS-8YQ-X)
               MOVE 0 TO WS-8YQ-QUERY-CNT (WS-8YQ-X)
               MOVE 'Y' TO WS-8YQ-FOUND-SW.
           IF WS-8YQ-FOUND
               ADD 1 TO WS-8YQ-QUERY-CNT (WS-8YQ-X).
       P6840-EXIT.
           EXIT.
       P6842-SEARCH-8YY-OCN.
           IF WS-8YQ-OCN (WS-8YQ-X) = WS-CW-OCN
               MOVE 'Y' TO WS-8YQ-FOUND-SW
               SET WS-8YQ-FOUND-NUM TO WS-8YQ-X.
       P6842-EXIT.
           EXIT.
      * BANDED LOOKUP AGAINST THE ACCUMULATED QUERY COUNT, SAME SHAPE
      * AS THE MILEAGE BAND SCAN IN P6410 BUT A FIXED FOUR-ROW TABLE.
       P6850-APPLY-VOLUME-DISCOUNT.
           MOVE 0 TO WS-8YY-DISCOUNT-PCT.
           IF WS-8YQ-FOUND
               PERFORM P6852-SCAN-ONE-BAND THRU P6852-EXIT
                   VARYING WS-8YB-X FROM 1 BY 1
                   UNTIL WS-8YB-X > 4.
           COMPUTE WS-8YY-QUERY-AMOUNT ROUNDED =
               WS-8YY-QUERY-AMOUNT -
               (WS-8YY-QUERY-AMOUNT * WS-8YY-DISCOUNT-PCT / 100).
       P6850-EXIT.
           EXIT.
       P6852-SCAN-ONE-BAND.
           IF WS-8YQ-QUERY-CNT (WS-8YQ-X) NOT <
                   WS-8YB-FROM-QTY (WS-8YB-X) AND
                   WS-8YQ-QUERY-CNT (WS-8YQ-X) NOT >
                   WS-8YB-THRU-QTY (WS-8YB-X)
               MOVE WS-8YB-DISC-PCT (WS-8YB-X) TO
                   WS-8YY-DISCOUNT-PCT.
       P6852-EXIT.
           EXIT.
      * RELEASES A SORT RECORD THROUGH THE SAME SORTWK / P3100 PATH
      * AS THE FIVE S500 ELEMENTS, SO IT BECOMES ITS OWN BD-ELEMENT
      * AND FEEDS THE HASH TOTALS / P8000 BALANCE AUTOMATICALLY.
       P6860-BUILD-8YY-SORT-REC.
           MOVE WS-CW-OCN TO SR-OCN.
           MOVE WS-CW-BAN TO SR-BAN.
           MOVE WS-CW-JURIS-CD TO SR-JURIS-CD.
           MOVE WS-ELEM-DBQURY TO SR-RATE-ELEM.
           MOVE WS-CW-STATE-CD TO SR-STATE-CD.
           MOVE R1-BILL-PERIOD TO SR-BILL-PERIOD.
           MOVE 'VC' TO SR-SECTION.
           MOVE WS-CW-SEQ-NBR TO SR-SEQ-NBR.
           MOVE 1 TO SR-QTY.
           MOVE WS-SEL-INIT-RATE TO SR-RATE.
           MOVE WS-8YY-QUERY-AMOUNT TO SR-AMOUNT.
           MOVE WS-SEL-ROUND-RULE TO SR-ROUND-RULE.
           MOVE WS-PGM-NAME TO SR-SRC-PROCESS.
           MOVE SPACES TO SR-MILEAGE-BAND.
           MOVE 'N' TO SR-CCL-ELIGIBLE-SW.
           PERFORM P6862-BUILD-8YY-DESCRIPTION THRU P6862-EXIT.
           PERFORM P3100-RELEASE-ELEMENT-REC THRU P3100-EXIT.
           ADD 1 TO WS-MC-8YY-QUERIES-RATED.
       P6860-EXIT.
           EXIT.
      * BUILDS ITS OWN DESCRIPTION RATHER THAN THE SIX-FRAGMENT
      * ASSEMBLY IN P2510, AND STAMPS THE FEATURE GROUP ONTO IT TOO.
       P6862-BUILD-8YY-DESCRIPTION.
           MOVE SPACES TO SR-DESCRIPTION.
           MOVE '8YY DB QUERY   ' TO WS-8YY-DESC-FRAG1.
           MOVE 'HOMEDB  ' TO WS-8YY-DESC-FRAG2.
           IF WS-8YY-THIRD-PARTY-DB
               MOVE '3RDPARTY' TO WS-8YY-DESC-FRAG2.
           STRING WS-8YY-DESC-FRAG1 DELIMITED BY SIZE
                  WS-8YY-DESC-FRAG2 DELIMITED BY SIZE
                  ' FG-' DELIMITED BY SIZE
                  WS-FG-CODE DELIMITED BY SIZE
               INTO SR-DESCRIPTION.
       P6862-EXIT.
           EXIT.
      *****************************************************************
      * S700-ELEMENT-DISPATCH SECTION - CHOOSES BETWEEN THE INLINE    *
      * S500 RATING LOGIC AND A DYNAMIC CALL TO AN EXTERNAL RATING    *
      * MODULE.  THE OVERRIDE IS DRIVEN ENTIRELY BY DATA LOADED FROM  *
      * VSAM AT RUN TIME (R2-EN-MODULE-SFX), SO THE ACTUAL TARGET OF  *
      * THE CALL IN P7200 IS NOT KNOWN UNTIL EXECUTION - IT CANNOT BE *
      * RESOLVED BY READING THE SOURCE ALONE.                         *
      *****************************************************************
       S700-ELEMENT-DISPATCH SECTION.
       P7100-DETERMINE-OVERRIDE.
           MOVE WS-EC-ELEM-NAME (WS-EC-X) TO WS-DESC-FRAG1.
           IF WS-SEL-MODULE-SFX NOT = SPACES
               PERFORM P7200-CALL-ELEMENT-MODULE THRU P7200-EXIT
           ELSE
               PERFORM P7300-DISPATCH-INLINE THRU P7300-EXIT.
      * OWN SENTENCE (NOT NESTED IN THE IF/ELSE) SO IT APPLIES
      * REGARDLESS OF WHICH OF THE TWO PATHS ABOVE RATED CCLINE.
           IF WS-ELEM-CLASS = WS-ELEM-CCLINE
               PERFORM P6570-APPLY-CCL-DIFFERENTIAL THRU P6570-EXIT.
       P7100-EXIT.
           EXIT.
      *****************************************************************
      * P7200-CALL-ELEMENT-MODULE - DYNAMIC CALL.  THE PREFIX IS THE  *
      * FIXED LITERAL 'CABRAT' FROM R1-CALL-PREFIX (SEE CABSRT01);    *
      * THE SUFFIX COMES STRAIGHT OFF THE RATE TABLE ROW THAT WON     *
      * THE SEARCH IN S400.  TARGETS IN PRODUCTION ARE CABRATOA,      *
      * CABRATTA, CABRATLT, CABRATTS AND CABRATCC - ONE PER ELEMENT - *
      * BUT ONLY WHEN A ROW EXPLICITLY ASKS FOR THE OVERRIDE; THE     *
      * DEFAULT PATH IS THE INLINE S500 LOGIC.                        *
      *****************************************************************
       P7200-CALL-ELEMENT-MODULE.
           MOVE R1-CALL-PREFIX TO WS-TGT-PREFIX.
           MOVE R2-EN-MODULE-SFX (R2-EX) TO WS-TGT-SUFFIX.
           MOVE SPACES TO R1-CALL-TARGET.
           STRING WS-TGT-PREFIX DELIMITED BY SIZE
                  WS-TGT-SUFFIX DELIMITED BY SIZE
               INTO R1-CALL-TARGET.
           MOVE WS-ELEM-CLASS TO R1-ELEM-IN.
           MOVE WS-BILLABLE-MIN TO R1-QTY-IN.
           MOVE 0 TO R1-AMT-OUT.
           CALL R1-CALL-TARGET USING CABS-CDR-RECORD R1-ELEM-IN
               R1-QTY-IN R1-AMT-OUT R1-RC.
           MOVE R1-AMT-OUT TO WS-ELEM-AMOUNT.
           MOVE WS-SEL-RATE TO R4-EDIT-RATE.
           MOVE R4-EDIT-RATE TO WS-DESC-FRAG5.
           MOVE 'OVERRIDE' TO WS-DESC-FRAG6.
           MOVE 'Y' TO WS-CALL-USED-SW.
           ADD 1 TO WS-MC-DYNAMIC-CALLS.
       P7200-EXIT.
           EXIT.
      *****************************************************************
      * P7300-DISPATCH-INLINE - CLASSIC GO TO ... DEPENDING ON        *
      * DISPATCH TABLE TO THE FIVE S500 RATING GROUPS.                *
      *****************************************************************
       P7300-DISPATCH-INLINE.
           MOVE 1 TO WS-RD-RULE-INDEX.
           IF WS-ELEM-CLASS = WS-ELEM-TERMAC
               MOVE 2 TO WS-RD-RULE-INDEX.
           IF WS-ELEM-CLASS = WS-ELEM-LTRANS
               MOVE 3 TO WS-RD-RULE-INDEX.
           IF WS-ELEM-CLASS = WS-ELEM-TANSW
               MOVE 4 TO WS-RD-RULE-INDEX.
           IF WS-ELEM-CLASS = WS-ELEM-CCLINE
               MOVE 5 TO WS-RD-RULE-INDEX.
           GO TO P7310-ORIGAC P7320-TERMAC P7330-LTRANS P7340-TANSW
               P7350-CCLINE DEPENDING ON WS-RD-RULE-INDEX.
       P7310-ORIGAC.
           PERFORM P5100-RATE-ORIG-ACCESS THRU P5100-EXIT.
           GO TO P7300-EXIT.
       P7320-TERMAC.
           PERFORM P5200-RATE-TERM-ACCESS THRU P5200-EXIT.
           GO TO P7300-EXIT.
       P7330-LTRANS.
           PERFORM P5300-RATE-LOCAL-TRANSPORT THRU P5300-EXIT.
           GO TO P7300-EXIT.
       P7340-TANSW.
           PERFORM P5400-RATE-TANDEM THRU P5400-EXIT.
           GO TO P7300-EXIT.
       P7350-CCLINE.
           PERFORM P5500-RATE-CCL THRU P5500-EXIT.
       P7300-EXIT.
           EXIT.
      *****************************************************************
      * S800-CONTROL-BALANCE SECTION - THE MANDATORY CONTROL STEP,    *
      * PLUS THE LEVEL-BREAK REPORT ON RPTOUT (PER-OCN AND PER-       *
      * ELEMENT TOTALS) THAT ACCOMPANIES IT.                          *
      *****************************************************************
       S800-CONTROL-BALANCE SECTION.
       P8000-CONTROL.
           PERFORM P8100-BUILD-REPORT-HEADER THRU P8100-EXIT.
           PERFORM P8150-PRINT-TABLE-LOAD-SUMMARY THRU P8150-EXIT.
           PERFORM P8200-PRINT-OCN-TOTALS THRU P8200-EXIT.
           PERFORM P8250-PRINT-FALLBACK-DIST THRU P8250-EXIT.
           PERFORM P8280-PRINT-CARRIER-TYPE-MIX THRU P8280-EXIT.
           PERFORM P8300-PRINT-ELEMENT-TOTALS THRU P8300-EXIT.
           PERFORM P8700-PRINT-EXCEPTION-SUMMARY THRU P8700-EXIT.
           PERFORM P8400-BUILD-CONTROL-REC THRU P8400-EXIT.
           PERFORM P8500-CHECK-BALANCE THRU P8500-EXIT.
           PERFORM P8410-PRINT-CONTROL-SUMMARY THRU P8410-EXIT.
           PERFORM P8600-WRITE-CONTROL-REC THRU P8600-EXIT.
       P8000-EXIT.
           EXIT.
      *****************************************************************
      * P8100-BUILD-REPORT-HEADER - TWO TITLE LINES PLUS THE RUN AND  *
      * TABLE-LOAD SUMMARY LINE.                                      *
      *****************************************************************
       P8100-BUILD-REPORT-HEADER.
           ADD 1 TO WS-RPT-PAGE-NBR.
           MOVE 1 TO WS-RPT-LINE-NBR.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '1' TO PC-CC.
           MOVE WS-RPT-TITLE1 TO PC-TEXT.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-RPT-TITLE2 TO PC-TEXT.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE R1-RUN-ID TO PC-COL-001-020.
           MOVE WS-TL-RATE-ROWS-LOADED TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'CYCLE DATE (GREGORIAN):' TO PC-COL-001-020.
           IF WS-RC-DTCNV = 0
               STRING DW-GR-MM DELIMITED BY SIZE
                   '/' DELIMITED BY SIZE
                   DW-GR-DD DELIMITED BY SIZE
                   '/' DELIMITED BY SIZE
                   DW-GR-CCYY DELIMITED BY SIZE
                   INTO PC-COL-021-060
           ELSE
               MOVE 'CABDTCNV UNAVAILABLE - SEE CCYYDDD ABOVE' TO
                   PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LATEST SOURCE LOAD DATE:' TO PC-COL-001-020.
           MOVE WS-MC-MAX-LOAD-YYDDD TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           WRITE CABS-PRINT-LINE.
       P8100-EXIT.
           EXIT.
      *****************************************************************
      * P8150-PRINT-TABLE-LOAD-SUMMARY - HOW MANY ROWS WERE LOADED    *
      * INTO EACH OF THE FOUR REFERENCE TABLES AT INIT, PLUS THE      *
      * SEQUENCE / DUPLICATE COUNTS FROM P1900 AND THE PER-ELEMENT    *
      * RATE ROW COUNTS FROM P1930.  A QUIET WAY TO CATCH A BAD       *
      * RATEMST LOAD BEFORE ANYONE HAS TO ASK WHY THE BILLS LOOK      *
      * WRONG.                                                        *
      *****************************************************************
       P8150-PRINT-TABLE-LOAD-SUMMARY.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'TABLE LOADS -' TO PC-COL-001-020.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  RATE ROWS ' TO PC-COL-001-020.
           MOVE WS-TL-RATE-ROWS-LOADED TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  BAND ROWS ' TO PC-COL-001-020.
           MOVE WS-TL-BAND-ROWS-LOADED TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  V AND H ROWS' TO PC-COL-001-020.
           MOVE WS-TL-VH-ROWS-LOADED TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  LATA XREF ROWS' TO PC-COL-001-020.
           MOVE WS-TL-LATA-ROWS-LOADED TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  OUT OF SEQUENCE' TO PC-COL-001-020.
           MOVE WS-TV-OUT-OF-SEQ-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  EXACT DUPLICATES' TO PC-COL-001-020.
           MOVE WS-TV-DUP-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  DISCOUNT ELIGIBLE' TO PC-COL-001-020.
           MOVE WS-TL-DISC-ELIGIBLE-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  MPB ELIGIBLE LOOKUPS' TO PC-COL-001-020.
           MOVE WS-TL-MPB-ELIGIBLE-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           PERFORM P8160-PRINT-ONE-ELEM-CNT THRU P8160-EXIT
               VARYING WS-TV-X FROM 1 BY 1
               UNTIL WS-TV-X > 5.
           MOVE SPACES TO CABS-PRINT-LINE.
           WRITE CABS-PRINT-LINE.
       P8150-EXIT.
           EXIT.
       P8160-PRINT-ONE-ELEM-CNT.
           IF WS-TV-ELEM-CODE (WS-TV-X) NOT = SPACES
               MOVE SPACES TO CABS-PRINT-LINE
               MOVE ' ' TO PC-CC
               MOVE '  RATE ROWS FOR' TO PC-COL-001-020
               MOVE WS-TV-ELEM-CODE (WS-TV-X) TO PC-COL-021-060
               MOVE WS-TV-ELEM-ROWS (WS-TV-X) TO PC-COL-061-090
               WRITE CABS-PRINT-LINE.
       P8160-EXIT.
           EXIT.
      *****************************************************************
      * P8200-PRINT-OCN-TOTALS - ONE LINE PER DISTINCT OCN SEEN THIS  *
      * RUN, FROM THE 200-ROW SUMMARY TABLE BUILT IN P2460.           *
      *****************************************************************
       P8200-PRINT-OCN-TOTALS.
           IF WS-OT-CNT > 0
               PERFORM P8210-PRINT-ONE-OCN THRU P8210-EXIT
                   VARYING WS-OT-X FROM 1 BY 1
                   UNTIL WS-OT-X > WS-OT-CNT.
       P8200-EXIT.
           EXIT.
       P8210-PRINT-ONE-OCN.
           PERFORM P8215-CHECK-PAGE-BREAK THRU P8215-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-OT-OCN (WS-OT-X) TO WS-ED-OCN.
           MOVE WS-ED-OCN TO PC-COL-001-020.
           MOVE WS-OT-MINUTES (WS-OT-X) TO WS-ED-MINUTES.
           MOVE WS-ED-MINUTES TO PC-COL-021-060.
           MOVE WS-OT-AMOUNT (WS-OT-X) TO WS-ED-AMOUNT.
           MOVE WS-ED-AMOUNT TO PC-COL-061-090.
           MOVE WS-OT-CALL-CNT (WS-OT-X) TO WS-ED-COUNT.
           MOVE WS-ED-COUNT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P8210-EXIT.
           EXIT.
      *****************************************************************
      * P8215-CHECK-PAGE-BREAK - STANDARD LEVEL-BREAK PAGE OVERFLOW    *
      * TEST.  RPTOUT IS FBA 133, ONE PHYSICAL PRINT PAGE PER          *
      * WS-RPT-LINES-PER-PAGE LINES (55, THE STANDARD 66-LINE FORM     *
      * MINUS TITLE AND MARGIN ALLOWANCE).                             *
      *****************************************************************
       P8215-CHECK-PAGE-BREAK.
           IF WS-RPT-LINE-NBR NOT < WS-RPT-LINES-PER-PAGE
               MOVE SPACES TO CABS-PRINT-LINE
               MOVE '1' TO PC-CC
               MOVE 'OCN SUMMARY (CONTINUED)' TO PC-TEXT
               WRITE CABS-PRINT-LINE
               MOVE 1 TO WS-RPT-LINE-NBR
               ADD 1 TO WS-RPT-PAGE-NBR.
       P8215-EXIT.
           EXIT.
      *****************************************************************
      * P8250-PRINT-FALLBACK-DIST - HOW MANY RESOLVED RATES CAME FROM *
      * AN EXACT STATE MATCH VERSUS A JURISDICTION-GENERIC OR TARIFF- *
      * DEFAULT FALLBACK (SEE P4300 / P4400).  A HIGH FALLBACK        *
      * PROPORTION IS A SIGN THE STATE-SPECIFIC RATE LOAD IS THIN.    *
      *****************************************************************
       P8250-PRINT-FALLBACK-DIST.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RATE RESOLUTION -' TO PC-COL-001-020.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  EXACT STATE' TO PC-COL-001-020.
           MOVE WS-RE-FB-EXACT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  JURIS GENERIC' TO PC-COL-001-020.
           MOVE WS-RE-FB-JURIS-GEN TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  TARIFF DEFAULT' TO PC-COL-001-020.
           MOVE WS-RE-FB-TARIFF-DFLT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  JURIS NPA FALLBACK' TO PC-COL-001-020.
           MOVE WS-JD-NPA-FALLBACK-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           WRITE CABS-PRINT-LINE.
       P8250-EXIT.
           EXIT.
      *****************************************************************
      * P8280-PRINT-CARRIER-TYPE-MIX - HOW MANY DISTINCT OCNS THIS    *
      * RUN RATED FOR, BROKEN OUT BY CARRIER TYPE (IXC / CLEC / ILEC  *
      * / WIRELESS / RESELLER).  A SUDDEN SHIFT IN THIS MIX FROM ONE  *
      * CYCLE TO THE NEXT USUALLY MEANS AN UPSTREAM OCN FEED CHANGED, *
      * NOT THAT THE ACTUAL CARRIER BASE MOVED.                       *
      *****************************************************************
       P8280-PRINT-CARRIER-TYPE-MIX.
           MOVE 0 TO WS-CTM-IXC-CNT.
           MOVE 0 TO WS-CTM-CLEC-CNT.
           MOVE 0 TO WS-CTM-ILEC-CNT.
           MOVE 0 TO WS-CTM-WIRELESS-CNT.
           MOVE 0 TO WS-CTM-RESELLER-CNT.
           MOVE 0 TO WS-CTM-OTHER-CNT.
           IF WS-OT-CNT > 0
               PERFORM P8285-TALLY-ONE-CARRIER-TYPE THRU P8285-EXIT
                   VARYING WS-OT-X FROM 1 BY 1
                   UNTIL WS-OT-X > WS-OT-CNT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'CARRIER TYPE MIX -' TO PC-COL-001-020.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  IXC' TO PC-COL-001-020.
           MOVE WS-CTM-IXC-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  CLEC' TO PC-COL-001-020.
           MOVE WS-CTM-CLEC-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  ILEC' TO PC-COL-001-020.
           MOVE WS-CTM-ILEC-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  WIRELESS' TO PC-COL-001-020.
           MOVE WS-CTM-WIRELESS-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  RESELLER' TO PC-COL-001-020.
           MOVE WS-CTM-RESELLER-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  OTHER/UNKNOWN' TO PC-COL-001-020.
           MOVE WS-CTM-OTHER-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  DISTINCT END OFFICES' TO PC-COL-001-020.
           MOVE WS-EO-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           WRITE CABS-PRINT-LINE.
       P8280-EXIT.
           EXIT.
       P8285-TALLY-ONE-CARRIER-TYPE.
           IF WS-OT-CARRIER-TYPE (WS-OT-X) = 'I'
               ADD 1 TO WS-CTM-IXC-CNT.
           IF WS-OT-CARRIER-TYPE (WS-OT-X) = 'C'
               ADD 1 TO WS-CTM-CLEC-CNT.
           IF WS-OT-CARRIER-TYPE (WS-OT-X) = 'L'
               ADD 1 TO WS-CTM-ILEC-CNT.
           IF WS-OT-CARRIER-TYPE (WS-OT-X) = 'W'
               ADD 1 TO WS-CTM-WIRELESS-CNT.
           IF WS-OT-CARRIER-TYPE (WS-OT-X) = 'R'
               ADD 1 TO WS-CTM-RESELLER-CNT.
           IF WS-OT-CARRIER-TYPE (WS-OT-X) NOT = 'I' AND
                   WS-OT-CARRIER-TYPE (WS-OT-X) NOT = 'C' AND
                   WS-OT-CARRIER-TYPE (WS-OT-X) NOT = 'L' AND
                   WS-OT-CARRIER-TYPE (WS-OT-X) NOT = 'W' AND
                   WS-OT-CARRIER-TYPE (WS-OT-X) NOT = 'R'
               ADD 1 TO WS-CTM-OTHER-CNT.
       P8285-EXIT.
           EXIT.
      *****************************************************************
      * P8300-PRINT-ELEMENT-TOTALS - ONE LINE PER RATE ELEMENT.       *
      * INSPECTS WS-AI-FRACTION (VIA THE WS-AMT-INSPECT REDEFINES) TO *
      * FLAG A ROW THAT STILL CARRIES SUB-CENT RESIDUE.               *
      *****************************************************************
       P8300-PRINT-ELEMENT-TOTALS.
           PERFORM P8310-PRINT-ONE-ELEMENT THRU P8310-EXIT
               VARYING WS-RT-X FROM 1 BY 1
               UNTIL WS-RT-X > 5.
       P8300-EXIT.
           EXIT.
       P8310-PRINT-ONE-ELEMENT.
           PERFORM P8315-CHECK-PAGE-BREAK THRU P8315-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-RT-ELEM-CODE (WS-RT-X) TO PC-COL-001-020.
           MOVE WS-RT-ELEM-MINUTES (WS-RT-X) TO WS-ED-MINUTES.
           MOVE WS-ED-MINUTES TO PC-COL-021-060.
           MOVE WS-RT-ELEM-AMOUNT (WS-RT-X) TO WS-AMT-INSPECT.
           MOVE WS-RT-ELEM-AMOUNT (WS-RT-X) TO WS-ED-AMOUNT.
           MOVE WS-ED-AMOUNT TO PC-COL-061-090.
           MOVE SPACES TO PC-COL-091-132.
           IF WS-AI-FRACTION NOT = 0
               MOVE '* SUB-CENT RESIDUE PRESENT' TO PC-COL-091-132
           ELSE
               IF WS-RT-ELEM-MINUTES (WS-RT-X) > 0
                   COMPUTE WS-RTFMT-RATE-IN ROUNDED =
                       WS-RT-ELEM-AMOUNT (WS-RT-X) /
                       WS-RT-ELEM-MINUTES (WS-RT-X)
                   PERFORM P8312-FORMAT-AVG-RATE THRU P8312-EXIT
                   MOVE WS-RTFMT-OUT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P8310-EXIT.
           EXIT.
      *****************************************************************
      * P8312-FORMAT-AVG-RATE - CABRTFMT IS THE ESTATE-STANDARD RATE   *
      * EDIT UTILITY - EVERY REPORT THAT PRINTS A PER-MINUTE RATE      *
      * VALUE USES IT SO THE DECIMAL ALIGNMENT AND CURRENCY SYMBOL     *
      * PLACEMENT ARE CONSISTENT ACROSS THE WHOLE CABS SUITE, NOT      *
      * JUST WITHIN THIS ONE PROGRAM.  ON EXCEPTION LEAVES THE OUTPUT  *
      * FIELD AS SPACES RATHER THAN SHOWING A HALF-FORMATTED VALUE.    *
      *****************************************************************
       P8312-FORMAT-AVG-RATE.
           MOVE SPACES TO WS-RTFMT-OUT.
           CALL 'CABRTFMT' USING WS-RTFMT-RATE-IN WS-RTFMT-OUT
               ON EXCEPTION
                   MOVE 9999 TO WS-RC-RTFMT
                   MOVE SPACES TO WS-RTFMT-OUT
               NOT ON EXCEPTION
                   MOVE 0 TO WS-RC-RTFMT.
       P8312-EXIT.
           EXIT.
      *****************************************************************
      * P8315-CHECK-PAGE-BREAK - SAME PATTERN AS P8215, FOR THE        *
      * ELEMENT TOTALS BLOCK.                                          *
      *****************************************************************
       P8315-CHECK-PAGE-BREAK.
           IF WS-RPT-LINE-NBR NOT < WS-RPT-LINES-PER-PAGE
               MOVE SPACES TO CABS-PRINT-LINE
               MOVE '1' TO PC-CC
               MOVE 'ELEMENT SUMMARY (CONTINUED)' TO PC-TEXT
               WRITE CABS-PRINT-LINE
               MOVE 1 TO WS-RPT-LINE-NBR
               ADD 1 TO WS-RPT-PAGE-NBR.
       P8315-EXIT.
           EXIT.
      *****************************************************************
      * P8700-PRINT-EXCEPTION-SUMMARY - COUNTS OF THE THREE SILENT    *
      * BEHAVIOURS ELSEWHERE IN THIS PROGRAM: ELEMENTS RATED BUT      *
      * DROPPED FOR CCL INELIGIBILITY (P3100), ELEMENTS SUPPRESSED    *
      * FOR FALLING BELOW THE HALF-CENT THRESHOLD (P3650), AND        *
      * INDIVIDUAL ELEMENTS SOFT-REJECTED FOR A RATE-NOT-FOUND        *
      * CONDITION (P2820).  NONE OF THESE THREE COUNTS FEEDS THE      *
      * BALANCING EQUATION - THEY ARE INFORMATIONAL ONLY.             *
      *****************************************************************
       P8700-PRINT-EXCEPTION-SUMMARY.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'EXCEPTIONS -' TO PC-COL-001-020.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  CCL DROPPED' TO PC-COL-001-020.
           MOVE WS-MC-ELEMENTS-DROPPED-CCL TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  SUPPRESSED' TO PC-COL-001-020.
           MOVE WS-MC-ELEMENTS-SUPPRESSED TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  RATE NOT FOUND' TO PC-COL-001-020.
           MOVE WS-MC-RATE-NOT-FOUND-SOFT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  DYNAMIC CALLS' TO PC-COL-001-020.
           MOVE WS-MC-DYNAMIC-CALLS TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  BD TABLE FULL' TO PC-COL-001-020.
           MOVE WS-MC-BD-TABLE-FULL TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  V+H EXACT' TO PC-COL-001-020.
           MOVE WS-MC-VH-EXACT-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  V+H FALLBACK' TO PC-COL-001-020.
           MOVE WS-MC-VH-FALLBACK-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  ZERO DURATION' TO PC-COL-001-020.
           MOVE WS-MC-ZERO-DURATION-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           PERFORM P8710-SUM-MINMAX-COUNTS THRU P8710-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  MIN CHG APPLIED' TO PC-COL-001-020.
           MOVE WS-RE-TOT-MIN-APPLIED TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  MAX CHG APPLIED' TO PC-COL-001-020.
           MOVE WS-RE-TOT-MAX-APPLIED TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  RECIP CAP EXCEEDED' TO PC-COL-001-020.
           MOVE WS-RS-CAP-EXCEEDED-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  MILEAGE OVER CEILING' TO PC-COL-001-020.
           MOVE WS-MW-CEILING-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  8YY QUERIES RATED' TO PC-COL-001-020.
           MOVE WS-MC-8YY-QUERIES-RATED TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  8YY THIRD PARTY DB' TO PC-COL-001-020.
           MOVE WS-MC-8YY-THIRD-PARTY-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           WRITE CABS-PRINT-LINE.
       P8700-EXIT.
           EXIT.
      *****************************************************************
      * P8710-SUM-MINMAX-COUNTS - TOTALS THE MIN/MAX-CHARGE-APPLIED   *
      * COUNTS ACCUMULATED PER ELEMENT BY P2450 ACROSS ALL FIVE       *
      * ELEMENTS FOR THE RUN-LEVEL EXCEPTION SUMMARY LINE.            *
      *****************************************************************
       P8710-SUM-MINMAX-COUNTS.
           MOVE 0 TO WS-RE-TOT-MIN-APPLIED.
           MOVE 0 TO WS-RE-TOT-MAX-APPLIED.
           PERFORM P8720-ADD-ONE-ELEM-MINMAX THRU P8720-EXIT
               VARYING WS-RT-X FROM 1 BY 1
               UNTIL WS-RT-X > 5.
       P8710-EXIT.
           EXIT.
       P8720-ADD-ONE-ELEM-MINMAX.
           ADD WS-RT-MIN-APPLIED-CNT (WS-RT-X) TO
               WS-RE-TOT-MIN-APPLIED.
           ADD WS-RT-MAX-APPLIED-CNT (WS-RT-X) TO
               WS-RE-TOT-MAX-APPLIED.
       P8720-EXIT.
           EXIT.
      *****************************************************************
      * P8400-BUILD-CONTROL-REC - POPULATES CABS-CONTROL-RECORD.      *
      * CT-SUMMARISED IS ALWAYS ZERO FOR THIS PROGRAM - EVERY READ    *
      * RECORD THAT IS NOT CARRIED FORWARD BY A RESTART ENDS UP       *
      * EITHER WRITTEN (RATED, WHOLLY OR PARTLY) OR REJECTED IN       *
      * P2200.  CT-CARRIED-FWD IS WS-CFWD-CNT - RECORDS SKIPPED BY    *
      * P2110 BECAUSE THEY WERE ALREADY PROCESSED BEFORE A RESTART.   *
      * CT-RESTART-KEY ECHOES BACK WHATEVER RESTART KEY THIS RUN WAS  *
      * GIVEN ON THE PARM CARD (SPACES ON A NORMAL, NON-RESTART RUN)  *
      * FOR THE AUDIT TRAIL.                                          *
      *****************************************************************
       P8400-BUILD-CONTROL-REC.
           MOVE R1-RUN-ID TO CT-RUN-ID.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE 1 TO CT-STEP-SEQ.
           MOVE WS-EV-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE R1-BILL-PERIOD TO CT-BILL-PERIOD.
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
       P8400-EXIT.
           EXIT.
      *****************************************************************
      * P8500-CHECK-BALANCE - THE MANDATORY BALANCING TEST.           *
      *****************************************************************
       P8500-CHECK-BALANCE.
           IF CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARISED +
                   CT-CARRIED-FWD
               MOVE 'B' TO CT-BAL-IND
           ELSE
               MOVE 'O' TO CT-BAL-IND.
       P8500-EXIT.
           EXIT.
      *****************************************************************
      * P8410-PRINT-CONTROL-SUMMARY - THE SAME COUNTS THAT GO TO       *
      * CTLOUT, ALSO PRINTED ON RPTOUT SO AN OPERATOR REVIEWING THE    *
      * SYSOUT CAN SEE THE BALANCING RESULT WITHOUT WAITING FOR THE    *
      * DOWNSTREAM CONTROL REPORTING JOB TO PICK UP THE CTLOUT FILE.   *
      *****************************************************************
       P8410-PRINT-CONTROL-SUMMARY.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '1' TO PC-CC.
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
           MOVE '  CT-SUMMARISED' TO PC-COL-001-020.
           MOVE CT-SUMMARISED TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  CT-CARRIED-FWD' TO PC-COL-001-020.
           MOVE CT-CARRIED-FWD TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  HASH MINUTES' TO PC-COL-001-020.
           MOVE CT-HASH-MINUTES TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE '  HASH AMOUNT' TO PC-COL-001-020.
           MOVE CT-HASH-AMOUNT TO PC-COL-021-060.
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
       P8410-EXIT.
           EXIT.
      *****************************************************************
      * P8600-WRITE-CONTROL-REC.                                      *
      *****************************************************************
       P8600-WRITE-CONTROL-REC.
           MOVE CABS-CONTROL-RECORD TO CABS-CTLOUT-RECORD.
           WRITE CABS-CTLOUT-RECORD.
       P8600-EXIT.
           EXIT.
      *****************************************************************
      * S900-TERMINATION SECTION.                                     *
      *****************************************************************
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
           DISPLAY 'CABRAT03 - RUN COMPLETE'.
           DISPLAY '  READ       = ' WS-READ-CNT.
           DISPLAY '  WRITTEN    = ' WS-WRITE-CNT.
           DISPLAY '  REJECTED   = ' WS-REJECT-CNT.
           DISPLAY '  BANS OUT   = ' WS-MC-BANS-WRITTEN.
           DISPLAY '  CCL DROPPED= ' WS-MC-ELEMENTS-DROPPED-CCL.
           DISPLAY '  SUPPRESSED = ' WS-MC-ELEMENTS-SUPPRESSED.
           DISPLAY '  DYN CALLS  = ' WS-MC-DYNAMIC-CALLS.
           DISPLAY '  8YY QUERIES= ' WS-MC-8YY-QUERIES-RATED.
       P9000-EXIT.
           EXIT.
      *****************************************************************
      * P9990-RATE-FAILURE - HIDDEN AT THE PHYSICAL BOTTOM OF THE     *
      * PROGRAM.  REACHED ONLY BY GO TO FROM THREE WIDELY SEPARATED   *
      * PLACES: P5100-RATE-ORIG-ACCESS (RATE TABLE INDEX INVALID -    *
      * "TABLE EXHAUSTED"), P5400-RATE-TANDEM (TANDEM RATE RESOLVED   *
      * TO ZERO - "RATE MISSING"), AND P6300-COMPUTE-MILEAGE (THE     *
      * NEWTON'S METHOD SQUARE ROOT FAILED TO CONVERGE).  IN ALL      *
      * THREE CASES THE RUN CANNOT PRODUCE A TRUSTWORTHY BILL, SO IT  *
      * WRITES A FINAL OUT-OF-BALANCE CONTROL RECORD AND ABENDS       *
      * RATHER THAN SILENTLY UNDER- OR OVER-CHARGING A CARRIER.       *
      *****************************************************************
       P9990-RATE-FAILURE.
           MOVE WS-PGM-NAME TO WS-AB-PGM.
           MOVE 'P9990-RATE-FAILURE' TO WS-AB-PARA.
           MOVE 'RATE RESOLUTION OR MILEAGE FAILURE - SEE SYSOUT' TO
               WS-AB-REASON.
           MOVE 9990 TO WS-AB-USER-CODE.
           PERFORM P8400-BUILD-CONTROL-REC THRU P8400-EXIT.
           MOVE 'O' TO CT-BAL-IND.
           MOVE 9990 TO CT-RC.
           MOVE '9990' TO CT-ABEND-CD.
           PERFORM P8600-WRITE-CONTROL-REC THRU P8600-EXIT.
           DISPLAY 'CABRAT03 FATAL - ' WS-AB-REASON.
           CALL 'CABABEND' USING WS-AB-PGM WS-AB-PARA WS-AB-REASON
               WS-AB-USER-CODE.
           STOP RUN.
       P9990-EXIT.
           EXIT.
