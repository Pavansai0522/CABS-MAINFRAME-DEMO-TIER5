      *****************************************************************
      * CABSET04 - RECIPROCAL COMPENSATION MOU AGGREGATION            *
      * APPLICATION : SETL                                            *
      * INPUTS      : RECIPIN  TELCABS.SETL.RECIP.USAGE(0)    CABSCDR *
      * INPUTS      : CARRMAST TELCABS.SETL.CARRIER           CABSCARR*
      * INPUTS      : SYSIN    INSTREAM                       NONE    *
      * OUTPUTS     : RECAGG   TELCABS.SETL.RECIP.AGG(+1)     CABSSETL*
      * OUTPUTS     : REPORT   SYSOUT                         CABSPRNT*
      * CONTROL     : CTLOUT                         CABSCTL          *
      * BALANCE     : CT-READ = CT-SUMMARISED + CT-REJECTED           *
      *               EVERY TERMINATING RECORD IS AGGREGATED          *
      * RESTART     : FULL RERUN                                      *
      * STANDARDS   : CODED TO CABS-STD-014 AND CABS-STD-041.         *
      * REVISION HISTORY                                              *
      *   V1.00  1996-04-30  J.M.CASTILLO  INITIAL - 1996 ACT         *
      *   V1.02  1997-08-19  J.M.CASTILLO  ISP BOUND SPLIT ADDED      *
      *   V1.06  2001-03-27  P.NAIR        CAP TRACKING ADDED         *
      *   V2.00  2004-09-14  P.NAIR        WIRELESS SPLIT ADDED       *
      *   V2.02  2009-02-03  A.BUKOWSKI    TRANSIT MINUTES EXCLUDED   *
      *   V2.04  2013-10-08  L.FERREIRA    VOIP MINUTES INCLUDED      *
      *   V2.05  2019-04-16  M.OYELARAN    RECOMPILE ONLY             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CABSET04.
       AUTHOR.        J.M.CASTILLO.
       DATE-WRITTEN.  1996-04-30.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
               C01 IS TOP-OF-PAGE
               C04 IS NEW-SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * TERMINATING USAGE FROM CLEC INTERCONNECTION TRUNKS
           SELECT RECIP-IN-FILE
               ASSIGN TO UT-S-RECIPIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
      * CARRIER MASTER - RECIP RATE AND ISP CAP
           SELECT CARRIER-MASTER
               ASSIGN TO DA-I-CARRMAST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CRM-KEY
               FILE STATUS IS WS-FS-TABLE.
      * AGGREGATED MINUTES BY CARRIER - INPUT TO CABSET05
           SELECT RECIP-AGG-FILE
               ASSIGN TO UT-S-RECAGG
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
      * SYSIN - RUN CONTROL CARD, NO DEFAULTS SUPPLIED
           SELECT PARM-FILE
               ASSIGN TO UT-S-SYSIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-TABLE.
      * RUN CONTROL - BALANCING RECORD, GDG PLUS ONE
           SELECT CONTROL-FILE
               ASSIGN TO UT-S-CTLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CONTROL.
      * SUSPENSE - REJECTED RECORDS WITH ERROR CODE
           SELECT SUSPENSE-FILE
               ASSIGN TO UT-S-SUSPOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-SUSPENSE.
      * PRINTED REPORT - ASA CARRIAGE CONTROL COL 1
           SELECT PRINT-FILE
               ASSIGN TO UT-S-REPORT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-TABLE.

       DATA DIVISION.
       FILE SECTION.
       FD  RECIP-IN-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 200 CHARACTERS
               DATA RECORD IS RCI-RECORD.
       01  RCI-RECORD              PIC X(200).

       FD  CARRIER-MASTER
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 120 CHARACTERS
               DATA RECORD IS CRM-RECORD.
       01  CRM-RECORD.
           05  CRM-KEY                 PIC X(04).
           05  CRM-DATA                PIC X(116).

       FD  RECIP-AGG-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 180 CHARACTERS
               DATA RECORD IS RAG-RECORD.
       01  RAG-RECORD              PIC X(180).

       FD  PARM-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 80 CHARACTERS
               DATA RECORD IS PRM-RECORD.
       01  PRM-RECORD              PIC X(80).

       FD  CONTROL-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 180 CHARACTERS
               DATA RECORD IS CTL-RECORD.
       01  CTL-RECORD              PIC X(180).

       FD  SUSPENSE-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 300 CHARACTERS
               DATA RECORD IS SUS-RECORD.
       01  SUS-RECORD              PIC X(300).

       FD  PRINT-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 133 CHARACTERS
               DATA RECORD IS PRT-RECORD.
       01  PRT-RECORD              PIC X(133).

       WORKING-STORAGE SECTION.

      * PROGRAM IDENTIFICATION - MOVED TO THE CONTROL RECORD AND TO
      * EVERY SUSPENSE RECORD RAISED BY THIS MODULE.
       01  WS-PROGRAM-IDENT.
           05  WS-PGM-NAME         PIC X(08)         VALUE 'CABSET04'.
           05  WS-PGM-VERSION        PIC X(05)           VALUE 'V2.05'.
           05  WS-PGM-APPL           PIC X(04)           VALUE 'SETL'.
           05  WS-PGM-COMPILE      PIC X(08)         VALUE '20190416'.
           05  WS-PARA-NAME          PIC X(30)           VALUE SPACES.

      * RUN CONTEXT.  POPULATED FROM THE SYSIN CARD AND FROM THE
      * JCL SYMBOLICS THAT THE SCHEDULER SUBSTITUTES AT SUBMISSION.
      * NONE OF THESE HAVE DEFAULTS.
       01  WS-RUN-CONTEXT.
           05  WS-RUN-ID             PIC X(12)           VALUE SPACES.
           05  WS-CYCLE-YYDDD.
               10  WS-CYCLE-YY             PIC 9(02)            VALUE 0.
               10  WS-CYCLE-DDD            PIC 9(03)            VALUE 0.
           05  WS-BILL-PERIOD          PIC 9(06)             VALUE 0.
           05  WS-RERUN-NBR            PIC 9(02)             VALUE 0.
           05  WS-JOBNAME            PIC X(08)           VALUE SPACES.
           05  WS-STEPNAME           PIC X(08)           VALUE SPACES.
           05  WS-RETURN-CODE          PIC 9(04)             VALUE 0.
           05  WS-BAL-CHECK            PIC S9(11) COMP-3     VALUE 0.
           05  WS-ERR-CODE           PIC X(04)           VALUE SPACES.
           05  WS-ERR-SEVERITY         PIC X(01)             VALUE 'E'.
           05  WS-RESTART-KEY        PIC X(26)           VALUE SPACES.
           05  WS-JW-QUOT              PIC S9(07) COMP-3     VALUE 0.
           05  WS-SUB-RC               PIC S9(04) COMP       VALUE 0.
           05  WS-GREG-CYCLE           PIC 9(08)             VALUE 0.

       COPY CABSWRK.

       COPY CABSCDR.

       COPY CABSCARR.

       COPY CABSSETL.

       COPY CABSPRNT.

      * ACCEPT AREAS AND SPARE WORK FIELDS.
       01  WS-ACCEPT-AREAS.
           05  WS-ACCEPT-DATE          PIC 9(06)             VALUE 0.
           05  WS-ACCEPT-TIME          PIC 9(08)             VALUE 0.
       01  WS-AD-WORK.
           05  WS-AD-YY                PIC 9(02).
           05  WS-AD-MM                PIC 9(02).
           05  WS-AD-DD                PIC 9(02).
       01  WS-AD-ALT REDEFINES WS-AD-WORK.
           05  WS-AD-YYMM              PIC 9(04).
           05  WS-AD-DAY               PIC 9(02).

      * SYSIN CONTROL CARD.  READ AS 80 BYTES THEN REDEFINED THREE
      * WAYS.  THE CARD TYPE IN COLUMNS 1-2 DECIDES WHICH REDEFINE
      * IS VALID.  NOTHING IN THE PROGRAM ENFORCES THAT AGREEMENT.
      * NO COPYBOOK IS RAISED FOR CONTROL CARDS - SEE CABS-STD-014.
       01  WS-PARM-CARD.
           05  WS-PC-TYPE            PIC X(02)           VALUE SPACES.
           05  WS-PC-REST            PIC X(78)           VALUE SPACES.
       01  WS-PARM-RUN REDEFINES WS-PARM-CARD.
           05  FILLER                  PIC X(02).
           05  WS-PC-RUN-ID            PIC X(12).
           05  WS-PC-CYCLE.
               10  WS-PC-CYCLE-YY          PIC 9(02).
               10  WS-PC-CYCLE-DDD         PIC 9(03).
           05  WS-PC-BILL-PERIOD       PIC 9(06).
           05  WS-PC-RERUN             PIC 9(02).
           05  WS-PC-JOBNAME           PIC X(08).
           05  WS-PC-STEPNAME          PIC X(08).
           05  WS-PC-OPT1              PIC X(01).
           05  WS-PC-OPT2              PIC X(01).
           05  WS-PC-EXTRA             PIC X(35).
       01  WS-PARM-EXT REDEFINES WS-PARM-CARD.
           05  FILLER                  PIC X(45).
           05  WS-PE-SETTLE-PERIOD     PIC 9(06).
           05  WS-PE-ISP-ELEM          PIC X(06).
           05  WS-PE-FILLER            PIC X(23).
       01  WS-PARM-OLD REDEFINES WS-PARM-CARD.
      * THE 1989 CARD.  TWO REGIONS NEVER CONVERTED THEIR JCL AND
      * THE EDIT STILL ACCEPTS IT.  THE FIELDS ARE IN A DIFFERENT
      * ORDER AND THE CYCLE DATE IS SIX DIGITS YYMMDD, NOT YYDDD.
           05  FILLER                  PIC X(02).
           05  WS-PO-CYCLE             PIC 9(05).
           05  WS-PO-RUN-ID            PIC X(12).
           05  WS-PO-YYMMDD            PIC 9(06).
           05  WS-PO-PERIOD            PIC 9(06).
           05  FILLER                  PIC X(20).

      * LOCAL SWITCHES.  THE STANDARD ONES ARRIVE VIA CABSWRK.
       01  WS-LOCAL-SWITCHES.
           05  WS-PARM-EOF-SW          PIC X(01)             VALUE 'N'.
                   88  WS-PARM-EOF             VALUE 'Y'.
           05  WS-CARR-FOUND-SW        PIC X(01)             VALUE 'N'.
                   88  WS-CARR-FOUND           VALUE 'Y'.
           05  WS-ISP-SW               PIC X(01)             VALUE 'N'.
                   88  WS-ISP-BOUND            VALUE 'Y'.
           05  WS-ELIGIBLE-SW          PIC X(01)             VALUE 'Y'.
                   88  WS-ELIGIBLE             VALUE 'Y'.

      * SUBSCRIPTS AND INDEX WORK FIELDS.
       01  WS-SUBSCRIPTS.
           05  WS-SUB1                 PIC S9(05) COMP-3     VALUE 0.
           05  WS-SUB2                 PIC S9(05) COMP-3     VALUE 0.

      * AGGREGATION TABLE.  ONE ENTRY PER COUNTERPARTY CARRIER.
      * THE ISP BOUND MINUTES ARE HELD SEPARATELY BECAUSE THEY ARE
      * SUBJECT TO THE CAP AND THE VOICE MINUTES ARE NOT.
       01  WS-AGG-TABLE.
           05  WS-AG-ENTRY OCCURS 400 TIMES
                   INDEXED BY WS-AG-IX.
               10  WS-AG-OCN               PIC X(04).
               10  WS-AG-TOTAL-MOU         PIC S9(15)V9(02) COMP-3.
               10  WS-AG-ISP-MOU           PIC S9(15)V9(02) COMP-3.
               10  WS-AG-VOICE-MOU         PIC S9(15)V9(02) COMP-3.
               10  WS-AG-RATE              PIC S9(05)V9(05) COMP-3.
               10  WS-AG-CAP               PIC S9(13) COMP-3.
               10  WS-AG-COUNT             PIC S9(11) COMP-3.
               10  WS-AG-ELIG              PIC X(01).
       01  WS-AGG-CTL.
           05  WS-AG-USED              PIC S9(05) COMP-3     VALUE 0.
           05  WS-AG-MAX               PIC S9(05) COMP-3     VALUE 400.
           05  WS-AG-HIT               PIC S9(05) COMP-3     VALUE 0.
           05  WS-AG-FOUND-SW          PIC X(01)             VALUE 'N'.
                   88  WS-AG-FOUND              VALUE 'Y'.

      * AGGREGATION TOTALS.
       01  WS-AGG-TOTALS.
           05  WS-TOT-MOU              PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-TOT-ISP-MOU          PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-TOT-VOICE-MOU        PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-TRANSIT-CNT          PIC S9(11) COMP-3     VALUE 0.
           05  WS-INELIG-CNT           PIC S9(11) COMP-3     VALUE 0.
           05  WS-WORK-MOU             PIC S9(13)V9(02) COMP-3 VALUE 0.

      * AGGREGATION OUTPUT RECORD WITH TWO REDEFINES.
       01  WS-AGG-OUT.
           05  WS-AO-OCN             PIC X(04)           VALUE SPACES.
           05  WS-AO-PERIOD            PIC 9(06)             VALUE 0.
           05  WS-AO-TOTAL-MOU         PIC S9(15)V9(02)      VALUE 0.
           05  WS-AO-ISP-MOU           PIC S9(15)V9(02)      VALUE 0.
           05  WS-AO-VOICE-MOU         PIC S9(15)V9(02)      VALUE 0.
           05  WS-AO-RATE              PIC S9(05)V9(05)      VALUE 0.
           05  WS-AO-CAP               PIC S9(13)            VALUE 0.
           05  WS-AO-COUNT             PIC 9(11)             VALUE 0.
           05  WS-AO-FILLER          PIC X(60)           VALUE SPACES.
       01  WS-AGG-OUT-K REDEFINES WS-AGG-OUT.
           05  WS-AK-KEY               PIC X(10).
           05  WS-AK-REST              PIC X(170).
       01  WS-AGG-OUT-M REDEFINES WS-AGG-OUT.
           05  WS-AM-HEAD              PIC X(10).
           05  WS-AM-MOU-AREA          PIC X(51).
           05  WS-AM-TAIL              PIC X(119).

      * ABEND COMMUNICATION AREA.  PASSED TO CABABEND WHICH ISSUES
      * A USER ABEND WITH THE CODE IN WS-AB-CODE.
       01  WS-ABEND-AREA.
           05  WS-AB-CODE              PIC 9(04) COMP        VALUE 0.
           05  WS-AB-PGM             PIC X(08)           VALUE SPACES.
           05  WS-AB-PARA            PIC X(30)           VALUE SPACES.
           05  WS-AB-TEXT            PIC X(60)           VALUE SPACES.
           05  WS-AB-KEY             PIC X(26)           VALUE SPACES.

      * PARAMETER AREA FOR CABDATCV - THE SHARED DATE CONVERSION
      * SUBROUTINE.  CABDATCV IS 1988 VINTAGE AND STILL PIVOTS ON
      * 70 INTERNALLY.
       01  WS-DATE-PARM.
           05  WS-DP-FUNCTION        PIC X(02)           VALUE SPACES.
           05  WS-DP-YYDDD             PIC 9(05)             VALUE 0.
           05  WS-DP-CCYYMMDD          PIC 9(08)             VALUE 0.
           05  WS-DP-DAYS              PIC S9(07) COMP-3     VALUE 0.
           05  WS-DP-RC                PIC 9(02)             VALUE 0.

      * RATE ELEMENT ATTRIBUTE TABLE.  COLUMN 3 SAYS WHETHER PIU IS
      * APPLIED TO THE ELEMENT AT ALL.  COLUMN 5 CARRIES A ROUNDING
      * RULE THAT IS IGNORED BY THIS PROGRAM AND HONOURED BY CABJUR09
      * - THAT DIVERGENCE IS KNOWN AND HAS BEEN OPEN SINCE 1996.
       01  WS-RELEM-CONST.
           05  FILLER              PIC X(10)         VALUE 'CCLTRMIYYU'.
           05  FILLER              PIC X(10)         VALUE 'CCLORGSYNT'.
           05  FILLER              PIC X(10)         VALUE 'LSWTCHLYNE'.
           05  FILLER              PIC X(10)         VALUE 'TSWTCHINNC'.
           05  FILLER              PIC X(10)         VALUE 'TNDMSWSYNU'.
           05  FILLER              PIC X(10)         VALUE 'LTRANSLYYT'.
           05  FILLER              PIC X(10)         VALUE 'ENTRANIYNE'.
           05  FILLER              PIC X(10)         VALUE 'COMTRNSNNC'.
           05  FILLER              PIC X(10)         VALUE 'DTTRANLYNU'.
           05  FILLER              PIC X(10)         VALUE 'LOCTRMIYNT'.
           05  FILLER              PIC X(10)         VALUE 'LOCORGSYYE'.
           05  FILLER              PIC X(10)         VALUE '800DBQLNNC'.
           05  FILLER              PIC X(10)         VALUE 'SS7ISPIYNU'.
           05  FILLER              PIC X(10)         VALUE 'QUERY1SYNT'.
           05  FILLER              PIC X(10)         VALUE 'DEDTRNLYNE'.
           05  FILLER              PIC X(10)         VALUE 'SPCLACINYC'.
           05  FILLER              PIC X(10)         VALUE 'DS1LOCSYNU'.
           05  FILLER              PIC X(10)         VALUE 'DS3LOCLYNT'.
           05  FILLER              PIC X(10)         VALUE 'UNEPRTIYNE'.
           05  FILLER              PIC X(10)         VALUE 'UNELOPSNNC'.
           05  FILLER              PIC X(10)         VALUE 'COLLOCLYYU'.
           05  FILLER              PIC X(10)         VALUE 'MPBTRNIYNT'.
           05  FILLER              PIC X(10)         VALUE 'RECIPTSYNE'.
           05  FILLER              PIC X(10)         VALUE 'ISPBNDLNNC'.
           05  FILLER              PIC X(10)         VALUE 'TRANSPIYNU'.
           05  FILLER              PIC X(10)         VALUE 'TERMINSYYT'.
           05  FILLER              PIC X(10)         VALUE 'ORIGINLYNE'.
           05  FILLER              PIC X(10)         VALUE 'MOUCHGINNC'.
           05  FILLER              PIC X(10)         VALUE 'SETUPCSYNU'.
           05  FILLER              PIC X(10)         VALUE 'MINCHGLYNT'.
           05  FILLER              PIC X(10)         VALUE 'CARCOMIYYE'.
           05  FILLER              PIC X(10)         VALUE 'LNKCHGSNNC'.
           05  FILLER              PIC X(10)         VALUE 'DBQCHGLYNU'.
           05  FILLER              PIC X(10)         VALUE 'PORTCHIYNT'.
           05  FILLER              PIC X(10)         VALUE 'XCONNCSYNE'.
           05  FILLER              PIC X(10)         VALUE 'ENTFACLNYC'.
           05  FILLER              PIC X(10)         VALUE 'TANDEMIYNU'.
           05  FILLER              PIC X(10)         VALUE 'ENDOFFSYNT'.
           05  FILLER              PIC X(10)         VALUE 'SHRTRNLYNE'.
           05  FILLER              PIC X(10)         VALUE 'WIRTRMINNC'.
       01  WS-RELEM-TABLE REDEFINES WS-RELEM-CONST.
           05  WS-WS-RE-ENTRY OCCURS 40 TIMES
                   INDEXED BY WS-RE-IX.
               10  WS-RE-ELEM              PIC X(06).
               10  WS-RE-JURIS             PIC X(01).
               10  WS-RE-PIU-APPL          PIC X(01).
               10  WS-RE-PLU-APPL          PIC X(01).
               10  WS-RE-ROUND             PIC X(01).

      * PRINT LINE BUILD AREAS.  CABSPRNT SUPPLIES THE OUTPUT LINE;
      * THESE ARE THE ASSEMBLY AREAS.
       01  WS-PAGE-CONTROL.
           05  WS-PAGE-NBR             PIC 9(05) COMP-3      VALUE 0.
           05  WS-LINE-CNT             PIC 9(03) COMP-3      VALUE 99.
           05  WS-MAX-LINES            PIC 9(03) COMP-3      VALUE 58.
       01  WS-HEAD-1.
           05  FILLER                  PIC X(01)             VALUE '1'.
           05  FILLER              PIC X(08)         VALUE 'TELCABS '.
           05  FILLER              PIC X(52)
                   VALUE 'RECIPROCAL COMPENSATION MOU AGGREGATION'.
           05  FILLER                PIC X(06)           VALUE 'PAGE  '.
           05  WS-H1-PAGE              PIC ZZZZ9.
           05  FILLER                PIC X(60)           VALUE SPACES.
       01  WS-HEAD-2.
           05  FILLER                  PIC X(01)             VALUE ' '.
           05  FILLER              PIC X(10)         VALUE 'RUN ID   '.
           05  WS-H2-RUNID             PIC X(12).
           05  FILLER              PIC X(10)         VALUE '  CYCLE  '.
           05  WS-H2-CYCLE             PIC 9(05).
           05  FILLER              PIC X(10)         VALUE '  PGM    '.
           05  WS-H2-PGM               PIC X(08).
           05  FILLER                PIC X(75)           VALUE SPACES.
       01  WS-HEAD-3.
           05  FILLER                  PIC X(01)             VALUE ' '.
           05  FILLER              PIC X(47)
                   VALUE 'OCN  RECORDS      TOTAL-MOU        ISP-MOU'.
           05  FILLER              PIC X(34)
                   VALUE '     VOICE-MOU        RATE     CAP'.
       01  WS-HEAD-4.
           05  FILLER                  PIC X(01)             VALUE ' '.
           05  FILLER                PIC X(131)          VALUE ALL '-'.

      * DETAIL LINE WS-DETAIL-1.
       01  WS-DETAIL-1.
           05  FILLER                  PIC X(01)             VALUE ' '.
           05  WS-D1-OCN               PIC X(04).
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-COUNT             PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-TOTMOU            PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-ISPMOU            PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-VOIMOU            PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-RATE              PIC Z.ZZZZ9.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-CAP               PIC ZZZ,ZZZ,ZZ9.

      * MESSAGE TEXT TABLE.  ADDRESSED BY ERROR CODE SEQUENCE, NOT
      * BY THE CODE ITSELF - INSERTING A CODE IN THE MIDDLE SHIFTS
      * EVERY MESSAGE.  THIS HAS HAPPENED TWICE (1994 AND 2007).
       01  WS-MSG-CONST.
           05  FILLER              PIC X(44)
                   VALUE 'MINUTES AGGREGATED                          '.
           05  FILLER              PIC X(44)
                   VALUE 'CARRIER NOT ELIGIBLE FOR RECIP COMP         '.
           05  FILLER              PIC X(44)
                   VALUE 'CARRIER NOT ON MASTER                       '.
           05  FILLER              PIC X(44)
                   VALUE 'ISP BOUND MINUTES IDENTIFIED                '.
           05  FILLER              PIC X(44)
                   VALUE 'TRANSIT MINUTES EXCLUDED SINCE 2009         '.
           05  FILLER              PIC X(44)
                   VALUE 'VOIP MINUTES INCLUDED SINCE 2013            '.
           05  FILLER              PIC X(44)
                   VALUE 'AGGREGATION TABLE FULL                      '.
           05  FILLER              PIC X(44)
                   VALUE 'ZERO MINUTES RECORD IGNORED                 '.
           05  FILLER              PIC X(44)
                   VALUE 'WIRELESS CARRIER - SEPARATE PATH            '.
           05  FILLER              PIC X(44)
                   VALUE 'END OF AGGREGATION RUN                      '.
       01  WS-MSG-TABLE REDEFINES WS-MSG-CONST.
           05  WS-MSG-ENTRY OCCURS 10 TIMES
                   INDEXED BY WS-MSG-IX.
               10  WS-MSG-TEXT             PIC X(44).

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
      * NOTHING IS DEFAULTED.  IF THE SCHEDULER DID NOT SUPPLY A
      * CYCLE DATE THE STEP ABENDS - IT DOES NOT ASSUME TODAY.
           MOVE 'P1000-INIT' TO WS-PARA-NAME.
           ACCEPT WS-ACCEPT-DATE FROM DATE.
           ACCEPT WS-ACCEPT-TIME FROM TIME.
           OPEN INPUT  RECIP-IN-FILE
                       CARRIER-MASTER
                       PARM-FILE
           OPEN OUTPUT RECIP-AGG-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 5401 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-RECIPIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 5402 TO WS-AB-CODE
               MOVE 'OPEN FAILED DA-I-CARRMAST' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 5403 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-RECAGG' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 4901 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-SYSIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 4801 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-CTLOUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 4802 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-SUSPOUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 4803 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-REPORT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           MOVE WS-ACCEPT-DATE         TO WS-AD-WORK.
           MOVE WS-AD-YY               TO DW-CUR-YY.
           PERFORM P1100-READ-PARM THRU P1100-EXIT.
           PERFORM P1200-EDIT-PARM THRU P1200-EXIT.
           MOVE ZERO TO WS-TOT-MOU WS-TOT-ISP-MOU
                        WS-TOT-VOICE-MOU WS-TRANSIT-CNT
                        WS-INELIG-CNT WS-AG-USED.
           PERFORM P6000-HEADING THRU P6000-EXIT.
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
           DISPLAY WS-PGM-NAME ' STARTED - RUN ' WS-RUN-ID.
           DISPLAY '  CYCLE YYDDD  ' WS-CYCLE-YYDDD.
           DISPLAY '  BILL PERIOD  ' WS-BILL-PERIOD.

       P1000-EXIT.
           EXIT.

       P1100-READ-PARM.
      * THE SYSIN CARD CARRIES THE VALUES THE SCHEDULER SUBSTITUTED
      * INTO THE JCL AT SUBMISSION TIME.  THERE ARE NO DEFAULTS - AN
      * ABSENT CARD IS A FATAL ERROR, NOT A DEFAULTED RUN.
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
      * EDIT THE CONTROL CARD.  EVERY FIELD IS MANDATORY.  THE 1989
      * CARD FORMAT IS STILL ACCEPTED VIA THE WS-PARM-OLD REDEFINE.
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
           IF WS-PE-SETTLE-PERIOD NOT NUMERIC
               MOVE WS-PC-BILL-PERIOD TO WS-PE-SETTLE-PERIOD.
           IF WS-PE-ISP-ELEM = SPACES
               MOVE 'ISPBND' TO WS-PE-ISP-ELEM.

       P1200-EXIT.
           EXIT.


      *****************************************************************
      * S200-AGGREGATION                                              *
      * AGGREGATE TERMINATING MINUTES.                                *
      *****************************************************************
       S200-AGGREGATION SECTION.

       P2000-PROCESS.
      * ONE TERMINATING RECORD PER PASS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ THRU P2100-EXIT.
           IF WS-EOF
               GO TO P2000-EXIT.
           MOVE RCI-RECORD TO CABS-CDR-RECORD.
           MOVE CD-KEY TO WS-RESTART-KEY.
           IF CD-VC-CHG-MIN = ZERO
               ADD 1 TO WS-SUMM-CNT
               GO TO P2000-EXIT.
           PERFORM P2200-CARRIER-LOOKUP THRU P2200-EXIT.
           IF NOT WS-CARR-FOUND
               MOVE EC-OCN-UNKNOWN TO WS-ERR-CODE
               MOVE 'E' TO WS-ERR-SEVERITY
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               GO TO P2000-EXIT.
           IF NOT WS-ELIGIBLE
               ADD 1 TO WS-INELIG-CNT
               ADD 1 TO WS-SUMM-CNT
               GO TO P2000-EXIT.
           PERFORM P2300-CLASSIFY THRU P2400-EXIT.
           PERFORM P3000-ACCUMULATE THRU P3000-EXIT.
           ADD 1 TO WS-SUMM-CNT.

       P2000-EXIT.
           EXIT.

       P2100-READ.
      * SEQUENTIAL READ OF TERMINATING USAGE.
           READ RECIP-IN-FILE
               AT END
                   MOVE 'Y' TO WS-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00'
               MOVE 3540 TO WS-AB-CODE
               MOVE 'READ ERROR UT-S-RECIPIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-READ-CNT.

       P2100-EXIT.
           EXIT.

       P2200-CARRIER-LOOKUP.
      * FETCH THE RECIPROCAL RATE AND THE ISP CAP FOR THE CARRIER.
      * A CARRIER THAT IS NOT ELIGIBLE IS COUNTED AND DROPPED - THE
      * MINUTES ARE STILL TERMINATED, THEY ARE SIMPLY NOT PAID FOR.
           MOVE 'P2200-CARRIER-LOOKUP' TO WS-PARA-NAME.
           MOVE 'N' TO WS-CARR-FOUND-SW.
           MOVE 'Y' TO WS-ELIGIBLE-SW.
           MOVE CD-OCN TO CRM-KEY.
           READ CARRIER-MASTER
               INVALID KEY
                   GO TO P2200-EXIT.
           MOVE CRM-RECORD TO CABS-CARRIER-RECORD.
           MOVE 'Y' TO WS-CARR-FOUND-SW.
           IF CR-RECIP-COMP-ELIG NOT = 'Y'
               MOVE 'N' TO WS-ELIGIBLE-SW.
           IF CR-WIRELESS
               MOVE 'N' TO WS-ELIGIBLE-SW.

       P2200-EXIT.
           EXIT.

       P2300-CLASSIFY.
      * DECIDE WHETHER THE MINUTES ARE ISP BOUND OR VOICE.  THE
      * RATE ELEMENT ON THE RECORD IS THE ONLY EVIDENCE.  THIS
      * PARAGRAPH HAS NO EXIT OF ITS OWN - CONTROL DROPS THROUGH
      * INTO P2400-TRANSIT-TEST AND THE CALLER PERFORMS P2300 THRU
      * P2400-EXIT.
           MOVE 'P2300-CLASSIFY' TO WS-PARA-NAME.
           MOVE 'N' TO WS-ISP-SW.
           MOVE CD-VC-CHG-MIN TO WS-WORK-MOU.
           IF CD-RATE-ELEM = WS-PE-ISP-ELEM
               MOVE 'Y' TO WS-ISP-SW
               GO TO P2400-TRANSIT-TEST.
           IF CD-USAGE-TYPE = 'I'
               MOVE 'Y' TO WS-ISP-SW.

       P2400-TRANSIT-TEST.
      * TRANSIT MINUTES ARE MINUTES THAT MERELY PASSED THROUGH THE
      * TANDEM AND TERMINATED SOMEWHERE ELSE.  THEY WERE EXCLUDED
      * FROM RECIPROCAL COMPENSATION IN 2009.  ENTERED BOTH BY FALL
      * THROUGH FROM P2300 AND BY THE GO TO ABOVE.
           IF CD-VC-TANDEM-IND = 'T'
               ADD 1 TO WS-TRANSIT-CNT
               MOVE ZERO TO WS-WORK-MOU.

       P2400-EXIT.
           EXIT.


      *****************************************************************
      * S300-ACCUMULATE                                               *
      * BUILD THE AGGREGATION TABLE.                                  *
      *****************************************************************
       S300-ACCUMULATE SECTION.

       P3000-ACCUMULATE.
      * FIND OR CREATE THE CARRIER ENTRY AND ADD THE MINUTES.
           MOVE 'P3000-ACCUMULATE' TO WS-PARA-NAME.
           MOVE 'N' TO WS-AG-FOUND-SW.
           MOVE 1 TO WS-SUB1.
           PERFORM P3050-FIND-ENTRY THRU P3050-EXIT
               UNTIL WS-SUB1 > WS-AG-USED
                  OR WS-AG-FOUND.
           IF NOT WS-AG-FOUND
               PERFORM P3060-ADD-ENTRY THRU P3060-EXIT.
           IF NOT WS-AG-FOUND
               GO TO P3000-EXIT.
           SET WS-AG-IX TO WS-AG-HIT.
           ADD WS-WORK-MOU TO WS-AG-TOTAL-MOU (WS-AG-IX).
           ADD 1 TO WS-AG-COUNT (WS-AG-IX).
           IF WS-ISP-BOUND
               ADD WS-WORK-MOU TO WS-AG-ISP-MOU (WS-AG-IX)
               ADD WS-WORK-MOU TO WS-TOT-ISP-MOU
           ELSE
               ADD WS-WORK-MOU TO WS-AG-VOICE-MOU (WS-AG-IX)
               ADD WS-WORK-MOU TO WS-TOT-VOICE-MOU.
           ADD WS-WORK-MOU TO WS-TOT-MOU.
           ADD WS-WORK-MOU TO WS-ACC-MINUTES.

       P3000-EXIT.
           EXIT.

       P3050-FIND-ENTRY.
      * ONE TABLE COMPARE.
           IF WS-AG-OCN (WS-SUB1) = CD-OCN
               MOVE 'Y' TO WS-AG-FOUND-SW
               MOVE WS-SUB1 TO WS-AG-HIT
               GO TO P3050-EXIT.
           ADD 1 TO WS-SUB1.

       P3050-EXIT.
           EXIT.

       P3060-ADD-ENTRY.
      * ADD A CARRIER ENTRY CARRYING THE RATE AND THE CAP AS THEY
      * STAND ON THE CARRIER MASTER TODAY - NOT AS THEY STOOD
      * DURING THE SETTLEMENT PERIOD.  A RATE CHANGE MID PERIOD IS
      * THEREFORE APPLIED TO THE WHOLE PERIOD.
           IF WS-AG-USED NOT < WS-AG-MAX
               MOVE EC-OUT-OF-BALANCE TO WS-ERR-CODE
               MOVE 'E' TO WS-ERR-SEVERITY
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               GO TO P3060-EXIT.
           ADD 1 TO WS-AG-USED.
           MOVE WS-AG-USED TO WS-AG-HIT.
           SET WS-AG-IX TO WS-AG-HIT.
           MOVE CD-OCN TO WS-AG-OCN (WS-AG-IX).
           MOVE ZERO TO WS-AG-TOTAL-MOU (WS-AG-IX).
           MOVE ZERO TO WS-AG-ISP-MOU (WS-AG-IX).
           MOVE ZERO TO WS-AG-VOICE-MOU (WS-AG-IX).
           MOVE ZERO TO WS-AG-COUNT (WS-AG-IX).
           MOVE CR-RECIP-RATE TO WS-AG-RATE (WS-AG-IX).
           MOVE CR-ISP-CAP-MOU TO WS-AG-CAP (WS-AG-IX).
           MOVE CR-RECIP-COMP-ELIG TO WS-AG-ELIG (WS-AG-IX).
           MOVE 'Y' TO WS-AG-FOUND-SW.

       P3060-EXIT.
           EXIT.


      *****************************************************************
      * S400-WRITE                                                    *
      * WRITE THE AGGREGATION FILE.                                   *
      *****************************************************************
       S400-WRITE SECTION.

       P4000-WRITE-AGG.
      * ONE RECORD PER CARRIER AT END OF FILE.
           MOVE 'P4000-WRITE-AGG' TO WS-PARA-NAME.
           MOVE 1 TO WS-SUB2.
           PERFORM P4100-WRITE-ONE THRU P4100-EXIT
               UNTIL WS-SUB2 > WS-AG-USED.

       P4000-EXIT.
           EXIT.

       P4100-WRITE-ONE.
      * ONE AGGREGATION RECORD.
           SET WS-AG-IX TO WS-SUB2.
           MOVE SPACES TO WS-AGG-OUT.
           MOVE WS-AG-OCN (WS-AG-IX) TO WS-AO-OCN.
           MOVE WS-PE-SETTLE-PERIOD TO WS-AO-PERIOD.
           MOVE WS-AG-TOTAL-MOU (WS-AG-IX) TO WS-AO-TOTAL-MOU.
           MOVE WS-AG-ISP-MOU (WS-AG-IX) TO WS-AO-ISP-MOU.
           MOVE WS-AG-VOICE-MOU (WS-AG-IX) TO WS-AO-VOICE-MOU.
           MOVE WS-AG-RATE (WS-AG-IX) TO WS-AO-RATE.
           MOVE WS-AG-CAP (WS-AG-IX) TO WS-AO-CAP.
           MOVE WS-AG-COUNT (WS-AG-IX) TO WS-AO-COUNT.
           MOVE WS-AGG-OUT TO RAG-RECORD.
           WRITE RAG-RECORD.
           ADD 1 TO WS-WRITE-CNT.
           SUBTRACT 1 FROM WS-SUMM-CNT.
           PERFORM P6100-DETAIL THRU P6100-EXIT.
           ADD 1 TO WS-SUB2.

       P4100-EXIT.
           EXIT.


      *****************************************************************
      * S600-REPORT                                                   *
      * AGGREGATION REGISTER.                                         *
      *****************************************************************
       S600-REPORT SECTION.

       P6000-HEADING.
      * PAGE HEADINGS.
           ADD 1 TO WS-PAGE-NBR.
           MOVE WS-PAGE-NBR            TO WS-H1-PAGE.
           MOVE WS-RUN-ID              TO WS-H2-RUNID.
           MOVE WS-CYCLE-YYDDD         TO WS-H2-CYCLE.
           MOVE WS-PGM-NAME            TO WS-H2-PGM.
           WRITE PRT-RECORD FROM WS-HEAD-1 AFTER ADVANCING PAGE.
           WRITE PRT-RECORD FROM WS-HEAD-2 AFTER ADVANCING 1 LINES.
           WRITE PRT-RECORD FROM WS-HEAD-3 AFTER ADVANCING 2 LINES.
           WRITE PRT-RECORD FROM WS-HEAD-4 AFTER ADVANCING 1 LINES.
           MOVE 6 TO WS-LINE-CNT.

       P6000-EXIT.
           EXIT.

       P6100-DETAIL.
      * ONE LINE PER CARRIER.
           IF WS-LINE-CNT > WS-MAX-LINES
               PERFORM P6000-HEADING THRU P6000-EXIT.
           MOVE WS-AG-OCN (WS-AG-IX) TO WS-D1-OCN.
           MOVE WS-AG-COUNT (WS-AG-IX) TO WS-D1-COUNT.
           MOVE WS-AG-TOTAL-MOU (WS-AG-IX) TO WS-D1-TOTMOU.
           MOVE WS-AG-ISP-MOU (WS-AG-IX) TO WS-D1-ISPMOU.
           MOVE WS-AG-VOICE-MOU (WS-AG-IX) TO WS-D1-VOIMOU.
           MOVE WS-AG-RATE (WS-AG-IX) TO WS-D1-RATE.
           MOVE WS-AG-CAP (WS-AG-IX) TO WS-D1-CAP.
           WRITE PRT-RECORD FROM WS-DETAIL-1 AFTER ADVANCING 1 LINES.
           ADD 1 TO WS-LINE-CNT.

       P6100-EXIT.
           EXIT.


      *****************************************************************
      * S800-CONTROL                                                  *
      * BALANCING AND SUSPENSE.  P8000 IS NOT OPTIONAL.               *
      *****************************************************************
       S800-CONTROL SECTION.

       P7000-SUSPEND.
      * WRITE A SUSPENSE RECORD.  THE CALLER SETS WS-ERR-CODE AND
      * WS-ERR-SEVERITY BEFORE PERFORMING THIS PARAGRAPH.
           MOVE SPACES                 TO CABS-SUSPENSE-RECORD.
           MOVE WS-ERR-CODE            TO SU-ERR-CODE.
           MOVE WS-ERR-SEVERITY        TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME            TO SU-DETECT-PGM.
           MOVE WS-PARA-NAME           TO SU-DETECT-PARA.
           MOVE WS-RUN-ID              TO SU-RUN-ID.
           MOVE CABS-CDR-RECORD TO SU-ORIG-RECORD.
           CALL 'CABERRWR' USING CABS-SUSPENSE-RECORD
                                  WS-SUB-RC.
           WRITE SUS-RECORD FROM CABS-SUSPENSE-RECORD.
           ADD 1 TO WS-REJECT-CNT.
           MOVE 'Y' TO WS-ERROR-SW.

       P7000-EXIT.
           EXIT.

       P8000-CONTROL.
      * MANDATORY CONTROL RECORD.  THE BALANCING EQUATION IS
      *   CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARISED
      *           + CT-CARRIED-FWD
      * A FAILURE HERE SETS CT-OUT-OF-BAL AND RC 0008.  THE NIGHTLY
      * CONTROL REPORT (CABCTL02) READS EVERY CONTROL RECORD AND
      * HALTS THE CYCLE ON ANY OUT OF BALANCE PROCESS.
           MOVE SPACES                 TO CABS-CONTROL-RECORD.
           MOVE WS-RUN-ID              TO CT-RUN-ID.
           MOVE WS-PGM-NAME            TO CT-PROCESS-ID.
           MOVE 230                    TO CT-STEP-SEQ.
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
           DISPLAY '--------------------------------------------'.
           DISPLAY WS-PGM-NAME ' V' WS-PGM-VERSION ' RUN ' WS-RUN-ID.
           PERFORM P4000-WRITE-AGG THRU P4000-EXIT.
           DISPLAY 'CARRIERS         ' WS-AG-USED.
           DISPLAY 'TOTAL MOU        ' WS-TOT-MOU.
           DISPLAY 'ISP BOUND MOU    ' WS-TOT-ISP-MOU.
           DISPLAY 'VOICE MOU        ' WS-TOT-VOICE-MOU.
           DISPLAY 'TRANSIT EXCLUDED ' WS-TRANSIT-CNT.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE RECIP-IN-FILE
                 CARRIER-MASTER
                 RECIP-AGG-FILE
                 PARM-FILE
                 CONTROL-FILE
                 SUSPENSE-FILE
                 PRINT-FILE
           .
           MOVE WS-RETURN-CODE TO RETURN-CODE.

       P9000-EXIT.
           EXIT.

       P9500-ABEND.
      * UNRECOVERABLE ERROR.  CABABEND ISSUES A USER ABEND SO THAT
      * THE STEP FAILS VISIBLY RATHER THAN COMPLETING WITH BAD DATA.
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

