      *****************************************************************
      * CABBIL02 - BILL DETAIL LINE ASSEMBLY - VARIABLE LENGTH        *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               RATIN   TELCABS.CABS.RATED.JUR(0)         (LOCAL)*
      *               TRIGIN  TELCABS.CABS.BILLTRIG(0)          (LOCAL)*
      *               SYSIN   INSTREAM CONTROL CARD             (LOCAL)*
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               BDTLOUT TELCABS.CABS.BILLDTL(+1)          CABSBILL*
      *               SUSPOUT TELCABS.CABS.BILL.SUSP(+1)        CABSERR*
      *               REPORT  SYSOUT PRINT                      CABSPRNT*
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-SUMMARISED + CT-REJECTED + CT-CARRIED-FWD*
      *               CT-WRITTEN IS THE DETAIL LINE COUNT, NOT THE INPUT COUNT*
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY (BAN/PERIOD/SECTION/SEQ)*
      * REVISION HISTORY                                              *
      *   V1.00  1988-02-15  R.T.WHEELER  INITIAL RELEASE - ONE DETAIL LINE*
      *                      PER RATE ELEMENT, NO GROUPING            *
      *   V1.06  1990-11-27  D.OKONKWO    ELEMENTS NOW GROUPED INTO A SINGLE*
      *                      LINE WITH THE OCCURS DEPENDING ON        *
      *   V1.11  1993-07-09  M.J.FERRARO  ELEMENT NAME TABLE MOVED IN LINE -*
      *                      THE CARD FILE WAS LOST IN THE MOVE       *
      *   V1.18  1996-05-14  J.M.CASTILLO Y2K REVIEW - PERIOD TEXT ROUTINE*
      *                      CONFIRMED CENTURY SAFE VIA CABDATCV      *
      *   V2.00  1998-09-22  P.NAIR       SECTION NOW DERIVED FROM THE RATE*
      *                      ELEMENT PREFIX, NOT TAKEN FROM THE       *
      *                      RATED RECORD - SEE CR CABS-1998-114      *
      *   V2.04  2001-03-30  A.BUKOWSKI   CONTINUATION LINES ADDED FOR BANS*
      *                      WITH MORE THAN FORTY ELEMENTS            *
      *   V2.09  2004-06-11  T.VANCE      ROUND DELTA NOW CARRIED ON THE*
      *                      RECORD SO THE PENNY CAN BE TRACED        *
      *   V2.14  2008-10-02  S.MARCHETTI  TRIGGER TABLE RAISED FROM 800 TO*
      *                      2000 ENTRIES AFTER THE CLEC INTAKE       *
      *   V3.00  2012-04-18  R.KAMINSKI   ELEMENT CLASS VALIDATION SPLIT INTO*
      *                      EIGHT ROUTINES REACHED BY GO TO          *
      *                      DEPENDING ON - THE OLD IF NEST WAS       *
      *                      TWENTY THREE LEVELS DEEP                 *
      *   V3.04  2019-02-26  G.PRZYBYLSKI RECOMPILE - CABSBILL OCCURS LIMIT*
      *                      CONFIRMED UNCHANGED AT FORTY             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABBIL02.
       AUTHOR. TELCABS APPLICATIONS - BILLING TEAM.
      *****************************************************************
      * THE DETAIL LINE ASSEMBLER.  FOLDS RATED AND JURISDICTIONALISED*
      * USAGE INTO VARIABLE LENGTH BILL DETAIL RECORDS CARRYING ONE TO*
      * FORTY RATE ELEMENTS EACH, DERIVES THE BILL SECTION, ASSEMBLES *
      * THE PRINTED DESCRIPTION AND ROUNDS THE LINE TOTAL.            *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RATED-IN-FILE ASSIGN TO UT-S-RATIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT TRIG-IN-FILE ASSIGN TO UT-S-TRIGIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-TABLE.
           SELECT BILL-DTL-FILE ASSIGN TO UT-S-BDTLOUT
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
      * RATIN - RATED AND JURISDICTIONALISED USAGE.  ONE              *
      * RECORD PER RATE ELEMENT.  PRODUCED BY CABJUR04 AND            *
      * CABJUR05 AND SORTED BY CABSRT10.                              *
      *****************************************************************
       FD  RATED-IN-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       01  CABS-RATED-DETAIL-RECORD.
           05  RD-KEY.
               10  RD-BAN              PIC X(13).
               10  RD-BILL-PERIOD      PIC 9(06).
               10  RD-SECTION          PIC X(02).
               10  RD-LINE-SEQ         PIC 9(07) COMP-3.
           05  RD-OCN                  PIC X(04).
           05  RD-JURIS-CD             PIC X(01).
           05  RD-STATE-CD             PIC X(02).
           05  RD-RATE-ELEM            PIC X(06).
           05  RD-ELEM-SEQ             PIC 9(02).
           05  RD-QTY                  PIC S9(13)V9(02) COMP-3.
           05  RD-RATE                 PIC S9(05)V9(05) COMP-3.
           05  RD-AMOUNT               PIC S9(11)V9(05) COMP-3.
           05  RD-ROUND-RULE           PIC X(01).
           05  RD-SRC-PROCESS          PIC X(08).
           05  RD-CYCLE-YYDDD          PIC 9(05).
           05  RD-FILLER               PIC X(123).
      *****************************************************************
      * TRIGIN - THE TRIGGER FILE FROM CABBIL01.                      *
      *****************************************************************
       FD  TRIG-IN-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       01  TRIG-IN-RECORD                   PIC X(200).
      *****************************************************************
      * BDTLOUT - BILL DETAIL, VARIABLE LENGTH, 108 TO 1647 BYTES.    *
      * THE RECORD LENGTH IS SET BY BD-ELEM-CNT.  A PROGRAM THAT      *
      * MOVES ONE OF THESE TO A FIXED AREA LOSES THE TAIL.            *
      * VARIABLE LENGTH HANDLING PER CABS-STD-019.                    *
      *****************************************************************
       FD  BILL-DTL-FILE
           RECORDING MODE IS V
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD IS VARYING IN SIZE FROM 108 TO 1647
               CHARACTERS DEPENDING ON BD-ELEM-CNT.
       COPY CABSBILL.
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
           05  WS-PGM-NAME             PIC X(08) VALUE 'CABBIL02'.
           05  WS-PGM-VERSION          PIC X(05) VALUE 'V3.04'.
           05  WS-PGM-APPL             PIC X(04) VALUE 'CABS'.
           05  WS-PGM-COMPILE          PIC X(08) VALUE '20190226'.
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
      *****************************************************************
      * THE BILL DETAIL LAYOUT IS THE OUTPUT RECORD AND IS DECLARED IN*
      * THE FILE SECTION.  THE COPY BELOW GIVES A SECOND, WORKING STORAGE*
      * COPY USED WHILE THE GROUP IS STILL OPEN.  BOTH ARE THE SAME   *
      * LAYOUT - THE FILE ONE IS WRITTEN, THE WORKING ONE IS ACCUMULATED.*
      *****************************************************************
       COPY CABSWRK.

       COPY CABSRATE.

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
           05  WS-PE-MAX-ELEM          PIC 9(02).
           05  WS-PE-SUPPRESS-SW       PIC X(01).
           05  WS-PE-CONT-SW           PIC X(01).
           05  WS-PE-SECTION-FROM      PIC X(02).
           05  WS-PE-SECTION-THRU      PIC X(02).
           05  WS-PE-FILLER            PIC X(27).
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
           05  WS-RAT-EOF-SW           PIC X(01) VALUE 'N'.
               88  WS-RAT-EOF          VALUE 'Y'.
           05  WS-TRG-EOF-SW           PIC X(01) VALUE 'N'.
               88  WS-TRG-EOF          VALUE 'Y'.
           05  WS-TBL-EOF-SW           PIC X(01) VALUE 'N'.
               88  WS-TBL-EOF          VALUE 'Y'.
           05  WS-GROUP-OPEN-SW        PIC X(01) VALUE 'N'.
               88  WS-GROUP-OPEN       VALUE 'Y'.
           05  WS-BREAK-SW             PIC X(01) VALUE 'N'.
               88  WS-BREAK            VALUE 'Y'.
           05  WS-SUPPRESS-SW          PIC X(01) VALUE 'N'.
               88  WS-SUPPRESSING      VALUE 'Y'.
           05  WS-TRIG-FOUND-SW        PIC X(01) VALUE 'N'.
               88  WS-TRIG-FOUND       VALUE 'Y'.
           05  WS-CONT-SW              PIC X(01) VALUE 'N'.
               88  WS-CONTINUING       VALUE 'Y'.
           05  WS-ELEM-FOUND-SW        PIC X(01) VALUE 'N'.
               88  WS-ELEM-FOUND       VALUE 'Y'.
      *****************************************************************
      * THE RATE ELEMENT NAME TABLE.  SIXTY ENTRIES, LOADED AS LITERALS*
      * BECAUSE THE 1988 VERSION READ THEM FROM A CARD FILE AND THE CARD*
      * FILE WAS LOST IN THE 1993 DATA CENTRE MOVE.  NEW ELEMENTS ARE *
      * ADDED HERE AND THE MODULE IS RECOMPILED.                      *
      *****************************************************************
       01  WS-ELEM-NAME-TABLE.
           05  FILLER PIC X(30) VALUE
               'TANSW LOCAL SWITCHING         '.
           05  FILLER PIC X(30) VALUE
               'TANEO END OFFICE SWITCHING    '.
           05  FILLER PIC X(30) VALUE
               'TANTS TANDEM SWITCHING        '.
           05  FILLER PIC X(30) VALUE
               'TANLT LOCAL TRANSPORT TERMINATN'.
           05  FILLER PIC X(30) VALUE
               'TANLTFLOCAL TRANSPORT FACILITY'.
           05  FILLER PIC X(30) VALUE
               'TANCCLCARRIER COMMON LINE     '.
           05  FILLER PIC X(30) VALUE
               'TANIC INTERCONNECTION CHARGE  '.
           05  FILLER PIC X(30) VALUE
               'TANRICRESIDUAL INTERCONNECTION'.
           05  FILLER PIC X(30) VALUE
               'TANTICTRANSPORT INTERCONNECT  '.
           05  FILLER PIC X(30) VALUE
               'TANEUCEND USER COMMON LINE    '.
           05  FILLER PIC X(30) VALUE
               'TANPICPRESUBSCRIBED IXC CHARGE'.
           05  FILLER PIC X(30) VALUE
               'TANSLCSUBSCRIBER LINE CHARGE  '.
           05  FILLER PIC X(30) VALUE
               'ORGSW ORIG LOCAL SWITCHING    '.
           05  FILLER PIC X(30) VALUE
               'ORGEO ORIG END OFFICE         '.
           05  FILLER PIC X(30) VALUE
               'ORGTS ORIG TANDEM SWITCHING   '.
           05  FILLER PIC X(30) VALUE
               'ORGLT ORIG LOCAL TRANSPORT    '.
           05  FILLER PIC X(30) VALUE
               'ORGCCLORIG CARRIER COMMON LINE'.
           05  FILLER PIC X(30) VALUE
               'ORGDA DIRECTORY ASSISTANCE    '.
           05  FILLER PIC X(30) VALUE
               'ORGOPROPERATOR ASSISTED CALL  '.
           05  FILLER PIC X(30) VALUE
               'ORG800800 DATABASE QUERY      '.
           05  FILLER PIC X(30) VALUE
               'ORGLNPLNP DATABASE QUERY      '.
           05  FILLER PIC X(30) VALUE
               'ORGCNACNAM DATABASE QUERY     '.
           05  FILLER PIC X(30) VALUE
               'SPCDS0DS0 CHANNEL TERMINATION '.
           05  FILLER PIC X(30) VALUE
               'SPCDS1DS1 CHANNEL TERMINATION '.
           05  FILLER PIC X(30) VALUE
               'SPCDS3DS3 CHANNEL TERMINATION '.
           05  FILLER PIC X(30) VALUE
               'SPCVG VOICE GRADE CHANNEL     '.
           05  FILLER PIC X(30) VALUE
               'SPCMILSPECIAL ACCESS MILEAGE  '.
           05  FILLER PIC X(30) VALUE
               'SPCFXDSPECIAL ACCESS FIXED    '.
           05  FILLER PIC X(30) VALUE
               'SPCOC3OC3 CHANNEL TERMINATION '.
           05  FILLER PIC X(30) VALUE
               'SPCOC12OC12 CHANNEL TERM       '.
           05  FILLER PIC X(30) VALUE
               'SPCMUXMULTIPLEXING            '.
           05  FILLER PIC X(30) VALUE
               'SPCEXTEXTENDED SUPERFRAME     '.
           05  FILLER PIC X(30) VALUE
               'UNELP UNE LOOP 2 WIRE         '.
           05  FILLER PIC X(30) VALUE
               'UNELP4UNE LOOP 4 WIRE         '.
           05  FILLER PIC X(30) VALUE
               'UNEPRTUNE SWITCH PORT         '.
           05  FILLER PIC X(30) VALUE
               'UNETRNUNE DEDICATED TRANSPORT '.
           05  FILLER PIC X(30) VALUE
               'UNESHRUNE SHARED TRANSPORT    '.
           05  FILLER PIC X(30) VALUE
               'UNESGNUNE SIGNALLING SS7      '.
           05  FILLER PIC X(30) VALUE
               'UNEDSLUNE HIGH FREQ SPECTRUM  '.
           05  FILLER PIC X(30) VALUE
               'UNESUBUNE SUBLOOP             '.
           05  FILLER PIC X(30) VALUE
               'RCPTRMRECIP COMP TERMINATION  '.
           05  FILLER PIC X(30) VALUE
               'RCPTRNRECIP COMP TRANSPORT    '.
           05  FILLER PIC X(30) VALUE
               'RCPISPISP BOUND TERMINATION   '.
           05  FILLER PIC X(30) VALUE
               'RCPTNDRECIP COMP TANDEM       '.
           05  FILLER PIC X(30) VALUE
               'MPBLTRMEET POINT TRANSPORT    '.
           05  FILLER PIC X(30) VALUE
               'MPBLSWMEET POINT SWITCHING    '.
           05  FILLER PIC X(30) VALUE
               'MPBRESMEET POINT RESIDUAL     '.
           05  FILLER PIC X(30) VALUE
               'MPBFACMEET POINT FACILITY     '.
           05  FILLER PIC X(30) VALUE
               'NRCINSNON RECUR INSTALLATION  '.
           05  FILLER PIC X(30) VALUE
               'NRCCHGNON RECUR SERVICE ORDER '.
           05  FILLER PIC X(30) VALUE
               'NRCEXPNON RECUR EXPEDITE      '.
           05  FILLER PIC X(30) VALUE
               'NRCDSCNON RECUR DISCONNECT    '.
           05  FILLER PIC X(30) VALUE
               'ADJCRDBILLING ADJUSTMENT CREDIT'.
           05  FILLER PIC X(30) VALUE
               'ADJDBTBILLING ADJUSTMENT DEBIT'.
           05  FILLER PIC X(30) VALUE
               'ADJRSTFACTOR RESTATEMENT      '.
           05  FILLER PIC X(30) VALUE
               'ADJDSPDISPUTE SETTLEMENT      '.
           05  FILLER PIC X(30) VALUE
               'TAXFEDFEDERAL EXCISE TAX      '.
           05  FILLER PIC X(30) VALUE
               'TAXSTASTATE SALES TAX         '.
           05  FILLER PIC X(30) VALUE
               'TAXLOCLOCAL SURCHARGE         '.
           05  FILLER PIC X(30) VALUE
               'TAXE91E911 SURCHARGE          '.
       01  WS-ELEM-NAME-R REDEFINES WS-ELEM-NAME-TABLE.
           05  WS-EN-ENTRY OCCURS 60 TIMES INDEXED BY WS-EN-X.
               10  WS-EN-CODE          PIC X(06).
               10  WS-EN-NAME          PIC X(24).
      *****************************************************************
      * THE BILL SECTION TABLE.  SECTION CODE, PRINTED NAME AND THE   *
      * SECTION CLASS.  THE CLASS DRIVES THE SUMMARY PAGE LATER IN THE*
      * STREAM AND IS THE ONLY PLACE THE GROUPING IS DEFINED.         *
      *****************************************************************
       01  WS-SECTION-TABLE.
           05  FILLER PIC X(32) VALUE
               'U1SWITCHED ACCESS USAGE       U'.
           05  FILLER PIC X(32) VALUE
               'U2ORIGINATING ACCESS USAGE    U'.
           05  FILLER PIC X(32) VALUE
               'U3SPECIAL ACCESS USAGE        U'.
           05  FILLER PIC X(32) VALUE
               'C1RECURRING ACCESS CHARGES    C'.
           05  FILLER PIC X(32) VALUE
               'C2NON RECURRING CHARGES       C'.
           05  FILLER PIC X(32) VALUE
               'C3UNBUNDLED NETWORK ELEMENTS  C'.
           05  FILLER PIC X(32) VALUE
               'C4INTERCONNECTION CHARGES     C'.
           05  FILLER PIC X(32) VALUE
               'S1RECIPROCAL COMPENSATION     S'.
           05  FILLER PIC X(32) VALUE
               'S2MEET POINT BILLING          S'.
           05  FILLER PIC X(32) VALUE
               'A1ADJUSTMENTS AND RESTATEMENT A'.
           05  FILLER PIC X(32) VALUE
               'T1TAXES AND SURCHARGES        T'.
           05  FILLER PIC X(32) VALUE
               'Z1UNCLASSIFIED                Z'.
       01  WS-SECTION-TABLE-R REDEFINES WS-SECTION-TABLE.
           05  WS-ST-ENTRY OCCURS 12 TIMES INDEXED BY WS-ST-X.
               10  WS-ST-CODE          PIC X(02).
               10  WS-ST-NAME          PIC X(28).
               10  WS-ST-CLASS         PIC X(01).
               10  WS-ST-FILLER        PIC X(01).
      *****************************************************************
      * RATE ELEMENT PREFIX TO SECTION CROSS REFERENCE.  THE PREFIX IS*
      * THE FIRST THREE CHARACTERS OF THE ELEMENT CODE.  ANYTHING THAT*
      * DOES NOT MATCH LANDS IN SECTION Z1.                           *
      *****************************************************************
       01  WS-ELEM-SECTION-XREF.
           05  FILLER PIC X(05) VALUE 'TANU1'.
           05  FILLER PIC X(05) VALUE 'ORGU2'.
           05  FILLER PIC X(05) VALUE 'SPCU3'.
           05  FILLER PIC X(05) VALUE 'UNEC3'.
           05  FILLER PIC X(05) VALUE 'RCPS1'.
           05  FILLER PIC X(05) VALUE 'MPBS2'.
           05  FILLER PIC X(05) VALUE 'NRCC2'.
           05  FILLER PIC X(05) VALUE 'ADJA1'.
           05  FILLER PIC X(05) VALUE 'TAXT1'.
           05  FILLER PIC X(05) VALUE 'CCLC1'.
           05  FILLER PIC X(05) VALUE 'EUCC1'.
           05  FILLER PIC X(05) VALUE 'SLCC1'.
       01  WS-ELEM-SECTION-R REDEFINES WS-ELEM-SECTION-XREF.
           05  WS-EX-ENTRY OCCURS 12 TIMES INDEXED BY WS-EX-X.
               10  WS-EX-PREFIX        PIC X(03).
               10  WS-EX-SECTION       PIC X(02).
      *****************************************************************
      * SECTION BUCKETS.  ONE SET PER SECTION, IN TABLE ORDER.        *
      *****************************************************************
       01  WS-SECTION-BUCKETS.
           05  WS-SB-ENTRY OCCURS 12 TIMES.
               10  WS-SB-AMOUNT        PIC S9(15)V9(05) COMP-3.
               10  WS-SB-MINUTES       PIC S9(15)V9(02) COMP-3.
               10  WS-SB-LINES         PIC S9(09) COMP-3.
               10  WS-SB-ELEMS         PIC S9(09) COMP-3.
               10  WS-SB-SUPPRESSED    PIC S9(09) COMP-3.
               10  WS-SB-MANDATORY     PIC X(01).
       01  WS-SECTION-CTL.
           05  WS-SB-SUB               PIC S9(03) COMP-3 VALUE 0.
           05  WS-SB-FOUND-SW          PIC X(01) VALUE 'N'.
               88  WS-SB-FOUND         VALUE 'Y'.
           05  WS-SB-DERIVED           PIC X(02) VALUE SPACES.
      *****************************************************************
      * CONTROL BREAK KEYS.  THE GROUP IS BAN / BILL PERIOD / SECTION /*
      * LINE SEQUENCE.  A NEW LINE SEQUENCE INSIDE THE SAME SECTION IS A*
      * NEW DETAIL LINE, NOT A NEW SECTION.                           *
      *****************************************************************
       01  WS-BREAK-KEY-CURR.
           05  WS-BK-BAN               PIC X(13) VALUE SPACES.
           05  WS-BK-BILL-PERIOD       PIC 9(06) VALUE 0.
           05  WS-BK-SECTION           PIC X(02) VALUE SPACES.
           05  WS-BK-LINE-SEQ          PIC 9(07) VALUE 0.
       01  WS-BREAK-KEY-SAVE.
           05  WS-BS-BAN               PIC X(13) VALUE SPACES.
           05  WS-BS-BILL-PERIOD       PIC 9(06) VALUE 0.
           05  WS-BS-SECTION           PIC X(02) VALUE SPACES.
           05  WS-BS-LINE-SEQ          PIC 9(07) VALUE 0.
       01  WS-BREAK-KEY-ALT REDEFINES WS-BREAK-KEY-SAVE.
           05  WS-BA-ACCOUNT-KEY       PIC X(19).
           05  WS-BA-LINE-KEY          PIC X(09).
      *****************************************************************
      * THE OPEN GROUP.  ELEMENTS ARE ACCUMULATED HERE UNTIL THE BREAK,*
      * THEN MOVED INTO THE VARIABLE LENGTH OUTPUT RECORD.            *
      * THE ODO LIMIT IS AGREED WITH THE BILL PRINT VENDOR.           *
      *****************************************************************
       01  WS-GROUP-WORK.
           05  WS-GW-BAN               PIC X(13) VALUE SPACES.
           05  WS-GW-BILL-PERIOD       PIC 9(06) VALUE 0.
           05  WS-GW-SECTION           PIC X(02) VALUE SPACES.
           05  WS-GW-LINE-SEQ          PIC 9(07) VALUE 0.
           05  WS-GW-OCN               PIC X(04) VALUE SPACES.
           05  WS-GW-JURIS-CD          PIC X(01) VALUE SPACES.
           05  WS-GW-STATE-CD          PIC X(02) VALUE SPACES.
           05  WS-GW-SECTION-CLASS     PIC X(01) VALUE SPACES.
           05  WS-GW-ELEM-CNT          PIC 9(03) VALUE 0.
           05  WS-GW-ACC-MINUTES       PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-GW-ACC-AMOUNT        PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-GW-LAST-RULE         PIC X(01) VALUE SPACES.
           05  WS-GW-CONT-NBR          PIC 9(02) VALUE 0.
           05  WS-GW-OVERFLOW-SW       PIC X(01) VALUE 'N'.
               88  WS-GW-OVERFLOW      VALUE 'Y'.
      *****************************************************************
      * THE ELEMENT STAGING TABLE.  FORTY ENTRIES - THE COPYBOOK LIMIT.*
      * ELEMENT FORTY ONE OPENS A CONTINUATION LINE.                  *
      *****************************************************************
       01  WS-ELEM-STAGE.
           05  WS-ES-ENTRY OCCURS 40 TIMES INDEXED BY WS-ES-X.
               10  WS-ES-RATE-ELEM     PIC X(06).
               10  WS-ES-QTY           PIC S9(13)V9(02) COMP-3.
               10  WS-ES-RATE          PIC S9(05)V9(05) COMP-3.
               10  WS-ES-AMOUNT        PIC S9(11)V9(05) COMP-3.
               10  WS-ES-ROUND-RULE    PIC X(01).
               10  WS-ES-SRC-PROCESS   PIC X(08).
      *****************************************************************
      * THE TRIGGER TABLE.  LOADED AT INIT FROM THE TRIGGER FILE AND  *
      * SEARCHED FOR EVERY RATED RECORD.  AN ACCOUNT THAT IS NOT ON THE*
      * TRIGGER FILE DOES NOT BILL THIS CYCLE AND ITS USAGE IS CARRIED*
      * FORWARD, NOT REJECTED.                                        *
      *****************************************************************
       01  WS-TRIG-TABLE.
           05  WS-TT-ENTRY OCCURS 2000 TIMES INDEXED BY WS-TT-X.
               10  WS-TT-BAN           PIC X(13).
               10  WS-TT-BILL-PERIOD   PIC 9(06).
               10  WS-TT-OCN           PIC X(04).
               10  WS-TT-DUE-YYDDD     PIC 9(05).
               10  WS-TT-TRIGGER-CD    PIC X(02).
       01  WS-TRIG-CTL.
           05  WS-TT-USED              PIC S9(05) COMP-3 VALUE 0.
           05  WS-TT-MAX               PIC S9(05) COMP-3 VALUE 2000.
           05  WS-TT-HIT               PIC S9(05) COMP-3 VALUE 0.
           05  WS-TT-LOW               PIC S9(05) COMP-3 VALUE 0.
           05  WS-TT-HIGH              PIC S9(05) COMP-3 VALUE 0.
           05  WS-TT-MID               PIC S9(05) COMP-3 VALUE 0.
      *****************************************************************
      * THE INPUT TRIGGER RECORD AREA.                                *
      *****************************************************************
       01  WS-TRIGGER-IN.
           05  WS-TI-BAN               PIC X(13).
           05  WS-TI-BILL-PERIOD       PIC 9(06).
           05  WS-TI-OCN               PIC X(04).
           05  WS-TI-CYCLE-YYDDD       PIC 9(05).
           05  WS-TI-DUE-YYDDD         PIC 9(05).
           05  WS-TI-STATE-CD          PIC X(02).
           05  WS-TI-MEDIA-CD          PIC X(01).
           05  WS-TI-TERMS-DAYS        PIC 9(03).
           05  WS-TI-PRIOR-BAL         PIC S9(13)V9(02).
           05  WS-TI-TRIGGER-CD        PIC X(02).
           05  WS-TI-FILLER            PIC X(144).
       01  WS-TRIGGER-IN-K REDEFINES WS-TRIGGER-IN.
           05  WS-TIK-KEY              PIC X(19).
           05  WS-TIK-REST             PIC X(181).
      *****************************************************************
      * DESCRIPTION ASSEMBLY.  SIX FRAGMENTS BUILT IN FIVE SEPARATE   *
      * PARAGRAPHS AND STRUNG TOGETHER IN P5400.                      *
      * PRINT LINE ASSEMBLY PER CABS-STD-063.                         *
      *****************************************************************
       01  WS-DESC-FRAGMENTS.
           05  WS-DF-ELEM-NAME         PIC X(24) VALUE SPACES.
           05  WS-DF-QTY-EDIT          PIC X(14) VALUE SPACES.
           05  WS-DF-RATE-EDIT         PIC X(09) VALUE SPACES.
           05  WS-DF-JURIS-WORD        PIC X(11) VALUE SPACES.
           05  WS-DF-STATE             PIC X(03) VALUE SPACES.
           05  WS-DF-PERIOD-TEXT       PIC X(09) VALUE SPACES.
           05  WS-DF-CONT-TEXT         PIC X(08) VALUE SPACES.
       01  WS-DESC-ASSEMBLED           PIC X(60) VALUE SPACES.
       01  WS-DESC-ASSEMBLED-R REDEFINES WS-DESC-ASSEMBLED.
           05  WS-DA-CHAR OCCURS 60 TIMES PIC X(01).
       01  WS-DESC-SCAN-WORK.
           05  WS-DS-TRUE-LEN          PIC 9(02) VALUE 0.
           05  WS-DS-SCAN-SUB          PIC 9(02) VALUE 0.
           05  WS-DS-BLANK-CNT         PIC 9(02) VALUE 0.
           05  WS-DS-DIGIT-CNT         PIC 9(02) VALUE 0.
           05  WS-DS-PTR               PIC 9(03) VALUE 1.
           05  WS-DS-TRIM-DONE-SW      PIC X(01) VALUE 'N'.
               88  WS-DS-TRIM-DONE     VALUE 'Y'.
      *****************************************************************
      * JURISDICTION WORD TABLE.  THE PRINTED WORD DIFFERS FROM THE CODE*
      * CARRIED ON THE RECORD AND THE TARIFF WORDING IS FIXED BY FILING.*
      *****************************************************************
       01  WS-JURIS-WORD-TABLE.
           05  FILLER PIC X(12) VALUE 'IINTERSTATE '.
           05  FILLER PIC X(12) VALUE 'SINTRASTATE '.
           05  FILLER PIC X(12) VALUE 'LLOCAL      '.
           05  FILLER PIC X(12) VALUE 'TTRANSIT    '.
           05  FILLER PIC X(12) VALUE 'XINDETERMIN '.
           05  FILLER PIC X(12) VALUE 'MMIXED      '.
       01  WS-JURIS-WORD-R REDEFINES WS-JURIS-WORD-TABLE.
           05  WS-JW-ENTRY OCCURS 6 TIMES INDEXED BY WS-JW-X.
               10  WS-JW-CODE          PIC X(01).
               10  WS-JW-WORD          PIC X(11).
      *****************************************************************
      * PERIOD TEXT.  YYDDD TO MON YYYY THROUGH CABDATCV.             *
      *****************************************************************
       01  WS-PERIOD-WORK.
           05  WS-PW-CCYY              PIC 9(04) VALUE 0.
           05  WS-PW-MM                PIC 9(02) VALUE 0.
           05  WS-PW-DD                PIC 9(02) VALUE 0.
           05  WS-PW-TEXT              PIC X(09) VALUE SPACES.
       01  WS-MONTH-NAME-TABLE.
           05  FILLER PIC X(36) VALUE
               'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'.
       01  WS-MONTH-NAME-R REDEFINES WS-MONTH-NAME-TABLE.
           05  WS-MN-NAME OCCURS 12 TIMES PIC X(03).
      *****************************************************************
      * ROUNDING WORK.  THE GROUP TOTAL IS CARRIED AT FIVE DECIMAL    *
      * PLACES AND ROUNDED TO TWO FOR THE PRINTED AND BILLED FIGURE.  *
      * THE DIFFERENCE IS KEPT ON THE RECORD SO THAT THE BALANCING STEP*
      * CAN SEE IT.                                                   *
      *****************************************************************
       01  WS-ROUND-WORK.
           05  WS-RW-RAW               PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-RW-ROUNDED           PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-RW-DELTA             PIC S9(05)V9(05) COMP-3 VALUE 0.
           05  WS-RW-RUN-DELTA         PIC S9(11)V9(05) COMP-3 VALUE 0.
      *****************************************************************
      * ELEMENT VALIDATION WORK.                                      *
      *****************************************************************
       01  WS-ELEM-VALID-WORK.
           05  WS-EV-CHECK-AMT         PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-EV-VARIANCE          PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-EV-TOLERANCE         PIC S9(03)V9(05) COMP-3
                                                       VALUE 0.00500.
           05  WS-EV-NEG-TOLERANCE     PIC S9(03)V9(05) COMP-3
                                                      VALUE -0.00500.
           05  WS-EV-ZERO-RATE         PIC S9(09) COMP-3 VALUE 0.
           05  WS-EV-PREFIX            PIC X(03) VALUE SPACES.
           05  WS-EV-CLASS-SUB         PIC S9(03) COMP-3 VALUE 0.
       01  WS-ELEM-VALID-CNTS.
           05  WS-EV-CLASS-CNT OCCURS 8 TIMES PIC S9(09) COMP-3.
           05  WS-EV-RECOMPUTE OCCURS 8 TIMES PIC S9(09) COMP-3.
      *****************************************************************
      * RUN TOTALS.                                                   *
      *****************************************************************
       01  WS-RUN-TOTALS.
           05  WS-RT-DETAIL-LINES      PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-ELEMENTS          PIC S9(11) COMP-3 VALUE 0.
           05  WS-RT-CONTINUATIONS     PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-SUPPRESSED        PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-ACCOUNTS          PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-AMOUNT            PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-RT-ROUNDED           PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-RT-MINUTES           PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-RT-NOT-TRIGGERED     PIC S9(09) COMP-3 VALUE 0.
       01  WS-AUDIT-KEY                PIC X(26) VALUE SPACES.
       01  WS-AUDIT-KEY-R REDEFINES WS-AUDIT-KEY.
           05  WS-AK-BAN               PIC X(13).
           05  WS-AK-PERIOD            PIC 9(06).
           05  WS-AK-SECTION           PIC X(02).
           05  WS-AK-SEQ               PIC 9(05).
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
           OPEN INPUT  RATED-IN-FILE
                       TRIG-IN-FILE
                       PARM-FILE
           OPEN OUTPUT BILL-DTL-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 4021 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-RATIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 4022 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-TRIGIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 4023 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-BDTLOUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 4024 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-CTLOUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 4025 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-SUSPOUT' TO WS-AB-TEXT
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
           PERFORM P7300-SET-MANDATORY THRU P7300-EXIT.
           PERFORM P7200-LOAD-TRIGGERS THRU P7200-EXIT.
           PERFORM P7150-CLEAR-CLASS THRU P7150-EXIT
               VARYING WS-SUB1 FROM 1 BY 1
               UNTIL WS-SUB1 > 8.
           PERFORM P6000-HEADING THRU P6000-EXIT.
           DISPLAY WS-PGM-NAME ' STARTED - RUN ' WS-RUN-ID.
           DISPLAY '  CYCLE YYDDD  ' WS-CYCLE-YYDDD.
           DISPLAY '  BILL PERIOD  ' WS-BILL-PERIOD.
           DISPLAY '  MAX ELEMENTS ' WS-PE-MAX-ELEM.
           DISPLAY '  SECTION RANGE ' WS-PE-SECTION-FROM ' TO '
                   WS-PE-SECTION-THRU.

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
      * THE ELEMENT LIMIT ARRIVES ON THE CARD.  IT IS NEVER CODED IN
      * THE JCL - THE SCHEDULER SUBSTITUTES IT.  A VALUE ABOVE FORTY
      * IS FATAL BECAUSE THE COPYBOOK CANNOT HOLD IT.
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
           IF WS-PE-MAX-ELEM NOT NUMERIC
               MOVE 4031 TO WS-AB-CODE
               MOVE 'MAX ELEMENT COUNT NOT NUMERIC' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PE-MAX-ELEM < 1 OR WS-PE-MAX-ELEM > 40
               MOVE 4032 TO WS-AB-CODE
               MOVE 'MAX ELEMENT COUNT OUTSIDE 1 TO 40' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PE-SUPPRESS-SW NOT = 'Y'
               MOVE 'N' TO WS-PE-SUPPRESS-SW.
           IF WS-PE-CONT-SW NOT = 'N'
               MOVE 'Y' TO WS-PE-CONT-SW.

       P1200-EXIT.
           EXIT.

      *****************************************************************
      * S200-MAIN PROCESSING                                          *
      * ONE PASS OF THE RATED USAGE FILE.  THE FILE ARRIVES IN BAN,   *
      * BILL PERIOD, SECTION, LINE SEQUENCE, ELEMENT SEQUENCE ORDER - *
      * THAT ORDER IS PRODUCED BY CABSRT10 AND IS NOT VERIFIED HERE.  *
      *****************************************************************
       S200-MAIN-PROCESS SECTION.

       P2000-PROCESS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ-RATED THRU P2100-EXIT.
           IF WS-RAT-EOF
               PERFORM P4900-CLOSE-GROUP THRU P4900-EXIT
               MOVE 'Y' TO WS-EOF-SW
               GO TO P2000-EXIT.
           ADD 1 TO WS-READ-CNT.
           PERFORM P3000-VALIDATE-RATED THRU P3000-EXIT.
           IF WS-ERR-SEVERITY = 'F'
               GO TO P2000-EXIT.
           PERFORM P3100-CHECK-TRIGGER THRU P3100-EXIT.
           IF NOT WS-TRIG-FOUND
               ADD 1 TO WS-CFWD-CNT
               ADD 1 TO WS-RT-NOT-TRIGGERED
               GO TO P2000-EXIT.
           PERFORM P3200-DERIVE-SECTION THRU P3200-EXIT.
           PERFORM P3300-CHECK-BREAK THRU P3300-EXIT.
           IF WS-BREAK
               PERFORM P4900-CLOSE-GROUP THRU P4900-EXIT.
           IF NOT WS-GROUP-OPEN
               PERFORM P4000-OPEN-GROUP THRU P4000-EXIT.
           PERFORM P4400-VALIDATE-ELEMENT THRU P4400-EXIT.
           PERFORM P4100-ADD-ELEMENT THRU P4100-EXIT.
           ADD 1 TO WS-SUMM-CNT.

       P2000-EXIT.
           EXIT.

       P2100-READ-RATED.
      * THE RATED FILE CARRIES ONE RECORD PER RATE ELEMENT.  A SINGLE
      * BILL DETAIL LINE IS BUILT FROM ALL THE ELEMENT RECORDS THAT SHARE
      * A BAN, BILL PERIOD, SECTION AND LINE SEQUENCE.
           MOVE 'P2100-READ-RATED' TO WS-PARA-NAME.
           READ RATED-IN-FILE
               AT END
                   MOVE 'Y' TO WS-RAT-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00' AND WS-FS-INPUT NOT = '10'
               MOVE 4201 TO WS-AB-CODE
               MOVE 'RATED USAGE READ ERROR' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           MOVE RD-BAN                 TO WS-AK-BAN.
           MOVE RD-BILL-PERIOD         TO WS-AK-PERIOD.
           MOVE RD-SECTION             TO WS-AK-SECTION.
           MOVE RD-LINE-SEQ            TO WS-AK-SEQ.
           MOVE WS-AUDIT-KEY           TO WS-RESTART-KEY.
           ADD RD-QTY                  TO WS-ACC-MINUTES.
           ADD RD-AMOUNT               TO WS-ACC-AMOUNT.
           ADD RD-LINE-SEQ             TO WS-ACC-SEQ-HASH.

       P2100-EXIT.
           EXIT.

      *****************************************************************
      * S300-VALIDATION AND KEY DERIVATION                            *
      *****************************************************************
       S300-VALIDATION SECTION.

       P3000-VALIDATE-RATED.
      * FIELD LEVEL EDITS.  A FATAL EDIT LEAVES THE RECORD ON THE
      * SUSPENSE FILE AND THE RECORD TAKES NO FURTHER PART IN THE RUN.
           MOVE 'P3000-VALIDATE-RATED' TO WS-PARA-NAME.
           MOVE SPACES TO WS-ERR-CODE.
           MOVE 'W' TO WS-ERR-SEVERITY.
           IF RD-BAN = SPACES
               MOVE EC-BAN-UNKNOWN TO WS-ERR-CODE
               MOVE 'F' TO WS-ERR-SEVERITY
               GO TO P9990-DETAIL-FAILURE.
           IF RD-BILL-PERIOD NOT NUMERIC
               MOVE EC-DATE-INVALID TO WS-ERR-CODE
               MOVE 'F' TO WS-ERR-SEVERITY
               GO TO P9990-DETAIL-FAILURE.
           IF RD-OCN = SPACES
               MOVE EC-OCN-UNKNOWN TO WS-ERR-CODE
               MOVE 'F' TO WS-ERR-SEVERITY
               GO TO P9990-DETAIL-FAILURE.
           IF RD-JURIS-CD = SPACES
               MOVE EC-JURIS-INDET TO WS-ERR-CODE
               MOVE 'F' TO WS-ERR-SEVERITY
               GO TO P9990-DETAIL-FAILURE.
           IF RD-RATE-ELEM = SPACES
               MOVE EC-RATE-NOT-FOUND TO WS-ERR-CODE
               MOVE 'F' TO WS-ERR-SEVERITY
               GO TO P9990-DETAIL-FAILURE.
           IF RD-CYCLE-YYDDD NOT NUMERIC
               MOVE EC-DATE-INVALID TO WS-ERR-CODE
               MOVE 'W' TO WS-ERR-SEVERITY
               MOVE WS-CYCLE-YYDDD TO RD-CYCLE-YYDDD.

       P3000-EXIT.
           EXIT.

       P3100-CHECK-TRIGGER.
      * BINARY SEARCH OF THE TRIGGER TABLE ON BAN AND BILL PERIOD.  THE
      * TABLE IS LOADED IN THE ORDER THE TRIGGER FILE ARRIVES, WHICH IS
      * BAN ORDER, SO A BINARY SEARCH IS SAFE.
           MOVE 'P3100-CHECK-TRIGGER' TO WS-PARA-NAME.
           MOVE 'N' TO WS-TRIG-FOUND-SW.
           IF WS-TT-USED = ZERO
               GO TO P3100-EXIT.
           MOVE 1 TO WS-TT-LOW.
           MOVE WS-TT-USED TO WS-TT-HIGH.
           PERFORM P3110-BINARY-STEP THRU P3110-EXIT
               UNTIL WS-TT-LOW > WS-TT-HIGH OR WS-TRIG-FOUND.

       P3100-EXIT.
           EXIT.

       P3110-BINARY-STEP.
           COMPUTE WS-TT-MID = (WS-TT-LOW + WS-TT-HIGH) / 2.
           SET WS-TT-X TO WS-TT-MID.
           IF WS-TT-BAN (WS-TT-X) = RD-BAN
               MOVE 'Y' TO WS-TRIG-FOUND-SW
               MOVE WS-TT-MID TO WS-TT-HIT
               GO TO P3110-EXIT.
           IF WS-TT-BAN (WS-TT-X) < RD-BAN
               COMPUTE WS-TT-LOW = WS-TT-MID + 1
           ELSE
               COMPUTE WS-TT-HIGH = WS-TT-MID - 1.

       P3110-EXIT.
           EXIT.

       P3200-DERIVE-SECTION.
      * THE SECTION ON THE RATED RECORD IS ADVISORY.  THE AUTHORITATIVE
      * SECTION IS DERIVED FROM THE RATE ELEMENT PREFIX.  WHERE THE TWO
      * DISAGREE THE DERIVED VALUE WINS AND THE DIFFERENCE IS COUNTED.
           MOVE 'P3200-DERIVE-SECTION' TO WS-PARA-NAME.
           MOVE 'Z1' TO WS-SB-DERIVED.
           MOVE SPACES TO WS-EV-PREFIX.
           MOVE 'N' TO WS-SB-FOUND-SW.
           MOVE RD-RATE-ELEM TO WS-DESC-ASSEMBLED.
           MOVE WS-DA-CHAR (1) TO WS-EV-PREFIX.
           MOVE WS-DA-CHAR (2) TO WS-DF-STATE.
           PERFORM P3210-BUILD-PREFIX THRU P3210-EXIT.
           PERFORM P3220-MATCH-PREFIX THRU P3220-EXIT
               VARYING WS-EX-X FROM 1 BY 1
               UNTIL WS-EX-X > 12 OR WS-SB-FOUND.
           MOVE WS-SB-DERIVED TO WS-BK-SECTION.
           PERFORM P3230-SECTION-SUB THRU P3230-EXIT.

       P3200-EXIT.
           EXIT.

       P3210-BUILD-PREFIX.
      * THE PREFIX IS THE FIRST THREE BYTES OF THE ELEMENT CODE, TAKEN
      * CHARACTER BY CHARACTER THROUGH THE REDEFINED TABLE.  THERE IS NO
      * REFERENCE MODIFICATION IN THIS ESTATE.
      * AGREED WITH THE BILL PRINT VENDOR IN 1997.
           MOVE SPACES TO WS-EV-PREFIX.
           MOVE RD-RATE-ELEM TO WS-DESC-ASSEMBLED.
           MOVE WS-DA-CHAR (1) TO WS-DF-CONT-TEXT.
           STRING WS-DA-CHAR (1)
                  WS-DA-CHAR (2)
                  WS-DA-CHAR (3)
                  DELIMITED BY SIZE
                  INTO WS-EV-PREFIX.
           MOVE SPACES TO WS-DF-CONT-TEXT.

       P3210-EXIT.
           EXIT.

       P3220-MATCH-PREFIX.
           IF WS-EX-PREFIX (WS-EX-X) = WS-EV-PREFIX
               MOVE WS-EX-SECTION (WS-EX-X) TO WS-SB-DERIVED
               MOVE 'Y' TO WS-SB-FOUND-SW.

       P3220-EXIT.
           EXIT.

       P3230-SECTION-SUB.
      * RESOLVE THE SECTION CODE TO ITS SUBSCRIPT IN THE BUCKET TABLE.
           MOVE 12 TO WS-SB-SUB.
           PERFORM P3240-SCAN-SECTION THRU P3240-EXIT
               VARYING WS-ST-X FROM 1 BY 1
               UNTIL WS-ST-X > 12.

       P3230-EXIT.
           EXIT.

       P3240-SCAN-SECTION.
           IF WS-ST-CODE (WS-ST-X) = WS-SB-DERIVED
               SET WS-SUB1 TO WS-ST-X
               MOVE WS-SUB1 TO WS-SB-SUB.

       P3240-EXIT.
           EXIT.

       P3300-CHECK-BREAK.
      * GROUP BREAK.  A CHANGE IN ANY OF THE FOUR KEY PARTS CLOSES THE
      * OPEN DETAIL LINE.  A CHANGE IN THE BAN ALSO CLOSES THE ACCOUNT
      * AND RESETS THE SECTION BUCKETS.
           MOVE 'P3300-CHECK-BREAK' TO WS-PARA-NAME.
           MOVE 'N' TO WS-BREAK-SW.
           MOVE RD-BAN                 TO WS-BK-BAN.
           MOVE RD-BILL-PERIOD         TO WS-BK-BILL-PERIOD.
           MOVE RD-LINE-SEQ            TO WS-BK-LINE-SEQ.
           IF NOT WS-GROUP-OPEN
               GO TO P3300-EXIT.
           IF WS-BK-BAN NOT = WS-BS-BAN
               MOVE 'Y' TO WS-BREAK-SW
               GO TO P3300-EXIT.
           IF WS-BK-BILL-PERIOD NOT = WS-BS-BILL-PERIOD
               MOVE 'Y' TO WS-BREAK-SW
               GO TO P3300-EXIT.
           IF WS-BK-SECTION NOT = WS-BS-SECTION
               MOVE 'Y' TO WS-BREAK-SW
               GO TO P3300-EXIT.
           IF WS-BK-LINE-SEQ NOT = WS-BS-LINE-SEQ
               MOVE 'Y' TO WS-BREAK-SW.

       P3300-EXIT.
           EXIT.

      *****************************************************************
      * S400-GROUP ASSEMBLY                                           *
      * OPEN A GROUP, ADD ELEMENTS TO IT, ROUND IT AND WRITE IT AS A  *
      * VARIABLE LENGTH RECORD WHOSE LENGTH DEPENDS ON BD-ELEM-CNT.   *
      *****************************************************************
       S400-ASSEMBLY SECTION.

       P4000-OPEN-GROUP.
           MOVE 'P4000-OPEN-GROUP' TO WS-PARA-NAME.
           MOVE SPACES TO WS-GROUP-WORK.
           MOVE RD-BAN                 TO WS-GW-BAN.
           MOVE RD-BILL-PERIOD         TO WS-GW-BILL-PERIOD.
           MOVE WS-BK-SECTION          TO WS-GW-SECTION.
           MOVE RD-LINE-SEQ            TO WS-GW-LINE-SEQ.
           MOVE RD-OCN                 TO WS-GW-OCN.
           MOVE RD-JURIS-CD            TO WS-GW-JURIS-CD.
           MOVE RD-STATE-CD            TO WS-GW-STATE-CD.
           MOVE ZERO TO WS-GW-ELEM-CNT WS-GW-ACC-MINUTES
                        WS-GW-ACC-AMOUNT WS-GW-CONT-NBR.
           MOVE 'N' TO WS-GW-OVERFLOW-SW.
           MOVE 'N' TO WS-SUPPRESS-SW.
           MOVE 'Y' TO WS-GROUP-OPEN-SW.
           MOVE WS-BK-BAN              TO WS-BS-BAN.
           MOVE WS-BK-BILL-PERIOD      TO WS-BS-BILL-PERIOD.
           MOVE WS-BK-SECTION          TO WS-BS-SECTION.
           MOVE WS-BK-LINE-SEQ         TO WS-BS-LINE-SEQ.

       P4000-EXIT.
           EXIT.

       P4100-ADD-ELEMENT.
      * STAGE ONE RATE ELEMENT.  THE COPYBOOK ALLOWS FORTY.  ELEMENT
      * FORTY ONE CLOSES THE LINE AND OPENS A CONTINUATION LINE CARRYING
      * THE SAME KEY WITH THE CONTINUATION NUMBER IN THE DESCRIPTION.
           MOVE 'P4100-ADD-ELEMENT' TO WS-PARA-NAME.
           IF WS-GW-ELEM-CNT NOT < WS-PE-MAX-ELEM
               PERFORM P4200-OVERFLOW THRU P4200-EXIT.
           ADD 1 TO WS-GW-ELEM-CNT.
           MOVE WS-GW-ELEM-CNT TO WS-SUB2.
           SET WS-ES-X TO WS-SUB2.
           MOVE RD-RATE-ELEM   TO WS-ES-RATE-ELEM (WS-ES-X).
           MOVE RD-QTY         TO WS-ES-QTY (WS-ES-X).
           MOVE RD-RATE        TO WS-ES-RATE (WS-ES-X).
           MOVE RD-AMOUNT      TO WS-ES-AMOUNT (WS-ES-X).
           MOVE RD-ROUND-RULE  TO WS-ES-ROUND-RULE (WS-ES-X).
           MOVE RD-SRC-PROCESS TO WS-ES-SRC-PROCESS (WS-ES-X).
           ADD RD-QTY          TO WS-GW-ACC-MINUTES.
           ADD RD-AMOUNT       TO WS-GW-ACC-AMOUNT.
           MOVE RD-ROUND-RULE  TO WS-GW-LAST-RULE.
           ADD 1 TO WS-RT-ELEMENTS.

       P4100-EXIT.
           EXIT.

       P4200-OVERFLOW.
      * THE FORTY ELEMENT LIMIT IS REACHED.  CLOSE THE LINE, WRITE IT AND
      * REOPEN A CONTINUATION LINE UNDER THE SAME KEY.  THE CONTINUATION
      * NUMBER IS CARRIED IN THE DESCRIPTION, NOT IN A FIELD OF ITS OWN,
      * BECAUSE THE COPYBOOK HAS NO ROOM FOR ONE.
           MOVE 'P4200-OVERFLOW' TO WS-PARA-NAME.
           MOVE 'Y' TO WS-GW-OVERFLOW-SW.
           MOVE 'Y' TO WS-CONT-SW.
           PERFORM P4500-ROUND-GROUP THRU P4500-EXIT.
           PERFORM P4600-WRITE-DETAIL THRU P4600-EXIT.
           ADD 1 TO WS-GW-CONT-NBR.
           ADD 1 TO WS-RT-CONTINUATIONS.
           MOVE ZERO TO WS-GW-ELEM-CNT WS-GW-ACC-MINUTES
                        WS-GW-ACC-AMOUNT.

       P4200-EXIT.
           EXIT.

       P4300-ACCUM-SECTION.
      * DISPATCH TO THE SECTION HANDLER.  GO TO DEPENDING ON IS USED
      * RATHER THAN A NEST OF IFS - THE SECTION SUBSCRIPT IS ALREADY
      * RESOLVED AND THE TWELVE HANDLERS ARE THE SAME SHAPE.
           MOVE 'P4300-ACCUM-SECTION' TO WS-PARA-NAME.
           IF WS-SB-SUB < 1 OR WS-SB-SUB > 12
               MOVE 12 TO WS-SB-SUB.
           GO TO P5101-SECTION-U1
                 P5102-SECTION-U2
                 P5103-SECTION-U3
                 P5104-SECTION-C1
                 P5105-SECTION-C2
                 P5106-SECTION-C3
                 P5107-SECTION-C4
                 P5108-SECTION-S1
                 P5109-SECTION-S2
                 P5110-SECTION-A1
                 P5111-SECTION-T1
                 P5112-SECTION-Z1
               DEPENDING ON WS-SB-SUB.

       P4300-EXIT.
           EXIT.

       P4400-VALIDATE-ELEMENT.
      * ELEMENT LEVEL VALIDATION.  THE PREFIX SELECTS THE CLASS ROUTINE.
      * EIGHT CLASSES, EACH WITH ITS OWN RULE ABOUT WHAT A QUANTITY
      * MEANS.  ANYTHING THAT DOES NOT MATCH A CLASS IS PASSED THROUGH.
           MOVE 'P4400-VALIDATE-ELEMENT' TO WS-PARA-NAME.
           MOVE ZERO TO WS-EV-CLASS-SUB.
           IF WS-EV-PREFIX = 'TAN'
               MOVE 1 TO WS-EV-CLASS-SUB.
           IF WS-EV-PREFIX = 'ORG'
               MOVE 2 TO WS-EV-CLASS-SUB.
           IF WS-EV-PREFIX = 'SPC'
               MOVE 3 TO WS-EV-CLASS-SUB.
           IF WS-EV-PREFIX = 'UNE'
               MOVE 4 TO WS-EV-CLASS-SUB.
           IF WS-EV-PREFIX = 'RCP'
               MOVE 5 TO WS-EV-CLASS-SUB.
           IF WS-EV-PREFIX = 'MPB'
               MOVE 6 TO WS-EV-CLASS-SUB.
           IF WS-EV-PREFIX = 'NRC'
               MOVE 7 TO WS-EV-CLASS-SUB.
           IF WS-EV-PREFIX = 'ADJ'
               MOVE 8 TO WS-EV-CLASS-SUB.
           IF WS-EV-CLASS-SUB = ZERO
               GO TO P4400-EXIT.
           GO TO P7101-VALIDATE-TAN
                 P7102-VALIDATE-ORG
                 P7103-VALIDATE-SPC
                 P7104-VALIDATE-UNE
                 P7105-VALIDATE-RCP
                 P7106-VALIDATE-MPB
                 P7107-VALIDATE-NRC
                 P7108-VALIDATE-ADJ
               DEPENDING ON WS-EV-CLASS-SUB.

       P4400-EXIT.
           EXIT.

       P4500-ROUND-GROUP.
      * THE GROUP AMOUNT IS ACCUMULATED AT FIVE DECIMAL PLACES BECAUSE
      * ACCESS RATES CARRY FIVE.  THE BILLED FIGURE IS TWO PLACES.  THE
      * ROUNDED FIGURE IS COMPUTED WITH ROUNDED AND THE DIFFERENCE IS
      * KEPT SO THAT THE PENNY CAN BE ACCOUNTED FOR.
      * MONEY FIELDS FOLLOW CABS-STD-041 (TARIFF ROUNDING).
           MOVE 'P4500-ROUND-GROUP' TO WS-PARA-NAME.
           MOVE WS-GW-ACC-AMOUNT TO WS-RW-RAW.
           COMPUTE WS-RW-ROUNDED ROUNDED = WS-RW-RAW.
           COMPUTE WS-RW-DELTA = WS-RW-RAW - WS-RW-ROUNDED.
           ADD WS-RW-DELTA TO WS-RW-RUN-DELTA.
           ADD WS-RW-RAW TO WS-RT-AMOUNT.
           ADD WS-RW-ROUNDED TO WS-RT-ROUNDED.
           ADD WS-GW-ACC-MINUTES TO WS-RT-MINUTES.

       P4500-EXIT.
           EXIT.

       P4600-WRITE-DETAIL.
      * BUILD AND WRITE THE VARIABLE LENGTH BILL DETAIL RECORD.  THE
      * LENGTH OF THE RECORD IS SET BY BD-ELEM-CNT AND THE FILE IS
      * DECLARED RECORD VARYING DEPENDING ON THAT FIELD.
      * THE ODO LIMIT IS AGREED WITH THE BILL PRINT VENDOR.
           MOVE 'P4600-WRITE-DETAIL' TO WS-PARA-NAME.
           IF WS-SUPPRESSING
               ADD 1 TO WS-RT-SUPPRESSED
               MOVE 'N' TO WS-SUPPRESS-SW
               GO TO P4600-EXIT.
           MOVE SPACES TO CABS-BILL-DETAIL.
           MOVE WS-GW-BAN              TO BD-BAN.
           MOVE WS-GW-BILL-PERIOD      TO BD-BILL-PERIOD.
           MOVE WS-GW-SECTION          TO BD-SECTION.
           MOVE WS-GW-LINE-SEQ         TO BD-LINE-SEQ.
           MOVE WS-GW-OCN              TO BD-OCN.
           MOVE WS-GW-JURIS-CD         TO BD-JURIS-CD.
           MOVE WS-GW-STATE-CD         TO BD-STATE-CD.
           MOVE WS-GW-ACC-MINUTES      TO BD-TOT-MINUTES.
           MOVE WS-RW-RAW              TO BD-TOT-AMOUNT.
           MOVE WS-RW-ROUNDED          TO BD-TOT-ROUNDED.
           MOVE WS-RW-DELTA            TO BD-ROUND-DELTA.
           MOVE WS-GW-ELEM-CNT         TO BD-ELEM-CNT.
           IF BD-ELEM-CNT < 1
               MOVE 1 TO BD-ELEM-CNT.
           PERFORM P4700-MOVE-ELEMENTS THRU P4700-EXIT
               VARYING WS-SUB3 FROM 1 BY 1
               UNTIL WS-SUB3 > WS-GW-ELEM-CNT.
           PERFORM P5400-BUILD-DESCRIPTION THRU P5400-EXIT.
           MOVE WS-DESC-ASSEMBLED      TO BD-DESCRIPTION.
           WRITE CABS-BILL-DETAIL.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 4202 TO WS-AB-CODE
               MOVE 'BILL DETAIL WRITE FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-WRITE-CNT.
           ADD 1 TO WS-RT-DETAIL-LINES.
           PERFORM P4300-ACCUM-SECTION THRU P4300-EXIT.

       P4600-EXIT.
           EXIT.

       P4700-MOVE-ELEMENTS.
      * MOVE THE STAGED ELEMENTS INTO THE OCCURS DEPENDING ON AREA.  THE
      * SUBSCRIPT MUST NOT EXCEED BD-ELEM-CNT OR THE MOVE LANDS OUTSIDE
      * THE RECORD THAT WILL ACTUALLY BE WRITTEN.
           SET WS-ES-X TO WS-SUB3.
           SET BD-EX TO WS-SUB3.
           MOVE WS-ES-RATE-ELEM (WS-ES-X)
                                       TO BD-EL-RATE-ELEM (BD-EX).
           MOVE WS-ES-QTY (WS-ES-X)    TO BD-EL-QTY (BD-EX).
           MOVE WS-ES-RATE (WS-ES-X)   TO BD-EL-RATE (BD-EX).
           MOVE WS-ES-AMOUNT (WS-ES-X) TO BD-EL-AMOUNT (BD-EX).
           MOVE WS-ES-ROUND-RULE (WS-ES-X)
                                       TO BD-EL-ROUND-RULE (BD-EX).
           MOVE WS-ES-SRC-PROCESS (WS-ES-X)
                                       TO BD-EL-SRC-PROCESS (BD-EX).

       P4700-EXIT.
           EXIT.

       P4900-CLOSE-GROUP.
      * CLOSE THE OPEN DETAIL LINE.  CALLED ON EVERY BREAK AND ONCE MORE
      * AT END OF FILE.  A CLOSE WITH NO OPEN GROUP DOES NOTHING.
           MOVE 'P4900-CLOSE-GROUP' TO WS-PARA-NAME.
           IF NOT WS-GROUP-OPEN
               GO TO P4900-EXIT.
           PERFORM P4500-ROUND-GROUP THRU P4500-EXIT.
           PERFORM P4600-WRITE-DETAIL THRU P4600-EXIT.
           IF WS-BK-BAN NOT = WS-BS-BAN
               ADD 1 TO WS-RT-ACCOUNTS.
           MOVE 'N' TO WS-GROUP-OPEN-SW.
           MOVE 'N' TO WS-CONT-SW.

       P4900-EXIT.
           EXIT.

      *****************************************************************
      * S500-SECTION HANDLERS AND DESCRIPTION ASSEMBLY                *
      * TWELVE SECTION HANDLERS, ONE PER BILL SECTION, REACHED BY     *
      * GO TO DEPENDING ON FROM P4300.                                *
      *****************************************************************
       S500-SECTIONS SECTION.

       P5101-SECTION-U1.
      * SECTION U1 - SWITCHED ACCESS USAGE
      * ACCUMULATE THE GROUP INTO THE SECTION BUCKET AND APPLY THE
      * SECTION LEVEL SUPPRESSION RULE.  A ZERO VALUE LINE IS STILL
      * CARRIED FORWARD WHEN THE SECTION IS ONE THE TARIFF REQUIRES TO
      * BE SHOWN EVEN WHEN NOTHING WAS BILLED.
           MOVE 'P5101-SECTION-U1' TO WS-PARA-NAME.
           MOVE 1  TO WS-SB-SUB.
           ADD WS-GW-ACC-AMOUNT   TO WS-SB-AMOUNT (WS-SB-SUB).
           ADD WS-GW-ACC-MINUTES  TO WS-SB-MINUTES (WS-SB-SUB).
           ADD 1                  TO WS-SB-LINES (WS-SB-SUB).
           ADD WS-GW-ELEM-CNT     TO WS-SB-ELEMS (WS-SB-SUB).
           IF WS-GW-ACC-AMOUNT = ZERO
               IF WS-SB-MANDATORY (WS-SB-SUB) NOT = 'Y'
                   MOVE 'Y' TO WS-SUPPRESS-SW
                   ADD 1 TO WS-SB-SUPPRESSED (WS-SB-SUB).
           MOVE 'U' TO WS-GW-SECTION-CLASS.

       P5101-EXIT.
           EXIT.

       P5102-SECTION-U2.
      * SECTION U2 - ORIGINATING ACCESS USAGE
      * ACCUMULATE THE GROUP INTO THE SECTION BUCKET AND APPLY THE
      * SECTION LEVEL SUPPRESSION RULE.  A ZERO VALUE LINE IS STILL
      * CARRIED FORWARD WHEN THE SECTION IS ONE THE TARIFF REQUIRES TO
      * BE SHOWN EVEN WHEN NOTHING WAS BILLED.
           MOVE 'P5102-SECTION-U2' TO WS-PARA-NAME.
           MOVE 2  TO WS-SB-SUB.
           ADD WS-GW-ACC-AMOUNT   TO WS-SB-AMOUNT (WS-SB-SUB).
           ADD WS-GW-ACC-MINUTES  TO WS-SB-MINUTES (WS-SB-SUB).
           ADD 1                  TO WS-SB-LINES (WS-SB-SUB).
           ADD WS-GW-ELEM-CNT     TO WS-SB-ELEMS (WS-SB-SUB).
           IF WS-GW-ACC-AMOUNT = ZERO
               IF WS-SB-MANDATORY (WS-SB-SUB) NOT = 'Y'
                   MOVE 'Y' TO WS-SUPPRESS-SW
                   ADD 1 TO WS-SB-SUPPRESSED (WS-SB-SUB).
           MOVE 'U' TO WS-GW-SECTION-CLASS.

       P5102-EXIT.
           EXIT.

       P5103-SECTION-U3.
      * SECTION U3 - SPECIAL ACCESS USAGE
      * ACCUMULATE THE GROUP INTO THE SECTION BUCKET AND APPLY THE
      * SECTION LEVEL SUPPRESSION RULE.  A ZERO VALUE LINE IS STILL
      * CARRIED FORWARD WHEN THE SECTION IS ONE THE TARIFF REQUIRES TO
      * BE SHOWN EVEN WHEN NOTHING WAS BILLED.
           MOVE 'P5103-SECTION-U3' TO WS-PARA-NAME.
           MOVE 3  TO WS-SB-SUB.
           ADD WS-GW-ACC-AMOUNT   TO WS-SB-AMOUNT (WS-SB-SUB).
           ADD WS-GW-ACC-MINUTES  TO WS-SB-MINUTES (WS-SB-SUB).
           ADD 1                  TO WS-SB-LINES (WS-SB-SUB).
           ADD WS-GW-ELEM-CNT     TO WS-SB-ELEMS (WS-SB-SUB).
           IF WS-GW-ACC-AMOUNT = ZERO
               IF WS-SB-MANDATORY (WS-SB-SUB) NOT = 'Y'
                   MOVE 'Y' TO WS-SUPPRESS-SW
                   ADD 1 TO WS-SB-SUPPRESSED (WS-SB-SUB).
           MOVE 'U' TO WS-GW-SECTION-CLASS.

       P5103-EXIT.
           EXIT.

       P5104-SECTION-C1.
      * SECTION C1 - RECURRING ACCESS CHARGES
      * ACCUMULATE THE GROUP INTO THE SECTION BUCKET AND APPLY THE
      * SECTION LEVEL SUPPRESSION RULE.  A ZERO VALUE LINE IS STILL
      * CARRIED FORWARD WHEN THE SECTION IS ONE THE TARIFF REQUIRES TO
      * BE SHOWN EVEN WHEN NOTHING WAS BILLED.
           MOVE 'P5104-SECTION-C1' TO WS-PARA-NAME.
           MOVE 4  TO WS-SB-SUB.
           ADD WS-GW-ACC-AMOUNT   TO WS-SB-AMOUNT (WS-SB-SUB).
           ADD WS-GW-ACC-MINUTES  TO WS-SB-MINUTES (WS-SB-SUB).
           ADD 1                  TO WS-SB-LINES (WS-SB-SUB).
           ADD WS-GW-ELEM-CNT     TO WS-SB-ELEMS (WS-SB-SUB).
           IF WS-GW-ACC-AMOUNT = ZERO
               IF WS-SB-MANDATORY (WS-SB-SUB) NOT = 'Y'
                   MOVE 'Y' TO WS-SUPPRESS-SW
                   ADD 1 TO WS-SB-SUPPRESSED (WS-SB-SUB).
           MOVE 'C' TO WS-GW-SECTION-CLASS.

       P5104-EXIT.
           EXIT.

       P5105-SECTION-C2.
      * SECTION C2 - NON RECURRING CHARGES
      * ACCUMULATE THE GROUP INTO THE SECTION BUCKET AND APPLY THE
      * SECTION LEVEL SUPPRESSION RULE.  A ZERO VALUE LINE IS STILL
      * CARRIED FORWARD WHEN THE SECTION IS ONE THE TARIFF REQUIRES TO
      * BE SHOWN EVEN WHEN NOTHING WAS BILLED.
           MOVE 'P5105-SECTION-C2' TO WS-PARA-NAME.
           MOVE 5  TO WS-SB-SUB.
           ADD WS-GW-ACC-AMOUNT   TO WS-SB-AMOUNT (WS-SB-SUB).
           ADD WS-GW-ACC-MINUTES  TO WS-SB-MINUTES (WS-SB-SUB).
           ADD 1                  TO WS-SB-LINES (WS-SB-SUB).
           ADD WS-GW-ELEM-CNT     TO WS-SB-ELEMS (WS-SB-SUB).
           IF WS-GW-ACC-AMOUNT = ZERO
               IF WS-SB-MANDATORY (WS-SB-SUB) NOT = 'Y'
                   MOVE 'Y' TO WS-SUPPRESS-SW
                   ADD 1 TO WS-SB-SUPPRESSED (WS-SB-SUB).
           MOVE 'C' TO WS-GW-SECTION-CLASS.

       P5105-EXIT.
           EXIT.

       P5106-SECTION-C3.
      * SECTION C3 - UNBUNDLED NETWORK ELEMENTS
      * ACCUMULATE THE GROUP INTO THE SECTION BUCKET AND APPLY THE
      * SECTION LEVEL SUPPRESSION RULE.  A ZERO VALUE LINE IS STILL
      * CARRIED FORWARD WHEN THE SECTION IS ONE THE TARIFF REQUIRES TO
      * BE SHOWN EVEN WHEN NOTHING WAS BILLED.
           MOVE 'P5106-SECTION-C3' TO WS-PARA-NAME.
           MOVE 6  TO WS-SB-SUB.
           ADD WS-GW-ACC-AMOUNT   TO WS-SB-AMOUNT (WS-SB-SUB).
           ADD WS-GW-ACC-MINUTES  TO WS-SB-MINUTES (WS-SB-SUB).
           ADD 1                  TO WS-SB-LINES (WS-SB-SUB).
           ADD WS-GW-ELEM-CNT     TO WS-SB-ELEMS (WS-SB-SUB).
           IF WS-GW-ACC-AMOUNT = ZERO
               IF WS-SB-MANDATORY (WS-SB-SUB) NOT = 'Y'
                   MOVE 'Y' TO WS-SUPPRESS-SW
                   ADD 1 TO WS-SB-SUPPRESSED (WS-SB-SUB).
           MOVE 'C' TO WS-GW-SECTION-CLASS.

       P5106-EXIT.
           EXIT.

       P5107-SECTION-C4.
      * SECTION C4 - INTERCONNECTION CHARGES
      * ACCUMULATE THE GROUP INTO THE SECTION BUCKET AND APPLY THE
      * SECTION LEVEL SUPPRESSION RULE.  A ZERO VALUE LINE IS STILL
      * CARRIED FORWARD WHEN THE SECTION IS ONE THE TARIFF REQUIRES TO
      * BE SHOWN EVEN WHEN NOTHING WAS BILLED.
           MOVE 'P5107-SECTION-C4' TO WS-PARA-NAME.
           MOVE 7  TO WS-SB-SUB.
           ADD WS-GW-ACC-AMOUNT   TO WS-SB-AMOUNT (WS-SB-SUB).
           ADD WS-GW-ACC-MINUTES  TO WS-SB-MINUTES (WS-SB-SUB).
           ADD 1                  TO WS-SB-LINES (WS-SB-SUB).
           ADD WS-GW-ELEM-CNT     TO WS-SB-ELEMS (WS-SB-SUB).
           IF WS-GW-ACC-AMOUNT = ZERO
               IF WS-SB-MANDATORY (WS-SB-SUB) NOT = 'Y'
                   MOVE 'Y' TO WS-SUPPRESS-SW
                   ADD 1 TO WS-SB-SUPPRESSED (WS-SB-SUB).
           MOVE 'C' TO WS-GW-SECTION-CLASS.

       P5107-EXIT.
           EXIT.

       P5108-SECTION-S1.
      * SECTION S1 - RECIPROCAL COMPENSATION
      * ACCUMULATE THE GROUP INTO THE SECTION BUCKET AND APPLY THE
      * SECTION LEVEL SUPPRESSION RULE.  A ZERO VALUE LINE IS STILL
      * CARRIED FORWARD WHEN THE SECTION IS ONE THE TARIFF REQUIRES TO
      * BE SHOWN EVEN WHEN NOTHING WAS BILLED.
           MOVE 'P5108-SECTION-S1' TO WS-PARA-NAME.
           MOVE 8  TO WS-SB-SUB.
           ADD WS-GW-ACC-AMOUNT   TO WS-SB-AMOUNT (WS-SB-SUB).
           ADD WS-GW-ACC-MINUTES  TO WS-SB-MINUTES (WS-SB-SUB).
           ADD 1                  TO WS-SB-LINES (WS-SB-SUB).
           ADD WS-GW-ELEM-CNT     TO WS-SB-ELEMS (WS-SB-SUB).
           IF WS-GW-ACC-AMOUNT = ZERO
               IF WS-SB-MANDATORY (WS-SB-SUB) NOT = 'Y'
                   MOVE 'Y' TO WS-SUPPRESS-SW
                   ADD 1 TO WS-SB-SUPPRESSED (WS-SB-SUB).
           MOVE 'S' TO WS-GW-SECTION-CLASS.

       P5108-EXIT.
           EXIT.

       P5109-SECTION-S2.
      * SECTION S2 - MEET POINT BILLING
      * ACCUMULATE THE GROUP INTO THE SECTION BUCKET AND APPLY THE
      * SECTION LEVEL SUPPRESSION RULE.  A ZERO VALUE LINE IS STILL
      * CARRIED FORWARD WHEN THE SECTION IS ONE THE TARIFF REQUIRES TO
      * BE SHOWN EVEN WHEN NOTHING WAS BILLED.
           MOVE 'P5109-SECTION-S2' TO WS-PARA-NAME.
           MOVE 9  TO WS-SB-SUB.
           ADD WS-GW-ACC-AMOUNT   TO WS-SB-AMOUNT (WS-SB-SUB).
           ADD WS-GW-ACC-MINUTES  TO WS-SB-MINUTES (WS-SB-SUB).
           ADD 1                  TO WS-SB-LINES (WS-SB-SUB).
           ADD WS-GW-ELEM-CNT     TO WS-SB-ELEMS (WS-SB-SUB).
           IF WS-GW-ACC-AMOUNT = ZERO
               IF WS-SB-MANDATORY (WS-SB-SUB) NOT = 'Y'
                   MOVE 'Y' TO WS-SUPPRESS-SW
                   ADD 1 TO WS-SB-SUPPRESSED (WS-SB-SUB).
           MOVE 'S' TO WS-GW-SECTION-CLASS.

       P5109-EXIT.
           EXIT.

       P5110-SECTION-A1.
      * SECTION A1 - ADJUSTMENTS AND RESTATEMENT
      * ACCUMULATE THE GROUP INTO THE SECTION BUCKET AND APPLY THE
      * SECTION LEVEL SUPPRESSION RULE.  A ZERO VALUE LINE IS STILL
      * CARRIED FORWARD WHEN THE SECTION IS ONE THE TARIFF REQUIRES TO
      * BE SHOWN EVEN WHEN NOTHING WAS BILLED.
           MOVE 'P5110-SECTION-A1' TO WS-PARA-NAME.
           MOVE 10 TO WS-SB-SUB.
           ADD WS-GW-ACC-AMOUNT   TO WS-SB-AMOUNT (WS-SB-SUB).
           ADD WS-GW-ACC-MINUTES  TO WS-SB-MINUTES (WS-SB-SUB).
           ADD 1                  TO WS-SB-LINES (WS-SB-SUB).
           ADD WS-GW-ELEM-CNT     TO WS-SB-ELEMS (WS-SB-SUB).
           IF WS-GW-ACC-AMOUNT = ZERO
               IF WS-SB-MANDATORY (WS-SB-SUB) NOT = 'Y'
                   MOVE 'Y' TO WS-SUPPRESS-SW
                   ADD 1 TO WS-SB-SUPPRESSED (WS-SB-SUB).
           MOVE 'A' TO WS-GW-SECTION-CLASS.

       P5110-EXIT.
           EXIT.

       P5111-SECTION-T1.
      * SECTION T1 - TAXES AND SURCHARGES
      * ACCUMULATE THE GROUP INTO THE SECTION BUCKET AND APPLY THE
      * SECTION LEVEL SUPPRESSION RULE.  A ZERO VALUE LINE IS STILL
      * CARRIED FORWARD WHEN THE SECTION IS ONE THE TARIFF REQUIRES TO
      * BE SHOWN EVEN WHEN NOTHING WAS BILLED.
           MOVE 'P5111-SECTION-T1' TO WS-PARA-NAME.
           MOVE 11 TO WS-SB-SUB.
           ADD WS-GW-ACC-AMOUNT   TO WS-SB-AMOUNT (WS-SB-SUB).
           ADD WS-GW-ACC-MINUTES  TO WS-SB-MINUTES (WS-SB-SUB).
           ADD 1                  TO WS-SB-LINES (WS-SB-SUB).
           ADD WS-GW-ELEM-CNT     TO WS-SB-ELEMS (WS-SB-SUB).
           IF WS-GW-ACC-AMOUNT = ZERO
               IF WS-SB-MANDATORY (WS-SB-SUB) NOT = 'Y'
                   MOVE 'Y' TO WS-SUPPRESS-SW
                   ADD 1 TO WS-SB-SUPPRESSED (WS-SB-SUB).
           MOVE 'T' TO WS-GW-SECTION-CLASS.

       P5111-EXIT.
           EXIT.

       P5112-SECTION-Z1.
      * SECTION Z1 - UNCLASSIFIED
      * ACCUMULATE THE GROUP INTO THE SECTION BUCKET AND APPLY THE
      * SECTION LEVEL SUPPRESSION RULE.  A ZERO VALUE LINE IS STILL
      * CARRIED FORWARD WHEN THE SECTION IS ONE THE TARIFF REQUIRES TO
      * BE SHOWN EVEN WHEN NOTHING WAS BILLED.
           MOVE 'P5112-SECTION-Z1' TO WS-PARA-NAME.
           MOVE 12 TO WS-SB-SUB.
           ADD WS-GW-ACC-AMOUNT   TO WS-SB-AMOUNT (WS-SB-SUB).
           ADD WS-GW-ACC-MINUTES  TO WS-SB-MINUTES (WS-SB-SUB).
           ADD 1                  TO WS-SB-LINES (WS-SB-SUB).
           ADD WS-GW-ELEM-CNT     TO WS-SB-ELEMS (WS-SB-SUB).
           IF WS-GW-ACC-AMOUNT = ZERO
               IF WS-SB-MANDATORY (WS-SB-SUB) NOT = 'Y'
                   MOVE 'Y' TO WS-SUPPRESS-SW
                   ADD 1 TO WS-SB-SUPPRESSED (WS-SB-SUB).
           MOVE 'Z' TO WS-GW-SECTION-CLASS.

       P5112-EXIT.
           EXIT.

       P5400-BUILD-DESCRIPTION.
      * ASSEMBLE THE SIXTY BYTE DESCRIPTION FROM SIX FRAGMENTS.  THE
      * FRAGMENTS ARE BUILT IN FIVE SEPARATE PARAGRAPHS AND STRUNG
      * TOGETHER HERE.  THE ORDER IS FIXED BY THE TARIFF FILING.
      * LINE LAYOUT IS HELD WITH THE BILL FORMAT SPECIFICATION.
           MOVE 'P5400-BUILD-DESCRIPTION' TO WS-PARA-NAME.
           PERFORM P5410-FRAGMENT-NAME THRU P5410-EXIT.
           PERFORM P5420-FRAGMENT-QTY THRU P5420-EXIT.
           PERFORM P5430-FRAGMENT-RATE THRU P5430-EXIT.
           PERFORM P5440-FRAGMENT-JURIS THRU P5440-EXIT.
           PERFORM P5450-FRAGMENT-PERIOD THRU P5450-EXIT.
           MOVE SPACES TO WS-DESC-ASSEMBLED.
           MOVE 1 TO WS-DS-PTR.
           STRING WS-DF-ELEM-NAME  DELIMITED BY '  '
                  ' '              DELIMITED BY SIZE
                  WS-DF-JURIS-WORD DELIMITED BY '  '
                  ' '              DELIMITED BY SIZE
                  WS-DF-STATE      DELIMITED BY SIZE
                  ' '              DELIMITED BY SIZE
                  WS-DF-PERIOD-TEXT DELIMITED BY SIZE
                  ' '              DELIMITED BY SIZE
                  WS-DF-QTY-EDIT   DELIMITED BY SIZE
                  INTO WS-DESC-ASSEMBLED
                  WITH POINTER WS-DS-PTR
               ON OVERFLOW
                  ADD 1 TO WS-EV-ZERO-RATE.
           IF WS-CONTINUING
               PERFORM P5460-APPEND-CONT THRU P5460-EXIT.
           PERFORM P5500-TRIM-DESCRIPTION THRU P5500-EXIT.
           PERFORM P5600-EDIT-DESCRIPTION THRU P5600-EXIT.

       P5400-EXIT.
           EXIT.

       P5410-FRAGMENT-NAME.
      * THE PRINTED NAME OF THE RATE ELEMENT.  IF THE ELEMENT IS NOT IN
      * THE TABLE THE CODE ITSELF IS PRINTED - THE BILL STILL GOES OUT.
           MOVE 'P5410-FRAGMENT-NAME' TO WS-PARA-NAME.
           MOVE 'N' TO WS-ELEM-FOUND-SW.
           MOVE SPACES TO WS-DF-ELEM-NAME.
           SET WS-ES-X TO 1.
           PERFORM P5700-LOOKUP-ELEM THRU P5700-EXIT
               VARYING WS-EN-X FROM 1 BY 1
               UNTIL WS-EN-X > 60 OR WS-ELEM-FOUND.
           IF NOT WS-ELEM-FOUND
               MOVE WS-ES-RATE-ELEM (1) TO WS-DF-ELEM-NAME.

       P5410-EXIT.
           EXIT.

       P5420-FRAGMENT-QTY.
      * THE QUANTITY FRAGMENT.  MINUTES ARE PRINTED TO TWO PLACES,
      * COUNTS TO NONE.  THE DISTINCTION IS TAKEN FROM THE SECTION CLASS
      * AND NOT FROM THE ELEMENT.
           MOVE 'P5420-FRAGMENT-QTY' TO WS-PARA-NAME.
           MOVE SPACES TO WS-DF-QTY-EDIT.
           MOVE WS-GW-ACC-MINUTES TO WS-ED-MONEY.
           MOVE WS-ED-MONEY TO WS-DF-QTY-EDIT.
           IF WS-GW-SECTION-CLASS = 'C'
               MOVE WS-GW-ELEM-CNT TO WS-ED-COUNT
               MOVE WS-ED-COUNT TO WS-DF-QTY-EDIT.

       P5420-EXIT.
           EXIT.

       P5430-FRAGMENT-RATE.
      * THE RATE FRAGMENT IS ONLY PRINTED WHEN EVERY ELEMENT IN THE LINE
      * CARRIES THE SAME RATE.  A MIXED LINE PRINTS BLANK.
           MOVE 'P5430-FRAGMENT-RATE' TO WS-PARA-NAME.
           MOVE SPACES TO WS-DF-RATE-EDIT.
           IF WS-GW-ELEM-CNT = 1
               MOVE WS-ES-RATE (1) TO WS-ED-RATE
               MOVE WS-ED-RATE TO WS-DF-RATE-EDIT.

       P5430-EXIT.
           EXIT.

       P5440-FRAGMENT-JURIS.
      * THE JURISDICTION WORD.  THE TARIFF WORDING IS FIXED BY FILING AND
      * MUST NOT BE ABBREVIATED FURTHER.
           MOVE 'P5440-FRAGMENT-JURIS' TO WS-PARA-NAME.
           MOVE SPACES TO WS-DF-JURIS-WORD.
           PERFORM P5800-JURIS-WORD THRU P5800-EXIT
               VARYING WS-JW-X FROM 1 BY 1
               UNTIL WS-JW-X > 6.
           IF WS-DF-JURIS-WORD = SPACES
               MOVE 'INDETERMIN' TO WS-DF-JURIS-WORD.
           MOVE WS-GW-STATE-CD TO WS-DF-STATE.

       P5440-EXIT.
           EXIT.

       P5450-FRAGMENT-PERIOD.
      * THE BILL PERIOD IN MON YYYY FORM.  THE CONVERSION GOES THROUGH
      * CABDATCV SO THAT THE CENTURY IS RESOLVED IN ONE PLACE.
           MOVE 'P5450-FRAGMENT-PERIOD' TO WS-PARA-NAME.
           MOVE SPACES TO WS-DF-PERIOD-TEXT.
           MOVE 'JG' TO WS-DP-FUNCTION.
           MOVE WS-CYCLE-YYDDD TO WS-DP-YYDDD.
           CALL 'CABDATCV' USING WS-DATE-PARM.
           IF WS-DP-RC NOT = ZERO
               MOVE 'UNKNOWN  ' TO WS-DF-PERIOD-TEXT
               GO TO P5450-EXIT.
           MOVE WS-DP-CCYYMMDD TO DW-GREG-DATE.
           MOVE DW-GR-CCYY TO WS-PW-CCYY.
           MOVE DW-GR-MM TO WS-PW-MM.
           IF WS-PW-MM < 1 OR WS-PW-MM > 12
               MOVE 1 TO WS-PW-MM.
           STRING WS-MN-NAME (WS-PW-MM) DELIMITED BY SIZE
                  ' '                   DELIMITED BY SIZE
                  WS-PW-CCYY            DELIMITED BY SIZE
                  INTO WS-DF-PERIOD-TEXT.

       P5450-EXIT.
           EXIT.

       P5460-APPEND-CONT.
      * A CONTINUATION LINE CARRIES ITS NUMBER AT THE END OF THE
      * DESCRIPTION.  THE PRINT FORMATTER LOOKS FOR THE WORD CONT.
           MOVE 'P5460-APPEND-CONT' TO WS-PARA-NAME.
           MOVE SPACES TO WS-DF-CONT-TEXT.
           STRING ' (CONT '            DELIMITED BY SIZE
                  WS-GW-CONT-NBR       DELIMITED BY SIZE
                  INTO WS-DF-CONT-TEXT.
           MOVE 52 TO WS-DS-PTR.
           STRING WS-DF-CONT-TEXT      DELIMITED BY SIZE
                  INTO WS-DESC-ASSEMBLED
                  WITH POINTER WS-DS-PTR.

       P5460-EXIT.
           EXIT.

       P5500-TRIM-DESCRIPTION.
      * SQUEEZE OUT DOUBLE BLANKS AND ESTABLISH THE TRUE LENGTH BY
      * WALKING BACKWARDS FROM POSITION SIXTY.  THE PRINT FORMATTER
      * CENTRES ON THE TRUE LENGTH AND A TRAILING BLANK RUN THROWS IT.
      * SCAN LOGIC PER CABS-STD-063 (PRINT LINE EDITING).
           MOVE 'P5500-TRIM-DESCRIPTION' TO WS-PARA-NAME.
           MOVE ZERO TO WS-DS-BLANK-CNT WS-DS-DIGIT-CNT.
           INSPECT WS-DESC-ASSEMBLED
               TALLYING WS-DS-BLANK-CNT FOR ALL '  '.
           INSPECT WS-DESC-ASSEMBLED
               TALLYING WS-DS-DIGIT-CNT FOR ALL '0'.
           INSPECT WS-DESC-ASSEMBLED
               REPLACING ALL '   ' BY '  '.
           MOVE 60 TO WS-DS-SCAN-SUB.
           MOVE 'N' TO WS-DS-TRIM-DONE-SW.
           PERFORM P5510-SCAN-BACK THRU P5510-EXIT
               UNTIL WS-DS-TRIM-DONE OR WS-DS-SCAN-SUB < 1.
           MOVE WS-DS-SCAN-SUB TO WS-DS-TRUE-LEN.

       P5500-EXIT.
           EXIT.

       P5510-SCAN-BACK.
           IF WS-DA-CHAR (WS-DS-SCAN-SUB) NOT = SPACE
               MOVE 'Y' TO WS-DS-TRIM-DONE-SW
               GO TO P5510-EXIT.
           SUBTRACT 1 FROM WS-DS-SCAN-SUB.

       P5510-EXIT.
           EXIT.

       P5600-EDIT-DESCRIPTION.
      * THE DESCRIPTION MUST NOT BE EMPTY AND MUST NOT CONTAIN A LOW
      * VALUE.  EITHER WOULD STOP THE PRINT FORMATTER, AND THAT FAILURE
      * SHOWS UP AS A BLANK PAGE IN THE MIDDLE OF A CARRIER BILL.
           MOVE 'P5600-EDIT-DESCRIPTION' TO WS-PARA-NAME.
           IF WS-DS-TRUE-LEN = ZERO
               MOVE EC-RATE-NOT-FOUND TO WS-ERR-CODE
               MOVE 'E' TO WS-ERR-SEVERITY
               GO TO P9990-DETAIL-FAILURE.
           INSPECT WS-DESC-ASSEMBLED
               REPLACING ALL LOW-VALUE BY SPACE.
           INSPECT WS-DESC-ASSEMBLED
               REPLACING ALL HIGH-VALUE BY SPACE.

       P5600-EXIT.
           EXIT.

       P5700-LOOKUP-ELEM.
           IF WS-EN-CODE (WS-EN-X) = WS-ES-RATE-ELEM (1)
               MOVE WS-EN-NAME (WS-EN-X) TO WS-DF-ELEM-NAME
               MOVE 'Y' TO WS-ELEM-FOUND-SW.

       P5700-EXIT.
           EXIT.

       P5800-JURIS-WORD.
           IF WS-JW-CODE (WS-JW-X) = WS-GW-JURIS-CD
               MOVE WS-JW-WORD (WS-JW-X) TO WS-DF-JURIS-WORD.

       P5800-EXIT.
           EXIT.

      *****************************************************************
      * S700-ELEMENT CLASS VALIDATION                                 *
      * EIGHT CLASS ROUTINES REACHED BY GO TO DEPENDING ON FROM P4400.*
      * EACH ONE OWNS ITS OWN INTERPRETATION OF WHAT A QUANTITY MEANS.*
      *****************************************************************
       S700-CLASSES SECTION.

       P7101-VALIDATE-TAN.
      * TERMINATING ACCESS ELEMENTS.
      * MINUTES MUST BE POSITIVE AND THE RATE MUST
      * BE NON ZERO UNLESS THE ELEMENT IS UNDER A ZERO RATE ORDER
           MOVE 'P7101-VALIDATE-TAN' TO WS-PARA-NAME.
           IF RD-QTY < ZERO
               IF RD-RATE-ELEM NOT = 'ADJCRD'
                   MOVE EC-MIN-NEGATIVE TO WS-ERR-CODE
                   MOVE 'E' TO WS-ERR-SEVERITY
                   GO TO P9990-DETAIL-FAILURE.
           IF RD-RATE = ZERO
               IF RD-AMOUNT NOT = ZERO
                   MOVE EC-RATE-NOT-FOUND TO WS-ERR-CODE
                   MOVE 'W' TO WS-ERR-SEVERITY
                   ADD 1 TO WS-EV-ZERO-RATE.
           COMPUTE WS-EV-CHECK-AMT = RD-QTY * RD-RATE.
           COMPUTE WS-EV-VARIANCE = WS-EV-CHECK-AMT - RD-AMOUNT.
           IF WS-EV-VARIANCE > WS-EV-TOLERANCE
               ADD 1 TO WS-EV-RECOMPUTE (1 ).
           IF WS-EV-VARIANCE < WS-EV-NEG-TOLERANCE
               ADD 1 TO WS-EV-RECOMPUTE (1 ).
           ADD 1 TO WS-EV-CLASS-CNT (1 ).

       P7101-EXIT.
           EXIT.

       P7102-VALIDATE-ORG.
      * ORIGINATING ACCESS ELEMENTS.
      * QUERY ELEMENTS CARRY A COUNT, NOT MINUTES,
      * AND ARE ALLOWED A ZERO MINUTE QUANTITY
           MOVE 'P7102-VALIDATE-ORG' TO WS-PARA-NAME.
           IF RD-QTY < ZERO
               IF RD-RATE-ELEM NOT = 'ADJCRD'
                   MOVE EC-MIN-NEGATIVE TO WS-ERR-CODE
                   MOVE 'E' TO WS-ERR-SEVERITY
                   GO TO P9990-DETAIL-FAILURE.
           IF RD-RATE = ZERO
               IF RD-AMOUNT NOT = ZERO
                   MOVE EC-RATE-NOT-FOUND TO WS-ERR-CODE
                   MOVE 'W' TO WS-ERR-SEVERITY
                   ADD 1 TO WS-EV-ZERO-RATE.
           COMPUTE WS-EV-CHECK-AMT = RD-QTY * RD-RATE.
           COMPUTE WS-EV-VARIANCE = WS-EV-CHECK-AMT - RD-AMOUNT.
           IF WS-EV-VARIANCE > WS-EV-TOLERANCE
               ADD 1 TO WS-EV-RECOMPUTE (2 ).
           IF WS-EV-VARIANCE < WS-EV-NEG-TOLERANCE
               ADD 1 TO WS-EV-RECOMPUTE (2 ).
           ADD 1 TO WS-EV-CLASS-CNT (2 ).

       P7102-EXIT.
           EXIT.

       P7103-VALIDATE-SPC.
      * SPECIAL ACCESS ELEMENTS.
      * FIXED MONTHLY ELEMENTS CARRY A QUANTITY OF ONE
      * AND A PRORATION FACTOR ALREADY APPLIED UPSTREAM
           MOVE 'P7103-VALIDATE-SPC' TO WS-PARA-NAME.
           IF RD-QTY < ZERO
               IF RD-RATE-ELEM NOT = 'ADJCRD'
                   MOVE EC-MIN-NEGATIVE TO WS-ERR-CODE
                   MOVE 'E' TO WS-ERR-SEVERITY
                   GO TO P9990-DETAIL-FAILURE.
           IF RD-RATE = ZERO
               IF RD-AMOUNT NOT = ZERO
                   MOVE EC-RATE-NOT-FOUND TO WS-ERR-CODE
                   MOVE 'W' TO WS-ERR-SEVERITY
                   ADD 1 TO WS-EV-ZERO-RATE.
           COMPUTE WS-EV-CHECK-AMT = RD-QTY * RD-RATE.
           COMPUTE WS-EV-VARIANCE = WS-EV-CHECK-AMT - RD-AMOUNT.
           IF WS-EV-VARIANCE > WS-EV-TOLERANCE
               ADD 1 TO WS-EV-RECOMPUTE (3 ).
           IF WS-EV-VARIANCE < WS-EV-NEG-TOLERANCE
               ADD 1 TO WS-EV-RECOMPUTE (3 ).
           ADD 1 TO WS-EV-CLASS-CNT (3 ).

       P7103-EXIT.
           EXIT.

       P7104-VALIDATE-UNE.
      * UNBUNDLED ELEMENTS ELEMENTS.
      * QUANTITY IS A COUNT OF ELEMENTS IN SERVICE,
      * NOT MINUTES - A FRACTIONAL QUANTITY IS AN ERROR
           MOVE 'P7104-VALIDATE-UNE' TO WS-PARA-NAME.
           IF RD-QTY < ZERO
               IF RD-RATE-ELEM NOT = 'ADJCRD'
                   MOVE EC-MIN-NEGATIVE TO WS-ERR-CODE
                   MOVE 'E' TO WS-ERR-SEVERITY
                   GO TO P9990-DETAIL-FAILURE.
           IF RD-RATE = ZERO
               IF RD-AMOUNT NOT = ZERO
                   MOVE EC-RATE-NOT-FOUND TO WS-ERR-CODE
                   MOVE 'W' TO WS-ERR-SEVERITY
                   ADD 1 TO WS-EV-ZERO-RATE.
           COMPUTE WS-EV-CHECK-AMT = RD-QTY * RD-RATE.
           COMPUTE WS-EV-VARIANCE = WS-EV-CHECK-AMT - RD-AMOUNT.
           IF WS-EV-VARIANCE > WS-EV-TOLERANCE
               ADD 1 TO WS-EV-RECOMPUTE (4 ).
           IF WS-EV-VARIANCE < WS-EV-NEG-TOLERANCE
               ADD 1 TO WS-EV-RECOMPUTE (4 ).
           ADD 1 TO WS-EV-CLASS-CNT (4 ).

       P7104-EXIT.
           EXIT.

       P7105-VALIDATE-RCP.
      * RECIPROCAL COMPENSATION ELEMENTS.
      * THE CAPPED MINUTE FLAG IS CARRIED IN THE
      * SOURCE PROCESS FIELD BY CABSET05
           MOVE 'P7105-VALIDATE-RCP' TO WS-PARA-NAME.
           IF RD-QTY < ZERO
               IF RD-RATE-ELEM NOT = 'ADJCRD'
                   MOVE EC-MIN-NEGATIVE TO WS-ERR-CODE
                   MOVE 'E' TO WS-ERR-SEVERITY
                   GO TO P9990-DETAIL-FAILURE.
           IF RD-RATE = ZERO
               IF RD-AMOUNT NOT = ZERO
                   MOVE EC-RATE-NOT-FOUND TO WS-ERR-CODE
                   MOVE 'W' TO WS-ERR-SEVERITY
                   ADD 1 TO WS-EV-ZERO-RATE.
           COMPUTE WS-EV-CHECK-AMT = RD-QTY * RD-RATE.
           COMPUTE WS-EV-VARIANCE = WS-EV-CHECK-AMT - RD-AMOUNT.
           IF WS-EV-VARIANCE > WS-EV-TOLERANCE
               ADD 1 TO WS-EV-RECOMPUTE (5 ).
           IF WS-EV-VARIANCE < WS-EV-NEG-TOLERANCE
               ADD 1 TO WS-EV-RECOMPUTE (5 ).
           ADD 1 TO WS-EV-CLASS-CNT (5 ).

       P7105-EXIT.
           EXIT.

       P7106-VALIDATE-MPB.
      * MEET POINT BILLING ELEMENTS.
      * THE BILLED SHARE HAS ALREADY BEEN SPLIT BY
      * CABSET01 - THIS PROGRAM DOES NOT RE-SPLIT IT
           MOVE 'P7106-VALIDATE-MPB' TO WS-PARA-NAME.
           IF RD-QTY < ZERO
               IF RD-RATE-ELEM NOT = 'ADJCRD'
                   MOVE EC-MIN-NEGATIVE TO WS-ERR-CODE
                   MOVE 'E' TO WS-ERR-SEVERITY
                   GO TO P9990-DETAIL-FAILURE.
           IF RD-RATE = ZERO
               IF RD-AMOUNT NOT = ZERO
                   MOVE EC-RATE-NOT-FOUND TO WS-ERR-CODE
                   MOVE 'W' TO WS-ERR-SEVERITY
                   ADD 1 TO WS-EV-ZERO-RATE.
           COMPUTE WS-EV-CHECK-AMT = RD-QTY * RD-RATE.
           COMPUTE WS-EV-VARIANCE = WS-EV-CHECK-AMT - RD-AMOUNT.
           IF WS-EV-VARIANCE > WS-EV-TOLERANCE
               ADD 1 TO WS-EV-RECOMPUTE (6 ).
           IF WS-EV-VARIANCE < WS-EV-NEG-TOLERANCE
               ADD 1 TO WS-EV-RECOMPUTE (6 ).
           ADD 1 TO WS-EV-CLASS-CNT (6 ).

       P7106-EXIT.
           EXIT.

       P7107-VALIDATE-NRC.
      * NON RECURRING ELEMENTS.
      * ONE OCCURRENCE PER SERVICE ORDER.  A QUANTITY
      * GREATER THAN ONE IS ACCEPTED AND MULTIPLIED
           MOVE 'P7107-VALIDATE-NRC' TO WS-PARA-NAME.
           IF RD-QTY < ZERO
               IF RD-RATE-ELEM NOT = 'ADJCRD'
                   MOVE EC-MIN-NEGATIVE TO WS-ERR-CODE
                   MOVE 'E' TO WS-ERR-SEVERITY
                   GO TO P9990-DETAIL-FAILURE.
           IF RD-RATE = ZERO
               IF RD-AMOUNT NOT = ZERO
                   MOVE EC-RATE-NOT-FOUND TO WS-ERR-CODE
                   MOVE 'W' TO WS-ERR-SEVERITY
                   ADD 1 TO WS-EV-ZERO-RATE.
           COMPUTE WS-EV-CHECK-AMT = RD-QTY * RD-RATE.
           COMPUTE WS-EV-VARIANCE = WS-EV-CHECK-AMT - RD-AMOUNT.
           IF WS-EV-VARIANCE > WS-EV-TOLERANCE
               ADD 1 TO WS-EV-RECOMPUTE (7 ).
           IF WS-EV-VARIANCE < WS-EV-NEG-TOLERANCE
               ADD 1 TO WS-EV-RECOMPUTE (7 ).
           ADD 1 TO WS-EV-CLASS-CNT (7 ).

       P7107-EXIT.
           EXIT.

       P7108-VALIDATE-ADJ.
      * ADJUSTMENTS ELEMENTS.
      * A NEGATIVE AMOUNT IS NORMAL.  THE SIGN IS TAKEN
      * FROM THE AMOUNT, NOT FROM THE ELEMENT CODE
           MOVE 'P7108-VALIDATE-ADJ' TO WS-PARA-NAME.
           IF RD-QTY < ZERO
               IF RD-RATE-ELEM NOT = 'ADJCRD'
                   MOVE EC-MIN-NEGATIVE TO WS-ERR-CODE
                   MOVE 'E' TO WS-ERR-SEVERITY
                   GO TO P9990-DETAIL-FAILURE.
           IF RD-RATE = ZERO
               IF RD-AMOUNT NOT = ZERO
                   MOVE EC-RATE-NOT-FOUND TO WS-ERR-CODE
                   MOVE 'W' TO WS-ERR-SEVERITY
                   ADD 1 TO WS-EV-ZERO-RATE.
           COMPUTE WS-EV-CHECK-AMT = RD-QTY * RD-RATE.
           COMPUTE WS-EV-VARIANCE = WS-EV-CHECK-AMT - RD-AMOUNT.
           IF WS-EV-VARIANCE > WS-EV-TOLERANCE
               ADD 1 TO WS-EV-RECOMPUTE (8 ).
           IF WS-EV-VARIANCE < WS-EV-NEG-TOLERANCE
               ADD 1 TO WS-EV-RECOMPUTE (8 ).
           ADD 1 TO WS-EV-CLASS-CNT (8 ).

       P7108-EXIT.
           EXIT.

       P7200-LOAD-TRIGGERS.
      * LOAD THE TRIGGER FILE INTO STORAGE.  TWO THOUSAND ACCOUNTS IS
      * THE LIMIT.  A FULL TABLE IS FATAL - RUNNING ON WITH A PARTIAL
      * TABLE WOULD SILENTLY DROP THE ACCOUNTS THAT DID NOT FIT.
           MOVE 'P7200-LOAD-TRIGGERS' TO WS-PARA-NAME.
           MOVE ZERO TO WS-TT-USED.
           PERFORM P7210-READ-TRIGGER THRU P7210-EXIT
               UNTIL WS-TRG-EOF.
           DISPLAY 'TRIGGER ENTRIES LOADED ' WS-TT-USED.

       P7200-EXIT.
           EXIT.

       P7210-READ-TRIGGER.
           READ TRIG-IN-FILE INTO WS-TRIGGER-IN
               AT END
                   MOVE 'Y' TO WS-TRG-EOF-SW
                   GO TO P7210-EXIT.
           IF WS-TT-USED NOT < WS-TT-MAX
               MOVE 4203 TO WS-AB-CODE
               MOVE 'TRIGGER TABLE FULL - RAISE THE OCCURS'
                                       TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-TT-USED.
           SET WS-TT-X TO WS-TT-USED.
           MOVE WS-TI-BAN          TO WS-TT-BAN (WS-TT-X).
           MOVE WS-TI-BILL-PERIOD  TO WS-TT-BILL-PERIOD (WS-TT-X).
           MOVE WS-TI-OCN          TO WS-TT-OCN (WS-TT-X).
           MOVE WS-TI-DUE-YYDDD    TO WS-TT-DUE-YYDDD (WS-TT-X).
           MOVE WS-TI-TRIGGER-CD   TO WS-TT-TRIGGER-CD (WS-TT-X).

       P7210-EXIT.
           EXIT.

       P7300-SET-MANDATORY.
      * SECTIONS T1 AND A1 ARE PRINTED EVEN WHEN THEY ARE EMPTY.  THE
      * TARIFF REQUIRES THE TAX SECTION TO APPEAR ON EVERY INVOICE AND
      * THE ADJUSTMENT SECTION TO APPEAR WHENEVER ONE WAS RAISED IN THE
      * PERIOD, WHICH THIS PROGRAM CANNOT SEE, SO IT IS ALWAYS SHOWN.
           MOVE 'P7300-SET-MANDATORY' TO WS-PARA-NAME.
           PERFORM P7310-CLEAR-BUCKET THRU P7310-EXIT
               VARYING WS-SUB1 FROM 1 BY 1
               UNTIL WS-SUB1 > 12.
           MOVE 'Y' TO WS-SB-MANDATORY (10).
           MOVE 'Y' TO WS-SB-MANDATORY (11).

       P7300-EXIT.
           EXIT.

       P7310-CLEAR-BUCKET.
           MOVE ZERO TO WS-SB-AMOUNT (WS-SUB1)
                        WS-SB-MINUTES (WS-SUB1)
                        WS-SB-LINES (WS-SUB1)
                        WS-SB-ELEMS (WS-SUB1)
                        WS-SB-SUPPRESSED (WS-SUB1).
           MOVE 'N' TO WS-SB-MANDATORY (WS-SUB1).

       P7310-EXIT.
           EXIT.

       P7400-PRINT-SECTION-TOTALS.
      * THE SECTION RECAP AT THE END OF THE REGISTER.  THIS IS THE ONLY
      * PLACE THE SECTION SPLIT OF A RUN APPEARS IN PRINT BEFORE THE
      * INVOICES THEMSELVES ARE FORMATTED.
           MOVE 'P7400-PRINT-SECTION-TOTALS' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '1' TO PC-CC.
           MOVE 'BILL DETAIL ASSEMBLY - SECTION RECAP' TO PC-TEXT.
           WRITE PRT-RECORD FROM CABS-PRINT-LINE.
           PERFORM P7410-PRINT-ONE-SECTION THRU P7410-EXIT
               VARYING WS-ST-X FROM 1 BY 1
               UNTIL WS-ST-X > 12.

       P7400-EXIT.
           EXIT.

       P7410-PRINT-ONE-SECTION.
           SET WS-SUB1 TO WS-ST-X.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-ST-CODE (WS-ST-X)   TO PC-COL-001-020.
           MOVE WS-ST-NAME (WS-ST-X)   TO PC-COL-021-060.
           MOVE WS-SB-LINES (WS-SUB1)  TO WS-ED-COUNT.
           MOVE WS-ED-COUNT            TO PC-COL-061-090.
           MOVE WS-SB-AMOUNT (WS-SUB1) TO WS-ED-MONEY.
           MOVE WS-ED-MONEY            TO PC-COL-091-132.
           WRITE PRT-RECORD FROM CABS-PRINT-LINE.

       P7410-EXIT.
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
           MOVE 'CABBIL02  BILL DETAIL LINE ASSEMBLY REGISTER'
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
           MOVE 'BAN                 SECTION LINES          AMOUNT'
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
           MOVE 410                    TO CT-STEP-SEQ.
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
           PERFORM P7400-PRINT-SECTION-TOTALS THRU P7400-EXIT.
           DISPLAY 'DETAIL LINES BUILT' WS-RT-DETAIL-LINES.
           DISPLAY 'RATE ELEMENTS     ' WS-RT-ELEMENTS.
           DISPLAY 'CONTINUATIONS     ' WS-RT-CONTINUATIONS.
           DISPLAY 'LINES SUPPRESSED  ' WS-RT-SUPPRESSED.
           DISPLAY 'ACCOUNTS TOUCHED  ' WS-RT-ACCOUNTS.
           DISPLAY 'NOT TRIGGERED     ' WS-RT-NOT-TRIGGERED.
           DISPLAY 'RAW AMOUNT 5DP    ' WS-RT-AMOUNT.
           DISPLAY 'ROUNDED AMOUNT    ' WS-RT-ROUNDED.
           DISPLAY 'ROUNDING RESIDUE  ' WS-RW-RUN-DELTA.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE RATED-IN-FILE
                 TRIG-IN-FILE
                 BILL-DTL-FILE
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

       P7150-CLEAR-CLASS.
           MOVE ZERO TO WS-EV-CLASS-CNT (WS-SUB1)
                        WS-EV-RECOMPUTE (WS-SUB1).

       P7150-EXIT.
           EXIT.

       P9990-DETAIL-FAILURE.
      * A RATED RECORD THAT CANNOT BE TURNED INTO A DETAIL LINE.  THE
      * RECORD IS SUSPENDED AND THE RUN CARRIES ON - A SINGLE BAD USAGE
      * RECORD MUST NOT STOP AN ENTIRE BILL CYCLE.  THE SUSPENSE FILE IS
      * WORKED THE FOLLOWING MORNING AND RE-PRESENTED BY THE RECYCLE JOB.
      * REACHED BY GO TO FROM P3000, P4400 AND P5600.
      * RETAINED PER CABS-STD-047 - DO NOT RESEQUENCE.
           MOVE CABS-RATED-DETAIL-RECORD TO WS-EW-DATA.
           PERFORM P7000-SUSPEND THRU P7000-EXIT.
           IF WS-ERR-SEVERITY = 'F'
               MOVE 'N' TO WS-GROUP-OPEN-SW.
           MOVE SPACES TO WS-ERR-CODE.
           MOVE 'W' TO WS-ERR-SEVERITY.
           GO TO P2000-EXIT.

       P9990-EXIT.
           EXIT.
