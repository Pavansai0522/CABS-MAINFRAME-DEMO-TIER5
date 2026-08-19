      *****************************************************************
      * CABSET05 - RECIPROCAL COMPENSATION CALCULATION WITH ISP CAP   *
      * APPLICATION : SETL                                            *
      * INPUTS      : RECAGG   TELCABS.SETL.RECIP.AGG(0)      CABSSETL*
      * INPUTS      : RECIPCDR TELCABS.CABS.CDR.RECIP(0)      CABSCDR *
      * INPUTS      : CARRMAST TELCABS.SETL.CARRIER           CABSCARR*
      * INPUTS      : SYSIN    INSTREAM                       NONE    *
      * OUTPUTS     : SETLOUT  TELCABS.SETL.SETTLE.RECIP(+1)  CABSSETL*
      * OUTPUTS     : CAPOUT   TELCABS.SETL.RECIP.CAPPED(+1)  NONE    *
      * OUTPUTS     : REPORT   SYSOUT                         CABSPRNT*
      * CONTROL     : CTLOUT                         CABSCTL          *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARIS*
      *               SUMMARISED = CARRIERS WITH NO BILLABLE MINUTES  *
      *               TOTAL MOU = BILLABLE MOU + CAPPED MOU           *
      * RESTART     : FULL RERUN                                      *
      * THE RECIPROCAL COMPENSATION CALCULATOR.  TERMINATING          *
      * MINUTES FROM A CLEC ARE COMPENSATED AT THE                    *
      * NEGOTIATED RATE.  ISP BOUND MINUTES ABOVE THE CAP             *
      * ARE COMPENSATED AT ZERO.                                      *
      * STANDARDS   : CODED TO CABS-STD-041 (MONEY FIELDS) AND        *
      *               CABS-STD-063 (PRINT CONTROL).                   *
      * REVISION HISTORY                                              *
      *   V1.00  1996-05-28  J.M.CASTILLO  INITIAL - 1996 ACT         *
      *   V1.02  1997-11-14  J.M.CASTILLO  ISP BOUND CAP ADDED        *
      *   V1.05  1999-06-21  P.NAIR        CAP BY STATE ADDED         *
      *   V2.00  2001-04-27  P.NAIR        FCC ORDER 01-131 RATES     *
      *   V2.02  2004-10-19  P.NAIR        GROWTH CAP REMOVED         *
      *   V2.04  2009-03-05  A.BUKOWSKI    CAPPED MOU FILE ADDED      *
      *   V2.06  2012-01-30  A.BUKOWSKI    CAP APPLIED BEFORE RATE    *
      *   V2.08  2016-05-11  L.FERREIRA    RECOMPILE ONLY LE V6       *
      *   V2.08  2019-10-08  M.OYELARAN    NO CODE CHANGE - AUDIT     *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CABSET05.
       AUTHOR.        J.M.CASTILLO.
       DATE-WRITTEN.  1996-05-28.
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
      * AGGREGATED TERMINATING MINUTES FROM CABSET04
           SELECT RECIP-AGG-FILE
               ASSIGN TO UT-S-RECAGG
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
      * OWNED BY THE CABS APPLICATION - CROSS APP READ
           SELECT RECIP-CDR-FILE
               ASSIGN TO UT-S-RECIPCDR
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-TABLE.
      * CARRIER MASTER - RECIP RATE AND ISP CAP
           SELECT CARRIER-MASTER
               ASSIGN TO DA-I-CARRMAST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CRM-KEY
               FILE STATUS IS WS-FS-SUSPENSE.
      * RECIPROCAL COMPENSATION SETTLEMENT RECORDS
           SELECT SETTLE-OUT-FILE
               ASSIGN TO UT-S-SETLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
      * CAPPED MINUTE DETAIL - EVIDENCE FOR THE CLEC
           SELECT CAPPED-OUT-FILE
               ASSIGN TO UT-S-CAPOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CONTROL.
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
       FD  RECIP-AGG-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 180 CHARACTERS
               DATA RECORD IS RAI-RECORD.
       01  RAI-RECORD              PIC X(180).

       FD  RECIP-CDR-FILE
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

       FD  SETTLE-OUT-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 180 CHARACTERS
               DATA RECORD IS STO-RECORD.
       01  STO-RECORD              PIC X(180).

       FD  CAPPED-OUT-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 120 CHARACTERS
               DATA RECORD IS CAP-RECORD.
       01  CAP-RECORD              PIC X(120).

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
           05  WS-PGM-NAME         PIC X(08)         VALUE 'CABSET05'.
           05  WS-PGM-VERSION        PIC X(05)           VALUE 'V2.08'.
           05  WS-PGM-APPL           PIC X(04)           VALUE 'SETL'.
           05  WS-PGM-COMPILE      PIC X(08)         VALUE '20191008'.
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

       COPY CABSSETL.

       COPY CABSCARR.

       COPY CABSCDR.

       COPY CABSPRNT.

       COPY CABSRATE.

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
      * CARD LAYOUT FROZEN UNDER CABS-STD-014 SINCE THE 1994 REWRITE.
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
           05  WS-PE-DEF-RATE          PIC 9(01)V9(05).
           05  WS-PE-CAP-OVERRIDE      PIC 9(09).
           05  WS-PE-STATE             PIC X(02).
           05  WS-PE-SIM-SW            PIC X(01).
           05  WS-PE-FILLER            PIC X(11).
       01  WS-PARM-OLD REDEFINES WS-PARM-CARD.
      * THE 1989 CARD.  TWO REGIONS NEVER CONVERTED THEIR JCL AND
      * THE EDIT STILL ACCEPTS IT.  THE FIELDS ARE IN A DIFFERENT
      * ORDER AND THE CYCLE DATE IS SIX DIGITS YYMMDD, NOT YYDDD.
           05  FILLER                  PIC X(02).
           05  WS-PO-CYCLE             PIC 9(05).
           05  WS-PO-RUN-ID            PIC X(12).
           05  WS-PO-YYMMDD            PIC 9(06).
           05  WS-PO-PERIOD            PIC 9(06).
           05  WS-PO-RATE              PIC 9(01)V9(05).
           05  FILLER                  PIC X(20).

      * LOCAL SWITCHES.  THE STANDARD ONES ARRIVE VIA CABSWRK.
       01  WS-LOCAL-SWITCHES.
           05  WS-PARM-EOF-SW          PIC X(01)             VALUE 'N'.
                   88  WS-PARM-EOF             VALUE 'Y'.
           05  WS-CDR-EOF-SW           PIC X(01)             VALUE 'N'.
                   88  WS-CDR-EOF              VALUE 'Y'.
           05  WS-CARR-FOUND-SW        PIC X(01)             VALUE 'N'.
                   88  WS-CARR-FOUND           VALUE 'Y'.
           05  WS-AGREE-FOUND-SW       PIC X(01)             VALUE 'N'.
                   88  WS-AGREE-FOUND          VALUE 'Y'.
           05  WS-CAP-APPLIED-SW       PIC X(01)             VALUE 'N'.
                   88  WS-CAP-APPLIED          VALUE 'Y'.
           05  WS-ELIGIBLE-SW          PIC X(01)             VALUE 'Y'.
                   88  WS-ELIGIBLE             VALUE 'Y'.

      * SUBSCRIPTS AND INDEX WORK FIELDS.
       01  WS-SUBSCRIPTS.
           05  WS-SUB1                 PIC S9(05) COMP-3     VALUE 0.
           05  WS-SUB2                 PIC S9(05) COMP-3     VALUE 0.
           05  WS-SUB3                 PIC S9(05) COMP-3     VALUE 0.
           05  WS-IDX                  PIC S9(05) COMP-3     VALUE 0.

      * THE AGGREGATION RECORD AS WRITTEN BY CABSET04.  DECLARED
      * IN BOTH PROGRAMS.
       01  WS-AGG-IN.
           05  WS-AI-OCN             PIC X(04)           VALUE SPACES.
           05  WS-AI-PERIOD            PIC 9(06)             VALUE 0.
           05  WS-AI-TOTAL-MOU         PIC S9(15)V9(02)      VALUE 0.
           05  WS-AI-ISP-MOU           PIC S9(15)V9(02)      VALUE 0.
           05  WS-AI-VOICE-MOU         PIC S9(15)V9(02)      VALUE 0.
           05  WS-AI-RATE              PIC S9(05)V9(05)      VALUE 0.
           05  WS-AI-CAP               PIC S9(13)            VALUE 0.
           05  WS-AI-COUNT             PIC 9(11)             VALUE 0.
           05  WS-AI-FILLER          PIC X(60)           VALUE SPACES.
       01  WS-AGG-IN-K REDEFINES WS-AGG-IN.
           05  WS-AIK-KEY              PIC X(10).
           05  WS-AIK-REST             PIC X(170).
       01  WS-AGG-IN-M REDEFINES WS-AGG-IN.
           05  WS-AIM-HEAD             PIC X(10).
           05  WS-AIM-MOU-AREA         PIC X(51).
           05  WS-AIM-TAIL             PIC X(119).

      * THE RECIPROCAL COMPENSATION ARITHMETIC.  TERMINATING
      * MINUTES ARE COMPENSATED AT THE NEGOTIATED RATE.  ISP
      * BOUND MINUTES ARE SUBJECT TO A CAP AGREED IN THE
      * INTERCONNECTION AGREEMENT - MINUTES ABOVE THE CAP ARE
      * COMPENSATED AT ZERO.  THE ORDER IN WHICH THE CAP AND
      * THE RATE ARE APPLIED DECIDES WHETHER THE CAPPED
      * MINUTES COST ANYTHING.
       01  WS-RECIP-WORK.
           05  WS-RC-TOTAL-MOU         PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-RC-ISP-MOU           PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-RC-VOICE-MOU         PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-RC-BILL-MOU          PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-RC-CAPPED-MOU        PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-RC-CAP               PIC S9(13) COMP-3     VALUE 0.
           05  WS-RC-RATE              PIC S9(05)V9(05) COMP-3 VALUE 0.
           05  WS-RC-GROSS-AMT         PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-RC-CAP-AMT           PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-RC-NET-DUE           PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-RC-EXACT-AMT         PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-RC-ROUND-DIFF        PIC S9(05)V9(05) COMP-3 VALUE 0.
           05  WS-RC-OVER-MOU          PIC S9(15)V9(02) COMP-3 VALUE 0.

      * RECIPROCAL COMPENSATION RUN TOTALS.
       01  WS-RECIP-TOTALS.
           05  WS-SETTLE-CNT           PIC S9(09) COMP-3     VALUE 0.
           05  WS-CAPPED-CNT           PIC S9(09) COMP-3     VALUE 0.
           05  WS-NOAGREE-CNT          PIC S9(09) COMP-3     VALUE 0.
           05  WS-ZERORATE-CNT         PIC S9(09) COMP-3     VALUE 0.
           05  WS-TOT-MOU              PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-TOT-BILL-MOU         PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-TOT-CAP-MOU          PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-TOT-GROSS            PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-TOT-NET              PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-TOT-CAP-AMT          PIC S9(15)V9(05) COMP-3 VALUE 0.
           05  WS-TOT-ROUND-DIFF       PIC S9(09)V9(05) COMP-3 VALUE 0.

      * CAPPED MINUTE DETAIL RECORD.  SENT TO THE CLEC AS THE
      * EVIDENCE FOR WHY THEIR INVOICE IS SHORT.
       01  WS-CAP-RECORD.
           05  WS-CP-OCN             PIC X(04)           VALUE SPACES.
           05  WS-CP-PERIOD            PIC 9(06)             VALUE 0.
           05  WS-CP-STATE           PIC X(02)           VALUE SPACES.
           05  WS-CP-TOTAL-MOU         PIC S9(15)V9(02)      VALUE 0.
           05  WS-CP-ISP-MOU           PIC S9(15)V9(02)      VALUE 0.
           05  WS-CP-CAP               PIC S9(13)            VALUE 0.
           05  WS-CP-CAPPED-MOU        PIC S9(15)V9(02)      VALUE 0.
           05  WS-CP-CAP-AMT           PIC S9(13)V9(05)      VALUE 0.
           05  WS-CP-RATE              PIC S9(05)V9(05)      VALUE 0.
           05  WS-CP-FILLER          PIC X(30)           VALUE SPACES.
       01  WS-CAP-KEY-R REDEFINES WS-CAP-RECORD.
           05  WS-CK-KEY               PIC X(12).
           05  WS-CK-REST              PIC X(108).

      * INTERCONNECTION AGREEMENT TABLE.  EACH CLEC NEGOTIATED ITS
      * OWN RECIPROCAL RATE AND ITS OWN ISP BOUND CAP, STATE BY
      * STATE.  THE TABLE IS THE ONLY MACHINE READABLE RECORD OF
      * THOSE AGREEMENTS - THE SIGNED CONTRACTS ARE IN A FILING
      * CABINET AND THE TWO HAVE NOT BEEN RECONCILED SINCE 2004.
      * THE EFFECTIVE DATE IS A TWO DIGIT YEAR JULIAN.
       01  WS-AGREE-CONST.
           05  FILLER  PIC X(25)  VALUE '1213MI0010000000000015079'.
           05  FILLER  PIC X(25)  VALUE '1402CT0070001000000008161'.
           05  FILLER  PIC X(25)  VALUE '1540ID0015000100000015153'.
           05  FILLER  PIC X(25)  VALUE '1557ME0007001000000096295'.
           05  FILLER  PIC X(25)  VALUE '1624NY0010000500000099315'.
           05  FILLER  PIC X(25)  VALUE '1710LA0010001000000099281'.
           05  FILLER  PIC X(25)  VALUE '1788IN0015000500000097207'.
           05  FILLER  PIC X(25)  VALUE '1934DE0002500500000096365'.
           05  FILLER  PIC X(25)  VALUE '2049ND0015000500000011138'.
           05  FILLER  PIC X(25)  VALUE '2069VT0035000000000097325'.
           05  FILLER  PIC X(25)  VALUE '2173FL0007000100000004177'.
           05  FILLER  PIC X(25)  VALUE '2239WV0035000000000001313'.
           05  FILLER  PIC X(25)  VALUE '2338TN0035000000000008211'.
           05  FILLER  PIC X(25)  VALUE '2397OH0020000250000004189'.
           05  FILLER  PIC X(25)  VALUE '2576UT0007000000000001242'.
           05  FILLER  PIC X(25)  VALUE '2752KS0002500500000004271'.
           05  FILLER  PIC X(25)  VALUE '2819MA0002500000000011101'.
           05  FILLER  PIC X(25)  VALUE '2907NM0020000250000097196'.
           05  FILLER  PIC X(25)  VALUE '2919SC0007000500000015133'.
           05  FILLER  PIC X(25)  VALUE '3244RI0015000000000001302'.
           05  FILLER  PIC X(25)  VALUE '3246AK0007000250000008362'.
           05  FILLER  PIC X(25)  VALUE '3442OK0007000250000097280'.
           05  FILLER  PIC X(25)  VALUE '3523GA0010000000000015337'.
           05  FILLER  PIC X(25)  VALUE '3600NH0002500000000004342'.
           05  FILLER  PIC X(25)  VALUE '3610HI0007000100000004163'.
           05  FILLER  PIC X(25)  VALUE '3758RI0070000100000008102'.
           05  FILLER  PIC X(25)  VALUE '3805PA0002500000000008312'.
           05  FILLER  PIC X(25)  VALUE '4317MN0010000100000015196'.
           05  FILLER  PIC X(25)  VALUE '4332NJ0070000000000004300'.
           05  FILLER  PIC X(25)  VALUE '4340DC0070000000000008342'.
           05  FILLER  PIC X(25)  VALUE '4345TX0015001000000004225'.
           05  FILLER  PIC X(25)  VALUE '4463NE0070001000000004166'.
           05  FILLER  PIC X(25)  VALUE '4503DE0015000250000004107'.
           05  FILLER  PIC X(25)  VALUE '4524IL0010000500000001341'.
           05  FILLER  PIC X(25)  VALUE '4678VT0002501000000011145'.
           05  FILLER  PIC X(25)  VALUE '4790NH0007000500000015359'.
           05  FILLER  PIC X(25)  VALUE '4898AL0015000250000008308'.
           05  FILLER  PIC X(25)  VALUE '4944WI0010000000000004357'.
           05  FILLER  PIC X(25)  VALUE '5102TX0035000500000008199'.
           05  FILLER  PIC X(25)  VALUE '5124MT0020001000000004072'.
           05  FILLER  PIC X(25)  VALUE '5393NC0070001000000097202'.
           05  FILLER  PIC X(25)  VALUE '5613OK0002500250000099223'.
           05  FILLER  PIC X(25)  VALUE '5636MT0070001000000001307'.
           05  FILLER  PIC X(25)  VALUE '5637GA0070000100000004066'.
           05  FILLER  PIC X(25)  VALUE '5743MD0070000250000004365'.
           05  FILLER  PIC X(25)  VALUE '5820ME0002500000000015085'.
           05  FILLER  PIC X(25)  VALUE '5869ID0015000100000011041'.
           05  FILLER  PIC X(25)  VALUE '6095NC0002500000000004051'.
           05  FILLER  PIC X(25)  VALUE '6160AR0015000250000097361'.
           05  FILLER  PIC X(25)  VALUE '6320MS0007000500000004179'.
           05  FILLER  PIC X(25)  VALUE '6394IL0070000100000004258'.
           05  FILLER  PIC X(25)  VALUE '6409IA0015000000000004008'.
           05  FILLER  PIC X(25)  VALUE '6434SD0015000100000097197'.
           05  FILLER  PIC X(25)  VALUE '6631NY0035000000000015167'.
           05  FILLER  PIC X(25)  VALUE '6703KY0007001000000011026'.
           05  FILLER  PIC X(25)  VALUE '6873PA0015000100000004156'.
           05  FILLER  PIC X(25)  VALUE '6927KS0015001000000008343'.
           05  FILLER  PIC X(25)  VALUE '6931OH0015000100000008358'.
           05  FILLER  PIC X(25)  VALUE '6934MO0010000100000001218'.
           05  FILLER  PIC X(25)  VALUE '7063OR0010001000000011010'.
           05  FILLER  PIC X(25)  VALUE '7157WY0007001000000015300'.
           05  FILLER  PIC X(25)  VALUE '7158AZ0035000500000011009'.
           05  FILLER  PIC X(25)  VALUE '7227MD0007000500000008297'.
           05  FILLER  PIC X(25)  VALUE '7233MS0002500500000004070'.
           05  FILLER  PIC X(25)  VALUE '7270CA0007000500000099031'.
           05  FILLER  PIC X(25)  VALUE '7312AK0070000000000015206'.
           05  FILLER  PIC X(25)  VALUE '7501NV0015000500000001283'.
           05  FILLER  PIC X(25)  VALUE '7566IN0020001000000001338'.
           05  FILLER  PIC X(25)  VALUE '7738TN0020000000000011125'.
           05  FILLER  PIC X(25)  VALUE '7825ND0020000500000011301'.
           05  FILLER  PIC X(25)  VALUE '7852AZ0010001000000099056'.
           05  FILLER  PIC X(25)  VALUE '7871VA0010000000000096324'.
           05  FILLER  PIC X(25)  VALUE '7981HI0020000500000011340'.
           05  FILLER  PIC X(25)  VALUE '8042MI0010000500000015002'.
           05  FILLER  PIC X(25)  VALUE '8083CO0007000000000015047'.
           05  FILLER  PIC X(25)  VALUE '8158SD0070000500000008222'.
           05  FILLER  PIC X(25)  VALUE '8295NV0002500500000096053'.
           05  FILLER  PIC X(25)  VALUE '8368AR0070000500000008101'.
           05  FILLER  PIC X(25)  VALUE '8398NJ0070001000000097351'.
           05  FILLER  PIC X(25)  VALUE '8497WA0020000000000096020'.
           05  FILLER  PIC X(25)  VALUE '8851MA0035001000000015120'.
           05  FILLER  PIC X(25)  VALUE '8965IA0002500250000096224'.
           05  FILLER  PIC X(25)  VALUE '9166MN0010000500000096125'.
           05  FILLER  PIC X(25)  VALUE '9178NE0020000000000099143'.
           05  FILLER  PIC X(25)  VALUE '9232NM0020000000000004348'.
           05  FILLER  PIC X(25)  VALUE '9294CA0002501000000096262'.
           05  FILLER  PIC X(25)  VALUE '9313AL0002500500000015005'.
           05  FILLER  PIC X(25)  VALUE '9356MO0002500500000011283'.
           05  FILLER  PIC X(25)  VALUE '9415OR0015000000000015076'.
           05  FILLER  PIC X(25)  VALUE '9422KY0015000100000011139'.
           05  FILLER  PIC X(25)  VALUE '9484CT0035000000000008230'.
           05  FILLER  PIC X(25)  VALUE '9668LA0002500500000096338'.
           05  FILLER  PIC X(25)  VALUE '9752CO0015000500000011291'.
           05  FILLER  PIC X(25)  VALUE '9811FL0002500500000015100'.
           05  FILLER  PIC X(25)  VALUE '9869UT0020000100000008264'.
           05  FILLER  PIC X(25)  VALUE '9923SC0035000500000011159'.
       01  WS-AGREE-TABLE REDEFINES WS-AGREE-CONST.
           05  WS-WS-AG-ENTRY OCCURS 96 TIMES
                   INDEXED BY WS-AG-IX.
               10  WS-AG-OCN               PIC X(04).
               10  WS-AG-STATE             PIC X(02).
               10  WS-AG-RATE              PIC 9(00)V9(05).
               10  WS-AG-CAP               PIC 9(09).
               10  WS-AG-EFF               PIC 9(05).

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

      * TARIFF DEFAULT FACTOR TABLE BY OCN.  USED ONLY WHEN THE
      * CARRIER HAS SUPPLIED NOTHING AND THE CARRIER MASTER DEFAULT
      * IS ALSO ZERO.  THE VALUES ARE THE 1998 TARIFF DEFAULTS AND
      * HAVE NEVER BEEN REFRESHED.  SEE OPEN ITEM CABS-1998-0044.
       01  WS-OCNDEF-CONST.
           05  FILLER  PIC X(21)  VALUE '1052R0330000003500000'.
           05  FILLER  PIC X(21)  VALUE '1524R0880000002000000'.
           05  FILLER  PIC X(21)  VALUE '1612W0150000003500000'.
           05  FILLER  PIC X(21)  VALUE '1762L0330000000000000'.
           05  FILLER  PIC X(21)  VALUE '1827W0250000000000000'.
           05  FILLER  PIC X(21)  VALUE '1876C0330000003500000'.
           05  FILLER  PIC X(21)  VALUE '1884C1000000000500000'.
           05  FILLER  PIC X(21)  VALUE '2348C0150000003500000'.
           05  FILLER  PIC X(21)  VALUE '2412W0150000000000000'.
           05  FILLER  PIC X(21)  VALUE '2621L0330000000500000'.
           05  FILLER  PIC X(21)  VALUE '2677R0880000001200000'.
           05  FILLER  PIC X(21)  VALUE '2851I0650000000000000'.
           05  FILLER  PIC X(21)  VALUE '2866L0250000000000000'.
           05  FILLER  PIC X(21)  VALUE '3161L0500000001200000'.
           05  FILLER  PIC X(21)  VALUE '3781L0150000000000000'.
           05  FILLER  PIC X(21)  VALUE '4332I0650000000500000'.
           05  FILLER  PIC X(21)  VALUE '4399C0150000000000000'.
           05  FILLER  PIC X(21)  VALUE '4596I0650000000000000'.
           05  FILLER  PIC X(21)  VALUE '4780L0750000004800000'.
           05  FILLER  PIC X(21)  VALUE '4856W0500000001200000'.
           05  FILLER  PIC X(21)  VALUE '4868R0330000003500000'.
           05  FILLER  PIC X(21)  VALUE '5074R0650000000000000'.
           05  FILLER  PIC X(21)  VALUE '5722I0750000004800000'.
           05  FILLER  PIC X(21)  VALUE '6001L0500000003500000'.
           05  FILLER  PIC X(21)  VALUE '6247C0650000001200000'.
           05  FILLER  PIC X(21)  VALUE '6283W0880000000500000'.
           05  FILLER  PIC X(21)  VALUE '6306I1000000000000000'.
           05  FILLER  PIC X(21)  VALUE '6576I0000000004800000'.
           05  FILLER  PIC X(21)  VALUE '6593W0250000003500000'.
           05  FILLER  PIC X(21)  VALUE '6851C0650000001200000'.
           05  FILLER  PIC X(21)  VALUE '7026W0250000000000000'.
           05  FILLER  PIC X(21)  VALUE '7195L0250000003500000'.
           05  FILLER  PIC X(21)  VALUE '7728R0880000000000000'.
           05  FILLER  PIC X(21)  VALUE '7969C0000000003500000'.
           05  FILLER  PIC X(21)  VALUE '8344W0880000003500000'.
           05  FILLER  PIC X(21)  VALUE '8400I0330000000500000'.
           05  FILLER  PIC X(21)  VALUE '8481I0000000000000000'.
           05  FILLER  PIC X(21)  VALUE '8626R0750000002000000'.
           05  FILLER  PIC X(21)  VALUE '8765I0150000001200000'.
           05  FILLER  PIC X(21)  VALUE '8832C0750000002000000'.
           05  FILLER  PIC X(21)  VALUE '8854W0150000004800000'.
           05  FILLER  PIC X(21)  VALUE '8901L0330000000000000'.
           05  FILLER  PIC X(21)  VALUE '9326R0500000004800000'.
           05  FILLER  PIC X(21)  VALUE '9538I0150000002000000'.
           05  FILLER  PIC X(21)  VALUE '9539C0750000000000000'.
           05  FILLER  PIC X(21)  VALUE '9822C1000000003500000'.
           05  FILLER  PIC X(21)  VALUE '9842R0150000001200000'.
           05  FILLER  PIC X(21)  VALUE '9986L0150000000500000'.
       01  WS-OCNDEF-TABLE REDEFINES WS-OCNDEF-CONST.
           05  WS-WS-OD-ENTRY OCCURS 48 TIMES
                   INDEXED BY WS-OD-IX.
               10  WS-OD-OCN               PIC X(04).
               10  WS-OD-TYPE              PIC X(01).
               10  WS-OD-PIU               PIC 9(03)V9(05).
               10  WS-OD-PLU               PIC 9(03)V9(05).

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
                   VALUE 'RECIPROCAL COMPENSATION SETTLEMENT REGISTER'.
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
           05  FILLER              PIC X(42)
                   VALUE 'OCN  ST TOTAL-MOU        ISP-MOU          '.
           05  FILLER              PIC X(46)
                 VALUE 'CAP           BILL-MOU        RATE     NET-DUE'.
       01  WS-HEAD-4.
           05  FILLER                  PIC X(01)             VALUE ' '.
           05  FILLER                PIC X(131)          VALUE ALL '-'.

      * DETAIL LINE WS-DETAIL-1.
       01  WS-DETAIL-1.
           05  FILLER                  PIC X(01)             VALUE ' '.
           05  WS-D1-OCN               PIC X(04).
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-STATE             PIC X(02).
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-TOTMOU            PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-ISPMOU            PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-CAP               PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-BILLMOU           PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-RATE              PIC Z.ZZZZ9.
           05  FILLER                PIC X(01)           VALUE SPACES.
           05  WS-D1-NET               PIC ZZZ,ZZZ,ZZ9.99-.

      * DETAIL LINE WS-TOTAL-1.
       01  WS-TOTAL-1.
           05  FILLER                  PIC X(01)             VALUE ' '.
           05  FILLER                PIC X(20)           VALUE SPACES.
           05  WS-T1-DESC              PIC X(30).
           05  WS-T1-COUNT             PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                PIC X(02)           VALUE SPACES.
           05  WS-T1-AMOUNT            PIC Z,ZZZ,ZZZ,ZZ9.99999-.

      * MESSAGE TEXT TABLE.  ADDRESSED BY ERROR CODE SEQUENCE, NOT
      * BY THE CODE ITSELF - INSERTING A CODE IN THE MIDDLE SHIFTS
      * EVERY MESSAGE.  THIS HAS HAPPENED TWICE (1994 AND 2007).
       01  WS-MSG-CONST.
           05  FILLER              PIC X(44)
                   VALUE 'SETTLED AT THE AGREEMENT RATE               '.
           05  FILLER              PIC X(44)
                   VALUE 'ISP BOUND MINUTES ABOVE THE CAP             '.
           05  FILLER              PIC X(44)
                   VALUE 'NO INTERCONNECTION AGREEMENT ON FILE        '.
           05  FILLER              PIC X(44)
                   VALUE 'RATE TAKEN FROM THE CARRIER MASTER          '.
           05  FILLER              PIC X(44)
                   VALUE 'RATE TAKEN FROM THE CONTROL CARD            '.
           05  FILLER              PIC X(44)
                   VALUE 'ZERO RATE - NO COMPENSATION DUE             '.
           05  FILLER              PIC X(44)
                   VALUE 'CARRIER NOT ELIGIBLE FOR RECIP COMP         '.
           05  FILLER              PIC X(44)
                   VALUE 'CAP OVERRIDDEN FROM THE CONTROL CARD        '.
           05  FILLER              PIC X(44)
                   VALUE 'CAPPED MINUTE DETAIL WRITTEN                '.
           05  FILLER              PIC X(44)
                   VALUE 'NET DUE ROUNDED TO TWO PLACES               '.
           05  FILLER              PIC X(44)
                   VALUE 'NO BILLABLE MINUTES THIS PERIOD             '.
           05  FILLER              PIC X(44)
                   VALUE 'SIMULATION MODE - NOTHING WRITTEN           '.
           05  FILLER              PIC X(44)
                   VALUE 'AGREEMENT EFFECTIVE AFTER THE PERIOD        '.
           05  FILLER              PIC X(44)
                   VALUE 'WIRELESS CARRIER - SEPARATE PATH            '.
           05  FILLER              PIC X(44)
                   VALUE 'USAGE DETAIL NOT MATCHED                    '.
           05  FILLER              PIC X(44)
                   VALUE 'SETTLEMENT RECORD WRITTEN                   '.
           05  FILLER              PIC X(44)
                   VALUE 'CAP IS ZERO - NO CAP APPLIED                '.
           05  FILLER              PIC X(44)
                   VALUE 'TOTAL MOU DOES NOT RECONCILE                '.
           05  FILLER              PIC X(44)
                   VALUE 'GROWTH CAP REMOVED IN 2004                  '.
           05  FILLER              PIC X(44)
                   VALUE 'END OF RECIPROCAL COMPENSATION RUN          '.
       01  WS-MSG-TABLE REDEFINES WS-MSG-CONST.
           05  WS-MSG-ENTRY OCCURS 20 TIMES
                   INDEXED BY WS-MSG-IX.
               10  WS-MSG-TEXT             PIC X(44).

      * PRIOR PERIOD CAPPED MINUTE HISTORY.  THE 2004 GROWTH CAP
      * COMPARED THIS PERIODS MINUTES WITH THE SAME PERIOD A YEAR
      * EARLIER AND CAPPED THE GROWTH.  THE GROWTH CAP WAS REMOVED
      * IN 2004 BUT THE HISTORY TABLE IS STILL LOADED AND STILL
      * SEARCHED BY P5000 BELOW.
       01  WS-PRIORCP-CONST.
           05  FILLER  PIC X(21)  VALUE '200120159107673791264'.
           05  FILLER  PIC X(21)  VALUE '200120721855697833109'.
           05  FILLER  PIC X(21)  VALUE '200125115426053634703'.
           05  FILLER  PIC X(21)  VALUE '200127328177451199431'.
           05  FILLER  PIC X(21)  VALUE '200146742004698035601'.
           05  FILLER  PIC X(21)  VALUE '200155152193255831411'.
           05  FILLER  PIC X(21)  VALUE '200197915215615878219'.
           05  FILLER  PIC X(21)  VALUE '200225969017746108429'.
           05  FILLER  PIC X(21)  VALUE '200238877753959174343'.
           05  FILLER  PIC X(21)  VALUE '200239383213933711076'.
           05  FILLER  PIC X(21)  VALUE '200243172490428949168'.
           05  FILLER  PIC X(21)  VALUE '200249765320689143114'.
           05  FILLER  PIC X(21)  VALUE '200258896699714526667'.
           05  FILLER  PIC X(21)  VALUE '200261253697697097895'.
           05  FILLER  PIC X(21)  VALUE '200311177844969762614'.
           05  FILLER  PIC X(21)  VALUE '200324319864357253211'.
           05  FILLER  PIC X(21)  VALUE '200328325260221957483'.
           05  FILLER  PIC X(21)  VALUE '200331227129694638391'.
           05  FILLER  PIC X(21)  VALUE '200346235821801395585'.
           05  FILLER  PIC X(21)  VALUE '200346303278984898730'.
           05  FILLER  PIC X(21)  VALUE '200375613002818308917'.
           05  FILLER  PIC X(21)  VALUE '200383898182171187807'.
           05  FILLER  PIC X(21)  VALUE '200384202167534859459'.
           05  FILLER  PIC X(21)  VALUE '200390231801712586002'.
           05  FILLER  PIC X(21)  VALUE '200392955298437190795'.
           05  FILLER  PIC X(21)  VALUE '200425883027505102366'.
           05  FILLER  PIC X(21)  VALUE '200429135057148442208'.
           05  FILLER  PIC X(21)  VALUE '200453741108968935633'.
           05  FILLER  PIC X(21)  VALUE '200485617836912530062'.
           05  FILLER  PIC X(21)  VALUE '200493276144310577167'.
           05  FILLER  PIC X(21)  VALUE '200498485338590081784'.
           05  FILLER  PIC X(21)  VALUE '200513901669701142496'.
           05  FILLER  PIC X(21)  VALUE '200527352194428600310'.
           05  FILLER  PIC X(21)  VALUE '200537704240019156611'.
           05  FILLER  PIC X(21)  VALUE '200542873659524122822'.
           05  FILLER  PIC X(21)  VALUE '200573805235435758965'.
           05  FILLER  PIC X(21)  VALUE '200579866508077711298'.
           05  FILLER  PIC X(21)  VALUE '200581950552161447768'.
           05  FILLER  PIC X(21)  VALUE '200602672559575554811'.
           05  FILLER  PIC X(21)  VALUE '200605264674531076239'.
           05  FILLER  PIC X(21)  VALUE '200606788830306724130'.
           05  FILLER  PIC X(21)  VALUE '200607596306850418431'.
           05  FILLER  PIC X(21)  VALUE '200616987848450825525'.
           05  FILLER  PIC X(21)  VALUE '200677895916210168179'.
           05  FILLER  PIC X(21)  VALUE '200687254516859819835'.
           05  FILLER  PIC X(21)  VALUE '200723489371575191499'.
           05  FILLER  PIC X(21)  VALUE '200724614293703844621'.
           05  FILLER  PIC X(21)  VALUE '200733500040424406748'.
           05  FILLER  PIC X(21)  VALUE '200749509697475016952'.
           05  FILLER  PIC X(21)  VALUE '200763620038506108039'.
           05  FILLER  PIC X(21)  VALUE '200792357132359602613'.
           05  FILLER  PIC X(21)  VALUE '200796302309676236895'.
           05  FILLER  PIC X(21)  VALUE '200801243833087642424'.
           05  FILLER  PIC X(21)  VALUE '200803207609815650223'.
           05  FILLER  PIC X(21)  VALUE '200804216443832552090'.
           05  FILLER  PIC X(21)  VALUE '200810468598734450112'.
           05  FILLER  PIC X(21)  VALUE '200839604150719497789'.
           05  FILLER  PIC X(21)  VALUE '200842867371085934715'.
           05  FILLER  PIC X(21)  VALUE '200863711775318641295'.
           05  FILLER  PIC X(21)  VALUE '200865185693989348253'.
           05  FILLER  PIC X(21)  VALUE '200951350819981371464'.
           05  FILLER  PIC X(21)  VALUE '201006362469704514275'.
           05  FILLER  PIC X(21)  VALUE '201028487789605227669'.
           05  FILLER  PIC X(21)  VALUE '201037789140295685021'.
           05  FILLER  PIC X(21)  VALUE '201043898446498404007'.
           05  FILLER  PIC X(21)  VALUE '201047404378314739576'.
           05  FILLER  PIC X(21)  VALUE '201074441248461271174'.
           05  FILLER  PIC X(21)  VALUE '201078932008882830885'.
           05  FILLER  PIC X(21)  VALUE '201083625025207577608'.
           05  FILLER  PIC X(21)  VALUE '201099730190893351248'.
           05  FILLER  PIC X(21)  VALUE '201101971391455743113'.
           05  FILLER  PIC X(21)  VALUE '201125556631336425461'.
           05  FILLER  PIC X(21)  VALUE '201135127723754343996'.
           05  FILLER  PIC X(21)  VALUE '201136260352611843997'.
           05  FILLER  PIC X(21)  VALUE '201145561991031715946'.
           05  FILLER  PIC X(21)  VALUE '201151220240636007401'.
           05  FILLER  PIC X(21)  VALUE '201185886302694025768'.
           05  FILLER  PIC X(21)  VALUE '201196928361971918934'.
           05  FILLER  PIC X(21)  VALUE '201198281527535672486'.
           05  FILLER  PIC X(21)  VALUE '201207373700001502311'.
       01  WS-PRIORCP-TABLE REDEFINES WS-PRIORCP-CONST.
           05  WS-WS-PC-ENTRY OCCURS 80 TIMES
                   INDEXED BY WS-PC-IX.
               10  WS-PC-PERIOD            PIC 9(06).
               10  WS-PC-OCN               PIC X(04).
               10  WS-PC-MOU               PIC 9(11).

      * GROWTH CAP WORK AREA.  THE FCC ORDER OF 2001 IMPOSED A
      * GROWTH CEILING ON ISP BOUND MINUTES AND THE ORDER WAS
      * SUPERSEDED IN 2004.  THE FIELDS ARE STILL POPULATED.
       01  WS-GROWTH-WORK.
           05  WS-GW-PRIOR-MOU         PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-GW-GROWTH-MOU        PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-GW-GROWTH-PCT        PIC S9(05)V9(05) COMP-3 VALUE 0.
           05  WS-GW-ALLOWED-MOU       PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-GW-EXCESS-MOU        PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-GW-COUNT             PIC S9(09) COMP-3     VALUE 0.

      * ISP BOUND NPA NXX IDENTIFICATION TABLE.  A TERMINATING
      * NUMBER ON THIS TABLE IS TREATED AS INTERNET BOUND AND IS
      * SUBJECT TO THE CAP.  THE TABLE WAS BUILT IN 1997 FROM A
      * TRAFFIC STUDY AND IS MAINTAINED BY HAND.  EVERY CLEC
      * DISPUTES IT AT LEAST ONCE A YEAR AND THE DISPUTE IS ALWAYS
      * SETTLED BY NEGOTIATION RATHER THAN BY CHANGING THE TABLE.
       01  WS-ISPNXX-CONST.
           05  FILLER                PIC X(07)          VALUE '204799D'.
           05  FILLER                PIC X(07)          VALUE '208476D'.
           05  FILLER                PIC X(07)          VALUE '211842D'.
           05  FILLER                PIC X(07)          VALUE '212858D'.
           05  FILLER                PIC X(07)          VALUE '213521D'.
           05  FILLER                PIC X(07)          VALUE '224211I'.
           05  FILLER                PIC X(07)          VALUE '226302I'.
           05  FILLER                PIC X(07)          VALUE '233212V'.
           05  FILLER                PIC X(07)          VALUE '242636D'.
           05  FILLER                PIC X(07)          VALUE '251355D'.
           05  FILLER                PIC X(07)          VALUE '254670V'.
           05  FILLER                PIC X(07)          VALUE '254898V'.
           05  FILLER                PIC X(07)          VALUE '257609V'.
           05  FILLER                PIC X(07)          VALUE '259894V'.
           05  FILLER                PIC X(07)          VALUE '260726D'.
           05  FILLER                PIC X(07)          VALUE '262674D'.
           05  FILLER                PIC X(07)          VALUE '274456I'.
           05  FILLER                PIC X(07)          VALUE '278338I'.
           05  FILLER                PIC X(07)          VALUE '284445V'.
           05  FILLER                PIC X(07)          VALUE '291448D'.
           05  FILLER                PIC X(07)          VALUE '292587D'.
           05  FILLER                PIC X(07)          VALUE '293926D'.
           05  FILLER                PIC X(07)          VALUE '301483D'.
           05  FILLER                PIC X(07)          VALUE '303403D'.
           05  FILLER                PIC X(07)          VALUE '306355V'.
           05  FILLER                PIC X(07)          VALUE '313304D'.
           05  FILLER                PIC X(07)          VALUE '316250I'.
           05  FILLER                PIC X(07)          VALUE '320327I'.
           05  FILLER                PIC X(07)          VALUE '322511I'.
           05  FILLER                PIC X(07)          VALUE '328245D'.
           05  FILLER                PIC X(07)          VALUE '328713V'.
           05  FILLER                PIC X(07)          VALUE '332770I'.
           05  FILLER                PIC X(07)          VALUE '334931V'.
           05  FILLER                PIC X(07)          VALUE '336516I'.
           05  FILLER                PIC X(07)          VALUE '338714I'.
           05  FILLER                PIC X(07)          VALUE '338986D'.
           05  FILLER                PIC X(07)          VALUE '341945I'.
           05  FILLER                PIC X(07)          VALUE '343978I'.
           05  FILLER                PIC X(07)          VALUE '351257V'.
           05  FILLER                PIC X(07)          VALUE '356264V'.
           05  FILLER                PIC X(07)          VALUE '366418D'.
           05  FILLER                PIC X(07)          VALUE '374815D'.
           05  FILLER                PIC X(07)          VALUE '383340I'.
           05  FILLER                PIC X(07)          VALUE '385991I'.
           05  FILLER                PIC X(07)          VALUE '387491D'.
           05  FILLER                PIC X(07)          VALUE '391781I'.
           05  FILLER                PIC X(07)          VALUE '391942V'.
           05  FILLER                PIC X(07)          VALUE '392290I'.
           05  FILLER                PIC X(07)          VALUE '393385I'.
           05  FILLER                PIC X(07)          VALUE '394807I'.
           05  FILLER                PIC X(07)          VALUE '408943V'.
           05  FILLER                PIC X(07)          VALUE '410945I'.
           05  FILLER                PIC X(07)          VALUE '413288I'.
           05  FILLER                PIC X(07)          VALUE '414584V'.
           05  FILLER                PIC X(07)          VALUE '418657V'.
           05  FILLER                PIC X(07)          VALUE '418711V'.
           05  FILLER                PIC X(07)          VALUE '421771V'.
           05  FILLER                PIC X(07)          VALUE '427380I'.
           05  FILLER                PIC X(07)          VALUE '428687I'.
           05  FILLER                PIC X(07)          VALUE '433348I'.
           05  FILLER                PIC X(07)          VALUE '439863D'.
           05  FILLER                PIC X(07)          VALUE '445504I'.
           05  FILLER                PIC X(07)          VALUE '448434D'.
           05  FILLER                PIC X(07)          VALUE '448541I'.
           05  FILLER                PIC X(07)          VALUE '463807V'.
           05  FILLER                PIC X(07)          VALUE '466397I'.
           05  FILLER                PIC X(07)          VALUE '467537V'.
           05  FILLER                PIC X(07)          VALUE '469640I'.
           05  FILLER                PIC X(07)          VALUE '470375V'.
           05  FILLER                PIC X(07)          VALUE '472258D'.
           05  FILLER                PIC X(07)          VALUE '472678I'.
           05  FILLER                PIC X(07)          VALUE '473280V'.
           05  FILLER                PIC X(07)          VALUE '473544D'.
           05  FILLER                PIC X(07)          VALUE '477638D'.
           05  FILLER                PIC X(07)          VALUE '478919V'.
           05  FILLER                PIC X(07)          VALUE '485691I'.
           05  FILLER                PIC X(07)          VALUE '486371I'.
           05  FILLER                PIC X(07)          VALUE '489902V'.
           05  FILLER                PIC X(07)          VALUE '497609I'.
           05  FILLER                PIC X(07)          VALUE '513223I'.
           05  FILLER                PIC X(07)          VALUE '514850V'.
           05  FILLER                PIC X(07)          VALUE '514855I'.
           05  FILLER                PIC X(07)          VALUE '514864I'.
           05  FILLER                PIC X(07)          VALUE '520454V'.
           05  FILLER                PIC X(07)          VALUE '523977I'.
           05  FILLER                PIC X(07)          VALUE '524423D'.
           05  FILLER                PIC X(07)          VALUE '528826V'.
           05  FILLER                PIC X(07)          VALUE '530405D'.
           05  FILLER                PIC X(07)          VALUE '534494D'.
           05  FILLER                PIC X(07)          VALUE '535454V'.
           05  FILLER                PIC X(07)          VALUE '539698D'.
           05  FILLER                PIC X(07)          VALUE '541774I'.
           05  FILLER                PIC X(07)          VALUE '542336I'.
           05  FILLER                PIC X(07)          VALUE '546701I'.
           05  FILLER                PIC X(07)          VALUE '547380D'.
           05  FILLER                PIC X(07)          VALUE '553334I'.
           05  FILLER                PIC X(07)          VALUE '555338I'.
           05  FILLER                PIC X(07)          VALUE '556575V'.
           05  FILLER                PIC X(07)          VALUE '565837V'.
           05  FILLER                PIC X(07)          VALUE '574458D'.
           05  FILLER                PIC X(07)          VALUE '576361I'.
           05  FILLER                PIC X(07)          VALUE '576432D'.
           05  FILLER                PIC X(07)          VALUE '576518V'.
           05  FILLER                PIC X(07)          VALUE '588649I'.
           05  FILLER                PIC X(07)          VALUE '591709V'.
           05  FILLER                PIC X(07)          VALUE '598939I'.
           05  FILLER                PIC X(07)          VALUE '600677D'.
           05  FILLER                PIC X(07)          VALUE '602250V'.
           05  FILLER                PIC X(07)          VALUE '604518I'.
           05  FILLER                PIC X(07)          VALUE '609505D'.
           05  FILLER                PIC X(07)          VALUE '611510I'.
           05  FILLER                PIC X(07)          VALUE '612356D'.
           05  FILLER                PIC X(07)          VALUE '618426V'.
           05  FILLER                PIC X(07)          VALUE '618702D'.
           05  FILLER                PIC X(07)          VALUE '622414D'.
           05  FILLER                PIC X(07)          VALUE '625255D'.
           05  FILLER                PIC X(07)          VALUE '633544D'.
           05  FILLER                PIC X(07)          VALUE '634480I'.
           05  FILLER                PIC X(07)          VALUE '637851V'.
           05  FILLER                PIC X(07)          VALUE '643710D'.
           05  FILLER                PIC X(07)          VALUE '652954I'.
           05  FILLER                PIC X(07)          VALUE '663953I'.
           05  FILLER                PIC X(07)          VALUE '666316V'.
           05  FILLER                PIC X(07)          VALUE '671588D'.
           05  FILLER                PIC X(07)          VALUE '682772I'.
           05  FILLER                PIC X(07)          VALUE '692241I'.
           05  FILLER                PIC X(07)          VALUE '692886V'.
           05  FILLER                PIC X(07)          VALUE '696238D'.
           05  FILLER                PIC X(07)          VALUE '700901D'.
           05  FILLER                PIC X(07)          VALUE '701227D'.
           05  FILLER                PIC X(07)          VALUE '702242I'.
           05  FILLER                PIC X(07)          VALUE '711758D'.
           05  FILLER                PIC X(07)          VALUE '711935I'.
           05  FILLER                PIC X(07)          VALUE '716806V'.
           05  FILLER                PIC X(07)          VALUE '717261V'.
           05  FILLER                PIC X(07)          VALUE '719484V'.
           05  FILLER                PIC X(07)          VALUE '719516V'.
           05  FILLER                PIC X(07)          VALUE '722353D'.
           05  FILLER                PIC X(07)          VALUE '723780D'.
           05  FILLER                PIC X(07)          VALUE '728952V'.
           05  FILLER                PIC X(07)          VALUE '735245I'.
           05  FILLER                PIC X(07)          VALUE '744618D'.
           05  FILLER                PIC X(07)          VALUE '751691D'.
           05  FILLER                PIC X(07)          VALUE '755481V'.
           05  FILLER                PIC X(07)          VALUE '760698I'.
           05  FILLER                PIC X(07)          VALUE '760791V'.
           05  FILLER                PIC X(07)          VALUE '761666D'.
           05  FILLER                PIC X(07)          VALUE '766211D'.
           05  FILLER                PIC X(07)          VALUE '768766V'.
           05  FILLER                PIC X(07)          VALUE '769771V'.
           05  FILLER                PIC X(07)          VALUE '770334D'.
           05  FILLER                PIC X(07)          VALUE '771794I'.
           05  FILLER                PIC X(07)          VALUE '780815D'.
           05  FILLER                PIC X(07)          VALUE '782676V'.
           05  FILLER                PIC X(07)          VALUE '783629I'.
           05  FILLER                PIC X(07)          VALUE '787392I'.
           05  FILLER                PIC X(07)          VALUE '790425V'.
           05  FILLER                PIC X(07)          VALUE '801358V'.
           05  FILLER                PIC X(07)          VALUE '802295V'.
           05  FILLER                PIC X(07)          VALUE '804446D'.
           05  FILLER                PIC X(07)          VALUE '810612I'.
           05  FILLER                PIC X(07)          VALUE '811561V'.
           05  FILLER                PIC X(07)          VALUE '816232V'.
           05  FILLER                PIC X(07)          VALUE '818563D'.
           05  FILLER                PIC X(07)          VALUE '824816V'.
           05  FILLER                PIC X(07)          VALUE '837644D'.
           05  FILLER                PIC X(07)          VALUE '838208V'.
           05  FILLER                PIC X(07)          VALUE '840929D'.
           05  FILLER                PIC X(07)          VALUE '848737V'.
           05  FILLER                PIC X(07)          VALUE '852461V'.
           05  FILLER                PIC X(07)          VALUE '856906I'.
           05  FILLER                PIC X(07)          VALUE '860398D'.
           05  FILLER                PIC X(07)          VALUE '863867V'.
           05  FILLER                PIC X(07)          VALUE '874283D'.
           05  FILLER                PIC X(07)          VALUE '882273V'.
           05  FILLER                PIC X(07)          VALUE '884999V'.
           05  FILLER                PIC X(07)          VALUE '886267D'.
           05  FILLER                PIC X(07)          VALUE '897421D'.
           05  FILLER                PIC X(07)          VALUE '899985I'.
           05  FILLER                PIC X(07)          VALUE '910455V'.
           05  FILLER                PIC X(07)          VALUE '913998D'.
           05  FILLER                PIC X(07)          VALUE '916405D'.
           05  FILLER                PIC X(07)          VALUE '921872V'.
           05  FILLER                PIC X(07)          VALUE '923779I'.
           05  FILLER                PIC X(07)          VALUE '929470V'.
           05  FILLER                PIC X(07)          VALUE '932378V'.
           05  FILLER                PIC X(07)          VALUE '942601I'.
           05  FILLER                PIC X(07)          VALUE '944325V'.
           05  FILLER                PIC X(07)          VALUE '945896V'.
           05  FILLER                PIC X(07)          VALUE '946428D'.
           05  FILLER                PIC X(07)          VALUE '949369I'.
           05  FILLER                PIC X(07)          VALUE '961356V'.
           05  FILLER                PIC X(07)          VALUE '962425V'.
           05  FILLER                PIC X(07)          VALUE '968930I'.
           05  FILLER                PIC X(07)          VALUE '969436I'.
           05  FILLER                PIC X(07)          VALUE '972245I'.
           05  FILLER                PIC X(07)          VALUE '973869D'.
           05  FILLER                PIC X(07)          VALUE '974650D'.
           05  FILLER                PIC X(07)          VALUE '981330I'.
           05  FILLER                PIC X(07)          VALUE '982835V'.
       01  WS-ISPNXX-TABLE REDEFINES WS-ISPNXX-CONST.
           05  WS-WS-IN-ENTRY OCCURS 200 TIMES
                   INDEXED BY WS-IN-IX.
               10  WS-IN-NPA               PIC 9(03).
               10  WS-IN-NXX               PIC 9(03).
               10  WS-IN-CLASS             PIC X(01).

      * TRUE UP AND CLASSIFICATION WORK AREA.  THE ISP PERCENTAGE
      * IS THE PROPORTION OF TERMINATING MINUTES THAT WENT TO AN
      * INTERNET SERVICE PROVIDER.  A LARGE MONTH ON MONTH MOVEMENT
      * USUALLY MEANS THE CLEC HAS MOVED TRAFFIC ONTO A DIFFERENT
      * TRUNK GROUP RATHER THAN THAT ANYTHING REAL HAS CHANGED.
       01  WS-TRUEUP-WORK.
           05  WS-TU-PRIOR-NET         PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-TU-CURRENT-NET       PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-TU-DELTA             PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-TU-PCT               PIC S9(05)V9(05) COMP-3 VALUE 0.
           05  WS-TU-COUNT             PIC S9(09) COMP-3     VALUE 0.
           05  WS-TU-SWING-CNT         PIC S9(09) COMP-3     VALUE 0.
           05  WS-TU-ISP-PCT           PIC S9(05)V9(05) COMP-3 VALUE 0.
           05  WS-TU-CLASSIFIED        PIC S9(11) COMP-3     VALUE 0.

      * CLASSIFICATION WORK FIELDS.
       01  WS-NPANXX-WORK.
           05  WS-NW-NPA               PIC 9(03)             VALUE 0.
           05  WS-NW-NXX               PIC 9(03)             VALUE 0.
       01  WS-NPANXX-ALT REDEFINES WS-NPANXX-WORK.
           05  WS-NA-SIX               PIC 9(06).
       01  WS-CLASS-AREA.
           05  WS-CLASS-HOLD           PIC X(01)             VALUE 'V'.

      * ***************************************************
      * * RECIPROCAL COMPENSATION - RULES OF THE ROAD     *
      * ***************************************************
      * 1. A CLEC THAT TERMINATES A CALL ORIGINATED ON OUR
      *    NETWORK IS ENTITLED TO COMPENSATION FOR THE
      *    TERMINATING FUNCTION.  THE RATE IS NEGOTIATED IN
      *    THE INTERCONNECTION AGREEMENT AND IS NOT TARIFFED.
      * 2. TRAFFIC DELIVERED TO AN INTERNET SERVICE PROVIDER
      *    IS NOT LOCAL EXCHANGE TRAFFIC IN THE ORDINARY
      *    SENSE.  IT IS ONE WAY, IT LASTS FOR HOURS AND IT
      *    NEVER COMES BACK.  A CLEC THAT SERVES ONLY ISPS
      *    RECEIVES COMPENSATION AND PAYS NONE.
      * 3. THE CAP EXISTS BECAUSE OF POINT 2.  MINUTES ABOVE
      *    THE CAP ARE COMPENSATED AT ZERO.  THE CAP IS PER
      *    CARRIER, PER STATE AND PER MONTH.
      * 4. THE CAP MUST BE APPLIED TO THE MINUTES BEFORE THE
      *    RATE IS APPLIED TO THEM.  APPLYING THE RATE FIRST
      *    AND THE CAP AFTERWARDS LEAVES THE CAPPED MINUTES
      *    INSIDE THE AMOUNT AND PAYS FOR TRAFFIC THAT THE
      *    AGREEMENT SAYS IS NOT COMPENSABLE.
      * 5. THE CAPPED MINUTE DETAIL FILE IS EVIDENCE.  IT IS
      *    SENT TO THE CLEC AND IT IS THE FIRST THING THEIR
      *    REGULATORY TEAM ASKS FOR IN A DISPUTE.
      * 6. NOTHING IN THIS PROGRAM RECLASSIFIES TRAFFIC.  THE
      *    ISP BOUND SPLIT ARRIVES FROM CABSET04 AND IS TAKEN
      *    ON TRUST.  IF THE SPLIT IS WRONG THE SETTLEMENT IS
      *    WRONG AND NOTHING HERE WILL DETECT IT.
      * ***************************************************

      * JULIAN DATE WORK AREA - LOCAL TO CABSET05.  THE SHARED AREA IN
      * CABSDATE IS USED FOR THE CYCLE DATE ONLY.  THIS ONE CARRIES
      * THE WINDOW ENDPOINTS.
       01  WS-JULIAN-WORK.
           05  WS-JW-FROM.
               10  WS-JW-FROM-YY           PIC 9(02)            VALUE 0.
               10  WS-JW-FROM-DDD          PIC 9(03)            VALUE 0.
           05  WS-JW-THRU.
               10  WS-JW-THRU-YY           PIC 9(02)            VALUE 0.
               10  WS-JW-THRU-DDD          PIC 9(03)            VALUE 0.
           05  WS-JW-TEST.
               10  WS-JW-TEST-YY           PIC 9(02)            VALUE 0.
               10  WS-JW-TEST-DDD          PIC 9(03)            VALUE 0.
           05  WS-JW-HOLD.
               10  WS-JW-HOLD-YY           PIC 9(02)            VALUE 0.
               10  WS-JW-HOLD-DDD          PIC 9(03)            VALUE 0.
           05  WS-JW-CCYY              PIC 9(04)             VALUE 0.
           05  WS-JW-DAYS-IN-YR        PIC 9(03)             VALUE 365.
           05  WS-JW-ABS-FROM          PIC S9(07) COMP-3     VALUE 0.
           05  WS-JW-ABS-THRU          PIC S9(07) COMP-3     VALUE 0.
           05  WS-JW-ABS-TEST          PIC S9(07) COMP-3     VALUE 0.
           05  WS-JW-SPAN-DAYS         PIC S9(07) COMP-3     VALUE 0.
           05  WS-JW-REM               PIC S9(05) COMP-3     VALUE 0.
           05  WS-JW-LEAP-SW           PIC X(01)             VALUE 'N'.
                   88  WS-JW-LEAP              VALUE 'Y'.

      * SEQUENCE NUMBER AND SPARE WORK FIELDS.
       01  WS-RECIP-MISC.
           05  WS-SEQ-NBR              PIC 9(09) COMP-3      VALUE 0.
           05  WS-AGREE-EFF            PIC 9(05)             VALUE 0.
           05  WS-AGREE-CCYY           PIC 9(04)             VALUE 0.

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
      * THE SETTLEMENT PERIOD AND THE CAP OVERRIDE ARRIVE AS
      * SYMBOLICS SUBSTITUTED AT SUBMISSION TIME.  NEITHER HAS A
      * DEFAULT.  A CAP OVERRIDE OF ZERO MEANS USE THE AGREEMENT
      * CAP - IT DOES NOT MEAN A CAP OF ZERO MINUTES, AND THAT
      * DISTINCTION HAS CAUGHT THE OPERATIONS TEAM TWICE.
           MOVE 'P1000-INIT' TO WS-PARA-NAME.
           ACCEPT WS-ACCEPT-DATE FROM DATE.
           ACCEPT WS-ACCEPT-TIME FROM TIME.
           OPEN INPUT  RECIP-AGG-FILE
                       RECIP-CDR-FILE
                       CARRIER-MASTER
                       PARM-FILE
           OPEN OUTPUT SETTLE-OUT-FILE
                       CAPPED-OUT-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 6501 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-RECAGG' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 6502 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-RECIPCDR' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 6503 TO WS-AB-CODE
               MOVE 'OPEN FAILED DA-I-CARRMAST' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 6504 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-SETLOUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 6505 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-CAPOUT' TO WS-AB-TEXT
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
           MOVE ZERO TO WS-SETTLE-CNT WS-CAPPED-CNT WS-NOAGREE-CNT
                        WS-ZERORATE-CNT WS-TOT-MOU WS-TOT-BILL-MOU
                        WS-TOT-CAP-MOU WS-TOT-GROSS WS-TOT-NET
                        WS-TOT-CAP-AMT WS-TOT-ROUND-DIFF.
           PERFORM P6000-HEADING THRU P6000-EXIT.
           PERFORM P2600-READ-CDR THRU P2600-EXIT.
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
               MOVE 6510 TO WS-AB-CODE
               MOVE 'SETTLEMENT PERIOD NOT SUPPLIED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PE-DEF-RATE NOT NUMERIC
               MOVE ZERO TO WS-PE-DEF-RATE.
           IF WS-PE-CAP-OVERRIDE NOT NUMERIC
               MOVE ZERO TO WS-PE-CAP-OVERRIDE.
           IF WS-PE-SIM-SW NOT = 'Y'
               MOVE 'N' TO WS-PE-SIM-SW.

       P1200-EXIT.
           EXIT.


      *****************************************************************
      * S200-RECIP-DRIVER                                             *
      * READ THE AGGREGATED MINUTES AND SETTLE EACH CARRIER.          *
      *****************************************************************
       S200-RECIP-DRIVER SECTION.

       P2000-PROCESS.
      * ONE CARRIER PER PASS.  THE AGGREGATION FILE CARRIES ONE
      * RECORD PER COUNTERPARTY FOR THE PERIOD.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ THRU P2100-EXIT.
           IF WS-EOF
               GO TO P2000-EXIT.
           MOVE RAI-RECORD TO WS-AGG-IN.
           MOVE WS-AIK-KEY TO WS-RESTART-KEY.
           MOVE SPACES TO WS-D1-DISPO.
           MOVE 'N' TO WS-CAP-APPLIED-SW.
           MOVE 'Y' TO WS-ELIGIBLE-SW.
           PERFORM P2200-PERIOD-TEST THRU P2200-EXIT.
           IF NOT WS-ELIGIBLE
               ADD 1 TO WS-SUMM-CNT
               GO TO P2000-EXIT.
           PERFORM P2300-CARRIER-LOOKUP THRU P2300-EXIT.
           IF NOT WS-ELIGIBLE
               ADD 1 TO WS-SUMM-CNT
               MOVE WS-MSG-TEXT (7) TO WS-D1-DISPO
               PERFORM P6100-DETAIL THRU P6100-EXIT
               GO TO P2000-EXIT.
           PERFORM P2400-LOAD-MINUTES THRU P2400-EXIT.
           IF WS-RC-TOTAL-MOU = ZERO
               ADD 1 TO WS-SUMM-CNT
               MOVE WS-MSG-TEXT (11) TO WS-D1-DISPO
               PERFORM P6100-DETAIL THRU P6100-EXIT
               GO TO P2000-EXIT.
           PERFORM P2800-RESOLVE-RATE THRU P2800-EXIT.
           PERFORM P2900-RESOLVE-CAP THRU P2900-EXIT.
           PERFORM P3000-APPLY-RATE THRU P3000-EXIT.
           PERFORM P3200-ISP-CAP THRU P3200-EXIT.
           PERFORM P3400-NET-DUE THRU P3400-EXIT.
           PERFORM P3600-RECONCILE THRU P3600-EXIT.
           PERFORM P4000-BUILD-SETTLE THRU P4000-EXIT.
           PERFORM P4200-WRITE-SETTLE THRU P4200-EXIT.
           IF WS-CAP-APPLIED
               PERFORM P4400-WRITE-CAPPED THRU P4400-EXIT.
           PERFORM P6100-DETAIL THRU P6100-EXIT.
           ADD 1 TO WS-WRITE-CNT.
           ADD 1 TO WS-SETTLE-CNT.

       P2000-EXIT.
           EXIT.

       P2100-READ.
      * SEQUENTIAL READ OF THE AGGREGATION FILE.
           READ RECIP-AGG-FILE
               AT END
                   MOVE 'Y' TO WS-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00'
               MOVE 3650 TO WS-AB-CODE
               MOVE 'READ ERROR UT-S-RECAGG' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-READ-CNT.

       P2100-EXIT.
           EXIT.

       P2200-PERIOD-TEST.
      * ONLY THE PERIOD ON THE CONTROL CARD IS SETTLED.  A RECORD
      * FOR ANY OTHER PERIOD IS LEFT FOR THE RUN THAT OWNS IT.
           IF WS-AI-PERIOD NOT = WS-PE-SETTLE-PERIOD
               MOVE 'N' TO WS-ELIGIBLE-SW.
           IF WS-AI-OCN = SPACES
               MOVE 'N' TO WS-ELIGIBLE-SW
               MOVE EC-OCN-UNKNOWN TO WS-ERR-CODE
               MOVE 'E' TO WS-ERR-SEVERITY
               PERFORM P7000-SUSPEND THRU P7000-EXIT.

       P2200-EXIT.
           EXIT.

       P2300-CARRIER-LOOKUP.
      * THE CARRIER MASTER CARRIES THE ELIGIBILITY FLAG, THE
      * NEGOTIATED RATE AND THE ISP CAP.  A CARRIER THAT IS NOT
      * FLAGGED ELIGIBLE TERMINATES MINUTES FOR NOTHING.
           MOVE 'P2300-CARRIER-LOOKUP' TO WS-PARA-NAME.
           MOVE 'N' TO WS-CARR-FOUND-SW.
           MOVE WS-AI-OCN TO CRM-KEY.
           READ CARRIER-MASTER
               INVALID KEY
                   MOVE 'N' TO WS-ELIGIBLE-SW
                   GO TO P2300-EXIT.
           MOVE CRM-RECORD TO CABS-CARRIER-RECORD.
           MOVE 'Y' TO WS-CARR-FOUND-SW.
           IF CR-RECIP-COMP-ELIG NOT = 'Y'
               MOVE 'N' TO WS-ELIGIBLE-SW.
           IF CR-WIRELESS
               MOVE 'N' TO WS-ELIGIBLE-SW
               MOVE WS-MSG-TEXT (14) TO WS-D1-DISPO.

       P2300-EXIT.
           EXIT.

       P2400-LOAD-MINUTES.
      * MOVE THE AGGREGATED MINUTES INTO THE WORK AREA.  THE ISP
      * BOUND SPLIT WAS MADE BY CABSET04 AND IS TAKEN ON TRUST HERE
      * - THIS PROGRAM DOES NOT RECLASSIFY ANYTHING.
           MOVE WS-AI-TOTAL-MOU TO WS-RC-TOTAL-MOU.
           MOVE WS-AI-ISP-MOU TO WS-RC-ISP-MOU.
           MOVE WS-AI-VOICE-MOU TO WS-RC-VOICE-MOU.
           MOVE ZERO TO WS-RC-CAPPED-MOU.
           MOVE ZERO TO WS-RC-CAP-AMT.
           ADD WS-RC-TOTAL-MOU TO WS-TOT-MOU.
           ADD WS-RC-TOTAL-MOU TO WS-ACC-MINUTES.

       P2400-EXIT.
           EXIT.

       P2500-MATCH-USAGE.
      * OPTIONALLY MATCH THE AGGREGATE BACK TO THE USAGE DETAIL ON
      * THE BILLING SIDE.  THE USAGE FILE IS OWNED BY THE CABS
      * APPLICATION AND IS READ DIRECTLY - THE SETTLEMENT RUN
      * CANNOT START UNTIL BILLING HAS RELEASED IT.
      * DATASET SHARING APPROVED BY THE DATA ADMINISTRATION GROUP.
           MOVE 'P2500-MATCH-USAGE' TO WS-PARA-NAME.
           IF WS-CDR-EOF
               MOVE WS-MSG-TEXT (15) TO WS-D1-DISPO
               GO TO P2500-EXIT.
           MOVE ZERO TO WS-RC-OVER-MOU.
           PERFORM P2550-SCAN-USAGE THRU P2550-EXIT
               UNTIL WS-CDR-EOF
                  OR CD-OCN NOT < WS-AI-OCN.
           IF WS-CDR-EOF
               GO TO P2500-EXIT.
           IF CD-OCN NOT = WS-AI-OCN
               MOVE WS-MSG-TEXT (15) TO WS-D1-DISPO.

       P2500-EXIT.
           EXIT.

       P2550-SCAN-USAGE.
      * ADVANCE THE USAGE FILE AND ACCUMULATE THE MINUTES SEEN.
           IF CD-OCN = WS-AI-OCN
               ADD CD-VC-CHG-MIN TO WS-RC-OVER-MOU.
           PERFORM P2600-READ-CDR THRU P2600-EXIT.

       P2550-EXIT.
           EXIT.

       P2600-READ-CDR.
      * READ THE CABS RECIPROCAL USAGE FILE.
           READ RECIP-CDR-FILE INTO CABS-CDR-RECORD
               AT END
                   MOVE 'Y' TO WS-CDR-EOF-SW
                   GO TO P2600-EXIT.

       P2600-EXIT.
           EXIT.

       P2800-RESOLVE-RATE.
      * THE RATE COMES FROM THE INTERCONNECTION AGREEMENT TABLE
      * FIRST, THE CARRIER MASTER SECOND AND THE CONTROL CARD LAST.
      * A ZERO RATE IS A VALID OUTCOME - SEVERAL AGREEMENTS MOVED
      * TO BILL AND KEEP AND CARRY A GENUINE ZERO.
           MOVE 'P2800-RESOLVE-RATE' TO WS-PARA-NAME.
           MOVE ZERO TO WS-RC-RATE.
           MOVE 'N' TO WS-AGREE-FOUND-SW.
           SET WS-AG-IX TO 1.
           SEARCH WS-AG-ENTRY
               AT END
                   GO TO P2820-CARRIER-RATE
               WHEN WS-AG-OCN (WS-AG-IX) = WS-AI-OCN
                   MOVE WS-AG-RATE (WS-AG-IX) TO WS-RC-RATE
                   MOVE 'Y' TO WS-AGREE-FOUND-SW.
           IF WS-AGREE-FOUND
               MOVE WS-MSG-TEXT (1) TO WS-D1-DISPO
               GO TO P2800-EXIT.

       P2800-EXIT.
           EXIT.

       P2820-CARRIER-RATE.
      * FALL BACK TO THE CARRIER MASTER AND THEN TO THE CARD.
           ADD 1 TO WS-NOAGREE-CNT.
           IF WS-CARR-FOUND AND CR-RECIP-RATE NOT = ZERO
               MOVE CR-RECIP-RATE TO WS-RC-RATE
               MOVE WS-MSG-TEXT (4) TO WS-D1-DISPO
               GO TO P2820-EXIT.
           MOVE WS-PE-DEF-RATE TO WS-RC-RATE.
           MOVE WS-MSG-TEXT (5) TO WS-D1-DISPO.
           IF WS-RC-RATE = ZERO
               ADD 1 TO WS-ZERORATE-CNT
               MOVE WS-MSG-TEXT (6) TO WS-D1-DISPO.

       P2820-EXIT.
           EXIT.

       P2900-RESOLVE-CAP.
      * THE ISP BOUND CAP COMES FROM THE AGREEMENT TABLE, THE
      * CARRIER MASTER OR THE CONTROL CARD OVERRIDE.  A CAP OF ZERO
      * MEANS NO CAP - IT DOES NOT MEAN ZERO BILLABLE MINUTES.
           MOVE 'P2900-RESOLVE-CAP' TO WS-PARA-NAME.
           MOVE ZERO TO WS-RC-CAP.
           IF WS-AGREE-FOUND
               SET WS-AG-IX TO 1
               SEARCH WS-AG-ENTRY
                   AT END
                       MOVE ZERO TO WS-RC-CAP
                   WHEN WS-AG-OCN (WS-AG-IX) = WS-AI-OCN
                       MOVE WS-AG-CAP (WS-AG-IX) TO WS-RC-CAP.
           IF WS-RC-CAP = ZERO AND WS-CARR-FOUND
               MOVE CR-ISP-CAP-MOU TO WS-RC-CAP.
           IF WS-PE-CAP-OVERRIDE NOT = ZERO
               MOVE WS-PE-CAP-OVERRIDE TO WS-RC-CAP
               MOVE WS-MSG-TEXT (8) TO WS-D1-DISPO.
           IF WS-RC-CAP = ZERO
               MOVE WS-MSG-TEXT (17) TO WS-D1-DISPO.

       P2900-EXIT.
           EXIT.


      *****************************************************************
      * S300-COMPENSATION                                             *
      * APPLY THE RATE AND THE ISP BOUND CAP.                         *
      *****************************************************************
       S300-COMPENSATION SECTION.

       P3000-APPLY-RATE.
      * APPLY THE NEGOTIATED RATE TO THE TERMINATING MINUTES.  THE
      * GROSS AMOUNT IS THE COMPENSATION DUE TO THE CLEC BEFORE ANY
      * ADJUSTMENT.  IT IS CARRIED AT FIVE DECIMAL PLACES.
           MOVE 'P3000-APPLY-RATE' TO WS-PARA-NAME.
           COMPUTE WS-RC-GROSS-AMT ROUNDED =
                   WS-RC-TOTAL-MOU * WS-RC-RATE.
           ADD WS-RC-GROSS-AMT TO WS-TOT-GROSS.

       P3000-EXIT.
           EXIT.

       P3200-ISP-CAP.
      * THE ISP BOUND CAP.  MINUTES DELIVERED TO AN INTERNET
      * SERVICE PROVIDER ABOVE THE AGREED CAP ARE COMPENSATED AT
      * ZERO - THAT IS WHAT THE FCC ORDER AND EVERY INTERCONNECTION
      * AGREEMENT SINCE 2001 PROVIDE FOR.  THE CAPPED MINUTES ARE
      * RECORDED SO THAT THE CLEC CAN SEE WHY THEIR INVOICE IS
      * SHORT.
           MOVE 'P3200-ISP-CAP' TO WS-PARA-NAME.
           MOVE WS-RC-TOTAL-MOU TO WS-RC-BILL-MOU.
           IF WS-RC-CAP = ZERO
               MOVE ZERO TO WS-RC-CAPPED-MOU
               GO TO P3200-EXIT.
           IF WS-RC-ISP-MOU NOT > WS-RC-CAP
               MOVE ZERO TO WS-RC-CAPPED-MOU
               GO TO P3200-EXIT.
           COMPUTE WS-RC-CAPPED-MOU =
                   WS-RC-ISP-MOU - WS-RC-CAP.
           COMPUTE WS-RC-BILL-MOU =
                   WS-RC-TOTAL-MOU - WS-RC-CAPPED-MOU.
           COMPUTE WS-RC-CAP-AMT ROUNDED =
                   WS-RC-CAPPED-MOU * WS-RC-RATE.
           MOVE 'Y' TO WS-CAP-APPLIED-SW.
           ADD 1 TO WS-CAPPED-CNT.
           ADD WS-RC-CAPPED-MOU TO WS-TOT-CAP-MOU.
           ADD WS-RC-CAP-AMT TO WS-TOT-CAP-AMT.
           MOVE WS-MSG-TEXT (2) TO WS-D1-DISPO.

       P3200-EXIT.
           EXIT.

       P3400-NET-DUE.
      * THE NET DUE IS THE COMPENSATION PAYABLE TO THE CLEC.  IT IS
      * ROUNDED TO TWO PLACES FOR SETTLEMENT.  CABSET11 TRUNCATES
      * THE SAME FIGURE WHEN IT PRINTS THE STATEMENT.
      * AGREED WITH REVENUE ACCOUNTING AT THE 1992 TARIFF FILING.
           MOVE 'P3400-NET-DUE' TO WS-PARA-NAME.
           COMPUTE WS-RC-NET-DUE ROUNDED = WS-RC-GROSS-AMT.
           COMPUTE WS-RC-EXACT-AMT = WS-RC-GROSS-AMT.
           COMPUTE WS-RC-ROUND-DIFF =
                   WS-RC-EXACT-AMT - WS-RC-NET-DUE.
           ADD WS-RC-ROUND-DIFF TO WS-TOT-ROUND-DIFF.
           ADD WS-RC-NET-DUE TO WS-TOT-NET.
           ADD WS-RC-BILL-MOU TO WS-TOT-BILL-MOU.
           ADD WS-RC-NET-DUE TO WS-ACC-AMOUNT.
           MOVE WS-MSG-TEXT (10) TO WS-D1-DISPO.

       P3400-EXIT.
           EXIT.

       P3600-RECONCILE.
      * BILLABLE MINUTES PLUS CAPPED MINUTES MUST EQUAL THE TOTAL.
      * A FAILURE HERE MEANS MINUTES HAVE BEEN LOST BETWEEN THE
      * AGGREGATION AND THE CAP, AND THE CLEC WILL FIND IT.
           COMPUTE WS-RC-OVER-MOU =
                   WS-RC-BILL-MOU + WS-RC-CAPPED-MOU.
           IF WS-RC-OVER-MOU = WS-RC-TOTAL-MOU
               GO TO P3600-EXIT.
           MOVE WS-MSG-TEXT (18) TO WS-D1-DISPO.
           MOVE EC-RECIP-CAP-EXCEEDED TO WS-ERR-CODE.
           MOVE 'W' TO WS-ERR-SEVERITY.
           PERFORM P7000-SUSPEND THRU P7000-EXIT.
           SUBTRACT 1 FROM WS-REJECT-CNT.

       P3600-EXIT.
           EXIT.


      *****************************************************************
      * S400-OUTPUT                                                   *
      * BUILD AND WRITE THE SETTLEMENT.                               *
      *****************************************************************
       S400-OUTPUT SECTION.

       P4000-BUILD-SETTLE.
      * BUILD THE SETTLEMENT RECORD.  THE BASIS AREA CARRIES THE
      * TOTAL, THE BILLABLE AND THE CAPPED MINUTES SO THAT THE
      * STATEMENT CAN SHOW ALL THREE.
           MOVE 'P4000-BUILD-SETTLE' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-SETTLEMENT-RECORD.
           MOVE 'R' TO ST-SETTLE-TYPE.
           MOVE WS-AI-OCN TO ST-COUNTERPARTY-OCN.
           MOVE WS-PE-SETTLE-PERIOD TO ST-SETTLE-PERIOD.
           ADD 1 TO WS-SEQ-NBR.
           MOVE WS-SEQ-NBR TO ST-SEQ.
           MOVE WS-RC-TOTAL-MOU TO ST-TOTAL-MOU.
           MOVE WS-RC-BILL-MOU TO ST-BILLABLE-MOU.
           MOVE WS-RC-CAPPED-MOU TO ST-CAPPED-MOU.
           MOVE WS-RC-RATE TO ST-RATE-APPLIED.
           MOVE ZERO TO ST-OUR-PCT.
           MOVE ZERO TO ST-THEIR-PCT.
           MOVE ZERO TO ST-PCT-VARIANCE.
           MOVE SPACES TO ST-TRUNK-GRP.
           MOVE SPACES TO ST-CIRCUIT-ID.
           MOVE WS-RC-GROSS-AMT TO ST-GROSS-AMT.
           MOVE ZERO TO ST-OUR-SHARE.
           MOVE WS-RC-GROSS-AMT TO ST-THEIR-SHARE.
           MOVE WS-RC-NET-DUE TO ST-NET-DUE.
           MOVE WS-RC-ROUND-DIFF TO ST-ROUND-RESIDUE.
           MOVE 'P' TO ST-DIRECTION.
           MOVE 'N' TO ST-DISPUTE-SW.
           MOVE WS-CYCLE-YYDDD TO ST-EXCH-YYDDD.
           MOVE SPACES TO ST-RAO-CODE.

       P4000-EXIT.
           EXIT.

       P4200-WRITE-SETTLE.
      * WRITE THE SETTLEMENT RECORD.
           IF WS-PE-SIM-SW = 'Y'
               MOVE WS-MSG-TEXT (12) TO WS-D1-DISPO
               GO TO P4200-EXIT.
           MOVE CABS-SETTLEMENT-RECORD TO STO-RECORD.
           WRITE STO-RECORD.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 6504 TO WS-AB-CODE
               MOVE 'SETTLEMENT WRITE FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           MOVE WS-MSG-TEXT (16) TO WS-D1-DISPO.

       P4200-EXIT.
           EXIT.

       P4400-WRITE-CAPPED.
      * WRITE THE CAPPED MINUTE DETAIL.  THIS FILE IS SENT TO THE
      * CLEC EVERY MONTH AND IS THE ONLY EVIDENCE THEY GET FOR THE
      * MINUTES THAT WERE NOT COMPENSATED.
           MOVE SPACES TO WS-CAP-RECORD.
           MOVE WS-AI-OCN TO WS-CP-OCN.
           MOVE WS-PE-SETTLE-PERIOD TO WS-CP-PERIOD.
           MOVE WS-PE-STATE TO WS-CP-STATE.
           MOVE WS-RC-TOTAL-MOU TO WS-CP-TOTAL-MOU.
           MOVE WS-RC-ISP-MOU TO WS-CP-ISP-MOU.
           MOVE WS-RC-CAP TO WS-CP-CAP.
           MOVE WS-RC-CAPPED-MOU TO WS-CP-CAPPED-MOU.
           MOVE WS-RC-CAP-AMT TO WS-CP-CAP-AMT.
           MOVE WS-RC-RATE TO WS-CP-RATE.
           MOVE WS-CAP-RECORD TO CAP-RECORD.
           WRITE CAP-RECORD.
           MOVE WS-MSG-TEXT (9) TO WS-D1-DISPO.

       P4400-EXIT.
           EXIT.


      *****************************************************************
      * S600-REPORT                                                   *
      * SETTLEMENT REGISTER AND TOTALS.                               *
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
           MOVE WS-AI-OCN TO WS-D1-OCN.
           MOVE WS-PE-STATE TO WS-D1-STATE.
           MOVE WS-RC-TOTAL-MOU TO WS-D1-TOTMOU.
           MOVE WS-RC-ISP-MOU TO WS-D1-ISPMOU.
           MOVE WS-RC-CAP TO WS-D1-CAP.
           MOVE WS-RC-BILL-MOU TO WS-D1-BILLMOU.
           MOVE WS-RC-RATE TO WS-D1-RATE.
           MOVE WS-RC-NET-DUE TO WS-D1-NET.
           WRITE PRT-RECORD FROM WS-DETAIL-1 AFTER ADVANCING 1 LINES.
           ADD 1 TO WS-LINE-CNT.

       P6100-EXIT.
           EXIT.

       P6300-TOTALS.
      * END OF RUN TOTALS PAGE.
           PERFORM P6000-HEADING THRU P6000-EXIT.
           MOVE SPACES TO WS-TOTAL-1.
           MOVE 'TERMINATING MINUTES' TO WS-T1-DESC.
           MOVE WS-SETTLE-CNT TO WS-T1-COUNT.
           MOVE WS-TOT-MOU TO WS-T1-AMOUNT.
           WRITE PRT-RECORD FROM WS-TOTAL-1 AFTER ADVANCING 2 LINES.
           MOVE 'BILLABLE MINUTES' TO WS-T1-DESC.
           MOVE WS-SETTLE-CNT TO WS-T1-COUNT.
           MOVE WS-TOT-BILL-MOU TO WS-T1-AMOUNT.
           WRITE PRT-RECORD FROM WS-TOTAL-1 AFTER ADVANCING 1 LINES.
           MOVE 'CAPPED MINUTES' TO WS-T1-DESC.
           MOVE WS-CAPPED-CNT TO WS-T1-COUNT.
           MOVE WS-TOT-CAP-MOU TO WS-T1-AMOUNT.
           WRITE PRT-RECORD FROM WS-TOTAL-1 AFTER ADVANCING 1 LINES.
           MOVE 'GROSS COMPENSATION' TO WS-T1-DESC.
           MOVE WS-SETTLE-CNT TO WS-T1-COUNT.
           MOVE WS-TOT-GROSS TO WS-T1-AMOUNT.
           WRITE PRT-RECORD FROM WS-TOTAL-1 AFTER ADVANCING 1 LINES.
           MOVE 'VALUE OF CAPPED MINUTES' TO WS-T1-DESC.
           MOVE WS-CAPPED-CNT TO WS-T1-COUNT.
           MOVE WS-TOT-CAP-AMT TO WS-T1-AMOUNT.
           WRITE PRT-RECORD FROM WS-TOTAL-1 AFTER ADVANCING 1 LINES.
           MOVE 'NET DUE TO COUNTERPARTIES' TO WS-T1-DESC.
           MOVE WS-SETTLE-CNT TO WS-T1-COUNT.
           MOVE WS-TOT-NET TO WS-T1-AMOUNT.
           WRITE PRT-RECORD FROM WS-TOTAL-1 AFTER ADVANCING 1 LINES.
           ADD 7 TO WS-LINE-CNT.

       P6300-EXIT.
           EXIT.


      *****************************************************************
      * S500-GROWTH-CAP                                               *
      * THE 2001 GROWTH CEILING.  SUPERSEDED IN 2004.                 *
      *****************************************************************
       S500-GROWTH-CAP SECTION.

       P5000-GROWTH-COMPARE.
      * COMPARE THIS PERIODS ISP BOUND MINUTES WITH THE SAME PERIOD
      * A YEAR EARLIER.  UNDER THE 2001 ORDER THE GROWTH ABOVE TEN
      * PERCENT WAS NOT COMPENSABLE.  THE ORDER WAS SUPERSEDED IN
      * 2004 AND THE CEILING NO LONGER APPLIES, BUT THE COMPARISON
      * IS STILL MADE AND THE RESULT IS STILL REPORTED - IT IS THE
      * ONLY PLACE ANYBODY CAN SEE HOW ISP TRAFFIC IS MOVING.
           MOVE 'P5000-GROWTH-COMPARE' TO WS-PARA-NAME.
           MOVE ZERO TO WS-GW-PRIOR-MOU.
           SET WS-PC-IX TO 1.
           SEARCH WS-PC-ENTRY
               AT END
                   GO TO P5000-EXIT
               WHEN WS-PC-OCN (WS-PC-IX) = WS-AI-OCN
                   MOVE WS-PC-MOU (WS-PC-IX) TO WS-GW-PRIOR-MOU.
           IF WS-GW-PRIOR-MOU = ZERO
               GO TO P5000-EXIT.
           COMPUTE WS-GW-GROWTH-MOU =
                   WS-RC-ISP-MOU - WS-GW-PRIOR-MOU.
           COMPUTE WS-GW-GROWTH-PCT ROUNDED =
                   (WS-GW-GROWTH-MOU / WS-GW-PRIOR-MOU) * 100.
           COMPUTE WS-GW-ALLOWED-MOU ROUNDED =
                   WS-GW-PRIOR-MOU * 1.10000.
           IF WS-RC-ISP-MOU > WS-GW-ALLOWED-MOU
               COMPUTE WS-GW-EXCESS-MOU =
                       WS-RC-ISP-MOU - WS-GW-ALLOWED-MOU
               ADD 1 TO WS-GW-COUNT.

       P5000-EXIT.
           EXIT.

       P5200-GROWTH-REPORT.
      * REPORT THE GROWTH FIGURES ON THE REGISTER.  NOTHING IS
      * DEDUCTED - THE 2004 CHANGE REMOVED THE DEDUCTION AND LEFT
      * THE REPORTING.
           IF WS-GW-EXCESS-MOU = ZERO
               GO TO P5200-EXIT.
           MOVE SPACES TO WS-TOTAL-1.
           MOVE 'ISP GROWTH ABOVE THE 2001 CEILING' TO WS-T1-DESC.
           MOVE WS-GW-COUNT TO WS-T1-COUNT.
           MOVE WS-GW-EXCESS-MOU TO WS-T1-AMOUNT.
           WRITE PRT-RECORD FROM WS-TOTAL-1 AFTER ADVANCING 1 LINES.
           ADD 1 TO WS-LINE-CNT.
           MOVE WS-MSG-TEXT (19) TO WS-D1-DISPO.

       P5200-EXIT.
           EXIT.

       P5400-AGREEMENT-DATE.
      * CHECK THAT THE AGREEMENT WAS IN FORCE DURING THE PERIOD.
      * THE EFFECTIVE DATE ON THE TABLE IS A TWO DIGIT YEAR JULIAN
      * AND IS EXPANDED WITH THE PIVOT OF 70 BEFORE THE COMPARE.
           IF NOT WS-AGREE-FOUND
               GO TO P5400-EXIT.
           SET WS-AG-IX TO 1.
           SEARCH WS-AG-ENTRY
               AT END
                   GO TO P5400-EXIT
               WHEN WS-AG-OCN (WS-AG-IX) = WS-AI-OCN
                   MOVE WS-AG-EFF (WS-AG-IX) TO WS-AGREE-EFF.
           MOVE WS-AGREE-EFF TO WS-JW-TEST.
           IF WS-JW-TEST-YY < 70
               COMPUTE WS-AGREE-CCYY = 2000 + WS-JW-TEST-YY
           ELSE
               COMPUTE WS-AGREE-CCYY = 1900 + WS-JW-TEST-YY.
           IF WS-AGREE-CCYY > 2019
               MOVE WS-MSG-TEXT (13) TO WS-D1-DISPO.

       P5400-EXIT.
           EXIT.


      *****************************************************************
      * S550-CLASSIFICATION                                           *
      * ISP BOUND CLASSIFICATION AND MONTH ON MONTH TRUE UP.          *
      *****************************************************************
       S550-CLASSIFICATION SECTION.

       P5600-ISP-CLASSIFY.
      * CLASSIFY A TERMINATING NUMBER AS ISP BOUND OR VOICE.  THE
      * CLASSIFICATION DECIDES WHETHER THE MINUTES ARE SUBJECT TO
      * THE CAP, AND THEREFORE WHETHER THEY ARE PAID FOR.  THE
      * TABLE IS THE ONLY EVIDENCE - THERE IS NO NETWORK SIGNAL
      * THAT SAYS A CALL WENT TO AN INTERNET SERVICE PROVIDER.
           MOVE 'P5600-ISP-CLASSIFY' TO WS-PARA-NAME.
           MOVE 'V' TO WS-CLASS-HOLD.
           IF WS-CDR-EOF
               GO TO P5600-EXIT.
           MOVE CD-VC-TERM-NPANXX TO WS-NPANXX-WORK.
           SET WS-IN-IX TO 1.
           SEARCH WS-IN-ENTRY
               AT END
                   GO TO P5600-EXIT
               WHEN WS-IN-NPA (WS-IN-IX) = WS-NW-NPA AND
                    WS-IN-NXX (WS-IN-IX) = WS-NW-NXX
                   MOVE WS-IN-CLASS (WS-IN-IX) TO WS-CLASS-HOLD.
           ADD 1 TO WS-TU-CLASSIFIED.

       P5600-EXIT.
           EXIT.

       P5700-ISP-PROPORTION.
      * COMPUTE THE PROPORTION OF THE CARRIERS TERMINATING MINUTES
      * THAT ARE ISP BOUND.  THE PROPORTION IS REPORTED AND IS THE
      * FIGURE THE ACCESS MANAGEMENT GROUP ARGUES ABOUT WITH THE
      * CLEC EVERY QUARTER.
           IF WS-RC-TOTAL-MOU = ZERO
               MOVE ZERO TO WS-TU-ISP-PCT
               GO TO P5700-EXIT.
           COMPUTE WS-TU-ISP-PCT ROUNDED =
                   (WS-RC-ISP-MOU / WS-RC-TOTAL-MOU) * 100.
           IF WS-TU-ISP-PCT > 090.00000
               MOVE EC-RECIP-CAP-EXCEEDED TO WS-ERR-CODE
               MOVE 'W' TO WS-ERR-SEVERITY
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               SUBTRACT 1 FROM WS-REJECT-CNT.

       P5700-EXIT.
           EXIT.

       P5800-TRUE-UP.
      * COMPARE THIS PERIODS NET WITH THE PRIOR PERIOD.  A SWING OF
      * MORE THAN TWENTY FIVE PERCENT IS REPORTED.  NOTHING IS
      * ADJUSTED - THE TRUE UP IS INFORMATIONAL AND ANY CORRECTION
      * IS RAISED BY HAND THROUGH THE DISPUTE PROCESS.
           MOVE 'P5800-TRUE-UP' TO WS-PARA-NAME.
           MOVE WS-RC-NET-DUE TO WS-TU-CURRENT-NET.
           IF WS-TU-PRIOR-NET = ZERO
               MOVE WS-TU-CURRENT-NET TO WS-TU-PRIOR-NET
               GO TO P5800-EXIT.
           COMPUTE WS-TU-DELTA =
                   WS-TU-CURRENT-NET - WS-TU-PRIOR-NET.
           COMPUTE WS-TU-PCT ROUNDED =
                   (WS-TU-DELTA / WS-TU-PRIOR-NET) * 100.
           IF WS-TU-PCT < 0
               COMPUTE WS-TU-PCT = WS-TU-PCT * -1.
           IF WS-TU-PCT > 025.00000
               ADD 1 TO WS-TU-SWING-CNT.
           ADD 1 TO WS-TU-COUNT.
           MOVE WS-TU-CURRENT-NET TO WS-TU-PRIOR-NET.

       P5800-EXIT.
           EXIT.

       P5900-CLASS-TOTALS.
      * PRINT THE CLASSIFICATION TOTALS AT END OF RUN.
           MOVE SPACES TO WS-TOTAL-1.
           MOVE 'MINUTES CLASSIFIED FROM THE TABLE' TO WS-T1-DESC.
           MOVE WS-TU-CLASSIFIED TO WS-T1-COUNT.
           MOVE WS-TU-ISP-PCT TO WS-T1-AMOUNT.
           WRITE PRT-RECORD FROM WS-TOTAL-1 AFTER ADVANCING 2 LINES.
           MOVE 'CARRIERS WITH A LARGE SWING' TO WS-T1-DESC.
           MOVE WS-TU-SWING-CNT TO WS-T1-COUNT.
           MOVE WS-TU-DELTA TO WS-T1-AMOUNT.
           WRITE PRT-RECORD FROM WS-TOTAL-1 AFTER ADVANCING 1 LINES.
           ADD 3 TO WS-LINE-CNT.

       P5900-EXIT.
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
           MOVE WS-AGG-IN TO SU-ORIG-RECORD.
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
           MOVE 240                    TO CT-STEP-SEQ.
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
           PERFORM P6300-TOTALS THRU P6300-EXIT.
           DISPLAY 'CARRIERS SETTLED ' WS-SETTLE-CNT.
           DISPLAY 'CARRIERS CAPPED  ' WS-CAPPED-CNT.
           DISPLAY 'NO AGREEMENT     ' WS-NOAGREE-CNT.
           DISPLAY 'ZERO RATE        ' WS-ZERORATE-CNT.
           DISPLAY 'TOTAL MOU        ' WS-TOT-MOU.
           DISPLAY 'BILLABLE MOU     ' WS-TOT-BILL-MOU.
           DISPLAY 'CAPPED MOU       ' WS-TOT-CAP-MOU.
           DISPLAY 'GROSS AMOUNT     ' WS-TOT-GROSS.
           DISPLAY 'NET DUE          ' WS-TOT-NET.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE RECIP-AGG-FILE
                 RECIP-CDR-FILE
                 CARRIER-MASTER
                 SETTLE-OUT-FILE
                 CAPPED-OUT-FILE
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

