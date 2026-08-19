      *****************************************************************
      * CABRPT08 - MONTH END CLOSE REPORT AND LEDGER POSTING          *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               BHDRIN  TELCABS.CABS.BILLHDR.FIN(0)       CABSBHDR*
      *               CTLIN   TELCABS.CABS.CONTROL(0)           CABSCTL*
      *               SYSIN   INSTREAM CONTROL CARD             (LOCAL)*
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               CLSOUT  SYSOUT PRINT - CLOSE REPORT       CABSPRNT*
      *               CLOSEMS TELCABS.CABS.CLOSEMST             (LOCAL)*
      *               REPORT  SYSOUT PRINT - RUN REGISTER       CABSPRNT*
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : THE TEN LEDGER LINES MUST NET TO ZERO AND NO PROCESS IN*
      *               THE PERIOD MAY BE OUT OF BALANCE                *
      * RESTART     : FULL RERUN - THE CLOSE RECORD IS REWRITTEN      *
      * REVISION HISTORY                                              *
      *   V1.00  1989-09-29  L.HARGREAVES INITIAL RELEASE - FIVE LEDGER LINES*
      *   V1.06  1993-02-11  M.J.FERRARO  JURISDICTIONAL REVENUE SPLIT INTO*
      *                      THREE SEPARATE LEDGER ACCOUNTS           *
      *   V1.11  1996-06-20  J.M.CASTILLO CLOSE KEY INTRODUCED FOR THE NEW*
      *                      LEDGER INTERFACE                         *
      *   V2.00  2001-12-05  P.NAIR       CONTROL FILE READ AGAIN HERE RATHER*
      *                      THAN TRUSTING THE BALANCING REPORT       *
      *   V2.04  2007-03-08  A.BUKOWSKI   LEDGER PROOF ADDED - THE NINE LINES*
      *                      MUST ADD BACK TO THE RECEIVABLE          *
      *   V2.09  2019-06-11  G.PRZYBYLSKI SIGN OFF INITIALS CARRIED ONTO THE*
      *                      CLOSE RECORD FOR THE AUDIT TRAIL         *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABRPT08.
       AUTHOR. TELCABS APPLICATIONS - BILLING CONTROL TEAM.
      *****************************************************************
      * THE MONTH END CLOSE.  ACCUMULATES THE LEDGER POSTING LINES    *
      * FROM THE FINAL INVOICES, PROVES THEM AGAINST EACH OTHER,      *
      * PROVES THAT THE CYCLE BALANCED AND WRITES THE CLOSE RECORD    *
      * THE GENERAL LEDGER INTERFACE PICKS UP.                        *
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
           SELECT CTL-IN-FILE ASSIGN TO UT-S-CTLIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-TABLE.
           SELECT CLS-OUT-FILE ASSIGN TO UT-S-CLSOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT CLOSE-MST-FILE ASSIGN TO UT-S-CLOSEMS
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-SUSPENSE.
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
      * CTLIN - THE CONTROL FILE FOR THE WHOLE PERIOD.                *
      *****************************************************************
       FD  CTL-IN-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CTL-IN-REC                       PIC X(180).
      *****************************************************************
      * CLSOUT - THE PRINTED CLOSE REPORT.                            *
      *****************************************************************
       FD  CLS-OUT-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       01  CLS-RECORD                       PIC X(133).
      *****************************************************************
      * CLOSEMS - THE CLOSE MASTER.  THE DATASET IS                   *
      * DEFINED BY AN IDCAMS JOB IN THE VSAM LIBRARY AND              *
      * IS READ BY THE LEDGER INTERFACE, WHICH IS NOT                 *
      * PART OF THIS ESTATE.                                          *
      *****************************************************************
       FD  CLOSE-MST-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       01  CLOSE-RECORD                     PIC X(200).
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
           05  WS-PGM-NAME             PIC X(08) VALUE 'CABRPT08'.
           05  WS-PGM-VERSION          PIC X(05) VALUE 'V2.09'.
           05  WS-PGM-APPL             PIC X(04) VALUE 'CABS'.
           05  WS-PGM-COMPILE          PIC X(08) VALUE '20190611'.
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
           05  WS-PE-CLOSE-PERIOD      PIC 9(06).
           05  WS-PE-CLOSE-YYDDD       PIC 9(05).
           05  WS-PE-LEDGER-CO         PIC X(04).
           05  WS-PE-SIGN-OFF          PIC X(08).
           05  WS-PE-FILLER            PIC X(22).
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
           05  WS-CTL-EOF-SW           PIC X(01) VALUE 'N'.
               88  WS-CTL-EOF          VALUE 'Y'.
           05  WS-CLOSE-OK-SW          PIC X(01) VALUE 'Y'.
               88  WS-CLOSE-OK         VALUE 'Y'.
      *****************************************************************
      * THE CLOSE KEY.  THIRTY BYTES ASSEMBLED FROM SIX PIECES.  IT IS*
      * WRITTEN ONTO THE CLOSE MASTER AND IS WHAT THE GENERAL LEDGER  *
      * INTERFACE QUOTES WHEN IT PICKS THE PERIOD UP.  IT IS BUILT    *
      * PIECE BY PIECE BECAUSE THE LEDGER COMPANY CODE AND THE PERIOD *
      * COME FROM DIFFERENT PLACES AND THE SEQUENCE IS ZERO FILLED.   *
      * PRINT LINE ASSEMBLY PER CABS-STD-063.                         *
      *****************************************************************
       01  WS-CLOSE-PIECES.
           05  WS-CP-PREFIX            PIC X(03) VALUE 'CLS'.
           05  WS-CP-LEDGER-CO         PIC X(04) VALUE SPACES.
           05  WS-CP-PERIOD            PIC 9(06) VALUE 0.
           05  WS-CP-CYCLE             PIC 9(05) VALUE 0.
           05  WS-CP-SEQ               PIC 9(04) VALUE 0.
           05  WS-CP-STATUS            PIC X(01) VALUE SPACES.
       01  WS-CLOSE-KEY                PIC X(30) VALUE SPACES.
       01  WS-CLOSE-KEY-R REDEFINES WS-CLOSE-KEY.
           05  WS-CK-CHAR OCCURS 30 TIMES PIC X(01).
       01  WS-CLOSE-CTL.
           05  WS-CC-PTR               PIC 9(03) VALUE 1.
           05  WS-CC-LEN               PIC 9(03) VALUE 0.
      *****************************************************************
      * THE CLOSE RECORD.  ONE PER PERIOD PER LEDGER COMPANY.  IT     *
      * CARRIES THE FIGURES THE LEDGER TAKES AND THE EVIDENCE THAT THE*
      * CYCLE BALANCED WHEN THEY WERE TAKEN.                          *
      *****************************************************************
       01  WS-CLOSE-RECORD.
           05  WS-CR-KEY               PIC X(30) VALUE SPACES.
           05  WS-CR-PERIOD            PIC 9(06) VALUE 0.
           05  WS-CR-CLOSE-YYDDD       PIC 9(05) VALUE 0.
           05  WS-CR-INVOICES          PIC 9(09) VALUE 0.
           05  WS-CR-HELD              PIC 9(09) VALUE 0.
           05  WS-CR-CANCELLED         PIC 9(09) VALUE 0.
           05  WS-CR-BILLED            PIC S9(15)V9(02) VALUE 0.
           05  WS-CR-TAX               PIC S9(13)V9(02) VALUE 0.
           05  WS-CR-ADJUSTMENTS       PIC S9(13)V9(02) VALUE 0.
           05  WS-CR-SETTLEMENT        PIC S9(13)V9(02) VALUE 0.
           05  WS-CR-INTERSTATE        PIC S9(15)V9(02) VALUE 0.
           05  WS-CR-INTRASTATE        PIC S9(15)V9(02) VALUE 0.
           05  WS-CR-LOCAL             PIC S9(15)V9(02) VALUE 0.
           05  WS-CR-BAL-IND           PIC X(01) VALUE SPACES.
           05  WS-CR-SIGN-OFF          PIC X(08) VALUE SPACES.
           05  WS-CR-FILLER            PIC X(59) VALUE SPACES.
       01  WS-CLOSE-RECORD-K REDEFINES WS-CLOSE-RECORD.
           05  WS-CRK-KEY              PIC X(36).
           05  WS-CRK-REST             PIC X(164).
      *****************************************************************
      * THE LEDGER LINE TABLE.  THE FIGURES THE GENERAL LEDGER TAKES, *
      * IN THE ORDER THE LEDGER INTERFACE EXPECTS THEM.               *
      *****************************************************************
       01  WS-LEDGER-TABLE.
           05  FILLER PIC X(34) VALUE
               '4100ACCESS REVENUE INTERSTATE     '.
           05  FILLER PIC X(34) VALUE
               '4110ACCESS REVENUE INTRASTATE     '.
           05  FILLER PIC X(34) VALUE
               '4120ACCESS REVENUE LOCAL          '.
           05  FILLER PIC X(34) VALUE
               '4200RECURRING CHARGE REVENUE      '.
           05  FILLER PIC X(34) VALUE
               '4210NON RECURRING CHARGE REVENUE  '.
           05  FILLER PIC X(34) VALUE
               '4300INTER CARRIER SETTLEMENT      '.
           05  FILLER PIC X(34) VALUE
               '4400BILLING ADJUSTMENTS           '.
           05  FILLER PIC X(34) VALUE
               '4410PRIOR PERIOD RESTATEMENT      '.
           05  FILLER PIC X(34) VALUE
               '2100TAXES AND SURCHARGES PAYABLE  '.
           05  FILLER PIC X(34) VALUE
               '1200ACCOUNTS RECEIVABLE           '.
       01  WS-LEDGER-TABLE-R REDEFINES WS-LEDGER-TABLE.
           05  WS-LG-ENTRY OCCURS 10 TIMES INDEXED BY WS-LG-X.
               10  WS-LG-ACCOUNT       PIC X(04).
               10  WS-LG-NAME          PIC X(30).
       01  WS-LEDGER-AMOUNTS.
           05  WS-LA-AMOUNT OCCURS 10 TIMES
                                       PIC S9(15)V9(02) COMP-3.
      *****************************************************************
      * CONTROL EVIDENCE.  THE CLOSE IS ONLY VALID IF THE CYCLE THAT  *
      * PRODUCED THE FIGURES BALANCED.  THE CONTROL FILE IS READ AGAIN*
      * HERE RATHER THAN TRUSTING THE BALANCING REPORT.               *
      *****************************************************************
       01  WS-CTL-EVIDENCE.
           05  WS-CE-PROCESSES         PIC S9(05) COMP-3 VALUE 0.
           05  WS-CE-OUT-OF-BAL        PIC S9(05) COMP-3 VALUE 0.
           05  WS-CE-RC-NONZERO        PIC S9(05) COMP-3 VALUE 0.
       01  WS-RUN-TOTALS.
           05  WS-RT-INVOICES          PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-FINAL             PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-HELD              PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-CANCELLED         PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-BILLED            PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-RT-TAX               PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-RT-LEDGER-CHECK      PIC S9(15)V9(02) COMP-3 VALUE 0.
           05  WS-RT-LEDGER-DIFF       PIC S9(15)V9(02) COMP-3 VALUE 0.
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
                       CTL-IN-FILE
                       PARM-FILE
           OPEN OUTPUT CLS-OUT-FILE
                       CLOSE-MST-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 7711 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-BHDRIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 7712 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-CTLIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 7713 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-CLSOUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 7714 TO WS-AB-CODE
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
           PERFORM P5500-CLEAR-LEDGER THRU P5500-EXIT
               VARYING WS-SUB1 FROM 1 BY 1
               UNTIL WS-SUB1 > 10.
           PERFORM P4000-READ-CONTROL THRU P4000-EXIT.
           PERFORM P6000-HEADING THRU P6000-EXIT.
           DISPLAY WS-PGM-NAME ' STARTED - RUN ' WS-RUN-ID.
           DISPLAY '  CLOSE PERIOD  ' WS-PE-CLOSE-PERIOD.
           DISPLAY '  LEDGER CO     ' WS-PE-LEDGER-CO.
           DISPLAY '  SIGN OFF      ' WS-PE-SIGN-OFF.

       P1000-EXIT.
           EXIT.

       P1100-READ-PARM.
      * THE SYSIN CARD CARRIES THE VALUES THE SCHEDULER SUBSTITUTED INTO
      * THE JCL AT SUBMISSION TIME.  THERE ARE NO DEFAULTS - AN ABSENT
      * CARD IS A FATAL ERROR, NOT A DEFAULTED RUN.
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
      * EDIT THE CONTROL CARD.  EVERY FIELD IS MANDATORY.  THE 1989 CARD
      * FORMAT IS STILL ACCEPTED VIA THE WS-PARM-OLD REDEFINE.
      * THE CLOSE PERIOD, THE LEDGER COMPANY AND THE SIGN OFF
      * INITIALS ARE ALL SUPPLIED BY THE SCHEDULER FROM THE CLOSE
      * CALENDAR.  NONE OF THEM HAS A DEFAULT - THE STEP WILL NOT
      * RUN WITHOUT A NAMED PERSON AGAINST THE CLOSE.
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
           IF WS-PE-CLOSE-PERIOD NOT NUMERIC
               MOVE 7721 TO WS-AB-CODE
               MOVE 'CLOSE PERIOD NOT NUMERIC' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PE-LEDGER-CO = SPACES
               MOVE 7722 TO WS-AB-CODE
               MOVE 'LEDGER COMPANY NOT SUPPLIED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PE-SIGN-OFF = SPACES
               MOVE 7723 TO WS-AB-CODE
               MOVE 'SIGN OFF INITIALS NOT SUPPLIED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PE-CLOSE-YYDDD NOT NUMERIC
               MOVE WS-PC-CYCLE TO WS-PE-CLOSE-YYDDD.

       P1200-EXIT.
           EXIT.

      *****************************************************************
      * S200-MAIN PROCESSING                                          *
      * ONE PASS OF THE FINAL INVOICE HEADER FILE ACCUMULATING THE    *
      * LEDGER LINES, THEN A PASS OF THE CONTROL FILE TO PROVE THAT THE*
      * CYCLE THAT PRODUCED THEM BALANCED.                            *
      *****************************************************************
       S200-MAIN-PROCESS SECTION.

       P2000-PROCESS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ-HEADER THRU P2100-EXIT.
           IF WS-HDR-EOF
               MOVE 'Y' TO WS-EOF-SW
               GO TO P2000-EXIT.
           ADD 1 TO WS-READ-CNT.
           ADD 1 TO WS-RT-INVOICES.
           MOVE BH-BAN TO WS-RESTART-KEY.
           IF BH-BILL-PERIOD NOT = WS-PE-CLOSE-PERIOD
               ADD 1 TO WS-CFWD-CNT
               GO TO P2000-EXIT.
           PERFORM P3000-STATUS-COUNT THRU P3000-EXIT.
           IF NOT BH-FINAL
               GO TO P2000-EXIT.
           PERFORM P3200-ACCUM-LEDGER THRU P3200-EXIT.
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
               MOVE 7701 TO WS-AB-CODE
               MOVE 'BILL HEADER READ ERROR' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.

       P2100-EXIT.
           EXIT.

      *****************************************************************
      * S300-LEDGER ACCUMULATION                                      *
      *****************************************************************
       S300-LEDGER SECTION.

       P3000-STATUS-COUNT.
           MOVE 'P3000-STATUS-COUNT' TO WS-PARA-NAME.
           IF BH-FINAL
               ADD 1 TO WS-RT-FINAL
               GO TO P3000-EXIT.
           IF BH-HELD
               ADD 1 TO WS-RT-HELD
               MOVE 'N' TO WS-CLOSE-OK-SW
               GO TO P3000-EXIT.
           IF BH-CANCELLED
               ADD 1 TO WS-RT-CANCELLED
               GO TO P3000-EXIT.
           MOVE 'N' TO WS-CLOSE-OK-SW.

       P3000-EXIT.
           EXIT.

       P3200-ACCUM-LEDGER.
      * POST THE INVOICE INTO THE TEN LEDGER LINES.  THE THREE
      * JURISDICTIONAL LINES TOGETHER CARRY THE ACCESS REVENUE; THE
      * RECEIVABLE LINE CARRIES THE WHOLE INVOICE.  THE TEN LINES MUST
      * NET TO ZERO AND THAT IS PROVED AT THE END OF THE RUN.
           MOVE 'P3200-ACCUM-LEDGER' TO WS-PARA-NAME.
           ADD BH-INTERSTATE-AMT   TO WS-LA-AMOUNT (1).
           ADD BH-INTRASTATE-AMT   TO WS-LA-AMOUNT (2).
           ADD BH-LOCAL-AMT        TO WS-LA-AMOUNT (3).
           ADD BH-CURR-RECURRING   TO WS-LA-AMOUNT (4).
           ADD BH-CURR-NONRECUR    TO WS-LA-AMOUNT (5).
           ADD BH-SETTLEMENT-NET   TO WS-LA-AMOUNT (6).
           ADD BH-ADJUSTMENTS      TO WS-LA-AMOUNT (7).
           ADD BH-RESTATEMENT      TO WS-LA-AMOUNT (8).
           ADD BH-TAX              TO WS-LA-AMOUNT (9).
           ADD BH-TOTAL-DUE        TO WS-LA-AMOUNT (10).
           ADD BH-TOTAL-DUE        TO WS-RT-BILLED.
           ADD BH-TAX              TO WS-RT-TAX.
           ADD BH-TOTAL-DUE        TO WS-ACC-AMOUNT.

       P3200-EXIT.
           EXIT.

       P3400-PROVE-LEDGER.
      * THE LEDGER PROOF.  THE NINE REVENUE AND LIABILITY LINES MUST
      * ADD BACK TO THE RECEIVABLE LINE.  A DIFFERENCE MEANS THE
      * PERIOD CANNOT BE CLOSED.
           MOVE 'P3400-PROVE-LEDGER' TO WS-PARA-NAME.
           COMPUTE WS-RT-LEDGER-CHECK =
                   WS-LA-AMOUNT (1) + WS-LA-AMOUNT (2)
                 + WS-LA-AMOUNT (3) + WS-LA-AMOUNT (4)
                 + WS-LA-AMOUNT (5) + WS-LA-AMOUNT (6)
                 + WS-LA-AMOUNT (7) + WS-LA-AMOUNT (8)
                 + WS-LA-AMOUNT (9).
           COMPUTE WS-RT-LEDGER-DIFF =
                   WS-LA-AMOUNT (10) - WS-RT-LEDGER-CHECK.
           IF WS-RT-LEDGER-DIFF NOT = ZERO
               MOVE 'N' TO WS-CLOSE-OK-SW
               MOVE EC-OUT-OF-BALANCE TO WS-ERR-CODE
               MOVE 'E' TO WS-ERR-SEVERITY
               MOVE WS-CLOSE-RECORD TO WS-EW-DATA
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               MOVE SPACES TO WS-ERR-CODE.

       P3400-EXIT.
           EXIT.

      *****************************************************************
      * S400-CONTROL EVIDENCE AND THE CLOSE RECORD                    *
      *****************************************************************
       S400-CLOSE SECTION.

       P4000-READ-CONTROL.
      * READ THE CONTROL FILE AND COUNT ANY PROCESS THAT DID NOT
      * BALANCE.  THE CLOSE IS NOT PERMITTED WHILE ANY PROCESS IN THE
      * CYCLE IS OUT OF BALANCE.
           MOVE 'P4000-READ-CONTROL' TO WS-PARA-NAME.
           PERFORM P4010-ONE-CONTROL THRU P4010-EXIT
               UNTIL WS-CTL-EOF.
           IF WS-CE-OUT-OF-BAL > ZERO
               MOVE 'N' TO WS-CLOSE-OK-SW.

       P4000-EXIT.
           EXIT.

       P4010-ONE-CONTROL.
           READ CTL-IN-FILE INTO CABS-CONTROL-RECORD
               AT END
                   MOVE 'Y' TO WS-CTL-EOF-SW
                   GO TO P4010-EXIT.
           IF CT-BILL-PERIOD NOT = WS-PE-CLOSE-PERIOD
               GO TO P4010-EXIT.
           ADD 1 TO WS-CE-PROCESSES.
           IF CT-OUT-OF-BAL
               ADD 1 TO WS-CE-OUT-OF-BAL.
           IF CT-RC NOT = ZERO
               ADD 1 TO WS-CE-RC-NONZERO.

       P4010-EXIT.
           EXIT.

       P4200-BUILD-CLOSE-KEY.
      * ASSEMBLE THE THIRTY BYTE CLOSE KEY.  THE PIECES ARE THE FIXED
      * PREFIX, THE LEDGER COMPANY, THE BILL PERIOD, THE CLOSE CYCLE
      * DATE, A SEQUENCE AND THE STATUS BYTE.  THE LEDGER INTERFACE
      * PARSES THIS KEY BY POSITION.
           MOVE 'P4200-BUILD-CLOSE-KEY' TO WS-PARA-NAME.
           MOVE WS-PE-LEDGER-CO   TO WS-CP-LEDGER-CO.
           MOVE WS-PE-CLOSE-PERIOD TO WS-CP-PERIOD.
           MOVE WS-PE-CLOSE-YYDDD TO WS-CP-CYCLE.
           MOVE 1                 TO WS-CP-SEQ.
           IF WS-CLOSE-OK
               MOVE 'C' TO WS-CP-STATUS
           ELSE
               MOVE 'P' TO WS-CP-STATUS.
           MOVE SPACES TO WS-CLOSE-KEY.
           MOVE 1 TO WS-CC-PTR.
           STRING WS-CP-PREFIX     DELIMITED BY SIZE
                  WS-CP-LEDGER-CO  DELIMITED BY SIZE
                  WS-CP-PERIOD     DELIMITED BY SIZE
                  WS-CP-CYCLE      DELIMITED BY SIZE
                  WS-CP-SEQ        DELIMITED BY SIZE
                  WS-CP-STATUS     DELIMITED BY SIZE
                  INTO WS-CLOSE-KEY
                  WITH POINTER WS-CC-PTR
               ON OVERFLOW
                  MOVE 7702 TO WS-AB-CODE
                  MOVE 'CLOSE KEY OVERFLOW' TO WS-AB-TEXT
                  PERFORM P9500-ABEND THRU P9500-EXIT.
           COMPUTE WS-CC-LEN = WS-CC-PTR - 1.

       P4200-EXIT.
           EXIT.

       P4400-WRITE-CLOSE.
           MOVE 'P4400-WRITE-CLOSE' TO WS-PARA-NAME.
           MOVE SPACES TO WS-CLOSE-RECORD.
           MOVE WS-CLOSE-KEY        TO WS-CR-KEY.
           MOVE WS-PE-CLOSE-PERIOD  TO WS-CR-PERIOD.
           MOVE WS-PE-CLOSE-YYDDD   TO WS-CR-CLOSE-YYDDD.
           MOVE WS-RT-FINAL         TO WS-CR-INVOICES.
           MOVE WS-RT-HELD          TO WS-CR-HELD.
           MOVE WS-RT-CANCELLED     TO WS-CR-CANCELLED.
           MOVE WS-RT-BILLED        TO WS-CR-BILLED.
           MOVE WS-RT-TAX           TO WS-CR-TAX.
           MOVE WS-LA-AMOUNT (7)    TO WS-CR-ADJUSTMENTS.
           MOVE WS-LA-AMOUNT (6)    TO WS-CR-SETTLEMENT.
           MOVE WS-LA-AMOUNT (1)    TO WS-CR-INTERSTATE.
           MOVE WS-LA-AMOUNT (2)    TO WS-CR-INTRASTATE.
           MOVE WS-LA-AMOUNT (3)    TO WS-CR-LOCAL.
           MOVE WS-PE-SIGN-OFF      TO WS-CR-SIGN-OFF.
           IF WS-CLOSE-OK
               MOVE 'B' TO WS-CR-BAL-IND
           ELSE
               MOVE 'O' TO WS-CR-BAL-IND.
           WRITE CLOSE-RECORD FROM WS-CLOSE-RECORD.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 7703 TO WS-AB-CODE
               MOVE 'CLOSE RECORD WRITE FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-WRITE-CNT.

       P4400-EXIT.
           EXIT.

      *****************************************************************
      * S500-THE PRINTED CLOSE REPORT                                 *
      *****************************************************************
       S500-REPORT SECTION.

       P5000-PRINT-CLOSE.
           MOVE 'P5000-PRINT-CLOSE' TO WS-PARA-NAME.
           PERFORM P3400-PROVE-LEDGER THRU P3400-EXIT.
           PERFORM P4200-BUILD-CLOSE-KEY THRU P4200-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '4' TO PC-CC.
           MOVE 'MONTH END CLOSE - LEDGER POSTING' TO PC-TEXT.
           PERFORM P5400-WRITE-LINE THRU P5400-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '0' TO PC-CC.
           MOVE 'CLOSE KEY' TO PC-COL-001-020.
           MOVE WS-CLOSE-KEY TO PC-COL-021-060.
           PERFORM P5400-WRITE-LINE THRU P5400-EXIT.
           PERFORM P5100-LEDGER-LINES THRU P5100-EXIT.
           PERFORM P5200-EVIDENCE THRU P5200-EXIT.
           PERFORM P5300-VERDICT THRU P5300-EXIT.
           PERFORM P4400-WRITE-CLOSE THRU P4400-EXIT.

       P5000-EXIT.
           EXIT.

       P5100-LEDGER-LINES.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '0' TO PC-CC.
           MOVE 'ACCOUNT  DESCRIPTION' TO PC-COL-001-020.
           MOVE 'AMOUNT' TO PC-COL-091-132.
           PERFORM P5400-WRITE-LINE THRU P5400-EXIT.
           PERFORM P5110-ONE-LEDGER THRU P5110-EXIT
               VARYING WS-LG-X FROM 1 BY 1
               UNTIL WS-LG-X > 10.

       P5100-EXIT.
           EXIT.

       P5110-ONE-LEDGER.
           SET WS-SUB1 TO WS-LG-X.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           IF WS-LG-X = 10
               MOVE '0' TO PC-CC.
           MOVE WS-LG-ACCOUNT (WS-LG-X) TO PC-COL-001-020.
           MOVE WS-LG-NAME (WS-LG-X)    TO PC-COL-021-060.
           MOVE WS-LA-AMOUNT (WS-SUB1)  TO WS-ED-MONEY.
           MOVE WS-ED-MONEY             TO PC-COL-091-132.
           PERFORM P5400-WRITE-LINE THRU P5400-EXIT.

       P5110-EXIT.
           EXIT.

       P5200-EVIDENCE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '4' TO PC-CC.
           MOVE 'CONTROL EVIDENCE FOR THE PERIOD' TO PC-TEXT.
           PERFORM P5400-WRITE-LINE THRU P5400-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '0' TO PC-CC.
           MOVE 'PROCESSES IN PERIOD' TO PC-COL-001-020.
           MOVE WS-CE-PROCESSES TO WS-ED-COUNT.
           MOVE WS-ED-COUNT TO PC-COL-021-060.
           PERFORM P5400-WRITE-LINE THRU P5400-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'OUT OF BALANCE' TO PC-COL-001-020.
           MOVE WS-CE-OUT-OF-BAL TO WS-ED-COUNT.
           MOVE WS-ED-COUNT TO PC-COL-021-060.
           PERFORM P5400-WRITE-LINE THRU P5400-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'INVOICES STILL HELD' TO PC-COL-001-020.
           MOVE WS-RT-HELD TO WS-ED-COUNT.
           MOVE WS-ED-COUNT TO PC-COL-021-060.
           PERFORM P5400-WRITE-LINE THRU P5400-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LEDGER DIFFERENCE' TO PC-COL-001-020.
           MOVE WS-RT-LEDGER-DIFF TO WS-ED-MONEY.
           MOVE WS-ED-MONEY TO PC-COL-021-060.
           PERFORM P5400-WRITE-LINE THRU P5400-EXIT.

       P5200-EXIT.
           EXIT.

       P5300-VERDICT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '-' TO PC-CC.
           IF WS-CLOSE-OK
               MOVE 'PERIOD MAY BE CLOSED - POST TO THE LEDGER'
                                       TO PC-TEXT
           ELSE
               MOVE 'PERIOD MAY NOT BE CLOSED - SEE EVIDENCE PAGE'
                                       TO PC-TEXT
               MOVE 0008 TO WS-RETURN-CODE.
           PERFORM P5400-WRITE-LINE THRU P5400-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '0' TO PC-CC.
           MOVE 'AUTHORISED BY' TO PC-COL-001-020.
           MOVE WS-PE-SIGN-OFF TO PC-COL-021-060.
           PERFORM P5400-WRITE-LINE THRU P5400-EXIT.

       P5300-EXIT.
           EXIT.

       P5400-WRITE-LINE.
           WRITE CLS-RECORD FROM CABS-PRINT-LINE.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 7704 TO WS-AB-CODE
               MOVE 'CLOSE REPORT WRITE FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.

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
           MOVE 'CABRPT08  MONTH END CLOSE REPORT'
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
           MOVE 'LEDGER ACCOUNT      DESCRIPTION           AMOUNT'
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
           MOVE 635                    TO CT-STEP-SEQ.
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
           PERFORM P5000-PRINT-CLOSE THRU P5000-EXIT.
           DISPLAY 'INVOICES EXAMINED ' WS-RT-INVOICES.
           DISPLAY 'FINAL INVOICES    ' WS-RT-FINAL.
           DISPLAY 'STILL HELD        ' WS-RT-HELD.
           DISPLAY 'CANCELLED         ' WS-RT-CANCELLED.
           DISPLAY 'BILLED VALUE      ' WS-RT-BILLED.
           DISPLAY 'TAX VALUE         ' WS-RT-TAX.
           DISPLAY 'LEDGER DIFFERENCE ' WS-RT-LEDGER-DIFF.
           DISPLAY 'CLOSE PERMITTED   ' WS-CLOSE-OK-SW.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE BHDR-IN-FILE
                 CTL-IN-FILE
                 CLS-OUT-FILE
                 CLOSE-MST-FILE
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

       P5500-CLEAR-LEDGER.
           MOVE ZERO TO WS-LA-AMOUNT (WS-SUB1).

       P5500-EXIT.
           EXIT.
