      *****************************************************************
      * CABBIL03 - BILL SECTION SEQUENCING AND SUPPRESSION            *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               BDTLIN  TELCABS.CABS.BILLDTL(0)           CABSBILL*
      *               SYSIN   INSTREAM CONTROL CARD             (LOCAL)*
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               BDTLSEQ TELCABS.CABS.BILLDTL.SEQ(+1)      CABSBILL*
      *               REPORT  SYSOUT PRINT                      CABSPRNT*
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-SUMMARISED + CT-CARRIED-FWD        *
      *               CT-WRITTEN = CT-SUMMARISED AFTER THE SORT RETURNS*
      * RESTART     : FULL RERUN - THE SORT IS INTERNAL AND NOT CHECKPOINTED*
      * REVISION HISTORY                                              *
      *   V1.00  1988-06-30  D.OKONKWO    INITIAL RELEASE - EXTERNAL SORT STEP*
      *                      WITH A SEPARATE RENUMBER PROGRAM         *
      *   V1.05  1991-02-14  M.J.FERRARO  SORT PULLED INSIDE THE PROGRAM TO*
      *                      SAVE A PASS OF THE DETAIL FILE           *
      *   V1.09  1994-09-08  L.HARGREAVES SECTION PRINT ORDER TABLE ADDED -*
      *                      TAXES MOVED TO THE END OF THE BILL       *
      *   V1.14  1997-11-19  J.M.CASTILLO ZERO VALUE SUPPRESSION MADE OPTIONAL*
      *                      ON THE CONTROL CARD                      *
      *   V2.00  2001-05-23  P.NAIR       CHARGE SECTIONS EXEMPTED FROM ZERO*
      *                      SUPPRESSION - TARIFF REQUIRES THE        *
      *                      ELEMENT TO SHOW AT NO CHARGE             *
      *   V2.03  2006-07-12  T.VANCE      SECTION SEQUENCE NUMBER ADDED FOR*
      *                      THE CD-ROM BILL FORMAT                   *
      *   V2.06  2017-03-10  G.PRZYBYLSKI RECOMPILE FOR THE 1204 BYTE DETAIL*
      *                      RECORD - SORT WORK SPACE RAISED          *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABBIL03.
       AUTHOR. TELCABS APPLICATIONS - BILLING TEAM.
      *****************************************************************
      * SEQUENCES THE BILL DETAIL INTO PRINT ORDER AND RENUMBERS THE  *
      * LINES WITHIN EACH ACCOUNT.  THE ELIGIBILITY RULE AND THE      *
      * RENUMBERING RULE BOTH LIVE INSIDE THE SORT PROCEDURES.        *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BILL-DTL-IN ASSIGN TO UT-S-BDTLIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT BILL-DTL-OUT ASSIGN TO UT-S-BDTLSEQ
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
      * BDTLIN - BILL DETAIL FROM CABBIL02.  VARIABLE LENGTH.         *
      *****************************************************************
       FD  BILL-DTL-IN
           RECORDING MODE IS V
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD IS VARYING IN SIZE FROM 108 TO 1647
               CHARACTERS DEPENDING ON BD-ELEM-CNT.
       COPY CABSBILL.
      *****************************************************************
      * BDTLSEQ - THE SAME RECORDS IN PRINT ORDER, RENUMBERED.        *
      *****************************************************************
       FD  BILL-DTL-OUT
           RECORDING MODE IS V
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD IS VARYING IN SIZE FROM 108 TO 1647
               CHARACTERS DEPENDING ON BD-ELEM-CNT.
       01  BILL-DTL-OUT-REC             PIC X(1647).
      *****************************************************************
      * THE INTERNAL SORT FILE.  FIXED 1204 SO THAT THE LONGEST       *
      * DETAIL RECORD FITS WHOLE.  THE FOUR PART KEY IN FRONT OF      *
      * THE IMAGE IS BUILT IN THE INPUT PROCEDURE.                    *
      *****************************************************************
       SD  SORT-FILE
           RECORD CONTAINS 1232 CHARACTERS.
       01  SORT-RECORD.
           05  SR-KEY.
               10  SR-BAN              PIC X(13).
               10  SR-PERIOD           PIC 9(06).
               10  SR-SECT-ORDER       PIC 9(02).
               10  SR-LINE-SEQ         PIC 9(07).
           05  SR-IMAGE                PIC X(1204).
       01  SORT-RECORD-R REDEFINES SORT-RECORD.
           05  SR-FULL-KEY             PIC X(28).
           05  SR-BODY                 PIC X(1204).
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
           05  WS-PGM-NAME             PIC X(08) VALUE 'CABBIL03'.
           05  WS-PGM-VERSION          PIC X(05) VALUE 'V2.06'.
           05  WS-PGM-APPL             PIC X(04) VALUE 'CABS'.
           05  WS-PGM-COMPILE          PIC X(08) VALUE '20170310'.
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
           05  WS-PE-SUPPRESS-ZERO     PIC X(01).
           05  WS-PE-RENUMBER-SW       PIC X(01).
           05  WS-PE-SECT-ORDER-SW     PIC X(01).
           05  WS-PE-MIN-LINE-AMT      PIC 9(05)V9(02).
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
           05  WS-DTL-EOF-SW           PIC X(01) VALUE 'N'.
               88  WS-DTL-EOF          VALUE 'Y'.
           05  WS-SORT-EOF-SW          PIC X(01) VALUE 'N'.
               88  WS-SORT-EOF         VALUE 'Y'.
           05  WS-SORT-DONE-SW         PIC X(01) VALUE 'N'.
               88  WS-SORT-DONE        VALUE 'Y'.
           05  WS-RELEASE-SW           PIC X(01) VALUE 'Y'.
               88  WS-RELEASE-IT       VALUE 'Y'.
      *****************************************************************
      * THE SECTION PRINT ORDER TABLE.  THE ORDER A SECTION PRINTS IN IS*
      * NOT THE ORDER ITS CODE SORTS IN - TAXES PRINT LAST BUT SORT IN*
      * THE MIDDLE.  THE TWO DIGIT ORDER NUMBER BELOW IS WHAT THE SORT*
      * KEY CARRIES AND IT EXISTS NOWHERE ELSE IN THE ESTATE.         *
      *****************************************************************
       01  WS-SECT-ORDER-TABLE.
           05  FILLER PIC X(04) VALUE 'U101'.
           05  FILLER PIC X(04) VALUE 'U202'.
           05  FILLER PIC X(04) VALUE 'U303'.
           05  FILLER PIC X(04) VALUE 'C104'.
           05  FILLER PIC X(04) VALUE 'C305'.
           05  FILLER PIC X(04) VALUE 'C406'.
           05  FILLER PIC X(04) VALUE 'S107'.
           05  FILLER PIC X(04) VALUE 'S208'.
           05  FILLER PIC X(04) VALUE 'C209'.
           05  FILLER PIC X(04) VALUE 'A110'.
           05  FILLER PIC X(04) VALUE 'T111'.
           05  FILLER PIC X(04) VALUE 'Z112'.
       01  WS-SECT-ORDER-R REDEFINES WS-SECT-ORDER-TABLE.
           05  WS-SO-ENTRY OCCURS 12 TIMES INDEXED BY WS-SO-X.
               10  WS-SO-CODE          PIC X(02).
               10  WS-SO-ORDER         PIC 9(02).
      *****************************************************************
      * SECTION CLASSIFICATION.  THE RANGES BELOW WERE FILED SEPARATELY*
      * IN 1991 AND 1997 AND HAVE OVERLAPPED SINCE - U2 AND U3 SATISFY*
      * BOTH THE USAGE AND THE CHARGE CONDITION.  BOTH CONDITIONS ARE *
      * TESTED IN P4200 AND THE FIRST ONE TESTED WINS.                *
      * CONDITION NAMES FILED WITH THE 1993 EDIT SPECIFICATION.       *
      *****************************************************************
       01  WS-SECT-CLASS-WORK.
           05  WS-SC-SECTION           PIC X(02) VALUE SPACES.
               88  WS-SC-USAGE-SECTION VALUE 'U1' THRU 'U3'.
               88  WS-SC-CHARGE-SECTION VALUE 'U2' THRU 'C4'.
               88  WS-SC-SETTLE-SECTION VALUE 'S1' THRU 'S2'.
               88  WS-SC-ADJUST-SECTION VALUE 'A1'.
               88  WS-SC-TAX-SECTION   VALUE 'T1'.
               88  WS-SC-ANY-BILLABLE  VALUE 'U1' THRU 'T1'.
           05  WS-SC-ORDER             PIC 9(02) VALUE 0.
           05  WS-SC-FOUND-SW          PIC X(01) VALUE 'N'.
               88  WS-SC-FOUND         VALUE 'Y'.
      *****************************************************************
      * RENUMBER WORK.  LINE SEQUENCE IS REISSUED WITHIN AN ACCOUNT SO*
      * THAT THE PRINTED BILL READS 1, 2, 3 WHATEVER THE RATING PROCESS*
      * NUMBERED IT.  THE ORIGINAL SEQUENCE IS LOST AT THIS POINT.    *
      *****************************************************************
       01  WS-RENUM-WORK.
           05  WS-RN-BAN               PIC X(13) VALUE SPACES.
           05  WS-RN-PERIOD            PIC 9(06) VALUE 0.
           05  WS-RN-SEQ               PIC 9(07) VALUE 0.
           05  WS-RN-SECT-SEQ          PIC 9(07) VALUE 0.
           05  WS-RN-LAST-SECT         PIC X(02) VALUE SPACES.
       01  WS-ELEM-WALK.
           05  WS-EW-COUNT             PIC 9(03) VALUE 0.
           05  WS-EW-AMOUNT            PIC S9(13)V9(05) COMP-3 VALUE 0.
           05  WS-EW-QTY               PIC S9(13)V9(02) COMP-3 VALUE 0.
           05  WS-EW-ZERO-ELEMS        PIC 9(03) VALUE 0.
       01  WS-RUN-TOTALS.
           05  WS-RT-RELEASED          PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-DROPPED           PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-RETURNED          PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-RENUMBERED        PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-SECTIONS          PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-AMOUNT            PIC S9(15)V9(05) COMP-3 VALUE 0.
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
           OPEN INPUT  BILL-DTL-IN
                       PARM-FILE
           OPEN OUTPUT BILL-DTL-OUT
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 4040 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-BDTLIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 4044 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-BDTLSEQ' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-TABLE NOT = '00'
               MOVE 4041 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-SYSIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 4042 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-CTLOUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 4043 TO WS-AB-CODE
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
           PERFORM P6000-HEADING THRU P6000-EXIT.
           DISPLAY WS-PGM-NAME ' STARTED - RUN ' WS-RUN-ID.
           DISPLAY '  SUPPRESS ZERO ' WS-PE-SUPPRESS-ZERO.
           DISPLAY '  RENUMBER      ' WS-PE-RENUMBER-SW.

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
      * THE SUPPRESSION SWITCH IS SET BY THE SCHEDULER.  A REGULATORY
      * RE-RUN SETS IT TO N SO THAT EVERY LINE APPEARS.
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
           IF WS-PE-SUPPRESS-ZERO NOT = 'N'
               MOVE 'Y' TO WS-PE-SUPPRESS-ZERO.
           IF WS-PE-RENUMBER-SW NOT = 'N'
               MOVE 'Y' TO WS-PE-RENUMBER-SW.
           IF WS-PE-MIN-LINE-AMT NOT NUMERIC
               MOVE ZERO TO WS-PE-MIN-LINE-AMT.

       P1200-EXIT.
           EXIT.

      *****************************************************************
      * S200-MAIN PROCESSING                                          *
      * THE WHOLE OF THIS PROGRAM IS ONE INTERNAL SORT.  THE ELIGIBILITY*
      * AND SUPPRESSION RULES LIVE IN THE INPUT PROCEDURE AND THE     *
      * RENUMBERING LIVES IN THE OUTPUT PROCEDURE.  NEITHER RULE IS   *
      * VISIBLE FROM THE JCL OR FROM ANY SORT CONTROL CARD.           *
      * INTERNAL SORT APPROVED IN CR-4118 TO SAVE A JOB STEP.         *
      *****************************************************************
       S200-MAIN-PROCESS SECTION.

       P2000-PROCESS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           IF WS-SORT-DONE
               MOVE 'Y' TO WS-EOF-SW
               GO TO P2000-EXIT.
           SORT SORT-FILE
               ON ASCENDING KEY SR-BAN
                                SR-PERIOD
                                SR-SECT-ORDER
                                SR-LINE-SEQ
               INPUT PROCEDURE IS S310-SORT-INPUT
               OUTPUT PROCEDURE IS S320-SORT-OUTPUT.
           IF SORT-RETURN NOT = ZERO
               MOVE 4301 TO WS-AB-CODE
               MOVE 'INTERNAL SORT FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           MOVE 'Y' TO WS-SORT-DONE-SW.
           MOVE 'Y' TO WS-EOF-SW.

       P2000-EXIT.
           EXIT.

      *****************************************************************
      * S310-SORT INPUT PROCEDURE                                     *
      * READ THE BILL DETAIL FILE AND DECIDE WHAT GOES INTO THE SORT. *
      * A LINE THAT IS DROPPED HERE NEVER APPEARS ON THE BILL AND     *
      * NOTHING DOWNSTREAM CAN TELL THAT IT EXISTED.                  *
      *****************************************************************
       S310-SORT-INPUT SECTION.

       P3100-INPUT-DRIVER.
           MOVE 'P3100-INPUT-DRIVER' TO WS-PARA-NAME.
           PERFORM P3200-READ-DETAIL THRU P3200-EXIT
               UNTIL WS-DTL-EOF.
           GO TO P3100-EXIT.

       P3100-EXIT.
           EXIT.

       P3200-READ-DETAIL.
           READ BILL-DTL-IN
               AT END
                   MOVE 'Y' TO WS-DTL-EOF-SW
                   GO TO P3200-EXIT.
           ADD 1 TO WS-READ-CNT.
           MOVE BD-BAN TO WS-RESTART-KEY.
           MOVE 'Y' TO WS-RELEASE-SW.
           PERFORM P4000-WALK-ELEMENTS THRU P4000-EXIT.
           PERFORM P4100-SECTION-ORDER THRU P4100-EXIT.
           PERFORM P4200-ELIGIBILITY THRU P4200-EXIT.
           IF NOT WS-RELEASE-IT
               ADD 1 TO WS-RT-DROPPED
               ADD 1 TO WS-CFWD-CNT
               GO TO P3200-EXIT.
           PERFORM P4300-BUILD-SORT-REC THRU P4300-EXIT.
           RELEASE SORT-RECORD.
           ADD 1 TO WS-RT-RELEASED.
           ADD 1 TO WS-SUMM-CNT.

       P3200-EXIT.
           EXIT.

       P4000-WALK-ELEMENTS.
      * WALK THE OCCURS DEPENDING ON AREA.  THE NUMBER OF ELEMENTS IS ON
      * THE RECORD AND THE RECORD IS ONLY AS LONG AS THAT COUNT SAYS.
      * WALKING PAST BD-ELEM-CNT READS STORAGE THAT WAS NEVER WRITTEN.
      * RECORD LENGTHS ARE HELD IN THE DATASET REGISTER.
           MOVE 'P4000-WALK-ELEMENTS' TO WS-PARA-NAME.
           MOVE ZERO TO WS-EW-AMOUNT WS-EW-QTY WS-EW-ZERO-ELEMS.
           MOVE BD-ELEM-CNT TO WS-EW-COUNT.
           IF WS-EW-COUNT < 1
               MOVE 1 TO WS-EW-COUNT.
           IF WS-EW-COUNT > 40
               MOVE 40 TO WS-EW-COUNT.
           PERFORM P4010-ONE-ELEMENT THRU P4010-EXIT
               VARYING WS-SUB1 FROM 1 BY 1
               UNTIL WS-SUB1 > WS-EW-COUNT.

       P4000-EXIT.
           EXIT.

       P4010-ONE-ELEMENT.
           SET BD-EX TO WS-SUB1.
           ADD BD-EL-AMOUNT (BD-EX) TO WS-EW-AMOUNT.
           ADD BD-EL-QTY (BD-EX)    TO WS-EW-QTY.
           IF BD-EL-AMOUNT (BD-EX) = ZERO
               ADD 1 TO WS-EW-ZERO-ELEMS.

       P4010-EXIT.
           EXIT.

       P4100-SECTION-ORDER.
      * TRANSLATE THE SECTION CODE INTO ITS PRINT ORDER NUMBER.  A CODE
      * THAT IS NOT IN THE TABLE SORTS LAST UNDER ORDER 99 AND PRINTS
      * IN THE UNCLASSIFIED SECTION.
           MOVE 'P4100-SECTION-ORDER' TO WS-PARA-NAME.
           MOVE 99 TO WS-SC-ORDER.
           MOVE 'N' TO WS-SC-FOUND-SW.
           MOVE BD-SECTION TO WS-SC-SECTION.
           PERFORM P4110-MATCH-ORDER THRU P4110-EXIT
               VARYING WS-SO-X FROM 1 BY 1
               UNTIL WS-SO-X > 12 OR WS-SC-FOUND.

       P4100-EXIT.
           EXIT.

       P4110-MATCH-ORDER.
           IF WS-SO-CODE (WS-SO-X) = WS-SC-SECTION
               MOVE WS-SO-ORDER (WS-SO-X) TO WS-SC-ORDER
               MOVE 'Y' TO WS-SC-FOUND-SW.

       P4110-EXIT.
           EXIT.

       P4200-ELIGIBILITY.
      * THE SUPPRESSION RULE.  A ZERO VALUE USAGE LINE IS DROPPED.  A
      * ZERO VALUE CHARGE LINE IS KEPT BECAUSE THE TARIFF REQUIRES THE
      * ELEMENT TO BE SHOWN EVEN AT NO CHARGE.  THE TWO CONDITIONS
      * OVERLAP AND THE USAGE TEST IS MADE FIRST.
           MOVE 'P4200-ELIGIBILITY' TO WS-PARA-NAME.
           MOVE 'Y' TO WS-RELEASE-SW.
           IF WS-PE-SUPPRESS-ZERO NOT = 'Y'
               GO TO P4200-EXIT.
           IF BD-TOT-ROUNDED NOT = ZERO
               GO TO P4200-EXIT.
           IF WS-SC-USAGE-SECTION
               MOVE 'N' TO WS-RELEASE-SW
               GO TO P4200-EXIT.
           IF WS-SC-CHARGE-SECTION
               GO TO P4200-EXIT.
           IF WS-SC-TAX-SECTION
               GO TO P4200-EXIT.
           IF WS-EW-AMOUNT = ZERO
               MOVE 'N' TO WS-RELEASE-SW.

       P4200-EXIT.
           EXIT.

       P4300-BUILD-SORT-REC.
      * THE SORT RECORD IS A FIXED 1204 BYTE IMAGE OF THE VARIABLE
      * LENGTH DETAIL RECORD WITH A FOUR PART KEY IN FRONT OF IT.  THE
      * MAXIMUM LENGTH IS USED SO THAT NOTHING IS LOST.
           MOVE 'P4300-BUILD-SORT-REC' TO WS-PARA-NAME.
           MOVE SPACES TO SORT-RECORD.
           MOVE BD-BAN                 TO SR-BAN.
           MOVE BD-BILL-PERIOD         TO SR-PERIOD.
           MOVE WS-SC-ORDER            TO SR-SECT-ORDER.
           MOVE BD-LINE-SEQ            TO SR-LINE-SEQ.
           MOVE CABS-BILL-DETAIL       TO SR-IMAGE.
           ADD WS-EW-AMOUNT TO WS-RT-AMOUNT.

       P4300-EXIT.
           EXIT.

      *****************************************************************
      * S320-SORT OUTPUT PROCEDURE                                    *
      * RETURN THE SORTED LINES, RENUMBER THEM WITHIN THE ACCOUNT AND *
      * WRITE THEM BACK OUT AS VARIABLE LENGTH RECORDS.               *
      *****************************************************************
       S320-SORT-OUTPUT SECTION.

       P5100-OUTPUT-DRIVER.
           MOVE 'P5100-OUTPUT-DRIVER' TO WS-PARA-NAME.
           MOVE SPACES TO WS-RN-BAN WS-RN-LAST-SECT.
           MOVE ZERO TO WS-RN-SEQ WS-RN-SECT-SEQ.
           PERFORM P5200-RETURN-ONE THRU P5200-EXIT
               UNTIL WS-SORT-EOF.
           GO TO P5100-EXIT.

       P5100-EXIT.
           EXIT.

       P5200-RETURN-ONE.
           RETURN SORT-FILE
               AT END
                   MOVE 'Y' TO WS-SORT-EOF-SW
                   GO TO P5200-EXIT.
           ADD 1 TO WS-RT-RETURNED.
           MOVE SR-IMAGE TO CABS-BILL-DETAIL.
           PERFORM P5300-RENUMBER THRU P5300-EXIT.
           PERFORM P5400-WRITE-SEQUENCED THRU P5400-EXIT.

       P5200-EXIT.
           EXIT.

       P5300-RENUMBER.
      * LINE SEQUENCE RESTARTS AT ONE FOR EVERY ACCOUNT AND THE SECTION
      * SEQUENCE RESTARTS AT ONE FOR EVERY SECTION WITHIN THE ACCOUNT.
      * THE ORIGINAL RATING SEQUENCE IS NOT KEPT ANYWHERE.
           MOVE 'P5300-RENUMBER' TO WS-PARA-NAME.
           IF WS-PE-RENUMBER-SW NOT = 'Y'
               GO TO P5300-EXIT.
           IF BD-BAN NOT = WS-RN-BAN
               MOVE BD-BAN TO WS-RN-BAN
               MOVE BD-BILL-PERIOD TO WS-RN-PERIOD
               MOVE ZERO TO WS-RN-SEQ
               MOVE SPACES TO WS-RN-LAST-SECT.
           IF BD-SECTION NOT = WS-RN-LAST-SECT
               MOVE BD-SECTION TO WS-RN-LAST-SECT
               MOVE ZERO TO WS-RN-SECT-SEQ
               ADD 1 TO WS-RT-SECTIONS.
           ADD 1 TO WS-RN-SEQ.
           ADD 1 TO WS-RN-SECT-SEQ.
           MOVE WS-RN-SEQ TO BD-LINE-SEQ.
           ADD 1 TO WS-RT-RENUMBERED.

       P5300-EXIT.
           EXIT.

       P5400-WRITE-SEQUENCED.
      * WRITE THE RECORD BACK OUT AT ITS TRUE LENGTH.  BD-ELEM-CNT WAS
      * CARRIED THROUGH THE SORT INSIDE THE IMAGE AND STILL GOVERNS THE
      * LENGTH OF THE RECORD THAT LEAVES THIS PROGRAM.
           MOVE 'P5400-WRITE-SEQUENCED' TO WS-PARA-NAME.
           IF BD-ELEM-CNT < 1
               MOVE 1 TO BD-ELEM-CNT.
           IF BD-ELEM-CNT > 40
               MOVE 40 TO BD-ELEM-CNT.
           WRITE BILL-DTL-OUT-REC FROM CABS-BILL-DETAIL.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 4302 TO WS-AB-CODE
               MOVE 'SEQUENCED DETAIL WRITE FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-WRITE-CNT.

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
           MOVE 'CABBIL03  BILL SECTION SEQUENCING REGISTER'
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
           MOVE 'BAN                 SECTION LINES RELEASED  AMOUNT'
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
           MOVE 415                    TO CT-STEP-SEQ.
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
           DISPLAY 'LINES RELEASED    ' WS-RT-RELEASED.
           DISPLAY 'LINES DROPPED     ' WS-RT-DROPPED.
           DISPLAY 'LINES RETURNED    ' WS-RT-RETURNED.
           DISPLAY 'LINES RENUMBERED  ' WS-RT-RENUMBERED.
           DISPLAY 'SECTIONS SEEN     ' WS-RT-SECTIONS.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE BILL-DTL-IN
                 BILL-DTL-OUT
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

