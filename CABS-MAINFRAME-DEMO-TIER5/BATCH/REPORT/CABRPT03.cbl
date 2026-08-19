      *****************************************************************
      * CABRPT03 - SUSPENSE AGEING REPORT                             *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               SUSIN   TELCABS.CABS.SUSPENSE.ALL(0)      CABSERR*
      *               SYSIN   INSTREAM CONTROL CARD             (LOCAL)*
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               AGEOUT  SYSOUT PRINT - AGEING REPORT      CABSPRNT*
      *               REPORT  SYSOUT PRINT - RUN REGISTER       CABSPRNT*
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN                                      *
      * REVISION HISTORY                                              *
      *   V1.00  1990-02-19  D.OKONKWO    INITIAL RELEASE - COUNT BY CODE*
      *   V1.04  1993-11-08  M.J.FERRARO  FIVE AGEING BANDS ADDED     *
      *   V1.08  1997-05-27  J.M.CASTILLO AGE NOW DERIVED FROM THE RUN ID*
      *                      PREFIX THROUGH CABDATCV                  *
      *   V2.00  2003-01-14  A.BUKOWSKI   OLDEST ITEM LISTING ADDED - THE*
      *                      RECYCLE JOB HAD BEEN LEAVING ITEMS       *
      *                      ON THE FILE FOR OVER A YEAR              *
      *   V2.05  2012-09-26  G.PRZYBYLSKI SEVERITY SPLIT ADDED WITHIN BAND*
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABRPT03.
       AUTHOR. TELCABS APPLICATIONS - BILLING CONTROL TEAM.
      *****************************************************************
      * AGES THE SUSPENSE FILE INTO FIVE BANDS, TALLIES BY ERROR CODE *
      * AND LISTS ANYTHING OLDER THAN THE OPERATOR SUPPLIED LIMIT.    *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SUS-IN-FILE ASSIGN TO UT-S-SUSIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT AGE-OUT-FILE ASSIGN TO UT-S-AGEOUT
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
      * SUSIN - THE CONSOLIDATED SUSPENSE FILE FROM EVERY             *
      * PROCESS IN THE ESTATE.                                        *
      *****************************************************************
       FD  SUS-IN-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 300 CHARACTERS.
       01  SUS-IN-REC                       PIC X(300).
      *****************************************************************
      * AGEOUT - THE AGEING REPORT.                                   *
      *****************************************************************
       FD  AGE-OUT-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       01  AGE-RECORD                       PIC X(133).
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
           05  WS-PGM-NAME             PIC X(08) VALUE 'CABRPT03'.
           05  WS-PGM-VERSION          PIC X(05) VALUE 'V2.05'.
           05  WS-PGM-APPL             PIC X(04) VALUE 'CABS'.
           05  WS-PGM-COMPILE          PIC X(08) VALUE '20120926'.
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
      * LAYOUT AGREED WITH THE CARRIER GATEWAY TEAM, CR-3318.         *
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
           05  WS-PE-AGE-BASE          PIC 9(05).
           05  WS-PE-SEV-SEL           PIC X(01).
           05  WS-PE-OLDEST-DAYS       PIC 9(03).
           05  WS-PE-DETAIL-SW         PIC X(01).
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
           05  WS-SUS-EOF-SW           PIC X(01) VALUE 'N'.
               88  WS-SUS-EOF          VALUE 'Y'.
           05  WS-CODE-FOUND-SW        PIC X(01) VALUE 'N'.
               88  WS-CODE-FOUND       VALUE 'Y'.
      *****************************************************************
      * AGEING BUCKETS.  FIVE BANDS.  A SUSPENSE RECORD THAT HAS BEEN *
      * ON THE FILE FOR MORE THAN THE OLDEST BAND IS LISTED           *
      * INDIVIDUALLY - THE RECYCLE JOB SHOULD HAVE CLEARED IT.        *
      *****************************************************************
       01  WS-AGE-TABLE.
           05  FILLER PIC X(16) VALUE '000UNDER 7 DAYS '.
           05  FILLER PIC X(16) VALUE '0077 TO 30 DAYS '.
           05  FILLER PIC X(16) VALUE '03031 TO 60 DAYS'.
           05  FILLER PIC X(16) VALUE '06061 TO 90 DAYS'.
           05  FILLER PIC X(16) VALUE '090OVER 90 DAYS '.
       01  WS-AGE-TABLE-R REDEFINES WS-AGE-TABLE.
           05  WS-AG-ENTRY OCCURS 5 TIMES INDEXED BY WS-AG-X.
               10  WS-AG-FROM          PIC 9(03).
               10  WS-AG-NAME          PIC X(13).
       01  WS-AGE-COUNTS.
           05  WS-AC-ENTRY OCCURS 5 TIMES.
               10  WS-AC-COUNT         PIC S9(09) COMP-3.
               10  WS-AC-FATAL         PIC S9(09) COMP-3.
               10  WS-AC-ERROR         PIC S9(09) COMP-3.
               10  WS-AC-WARN          PIC S9(09) COMP-3.
      *****************************************************************
      * ERROR CODE TALLY.  ONE ENTRY PER DISTINCT CODE SEEN.          *
      *****************************************************************
       01  WS-CODE-TABLE.
           05  WS-CD-ENTRY OCCURS 60 TIMES INDEXED BY WS-CD-X.
               10  WS-CD-CODE          PIC X(04).
               10  WS-CD-COUNT         PIC S9(09) COMP-3.
               10  WS-CD-OLDEST        PIC S9(05) COMP-3.
               10  WS-CD-PGM           PIC X(08).
       01  WS-CODE-CTL.
           05  WS-CD-USED              PIC S9(03) COMP-3 VALUE 0.
           05  WS-CD-MAX               PIC S9(03) COMP-3 VALUE 60.
           05  WS-CD-HIT               PIC S9(03) COMP-3 VALUE 0.
      *****************************************************************
      * AGE WORK.  THE SUSPENSE RECORD CARRIES NO DATE OF ITS OWN - THE*
      * RUN ID IT WAS RAISED UNDER CARRIES THE CYCLE DATE IN ITS FIRST*
      * FIVE BYTES, WHICH IS WHERE THE AGE COMES FROM.                *
      *****************************************************************
       01  WS-AGE-WORK.
           05  WS-AW-RAISED-YYDDD      PIC 9(05) VALUE 0.
           05  WS-AW-BASE-ABS          PIC S9(07) COMP-3 VALUE 0.
           05  WS-AW-ITEM-ABS          PIC S9(07) COMP-3 VALUE 0.
           05  WS-AW-DAYS              PIC S9(05) COMP-3 VALUE 0.
           05  WS-AW-BAND              PIC S9(03) COMP-3 VALUE 0.
           05  WS-AW-RUN-PREFIX        PIC X(12) VALUE SPACES.
       01  WS-RUN-PREFIX-R REDEFINES WS-AGE-WORK.
           05  FILLER                  PIC X(21).
           05  WS-RP-YYDDD             PIC 9(05).
           05  WS-RP-REST              PIC X(07).
       01  WS-RUN-TOTALS.
           05  WS-RT-RECORDS           PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-FATAL             PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-ERROR             PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-WARN              PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-OLDEST            PIC S9(05) COMP-3 VALUE 0.
           05  WS-RT-LISTED            PIC S9(09) COMP-3 VALUE 0.
           05  WS-RT-UNDATED           PIC S9(09) COMP-3 VALUE 0.
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
       01  WS-CK-DUMMY                 PIC X(01) VALUE SPACES.
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
           OPEN INPUT  SUS-IN-FILE
                       PARM-FILE
           OPEN OUTPUT AGE-OUT-FILE
                       CONTROL-FILE
                       SUSPENSE-FILE
                       PRINT-FILE
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 7211 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-SUSIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 7212 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-AGEOUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 7213 TO WS-AB-CODE
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
           PERFORM P5600-CLEAR-BANDS THRU P5600-EXIT
               VARYING WS-SUB1 FROM 1 BY 1
               UNTIL WS-SUB1 > 5.
           PERFORM P6000-HEADING THRU P6000-EXIT.
           DISPLAY WS-PGM-NAME ' STARTED - RUN ' WS-RUN-ID.
           DISPLAY '  AGE BASE      ' WS-PE-AGE-BASE.
           DISPLAY '  OLDEST DAYS   ' WS-PE-OLDEST-DAYS.

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
      * THE OLDEST DAYS LIMIT IS SET BY THE CONTROL STANDARD AND
      * PASSED IN BY THE SCHEDULER.  IT HAS NO DEFAULT.
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
           IF WS-PE-AGE-BASE NOT NUMERIC
               MOVE WS-PC-CYCLE TO WS-PE-AGE-BASE.
           IF WS-PE-AGE-BASE = ZERO
               MOVE WS-PC-CYCLE TO WS-PE-AGE-BASE.
           IF WS-PE-OLDEST-DAYS NOT NUMERIC
               MOVE 7221 TO WS-AB-CODE
               MOVE 'OLDEST DAYS LIMIT NOT NUMERIC' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-PE-DETAIL-SW NOT = 'Y'
               MOVE 'N' TO WS-PE-DETAIL-SW.

       P1200-EXIT.
           EXIT.

      *****************************************************************
      * S200-MAIN PROCESSING                                          *
      *****************************************************************
       S200-MAIN-PROCESS SECTION.

       P2000-PROCESS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ-SUSPENSE THRU P2100-EXIT.
           IF WS-SUS-EOF
               MOVE 'Y' TO WS-EOF-SW
               GO TO P2000-EXIT.
           ADD 1 TO WS-READ-CNT.
           ADD 1 TO WS-RT-RECORDS.
           MOVE SU-RUN-ID TO WS-RESTART-KEY.
           PERFORM P3000-DERIVE-AGE THRU P3000-EXIT.
           PERFORM P4000-AGE-BAND THRU P4000-EXIT.
           PERFORM P5000-TALLY-CODE THRU P5000-EXIT.
           PERFORM P5200-SEVERITY-TALLY THRU P5200-EXIT.
           IF WS-AW-DAYS > WS-PE-OLDEST-DAYS
               PERFORM P5300-LIST-OLD THRU P5300-EXIT.
           ADD 1 TO WS-SUMM-CNT.

       P2000-EXIT.
           EXIT.

       P2100-READ-SUSPENSE.
           MOVE 'P2100-READ-SUSPENSE' TO WS-PARA-NAME.
           READ SUS-IN-FILE INTO CABS-SUSPENSE-RECORD
               AT END
                   MOVE 'Y' TO WS-SUS-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00' AND WS-FS-INPUT NOT = '10'
               MOVE 7201 TO WS-AB-CODE
               MOVE 'SUSPENSE FILE READ ERROR' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.

       P2100-EXIT.
           EXIT.

      *****************************************************************
      * S300-AGE DERIVATION                                           *
      *****************************************************************
       S300-AGE SECTION.

       P3000-DERIVE-AGE.
      * THE AGE OF A SUSPENSE RECORD.  THE RUN ID CARRIES THE CYCLE
      * DATE IN ITS FIRST FIVE BYTES BY CONVENTION.  A RUN ID THAT DOES
      * NOT FOLLOW THE CONVENTION LEAVES THE RECORD UNDATED AND IT IS
      * COUNTED IN THE YOUNGEST BAND.
           MOVE 'P3000-DERIVE-AGE' TO WS-PARA-NAME.
           MOVE ZERO TO WS-AW-DAYS.
           MOVE SU-RUN-ID TO WS-AW-RUN-PREFIX.
           IF WS-RP-YYDDD NOT NUMERIC
               ADD 1 TO WS-RT-UNDATED
               GO TO P3000-EXIT.
           MOVE WS-RP-YYDDD TO WS-AW-RAISED-YYDDD.
           IF WS-AW-RAISED-YYDDD = ZERO
               ADD 1 TO WS-RT-UNDATED
               GO TO P3000-EXIT.
           MOVE 'JA' TO WS-DP-FUNCTION.
           MOVE WS-PE-AGE-BASE TO WS-DP-YYDDD.
           CALL 'CABDATCV' USING WS-DATE-PARM.
           MOVE WS-DP-DAYS TO WS-AW-BASE-ABS.
           MOVE 'JA' TO WS-DP-FUNCTION.
           MOVE WS-AW-RAISED-YYDDD TO WS-DP-YYDDD.
           CALL 'CABDATCV' USING WS-DATE-PARM.
           MOVE WS-DP-DAYS TO WS-AW-ITEM-ABS.
           COMPUTE WS-AW-DAYS = WS-AW-BASE-ABS - WS-AW-ITEM-ABS.
           IF WS-AW-DAYS < ZERO
               MOVE ZERO TO WS-AW-DAYS.
           IF WS-AW-DAYS > WS-RT-OLDEST
               MOVE WS-AW-DAYS TO WS-RT-OLDEST.

       P3000-EXIT.
           EXIT.

      *****************************************************************
      * S400-AGE BANDING                                              *
      * THE FIVE BANDS ARE TESTED IN ORDER.  EACH BAND FALLS THROUGH TO*
      * THE NEXT SO THAT A RECORD LANDS IN THE FIRST BAND WHOSE UPPER *
      * LIMIT IT DOES NOT EXCEED.                                     *
      *****************************************************************
       S400-BANDING SECTION.

       P4000-AGE-BAND.
           MOVE 'P4000-AGE-BAND' TO WS-PARA-NAME.
           MOVE 5 TO WS-AW-BAND.
           IF WS-AW-DAYS < 7
               MOVE 1 TO WS-AW-BAND
               GO TO P4000-COUNT.
           IF WS-AW-DAYS < 31
               MOVE 2 TO WS-AW-BAND
               GO TO P4000-COUNT.
           PERFORM P4200-AGE-BUCKET-60 THRU P4300-EXIT.

       P4000-COUNT.
           ADD 1 TO WS-AC-COUNT (WS-AW-BAND).

       P4000-EXIT.
           EXIT.

       P4200-AGE-BUCKET-60.
      * THE 31 TO 60 DAY BAND.  A RECORD THAT IS OLDER THAN SIXTY DAYS
      * CONTINUES INTO THE NEXT BAND TEST BELOW.
           MOVE 'P4200-AGE-BUCKET-60' TO WS-PARA-NAME.
           IF WS-AW-DAYS < 61
               MOVE 3 TO WS-AW-BAND
               GO TO P4200-DONE.
           MOVE 5 TO WS-AW-BAND.

       P4200-DONE.
           MOVE SPACES TO WS-CK-DUMMY.

       P4300-AGE-BUCKET-90.
      * THE 61 TO 90 DAY BAND.  ANYTHING OLDER STAYS IN BAND FIVE,
      * WHICH IS THE ONE THE RECYCLE JOB IS SUPPOSED TO HAVE CLEARED.
           MOVE 'P4300-AGE-BUCKET-90' TO WS-PARA-NAME.
           IF WS-AW-BAND NOT = 5
               GO TO P4300-EXIT.
           IF WS-AW-DAYS < 91
               MOVE 4 TO WS-AW-BAND.

       P4300-EXIT.
           EXIT.

      *****************************************************************
      * S500-TALLIES AND LISTING                                      *
      *****************************************************************
       S500-TALLY SECTION.

       P5000-TALLY-CODE.
           MOVE 'P5000-TALLY-CODE' TO WS-PARA-NAME.
           MOVE 'N' TO WS-CODE-FOUND-SW.
           MOVE ZERO TO WS-CD-HIT.
           PERFORM P5010-MATCH-CODE THRU P5010-EXIT
               VARYING WS-CD-X FROM 1 BY 1
               UNTIL WS-CD-X > WS-CD-USED OR WS-CODE-FOUND.
           IF WS-CODE-FOUND
               GO TO P5000-BUMP.
           IF WS-CD-USED NOT < WS-CD-MAX
               GO TO P5000-EXIT.
           ADD 1 TO WS-CD-USED.
           MOVE WS-CD-USED TO WS-CD-HIT.
           SET WS-CD-X TO WS-CD-USED.
           MOVE SU-ERR-CODE   TO WS-CD-CODE (WS-CD-X).
           MOVE SU-DETECT-PGM TO WS-CD-PGM (WS-CD-X).
           MOVE ZERO TO WS-CD-COUNT (WS-CD-X)
                        WS-CD-OLDEST (WS-CD-X).

       P5000-BUMP.
           SET WS-CD-X TO WS-CD-HIT.
           ADD 1 TO WS-CD-COUNT (WS-CD-X).
           IF WS-AW-DAYS > WS-CD-OLDEST (WS-CD-X)
               MOVE WS-AW-DAYS TO WS-CD-OLDEST (WS-CD-X).

       P5000-EXIT.
           EXIT.

       P5010-MATCH-CODE.
           IF WS-CD-CODE (WS-CD-X) = SU-ERR-CODE
               SET WS-SUB1 TO WS-CD-X
               MOVE WS-SUB1 TO WS-CD-HIT
               MOVE 'Y' TO WS-CODE-FOUND-SW.

       P5010-EXIT.
           EXIT.

       P5200-SEVERITY-TALLY.
           IF SU-FATAL
               ADD 1 TO WS-RT-FATAL
               ADD 1 TO WS-AC-FATAL (WS-AW-BAND)
               GO TO P5200-EXIT.
           IF SU-ERROR
               ADD 1 TO WS-RT-ERROR
               ADD 1 TO WS-AC-ERROR (WS-AW-BAND)
               GO TO P5200-EXIT.
           ADD 1 TO WS-RT-WARN.
           ADD 1 TO WS-AC-WARN (WS-AW-BAND).

       P5200-EXIT.
           EXIT.

       P5300-LIST-OLD.
           MOVE 'P5300-LIST-OLD' TO WS-PARA-NAME.
           IF WS-PE-DETAIL-SW NOT = 'Y'
               GO TO P5300-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE SU-ERR-CODE    TO PC-COL-001-020.
           MOVE SU-DETECT-PGM  TO PC-COL-021-060.
           MOVE SU-DETECT-PARA TO PC-COL-061-090.
           MOVE WS-AW-DAYS TO WS-ED-COUNT.
           MOVE WS-ED-COUNT    TO PC-COL-091-132.
           PERFORM P5500-WRITE-LINE THRU P5500-EXIT.
           ADD 1 TO WS-RT-LISTED.

       P5300-EXIT.
           EXIT.

       P5400-PRINT-AGEING.
           MOVE 'P5400-PRINT-AGEING' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '4' TO PC-CC.
           MOVE 'SUSPENSE AGEING SUMMARY' TO PC-TEXT.
           PERFORM P5500-WRITE-LINE THRU P5500-EXIT.
           PERFORM P5410-AGE-LINE THRU P5410-EXIT
               VARYING WS-AG-X FROM 1 BY 1
               UNTIL WS-AG-X > 5.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '4' TO PC-CC.
           MOVE 'SUSPENSE BY ERROR CODE' TO PC-TEXT.
           PERFORM P5500-WRITE-LINE THRU P5500-EXIT.
           PERFORM P5420-CODE-LINE THRU P5420-EXIT
               VARYING WS-CD-X FROM 1 BY 1
               UNTIL WS-CD-X > WS-CD-USED.

       P5400-EXIT.
           EXIT.

       P5410-AGE-LINE.
           SET WS-SUB1 TO WS-AG-X.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-AG-NAME (WS-AG-X)   TO PC-COL-001-020.
           MOVE WS-AC-COUNT (WS-SUB1)  TO WS-ED-COUNT.
           MOVE WS-ED-COUNT            TO PC-COL-021-060.
           MOVE WS-AC-FATAL (WS-SUB1)  TO WS-ED-COUNT.
           MOVE WS-ED-COUNT            TO PC-COL-061-090.
           MOVE WS-AC-ERROR (WS-SUB1)  TO WS-ED-COUNT.
           MOVE WS-ED-COUNT            TO PC-COL-091-132.
           PERFORM P5500-WRITE-LINE THRU P5500-EXIT.

       P5410-EXIT.
           EXIT.

       P5420-CODE-LINE.
           SET WS-SUB2 TO WS-CD-X.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-CD-CODE (WS-CD-X)   TO PC-COL-001-020.
           MOVE WS-CD-PGM (WS-CD-X)    TO PC-COL-021-060.
           MOVE WS-CD-COUNT (WS-SUB2)  TO WS-ED-COUNT.
           MOVE WS-ED-COUNT            TO PC-COL-061-090.
           MOVE WS-CD-OLDEST (WS-SUB2) TO WS-ED-COUNT.
           MOVE WS-ED-COUNT            TO PC-COL-091-132.
           PERFORM P5500-WRITE-LINE THRU P5500-EXIT.

       P5420-EXIT.
           EXIT.

       P5500-WRITE-LINE.
           WRITE AGE-RECORD FROM CABS-PRINT-LINE.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 7202 TO WS-AB-CODE
               MOVE 'AGEING REPORT WRITE FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-WRITE-CNT.

       P5500-EXIT.
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
           MOVE 'CABRPT03  SUSPENSE AGEING REPORT'
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
           MOVE 'ERROR CODE  DETECTED BY      COUNT       OLDEST'
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
           MOVE 610                    TO CT-STEP-SEQ.
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
           PERFORM P5400-PRINT-AGEING THRU P5400-EXIT.
           DISPLAY 'SUSPENSE RECORDS  ' WS-RT-RECORDS.
           DISPLAY 'FATAL             ' WS-RT-FATAL.
           DISPLAY 'ERROR             ' WS-RT-ERROR.
           DISPLAY 'WARNING           ' WS-RT-WARN.
           DISPLAY 'OLDEST IN DAYS    ' WS-RT-OLDEST.
           DISPLAY 'LISTED AS OLD     ' WS-RT-LISTED.
           DISPLAY 'UNDATED RECORDS   ' WS-RT-UNDATED.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE SUS-IN-FILE
                 AGE-OUT-FILE
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

       P5600-CLEAR-BANDS.
           MOVE ZERO TO WS-AC-COUNT (WS-SUB1)
                        WS-AC-FATAL (WS-SUB1)
                        WS-AC-ERROR (WS-SUB1)
                        WS-AC-WARN (WS-SUB1).

       P5600-EXIT.
           EXIT.
