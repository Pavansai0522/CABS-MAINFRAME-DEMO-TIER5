      *****************************************************************
      * CABJUR10 - RESTATEMENT ADJUSTMENT POSTING - DB2 AND VSAM      *
      * APPLICATION : CABS                                            *
      * COMPILER    : ENTERPRISE COBOL                                *
      * INPUTS      : ADJIN    TELCABS.CABS.RESTATE.ADJ(0)    CABSBILL*
      * INPUTS      : SYSIN    INSTREAM                       NONE    *
      * OUTPUTS     : BALMAST  TELCABS.CABS.BALANCE           CABSBHDR*
      * OUTPUTS     : DB2      CABSADJ TABLE (DSNCABS)        DCLGEN  *
      * CONTROL     : CTLOUT                         CABSCTL          *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED              *
      *               CT-WRITTEN COUNTS ROWS INSERTED AND VSAM RECORDS*
      *               REWRITTEN - THE TWO MUST AGREE, AND WHEN THEY DO*
      *               NOT THERE IS NO WAY TO TELL WHICH ONE IS RIGHT  *
      * RESTART     : FULL RERUN NOT SAFE - SEE THE NOTE IN P3000     *
      * COMPILED WITH ENTERPRISE COBOL AND THE DB2                    *
      * PRECOMPILER.  SCOPE TERMINATORS ARE PERMITTED IN              *
      * THIS MODULE AND NOWHERE ELSE IN THE BATCH ESTATE.             *
      * STANDARDS   : CODED TO CABS-STD-014 (RECORD LAYOUTS) AND      *
      *               CABS-STD-058 (DATE HANDLING). REVIEWED AT THE   *
      *               2013 APPLICATION AUDIT. NO WAIVERS ON FILE FOR  *
      *               THIS MODULE. RECOMPILE ONLY CHANGES DO NOT      *
      *               REQUIRE A NEW DESIGN NOTE.                      *
      * REVISION HISTORY                                              *
      *   V1.00  1998-06-15  P.NAIR        INITIAL - DB2 POSTING      *
      *   V1.02  1999-11-30  P.NAIR        VSAM BALANCE ADDED         *
      *   V1.05  2003-02-18  P.NAIR        COMMIT EVERY 500 ROWS      *
      *   V1.07  2007-07-11  A.BUKOWSKI    DEADLOCK RETRY ADDED       *
      *   V2.00  2011-09-06  A.BUKOWSKI    TWO PHASE - NOT DONE       *
      *   V2.02  2015-04-23  L.FERREIRA    SQLCODE 803 TOLERATED      *
      *   V2.03  2019-05-14  M.OYELARAN    RECOMPILE ONLY             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CABJUR10.
       AUTHOR.        P.NAIR.
       DATE-WRITTEN.  1998-06-15.
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
      * RESTATEMENT ADJUSTMENTS FROM CABJUR07 OR CABJUR08
           SELECT ADJUST-IN-FILE
               ASSIGN TO UT-S-ADJIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
      * ACCOUNT BALANCE KSDS - THE SECOND STORE
           SELECT BALANCE-MASTER
               ASSIGN TO DA-I-BALMAST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS BAL-KEY
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

       DATA DIVISION.
       FILE SECTION.
       FD  ADJUST-IN-FILE
               RECORDING MODE IS V
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 1647 CHARACTERS
               DATA RECORD IS ADI-RECORD.
       01  ADI-RECORD              PIC X(1647).

       FD  BALANCE-MASTER
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 400 CHARACTERS
               DATA RECORD IS BAL-RECORD.
       01  BAL-RECORD.
           05  BAL-KEY                 PIC X(19).
           05  BAL-DATA                PIC X(381).

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

       WORKING-STORAGE SECTION.

      * PROGRAM IDENTIFICATION - MOVED TO THE CONTROL RECORD AND TO
      * EVERY SUSPENSE RECORD RAISED BY THIS MODULE.
       01  WS-PROGRAM-IDENT.
           05  WS-PGM-NAME         PIC X(08)         VALUE 'CABJUR10'.
           05  WS-PGM-VERSION        PIC X(05)           VALUE 'V2.03'.
           05  WS-PGM-APPL           PIC X(04)           VALUE 'CABS'.
           05  WS-PGM-COMPILE      PIC X(08)         VALUE '20190514'.
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

       COPY CABSBILL.

       COPY CABSBHDR.

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

      * DB2 COMMUNICATION AREA.  THIS MODULE IS COMPILED WITH THE
      * ENTERPRISE COBOL COMPILER AND THE DB2 PRECOMPILER; SCOPE
      * TERMINATORS ARE PERMITTED HERE AND NOWHERE ELSE.
       EXEC SQL INCLUDE SQLCA END-EXEC.

      * SYSIN CONTROL CARD.  READ AS 80 BYTES THEN REDEFINED THREE
      * WAYS.  THE CARD TYPE IN COLUMNS 1-2 DECIDES WHICH REDEFINE
      * IS VALID.  NOTHING IN THE PROGRAM ENFORCES THAT AGREEMENT.
      * LAYOUT AGREED WITH THE CARRIER GATEWAY TEAM, CR-3318.
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
           05  WS-PE-COMMIT-FREQ       PIC 9(05).
           05  WS-PE-DB2-ONLY          PIC X(01).
           05  WS-PE-FILLER            PIC X(29).
       01  WS-PARM-OLD REDEFINES WS-PARM-CARD.
      * THE 1989 CARD.  TWO REGIONS NEVER CONVERTED THEIR JCL AND
      * THE EDIT STILL ACCEPTS IT.  THE FIELDS ARE IN A DIFFERENT
      * ORDER AND THE CYCLE DATE IS SIX DIGITS YYMMDD, NOT YYDDD.
           05  FILLER                  PIC X(02).
           05  WS-PO-CYCLE             PIC 9(05).
           05  WS-PO-RUN-ID            PIC X(12).
           05  WS-PO-YYMMDD            PIC 9(06).
           05  WS-PO-COMMIT            PIC 9(05).
           05  FILLER                  PIC X(20).

      * LOCAL SWITCHES.  THE STANDARD ONES ARRIVE VIA CABSWRK.
       01  WS-LOCAL-SWITCHES.
           05  WS-PARM-EOF-SW          PIC X(01)             VALUE 'N'.
                   88  WS-PARM-EOF             VALUE 'Y'.
           05  WS-SQL-OK-SW            PIC X(01)             VALUE 'Y'.
                   88  WS-SQL-OK               VALUE 'Y'.
                   88  WS-SQL-BAD              VALUE 'N'.
           05  WS-VSAM-FOUND-SW        PIC X(01)             VALUE 'N'.
                   88  WS-VSAM-FOUND           VALUE 'Y'.
           05  WS-RETRY-SW             PIC X(01)             VALUE 'N'.
                   88  WS-RETRYING             VALUE 'Y'.

      * SUBSCRIPTS AND INDEX WORK FIELDS.
       01  WS-SUBSCRIPTS.
           05  WS-SUB1                 PIC S9(05) COMP-3     VALUE 0.
           05  WS-SUB2                 PIC S9(05) COMP-3     VALUE 0.

      * DB2 HOST VARIABLES FOR THE CABSADJ TABLE.  THE TABLE IS
      * IN DATABASE DSNCABS AND IS THE SYSTEM OF RECORD FOR
      * ADJUSTMENTS.  THE VSAM BALANCE FILE IS THE SYSTEM OF
      * RECORD FOR THE ACCOUNT BALANCE.  BOTH ARE UPDATED BELOW.
       01  WS-HOST-VARIABLES.
           05  WS-HV-BAN               PIC X(13).
           05  WS-HV-BILL-PERIOD       PIC S9(09) COMP.
           05  WS-HV-SECTION           PIC X(02).
           05  WS-HV-LINE-SEQ          PIC S9(09) COMP.
           05  WS-HV-OCN               PIC X(04).
           05  WS-HV-JURIS             PIC X(01).
           05  WS-HV-STATE             PIC X(02).
           05  WS-HV-DESC              PIC X(60).
           05  WS-HV-MINUTES           PIC S9(13)V9(02) COMP-3.
           05  WS-HV-AMOUNT            PIC S9(13)V9(05) COMP-3.
           05  WS-HV-ROUNDED           PIC S9(13)V9(02) COMP-3.
           05  WS-HV-RUN-ID            PIC X(12).
           05  WS-HV-POST-DATE         PIC X(10).
           05  WS-HV-REASON            PIC X(02).
           05  WS-HV-ROWCOUNT          PIC S9(09) COMP.

      * POSTING COUNTERS.  WS-SINCE-COMMIT DRIVES THE COMMIT
      * FREQUENCY.  THE COMMIT COMMITS DB2 ONLY - THE VSAM FILE
      * IS NOT UNDER DB2 CONTROL AND ITS WRITES ARE ALREADY
      * HARDENED BY THE TIME THE COMMIT IS ISSUED.
       01  WS-POST-COUNTERS.
           05  WS-SQL-INS-CNT          PIC S9(09) COMP-3     VALUE 0.
           05  WS-SQL-UPD-CNT          PIC S9(09) COMP-3     VALUE 0.
           05  WS-VSAM-UPD-CNT         PIC S9(09) COMP-3     VALUE 0.
           05  WS-VSAM-ADD-CNT         PIC S9(09) COMP-3     VALUE 0.
           05  WS-COMMIT-CNT           PIC S9(09) COMP-3     VALUE 0.
           05  WS-DUP-ROW-CNT          PIC S9(09) COMP-3     VALUE 0.
           05  WS-RETRY-CNT            PIC S9(09) COMP-3     VALUE 0.
           05  WS-SINCE-COMMIT         PIC S9(09) COMP-3     VALUE 0.
           05  WS-TOT-POST-AMT         PIC S9(15)V9(05) COMP-3 VALUE 0.

      * BALANCE RECORD WORK AREA WITH TWO REDEFINES.  THE KSDS
      * RECORD IS FOUR HUNDRED BYTES OF WHICH THE FIRST PART IS
      * THE BILL HEADER LAYOUT AND THE REST IS FREE FORMAT.
       01  WS-BALANCE-WORK.
           05  WS-BW-BAN             PIC X(13)           VALUE SPACES.
           05  WS-BW-PERIOD            PIC 9(06)             VALUE 0.
           05  WS-BW-RESTATEMENT       PIC S9(13)V9(02)      VALUE 0.
           05  WS-BW-TOTAL-DUE         PIC S9(13)V9(02)      VALUE 0.
           05  WS-BW-UPDATE-YYDDD      PIC 9(05)             VALUE 0.
           05  WS-BW-UPDATE-PGM      PIC X(08)           VALUE SPACES.
           05  WS-BW-FILLER          PIC X(338)          VALUE SPACES.
       01  WS-BALANCE-KEY-R REDEFINES WS-BALANCE-WORK.
           05  WS-BK-FULL-KEY          PIC X(19).
           05  WS-BK-REST              PIC X(381).
       01  WS-BALANCE-AMT-R REDEFINES WS-BALANCE-WORK.
           05  WS-BA-HEAD              PIC X(19).
           05  WS-BA-AMOUNTS           PIC X(30).
           05  WS-BA-TAIL              PIC X(351).

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

      * MESSAGE TEXT TABLE.  ADDRESSED BY ERROR CODE SEQUENCE, NOT
      * BY THE CODE ITSELF - INSERTING A CODE IN THE MIDDLE SHIFTS
      * EVERY MESSAGE.  THIS HAS HAPPENED TWICE (1994 AND 2007).
       01  WS-MSG-CONST.
           05  FILLER              PIC X(44)
                   VALUE 'ROW INSERTED INTO CABSADJ                   '.
           05  FILLER              PIC X(44)
                   VALUE 'ROW UPDATED IN CABSADJ                      '.
           05  FILLER              PIC X(44)
                   VALUE 'DUPLICATE ROW TOLERATED SINCE 2015          '.
           05  FILLER              PIC X(44)
                   VALUE 'VSAM BALANCE RECORD REWRITTEN               '.
           05  FILLER              PIC X(44)
                   VALUE 'VSAM BALANCE RECORD ADDED                   '.
           05  FILLER              PIC X(44)
                   VALUE 'DEADLOCK - RETRYING                         '.
           05  FILLER              PIC X(44)
                   VALUE 'COMMIT TAKEN                                '.
           05  FILLER              PIC X(44)
                   VALUE 'SQL ERROR - RUN TERMINATED                  '.
           05  FILLER              PIC X(44)
                   VALUE 'VSAM UPDATED BUT DB2 NOT COMMITTED          '.
           05  FILLER              PIC X(44)
                   VALUE 'DB2 ONLY MODE - VSAM NOT TOUCHED            '.
       01  WS-MSG-TABLE REDEFINES WS-MSG-CONST.
           05  WS-MSG-ENTRY OCCURS 10 TIMES
                   INDEXED BY WS-MSG-IX.
               10  WS-MSG-TEXT             PIC X(44).

      * LAYOUTS COPIED FOR THE CROSS REFERENCE AND VALIDATION
      * ROUTINES.  NOT EVERY FIELD IN EVERY LAYOUT IS USED BY
      * THIS MODULE - THE COPY IS HERE BECAUSE THE LAYOUT WAS
      * NEEDED AT SOME POINT AND REMOVING A COPY MEMBER FORCES
      * A FULL REGRESSION UNDER CABS-STD-009.
       COPY CABSRATE.
       COPY CABSCARR.

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
           OPEN INPUT  ADJUST-IN-FILE
                       PARM-FILE
           OPEN OUTPUT CONTROL-FILE
                       SUSPENSE-FILE
           OPEN I-O    BALANCE-MASTER
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 5001 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-ADJIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 5002 TO WS-AB-CODE
               MOVE 'OPEN FAILED DA-I-BALMAST' TO WS-AB-TEXT
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
           MOVE WS-ACCEPT-DATE         TO WS-AD-WORK.
           MOVE WS-AD-YY               TO DW-CUR-YY.
           PERFORM P1100-READ-PARM THRU P1100-EXIT.
           PERFORM P1200-EDIT-PARM THRU P1200-EXIT.
           MOVE ZERO TO WS-SQL-INS-CNT WS-SQL-UPD-CNT
                        WS-VSAM-UPD-CNT WS-VSAM-ADD-CNT
                        WS-COMMIT-CNT WS-DUP-ROW-CNT
                        WS-RETRY-CNT WS-SINCE-COMMIT
                        WS-TOT-POST-AMT.
           MOVE WS-RUN-ID TO WS-HV-RUN-ID.
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
           IF WS-PE-COMMIT-FREQ NOT NUMERIC
               MOVE 00500 TO WS-PE-COMMIT-FREQ.
           IF WS-PE-COMMIT-FREQ = ZERO
               MOVE 00500 TO WS-PE-COMMIT-FREQ.
           IF WS-PE-DB2-ONLY NOT = 'Y'
               MOVE 'N' TO WS-PE-DB2-ONLY.

       P1200-EXIT.
           EXIT.


      *****************************************************************
      * S200-POSTING                                                  *
      * POST TO DB2 AND TO VSAM.                                      *
      *****************************************************************
       S200-POSTING SECTION.

       P2000-PROCESS.
      * ONE ADJUSTMENT RECORD PER PASS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ THRU P2100-EXIT.
           IF WS-EOF
               GO TO P2000-EXIT.
           MOVE ADI-RECORD TO CABS-BILL-DETAIL.
           MOVE BD-KEY TO WS-RESTART-KEY.
           PERFORM P2200-BUILD-HOST THRU P2200-EXIT.
           PERFORM P3000-POST-DB2 THRU P3000-EXIT.
           IF WS-SQL-BAD
               GO TO P2000-EXIT.
           PERFORM P4000-POST-VSAM THRU P4000-EXIT.
           PERFORM P5000-COMMIT-CHECK THRU P5000-EXIT.
           ADD 1 TO WS-WRITE-CNT.
           ADD BD-TOT-AMOUNT TO WS-TOT-POST-AMT.
           ADD BD-TOT-AMOUNT TO WS-ACC-AMOUNT.

       P2000-EXIT.
           EXIT.

       P2100-READ.
      * SEQUENTIAL READ OF THE ADJUSTMENT FILE.
           READ ADJUST-IN-FILE
               AT END
                   MOVE 'Y' TO WS-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00'
               MOVE 3500 TO WS-AB-CODE
               MOVE 'READ ERROR UT-S-ADJIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-READ-CNT.

       P2100-EXIT.
           EXIT.

       P2200-BUILD-HOST.
      * MOVE THE ADJUSTMENT INTO THE DB2 HOST VARIABLES.
           MOVE BD-BAN TO WS-HV-BAN.
           MOVE BD-BILL-PERIOD TO WS-HV-BILL-PERIOD.
           MOVE BD-SECTION TO WS-HV-SECTION.
           MOVE BD-LINE-SEQ TO WS-HV-LINE-SEQ.
           MOVE BD-OCN TO WS-HV-OCN.
           MOVE BD-JURIS-CD TO WS-HV-JURIS.
           MOVE BD-STATE-CD TO WS-HV-STATE.
           MOVE BD-DESCRIPTION TO WS-HV-DESC.
           MOVE BD-TOT-MINUTES TO WS-HV-MINUTES.
           MOVE BD-TOT-AMOUNT TO WS-HV-AMOUNT.
           MOVE BD-TOT-ROUNDED TO WS-HV-ROUNDED.
           MOVE WS-RUN-ID TO WS-HV-RUN-ID.

       P2200-EXIT.
           EXIT.

       P3000-POST-DB2.
      * INSERT THE ADJUSTMENT ROW.  A DUPLICATE KEY HAS BEEN
      * TOLERATED SINCE 2015 BECAUSE THE RERUN PROCEDURE RELIES ON
      * IT.  THAT TOLERANCE IS ALSO WHY A RERUN AFTER A PART DONE
      * RUN LEAVES THE VSAM BALANCE DOUBLE COUNTED - THE INSERT IS
      * REJECTED AS A DUPLICATE BUT THE VSAM REWRITE BELOW IS NOT.
      * THIS IS THE UNCOORDINATED PAIR.
           MOVE 'P3000-POST-DB2' TO WS-PARA-NAME.
           MOVE 'Y' TO WS-SQL-OK-SW.
           EXEC SQL
               INSERT INTO CABSADJ
                     (BAN, BILL_PERIOD, SECTION_CD, LINE_SEQ,
                      OCN, JURIS_CD, STATE_CD, DESCRIPTION,
                      MINUTES, AMOUNT, AMOUNT_ROUNDED, RUN_ID)
               VALUES (:WS-HV-BAN, :WS-HV-BILL-PERIOD,
                       :WS-HV-SECTION, :WS-HV-LINE-SEQ,
                       :WS-HV-OCN, :WS-HV-JURIS,
                       :WS-HV-STATE, :WS-HV-DESC,
                       :WS-HV-MINUTES, :WS-HV-AMOUNT,
                       :WS-HV-ROUNDED, :WS-HV-RUN-ID)
           END-EXEC.
           EVALUATE SQLCODE
               WHEN 0
                   ADD 1 TO WS-SQL-INS-CNT
               WHEN -803
                   ADD 1 TO WS-DUP-ROW-CNT
                   PERFORM P3100-UPDATE-ROW THRU P3100-EXIT
               WHEN -911
                   ADD 1 TO WS-RETRY-CNT
                   PERFORM P3200-RETRY THRU P3200-EXIT
               WHEN OTHER
                   MOVE 5010 TO WS-AB-CODE
                   MOVE 'SQL ERROR ON INSERT TO CABSADJ' TO WS-AB-TEXT
                   DISPLAY 'SQLCODE ' SQLCODE
                   PERFORM P9500-ABEND THRU P9500-EXIT
           END-EVALUATE.

       P3000-EXIT.
           EXIT.

       P3100-UPDATE-ROW.
      * THE ROW ALREADY EXISTS.  UPDATE IT INSTEAD.  THE AMOUNT IS
      * REPLACED, NOT ADDED TO - A RERUN MUST NOT DOUBLE THE ROW.
      * THE VSAM SIDE HAS NO EQUIVALENT PROTECTION.
           EXEC SQL
               UPDATE CABSADJ
                  SET AMOUNT = :WS-HV-AMOUNT,
                      AMOUNT_ROUNDED = :WS-HV-ROUNDED,
                      MINUTES = :WS-HV-MINUTES,
                      RUN_ID = :WS-HV-RUN-ID
                WHERE BAN = :WS-HV-BAN
                  AND BILL_PERIOD = :WS-HV-BILL-PERIOD
                  AND SECTION_CD = :WS-HV-SECTION
                  AND LINE_SEQ = :WS-HV-LINE-SEQ
           END-EXEC.
           IF SQLCODE = 0
               ADD 1 TO WS-SQL-UPD-CNT
           ELSE
               MOVE 5011 TO WS-AB-CODE
               MOVE 'SQL ERROR ON UPDATE OF CABSADJ' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT
           END-IF.

       P3100-EXIT.
           EXIT.

       P3200-RETRY.
      * DEADLOCK RETRY.  ONE RETRY ONLY - A SECOND DEADLOCK ABENDS
      * THE STEP.  THE VSAM WRITE FOR THE PREVIOUS RECORD HAS
      * ALREADY HAPPENED AND IS NOT ROLLED BACK BY THE DB2 ROLLBACK.
           EXEC SQL
               ROLLBACK
           END-EXEC.
           MOVE 'Y' TO WS-RETRY-SW.
           PERFORM P3000-POST-DB2 THRU P3000-EXIT.
           MOVE 'N' TO WS-RETRY-SW.

       P3200-EXIT.
           EXIT.

       P4000-POST-VSAM.
      * UPDATE THE ACCOUNT BALANCE ON THE VSAM KSDS.  THIS IS THE
      * SECOND STORE.  THERE IS NO COORDINATION WITH THE DB2 INSERT
      * ABOVE - NO TWO PHASE COMMIT, NO COMPENSATING TRANSACTION,
      * NO RECOVERY LOG THAT SPANS BOTH.  AN ABEND BETWEEN THE
      * INSERT AND THE REWRITE LEAVES THE ADJUSTMENT IN DB2 AND NOT
      * IN THE BALANCE, AND AN ABEND AFTER THE REWRITE BUT BEFORE
      * THE COMMIT LEAVES IT IN THE BALANCE AND NOT IN DB2.  THE
      * 2011 CHANGE NOTE SAYS TWO PHASE COMMIT WAS ADDED.  IT WAS
      * SPECIFIED AND NEVER WRITTEN.
           MOVE 'P4000-POST-VSAM' TO WS-PARA-NAME.
           IF WS-PE-DB2-ONLY = 'Y'
               GO TO P4000-EXIT
           END-IF.
           MOVE SPACES TO WS-BALANCE-WORK.
           MOVE BD-BAN TO WS-BW-BAN.
           MOVE BD-BILL-PERIOD TO WS-BW-PERIOD.
           MOVE WS-BK-FULL-KEY TO BAL-KEY.
           MOVE 'N' TO WS-VSAM-FOUND-SW.
           READ BALANCE-MASTER
               INVALID KEY
                   CONTINUE
           END-READ.
           IF WS-FS-OUTPUT = '00'
               MOVE 'Y' TO WS-VSAM-FOUND-SW
           END-IF.
           IF WS-VSAM-FOUND
               PERFORM P4100-REWRITE-BALANCE THRU P4100-EXIT
           ELSE
               PERFORM P4200-ADD-BALANCE THRU P4200-EXIT
           END-IF.

       P4000-EXIT.
           EXIT.

       P4100-REWRITE-BALANCE.
      * ADD THE ADJUSTMENT TO THE EXISTING BALANCE.  THE AMOUNT IS
      * ROUNDED TO TWO PLACES HERE - THE FIFTH PLACE THAT CABJUR07
      * CARRIED ALL THE WAY THROUGH IS LOST AT THIS POINT AND
      * NOTHING RECORDS THE DIFFERENCE.
           MOVE BAL-RECORD TO WS-BALANCE-WORK.
           COMPUTE WS-BW-RESTATEMENT ROUNDED =
                   WS-BW-RESTATEMENT + BD-TOT-AMOUNT.
           COMPUTE WS-BW-TOTAL-DUE ROUNDED =
                   WS-BW-TOTAL-DUE + BD-TOT-AMOUNT.
           MOVE WS-CYCLE-YYDDD TO WS-BW-UPDATE-YYDDD.
           MOVE WS-PGM-NAME TO WS-BW-UPDATE-PGM.
           MOVE WS-BALANCE-WORK TO BAL-RECORD.
           REWRITE BAL-RECORD
               INVALID KEY
                   MOVE 5020 TO WS-AB-CODE
                   MOVE 'REWRITE FAILED ON BALANCE KSDS' TO WS-AB-TEXT
                   PERFORM P9500-ABEND THRU P9500-EXIT
           END-REWRITE.
           ADD 1 TO WS-VSAM-UPD-CNT.

       P4100-EXIT.
           EXIT.

       P4200-ADD-BALANCE.
      * NO BALANCE RECORD EXISTS FOR THIS ACCOUNT AND PERIOD.  ADD
      * ONE CARRYING THE ADJUSTMENT ONLY.
           MOVE SPACES TO WS-BALANCE-WORK.
           MOVE BD-BAN TO WS-BW-BAN.
           MOVE BD-BILL-PERIOD TO WS-BW-PERIOD.
           COMPUTE WS-BW-RESTATEMENT ROUNDED = BD-TOT-AMOUNT.
           COMPUTE WS-BW-TOTAL-DUE ROUNDED = BD-TOT-AMOUNT.
           MOVE WS-CYCLE-YYDDD TO WS-BW-UPDATE-YYDDD.
           MOVE WS-PGM-NAME TO WS-BW-UPDATE-PGM.
           MOVE WS-BALANCE-WORK TO BAL-RECORD.
           MOVE WS-BK-FULL-KEY TO BAL-KEY.
           WRITE BAL-RECORD
               INVALID KEY
                   MOVE 5021 TO WS-AB-CODE
                   MOVE 'WRITE FAILED ON BALANCE KSDS' TO WS-AB-TEXT
                   PERFORM P9500-ABEND THRU P9500-EXIT
           END-WRITE.
           ADD 1 TO WS-VSAM-ADD-CNT.

       P4200-EXIT.
           EXIT.

       P5000-COMMIT-CHECK.
      * COMMIT EVERY N ROWS.  THE COMMIT APPLIES TO DB2 ONLY.  THE
      * VSAM WRITES SINCE THE LAST COMMIT ARE ALREADY PERMANENT.
      * A RESTART AFTER AN ABEND THEREFORE REPLAYS DB2 WORK THAT
      * WAS ROLLED BACK AND REPLAYS VSAM WORK THAT WAS NOT.
           ADD 1 TO WS-SINCE-COMMIT.
           IF WS-SINCE-COMMIT < WS-PE-COMMIT-FREQ
               GO TO P5000-EXIT
           END-IF.
           EXEC SQL
               COMMIT
           END-EXEC.
           IF SQLCODE NOT = 0
               MOVE 5030 TO WS-AB-CODE
               MOVE 'COMMIT FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT
           END-IF.
           ADD 1 TO WS-COMMIT-CNT.
           MOVE ZERO TO WS-SINCE-COMMIT.

       P5000-EXIT.
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
           MOVE CABS-BILL-DETAIL TO SU-ORIG-RECORD.
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
           MOVE 100                    TO CT-STEP-SEQ.
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
           DISPLAY 'ROWS INSERTED    ' WS-SQL-INS-CNT.
           DISPLAY 'ROWS UPDATED     ' WS-SQL-UPD-CNT.
           DISPLAY 'VSAM REWRITTEN   ' WS-VSAM-UPD-CNT.
           DISPLAY 'VSAM ADDED       ' WS-VSAM-ADD-CNT.
           DISPLAY 'COMMITS TAKEN    ' WS-COMMIT-CNT.
           DISPLAY 'DUPLICATE ROWS   ' WS-DUP-ROW-CNT.
           DISPLAY 'DEADLOCK RETRIES ' WS-RETRY-CNT.
           DISPLAY 'POSTED AMOUNT    ' WS-TOT-POST-AMT.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE ADJUST-IN-FILE
                 BALANCE-MASTER
                 PARM-FILE
                 CONTROL-FILE
                 SUSPENSE-FILE
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

