      *****************************************************************
      * CABSET13 - SETTLEMENT POSTING TO DB2 AND SETTLEMENT MASTER    *
      * APPLICATION : SETL                                            *
      * COMPILER    : ENTERPRISE COBOL                                *
      * INPUTS      : SETLADD  TELCABS.SETL.SETTLE.ADD(0)     CABSSETL*
      * INPUTS      : SYSIN    INSTREAM                       NONE    *
      * OUTPUTS     : SETLMAST TELCABS.SETL.SETTLE.MASTER     CABSSETL*
      * OUTPUTS     : DB2      SETLTRAN TABLE (DSNSETL)       DCLGEN  *
      * CONTROL     : CTLOUT                         CABSCTL          *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED              *
      *               ROWS INSERTED MUST EQUAL VSAM RECORDS ADDED     *
      * RESTART     : FULL RERUN NOT SAFE - SEE P4000                 *
      * COMPILED WITH ENTERPRISE COBOL AND THE DB2                    *
      * PRECOMPILER.  SCOPE TERMINATORS PERMITTED HERE.               *
      * THE POSTING ORDER IS FIXED BY CABS-STD-071.                   *
      * REVISION HISTORY                                              *
      *   V1.00  2000-02-29  P.NAIR        INITIAL                    *
      *   V1.03  2004-01-12  P.NAIR        COMMIT FREQUENCY FROM CARD *
      *   V1.06  2008-04-28  A.BUKOWSKI    DUPLICATE KEY TOLERATED    *
      *   V2.00  2012-08-16  A.BUKOWSKI    RESYNC UTILITY WRITTEN     *
      *   V2.01  2013-01-09  A.BUKOWSKI    RESYNC NEVER SCHEDULED     *
      *   V2.03  2018-06-05  M.OYELARAN    RECOMPILE ONLY             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CABSET13.
       AUTHOR.        P.NAIR.
       DATE-WRITTEN.  2000-02-29.
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
      * NEW SETTLEMENT RECORDS FROM EVERY SETTLEMENT KIND
           SELECT SETTLE-ADD-FILE
               ASSIGN TO UT-S-SETLADD
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
      * SETTLEMENT MASTER KSDS - THE SECOND STORE
           SELECT SETTLE-MASTER
               ASSIGN TO DA-I-SETLMAST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS STM-KEY
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
       FD  SETTLE-ADD-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 180 CHARACTERS
               DATA RECORD IS SAI-RECORD.
       01  SAI-RECORD              PIC X(180).

       FD  SETTLE-MASTER
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 180 CHARACTERS
               DATA RECORD IS STM-RECORD.
       01  STM-RECORD.
           05  STM-KEY                 PIC X(20).
           05  STM-DATA                PIC X(160).

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
           05  WS-PGM-NAME         PIC X(08)         VALUE 'CABSET13'.
           05  WS-PGM-VERSION        PIC X(05)           VALUE 'V2.03'.
           05  WS-PGM-APPL           PIC X(04)           VALUE 'SETL'.
           05  WS-PGM-COMPILE      PIC X(08)         VALUE '20180605'.
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
           05  WS-PE-VSAM-ONLY         PIC X(01).
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

      * SUBSCRIPTS AND INDEX WORK FIELDS.
       01  WS-SUBSCRIPTS.
           05  WS-SUB1                 PIC S9(05) COMP-3     VALUE 0.
           05  WS-SUB2                 PIC S9(05) COMP-3     VALUE 0.

      * DB2 HOST VARIABLES FOR THE SETLTRAN TABLE.  THE TABLE
      * IS THE REPORTING COPY OF THE SETTLEMENT MASTER.  IT IS
      * MEANT TO BE AN EXACT COPY.  IT IS NOT ALWAYS ONE.
       01  WS-HOST-VARIABLES.
           05  WS-HV-TYPE              PIC X(01).
           05  WS-HV-OCN               PIC X(04).
           05  WS-HV-PERIOD            PIC S9(09) COMP.
           05  WS-HV-SEQ               PIC S9(09) COMP.
           05  WS-HV-TOTAL-MOU         PIC S9(15)V9(02) COMP-3.
           05  WS-HV-BILL-MOU          PIC S9(15)V9(02) COMP-3.
           05  WS-HV-CAP-MOU           PIC S9(15)V9(02) COMP-3.
           05  WS-HV-RATE              PIC S9(05)V9(05) COMP-3.
           05  WS-HV-GROSS             PIC S9(13)V9(05) COMP-3.
           05  WS-HV-OUR-SHARE         PIC S9(13)V9(05) COMP-3.
           05  WS-HV-THEIR-SHARE       PIC S9(13)V9(05) COMP-3.
           05  WS-HV-NET-DUE           PIC S9(13)V9(02) COMP-3.
           05  WS-HV-DIRECTION         PIC X(01).
           05  WS-HV-RUN-ID            PIC X(12).

      * POSTING COUNTERS.  WHEN THE INSERT COUNT AND THE VSAM
      * ADD COUNT DIVERGE THE TWO STORES ARE OUT OF STEP AND
      * THE RESYNC UTILITY HAS TO BE RUN BY HAND.  IT WAS
      * WRITTEN IN 2012 AND HAS NEVER BEEN SCHEDULED.
       01  WS-POST-COUNTERS.
           05  WS-SQL-INS-CNT          PIC S9(09) COMP-3     VALUE 0.
           05  WS-SQL-UPD-CNT          PIC S9(09) COMP-3     VALUE 0.
           05  WS-VSAM-ADD-CNT         PIC S9(09) COMP-3     VALUE 0.
           05  WS-VSAM-UPD-CNT         PIC S9(09) COMP-3     VALUE 0.
           05  WS-DUP-CNT              PIC S9(09) COMP-3     VALUE 0.
           05  WS-COMMIT-CNT           PIC S9(09) COMP-3     VALUE 0.
           05  WS-SINCE-COMMIT         PIC S9(09) COMP-3     VALUE 0.
           05  WS-TOT-POST-AMT         PIC S9(15)V9(02) COMP-3 VALUE 0.

      * SETTLEMENT KEY BUILD AREA WITH TWO REDEFINES.
       01  WS-SETL-KEY.
           05  WS-SK-TYPE            PIC X(01)           VALUE SPACES.
           05  WS-SK-OCN             PIC X(04)           VALUE SPACES.
           05  WS-SK-PERIOD            PIC 9(06)             VALUE 0.
           05  WS-SK-SEQ               PIC 9(09)             VALUE 0.
       01  WS-SETL-KEY-A REDEFINES WS-SETL-KEY.
           05  WS-SKA-PREFIX           PIC X(05).
           05  WS-SKA-REST             PIC X(15).
       01  WS-SETL-KEY-B REDEFINES WS-SETL-KEY.
           05  WS-SKB-ALL              PIC X(20).

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
                   VALUE 'ROW INSERTED INTO SETLTRAN                  '.
           05  FILLER              PIC X(44)
                   VALUE 'ROW UPDATED IN SETLTRAN                     '.
           05  FILLER              PIC X(44)
                   VALUE 'DUPLICATE ROW TOLERATED SINCE 2008          '.
           05  FILLER              PIC X(44)
                   VALUE 'VSAM RECORD ADDED                           '.
           05  FILLER              PIC X(44)
                   VALUE 'VSAM RECORD REWRITTEN                       '.
           05  FILLER              PIC X(44)
                   VALUE 'COMMIT TAKEN                                '.
           05  FILLER              PIC X(44)
                   VALUE 'SQL ERROR - RUN TERMINATED                  '.
           05  FILLER              PIC X(44)
                   VALUE 'VSAM WRITTEN BUT DB2 NOT COMMITTED          '.
           05  FILLER              PIC X(44)
                   VALUE 'RESYNC UTILITY REQUIRED                     '.
           05  FILLER              PIC X(44)
                   VALUE 'END OF POSTING RUN                          '.
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
       COPY CABSCIRC.

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
           OPEN INPUT  SETTLE-ADD-FILE
                       PARM-FILE
           OPEN OUTPUT CONTROL-FILE
                       SUSPENSE-FILE
           OPEN I-O    SETTLE-MASTER
           .
           IF WS-FS-INPUT NOT = '00'
               MOVE 6301 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-SETLADD' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 6302 TO WS-AB-CODE
               MOVE 'OPEN FAILED DA-I-SETLMAST' TO WS-AB-TEXT
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
                        WS-VSAM-ADD-CNT WS-VSAM-UPD-CNT
                        WS-DUP-CNT WS-COMMIT-CNT
                        WS-SINCE-COMMIT WS-TOT-POST-AMT.
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
               MOVE 01000 TO WS-PE-COMMIT-FREQ.
           IF WS-PE-COMMIT-FREQ = ZERO
               MOVE 01000 TO WS-PE-COMMIT-FREQ.
           IF WS-PE-VSAM-ONLY NOT = 'Y'
               MOVE 'N' TO WS-PE-VSAM-ONLY.

       P1200-EXIT.
           EXIT.


      *****************************************************************
      * S200-POSTING                                                  *
      * POST TO BOTH STORES.                                          *
      *****************************************************************
       S200-POSTING SECTION.

       P2000-PROCESS.
      * ONE SETTLEMENT RECORD PER PASS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ THRU P2100-EXIT.
           IF WS-EOF
               GO TO P2000-EXIT.
           MOVE SAI-RECORD TO CABS-SETTLEMENT-RECORD.
           MOVE ST-KEY TO WS-RESTART-KEY.
           PERFORM P2200-BUILD-HOST THRU P2200-EXIT.
           PERFORM P3000-POST-DB2 THRU P3000-EXIT.
           IF WS-SQL-BAD
               GO TO P2000-EXIT
           END-IF.
           PERFORM P4000-POST-VSAM THRU P4000-EXIT.
           PERFORM P5000-COMMIT-CHECK THRU P5000-EXIT.
           ADD 1 TO WS-WRITE-CNT.
           ADD ST-NET-DUE TO WS-TOT-POST-AMT.
           ADD ST-NET-DUE TO WS-ACC-AMOUNT.
           ADD ST-TOTAL-MOU TO WS-ACC-MINUTES.

       P2000-EXIT.
           EXIT.

       P2100-READ.
      * SEQUENTIAL READ OF THE ADD FILE.
           READ SETTLE-ADD-FILE
               AT END
                   MOVE 'Y' TO WS-EOF-SW
                   GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00'
               MOVE 3630 TO WS-AB-CODE
               MOVE 'READ ERROR UT-S-SETLADD' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT.
           ADD 1 TO WS-READ-CNT.

       P2100-EXIT.
           EXIT.

       P2200-BUILD-HOST.
      * MOVE TO THE HOST VARIABLES.
           MOVE ST-SETTLE-TYPE TO WS-HV-TYPE.
           MOVE ST-COUNTERPARTY-OCN TO WS-HV-OCN.
           MOVE ST-SETTLE-PERIOD TO WS-HV-PERIOD.
           MOVE ST-SEQ TO WS-HV-SEQ.
           MOVE ST-TOTAL-MOU TO WS-HV-TOTAL-MOU.
           MOVE ST-BILLABLE-MOU TO WS-HV-BILL-MOU.
           MOVE ST-CAPPED-MOU TO WS-HV-CAP-MOU.
           MOVE ST-RATE-APPLIED TO WS-HV-RATE.
           MOVE ST-GROSS-AMT TO WS-HV-GROSS.
           MOVE ST-OUR-SHARE TO WS-HV-OUR-SHARE.
           MOVE ST-THEIR-SHARE TO WS-HV-THEIR-SHARE.
           MOVE ST-NET-DUE TO WS-HV-NET-DUE.
           MOVE ST-DIRECTION TO WS-HV-DIRECTION.

       P2200-EXIT.
           EXIT.

       P3000-POST-DB2.
      * INSERT THE SETTLEMENT ROW.  A DUPLICATE HAS BEEN TOLERATED
      * SINCE 2008 SO THAT A RERUN DOES NOT FAIL.  THE VSAM SIDE
      * HAS THE SAME TOLERANCE BUT IMPLEMENTS IT DIFFERENTLY - THE
      * ROW IS UPDATED AND THE VSAM RECORD IS REWRITTEN, AND THE
      * TWO PATHS ARE NOT GUARANTEED TO BE TAKEN TOGETHER.
           MOVE 'P3000-POST-DB2' TO WS-PARA-NAME.
           MOVE 'Y' TO WS-SQL-OK-SW.
           IF WS-PE-VSAM-ONLY = 'Y'
               GO TO P3000-EXIT
           END-IF.
           EXEC SQL
               INSERT INTO SETLTRAN
                     (SETTLE_TYPE, OCN, SETTLE_PERIOD, SEQ_NBR,
                      TOTAL_MOU, BILLABLE_MOU, CAPPED_MOU,
                      RATE_APPLIED, GROSS_AMT, OUR_SHARE,
                      THEIR_SHARE, NET_DUE, DIRECTION, RUN_ID)
               VALUES (:WS-HV-TYPE, :WS-HV-OCN, :WS-HV-PERIOD,
                       :WS-HV-SEQ, :WS-HV-TOTAL-MOU,
                       :WS-HV-BILL-MOU, :WS-HV-CAP-MOU,
                       :WS-HV-RATE, :WS-HV-GROSS,
                       :WS-HV-OUR-SHARE, :WS-HV-THEIR-SHARE,
                       :WS-HV-NET-DUE, :WS-HV-DIRECTION,
                       :WS-HV-RUN-ID)
           END-EXEC.
           EVALUATE SQLCODE
               WHEN 0
                   ADD 1 TO WS-SQL-INS-CNT
               WHEN -803
                   ADD 1 TO WS-DUP-CNT
                   PERFORM P3200-UPDATE-ROW THRU P3200-EXIT
               WHEN OTHER
                   MOVE 6310 TO WS-AB-CODE
                   MOVE 'SQL ERROR ON INSERT SETLTRAN' TO WS-AB-TEXT
                   DISPLAY 'SQLCODE ' SQLCODE
                   PERFORM P9500-ABEND THRU P9500-EXIT
           END-EVALUATE.

       P3000-EXIT.
           EXIT.

       P3200-UPDATE-ROW.
      * THE ROW EXISTS.  REPLACE THE AMOUNTS.
           EXEC SQL
               UPDATE SETLTRAN
                  SET TOTAL_MOU = :WS-HV-TOTAL-MOU,
                      BILLABLE_MOU = :WS-HV-BILL-MOU,
                      CAPPED_MOU = :WS-HV-CAP-MOU,
                      GROSS_AMT = :WS-HV-GROSS,
                      NET_DUE = :WS-HV-NET-DUE,
                      RUN_ID = :WS-HV-RUN-ID
                WHERE SETTLE_TYPE = :WS-HV-TYPE
                  AND OCN = :WS-HV-OCN
                  AND SETTLE_PERIOD = :WS-HV-PERIOD
                  AND SEQ_NBR = :WS-HV-SEQ
           END-EXEC.
           IF SQLCODE = 0
               ADD 1 TO WS-SQL-UPD-CNT
           ELSE
               MOVE 6311 TO WS-AB-CODE
               MOVE 'SQL ERROR ON UPDATE SETLTRAN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT
           END-IF.

       P3200-EXIT.
           EXIT.

       P4000-POST-VSAM.
      * WRITE THE SAME SETTLEMENT INTO THE VSAM MASTER.  SECOND
      * STORE, NO COORDINATION.  IF THE STEP ABENDS AFTER THIS
      * WRITE AND BEFORE THE NEXT COMMIT, THE VSAM MASTER HAS A
      * SETTLEMENT THAT DB2 HAS NEVER HEARD OF.  THE REPORTING
      * COPY IS THEN SHORT AND EVERY REPORT DRAWN FROM DB2
      * UNDERSTATES THE POSITION.
           MOVE 'P4000-POST-VSAM' TO WS-PARA-NAME.
           MOVE SPACES TO WS-SETL-KEY.
           MOVE ST-SETTLE-TYPE TO WS-SK-TYPE.
           MOVE ST-COUNTERPARTY-OCN TO WS-SK-OCN.
           MOVE ST-SETTLE-PERIOD TO WS-SK-PERIOD.
           MOVE ST-SEQ TO WS-SK-SEQ.
           MOVE WS-SKB-ALL TO STM-KEY.
           MOVE 'N' TO WS-VSAM-FOUND-SW.
           READ SETTLE-MASTER
               INVALID KEY
                   CONTINUE
           END-READ.
           IF WS-FS-OUTPUT = '00'
               MOVE 'Y' TO WS-VSAM-FOUND-SW
           END-IF.
           MOVE CABS-SETTLEMENT-RECORD TO STM-RECORD.
           MOVE WS-SKB-ALL TO STM-KEY.
           IF WS-VSAM-FOUND
               REWRITE STM-RECORD
                   INVALID KEY
                       MOVE 6320 TO WS-AB-CODE
                       MOVE 'REWRITE FAILED ON SETTLE MASTER'
                            TO WS-AB-TEXT
                       PERFORM P9500-ABEND THRU P9500-EXIT
               END-REWRITE
               ADD 1 TO WS-VSAM-UPD-CNT
           ELSE
               WRITE STM-RECORD
                   INVALID KEY
                       MOVE 6321 TO WS-AB-CODE
                       MOVE 'WRITE FAILED ON SETTLE MASTER'
                            TO WS-AB-TEXT
                       PERFORM P9500-ABEND THRU P9500-EXIT
               END-WRITE
               ADD 1 TO WS-VSAM-ADD-CNT
           END-IF.

       P4000-EXIT.
           EXIT.

       P5000-COMMIT-CHECK.
      * COMMIT THE DB2 WORK ON A COUNT.  VSAM IS OUTSIDE THE SCOPE.
           ADD 1 TO WS-SINCE-COMMIT.
           IF WS-SINCE-COMMIT < WS-PE-COMMIT-FREQ
               GO TO P5000-EXIT
           END-IF.
           EXEC SQL
               COMMIT
           END-EXEC.
           IF SQLCODE NOT = 0
               MOVE 6330 TO WS-AB-CODE
               MOVE 'COMMIT FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT
           END-IF.
           ADD 1 TO WS-COMMIT-CNT.
           MOVE ZERO TO WS-SINCE-COMMIT.

       P5000-EXIT.
           EXIT.

       P5500-RESYNC-CHECK.
      * COMPARE THE TWO COUNTS AT END OF RUN.  A DIVERGENCE MEANS
      * THE RESYNC UTILITY MUST BE RUN.  THE UTILITY EXISTS AND HAS
      * NEVER BEEN SCHEDULED, SO IN PRACTICE THIS MESSAGE IS THE
      * ONLY THING THAT HAPPENS.
           COMPUTE WS-SUB1 = WS-SQL-INS-CNT + WS-SQL-UPD-CNT.
           COMPUTE WS-SUB2 = WS-VSAM-ADD-CNT + WS-VSAM-UPD-CNT.
           IF WS-SUB1 NOT = WS-SUB2
               DISPLAY WS-MSG-TEXT (9)
               DISPLAY 'DB2 ' WS-SUB1 ' VSAM ' WS-SUB2
               MOVE 0004 TO WS-RETURN-CODE
           END-IF.

       P5500-EXIT.
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
           MOVE CABS-SETTLEMENT-RECORD TO SU-ORIG-RECORD.
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
           MOVE 320                    TO CT-STEP-SEQ.
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
           PERFORM P5500-RESYNC-CHECK THRU P5500-EXIT.
           DISPLAY 'ROWS INSERTED    ' WS-SQL-INS-CNT.
           DISPLAY 'ROWS UPDATED     ' WS-SQL-UPD-CNT.
           DISPLAY 'VSAM ADDED       ' WS-VSAM-ADD-CNT.
           DISPLAY 'VSAM REWRITTEN   ' WS-VSAM-UPD-CNT.
           DISPLAY 'DUPLICATE ROWS   ' WS-DUP-CNT.
           DISPLAY 'COMMITS TAKEN    ' WS-COMMIT-CNT.
           DISPLAY 'POSTED AMOUNT    ' WS-TOT-POST-AMT.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'RECORDS SUMMARISED' WS-SUMM-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
           DISPLAY '--------------------------------------------'.
           CLOSE SETTLE-ADD-FILE
                 SETTLE-MASTER
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

