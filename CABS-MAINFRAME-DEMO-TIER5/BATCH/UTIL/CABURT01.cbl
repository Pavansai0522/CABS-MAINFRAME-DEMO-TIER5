      *****************************************************************
      * CABURT01 - RATE ELEMENT DESCRIPTION MAINTENANCE               *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               TARIN   TELCABS.CABS.TARIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               TBLOUT  TELCABS.CABS.TBLOUT         (LOCAL)     *
      *               RATOUT  TELCABS.CABS.RATOUT         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - NOTHING IS UPDATED IN PLACE        *
      * REVISION HISTORY                                              *
      *   V1.00  1987-02-08  R.T.WHEELER  INITIAL RELEASE             *
      *   V1.04  1992-09-23  G.PRZYBYLSKI CENTURY PIVOT APPLIED TO THE*
      *                      CYCLE DATE                               *
      *   V1.07  1993-04-04  K.O.BRIEN    SUSPENSE WRITE ADDED -      *
      *                      RECORDS WERE BEING DROPPED               *
      *   V1.09  2002-07-01  A.BUKOWSKI   REPORT PAGINATION CORRECTED *
      *   V1.13  2004-12-28  P.NAIR       JOB PARAMETER MADE MANDATORY*
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABURT01.
       AUTHOR. TELCABS APPLICATIONS - RATE TABLE MAINTENANCE.
      *****************************************************************
      * RATE ELEMENT DESCRIPTION MAINTENANCE. THE STEP RUNS ONCE PER  *
      * BILL CYCLE AND IS RERUN FROM THE TOP IF IT FAILS.             *
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL*
      * CHARACTER CARRIES MEANING DOWNSTREAM.                         *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           C01 IS TO-NEW-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TARIN ASSIGN TO UT-S-TARIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT TBLOUT ASSIGN TO UT-S-TBLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT RATOUT ASSIGN TO UT-S-RATOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
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
      * TARIN - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  TARIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 150 CHARACTERS.
       01  CABS-AD-IN-RECORD.
           05  IA-TARIFF                   PIC S9(11)V9(05) COMP-3.
           05  IA-LEVEL                    PIC S9(13) COMP-3.
           05  IA-TYPE                     PIC 9(06).
           05  IA-ELEM                     PIC S9(11)V9(05) COMP-3.
           05  IA-SOURCE                   PIC 9(04).
           05  IA-CLASS                    PIC 9(06).
           05  IA-SEGMENT                  PIC 9(04).
           05  IA-MEDIA                    PIC 9(06).
           05  IA-CARRIER                  PIC X(20).
           05  IA-CENTRE                   PIC X(04).
           05  IA-CYCLE                    PIC X(10).
           05  IA-JURIS                    PIC 9(02).
           05  IA-OCN                      PIC X(03).
           05  IA-ELEM2                    PIC X(16).
           05  IA-ACCOUNT                  PIC S9(09)V9(02) COMP-3.
           05  IA-OCN2                     PIC X(06).
           05  IA-TARGET                   PIC 9(09).
           05  IA-CODE                     PIC X(13).
           05  IA-ACCOUNT2                 PIC X(06).
           05  AD-FILL-01                  PIC X(4).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-AD-VIEW1 REDEFINES CABS-AD-IN-RECORD.
           05  R0A-GROUP                   PIC S9(11)V9(02) COMP-3.
           05  R0A-TARIFF                  PIC S9(07)V9(02) COMP-3.
           05  R0A-OCN                     PIC X(10).
           05  R0A-SEGMENT                 PIC S9(07)V9(02) COMP-3.
           05  R0A-TARGET                  PIC 9(02).
           05  R0A-TYPE                    PIC 9(04).
           05  R0A-STATUS                  PIC 9(04).
           05  R0A-SEQ                     PIC S9(07) COMP-3.
           05  R0A-REST                    PIC X(109).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-AD-VIEW2 REDEFINES CABS-AD-IN-RECORD.
           05  R1A-CYCLE                   PIC S9(13) COMP-3.
           05  R1A-CYCLE2                  PIC X(08).
           05  R1A-BAN                     PIC X(04).
           05  R1A-INVOICE                 PIC S9(09)V9(02) COMP-3.
           05  R1A-INVOICE2                PIC X(13).
           05  R1A-JURIS                   PIC X(16).
           05  R1A-BAND                    PIC S9(13) COMP-3.
           05  R1A-BAN2                    PIC X(02).
           05  R1A-ELEM                    PIC X(20).
           05  R1A-REST                    PIC X(67).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-AD-VIEW3 REDEFINES CABS-AD-IN-RECORD.
           05  R2A-LEVEL                   PIC X(04).
           05  R2A-LEVEL2                  PIC X(20).
           05  R2A-INVOICE                 PIC 9(09).
           05  R2A-TYPE                    PIC X(03).
           05  R2A-OCN                     PIC S9(09) COMP-3.
           05  R2A-CLASS                   PIC X(06).
           05  R2A-CARRIER                 PIC S9(09)V9(02) COMP-3.
           05  R2A-REST                    PIC X(97).
      * TBLOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  TBLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-AD-OUT-RECORD.
           05  OA-STATUS                   PIC 9(06).
           05  OA-ACCOUNT                  PIC X(03).
           05  OA-REGION                   PIC S9(13)V9(02) COMP-3.
           05  OA-LEVEL                    PIC X(10).
           05  OA-CARRIER                  PIC S9(13)V9(05) COMP-3.
           05  OA-STATUS2                  PIC X(03).
           05  OA-CLASS                    PIC 9(07).
           05  OA-TARGET                   PIC X(04).
           05  OA-JURIS                    PIC X(10).
           05  OA-SEGMENT                  PIC S9(07) COMP-3.
           05  AD-FILL-02                  PIC X(15).
      * RATOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  RATOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-AD-OUT1-RECORD         PIC X(80).
      * CTLOUT - PERMANENT DATASET HELD ON DASD.
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD              PIC X(180).
      * RPTOUT - WORK FILE, DELETED AT STEP END.
       FD  RPTOUT
           RECORDING MODE IS FA
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       COPY CABSPRNT.
       WORKING-STORAGE SECTION.
      * STANDARD SHARED WORKING STORAGE. SEE CABSWRK.
       COPY CABSWRK.
      * SHARED LAYOUT PULLED IN FOR THE TARIFF SIDE.
       COPY CABSRATE.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABURT01'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V3.03'.
           05  WS-TABLE-LIMIT              PIC S9(04) COMP-3
                                         VALUE 150.
      * SYSIN PARM CARD. POSITIONAL LAYOUT ONLY.
       01  WS-PARM-CARD                    PIC X(80).
       01  WS-PARM-CARD-R1 REDEFINES WS-PARM-CARD.
           05  PC1-REC-ID                  PIC X(02).
           05  PC1-RUN-ID                  PIC X(12).
           05  PC1-CYCLE-YYDDD             PIC 9(05).
           05  PC1-BILL-PERIOD             PIC 9(06).
           05  PC1-JOBNAME                 PIC X(08).
           05  PC1-STEPNAME                PIC X(08).
           05  PC1-OPT-ONE                 PIC X(01).
           05  PC1-OPT-TWO                 PIC X(01).
           05  PC1-FILLER                  PIC X(37).
       01  WS-COUNT-AREA.
           05  WS-AD-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AD-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AD-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AD-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AD-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AD-CNT-06                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AD-CNT-07                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AD-CNT-08                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-AD-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AD-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AD-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-AD-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AD-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AD-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AD-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AD-AMT-05                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-AD-TXT-01                PIC X(08) VALUE SPACES.
           05  WS-AD-TXT-02                PIC X(16) VALUE SPACES.
           05  WS-AD-TXT-03                PIC X(20) VALUE SPACES.
           05  WS-AD-TXT-04                PIC X(12) VALUE SPACES.
           05  WS-AD-TXT-05                PIC X(12) VALUE SPACES.
           05  WS-AD-TXT-06                PIC X(10) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-AD-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-AD-ON-01                 VALUE 'Y'.
               88  WS-AD-OFF-01                VALUE 'N'.
           05  WS-AD-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-AD-ON-02                 VALUE 'Y'.
               88  WS-AD-OFF-02                VALUE 'N'.
           05  WS-AD-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-AD-ON-03                 VALUE 'Y'.
               88  WS-AD-OFF-03                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-AD-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-AD-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-AD-SUB-03                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. LOADED ONCE AT INITIALISATION AND SEARCHED WITH
      * A SUBSCRIPTED WALK.
       01  WS-AD-TABLE.
           05  WS-AD-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-AD-TB-ENTRY OCCURS 150 TIMES
                                       INDEXED BY WS-AD-IX.
               10  WS-AD-TB-KEY                PIC X(08).
               10  WS-AD-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-AD-TB-TXT                PIC X(40).
               10  WS-AD-TB-EFF                PIC 9(05).
               10  WS-AD-TB-EXP                PIC 9(05).
       01  WS-AD-WORK-GROUP-1.
           05  WS-AD-G1-CARRIER            PIC X(10).
           05  WS-AD-G1-BAND               PIC S9(11)V9(02) COMP-3.
           05  WS-AD-G1-CYCLE              PIC 9(07).
           05  WS-AD-G1-BAND               PIC 9(05).
       01  WS-AD-WORK-GROUP-2.
           05  WS-AD-G2-CARRIER            PIC S9(11)V9(02) COMP-3.
           05  WS-AD-G2-CODE               PIC 9(05).
           05  WS-AD-G2-CARRIER            PIC X(10).
           05  WS-AD-G2-SEQ                PIC S9(09) COMP-3.
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 55.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABURT01 - RATE ELEMENT DESCRIPTION MAINTENANCE'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-AD-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-AD-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9982.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-AD-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-AD-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
       PROCEDURE DIVISION.
      * P0000-MAINLINE - MANDATORY CABS BATCH SHAPE. ONE PASS OF
      * P2000-PROCESS CONSUMES ONE INPUT RECORD.
       P0000-MAINLINE.
           PERFORM P1000-INIT THRU P1000-EXIT.
           PERFORM P2000-PROCESS THRU P2000-EXIT UNTIL WS-EOF.
           PERFORM P8000-CONTROL THRU P8000-EXIT.
           PERFORM P9000-TERM THRU P9000-EXIT.
           STOP RUN.
      * S100-INITIALISATION SECTION
       S100-INITIALISATION SECTION.
       P1000-INIT.
           PERFORM P1100-OPEN-FILES THRU P1100-EXIT.
           PERFORM P1200-READ-PARM THRU P1200-EXIT.
           PERFORM P1300-LOAD-TABLE THRU P1300-EXIT.
           PERFORM P1400-PRIME-READ THRU P1400-EXIT.
       P1000-EXIT.
           EXIT.
       P1100-OPEN-FILES.
           OPEN INPUT TARIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'TARIN OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT TBLOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'TBLOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RATOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RATOUT OPEN FAILED - FILE STATUS BAD' TO
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
       P1100-EXIT.
           EXIT.
      * P1200-READ-PARM - THE CYCLE DATE ARRIVES AS TWO DIGITS AND IS
      * PIVOTED ON DW-PIVOT-YY BEFORE ANY DATE MATH.
       P1200-READ-PARM.
           MOVE SPACES TO WS-PARM-CARD.
           ACCEPT WS-PARM-CARD FROM SYSIN.
           CALL 'CABPARMR' USING WS-PARM-CARD WS-RC-PARMR.
           MOVE PC1-CYCLE-YYDDD TO WS-AD-CYCLE-YYDDD.
           COMPUTE WS-AD-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-AD-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-AD-CNT-06.
           MOVE 0 TO WS-AD-CNT-04.
           MOVE 0 TO WS-AD-CNT-01.
       P1200-EXIT.
           EXIT.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-AD-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-AD-TAB-CNT NOT < 150
               MOVE 'Y' TO WS-AD-SW-01
               ADD 1 TO WS-AD-CNT-06
           ELSE
               ADD 1 TO WS-AD-TAB-CNT
               SET WS-AD-IX TO WS-AD-TAB-CNT
               MOVE IA-TARGET TO WS-AD-TB-KEY (WS-AD-IX)
               MOVE 0 TO WS-AD-TB-VAL (WS-AD-IX)
               MOVE SPACES TO WS-AD-TB-TXT (WS-AD-IX)
               ADD 1 TO WS-AD-CNT-01.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ TARIN
               AT END MOVE 'Y' TO WS-AD-SW-01.
       P1320-EXIT.
           EXIT.
       P1400-PRIME-READ.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P1400-EXIT.
           EXIT.
       P9900-FATAL-OPEN.
           MOVE WS-PGM-NAME TO WS-AB-PGM.
           CALL 'CABABEND' USING WS-AB-PGM WS-AB-PARA WS-AB-REASON
               WS-AB-USER-CODE WS-RC-ABEND.
       P9900-EXIT.
           EXIT.
      * S200-MAIN-PROCESSING SECTION - ONE PASS PER INPUT RECORD.
       S200-MAIN-PROCESSING SECTION.
       P2000-PROCESS.
           ADD 1 TO WS-READ-CNT.
           PERFORM P2200-EDIT-WINDOW THRU P2200-EDIT-WINDOW-EXIT.
           PERFORM P2300-BUILD-TARIFF THRU P2300-BUILD-TARIFF-EXIT.
           PERFORM P2400-APPLY-OVERRIDE THRU P2400-APPLY-OVERRIDE-EXIT.
           IF WS-AD-ON-03
               PERFORM P2500-RESOLVE-OVERRIDE THRU
                   P2500-RESOLVE-OVERRIDE-EXIT.
           PERFORM P2600-SELECT-TARIFF THRU P2600-SELECT-TARIFF-EXIT.
           PERFORM P2700-EDIT-OVERRIDE THRU P2700-EDIT-OVERRIDE-EXIT.
           IF WS-AD-ON-02
               PERFORM P2800-SELECT-KEY THRU P2800-SELECT-KEY-EXIT.
           PERFORM P2900-EDIT-ROW THRU P2900-EDIT-ROW-EXIT.
           IF WS-AD-ON-03
               PERFORM P21000-EDIT-ELEMENT THRU
                   P21000-EDIT-ELEMENT-EXIT.
           IF WS-AD-ON-01
               PERFORM P21100-BUILD-ELEMENT THRU
                   P21100-BUILD-ELEMENT-EXIT.
           IF WS-AD-ON-02
               PERFORM P21200-CONVERT-KEY THRU P21200-CONVERT-KEY-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ TARIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P2200-EDIT-WINDOW.
           CALL 'CABHASH' USING IA-ELEM2 WS-ACC-OCN-HASH.
           ADD WS-AD-CNT-06 TO WS-ACC-SEQ-HASH.
           MOVE 'N' TO WS-AD-SW-01.
           IF WS-AD-TAB-CNT > 0
               PERFORM P260-COMPARE-DESCRIPTION THRU
                   P260-COMPARE-DESCRIPTION-EXIT
               VARYING WS-AD-SUB-01 FROM 1 BY 1
               UNTIL WS-AD-SUB-01 > WS-AD-TAB-CNT
               OR WS-AD-SW-01 = 'Y'.
       P2200-EDIT-WINDOW-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P2300-BUILD-TARIFF.
           IF IA-ACCOUNT = 'X'
               ADD 1 TO WS-AD-CNT-07
           ELSE
               IF IA-ACCOUNT = 'C'
                   ADD 1 TO WS-AD-CNT-06
               ELSE
                   IF IA-ACCOUNT = 'B'
                       ADD 1 TO WS-AD-CNT-05
                   ELSE
                       ADD 1 TO WS-AD-CNT-03.
       P2300-BUILD-TARIFF-EXIT.
           EXIT.
       P2400-APPLY-OVERRIDE.
           MOVE 'N' TO WS-AD-SW-01.
           IF WS-AD-TXT-05 NOT = WS-AD-TXT-01
               MOVE 'Y' TO WS-AD-SW-01
               MOVE WS-AD-TXT-05 TO WS-AD-TXT-01
               ADD 1 TO WS-AD-CNT-04.
       P2400-APPLY-OVERRIDE-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P2500-RESOLVE-OVERRIDE.
           CALL 'CABEDITF' USING WS-AD-TXT-01 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-AD-CNT-01.
       P2500-RESOLVE-OVERRIDE-EXIT.
           EXIT.
       P2600-SELECT-TARIFF.
           UNSTRING WS-AD-TXT-06 DELIMITED BY '/'
               INTO WS-AD-TXT-03
               WS-AD-TXT-05
               TALLYING IN WS-AD-CNT-02.
       P2600-SELECT-TARIFF-EXIT.
           EXIT.
       P2700-EDIT-OVERRIDE.
           MOVE WS-AD-AMT-02 TO WS-AD-AMT-02.
           IF WS-AD-AMT-02 < 0
               COMPUTE WS-AD-AMT-02 = 0 - WS-AD-AMT-02.
       P2700-EDIT-OVERRIDE-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P2800-SELECT-KEY.
           MOVE SPACES TO WS-AD-TXT-02.
           STRING IA-CYCLE DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IA-CENTRE DELIMITED BY SIZE
               INTO WS-AD-TXT-02.
       P2800-SELECT-KEY-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P2900-EDIT-ROW.
           ADD IA-LEVEL TO WS-AD-QTY-02.
           COMPUTE WS-AD-AMT-01 ROUNDED = WS-AD-QTY-02 * WS-AD-QTY-02.
           ADD WS-AD-AMT-01 TO WS-AD-AMT-01.
       P2900-EDIT-ROW-EXIT.
           EXIT.
       P21000-EDIT-ELEMENT.
           MOVE 0 TO WS-AD-CNT-03.
           INSPECT WS-AD-TXT-02 TALLYING WS-AD-CNT-03
               FOR ALL SPACES.
           INSPECT WS-AD-TXT-02 REPLACING ALL LOW-VALUES BY SPACES.
       P21000-EDIT-ELEMENT-EXIT.
           EXIT.
       P21100-BUILD-ELEMENT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-AD-TXT-02 TO PC-COL-001-020.
           MOVE WS-AD-TXT-06 TO PC-COL-021-060.
           MOVE WS-AD-AMT-05 TO WS-AD-AMT-EDIT.
           MOVE WS-AD-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P21100-BUILD-ELEMENT-EXIT.
           EXIT.
       P21200-CONVERT-KEY.
           MOVE IA-SOURCE TO WS-AD-TXT-01.
           MOVE IA-SEGMENT TO WS-AD-TXT-06.
           MOVE IA-LEVEL TO WS-AD-TXT-01.
           MOVE IA-ACCOUNT TO WS-AD-TXT-05.
           ADD 1 TO WS-AD-CNT-06.
       P21200-CONVERT-KEY-EXIT.
           EXIT.
       P260-COMPARE-DESCRIPTION.
           SET WS-AD-IX TO WS-AD-SUB-02.
           IF WS-AD-TB-KEY (WS-AD-IX) = IA-CENTRE
               MOVE 'Y' TO WS-AD-SW-02
               MOVE WS-AD-TB-VAL (WS-AD-IX) TO WS-AD-QTY-03
               MOVE WS-AD-SUB-02 TO WS-AD-SUB-01.
       P260-COMPARE-DESCRIPTION-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-RELEASE-BAND.
           MOVE SPACES TO CABS-AD-OUT-RECORD.
           MOVE IA-MEDIA TO OA-STATUS.
           MOVE IA-TARIFF TO OA-ACCOUNT.
           MOVE IA-LEVEL TO OA-REGION.
           MOVE IA-SOURCE TO OA-LEVEL.
           MOVE IA-TYPE TO OA-CARRIER.
           MOVE IA-CARRIER TO OA-STATUS2.
           MOVE IA-TARGET TO OA-CLASS.
           MOVE IA-CLASS TO OA-TARGET.
           MOVE IA-TARIFF TO OA-JURIS.
           WRITE CABS-AD-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P3100-RELEASE-BAND-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P3200-STAGE-ROW.
           CALL 'CABHASH' USING IA-CARRIER WS-ACC-OCN-HASH.
           ADD WS-AD-CNT-02 TO WS-ACC-SEQ-HASH.
       P3200-STAGE-ROW-EXIT.
           EXIT.
       P3300-STAGE-ROW.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-AD-TXT-05 TO PC-COL-001-020.
           MOVE WS-AD-TXT-02 TO PC-COL-021-060.
           MOVE WS-AD-AMT-01 TO WS-AD-AMT-EDIT.
           MOVE WS-AD-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P3300-STAGE-ROW-EXIT.
           EXIT.
       P3400-EMIT-ELEMENT.
           MOVE 0 TO WS-AD-QTY-03.
           MOVE 0 TO WS-AD-QTY-01.
           MOVE 0 TO WS-AD-AMT-01.
           MOVE 0 TO WS-AD-AMT-05.
       P3400-EMIT-ELEMENT-EXIT.
           EXIT.
      * S400-SECONDARY-PROCESSING SECTION
       S400-SECONDARY-PROCESSING SECTION.
       P4000-SECONDARY.
           PERFORM P4100-COMPARE-GROUP THRU P4100-COMPARE-GROUP-EXIT.
           PERFORM P4200-REPORT-ROW THRU P4200-REPORT-ROW-EXIT.
       P4000-EXIT.
           EXIT.
       P4100-COMPARE-GROUP.
           CALL 'CABHASH' USING IA-ACCOUNT WS-ACC-OCN-HASH.
           ADD WS-AD-CNT-05 TO WS-ACC-SEQ-HASH.
       P4100-COMPARE-GROUP-EXIT.
           EXIT.
       P4200-REPORT-ROW.
           ADD IA-ELEM TO WS-AD-QTY-02.
           COMPUTE WS-AD-AMT-02 ROUNDED = WS-AD-QTY-02 * WS-AD-QTY-01.
           ADD WS-AD-AMT-02 TO WS-AD-AMT-05.
       P4200-REPORT-ROW-EXIT.
           EXIT.
           MOVE 0 TO WS-AD-QTY-03.
           PERFORM P370-WALK-TARIFF THRU P370-WALK-TARIFF-EXIT
               VARYING WS-AD-SUB-01 FROM 1 BY 1
               UNTIL WS-AD-SUB-01 > WS-AD-TAB-CNT.
       P370-WALK-TARIFF.
           SET WS-AD-IX TO WS-AD-SUB-01.
           IF WS-AD-TB-KEY (WS-AD-IX) NOT = SPACES
               ADD WS-AD-TB-VAL (WS-AD-IX) TO WS-AD-QTY-02.
       P370-WALK-TARIFF-EXIT.
           EXIT.
      * S800-CONTROL SECTION - THE MANDATORY CABS CONTROL BOUNDARY.
       S800-CONTROL SECTION.
       P8000-CONTROL.
           PERFORM P4000-SECONDARY THRU P4000-EXIT.
           PERFORM P8010-PRINT-AUDIT-REPORT THRU P8010-EXIT.
           PERFORM P8100-BUILD-CONTROL-REC THRU P8100-EXIT.
           PERFORM P8200-CHECK-BALANCE THRU P8200-EXIT.
           PERFORM P8300-WRITE-CONTROL-REC THRU P8300-EXIT.
       P8000-EXIT.
           EXIT.
       P8010-PRINT-AUDIT-REPORT.
           ADD 1 TO WS-RPT-PAGE-NBR.
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
           MOVE 'RECORDS READ' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO WS-AD-CNT-EDIT.
           MOVE WS-AD-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS WRITTEN' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-AD-CNT-EDIT.
           MOVE WS-AD-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS REJECTED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-AD-CNT-EDIT.
           MOVE WS-AD-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS SUMMARISED' TO PC-COL-001-020.
           MOVE WS-SUMM-CNT TO WS-AD-CNT-EDIT.
           MOVE WS-AD-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS CARRIED FWD' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-AD-CNT-EDIT.
           MOVE WS-AD-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-AD-CNT-01 TO WS-AD-CNT-EDIT.
           MOVE WS-AD-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 02' TO PC-COL-001-020.
           MOVE WS-AD-CNT-02 TO WS-AD-CNT-EDIT.
           MOVE WS-AD-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 03' TO PC-COL-001-020.
           MOVE WS-AD-CNT-03 TO WS-AD-CNT-EDIT.
           MOVE WS-AD-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE SPACES TO CT-FILLER.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE 0 TO CT-RC.
           MOVE WS-AD-TXT-06 TO CT-RESTART-KEY.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE 3 TO CT-STEP-SEQ.
           MOVE WS-AD-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
       P8100-EXIT.
           EXIT.
      * P8200-CHECK-BALANCE - THE EQUATION IS TESTED AS IT STANDS AND
      * THE RETURN CODE IS SET FROM THE RESULT SO THE SCHEDULER CAN
      * SEE IT.
       P8200-CHECK-BALANCE.
           IF CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARISED +
                       CT-CARRIED-FWD
               MOVE 'B' TO CT-BAL-IND
           ELSE
               MOVE 'O' TO CT-BAL-IND.
           IF CT-OUT-OF-BAL
               MOVE 0004 TO CT-RC.
       P8200-EXIT.
           EXIT.
       P8300-WRITE-CONTROL-REC.
           MOVE CABS-CONTROL-RECORD TO CABS-CTLOUT-RECORD.
           WRITE CABS-CTLOUT-RECORD.
       P8300-EXIT.
           EXIT.
      * S900-TERMINATION SECTION.
       S900-TERMINATION SECTION.
       P9000-TERM.
           CLOSE TARIN.
           CLOSE TBLOUT.
           CLOSE RATOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABURT01 - RUN COMPLETE'.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  AD-CNT-06 = ' WS-AD-CNT-06.
           DISPLAY '  AD-CNT-01 = ' WS-AD-CNT-01.
       P9000-EXIT.
           EXIT.
