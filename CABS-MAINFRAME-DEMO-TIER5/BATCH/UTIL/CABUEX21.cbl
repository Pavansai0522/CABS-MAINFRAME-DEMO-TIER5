      *****************************************************************
      * CABUEX21 - SUSPENSE EXTRACT FOR THE RECYCLE JOB               *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               CIRIN   TELCABS.CABS.CIRIN          (LOCAL)     *
      *               CARIN   TELCABS.CABS.CARIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               SELOUT  TELCABS.CABS.SELOUT         (LOCAL)     *
      *               CAROUT  TELCABS.CABS.CAROUT         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * REVISION HISTORY                                              *
      *   V1.00  1990-10-14  T.YAMASHITA  INITIAL RELEASE             *
      *   V1.01  1998-02-08  G.PRZYBYLSKI PRINT LINE WIDENED TO 133   *
      *   V1.03  2005-12-22  G.PRZYBYLSKI ROUNDING RULE TAKEN FROM THE*
      *                      RATE ROW                                 *
      *   V1.05  2012-07-21  K.O.BRIEN    OCCURS RAISED AFTER THE     *
      *                      FEBRUARY OVERFLOW                        *
      *   V1.06  2014-01-10  J.M.CASTILLO RETIRED THE SECOND SORT STEP*
      *                      - DONE IN PROGRAM                        *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUEX21.
       AUTHOR. TELCABS APPLICATIONS - DATASET EXTRACT.
      *****************************************************************
      * SUSPENSE EXTRACT FOR THE RECYCLE JOB. THE STEP IS SCHEDULED   *
      * MONTHLY AND ALSO RUN ON DEMAND WHEN A CENTRE ASKS FOR IT.     *
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT     *
      * PRECEDES THIS PROGRAM IN THE JOB.                             *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           C01 IS TO-NEW-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CIRIN ASSIGN TO UT-S-CIRIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT CARIN ASSIGN TO UT-S-CARIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT SELOUT ASSIGN TO UT-S-SELOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT CAROUT ASSIGN TO UT-S-CAROUT
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
      * CIRIN - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  CIRIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 160 CHARACTERS.
       01  CABS-AY-IN-RECORD.
           05  IA-ACCOUNT                  PIC 9(09).
           05  IA-REGION                   PIC X(20).
           05  IA-TARGET                   PIC S9(13) COMP-3.
           05  IA-OCN                      PIC X(13).
           05  IA-SEQ                      PIC 9(05).
           05  IA-LEVEL                    PIC 9(04).
           05  IA-JURIS                    PIC X(20).
           05  IA-CARRIER                  PIC X(08).
           05  IA-BAND                     PIC X(04).
           05  IA-PERIOD                   PIC S9(07) COMP-3.
           05  IA-CLASS                    PIC X(13).
           05  IA-ELEM                     PIC X(03).
           05  IA-ACCOUNT2                 PIC 9(09).
           05  IA-BAN                      PIC X(02).
           05  IA-GROUP                    PIC S9(11)V9(05) COMP-3.
           05  IA-SEGMENT                  PIC X(06).
           05  IA-LEVEL2                   PIC X(02).
           05  IA-CODE                     PIC 9(05).
           05  IA-CLASS2                   PIC X(02).
           05  IA-OCN2                     PIC X(06).
           05  AY-FILL-01                  PIC X(9).
      * CARIN - PERMANENT DATASET HELD ON DASD.
       FD  CARIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 160 CHARACTERS.
       01  CABS-AY-ALT1-RECORD.
           05  A1-ACCOUNT                  PIC X(16).
           05  A1-ELEM                     PIC X(06).
           05  A1-TYPE                     PIC 9(04).
           05  A1-CODE                     PIC X(13).
           05  A1-CARRIER                  PIC X(04).
           05  A1-ACCOUNT2                 PIC 9(05).
           05  A1-BAN                      PIC X(03).
           05  A1-CYCLE                    PIC S9(13) COMP-3.
           05  AY-FILL-02                  PIC X(102).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-AY-VIEW1 REDEFINES CABS-AY-IN-RECORD.
           05  R0A-JURIS                   PIC X(03).
           05  R0A-MEDIA                   PIC 9(09).
           05  R0A-CYCLE                   PIC X(06).
           05  R0A-TARGET                  PIC X(08).
           05  R0A-CIRCUIT                 PIC S9(09) COMP-3.
           05  R0A-TYPE                    PIC S9(13)V9(02) COMP-3.
           05  R0A-REST                    PIC X(121).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-AY-VIEW2 REDEFINES CABS-AY-IN-RECORD.
           05  R1A-JURIS                   PIC 9(09).
           05  R1A-TARGET                  PIC S9(09)V9(02) COMP-3.
           05  R1A-MEDIA                   PIC 9(02).
           05  R1A-OCN                     PIC S9(09)V9(05) COMP-3.
           05  R1A-CYCLE                   PIC X(06).
           05  R1A-CYCLE2                  PIC S9(07) COMP-3.
           05  R1A-TARGET2                 PIC X(10).
           05  R1A-BAN                     PIC X(10).
           05  R1A-CIRCUIT                 PIC X(13).
           05  R1A-REST                    PIC X(92).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-AY-VIEW3 REDEFINES CABS-AY-IN-RECORD.
           05  R2A-CODE                    PIC S9(11) COMP-3.
           05  R2A-TYPE                    PIC X(02).
           05  R2A-OCN                     PIC X(20).
           05  R2A-CIRCUIT                 PIC S9(11)V9(05) COMP-3.
           05  R2A-CARRIER                 PIC S9(05) COMP-3.
           05  R2A-TARGET                  PIC 9(09).
           05  R2A-CIRCUIT2                PIC S9(11)V9(02) COMP-3.
           05  R2A-REGION                  PIC S9(13)V9(05) COMP-3.
           05  R2A-TARIFF                  PIC X(10).
           05  R2A-REST                    PIC X(84).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-AY-VIEW4 REDEFINES CABS-AY-IN-RECORD.
           05  R3A-JURIS                   PIC X(08).
           05  R3A-CODE                    PIC S9(15) COMP-3.
           05  R3A-MEDIA                   PIC X(03).
           05  R3A-INVOICE                 PIC S9(07)V9(02) COMP-3.
           05  R3A-JURIS2                  PIC X(13).
           05  R3A-REST                    PIC X(123).
      * SELOUT - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  SELOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 150 CHARACTERS.
       01  CABS-AY-OUT-RECORD.
           05  OA-SEQ                      PIC X(08).
           05  OA-TARIFF                   PIC S9(15) COMP-3.
           05  OA-STATUS                   PIC X(16).
           05  OA-LEVEL                    PIC S9(09)V9(02) COMP-3.
           05  OA-OCN                      PIC S9(13) COMP-3.
           05  OA-OCN2                     PIC X(04).
           05  OA-SOURCE                   PIC X(20).
           05  OA-LEVEL2                   PIC X(06).
           05  OA-SOURCE2                  PIC 9(07).
           05  OA-SEQ2                     PIC S9(13) COMP-3.
           05  OA-BAND                     PIC X(20).
           05  OA-BAN                      PIC S9(09)V9(02) COMP-3.
           05  OA-ACCOUNT                  PIC 9(06).
           05  OA-INVOICE                  PIC S9(05) COMP-3.
           05  OA-OCN3                     PIC X(20).
           05  AY-FILL-03                  PIC X(6).
      * CAROUT - CATALOGUED GENERATION DATA GROUP.
       FD  CAROUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 150 CHARACTERS.
       01  CABS-AY-OUT1-RECORD         PIC X(150).
      * CTLOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD              PIC X(180).
      * RPTOUT - PERMANENT DATASET HELD ON DASD.
       FD  RPTOUT
           RECORDING MODE IS FA
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       COPY CABSPRNT.
       WORKING-STORAGE SECTION.
      * STANDARD SHARED WORKING STORAGE. SEE CABSWRK.
       COPY CABSWRK.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUEX21'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V1.30'.
           05  WS-TABLE-LIMIT              PIC S9(04) COMP-3
                                         VALUE 400.
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
       01  WS-PARM-CARD-R2 REDEFINES WS-PARM-CARD.
           05  PC2-LEAD                    PIC X(14).
           05  PC2-CYCLE-VIEW.
               10  PC2-CV-YY                   PIC 9(02).
               10  PC2-CV-DDD                  PIC 9(03).
           05  PC2-REST                    PIC X(61).
       01  WS-COUNT-AREA.
           05  WS-AY-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AY-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AY-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AY-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AY-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AY-CNT-06                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AY-CNT-07                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AY-CNT-08                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AY-CNT-09                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-AY-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AY-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AY-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AY-QTY-04                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AY-QTY-05                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-AY-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AY-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AY-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-AY-TXT-01                PIC X(10) VALUE SPACES.
           05  WS-AY-TXT-02                PIC X(26) VALUE SPACES.
           05  WS-AY-TXT-03                PIC X(30) VALUE SPACES.
           05  WS-AY-TXT-04                PIC X(30) VALUE SPACES.
           05  WS-AY-TXT-05                PIC X(08) VALUE SPACES.
           05  WS-AY-TXT-06                PIC X(20) VALUE SPACES.
           05  WS-AY-TXT-07                PIC X(30) VALUE SPACES.
           05  WS-AY-TXT-08                PIC X(26) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-AY-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-AY-ON-01                 VALUE 'Y'.
               88  WS-AY-OFF-01                VALUE 'N'.
           05  WS-AY-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-AY-ON-02                 VALUE 'Y'.
               88  WS-AY-OFF-02                VALUE 'N'.
           05  WS-AY-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-AY-ON-03                 VALUE 'Y'.
               88  WS-AY-OFF-03                VALUE 'N'.
           05  WS-AY-SW-04                 PIC X(01) VALUE 'N'.
               88  WS-AY-ON-04                 VALUE 'Y'.
               88  WS-AY-OFF-04                VALUE 'N'.
           05  WS-AY-SW-05                 PIC X(01) VALUE 'N'.
               88  WS-AY-ON-05                 VALUE 'Y'.
               88  WS-AY-OFF-05                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-AY-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-AY-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-AY-SUB-03                PIC S9(04) COMP-3 VALUE 0.
           05  WS-AY-SUB-04                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. LOADED ONCE AT INITIALISATION AND SEARCHED WITH
      * A SUBSCRIPTED WALK.
       01  WS-AY-TABLE.
           05  WS-AY-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-AY-TB-ENTRY OCCURS 400 TIMES
                                       INDEXED BY WS-AY-IX.
               10  WS-AY-TB-KEY                PIC X(13).
               10  WS-AY-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-AY-TB-TXT                PIC X(30).
               10  WS-AY-TB-EFF                PIC 9(05).
               10  WS-AY-TB-EXP                PIC 9(05).
       01  WS-AY-WORK-GROUP-1.
           05  WS-AY-G1-LEVEL              PIC X(10).
           05  WS-AY-G1-ACCOUNT            PIC 9(07).
           05  WS-AY-G1-REGION             PIC S9(09) COMP-3.
           05  WS-AY-G1-SEGMENT            PIC X(10).
           05  WS-AY-G1-REGION             PIC S9(11)V9(02) COMP-3.
       01  WS-AY-WORK-GROUP-2.
           05  WS-AY-G2-PERIOD             PIC S9(11)V9(02) COMP-3.
           05  WS-AY-G2-ELEM               PIC 9(05).
           05  WS-AY-G2-BAND               PIC 9(07).
           05  WS-AY-G2-STATE              PIC S9(09) COMP-3.
           05  WS-AY-G2-STATE              PIC S9(11)V9(02) COMP-3.
           05  WS-AY-G2-OCN                PIC 9(05).
           05  WS-AY-G2-BAN                PIC 9(05).
           05  WS-AY-G2-MEDIA              PIC X(10).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 60.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUEX21 - SUSPENSE EXTRACT FOR THE RECYCLE JOB'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-AY-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-AY-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9913.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-AY-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-AY-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           PERFORM P1400-PRIME-READ THRU P1400-EXIT.
       P1000-EXIT.
           EXIT.
       P1100-OPEN-FILES.
           OPEN INPUT CIRIN.
           IF WS-FS-INPUT NOT = '00'
               DISPLAY 'CIRIN FILE STATUS = ' WS-FS-INPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF CIRIN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN INPUT CARIN.
           IF WS-FS-INPUT NOT = '00'
               DISPLAY 'CARIN FILE STATUS = ' WS-FS-INPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF CARIN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SELOUT.
           IF WS-FS-OUTPUT NOT = '00'
               DISPLAY 'SELOUT FILE STATUS = ' WS-FS-OUTPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF SELOUT' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CAROUT.
           IF WS-FS-OUTPUT NOT = '00'
               DISPLAY 'CAROUT FILE STATUS = ' WS-FS-OUTPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF CAROUT' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               DISPLAY 'CTLOUT FILE STATUS = ' WS-FS-CONTROL
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF CTLOUT' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RPTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               DISPLAY 'RPTOUT FILE STATUS = ' WS-FS-OUTPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF RPTOUT' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-AY-CYCLE-YYDDD.
           COMPUTE WS-AY-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-AY-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-AY-CNT-09.
           MOVE 0 TO WS-AY-CNT-03.
           MOVE 0 TO WS-AY-CNT-07.
       P1200-EXIT.
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
           PERFORM P2200-BUILD-SUBSET THRU P2200-BUILD-SUBSET-EXIT.
           PERFORM P2300-BUILD-EXTRACT THRU P2300-BUILD-EXTRACT-EXIT.
           PERFORM P2400-VALIDATE-FILTER THRU
               P2400-VALIDATE-FILTER-EXIT.
           PERFORM P2500-SELECT-RANGE THRU P2500-SELECT-RANGE-EXIT.
           PERFORM P2600-CONVERT-SELECTION THRU
               P2600-CONVERT-SELECTION-EXIT.
           PERFORM P2700-DERIVE-RANGE THRU P2700-DERIVE-RANGE-EXIT.
           PERFORM P2800-VALIDATE-EXTRACT THRU
               P2800-VALIDATE-EXTRACT-EXIT.
           PERFORM P2900-RESOLVE-FILTER THRU P2900-RESOLVE-FILTER-EXIT.
           PERFORM P21000-APPLY-SUBSET THRU P21000-APPLY-SUBSET-EXIT.
           PERFORM P21100-SELECT-MASTER THRU P21100-SELECT-MASTER-EXIT.
           PERFORM P21200-APPLY-EXTRACT THRU P21200-APPLY-EXTRACT-EXIT.
           PERFORM P21300-EXPAND-EXTRACT THRU
               P21300-EXPAND-EXTRACT-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ CIRIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT
      * PRECEDES THIS PROGRAM IN THE JOB.
       P2200-BUILD-SUBSET.
           MOVE 'N' TO WS-AY-SW-02.
           IF WS-AY-TXT-06 NOT = WS-AY-TXT-02
               MOVE 'Y' TO WS-AY-SW-02
               MOVE WS-AY-TXT-06 TO WS-AY-TXT-02
               ADD 1 TO WS-AY-CNT-03.
       P2200-BUILD-SUBSET-EXIT.
           EXIT.
       P2300-BUILD-EXTRACT.
           CALL 'CABPARMR' USING WS-AY-TXT-04 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-AY-CNT-02.
       P2300-BUILD-EXTRACT-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P2400-VALIDATE-FILTER.
           IF IA-LEVEL2 = 'A'
               ADD 1 TO WS-AY-CNT-06
           ELSE
               IF IA-LEVEL2 = 'S'
                   ADD 1 TO WS-AY-CNT-09
               ELSE
                   IF IA-LEVEL2 = 'X'
                       ADD 1 TO WS-AY-CNT-02
                   ELSE
                       ADD 1 TO WS-AY-CNT-09.
       P2400-VALIDATE-FILTER-EXIT.
           EXIT.
       P2500-SELECT-RANGE.
           IF WS-AY-AMT-01 < 5
               MOVE 5 TO WS-AY-AMT-01
               ADD 1 TO WS-AY-CNT-08.
           IF WS-AY-AMT-01 > 82720
               MOVE 82720 TO WS-AY-AMT-01
               ADD 1 TO WS-AY-CNT-02.
       P2500-SELECT-RANGE-EXIT.
           EXIT.
       P2600-CONVERT-SELECTION.
           MOVE 0 TO WS-AY-QTY-04.
           MOVE 0 TO WS-AY-QTY-03.
           MOVE 0 TO WS-AY-AMT-02.
       P2600-CONVERT-SELECTION-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P2700-DERIVE-RANGE.
           MOVE 'Y' TO WS-AY-SW-05.
           IF IA-GROUP < 20
               MOVE 'N' TO WS-AY-SW-05
               ADD 1 TO WS-AY-CNT-04.
           IF IA-GROUP > 7772
               MOVE 'N' TO WS-AY-SW-05
               ADD 1 TO WS-AY-CNT-08.
       P2700-DERIVE-RANGE-EXIT.
           EXIT.
       P2800-VALIDATE-EXTRACT.
           MOVE 0 TO WS-AY-CNT-03.
           INSPECT WS-AY-TXT-07 TALLYING WS-AY-CNT-03
               FOR ALL SPACES.
           INSPECT WS-AY-TXT-07 REPLACING ALL LOW-VALUES BY SPACES.
       P2800-VALIDATE-EXTRACT-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P2900-RESOLVE-FILTER.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-DATE-INVALID TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-AY-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
       P2900-RESOLVE-FILTER-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P21000-APPLY-SUBSET.
           MOVE WS-AY-AMT-01 TO WS-AY-AMT-03.
           IF WS-AY-AMT-03 < 0
               COMPUTE WS-AY-AMT-03 = 0 - WS-AY-AMT-01.
       P21000-APPLY-SUBSET-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P21100-SELECT-MASTER.
           IF WS-AY-AMT-01 NOT = 0
               COMPUTE WS-AY-QTY-04 = WS-AY-AMT-01 * 100 / WS-AY-AMT-01
           ELSE
               MOVE 0 TO WS-AY-QTY-04.
       P21100-SELECT-MASTER-EXIT.
           EXIT.
       P21200-APPLY-EXTRACT.
           ADD IA-PERIOD TO WS-AY-QTY-01.
           COMPUTE WS-AY-AMT-03 = WS-AY-QTY-01 * WS-AY-QTY-04.
           ADD WS-AY-AMT-03 TO WS-AY-AMT-01.
       P21200-APPLY-EXTRACT-EXIT.
           EXIT.
       P21300-EXPAND-EXTRACT.
           MOVE SPACES TO WS-AY-TXT-03.
           STRING IA-REGION DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IA-LEVEL DELIMITED BY SIZE
               INTO WS-AY-TXT-03.
       P21300-EXPAND-EXTRACT-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P3100-POST-RANGE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-AY-TXT-08 TO PC-COL-001-020.
           MOVE WS-AY-TXT-08 TO PC-COL-021-060.
           MOVE WS-AY-AMT-02 TO WS-AY-AMT-EDIT.
           MOVE WS-AY-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P3100-POST-RANGE-EXIT.
           EXIT.
       P3200-CLOSE-OFF-FILTER.
           MOVE IA-TARGET TO WS-AY-TXT-06.
           MOVE IA-GROUP TO WS-AY-TXT-04.
           MOVE IA-BAND TO WS-AY-TXT-08.
           MOVE IA-OCN TO WS-AY-TXT-03.
           ADD 1 TO WS-AY-CNT-08.
       P3200-CLOSE-OFF-FILTER-EXIT.
           EXIT.
       P3300-CLOSE-OFF-SUBSET.
           ADD IA-GROUP TO WS-AY-QTY-04.
           COMPUTE WS-AY-AMT-02 = WS-AY-QTY-04 * WS-AY-QTY-02.
           ADD WS-AY-AMT-02 TO WS-AY-AMT-01.
       P3300-CLOSE-OFF-SUBSET-EXIT.
           EXIT.
       P3400-POST-RANGE.
           MOVE SPACES TO CABS-AY-OUT-RECORD.
           MOVE IA-LEVEL TO OA-SEQ.
           MOVE IA-CLASS2 TO OA-TARIFF.
           MOVE IA-SEGMENT TO OA-STATUS.
           MOVE IA-LEVEL TO OA-LEVEL.
           MOVE IA-LEVEL2 TO OA-OCN.
           MOVE IA-CLASS TO OA-OCN2.
           MOVE IA-BAND TO OA-SOURCE.
           MOVE IA-LEVEL TO OA-LEVEL2.
           MOVE IA-CLASS TO OA-SOURCE2.
           WRITE CABS-AY-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P3400-POST-RANGE-EXIT.
           EXIT.
      * S400-SECONDARY-PROCESSING SECTION
       S400-SECONDARY-PROCESSING SECTION.
       P4000-SECONDARY.
           PERFORM P4100-NORMALISE-CANDIDATE THRU
               P4100-NORMALISE-CANDIDATE-EXIT.
           PERFORM P4200-SUMMARISE-BAND THRU P4200-SUMMARISE-BAND-EXIT.
       P4000-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P4100-NORMALISE-CANDIDATE.
           CALL 'CABHASH' USING WS-AY-TXT-05 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-AY-CNT-05.
       P4100-NORMALISE-CANDIDATE-EXIT.
           EXIT.
       P4200-SUMMARISE-BAND.
           IF WS-AY-AMT-01 NOT = 0
               COMPUTE WS-AY-QTY-05 = WS-AY-AMT-01 * 100 / WS-AY-AMT-01
           ELSE
               MOVE 0 TO WS-AY-QTY-05.
       P4200-SUMMARISE-BAND-EXIT.
           EXIT.
           MOVE 0 TO WS-AY-QTY-03.
           PERFORM P350-WALK-RANGE THRU P350-WALK-RANGE-EXIT
               VARYING WS-AY-SUB-03 FROM 1 BY 1
               UNTIL WS-AY-SUB-03 > WS-AY-TAB-CNT.
       P350-WALK-RANGE.
           SET WS-AY-IX TO WS-AY-SUB-02.
           IF WS-AY-TB-KEY (WS-AY-IX) NOT = SPACES
               ADD WS-AY-TB-VAL (WS-AY-IX) TO WS-AY-QTY-04.
       P350-WALK-RANGE-EXIT.
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
           MOVE 'RECORDS REJECTED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-AY-CNT-EDIT.
           MOVE WS-AY-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS CARRIED FWD' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-AY-CNT-EDIT.
           MOVE WS-AY-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS SUMMARISED' TO PC-COL-001-020.
           MOVE WS-SUMM-CNT TO WS-AY-CNT-EDIT.
           MOVE WS-AY-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS READ' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO WS-AY-CNT-EDIT.
           MOVE WS-AY-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS WRITTEN' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-AY-CNT-EDIT.
           MOVE WS-AY-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-AY-CNT-01 TO WS-AY-CNT-EDIT.
           MOVE WS-AY-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-FILLER.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE WS-AY-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE 4 TO CT-STEP-SEQ.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
       P8100-EXIT.
           EXIT.
      * P8200-CHECK-BALANCE - THE REPORT LINES ARE NOT RECORDS, SO THE
      * WRITTEN COUNT IS ZEROED BEFORE THE EQUATION IS TESTED.
       P8200-CHECK-BALANCE.
           MOVE 0 TO CT-WRITTEN.
           IF CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARISED +
                       CT-CARRIED-FWD
               MOVE 'B' TO CT-BAL-IND
           ELSE
               MOVE 'O' TO CT-BAL-IND.
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
           CLOSE CIRIN.
           CLOSE CARIN.
           CLOSE SELOUT.
           CLOSE CAROUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABUEX21 - STEP COMPLETE'.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  AY-CNT-06 = ' WS-AY-CNT-06.
           DISPLAY '  AY-CNT-08 = ' WS-AY-CNT-08.
           DISPLAY '  AY-CNT-02 = ' WS-AY-CNT-02.
           DISPLAY '  AY-CNT-01 = ' WS-AY-CNT-01.
       P9000-EXIT.
           EXIT.
