      *****************************************************************
      * CABURT10 - RATE OVERRIDE TABLE LOAD                           *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               TBLIN   TELCABS.CABS.TBLIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               TAROUT  TELCABS.CABS.TAROUT         (LOCAL)     *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - NOTHING IS UPDATED IN PLACE        *
      * REVISION HISTORY                                              *
      *   V1.00  1989-03-22  K.O.BRIEN    INITIAL RELEASE             *
      *   V1.02  2003-09-16  W.J.MCALLISTER HASH TOTAL ADDED TO THE   *
      *                      CONTROL RECORD                           *
      *   V1.05  2010-07-07  L.FERREIRA   SUSPENSE WRITE ADDED -      *
      *                      RECORDS WERE BEING DROPPED               *
      *   V1.08  2013-01-27  M.DELACROIX  CONTROL RECORD ADDED PER    *
      *                      CABS-STD-002                             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABURT10.
       AUTHOR. TELCABS APPLICATIONS - RATE TABLE MAINTENANCE.
      *****************************************************************
      * RATE OVERRIDE TABLE LOAD. THE STEP IS SCHEDULED MONTHLY AND   *
      * ALSO RUN ON DEMAND WHEN A CENTRE ASKS FOR IT.                 *
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
           SELECT TBLIN ASSIGN TO UT-S-TBLIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT TAROUT ASSIGN TO UT-S-TAROUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT SUSOUT ASSIGN TO UT-S-SUSOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-SUSPENSE.
           SELECT CTLOUT ASSIGN TO UT-S-CTLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      * TBLIN - PERMANENT DATASET HELD ON DASD.
       FD  TBLIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 100 CHARACTERS.
       01  CABS-BL-IN-RECORD.
           05  IB-INVOICE                  PIC X(02).
           05  IB-REGION                   PIC S9(13)V9(02) COMP-3.
           05  IB-CENTRE                   PIC X(06).
           05  IB-STATUS                   PIC X(04).
           05  IB-BAND                     PIC X(02).
           05  IB-ACCOUNT                  PIC X(13).
           05  IB-GROUP                    PIC S9(15) COMP-3.
           05  IB-SEQ                      PIC X(06).
           05  IB-CYCLE                    PIC 9(03).
           05  IB-TARGET                   PIC 9(09).
           05  IB-INVOICE2                 PIC X(13).
           05  IB-CYCLE2                   PIC S9(07)V9(05) COMP-3.
           05  IB-CODE                     PIC S9(09)V9(05) COMP-3.
           05  IB-OCN                      PIC X(06).
           05  IB-BAN                      PIC 9(03).
           05  BL-FILL-01                  PIC X(2).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-BL-VIEW1 REDEFINES CABS-BL-IN-RECORD.
           05  R0B-REGION                  PIC X(03).
           05  R0B-JURIS                   PIC S9(11)V9(02) COMP-3.
           05  R0B-CLASS                   PIC X(10).
           05  R0B-CIRCUIT                 PIC X(04).
           05  R0B-GROUP                   PIC S9(15) COMP-3.
           05  R0B-TYPE                    PIC S9(13) COMP-3.
           05  R0B-ELEM                    PIC X(02).
           05  R0B-ACCOUNT                 PIC 9(09).
           05  R0B-SEQ                     PIC 9(09).
           05  R0B-REST                    PIC X(41).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-BL-VIEW2 REDEFINES CABS-BL-IN-RECORD.
           05  R1B-JURIS                   PIC S9(11) COMP-3.
           05  R1B-STATUS                  PIC 9(03).
           05  R1B-INVOICE                 PIC S9(09)V9(02) COMP-3.
           05  R1B-BAND                    PIC X(10).
           05  R1B-STATE                   PIC X(04).
           05  R1B-OCN                     PIC 9(05).
           05  R1B-CIRCUIT                 PIC 9(05).
           05  R1B-JURIS2                  PIC S9(07) COMP-3.
           05  R1B-SEGMENT                 PIC 9(04).
           05  R1B-REST                    PIC X(53).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-BL-VIEW3 REDEFINES CABS-BL-IN-RECORD.
           05  R2B-CODE                    PIC S9(07)V9(05) COMP-3.
           05  R2B-STATUS                  PIC 9(04).
           05  R2B-CYCLE                   PIC X(06).
           05  R2B-JURIS                   PIC S9(11)V9(02) COMP-3.
           05  R2B-REST                    PIC X(76).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-BL-VIEW4 REDEFINES CABS-BL-IN-RECORD.
           05  R3B-SOURCE                  PIC S9(07)V9(02) COMP-3.
           05  R3B-LEVEL                   PIC X(02).
           05  R3B-STATE                   PIC X(20).
           05  R3B-SEQ                     PIC X(08).
           05  R3B-SEGMENT                 PIC 9(03).
           05  R3B-REST                    PIC X(62).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-BL-VIEW5 REDEFINES CABS-BL-IN-RECORD.
           05  R4B-CODE                    PIC S9(09) COMP-3.
           05  R4B-CENTRE                  PIC S9(09)V9(02) COMP-3.
           05  R4B-TYPE                    PIC X(16).
           05  R4B-JURIS                   PIC X(13).
           05  R4B-OCN                     PIC X(16).
           05  R4B-TARIFF                  PIC S9(11) COMP-3.
           05  R4B-JURIS2                  PIC S9(13)V9(02) COMP-3.
           05  R4B-JURIS3                  PIC X(03).
           05  R4B-REST                    PIC X(27).
      * TAROUT - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  TAROUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 90 CHARACTERS.
       01  CABS-BL-OUT-RECORD.
           05  OB-OCN                      PIC 9(09).
           05  OB-CYCLE                    PIC 9(07).
           05  OB-CENTRE                   PIC S9(13) COMP-3.
           05  OB-ACCOUNT                  PIC X(04).
           05  OB-TARGET                   PIC 9(03).
           05  OB-TARIFF                   PIC S9(09) COMP-3.
           05  OB-SEQ                      PIC S9(15) COMP-3.
           05  OB-OCN2                     PIC S9(09) COMP-3.
           05  OB-ACCOUNT2                 PIC X(10).
           05  OB-ACCOUNT3                 PIC S9(13)V9(05) COMP-3.
           05  OB-LEVEL                    PIC S9(07) COMP-3.
           05  OB-ELEM                     PIC S9(05) COMP-3.
           05  OB-CENTRE2                  PIC S9(13) COMP-3.
           05  BL-FILL-02                  PIC X(8).
      * SUSOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  SUSOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 300 CHARACTERS.
       01  CABS-SUSOUT-RECORD              PIC X(300).
      * CTLOUT - CATALOGUED GENERATION DATA GROUP.
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD              PIC X(180).
       WORKING-STORAGE SECTION.
      * STANDARD SHARED WORKING STORAGE. SEE CABSWRK.
       COPY CABSWRK.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABURT10'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V2.08'.
           05  WS-TABLE-LIMIT              PIC S9(04) COMP-3
                                         VALUE 250.
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
           05  WS-BL-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BL-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BL-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BL-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BL-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BL-CNT-06                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BL-CNT-07                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-BL-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BL-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BL-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-BL-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BL-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BL-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BL-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BL-AMT-05                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-BL-TXT-01                PIC X(16) VALUE SPACES.
           05  WS-BL-TXT-02                PIC X(12) VALUE SPACES.
           05  WS-BL-TXT-03                PIC X(08) VALUE SPACES.
           05  WS-BL-TXT-04                PIC X(08) VALUE SPACES.
           05  WS-BL-TXT-05                PIC X(26) VALUE SPACES.
           05  WS-BL-TXT-06                PIC X(10) VALUE SPACES.
           05  WS-BL-TXT-07                PIC X(12) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-BL-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-BL-ON-01                 VALUE 'Y'.
               88  WS-BL-OFF-01                VALUE 'N'.
           05  WS-BL-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-BL-ON-02                 VALUE 'Y'.
               88  WS-BL-OFF-02                VALUE 'N'.
           05  WS-BL-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-BL-ON-03                 VALUE 'Y'.
               88  WS-BL-OFF-03                VALUE 'N'.
           05  WS-BL-SW-04                 PIC X(01) VALUE 'N'.
               88  WS-BL-ON-04                 VALUE 'Y'.
               88  WS-BL-OFF-04                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-BL-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-BL-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-BL-SUB-03                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. A ROW THAT DOES NOT FIT IS COUNTED AND DROPPED
      * - THE STEP DOES NOT FAIL.
       01  WS-BL-TABLE.
           05  WS-BL-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-BL-TB-ENTRY OCCURS 250 TIMES
                                       INDEXED BY WS-BL-IX.
               10  WS-BL-TB-KEY                PIC X(04).
               10  WS-BL-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-BL-TB-TXT                PIC X(20).
               10  WS-BL-TB-EFF                PIC 9(05).
               10  WS-BL-TB-EXP                PIC 9(05).
       01  WS-BL-WORK-GROUP-1.
           05  WS-BL-G1-REGION             PIC S9(09) COMP-3.
           05  WS-BL-G1-PERIOD             PIC S9(09) COMP-3.
           05  WS-BL-G1-SEQ                PIC X(20).
           05  WS-BL-G1-TYPE               PIC 9(07).
       01  WS-BL-WORK-GROUP-2.
           05  WS-BL-G2-BAND               PIC X(10).
           05  WS-BL-G2-STATE              PIC S9(09) COMP-3.
           05  WS-BL-G2-MEDIA              PIC 9(05).
           05  WS-BL-G2-ELEM               PIC X(10).
           05  WS-BL-G2-GROUP              PIC X(20).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 58.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABURT10 - RATE OVERRIDE TABLE LOAD'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-BL-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-BL-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9961.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-BL-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-BL-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT TBLIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON TBLIN - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT TAROUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON TAROUT - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SUSOUT.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON SUSOUT - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON CTLOUT - CHECK THE ALLOCATION' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-BL-CYCLE-YYDDD.
           COMPUTE WS-BL-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-BL-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-BL-CNT-07.
           MOVE 0 TO WS-BL-CNT-04.
           MOVE 0 TO WS-BL-CNT-02.
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
           PERFORM P2200-SPLIT-ROW THRU P2200-SPLIT-ROW-EXIT.
           PERFORM P2300-RESOLVE-ELEMENT THRU
               P2300-RESOLVE-ELEMENT-EXIT.
           PERFORM P2400-APPLY-BAND THRU P2400-APPLY-BAND-EXIT.
           IF WS-BL-ON-04
               PERFORM P2500-RESOLVE-BAND THRU P2500-RESOLVE-BAND-EXIT.
           IF WS-BL-ON-01
               PERFORM P2600-VALIDATE-TARIFF THRU
                   P2600-VALIDATE-TARIFF-EXIT.
           IF WS-BL-ON-01
               PERFORM P2700-EDIT-KEY THRU P2700-EDIT-KEY-EXIT.
           PERFORM P2800-EDIT-ROW THRU P2800-EDIT-ROW-EXIT.
           IF WS-BL-ON-03
               PERFORM P2900-DERIVE-TARIFF THRU
                   P2900-DERIVE-TARIFF-EXIT.
           PERFORM P21000-SELECT-DESCRIPTION THRU
               P21000-SELECT-DESCRIPTION-EXIT.
           PERFORM P21100-SPLIT-DESCRIPTION THRU
               P21100-SPLIT-DESCRIPTION-EXIT.
           PERFORM P21200-APPLY-ELEMENT THRU P21200-APPLY-ELEMENT-EXIT.
           PERFORM P21300-EXPAND-BAND THRU P21300-EXPAND-BAND-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ TBLIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P2200-SPLIT-ROW.
           IF WS-BL-AMT-05 NOT = 0
               COMPUTE WS-BL-QTY-02 = WS-BL-AMT-01 * 100 / WS-BL-AMT-05
           ELSE
               MOVE 0 TO WS-BL-QTY-02.
       P2200-SPLIT-ROW-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P2300-RESOLVE-ELEMENT.
           MOVE WS-BL-AMT-04 TO WS-BL-AMT-04.
           IF WS-BL-AMT-04 < 0
               COMPUTE WS-BL-AMT-04 = 0 - WS-BL-AMT-04.
       P2300-RESOLVE-ELEMENT-EXIT.
           EXIT.
       P2400-APPLY-BAND.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-RATE-NOT-FOUND TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-BL-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
       P2400-APPLY-BAND-EXIT.
           EXIT.
       P2500-RESOLVE-BAND.
           ADD IB-CYCLE2 TO WS-BL-QTY-03.
           COMPUTE WS-BL-AMT-03 ROUNDED = WS-BL-QTY-03 * WS-BL-QTY-02.
           ADD WS-BL-AMT-03 TO WS-BL-AMT-01.
       P2500-RESOLVE-BAND-EXIT.
           EXIT.
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT
      * PRECEDES THIS PROGRAM IN THE JOB.
       P2600-VALIDATE-TARIFF.
           MOVE SPACES TO WS-BL-TXT-02.
           STRING IB-CENTRE DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IB-CYCLE DELIMITED BY SIZE
               INTO WS-BL-TXT-02.
       P2600-VALIDATE-TARIFF-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P2700-EDIT-KEY.
           CALL 'CABHASH' USING IB-STATUS WS-ACC-OCN-HASH.
           ADD WS-BL-CNT-01 TO WS-ACC-SEQ-HASH.
       P2700-EDIT-KEY-EXIT.
           EXIT.
      * A FAILURE HERE SUSPENDS THE RECORD, IT DOES NOT STOP THE STEP.
       P2800-EDIT-ROW.
           MOVE 'Y' TO WS-BL-SW-04.
           IF IB-BAN < 1
               MOVE 'N' TO WS-BL-SW-04
               ADD 1 TO WS-BL-CNT-07.
           IF IB-BAN > 2348
               MOVE 'N' TO WS-BL-SW-04
               ADD 1 TO WS-BL-CNT-05.
       P2800-EDIT-ROW-EXIT.
           EXIT.
       P2900-DERIVE-TARIFF.
           UNSTRING WS-BL-TXT-01 DELIMITED BY '/'
               INTO WS-BL-TXT-03
               WS-BL-TXT-05
               TALLYING IN WS-BL-CNT-01.
       P2900-DERIVE-TARIFF-EXIT.
           EXIT.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P21000-SELECT-DESCRIPTION.
           MOVE 0 TO WS-BL-CNT-02.
           INSPECT WS-BL-TXT-05 TALLYING WS-BL-CNT-02
               FOR ALL SPACES.
           INSPECT WS-BL-TXT-05 REPLACING ALL LOW-VALUES BY SPACES.
       P21000-SELECT-DESCRIPTION-EXIT.
           EXIT.
       P21100-SPLIT-DESCRIPTION.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-BL-TXT-06 TO PC-COL-001-020.
           MOVE WS-BL-TXT-02 TO PC-COL-021-060.
           MOVE WS-BL-AMT-02 TO WS-BL-AMT-EDIT.
           MOVE WS-BL-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P21100-SPLIT-DESCRIPTION-EXIT.
           EXIT.
       P21200-APPLY-ELEMENT.
           MOVE SPACES TO CABS-BL-OUT-RECORD.
           MOVE IB-REGION TO OB-OCN.
           MOVE IB-BAND TO OB-CYCLE.
           MOVE IB-CODE TO OB-CENTRE.
           MOVE IB-BAN TO OB-ACCOUNT.
           MOVE IB-REGION TO OB-TARGET.
           WRITE CABS-BL-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P21200-APPLY-ELEMENT-EXIT.
           EXIT.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P21300-EXPAND-BAND.
           MOVE IB-CYCLE TO WS-BL-TXT-04.
           MOVE IB-CYCLE2 TO WS-BL-TXT-04.
           MOVE IB-BAN TO WS-BL-TXT-06.
           MOVE IB-BAND TO WS-BL-TXT-01.
           ADD 1 TO WS-BL-CNT-06.
       P21300-EXPAND-BAND-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P3100-STAGE-ROW.
           CALL 'CABHASH' USING IB-REGION WS-ACC-OCN-HASH.
           ADD WS-BL-CNT-05 TO WS-ACC-SEQ-HASH.
       P3100-STAGE-ROW-EXIT.
           EXIT.
       P3200-RELEASE-ROW.
           MOVE SPACES TO CABS-BL-OUT-RECORD.
           MOVE IB-ACCOUNT TO OB-OCN.
           MOVE IB-OCN TO OB-CYCLE.
           MOVE IB-CYCLE TO OB-CENTRE.
           MOVE IB-CENTRE TO OB-ACCOUNT.
           MOVE IB-TARGET TO OB-TARGET.
           MOVE IB-OCN TO OB-TARIFF.
           WRITE CABS-BL-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P3200-RELEASE-ROW-EXIT.
           EXIT.
       P3300-WRITE-DESCRIPTION.
           MOVE 0 TO WS-BL-QTY-03.
           MOVE 0 TO WS-BL-QTY-02.
           MOVE 0 TO WS-BL-AMT-05.
           MOVE 0 TO WS-BL-AMT-01.
       P3300-WRITE-DESCRIPTION-EXIT.
           EXIT.
       P3400-EMIT-BAND.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-DUP-SEQ TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-BL-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
           MOVE CABS-SUSPENSE-RECORD TO CABS-SUSOUT-RECORD.
           WRITE CABS-SUSOUT-RECORD.
       P3400-EMIT-BAND-EXIT.
           EXIT.
      * S400-SECONDARY-PROCESSING SECTION
       S400-SECONDARY-PROCESSING SECTION.
       P4000-SECONDARY.
           PERFORM P4100-SUMMARISE-TYPE THRU P4100-SUMMARISE-TYPE-EXIT.
           PERFORM P4200-REPORT-CYCLE THRU P4200-REPORT-CYCLE-EXIT.
       P4000-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P4100-SUMMARISE-TYPE.
           CALL 'CABPARMR' USING WS-BL-TXT-02 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-BL-CNT-03.
       P4100-SUMMARISE-TYPE-EXIT.
           EXIT.
       P4200-REPORT-CYCLE.
           MOVE 0 TO WS-BL-QTY-03.
           MOVE 0 TO WS-BL-QTY-02.
           MOVE 0 TO WS-BL-QTY-01.
           MOVE 0 TO WS-BL-AMT-01.
       P4200-REPORT-CYCLE-EXIT.
           EXIT.
           MOVE 0 TO WS-BL-QTY-01.
           PERFORM P350-WALK-WINDOW THRU P350-WALK-WINDOW-EXIT
               VARYING WS-BL-SUB-01 FROM 1 BY 1
               UNTIL WS-BL-SUB-01 > WS-BL-TAB-CNT.
       P350-WALK-WINDOW.
           SET WS-BL-IX TO WS-BL-SUB-01.
           IF WS-BL-TB-KEY (WS-BL-IX) NOT = SPACES
               ADD WS-BL-TB-VAL (WS-BL-IX) TO WS-BL-QTY-01.
       P350-WALK-WINDOW-EXIT.
           EXIT.
      * S800-CONTROL SECTION - THE MANDATORY CABS CONTROL BOUNDARY.
       S800-CONTROL SECTION.
       P8000-CONTROL.
           PERFORM P4000-SECONDARY THRU P4000-EXIT.
           PERFORM P8100-BUILD-CONTROL-REC THRU P8100-EXIT.
           PERFORM P8200-CHECK-BALANCE THRU P8200-EXIT.
           PERFORM P8300-WRITE-CONTROL-REC THRU P8300-EXIT.
       P8000-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE SPACES TO CT-FILLER.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE WS-BL-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE 6 TO CT-STEP-SEQ.
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
           CLOSE TBLIN.
           CLOSE TAROUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABURT10 - END OF RUN'.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  BL-CNT-03 = ' WS-BL-CNT-03.
           DISPLAY '  BL-CNT-06 = ' WS-BL-CNT-06.
           DISPLAY '  BL-CNT-04 = ' WS-BL-CNT-04.
       P9000-EXIT.
           EXIT.
