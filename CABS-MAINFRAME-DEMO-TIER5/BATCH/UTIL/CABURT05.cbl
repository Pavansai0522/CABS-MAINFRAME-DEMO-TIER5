      *****************************************************************
      * CABURT05 - RATE OVERRIDE TABLE LOAD                           *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               BNDIN   TELCABS.CABS.BNDIN          (LOCAL)     *
      *               CTLIN   TELCABS.CABS.CTLIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               TBLOUT  TELCABS.CABS.TBLOUT         (LOCAL)     *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - NOTHING IS UPDATED IN PLACE        *
      * REVISION HISTORY                                              *
      *   V1.00  1987-01-23  P.NAIR       INITIAL RELEASE             *
      *   V1.01  1992-05-11  A.BUKOWSKI   RETIRED THE SECOND SORT STEP*
      *                      - DONE IN PROGRAM                        *
      *   V1.02  1994-03-04  R.T.WHEELER  CENTURY PIVOT APPLIED TO THE*
      *                      CYCLE DATE                               *
      *   V1.06  2007-08-19  T.YAMASHITA  SUSPENSE WRITE ADDED -      *
      *                      RECORDS WERE BEING DROPPED               *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABURT05.
       AUTHOR. TELCABS APPLICATIONS - RATE TABLE MAINTENANCE.
      *****************************************************************
      * RATE OVERRIDE TABLE LOAD. THE STEP IS DRIVEN ENTIRELY FROM THE*
      * SYSIN PARM CARD AND THE DD ALLOCATIONS IN THE JOB.            *
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS*
      * BUILT ON THE SAME ORDER.                                      *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           C01 IS TO-NEW-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BNDIN ASSIGN TO UT-S-BNDIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT CTLIN ASSIGN TO UT-S-CTLIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT TBLOUT ASSIGN TO UT-S-TBLOUT
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
      * BNDIN - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  BNDIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 100 CHARACTERS.
       01  CABS-BT-IN-RECORD.
           05  IB-ELEM                     PIC 9(04).
           05  IB-CODE                     PIC X(16).
           05  IB-GROUP                    PIC X(06).
           05  IB-ELEM2                    PIC X(04).
           05  IB-INVOICE                  PIC S9(13) COMP-3.
           05  IB-CARRIER                  PIC S9(13) COMP-3.
           05  IB-CENTRE                   PIC X(08).
           05  IB-SEQ                      PIC X(03).
           05  IB-ACCOUNT                  PIC X(02).
           05  IB-REGION                   PIC S9(13) COMP-3.
           05  IB-STATUS                   PIC 9(02).
           05  IB-TARGET                   PIC X(02).
           05  IB-CYCLE                    PIC S9(15) COMP-3.
           05  IB-STATUS2                  PIC S9(15) COMP-3.
           05  IB-ACCOUNT2                 PIC S9(07) COMP-3.
           05  IB-CIRCUIT                  PIC S9(09) COMP-3.
           05  BT-FILL-01                  PIC X(7).
      * CTLIN - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  CTLIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 100 CHARACTERS.
       01  CABS-BT-ALT1-RECORD.
           05  A1-OCN                      PIC S9(13) COMP-3.
           05  A1-TARIFF                   PIC S9(07) COMP-3.
           05  A1-PERIOD                   PIC 9(02).
           05  A1-GROUP                    PIC S9(13) COMP-3.
           05  A1-ELEM                     PIC S9(09)V9(02) COMP-3.
           05  A1-CIRCUIT                  PIC X(03).
           05  A1-OCN2                     PIC S9(05) COMP-3.
           05  BT-FILL-02                  PIC X(68).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-BT-VIEW1 REDEFINES CABS-BT-IN-RECORD.
           05  R0B-ACCOUNT                 PIC S9(09)V9(05) COMP-3.
           05  R0B-STATE                   PIC 9(05).
           05  R0B-INVOICE                 PIC X(16).
           05  R0B-CARRIER                 PIC 9(09).
           05  R0B-ACCOUNT2                PIC X(03).
           05  R0B-LEVEL                   PIC X(20).
           05  R0B-STATUS                  PIC S9(13)V9(05) COMP-3.
           05  R0B-CYCLE                   PIC S9(07)V9(02) COMP-3.
           05  R0B-PERIOD                  PIC 9(03).
           05  R0B-REST                    PIC X(21).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-BT-VIEW2 REDEFINES CABS-BT-IN-RECORD.
           05  R1B-JURIS                   PIC S9(09)V9(02) COMP-3.
           05  R1B-OCN                     PIC S9(09) COMP-3.
           05  R1B-CYCLE                   PIC 9(02).
           05  R1B-CENTRE                  PIC S9(07)V9(02) COMP-3.
           05  R1B-STATUS                  PIC S9(13)V9(05) COMP-3.
           05  R1B-TYPE                    PIC 9(06).
           05  R1B-CIRCUIT                 PIC S9(13)V9(05) COMP-3.
           05  R1B-LEVEL                   PIC X(02).
           05  R1B-REST                    PIC X(54).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-BT-VIEW3 REDEFINES CABS-BT-IN-RECORD.
           05  R2B-CODE                    PIC 9(03).
           05  R2B-OCN                     PIC S9(05) COMP-3.
           05  R2B-STATUS                  PIC S9(13)V9(02) COMP-3.
           05  R2B-REGION                  PIC 9(09).
           05  R2B-CLASS                   PIC 9(02).
           05  R2B-STATE                   PIC X(13).
           05  R2B-BAN                     PIC 9(03).
           05  R2B-CODE2                   PIC X(16).
           05  R2B-ACCOUNT                 PIC X(13).
           05  R2B-REST                    PIC X(30).
      * TBLOUT - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  TBLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-BT-OUT-RECORD.
           05  OB-CODE                     PIC X(03).
           05  OB-TYPE                     PIC X(10).
           05  OB-LEVEL                    PIC S9(11)V9(02) COMP-3.
           05  OB-SEQ                      PIC S9(09)V9(05) COMP-3.
           05  OB-TYPE2                    PIC S9(11)V9(02) COMP-3.
           05  OB-CLASS                    PIC X(10).
           05  OB-LEVEL2                   PIC S9(07)V9(02) COMP-3.
           05  OB-SEQ2                     PIC X(03).
           05  OB-TARGET                   PIC S9(11) COMP-3.
           05  OB-SOURCE                   PIC S9(07)V9(02) COMP-3.
           05  OB-LEVEL3                   PIC X(02).
           05  BT-FILL-03                  PIC X(14).
      * SUSOUT - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  SUSOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 300 CHARACTERS.
       01  CABS-SUSOUT-RECORD              PIC X(300).
      * CTLOUT - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD              PIC X(180).
       WORKING-STORAGE SECTION.
      * STANDARD SHARED WORKING STORAGE. SEE CABSWRK.
       COPY CABSWRK.
      * SHARED LAYOUT PULLED IN FOR THE ELEMENT SIDE.
       COPY CABSRT01.
      * SHARED LAYOUT PULLED IN FOR THE BAND SIDE.
       COPY CABSRATE.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABURT05'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V2.12'.
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
       01  WS-COUNT-AREA.
           05  WS-BT-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BT-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BT-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BT-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BT-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BT-CNT-06                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BT-CNT-07                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BT-CNT-08                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BT-CNT-09                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-BT-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BT-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BT-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BT-QTY-04                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-BT-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BT-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BT-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-BT-TXT-01                PIC X(10) VALUE SPACES.
           05  WS-BT-TXT-02                PIC X(30) VALUE SPACES.
           05  WS-BT-TXT-03                PIC X(10) VALUE SPACES.
           05  WS-BT-TXT-04                PIC X(26) VALUE SPACES.
           05  WS-BT-TXT-05                PIC X(08) VALUE SPACES.
           05  WS-BT-TXT-06                PIC X(10) VALUE SPACES.
           05  WS-BT-TXT-07                PIC X(08) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-BT-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-BT-ON-01                 VALUE 'Y'.
               88  WS-BT-OFF-01                VALUE 'N'.
           05  WS-BT-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-BT-ON-02                 VALUE 'Y'.
               88  WS-BT-OFF-02                VALUE 'N'.
           05  WS-BT-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-BT-ON-03                 VALUE 'Y'.
               88  WS-BT-OFF-03                VALUE 'N'.
           05  WS-BT-SW-04                 PIC X(01) VALUE 'N'.
               88  WS-BT-ON-04                 VALUE 'Y'.
               88  WS-BT-OFF-04                VALUE 'N'.
           05  WS-BT-SW-05                 PIC X(01) VALUE 'N'.
               88  WS-BT-ON-05                 VALUE 'Y'.
               88  WS-BT-OFF-05                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-BT-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-BT-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-BT-SUB-03                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. LOADED ONCE AT INITIALISATION AND SEARCHED WITH
      * A SUBSCRIPTED WALK.
       01  WS-BT-TABLE.
           05  WS-BT-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-BT-TB-ENTRY OCCURS 400 TIMES
                                       INDEXED BY WS-BT-IX.
               10  WS-BT-TB-KEY                PIC X(13).
               10  WS-BT-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-BT-TB-TXT                PIC X(20).
               10  WS-BT-TB-EFF                PIC 9(05).
               10  WS-BT-TB-EXP                PIC 9(05).
       01  WS-BT-WORK-GROUP-1.
           05  WS-BT-G1-CARRIER            PIC 9(07).
           05  WS-BT-G1-SOURCE             PIC S9(09) COMP-3.
           05  WS-BT-G1-CLASS              PIC X(10).
           05  WS-BT-G1-STATUS             PIC S9(11)V9(02) COMP-3.
       01  WS-BT-WORK-GROUP-2.
           05  WS-BT-G2-BAN                PIC X(10).
           05  WS-BT-G2-JURIS              PIC S9(11)V9(02) COMP-3.
           05  WS-BT-G2-CIRCUIT            PIC S9(11)V9(02) COMP-3.
           05  WS-BT-G2-REGION             PIC S9(09) COMP-3.
           05  WS-BT-G2-SEQ                PIC S9(11)V9(02) COMP-3.
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 60.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABURT05 - RATE OVERRIDE TABLE LOAD'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-BT-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-BT-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9970.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-BT-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-BT-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT BNDIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON BNDIN - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               DISPLAY 'BNDIN FILE STATUS = ' WS-FS-INPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN INPUT CTLIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON CTLIN - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               DISPLAY 'CTLIN FILE STATUS = ' WS-FS-INPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT TBLOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON TBLOUT - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               DISPLAY 'TBLOUT FILE STATUS = ' WS-FS-OUTPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SUSOUT.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON SUSOUT - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               DISPLAY 'SUSOUT FILE STATUS = ' WS-FS-SUSPENSE
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON CTLOUT - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               DISPLAY 'CTLOUT FILE STATUS = ' WS-FS-CONTROL
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
       P1100-EXIT.
           EXIT.
      * P1200-READ-PARM - THE CYCLE DATE ARRIVES AS TWO DIGITS AND IS
      * PIVOTED ON DW-PIVOT-YY BEFORE ANY DATE MATH.
       P1200-READ-PARM.
           MOVE SPACES TO WS-PARM-CARD.
           ACCEPT WS-PARM-CARD FROM SYSIN.
           CALL 'CABPARMR' USING WS-PARM-CARD WS-RC-PARMR.
           MOVE PC1-CYCLE-YYDDD TO WS-BT-CYCLE-YYDDD.
           COMPUTE WS-BT-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-BT-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-BT-CNT-01.
           MOVE 0 TO WS-BT-CNT-08.
           MOVE 0 TO WS-BT-CNT-07.
       P1200-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-BT-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-BT-TAB-CNT NOT < 400
               MOVE 'Y' TO WS-BT-SW-01
               ADD 1 TO WS-BT-CNT-01
           ELSE
               ADD 1 TO WS-BT-TAB-CNT
               SET WS-BT-IX TO WS-BT-TAB-CNT
               MOVE IB-ELEM2 TO WS-BT-TB-KEY (WS-BT-IX)
               MOVE 0 TO WS-BT-TB-VAL (WS-BT-IX)
               MOVE SPACES TO WS-BT-TB-TXT (WS-BT-IX)
               ADD 1 TO WS-BT-CNT-06.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ BNDIN
               AT END MOVE 'Y' TO WS-BT-SW-01.
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
           PERFORM P2200-BUILD-ROW THRU P2200-BUILD-ROW-EXIT.
           PERFORM P2300-EDIT-KEY THRU P2300-EDIT-KEY-EXIT.
           PERFORM P2400-DERIVE-TARIFF THRU P2400-DERIVE-TARIFF-EXIT.
           PERFORM P2500-CONVERT-BAND THRU P2500-CONVERT-BAND-EXIT.
           PERFORM P2600-SPLIT-OVERRIDE THRU P2600-SPLIT-OVERRIDE-EXIT.
           PERFORM P2700-CHECK-ROW THRU P2700-CHECK-ROW-EXIT.
           PERFORM P2800-APPLY-DESCRIPTION THRU
               P2800-APPLY-DESCRIPTION-EXIT.
           IF WS-BT-ON-05
               PERFORM P2900-EXPAND-TARIFF THRU
                   P2900-EXPAND-TARIFF-EXIT.
           IF WS-BT-ON-02
               PERFORM P21000-CHECK-KEY THRU P21000-CHECK-KEY-EXIT.
           PERFORM P21100-BUILD-BAND THRU P21100-BUILD-BAND-EXIT.
           PERFORM P21200-APPLY-ROW THRU P21200-APPLY-ROW-EXIT.
           PERFORM P21300-VALIDATE-KEY THRU P21300-VALIDATE-KEY-EXIT.
           PERFORM P21400-EXPAND-DESCRIPTION THRU
               P21400-EXPAND-DESCRIPTION-EXIT.
           PERFORM P21500-BUILD-WINDOW THRU P21500-BUILD-WINDOW-EXIT.
           PERFORM P21600-MATCH-TARIFF THRU P21600-MATCH-TARIFF-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ BNDIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P2200-BUILD-ROW.
           IF IB-TARGET = 'S'
               ADD 1 TO WS-BT-CNT-06
           ELSE
               IF IB-TARGET = 'C'
                   ADD 1 TO WS-BT-CNT-04
               ELSE
                   IF IB-TARGET = 'B'
                       ADD 1 TO WS-BT-CNT-01
                   ELSE
                       ADD 1 TO WS-BT-CNT-06.
           MOVE 'N' TO WS-BT-SW-03.
           IF WS-BT-TAB-CNT > 0
               PERFORM P270-COMPARE-WINDOW THRU P270-COMPARE-WINDOW-EXIT
               VARYING WS-BT-SUB-01 FROM 1 BY 1
               UNTIL WS-BT-SUB-01 > WS-BT-TAB-CNT
               OR WS-BT-SW-03 = 'Y'.
       P2200-BUILD-ROW-EXIT.
           EXIT.
       P2300-EDIT-KEY.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-BT-TXT-04 TO PC-COL-001-020.
           MOVE WS-BT-TXT-06 TO PC-COL-021-060.
           MOVE WS-BT-AMT-01 TO WS-BT-AMT-EDIT.
           MOVE WS-BT-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P2300-EDIT-KEY-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P2400-DERIVE-TARIFF.
           CALL 'CABHASH' USING IB-CYCLE WS-ACC-OCN-HASH.
           ADD WS-BT-CNT-09 TO WS-ACC-SEQ-HASH.
       P2400-DERIVE-TARIFF-EXIT.
           EXIT.
       P2500-CONVERT-BAND.
           IF WS-BT-AMT-03 NOT = 0
               COMPUTE WS-BT-QTY-04 = WS-BT-AMT-03 * 100 / WS-BT-AMT-03
           ELSE
               MOVE 0 TO WS-BT-QTY-04.
       P2500-CONVERT-BAND-EXIT.
           EXIT.
       P2600-SPLIT-OVERRIDE.
           MOVE SPACES TO WS-BT-TXT-02.
           STRING IB-ELEM DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IB-CYCLE DELIMITED BY SIZE
               INTO WS-BT-TXT-02.
       P2600-SPLIT-OVERRIDE-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P2700-CHECK-ROW.
           ADD IB-CIRCUIT TO WS-BT-QTY-01.
           COMPUTE WS-BT-AMT-03 = WS-BT-QTY-01 * WS-BT-QTY-04.
           ADD WS-BT-AMT-03 TO WS-BT-AMT-02.
       P2700-CHECK-ROW-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P2800-APPLY-DESCRIPTION.
           CALL 'CABPARMR' USING WS-BT-TXT-02 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-BT-CNT-02.
       P2800-APPLY-DESCRIPTION-EXIT.
           EXIT.
       P2900-EXPAND-TARIFF.
           MOVE 0 TO WS-BT-QTY-02.
           MOVE 0 TO WS-BT-QTY-03.
           MOVE 0 TO WS-BT-QTY-01.
           MOVE 0 TO WS-BT-AMT-02.
           MOVE 0 TO WS-BT-AMT-03.
       P2900-EXPAND-TARIFF-EXIT.
           EXIT.
      * A FAILURE HERE SUSPENDS THE RECORD, IT DOES NOT STOP THE STEP.
       P21000-CHECK-KEY.
           MOVE WS-BT-AMT-03 TO WS-BT-AMT-01.
           IF WS-BT-AMT-01 < 0
               COMPUTE WS-BT-AMT-01 = 0 - WS-BT-AMT-03.
       P21000-CHECK-KEY-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P21100-BUILD-BAND.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-RATE-NOT-FOUND TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-BT-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
       P21100-BUILD-BAND-EXIT.
           EXIT.
       P21200-APPLY-ROW.
           MOVE 'Y' TO WS-BT-SW-02.
           IF IB-INVOICE < 22
               MOVE 'N' TO WS-BT-SW-02
               ADD 1 TO WS-BT-CNT-05.
           IF IB-INVOICE > 576
               MOVE 'N' TO WS-BT-SW-02
               ADD 1 TO WS-BT-CNT-09.
       P21200-APPLY-ROW-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P21300-VALIDATE-KEY.
           IF WS-BT-AMT-02 < 6
               MOVE 6 TO WS-BT-AMT-02
               ADD 1 TO WS-BT-CNT-06.
           IF WS-BT-AMT-02 > 30965
               MOVE 30965 TO WS-BT-AMT-02
               ADD 1 TO WS-BT-CNT-01.
       P21300-VALIDATE-KEY-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P21400-EXPAND-DESCRIPTION.
           UNSTRING WS-BT-TXT-02 DELIMITED BY '/'
               INTO WS-BT-TXT-02
               WS-BT-TXT-05
               TALLYING IN WS-BT-CNT-07.
       P21400-EXPAND-DESCRIPTION-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P21500-BUILD-WINDOW.
           MOVE 0 TO WS-BT-CNT-02.
           INSPECT WS-BT-TXT-05 TALLYING WS-BT-CNT-02
               FOR ALL SPACES.
           INSPECT WS-BT-TXT-05 REPLACING ALL LOW-VALUES BY SPACES.
       P21500-BUILD-WINDOW-EXIT.
           EXIT.
      * A FAILURE HERE SUSPENDS THE RECORD, IT DOES NOT STOP THE STEP.
       P21600-MATCH-TARIFF.
           MOVE IB-STATUS TO WS-BT-TXT-03.
           MOVE IB-CIRCUIT TO WS-BT-TXT-02.
           MOVE IB-ELEM TO WS-BT-TXT-05.
           MOVE IB-SEQ TO WS-BT-TXT-03.
           ADD 1 TO WS-BT-CNT-04.
       P21600-MATCH-TARIFF-EXIT.
           EXIT.
       P270-COMPARE-WINDOW.
           SET WS-BT-IX TO WS-BT-SUB-02.
           IF WS-BT-TB-KEY (WS-BT-IX) = IB-CODE
               MOVE 'Y' TO WS-BT-SW-01
               MOVE WS-BT-TB-VAL (WS-BT-IX) TO WS-BT-QTY-02
               MOVE WS-BT-SUB-02 TO WS-BT-SUB-01.
       P270-COMPARE-WINDOW-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-EMIT-WINDOW.
           MOVE 0 TO WS-BT-QTY-04.
           MOVE 0 TO WS-BT-QTY-01.
           MOVE 0 TO WS-BT-QTY-03.
           MOVE 0 TO WS-BT-AMT-02.
           MOVE 0 TO WS-BT-AMT-03.
       P3100-EMIT-WINDOW-EXIT.
           EXIT.
       P3200-CLOSE-OFF-ROW.
           MOVE IB-ELEM2 TO WS-BT-TXT-06.
           MOVE IB-ELEM TO WS-BT-TXT-03.
           MOVE IB-CYCLE TO WS-BT-TXT-04.
           ADD 1 TO WS-BT-CNT-01.
       P3200-CLOSE-OFF-ROW-EXIT.
           EXIT.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P3300-WRITE-DESCRIPTION.
           MOVE SPACES TO WS-BT-TXT-07.
           STRING IB-SEQ DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IB-CENTRE DELIMITED BY SIZE
               INTO WS-BT-TXT-07.
       P3300-WRITE-DESCRIPTION-EXIT.
           EXIT.
       P3400-CLOSE-OFF-KEY.
           ADD IB-CIRCUIT TO WS-BT-QTY-04.
           COMPUTE WS-BT-AMT-02 ROUNDED = WS-BT-QTY-04 * WS-BT-QTY-01.
           ADD WS-BT-AMT-02 TO WS-BT-AMT-03.
       P3400-CLOSE-OFF-KEY-EXIT.
           EXIT.
      * S400-SECONDARY-PROCESSING SECTION
       S400-SECONDARY-PROCESSING SECTION.
       P4000-SECONDARY.
           PERFORM P4100-NORMALISE-LEVEL THRU
               P4100-NORMALISE-LEVEL-EXIT.
           PERFORM P4200-TRACE-STATE THRU P4200-TRACE-STATE-EXIT.
       P4000-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P4100-NORMALISE-LEVEL.
           CALL 'CABEDITF' USING WS-BT-TXT-05 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-BT-CNT-07.
       P4100-NORMALISE-LEVEL-EXIT.
           EXIT.
       P4200-TRACE-STATE.
           IF WS-BT-AMT-02 NOT = 0
               COMPUTE WS-BT-QTY-02 = WS-BT-AMT-01 * 100 / WS-BT-AMT-02
           ELSE
               MOVE 0 TO WS-BT-QTY-02.
       P4200-TRACE-STATE-EXIT.
           EXIT.
           MOVE 0 TO WS-BT-QTY-03.
           PERFORM P350-WALK-KEY THRU P350-WALK-KEY-EXIT
               VARYING WS-BT-SUB-02 FROM 1 BY 1
               UNTIL WS-BT-SUB-02 > WS-BT-TAB-CNT.
       P350-WALK-KEY.
           SET WS-BT-IX TO WS-BT-SUB-01.
           IF WS-BT-TB-KEY (WS-BT-IX) NOT = SPACES
               ADD WS-BT-TB-VAL (WS-BT-IX) TO WS-BT-QTY-02.
       P350-WALK-KEY-EXIT.
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
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE SPACES TO CT-FILLER.
           MOVE 0 TO CT-RC.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-BT-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE 1 TO CT-STEP-SEQ.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
       P8100-EXIT.
           EXIT.
      * P8200-CHECK-BALANCE - A RECORD THAT WAS HELD FOR THE NEXT
      * CYCLE IS BROUGHT INTO THE CARRIED FORWARD SIDE BEFORE THE
      * EQUATION IS TESTED.
       P8200-CHECK-BALANCE.
           ADD WS-BT-CNT-04 TO CT-CARRIED-FWD.
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
           CLOSE BNDIN.
           CLOSE CTLIN.
           CLOSE TBLOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABURT05 - END OF RUN'.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  BT-CNT-03 = ' WS-BT-CNT-03.
           DISPLAY '  BT-CNT-05 = ' WS-BT-CNT-05.
       P9000-EXIT.
           EXIT.
