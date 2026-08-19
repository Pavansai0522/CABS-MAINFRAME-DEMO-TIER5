      *****************************************************************
      * CABUXR04 - CARRIER TO BILLING ACCOUNT CROSS REFERENCE         *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               MSTIN   TELCABS.CABS.MSTIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               LNKOUT  TELCABS.CABS.LNKOUT         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - THE OUTPUT IS REBUILT EACH RUN     *
      * REVISION HISTORY                                              *
      *   V1.00  1987-08-15  K.O.BRIEN    INITIAL RELEASE             *
      *   V1.01  1988-05-14  M.DELACROIX  RECOMPILE ONLY - COPYBOOK   *
      *                      CHANGE UPSTREAM                          *
      *   V1.02  1993-04-15  K.O.BRIEN    PARM CARD EXTENDED,         *
      *                      POSITIONS 40 THROUGH 48                  *
      *   V1.03  2015-04-19  B.R.HALVORSEN CENTURY PIVOT APPLIED TO   *
      *                      THE CYCLE DATE                           *
      *   V1.06  2018-06-13  J.M.CASTILLO RESTART KEY WRITTEN SO A    *
      *                      RERUN CAN POSITION                       *
      *   V1.09  2019-02-12  S.MARCHETTI  REGION SIZE REDUCED - TABLE *
      *                      MOVED OUT OF WORKING STORAGE             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUXR04.
       AUTHOR. TELCABS APPLICATIONS - CROSS REFERENCE REPORT.
      *****************************************************************
      * CARRIER TO BILLING ACCOUNT CROSS REFERENCE. THE STEP IS       *
      * SCHEDULED MONTHLY AND ALSO RUN ON DEMAND WHEN A CENTRE ASKS   *
      * FOR IT.                                                       *
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND   *
      * THERE NEVER HAS BEEN.                                         *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           C01 IS TO-NEW-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MSTIN ASSIGN TO UT-S-MSTIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT LNKOUT ASSIGN TO UT-S-LNKOUT
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
      * MSTIN - WORK FILE, DELETED AT STEP END.
       FD  MSTIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 100 CHARACTERS.
       01  CABS-CF-IN-RECORD.
           05  IC-STATUS                   PIC S9(11) COMP-3.
           05  IC-ELEM                     PIC 9(02).
           05  IC-SOURCE                   PIC X(10).
           05  IC-CLASS                    PIC S9(05) COMP-3.
           05  IC-CARRIER                  PIC X(03).
           05  IC-CLASS2                   PIC X(10).
           05  IC-SEQ                      PIC X(13).
           05  IC-JURIS                    PIC X(06).
           05  IC-CLASS3                   PIC 9(06).
           05  IC-CYCLE                    PIC X(13).
           05  IC-ACCOUNT                  PIC X(13).
           05  IC-INVOICE                  PIC S9(07)V9(02) COMP-3.
           05  IC-MEDIA                    PIC S9(13)V9(02) COMP-3.
           05  CF-FILL-01                  PIC X(2).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-CF-VIEW1 REDEFINES CABS-CF-IN-RECORD.
           05  R0C-CLASS                   PIC 9(06).
           05  R0C-TARIFF                  PIC S9(11) COMP-3.
           05  R0C-CLASS2                  PIC 9(04).
           05  R0C-JURIS                   PIC S9(09)V9(05) COMP-3.
           05  R0C-STATUS                  PIC X(20).
           05  R0C-LEVEL                   PIC 9(04).
           05  R0C-CARRIER                 PIC S9(09)V9(02) COMP-3.
           05  R0C-ACCOUNT                 PIC 9(05).
           05  R0C-REST                    PIC X(41).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-CF-VIEW2 REDEFINES CABS-CF-IN-RECORD.
           05  R1C-CODE                    PIC 9(03).
           05  R1C-OCN                     PIC X(20).
           05  R1C-LEVEL                   PIC S9(07)V9(05) COMP-3.
           05  R1C-REGION                  PIC X(16).
           05  R1C-TYPE                    PIC X(16).
           05  R1C-CLASS                   PIC X(20).
           05  R1C-TYPE2                   PIC S9(15) COMP-3.
           05  R1C-REST                    PIC X(10).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-CF-VIEW3 REDEFINES CABS-CF-IN-RECORD.
           05  R2C-PERIOD                  PIC S9(07) COMP-3.
           05  R2C-BAND                    PIC X(06).
           05  R2C-SOURCE                  PIC S9(07)V9(05) COMP-3.
           05  R2C-INVOICE                 PIC S9(09) COMP-3.
           05  R2C-LEVEL                   PIC X(20).
           05  R2C-TARIFF                  PIC S9(13)V9(02) COMP-3.
           05  R2C-BAND2                   PIC X(10).
           05  R2C-SOURCE2                 PIC S9(11) COMP-3.
           05  R2C-REST                    PIC X(34).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-CF-VIEW4 REDEFINES CABS-CF-IN-RECORD.
           05  R3C-MEDIA                   PIC S9(07) COMP-3.
           05  R3C-CYCLE                   PIC S9(13) COMP-3.
           05  R3C-MEDIA2                  PIC S9(13) COMP-3.
           05  R3C-SOURCE                  PIC S9(15) COMP-3.
           05  R3C-SOURCE2                 PIC S9(07)V9(02) COMP-3.
           05  R3C-REST                    PIC X(69).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-CF-VIEW5 REDEFINES CABS-CF-IN-RECORD.
           05  R4C-TYPE                    PIC 9(03).
           05  R4C-SOURCE                  PIC S9(07)V9(02) COMP-3.
           05  R4C-STATUS                  PIC X(03).
           05  R4C-INVOICE                 PIC S9(11)V9(02) COMP-3.
           05  R4C-REST                    PIC X(82).
      * LNKOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  LNKOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-CF-OUT-RECORD.
           05  OC-CIRCUIT                  PIC S9(09) COMP-3.
           05  OC-OCN                      PIC S9(15) COMP-3.
           05  OC-CYCLE                    PIC X(06).
           05  OC-CARRIER                  PIC X(04).
           05  OC-CARRIER2                 PIC X(03).
           05  OC-SEQ                      PIC X(08).
           05  OC-GROUP                    PIC 9(07).
           05  OC-TYPE                     PIC S9(07) COMP-3.
           05  OC-LEVEL                    PIC S9(13)V9(02) COMP-3.
           05  OC-BAN                      PIC X(02).
           05  OC-TYPE2                    PIC 9(04).
           05  CF-FILL-02                  PIC X(21).
      * CTLOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD              PIC X(180).
      * RPTOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  RPTOUT
           RECORDING MODE IS FA
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       COPY CABSPRNT.
       WORKING-STORAGE SECTION.
      * STANDARD SHARED WORKING STORAGE. SEE CABSWRK.
       COPY CABSWRK.
      * SHARED LAYOUT PULLED IN FOR THE MATCH SIDE.
       COPY CABSBHDR.
      * SHARED LAYOUT PULLED IN FOR THE PAIR SIDE.
       COPY CABSCIRC.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUXR04'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V1.03'.
           05  WS-TABLE-LIMIT              PIC S9(04) COMP-3
                                         VALUE 300.
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
           05  WS-CF-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CF-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CF-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CF-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CF-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CF-CNT-06                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CF-CNT-07                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CF-CNT-08                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-CF-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CF-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CF-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-CF-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CF-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CF-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-CF-TXT-01                PIC X(30) VALUE SPACES.
           05  WS-CF-TXT-02                PIC X(30) VALUE SPACES.
           05  WS-CF-TXT-03                PIC X(16) VALUE SPACES.
           05  WS-CF-TXT-04                PIC X(12) VALUE SPACES.
           05  WS-CF-TXT-05                PIC X(26) VALUE SPACES.
           05  WS-CF-TXT-06                PIC X(16) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-CF-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-CF-ON-01                 VALUE 'Y'.
               88  WS-CF-OFF-01                VALUE 'N'.
           05  WS-CF-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-CF-ON-02                 VALUE 'Y'.
               88  WS-CF-OFF-02                VALUE 'N'.
           05  WS-CF-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-CF-ON-03                 VALUE 'Y'.
               88  WS-CF-OFF-03                VALUE 'N'.
           05  WS-CF-SW-04                 PIC X(01) VALUE 'N'.
               88  WS-CF-ON-04                 VALUE 'Y'.
               88  WS-CF-OFF-04                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-CF-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CF-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CF-SUB-03                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. LOADED ONCE AT INITIALISATION AND SEARCHED WITH
      * A SUBSCRIPTED WALK.
       01  WS-CF-TABLE.
           05  WS-CF-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-CF-TB-ENTRY OCCURS 300 TIMES
                                       INDEXED BY WS-CF-IX.
               10  WS-CF-TB-KEY                PIC X(06).
               10  WS-CF-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-CF-TB-TXT                PIC X(20).
               10  WS-CF-TB-EFF                PIC 9(05).
               10  WS-CF-TB-EXP                PIC 9(05).
       01  WS-CF-WORK-GROUP-1.
           05  WS-CF-G1-CLASS              PIC S9(11)V9(02) COMP-3.
           05  WS-CF-G1-SEQ                PIC 9(07).
           05  WS-CF-G1-STATUS             PIC S9(09) COMP-3.
           05  WS-CF-G1-CLASS              PIC 9(05).
           05  WS-CF-G1-ACCOUNT            PIC X(20).
           05  WS-CF-G1-CIRCUIT            PIC S9(09) COMP-3.
       01  WS-CF-WORK-GROUP-2.
           05  WS-CF-G2-TARGET             PIC X(20).
           05  WS-CF-G2-ACCOUNT            PIC X(10).
           05  WS-CF-G2-CLASS              PIC S9(11)V9(02) COMP-3.
           05  WS-CF-G2-CIRCUIT            PIC S9(11)V9(02) COMP-3.
           05  WS-CF-G2-OCN                PIC 9(05).
       01  WS-CF-WORK-GROUP-3.
           05  WS-CF-G3-SOURCE             PIC 9(07).
           05  WS-CF-G3-CYCLE              PIC 9(07).
           05  WS-CF-G3-CLASS              PIC X(10).
           05  WS-CF-G3-CYCLE              PIC S9(09) COMP-3.
           05  WS-CF-G3-BAND               PIC X(10).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 50.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUXR04 - CARRIER TO BILLING ACCOUNT CROSS REFER'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-CF-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-CF-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9950.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-CF-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-CF-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT MSTIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF MSTIN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT LNKOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF LNKOUT' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF CTLOUT' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RPTOUT.
           IF WS-FS-OUTPUT NOT = '00'
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
           MOVE PC1-CYCLE-YYDDD TO WS-CF-CYCLE-YYDDD.
           COMPUTE WS-CF-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-CF-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-CF-CNT-02.
           MOVE 0 TO WS-CF-CNT-06.
           MOVE 0 TO WS-CF-CNT-03.
       P1200-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-CF-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-CF-TAB-CNT NOT < 300
               MOVE 'Y' TO WS-CF-SW-01
               ADD 1 TO WS-CF-CNT-04
           ELSE
               ADD 1 TO WS-CF-TAB-CNT
               SET WS-CF-IX TO WS-CF-TAB-CNT
               MOVE IC-CLASS TO WS-CF-TB-KEY (WS-CF-IX)
               MOVE 0 TO WS-CF-TB-VAL (WS-CF-IX)
               MOVE SPACES TO WS-CF-TB-TXT (WS-CF-IX)
               ADD 1 TO WS-CF-CNT-06.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ MSTIN
               AT END MOVE 'Y' TO WS-CF-SW-01.
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
           PERFORM P2200-BUILD-ORPHAN THRU P2200-BUILD-ORPHAN-EXIT.
           PERFORM P2300-EXPAND-MATCH THRU P2300-EXPAND-MATCH-EXIT.
           IF WS-CF-ON-01
               PERFORM P2400-DERIVE-GROUP THRU P2400-DERIVE-GROUP-EXIT.
           PERFORM P2500-MATCH-SIDE THRU P2500-MATCH-SIDE-EXIT.
           IF WS-CF-ON-03
               PERFORM P2600-BUILD-SIDE THRU P2600-BUILD-SIDE-EXIT.
           PERFORM P2700-RESOLVE-MATCH THRU P2700-RESOLVE-MATCH-EXIT.
           PERFORM P2800-CONVERT-REFERENCE THRU
               P2800-CONVERT-REFERENCE-EXIT.
           PERFORM P2900-MATCH-SIDE THRU P2900-MATCH-SIDE-EXIT.
           PERFORM P21000-CHECK-ORPHAN THRU P21000-CHECK-ORPHAN-EXIT.
           PERFORM P21100-SELECT-PAIR THRU P21100-SELECT-PAIR-EXIT.
           PERFORM P21200-SPLIT-SIDE THRU P21200-SPLIT-SIDE-EXIT.
           PERFORM P21300-EXPAND-GROUP THRU P21300-EXPAND-GROUP-EXIT.
           PERFORM P21400-SELECT-REFERENCE THRU
               P21400-SELECT-REFERENCE-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ MSTIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P2200-BUILD-ORPHAN.
           MOVE 'N' TO WS-CF-SW-03.
           IF WS-CF-TXT-06 NOT = WS-CF-TXT-02
               MOVE 'Y' TO WS-CF-SW-03
               MOVE WS-CF-TXT-06 TO WS-CF-TXT-02
               ADD 1 TO WS-CF-CNT-08.
           MOVE 'N' TO WS-CF-SW-03.
           IF WS-CF-TAB-CNT > 0
               PERFORM P280-COMPARE-PAIR THRU P280-COMPARE-PAIR-EXIT
               VARYING WS-CF-SUB-01 FROM 1 BY 1
               UNTIL WS-CF-SUB-01 > WS-CF-TAB-CNT
               OR WS-CF-SW-03 = 'Y'.
       P2200-BUILD-ORPHAN-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P2300-EXPAND-MATCH.
           IF IC-JURIS = 'S'
               ADD 1 TO WS-CF-CNT-07
           ELSE
               IF IC-JURIS = 'C'
                   ADD 1 TO WS-CF-CNT-05
               ELSE
                   IF IC-JURIS = 'S'
                       ADD 1 TO WS-CF-CNT-08
                   ELSE
                       ADD 1 TO WS-CF-CNT-01.
       P2300-EXPAND-MATCH-EXIT.
           EXIT.
       P2400-DERIVE-GROUP.
           MOVE IC-JURIS TO WS-CF-TXT-05.
           MOVE IC-SOURCE TO WS-CF-TXT-06.
           MOVE IC-SEQ TO WS-CF-TXT-03.
           ADD 1 TO WS-CF-CNT-08.
       P2400-DERIVE-GROUP-EXIT.
           EXIT.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P2500-MATCH-SIDE.
           MOVE SPACES TO WS-CF-TXT-06.
           STRING IC-ELEM DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IC-ACCOUNT DELIMITED BY SIZE
               INTO WS-CF-TXT-06.
       P2500-MATCH-SIDE-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P2600-BUILD-SIDE.
           MOVE SPACES TO CABS-CF-OUT-RECORD.
           MOVE IC-CARRIER TO OC-CIRCUIT.
           MOVE IC-CLASS3 TO OC-OCN.
           MOVE IC-INVOICE TO OC-CYCLE.
           MOVE IC-INVOICE TO OC-CARRIER.
           MOVE IC-MEDIA TO OC-CARRIER2.
           MOVE IC-SOURCE TO OC-SEQ.
           WRITE CABS-CF-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P2600-BUILD-SIDE-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P2700-RESOLVE-MATCH.
           MOVE 'Y' TO WS-CF-SW-03.
           IF IC-CLASS3 < 38
               MOVE 'N' TO WS-CF-SW-03
               ADD 1 TO WS-CF-CNT-02.
           IF IC-CLASS3 > 2965
               MOVE 'N' TO WS-CF-SW-03
               ADD 1 TO WS-CF-CNT-07.
       P2700-RESOLVE-MATCH-EXIT.
           EXIT.
       P2800-CONVERT-REFERENCE.
           MOVE 0 TO WS-CF-CNT-06.
           INSPECT WS-CF-TXT-06 TALLYING WS-CF-CNT-06
               FOR ALL SPACES.
           INSPECT WS-CF-TXT-06 REPLACING ALL LOW-VALUES BY SPACES.
       P2800-CONVERT-REFERENCE-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P2900-MATCH-SIDE.
           CALL 'CABHASH' USING IC-CARRIER WS-ACC-OCN-HASH.
           ADD WS-CF-CNT-01 TO WS-ACC-SEQ-HASH.
       P2900-MATCH-SIDE-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P21000-CHECK-ORPHAN.
           ADD IC-MEDIA TO WS-CF-QTY-03.
           COMPUTE WS-CF-AMT-03 = WS-CF-QTY-03 * WS-CF-QTY-03.
           ADD WS-CF-AMT-03 TO WS-CF-AMT-02.
       P21000-CHECK-ORPHAN-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P21100-SELECT-PAIR.
           MOVE 0 TO WS-CF-QTY-03.
           MOVE 0 TO WS-CF-QTY-02.
           MOVE 0 TO WS-CF-AMT-03.
           MOVE 0 TO WS-CF-AMT-01.
       P21100-SELECT-PAIR-EXIT.
           EXIT.
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT
      * PRECEDES THIS PROGRAM IN THE JOB.
       P21200-SPLIT-SIDE.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-OCN-UNKNOWN TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-CF-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
       P21200-SPLIT-SIDE-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P21300-EXPAND-GROUP.
           MOVE WS-CF-AMT-01 TO WS-CF-AMT-01.
           IF WS-CF-AMT-01 < 0
               COMPUTE WS-CF-AMT-01 = 0 - WS-CF-AMT-01.
       P21300-EXPAND-GROUP-EXIT.
           EXIT.
       P21400-SELECT-REFERENCE.
           CALL 'CABPARMR' USING WS-CF-TXT-02 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-CF-CNT-07.
       P21400-SELECT-REFERENCE-EXIT.
           EXIT.
       P280-COMPARE-PAIR.
           SET WS-CF-IX TO WS-CF-SUB-02.
           IF WS-CF-TB-KEY (WS-CF-IX) = IC-JURIS
               MOVE 'Y' TO WS-CF-SW-02
               MOVE WS-CF-TB-VAL (WS-CF-IX) TO WS-CF-QTY-03
               MOVE WS-CF-SUB-02 TO WS-CF-SUB-03.
       P280-COMPARE-PAIR-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-CLOSE-OFF-PAIR.
           MOVE 0 TO WS-CF-QTY-03.
           MOVE 0 TO WS-CF-QTY-02.
           MOVE 0 TO WS-CF-QTY-01.
           MOVE 0 TO WS-CF-AMT-01.
           MOVE 0 TO WS-CF-AMT-03.
       P3100-CLOSE-OFF-PAIR-EXIT.
           EXIT.
       P3200-STAGE-ORPHAN.
           MOVE SPACES TO CABS-CF-OUT-RECORD.
           MOVE IC-ELEM TO OC-CIRCUIT.
           MOVE IC-CLASS TO OC-OCN.
           MOVE IC-CLASS3 TO OC-CYCLE.
           MOVE IC-ELEM TO OC-CARRIER.
           MOVE IC-CLASS TO OC-CARRIER2.
           WRITE CABS-CF-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P3200-STAGE-ORPHAN-EXIT.
           EXIT.
       P3300-WRITE-ORPHAN.
           ADD IC-MEDIA TO WS-CF-QTY-01.
           COMPUTE WS-CF-AMT-03 = WS-CF-QTY-01 * WS-CF-QTY-02.
           ADD WS-CF-AMT-03 TO WS-CF-AMT-02.
       P3300-WRITE-ORPHAN-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P3400-CLOSE-OFF-ORPHAN.
           MOVE SPACES TO WS-CF-TXT-05.
           STRING IC-MEDIA DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IC-JURIS DELIMITED BY SIZE
               INTO WS-CF-TXT-05.
       P3400-CLOSE-OFF-ORPHAN-EXIT.
           EXIT.
      * S400-SECONDARY-PROCESSING SECTION
       S400-SECONDARY-PROCESSING SECTION.
       P4000-SECONDARY.
           PERFORM P4100-COMPARE-OCN THRU P4100-COMPARE-OCN-EXIT.
           PERFORM P4200-NORMALISE-TARIFF THRU
               P4200-NORMALISE-TARIFF-EXIT.
       P4000-EXIT.
           EXIT.
       P4100-COMPARE-OCN.
           CALL 'CABHASH' USING IC-CARRIER WS-ACC-OCN-HASH.
           ADD WS-CF-CNT-01 TO WS-ACC-SEQ-HASH.
       P4100-COMPARE-OCN-EXIT.
           EXIT.
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT
      * PRECEDES THIS PROGRAM IN THE JOB.
       P4200-NORMALISE-TARIFF.
           IF IC-SOURCE = 'X'
               ADD 1 TO WS-CF-CNT-06
           ELSE
               IF IC-SOURCE = 'E'
                   ADD 1 TO WS-CF-CNT-01
               ELSE
                   IF IC-SOURCE = 'X'
                       ADD 1 TO WS-CF-CNT-08
                   ELSE
                       ADD 1 TO WS-CF-CNT-06.
       P4200-NORMALISE-TARIFF-EXIT.
           EXIT.
           MOVE 0 TO WS-CF-QTY-03.
           PERFORM P360-WALK-MATCH THRU P360-WALK-MATCH-EXIT
               VARYING WS-CF-SUB-03 FROM 1 BY 1
               UNTIL WS-CF-SUB-03 > WS-CF-TAB-CNT.
       P360-WALK-MATCH.
           SET WS-CF-IX TO WS-CF-SUB-03.
           IF WS-CF-TB-KEY (WS-CF-IX) NOT = SPACES
               ADD WS-CF-TB-VAL (WS-CF-IX) TO WS-CF-QTY-03.
       P360-WALK-MATCH-EXIT.
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
           MOVE 'READ FROM INPUT' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO WS-CF-CNT-EDIT.
           MOVE WS-CF-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'WRITTEN TO OUTPUT' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-CF-CNT-EDIT.
           MOVE WS-CF-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'REJECTED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-CF-CNT-EDIT.
           MOVE WS-CF-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'ROLLED INTO SUMMARY' TO PC-COL-001-020.
           MOVE WS-SUMM-CNT TO WS-CF-CNT-EDIT.
           MOVE WS-CF-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'CARRIED FORWARD' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-CF-CNT-EDIT.
           MOVE WS-CF-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-CF-CNT-01 TO WS-CF-CNT-EDIT.
           MOVE WS-CF-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 02' TO PC-COL-001-020.
           MOVE WS-CF-CNT-02 TO WS-CF-CNT-EDIT.
           MOVE WS-CF-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 03' TO PC-COL-001-020.
           MOVE WS-CF-CNT-03 TO WS-CF-CNT-EDIT.
           MOVE WS-CF-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE WS-CF-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE 4 TO CT-STEP-SEQ.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-FILLER.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE WS-CF-TXT-01 TO CT-RESTART-KEY.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
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
           CLOSE MSTIN.
           CLOSE LNKOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABUXR04 - STEP COMPLETE'.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  CF-CNT-05 = ' WS-CF-CNT-05.
           DISPLAY '  CF-CNT-08 = ' WS-CF-CNT-08.
           DISPLAY '  CF-CNT-04 = ' WS-CF-CNT-04.
       P9000-EXIT.
           EXIT.
