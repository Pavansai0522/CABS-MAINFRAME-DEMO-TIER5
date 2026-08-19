      *****************************************************************
      * CABUCV18 - INTERCHANGE FORMAT CONVERSION                      *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               OLDIN   TELCABS.CABS.OLDIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               UPLOUT  TELCABS.CABS.UPLOUT         (LOCAL)     *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * REVISION HISTORY                                              *
      *   V1.00  1989-01-10  J.M.CASTILLO INITIAL RELEASE             *
      *   V1.03  1991-03-08  D.OKONKWO    SUSPENSE WRITE ADDED -      *
      *                      RECORDS WERE BEING DROPPED               *
      *   V1.05  1993-09-04  A.BUKOWSKI   PRINT LINE WIDENED TO 133   *
      *   V1.07  1995-12-11  T.YAMASHITA  CARRIER TYPE BROUGHT ONTO   *
      *                      THE EXTRACT                              *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUCV18.
       AUTHOR. TELCABS APPLICATIONS - FILE CONVERSION.
      *****************************************************************
      * INTERCHANGE FORMAT CONVERSION. THE STEP RUNS ONCE PER BILL    *
      * CYCLE AND IS RERUN FROM THE TOP IF IT FAILS.                  *
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
           SELECT OLDIN ASSIGN TO UT-S-OLDIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT UPLOUT ASSIGN TO UT-S-UPLOUT
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
      * OLDIN - PERMANENT DATASET HELD ON DASD.
       FD  OLDIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 130 CHARACTERS.
       01  CABS-BA-IN-RECORD.
           05  IB-CENTRE                   PIC S9(11) COMP-3.
           05  IB-SEGMENT                  PIC X(06).
           05  IB-TARIFF                   PIC X(04).
           05  IB-CARRIER                  PIC 9(04).
           05  IB-MEDIA                    PIC S9(07)V9(02) COMP-3.
           05  IB-CODE                     PIC X(10).
           05  IB-CLASS                    PIC 9(05).
           05  IB-CENTRE2                  PIC X(04).
           05  IB-ELEM                     PIC X(04).
           05  IB-CYCLE                    PIC S9(11)V9(02) COMP-3.
           05  IB-BAND                     PIC X(08).
           05  IB-BAND2                    PIC S9(11)V9(05) COMP-3.
           05  IB-GROUP                    PIC X(04).
           05  IB-INVOICE                  PIC X(08).
           05  IB-SOURCE                   PIC S9(11)V9(02) COMP-3.
           05  IB-ACCOUNT                  PIC 9(09).
           05  IB-STATUS                   PIC 9(05).
           05  IB-BAN                      PIC X(16).
           05  IB-INVOICE2                 PIC S9(07)V9(02) COMP-3.
           05  BA-FILL-01                  PIC X(4).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-BA-VIEW1 REDEFINES CABS-BA-IN-RECORD.
           05  R0B-SOURCE                  PIC X(16).
           05  R0B-STATE                   PIC 9(04).
           05  R0B-CLASS                   PIC 9(05).
           05  R0B-GROUP                   PIC S9(13)V9(02) COMP-3.
           05  R0B-CENTRE                  PIC X(03).
           05  R0B-SEGMENT                 PIC X(16).
           05  R0B-REST                    PIC X(78).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-BA-VIEW2 REDEFINES CABS-BA-IN-RECORD.
           05  R1B-INVOICE                 PIC S9(11) COMP-3.
           05  R1B-CYCLE                   PIC S9(11) COMP-3.
           05  R1B-TARGET                  PIC 9(07).
           05  R1B-OCN                     PIC X(03).
           05  R1B-CIRCUIT                 PIC X(02).
           05  R1B-REGION                  PIC S9(09)V9(02) COMP-3.
           05  R1B-STATE                   PIC X(20).
           05  R1B-JURIS                   PIC S9(05) COMP-3.
           05  R1B-GROUP                   PIC 9(06).
           05  R1B-REST                    PIC X(71).
      * UPLOUT - WORK FILE, DELETED AT STEP END.
       FD  UPLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 120 CHARACTERS.
       01  CABS-BA-OUT-RECORD.
           05  OB-ACCOUNT                  PIC 9(07).
           05  OB-CLASS                    PIC X(13).
           05  OB-SEGMENT                  PIC 9(05).
           05  OB-TYPE                     PIC 9(03).
           05  OB-TARIFF                   PIC S9(11)V9(05) COMP-3.
           05  OB-TARIFF2                  PIC X(04).
           05  OB-BAN                      PIC S9(07)V9(02) COMP-3.
           05  OB-MEDIA                    PIC S9(09)V9(02) COMP-3.
           05  OB-CARRIER                  PIC X(20).
           05  OB-CENTRE                   PIC S9(07) COMP-3.
           05  OB-BAN2                     PIC S9(05) COMP-3.
           05  OB-CENTRE2                  PIC X(10).
           05  OB-ELEM                     PIC 9(09).
           05  OB-CYCLE                    PIC X(06).
           05  OB-TARGET                   PIC 9(04).
           05  OB-SEGMENT2                 PIC S9(09) COMP-3.
           05  BA-FILL-02                  PIC X(7).
      * SUSOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  SUSOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 300 CHARACTERS.
       01  CABS-SUSOUT-RECORD              PIC X(300).
      * CTLOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD              PIC X(180).
       WORKING-STORAGE SECTION.
      * STANDARD SHARED WORKING STORAGE. SEE CABSWRK.
       COPY CABSWRK.
      * SHARED LAYOUT PULLED IN FOR THE PACKED SIDE.
       COPY CABSCOMM.
      * SHARED LAYOUT PULLED IN FOR THE RECORD SIDE.
       COPY CABSCDR.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUCV18'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V1.23'.
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
       01  WS-PARM-CARD-R2 REDEFINES WS-PARM-CARD.
           05  PC2-LEAD                    PIC X(14).
           05  PC2-CYCLE-VIEW.
               10  PC2-CV-YY                   PIC 9(02).
               10  PC2-CV-DDD                  PIC 9(03).
           05  PC2-REST                    PIC X(61).
       01  WS-COUNT-AREA.
           05  WS-BA-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BA-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BA-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BA-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BA-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BA-CNT-06                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BA-CNT-07                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BA-CNT-08                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-BA-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BA-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BA-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BA-QTY-04                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BA-QTY-05                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-BA-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BA-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BA-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BA-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-BA-TXT-01                PIC X(16) VALUE SPACES.
           05  WS-BA-TXT-02                PIC X(26) VALUE SPACES.
           05  WS-BA-TXT-03                PIC X(30) VALUE SPACES.
           05  WS-BA-TXT-04                PIC X(26) VALUE SPACES.
           05  WS-BA-TXT-05                PIC X(30) VALUE SPACES.
           05  WS-BA-TXT-06                PIC X(20) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-BA-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-BA-ON-01                 VALUE 'Y'.
               88  WS-BA-OFF-01                VALUE 'N'.
           05  WS-BA-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-BA-ON-02                 VALUE 'Y'.
               88  WS-BA-OFF-02                VALUE 'N'.
           05  WS-BA-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-BA-ON-03                 VALUE 'Y'.
               88  WS-BA-OFF-03                VALUE 'N'.
           05  WS-BA-SW-04                 PIC X(01) VALUE 'N'.
               88  WS-BA-ON-04                 VALUE 'Y'.
               88  WS-BA-OFF-04                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-BA-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-BA-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-BA-SUB-03                PIC S9(04) COMP-3 VALUE 0.
           05  WS-BA-SUB-04                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. THE LIMIT WAS RAISED WHEN THE SOUTHEAST CENTRES
      * WERE MERGED AND HAS NOT BEEN REVIEWED SINCE.
       01  WS-BA-TABLE.
           05  WS-BA-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-BA-TB-ENTRY OCCURS 250 TIMES
                                       INDEXED BY WS-BA-IX.
               10  WS-BA-TB-KEY                PIC X(04).
               10  WS-BA-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-BA-TB-TXT                PIC X(20).
               10  WS-BA-TB-EFF                PIC 9(05).
               10  WS-BA-TB-EXP                PIC 9(05).
       01  WS-BA-WORK-GROUP-1.
           05  WS-BA-G1-TYPE               PIC S9(11)V9(02) COMP-3.
           05  WS-BA-G1-CODE               PIC X(20).
           05  WS-BA-G1-SEQ                PIC 9(07).
       01  WS-BA-WORK-GROUP-2.
           05  WS-BA-G2-TARIFF             PIC 9(05).
           05  WS-BA-G2-PERIOD             PIC 9(07).
           05  WS-BA-G2-TYPE               PIC X(20).
           05  WS-BA-G2-SEGMENT            PIC S9(11)V9(02) COMP-3.
           05  WS-BA-G2-CARRIER            PIC 9(07).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 58.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUCV18 - INTERCHANGE FORMAT CONVERSION'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-BA-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-BA-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9912.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-BA-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-BA-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT OLDIN.
           IF WS-FS-INPUT NOT = '00'
               DISPLAY 'OLDIN FILE STATUS = ' WS-FS-INPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OLDIN COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT UPLOUT.
           IF WS-FS-OUTPUT NOT = '00'
               DISPLAY 'UPLOUT FILE STATUS = ' WS-FS-OUTPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'UPLOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SUSOUT.
           IF WS-FS-SUSPENSE NOT = '00'
               DISPLAY 'SUSOUT FILE STATUS = ' WS-FS-SUSPENSE
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'SUSOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               DISPLAY 'CTLOUT FILE STATUS = ' WS-FS-CONTROL
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CTLOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-BA-CYCLE-YYDDD.
           COMPUTE WS-BA-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-BA-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-BA-CNT-05.
           MOVE 0 TO WS-BA-CNT-04.
           MOVE 0 TO WS-BA-CNT-06.
       P1200-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-BA-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-BA-TAB-CNT NOT < 250
               MOVE 'Y' TO WS-BA-SW-01
               ADD 1 TO WS-BA-CNT-05
           ELSE
               ADD 1 TO WS-BA-TAB-CNT
               SET WS-BA-IX TO WS-BA-TAB-CNT
               MOVE IB-TARIFF TO WS-BA-TB-KEY (WS-BA-IX)
               MOVE 0 TO WS-BA-TB-VAL (WS-BA-IX)
               MOVE SPACES TO WS-BA-TB-TXT (WS-BA-IX)
               ADD 1 TO WS-BA-CNT-03.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ OLDIN
               AT END MOVE 'Y' TO WS-BA-SW-01.
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
           PERFORM P2200-EDIT-RECORD THRU P2200-EDIT-RECORD-EXIT.
           PERFORM P2300-CONVERT-PACKED THRU P2300-CONVERT-PACKED-EXIT.
           IF WS-BA-ON-04
               PERFORM P2400-EDIT-ZONE THRU P2400-EDIT-ZONE-EXIT.
           IF WS-BA-ON-03
               PERFORM P2500-EDIT-CENTURY THRU P2500-EDIT-CENTURY-EXIT.
           IF WS-BA-ON-02
               PERFORM P2600-RESOLVE-RECORD THRU
                   P2600-RESOLVE-RECORD-EXIT.
           PERFORM P2700-SPLIT-LAYOUT THRU P2700-SPLIT-LAYOUT-EXIT.
           PERFORM P2800-SPLIT-CENTURY THRU P2800-SPLIT-CENTURY-EXIT.
           PERFORM P2900-RESOLVE-FIELD THRU P2900-RESOLVE-FIELD-EXIT.
           IF WS-BA-ON-04
               PERFORM P21000-SPLIT-LAYOUT THRU
                   P21000-SPLIT-LAYOUT-EXIT.
           IF WS-BA-ON-04
               PERFORM P21100-SELECT-ZONE THRU P21100-SELECT-ZONE-EXIT.
           PERFORM P21200-EDIT-RECORD THRU P21200-EDIT-RECORD-EXIT.
           PERFORM P21300-EDIT-LAYOUT THRU P21300-EDIT-LAYOUT-EXIT.
           PERFORM P21400-APPLY-FIELD THRU P21400-APPLY-FIELD-EXIT.
           PERFORM P21500-VALIDATE-FIELD THRU
               P21500-VALIDATE-FIELD-EXIT.
           IF WS-BA-ON-03
               PERFORM P21600-MATCH-RECORD THRU
                   P21600-MATCH-RECORD-EXIT.
           PERFORM P21700-MATCH-CENTURY THRU P21700-MATCH-CENTURY-EXIT.
           IF WS-BA-ON-04
               PERFORM P21800-VALIDATE-FIELD THRU
                   P21800-VALIDATE-FIELD-EXIT.
           IF WS-BA-ON-02
               PERFORM P21900-CHECK-FIELD THRU P21900-CHECK-FIELD-EXIT.
           PERFORM P22000-CHECK-PACKED THRU P22000-CHECK-PACKED-EXIT.
           PERFORM P22100-APPLY-CENTURY THRU P22100-APPLY-CENTURY-EXIT.
           PERFORM P22200-APPLY-ZONE THRU P22200-APPLY-ZONE-EXIT.
           PERFORM P22300-CONVERT-CENTURY THRU
               P22300-CONVERT-CENTURY-EXIT.
           PERFORM P22400-MATCH-RECORD THRU P22400-MATCH-RECORD-EXIT.
           IF WS-BA-ON-03
               PERFORM P22500-APPLY-PACKED THRU
                   P22500-APPLY-PACKED-EXIT.
           PERFORM P22600-DERIVE-PACKED THRU P22600-DERIVE-PACKED-EXIT.
           PERFORM P22700-MATCH-ZONE THRU P22700-MATCH-ZONE-EXIT.
           PERFORM P22800-DERIVE-SIGN THRU P22800-DERIVE-SIGN-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ OLDIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P2200-EDIT-RECORD.
           IF IB-CYCLE = 'B'
               ADD 1 TO WS-BA-CNT-01
           ELSE
               IF IB-CYCLE = 'A'
                   ADD 1 TO WS-BA-CNT-05
               ELSE
                   IF IB-CYCLE = 'A'
                       ADD 1 TO WS-BA-CNT-04
                   ELSE
                       ADD 1 TO WS-BA-CNT-03.
           MOVE 'N' TO WS-BA-SW-01.
           IF WS-BA-TAB-CNT > 0
               PERFORM P270-COMPARE-FIELD THRU P270-COMPARE-FIELD-EXIT
               VARYING WS-BA-SUB-04 FROM 1 BY 1
               UNTIL WS-BA-SUB-04 > WS-BA-TAB-CNT
               OR WS-BA-SW-01 = 'Y'.
       P2200-EDIT-RECORD-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P2300-CONVERT-PACKED.
           MOVE 0 TO WS-BA-QTY-04.
           MOVE 0 TO WS-BA-QTY-03.
           MOVE 0 TO WS-BA-AMT-01.
       P2300-CONVERT-PACKED-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P2400-EDIT-ZONE.
           IF WS-BA-AMT-03 < 6
               MOVE 6 TO WS-BA-AMT-03
               ADD 1 TO WS-BA-CNT-08.
           IF WS-BA-AMT-03 > 83838
               MOVE 83838 TO WS-BA-AMT-03
               ADD 1 TO WS-BA-CNT-02.
       P2400-EDIT-ZONE-EXIT.
           EXIT.
       P2500-EDIT-CENTURY.
           IF WS-BA-AMT-04 NOT = 0
               COMPUTE WS-BA-QTY-02 = WS-BA-AMT-02 * 100 / WS-BA-AMT-04
           ELSE
               MOVE 0 TO WS-BA-QTY-02.
       P2500-EDIT-CENTURY-EXIT.
           EXIT.
       P2600-RESOLVE-RECORD.
           ADD IB-SOURCE TO WS-BA-QTY-01.
           COMPUTE WS-BA-AMT-02 = WS-BA-QTY-01 * WS-BA-QTY-01.
           ADD WS-BA-AMT-02 TO WS-BA-AMT-01.
       P2600-RESOLVE-RECORD-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P2700-SPLIT-LAYOUT.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-OCN-UNKNOWN TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-BA-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
       P2700-SPLIT-LAYOUT-EXIT.
           EXIT.
       P2800-SPLIT-CENTURY.
           MOVE SPACES TO WS-BA-TXT-01.
           STRING IB-MEDIA DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IB-BAND2 DELIMITED BY SIZE
               INTO WS-BA-TXT-01.
       P2800-SPLIT-CENTURY-EXIT.
           EXIT.
       P2900-RESOLVE-FIELD.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-BA-TXT-03 TO PC-COL-001-020.
           MOVE WS-BA-TXT-02 TO PC-COL-021-060.
           MOVE WS-BA-AMT-02 TO WS-BA-AMT-EDIT.
           MOVE WS-BA-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P2900-RESOLVE-FIELD-EXIT.
           EXIT.
       P21000-SPLIT-LAYOUT.
           MOVE 'N' TO WS-BA-SW-03.
           IF WS-BA-TXT-03 NOT = WS-BA-TXT-02
               MOVE 'Y' TO WS-BA-SW-03
               MOVE WS-BA-TXT-03 TO WS-BA-TXT-02
               ADD 1 TO WS-BA-CNT-04.
       P21000-SPLIT-LAYOUT-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P21100-SELECT-ZONE.
           CALL 'CABSEQCK' USING WS-BA-TXT-05 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-BA-CNT-06.
       P21100-SELECT-ZONE-EXIT.
           EXIT.
       P21200-EDIT-RECORD.
           MOVE 'Y' TO WS-BA-SW-02.
           IF IB-CARRIER < 11
               MOVE 'N' TO WS-BA-SW-02
               ADD 1 TO WS-BA-CNT-03.
           IF IB-CARRIER > 6147
               MOVE 'N' TO WS-BA-SW-02
               ADD 1 TO WS-BA-CNT-04.
       P21200-EDIT-RECORD-EXIT.
           EXIT.
       P21300-EDIT-LAYOUT.
           MOVE 0 TO WS-BA-CNT-02.
           INSPECT WS-BA-TXT-03 TALLYING WS-BA-CNT-02
               FOR ALL SPACES.
           INSPECT WS-BA-TXT-03 REPLACING ALL LOW-VALUES BY SPACES.
       P21300-EDIT-LAYOUT-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P21400-APPLY-FIELD.
           UNSTRING WS-BA-TXT-05 DELIMITED BY '/'
               INTO WS-BA-TXT-04
               WS-BA-TXT-04
               TALLYING IN WS-BA-CNT-06.
       P21400-APPLY-FIELD-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P21500-VALIDATE-FIELD.
           CALL 'CABHASH' USING IB-BAND WS-ACC-OCN-HASH.
           ADD WS-BA-CNT-01 TO WS-ACC-SEQ-HASH.
       P21500-VALIDATE-FIELD-EXIT.
           EXIT.
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT
      * PRECEDES THIS PROGRAM IN THE JOB.
       P21600-MATCH-RECORD.
           MOVE WS-BA-AMT-01 TO WS-BA-AMT-01.
           IF WS-BA-AMT-01 < 0
               COMPUTE WS-BA-AMT-01 = 0 - WS-BA-AMT-01.
       P21600-MATCH-RECORD-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P21700-MATCH-CENTURY.
           MOVE IB-STATUS TO WS-BA-TXT-01.
           MOVE IB-SOURCE TO WS-BA-TXT-03.
           MOVE IB-BAND TO WS-BA-TXT-06.
           ADD 1 TO WS-BA-CNT-02.
       P21700-MATCH-CENTURY-EXIT.
           EXIT.
       P21800-VALIDATE-FIELD.
           MOVE SPACES TO CABS-BA-OUT-RECORD.
           MOVE IB-BAND2 TO OB-ACCOUNT.
           MOVE IB-BAND2 TO OB-CLASS.
           MOVE IB-TARIFF TO OB-SEGMENT.
           MOVE IB-SEGMENT TO OB-TYPE.
           WRITE CABS-BA-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P21800-VALIDATE-FIELD-EXIT.
           EXIT.
       P21900-CHECK-FIELD.
           IF IB-BAND = 'A'
               ADD 1 TO WS-BA-CNT-04
           ELSE
               IF IB-BAND = 'S'
                   ADD 1 TO WS-BA-CNT-03
               ELSE
                   IF IB-BAND = 'X'
                       ADD 1 TO WS-BA-CNT-08
                   ELSE
                       ADD 1 TO WS-BA-CNT-01.
       P21900-CHECK-FIELD-EXIT.
           EXIT.
       P22000-CHECK-PACKED.
           MOVE 0 TO WS-BA-QTY-01.
           MOVE 0 TO WS-BA-QTY-03.
           MOVE 0 TO WS-BA-AMT-03.
           MOVE 0 TO WS-BA-AMT-01.
       P22000-CHECK-PACKED-EXIT.
           EXIT.
       P22100-APPLY-CENTURY.
           IF WS-BA-AMT-02 < 29
               MOVE 29 TO WS-BA-AMT-02
               ADD 1 TO WS-BA-CNT-03.
           IF WS-BA-AMT-02 > 17295
               MOVE 17295 TO WS-BA-AMT-02
               ADD 1 TO WS-BA-CNT-02.
       P22100-APPLY-CENTURY-EXIT.
           EXIT.
       P22200-APPLY-ZONE.
           IF WS-BA-AMT-03 NOT = 0
               COMPUTE WS-BA-QTY-01 = WS-BA-AMT-01 * 100 / WS-BA-AMT-03
           ELSE
               MOVE 0 TO WS-BA-QTY-01.
       P22200-APPLY-ZONE-EXIT.
           EXIT.
       P22300-CONVERT-CENTURY.
           ADD IB-BAND2 TO WS-BA-QTY-01.
           COMPUTE WS-BA-AMT-04 ROUNDED = WS-BA-QTY-01 * WS-BA-QTY-04.
           ADD WS-BA-AMT-04 TO WS-BA-AMT-03.
       P22300-CONVERT-CENTURY-EXIT.
           EXIT.
       P22400-MATCH-RECORD.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-BAN-UNKNOWN TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-BA-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
       P22400-MATCH-RECORD-EXIT.
           EXIT.
       P22500-APPLY-PACKED.
           MOVE SPACES TO WS-BA-TXT-03.
           STRING IB-CODE DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IB-CENTRE2 DELIMITED BY SIZE
               INTO WS-BA-TXT-03.
       P22500-APPLY-PACKED-EXIT.
           EXIT.
       P22600-DERIVE-PACKED.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-BA-TXT-03 TO PC-COL-001-020.
           MOVE WS-BA-TXT-01 TO PC-COL-021-060.
           MOVE WS-BA-AMT-01 TO WS-BA-AMT-EDIT.
           MOVE WS-BA-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P22600-DERIVE-PACKED-EXIT.
           EXIT.
      * A FAILURE HERE SUSPENDS THE RECORD, IT DOES NOT STOP THE STEP.
       P22700-MATCH-ZONE.
           MOVE 'N' TO WS-BA-SW-04.
           IF WS-BA-TXT-02 NOT = WS-BA-TXT-01
               MOVE 'Y' TO WS-BA-SW-04
               MOVE WS-BA-TXT-02 TO WS-BA-TXT-01
               ADD 1 TO WS-BA-CNT-02.
       P22700-MATCH-ZONE-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P22800-DERIVE-SIGN.
           CALL 'CABPARMR' USING WS-BA-TXT-05 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-BA-CNT-08.
       P22800-DERIVE-SIGN-EXIT.
           EXIT.
       P270-COMPARE-FIELD.
           SET WS-BA-IX TO WS-BA-SUB-02.
           IF WS-BA-TB-KEY (WS-BA-IX) = IB-BAN
               MOVE 'Y' TO WS-BA-SW-02
               MOVE WS-BA-TB-VAL (WS-BA-IX) TO WS-BA-QTY-03
               MOVE WS-BA-SUB-02 TO WS-BA-SUB-03.
       P270-COMPARE-FIELD-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-EMIT-ZONE.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-OCN-UNKNOWN TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-BA-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
           MOVE CABS-SUSPENSE-RECORD TO CABS-SUSOUT-RECORD.
           WRITE CABS-SUSOUT-RECORD.
       P3100-EMIT-ZONE-EXIT.
           EXIT.
       P3200-POST-SIGN.
           MOVE IB-BAND2 TO WS-BA-TXT-05.
           MOVE IB-GROUP TO WS-BA-TXT-01.
           MOVE IB-CODE TO WS-BA-TXT-04.
           ADD 1 TO WS-BA-CNT-04.
       P3200-POST-SIGN-EXIT.
           EXIT.
       P3300-POST-LAYOUT.
           MOVE SPACES TO WS-BA-TXT-04.
           STRING IB-BAN DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IB-MEDIA DELIMITED BY SIZE
               INTO WS-BA-TXT-04.
       P3300-POST-LAYOUT-EXIT.
           EXIT.
       P3400-STAGE-ZONE.
           MOVE IB-INVOICE2 TO WS-BA-TXT-01.
           MOVE IB-CODE TO WS-BA-TXT-02.
           MOVE IB-CENTRE TO WS-BA-TXT-03.
           ADD 1 TO WS-BA-CNT-03.
       P3400-STAGE-ZONE-EXIT.
           EXIT.
      * S400-SECONDARY-PROCESSING SECTION
       S400-SECONDARY-PROCESSING SECTION.
       P4000-SECONDARY.
           PERFORM P4100-REPORT-CODE THRU P4100-REPORT-CODE-EXIT.
           PERFORM P4200-REPORT-STATUS THRU P4200-REPORT-STATUS-EXIT.
       P4000-EXIT.
           EXIT.
       P4100-REPORT-CODE.
           MOVE SPACES TO WS-BA-TXT-04.
           STRING IB-TARIFF DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IB-TARIFF DELIMITED BY SIZE
               INTO WS-BA-TXT-04.
       P4100-REPORT-CODE-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P4200-REPORT-STATUS.
           IF IB-STATUS = 'D'
               ADD 1 TO WS-BA-CNT-01
           ELSE
               IF IB-STATUS = 'B'
                   ADD 1 TO WS-BA-CNT-05
               ELSE
                   IF IB-STATUS = 'B'
                       ADD 1 TO WS-BA-CNT-02
                   ELSE
                       ADD 1 TO WS-BA-CNT-04.
       P4200-REPORT-STATUS-EXIT.
           EXIT.
           MOVE 0 TO WS-BA-QTY-04.
           PERFORM P360-WALK-RECORD THRU P360-WALK-RECORD-EXIT
               VARYING WS-BA-SUB-04 FROM 1 BY 1
               UNTIL WS-BA-SUB-04 > WS-BA-TAB-CNT.
       P360-WALK-RECORD.
           SET WS-BA-IX TO WS-BA-SUB-04.
           IF WS-BA-TB-KEY (WS-BA-IX) NOT = SPACES
               ADD WS-BA-TB-VAL (WS-BA-IX) TO WS-BA-QTY-01.
       P360-WALK-RECORD-EXIT.
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
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE SPACES TO CT-FILLER.
           MOVE WS-BA-TXT-01 TO CT-RESTART-KEY.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE 7 TO CT-STEP-SEQ.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE WS-BA-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
       P8100-EXIT.
           EXIT.
      * P8200-CHECK-BALANCE - EVERY RECORD READ IS EITHER WRITTEN,
      * REJECTED, SUMMARISED OR CARRIED FORWARD.
       P8200-CHECK-BALANCE.
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
           CLOSE OLDIN.
           CLOSE UPLOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABUCV18 - END OF RUN'.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  BA-CNT-06 = ' WS-BA-CNT-06.
           DISPLAY '  BA-CNT-02 = ' WS-BA-CNT-02.
           DISPLAY '  BA-CNT-03 = ' WS-BA-CNT-03.
       P9000-EXIT.
           EXIT.
