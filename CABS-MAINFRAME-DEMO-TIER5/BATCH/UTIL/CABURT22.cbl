      *****************************************************************
      * CABURT22 - JURISDICTION TABLE MAINTENANCE                     *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               ELMIN   TELCABS.CABS.ELMIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               BNDOUT  TELCABS.CABS.BNDOUT         (LOCAL)     *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * REVISION HISTORY                                              *
      *   V1.00  1998-09-22  P.NAIR       INITIAL RELEASE             *
      *   V1.04  1999-10-05  W.J.MCALLISTER REGION SIZE REDUCED -     *
      *                      TABLE MOVED OUT OF WORKING STORAGE       *
      *   V1.08  2007-06-10  S.MARCHETTI  BLOCK SIZE SET TO ZERO -    *
      *                      SYSTEM DETERMINED                        *
      *   V1.09  2008-12-03  P.NAIR       SECOND OUTPUT FILE ADDED FOR*
      *                      THE FACTOR STUDY                         *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABURT22.
       AUTHOR. TELCABS APPLICATIONS - RATE TABLE MAINTENANCE.
      *****************************************************************
      * JURISDICTION TABLE MAINTENANCE. THE STEP IS DRIVEN ENTIRELY   *
      * FROM THE SYSIN PARM CARD AND THE DD ALLOCATIONS IN THE JOB.   *
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
           SELECT ELMIN ASSIGN TO UT-S-ELMIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT BNDOUT ASSIGN TO UT-S-BNDOUT
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
      * ELMIN - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  ELMIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 90 CHARACTERS.
       01  CABS-AM-IN-RECORD.
           05  IA-SOURCE                   PIC X(03).
           05  IA-SEGMENT                  PIC X(06).
           05  IA-GROUP                    PIC S9(07)V9(05) COMP-3.
           05  IA-GROUP2                   PIC X(02).
           05  IA-CLASS                    PIC 9(05).
           05  IA-BAN                      PIC X(10).
           05  IA-STATUS                   PIC X(06).
           05  IA-CODE                     PIC X(20).
           05  IA-OCN                      PIC S9(13)V9(05) COMP-3.
           05  IA-SEQ                      PIC S9(13)V9(02) COMP-3.
           05  IA-PERIOD                   PIC X(06).
           05  IA-ACCOUNT                  PIC X(06).
           05  AM-FILL-01                  PIC X(1).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-AM-VIEW1 REDEFINES CABS-AM-IN-RECORD.
           05  R0A-ELEM                    PIC X(04).
           05  R0A-SEQ                     PIC S9(09) COMP-3.
           05  R0A-CODE                    PIC 9(07).
           05  R0A-SEQ2                    PIC S9(09)V9(02) COMP-3.
           05  R0A-JURIS                   PIC X(20).
           05  R0A-INVOICE                 PIC S9(11)V9(02) COMP-3.
           05  R0A-INVOICE2                PIC S9(09) COMP-3.
           05  R0A-BAND                    PIC X(10).
           05  R0A-REST                    PIC X(26).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-AM-VIEW2 REDEFINES CABS-AM-IN-RECORD.
           05  R1A-CIRCUIT                 PIC X(08).
           05  R1A-BAN                     PIC X(10).
           05  R1A-SOURCE                  PIC X(16).
           05  R1A-OCN                     PIC S9(07)V9(02) COMP-3.
           05  R1A-BAND                    PIC X(06).
           05  R1A-REST                    PIC X(45).
      * BNDOUT - CATALOGUED GENERATION DATA GROUP.
       FD  BNDOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-AM-OUT-RECORD.
           05  OA-CENTRE                   PIC S9(15) COMP-3.
           05  OA-CENTRE2                  PIC S9(09) COMP-3.
           05  OA-ACCOUNT                  PIC 9(06).
           05  OA-OCN                      PIC S9(07)V9(05) COMP-3.
           05  OA-STATUS                   PIC X(02).
           05  OA-INVOICE                  PIC X(02).
           05  OA-PERIOD                   PIC X(03).
           05  OA-MEDIA                    PIC S9(07)V9(02) COMP-3.
           05  OA-CENTRE3                  PIC S9(07)V9(02) COMP-3.
           05  OA-BAND                     PIC X(04).
           05  AM-FILL-02                  PIC X(33).
      * SUSOUT - PERMANENT DATASET HELD ON DASD.
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
      * SHARED LAYOUT PULLED IN FOR THE KEY SIDE.
       COPY CABSRT01.
      * SHARED LAYOUT PULLED IN FOR THE ELEMENT SIDE.
       COPY CABSCOMM.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABURT22'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V2.14'.
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
       01  WS-PARM-CARD-R2 REDEFINES WS-PARM-CARD.
           05  PC2-LEAD                    PIC X(14).
           05  PC2-CYCLE-VIEW.
               10  PC2-CV-YY                   PIC 9(02).
               10  PC2-CV-DDD                  PIC 9(03).
           05  PC2-REST                    PIC X(61).
       01  WS-COUNT-AREA.
           05  WS-AM-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AM-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AM-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AM-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AM-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AM-CNT-06                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AM-CNT-07                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AM-CNT-08                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-AM-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AM-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AM-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AM-QTY-04                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AM-QTY-05                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AM-QTY-06                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-AM-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AM-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AM-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-AM-TXT-01                PIC X(26) VALUE SPACES.
           05  WS-AM-TXT-02                PIC X(16) VALUE SPACES.
           05  WS-AM-TXT-03                PIC X(20) VALUE SPACES.
           05  WS-AM-TXT-04                PIC X(10) VALUE SPACES.
           05  WS-AM-TXT-05                PIC X(08) VALUE SPACES.
           05  WS-AM-TXT-06                PIC X(16) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-AM-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-AM-ON-01                 VALUE 'Y'.
               88  WS-AM-OFF-01                VALUE 'N'.
           05  WS-AM-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-AM-ON-02                 VALUE 'Y'.
               88  WS-AM-OFF-02                VALUE 'N'.
           05  WS-AM-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-AM-ON-03                 VALUE 'Y'.
               88  WS-AM-OFF-03                VALUE 'N'.
           05  WS-AM-SW-04                 PIC X(01) VALUE 'N'.
               88  WS-AM-ON-04                 VALUE 'Y'.
               88  WS-AM-OFF-04                VALUE 'N'.
           05  WS-AM-SW-05                 PIC X(01) VALUE 'N'.
               88  WS-AM-ON-05                 VALUE 'Y'.
               88  WS-AM-OFF-05                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-AM-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-AM-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-AM-SUB-03                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. A ROW THAT DOES NOT FIT IS COUNTED AND DROPPED
      * - THE STEP DOES NOT FAIL.
       01  WS-AM-TABLE.
           05  WS-AM-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-AM-TB-ENTRY OCCURS 300 TIMES
                                       INDEXED BY WS-AM-IX.
               10  WS-AM-TB-KEY                PIC X(04).
               10  WS-AM-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-AM-TB-TXT                PIC X(20).
               10  WS-AM-TB-EFF                PIC 9(05).
               10  WS-AM-TB-EXP                PIC 9(05).
       01  WS-AM-WORK-GROUP-1.
           05  WS-AM-G1-TARIFF             PIC S9(09) COMP-3.
           05  WS-AM-G1-BAN                PIC S9(09) COMP-3.
           05  WS-AM-G1-JURIS              PIC S9(11)V9(02) COMP-3.
           05  WS-AM-G1-STATE              PIC 9(05).
           05  WS-AM-G1-BAN                PIC X(20).
       01  WS-AM-WORK-GROUP-2.
           05  WS-AM-G2-SEQ                PIC 9(05).
           05  WS-AM-G2-SOURCE             PIC X(20).
           05  WS-AM-G2-ELEM               PIC S9(11)V9(02) COMP-3.
       01  WS-AM-WORK-GROUP-3.
           05  WS-AM-G3-OCN                PIC 9(05).
           05  WS-AM-G3-OCN                PIC 9(07).
           05  WS-AM-G3-TARIFF             PIC S9(11)V9(02) COMP-3.
           05  WS-AM-G3-GROUP              PIC S9(11)V9(02) COMP-3.
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 55.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABURT22 - JURISDICTION TABLE MAINTENANCE'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-AM-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-AM-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9923.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-AM-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-AM-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT ELMIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'ELMIN COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT BNDOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BNDOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SUSOUT.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'SUSOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
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
           MOVE PC1-CYCLE-YYDDD TO WS-AM-CYCLE-YYDDD.
           COMPUTE WS-AM-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-AM-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-AM-CNT-08.
           MOVE 0 TO WS-AM-CNT-03.
           MOVE 0 TO WS-AM-CNT-06.
       P1200-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-AM-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-AM-TAB-CNT NOT < 300
               MOVE 'Y' TO WS-AM-SW-01
               ADD 1 TO WS-AM-CNT-08
           ELSE
               ADD 1 TO WS-AM-TAB-CNT
               SET WS-AM-IX TO WS-AM-TAB-CNT
               MOVE IA-BAN TO WS-AM-TB-KEY (WS-AM-IX)
               MOVE 0 TO WS-AM-TB-VAL (WS-AM-IX)
               MOVE SPACES TO WS-AM-TB-TXT (WS-AM-IX)
               ADD 1 TO WS-AM-CNT-03.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ ELMIN
               AT END MOVE 'Y' TO WS-AM-SW-01.
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
           PERFORM P2200-SELECT-KEY THRU P2200-SELECT-KEY-EXIT.
           IF WS-AM-ON-04
               PERFORM P2300-RESOLVE-TARIFF THRU
                   P2300-RESOLVE-TARIFF-EXIT.
           PERFORM P2400-APPLY-DESCRIPTION THRU
               P2400-APPLY-DESCRIPTION-EXIT.
           IF WS-AM-ON-01
               PERFORM P2500-RESOLVE-TARIFF THRU
                   P2500-RESOLVE-TARIFF-EXIT.
           PERFORM P2600-APPLY-DESCRIPTION THRU
               P2600-APPLY-DESCRIPTION-EXIT.
           PERFORM P2700-CONVERT-DESCRIPTION THRU
               P2700-CONVERT-DESCRIPTION-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ ELMIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-SELECT-KEY.
           CALL 'CABEDITF' USING WS-AM-TXT-06 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-AM-CNT-01.
           MOVE 'N' TO WS-AM-SW-02.
           IF WS-AM-TAB-CNT > 0
               PERFORM P280-COMPARE-KEY THRU P280-COMPARE-KEY-EXIT
               VARYING WS-AM-SUB-02 FROM 1 BY 1
               UNTIL WS-AM-SUB-02 > WS-AM-TAB-CNT
               OR WS-AM-SW-02 = 'Y'.
       P2200-SELECT-KEY-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P2300-RESOLVE-TARIFF.
           IF WS-AM-AMT-02 < 43
               MOVE 43 TO WS-AM-AMT-02
               ADD 1 TO WS-AM-CNT-04.
           IF WS-AM-AMT-02 > 31624
               MOVE 31624 TO WS-AM-AMT-02
               ADD 1 TO WS-AM-CNT-05.
       P2300-RESOLVE-TARIFF-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P2400-APPLY-DESCRIPTION.
           MOVE IA-PERIOD TO WS-AM-TXT-03.
           MOVE IA-SEGMENT TO WS-AM-TXT-05.
           MOVE IA-CODE TO WS-AM-TXT-04.
           MOVE IA-CLASS TO WS-AM-TXT-04.
           ADD 1 TO WS-AM-CNT-07.
       P2400-APPLY-DESCRIPTION-EXIT.
           EXIT.
       P2500-RESOLVE-TARIFF.
           CALL 'CABHASH' USING IA-OCN WS-ACC-OCN-HASH.
           ADD WS-AM-CNT-04 TO WS-ACC-SEQ-HASH.
       P2500-RESOLVE-TARIFF-EXIT.
           EXIT.
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT
      * PRECEDES THIS PROGRAM IN THE JOB.
       P2600-APPLY-DESCRIPTION.
           MOVE 0 TO WS-AM-CNT-03.
           INSPECT WS-AM-TXT-06 TALLYING WS-AM-CNT-03
               FOR ALL SPACES.
           INSPECT WS-AM-TXT-06 REPLACING ALL LOW-VALUES BY SPACES.
       P2600-APPLY-DESCRIPTION-EXIT.
           EXIT.
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT
      * PRECEDES THIS PROGRAM IN THE JOB.
       P2700-CONVERT-DESCRIPTION.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-DATE-INVALID TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-AM-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
       P2700-CONVERT-DESCRIPTION-EXIT.
           EXIT.
       P280-COMPARE-KEY.
           SET WS-AM-IX TO WS-AM-SUB-01.
           IF WS-AM-TB-KEY (WS-AM-IX) = IA-CLASS
               MOVE 'Y' TO WS-AM-SW-01
               MOVE WS-AM-TB-VAL (WS-AM-IX) TO WS-AM-QTY-01
               MOVE WS-AM-SUB-01 TO WS-AM-SUB-02.
       P280-COMPARE-KEY-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P3100-EMIT-KEY.
           MOVE SPACES TO CABS-AM-OUT-RECORD.
           MOVE IA-GROUP2 TO OA-CENTRE.
           MOVE IA-BAN TO OA-CENTRE2.
           MOVE IA-CODE TO OA-ACCOUNT.
           MOVE IA-PERIOD TO OA-OCN.
           WRITE CABS-AM-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P3100-EMIT-KEY-EXIT.
           EXIT.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P3200-EMIT-WINDOW.
           MOVE IA-SEQ TO WS-AM-TXT-04.
           MOVE IA-SEQ TO WS-AM-TXT-06.
           MOVE IA-STATUS TO WS-AM-TXT-03.
           ADD 1 TO WS-AM-CNT-01.
       P3200-EMIT-WINDOW-EXIT.
           EXIT.
       P3300-RELEASE-KEY.
           MOVE SPACES TO WS-AM-TXT-01.
           STRING IA-ACCOUNT DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IA-SEGMENT DELIMITED BY SIZE
               INTO WS-AM-TXT-01.
       P3300-RELEASE-KEY-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P3400-FORMAT-KEY.
           ADD IA-SEQ TO WS-AM-QTY-06.
           COMPUTE WS-AM-AMT-02 = WS-AM-QTY-06 * WS-AM-QTY-04.
           ADD WS-AM-AMT-02 TO WS-AM-AMT-02.
       P3400-FORMAT-KEY-EXIT.
           EXIT.
      * S800-CONTROL SECTION - THE MANDATORY CABS CONTROL BOUNDARY.
       S800-CONTROL SECTION.
       P8000-CONTROL.
           PERFORM P8100-BUILD-CONTROL-REC THRU P8100-EXIT.
           PERFORM P8200-CHECK-BALANCE THRU P8200-EXIT.
           PERFORM P8300-WRITE-CONTROL-REC THRU P8300-EXIT.
       P8000-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE WS-AM-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE 6 TO CT-STEP-SEQ.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE WS-AM-TXT-05 TO CT-RESTART-KEY.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE SPACES TO CT-FILLER.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
       P8100-EXIT.
           EXIT.
      * P8200-CHECK-BALANCE - A RECORD THAT WAS HELD FOR THE NEXT
      * CYCLE IS BROUGHT INTO THE CARRIED FORWARD SIDE BEFORE THE
      * EQUATION IS TESTED.
       P8200-CHECK-BALANCE.
           ADD WS-AM-CNT-03 TO CT-CARRIED-FWD.
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
           CLOSE ELMIN.
           CLOSE BNDOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABURT22 - STEP COMPLETE'.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  AM-CNT-05 = ' WS-AM-CNT-05.
           DISPLAY '  AM-CNT-04 = ' WS-AM-CNT-04.
           DISPLAY '  AM-CNT-01 = ' WS-AM-CNT-01.
           DISPLAY '  AM-CNT-07 = ' WS-AM-CNT-07.
       P9000-EXIT.
           EXIT.
