      *****************************************************************
      * CABUCV09 - FIXED TO VARIABLE RECORD CONVERSION                *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               LEGIN   TELCABS.CABS.LEGIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               UPLOUT  TELCABS.CABS.UPLOUT         (LOCAL)     *
      *               CNVOUT  TELCABS.CABS.CNVOUT         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * REVISION HISTORY                                              *
      *   V1.00  1992-11-25  T.YAMASHITA  INITIAL RELEASE             *
      *   V1.04  2002-08-05  W.J.MCALLISTER CONTROL RECORD ADDED PER  *
      *                      CABS-STD-002                             *
      *   V1.07  2006-10-12  D.OKONKWO    REGION SIZE REDUCED - TABLE *
      *                      MOVED OUT OF WORKING STORAGE             *
      *   V1.09  2008-06-07  J.M.CASTILLO HASH TOTAL ADDED TO THE     *
      *                      CONTROL RECORD                           *
      *   V1.10  2016-04-19  D.OKONKWO    REPORT PAGINATION CORRECTED *
      *   V1.14  2018-04-09  S.MARCHETTI  RETIRED THE SECOND SORT STEP*
      *                      - DONE IN PROGRAM                        *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUCV09.
       AUTHOR. TELCABS APPLICATIONS - FILE CONVERSION.
      *****************************************************************
      * FIXED TO VARIABLE RECORD CONVERSION. THIS STEP IS SCHEDULED   *
      * INSIDE THE NIGHTLY ACCESS BILLING STREAM AND HAS NO           *
      * INTERACTIVE ENTRY POINT.                                      *
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
           SELECT LEGIN ASSIGN TO UT-S-LEGIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT UPLOUT ASSIGN TO UT-S-UPLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT CNVOUT ASSIGN TO UT-S-CNVOUT
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
      * LEGIN - PERMANENT DATASET HELD ON DASD.
       FD  LEGIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 120 CHARACTERS.
       01  CABS-BG-IN-RECORD.
           05  IB-BAN                      PIC S9(09)V9(05) COMP-3.
           05  IB-TARGET                   PIC X(10).
           05  IB-GROUP                    PIC 9(04).
           05  IB-GROUP2                   PIC X(02).
           05  IB-ACCOUNT                  PIC S9(11) COMP-3.
           05  IB-CODE                     PIC X(08).
           05  IB-GROUP3                   PIC S9(13) COMP-3.
           05  IB-TARIFF                   PIC 9(07).
           05  IB-INVOICE                  PIC X(20).
           05  IB-SOURCE                   PIC S9(13)V9(05) COMP-3.
           05  IB-REGION                   PIC X(16).
           05  IB-SOURCE2                  PIC 9(06).
           05  IB-REGION2                  PIC 9(02).
           05  IB-SOURCE3                  PIC X(02).
           05  IB-CIRCUIT                  PIC S9(07) COMP-3.
           05  IB-TARIFF2                  PIC X(03).
           05  IB-CLASS                    PIC 9(04).
           05  BG-FILL-01                  PIC X(1).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-BG-VIEW1 REDEFINES CABS-BG-IN-RECORD.
           05  R0B-BAND                    PIC X(03).
           05  R0B-ELEM                    PIC X(20).
           05  R0B-STATUS                  PIC S9(07)V9(02) COMP-3.
           05  R0B-INVOICE                 PIC S9(07)V9(05) COMP-3.
           05  R0B-CIRCUIT                 PIC S9(11) COMP-3.
           05  R0B-REST                    PIC X(79).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-BG-VIEW2 REDEFINES CABS-BG-IN-RECORD.
           05  R1B-TARIFF                  PIC S9(11)V9(02) COMP-3.
           05  R1B-CENTRE                  PIC X(02).
           05  R1B-TYPE                    PIC X(03).
           05  R1B-JURIS                   PIC S9(11)V9(02) COMP-3.
           05  R1B-SOURCE                  PIC S9(05) COMP-3.
           05  R1B-GROUP                   PIC X(08).
           05  R1B-REST                    PIC X(90).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-BG-VIEW3 REDEFINES CABS-BG-IN-RECORD.
           05  R2B-CARRIER                 PIC 9(07).
           05  R2B-BAND                    PIC 9(02).
           05  R2B-CYCLE                   PIC X(06).
           05  R2B-STATE                   PIC X(06).
           05  R2B-CLASS                   PIC X(10).
           05  R2B-BAND2                   PIC 9(06).
           05  R2B-REST                    PIC X(83).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-BG-VIEW4 REDEFINES CABS-BG-IN-RECORD.
           05  R3B-REGION                  PIC X(06).
           05  R3B-CODE                    PIC S9(07) COMP-3.
           05  R3B-BAN                     PIC 9(04).
           05  R3B-BAN2                    PIC 9(03).
           05  R3B-REGION2                 PIC S9(09)V9(02) COMP-3.
           05  R3B-TYPE                    PIC X(10).
           05  R3B-REST                    PIC X(87).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-BG-VIEW5 REDEFINES CABS-BG-IN-RECORD.
           05  R4B-OCN                     PIC X(13).
           05  R4B-CARRIER                 PIC X(13).
           05  R4B-CLASS                   PIC S9(11)V9(02) COMP-3.
           05  R4B-PERIOD                  PIC S9(07)V9(05) COMP-3.
           05  R4B-CODE                    PIC X(02).
           05  R4B-REGION                  PIC X(04).
           05  R4B-TARGET                  PIC S9(09) COMP-3.
           05  R4B-SEQ                     PIC X(08).
           05  R4B-REST                    PIC X(61).
      * UPLOUT - WORK FILE, DELETED AT STEP END.
       FD  UPLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 100 CHARACTERS.
       01  CABS-BG-OUT-RECORD.
           05  OB-TARIFF                   PIC X(13).
           05  OB-REGION                   PIC 9(03).
           05  OB-GROUP                    PIC S9(07) COMP-3.
           05  OB-TARIFF2                  PIC S9(07) COMP-3.
           05  OB-SOURCE                   PIC S9(15) COMP-3.
           05  OB-TARGET                   PIC X(03).
           05  OB-TYPE                     PIC 9(04).
           05  OB-CODE                     PIC 9(09).
           05  OB-BAN                      PIC S9(09)V9(05) COMP-3.
           05  OB-CIRCUIT                  PIC S9(13)V9(05) COMP-3.
           05  OB-PERIOD                   PIC S9(09)V9(02) COMP-3.
           05  OB-CLASS                    PIC S9(07)V9(02) COMP-3.
           05  OB-TARIFF3                  PIC X(06).
           05  OB-SOURCE2                  PIC S9(07)V9(05) COMP-3.
           05  BG-FILL-02                  PIC X(10).
      * CNVOUT - CATALOGUED GENERATION DATA GROUP.
       FD  CNVOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 100 CHARACTERS.
       01  CABS-BG-OUT1-RECORD         PIC X(100).
      * CTLOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD              PIC X(180).
      * RPTOUT - CATALOGUED GENERATION DATA GROUP.
       FD  RPTOUT
           RECORDING MODE IS FA
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       COPY CABSPRNT.
       WORKING-STORAGE SECTION.
      * STANDARD SHARED WORKING STORAGE. SEE CABSWRK.
       COPY CABSWRK.
      * SHARED LAYOUT PULLED IN FOR THE ZONE SIDE.
       COPY CABSSETL.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUCV09'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V2.16'.
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
           05  WS-BG-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BG-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BG-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BG-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BG-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BG-CNT-06                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BG-CNT-07                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BG-CNT-08                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BG-CNT-09                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-BG-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BG-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BG-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-BG-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BG-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BG-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BG-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BG-AMT-05                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-BG-TXT-01                PIC X(12) VALUE SPACES.
           05  WS-BG-TXT-02                PIC X(10) VALUE SPACES.
           05  WS-BG-TXT-03                PIC X(08) VALUE SPACES.
           05  WS-BG-TXT-04                PIC X(12) VALUE SPACES.
           05  WS-BG-TXT-05                PIC X(30) VALUE SPACES.
           05  WS-BG-TXT-06                PIC X(20) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-BG-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-BG-ON-01                 VALUE 'Y'.
               88  WS-BG-OFF-01                VALUE 'N'.
           05  WS-BG-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-BG-ON-02                 VALUE 'Y'.
               88  WS-BG-OFF-02                VALUE 'N'.
           05  WS-BG-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-BG-ON-03                 VALUE 'Y'.
               88  WS-BG-OFF-03                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-BG-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-BG-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-BG-SUB-03                PIC S9(04) COMP-3 VALUE 0.
           05  WS-BG-SUB-04                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. A ROW THAT DOES NOT FIT IS COUNTED AND DROPPED
      * - THE STEP DOES NOT FAIL.
       01  WS-BG-TABLE.
           05  WS-BG-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-BG-TB-ENTRY OCCURS 250 TIMES
                                       INDEXED BY WS-BG-IX.
               10  WS-BG-TB-KEY                PIC X(04).
               10  WS-BG-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-BG-TB-TXT                PIC X(30).
               10  WS-BG-TB-EFF                PIC 9(05).
               10  WS-BG-TB-EXP                PIC 9(05).
       01  WS-BG-WORK-GROUP-1.
           05  WS-BG-G1-SOURCE             PIC S9(09) COMP-3.
           05  WS-BG-G1-JURIS              PIC X(20).
           05  WS-BG-G1-CYCLE              PIC S9(11)V9(02) COMP-3.
           05  WS-BG-G1-BAN                PIC 9(07).
           05  WS-BG-G1-CARRIER            PIC 9(05).
           05  WS-BG-G1-BAND               PIC X(20).
           05  WS-BG-G1-CYCLE              PIC X(20).
           05  WS-BG-G1-CARRIER            PIC X(10).
       01  WS-BG-WORK-GROUP-2.
           05  WS-BG-G2-ACCOUNT            PIC X(20).
           05  WS-BG-G2-BAN                PIC S9(11)V9(02) COMP-3.
           05  WS-BG-G2-CLASS              PIC S9(11)V9(02) COMP-3.
           05  WS-BG-G2-JURIS              PIC 9(05).
           05  WS-BG-G2-CYCLE              PIC S9(09) COMP-3.
           05  WS-BG-G2-CYCLE              PIC X(20).
           05  WS-BG-G2-CLASS              PIC 9(05).
           05  WS-BG-G2-SEGMENT            PIC 9(05).
       01  WS-BG-WORK-GROUP-3.
           05  WS-BG-G3-PERIOD             PIC 9(07).
           05  WS-BG-G3-SOURCE             PIC X(20).
           05  WS-BG-G3-CIRCUIT            PIC X(20).
           05  WS-BG-G3-CLASS              PIC X(20).
           05  WS-BG-G3-CYCLE              PIC 9(05).
           05  WS-BG-G3-CLASS              PIC 9(05).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 60.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUCV09 - FIXED TO VARIABLE RECORD CONVERSION'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-BG-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-BG-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9915.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-BG-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-BG-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT LEGIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'LEGIN COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT UPLOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'UPLOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CNVOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CNVOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CTLOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RPTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RPTOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-BG-CYCLE-YYDDD.
           COMPUTE WS-BG-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-BG-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-BG-CNT-09.
           MOVE 0 TO WS-BG-CNT-07.
           MOVE 0 TO WS-BG-CNT-05.
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
           IF WS-BG-ON-02
               PERFORM P2200-CHECK-CENTURY THRU
                   P2200-CHECK-CENTURY-EXIT.
           PERFORM P2300-DERIVE-CENTURY THRU P2300-DERIVE-CENTURY-EXIT.
           PERFORM P2400-SELECT-FIELD THRU P2400-SELECT-FIELD-EXIT.
           IF WS-BG-ON-02
               PERFORM P2500-CHECK-FIELD THRU P2500-CHECK-FIELD-EXIT.
           IF WS-BG-ON-02
               PERFORM P2600-MATCH-PACKED THRU P2600-MATCH-PACKED-EXIT.
           PERFORM P2700-SPLIT-PACKED THRU P2700-SPLIT-PACKED-EXIT.
           PERFORM P2800-APPLY-CENTURY THRU P2800-APPLY-CENTURY-EXIT.
           PERFORM P2900-CONVERT-RECORD THRU P2900-CONVERT-RECORD-EXIT.
           PERFORM P21000-RESOLVE-PACKED THRU
               P21000-RESOLVE-PACKED-EXIT.
           PERFORM P21100-SELECT-SIGN THRU P21100-SELECT-SIGN-EXIT.
           IF WS-BG-ON-02
               PERFORM P21200-CONVERT-FIELD THRU
                   P21200-CONVERT-FIELD-EXIT.
           PERFORM P21300-EXPAND-LAYOUT THRU P21300-EXPAND-LAYOUT-EXIT.
           PERFORM P21400-SELECT-LAYOUT THRU P21400-SELECT-LAYOUT-EXIT.
           IF WS-BG-ON-03
               PERFORM P21500-VALIDATE-RECORD THRU
                   P21500-VALIDATE-RECORD-EXIT.
           PERFORM P21600-APPLY-ZONE THRU P21600-APPLY-ZONE-EXIT.
           PERFORM P21700-CONVERT-PACKED THRU
               P21700-CONVERT-PACKED-EXIT.
           PERFORM P21800-DERIVE-LAYOUT THRU P21800-DERIVE-LAYOUT-EXIT.
           PERFORM P21900-VALIDATE-RECORD THRU
               P21900-VALIDATE-RECORD-EXIT.
           PERFORM P22000-EDIT-RECORD THRU P22000-EDIT-RECORD-EXIT.
           PERFORM P22100-CHECK-RECORD THRU P22100-CHECK-RECORD-EXIT.
           IF WS-BG-ON-01
               PERFORM P22200-VALIDATE-LAYOUT THRU
                   P22200-VALIDATE-LAYOUT-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ LEGIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-CHECK-CENTURY.
           MOVE SPACES TO WS-BG-TXT-05.
           STRING IB-GROUP DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IB-REGION DELIMITED BY SIZE
               INTO WS-BG-TXT-05.
       P2200-CHECK-CENTURY-EXIT.
           EXIT.
      * A FAILURE HERE SUSPENDS THE RECORD, IT DOES NOT STOP THE STEP.
       P2300-DERIVE-CENTURY.
           ADD IB-GROUP3 TO WS-BG-QTY-01.
           COMPUTE WS-BG-AMT-02 ROUNDED = WS-BG-QTY-01 * WS-BG-QTY-02.
           ADD WS-BG-AMT-02 TO WS-BG-AMT-03.
       P2300-DERIVE-CENTURY-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P2400-SELECT-FIELD.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-BAN-UNKNOWN TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-BG-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
       P2400-SELECT-FIELD-EXIT.
           EXIT.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P2500-CHECK-FIELD.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-BG-TXT-01 TO PC-COL-001-020.
           MOVE WS-BG-TXT-06 TO PC-COL-021-060.
           MOVE WS-BG-AMT-03 TO WS-BG-AMT-EDIT.
           MOVE WS-BG-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P2500-CHECK-FIELD-EXIT.
           EXIT.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P2600-MATCH-PACKED.
           IF WS-BG-AMT-02 NOT = 0
               COMPUTE WS-BG-QTY-03 = WS-BG-AMT-01 * 100 / WS-BG-AMT-02
           ELSE
               MOVE 0 TO WS-BG-QTY-03.
       P2600-MATCH-PACKED-EXIT.
           EXIT.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P2700-SPLIT-PACKED.
           MOVE 0 TO WS-BG-CNT-05.
           INSPECT WS-BG-TXT-02 TALLYING WS-BG-CNT-05
               FOR ALL SPACES.
           INSPECT WS-BG-TXT-02 REPLACING ALL LOW-VALUES BY SPACES.
       P2700-SPLIT-PACKED-EXIT.
           EXIT.
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT
      * PRECEDES THIS PROGRAM IN THE JOB.
       P2800-APPLY-CENTURY.
           MOVE 0 TO WS-BG-QTY-02.
           MOVE 0 TO WS-BG-QTY-03.
           MOVE 0 TO WS-BG-QTY-01.
           MOVE 0 TO WS-BG-AMT-05.
           MOVE 0 TO WS-BG-AMT-01.
       P2800-APPLY-CENTURY-EXIT.
           EXIT.
       P2900-CONVERT-RECORD.
           MOVE 'N' TO WS-BG-SW-01.
           IF WS-BG-TXT-01 NOT = WS-BG-TXT-03
               MOVE 'Y' TO WS-BG-SW-01
               MOVE WS-BG-TXT-01 TO WS-BG-TXT-03
               ADD 1 TO WS-BG-CNT-03.
       P2900-CONVERT-RECORD-EXIT.
           EXIT.
       P21000-RESOLVE-PACKED.
           CALL 'CABFMTR' USING WS-BG-TXT-03 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-BG-CNT-08.
       P21000-RESOLVE-PACKED-EXIT.
           EXIT.
       P21100-SELECT-SIGN.
           IF IB-REGION2 = 'X'
               ADD 1 TO WS-BG-CNT-04
           ELSE
               IF IB-REGION2 = 'X'
                   ADD 1 TO WS-BG-CNT-04
               ELSE
                   IF IB-REGION2 = 'D'
                       ADD 1 TO WS-BG-CNT-08
                   ELSE
                       ADD 1 TO WS-BG-CNT-03.
       P21100-SELECT-SIGN-EXIT.
           EXIT.
       P21200-CONVERT-FIELD.
           IF WS-BG-AMT-05 < 30
               MOVE 30 TO WS-BG-AMT-05
               ADD 1 TO WS-BG-CNT-02.
           IF WS-BG-AMT-05 > 16651
               MOVE 16651 TO WS-BG-AMT-05
               ADD 1 TO WS-BG-CNT-03.
       P21200-CONVERT-FIELD-EXIT.
           EXIT.
       P21300-EXPAND-LAYOUT.
           MOVE IB-SOURCE3 TO WS-BG-TXT-02.
           MOVE IB-CLASS TO WS-BG-TXT-03.
           MOVE IB-SOURCE2 TO WS-BG-TXT-03.
           MOVE IB-REGION TO WS-BG-TXT-01.
           ADD 1 TO WS-BG-CNT-08.
       P21300-EXPAND-LAYOUT-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P21400-SELECT-LAYOUT.
           CALL 'CABHASH' USING IB-REGION WS-ACC-OCN-HASH.
           ADD WS-BG-CNT-04 TO WS-ACC-SEQ-HASH.
       P21400-SELECT-LAYOUT-EXIT.
           EXIT.
       P21500-VALIDATE-RECORD.
           UNSTRING WS-BG-TXT-03 DELIMITED BY '/'
               INTO WS-BG-TXT-01
               WS-BG-TXT-03
               TALLYING IN WS-BG-CNT-08.
       P21500-VALIDATE-RECORD-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P21600-APPLY-ZONE.
           MOVE 'Y' TO WS-BG-SW-01.
           IF IB-SOURCE2 < 29
               MOVE 'N' TO WS-BG-SW-01
               ADD 1 TO WS-BG-CNT-03.
           IF IB-SOURCE2 > 5540
               MOVE 'N' TO WS-BG-SW-01
               ADD 1 TO WS-BG-CNT-08.
       P21600-APPLY-ZONE-EXIT.
           EXIT.
       P21700-CONVERT-PACKED.
           MOVE WS-BG-AMT-04 TO WS-BG-AMT-01.
           IF WS-BG-AMT-01 < 0
               COMPUTE WS-BG-AMT-01 = 0 - WS-BG-AMT-04.
       P21700-CONVERT-PACKED-EXIT.
           EXIT.
       P21800-DERIVE-LAYOUT.
           MOVE SPACES TO CABS-BG-OUT-RECORD.
           MOVE IB-BAN TO OB-TARIFF.
           MOVE IB-SOURCE2 TO OB-REGION.
           MOVE IB-TARIFF2 TO OB-GROUP.
           MOVE IB-GROUP3 TO OB-TARIFF2.
           MOVE IB-TARIFF TO OB-SOURCE.
           MOVE IB-ACCOUNT TO OB-TARGET.
           MOVE IB-TARIFF2 TO OB-TYPE.
           WRITE CABS-BG-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P21800-DERIVE-LAYOUT-EXIT.
           EXIT.
       P21900-VALIDATE-RECORD.
           MOVE SPACES TO WS-BG-TXT-05.
           STRING IB-ACCOUNT DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IB-TARIFF DELIMITED BY SIZE
               INTO WS-BG-TXT-05.
       P21900-VALIDATE-RECORD-EXIT.
           EXIT.
      * A FAILURE HERE SUSPENDS THE RECORD, IT DOES NOT STOP THE STEP.
       P22000-EDIT-RECORD.
           ADD IB-CIRCUIT TO WS-BG-QTY-02.
           COMPUTE WS-BG-AMT-05 ROUNDED = WS-BG-QTY-02 * WS-BG-QTY-01.
           ADD WS-BG-AMT-05 TO WS-BG-AMT-02.
       P22000-EDIT-RECORD-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P22100-CHECK-RECORD.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-DUP-SEQ TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-BG-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
       P22100-CHECK-RECORD-EXIT.
           EXIT.
       P22200-VALIDATE-LAYOUT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-BG-TXT-03 TO PC-COL-001-020.
           MOVE WS-BG-TXT-05 TO PC-COL-021-060.
           MOVE WS-BG-AMT-02 TO WS-BG-AMT-EDIT.
           MOVE WS-BG-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P22200-VALIDATE-LAYOUT-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-FORMAT-SIGN.
           MOVE IB-SOURCE3 TO WS-BG-TXT-05.
           MOVE IB-CLASS TO WS-BG-TXT-04.
           ADD 1 TO WS-BG-CNT-07.
       P3100-FORMAT-SIGN-EXIT.
           EXIT.
       P3200-STAGE-SIGN.
           MOVE SPACES TO CABS-BG-OUT-RECORD.
           MOVE IB-BAN TO OB-TARIFF.
           MOVE IB-TARGET TO OB-REGION.
           MOVE IB-REGION TO OB-GROUP.
           MOVE IB-CLASS TO OB-TARIFF2.
           MOVE IB-BAN TO OB-SOURCE.
           MOVE IB-REGION2 TO OB-TARGET.
           MOVE IB-BAN TO OB-TYPE.
           MOVE IB-SOURCE2 TO OB-CODE.
           WRITE CABS-BG-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P3200-STAGE-SIGN-EXIT.
           EXIT.
       P3300-EMIT-SIGN.
           MOVE 0 TO WS-BG-QTY-02.
           MOVE 0 TO WS-BG-QTY-03.
           MOVE 0 TO WS-BG-QTY-01.
           MOVE 0 TO WS-BG-AMT-03.
           MOVE 0 TO WS-BG-AMT-04.
       P3300-EMIT-SIGN-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P3400-STAGE-SIGN.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-BG-TXT-03 TO PC-COL-001-020.
           MOVE WS-BG-TXT-06 TO PC-COL-021-060.
           MOVE WS-BG-AMT-05 TO WS-BG-AMT-EDIT.
           MOVE WS-BG-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P3400-STAGE-SIGN-EXIT.
           EXIT.
      * S400-SECONDARY-PROCESSING SECTION
       S400-SECONDARY-PROCESSING SECTION.
       P4000-SECONDARY.
           PERFORM P4100-TRACE-ACCOUNT THRU P4100-TRACE-ACCOUNT-EXIT.
           PERFORM P4200-RECONCILE-TYPE THRU P4200-RECONCILE-TYPE-EXIT.
       P4000-EXIT.
           EXIT.
       P4100-TRACE-ACCOUNT.
           MOVE SPACES TO CABS-BG-OUT-RECORD.
           MOVE IB-GROUP3 TO OB-TARIFF.
           MOVE IB-SOURCE TO OB-REGION.
           MOVE IB-SOURCE TO OB-GROUP.
           MOVE IB-TARGET TO OB-TARIFF2.
           MOVE IB-TARIFF2 TO OB-SOURCE.
           MOVE IB-SOURCE TO OB-TARGET.
           MOVE IB-TARGET TO OB-TYPE.
           MOVE IB-ACCOUNT TO OB-CODE.
           WRITE CABS-BG-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P4100-TRACE-ACCOUNT-EXIT.
           EXIT.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P4200-RECONCILE-TYPE.
           CALL 'CABHASH' USING IB-CLASS WS-ACC-OCN-HASH.
           ADD WS-BG-CNT-02 TO WS-ACC-SEQ-HASH.
       P4200-RECONCILE-TYPE-EXIT.
           EXIT.
           MOVE 0 TO WS-BG-QTY-03.
           PERFORM P380-WALK-FIELD THRU P380-WALK-FIELD-EXIT
               VARYING WS-BG-SUB-03 FROM 1 BY 1
               UNTIL WS-BG-SUB-03 > WS-BG-TAB-CNT.
       P380-WALK-FIELD.
           SET WS-BG-IX TO WS-BG-SUB-04.
           IF WS-BG-TB-KEY (WS-BG-IX) NOT = SPACES
               ADD WS-BG-TB-VAL (WS-BG-IX) TO WS-BG-QTY-02.
       P380-WALK-FIELD-EXIT.
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
           MOVE 'DETAIL CARRIED FWD' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-BG-CNT-EDIT.
           MOVE WS-BG-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL IN' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO WS-BG-CNT-EDIT.
           MOVE WS-BG-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL SUSPENDED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-BG-CNT-EDIT.
           MOVE WS-BG-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL OUT' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-BG-CNT-EDIT.
           MOVE WS-BG-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL SUMMARISED' TO PC-COL-001-020.
           MOVE WS-SUMM-CNT TO WS-BG-CNT-EDIT.
           MOVE WS-BG-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-BG-CNT-01 TO WS-BG-CNT-EDIT.
           MOVE WS-BG-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 02' TO PC-COL-001-020.
           MOVE WS-BG-CNT-02 TO WS-BG-CNT-EDIT.
           MOVE WS-BG-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 03' TO PC-COL-001-020.
           MOVE WS-BG-CNT-03 TO WS-BG-CNT-EDIT.
           MOVE WS-BG-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE SPACES TO CT-FILLER.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE WS-BG-CNT-09 TO CT-RC.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE 1 TO CT-STEP-SEQ.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE WS-BG-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
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
           CLOSE LEGIN.
           CLOSE UPLOUT.
           CLOSE CNVOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABUCV09 - STEP COMPLETE'.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  BG-CNT-02 = ' WS-BG-CNT-02.
           DISPLAY '  BG-CNT-05 = ' WS-BG-CNT-05.
           DISPLAY '  BG-CNT-07 = ' WS-BG-CNT-07.
           DISPLAY '  BG-CNT-08 = ' WS-BG-CNT-08.
       P9000-EXIT.
           EXIT.
