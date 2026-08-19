      *****************************************************************
      * CABUXR15 - ORPHAN KEY CROSS REFERENCE                         *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               XRFIN   TELCABS.CABS.XRFIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               ORPOUT  TELCABS.CABS.ORPOUT         (LOCAL)     *
      *               LNKOUT  TELCABS.CABS.LNKOUT         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - THE OUTPUT IS REBUILT EACH RUN     *
      * REVISION HISTORY                                              *
      *   V1.00  1988-02-02  R.T.WHEELER  INITIAL RELEASE             *
      *   V1.01  1990-12-16  L.FERREIRA   EFFECTIVE DATE FILTER ADDED *
      *                      PER AUDIT FINDING                        *
      *   V1.02  2010-11-12  J.M.CASTILLO RETIRED THE SECOND SORT STEP*
      *                      - DONE IN PROGRAM                        *
      *   V1.03  2018-04-21  P.NAIR       REGION SIZE REDUCED - TABLE *
      *                      MOVED OUT OF WORKING STORAGE             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUXR15.
       AUTHOR. TELCABS APPLICATIONS - CROSS REFERENCE REPORT.
      *****************************************************************
      * ORPHAN KEY CROSS REFERENCE. THIS STEP IS SCHEDULED INSIDE THE *
      * NIGHTLY ACCESS BILLING STREAM AND HAS NO INTERACTIVE ENTRY    *
      * POINT.                                                        *
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE     *
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.                      *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           C01 IS TO-NEW-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT XRFIN ASSIGN TO UT-S-XRFIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT ORPOUT ASSIGN TO UT-S-ORPOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT LNKOUT ASSIGN TO UT-S-LNKOUT
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
           SELECT RPTOUT ASSIGN TO UT-S-RPTOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
       DATA DIVISION.
       FILE SECTION.
      * XRFIN - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  XRFIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 110 CHARACTERS.
       01  CABS-AJ-IN-RECORD.
           05  IA-STATUS                   PIC X(20).
           05  IA-GROUP                    PIC X(08).
           05  IA-TYPE                     PIC S9(09) COMP-3.
           05  IA-BAND                     PIC 9(03).
           05  IA-ACCOUNT                  PIC X(20).
           05  IA-BAND2                    PIC X(20).
           05  IA-CARRIER                  PIC 9(06).
           05  IA-CLASS                    PIC X(08).
           05  IA-LEVEL                    PIC S9(05) COMP-3.
           05  IA-CIRCUIT                  PIC S9(05) COMP-3.
           05  IA-SEQ                      PIC X(06).
           05  IA-LEVEL2                   PIC 9(03).
           05  IA-TARGET                   PIC X(02).
           05  AJ-FILL-01                  PIC X(3).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-AJ-VIEW1 REDEFINES CABS-AJ-IN-RECORD.
           05  R0A-BAN                     PIC X(08).
           05  R0A-TARGET                  PIC S9(15) COMP-3.
           05  R0A-LEVEL                   PIC S9(15) COMP-3.
           05  R0A-TYPE                    PIC 9(03).
           05  R0A-REST                    PIC X(83).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-AJ-VIEW2 REDEFINES CABS-AJ-IN-RECORD.
           05  R1A-CYCLE                   PIC 9(09).
           05  R1A-CENTRE                  PIC S9(09) COMP-3.
           05  R1A-GROUP                   PIC S9(09) COMP-3.
           05  R1A-INVOICE                 PIC 9(05).
           05  R1A-STATUS                  PIC S9(09)V9(02) COMP-3.
           05  R1A-LEVEL                   PIC S9(09)V9(05) COMP-3.
           05  R1A-TYPE                    PIC S9(13)V9(02) COMP-3.
           05  R1A-TARGET                  PIC X(16).
           05  R1A-REST                    PIC X(48).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-AJ-VIEW3 REDEFINES CABS-AJ-IN-RECORD.
           05  R2A-GROUP                   PIC 9(04).
           05  R2A-CIRCUIT                 PIC 9(02).
           05  R2A-BAND                    PIC X(06).
           05  R2A-MEDIA                   PIC X(20).
           05  R2A-ELEM                    PIC S9(07)V9(05) COMP-3.
           05  R2A-LEVEL                   PIC S9(09) COMP-3.
           05  R2A-SEGMENT                 PIC X(10).
           05  R2A-CODE                    PIC S9(13)V9(02) COMP-3.
           05  R2A-REST                    PIC X(48).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-AJ-VIEW4 REDEFINES CABS-AJ-IN-RECORD.
           05  R3A-STATE                   PIC S9(09) COMP-3.
           05  R3A-CYCLE                   PIC X(03).
           05  R3A-LEVEL                   PIC 9(05).
           05  R3A-GROUP                   PIC X(08).
           05  R3A-GROUP2                  PIC X(10).
           05  R3A-CIRCUIT                 PIC S9(09)V9(02) COMP-3.
           05  R3A-REST                    PIC X(73).
      * ORPOUT - WORK FILE, DELETED AT STEP END.
       FD  ORPOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-AJ-OUT-RECORD.
           05  OA-ACCOUNT                  PIC S9(07)V9(02) COMP-3.
           05  OA-STATUS                   PIC X(04).
           05  OA-BAN                      PIC S9(15) COMP-3.
           05  OA-ACCOUNT2                 PIC S9(07) COMP-3.
           05  OA-CYCLE                    PIC X(16).
           05  OA-CENTRE                   PIC S9(05) COMP-3.
           05  OA-GROUP                    PIC 9(07).
           05  OA-TARGET                   PIC 9(09).
           05  OA-INVOICE                  PIC S9(13)V9(02) COMP-3.
           05  OA-SEQ                      PIC X(04).
           05  AJ-FILL-02                  PIC X(12).
      * LNKOUT - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  LNKOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-AJ-OUT1-RECORD         PIC X(80).
      * SUSOUT - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  SUSOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 300 CHARACTERS.
       01  CABS-SUSOUT-RECORD              PIC X(300).
      * CTLOUT - WORK FILE, DELETED AT STEP END.
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
      * SHARED LAYOUT PULLED IN FOR THE ORPHAN SIDE.
       COPY CABSCIRC.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUXR15'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V2.13'.
           05  WS-TABLE-LIMIT              PIC S9(04) COMP-3
                                         VALUE 200.
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
           05  WS-AJ-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AJ-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AJ-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AJ-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AJ-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AJ-CNT-06                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AJ-CNT-07                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AJ-CNT-08                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-AJ-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AJ-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AJ-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AJ-QTY-04                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AJ-QTY-05                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AJ-QTY-06                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-AJ-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AJ-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AJ-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-AJ-TXT-01                PIC X(08) VALUE SPACES.
           05  WS-AJ-TXT-02                PIC X(20) VALUE SPACES.
           05  WS-AJ-TXT-03                PIC X(08) VALUE SPACES.
           05  WS-AJ-TXT-04                PIC X(30) VALUE SPACES.
           05  WS-AJ-TXT-05                PIC X(20) VALUE SPACES.
           05  WS-AJ-TXT-06                PIC X(10) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-AJ-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-AJ-ON-01                 VALUE 'Y'.
               88  WS-AJ-OFF-01                VALUE 'N'.
           05  WS-AJ-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-AJ-ON-02                 VALUE 'Y'.
               88  WS-AJ-OFF-02                VALUE 'N'.
           05  WS-AJ-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-AJ-ON-03                 VALUE 'Y'.
               88  WS-AJ-OFF-03                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-AJ-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-AJ-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-AJ-SUB-03                PIC S9(04) COMP-3 VALUE 0.
           05  WS-AJ-SUB-04                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. A ROW THAT DOES NOT FIT IS COUNTED AND DROPPED
      * - THE STEP DOES NOT FAIL.
       01  WS-AJ-TABLE.
           05  WS-AJ-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-AJ-TB-ENTRY OCCURS 200 TIMES
                                       INDEXED BY WS-AJ-IX.
               10  WS-AJ-TB-KEY                PIC X(08).
               10  WS-AJ-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-AJ-TB-TXT                PIC X(20).
               10  WS-AJ-TB-EFF                PIC 9(05).
               10  WS-AJ-TB-EXP                PIC 9(05).
       01  WS-AJ-WORK-GROUP-1.
           05  WS-AJ-G1-GROUP              PIC 9(05).
           05  WS-AJ-G1-BAND               PIC S9(11)V9(02) COMP-3.
           05  WS-AJ-G1-TYPE               PIC S9(11)V9(02) COMP-3.
           05  WS-AJ-G1-STATUS             PIC 9(07).
           05  WS-AJ-G1-INVOICE            PIC 9(05).
           05  WS-AJ-G1-GROUP              PIC S9(09) COMP-3.
           05  WS-AJ-G1-TARGET             PIC S9(09) COMP-3.
       01  WS-AJ-WORK-GROUP-2.
           05  WS-AJ-G2-JURIS              PIC X(10).
           05  WS-AJ-G2-INVOICE            PIC X(20).
           05  WS-AJ-G2-PERIOD             PIC S9(09) COMP-3.
       01  WS-AJ-WORK-GROUP-3.
           05  WS-AJ-G3-STATUS             PIC S9(11)V9(02) COMP-3.
           05  WS-AJ-G3-LEVEL              PIC 9(07).
           05  WS-AJ-G3-CENTRE             PIC X(10).
           05  WS-AJ-G3-GROUP              PIC S9(09) COMP-3.
           05  WS-AJ-G3-TARGET             PIC S9(09) COMP-3.
           05  WS-AJ-G3-STATUS             PIC X(20).
           05  WS-AJ-G3-CENTRE             PIC 9(05).
           05  WS-AJ-G3-SEGMENT            PIC X(20).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 50.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUXR15 - ORPHAN KEY CROSS REFERENCE'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-AJ-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-AJ-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9936.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-AJ-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-AJ-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT XRFIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'XRFIN OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               DISPLAY 'XRFIN FILE STATUS = ' WS-FS-INPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT ORPOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'ORPOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               DISPLAY 'ORPOUT FILE STATUS = ' WS-FS-OUTPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT LNKOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'LNKOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               DISPLAY 'LNKOUT FILE STATUS = ' WS-FS-OUTPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SUSOUT.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'SUSOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               DISPLAY 'SUSOUT FILE STATUS = ' WS-FS-SUSPENSE
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CTLOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               DISPLAY 'CTLOUT FILE STATUS = ' WS-FS-CONTROL
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RPTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RPTOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               DISPLAY 'RPTOUT FILE STATUS = ' WS-FS-OUTPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
       P1100-EXIT.
           EXIT.
      * P1200-READ-PARM - THE CYCLE DATE ARRIVES AS TWO DIGITS AND IS
      * PIVOTED ON DW-PIVOT-YY BEFORE ANY DATE MATH.
       P1200-READ-PARM.
           MOVE SPACES TO WS-PARM-CARD.
           ACCEPT WS-PARM-CARD FROM SYSIN.
           CALL 'CABPARMR' USING WS-PARM-CARD WS-RC-PARMR.
           MOVE PC1-CYCLE-YYDDD TO WS-AJ-CYCLE-YYDDD.
           COMPUTE WS-AJ-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-AJ-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-AJ-CNT-04.
           MOVE 0 TO WS-AJ-CNT-02.
           MOVE 0 TO WS-AJ-CNT-05.
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
           IF WS-AJ-ON-02
               PERFORM P2200-CONVERT-GROUP THRU
                   P2200-CONVERT-GROUP-EXIT.
           PERFORM P2300-BUILD-GROUP THRU P2300-BUILD-GROUP-EXIT.
           PERFORM P2400-EDIT-MATCH THRU P2400-EDIT-MATCH-EXIT.
           IF WS-AJ-ON-03
               PERFORM P2500-MATCH-REFERENCE THRU
                   P2500-MATCH-REFERENCE-EXIT.
           PERFORM P2600-CONVERT-REFERENCE THRU
               P2600-CONVERT-REFERENCE-EXIT.
           IF WS-AJ-ON-02
               PERFORM P2700-EXPAND-MATCH THRU P2700-EXPAND-MATCH-EXIT.
           IF WS-AJ-ON-02
               PERFORM P2800-MATCH-MATCH THRU P2800-MATCH-MATCH-EXIT.
           IF WS-AJ-ON-02
               PERFORM P2900-CHECK-LINK THRU P2900-CHECK-LINK-EXIT.
           PERFORM P21000-DERIVE-LINK THRU P21000-DERIVE-LINK-EXIT.
           PERFORM P21100-RESOLVE-REFERENCE THRU
               P21100-RESOLVE-REFERENCE-EXIT.
           IF WS-AJ-ON-03
               PERFORM P21200-MATCH-SIDE THRU P21200-MATCH-SIDE-EXIT.
           PERFORM P21300-APPLY-GROUP THRU P21300-APPLY-GROUP-EXIT.
           PERFORM P21400-APPLY-MATCH THRU P21400-APPLY-MATCH-EXIT.
           PERFORM P21500-SELECT-SIDE THRU P21500-SELECT-SIDE-EXIT.
           IF WS-AJ-ON-02
               PERFORM P21600-RESOLVE-MATCH THRU
                   P21600-RESOLVE-MATCH-EXIT.
           IF WS-AJ-ON-01
               PERFORM P21700-VALIDATE-LINK THRU
                   P21700-VALIDATE-LINK-EXIT.
           PERFORM P21800-CONVERT-PAIR THRU P21800-CONVERT-PAIR-EXIT.
           IF WS-AJ-ON-01
               PERFORM P21900-APPLY-MATCH THRU P21900-APPLY-MATCH-EXIT.
           PERFORM P22000-SPLIT-GROUP THRU P22000-SPLIT-GROUP-EXIT.
           PERFORM P22100-VALIDATE-PAIR THRU P22100-VALIDATE-PAIR-EXIT.
           PERFORM P22200-RESOLVE-GROUP THRU P22200-RESOLVE-GROUP-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ XRFIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P2200-CONVERT-GROUP.
           UNSTRING WS-AJ-TXT-03 DELIMITED BY '/'
               INTO WS-AJ-TXT-04
               WS-AJ-TXT-02
               TALLYING IN WS-AJ-CNT-01.
       P2200-CONVERT-GROUP-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P2300-BUILD-GROUP.
           CALL 'CABHASH' USING IA-LEVEL WS-ACC-OCN-HASH.
           ADD WS-AJ-CNT-04 TO WS-ACC-SEQ-HASH.
       P2300-BUILD-GROUP-EXIT.
           EXIT.
       P2400-EDIT-MATCH.
           MOVE WS-AJ-AMT-03 TO WS-AJ-AMT-01.
           IF WS-AJ-AMT-01 < 0
               COMPUTE WS-AJ-AMT-01 = 0 - WS-AJ-AMT-03.
       P2400-EDIT-MATCH-EXIT.
           EXIT.
       P2500-MATCH-REFERENCE.
           CALL 'CABFMTR' USING WS-AJ-TXT-02 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-AJ-CNT-02.
       P2500-MATCH-REFERENCE-EXIT.
           EXIT.
       P2600-CONVERT-REFERENCE.
           IF WS-AJ-AMT-02 NOT = 0
               COMPUTE WS-AJ-QTY-01 = WS-AJ-AMT-01 * 100 / WS-AJ-AMT-02
           ELSE
               MOVE 0 TO WS-AJ-QTY-01.
       P2600-CONVERT-REFERENCE-EXIT.
           EXIT.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P2700-EXPAND-MATCH.
           IF WS-AJ-AMT-01 < 3
               MOVE 3 TO WS-AJ-AMT-01
               ADD 1 TO WS-AJ-CNT-04.
           IF WS-AJ-AMT-01 > 91063
               MOVE 91063 TO WS-AJ-AMT-01
               ADD 1 TO WS-AJ-CNT-01.
       P2700-EXPAND-MATCH-EXIT.
           EXIT.
       P2800-MATCH-MATCH.
           MOVE 0 TO WS-AJ-CNT-05.
           INSPECT WS-AJ-TXT-03 TALLYING WS-AJ-CNT-05
               FOR ALL SPACES.
           INSPECT WS-AJ-TXT-03 REPLACING ALL LOW-VALUES BY SPACES.
       P2800-MATCH-MATCH-EXIT.
           EXIT.
       P2900-CHECK-LINK.
           MOVE 0 TO WS-AJ-QTY-04.
           MOVE 0 TO WS-AJ-QTY-02.
           MOVE 0 TO WS-AJ-AMT-03.
       P2900-CHECK-LINK-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P21000-DERIVE-LINK.
           MOVE SPACES TO CABS-AJ-OUT-RECORD.
           MOVE IA-BAND2 TO OA-ACCOUNT.
           MOVE IA-BAND TO OA-STATUS.
           MOVE IA-SEQ TO OA-BAN.
           MOVE IA-ACCOUNT TO OA-ACCOUNT2.
           MOVE IA-GROUP TO OA-CYCLE.
           MOVE IA-ACCOUNT TO OA-CENTRE.
           MOVE IA-BAND2 TO OA-GROUP.
           MOVE IA-SEQ TO OA-TARGET.
           WRITE CABS-AJ-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P21000-DERIVE-LINK-EXIT.
           EXIT.
       P21100-RESOLVE-REFERENCE.
           MOVE 'N' TO WS-AJ-SW-01.
           IF WS-AJ-TXT-06 NOT = WS-AJ-TXT-02
               MOVE 'Y' TO WS-AJ-SW-01
               MOVE WS-AJ-TXT-06 TO WS-AJ-TXT-02
               ADD 1 TO WS-AJ-CNT-07.
       P21100-RESOLVE-REFERENCE-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P21200-MATCH-SIDE.
           IF IA-LEVEL = 'C'
               ADD 1 TO WS-AJ-CNT-07
           ELSE
               IF IA-LEVEL = 'B'
                   ADD 1 TO WS-AJ-CNT-02
               ELSE
                   IF IA-LEVEL = 'C'
                       ADD 1 TO WS-AJ-CNT-05
                   ELSE
                       ADD 1 TO WS-AJ-CNT-03.
       P21200-MATCH-SIDE-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P21300-APPLY-GROUP.
           MOVE 'Y' TO WS-AJ-SW-02.
           IF IA-TYPE < 8
               MOVE 'N' TO WS-AJ-SW-02
               ADD 1 TO WS-AJ-CNT-04.
           IF IA-TYPE > 660
               MOVE 'N' TO WS-AJ-SW-02
               ADD 1 TO WS-AJ-CNT-05.
       P21300-APPLY-GROUP-EXIT.
           EXIT.
       P21400-APPLY-MATCH.
           ADD IA-TYPE TO WS-AJ-QTY-04.
           COMPUTE WS-AJ-AMT-01 = WS-AJ-QTY-04 * WS-AJ-QTY-06.
           ADD WS-AJ-AMT-01 TO WS-AJ-AMT-03.
       P21400-APPLY-MATCH-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P21500-SELECT-SIDE.
           MOVE SPACES TO WS-AJ-TXT-04.
           STRING IA-ACCOUNT DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IA-TARGET DELIMITED BY SIZE
               INTO WS-AJ-TXT-04.
       P21500-SELECT-SIDE-EXIT.
           EXIT.
       P21600-RESOLVE-MATCH.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-BAN-UNKNOWN TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-AJ-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
       P21600-RESOLVE-MATCH-EXIT.
           EXIT.
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT
      * PRECEDES THIS PROGRAM IN THE JOB.
       P21700-VALIDATE-LINK.
           MOVE IA-ACCOUNT TO WS-AJ-TXT-05.
           MOVE IA-BAND TO WS-AJ-TXT-02.
           MOVE IA-BAND TO WS-AJ-TXT-01.
           MOVE IA-LEVEL TO WS-AJ-TXT-02.
           ADD 1 TO WS-AJ-CNT-02.
       P21700-VALIDATE-LINK-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P21800-CONVERT-PAIR.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-AJ-TXT-04 TO PC-COL-001-020.
           MOVE WS-AJ-TXT-03 TO PC-COL-021-060.
           MOVE WS-AJ-AMT-01 TO WS-AJ-AMT-EDIT.
           MOVE WS-AJ-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P21800-CONVERT-PAIR-EXIT.
           EXIT.
       P21900-APPLY-MATCH.
           UNSTRING WS-AJ-TXT-04 DELIMITED BY '/'
               INTO WS-AJ-TXT-04
               WS-AJ-TXT-05
               TALLYING IN WS-AJ-CNT-04.
       P21900-APPLY-MATCH-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P22000-SPLIT-GROUP.
           CALL 'CABHASH' USING IA-LEVEL2 WS-ACC-OCN-HASH.
           ADD WS-AJ-CNT-08 TO WS-ACC-SEQ-HASH.
       P22000-SPLIT-GROUP-EXIT.
           EXIT.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P22100-VALIDATE-PAIR.
           MOVE WS-AJ-AMT-03 TO WS-AJ-AMT-02.
           IF WS-AJ-AMT-02 < 0
               COMPUTE WS-AJ-AMT-02 = 0 - WS-AJ-AMT-03.
       P22100-VALIDATE-PAIR-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P22200-RESOLVE-GROUP.
           CALL 'CABCTLWR' USING WS-AJ-TXT-02 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-AJ-CNT-03.
       P22200-RESOLVE-GROUP-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-POST-GROUP.
           ADD IA-LEVEL TO WS-AJ-QTY-03.
           COMPUTE WS-AJ-AMT-02 = WS-AJ-QTY-03 * WS-AJ-QTY-02.
           ADD WS-AJ-AMT-02 TO WS-AJ-AMT-03.
       P3100-POST-GROUP-EXIT.
           EXIT.
       P3200-WRITE-ORPHAN.
           MOVE IA-GROUP TO WS-AJ-TXT-03.
           MOVE IA-CLASS TO WS-AJ-TXT-05.
           MOVE IA-CARRIER TO WS-AJ-TXT-05.
           ADD 1 TO WS-AJ-CNT-08.
       P3200-WRITE-ORPHAN-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P3300-WRITE-SIDE.
           MOVE SPACES TO WS-AJ-TXT-04.
           STRING IA-LEVEL DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IA-CIRCUIT DELIMITED BY SIZE
               INTO WS-AJ-TXT-04.
       P3300-WRITE-SIDE-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P3400-POST-PAIR.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-OCN-UNKNOWN TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-AJ-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
           MOVE CABS-SUSPENSE-RECORD TO CABS-SUSOUT-RECORD.
           WRITE CABS-SUSOUT-RECORD.
       P3400-POST-PAIR-EXIT.
           EXIT.
      * S400-SECONDARY-PROCESSING SECTION
       S400-SECONDARY-PROCESSING SECTION.
       P4000-SECONDARY.
           PERFORM P4100-REPORT-STATUS THRU P4100-REPORT-STATUS-EXIT.
           PERFORM P4200-RECONCILE-GROUP THRU
               P4200-RECONCILE-GROUP-EXIT.
       P4000-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P4100-REPORT-STATUS.
           IF WS-AJ-AMT-03 < 26
               MOVE 26 TO WS-AJ-AMT-03
               ADD 1 TO WS-AJ-CNT-01.
           IF WS-AJ-AMT-03 > 8267
               MOVE 8267 TO WS-AJ-AMT-03
               ADD 1 TO WS-AJ-CNT-06.
       P4100-REPORT-STATUS-EXIT.
           EXIT.
       P4200-RECONCILE-GROUP.
           MOVE 0 TO WS-AJ-QTY-06.
           MOVE 0 TO WS-AJ-QTY-03.
           MOVE 0 TO WS-AJ-QTY-01.
           MOVE 0 TO WS-AJ-AMT-02.
           MOVE 0 TO WS-AJ-AMT-01.
       P4200-RECONCILE-GROUP-EXIT.
           EXIT.
           MOVE 0 TO WS-AJ-QTY-04.
           PERFORM P380-WALK-REFERENCE THRU P380-WALK-REFERENCE-EXIT
               VARYING WS-AJ-SUB-03 FROM 1 BY 1
               UNTIL WS-AJ-SUB-03 > WS-AJ-TAB-CNT.
       P380-WALK-REFERENCE.
           SET WS-AJ-IX TO WS-AJ-SUB-04.
           IF WS-AJ-TB-KEY (WS-AJ-IX) NOT = SPACES
               ADD WS-AJ-TB-VAL (WS-AJ-IX) TO WS-AJ-QTY-06.
       P380-WALK-REFERENCE-EXIT.
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
           MOVE 'INPUT RECORDS' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO WS-AJ-CNT-EDIT.
           MOVE WS-AJ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'OUTPUT RECORDS' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-AJ-CNT-EDIT.
           MOVE WS-AJ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'SUSPENDED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-AJ-CNT-EDIT.
           MOVE WS-AJ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'SUMMARISED' TO PC-COL-001-020.
           MOVE WS-SUMM-CNT TO WS-AJ-CNT-EDIT.
           MOVE WS-AJ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'HELD FOR NEXT RUN' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-AJ-CNT-EDIT.
           MOVE WS-AJ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-AJ-CNT-01 TO WS-AJ-CNT-EDIT.
           MOVE WS-AJ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 02' TO PC-COL-001-020.
           MOVE WS-AJ-CNT-02 TO WS-AJ-CNT-EDIT.
           MOVE WS-AJ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE WS-AJ-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE 1 TO CT-STEP-SEQ.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-FILLER.
           MOVE WS-AJ-TXT-06 TO CT-RESTART-KEY.
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
           CLOSE XRFIN.
           CLOSE ORPOUT.
           CLOSE LNKOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABUXR15 - NORMAL END OF JOB'.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  AJ-CNT-02 = ' WS-AJ-CNT-02.
           DISPLAY '  AJ-CNT-04 = ' WS-AJ-CNT-04.
           DISPLAY '  AJ-CNT-03 = ' WS-AJ-CNT-03.
           DISPLAY '  AJ-CNT-06 = ' WS-AJ-CNT-06.
       P9000-EXIT.
           EXIT.
