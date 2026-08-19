      *****************************************************************
      * CABUXR13 - JURISDICTION TO STATE CROSS REFERENCE              *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               XRFIN   TELCABS.CABS.XRFIN          (LOCAL)     *
      *               RGTIN   TELCABS.CABS.RGTIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               ORPOUT  TELCABS.CABS.ORPOUT         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - THE OUTPUT IS REBUILT EACH RUN     *
      * REVISION HISTORY                                              *
      *   V1.00  1990-12-01  M.DELACROIX  INITIAL RELEASE             *
      *   V1.04  1993-12-06  C.ADEYEMI    CENTURY PIVOT APPLIED TO THE*
      *                      CYCLE DATE                               *
      *   V1.08  2000-11-11  S.MARCHETTI  HASH TOTAL ADDED TO THE     *
      *                      CONTROL RECORD                           *
      *   V1.10  2003-10-02  A.BUKOWSKI   REGION SIZE REDUCED - TABLE *
      *                      MOVED OUT OF WORKING STORAGE             *
      *   V1.13  2007-11-05  G.PRZYBYLSKI ROUNDING RULE TAKEN FROM THE*
      *                      RATE ROW                                 *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUXR13.
       AUTHOR. TELCABS APPLICATIONS - CROSS REFERENCE REPORT.
      *****************************************************************
      * JURISDICTION TO STATE CROSS REFERENCE. THIS STEP IS SCHEDULED *
      * INSIDE THE NIGHTLY ACCESS BILLING STREAM AND HAS NO           *
      * INTERACTIVE ENTRY POINT.                                      *
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
           SELECT RGTIN ASSIGN TO UT-S-RGTIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT ORPOUT ASSIGN TO UT-S-ORPOUT
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
      * XRFIN - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  XRFIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 110 CHARACTERS.
       01  CABS-BP-IN-RECORD.
           05  IB-TARIFF                   PIC S9(11) COMP-3.
           05  IB-SOURCE                   PIC S9(13)V9(02) COMP-3.
           05  IB-CENTRE                   PIC 9(03).
           05  IB-ACCOUNT                  PIC X(20).
           05  IB-REGION                   PIC S9(13)V9(02) COMP-3.
           05  IB-CARRIER                  PIC X(13).
           05  IB-TARIFF2                  PIC X(03).
           05  IB-SEQ                      PIC S9(09)V9(02) COMP-3.
           05  IB-PERIOD                   PIC S9(11) COMP-3.
           05  IB-CIRCUIT                  PIC X(03).
           05  IB-STATE                    PIC X(16).
           05  IB-SOURCE2                  PIC X(10).
           05  BP-FILL-01                  PIC X(8).
      * RGTIN - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  RGTIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 110 CHARACTERS.
       01  CABS-BP-ALT1-RECORD.
           05  A1-BAND                     PIC S9(15) COMP-3.
           05  A1-CIRCUIT                  PIC X(16).
           05  A1-BAN                      PIC S9(13)V9(02) COMP-3.
           05  A1-SEGMENT                  PIC S9(13) COMP-3.
           05  A1-STATE                    PIC X(13).
           05  A1-STATE2                   PIC X(10).
           05  A1-CYCLE                    PIC X(06).
           05  A1-STATE3                   PIC S9(07)V9(05) COMP-3.
           05  A1-TARGET                   PIC X(08).
           05  A1-TYPE                     PIC S9(13)V9(02) COMP-3.
           05  BP-FILL-02                  PIC X(19).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-BP-VIEW1 REDEFINES CABS-BP-IN-RECORD.
           05  R0B-PERIOD                  PIC X(10).
           05  R0B-SEQ                     PIC X(13).
           05  R0B-PERIOD2                 PIC X(04).
           05  R0B-GROUP                   PIC 9(09).
           05  R0B-CYCLE                   PIC 9(09).
           05  R0B-TARGET                  PIC S9(09)V9(02) COMP-3.
           05  R0B-BAND                    PIC S9(09)V9(02) COMP-3.
           05  R0B-OCN                     PIC X(10).
           05  R0B-INVOICE                 PIC X(06).
           05  R0B-REST                    PIC X(37).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-BP-VIEW2 REDEFINES CABS-BP-IN-RECORD.
           05  R1B-STATE                   PIC S9(13) COMP-3.
           05  R1B-CLASS                   PIC 9(05).
           05  R1B-TARIFF                  PIC S9(13)V9(02) COMP-3.
           05  R1B-SEGMENT                 PIC 9(06).
           05  R1B-INVOICE                 PIC S9(09) COMP-3.
           05  R1B-REGION                  PIC 9(06).
           05  R1B-JURIS                   PIC X(10).
           05  R1B-OCN                     PIC S9(13)V9(05) COMP-3.
           05  R1B-CENTRE                  PIC X(10).
           05  R1B-REST                    PIC X(43).
      * ORPOUT - PERMANENT DATASET HELD ON DASD.
       FD  ORPOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 150 CHARACTERS.
       01  CABS-BP-OUT-RECORD.
           05  OB-OCN                      PIC S9(15) COMP-3.
           05  OB-CYCLE                    PIC X(03).
           05  OB-CENTRE                   PIC X(13).
           05  OB-TARIFF                   PIC X(13).
           05  OB-SEQ                      PIC S9(07)V9(05) COMP-3.
           05  OB-SEQ2                     PIC X(20).
           05  OB-ELEM                     PIC S9(05) COMP-3.
           05  OB-TARIFF2                  PIC X(16).
           05  OB-JURIS                    PIC X(02).
           05  OB-TARGET                   PIC X(08).
           05  OB-CIRCUIT                  PIC S9(13) COMP-3.
           05  OB-CARRIER                  PIC S9(09)V9(02) COMP-3.
           05  OB-GROUP                    PIC X(04).
           05  OB-GROUP2                   PIC X(16).
           05  OB-TARGET2                  PIC S9(11)V9(02) COMP-3.
           05  OB-JURIS2                   PIC 9(06).
           05  OB-BAN                      PIC 9(02).
           05  OB-CIRCUIT2                 PIC S9(11) COMP-3.
           05  BP-FILL-03                  PIC X(3).
      * CTLOUT - CATALOGUED GENERATION DATA GROUP.
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
      * SHARED LAYOUT PULLED IN FOR THE PAIR SIDE.
       COPY CABSCOMM.
      * SHARED LAYOUT PULLED IN FOR THE MATCH SIDE.
       COPY CABSCIRC.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUXR13'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V3.24'.
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
       01  WS-PARM-CARD-R2 REDEFINES WS-PARM-CARD.
           05  PC2-LEAD                    PIC X(14).
           05  PC2-CYCLE-VIEW.
               10  PC2-CV-YY                   PIC 9(02).
               10  PC2-CV-DDD                  PIC 9(03).
           05  PC2-REST                    PIC X(61).
       01  WS-COUNT-AREA.
           05  WS-BP-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BP-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BP-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BP-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BP-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BP-CNT-06                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-BP-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BP-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BP-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BP-QTY-04                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BP-QTY-05                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BP-QTY-06                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-BP-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BP-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BP-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-BP-TXT-01                PIC X(08) VALUE SPACES.
           05  WS-BP-TXT-02                PIC X(20) VALUE SPACES.
           05  WS-BP-TXT-03                PIC X(08) VALUE SPACES.
           05  WS-BP-TXT-04                PIC X(10) VALUE SPACES.
           05  WS-BP-TXT-05                PIC X(20) VALUE SPACES.
           05  WS-BP-TXT-06                PIC X(26) VALUE SPACES.
           05  WS-BP-TXT-07                PIC X(10) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-BP-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-BP-ON-01                 VALUE 'Y'.
               88  WS-BP-OFF-01                VALUE 'N'.
           05  WS-BP-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-BP-ON-02                 VALUE 'Y'.
               88  WS-BP-OFF-02                VALUE 'N'.
           05  WS-BP-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-BP-ON-03                 VALUE 'Y'.
               88  WS-BP-OFF-03                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-BP-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-BP-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-BP-SUB-03                PIC S9(04) COMP-3 VALUE 0.
           05  WS-BP-SUB-04                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. A ROW THAT DOES NOT FIT IS COUNTED AND DROPPED
      * - THE STEP DOES NOT FAIL.
       01  WS-BP-TABLE.
           05  WS-BP-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-BP-TB-ENTRY OCCURS 150 TIMES
                                       INDEXED BY WS-BP-IX.
               10  WS-BP-TB-KEY                PIC X(10).
               10  WS-BP-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-BP-TB-TXT                PIC X(40).
               10  WS-BP-TB-EFF                PIC 9(05).
               10  WS-BP-TB-EXP                PIC 9(05).
       01  WS-BP-WORK-GROUP-1.
           05  WS-BP-G1-CODE               PIC 9(05).
           05  WS-BP-G1-STATE              PIC 9(07).
           05  WS-BP-G1-BAN                PIC 9(07).
           05  WS-BP-G1-MEDIA              PIC S9(11)V9(02) COMP-3.
       01  WS-BP-WORK-GROUP-2.
           05  WS-BP-G2-PERIOD             PIC X(20).
           05  WS-BP-G2-JURIS              PIC 9(07).
           05  WS-BP-G2-STATUS             PIC S9(09) COMP-3.
           05  WS-BP-G2-SEGMENT            PIC 9(05).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 55.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUXR13 - JURISDICTION TO STATE CROSS REFERENCE'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-BP-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-BP-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9969.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-BP-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-BP-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
               DISPLAY 'XRFIN FILE STATUS = ' WS-FS-INPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON XRFIN - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN INPUT RGTIN.
           IF WS-FS-INPUT NOT = '00'
               DISPLAY 'RGTIN FILE STATUS = ' WS-FS-INPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON RGTIN - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT ORPOUT.
           IF WS-FS-OUTPUT NOT = '00'
               DISPLAY 'ORPOUT FILE STATUS = ' WS-FS-OUTPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON ORPOUT - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               DISPLAY 'CTLOUT FILE STATUS = ' WS-FS-CONTROL
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON CTLOUT - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RPTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               DISPLAY 'RPTOUT FILE STATUS = ' WS-FS-OUTPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON RPTOUT - CHECK THE ALLOCATION' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-BP-CYCLE-YYDDD.
           COMPUTE WS-BP-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-BP-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-BP-CNT-04.
           MOVE 0 TO WS-BP-CNT-06.
           MOVE 0 TO WS-BP-CNT-02.
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
           IF WS-BP-ON-03
               PERFORM P2200-RESOLVE-ORPHAN THRU
                   P2200-RESOLVE-ORPHAN-EXIT.
           PERFORM P2300-MATCH-PAIR THRU P2300-MATCH-PAIR-EXIT.
           PERFORM P2400-EDIT-GROUP THRU P2400-EDIT-GROUP-EXIT.
           PERFORM P2500-MATCH-REFERENCE THRU
               P2500-MATCH-REFERENCE-EXIT.
           PERFORM P2600-SPLIT-SIDE THRU P2600-SPLIT-SIDE-EXIT.
           PERFORM P2700-EXPAND-LINK THRU P2700-EXPAND-LINK-EXIT.
           PERFORM P2800-BUILD-GROUP THRU P2800-BUILD-GROUP-EXIT.
           PERFORM P2900-EDIT-LINK THRU P2900-EDIT-LINK-EXIT.
           PERFORM P21000-SELECT-REFERENCE THRU
               P21000-SELECT-REFERENCE-EXIT.
           PERFORM P21100-DERIVE-SIDE THRU P21100-DERIVE-SIDE-EXIT.
           IF WS-BP-ON-02
               PERFORM P21200-SPLIT-PAIR THRU P21200-SPLIT-PAIR-EXIT.
           IF WS-BP-ON-01
               PERFORM P21300-MATCH-ORPHAN THRU
                   P21300-MATCH-ORPHAN-EXIT.
           IF WS-BP-ON-02
               PERFORM P21400-APPLY-REFERENCE THRU
                   P21400-APPLY-REFERENCE-EXIT.
           IF WS-BP-ON-02
               PERFORM P21500-RESOLVE-PAIR THRU
                   P21500-RESOLVE-PAIR-EXIT.
           PERFORM P21600-EXPAND-LINK THRU P21600-EXPAND-LINK-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ XRFIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-RESOLVE-ORPHAN.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-BAN-UNKNOWN TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-BP-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
       P2200-RESOLVE-ORPHAN-EXIT.
           EXIT.
       P2300-MATCH-PAIR.
           MOVE 0 TO WS-BP-CNT-06.
           INSPECT WS-BP-TXT-03 TALLYING WS-BP-CNT-06
               FOR ALL SPACES.
           INSPECT WS-BP-TXT-03 REPLACING ALL LOW-VALUES BY SPACES.
       P2300-MATCH-PAIR-EXIT.
           EXIT.
       P2400-EDIT-GROUP.
           IF WS-BP-AMT-03 < 17
               MOVE 17 TO WS-BP-AMT-03
               ADD 1 TO WS-BP-CNT-05.
           IF WS-BP-AMT-03 > 62525
               MOVE 62525 TO WS-BP-AMT-03
               ADD 1 TO WS-BP-CNT-05.
       P2400-EDIT-GROUP-EXIT.
           EXIT.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P2500-MATCH-REFERENCE.
           MOVE 0 TO WS-BP-QTY-02.
           MOVE 0 TO WS-BP-QTY-04.
           MOVE 0 TO WS-BP-QTY-05.
           MOVE 0 TO WS-BP-AMT-02.
       P2500-MATCH-REFERENCE-EXIT.
           EXIT.
       P2600-SPLIT-SIDE.
           MOVE 'N' TO WS-BP-SW-03.
           IF WS-BP-TXT-02 NOT = WS-BP-TXT-03
               MOVE 'Y' TO WS-BP-SW-03
               MOVE WS-BP-TXT-02 TO WS-BP-TXT-03
               ADD 1 TO WS-BP-CNT-02.
       P2600-SPLIT-SIDE-EXIT.
           EXIT.
       P2700-EXPAND-LINK.
           MOVE IB-ACCOUNT TO WS-BP-TXT-04.
           MOVE IB-TARIFF2 TO WS-BP-TXT-07.
           MOVE IB-CIRCUIT TO WS-BP-TXT-03.
           MOVE IB-STATE TO WS-BP-TXT-06.
           ADD 1 TO WS-BP-CNT-06.
       P2700-EXPAND-LINK-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P2800-BUILD-GROUP.
           CALL 'CABSEQCK' USING WS-BP-TXT-05 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-BP-CNT-05.
       P2800-BUILD-GROUP-EXIT.
           EXIT.
       P2900-EDIT-LINK.
           MOVE WS-BP-AMT-01 TO WS-BP-AMT-02.
           IF WS-BP-AMT-02 < 0
               COMPUTE WS-BP-AMT-02 = 0 - WS-BP-AMT-01.
       P2900-EDIT-LINK-EXIT.
           EXIT.
       P21000-SELECT-REFERENCE.
           MOVE SPACES TO WS-BP-TXT-07.
           STRING IB-PERIOD DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IB-SEQ DELIMITED BY SIZE
               INTO WS-BP-TXT-07.
       P21000-SELECT-REFERENCE-EXIT.
           EXIT.
      * A FAILURE HERE SUSPENDS THE RECORD, IT DOES NOT STOP THE STEP.
       P21100-DERIVE-SIDE.
           MOVE SPACES TO CABS-BP-OUT-RECORD.
           MOVE IB-CIRCUIT TO OB-OCN.
           MOVE IB-ACCOUNT TO OB-CYCLE.
           MOVE IB-REGION TO OB-CENTRE.
           MOVE IB-SEQ TO OB-TARIFF.
           MOVE IB-STATE TO OB-SEQ.
           MOVE IB-SOURCE TO OB-SEQ2.
           MOVE IB-PERIOD TO OB-ELEM.
           MOVE IB-CENTRE TO OB-TARIFF2.
           WRITE CABS-BP-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P21100-DERIVE-SIDE-EXIT.
           EXIT.
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT
      * PRECEDES THIS PROGRAM IN THE JOB.
       P21200-SPLIT-PAIR.
           UNSTRING WS-BP-TXT-04 DELIMITED BY '/'
               INTO WS-BP-TXT-03
               WS-BP-TXT-07
               TALLYING IN WS-BP-CNT-04.
       P21200-SPLIT-PAIR-EXIT.
           EXIT.
       P21300-MATCH-ORPHAN.
           IF IB-TARIFF2 = 'S'
               ADD 1 TO WS-BP-CNT-04
           ELSE
               IF IB-TARIFF2 = 'X'
                   ADD 1 TO WS-BP-CNT-05
               ELSE
                   IF IB-TARIFF2 = 'A'
                       ADD 1 TO WS-BP-CNT-01
                   ELSE
                       ADD 1 TO WS-BP-CNT-04.
       P21300-MATCH-ORPHAN-EXIT.
           EXIT.
       P21400-APPLY-REFERENCE.
           ADD IB-REGION TO WS-BP-QTY-05.
           COMPUTE WS-BP-AMT-01 = WS-BP-QTY-05 * WS-BP-QTY-01.
           ADD WS-BP-AMT-01 TO WS-BP-AMT-02.
       P21400-APPLY-REFERENCE-EXIT.
           EXIT.
       P21500-RESOLVE-PAIR.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-BP-TXT-07 TO PC-COL-001-020.
           MOVE WS-BP-TXT-06 TO PC-COL-021-060.
           MOVE WS-BP-AMT-03 TO WS-BP-AMT-EDIT.
           MOVE WS-BP-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P21500-RESOLVE-PAIR-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P21600-EXPAND-LINK.
           CALL 'CABHASH' USING IB-STATE WS-ACC-OCN-HASH.
           ADD WS-BP-CNT-06 TO WS-ACC-SEQ-HASH.
       P21600-EXPAND-LINK-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P3100-POST-REFERENCE.
           MOVE SPACES TO CABS-BP-OUT-RECORD.
           MOVE IB-TARIFF2 TO OB-OCN.
           MOVE IB-TARIFF TO OB-CYCLE.
           MOVE IB-TARIFF2 TO OB-CENTRE.
           MOVE IB-SEQ TO OB-TARIFF.
           MOVE IB-ACCOUNT TO OB-SEQ.
           MOVE IB-CARRIER TO OB-SEQ2.
           MOVE IB-CARRIER TO OB-ELEM.
           MOVE IB-CENTRE TO OB-TARIFF2.
           MOVE IB-CENTRE TO OB-JURIS.
           WRITE CABS-BP-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P3100-POST-REFERENCE-EXIT.
           EXIT.
       P3200-POST-PAIR.
           MOVE IB-CENTRE TO WS-BP-TXT-04.
           MOVE IB-CIRCUIT TO WS-BP-TXT-03.
           MOVE IB-TARIFF TO WS-BP-TXT-06.
           ADD 1 TO WS-BP-CNT-05.
       P3200-POST-PAIR-EXIT.
           EXIT.
       P3300-FORMAT-ORPHAN.
           MOVE SPACES TO WS-BP-TXT-01.
           STRING IB-TARIFF2 DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IB-CIRCUIT DELIMITED BY SIZE
               INTO WS-BP-TXT-01.
       P3300-FORMAT-ORPHAN-EXIT.
           EXIT.
       P3400-POST-SIDE.
           ADD IB-SOURCE TO WS-BP-QTY-04.
           COMPUTE WS-BP-AMT-02 = WS-BP-QTY-04 * WS-BP-QTY-04.
           ADD WS-BP-AMT-02 TO WS-BP-AMT-01.
       P3400-POST-SIDE-EXIT.
           EXIT.
      * S400-SECONDARY-PROCESSING SECTION
       S400-SECONDARY-PROCESSING SECTION.
       P4000-SECONDARY.
           PERFORM P4100-TRACE-TYPE THRU P4100-TRACE-TYPE-EXIT.
           PERFORM P4200-AUDIT-GROUP THRU P4200-AUDIT-GROUP-EXIT.
       P4000-EXIT.
           EXIT.
       P4100-TRACE-TYPE.
           IF WS-BP-AMT-03 < 50
               MOVE 50 TO WS-BP-AMT-03
               ADD 1 TO WS-BP-CNT-03.
           IF WS-BP-AMT-03 > 60862
               MOVE 60862 TO WS-BP-AMT-03
               ADD 1 TO WS-BP-CNT-06.
       P4100-TRACE-TYPE-EXIT.
           EXIT.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P4200-AUDIT-GROUP.
           IF IB-SOURCE2 = 'S'
               ADD 1 TO WS-BP-CNT-04
           ELSE
               IF IB-SOURCE2 = 'B'
                   ADD 1 TO WS-BP-CNT-06
               ELSE
                   IF IB-SOURCE2 = 'S'
                       ADD 1 TO WS-BP-CNT-03
                   ELSE
                       ADD 1 TO WS-BP-CNT-04.
       P4200-AUDIT-GROUP-EXIT.
           EXIT.
           MOVE 0 TO WS-BP-QTY-05.
           PERFORM P380-WALK-MATCH THRU P380-WALK-MATCH-EXIT
               VARYING WS-BP-SUB-03 FROM 1 BY 1
               UNTIL WS-BP-SUB-03 > WS-BP-TAB-CNT.
       P380-WALK-MATCH.
           SET WS-BP-IX TO WS-BP-SUB-01.
           IF WS-BP-TB-KEY (WS-BP-IX) NOT = SPACES
               ADD WS-BP-TB-VAL (WS-BP-IX) TO WS-BP-QTY-01.
       P380-WALK-MATCH-EXIT.
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
           MOVE 'RECORDS CARRIED FWD' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-BP-CNT-EDIT.
           MOVE WS-BP-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS WRITTEN' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-BP-CNT-EDIT.
           MOVE WS-BP-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS READ' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO WS-BP-CNT-EDIT.
           MOVE WS-BP-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS REJECTED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-BP-CNT-EDIT.
           MOVE WS-BP-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS SUMMARISED' TO PC-COL-001-020.
           MOVE WS-SUMM-CNT TO WS-BP-CNT-EDIT.
           MOVE WS-BP-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-BP-CNT-01 TO WS-BP-CNT-EDIT.
           MOVE WS-BP-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 02' TO PC-COL-001-020.
           MOVE WS-BP-CNT-02 TO WS-BP-CNT-EDIT.
           MOVE WS-BP-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE SPACES TO CT-FILLER.
           MOVE 0 TO CT-RC.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE WS-BP-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE 7 TO CT-STEP-SEQ.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
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
           CLOSE RGTIN.
           CLOSE ORPOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABUXR13 - RUN COMPLETE'.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  BP-CNT-01 = ' WS-BP-CNT-01.
           DISPLAY '  BP-CNT-04 = ' WS-BP-CNT-04.
       P9000-EXIT.
           EXIT.
