      *****************************************************************
      * CABUCV02 - RECORD LAYOUT UPLIFT                               *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               SRCIN   TELCABS.CABS.SRCIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               DSPOUT  TELCABS.CABS.DSPOUT         (LOCAL)     *
      *               CNVOUT  TELCABS.CABS.CNVOUT         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - THE OUTPUT IS REBUILT EACH RUN     *
      * REVISION HISTORY                                              *
      *   V1.00  1989-06-06  T.YAMASHITA  INITIAL RELEASE             *
      *   V1.02  1993-10-04  D.OKONKWO    CONTROL RECORD ADDED PER    *
      *                      CABS-STD-002                             *
      *   V1.04  1994-06-17  D.OKONKWO    OCCURS RAISED AFTER THE     *
      *                      FEBRUARY OVERFLOW                        *
      *   V1.06  2004-02-10  P.NAIR       CONTROL RECORD ADDED PER    *
      *                      CABS-STD-002                             *
      *   V1.10  2014-07-27  A.BUKOWSKI   HASH TOTAL ADDED TO THE     *
      *                      CONTROL RECORD                           *
      *   V1.11  2017-08-05  C.ADEYEMI    PRINT LINE WIDENED TO 133   *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUCV02.
       AUTHOR. TELCABS APPLICATIONS - FILE CONVERSION.
      *****************************************************************
      * RECORD LAYOUT UPLIFT. THIS STEP IS SCHEDULED INSIDE THE       *
      * NIGHTLY ACCESS BILLING STREAM AND HAS NO INTERACTIVE ENTRY    *
      * POINT.                                                        *
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO   *
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.                     *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           C01 IS TO-NEW-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SRCIN ASSIGN TO UT-S-SRCIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT DSPOUT ASSIGN TO UT-S-DSPOUT
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
      * SRCIN - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  SRCIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 170 CHARACTERS.
       01  CABS-AW-IN-RECORD.
           05  IA-ACCOUNT                  PIC X(10).
           05  IA-MEDIA                    PIC X(16).
           05  IA-CYCLE                    PIC X(10).
           05  IA-TARIFF                   PIC X(16).
           05  IA-LEVEL                    PIC X(06).
           05  IA-TYPE                     PIC X(08).
           05  IA-LEVEL2                   PIC X(10).
           05  IA-INVOICE                  PIC X(04).
           05  IA-MEDIA2                   PIC S9(13) COMP-3.
           05  IA-SEQ                      PIC 9(04).
           05  IA-BAN                      PIC X(20).
           05  IA-INVOICE2                 PIC X(03).
           05  IA-BAN2                     PIC X(16).
           05  IA-GROUP                    PIC X(04).
           05  IA-STATUS                   PIC X(10).
           05  IA-OCN                      PIC X(04).
           05  IA-CENTRE                   PIC X(06).
           05  IA-CARRIER                  PIC X(02).
           05  IA-BAN3                     PIC S9(07) COMP-3.
           05  AW-FILL-01                  PIC X(10).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-AW-VIEW1 REDEFINES CABS-AW-IN-RECORD.
           05  R0A-ACCOUNT                 PIC X(03).
           05  R0A-OCN                     PIC S9(07)V9(02) COMP-3.
           05  R0A-JURIS                   PIC X(08).
           05  R0A-TARIFF                  PIC X(13).
           05  R0A-LEVEL                   PIC 9(06).
           05  R0A-ELEM                    PIC S9(05) COMP-3.
           05  R0A-CLASS                   PIC S9(11)V9(02) COMP-3.
           05  R0A-REST                    PIC X(125).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-AW-VIEW2 REDEFINES CABS-AW-IN-RECORD.
           05  R1A-INVOICE                 PIC S9(07)V9(02) COMP-3.
           05  R1A-TARIFF                  PIC X(16).
           05  R1A-PERIOD                  PIC S9(11)V9(02) COMP-3.
           05  R1A-CLASS                   PIC S9(07)V9(02) COMP-3.
           05  R1A-REST                    PIC X(137).
      * DSPOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  DSPOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 100 CHARACTERS.
       01  CABS-AW-OUT-RECORD.
           05  OA-SEQ                      PIC X(02).
           05  OA-TARIFF                   PIC X(13).
           05  OA-STATE                    PIC X(03).
           05  OA-TARGET                   PIC S9(11) COMP-3.
           05  OA-BAND                     PIC S9(09)V9(02) COMP-3.
           05  OA-STATUS                   PIC X(04).
           05  OA-LEVEL                    PIC X(13).
           05  OA-TYPE                     PIC S9(05) COMP-3.
           05  OA-CLASS                    PIC X(08).
           05  OA-CLASS2                   PIC S9(15) COMP-3.
           05  OA-GROUP                    PIC X(08).
           05  OA-CODE                     PIC X(13).
           05  OA-CODE2                    PIC X(04).
           05  AW-FILL-02                  PIC X(9).
      * CNVOUT - WORK FILE, DELETED AT STEP END.
       FD  CNVOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 100 CHARACTERS.
       01  CABS-AW-OUT1-RECORD         PIC X(100).
      * CTLOUT - CATALOGUED GENERATION DATA GROUP.
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
      * SHARED LAYOUT PULLED IN FOR THE SIGN SIDE.
       COPY CABSCOMM.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUCV02'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V1.28'.
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
           05  WS-AW-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AW-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AW-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AW-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AW-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AW-CNT-06                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AW-CNT-07                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AW-CNT-08                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-AW-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AW-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AW-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AW-QTY-04                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AW-QTY-05                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-AW-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AW-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AW-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AW-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AW-AMT-05                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-AW-TXT-01                PIC X(30) VALUE SPACES.
           05  WS-AW-TXT-02                PIC X(12) VALUE SPACES.
           05  WS-AW-TXT-03                PIC X(12) VALUE SPACES.
           05  WS-AW-TXT-04                PIC X(10) VALUE SPACES.
           05  WS-AW-TXT-05                PIC X(30) VALUE SPACES.
           05  WS-AW-TXT-06                PIC X(12) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-AW-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-AW-ON-01                 VALUE 'Y'.
               88  WS-AW-OFF-01                VALUE 'N'.
           05  WS-AW-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-AW-ON-02                 VALUE 'Y'.
               88  WS-AW-OFF-02                VALUE 'N'.
           05  WS-AW-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-AW-ON-03                 VALUE 'Y'.
               88  WS-AW-OFF-03                VALUE 'N'.
           05  WS-AW-SW-04                 PIC X(01) VALUE 'N'.
               88  WS-AW-ON-04                 VALUE 'Y'.
               88  WS-AW-OFF-04                VALUE 'N'.
           05  WS-AW-SW-05                 PIC X(01) VALUE 'N'.
               88  WS-AW-ON-05                 VALUE 'Y'.
               88  WS-AW-OFF-05                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-AW-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-AW-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-AW-SUB-03                PIC S9(04) COMP-3 VALUE 0.
           05  WS-AW-SUB-04                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. A ROW THAT DOES NOT FIT IS COUNTED AND DROPPED
      * - THE STEP DOES NOT FAIL.
       01  WS-AW-TABLE.
           05  WS-AW-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-AW-TB-ENTRY OCCURS 400 TIMES
                                       INDEXED BY WS-AW-IX.
               10  WS-AW-TB-KEY                PIC X(08).
               10  WS-AW-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-AW-TB-TXT                PIC X(20).
               10  WS-AW-TB-EFF                PIC 9(05).
               10  WS-AW-TB-EXP                PIC 9(05).
       01  WS-AW-WORK-GROUP-1.
           05  WS-AW-G1-SEGMENT            PIC X(10).
           05  WS-AW-G1-BAN                PIC 9(05).
           05  WS-AW-G1-TARGET             PIC S9(11)V9(02) COMP-3.
           05  WS-AW-G1-LEVEL              PIC X(20).
           05  WS-AW-G1-SEGMENT            PIC X(10).
           05  WS-AW-G1-LEVEL              PIC S9(09) COMP-3.
           05  WS-AW-G1-ACCOUNT            PIC 9(05).
       01  WS-AW-WORK-GROUP-2.
           05  WS-AW-G2-CIRCUIT            PIC X(10).
           05  WS-AW-G2-JURIS              PIC 9(05).
           05  WS-AW-G2-SOURCE             PIC 9(05).
           05  WS-AW-G2-MEDIA              PIC S9(09) COMP-3.
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 50.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUCV02 - RECORD LAYOUT UPLIFT'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-AW-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-AW-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
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
           05  WS-AW-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-AW-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT SRCIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'SRCIN COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT DSPOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'DSPOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-AW-CYCLE-YYDDD.
           COMPUTE WS-AW-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-AW-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-AW-CNT-01.
           MOVE 0 TO WS-AW-CNT-06.
           MOVE 0 TO WS-AW-CNT-03.
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
           PERFORM P2200-CONVERT-CENTURY THRU
               P2200-CONVERT-CENTURY-EXIT.
           PERFORM P2300-EDIT-FIELD THRU P2300-EDIT-FIELD-EXIT.
           IF WS-AW-ON-04
               PERFORM P2400-CHECK-ZONE THRU P2400-CHECK-ZONE-EXIT.
           IF WS-AW-ON-02
               PERFORM P2500-RESOLVE-SIGN THRU P2500-RESOLVE-SIGN-EXIT.
           PERFORM P2600-MATCH-SIGN THRU P2600-MATCH-SIGN-EXIT.
           PERFORM P2700-APPLY-CENTURY THRU P2700-APPLY-CENTURY-EXIT.
           PERFORM P2800-RESOLVE-SIGN THRU P2800-RESOLVE-SIGN-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ SRCIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-CONVERT-CENTURY.
           MOVE 'N' TO WS-AW-SW-01.
           IF WS-AW-TXT-06 NOT = WS-AW-TXT-04
               MOVE 'Y' TO WS-AW-SW-01
               MOVE WS-AW-TXT-06 TO WS-AW-TXT-04
               ADD 1 TO WS-AW-CNT-03.
       P2200-CONVERT-CENTURY-EXIT.
           EXIT.
       P2300-EDIT-FIELD.
           MOVE 'Y' TO WS-AW-SW-05.
           IF IA-MEDIA2 < 13
               MOVE 'N' TO WS-AW-SW-05
               ADD 1 TO WS-AW-CNT-02.
           IF IA-MEDIA2 > 9658
               MOVE 'N' TO WS-AW-SW-05
               ADD 1 TO WS-AW-CNT-01.
       P2300-EDIT-FIELD-EXIT.
           EXIT.
       P2400-CHECK-ZONE.
           MOVE 0 TO WS-AW-QTY-04.
           MOVE 0 TO WS-AW-QTY-01.
           MOVE 0 TO WS-AW-AMT-03.
           MOVE 0 TO WS-AW-AMT-01.
       P2400-CHECK-ZONE-EXIT.
           EXIT.
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT
      * PRECEDES THIS PROGRAM IN THE JOB.
       P2500-RESOLVE-SIGN.
           MOVE SPACES TO CABS-AW-OUT-RECORD.
           MOVE IA-BAN3 TO OA-SEQ.
           MOVE IA-STATUS TO OA-TARIFF.
           MOVE IA-TYPE TO OA-STATE.
           MOVE IA-BAN TO OA-TARGET.
           MOVE IA-INVOICE2 TO OA-BAND.
           MOVE IA-MEDIA2 TO OA-STATUS.
           MOVE IA-STATUS TO OA-LEVEL.
           WRITE CABS-AW-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P2500-RESOLVE-SIGN-EXIT.
           EXIT.
       P2600-MATCH-SIGN.
           MOVE IA-BAN3 TO WS-AW-TXT-06.
           MOVE IA-OCN TO WS-AW-TXT-04.
           MOVE IA-CENTRE TO WS-AW-TXT-04.
           ADD 1 TO WS-AW-CNT-02.
       P2600-MATCH-SIGN-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P2700-APPLY-CENTURY.
           IF WS-AW-AMT-04 NOT = 0
               COMPUTE WS-AW-QTY-05 = WS-AW-AMT-05 * 100 / WS-AW-AMT-04
           ELSE
               MOVE 0 TO WS-AW-QTY-05.
       P2700-APPLY-CENTURY-EXIT.
           EXIT.
       P2800-RESOLVE-SIGN.
           CALL 'CABHASH' USING IA-BAN3 WS-ACC-OCN-HASH.
           ADD WS-AW-CNT-08 TO WS-ACC-SEQ-HASH.
       P2800-RESOLVE-SIGN-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-STAGE-SIGN.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-AW-TXT-04 TO PC-COL-001-020.
           MOVE WS-AW-TXT-05 TO PC-COL-021-060.
           MOVE WS-AW-AMT-01 TO WS-AW-AMT-EDIT.
           MOVE WS-AW-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P3100-STAGE-SIGN-EXIT.
           EXIT.
       P3200-WRITE-RECORD.
           ADD IA-MEDIA2 TO WS-AW-QTY-01.
           COMPUTE WS-AW-AMT-05 ROUNDED = WS-AW-QTY-01 * WS-AW-QTY-03.
           ADD WS-AW-AMT-05 TO WS-AW-AMT-02.
       P3200-WRITE-RECORD-EXIT.
           EXIT.
       P3300-POST-ZONE.
           MOVE 0 TO WS-AW-QTY-01.
           MOVE 0 TO WS-AW-QTY-05.
           MOVE 0 TO WS-AW-QTY-03.
           MOVE 0 TO WS-AW-AMT-05.
       P3300-POST-ZONE-EXIT.
           EXIT.
       P3400-WRITE-ZONE.
           MOVE IA-SEQ TO WS-AW-TXT-03.
           MOVE IA-LEVEL2 TO WS-AW-TXT-04.
           MOVE IA-MEDIA2 TO WS-AW-TXT-03.
           ADD 1 TO WS-AW-CNT-06.
       P3400-WRITE-ZONE-EXIT.
           EXIT.
      * S400-SECONDARY-PROCESSING SECTION
       S400-SECONDARY-PROCESSING SECTION.
       P4000-SECONDARY.
           PERFORM P4100-AUDIT-TARIFF THRU P4100-AUDIT-TARIFF-EXIT.
           PERFORM P4200-ADJUST-MEDIA THRU P4200-ADJUST-MEDIA-EXIT.
       P4000-EXIT.
           EXIT.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P4100-AUDIT-TARIFF.
           MOVE 0 TO WS-AW-QTY-04.
           MOVE 0 TO WS-AW-QTY-01.
           MOVE 0 TO WS-AW-AMT-05.
           MOVE 0 TO WS-AW-AMT-03.
       P4100-AUDIT-TARIFF-EXIT.
           EXIT.
       P4200-ADJUST-MEDIA.
           MOVE 'Y' TO WS-AW-SW-05.
           IF IA-MEDIA2 < 13
               MOVE 'N' TO WS-AW-SW-05
               ADD 1 TO WS-AW-CNT-08.
           IF IA-MEDIA2 > 4175
               MOVE 'N' TO WS-AW-SW-05
               ADD 1 TO WS-AW-CNT-01.
       P4200-ADJUST-MEDIA-EXIT.
           EXIT.
           MOVE 0 TO WS-AW-QTY-04.
           PERFORM P360-WALK-LAYOUT THRU P360-WALK-LAYOUT-EXIT
               VARYING WS-AW-SUB-04 FROM 1 BY 1
               UNTIL WS-AW-SUB-04 > WS-AW-TAB-CNT.
       P360-WALK-LAYOUT.
           SET WS-AW-IX TO WS-AW-SUB-03.
           IF WS-AW-TB-KEY (WS-AW-IX) NOT = SPACES
               ADD WS-AW-TB-VAL (WS-AW-IX) TO WS-AW-QTY-04.
       P360-WALK-LAYOUT-EXIT.
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
           MOVE 'DETAIL IN' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO WS-AW-CNT-EDIT.
           MOVE WS-AW-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL OUT' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-AW-CNT-EDIT.
           MOVE WS-AW-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL SUSPENDED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-AW-CNT-EDIT.
           MOVE WS-AW-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL SUMMARISED' TO PC-COL-001-020.
           MOVE WS-SUMM-CNT TO WS-AW-CNT-EDIT.
           MOVE WS-AW-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL CARRIED FWD' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-AW-CNT-EDIT.
           MOVE WS-AW-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-AW-CNT-01 TO WS-AW-CNT-EDIT.
           MOVE WS-AW-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE WS-AW-CNT-07 TO CT-RC.
           MOVE SPACES TO CT-FILLER.
           MOVE WS-AW-TXT-03 TO CT-RESTART-KEY.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE WS-AW-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE 7 TO CT-STEP-SEQ.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
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
           CLOSE SRCIN.
           CLOSE DSPOUT.
           CLOSE CNVOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABUCV02 - END OF RUN'.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  AW-CNT-04 = ' WS-AW-CNT-04.
           DISPLAY '  AW-CNT-07 = ' WS-AW-CNT-07.
           DISPLAY '  AW-CNT-03 = ' WS-AW-CNT-03.
           DISPLAY '  AW-CNT-06 = ' WS-AW-CNT-06.
       P9000-EXIT.
           EXIT.
