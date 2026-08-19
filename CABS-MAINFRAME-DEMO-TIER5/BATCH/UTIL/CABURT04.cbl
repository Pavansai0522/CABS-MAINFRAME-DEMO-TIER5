      *****************************************************************
      * CABURT04 - BAND TABLE MAINTENANCE                             *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               RATIN   TELCABS.CABS.RATIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               ELMOUT  TELCABS.CABS.ELMOUT         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - THE OUTPUT IS REBUILT EACH RUN     *
      * REVISION HISTORY                                              *
      *   V1.00  2004-02-17  P.NAIR       INITIAL RELEASE             *
      *   V1.02  2009-12-25  J.M.CASTILLO PARM CARD EXTENDED,         *
      *                      POSITIONS 40 THROUGH 48                  *
      *   V1.03  2017-02-16  T.YAMASHITA  REPORT PAGINATION CORRECTED *
      *   V1.06  2018-08-01  M.DELACROIX  RECOMPILE ONLY - COPYBOOK   *
      *                      CHANGE UPSTREAM                          *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABURT04.
       AUTHOR. TELCABS APPLICATIONS - RATE TABLE MAINTENANCE.
      *****************************************************************
      * BAND TABLE MAINTENANCE. THE STEP RUNS ONCE PER BILL CYCLE AND *
      * IS RERUN FROM THE TOP IF IT FAILS.                            *
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES     *
      * RATHER THAN LOW VALUES.                                       *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           C01 IS TO-NEW-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RATIN ASSIGN TO UT-S-RATIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT ELMOUT ASSIGN TO UT-S-ELMOUT
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
      * RATIN - WORK FILE, DELETED AT STEP END.
       FD  RATIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 110 CHARACTERS.
       01  CABS-DN-IN-RECORD.
           05  ID-CYCLE                    PIC X(02).
           05  ID-OCN                      PIC 9(07).
           05  ID-SEQ                      PIC X(02).
           05  ID-MEDIA                    PIC S9(15) COMP-3.
           05  ID-MEDIA2                   PIC X(20).
           05  ID-OCN2                     PIC X(02).
           05  ID-CARRIER                  PIC S9(09)V9(02) COMP-3.
           05  ID-TYPE                     PIC X(03).
           05  ID-BAN                      PIC X(02).
           05  ID-BAND                     PIC 9(09).
           05  ID-PERIOD                   PIC S9(11) COMP-3.
           05  ID-ACCOUNT                  PIC X(03).
           05  ID-CARRIER2                 PIC X(08).
           05  ID-BAN2                     PIC S9(11) COMP-3.
           05  ID-CENTRE                   PIC 9(03).
           05  ID-ELEM                     PIC 9(03).
           05  ID-LEVEL                    PIC S9(13) COMP-3.
           05  ID-PERIOD2                  PIC 9(02).
           05  ID-LEVEL2                   PIC X(10).
           05  DN-FILL-01                  PIC X(1).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-DN-VIEW1 REDEFINES CABS-DN-IN-RECORD.
           05  R0D-REGION                  PIC X(10).
           05  R0D-LEVEL                   PIC X(04).
           05  R0D-MEDIA                   PIC 9(07).
           05  R0D-BAN                     PIC S9(11)V9(02) COMP-3.
           05  R0D-BAN2                    PIC S9(07)V9(02) COMP-3.
           05  R0D-ELEM                    PIC X(20).
           05  R0D-LEVEL2                  PIC S9(13) COMP-3.
           05  R0D-REST                    PIC X(50).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-DN-VIEW2 REDEFINES CABS-DN-IN-RECORD.
           05  R1D-CLASS                   PIC X(16).
           05  R1D-BAND                    PIC S9(11) COMP-3.
           05  R1D-BAN                     PIC X(16).
           05  R1D-BAN2                    PIC X(04).
           05  R1D-REST                    PIC X(68).
      * ELMOUT - CATALOGUED GENERATION DATA GROUP.
       FD  ELMOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 140 CHARACTERS.
       01  CABS-DN-OUT-RECORD.
           05  OD-SOURCE                   PIC X(04).
           05  OD-ELEM                     PIC S9(09) COMP-3.
           05  OD-BAND                     PIC X(04).
           05  OD-LEVEL                    PIC S9(05) COMP-3.
           05  OD-STATUS                   PIC 9(04).
           05  OD-CARRIER                  PIC S9(13) COMP-3.
           05  OD-GROUP                    PIC X(16).
           05  OD-STATE                    PIC S9(11)V9(02) COMP-3.
           05  OD-CIRCUIT                  PIC X(20).
           05  OD-JURIS                    PIC 9(06).
           05  OD-BAN                      PIC X(06).
           05  OD-OCN                      PIC X(10).
           05  OD-OCN2                     PIC S9(11) COMP-3.
           05  OD-CYCLE                    PIC X(10).
           05  OD-CLASS                    PIC S9(13) COMP-3.
           05  OD-CIRCUIT2                 PIC X(03).
           05  OD-JURIS2                   PIC X(10).
           05  OD-STATE2                   PIC S9(13)V9(05) COMP-3.
           05  DN-FILL-02                  PIC X(2).
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
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABURT04'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V3.30'.
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
           05  WS-DN-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DN-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DN-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DN-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DN-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DN-CNT-06                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DN-CNT-07                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DN-CNT-08                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DN-CNT-09                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-DN-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DN-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DN-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DN-QTY-04                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DN-QTY-05                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DN-QTY-06                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-DN-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DN-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DN-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DN-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-DN-TXT-01                PIC X(10) VALUE SPACES.
           05  WS-DN-TXT-02                PIC X(20) VALUE SPACES.
           05  WS-DN-TXT-03                PIC X(20) VALUE SPACES.
           05  WS-DN-TXT-04                PIC X(30) VALUE SPACES.
           05  WS-DN-TXT-05                PIC X(26) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-DN-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-DN-ON-01                 VALUE 'Y'.
               88  WS-DN-OFF-01                VALUE 'N'.
           05  WS-DN-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-DN-ON-02                 VALUE 'Y'.
               88  WS-DN-OFF-02                VALUE 'N'.
           05  WS-DN-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-DN-ON-03                 VALUE 'Y'.
               88  WS-DN-OFF-03                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-DN-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DN-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DN-SUB-03                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. LOADED ONCE AT INITIALISATION AND SEARCHED WITH
      * A SUBSCRIPTED WALK.
       01  WS-DN-TABLE.
           05  WS-DN-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-DN-TB-ENTRY OCCURS 250 TIMES
                                       INDEXED BY WS-DN-IX.
               10  WS-DN-TB-KEY                PIC X(10).
               10  WS-DN-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-DN-TB-TXT                PIC X(20).
               10  WS-DN-TB-EFF                PIC 9(05).
               10  WS-DN-TB-EXP                PIC 9(05).
       01  WS-DN-WORK-GROUP-1.
           05  WS-DN-G1-SEGMENT            PIC S9(09) COMP-3.
           05  WS-DN-G1-CYCLE              PIC S9(11)V9(02) COMP-3.
           05  WS-DN-G1-TARIFF             PIC X(20).
           05  WS-DN-G1-REGION             PIC X(10).
       01  WS-DN-WORK-GROUP-2.
           05  WS-DN-G2-OCN                PIC 9(05).
           05  WS-DN-G2-GROUP              PIC X(10).
           05  WS-DN-G2-ACCOUNT            PIC X(20).
           05  WS-DN-G2-OCN                PIC 9(05).
       01  WS-DN-WORK-GROUP-3.
           05  WS-DN-G3-INVOICE            PIC 9(07).
           05  WS-DN-G3-STATUS             PIC S9(09) COMP-3.
           05  WS-DN-G3-SEQ                PIC 9(05).
           05  WS-DN-G3-SEQ                PIC 9(05).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 60.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABURT04 - BAND TABLE MAINTENANCE'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-DN-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-DN-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9976.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-DN-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-DN-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT RATIN.
           IF WS-FS-INPUT NOT = '00'
               DISPLAY 'RATIN FILE STATUS = ' WS-FS-INPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF RATIN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT ELMOUT.
           IF WS-FS-OUTPUT NOT = '00'
               DISPLAY 'ELMOUT FILE STATUS = ' WS-FS-OUTPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF ELMOUT' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-DN-CYCLE-YYDDD.
           COMPUTE WS-DN-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-DN-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-DN-CNT-05.
           MOVE 0 TO WS-DN-CNT-07.
           MOVE 0 TO WS-DN-CNT-03.
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
           PERFORM P2200-CHECK-WINDOW THRU P2200-CHECK-WINDOW-EXIT.
           IF WS-DN-ON-03
               PERFORM P2300-SELECT-DESCRIPTION THRU
                   P2300-SELECT-DESCRIPTION-EXIT.
           IF WS-DN-ON-02
               PERFORM P2400-APPLY-TARIFF THRU P2400-APPLY-TARIFF-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ RATIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-CHECK-WINDOW.
           MOVE SPACES TO CABS-DN-OUT-RECORD.
           MOVE ID-BAN2 TO OD-SOURCE.
           MOVE ID-PERIOD TO OD-ELEM.
           MOVE ID-CYCLE TO OD-BAND.
           MOVE ID-PERIOD2 TO OD-LEVEL.
           MOVE ID-BAND TO OD-STATUS.
           WRITE CABS-DN-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P2200-CHECK-WINDOW-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P2300-SELECT-DESCRIPTION.
           MOVE 0 TO WS-DN-CNT-07.
           INSPECT WS-DN-TXT-03 TALLYING WS-DN-CNT-07
               FOR ALL SPACES.
           INSPECT WS-DN-TXT-03 REPLACING ALL LOW-VALUES BY SPACES.
       P2300-SELECT-DESCRIPTION-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P2400-APPLY-TARIFF.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-BAN-UNKNOWN TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-DN-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
       P2400-APPLY-TARIFF-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P3100-WRITE-WINDOW.
           MOVE 0 TO WS-DN-QTY-05.
           MOVE 0 TO WS-DN-QTY-01.
           MOVE 0 TO WS-DN-AMT-03.
       P3100-WRITE-WINDOW-EXIT.
           EXIT.
       P3200-RELEASE-BAND.
           ADD ID-PERIOD TO WS-DN-QTY-02.
           COMPUTE WS-DN-AMT-02 = WS-DN-QTY-02 * WS-DN-QTY-06.
           ADD WS-DN-AMT-02 TO WS-DN-AMT-03.
       P3200-RELEASE-BAND-EXIT.
           EXIT.
       P3300-POST-DESCRIPTION.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-DN-TXT-03 TO PC-COL-001-020.
           MOVE WS-DN-TXT-03 TO PC-COL-021-060.
           MOVE WS-DN-AMT-01 TO WS-DN-AMT-EDIT.
           MOVE WS-DN-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P3300-POST-DESCRIPTION-EXIT.
           EXIT.
       P3400-STAGE-ELEMENT.
           MOVE SPACES TO CABS-DN-OUT-RECORD.
           MOVE ID-PERIOD2 TO OD-SOURCE.
           MOVE ID-BAN TO OD-ELEM.
           MOVE ID-CYCLE TO OD-BAND.
           MOVE ID-CARRIER TO OD-LEVEL.
           MOVE ID-ELEM TO OD-STATUS.
           MOVE ID-ELEM TO OD-CARRIER.
           MOVE ID-ELEM TO OD-GROUP.
           MOVE ID-OCN2 TO OD-STATE.
           MOVE ID-PERIOD2 TO OD-CIRCUIT.
           WRITE CABS-DN-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P3400-STAGE-ELEMENT-EXIT.
           EXIT.
      * S800-CONTROL SECTION - THE MANDATORY CABS CONTROL BOUNDARY.
       S800-CONTROL SECTION.
       P8000-CONTROL.
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
           MOVE 'SUMMARISED' TO PC-COL-001-020.
           MOVE WS-SUMM-CNT TO WS-DN-CNT-EDIT.
           MOVE WS-DN-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'HELD FOR NEXT RUN' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-DN-CNT-EDIT.
           MOVE WS-DN-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'INPUT RECORDS' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO WS-DN-CNT-EDIT.
           MOVE WS-DN-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'OUTPUT RECORDS' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-DN-CNT-EDIT.
           MOVE WS-DN-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'SUSPENDED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-DN-CNT-EDIT.
           MOVE WS-DN-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-DN-CNT-01 TO WS-DN-CNT-EDIT.
           MOVE WS-DN-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 02' TO PC-COL-001-020.
           MOVE WS-DN-CNT-02 TO WS-DN-CNT-EDIT.
           MOVE WS-DN-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 03' TO PC-COL-001-020.
           MOVE WS-DN-CNT-03 TO WS-DN-CNT-EDIT.
           MOVE WS-DN-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 04' TO PC-COL-001-020.
           MOVE WS-DN-CNT-04 TO WS-DN-CNT-EDIT.
           MOVE WS-DN-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE 6 TO CT-STEP-SEQ.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE WS-DN-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE SPACES TO CT-FILLER.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE SPACES TO CT-RESTART-KEY.
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
           CLOSE RATIN.
           CLOSE ELMOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABURT04 - STEP COMPLETE'.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  DN-CNT-01 = ' WS-DN-CNT-01.
           DISPLAY '  DN-CNT-08 = ' WS-DN-CNT-08.
           DISPLAY '  DN-CNT-07 = ' WS-DN-CNT-07.
           DISPLAY '  DN-CNT-02 = ' WS-DN-CNT-02.
       P9000-EXIT.
           EXIT.
