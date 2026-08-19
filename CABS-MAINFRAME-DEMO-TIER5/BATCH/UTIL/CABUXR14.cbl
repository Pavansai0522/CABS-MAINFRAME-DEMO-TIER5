      *****************************************************************
      * CABUXR14 - RATE ELEMENT TO TARIFF CROSS REFERENCE             *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               LFTIN   TELCABS.CABS.LFTIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               GRPOUT  TELCABS.CABS.GRPOUT         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - NOTHING IS UPDATED IN PLACE        *
      * REVISION HISTORY                                              *
      *   V1.00  1991-04-13  P.NAIR       INITIAL RELEASE             *
      *   V1.01  1995-10-17  R.T.WHEELER  RETIRED THE SECOND SORT STEP*
      *                      - DONE IN PROGRAM                        *
      *   V1.02  2001-09-12  C.ADEYEMI    TABLE LIMIT RAISED FOR THE  *
      *                      SOUTHEAST CENTRES                        *
      *   V1.05  2016-11-10  J.M.CASTILLO OCCURS RAISED AFTER THE     *
      *                      FEBRUARY OVERFLOW                        *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUXR14.
       AUTHOR. TELCABS APPLICATIONS - CROSS REFERENCE REPORT.
      *****************************************************************
      * RATE ELEMENT TO TARIFF CROSS REFERENCE. THE STEP IS SCHEDULED *
      * MONTHLY AND ALSO RUN ON DEMAND WHEN A CENTRE ASKS FOR IT.     *
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
           SELECT LFTIN ASSIGN TO UT-S-LFTIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT GRPOUT ASSIGN TO UT-S-GRPOUT
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
      * LFTIN - CATALOGUED GENERATION DATA GROUP.
       FD  LFTIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 150 CHARACTERS.
       01  CABS-CY-IN-RECORD.
           05  IC-CIRCUIT                  PIC X(16).
           05  IC-CLASS                    PIC X(03).
           05  IC-MEDIA                    PIC S9(11)V9(05) COMP-3.
           05  IC-CIRCUIT2                 PIC X(10).
           05  IC-STATUS                   PIC S9(07) COMP-3.
           05  IC-CARRIER                  PIC S9(11) COMP-3.
           05  IC-REGION                   PIC S9(15) COMP-3.
           05  IC-LEVEL                    PIC 9(09).
           05  IC-TARIFF                   PIC S9(11)V9(02) COMP-3.
           05  IC-REGION2                  PIC 9(04).
           05  IC-BAN                      PIC S9(07) COMP-3.
           05  IC-PERIOD                   PIC S9(07)V9(05) COMP-3.
           05  IC-ACCOUNT                  PIC S9(13)V9(02) COMP-3.
           05  IC-CENTRE                   PIC X(04).
           05  IC-BAND                     PIC X(10).
           05  IC-SEGMENT                  PIC S9(11)V9(05) COMP-3.
           05  IC-STATUS2                  PIC S9(11)V9(02) COMP-3.
           05  IC-CODE                     PIC S9(13)V9(05) COMP-3.
           05  IC-CARRIER2                 PIC 9(07).
           05  CY-FILL-01                  PIC X(8).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-CY-VIEW1 REDEFINES CABS-CY-IN-RECORD.
           05  R0C-JURIS                   PIC S9(15) COMP-3.
           05  R0C-TARGET                  PIC X(04).
           05  R0C-INVOICE                 PIC S9(11) COMP-3.
           05  R0C-CENTRE                  PIC S9(05) COMP-3.
           05  R0C-INVOICE2                PIC 9(07).
           05  R0C-REST                    PIC X(122).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-CY-VIEW2 REDEFINES CABS-CY-IN-RECORD.
           05  R1C-SEQ                     PIC S9(07)V9(02) COMP-3.
           05  R1C-SEGMENT                 PIC 9(02).
           05  R1C-CYCLE                   PIC S9(09)V9(05) COMP-3.
           05  R1C-ELEM                    PIC X(03).
           05  R1C-CENTRE                  PIC X(03).
           05  R1C-SOURCE                  PIC S9(11)V9(02) COMP-3.
           05  R1C-CODE                    PIC S9(09) COMP-3.
           05  R1C-TARIFF                  PIC X(04).
           05  R1C-REST                    PIC X(113).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-CY-VIEW3 REDEFINES CABS-CY-IN-RECORD.
           05  R2C-SEGMENT                 PIC X(10).
           05  R2C-TARIFF                  PIC S9(05) COMP-3.
           05  R2C-SOURCE                  PIC S9(13)V9(02) COMP-3.
           05  R2C-LEVEL                   PIC S9(15) COMP-3.
           05  R2C-REST                    PIC X(121).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-CY-VIEW4 REDEFINES CABS-CY-IN-RECORD.
           05  R3C-OCN                     PIC 9(03).
           05  R3C-CIRCUIT                 PIC S9(09) COMP-3.
           05  R3C-CARRIER                 PIC S9(07) COMP-3.
           05  R3C-OCN2                    PIC 9(09).
           05  R3C-PERIOD                  PIC X(08).
           05  R3C-SEQ                     PIC X(08).
           05  R3C-SEGMENT                 PIC X(08).
           05  R3C-REST                    PIC X(105).
      * GRPOUT - PERMANENT DATASET HELD ON DASD.
       FD  GRPOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-CY-OUT-RECORD.
           05  OC-OCN                      PIC S9(15) COMP-3.
           05  OC-OCN2                     PIC S9(11)V9(02) COMP-3.
           05  OC-JURIS                    PIC S9(11)V9(05) COMP-3.
           05  OC-INVOICE                  PIC X(08).
           05  OC-OCN3                     PIC X(10).
           05  OC-CIRCUIT                  PIC 9(06).
           05  OC-INVOICE2                 PIC S9(11)V9(02) COMP-3.
           05  OC-SEGMENT                  PIC 9(06).
           05  OC-LEVEL                    PIC S9(09)V9(02) COMP-3.
           05  OC-LEVEL2                   PIC X(03).
           05  OC-CODE                     PIC X(03).
           05  CY-FILL-02                  PIC X(7).
      * SUSOUT - WORK FILE, DELETED AT STEP END.
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
      * SHARED LAYOUT PULLED IN FOR THE SIDE SIDE.
       COPY CABSCOMM.
      * SHARED LAYOUT PULLED IN FOR THE LINK SIDE.
       COPY CABSBHDR.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUXR14'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V2.10'.
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
           05  WS-CY-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CY-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CY-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CY-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CY-CNT-05                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-CY-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CY-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CY-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-CY-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CY-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CY-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-CY-TXT-01                PIC X(10) VALUE SPACES.
           05  WS-CY-TXT-02                PIC X(08) VALUE SPACES.
           05  WS-CY-TXT-03                PIC X(08) VALUE SPACES.
           05  WS-CY-TXT-04                PIC X(26) VALUE SPACES.
           05  WS-CY-TXT-05                PIC X(08) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-CY-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-CY-ON-01                 VALUE 'Y'.
               88  WS-CY-OFF-01                VALUE 'N'.
           05  WS-CY-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-CY-ON-02                 VALUE 'Y'.
               88  WS-CY-OFF-02                VALUE 'N'.
           05  WS-CY-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-CY-ON-03                 VALUE 'Y'.
               88  WS-CY-OFF-03                VALUE 'N'.
           05  WS-CY-SW-04                 PIC X(01) VALUE 'N'.
               88  WS-CY-ON-04                 VALUE 'Y'.
               88  WS-CY-OFF-04                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-CY-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CY-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CY-SUB-03                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CY-SUB-04                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. A ROW THAT DOES NOT FIT IS COUNTED AND DROPPED
      * - THE STEP DOES NOT FAIL.
       01  WS-CY-TABLE.
           05  WS-CY-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-CY-TB-ENTRY OCCURS 250 TIMES
                                       INDEXED BY WS-CY-IX.
               10  WS-CY-TB-KEY                PIC X(10).
               10  WS-CY-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-CY-TB-TXT                PIC X(20).
               10  WS-CY-TB-EFF                PIC 9(05).
               10  WS-CY-TB-EXP                PIC 9(05).
       01  WS-CY-WORK-GROUP-1.
           05  WS-CY-G1-TARGET             PIC 9(05).
           05  WS-CY-G1-BAN                PIC X(20).
           05  WS-CY-G1-INVOICE            PIC X(20).
           05  WS-CY-G1-CLASS              PIC S9(09) COMP-3.
           05  WS-CY-G1-CYCLE              PIC X(20).
           05  WS-CY-G1-LEVEL              PIC S9(11)V9(02) COMP-3.
           05  WS-CY-G1-JURIS              PIC 9(07).
       01  WS-CY-WORK-GROUP-2.
           05  WS-CY-G2-SEQ                PIC S9(11)V9(02) COMP-3.
           05  WS-CY-G2-ACCOUNT            PIC X(10).
           05  WS-CY-G2-CYCLE              PIC 9(05).
           05  WS-CY-G2-ACCOUNT            PIC X(20).
           05  WS-CY-G2-CIRCUIT            PIC S9(11)V9(02) COMP-3.
           05  WS-CY-G2-CARRIER            PIC S9(11)V9(02) COMP-3.
       01  WS-CY-WORK-GROUP-3.
           05  WS-CY-G3-OCN                PIC 9(05).
           05  WS-CY-G3-CARRIER            PIC X(10).
           05  WS-CY-G3-CENTRE             PIC 9(07).
           05  WS-CY-G3-OCN                PIC X(20).
           05  WS-CY-G3-STATUS             PIC X(10).
           05  WS-CY-G3-GROUP              PIC S9(11)V9(02) COMP-3.
           05  WS-CY-G3-STATE              PIC X(10).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 60.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUXR14 - RATE ELEMENT TO TARIFF CROSS REFERENCE'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-CY-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-CY-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9946.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-CY-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-CY-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT LFTIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF LFTIN' TO
                   WS-AB-REASON
               DISPLAY 'LFTIN FILE STATUS = ' WS-FS-INPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT GRPOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF GRPOUT' TO
                   WS-AB-REASON
               DISPLAY 'GRPOUT FILE STATUS = ' WS-FS-OUTPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SUSOUT.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF SUSOUT' TO
                   WS-AB-REASON
               DISPLAY 'SUSOUT FILE STATUS = ' WS-FS-SUSPENSE
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF CTLOUT' TO
                   WS-AB-REASON
               DISPLAY 'CTLOUT FILE STATUS = ' WS-FS-CONTROL
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RPTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF RPTOUT' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-CY-CYCLE-YYDDD.
           COMPUTE WS-CY-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-CY-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-CY-CNT-02.
           MOVE 0 TO WS-CY-CNT-04.
           MOVE 0 TO WS-CY-CNT-01.
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
           PERFORM P2200-SELECT-PAIR THRU P2200-SELECT-PAIR-EXIT.
           IF WS-CY-ON-01
               PERFORM P2300-VALIDATE-MATCH THRU
                   P2300-VALIDATE-MATCH-EXIT.
           PERFORM P2400-SELECT-PAIR THRU P2400-SELECT-PAIR-EXIT.
           IF WS-CY-ON-04
               PERFORM P2500-MATCH-SIDE THRU P2500-MATCH-SIDE-EXIT.
           PERFORM P2600-EXPAND-LINK THRU P2600-EXPAND-LINK-EXIT.
           IF WS-CY-ON-01
               PERFORM P2700-BUILD-MATCH THRU P2700-BUILD-MATCH-EXIT.
           IF WS-CY-ON-04
               PERFORM P2800-SELECT-REFERENCE THRU
                   P2800-SELECT-REFERENCE-EXIT.
           PERFORM P2900-CONVERT-MATCH THRU P2900-CONVERT-MATCH-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ LFTIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P2200-SELECT-PAIR.
           MOVE 'Y' TO WS-CY-SW-01.
           IF IC-MEDIA < 18
               MOVE 'N' TO WS-CY-SW-01
               ADD 1 TO WS-CY-CNT-05.
           IF IC-MEDIA > 6780
               MOVE 'N' TO WS-CY-SW-01
               ADD 1 TO WS-CY-CNT-02.
       P2200-SELECT-PAIR-EXIT.
           EXIT.
       P2300-VALIDATE-MATCH.
           MOVE SPACES TO WS-CY-TXT-05.
           STRING IC-REGION DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IC-CIRCUIT2 DELIMITED BY SIZE
               INTO WS-CY-TXT-05.
       P2300-VALIDATE-MATCH-EXIT.
           EXIT.
      * A FAILURE HERE SUSPENDS THE RECORD, IT DOES NOT STOP THE STEP.
       P2400-SELECT-PAIR.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-CY-TXT-03 TO PC-COL-001-020.
           MOVE WS-CY-TXT-04 TO PC-COL-021-060.
           MOVE WS-CY-AMT-02 TO WS-CY-AMT-EDIT.
           MOVE WS-CY-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P2400-SELECT-PAIR-EXIT.
           EXIT.
       P2500-MATCH-SIDE.
           ADD IC-REGION TO WS-CY-QTY-02.
           COMPUTE WS-CY-AMT-03 = WS-CY-QTY-02 * WS-CY-QTY-01.
           ADD WS-CY-AMT-03 TO WS-CY-AMT-02.
       P2500-MATCH-SIDE-EXIT.
           EXIT.
       P2600-EXPAND-LINK.
           MOVE 0 TO WS-CY-CNT-01.
           INSPECT WS-CY-TXT-03 TALLYING WS-CY-CNT-01
               FOR ALL SPACES.
           INSPECT WS-CY-TXT-03 REPLACING ALL LOW-VALUES BY SPACES.
       P2600-EXPAND-LINK-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P2700-BUILD-MATCH.
           MOVE IC-CARRIER2 TO WS-CY-TXT-05.
           MOVE IC-TARIFF TO WS-CY-TXT-02.
           MOVE IC-REGION2 TO WS-CY-TXT-04.
           MOVE IC-PERIOD TO WS-CY-TXT-01.
           ADD 1 TO WS-CY-CNT-03.
       P2700-BUILD-MATCH-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P2800-SELECT-REFERENCE.
           MOVE 0 TO WS-CY-QTY-02.
           MOVE 0 TO WS-CY-QTY-03.
           MOVE 0 TO WS-CY-AMT-02.
           MOVE 0 TO WS-CY-AMT-01.
       P2800-SELECT-REFERENCE-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P2900-CONVERT-MATCH.
           MOVE 'N' TO WS-CY-SW-03.
           IF WS-CY-TXT-04 NOT = WS-CY-TXT-05
               MOVE 'Y' TO WS-CY-SW-03
               MOVE WS-CY-TXT-04 TO WS-CY-TXT-05
               ADD 1 TO WS-CY-CNT-03.
       P2900-CONVERT-MATCH-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-EMIT-ORPHAN.
           MOVE IC-REGION TO WS-CY-TXT-04.
           MOVE IC-CLASS TO WS-CY-TXT-04.
           MOVE IC-CLASS TO WS-CY-TXT-05.
           ADD 1 TO WS-CY-CNT-01.
       P3100-EMIT-ORPHAN-EXIT.
           EXIT.
       P3200-CLOSE-OFF-MATCH.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-RATE-NOT-FOUND TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-CY-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
           MOVE CABS-SUSPENSE-RECORD TO CABS-SUSOUT-RECORD.
           WRITE CABS-SUSOUT-RECORD.
       P3200-CLOSE-OFF-MATCH-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P3300-FORMAT-LINK.
           MOVE SPACES TO WS-CY-TXT-05.
           STRING IC-LEVEL DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IC-STATUS2 DELIMITED BY SIZE
               INTO WS-CY-TXT-05.
       P3300-FORMAT-LINK-EXIT.
           EXIT.
       P3400-EMIT-LINK.
           MOVE 0 TO WS-CY-QTY-03.
           MOVE 0 TO WS-CY-QTY-01.
           MOVE 0 TO WS-CY-AMT-03.
       P3400-EMIT-LINK-EXIT.
           EXIT.
      * S400-SECONDARY-PROCESSING SECTION
       S400-SECONDARY-PROCESSING SECTION.
       P4000-SECONDARY.
           PERFORM P4100-RECONCILE-MATCH THRU
               P4100-RECONCILE-MATCH-EXIT.
           PERFORM P4200-ADJUST-LINK THRU P4200-ADJUST-LINK-EXIT.
       P4000-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P4100-RECONCILE-MATCH.
           CALL 'CABEDITF' USING WS-CY-TXT-04 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-CY-CNT-02.
       P4100-RECONCILE-MATCH-EXIT.
           EXIT.
       P4200-ADJUST-LINK.
           MOVE SPACES TO WS-CY-TXT-01.
           STRING IC-STATUS DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IC-REGION2 DELIMITED BY SIZE
               INTO WS-CY-TXT-01.
       P4200-ADJUST-LINK-EXIT.
           EXIT.
           MOVE 0 TO WS-CY-QTY-02.
           PERFORM P350-WALK-PAIR THRU P350-WALK-PAIR-EXIT
               VARYING WS-CY-SUB-02 FROM 1 BY 1
               UNTIL WS-CY-SUB-02 > WS-CY-TAB-CNT.
       P350-WALK-PAIR.
           SET WS-CY-IX TO WS-CY-SUB-01.
           IF WS-CY-TB-KEY (WS-CY-IX) NOT = SPACES
               ADD WS-CY-TB-VAL (WS-CY-IX) TO WS-CY-QTY-03.
       P350-WALK-PAIR-EXIT.
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
           MOVE 'DETAIL SUMMARISED' TO PC-COL-001-020.
           MOVE WS-SUMM-CNT TO WS-CY-CNT-EDIT.
           MOVE WS-CY-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL IN' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO WS-CY-CNT-EDIT.
           MOVE WS-CY-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL OUT' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-CY-CNT-EDIT.
           MOVE WS-CY-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL SUSPENDED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-CY-CNT-EDIT.
           MOVE WS-CY-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL CARRIED FWD' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-CY-CNT-EDIT.
           MOVE WS-CY-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-CY-CNT-01 TO WS-CY-CNT-EDIT.
           MOVE WS-CY-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE WS-CY-TXT-01 TO CT-RESTART-KEY.
           MOVE SPACES TO CT-FILLER.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE 9 TO CT-STEP-SEQ.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE WS-CY-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
       P8100-EXIT.
           EXIT.
      * P8200-CHECK-BALANCE - A RECORD THAT WAS HELD FOR THE NEXT
      * CYCLE IS BROUGHT INTO THE CARRIED FORWARD SIDE BEFORE THE
      * EQUATION IS TESTED.
       P8200-CHECK-BALANCE.
           ADD WS-CY-CNT-04 TO CT-CARRIED-FWD.
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
           CLOSE LFTIN.
           CLOSE GRPOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABUXR14 - NORMAL END OF JOB'.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  CY-CNT-04 = ' WS-CY-CNT-04.
           DISPLAY '  CY-CNT-02 = ' WS-CY-CNT-02.
       P9000-EXIT.
           EXIT.
