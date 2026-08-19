      *****************************************************************
      * CABUCV01 - CODE PAGE AND SIGN CONVERSION                      *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               SRCIN   TELCABS.CABS.SRCIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               CNVOUT  TELCABS.CABS.CNVOUT         (LOCAL)     *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - NOTHING IS UPDATED IN PLACE        *
      * REVISION HISTORY                                              *
      *   V1.00  1990-03-26  C.ADEYEMI    INITIAL RELEASE             *
      *   V1.03  1995-04-15  D.OKONKWO    CENTURY PIVOT APPLIED TO THE*
      *                      CYCLE DATE                               *
      *   V1.06  2007-05-12  T.YAMASHITA  HASH TOTAL ADDED TO THE     *
      *                      CONTROL RECORD                           *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUCV01.
       AUTHOR. TELCABS APPLICATIONS - FILE CONVERSION.
      *****************************************************************
      * CODE PAGE AND SIGN CONVERSION. THE STEP IS SCHEDULED MONTHLY  *
      * AND ALSO RUN ON DEMAND WHEN A CENTRE ASKS FOR IT.             *
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
           SELECT SRCIN ASSIGN TO UT-S-SRCIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT CNVOUT ASSIGN TO UT-S-CNVOUT
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
      * SRCIN - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  SRCIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-EF-IN-RECORD.
           05  IE-INVOICE                  PIC S9(07) COMP-3.
           05  IE-LEVEL                    PIC X(13).
           05  IE-TYPE                     PIC X(02).
           05  IE-OCN                      PIC X(06).
           05  IE-SEGMENT                  PIC X(16).
           05  IE-INVOICE2                 PIC 9(07).
           05  IE-JURIS                    PIC S9(11) COMP-3.
           05  IE-SEGMENT2                 PIC 9(02).
           05  EF-FILL-01                  PIC X(24).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-EF-VIEW1 REDEFINES CABS-EF-IN-RECORD.
           05  R0E-CODE                    PIC S9(07)V9(02) COMP-3.
           05  R0E-CLASS                   PIC 9(03).
           05  R0E-CYCLE                   PIC S9(07)V9(05) COMP-3.
           05  R0E-BAN                     PIC 9(06).
           05  R0E-SEQ                     PIC X(08).
           05  R0E-OCN                     PIC X(20).
           05  R0E-REST                    PIC X(31).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-EF-VIEW2 REDEFINES CABS-EF-IN-RECORD.
           05  R1E-STATE                   PIC X(20).
           05  R1E-CODE                    PIC S9(13)V9(02) COMP-3.
           05  R1E-SEQ                     PIC S9(09) COMP-3.
           05  R1E-JURIS                   PIC S9(11)V9(05) COMP-3.
           05  R1E-CARRIER                 PIC X(13).
           05  R1E-JURIS2                  PIC S9(11)V9(05) COMP-3.
           05  R1E-REST                    PIC X(16).
      * CNVOUT - CATALOGUED GENERATION DATA GROUP.
       FD  CNVOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-EF-OUT-RECORD.
           05  OE-SEGMENT                  PIC S9(13) COMP-3.
           05  OE-SEGMENT2                 PIC 9(04).
           05  OE-SEQ                      PIC S9(13) COMP-3.
           05  OE-LEVEL                    PIC S9(07)V9(05) COMP-3.
           05  OE-STATE                    PIC 9(05).
           05  OE-OCN                      PIC X(13).
           05  OE-JURIS                    PIC 9(09).
           05  OE-BAN                      PIC X(02).
           05  OE-OCN2                     PIC S9(09)V9(05) COMP-3.
           05  OE-MEDIA                    PIC 9(06).
           05  OE-STATE2                   PIC X(03).
           05  EF-FILL-02                  PIC X(9).
      * SUSOUT - SEQUENTIAL, BLOCKED BY THE SYSTEM.
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
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUCV01'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V1.08'.
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
           05  WS-EF-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-EF-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-EF-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-EF-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-EF-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-EF-CNT-06                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-EF-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-EF-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-EF-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-EF-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-EF-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-EF-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-EF-TXT-01                PIC X(20) VALUE SPACES.
           05  WS-EF-TXT-02                PIC X(30) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-EF-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-EF-ON-01                 VALUE 'Y'.
               88  WS-EF-OFF-01                VALUE 'N'.
           05  WS-EF-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-EF-ON-02                 VALUE 'Y'.
               88  WS-EF-OFF-02                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-EF-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-EF-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-EF-SUB-03                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. LOADED ONCE AT INITIALISATION AND SEARCHED WITH
      * A SUBSCRIPTED WALK.
       01  WS-EF-TABLE.
           05  WS-EF-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-EF-TB-ENTRY OCCURS 150 TIMES
                                       INDEXED BY WS-EF-IX.
               10  WS-EF-TB-KEY                PIC X(10).
               10  WS-EF-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-EF-TB-TXT                PIC X(20).
               10  WS-EF-TB-EFF                PIC 9(05).
               10  WS-EF-TB-EXP                PIC 9(05).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 50.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUCV01 - CODE PAGE AND SIGN CONVERSION'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-EF-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-EF-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9981.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-EF-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-EF-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
               MOVE 'SRCIN NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               DISPLAY 'SRCIN FILE STATUS = ' WS-FS-INPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CNVOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CNVOUT NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               DISPLAY 'CNVOUT FILE STATUS = ' WS-FS-OUTPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SUSOUT.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'SUSOUT NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               DISPLAY 'SUSOUT FILE STATUS = ' WS-FS-SUSPENSE
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CTLOUT NOT AVAILABLE - OPEN REJECTED' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-EF-CYCLE-YYDDD.
           COMPUTE WS-EF-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-EF-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-EF-CNT-02.
           MOVE 0 TO WS-EF-CNT-01.
           MOVE 0 TO WS-EF-CNT-05.
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
           PERFORM P2200-BUILD-SIGN THRU P2200-BUILD-SIGN-EXIT.
           IF WS-EF-ON-01
               PERFORM P2300-BUILD-SIGN THRU P2300-BUILD-SIGN-EXIT.
           PERFORM P2400-MATCH-PACKED THRU P2400-MATCH-PACKED-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ SRCIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P2200-BUILD-SIGN.
           IF WS-EF-AMT-02 NOT = 0
               COMPUTE WS-EF-QTY-02 = WS-EF-AMT-02 * 100 / WS-EF-AMT-02
           ELSE
               MOVE 0 TO WS-EF-QTY-02.
       P2200-BUILD-SIGN-EXIT.
           EXIT.
       P2300-BUILD-SIGN.
           MOVE 0 TO WS-EF-CNT-03.
           INSPECT WS-EF-TXT-01 TALLYING WS-EF-CNT-03
               FOR ALL SPACES.
           INSPECT WS-EF-TXT-01 REPLACING ALL LOW-VALUES BY SPACES.
       P2300-BUILD-SIGN-EXIT.
           EXIT.
       P2400-MATCH-PACKED.
           MOVE SPACES TO CABS-EF-OUT-RECORD.
           MOVE IE-SEGMENT2 TO OE-SEGMENT.
           MOVE IE-SEGMENT TO OE-SEGMENT2.
           MOVE IE-TYPE TO OE-SEQ.
           MOVE IE-LEVEL TO OE-LEVEL.
           MOVE IE-OCN TO OE-STATE.
           MOVE IE-LEVEL TO OE-OCN.
           MOVE IE-JURIS TO OE-JURIS.
           MOVE IE-TYPE TO OE-BAN.
           WRITE CABS-EF-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P2400-MATCH-PACKED-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-EMIT-FIELD.
           ADD IE-INVOICE TO WS-EF-QTY-02.
           COMPUTE WS-EF-AMT-02 ROUNDED = WS-EF-QTY-02 * WS-EF-QTY-02.
           ADD WS-EF-AMT-02 TO WS-EF-AMT-03.
       P3100-EMIT-FIELD-EXIT.
           EXIT.
       P3200-EMIT-FIELD.
           MOVE 0 TO WS-EF-QTY-01.
           MOVE 0 TO WS-EF-QTY-02.
           MOVE 0 TO WS-EF-AMT-01.
       P3200-EMIT-FIELD-EXIT.
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
           MOVE SPACES TO CT-FILLER.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE 9 TO CT-STEP-SEQ.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE WS-EF-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
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
           CLOSE CNVOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABUCV01 - NORMAL END OF JOB'.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  EF-CNT-02 = ' WS-EF-CNT-02.
           DISPLAY '  EF-CNT-01 = ' WS-EF-CNT-01.
       P9000-EXIT.
           EXIT.
