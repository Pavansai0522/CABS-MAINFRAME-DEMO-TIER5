      *****************************************************************
      * CABUEX23 - ADJUSTMENT EXTRACT FOR THE GENERAL LEDGER          *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               EXTIN   TELCABS.CABS.EXTIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               SELOUT  TELCABS.CABS.SELOUT         (LOCAL)     *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * REVISION HISTORY                                              *
      *   V1.00  1992-08-22  G.PRZYBYLSKI INITIAL RELEASE             *
      *   V1.02  2003-01-24  C.ADEYEMI    OCCURS RAISED AFTER THE     *
      *                      FEBRUARY OVERFLOW                        *
      *   V1.04  2011-03-26  G.PRZYBYLSKI SUSPENSE WRITE ADDED -      *
      *                      RECORDS WERE BEING DROPPED               *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUEX23.
       AUTHOR. TELCABS APPLICATIONS - DATASET EXTRACT.
      *****************************************************************
      * ADJUSTMENT EXTRACT FOR THE GENERAL LEDGER. THE STEP IS DRIVEN *
      * ENTIRELY FROM THE SYSIN PARM CARD AND THE DD ALLOCATIONS IN   *
      * THE JOB.                                                      *
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
           SELECT EXTIN ASSIGN TO UT-S-EXTIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT SELOUT ASSIGN TO UT-S-SELOUT
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
      * EXTIN - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  EXTIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-AK-IN-RECORD.
           05  IA-STATUS                   PIC S9(05) COMP-3.
           05  IA-LEVEL                    PIC 9(02).
           05  IA-BAND                     PIC 9(02).
           05  IA-SEQ                      PIC S9(15) COMP-3.
           05  IA-OCN                      PIC 9(04).
           05  IA-OCN2                     PIC S9(11)V9(02) COMP-3.
           05  IA-BAND2                    PIC 9(09).
           05  IA-OCN3                     PIC X(03).
           05  IA-MEDIA                    PIC X(02).
           05  IA-SEGMENT                  PIC X(10).
           05  IA-TARGET                   PIC 9(05).
           05  IA-INVOICE                  PIC 9(06).
           05  AK-FILL-01                  PIC X(19).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-AK-VIEW1 REDEFINES CABS-AK-IN-RECORD.
           05  R0A-MEDIA                   PIC X(04).
           05  R0A-TARIFF                  PIC X(06).
           05  R0A-STATE                   PIC 9(03).
           05  R0A-STATUS                  PIC 9(03).
           05  R0A-ACCOUNT                 PIC S9(11)V9(05) COMP-3.
           05  R0A-TARGET                  PIC X(02).
           05  R0A-REST                    PIC X(53).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-AK-VIEW2 REDEFINES CABS-AK-IN-RECORD.
           05  R1A-CLASS                   PIC 9(04).
           05  R1A-INVOICE                 PIC 9(06).
           05  R1A-OCN                     PIC S9(15) COMP-3.
           05  R1A-MEDIA                   PIC X(10).
           05  R1A-OCN2                    PIC S9(11)V9(02) COMP-3.
           05  R1A-GROUP                   PIC X(10).
           05  R1A-GROUP2                  PIC X(03).
           05  R1A-STATUS                  PIC X(13).
           05  R1A-REST                    PIC X(19).
      * SELOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  SELOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-AK-OUT-RECORD.
           05  OA-CENTRE                   PIC S9(07)V9(05) COMP-3.
           05  OA-CODE                     PIC X(06).
           05  OA-GROUP                    PIC X(04).
           05  OA-CLASS                    PIC X(03).
           05  OA-TARGET                   PIC X(16).
           05  OA-BAN                      PIC S9(07)V9(05) COMP-3.
           05  OA-BAN2                     PIC X(16).
           05  OA-TARGET2                  PIC X(16).
           05  AK-FILL-02                  PIC X(5).
      * SUSOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
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
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUEX23'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V3.22'.
           05  WS-TABLE-LIMIT              PIC S9(04) COMP-3
                                         VALUE 100.
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
           05  WS-AK-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AK-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AK-CNT-03                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-AK-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AK-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AK-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-AK-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AK-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AK-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AK-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-AK-TXT-01                PIC X(26) VALUE SPACES.
           05  WS-AK-TXT-02                PIC X(30) VALUE SPACES.
           05  WS-AK-TXT-03                PIC X(08) VALUE SPACES.
           05  WS-AK-TXT-04                PIC X(16) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-AK-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-AK-ON-01                 VALUE 'Y'.
               88  WS-AK-OFF-01                VALUE 'N'.
           05  WS-AK-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-AK-ON-02                 VALUE 'Y'.
               88  WS-AK-OFF-02                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-AK-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-AK-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-AK-SUB-03                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. A ROW THAT DOES NOT FIT IS COUNTED AND DROPPED
      * - THE STEP DOES NOT FAIL.
       01  WS-AK-TABLE.
           05  WS-AK-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-AK-TB-ENTRY OCCURS 100 TIMES
                                       INDEXED BY WS-AK-IX.
               10  WS-AK-TB-KEY                PIC X(06).
               10  WS-AK-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-AK-TB-TXT                PIC X(30).
               10  WS-AK-TB-EFF                PIC 9(05).
               10  WS-AK-TB-EXP                PIC 9(05).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 50.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUEX23 - ADJUSTMENT EXTRACT FOR THE GENERAL LED'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-AK-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-AK-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9986.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-AK-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-AK-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT EXTIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON EXTIN - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               DISPLAY 'EXTIN FILE STATUS = ' WS-FS-INPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SELOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON SELOUT - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               DISPLAY 'SELOUT FILE STATUS = ' WS-FS-OUTPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SUSOUT.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON SUSOUT - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               DISPLAY 'SUSOUT FILE STATUS = ' WS-FS-SUSPENSE
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON CTLOUT - CHECK THE ALLOCATION' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-AK-CYCLE-YYDDD.
           COMPUTE WS-AK-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-AK-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-AK-CNT-01.
           MOVE 0 TO WS-AK-CNT-03.
           MOVE 0 TO WS-AK-CNT-02.
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
           IF WS-AK-ON-01
               PERFORM P2200-CONVERT-SUBSET THRU
                   P2200-CONVERT-SUBSET-EXIT.
           PERFORM P2300-BUILD-CANDIDATE THRU
               P2300-BUILD-CANDIDATE-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ EXTIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P2200-CONVERT-SUBSET.
           MOVE 'Y' TO WS-AK-SW-01.
           IF IA-LEVEL < 39
               MOVE 'N' TO WS-AK-SW-01
               ADD 1 TO WS-AK-CNT-01.
           IF IA-LEVEL > 4814
               MOVE 'N' TO WS-AK-SW-01
               ADD 1 TO WS-AK-CNT-01.
       P2200-CONVERT-SUBSET-EXIT.
           EXIT.
      * A FAILURE HERE SUSPENDS THE RECORD, IT DOES NOT STOP THE STEP.
       P2300-BUILD-CANDIDATE.
           MOVE SPACES TO WS-AK-TXT-01.
           STRING IA-SEGMENT DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IA-OCN2 DELIMITED BY SIZE
               INTO WS-AK-TXT-01.
       P2300-BUILD-CANDIDATE-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-STAGE-SELECTION.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-DUP-SEQ TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-AK-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
           MOVE CABS-SUSPENSE-RECORD TO CABS-SUSOUT-RECORD.
           WRITE CABS-SUSOUT-RECORD.
       P3100-STAGE-SELECTION-EXIT.
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
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-AK-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE 5 TO CT-STEP-SEQ.
           MOVE WS-AK-TXT-04 TO CT-RESTART-KEY.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-FILLER.
           MOVE SPACES TO CT-ABEND-CD.
       P8100-EXIT.
           EXIT.
      * P8200-CHECK-BALANCE - A RECORD THAT WAS HELD FOR THE NEXT
      * CYCLE IS BROUGHT INTO THE CARRIED FORWARD SIDE BEFORE THE
      * EQUATION IS TESTED.
       P8200-CHECK-BALANCE.
           ADD WS-AK-CNT-01 TO CT-CARRIED-FWD.
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
           CLOSE EXTIN.
           CLOSE SELOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABUEX23 - STEP COMPLETE'.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  AK-CNT-01 = ' WS-AK-CNT-01.
           DISPLAY '  AK-CNT-02 = ' WS-AK-CNT-02.
           DISPLAY '  AK-CNT-03 = ' WS-AK-CNT-03.
       P9000-EXIT.
           EXIT.
