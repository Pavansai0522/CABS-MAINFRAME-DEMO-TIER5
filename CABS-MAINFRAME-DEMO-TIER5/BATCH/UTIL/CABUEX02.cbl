      *****************************************************************
      * CABUEX02 - CIRCUIT INVENTORY EXTRACT                          *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               CIRIN   TELCABS.CABS.CIRIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               EXTOUT  TELCABS.CABS.EXTOUT         (LOCAL)     *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * REVISION HISTORY                                              *
      *   V1.00  1992-06-26  P.NAIR       INITIAL RELEASE             *
      *   V1.02  1993-03-14  C.ADEYEMI    HASH TOTAL ADDED TO THE     *
      *                      CONTROL RECORD                           *
      *   V1.04  1998-07-10  M.DELACROIX  CARRIER TYPE BROUGHT ONTO   *
      *                      THE EXTRACT                              *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUEX02.
       AUTHOR. TELCABS APPLICATIONS - DATASET EXTRACT.
      *****************************************************************
      * CIRCUIT INVENTORY EXTRACT. THE STEP IS DRIVEN ENTIRELY FROM   *
      * THE SYSIN PARM CARD AND THE DD ALLOCATIONS IN THE JOB.        *
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
           SELECT CIRIN ASSIGN TO UT-S-CIRIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT EXTOUT ASSIGN TO UT-S-EXTOUT
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
      * CIRIN - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  CIRIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-DH-IN-RECORD.
           05  ID-CARRIER                  PIC 9(02).
           05  ID-STATUS                   PIC S9(11)V9(02) COMP-3.
           05  ID-CYCLE                    PIC S9(11)V9(05) COMP-3.
           05  ID-PERIOD                   PIC X(02).
           05  ID-SOURCE                   PIC S9(11) COMP-3.
           05  ID-BAN                      PIC X(03).
           05  ID-TYPE                     PIC X(04).
           05  ID-GROUP                    PIC 9(05).
           05  ID-STATE                    PIC S9(11) COMP-3.
           05  ID-BAN2                     PIC 9(05).
           05  DH-FILL-01                  PIC X(31).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-DH-VIEW1 REDEFINES CABS-DH-IN-RECORD.
           05  R0D-PERIOD                  PIC X(06).
           05  R0D-INVOICE                 PIC X(13).
           05  R0D-PERIOD2                 PIC X(06).
           05  R0D-CODE                    PIC 9(06).
           05  R0D-MEDIA                   PIC X(04).
           05  R0D-TYPE                    PIC S9(07)V9(05) COMP-3.
           05  R0D-REST                    PIC X(38).
      * EXTOUT - CATALOGUED GENERATION DATA GROUP.
       FD  EXTOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 90 CHARACTERS.
       01  CABS-DH-OUT-RECORD.
           05  OD-CENTRE                   PIC X(20).
           05  OD-REGION                   PIC X(02).
           05  OD-STATUS                   PIC 9(06).
           05  OD-BAN                      PIC S9(15) COMP-3.
           05  OD-INVOICE                  PIC X(10).
           05  OD-BAN2                     PIC 9(06).
           05  OD-CLASS                    PIC X(10).
           05  OD-BAN3                     PIC S9(09)V9(02) COMP-3.
           05  OD-GROUP                    PIC S9(11)V9(05) COMP-3.
           05  OD-PERIOD                   PIC S9(05) COMP-3.
           05  OD-TARGET                   PIC X(08).
           05  DH-FILL-02                  PIC X(2).
      * SUSOUT - WORK FILE, DELETED AT STEP END.
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
      * SHARED LAYOUT PULLED IN FOR THE EXTRACT SIDE.
       COPY CABSCARR.
      * SHARED LAYOUT PULLED IN FOR THE MASTER SIDE.
       COPY CABSCDR.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUEX02'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V1.09'.
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
           05  WS-DH-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DH-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DH-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DH-CNT-04                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-DH-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DH-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DH-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DH-QTY-04                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-DH-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DH-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-DH-TXT-01                PIC X(20) VALUE SPACES.
           05  WS-DH-TXT-02                PIC X(26) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-DH-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-DH-ON-01                 VALUE 'Y'.
               88  WS-DH-OFF-01                VALUE 'N'.
           05  WS-DH-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-DH-ON-02                 VALUE 'Y'.
               88  WS-DH-OFF-02                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-DH-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DH-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DH-SUB-03                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. LOADED ONCE AT INITIALISATION AND SEARCHED WITH
      * A SUBSCRIPTED WALK.
       01  WS-DH-TABLE.
           05  WS-DH-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-DH-TB-ENTRY OCCURS 150 TIMES
                                       INDEXED BY WS-DH-IX.
               10  WS-DH-TB-KEY                PIC X(04).
               10  WS-DH-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-DH-TB-TXT                PIC X(30).
               10  WS-DH-TB-EFF                PIC 9(05).
               10  WS-DH-TB-EXP                PIC 9(05).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 50.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUEX02 - CIRCUIT INVENTORY EXTRACT'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-DH-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-DH-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9913.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-DH-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-DH-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT CIRIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON CIRIN - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT EXTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON EXTOUT - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SUSOUT.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON SUSOUT - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON CTLOUT - CHECK THE ALLOCATION' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-DH-CYCLE-YYDDD.
           COMPUTE WS-DH-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-DH-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-DH-CNT-03.
           MOVE 0 TO WS-DH-CNT-01.
           MOVE 0 TO WS-DH-CNT-04.
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
           IF WS-DH-ON-01
               PERFORM P2200-DERIVE-SUBSET THRU
                   P2200-DERIVE-SUBSET-EXIT.
           PERFORM P2300-DERIVE-MASTER THRU P2300-DERIVE-MASTER-EXIT.
           PERFORM P2400-APPLY-SUBSET THRU P2400-APPLY-SUBSET-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ CIRIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-DERIVE-SUBSET.
           MOVE SPACES TO CABS-DH-OUT-RECORD.
           MOVE ID-CARRIER TO OD-CENTRE.
           MOVE ID-CYCLE TO OD-REGION.
           MOVE ID-CYCLE TO OD-STATUS.
           MOVE ID-STATE TO OD-BAN.
           MOVE ID-BAN2 TO OD-INVOICE.
           MOVE ID-STATE TO OD-BAN2.
           MOVE ID-SOURCE TO OD-CLASS.
           MOVE ID-BAN TO OD-BAN3.
           MOVE ID-CARRIER TO OD-GROUP.
           WRITE CABS-DH-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P2200-DERIVE-SUBSET-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P2300-DERIVE-MASTER.
           MOVE 0 TO WS-DH-QTY-02.
           MOVE 0 TO WS-DH-QTY-01.
           MOVE 0 TO WS-DH-QTY-03.
           MOVE 0 TO WS-DH-AMT-01.
       P2300-DERIVE-MASTER-EXIT.
           EXIT.
       P2400-APPLY-SUBSET.
           IF WS-DH-AMT-01 NOT = 0
               COMPUTE WS-DH-QTY-02 = WS-DH-AMT-01 * 100 / WS-DH-AMT-01
           ELSE
               MOVE 0 TO WS-DH-QTY-02.
       P2400-APPLY-SUBSET-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-POST-MASTER.
           MOVE 0 TO WS-DH-QTY-01.
           MOVE 0 TO WS-DH-QTY-02.
           MOVE 0 TO WS-DH-QTY-04.
           MOVE 0 TO WS-DH-AMT-01.
       P3100-POST-MASTER-EXIT.
           EXIT.
       P3200-EMIT-FILTER.
           MOVE SPACES TO CABS-DH-OUT-RECORD.
           MOVE ID-SOURCE TO OD-CENTRE.
           MOVE ID-CYCLE TO OD-REGION.
           MOVE ID-SOURCE TO OD-STATUS.
           MOVE ID-STATUS TO OD-BAN.
           MOVE ID-TYPE TO OD-INVOICE.
           MOVE ID-TYPE TO OD-BAN2.
           WRITE CABS-DH-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P3200-EMIT-FILTER-EXIT.
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
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE 2 TO CT-STEP-SEQ.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE WS-DH-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE SPACES TO CT-FILLER.
           MOVE WS-DH-TXT-01 TO CT-RESTART-KEY.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE 0 TO CT-RC.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
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
           CLOSE CIRIN.
           CLOSE EXTOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABUEX02 - NORMAL END OF JOB'.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  DH-CNT-01 = ' WS-DH-CNT-01.
           DISPLAY '  DH-CNT-02 = ' WS-DH-CNT-02.
       P9000-EXIT.
           EXIT.
