      *****************************************************************
      * CABUEX07 - ADJUSTMENT EXTRACT FOR THE GENERAL LEDGER          *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               SELIN   TELCABS.CABS.SELIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               CIROUT  TELCABS.CABS.CIROUT         (LOCAL)     *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - THE OUTPUT IS REBUILT EACH RUN     *
      * REVISION HISTORY                                              *
      *   V1.00  1989-08-03  C.ADEYEMI    INITIAL RELEASE             *
      *   V1.01  2006-03-10  D.OKONKWO    RESTART KEY WRITTEN SO A    *
      *                      RERUN CAN POSITION                       *
      *   V1.02  2011-05-27  D.OKONKWO    CENTURY PIVOT APPLIED TO THE*
      *                      CYCLE DATE                               *
      *   V1.04  2012-05-15  S.MARCHETTI  SUSPENSE WRITE ADDED -      *
      *                      RECORDS WERE BEING DROPPED               *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUEX07.
       AUTHOR. TELCABS APPLICATIONS - DATASET EXTRACT.
      *****************************************************************
      * ADJUSTMENT EXTRACT FOR THE GENERAL LEDGER. THE STEP RUNS ONCE *
      * PER BILL CYCLE AND IS RERUN FROM THE TOP IF IT FAILS.         *
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND   *
      * ARE NOT PART OF THE BALANCE.                                  *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           C01 IS TO-NEW-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SELIN ASSIGN TO UT-S-SELIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT CIROUT ASSIGN TO UT-S-CIROUT
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
      * SELIN - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  SELIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-BE-IN-RECORD.
           05  IB-GROUP                    PIC S9(13) COMP-3.
           05  IB-OCN                      PIC S9(09)V9(02) COMP-3.
           05  IB-CARRIER                  PIC 9(05).
           05  IB-PERIOD                   PIC S9(11) COMP-3.
           05  IB-SEQ                      PIC 9(09).
           05  IB-CARRIER2                 PIC X(10).
           05  IB-TYPE                     PIC 9(05).
           05  IB-CODE                     PIC 9(04).
           05  BE-FILL-01                  PIC X(28).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-BE-VIEW1 REDEFINES CABS-BE-IN-RECORD.
           05  R0B-MEDIA                   PIC X(04).
           05  R0B-LEVEL                   PIC X(16).
           05  R0B-ACCOUNT                 PIC 9(04).
           05  R0B-STATUS                  PIC S9(07)V9(05) COMP-3.
           05  R0B-MEDIA2                  PIC S9(09) COMP-3.
           05  R0B-CYCLE                   PIC S9(15) COMP-3.
           05  R0B-OCN                     PIC S9(11)V9(02) COMP-3.
           05  R0B-REST                    PIC X(29).
      * CIROUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  CIROUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 90 CHARACTERS.
       01  CABS-BE-OUT-RECORD.
           05  OB-CENTRE                   PIC S9(09) COMP-3.
           05  OB-GROUP                    PIC 9(04).
           05  OB-GROUP2                   PIC X(06).
           05  OB-GROUP3                   PIC X(10).
           05  OB-GROUP4                   PIC S9(15) COMP-3.
           05  OB-CARRIER                  PIC X(06).
           05  OB-BAN                      PIC S9(11)V9(05) COMP-3.
           05  OB-TARGET                   PIC X(06).
           05  OB-INVOICE                  PIC 9(09).
           05  OB-CODE                     PIC X(20).
           05  OB-ACCOUNT                  PIC 9(02).
           05  BE-FILL-02                  PIC X(5).
      * SUSOUT - WORK FILE, DELETED AT STEP END.
       FD  SUSOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 300 CHARACTERS.
       01  CABS-SUSOUT-RECORD              PIC X(300).
      * CTLOUT - PERMANENT DATASET HELD ON DASD.
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD              PIC X(180).
       WORKING-STORAGE SECTION.
      * STANDARD SHARED WORKING STORAGE. SEE CABSWRK.
       COPY CABSWRK.
      * SHARED LAYOUT PULLED IN FOR THE CANDIDATE SIDE.
       COPY CABSCIRC.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUEX07'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V2.02'.
           05  WS-TABLE-LIMIT              PIC S9(04) COMP-3
                                         VALUE 80.
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
           05  WS-BE-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BE-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BE-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BE-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BE-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BE-CNT-06                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-BE-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BE-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BE-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BE-QTY-04                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-BE-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BE-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BE-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-BE-TXT-01                PIC X(16) VALUE SPACES.
           05  WS-BE-TXT-02                PIC X(16) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-BE-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-BE-ON-01                 VALUE 'Y'.
               88  WS-BE-OFF-01                VALUE 'N'.
           05  WS-BE-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-BE-ON-02                 VALUE 'Y'.
               88  WS-BE-OFF-02                VALUE 'N'.
           05  WS-BE-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-BE-ON-03                 VALUE 'Y'.
               88  WS-BE-OFF-03                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-BE-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-BE-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-BE-SUB-03                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. THE LIMIT WAS RAISED WHEN THE SOUTHEAST CENTRES
      * WERE MERGED AND HAS NOT BEEN REVIEWED SINCE.
       01  WS-BE-TABLE.
           05  WS-BE-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-BE-TB-ENTRY OCCURS 80 TIMES
                                       INDEXED BY WS-BE-IX.
               10  WS-BE-TB-KEY                PIC X(08).
               10  WS-BE-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-BE-TB-TXT                PIC X(30).
               10  WS-BE-TB-EFF                PIC 9(05).
               10  WS-BE-TB-EXP                PIC 9(05).
       01  WS-BE-WORK-GROUP-1.
           05  WS-BE-G1-BAN                PIC X(10).
           05  WS-BE-G1-CYCLE              PIC 9(07).
           05  WS-BE-G1-REGION             PIC 9(07).
           05  WS-BE-G1-CENTRE             PIC S9(09) COMP-3.
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 60.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUEX07 - ADJUSTMENT EXTRACT FOR THE GENERAL LED'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-BE-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-BE-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9914.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-BE-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-BE-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT SELIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON SELIN - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CIROUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON CIROUT - CHECK THE ALLOCATION' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-BE-CYCLE-YYDDD.
           COMPUTE WS-BE-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-BE-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-BE-CNT-01.
           MOVE 0 TO WS-BE-CNT-05.
           MOVE 0 TO WS-BE-CNT-03.
       P1200-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-BE-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-BE-TAB-CNT NOT < 80
               MOVE 'Y' TO WS-BE-SW-01
               ADD 1 TO WS-BE-CNT-04
           ELSE
               ADD 1 TO WS-BE-TAB-CNT
               SET WS-BE-IX TO WS-BE-TAB-CNT
               MOVE IB-OCN TO WS-BE-TB-KEY (WS-BE-IX)
               MOVE 0 TO WS-BE-TB-VAL (WS-BE-IX)
               MOVE SPACES TO WS-BE-TB-TXT (WS-BE-IX)
               ADD 1 TO WS-BE-CNT-03.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ SELIN
               AT END MOVE 'Y' TO WS-BE-SW-01.
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
           PERFORM P2200-CHECK-EXTRACT THRU P2200-CHECK-EXTRACT-EXIT.
           PERFORM P2300-VALIDATE-EXTRACT THRU
               P2300-VALIDATE-EXTRACT-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ SELIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P2200-CHECK-EXTRACT.
           IF WS-BE-AMT-03 < 11
               MOVE 11 TO WS-BE-AMT-03
               ADD 1 TO WS-BE-CNT-03.
           IF WS-BE-AMT-03 > 92006
               MOVE 92006 TO WS-BE-AMT-03
               ADD 1 TO WS-BE-CNT-01.
           MOVE 'N' TO WS-BE-SW-02.
           IF WS-BE-TAB-CNT > 0
               PERFORM P280-COMPARE-SELECTION THRU
                   P280-COMPARE-SELECTION-EXIT
               VARYING WS-BE-SUB-02 FROM 1 BY 1
               UNTIL WS-BE-SUB-02 > WS-BE-TAB-CNT
               OR WS-BE-SW-02 = 'Y'.
       P2200-CHECK-EXTRACT-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P2300-VALIDATE-EXTRACT.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-BAN-UNKNOWN TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-BE-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
       P2300-VALIDATE-EXTRACT-EXIT.
           EXIT.
       P280-COMPARE-SELECTION.
           SET WS-BE-IX TO WS-BE-SUB-01.
           IF WS-BE-TB-KEY (WS-BE-IX) = IB-SEQ
               MOVE 'Y' TO WS-BE-SW-02
               MOVE WS-BE-TB-VAL (WS-BE-IX) TO WS-BE-QTY-03
               MOVE WS-BE-SUB-01 TO WS-BE-SUB-01.
       P280-COMPARE-SELECTION-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-WRITE-MASTER.
           MOVE 0 TO WS-BE-QTY-02.
           MOVE 0 TO WS-BE-QTY-01.
           MOVE 0 TO WS-BE-QTY-03.
           MOVE 0 TO WS-BE-AMT-03.
       P3100-WRITE-MASTER-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P3200-CLOSE-OFF-EXTRACT.
           MOVE IB-OCN TO WS-BE-TXT-02.
           MOVE IB-TYPE TO WS-BE-TXT-01.
           MOVE IB-CARRIER2 TO WS-BE-TXT-01.
           MOVE IB-CODE TO WS-BE-TXT-01.
           ADD 1 TO WS-BE-CNT-04.
       P3200-CLOSE-OFF-EXTRACT-EXIT.
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
           MOVE SPACES TO CT-ABEND-CD.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE SPACES TO CT-FILLER.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE WS-BE-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE 6 TO CT-STEP-SEQ.
       P8100-EXIT.
           EXIT.
      * P8200-CHECK-BALANCE - EVERY RECORD READ IS EITHER WRITTEN,
      * REJECTED, SUMMARISED OR CARRIED FORWARD.
       P8200-CHECK-BALANCE.
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
           CLOSE SELIN.
           CLOSE CIROUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABUEX07 - RUN COMPLETE'.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  BE-CNT-02 = ' WS-BE-CNT-02.
           DISPLAY '  BE-CNT-01 = ' WS-BE-CNT-01.
           DISPLAY '  BE-CNT-06 = ' WS-BE-CNT-06.
           DISPLAY '  BE-CNT-05 = ' WS-BE-CNT-05.
       P9000-EXIT.
           EXIT.
