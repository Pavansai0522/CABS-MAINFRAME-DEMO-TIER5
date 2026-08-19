      *****************************************************************
      * CABUEX05 - ADJUSTMENT EXTRACT FOR THE GENERAL LEDGER          *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               GLIN    TELCABS.CABS.GLIN           (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               EXTOUT  TELCABS.CABS.EXTOUT         (LOCAL)     *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * REVISION HISTORY                                              *
      *   V1.00  1987-07-19  W.J.MCALLISTER INITIAL RELEASE           *
      *   V1.04  2004-02-16  A.BUKOWSKI   REGION SIZE REDUCED - TABLE *
      *                      MOVED OUT OF WORKING STORAGE             *
      *   V1.06  2012-11-02  A.BUKOWSKI   JOB PARAMETER MADE MANDATORY*
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUEX05.
       AUTHOR. TELCABS APPLICATIONS - DATASET EXTRACT.
      *****************************************************************
      * ADJUSTMENT EXTRACT FOR THE GENERAL LEDGER. THE STEP IS        *
      * SCHEDULED MONTHLY AND ALSO RUN ON DEMAND WHEN A CENTRE ASKS   *
      * FOR IT.                                                       *
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
           SELECT GLIN ASSIGN TO UT-S-GLIN
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
      * GLIN - WORK FILE, DELETED AT STEP END.
       FD  GLIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 90 CHARACTERS.
       01  CABS-BW-IN-RECORD.
           05  IB-JURIS                    PIC S9(15) COMP-3.
           05  IB-OCN                      PIC S9(15) COMP-3.
           05  IB-SOURCE                   PIC X(02).
           05  IB-TYPE                     PIC S9(13) COMP-3.
           05  IB-OCN2                     PIC 9(07).
           05  IB-MEDIA                    PIC S9(05) COMP-3.
           05  IB-SEGMENT                  PIC 9(05).
           05  IB-SEGMENT2                 PIC X(08).
           05  IB-CODE                     PIC S9(09) COMP-3.
           05  IB-REGION                   PIC X(20).
           05  IB-CYCLE                    PIC 9(09).
           05  BW-FILL-01                  PIC X(8).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-BW-VIEW1 REDEFINES CABS-BW-IN-RECORD.
           05  R0B-CYCLE                   PIC 9(06).
           05  R0B-CODE                    PIC 9(06).
           05  R0B-SOURCE                  PIC S9(07) COMP-3.
           05  R0B-BAN                     PIC S9(13) COMP-3.
           05  R0B-MEDIA                   PIC 9(09).
           05  R0B-OCN                     PIC 9(04).
           05  R0B-REST                    PIC X(54).
      * EXTOUT - CATALOGUED GENERATION DATA GROUP.
       FD  EXTOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-BW-OUT-RECORD.
           05  OB-CIRCUIT                  PIC S9(07)V9(02) COMP-3.
           05  OB-CYCLE                    PIC S9(15) COMP-3.
           05  OB-STATUS                   PIC X(16).
           05  OB-JURIS                    PIC X(13).
           05  OB-BAN                      PIC X(03).
           05  OB-LEVEL                    PIC S9(07)V9(02) COMP-3.
           05  OB-TARGET                   PIC 9(03).
           05  OB-CODE                     PIC X(03).
           05  OB-BAN2                     PIC S9(07)V9(02) COMP-3.
           05  OB-OCN                      PIC S9(13) COMP-3.
           05  BW-FILL-02                  PIC X(12).
      * SUSOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
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
       COPY CABSCDR.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUEX05'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V2.04'.
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
           05  WS-BW-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BW-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BW-CNT-03                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-BW-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BW-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BW-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-BW-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BW-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BW-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-BW-TXT-01                PIC X(26) VALUE SPACES.
           05  WS-BW-TXT-02                PIC X(20) VALUE SPACES.
           05  WS-BW-TXT-03                PIC X(26) VALUE SPACES.
           05  WS-BW-TXT-04                PIC X(10) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-BW-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-BW-ON-01                 VALUE 'Y'.
               88  WS-BW-OFF-01                VALUE 'N'.
           05  WS-BW-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-BW-ON-02                 VALUE 'Y'.
               88  WS-BW-OFF-02                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-BW-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-BW-SUB-02                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. THE LIMIT WAS RAISED WHEN THE SOUTHEAST CENTRES
      * WERE MERGED AND HAS NOT BEEN REVIEWED SINCE.
       01  WS-BW-TABLE.
           05  WS-BW-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-BW-TB-ENTRY OCCURS 150 TIMES
                                       INDEXED BY WS-BW-IX.
               10  WS-BW-TB-KEY                PIC X(04).
               10  WS-BW-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-BW-TB-TXT                PIC X(30).
               10  WS-BW-TB-EFF                PIC 9(05).
               10  WS-BW-TB-EXP                PIC 9(05).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 50.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUEX05 - ADJUSTMENT EXTRACT FOR THE GENERAL LED'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-BW-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-BW-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9984.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-BW-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-BW-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT GLIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'GLIN COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT EXTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'EXTOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SUSOUT.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'SUSOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CTLOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-BW-CYCLE-YYDDD.
           COMPUTE WS-BW-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-BW-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-BW-CNT-03.
           MOVE 0 TO WS-BW-CNT-02.
           MOVE 0 TO WS-BW-CNT-01.
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
           IF WS-BW-ON-01
               PERFORM P2200-EXPAND-EXTRACT THRU
                   P2200-EXPAND-EXTRACT-EXIT.
           PERFORM P2300-APPLY-SELECTION THRU
               P2300-APPLY-SELECTION-EXIT.
           PERFORM P2400-CHECK-FILTER THRU P2400-CHECK-FILTER-EXIT.
           PERFORM P2500-MATCH-SELECTION THRU
               P2500-MATCH-SELECTION-EXIT.
           PERFORM P2600-DERIVE-MASTER THRU P2600-DERIVE-MASTER-EXIT.
           IF WS-BW-ON-01
               PERFORM P2700-BUILD-FILTER THRU P2700-BUILD-FILTER-EXIT.
           PERFORM P2800-SELECT-SELECTION THRU
               P2800-SELECT-SELECTION-EXIT.
           IF WS-BW-ON-02
               PERFORM P2900-EXPAND-SELECTION THRU
                   P2900-EXPAND-SELECTION-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ GLIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P2200-EXPAND-EXTRACT.
           IF WS-BW-AMT-03 NOT = 0
               COMPUTE WS-BW-QTY-03 = WS-BW-AMT-03 * 100 / WS-BW-AMT-03
           ELSE
               MOVE 0 TO WS-BW-QTY-03.
       P2200-EXPAND-EXTRACT-EXIT.
           EXIT.
       P2300-APPLY-SELECTION.
           IF WS-BW-AMT-02 < 44
               MOVE 44 TO WS-BW-AMT-02
               ADD 1 TO WS-BW-CNT-03.
           IF WS-BW-AMT-02 > 58350
               MOVE 58350 TO WS-BW-AMT-02
               ADD 1 TO WS-BW-CNT-03.
       P2300-APPLY-SELECTION-EXIT.
           EXIT.
       P2400-CHECK-FILTER.
           MOVE 0 TO WS-BW-QTY-01.
           MOVE 0 TO WS-BW-QTY-03.
           MOVE 0 TO WS-BW-AMT-03.
       P2400-CHECK-FILTER-EXIT.
           EXIT.
       P2500-MATCH-SELECTION.
           MOVE SPACES TO CABS-BW-OUT-RECORD.
           MOVE IB-SEGMENT2 TO OB-CIRCUIT.
           MOVE IB-OCN TO OB-CYCLE.
           MOVE IB-CYCLE TO OB-STATUS.
           MOVE IB-JURIS TO OB-JURIS.
           MOVE IB-JURIS TO OB-BAN.
           MOVE IB-CODE TO OB-LEVEL.
           MOVE IB-CODE TO OB-TARGET.
           MOVE IB-CYCLE TO OB-CODE.
           WRITE CABS-BW-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P2500-MATCH-SELECTION-EXIT.
           EXIT.
       P2600-DERIVE-MASTER.
           UNSTRING WS-BW-TXT-04 DELIMITED BY '/'
               INTO WS-BW-TXT-01
               WS-BW-TXT-02
               TALLYING IN WS-BW-CNT-02.
       P2600-DERIVE-MASTER-EXIT.
           EXIT.
       P2700-BUILD-FILTER.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-BW-TXT-04 TO PC-COL-001-020.
           MOVE WS-BW-TXT-03 TO PC-COL-021-060.
           MOVE WS-BW-AMT-02 TO WS-BW-AMT-EDIT.
           MOVE WS-BW-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P2700-BUILD-FILTER-EXIT.
           EXIT.
       P2800-SELECT-SELECTION.
           CALL 'CABHASH' USING IB-REGION WS-ACC-OCN-HASH.
           ADD WS-BW-CNT-01 TO WS-ACC-SEQ-HASH.
       P2800-SELECT-SELECTION-EXIT.
           EXIT.
       P2900-EXPAND-SELECTION.
           ADD IB-OCN TO WS-BW-QTY-03.
           COMPUTE WS-BW-AMT-02 = WS-BW-QTY-03 * WS-BW-QTY-03.
           ADD WS-BW-AMT-02 TO WS-BW-AMT-03.
       P2900-EXPAND-SELECTION-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-CLOSE-OFF-FILTER.
           MOVE SPACES TO CABS-BW-OUT-RECORD.
           MOVE IB-OCN2 TO OB-CIRCUIT.
           MOVE IB-SOURCE TO OB-CYCLE.
           MOVE IB-SEGMENT2 TO OB-STATUS.
           MOVE IB-CYCLE TO OB-JURIS.
           MOVE IB-MEDIA TO OB-BAN.
           MOVE IB-TYPE TO OB-LEVEL.
           WRITE CABS-BW-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P3100-CLOSE-OFF-FILTER-EXIT.
           EXIT.
       P3200-EMIT-RANGE.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-OCN-UNKNOWN TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-BW-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
           MOVE CABS-SUSPENSE-RECORD TO CABS-SUSOUT-RECORD.
           WRITE CABS-SUSOUT-RECORD.
       P3200-EMIT-RANGE-EXIT.
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
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE 7 TO CT-STEP-SEQ.
           MOVE WS-BW-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE 0 TO CT-RC.
           MOVE WS-BW-TXT-04 TO CT-RESTART-KEY.
           MOVE SPACES TO CT-FILLER.
           MOVE SPACES TO CT-ABEND-CD.
       P8100-EXIT.
           EXIT.
      * P8200-CHECK-BALANCE - A RECORD THAT WAS HELD FOR THE NEXT
      * CYCLE IS BROUGHT INTO THE CARRIED FORWARD SIDE BEFORE THE
      * EQUATION IS TESTED.
       P8200-CHECK-BALANCE.
           ADD WS-BW-CNT-02 TO CT-CARRIED-FWD.
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
           CLOSE GLIN.
           CLOSE EXTOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABUEX05 - STEP COMPLETE'.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  BW-CNT-03 = ' WS-BW-CNT-03.
           DISPLAY '  BW-CNT-01 = ' WS-BW-CNT-01.
           DISPLAY '  BW-CNT-02 = ' WS-BW-CNT-02.
       P9000-EXIT.
           EXIT.
