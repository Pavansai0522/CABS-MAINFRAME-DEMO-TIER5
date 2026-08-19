      *****************************************************************
      * CABUEX15 - SUSPENSE EXTRACT FOR THE RECYCLE JOB               *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               USGIN   TELCABS.CABS.USGIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               DROPOUT TELCABS.CABS.DROPOU         (LOCAL)     *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * REVISION HISTORY                                              *
      *   V1.00  1991-05-07  G.PRZYBYLSKI INITIAL RELEASE             *
      *   V1.03  1999-09-08  J.M.CASTILLO PRINT LINE WIDENED TO 133   *
      *   V1.05  2008-07-17  S.MARCHETTI  REGION SIZE REDUCED - TABLE *
      *                      MOVED OUT OF WORKING STORAGE             *
      *   V1.08  2009-11-06  R.T.WHEELER  EFFECTIVE DATE FILTER ADDED *
      *                      PER AUDIT FINDING                        *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUEX15.
       AUTHOR. TELCABS APPLICATIONS - DATASET EXTRACT.
      *****************************************************************
      * SUSPENSE EXTRACT FOR THE RECYCLE JOB. THE STEP IS DRIVEN      *
      * ENTIRELY FROM THE SYSIN PARM CARD AND THE DD ALLOCATIONS IN   *
      * THE JOB.                                                      *
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF  *
      * ZERO IS OPEN ENDED.                                           *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           C01 IS TO-NEW-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USGIN ASSIGN TO UT-S-USGIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT DROPOUT ASSIGN TO UT-S-DROPOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT CTLOUT ASSIGN TO UT-S-CTLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      * USGIN - CATALOGUED GENERATION DATA GROUP.
       FD  USGIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 100 CHARACTERS.
       01  CABS-CB-IN-RECORD.
           05  IC-MEDIA                    PIC S9(13)V9(02) COMP-3.
           05  IC-TARGET                   PIC X(16).
           05  IC-REGION                   PIC S9(09)V9(02) COMP-3.
           05  IC-INVOICE                  PIC 9(03).
           05  IC-CODE                     PIC S9(13)V9(02) COMP-3.
           05  IC-MEDIA2                   PIC X(03).
           05  IC-GROUP                    PIC S9(07) COMP-3.
           05  IC-MEDIA3                   PIC 9(07).
           05  IC-LEVEL                    PIC X(04).
           05  IC-SEQ                      PIC X(20).
           05  IC-STATUS                   PIC X(02).
           05  IC-OCN                      PIC 9(07).
           05  IC-CYCLE                    PIC X(06).
           05  IC-PERIOD                   PIC 9(02).
           05  CB-FILL-01                  PIC X(4).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-CB-VIEW1 REDEFINES CABS-CB-IN-RECORD.
           05  R0C-SEQ                     PIC X(04).
           05  R0C-ELEM                    PIC 9(06).
           05  R0C-STATE                   PIC X(08).
           05  R0C-STATUS                  PIC S9(11) COMP-3.
           05  R0C-INVOICE                 PIC X(16).
           05  R0C-CIRCUIT                 PIC X(10).
           05  R0C-JURIS                   PIC 9(02).
           05  R0C-GROUP                   PIC X(03).
           05  R0C-REST                    PIC X(45).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-CB-VIEW2 REDEFINES CABS-CB-IN-RECORD.
           05  R1C-STATE                   PIC 9(09).
           05  R1C-PERIOD                  PIC 9(04).
           05  R1C-TYPE                    PIC S9(13)V9(02) COMP-3.
           05  R1C-CODE                    PIC 9(05).
           05  R1C-REST                    PIC X(74).
      * DROPOUT - CATALOGUED GENERATION DATA GROUP.
       FD  DROPOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-CB-OUT-RECORD.
           05  OC-LEVEL                    PIC X(08).
           05  OC-REGION                   PIC S9(09)V9(02) COMP-3.
           05  OC-TARIFF                   PIC X(04).
           05  OC-CIRCUIT                  PIC X(08).
           05  OC-BAN                      PIC 9(04).
           05  OC-SOURCE                   PIC X(10).
           05  OC-BAN2                     PIC X(03).
           05  OC-CLASS                    PIC S9(07) COMP-3.
           05  OC-SEQ                      PIC X(16).
           05  OC-PERIOD                   PIC X(08).
           05  OC-ELEM                     PIC S9(09) COMP-3.
           05  CB-FILL-02                  PIC X(4).
      * CTLOUT - CATALOGUED GENERATION DATA GROUP.
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD              PIC X(180).
       WORKING-STORAGE SECTION.
      * STANDARD SHARED WORKING STORAGE. SEE CABSWRK.
       COPY CABSWRK.
      * SHARED LAYOUT PULLED IN FOR THE FILTER SIDE.
       COPY CABSCDR.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUEX15'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V1.22'.
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
       01  WS-COUNT-AREA.
           05  WS-CB-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CB-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CB-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CB-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CB-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CB-CNT-06                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-CB-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CB-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CB-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-CB-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CB-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CB-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-CB-TXT-01                PIC X(30) VALUE SPACES.
           05  WS-CB-TXT-02                PIC X(16) VALUE SPACES.
           05  WS-CB-TXT-03                PIC X(08) VALUE SPACES.
           05  WS-CB-TXT-04                PIC X(26) VALUE SPACES.
           05  WS-CB-TXT-05                PIC X(26) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-CB-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-CB-ON-01                 VALUE 'Y'.
               88  WS-CB-OFF-01                VALUE 'N'.
           05  WS-CB-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-CB-ON-02                 VALUE 'Y'.
               88  WS-CB-OFF-02                VALUE 'N'.
           05  WS-CB-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-CB-ON-03                 VALUE 'Y'.
               88  WS-CB-OFF-03                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-CB-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CB-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CB-SUB-03                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. THE LIMIT WAS RAISED WHEN THE SOUTHEAST CENTRES
      * WERE MERGED AND HAS NOT BEEN REVIEWED SINCE.
       01  WS-CB-TABLE.
           05  WS-CB-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-CB-TB-ENTRY OCCURS 100 TIMES
                                       INDEXED BY WS-CB-IX.
               10  WS-CB-TB-KEY                PIC X(10).
               10  WS-CB-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-CB-TB-TXT                PIC X(30).
               10  WS-CB-TB-EFF                PIC 9(05).
               10  WS-CB-TB-EXP                PIC 9(05).
       01  WS-CB-WORK-GROUP-1.
           05  WS-CB-G1-JURIS              PIC X(20).
           05  WS-CB-G1-ACCOUNT            PIC S9(09) COMP-3.
           05  WS-CB-G1-BAN                PIC 9(05).
           05  WS-CB-G1-TARGET             PIC 9(07).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 60.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUEX15 - SUSPENSE EXTRACT FOR THE RECYCLE JOB'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-CB-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-CB-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9956.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-CB-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-CB-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT USGIN.
           IF WS-FS-INPUT NOT = '00'
               DISPLAY 'USGIN FILE STATUS = ' WS-FS-INPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF USGIN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT DROPOUT.
           IF WS-FS-OUTPUT NOT = '00'
               DISPLAY 'DROPOUT FILE STATUS = ' WS-FS-OUTPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF DROPOUT' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               DISPLAY 'CTLOUT FILE STATUS = ' WS-FS-CONTROL
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF CTLOUT' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-CB-CYCLE-YYDDD.
           COMPUTE WS-CB-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-CB-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-CB-CNT-05.
           MOVE 0 TO WS-CB-CNT-01.
           MOVE 0 TO WS-CB-CNT-02.
       P1200-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-CB-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-CB-TAB-CNT NOT < 100
               MOVE 'Y' TO WS-CB-SW-01
               ADD 1 TO WS-CB-CNT-03
           ELSE
               ADD 1 TO WS-CB-TAB-CNT
               SET WS-CB-IX TO WS-CB-TAB-CNT
               MOVE IC-OCN TO WS-CB-TB-KEY (WS-CB-IX)
               MOVE 0 TO WS-CB-TB-VAL (WS-CB-IX)
               MOVE SPACES TO WS-CB-TB-TXT (WS-CB-IX)
               ADD 1 TO WS-CB-CNT-06.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ USGIN
               AT END MOVE 'Y' TO WS-CB-SW-01.
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
           PERFORM P2200-BUILD-SELECTION THRU
               P2200-BUILD-SELECTION-EXIT.
           PERFORM P2300-EDIT-CANDIDATE THRU P2300-EDIT-CANDIDATE-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ USGIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P2200-BUILD-SELECTION.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-RATE-NOT-FOUND TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-CB-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
           MOVE 'N' TO WS-CB-SW-03.
           IF WS-CB-TAB-CNT > 0
               PERFORM P270-COMPARE-SUBSET THRU P270-COMPARE-SUBSET-EXIT
               VARYING WS-CB-SUB-01 FROM 1 BY 1
               UNTIL WS-CB-SUB-01 > WS-CB-TAB-CNT
               OR WS-CB-SW-03 = 'Y'.
       P2200-BUILD-SELECTION-EXIT.
           EXIT.
       P2300-EDIT-CANDIDATE.
           CALL 'CABHASH' USING IC-OCN WS-ACC-OCN-HASH.
           ADD WS-CB-CNT-05 TO WS-ACC-SEQ-HASH.
       P2300-EDIT-CANDIDATE-EXIT.
           EXIT.
       P270-COMPARE-SUBSET.
           SET WS-CB-IX TO WS-CB-SUB-02.
           IF WS-CB-TB-KEY (WS-CB-IX) = IC-TARGET
               MOVE 'Y' TO WS-CB-SW-03
               MOVE WS-CB-TB-VAL (WS-CB-IX) TO WS-CB-QTY-01
               MOVE WS-CB-SUB-02 TO WS-CB-SUB-01.
       P270-COMPARE-SUBSET-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P3100-EMIT-SUBSET.
           ADD IC-REGION TO WS-CB-QTY-01.
           COMPUTE WS-CB-AMT-01 = WS-CB-QTY-01 * WS-CB-QTY-01.
           ADD WS-CB-AMT-01 TO WS-CB-AMT-02.
       P3100-EMIT-SUBSET-EXIT.
           EXIT.
       P3200-CLOSE-OFF-RANGE.
           MOVE IC-CYCLE TO WS-CB-TXT-03.
           MOVE IC-INVOICE TO WS-CB-TXT-03.
           MOVE IC-TARGET TO WS-CB-TXT-02.
           ADD 1 TO WS-CB-CNT-01.
       P3200-CLOSE-OFF-RANGE-EXIT.
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
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE SPACES TO CT-FILLER.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE WS-CB-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE 4 TO CT-STEP-SEQ.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
       P8100-EXIT.
           EXIT.
      * P8200-CHECK-BALANCE - A RECORD THAT WAS HELD FOR THE NEXT
      * CYCLE IS BROUGHT INTO THE CARRIED FORWARD SIDE BEFORE THE
      * EQUATION IS TESTED.
       P8200-CHECK-BALANCE.
           ADD WS-CB-CNT-05 TO CT-CARRIED-FWD.
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
           CLOSE USGIN.
           CLOSE DROPOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABUEX15 - END OF RUN'.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  CB-CNT-03 = ' WS-CB-CNT-03.
           DISPLAY '  CB-CNT-01 = ' WS-CB-CNT-01.
       P9000-EXIT.
           EXIT.
