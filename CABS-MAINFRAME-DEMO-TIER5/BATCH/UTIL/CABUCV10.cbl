      *****************************************************************
      * CABUCV10 - FIXED TO VARIABLE RECORD CONVERSION                *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               OLDIN   TELCABS.CABS.OLDIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               NEWOUT  TELCABS.CABS.NEWOUT         (LOCAL)     *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * REVISION HISTORY                                              *
      *   V1.00  1992-10-27  P.NAIR       INITIAL RELEASE             *
      *   V1.02  1996-12-21  A.BUKOWSKI   EFFECTIVE DATE FILTER ADDED *
      *                      PER AUDIT FINDING                        *
      *   V1.03  2001-01-11  T.YAMASHITA  TABLE LIMIT RAISED FOR THE  *
      *                      SOUTHEAST CENTRES                        *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUCV10.
       AUTHOR. TELCABS APPLICATIONS - FILE CONVERSION.
      *****************************************************************
      * FIXED TO VARIABLE RECORD CONVERSION. THIS STEP IS SCHEDULED   *
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
           SELECT OLDIN ASSIGN TO UT-S-OLDIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT NEWOUT ASSIGN TO UT-S-NEWOUT
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
      * OLDIN - PERMANENT DATASET HELD ON DASD.
       FD  OLDIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-CN-IN-RECORD.
           05  IC-SOURCE                   PIC S9(13)V9(02) COMP-3.
           05  IC-CLASS                    PIC 9(09).
           05  IC-STATUS                   PIC S9(09) COMP-3.
           05  IC-TARIFF                   PIC X(02).
           05  IC-REGION                   PIC S9(09) COMP-3.
           05  IC-MEDIA                    PIC 9(06).
           05  IC-CODE                     PIC X(02).
           05  IC-SEQ                      PIC S9(13)V9(02) COMP-3.
           05  IC-TARIFF2                  PIC S9(07)V9(05) COMP-3.
           05  CN-FILL-01                  PIC X(28).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-CN-VIEW1 REDEFINES CABS-CN-IN-RECORD.
           05  R0C-STATE                   PIC S9(11)V9(02) COMP-3.
           05  R0C-TARIFF                  PIC X(08).
           05  R0C-OCN                     PIC S9(11)V9(05) COMP-3.
           05  R0C-INVOICE                 PIC S9(11) COMP-3.
           05  R0C-REST                    PIC X(50).
      * NEWOUT - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  NEWOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 110 CHARACTERS.
       01  CABS-CN-OUT-RECORD.
           05  OC-ELEM                     PIC X(13).
           05  OC-TARIFF                   PIC X(20).
           05  OC-INVOICE                  PIC X(20).
           05  OC-GROUP                    PIC X(03).
           05  OC-SEGMENT                  PIC S9(13) COMP-3.
           05  OC-STATUS                   PIC S9(13) COMP-3.
           05  OC-SEQ                      PIC S9(07) COMP-3.
           05  OC-SEQ2                     PIC S9(15) COMP-3.
           05  OC-SEQ3                     PIC X(16).
           05  OC-BAND                     PIC S9(09) COMP-3.
           05  CN-FILL-02                  PIC X(7).
      * SUSOUT - PERMANENT DATASET HELD ON DASD.
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
      * SHARED LAYOUT PULLED IN FOR THE SIGN SIDE.
       COPY CABSSETL.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUCV10'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V1.05'.
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
           05  WS-CN-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CN-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CN-CNT-03                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-CN-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CN-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-CN-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CN-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CN-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-CN-TXT-01                PIC X(08) VALUE SPACES.
           05  WS-CN-TXT-02                PIC X(12) VALUE SPACES.
           05  WS-CN-TXT-03                PIC X(26) VALUE SPACES.
           05  WS-CN-TXT-04                PIC X(20) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-CN-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-CN-ON-01                 VALUE 'Y'.
               88  WS-CN-OFF-01                VALUE 'N'.
           05  WS-CN-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-CN-ON-02                 VALUE 'Y'.
               88  WS-CN-OFF-02                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-CN-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CN-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CN-SUB-03                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. THE LIMIT WAS RAISED WHEN THE SOUTHEAST CENTRES
      * WERE MERGED AND HAS NOT BEEN REVIEWED SINCE.
       01  WS-CN-TABLE.
           05  WS-CN-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-CN-TB-ENTRY OCCURS 80 TIMES
                                       INDEXED BY WS-CN-IX.
               10  WS-CN-TB-KEY                PIC X(13).
               10  WS-CN-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-CN-TB-TXT                PIC X(20).
               10  WS-CN-TB-EFF                PIC 9(05).
               10  WS-CN-TB-EXP                PIC 9(05).
       01  WS-CN-WORK-GROUP-1.
           05  WS-CN-G1-CARRIER            PIC 9(05).
           05  WS-CN-G1-CENTRE             PIC X(20).
           05  WS-CN-G1-PERIOD             PIC S9(09) COMP-3.
           05  WS-CN-G1-CYCLE              PIC 9(05).
           05  WS-CN-G1-LEVEL              PIC 9(07).
           05  WS-CN-G1-TARGET             PIC S9(09) COMP-3.
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 50.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUCV10 - FIXED TO VARIABLE RECORD CONVERSION'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-CN-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-CN-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9911.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-CN-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-CN-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT OLDIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OLDIN NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               DISPLAY 'OLDIN FILE STATUS = ' WS-FS-INPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT NEWOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'NEWOUT NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               DISPLAY 'NEWOUT FILE STATUS = ' WS-FS-OUTPUT
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
           MOVE PC1-CYCLE-YYDDD TO WS-CN-CYCLE-YYDDD.
           COMPUTE WS-CN-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-CN-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-CN-CNT-01.
           MOVE 0 TO WS-CN-CNT-02.
           MOVE 0 TO WS-CN-CNT-03.
       P1200-EXIT.
           EXIT.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-CN-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-CN-TAB-CNT NOT < 80
               MOVE 'Y' TO WS-CN-SW-01
               ADD 1 TO WS-CN-CNT-03
           ELSE
               ADD 1 TO WS-CN-TAB-CNT
               SET WS-CN-IX TO WS-CN-TAB-CNT
               MOVE IC-CODE TO WS-CN-TB-KEY (WS-CN-IX)
               MOVE 0 TO WS-CN-TB-VAL (WS-CN-IX)
               MOVE SPACES TO WS-CN-TB-TXT (WS-CN-IX)
               ADD 1 TO WS-CN-CNT-03.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ OLDIN
               AT END MOVE 'Y' TO WS-CN-SW-01.
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
           PERFORM P2200-APPLY-FIELD THRU P2200-APPLY-FIELD-EXIT.
           IF WS-CN-ON-01
               PERFORM P2300-EXPAND-RECORD THRU
                   P2300-EXPAND-RECORD-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ OLDIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-APPLY-FIELD.
           CALL 'CABFMTR' USING WS-CN-TXT-03 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-CN-CNT-01.
           MOVE 'N' TO WS-CN-SW-02.
           IF WS-CN-TAB-CNT > 0
               PERFORM P250-COMPARE-LAYOUT THRU P250-COMPARE-LAYOUT-EXIT
               VARYING WS-CN-SUB-01 FROM 1 BY 1
               UNTIL WS-CN-SUB-01 > WS-CN-TAB-CNT
               OR WS-CN-SW-02 = 'Y'.
       P2200-APPLY-FIELD-EXIT.
           EXIT.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P2300-EXPAND-RECORD.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-CN-TXT-01 TO PC-COL-001-020.
           MOVE WS-CN-TXT-01 TO PC-COL-021-060.
           MOVE WS-CN-AMT-03 TO WS-CN-AMT-EDIT.
           MOVE WS-CN-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P2300-EXPAND-RECORD-EXIT.
           EXIT.
       P250-COMPARE-LAYOUT.
           SET WS-CN-IX TO WS-CN-SUB-03.
           IF WS-CN-TB-KEY (WS-CN-IX) = IC-SEQ
               MOVE 'Y' TO WS-CN-SW-02
               MOVE WS-CN-TB-VAL (WS-CN-IX) TO WS-CN-QTY-01
               MOVE WS-CN-SUB-03 TO WS-CN-SUB-03.
       P250-COMPARE-LAYOUT-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P3100-POST-SIGN.
           MOVE IC-STATUS TO WS-CN-TXT-02.
           MOVE IC-MEDIA TO WS-CN-TXT-03.
           ADD 1 TO WS-CN-CNT-01.
       P3100-POST-SIGN-EXIT.
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
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE 7 TO CT-STEP-SEQ.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE WS-CN-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE WS-CN-CNT-03 TO CT-RC.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE WS-CN-TXT-01 TO CT-RESTART-KEY.
           MOVE SPACES TO CT-FILLER.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
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
           CLOSE OLDIN.
           CLOSE NEWOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABUCV10 - STEP COMPLETE'.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  CN-CNT-01 = ' WS-CN-CNT-01.
           DISPLAY '  CN-CNT-03 = ' WS-CN-CNT-03.
           DISPLAY '  CN-CNT-02 = ' WS-CN-CNT-02.
       P9000-EXIT.
           EXIT.
