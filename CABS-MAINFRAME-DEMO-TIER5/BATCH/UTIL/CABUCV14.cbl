      *****************************************************************
      * CABUCV14 - LEGACY LAYOUT DOWN CONVERSION                      *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               SRCIN   TELCABS.CABS.SRCIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               DSPOUT  TELCABS.CABS.DSPOUT         (LOCAL)     *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * REVISION HISTORY                                              *
      *   V1.00  1989-09-27  J.M.CASTILLO INITIAL RELEASE             *
      *   V1.02  2002-07-06  T.YAMASHITA  ROUNDING RULE TAKEN FROM THE*
      *                      RATE ROW                                 *
      *   V1.03  2018-08-06  T.YAMASHITA  REGION SIZE REDUCED - TABLE *
      *                      MOVED OUT OF WORKING STORAGE             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUCV14.
       AUTHOR. TELCABS APPLICATIONS - FILE CONVERSION.
      *****************************************************************
      * LEGACY LAYOUT DOWN CONVERSION. THE STEP IS SCHEDULED MONTHLY  *
      * AND ALSO RUN ON DEMAND WHEN A CENTRE ASKS FOR IT.             *
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
           SELECT SRCIN ASSIGN TO UT-S-SRCIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT DSPOUT ASSIGN TO UT-S-DSPOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT CTLOUT ASSIGN TO UT-S-CTLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      * SRCIN - CATALOGUED GENERATION DATA GROUP.
       FD  SRCIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-CQ-IN-RECORD.
           05  IC-CIRCUIT                  PIC 9(06).
           05  IC-JURIS                    PIC 9(06).
           05  IC-ACCOUNT                  PIC X(03).
           05  IC-REGION                   PIC S9(09) COMP-3.
           05  IC-CIRCUIT2                 PIC X(10).
           05  IC-CARRIER                  PIC S9(13) COMP-3.
           05  IC-SEGMENT                  PIC 9(06).
           05  IC-CIRCUIT3                 PIC S9(09) COMP-3.
           05  IC-GROUP                    PIC X(06).
           05  CQ-FILL-01                  PIC X(26).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-CQ-VIEW1 REDEFINES CABS-CQ-IN-RECORD.
           05  R0C-SOURCE                  PIC 9(07).
           05  R0C-CLASS                   PIC S9(05) COMP-3.
           05  R0C-SEQ                     PIC X(03).
           05  R0C-CODE                    PIC 9(02).
           05  R0C-CIRCUIT                 PIC S9(11)V9(05) COMP-3.
           05  R0C-INVOICE                 PIC 9(03).
           05  R0C-REST                    PIC X(53).
      * DSPOUT - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  DSPOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 100 CHARACTERS.
       01  CABS-CQ-OUT-RECORD.
           05  OC-JURIS                    PIC S9(09) COMP-3.
           05  OC-SOURCE                   PIC S9(09)V9(02) COMP-3.
           05  OC-SEGMENT                  PIC X(16).
           05  OC-OCN                      PIC X(13).
           05  OC-REGION                   PIC S9(09)V9(05) COMP-3.
           05  OC-STATE                    PIC 9(05).
           05  OC-CYCLE                    PIC X(10).
           05  OC-SEGMENT2                 PIC S9(11) COMP-3.
           05  OC-CYCLE2                   PIC X(20).
           05  OC-GROUP                    PIC S9(13) COMP-3.
           05  OC-TARIFF                   PIC 9(03).
           05  CQ-FILL-02                  PIC X(1).
      * CTLOUT - WORK FILE, DELETED AT STEP END.
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
       COPY CABSBILL.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUCV14'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V2.06'.
           05  WS-TABLE-LIMIT              PIC S9(04) COMP-3
                                         VALUE 120.
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
           05  WS-CQ-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CQ-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CQ-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CQ-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CQ-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CQ-CNT-06                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-CQ-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CQ-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CQ-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-CQ-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CQ-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CQ-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-CQ-TXT-01                PIC X(26) VALUE SPACES.
           05  WS-CQ-TXT-02                PIC X(26) VALUE SPACES.
           05  WS-CQ-TXT-03                PIC X(12) VALUE SPACES.
           05  WS-CQ-TXT-04                PIC X(10) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-CQ-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-CQ-ON-01                 VALUE 'Y'.
               88  WS-CQ-OFF-01                VALUE 'N'.
           05  WS-CQ-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-CQ-ON-02                 VALUE 'Y'.
               88  WS-CQ-OFF-02                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-CQ-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CQ-SUB-02                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. LOADED ONCE AT INITIALISATION AND SEARCHED WITH
      * A SUBSCRIPTED WALK.
       01  WS-CQ-TABLE.
           05  WS-CQ-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-CQ-TB-ENTRY OCCURS 120 TIMES
                                       INDEXED BY WS-CQ-IX.
               10  WS-CQ-TB-KEY                PIC X(13).
               10  WS-CQ-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-CQ-TB-TXT                PIC X(30).
               10  WS-CQ-TB-EFF                PIC 9(05).
               10  WS-CQ-TB-EXP                PIC 9(05).
       01  WS-CQ-WORK-GROUP-1.
           05  WS-CQ-G1-SEQ                PIC X(10).
           05  WS-CQ-G1-MEDIA              PIC X(20).
           05  WS-CQ-G1-CARRIER            PIC X(20).
           05  WS-CQ-G1-SOURCE             PIC X(10).
           05  WS-CQ-G1-CLASS              PIC S9(09) COMP-3.
           05  WS-CQ-G1-TYPE               PIC X(10).
           05  WS-CQ-G1-TARGET             PIC X(10).
           05  WS-CQ-G1-CLASS              PIC X(10).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 55.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUCV14 - LEGACY LAYOUT DOWN CONVERSION'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-CQ-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-CQ-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
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
           05  WS-CQ-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-CQ-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           MOVE PC1-CYCLE-YYDDD TO WS-CQ-CYCLE-YYDDD.
           COMPUTE WS-CQ-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-CQ-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-CQ-CNT-03.
           MOVE 0 TO WS-CQ-CNT-02.
           MOVE 0 TO WS-CQ-CNT-06.
       P1200-EXIT.
           EXIT.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-CQ-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-CQ-TAB-CNT NOT < 120
               MOVE 'Y' TO WS-CQ-SW-01
               ADD 1 TO WS-CQ-CNT-05
           ELSE
               ADD 1 TO WS-CQ-TAB-CNT
               SET WS-CQ-IX TO WS-CQ-TAB-CNT
               MOVE IC-CIRCUIT2 TO WS-CQ-TB-KEY (WS-CQ-IX)
               MOVE 0 TO WS-CQ-TB-VAL (WS-CQ-IX)
               MOVE SPACES TO WS-CQ-TB-TXT (WS-CQ-IX)
               ADD 1 TO WS-CQ-CNT-02.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ SRCIN
               AT END MOVE 'Y' TO WS-CQ-SW-01.
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
           PERFORM P2200-VALIDATE-FIELD THRU P2200-VALIDATE-FIELD-EXIT.
           PERFORM P2300-BUILD-CENTURY THRU P2300-BUILD-CENTURY-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ SRCIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-VALIDATE-FIELD.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-OCN-UNKNOWN TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-CQ-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
           MOVE 'N' TO WS-CQ-SW-02.
           IF WS-CQ-TAB-CNT > 0
               PERFORM P270-COMPARE-CENTURY THRU
                   P270-COMPARE-CENTURY-EXIT
               VARYING WS-CQ-SUB-01 FROM 1 BY 1
               UNTIL WS-CQ-SUB-01 > WS-CQ-TAB-CNT
               OR WS-CQ-SW-02 = 'Y'.
       P2200-VALIDATE-FIELD-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P2300-BUILD-CENTURY.
           CALL 'CABHASH' USING IC-CIRCUIT3 WS-ACC-OCN-HASH.
           ADD WS-CQ-CNT-03 TO WS-ACC-SEQ-HASH.
       P2300-BUILD-CENTURY-EXIT.
           EXIT.
       P270-COMPARE-CENTURY.
           SET WS-CQ-IX TO WS-CQ-SUB-02.
           IF WS-CQ-TB-KEY (WS-CQ-IX) = IC-ACCOUNT
               MOVE 'Y' TO WS-CQ-SW-01
               MOVE WS-CQ-TB-VAL (WS-CQ-IX) TO WS-CQ-QTY-02
               MOVE WS-CQ-SUB-02 TO WS-CQ-SUB-01.
       P270-COMPARE-CENTURY-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-POST-CENTURY.
           MOVE SPACES TO WS-CQ-TXT-04.
           STRING IC-REGION DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IC-CIRCUIT3 DELIMITED BY SIZE
               INTO WS-CQ-TXT-04.
       P3100-POST-CENTURY-EXIT.
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
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE 8 TO CT-STEP-SEQ.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE WS-CQ-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE SPACES TO CT-FILLER.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
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
           CLOSE SRCIN.
           CLOSE DSPOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABUCV14 - NORMAL END OF JOB'.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  CQ-CNT-05 = ' WS-CQ-CNT-05.
           DISPLAY '  CQ-CNT-03 = ' WS-CQ-CNT-03.
           DISPLAY '  CQ-CNT-04 = ' WS-CQ-CNT-04.
       P9000-EXIT.
           EXIT.
