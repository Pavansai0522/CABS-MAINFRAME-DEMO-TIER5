      *****************************************************************
      * CABUCV06 - EMI FORMAT CONVERSION                              *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               EMIIN   TELCABS.CABS.EMIIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               REJOUT  TELCABS.CABS.REJOUT         (LOCAL)     *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - THE OUTPUT IS REBUILT EACH RUN     *
      * REVISION HISTORY                                              *
      *   V1.00  1991-08-10  W.J.MCALLISTER INITIAL RELEASE           *
      *   V1.01  1994-07-18  T.YAMASHITA  PRINT LINE WIDENED TO 133   *
      *   V1.02  2001-07-23  M.DELACROIX  EFFECTIVE DATE FILTER ADDED *
      *                      PER AUDIT FINDING                        *
      *   V1.03  2011-09-21  D.OKONKWO    RETIRED THE SECOND SORT STEP*
      *                      - DONE IN PROGRAM                        *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUCV06.
       AUTHOR. TELCABS APPLICATIONS - FILE CONVERSION.
      *****************************************************************
      * EMI FORMAT CONVERSION. THIS STEP IS SCHEDULED INSIDE THE      *
      * NIGHTLY ACCESS BILLING STREAM AND HAS NO INTERACTIVE ENTRY    *
      * POINT.                                                        *
      * A FAILURE HERE SUSPENDS THE RECORD, IT DOES NOT STOP THE STEP.*
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           C01 IS TO-NEW-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMIIN ASSIGN TO UT-S-EMIIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT REJOUT ASSIGN TO UT-S-REJOUT
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
      * EMIIN - WORK FILE, DELETED AT STEP END.
       FD  EMIIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-DW-IN-RECORD.
           05  ID-JURIS                    PIC S9(11)V9(05) COMP-3.
           05  ID-GROUP                    PIC S9(05) COMP-3.
           05  ID-CIRCUIT                  PIC X(13).
           05  ID-BAND                     PIC X(03).
           05  ID-TYPE                     PIC S9(13) COMP-3.
           05  ID-SEGMENT                  PIC 9(02).
           05  ID-REGION                   PIC 9(02).
           05  ID-BAN                      PIC S9(09)V9(05) COMP-3.
           05  ID-CODE                     PIC X(20).
           05  ID-JURIS2                   PIC X(04).
           05  DW-FILL-01                  PIC X(9).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-DW-VIEW1 REDEFINES CABS-DW-IN-RECORD.
           05  R0D-INVOICE                 PIC 9(02).
           05  R0D-SEQ                     PIC 9(03).
           05  R0D-INVOICE2                PIC X(16).
           05  R0D-BAND                    PIC X(06).
           05  R0D-SEQ2                    PIC 9(07).
           05  R0D-SEQ3                    PIC X(04).
           05  R0D-LEVEL                   PIC 9(03).
           05  R0D-BAND2                   PIC X(13).
           05  R0D-REST                    PIC X(26).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-DW-VIEW2 REDEFINES CABS-DW-IN-RECORD.
           05  R1D-SEQ                     PIC S9(09)V9(02) COMP-3.
           05  R1D-TARIFF                  PIC 9(07).
           05  R1D-JURIS                   PIC 9(03).
           05  R1D-TYPE                    PIC S9(09)V9(05) COMP-3.
           05  R1D-SEQ2                    PIC X(03).
           05  R1D-REGION                  PIC X(06).
           05  R1D-CIRCUIT                 PIC S9(11) COMP-3.
           05  R1D-LEVEL                   PIC S9(07) COMP-3.
           05  R1D-REGION2                 PIC X(10).
           05  R1D-REST                    PIC X(27).
      * REJOUT - PERMANENT DATASET HELD ON DASD.
       FD  REJOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-DW-OUT-RECORD.
           05  OD-OCN                      PIC S9(11) COMP-3.
           05  OD-CIRCUIT                  PIC X(10).
           05  OD-STATE                    PIC X(10).
           05  OD-STATUS                   PIC 9(03).
           05  OD-MEDIA                    PIC X(16).
           05  OD-SOURCE                   PIC 9(04).
           05  OD-LEVEL                    PIC 9(07).
           05  OD-CARRIER                  PIC S9(13)V9(02) COMP-3.
           05  OD-BAN                      PIC 9(05).
           05  OD-BAND                     PIC X(02).
           05  DW-FILL-02                  PIC X(9).
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
      * SHARED LAYOUT PULLED IN FOR THE ZONE SIDE.
       COPY CABSSETL.
      * SHARED LAYOUT PULLED IN FOR THE ZONE SIDE.
       COPY CABSCOMM.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUCV06'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V3.14'.
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
       01  WS-COUNT-AREA.
           05  WS-DW-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DW-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DW-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DW-CNT-04                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-DW-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DW-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DW-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DW-QTY-04                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-DW-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DW-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DW-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DW-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-DW-TXT-01                PIC X(20) VALUE SPACES.
           05  WS-DW-TXT-02                PIC X(16) VALUE SPACES.
           05  WS-DW-TXT-03                PIC X(26) VALUE SPACES.
           05  WS-DW-TXT-04                PIC X(08) VALUE SPACES.
           05  WS-DW-TXT-05                PIC X(30) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-DW-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-DW-ON-01                 VALUE 'Y'.
               88  WS-DW-OFF-01                VALUE 'N'.
           05  WS-DW-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-DW-ON-02                 VALUE 'Y'.
               88  WS-DW-OFF-02                VALUE 'N'.
           05  WS-DW-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-DW-ON-03                 VALUE 'Y'.
               88  WS-DW-OFF-03                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-DW-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DW-SUB-02                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. LOADED ONCE AT INITIALISATION AND SEARCHED WITH
      * A SUBSCRIPTED WALK.
       01  WS-DW-TABLE.
           05  WS-DW-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-DW-TB-ENTRY OCCURS 150 TIMES
                                       INDEXED BY WS-DW-IX.
               10  WS-DW-TB-KEY                PIC X(10).
               10  WS-DW-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-DW-TB-TXT                PIC X(30).
               10  WS-DW-TB-EFF                PIC 9(05).
               10  WS-DW-TB-EXP                PIC 9(05).
       01  WS-DW-WORK-GROUP-1.
           05  WS-DW-G1-BAN                PIC X(20).
           05  WS-DW-G1-BAND               PIC 9(07).
           05  WS-DW-G1-SEQ                PIC S9(09) COMP-3.
           05  WS-DW-G1-CYCLE              PIC 9(05).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 58.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUCV06 - EMI FORMAT CONVERSION'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-DW-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-DW-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9983.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-DW-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-DW-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT EMIIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'EMIIN OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT REJOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'REJOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SUSOUT.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'SUSOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CTLOUT OPEN FAILED - FILE STATUS BAD' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-DW-CYCLE-YYDDD.
           COMPUTE WS-DW-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-DW-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-DW-CNT-01.
           MOVE 0 TO WS-DW-CNT-02.
           MOVE 0 TO WS-DW-CNT-04.
       P1200-EXIT.
           EXIT.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-DW-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-DW-TAB-CNT NOT < 150
               MOVE 'Y' TO WS-DW-SW-01
               ADD 1 TO WS-DW-CNT-03
           ELSE
               ADD 1 TO WS-DW-TAB-CNT
               SET WS-DW-IX TO WS-DW-TAB-CNT
               MOVE ID-CODE TO WS-DW-TB-KEY (WS-DW-IX)
               MOVE 0 TO WS-DW-TB-VAL (WS-DW-IX)
               MOVE SPACES TO WS-DW-TB-TXT (WS-DW-IX)
               ADD 1 TO WS-DW-CNT-04.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ EMIIN
               AT END MOVE 'Y' TO WS-DW-SW-01.
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
           IF WS-DW-ON-03
               PERFORM P2200-CHECK-SIGN THRU P2200-CHECK-SIGN-EXIT.
           PERFORM P2300-EXPAND-CENTURY THRU P2300-EXPAND-CENTURY-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ EMIIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-CHECK-SIGN.
           MOVE SPACES TO CABS-DW-OUT-RECORD.
           MOVE ID-JURIS TO OD-OCN.
           MOVE ID-BAN TO OD-CIRCUIT.
           MOVE ID-GROUP TO OD-STATE.
           MOVE ID-BAN TO OD-STATUS.
           WRITE CABS-DW-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
           MOVE 'N' TO WS-DW-SW-03.
           IF WS-DW-TAB-CNT > 0
               PERFORM P250-COMPARE-LAYOUT THRU P250-COMPARE-LAYOUT-EXIT
               VARYING WS-DW-SUB-02 FROM 1 BY 1
               UNTIL WS-DW-SUB-02 > WS-DW-TAB-CNT
               OR WS-DW-SW-03 = 'Y'.
       P2200-CHECK-SIGN-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P2300-EXPAND-CENTURY.
           IF WS-DW-AMT-02 < 38
               MOVE 38 TO WS-DW-AMT-02
               ADD 1 TO WS-DW-CNT-01.
           IF WS-DW-AMT-02 > 78382
               MOVE 78382 TO WS-DW-AMT-02
               ADD 1 TO WS-DW-CNT-02.
       P2300-EXPAND-CENTURY-EXIT.
           EXIT.
       P250-COMPARE-LAYOUT.
           SET WS-DW-IX TO WS-DW-SUB-02.
           IF WS-DW-TB-KEY (WS-DW-IX) = ID-GROUP
               MOVE 'Y' TO WS-DW-SW-03
               MOVE WS-DW-TB-VAL (WS-DW-IX) TO WS-DW-QTY-01
               MOVE WS-DW-SUB-02 TO WS-DW-SUB-01.
       P250-COMPARE-LAYOUT-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-POST-CENTURY.
           CALL 'CABHASH' USING ID-TYPE WS-ACC-OCN-HASH.
           ADD WS-DW-CNT-03 TO WS-ACC-SEQ-HASH.
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
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE 1 TO CT-STEP-SEQ.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE WS-DW-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE SPACES TO CT-FILLER.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE 0 TO CT-RC.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-READ-CNT TO CT-READ.
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
           CLOSE EMIIN.
           CLOSE REJOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABUCV06 - NORMAL END OF JOB'.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  DW-CNT-02 = ' WS-DW-CNT-02.
           DISPLAY '  DW-CNT-04 = ' WS-DW-CNT-04.
           DISPLAY '  DW-CNT-01 = ' WS-DW-CNT-01.
           DISPLAY '  DW-CNT-03 = ' WS-DW-CNT-03.
       P9000-EXIT.
           EXIT.
