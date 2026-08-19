      *****************************************************************
      * CABURT03 - RATE ELEMENT DESCRIPTION MAINTENANCE               *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               BNDIN   TELCABS.CABS.BNDIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               ELMOUT  TELCABS.CABS.ELMOUT         (LOCAL)     *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - THE OUTPUT IS REBUILT EACH RUN     *
      * REVISION HISTORY                                              *
      *   V1.00  1990-09-24  C.ADEYEMI    INITIAL RELEASE             *
      *   V1.01  1998-10-16  T.YAMASHITA  PARM CARD EXTENDED,         *
      *                      POSITIONS 40 THROUGH 48                  *
      *   V1.05  2009-12-02  J.M.CASTILLO BLOCK SIZE SET TO ZERO -    *
      *                      SYSTEM DETERMINED                        *
      *   V1.09  2016-10-19  C.ADEYEMI    RECOMPILE ONLY - COPYBOOK   *
      *                      CHANGE UPSTREAM                          *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABURT03.
       AUTHOR. TELCABS APPLICATIONS - RATE TABLE MAINTENANCE.
      *****************************************************************
      * RATE ELEMENT DESCRIPTION MAINTENANCE. THIS STEP IS SCHEDULED  *
      * INSIDE THE NIGHTLY ACCESS BILLING STREAM AND HAS NO           *
      * INTERACTIVE ENTRY POINT.                                      *
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO   *
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.                     *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           C01 IS TO-NEW-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BNDIN ASSIGN TO UT-S-BNDIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT ELMOUT ASSIGN TO UT-S-ELMOUT
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
      * BNDIN - PERMANENT DATASET HELD ON DASD.
       FD  BNDIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 110 CHARACTERS.
       01  CABS-BM-IN-RECORD.
           05  IB-CIRCUIT                  PIC 9(06).
           05  IB-SEGMENT                  PIC S9(13)V9(05) COMP-3.
           05  IB-MEDIA                    PIC S9(15) COMP-3.
           05  IB-CARRIER                  PIC S9(13)V9(02) COMP-3.
           05  IB-CODE                     PIC X(08).
           05  IB-STATE                    PIC X(10).
           05  IB-BAN                      PIC X(20).
           05  IB-SOURCE                   PIC X(03).
           05  IB-PERIOD                   PIC X(20).
           05  IB-CARRIER2                 PIC X(13).
           05  BM-FILL-01                  PIC X(4).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-BM-VIEW1 REDEFINES CABS-BM-IN-RECORD.
           05  R0B-PERIOD                  PIC S9(13) COMP-3.
           05  R0B-STATUS                  PIC 9(03).
           05  R0B-REGION                  PIC X(06).
           05  R0B-CLASS                   PIC X(16).
           05  R0B-PERIOD2                 PIC S9(07)V9(02) COMP-3.
           05  R0B-TARIFF                  PIC X(08).
           05  R0B-INVOICE                 PIC X(16).
           05  R0B-CENTRE                  PIC S9(05) COMP-3.
           05  R0B-OCN                     PIC 9(06).
           05  R0B-REST                    PIC X(40).
      * ELMOUT - PERMANENT DATASET HELD ON DASD.
       FD  ELMOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 90 CHARACTERS.
       01  CABS-BM-OUT-RECORD.
           05  OB-STATUS                   PIC 9(09).
           05  OB-SEGMENT                  PIC S9(13) COMP-3.
           05  OB-INVOICE                  PIC X(13).
           05  OB-CYCLE                    PIC 9(09).
           05  OB-ELEM                     PIC X(10).
           05  OB-LEVEL                    PIC 9(07).
           05  OB-TYPE                     PIC X(04).
           05  OB-TYPE2                    PIC S9(09)V9(02) COMP-3.
           05  OB-CENTRE                   PIC 9(07).
           05  OB-GROUP                    PIC X(16).
           05  BM-FILL-02                  PIC X(2).
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
      * SHARED LAYOUT PULLED IN FOR THE OVERRIDE SIDE.
       COPY CABSRT01.
      * SHARED LAYOUT PULLED IN FOR THE WINDOW SIDE.
       COPY CABSCOMM.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABURT03'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V1.28'.
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
           05  WS-BM-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BM-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BM-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BM-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BM-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BM-CNT-06                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-BM-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BM-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BM-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BM-QTY-04                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-BM-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BM-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-BM-TXT-01                PIC X(12) VALUE SPACES.
           05  WS-BM-TXT-02                PIC X(16) VALUE SPACES.
           05  WS-BM-TXT-03                PIC X(20) VALUE SPACES.
           05  WS-BM-TXT-04                PIC X(20) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-BM-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-BM-ON-01                 VALUE 'Y'.
               88  WS-BM-OFF-01                VALUE 'N'.
           05  WS-BM-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-BM-ON-02                 VALUE 'Y'.
               88  WS-BM-OFF-02                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-BM-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-BM-SUB-02                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. THE LIMIT WAS RAISED WHEN THE SOUTHEAST CENTRES
      * WERE MERGED AND HAS NOT BEEN REVIEWED SINCE.
       01  WS-BM-TABLE.
           05  WS-BM-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-BM-TB-ENTRY OCCURS 120 TIMES
                                       INDEXED BY WS-BM-IX.
               10  WS-BM-TB-KEY                PIC X(04).
               10  WS-BM-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-BM-TB-TXT                PIC X(20).
               10  WS-BM-TB-EFF                PIC 9(05).
               10  WS-BM-TB-EXP                PIC 9(05).
       01  WS-BM-WORK-GROUP-1.
           05  WS-BM-G1-ELEM               PIC 9(05).
           05  WS-BM-G1-CODE               PIC S9(09) COMP-3.
           05  WS-BM-G1-MEDIA              PIC 9(05).
           05  WS-BM-G1-CIRCUIT            PIC S9(09) COMP-3.
           05  WS-BM-G1-CARRIER            PIC X(20).
           05  WS-BM-G1-JURIS              PIC S9(09) COMP-3.
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 50.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABURT03 - RATE ELEMENT DESCRIPTION MAINTENANCE'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-BM-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-BM-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9966.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-BM-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-BM-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT BNDIN.
           IF WS-FS-INPUT NOT = '00'
               DISPLAY 'BNDIN FILE STATUS = ' WS-FS-INPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BNDIN NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT ELMOUT.
           IF WS-FS-OUTPUT NOT = '00'
               DISPLAY 'ELMOUT FILE STATUS = ' WS-FS-OUTPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'ELMOUT NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SUSOUT.
           IF WS-FS-SUSPENSE NOT = '00'
               DISPLAY 'SUSOUT FILE STATUS = ' WS-FS-SUSPENSE
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'SUSOUT NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               DISPLAY 'CTLOUT FILE STATUS = ' WS-FS-CONTROL
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CTLOUT NOT AVAILABLE - OPEN REJECTED' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-BM-CYCLE-YYDDD.
           COMPUTE WS-BM-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-BM-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-BM-CNT-02.
           MOVE 0 TO WS-BM-CNT-05.
           MOVE 0 TO WS-BM-CNT-01.
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
           IF WS-BM-ON-02
               PERFORM P2200-BUILD-TARIFF THRU P2200-BUILD-TARIFF-EXIT.
           IF WS-BM-ON-01
               PERFORM P2300-VALIDATE-OVERRIDE THRU
                   P2300-VALIDATE-OVERRIDE-EXIT.
           PERFORM P2400-APPLY-ROW THRU P2400-APPLY-ROW-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ BNDIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT
      * PRECEDES THIS PROGRAM IN THE JOB.
       P2200-BUILD-TARIFF.
           IF IB-SOURCE = 'D'
               ADD 1 TO WS-BM-CNT-06
           ELSE
               IF IB-SOURCE = 'A'
                   ADD 1 TO WS-BM-CNT-03
               ELSE
                   IF IB-SOURCE = 'E'
                       ADD 1 TO WS-BM-CNT-05
                   ELSE
                       ADD 1 TO WS-BM-CNT-02.
       P2200-BUILD-TARIFF-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P2300-VALIDATE-OVERRIDE.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-OCN-UNKNOWN TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-BM-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
       P2300-VALIDATE-OVERRIDE-EXIT.
           EXIT.
       P2400-APPLY-ROW.
           MOVE 0 TO WS-BM-CNT-04.
           INSPECT WS-BM-TXT-01 TALLYING WS-BM-CNT-04
               FOR ALL SPACES.
           INSPECT WS-BM-TXT-01 REPLACING ALL LOW-VALUES BY SPACES.
       P2400-APPLY-ROW-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-POST-OVERRIDE.
           MOVE IB-SOURCE TO WS-BM-TXT-01.
           MOVE IB-SEGMENT TO WS-BM-TXT-03.
           MOVE IB-SEGMENT TO WS-BM-TXT-03.
           MOVE IB-STATE TO WS-BM-TXT-01.
           ADD 1 TO WS-BM-CNT-01.
       P3100-POST-OVERRIDE-EXIT.
           EXIT.
       P3200-POST-ELEMENT.
           MOVE SPACES TO WS-BM-TXT-03.
           STRING IB-CODE DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IB-SEGMENT DELIMITED BY SIZE
               INTO WS-BM-TXT-03.
       P3200-POST-ELEMENT-EXIT.
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
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-BM-CNT-03 TO CT-RC.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE SPACES TO CT-FILLER.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE 5 TO CT-STEP-SEQ.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE WS-BM-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
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
           CLOSE BNDIN.
           CLOSE ELMOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABURT03 - NORMAL END OF JOB'.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  BM-CNT-05 = ' WS-BM-CNT-05.
           DISPLAY '  BM-CNT-06 = ' WS-BM-CNT-06.
           DISPLAY '  BM-CNT-02 = ' WS-BM-CNT-02.
       P9000-EXIT.
           EXIT.
