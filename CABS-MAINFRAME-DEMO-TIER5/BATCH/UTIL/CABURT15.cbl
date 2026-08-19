      *****************************************************************
      * CABURT15 - RATE OVERRIDE TABLE LOAD                           *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               TBLIN   TELCABS.CABS.TBLIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               BNDOUT  TELCABS.CABS.BNDOUT         (LOCAL)     *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * REVISION HISTORY                                              *
      *   V1.00  1990-02-28  M.DELACROIX  INITIAL RELEASE             *
      *   V1.03  2004-04-24  A.BUKOWSKI   HASH TOTAL ADDED TO THE     *
      *                      CONTROL RECORD                           *
      *   V1.04  2009-01-22  L.FERREIRA   PRINT LINE WIDENED TO 133   *
      *   V1.07  2016-05-07  T.YAMASHITA  OCCURS RAISED AFTER THE     *
      *                      FEBRUARY OVERFLOW                        *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABURT15.
       AUTHOR. TELCABS APPLICATIONS - RATE TABLE MAINTENANCE.
      *****************************************************************
      * RATE OVERRIDE TABLE LOAD. THE STEP IS DRIVEN ENTIRELY FROM THE*
      * SYSIN PARM CARD AND THE DD ALLOCATIONS IN THE JOB.            *
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
           SELECT TBLIN ASSIGN TO UT-S-TBLIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT BNDOUT ASSIGN TO UT-S-BNDOUT
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
      * TBLIN - WORK FILE, DELETED AT STEP END.
       FD  TBLIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-EB-IN-RECORD.
           05  IE-ACCOUNT                  PIC X(06).
           05  IE-BAN                      PIC 9(03).
           05  IE-CLASS                    PIC X(16).
           05  IE-CARRIER                  PIC S9(07)V9(05) COMP-3.
           05  IE-LEVEL                    PIC S9(11) COMP-3.
           05  IE-TARGET                   PIC S9(15) COMP-3.
           05  IE-JURIS                    PIC X(04).
           05  IE-JURIS2                   PIC 9(09).
           05  IE-CENTRE                   PIC S9(09) COMP-3.
           05  IE-STATUS                   PIC S9(07)V9(02) COMP-3.
           05  IE-SEGMENT                  PIC 9(05).
           05  EB-FILL-01                  PIC X(6).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-EB-VIEW1 REDEFINES CABS-EB-IN-RECORD.
           05  R0E-MEDIA                   PIC S9(11) COMP-3.
           05  R0E-BAN                     PIC S9(09)V9(02) COMP-3.
           05  R0E-ELEM                    PIC X(06).
           05  R0E-BAND                    PIC X(08).
           05  R0E-CYCLE                   PIC X(10).
           05  R0E-GROUP                   PIC 9(03).
           05  R0E-BAND2                   PIC S9(13)V9(02) COMP-3.
           05  R0E-REST                    PIC X(33).
      * BNDOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  BNDOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-EB-OUT-RECORD.
           05  OE-TARIFF                   PIC 9(05).
           05  OE-GROUP                    PIC X(08).
           05  OE-ACCOUNT                  PIC X(02).
           05  OE-INVOICE                  PIC X(08).
           05  OE-PERIOD                   PIC S9(11) COMP-3.
           05  OE-CLASS                    PIC S9(07)V9(05) COMP-3.
           05  OE-JURIS                    PIC X(20).
           05  OE-MEDIA                    PIC X(02).
           05  OE-OCN                      PIC S9(07)V9(05) COMP-3.
           05  EB-FILL-02                  PIC X(15).
      * SUSOUT - WORK FILE, DELETED AT STEP END.
       FD  SUSOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 300 CHARACTERS.
       01  CABS-SUSOUT-RECORD              PIC X(300).
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
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABURT15'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V2.09'.
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
           05  WS-EB-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-EB-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-EB-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-EB-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-EB-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-EB-CNT-06                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-EB-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-EB-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-EB-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-EB-QTY-04                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-EB-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-EB-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-EB-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-EB-TXT-01                PIC X(10) VALUE SPACES.
           05  WS-EB-TXT-02                PIC X(20) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-EB-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-EB-ON-01                 VALUE 'Y'.
               88  WS-EB-OFF-01                VALUE 'N'.
           05  WS-EB-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-EB-ON-02                 VALUE 'Y'.
               88  WS-EB-OFF-02                VALUE 'N'.
           05  WS-EB-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-EB-ON-03                 VALUE 'Y'.
               88  WS-EB-OFF-03                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-EB-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-EB-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-EB-SUB-03                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. THE LIMIT WAS RAISED WHEN THE SOUTHEAST CENTRES
      * WERE MERGED AND HAS NOT BEEN REVIEWED SINCE.
       01  WS-EB-TABLE.
           05  WS-EB-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-EB-TB-ENTRY OCCURS 100 TIMES
                                       INDEXED BY WS-EB-IX.
               10  WS-EB-TB-KEY                PIC X(10).
               10  WS-EB-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-EB-TB-TXT                PIC X(30).
               10  WS-EB-TB-EFF                PIC 9(05).
               10  WS-EB-TB-EXP                PIC 9(05).
       01  WS-EB-WORK-GROUP-1.
           05  WS-EB-G1-CYCLE              PIC S9(09) COMP-3.
           05  WS-EB-G1-OCN                PIC X(20).
           05  WS-EB-G1-CENTRE             PIC S9(11)V9(02) COMP-3.
           05  WS-EB-G1-CENTRE             PIC X(20).
           05  WS-EB-G1-SEQ                PIC S9(09) COMP-3.
           05  WS-EB-G1-CENTRE             PIC 9(05).
           05  WS-EB-G1-CYCLE              PIC 9(07).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 58.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABURT15 - RATE OVERRIDE TABLE LOAD'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-EB-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-EB-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9951.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-EB-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-EB-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT TBLIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON TBLIN - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT BNDOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON BNDOUT - CHECK THE ALLOCATION' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-EB-CYCLE-YYDDD.
           COMPUTE WS-EB-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-EB-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-EB-CNT-04.
           MOVE 0 TO WS-EB-CNT-05.
           MOVE 0 TO WS-EB-CNT-02.
       P1200-EXIT.
           EXIT.
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT
      * PRECEDES THIS PROGRAM IN THE JOB.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-EB-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-EB-TAB-CNT NOT < 100
               MOVE 'Y' TO WS-EB-SW-01
               ADD 1 TO WS-EB-CNT-04
           ELSE
               ADD 1 TO WS-EB-TAB-CNT
               SET WS-EB-IX TO WS-EB-TAB-CNT
               MOVE IE-CLASS TO WS-EB-TB-KEY (WS-EB-IX)
               MOVE 0 TO WS-EB-TB-VAL (WS-EB-IX)
               MOVE SPACES TO WS-EB-TB-TXT (WS-EB-IX)
               ADD 1 TO WS-EB-CNT-02.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ TBLIN
               AT END MOVE 'Y' TO WS-EB-SW-01.
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
           PERFORM P2200-BUILD-ELEMENT THRU P2200-BUILD-ELEMENT-EXIT.
           PERFORM P2300-CONVERT-TARIFF THRU P2300-CONVERT-TARIFF-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ TBLIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P2200-BUILD-ELEMENT.
           IF WS-EB-AMT-03 < 21
               MOVE 21 TO WS-EB-AMT-03
               ADD 1 TO WS-EB-CNT-01.
           IF WS-EB-AMT-03 > 41669
               MOVE 41669 TO WS-EB-AMT-03
               ADD 1 TO WS-EB-CNT-06.
           MOVE 'N' TO WS-EB-SW-03.
           IF WS-EB-TAB-CNT > 0
               PERFORM P270-COMPARE-BAND THRU P270-COMPARE-BAND-EXIT
               VARYING WS-EB-SUB-02 FROM 1 BY 1
               UNTIL WS-EB-SUB-02 > WS-EB-TAB-CNT
               OR WS-EB-SW-03 = 'Y'.
       P2200-BUILD-ELEMENT-EXIT.
           EXIT.
       P2300-CONVERT-TARIFF.
           MOVE 0 TO WS-EB-CNT-04.
           INSPECT WS-EB-TXT-01 TALLYING WS-EB-CNT-04
               FOR ALL SPACES.
           INSPECT WS-EB-TXT-01 REPLACING ALL LOW-VALUES BY SPACES.
       P2300-CONVERT-TARIFF-EXIT.
           EXIT.
       P270-COMPARE-BAND.
           SET WS-EB-IX TO WS-EB-SUB-02.
           IF WS-EB-TB-KEY (WS-EB-IX) = IE-JURIS
               MOVE 'Y' TO WS-EB-SW-01
               MOVE WS-EB-TB-VAL (WS-EB-IX) TO WS-EB-QTY-02
               MOVE WS-EB-SUB-02 TO WS-EB-SUB-01.
       P270-COMPARE-BAND-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P3100-EMIT-KEY.
           MOVE IE-BAN TO WS-EB-TXT-01.
           MOVE IE-CARRIER TO WS-EB-TXT-02.
           ADD 1 TO WS-EB-CNT-04.
       P3100-EMIT-KEY-EXIT.
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
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE 2 TO CT-STEP-SEQ.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE WS-EB-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
       P8100-EXIT.
           EXIT.
      * P8200-CHECK-BALANCE - A RECORD THAT WAS HELD FOR THE NEXT
      * CYCLE IS BROUGHT INTO THE CARRIED FORWARD SIDE BEFORE THE
      * EQUATION IS TESTED.
       P8200-CHECK-BALANCE.
           ADD WS-EB-CNT-03 TO CT-CARRIED-FWD.
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
           CLOSE TBLIN.
           CLOSE BNDOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABURT15 - RUN COMPLETE'.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  EB-CNT-06 = ' WS-EB-CNT-06.
           DISPLAY '  EB-CNT-05 = ' WS-EB-CNT-05.
           DISPLAY '  EB-CNT-01 = ' WS-EB-CNT-01.
           DISPLAY '  EB-CNT-04 = ' WS-EB-CNT-04.
       P9000-EXIT.
           EXIT.
