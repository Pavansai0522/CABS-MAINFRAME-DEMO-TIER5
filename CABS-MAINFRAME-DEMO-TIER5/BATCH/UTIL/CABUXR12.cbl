      *****************************************************************
      * CABUXR12 - SUSPENSE CODE CROSS REFERENCE                      *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               MSTIN   TELCABS.CABS.MSTIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               MTCOUT  TELCABS.CABS.MTCOUT         (LOCAL)     *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * REVISION HISTORY                                              *
      *   V1.00  1987-01-23  J.M.CASTILLO INITIAL RELEASE             *
      *   V1.01  1999-03-23  P.NAIR       TABLE LIMIT RAISED FOR THE  *
      *                      SOUTHEAST CENTRES                        *
      *   V1.04  2012-08-01  R.T.WHEELER  PARM CARD EXTENDED,         *
      *                      POSITIONS 40 THROUGH 48                  *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUXR12.
       AUTHOR. TELCABS APPLICATIONS - CROSS REFERENCE REPORT.
      *****************************************************************
      * SUSPENSE CODE CROSS REFERENCE. THE STEP IS DRIVEN ENTIRELY    *
      * FROM THE SYSIN PARM CARD AND THE DD ALLOCATIONS IN THE JOB.   *
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
           SELECT MSTIN ASSIGN TO UT-S-MSTIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT MTCOUT ASSIGN TO UT-S-MTCOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT CTLOUT ASSIGN TO UT-S-CTLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      * MSTIN - CATALOGUED GENERATION DATA GROUP.
       FD  MSTIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-DV-IN-RECORD.
           05  ID-TARGET                   PIC 9(07).
           05  ID-TARIFF                   PIC S9(13) COMP-3.
           05  ID-CYCLE                    PIC X(16).
           05  ID-ACCOUNT                  PIC S9(05) COMP-3.
           05  ID-SEQ                      PIC X(04).
           05  ID-ELEM                     PIC S9(07) COMP-3.
           05  ID-CARRIER                  PIC X(03).
           05  ID-SEGMENT                  PIC S9(05) COMP-3.
           05  ID-LEVEL                    PIC 9(04).
           05  DV-FILL-01                  PIC X(29).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-DV-VIEW1 REDEFINES CABS-DV-IN-RECORD.
           05  R0D-CLASS                   PIC X(04).
           05  R0D-BAND                    PIC S9(13)V9(05) COMP-3.
           05  R0D-GROUP                   PIC X(06).
           05  R0D-TARIFF                  PIC X(10).
           05  R0D-REST                    PIC X(50).
      * MTCOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  MTCOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-DV-OUT-RECORD.
           05  OD-CODE                     PIC S9(11)V9(02) COMP-3.
           05  OD-BAN                      PIC X(06).
           05  OD-STATE                    PIC 9(05).
           05  OD-ACCOUNT                  PIC X(06).
           05  OD-CYCLE                    PIC X(10).
           05  OD-LEVEL                    PIC 9(07).
           05  OD-CODE2                    PIC S9(13) COMP-3.
           05  DV-FILL-02                  PIC X(32).
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
      * SHARED LAYOUT PULLED IN FOR THE LINK SIDE.
       COPY CABSBHDR.
      * SHARED LAYOUT PULLED IN FOR THE ORPHAN SIDE.
       COPY CABSCOMM.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUXR12'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V2.25'.
           05  WS-TABLE-LIMIT              PIC S9(04) COMP-3
                                         VALUE 50.
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
           05  WS-DV-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DV-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DV-CNT-03                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-DV-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DV-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DV-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-DV-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DV-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DV-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-DV-TXT-01                PIC X(08) VALUE SPACES.
           05  WS-DV-TXT-02                PIC X(10) VALUE SPACES.
           05  WS-DV-TXT-03                PIC X(10) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-DV-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-DV-ON-01                 VALUE 'Y'.
               88  WS-DV-OFF-01                VALUE 'N'.
           05  WS-DV-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-DV-ON-02                 VALUE 'Y'.
               88  WS-DV-OFF-02                VALUE 'N'.
           05  WS-DV-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-DV-ON-03                 VALUE 'Y'.
               88  WS-DV-OFF-03                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-DV-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DV-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DV-SUB-03                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. THE LIMIT WAS RAISED WHEN THE SOUTHEAST CENTRES
      * WERE MERGED AND HAS NOT BEEN REVIEWED SINCE.
       01  WS-DV-TABLE.
           05  WS-DV-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-DV-TB-ENTRY OCCURS 50 TIMES
                                       INDEXED BY WS-DV-IX.
               10  WS-DV-TB-KEY                PIC X(08).
               10  WS-DV-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-DV-TB-TXT                PIC X(40).
               10  WS-DV-TB-EFF                PIC 9(05).
               10  WS-DV-TB-EXP                PIC 9(05).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 60.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUXR12 - SUSPENSE CODE CROSS REFERENCE'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-DV-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-DV-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9971.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-DV-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-DV-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT MSTIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'MSTIN NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               DISPLAY 'MSTIN FILE STATUS = ' WS-FS-INPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT MTCOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'MTCOUT NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               DISPLAY 'MTCOUT FILE STATUS = ' WS-FS-OUTPUT
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
           MOVE PC1-CYCLE-YYDDD TO WS-DV-CYCLE-YYDDD.
           COMPUTE WS-DV-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-DV-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-DV-CNT-03.
           MOVE 0 TO WS-DV-CNT-02.
           MOVE 0 TO WS-DV-CNT-01.
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
           IF WS-DV-ON-02
               PERFORM P2200-DERIVE-PAIR THRU P2200-DERIVE-PAIR-EXIT.
           PERFORM P2300-EXPAND-SIDE THRU P2300-EXPAND-SIDE-EXIT.
           PERFORM P2400-EDIT-SIDE THRU P2400-EDIT-SIDE-EXIT.
           IF WS-DV-ON-02
               PERFORM P2500-BUILD-PAIR THRU P2500-BUILD-PAIR-EXIT.
           PERFORM P2600-SELECT-ORPHAN THRU P2600-SELECT-ORPHAN-EXIT.
           PERFORM P2700-VALIDATE-PAIR THRU P2700-VALIDATE-PAIR-EXIT.
           PERFORM P2800-BUILD-LINK THRU P2800-BUILD-LINK-EXIT.
           PERFORM P2900-VALIDATE-SIDE THRU P2900-VALIDATE-SIDE-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ MSTIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P2200-DERIVE-PAIR.
           IF ID-CARRIER = 'D'
               ADD 1 TO WS-DV-CNT-02
           ELSE
               IF ID-CARRIER = 'S'
                   ADD 1 TO WS-DV-CNT-02
               ELSE
                   IF ID-CARRIER = 'E'
                       ADD 1 TO WS-DV-CNT-01
                   ELSE
                       ADD 1 TO WS-DV-CNT-01.
       P2200-DERIVE-PAIR-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P2300-EXPAND-SIDE.
           MOVE 'N' TO WS-DV-SW-02.
           IF WS-DV-TXT-02 NOT = WS-DV-TXT-02
               MOVE 'Y' TO WS-DV-SW-02
               MOVE WS-DV-TXT-02 TO WS-DV-TXT-02
               ADD 1 TO WS-DV-CNT-01.
       P2300-EXPAND-SIDE-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P2400-EDIT-SIDE.
           MOVE SPACES TO CABS-DV-OUT-RECORD.
           MOVE ID-CYCLE TO OD-CODE.
           MOVE ID-CYCLE TO OD-BAN.
           MOVE ID-TARIFF TO OD-STATE.
           MOVE ID-ELEM TO OD-ACCOUNT.
           MOVE ID-CARRIER TO OD-CYCLE.
           WRITE CABS-DV-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P2400-EDIT-SIDE-EXIT.
           EXIT.
       P2500-BUILD-PAIR.
           CALL 'CABSEQCK' USING WS-DV-TXT-03 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-DV-CNT-02.
       P2500-BUILD-PAIR-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P2600-SELECT-ORPHAN.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-DV-TXT-01 TO PC-COL-001-020.
           MOVE WS-DV-TXT-03 TO PC-COL-021-060.
           MOVE WS-DV-AMT-01 TO WS-DV-AMT-EDIT.
           MOVE WS-DV-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P2600-SELECT-ORPHAN-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P2700-VALIDATE-PAIR.
           UNSTRING WS-DV-TXT-03 DELIMITED BY '/'
               INTO WS-DV-TXT-01
               WS-DV-TXT-03
               TALLYING IN WS-DV-CNT-03.
       P2700-VALIDATE-PAIR-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P2800-BUILD-LINK.
           CALL 'CABHASH' USING ID-CYCLE WS-ACC-OCN-HASH.
           ADD WS-DV-CNT-02 TO WS-ACC-SEQ-HASH.
       P2800-BUILD-LINK-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P2900-VALIDATE-SIDE.
           MOVE 'Y' TO WS-DV-SW-02.
           IF ID-TARIFF < 21
               MOVE 'N' TO WS-DV-SW-02
               ADD 1 TO WS-DV-CNT-02.
           IF ID-TARIFF > 2541
               MOVE 'N' TO WS-DV-SW-02
               ADD 1 TO WS-DV-CNT-03.
       P2900-VALIDATE-SIDE-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-POST-PAIR.
           MOVE ID-SEGMENT TO WS-DV-TXT-02.
           MOVE ID-CARRIER TO WS-DV-TXT-02.
           MOVE ID-TARIFF TO WS-DV-TXT-03.
           MOVE ID-TARIFF TO WS-DV-TXT-03.
           ADD 1 TO WS-DV-CNT-03.
       P3100-POST-PAIR-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P3200-CLOSE-OFF-SIDE.
           MOVE SPACES TO CABS-DV-OUT-RECORD.
           MOVE ID-ACCOUNT TO OD-CODE.
           MOVE ID-SEGMENT TO OD-BAN.
           MOVE ID-CARRIER TO OD-STATE.
           MOVE ID-TARGET TO OD-ACCOUNT.
           MOVE ID-TARIFF TO OD-CYCLE.
           MOVE ID-SEGMENT TO OD-LEVEL.
           MOVE ID-TARGET TO OD-CODE2.
           WRITE CABS-DV-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P3200-CLOSE-OFF-SIDE-EXIT.
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
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE WS-DV-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE 3 TO CT-STEP-SEQ.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE WS-DV-TXT-01 TO CT-RESTART-KEY.
           MOVE SPACES TO CT-FILLER.
           MOVE WS-DV-CNT-02 TO CT-RC.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
       P8100-EXIT.
           EXIT.
      * P8200-CHECK-BALANCE - A RECORD THAT WAS HELD FOR THE NEXT
      * CYCLE IS BROUGHT INTO THE CARRIED FORWARD SIDE BEFORE THE
      * EQUATION IS TESTED.
       P8200-CHECK-BALANCE.
           ADD WS-DV-CNT-02 TO CT-CARRIED-FWD.
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
           CLOSE MSTIN.
           CLOSE MTCOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABUXR12 - STEP COMPLETE'.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  DV-CNT-01 = ' WS-DV-CNT-01.
           DISPLAY '  DV-CNT-02 = ' WS-DV-CNT-02.
           DISPLAY '  DV-CNT-03 = ' WS-DV-CNT-03.
       P9000-EXIT.
           EXIT.
