      *****************************************************************
      * CABUXR01 - CIRCUIT TO ACCOUNT CROSS REFERENCE                 *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               RGTIN   TELCABS.CABS.RGTIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               LNKOUT  TELCABS.CABS.LNKOUT         (LOCAL)     *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - THE OUTPUT IS REBUILT EACH RUN     *
      * REVISION HISTORY                                              *
      *   V1.00  1992-03-15  J.M.CASTILLO INITIAL RELEASE             *
      *   V1.01  1994-01-08  W.J.MCALLISTER PRINT LINE WIDENED TO 133 *
      *   V1.03  2001-03-23  S.MARCHETTI  RETIRED THE SECOND SORT STEP*
      *                      - DONE IN PROGRAM                        *
      *   V1.07  2019-03-24  C.ADEYEMI    SUSPENSE WRITE ADDED -      *
      *                      RECORDS WERE BEING DROPPED               *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUXR01.
       AUTHOR. TELCABS APPLICATIONS - CROSS REFERENCE REPORT.
      *****************************************************************
      * CIRCUIT TO ACCOUNT CROSS REFERENCE. THE STEP IS SCHEDULED     *
      * MONTHLY AND ALSO RUN ON DEMAND WHEN A CENTRE ASKS FOR IT.     *
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT     *
      * PRECEDES THIS PROGRAM IN THE JOB.                             *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           C01 IS TO-NEW-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RGTIN ASSIGN TO UT-S-RGTIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT LNKOUT ASSIGN TO UT-S-LNKOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT CTLOUT ASSIGN TO UT-S-CTLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      * RGTIN - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  RGTIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 100 CHARACTERS.
       01  CABS-CR-IN-RECORD.
           05  IC-CARRIER                  PIC S9(13)V9(05) COMP-3.
           05  IC-PERIOD                   PIC X(16).
           05  IC-LEVEL                    PIC S9(07)V9(02) COMP-3.
           05  IC-SOURCE                   PIC 9(02).
           05  IC-TYPE                     PIC X(02).
           05  IC-OCN                      PIC X(06).
           05  IC-REGION                   PIC 9(09).
           05  IC-BAN                      PIC S9(09) COMP-3.
           05  IC-TYPE2                    PIC X(16).
           05  IC-LEVEL2                   PIC S9(09)V9(05) COMP-3.
           05  IC-STATE                    PIC S9(13)V9(02) COMP-3.
           05  IC-OCN2                     PIC X(02).
           05  IC-CARRIER2                 PIC X(03).
           05  IC-LEVEL3                   PIC X(04).
           05  CR-FILL-01                  PIC X(4).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-CR-VIEW1 REDEFINES CABS-CR-IN-RECORD.
           05  R0C-SEQ                     PIC X(13).
           05  R0C-TARGET                  PIC X(13).
           05  R0C-TARIFF                  PIC S9(11)V9(02) COMP-3.
           05  R0C-CLASS                   PIC X(13).
           05  R0C-MEDIA                   PIC 9(03).
           05  R0C-REST                    PIC X(51).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-CR-VIEW2 REDEFINES CABS-CR-IN-RECORD.
           05  R1C-CARRIER                 PIC S9(13)V9(05) COMP-3.
           05  R1C-PERIOD                  PIC S9(07)V9(02) COMP-3.
           05  R1C-TYPE                    PIC S9(15) COMP-3.
           05  R1C-SEQ                     PIC 9(02).
           05  R1C-REST                    PIC X(75).
      * LNKOUT - PERMANENT DATASET HELD ON DASD.
       FD  LNKOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-CR-OUT-RECORD.
           05  OC-SEQ                      PIC X(08).
           05  OC-SEGMENT                  PIC S9(05) COMP-3.
           05  OC-BAN                      PIC 9(02).
           05  OC-CENTRE                   PIC 9(07).
           05  OC-BAND                     PIC 9(06).
           05  OC-CLASS                    PIC X(06).
           05  OC-BAN2                     PIC X(08).
           05  CR-FILL-02                  PIC X(40).
      * CTLOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD              PIC X(180).
       WORKING-STORAGE SECTION.
      * STANDARD SHARED WORKING STORAGE. SEE CABSWRK.
       COPY CABSWRK.
      * SHARED LAYOUT PULLED IN FOR THE GROUP SIDE.
       COPY CABSCOMM.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUXR01'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V1.10'.
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
       01  WS-PARM-CARD-R2 REDEFINES WS-PARM-CARD.
           05  PC2-LEAD                    PIC X(14).
           05  PC2-CYCLE-VIEW.
               10  PC2-CV-YY                   PIC 9(02).
               10  PC2-CV-DDD                  PIC 9(03).
           05  PC2-REST                    PIC X(61).
       01  WS-COUNT-AREA.
           05  WS-CR-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CR-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CR-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CR-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CR-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CR-CNT-06                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-CR-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CR-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-CR-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CR-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CR-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CR-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-CR-TXT-01                PIC X(10) VALUE SPACES.
           05  WS-CR-TXT-02                PIC X(10) VALUE SPACES.
           05  WS-CR-TXT-03                PIC X(20) VALUE SPACES.
           05  WS-CR-TXT-04                PIC X(12) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-CR-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-CR-ON-01                 VALUE 'Y'.
               88  WS-CR-OFF-01                VALUE 'N'.
           05  WS-CR-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-CR-ON-02                 VALUE 'Y'.
               88  WS-CR-OFF-02                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-CR-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CR-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CR-SUB-03                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. THE LIMIT WAS RAISED WHEN THE SOUTHEAST CENTRES
      * WERE MERGED AND HAS NOT BEEN REVIEWED SINCE.
       01  WS-CR-TABLE.
           05  WS-CR-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-CR-TB-ENTRY OCCURS 50 TIMES
                                       INDEXED BY WS-CR-IX.
               10  WS-CR-TB-KEY                PIC X(10).
               10  WS-CR-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-CR-TB-TXT                PIC X(20).
               10  WS-CR-TB-EFF                PIC 9(05).
               10  WS-CR-TB-EXP                PIC 9(05).
       01  WS-CR-WORK-GROUP-1.
           05  WS-CR-G1-TYPE               PIC S9(11)V9(02) COMP-3.
           05  WS-CR-G1-INVOICE            PIC X(20).
           05  WS-CR-G1-GROUP              PIC X(20).
           05  WS-CR-G1-SOURCE             PIC S9(11)V9(02) COMP-3.
           05  WS-CR-G1-ELEM               PIC X(20).
           05  WS-CR-G1-GROUP              PIC S9(09) COMP-3.
           05  WS-CR-G1-STATE              PIC 9(07).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 50.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUXR01 - CIRCUIT TO ACCOUNT CROSS REFERENCE'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-CR-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-CR-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9975.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-CR-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-CR-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT RGTIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON RGTIN - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT LNKOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON LNKOUT - CHECK THE ALLOCATION' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-CR-CYCLE-YYDDD.
           COMPUTE WS-CR-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-CR-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-CR-CNT-02.
           MOVE 0 TO WS-CR-CNT-03.
           MOVE 0 TO WS-CR-CNT-04.
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
           PERFORM P2200-EDIT-ORPHAN THRU P2200-EDIT-ORPHAN-EXIT.
           IF WS-CR-ON-02
               PERFORM P2300-MATCH-SIDE THRU P2300-MATCH-SIDE-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ RGTIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-EDIT-ORPHAN.
           MOVE SPACES TO CABS-CR-OUT-RECORD.
           MOVE IC-TYPE2 TO OC-SEQ.
           MOVE IC-CARRIER2 TO OC-SEGMENT.
           MOVE IC-TYPE TO OC-BAN.
           MOVE IC-PERIOD TO OC-CENTRE.
           MOVE IC-LEVEL2 TO OC-BAND.
           MOVE IC-REGION TO OC-CLASS.
           MOVE IC-CARRIER TO OC-BAN2.
           WRITE CABS-CR-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P2200-EDIT-ORPHAN-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P2300-MATCH-SIDE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-CR-TXT-02 TO PC-COL-001-020.
           MOVE WS-CR-TXT-03 TO PC-COL-021-060.
           MOVE WS-CR-AMT-03 TO WS-CR-AMT-EDIT.
           MOVE WS-CR-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P2300-MATCH-SIDE-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-CLOSE-OFF-REFERENCE.
           CALL 'CABHASH' USING IC-LEVEL WS-ACC-OCN-HASH.
           ADD WS-CR-CNT-03 TO WS-ACC-SEQ-HASH.
       P3100-CLOSE-OFF-REFERENCE-EXIT.
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
           MOVE WS-CR-CNT-02 TO CT-RC.
           MOVE WS-CR-TXT-01 TO CT-RESTART-KEY.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE 7 TO CT-STEP-SEQ.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE WS-CR-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
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
           ADD WS-CR-CNT-05 TO CT-CARRIED-FWD.
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
           CLOSE RGTIN.
           CLOSE LNKOUT.
           CLOSE CTLOUT.
           DISPLAY 'CABUXR01 - RUN COMPLETE'.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  CR-CNT-01 = ' WS-CR-CNT-01.
           DISPLAY '  CR-CNT-05 = ' WS-CR-CNT-05.
           DISPLAY '  CR-CNT-06 = ' WS-CR-CNT-06.
       P9000-EXIT.
           EXIT.
