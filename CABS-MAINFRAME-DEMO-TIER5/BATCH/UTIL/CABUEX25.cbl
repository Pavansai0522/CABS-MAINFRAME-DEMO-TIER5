      *****************************************************************
      * CABUEX25 - FACTOR STUDY EXTRACT                               *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               USGIN   TELCABS.CABS.USGIN          (LOCAL)     *
      *               MSTIN   TELCABS.CABS.MSTIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               FEEDOUT TELCABS.CABS.FEEDOU         (LOCAL)     *
      *               DROPOUT TELCABS.CABS.DROPOU         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - NOTHING IS UPDATED IN PLACE        *
      * REVISION HISTORY                                              *
      *   V1.00  2005-10-21  A.BUKOWSKI   INITIAL RELEASE             *
      *   V1.02  2010-11-22  J.M.CASTILLO REPORT PAGINATION CORRECTED *
      *   V1.05  2014-04-23  P.NAIR       CONTROL RECORD ADDED PER    *
      *                      CABS-STD-002                             *
      *   V1.06  2015-09-07  P.NAIR       JOB PARAMETER MADE MANDATORY*
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUEX25.
       AUTHOR. TELCABS APPLICATIONS - DATASET EXTRACT.
      *****************************************************************
      * FACTOR STUDY EXTRACT. THE STEP IS DRIVEN ENTIRELY FROM THE    *
      * SYSIN PARM CARD AND THE DD ALLOCATIONS IN THE JOB.            *
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
           SELECT USGIN ASSIGN TO UT-S-USGIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT MSTIN ASSIGN TO UT-S-MSTIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT FEEDOUT ASSIGN TO UT-S-FEEDOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT DROPOUT ASSIGN TO UT-S-DROPOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT CTLOUT ASSIGN TO UT-S-CTLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CONTROL.
           SELECT RPTOUT ASSIGN TO UT-S-RPTOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
       DATA DIVISION.
       FILE SECTION.
      * USGIN - PERMANENT DATASET HELD ON DASD.
       FD  USGIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 120 CHARACTERS.
       01  CABS-AH-IN-RECORD.
           05  IA-CYCLE                    PIC 9(07).
           05  IA-CARRIER                  PIC X(02).
           05  IA-BAN                      PIC X(16).
           05  IA-INVOICE                  PIC X(16).
           05  IA-TYPE                     PIC X(08).
           05  IA-BAND                     PIC X(20).
           05  IA-CLASS                    PIC 9(03).
           05  IA-ELEM                     PIC X(13).
           05  IA-BAN2                     PIC X(20).
           05  IA-ELEM2                    PIC 9(05).
           05  IA-BAND2                    PIC X(02).
           05  IA-JURIS                    PIC S9(09) COMP-3.
           05  IA-TARIFF                   PIC 9(02).
           05  AH-FILL-01                  PIC X(1).
      * MSTIN - PERMANENT DATASET HELD ON DASD.
       FD  MSTIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 120 CHARACTERS.
       01  CABS-AH-ALT1-RECORD.
           05  A1-OCN                      PIC S9(07)V9(02) COMP-3.
           05  A1-TARIFF                   PIC 9(03).
           05  A1-INVOICE                  PIC S9(09) COMP-3.
           05  A1-INVOICE2                 PIC S9(09)V9(05) COMP-3.
           05  A1-TYPE                     PIC X(02).
           05  A1-CARRIER                  PIC S9(09) COMP-3.
           05  AH-FILL-02                  PIC X(92).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-AH-VIEW1 REDEFINES CABS-AH-IN-RECORD.
           05  R0A-ACCOUNT                 PIC S9(05) COMP-3.
           05  R0A-BAND                    PIC X(04).
           05  R0A-GROUP                   PIC X(13).
           05  R0A-TARIFF                  PIC S9(09)V9(02) COMP-3.
           05  R0A-TARIFF2                 PIC X(20).
           05  R0A-REST                    PIC X(74).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-AH-VIEW2 REDEFINES CABS-AH-IN-RECORD.
           05  R1A-JURIS                   PIC X(04).
           05  R1A-TARGET                  PIC S9(15) COMP-3.
           05  R1A-CIRCUIT                 PIC 9(04).
           05  R1A-CARRIER                 PIC X(02).
           05  R1A-STATUS                  PIC 9(04).
           05  R1A-REST                    PIC X(98).
      * FEEDOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  FEEDOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-AH-OUT-RECORD.
           05  OA-LEVEL                    PIC 9(04).
           05  OA-TYPE                     PIC X(04).
           05  OA-STATE                    PIC S9(07) COMP-3.
           05  OA-INVOICE                  PIC 9(09).
           05  OA-TARGET                   PIC X(06).
           05  OA-INVOICE2                 PIC 9(09).
           05  OA-SEQ                      PIC S9(07)V9(02) COMP-3.
           05  OA-CYCLE                    PIC 9(02).
           05  OA-TARIFF                   PIC X(10).
           05  OA-SEGMENT                  PIC X(02).
           05  OA-OCN                      PIC 9(06).
           05  AH-FILL-03                  PIC X(19).
      * DROPOUT - WORK FILE, DELETED AT STEP END.
       FD  DROPOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-AH-OUT1-RECORD         PIC X(80).
      * CTLOUT - PERMANENT DATASET HELD ON DASD.
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD              PIC X(180).
      * RPTOUT - CATALOGUED GENERATION DATA GROUP.
       FD  RPTOUT
           RECORDING MODE IS FA
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       COPY CABSPRNT.
       WORKING-STORAGE SECTION.
      * STANDARD SHARED WORKING STORAGE. SEE CABSWRK.
       COPY CABSWRK.
      * SHARED LAYOUT PULLED IN FOR THE FILTER SIDE.
       COPY CABSCIRC.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUEX25'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V2.08'.
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
           05  WS-AH-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AH-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AH-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AH-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AH-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AH-CNT-06                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AH-CNT-07                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-AH-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AH-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AH-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AH-QTY-04                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-AH-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AH-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AH-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AH-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AH-AMT-05                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AH-AMT-06                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-AH-TXT-01                PIC X(26) VALUE SPACES.
           05  WS-AH-TXT-02                PIC X(12) VALUE SPACES.
           05  WS-AH-TXT-03                PIC X(10) VALUE SPACES.
           05  WS-AH-TXT-04                PIC X(26) VALUE SPACES.
           05  WS-AH-TXT-05                PIC X(10) VALUE SPACES.
           05  WS-AH-TXT-06                PIC X(20) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-AH-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-AH-ON-01                 VALUE 'Y'.
               88  WS-AH-OFF-01                VALUE 'N'.
           05  WS-AH-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-AH-ON-02                 VALUE 'Y'.
               88  WS-AH-OFF-02                VALUE 'N'.
           05  WS-AH-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-AH-ON-03                 VALUE 'Y'.
               88  WS-AH-OFF-03                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-AH-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-AH-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-AH-SUB-03                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. THE LIMIT WAS RAISED WHEN THE SOUTHEAST CENTRES
      * WERE MERGED AND HAS NOT BEEN REVIEWED SINCE.
       01  WS-AH-TABLE.
           05  WS-AH-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-AH-TB-ENTRY OCCURS 150 TIMES
                                       INDEXED BY WS-AH-IX.
               10  WS-AH-TB-KEY                PIC X(08).
               10  WS-AH-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-AH-TB-TXT                PIC X(30).
               10  WS-AH-TB-EFF                PIC 9(05).
               10  WS-AH-TB-EXP                PIC 9(05).
       01  WS-AH-WORK-GROUP-1.
           05  WS-AH-G1-STATUS             PIC X(10).
           05  WS-AH-G1-CYCLE              PIC X(10).
           05  WS-AH-G1-CARRIER            PIC S9(09) COMP-3.
           05  WS-AH-G1-REGION             PIC 9(05).
           05  WS-AH-G1-CYCLE              PIC 9(05).
           05  WS-AH-G1-CODE               PIC X(20).
           05  WS-AH-G1-CIRCUIT            PIC X(10).
       01  WS-AH-WORK-GROUP-2.
           05  WS-AH-G2-CIRCUIT            PIC S9(09) COMP-3.
           05  WS-AH-G2-CYCLE              PIC S9(09) COMP-3.
           05  WS-AH-G2-STATE              PIC X(10).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 60.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUEX25 - FACTOR STUDY EXTRACT'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-AH-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-AH-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9986.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-AH-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-AH-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'USGIN OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN INPUT MSTIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'MSTIN OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT FEEDOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'FEEDOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT DROPOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'DROPOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CTLOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RPTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RPTOUT OPEN FAILED - FILE STATUS BAD' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-AH-CYCLE-YYDDD.
           COMPUTE WS-AH-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-AH-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-AH-CNT-05.
           MOVE 0 TO WS-AH-CNT-07.
           MOVE 0 TO WS-AH-CNT-03.
       P1200-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-AH-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-AH-TAB-CNT NOT < 150
               MOVE 'Y' TO WS-AH-SW-01
               ADD 1 TO WS-AH-CNT-02
           ELSE
               ADD 1 TO WS-AH-TAB-CNT
               SET WS-AH-IX TO WS-AH-TAB-CNT
               MOVE IA-INVOICE TO WS-AH-TB-KEY (WS-AH-IX)
               MOVE 0 TO WS-AH-TB-VAL (WS-AH-IX)
               MOVE SPACES TO WS-AH-TB-TXT (WS-AH-IX)
               ADD 1 TO WS-AH-CNT-01.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ USGIN
               AT END MOVE 'Y' TO WS-AH-SW-01.
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
           PERFORM P2200-SELECT-FILTER THRU P2200-SELECT-FILTER-EXIT.
           PERFORM P2300-EXPAND-RANGE THRU P2300-EXPAND-RANGE-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ USGIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P2200-SELECT-FILTER.
           UNSTRING WS-AH-TXT-01 DELIMITED BY '/'
               INTO WS-AH-TXT-05
               WS-AH-TXT-01
               TALLYING IN WS-AH-CNT-02.
           MOVE 'N' TO WS-AH-SW-03.
           IF WS-AH-TAB-CNT > 0
               PERFORM P250-COMPARE-CANDIDATE THRU
                   P250-COMPARE-CANDIDATE-EXIT
               VARYING WS-AH-SUB-03 FROM 1 BY 1
               UNTIL WS-AH-SUB-03 > WS-AH-TAB-CNT
               OR WS-AH-SW-03 = 'Y'.
       P2200-SELECT-FILTER-EXIT.
           EXIT.
       P2300-EXPAND-RANGE.
           MOVE SPACES TO CABS-AH-OUT-RECORD.
           MOVE IA-TARIFF TO OA-LEVEL.
           MOVE IA-CARRIER TO OA-TYPE.
           MOVE IA-JURIS TO OA-STATE.
           MOVE IA-TYPE TO OA-INVOICE.
           WRITE CABS-AH-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P2300-EXPAND-RANGE-EXIT.
           EXIT.
       P250-COMPARE-CANDIDATE.
           SET WS-AH-IX TO WS-AH-SUB-02.
           IF WS-AH-TB-KEY (WS-AH-IX) = IA-BAND2
               MOVE 'Y' TO WS-AH-SW-01
               MOVE WS-AH-TB-VAL (WS-AH-IX) TO WS-AH-QTY-01
               MOVE WS-AH-SUB-02 TO WS-AH-SUB-03.
       P250-COMPARE-CANDIDATE-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P3100-STAGE-MASTER.
           MOVE SPACES TO CABS-AH-OUT-RECORD.
           MOVE IA-TARIFF TO OA-LEVEL.
           MOVE IA-BAN2 TO OA-TYPE.
           MOVE IA-TYPE TO OA-STATE.
           MOVE IA-TARIFF TO OA-INVOICE.
           MOVE IA-INVOICE TO OA-TARGET.
           MOVE IA-JURIS TO OA-INVOICE2.
           MOVE IA-INVOICE TO OA-SEQ.
           WRITE CABS-AH-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P3100-STAGE-MASTER-EXIT.
           EXIT.
       P3200-WRITE-SELECTION.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-AH-TXT-06 TO PC-COL-001-020.
           MOVE WS-AH-TXT-01 TO PC-COL-021-060.
           MOVE WS-AH-AMT-03 TO WS-AH-AMT-EDIT.
           MOVE WS-AH-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P3200-WRITE-SELECTION-EXIT.
           EXIT.
       P3300-RELEASE-EXTRACT.
           MOVE 0 TO WS-AH-QTY-02.
           MOVE 0 TO WS-AH-QTY-04.
           MOVE 0 TO WS-AH-AMT-04.
       P3300-RELEASE-EXTRACT-EXIT.
           EXIT.
      * S800-CONTROL SECTION - THE MANDATORY CABS CONTROL BOUNDARY.
       S800-CONTROL SECTION.
       P8000-CONTROL.
           PERFORM P8010-PRINT-AUDIT-REPORT THRU P8010-EXIT.
           PERFORM P8100-BUILD-CONTROL-REC THRU P8100-EXIT.
           PERFORM P8200-CHECK-BALANCE THRU P8200-EXIT.
           PERFORM P8300-WRITE-CONTROL-REC THRU P8300-EXIT.
       P8000-EXIT.
           EXIT.
       P8010-PRINT-AUDIT-REPORT.
           ADD 1 TO WS-RPT-PAGE-NBR.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '1' TO PC-CC.
           MOVE WS-RPT-TITLE1 TO PC-TEXT.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-RPT-TITLE2 TO PC-TEXT.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'READ FROM INPUT' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO WS-AH-CNT-EDIT.
           MOVE WS-AH-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'WRITTEN TO OUTPUT' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-AH-CNT-EDIT.
           MOVE WS-AH-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'REJECTED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-AH-CNT-EDIT.
           MOVE WS-AH-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'ROLLED INTO SUMMARY' TO PC-COL-001-020.
           MOVE WS-SUMM-CNT TO WS-AH-CNT-EDIT.
           MOVE WS-AH-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'CARRIED FORWARD' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-AH-CNT-EDIT.
           MOVE WS-AH-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-AH-CNT-01 TO WS-AH-CNT-EDIT.
           MOVE WS-AH-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 02' TO PC-COL-001-020.
           MOVE WS-AH-CNT-02 TO WS-AH-CNT-EDIT.
           MOVE WS-AH-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE 3 TO CT-STEP-SEQ.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE WS-AH-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE SPACES TO CT-FILLER.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-ABEND-CD.
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
           CLOSE USGIN.
           CLOSE MSTIN.
           CLOSE FEEDOUT.
           CLOSE DROPOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABUEX25 - END OF RUN'.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  AH-CNT-03 = ' WS-AH-CNT-03.
           DISPLAY '  AH-CNT-06 = ' WS-AH-CNT-06.
           DISPLAY '  AH-CNT-01 = ' WS-AH-CNT-01.
       P9000-EXIT.
           EXIT.
