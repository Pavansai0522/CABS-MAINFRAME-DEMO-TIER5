      *****************************************************************
      * CABUXR09 - CIRCUIT TO ACCOUNT CROSS REFERENCE                 *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               REFIN   TELCABS.CABS.REFIN          (LOCAL)     *
      *               MSTIN   TELCABS.CABS.MSTIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               MTCOUT  TELCABS.CABS.MTCOUT         (LOCAL)     *
      *               GRPOUT  TELCABS.CABS.GRPOUT         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - THE OUTPUT IS REBUILT EACH RUN     *
      * REVISION HISTORY                                              *
      *   V1.00  2000-09-03  G.PRZYBYLSKI INITIAL RELEASE             *
      *   V1.01  2001-05-02  P.NAIR       TABLE LIMIT RAISED FOR THE  *
      *                      SOUTHEAST CENTRES                        *
      *   V1.05  2002-01-26  W.J.MCALLISTER TABLE LIMIT RAISED FOR THE*
      *                      SOUTHEAST CENTRES                        *
      *   V1.07  2006-11-06  K.O.BRIEN    RETIRED THE SECOND SORT STEP*
      *                      - DONE IN PROGRAM                        *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUXR09.
       AUTHOR. TELCABS APPLICATIONS - CROSS REFERENCE REPORT.
      *****************************************************************
      * CIRCUIT TO ACCOUNT CROSS REFERENCE. THE STEP RUNS ONCE PER    *
      * BILL CYCLE AND IS RERUN FROM THE TOP IF IT FAILS.             *
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
           SELECT REFIN ASSIGN TO UT-S-REFIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT MSTIN ASSIGN TO UT-S-MSTIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT MTCOUT ASSIGN TO UT-S-MTCOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT GRPOUT ASSIGN TO UT-S-GRPOUT
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
           SELECT RPTOUT ASSIGN TO UT-S-RPTOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
       DATA DIVISION.
       FILE SECTION.
      * REFIN - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  REFIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 100 CHARACTERS.
       01  CABS-BD-IN-RECORD.
           05  IB-SEQ                      PIC X(20).
           05  IB-CENTRE                   PIC S9(13) COMP-3.
           05  IB-CIRCUIT                  PIC X(20).
           05  IB-MEDIA                    PIC S9(13) COMP-3.
           05  IB-SEQ2                     PIC 9(07).
           05  IB-BAND                     PIC X(02).
           05  IB-CENTRE2                  PIC S9(07) COMP-3.
           05  IB-INVOICE                  PIC S9(11)V9(02) COMP-3.
           05  IB-CIRCUIT2                 PIC S9(13) COMP-3.
           05  IB-JURIS                    PIC X(02).
           05  IB-TARGET                   PIC X(08).
           05  IB-CLASS                    PIC 9(07).
           05  BD-FILL-01                  PIC X(2).
      * MSTIN - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  MSTIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 100 CHARACTERS.
       01  CABS-BD-ALT1-RECORD.
           05  A1-PERIOD                   PIC X(06).
           05  A1-SEQ                      PIC S9(15) COMP-3.
           05  A1-CYCLE                    PIC X(06).
           05  A1-JURIS                    PIC 9(07).
           05  A1-ACCOUNT                  PIC X(13).
           05  A1-JURIS2                   PIC X(16).
           05  A1-CARRIER                  PIC X(13).
           05  A1-TYPE                     PIC S9(13) COMP-3.
           05  A1-CYCLE2                   PIC X(04).
           05  A1-CARRIER2                 PIC X(10).
           05  BD-FILL-02                  PIC X(10).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-BD-VIEW1 REDEFINES CABS-BD-IN-RECORD.
           05  R0B-SOURCE                  PIC S9(09)V9(02) COMP-3.
           05  R0B-JURIS                   PIC X(02).
           05  R0B-SOURCE2                 PIC S9(15) COMP-3.
           05  R0B-GROUP                   PIC X(20).
           05  R0B-CIRCUIT                 PIC S9(05) COMP-3.
           05  R0B-REST                    PIC X(61).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-BD-VIEW2 REDEFINES CABS-BD-IN-RECORD.
           05  R1B-SEQ                     PIC 9(04).
           05  R1B-GROUP                   PIC S9(13) COMP-3.
           05  R1B-BAN                     PIC S9(07) COMP-3.
           05  R1B-CARRIER                 PIC X(20).
           05  R1B-STATE                   PIC S9(13)V9(02) COMP-3.
           05  R1B-CLASS                   PIC S9(13)V9(02) COMP-3.
           05  R1B-STATUS                  PIC S9(05) COMP-3.
           05  R1B-ACCOUNT                 PIC X(20).
           05  R1B-REST                    PIC X(26).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-BD-VIEW3 REDEFINES CABS-BD-IN-RECORD.
           05  R2B-TARGET                  PIC X(03).
           05  R2B-REGION                  PIC X(16).
           05  R2B-PERIOD                  PIC S9(15) COMP-3.
           05  R2B-ACCOUNT                 PIC X(06).
           05  R2B-SEQ                     PIC S9(05) COMP-3.
           05  R2B-STATE                   PIC X(10).
           05  R2B-REST                    PIC X(54).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-BD-VIEW4 REDEFINES CABS-BD-IN-RECORD.
           05  R3B-JURIS                   PIC S9(09)V9(02) COMP-3.
           05  R3B-CYCLE                   PIC S9(09) COMP-3.
           05  R3B-CENTRE                  PIC S9(13) COMP-3.
           05  R3B-BAND                    PIC X(03).
           05  R3B-REST                    PIC X(79).
      * MTCOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  MTCOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 140 CHARACTERS.
       01  CABS-BD-OUT-RECORD.
           05  OB-STATE                    PIC X(10).
           05  OB-OCN                      PIC S9(13) COMP-3.
           05  OB-CLASS                    PIC X(02).
           05  OB-TYPE                     PIC 9(02).
           05  OB-CYCLE                    PIC 9(02).
           05  OB-STATE2                   PIC X(16).
           05  OB-CIRCUIT                  PIC S9(07)V9(02) COMP-3.
           05  OB-OCN2                     PIC S9(07)V9(02) COMP-3.
           05  OB-CLASS2                   PIC X(10).
           05  OB-JURIS                    PIC X(20).
           05  OB-TARIFF                   PIC S9(13)V9(05) COMP-3.
           05  OB-INVOICE                  PIC X(16).
           05  OB-TARGET                   PIC S9(05) COMP-3.
           05  OB-SEQ                      PIC S9(11)V9(05) COMP-3.
           05  OB-LEVEL                    PIC X(16).
           05  BD-FILL-03                  PIC X(7).
      * GRPOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  GRPOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 140 CHARACTERS.
       01  CABS-BD-OUT1-RECORD         PIC X(140).
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
      * RPTOUT - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  RPTOUT
           RECORDING MODE IS FA
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       COPY CABSPRNT.
       WORKING-STORAGE SECTION.
      * STANDARD SHARED WORKING STORAGE. SEE CABSWRK.
       COPY CABSWRK.
      * SHARED LAYOUT PULLED IN FOR THE LINK SIDE.
       COPY CABSCIRC.
      * SHARED LAYOUT PULLED IN FOR THE ORPHAN SIDE.
       COPY CABSCARR.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUXR09'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V2.01'.
           05  WS-TABLE-LIMIT              PIC S9(04) COMP-3
                                         VALUE 250.
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
           05  WS-BD-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BD-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BD-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BD-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BD-CNT-05                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-BD-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BD-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BD-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BD-QTY-04                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BD-QTY-05                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BD-QTY-06                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-BD-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BD-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BD-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-BD-TXT-01                PIC X(12) VALUE SPACES.
           05  WS-BD-TXT-02                PIC X(10) VALUE SPACES.
           05  WS-BD-TXT-03                PIC X(10) VALUE SPACES.
           05  WS-BD-TXT-04                PIC X(16) VALUE SPACES.
           05  WS-BD-TXT-05                PIC X(10) VALUE SPACES.
           05  WS-BD-TXT-06                PIC X(12) VALUE SPACES.
           05  WS-BD-TXT-07                PIC X(12) VALUE SPACES.
           05  WS-BD-TXT-08                PIC X(08) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-BD-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-BD-ON-01                 VALUE 'Y'.
               88  WS-BD-OFF-01                VALUE 'N'.
           05  WS-BD-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-BD-ON-02                 VALUE 'Y'.
               88  WS-BD-OFF-02                VALUE 'N'.
           05  WS-BD-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-BD-ON-03                 VALUE 'Y'.
               88  WS-BD-OFF-03                VALUE 'N'.
           05  WS-BD-SW-04                 PIC X(01) VALUE 'N'.
               88  WS-BD-ON-04                 VALUE 'Y'.
               88  WS-BD-OFF-04                VALUE 'N'.
           05  WS-BD-SW-05                 PIC X(01) VALUE 'N'.
               88  WS-BD-ON-05                 VALUE 'Y'.
               88  WS-BD-OFF-05                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-BD-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-BD-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-BD-SUB-03                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. A ROW THAT DOES NOT FIT IS COUNTED AND DROPPED
      * - THE STEP DOES NOT FAIL.
       01  WS-BD-TABLE.
           05  WS-BD-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-BD-TB-ENTRY OCCURS 250 TIMES
                                       INDEXED BY WS-BD-IX.
               10  WS-BD-TB-KEY                PIC X(10).
               10  WS-BD-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-BD-TB-TXT                PIC X(30).
               10  WS-BD-TB-EFF                PIC 9(05).
               10  WS-BD-TB-EXP                PIC 9(05).
       01  WS-BD-WORK-GROUP-1.
           05  WS-BD-G1-CARRIER            PIC S9(09) COMP-3.
           05  WS-BD-G1-TYPE               PIC 9(07).
           05  WS-BD-G1-CODE               PIC X(10).
           05  WS-BD-G1-SOURCE             PIC S9(09) COMP-3.
           05  WS-BD-G1-REGION             PIC X(20).
       01  WS-BD-WORK-GROUP-2.
           05  WS-BD-G2-TARGET             PIC X(10).
           05  WS-BD-G2-ACCOUNT            PIC S9(11)V9(02) COMP-3.
           05  WS-BD-G2-CODE               PIC S9(09) COMP-3.
           05  WS-BD-G2-CODE               PIC 9(07).
           05  WS-BD-G2-TARIFF             PIC S9(09) COMP-3.
           05  WS-BD-G2-SEGMENT            PIC X(10).
       01  WS-BD-WORK-GROUP-3.
           05  WS-BD-G3-CENTRE             PIC S9(09) COMP-3.
           05  WS-BD-G3-CLASS              PIC X(10).
           05  WS-BD-G3-ELEM               PIC S9(09) COMP-3.
           05  WS-BD-G3-OCN                PIC 9(07).
           05  WS-BD-G3-CODE               PIC S9(09) COMP-3.
           05  WS-BD-G3-TARIFF             PIC S9(11)V9(02) COMP-3.
           05  WS-BD-G3-SOURCE             PIC X(10).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 60.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUXR09 - CIRCUIT TO ACCOUNT CROSS REFERENCE'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-BD-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-BD-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9939.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-BD-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-BD-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT REFIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'REFIN OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN INPUT MSTIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'MSTIN OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT MTCOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'MTCOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT GRPOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'GRPOUT OPEN FAILED - FILE STATUS BAD' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-BD-CYCLE-YYDDD.
           COMPUTE WS-BD-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-BD-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-BD-CNT-01.
           MOVE 0 TO WS-BD-CNT-04.
           MOVE 0 TO WS-BD-CNT-03.
       P1200-EXIT.
           EXIT.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-BD-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-BD-TAB-CNT NOT < 250
               MOVE 'Y' TO WS-BD-SW-01
               ADD 1 TO WS-BD-CNT-04
           ELSE
               ADD 1 TO WS-BD-TAB-CNT
               SET WS-BD-IX TO WS-BD-TAB-CNT
               MOVE IB-JURIS TO WS-BD-TB-KEY (WS-BD-IX)
               MOVE 0 TO WS-BD-TB-VAL (WS-BD-IX)
               MOVE SPACES TO WS-BD-TB-TXT (WS-BD-IX)
               ADD 1 TO WS-BD-CNT-04.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ REFIN
               AT END MOVE 'Y' TO WS-BD-SW-01.
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
           PERFORM P2200-DERIVE-LINK THRU P2200-DERIVE-LINK-EXIT.
           PERFORM P2300-CONVERT-MATCH THRU P2300-CONVERT-MATCH-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ REFIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-DERIVE-LINK.
           CALL 'CABHASH' USING IB-CENTRE2 WS-ACC-OCN-HASH.
           ADD WS-BD-CNT-04 TO WS-ACC-SEQ-HASH.
           MOVE 'N' TO WS-BD-SW-01.
           IF WS-BD-TAB-CNT > 0
               PERFORM P270-COMPARE-PAIR THRU P270-COMPARE-PAIR-EXIT
               VARYING WS-BD-SUB-03 FROM 1 BY 1
               UNTIL WS-BD-SUB-03 > WS-BD-TAB-CNT
               OR WS-BD-SW-01 = 'Y'.
       P2200-DERIVE-LINK-EXIT.
           EXIT.
       P2300-CONVERT-MATCH.
           IF WS-BD-AMT-02 < 43
               MOVE 43 TO WS-BD-AMT-02
               ADD 1 TO WS-BD-CNT-05.
           IF WS-BD-AMT-02 > 6666
               MOVE 6666 TO WS-BD-AMT-02
               ADD 1 TO WS-BD-CNT-04.
       P2300-CONVERT-MATCH-EXIT.
           EXIT.
       P270-COMPARE-PAIR.
           SET WS-BD-IX TO WS-BD-SUB-02.
           IF WS-BD-TB-KEY (WS-BD-IX) = IB-SEQ2
               MOVE 'Y' TO WS-BD-SW-01
               MOVE WS-BD-TB-VAL (WS-BD-IX) TO WS-BD-QTY-02
               MOVE WS-BD-SUB-02 TO WS-BD-SUB-01.
       P270-COMPARE-PAIR-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-EMIT-GROUP.
           MOVE IB-CLASS TO WS-BD-TXT-05.
           MOVE IB-TARGET TO WS-BD-TXT-08.
           MOVE IB-MEDIA TO WS-BD-TXT-01.
           MOVE IB-CIRCUIT TO WS-BD-TXT-03.
           ADD 1 TO WS-BD-CNT-03.
       P3100-EMIT-GROUP-EXIT.
           EXIT.
       P3200-CLOSE-OFF-LINK.
           MOVE SPACES TO WS-BD-TXT-05.
           STRING IB-CIRCUIT2 DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IB-MEDIA DELIMITED BY SIZE
               INTO WS-BD-TXT-05.
       P3200-CLOSE-OFF-LINK-EXIT.
           EXIT.
       P3300-FORMAT-GROUP.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-DATE-INVALID TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-BD-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
           MOVE CABS-SUSPENSE-RECORD TO CABS-SUSOUT-RECORD.
           WRITE CABS-SUSOUT-RECORD.
       P3300-FORMAT-GROUP-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P3400-RELEASE-PAIR.
           ADD IB-CENTRE2 TO WS-BD-QTY-05.
           COMPUTE WS-BD-AMT-03 ROUNDED = WS-BD-QTY-05 * WS-BD-QTY-04.
           ADD WS-BD-AMT-03 TO WS-BD-AMT-03.
       P3400-RELEASE-PAIR-EXIT.
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
           MOVE 'DETAIL IN' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO WS-BD-CNT-EDIT.
           MOVE WS-BD-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL OUT' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-BD-CNT-EDIT.
           MOVE WS-BD-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL SUSPENDED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-BD-CNT-EDIT.
           MOVE WS-BD-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL SUMMARISED' TO PC-COL-001-020.
           MOVE WS-SUMM-CNT TO WS-BD-CNT-EDIT.
           MOVE WS-BD-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL CARRIED FWD' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-BD-CNT-EDIT.
           MOVE WS-BD-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-BD-CNT-01 TO WS-BD-CNT-EDIT.
           MOVE WS-BD-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-BD-TXT-02 TO CT-RESTART-KEY.
           MOVE WS-BD-CNT-03 TO CT-RC.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE SPACES TO CT-FILLER.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE WS-BD-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE 9 TO CT-STEP-SEQ.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE 0 TO CT-RERUN-NBR.
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
           CLOSE REFIN.
           CLOSE MSTIN.
           CLOSE MTCOUT.
           CLOSE GRPOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABUXR09 - RUN COMPLETE'.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  BD-CNT-05 = ' WS-BD-CNT-05.
           DISPLAY '  BD-CNT-03 = ' WS-BD-CNT-03.
           DISPLAY '  BD-CNT-01 = ' WS-BD-CNT-01.
           DISPLAY '  BD-CNT-02 = ' WS-BD-CNT-02.
       P9000-EXIT.
           EXIT.
