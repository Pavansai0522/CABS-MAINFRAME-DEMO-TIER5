      *****************************************************************
      * CABUXR21 - ACCOUNT TO INVOICE CROSS REFERENCE                 *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               MSTIN   TELCABS.CABS.MSTIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               GRPOUT  TELCABS.CABS.GRPOUT         (LOCAL)     *
      *               PAIROUT TELCABS.CABS.PAIROU         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - NOTHING IS UPDATED IN PLACE        *
      * REVISION HISTORY                                              *
      *   V1.00  1991-06-20  B.R.HALVORSEN INITIAL RELEASE            *
      *   V1.03  1998-09-11  L.FERREIRA   HASH TOTAL ADDED TO THE     *
      *                      CONTROL RECORD                           *
      *   V1.06  2012-05-27  D.OKONKWO    TABLE LIMIT RAISED FOR THE  *
      *                      SOUTHEAST CENTRES                        *
      *   V1.07  2015-11-01  B.R.HALVORSEN CARRIER TYPE BROUGHT ONTO  *
      *                      THE EXTRACT                              *
      *   V1.08  2018-12-12  R.T.WHEELER  REGION SIZE REDUCED - TABLE *
      *                      MOVED OUT OF WORKING STORAGE             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUXR21.
       AUTHOR. TELCABS APPLICATIONS - CROSS REFERENCE REPORT.
      *****************************************************************
      * ACCOUNT TO INVOICE CROSS REFERENCE. THE STEP IS DRIVEN        *
      * ENTIRELY FROM THE SYSIN PARM CARD AND THE DD ALLOCATIONS IN   *
      * THE JOB.                                                      *
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
           SELECT MSTIN ASSIGN TO UT-S-MSTIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT GRPOUT ASSIGN TO UT-S-GRPOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT PAIROUT ASSIGN TO UT-S-PAIROUT
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
      * MSTIN - PERMANENT DATASET HELD ON DASD.
       FD  MSTIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 100 CHARACTERS.
       01  CABS-DZ-IN-RECORD.
           05  ID-CENTRE                   PIC X(13).
           05  ID-ELEM                     PIC X(13).
           05  ID-CLASS                    PIC S9(13)V9(02) COMP-3.
           05  ID-SOURCE                   PIC S9(05) COMP-3.
           05  ID-CARRIER                  PIC S9(11) COMP-3.
           05  ID-CENTRE2                  PIC S9(13) COMP-3.
           05  ID-ELEM2                    PIC X(08).
           05  ID-SEGMENT                  PIC S9(09) COMP-3.
           05  ID-REGION                   PIC X(20).
           05  ID-CIRCUIT                  PIC 9(04).
           05  ID-TARGET                   PIC 9(02).
           05  ID-MEDIA                    PIC S9(13)V9(02) COMP-3.
           05  DZ-FILL-01                  PIC X(3).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-DZ-VIEW1 REDEFINES CABS-DZ-IN-RECORD.
           05  R0D-JURIS                   PIC 9(04).
           05  R0D-CARRIER                 PIC X(10).
           05  R0D-CODE                    PIC 9(03).
           05  R0D-SOURCE                  PIC S9(09) COMP-3.
           05  R0D-JURIS2                  PIC S9(05) COMP-3.
           05  R0D-REST                    PIC X(75).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-DZ-VIEW2 REDEFINES CABS-DZ-IN-RECORD.
           05  R1D-TARIFF                  PIC S9(15) COMP-3.
           05  R1D-STATUS                  PIC S9(07)V9(02) COMP-3.
           05  R1D-JURIS                   PIC S9(11) COMP-3.
           05  R1D-GROUP                   PIC X(16).
           05  R1D-LEVEL                   PIC X(06).
           05  R1D-JURIS2                  PIC 9(05).
           05  R1D-REST                    PIC X(54).
      * GRPOUT - WORK FILE, DELETED AT STEP END.
       FD  GRPOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 120 CHARACTERS.
       01  CABS-DZ-OUT-RECORD.
           05  OD-INVOICE                  PIC X(10).
           05  OD-LEVEL                    PIC X(02).
           05  OD-SOURCE                   PIC S9(05) COMP-3.
           05  OD-BAND                     PIC X(10).
           05  OD-ELEM                     PIC S9(11)V9(02) COMP-3.
           05  OD-GROUP                    PIC X(02).
           05  OD-CARRIER                  PIC X(10).
           05  OD-OCN                      PIC X(16).
           05  OD-STATE                    PIC X(02).
           05  OD-CLASS                    PIC S9(09)V9(02) COMP-3.
           05  OD-CIRCUIT                  PIC X(04).
           05  OD-BAND2                    PIC X(10).
           05  OD-REGION                   PIC 9(09).
           05  OD-SOURCE2                  PIC S9(09)V9(02) COMP-3.
           05  OD-CARRIER2                 PIC X(16).
           05  DZ-FILL-02                  PIC X(7).
      * PAIROUT - CATALOGUED GENERATION DATA GROUP.
       FD  PAIROUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 120 CHARACTERS.
       01  CABS-DZ-OUT1-RECORD         PIC X(120).
      * CTLOUT - CATALOGUED GENERATION DATA GROUP.
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD              PIC X(180).
      * RPTOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  RPTOUT
           RECORDING MODE IS FA
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       COPY CABSPRNT.
       WORKING-STORAGE SECTION.
      * STANDARD SHARED WORKING STORAGE. SEE CABSWRK.
       COPY CABSWRK.
      * SHARED LAYOUT PULLED IN FOR THE SIDE SIDE.
       COPY CABSCIRC.
      * SHARED LAYOUT PULLED IN FOR THE ORPHAN SIDE.
       COPY CABSBHDR.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUXR21'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V1.16'.
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
       01  WS-COUNT-AREA.
           05  WS-DZ-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DZ-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DZ-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DZ-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DZ-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DZ-CNT-06                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DZ-CNT-07                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DZ-CNT-08                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-DZ-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DZ-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DZ-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DZ-QTY-04                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DZ-QTY-05                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-DZ-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DZ-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DZ-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DZ-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DZ-AMT-05                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-DZ-TXT-01                PIC X(08) VALUE SPACES.
           05  WS-DZ-TXT-02                PIC X(26) VALUE SPACES.
           05  WS-DZ-TXT-03                PIC X(26) VALUE SPACES.
           05  WS-DZ-TXT-04                PIC X(26) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-DZ-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-DZ-ON-01                 VALUE 'Y'.
               88  WS-DZ-OFF-01                VALUE 'N'.
           05  WS-DZ-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-DZ-ON-02                 VALUE 'Y'.
               88  WS-DZ-OFF-02                VALUE 'N'.
           05  WS-DZ-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-DZ-ON-03                 VALUE 'Y'.
               88  WS-DZ-OFF-03                VALUE 'N'.
           05  WS-DZ-SW-04                 PIC X(01) VALUE 'N'.
               88  WS-DZ-ON-04                 VALUE 'Y'.
               88  WS-DZ-OFF-04                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-DZ-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DZ-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DZ-SUB-03                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. THE LIMIT WAS RAISED WHEN THE SOUTHEAST CENTRES
      * WERE MERGED AND HAS NOT BEEN REVIEWED SINCE.
       01  WS-DZ-TABLE.
           05  WS-DZ-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-DZ-TB-ENTRY OCCURS 250 TIMES
                                       INDEXED BY WS-DZ-IX.
               10  WS-DZ-TB-KEY                PIC X(10).
               10  WS-DZ-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-DZ-TB-TXT                PIC X(20).
               10  WS-DZ-TB-EFF                PIC 9(05).
               10  WS-DZ-TB-EXP                PIC 9(05).
       01  WS-DZ-WORK-GROUP-1.
           05  WS-DZ-G1-SEQ                PIC 9(07).
           05  WS-DZ-G1-CLASS              PIC X(10).
           05  WS-DZ-G1-REGION             PIC X(20).
           05  WS-DZ-G1-JURIS              PIC X(20).
           05  WS-DZ-G1-GROUP              PIC X(20).
           05  WS-DZ-G1-TARIFF             PIC X(10).
           05  WS-DZ-G1-JURIS              PIC X(10).
       01  WS-DZ-WORK-GROUP-2.
           05  WS-DZ-G2-JURIS              PIC X(20).
           05  WS-DZ-G2-TARGET             PIC X(20).
           05  WS-DZ-G2-ACCOUNT            PIC S9(11)V9(02) COMP-3.
           05  WS-DZ-G2-BAND               PIC X(10).
           05  WS-DZ-G2-OCN                PIC 9(07).
           05  WS-DZ-G2-SOURCE             PIC 9(07).
           05  WS-DZ-G2-ELEM               PIC X(20).
       01  WS-DZ-WORK-GROUP-3.
           05  WS-DZ-G3-CYCLE              PIC 9(05).
           05  WS-DZ-G3-INVOICE            PIC 9(05).
           05  WS-DZ-G3-GROUP              PIC S9(11)V9(02) COMP-3.
           05  WS-DZ-G3-LEVEL              PIC S9(09) COMP-3.
           05  WS-DZ-G3-CENTRE             PIC S9(09) COMP-3.
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 50.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUXR21 - ACCOUNT TO INVOICE CROSS REFERENCE'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-DZ-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-DZ-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9978.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-DZ-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-DZ-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT MSTIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'MSTIN OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               DISPLAY 'MSTIN FILE STATUS = ' WS-FS-INPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT GRPOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'GRPOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               DISPLAY 'GRPOUT FILE STATUS = ' WS-FS-OUTPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT PAIROUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'PAIROUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               DISPLAY 'PAIROUT FILE STATUS = ' WS-FS-OUTPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CTLOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               DISPLAY 'CTLOUT FILE STATUS = ' WS-FS-CONTROL
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RPTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RPTOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               DISPLAY 'RPTOUT FILE STATUS = ' WS-FS-OUTPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
       P1100-EXIT.
           EXIT.
      * P1200-READ-PARM - THE CYCLE DATE ARRIVES AS TWO DIGITS AND IS
      * PIVOTED ON DW-PIVOT-YY BEFORE ANY DATE MATH.
       P1200-READ-PARM.
           MOVE SPACES TO WS-PARM-CARD.
           ACCEPT WS-PARM-CARD FROM SYSIN.
           CALL 'CABPARMR' USING WS-PARM-CARD WS-RC-PARMR.
           MOVE PC1-CYCLE-YYDDD TO WS-DZ-CYCLE-YYDDD.
           COMPUTE WS-DZ-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-DZ-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-DZ-CNT-02.
           MOVE 0 TO WS-DZ-CNT-03.
           MOVE 0 TO WS-DZ-CNT-08.
       P1200-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-DZ-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-DZ-TAB-CNT NOT < 250
               MOVE 'Y' TO WS-DZ-SW-01
               ADD 1 TO WS-DZ-CNT-08
           ELSE
               ADD 1 TO WS-DZ-TAB-CNT
               SET WS-DZ-IX TO WS-DZ-TAB-CNT
               MOVE ID-CIRCUIT TO WS-DZ-TB-KEY (WS-DZ-IX)
               MOVE 0 TO WS-DZ-TB-VAL (WS-DZ-IX)
               MOVE SPACES TO WS-DZ-TB-TXT (WS-DZ-IX)
               ADD 1 TO WS-DZ-CNT-08.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ MSTIN
               AT END MOVE 'Y' TO WS-DZ-SW-01.
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
           IF WS-DZ-ON-03
               PERFORM P2200-CHECK-GROUP THRU P2200-CHECK-GROUP-EXIT.
           IF WS-DZ-ON-04
               PERFORM P2300-VALIDATE-ORPHAN THRU
                   P2300-VALIDATE-ORPHAN-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ MSTIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-CHECK-GROUP.
           CALL 'CABEDITF' USING WS-DZ-TXT-04 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-DZ-CNT-06.
           MOVE 'N' TO WS-DZ-SW-01.
           IF WS-DZ-TAB-CNT > 0
               PERFORM P260-COMPARE-GROUP THRU P260-COMPARE-GROUP-EXIT
               VARYING WS-DZ-SUB-02 FROM 1 BY 1
               UNTIL WS-DZ-SUB-02 > WS-DZ-TAB-CNT
               OR WS-DZ-SW-01 = 'Y'.
       P2200-CHECK-GROUP-EXIT.
           EXIT.
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT
      * PRECEDES THIS PROGRAM IN THE JOB.
       P2300-VALIDATE-ORPHAN.
           MOVE ID-MEDIA TO WS-DZ-TXT-04.
           MOVE ID-TARGET TO WS-DZ-TXT-03.
           ADD 1 TO WS-DZ-CNT-03.
       P2300-VALIDATE-ORPHAN-EXIT.
           EXIT.
       P260-COMPARE-GROUP.
           SET WS-DZ-IX TO WS-DZ-SUB-02.
           IF WS-DZ-TB-KEY (WS-DZ-IX) = ID-CIRCUIT
               MOVE 'Y' TO WS-DZ-SW-03
               MOVE WS-DZ-TB-VAL (WS-DZ-IX) TO WS-DZ-QTY-02
               MOVE WS-DZ-SUB-02 TO WS-DZ-SUB-03.
       P260-COMPARE-GROUP-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P3100-RELEASE-REFERENCE.
           MOVE SPACES TO CABS-DZ-OUT-RECORD.
           MOVE ID-CARRIER TO OD-INVOICE.
           MOVE ID-CLASS TO OD-LEVEL.
           MOVE ID-ELEM TO OD-SOURCE.
           MOVE ID-CENTRE2 TO OD-BAND.
           MOVE ID-CLASS TO OD-ELEM.
           MOVE ID-CIRCUIT TO OD-GROUP.
           MOVE ID-CIRCUIT TO OD-CARRIER.
           WRITE CABS-DZ-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P3100-RELEASE-REFERENCE-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P3200-STAGE-GROUP.
           ADD ID-SEGMENT TO WS-DZ-QTY-03.
           COMPUTE WS-DZ-AMT-02 = WS-DZ-QTY-03 * WS-DZ-QTY-04.
           ADD WS-DZ-AMT-02 TO WS-DZ-AMT-01.
       P3200-STAGE-GROUP-EXIT.
           EXIT.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P3300-STAGE-PAIR.
           MOVE ID-SEGMENT TO WS-DZ-TXT-03.
           MOVE ID-CARRIER TO WS-DZ-TXT-03.
           ADD 1 TO WS-DZ-CNT-02.
       P3300-STAGE-PAIR-EXIT.
           EXIT.
       P3400-RELEASE-REFERENCE.
           MOVE SPACES TO WS-DZ-TXT-02.
           STRING ID-CENTRE DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               ID-CENTRE DELIMITED BY SIZE
               INTO WS-DZ-TXT-02.
       P3400-RELEASE-REFERENCE-EXIT.
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
           MOVE 'RECORDS SUMMARISED' TO PC-COL-001-020.
           MOVE WS-SUMM-CNT TO WS-DZ-CNT-EDIT.
           MOVE WS-DZ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS WRITTEN' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-DZ-CNT-EDIT.
           MOVE WS-DZ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS READ' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO WS-DZ-CNT-EDIT.
           MOVE WS-DZ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS REJECTED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-DZ-CNT-EDIT.
           MOVE WS-DZ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS CARRIED FWD' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-DZ-CNT-EDIT.
           MOVE WS-DZ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-DZ-CNT-01 TO WS-DZ-CNT-EDIT.
           MOVE WS-DZ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 02' TO PC-COL-001-020.
           MOVE WS-DZ-CNT-02 TO WS-DZ-CNT-EDIT.
           MOVE WS-DZ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 03' TO PC-COL-001-020.
           MOVE WS-DZ-CNT-03 TO WS-DZ-CNT-EDIT.
           MOVE WS-DZ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE SPACES TO CT-FILLER.
           MOVE WS-DZ-CNT-08 TO CT-RC.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE WS-DZ-TXT-03 TO CT-RESTART-KEY.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE WS-DZ-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE 7 TO CT-STEP-SEQ.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
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
           CLOSE MSTIN.
           CLOSE GRPOUT.
           CLOSE PAIROUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABUXR21 - END OF RUN'.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  DZ-CNT-05 = ' WS-DZ-CNT-05.
           DISPLAY '  DZ-CNT-06 = ' WS-DZ-CNT-06.
       P9000-EXIT.
           EXIT.
