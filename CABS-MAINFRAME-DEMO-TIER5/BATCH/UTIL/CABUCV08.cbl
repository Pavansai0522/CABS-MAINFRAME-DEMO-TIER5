      *****************************************************************
      * CABUCV08 - LEGACY LAYOUT DOWN CONVERSION                      *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               IXCIN   TELCABS.CABS.IXCIN          (LOCAL)     *
      *               SRCIN   TELCABS.CABS.SRCIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               REJOUT  TELCABS.CABS.REJOUT         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * REVISION HISTORY                                              *
      *   V1.00  1992-08-03  L.FERREIRA   INITIAL RELEASE             *
      *   V1.01  1997-08-25  R.T.WHEELER  REPORT PAGINATION CORRECTED *
      *   V1.03  2000-10-25  S.MARCHETTI  SECOND OUTPUT FILE ADDED FOR*
      *                      THE FACTOR STUDY                         *
      *   V1.07  2013-04-17  M.DELACROIX  REPORT PAGINATION CORRECTED *
      *   V1.08  2017-03-09  K.O.BRIEN    RECOMPILE ONLY - COPYBOOK   *
      *                      CHANGE UPSTREAM                          *
      *   V1.12  2019-06-08  W.J.MCALLISTER PARM CARD EXTENDED,       *
      *                      POSITIONS 40 THROUGH 48                  *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUCV08.
       AUTHOR. TELCABS APPLICATIONS - FILE CONVERSION.
      *****************************************************************
      * LEGACY LAYOUT DOWN CONVERSION. THE STEP IS SCHEDULED MONTHLY  *
      * AND ALSO RUN ON DEMAND WHEN A CENTRE ASKS FOR IT.             *
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE   *
      * MORE AT END OF FILE.                                          *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           C01 IS TO-NEW-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IXCIN ASSIGN TO UT-S-IXCIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT SRCIN ASSIGN TO UT-S-SRCIN
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
           SELECT RPTOUT ASSIGN TO UT-S-RPTOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
       DATA DIVISION.
       FILE SECTION.
      * IXCIN - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  IXCIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 110 CHARACTERS.
       01  CABS-CP-IN-RECORD.
           05  IC-ACCOUNT                  PIC S9(11) COMP-3.
           05  IC-OCN                      PIC S9(05) COMP-3.
           05  IC-STATE                    PIC S9(05) COMP-3.
           05  IC-BAN                      PIC S9(07)V9(02) COMP-3.
           05  IC-CARRIER                  PIC S9(05) COMP-3.
           05  IC-CLASS                    PIC S9(09)V9(02) COMP-3.
           05  IC-TYPE                     PIC S9(09)V9(05) COMP-3.
           05  IC-MEDIA                    PIC S9(13) COMP-3.
           05  IC-CODE                     PIC S9(15) COMP-3.
           05  IC-OCN2                     PIC 9(04).
           05  IC-CYCLE                    PIC X(04).
           05  IC-LEVEL                    PIC 9(09).
           05  IC-BAND                     PIC X(06).
           05  IC-BAND2                    PIC X(13).
           05  IC-CLASS2                   PIC S9(15) COMP-3.
           05  IC-GROUP                    PIC 9(07).
           05  CP-FILL-01                  PIC X(10).
      * SRCIN - CATALOGUED GENERATION DATA GROUP.
       FD  SRCIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 110 CHARACTERS.
       01  CABS-CP-ALT1-RECORD.
           05  A1-CYCLE                    PIC X(20).
           05  A1-BAND                     PIC S9(13)V9(02) COMP-3.
           05  A1-TARGET                   PIC X(02).
           05  A1-CIRCUIT                  PIC S9(07)V9(05) COMP-3.
           05  A1-JURIS                    PIC S9(05) COMP-3.
           05  A1-PERIOD                   PIC 9(09).
           05  A1-GROUP                    PIC 9(02).
           05  CP-FILL-02                  PIC X(59).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-CP-VIEW1 REDEFINES CABS-CP-IN-RECORD.
           05  R0C-CLASS                   PIC S9(07) COMP-3.
           05  R0C-ELEM                    PIC X(02).
           05  R0C-ACCOUNT                 PIC S9(11)V9(05) COMP-3.
           05  R0C-GROUP                   PIC S9(07)V9(05) COMP-3.
           05  R0C-REGION                  PIC X(20).
           05  R0C-LEVEL                   PIC X(02).
           05  R0C-REST                    PIC X(66).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-CP-VIEW2 REDEFINES CABS-CP-IN-RECORD.
           05  R1C-STATUS                  PIC S9(13) COMP-3.
           05  R1C-TYPE                    PIC X(20).
           05  R1C-GROUP                   PIC 9(02).
           05  R1C-TARIFF                  PIC S9(07)V9(02) COMP-3.
           05  R1C-CIRCUIT                 PIC X(04).
           05  R1C-PERIOD                  PIC 9(02).
           05  R1C-CARRIER                 PIC X(06).
           05  R1C-MEDIA                   PIC S9(07) COMP-3.
           05  R1C-ACCOUNT                 PIC S9(13)V9(02) COMP-3.
           05  R1C-REST                    PIC X(52).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-CP-VIEW3 REDEFINES CABS-CP-IN-RECORD.
           05  R2C-CLASS                   PIC S9(07) COMP-3.
           05  R2C-REGION                  PIC X(06).
           05  R2C-JURIS                   PIC 9(02).
           05  R2C-GROUP                   PIC X(10).
           05  R2C-REGION2                 PIC X(04).
           05  R2C-GROUP2                  PIC S9(07)V9(02) COMP-3.
           05  R2C-INVOICE                 PIC X(06).
           05  R2C-PERIOD                  PIC X(08).
           05  R2C-TYPE                    PIC S9(11)V9(05) COMP-3.
           05  R2C-REST                    PIC X(56).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-CP-VIEW4 REDEFINES CABS-CP-IN-RECORD.
           05  R3C-REGION                  PIC S9(11) COMP-3.
           05  R3C-STATE                   PIC X(04).
           05  R3C-CYCLE                   PIC S9(07)V9(02) COMP-3.
           05  R3C-SEGMENT                 PIC X(08).
           05  R3C-REST                    PIC X(87).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-CP-VIEW5 REDEFINES CABS-CP-IN-RECORD.
           05  R4C-CYCLE                   PIC X(08).
           05  R4C-INVOICE                 PIC X(06).
           05  R4C-PERIOD                  PIC S9(09)V9(02) COMP-3.
           05  R4C-CIRCUIT                 PIC X(16).
           05  R4C-CODE                    PIC X(13).
           05  R4C-REST                    PIC X(61).
      * REJOUT - WORK FILE, DELETED AT STEP END.
       FD  REJOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 130 CHARACTERS.
       01  CABS-CP-OUT-RECORD.
           05  OC-CENTRE                   PIC 9(09).
           05  OC-INVOICE                  PIC X(13).
           05  OC-ACCOUNT                  PIC X(04).
           05  OC-REGION                   PIC X(16).
           05  OC-STATE                    PIC S9(07)V9(02) COMP-3.
           05  OC-INVOICE2                 PIC X(03).
           05  OC-CENTRE2                  PIC S9(13)V9(05) COMP-3.
           05  OC-MEDIA                    PIC X(04).
           05  OC-SOURCE                   PIC S9(07) COMP-3.
           05  OC-REGION2                  PIC 9(04).
           05  OC-TYPE                     PIC X(04).
           05  OC-LEVEL                    PIC 9(05).
           05  OC-PERIOD                   PIC X(04).
           05  OC-INVOICE3                 PIC X(20).
           05  OC-JURIS                    PIC S9(05) COMP-3.
           05  OC-OCN                      PIC X(16).
           05  CP-FILL-03                  PIC X(6).
      * SUSOUT - PERMANENT DATASET HELD ON DASD.
       FD  SUSOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 300 CHARACTERS.
       01  CABS-SUSOUT-RECORD              PIC X(300).
      * CTLOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
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
      * SHARED LAYOUT PULLED IN FOR THE ZONE SIDE.
       COPY CABSSETL.
      * SHARED LAYOUT PULLED IN FOR THE FIELD SIDE.
       COPY CABSCOMM.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUCV08'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V1.23'.
           05  WS-TABLE-LIMIT              PIC S9(04) COMP-3
                                         VALUE 300.
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
           05  WS-CP-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CP-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CP-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CP-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CP-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CP-CNT-06                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CP-CNT-07                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-CP-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CP-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CP-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-CP-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CP-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CP-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CP-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CP-AMT-05                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-CP-TXT-01                PIC X(16) VALUE SPACES.
           05  WS-CP-TXT-02                PIC X(16) VALUE SPACES.
           05  WS-CP-TXT-03                PIC X(30) VALUE SPACES.
           05  WS-CP-TXT-04                PIC X(10) VALUE SPACES.
           05  WS-CP-TXT-05                PIC X(10) VALUE SPACES.
           05  WS-CP-TXT-06                PIC X(16) VALUE SPACES.
           05  WS-CP-TXT-07                PIC X(20) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-CP-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-CP-ON-01                 VALUE 'Y'.
               88  WS-CP-OFF-01                VALUE 'N'.
           05  WS-CP-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-CP-ON-02                 VALUE 'Y'.
               88  WS-CP-OFF-02                VALUE 'N'.
           05  WS-CP-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-CP-ON-03                 VALUE 'Y'.
               88  WS-CP-OFF-03                VALUE 'N'.
           05  WS-CP-SW-04                 PIC X(01) VALUE 'N'.
               88  WS-CP-ON-04                 VALUE 'Y'.
               88  WS-CP-OFF-04                VALUE 'N'.
           05  WS-CP-SW-05                 PIC X(01) VALUE 'N'.
               88  WS-CP-ON-05                 VALUE 'Y'.
               88  WS-CP-OFF-05                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-CP-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CP-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CP-SUB-03                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CP-SUB-04                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. THE LIMIT WAS RAISED WHEN THE SOUTHEAST CENTRES
      * WERE MERGED AND HAS NOT BEEN REVIEWED SINCE.
       01  WS-CP-TABLE.
           05  WS-CP-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-CP-TB-ENTRY OCCURS 300 TIMES
                                       INDEXED BY WS-CP-IX.
               10  WS-CP-TB-KEY                PIC X(10).
               10  WS-CP-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-CP-TB-TXT                PIC X(20).
               10  WS-CP-TB-EFF                PIC 9(05).
               10  WS-CP-TB-EXP                PIC 9(05).
       01  WS-CP-WORK-GROUP-1.
           05  WS-CP-G1-ELEM               PIC 9(07).
           05  WS-CP-G1-CIRCUIT            PIC S9(11)V9(02) COMP-3.
           05  WS-CP-G1-GROUP              PIC X(20).
       01  WS-CP-WORK-GROUP-2.
           05  WS-CP-G2-SEGMENT            PIC X(20).
           05  WS-CP-G2-SOURCE             PIC 9(07).
           05  WS-CP-G2-BAND               PIC X(10).
           05  WS-CP-G2-SEGMENT            PIC X(20).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 58.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUCV08 - LEGACY LAYOUT DOWN CONVERSION'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-CP-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-CP-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9955.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-CP-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-CP-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT IXCIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'IXCIN COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN INPUT SRCIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'SRCIN COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT REJOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'REJOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SUSOUT.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'SUSOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CTLOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RPTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RPTOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-CP-CYCLE-YYDDD.
           COMPUTE WS-CP-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-CP-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-CP-CNT-04.
           MOVE 0 TO WS-CP-CNT-01.
           MOVE 0 TO WS-CP-CNT-05.
       P1200-EXIT.
           EXIT.
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT
      * PRECEDES THIS PROGRAM IN THE JOB.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-CP-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-CP-TAB-CNT NOT < 300
               MOVE 'Y' TO WS-CP-SW-01
               ADD 1 TO WS-CP-CNT-06
           ELSE
               ADD 1 TO WS-CP-TAB-CNT
               SET WS-CP-IX TO WS-CP-TAB-CNT
               MOVE IC-OCN TO WS-CP-TB-KEY (WS-CP-IX)
               MOVE 0 TO WS-CP-TB-VAL (WS-CP-IX)
               MOVE SPACES TO WS-CP-TB-TXT (WS-CP-IX)
               ADD 1 TO WS-CP-CNT-03.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ IXCIN
               AT END MOVE 'Y' TO WS-CP-SW-01.
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
           IF WS-CP-ON-04
               PERFORM P2200-SELECT-CENTURY THRU
                   P2200-SELECT-CENTURY-EXIT.
           PERFORM P2300-EXPAND-LAYOUT THRU P2300-EXPAND-LAYOUT-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ IXCIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P2200-SELECT-CENTURY.
           MOVE 'N' TO WS-CP-SW-01.
           IF WS-CP-TXT-03 NOT = WS-CP-TXT-05
               MOVE 'Y' TO WS-CP-SW-01
               MOVE WS-CP-TXT-03 TO WS-CP-TXT-05
               ADD 1 TO WS-CP-CNT-06.
           MOVE 'N' TO WS-CP-SW-05.
           IF WS-CP-TAB-CNT > 0
               PERFORM P280-COMPARE-FIELD THRU P280-COMPARE-FIELD-EXIT
               VARYING WS-CP-SUB-04 FROM 1 BY 1
               UNTIL WS-CP-SUB-04 > WS-CP-TAB-CNT
               OR WS-CP-SW-05 = 'Y'.
       P2200-SELECT-CENTURY-EXIT.
           EXIT.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P2300-EXPAND-LAYOUT.
           CALL 'CABHASH' USING IC-OCN WS-ACC-OCN-HASH.
           ADD WS-CP-CNT-05 TO WS-ACC-SEQ-HASH.
       P2300-EXPAND-LAYOUT-EXIT.
           EXIT.
       P280-COMPARE-FIELD.
           SET WS-CP-IX TO WS-CP-SUB-02.
           IF WS-CP-TB-KEY (WS-CP-IX) = IC-GROUP
               MOVE 'Y' TO WS-CP-SW-05
               MOVE WS-CP-TB-VAL (WS-CP-IX) TO WS-CP-QTY-02
               MOVE WS-CP-SUB-02 TO WS-CP-SUB-04.
       P280-COMPARE-FIELD-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P3100-CLOSE-OFF-CENTURY.
           MOVE SPACES TO WS-CP-TXT-04.
           STRING IC-BAND DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IC-GROUP DELIMITED BY SIZE
               INTO WS-CP-TXT-04.
       P3100-CLOSE-OFF-CENTURY-EXIT.
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
           MOVE 'RECORDS READ' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO WS-CP-CNT-EDIT.
           MOVE WS-CP-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS WRITTEN' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-CP-CNT-EDIT.
           MOVE WS-CP-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS REJECTED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-CP-CNT-EDIT.
           MOVE WS-CP-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS SUMMARISED' TO PC-COL-001-020.
           MOVE WS-SUMM-CNT TO WS-CP-CNT-EDIT.
           MOVE WS-CP-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS CARRIED FWD' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-CP-CNT-EDIT.
           MOVE WS-CP-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-CP-CNT-01 TO WS-CP-CNT-EDIT.
           MOVE WS-CP-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 02' TO PC-COL-001-020.
           MOVE WS-CP-CNT-02 TO WS-CP-CNT-EDIT.
           MOVE WS-CP-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE WS-CP-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE 3 TO CT-STEP-SEQ.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE SPACES TO CT-FILLER.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE 0 TO CT-RC.
           MOVE WS-CP-TXT-07 TO CT-RESTART-KEY.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
       P8100-EXIT.
           EXIT.
      * P8200-CHECK-BALANCE - A RECORD THAT WAS HELD FOR THE NEXT
      * CYCLE IS BROUGHT INTO THE CARRIED FORWARD SIDE BEFORE THE
      * EQUATION IS TESTED.
       P8200-CHECK-BALANCE.
           ADD WS-CP-CNT-04 TO CT-CARRIED-FWD.
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
           CLOSE IXCIN.
           CLOSE SRCIN.
           CLOSE REJOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABUCV08 - END OF RUN'.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  CP-CNT-05 = ' WS-CP-CNT-05.
           DISPLAY '  CP-CNT-01 = ' WS-CP-CNT-01.
       P9000-EXIT.
           EXIT.
