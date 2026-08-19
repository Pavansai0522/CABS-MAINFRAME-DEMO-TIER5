      *****************************************************************
      * CABUCV05 - CODE PAGE AND SIGN CONVERSION                      *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               PCKIN   TELCABS.CABS.PCKIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               REJOUT  TELCABS.CABS.REJOUT         (LOCAL)     *
      *               CNVOUT  TELCABS.CABS.CNVOUT         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - THE OUTPUT IS REBUILT EACH RUN     *
      * REVISION HISTORY                                              *
      *   V1.00  1988-01-23  J.M.CASTILLO INITIAL RELEASE             *
      *   V1.01  1989-02-08  L.FERREIRA   OCCURS RAISED AFTER THE     *
      *                      FEBRUARY OVERFLOW                        *
      *   V1.05  1990-03-12  L.FERREIRA   RECOMPILE ONLY - COPYBOOK   *
      *                      CHANGE UPSTREAM                          *
      *   V1.08  1995-03-14  R.T.WHEELER  EFFECTIVE DATE FILTER ADDED *
      *                      PER AUDIT FINDING                        *
      *   V1.12  2014-10-20  D.OKONKWO    REPORT PAGINATION CORRECTED *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUCV05.
       AUTHOR. TELCABS APPLICATIONS - FILE CONVERSION.
      *****************************************************************
      * CODE PAGE AND SIGN CONVERSION. THE STEP IS DRIVEN ENTIRELY    *
      * FROM THE SYSIN PARM CARD AND THE DD ALLOCATIONS IN THE JOB.   *
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES     *
      * RATHER THAN LOW VALUES.                                       *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           C01 IS TO-NEW-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PCKIN ASSIGN TO UT-S-PCKIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT REJOUT ASSIGN TO UT-S-REJOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT CNVOUT ASSIGN TO UT-S-CNVOUT
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
      * PCKIN - PERMANENT DATASET HELD ON DASD.
       FD  PCKIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CV-IN-RECORD.
           05  IC-OCN                      PIC S9(13) COMP-3.
           05  IC-MEDIA                    PIC S9(09) COMP-3.
           05  IC-CODE                     PIC S9(11)V9(05) COMP-3.
           05  IC-CYCLE                    PIC S9(09)V9(02) COMP-3.
           05  IC-SOURCE                   PIC X(03).
           05  IC-TYPE                     PIC X(13).
           05  IC-SEQ                      PIC X(20).
           05  IC-BAN                      PIC S9(05) COMP-3.
           05  IC-STATE                    PIC 9(03).
           05  IC-OCN2                     PIC X(06).
           05  IC-STATUS                   PIC X(06).
           05  IC-ACCOUNT                  PIC S9(09)V9(05) COMP-3.
           05  IC-TYPE2                    PIC X(20).
           05  IC-OCN3                     PIC X(16).
           05  IC-SEQ2                     PIC X(10).
           05  IC-PERIOD                   PIC 9(03).
           05  IC-CODE2                    PIC X(02).
           05  IC-SEGMENT                  PIC X(16).
           05  IC-CYCLE2                   PIC X(16).
           05  IC-SEQ3                     PIC S9(09)V9(02) COMP-3.
           05  CV-FILL-01                  PIC X(2).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-CV-VIEW1 REDEFINES CABS-CV-IN-RECORD.
           05  R0C-SEQ                     PIC X(06).
           05  R0C-BAND                    PIC X(06).
           05  R0C-CENTRE                  PIC X(04).
           05  R0C-TARIFF                  PIC X(03).
           05  R0C-PERIOD                  PIC X(10).
           05  R0C-REST                    PIC X(151).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-CV-VIEW2 REDEFINES CABS-CV-IN-RECORD.
           05  R1C-ELEM                    PIC X(16).
           05  R1C-INVOICE                 PIC 9(02).
           05  R1C-CLASS                   PIC S9(07)V9(02) COMP-3.
           05  R1C-CIRCUIT                 PIC X(06).
           05  R1C-BAND                    PIC S9(09)V9(02) COMP-3.
           05  R1C-REST                    PIC X(145).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-CV-VIEW3 REDEFINES CABS-CV-IN-RECORD.
           05  R2C-BAN                     PIC 9(02).
           05  R2C-REGION                  PIC X(20).
           05  R2C-TARGET                  PIC 9(04).
           05  R2C-INVOICE                 PIC X(08).
           05  R2C-GROUP                   PIC 9(03).
           05  R2C-ELEM                    PIC X(10).
           05  R2C-INVOICE2                PIC 9(05).
           05  R2C-REST                    PIC X(128).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-CV-VIEW4 REDEFINES CABS-CV-IN-RECORD.
           05  R3C-JURIS                   PIC X(04).
           05  R3C-PERIOD                  PIC 9(06).
           05  R3C-BAND                    PIC S9(05) COMP-3.
           05  R3C-LEVEL                   PIC S9(15) COMP-3.
           05  R3C-CYCLE                   PIC X(08).
           05  R3C-TARGET                  PIC S9(13)V9(05) COMP-3.
           05  R3C-REST                    PIC X(141).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-CV-VIEW5 REDEFINES CABS-CV-IN-RECORD.
           05  R4C-BAND                    PIC X(13).
           05  R4C-STATE                   PIC 9(03).
           05  R4C-LEVEL                   PIC X(04).
           05  R4C-ACCOUNT                 PIC 9(02).
           05  R4C-TYPE                    PIC X(02).
           05  R4C-SEQ                     PIC S9(09)V9(02) COMP-3.
           05  R4C-BAN                     PIC X(13).
           05  R4C-MEDIA                   PIC S9(13) COMP-3.
           05  R4C-PERIOD                  PIC X(03).
           05  R4C-REST                    PIC X(127).
      * REJOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  REJOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 90 CHARACTERS.
       01  CABS-CV-OUT-RECORD.
           05  OC-STATUS                   PIC S9(05) COMP-3.
           05  OC-ELEM                     PIC 9(05).
           05  OC-BAND                     PIC X(13).
           05  OC-SOURCE                   PIC X(16).
           05  OC-CENTRE                   PIC X(03).
           05  OC-GROUP                    PIC X(04).
           05  OC-CODE                     PIC S9(11)V9(02) COMP-3.
           05  OC-STATUS2                  PIC X(03).
           05  OC-OCN                      PIC 9(05).
           05  OC-SOURCE2                  PIC S9(09) COMP-3.
           05  OC-BAN                      PIC X(20).
           05  CV-FILL-02                  PIC X(6).
      * CNVOUT - PERMANENT DATASET HELD ON DASD.
       FD  CNVOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 90 CHARACTERS.
       01  CABS-CV-OUT1-RECORD         PIC X(90).
      * CTLOUT - WORK FILE, DELETED AT STEP END.
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
      * SHARED LAYOUT PULLED IN FOR THE CENTURY SIDE.
       COPY CABSCOMM.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUCV05'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V1.22'.
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
           05  WS-CV-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CV-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CV-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CV-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CV-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CV-CNT-06                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CV-CNT-07                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CV-CNT-08                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CV-CNT-09                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-CV-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CV-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CV-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CV-QTY-04                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CV-QTY-05                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-CV-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CV-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CV-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CV-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CV-AMT-05                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CV-AMT-06                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-CV-TXT-01                PIC X(10) VALUE SPACES.
           05  WS-CV-TXT-02                PIC X(16) VALUE SPACES.
           05  WS-CV-TXT-03                PIC X(08) VALUE SPACES.
           05  WS-CV-TXT-04                PIC X(12) VALUE SPACES.
           05  WS-CV-TXT-05                PIC X(26) VALUE SPACES.
           05  WS-CV-TXT-06                PIC X(26) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-CV-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-CV-ON-01                 VALUE 'Y'.
               88  WS-CV-OFF-01                VALUE 'N'.
           05  WS-CV-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-CV-ON-02                 VALUE 'Y'.
               88  WS-CV-OFF-02                VALUE 'N'.
           05  WS-CV-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-CV-ON-03                 VALUE 'Y'.
               88  WS-CV-OFF-03                VALUE 'N'.
           05  WS-CV-SW-04                 PIC X(01) VALUE 'N'.
               88  WS-CV-ON-04                 VALUE 'Y'.
               88  WS-CV-OFF-04                VALUE 'N'.
           05  WS-CV-SW-05                 PIC X(01) VALUE 'N'.
               88  WS-CV-ON-05                 VALUE 'Y'.
               88  WS-CV-OFF-05                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-CV-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CV-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CV-SUB-03                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. LOADED ONCE AT INITIALISATION AND SEARCHED WITH
      * A SUBSCRIPTED WALK.
       01  WS-CV-TABLE.
           05  WS-CV-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-CV-TB-ENTRY OCCURS 300 TIMES
                                       INDEXED BY WS-CV-IX.
               10  WS-CV-TB-KEY                PIC X(13).
               10  WS-CV-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-CV-TB-TXT                PIC X(40).
               10  WS-CV-TB-EFF                PIC 9(05).
               10  WS-CV-TB-EXP                PIC 9(05).
       01  WS-CV-WORK-GROUP-1.
           05  WS-CV-G1-SEQ                PIC 9(07).
           05  WS-CV-G1-STATUS             PIC 9(07).
           05  WS-CV-G1-TARIFF             PIC X(10).
           05  WS-CV-G1-BAND               PIC X(20).
           05  WS-CV-G1-BAND               PIC S9(11)V9(02) COMP-3.
           05  WS-CV-G1-TYPE               PIC 9(05).
           05  WS-CV-G1-CODE               PIC 9(07).
       01  WS-CV-WORK-GROUP-2.
           05  WS-CV-G2-REGION             PIC S9(09) COMP-3.
           05  WS-CV-G2-SOURCE             PIC S9(11)V9(02) COMP-3.
           05  WS-CV-G2-CENTRE             PIC 9(07).
           05  WS-CV-G2-GROUP              PIC 9(07).
           05  WS-CV-G2-OCN                PIC 9(05).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 50.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUCV05 - CODE PAGE AND SIGN CONVERSION'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-CV-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-CV-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9938.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-CV-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-CV-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT PCKIN.
           IF WS-FS-INPUT NOT = '00'
               DISPLAY 'PCKIN FILE STATUS = ' WS-FS-INPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON PCKIN - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT REJOUT.
           IF WS-FS-OUTPUT NOT = '00'
               DISPLAY 'REJOUT FILE STATUS = ' WS-FS-OUTPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON REJOUT - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CNVOUT.
           IF WS-FS-OUTPUT NOT = '00'
               DISPLAY 'CNVOUT FILE STATUS = ' WS-FS-OUTPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON CNVOUT - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               DISPLAY 'CTLOUT FILE STATUS = ' WS-FS-CONTROL
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON CTLOUT - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RPTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               DISPLAY 'RPTOUT FILE STATUS = ' WS-FS-OUTPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON RPTOUT - CHECK THE ALLOCATION' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-CV-CYCLE-YYDDD.
           COMPUTE WS-CV-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-CV-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-CV-CNT-01.
           MOVE 0 TO WS-CV-CNT-07.
           MOVE 0 TO WS-CV-CNT-05.
       P1200-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-CV-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-CV-TAB-CNT NOT < 300
               MOVE 'Y' TO WS-CV-SW-01
               ADD 1 TO WS-CV-CNT-08
           ELSE
               ADD 1 TO WS-CV-TAB-CNT
               SET WS-CV-IX TO WS-CV-TAB-CNT
               MOVE IC-SOURCE TO WS-CV-TB-KEY (WS-CV-IX)
               MOVE 0 TO WS-CV-TB-VAL (WS-CV-IX)
               MOVE SPACES TO WS-CV-TB-TXT (WS-CV-IX)
               ADD 1 TO WS-CV-CNT-02.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ PCKIN
               AT END MOVE 'Y' TO WS-CV-SW-01.
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
           IF WS-CV-ON-02
               PERFORM P2200-EDIT-PACKED THRU P2200-EDIT-PACKED-EXIT.
           PERFORM P2300-EDIT-FIELD THRU P2300-EDIT-FIELD-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ PCKIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-EDIT-PACKED.
           MOVE SPACES TO CABS-CV-OUT-RECORD.
           MOVE IC-TYPE TO OC-STATUS.
           MOVE IC-STATE TO OC-ELEM.
           MOVE IC-SEGMENT TO OC-BAND.
           MOVE IC-SOURCE TO OC-SOURCE.
           WRITE CABS-CV-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
           MOVE 'N' TO WS-CV-SW-02.
           IF WS-CV-TAB-CNT > 0
               PERFORM P260-COMPARE-RECORD THRU P260-COMPARE-RECORD-EXIT
               VARYING WS-CV-SUB-01 FROM 1 BY 1
               UNTIL WS-CV-SUB-01 > WS-CV-TAB-CNT
               OR WS-CV-SW-02 = 'Y'.
       P2200-EDIT-PACKED-EXIT.
           EXIT.
       P2300-EDIT-FIELD.
           IF WS-CV-AMT-06 < 27
               MOVE 27 TO WS-CV-AMT-06
               ADD 1 TO WS-CV-CNT-02.
           IF WS-CV-AMT-06 > 30264
               MOVE 30264 TO WS-CV-AMT-06
               ADD 1 TO WS-CV-CNT-06.
       P2300-EDIT-FIELD-EXIT.
           EXIT.
       P260-COMPARE-RECORD.
           SET WS-CV-IX TO WS-CV-SUB-03.
           IF WS-CV-TB-KEY (WS-CV-IX) = IC-CYCLE2
               MOVE 'Y' TO WS-CV-SW-01
               MOVE WS-CV-TB-VAL (WS-CV-IX) TO WS-CV-QTY-01
               MOVE WS-CV-SUB-03 TO WS-CV-SUB-01.
       P260-COMPARE-RECORD-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-CLOSE-OFF-FIELD.
           MOVE 0 TO WS-CV-QTY-01.
           MOVE 0 TO WS-CV-QTY-02.
           MOVE 0 TO WS-CV-QTY-03.
           MOVE 0 TO WS-CV-AMT-02.
       P3100-CLOSE-OFF-FIELD-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P3200-WRITE-LAYOUT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-CV-TXT-02 TO PC-COL-001-020.
           MOVE WS-CV-TXT-06 TO PC-COL-021-060.
           MOVE WS-CV-AMT-01 TO WS-CV-AMT-EDIT.
           MOVE WS-CV-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P3200-WRITE-LAYOUT-EXIT.
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
           MOVE WS-READ-CNT TO WS-CV-CNT-EDIT.
           MOVE WS-CV-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'WRITTEN TO OUTPUT' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-CV-CNT-EDIT.
           MOVE WS-CV-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'REJECTED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-CV-CNT-EDIT.
           MOVE WS-CV-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'CARRIED FORWARD' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-CV-CNT-EDIT.
           MOVE WS-CV-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'ROLLED INTO SUMMARY' TO PC-COL-001-020.
           MOVE WS-SUMM-CNT TO WS-CV-CNT-EDIT.
           MOVE WS-CV-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-CV-CNT-01 TO WS-CV-CNT-EDIT.
           MOVE WS-CV-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE 4 TO CT-STEP-SEQ.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE WS-CV-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-CV-TXT-03 TO CT-RESTART-KEY.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE WS-CV-CNT-07 TO CT-RC.
           MOVE SPACES TO CT-FILLER.
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
           CLOSE PCKIN.
           CLOSE REJOUT.
           CLOSE CNVOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABUCV05 - RUN COMPLETE'.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  CV-CNT-03 = ' WS-CV-CNT-03.
           DISPLAY '  CV-CNT-07 = ' WS-CV-CNT-07.
           DISPLAY '  CV-CNT-02 = ' WS-CV-CNT-02.
           DISPLAY '  CV-CNT-04 = ' WS-CV-CNT-04.
       P9000-EXIT.
           EXIT.
