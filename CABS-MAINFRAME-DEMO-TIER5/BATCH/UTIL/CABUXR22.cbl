      *****************************************************************
      * CABUXR22 - ACCOUNT TO INVOICE CROSS REFERENCE                 *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               KEYIN   TELCABS.CABS.KEYIN          (LOCAL)     *
      *               LFTIN   TELCABS.CABS.LFTIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               MTCOUT  TELCABS.CABS.MTCOUT         (LOCAL)     *
      *               LNKOUT  TELCABS.CABS.LNKOUT         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - NOTHING IS UPDATED IN PLACE        *
      * REVISION HISTORY                                              *
      *   V1.00  1987-09-06  R.T.WHEELER  INITIAL RELEASE             *
      *   V1.01  2002-05-21  B.R.HALVORSEN CENTURY PIVOT APPLIED TO   *
      *                      THE CYCLE DATE                           *
      *   V1.02  2003-12-10  G.PRZYBYLSKI ROUNDING RULE TAKEN FROM THE*
      *                      RATE ROW                                 *
      *   V1.03  2004-10-01  C.ADEYEMI    CARRIER TYPE BROUGHT ONTO   *
      *                      THE EXTRACT                              *
      *   V1.04  2005-10-03  D.OKONKWO    TABLE LIMIT RAISED FOR THE  *
      *                      SOUTHEAST CENTRES                        *
      *   V1.05  2007-01-19  W.J.MCALLISTER REGION SIZE REDUCED -     *
      *                      TABLE MOVED OUT OF WORKING STORAGE       *
      *   V1.09  2009-09-08  T.YAMASHITA  REPORT PAGINATION CORRECTED *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUXR22.
       AUTHOR. TELCABS APPLICATIONS - CROSS REFERENCE REPORT.
      *****************************************************************
      * ACCOUNT TO INVOICE CROSS REFERENCE. THE STEP RUNS ONCE PER    *
      * BILL CYCLE AND IS RERUN FROM THE TOP IF IT FAILS.             *
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
           SELECT KEYIN ASSIGN TO UT-S-KEYIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT LFTIN ASSIGN TO UT-S-LFTIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT MTCOUT ASSIGN TO UT-S-MTCOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT LNKOUT ASSIGN TO UT-S-LNKOUT
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
      * KEYIN - WORK FILE, DELETED AT STEP END.
       FD  KEYIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 130 CHARACTERS.
       01  CABS-BZ-IN-RECORD.
           05  IB-CIRCUIT                  PIC X(03).
           05  IB-LEVEL                    PIC S9(05) COMP-3.
           05  IB-PERIOD                   PIC X(10).
           05  IB-REGION                   PIC X(03).
           05  IB-SEGMENT                  PIC S9(07)V9(05) COMP-3.
           05  IB-TARIFF                   PIC X(08).
           05  IB-TARGET                   PIC 9(03).
           05  IB-CARRIER                  PIC X(06).
           05  IB-TARIFF2                  PIC S9(13) COMP-3.
           05  IB-PERIOD2                  PIC X(10).
           05  IB-CENTRE                   PIC S9(15) COMP-3.
           05  IB-JURIS                    PIC S9(09) COMP-3.
           05  IB-MEDIA                    PIC S9(13) COMP-3.
           05  IB-SEQ                      PIC X(16).
           05  IB-CYCLE                    PIC 9(04).
           05  IB-STATE                    PIC X(06).
           05  IB-OCN                      PIC 9(07).
           05  IB-SEQ2                     PIC X(13).
           05  BZ-FILL-01                  PIC X(4).
      * LFTIN - WORK FILE, DELETED AT STEP END.
       FD  LFTIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 130 CHARACTERS.
       01  CABS-BZ-ALT1-RECORD.
           05  A1-ELEM                     PIC X(03).
           05  A1-BAN                      PIC 9(07).
           05  A1-STATE                    PIC X(10).
           05  A1-BAND                     PIC 9(05).
           05  A1-JURIS                    PIC X(03).
           05  A1-CYCLE                    PIC 9(06).
           05  A1-CYCLE2                   PIC X(13).
           05  BZ-FILL-02                  PIC X(83).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-BZ-VIEW1 REDEFINES CABS-BZ-IN-RECORD.
           05  R0B-SOURCE                  PIC S9(13)V9(02) COMP-3.
           05  R0B-TYPE                    PIC 9(07).
           05  R0B-STATE                   PIC S9(07) COMP-3.
           05  R0B-CYCLE                   PIC X(20).
           05  R0B-CODE                    PIC X(20).
           05  R0B-BAN                     PIC X(06).
           05  R0B-CIRCUIT                 PIC 9(05).
           05  R0B-JURIS                   PIC X(08).
           05  R0B-REST                    PIC X(52).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-BZ-VIEW2 REDEFINES CABS-BZ-IN-RECORD.
           05  R1B-GROUP                   PIC 9(04).
           05  R1B-PERIOD                  PIC 9(06).
           05  R1B-STATUS                  PIC X(16).
           05  R1B-PERIOD2                 PIC X(08).
           05  R1B-ACCOUNT                 PIC S9(07)V9(02) COMP-3.
           05  R1B-STATUS2                 PIC S9(09) COMP-3.
           05  R1B-REST                    PIC X(86).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-BZ-VIEW3 REDEFINES CABS-BZ-IN-RECORD.
           05  R2B-INVOICE                 PIC X(08).
           05  R2B-CYCLE                   PIC X(10).
           05  R2B-JURIS                   PIC X(02).
           05  R2B-CODE                    PIC S9(15) COMP-3.
           05  R2B-JURIS2                  PIC S9(15) COMP-3.
           05  R2B-CODE2                   PIC 9(02).
           05  R2B-REST                    PIC X(92).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-BZ-VIEW4 REDEFINES CABS-BZ-IN-RECORD.
           05  R3B-CIRCUIT                 PIC X(20).
           05  R3B-STATE                   PIC S9(11) COMP-3.
           05  R3B-STATE2                  PIC S9(07) COMP-3.
           05  R3B-CODE                    PIC X(20).
           05  R3B-BAN                     PIC 9(07).
           05  R3B-JURIS                   PIC S9(07)V9(02) COMP-3.
           05  R3B-CARRIER                 PIC X(04).
           05  R3B-ELEM                    PIC S9(09)V9(02) COMP-3.
           05  R3B-REST                    PIC X(58).
      * MTCOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  MTCOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 160 CHARACTERS.
       01  CABS-BZ-OUT-RECORD.
           05  OB-SEGMENT                  PIC X(20).
           05  OB-ACCOUNT                  PIC S9(13) COMP-3.
           05  OB-GROUP                    PIC X(06).
           05  OB-REGION                   PIC S9(09)V9(05) COMP-3.
           05  OB-GROUP2                   PIC 9(05).
           05  OB-TARIFF                   PIC X(04).
           05  OB-JURIS                    PIC 9(02).
           05  OB-CYCLE                    PIC 9(07).
           05  OB-JURIS2                   PIC X(13).
           05  OB-CLASS                    PIC X(10).
           05  OB-ELEM                     PIC 9(04).
           05  OB-BAN                      PIC 9(09).
           05  OB-SEGMENT2                 PIC S9(09) COMP-3.
           05  OB-TYPE                     PIC S9(11)V9(05) COMP-3.
           05  OB-STATUS                   PIC 9(04).
           05  OB-REGION2                  PIC X(06).
           05  OB-SEQ                      PIC X(13).
           05  OB-STATUS2                  PIC S9(09)V9(05) COMP-3.
           05  OB-BAND                     PIC S9(11)V9(02) COMP-3.
           05  OB-CENTRE                   PIC S9(13)V9(02) COMP-3.
           05  BZ-FILL-03                  PIC X(5).
      * LNKOUT - CATALOGUED GENERATION DATA GROUP.
       FD  LNKOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 160 CHARACTERS.
       01  CABS-BZ-OUT1-RECORD         PIC X(160).
      * CTLOUT - WORK FILE, DELETED AT STEP END.
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD              PIC X(180).
      * RPTOUT - WORK FILE, DELETED AT STEP END.
       FD  RPTOUT
           RECORDING MODE IS FA
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       COPY CABSPRNT.
       WORKING-STORAGE SECTION.
      * STANDARD SHARED WORKING STORAGE. SEE CABSWRK.
       COPY CABSWRK.
      * SHARED LAYOUT PULLED IN FOR THE ORPHAN SIDE.
       COPY CABSCARR.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUXR22'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V1.04'.
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
           05  WS-BZ-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BZ-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BZ-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BZ-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BZ-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BZ-CNT-06                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BZ-CNT-07                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BZ-CNT-08                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BZ-CNT-09                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BZ-CNT-10                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BZ-CNT-11                PIC S9(09) COMP-3 VALUE 0.
           05  WS-BZ-CNT-12                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-BZ-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BZ-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BZ-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BZ-QTY-04                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BZ-QTY-05                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BZ-QTY-06                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BZ-QTY-07                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-BZ-QTY-08                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-BZ-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BZ-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BZ-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BZ-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-BZ-AMT-05                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-BZ-TXT-01                PIC X(26) VALUE SPACES.
           05  WS-BZ-TXT-02                PIC X(16) VALUE SPACES.
           05  WS-BZ-TXT-03                PIC X(20) VALUE SPACES.
           05  WS-BZ-TXT-04                PIC X(26) VALUE SPACES.
           05  WS-BZ-TXT-05                PIC X(10) VALUE SPACES.
           05  WS-BZ-TXT-06                PIC X(10) VALUE SPACES.
           05  WS-BZ-TXT-07                PIC X(30) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-BZ-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-BZ-ON-01                 VALUE 'Y'.
               88  WS-BZ-OFF-01                VALUE 'N'.
           05  WS-BZ-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-BZ-ON-02                 VALUE 'Y'.
               88  WS-BZ-OFF-02                VALUE 'N'.
           05  WS-BZ-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-BZ-ON-03                 VALUE 'Y'.
               88  WS-BZ-OFF-03                VALUE 'N'.
           05  WS-BZ-SW-04                 PIC X(01) VALUE 'N'.
               88  WS-BZ-ON-04                 VALUE 'Y'.
               88  WS-BZ-OFF-04                VALUE 'N'.
           05  WS-BZ-SW-05                 PIC X(01) VALUE 'N'.
               88  WS-BZ-ON-05                 VALUE 'Y'.
               88  WS-BZ-OFF-05                VALUE 'N'.
           05  WS-BZ-SW-06                 PIC X(01) VALUE 'N'.
               88  WS-BZ-ON-06                 VALUE 'Y'.
               88  WS-BZ-OFF-06                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-BZ-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-BZ-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-BZ-SUB-03                PIC S9(04) COMP-3 VALUE 0.
           05  WS-BZ-SUB-04                PIC S9(04) COMP-3 VALUE 0.
           05  WS-BZ-SUB-05                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. LOADED ONCE AT INITIALISATION AND SEARCHED WITH
      * A SUBSCRIPTED WALK.
       01  WS-BZ-TABLE.
           05  WS-BZ-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-BZ-TB-ENTRY OCCURS 300 TIMES
                                       INDEXED BY WS-BZ-IX.
               10  WS-BZ-TB-KEY                PIC X(08).
               10  WS-BZ-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-BZ-TB-TXT                PIC X(40).
               10  WS-BZ-TB-EFF                PIC 9(05).
               10  WS-BZ-TB-EXP                PIC 9(05).
       01  WS-BZ-WORK-GROUP-1.
           05  WS-BZ-G1-TARGET             PIC 9(07).
           05  WS-BZ-G1-BAND               PIC X(10).
           05  WS-BZ-G1-CENTRE             PIC S9(11)V9(02) COMP-3.
           05  WS-BZ-G1-CYCLE              PIC 9(05).
       01  WS-BZ-WORK-GROUP-2.
           05  WS-BZ-G2-ELEM               PIC X(10).
           05  WS-BZ-G2-CENTRE             PIC S9(11)V9(02) COMP-3.
           05  WS-BZ-G2-STATUS             PIC X(10).
           05  WS-BZ-G2-OCN                PIC 9(05).
           05  WS-BZ-G2-CYCLE              PIC S9(11)V9(02) COMP-3.
       01  WS-BZ-WORK-GROUP-3.
           05  WS-BZ-G3-ELEM               PIC X(20).
           05  WS-BZ-G3-ELEM               PIC 9(05).
           05  WS-BZ-G3-TYPE               PIC 9(05).
           05  WS-BZ-G3-TARIFF             PIC X(10).
       01  WS-BZ-WORK-GROUP-4.
           05  WS-BZ-G4-SOURCE             PIC S9(09) COMP-3.
           05  WS-BZ-G4-TARIFF             PIC 9(07).
           05  WS-BZ-G4-CENTRE             PIC X(20).
           05  WS-BZ-G4-BAN                PIC 9(05).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 60.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUXR22 - ACCOUNT TO INVOICE CROSS REFERENCE'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-BZ-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-BZ-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
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
           05  WS-BZ-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-BZ-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT KEYIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'KEYIN COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN INPUT LFTIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'LFTIN COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT MTCOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'MTCOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT LNKOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'LNKOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-BZ-CYCLE-YYDDD.
           COMPUTE WS-BZ-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-BZ-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-BZ-CNT-11.
           MOVE 0 TO WS-BZ-CNT-08.
           MOVE 0 TO WS-BZ-CNT-09.
       P1200-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-BZ-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-BZ-TAB-CNT NOT < 300
               MOVE 'Y' TO WS-BZ-SW-01
               ADD 1 TO WS-BZ-CNT-01
           ELSE
               ADD 1 TO WS-BZ-TAB-CNT
               SET WS-BZ-IX TO WS-BZ-TAB-CNT
               MOVE IB-CENTRE TO WS-BZ-TB-KEY (WS-BZ-IX)
               MOVE 0 TO WS-BZ-TB-VAL (WS-BZ-IX)
               MOVE SPACES TO WS-BZ-TB-TXT (WS-BZ-IX)
               ADD 1 TO WS-BZ-CNT-10.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ KEYIN
               AT END MOVE 'Y' TO WS-BZ-SW-01.
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
           IF WS-BZ-ON-05
               PERFORM P2200-DERIVE-MATCH THRU P2200-DERIVE-MATCH-EXIT.
           PERFORM P2300-DERIVE-MATCH THRU P2300-DERIVE-MATCH-EXIT.
           PERFORM P2400-DERIVE-PAIR THRU P2400-DERIVE-PAIR-EXIT.
           IF WS-BZ-ON-03
               PERFORM P2500-SELECT-PAIR THRU P2500-SELECT-PAIR-EXIT.
           PERFORM P2600-EDIT-ORPHAN THRU P2600-EDIT-ORPHAN-EXIT.
           PERFORM P2700-BUILD-PAIR THRU P2700-BUILD-PAIR-EXIT.
           PERFORM P2800-APPLY-LINK THRU P2800-APPLY-LINK-EXIT.
           PERFORM P2900-RESOLVE-SIDE THRU P2900-RESOLVE-SIDE-EXIT.
           PERFORM P21000-CONVERT-LINK THRU P21000-CONVERT-LINK-EXIT.
           IF WS-BZ-ON-01
               PERFORM P21100-SELECT-LINK THRU P21100-SELECT-LINK-EXIT.
           PERFORM P21200-SPLIT-PAIR THRU P21200-SPLIT-PAIR-EXIT.
           PERFORM P21300-CONVERT-PAIR THRU P21300-CONVERT-PAIR-EXIT.
           IF WS-BZ-ON-02
               PERFORM P21400-SPLIT-REFERENCE THRU
                   P21400-SPLIT-REFERENCE-EXIT.
           PERFORM P21500-EXPAND-PAIR THRU P21500-EXPAND-PAIR-EXIT.
           PERFORM P21600-SPLIT-ORPHAN THRU P21600-SPLIT-ORPHAN-EXIT.
           PERFORM P21700-APPLY-SIDE THRU P21700-APPLY-SIDE-EXIT.
           PERFORM P21800-CHECK-REFERENCE THRU
               P21800-CHECK-REFERENCE-EXIT.
           IF WS-BZ-ON-05
               PERFORM P21900-RESOLVE-GROUP THRU
                   P21900-RESOLVE-GROUP-EXIT.
           PERFORM P22000-VALIDATE-GROUP THRU
               P22000-VALIDATE-GROUP-EXIT.
           IF WS-BZ-ON-02
               PERFORM P22100-DERIVE-PAIR THRU P22100-DERIVE-PAIR-EXIT.
           PERFORM P22200-BUILD-SIDE THRU P22200-BUILD-SIDE-EXIT.
           PERFORM P22300-CHECK-GROUP THRU P22300-CHECK-GROUP-EXIT.
           PERFORM P22400-DERIVE-GROUP THRU P22400-DERIVE-GROUP-EXIT.
           PERFORM P22500-BUILD-SIDE THRU P22500-BUILD-SIDE-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ KEYIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-DERIVE-MATCH.
           MOVE SPACES TO CABS-BZ-OUT-RECORD.
           MOVE IB-PERIOD2 TO OB-SEGMENT.
           MOVE IB-PERIOD2 TO OB-ACCOUNT.
           MOVE IB-CARRIER TO OB-GROUP.
           MOVE IB-CYCLE TO OB-REGION.
           MOVE IB-TARIFF2 TO OB-GROUP2.
           MOVE IB-PERIOD TO OB-TARIFF.
           MOVE IB-OCN TO OB-JURIS.
           WRITE CABS-BZ-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
           MOVE 'N' TO WS-BZ-SW-05.
           IF WS-BZ-TAB-CNT > 0
               PERFORM P270-COMPARE-PAIR THRU P270-COMPARE-PAIR-EXIT
               VARYING WS-BZ-SUB-01 FROM 1 BY 1
               UNTIL WS-BZ-SUB-01 > WS-BZ-TAB-CNT
               OR WS-BZ-SW-05 = 'Y'.
       P2200-DERIVE-MATCH-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P2300-DERIVE-MATCH.
           UNSTRING WS-BZ-TXT-06 DELIMITED BY '/'
               INTO WS-BZ-TXT-01
               WS-BZ-TXT-01
               TALLYING IN WS-BZ-CNT-03.
       P2300-DERIVE-MATCH-EXIT.
           EXIT.
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT
      * PRECEDES THIS PROGRAM IN THE JOB.
       P2400-DERIVE-PAIR.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-BAN-UNKNOWN TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-BZ-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
       P2400-DERIVE-PAIR-EXIT.
           EXIT.
       P2500-SELECT-PAIR.
           MOVE IB-MEDIA TO WS-BZ-TXT-06.
           MOVE IB-STATE TO WS-BZ-TXT-04.
           MOVE IB-PERIOD TO WS-BZ-TXT-02.
           ADD 1 TO WS-BZ-CNT-11.
       P2500-SELECT-PAIR-EXIT.
           EXIT.
       P2600-EDIT-ORPHAN.
           MOVE SPACES TO WS-BZ-TXT-01.
           STRING IB-CARRIER DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IB-CYCLE DELIMITED BY SIZE
               INTO WS-BZ-TXT-01.
       P2600-EDIT-ORPHAN-EXIT.
           EXIT.
       P2700-BUILD-PAIR.
           MOVE WS-BZ-AMT-01 TO WS-BZ-AMT-05.
           IF WS-BZ-AMT-05 < 0
               COMPUTE WS-BZ-AMT-05 = 0 - WS-BZ-AMT-01.
       P2700-BUILD-PAIR-EXIT.
           EXIT.
       P2800-APPLY-LINK.
           CALL 'CABCTLWR' USING WS-BZ-TXT-06 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-BZ-CNT-09.
       P2800-APPLY-LINK-EXIT.
           EXIT.
       P2900-RESOLVE-SIDE.
           MOVE 0 TO WS-BZ-QTY-08.
           MOVE 0 TO WS-BZ-QTY-04.
           MOVE 0 TO WS-BZ-AMT-02.
       P2900-RESOLVE-SIDE-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P21000-CONVERT-LINK.
           IF WS-BZ-AMT-03 NOT = 0
               COMPUTE WS-BZ-QTY-03 = WS-BZ-AMT-05 * 100 / WS-BZ-AMT-03
           ELSE
               MOVE 0 TO WS-BZ-QTY-03.
       P21000-CONVERT-LINK-EXIT.
           EXIT.
       P21100-SELECT-LINK.
           IF WS-BZ-AMT-01 < 4
               MOVE 4 TO WS-BZ-AMT-01
               ADD 1 TO WS-BZ-CNT-03.
           IF WS-BZ-AMT-01 > 88732
               MOVE 88732 TO WS-BZ-AMT-01
               ADD 1 TO WS-BZ-CNT-02.
       P21100-SELECT-LINK-EXIT.
           EXIT.
       P21200-SPLIT-PAIR.
           CALL 'CABHASH' USING IB-PERIOD WS-ACC-OCN-HASH.
           ADD WS-BZ-CNT-06 TO WS-ACC-SEQ-HASH.
       P21200-SPLIT-PAIR-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P21300-CONVERT-PAIR.
           MOVE 0 TO WS-BZ-CNT-08.
           INSPECT WS-BZ-TXT-06 TALLYING WS-BZ-CNT-08
               FOR ALL SPACES.
           INSPECT WS-BZ-TXT-06 REPLACING ALL LOW-VALUES BY SPACES.
       P21300-CONVERT-PAIR-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P21400-SPLIT-REFERENCE.
           MOVE 'Y' TO WS-BZ-SW-01.
           IF IB-TARIFF2 < 28
               MOVE 'N' TO WS-BZ-SW-01
               ADD 1 TO WS-BZ-CNT-03.
           IF IB-TARIFF2 > 1866
               MOVE 'N' TO WS-BZ-SW-01
               ADD 1 TO WS-BZ-CNT-11.
       P21400-SPLIT-REFERENCE-EXIT.
           EXIT.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P21500-EXPAND-PAIR.
           IF IB-CYCLE = 'B'
               ADD 1 TO WS-BZ-CNT-06
           ELSE
               IF IB-CYCLE = 'C'
                   ADD 1 TO WS-BZ-CNT-12
               ELSE
                   IF IB-CYCLE = 'B'
                       ADD 1 TO WS-BZ-CNT-07
                   ELSE
                       ADD 1 TO WS-BZ-CNT-02.
       P21500-EXPAND-PAIR-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P21600-SPLIT-ORPHAN.
           MOVE 'N' TO WS-BZ-SW-06.
           IF WS-BZ-TXT-05 NOT = WS-BZ-TXT-05
               MOVE 'Y' TO WS-BZ-SW-06
               MOVE WS-BZ-TXT-05 TO WS-BZ-TXT-05
               ADD 1 TO WS-BZ-CNT-12.
       P21600-SPLIT-ORPHAN-EXIT.
           EXIT.
       P21700-APPLY-SIDE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-BZ-TXT-06 TO PC-COL-001-020.
           MOVE WS-BZ-TXT-01 TO PC-COL-021-060.
           MOVE WS-BZ-AMT-05 TO WS-BZ-AMT-EDIT.
           MOVE WS-BZ-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P21700-APPLY-SIDE-EXIT.
           EXIT.
       P21800-CHECK-REFERENCE.
           ADD IB-CENTRE TO WS-BZ-QTY-05.
           COMPUTE WS-BZ-AMT-03 = WS-BZ-QTY-05 * WS-BZ-QTY-05.
           ADD WS-BZ-AMT-03 TO WS-BZ-AMT-03.
       P21800-CHECK-REFERENCE-EXIT.
           EXIT.
       P21900-RESOLVE-GROUP.
           MOVE SPACES TO CABS-BZ-OUT-RECORD.
           MOVE IB-CARRIER TO OB-SEGMENT.
           MOVE IB-MEDIA TO OB-ACCOUNT.
           MOVE IB-LEVEL TO OB-GROUP.
           MOVE IB-CIRCUIT TO OB-REGION.
           MOVE IB-TARGET TO OB-GROUP2.
           MOVE IB-PERIOD TO OB-TARIFF.
           MOVE IB-CENTRE TO OB-JURIS.
           MOVE IB-MEDIA TO OB-CYCLE.
           MOVE IB-PERIOD TO OB-JURIS2.
           WRITE CABS-BZ-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P21900-RESOLVE-GROUP-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P22000-VALIDATE-GROUP.
           UNSTRING WS-BZ-TXT-04 DELIMITED BY '/'
               INTO WS-BZ-TXT-06
               WS-BZ-TXT-05
               TALLYING IN WS-BZ-CNT-11.
       P22000-VALIDATE-GROUP-EXIT.
           EXIT.
       P22100-DERIVE-PAIR.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-DUP-SEQ TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-BZ-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
       P22100-DERIVE-PAIR-EXIT.
           EXIT.
       P22200-BUILD-SIDE.
           MOVE IB-CARRIER TO WS-BZ-TXT-03.
           MOVE IB-PERIOD TO WS-BZ-TXT-04.
           MOVE IB-OCN TO WS-BZ-TXT-01.
           ADD 1 TO WS-BZ-CNT-04.
       P22200-BUILD-SIDE-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P22300-CHECK-GROUP.
           MOVE SPACES TO WS-BZ-TXT-04.
           STRING IB-OCN DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IB-PERIOD2 DELIMITED BY SIZE
               INTO WS-BZ-TXT-04.
       P22300-CHECK-GROUP-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P22400-DERIVE-GROUP.
           MOVE WS-BZ-AMT-01 TO WS-BZ-AMT-01.
           IF WS-BZ-AMT-01 < 0
               COMPUTE WS-BZ-AMT-01 = 0 - WS-BZ-AMT-01.
       P22400-DERIVE-GROUP-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P22500-BUILD-SIDE.
           CALL 'CABHASH' USING WS-BZ-TXT-01 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-BZ-CNT-10.
       P22500-BUILD-SIDE-EXIT.
           EXIT.
       P270-COMPARE-PAIR.
           SET WS-BZ-IX TO WS-BZ-SUB-03.
           IF WS-BZ-TB-KEY (WS-BZ-IX) = IB-CYCLE
               MOVE 'Y' TO WS-BZ-SW-03
               MOVE WS-BZ-TB-VAL (WS-BZ-IX) TO WS-BZ-QTY-08
               MOVE WS-BZ-SUB-03 TO WS-BZ-SUB-02.
       P270-COMPARE-PAIR-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P3100-STAGE-GROUP.
           MOVE 0 TO WS-BZ-QTY-02.
           MOVE 0 TO WS-BZ-QTY-04.
           MOVE 0 TO WS-BZ-QTY-05.
           MOVE 0 TO WS-BZ-AMT-02.
           MOVE 0 TO WS-BZ-AMT-05.
       P3100-STAGE-GROUP-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P3200-EMIT-SIDE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-BZ-TXT-07 TO PC-COL-001-020.
           MOVE WS-BZ-TXT-04 TO PC-COL-021-060.
           MOVE WS-BZ-AMT-01 TO WS-BZ-AMT-EDIT.
           MOVE WS-BZ-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P3200-EMIT-SIDE-EXIT.
           EXIT.
       P3300-POST-LINK.
           MOVE IB-OCN TO WS-BZ-TXT-05.
           MOVE IB-PERIOD2 TO WS-BZ-TXT-04.
           MOVE IB-CIRCUIT TO WS-BZ-TXT-05.
           MOVE IB-MEDIA TO WS-BZ-TXT-04.
           ADD 1 TO WS-BZ-CNT-04.
       P3300-POST-LINK-EXIT.
           EXIT.
       P3400-WRITE-PAIR.
           MOVE SPACES TO CABS-BZ-OUT-RECORD.
           MOVE IB-CENTRE TO OB-SEGMENT.
           MOVE IB-SEGMENT TO OB-ACCOUNT.
           MOVE IB-TARGET TO OB-GROUP.
           MOVE IB-TARIFF TO OB-REGION.
           MOVE IB-REGION TO OB-GROUP2.
           MOVE IB-REGION TO OB-TARIFF.
           MOVE IB-PERIOD TO OB-JURIS.
           MOVE IB-SEQ TO OB-CYCLE.
           WRITE CABS-BZ-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P3400-WRITE-PAIR-EXIT.
           EXIT.
       P3500-RELEASE-ORPHAN.
           ADD IB-CENTRE TO WS-BZ-QTY-04.
           COMPUTE WS-BZ-AMT-03 = WS-BZ-QTY-04 * WS-BZ-QTY-04.
           ADD WS-BZ-AMT-03 TO WS-BZ-AMT-03.
       P3500-RELEASE-ORPHAN-EXIT.
           EXIT.
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT
      * PRECEDES THIS PROGRAM IN THE JOB.
       P3600-WRITE-REFERENCE.
           MOVE SPACES TO WS-BZ-TXT-03.
           STRING IB-PERIOD2 DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IB-SEGMENT DELIMITED BY SIZE
               INTO WS-BZ-TXT-03.
       P3600-WRITE-REFERENCE-EXIT.
           EXIT.
      * S400-SECONDARY-PROCESSING SECTION
       S400-SECONDARY-PROCESSING SECTION.
       P4000-SECONDARY.
           PERFORM P4100-NORMALISE-TARIFF THRU
               P4100-NORMALISE-TARIFF-EXIT.
           PERFORM P4200-COMPARE-CYCLE THRU P4200-COMPARE-CYCLE-EXIT.
           PERFORM P4300-ADJUST-ACCOUNT THRU P4300-ADJUST-ACCOUNT-EXIT.
           PERFORM P4400-RECONCILE-TYPE THRU P4400-RECONCILE-TYPE-EXIT.
           PERFORM P4500-SUMMARISE-TYPE THRU P4500-SUMMARISE-TYPE-EXIT.
           PERFORM P4600-TRACE-ORPHAN THRU P4600-TRACE-ORPHAN-EXIT.
           PERFORM P4700-RECONCILE-CENTRE THRU
               P4700-RECONCILE-CENTRE-EXIT.
           PERFORM P4800-SUMMARISE-CYCLE THRU
               P4800-SUMMARISE-CYCLE-EXIT.
           PERFORM P4900-ADJUST-GROUP THRU P4900-ADJUST-GROUP-EXIT.
           PERFORM P41000-ADJUST-SEQ THRU P41000-ADJUST-SEQ-EXIT.
           PERFORM P41100-COMPARE-CODE THRU P41100-COMPARE-CODE-EXIT.
           PERFORM P41200-NORMALISE-TYPE THRU
               P41200-NORMALISE-TYPE-EXIT.
           PERFORM P41300-TRACE-STATUS THRU P41300-TRACE-STATUS-EXIT.
           PERFORM P41400-RECONCILE-STATE THRU
               P41400-RECONCILE-STATE-EXIT.
           PERFORM P41500-COMPARE-GROUP THRU P41500-COMPARE-GROUP-EXIT.
           PERFORM P41600-TRACE-CODE THRU P41600-TRACE-CODE-EXIT.
           PERFORM P41700-REPORT-INVOICE THRU
               P41700-REPORT-INVOICE-EXIT.
           PERFORM P41800-RECONCILE-OCN THRU P41800-RECONCILE-OCN-EXIT.
           PERFORM P41900-TRACE-GROUP THRU P41900-TRACE-GROUP-EXIT.
           PERFORM P42000-ADJUST-CODE THRU P42000-ADJUST-CODE-EXIT.
           PERFORM P42100-TRACE-CARRIER THRU P42100-TRACE-CARRIER-EXIT.
           PERFORM P42200-COMPARE-ORPHAN THRU
               P42200-COMPARE-ORPHAN-EXIT.
           PERFORM P42300-AUDIT-REFERENCE THRU
               P42300-AUDIT-REFERENCE-EXIT.
           PERFORM P42400-NORMALISE-SEGMENT THRU
               P42400-NORMALISE-SEGMENT-EXIT.
           PERFORM P42500-TRACE-SEQ THRU P42500-TRACE-SEQ-EXIT.
           PERFORM P42600-TRACE-CLASS THRU P42600-TRACE-CLASS-EXIT.
           PERFORM P42700-AUDIT-ORPHAN THRU P42700-AUDIT-ORPHAN-EXIT.
           PERFORM P42800-ADJUST-OCN THRU P42800-ADJUST-OCN-EXIT.
           PERFORM P42900-TRACE-TYPE THRU P42900-TRACE-TYPE-EXIT.
           PERFORM P43000-TRACE-BAN THRU P43000-TRACE-BAN-EXIT.
           PERFORM P43100-NORMALISE-PAIR THRU
               P43100-NORMALISE-PAIR-EXIT.
           PERFORM P43200-AUDIT-TARGET THRU P43200-AUDIT-TARGET-EXIT.
           PERFORM P43300-ADJUST-SEQ THRU P43300-ADJUST-SEQ-EXIT.
           PERFORM P43400-AUDIT-SOURCE THRU P43400-AUDIT-SOURCE-EXIT.
           PERFORM P43500-RECONCILE-MATCH THRU
               P43500-RECONCILE-MATCH-EXIT.
       P4000-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P4100-NORMALISE-TARIFF.
           IF WS-BZ-AMT-02 < 16
               MOVE 16 TO WS-BZ-AMT-02
               ADD 1 TO WS-BZ-CNT-09.
           IF WS-BZ-AMT-02 > 19033
               MOVE 19033 TO WS-BZ-AMT-02
               ADD 1 TO WS-BZ-CNT-12.
       P4100-NORMALISE-TARIFF-EXIT.
           EXIT.
       P4200-COMPARE-CYCLE.
           MOVE 'N' TO WS-BZ-SW-06.
           IF WS-BZ-TXT-07 NOT = WS-BZ-TXT-07
               MOVE 'Y' TO WS-BZ-SW-06
               MOVE WS-BZ-TXT-07 TO WS-BZ-TXT-07
               ADD 1 TO WS-BZ-CNT-01.
       P4200-COMPARE-CYCLE-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P4300-ADJUST-ACCOUNT.
           IF WS-BZ-AMT-02 < 38
               MOVE 38 TO WS-BZ-AMT-02
               ADD 1 TO WS-BZ-CNT-04.
           IF WS-BZ-AMT-02 > 89602
               MOVE 89602 TO WS-BZ-AMT-02
               ADD 1 TO WS-BZ-CNT-06.
       P4300-ADJUST-ACCOUNT-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P4400-RECONCILE-TYPE.
           MOVE WS-BZ-AMT-02 TO WS-BZ-AMT-01.
           IF WS-BZ-AMT-01 < 0
               COMPUTE WS-BZ-AMT-01 = 0 - WS-BZ-AMT-02.
       P4400-RECONCILE-TYPE-EXIT.
           EXIT.
       P4500-SUMMARISE-TYPE.
           IF IB-STATE = 'S'
               ADD 1 TO WS-BZ-CNT-12
           ELSE
               IF IB-STATE = 'E'
                   ADD 1 TO WS-BZ-CNT-02
               ELSE
                   IF IB-STATE = 'E'
                       ADD 1 TO WS-BZ-CNT-07
                   ELSE
                       ADD 1 TO WS-BZ-CNT-02.
       P4500-SUMMARISE-TYPE-EXIT.
           EXIT.
       P4600-TRACE-ORPHAN.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-BZ-TXT-04 TO PC-COL-001-020.
           MOVE WS-BZ-TXT-03 TO PC-COL-021-060.
           MOVE WS-BZ-AMT-04 TO WS-BZ-AMT-EDIT.
           MOVE WS-BZ-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P4600-TRACE-ORPHAN-EXIT.
           EXIT.
       P4700-RECONCILE-CENTRE.
           MOVE IB-TARIFF2 TO WS-BZ-TXT-05.
           MOVE IB-PERIOD TO WS-BZ-TXT-03.
           ADD 1 TO WS-BZ-CNT-06.
       P4700-RECONCILE-CENTRE-EXIT.
           EXIT.
       P4800-SUMMARISE-CYCLE.
           UNSTRING WS-BZ-TXT-06 DELIMITED BY '/'
               INTO WS-BZ-TXT-01
               WS-BZ-TXT-02
               TALLYING IN WS-BZ-CNT-08.
       P4800-SUMMARISE-CYCLE-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P4900-ADJUST-GROUP.
           MOVE SPACES TO CABS-BZ-OUT-RECORD.
           MOVE IB-PERIOD2 TO OB-SEGMENT.
           MOVE IB-LEVEL TO OB-ACCOUNT.
           MOVE IB-TARIFF2 TO OB-GROUP.
           MOVE IB-JURIS TO OB-REGION.
           WRITE CABS-BZ-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P4900-ADJUST-GROUP-EXIT.
           EXIT.
       P41000-ADJUST-SEQ.
           CALL 'CABHASH' USING IB-LEVEL WS-ACC-OCN-HASH.
           ADD WS-BZ-CNT-06 TO WS-ACC-SEQ-HASH.
       P41000-ADJUST-SEQ-EXIT.
           EXIT.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P41100-COMPARE-CODE.
           MOVE 0 TO WS-BZ-QTY-05.
           MOVE 0 TO WS-BZ-QTY-07.
           MOVE 0 TO WS-BZ-QTY-01.
           MOVE 0 TO WS-BZ-AMT-05.
       P41100-COMPARE-CODE-EXIT.
           EXIT.
       P41200-NORMALISE-TYPE.
           IF WS-BZ-AMT-04 NOT = 0
               COMPUTE WS-BZ-QTY-02 = WS-BZ-AMT-01 * 100 / WS-BZ-AMT-04
           ELSE
               MOVE 0 TO WS-BZ-QTY-02.
       P41200-NORMALISE-TYPE-EXIT.
           EXIT.
       P41300-TRACE-STATUS.
           MOVE 'Y' TO WS-BZ-SW-06.
           IF IB-TARGET < 39
               MOVE 'N' TO WS-BZ-SW-06
               ADD 1 TO WS-BZ-CNT-12.
           IF IB-TARGET > 57
               MOVE 'N' TO WS-BZ-SW-06
               ADD 1 TO WS-BZ-CNT-06.
       P41300-TRACE-STATUS-EXIT.
           EXIT.
       P41400-RECONCILE-STATE.
           MOVE SPACES TO WS-BZ-TXT-03.
           STRING IB-TARIFF DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IB-PERIOD2 DELIMITED BY SIZE
               INTO WS-BZ-TXT-03.
       P41400-RECONCILE-STATE-EXIT.
           EXIT.
       P41500-COMPARE-GROUP.
           CALL 'CABFMTR' USING WS-BZ-TXT-05 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-BZ-CNT-02.
       P41500-COMPARE-GROUP-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P41600-TRACE-CODE.
           MOVE 0 TO WS-BZ-CNT-10.
           INSPECT WS-BZ-TXT-07 TALLYING WS-BZ-CNT-10
               FOR ALL SPACES.
           INSPECT WS-BZ-TXT-07 REPLACING ALL LOW-VALUES BY SPACES.
       P41600-TRACE-CODE-EXIT.
           EXIT.
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT
      * PRECEDES THIS PROGRAM IN THE JOB.
       P41700-REPORT-INVOICE.
           ADD IB-CENTRE TO WS-BZ-QTY-06.
           COMPUTE WS-BZ-AMT-03 = WS-BZ-QTY-06 * WS-BZ-QTY-04.
           ADD WS-BZ-AMT-03 TO WS-BZ-AMT-05.
       P41700-REPORT-INVOICE-EXIT.
           EXIT.
       P41800-RECONCILE-OCN.
           IF WS-BZ-AMT-02 < 14
               MOVE 14 TO WS-BZ-AMT-02
               ADD 1 TO WS-BZ-CNT-05.
           IF WS-BZ-AMT-02 > 47616
               MOVE 47616 TO WS-BZ-AMT-02
               ADD 1 TO WS-BZ-CNT-10.
       P41800-RECONCILE-OCN-EXIT.
           EXIT.
       P41900-TRACE-GROUP.
           MOVE 'N' TO WS-BZ-SW-05.
           IF WS-BZ-TXT-06 NOT = WS-BZ-TXT-06
               MOVE 'Y' TO WS-BZ-SW-05
               MOVE WS-BZ-TXT-06 TO WS-BZ-TXT-06
               ADD 1 TO WS-BZ-CNT-10.
       P41900-TRACE-GROUP-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P42000-ADJUST-CODE.
           IF WS-BZ-AMT-04 < 30
               MOVE 30 TO WS-BZ-AMT-04
               ADD 1 TO WS-BZ-CNT-07.
           IF WS-BZ-AMT-04 > 12533
               MOVE 12533 TO WS-BZ-AMT-04
               ADD 1 TO WS-BZ-CNT-06.
       P42000-ADJUST-CODE-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P42100-TRACE-CARRIER.
           MOVE WS-BZ-AMT-04 TO WS-BZ-AMT-05.
           IF WS-BZ-AMT-05 < 0
               COMPUTE WS-BZ-AMT-05 = 0 - WS-BZ-AMT-04.
       P42100-TRACE-CARRIER-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P42200-COMPARE-ORPHAN.
           IF IB-TARGET = 'D'
               ADD 1 TO WS-BZ-CNT-03
           ELSE
               IF IB-TARGET = 'X'
                   ADD 1 TO WS-BZ-CNT-03
               ELSE
                   IF IB-TARGET = 'X'
                       ADD 1 TO WS-BZ-CNT-08
                   ELSE
                       ADD 1 TO WS-BZ-CNT-03.
       P42200-COMPARE-ORPHAN-EXIT.
           EXIT.
       P42300-AUDIT-REFERENCE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-BZ-TXT-02 TO PC-COL-001-020.
           MOVE WS-BZ-TXT-04 TO PC-COL-021-060.
           MOVE WS-BZ-AMT-02 TO WS-BZ-AMT-EDIT.
           MOVE WS-BZ-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P42300-AUDIT-REFERENCE-EXIT.
           EXIT.
       P42400-NORMALISE-SEGMENT.
           MOVE IB-SEQ TO WS-BZ-TXT-04.
           MOVE IB-SEQ2 TO WS-BZ-TXT-07.
           ADD 1 TO WS-BZ-CNT-07.
       P42400-NORMALISE-SEGMENT-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P42500-TRACE-SEQ.
           UNSTRING WS-BZ-TXT-05 DELIMITED BY '/'
               INTO WS-BZ-TXT-04
               WS-BZ-TXT-05
               TALLYING IN WS-BZ-CNT-04.
       P42500-TRACE-SEQ-EXIT.
           EXIT.
       P42600-TRACE-CLASS.
           MOVE SPACES TO CABS-BZ-OUT-RECORD.
           MOVE IB-LEVEL TO OB-SEGMENT.
           MOVE IB-CENTRE TO OB-ACCOUNT.
           MOVE IB-CYCLE TO OB-GROUP.
           MOVE IB-CENTRE TO OB-REGION.
           WRITE CABS-BZ-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P42600-TRACE-CLASS-EXIT.
           EXIT.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P42700-AUDIT-ORPHAN.
           CALL 'CABHASH' USING IB-MEDIA WS-ACC-OCN-HASH.
           ADD WS-BZ-CNT-04 TO WS-ACC-SEQ-HASH.
       P42700-AUDIT-ORPHAN-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P42800-ADJUST-OCN.
           MOVE 0 TO WS-BZ-QTY-01.
           MOVE 0 TO WS-BZ-QTY-03.
           MOVE 0 TO WS-BZ-QTY-05.
           MOVE 0 TO WS-BZ-AMT-03.
           MOVE 0 TO WS-BZ-AMT-04.
       P42800-ADJUST-OCN-EXIT.
           EXIT.
       P42900-TRACE-TYPE.
           IF WS-BZ-AMT-05 NOT = 0
               COMPUTE WS-BZ-QTY-08 = WS-BZ-AMT-02 * 100 / WS-BZ-AMT-05
           ELSE
               MOVE 0 TO WS-BZ-QTY-08.
       P42900-TRACE-TYPE-EXIT.
           EXIT.
       P43000-TRACE-BAN.
           MOVE 'Y' TO WS-BZ-SW-04.
           IF IB-OCN < 22
               MOVE 'N' TO WS-BZ-SW-04
               ADD 1 TO WS-BZ-CNT-08.
           IF IB-OCN > 8239
               MOVE 'N' TO WS-BZ-SW-04
               ADD 1 TO WS-BZ-CNT-11.
       P43000-TRACE-BAN-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P43100-NORMALISE-PAIR.
           MOVE SPACES TO WS-BZ-TXT-07.
           STRING IB-CYCLE DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IB-SEQ2 DELIMITED BY SIZE
               INTO WS-BZ-TXT-07.
       P43100-NORMALISE-PAIR-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P43200-AUDIT-TARGET.
           CALL 'CABEDITF' USING WS-BZ-TXT-05 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-BZ-CNT-08.
       P43200-AUDIT-TARGET-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P43300-ADJUST-SEQ.
           MOVE 0 TO WS-BZ-CNT-08.
           INSPECT WS-BZ-TXT-01 TALLYING WS-BZ-CNT-08
               FOR ALL SPACES.
           INSPECT WS-BZ-TXT-01 REPLACING ALL LOW-VALUES BY SPACES.
       P43300-ADJUST-SEQ-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P43400-AUDIT-SOURCE.
           ADD IB-MEDIA TO WS-BZ-QTY-01.
           COMPUTE WS-BZ-AMT-04 = WS-BZ-QTY-01 * WS-BZ-QTY-03.
           ADD WS-BZ-AMT-04 TO WS-BZ-AMT-02.
       P43400-AUDIT-SOURCE-EXIT.
           EXIT.
       P43500-RECONCILE-MATCH.
           IF WS-BZ-AMT-03 < 26
               MOVE 26 TO WS-BZ-AMT-03
               ADD 1 TO WS-BZ-CNT-04.
           IF WS-BZ-AMT-03 > 50763
               MOVE 50763 TO WS-BZ-AMT-03
               ADD 1 TO WS-BZ-CNT-05.
       P43500-RECONCILE-MATCH-EXIT.
           EXIT.
           MOVE 0 TO WS-BZ-QTY-06.
           PERFORM P380-WALK-MATCH THRU P380-WALK-MATCH-EXIT
               VARYING WS-BZ-SUB-02 FROM 1 BY 1
               UNTIL WS-BZ-SUB-02 > WS-BZ-TAB-CNT.
       P380-WALK-MATCH.
           SET WS-BZ-IX TO WS-BZ-SUB-01.
           IF WS-BZ-TB-KEY (WS-BZ-IX) NOT = SPACES
               ADD WS-BZ-TB-VAL (WS-BZ-IX) TO WS-BZ-QTY-05.
       P380-WALK-MATCH-EXIT.
           EXIT.
      * S800-CONTROL SECTION - THE MANDATORY CABS CONTROL BOUNDARY.
       S800-CONTROL SECTION.
       P8000-CONTROL.
           PERFORM P4000-SECONDARY THRU P4000-EXIT.
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
           MOVE 'DETAIL OUT' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-BZ-CNT-EDIT.
           MOVE WS-BZ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL SUSPENDED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-BZ-CNT-EDIT.
           MOVE WS-BZ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL IN' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO WS-BZ-CNT-EDIT.
           MOVE WS-BZ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL CARRIED FWD' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-BZ-CNT-EDIT.
           MOVE WS-BZ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL SUMMARISED' TO PC-COL-001-020.
           MOVE WS-SUMM-CNT TO WS-BZ-CNT-EDIT.
           MOVE WS-BZ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-BZ-CNT-01 TO WS-BZ-CNT-EDIT.
           MOVE WS-BZ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 02' TO PC-COL-001-020.
           MOVE WS-BZ-CNT-02 TO WS-BZ-CNT-EDIT.
           MOVE WS-BZ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 03' TO PC-COL-001-020.
           MOVE WS-BZ-CNT-03 TO WS-BZ-CNT-EDIT.
           MOVE WS-BZ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 04' TO PC-COL-001-020.
           MOVE WS-BZ-CNT-04 TO WS-BZ-CNT-EDIT.
           MOVE WS-BZ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE 7 TO CT-STEP-SEQ.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE WS-BZ-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE SPACES TO CT-FILLER.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-RESTART-KEY.
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
           CLOSE KEYIN.
           CLOSE LFTIN.
           CLOSE MTCOUT.
           CLOSE LNKOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABUXR22 - RUN COMPLETE'.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  BZ-CNT-04 = ' WS-BZ-CNT-04.
           DISPLAY '  BZ-CNT-12 = ' WS-BZ-CNT-12.
           DISPLAY '  BZ-CNT-08 = ' WS-BZ-CNT-08.
       P9000-EXIT.
           EXIT.
