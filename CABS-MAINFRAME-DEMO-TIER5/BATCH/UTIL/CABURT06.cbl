      *****************************************************************
      * CABURT06 - RATE OVERRIDE TABLE LOAD                           *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               OVRIN   TELCABS.CABS.OVRIN          (LOCAL)     *
      *               CTLIN   TELCABS.CABS.CTLIN          (LOCAL)     *
      *               MNTIN   TELCABS.CABS.MNTIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               ELMOUT  TELCABS.CABS.ELMOUT         (LOCAL)     *
      *               TAROUT  TELCABS.CABS.TAROUT         (LOCAL)     *
      *               RATOUT  TELCABS.CABS.RATOUT         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * REVISION HISTORY                                              *
      *   V1.00  1989-01-02  M.DELACROIX  INITIAL RELEASE             *
      *   V1.01  1991-06-10  C.ADEYEMI    CARRIER TYPE BROUGHT ONTO   *
      *                      THE EXTRACT                              *
      *   V1.05  1993-05-11  P.NAIR       PRINT LINE WIDENED TO 133   *
      *   V1.08  1997-10-23  L.FERREIRA   BLOCK SIZE SET TO ZERO -    *
      *                      SYSTEM DETERMINED                        *
      *   V1.12  2009-05-11  S.MARCHETTI  RECOMPILE ONLY - COPYBOOK   *
      *                      CHANGE UPSTREAM                          *
      *   V1.14  2013-11-22  T.YAMASHITA  SUSPENSE WRITE ADDED -      *
      *                      RECORDS WERE BEING DROPPED               *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABURT06.
       AUTHOR. TELCABS APPLICATIONS - RATE TABLE MAINTENANCE.
      *****************************************************************
      * RATE OVERRIDE TABLE LOAD. THE STEP IS DRIVEN ENTIRELY FROM THE*
      * SYSIN PARM CARD AND THE DD ALLOCATIONS IN THE JOB.            *
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
           SELECT OVRIN ASSIGN TO UT-S-OVRIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT CTLIN ASSIGN TO UT-S-CTLIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT MNTIN ASSIGN TO UT-S-MNTIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT ELMOUT ASSIGN TO UT-S-ELMOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT TAROUT ASSIGN TO UT-S-TAROUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT RATOUT ASSIGN TO UT-S-RATOUT
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
      * OVRIN - PERMANENT DATASET HELD ON DASD.
       FD  OVRIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 140 CHARACTERS.
       01  CABS-DE-IN-RECORD.
           05  ID-CARRIER                  PIC X(08).
           05  ID-CARRIER2                 PIC X(04).
           05  ID-CIRCUIT                  PIC X(08).
           05  ID-TARIFF                   PIC S9(13)V9(02) COMP-3.
           05  ID-TARIFF2                  PIC S9(05) COMP-3.
           05  ID-BAND                     PIC 9(06).
           05  ID-INVOICE                  PIC 9(04).
           05  ID-CARRIER3                 PIC 9(06).
           05  ID-BAN                      PIC X(06).
           05  ID-OCN                      PIC X(04).
           05  ID-BAND2                    PIC X(20).
           05  ID-TARIFF3                  PIC 9(05).
           05  ID-SEQ                      PIC S9(07)V9(02) COMP-3.
           05  ID-CIRCUIT2                 PIC X(04).
           05  ID-GROUP                    PIC X(06).
           05  ID-MEDIA                    PIC X(06).
           05  ID-SOURCE                   PIC X(02).
           05  ID-CARRIER4                 PIC X(20).
           05  ID-JURIS                    PIC S9(13)V9(02) COMP-3.
           05  ID-CIRCUIT3                 PIC 9(05).
           05  DE-FILL-01                  PIC X(2).
      * CTLIN - CATALOGUED GENERATION DATA GROUP.
       FD  CTLIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 140 CHARACTERS.
       01  CABS-DE-ALT1-RECORD.
           05  A1-MEDIA                    PIC X(13).
           05  A1-SEQ                      PIC S9(13)V9(05) COMP-3.
           05  A1-CIRCUIT                  PIC S9(09)V9(05) COMP-3.
           05  A1-TARGET                   PIC S9(07)V9(02) COMP-3.
           05  A1-ELEM                     PIC S9(15) COMP-3.
           05  A1-SEQ2                     PIC S9(07)V9(02) COMP-3.
           05  A1-INVOICE                  PIC 9(09).
           05  DE-FILL-02                  PIC X(82).
      * MNTIN - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  MNTIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 140 CHARACTERS.
       01  CABS-DE-ALT2-RECORD.
           05  A2-CIRCUIT                  PIC X(06).
           05  A2-CYCLE                    PIC S9(13)V9(02) COMP-3.
           05  A2-MEDIA                    PIC X(16).
           05  A2-CENTRE                   PIC 9(04).
           05  A2-SEGMENT                  PIC X(02).
           05  A2-SEGMENT2                 PIC X(16).
           05  DE-FILL-03                  PIC X(88).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-DE-VIEW1 REDEFINES CABS-DE-IN-RECORD.
           05  R0D-BAN                     PIC S9(09)V9(02) COMP-3.
           05  R0D-INVOICE                 PIC S9(09) COMP-3.
           05  R0D-TARIFF                  PIC S9(09)V9(02) COMP-3.
           05  R0D-BAND                    PIC 9(02).
           05  R0D-CYCLE                   PIC S9(13)V9(05) COMP-3.
           05  R0D-BAND2                   PIC S9(05) COMP-3.
           05  R0D-SEQ                     PIC S9(09)V9(05) COMP-3.
           05  R0D-REST                    PIC X(100).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-DE-VIEW2 REDEFINES CABS-DE-IN-RECORD.
           05  R1D-REGION                  PIC S9(11) COMP-3.
           05  R1D-GROUP                   PIC X(20).
           05  R1D-CYCLE                   PIC 9(06).
           05  R1D-ACCOUNT                 PIC 9(04).
           05  R1D-REST                    PIC X(104).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-DE-VIEW3 REDEFINES CABS-DE-IN-RECORD.
           05  R2D-GROUP                   PIC X(03).
           05  R2D-INVOICE                 PIC X(10).
           05  R2D-BAN                     PIC S9(13) COMP-3.
           05  R2D-CIRCUIT                 PIC X(06).
           05  R2D-CYCLE                   PIC X(03).
           05  R2D-ELEM                    PIC X(02).
           05  R2D-CLASS                   PIC S9(07)V9(02) COMP-3.
           05  R2D-GROUP2                  PIC X(02).
           05  R2D-CIRCUIT2                PIC S9(11) COMP-3.
           05  R2D-REST                    PIC X(96).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-DE-VIEW4 REDEFINES CABS-DE-IN-RECORD.
           05  R3D-MEDIA                   PIC S9(09)V9(02) COMP-3.
           05  R3D-CODE                    PIC 9(03).
           05  R3D-SEQ                     PIC S9(13) COMP-3.
           05  R3D-JURIS                   PIC X(06).
           05  R3D-ACCOUNT                 PIC 9(05).
           05  R3D-REST                    PIC X(113).
      * ELMOUT - WORK FILE, DELETED AT STEP END.
       FD  ELMOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 170 CHARACTERS.
       01  CABS-DE-OUT-RECORD.
           05  OD-TARGET                   PIC S9(15) COMP-3.
           05  OD-TYPE                     PIC X(20).
           05  OD-MEDIA                    PIC X(06).
           05  OD-GROUP                    PIC X(08).
           05  OD-TYPE2                    PIC S9(07) COMP-3.
           05  OD-BAND                     PIC X(04).
           05  OD-TARIFF                   PIC X(02).
           05  OD-CIRCUIT                  PIC 9(04).
           05  OD-LEVEL                    PIC S9(11)V9(05) COMP-3.
           05  OD-SOURCE                   PIC 9(07).
           05  OD-REGION                   PIC 9(03).
           05  OD-CYCLE                    PIC X(10).
           05  OD-TARGET2                  PIC X(02).
           05  OD-TYPE3                    PIC S9(07)V9(02) COMP-3.
           05  OD-STATE                    PIC X(06).
           05  OD-SEGMENT                  PIC X(06).
           05  OD-ACCOUNT                  PIC 9(06).
           05  OD-MEDIA2                   PIC S9(13)V9(05) COMP-3.
           05  OD-BAN                      PIC 9(06).
           05  OD-STATUS                   PIC X(16).
           05  OD-TARGET3                  PIC 9(04).
           05  OD-STATE2                   PIC X(08).
           05  OD-CARRIER                  PIC S9(05) COMP-3.
           05  OD-TARGET4                  PIC S9(05) COMP-3.
           05  OD-BAND2                    PIC X(03).
           05  DE-FILL-04                  PIC X(7).
      * TAROUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  TAROUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 170 CHARACTERS.
       01  CABS-DE-OUT1-RECORD         PIC X(170).
      * RATOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  RATOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 170 CHARACTERS.
       01  CABS-DE-OUT2-RECORD         PIC X(170).
      * SUSOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
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
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABURT06'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V2.11'.
           05  WS-TABLE-LIMIT              PIC S9(04) COMP-3
                                         VALUE 750.
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
           05  WS-DE-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DE-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DE-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DE-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DE-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DE-CNT-06                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DE-CNT-07                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DE-CNT-08                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DE-CNT-09                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DE-CNT-10                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DE-CNT-11                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-DE-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DE-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DE-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DE-QTY-04                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DE-QTY-05                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DE-QTY-06                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DE-QTY-07                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DE-QTY-08                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-DE-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DE-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DE-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DE-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DE-AMT-05                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DE-AMT-06                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DE-AMT-07                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-DE-TXT-01                PIC X(10) VALUE SPACES.
           05  WS-DE-TXT-02                PIC X(20) VALUE SPACES.
           05  WS-DE-TXT-03                PIC X(26) VALUE SPACES.
           05  WS-DE-TXT-04                PIC X(10) VALUE SPACES.
           05  WS-DE-TXT-05                PIC X(08) VALUE SPACES.
           05  WS-DE-TXT-06                PIC X(20) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-DE-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-DE-ON-01                 VALUE 'Y'.
               88  WS-DE-OFF-01                VALUE 'N'.
           05  WS-DE-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-DE-ON-02                 VALUE 'Y'.
               88  WS-DE-OFF-02                VALUE 'N'.
           05  WS-DE-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-DE-ON-03                 VALUE 'Y'.
               88  WS-DE-OFF-03                VALUE 'N'.
           05  WS-DE-SW-04                 PIC X(01) VALUE 'N'.
               88  WS-DE-ON-04                 VALUE 'Y'.
               88  WS-DE-OFF-04                VALUE 'N'.
           05  WS-DE-SW-05                 PIC X(01) VALUE 'N'.
               88  WS-DE-ON-05                 VALUE 'Y'.
               88  WS-DE-OFF-05                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-DE-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DE-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DE-SUB-03                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DE-SUB-04                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. THE LIMIT WAS RAISED WHEN THE SOUTHEAST CENTRES
      * WERE MERGED AND HAS NOT BEEN REVIEWED SINCE.
       01  WS-DE-TABLE.
           05  WS-DE-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-DE-TB-ENTRY OCCURS 750 TIMES
                                       INDEXED BY WS-DE-IX.
               10  WS-DE-TB-KEY                PIC X(08).
               10  WS-DE-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-DE-TB-TXT                PIC X(30).
               10  WS-DE-TB-EFF                PIC 9(05).
               10  WS-DE-TB-EXP                PIC 9(05).
       01  WS-DE-WORK-GROUP-1.
           05  WS-DE-G1-TYPE               PIC X(10).
           05  WS-DE-G1-CIRCUIT            PIC S9(11)V9(02) COMP-3.
           05  WS-DE-G1-LEVEL              PIC S9(09) COMP-3.
       01  WS-DE-WORK-GROUP-2.
           05  WS-DE-G2-CARRIER            PIC S9(11)V9(02) COMP-3.
           05  WS-DE-G2-CIRCUIT            PIC S9(09) COMP-3.
           05  WS-DE-G2-CYCLE              PIC 9(05).
           05  WS-DE-G2-INVOICE            PIC 9(07).
           05  WS-DE-G2-SEQ                PIC X(10).
           05  WS-DE-G2-ELEM               PIC X(10).
       01  WS-DE-WORK-GROUP-3.
           05  WS-DE-G3-ACCOUNT            PIC X(20).
           05  WS-DE-G3-PERIOD             PIC X(10).
           05  WS-DE-G3-SEGMENT            PIC 9(07).
           05  WS-DE-G3-SEGMENT            PIC X(10).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 55.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABURT06 - RATE OVERRIDE TABLE LOAD'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-DE-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-DE-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9944.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-DE-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-DE-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT OVRIN.
           IF WS-FS-INPUT NOT = '00'
               DISPLAY 'OVRIN FILE STATUS = ' WS-FS-INPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OVRIN COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN INPUT CTLIN.
           IF WS-FS-INPUT NOT = '00'
               DISPLAY 'CTLIN FILE STATUS = ' WS-FS-INPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CTLIN COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN INPUT MNTIN.
           IF WS-FS-INPUT NOT = '00'
               DISPLAY 'MNTIN FILE STATUS = ' WS-FS-INPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'MNTIN COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT ELMOUT.
           IF WS-FS-OUTPUT NOT = '00'
               DISPLAY 'ELMOUT FILE STATUS = ' WS-FS-OUTPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'ELMOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT TAROUT.
           IF WS-FS-OUTPUT NOT = '00'
               DISPLAY 'TAROUT FILE STATUS = ' WS-FS-OUTPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'TAROUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RATOUT.
           IF WS-FS-OUTPUT NOT = '00'
               DISPLAY 'RATOUT FILE STATUS = ' WS-FS-OUTPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RATOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SUSOUT.
           IF WS-FS-SUSPENSE NOT = '00'
               DISPLAY 'SUSOUT FILE STATUS = ' WS-FS-SUSPENSE
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'SUSOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               DISPLAY 'CTLOUT FILE STATUS = ' WS-FS-CONTROL
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CTLOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RPTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               DISPLAY 'RPTOUT FILE STATUS = ' WS-FS-OUTPUT
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
           MOVE PC1-CYCLE-YYDDD TO WS-DE-CYCLE-YYDDD.
           COMPUTE WS-DE-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-DE-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-DE-CNT-07.
           MOVE 0 TO WS-DE-CNT-02.
           MOVE 0 TO WS-DE-CNT-05.
       P1200-EXIT.
           EXIT.
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT
      * PRECEDES THIS PROGRAM IN THE JOB.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-DE-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-DE-TAB-CNT NOT < 750
               MOVE 'Y' TO WS-DE-SW-01
               ADD 1 TO WS-DE-CNT-02
           ELSE
               ADD 1 TO WS-DE-TAB-CNT
               SET WS-DE-IX TO WS-DE-TAB-CNT
               MOVE ID-GROUP TO WS-DE-TB-KEY (WS-DE-IX)
               MOVE 0 TO WS-DE-TB-VAL (WS-DE-IX)
               MOVE SPACES TO WS-DE-TB-TXT (WS-DE-IX)
               ADD 1 TO WS-DE-CNT-11.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ OVRIN
               AT END MOVE 'Y' TO WS-DE-SW-01.
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
           PERFORM P2200-CONVERT-BAND THRU P2200-CONVERT-BAND-EXIT.
           IF WS-DE-ON-02
               PERFORM P2300-MATCH-BAND THRU P2300-MATCH-BAND-EXIT.
           PERFORM P2400-CONVERT-BAND THRU P2400-CONVERT-BAND-EXIT.
           PERFORM P2500-DERIVE-WINDOW THRU P2500-DERIVE-WINDOW-EXIT.
           PERFORM P2600-SELECT-ROW THRU P2600-SELECT-ROW-EXIT.
           PERFORM P2700-SELECT-DESCRIPTION THRU
               P2700-SELECT-DESCRIPTION-EXIT.
           PERFORM P2800-MATCH-ELEMENT THRU P2800-MATCH-ELEMENT-EXIT.
           IF WS-DE-ON-01
               PERFORM P2900-EDIT-KEY THRU P2900-EDIT-KEY-EXIT.
           PERFORM P21000-BUILD-ELEMENT THRU P21000-BUILD-ELEMENT-EXIT.
           IF WS-DE-ON-01
               PERFORM P21100-RESOLVE-BAND THRU
                   P21100-RESOLVE-BAND-EXIT.
           PERFORM P21200-DERIVE-OVERRIDE THRU
               P21200-DERIVE-OVERRIDE-EXIT.
           PERFORM P21300-DERIVE-ELEMENT THRU
               P21300-DERIVE-ELEMENT-EXIT.
           IF WS-DE-ON-05
               PERFORM P21400-BUILD-TARIFF THRU
                   P21400-BUILD-TARIFF-EXIT.
           PERFORM P21500-EDIT-ELEMENT THRU P21500-EDIT-ELEMENT-EXIT.
           PERFORM P21600-DERIVE-ELEMENT THRU
               P21600-DERIVE-ELEMENT-EXIT.
           PERFORM P21700-APPLY-DESCRIPTION THRU
               P21700-APPLY-DESCRIPTION-EXIT.
           IF WS-DE-ON-02
               PERFORM P21800-MATCH-KEY THRU P21800-MATCH-KEY-EXIT.
           PERFORM P21900-RESOLVE-OVERRIDE THRU
               P21900-RESOLVE-OVERRIDE-EXIT.
           PERFORM P22000-EXPAND-DESCRIPTION THRU
               P22000-EXPAND-DESCRIPTION-EXIT.
           PERFORM P22100-EXPAND-KEY THRU P22100-EXPAND-KEY-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ OVRIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P2200-CONVERT-BAND.
           MOVE 'N' TO WS-DE-SW-01.
           IF WS-DE-TXT-06 NOT = WS-DE-TXT-03
               MOVE 'Y' TO WS-DE-SW-01
               MOVE WS-DE-TXT-06 TO WS-DE-TXT-03
               ADD 1 TO WS-DE-CNT-09.
           MOVE 'N' TO WS-DE-SW-05.
           IF WS-DE-TAB-CNT > 0
               PERFORM P260-COMPARE-TARIFF THRU P260-COMPARE-TARIFF-EXIT
               VARYING WS-DE-SUB-04 FROM 1 BY 1
               UNTIL WS-DE-SUB-04 > WS-DE-TAB-CNT
               OR WS-DE-SW-05 = 'Y'.
       P2200-CONVERT-BAND-EXIT.
           EXIT.
       P2300-MATCH-BAND.
           MOVE WS-DE-AMT-01 TO WS-DE-AMT-03.
           IF WS-DE-AMT-03 < 0
               COMPUTE WS-DE-AMT-03 = 0 - WS-DE-AMT-01.
       P2300-MATCH-BAND-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P2400-CONVERT-BAND.
           MOVE 0 TO WS-DE-QTY-06.
           MOVE 0 TO WS-DE-QTY-08.
           MOVE 0 TO WS-DE-AMT-06.
           MOVE 0 TO WS-DE-AMT-01.
       P2400-CONVERT-BAND-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P2500-DERIVE-WINDOW.
           MOVE SPACES TO WS-DE-TXT-03.
           STRING ID-SEQ DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               ID-CIRCUIT3 DELIMITED BY SIZE
               INTO WS-DE-TXT-03.
       P2500-DERIVE-WINDOW-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P2600-SELECT-ROW.
           CALL 'CABEDITF' USING WS-DE-TXT-06 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-DE-CNT-06.
       P2600-SELECT-ROW-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P2700-SELECT-DESCRIPTION.
           UNSTRING WS-DE-TXT-04 DELIMITED BY '/'
               INTO WS-DE-TXT-03
               WS-DE-TXT-05
               TALLYING IN WS-DE-CNT-01.
       P2700-SELECT-DESCRIPTION-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P2800-MATCH-ELEMENT.
           CALL 'CABHASH' USING ID-CARRIER2 WS-ACC-OCN-HASH.
           ADD WS-DE-CNT-11 TO WS-ACC-SEQ-HASH.
       P2800-MATCH-ELEMENT-EXIT.
           EXIT.
       P2900-EDIT-KEY.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-DE-TXT-03 TO PC-COL-001-020.
           MOVE WS-DE-TXT-02 TO PC-COL-021-060.
           MOVE WS-DE-AMT-04 TO WS-DE-AMT-EDIT.
           MOVE WS-DE-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P2900-EDIT-KEY-EXIT.
           EXIT.
      * A FAILURE HERE SUSPENDS THE RECORD, IT DOES NOT STOP THE STEP.
       P21000-BUILD-ELEMENT.
           MOVE 'Y' TO WS-DE-SW-02.
           IF ID-CARRIER3 < 8
               MOVE 'N' TO WS-DE-SW-02
               ADD 1 TO WS-DE-CNT-10.
           IF ID-CARRIER3 > 7661
               MOVE 'N' TO WS-DE-SW-02
               ADD 1 TO WS-DE-CNT-05.
       P21000-BUILD-ELEMENT-EXIT.
           EXIT.
       P21100-RESOLVE-BAND.
           MOVE ID-BAND TO WS-DE-TXT-02.
           MOVE ID-MEDIA TO WS-DE-TXT-04.
           ADD 1 TO WS-DE-CNT-01.
       P21100-RESOLVE-BAND-EXIT.
           EXIT.
       P21200-DERIVE-OVERRIDE.
           MOVE SPACES TO CABS-DE-OUT-RECORD.
           MOVE ID-SOURCE TO OD-TARGET.
           MOVE ID-CARRIER TO OD-TYPE.
           MOVE ID-CIRCUIT3 TO OD-MEDIA.
           MOVE ID-TARIFF TO OD-GROUP.
           MOVE ID-SOURCE TO OD-TYPE2.
           MOVE ID-CIRCUIT TO OD-BAND.
           MOVE ID-OCN TO OD-TARIFF.
           MOVE ID-MEDIA TO OD-CIRCUIT.
           WRITE CABS-DE-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P21200-DERIVE-OVERRIDE-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P21300-DERIVE-ELEMENT.
           MOVE 0 TO WS-DE-CNT-08.
           INSPECT WS-DE-TXT-04 TALLYING WS-DE-CNT-08
               FOR ALL SPACES.
           INSPECT WS-DE-TXT-04 REPLACING ALL LOW-VALUES BY SPACES.
       P21300-DERIVE-ELEMENT-EXIT.
           EXIT.
       P21400-BUILD-TARIFF.
           IF WS-DE-AMT-05 < 8
               MOVE 8 TO WS-DE-AMT-05
               ADD 1 TO WS-DE-CNT-02.
           IF WS-DE-AMT-05 > 14085
               MOVE 14085 TO WS-DE-AMT-05
               ADD 1 TO WS-DE-CNT-08.
       P21400-BUILD-TARIFF-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P21500-EDIT-ELEMENT.
           ADD ID-JURIS TO WS-DE-QTY-08.
           COMPUTE WS-DE-AMT-03 = WS-DE-QTY-08 * WS-DE-QTY-03.
           ADD WS-DE-AMT-03 TO WS-DE-AMT-04.
       P21500-EDIT-ELEMENT-EXIT.
           EXIT.
       P21600-DERIVE-ELEMENT.
           IF WS-DE-AMT-03 NOT = 0
               COMPUTE WS-DE-QTY-08 = WS-DE-AMT-02 * 100 / WS-DE-AMT-03
           ELSE
               MOVE 0 TO WS-DE-QTY-08.
       P21600-DERIVE-ELEMENT-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P21700-APPLY-DESCRIPTION.
           IF ID-CIRCUIT3 = 'D'
               ADD 1 TO WS-DE-CNT-06
           ELSE
               IF ID-CIRCUIT3 = 'C'
                   ADD 1 TO WS-DE-CNT-10
               ELSE
                   IF ID-CIRCUIT3 = 'D'
                       ADD 1 TO WS-DE-CNT-03
                   ELSE
                       ADD 1 TO WS-DE-CNT-07.
       P21700-APPLY-DESCRIPTION-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P21800-MATCH-KEY.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-OCN-UNKNOWN TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-DE-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
       P21800-MATCH-KEY-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P21900-RESOLVE-OVERRIDE.
           MOVE 'N' TO WS-DE-SW-01.
           IF WS-DE-TXT-02 NOT = WS-DE-TXT-04
               MOVE 'Y' TO WS-DE-SW-01
               MOVE WS-DE-TXT-02 TO WS-DE-TXT-04
               ADD 1 TO WS-DE-CNT-04.
       P21900-RESOLVE-OVERRIDE-EXIT.
           EXIT.
       P22000-EXPAND-DESCRIPTION.
           MOVE WS-DE-AMT-06 TO WS-DE-AMT-05.
           IF WS-DE-AMT-05 < 0
               COMPUTE WS-DE-AMT-05 = 0 - WS-DE-AMT-06.
       P22000-EXPAND-DESCRIPTION-EXIT.
           EXIT.
       P22100-EXPAND-KEY.
           MOVE 0 TO WS-DE-QTY-07.
           MOVE 0 TO WS-DE-QTY-06.
           MOVE 0 TO WS-DE-AMT-06.
           MOVE 0 TO WS-DE-AMT-01.
       P22100-EXPAND-KEY-EXIT.
           EXIT.
       P260-COMPARE-TARIFF.
           SET WS-DE-IX TO WS-DE-SUB-01.
           IF WS-DE-TB-KEY (WS-DE-IX) = ID-CIRCUIT
               MOVE 'Y' TO WS-DE-SW-04
               MOVE WS-DE-TB-VAL (WS-DE-IX) TO WS-DE-QTY-03
               MOVE WS-DE-SUB-01 TO WS-DE-SUB-02.
       P260-COMPARE-TARIFF-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P3100-EMIT-KEY.
           MOVE SPACES TO WS-DE-TXT-04.
           STRING ID-CIRCUIT DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               ID-BAND2 DELIMITED BY SIZE
               INTO WS-DE-TXT-04.
       P3100-EMIT-KEY-EXIT.
           EXIT.
       P3200-WRITE-ELEMENT.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-DATE-INVALID TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-DE-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
           MOVE CABS-SUSPENSE-RECORD TO CABS-SUSOUT-RECORD.
           WRITE CABS-SUSOUT-RECORD.
       P3200-WRITE-ELEMENT-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P3300-CLOSE-OFF-OVERRIDE.
           CALL 'CABHASH' USING ID-CIRCUIT WS-ACC-OCN-HASH.
           ADD WS-DE-CNT-08 TO WS-ACC-SEQ-HASH.
       P3300-CLOSE-OFF-OVERRIDE-EXIT.
           EXIT.
       P3400-POST-TARIFF.
           MOVE 0 TO WS-DE-QTY-05.
           MOVE 0 TO WS-DE-QTY-04.
           MOVE 0 TO WS-DE-QTY-06.
           MOVE 0 TO WS-DE-AMT-06.
       P3400-POST-TARIFF-EXIT.
           EXIT.
       P3500-POST-TARIFF.
           MOVE ID-OCN TO WS-DE-TXT-06.
           MOVE ID-SOURCE TO WS-DE-TXT-04.
           MOVE ID-JURIS TO WS-DE-TXT-02.
           MOVE ID-GROUP TO WS-DE-TXT-02.
           ADD 1 TO WS-DE-CNT-08.
       P3500-POST-TARIFF-EXIT.
           EXIT.
       P3600-POST-TARIFF.
           ADD ID-TARIFF TO WS-DE-QTY-08.
           COMPUTE WS-DE-AMT-05 = WS-DE-QTY-08 * WS-DE-QTY-07.
           ADD WS-DE-AMT-05 TO WS-DE-AMT-04.
       P3600-POST-TARIFF-EXIT.
           EXIT.
      * S400-SECONDARY-PROCESSING SECTION
       S400-SECONDARY-PROCESSING SECTION.
       P4000-SECONDARY.
           PERFORM P4100-NORMALISE-TARIFF THRU
               P4100-NORMALISE-TARIFF-EXIT.
           PERFORM P4200-SUMMARISE-TYPE THRU P4200-SUMMARISE-TYPE-EXIT.
           PERFORM P4300-RECONCILE-SOURCE THRU
               P4300-RECONCILE-SOURCE-EXIT.
           PERFORM P4400-ADJUST-CENTRE THRU P4400-ADJUST-CENTRE-EXIT.
           PERFORM P4500-AUDIT-CYCLE THRU P4500-AUDIT-CYCLE-EXIT.
           PERFORM P4600-COMPARE-TARIFF THRU P4600-COMPARE-TARIFF-EXIT.
           PERFORM P4700-TRACE-ELEM THRU P4700-TRACE-ELEM-EXIT.
           PERFORM P4800-RECONCILE-TARGET THRU
               P4800-RECONCILE-TARGET-EXIT.
           PERFORM P4900-SUMMARISE-CODE THRU P4900-SUMMARISE-CODE-EXIT.
           PERFORM P41000-COMPARE-WINDOW THRU
               P41000-COMPARE-WINDOW-EXIT.
           PERFORM P41100-NORMALISE-CIRCUIT THRU
               P41100-NORMALISE-CIRCUIT-EXIT.
           PERFORM P41200-RECONCILE-BAND THRU
               P41200-RECONCILE-BAND-EXIT.
           PERFORM P41300-COMPARE-ACCOUNT THRU
               P41300-COMPARE-ACCOUNT-EXIT.
           PERFORM P41400-NORMALISE-MEDIA THRU
               P41400-NORMALISE-MEDIA-EXIT.
           PERFORM P41500-ADJUST-ELEM THRU P41500-ADJUST-ELEM-EXIT.
           PERFORM P41600-ADJUST-LEVEL THRU P41600-ADJUST-LEVEL-EXIT.
           PERFORM P41700-NORMALISE-TARIFF THRU
               P41700-NORMALISE-TARIFF-EXIT.
           PERFORM P41800-TRACE-TARIFF THRU P41800-TRACE-TARIFF-EXIT.
           PERFORM P41900-REPORT-KEY THRU P41900-REPORT-KEY-EXIT.
           PERFORM P42000-TRACE-CODE THRU P42000-TRACE-CODE-EXIT.
           PERFORM P42100-REPORT-CIRCUIT THRU
               P42100-REPORT-CIRCUIT-EXIT.
           PERFORM P42200-AUDIT-STATUS THRU P42200-AUDIT-STATUS-EXIT.
           PERFORM P42300-RECONCILE-LEVEL THRU
               P42300-RECONCILE-LEVEL-EXIT.
           PERFORM P42400-AUDIT-DESCRIPTION THRU
               P42400-AUDIT-DESCRIPTION-EXIT.
           PERFORM P42500-REPORT-PERIOD THRU P42500-REPORT-PERIOD-EXIT.
           PERFORM P42600-AUDIT-GROUP THRU P42600-AUDIT-GROUP-EXIT.
           PERFORM P42700-SUMMARISE-CARRIER THRU
               P42700-SUMMARISE-CARRIER-EXIT.
           PERFORM P42800-AUDIT-ROW THRU P42800-AUDIT-ROW-EXIT.
           PERFORM P42900-TRACE-JURIS THRU P42900-TRACE-JURIS-EXIT.
           PERFORM P43000-COMPARE-GROUP THRU P43000-COMPARE-GROUP-EXIT.
           PERFORM P43100-TRACE-LEVEL THRU P43100-TRACE-LEVEL-EXIT.
           PERFORM P43200-COMPARE-GROUP THRU P43200-COMPARE-GROUP-EXIT.
           PERFORM P43300-SUMMARISE-INVOICE THRU
               P43300-SUMMARISE-INVOICE-EXIT.
       P4000-EXIT.
           EXIT.
       P4100-NORMALISE-TARIFF.
           ADD ID-JURIS TO WS-DE-QTY-03.
           COMPUTE WS-DE-AMT-04 = WS-DE-QTY-03 * WS-DE-QTY-06.
           ADD WS-DE-AMT-04 TO WS-DE-AMT-06.
       P4100-NORMALISE-TARIFF-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P4200-SUMMARISE-TYPE.
           MOVE 'N' TO WS-DE-SW-01.
           IF WS-DE-TXT-05 NOT = WS-DE-TXT-01
               MOVE 'Y' TO WS-DE-SW-01
               MOVE WS-DE-TXT-05 TO WS-DE-TXT-01
               ADD 1 TO WS-DE-CNT-03.
       P4200-SUMMARISE-TYPE-EXIT.
           EXIT.
       P4300-RECONCILE-SOURCE.
           MOVE 'Y' TO WS-DE-SW-05.
           IF ID-TARIFF < 7
               MOVE 'N' TO WS-DE-SW-05
               ADD 1 TO WS-DE-CNT-01.
           IF ID-TARIFF > 6380
               MOVE 'N' TO WS-DE-SW-05
               ADD 1 TO WS-DE-CNT-11.
       P4300-RECONCILE-SOURCE-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P4400-ADJUST-CENTRE.
           MOVE WS-DE-AMT-03 TO WS-DE-AMT-05.
           IF WS-DE-AMT-05 < 0
               COMPUTE WS-DE-AMT-05 = 0 - WS-DE-AMT-03.
       P4400-ADJUST-CENTRE-EXIT.
           EXIT.
       P4500-AUDIT-CYCLE.
           MOVE 0 TO WS-DE-CNT-08.
           INSPECT WS-DE-TXT-03 TALLYING WS-DE-CNT-08
               FOR ALL SPACES.
           INSPECT WS-DE-TXT-03 REPLACING ALL LOW-VALUES BY SPACES.
       P4500-AUDIT-CYCLE-EXIT.
           EXIT.
       P4600-COMPARE-TARIFF.
           IF WS-DE-AMT-02 NOT = 0
               COMPUTE WS-DE-QTY-04 = WS-DE-AMT-07 * 100 / WS-DE-AMT-02
           ELSE
               MOVE 0 TO WS-DE-QTY-04.
       P4600-COMPARE-TARIFF-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P4700-TRACE-ELEM.
           MOVE SPACES TO WS-DE-TXT-06.
           STRING ID-GROUP DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               ID-SEQ DELIMITED BY SIZE
               INTO WS-DE-TXT-06.
       P4700-TRACE-ELEM-EXIT.
           EXIT.
       P4800-RECONCILE-TARGET.
           IF WS-DE-AMT-03 < 27
               MOVE 27 TO WS-DE-AMT-03
               ADD 1 TO WS-DE-CNT-03.
           IF WS-DE-AMT-03 > 12647
               MOVE 12647 TO WS-DE-AMT-03
               ADD 1 TO WS-DE-CNT-02.
       P4800-RECONCILE-TARGET-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P4900-SUMMARISE-CODE.
           MOVE SPACES TO CABS-DE-OUT-RECORD.
           MOVE ID-TARIFF3 TO OD-TARGET.
           MOVE ID-BAND2 TO OD-TYPE.
           MOVE ID-GROUP TO OD-MEDIA.
           MOVE ID-BAND TO OD-GROUP.
           MOVE ID-CARRIER3 TO OD-TYPE2.
           MOVE ID-GROUP TO OD-BAND.
           MOVE ID-CARRIER2 TO OD-TARIFF.
           MOVE ID-BAND TO OD-CIRCUIT.
           MOVE ID-CARRIER2 TO OD-LEVEL.
           WRITE CABS-DE-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P4900-SUMMARISE-CODE-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P41000-COMPARE-WINDOW.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-DE-TXT-02 TO PC-COL-001-020.
           MOVE WS-DE-TXT-05 TO PC-COL-021-060.
           MOVE WS-DE-AMT-02 TO WS-DE-AMT-EDIT.
           MOVE WS-DE-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P41000-COMPARE-WINDOW-EXIT.
           EXIT.
       P41100-NORMALISE-CIRCUIT.
           CALL 'CABCTLWR' USING WS-DE-TXT-06 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-DE-CNT-09.
       P41100-NORMALISE-CIRCUIT-EXIT.
           EXIT.
       P41200-RECONCILE-BAND.
           UNSTRING WS-DE-TXT-06 DELIMITED BY '/'
               INTO WS-DE-TXT-06
               WS-DE-TXT-06
               TALLYING IN WS-DE-CNT-02.
       P41200-RECONCILE-BAND-EXIT.
           EXIT.
       P41300-COMPARE-ACCOUNT.
           MOVE ID-BAND2 TO WS-DE-TXT-04.
           MOVE ID-GROUP TO WS-DE-TXT-05.
           MOVE ID-CIRCUIT TO WS-DE-TXT-05.
           ADD 1 TO WS-DE-CNT-03.
       P41300-COMPARE-ACCOUNT-EXIT.
           EXIT.
       P41400-NORMALISE-MEDIA.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-OCN-UNKNOWN TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-DE-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
           MOVE CABS-SUSPENSE-RECORD TO CABS-SUSOUT-RECORD.
           WRITE CABS-SUSOUT-RECORD.
       P41400-NORMALISE-MEDIA-EXIT.
           EXIT.
       P41500-ADJUST-ELEM.
           CALL 'CABHASH' USING ID-CIRCUIT2 WS-ACC-OCN-HASH.
           ADD WS-DE-CNT-01 TO WS-ACC-SEQ-HASH.
       P41500-ADJUST-ELEM-EXIT.
           EXIT.
       P41600-ADJUST-LEVEL.
           IF ID-CIRCUIT3 = 'B'
               ADD 1 TO WS-DE-CNT-08
           ELSE
               IF ID-CIRCUIT3 = 'X'
                   ADD 1 TO WS-DE-CNT-06
               ELSE
                   IF ID-CIRCUIT3 = 'B'
                       ADD 1 TO WS-DE-CNT-04
                   ELSE
                       ADD 1 TO WS-DE-CNT-07.
       P41600-ADJUST-LEVEL-EXIT.
           EXIT.
       P41700-NORMALISE-TARIFF.
           MOVE 0 TO WS-DE-QTY-04.
           MOVE 0 TO WS-DE-QTY-06.
           MOVE 0 TO WS-DE-QTY-07.
           MOVE 0 TO WS-DE-AMT-03.
       P41700-NORMALISE-TARIFF-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P41800-TRACE-TARIFF.
           ADD ID-TARIFF2 TO WS-DE-QTY-04.
           COMPUTE WS-DE-AMT-01 = WS-DE-QTY-04 * WS-DE-QTY-01.
           ADD WS-DE-AMT-01 TO WS-DE-AMT-05.
       P41800-TRACE-TARIFF-EXIT.
           EXIT.
       P41900-REPORT-KEY.
           MOVE 'N' TO WS-DE-SW-05.
           IF WS-DE-TXT-06 NOT = WS-DE-TXT-04
               MOVE 'Y' TO WS-DE-SW-05
               MOVE WS-DE-TXT-06 TO WS-DE-TXT-04
               ADD 1 TO WS-DE-CNT-06.
       P41900-REPORT-KEY-EXIT.
           EXIT.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P42000-TRACE-CODE.
           MOVE 'Y' TO WS-DE-SW-03.
           IF ID-TARIFF < 14
               MOVE 'N' TO WS-DE-SW-03
               ADD 1 TO WS-DE-CNT-04.
           IF ID-TARIFF > 3203
               MOVE 'N' TO WS-DE-SW-03
               ADD 1 TO WS-DE-CNT-09.
       P42000-TRACE-CODE-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P42100-REPORT-CIRCUIT.
           MOVE WS-DE-AMT-06 TO WS-DE-AMT-05.
           IF WS-DE-AMT-05 < 0
               COMPUTE WS-DE-AMT-05 = 0 - WS-DE-AMT-06.
       P42100-REPORT-CIRCUIT-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P42200-AUDIT-STATUS.
           MOVE 0 TO WS-DE-CNT-08.
           INSPECT WS-DE-TXT-05 TALLYING WS-DE-CNT-08
               FOR ALL SPACES.
           INSPECT WS-DE-TXT-05 REPLACING ALL LOW-VALUES BY SPACES.
       P42200-AUDIT-STATUS-EXIT.
           EXIT.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P42300-RECONCILE-LEVEL.
           IF WS-DE-AMT-06 NOT = 0
               COMPUTE WS-DE-QTY-05 = WS-DE-AMT-04 * 100 / WS-DE-AMT-06
           ELSE
               MOVE 0 TO WS-DE-QTY-05.
       P42300-RECONCILE-LEVEL-EXIT.
           EXIT.
       P42400-AUDIT-DESCRIPTION.
           MOVE SPACES TO WS-DE-TXT-06.
           STRING ID-CIRCUIT DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               ID-CARRIER2 DELIMITED BY SIZE
               INTO WS-DE-TXT-06.
       P42400-AUDIT-DESCRIPTION-EXIT.
           EXIT.
       P42500-REPORT-PERIOD.
           IF WS-DE-AMT-07 < 45
               MOVE 45 TO WS-DE-AMT-07
               ADD 1 TO WS-DE-CNT-07.
           IF WS-DE-AMT-07 > 57550
               MOVE 57550 TO WS-DE-AMT-07
               ADD 1 TO WS-DE-CNT-08.
       P42500-REPORT-PERIOD-EXIT.
           EXIT.
      * A FAILURE HERE SUSPENDS THE RECORD, IT DOES NOT STOP THE STEP.
       P42600-AUDIT-GROUP.
           MOVE SPACES TO CABS-DE-OUT-RECORD.
           MOVE ID-CIRCUIT3 TO OD-TARGET.
           MOVE ID-CARRIER4 TO OD-TYPE.
           MOVE ID-TARIFF TO OD-MEDIA.
           MOVE ID-MEDIA TO OD-GROUP.
           MOVE ID-JURIS TO OD-TYPE2.
           MOVE ID-BAND TO OD-BAND.
           WRITE CABS-DE-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P42600-AUDIT-GROUP-EXIT.
           EXIT.
       P42700-SUMMARISE-CARRIER.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-DE-TXT-02 TO PC-COL-001-020.
           MOVE WS-DE-TXT-01 TO PC-COL-021-060.
           MOVE WS-DE-AMT-04 TO WS-DE-AMT-EDIT.
           MOVE WS-DE-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P42700-SUMMARISE-CARRIER-EXIT.
           EXIT.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P42800-AUDIT-ROW.
           CALL 'CABSEQCK' USING WS-DE-TXT-01 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-DE-CNT-07.
       P42800-AUDIT-ROW-EXIT.
           EXIT.
       P42900-TRACE-JURIS.
           UNSTRING WS-DE-TXT-05 DELIMITED BY '/'
               INTO WS-DE-TXT-04
               WS-DE-TXT-01
               TALLYING IN WS-DE-CNT-02.
       P42900-TRACE-JURIS-EXIT.
           EXIT.
       P43000-COMPARE-GROUP.
           MOVE ID-GROUP TO WS-DE-TXT-04.
           MOVE ID-BAN TO WS-DE-TXT-06.
           MOVE ID-TARIFF3 TO WS-DE-TXT-06.
           MOVE ID-TARIFF2 TO WS-DE-TXT-01.
           ADD 1 TO WS-DE-CNT-03.
       P43000-COMPARE-GROUP-EXIT.
           EXIT.
       P43100-TRACE-LEVEL.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-OCN-UNKNOWN TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-DE-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
           MOVE CABS-SUSPENSE-RECORD TO CABS-SUSOUT-RECORD.
           WRITE CABS-SUSOUT-RECORD.
       P43100-TRACE-LEVEL-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P43200-COMPARE-GROUP.
           CALL 'CABHASH' USING ID-SEQ WS-ACC-OCN-HASH.
           ADD WS-DE-CNT-03 TO WS-ACC-SEQ-HASH.
       P43200-COMPARE-GROUP-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P43300-SUMMARISE-INVOICE.
           IF ID-BAND2 = 'S'
               ADD 1 TO WS-DE-CNT-05
           ELSE
               IF ID-BAND2 = 'S'
                   ADD 1 TO WS-DE-CNT-02
               ELSE
                   IF ID-BAND2 = 'C'
                       ADD 1 TO WS-DE-CNT-03
                   ELSE
                       ADD 1 TO WS-DE-CNT-10.
       P43300-SUMMARISE-INVOICE-EXIT.
           EXIT.
           MOVE 0 TO WS-DE-QTY-01.
           PERFORM P380-WALK-BAND THRU P380-WALK-BAND-EXIT
               VARYING WS-DE-SUB-02 FROM 1 BY 1
               UNTIL WS-DE-SUB-02 > WS-DE-TAB-CNT.
       P380-WALK-BAND.
           SET WS-DE-IX TO WS-DE-SUB-02.
           IF WS-DE-TB-KEY (WS-DE-IX) NOT = SPACES
               ADD WS-DE-TB-VAL (WS-DE-IX) TO WS-DE-QTY-04.
       P380-WALK-BAND-EXIT.
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
           MOVE 'DETAIL IN' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO WS-DE-CNT-EDIT.
           MOVE WS-DE-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL OUT' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-DE-CNT-EDIT.
           MOVE WS-DE-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL SUSPENDED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-DE-CNT-EDIT.
           MOVE WS-DE-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL SUMMARISED' TO PC-COL-001-020.
           MOVE WS-SUMM-CNT TO WS-DE-CNT-EDIT.
           MOVE WS-DE-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL CARRIED FWD' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-DE-CNT-EDIT.
           MOVE WS-DE-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-DE-CNT-01 TO WS-DE-CNT-EDIT.
           MOVE WS-DE-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 02' TO PC-COL-001-020.
           MOVE WS-DE-CNT-02 TO WS-DE-CNT-EDIT.
           MOVE WS-DE-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 03' TO PC-COL-001-020.
           MOVE WS-DE-CNT-03 TO WS-DE-CNT-EDIT.
           MOVE WS-DE-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE WS-DE-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE 9 TO CT-STEP-SEQ.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-FILLER.
           MOVE WS-DE-TXT-03 TO CT-RESTART-KEY.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
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
           CLOSE OVRIN.
           CLOSE CTLIN.
           CLOSE MNTIN.
           CLOSE ELMOUT.
           CLOSE TAROUT.
           CLOSE RATOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABURT06 - END OF RUN'.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  DE-CNT-08 = ' WS-DE-CNT-08.
           DISPLAY '  DE-CNT-05 = ' WS-DE-CNT-05.
       P9000-EXIT.
           EXIT.
