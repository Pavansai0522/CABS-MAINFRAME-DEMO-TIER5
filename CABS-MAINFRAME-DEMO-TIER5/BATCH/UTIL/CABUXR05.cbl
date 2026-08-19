      *****************************************************************
      * CABUXR05 - ACCOUNT TO INVOICE CROSS REFERENCE                 *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               MSTIN   TELCABS.CABS.MSTIN          (LOCAL)     *
      *               SUSIN   TELCABS.CABS.SUSIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               MTCOUT  TELCABS.CABS.MTCOUT         (LOCAL)     *
      *               ORPOUT  TELCABS.CABS.ORPOUT         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * REVISION HISTORY                                              *
      *   V1.00  1990-07-04  M.DELACROIX  INITIAL RELEASE             *
      *   V1.02  1991-04-02  L.FERREIRA   PARM CARD EXTENDED,         *
      *                      POSITIONS 40 THROUGH 48                  *
      *   V1.06  1993-08-21  K.O.BRIEN    CONTROL RECORD ADDED PER    *
      *                      CABS-STD-002                             *
      *   V1.09  2004-11-04  P.NAIR       JOB PARAMETER MADE MANDATORY*
      *   V1.12  2010-01-11  L.FERREIRA   SECOND OUTPUT FILE ADDED FOR*
      *                      THE FACTOR STUDY                         *
      *   V1.16  2011-12-13  G.PRZYBYLSKI HASH TOTAL ADDED TO THE     *
      *                      CONTROL RECORD                           *
      *   V1.20  2012-02-19  C.ADEYEMI    CENTURY PIVOT APPLIED TO THE*
      *                      CYCLE DATE                               *
      *   V1.21  2017-02-28  L.FERREIRA   BLOCK SIZE SET TO ZERO -    *
      *                      SYSTEM DETERMINED                        *
      *   V1.24  2018-09-09  P.NAIR       HASH TOTAL ADDED TO THE     *
      *                      CONTROL RECORD                           *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUXR05.
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
           SELECT SUSIN ASSIGN TO UT-S-SUSIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT MTCOUT ASSIGN TO UT-S-MTCOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT ORPOUT ASSIGN TO UT-S-ORPOUT
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
      * MSTIN - PERMANENT DATASET HELD ON DASD.
       FD  MSTIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 170 CHARACTERS.
       01  CABS-DU-IN-RECORD.
           05  ID-MEDIA                    PIC X(10).
           05  ID-CIRCUIT                  PIC X(03).
           05  ID-REGION                   PIC X(13).
           05  ID-SEQ                      PIC 9(05).
           05  ID-CYCLE                    PIC S9(07) COMP-3.
           05  ID-LEVEL                    PIC 9(03).
           05  ID-ACCOUNT                  PIC S9(13)V9(02) COMP-3.
           05  ID-ELEM                     PIC X(04).
           05  ID-CIRCUIT2                 PIC X(08).
           05  ID-LEVEL2                   PIC S9(13) COMP-3.
           05  ID-CODE                     PIC X(04).
           05  ID-SEQ2                     PIC S9(09) COMP-3.
           05  ID-LEVEL3                   PIC S9(09)V9(05) COMP-3.
           05  ID-CARRIER                  PIC S9(13)V9(02) COMP-3.
           05  ID-REGION2                  PIC X(03).
           05  ID-SEQ3                     PIC X(20).
           05  ID-INVOICE                  PIC 9(06).
           05  ID-ELEM2                    PIC S9(13)V9(02) COMP-3.
           05  ID-CARRIER2                 PIC S9(13)V9(02) COMP-3.
           05  ID-CIRCUIT3                 PIC X(02).
           05  ID-STATUS                   PIC X(02).
           05  ID-CIRCUIT4                 PIC S9(09)V9(05) COMP-3.
           05  ID-TYPE                     PIC 9(03).
           05  ID-CARRIER3                 PIC X(16).
           05  ID-TARIFF                   PIC 9(03).
           05  DU-FILL-01                  PIC X(1).
      * SUSIN - WORK FILE, DELETED AT STEP END.
       FD  SUSIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 170 CHARACTERS.
       01  CABS-DU-ALT1-RECORD.
           05  A1-BAND                     PIC S9(09)V9(02) COMP-3.
           05  A1-STATUS                   PIC X(13).
           05  A1-SEQ                      PIC S9(07)V9(02) COMP-3.
           05  A1-CIRCUIT                  PIC S9(07)V9(05) COMP-3.
           05  A1-LEVEL                    PIC X(03).
           05  A1-STATUS2                  PIC S9(15) COMP-3.
           05  A1-ACCOUNT                  PIC X(04).
           05  A1-SOURCE                   PIC X(02).
           05  A1-CLASS                    PIC S9(07)V9(02) COMP-3.
           05  A1-CLASS2                   PIC 9(03).
           05  A1-REGION                   PIC X(06).
           05  DU-FILL-02                  PIC X(108).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-DU-VIEW1 REDEFINES CABS-DU-IN-RECORD.
           05  R0D-CLASS                   PIC 9(02).
           05  R0D-INVOICE                 PIC X(20).
           05  R0D-TARIFF                  PIC X(06).
           05  R0D-TARIFF2                 PIC 9(02).
           05  R0D-SEGMENT                 PIC X(04).
           05  R0D-CLASS2                  PIC S9(11) COMP-3.
           05  R0D-GROUP                   PIC 9(02).
           05  R0D-BAN                     PIC X(13).
           05  R0D-MEDIA                   PIC X(02).
           05  R0D-REST                    PIC X(113).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-DU-VIEW2 REDEFINES CABS-DU-IN-RECORD.
           05  R1D-SEGMENT                 PIC S9(07)V9(05) COMP-3.
           05  R1D-PERIOD                  PIC X(02).
           05  R1D-CYCLE                   PIC S9(09) COMP-3.
           05  R1D-CIRCUIT                 PIC 9(06).
           05  R1D-TARIFF                  PIC X(16).
           05  R1D-CLASS                   PIC 9(05).
           05  R1D-SOURCE                  PIC X(16).
           05  R1D-TYPE                    PIC S9(13) COMP-3.
           05  R1D-MEDIA                   PIC 9(05).
           05  R1D-REST                    PIC X(101).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-DU-VIEW3 REDEFINES CABS-DU-IN-RECORD.
           05  R2D-BAN                     PIC X(08).
           05  R2D-JURIS                   PIC S9(11)V9(02) COMP-3.
           05  R2D-SEQ                     PIC 9(05).
           05  R2D-STATE                   PIC S9(09) COMP-3.
           05  R2D-STATUS                  PIC S9(13)V9(02) COMP-3.
           05  R2D-SEQ2                    PIC 9(03).
           05  R2D-INVOICE                 PIC 9(03).
           05  R2D-REST                    PIC X(131).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-DU-VIEW4 REDEFINES CABS-DU-IN-RECORD.
           05  R3D-TARGET                  PIC X(03).
           05  R3D-CENTRE                  PIC 9(04).
           05  R3D-BAN                     PIC X(03).
           05  R3D-JURIS                   PIC X(08).
           05  R3D-SEGMENT                 PIC 9(05).
           05  R3D-BAND                    PIC 9(07).
           05  R3D-TYPE                    PIC X(04).
           05  R3D-REST                    PIC X(136).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-DU-VIEW5 REDEFINES CABS-DU-IN-RECORD.
           05  R4D-TARGET                  PIC S9(05) COMP-3.
           05  R4D-CODE                    PIC S9(13) COMP-3.
           05  R4D-CODE2                   PIC 9(04).
           05  R4D-PERIOD                  PIC S9(11) COMP-3.
           05  R4D-CLASS                   PIC X(16).
           05  R4D-BAND                    PIC X(13).
           05  R4D-CARRIER                 PIC X(04).
           05  R4D-GROUP                   PIC S9(13) COMP-3.
           05  R4D-REST                    PIC X(110).
      * MTCOUT - PERMANENT DATASET HELD ON DASD.
       FD  MTCOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 120 CHARACTERS.
       01  CABS-DU-OUT-RECORD.
           05  OD-TARGET                   PIC 9(07).
           05  OD-BAND                     PIC S9(09)V9(02) COMP-3.
           05  OD-BAN                      PIC X(03).
           05  OD-JURIS                    PIC 9(02).
           05  OD-TARGET2                  PIC 9(09).
           05  OD-OCN                      PIC 9(06).
           05  OD-CENTRE                   PIC 9(02).
           05  OD-TARGET3                  PIC S9(09) COMP-3.
           05  OD-MEDIA                    PIC S9(11)V9(02) COMP-3.
           05  OD-JURIS2                   PIC 9(09).
           05  OD-JURIS3                   PIC 9(03).
           05  OD-CYCLE                    PIC X(04).
           05  OD-REGION                   PIC 9(05).
           05  OD-JURIS4                   PIC X(16).
           05  OD-LEVEL                    PIC X(08).
           05  OD-SEGMENT                  PIC 9(04).
           05  OD-PERIOD                   PIC 9(03).
           05  OD-BAN2                     PIC X(04).
           05  OD-TYPE                     PIC S9(13) COMP-3.
           05  OD-SEQ                      PIC S9(05) COMP-3.
           05  DU-FILL-03                  PIC X(7).
      * ORPOUT - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  ORPOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 120 CHARACTERS.
       01  CABS-DU-OUT1-RECORD         PIC X(120).
      * SUSOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  SUSOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 300 CHARACTERS.
       01  CABS-SUSOUT-RECORD              PIC X(300).
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
      * SHARED LAYOUT PULLED IN FOR THE LINK SIDE.
       COPY CABSCIRC.
      * SHARED LAYOUT PULLED IN FOR THE LINK SIDE.
       COPY CABSBHDR.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUXR05'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V1.02'.
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
           05  WS-DU-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DU-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DU-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DU-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DU-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DU-CNT-06                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DU-CNT-07                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DU-CNT-08                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DU-CNT-09                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DU-CNT-10                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DU-CNT-11                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DU-CNT-12                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-DU-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DU-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DU-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DU-QTY-04                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DU-QTY-05                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DU-QTY-06                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DU-QTY-07                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-DU-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DU-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DU-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DU-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DU-AMT-05                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DU-AMT-06                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DU-AMT-07                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-DU-TXT-01                PIC X(30) VALUE SPACES.
           05  WS-DU-TXT-02                PIC X(30) VALUE SPACES.
           05  WS-DU-TXT-03                PIC X(20) VALUE SPACES.
           05  WS-DU-TXT-04                PIC X(30) VALUE SPACES.
           05  WS-DU-TXT-05                PIC X(12) VALUE SPACES.
           05  WS-DU-TXT-06                PIC X(08) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-DU-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-DU-ON-01                 VALUE 'Y'.
               88  WS-DU-OFF-01                VALUE 'N'.
           05  WS-DU-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-DU-ON-02                 VALUE 'Y'.
               88  WS-DU-OFF-02                VALUE 'N'.
           05  WS-DU-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-DU-ON-03                 VALUE 'Y'.
               88  WS-DU-OFF-03                VALUE 'N'.
           05  WS-DU-SW-04                 PIC X(01) VALUE 'N'.
               88  WS-DU-ON-04                 VALUE 'Y'.
               88  WS-DU-OFF-04                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-DU-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DU-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DU-SUB-03                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DU-SUB-04                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. LOADED ONCE AT INITIALISATION AND SEARCHED WITH
      * A SUBSCRIPTED WALK.
       01  WS-DU-TABLE.
           05  WS-DU-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-DU-TB-ENTRY OCCURS 300 TIMES
                                       INDEXED BY WS-DU-IX.
               10  WS-DU-TB-KEY                PIC X(06).
               10  WS-DU-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-DU-TB-TXT                PIC X(20).
               10  WS-DU-TB-EFF                PIC 9(05).
               10  WS-DU-TB-EXP                PIC 9(05).
       01  WS-DU-WORK-GROUP-1.
           05  WS-DU-G1-CYCLE              PIC X(10).
           05  WS-DU-G1-CODE               PIC S9(11)V9(02) COMP-3.
           05  WS-DU-G1-OCN                PIC X(10).
           05  WS-DU-G1-REGION             PIC X(10).
           05  WS-DU-G1-SOURCE             PIC 9(07).
           05  WS-DU-G1-GROUP              PIC 9(05).
           05  WS-DU-G1-JURIS              PIC S9(11)V9(02) COMP-3.
           05  WS-DU-G1-ACCOUNT            PIC 9(05).
       01  WS-DU-WORK-GROUP-2.
           05  WS-DU-G2-BAN                PIC S9(11)V9(02) COMP-3.
           05  WS-DU-G2-TARIFF             PIC X(10).
           05  WS-DU-G2-CARRIER            PIC X(10).
           05  WS-DU-G2-JURIS              PIC X(10).
           05  WS-DU-G2-SEGMENT            PIC X(10).
           05  WS-DU-G2-JURIS              PIC S9(11)V9(02) COMP-3.
           05  WS-DU-G2-GROUP              PIC X(20).
           05  WS-DU-G2-ELEM               PIC 9(07).
       01  WS-DU-WORK-GROUP-3.
           05  WS-DU-G3-STATE              PIC S9(09) COMP-3.
           05  WS-DU-G3-BAND               PIC S9(11)V9(02) COMP-3.
           05  WS-DU-G3-ACCOUNT            PIC S9(09) COMP-3.
           05  WS-DU-G3-ACCOUNT            PIC X(20).
           05  WS-DU-G3-BAN                PIC 9(07).
           05  WS-DU-G3-SEQ                PIC S9(09) COMP-3.
           05  WS-DU-G3-CIRCUIT            PIC 9(05).
           05  WS-DU-G3-SOURCE             PIC X(10).
       01  WS-DU-WORK-GROUP-4.
           05  WS-DU-G4-LEVEL              PIC X(10).
           05  WS-DU-G4-PERIOD             PIC 9(07).
           05  WS-DU-G4-PERIOD             PIC 9(05).
           05  WS-DU-G4-STATUS             PIC 9(07).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 55.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUXR05 - ACCOUNT TO INVOICE CROSS REFERENCE'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-DU-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-DU-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9962.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-DU-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-DU-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN INPUT SUSIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'SUSIN NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT MTCOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'MTCOUT NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT ORPOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'ORPOUT NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SUSOUT.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'SUSOUT NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CTLOUT NOT AVAILABLE - OPEN REJECTED' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RPTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RPTOUT NOT AVAILABLE - OPEN REJECTED' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-DU-CYCLE-YYDDD.
           COMPUTE WS-DU-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-DU-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-DU-CNT-08.
           MOVE 0 TO WS-DU-CNT-06.
           MOVE 0 TO WS-DU-CNT-09.
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
           PERFORM P2200-RESOLVE-MATCH THRU P2200-RESOLVE-MATCH-EXIT.
           PERFORM P2300-APPLY-PAIR THRU P2300-APPLY-PAIR-EXIT.
           IF WS-DU-ON-02
               PERFORM P2400-APPLY-LINK THRU P2400-APPLY-LINK-EXIT.
           IF WS-DU-ON-03
               PERFORM P2500-APPLY-PAIR THRU P2500-APPLY-PAIR-EXIT.
           IF WS-DU-ON-02
               PERFORM P2600-RESOLVE-ORPHAN THRU
                   P2600-RESOLVE-ORPHAN-EXIT.
           PERFORM P2700-BUILD-PAIR THRU P2700-BUILD-PAIR-EXIT.
           IF WS-DU-ON-01
               PERFORM P2800-BUILD-PAIR THRU P2800-BUILD-PAIR-EXIT.
           PERFORM P2900-RESOLVE-REFERENCE THRU
               P2900-RESOLVE-REFERENCE-EXIT.
           PERFORM P21000-CHECK-PAIR THRU P21000-CHECK-PAIR-EXIT.
           PERFORM P21100-EDIT-REFERENCE THRU
               P21100-EDIT-REFERENCE-EXIT.
           IF WS-DU-ON-01
               PERFORM P21200-EDIT-SIDE THRU P21200-EDIT-SIDE-EXIT.
           IF WS-DU-ON-02
               PERFORM P21300-EDIT-ORPHAN THRU P21300-EDIT-ORPHAN-EXIT.
           IF WS-DU-ON-04
               PERFORM P21400-DERIVE-SIDE THRU P21400-DERIVE-SIDE-EXIT.
           IF WS-DU-ON-02
               PERFORM P21500-RESOLVE-SIDE THRU
                   P21500-RESOLVE-SIDE-EXIT.
           PERFORM P21600-VALIDATE-REFERENCE THRU
               P21600-VALIDATE-REFERENCE-EXIT.
           IF WS-DU-ON-04
               PERFORM P21700-EXPAND-ORPHAN THRU
                   P21700-EXPAND-ORPHAN-EXIT.
           PERFORM P21800-EDIT-REFERENCE THRU
               P21800-EDIT-REFERENCE-EXIT.
           PERFORM P21900-VALIDATE-LINK THRU P21900-VALIDATE-LINK-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ MSTIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P2200-RESOLVE-MATCH.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-DUP-SEQ TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-DU-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
       P2200-RESOLVE-MATCH-EXIT.
           EXIT.
       P2300-APPLY-PAIR.
           ADD ID-CARRIER2 TO WS-DU-QTY-07.
           COMPUTE WS-DU-AMT-01 ROUNDED = WS-DU-QTY-07 * WS-DU-QTY-01.
           ADD WS-DU-AMT-01 TO WS-DU-AMT-05.
       P2300-APPLY-PAIR-EXIT.
           EXIT.
      * A FAILURE HERE SUSPENDS THE RECORD, IT DOES NOT STOP THE STEP.
       P2400-APPLY-LINK.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-DU-TXT-06 TO PC-COL-001-020.
           MOVE WS-DU-TXT-05 TO PC-COL-021-060.
           MOVE WS-DU-AMT-01 TO WS-DU-AMT-EDIT.
           MOVE WS-DU-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P2400-APPLY-LINK-EXIT.
           EXIT.
       P2500-APPLY-PAIR.
           IF WS-DU-AMT-03 < 47
               MOVE 47 TO WS-DU-AMT-03
               ADD 1 TO WS-DU-CNT-04.
           IF WS-DU-AMT-03 > 44793
               MOVE 44793 TO WS-DU-AMT-03
               ADD 1 TO WS-DU-CNT-01.
       P2500-APPLY-PAIR-EXIT.
           EXIT.
       P2600-RESOLVE-ORPHAN.
           IF WS-DU-AMT-06 NOT = 0
               COMPUTE WS-DU-QTY-04 = WS-DU-AMT-01 * 100 / WS-DU-AMT-06
           ELSE
               MOVE 0 TO WS-DU-QTY-04.
       P2600-RESOLVE-ORPHAN-EXIT.
           EXIT.
       P2700-BUILD-PAIR.
           MOVE ID-CIRCUIT3 TO WS-DU-TXT-04.
           MOVE ID-CARRIER TO WS-DU-TXT-06.
           ADD 1 TO WS-DU-CNT-01.
       P2700-BUILD-PAIR-EXIT.
           EXIT.
       P2800-BUILD-PAIR.
           MOVE WS-DU-AMT-01 TO WS-DU-AMT-07.
           IF WS-DU-AMT-07 < 0
               COMPUTE WS-DU-AMT-07 = 0 - WS-DU-AMT-01.
       P2800-BUILD-PAIR-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P2900-RESOLVE-REFERENCE.
           MOVE 'N' TO WS-DU-SW-02.
           IF WS-DU-TXT-02 NOT = WS-DU-TXT-04
               MOVE 'Y' TO WS-DU-SW-02
               MOVE WS-DU-TXT-02 TO WS-DU-TXT-04
               ADD 1 TO WS-DU-CNT-02.
       P2900-RESOLVE-REFERENCE-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P21000-CHECK-PAIR.
           MOVE 0 TO WS-DU-CNT-11.
           INSPECT WS-DU-TXT-05 TALLYING WS-DU-CNT-11
               FOR ALL SPACES.
           INSPECT WS-DU-TXT-05 REPLACING ALL LOW-VALUES BY SPACES.
       P21000-CHECK-PAIR-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P21100-EDIT-REFERENCE.
           MOVE SPACES TO CABS-DU-OUT-RECORD.
           MOVE ID-SEQ TO OD-TARGET.
           MOVE ID-CARRIER3 TO OD-BAND.
           MOVE ID-SEQ3 TO OD-BAN.
           MOVE ID-CYCLE TO OD-JURIS.
           MOVE ID-SEQ3 TO OD-TARGET2.
           MOVE ID-LEVEL2 TO OD-OCN.
           MOVE ID-INVOICE TO OD-CENTRE.
           WRITE CABS-DU-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P21100-EDIT-REFERENCE-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P21200-EDIT-SIDE.
           MOVE 'Y' TO WS-DU-SW-02.
           IF ID-LEVEL2 < 5
               MOVE 'N' TO WS-DU-SW-02
               ADD 1 TO WS-DU-CNT-07.
           IF ID-LEVEL2 > 6473
               MOVE 'N' TO WS-DU-SW-02
               ADD 1 TO WS-DU-CNT-07.
       P21200-EDIT-SIDE-EXIT.
           EXIT.
       P21300-EDIT-ORPHAN.
           UNSTRING WS-DU-TXT-04 DELIMITED BY '/'
               INTO WS-DU-TXT-01
               WS-DU-TXT-02
               TALLYING IN WS-DU-CNT-12.
       P21300-EDIT-ORPHAN-EXIT.
           EXIT.
       P21400-DERIVE-SIDE.
           MOVE SPACES TO WS-DU-TXT-02.
           STRING ID-CIRCUIT2 DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               ID-CIRCUIT3 DELIMITED BY SIZE
               INTO WS-DU-TXT-02.
       P21400-DERIVE-SIDE-EXIT.
           EXIT.
       P21500-RESOLVE-SIDE.
           CALL 'CABTBLLU' USING WS-DU-TXT-06 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-DU-CNT-02.
       P21500-RESOLVE-SIDE-EXIT.
           EXIT.
       P21600-VALIDATE-REFERENCE.
           CALL 'CABHASH' USING ID-LEVEL WS-ACC-OCN-HASH.
           ADD WS-DU-CNT-12 TO WS-ACC-SEQ-HASH.
       P21600-VALIDATE-REFERENCE-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P21700-EXPAND-ORPHAN.
           IF ID-LEVEL3 = 'C'
               ADD 1 TO WS-DU-CNT-07
           ELSE
               IF ID-LEVEL3 = 'D'
                   ADD 1 TO WS-DU-CNT-01
               ELSE
                   IF ID-LEVEL3 = 'A'
                       ADD 1 TO WS-DU-CNT-11
                   ELSE
                       ADD 1 TO WS-DU-CNT-08.
       P21700-EXPAND-ORPHAN-EXIT.
           EXIT.
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT
      * PRECEDES THIS PROGRAM IN THE JOB.
       P21800-EDIT-REFERENCE.
           MOVE 0 TO WS-DU-QTY-06.
           MOVE 0 TO WS-DU-QTY-07.
           MOVE 0 TO WS-DU-AMT-04.
       P21800-EDIT-REFERENCE-EXIT.
           EXIT.
       P21900-VALIDATE-LINK.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-DATE-INVALID TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-DU-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
       P21900-VALIDATE-LINK-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P3100-RELEASE-LINK.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-DU-TXT-03 TO PC-COL-001-020.
           MOVE WS-DU-TXT-06 TO PC-COL-021-060.
           MOVE WS-DU-AMT-07 TO WS-DU-AMT-EDIT.
           MOVE WS-DU-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P3100-RELEASE-LINK-EXIT.
           EXIT.
       P3200-CLOSE-OFF-LINK.
           MOVE SPACES TO CABS-DU-OUT-RECORD.
           MOVE ID-CARRIER3 TO OD-TARGET.
           MOVE ID-CIRCUIT2 TO OD-BAND.
           MOVE ID-CIRCUIT3 TO OD-BAN.
           MOVE ID-CARRIER3 TO OD-JURIS.
           MOVE ID-LEVEL TO OD-TARGET2.
           WRITE CABS-DU-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P3200-CLOSE-OFF-LINK-EXIT.
           EXIT.
       P3300-CLOSE-OFF-GROUP.
           MOVE 0 TO WS-DU-QTY-01.
           MOVE 0 TO WS-DU-QTY-07.
           MOVE 0 TO WS-DU-QTY-06.
           MOVE 0 TO WS-DU-AMT-07.
       P3300-CLOSE-OFF-GROUP-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P3400-RELEASE-ORPHAN.
           MOVE ID-CODE TO WS-DU-TXT-01.
           MOVE ID-CIRCUIT4 TO WS-DU-TXT-06.
           MOVE ID-ELEM TO WS-DU-TXT-03.
           ADD 1 TO WS-DU-CNT-08.
       P3400-RELEASE-ORPHAN-EXIT.
           EXIT.
       P3500-EMIT-SIDE.
           ADD ID-LEVEL2 TO WS-DU-QTY-06.
           COMPUTE WS-DU-AMT-01 ROUNDED = WS-DU-QTY-06 * WS-DU-QTY-06.
           ADD WS-DU-AMT-01 TO WS-DU-AMT-07.
       P3500-EMIT-SIDE-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P3600-RELEASE-SIDE.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-DATE-INVALID TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-DU-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
           MOVE CABS-SUSPENSE-RECORD TO CABS-SUSOUT-RECORD.
           WRITE CABS-SUSOUT-RECORD.
       P3600-RELEASE-SIDE-EXIT.
           EXIT.
      * S400-SECONDARY-PROCESSING SECTION
       S400-SECONDARY-PROCESSING SECTION.
       P4000-SECONDARY.
           PERFORM P4100-COMPARE-TARIFF THRU P4100-COMPARE-TARIFF-EXIT.
           PERFORM P4200-TRACE-SOURCE THRU P4200-TRACE-SOURCE-EXIT.
           PERFORM P4300-AUDIT-SOURCE THRU P4300-AUDIT-SOURCE-EXIT.
           PERFORM P4400-SUMMARISE-REGION THRU
               P4400-SUMMARISE-REGION-EXIT.
           PERFORM P4500-ADJUST-SEQ THRU P4500-ADJUST-SEQ-EXIT.
           PERFORM P4600-AUDIT-REFERENCE THRU
               P4600-AUDIT-REFERENCE-EXIT.
           PERFORM P4700-SUMMARISE-OCN THRU P4700-SUMMARISE-OCN-EXIT.
           PERFORM P4800-AUDIT-ORPHAN THRU P4800-AUDIT-ORPHAN-EXIT.
           PERFORM P4900-COMPARE-PAIR THRU P4900-COMPARE-PAIR-EXIT.
           PERFORM P41000-REPORT-SEQ THRU P41000-REPORT-SEQ-EXIT.
           PERFORM P41100-TRACE-REGION THRU P41100-TRACE-REGION-EXIT.
           PERFORM P41200-NORMALISE-PAIR THRU
               P41200-NORMALISE-PAIR-EXIT.
           PERFORM P41300-COMPARE-OCN THRU P41300-COMPARE-OCN-EXIT.
           PERFORM P41400-NORMALISE-TARGET THRU
               P41400-NORMALISE-TARGET-EXIT.
           PERFORM P41500-REPORT-GROUP THRU P41500-REPORT-GROUP-EXIT.
           PERFORM P41600-COMPARE-CLASS THRU P41600-COMPARE-CLASS-EXIT.
           PERFORM P41700-SUMMARISE-CODE THRU
               P41700-SUMMARISE-CODE-EXIT.
           PERFORM P41800-NORMALISE-OCN THRU P41800-NORMALISE-OCN-EXIT.
           PERFORM P41900-TRACE-PERIOD THRU P41900-TRACE-PERIOD-EXIT.
           PERFORM P42000-TRACE-JURIS THRU P42000-TRACE-JURIS-EXIT.
           PERFORM P42100-NORMALISE-TARGET THRU
               P42100-NORMALISE-TARGET-EXIT.
           PERFORM P42200-RECONCILE-JURIS THRU
               P42200-RECONCILE-JURIS-EXIT.
           PERFORM P42300-TRACE-CLASS THRU P42300-TRACE-CLASS-EXIT.
           PERFORM P42400-NORMALISE-MATCH THRU
               P42400-NORMALISE-MATCH-EXIT.
           PERFORM P42500-ADJUST-STATE THRU P42500-ADJUST-STATE-EXIT.
           PERFORM P42600-COMPARE-CLASS THRU P42600-COMPARE-CLASS-EXIT.
           PERFORM P42700-NORMALISE-SOURCE THRU
               P42700-NORMALISE-SOURCE-EXIT.
           PERFORM P42800-TRACE-REFERENCE THRU
               P42800-TRACE-REFERENCE-EXIT.
           PERFORM P42900-COMPARE-PERIOD THRU
               P42900-COMPARE-PERIOD-EXIT.
           PERFORM P43000-SUMMARISE-BAN THRU P43000-SUMMARISE-BAN-EXIT.
           PERFORM P43100-REPORT-REFERENCE THRU
               P43100-REPORT-REFERENCE-EXIT.
           PERFORM P43200-NORMALISE-TARGET THRU
               P43200-NORMALISE-TARGET-EXIT.
       P4000-EXIT.
           EXIT.
       P4100-COMPARE-TARIFF.
           MOVE SPACES TO WS-DU-TXT-01.
           STRING ID-MEDIA DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               ID-CIRCUIT4 DELIMITED BY SIZE
               INTO WS-DU-TXT-01.
       P4100-COMPARE-TARIFF-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P4200-TRACE-SOURCE.
           MOVE 'Y' TO WS-DU-SW-03.
           IF ID-SEQ2 < 15
               MOVE 'N' TO WS-DU-SW-03
               ADD 1 TO WS-DU-CNT-08.
           IF ID-SEQ2 > 7433
               MOVE 'N' TO WS-DU-SW-03
               ADD 1 TO WS-DU-CNT-06.
       P4200-TRACE-SOURCE-EXIT.
           EXIT.
       P4300-AUDIT-SOURCE.
           IF WS-DU-AMT-05 < 45
               MOVE 45 TO WS-DU-AMT-05
               ADD 1 TO WS-DU-CNT-07.
           IF WS-DU-AMT-05 > 23145
               MOVE 23145 TO WS-DU-AMT-05
               ADD 1 TO WS-DU-CNT-05.
       P4300-AUDIT-SOURCE-EXIT.
           EXIT.
       P4400-SUMMARISE-REGION.
           MOVE ID-SEQ2 TO WS-DU-TXT-02.
           MOVE ID-LEVEL2 TO WS-DU-TXT-04.
           ADD 1 TO WS-DU-CNT-03.
       P4400-SUMMARISE-REGION-EXIT.
           EXIT.
       P4500-ADJUST-SEQ.
           CALL 'CABCTLWR' USING WS-DU-TXT-02 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-DU-CNT-07.
       P4500-ADJUST-SEQ-EXIT.
           EXIT.
       P4600-AUDIT-REFERENCE.
           MOVE 0 TO WS-DU-CNT-07.
           INSPECT WS-DU-TXT-02 TALLYING WS-DU-CNT-07
               FOR ALL SPACES.
           INSPECT WS-DU-TXT-02 REPLACING ALL LOW-VALUES BY SPACES.
       P4600-AUDIT-REFERENCE-EXIT.
           EXIT.
       P4700-SUMMARISE-OCN.
           CALL 'CABHASH' USING ID-SEQ3 WS-ACC-OCN-HASH.
           ADD WS-DU-CNT-08 TO WS-ACC-SEQ-HASH.
       P4700-SUMMARISE-OCN-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P4800-AUDIT-ORPHAN.
           ADD ID-ACCOUNT TO WS-DU-QTY-01.
           COMPUTE WS-DU-AMT-02 ROUNDED = WS-DU-QTY-01 * WS-DU-QTY-04.
           ADD WS-DU-AMT-02 TO WS-DU-AMT-07.
       P4800-AUDIT-ORPHAN-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P4900-COMPARE-PAIR.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-RATE-NOT-FOUND TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-DU-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
           MOVE CABS-SUSPENSE-RECORD TO CABS-SUSOUT-RECORD.
           WRITE CABS-SUSOUT-RECORD.
       P4900-COMPARE-PAIR-EXIT.
           EXIT.
       P41000-REPORT-SEQ.
           MOVE 'N' TO WS-DU-SW-01.
           IF WS-DU-TXT-05 NOT = WS-DU-TXT-02
               MOVE 'Y' TO WS-DU-SW-01
               MOVE WS-DU-TXT-05 TO WS-DU-TXT-02
               ADD 1 TO WS-DU-CNT-10.
       P41000-REPORT-SEQ-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P41100-TRACE-REGION.
           MOVE SPACES TO CABS-DU-OUT-RECORD.
           MOVE ID-CODE TO OD-TARGET.
           MOVE ID-LEVEL2 TO OD-BAND.
           MOVE ID-CIRCUIT TO OD-BAN.
           MOVE ID-REGION TO OD-JURIS.
           WRITE CABS-DU-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P41100-TRACE-REGION-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P41200-NORMALISE-PAIR.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-DU-TXT-06 TO PC-COL-001-020.
           MOVE WS-DU-TXT-01 TO PC-COL-021-060.
           MOVE WS-DU-AMT-04 TO WS-DU-AMT-EDIT.
           MOVE WS-DU-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P41200-NORMALISE-PAIR-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P41300-COMPARE-OCN.
           MOVE 0 TO WS-DU-QTY-05.
           MOVE 0 TO WS-DU-QTY-06.
           MOVE 0 TO WS-DU-QTY-01.
           MOVE 0 TO WS-DU-AMT-06.
           MOVE 0 TO WS-DU-AMT-02.
       P41300-COMPARE-OCN-EXIT.
           EXIT.
       P41400-NORMALISE-TARGET.
           IF ID-SEQ2 = 'X'
               ADD 1 TO WS-DU-CNT-02
           ELSE
               IF ID-SEQ2 = 'C'
                   ADD 1 TO WS-DU-CNT-05
               ELSE
                   IF ID-SEQ2 = 'E'
                       ADD 1 TO WS-DU-CNT-07
                   ELSE
                       ADD 1 TO WS-DU-CNT-12.
       P41400-NORMALISE-TARGET-EXIT.
           EXIT.
       P41500-REPORT-GROUP.
           IF WS-DU-AMT-02 NOT = 0
               COMPUTE WS-DU-QTY-04 = WS-DU-AMT-07 * 100 / WS-DU-AMT-02
           ELSE
               MOVE 0 TO WS-DU-QTY-04.
       P41500-REPORT-GROUP-EXIT.
           EXIT.
       P41600-COMPARE-CLASS.
           MOVE WS-DU-AMT-03 TO WS-DU-AMT-04.
           IF WS-DU-AMT-04 < 0
               COMPUTE WS-DU-AMT-04 = 0 - WS-DU-AMT-03.
       P41600-COMPARE-CLASS-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P41700-SUMMARISE-CODE.
           UNSTRING WS-DU-TXT-01 DELIMITED BY '/'
               INTO WS-DU-TXT-03
               WS-DU-TXT-06
               TALLYING IN WS-DU-CNT-07.
       P41700-SUMMARISE-CODE-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P41800-NORMALISE-OCN.
           MOVE SPACES TO WS-DU-TXT-05.
           STRING ID-LEVEL2 DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               ID-CIRCUIT DELIMITED BY SIZE
               INTO WS-DU-TXT-05.
       P41800-NORMALISE-OCN-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P41900-TRACE-PERIOD.
           MOVE 'Y' TO WS-DU-SW-03.
           IF ID-TARIFF < 20
               MOVE 'N' TO WS-DU-SW-03
               ADD 1 TO WS-DU-CNT-09.
           IF ID-TARIFF > 931
               MOVE 'N' TO WS-DU-SW-03
               ADD 1 TO WS-DU-CNT-06.
       P41900-TRACE-PERIOD-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P42000-TRACE-JURIS.
           IF WS-DU-AMT-02 < 5
               MOVE 5 TO WS-DU-AMT-02
               ADD 1 TO WS-DU-CNT-10.
           IF WS-DU-AMT-02 > 17413
               MOVE 17413 TO WS-DU-AMT-02
               ADD 1 TO WS-DU-CNT-12.
       P42000-TRACE-JURIS-EXIT.
           EXIT.
       P42100-NORMALISE-TARGET.
           MOVE ID-REGION TO WS-DU-TXT-04.
           MOVE ID-LEVEL2 TO WS-DU-TXT-05.
           ADD 1 TO WS-DU-CNT-10.
       P42100-NORMALISE-TARGET-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P42200-RECONCILE-JURIS.
           CALL 'CABHASH' USING WS-DU-TXT-05 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-DU-CNT-04.
       P42200-RECONCILE-JURIS-EXIT.
           EXIT.
       P42300-TRACE-CLASS.
           MOVE 0 TO WS-DU-CNT-06.
           INSPECT WS-DU-TXT-04 TALLYING WS-DU-CNT-06
               FOR ALL SPACES.
           INSPECT WS-DU-TXT-04 REPLACING ALL LOW-VALUES BY SPACES.
       P42300-TRACE-CLASS-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P42400-NORMALISE-MATCH.
           CALL 'CABHASH' USING ID-TARIFF WS-ACC-OCN-HASH.
           ADD WS-DU-CNT-12 TO WS-ACC-SEQ-HASH.
       P42400-NORMALISE-MATCH-EXIT.
           EXIT.
       P42500-ADJUST-STATE.
           ADD ID-LEVEL2 TO WS-DU-QTY-01.
           COMPUTE WS-DU-AMT-03 = WS-DU-QTY-01 * WS-DU-QTY-07.
           ADD WS-DU-AMT-03 TO WS-DU-AMT-05.
       P42500-ADJUST-STATE-EXIT.
           EXIT.
       P42600-COMPARE-CLASS.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-DATE-INVALID TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-DU-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
           MOVE CABS-SUSPENSE-RECORD TO CABS-SUSOUT-RECORD.
           WRITE CABS-SUSOUT-RECORD.
       P42600-COMPARE-CLASS-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P42700-NORMALISE-SOURCE.
           MOVE 'N' TO WS-DU-SW-04.
           IF WS-DU-TXT-05 NOT = WS-DU-TXT-05
               MOVE 'Y' TO WS-DU-SW-04
               MOVE WS-DU-TXT-05 TO WS-DU-TXT-05
               ADD 1 TO WS-DU-CNT-04.
       P42700-NORMALISE-SOURCE-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P42800-TRACE-REFERENCE.
           MOVE SPACES TO CABS-DU-OUT-RECORD.
           MOVE ID-INVOICE TO OD-TARGET.
           MOVE ID-CIRCUIT2 TO OD-BAND.
           MOVE ID-CIRCUIT4 TO OD-BAN.
           MOVE ID-INVOICE TO OD-JURIS.
           MOVE ID-ELEM2 TO OD-TARGET2.
           WRITE CABS-DU-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P42800-TRACE-REFERENCE-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P42900-COMPARE-PERIOD.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-DU-TXT-05 TO PC-COL-001-020.
           MOVE WS-DU-TXT-06 TO PC-COL-021-060.
           MOVE WS-DU-AMT-07 TO WS-DU-AMT-EDIT.
           MOVE WS-DU-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P42900-COMPARE-PERIOD-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P43000-SUMMARISE-BAN.
           MOVE 0 TO WS-DU-QTY-02.
           MOVE 0 TO WS-DU-QTY-04.
           MOVE 0 TO WS-DU-AMT-06.
           MOVE 0 TO WS-DU-AMT-03.
       P43000-SUMMARISE-BAN-EXIT.
           EXIT.
       P43100-REPORT-REFERENCE.
           IF ID-CIRCUIT2 = 'S'
               ADD 1 TO WS-DU-CNT-11
           ELSE
               IF ID-CIRCUIT2 = 'D'
                   ADD 1 TO WS-DU-CNT-11
               ELSE
                   IF ID-CIRCUIT2 = 'A'
                       ADD 1 TO WS-DU-CNT-09
                   ELSE
                       ADD 1 TO WS-DU-CNT-01.
       P43100-REPORT-REFERENCE-EXIT.
           EXIT.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P43200-NORMALISE-TARGET.
           IF WS-DU-AMT-03 NOT = 0
               COMPUTE WS-DU-QTY-02 = WS-DU-AMT-06 * 100 / WS-DU-AMT-03
           ELSE
               MOVE 0 TO WS-DU-QTY-02.
       P43200-NORMALISE-TARGET-EXIT.
           EXIT.
           MOVE 0 TO WS-DU-QTY-04.
           PERFORM P380-WALK-SIDE THRU P380-WALK-SIDE-EXIT
               VARYING WS-DU-SUB-02 FROM 1 BY 1
               UNTIL WS-DU-SUB-02 > WS-DU-TAB-CNT.
       P380-WALK-SIDE.
           SET WS-DU-IX TO WS-DU-SUB-03.
           IF WS-DU-TB-KEY (WS-DU-IX) NOT = SPACES
               ADD WS-DU-TB-VAL (WS-DU-IX) TO WS-DU-QTY-04.
       P380-WALK-SIDE-EXIT.
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
           MOVE WS-READ-CNT TO WS-DU-CNT-EDIT.
           MOVE WS-DU-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL OUT' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-DU-CNT-EDIT.
           MOVE WS-DU-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL SUSPENDED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-DU-CNT-EDIT.
           MOVE WS-DU-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL SUMMARISED' TO PC-COL-001-020.
           MOVE WS-SUMM-CNT TO WS-DU-CNT-EDIT.
           MOVE WS-DU-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL CARRIED FWD' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-DU-CNT-EDIT.
           MOVE WS-DU-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-DU-CNT-01 TO WS-DU-CNT-EDIT.
           MOVE WS-DU-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 02' TO PC-COL-001-020.
           MOVE WS-DU-CNT-02 TO WS-DU-CNT-EDIT.
           MOVE WS-DU-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE WS-DU-CNT-06 TO CT-RC.
           MOVE SPACES TO CT-FILLER.
           MOVE WS-DU-TXT-01 TO CT-RESTART-KEY.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE WS-DU-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE 6 TO CT-STEP-SEQ.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
       P8100-EXIT.
           EXIT.
      * P8200-CHECK-BALANCE - THE REPORT LINES ARE NOT RECORDS, SO THE
      * WRITTEN COUNT IS ZEROED BEFORE THE EQUATION IS TESTED.
       P8200-CHECK-BALANCE.
           MOVE 0 TO CT-WRITTEN.
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
           CLOSE SUSIN.
           CLOSE MTCOUT.
           CLOSE ORPOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABUXR05 - NORMAL END OF JOB'.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  DU-CNT-11 = ' WS-DU-CNT-11.
           DISPLAY '  DU-CNT-09 = ' WS-DU-CNT-09.
       P9000-EXIT.
           EXIT.
