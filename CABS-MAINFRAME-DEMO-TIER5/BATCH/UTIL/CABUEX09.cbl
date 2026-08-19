      *****************************************************************
      * CABUEX09 - FACTOR STUDY EXTRACT                               *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               SELIN   TELCABS.CABS.SELIN          (LOCAL)     *
      *               CIRIN   TELCABS.CABS.CIRIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               CIROUT  TELCABS.CABS.CIROUT         (LOCAL)     *
      *               CAROUT  TELCABS.CABS.CAROUT         (LOCAL)     *
      *               SELOUT  TELCABS.CABS.SELOUT         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - NOTHING IS UPDATED IN PLACE        *
      * REVISION HISTORY                                              *
      *   V1.00  1988-09-09  B.R.HALVORSEN INITIAL RELEASE            *
      *   V1.03  1989-04-13  B.R.HALVORSEN RECOMPILE ONLY - COPYBOOK  *
      *                      CHANGE UPSTREAM                          *
      *   V1.05  1992-06-14  L.FERREIRA   BLOCK SIZE SET TO ZERO -    *
      *                      SYSTEM DETERMINED                        *
      *   V1.08  2009-09-02  P.NAIR       RETIRED THE SECOND SORT STEP*
      *                      - DONE IN PROGRAM                        *
      *   V1.12  2013-11-01  G.PRZYBYLSKI EFFECTIVE DATE FILTER ADDED *
      *                      PER AUDIT FINDING                        *
      *   V1.13  2017-07-27  L.FERREIRA   RECOMPILE ONLY - COPYBOOK   *
      *                      CHANGE UPSTREAM                          *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUEX09.
       AUTHOR. TELCABS APPLICATIONS - DATASET EXTRACT.
      *****************************************************************
      * FACTOR STUDY EXTRACT. THE STEP IS DRIVEN ENTIRELY FROM THE    *
      * SYSIN PARM CARD AND THE DD ALLOCATIONS IN THE JOB.            *
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE     *
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.                      *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           C01 IS TO-NEW-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SELIN ASSIGN TO UT-S-SELIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT CIRIN ASSIGN TO UT-S-CIRIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT CIROUT ASSIGN TO UT-S-CIROUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT CAROUT ASSIGN TO UT-S-CAROUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT SELOUT ASSIGN TO UT-S-SELOUT
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
      * SELIN - CATALOGUED GENERATION DATA GROUP.
       FD  SELIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 140 CHARACTERS.
       01  CABS-DG-IN-RECORD.
           05  ID-STATE                    PIC X(03).
           05  ID-TARGET                   PIC S9(13)V9(05) COMP-3.
           05  ID-PERIOD                   PIC 9(02).
           05  ID-BAND                     PIC S9(13)V9(02) COMP-3.
           05  ID-BAND2                    PIC S9(07)V9(02) COMP-3.
           05  ID-SEGMENT                  PIC S9(13)V9(05) COMP-3.
           05  ID-CIRCUIT                  PIC X(06).
           05  ID-OCN                      PIC X(16).
           05  ID-CIRCUIT2                 PIC X(10).
           05  ID-TARGET2                  PIC 9(03).
           05  ID-ACCOUNT                  PIC X(10).
           05  ID-REGION                   PIC X(08).
           05  ID-TARGET3                  PIC X(06).
           05  ID-STATE2                   PIC 9(02).
           05  ID-BAND3                    PIC X(03).
           05  ID-CENTRE                   PIC X(08).
           05  ID-CIRCUIT3                 PIC S9(13) COMP-3.
           05  ID-JURIS                    PIC 9(06).
           05  ID-PERIOD2                  PIC 9(03).
           05  ID-TYPE                     PIC S9(11) COMP-3.
           05  ID-CYCLE                    PIC X(04).
           05  DG-FILL-01                  PIC X(4).
      * CIRIN - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  CIRIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 140 CHARACTERS.
       01  CABS-DG-ALT1-RECORD.
           05  A1-PERIOD                   PIC S9(13) COMP-3.
           05  A1-PERIOD2                  PIC 9(06).
           05  A1-LEVEL                    PIC S9(09)V9(02) COMP-3.
           05  A1-ACCOUNT                  PIC S9(05) COMP-3.
           05  A1-INVOICE                  PIC X(02).
           05  A1-OCN                      PIC S9(09)V9(02) COMP-3.
           05  A1-TARGET                   PIC S9(15) COMP-3.
           05  A1-CARRIER                  PIC S9(07) COMP-3.
           05  DG-FILL-02                  PIC X(98).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-DG-VIEW1 REDEFINES CABS-DG-IN-RECORD.
           05  R0D-CARRIER                 PIC 9(06).
           05  R0D-ELEM                    PIC X(02).
           05  R0D-MEDIA                   PIC 9(02).
           05  R0D-STATUS                  PIC S9(11) COMP-3.
           05  R0D-OCN                     PIC X(08).
           05  R0D-CENTRE                  PIC S9(09)V9(05) COMP-3.
           05  R0D-BAND                    PIC X(08).
           05  R0D-REST                    PIC X(100).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-DG-VIEW2 REDEFINES CABS-DG-IN-RECORD.
           05  R1D-OCN                     PIC X(03).
           05  R1D-BAND                    PIC S9(09)V9(05) COMP-3.
           05  R1D-GROUP                   PIC X(06).
           05  R1D-CIRCUIT                 PIC 9(02).
           05  R1D-REGION                  PIC X(08).
           05  R1D-SOURCE                  PIC X(02).
           05  R1D-CODE                    PIC 9(02).
           05  R1D-SEGMENT                 PIC S9(09) COMP-3.
           05  R1D-REST                    PIC X(104).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-DG-VIEW3 REDEFINES CABS-DG-IN-RECORD.
           05  R2D-CLASS                   PIC S9(05) COMP-3.
           05  R2D-BAND                    PIC X(06).
           05  R2D-INVOICE                 PIC X(13).
           05  R2D-JURIS                   PIC 9(05).
           05  R2D-SEQ                     PIC 9(04).
           05  R2D-REST                    PIC X(109).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-DG-VIEW4 REDEFINES CABS-DG-IN-RECORD.
           05  R3D-CYCLE                   PIC S9(13) COMP-3.
           05  R3D-GROUP                   PIC X(13).
           05  R3D-JURIS                   PIC X(06).
           05  R3D-CIRCUIT                 PIC X(08).
           05  R3D-BAND                    PIC X(06).
           05  R3D-SEGMENT                 PIC 9(05).
           05  R3D-TARIFF                  PIC S9(09)V9(02) COMP-3.
           05  R3D-CIRCUIT2                PIC X(04).
           05  R3D-BAN                     PIC S9(13)V9(02) COMP-3.
           05  R3D-REST                    PIC X(77).
      * CIROUT - PERMANENT DATASET HELD ON DASD.
       FD  CIROUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 160 CHARACTERS.
       01  CABS-DG-OUT-RECORD.
           05  OD-REGION                   PIC 9(09).
           05  OD-SEGMENT                  PIC S9(05) COMP-3.
           05  OD-ELEM                     PIC X(03).
           05  OD-CLASS                    PIC S9(07) COMP-3.
           05  OD-JURIS                    PIC 9(02).
           05  OD-JURIS2                   PIC S9(09) COMP-3.
           05  OD-GROUP                    PIC X(20).
           05  OD-STATE                    PIC X(08).
           05  OD-BAN                      PIC X(20).
           05  OD-SEGMENT2                 PIC 9(02).
           05  OD-PERIOD                   PIC S9(09) COMP-3.
           05  OD-CIRCUIT                  PIC S9(13)V9(02) COMP-3.
           05  OD-STATE2                   PIC X(02).
           05  OD-INVOICE                  PIC X(04).
           05  OD-SEGMENT3                 PIC 9(06).
           05  OD-CODE                     PIC S9(13)V9(02) COMP-3.
           05  OD-TARGET                   PIC S9(09) COMP-3.
           05  OD-CENTRE                   PIC X(20).
           05  OD-PERIOD2                  PIC 9(05).
           05  OD-TYPE                     PIC X(13).
           05  DG-FILL-03                  PIC X(8).
      * CAROUT - PERMANENT DATASET HELD ON DASD.
       FD  CAROUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 160 CHARACTERS.
       01  CABS-DG-OUT1-RECORD         PIC X(160).
      * SELOUT - WORK FILE, DELETED AT STEP END.
       FD  SELOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 160 CHARACTERS.
       01  CABS-DG-OUT2-RECORD         PIC X(160).
      * SUSOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  SUSOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 300 CHARACTERS.
       01  CABS-SUSOUT-RECORD              PIC X(300).
      * CTLOUT - SEQUENTIAL, BLOCKED BY THE SYSTEM.
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
      * SHARED LAYOUT PULLED IN FOR THE EXTRACT SIDE.
       COPY CABSFCTR.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUEX09'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V2.16'.
           05  WS-TABLE-LIMIT              PIC S9(04) COMP-3
                                         VALUE 500.
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
           05  WS-DG-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DG-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DG-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DG-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DG-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DG-CNT-06                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DG-CNT-07                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DG-CNT-08                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-DG-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DG-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DG-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DG-QTY-04                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DG-QTY-05                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DG-QTY-06                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-DG-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DG-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DG-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DG-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DG-AMT-05                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DG-AMT-06                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DG-AMT-07                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DG-AMT-08                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-DG-TXT-01                PIC X(16) VALUE SPACES.
           05  WS-DG-TXT-02                PIC X(20) VALUE SPACES.
           05  WS-DG-TXT-03                PIC X(30) VALUE SPACES.
           05  WS-DG-TXT-04                PIC X(12) VALUE SPACES.
           05  WS-DG-TXT-05                PIC X(08) VALUE SPACES.
           05  WS-DG-TXT-06                PIC X(08) VALUE SPACES.
           05  WS-DG-TXT-07                PIC X(12) VALUE SPACES.
           05  WS-DG-TXT-08                PIC X(30) VALUE SPACES.
           05  WS-DG-TXT-09                PIC X(20) VALUE SPACES.
           05  WS-DG-TXT-10                PIC X(26) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-DG-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-DG-ON-01                 VALUE 'Y'.
               88  WS-DG-OFF-01                VALUE 'N'.
           05  WS-DG-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-DG-ON-02                 VALUE 'Y'.
               88  WS-DG-OFF-02                VALUE 'N'.
           05  WS-DG-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-DG-ON-03                 VALUE 'Y'.
               88  WS-DG-OFF-03                VALUE 'N'.
           05  WS-DG-SW-04                 PIC X(01) VALUE 'N'.
               88  WS-DG-ON-04                 VALUE 'Y'.
               88  WS-DG-OFF-04                VALUE 'N'.
           05  WS-DG-SW-05                 PIC X(01) VALUE 'N'.
               88  WS-DG-ON-05                 VALUE 'Y'.
               88  WS-DG-OFF-05                VALUE 'N'.
           05  WS-DG-SW-06                 PIC X(01) VALUE 'N'.
               88  WS-DG-ON-06                 VALUE 'Y'.
               88  WS-DG-OFF-06                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-DG-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DG-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DG-SUB-03                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DG-SUB-04                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DG-SUB-05                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. LOADED ONCE AT INITIALISATION AND SEARCHED WITH
      * A SUBSCRIPTED WALK.
       01  WS-DG-TABLE.
           05  WS-DG-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-DG-TB-ENTRY OCCURS 500 TIMES
                                       INDEXED BY WS-DG-IX.
               10  WS-DG-TB-KEY                PIC X(04).
               10  WS-DG-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-DG-TB-TXT                PIC X(40).
               10  WS-DG-TB-EFF                PIC 9(05).
               10  WS-DG-TB-EXP                PIC 9(05).
       01  WS-DG-WORK-GROUP-1.
           05  WS-DG-G1-SEQ                PIC X(20).
           05  WS-DG-G1-SEGMENT            PIC S9(11)V9(02) COMP-3.
           05  WS-DG-G1-CODE               PIC X(10).
           05  WS-DG-G1-MEDIA              PIC X(10).
       01  WS-DG-WORK-GROUP-2.
           05  WS-DG-G2-CLASS              PIC X(10).
           05  WS-DG-G2-REGION             PIC 9(05).
           05  WS-DG-G2-OCN                PIC S9(09) COMP-3.
           05  WS-DG-G2-TARIFF             PIC 9(05).
           05  WS-DG-G2-OCN                PIC 9(05).
           05  WS-DG-G2-CARRIER            PIC 9(05).
           05  WS-DG-G2-CENTRE             PIC 9(07).
       01  WS-DG-WORK-GROUP-3.
           05  WS-DG-G3-SEGMENT            PIC X(20).
           05  WS-DG-G3-SEGMENT            PIC X(10).
           05  WS-DG-G3-PERIOD             PIC S9(11)V9(02) COMP-3.
       01  WS-DG-WORK-GROUP-4.
           05  WS-DG-G4-TARIFF             PIC X(10).
           05  WS-DG-G4-CODE               PIC 9(07).
           05  WS-DG-G4-CIRCUIT            PIC 9(07).
           05  WS-DG-G4-CIRCUIT            PIC X(20).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 58.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUEX09 - FACTOR STUDY EXTRACT'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-DG-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-DG-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9960.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-DG-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-DG-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT SELIN.
           IF WS-FS-INPUT NOT = '00'
               DISPLAY 'SELIN FILE STATUS = ' WS-FS-INPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF SELIN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN INPUT CIRIN.
           IF WS-FS-INPUT NOT = '00'
               DISPLAY 'CIRIN FILE STATUS = ' WS-FS-INPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF CIRIN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CIROUT.
           IF WS-FS-OUTPUT NOT = '00'
               DISPLAY 'CIROUT FILE STATUS = ' WS-FS-OUTPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF CIROUT' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CAROUT.
           IF WS-FS-OUTPUT NOT = '00'
               DISPLAY 'CAROUT FILE STATUS = ' WS-FS-OUTPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF CAROUT' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SELOUT.
           IF WS-FS-OUTPUT NOT = '00'
               DISPLAY 'SELOUT FILE STATUS = ' WS-FS-OUTPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF SELOUT' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SUSOUT.
           IF WS-FS-SUSPENSE NOT = '00'
               DISPLAY 'SUSOUT FILE STATUS = ' WS-FS-SUSPENSE
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF SUSOUT' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               DISPLAY 'CTLOUT FILE STATUS = ' WS-FS-CONTROL
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF CTLOUT' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RPTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               DISPLAY 'RPTOUT FILE STATUS = ' WS-FS-OUTPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BAD FILE STATUS ON OPEN OF RPTOUT' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-DG-CYCLE-YYDDD.
           COMPUTE WS-DG-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-DG-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-DG-CNT-02.
           MOVE 0 TO WS-DG-CNT-05.
           MOVE 0 TO WS-DG-CNT-08.
       P1200-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-DG-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-DG-TAB-CNT NOT < 500
               MOVE 'Y' TO WS-DG-SW-01
               ADD 1 TO WS-DG-CNT-08
           ELSE
               ADD 1 TO WS-DG-TAB-CNT
               SET WS-DG-IX TO WS-DG-TAB-CNT
               MOVE ID-PERIOD TO WS-DG-TB-KEY (WS-DG-IX)
               MOVE 0 TO WS-DG-TB-VAL (WS-DG-IX)
               MOVE SPACES TO WS-DG-TB-TXT (WS-DG-IX)
               ADD 1 TO WS-DG-CNT-01.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ SELIN
               AT END MOVE 'Y' TO WS-DG-SW-01.
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
           PERFORM P2200-CONVERT-CANDIDATE THRU
               P2200-CONVERT-CANDIDATE-EXIT.
           PERFORM P2300-EXPAND-CANDIDATE THRU
               P2300-EXPAND-CANDIDATE-EXIT.
           PERFORM P2400-SELECT-SUBSET THRU P2400-SELECT-SUBSET-EXIT.
           PERFORM P2500-VALIDATE-SUBSET THRU
               P2500-VALIDATE-SUBSET-EXIT.
           PERFORM P2600-CHECK-CANDIDATE THRU
               P2600-CHECK-CANDIDATE-EXIT.
           PERFORM P2700-EXPAND-CANDIDATE THRU
               P2700-EXPAND-CANDIDATE-EXIT.
           PERFORM P2800-BUILD-SUBSET THRU P2800-BUILD-SUBSET-EXIT.
           IF WS-DG-ON-06
               PERFORM P2900-APPLY-SELECTION THRU
                   P2900-APPLY-SELECTION-EXIT.
           IF WS-DG-ON-05
               PERFORM P21000-EXPAND-RANGE THRU
                   P21000-EXPAND-RANGE-EXIT.
           PERFORM P21100-CHECK-FILTER THRU P21100-CHECK-FILTER-EXIT.
           PERFORM P21200-CONVERT-RANGE THRU P21200-CONVERT-RANGE-EXIT.
           PERFORM P21300-CHECK-MASTER THRU P21300-CHECK-MASTER-EXIT.
           IF WS-DG-ON-06
               PERFORM P21400-BUILD-CANDIDATE THRU
                   P21400-BUILD-CANDIDATE-EXIT.
           PERFORM P21500-EXPAND-FILTER THRU P21500-EXPAND-FILTER-EXIT.
           PERFORM P21600-EXPAND-CANDIDATE THRU
               P21600-EXPAND-CANDIDATE-EXIT.
           PERFORM P21700-SPLIT-SELECTION THRU
               P21700-SPLIT-SELECTION-EXIT.
           PERFORM P21800-RESOLVE-CANDIDATE THRU
               P21800-RESOLVE-CANDIDATE-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ SELIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P2200-CONVERT-CANDIDATE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-DG-TXT-09 TO PC-COL-001-020.
           MOVE WS-DG-TXT-02 TO PC-COL-021-060.
           MOVE WS-DG-AMT-01 TO WS-DG-AMT-EDIT.
           MOVE WS-DG-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
           MOVE 'N' TO WS-DG-SW-04.
           IF WS-DG-TAB-CNT > 0
               PERFORM P260-COMPARE-SELECTION THRU
                   P260-COMPARE-SELECTION-EXIT
               VARYING WS-DG-SUB-05 FROM 1 BY 1
               UNTIL WS-DG-SUB-05 > WS-DG-TAB-CNT
               OR WS-DG-SW-04 = 'Y'.
       P2200-CONVERT-CANDIDATE-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P2300-EXPAND-CANDIDATE.
           MOVE WS-DG-AMT-08 TO WS-DG-AMT-01.
           IF WS-DG-AMT-01 < 0
               COMPUTE WS-DG-AMT-01 = 0 - WS-DG-AMT-08.
       P2300-EXPAND-CANDIDATE-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P2400-SELECT-SUBSET.
           CALL 'CABHASH' USING ID-CENTRE WS-ACC-OCN-HASH.
           ADD WS-DG-CNT-04 TO WS-ACC-SEQ-HASH.
       P2400-SELECT-SUBSET-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P2500-VALIDATE-SUBSET.
           IF WS-DG-AMT-07 NOT = 0
               COMPUTE WS-DG-QTY-03 = WS-DG-AMT-07 * 100 / WS-DG-AMT-07
           ELSE
               MOVE 0 TO WS-DG-QTY-03.
       P2500-VALIDATE-SUBSET-EXIT.
           EXIT.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P2600-CHECK-CANDIDATE.
           IF ID-STATE = 'C'
               ADD 1 TO WS-DG-CNT-08
           ELSE
               IF ID-STATE = 'E'
                   ADD 1 TO WS-DG-CNT-06
               ELSE
                   IF ID-STATE = 'C'
                       ADD 1 TO WS-DG-CNT-01
                   ELSE
                       ADD 1 TO WS-DG-CNT-03.
       P2600-CHECK-CANDIDATE-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P2700-EXPAND-CANDIDATE.
           MOVE SPACES TO WS-DG-TXT-01.
           STRING ID-JURIS DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               ID-BAND DELIMITED BY SIZE
               INTO WS-DG-TXT-01.
       P2700-EXPAND-CANDIDATE-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P2800-BUILD-SUBSET.
           MOVE 0 TO WS-DG-QTY-05.
           MOVE 0 TO WS-DG-QTY-06.
           MOVE 0 TO WS-DG-QTY-01.
           MOVE 0 TO WS-DG-AMT-06.
           MOVE 0 TO WS-DG-AMT-02.
       P2800-BUILD-SUBSET-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P2900-APPLY-SELECTION.
           ADD ID-TARGET TO WS-DG-QTY-01.
           COMPUTE WS-DG-AMT-04 ROUNDED = WS-DG-QTY-01 * WS-DG-QTY-06.
           ADD WS-DG-AMT-04 TO WS-DG-AMT-06.
       P2900-APPLY-SELECTION-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P21000-EXPAND-RANGE.
           MOVE 0 TO WS-DG-CNT-05.
           INSPECT WS-DG-TXT-01 TALLYING WS-DG-CNT-05
               FOR ALL SPACES.
           INSPECT WS-DG-TXT-01 REPLACING ALL LOW-VALUES BY SPACES.
       P21000-EXPAND-RANGE-EXIT.
           EXIT.
       P21100-CHECK-FILTER.
           MOVE 'Y' TO WS-DG-SW-04.
           IF ID-CIRCUIT3 < 21
               MOVE 'N' TO WS-DG-SW-04
               ADD 1 TO WS-DG-CNT-03.
           IF ID-CIRCUIT3 > 4808
               MOVE 'N' TO WS-DG-SW-04
               ADD 1 TO WS-DG-CNT-05.
       P21100-CHECK-FILTER-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P21200-CONVERT-RANGE.
           CALL 'CABSEQCK' USING WS-DG-TXT-07 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-DG-CNT-02.
       P21200-CONVERT-RANGE-EXIT.
           EXIT.
       P21300-CHECK-MASTER.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-DATE-INVALID TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-DG-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
       P21300-CHECK-MASTER-EXIT.
           EXIT.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P21400-BUILD-CANDIDATE.
           MOVE 'N' TO WS-DG-SW-05.
           IF WS-DG-TXT-06 NOT = WS-DG-TXT-03
               MOVE 'Y' TO WS-DG-SW-05
               MOVE WS-DG-TXT-06 TO WS-DG-TXT-03
               ADD 1 TO WS-DG-CNT-01.
       P21400-BUILD-CANDIDATE-EXIT.
           EXIT.
       P21500-EXPAND-FILTER.
           MOVE SPACES TO CABS-DG-OUT-RECORD.
           MOVE ID-TYPE TO OD-REGION.
           MOVE ID-BAND TO OD-SEGMENT.
           MOVE ID-BAND3 TO OD-ELEM.
           MOVE ID-BAND2 TO OD-CLASS.
           MOVE ID-JURIS TO OD-JURIS.
           MOVE ID-BAND3 TO OD-JURIS2.
           WRITE CABS-DG-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P21500-EXPAND-FILTER-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P21600-EXPAND-CANDIDATE.
           MOVE ID-OCN TO WS-DG-TXT-06.
           MOVE ID-BAND2 TO WS-DG-TXT-08.
           MOVE ID-PERIOD TO WS-DG-TXT-09.
           ADD 1 TO WS-DG-CNT-03.
       P21600-EXPAND-CANDIDATE-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P21700-SPLIT-SELECTION.
           IF WS-DG-AMT-07 < 16
               MOVE 16 TO WS-DG-AMT-07
               ADD 1 TO WS-DG-CNT-06.
           IF WS-DG-AMT-07 > 54902
               MOVE 54902 TO WS-DG-AMT-07
               ADD 1 TO WS-DG-CNT-02.
       P21700-SPLIT-SELECTION-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P21800-RESOLVE-CANDIDATE.
           UNSTRING WS-DG-TXT-10 DELIMITED BY '/'
               INTO WS-DG-TXT-09
               WS-DG-TXT-01
               TALLYING IN WS-DG-CNT-04.
       P21800-RESOLVE-CANDIDATE-EXIT.
           EXIT.
       P260-COMPARE-SELECTION.
           SET WS-DG-IX TO WS-DG-SUB-05.
           IF WS-DG-TB-KEY (WS-DG-IX) = ID-CIRCUIT
               MOVE 'Y' TO WS-DG-SW-05
               MOVE WS-DG-TB-VAL (WS-DG-IX) TO WS-DG-QTY-06
               MOVE WS-DG-SUB-05 TO WS-DG-SUB-01.
       P260-COMPARE-SELECTION-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P3100-RELEASE-SELECTION.
           ADD ID-SEGMENT TO WS-DG-QTY-01.
           COMPUTE WS-DG-AMT-06 = WS-DG-QTY-01 * WS-DG-QTY-04.
           ADD WS-DG-AMT-06 TO WS-DG-AMT-03.
       P3100-RELEASE-SELECTION-EXIT.
           EXIT.
       P3200-CLOSE-OFF-SUBSET.
           MOVE ID-STATE2 TO WS-DG-TXT-01.
           MOVE ID-BAND3 TO WS-DG-TXT-05.
           MOVE ID-ACCOUNT TO WS-DG-TXT-02.
           ADD 1 TO WS-DG-CNT-01.
       P3200-CLOSE-OFF-SUBSET-EXIT.
           EXIT.
       P3300-STAGE-SUBSET.
           MOVE SPACES TO CABS-DG-OUT-RECORD.
           MOVE ID-PERIOD TO OD-REGION.
           MOVE ID-TARGET TO OD-SEGMENT.
           MOVE ID-BAND3 TO OD-ELEM.
           MOVE ID-OCN TO OD-CLASS.
           MOVE ID-REGION TO OD-JURIS.
           MOVE ID-STATE TO OD-JURIS2.
           MOVE ID-CENTRE TO OD-GROUP.
           WRITE CABS-DG-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P3300-STAGE-SUBSET-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P3400-POST-EXTRACT.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-BAN-UNKNOWN TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-DG-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
           MOVE CABS-SUSPENSE-RECORD TO CABS-SUSOUT-RECORD.
           WRITE CABS-SUSOUT-RECORD.
       P3400-POST-EXTRACT-EXIT.
           EXIT.
       P3500-EMIT-MASTER.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-DG-TXT-10 TO PC-COL-001-020.
           MOVE WS-DG-TXT-10 TO PC-COL-021-060.
           MOVE WS-DG-AMT-03 TO WS-DG-AMT-EDIT.
           MOVE WS-DG-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P3500-EMIT-MASTER-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P3600-CLOSE-OFF-RANGE.
           MOVE 0 TO WS-DG-QTY-04.
           MOVE 0 TO WS-DG-QTY-03.
           MOVE 0 TO WS-DG-AMT-07.
           MOVE 0 TO WS-DG-AMT-01.
       P3600-CLOSE-OFF-RANGE-EXIT.
           EXIT.
      * S400-SECONDARY-PROCESSING SECTION
       S400-SECONDARY-PROCESSING SECTION.
       P4000-SECONDARY.
           PERFORM P4100-TRACE-STATE THRU P4100-TRACE-STATE-EXIT.
           PERFORM P4200-COMPARE-SOURCE THRU P4200-COMPARE-SOURCE-EXIT.
           PERFORM P4300-AUDIT-RANGE THRU P4300-AUDIT-RANGE-EXIT.
           PERFORM P4400-SUMMARISE-CIRCUIT THRU
               P4400-SUMMARISE-CIRCUIT-EXIT.
           PERFORM P4500-ADJUST-SOURCE THRU P4500-ADJUST-SOURCE-EXIT.
           PERFORM P4600-COMPARE-JURIS THRU P4600-COMPARE-JURIS-EXIT.
           PERFORM P4700-TRACE-SEQ THRU P4700-TRACE-SEQ-EXIT.
           PERFORM P4800-RECONCILE-LEVEL THRU
               P4800-RECONCILE-LEVEL-EXIT.
           PERFORM P4900-TRACE-MASTER THRU P4900-TRACE-MASTER-EXIT.
           PERFORM P41000-ADJUST-MEDIA THRU P41000-ADJUST-MEDIA-EXIT.
           PERFORM P41100-SUMMARISE-STATE THRU
               P41100-SUMMARISE-STATE-EXIT.
           PERFORM P41200-TRACE-TARIFF THRU P41200-TRACE-TARIFF-EXIT.
           PERFORM P41300-RECONCILE-OCN THRU P41300-RECONCILE-OCN-EXIT.
           PERFORM P41400-ADJUST-CANDIDATE THRU
               P41400-ADJUST-CANDIDATE-EXIT.
           PERFORM P41500-NORMALISE-MASTER THRU
               P41500-NORMALISE-MASTER-EXIT.
           PERFORM P41600-SUMMARISE-CIRCUIT THRU
               P41600-SUMMARISE-CIRCUIT-EXIT.
           PERFORM P41700-COMPARE-CYCLE THRU P41700-COMPARE-CYCLE-EXIT.
           PERFORM P41800-AUDIT-JURIS THRU P41800-AUDIT-JURIS-EXIT.
           PERFORM P41900-TRACE-SOURCE THRU P41900-TRACE-SOURCE-EXIT.
           PERFORM P42000-RECONCILE-CENTRE THRU
               P42000-RECONCILE-CENTRE-EXIT.
           PERFORM P42100-SUMMARISE-TARIFF THRU
               P42100-SUMMARISE-TARIFF-EXIT.
           PERFORM P42200-SUMMARISE-INVOICE THRU
               P42200-SUMMARISE-INVOICE-EXIT.
           PERFORM P42300-RECONCILE-TARIFF THRU
               P42300-RECONCILE-TARIFF-EXIT.
           PERFORM P42400-REPORT-CARRIER THRU
               P42400-REPORT-CARRIER-EXIT.
           PERFORM P42500-REPORT-SELECTION THRU
               P42500-REPORT-SELECTION-EXIT.
           PERFORM P42600-RECONCILE-SEGMENT THRU
               P42600-RECONCILE-SEGMENT-EXIT.
           PERFORM P42700-REPORT-TARGET THRU P42700-REPORT-TARGET-EXIT.
           PERFORM P42800-COMPARE-STATUS THRU
               P42800-COMPARE-STATUS-EXIT.
           PERFORM P42900-REPORT-JURIS THRU P42900-REPORT-JURIS-EXIT.
           PERFORM P43000-RECONCILE-TARIFF THRU
               P43000-RECONCILE-TARIFF-EXIT.
           PERFORM P43100-AUDIT-GROUP THRU P43100-AUDIT-GROUP-EXIT.
           PERFORM P43200-SUMMARISE-FILTER THRU
               P43200-SUMMARISE-FILTER-EXIT.
           PERFORM P43300-REPORT-BAND THRU P43300-REPORT-BAND-EXIT.
       P4000-EXIT.
           EXIT.
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT
      * PRECEDES THIS PROGRAM IN THE JOB.
       P4100-TRACE-STATE.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-BAN-UNKNOWN TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-DG-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
           MOVE CABS-SUSPENSE-RECORD TO CABS-SUSOUT-RECORD.
           WRITE CABS-SUSOUT-RECORD.
       P4100-TRACE-STATE-EXIT.
           EXIT.
       P4200-COMPARE-SOURCE.
           ADD ID-BAND TO WS-DG-QTY-03.
           COMPUTE WS-DG-AMT-02 ROUNDED = WS-DG-QTY-03 * WS-DG-QTY-01.
           ADD WS-DG-AMT-02 TO WS-DG-AMT-02.
       P4200-COMPARE-SOURCE-EXIT.
           EXIT.
       P4300-AUDIT-RANGE.
           CALL 'CABEDITF' USING WS-DG-TXT-05 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-DG-CNT-07.
       P4300-AUDIT-RANGE-EXIT.
           EXIT.
       P4400-SUMMARISE-CIRCUIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-DG-TXT-04 TO PC-COL-001-020.
           MOVE WS-DG-TXT-10 TO PC-COL-021-060.
           MOVE WS-DG-AMT-03 TO WS-DG-AMT-EDIT.
           MOVE WS-DG-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P4400-SUMMARISE-CIRCUIT-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P4500-ADJUST-SOURCE.
           MOVE 0 TO WS-DG-QTY-06.
           MOVE 0 TO WS-DG-QTY-03.
           MOVE 0 TO WS-DG-QTY-02.
           MOVE 0 TO WS-DG-AMT-04.
       P4500-ADJUST-SOURCE-EXIT.
           EXIT.
       P4600-COMPARE-JURIS.
           UNSTRING WS-DG-TXT-09 DELIMITED BY '/'
               INTO WS-DG-TXT-04
               WS-DG-TXT-03
               TALLYING IN WS-DG-CNT-08.
       P4600-COMPARE-JURIS-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P4700-TRACE-SEQ.
           IF WS-DG-AMT-01 NOT = 0
               COMPUTE WS-DG-QTY-02 = WS-DG-AMT-08 * 100 / WS-DG-AMT-01
           ELSE
               MOVE 0 TO WS-DG-QTY-02.
       P4700-TRACE-SEQ-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P4800-RECONCILE-LEVEL.
           IF WS-DG-AMT-03 < 20
               MOVE 20 TO WS-DG-AMT-03
               ADD 1 TO WS-DG-CNT-03.
           IF WS-DG-AMT-03 > 41197
               MOVE 41197 TO WS-DG-AMT-03
               ADD 1 TO WS-DG-CNT-02.
       P4800-RECONCILE-LEVEL-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P4900-TRACE-MASTER.
           CALL 'CABHASH' USING ID-BAND3 WS-ACC-OCN-HASH.
           ADD WS-DG-CNT-05 TO WS-ACC-SEQ-HASH.
       P4900-TRACE-MASTER-EXIT.
           EXIT.
       P41000-ADJUST-MEDIA.
           MOVE SPACES TO CABS-DG-OUT-RECORD.
           MOVE ID-CIRCUIT TO OD-REGION.
           MOVE ID-CIRCUIT2 TO OD-SEGMENT.
           MOVE ID-CIRCUIT3 TO OD-ELEM.
           MOVE ID-CIRCUIT3 TO OD-CLASS.
           MOVE ID-STATE2 TO OD-JURIS.
           MOVE ID-STATE2 TO OD-JURIS2.
           MOVE ID-TARGET TO OD-GROUP.
           MOVE ID-TARGET2 TO OD-STATE.
           WRITE CABS-DG-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P41000-ADJUST-MEDIA-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P41100-SUMMARISE-STATE.
           MOVE 0 TO WS-DG-CNT-02.
           INSPECT WS-DG-TXT-04 TALLYING WS-DG-CNT-02
               FOR ALL SPACES.
           INSPECT WS-DG-TXT-04 REPLACING ALL LOW-VALUES BY SPACES.
       P41100-SUMMARISE-STATE-EXIT.
           EXIT.
       P41200-TRACE-TARIFF.
           MOVE 'N' TO WS-DG-SW-05.
           IF WS-DG-TXT-08 NOT = WS-DG-TXT-05
               MOVE 'Y' TO WS-DG-SW-05
               MOVE WS-DG-TXT-08 TO WS-DG-TXT-05
               ADD 1 TO WS-DG-CNT-04.
       P41200-TRACE-TARIFF-EXIT.
           EXIT.
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT
      * PRECEDES THIS PROGRAM IN THE JOB.
       P41300-RECONCILE-OCN.
           MOVE WS-DG-AMT-08 TO WS-DG-AMT-05.
           IF WS-DG-AMT-05 < 0
               COMPUTE WS-DG-AMT-05 = 0 - WS-DG-AMT-08.
       P41300-RECONCILE-OCN-EXIT.
           EXIT.
       P41400-ADJUST-CANDIDATE.
           MOVE 'Y' TO WS-DG-SW-02.
           IF ID-BAND2 < 2
               MOVE 'N' TO WS-DG-SW-02
               ADD 1 TO WS-DG-CNT-06.
           IF ID-BAND2 > 3578
               MOVE 'N' TO WS-DG-SW-02
               ADD 1 TO WS-DG-CNT-07.
       P41400-ADJUST-CANDIDATE-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P41500-NORMALISE-MASTER.
           IF ID-STATE2 = 'X'
               ADD 1 TO WS-DG-CNT-07
           ELSE
               IF ID-STATE2 = 'E'
                   ADD 1 TO WS-DG-CNT-01
               ELSE
                   IF ID-STATE2 = 'X'
                       ADD 1 TO WS-DG-CNT-01
                   ELSE
                       ADD 1 TO WS-DG-CNT-01.
       P41500-NORMALISE-MASTER-EXIT.
           EXIT.
       P41600-SUMMARISE-CIRCUIT.
           MOVE ID-CYCLE TO WS-DG-TXT-02.
           MOVE ID-STATE2 TO WS-DG-TXT-10.
           MOVE ID-CYCLE TO WS-DG-TXT-06.
           ADD 1 TO WS-DG-CNT-03.
       P41600-SUMMARISE-CIRCUIT-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P41700-COMPARE-CYCLE.
           MOVE SPACES TO WS-DG-TXT-09.
           STRING ID-REGION DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               ID-STATE2 DELIMITED BY SIZE
               INTO WS-DG-TXT-09.
       P41700-COMPARE-CYCLE-EXIT.
           EXIT.
       P41800-AUDIT-JURIS.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-BAN-UNKNOWN TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-DG-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
           MOVE CABS-SUSPENSE-RECORD TO CABS-SUSOUT-RECORD.
           WRITE CABS-SUSOUT-RECORD.
       P41800-AUDIT-JURIS-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P41900-TRACE-SOURCE.
           ADD ID-TYPE TO WS-DG-QTY-04.
           COMPUTE WS-DG-AMT-07 = WS-DG-QTY-04 * WS-DG-QTY-06.
           ADD WS-DG-AMT-07 TO WS-DG-AMT-04.
       P41900-TRACE-SOURCE-EXIT.
           EXIT.
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT
      * PRECEDES THIS PROGRAM IN THE JOB.
       P42000-RECONCILE-CENTRE.
           CALL 'CABFMTR' USING WS-DG-TXT-04 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-DG-CNT-07.
       P42000-RECONCILE-CENTRE-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P42100-SUMMARISE-TARIFF.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-DG-TXT-07 TO PC-COL-001-020.
           MOVE WS-DG-TXT-10 TO PC-COL-021-060.
           MOVE WS-DG-AMT-01 TO WS-DG-AMT-EDIT.
           MOVE WS-DG-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P42100-SUMMARISE-TARIFF-EXIT.
           EXIT.
       P42200-SUMMARISE-INVOICE.
           MOVE 0 TO WS-DG-QTY-04.
           MOVE 0 TO WS-DG-QTY-06.
           MOVE 0 TO WS-DG-QTY-05.
           MOVE 0 TO WS-DG-AMT-07.
       P42200-SUMMARISE-INVOICE-EXIT.
           EXIT.
       P42300-RECONCILE-TARIFF.
           UNSTRING WS-DG-TXT-05 DELIMITED BY '/'
               INTO WS-DG-TXT-04
               WS-DG-TXT-04
               TALLYING IN WS-DG-CNT-03.
       P42300-RECONCILE-TARIFF-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P42400-REPORT-CARRIER.
           IF WS-DG-AMT-01 NOT = 0
               COMPUTE WS-DG-QTY-04 = WS-DG-AMT-01 * 100 / WS-DG-AMT-01
           ELSE
               MOVE 0 TO WS-DG-QTY-04.
       P42400-REPORT-CARRIER-EXIT.
           EXIT.
       P42500-REPORT-SELECTION.
           IF WS-DG-AMT-07 < 45
               MOVE 45 TO WS-DG-AMT-07
               ADD 1 TO WS-DG-CNT-03.
           IF WS-DG-AMT-07 > 87319
               MOVE 87319 TO WS-DG-AMT-07
               ADD 1 TO WS-DG-CNT-02.
       P42500-REPORT-SELECTION-EXIT.
           EXIT.
       P42600-RECONCILE-SEGMENT.
           CALL 'CABHASH' USING ID-CIRCUIT3 WS-ACC-OCN-HASH.
           ADD WS-DG-CNT-06 TO WS-ACC-SEQ-HASH.
       P42600-RECONCILE-SEGMENT-EXIT.
           EXIT.
       P42700-REPORT-TARGET.
           MOVE SPACES TO CABS-DG-OUT-RECORD.
           MOVE ID-STATE TO OD-REGION.
           MOVE ID-REGION TO OD-SEGMENT.
           MOVE ID-TARGET2 TO OD-ELEM.
           MOVE ID-BAND TO OD-CLASS.
           MOVE ID-SEGMENT TO OD-JURIS.
           MOVE ID-BAND2 TO OD-JURIS2.
           MOVE ID-PERIOD2 TO OD-GROUP.
           MOVE ID-CIRCUIT3 TO OD-STATE.
           WRITE CABS-DG-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P42700-REPORT-TARGET-EXIT.
           EXIT.
       P42800-COMPARE-STATUS.
           MOVE 0 TO WS-DG-CNT-08.
           INSPECT WS-DG-TXT-03 TALLYING WS-DG-CNT-08
               FOR ALL SPACES.
           INSPECT WS-DG-TXT-03 REPLACING ALL LOW-VALUES BY SPACES.
       P42800-COMPARE-STATUS-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P42900-REPORT-JURIS.
           MOVE 'N' TO WS-DG-SW-05.
           IF WS-DG-TXT-02 NOT = WS-DG-TXT-01
               MOVE 'Y' TO WS-DG-SW-05
               MOVE WS-DG-TXT-02 TO WS-DG-TXT-01
               ADD 1 TO WS-DG-CNT-01.
       P42900-REPORT-JURIS-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P43000-RECONCILE-TARIFF.
           MOVE WS-DG-AMT-02 TO WS-DG-AMT-06.
           IF WS-DG-AMT-06 < 0
               COMPUTE WS-DG-AMT-06 = 0 - WS-DG-AMT-02.
       P43000-RECONCILE-TARIFF-EXIT.
           EXIT.
       P43100-AUDIT-GROUP.
           MOVE 'Y' TO WS-DG-SW-04.
           IF ID-JURIS < 8
               MOVE 'N' TO WS-DG-SW-04
               ADD 1 TO WS-DG-CNT-01.
           IF ID-JURIS > 4524
               MOVE 'N' TO WS-DG-SW-04
               ADD 1 TO WS-DG-CNT-01.
       P43100-AUDIT-GROUP-EXIT.
           EXIT.
       P43200-SUMMARISE-FILTER.
           IF ID-CIRCUIT2 = 'D'
               ADD 1 TO WS-DG-CNT-01
           ELSE
               IF ID-CIRCUIT2 = 'B'
                   ADD 1 TO WS-DG-CNT-03
               ELSE
                   IF ID-CIRCUIT2 = 'C'
                       ADD 1 TO WS-DG-CNT-08
                   ELSE
                       ADD 1 TO WS-DG-CNT-08.
       P43200-SUMMARISE-FILTER-EXIT.
           EXIT.
       P43300-REPORT-BAND.
           MOVE ID-CIRCUIT2 TO WS-DG-TXT-01.
           MOVE ID-CIRCUIT TO WS-DG-TXT-06.
           MOVE ID-STATE TO WS-DG-TXT-10.
           ADD 1 TO WS-DG-CNT-06.
       P43300-REPORT-BAND-EXIT.
           EXIT.
           MOVE 0 TO WS-DG-QTY-03.
           PERFORM P380-WALK-FILTER THRU P380-WALK-FILTER-EXIT
               VARYING WS-DG-SUB-02 FROM 1 BY 1
               UNTIL WS-DG-SUB-02 > WS-DG-TAB-CNT.
       P380-WALK-FILTER.
           SET WS-DG-IX TO WS-DG-SUB-03.
           IF WS-DG-TB-KEY (WS-DG-IX) NOT = SPACES
               ADD WS-DG-TB-VAL (WS-DG-IX) TO WS-DG-QTY-04.
       P380-WALK-FILTER-EXIT.
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
           MOVE 'ROLLED INTO SUMMARY' TO PC-COL-001-020.
           MOVE WS-SUMM-CNT TO WS-DG-CNT-EDIT.
           MOVE WS-DG-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'READ FROM INPUT' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO WS-DG-CNT-EDIT.
           MOVE WS-DG-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'REJECTED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-DG-CNT-EDIT.
           MOVE WS-DG-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'WRITTEN TO OUTPUT' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-DG-CNT-EDIT.
           MOVE WS-DG-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'CARRIED FORWARD' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-DG-CNT-EDIT.
           MOVE WS-DG-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-DG-CNT-01 TO WS-DG-CNT-EDIT.
           MOVE WS-DG-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 02' TO PC-COL-001-020.
           MOVE WS-DG-CNT-02 TO WS-DG-CNT-EDIT.
           MOVE WS-DG-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 03' TO PC-COL-001-020.
           MOVE WS-DG-CNT-03 TO WS-DG-CNT-EDIT.
           MOVE WS-DG-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 04' TO PC-COL-001-020.
           MOVE WS-DG-CNT-04 TO WS-DG-CNT-EDIT.
           MOVE WS-DG-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE 8 TO CT-STEP-SEQ.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE WS-DG-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE SPACES TO CT-FILLER.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
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
           CLOSE SELIN.
           CLOSE CIRIN.
           CLOSE CIROUT.
           CLOSE CAROUT.
           CLOSE SELOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABUEX09 - END OF RUN'.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  DG-CNT-03 = ' WS-DG-CNT-03.
           DISPLAY '  DG-CNT-05 = ' WS-DG-CNT-05.
           DISPLAY '  DG-CNT-06 = ' WS-DG-CNT-06.
           DISPLAY '  DG-CNT-08 = ' WS-DG-CNT-08.
       P9000-EXIT.
           EXIT.
