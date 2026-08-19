      *****************************************************************
      * CABUCV04 - PACKED TO DISPLAY CONVERSION                       *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               IXCIN   TELCABS.CABS.IXCIN          (LOCAL)     *
      *               EMIIN   TELCABS.CABS.EMIIN          (LOCAL)     *
      *               PCKIN   TELCABS.CABS.PCKIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               TGTOUT  TELCABS.CABS.TGTOUT         (LOCAL)     *
      *               UPLOUT  TELCABS.CABS.UPLOUT         (LOCAL)     *
      *               CNVOUT  TELCABS.CABS.CNVOUT         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - NOTHING IS UPDATED IN PLACE        *
      * REVISION HISTORY                                              *
      *   V1.00  1988-10-24  S.MARCHETTI  INITIAL RELEASE             *
      *   V1.03  1992-08-28  L.FERREIRA   CARRIER TYPE BROUGHT ONTO   *
      *                      THE EXTRACT                              *
      *   V1.07  1997-12-07  C.ADEYEMI    SUSPENSE WRITE ADDED -      *
      *                      RECORDS WERE BEING DROPPED               *
      *   V1.08  1998-02-03  S.MARCHETTI  HASH TOTAL ADDED TO THE     *
      *                      CONTROL RECORD                           *
      *   V1.09  2002-07-26  G.PRZYBYLSKI TABLE LIMIT RAISED FOR THE  *
      *                      SOUTHEAST CENTRES                        *
      *   V1.10  2006-12-21  C.ADEYEMI    HASH TOTAL ADDED TO THE     *
      *                      CONTROL RECORD                           *
      *   V1.14  2017-07-21  M.DELACROIX  CARRIER TYPE BROUGHT ONTO   *
      *                      THE EXTRACT                              *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUCV04.
       AUTHOR. TELCABS APPLICATIONS - FILE CONVERSION.
      *****************************************************************
      * PACKED TO DISPLAY CONVERSION. THE STEP RUNS ONCE PER BILL     *
      * CYCLE AND IS RERUN FROM THE TOP IF IT FAILS.                  *
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE  *
      * RESET INSIDE THE LOOP.                                        *
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
           SELECT EMIIN ASSIGN TO UT-S-EMIIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT PCKIN ASSIGN TO UT-S-PCKIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT TGTOUT ASSIGN TO UT-S-TGTOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT UPLOUT ASSIGN TO UT-S-UPLOUT
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
      * IXCIN - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  IXCIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 150 CHARACTERS.
       01  CABS-AZ-IN-RECORD.
           05  IA-CARRIER                  PIC X(04).
           05  IA-CLASS                    PIC X(13).
           05  IA-SEQ                      PIC S9(09)V9(02) COMP-3.
           05  IA-CARRIER2                 PIC 9(04).
           05  IA-ELEM                     PIC S9(13)V9(05) COMP-3.
           05  IA-TYPE                     PIC 9(09).
           05  IA-REGION                   PIC X(03).
           05  IA-SOURCE                   PIC X(20).
           05  IA-SEGMENT                  PIC 9(03).
           05  IA-STATUS                   PIC X(13).
           05  IA-ELEM2                    PIC X(03).
           05  IA-ACCOUNT                  PIC 9(07).
           05  IA-REGION2                  PIC X(10).
           05  IA-CENTRE                   PIC S9(15) COMP-3.
           05  IA-LEVEL                    PIC 9(03).
           05  IA-CENTRE2                  PIC X(10).
           05  IA-INVOICE                  PIC S9(13)V9(05) COMP-3.
           05  IA-REGION3                  PIC X(04).
           05  AZ-FILL-01                  PIC X(10).
      * EMIIN - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  EMIIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 150 CHARACTERS.
       01  CABS-AZ-ALT1-RECORD.
           05  A1-CENTRE                   PIC S9(07)V9(05) COMP-3.
           05  A1-INVOICE                  PIC 9(02).
           05  A1-TARIFF                   PIC 9(06).
           05  A1-SOURCE                   PIC S9(05) COMP-3.
           05  A1-SOURCE2                  PIC X(02).
           05  A1-OCN                      PIC 9(09).
           05  A1-STATE                    PIC 9(02).
           05  A1-CARRIER                  PIC S9(11)V9(02) COMP-3.
           05  A1-CODE                     PIC 9(02).
           05  A1-CYCLE                    PIC X(03).
           05  A1-REGION                   PIC 9(07).
           05  A1-INVOICE2                 PIC S9(11)V9(05) COMP-3.
           05  AZ-FILL-02                  PIC X(91).
      * PCKIN - PERMANENT DATASET HELD ON DASD.
       FD  PCKIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 150 CHARACTERS.
       01  CABS-AZ-ALT2-RECORD.
           05  A2-INVOICE                  PIC S9(13)V9(05) COMP-3.
           05  A2-SEGMENT                  PIC S9(11)V9(02) COMP-3.
           05  A2-REGION                   PIC S9(07) COMP-3.
           05  A2-ELEM                     PIC 9(06).
           05  A2-SEQ                      PIC S9(11) COMP-3.
           05  A2-SEQ2                     PIC X(06).
           05  A2-JURIS                    PIC S9(07) COMP-3.
           05  A2-JURIS2                   PIC S9(13) COMP-3.
           05  A2-SEGMENT2                 PIC 9(03).
           05  A2-BAN                      PIC S9(07)V9(02) COMP-3.
           05  AZ-FILL-03                  PIC X(92).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-AZ-VIEW1 REDEFINES CABS-AZ-IN-RECORD.
           05  R0A-JURIS                   PIC X(03).
           05  R0A-TARIFF                  PIC X(06).
           05  R0A-STATE                   PIC X(16).
           05  R0A-CLASS                   PIC X(20).
           05  R0A-MEDIA                   PIC X(20).
           05  R0A-SOURCE                  PIC X(20).
           05  R0A-CARRIER                 PIC 9(09).
           05  R0A-STATE2                  PIC S9(15) COMP-3.
           05  R0A-CLASS2                  PIC X(08).
           05  R0A-REST                    PIC X(40).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-AZ-VIEW2 REDEFINES CABS-AZ-IN-RECORD.
           05  R1A-SEGMENT                 PIC S9(07)V9(05) COMP-3.
           05  R1A-PERIOD                  PIC S9(09) COMP-3.
           05  R1A-TARGET                  PIC X(06).
           05  R1A-JURIS                   PIC S9(13) COMP-3.
           05  R1A-REST                    PIC X(125).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-AZ-VIEW3 REDEFINES CABS-AZ-IN-RECORD.
           05  R2A-TARIFF                  PIC X(20).
           05  R2A-SEQ                     PIC S9(05) COMP-3.
           05  R2A-SEQ2                    PIC X(04).
           05  R2A-CIRCUIT                 PIC S9(13) COMP-3.
           05  R2A-REST                    PIC X(116).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-AZ-VIEW4 REDEFINES CABS-AZ-IN-RECORD.
           05  R3A-PERIOD                  PIC 9(05).
           05  R3A-SEQ                     PIC S9(11)V9(02) COMP-3.
           05  R3A-MEDIA                   PIC 9(04).
           05  R3A-TYPE                    PIC S9(13)V9(02) COMP-3.
           05  R3A-BAN                     PIC X(13).
           05  R3A-OCN                     PIC S9(11)V9(02) COMP-3.
           05  R3A-SOURCE                  PIC 9(09).
           05  R3A-GROUP                   PIC X(20).
           05  R3A-CODE                    PIC 9(03).
           05  R3A-REST                    PIC X(74).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-AZ-VIEW5 REDEFINES CABS-AZ-IN-RECORD.
           05  R4A-SEQ                     PIC S9(11)V9(02) COMP-3.
           05  R4A-SOURCE                  PIC S9(09)V9(02) COMP-3.
           05  R4A-BAND                    PIC S9(11) COMP-3.
           05  R4A-STATUS                  PIC 9(02).
           05  R4A-INVOICE                 PIC X(10).
           05  R4A-JURIS                   PIC S9(13) COMP-3.
           05  R4A-REGION                  PIC S9(07)V9(02) COMP-3.
           05  R4A-REST                    PIC X(107).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-AZ-VIEW6 REDEFINES CABS-AZ-IN-RECORD.
           05  R5A-SEQ                     PIC X(10).
           05  R5A-CARRIER                 PIC S9(13)V9(05) COMP-3.
           05  R5A-TARGET                  PIC S9(09) COMP-3.
           05  R5A-TARIFF                  PIC X(08).
           05  R5A-CYCLE                   PIC S9(09) COMP-3.
           05  R5A-REGION                  PIC S9(15) COMP-3.
           05  R5A-REGION2                 PIC S9(11) COMP-3.
           05  R5A-BAND                    PIC S9(07)V9(02) COMP-3.
           05  R5A-REST                    PIC X(93).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-AZ-VIEW7 REDEFINES CABS-AZ-IN-RECORD.
           05  R6A-ELEM                    PIC X(04).
           05  R6A-SEQ                     PIC 9(05).
           05  R6A-SOURCE                  PIC X(06).
           05  R6A-CYCLE                   PIC 9(06).
           05  R6A-REST                    PIC X(129).
      * TGTOUT - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  TGTOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 120 CHARACTERS.
       01  CABS-AZ-OUT-RECORD.
           05  OA-CENTRE                   PIC S9(15) COMP-3.
           05  OA-LEVEL                    PIC X(08).
           05  OA-CODE                     PIC 9(02).
           05  OA-JURIS                    PIC S9(13)V9(02) COMP-3.
           05  OA-ELEM                     PIC S9(09)V9(02) COMP-3.
           05  OA-LEVEL2                   PIC X(03).
           05  OA-JURIS2                   PIC 9(03).
           05  OA-GROUP                    PIC 9(02).
           05  OA-BAND                     PIC X(04).
           05  OA-TYPE                     PIC 9(06).
           05  OA-CARRIER                  PIC X(08).
           05  OA-CLASS                    PIC X(04).
           05  OA-SEQ                      PIC 9(04).
           05  OA-REGION                   PIC X(16).
           05  OA-CENTRE2                  PIC S9(07)V9(02) COMP-3.
           05  OA-ACCOUNT                  PIC 9(06).
           05  OA-CLASS2                   PIC X(10).
           05  OA-BAND2                    PIC S9(15) COMP-3.
           05  AZ-FILL-04                  PIC X(9).
      * UPLOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  UPLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 120 CHARACTERS.
       01  CABS-AZ-OUT1-RECORD         PIC X(120).
      * CNVOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  CNVOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 120 CHARACTERS.
       01  CABS-AZ-OUT2-RECORD         PIC X(120).
      * CTLOUT - CATALOGUED GENERATION DATA GROUP.
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD              PIC X(180).
      * RPTOUT - PERMANENT DATASET HELD ON DASD.
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
       COPY CABSCOMM.
      * SHARED LAYOUT PULLED IN FOR THE FIELD SIDE.
       COPY CABSBILL.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUCV04'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V2.30'.
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
           05  WS-AZ-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AZ-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AZ-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AZ-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AZ-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AZ-CNT-06                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AZ-CNT-07                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AZ-CNT-08                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AZ-CNT-09                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AZ-CNT-10                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AZ-CNT-11                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AZ-CNT-12                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-AZ-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AZ-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AZ-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AZ-QTY-04                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AZ-QTY-05                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AZ-QTY-06                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-AZ-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AZ-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AZ-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AZ-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AZ-AMT-05                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AZ-AMT-06                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AZ-AMT-07                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-AZ-TXT-01                PIC X(08) VALUE SPACES.
           05  WS-AZ-TXT-02                PIC X(08) VALUE SPACES.
           05  WS-AZ-TXT-03                PIC X(16) VALUE SPACES.
           05  WS-AZ-TXT-04                PIC X(16) VALUE SPACES.
           05  WS-AZ-TXT-05                PIC X(10) VALUE SPACES.
           05  WS-AZ-TXT-06                PIC X(30) VALUE SPACES.
           05  WS-AZ-TXT-07                PIC X(16) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-AZ-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-AZ-ON-01                 VALUE 'Y'.
               88  WS-AZ-OFF-01                VALUE 'N'.
           05  WS-AZ-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-AZ-ON-02                 VALUE 'Y'.
               88  WS-AZ-OFF-02                VALUE 'N'.
           05  WS-AZ-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-AZ-ON-03                 VALUE 'Y'.
               88  WS-AZ-OFF-03                VALUE 'N'.
           05  WS-AZ-SW-04                 PIC X(01) VALUE 'N'.
               88  WS-AZ-ON-04                 VALUE 'Y'.
               88  WS-AZ-OFF-04                VALUE 'N'.
           05  WS-AZ-SW-05                 PIC X(01) VALUE 'N'.
               88  WS-AZ-ON-05                 VALUE 'Y'.
               88  WS-AZ-OFF-05                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-AZ-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-AZ-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-AZ-SUB-03                PIC S9(04) COMP-3 VALUE 0.
           05  WS-AZ-SUB-04                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. A ROW THAT DOES NOT FIT IS COUNTED AND DROPPED
      * - THE STEP DOES NOT FAIL.
       01  WS-AZ-TABLE.
           05  WS-AZ-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-AZ-TB-ENTRY OCCURS 500 TIMES
                                       INDEXED BY WS-AZ-IX.
               10  WS-AZ-TB-KEY                PIC X(13).
               10  WS-AZ-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-AZ-TB-TXT                PIC X(20).
               10  WS-AZ-TB-EFF                PIC 9(05).
               10  WS-AZ-TB-EXP                PIC 9(05).
       01  WS-AZ-WORK-GROUP-1.
           05  WS-AZ-G1-GROUP              PIC X(20).
           05  WS-AZ-G1-GROUP              PIC X(10).
           05  WS-AZ-G1-CIRCUIT            PIC X(10).
           05  WS-AZ-G1-TYPE               PIC 9(05).
           05  WS-AZ-G1-CARRIER            PIC X(20).
           05  WS-AZ-G1-OCN                PIC X(20).
           05  WS-AZ-G1-CLASS              PIC 9(07).
       01  WS-AZ-WORK-GROUP-2.
           05  WS-AZ-G2-PERIOD             PIC 9(05).
           05  WS-AZ-G2-CARRIER            PIC X(10).
           05  WS-AZ-G2-MEDIA              PIC X(20).
       01  WS-AZ-WORK-GROUP-3.
           05  WS-AZ-G3-TYPE               PIC S9(09) COMP-3.
           05  WS-AZ-G3-CYCLE              PIC X(20).
           05  WS-AZ-G3-CENTRE             PIC X(10).
           05  WS-AZ-G3-SOURCE             PIC X(10).
           05  WS-AZ-G3-STATUS             PIC X(10).
       01  WS-AZ-WORK-GROUP-4.
           05  WS-AZ-G4-BAND               PIC X(10).
           05  WS-AZ-G4-SEQ                PIC 9(07).
           05  WS-AZ-G4-LEVEL              PIC S9(09) COMP-3.
           05  WS-AZ-G4-BAN                PIC S9(11)V9(02) COMP-3.
       01  WS-AZ-WORK-GROUP-5.
           05  WS-AZ-G5-CLASS              PIC X(10).
           05  WS-AZ-G5-STATUS             PIC 9(07).
           05  WS-AZ-G5-TARIFF             PIC 9(07).
           05  WS-AZ-G5-SEQ                PIC 9(07).
           05  WS-AZ-G5-CODE               PIC X(10).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 55.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUCV04 - PACKED TO DISPLAY CONVERSION'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-AZ-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-AZ-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
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
           05  WS-AZ-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-AZ-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
               MOVE 'IXCIN OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN INPUT EMIIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'EMIIN OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN INPUT PCKIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'PCKIN OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT TGTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'TGTOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT UPLOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'UPLOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CNVOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CNVOUT OPEN FAILED - FILE STATUS BAD' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-AZ-CYCLE-YYDDD.
           COMPUTE WS-AZ-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-AZ-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-AZ-CNT-10.
           MOVE 0 TO WS-AZ-CNT-08.
           MOVE 0 TO WS-AZ-CNT-09.
       P1200-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-AZ-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-AZ-TAB-CNT NOT < 500
               MOVE 'Y' TO WS-AZ-SW-01
               ADD 1 TO WS-AZ-CNT-05
           ELSE
               ADD 1 TO WS-AZ-TAB-CNT
               SET WS-AZ-IX TO WS-AZ-TAB-CNT
               MOVE IA-STATUS TO WS-AZ-TB-KEY (WS-AZ-IX)
               MOVE 0 TO WS-AZ-TB-VAL (WS-AZ-IX)
               MOVE SPACES TO WS-AZ-TB-TXT (WS-AZ-IX)
               ADD 1 TO WS-AZ-CNT-05.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ IXCIN
               AT END MOVE 'Y' TO WS-AZ-SW-01.
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
           PERFORM P2200-CONVERT-LAYOUT THRU P2200-CONVERT-LAYOUT-EXIT.
           PERFORM P2300-BUILD-RECORD THRU P2300-BUILD-RECORD-EXIT.
           PERFORM P2400-CHECK-CENTURY THRU P2400-CHECK-CENTURY-EXIT.
           PERFORM P2500-MATCH-PACKED THRU P2500-MATCH-PACKED-EXIT.
           IF WS-AZ-ON-03
               PERFORM P2600-EXPAND-ZONE THRU P2600-EXPAND-ZONE-EXIT.
           PERFORM P2700-SELECT-LAYOUT THRU P2700-SELECT-LAYOUT-EXIT.
           PERFORM P2800-SELECT-PACKED THRU P2800-SELECT-PACKED-EXIT.
           PERFORM P2900-MATCH-FIELD THRU P2900-MATCH-FIELD-EXIT.
           PERFORM P21000-CHECK-ZONE THRU P21000-CHECK-ZONE-EXIT.
           IF WS-AZ-ON-04
               PERFORM P21100-VALIDATE-LAYOUT THRU
                   P21100-VALIDATE-LAYOUT-EXIT.
           IF WS-AZ-ON-03
               PERFORM P21200-APPLY-FIELD THRU P21200-APPLY-FIELD-EXIT.
           PERFORM P21300-EXPAND-FIELD THRU P21300-EXPAND-FIELD-EXIT.
           IF WS-AZ-ON-04
               PERFORM P21400-MATCH-SIGN THRU P21400-MATCH-SIGN-EXIT.
           PERFORM P21500-VALIDATE-CENTURY THRU
               P21500-VALIDATE-CENTURY-EXIT.
           IF WS-AZ-ON-02
               PERFORM P21600-VALIDATE-CENTURY THRU
                   P21600-VALIDATE-CENTURY-EXIT.
           PERFORM P21700-EXPAND-FIELD THRU P21700-EXPAND-FIELD-EXIT.
           PERFORM P21800-MATCH-PACKED THRU P21800-MATCH-PACKED-EXIT.
           PERFORM P21900-RESOLVE-RECORD THRU
               P21900-RESOLVE-RECORD-EXIT.
           IF WS-AZ-ON-02
               PERFORM P22000-SPLIT-ZONE THRU P22000-SPLIT-ZONE-EXIT.
           PERFORM P22100-VALIDATE-FIELD THRU
               P22100-VALIDATE-FIELD-EXIT.
           PERFORM P22200-RESOLVE-SIGN THRU P22200-RESOLVE-SIGN-EXIT.
           IF WS-AZ-ON-02
               PERFORM P22300-VALIDATE-CENTURY THRU
                   P22300-VALIDATE-CENTURY-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ IXCIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-CONVERT-LAYOUT.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-BAN-UNKNOWN TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-AZ-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
           MOVE 'N' TO WS-AZ-SW-05.
           IF WS-AZ-TAB-CNT > 0
               PERFORM P250-COMPARE-PACKED THRU P250-COMPARE-PACKED-EXIT
               VARYING WS-AZ-SUB-02 FROM 1 BY 1
               UNTIL WS-AZ-SUB-02 > WS-AZ-TAB-CNT
               OR WS-AZ-SW-05 = 'Y'.
       P2200-CONVERT-LAYOUT-EXIT.
           EXIT.
       P2300-BUILD-RECORD.
           ADD IA-CENTRE TO WS-AZ-QTY-05.
           COMPUTE WS-AZ-AMT-04 = WS-AZ-QTY-05 * WS-AZ-QTY-05.
           ADD WS-AZ-AMT-04 TO WS-AZ-AMT-04.
       P2300-BUILD-RECORD-EXIT.
           EXIT.
       P2400-CHECK-CENTURY.
           MOVE 0 TO WS-AZ-CNT-11.
           INSPECT WS-AZ-TXT-05 TALLYING WS-AZ-CNT-11
               FOR ALL SPACES.
           INSPECT WS-AZ-TXT-05 REPLACING ALL LOW-VALUES BY SPACES.
       P2400-CHECK-CENTURY-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P2500-MATCH-PACKED.
           MOVE IA-SOURCE TO WS-AZ-TXT-04.
           MOVE IA-SOURCE TO WS-AZ-TXT-04.
           MOVE IA-CENTRE TO WS-AZ-TXT-07.
           ADD 1 TO WS-AZ-CNT-11.
       P2500-MATCH-PACKED-EXIT.
           EXIT.
      * A FAILURE HERE SUSPENDS THE RECORD, IT DOES NOT STOP THE STEP.
       P2600-EXPAND-ZONE.
           CALL 'CABEDITF' USING WS-AZ-TXT-03 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-AZ-CNT-03.
       P2600-EXPAND-ZONE-EXIT.
           EXIT.
       P2700-SELECT-LAYOUT.
           IF WS-AZ-AMT-04 NOT = 0
               COMPUTE WS-AZ-QTY-06 = WS-AZ-AMT-02 * 100 / WS-AZ-AMT-04
           ELSE
               MOVE 0 TO WS-AZ-QTY-06.
       P2700-SELECT-LAYOUT-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P2800-SELECT-PACKED.
           MOVE SPACES TO WS-AZ-TXT-05.
           STRING IA-CARRIER DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IA-ACCOUNT DELIMITED BY SIZE
               INTO WS-AZ-TXT-05.
       P2800-SELECT-PACKED-EXIT.
           EXIT.
       P2900-MATCH-FIELD.
           MOVE 'Y' TO WS-AZ-SW-02.
           IF IA-LEVEL < 7
               MOVE 'N' TO WS-AZ-SW-02
               ADD 1 TO WS-AZ-CNT-07.
           IF IA-LEVEL > 63
               MOVE 'N' TO WS-AZ-SW-02
               ADD 1 TO WS-AZ-CNT-01.
       P2900-MATCH-FIELD-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P21000-CHECK-ZONE.
           MOVE WS-AZ-AMT-06 TO WS-AZ-AMT-01.
           IF WS-AZ-AMT-01 < 0
               COMPUTE WS-AZ-AMT-01 = 0 - WS-AZ-AMT-06.
       P21000-CHECK-ZONE-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P21100-VALIDATE-LAYOUT.
           IF IA-ELEM2 = 'C'
               ADD 1 TO WS-AZ-CNT-06
           ELSE
               IF IA-ELEM2 = 'B'
                   ADD 1 TO WS-AZ-CNT-07
               ELSE
                   IF IA-ELEM2 = 'S'
                       ADD 1 TO WS-AZ-CNT-04
                   ELSE
                       ADD 1 TO WS-AZ-CNT-05.
       P21100-VALIDATE-LAYOUT-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P21200-APPLY-FIELD.
           UNSTRING WS-AZ-TXT-06 DELIMITED BY '/'
               INTO WS-AZ-TXT-05
               WS-AZ-TXT-03
               TALLYING IN WS-AZ-CNT-04.
       P21200-APPLY-FIELD-EXIT.
           EXIT.
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT
      * PRECEDES THIS PROGRAM IN THE JOB.
       P21300-EXPAND-FIELD.
           CALL 'CABHASH' USING IA-REGION3 WS-ACC-OCN-HASH.
           ADD WS-AZ-CNT-08 TO WS-ACC-SEQ-HASH.
       P21300-EXPAND-FIELD-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P21400-MATCH-SIGN.
           MOVE SPACES TO CABS-AZ-OUT-RECORD.
           MOVE IA-ACCOUNT TO OA-CENTRE.
           MOVE IA-ELEM TO OA-LEVEL.
           MOVE IA-SEQ TO OA-CODE.
           MOVE IA-SEGMENT TO OA-JURIS.
           MOVE IA-REGION TO OA-ELEM.
           MOVE IA-CENTRE2 TO OA-LEVEL2.
           MOVE IA-CARRIER TO OA-JURIS2.
           MOVE IA-CLASS TO OA-GROUP.
           WRITE CABS-AZ-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P21400-MATCH-SIGN-EXIT.
           EXIT.
       P21500-VALIDATE-CENTURY.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-AZ-TXT-01 TO PC-COL-001-020.
           MOVE WS-AZ-TXT-05 TO PC-COL-021-060.
           MOVE WS-AZ-AMT-03 TO WS-AZ-AMT-EDIT.
           MOVE WS-AZ-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P21500-VALIDATE-CENTURY-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P21600-VALIDATE-CENTURY.
           MOVE 0 TO WS-AZ-QTY-03.
           MOVE 0 TO WS-AZ-QTY-04.
           MOVE 0 TO WS-AZ-QTY-02.
           MOVE 0 TO WS-AZ-AMT-02.
       P21600-VALIDATE-CENTURY-EXIT.
           EXIT.
       P21700-EXPAND-FIELD.
           IF WS-AZ-AMT-04 < 9
               MOVE 9 TO WS-AZ-AMT-04
               ADD 1 TO WS-AZ-CNT-03.
           IF WS-AZ-AMT-04 > 12125
               MOVE 12125 TO WS-AZ-AMT-04
               ADD 1 TO WS-AZ-CNT-02.
       P21700-EXPAND-FIELD-EXIT.
           EXIT.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P21800-MATCH-PACKED.
           MOVE 'N' TO WS-AZ-SW-01.
           IF WS-AZ-TXT-04 NOT = WS-AZ-TXT-03
               MOVE 'Y' TO WS-AZ-SW-01
               MOVE WS-AZ-TXT-04 TO WS-AZ-TXT-03
               ADD 1 TO WS-AZ-CNT-11.
       P21800-MATCH-PACKED-EXIT.
           EXIT.
       P21900-RESOLVE-RECORD.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-DATE-INVALID TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-AZ-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
       P21900-RESOLVE-RECORD-EXIT.
           EXIT.
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT
      * PRECEDES THIS PROGRAM IN THE JOB.
       P22000-SPLIT-ZONE.
           ADD IA-CENTRE TO WS-AZ-QTY-03.
           COMPUTE WS-AZ-AMT-02 = WS-AZ-QTY-03 * WS-AZ-QTY-05.
           ADD WS-AZ-AMT-02 TO WS-AZ-AMT-05.
       P22000-SPLIT-ZONE-EXIT.
           EXIT.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P22100-VALIDATE-FIELD.
           MOVE 0 TO WS-AZ-CNT-10.
           INSPECT WS-AZ-TXT-03 TALLYING WS-AZ-CNT-10
               FOR ALL SPACES.
           INSPECT WS-AZ-TXT-03 REPLACING ALL LOW-VALUES BY SPACES.
       P22100-VALIDATE-FIELD-EXIT.
           EXIT.
       P22200-RESOLVE-SIGN.
           MOVE IA-REGION2 TO WS-AZ-TXT-04.
           MOVE IA-REGION TO WS-AZ-TXT-07.
           MOVE IA-CARRIER TO WS-AZ-TXT-07.
           ADD 1 TO WS-AZ-CNT-09.
       P22200-RESOLVE-SIGN-EXIT.
           EXIT.
       P22300-VALIDATE-CENTURY.
           CALL 'CABSEQCK' USING WS-AZ-TXT-03 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-AZ-CNT-11.
       P22300-VALIDATE-CENTURY-EXIT.
           EXIT.
       P250-COMPARE-PACKED.
           SET WS-AZ-IX TO WS-AZ-SUB-01.
           IF WS-AZ-TB-KEY (WS-AZ-IX) = IA-CARRIER2
               MOVE 'Y' TO WS-AZ-SW-03
               MOVE WS-AZ-TB-VAL (WS-AZ-IX) TO WS-AZ-QTY-02
               MOVE WS-AZ-SUB-01 TO WS-AZ-SUB-04.
       P250-COMPARE-PACKED-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-STAGE-RECORD.
           MOVE SPACES TO CABS-AZ-OUT-RECORD.
           MOVE IA-ELEM TO OA-CENTRE.
           MOVE IA-INVOICE TO OA-LEVEL.
           MOVE IA-SOURCE TO OA-CODE.
           MOVE IA-CARRIER TO OA-JURIS.
           MOVE IA-CARRIER TO OA-ELEM.
           MOVE IA-TYPE TO OA-LEVEL2.
           MOVE IA-STATUS TO OA-JURIS2.
           MOVE IA-REGION TO OA-GROUP.
           MOVE IA-CARRIER2 TO OA-BAND.
           WRITE CABS-AZ-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P3100-STAGE-RECORD-EXIT.
           EXIT.
       P3200-CLOSE-OFF-FIELD.
           CALL 'CABHASH' USING IA-REGION WS-ACC-OCN-HASH.
           ADD WS-AZ-CNT-02 TO WS-ACC-SEQ-HASH.
       P3200-CLOSE-OFF-FIELD-EXIT.
           EXIT.
       P3300-FORMAT-RECORD.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-AZ-TXT-07 TO PC-COL-001-020.
           MOVE WS-AZ-TXT-04 TO PC-COL-021-060.
           MOVE WS-AZ-AMT-07 TO WS-AZ-AMT-EDIT.
           MOVE WS-AZ-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P3300-FORMAT-RECORD-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P3400-CLOSE-OFF-LAYOUT.
           ADD IA-SEQ TO WS-AZ-QTY-02.
           COMPUTE WS-AZ-AMT-06 = WS-AZ-QTY-02 * WS-AZ-QTY-06.
           ADD WS-AZ-AMT-06 TO WS-AZ-AMT-05.
       P3400-CLOSE-OFF-LAYOUT-EXIT.
           EXIT.
       P3500-POST-LAYOUT.
           MOVE 0 TO WS-AZ-QTY-04.
           MOVE 0 TO WS-AZ-QTY-02.
           MOVE 0 TO WS-AZ-QTY-03.
           MOVE 0 TO WS-AZ-AMT-04.
           MOVE 0 TO WS-AZ-AMT-06.
       P3500-POST-LAYOUT-EXIT.
           EXIT.
       P3600-CLOSE-OFF-RECORD.
           MOVE IA-TYPE TO WS-AZ-TXT-02.
           MOVE IA-CARRIER2 TO WS-AZ-TXT-04.
           MOVE IA-INVOICE TO WS-AZ-TXT-05.
           ADD 1 TO WS-AZ-CNT-07.
       P3600-CLOSE-OFF-RECORD-EXIT.
           EXIT.
      * S400-SECONDARY-PROCESSING SECTION
       S400-SECONDARY-PROCESSING SECTION.
       P4000-SECONDARY.
           PERFORM P4100-AUDIT-BAND THRU P4100-AUDIT-BAND-EXIT.
           PERFORM P4200-SUMMARISE-TYPE THRU P4200-SUMMARISE-TYPE-EXIT.
           PERFORM P4300-TRACE-TYPE THRU P4300-TRACE-TYPE-EXIT.
           PERFORM P4400-COMPARE-SEGMENT THRU
               P4400-COMPARE-SEGMENT-EXIT.
           PERFORM P4500-REPORT-SEQ THRU P4500-REPORT-SEQ-EXIT.
           PERFORM P4600-REPORT-STATE THRU P4600-REPORT-STATE-EXIT.
           PERFORM P4700-NORMALISE-CLASS THRU
               P4700-NORMALISE-CLASS-EXIT.
           PERFORM P4800-REPORT-ELEM THRU P4800-REPORT-ELEM-EXIT.
           PERFORM P4900-SUMMARISE-LAYOUT THRU
               P4900-SUMMARISE-LAYOUT-EXIT.
           PERFORM P41000-TRACE-BAN THRU P41000-TRACE-BAN-EXIT.
           PERFORM P41100-COMPARE-STATE THRU P41100-COMPARE-STATE-EXIT.
           PERFORM P41200-ADJUST-STATE THRU P41200-ADJUST-STATE-EXIT.
           PERFORM P41300-ADJUST-PERIOD THRU P41300-ADJUST-PERIOD-EXIT.
           PERFORM P41400-ADJUST-CARRIER THRU
               P41400-ADJUST-CARRIER-EXIT.
           PERFORM P41500-SUMMARISE-MEDIA THRU
               P41500-SUMMARISE-MEDIA-EXIT.
           PERFORM P41600-COMPARE-SEGMENT THRU
               P41600-COMPARE-SEGMENT-EXIT.
           PERFORM P41700-ADJUST-SEGMENT THRU
               P41700-ADJUST-SEGMENT-EXIT.
           PERFORM P41800-COMPARE-STATE THRU P41800-COMPARE-STATE-EXIT.
           PERFORM P41900-RECONCILE-PERIOD THRU
               P41900-RECONCILE-PERIOD-EXIT.
           PERFORM P42000-RECONCILE-TARIFF THRU
               P42000-RECONCILE-TARIFF-EXIT.
           PERFORM P42100-SUMMARISE-SEQ THRU P42100-SUMMARISE-SEQ-EXIT.
           PERFORM P42200-ADJUST-CYCLE THRU P42200-ADJUST-CYCLE-EXIT.
           PERFORM P42300-AUDIT-REGION THRU P42300-AUDIT-REGION-EXIT.
           PERFORM P42400-ADJUST-BAN THRU P42400-ADJUST-BAN-EXIT.
           PERFORM P42500-COMPARE-MEDIA THRU P42500-COMPARE-MEDIA-EXIT.
           PERFORM P42600-TRACE-CYCLE THRU P42600-TRACE-CYCLE-EXIT.
           PERFORM P42700-AUDIT-CENTRE THRU P42700-AUDIT-CENTRE-EXIT.
           PERFORM P42800-TRACE-CYCLE THRU P42800-TRACE-CYCLE-EXIT.
           PERFORM P42900-ADJUST-SIGN THRU P42900-ADJUST-SIGN-EXIT.
           PERFORM P43000-RECONCILE-RECORD THRU
               P43000-RECONCILE-RECORD-EXIT.
           PERFORM P43100-AUDIT-STATE THRU P43100-AUDIT-STATE-EXIT.
           PERFORM P43200-RECONCILE-RECORD THRU
               P43200-RECONCILE-RECORD-EXIT.
           PERFORM P43300-ADJUST-BAN THRU P43300-ADJUST-BAN-EXIT.
           PERFORM P43400-SUMMARISE-LEVEL THRU
               P43400-SUMMARISE-LEVEL-EXIT.
           PERFORM P43500-AUDIT-ELEM THRU P43500-AUDIT-ELEM-EXIT.
           PERFORM P43600-RECONCILE-CYCLE THRU
               P43600-RECONCILE-CYCLE-EXIT.
           PERFORM P43700-NORMALISE-RECORD THRU
               P43700-NORMALISE-RECORD-EXIT.
       P4000-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P4100-AUDIT-BAND.
           IF WS-AZ-AMT-04 < 8
               MOVE 8 TO WS-AZ-AMT-04
               ADD 1 TO WS-AZ-CNT-09.
           IF WS-AZ-AMT-04 > 50350
               MOVE 50350 TO WS-AZ-AMT-04
               ADD 1 TO WS-AZ-CNT-01.
       P4100-AUDIT-BAND-EXIT.
           EXIT.
       P4200-SUMMARISE-TYPE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-AZ-TXT-04 TO PC-COL-001-020.
           MOVE WS-AZ-TXT-06 TO PC-COL-021-060.
           MOVE WS-AZ-AMT-05 TO WS-AZ-AMT-EDIT.
           MOVE WS-AZ-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P4200-SUMMARISE-TYPE-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P4300-TRACE-TYPE.
           IF IA-LEVEL = 'C'
               ADD 1 TO WS-AZ-CNT-10
           ELSE
               IF IA-LEVEL = 'A'
                   ADD 1 TO WS-AZ-CNT-09
               ELSE
                   IF IA-LEVEL = 'C'
                       ADD 1 TO WS-AZ-CNT-03
                   ELSE
                       ADD 1 TO WS-AZ-CNT-09.
       P4300-TRACE-TYPE-EXIT.
           EXIT.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P4400-COMPARE-SEGMENT.
           UNSTRING WS-AZ-TXT-03 DELIMITED BY '/'
               INTO WS-AZ-TXT-03
               WS-AZ-TXT-05
               TALLYING IN WS-AZ-CNT-10.
       P4400-COMPARE-SEGMENT-EXIT.
           EXIT.
       P4500-REPORT-SEQ.
           ADD IA-ELEM TO WS-AZ-QTY-05.
           COMPUTE WS-AZ-AMT-02 ROUNDED = WS-AZ-QTY-05 * WS-AZ-QTY-06.
           ADD WS-AZ-AMT-02 TO WS-AZ-AMT-01.
       P4500-REPORT-SEQ-EXIT.
           EXIT.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P4600-REPORT-STATE.
           MOVE 0 TO WS-AZ-QTY-04.
           MOVE 0 TO WS-AZ-QTY-05.
           MOVE 0 TO WS-AZ-QTY-06.
           MOVE 0 TO WS-AZ-AMT-03.
           MOVE 0 TO WS-AZ-AMT-01.
       P4600-REPORT-STATE-EXIT.
           EXIT.
       P4700-NORMALISE-CLASS.
           CALL 'CABEDITF' USING WS-AZ-TXT-05 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-AZ-CNT-04.
       P4700-NORMALISE-CLASS-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P4800-REPORT-ELEM.
           CALL 'CABHASH' USING IA-ACCOUNT WS-ACC-OCN-HASH.
           ADD WS-AZ-CNT-02 TO WS-ACC-SEQ-HASH.
       P4800-REPORT-ELEM-EXIT.
           EXIT.
       P4900-SUMMARISE-LAYOUT.
           IF WS-AZ-AMT-06 NOT = 0
               COMPUTE WS-AZ-QTY-06 = WS-AZ-AMT-06 * 100 / WS-AZ-AMT-06
           ELSE
               MOVE 0 TO WS-AZ-QTY-06.
       P4900-SUMMARISE-LAYOUT-EXIT.
           EXIT.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P41000-TRACE-BAN.
           MOVE WS-AZ-AMT-02 TO WS-AZ-AMT-04.
           IF WS-AZ-AMT-04 < 0
               COMPUTE WS-AZ-AMT-04 = 0 - WS-AZ-AMT-02.
       P41000-TRACE-BAN-EXIT.
           EXIT.
       P41100-COMPARE-STATE.
           MOVE IA-ELEM2 TO WS-AZ-TXT-07.
           MOVE IA-CARRIER2 TO WS-AZ-TXT-03.
           ADD 1 TO WS-AZ-CNT-12.
       P41100-COMPARE-STATE-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P41200-ADJUST-STATE.
           IF WS-AZ-AMT-01 < 29
               MOVE 29 TO WS-AZ-AMT-01
               ADD 1 TO WS-AZ-CNT-04.
           IF WS-AZ-AMT-01 > 40184
               MOVE 40184 TO WS-AZ-AMT-01
               ADD 1 TO WS-AZ-CNT-01.
       P41200-ADJUST-STATE-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P41300-ADJUST-PERIOD.
           MOVE SPACES TO WS-AZ-TXT-03.
           STRING IA-SEQ DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IA-SEQ DELIMITED BY SIZE
               INTO WS-AZ-TXT-03.
       P41300-ADJUST-PERIOD-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P41400-ADJUST-CARRIER.
           MOVE SPACES TO CABS-AZ-OUT-RECORD.
           MOVE IA-INVOICE TO OA-CENTRE.
           MOVE IA-REGION TO OA-LEVEL.
           MOVE IA-ELEM2 TO OA-CODE.
           MOVE IA-ELEM2 TO OA-JURIS.
           MOVE IA-SOURCE TO OA-ELEM.
           MOVE IA-SOURCE TO OA-LEVEL2.
           MOVE IA-ELEM TO OA-JURIS2.
           WRITE CABS-AZ-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P41400-ADJUST-CARRIER-EXIT.
           EXIT.
       P41500-SUMMARISE-MEDIA.
           MOVE 'N' TO WS-AZ-SW-04.
           IF WS-AZ-TXT-06 NOT = WS-AZ-TXT-02
               MOVE 'Y' TO WS-AZ-SW-04
               MOVE WS-AZ-TXT-06 TO WS-AZ-TXT-02
               ADD 1 TO WS-AZ-CNT-08.
       P41500-SUMMARISE-MEDIA-EXIT.
           EXIT.
       P41600-COMPARE-SEGMENT.
           MOVE 0 TO WS-AZ-CNT-02.
           INSPECT WS-AZ-TXT-06 TALLYING WS-AZ-CNT-02
               FOR ALL SPACES.
           INSPECT WS-AZ-TXT-06 REPLACING ALL LOW-VALUES BY SPACES.
       P41600-COMPARE-SEGMENT-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P41700-ADJUST-SEGMENT.
           MOVE 'Y' TO WS-AZ-SW-03.
           IF IA-LEVEL < 14
               MOVE 'N' TO WS-AZ-SW-03
               ADD 1 TO WS-AZ-CNT-06.
           IF IA-LEVEL > 285
               MOVE 'N' TO WS-AZ-SW-03
               ADD 1 TO WS-AZ-CNT-03.
       P41700-ADJUST-SEGMENT-EXIT.
           EXIT.
       P41800-COMPARE-STATE.
           IF WS-AZ-AMT-03 < 33
               MOVE 33 TO WS-AZ-AMT-03
               ADD 1 TO WS-AZ-CNT-07.
           IF WS-AZ-AMT-03 > 75600
               MOVE 75600 TO WS-AZ-AMT-03
               ADD 1 TO WS-AZ-CNT-02.
       P41800-COMPARE-STATE-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P41900-RECONCILE-PERIOD.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-AZ-TXT-02 TO PC-COL-001-020.
           MOVE WS-AZ-TXT-01 TO PC-COL-021-060.
           MOVE WS-AZ-AMT-06 TO WS-AZ-AMT-EDIT.
           MOVE WS-AZ-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P41900-RECONCILE-PERIOD-EXIT.
           EXIT.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P42000-RECONCILE-TARIFF.
           IF IA-SEQ = 'S'
               ADD 1 TO WS-AZ-CNT-12
           ELSE
               IF IA-SEQ = 'C'
                   ADD 1 TO WS-AZ-CNT-11
               ELSE
                   IF IA-SEQ = 'C'
                       ADD 1 TO WS-AZ-CNT-01
                   ELSE
                       ADD 1 TO WS-AZ-CNT-12.
       P42000-RECONCILE-TARIFF-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P42100-SUMMARISE-SEQ.
           UNSTRING WS-AZ-TXT-03 DELIMITED BY '/'
               INTO WS-AZ-TXT-03
               WS-AZ-TXT-05
               TALLYING IN WS-AZ-CNT-07.
       P42100-SUMMARISE-SEQ-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P42200-ADJUST-CYCLE.
           ADD IA-SEQ TO WS-AZ-QTY-01.
           COMPUTE WS-AZ-AMT-01 ROUNDED = WS-AZ-QTY-01 * WS-AZ-QTY-03.
           ADD WS-AZ-AMT-01 TO WS-AZ-AMT-04.
       P42200-ADJUST-CYCLE-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P42300-AUDIT-REGION.
           MOVE 0 TO WS-AZ-QTY-04.
           MOVE 0 TO WS-AZ-QTY-06.
           MOVE 0 TO WS-AZ-AMT-07.
       P42300-AUDIT-REGION-EXIT.
           EXIT.
       P42400-ADJUST-BAN.
           CALL 'CABCTLWR' USING WS-AZ-TXT-06 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-AZ-CNT-08.
       P42400-ADJUST-BAN-EXIT.
           EXIT.
       P42500-COMPARE-MEDIA.
           CALL 'CABHASH' USING IA-ACCOUNT WS-ACC-OCN-HASH.
           ADD WS-AZ-CNT-07 TO WS-ACC-SEQ-HASH.
       P42500-COMPARE-MEDIA-EXIT.
           EXIT.
       P42600-TRACE-CYCLE.
           IF WS-AZ-AMT-02 NOT = 0
               COMPUTE WS-AZ-QTY-01 = WS-AZ-AMT-07 * 100 / WS-AZ-AMT-02
           ELSE
               MOVE 0 TO WS-AZ-QTY-01.
       P42600-TRACE-CYCLE-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P42700-AUDIT-CENTRE.
           MOVE WS-AZ-AMT-01 TO WS-AZ-AMT-03.
           IF WS-AZ-AMT-03 < 0
               COMPUTE WS-AZ-AMT-03 = 0 - WS-AZ-AMT-01.
       P42700-AUDIT-CENTRE-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P42800-TRACE-CYCLE.
           MOVE IA-SOURCE TO WS-AZ-TXT-05.
           MOVE IA-CENTRE2 TO WS-AZ-TXT-01.
           ADD 1 TO WS-AZ-CNT-03.
       P42800-TRACE-CYCLE-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P42900-ADJUST-SIGN.
           IF WS-AZ-AMT-04 < 26
               MOVE 26 TO WS-AZ-AMT-04
               ADD 1 TO WS-AZ-CNT-12.
           IF WS-AZ-AMT-04 > 72280
               MOVE 72280 TO WS-AZ-AMT-04
               ADD 1 TO WS-AZ-CNT-04.
       P42900-ADJUST-SIGN-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P43000-RECONCILE-RECORD.
           MOVE SPACES TO WS-AZ-TXT-04.
           STRING IA-SOURCE DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IA-REGION DELIMITED BY SIZE
               INTO WS-AZ-TXT-04.
       P43000-RECONCILE-RECORD-EXIT.
           EXIT.
       P43100-AUDIT-STATE.
           MOVE SPACES TO CABS-AZ-OUT-RECORD.
           MOVE IA-SOURCE TO OA-CENTRE.
           MOVE IA-SOURCE TO OA-LEVEL.
           MOVE IA-CLASS TO OA-CODE.
           MOVE IA-CENTRE2 TO OA-JURIS.
           MOVE IA-SEGMENT TO OA-ELEM.
           MOVE IA-REGION2 TO OA-LEVEL2.
           WRITE CABS-AZ-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P43100-AUDIT-STATE-EXIT.
           EXIT.
       P43200-RECONCILE-RECORD.
           MOVE 'N' TO WS-AZ-SW-04.
           IF WS-AZ-TXT-01 NOT = WS-AZ-TXT-07
               MOVE 'Y' TO WS-AZ-SW-04
               MOVE WS-AZ-TXT-01 TO WS-AZ-TXT-07
               ADD 1 TO WS-AZ-CNT-03.
       P43200-RECONCILE-RECORD-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P43300-ADJUST-BAN.
           MOVE 0 TO WS-AZ-CNT-01.
           INSPECT WS-AZ-TXT-06 TALLYING WS-AZ-CNT-01
               FOR ALL SPACES.
           INSPECT WS-AZ-TXT-06 REPLACING ALL LOW-VALUES BY SPACES.
       P43300-ADJUST-BAN-EXIT.
           EXIT.
       P43400-SUMMARISE-LEVEL.
           MOVE 'Y' TO WS-AZ-SW-03.
           IF IA-CENTRE < 32
               MOVE 'N' TO WS-AZ-SW-03
               ADD 1 TO WS-AZ-CNT-07.
           IF IA-CENTRE > 6251
               MOVE 'N' TO WS-AZ-SW-03
               ADD 1 TO WS-AZ-CNT-05.
       P43400-SUMMARISE-LEVEL-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P43500-AUDIT-ELEM.
           IF WS-AZ-AMT-06 < 17
               MOVE 17 TO WS-AZ-AMT-06
               ADD 1 TO WS-AZ-CNT-02.
           IF WS-AZ-AMT-06 > 80802
               MOVE 80802 TO WS-AZ-AMT-06
               ADD 1 TO WS-AZ-CNT-08.
       P43500-AUDIT-ELEM-EXIT.
           EXIT.
       P43600-RECONCILE-CYCLE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-AZ-TXT-01 TO PC-COL-001-020.
           MOVE WS-AZ-TXT-06 TO PC-COL-021-060.
           MOVE WS-AZ-AMT-02 TO WS-AZ-AMT-EDIT.
           MOVE WS-AZ-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P43600-RECONCILE-CYCLE-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P43700-NORMALISE-RECORD.
           IF IA-SOURCE = 'X'
               ADD 1 TO WS-AZ-CNT-08
           ELSE
               IF IA-SOURCE = 'B'
                   ADD 1 TO WS-AZ-CNT-08
               ELSE
                   IF IA-SOURCE = 'D'
                       ADD 1 TO WS-AZ-CNT-01
                   ELSE
                       ADD 1 TO WS-AZ-CNT-09.
       P43700-NORMALISE-RECORD-EXIT.
           EXIT.
           MOVE 0 TO WS-AZ-QTY-04.
           PERFORM P380-WALK-LAYOUT THRU P380-WALK-LAYOUT-EXIT
               VARYING WS-AZ-SUB-01 FROM 1 BY 1
               UNTIL WS-AZ-SUB-01 > WS-AZ-TAB-CNT.
       P380-WALK-LAYOUT.
           SET WS-AZ-IX TO WS-AZ-SUB-01.
           IF WS-AZ-TB-KEY (WS-AZ-IX) NOT = SPACES
               ADD WS-AZ-TB-VAL (WS-AZ-IX) TO WS-AZ-QTY-05.
       P380-WALK-LAYOUT-EXIT.
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
           MOVE 'DETAIL SUMMARISED' TO PC-COL-001-020.
           MOVE WS-SUMM-CNT TO WS-AZ-CNT-EDIT.
           MOVE WS-AZ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL SUSPENDED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-AZ-CNT-EDIT.
           MOVE WS-AZ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL IN' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO WS-AZ-CNT-EDIT.
           MOVE WS-AZ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL OUT' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-AZ-CNT-EDIT.
           MOVE WS-AZ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL CARRIED FWD' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-AZ-CNT-EDIT.
           MOVE WS-AZ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-AZ-CNT-01 TO WS-AZ-CNT-EDIT.
           MOVE WS-AZ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 02' TO PC-COL-001-020.
           MOVE WS-AZ-CNT-02 TO WS-AZ-CNT-EDIT.
           MOVE WS-AZ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 03' TO PC-COL-001-020.
           MOVE WS-AZ-CNT-03 TO WS-AZ-CNT-EDIT.
           MOVE WS-AZ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 04' TO PC-COL-001-020.
           MOVE WS-AZ-CNT-04 TO WS-AZ-CNT-EDIT.
           MOVE WS-AZ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-FILLER.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE WS-AZ-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE 3 TO CT-STEP-SEQ.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
       P8100-EXIT.
           EXIT.
      * P8200-CHECK-BALANCE - A RECORD THAT WAS HELD FOR THE NEXT
      * CYCLE IS BROUGHT INTO THE CARRIED FORWARD SIDE BEFORE THE
      * EQUATION IS TESTED.
       P8200-CHECK-BALANCE.
           ADD WS-AZ-CNT-01 TO CT-CARRIED-FWD.
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
           CLOSE EMIIN.
           CLOSE PCKIN.
           CLOSE TGTOUT.
           CLOSE UPLOUT.
           CLOSE CNVOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABUCV04 - STEP COMPLETE'.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  AZ-CNT-04 = ' WS-AZ-CNT-04.
           DISPLAY '  AZ-CNT-11 = ' WS-AZ-CNT-11.
           DISPLAY '  AZ-CNT-05 = ' WS-AZ-CNT-05.
       P9000-EXIT.
           EXIT.
