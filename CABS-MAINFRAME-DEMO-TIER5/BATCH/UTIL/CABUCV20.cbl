      *****************************************************************
      * CABUCV20 - INTERCHANGE FORMAT CONVERSION                      *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               IXCIN   TELCABS.CABS.IXCIN          (LOCAL)     *
      *               OLDIN   TELCABS.CABS.OLDIN          (LOCAL)     *
      *               LEGIN   TELCABS.CABS.LEGIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               NEWOUT  TELCABS.CABS.NEWOUT         (LOCAL)     *
      *               DSPOUT  TELCABS.CABS.DSPOUT         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - THE OUTPUT IS REBUILT EACH RUN     *
      * REVISION HISTORY                                              *
      *   V1.00  1993-12-16  L.FERREIRA   INITIAL RELEASE             *
      *   V1.03  1995-09-27  J.M.CASTILLO CONTROL RECORD ADDED PER    *
      *                      CABS-STD-002                             *
      *   V1.07  1999-12-28  W.J.MCALLISTER TABLE LIMIT RAISED FOR THE*
      *                      SOUTHEAST CENTRES                        *
      *   V1.09  2005-05-07  M.DELACROIX  TABLE LIMIT RAISED FOR THE  *
      *                      SOUTHEAST CENTRES                        *
      *   V1.10  2006-04-28  D.OKONKWO    PRINT LINE WIDENED TO 133   *
      *   V1.12  2007-01-17  W.J.MCALLISTER RETIRED THE SECOND SORT   *
      *                      STEP - DONE IN PROGRAM                   *
      *   V1.13  2009-05-23  C.ADEYEMI    HASH TOTAL ADDED TO THE     *
      *                      CONTROL RECORD                           *
      *   V1.15  2010-12-27  B.R.HALVORSEN OCCURS RAISED AFTER THE    *
      *                      FEBRUARY OVERFLOW                        *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUCV20.
       AUTHOR. TELCABS APPLICATIONS - FILE CONVERSION.
      *****************************************************************
      * INTERCHANGE FORMAT CONVERSION. THE STEP RUNS ONCE PER BILL    *
      * CYCLE AND IS RERUN FROM THE TOP IF IT FAILS.                  *
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT     *
      * PRECEDES THIS PROGRAM IN THE JOB.                             *
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
           SELECT OLDIN ASSIGN TO UT-S-OLDIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT LEGIN ASSIGN TO UT-S-LEGIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT NEWOUT ASSIGN TO UT-S-NEWOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT DSPOUT ASSIGN TO UT-S-DSPOUT
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
      * IXCIN - PERMANENT DATASET HELD ON DASD.
       FD  IXCIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 170 CHARACTERS.
       01  CABS-DM-IN-RECORD.
           05  ID-JURIS                    PIC X(08).
           05  ID-STATE                    PIC X(03).
           05  ID-ACCOUNT                  PIC X(16).
           05  ID-STATE2                   PIC X(08).
           05  ID-JURIS2                   PIC 9(05).
           05  ID-REGION                   PIC X(20).
           05  ID-BAN                      PIC 9(06).
           05  ID-CENTRE                   PIC S9(09)V9(02) COMP-3.
           05  ID-SOURCE                   PIC X(08).
           05  ID-GROUP                    PIC 9(05).
           05  ID-CODE                     PIC X(06).
           05  ID-CLASS                    PIC S9(11)V9(02) COMP-3.
           05  ID-SEGMENT                  PIC X(06).
           05  ID-TARGET                   PIC S9(11)V9(05) COMP-3.
           05  ID-GROUP2                   PIC S9(13)V9(05) COMP-3.
           05  ID-TYPE                     PIC 9(04).
           05  ID-LEVEL                    PIC X(03).
           05  ID-TYPE2                    PIC S9(09)V9(02) COMP-3.
           05  ID-CODE2                    PIC S9(13)V9(05) COMP-3.
           05  ID-TARGET2                  PIC 9(03).
           05  ID-OCN                      PIC X(03).
           05  ID-TYPE3                    PIC X(02).
           05  ID-MEDIA                    PIC S9(13)V9(05) COMP-3.
           05  DM-FILL-01                  PIC X(6).
      * OLDIN - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  OLDIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 170 CHARACTERS.
       01  CABS-DM-ALT1-RECORD.
           05  A1-ACCOUNT                  PIC X(08).
           05  A1-LEVEL                    PIC S9(09) COMP-3.
           05  A1-TARIFF                   PIC S9(09)V9(05) COMP-3.
           05  A1-REGION                   PIC X(02).
           05  A1-MEDIA                    PIC X(04).
           05  A1-SOURCE                   PIC X(03).
           05  A1-LEVEL2                   PIC 9(02).
           05  A1-CLASS                    PIC X(08).
           05  A1-GROUP                    PIC S9(09) COMP-3.
           05  A1-SOURCE2                  PIC S9(07)V9(05) COMP-3.
           05  A1-CYCLE                    PIC X(02).
           05  DM-FILL-02                  PIC X(116).
      * LEGIN - WORK FILE, DELETED AT STEP END.
       FD  LEGIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 170 CHARACTERS.
       01  CABS-DM-ALT2-RECORD.
           05  A2-TARGET                   PIC S9(09) COMP-3.
           05  A2-TARGET2                  PIC S9(13) COMP-3.
           05  A2-OCN                      PIC S9(13)V9(02) COMP-3.
           05  A2-CIRCUIT                  PIC S9(13) COMP-3.
           05  A2-OCN2                     PIC X(08).
           05  A2-CARRIER                  PIC S9(11)V9(02) COMP-3.
           05  A2-CIRCUIT2                 PIC X(20).
           05  A2-BAND                     PIC X(03).
           05  A2-BAN                      PIC X(10).
           05  A2-CYCLE                    PIC S9(09)V9(02) COMP-3.
           05  DM-FILL-03                  PIC X(89).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-DM-VIEW1 REDEFINES CABS-DM-IN-RECORD.
           05  R0D-CODE                    PIC X(10).
           05  R0D-STATE                   PIC S9(09)V9(02) COMP-3.
           05  R0D-STATE2                  PIC S9(11) COMP-3.
           05  R0D-LEVEL                   PIC S9(11)V9(05) COMP-3.
           05  R0D-SOURCE                  PIC X(13).
           05  R0D-TYPE                    PIC X(13).
           05  R0D-MEDIA                   PIC S9(13) COMP-3.
           05  R0D-BAN                     PIC S9(13) COMP-3.
           05  R0D-REST                    PIC X(99).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-DM-VIEW2 REDEFINES CABS-DM-IN-RECORD.
           05  R1D-CIRCUIT                 PIC X(16).
           05  R1D-LEVEL                   PIC S9(09) COMP-3.
           05  R1D-SEQ                     PIC S9(09)V9(02) COMP-3.
           05  R1D-CLASS                   PIC X(03).
           05  R1D-MEDIA                   PIC S9(13) COMP-3.
           05  R1D-CIRCUIT2                PIC S9(11)V9(05) COMP-3.
           05  R1D-CODE                    PIC X(16).
           05  R1D-ACCOUNT                 PIC X(10).
           05  R1D-CLASS2                  PIC X(08).
           05  R1D-REST                    PIC X(90).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-DM-VIEW3 REDEFINES CABS-DM-IN-RECORD.
           05  R2D-INVOICE                 PIC X(04).
           05  R2D-TARIFF                  PIC X(02).
           05  R2D-CYCLE                   PIC S9(07)V9(02) COMP-3.
           05  R2D-CODE                    PIC X(08).
           05  R2D-CARRIER                 PIC S9(09) COMP-3.
           05  R2D-TYPE                    PIC S9(13) COMP-3.
           05  R2D-BAND                    PIC S9(09)V9(05) COMP-3.
           05  R2D-SOURCE                  PIC X(08).
           05  R2D-LEVEL                   PIC S9(09)V9(05) COMP-3.
           05  R2D-REST                    PIC X(115).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-DM-VIEW4 REDEFINES CABS-DM-IN-RECORD.
           05  R3D-TYPE                    PIC X(10).
           05  R3D-CYCLE                   PIC S9(13) COMP-3.
           05  R3D-MEDIA                   PIC X(02).
           05  R3D-SEQ                     PIC S9(09)V9(05) COMP-3.
           05  R3D-INVOICE                 PIC S9(11)V9(02) COMP-3.
           05  R3D-MEDIA2                  PIC X(08).
           05  R3D-SEGMENT                 PIC S9(13)V9(05) COMP-3.
           05  R3D-REST                    PIC X(118).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-DM-VIEW5 REDEFINES CABS-DM-IN-RECORD.
           05  R4D-TARIFF                  PIC S9(07) COMP-3.
           05  R4D-OCN                     PIC S9(07)V9(05) COMP-3.
           05  R4D-BAND                    PIC X(08).
           05  R4D-OCN2                    PIC X(03).
           05  R4D-CENTRE                  PIC S9(15) COMP-3.
           05  R4D-SEGMENT                 PIC X(16).
           05  R4D-STATE                   PIC X(06).
           05  R4D-CODE                    PIC 9(05).
           05  R4D-GROUP                   PIC S9(07)V9(02) COMP-3.
           05  R4D-REST                    PIC X(108).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-DM-VIEW6 REDEFINES CABS-DM-IN-RECORD.
           05  R5D-ACCOUNT                 PIC S9(15) COMP-3.
           05  R5D-TYPE                    PIC S9(07) COMP-3.
           05  R5D-STATUS                  PIC X(08).
           05  R5D-BAN                     PIC S9(11)V9(02) COMP-3.
           05  R5D-PERIOD                  PIC S9(11)V9(02) COMP-3.
           05  R5D-STATE                   PIC 9(04).
           05  R5D-CLASS                   PIC S9(09)V9(02) COMP-3.
           05  R5D-TYPE2                   PIC X(06).
           05  R5D-STATE2                  PIC S9(15) COMP-3.
           05  R5D-REST                    PIC X(112).
      * NEWOUT - WORK FILE, DELETED AT STEP END.
       FD  NEWOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 140 CHARACTERS.
       01  CABS-DM-OUT-RECORD.
           05  OD-TARGET                   PIC X(08).
           05  OD-CODE                     PIC S9(07)V9(05) COMP-3.
           05  OD-SEGMENT                  PIC 9(04).
           05  OD-REGION                   PIC X(03).
           05  OD-STATE                    PIC X(10).
           05  OD-INVOICE                  PIC X(20).
           05  OD-CENTRE                   PIC X(02).
           05  OD-INVOICE2                 PIC 9(05).
           05  OD-REGION2                  PIC X(10).
           05  OD-SEQ                      PIC X(02).
           05  OD-STATE2                   PIC X(10).
           05  OD-OCN                      PIC 9(06).
           05  OD-MEDIA                    PIC S9(07) COMP-3.
           05  OD-TYPE                     PIC X(08).
           05  OD-SOURCE                   PIC S9(13) COMP-3.
           05  OD-JURIS                    PIC 9(03).
           05  OD-ELEM                     PIC S9(13)V9(02) COMP-3.
           05  OD-STATE3                   PIC S9(11) COMP-3.
           05  OD-BAND                     PIC S9(09)V9(05) COMP-3.
           05  OD-CYCLE                    PIC X(04).
           05  OD-SOURCE2                  PIC X(04).
           05  DM-FILL-04                  PIC X(1).
      * DSPOUT - CATALOGUED GENERATION DATA GROUP.
       FD  DSPOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 140 CHARACTERS.
       01  CABS-DM-OUT1-RECORD         PIC X(140).
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
      * SHARED LAYOUT PULLED IN FOR THE RECORD SIDE.
       COPY CABSSETL.
      * SHARED LAYOUT PULLED IN FOR THE RECORD SIDE.
       COPY CABSCOMM.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUCV20'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V1.19'.
           05  WS-TABLE-LIMIT              PIC S9(04) COMP-3
                                         VALUE 600.
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
           05  WS-DM-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DM-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DM-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DM-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DM-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DM-CNT-06                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DM-CNT-07                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DM-CNT-08                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DM-CNT-09                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DM-CNT-10                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DM-CNT-11                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DM-CNT-12                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-DM-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DM-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DM-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DM-QTY-04                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DM-QTY-05                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DM-QTY-06                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DM-QTY-07                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DM-QTY-08                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-DM-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DM-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DM-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DM-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DM-AMT-05                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DM-AMT-06                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DM-AMT-07                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DM-AMT-08                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DM-AMT-09                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-DM-TXT-01                PIC X(12) VALUE SPACES.
           05  WS-DM-TXT-02                PIC X(30) VALUE SPACES.
           05  WS-DM-TXT-03                PIC X(26) VALUE SPACES.
           05  WS-DM-TXT-04                PIC X(10) VALUE SPACES.
           05  WS-DM-TXT-05                PIC X(30) VALUE SPACES.
           05  WS-DM-TXT-06                PIC X(10) VALUE SPACES.
           05  WS-DM-TXT-07                PIC X(12) VALUE SPACES.
           05  WS-DM-TXT-08                PIC X(26) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-DM-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-DM-ON-01                 VALUE 'Y'.
               88  WS-DM-OFF-01                VALUE 'N'.
           05  WS-DM-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-DM-ON-02                 VALUE 'Y'.
               88  WS-DM-OFF-02                VALUE 'N'.
           05  WS-DM-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-DM-ON-03                 VALUE 'Y'.
               88  WS-DM-OFF-03                VALUE 'N'.
           05  WS-DM-SW-04                 PIC X(01) VALUE 'N'.
               88  WS-DM-ON-04                 VALUE 'Y'.
               88  WS-DM-OFF-04                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-DM-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DM-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DM-SUB-03                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DM-SUB-04                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DM-SUB-05                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DM-SUB-06                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. THE LIMIT WAS RAISED WHEN THE SOUTHEAST CENTRES
      * WERE MERGED AND HAS NOT BEEN REVIEWED SINCE.
       01  WS-DM-TABLE.
           05  WS-DM-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-DM-TB-ENTRY OCCURS 600 TIMES
                                       INDEXED BY WS-DM-IX.
               10  WS-DM-TB-KEY                PIC X(06).
               10  WS-DM-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-DM-TB-TXT                PIC X(30).
               10  WS-DM-TB-EFF                PIC 9(05).
               10  WS-DM-TB-EXP                PIC 9(05).
       01  WS-DM-WORK-GROUP-1.
           05  WS-DM-G1-TARGET             PIC S9(11)V9(02) COMP-3.
           05  WS-DM-G1-BAN                PIC 9(07).
           05  WS-DM-G1-PERIOD             PIC 9(05).
           05  WS-DM-G1-STATE              PIC S9(11)V9(02) COMP-3.
           05  WS-DM-G1-REGION             PIC S9(09) COMP-3.
       01  WS-DM-WORK-GROUP-2.
           05  WS-DM-G2-TARGET             PIC 9(05).
           05  WS-DM-G2-CIRCUIT            PIC S9(11)V9(02) COMP-3.
           05  WS-DM-G2-OCN                PIC 9(07).
           05  WS-DM-G2-SEQ                PIC S9(09) COMP-3.
           05  WS-DM-G2-CODE               PIC S9(09) COMP-3.
           05  WS-DM-G2-TARIFF             PIC 9(05).
           05  WS-DM-G2-REGION             PIC 9(05).
           05  WS-DM-G2-STATE              PIC X(20).
       01  WS-DM-WORK-GROUP-3.
           05  WS-DM-G3-CENTRE             PIC S9(11)V9(02) COMP-3.
           05  WS-DM-G3-STATUS             PIC S9(11)V9(02) COMP-3.
           05  WS-DM-G3-CENTRE             PIC 9(05).
           05  WS-DM-G3-CARRIER            PIC X(20).
           05  WS-DM-G3-MEDIA              PIC X(10).
           05  WS-DM-G3-MEDIA              PIC S9(09) COMP-3.
       01  WS-DM-WORK-GROUP-4.
           05  WS-DM-G4-STATUS             PIC S9(09) COMP-3.
           05  WS-DM-G4-TARGET             PIC 9(07).
           05  WS-DM-G4-SOURCE             PIC 9(07).
           05  WS-DM-G4-STATUS             PIC 9(07).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 55.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUCV20 - INTERCHANGE FORMAT CONVERSION'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-DM-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-DM-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9912.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-DM-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-DM-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
               MOVE 'OPEN FAILED ON IXCIN - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               DISPLAY 'IXCIN FILE STATUS = ' WS-FS-INPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN INPUT OLDIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON OLDIN - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               DISPLAY 'OLDIN FILE STATUS = ' WS-FS-INPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN INPUT LEGIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON LEGIN - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               DISPLAY 'LEGIN FILE STATUS = ' WS-FS-INPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT NEWOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON NEWOUT - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               DISPLAY 'NEWOUT FILE STATUS = ' WS-FS-OUTPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT DSPOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON DSPOUT - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               DISPLAY 'DSPOUT FILE STATUS = ' WS-FS-OUTPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SUSOUT.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON SUSOUT - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               DISPLAY 'SUSOUT FILE STATUS = ' WS-FS-SUSPENSE
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON CTLOUT - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               DISPLAY 'CTLOUT FILE STATUS = ' WS-FS-CONTROL
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RPTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON RPTOUT - CHECK THE ALLOCATION' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-DM-CYCLE-YYDDD.
           COMPUTE WS-DM-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-DM-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-DM-CNT-08.
           MOVE 0 TO WS-DM-CNT-10.
           MOVE 0 TO WS-DM-CNT-02.
       P1200-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-DM-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-DM-TAB-CNT NOT < 600
               MOVE 'Y' TO WS-DM-SW-01
               ADD 1 TO WS-DM-CNT-07
           ELSE
               ADD 1 TO WS-DM-TAB-CNT
               SET WS-DM-IX TO WS-DM-TAB-CNT
               MOVE ID-CODE2 TO WS-DM-TB-KEY (WS-DM-IX)
               MOVE 0 TO WS-DM-TB-VAL (WS-DM-IX)
               MOVE SPACES TO WS-DM-TB-TXT (WS-DM-IX)
               ADD 1 TO WS-DM-CNT-02.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ IXCIN
               AT END MOVE 'Y' TO WS-DM-SW-01.
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
           PERFORM P2200-MATCH-SIGN THRU P2200-MATCH-SIGN-EXIT.
           PERFORM P2300-MATCH-LAYOUT THRU P2300-MATCH-LAYOUT-EXIT.
           PERFORM P2400-SELECT-CENTURY THRU P2400-SELECT-CENTURY-EXIT.
           IF WS-DM-ON-01
               PERFORM P2500-VALIDATE-PACKED THRU
                   P2500-VALIDATE-PACKED-EXIT.
           PERFORM P2600-DERIVE-ZONE THRU P2600-DERIVE-ZONE-EXIT.
           PERFORM P2700-MATCH-SIGN THRU P2700-MATCH-SIGN-EXIT.
           PERFORM P2800-SPLIT-PACKED THRU P2800-SPLIT-PACKED-EXIT.
           PERFORM P2900-SPLIT-ZONE THRU P2900-SPLIT-ZONE-EXIT.
           IF WS-DM-ON-02
               PERFORM P21000-CHECK-LAYOUT THRU
                   P21000-CHECK-LAYOUT-EXIT.
           PERFORM P21100-MATCH-ZONE THRU P21100-MATCH-ZONE-EXIT.
           PERFORM P21200-EXPAND-CENTURY THRU
               P21200-EXPAND-CENTURY-EXIT.
           PERFORM P21300-MATCH-ZONE THRU P21300-MATCH-ZONE-EXIT.
           PERFORM P21400-SPLIT-CENTURY THRU P21400-SPLIT-CENTURY-EXIT.
           IF WS-DM-ON-01
               PERFORM P21500-EDIT-CENTURY THRU
                   P21500-EDIT-CENTURY-EXIT.
           IF WS-DM-ON-04
               PERFORM P21600-SELECT-RECORD THRU
                   P21600-SELECT-RECORD-EXIT.
           PERFORM P21700-RESOLVE-CENTURY THRU
               P21700-RESOLVE-CENTURY-EXIT.
           PERFORM P21800-RESOLVE-FIELD THRU P21800-RESOLVE-FIELD-EXIT.
           PERFORM P21900-EDIT-CENTURY THRU P21900-EDIT-CENTURY-EXIT.
           IF WS-DM-ON-04
               PERFORM P22000-SELECT-ZONE THRU P22000-SELECT-ZONE-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ IXCIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-MATCH-SIGN.
           MOVE SPACES TO CABS-DM-OUT-RECORD.
           MOVE ID-TYPE2 TO OD-TARGET.
           MOVE ID-CODE2 TO OD-CODE.
           MOVE ID-ACCOUNT TO OD-SEGMENT.
           MOVE ID-LEVEL TO OD-REGION.
           MOVE ID-SOURCE TO OD-STATE.
           MOVE ID-MEDIA TO OD-INVOICE.
           MOVE ID-GROUP2 TO OD-CENTRE.
           MOVE ID-TARGET TO OD-INVOICE2.
           MOVE ID-SOURCE TO OD-REGION2.
           WRITE CABS-DM-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
           MOVE 'N' TO WS-DM-SW-04.
           IF WS-DM-TAB-CNT > 0
               PERFORM P280-COMPARE-RECORD THRU P280-COMPARE-RECORD-EXIT
               VARYING WS-DM-SUB-06 FROM 1 BY 1
               UNTIL WS-DM-SUB-06 > WS-DM-TAB-CNT
               OR WS-DM-SW-04 = 'Y'.
       P2200-MATCH-SIGN-EXIT.
           EXIT.
       P2300-MATCH-LAYOUT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-DM-TXT-01 TO PC-COL-001-020.
           MOVE WS-DM-TXT-03 TO PC-COL-021-060.
           MOVE WS-DM-AMT-07 TO WS-DM-AMT-EDIT.
           MOVE WS-DM-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P2300-MATCH-LAYOUT-EXIT.
           EXIT.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P2400-SELECT-CENTURY.
           IF WS-DM-AMT-02 NOT = 0
               COMPUTE WS-DM-QTY-08 = WS-DM-AMT-09 * 100 / WS-DM-AMT-02
           ELSE
               MOVE 0 TO WS-DM-QTY-08.
       P2400-SELECT-CENTURY-EXIT.
           EXIT.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P2500-VALIDATE-PACKED.
           MOVE WS-DM-AMT-09 TO WS-DM-AMT-03.
           IF WS-DM-AMT-03 < 0
               COMPUTE WS-DM-AMT-03 = 0 - WS-DM-AMT-09.
       P2500-VALIDATE-PACKED-EXIT.
           EXIT.
       P2600-DERIVE-ZONE.
           MOVE SPACES TO WS-DM-TXT-02.
           STRING ID-TYPE3 DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               ID-ACCOUNT DELIMITED BY SIZE
               INTO WS-DM-TXT-02.
       P2600-DERIVE-ZONE-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P2700-MATCH-SIGN.
           CALL 'CABSEQCK' USING WS-DM-TXT-01 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-DM-CNT-02.
       P2700-MATCH-SIGN-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P2800-SPLIT-PACKED.
           MOVE ID-GROUP2 TO WS-DM-TXT-03.
           MOVE ID-CODE TO WS-DM-TXT-01.
           MOVE ID-ACCOUNT TO WS-DM-TXT-01.
           ADD 1 TO WS-DM-CNT-02.
       P2800-SPLIT-PACKED-EXIT.
           EXIT.
       P2900-SPLIT-ZONE.
           MOVE 'Y' TO WS-DM-SW-04.
           IF ID-MEDIA < 5
               MOVE 'N' TO WS-DM-SW-04
               ADD 1 TO WS-DM-CNT-12.
           IF ID-MEDIA > 2375
               MOVE 'N' TO WS-DM-SW-04
               ADD 1 TO WS-DM-CNT-01.
       P2900-SPLIT-ZONE-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P21000-CHECK-LAYOUT.
           ADD ID-GROUP2 TO WS-DM-QTY-03.
           COMPUTE WS-DM-AMT-09 ROUNDED = WS-DM-QTY-03 * WS-DM-QTY-08.
           ADD WS-DM-AMT-09 TO WS-DM-AMT-07.
       P21000-CHECK-LAYOUT-EXIT.
           EXIT.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P21100-MATCH-ZONE.
           UNSTRING WS-DM-TXT-08 DELIMITED BY '/'
               INTO WS-DM-TXT-03
               WS-DM-TXT-07
               TALLYING IN WS-DM-CNT-06.
       P21100-MATCH-ZONE-EXIT.
           EXIT.
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT
      * PRECEDES THIS PROGRAM IN THE JOB.
       P21200-EXPAND-CENTURY.
           MOVE 0 TO WS-DM-CNT-03.
           INSPECT WS-DM-TXT-08 TALLYING WS-DM-CNT-03
               FOR ALL SPACES.
           INSPECT WS-DM-TXT-08 REPLACING ALL LOW-VALUES BY SPACES.
       P21200-EXPAND-CENTURY-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P21300-MATCH-ZONE.
           CALL 'CABHASH' USING ID-TARGET WS-ACC-OCN-HASH.
           ADD WS-DM-CNT-09 TO WS-ACC-SEQ-HASH.
       P21300-MATCH-ZONE-EXIT.
           EXIT.
       P21400-SPLIT-CENTURY.
           IF ID-STATE2 = 'A'
               ADD 1 TO WS-DM-CNT-09
           ELSE
               IF ID-STATE2 = 'E'
                   ADD 1 TO WS-DM-CNT-02
               ELSE
                   IF ID-STATE2 = 'B'
                       ADD 1 TO WS-DM-CNT-02
                   ELSE
                       ADD 1 TO WS-DM-CNT-03.
       P21400-SPLIT-CENTURY-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P21500-EDIT-CENTURY.
           MOVE 'N' TO WS-DM-SW-04.
           IF WS-DM-TXT-01 NOT = WS-DM-TXT-02
               MOVE 'Y' TO WS-DM-SW-04
               MOVE WS-DM-TXT-01 TO WS-DM-TXT-02
               ADD 1 TO WS-DM-CNT-11.
       P21500-EDIT-CENTURY-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P21600-SELECT-RECORD.
           IF WS-DM-AMT-07 < 22
               MOVE 22 TO WS-DM-AMT-07
               ADD 1 TO WS-DM-CNT-09.
           IF WS-DM-AMT-07 > 10012
               MOVE 10012 TO WS-DM-AMT-07
               ADD 1 TO WS-DM-CNT-06.
       P21600-SELECT-RECORD-EXIT.
           EXIT.
      * A FAILURE HERE SUSPENDS THE RECORD, IT DOES NOT STOP THE STEP.
       P21700-RESOLVE-CENTURY.
           MOVE 0 TO WS-DM-QTY-03.
           MOVE 0 TO WS-DM-QTY-01.
           MOVE 0 TO WS-DM-QTY-04.
           MOVE 0 TO WS-DM-AMT-08.
           MOVE 0 TO WS-DM-AMT-07.
       P21700-RESOLVE-CENTURY-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P21800-RESOLVE-FIELD.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-RATE-NOT-FOUND TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-DM-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
       P21800-RESOLVE-FIELD-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P21900-EDIT-CENTURY.
           MOVE SPACES TO CABS-DM-OUT-RECORD.
           MOVE ID-TYPE TO OD-TARGET.
           MOVE ID-TARGET TO OD-CODE.
           MOVE ID-REGION TO OD-SEGMENT.
           MOVE ID-OCN TO OD-REGION.
           MOVE ID-ACCOUNT TO OD-STATE.
           MOVE ID-SOURCE TO OD-INVOICE.
           MOVE ID-BAN TO OD-CENTRE.
           WRITE CABS-DM-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P21900-EDIT-CENTURY-EXIT.
           EXIT.
       P22000-SELECT-ZONE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-DM-TXT-06 TO PC-COL-001-020.
           MOVE WS-DM-TXT-03 TO PC-COL-021-060.
           MOVE WS-DM-AMT-01 TO WS-DM-AMT-EDIT.
           MOVE WS-DM-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P22000-SELECT-ZONE-EXIT.
           EXIT.
       P280-COMPARE-RECORD.
           SET WS-DM-IX TO WS-DM-SUB-01.
           IF WS-DM-TB-KEY (WS-DM-IX) = ID-JURIS2
               MOVE 'Y' TO WS-DM-SW-02
               MOVE WS-DM-TB-VAL (WS-DM-IX) TO WS-DM-QTY-07
               MOVE WS-DM-SUB-01 TO WS-DM-SUB-02.
       P280-COMPARE-RECORD-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P3100-FORMAT-LAYOUT.
           MOVE 0 TO WS-DM-QTY-07.
           MOVE 0 TO WS-DM-QTY-04.
           MOVE 0 TO WS-DM-QTY-06.
           MOVE 0 TO WS-DM-AMT-06.
       P3100-FORMAT-LAYOUT-EXIT.
           EXIT.
       P3200-POST-PACKED.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-DM-TXT-08 TO PC-COL-001-020.
           MOVE WS-DM-TXT-06 TO PC-COL-021-060.
           MOVE WS-DM-AMT-05 TO WS-DM-AMT-EDIT.
           MOVE WS-DM-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P3200-POST-PACKED-EXIT.
           EXIT.
       P3300-POST-LAYOUT.
           ADD ID-CODE2 TO WS-DM-QTY-04.
           COMPUTE WS-DM-AMT-09 = WS-DM-QTY-04 * WS-DM-QTY-07.
           ADD WS-DM-AMT-09 TO WS-DM-AMT-01.
       P3300-POST-LAYOUT-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P3400-EMIT-PACKED.
           MOVE ID-LEVEL TO WS-DM-TXT-03.
           MOVE ID-JURIS2 TO WS-DM-TXT-02.
           ADD 1 TO WS-DM-CNT-08.
       P3400-EMIT-PACKED-EXIT.
           EXIT.
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT
      * PRECEDES THIS PROGRAM IN THE JOB.
       P3500-RELEASE-RECORD.
           MOVE SPACES TO CABS-DM-OUT-RECORD.
           MOVE ID-STATE2 TO OD-TARGET.
           MOVE ID-STATE2 TO OD-CODE.
           MOVE ID-JURIS TO OD-SEGMENT.
           MOVE ID-JURIS TO OD-REGION.
           MOVE ID-STATE TO OD-STATE.
           MOVE ID-TARGET2 TO OD-INVOICE.
           WRITE CABS-DM-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P3500-RELEASE-RECORD-EXIT.
           EXIT.
       P3600-CLOSE-OFF-LAYOUT.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-DUP-SEQ TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-DM-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
           MOVE CABS-SUSPENSE-RECORD TO CABS-SUSOUT-RECORD.
           WRITE CABS-SUSOUT-RECORD.
       P3600-CLOSE-OFF-LAYOUT-EXIT.
           EXIT.
      * S400-SECONDARY-PROCESSING SECTION
       S400-SECONDARY-PROCESSING SECTION.
       P4000-SECONDARY.
           PERFORM P4100-COMPARE-SOURCE THRU P4100-COMPARE-SOURCE-EXIT.
           PERFORM P4200-COMPARE-BAND THRU P4200-COMPARE-BAND-EXIT.
           PERFORM P4300-SUMMARISE-GROUP THRU
               P4300-SUMMARISE-GROUP-EXIT.
           PERFORM P4400-COMPARE-PACKED THRU P4400-COMPARE-PACKED-EXIT.
           PERFORM P4500-COMPARE-ZONE THRU P4500-COMPARE-ZONE-EXIT.
           PERFORM P4600-AUDIT-TARIFF THRU P4600-AUDIT-TARIFF-EXIT.
           PERFORM P4700-TRACE-MEDIA THRU P4700-TRACE-MEDIA-EXIT.
           PERFORM P4800-ADJUST-BAND THRU P4800-ADJUST-BAND-EXIT.
           PERFORM P4900-TRACE-TARGET THRU P4900-TRACE-TARGET-EXIT.
           PERFORM P41000-REPORT-SEGMENT THRU
               P41000-REPORT-SEGMENT-EXIT.
           PERFORM P41100-AUDIT-CODE THRU P41100-AUDIT-CODE-EXIT.
           PERFORM P41200-ADJUST-STATUS THRU P41200-ADJUST-STATUS-EXIT.
           PERFORM P41300-RECONCILE-SEGMENT THRU
               P41300-RECONCILE-SEGMENT-EXIT.
           PERFORM P41400-NORMALISE-PERIOD THRU
               P41400-NORMALISE-PERIOD-EXIT.
           PERFORM P41500-TRACE-STATUS THRU P41500-TRACE-STATUS-EXIT.
           PERFORM P41600-TRACE-STATUS THRU P41600-TRACE-STATUS-EXIT.
           PERFORM P41700-TRACE-TYPE THRU P41700-TRACE-TYPE-EXIT.
           PERFORM P41800-REPORT-TARGET THRU P41800-REPORT-TARGET-EXIT.
           PERFORM P41900-TRACE-CYCLE THRU P41900-TRACE-CYCLE-EXIT.
           PERFORM P42000-COMPARE-TARIFF THRU
               P42000-COMPARE-TARIFF-EXIT.
           PERFORM P42100-NORMALISE-MEDIA THRU
               P42100-NORMALISE-MEDIA-EXIT.
           PERFORM P42200-REPORT-ELEM THRU P42200-REPORT-ELEM-EXIT.
           PERFORM P42300-COMPARE-LAYOUT THRU
               P42300-COMPARE-LAYOUT-EXIT.
           PERFORM P42400-REPORT-STATUS THRU P42400-REPORT-STATUS-EXIT.
           PERFORM P42500-TRACE-TARIFF THRU P42500-TRACE-TARIFF-EXIT.
           PERFORM P42600-REPORT-MEDIA THRU P42600-REPORT-MEDIA-EXIT.
           PERFORM P42700-ADJUST-INVOICE THRU
               P42700-ADJUST-INVOICE-EXIT.
           PERFORM P42800-SUMMARISE-MEDIA THRU
               P42800-SUMMARISE-MEDIA-EXIT.
           PERFORM P42900-ADJUST-FIELD THRU P42900-ADJUST-FIELD-EXIT.
       P4000-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P4100-COMPARE-SOURCE.
           MOVE 0 TO WS-DM-QTY-07.
           MOVE 0 TO WS-DM-QTY-08.
           MOVE 0 TO WS-DM-AMT-08.
       P4100-COMPARE-SOURCE-EXIT.
           EXIT.
       P4200-COMPARE-BAND.
           CALL 'CABHASH' USING ID-STATE2 WS-ACC-OCN-HASH.
           ADD WS-DM-CNT-03 TO WS-ACC-SEQ-HASH.
       P4200-COMPARE-BAND-EXIT.
           EXIT.
      * THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO
      * THE RECYCLE JOB DOES NOT NEED THE SOURCE.
       P4300-SUMMARISE-GROUP.
           MOVE ID-JURIS TO WS-DM-TXT-01.
           MOVE ID-CODE TO WS-DM-TXT-02.
           MOVE ID-TARGET2 TO WS-DM-TXT-02.
           MOVE ID-TARGET TO WS-DM-TXT-07.
           ADD 1 TO WS-DM-CNT-02.
       P4300-SUMMARISE-GROUP-EXIT.
           EXIT.
       P4400-COMPARE-PACKED.
           IF WS-DM-AMT-03 NOT = 0
               COMPUTE WS-DM-QTY-08 = WS-DM-AMT-08 * 100 / WS-DM-AMT-03
           ELSE
               MOVE 0 TO WS-DM-QTY-08.
       P4400-COMPARE-PACKED-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P4500-COMPARE-ZONE.
           MOVE WS-DM-AMT-09 TO WS-DM-AMT-05.
           IF WS-DM-AMT-05 < 0
               COMPUTE WS-DM-AMT-05 = 0 - WS-DM-AMT-09.
       P4500-COMPARE-ZONE-EXIT.
           EXIT.
       P4600-AUDIT-TARIFF.
           ADD ID-CODE2 TO WS-DM-QTY-05.
           COMPUTE WS-DM-AMT-04 ROUNDED = WS-DM-QTY-05 * WS-DM-QTY-05.
           ADD WS-DM-AMT-04 TO WS-DM-AMT-04.
       P4600-AUDIT-TARIFF-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P4700-TRACE-MEDIA.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-DUP-SEQ TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-DM-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
           MOVE CABS-SUSPENSE-RECORD TO CABS-SUSOUT-RECORD.
           WRITE CABS-SUSOUT-RECORD.
       P4700-TRACE-MEDIA-EXIT.
           EXIT.
       P4800-ADJUST-BAND.
           MOVE 'Y' TO WS-DM-SW-01.
           IF ID-BAN < 8
               MOVE 'N' TO WS-DM-SW-01
               ADD 1 TO WS-DM-CNT-02.
           IF ID-BAN > 2812
               MOVE 'N' TO WS-DM-SW-01
               ADD 1 TO WS-DM-CNT-05.
       P4800-ADJUST-BAND-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P4900-TRACE-TARGET.
           CALL 'CABHASH' USING WS-DM-TXT-01 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-DM-CNT-06.
       P4900-TRACE-TARGET-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P41000-REPORT-SEGMENT.
           UNSTRING WS-DM-TXT-04 DELIMITED BY '/'
               INTO WS-DM-TXT-07
               WS-DM-TXT-08
               TALLYING IN WS-DM-CNT-12.
       P41000-REPORT-SEGMENT-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P41100-AUDIT-CODE.
           MOVE 0 TO WS-DM-CNT-11.
           INSPECT WS-DM-TXT-01 TALLYING WS-DM-CNT-11
               FOR ALL SPACES.
           INSPECT WS-DM-TXT-01 REPLACING ALL LOW-VALUES BY SPACES.
       P41100-AUDIT-CODE-EXIT.
           EXIT.
       P41200-ADJUST-STATUS.
           MOVE 'N' TO WS-DM-SW-03.
           IF WS-DM-TXT-07 NOT = WS-DM-TXT-07
               MOVE 'Y' TO WS-DM-SW-03
               MOVE WS-DM-TXT-07 TO WS-DM-TXT-07
               ADD 1 TO WS-DM-CNT-10.
       P41200-ADJUST-STATUS-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P41300-RECONCILE-SEGMENT.
           IF ID-GROUP2 = 'B'
               ADD 1 TO WS-DM-CNT-08
           ELSE
               IF ID-GROUP2 = 'C'
                   ADD 1 TO WS-DM-CNT-01
               ELSE
                   IF ID-GROUP2 = 'C'
                       ADD 1 TO WS-DM-CNT-02
                   ELSE
                       ADD 1 TO WS-DM-CNT-08.
       P41300-RECONCILE-SEGMENT-EXIT.
           EXIT.
       P41400-NORMALISE-PERIOD.
           MOVE SPACES TO WS-DM-TXT-02.
           STRING ID-CODE DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               ID-TYPE2 DELIMITED BY SIZE
               INTO WS-DM-TXT-02.
       P41400-NORMALISE-PERIOD-EXIT.
           EXIT.
       P41500-TRACE-STATUS.
           MOVE SPACES TO CABS-DM-OUT-RECORD.
           MOVE ID-STATE TO OD-TARGET.
           MOVE ID-TYPE TO OD-CODE.
           MOVE ID-BAN TO OD-SEGMENT.
           MOVE ID-JURIS2 TO OD-REGION.
           MOVE ID-STATE TO OD-STATE.
           MOVE ID-STATE TO OD-INVOICE.
           MOVE ID-SOURCE TO OD-CENTRE.
           WRITE CABS-DM-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P41500-TRACE-STATUS-EXIT.
           EXIT.
       P41600-TRACE-STATUS.
           IF WS-DM-AMT-09 < 23
               MOVE 23 TO WS-DM-AMT-09
               ADD 1 TO WS-DM-CNT-06.
           IF WS-DM-AMT-09 > 94807
               MOVE 94807 TO WS-DM-AMT-09
               ADD 1 TO WS-DM-CNT-01.
       P41600-TRACE-STATUS-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P41700-TRACE-TYPE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-DM-TXT-04 TO PC-COL-001-020.
           MOVE WS-DM-TXT-05 TO PC-COL-021-060.
           MOVE WS-DM-AMT-02 TO WS-DM-AMT-EDIT.
           MOVE WS-DM-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P41700-TRACE-TYPE-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P41800-REPORT-TARGET.
           MOVE 0 TO WS-DM-QTY-06.
           MOVE 0 TO WS-DM-QTY-08.
           MOVE 0 TO WS-DM-AMT-02.
           MOVE 0 TO WS-DM-AMT-05.
       P41800-REPORT-TARGET-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P41900-TRACE-CYCLE.
           CALL 'CABHASH' USING ID-TYPE WS-ACC-OCN-HASH.
           ADD WS-DM-CNT-11 TO WS-ACC-SEQ-HASH.
       P41900-TRACE-CYCLE-EXIT.
           EXIT.
       P42000-COMPARE-TARIFF.
           MOVE ID-CODE TO WS-DM-TXT-06.
           MOVE ID-BAN TO WS-DM-TXT-05.
           MOVE ID-STATE TO WS-DM-TXT-08.
           ADD 1 TO WS-DM-CNT-10.
       P42000-COMPARE-TARIFF-EXIT.
           EXIT.
       P42100-NORMALISE-MEDIA.
           IF WS-DM-AMT-02 NOT = 0
               COMPUTE WS-DM-QTY-02 = WS-DM-AMT-05 * 100 / WS-DM-AMT-02
           ELSE
               MOVE 0 TO WS-DM-QTY-02.
       P42100-NORMALISE-MEDIA-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P42200-REPORT-ELEM.
           MOVE WS-DM-AMT-08 TO WS-DM-AMT-05.
           IF WS-DM-AMT-05 < 0
               COMPUTE WS-DM-AMT-05 = 0 - WS-DM-AMT-08.
       P42200-REPORT-ELEM-EXIT.
           EXIT.
       P42300-COMPARE-LAYOUT.
           ADD ID-CODE2 TO WS-DM-QTY-08.
           COMPUTE WS-DM-AMT-03 ROUNDED = WS-DM-QTY-08 * WS-DM-QTY-07.
           ADD WS-DM-AMT-03 TO WS-DM-AMT-05.
       P42300-COMPARE-LAYOUT-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P42400-REPORT-STATUS.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-OCN-UNKNOWN TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-DM-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
           MOVE CABS-SUSPENSE-RECORD TO CABS-SUSOUT-RECORD.
           WRITE CABS-SUSOUT-RECORD.
       P42400-REPORT-STATUS-EXIT.
           EXIT.
       P42500-TRACE-TARIFF.
           MOVE 'Y' TO WS-DM-SW-02.
           IF ID-BAN < 26
               MOVE 'N' TO WS-DM-SW-02
               ADD 1 TO WS-DM-CNT-11.
           IF ID-BAN > 4468
               MOVE 'N' TO WS-DM-SW-02
               ADD 1 TO WS-DM-CNT-01.
       P42500-TRACE-TARIFF-EXIT.
           EXIT.
       P42600-REPORT-MEDIA.
           CALL 'CABEDITF' USING WS-DM-TXT-01 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-DM-CNT-11.
       P42600-REPORT-MEDIA-EXIT.
           EXIT.
       P42700-ADJUST-INVOICE.
           UNSTRING WS-DM-TXT-01 DELIMITED BY '/'
               INTO WS-DM-TXT-04
               WS-DM-TXT-03
               TALLYING IN WS-DM-CNT-01.
       P42700-ADJUST-INVOICE-EXIT.
           EXIT.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P42800-SUMMARISE-MEDIA.
           MOVE 0 TO WS-DM-CNT-02.
           INSPECT WS-DM-TXT-03 TALLYING WS-DM-CNT-02
               FOR ALL SPACES.
           INSPECT WS-DM-TXT-03 REPLACING ALL LOW-VALUES BY SPACES.
       P42800-SUMMARISE-MEDIA-EXIT.
           EXIT.
       P42900-ADJUST-FIELD.
           MOVE 'N' TO WS-DM-SW-01.
           IF WS-DM-TXT-02 NOT = WS-DM-TXT-02
               MOVE 'Y' TO WS-DM-SW-01
               MOVE WS-DM-TXT-02 TO WS-DM-TXT-02
               ADD 1 TO WS-DM-CNT-11.
       P42900-ADJUST-FIELD-EXIT.
           EXIT.
           MOVE 0 TO WS-DM-QTY-08.
           PERFORM P380-WALK-FIELD THRU P380-WALK-FIELD-EXIT
               VARYING WS-DM-SUB-04 FROM 1 BY 1
               UNTIL WS-DM-SUB-04 > WS-DM-TAB-CNT.
       P380-WALK-FIELD.
           SET WS-DM-IX TO WS-DM-SUB-02.
           IF WS-DM-TB-KEY (WS-DM-IX) NOT = SPACES
               ADD WS-DM-TB-VAL (WS-DM-IX) TO WS-DM-QTY-08.
       P380-WALK-FIELD-EXIT.
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
           MOVE 'RECORDS SUMMARISED' TO PC-COL-001-020.
           MOVE WS-SUMM-CNT TO WS-DM-CNT-EDIT.
           MOVE WS-DM-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS WRITTEN' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-DM-CNT-EDIT.
           MOVE WS-DM-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS REJECTED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-DM-CNT-EDIT.
           MOVE WS-DM-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS READ' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO WS-DM-CNT-EDIT.
           MOVE WS-DM-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS CARRIED FWD' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-DM-CNT-EDIT.
           MOVE WS-DM-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-DM-CNT-01 TO WS-DM-CNT-EDIT.
           MOVE WS-DM-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE 2 TO CT-STEP-SEQ.
           MOVE WS-DM-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE SPACES TO CT-FILLER.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE WS-DM-CNT-02 TO CT-RC.
           MOVE SPACES TO CT-ABEND-CD.
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
           CLOSE IXCIN.
           CLOSE OLDIN.
           CLOSE LEGIN.
           CLOSE NEWOUT.
           CLOSE DSPOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABUCV20 - NORMAL END OF JOB'.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  DM-CNT-05 = ' WS-DM-CNT-05.
           DISPLAY '  DM-CNT-10 = ' WS-DM-CNT-10.
           DISPLAY '  DM-CNT-01 = ' WS-DM-CNT-01.
       P9000-EXIT.
           EXIT.
