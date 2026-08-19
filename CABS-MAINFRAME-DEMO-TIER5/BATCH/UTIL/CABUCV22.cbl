      *****************************************************************
      * CABUCV22 - CENTURY FIELD CONVERSION                           *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               LEGIN   TELCABS.CABS.LEGIN          (LOCAL)     *
      *               EMIIN   TELCABS.CABS.EMIIN          (LOCAL)     *
      *               PCKIN   TELCABS.CABS.PCKIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               TGTOUT  TELCABS.CABS.TGTOUT         (LOCAL)     *
      *               CNVOUT  TELCABS.CABS.CNVOUT         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * REVISION HISTORY                                              *
      *   V1.00  1992-12-07  T.YAMASHITA  INITIAL RELEASE             *
      *   V1.03  1993-02-09  T.YAMASHITA  CONTROL RECORD ADDED PER    *
      *                      CABS-STD-002                             *
      *   V1.07  1994-03-06  B.R.HALVORSEN EFFECTIVE DATE FILTER ADDED*
      *                      PER AUDIT FINDING                        *
      *   V1.10  1998-03-27  G.PRZYBYLSKI PRINT LINE WIDENED TO 133   *
      *   V1.13  2001-02-01  K.O.BRIEN    PRINT LINE WIDENED TO 133   *
      *   V1.15  2005-02-07  K.O.BRIEN    JOB PARAMETER MADE MANDATORY*
      *   V1.18  2008-03-19  C.ADEYEMI    RETIRED THE SECOND SORT STEP*
      *                      - DONE IN PROGRAM                        *
      *   V1.22  2013-09-20  T.YAMASHITA  REPORT PAGINATION CORRECTED *
      *   V1.24  2016-12-01  R.T.WHEELER  RECOMPILE ONLY - COPYBOOK   *
      *                      CHANGE UPSTREAM                          *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUCV22.
       AUTHOR. TELCABS APPLICATIONS - FILE CONVERSION.
      *****************************************************************
      * CENTURY FIELD CONVERSION. THE STEP RUNS ONCE PER BILL CYCLE   *
      * AND IS RERUN FROM THE TOP IF IT FAILS.                        *
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
           SELECT LEGIN ASSIGN TO UT-S-LEGIN
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
           SELECT CNVOUT ASSIGN TO UT-S-CNVOUT
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
      * LEGIN - WORK FILE, DELETED AT STEP END.
       FD  LEGIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 120 CHARACTERS.
       01  CABS-CL-IN-RECORD.
           05  IC-STATE                    PIC S9(05) COMP-3.
           05  IC-STATUS                   PIC 9(02).
           05  IC-CENTRE                   PIC S9(07) COMP-3.
           05  IC-CODE                     PIC S9(11)V9(05) COMP-3.
           05  IC-REGION                   PIC S9(05) COMP-3.
           05  IC-PERIOD                   PIC X(08).
           05  IC-REGION2                  PIC S9(07)V9(02) COMP-3.
           05  IC-TARGET                   PIC 9(04).
           05  IC-PERIOD2                  PIC S9(07) COMP-3.
           05  IC-SOURCE                   PIC S9(07)V9(05) COMP-3.
           05  IC-STATE2                   PIC S9(09) COMP-3.
           05  IC-TARIFF                   PIC X(13).
           05  IC-STATUS2                  PIC S9(09)V9(05) COMP-3.
           05  IC-SEQ                      PIC 9(02).
           05  IC-MEDIA                    PIC S9(11)V9(02) COMP-3.
           05  IC-TARGET2                  PIC X(16).
           05  IC-GROUP                    PIC X(06).
           05  IC-PERIOD3                  PIC 9(04).
           05  CL-FILL-01                  PIC X(10).
      * EMIIN - PERMANENT DATASET HELD ON DASD.
       FD  EMIIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 120 CHARACTERS.
       01  CABS-CL-ALT1-RECORD.
           05  A1-CLASS                    PIC S9(15) COMP-3.
           05  A1-CODE                     PIC S9(13)V9(02) COMP-3.
           05  A1-STATE                    PIC S9(13)V9(02) COMP-3.
           05  A1-BAN                      PIC S9(13)V9(05) COMP-3.
           05  A1-BAN2                     PIC X(13).
           05  A1-TARGET                   PIC 9(09).
           05  CL-FILL-02                  PIC X(64).
      * PCKIN - WORK FILE, DELETED AT STEP END.
       FD  PCKIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 120 CHARACTERS.
       01  CABS-CL-ALT2-RECORD.
           05  A2-CYCLE                    PIC S9(05) COMP-3.
           05  A2-CLASS                    PIC X(20).
           05  A2-OCN                      PIC X(04).
           05  A2-CLASS2                   PIC S9(15) COMP-3.
           05  A2-TYPE                     PIC X(10).
           05  A2-REGION                   PIC 9(02).
           05  CL-FILL-03                  PIC X(73).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-CL-VIEW1 REDEFINES CABS-CL-IN-RECORD.
           05  R0C-BAND                    PIC X(03).
           05  R0C-SOURCE                  PIC 9(09).
           05  R0C-JURIS                   PIC X(04).
           05  R0C-SEGMENT                 PIC S9(13)V9(02) COMP-3.
           05  R0C-CENTRE                  PIC X(10).
           05  R0C-CENTRE2                 PIC S9(09)V9(02) COMP-3.
           05  R0C-CYCLE                   PIC 9(06).
           05  R0C-REST                    PIC X(74).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-CL-VIEW2 REDEFINES CABS-CL-IN-RECORD.
           05  R1C-CIRCUIT                 PIC X(06).
           05  R1C-OCN                     PIC S9(07)V9(05) COMP-3.
           05  R1C-PERIOD                  PIC X(16).
           05  R1C-CODE                    PIC X(16).
           05  R1C-TARGET                  PIC S9(13) COMP-3.
           05  R1C-JURIS                   PIC S9(11)V9(02) COMP-3.
           05  R1C-ELEM                    PIC X(02).
           05  R1C-OCN2                    PIC S9(09)V9(02) COMP-3.
           05  R1C-LEVEL                   PIC S9(09)V9(02) COMP-3.
           05  R1C-REST                    PIC X(47).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-CL-VIEW3 REDEFINES CABS-CL-IN-RECORD.
           05  R2C-INVOICE                 PIC 9(09).
           05  R2C-OCN                     PIC X(13).
           05  R2C-JURIS                   PIC X(04).
           05  R2C-CARRIER                 PIC S9(13)V9(02) COMP-3.
           05  R2C-BAND                    PIC S9(07) COMP-3.
           05  R2C-SOURCE                  PIC X(10).
           05  R2C-GROUP                   PIC X(08).
           05  R2C-BAND2                   PIC S9(13) COMP-3.
           05  R2C-REST                    PIC X(57).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-CL-VIEW4 REDEFINES CABS-CL-IN-RECORD.
           05  R3C-LEVEL                   PIC 9(05).
           05  R3C-JURIS                   PIC 9(09).
           05  R3C-STATE                   PIC X(02).
           05  R3C-CODE                    PIC X(06).
           05  R3C-TARIFF                  PIC 9(09).
           05  R3C-REST                    PIC X(89).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-CL-VIEW5 REDEFINES CABS-CL-IN-RECORD.
           05  R4C-SEQ                     PIC X(02).
           05  R4C-CIRCUIT                 PIC X(03).
           05  R4C-JURIS                   PIC S9(15) COMP-3.
           05  R4C-INVOICE                 PIC 9(05).
           05  R4C-REST                    PIC X(102).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-CL-VIEW6 REDEFINES CABS-CL-IN-RECORD.
           05  R5C-OCN                     PIC X(02).
           05  R5C-ACCOUNT                 PIC S9(11) COMP-3.
           05  R5C-ACCOUNT2                PIC S9(07)V9(02) COMP-3.
           05  R5C-CIRCUIT                 PIC S9(15) COMP-3.
           05  R5C-BAND                    PIC 9(05).
           05  R5C-TARGET                  PIC X(13).
           05  R5C-JURIS                   PIC S9(11)V9(02) COMP-3.
           05  R5C-ELEM                    PIC S9(13)V9(02) COMP-3.
           05  R5C-REST                    PIC X(66).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-CL-VIEW7 REDEFINES CABS-CL-IN-RECORD.
           05  R6C-INVOICE                 PIC S9(11) COMP-3.
           05  R6C-LEVEL                   PIC X(16).
           05  R6C-CLASS                   PIC S9(09) COMP-3.
           05  R6C-STATE                   PIC S9(07) COMP-3.
           05  R6C-STATE2                  PIC X(02).
           05  R6C-REST                    PIC X(87).
      * TGTOUT - PERMANENT DATASET HELD ON DASD.
       FD  TGTOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CL-OUT-RECORD.
           05  OC-OCN                      PIC 9(09).
           05  OC-MEDIA                    PIC X(06).
           05  OC-LEVEL                    PIC X(13).
           05  OC-BAND                     PIC X(16).
           05  OC-CLASS                    PIC S9(11) COMP-3.
           05  OC-SOURCE                   PIC 9(03).
           05  OC-SOURCE2                  PIC S9(09) COMP-3.
           05  OC-TYPE                     PIC X(06).
           05  OC-JURIS                    PIC X(04).
           05  OC-CIRCUIT                  PIC S9(11)V9(02) COMP-3.
           05  OC-TARGET                   PIC 9(04).
           05  OC-TARGET2                  PIC 9(06).
           05  OC-LEVEL2                   PIC 9(04).
           05  OC-JURIS2                   PIC 9(09).
           05  OC-SEGMENT                  PIC X(10).
           05  OC-CARRIER                  PIC S9(13)V9(02) COMP-3.
           05  OC-MEDIA2                   PIC S9(09)V9(05) COMP-3.
           05  OC-INVOICE                  PIC X(13).
           05  OC-OCN2                     PIC X(06).
           05  OC-MEDIA3                   PIC S9(07)V9(02) COMP-3.
           05  OC-BAN                      PIC 9(07).
           05  OC-MEDIA4                   PIC S9(09) COMP-3.
           05  OC-CYCLE                    PIC 9(09).
           05  OC-BAND2                    PIC S9(09)V9(05) COMP-3.
           05  CL-FILL-04                  PIC X(3).
      * CNVOUT - PERMANENT DATASET HELD ON DASD.
       FD  CNVOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CL-OUT1-RECORD         PIC X(180).
      * SUSOUT - WORK FILE, DELETED AT STEP END.
       FD  SUSOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 300 CHARACTERS.
       01  CABS-SUSOUT-RECORD              PIC X(300).
      * CTLOUT - CATALOGUED GENERATION DATA GROUP.
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
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUCV22'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V3.11'.
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
       01  WS-PARM-CARD-R2 REDEFINES WS-PARM-CARD.
           05  PC2-LEAD                    PIC X(14).
           05  PC2-CYCLE-VIEW.
               10  PC2-CV-YY                   PIC 9(02).
               10  PC2-CV-DDD                  PIC 9(03).
           05  PC2-REST                    PIC X(61).
       01  WS-COUNT-AREA.
           05  WS-CL-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CL-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CL-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CL-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CL-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CL-CNT-06                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CL-CNT-07                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CL-CNT-08                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CL-CNT-09                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CL-CNT-10                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CL-CNT-11                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CL-CNT-12                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-CL-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CL-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CL-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CL-QTY-04                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CL-QTY-05                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-CL-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CL-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CL-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CL-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CL-AMT-05                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CL-AMT-06                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CL-AMT-07                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CL-AMT-08                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-CL-TXT-01                PIC X(16) VALUE SPACES.
           05  WS-CL-TXT-02                PIC X(16) VALUE SPACES.
           05  WS-CL-TXT-03                PIC X(26) VALUE SPACES.
           05  WS-CL-TXT-04                PIC X(30) VALUE SPACES.
           05  WS-CL-TXT-05                PIC X(08) VALUE SPACES.
           05  WS-CL-TXT-06                PIC X(10) VALUE SPACES.
           05  WS-CL-TXT-07                PIC X(16) VALUE SPACES.
           05  WS-CL-TXT-08                PIC X(10) VALUE SPACES.
           05  WS-CL-TXT-09                PIC X(20) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-CL-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-CL-ON-01                 VALUE 'Y'.
               88  WS-CL-OFF-01                VALUE 'N'.
           05  WS-CL-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-CL-ON-02                 VALUE 'Y'.
               88  WS-CL-OFF-02                VALUE 'N'.
           05  WS-CL-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-CL-ON-03                 VALUE 'Y'.
               88  WS-CL-OFF-03                VALUE 'N'.
           05  WS-CL-SW-04                 PIC X(01) VALUE 'N'.
               88  WS-CL-ON-04                 VALUE 'Y'.
               88  WS-CL-OFF-04                VALUE 'N'.
           05  WS-CL-SW-05                 PIC X(01) VALUE 'N'.
               88  WS-CL-ON-05                 VALUE 'Y'.
               88  WS-CL-OFF-05                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-CL-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CL-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CL-SUB-03                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CL-SUB-04                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CL-SUB-05                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CL-SUB-06                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. THE LIMIT WAS RAISED WHEN THE SOUTHEAST CENTRES
      * WERE MERGED AND HAS NOT BEEN REVIEWED SINCE.
       01  WS-CL-TABLE.
           05  WS-CL-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-CL-TB-ENTRY OCCURS 750 TIMES
                                       INDEXED BY WS-CL-IX.
               10  WS-CL-TB-KEY                PIC X(06).
               10  WS-CL-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-CL-TB-TXT                PIC X(20).
               10  WS-CL-TB-EFF                PIC 9(05).
               10  WS-CL-TB-EXP                PIC 9(05).
       01  WS-CL-WORK-GROUP-1.
           05  WS-CL-G1-REGION             PIC S9(11)V9(02) COMP-3.
           05  WS-CL-G1-CARRIER            PIC X(20).
           05  WS-CL-G1-BAND               PIC S9(11)V9(02) COMP-3.
           05  WS-CL-G1-TARGET             PIC X(20).
           05  WS-CL-G1-INVOICE            PIC S9(09) COMP-3.
           05  WS-CL-G1-STATUS             PIC X(20).
           05  WS-CL-G1-CYCLE              PIC 9(05).
       01  WS-CL-WORK-GROUP-2.
           05  WS-CL-G2-JURIS              PIC 9(07).
           05  WS-CL-G2-STATE              PIC X(20).
           05  WS-CL-G2-SEQ                PIC S9(11)V9(02) COMP-3.
       01  WS-CL-WORK-GROUP-3.
           05  WS-CL-G3-INVOICE            PIC X(10).
           05  WS-CL-G3-MEDIA              PIC 9(07).
           05  WS-CL-G3-BAND               PIC S9(09) COMP-3.
           05  WS-CL-G3-TARIFF             PIC 9(05).
           05  WS-CL-G3-REGION             PIC X(20).
       01  WS-CL-WORK-GROUP-4.
           05  WS-CL-G4-LEVEL              PIC S9(11)V9(02) COMP-3.
           05  WS-CL-G4-SEQ                PIC X(20).
           05  WS-CL-G4-CIRCUIT            PIC 9(07).
           05  WS-CL-G4-JURIS              PIC X(10).
           05  WS-CL-G4-TARIFF             PIC S9(11)V9(02) COMP-3.
           05  WS-CL-G4-TARGET             PIC S9(11)V9(02) COMP-3.
           05  WS-CL-G4-INVOICE            PIC X(20).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 50.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUCV22 - CENTURY FIELD CONVERSION'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-CL-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-CL-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9981.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-CL-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-CL-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT LEGIN.
           IF WS-FS-INPUT NOT = '00'
               DISPLAY 'LEGIN FILE STATUS = ' WS-FS-INPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON LEGIN - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN INPUT EMIIN.
           IF WS-FS-INPUT NOT = '00'
               DISPLAY 'EMIIN FILE STATUS = ' WS-FS-INPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON EMIIN - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN INPUT PCKIN.
           IF WS-FS-INPUT NOT = '00'
               DISPLAY 'PCKIN FILE STATUS = ' WS-FS-INPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON PCKIN - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT TGTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               DISPLAY 'TGTOUT FILE STATUS = ' WS-FS-OUTPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON TGTOUT - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CNVOUT.
           IF WS-FS-OUTPUT NOT = '00'
               DISPLAY 'CNVOUT FILE STATUS = ' WS-FS-OUTPUT
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON CNVOUT - CHECK THE ALLOCATION' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SUSOUT.
           IF WS-FS-SUSPENSE NOT = '00'
               DISPLAY 'SUSOUT FILE STATUS = ' WS-FS-SUSPENSE
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OPEN FAILED ON SUSOUT - CHECK THE ALLOCATION' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-CL-CYCLE-YYDDD.
           COMPUTE WS-CL-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-CL-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-CL-CNT-01.
           MOVE 0 TO WS-CL-CNT-03.
           MOVE 0 TO WS-CL-CNT-02.
       P1200-EXIT.
           EXIT.
      * A FAILURE HERE SUSPENDS THE RECORD, IT DOES NOT STOP THE STEP.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-CL-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-CL-TAB-CNT NOT < 750
               MOVE 'Y' TO WS-CL-SW-01
               ADD 1 TO WS-CL-CNT-02
           ELSE
               ADD 1 TO WS-CL-TAB-CNT
               SET WS-CL-IX TO WS-CL-TAB-CNT
               MOVE IC-STATE TO WS-CL-TB-KEY (WS-CL-IX)
               MOVE 0 TO WS-CL-TB-VAL (WS-CL-IX)
               MOVE SPACES TO WS-CL-TB-TXT (WS-CL-IX)
               ADD 1 TO WS-CL-CNT-04.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ LEGIN
               AT END MOVE 'Y' TO WS-CL-SW-01.
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
           PERFORM P2200-APPLY-RECORD THRU P2200-APPLY-RECORD-EXIT.
           IF WS-CL-ON-01
               PERFORM P2300-DERIVE-ZONE THRU P2300-DERIVE-ZONE-EXIT.
           PERFORM P2400-CONVERT-ZONE THRU P2400-CONVERT-ZONE-EXIT.
           IF WS-CL-ON-02
               PERFORM P2500-EXPAND-LAYOUT THRU
                   P2500-EXPAND-LAYOUT-EXIT.
           IF WS-CL-ON-03
               PERFORM P2600-SPLIT-SIGN THRU P2600-SPLIT-SIGN-EXIT.
           PERFORM P2700-EXPAND-ZONE THRU P2700-EXPAND-ZONE-EXIT.
           IF WS-CL-ON-05
               PERFORM P2800-SELECT-CENTURY THRU
                   P2800-SELECT-CENTURY-EXIT.
           PERFORM P2900-MATCH-FIELD THRU P2900-MATCH-FIELD-EXIT.
           IF WS-CL-ON-02
               PERFORM P21000-BUILD-FIELD THRU P21000-BUILD-FIELD-EXIT.
           IF WS-CL-ON-01
               PERFORM P21100-CHECK-RECORD THRU
                   P21100-CHECK-RECORD-EXIT.
           IF WS-CL-ON-04
               PERFORM P21200-SELECT-RECORD THRU
                   P21200-SELECT-RECORD-EXIT.
           PERFORM P21300-VALIDATE-PACKED THRU
               P21300-VALIDATE-PACKED-EXIT.
           PERFORM P21400-SPLIT-SIGN THRU P21400-SPLIT-SIGN-EXIT.
           PERFORM P21500-SELECT-PACKED THRU P21500-SELECT-PACKED-EXIT.
           PERFORM P21600-BUILD-RECORD THRU P21600-BUILD-RECORD-EXIT.
           IF WS-CL-ON-01
               PERFORM P21700-SPLIT-LAYOUT THRU
                   P21700-SPLIT-LAYOUT-EXIT.
           IF WS-CL-ON-05
               PERFORM P21800-RESOLVE-CENTURY THRU
                   P21800-RESOLVE-CENTURY-EXIT.
           PERFORM P21900-SELECT-SIGN THRU P21900-SELECT-SIGN-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ LEGIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-APPLY-RECORD.
           MOVE 0 TO WS-CL-QTY-02.
           MOVE 0 TO WS-CL-QTY-03.
           MOVE 0 TO WS-CL-QTY-04.
           MOVE 0 TO WS-CL-AMT-05.
           MOVE 0 TO WS-CL-AMT-03.
           MOVE 'N' TO WS-CL-SW-05.
           IF WS-CL-TAB-CNT > 0
               PERFORM P250-COMPARE-SIGN THRU P250-COMPARE-SIGN-EXIT
               VARYING WS-CL-SUB-02 FROM 1 BY 1
               UNTIL WS-CL-SUB-02 > WS-CL-TAB-CNT
               OR WS-CL-SW-05 = 'Y'.
       P2200-APPLY-RECORD-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P2300-DERIVE-ZONE.
           MOVE 'N' TO WS-CL-SW-04.
           IF WS-CL-TXT-05 NOT = WS-CL-TXT-04
               MOVE 'Y' TO WS-CL-SW-04
               MOVE WS-CL-TXT-05 TO WS-CL-TXT-04
               ADD 1 TO WS-CL-CNT-08.
       P2300-DERIVE-ZONE-EXIT.
           EXIT.
       P2400-CONVERT-ZONE.
           IF IC-CODE = 'D'
               ADD 1 TO WS-CL-CNT-07
           ELSE
               IF IC-CODE = 'S'
                   ADD 1 TO WS-CL-CNT-05
               ELSE
                   IF IC-CODE = 'D'
                       ADD 1 TO WS-CL-CNT-07
                   ELSE
                       ADD 1 TO WS-CL-CNT-10.
       P2400-CONVERT-ZONE-EXIT.
           EXIT.
       P2500-EXPAND-LAYOUT.
           MOVE 'Y' TO WS-CL-SW-04.
           IF IC-STATUS2 < 26
               MOVE 'N' TO WS-CL-SW-04
               ADD 1 TO WS-CL-CNT-11.
           IF IC-STATUS2 > 4884
               MOVE 'N' TO WS-CL-SW-04
               ADD 1 TO WS-CL-CNT-01.
       P2500-EXPAND-LAYOUT-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P2600-SPLIT-SIGN.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-CL-TXT-02 TO PC-COL-001-020.
           MOVE WS-CL-TXT-07 TO PC-COL-021-060.
           MOVE WS-CL-AMT-03 TO WS-CL-AMT-EDIT.
           MOVE WS-CL-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P2600-SPLIT-SIGN-EXIT.
           EXIT.
       P2700-EXPAND-ZONE.
           CALL 'CABPARMR' USING WS-CL-TXT-06 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-CL-CNT-05.
       P2700-EXPAND-ZONE-EXIT.
           EXIT.
       P2800-SELECT-CENTURY.
           IF WS-CL-AMT-07 < 14
               MOVE 14 TO WS-CL-AMT-07
               ADD 1 TO WS-CL-CNT-10.
           IF WS-CL-AMT-07 > 5925
               MOVE 5925 TO WS-CL-AMT-07
               ADD 1 TO WS-CL-CNT-08.
       P2800-SELECT-CENTURY-EXIT.
           EXIT.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P2900-MATCH-FIELD.
           MOVE 0 TO WS-CL-CNT-12.
           INSPECT WS-CL-TXT-05 TALLYING WS-CL-CNT-12
               FOR ALL SPACES.
           INSPECT WS-CL-TXT-05 REPLACING ALL LOW-VALUES BY SPACES.
       P2900-MATCH-FIELD-EXIT.
           EXIT.
       P21000-BUILD-FIELD.
           MOVE SPACES TO WS-CL-TXT-02.
           STRING IC-REGION2 DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IC-CENTRE DELIMITED BY SIZE
               INTO WS-CL-TXT-02.
       P21000-BUILD-FIELD-EXIT.
           EXIT.
       P21100-CHECK-RECORD.
           MOVE WS-CL-AMT-04 TO WS-CL-AMT-05.
           IF WS-CL-AMT-05 < 0
               COMPUTE WS-CL-AMT-05 = 0 - WS-CL-AMT-04.
       P21100-CHECK-RECORD-EXIT.
           EXIT.
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT
      * PRECEDES THIS PROGRAM IN THE JOB.
       P21200-SELECT-RECORD.
           MOVE SPACES TO CABS-CL-OUT-RECORD.
           MOVE IC-SOURCE TO OC-OCN.
           MOVE IC-TARGET2 TO OC-MEDIA.
           MOVE IC-REGION2 TO OC-LEVEL.
           MOVE IC-STATE2 TO OC-BAND.
           MOVE IC-TARGET2 TO OC-CLASS.
           MOVE IC-STATE2 TO OC-SOURCE.
           MOVE IC-TARGET TO OC-SOURCE2.
           WRITE CABS-CL-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P21200-SELECT-RECORD-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P21300-VALIDATE-PACKED.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-RATE-NOT-FOUND TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-CL-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
       P21300-VALIDATE-PACKED-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P21400-SPLIT-SIGN.
           MOVE IC-PERIOD3 TO WS-CL-TXT-08.
           MOVE IC-GROUP TO WS-CL-TXT-04.
           MOVE IC-STATE TO WS-CL-TXT-09.
           ADD 1 TO WS-CL-CNT-08.
       P21400-SPLIT-SIGN-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P21500-SELECT-PACKED.
           ADD IC-STATE TO WS-CL-QTY-03.
           COMPUTE WS-CL-AMT-07 ROUNDED = WS-CL-QTY-03 * WS-CL-QTY-04.
           ADD WS-CL-AMT-07 TO WS-CL-AMT-04.
       P21500-SELECT-PACKED-EXIT.
           EXIT.
       P21600-BUILD-RECORD.
           IF WS-CL-AMT-06 NOT = 0
               COMPUTE WS-CL-QTY-04 = WS-CL-AMT-01 * 100 / WS-CL-AMT-06
           ELSE
               MOVE 0 TO WS-CL-QTY-04.
       P21600-BUILD-RECORD-EXIT.
           EXIT.
       P21700-SPLIT-LAYOUT.
           UNSTRING WS-CL-TXT-05 DELIMITED BY '/'
               INTO WS-CL-TXT-04
               WS-CL-TXT-03
               TALLYING IN WS-CL-CNT-07.
       P21700-SPLIT-LAYOUT-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P21800-RESOLVE-CENTURY.
           CALL 'CABHASH' USING IC-PERIOD3 WS-ACC-OCN-HASH.
           ADD WS-CL-CNT-02 TO WS-ACC-SEQ-HASH.
       P21800-RESOLVE-CENTURY-EXIT.
           EXIT.
       P21900-SELECT-SIGN.
           MOVE 0 TO WS-CL-QTY-05.
           MOVE 0 TO WS-CL-QTY-02.
           MOVE 0 TO WS-CL-QTY-01.
           MOVE 0 TO WS-CL-AMT-02.
       P21900-SELECT-SIGN-EXIT.
           EXIT.
       P250-COMPARE-SIGN.
           SET WS-CL-IX TO WS-CL-SUB-02.
           IF WS-CL-TB-KEY (WS-CL-IX) = IC-PERIOD
               MOVE 'Y' TO WS-CL-SW-03
               MOVE WS-CL-TB-VAL (WS-CL-IX) TO WS-CL-QTY-04
               MOVE WS-CL-SUB-02 TO WS-CL-SUB-02.
       P250-COMPARE-SIGN-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-STAGE-LAYOUT.
           MOVE 0 TO WS-CL-QTY-01.
           MOVE 0 TO WS-CL-QTY-04.
           MOVE 0 TO WS-CL-QTY-03.
           MOVE 0 TO WS-CL-AMT-05.
       P3100-STAGE-LAYOUT-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P3200-RELEASE-RECORD.
           MOVE SPACES TO WS-CL-TXT-06.
           STRING IC-GROUP DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IC-GROUP DELIMITED BY SIZE
               INTO WS-CL-TXT-06.
       P3200-RELEASE-RECORD-EXIT.
           EXIT.
       P3300-EMIT-CENTURY.
           MOVE SPACES TO CABS-CL-OUT-RECORD.
           MOVE IC-REGION2 TO OC-OCN.
           MOVE IC-CENTRE TO OC-MEDIA.
           MOVE IC-GROUP TO OC-LEVEL.
           MOVE IC-CODE TO OC-BAND.
           MOVE IC-CODE TO OC-CLASS.
           MOVE IC-GROUP TO OC-SOURCE.
           MOVE IC-REGION2 TO OC-SOURCE2.
           MOVE IC-TARGET TO OC-TYPE.
           MOVE IC-REGION2 TO OC-JURIS.
           WRITE CABS-CL-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P3300-EMIT-CENTURY-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P3400-POST-ZONE.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-BAN-UNKNOWN TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-CL-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
           MOVE CABS-SUSPENSE-RECORD TO CABS-SUSOUT-RECORD.
           WRITE CABS-SUSOUT-RECORD.
       P3400-POST-ZONE-EXIT.
           EXIT.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P3500-EMIT-PACKED.
           MOVE IC-PERIOD2 TO WS-CL-TXT-06.
           MOVE IC-STATE TO WS-CL-TXT-09.
           MOVE IC-STATE2 TO WS-CL-TXT-04.
           MOVE IC-SOURCE TO WS-CL-TXT-02.
           ADD 1 TO WS-CL-CNT-02.
       P3500-EMIT-PACKED-EXIT.
           EXIT.
       P3600-POST-PACKED.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-CL-TXT-09 TO PC-COL-001-020.
           MOVE WS-CL-TXT-03 TO PC-COL-021-060.
           MOVE WS-CL-AMT-05 TO WS-CL-AMT-EDIT.
           MOVE WS-CL-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P3600-POST-PACKED-EXIT.
           EXIT.
      * S400-SECONDARY-PROCESSING SECTION
       S400-SECONDARY-PROCESSING SECTION.
       P4000-SECONDARY.
           PERFORM P4100-AUDIT-PERIOD THRU P4100-AUDIT-PERIOD-EXIT.
           PERFORM P4200-COMPARE-SOURCE THRU P4200-COMPARE-SOURCE-EXIT.
           PERFORM P4300-NORMALISE-FIELD THRU
               P4300-NORMALISE-FIELD-EXIT.
           PERFORM P4400-ADJUST-OCN THRU P4400-ADJUST-OCN-EXIT.
           PERFORM P4500-REPORT-PERIOD THRU P4500-REPORT-PERIOD-EXIT.
           PERFORM P4600-TRACE-FIELD THRU P4600-TRACE-FIELD-EXIT.
           PERFORM P4700-RECONCILE-PERIOD THRU
               P4700-RECONCILE-PERIOD-EXIT.
           PERFORM P4800-RECONCILE-CIRCUIT THRU
               P4800-RECONCILE-CIRCUIT-EXIT.
           PERFORM P4900-TRACE-TARGET THRU P4900-TRACE-TARGET-EXIT.
           PERFORM P41000-AUDIT-STATUS THRU P41000-AUDIT-STATUS-EXIT.
           PERFORM P41100-COMPARE-STATUS THRU
               P41100-COMPARE-STATUS-EXIT.
           PERFORM P41200-SUMMARISE-ELEM THRU
               P41200-SUMMARISE-ELEM-EXIT.
           PERFORM P41300-ADJUST-SEGMENT THRU
               P41300-ADJUST-SEGMENT-EXIT.
           PERFORM P41400-TRACE-CODE THRU P41400-TRACE-CODE-EXIT.
           PERFORM P41500-REPORT-BAN THRU P41500-REPORT-BAN-EXIT.
           PERFORM P41600-RECONCILE-INVOICE THRU
               P41600-RECONCILE-INVOICE-EXIT.
           PERFORM P41700-REPORT-GROUP THRU P41700-REPORT-GROUP-EXIT.
           PERFORM P41800-SUMMARISE-GROUP THRU
               P41800-SUMMARISE-GROUP-EXIT.
           PERFORM P41900-RECONCILE-PERIOD THRU
               P41900-RECONCILE-PERIOD-EXIT.
           PERFORM P42000-TRACE-ELEM THRU P42000-TRACE-ELEM-EXIT.
           PERFORM P42100-TRACE-CARRIER THRU P42100-TRACE-CARRIER-EXIT.
           PERFORM P42200-ADJUST-BAND THRU P42200-ADJUST-BAND-EXIT.
           PERFORM P42300-COMPARE-CENTURY THRU
               P42300-COMPARE-CENTURY-EXIT.
           PERFORM P42400-TRACE-TARGET THRU P42400-TRACE-TARGET-EXIT.
           PERFORM P42500-COMPARE-FIELD THRU P42500-COMPARE-FIELD-EXIT.
           PERFORM P42600-REPORT-JURIS THRU P42600-REPORT-JURIS-EXIT.
       P4000-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P4100-AUDIT-PERIOD.
           MOVE IC-MEDIA TO WS-CL-TXT-08.
           MOVE IC-MEDIA TO WS-CL-TXT-09.
           MOVE IC-STATUS2 TO WS-CL-TXT-07.
           ADD 1 TO WS-CL-CNT-10.
       P4100-AUDIT-PERIOD-EXIT.
           EXIT.
       P4200-COMPARE-SOURCE.
           MOVE 0 TO WS-CL-QTY-02.
           MOVE 0 TO WS-CL-QTY-03.
           MOVE 0 TO WS-CL-QTY-05.
           MOVE 0 TO WS-CL-AMT-05.
           MOVE 0 TO WS-CL-AMT-08.
       P4200-COMPARE-SOURCE-EXIT.
           EXIT.
       P4300-NORMALISE-FIELD.
           MOVE SPACES TO CABS-CL-OUT-RECORD.
           MOVE IC-TARGET TO OC-OCN.
           MOVE IC-MEDIA TO OC-MEDIA.
           MOVE IC-TARGET TO OC-LEVEL.
           MOVE IC-MEDIA TO OC-BAND.
           MOVE IC-GROUP TO OC-CLASS.
           WRITE CABS-CL-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P4300-NORMALISE-FIELD-EXIT.
           EXIT.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P4400-ADJUST-OCN.
           MOVE 'Y' TO WS-CL-SW-01.
           IF IC-PERIOD3 < 12
               MOVE 'N' TO WS-CL-SW-01
               ADD 1 TO WS-CL-CNT-01.
           IF IC-PERIOD3 > 8302
               MOVE 'N' TO WS-CL-SW-01
               ADD 1 TO WS-CL-CNT-05.
       P4400-ADJUST-OCN-EXIT.
           EXIT.
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT
      * PRECEDES THIS PROGRAM IN THE JOB.
       P4500-REPORT-PERIOD.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-CL-TXT-01 TO PC-COL-001-020.
           MOVE WS-CL-TXT-04 TO PC-COL-021-060.
           MOVE WS-CL-AMT-02 TO WS-CL-AMT-EDIT.
           MOVE WS-CL-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P4500-REPORT-PERIOD-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P4600-TRACE-FIELD.
           ADD IC-REGION2 TO WS-CL-QTY-03.
           COMPUTE WS-CL-AMT-05 ROUNDED = WS-CL-QTY-03 * WS-CL-QTY-01.
           ADD WS-CL-AMT-05 TO WS-CL-AMT-08.
       P4600-TRACE-FIELD-EXIT.
           EXIT.
       P4700-RECONCILE-PERIOD.
           MOVE 'N' TO WS-CL-SW-04.
           IF WS-CL-TXT-02 NOT = WS-CL-TXT-08
               MOVE 'Y' TO WS-CL-SW-04
               MOVE WS-CL-TXT-02 TO WS-CL-TXT-08
               ADD 1 TO WS-CL-CNT-11.
       P4700-RECONCILE-PERIOD-EXIT.
           EXIT.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P4800-RECONCILE-CIRCUIT.
           UNSTRING WS-CL-TXT-02 DELIMITED BY '/'
               INTO WS-CL-TXT-01
               WS-CL-TXT-06
               TALLYING IN WS-CL-CNT-10.
       P4800-RECONCILE-CIRCUIT-EXIT.
           EXIT.
       P4900-TRACE-TARGET.
           MOVE 0 TO WS-CL-CNT-09.
           INSPECT WS-CL-TXT-05 TALLYING WS-CL-CNT-09
               FOR ALL SPACES.
           INSPECT WS-CL-TXT-05 REPLACING ALL LOW-VALUES BY SPACES.
       P4900-TRACE-TARGET-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P41000-AUDIT-STATUS.
           IF IC-STATUS = 'S'
               ADD 1 TO WS-CL-CNT-09
           ELSE
               IF IC-STATUS = 'B'
                   ADD 1 TO WS-CL-CNT-02
               ELSE
                   IF IC-STATUS = 'D'
                       ADD 1 TO WS-CL-CNT-11
                   ELSE
                       ADD 1 TO WS-CL-CNT-04.
       P41000-AUDIT-STATUS-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P41100-COMPARE-STATUS.
           CALL 'CABFMTR' USING WS-CL-TXT-05 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-CL-CNT-07.
       P41100-COMPARE-STATUS-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P41200-SUMMARISE-ELEM.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-DUP-SEQ TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-CL-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
           MOVE CABS-SUSPENSE-RECORD TO CABS-SUSOUT-RECORD.
           WRITE CABS-SUSOUT-RECORD.
       P41200-SUMMARISE-ELEM-EXIT.
           EXIT.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P41300-ADJUST-SEGMENT.
           IF WS-CL-AMT-01 < 12
               MOVE 12 TO WS-CL-AMT-01
               ADD 1 TO WS-CL-CNT-03.
           IF WS-CL-AMT-01 > 10836
               MOVE 10836 TO WS-CL-AMT-01
               ADD 1 TO WS-CL-CNT-02.
       P41300-ADJUST-SEGMENT-EXIT.
           EXIT.
      * A FAILURE HERE SUSPENDS THE RECORD, IT DOES NOT STOP THE STEP.
       P41400-TRACE-CODE.
           IF WS-CL-AMT-05 NOT = 0
               COMPUTE WS-CL-QTY-03 = WS-CL-AMT-03 * 100 / WS-CL-AMT-05
           ELSE
               MOVE 0 TO WS-CL-QTY-03.
       P41400-TRACE-CODE-EXIT.
           EXIT.
       P41500-REPORT-BAN.
           CALL 'CABHASH' USING IC-SEQ WS-ACC-OCN-HASH.
           ADD WS-CL-CNT-06 TO WS-ACC-SEQ-HASH.
       P41500-REPORT-BAN-EXIT.
           EXIT.
       P41600-RECONCILE-INVOICE.
           MOVE SPACES TO WS-CL-TXT-05.
           STRING IC-PERIOD3 DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IC-STATUS DELIMITED BY SIZE
               INTO WS-CL-TXT-05.
       P41600-RECONCILE-INVOICE-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P41700-REPORT-GROUP.
           MOVE WS-CL-AMT-07 TO WS-CL-AMT-08.
           IF WS-CL-AMT-08 < 0
               COMPUTE WS-CL-AMT-08 = 0 - WS-CL-AMT-07.
       P41700-REPORT-GROUP-EXIT.
           EXIT.
       P41800-SUMMARISE-GROUP.
           MOVE IC-TARIFF TO WS-CL-TXT-09.
           MOVE IC-STATUS TO WS-CL-TXT-06.
           MOVE IC-TARGET2 TO WS-CL-TXT-04.
           ADD 1 TO WS-CL-CNT-04.
       P41800-SUMMARISE-GROUP-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P41900-RECONCILE-PERIOD.
           MOVE 0 TO WS-CL-QTY-05.
           MOVE 0 TO WS-CL-QTY-04.
           MOVE 0 TO WS-CL-AMT-04.
           MOVE 0 TO WS-CL-AMT-01.
       P41900-RECONCILE-PERIOD-EXIT.
           EXIT.
       P42000-TRACE-ELEM.
           MOVE SPACES TO CABS-CL-OUT-RECORD.
           MOVE IC-TARGET TO OC-OCN.
           MOVE IC-REGION TO OC-MEDIA.
           MOVE IC-REGION2 TO OC-LEVEL.
           MOVE IC-TARGET TO OC-BAND.
           WRITE CABS-CL-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P42000-TRACE-ELEM-EXIT.
           EXIT.
       P42100-TRACE-CARRIER.
           MOVE 'Y' TO WS-CL-SW-04.
           IF IC-CODE < 24
               MOVE 'N' TO WS-CL-SW-04
               ADD 1 TO WS-CL-CNT-05.
           IF IC-CODE > 1354
               MOVE 'N' TO WS-CL-SW-04
               ADD 1 TO WS-CL-CNT-01.
       P42100-TRACE-CARRIER-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P42200-ADJUST-BAND.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-CL-TXT-08 TO PC-COL-001-020.
           MOVE WS-CL-TXT-01 TO PC-COL-021-060.
           MOVE WS-CL-AMT-02 TO WS-CL-AMT-EDIT.
           MOVE WS-CL-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P42200-ADJUST-BAND-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P42300-COMPARE-CENTURY.
           ADD IC-SOURCE TO WS-CL-QTY-05.
           COMPUTE WS-CL-AMT-02 ROUNDED = WS-CL-QTY-05 * WS-CL-QTY-04.
           ADD WS-CL-AMT-02 TO WS-CL-AMT-08.
       P42300-COMPARE-CENTURY-EXIT.
           EXIT.
       P42400-TRACE-TARGET.
           MOVE 'N' TO WS-CL-SW-03.
           IF WS-CL-TXT-03 NOT = WS-CL-TXT-04
               MOVE 'Y' TO WS-CL-SW-03
               MOVE WS-CL-TXT-03 TO WS-CL-TXT-04
               ADD 1 TO WS-CL-CNT-12.
       P42400-TRACE-TARGET-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P42500-COMPARE-FIELD.
           UNSTRING WS-CL-TXT-06 DELIMITED BY '/'
               INTO WS-CL-TXT-07
               WS-CL-TXT-03
               TALLYING IN WS-CL-CNT-04.
       P42500-COMPARE-FIELD-EXIT.
           EXIT.
       P42600-REPORT-JURIS.
           MOVE 0 TO WS-CL-CNT-05.
           INSPECT WS-CL-TXT-05 TALLYING WS-CL-CNT-05
               FOR ALL SPACES.
           INSPECT WS-CL-TXT-05 REPLACING ALL LOW-VALUES BY SPACES.
       P42600-REPORT-JURIS-EXIT.
           EXIT.
           MOVE 0 TO WS-CL-QTY-04.
           PERFORM P370-WALK-ZONE THRU P370-WALK-ZONE-EXIT
               VARYING WS-CL-SUB-05 FROM 1 BY 1
               UNTIL WS-CL-SUB-05 > WS-CL-TAB-CNT.
       P370-WALK-ZONE.
           SET WS-CL-IX TO WS-CL-SUB-03.
           IF WS-CL-TB-KEY (WS-CL-IX) NOT = SPACES
               ADD WS-CL-TB-VAL (WS-CL-IX) TO WS-CL-QTY-03.
       P370-WALK-ZONE-EXIT.
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
           MOVE WS-READ-CNT TO WS-CL-CNT-EDIT.
           MOVE WS-CL-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL OUT' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-CL-CNT-EDIT.
           MOVE WS-CL-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL SUSPENDED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-CL-CNT-EDIT.
           MOVE WS-CL-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL SUMMARISED' TO PC-COL-001-020.
           MOVE WS-SUMM-CNT TO WS-CL-CNT-EDIT.
           MOVE WS-CL-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL CARRIED FWD' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-CL-CNT-EDIT.
           MOVE WS-CL-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-CL-CNT-01 TO WS-CL-CNT-EDIT.
           MOVE WS-CL-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 02' TO PC-COL-001-020.
           MOVE WS-CL-CNT-02 TO WS-CL-CNT-EDIT.
           MOVE WS-CL-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE SPACES TO CT-FILLER.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE 0 TO CT-RC.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE 8 TO CT-STEP-SEQ.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE WS-CL-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
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
           CLOSE LEGIN.
           CLOSE EMIIN.
           CLOSE PCKIN.
           CLOSE TGTOUT.
           CLOSE CNVOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABUCV22 - RUN COMPLETE'.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  CL-CNT-03 = ' WS-CL-CNT-03.
           DISPLAY '  CL-CNT-04 = ' WS-CL-CNT-04.
           DISPLAY '  CL-CNT-06 = ' WS-CL-CNT-06.
           DISPLAY '  CL-CNT-09 = ' WS-CL-CNT-09.
       P9000-EXIT.
           EXIT.
