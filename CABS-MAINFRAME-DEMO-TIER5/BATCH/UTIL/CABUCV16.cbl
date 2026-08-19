      *****************************************************************
      * CABUCV16 - EMI FORMAT CONVERSION                              *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               LEGIN   TELCABS.CABS.LEGIN          (LOCAL)     *
      *               EMIIN   TELCABS.CABS.EMIIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               NEWOUT  TELCABS.CABS.NEWOUT         (LOCAL)     *
      *               CNVOUT  TELCABS.CABS.CNVOUT         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * REVISION HISTORY                                              *
      *   V1.00  1993-05-18  L.FERREIRA   INITIAL RELEASE             *
      *   V1.02  2006-07-10  J.M.CASTILLO SECOND OUTPUT FILE ADDED FOR*
      *                      THE FACTOR STUDY                         *
      *   V1.03  2010-12-27  M.DELACROIX  RECOMPILE ONLY - COPYBOOK   *
      *                      CHANGE UPSTREAM                          *
      *   V1.07  2016-08-16  T.YAMASHITA  EFFECTIVE DATE FILTER ADDED *
      *                      PER AUDIT FINDING                        *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUCV16.
       AUTHOR. TELCABS APPLICATIONS - FILE CONVERSION.
      *****************************************************************
      * EMI FORMAT CONVERSION. THE STEP RUNS ONCE PER BILL CYCLE AND  *
      * IS RERUN FROM THE TOP IF IT FAILS.                            *
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
           SELECT LEGIN ASSIGN TO UT-S-LEGIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT EMIIN ASSIGN TO UT-S-EMIIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT NEWOUT ASSIGN TO UT-S-NEWOUT
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
      * LEGIN - CATALOGUED GENERATION DATA GROUP.
       FD  LEGIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 130 CHARACTERS.
       01  CABS-CX-IN-RECORD.
           05  IC-CYCLE                    PIC X(04).
           05  IC-CENTRE                   PIC X(10).
           05  IC-MEDIA                    PIC X(04).
           05  IC-REGION                   PIC X(02).
           05  IC-GROUP                    PIC 9(03).
           05  IC-STATUS                   PIC X(16).
           05  IC-SOURCE                   PIC X(10).
           05  IC-CIRCUIT                  PIC S9(13)V9(02) COMP-3.
           05  IC-TARGET                   PIC S9(11) COMP-3.
           05  IC-JURIS                    PIC S9(07) COMP-3.
           05  IC-PERIOD                   PIC 9(07).
           05  IC-CIRCUIT2                 PIC 9(03).
           05  IC-TARGET2                  PIC S9(11)V9(05) COMP-3.
           05  IC-BAN                      PIC S9(07) COMP-3.
           05  IC-SOURCE2                  PIC 9(05).
           05  IC-BAND                     PIC S9(07)V9(05) COMP-3.
           05  IC-CLASS                    PIC X(03).
           05  IC-TARIFF                   PIC X(08).
           05  IC-CENTRE2                  PIC 9(07).
           05  IC-CIRCUIT3                 PIC S9(11)V9(05) COMP-3.
           05  CX-FILL-01                  PIC X(1).
      * EMIIN - CATALOGUED GENERATION DATA GROUP.
       FD  EMIIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 130 CHARACTERS.
       01  CABS-CX-ALT1-RECORD.
           05  A1-SOURCE                   PIC S9(13) COMP-3.
           05  A1-TARIFF                   PIC S9(11) COMP-3.
           05  A1-STATUS                   PIC X(16).
           05  A1-SOURCE2                  PIC S9(11) COMP-3.
           05  A1-PERIOD                   PIC 9(09).
           05  A1-INVOICE                  PIC X(16).
           05  A1-SEQ                      PIC S9(09)V9(05) COMP-3.
           05  A1-PERIOD2                  PIC 9(05).
           05  A1-SEGMENT                  PIC S9(13) COMP-3.
           05  A1-CARRIER                  PIC X(08).
           05  CX-FILL-02                  PIC X(42).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-CX-VIEW1 REDEFINES CABS-CX-IN-RECORD.
           05  R0C-CIRCUIT                 PIC 9(07).
           05  R0C-TYPE                    PIC S9(09) COMP-3.
           05  R0C-TARGET                  PIC S9(05) COMP-3.
           05  R0C-SEGMENT                 PIC S9(07)V9(05) COMP-3.
           05  R0C-INVOICE                 PIC S9(07)V9(02) COMP-3.
           05  R0C-BAN                     PIC 9(06).
           05  R0C-REST                    PIC X(97).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-CX-VIEW2 REDEFINES CABS-CX-IN-RECORD.
           05  R1C-INVOICE                 PIC S9(13)V9(02) COMP-3.
           05  R1C-CENTRE                  PIC 9(05).
           05  R1C-INVOICE2                PIC X(20).
           05  R1C-CLASS                   PIC 9(06).
           05  R1C-MEDIA                   PIC S9(11)V9(02) COMP-3.
           05  R1C-MEDIA2                  PIC S9(11)V9(02) COMP-3.
           05  R1C-REST                    PIC X(77).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-CX-VIEW3 REDEFINES CABS-CX-IN-RECORD.
           05  R2C-GROUP                   PIC S9(05) COMP-3.
           05  R2C-GROUP2                  PIC S9(05) COMP-3.
           05  R2C-ACCOUNT                 PIC S9(13) COMP-3.
           05  R2C-BAND                    PIC X(06).
           05  R2C-REST                    PIC X(111).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-CX-VIEW4 REDEFINES CABS-CX-IN-RECORD.
           05  R3C-JURIS                   PIC 9(06).
           05  R3C-REGION                  PIC 9(04).
           05  R3C-CENTRE                  PIC S9(09)V9(02) COMP-3.
           05  R3C-JURIS2                  PIC X(08).
           05  R3C-TARIFF                  PIC S9(11) COMP-3.
           05  R3C-STATE                   PIC S9(13)V9(02) COMP-3.
           05  R3C-CODE                    PIC X(10).
           05  R3C-CARRIER                 PIC 9(02).
           05  R3C-REST                    PIC X(80).
      * NEWOUT - WORK FILE, DELETED AT STEP END.
       FD  NEWOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-CX-OUT-RECORD.
           05  OC-BAND                     PIC 9(04).
           05  OC-CODE                     PIC S9(11)V9(05) COMP-3.
           05  OC-CODE2                    PIC 9(02).
           05  OC-CODE3                    PIC 9(04).
           05  OC-PERIOD                   PIC S9(11)V9(02) COMP-3.
           05  OC-JURIS                    PIC S9(07) COMP-3.
           05  OC-CENTRE                   PIC X(06).
           05  OC-ACCOUNT                  PIC 9(04).
           05  OC-ACCOUNT2                 PIC 9(02).
           05  OC-TARIFF                   PIC S9(15) COMP-3.
           05  OC-BAND2                    PIC X(06).
           05  OC-JURIS2                   PIC S9(07)V9(02) COMP-3.
           05  OC-JURIS3                   PIC X(13).
           05  CX-FILL-03                  PIC X(6).
      * CNVOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  CNVOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-CX-OUT1-RECORD         PIC X(80).
      * SUSOUT - CATALOGUED GENERATION DATA GROUP.
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
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUCV16'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V2.04'.
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
       01  WS-PARM-CARD-R2 REDEFINES WS-PARM-CARD.
           05  PC2-LEAD                    PIC X(14).
           05  PC2-CYCLE-VIEW.
               10  PC2-CV-YY                   PIC 9(02).
               10  PC2-CV-DDD                  PIC 9(03).
           05  PC2-REST                    PIC X(61).
       01  WS-COUNT-AREA.
           05  WS-CX-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CX-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CX-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CX-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CX-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CX-CNT-06                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CX-CNT-07                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-CX-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CX-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CX-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CX-QTY-04                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CX-QTY-05                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CX-QTY-06                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-CX-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CX-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CX-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-CX-TXT-01                PIC X(08) VALUE SPACES.
           05  WS-CX-TXT-02                PIC X(30) VALUE SPACES.
           05  WS-CX-TXT-03                PIC X(08) VALUE SPACES.
           05  WS-CX-TXT-04                PIC X(10) VALUE SPACES.
           05  WS-CX-TXT-05                PIC X(30) VALUE SPACES.
           05  WS-CX-TXT-06                PIC X(26) VALUE SPACES.
           05  WS-CX-TXT-07                PIC X(12) VALUE SPACES.
           05  WS-CX-TXT-08                PIC X(20) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-CX-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-CX-ON-01                 VALUE 'Y'.
               88  WS-CX-OFF-01                VALUE 'N'.
           05  WS-CX-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-CX-ON-02                 VALUE 'Y'.
               88  WS-CX-OFF-02                VALUE 'N'.
           05  WS-CX-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-CX-ON-03                 VALUE 'Y'.
               88  WS-CX-OFF-03                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-CX-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CX-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CX-SUB-03                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CX-SUB-04                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. LOADED ONCE AT INITIALISATION AND SEARCHED WITH
      * A SUBSCRIPTED WALK.
       01  WS-CX-TABLE.
           05  WS-CX-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-CX-TB-ENTRY OCCURS 300 TIMES
                                       INDEXED BY WS-CX-IX.
               10  WS-CX-TB-KEY                PIC X(08).
               10  WS-CX-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-CX-TB-TXT                PIC X(40).
               10  WS-CX-TB-EFF                PIC 9(05).
               10  WS-CX-TB-EXP                PIC 9(05).
       01  WS-CX-WORK-GROUP-1.
           05  WS-CX-G1-CIRCUIT            PIC S9(11)V9(02) COMP-3.
           05  WS-CX-G1-SEQ                PIC 9(05).
           05  WS-CX-G1-MEDIA              PIC S9(11)V9(02) COMP-3.
           05  WS-CX-G1-SOURCE             PIC X(10).
           05  WS-CX-G1-CODE               PIC 9(05).
       01  WS-CX-WORK-GROUP-2.
           05  WS-CX-G2-GROUP              PIC X(20).
           05  WS-CX-G2-BAN                PIC X(10).
           05  WS-CX-G2-INVOICE            PIC X(20).
       01  WS-CX-WORK-GROUP-3.
           05  WS-CX-G3-JURIS              PIC X(20).
           05  WS-CX-G3-BAND               PIC 9(07).
           05  WS-CX-G3-CARRIER            PIC 9(07).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 60.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUCV16 - EMI FORMAT CONVERSION'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-CX-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-CX-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9972.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-CX-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-CX-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT LEGIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'LEGIN COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN INPUT EMIIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'EMIIN COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT NEWOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'NEWOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CNVOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CNVOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-CX-CYCLE-YYDDD.
           COMPUTE WS-CX-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-CX-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-CX-CNT-03.
           MOVE 0 TO WS-CX-CNT-05.
           MOVE 0 TO WS-CX-CNT-07.
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
           IF WS-CX-ON-01
               PERFORM P2200-BUILD-CENTURY THRU
                   P2200-BUILD-CENTURY-EXIT.
           PERFORM P2300-BUILD-SIGN THRU P2300-BUILD-SIGN-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ LEGIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P2200-BUILD-CENTURY.
           MOVE WS-CX-AMT-03 TO WS-CX-AMT-02.
           IF WS-CX-AMT-02 < 0
               COMPUTE WS-CX-AMT-02 = 0 - WS-CX-AMT-03.
       P2200-BUILD-CENTURY-EXIT.
           EXIT.
       P2300-BUILD-SIGN.
           MOVE 0 TO WS-CX-CNT-07.
           INSPECT WS-CX-TXT-02 TALLYING WS-CX-CNT-07
               FOR ALL SPACES.
           INSPECT WS-CX-TXT-02 REPLACING ALL LOW-VALUES BY SPACES.
       P2300-BUILD-SIGN-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
      * THE PRINT LINE IS BUILT COLUMN BY COLUMN. THE CARRIAGE CONTROL
      * CHARACTER CARRIES MEANING DOWNSTREAM.
       P3100-RELEASE-ZONE.
           MOVE 0 TO WS-CX-QTY-03.
           MOVE 0 TO WS-CX-QTY-06.
           MOVE 0 TO WS-CX-AMT-02.
       P3100-RELEASE-ZONE-EXIT.
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
           MOVE 'RECORDS WRITTEN' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-CX-CNT-EDIT.
           MOVE WS-CX-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS REJECTED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-CX-CNT-EDIT.
           MOVE WS-CX-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS SUMMARISED' TO PC-COL-001-020.
           MOVE WS-SUMM-CNT TO WS-CX-CNT-EDIT.
           MOVE WS-CX-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS CARRIED FWD' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-CX-CNT-EDIT.
           MOVE WS-CX-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS READ' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO WS-CX-CNT-EDIT.
           MOVE WS-CX-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-CX-CNT-01 TO WS-CX-CNT-EDIT.
           MOVE WS-CX-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE WS-CX-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE 4 TO CT-STEP-SEQ.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE SPACES TO CT-FILLER.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE WS-CX-TXT-03 TO CT-RESTART-KEY.
           MOVE WS-CX-CNT-03 TO CT-RC.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
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
           CLOSE NEWOUT.
           CLOSE CNVOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABUCV16 - STEP COMPLETE'.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  CX-CNT-02 = ' WS-CX-CNT-02.
           DISPLAY '  CX-CNT-06 = ' WS-CX-CNT-06.
           DISPLAY '  CX-CNT-05 = ' WS-CX-CNT-05.
       P9000-EXIT.
           EXIT.
