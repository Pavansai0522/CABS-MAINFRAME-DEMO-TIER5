      *****************************************************************
      * CABURT19 - RATE OVERRIDE TABLE LOAD                           *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               BNDIN   TELCABS.CABS.BNDIN          (LOCAL)     *
      *               MNTIN   TELCABS.CABS.MNTIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               RATOUT  TELCABS.CABS.RATOUT         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - NOTHING IS UPDATED IN PLACE        *
      * REVISION HISTORY                                              *
      *   V1.00  1987-08-01  A.BUKOWSKI   INITIAL RELEASE             *
      *   V1.02  1992-08-09  T.YAMASHITA  ROUNDING RULE TAKEN FROM THE*
      *                      RATE ROW                                 *
      *   V1.05  2003-06-03  A.BUKOWSKI   JOB PARAMETER MADE MANDATORY*
      *   V1.09  2005-08-07  R.T.WHEELER  RECOMPILE ONLY - COPYBOOK   *
      *                      CHANGE UPSTREAM                          *
      *   V1.11  2012-08-03  S.MARCHETTI  REPORT PAGINATION CORRECTED *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABURT19.
       AUTHOR. TELCABS APPLICATIONS - RATE TABLE MAINTENANCE.
      *****************************************************************
      * RATE OVERRIDE TABLE LOAD. THE STEP IS DRIVEN ENTIRELY FROM THE*
      * SYSIN PARM CARD AND THE DD ALLOCATIONS IN THE JOB.            *
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
           SELECT BNDIN ASSIGN TO UT-S-BNDIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT MNTIN ASSIGN TO UT-S-MNTIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
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
      * BNDIN - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  BNDIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 120 CHARACTERS.
       01  CABS-CJ-IN-RECORD.
           05  IC-JURIS                    PIC 9(03).
           05  IC-INVOICE                  PIC X(02).
           05  IC-CLASS                    PIC X(20).
           05  IC-INVOICE2                 PIC S9(15) COMP-3.
           05  IC-CARRIER                  PIC S9(13) COMP-3.
           05  IC-CLASS2                   PIC X(02).
           05  IC-SEGMENT                  PIC 9(04).
           05  IC-CLASS3                   PIC X(04).
           05  IC-CYCLE                    PIC X(13).
           05  IC-TYPE                     PIC S9(11)V9(05) COMP-3.
           05  IC-PERIOD                   PIC 9(03).
           05  IC-STATUS                   PIC X(02).
           05  IC-TYPE2                    PIC X(20).
           05  IC-STATE                    PIC X(16).
           05  CJ-FILL-01                  PIC X(7).
      * MNTIN - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  MNTIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 120 CHARACTERS.
       01  CABS-CJ-ALT1-RECORD.
           05  A1-CLASS                    PIC 9(05).
           05  A1-SEQ                      PIC S9(15) COMP-3.
           05  A1-STATUS                   PIC 9(02).
           05  A1-REGION                   PIC X(03).
           05  A1-SOURCE                   PIC S9(09)V9(02) COMP-3.
           05  A1-STATE                    PIC 9(09).
           05  A1-CENTRE                   PIC S9(13)V9(02) COMP-3.
           05  A1-TYPE                     PIC 9(04).
           05  A1-CARRIER                  PIC 9(05).
           05  CJ-FILL-02                  PIC X(70).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-CJ-VIEW1 REDEFINES CABS-CJ-IN-RECORD.
           05  R0C-CODE                    PIC 9(04).
           05  R0C-TARGET                  PIC 9(05).
           05  R0C-CODE2                   PIC X(13).
           05  R0C-JURIS                   PIC 9(03).
           05  R0C-TARIFF                  PIC 9(07).
           05  R0C-TARIFF2                 PIC S9(07)V9(05) COMP-3.
           05  R0C-REGION                  PIC X(16).
           05  R0C-REST                    PIC X(65).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-CJ-VIEW2 REDEFINES CABS-CJ-IN-RECORD.
           05  R1C-MEDIA                   PIC S9(13) COMP-3.
           05  R1C-BAND                    PIC X(06).
           05  R1C-TYPE                    PIC X(03).
           05  R1C-STATE                   PIC X(20).
           05  R1C-TYPE2                   PIC S9(13) COMP-3.
           05  R1C-MEDIA2                  PIC X(20).
           05  R1C-REST                    PIC X(57).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-CJ-VIEW3 REDEFINES CABS-CJ-IN-RECORD.
           05  R2C-CODE                    PIC S9(05) COMP-3.
           05  R2C-TYPE                    PIC X(20).
           05  R2C-STATUS                  PIC X(13).
           05  R2C-TARIFF                  PIC S9(13) COMP-3.
           05  R2C-TARIFF2                 PIC X(06).
           05  R2C-REST                    PIC X(71).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-CJ-VIEW4 REDEFINES CABS-CJ-IN-RECORD.
           05  R3C-CENTRE                  PIC S9(15) COMP-3.
           05  R3C-BAND                    PIC S9(07) COMP-3.
           05  R3C-GROUP                   PIC X(06).
           05  R3C-SEQ                     PIC X(04).
           05  R3C-BAND2                   PIC S9(07) COMP-3.
           05  R3C-SEQ2                    PIC S9(11)V9(02) COMP-3.
           05  R3C-REST                    PIC X(87).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-CJ-VIEW5 REDEFINES CABS-CJ-IN-RECORD.
           05  R4C-OCN                     PIC S9(11)V9(02) COMP-3.
           05  R4C-CODE                    PIC S9(13)V9(02) COMP-3.
           05  R4C-STATUS                  PIC 9(04).
           05  R4C-GROUP                   PIC 9(03).
           05  R4C-TYPE                    PIC X(10).
           05  R4C-BAND                    PIC X(08).
           05  R4C-INVOICE                 PIC X(02).
           05  R4C-OCN2                    PIC X(06).
           05  R4C-REST                    PIC X(72).
      * RATOUT - CATALOGUED GENERATION DATA GROUP.
       FD  RATOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 110 CHARACTERS.
       01  CABS-CJ-OUT-RECORD.
           05  OC-INVOICE                  PIC S9(15) COMP-3.
           05  OC-TARGET                   PIC 9(02).
           05  OC-CYCLE                    PIC 9(06).
           05  OC-TARIFF                   PIC S9(11)V9(02) COMP-3.
           05  OC-CYCLE2                   PIC 9(05).
           05  OC-BAND                     PIC S9(07)V9(02) COMP-3.
           05  OC-BAND2                    PIC X(06).
           05  OC-CYCLE3                   PIC X(16).
           05  OC-CYCLE4                   PIC S9(09) COMP-3.
           05  OC-REGION                   PIC 9(04).
           05  OC-CARRIER                  PIC 9(07).
           05  OC-BAN                      PIC S9(05) COMP-3.
           05  OC-GROUP                    PIC X(08).
           05  OC-REGION2                  PIC S9(11)V9(05) COMP-3.
           05  OC-PERIOD                   PIC S9(05) COMP-3.
           05  OC-LEVEL                    PIC 9(06).
           05  CJ-FILL-03                  PIC X(10).
      * SUSOUT - CATALOGUED GENERATION DATA GROUP.
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
      * RPTOUT - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
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
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABURT19'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V1.24'.
           05  WS-TABLE-LIMIT              PIC S9(04) COMP-3
                                         VALUE 200.
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
           05  WS-CJ-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CJ-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CJ-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CJ-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CJ-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CJ-CNT-06                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CJ-CNT-07                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CJ-CNT-08                PIC S9(09) COMP-3 VALUE 0.
           05  WS-CJ-CNT-09                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-CJ-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CJ-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CJ-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CJ-QTY-04                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CJ-QTY-05                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-CJ-QTY-06                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-CJ-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CJ-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CJ-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-CJ-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-CJ-TXT-01                PIC X(20) VALUE SPACES.
           05  WS-CJ-TXT-02                PIC X(20) VALUE SPACES.
           05  WS-CJ-TXT-03                PIC X(26) VALUE SPACES.
           05  WS-CJ-TXT-04                PIC X(10) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-CJ-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-CJ-ON-01                 VALUE 'Y'.
               88  WS-CJ-OFF-01                VALUE 'N'.
           05  WS-CJ-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-CJ-ON-02                 VALUE 'Y'.
               88  WS-CJ-OFF-02                VALUE 'N'.
           05  WS-CJ-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-CJ-ON-03                 VALUE 'Y'.
               88  WS-CJ-OFF-03                VALUE 'N'.
           05  WS-CJ-SW-04                 PIC X(01) VALUE 'N'.
               88  WS-CJ-ON-04                 VALUE 'Y'.
               88  WS-CJ-OFF-04                VALUE 'N'.
           05  WS-CJ-SW-05                 PIC X(01) VALUE 'N'.
               88  WS-CJ-ON-05                 VALUE 'Y'.
               88  WS-CJ-OFF-05                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-CJ-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CJ-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CJ-SUB-03                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CJ-SUB-04                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. LOADED ONCE AT INITIALISATION AND SEARCHED WITH
      * A SUBSCRIPTED WALK.
       01  WS-CJ-TABLE.
           05  WS-CJ-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-CJ-TB-ENTRY OCCURS 200 TIMES
                                       INDEXED BY WS-CJ-IX.
               10  WS-CJ-TB-KEY                PIC X(13).
               10  WS-CJ-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-CJ-TB-TXT                PIC X(30).
               10  WS-CJ-TB-EFF                PIC 9(05).
               10  WS-CJ-TB-EXP                PIC 9(05).
       01  WS-CJ-WORK-GROUP-1.
           05  WS-CJ-G1-SEGMENT            PIC 9(07).
           05  WS-CJ-G1-SEQ                PIC 9(05).
           05  WS-CJ-G1-STATUS             PIC X(10).
           05  WS-CJ-G1-CYCLE              PIC S9(09) COMP-3.
           05  WS-CJ-G1-CYCLE              PIC X(20).
           05  WS-CJ-G1-SEQ                PIC 9(05).
           05  WS-CJ-G1-TARIFF             PIC S9(11)V9(02) COMP-3.
           05  WS-CJ-G1-JURIS              PIC X(20).
       01  WS-CJ-WORK-GROUP-2.
           05  WS-CJ-G2-CLASS              PIC 9(05).
           05  WS-CJ-G2-CLASS              PIC S9(09) COMP-3.
           05  WS-CJ-G2-ELEM               PIC X(20).
           05  WS-CJ-G2-INVOICE            PIC X(10).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 58.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABURT19 - RATE OVERRIDE TABLE LOAD'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-CJ-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-CJ-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9980.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-CJ-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-CJ-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT BNDIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BNDIN OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               DISPLAY 'BNDIN FILE STATUS = ' WS-FS-INPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN INPUT MNTIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'MNTIN OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               DISPLAY 'MNTIN FILE STATUS = ' WS-FS-INPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RATOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RATOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               DISPLAY 'RATOUT FILE STATUS = ' WS-FS-OUTPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT SUSOUT.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'SUSOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               DISPLAY 'SUSOUT FILE STATUS = ' WS-FS-SUSPENSE
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CTLOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               DISPLAY 'CTLOUT FILE STATUS = ' WS-FS-CONTROL
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RPTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RPTOUT OPEN FAILED - FILE STATUS BAD' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-CJ-CYCLE-YYDDD.
           COMPUTE WS-CJ-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-CJ-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-CJ-CNT-08.
           MOVE 0 TO WS-CJ-CNT-05.
           MOVE 0 TO WS-CJ-CNT-03.
       P1200-EXIT.
           EXIT.
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT
      * PRECEDES THIS PROGRAM IN THE JOB.
       P1300-LOAD-TABLE.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
           PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT
               UNTIL WS-CJ-ON-01.
       P1300-EXIT.
           EXIT.
       P1310-STORE-TABLE-ROW.
           IF WS-CJ-TAB-CNT NOT < 200
               MOVE 'Y' TO WS-CJ-SW-01
               ADD 1 TO WS-CJ-CNT-01
           ELSE
               ADD 1 TO WS-CJ-TAB-CNT
               SET WS-CJ-IX TO WS-CJ-TAB-CNT
               MOVE IC-CLASS3 TO WS-CJ-TB-KEY (WS-CJ-IX)
               MOVE 0 TO WS-CJ-TB-VAL (WS-CJ-IX)
               MOVE SPACES TO WS-CJ-TB-TXT (WS-CJ-IX)
               ADD 1 TO WS-CJ-CNT-08.
           PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.
       P1310-EXIT.
           EXIT.
       P1320-READ-TABLE-ROW.
           READ BNDIN
               AT END MOVE 'Y' TO WS-CJ-SW-01.
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
           PERFORM P2200-SELECT-OVERRIDE THRU
               P2200-SELECT-OVERRIDE-EXIT.
           PERFORM P2300-CONVERT-DESCRIPTION THRU
               P2300-CONVERT-DESCRIPTION-EXIT.
           PERFORM P2400-EDIT-WINDOW THRU P2400-EDIT-WINDOW-EXIT.
           PERFORM P2500-BUILD-ROW THRU P2500-BUILD-ROW-EXIT.
           PERFORM P2600-CONVERT-DESCRIPTION THRU
               P2600-CONVERT-DESCRIPTION-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ BNDIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-SELECT-OVERRIDE.
           IF IC-CLASS = 'B'
               ADD 1 TO WS-CJ-CNT-02
           ELSE
               IF IC-CLASS = 'B'
                   ADD 1 TO WS-CJ-CNT-08
               ELSE
                   IF IC-CLASS = 'S'
                       ADD 1 TO WS-CJ-CNT-02
                   ELSE
                       ADD 1 TO WS-CJ-CNT-04.
           MOVE 'N' TO WS-CJ-SW-04.
           IF WS-CJ-TAB-CNT > 0
               PERFORM P260-COMPARE-BAND THRU P260-COMPARE-BAND-EXIT
               VARYING WS-CJ-SUB-02 FROM 1 BY 1
               UNTIL WS-CJ-SUB-02 > WS-CJ-TAB-CNT
               OR WS-CJ-SW-04 = 'Y'.
       P2200-SELECT-OVERRIDE-EXIT.
           EXIT.
       P2300-CONVERT-DESCRIPTION.
           MOVE 'Y' TO WS-CJ-SW-02.
           IF IC-SEGMENT < 9
               MOVE 'N' TO WS-CJ-SW-02
               ADD 1 TO WS-CJ-CNT-04.
           IF IC-SEGMENT > 8358
               MOVE 'N' TO WS-CJ-SW-02
               ADD 1 TO WS-CJ-CNT-08.
       P2300-CONVERT-DESCRIPTION-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P2400-EDIT-WINDOW.
           MOVE IC-INVOICE TO WS-CJ-TXT-01.
           MOVE IC-CARRIER TO WS-CJ-TXT-03.
           ADD 1 TO WS-CJ-CNT-06.
       P2400-EDIT-WINDOW-EXIT.
           EXIT.
       P2500-BUILD-ROW.
           CALL 'CABTBLLU' USING WS-CJ-TXT-02 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-CJ-CNT-05.
       P2500-BUILD-ROW-EXIT.
           EXIT.
       P2600-CONVERT-DESCRIPTION.
           IF WS-CJ-AMT-02 NOT = 0
               COMPUTE WS-CJ-QTY-03 = WS-CJ-AMT-02 * 100 / WS-CJ-AMT-02
           ELSE
               MOVE 0 TO WS-CJ-QTY-03.
       P2600-CONVERT-DESCRIPTION-EXIT.
           EXIT.
       P260-COMPARE-BAND.
           SET WS-CJ-IX TO WS-CJ-SUB-02.
           IF WS-CJ-TB-KEY (WS-CJ-IX) = IC-PERIOD
               MOVE 'Y' TO WS-CJ-SW-01
               MOVE WS-CJ-TB-VAL (WS-CJ-IX) TO WS-CJ-QTY-01
               MOVE WS-CJ-SUB-02 TO WS-CJ-SUB-04.
       P260-COMPARE-BAND-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P3100-POST-WINDOW.
           CALL 'CABHASH' USING IC-STATUS WS-ACC-OCN-HASH.
           ADD WS-CJ-CNT-05 TO WS-ACC-SEQ-HASH.
       P3100-POST-WINDOW-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P3200-STAGE-DESCRIPTION.
           MOVE SPACES TO WS-CJ-TXT-01.
           STRING IC-CARRIER DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               IC-CLASS2 DELIMITED BY SIZE
               INTO WS-CJ-TXT-01.
       P3200-STAGE-DESCRIPTION-EXIT.
           EXIT.
       P3300-EMIT-TARIFF.
           ADD IC-TYPE TO WS-CJ-QTY-03.
           COMPUTE WS-CJ-AMT-02 = WS-CJ-QTY-03 * WS-CJ-QTY-03.
           ADD WS-CJ-AMT-02 TO WS-CJ-AMT-03.
       P3300-EMIT-TARIFF-EXIT.
           EXIT.
       P3400-STAGE-TARIFF.
           MOVE SPACES TO CABS-CJ-OUT-RECORD.
           MOVE IC-STATE TO OC-INVOICE.
           MOVE IC-SEGMENT TO OC-TARGET.
           MOVE IC-INVOICE2 TO OC-CYCLE.
           MOVE IC-INVOICE TO OC-TARIFF.
           MOVE IC-CARRIER TO OC-CYCLE2.
           MOVE IC-STATUS TO OC-BAND.
           MOVE IC-INVOICE2 TO OC-BAND2.
           MOVE IC-STATE TO OC-CYCLE3.
           MOVE IC-CLASS3 TO OC-CYCLE4.
           WRITE CABS-CJ-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P3400-STAGE-TARIFF-EXIT.
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
           MOVE 'INPUT RECORDS' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO WS-CJ-CNT-EDIT.
           MOVE WS-CJ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'OUTPUT RECORDS' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-CJ-CNT-EDIT.
           MOVE WS-CJ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'SUSPENDED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-CJ-CNT-EDIT.
           MOVE WS-CJ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'SUMMARISED' TO PC-COL-001-020.
           MOVE WS-SUMM-CNT TO WS-CJ-CNT-EDIT.
           MOVE WS-CJ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'HELD FOR NEXT RUN' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-CJ-CNT-EDIT.
           MOVE WS-CJ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-CJ-CNT-01 TO WS-CJ-CNT-EDIT.
           MOVE WS-CJ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 02' TO PC-COL-001-020.
           MOVE WS-CJ-CNT-02 TO WS-CJ-CNT-EDIT.
           MOVE WS-CJ-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE 9 TO CT-STEP-SEQ.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE WS-CJ-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE SPACES TO CT-FILLER.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE 0 TO CT-RC.
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
           CLOSE BNDIN.
           CLOSE MNTIN.
           CLOSE RATOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABURT19 - STEP COMPLETE'.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  CJ-CNT-06 = ' WS-CJ-CNT-06.
           DISPLAY '  CJ-CNT-05 = ' WS-CJ-CNT-05.
       P9000-EXIT.
           EXIT.
