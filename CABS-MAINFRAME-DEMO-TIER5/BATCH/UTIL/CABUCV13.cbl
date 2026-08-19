      *****************************************************************
      * CABUCV13 - LEGACY LAYOUT DOWN CONVERSION                      *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               OLDIN   TELCABS.CABS.OLDIN          (LOCAL)     *
      *               IXCIN   TELCABS.CABS.IXCIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               DSPOUT  TELCABS.CABS.DSPOUT         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - NOTHING IS UPDATED IN PLACE        *
      * REVISION HISTORY                                              *
      *   V1.00  1990-10-23  G.PRZYBYLSKI INITIAL RELEASE             *
      *   V1.03  1995-01-01  C.ADEYEMI    EFFECTIVE DATE FILTER ADDED *
      *                      PER AUDIT FINDING                        *
      *   V1.04  1999-12-28  D.OKONKWO    CENTURY PIVOT APPLIED TO THE*
      *                      CYCLE DATE                               *
      *   V1.07  2012-09-18  C.ADEYEMI    ROUNDING RULE TAKEN FROM THE*
      *                      RATE ROW                                 *
      *   V1.11  2014-10-28  W.J.MCALLISTER RETIRED THE SECOND SORT   *
      *                      STEP - DONE IN PROGRAM                   *
      *   V1.14  2016-01-16  D.OKONKWO    PARM CARD EXTENDED,         *
      *                      POSITIONS 40 THROUGH 48                  *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUCV13.
       AUTHOR. TELCABS APPLICATIONS - FILE CONVERSION.
      *****************************************************************
      * LEGACY LAYOUT DOWN CONVERSION. THE STEP IS DRIVEN ENTIRELY    *
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
           SELECT OLDIN ASSIGN TO UT-S-OLDIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT IXCIN ASSIGN TO UT-S-IXCIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
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
      * OLDIN - PERMANENT DATASET HELD ON DASD.
       FD  OLDIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 110 CHARACTERS.
       01  CABS-DX-IN-RECORD.
           05  ID-CLASS                    PIC 9(05).
           05  ID-MEDIA                    PIC S9(11)V9(02) COMP-3.
           05  ID-BAN                      PIC S9(07)V9(02) COMP-3.
           05  ID-SEQ                      PIC 9(06).
           05  ID-REGION                   PIC X(04).
           05  ID-GROUP                    PIC X(06).
           05  ID-TYPE                     PIC X(08).
           05  ID-STATUS                   PIC X(04).
           05  ID-BAN2                     PIC S9(09)V9(05) COMP-3.
           05  ID-STATUS2                  PIC 9(06).
           05  ID-BAN3                     PIC S9(11) COMP-3.
           05  ID-CIRCUIT                  PIC S9(11) COMP-3.
           05  ID-TARGET                   PIC 9(06).
           05  ID-ACCOUNT                  PIC 9(05).
           05  ID-CODE                     PIC X(10).
           05  ID-CARRIER                  PIC X(04).
           05  ID-INVOICE                  PIC S9(07) COMP-3.
           05  DX-FILL-01                  PIC X(10).
      * IXCIN - CATALOGUED GENERATION DATA GROUP.
       FD  IXCIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 110 CHARACTERS.
       01  CABS-DX-ALT1-RECORD.
           05  A1-TYPE                     PIC S9(07) COMP-3.
           05  A1-TARIFF                   PIC X(08).
           05  A1-TYPE2                    PIC X(10).
           05  A1-SOURCE                   PIC S9(09)V9(02) COMP-3.
           05  A1-GROUP                    PIC 9(03).
           05  A1-TARGET                   PIC S9(13) COMP-3.
           05  DX-FILL-02                  PIC X(72).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-DX-VIEW1 REDEFINES CABS-DX-IN-RECORD.
           05  R0D-GROUP                   PIC S9(09) COMP-3.
           05  R0D-REGION                  PIC 9(04).
           05  R0D-BAN                     PIC X(02).
           05  R0D-SEGMENT                 PIC S9(11)V9(05) COMP-3.
           05  R0D-REST                    PIC X(90).
      * THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER
      * LAYOUT. BOTH ARE STILL ARRIVING.
       01  CABS-DX-VIEW2 REDEFINES CABS-DX-IN-RECORD.
           05  R1D-OCN                     PIC S9(07)V9(05) COMP-3.
           05  R1D-BAND                    PIC X(16).
           05  R1D-REGION                  PIC X(13).
           05  R1D-CODE                    PIC X(02).
           05  R1D-CENTRE                  PIC S9(07)V9(05) COMP-3.
           05  R1D-REST                    PIC X(65).
      * DSPOUT - CATALOGUED GENERATION DATA GROUP.
       FD  DSPOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-DX-OUT-RECORD.
           05  OD-BAN                      PIC X(02).
           05  OD-GROUP                    PIC X(02).
           05  OD-CLASS                    PIC X(03).
           05  OD-TARGET                   PIC S9(07)V9(02) COMP-3.
           05  OD-OCN                      PIC S9(15) COMP-3.
           05  OD-SOURCE                   PIC X(06).
           05  OD-MEDIA                    PIC S9(11) COMP-3.
           05  OD-ACCOUNT                  PIC 9(04).
           05  OD-CODE                     PIC S9(07) COMP-3.
           05  OD-OCN2                     PIC X(08).
           05  DX-FILL-03                  PIC X(32).
      * SUSOUT - SEQUENTIAL, BLOCKED BY THE SYSTEM.
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
      * SHARED LAYOUT PULLED IN FOR THE CENTURY SIDE.
       COPY CABSBILL.
      * SHARED LAYOUT PULLED IN FOR THE SIGN SIDE.
       COPY CABSCOMM.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUCV13'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V3.30'.
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
           05  WS-DX-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DX-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DX-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DX-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-DX-CNT-05                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-DX-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DX-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DX-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DX-QTY-04                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DX-QTY-05                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-DX-QTY-06                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-DX-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DX-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DX-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-DX-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-DX-TXT-01                PIC X(10) VALUE SPACES.
           05  WS-DX-TXT-02                PIC X(10) VALUE SPACES.
           05  WS-DX-TXT-03                PIC X(12) VALUE SPACES.
           05  WS-DX-TXT-04                PIC X(30) VALUE SPACES.
           05  WS-DX-TXT-05                PIC X(20) VALUE SPACES.
           05  WS-DX-TXT-06                PIC X(20) VALUE SPACES.
           05  WS-DX-TXT-07                PIC X(16) VALUE SPACES.
           05  WS-DX-TXT-08                PIC X(12) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-DX-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-DX-ON-01                 VALUE 'Y'.
               88  WS-DX-OFF-01                VALUE 'N'.
           05  WS-DX-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-DX-ON-02                 VALUE 'Y'.
               88  WS-DX-OFF-02                VALUE 'N'.
           05  WS-DX-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-DX-ON-03                 VALUE 'Y'.
               88  WS-DX-OFF-03                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-DX-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DX-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-DX-SUB-03                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. LOADED ONCE AT INITIALISATION AND SEARCHED WITH
      * A SUBSCRIPTED WALK.
       01  WS-DX-TABLE.
           05  WS-DX-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-DX-TB-ENTRY OCCURS 300 TIMES
                                       INDEXED BY WS-DX-IX.
               10  WS-DX-TB-KEY                PIC X(10).
               10  WS-DX-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-DX-TB-TXT                PIC X(40).
               10  WS-DX-TB-EFF                PIC 9(05).
               10  WS-DX-TB-EXP                PIC 9(05).
       01  WS-DX-WORK-GROUP-1.
           05  WS-DX-G1-CYCLE              PIC X(20).
           05  WS-DX-G1-CIRCUIT            PIC X(10).
           05  WS-DX-G1-CODE               PIC S9(09) COMP-3.
       01  WS-DX-WORK-GROUP-2.
           05  WS-DX-G2-CIRCUIT            PIC S9(09) COMP-3.
           05  WS-DX-G2-BAND               PIC X(10).
           05  WS-DX-G2-STATE              PIC S9(09) COMP-3.
           05  WS-DX-G2-STATUS             PIC X(20).
           05  WS-DX-G2-CENTRE             PIC X(10).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 50.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUCV13 - LEGACY LAYOUT DOWN CONVERSION'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-DX-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-DX-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
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
           05  WS-DX-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-DX-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT OLDIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OLDIN COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN INPUT IXCIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'IXCIN COULD NOT BE OPENED - STEP CANNOT RUN' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT DSPOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'DSPOUT COULD NOT BE OPENED - STEP CANNOT RUN' TO
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
           MOVE PC1-CYCLE-YYDDD TO WS-DX-CYCLE-YYDDD.
           COMPUTE WS-DX-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-DX-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-DX-CNT-05.
           MOVE 0 TO WS-DX-CNT-03.
           MOVE 0 TO WS-DX-CNT-01.
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
           PERFORM P2200-BUILD-CENTURY THRU P2200-BUILD-CENTURY-EXIT.
           PERFORM P2300-RESOLVE-CENTURY THRU
               P2300-RESOLVE-CENTURY-EXIT.
           PERFORM P2400-SPLIT-CENTURY THRU P2400-SPLIT-CENTURY-EXIT.
           IF WS-DX-ON-01
               PERFORM P2500-APPLY-CENTURY THRU
                   P2500-APPLY-CENTURY-EXIT.
           PERFORM P2600-EDIT-FIELD THRU P2600-EDIT-FIELD-EXIT.
           IF WS-DX-ON-03
               PERFORM P2700-EDIT-CENTURY THRU P2700-EDIT-CENTURY-EXIT.
           IF WS-DX-ON-01
               PERFORM P2800-SELECT-CENTURY THRU
                   P2800-SELECT-CENTURY-EXIT.
           PERFORM P2900-SPLIT-CENTURY THRU P2900-SPLIT-CENTURY-EXIT.
           PERFORM P21000-SELECT-CENTURY THRU
               P21000-SELECT-CENTURY-EXIT.
           PERFORM P21100-SELECT-PACKED THRU P21100-SELECT-PACKED-EXIT.
           PERFORM P21200-MATCH-LAYOUT THRU P21200-MATCH-LAYOUT-EXIT.
           PERFORM P21300-SELECT-RECORD THRU P21300-SELECT-RECORD-EXIT.
           PERFORM P21400-CHECK-ZONE THRU P21400-CHECK-ZONE-EXIT.
           IF WS-DX-ON-02
               PERFORM P21500-EDIT-CENTURY THRU
                   P21500-EDIT-CENTURY-EXIT.
           PERFORM P21600-EXPAND-PACKED THRU P21600-EXPAND-PACKED-EXIT.
           PERFORM P21700-DERIVE-FIELD THRU P21700-DERIVE-FIELD-EXIT.
           PERFORM P21800-RESOLVE-RECORD THRU
               P21800-RESOLVE-RECORD-EXIT.
           PERFORM P21900-DERIVE-LAYOUT THRU P21900-DERIVE-LAYOUT-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ OLDIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-BUILD-CENTURY.
           MOVE WS-DX-AMT-01 TO WS-DX-AMT-01.
           IF WS-DX-AMT-01 < 0
               COMPUTE WS-DX-AMT-01 = 0 - WS-DX-AMT-01.
       P2200-BUILD-CENTURY-EXIT.
           EXIT.
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS
      * BUILT ON THE SAME ORDER.
       P2300-RESOLVE-CENTURY.
           CALL 'CABHASH' USING ID-CODE WS-ACC-OCN-HASH.
           ADD WS-DX-CNT-05 TO WS-ACC-SEQ-HASH.
       P2300-RESOLVE-CENTURY-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P2400-SPLIT-CENTURY.
           MOVE SPACES TO WS-DX-TXT-04.
           STRING ID-TYPE DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               ID-TYPE DELIMITED BY SIZE
               INTO WS-DX-TXT-04.
       P2400-SPLIT-CENTURY-EXIT.
           EXIT.
       P2500-APPLY-CENTURY.
           IF ID-CLASS = 'D'
               ADD 1 TO WS-DX-CNT-03
           ELSE
               IF ID-CLASS = 'E'
                   ADD 1 TO WS-DX-CNT-05
               ELSE
                   IF ID-CLASS = 'S'
                       ADD 1 TO WS-DX-CNT-03
                   ELSE
                       ADD 1 TO WS-DX-CNT-02.
       P2500-APPLY-CENTURY-EXIT.
           EXIT.
       P2600-EDIT-FIELD.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-OCN-UNKNOWN TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-DX-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
       P2600-EDIT-FIELD-EXIT.
           EXIT.
       P2700-EDIT-CENTURY.
           IF WS-DX-AMT-02 < 42
               MOVE 42 TO WS-DX-AMT-02
               ADD 1 TO WS-DX-CNT-04.
           IF WS-DX-AMT-02 > 20098
               MOVE 20098 TO WS-DX-AMT-02
               ADD 1 TO WS-DX-CNT-05.
       P2700-EDIT-CENTURY-EXIT.
           EXIT.
      * THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT
      * PRECEDES THIS PROGRAM IN THE JOB.
       P2800-SELECT-CENTURY.
           MOVE SPACES TO CABS-DX-OUT-RECORD.
           MOVE ID-ACCOUNT TO OD-BAN.
           MOVE ID-CODE TO OD-GROUP.
           MOVE ID-MEDIA TO OD-CLASS.
           MOVE ID-CARRIER TO OD-TARGET.
           MOVE ID-BAN3 TO OD-OCN.
           MOVE ID-INVOICE TO OD-SOURCE.
           WRITE CABS-DX-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P2800-SELECT-CENTURY-EXIT.
           EXIT.
      * THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS. AN EXPIRY OF
      * ZERO IS OPEN ENDED.
       P2900-SPLIT-CENTURY.
           UNSTRING WS-DX-TXT-05 DELIMITED BY '/'
               INTO WS-DX-TXT-02
               WS-DX-TXT-01
               TALLYING IN WS-DX-CNT-02.
       P2900-SPLIT-CENTURY-EXIT.
           EXIT.
       P21000-SELECT-CENTURY.
           CALL 'CABHASH' USING WS-DX-TXT-01 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-DX-CNT-05.
       P21000-SELECT-CENTURY-EXIT.
           EXIT.
      * THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE
      * RESET INSIDE THE LOOP.
       P21100-SELECT-PACKED.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-DX-TXT-01 TO PC-COL-001-020.
           MOVE WS-DX-TXT-08 TO PC-COL-021-060.
           MOVE WS-DX-AMT-01 TO WS-DX-AMT-EDIT.
           MOVE WS-DX-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P21100-SELECT-PACKED-EXIT.
           EXIT.
      * A FAILURE HERE SUSPENDS THE RECORD, IT DOES NOT STOP THE STEP.
       P21200-MATCH-LAYOUT.
           MOVE 0 TO WS-DX-QTY-01.
           MOVE 0 TO WS-DX-QTY-06.
           MOVE 0 TO WS-DX-AMT-02.
       P21200-MATCH-LAYOUT-EXIT.
           EXIT.
       P21300-SELECT-RECORD.
           MOVE 'N' TO WS-DX-SW-02.
           IF WS-DX-TXT-05 NOT = WS-DX-TXT-08
               MOVE 'Y' TO WS-DX-SW-02
               MOVE WS-DX-TXT-05 TO WS-DX-TXT-08
               ADD 1 TO WS-DX-CNT-01.
       P21300-SELECT-RECORD-EXIT.
           EXIT.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P21400-CHECK-ZONE.
           MOVE 0 TO WS-DX-CNT-04.
           INSPECT WS-DX-TXT-01 TALLYING WS-DX-CNT-04
               FOR ALL SPACES.
           INSPECT WS-DX-TXT-01 REPLACING ALL LOW-VALUES BY SPACES.
       P21400-CHECK-ZONE-EXIT.
           EXIT.
       P21500-EDIT-CENTURY.
           MOVE ID-TARGET TO WS-DX-TXT-03.
           MOVE ID-TARGET TO WS-DX-TXT-01.
           MOVE ID-BAN2 TO WS-DX-TXT-05.
           ADD 1 TO WS-DX-CNT-03.
       P21500-EDIT-CENTURY-EXIT.
           EXIT.
      * FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES
      * RATHER THAN LOW VALUES.
       P21600-EXPAND-PACKED.
           ADD ID-INVOICE TO WS-DX-QTY-01.
           COMPUTE WS-DX-AMT-01 = WS-DX-QTY-01 * WS-DX-QTY-05.
           ADD WS-DX-AMT-01 TO WS-DX-AMT-02.
       P21600-EXPAND-PACKED-EXIT.
           EXIT.
       P21700-DERIVE-FIELD.
           IF WS-DX-AMT-04 NOT = 0
               COMPUTE WS-DX-QTY-01 = WS-DX-AMT-01 * 100 / WS-DX-AMT-04
           ELSE
               MOVE 0 TO WS-DX-QTY-01.
       P21700-DERIVE-FIELD-EXIT.
           EXIT.
       P21800-RESOLVE-RECORD.
           MOVE 'Y' TO WS-DX-SW-01.
           IF ID-ACCOUNT < 25
               MOVE 'N' TO WS-DX-SW-01
               ADD 1 TO WS-DX-CNT-03.
           IF ID-ACCOUNT > 8884
               MOVE 'N' TO WS-DX-SW-01
               ADD 1 TO WS-DX-CNT-04.
       P21800-RESOLVE-RECORD-EXIT.
           EXIT.
       P21900-DERIVE-LAYOUT.
           MOVE WS-DX-AMT-01 TO WS-DX-AMT-04.
           IF WS-DX-AMT-04 < 0
               COMPUTE WS-DX-AMT-04 = 0 - WS-DX-AMT-01.
       P21900-DERIVE-LAYOUT-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
       P3100-CLOSE-OFF-LAYOUT.
           MOVE SPACES TO CABS-DX-OUT-RECORD.
           MOVE ID-ACCOUNT TO OD-BAN.
           MOVE ID-TYPE TO OD-GROUP.
           MOVE ID-BAN TO OD-CLASS.
           MOVE ID-CARRIER TO OD-TARGET.
           MOVE ID-ACCOUNT TO OD-OCN.
           MOVE ID-CLASS TO OD-SOURCE.
           WRITE CABS-DX-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P3100-CLOSE-OFF-LAYOUT-EXIT.
           EXIT.
      * THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND
      * ARE NOT PART OF THE BALANCE.
       P3200-STAGE-CENTURY.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-RATE-NOT-FOUND TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-DX-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
           MOVE CABS-SUSPENSE-RECORD TO CABS-SUSOUT-RECORD.
           WRITE CABS-SUSOUT-RECORD.
       P3200-STAGE-CENTURY-EXIT.
           EXIT.
       P3300-STAGE-PACKED.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-DX-TXT-08 TO PC-COL-001-020.
           MOVE WS-DX-TXT-04 TO PC-COL-021-060.
           MOVE WS-DX-AMT-01 TO WS-DX-AMT-EDIT.
           MOVE WS-DX-AMT-EDIT TO PC-COL-091-132.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-NBR.
       P3300-STAGE-PACKED-EXIT.
           EXIT.
      * THE PARM CARD IS POSITIONAL. THERE IS NO KEYWORD PARSER AND
      * THERE NEVER HAS BEEN.
       P3400-RELEASE-LAYOUT.
           MOVE 0 TO WS-DX-QTY-02.
           MOVE 0 TO WS-DX-QTY-01.
           MOVE 0 TO WS-DX-QTY-06.
           MOVE 0 TO WS-DX-AMT-04.
           MOVE 0 TO WS-DX-AMT-02.
       P3400-RELEASE-LAYOUT-EXIT.
           EXIT.
      * S400-SECONDARY-PROCESSING SECTION
       S400-SECONDARY-PROCESSING SECTION.
       P4000-SECONDARY.
           PERFORM P4100-NORMALISE-TYPE THRU P4100-NORMALISE-TYPE-EXIT.
           PERFORM P4200-NORMALISE-CLASS THRU
               P4200-NORMALISE-CLASS-EXIT.
       P4000-EXIT.
           EXIT.
       P4100-NORMALISE-TYPE.
           MOVE SPACES TO WS-DX-TXT-06.
           STRING ID-CIRCUIT DELIMITED BY SIZE
               '-' DELIMITED BY SIZE
               ID-BAN3 DELIMITED BY SIZE
               INTO WS-DX-TXT-06.
       P4100-NORMALISE-TYPE-EXIT.
           EXIT.
      * THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE
      * MORE AT END OF FILE.
       P4200-NORMALISE-CLASS.
           MOVE 'Y' TO WS-DX-SW-02.
           IF ID-MEDIA < 27
               MOVE 'N' TO WS-DX-SW-02
               ADD 1 TO WS-DX-CNT-04.
           IF ID-MEDIA > 7109
               MOVE 'N' TO WS-DX-SW-02
               ADD 1 TO WS-DX-CNT-02.
       P4200-NORMALISE-CLASS-EXIT.
           EXIT.
           MOVE 0 TO WS-DX-QTY-01.
           PERFORM P350-WALK-SIGN THRU P350-WALK-SIGN-EXIT
               VARYING WS-DX-SUB-03 FROM 1 BY 1
               UNTIL WS-DX-SUB-03 > WS-DX-TAB-CNT.
       P350-WALK-SIGN.
           SET WS-DX-IX TO WS-DX-SUB-03.
           IF WS-DX-TB-KEY (WS-DX-IX) NOT = SPACES
               ADD WS-DX-TB-VAL (WS-DX-IX) TO WS-DX-QTY-01.
       P350-WALK-SIGN-EXIT.
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
           MOVE 'RECORDS READ' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO WS-DX-CNT-EDIT.
           MOVE WS-DX-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS WRITTEN' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-DX-CNT-EDIT.
           MOVE WS-DX-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS REJECTED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-DX-CNT-EDIT.
           MOVE WS-DX-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS CARRIED FWD' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-DX-CNT-EDIT.
           MOVE WS-DX-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS SUMMARISED' TO PC-COL-001-020.
           MOVE WS-SUMM-CNT TO WS-DX-CNT-EDIT.
           MOVE WS-DX-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-DX-CNT-01 TO WS-DX-CNT-EDIT.
           MOVE WS-DX-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 02' TO PC-COL-001-020.
           MOVE WS-DX-CNT-02 TO WS-DX-CNT-EDIT.
           MOVE WS-DX-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 03' TO PC-COL-001-020.
           MOVE WS-DX-CNT-03 TO WS-DX-CNT-EDIT.
           MOVE WS-DX-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 04' TO PC-COL-001-020.
           MOVE WS-DX-CNT-04 TO WS-DX-CNT-EDIT.
           MOVE WS-DX-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE WS-DX-CNT-03 TO CT-RC.
           MOVE SPACES TO CT-FILLER.
           MOVE WS-DX-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE 6 TO CT-STEP-SEQ.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
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
           CLOSE OLDIN.
           CLOSE IXCIN.
           CLOSE DSPOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABUCV13 - STEP COMPLETE'.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  DX-CNT-05 = ' WS-DX-CNT-05.
           DISPLAY '  DX-CNT-02 = ' WS-DX-CNT-02.
           DISPLAY '  DX-CNT-01 = ' WS-DX-CNT-01.
           DISPLAY '  DX-CNT-04 = ' WS-DX-CNT-04.
       P9000-EXIT.
           EXIT.
