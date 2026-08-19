      *****************************************************************
      * CABUXR18 - CONTROL RECORD CROSS REFERENCE                     *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               RGTIN   TELCABS.CABS.RGTIN          (LOCAL)     *
      *               MSTIN   TELCABS.CABS.MSTIN          (LOCAL)     *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               PAIROUT TELCABS.CABS.PAIROU         (LOCAL)     *
      *               LNKOUT  TELCABS.CABS.LNKOUT         (LOCAL)     *
      *               RPTOUT  SYSOUT CLASS A               CABSPRNT   *
      *               SUSOUT  TELCABS.CABS.UTIL.SUSP    CABSERR       *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : FULL RERUN - NOTHING IS UPDATED IN PLACE        *
      * REVISION HISTORY                                              *
      *   V1.00  1988-02-17  A.BUKOWSKI   INITIAL RELEASE             *
      *   V1.02  1997-07-08  L.FERREIRA   JOB PARAMETER MADE MANDATORY*
      *   V1.03  2004-12-19  L.FERREIRA   PARM CARD EXTENDED,         *
      *                      POSITIONS 40 THROUGH 48                  *
      *   V1.04  2017-11-04  A.BUKOWSKI   EFFECTIVE DATE FILTER ADDED *
      *                      PER AUDIT FINDING                        *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABUXR18.
       AUTHOR. TELCABS APPLICATIONS - CROSS REFERENCE REPORT.
      *****************************************************************
      * CONTROL RECORD CROSS REFERENCE. THIS STEP IS SCHEDULED INSIDE *
      * THE NIGHTLY ACCESS BILLING STREAM AND HAS NO INTERACTIVE ENTRY*
      * POINT.                                                        *
      * ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS*
      * BUILT ON THE SAME ORDER.                                      *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           C01 IS TO-NEW-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RGTIN ASSIGN TO UT-S-RGTIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT MSTIN ASSIGN TO UT-S-MSTIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT PAIROUT ASSIGN TO UT-S-PAIROUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT LNKOUT ASSIGN TO UT-S-LNKOUT
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
      * RGTIN - ALLOCATED BY THE JOB, NEVER BY THE PROGRAM.
       FD  RGTIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 130 CHARACTERS.
       01  CABS-AP-IN-RECORD.
           05  IA-TYPE                     PIC S9(09)V9(02) COMP-3.
           05  IA-JURIS                    PIC 9(09).
           05  IA-TARGET                   PIC S9(11)V9(02) COMP-3.
           05  IA-GROUP                    PIC S9(05) COMP-3.
           05  IA-SEQ                      PIC 9(07).
           05  IA-CIRCUIT                  PIC 9(09).
           05  IA-ACCOUNT                  PIC 9(02).
           05  IA-CYCLE                    PIC X(04).
           05  IA-SEGMENT                  PIC 9(02).
           05  IA-STATUS                   PIC S9(13) COMP-3.
           05  IA-TARIFF                   PIC X(10).
           05  IA-BAND                     PIC 9(02).
           05  IA-INVOICE                  PIC X(13).
           05  IA-CENTRE                   PIC X(20).
           05  IA-STATUS2                  PIC S9(13)V9(05) COMP-3.
           05  IA-BAND2                    PIC S9(05) COMP-3.
           05  IA-CENTRE2                  PIC S9(07)V9(02) COMP-3.
           05  IA-OCN                      PIC X(02).
           05  AP-FILL-01                  PIC X(9).
      * MSTIN - PERMANENT DATASET HELD ON DASD.
       FD  MSTIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 130 CHARACTERS.
       01  CABS-AP-ALT1-RECORD.
           05  A1-ACCOUNT                  PIC 9(06).
           05  A1-CYCLE                    PIC S9(13) COMP-3.
           05  A1-TYPE                     PIC S9(05) COMP-3.
           05  A1-TARIFF                   PIC S9(07)V9(05) COMP-3.
           05  A1-TYPE2                    PIC X(16).
           05  A1-REGION                   PIC S9(13)V9(02) COMP-3.
           05  A1-CLASS                    PIC 9(06).
           05  AP-FILL-02                  PIC X(77).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-AP-VIEW1 REDEFINES CABS-AP-IN-RECORD.
           05  R0A-GROUP                   PIC X(04).
           05  R0A-BAN                     PIC X(08).
           05  R0A-SEQ                     PIC 9(03).
           05  R0A-OCN                     PIC S9(07) COMP-3.
           05  R0A-REST                    PIC X(111).
      * ALTERNATE VIEW OF THE SAME BYTES. THE RECORD TYPE MUST BE
      * TESTED BEFORE ANY FIELD BELOW IS REFERENCED.
       01  CABS-AP-VIEW2 REDEFINES CABS-AP-IN-RECORD.
           05  R1A-CENTRE                  PIC X(03).
           05  R1A-PERIOD                  PIC S9(11)V9(02) COMP-3.
           05  R1A-TARIFF                  PIC 9(07).
           05  R1A-SEQ                     PIC S9(15) COMP-3.
           05  R1A-BAND                    PIC S9(05) COMP-3.
           05  R1A-OCN                     PIC X(06).
           05  R1A-SEQ2                    PIC S9(07)V9(02) COMP-3.
           05  R1A-REST                    PIC X(91).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-AP-VIEW3 REDEFINES CABS-AP-IN-RECORD.
           05  R2A-REGION                  PIC X(02).
           05  R2A-LEVEL                   PIC 9(05).
           05  R2A-PERIOD                  PIC X(04).
           05  R2A-CENTRE                  PIC X(10).
           05  R2A-STATUS                  PIC S9(15) COMP-3.
           05  R2A-MEDIA                   PIC 9(09).
           05  R2A-ACCOUNT                 PIC S9(05) COMP-3.
           05  R2A-REST                    PIC X(89).
      * THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD
      * FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.
       01  CABS-AP-VIEW4 REDEFINES CABS-AP-IN-RECORD.
           05  R3A-BAN                     PIC X(03).
           05  R3A-ELEM                    PIC X(08).
           05  R3A-INVOICE                 PIC 9(07).
           05  R3A-CYCLE                   PIC X(20).
           05  R3A-BAND                    PIC 9(06).
           05  R3A-REST                    PIC X(86).
      * PAIROUT - SEQUENTIAL, BLOCKED BY THE SYSTEM.
       FD  PAIROUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 110 CHARACTERS.
       01  CABS-AP-OUT-RECORD.
           05  OA-OCN                      PIC S9(09)V9(02) COMP-3.
           05  OA-STATE                    PIC S9(11) COMP-3.
           05  OA-SEQ                      PIC 9(03).
           05  OA-CLASS                    PIC X(16).
           05  OA-CODE                     PIC 9(03).
           05  OA-TARGET                   PIC X(16).
           05  OA-JURIS                    PIC S9(09)V9(02) COMP-3.
           05  OA-ACCOUNT                  PIC S9(05) COMP-3.
           05  OA-PERIOD                   PIC X(08).
           05  OA-CARRIER                  PIC X(03).
           05  OA-TARIFF                   PIC S9(09) COMP-3.
           05  OA-LEVEL                    PIC S9(15) COMP-3.
           05  OA-SEGMENT                  PIC 9(02).
           05  OA-CENTRE                   PIC X(03).
           05  OA-CIRCUIT                  PIC 9(07).
           05  OA-MEDIA                    PIC X(04).
           05  OA-ELEM                     PIC 9(07).
           05  AP-FILL-03                  PIC X(4).
      * LNKOUT - PERMANENT DATASET HELD ON DASD.
       FD  LNKOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 110 CHARACTERS.
       01  CABS-AP-OUT1-RECORD         PIC X(110).
      * SUSOUT - CATALOGUED GENERATION DATA GROUP.
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
      * SHARED LAYOUT PULLED IN FOR THE ORPHAN SIDE.
       COPY CABSCOMM.
      * SHARED LAYOUT PULLED IN FOR THE LINK SIDE.
       COPY CABSCARR.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABUXR18'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V2.02'.
           05  WS-TABLE-LIMIT              PIC S9(04) COMP-3
                                         VALUE 250.
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
           05  WS-AP-CNT-01                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AP-CNT-02                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AP-CNT-03                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AP-CNT-04                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AP-CNT-05                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AP-CNT-06                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AP-CNT-07                PIC S9(09) COMP-3 VALUE 0.
           05  WS-AP-CNT-08                PIC S9(09) COMP-3 VALUE 0.
       01  WS-QUANTITY-AREA.
           05  WS-AP-QTY-01                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AP-QTY-02                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AP-QTY-03                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AP-QTY-04                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AP-QTY-05                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-AP-QTY-06                PIC S9(11)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-AMOUNT-AREA.
           05  WS-AP-AMT-01                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AP-AMT-02                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AP-AMT-03                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AP-AMT-04                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AP-AMT-05                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
           05  WS-AP-AMT-06                PIC S9(13)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-TEXT-AREA.
           05  WS-AP-TXT-01                PIC X(12) VALUE SPACES.
           05  WS-AP-TXT-02                PIC X(30) VALUE SPACES.
           05  WS-AP-TXT-03                PIC X(08) VALUE SPACES.
           05  WS-AP-TXT-04                PIC X(16) VALUE SPACES.
           05  WS-AP-TXT-05                PIC X(12) VALUE SPACES.
           05  WS-AP-TXT-06                PIC X(26) VALUE SPACES.
       01  WS-SWITCH-AREA.
           05  WS-AP-SW-01                 PIC X(01) VALUE 'N'.
               88  WS-AP-ON-01                 VALUE 'Y'.
               88  WS-AP-OFF-01                VALUE 'N'.
           05  WS-AP-SW-02                 PIC X(01) VALUE 'N'.
               88  WS-AP-ON-02                 VALUE 'Y'.
               88  WS-AP-OFF-02                VALUE 'N'.
           05  WS-AP-SW-03                 PIC X(01) VALUE 'N'.
               88  WS-AP-ON-03                 VALUE 'Y'.
               88  WS-AP-OFF-03                VALUE 'N'.
       01  WS-SUBSCRIPT-AREA.
           05  WS-AP-SUB-01                PIC S9(04) COMP-3 VALUE 0.
           05  WS-AP-SUB-02                PIC S9(04) COMP-3 VALUE 0.
           05  WS-AP-SUB-03                PIC S9(04) COMP-3 VALUE 0.
      * IN CORE TABLE. A ROW THAT DOES NOT FIT IS COUNTED AND DROPPED
      * - THE STEP DOES NOT FAIL.
       01  WS-AP-TABLE.
           05  WS-AP-TAB-CNT               PIC S9(04) COMP-3 VALUE 0.
           05  WS-AP-TB-ENTRY OCCURS 250 TIMES
                                       INDEXED BY WS-AP-IX.
               10  WS-AP-TB-KEY                PIC X(08).
               10  WS-AP-TB-VAL                PIC S9(09)V9(05) COMP-3.
               10  WS-AP-TB-TXT                PIC X(30).
               10  WS-AP-TB-EFF                PIC 9(05).
               10  WS-AP-TB-EXP                PIC 9(05).
       01  WS-AP-WORK-GROUP-1.
           05  WS-AP-G1-PERIOD             PIC X(10).
           05  WS-AP-G1-PERIOD             PIC X(10).
           05  WS-AP-G1-TARGET             PIC S9(11)V9(02) COMP-3.
           05  WS-AP-G1-SEGMENT            PIC X(10).
           05  WS-AP-G1-GROUP              PIC S9(11)V9(02) COMP-3.
           05  WS-AP-G1-LEVEL              PIC S9(09) COMP-3.
       01  WS-AP-WORK-GROUP-2.
           05  WS-AP-G2-REGION             PIC X(20).
           05  WS-AP-G2-REGION             PIC X(20).
           05  WS-AP-G2-MEDIA              PIC S9(09) COMP-3.
           05  WS-AP-G2-INVOICE            PIC 9(05).
           05  WS-AP-G2-SEQ                PIC X(20).
       01  WS-REPORT-WORK.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-LINE-NBR             PIC S9(03) COMP-3
                                               VALUE 0.
           05  WS-RPT-MAX-LINES            PIC S9(03) COMP-3
                                               VALUE 55.
           05  WS-RPT-TITLE1               PIC X(60) VALUE
                   'CABUXR18 - CONTROL RECORD CROSS REFERENCE'.
           05  WS-RPT-TITLE2               PIC X(60) VALUE
                   'TELCABS WHOLESALE ACCESS BILLING'.
           05  WS-AP-AMT-EDIT              PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-AP-CNT-EDIT              PIC ZZZ,ZZZ,ZZ9.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9945.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-EXT                   PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
           05  WS-RC-ABEND                 PIC 9(04) VALUE 0.
       01  WS-CYCLE-VIEW.
           05  WS-AP-CYCLE-YYDDD           PIC 9(05) VALUE 0.
           05  WS-AP-CYCLE-CCYYDDD         PIC 9(07) VALUE 0.
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
           OPEN INPUT RGTIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RGTIN OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               DISPLAY 'RGTIN FILE STATUS = ' WS-FS-INPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN INPUT MSTIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'MSTIN OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               DISPLAY 'MSTIN FILE STATUS = ' WS-FS-INPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT PAIROUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'PAIROUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               DISPLAY 'PAIROUT FILE STATUS = ' WS-FS-OUTPUT
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT LNKOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'LNKOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               DISPLAY 'LNKOUT FILE STATUS = ' WS-FS-OUTPUT
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
           MOVE PC1-CYCLE-YYDDD TO WS-AP-CYCLE-YYDDD.
           COMPUTE WS-AP-CYCLE-CCYYDDD = 19000000 + PC1-CYCLE-YYDDD.
           MOVE PC1-CYCLE-YYDDD TO DW-CUR-YY DW-CUR-DDD.
           COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.
           IF DW-CUR-YY < DW-PIVOT-YY
               COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.
           CALL 'CABDTCNV' USING WS-AP-CYCLE-CCYYDDD DW-GREG-DATE
               WS-RC-DTCNV.
           MOVE 0 TO WS-AP-CNT-02.
           MOVE 0 TO WS-AP-CNT-01.
           MOVE 0 TO WS-AP-CNT-06.
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
           IF WS-AP-ON-03
               PERFORM P2200-CONVERT-MATCH THRU
                   P2200-CONVERT-MATCH-EXIT.
           PERFORM P2300-CONVERT-LINK THRU P2300-CONVERT-LINK-EXIT.
           PERFORM P2100-READ-INPUT THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-INPUT.
           IF NOT WS-EOF
               READ RGTIN
                   AT END MOVE 'Y' TO WS-EOF-SW.
       P2100-EXIT.
           EXIT.
       P2200-CONVERT-MATCH.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-OCN-UNKNOWN TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-AP-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
       P2200-CONVERT-MATCH-EXIT.
           EXIT.
      * A FAILURE HERE SUSPENDS THE RECORD, IT DOES NOT STOP THE STEP.
       P2300-CONVERT-LINK.
           CALL 'CABCTLWR' USING WS-AP-TXT-05 WS-RC-EXT.
           IF WS-RC-EXT NOT = 0
               ADD 1 TO WS-AP-CNT-05.
       P2300-CONVERT-LINK-EXIT.
           EXIT.
      * S300-OUTPUT SECTION
       S300-OUTPUT SECTION.
      * THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE
      * INPUT IS NOT GUARANTEED TO BE ASCENDING.
       P3100-CLOSE-OFF-ORPHAN.
           ADD IA-TYPE TO WS-AP-QTY-02.
           COMPUTE WS-AP-AMT-03 = WS-AP-QTY-02 * WS-AP-QTY-02.
           ADD WS-AP-AMT-03 TO WS-AP-AMT-04.
       P3100-CLOSE-OFF-ORPHAN-EXIT.
           EXIT.
       P3200-RELEASE-SIDE.
           CALL 'CABHASH' USING IA-GROUP WS-ACC-OCN-HASH.
           ADD WS-AP-CNT-07 TO WS-ACC-SEQ-HASH.
       P3200-RELEASE-SIDE-EXIT.
           EXIT.
       P3300-CLOSE-OFF-ORPHAN.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-DATE-INVALID TO SU-ERR-CODE.
           MOVE 'E' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE PC1-RUN-ID TO SU-RUN-ID.
           MOVE CABS-AP-IN-RECORD TO SU-ORIG-RECORD.
           ADD 1 TO WS-REJECT-CNT.
           MOVE CABS-SUSPENSE-RECORD TO CABS-SUSOUT-RECORD.
           WRITE CABS-SUSOUT-RECORD.
       P3300-CLOSE-OFF-ORPHAN-EXIT.
           EXIT.
       P3400-RELEASE-GROUP.
           MOVE 0 TO WS-AP-QTY-02.
           MOVE 0 TO WS-AP-QTY-03.
           MOVE 0 TO WS-AP-QTY-01.
           MOVE 0 TO WS-AP-AMT-04.
           MOVE 0 TO WS-AP-AMT-05.
       P3400-RELEASE-GROUP-EXIT.
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
           MOVE 'HELD FOR NEXT RUN' TO PC-COL-001-020.
           MOVE WS-CFWD-CNT TO WS-AP-CNT-EDIT.
           MOVE WS-AP-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'SUSPENDED' TO PC-COL-001-020.
           MOVE WS-REJECT-CNT TO WS-AP-CNT-EDIT.
           MOVE WS-AP-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'INPUT RECORDS' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO WS-AP-CNT-EDIT.
           MOVE WS-AP-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'SUMMARISED' TO PC-COL-001-020.
           MOVE WS-SUMM-CNT TO WS-AP-CNT-EDIT.
           MOVE WS-AP-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'OUTPUT RECORDS' TO PC-COL-001-020.
           MOVE WS-WRITE-CNT TO WS-AP-CNT-EDIT.
           MOVE WS-AP-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 01' TO PC-COL-001-020.
           MOVE WS-AP-CNT-01 TO WS-AP-CNT-EDIT.
           MOVE WS-AP-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'LOCAL COUNTER 02' TO PC-COL-001-020.
           MOVE WS-AP-CNT-02 TO WS-AP-CNT-EDIT.
           MOVE WS-AP-CNT-EDIT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8010-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-FILLER.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE WS-AP-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE 4 TO CT-STEP-SEQ.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE PC1-STEPNAME TO CT-STEPNAME.
           MOVE PC1-RUN-ID TO CT-RUN-ID.
           MOVE PC1-JOBNAME TO CT-JOBNAME.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
       P8100-EXIT.
           EXIT.
      * P8200-CHECK-BALANCE - A RECORD THAT WAS HELD FOR THE NEXT
      * CYCLE IS BROUGHT INTO THE CARRIED FORWARD SIDE BEFORE THE
      * EQUATION IS TESTED.
       P8200-CHECK-BALANCE.
           ADD WS-AP-CNT-08 TO CT-CARRIED-FWD.
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
           CLOSE RGTIN.
           CLOSE MSTIN.
           CLOSE PAIROUT.
           CLOSE LNKOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABUXR18 - END OF RUN'.
           DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.
           DISPLAY '  WRITTEN   = ' WS-WRITE-CNT.
           DISPLAY '  REJECTED  = ' WS-REJECT-CNT.
           DISPLAY '  READ      = ' WS-READ-CNT.
           DISPLAY '  AP-CNT-01 = ' WS-AP-CNT-01.
           DISPLAY '  AP-CNT-02 = ' WS-AP-CNT-02.
           DISPLAY '  AP-CNT-07 = ' WS-AP-CNT-07.
           DISPLAY '  AP-CNT-04 = ' WS-AP-CNT-04.
       P9000-EXIT.
           EXIT.
