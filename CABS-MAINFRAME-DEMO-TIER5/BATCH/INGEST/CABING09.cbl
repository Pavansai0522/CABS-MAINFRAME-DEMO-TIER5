      *****************************************************************
      * CABING09 - DAILY CONSOLIDATION                                 *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               VOCIN   TELCABS.CABS.USAGE.VOICE(0)  CABSCDR    *
      *               DATIN   TELCABS.CABS.USAGE.DATA(0)   (LOCAL,VB) *
      *               SPCIN   TELCABS.CABS.USAGE.SPCL(0)   CABSCDR    *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               CONOUT  TELCABS.CABS.USAGE.CONSOL(+1) (LOCAL)   *
      *               RPTOUT  SYSOUT FBA 133                CABSPRNT  *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN (STRAIGHT 3-WAY MERGE,     *
      *               ONE CONOUT RECORD PER INPUT RECORD)             *
      * RESTART     : FULL RERUN                                     *
      * REVISION HISTORY                                              *
      *   V1.00  1988-09-12  R.T.WHEELER  INITIAL 2-WAY MERGE (V+D)   *
      *   V1.03  1991-04-25  D.OKONKWO    SPCIN ADDED - 3-WAY MERGE   *
      *   V1.07  1995-10-08  J.M.CASTILLO OCN SUMMARY TABLE ADDED     *
      *   V2.00  1999-02-17  P.NAIR       Y2K REVIEW - NO IMPACT      *
      *   V2.02  2002-07-30  A.BUKOWSKI   PRINTED VOLUME REPORT ADDED *
      *   V2.05  2008-12-04  L.FERREIRA   DATIN CONVERTED TO VB       *
      *   V2.07  2015-03-19  K.ADEYEMI    SUMMARY TABLE SIZE 300      *
      *   V2.09  2019-10-11  M.HOLLIS     RECOMPILE ONLY - LE V7      *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CABING09.
       AUTHOR.        RADIANT-CABS-TEAM.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-Z15.
       OBJECT-COMPUTER. IBM-Z15.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT VOCIN  ASSIGN TO VOCIN
               ORGANIZATION SEQUENTIAL FILE STATUS WS-FS-INPUT.
           SELECT DATIN  ASSIGN TO DATIN
               ORGANIZATION SEQUENTIAL FILE STATUS WS-FS-TABLE.
           SELECT SPCIN  ASSIGN TO SPCIN
               ORGANIZATION SEQUENTIAL FILE STATUS WS-FS-SUSPENSE.
           SELECT CONOUT ASSIGN TO CONOUT
               ORGANIZATION SEQUENTIAL FILE STATUS WS-FS-OUTPUT.
           SELECT RPTOUT ASSIGN TO RPTOUT
               ORGANIZATION SEQUENTIAL FILE STATUS WS-FS-RPTOUT.
           SELECT CTLOUT ASSIGN TO CTLOUT
               ORGANIZATION SEQUENTIAL FILE STATUS WS-FS-CONTROL.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  VOCIN
           RECORD CONTAINS 200 CHARACTERS RECORDING MODE F.
           COPY CABSCDR.
      *
      * CABS-STD-007 - VARIABLE-LENGTH RECORD.  DATIN IS THE ONLY
      * INGEST STREAM CARRYING AN OPTIONAL EXTENDED-ATTRIBUTE TAIL,
      * SO IT ALONE IS VB.  WS-DAT-REC-LEN IS SET BY THE RUNTIME ON
      * EVERY READ - CALLERS MUST NOT ASSUME THE FULL 200 IS PRESENT.
       FD  DATIN
           RECORD IS VARYING IN SIZE FROM 100 TO 200 CHARACTERS
               DEPENDING ON WS-DAT-REC-LEN
           RECORDING MODE V.
       01  DATIN-RECORD.
           05  DT-KEY.
               10  DT-OCN                  PIC X(04).
               10  DT-BAN                  PIC X(13).
               10  DT-SEQ-NBR               PIC 9(09) COMP-3.
           05  DT-RECORD-CTL.
               10  DT-REC-TYPE             PIC X(02).
               10  DT-USAGE-TYPE           PIC X(01).
               10  DT-JURIS-CD             PIC X(01).
               10  DT-RATE-ELEM            PIC X(06).
           05  DT-DATE-TIME.
               10  DT-CONN-YYDDD.
                   15  DT-CONN-YY          PIC 9(02).
                   15  DT-CONN-DDD         PIC 9(03).
               10  DT-CONN-HHMMSS          PIC 9(06).
               10  DT-DISC-YYDDD           PIC 9(05).
               10  DT-DISC-HHMMSS          PIC 9(06).
           05  DT-CIRCUIT-ID               PIC X(20).
           05  DT-OCTETS-IN                PIC S9(15)    COMP-3.
           05  DT-OCTETS-OUT               PIC S9(15)    COMP-3.
           05  DT-CORE-FILLER              PIC X(10).
           05  DT-EXTENDED-TAIL            PIC X(100).
      *
       FD  SPCIN
           RECORD CONTAINS 200 CHARACTERS RECORDING MODE F.
           COPY CABSCDR REPLACING ==CABS-CDR-RECORD== BY
               ==CABS-CDR-RECORD-SP== ==CD-== BY ==SD-==.
      *
       FD  CONOUT
           RECORD CONTAINS 200 CHARACTERS RECORDING MODE F.
       01  CONOUT-RECORD                   PIC X(200).
      *
       FD  RPTOUT
           RECORD CONTAINS 133 CHARACTERS RECORDING MODE F.
           COPY CABSPRNT.
      *
       FD  CTLOUT
           RECORD CONTAINS 180 CHARACTERS RECORDING MODE F.
       01  CTLOUT-RECORD                   PIC X(180).
      *
       WORKING-STORAGE SECTION.
      * STANDARD ESTATE WORKING STORAGE (COUNTERS/SWITCHES/DATE/CTL)
           COPY CABSWRK.
       01  WS-FS-RPTOUT-AREA.
           05  WS-FS-RPTOUT                PIC X(02) VALUE '00'.
      * VARIABLE-LENGTH RECORD LENGTH HOLDER FOR DATIN (CABS-STD-007)
       01  WS-DAT-LEN-AREA.
           05  WS-DAT-REC-LEN              PIC S9(03) COMP-3
                                                          VALUE 200.
      *---------------------------------------------------------------*
      * COMMON STAGING RECORD - WHICHEVER STREAM IS CURRENTLY SELECTED
      * BY THE MERGE IS COPIED HERE.  THE THREE REDEFINES LET P5100/
      * P5200/P5300 READ THE VARIANT AREA AS VOICE, DATA, OR SPECIAL
      * ACCESS DEPENDING ON WHICH PARAGRAPH IS EXECUTING.
      *---------------------------------------------------------------*
       01  WS-CURRENT-REC.
           05  CU-KEY.
               10  CU-OCN                  PIC X(04).
               10  CU-BAN                  PIC X(13).
               10  CU-SEQ-NBR               PIC 9(09) COMP-3.
           05  CU-RECORD-CTL.
               10  CU-REC-TYPE             PIC X(02).
               10  CU-USAGE-TYPE           PIC X(01).
               10  CU-JURIS-CD             PIC X(01).
               10  CU-RATE-ELEM            PIC X(06).
           05  CU-DATE-TIME.
               10  CU-CONN-YYDDD.
                   15  CU-CONN-YY          PIC 9(02).
                   15  CU-CONN-DDD         PIC 9(03).
               10  CU-CONN-HHMMSS          PIC 9(06).
               10  CU-DISC-YYDDD           PIC 9(05).
               10  CU-DISC-HHMMSS          PIC 9(06).
           05  CU-VARIANT-AREA             PIC X(96).
           05  CU-VOICE-DETAIL REDEFINES CU-VARIANT-AREA.
               10  CU-VC-ORIG-NPANXX       PIC 9(06).
               10  CU-VC-TERM-NPANXX       PIC 9(06).
               10  CU-VC-CONV-MIN          PIC S9(07)V9(02) COMP-3.
               10  CU-VC-CHG-MIN           PIC S9(07)V9(02) COMP-3.
               10  CU-VC-FILLER            PIC X(74).
           05  CU-DATA-DETAIL REDEFINES CU-VARIANT-AREA.
               10  CU-DT-CIRCUIT-ID        PIC X(20).
               10  CU-DT-OCTETS-IN         PIC S9(15)    COMP-3.
               10  CU-DT-OCTETS-OUT        PIC S9(15)    COMP-3.
               10  CU-DT-FILLER            PIC X(60).
           05  CU-SPCL-DETAIL REDEFINES CU-VARIANT-AREA.
               10  CU-SP-QTY               PIC S9(05)    COMP-3.
               10  CU-SP-FILLER            PIC X(91).
           05  CU-AUDIT.
               10  CU-SRC-SYSTEM           PIC X(04).
               10  CU-LOAD-YYDDD           PIC 9(05).
               10  CU-EDIT-STATUS          PIC X(01).
               10  CU-FILLER               PIC X(40).
      * PER-STREAM RAW RECORD BUFFERS AND MERGE-KEY EXTRACTS
       01  WS-VOC-BUFFER-RAW               PIC X(200).
       01  WS-VOC-KEY-AREA.
           05  VB-OCN                      PIC X(04).
           05  VB-BAN                      PIC X(13).
           05  VB-CONN-YYDDD               PIC 9(05).
       01  WS-DAT-BUFFER-RAW               PIC X(200).
       01  WS-DAT-KEY-AREA.
           05  DB-OCN                      PIC X(04).
           05  DB-BAN                      PIC X(13).
           05  DB-CONN-YYDDD               PIC 9(05).
       01  WS-SPC-BUFFER-RAW               PIC X(200).
       01  WS-SPC-KEY-AREA.
           05  SB-OCN                      PIC X(04).
           05  SB-BAN                      PIC X(13).
           05  SB-CONN-YYDDD               PIC 9(05).
       01  WS-LOW-KEY-AREA.
           05  WS-LOW-OCN                  PIC X(04).
           05  WS-LOW-BAN                  PIC X(13).
           05  WS-LOW-CONN-YYDDD           PIC 9(05).
      * STREAM EOF SWITCHES AND SELECTOR
       01  WS-STREAM-EOF-SWITCHES.
           05  WS-VOC-EOF-SW               PIC X(01) VALUE 'N'.
               88  WS-VOC-EOF               VALUE 'Y'.
           05  WS-DAT-EOF-SW               PIC X(01) VALUE 'N'.
               88  WS-DAT-EOF               VALUE 'Y'.
           05  WS-SPC-EOF-SW               PIC X(01) VALUE 'N'.
               88  WS-SPC-EOF               VALUE 'Y'.
       01  WS-STREAM-SELECTED              PIC 9(01) VALUE 0.
      *---------------------------------------------------------------*
      * OCN SUMMARY TABLE - CABS-STD-007, SECOND INSTANCE.  OCCURS
      * DEPENDING ON WS-SUMM-COUNT, NOT A FIXED OCCURS 300.
      *---------------------------------------------------------------*
       01  WS-SUMMARY-CTL.
           05  WS-SUMM-COUNT                PIC S9(04) COMP-3 VALUE 0.
           05  WS-SUMM-MAX                  PIC S9(04) COMP-3
                                                          VALUE 300.
           05  WS-SUMM-SUB                  PIC S9(04) COMP-3 VALUE 0.
           05  WS-SUMM-FOUND-SW             PIC X(01) VALUE 'N'.
               88  WS-SUMM-FOUND            VALUE 'Y'.
       01  WS-SUMMARY-TABLE.
           05  WS-SUMM-ENTRY OCCURS 1 TO 300 TIMES
                   DEPENDING ON WS-SUMM-COUNT.
               10  SM-OCN                  PIC X(04).
               10  SM-VOICE-MIN            PIC S9(09)V9(02) COMP-3.
               10  SM-DATA-OCTETS          PIC S9(15)       COMP-3.
               10  SM-SPCL-QTY             PIC S9(07)       COMP-3.
               10  SM-REC-COUNT            PIC S9(07)       COMP-3.
               10  SM-INTERSTATE-MIN       PIC S9(09)V9(02) COMP-3.
               10  SM-INTRASTATE-MIN       PIC S9(09)V9(02) COMP-3.
               10  SM-LOCAL-MIN            PIC S9(09)V9(02) COMP-3.
               10  SM-INDETERM-MIN         PIC S9(09)V9(02) COMP-3.
      * JURISDICTION GRAND TOTALS - PRINTED AS A SECOND REPORT BLOCK
       01  WS-JURIS-GRAND-TOTALS.
           05  WS-GRAND-INTERSTATE-MIN     PIC S9(11)V9(02) COMP-3
                                                             VALUE 0.
           05  WS-GRAND-INTRASTATE-MIN     PIC S9(11)V9(02) COMP-3
                                                             VALUE 0.
           05  WS-GRAND-LOCAL-MIN          PIC S9(11)V9(02) COMP-3
                                                             VALUE 0.
           05  WS-GRAND-INDETERM-MIN       PIC S9(11)V9(02) COMP-3
                                                             VALUE 0.
      * GRAND TOTALS FOR THE FINAL REPORT BREAK
       01  WS-GRAND-TOTALS.
           05  WS-GRAND-VOICE-MIN          PIC S9(11)V9(02) COMP-3
                                                             VALUE 0.
           05  WS-GRAND-DATA-OCTETS        PIC S9(17)       COMP-3
                                                             VALUE 0.
           05  WS-GRAND-SPCL-QTY           PIC S9(09)       COMP-3
                                                             VALUE 0.
           05  WS-GRAND-REC-COUNT          PIC S9(09)       COMP-3
                                                             VALUE 0.
      * NEGATIVE MINUTE ADJUSTMENT WORK AREA.  USED BY P6600 WHEN
      * A STREAM RETURNS A CREDIT VOLUME.  SEE CABS-STD-026.
       01  WS-CONSOL-CTL.
           05  WS-CONSOL-CNT               PIC S9(07) COMP-3 VALUE 0.
           05  WS-NEG-ADJ-AMOUNT           PIC S9(09)V9(02) COMP-3
                                                             VALUE 0.
           05  WS-NEG-ADJ-TOTAL            PIC S9(09)V9(02) COMP-3
                                                             VALUE 0.
      * REPORT WORK AREAS
       01  WS-REPORT-EDIT-AREA.
           05  WS-RPT-OCN                  PIC X(04).
           05  WS-RPT-VOICE-MIN-ED         PIC ZZZ,ZZZ,ZZ9.99.
           05  WS-RPT-DATA-OCTETS-ED       PIC Z,ZZZ,ZZZ,ZZ9.
           05  WS-RPT-SPCL-QTY-ED          PIC ZZZ,ZZ9.
           05  WS-RPT-REC-COUNT-ED         PIC ZZZ,ZZ9.
       01  WS-REPORT-CTL.
           05  WS-RPT-LINE-CNT             PIC S9(05) COMP-3 VALUE 0.
           05  WS-RPT-PAGE-NBR             PIC S9(03) COMP-3 VALUE 1.
       01  WS-REPORT-DATE-AREA.
           05  WS-RPT-RUN-DATE             PIC 9(08).
       01  WS-REPORT-HEADING-AREA.
           05  WS-RPT-COL-HDR-1            PIC X(60) VALUE
               'OCN     VOICE-MIN     DATA-OCTETS   SPCL-QTY  RECS'.
           05  WS-RPT-UNDERLINE            PIC X(60) VALUE ALL '-'.
      * PER-STREAM SELECTION COUNTS AND USAGE-TYPE VOLUME COUNTS
       01  WS-MERGE-STATISTICS.
           05  WS-VOC-SELECTED-CNT         PIC S9(07) COMP-3 VALUE 0.
           05  WS-DAT-SELECTED-CNT         PIC S9(07) COMP-3 VALUE 0.
           05  WS-SPC-SELECTED-CNT         PIC S9(07) COMP-3 VALUE 0.
       01  WS-USAGE-TYPE-COUNTS.
           05  WS-TOTAL-VOICE-RECS         PIC S9(07) COMP-3 VALUE 0.
           05  WS-TOTAL-DATA-RECS          PIC S9(07) COMP-3 VALUE 0.
           05  WS-TOTAL-SPCL-RECS          PIC S9(07) COMP-3 VALUE 0.
      * OCN VALIDATION - CALLED ONCE PER NEW SUMMARY TABLE INSERT
       01  WS-VALIDATION-SWITCHES.
           05  WS-OCN-VALID-SW             PIC X(01) VALUE 'Y'.
               88  WS-OCN-VALID            VALUE 'Y'.
       01  WS-INVALID-OCN-COUNTER.
           05  WS-INVALID-OCN-CNT          PIC S9(05) COMP-3 VALUE 0.
       01  WS-DATE-VALIDATION-AREA.
           05  WS-DATE-VALIDATION-SW       PIC X(01) VALUE 'Y'.
               88  WS-DATE-VALID           VALUE 'Y'.
           05  WS-ZERO-DATE-CNT            PIC S9(05) COMP-3 VALUE 0.
           05  WS-FUTURE-DATE-CNT          PIC S9(05) COMP-3 VALUE 0.
       01  WS-CALL-PARM-AREA.
           05  WS-CP-DATE-OUT              PIC 9(08).
           05  WS-CP-RC                    PIC S9(04) COMP-3.
           05  WS-CP-OCN                   PIC X(04).
           05  WS-CP-VALID-SW              PIC X(01).
           05  WS-CP-HASH-IN               PIC S9(09) COMP-3.
       01  WS-PARM-VALIDATION.
           05  WS-PARM-VALID-SW            PIC X(01) VALUE 'Y'.
               88  WS-PARM-VALID           VALUE 'Y'.
       01  WS-JOB-INFO.
           05  WS-JOBNAME                  PIC X(08).
           05  WS-STEPNAME                 PIC X(08).
       01  WS-TIMESTAMP-AREA.
           05  WS-JOB-START-TIME           PIC X(08).
       01  WS-RESTART-WORK-AREA.
           05  WS-RESTART-KEY-SAVE         PIC X(26).
           05  WS-RESTART-SEQ-DISP         PIC 9(09).
       01  WS-DISPLAY-WORK-AREA.
           05  WS-DISPLAY-LINE             PIC X(80).
       01  WS-ERROR-WORK-AREA.
           05  WS-ERR-TEXT                 PIC X(60).
      * MISCELLANEOUS
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABING09'.
           05  WS-PROCESS-ID               PIC X(08) VALUE 'ING09   '.
       01  WS-RUN-ID-AREA.
           05  WS-RUN-ID                   PIC X(12).
       01  WS-RUN-ID-SPLIT REDEFINES WS-RUN-ID-AREA.
           05  WS-RUN-ID-PREFIX            PIC X(08).
           05  WS-RUN-ID-SUFFIX            PIC X(04).
       01  WS-CT-BAL-CHECK-AREA.
           05  WS-CT-BAL-CHECK             PIC S9(11) COMP-3 VALUE 0.
       01  WS-ABEND-WORK-AREA.
           05  WS-ABEND-REASON             PIC X(40).
      *
       PROCEDURE DIVISION.
      *
       S100-MAINLINE SECTION.
       P0000-MAINLINE.
           PERFORM P1000-INIT THRU P1000-EXIT.
           PERFORM P2000-PROCESS THRU P2000-EXIT
               UNTIL WS-VOC-EOF AND WS-DAT-EOF AND WS-SPC-EOF.
           PERFORM P7000-PRINT-REPORT THRU P7000-EXIT.
           PERFORM P8000-CONTROL THRU P8000-EXIT.
           PERFORM P9000-TERM THRU P9000-EXIT.
           STOP RUN.
      *
       S200-INITIALIZATION SECTION.
       P1000-INIT.
           PERFORM P1100-OPEN-FILES THRU P1100-EXIT.
           CALL 'CABPARMR' USING WS-RUN-ID.
           CALL 'CABDTCNV' USING DW-CURRENT-YYDDD WS-CP-DATE-OUT
               WS-CP-RC.
           MOVE WS-CP-DATE-OUT TO WS-RPT-RUN-DATE.
           PERFORM P1200-VALIDATE-PARMS THRU P1200-EXIT.
           PERFORM P1300-INIT-READS THRU P1300-EXIT.
       P1000-EXIT.
           EXIT.
      *
       P1100-OPEN-FILES.
           OPEN INPUT  VOCIN.
           OPEN INPUT  DATIN.
           OPEN INPUT  SPCIN.
           OPEN OUTPUT CONOUT.
           OPEN OUTPUT RPTOUT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-INPUT NOT = '00'
               DISPLAY 'CABING09 - VOCIN OPEN FAILED ' WS-FS-INPUT
               CALL 'CABABEND' USING CT-ABEND-CD.
       P1100-EXIT.
           EXIT.
      *
      * P1200 SANITY-CHECKS THE RUN ID SUPPLIED BY THE PARM READER.
      * AN INVALID RUN ID IS FATAL - THERE IS NO SAFE DEFAULT FOR THE
      * CONTROL RECORD KEY.
       P1200-VALIDATE-PARMS.
           MOVE 'Y' TO WS-PARM-VALID-SW.
           IF WS-RUN-ID = SPACES
               MOVE 'N' TO WS-PARM-VALID-SW.
           MOVE WS-RUN-ID-PREFIX TO WS-JOBNAME.
           IF NOT WS-PARM-VALID
               DISPLAY 'CABING09 - INVALID RUN ID ON PARM CARD'
               CALL 'CABABEND' USING CT-ABEND-CD.
       P1200-EXIT.
           EXIT.
      *
       P1300-INIT-READS.
           PERFORM P6100-READ-VOC THRU P6100-EXIT.
           PERFORM P6200-READ-DAT THRU P6200-EXIT.
           PERFORM P6300-READ-SPC THRU P6300-EXIT.
       P1300-EXIT.
           EXIT.
      *
       S300-MERGE-AND-CONSOLIDATION SECTION.
       P2000-PROCESS.
           PERFORM P3000-SELECT-LOW-KEY THRU P3000-EXIT.
           GO TO P5100-CONSOL-VOICE P5200-CONSOL-DATA
                 P5300-CONSOL-SPCL
               DEPENDING ON WS-STREAM-SELECTED.
       P2000-CONTINUE.
           PERFORM P6000-READ-NEXT-SELECTED THRU P6000-EXIT.
       P2000-EXIT.
           EXIT.
      *
      * P3000 IS THE LOW-KEY SELECTION PARAGRAPH FOR THE 3-WAY MERGE.
      * KEY ORDER IS OCN / BAN / CONNECT DATE, PER THE STANDARD.
       P3000-SELECT-LOW-KEY.
           MOVE HIGH-VALUES TO WS-LOW-KEY-AREA.
           MOVE ZERO TO WS-STREAM-SELECTED.
           IF WS-VOC-EOF
               GO TO P3100-CHECK-DAT.
           IF WS-VOC-KEY-AREA NOT < WS-LOW-KEY-AREA
               GO TO P3100-CHECK-DAT.
           MOVE VB-OCN TO WS-LOW-OCN.
           MOVE VB-BAN TO WS-LOW-BAN.
           MOVE VB-CONN-YYDDD TO WS-LOW-CONN-YYDDD.
           MOVE 1 TO WS-STREAM-SELECTED.
       P3100-CHECK-DAT.
           IF WS-DAT-EOF
               GO TO P3200-CHECK-SPC.
           IF WS-DAT-KEY-AREA NOT < WS-LOW-KEY-AREA
               GO TO P3200-CHECK-SPC.
           MOVE DB-OCN TO WS-LOW-OCN.
           MOVE DB-BAN TO WS-LOW-BAN.
           MOVE DB-CONN-YYDDD TO WS-LOW-CONN-YYDDD.
           MOVE 2 TO WS-STREAM-SELECTED.
       P3200-CHECK-SPC.
           IF WS-SPC-EOF
               GO TO P3300-STAGE-RECORD.
           IF WS-SPC-KEY-AREA NOT < WS-LOW-KEY-AREA
               GO TO P3300-STAGE-RECORD.
           MOVE SB-OCN TO WS-LOW-OCN.
           MOVE SB-BAN TO WS-LOW-BAN.
           MOVE SB-CONN-YYDDD TO WS-LOW-CONN-YYDDD.
           MOVE 3 TO WS-STREAM-SELECTED.
       P3300-STAGE-RECORD.
           IF WS-STREAM-SELECTED = 1
               MOVE WS-VOC-BUFFER-RAW TO WS-CURRENT-REC.
           IF WS-STREAM-SELECTED = 2
               MOVE WS-DAT-BUFFER-RAW TO WS-CURRENT-REC.
           IF WS-STREAM-SELECTED = 3
               MOVE WS-SPC-BUFFER-RAW TO WS-CURRENT-REC.
       P3000-EXIT.
           EXIT.
      *
      * P4700 SEARCHES THE OCN SUMMARY TABLE, INSERTING A NEW ENTRY
      * WHEN THE OCN HAS NOT BEEN SEEN YET THIS RUN.
       P4700-FIND-OR-INSERT-OCN.
           MOVE 'N' TO WS-SUMM-FOUND-SW.
           MOVE 1 TO WS-SUMM-SUB.
           PERFORM P4710-SCAN-ENTRY THRU P4710-EXIT
               UNTIL WS-SUMM-SUB > WS-SUMM-COUNT
               OR WS-SUMM-FOUND.
           IF WS-SUMM-FOUND
               GO TO P4700-EXIT.
           IF WS-SUMM-COUNT NOT < WS-SUMM-MAX
               GO TO P4700-EXIT.
           ADD 1 TO WS-SUMM-COUNT.
           MOVE WS-SUMM-COUNT TO WS-SUMM-SUB.
           MOVE CU-OCN TO SM-OCN (WS-SUMM-SUB).
           MOVE ZERO TO SM-VOICE-MIN (WS-SUMM-SUB).
           MOVE ZERO TO SM-DATA-OCTETS (WS-SUMM-SUB).
           MOVE ZERO TO SM-SPCL-QTY (WS-SUMM-SUB).
           MOVE ZERO TO SM-REC-COUNT (WS-SUMM-SUB).
           MOVE ZERO TO SM-INTERSTATE-MIN (WS-SUMM-SUB).
           MOVE ZERO TO SM-INTRASTATE-MIN (WS-SUMM-SUB).
           MOVE ZERO TO SM-LOCAL-MIN (WS-SUMM-SUB).
           MOVE ZERO TO SM-INDETERM-MIN (WS-SUMM-SUB).
           MOVE CU-OCN TO WS-CP-OCN.
           CALL 'CABOCNVL' USING WS-CP-OCN WS-CP-VALID-SW.
           IF WS-CP-VALID-SW NOT = 'Y'
               ADD 1 TO WS-INVALID-OCN-CNT.
           PERFORM P4720-VALIDATE-CONN-DATE THRU P4720-EXIT.
       P4700-EXIT.
           EXIT.
      *
       P4710-SCAN-ENTRY.
           IF SM-OCN (WS-SUMM-SUB) = CU-OCN
               MOVE 'Y' TO WS-SUMM-FOUND-SW
               GO TO P4710-EXIT.
           ADD 1 TO WS-SUMM-SUB.
       P4710-EXIT.
           EXIT.
      *
      * P4720 IS A LIGHTWEIGHT SANITY CHECK ON THE CONNECT DATE OF THE
      * FIRST RECORD SEEN FOR A NEW OCN.  A ZERO OR FUTURE-DATED
      * CONNECT DATE DOES NOT STOP THE RUN - IT ONLY COUNTS, SINCE THE
      * UPSTREAM EDIT STEP (CABING01) IS THE PROPER GATE FOR THIS.
       P4720-VALIDATE-CONN-DATE.
           MOVE 'Y' TO WS-DATE-VALIDATION-SW.
           IF CU-CONN-YYDDD = ZERO
               MOVE 'N' TO WS-DATE-VALIDATION-SW
               ADD 1 TO WS-ZERO-DATE-CNT
               GO TO P4720-EXIT.
           IF CU-CONN-YYDDD > DW-CURRENT-YYDDD
               MOVE 'N' TO WS-DATE-VALIDATION-SW
               ADD 1 TO WS-FUTURE-DATE-CNT.
       P4720-EXIT.
           EXIT.
      *
      * P5150 SPLITS VOICE MINUTES BY JURISDICTION FOR THE SECOND
      * REPORT BLOCK.  CU-JURIS-CD 88-LEVELS ARE THE SAME ONES USED
      * ACROSS THE ESTATE (I/S/L/X-OR-SPACE).
       P5150-CLASSIFY-JURISDICTION.
           IF CU-JURIS-CD = 'I'
               ADD CU-VC-CHG-MIN TO SM-INTERSTATE-MIN (WS-SUMM-SUB)
               GO TO P5150-EXIT.
           IF CU-JURIS-CD = 'S'
               ADD CU-VC-CHG-MIN TO SM-INTRASTATE-MIN (WS-SUMM-SUB)
               GO TO P5150-EXIT.
           IF CU-JURIS-CD = 'L'
               ADD CU-VC-CHG-MIN TO SM-LOCAL-MIN (WS-SUMM-SUB)
               GO TO P5150-EXIT.
           ADD CU-VC-CHG-MIN TO SM-INDETERM-MIN (WS-SUMM-SUB).
       P5150-EXIT.
           EXIT.
      *
       P5100-CONSOL-VOICE.
           PERFORM P4700-FIND-OR-INSERT-OCN THRU P4700-EXIT.
           ADD CU-VC-CHG-MIN TO SM-VOICE-MIN (WS-SUMM-SUB).
           ADD 1 TO SM-REC-COUNT (WS-SUMM-SUB).
           ADD 1 TO WS-TOTAL-VOICE-RECS.
           PERFORM P5150-CLASSIFY-JURISDICTION THRU P5150-EXIT.
       P5200-CONSOL-DATA.
           PERFORM P4700-FIND-OR-INSERT-OCN THRU P4700-EXIT.
           ADD CU-DT-OCTETS-IN TO SM-DATA-OCTETS (WS-SUMM-SUB).
           ADD 1 TO SM-REC-COUNT (WS-SUMM-SUB).
           ADD 1 TO WS-TOTAL-DATA-RECS.
           PERFORM P5900-WRITE-CONOUT THRU P5900-EXIT.
           GO TO P2000-CONTINUE.
       P5200-EXIT.
           EXIT.
      *
       P5300-CONSOL-SPCL.
           PERFORM P4700-FIND-OR-INSERT-OCN THRU P4700-EXIT.
           ADD 1 TO WS-TOTAL-SPCL-RECS.
           ADD CU-SP-QTY TO SM-SPCL-QTY (WS-SUMM-SUB).
           ADD 1 TO SM-REC-COUNT (WS-SUMM-SUB).
           PERFORM P5900-WRITE-CONOUT THRU P5900-EXIT.
           GO TO P2000-CONTINUE.
       P5300-EXIT.
           EXIT.
      *
       P5900-WRITE-CONOUT.
           MOVE WS-CURRENT-REC TO CONOUT-RECORD.
           WRITE CONOUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
           ADD 1 TO WS-CONSOL-CNT.
           MOVE CU-SEQ-NBR TO WS-CP-HASH-IN.
           CALL 'CABHASH' USING WS-CP-HASH-IN WS-ACC-SEQ-HASH.
           PERFORM P6600-NEGATIVE-MOU-ADJ THRU P6600-EXIT.
       P5900-EXIT.
           EXIT.
      *
       P6000-READ-NEXT-SELECTED.
           GO TO P6100-READ-VOC P6200-READ-DAT P6300-READ-SPC
               DEPENDING ON WS-STREAM-SELECTED.
       P6100-READ-VOC.
           READ VOCIN
               AT END
                   MOVE 'Y' TO WS-VOC-EOF-SW
                   GO TO P6000-EXIT.
           ADD 1 TO WS-READ-CNT.
           ADD 1 TO WS-VOC-SELECTED-CNT.
           MOVE CABS-CDR-RECORD TO WS-VOC-BUFFER-RAW.
           MOVE CD-OCN TO VB-OCN.
           MOVE CD-BAN TO VB-BAN.
           MOVE CD-CONN-YYDDD TO VB-CONN-YYDDD.
           GO TO P6000-EXIT.
       P6200-READ-DAT.
           READ DATIN
               AT END
                   MOVE 'Y' TO WS-DAT-EOF-SW
                   GO TO P6000-EXIT.
           ADD 1 TO WS-READ-CNT.
           ADD 1 TO WS-DAT-SELECTED-CNT.
           IF WS-DAT-REC-LEN < 150
               MOVE SPACES TO DT-EXTENDED-TAIL.
           MOVE SPACES TO WS-DAT-BUFFER-RAW.
           MOVE DATIN-RECORD TO WS-DAT-BUFFER-RAW.
           MOVE DT-OCN TO DB-OCN.
           MOVE DT-BAN TO DB-BAN.
           MOVE DT-CONN-YYDDD TO DB-CONN-YYDDD.
           GO TO P6000-EXIT.
       P6300-READ-SPC.
           READ SPCIN
               AT END
                   MOVE 'Y' TO WS-SPC-EOF-SW
                   GO TO P6000-EXIT.
           ADD 1 TO WS-READ-CNT.
           ADD 1 TO WS-SPC-SELECTED-CNT.
           MOVE CABS-CDR-RECORD-SP TO WS-SPC-BUFFER-RAW.
           MOVE SD-OCN TO SB-OCN.
           MOVE SD-BAN TO SB-BAN.
           MOVE SD-CONN-YYDDD TO SB-CONN-YYDDD.
       P6000-EXIT.
           EXIT.
      *
      * P6600 - APPLIES THE NEGATIVE MINUTE ADJUSTMENT WHERE A
      * STREAM RETURNS A CREDIT VOLUME OUTSIDE THE NORMAL RANGE.
      * ADDED FOR THE 1996 RESTATEMENT.  RARELY TAKEN.
       P6600-NEGATIVE-MOU-ADJ.
           IF WS-CONSOL-CNT NOT < ZERO
               GO TO P6600-EXIT.
           IF WS-CONSOL-CNT NOT > 100
               GO TO P6600-EXIT.
           MOVE ZERO TO WS-NEG-ADJ-AMOUNT.
           IF SM-VOICE-MIN (WS-SUMM-SUB) NOT < ZERO
               GO TO P6600-CHECK-OCTETS.
           COMPUTE WS-NEG-ADJ-AMOUNT =
               ZERO - SM-VOICE-MIN (WS-SUMM-SUB).
           MOVE ZERO TO SM-VOICE-MIN (WS-SUMM-SUB).
           ADD WS-NEG-ADJ-AMOUNT TO WS-NEG-ADJ-TOTAL.
           ADD 1 TO WS-REJECT-CNT.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           MOVE EC-MIN-NEGATIVE TO SU-ERR-CODE.
           MOVE 'W' TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME TO SU-DETECT-PGM.
           MOVE 'P6600-NEGATIVE-MOU-ADJ' TO SU-DETECT-PARA.
           MOVE WS-RUN-ID TO SU-RUN-ID.
           MOVE WS-CURRENT-REC TO SU-ORIG-RECORD.
           DISPLAY 'CABING09 NEGATIVE MOU ADJUSTED FOR OCN '
               SM-OCN (WS-SUMM-SUB).
           IF SM-REC-COUNT (WS-SUMM-SUB) > ZERO
               SUBTRACT 1 FROM SM-REC-COUNT (WS-SUMM-SUB).
       P6600-CHECK-OCTETS.
           IF SM-DATA-OCTETS (WS-SUMM-SUB) < ZERO
               MOVE ZERO TO SM-DATA-OCTETS (WS-SUMM-SUB).
       P6600-EXIT.
           EXIT.
      *
       S400-REPORT SECTION.
      * P7000 WALKS THE SUMMARY TABLE IN ITS NATURAL (OCN-ASCENDING)
      * INSERTION ORDER AND PRINTS ONE DETAIL LINE PER OCN, THEN A
      * SINGLE FINAL CONTROL-BREAK TOTAL LINE.
       P7000-PRINT-REPORT.
           PERFORM P7100-PRINT-HEADING THRU P7100-EXIT.
           MOVE 1 TO WS-SUMM-SUB.
           PERFORM P7200-PRINT-DETAIL-LINE THRU P7200-EXIT
               VARYING WS-SUMM-SUB FROM 1 BY 1
               UNTIL WS-SUMM-SUB > WS-SUMM-COUNT.
           PERFORM P7300-PRINT-TOTAL-LINE THRU P7300-EXIT.
       P7000-EXIT.
           EXIT.
      *
       P7100-PRINT-HEADING.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '1' TO PC-CC.
           MOVE 'CABS DAILY CONSOLIDATION VOLUME REPORT' TO PC-TEXT.
           PERFORM P7400-WRITE-RPTOUT THRU P7400-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           STRING 'RUN ' WS-RUN-ID ' DATE ' WS-RPT-RUN-DATE
               DELIMITED BY SIZE INTO PC-TEXT.
           PERFORM P7400-WRITE-RPTOUT THRU P7400-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '0' TO PC-CC.
           MOVE WS-RPT-COL-HDR-1 TO PC-TEXT.
           PERFORM P7400-WRITE-RPTOUT THRU P7400-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-RPT-UNDERLINE TO PC-TEXT.
           PERFORM P7400-WRITE-RPTOUT THRU P7400-EXIT.
       P7100-EXIT.
           EXIT.
      *
       P7200-PRINT-DETAIL-LINE.
           MOVE SM-OCN (WS-SUMM-SUB) TO WS-RPT-OCN.
           MOVE SM-VOICE-MIN (WS-SUMM-SUB) TO WS-RPT-VOICE-MIN-ED.
           MOVE SM-DATA-OCTETS (WS-SUMM-SUB) TO
               WS-RPT-DATA-OCTETS-ED.
           MOVE SM-SPCL-QTY (WS-SUMM-SUB) TO WS-RPT-SPCL-QTY-ED.
           MOVE SM-REC-COUNT (WS-SUMM-SUB) TO WS-RPT-REC-COUNT-ED.
           ADD SM-VOICE-MIN (WS-SUMM-SUB) TO WS-GRAND-VOICE-MIN.
           ADD SM-DATA-OCTETS (WS-SUMM-SUB) TO WS-GRAND-DATA-OCTETS.
           ADD SM-SPCL-QTY (WS-SUMM-SUB) TO WS-GRAND-SPCL-QTY.
           ADD SM-REC-COUNT (WS-SUMM-SUB) TO WS-GRAND-REC-COUNT.
           ADD SM-INTERSTATE-MIN (WS-SUMM-SUB) TO
               WS-GRAND-INTERSTATE-MIN.
           ADD SM-INTRASTATE-MIN (WS-SUMM-SUB) TO
               WS-GRAND-INTRASTATE-MIN.
           ADD SM-LOCAL-MIN (WS-SUMM-SUB) TO WS-GRAND-LOCAL-MIN.
           ADD SM-INDETERM-MIN (WS-SUMM-SUB) TO
               WS-GRAND-INDETERM-MIN.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           STRING WS-RPT-OCN ' ' WS-RPT-VOICE-MIN-ED ' '
               WS-RPT-DATA-OCTETS-ED ' ' WS-RPT-SPCL-QTY-ED ' '
               WS-RPT-REC-COUNT-ED DELIMITED BY SIZE INTO PC-TEXT.
           PERFORM P7400-WRITE-RPTOUT THRU P7400-EXIT.
       P7200-EXIT.
           EXIT.
      *
       P7300-PRINT-TOTAL-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '0' TO PC-CC.
           MOVE 'TOTAL' TO WS-RPT-OCN.
           MOVE WS-GRAND-VOICE-MIN TO WS-RPT-VOICE-MIN-ED.
           MOVE WS-GRAND-DATA-OCTETS TO WS-RPT-DATA-OCTETS-ED.
           MOVE WS-GRAND-SPCL-QTY TO WS-RPT-SPCL-QTY-ED.
           MOVE WS-GRAND-REC-COUNT TO WS-RPT-REC-COUNT-ED.
           STRING WS-RPT-OCN ' ' WS-RPT-VOICE-MIN-ED ' '
               WS-RPT-DATA-OCTETS-ED ' ' WS-RPT-SPCL-QTY-ED ' '
               WS-RPT-REC-COUNT-ED DELIMITED BY SIZE INTO PC-TEXT.
           PERFORM P7400-WRITE-RPTOUT THRU P7400-EXIT.
           PERFORM P7350-PRINT-JURIS-BLOCK THRU P7350-EXIT.
           PERFORM P7360-PRINT-STREAM-SUMMARY THRU P7360-EXIT.
       P7300-EXIT.
           EXIT.
      *
      * P7350 IS A SECOND REPORT BLOCK - VOICE MINUTES BY JURISDICTION
      * ACROSS ALL OCNS.  A SEPARATE LEVEL BREAK FROM THE OCN DETAIL.
       P7350-PRINT-JURIS-BLOCK.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '0' TO PC-CC.
           MOVE 'VOICE MINUTES BY JURISDICTION' TO PC-TEXT.
           PERFORM P7400-WRITE-RPTOUT THRU P7400-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-GRAND-INTERSTATE-MIN TO WS-RPT-VOICE-MIN-ED.
           STRING 'INTERSTATE ' WS-RPT-VOICE-MIN-ED
               DELIMITED BY SIZE INTO PC-TEXT.
           PERFORM P7400-WRITE-RPTOUT THRU P7400-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-GRAND-INTRASTATE-MIN TO WS-RPT-VOICE-MIN-ED.
           STRING 'INTRASTATE ' WS-RPT-VOICE-MIN-ED
               DELIMITED BY SIZE INTO PC-TEXT.
           PERFORM P7400-WRITE-RPTOUT THRU P7400-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-GRAND-LOCAL-MIN TO WS-RPT-VOICE-MIN-ED.
           STRING 'LOCAL      ' WS-RPT-VOICE-MIN-ED
               DELIMITED BY SIZE INTO PC-TEXT.
           PERFORM P7400-WRITE-RPTOUT THRU P7400-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-GRAND-INDETERM-MIN TO WS-RPT-VOICE-MIN-ED.
           STRING 'INDETERM   ' WS-RPT-VOICE-MIN-ED
               DELIMITED BY SIZE INTO PC-TEXT.
           PERFORM P7400-WRITE-RPTOUT THRU P7400-EXIT.
       P7350-EXIT.
           EXIT.
      *
      * P7360 PRINTS THE INPUT-STREAM SELECTION COUNTS - HOW MANY
      * RECORDS THE MERGE TOOK FROM EACH OF THE THREE INPUTS.
       P7360-PRINT-STREAM-SUMMARY.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '0' TO PC-CC.
           MOVE 'INPUT RECORDS BY STREAM' TO PC-TEXT.
           PERFORM P7400-WRITE-RPTOUT THRU P7400-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-VOC-SELECTED-CNT TO WS-RPT-REC-COUNT-ED.
           STRING 'VOICE   ' WS-RPT-REC-COUNT-ED
               DELIMITED BY SIZE INTO PC-TEXT.
           PERFORM P7400-WRITE-RPTOUT THRU P7400-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-DAT-SELECTED-CNT TO WS-RPT-REC-COUNT-ED.
           STRING 'DATA    ' WS-RPT-REC-COUNT-ED
               DELIMITED BY SIZE INTO PC-TEXT.
           PERFORM P7400-WRITE-RPTOUT THRU P7400-EXIT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE WS-SPC-SELECTED-CNT TO WS-RPT-REC-COUNT-ED.
           STRING 'SPECIAL ' WS-RPT-REC-COUNT-ED
               DELIMITED BY SIZE INTO PC-TEXT.
           PERFORM P7400-WRITE-RPTOUT THRU P7400-EXIT.
       P7360-EXIT.
           EXIT.
      *
       P7400-WRITE-RPTOUT.
           WRITE CABS-PRINT-LINE.
           ADD 1 TO WS-RPT-LINE-CNT.
       P7400-EXIT.
           EXIT.
      *
       S500-RUN-CONTROL SECTION.
       P8000-CONTROL.
           PERFORM P8100-BUILD-CONTROL-REC THRU P8100-EXIT.
           PERFORM P8200-WRITE-CONTROL THRU P8200-EXIT.
       P8000-EXIT.
           EXIT.
      *
       P8100-BUILD-CONTROL-REC.
           MOVE SPACES TO CABS-CONTROL-RECORD.
           MOVE WS-RUN-ID TO CT-RUN-ID.
           MOVE WS-PROCESS-ID TO CT-PROCESS-ID.
           MOVE 1 TO CT-STEP-SEQ.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE ZERO TO CT-SUMMARISED.
           MOVE ZERO TO CT-CARRIED-FWD.
           MOVE WS-GRAND-VOICE-MIN TO CT-HASH-MINUTES.
           MOVE ZERO TO CT-HASH-AMOUNT.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE ZERO TO CT-HASH-OCN.
           MOVE CU-SEQ-NBR TO WS-RESTART-SEQ-DISP.
           STRING CU-OCN ' ' WS-RESTART-SEQ-DISP
               DELIMITED BY SIZE INTO WS-RESTART-KEY-SAVE.
           MOVE WS-RESTART-KEY-SAVE TO CT-RESTART-KEY.
           COMPUTE WS-CT-BAL-CHECK = CT-WRITTEN + CT-REJECTED +
               CT-SUMMARISED + CT-CARRIED-FWD.
           IF CT-READ = WS-CT-BAL-CHECK
               SET CT-IN-BALANCE TO TRUE
           ELSE
               SET CT-OUT-OF-BAL TO TRUE.
       P8100-EXIT.
           EXIT.
      *
       P8200-WRITE-CONTROL.
           MOVE CABS-CONTROL-RECORD TO CTLOUT-RECORD.
           WRITE CTLOUT-RECORD.
       P8200-EXIT.
           EXIT.
      *
       S600-TERMINATION SECTION.
       P9000-TERM.
           PERFORM P9100-CLOSE-FILES THRU P9100-EXIT.
           PERFORM P9150-DISPLAY-SUMMARY THRU P9150-EXIT.
       P9000-EXIT.
           EXIT.
      *
       P9100-CLOSE-FILES.
           CLOSE VOCIN.
           CLOSE DATIN.
           CLOSE SPCIN.
           CLOSE CONOUT.
           CLOSE RPTOUT.
           CLOSE CTLOUT.
       P9100-EXIT.
           EXIT.
      *
      * P9150 IS AN OPERATOR-FACING SUMMARY - CONSOLE ONLY, NOT PART
      * OF THE CONTROL RECORD OR THE PRINTED REPORT.
       P9150-DISPLAY-SUMMARY.
           DISPLAY 'CABING09 RUN COMPLETE - READ=' WS-READ-CNT
               ' WRITTEN=' WS-WRITE-CNT.
           DISPLAY 'VOC=' WS-VOC-SELECTED-CNT
               ' DAT=' WS-DAT-SELECTED-CNT
               ' SPC=' WS-SPC-SELECTED-CNT.
           DISPLAY 'VOICE RECS=' WS-TOTAL-VOICE-RECS
               ' DATA RECS=' WS-TOTAL-DATA-RECS
               ' SPCL RECS=' WS-TOTAL-SPCL-RECS.
           DISPLAY 'OCN GROUPS=' WS-SUMM-COUNT
               ' INVALID OCN=' WS-INVALID-OCN-CNT.
           DISPLAY 'ZERO CONN DATE=' WS-ZERO-DATE-CNT
               ' FUTURE CONN DATE=' WS-FUTURE-DATE-CNT.
           DISPLAY 'RPT LINES=' WS-RPT-LINE-CNT
               ' NEG MOU ADJ TOTAL=' WS-NEG-ADJ-TOTAL.
       P9150-EXIT.
           EXIT.
