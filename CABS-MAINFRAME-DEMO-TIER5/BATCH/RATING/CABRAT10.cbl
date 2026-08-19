      *****************************************************************
      * CABRAT10 - BILL DETAIL LINE CONSTRUCTION                      *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               RATIN   TELCABS.CABS.RATED(0)         (LOCAL)   *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               BDTLOUT TELCABS.CABS.BILLDTL(+1)      CABSBILL  *
      *               BDTLFIX TELCABS.CABS.BILLDTL.FIXED(+1)(LOCAL)   *
      * CONTROL     : CTLOUT                               CABSCTL    *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +             *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * REVISION HISTORY                                              *
      *   V1.00  1989-09-05  R.T.WHEELER  INITIAL RELEASE - BUILDS    *
      *                      BILL DETAIL FOR SWITCHED ACCESS ONLY     *
      *   V1.03  1992-04-11  D.OKONKWO    ADDED FIXED-LENGTH EXTRACT  *
      *                      FOR THE REGULATORY REPORTING FEED        *
      *   V1.07  1996-01-30  J.M.CASTILLO Y2K REVIEW - PERIOD TEXT    *
      *                      ROUTINE CONFIRMED CENTURY-SAFE           *
      *   V2.00  1999-08-14  P.NAIR       SECTION CODE NOW DERIVED    *
      *                      FROM RATE ELEMENT PREFIX TABLE, NOT      *
      *                      HARDCODED PER-PROGRAM LITERAL            *
      *   V2.02  2003-02-19  A.BUKOWSKI   DESCRIPTION FORMATTER MADE  *
      *                      DYNAMIC - SECTION TABLE DRIVES THE CALL  *
      *   V2.05  2008-11-03  S.MARCHETTI  BDTLFIX EXTRACT WIDENED TO  *
      *                      500 BYTES TO CARRY MORE RATE ELEMENTS    *
      *   V2.08  2013-05-22  T.VANCE      RIGHT-TRIM ROUTINE ADDED TO *
      *                      DESCRIPTION BUILD - TRAILING BLANKS WERE *
      *                      CONFUSING THE DOWNSTREAM PRINT FORMATTER *
      *   V2.11  2019-01-08  G.PRZYBYLSKI RECOMPILE ONLY - CABSBILL   *
      *                      OCCURS LIMIT CONFIRMED UNCHANGED AT 40   *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABRAT10.
       AUTHOR. TELCABS APPLICATIONS - RATING TEAM.
      *****************************************************************
      * FOLDS RATED-ELEMENT RECORDS INTO BILL DETAIL GROUPS AND A  *
      * FIXED-LENGTH EXTRACT FOR THE LEGACY REPORTING FEED.        *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RATIN ASSIGN TO UT-S-RATIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT BDTLOUT ASSIGN TO UT-S-BDTLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT BDTLFIX ASSIGN TO UT-S-BDTLFIX
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT CTLOUT ASSIGN TO UT-S-CTLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      *****************************************************************
      * RATIN - ONE RECORD PER RATED ELEMENT, LOCAL LAYOUT.        *
      *****************************************************************
       FD  RATIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       01  CABS-RATED-DETAIL-RECORD.
           05  RD-KEY.
               10  RD-BAN                  PIC X(13).
               10  RD-BILL-PERIOD           PIC 9(06).
               10  RD-SECTION               PIC X(02).
               10  RD-LINE-SEQ              PIC 9(07) COMP-3.
           05  RD-OCN                       PIC X(04).
           05  RD-JURIS-CD                  PIC X(01).
           05  RD-STATE-CD                  PIC X(02).
           05  RD-RATE-ELEM                 PIC X(06).
           05  RD-ELEM-SEQ                  PIC 9(02).
           05  RD-QTY                       PIC S9(13)V9(02) COMP-3.
           05  RD-RATE                      PIC S9(05)V9(05) COMP-3.
           05  RD-AMOUNT                    PIC S9(11)V9(05) COMP-3.
           05  RD-ROUND-RULE                PIC X(01).
           05  RD-SRC-PROCESS               PIC X(08).
           05  RD-CYCLE-YYDDD               PIC 9(05).
           05  RD-FILLER                    PIC X(123).
      *****************************************************************
      * BDTLOUT - BILL DETAIL, VARIABLE LENGTH, UP TO 40 ELEMENTS.    *
      *****************************************************************
       FD  BDTLOUT
           RECORDING MODE IS V
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD IS VARYING IN SIZE FROM 108 TO 1647 CHARACTERS
               DEPENDING ON BD-ELEM-CNT.
       COPY CABSBILL.
      *****************************************************************
      * BDTLFIX - FIXED 500-BYTE EXTRACT FOR THE LEGACY FEED.      *
      *****************************************************************
       FD  BDTLFIX
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 500 CHARACTERS.
       01  CABS-BDTLFIX-RECORD              PIC X(500).
      *****************************************************************
      * CTLOUT - RUN CONTROL / BALANCING RECORD.                      *
      *****************************************************************
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD               PIC X(180).
       WORKING-STORAGE SECTION.
      *****************************************************************
      * STANDARD SHARED WORKING STORAGE.  SEE CABSWRK.                *
      *****************************************************************
       COPY CABSWRK.
      *****************************************************************
      * RATING FAMILY CONTROL BLOCKS - FOUR-DEEP NEST VIA CABSRT01.*
      *****************************************************************
       COPY CABSRT01.
      *****************************************************************
      * PROGRAM CONSTANTS                                              *
      *****************************************************************
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                  PIC X(08) VALUE 'CABRAT10'.
           05  WS-PGM-VERSION               PIC X(05) VALUE 'V2.11'.
           05  WS-MAX-ELEM-CNT              PIC 9(02) VALUE 40.
           05  WS-SUPPRESS-THRESHOLD        PIC S9(03)V9(05) COMP-3
                                                            VALUE 0.
      *****************************************************************
      * SYSIN PARM CARD - POSITIONAL LAYOUT ONLY.                  *
      *****************************************************************
       01  WS-PARM-CARD                     PIC X(80).
       01  WS-PARM-CARD-R1 REDEFINES WS-PARM-CARD.
           05  PC1-CYCLE-YYDDD              PIC 9(05).
           05  PC1-BILL-PERIOD              PIC 9(06).
           05  PC1-RUN-ID                   PIC X(12).
           05  PC1-RESTART-KEY              PIC X(26).
           05  PC1-FILLER                   PIC X(31).
      *****************************************************************
      * CONTROL-BREAK KEY - CURRENT RECORD VERSUS OPEN GROUP KEY.  *
      *****************************************************************
       01  WS-BREAK-KEY-CURR.
           05  WS-BK-BAN                    PIC X(13).
           05  WS-BK-BILL-PERIOD            PIC 9(06).
           05  WS-BK-SECTION                PIC X(02).
           05  WS-BK-LINE-SEQ               PIC 9(07).
       01  WS-BREAK-KEY-SAVE.
           05  WS-BS-BAN                    PIC X(13).
           05  WS-BS-BILL-PERIOD            PIC 9(06).
           05  WS-BS-SECTION                PIC X(02).
           05  WS-BS-LINE-SEQ               PIC 9(07).
       01  WS-BREAK-SWITCHES.
           05  WS-BK-BREAK-SW               PIC X(01) VALUE 'N'.
               88  WS-BK-BREAK               VALUE 'Y'.
           05  WS-GROUP-OPEN-SW             PIC X(01) VALUE 'N'.
               88  WS-GROUP-OPEN             VALUE 'Y'.
      *****************************************************************
      * OPEN GROUP WORK AREA - ACCUMULATES THE CURRENT GROUP.      *
      *****************************************************************
       01  WS-GROUP-WORK.
           05  WS-GW-OCN                    PIC X(04).
           05  WS-GW-JURIS-CD               PIC X(01).
           05  WS-GW-STATE-CD               PIC X(02).
           05  WS-GW-ELEM-CNT               PIC 9(03) VALUE 0.
           05  WS-GW-ACC-MINUTES            PIC S9(13)V9(02) COMP-3
                                                            VALUE 0.
           05  WS-GW-ACC-AMOUNT             PIC S9(13)V9(05) COMP-3
                                                            VALUE 0.
           05  WS-GW-TABLE-FULL-SW          PIC X(01) VALUE 'N'.
               88  WS-GW-TABLE-FULL          VALUE 'Y'.
           05  WS-GW-LAST-ROUND-RULE        PIC X(01).
      *****************************************************************
      * SECTION-CODE / FORMATTER-MODULE TABLE, LOADED AT INIT.     *
      *****************************************************************
       01  WS-SECTION-FMT-TABLE.
           05  WS-SF-CNT                    PIC 9(02) VALUE 8.
           05  WS-SF-ENTRY OCCURS 8 TIMES INDEXED BY WS-SF-X.
               10  WS-SF-SECTION-CD          PIC X(02).
               10  WS-SF-MODULE-NAME         PIC X(08).
      *****************************************************************
      * RATE-ELEMENT-PREFIX TO BILL-SECTION CROSS REFERENCE.          *
      *****************************************************************
       01  WS-ELEM-SECTION-XREF.
           05  WS-ES-CNT                    PIC 9(02) VALUE 8.
           05  WS-ES-ENTRY OCCURS 8 TIMES INDEXED BY WS-ES-X.
               10  WS-ES-ELEM-PREFIX         PIC X(03).
               10  WS-ES-SECTION-CD          PIC X(02).
       01  WS-FMT-CALL-WORK.
           05  WS-FMT-MODULE-NAME           PIC X(08).
           05  WS-FMT-FOUND-SW              PIC X(01) VALUE 'N'.
               88  WS-FMT-FOUND              VALUE 'Y'.
           05  WS-FMT-QTY-IN                PIC S9(13)V9(02) COMP-3.
           05  WS-FMT-DESC-OUT              PIC X(60).
      *****************************************************************
      * DESC FRAGMENTS - SIX PIECES, THREE SEPARATE PARAGRAPHS.    *
      *****************************************************************
       01  WS-DESC-FRAGMENTS.
           05  WS-DF-ELEM-NAME              PIC X(10).
           05  WS-DF-QTY-EDIT               PIC X(12).
           05  WS-DF-RATE-EDIT              PIC X(09).
           05  WS-DF-JURIS-WORD             PIC X(11).
           05  WS-DF-STATE                  PIC X(03).
           05  WS-DF-PERIOD-TEXT            PIC X(09).
       01  WS-DESC-ASSEMBLED                PIC X(60).
       01  WS-DESC-ASSEMBLED-R REDEFINES WS-DESC-ASSEMBLED.
           05  WS-DA-CHAR OCCURS 60 TIMES   PIC X(01).
      *****************************************************************
      * SCAN WORK FOR P5710-P5730. NO REFERENCE MODIFICATION.      *
      *****************************************************************
       01  WS-DESC-SCAN-WORK.
           05  WS-DS-TRUE-LEN               PIC 9(02) VALUE 0.
           05  WS-DS-SCAN-SUB               PIC 9(02) VALUE 0.
           05  WS-DS-TRIM-DONE-SW           PIC X(01) VALUE 'N'.
               88  WS-DS-TRIM-DONE           VALUE 'Y'.
       01  WS-EDIT-FIELDS.
           05  WS-ED-QTY                    PIC ZZZ,ZZZ,ZZ9.99-.
           05  WS-ED-RATE                   PIC Z.ZZZZ9.
           05  WS-ED-AMOUNT                 PIC Z,ZZZ,ZZZ,ZZ9.99-.
           05  WS-ED-MINUTES                PIC Z,ZZZ,ZZZ,ZZ9.99-.
       01  WS-JURIS-WORD-WORK.
           05  WS-JW-WORD                   PIC X(11).
      *****************************************************************
      * BILL PERIOD TEXT - YYDDD TO 'MON YYYY' VIA CABDTCNV.       *
      *****************************************************************
       01  WS-PERIOD-WORK.
           05  WS-PW-GREG-CCYY              PIC 9(04).
           05  WS-PW-GREG-MM                PIC 9(02).
           05  WS-PW-GREG-DD                PIC 9(02).
           05  WS-PW-TEXT                   PIC X(09).
       01  WS-MONTH-NAME-TABLE.
           05  FILLER PIC X(36) VALUE
               'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'.
       01  WS-MONTH-NAME-TABLE-R REDEFINES WS-MONTH-NAME-TABLE.
           05  WS-MN-NAME OCCURS 12 TIMES   PIC X(03).
      *****************************************************************
      * ROUNDING WORK - RULE FROM LAST ELEMENT, NOT A FIXED RULE.  *
      *****************************************************************
       01  WS-ROUND-WORK.
           05  WS-RW-RAW-AMOUNT             PIC S9(13)V9(05) COMP-3
                                                            VALUE 0.
           05  WS-RW-ROUNDED-AMOUNT         PIC S9(13)V9(02) COMP-3
                                                            VALUE 0.
           05  WS-RW-DELTA                  PIC S9(05)V9(05) COMP-3
                                                            VALUE 0.
       01  WS-RTFMT-CALL-WORK.
           05  WS-RTFMT-RATE-IN             PIC S9(13)V9(05) COMP-3.
           05  WS-RTFMT-RULE-IN             PIC X(01).
           05  WS-RTFMT-OUT                 PIC S9(13)V9(02) COMP-3.
      *****************************************************************
      * THE FIXED-LENGTH EXTRACT AREA.  500 BYTES - SEE P5200.        *
      *****************************************************************
       01  WS-FIXED-DETAIL-AREA             PIC X(500).
       01  WS-HASH-CALL-WORK.
           05  WS-HC-MINUTES-IN             PIC S9(15)V9(02) COMP-3.
           05  WS-HC-AMOUNT-IN              PIC S9(13)V9(05) COMP-3.
           05  WS-HC-SEQ-IN                 PIC S9(17)       COMP-3.
       01  WS-DATE-CONV-WORK.
           05  WS-DC-YYDDD-IN               PIC 9(05).
           05  WS-DC-GREG-CCYYMMDD-OUT      PIC 9(08).
       01  WS-DC-GREG-R REDEFINES WS-DC-GREG-CCYYMMDD-OUT.
           05  WS-DC-GREG-CCYY              PIC 9(04).
           05  WS-DC-GREG-MM                PIC 9(02).
           05  WS-DC-GREG-DD                PIC 9(02).
       01  WS-CALL-RC-AREA.
           05  WS-RC-PARMR                  PIC 9(04).
           05  WS-RC-DTCNV                  PIC 9(04).
           05  WS-RC-HASH                   PIC 9(04).
           05  WS-RC-RTFMT                  PIC 9(04).
           05  WS-RC-FMT                    PIC 9(04).
       01  WS-RESTART-WORK.
           05  WS-RESTART-KEY-SAVE          PIC X(26).
           05  WS-RESTART-SKIP-SW           PIC X(01) VALUE 'N'.
               88  WS-RESTART-SKIP           VALUE 'Y'.
       01  WS-VALIDATION-WORK.
           05  WS-VL-BAD-QTY-SW             PIC X(01) VALUE 'N'.
               88  WS-VL-BAD-QTY             VALUE 'Y'.
           05  WS-VL-BAD-AMT-SW             PIC X(01) VALUE 'N'.
               88  WS-VL-BAD-AMT             VALUE 'Y'.
       01  WS-MISC-COUNTERS.
           05  WS-MC-GROUPS-WRITTEN         PIC S9(09) COMP-3 VALUE 0.
           05  WS-MC-ELEMENTS-FOLDED        PIC S9(09) COMP-3 VALUE 0.
           05  WS-MC-FIXED-WRITTEN          PIC S9(09) COMP-3 VALUE 0.
           05  WS-MC-FORMATTER-CALLS        PIC S9(09) COMP-3 VALUE 0.
           05  WS-MC-TABLE-FULL-CNT         PIC S9(09) COMP-3 VALUE 0.
           05  WS-MC-CHECKPOINT-QUOT        PIC S9(09) COMP-3 VALUE 0.
           05  WS-MC-CHECKPOINT-REM         PIC S9(09) COMP-3 VALUE 0.
       01  WS-ABEND-WORK.
           05  WS-AB-PARA                   PIC X(30).
           05  WS-AB-REASON                 PIC X(60).
       PROCEDURE DIVISION.
       P0000-MAINLINE.
           PERFORM P1000-INIT THRU P1000-EXIT.
           PERFORM P2000-PROCESS THRU P2000-EXIT UNTIL WS-EOF.
           PERFORM P8000-CONTROL THRU P8000-EXIT.
           PERFORM P9000-TERM THRU P9000-EXIT.
           STOP RUN.
      *****************************************************************
      * S100-INITIALISATION SECTION                                   *
      *****************************************************************
       S100-INITIALISATION SECTION.
       P1000-INIT.
           PERFORM P1100-OPEN-FILES THRU P1100-EXIT.
           PERFORM P1200-READ-PARM THRU P1200-EXIT.
           PERFORM P1300-LOAD-FMT-TABLE THRU P1300-EXIT.
           PERFORM P1350-LOAD-SECTION-XREF THRU P1350-EXIT.
           PERFORM P1400-INIT-COUNTERS THRU P1400-EXIT.
           PERFORM P2100-READ-RATIN THRU P2100-EXIT.
       P1000-EXIT.
           EXIT.
       P1100-OPEN-FILES.
           OPEN INPUT RATIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RATIN OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT BDTLOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BDTLOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT BDTLFIX.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'BDTLFIX OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CTLOUT OPEN FAILED - FILE STATUS BAD' TO
                   WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
       P1100-EXIT.
           EXIT.
      *****************************************************************
      * P1200-READ-PARM - POSITIONAL SYSIN CARD ONLY.                 *
      *****************************************************************
       P1200-READ-PARM.
           MOVE SPACES TO WS-PARM-CARD.
           ACCEPT WS-PARM-CARD FROM SYSIN.
           CALL 'CABPARMR' USING WS-PARM-CARD WS-RC-PARMR.
           MOVE PC1-CYCLE-YYDDD TO R1-CYCLE-YYDDD.
           MOVE PC1-BILL-PERIOD TO R1-BILL-PERIOD.
           MOVE PC1-RUN-ID TO R1-RUN-ID.
           MOVE PC1-RESTART-KEY TO WS-RESTART-KEY-SAVE.
           IF WS-RESTART-KEY-SAVE NOT = SPACES
               MOVE 'Y' TO WS-RESTART-SW.
       P1200-EXIT.
           EXIT.
      * P1300-LOAD-FMT-TABLE - SECTION TO FORMATTER MODULE TABLE.
       P1300-LOAD-FMT-TABLE.
           MOVE 'VC' TO WS-SF-SECTION-CD (1).
           MOVE 'CABFMTVC' TO WS-SF-MODULE-NAME (1).
           MOVE 'DT' TO WS-SF-SECTION-CD (2).
           MOVE 'CABFMTDT' TO WS-SF-MODULE-NAME (2).
           MOVE 'SP' TO WS-SF-SECTION-CD (3).
           MOVE 'CABFMTSP' TO WS-SF-MODULE-NAME (3).
           MOVE 'RC' TO WS-SF-SECTION-CD (4).
           MOVE 'CABFMTRC' TO WS-SF-MODULE-NAME (4).
           MOVE 'UN' TO WS-SF-SECTION-CD (5).
           MOVE 'CABFMTUN' TO WS-SF-MODULE-NAME (5).
           MOVE 'OS' TO WS-SF-SECTION-CD (6).
           MOVE 'CABFMTOS' TO WS-SF-MODULE-NAME (6).
           MOVE 'TS' TO WS-SF-SECTION-CD (7).
           MOVE 'CABFMTTS' TO WS-SF-MODULE-NAME (7).
           MOVE 'MP' TO WS-SF-SECTION-CD (8).
           MOVE 'CABFMTMP' TO WS-SF-MODULE-NAME (8).
       P1300-EXIT.
           EXIT.
      * P1350-LOAD-SECTION-XREF - ELEM PREFIX TO SECTION CODE TABLE.
       P1350-LOAD-SECTION-XREF.
           MOVE 'ORI' TO WS-ES-ELEM-PREFIX (1).
           MOVE 'VC' TO WS-ES-SECTION-CD (1).
           MOVE 'TER' TO WS-ES-ELEM-PREFIX (2).
           MOVE 'VC' TO WS-ES-SECTION-CD (2).
           MOVE 'LTR' TO WS-ES-ELEM-PREFIX (3).
           MOVE 'VC' TO WS-ES-SECTION-CD (3).
           MOVE 'TAN' TO WS-ES-ELEM-PREFIX (4).
           MOVE 'TS' TO WS-ES-SECTION-CD (4).
           MOVE 'CCL' TO WS-ES-ELEM-PREFIX (5).
           MOVE 'VC' TO WS-ES-SECTION-CD (5).
           MOVE 'DAT' TO WS-ES-ELEM-PREFIX (6).
           MOVE 'DT' TO WS-ES-SECTION-CD (6).
           MOVE 'SPC' TO WS-ES-ELEM-PREFIX (7).
           MOVE 'SP' TO WS-ES-SECTION-CD (7).
           MOVE 'UNE' TO WS-ES-ELEM-PREFIX (8).
           MOVE 'UN' TO WS-ES-SECTION-CD (8).
       P1350-EXIT.
           EXIT.
       P1400-INIT-COUNTERS.
           MOVE 0 TO WS-READ-CNT.
           MOVE 0 TO WS-WRITE-CNT.
           MOVE 0 TO WS-REJECT-CNT.
           MOVE 0 TO WS-SUMM-CNT.
           MOVE 0 TO WS-CFWD-CNT.
           MOVE 0 TO WS-ACC-MINUTES.
           MOVE 0 TO WS-ACC-AMOUNT.
           MOVE 0 TO WS-ACC-SEQ-HASH.
           MOVE 0 TO WS-ACC-OCN-HASH.
           MOVE 0 TO WS-MC-GROUPS-WRITTEN.
           MOVE 0 TO WS-MC-ELEMENTS-FOLDED.
           MOVE 0 TO WS-MC-FIXED-WRITTEN.
           MOVE 0 TO WS-MC-FORMATTER-CALLS.
           MOVE 0 TO WS-MC-TABLE-FULL-CNT.
       P1400-EXIT.
           EXIT.
       P9900-FATAL-OPEN.
           MOVE 'B037' TO CT-ABEND-CD.
           CALL 'CABABEND' USING WS-AB-PARA WS-AB-REASON
               CT-ABEND-CD.
       P9900-EXIT.
           EXIT.
      * S200-MAIN-PROCESS SECTION - READ-AHEAD CONTROL BREAK LOGIC.
       S200-MAIN-PROCESS SECTION.
       P2000-PROCESS.
           IF WS-GROUP-OPEN
               PERFORM P2110-CHECK-GROUP-BREAK THRU P2110-EXIT
           ELSE
               PERFORM P2200-START-NEW-GROUP THRU P2200-EXIT.
           IF WS-BK-BREAK
               PERFORM P5000-FINISH-GROUP THRU P5000-EXIT
               PERFORM P2200-START-NEW-GROUP THRU P2200-EXIT.
           PERFORM P2300-ADD-ELEMENT THRU P2300-EXIT.
           PERFORM P2100-READ-RATIN THRU P2100-EXIT.
           IF WS-EOF
               PERFORM P5000-FINISH-GROUP THRU P5000-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-RATIN.
           READ RATIN
               AT END MOVE 'Y' TO WS-EOF-SW.
           IF NOT WS-EOF
               ADD 1 TO WS-READ-CNT
               PERFORM P2105-CHECKPOINT-DISPLAY THRU P2105-EXIT.
       P2100-EXIT.
           EXIT.
       P2105-CHECKPOINT-DISPLAY.
           DIVIDE WS-READ-CNT BY 25000 GIVING WS-MC-CHECKPOINT-QUOT
               REMAINDER WS-MC-CHECKPOINT-REM.
           IF WS-MC-CHECKPOINT-REM = 0
               DISPLAY 'CABRAT10 - ' WS-READ-CNT ' RECORDS READ'.
       P2105-EXIT.
           EXIT.
       P2110-CHECK-GROUP-BREAK.
           MOVE RD-BAN TO WS-BK-BAN.
           MOVE RD-BILL-PERIOD TO WS-BK-BILL-PERIOD.
           MOVE RD-SECTION TO WS-BK-SECTION.
           MOVE RD-LINE-SEQ TO WS-BK-LINE-SEQ.
           MOVE 'N' TO WS-BK-BREAK-SW.
           IF WS-BK-BAN NOT = WS-BS-BAN OR
                   WS-BK-BILL-PERIOD NOT = WS-BS-BILL-PERIOD OR
                   WS-BK-SECTION NOT = WS-BS-SECTION OR
                   WS-BK-LINE-SEQ NOT = WS-BS-LINE-SEQ
               MOVE 'Y' TO WS-BK-BREAK-SW.
       P2110-EXIT.
           EXIT.
      *****************************************************************
      * P2200-START-NEW-GROUP - OPENS A NEW BILL DETAIL GROUP.     *
      *****************************************************************
       P2200-START-NEW-GROUP.
           MOVE RD-BAN TO BD-BAN.
           MOVE RD-BILL-PERIOD TO BD-BILL-PERIOD.
           MOVE RD-LINE-SEQ TO BD-LINE-SEQ.
           MOVE RD-OCN TO WS-GW-OCN.
           MOVE RD-JURIS-CD TO WS-GW-JURIS-CD.
           MOVE RD-STATE-CD TO WS-GW-STATE-CD.
           MOVE RD-BAN TO WS-BS-BAN.
           MOVE RD-BILL-PERIOD TO WS-BS-BILL-PERIOD.
           MOVE RD-SECTION TO WS-BS-SECTION.
           MOVE RD-LINE-SEQ TO WS-BS-LINE-SEQ.
           MOVE 0 TO WS-GW-ELEM-CNT.
           MOVE 0 TO WS-GW-ACC-MINUTES.
           MOVE 0 TO WS-GW-ACC-AMOUNT.
           MOVE 'N' TO WS-GW-TABLE-FULL-SW.
           MOVE 'Y' TO WS-GROUP-OPEN-SW.
           PERFORM P6100-ASSIGN-SECTION-CODE THRU P6100-EXIT.
       P2200-EXIT.
           EXIT.
      * P2300-ADD-ELEMENT - FOLDS RD RECORD INTO NEXT BD-ELEMENT.
      * TABLE IS 1 TO 40 - BEYOND THAT, COUNTED BUT NOT ADDED.
       P2300-ADD-ELEMENT.
           ADD RD-QTY TO WS-GW-ACC-MINUTES.
           ADD RD-AMOUNT TO WS-GW-ACC-AMOUNT.
           MOVE RD-ROUND-RULE TO WS-GW-LAST-ROUND-RULE.
           IF WS-GW-ELEM-CNT < WS-MAX-ELEM-CNT
               ADD 1 TO WS-GW-ELEM-CNT
               MOVE RD-RATE-ELEM TO BD-EL-RATE-ELEM (WS-GW-ELEM-CNT)
               MOVE RD-QTY TO BD-EL-QTY (WS-GW-ELEM-CNT)
               MOVE RD-RATE TO BD-EL-RATE (WS-GW-ELEM-CNT)
               MOVE RD-AMOUNT TO BD-EL-AMOUNT (WS-GW-ELEM-CNT)
               MOVE RD-ROUND-RULE TO
                   BD-EL-ROUND-RULE (WS-GW-ELEM-CNT)
               MOVE RD-SRC-PROCESS TO
                   BD-EL-SRC-PROCESS (WS-GW-ELEM-CNT)
               PERFORM P5010-DESC-FRAG-ELEM THRU P5010-EXIT
               ADD 1 TO WS-MC-ELEMENTS-FOLDED
           ELSE
               MOVE 'Y' TO WS-GW-TABLE-FULL-SW
               ADD 1 TO WS-MC-TABLE-FULL-CNT.
       P2300-EXIT.
           EXIT.
      * S300-DESCRIPTION-BUILD SECTION - SIX FRAGMENTS, SET IN
      * THREE SEPARATE PARAGRAPHS (P5010/P5300/P5600), ASSEMBLED,
      * SQUEEZED, TALLIED AND RIGHT-TRIMMED IN P5700-P5730 BELOW.
       S300-DESCRIPTION-BUILD SECTION.
      *****************************************************************
      * P5010-DESC-FRAG-ELEM - FIRST FRAGMENT, SET AT FOLD TIME.   *
      *****************************************************************
       P5010-DESC-FRAG-ELEM.
           MOVE RD-RATE-ELEM TO WS-DF-ELEM-NAME.
       P5010-EXIT.
           EXIT.
       P5000-FINISH-GROUP.
           MOVE WS-GW-ELEM-CNT TO BD-ELEM-CNT.
           MOVE WS-GW-OCN TO BD-OCN.
           MOVE WS-GW-JURIS-CD TO BD-JURIS-CD.
           MOVE WS-GW-STATE-CD TO BD-STATE-CD.
           PERFORM P5300-DESC-FRAG-QTY-RATE THRU P5300-EXIT.
           PERFORM P5600-DESC-FRAG-JURIS THRU P5600-EXIT.
           PERFORM P5700-ASSEMBLE-DESCRIPTION THRU P5700-EXIT.
           PERFORM P6000-COMPUTE-TOTALS THRU P6000-EXIT.
           PERFORM P4500-CALL-FORMATTER THRU P4500-EXIT.
           PERFORM P5100-WRITE-BILL-DETAIL THRU P5100-EXIT.
           PERFORM P5200-MOVE-TO-FIXED THRU P5200-EXIT.
           PERFORM P5210-WRITE-FIXED-EXTRACT THRU P5210-EXIT.
           MOVE 'N' TO WS-GROUP-OPEN-SW.
           ADD 1 TO WS-MC-GROUPS-WRITTEN.
       P5000-EXIT.
           EXIT.
      * P5300-DESC-FRAG-QTY-RATE - SECOND/THIRD FRAGMENTS: QTY/RATE.
       P5300-DESC-FRAG-QTY-RATE.
           MOVE RD-QTY TO WS-ED-QTY.
           MOVE WS-ED-QTY TO WS-DF-QTY-EDIT.
           MOVE RD-RATE TO WS-ED-RATE.
           MOVE WS-ED-RATE TO WS-DF-RATE-EDIT.
       P5300-EXIT.
           EXIT.
      *****************************************************************
      * P5600-DESC-FRAG-JURIS - FRAGMENTS 4/5/6: JURIS/STATE/PRD.  *
      *****************************************************************
       P5600-DESC-FRAG-JURIS.
           PERFORM P5610-SET-JURIS-WORD THRU P5610-EXIT.
           MOVE WS-JW-WORD TO WS-DF-JURIS-WORD.
           MOVE WS-GW-STATE-CD TO WS-DF-STATE.
           PERFORM P5620-BUILD-PERIOD-TEXT THRU P5620-EXIT.
           MOVE WS-PW-TEXT TO WS-DF-PERIOD-TEXT.
       P5600-EXIT.
           EXIT.
       P5610-SET-JURIS-WORD.
           MOVE 'INTERSTATE' TO WS-JW-WORD.
           IF WS-GW-JURIS-CD = 'S'
               MOVE 'INTRASTATE' TO WS-JW-WORD.
           IF WS-GW-JURIS-CD = 'L'
               MOVE 'LOCAL' TO WS-JW-WORD.
           IF WS-GW-JURIS-CD = 'X' OR WS-GW-JURIS-CD = ' '
               MOVE 'INDETERMINATE' TO WS-JW-WORD.
       P5610-EXIT.
           EXIT.
      *****************************************************************
      * P5620-BUILD-PERIOD-TEXT - YYDDD TO GREGORIAN VIA CABDTCNV. *
      *****************************************************************
       P5620-BUILD-PERIOD-TEXT.
           MOVE R1-CYCLE-YYDDD TO WS-DC-YYDDD-IN.
           CALL 'CABDTCNV' USING WS-DC-YYDDD-IN
               WS-DC-GREG-CCYYMMDD-OUT WS-RC-DTCNV.
           MOVE WS-DC-GREG-CCYY TO WS-PW-GREG-CCYY.
           MOVE WS-DC-GREG-MM TO WS-PW-GREG-MM.
           IF WS-PW-GREG-MM < 1 OR WS-PW-GREG-MM > 12
               MOVE 1 TO WS-PW-GREG-MM.
           STRING WS-MN-NAME (WS-PW-GREG-MM) DELIMITED BY SIZE
                  ' '                        DELIMITED BY SIZE
                  WS-PW-GREG-CCYY             DELIMITED BY SIZE
               INTO WS-PW-TEXT.
       P5620-EXIT.
           EXIT.
      *****************************************************************
      * P5700-ASSEMBLE-DESCRIPTION - STRINGS THE SIX FRAGMENTS.    *
      *****************************************************************
       P5700-ASSEMBLE-DESCRIPTION.
           MOVE SPACES TO WS-DESC-ASSEMBLED.
           STRING WS-DF-ELEM-NAME    DELIMITED BY SIZE
                  ' '                DELIMITED BY SIZE
                  WS-DF-QTY-EDIT     DELIMITED BY SIZE
                  WS-DF-RATE-EDIT    DELIMITED BY SIZE
                  ' '                DELIMITED BY SIZE
                  WS-DF-JURIS-WORD   DELIMITED BY SIZE
                  WS-DF-STATE        DELIMITED BY SIZE
                  WS-DF-PERIOD-TEXT  DELIMITED BY SIZE
               INTO WS-DESC-ASSEMBLED.
           PERFORM P5710-SQUEEZE-SPACES THRU P5710-EXIT.
           PERFORM P5720-TALLY-LENGTH THRU P5720-EXIT.
           PERFORM P5730-RIGHT-TRIM THRU P5730-EXIT.
           MOVE WS-DESC-ASSEMBLED TO BD-DESCRIPTION.
       P5700-EXIT.
           EXIT.
      * P5710-SQUEEZE-SPACES - COLLAPSES DOUBLE BLANKS LEFT BY THE
      * FIXED-WIDTH FRAGMENTS. REPEATED TO COLLAPSE LONGER RUNS.
       P5710-SQUEEZE-SPACES.
           INSPECT WS-DESC-ASSEMBLED REPLACING ALL '  ' BY ' '.
           INSPECT WS-DESC-ASSEMBLED REPLACING ALL '  ' BY ' '.
           INSPECT WS-DESC-ASSEMBLED REPLACING ALL '  ' BY ' '.
       P5710-EXIT.
           EXIT.
      * P5720-TALLY-LENGTH - TRUE LENGTH BEFORE TRAILING BLANKS.
       P5720-TALLY-LENGTH.
           MOVE 0 TO WS-DS-TRUE-LEN.
           INSPECT WS-DESC-ASSEMBLED TALLYING WS-DS-TRUE-LEN
               FOR CHARACTERS BEFORE INITIAL '  '.
           IF WS-DS-TRUE-LEN = 0
               MOVE 60 TO WS-DS-TRUE-LEN.
       P5720-EXIT.
           EXIT.
      * P5730-RIGHT-TRIM - WALKS BACKWARDS VIA THE OCCURS 60
      * REDEFINE UNTIL A NON-BLANK IS FOUND. NO REFERENCE MOD.
       P5730-RIGHT-TRIM.
           MOVE 60 TO WS-DS-SCAN-SUB.
           MOVE 'N' TO WS-DS-TRIM-DONE-SW.
           PERFORM P5735-TRIM-ONE-CHAR THRU P5735-EXIT
               UNTIL WS-DS-TRIM-DONE OR WS-DS-SCAN-SUB < 1.
       P5730-EXIT.
           EXIT.
       P5735-TRIM-ONE-CHAR.
           IF WS-DA-CHAR (WS-DS-SCAN-SUB) NOT = ' '
               MOVE 'Y' TO WS-DS-TRIM-DONE-SW
           ELSE
               SUBTRACT 1 FROM WS-DS-SCAN-SUB.
       P5735-EXIT.
           EXIT.
      * S400-FORMATTER-DISPATCH SECTION - THE CALL TARGET COMES OFF
      * WS-SECTION-FMT-TABLE AT RUN TIME, NOT A LITERAL BELOW - THE
      * ACTUAL PROGRAM CANNOT BE RESOLVED BY READING P4500 ALONE.
       S400-FORMATTER-DISPATCH SECTION.
       P4500-CALL-FORMATTER.
           PERFORM P4510-SEARCH-FORMAT-TABLE THRU P4510-EXIT.
           IF WS-FMT-FOUND
               MOVE WS-GW-ACC-MINUTES TO WS-FMT-QTY-IN
               MOVE SPACES TO WS-FMT-DESC-OUT
               CALL WS-FMT-MODULE-NAME USING CABS-BILL-DETAIL
                   WS-FMT-QTY-IN WS-FMT-DESC-OUT WS-RC-FMT
               ADD 1 TO WS-MC-FORMATTER-CALLS.
       P4500-EXIT.
           EXIT.
       P4510-SEARCH-FORMAT-TABLE.
           MOVE 'N' TO WS-FMT-FOUND-SW.
           MOVE SPACES TO WS-FMT-MODULE-NAME.
           SET WS-SF-X TO 1.
           PERFORM P4520-CHECK-ONE-ENTRY THRU P4520-EXIT
               VARYING WS-SF-X FROM 1 BY 1
               UNTIL WS-SF-X > WS-SF-CNT OR WS-FMT-FOUND.
       P4510-EXIT.
           EXIT.
       P4520-CHECK-ONE-ENTRY.
           IF WS-SF-SECTION-CD (WS-SF-X) = BD-SECTION
               MOVE WS-SF-MODULE-NAME (WS-SF-X) TO WS-FMT-MODULE-NAME
               MOVE 'Y' TO WS-FMT-FOUND-SW.
       P4520-EXIT.
           EXIT.
      * S500-TOTALS SECTION - ROUNDING FOLLOWS THE LAST ELEMENT'S
      * ROUND RULE, NOT A FIXED CONVENTION. CABRTFMT APPLIES IT.
       S500-TOTALS SECTION.
       P6000-COMPUTE-TOTALS.
           MOVE WS-GW-ACC-MINUTES TO BD-TOT-MINUTES.
           MOVE WS-GW-ACC-AMOUNT TO BD-TOT-AMOUNT.
           MOVE WS-GW-ACC-AMOUNT TO WS-RTFMT-RATE-IN.
           MOVE WS-GW-LAST-ROUND-RULE TO WS-RTFMT-RULE-IN.
           CALL 'CABRTFMT' USING WS-RTFMT-RATE-IN WS-RTFMT-RULE-IN
               WS-RTFMT-OUT WS-RC-RTFMT.
           MOVE WS-RTFMT-OUT TO BD-TOT-ROUNDED.
           COMPUTE BD-ROUND-DELTA =
               BD-TOT-AMOUNT - BD-TOT-ROUNDED.
           MOVE BD-TOT-MINUTES TO WS-HC-MINUTES-IN.
           MOVE BD-TOT-AMOUNT TO WS-HC-AMOUNT-IN.
           MOVE BD-LINE-SEQ TO WS-HC-SEQ-IN.
           CALL 'CABHASH' USING WS-HC-MINUTES-IN WS-HC-AMOUNT-IN
               WS-HC-SEQ-IN WS-RC-HASH.
           ADD WS-HC-MINUTES-IN TO WS-ACC-MINUTES.
           ADD WS-HC-AMOUNT-IN TO WS-ACC-AMOUNT.
           ADD WS-HC-SEQ-IN TO WS-ACC-SEQ-HASH.
       P6000-EXIT.
           EXIT.
      * P6100-ASSIGN-SECTION-CODE - XREF LOOKUP, DEFAULTS TO 'VC'.
       P6100-ASSIGN-SECTION-CODE.
           MOVE 'VC' TO BD-SECTION.
           MOVE RD-RATE-ELEM TO WS-DF-ELEM-NAME.
           SET WS-ES-X TO 1.
           PERFORM P6110-CHECK-ONE-PREFIX THRU P6110-EXIT
               VARYING WS-ES-X FROM 1 BY 1
               UNTIL WS-ES-X > WS-ES-CNT.
       P6100-EXIT.
           EXIT.
       P6110-CHECK-ONE-PREFIX.
           IF RD-RATE-ELEM = WS-ES-ELEM-PREFIX (WS-ES-X)
               MOVE WS-ES-SECTION-CD (WS-ES-X) TO BD-SECTION.
       P6110-EXIT.
           EXIT.
      *****************************************************************
      * S600-WRITE SECTION - WRITES BDTLOUT AND THE BDTLFIX EXTRACT*
      *****************************************************************
       S600-WRITE SECTION.
       P5100-WRITE-BILL-DETAIL.
           WRITE CABS-BILL-DETAIL.
           ADD 1 TO WS-WRITE-CNT.
       P5100-EXIT.
           EXIT.
      *****************************************************************
      * P5200-MOVE-TO-FIXED - BUILDS THE BDTLFIX EXTRACT AREA.     *
      *****************************************************************
       P5200-MOVE-TO-FIXED.
           MOVE SPACES TO WS-FIXED-DETAIL-AREA.
           MOVE CABS-BILL-DETAIL TO WS-FIXED-DETAIL-AREA.
       P5200-EXIT.
           EXIT.
       P5210-WRITE-FIXED-EXTRACT.
           MOVE WS-FIXED-DETAIL-AREA TO CABS-BDTLFIX-RECORD.
           WRITE CABS-BDTLFIX-RECORD.
           ADD 1 TO WS-MC-FIXED-WRITTEN.
       P5210-EXIT.
           EXIT.
      * S800-CONTROL-BALANCE SECTION - THE MANDATORY CONTROL STEP.
       S800-CONTROL-BALANCE SECTION.
       P8000-CONTROL.
           PERFORM P8100-BUILD-CONTROL-REC THRU P8100-EXIT.
           PERFORM P8200-CHECK-BALANCE THRU P8200-EXIT.
           PERFORM P8300-WRITE-CONTROL-REC THRU P8300-EXIT.
       P8000-EXIT.
           EXIT.
       P8100-BUILD-CONTROL-REC.
           MOVE R1-RUN-ID TO CT-RUN-ID.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE 1 TO CT-STEP-SEQ.
           MOVE R1-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE R1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE SPACES TO CT-JOBNAME.
           MOVE SPACES TO CT-STEPNAME.
      * CT-WRITTEN IS GROUPS, NOT INPUT RECORDS; CT-SUMMARISED
      * CARRIES THE DIFFERENCE (RECORDS FOLDED INTO AN OPEN GROUP).
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-MC-GROUPS-WRITTEN TO CT-WRITTEN.
           MOVE 0 TO CT-REJECTED.
           COMPUTE CT-SUMMARISED =
               WS-READ-CNT - WS-MC-GROUPS-WRITTEN.
           MOVE 0 TO CT-CARRIED-FWD.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE WS-RESTART-KEY-SAVE TO CT-RESTART-KEY.
           MOVE SPACES TO CT-FILLER.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-ABEND-CD.
       P8100-EXIT.
           EXIT.
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
      *****************************************************************
      * S900-TERMINATION SECTION.                                     *
      *****************************************************************
       S900-TERMINATION SECTION.
       P9000-TERM.
           CLOSE RATIN.
           CLOSE BDTLOUT.
           CLOSE BDTLFIX.
           CLOSE CTLOUT.
           DISPLAY 'CABRAT10 - RUN COMPLETE'.
           DISPLAY '  READ        = ' WS-READ-CNT.
           DISPLAY '  GROUPS OUT  = ' WS-MC-GROUPS-WRITTEN.
           DISPLAY '  ELEMS FOLDED= ' WS-MC-ELEMENTS-FOLDED.
           DISPLAY '  FIXED WRITE = ' WS-MC-FIXED-WRITTEN.
           DISPLAY '  FMT CALLS   = ' WS-MC-FORMATTER-CALLS.
           DISPLAY '  TABLE FULL  = ' WS-MC-TABLE-FULL-CNT.
       P9000-EXIT.
           EXIT.
