       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABCTL06.
      *****************************************************************
      * CABCTL06 - SETTLEMENT DISPUTE ENQUIRY BY COUNTERPARTY AND     *
      *            PERIOD, THROUGH THE SECONDARY INDEX                *
      * APPLICATION : SETL                                            *
      * COMPILER    : ENTERPRISE COBOL                                *
      * INPUTS      : IMS PCB 1  CABSETDB  PSB CABCT06P  PROCOPT G    *
      *               PROCSEQ=CABSETSX - THE DATABASE IS PRESENTED    *
      *               IN SECONDARY INDEX SEQUENCE, NOT IN ROOT KEY    *
      *               SEQUENCE                                        *
      *               REQIN   TELCABS.SETL.DISPUTE.REQ(0)     FB 080  *
      *               PARMIN  INSTREAM SYSIN PARM CARD        FB 080  *
      * OUTPUTS     : DSPOUT  TELCABS.SETL.DISPUTE.RPT(+1)    FB 200  *
      *               RPTOUT  SYSOUT PRINT                    FBA 133 *
      * CONTROL     : CTLOUT                                  CABSCTL *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED               *
      *                       + CT-SUMMARISED + CT-CARRIED-FWD        *
      * RESTART     : FULL RERUN - READ ONLY                          *
      *                                                               *
      * READING THROUGH THE SECONDARY INDEX MEANS THE ROOT SEGMENT    *
      * IS RETURNED IN COUNTERPARTY AND PERIOD ORDER.  THE KEY        *
      * FEEDBACK AREA THEREFORE CONTAINS THE INDEX KEY, NOT THE ROOT  *
      * KEY.  ANY CODE THAT NEEDS THE ROOT KEY MUST TAKE IT OUT OF    *
      * THE SEGMENT ITSELF - SEE CABS-STD-047.                        *
      *                                                               *
      * REVISION HISTORY                                              *
      *   V1.00  1995-03-08  J.M.CASTILLO  INITIAL                    *
      *   V1.02  1999-07-19  D.OKONKWO     DISPUTE SEGMENT ADDED      *
      *   V1.05  2004-12-06  P.NAIR        AGEING BANDS ON THE REPORT *
      *   V2.00  2011-06-14  A.BUKOWSKI    RUNS AS A LINKED           *
      *                                    SUBROUTINE FROM THE ONLINE *
      *                                    ENQUIRY AS WELL AS BATCH   *
      *   V2.02  2018-09-04  M.HAAS        RECOMPILE ONLY - IMS V14   *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PARM-FILE     ASSIGN TO PARMIN
                  FILE STATUS IS WS-FS-INPUT.
           SELECT REQUEST-FILE  ASSIGN TO REQIN
                  FILE STATUS IS WS-FS-INPUT.
           SELECT DISPUTE-OUT   ASSIGN TO DSPOUT
                  FILE STATUS IS WS-FS-OUTPUT.
           SELECT CONTROL-FILE  ASSIGN TO CTLOUT
                  FILE STATUS IS WS-FS-CONTROL.
           SELECT REPORT-FILE   ASSIGN TO RPTOUT.
       DATA DIVISION.
       FILE SECTION.
       FD  PARM-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE OMITTED.
       01  PARM-CARD                   PIC X(80).
       FD  REQUEST-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  REQUEST-CARD                PIC X(80).
       FD  DISPUTE-OUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  DISPUTE-OUT-RECORD          PIC X(200).
       FD  CONTROL-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  CONTROL-RECORD              PIC X(180).
       FD  REPORT-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE OMITTED.
       01  REPORT-LINE                 PIC X(133).
       WORKING-STORAGE SECTION.
       01  WS-PGM-NAME                 PIC X(08) VALUE 'CABCTL06'.
       01  WS-PARA-NAME                PIC X(30) VALUE SPACES.
       COPY CABSWRK.
       COPY CABSPRNT.
      *
       01  WS-DLI-FUNCTIONS.
           05  DLI-GU                  PIC X(04) VALUE 'GU  '.
           05  DLI-GN                  PIC X(04) VALUE 'GN  '.
           05  DLI-GNP                 PIC X(04) VALUE 'GNP '.
      *
      * QUALIFIED ON THE SECONDARY INDEX SEARCH FIELD.  UNDER
      * PROCSEQ THE ROOT IS ADDRESSED BY THE XDFLD NAME, NOT BY THE
      * SEQUENCE FIELD.
      *
       01  SSA-SETLSEG-X.
           05  FILLER                  PIC X(08) VALUE 'SETLSEG '.
           05  FILLER                  PIC X(01) VALUE '('.
           05  FILLER                  PIC X(08) VALUE 'SETLXOCN'.
           05  FILLER                  PIC X(02) VALUE ' ='.
           05  SSA-SX-KEY.
               10  SSA-SX-OCN          PIC X(04) VALUE SPACES.
               10  SSA-SX-PERIOD       PIC 9(06) VALUE ZERO.
           05  FILLER                  PIC X(01) VALUE ')'.
       01  SSA-SETLSEG-U.
           05  FILLER                  PIC X(08) VALUE 'SETLSEG '.
           05  FILLER                  PIC X(01) VALUE SPACE.
       01  SSA-SETLDTL-U.
           05  FILLER                  PIC X(08) VALUE 'SETLDTL '.
           05  FILLER                  PIC X(01) VALUE SPACE.
       01  SSA-SETLDISP-Q.
           05  FILLER                  PIC X(08) VALUE 'SETLDISP'.
           05  FILLER                  PIC X(01) VALUE '('.
           05  FILLER                  PIC X(08) VALUE 'SPSTATUS'.
           05  FILLER                  PIC X(02) VALUE ' ='.
           05  SSA-SP-STATUS           PIC X(01) VALUE 'O'.
           05  FILLER                  PIC X(01) VALUE ')'.
       01  SSA-SETLDISP-U.
           05  FILLER                  PIC X(08) VALUE 'SETLDISP'.
           05  FILLER                  PIC X(01) VALUE SPACE.
      *
       01  WS-SETLSEG-IO.
           05  SS-KEY.
               10  SS-SETTLE-TYPE      PIC X(01).
               10  SS-COUNTERPARTY     PIC X(04).
               10  SS-SETTLE-PERIOD    PIC 9(06).
               10  SS-SEQ              PIC 9(09).
           05  SS-OCN-PERIOD           PIC X(10).
           05  SS-TOTAL-MOU            PIC S9(15)V9(02) COMP-3.
           05  SS-BILLABLE-MOU         PIC S9(15)V9(02) COMP-3.
           05  SS-CAPPED-MOU           PIC S9(15)V9(02) COMP-3.
           05  SS-RATE-APPLIED         PIC S9(05)V9(05) COMP-3.
           05  SS-GROSS-AMT            PIC S9(13)V9(05) COMP-3.
           05  SS-OUR-SHARE            PIC S9(13)V9(05) COMP-3.
           05  SS-THEIR-SHARE          PIC S9(13)V9(05) COMP-3.
           05  SS-NET-DUE              PIC S9(13)V9(02) COMP-3.
           05  SS-ROUND-RESIDUE        PIC S9(05)V9(05) COMP-3.
           05  SS-DIRECTION            PIC X(01).
               88  SS-RECEIVABLE       VALUE 'R'.
               88  SS-PAYABLE          VALUE 'P'.
           05  SS-DISPUTE-SW           PIC X(01).
               88  SS-IN-DISPUTE       VALUE 'Y'.
           05  SS-EXCH-YYDDD           PIC 9(05).
           05  SS-RAO-CODE             PIC X(03).
           05  SS-POST-YYDDD           PIC 9(05).
           05  SS-POST-PGM             PIC X(08).
           05  SS-FILLER               PIC X(105).
       01  WS-SETLDTL-IO.
           05  SD-SEQ                  PIC 9(09).
           05  SD-TRUNK-GRP            PIC X(08).
           05  SD-CIRCUIT-ID           PIC X(20).
           05  SD-OUR-PCT              PIC S9(03)V9(05) COMP-3.
           05  SD-THEIR-PCT            PIC S9(03)V9(05) COMP-3.
           05  SD-PCT-VARIANCE         PIC S9(03)V9(05) COMP-3.
           05  SD-MOU                  PIC S9(15)V9(02) COMP-3.
           05  SD-AMOUNT               PIC S9(13)V9(05) COMP-3.
           05  SD-FILLER               PIC X(90).
       01  WS-SETLDISP-IO.
           05  SP-DISPUTE-ID           PIC X(08).
           05  SP-STATUS               PIC X(01).
               88  SP-OPEN             VALUE 'O'.
               88  SP-RESOLVED         VALUE 'R'.
               88  SP-WITHDRAWN        VALUE 'W'.
           05  SP-RAISED-YYDDD         PIC 9(05).
           05  SP-RESOLVED-YYDDD       PIC 9(05).
           05  SP-DISPUTED-AMT         PIC S9(13)V9(02) COMP-3.
           05  SP-ALLOWED-AMT          PIC S9(13)V9(02) COMP-3.
           05  SP-REASON-CD            PIC X(04).
           05  SP-RAISED-BY            PIC X(08).
           05  SP-NARRATIVE            PIC X(60).
           05  SP-FILLER               PIC X(41).
      *
       01  WS-REQUEST-AREA.
           05  WR-OCN                  PIC X(04).
           05  WR-PERIOD-FROM          PIC 9(06).
           05  WR-PERIOD-THRU          PIC 9(06).
           05  WR-STATUS-WANTED        PIC X(01).
           05  WR-DETAIL-SW            PIC X(01).
           05  WR-FILLER               PIC X(62).
      *
       01  WS-OUT-AREA.
           05  WO-OCN                  PIC X(04).
           05  WO-SETTLE-PERIOD        PIC 9(06).
           05  WO-SETTLE-TYPE          PIC X(01).
           05  WO-SEQ                  PIC 9(09).
           05  WO-DISPUTE-ID           PIC X(08).
           05  WO-STATUS               PIC X(01).
           05  WO-RAISED-YYDDD         PIC 9(05).
           05  WO-AGE-DAYS             PIC S9(05) COMP-3.
           05  WO-AGE-BAND             PIC X(02).
           05  WO-NET-DUE              PIC S9(13)V9(02) COMP-3.
           05  WO-DISPUTED-AMT         PIC S9(13)V9(02) COMP-3.
           05  WO-ALLOWED-AMT          PIC S9(13)V9(02) COMP-3.
           05  WO-REASON-CD            PIC X(04).
           05  WO-CIRCUIT-ID           PIC X(20).
           05  WO-NARRATIVE            PIC X(60).
           05  WO-FILLER               PIC X(58).
      *
       01  WS-PARM-AREA.
           05  WP-RUN-ID               PIC X(12).
           05  WP-CYCLE-YYDDD          PIC 9(05).
           05  WP-BILL-PERIOD          PIC 9(06).
           05  WP-FILLER               PIC X(57).
      *
       01  WS-SWITCHES-LOCAL.
           05  WS-ROOT-EOF-SW          PIC X(01) VALUE 'N'.
               88  WS-ROOT-EOF         VALUE 'Y'.
           05  WS-DISP-EOF-SW          PIC X(01) VALUE 'N'.
               88  WS-DISP-EOF         VALUE 'Y'.
           05  WS-DTL-FOUND-SW         PIC X(01) VALUE 'N'.
               88  WS-DTL-FOUND        VALUE 'Y'.
       01  WS-DLI-STATUS               PIC X(02) VALUE SPACES.
           88  WS-DLI-OK               VALUE '  '.
           88  WS-DLI-NOT-FOUND        VALUE 'GE'.
           88  WS-DLI-END-OF-DB        VALUE 'GB'.
           88  WS-DLI-NO-MORE          VALUE 'GE' 'GB' 'GA' 'GK'.
       01  WS-LOCAL-COUNTS.
           05  WS-ROOT-CNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-DISP-CNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-OPEN-CNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-OUT-RANGE-CNT        PIC S9(09) COMP-3 VALUE 0.
       01  WS-AGE-WORK.
           05  WA-RAISED-YY            PIC 9(02) VALUE 0.
           05  WA-RAISED-DDD           PIC 9(03) VALUE 0.
           05  WA-CUR-YY               PIC 9(02) VALUE 0.
           05  WA-CUR-DDD              PIC 9(03) VALUE 0.
           05  WA-DAYS                 PIC S9(05) COMP-3 VALUE 0.
       01  WS-ACCUMS-LOCAL.
           05  WS-ACC-DISPUTED         PIC S9(15)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-ACC-ALLOWED          PIC S9(15)V9(02) COMP-3
                                                  VALUE 0.
       01  WS-EDIT-FIELDS.
           05  WS-ED-COUNT             PIC ZZZ,ZZZ,ZZ9.
           05  WS-ED-AMT               PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
      *
       LINKAGE SECTION.
       01  DB-PCB-SETX.
           05  DBP-DBD-NAME            PIC X(08).
           05  DBP-SEG-LEVEL           PIC X(02).
           05  DBP-STATUS-CODE         PIC X(02).
           05  DBP-PROC-OPTIONS        PIC X(04).
           05  DBP-RESERVED            PIC S9(05) COMP.
           05  DBP-SEG-NAME-FB         PIC X(08).
           05  DBP-KEY-LENGTH          PIC S9(05) COMP.
           05  DBP-NUMB-SENS-SEGS      PIC S9(05) COMP.
           05  DBP-KEY-FB              PIC X(30).
      *
       PROCEDURE DIVISION USING DB-PCB-SETX.
       P0000-MAINLINE.
           PERFORM P1000-INIT THRU P1000-EXIT.
           PERFORM P2000-PROCESS THRU P2000-EXIT
               UNTIL WS-EOF.
           PERFORM P8000-CONTROL THRU P8000-EXIT.
           PERFORM P9000-TERM THRU P9000-EXIT.
           GOBACK.

      *****************************************************************
      * S100-INITIALISATION                                           *
      *****************************************************************
       S100-INITIALISATION SECTION.

       P1000-INIT.
           MOVE 'P1000-INIT' TO WS-PARA-NAME.
           OPEN INPUT PARM-FILE.
           OPEN INPUT REQUEST-FILE.
           OPEN OUTPUT DISPUTE-OUT.
           OPEN OUTPUT CONTROL-FILE.
           OPEN OUTPUT REPORT-FILE.
           MOVE SPACES TO PARM-CARD.
           READ PARM-FILE
               AT END
                   MOVE 8 TO RETURN-CODE
                   GOBACK
           END-READ.
           MOVE PARM-CARD TO WS-PARM-AREA.
           MOVE WP-CYCLE-YYDDD TO DW-CURRENT-YYDDD.
           MOVE DW-CUR-YY TO WA-CUR-YY.
           MOVE DW-CUR-DDD TO WA-CUR-DDD.
           PERFORM P2100-READ-REQUEST THRU P2100-EXIT.

       P1000-EXIT.
           EXIT.

      *****************************************************************
      * S200-MAIN-PROCESS                                             *
      *****************************************************************
       S200-MAIN-PROCESS SECTION.

       P2000-PROCESS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P3000-POSITION-INDEX THRU P3000-EXIT.
           PERFORM P3400-WALK-INDEX THRU P3400-EXIT
               UNTIL WS-ROOT-EOF.
           PERFORM P2100-READ-REQUEST THRU P2100-EXIT.

       P2000-EXIT.
           EXIT.

       P2100-READ-REQUEST.
           READ REQUEST-FILE
               AT END
                   SET WS-EOF TO TRUE
                   GO TO P2100-EXIT
           END-READ.
           MOVE REQUEST-CARD TO WS-REQUEST-AREA.
           ADD 1 TO WS-READ-CNT.
           IF WR-STATUS-WANTED = SPACE
               MOVE 'O' TO WR-STATUS-WANTED
           END-IF.
           MOVE WR-STATUS-WANTED TO SSA-SP-STATUS.

       P2100-EXIT.
           EXIT.

      *****************************************************************
      * S300-INDEX-WALK                                               *
      *****************************************************************
       S300-INDEX-WALK SECTION.

       P3000-POSITION-INDEX.
      * POSITION ON THE FIRST ROOT WHOSE INDEX KEY MATCHES THE
      * REQUESTED COUNTERPARTY AND THE START OF THE PERIOD RANGE.
      * THE INDEX IS UNIQUE ON OCN AND PERIOD, SO A COUNTERPARTY
      * WITH MORE THAN ONE SETTLEMENT KIND IN THE SAME PERIOD IS
      * REACHED THROUGH THE FIRST ONE ONLY AND THE REST ARE FOUND BY
      * READING FORWARD.
           MOVE 'P3000-POSITION-INDEX' TO WS-PARA-NAME.
           MOVE 'N' TO WS-ROOT-EOF-SW.
           MOVE WR-OCN         TO SSA-SX-OCN.
           MOVE WR-PERIOD-FROM TO SSA-SX-PERIOD.
           CALL 'CBLTDLI' USING DLI-GU
                                DB-PCB-SETX
                                WS-SETLSEG-IO
                                SSA-SETLSEG-X.
           MOVE DBP-STATUS-CODE TO WS-DLI-STATUS.
           IF WS-DLI-OK
               ADD 1 TO WS-ROOT-CNT
               GO TO P3000-EXIT
           END-IF.
           MOVE 'Y' TO WS-ROOT-EOF-SW.
           ADD 1 TO WS-REJECT-CNT.
           IF NOT WS-DLI-NO-MORE
               PERFORM P9500-DLI-ERROR THRU P9500-EXIT
           END-IF.

       P3000-EXIT.
           EXIT.

       P3400-WALK-INDEX.
      * PROCESS THE ROOT IN HAND, THEN STEP FORWARD.  THE WALK STOPS
      * WHEN THE COUNTERPARTY CHANGES OR THE PERIOD PASSES THE END
      * OF THE RANGE.
           IF SS-COUNTERPARTY NOT = WR-OCN
               MOVE 'Y' TO WS-ROOT-EOF-SW
               GO TO P3400-EXIT
           END-IF.
           IF SS-SETTLE-PERIOD > WR-PERIOD-THRU
               ADD 1 TO WS-OUT-RANGE-CNT
               MOVE 'Y' TO WS-ROOT-EOF-SW
               GO TO P3400-EXIT
           END-IF.
           PERFORM P4000-READ-DISPUTES THRU P4000-EXIT.
           PERFORM P3600-NEXT-ROOT THRU P3600-EXIT.

       P3400-EXIT.
           EXIT.

       P3600-NEXT-ROOT.
           CALL 'CBLTDLI' USING DLI-GN
                                DB-PCB-SETX
                                WS-SETLSEG-IO
                                SSA-SETLSEG-U.
           MOVE DBP-STATUS-CODE TO WS-DLI-STATUS.
           IF WS-DLI-OK
               ADD 1 TO WS-ROOT-CNT
               GO TO P3600-EXIT
           END-IF.
           MOVE 'Y' TO WS-ROOT-EOF-SW.
           IF NOT WS-DLI-NO-MORE
               PERFORM P9500-DLI-ERROR THRU P9500-EXIT
           END-IF.

       P3600-EXIT.
           EXIT.

      *****************************************************************
      * S400-DEPENDENT-WALK                                           *
      *****************************************************************
       S400-DEPENDENT-WALK SECTION.

       P4000-READ-DISPUTES.
      * WALK THE DISPUTE SEGMENTS UNDER THIS ROOT, QUALIFIED ON THE
      * STATUS THE REQUEST ASKED FOR.  A SETTLEMENT ROW WITH THE
      * DISPUTE SWITCH SET BUT NO DISPUTE SEGMENT IS REPORTED WITH A
      * BLANK IDENTIFIER RATHER THAN BEING SKIPPED - THAT COMBINATION
      * ARISES WHEN A DISPUTE IS RAISED ONLINE AND THE OVERNIGHT
      * POSTING HAS NOT RUN.
           MOVE 'P4000-READ-DISPUTES' TO WS-PARA-NAME.
           MOVE 'N' TO WS-DISP-EOF-SW.
           MOVE 'N' TO WS-DTL-FOUND-SW.
           PERFORM P4200-GET-DETAIL THRU P4200-EXIT.
           PERFORM P4400-NEXT-DISPUTE THRU P4400-EXIT
               UNTIL WS-DISP-EOF.
           IF WS-DISP-CNT = ZERO AND SS-IN-DISPUTE
               MOVE SPACES TO WS-SETLDISP-IO
               MOVE WR-STATUS-WANTED TO SP-STATUS
               PERFORM P5000-BUILD-OUTPUT THRU P5000-EXIT
               PERFORM P6000-WRITE-OUTPUT THRU P6000-EXIT
           END-IF.

       P4000-EXIT.
           EXIT.

       P4200-GET-DETAIL.
      * ONE DETAIL SEGMENT CARRIES THE CIRCUIT.  IT IS TAKEN FIRST
      * SO THE CIRCUIT IS AVAILABLE FOR EVERY DISPUTE LINE BELOW.
           CALL 'CBLTDLI' USING DLI-GNP
                                DB-PCB-SETX
                                WS-SETLDTL-IO
                                SSA-SETLDTL-U.
           MOVE DBP-STATUS-CODE TO WS-DLI-STATUS.
           IF WS-DLI-OK
               MOVE 'Y' TO WS-DTL-FOUND-SW
           ELSE
               MOVE SPACES TO WS-SETLDTL-IO
           END-IF.

       P4200-EXIT.
           EXIT.

       P4400-NEXT-DISPUTE.
           CALL 'CBLTDLI' USING DLI-GNP
                                DB-PCB-SETX
                                WS-SETLDISP-IO
                                SSA-SETLDISP-Q.
           MOVE DBP-STATUS-CODE TO WS-DLI-STATUS.
           IF WS-DLI-NO-MORE
               MOVE 'Y' TO WS-DISP-EOF-SW
               GO TO P4400-EXIT
           END-IF.
           IF NOT WS-DLI-OK
               MOVE 'Y' TO WS-DISP-EOF-SW
               PERFORM P9500-DLI-ERROR THRU P9500-EXIT
               GO TO P4400-EXIT
           END-IF.
           ADD 1 TO WS-DISP-CNT.
           IF SP-OPEN
               ADD 1 TO WS-OPEN-CNT
           END-IF.
           PERFORM P5000-BUILD-OUTPUT THRU P5000-EXIT.
           PERFORM P6000-WRITE-OUTPUT THRU P6000-EXIT.

       P4400-EXIT.
           EXIT.

      *****************************************************************
      * S500-OUTPUT-BUILD                                             *
      *****************************************************************
       S500-OUTPUT-BUILD SECTION.

       P5000-BUILD-OUTPUT.
           MOVE SPACES TO WS-OUT-AREA.
           MOVE SS-COUNTERPARTY  TO WO-OCN.
           MOVE SS-SETTLE-PERIOD TO WO-SETTLE-PERIOD.
           MOVE SS-SETTLE-TYPE   TO WO-SETTLE-TYPE.
           MOVE SS-SEQ           TO WO-SEQ.
           MOVE SS-NET-DUE       TO WO-NET-DUE.
           MOVE SP-DISPUTE-ID    TO WO-DISPUTE-ID.
           MOVE SP-STATUS        TO WO-STATUS.
           MOVE SP-RAISED-YYDDD  TO WO-RAISED-YYDDD.
           MOVE SP-DISPUTED-AMT  TO WO-DISPUTED-AMT.
           MOVE SP-ALLOWED-AMT   TO WO-ALLOWED-AMT.
           MOVE SP-REASON-CD     TO WO-REASON-CD.
           MOVE SP-NARRATIVE     TO WO-NARRATIVE.
           IF WS-DTL-FOUND
               MOVE SD-CIRCUIT-ID TO WO-CIRCUIT-ID
           END-IF.
           PERFORM P5400-AGE-DISPUTE THRU P5400-EXIT.
           ADD SP-DISPUTED-AMT TO WS-ACC-DISPUTED.
           ADD SP-ALLOWED-AMT  TO WS-ACC-ALLOWED.

       P5000-EXIT.
           EXIT.

       P5400-AGE-DISPUTE.
      * AGE THE DISPUTE IN DAYS AND PUT IT IN A BAND.  THE DAY
      * COUNT IS THE DIFFERENCE BETWEEN THE TWO DAY-OF-YEAR VALUES
      * PLUS THREE HUNDRED AND SIXTY FIVE FOR EACH WHOLE YEAR
      * BETWEEN THEM, WHICH IS ACCURATE ENOUGH FOR A BANDED REPORT.
           MOVE ZERO TO WA-DAYS.
           IF SP-RAISED-YYDDD = ZERO
               MOVE ZERO TO WO-AGE-DAYS
               MOVE '  ' TO WO-AGE-BAND
               GO TO P5400-EXIT
           END-IF.
           DIVIDE SP-RAISED-YYDDD BY 1000
                  GIVING WA-RAISED-YY
                  REMAINDER WA-RAISED-DDD.
           COMPUTE WA-DAYS = ((WA-CUR-YY - WA-RAISED-YY) * 365)
                           + (WA-CUR-DDD - WA-RAISED-DDD).
           MOVE WA-DAYS TO WO-AGE-DAYS.
           IF WA-DAYS < 31
               MOVE '01' TO WO-AGE-BAND
               GO TO P5400-EXIT
           END-IF.
           IF WA-DAYS < 61
               MOVE '02' TO WO-AGE-BAND
               GO TO P5400-EXIT
           END-IF.
           IF WA-DAYS < 91
               MOVE '03' TO WO-AGE-BAND
               GO TO P5400-EXIT
           END-IF.
           IF WA-DAYS < 181
               MOVE '04' TO WO-AGE-BAND
               GO TO P5400-EXIT
           END-IF.
           MOVE '05' TO WO-AGE-BAND.

       P5400-EXIT.
           EXIT.

       P6000-WRITE-OUTPUT.
           MOVE WS-OUT-AREA TO DISPUTE-OUT-RECORD.
           WRITE DISPUTE-OUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.

       P6000-EXIT.
           EXIT.

      *****************************************************************
      * S800-CONTROL-AND-TERMINATION                                  *
      *****************************************************************
       S800-CONTROL SECTION.

       P8000-CONTROL.
      * THE READ COUNT IS THE NUMBER OF REQUEST CARDS.  THE WRITE
      * COUNT IS THE NUMBER OF DISPUTE LINES PRODUCED, WHICH IS
      * NORMALLY LARGER.  THE DIFFERENCE IS CARRIED IN THE
      * SUMMARISED FIELD SO THE EQUATION STILL HOLDS.
           MOVE 'P8000-CONTROL' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-CONTROL-RECORD.
           MOVE WP-RUN-ID        TO CT-RUN-ID.
           MOVE WS-PGM-NAME      TO CT-PROCESS-ID.
           MOVE 060              TO CT-STEP-SEQ.
           MOVE WP-CYCLE-YYDDD   TO CT-CYCLE-YYDDD.
           MOVE WP-BILL-PERIOD   TO CT-BILL-PERIOD.
           MOVE ZERO             TO CT-RERUN-NBR.
           MOVE 'CABJ2600'       TO CT-JOBNAME.
           MOVE 'IMSSTEP'        TO CT-STEPNAME.
           MOVE WS-READ-CNT      TO CT-READ.
           MOVE WS-READ-CNT      TO CT-WRITTEN.
           MOVE WS-REJECT-CNT    TO CT-REJECTED.
           SUBTRACT WS-REJECT-CNT FROM CT-WRITTEN.
           MOVE ZERO             TO CT-SUMMARISED.
           MOVE ZERO             TO CT-CARRIED-FWD.
           MOVE WS-ACC-DISPUTED  TO CT-HASH-MINUTES.
           MOVE WS-ACC-ALLOWED   TO CT-HASH-AMOUNT.
           MOVE WS-DISP-CNT      TO CT-HASH-SEQ.
           MOVE WS-ROOT-CNT      TO CT-HASH-OCN.
           COMPUTE WS-ACC-SEQ-HASH = CT-WRITTEN + CT-REJECTED
                                   + CT-SUMMARISED + CT-CARRIED-FWD.
           IF WS-ACC-SEQ-HASH = CT-READ
               SET CT-IN-BALANCE TO TRUE
           ELSE
               SET CT-OUT-OF-BAL TO TRUE
           END-IF.
           MOVE ZERO             TO CT-RC.
           MOVE SPACES           TO CT-ABEND-CD.
           MOVE SS-KEY           TO CT-RESTART-KEY.
           MOVE CABS-CONTROL-RECORD TO CONTROL-RECORD.
           WRITE CONTROL-RECORD.

       P8000-EXIT.
           EXIT.

       P9000-TERM.
           MOVE 'P9000-TERM' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '1' TO PC-CC.
           MOVE 'CABCTL06' TO PC-COL-001-020.
           MOVE 'SETTLEMENT DISPUTE ENQUIRY' TO PC-COL-021-060.
           MOVE CABS-PRINT-LINE TO REPORT-LINE.
           WRITE REPORT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '0' TO PC-CC.
           MOVE 'ROOTS READ' TO PC-COL-001-020.
           MOVE WS-ROOT-CNT TO WS-ED-COUNT.
           MOVE WS-ED-COUNT TO PC-COL-021-060.
           MOVE CABS-PRINT-LINE TO REPORT-LINE.
           WRITE REPORT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DISPUTE SEGMENTS' TO PC-COL-001-020.
           MOVE WS-DISP-CNT TO WS-ED-COUNT.
           MOVE WS-ED-COUNT TO PC-COL-021-060.
           MOVE CABS-PRINT-LINE TO REPORT-LINE.
           WRITE REPORT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'OPEN DISPUTES' TO PC-COL-001-020.
           MOVE WS-OPEN-CNT TO WS-ED-COUNT.
           MOVE WS-ED-COUNT TO PC-COL-021-060.
           MOVE CABS-PRINT-LINE TO REPORT-LINE.
           WRITE REPORT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DISPUTED VALUE' TO PC-COL-001-020.
           MOVE WS-ACC-DISPUTED TO WS-ED-AMT.
           MOVE WS-ED-AMT TO PC-COL-021-060.
           MOVE CABS-PRINT-LINE TO REPORT-LINE.
           WRITE REPORT-LINE.
           CLOSE PARM-FILE.
           CLOSE REQUEST-FILE.
           CLOSE DISPUTE-OUT.
           CLOSE CONTROL-FILE.
           CLOSE REPORT-FILE.

       P9000-EXIT.
           EXIT.

       P9500-DLI-ERROR.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '0' TO PC-CC.
           MOVE 'DLI STATUS' TO PC-COL-001-020.
           MOVE WS-DLI-STATUS TO PC-COL-021-060.
           MOVE WS-PARA-NAME TO PC-COL-061-090.
           MOVE DBP-SEG-NAME-FB TO PC-COL-091-132.
           MOVE CABS-PRINT-LINE TO REPORT-LINE.
           WRITE REPORT-LINE.
           MOVE 3510 TO CT-RC.
           MOVE 8 TO RETURN-CODE.

       P9500-EXIT.
           EXIT.
