       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABCTL04.
      *****************************************************************
      * CABCTL04 - BILL HISTORY LOAD AND INVOICE REGISTER UPDATE      *
      * APPLICATION : CABS                                            *
      * COMPILER    : ENTERPRISE COBOL                                *
      * INPUTS      : IMS PCB 1  CABBHSDB  PSB CABCT04P  PROCOPT AP   *
      *               BHDRIN  TELCABS.CABS.BILLHDR.CUT(0)    FB 400   *
      *               BDTLIN  TELCABS.CABS.BILLDTL(0)        VB 1651  *
      *               PARMIN  INSTREAM SYSIN PARM CARD        FB 080  *
      * OUTPUTS     : BHDRMST TELCABS.CABS.BILLHDR      VSAM KSDS     *
      *               SUSOUT  TELCABS.CABS.USAGE.SUSPENSE(+1) FB 300  *
      *               RPTOUT  SYSOUT PRINT                    FBA 133 *
      * CONTROL     : CTLOUT                                  CABSCTL *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED               *
      *                       + CT-SUMMARISED + CT-CARRIED-FWD        *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      *                                                               *
      * THE BILL HISTORY DATABASE HOLDS SEVEN YEARS OF INVOICES FOR   *
      * THE ONLINE ENQUIRY AND FOR REGULATORY RETENTION.  THE VSAM    *
      * BILL HEADER MASTER HOLDS THE CURRENT PERIOD ONLY AND IS WHAT  *
      * THE SETTLEMENT STATEMENT PROGRAM READS FOR THE INVOICE        *
      * NUMBER.  BOTH ARE WRITTEN HERE.                               *
      *                                                               *
      * REVISION HISTORY                                              *
      *   V1.00  1991-09-30  J.M.CASTILLO  INITIAL                    *
      *   V1.04  1996-05-08  D.OKONKWO     DETAIL SEGMENTS ADDED      *
      *   V1.06  1998-08-25  P.NAIR        1987 FORMAT BRIDGE ADDED   *
      *                                    FOR THE SITES STILL ON THE *
      *                                    OLD CUT                    *
      *   V2.00  2005-04-11  A.BUKOWSKI    HISAM REORG DEPENDENCY     *
      *                                    NOTED - SEE OPS RUNBOOK    *
      *   V2.02  2012-07-19  L.FERREIRA    RETENTION RAISED TO SEVEN  *
      *                                    YEARS                      *
      *   V2.03  2017-10-02  M.HAAS        RECOMPILE ONLY - IMS V14   *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PARM-FILE     ASSIGN TO PARMIN
                  FILE STATUS IS WS-FS-INPUT.
           SELECT HEADER-IN     ASSIGN TO BHDRIN
                  FILE STATUS IS WS-FS-INPUT.
           SELECT DETAIL-IN     ASSIGN TO BDTLIN
                  FILE STATUS IS WS-FS-TABLE.
           SELECT HEADER-MASTER ASSIGN TO BHDRMST
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS HM-KEY
                  FILE STATUS IS WS-FS-OUTPUT.
           SELECT SUSPENSE-FILE ASSIGN TO SUSOUT
                  FILE STATUS IS WS-FS-SUSPENSE.
           SELECT CONTROL-FILE  ASSIGN TO CTLOUT
                  FILE STATUS IS WS-FS-CONTROL.
           SELECT REPORT-FILE   ASSIGN TO RPTOUT.
       DATA DIVISION.
       FILE SECTION.
       FD  PARM-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE OMITTED.
       01  PARM-CARD                   PIC X(80).
       FD  HEADER-IN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  HEADER-IN-RECORD            PIC X(400).
       FD  DETAIL-IN
           RECORDING MODE IS V
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD IS VARYING IN SIZE FROM 200 TO 1647
                  DEPENDING ON WS-DETAIL-LEN.
       01  DETAIL-IN-RECORD            PIC X(1647).
       FD  HEADER-MASTER
           RECORD CONTAINS 400 CHARACTERS.
       01  HEADER-MASTER-RECORD.
           05  HM-KEY.
               10  HM-BAN              PIC X(13).
               10  HM-BILL-PERIOD      PIC 9(06).
           05  HM-REST                 PIC X(381).
       FD  SUSPENSE-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  SUSPENSE-OUT-RECORD         PIC X(300).
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
       01  WS-PGM-NAME                 PIC X(08) VALUE 'CABCTL04'.
       01  WS-PARA-NAME                PIC X(30) VALUE SPACES.
       01  WS-DETAIL-LEN               PIC S9(04) COMP VALUE 200.
       COPY CABSWRK.
       COPY CABSBHDR.
       COPY CABSPRNT.
      *
       01  WS-DLI-FUNCTIONS.
           05  DLI-GU                  PIC X(04) VALUE 'GU  '.
           05  DLI-GHU                 PIC X(04) VALUE 'GHU '.
           05  DLI-GN                  PIC X(04) VALUE 'GN  '.
           05  DLI-GNP                 PIC X(04) VALUE 'GNP '.
           05  DLI-ISRT                PIC X(04) VALUE 'ISRT'.
           05  DLI-REPL                PIC X(04) VALUE 'REPL'.
           05  DLI-DLET                PIC X(04) VALUE 'DLET'.
           05  DLI-CHKP                PIC X(04) VALUE 'CHKP'.
      *
       01  SSA-BHSTSEG-Q.
           05  FILLER                  PIC X(08) VALUE 'BHSTSEG '.
           05  FILLER                  PIC X(01) VALUE '('.
           05  FILLER                  PIC X(08) VALUE 'BHKEY   '.
           05  FILLER                  PIC X(02) VALUE ' ='.
           05  SSA-BH-KEY.
               10  SSA-BH-BAN          PIC X(13) VALUE SPACES.
               10  SSA-BH-PERIOD       PIC 9(06) VALUE ZERO.
           05  FILLER                  PIC X(01) VALUE ')'.
       01  SSA-BHSTSEG-U.
           05  FILLER                  PIC X(08) VALUE 'BHSTSEG '.
           05  FILLER                  PIC X(01) VALUE SPACE.
       01  SSA-BHSTDTL-U.
           05  FILLER                  PIC X(08) VALUE 'BHSTDTL '.
           05  FILLER                  PIC X(01) VALUE SPACE.
      *
       01  WS-BHSTSEG-IO.
           05  BS-KEY.
               10  BS-BAN              PIC X(13).
               10  BS-BILL-PERIOD      PIC 9(06).
           05  BS-OCN                  PIC X(04).
           05  BS-INVOICE-NBR          PIC X(14).
           05  BS-BILL-YYDDD           PIC 9(05).
           05  BS-DUE-YYDDD            PIC 9(05).
           05  BS-PRIOR-BAL            PIC S9(13)V9(02) COMP-3.
           05  BS-PAYMENTS             PIC S9(13)V9(02) COMP-3.
           05  BS-ADJUSTMENTS          PIC S9(13)V9(02) COMP-3.
           05  BS-CURR-USAGE           PIC S9(13)V9(02) COMP-3.
           05  BS-CURR-RECURRING       PIC S9(13)V9(02) COMP-3.
           05  BS-CURR-NONRECUR        PIC S9(13)V9(02) COMP-3.
           05  BS-RESTATEMENT          PIC S9(13)V9(02) COMP-3.
           05  BS-SETTLEMENT-NET       PIC S9(13)V9(02) COMP-3.
           05  BS-TAX                  PIC S9(11)V9(02) COMP-3.
           05  BS-TOTAL-DUE            PIC S9(13)V9(02) COMP-3.
           05  BS-INTERSTATE-AMT       PIC S9(13)V9(02) COMP-3.
           05  BS-INTRASTATE-AMT       PIC S9(13)V9(02) COMP-3.
           05  BS-LOCAL-AMT            PIC S9(13)V9(02) COMP-3.
           05  BS-DETAIL-LINES         PIC S9(07) COMP-3.
           05  BS-CDR-COUNT            PIC S9(11) COMP-3.
           05  BS-STATUS               PIC X(01).
           05  BS-HOLD-REASON          PIC X(04).
           05  BS-LOAD-YYDDD           PIC 9(05).
           05  BS-RETAIN-THRU          PIC 9(05).
           05  BS-FILLER               PIC X(238).
       01  WS-BHSTDTL-IO.
           05  BD-SEQ                  PIC 9(07).
           05  BD-SECTION              PIC X(02).
           05  BD-JURIS-CD             PIC X(01).
           05  BD-STATE-CD             PIC X(02).
           05  BD-RATE-ELEM            PIC X(06).
           05  BD-DESCRIPTION          PIC X(60).
           05  BD-MINUTES              PIC S9(13)V9(02) COMP-3.
           05  BD-AMOUNT               PIC S9(13)V9(02) COMP-3.
           05  BD-FILLER               PIC X(206).
      *
       01  WS-DETAIL-AREA.
           05  WD-BAN                  PIC X(13).
           05  WD-BILL-PERIOD          PIC 9(06).
           05  WD-SECTION              PIC X(02).
           05  WD-LINE-SEQ             PIC S9(07) COMP-3.
           05  WD-OCN                  PIC X(04).
           05  WD-JURIS-CD             PIC X(01).
           05  WD-STATE-CD             PIC X(02).
           05  WD-DESCRIPTION          PIC X(60).
           05  WD-TOT-MINUTES          PIC S9(13)V9(02) COMP-3.
           05  WD-TOT-AMOUNT           PIC S9(13)V9(05) COMP-3.
           05  WD-TOT-ROUNDED          PIC S9(13)V9(02) COMP-3.
           05  WD-REST                 PIC X(1090).
      *
       01  WS-PARM-AREA.
           05  WP-RUN-ID               PIC X(12).
           05  WP-CYCLE-YYDDD          PIC 9(05).
           05  WP-BILL-PERIOD          PIC 9(06).
           05  WP-RETAIN-YEARS         PIC 9(02).
           05  WP-COMMIT-INT           PIC 9(04).
           05  WP-BRIDGE-SW            PIC X(01).
           05  WP-RESTART-KEY          PIC X(19).
           05  WP-FILLER               PIC X(31).
      *
       01  WS-SWITCHES-LOCAL.
           05  WS-DTL-EOF-SW           PIC X(01) VALUE 'N'.
               88  WS-DTL-EOF          VALUE 'Y'.
           05  WS-SEG-FOUND-SW         PIC X(01) VALUE 'N'.
               88  WS-SEG-FOUND        VALUE 'Y'.
           05  WS-VSAM-FOUND-SW        PIC X(01) VALUE 'N'.
               88  WS-VSAM-FOUND       VALUE 'Y'.
      *
      * THE 1987 CUT FORMAT IS STILL SUPPORTED FOR THE SITES THAT
      * WERE NOT CONVERTED.  THE SWITCH IS SET FROM THE PARM CARD.
      *
       01  WS-BRIDGE-CONTROL.
           05  WS-BRIDGE-SW            PIC X(01) VALUE 'N'.
               88  WS-BRIDGE-REQUIRED  VALUE 'Y'.
           05  WS-BRIDGE-CNT           PIC S9(09) COMP-3 VALUE 0.
           05  WS-BRIDGE-RC            PIC S9(04) COMP VALUE 0.
       01  WS-BRIDGE-PARM.
           05  WB-IN-LEN               PIC S9(04) COMP VALUE 133.
           05  WB-OUT-LEN              PIC S9(04) COMP VALUE 200.
           05  WB-IN-AREA              PIC X(133) VALUE SPACES.
           05  WB-OUT-AREA             PIC X(200) VALUE SPACES.
           05  WB-RETURN               PIC S9(04) COMP VALUE 0.
       01  WS-DLI-STATUS               PIC X(02) VALUE SPACES.
           88  WS-DLI-OK               VALUE '  '.
           88  WS-DLI-NOT-FOUND        VALUE 'GE'.
           88  WS-DLI-DUP-INSERT       VALUE 'II'.
           88  WS-DLI-NO-MORE          VALUE 'GE' 'GB' 'GA' 'GK'.
       01  WS-LOCAL-COUNTS.
           05  WS-HDR-ISRT-CNT         PIC S9(09) COMP-3 VALUE 0.
           05  WS-HDR-REPL-CNT         PIC S9(09) COMP-3 VALUE 0.
           05  WS-DTL-ISRT-CNT         PIC S9(09) COMP-3 VALUE 0.
           05  WS-VSAM-CNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-DTL-READ-CNT         PIC S9(09) COMP-3 VALUE 0.
           05  WS-SINCE-CHKP           PIC S9(05) COMP-3 VALUE 0.
           05  WS-CHKP-CNT             PIC S9(05) COMP-3 VALUE 0.
       01  WS-CHKP-AREA.
           05  WS-CHKP-ID              PIC X(08) VALUE 'CTL04000'.
           05  WS-CHKP-KEY             PIC X(19) VALUE SPACES.
       01  WS-DATE-CALC.
           05  WS-RETAIN-YY            PIC 9(02) VALUE 0.
           05  WS-RETAIN-YYDDD         PIC 9(05) VALUE 0.
       01  WS-ED-COUNT                 PIC ZZZ,ZZZ,ZZ9.
      *
       LINKAGE SECTION.
       01  DB-PCB-BHST.
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
       PROCEDURE DIVISION USING DB-PCB-BHST.
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
           OPEN INPUT HEADER-IN.
           OPEN INPUT DETAIL-IN.
           OPEN I-O HEADER-MASTER.
           OPEN OUTPUT SUSPENSE-FILE.
           OPEN OUTPUT CONTROL-FILE.
           OPEN OUTPUT REPORT-FILE.
           MOVE SPACES TO PARM-CARD.
           READ PARM-FILE
               AT END
                   MOVE 8 TO RETURN-CODE
                   GOBACK
           END-READ.
           MOVE PARM-CARD TO WS-PARM-AREA.
           IF WP-COMMIT-INT = ZERO
               MOVE 200 TO WP-COMMIT-INT
           END-IF.
           IF WP-RETAIN-YEARS = ZERO
               MOVE 07 TO WP-RETAIN-YEARS
           END-IF.
           PERFORM P1300-SET-RETENTION THRU P1300-EXIT.
           PERFORM P2100-READ-HEADER THRU P2100-EXIT.
           PERFORM P2200-READ-DETAIL THRU P2200-EXIT.

       P1000-EXIT.
           EXIT.

       P1300-SET-RETENTION.
      * THE RETENTION DATE IS THE CYCLE DATE PLUS THE RETENTION
      * PERIOD IN YEARS.  THE TWO DIGIT YEAR IS STEPPED DIRECTLY.
           MOVE WP-CYCLE-YYDDD TO DW-CURRENT-YYDDD.
           COMPUTE WS-RETAIN-YY = DW-CUR-YY + WP-RETAIN-YEARS.
           IF WS-RETAIN-YY > 99
               SUBTRACT 100 FROM WS-RETAIN-YY
           END-IF.
           COMPUTE WS-RETAIN-YYDDD =
                   (WS-RETAIN-YY * 1000) + DW-CUR-DDD.

       P1300-EXIT.
           EXIT.

      *****************************************************************
      * S200-MAIN-PROCESS                                             *
      *****************************************************************
       S200-MAIN-PROCESS SECTION.

       P2000-PROCESS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P3000-LOAD-HISTORY THRU P3000-EXIT.
           PERFORM P4000-ATTACH-DETAIL THRU P4000-EXIT.
           PERFORM P5000-UPDATE-VSAM THRU P5000-EXIT.
           ADD 1 TO WS-WRITE-CNT.
           PERFORM P6500-CHECKPOINT THRU P6500-EXIT.
           PERFORM P2100-READ-HEADER THRU P2100-EXIT.

       P2000-EXIT.
           EXIT.

       P2100-READ-HEADER.
           READ HEADER-IN
               AT END
                   SET WS-EOF TO TRUE
                   GO TO P2100-EXIT
           END-READ.
           MOVE HEADER-IN-RECORD TO CABS-BILL-HEADER.
           ADD 1 TO WS-READ-CNT.
           IF WS-BRIDGE-REQUIRED
               PERFORM P7700-BRIDGE-OLD-FORMAT THRU P7700-EXIT
           END-IF.

       P2100-EXIT.
           EXIT.

       P2200-READ-DETAIL.
           READ DETAIL-IN
               AT END
                   MOVE 'Y' TO WS-DTL-EOF-SW
                   GO TO P2200-EXIT
           END-READ.
           MOVE DETAIL-IN-RECORD TO WS-DETAIL-AREA.
           ADD 1 TO WS-DTL-READ-CNT.

       P2200-EXIT.
           EXIT.

      *****************************************************************
      * S300-IMS-LOAD                                                 *
      *****************************************************************
       S300-IMS-LOAD SECTION.

       P3000-LOAD-HISTORY.
      * FIRST STORE.  THE HISTORY ROOT IS INSERTED IF IT IS NOT
      * THERE AND REPLACED IF IT IS.  A REPLACE HAPPENS ON A RERUN
      * OF THE SAME CYCLE AND ON A CORRECTED BILL.
           MOVE 'P3000-LOAD-HISTORY' TO WS-PARA-NAME.
           MOVE BH-BAN         TO SSA-BH-BAN.
           MOVE BH-BILL-PERIOD TO SSA-BH-PERIOD.
           MOVE 'N' TO WS-SEG-FOUND-SW.
           CALL 'CBLTDLI' USING DLI-GHU
                                DB-PCB-BHST
                                WS-BHSTSEG-IO
                                SSA-BHSTSEG-Q.
           MOVE DBP-STATUS-CODE TO WS-DLI-STATUS.
           IF WS-DLI-OK
               MOVE 'Y' TO WS-SEG-FOUND-SW
           END-IF.
           PERFORM P3200-BUILD-ROOT THRU P3200-EXIT.
           IF WS-SEG-FOUND
               PERFORM P3400-REPLACE-ROOT THRU P3400-EXIT
           ELSE
               PERFORM P3600-INSERT-ROOT THRU P3600-EXIT
           END-IF.

       P3000-EXIT.
           EXIT.

       P3200-BUILD-ROOT.
           MOVE SPACES TO WS-BHSTSEG-IO.
           MOVE BH-BAN            TO BS-BAN.
           MOVE BH-BILL-PERIOD    TO BS-BILL-PERIOD.
           MOVE BH-OCN            TO BS-OCN.
           MOVE BH-INVOICE-NBR    TO BS-INVOICE-NBR.
           MOVE BH-BILL-YYDDD     TO BS-BILL-YYDDD.
           MOVE BH-DUE-YYDDD      TO BS-DUE-YYDDD.
           MOVE BH-PRIOR-BAL      TO BS-PRIOR-BAL.
           MOVE BH-PAYMENTS       TO BS-PAYMENTS.
           MOVE BH-ADJUSTMENTS    TO BS-ADJUSTMENTS.
           MOVE BH-CURR-USAGE     TO BS-CURR-USAGE.
           MOVE BH-CURR-RECURRING TO BS-CURR-RECURRING.
           MOVE BH-CURR-NONRECUR  TO BS-CURR-NONRECUR.
           MOVE BH-RESTATEMENT    TO BS-RESTATEMENT.
           MOVE BH-SETTLEMENT-NET TO BS-SETTLEMENT-NET.
           MOVE BH-TAX            TO BS-TAX.
           MOVE BH-TOTAL-DUE      TO BS-TOTAL-DUE.
           MOVE BH-INTERSTATE-AMT TO BS-INTERSTATE-AMT.
           MOVE BH-INTRASTATE-AMT TO BS-INTRASTATE-AMT.
           MOVE BH-LOCAL-AMT      TO BS-LOCAL-AMT.
           MOVE BH-DETAIL-LINES   TO BS-DETAIL-LINES.
           MOVE BH-CDR-COUNT      TO BS-CDR-COUNT.
           MOVE BH-STATUS         TO BS-STATUS.
           MOVE BH-HOLD-REASON    TO BS-HOLD-REASON.
           MOVE WP-CYCLE-YYDDD    TO BS-LOAD-YYDDD.
           MOVE WS-RETAIN-YYDDD   TO BS-RETAIN-THRU.

       P3200-EXIT.
           EXIT.

       P3400-REPLACE-ROOT.
           CALL 'CBLTDLI' USING DLI-REPL
                                DB-PCB-BHST
                                WS-BHSTSEG-IO.
           MOVE DBP-STATUS-CODE TO WS-DLI-STATUS.
           IF WS-DLI-OK
               ADD 1 TO WS-HDR-REPL-CNT
           ELSE
               PERFORM P9500-DLI-ERROR THRU P9500-EXIT
           END-IF.

       P3400-EXIT.
           EXIT.

       P3600-INSERT-ROOT.
      * A HISAM ROOT INSERT THAT DOES NOT FIT IN THE PRIME AREA GOES
      * TO OVERFLOW.  THE DATABASE IS REORGANISED QUARTERLY - IF THE
      * REORG IS MISSED THE OVERFLOW CHAINS LENGTHEN AND THE ONLINE
      * ENQUIRY SLOWS DOWN, BUT NOTHING FAILS.
           CALL 'CBLTDLI' USING DLI-ISRT
                                DB-PCB-BHST
                                WS-BHSTSEG-IO
                                SSA-BHSTSEG-U.
           MOVE DBP-STATUS-CODE TO WS-DLI-STATUS.
           IF WS-DLI-OK
               ADD 1 TO WS-HDR-ISRT-CNT
               GO TO P3600-EXIT
           END-IF.
           IF WS-DLI-DUP-INSERT
               ADD 1 TO WS-CFWD-CNT
               GO TO P3600-EXIT
           END-IF.
           PERFORM P9500-DLI-ERROR THRU P9500-EXIT.

       P3600-EXIT.
           EXIT.

       P4000-ATTACH-DETAIL.
      * ATTACH EVERY DETAIL LINE THAT BELONGS TO THIS HEADER.  THE
      * TWO FILES ARE IN THE SAME KEY ORDER, SO THE DETAIL FILE IS
      * READ FORWARD UNTIL THE KEY CHANGES.
           MOVE 'P4000-ATTACH-DETAIL' TO WS-PARA-NAME.
           PERFORM P4200-INSERT-DETAIL THRU P4200-EXIT
               UNTIL WS-DTL-EOF
                  OR WD-BAN NOT = BH-BAN
                  OR WD-BILL-PERIOD NOT = BH-BILL-PERIOD.

       P4000-EXIT.
           EXIT.

       P4200-INSERT-DETAIL.
           MOVE SPACES TO WS-BHSTDTL-IO.
           MOVE WD-LINE-SEQ     TO BD-SEQ.
           MOVE WD-SECTION      TO BD-SECTION.
           MOVE WD-JURIS-CD     TO BD-JURIS-CD.
           MOVE WD-STATE-CD     TO BD-STATE-CD.
           MOVE SPACES          TO BD-RATE-ELEM.
           MOVE WD-DESCRIPTION  TO BD-DESCRIPTION.
           MOVE WD-TOT-MINUTES  TO BD-MINUTES.
           MOVE WD-TOT-ROUNDED  TO BD-AMOUNT.
           CALL 'CBLTDLI' USING DLI-ISRT
                                DB-PCB-BHST
                                WS-BHSTDTL-IO
                                SSA-BHSTSEG-Q
                                SSA-BHSTDTL-U.
           MOVE DBP-STATUS-CODE TO WS-DLI-STATUS.
           IF WS-DLI-OK
               ADD 1 TO WS-DTL-ISRT-CNT
           ELSE
               IF WS-DLI-DUP-INSERT
                   ADD 1 TO WS-SUMM-CNT
               ELSE
                   PERFORM P9500-DLI-ERROR THRU P9500-EXIT
               END-IF
           END-IF.
           PERFORM P2200-READ-DETAIL THRU P2200-EXIT.

       P4200-EXIT.
           EXIT.

      *****************************************************************
      * S500-VSAM-REGISTER                                            *
      *****************************************************************
       S500-VSAM-REGISTER SECTION.

       P5000-UPDATE-VSAM.
      * SECOND STORE.  THE VSAM HEADER MASTER IS WRITTEN OUTSIDE THE
      * CHECKPOINT SCOPE.  A CHECKPOINT ABANDONED AFTER THIS POINT
      * BACKS OUT THE IMS SEGMENTS AND LEAVES THE VSAM RECORD IN
      * PLACE, SO THE INVOICE NUMBER IS VISIBLE TO THE SETTLEMENT
      * STATEMENT WHILE THE HISTORY ROW IS NOT YET THERE.  THE
      * COMPARISON AT END OF RUN IN P9200 IS WHAT PICKS THAT UP.
           MOVE 'P5000-UPDATE-VSAM' TO WS-PARA-NAME.
           MOVE 'N' TO WS-VSAM-FOUND-SW.
           MOVE BH-BAN         TO HM-BAN.
           MOVE BH-BILL-PERIOD TO HM-BILL-PERIOD.
           READ HEADER-MASTER
               INVALID KEY
                   CONTINUE
           END-READ.
           IF WS-FS-OUTPUT = '00'
               MOVE 'Y' TO WS-VSAM-FOUND-SW
           END-IF.
           MOVE CABS-BILL-HEADER TO HEADER-MASTER-RECORD.
           MOVE BH-BAN         TO HM-BAN.
           MOVE BH-BILL-PERIOD TO HM-BILL-PERIOD.
           IF WS-VSAM-FOUND
               REWRITE HEADER-MASTER-RECORD
                   INVALID KEY
                       MOVE 3320 TO CT-RC
                       PERFORM P7000-SUSPEND THRU P7000-EXIT
               END-REWRITE
           ELSE
               WRITE HEADER-MASTER-RECORD
                   INVALID KEY
                       MOVE 3321 TO CT-RC
                       PERFORM P7000-SUSPEND THRU P7000-EXIT
               END-WRITE
           END-IF.
           IF WS-FS-OUTPUT = '00'
               ADD 1 TO WS-VSAM-CNT
           END-IF.

       P5000-EXIT.
           EXIT.

       P6500-CHECKPOINT.
           ADD 1 TO WS-SINCE-CHKP.
           IF WS-SINCE-CHKP < WP-COMMIT-INT
               GO TO P6500-EXIT
           END-IF.
           MOVE BS-KEY TO WS-CHKP-KEY.
           CALL 'CBLTDLI' USING DLI-CHKP
                                DB-PCB-BHST
                                WS-CHKP-ID.
           ADD 1 TO WS-CHKP-CNT.
           MOVE ZERO TO WS-SINCE-CHKP.

       P6500-EXIT.
           EXIT.

      *****************************************************************
      * S700-CONVERSION-AND-SUSPENSE                                  *
      *****************************************************************
       S700-SUPPORT SECTION.

       P7000-SUSPEND.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           IF SU-ERR-CODE = SPACES
               MOVE EC-BAN-UNKNOWN TO SU-ERR-CODE
           END-IF.
           SET SU-ERROR TO TRUE.
           MOVE WS-PGM-NAME  TO SU-DETECT-PGM.
           MOVE WS-PARA-NAME TO SU-DETECT-PARA.
           MOVE WP-RUN-ID    TO SU-RUN-ID.
           MOVE CABS-BILL-HEADER TO SU-ORIG-RECORD.
           MOVE CABS-SUSPENSE-RECORD TO SUSPENSE-OUT-RECORD.
           WRITE SUSPENSE-OUT-RECORD.

       P7000-EXIT.
           EXIT.

       P7700-BRIDGE-OLD-FORMAT.
      * THE SITES THAT WERE NOT CONVERTED IN 1998 STILL CUT THE
      * 133 BYTE HEADER.  THOSE RECORDS ARE PASSED THROUGH THE
      * CONVERSION MODULE BEFORE THEY ARE LOADED.  THE SWITCH ON
      * THE PARM CARD IS SET FOR THOSE SITES ONLY - SEE THE ENTRY
      * IN THE OPERATIONS RUNBOOK FOR THE REGIONS CONCERNED.
           MOVE 'P7700-BRIDGE-OLD-FORMAT' TO WS-PARA-NAME.
           MOVE SPACES TO WB-IN-AREA.
           MOVE HEADER-IN-RECORD TO WB-IN-AREA.
           MOVE 133 TO WB-IN-LEN.
           MOVE 200 TO WB-OUT-LEN.
           MOVE ZERO TO WB-RETURN.
           CALL 'CABLGCNV' USING WB-IN-LEN
                                 WB-IN-AREA
                                 WB-OUT-LEN
                                 WB-OUT-AREA
                                 WB-RETURN.
           MOVE WB-RETURN TO WS-BRIDGE-RC.
           IF WS-BRIDGE-RC = ZERO
               MOVE WB-OUT-AREA TO CABS-BILL-HEADER
               ADD 1 TO WS-BRIDGE-CNT
           ELSE
               ADD 1 TO WS-REJECT-CNT
               MOVE EC-DATE-INVALID TO SU-ERR-CODE
               PERFORM P7000-SUSPEND THRU P7000-EXIT
           END-IF.

       P7700-EXIT.
           EXIT.

      *****************************************************************
      * S800-CONTROL-AND-TERMINATION                                  *
      *****************************************************************
       S800-CONTROL SECTION.

       P8000-CONTROL.
           MOVE 'P8000-CONTROL' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-CONTROL-RECORD.
           MOVE WP-RUN-ID        TO CT-RUN-ID.
           MOVE WS-PGM-NAME      TO CT-PROCESS-ID.
           MOVE 040              TO CT-STEP-SEQ.
           MOVE WP-CYCLE-YYDDD   TO CT-CYCLE-YYDDD.
           MOVE WP-BILL-PERIOD   TO CT-BILL-PERIOD.
           MOVE ZERO             TO CT-RERUN-NBR.
           MOVE 'CABJ2400'       TO CT-JOBNAME.
           MOVE 'IMSSTEP'        TO CT-STEPNAME.
           MOVE WS-READ-CNT      TO CT-READ.
           MOVE WS-WRITE-CNT     TO CT-WRITTEN.
           MOVE WS-REJECT-CNT    TO CT-REJECTED.
           MOVE WS-SUMM-CNT      TO CT-SUMMARISED.
           MOVE WS-CFWD-CNT      TO CT-CARRIED-FWD.
           MOVE ZERO             TO CT-HASH-MINUTES.
           MOVE BH-HASH-AMOUNT   TO CT-HASH-AMOUNT.
           MOVE WS-ACC-SEQ-HASH  TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH  TO CT-HASH-OCN.
           COMPUTE WS-ACC-SEQ-HASH = CT-WRITTEN + CT-REJECTED
                                   + CT-SUMMARISED + CT-CARRIED-FWD.
           IF WS-ACC-SEQ-HASH = CT-READ
               SET CT-IN-BALANCE TO TRUE
           ELSE
               SET CT-OUT-OF-BAL TO TRUE
           END-IF.
           MOVE SPACES           TO CT-ABEND-CD.
           MOVE WS-CHKP-KEY      TO CT-RESTART-KEY.
           MOVE CABS-CONTROL-RECORD TO CONTROL-RECORD.
           WRITE CONTROL-RECORD.

       P8000-EXIT.
           EXIT.

       P9000-TERM.
           MOVE 'P9000-TERM' TO WS-PARA-NAME.
           PERFORM P9200-PRINT-TOTALS THRU P9200-EXIT.
           CLOSE PARM-FILE.
           CLOSE HEADER-IN.
           CLOSE DETAIL-IN.
           CLOSE HEADER-MASTER.
           CLOSE SUSPENSE-FILE.
           CLOSE CONTROL-FILE.
           CLOSE REPORT-FILE.

       P9000-EXIT.
           EXIT.

       P9200-PRINT-TOTALS.
      * A DIFFERENCE BETWEEN THE HISTORY ROOT COUNT AND THE VSAM
      * COUNT IS PRINTED HERE.  NO ACTION IS TAKEN ON IT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '1' TO PC-CC.
           MOVE 'CABCTL04' TO PC-COL-001-020.
           MOVE 'BILL HISTORY LOAD' TO PC-COL-021-060.
           MOVE CABS-PRINT-LINE TO REPORT-LINE.
           WRITE REPORT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '0' TO PC-CC.
           MOVE 'HISTORY ROOTS ISRT' TO PC-COL-001-020.
           MOVE WS-HDR-ISRT-CNT TO WS-ED-COUNT.
           MOVE WS-ED-COUNT TO PC-COL-021-060.
           MOVE CABS-PRINT-LINE TO REPORT-LINE.
           WRITE REPORT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'HISTORY ROOTS REPL' TO PC-COL-001-020.
           MOVE WS-HDR-REPL-CNT TO WS-ED-COUNT.
           MOVE WS-ED-COUNT TO PC-COL-021-060.
           MOVE CABS-PRINT-LINE TO REPORT-LINE.
           WRITE REPORT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DETAIL SEGMENTS' TO PC-COL-001-020.
           MOVE WS-DTL-ISRT-CNT TO WS-ED-COUNT.
           MOVE WS-ED-COUNT TO PC-COL-021-060.
           MOVE CABS-PRINT-LINE TO REPORT-LINE.
           WRITE REPORT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'VSAM REGISTER ROWS' TO PC-COL-001-020.
           MOVE WS-VSAM-CNT TO WS-ED-COUNT.
           MOVE WS-ED-COUNT TO PC-COL-021-060.
           MOVE CABS-PRINT-LINE TO REPORT-LINE.
           WRITE REPORT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'BRIDGED RECORDS' TO PC-COL-001-020.
           MOVE WS-BRIDGE-CNT TO WS-ED-COUNT.
           MOVE WS-ED-COUNT TO PC-COL-021-060.
           MOVE CABS-PRINT-LINE TO REPORT-LINE.
           WRITE REPORT-LINE.

       P9200-EXIT.
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
           MOVE 3310 TO CT-RC.
           MOVE 12 TO RETURN-CODE.
           SET WS-EOF TO TRUE.

       P9500-EXIT.
           EXIT.
