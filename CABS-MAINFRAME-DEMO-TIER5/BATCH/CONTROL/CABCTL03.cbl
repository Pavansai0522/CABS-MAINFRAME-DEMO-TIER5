       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABCTL03.
      *****************************************************************
      * CABCTL03 - CIRCUIT INVENTORY MAINTENANCE                      *
      * APPLICATION : CABS                                            *
      * COMPILER    : ENTERPRISE COBOL                                *
      * INPUTS      : IMS PCB 1  CABCIRDB  PSB CABCT03P  PROCOPT A    *
      *               CKTTRAN TELCABS.CABS.CIRCUIT.TRAN(0)   FB 120   *
      *               PARMIN  INSTREAM SYSIN PARM CARD        FB 080  *
      * OUTPUTS     : CKTAUD  TELCABS.CABS.CIRCUIT.AUDIT(+1) FB 200   *
      *               SUSOUT  TELCABS.CABS.USAGE.SUSPENSE(+1) FB 300  *
      *               RPTOUT  SYSOUT PRINT                    FBA 133 *
      * CONTROL     : CTLOUT                                  CABSCTL *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED               *
      *                       + CT-SUMMARISED + CT-CARRIED-FWD        *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      *                                                               *
      * APPLIES ADD, CHANGE, DISCONNECT AND MEET POINT TRANSACTIONS   *
      * AGAINST THE CIRCUIT DATABASE.  THE TRANSACTION FILE IS CUT    *
      * BY THE PROVISIONING SYSTEM AND ARRIVES DAILY.                 *
      *                                                               *
      * REVISION HISTORY                                              *
      *   V1.00  1990-11-06  R.T.WHEELER   INITIAL                    *
      *   V1.05  1995-06-13  D.OKONKWO     MEET POINT SEGMENT ADDED   *
      *   V1.09  2000-10-24  J.M.CASTILLO  DISCONNECT NO LONGER       *
      *                                    DELETES THE ROOT - THE     *
      *                                    SEGMENT IS FLAGGED INSTEAD *
      *   V2.00  2008-02-19  P.NAIR        TERM SEGMENT ADDED         *
      *   V2.02  2015-12-08  A.BUKOWSKI    AUDIT TRAIL WIDENED        *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PARM-FILE     ASSIGN TO PARMIN
                  FILE STATUS IS WS-FS-INPUT.
           SELECT TRAN-FILE     ASSIGN TO CKTTRAN
                  FILE STATUS IS WS-FS-INPUT.
           SELECT AUDIT-FILE    ASSIGN TO CKTAUD
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
       FD  TRAN-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  TRAN-IN-RECORD              PIC X(120).
       FD  AUDIT-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  AUDIT-OUT-RECORD            PIC X(200).
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
       01  WS-PGM-NAME                 PIC X(08) VALUE 'CABCTL03'.
       01  WS-PARA-NAME                PIC X(30) VALUE SPACES.
       COPY CABSWRK.
       COPY CABSCIRC.
       COPY CABSPRNT.
      *
       01  WS-DLI-FUNCTIONS.
           05  DLI-GU                  PIC X(04) VALUE 'GU  '.
           05  DLI-GHU                 PIC X(04) VALUE 'GHU '.
           05  DLI-GN                  PIC X(04) VALUE 'GN  '.
           05  DLI-GNP                 PIC X(04) VALUE 'GNP '.
           05  DLI-GHNP                PIC X(04) VALUE 'GHNP'.
           05  DLI-ISRT                PIC X(04) VALUE 'ISRT'.
           05  DLI-REPL                PIC X(04) VALUE 'REPL'.
           05  DLI-DLET                PIC X(04) VALUE 'DLET'.
           05  DLI-CHKP                PIC X(04) VALUE 'CHKP'.
      *
       01  SSA-CIRCSEG-Q.
           05  FILLER                  PIC X(08) VALUE 'CIRCSEG '.
           05  FILLER                  PIC X(01) VALUE '('.
           05  FILLER                  PIC X(08) VALUE 'CICKTID '.
           05  FILLER                  PIC X(02) VALUE ' ='.
           05  SSA-CI-CKTID            PIC X(20) VALUE SPACES.
           05  FILLER                  PIC X(01) VALUE ')'.
       01  SSA-CIRCSEG-U.
           05  FILLER                  PIC X(08) VALUE 'CIRCSEG '.
           05  FILLER                  PIC X(01) VALUE SPACE.
       01  SSA-CIRCMPB-Q.
           05  FILLER                  PIC X(08) VALUE 'CIRCMPB '.
           05  FILLER                  PIC X(01) VALUE '('.
           05  FILLER                  PIC X(08) VALUE 'CIMOCN  '.
           05  FILLER                  PIC X(02) VALUE ' ='.
           05  SSA-CM-OCN              PIC X(04) VALUE SPACES.
           05  FILLER                  PIC X(01) VALUE ')'.
       01  SSA-CIRCMPB-U.
           05  FILLER                  PIC X(08) VALUE 'CIRCMPB '.
           05  FILLER                  PIC X(01) VALUE SPACE.
       01  SSA-CIRCTERM-Q.
           05  FILLER                  PIC X(08) VALUE 'CIRCTERM'.
           05  FILLER                  PIC X(01) VALUE '('.
           05  FILLER                  PIC X(08) VALUE 'CITEFFDT'.
           05  FILLER                  PIC X(02) VALUE ' ='.
           05  SSA-CT-EFFDT            PIC 9(05) VALUE ZERO.
           05  FILLER                  PIC X(01) VALUE ')'.
       01  SSA-CIRCTERM-U.
           05  FILLER                  PIC X(08) VALUE 'CIRCTERM'.
           05  FILLER                  PIC X(01) VALUE SPACE.
      *
       01  WS-CIRCSEG-IO.
           05  CI-CKT-ID               PIC X(20).
           05  CI-TRUNK-GRP            PIC X(08).
           05  CI-OCN                  PIC X(04).
           05  CI-BAN                  PIC X(13).
           05  CI-USOC                 PIC X(05).
           05  CI-SVC-TYPE             PIC X(02).
           05  CI-A-CLLI               PIC X(11).
           05  CI-Z-CLLI               PIC X(11).
           05  CI-A-LATA               PIC 9(03).
           05  CI-Z-LATA               PIC 9(03).
           05  CI-STATE-CD             PIC X(02).
           05  CI-INSTALL-YYDDD        PIC 9(05).
           05  CI-DISC-YYDDD           PIC 9(05).
           05  CI-STATUS               PIC X(01).
               88  CI-ACTIVE           VALUE 'A'.
               88  CI-PENDING          VALUE 'P'.
               88  CI-DISCONNECTED     VALUE 'D'.
           05  CI-UPD-YYDDD            PIC 9(05).
           05  CI-UPD-PGM              PIC X(08).
           05  CI-FILLER               PIC X(94).
       01  WS-CIRCMPB-IO.
           05  CM-OTHER-OCN            PIC X(04).
           05  CM-OUR-PCT              PIC S9(03)V9(05) COMP-3.
           05  CM-OTHER-PCT            PIC S9(03)V9(05) COMP-3.
           05  CM-MPB-SW               PIC X(01).
           05  CM-EFF-YYDDD            PIC 9(05).
           05  CM-FILLER               PIC X(60).
       01  WS-CIRCTERM-IO.
           05  CT-EFF-YYDDD            PIC 9(05).
           05  CT-TERM-MONTHS          PIC 9(03).
           05  CT-DISC-CHG             PIC S9(09)V9(02) COMP-3.
           05  CT-RENEW-SW             PIC X(01).
           05  CT-FILLER               PIC X(55).
      *
       01  WS-TRAN-AREA.
           05  WT-ACTION               PIC X(01).
               88  WT-ADD              VALUE 'A'.
               88  WT-CHANGE           VALUE 'C'.
               88  WT-DISCONNECT       VALUE 'D'.
               88  WT-MEET-POINT       VALUE 'M'.
               88  WT-TERM             VALUE 'T'.
               88  WT-VALID-ACTION     VALUE 'A' 'C' 'D' 'M' 'T'.
           05  WT-CKT-ID               PIC X(20).
           05  WT-TRUNK-GRP            PIC X(08).
           05  WT-OCN                  PIC X(04).
           05  WT-BAN                  PIC X(13).
           05  WT-USOC                 PIC X(05).
           05  WT-SVC-TYPE             PIC X(02).
           05  WT-A-CLLI               PIC X(11).
           05  WT-Z-CLLI               PIC X(11).
           05  WT-A-LATA               PIC 9(03).
           05  WT-Z-LATA               PIC 9(03).
           05  WT-STATE-CD             PIC X(02).
           05  WT-EFF-YYDDD            PIC 9(05).
           05  WT-OTHER-OCN            PIC X(04).
           05  WT-PCT-X                PIC 9(08).
           05  WT-PCT-R REDEFINES WT-PCT-X.
               10  WT-PCT-WHOLE        PIC 9(03).
               10  WT-PCT-FRAC         PIC 9(05).
           05  WT-TERM-MONTHS          PIC 9(03).
           05  WT-FILLER               PIC X(15).
      *
       01  WS-AUDIT-AREA.
           05  WA-CKT-ID               PIC X(20).
           05  WA-ACTION               PIC X(01).
           05  WA-SEG-NAME             PIC X(08).
           05  WA-DLI-FUNC             PIC X(04).
           05  WA-STATUS               PIC X(02).
           05  WA-RUN-ID               PIC X(12).
           05  WA-YYDDD                PIC 9(05).
           05  WA-PGM                  PIC X(08).
           05  WA-BEFORE               PIC X(60).
           05  WA-AFTER                PIC X(60).
           05  WA-FILLER               PIC X(20).
      *
       01  WS-PARM-AREA.
           05  WP-RUN-ID               PIC X(12).
           05  WP-CYCLE-YYDDD          PIC 9(05).
           05  WP-BILL-PERIOD          PIC 9(06).
           05  WP-COMMIT-INT           PIC 9(04).
           05  WP-RESTART-KEY          PIC X(20).
           05  WP-FILLER               PIC X(33).
      *
       01  WS-SWITCHES-LOCAL.
           05  WS-ROOT-FOUND-SW        PIC X(01) VALUE 'N'.
               88  WS-ROOT-FOUND       VALUE 'Y'.
           05  WS-CHILD-FOUND-SW       PIC X(01) VALUE 'N'.
               88  WS-CHILD-FOUND      VALUE 'Y'.
           05  WS-TRAN-OK-SW           PIC X(01) VALUE 'Y'.
               88  WS-TRAN-OK          VALUE 'Y'.
       01  WS-DLI-STATUS               PIC X(02) VALUE SPACES.
           88  WS-DLI-OK               VALUE '  '.
           88  WS-DLI-NOT-FOUND        VALUE 'GE'.
           88  WS-DLI-DUP-INSERT       VALUE 'II'.
           88  WS-DLI-NO-MORE          VALUE 'GE' 'GB' 'GA' 'GK'.
       01  WS-LOCAL-COUNTS.
           05  WS-ADD-CNT              PIC S9(09) COMP-3 VALUE 0.
           05  WS-CHG-CNT              PIC S9(09) COMP-3 VALUE 0.
           05  WS-DSC-CNT              PIC S9(09) COMP-3 VALUE 0.
           05  WS-MPB-CNT              PIC S9(09) COMP-3 VALUE 0.
           05  WS-TRM-CNT              PIC S9(09) COMP-3 VALUE 0.
           05  WS-DLI-FAIL-CNT         PIC S9(09) COMP-3 VALUE 0.
           05  WS-SINCE-CHKP           PIC S9(05) COMP-3 VALUE 0.
           05  WS-CHKP-CNT             PIC S9(05) COMP-3 VALUE 0.
       01  WS-CHKP-AREA.
           05  WS-CHKP-ID              PIC X(08) VALUE 'CTL03000'.
           05  WS-CHKP-KEY             PIC X(20) VALUE SPACES.
       01  WS-WORK-PCT                 PIC S9(03)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-ED-COUNT                 PIC ZZZ,ZZZ,ZZ9.
      *
       LINKAGE SECTION.
       01  DB-PCB-CIRC.
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
       PROCEDURE DIVISION USING DB-PCB-CIRC.
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
           OPEN INPUT TRAN-FILE.
           OPEN OUTPUT AUDIT-FILE.
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
               MOVE 250 TO WP-COMMIT-INT
           END-IF.
           MOVE WP-RESTART-KEY TO WS-CHKP-KEY.
           PERFORM P2100-READ-TRAN THRU P2100-EXIT.

       P1000-EXIT.
           EXIT.

      *****************************************************************
      * S200-MAIN-PROCESS                                             *
      *****************************************************************
       S200-MAIN-PROCESS SECTION.

       P2000-PROCESS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           MOVE 'Y' TO WS-TRAN-OK-SW.
           MOVE SPACES TO WS-AUDIT-AREA.
           MOVE WT-CKT-ID TO WA-CKT-ID.
           MOVE WT-ACTION TO WA-ACTION.
           MOVE WP-RUN-ID TO WA-RUN-ID.
           MOVE WP-CYCLE-YYDDD TO WA-YYDDD.
           MOVE WS-PGM-NAME TO WA-PGM.
           IF NOT WT-VALID-ACTION
               ADD 1 TO WS-REJECT-CNT
               MOVE EC-CIRCUIT-UNKNOWN TO SU-ERR-CODE
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               PERFORM P2100-READ-TRAN THRU P2100-EXIT
               GO TO P2000-EXIT
           END-IF.
           IF WT-ADD
               PERFORM P3100-ADD-CIRCUIT THRU P3100-EXIT
           ELSE
               PERFORM P3200-POSITION-ROOT THRU P3200-EXIT
               IF WS-ROOT-FOUND
                   PERFORM P3300-DISPATCH-ACTION THRU P3300-EXIT
               ELSE
                   ADD 1 TO WS-REJECT-CNT
                   MOVE EC-CIRCUIT-UNKNOWN TO SU-ERR-CODE
                   PERFORM P7000-SUSPEND THRU P7000-EXIT
               END-IF
           END-IF.
           PERFORM P6000-WRITE-AUDIT THRU P6000-EXIT.
           PERFORM P6500-CHECKPOINT THRU P6500-EXIT.
           PERFORM P2100-READ-TRAN THRU P2100-EXIT.

       P2000-EXIT.
           EXIT.

       P2100-READ-TRAN.
           READ TRAN-FILE
               AT END
                   SET WS-EOF TO TRUE
                   GO TO P2100-EXIT
           END-READ.
           MOVE TRAN-IN-RECORD TO WS-TRAN-AREA.
           ADD 1 TO WS-READ-CNT.

       P2100-EXIT.
           EXIT.

      *****************************************************************
      * S300-DATABASE-MAINTENANCE                                     *
      *****************************************************************
       S300-DATABASE-MAINTENANCE SECTION.

       P3100-ADD-CIRCUIT.
      * BUILD AND INSERT THE ROOT.  A DUPLICATE CIRCUIT IDENTIFIER
      * IS TREATED AS A REISSUE - THE PROVISIONING SYSTEM SENDS THE
      * ADD AGAIN WHEN AN ORDER IS AMENDED BEFORE IT COMPLETES.
           MOVE 'P3100-ADD-CIRCUIT' TO WS-PARA-NAME.
           MOVE SPACES TO WS-CIRCSEG-IO.
           MOVE WT-CKT-ID     TO CI-CKT-ID.
           MOVE WT-TRUNK-GRP  TO CI-TRUNK-GRP.
           MOVE WT-OCN        TO CI-OCN.
           MOVE WT-BAN        TO CI-BAN.
           MOVE WT-USOC       TO CI-USOC.
           MOVE WT-SVC-TYPE   TO CI-SVC-TYPE.
           MOVE WT-A-CLLI     TO CI-A-CLLI.
           MOVE WT-Z-CLLI     TO CI-Z-CLLI.
           MOVE WT-A-LATA     TO CI-A-LATA.
           MOVE WT-Z-LATA     TO CI-Z-LATA.
           MOVE WT-STATE-CD   TO CI-STATE-CD.
           MOVE WT-EFF-YYDDD  TO CI-INSTALL-YYDDD.
           MOVE ZERO          TO CI-DISC-YYDDD.
           MOVE 'A'           TO CI-STATUS.
           MOVE WP-CYCLE-YYDDD TO CI-UPD-YYDDD.
           MOVE WS-PGM-NAME   TO CI-UPD-PGM.
           MOVE 'CIRCSEG'     TO WA-SEG-NAME.
           MOVE DLI-ISRT      TO WA-DLI-FUNC.
           CALL 'CBLTDLI' USING DLI-ISRT
                                DB-PCB-CIRC
                                WS-CIRCSEG-IO
                                SSA-CIRCSEG-U.
           MOVE DBP-STATUS-CODE TO WS-DLI-STATUS.
           MOVE WS-DLI-STATUS TO WA-STATUS.
           IF WS-DLI-OK
               ADD 1 TO WS-ADD-CNT
               ADD 1 TO WS-WRITE-CNT
               GO TO P3100-EXIT
           END-IF.
           IF WS-DLI-DUP-INSERT
               ADD 1 TO WS-CFWD-CNT
               GO TO P3100-EXIT
           END-IF.
           GO TO P9990-DLI-FAILURE.

       P3100-EXIT.
           EXIT.

       P3200-POSITION-ROOT.
           MOVE 'P3200-POSITION-ROOT' TO WS-PARA-NAME.
           MOVE 'N' TO WS-ROOT-FOUND-SW.
           MOVE WT-CKT-ID TO SSA-CI-CKTID.
           CALL 'CBLTDLI' USING DLI-GHU
                                DB-PCB-CIRC
                                WS-CIRCSEG-IO
                                SSA-CIRCSEG-Q.
           MOVE DBP-STATUS-CODE TO WS-DLI-STATUS.
           IF WS-DLI-OK
               MOVE 'Y' TO WS-ROOT-FOUND-SW
               MOVE CI-CKT-ID TO WA-BEFORE
               GO TO P3200-EXIT
           END-IF.
           IF WS-DLI-NOT-FOUND
               GO TO P3200-EXIT
           END-IF.
           GO TO P9990-DLI-FAILURE.

       P3200-EXIT.
           EXIT.

       P3300-DISPATCH-ACTION.
           IF WT-CHANGE
               PERFORM P3400-CHANGE-CIRCUIT THRU P3400-EXIT
               GO TO P3300-EXIT
           END-IF.
           IF WT-DISCONNECT
               PERFORM P3500-DISCONNECT THRU P3500-EXIT
               GO TO P3300-EXIT
           END-IF.
           IF WT-MEET-POINT
               PERFORM P4000-MEET-POINT THRU P4000-EXIT
               GO TO P3300-EXIT
           END-IF.
           IF WT-TERM
               PERFORM P4500-TERM-SEGMENT THRU P4500-EXIT
           END-IF.

       P3300-EXIT.
           EXIT.

       P3400-CHANGE-CIRCUIT.
      * ONLY THE FIELDS SUPPLIED ON THE TRANSACTION ARE REPLACED.
      * A SPACE OR ZERO MEANS LEAVE ALONE, WHICH IS WHY A CIRCUIT
      * CANNOT BE MOVED TO A BLANK TRUNK GROUP THROUGH THIS ROUTE.
           MOVE 'P3400-CHANGE-CIRCUIT' TO WS-PARA-NAME.
           IF WT-TRUNK-GRP NOT = SPACES
               MOVE WT-TRUNK-GRP TO CI-TRUNK-GRP
           END-IF.
           IF WT-OCN NOT = SPACES
               MOVE WT-OCN TO CI-OCN
           END-IF.
           IF WT-BAN NOT = SPACES
               MOVE WT-BAN TO CI-BAN
           END-IF.
           IF WT-USOC NOT = SPACES
               MOVE WT-USOC TO CI-USOC
           END-IF.
           IF WT-STATE-CD NOT = SPACES
               MOVE WT-STATE-CD TO CI-STATE-CD
           END-IF.
           MOVE WP-CYCLE-YYDDD TO CI-UPD-YYDDD.
           MOVE WS-PGM-NAME TO CI-UPD-PGM.
           MOVE 'CIRCSEG' TO WA-SEG-NAME.
           MOVE DLI-REPL TO WA-DLI-FUNC.
           MOVE CI-TRUNK-GRP TO WA-AFTER.
           CALL 'CBLTDLI' USING DLI-REPL
                                DB-PCB-CIRC
                                WS-CIRCSEG-IO.
           MOVE DBP-STATUS-CODE TO WS-DLI-STATUS.
           MOVE WS-DLI-STATUS TO WA-STATUS.
           IF WS-DLI-OK
               ADD 1 TO WS-CHG-CNT
               ADD 1 TO WS-WRITE-CNT
           ELSE
               GO TO P9990-DLI-FAILURE
           END-IF.

       P3400-EXIT.
           EXIT.

       P3500-DISCONNECT.
      * SINCE THE 2000 CHANGE A DISCONNECT FLAGS THE ROOT AND LEAVES
      * IT IN PLACE.  THE MEET POINT SEGMENTS ARE STILL DELETED
      * BECAUSE A DISCONNECTED CIRCUIT CANNOT BE JOINTLY BILLED AND
      * LEAVING THEM WOULD KEEP THE CIRCUIT IN THE MEET POINT
      * EXTRACT FOREVER.
           MOVE 'P3500-DISCONNECT' TO WS-PARA-NAME.
           MOVE 'D' TO CI-STATUS.
           MOVE WT-EFF-YYDDD TO CI-DISC-YYDDD.
           MOVE WP-CYCLE-YYDDD TO CI-UPD-YYDDD.
           MOVE WS-PGM-NAME TO CI-UPD-PGM.
           MOVE 'CIRCSEG' TO WA-SEG-NAME.
           MOVE DLI-REPL TO WA-DLI-FUNC.
           CALL 'CBLTDLI' USING DLI-REPL
                                DB-PCB-CIRC
                                WS-CIRCSEG-IO.
           MOVE DBP-STATUS-CODE TO WS-DLI-STATUS.
           MOVE WS-DLI-STATUS TO WA-STATUS.
           IF NOT WS-DLI-OK
               GO TO P9990-DLI-FAILURE
           END-IF.
           ADD 1 TO WS-DSC-CNT.
           ADD 1 TO WS-WRITE-CNT.
           PERFORM P3600-DELETE-MPB THRU P3600-EXIT.

       P3500-EXIT.
           EXIT.

       P3600-DELETE-MPB.
           MOVE 'P3600-DELETE-MPB' TO WS-PARA-NAME.
           CALL 'CBLTDLI' USING DLI-GHNP
                                DB-PCB-CIRC
                                WS-CIRCMPB-IO
                                SSA-CIRCMPB-U.
           MOVE DBP-STATUS-CODE TO WS-DLI-STATUS.
           IF WS-DLI-NO-MORE
               GO TO P3600-EXIT
           END-IF.
           IF NOT WS-DLI-OK
               GO TO P3600-EXIT
           END-IF.
           CALL 'CBLTDLI' USING DLI-DLET
                                DB-PCB-CIRC
                                WS-CIRCMPB-IO.
           MOVE DBP-STATUS-CODE TO WS-DLI-STATUS.
           IF WS-DLI-OK
               ADD 1 TO WS-SUMM-CNT
           END-IF.

       P3600-EXIT.
           EXIT.

      *****************************************************************
      * S400-DEPENDENT-SEGMENTS                                       *
      *****************************************************************
       S400-DEPENDENT-SEGMENTS SECTION.

       P4000-MEET-POINT.
      * THE TWO PERCENTAGES ARE FILED SEPARATELY BY THE TWO LECS AND
      * ARE NOT VALIDATED AGAINST EACH OTHER HERE.  THE VARIANCE IS
      * PICKED UP BY THE SETTLEMENT VALIDATION LATER IN THE MONTH.
           MOVE 'P4000-MEET-POINT' TO WS-PARA-NAME.
           COMPUTE WS-WORK-PCT = WT-PCT-WHOLE
                               + (WT-PCT-FRAC / 100000).
           MOVE WT-OTHER-OCN TO SSA-CM-OCN.
           MOVE 'CIRCMPB' TO WA-SEG-NAME.
           CALL 'CBLTDLI' USING DLI-GHNP
                                DB-PCB-CIRC
                                WS-CIRCMPB-IO
                                SSA-CIRCMPB-Q.
           MOVE DBP-STATUS-CODE TO WS-DLI-STATUS.
           IF WS-DLI-OK
               PERFORM P4200-REPL-MPB THRU P4200-EXIT
               GO TO P4000-EXIT
           END-IF.
           IF WS-DLI-NO-MORE
               PERFORM P4300-ISRT-MPB THRU P4300-EXIT
               GO TO P4000-EXIT
           END-IF.
           GO TO P9990-DLI-FAILURE.

       P4000-EXIT.
           EXIT.

       P4200-REPL-MPB.
           MOVE WS-WORK-PCT TO CM-OUR-PCT.
           COMPUTE CM-OTHER-PCT = 1.00000 - WS-WORK-PCT.
           MOVE 'Y' TO CM-MPB-SW.
           MOVE WT-EFF-YYDDD TO CM-EFF-YYDDD.
           MOVE DLI-REPL TO WA-DLI-FUNC.
           CALL 'CBLTDLI' USING DLI-REPL
                                DB-PCB-CIRC
                                WS-CIRCMPB-IO.
           MOVE DBP-STATUS-CODE TO WS-DLI-STATUS.
           MOVE WS-DLI-STATUS TO WA-STATUS.
           IF WS-DLI-OK
               ADD 1 TO WS-MPB-CNT
               ADD 1 TO WS-WRITE-CNT
           ELSE
               GO TO P9990-DLI-FAILURE
           END-IF.

       P4200-EXIT.
           EXIT.

       P4300-ISRT-MPB.
           MOVE SPACES TO WS-CIRCMPB-IO.
           MOVE WT-OTHER-OCN TO CM-OTHER-OCN.
           MOVE WS-WORK-PCT TO CM-OUR-PCT.
           COMPUTE CM-OTHER-PCT = 1.00000 - WS-WORK-PCT.
           MOVE 'Y' TO CM-MPB-SW.
           MOVE WT-EFF-YYDDD TO CM-EFF-YYDDD.
           MOVE DLI-ISRT TO WA-DLI-FUNC.
           CALL 'CBLTDLI' USING DLI-ISRT
                                DB-PCB-CIRC
                                WS-CIRCMPB-IO
                                SSA-CIRCSEG-Q
                                SSA-CIRCMPB-U.
           MOVE DBP-STATUS-CODE TO WS-DLI-STATUS.
           MOVE WS-DLI-STATUS TO WA-STATUS.
           IF WS-DLI-OK
               ADD 1 TO WS-MPB-CNT
               ADD 1 TO WS-WRITE-CNT
           ELSE
               GO TO P9990-DLI-FAILURE
           END-IF.

       P4300-EXIT.
           EXIT.

       P4500-TERM-SEGMENT.
      * TERM COMMITMENTS WERE PUT ON A SEGMENT OF THEIR OWN IN 2008
      * SO A CIRCUIT COULD CARRY MORE THAN ONE OVERLAPPING TERM.
      * IN PRACTICE EVERY CIRCUIT HAS EXACTLY ONE.
           MOVE 'P4500-TERM-SEGMENT' TO WS-PARA-NAME.
           MOVE WT-EFF-YYDDD TO SSA-CT-EFFDT.
           MOVE 'CIRCTERM' TO WA-SEG-NAME.
           MOVE SPACES TO WS-CIRCTERM-IO.
           MOVE WT-EFF-YYDDD   TO CT-EFF-YYDDD.
           MOVE WT-TERM-MONTHS TO CT-TERM-MONTHS.
           MOVE ZERO           TO CT-DISC-CHG.
           MOVE 'N'            TO CT-RENEW-SW.
           MOVE DLI-ISRT TO WA-DLI-FUNC.
           CALL 'CBLTDLI' USING DLI-ISRT
                                DB-PCB-CIRC
                                WS-CIRCTERM-IO
                                SSA-CIRCSEG-Q
                                SSA-CIRCTERM-U.
           MOVE DBP-STATUS-CODE TO WS-DLI-STATUS.
           MOVE WS-DLI-STATUS TO WA-STATUS.
           IF WS-DLI-OK
               ADD 1 TO WS-TRM-CNT
               ADD 1 TO WS-WRITE-CNT
               GO TO P4500-EXIT
           END-IF.
           IF WS-DLI-DUP-INSERT
               ADD 1 TO WS-CFWD-CNT
               GO TO P4500-EXIT
           END-IF.
           GO TO P9990-DLI-FAILURE.

       P4500-EXIT.
           EXIT.

      *****************************************************************
      * S600-OUTPUT-AND-CHECKPOINT                                    *
      *****************************************************************
       S600-OUTPUT SECTION.

       P6000-WRITE-AUDIT.
           MOVE WS-AUDIT-AREA TO AUDIT-OUT-RECORD.
           WRITE AUDIT-OUT-RECORD.

       P6000-EXIT.
           EXIT.

       P6500-CHECKPOINT.
           ADD 1 TO WS-SINCE-CHKP.
           IF WS-SINCE-CHKP < WP-COMMIT-INT
               GO TO P6500-EXIT
           END-IF.
           MOVE WT-CKT-ID TO WS-CHKP-KEY.
           CALL 'CBLTDLI' USING DLI-CHKP
                                DB-PCB-CIRC
                                WS-CHKP-ID.
           ADD 1 TO WS-CHKP-CNT.
           MOVE ZERO TO WS-SINCE-CHKP.

       P6500-EXIT.
           EXIT.

      *****************************************************************
      * S700-SUSPENSE                                                 *
      *****************************************************************
       S700-SUSPENSE SECTION.

       P7000-SUSPEND.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           IF SU-ERR-CODE = SPACES
               MOVE EC-CIRCUIT-UNKNOWN TO SU-ERR-CODE
           END-IF.
           SET SU-ERROR TO TRUE.
           MOVE WS-PGM-NAME  TO SU-DETECT-PGM.
           MOVE WS-PARA-NAME TO SU-DETECT-PARA.
           MOVE WP-RUN-ID    TO SU-RUN-ID.
           MOVE WS-TRAN-AREA TO SU-ORIG-RECORD.
           MOVE CABS-SUSPENSE-RECORD TO SUSPENSE-OUT-RECORD.
           WRITE SUSPENSE-OUT-RECORD.

       P7000-EXIT.
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
           MOVE 030              TO CT-STEP-SEQ.
           MOVE WP-CYCLE-YYDDD   TO CT-CYCLE-YYDDD.
           MOVE WP-BILL-PERIOD   TO CT-BILL-PERIOD.
           MOVE ZERO             TO CT-RERUN-NBR.
           MOVE 'CABJ2300'       TO CT-JOBNAME.
           MOVE 'IMSSTEP'        TO CT-STEPNAME.
           MOVE WS-READ-CNT      TO CT-READ.
           MOVE WS-WRITE-CNT     TO CT-WRITTEN.
           MOVE WS-REJECT-CNT    TO CT-REJECTED.
           MOVE WS-SUMM-CNT      TO CT-SUMMARISED.
           MOVE WS-CFWD-CNT      TO CT-CARRIED-FWD.
           MOVE ZERO             TO CT-HASH-MINUTES.
           MOVE ZERO             TO CT-HASH-AMOUNT.
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
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '1' TO PC-CC.
           MOVE 'CABCTL03' TO PC-COL-001-020.
           MOVE 'CIRCUIT INVENTORY MAINTENANCE' TO PC-COL-021-060.
           MOVE CABS-PRINT-LINE TO REPORT-LINE.
           WRITE REPORT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '0' TO PC-CC.
           MOVE 'ADDS' TO PC-COL-001-020.
           MOVE WS-ADD-CNT TO WS-ED-COUNT.
           MOVE WS-ED-COUNT TO PC-COL-021-060.
           MOVE CABS-PRINT-LINE TO REPORT-LINE.
           WRITE REPORT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'CHANGES' TO PC-COL-001-020.
           MOVE WS-CHG-CNT TO WS-ED-COUNT.
           MOVE WS-ED-COUNT TO PC-COL-021-060.
           MOVE CABS-PRINT-LINE TO REPORT-LINE.
           WRITE REPORT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DISCONNECTS' TO PC-COL-001-020.
           MOVE WS-DSC-CNT TO WS-ED-COUNT.
           MOVE WS-ED-COUNT TO PC-COL-021-060.
           MOVE CABS-PRINT-LINE TO REPORT-LINE.
           WRITE REPORT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'MEET POINT ROWS' TO PC-COL-001-020.
           MOVE WS-MPB-CNT TO WS-ED-COUNT.
           MOVE WS-ED-COUNT TO PC-COL-021-060.
           MOVE CABS-PRINT-LINE TO REPORT-LINE.
           WRITE REPORT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DLI FAILURES' TO PC-COL-001-020.
           MOVE WS-DLI-FAIL-CNT TO WS-ED-COUNT.
           MOVE WS-ED-COUNT TO PC-COL-021-060.
           MOVE CABS-PRINT-LINE TO REPORT-LINE.
           WRITE REPORT-LINE.
           CLOSE PARM-FILE.
           CLOSE TRAN-FILE.
           CLOSE AUDIT-FILE.
           CLOSE SUSPENSE-FILE.
           CLOSE CONTROL-FILE.
           CLOSE REPORT-FILE.

       P9000-EXIT.
           EXIT.

       P9990-DLI-FAILURE.
      * COMMON DATABASE FAILURE HANDLING.  SEE CABS-STD-036.
      * THE TRANSACTION IS SUSPENDED WITH THE STATUS CODE, THE AUDIT
      * ROW IS CUT SO THE ATTEMPT IS ON THE TRAIL, AND A CHECKPOINT
      * IS FORCED SO THE WORK DONE UP TO THIS POINT IS COMMITTED
      * BEFORE THE NEXT TRANSACTION IS READ.
           MOVE 'P9990-DLI-FAILURE' TO WS-PARA-NAME.
           ADD 1 TO WS-DLI-FAIL-CNT.
           ADD 1 TO WS-REJECT-CNT.
           MOVE EC-OUT-OF-BALANCE TO SU-ERR-CODE.
           PERFORM P7000-SUSPEND THRU P7000-EXIT.
           MOVE WS-DLI-STATUS TO WA-STATUS.
           PERFORM P6000-WRITE-AUDIT THRU P6000-EXIT.
           MOVE WT-CKT-ID TO WS-CHKP-KEY.
           CALL 'CBLTDLI' USING DLI-CHKP
                                DB-PCB-CIRC
                                WS-CHKP-ID.
           ADD 1 TO WS-CHKP-CNT.
           MOVE ZERO TO WS-SINCE-CHKP.
           IF WS-DLI-FAIL-CNT > 100
               MOVE 3210 TO CT-RC
               MOVE 12 TO RETURN-CODE
               SET WS-EOF TO TRUE
           END-IF.
           PERFORM P2100-READ-TRAN THRU P2100-EXIT.
           GO TO P2000-EXIT.
