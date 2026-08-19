       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABCTL02.
      *****************************************************************
      * CABCTL02 - QUARTERLY FACTOR APPLICATION TO THE CARRIER        *
      *            PROFILE AND THE FACTOR MASTER                      *
      * APPLICATION : CABS                                            *
      * COMPILER    : ENTERPRISE COBOL                                *
      * INPUTS      : IMS PCB 1  CABCARDB  PSB CABCT02P  PROCOPT A    *
      *               FCTRIN  TELCABS.CABS.FACTOR.QTRLY(0)   FB 100   *
      *               PARMIN  INSTREAM SYSIN PARM CARD        FB 080  *
      * OUTPUTS     : FCTRMST TELCABS.CABS.FACTOR      VSAM KSDS      *
      *               SUSOUT  TELCABS.CABS.USAGE.SUSPENSE(+1) FB 300  *
      *               RPTOUT  SYSOUT PRINT                    FBA 133 *
      * CONTROL     : CTLOUT                                  CABSCTL *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED               *
      *                       + CT-SUMMARISED + CT-CARRIED-FWD        *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      *                                                               *
      * FACTORS ARRIVE QUARTERLY FROM THE CARRIERS.  THEY ARE HELD    *
      * IN TWO PLACES - THE CARRFACT SEGMENT ON THE IMS CARRIER       *
      * PROFILE, WHICH THE ONLINE ENQUIRY READS, AND THE VSAM FACTOR  *
      * MASTER, WHICH THE JURISDICTIONAL SPLIT READS.  BOTH MUST BE   *
      * APPLIED FOR THE QUARTER TO BE CONSIDERED LOADED.              *
      *                                                               *
      * REVISION HISTORY                                              *
      *   V1.00  1992-07-14  D.OKONKWO     INITIAL                    *
      *   V1.03  1996-04-02  J.M.CASTILLO  PRIOR FACTORS CARRIED SO   *
      *                                    THE RESTATEMENT CAN SEE    *
      *                                    WHAT CHANGED               *
      *   V1.07  2001-01-29  P.NAIR        RANGE EDIT TIGHTENED       *
      *   V2.00  2007-08-16  A.BUKOWSKI    TWO PHASE COMMIT ADDED     *
      *                                    ACROSS IMS AND VSAM        *
      *   V2.02  2014-11-05  L.FERREIRA    COMMIT INTERVAL FROM PARM  *
      *   V2.03  2019-03-21  M.HAAS        RECOMPILE ONLY - IMS V14   *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PARM-FILE     ASSIGN TO PARMIN
                  FILE STATUS IS WS-FS-INPUT.
           SELECT FACTOR-IN     ASSIGN TO FCTRIN
                  FILE STATUS IS WS-FS-INPUT.
           SELECT FACTOR-MASTER ASSIGN TO FCTRMST
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS FM-KEY
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
       FD  FACTOR-IN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  FACTOR-IN-RECORD            PIC X(100).
       FD  FACTOR-MASTER
           RECORD CONTAINS 100 CHARACTERS.
       01  FACTOR-MASTER-RECORD.
           05  FM-KEY.
               10  FM-OCN              PIC X(04).
               10  FM-STATE-CD         PIC X(02).
               10  FM-LATA             PIC 9(03).
               10  FM-EFF-YYDDD        PIC 9(05).
           05  FM-REST                 PIC X(86).
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
       01  WS-PGM-NAME                 PIC X(08) VALUE 'CABCTL02'.
       01  WS-PARA-NAME                PIC X(30) VALUE SPACES.
       COPY CABSWRK.
       COPY CABSFCTR.
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
       01  SSA-CARRSEG-Q.
           05  FILLER                  PIC X(08) VALUE 'CARRSEG '.
           05  FILLER                  PIC X(01) VALUE '('.
           05  FILLER                  PIC X(08) VALUE 'CROCN   '.
           05  FILLER                  PIC X(02) VALUE ' ='.
           05  SSA-CS-OCN              PIC X(04) VALUE SPACES.
           05  FILLER                  PIC X(01) VALUE ')'.
       01  SSA-CARRFACT-Q.
           05  FILLER                  PIC X(08) VALUE 'CARRFACT'.
           05  FILLER                  PIC X(01) VALUE '('.
           05  FILLER                  PIC X(08) VALUE 'CRFEFFDT'.
           05  FILLER                  PIC X(02) VALUE ' ='.
           05  SSA-CF-EFFDT            PIC 9(05) VALUE ZERO.
           05  FILLER                  PIC X(01) VALUE ')'.
       01  SSA-CARRFACT-U.
           05  FILLER                  PIC X(08) VALUE 'CARRFACT'.
           05  FILLER                  PIC X(01) VALUE SPACE.
      *
       01  WS-CARRSEG-IO.
           05  CS-OCN                  PIC X(04).
           05  CS-NAME                 PIC X(40).
           05  CS-ACNA                 PIC X(03).
           05  CS-TYPE                 PIC X(01).
           05  CS-CIC                  PIC 9(04).
           05  CS-PARENT-OCN           PIC X(04).
           05  CS-STATUS               PIC X(01).
           05  CS-EFF-YYDDD            PIC 9(05).
           05  CS-EXP-YYDDD            PIC 9(05).
           05  CS-REST                 PIC X(113).
       01  WS-CARRFACT-IO.
           05  CF-EFF-YYDDD            PIC 9(05).
           05  CF-STATE-CD             PIC X(02).
           05  CF-LATA                 PIC 9(03).
           05  CF-PIU                  PIC S9(03)V9(05) COMP-3.
           05  CF-PLU                  PIC S9(03)V9(05) COMP-3.
           05  CF-PSU                  PIC S9(03)V9(05) COMP-3.
           05  CF-SOURCE               PIC X(01).
           05  CF-RESTATE-SW           PIC X(01).
           05  CF-RECV-YYDDD           PIC 9(05).
           05  CF-PRIOR-PIU            PIC S9(03)V9(05) COMP-3.
           05  CF-PRIOR-PLU            PIC S9(03)V9(05) COMP-3.
           05  CF-UPD-YYDDD            PIC 9(05).
           05  CF-UPD-PGM              PIC X(08).
           05  CF-FILLER               PIC X(32).
      *
      * THE INBOUND QUARTERLY CARD.
      *
       01  WS-FACTOR-CARD.
           05  WF-OCN                  PIC X(04).
           05  WF-STATE-CD             PIC X(02).
           05  WF-LATA                 PIC 9(03).
           05  WF-EFF-YYDDD            PIC 9(05).
           05  WF-PIU-X                PIC 9(08).
           05  WF-PIU-R REDEFINES WF-PIU-X.
               10  WF-PIU-WHOLE        PIC 9(03).
               10  WF-PIU-FRAC         PIC 9(05).
           05  WF-PLU-X                PIC 9(08).
           05  WF-PLU-R REDEFINES WF-PLU-X.
               10  WF-PLU-WHOLE        PIC 9(03).
               10  WF-PLU-FRAC         PIC 9(05).
           05  WF-PSU-X                PIC 9(08).
           05  WF-SOURCE               PIC X(01).
           05  WF-RESTATE-SW           PIC X(01).
           05  WF-RECV-YYDDD           PIC 9(05).
           05  WF-FILLER               PIC X(45).
      *
       01  WS-PARM-AREA.
           05  WP-RUN-ID               PIC X(12).
           05  WP-CYCLE-YYDDD          PIC 9(05).
           05  WP-BILL-PERIOD          PIC 9(06).
           05  WP-QUARTER-YYDDD        PIC 9(05).
           05  WP-COMMIT-INT           PIC 9(04).
           05  WP-RESTART-KEY          PIC X(14).
           05  WP-FILLER               PIC X(34).
      *
       01  WS-WORK-FACTORS.
           05  WW-PIU                  PIC S9(03)V9(05) COMP-3
                                                  VALUE 0.
           05  WW-PLU                  PIC S9(03)V9(05) COMP-3
                                                  VALUE 0.
           05  WW-PSU                  PIC S9(03)V9(05) COMP-3
                                                  VALUE 0.
           05  WW-SUM-CHECK            PIC S9(04)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-SWITCHES-LOCAL.
           05  WS-IMS-DONE-SW          PIC X(01) VALUE 'N'.
               88  WS-IMS-DONE         VALUE 'Y'.
           05  WS-VSAM-FOUND-SW        PIC X(01) VALUE 'N'.
               88  WS-VSAM-FOUND       VALUE 'Y'.
           05  WS-SEG-FOUND-SW         PIC X(01) VALUE 'N'.
               88  WS-SEG-FOUND        VALUE 'Y'.
           05  WS-EDIT-OK-SW           PIC X(01) VALUE 'Y'.
               88  WS-EDIT-OK          VALUE 'Y'.
       01  WS-DLI-STATUS               PIC X(02) VALUE SPACES.
           88  WS-DLI-OK               VALUE '  '.
           88  WS-DLI-NOT-FOUND        VALUE 'GE'.
           88  WS-DLI-DUP-INSERT       VALUE 'II'.
           88  WS-DLI-NO-MORE          VALUE 'GE' 'GB' 'GA' 'GK'.
       01  WS-LOCAL-COUNTS.
           05  WS-IMS-REPL-CNT         PIC S9(09) COMP-3 VALUE 0.
           05  WS-IMS-ISRT-CNT         PIC S9(09) COMP-3 VALUE 0.
           05  WS-VSAM-REWR-CNT        PIC S9(09) COMP-3 VALUE 0.
           05  WS-VSAM-WRIT-CNT        PIC S9(09) COMP-3 VALUE 0.
           05  WS-CHKP-CNT             PIC S9(05) COMP-3 VALUE 0.
           05  WS-SINCE-CHKP           PIC S9(05) COMP-3 VALUE 0.
           05  WS-EDIT-FAIL-CNT        PIC S9(09) COMP-3 VALUE 0.
           05  WS-NO-CARRIER-CNT       PIC S9(09) COMP-3 VALUE 0.
       01  WS-CHKP-AREA.
           05  WS-CHKP-ID              PIC X(08) VALUE 'CTL02000'.
           05  WS-CHKP-KEY.
               10  WS-CK-OCN           PIC X(04) VALUE SPACES.
               10  WS-CK-STATE         PIC X(02) VALUE SPACES.
               10  WS-CK-LATA          PIC 9(03) VALUE ZERO.
               10  WS-CK-EFF-YYDDD     PIC 9(05) VALUE ZERO.
       01  WS-EDIT-FIELDS.
           05  WS-ED-COUNT             PIC ZZZ,ZZZ,ZZ9.
           05  WS-ED-FACTOR            PIC ZZ9.99999.
      *
       LINKAGE SECTION.
       01  DB-PCB-CARR.
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
       PROCEDURE DIVISION USING DB-PCB-CARR.
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
           OPEN INPUT FACTOR-IN.
           OPEN I-O FACTOR-MASTER.
           OPEN OUTPUT SUSPENSE-FILE.
           OPEN OUTPUT CONTROL-FILE.
           OPEN OUTPUT REPORT-FILE.
           PERFORM P1200-READ-PARM THRU P1200-EXIT.
           IF WP-COMMIT-INT = ZERO
               MOVE 500 TO WP-COMMIT-INT
           END-IF.
           PERFORM P2100-READ-FACTOR THRU P2100-EXIT.

       P1000-EXIT.
           EXIT.

       P1200-READ-PARM.
           MOVE SPACES TO PARM-CARD.
           READ PARM-FILE
               AT END
                   MOVE 8 TO RETURN-CODE
                   GOBACK
           END-READ.
           MOVE PARM-CARD TO WS-PARM-AREA.
           IF WP-RESTART-KEY NOT = SPACES
               SET WS-RESTARTING TO TRUE
               MOVE WP-RESTART-KEY TO WS-CHKP-KEY
           END-IF.

       P1200-EXIT.
           EXIT.

      *****************************************************************
      * S200-MAIN-PROCESS                                             *
      *****************************************************************
       S200-MAIN-PROCESS SECTION.

       P2000-PROCESS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2400-EDIT-CARD THRU P2400-EXIT.
           IF NOT WS-EDIT-OK
               ADD 1 TO WS-REJECT-CNT
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               PERFORM P2100-READ-FACTOR THRU P2100-EXIT
               GO TO P2000-EXIT
           END-IF.
           PERFORM P3000-POSITION-CARRIER THRU P3000-EXIT.
           IF NOT WS-SEG-FOUND
               ADD 1 TO WS-NO-CARRIER-CNT
               ADD 1 TO WS-REJECT-CNT
               MOVE EC-OCN-UNKNOWN TO SU-ERR-CODE
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               PERFORM P2100-READ-FACTOR THRU P2100-EXIT
               GO TO P2000-EXIT
           END-IF.
           PERFORM P4000-UPDATE-IMS THRU P4000-EXIT.
           PERFORM P5000-UPDATE-VSAM THRU P5000-EXIT.
           ADD 1 TO WS-WRITE-CNT.
           PERFORM P6000-CHECKPOINT THRU P6000-EXIT.
           PERFORM P2100-READ-FACTOR THRU P2100-EXIT.

       P2000-EXIT.
           EXIT.

       P2100-READ-FACTOR.
           READ FACTOR-IN
               AT END
                   SET WS-EOF TO TRUE
                   GO TO P2100-EXIT
           END-READ.
           MOVE FACTOR-IN-RECORD TO WS-FACTOR-CARD.
           ADD 1 TO WS-READ-CNT.

       P2100-EXIT.
           EXIT.

       P2400-EDIT-CARD.
      * BOTH FACTORS MUST FALL BETWEEN ZERO AND ONE.  THE TWO ARE
      * ALSO EXPECTED NOT TO EXCEED ONE WHEN ADDED - A CARRIER
      * CANNOT CLAIM MORE THAN ALL OF ITS TRAFFIC IS INTERSTATE AND
      * ALL OF IT IS LOCAL AT THE SAME TIME.  A CARD THAT BREAKS THE
      * SUM TEST IS STILL LOADED, WITH A WARNING, BECAUSE THE 2001
      * REVIEW FOUND SEVERAL LEGITIMATE CASES ON JOINTLY PROVISIONED
      * TRUNK GROUPS.
           MOVE 'P2400-EDIT-CARD' TO WS-PARA-NAME.
           MOVE 'Y' TO WS-EDIT-OK-SW.
           MOVE SPACES TO SU-ERR-CODE.
           IF WF-OCN = SPACES
               MOVE 'N' TO WS-EDIT-OK-SW
               MOVE EC-OCN-UNKNOWN TO SU-ERR-CODE
               GO TO P2400-EXIT
           END-IF.
           COMPUTE WW-PIU = WF-PIU-WHOLE + (WF-PIU-FRAC / 100000).
           COMPUTE WW-PLU = WF-PLU-WHOLE + (WF-PLU-FRAC / 100000).
           MOVE ZERO TO WW-PSU.
           IF WW-PIU < ZERO OR WW-PIU > 1.00000
               MOVE 'N' TO WS-EDIT-OK-SW
               MOVE EC-PIU-OUT-OF-RANGE TO SU-ERR-CODE
               ADD 1 TO WS-EDIT-FAIL-CNT
               GO TO P2400-EXIT
           END-IF.
           IF WW-PLU < ZERO OR WW-PLU > 1.00000
               MOVE 'N' TO WS-EDIT-OK-SW
               MOVE EC-PIU-OUT-OF-RANGE TO SU-ERR-CODE
               ADD 1 TO WS-EDIT-FAIL-CNT
               GO TO P2400-EXIT
           END-IF.
           COMPUTE WW-SUM-CHECK = WW-PIU + WW-PLU.
           IF WW-SUM-CHECK > 1.00000
               ADD 1 TO WS-SUMM-CNT
           END-IF.
           IF WF-EFF-YYDDD = ZERO
               MOVE 'N' TO WS-EDIT-OK-SW
               MOVE EC-DATE-INVALID TO SU-ERR-CODE
           END-IF.

       P2400-EXIT.
           EXIT.

      *****************************************************************
      * S300-IMS-ACCESS                                               *
      *****************************************************************
       S300-IMS-ACCESS SECTION.

       P3000-POSITION-CARRIER.
      * GHU ON THE ROOT ESTABLISHES POSITION AND HOLDS IT FOR THE
      * DEPENDENT UPDATE.  A GU WOULD POSITION BUT WOULD NOT ALLOW
      * THE REPL THAT FOLLOWS.
           MOVE 'P3000-POSITION-CARRIER' TO WS-PARA-NAME.
           MOVE 'N' TO WS-SEG-FOUND-SW.
           MOVE WF-OCN TO SSA-CS-OCN.
           CALL 'CBLTDLI' USING DLI-GHU
                                DB-PCB-CARR
                                WS-CARRSEG-IO
                                SSA-CARRSEG-Q.
           MOVE DBP-STATUS-CODE TO WS-DLI-STATUS.
           IF WS-DLI-OK
               MOVE 'Y' TO WS-SEG-FOUND-SW
               GO TO P3000-EXIT
           END-IF.
           IF WS-DLI-NOT-FOUND
               GO TO P3000-EXIT
           END-IF.
           PERFORM P9500-DLI-ERROR THRU P9500-EXIT.

       P3000-EXIT.
           EXIT.

       P4000-UPDATE-IMS.
      * THIS IS THE FIRST OF THE TWO STORES.  THE FACTOR SEGMENT IS
      * REPLACED IF IT IS ALREADY THERE FOR THIS EFFECTIVE DATE AND
      * INSERTED IF IT IS NOT.  THE PRIOR VALUES ARE CARRIED SO THE
      * RESTATEMENT PROCESS CAN SEE WHAT CHANGED.
           MOVE 'P4000-UPDATE-IMS' TO WS-PARA-NAME.
           MOVE WF-EFF-YYDDD TO SSA-CF-EFFDT.
           CALL 'CBLTDLI' USING DLI-GHNP
                                DB-PCB-CARR
                                WS-CARRFACT-IO
                                SSA-CARRFACT-Q.
           MOVE DBP-STATUS-CODE TO WS-DLI-STATUS.
           IF WS-DLI-OK
               PERFORM P4200-REPLACE-SEGMENT THRU P4200-EXIT
               GO TO P4000-EXIT
           END-IF.
           IF WS-DLI-NO-MORE
               PERFORM P4400-INSERT-SEGMENT THRU P4400-EXIT
               GO TO P4000-EXIT
           END-IF.
           PERFORM P9500-DLI-ERROR THRU P9500-EXIT.

       P4000-EXIT.
           EXIT.

       P4200-REPLACE-SEGMENT.
           MOVE CF-PIU        TO CF-PRIOR-PIU.
           MOVE CF-PLU        TO CF-PRIOR-PLU.
           MOVE WW-PIU        TO CF-PIU.
           MOVE WW-PLU        TO CF-PLU.
           MOVE WW-PSU        TO CF-PSU.
           MOVE WF-SOURCE     TO CF-SOURCE.
           MOVE WF-RESTATE-SW TO CF-RESTATE-SW.
           MOVE WF-RECV-YYDDD TO CF-RECV-YYDDD.
           MOVE WP-CYCLE-YYDDD TO CF-UPD-YYDDD.
           MOVE WS-PGM-NAME   TO CF-UPD-PGM.
           CALL 'CBLTDLI' USING DLI-REPL
                                DB-PCB-CARR
                                WS-CARRFACT-IO.
           MOVE DBP-STATUS-CODE TO WS-DLI-STATUS.
           IF WS-DLI-OK
               ADD 1 TO WS-IMS-REPL-CNT
           ELSE
               PERFORM P9500-DLI-ERROR THRU P9500-EXIT
           END-IF.

       P4200-EXIT.
           EXIT.

       P4400-INSERT-SEGMENT.
           MOVE SPACES TO WS-CARRFACT-IO.
           MOVE WF-EFF-YYDDD  TO CF-EFF-YYDDD.
           MOVE WF-STATE-CD   TO CF-STATE-CD.
           MOVE WF-LATA       TO CF-LATA.
           MOVE WW-PIU        TO CF-PIU.
           MOVE WW-PLU        TO CF-PLU.
           MOVE WW-PSU        TO CF-PSU.
           MOVE WF-SOURCE     TO CF-SOURCE.
           MOVE WF-RESTATE-SW TO CF-RESTATE-SW.
           MOVE WF-RECV-YYDDD TO CF-RECV-YYDDD.
           MOVE ZERO          TO CF-PRIOR-PIU.
           MOVE ZERO          TO CF-PRIOR-PLU.
           MOVE WP-CYCLE-YYDDD TO CF-UPD-YYDDD.
           MOVE WS-PGM-NAME   TO CF-UPD-PGM.
           CALL 'CBLTDLI' USING DLI-ISRT
                                DB-PCB-CARR
                                WS-CARRFACT-IO
                                SSA-CARRFACT-U.
           MOVE DBP-STATUS-CODE TO WS-DLI-STATUS.
           IF WS-DLI-OK
               ADD 1 TO WS-IMS-ISRT-CNT
               GO TO P4400-EXIT
           END-IF.
           IF WS-DLI-DUP-INSERT
               ADD 1 TO WS-CFWD-CNT
               GO TO P4400-EXIT
           END-IF.
           PERFORM P9500-DLI-ERROR THRU P9500-EXIT.

       P4400-EXIT.
           EXIT.

      *****************************************************************
      * S500-VSAM-ACCESS                                              *
      *****************************************************************
       S500-VSAM-ACCESS SECTION.

       P5000-UPDATE-VSAM.
      * THIS IS THE SECOND STORE.  THE VSAM WRITE IS HARDENED WHEN
      * IT IS ISSUED.  THE IMS UPDATE ABOVE IS INSIDE THE UNIT OF
      * WORK THAT THE CHECKPOINT IN P6000 COMMITS.  THE 2007 CHANGE
      * NOTE RECORDS THAT TWO PHASE COMMIT WAS PUT IN ACROSS THE
      * TWO.  WHAT WAS PUT IN WAS THE CHECKPOINT CALL - THE VSAM
      * SIDE IS NOT PART OF ITS SCOPE AND IS NOT BACKED OUT WHEN THE
      * CHECKPOINT IS ABANDONED.
           MOVE 'P5000-UPDATE-VSAM' TO WS-PARA-NAME.
           MOVE SPACES TO CABS-FACTOR-RECORD.
           MOVE 'N' TO WS-VSAM-FOUND-SW.
           MOVE WF-OCN       TO FM-OCN.
           MOVE WF-STATE-CD  TO FM-STATE-CD.
           MOVE WF-LATA      TO FM-LATA.
           MOVE WF-EFF-YYDDD TO FM-EFF-YYDDD.
           READ FACTOR-MASTER
               INVALID KEY
                   CONTINUE
           END-READ.
           IF WS-FS-OUTPUT = '00'
               MOVE 'Y' TO WS-VSAM-FOUND-SW
               MOVE FACTOR-MASTER-RECORD TO CABS-FACTOR-RECORD
           END-IF.
           MOVE WF-OCN        TO FC-OCN.
           MOVE WF-STATE-CD   TO FC-STATE-CD.
           MOVE WF-LATA       TO FC-LATA.
           MOVE WF-EFF-YYDDD  TO FC-EFF-YYDDD.
           PERFORM P5200-BUILD-VSAM THRU P5200-EXIT.
           IF WS-VSAM-FOUND
               PERFORM P5400-REWRITE THRU P5400-EXIT
           ELSE
               PERFORM P5600-WRITE THRU P5600-EXIT
           END-IF.

       P5000-EXIT.
           EXIT.

       P5200-BUILD-VSAM.
      * THE PRIOR VALUES ON THE VSAM RECORD COME FROM THE VSAM
      * RECORD ITSELF, NOT FROM THE IMS SEGMENT.  WHERE THE TWO HAVE
      * DRIFTED THE TWO PRIOR VALUES WILL DIFFER AND THE
      * RESTATEMENT WILL USE WHICHEVER OF THEM ITS OWN INPUT FILE
      * HAPPENS TO CARRY.
           IF WS-VSAM-FOUND
               MOVE FC-PIU TO FC-PRIOR-PIU
               MOVE FC-PLU TO FC-PRIOR-PLU
           ELSE
               MOVE ZERO TO FC-PRIOR-PIU
               MOVE ZERO TO FC-PRIOR-PLU
           END-IF.
           MOVE WW-PIU        TO FC-PIU.
           MOVE WW-PLU        TO FC-PLU.
           MOVE WW-PSU        TO FC-PSU.
           MOVE WF-SOURCE     TO FC-SOURCE.
           MOVE WF-RESTATE-SW TO FC-RESTATE-SW.
           MOVE WF-RECV-YYDDD TO FC-RECV-YYDDD.
           MOVE WP-QUARTER-YYDDD TO FC-RESTATE-FROM-YYDDD.
           MOVE WF-EFF-YYDDD  TO FC-RESTATE-THRU-YYDDD.

       P5200-EXIT.
           EXIT.

       P5400-REWRITE.
           MOVE CABS-FACTOR-RECORD TO FACTOR-MASTER-RECORD.
           REWRITE FACTOR-MASTER-RECORD
               INVALID KEY
                   MOVE 3120 TO CT-RC
                   PERFORM P7000-SUSPEND THRU P7000-EXIT
           END-REWRITE.
           IF WS-FS-OUTPUT = '00'
               ADD 1 TO WS-VSAM-REWR-CNT
           END-IF.

       P5400-EXIT.
           EXIT.

       P5600-WRITE.
           MOVE CABS-FACTOR-RECORD TO FACTOR-MASTER-RECORD.
           WRITE FACTOR-MASTER-RECORD
               INVALID KEY
                   MOVE 3121 TO CT-RC
                   PERFORM P7000-SUSPEND THRU P7000-EXIT
           END-WRITE.
           IF WS-FS-OUTPUT = '00'
               ADD 1 TO WS-VSAM-WRIT-CNT
           END-IF.

       P5600-EXIT.
           EXIT.

       P6000-CHECKPOINT.
      * TAKE A CHECKPOINT EVERY WP-COMMIT-INT CARDS.  THE CHECKPOINT
      * IDENTIFIER CARRIES THE LAST KEY PROCESSED SO A RESTART CAN
      * BE POSITIONED WITHOUT REREADING THE WHOLE QUARTERLY FILE.
           ADD 1 TO WS-SINCE-CHKP.
           IF WS-SINCE-CHKP < WP-COMMIT-INT
               GO TO P6000-EXIT
           END-IF.
           MOVE WF-OCN        TO WS-CK-OCN.
           MOVE WF-STATE-CD   TO WS-CK-STATE.
           MOVE WF-LATA       TO WS-CK-LATA.
           MOVE WF-EFF-YYDDD  TO WS-CK-EFF-YYDDD.
           CALL 'CBLTDLI' USING DLI-CHKP
                                DB-PCB-CARR
                                WS-CHKP-ID.
           ADD 1 TO WS-CHKP-CNT.
           MOVE ZERO TO WS-SINCE-CHKP.

       P6000-EXIT.
           EXIT.

      *****************************************************************
      * S700-SUSPENSE                                                 *
      *****************************************************************
       S700-SUSPENSE SECTION.

       P7000-SUSPEND.
           MOVE SPACES TO CABS-SUSPENSE-RECORD.
           IF SU-ERR-CODE = SPACES
               MOVE EC-FACTOR-MISSING TO SU-ERR-CODE
           END-IF.
           SET SU-ERROR TO TRUE.
           MOVE WS-PGM-NAME  TO SU-DETECT-PGM.
           MOVE WS-PARA-NAME TO SU-DETECT-PARA.
           MOVE WP-RUN-ID    TO SU-RUN-ID.
           MOVE WS-FACTOR-CARD TO SU-ORIG-RECORD.
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
           MOVE 020              TO CT-STEP-SEQ.
           MOVE WP-CYCLE-YYDDD   TO CT-CYCLE-YYDDD.
           MOVE WP-BILL-PERIOD   TO CT-BILL-PERIOD.
           MOVE ZERO             TO CT-RERUN-NBR.
           MOVE 'CABJ2200'       TO CT-JOBNAME.
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
           PERFORM P8400-BALANCE THRU P8400-EXIT.
           MOVE SPACES           TO CT-ABEND-CD.
           MOVE WS-CHKP-KEY      TO CT-RESTART-KEY.
           MOVE CABS-CONTROL-RECORD TO CONTROL-RECORD.
           WRITE CONTROL-RECORD.

       P8000-EXIT.
           EXIT.

       P8400-BALANCE.
      * THE SUMMARISED COUNT HERE IS THE NUMBER OF CARDS THAT FAILED
      * THE SUM TEST AND WERE LOADED ANYWAY.  THOSE CARDS ARE ALSO
      * COUNTED AS WRITTEN, SO THE EQUATION IS SATISFIED BY MOVING
      * ZERO INTO THE SUMMARISED FIELD ON THE CONTROL RECORD AND
      * REPORTING THE REAL FIGURE ON THE LISTING INSTEAD.
           MOVE ZERO TO CT-SUMMARISED.
           COMPUTE WS-ACC-SEQ-HASH = CT-WRITTEN + CT-REJECTED
                                   + CT-SUMMARISED + CT-CARRIED-FWD.
           IF WS-ACC-SEQ-HASH = CT-READ
               SET CT-IN-BALANCE TO TRUE
           ELSE
               SET CT-OUT-OF-BAL TO TRUE
           END-IF.

       P8400-EXIT.
           EXIT.

       P9000-TERM.
           MOVE 'P9000-TERM' TO WS-PARA-NAME.
           PERFORM P9200-PRINT-TOTALS THRU P9200-EXIT.
           CLOSE PARM-FILE.
           CLOSE FACTOR-IN.
           CLOSE FACTOR-MASTER.
           CLOSE SUSPENSE-FILE.
           CLOSE CONTROL-FILE.
           CLOSE REPORT-FILE.

       P9000-EXIT.
           EXIT.

       P9200-PRINT-TOTALS.
      * THE FOUR COUNTS BELOW ARE THE ONLY PLACE THE TWO STORES ARE
      * COMPARED.  A DIFFERENCE BETWEEN THE IMS TOTAL AND THE VSAM
      * TOTAL IS PRINTED AND NOTHING ELSE HAPPENS.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '1' TO PC-CC.
           MOVE 'CABCTL02' TO PC-COL-001-020.
           MOVE 'QUARTERLY FACTOR APPLICATION' TO PC-COL-021-060.
           MOVE CABS-PRINT-LINE TO REPORT-LINE.
           WRITE REPORT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '0' TO PC-CC.
           MOVE 'IMS SEGMENTS REPL' TO PC-COL-001-020.
           MOVE WS-IMS-REPL-CNT TO WS-ED-COUNT.
           MOVE WS-ED-COUNT TO PC-COL-021-060.
           MOVE CABS-PRINT-LINE TO REPORT-LINE.
           WRITE REPORT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'IMS SEGMENTS ISRT' TO PC-COL-001-020.
           MOVE WS-IMS-ISRT-CNT TO WS-ED-COUNT.
           MOVE WS-ED-COUNT TO PC-COL-021-060.
           MOVE CABS-PRINT-LINE TO REPORT-LINE.
           WRITE REPORT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'VSAM RECORDS REWR' TO PC-COL-001-020.
           MOVE WS-VSAM-REWR-CNT TO WS-ED-COUNT.
           MOVE WS-ED-COUNT TO PC-COL-021-060.
           MOVE CABS-PRINT-LINE TO REPORT-LINE.
           WRITE REPORT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'VSAM RECORDS WRIT' TO PC-COL-001-020.
           MOVE WS-VSAM-WRIT-CNT TO WS-ED-COUNT.
           MOVE WS-ED-COUNT TO PC-COL-021-060.
           MOVE CABS-PRINT-LINE TO REPORT-LINE.
           WRITE REPORT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'CHECKPOINTS TAKEN' TO PC-COL-001-020.
           MOVE WS-CHKP-CNT TO WS-ED-COUNT.
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
           MOVE 3110 TO CT-RC.
           MOVE 12 TO RETURN-CODE.
           SET WS-EOF TO TRUE.

       P9500-EXIT.
           EXIT.
