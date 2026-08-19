       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABCTL05.
      *****************************************************************
      * CABCTL05 - SETTLEMENT POSTING TO THE SETTLEMENT DATABASE      *
      *            AND THE SETTLEMENT MASTER                          *
      * APPLICATION : SETL                                            *
      * COMPILER    : ENTERPRISE COBOL                                *
      * INPUTS      : IMS PCB 1  CABSETDB  PSB CABCT05P  PROCOPT A    *
      *               SETLIN  TELCABS.SETL.NET(0)             FB 200  *
      *               PARMIN  INSTREAM SYSIN PARM CARD        FB 080  *
      * OUTPUTS     : SETLMST TELCABS.SETL.MASTER      VSAM KSDS      *
      *               SUSOUT  TELCABS.SETL.SUSPENSE(+1)       FB 300  *
      *               RPTOUT  SYSOUT PRINT                    FBA 133 *
      * CONTROL     : CTLOUT                                  CABSCTL *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED               *
      *                       + CT-SUMMARISED + CT-CARRIED-FWD        *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      *                                                               *
      * THE SETTLEMENT POSITION IS HELD IN TWO PLACES.  THE IMS       *
      * DATABASE IS WHAT THE ONLINE SETTLEMENT ENQUIRY AND THE        *
      * DISPUTE TRANSACTION READ.  THE VSAM MASTER IS WHAT THE        *
      * NETTING AND STATEMENT PROGRAMS READ.  A POSTING IS ONLY       *
      * COMPLETE WHEN BOTH HAVE BEEN WRITTEN.                         *
      *                                                               *
      * REVISION HISTORY                                              *
      *   V1.00  1993-06-21  D.OKONKWO     INITIAL                    *
      *   V1.03  1997-02-04  J.M.CASTILLO  DETAIL SEGMENT ADDED       *
      *   V1.08  2002-09-16  P.NAIR        DIRECTION INVERTED ON THE  *
      *                                    INBOUND EXCHANGE           *
      *   V2.00  2009-05-27  A.BUKOWSKI    RESYNCHRONISATION UTILITY  *
      *                                    WRITTEN - CABSRSYN         *
      *   V2.02  2016-01-13  L.FERREIRA    COMMIT INTERVAL FROM PARM  *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PARM-FILE     ASSIGN TO PARMIN
                  FILE STATUS IS WS-FS-INPUT.
           SELECT SETTLE-IN     ASSIGN TO SETLIN
                  FILE STATUS IS WS-FS-INPUT.
           SELECT SETTLE-MASTER ASSIGN TO SETLMST
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS SM-KEY
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
       FD  SETTLE-IN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  SETTLE-IN-RECORD            PIC X(200).
       FD  SETTLE-MASTER
           RECORD CONTAINS 200 CHARACTERS.
       01  SETTLE-MASTER-RECORD.
           05  SM-KEY.
               10  SM-SETTLE-TYPE      PIC X(01).
               10  SM-COUNTERPARTY     PIC X(04).
               10  SM-SETTLE-PERIOD    PIC 9(06).
               10  SM-SEQ              PIC 9(09).
           05  SM-REST                 PIC X(180).
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
       01  WS-PGM-NAME                 PIC X(08) VALUE 'CABCTL05'.
       01  WS-PARA-NAME                PIC X(30) VALUE SPACES.
       COPY CABSWRK.
       COPY CABSSETL.
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
       01  SSA-SETLSEG-Q.
           05  FILLER                  PIC X(08) VALUE 'SETLSEG '.
           05  FILLER                  PIC X(01) VALUE '('.
           05  FILLER                  PIC X(08) VALUE 'STKEY   '.
           05  FILLER                  PIC X(02) VALUE ' ='.
           05  SSA-ST-KEY.
               10  SSA-ST-TYPE         PIC X(01) VALUE SPACE.
               10  SSA-ST-OCN          PIC X(04) VALUE SPACES.
               10  SSA-ST-PERIOD       PIC 9(06) VALUE ZERO.
               10  SSA-ST-SEQ          PIC 9(09) VALUE ZERO.
           05  FILLER                  PIC X(01) VALUE ')'.
       01  SSA-SETLSEG-U.
           05  FILLER                  PIC X(08) VALUE 'SETLSEG '.
           05  FILLER                  PIC X(01) VALUE SPACE.
       01  SSA-SETLDTL-U.
           05  FILLER                  PIC X(08) VALUE 'SETLDTL '.
           05  FILLER                  PIC X(01) VALUE SPACE.
      *
       01  WS-SETLSEG-IO.
           05  SS-KEY.
               10  SS-SETTLE-TYPE      PIC X(01).
               10  SS-COUNTERPARTY     PIC X(04).
               10  SS-SETTLE-PERIOD    PIC 9(06).
               10  SS-SEQ              PIC 9(09).
           05  SS-OCN-PERIOD.
               10  SS-XI-OCN           PIC X(04).
               10  SS-XI-PERIOD        PIC 9(06).
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
           05  SS-DISPUTE-SW           PIC X(01).
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
      *
       01  WS-PARM-AREA.
           05  WP-RUN-ID               PIC X(12).
           05  WP-CYCLE-YYDDD          PIC 9(05).
           05  WP-BILL-PERIOD          PIC 9(06).
           05  WP-SETTLE-PERIOD        PIC 9(06).
           05  WP-COMMIT-INT           PIC 9(04).
           05  WP-RESTART-KEY          PIC X(20).
           05  WP-FILLER               PIC X(27).
      *
       01  WS-SWITCHES-LOCAL.
           05  WS-SEG-FOUND-SW         PIC X(01) VALUE 'N'.
               88  WS-SEG-FOUND        VALUE 'Y'.
           05  WS-VSAM-FOUND-SW        PIC X(01) VALUE 'N'.
               88  WS-VSAM-FOUND       VALUE 'Y'.
           05  WS-POST-OK-SW           PIC X(01) VALUE 'Y'.
               88  WS-POST-OK          VALUE 'Y'.
       01  WS-DLI-STATUS               PIC X(02) VALUE SPACES.
           88  WS-DLI-OK               VALUE '  '.
           88  WS-DLI-NOT-FOUND        VALUE 'GE'.
           88  WS-DLI-DUP-INSERT       VALUE 'II'.
           88  WS-DLI-NO-MORE          VALUE 'GE' 'GB' 'GA' 'GK'.
       01  WS-LOCAL-COUNTS.
           05  WS-IMS-ISRT-CNT         PIC S9(09) COMP-3 VALUE 0.
           05  WS-IMS-REPL-CNT         PIC S9(09) COMP-3 VALUE 0.
           05  WS-DTL-ISRT-CNT         PIC S9(09) COMP-3 VALUE 0.
           05  WS-VSAM-WRIT-CNT        PIC S9(09) COMP-3 VALUE 0.
           05  WS-VSAM-REWR-CNT        PIC S9(09) COMP-3 VALUE 0.
           05  WS-SINCE-CHKP           PIC S9(05) COMP-3 VALUE 0.
           05  WS-CHKP-CNT             PIC S9(05) COMP-3 VALUE 0.
           05  WS-DIVERGENCE           PIC S9(09) COMP-3 VALUE 0.
       01  WS-ACCUMS-LOCAL.
           05  WS-ACC-NET              PIC S9(15)V9(02) COMP-3
                                                  VALUE 0.
           05  WS-ACC-GROSS            PIC S9(15)V9(05) COMP-3
                                                  VALUE 0.
       01  WS-CHKP-AREA.
           05  WS-CHKP-ID              PIC X(08) VALUE 'CTL05000'.
           05  WS-CHKP-KEY             PIC X(20) VALUE SPACES.
       01  WS-ED-COUNT                 PIC ZZZ,ZZZ,ZZ9.
       01  WS-ED-AMT                   PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
      *
       LINKAGE SECTION.
       01  DB-PCB-SETL.
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
       PROCEDURE DIVISION USING DB-PCB-SETL.
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
           OPEN INPUT SETTLE-IN.
           OPEN I-O SETTLE-MASTER.
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
               MOVE 300 TO WP-COMMIT-INT
           END-IF.
           MOVE WP-RESTART-KEY TO WS-CHKP-KEY.
           PERFORM P2100-READ-SETTLE THRU P2100-EXIT.

       P1000-EXIT.
           EXIT.

      *****************************************************************
      * S200-MAIN-PROCESS                                             *
      *****************************************************************
       S200-MAIN-PROCESS SECTION.

       P2000-PROCESS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           MOVE 'Y' TO WS-POST-OK-SW.
           PERFORM P2400-EDIT-RECORD THRU P2400-EXIT.
           IF NOT WS-POST-OK
               ADD 1 TO WS-REJECT-CNT
               PERFORM P7000-SUSPEND THRU P7000-EXIT
               PERFORM P2100-READ-SETTLE THRU P2100-EXIT
               GO TO P2000-EXIT
           END-IF.
           PERFORM P3000-POST-IMS THRU P3000-EXIT.
           PERFORM P4000-POST-DETAIL THRU P4000-EXIT.
           PERFORM P5000-POST-VSAM THRU P5000-EXIT.
           ADD 1 TO WS-WRITE-CNT.
           ADD ST-NET-DUE TO WS-ACC-NET.
           ADD ST-GROSS-AMT TO WS-ACC-GROSS.
           PERFORM P6500-CHECKPOINT THRU P6500-EXIT.
           PERFORM P2100-READ-SETTLE THRU P2100-EXIT.

       P2000-EXIT.
           EXIT.

       P2100-READ-SETTLE.
           READ SETTLE-IN
               AT END
                   SET WS-EOF TO TRUE
                   GO TO P2100-EXIT
           END-READ.
           MOVE SETTLE-IN-RECORD TO CABS-SETTLEMENT-RECORD.
           ADD 1 TO WS-READ-CNT.

       P2100-EXIT.
           EXIT.

       P2400-EDIT-RECORD.
      * A SETTLEMENT RECORD WITHOUT A COUNTERPARTY OR WITHOUT A
      * PERIOD CANNOT BE KEYED.  A ZERO NET IS ACCEPTED - A PERIOD
      * THAT NETS TO NOTHING STILL HAS TO BE POSTED SO THE ONLINE
      * ENQUIRY SHOWS THAT IT WAS SETTLED.
           MOVE 'P2400-EDIT-RECORD' TO WS-PARA-NAME.
           MOVE SPACES TO SU-ERR-CODE.
           IF ST-COUNTERPARTY-OCN = SPACES
               MOVE 'N' TO WS-POST-OK-SW
               MOVE EC-OCN-UNKNOWN TO SU-ERR-CODE
               GO TO P2400-EXIT
           END-IF.
           IF ST-SETTLE-PERIOD = ZERO
               MOVE 'N' TO WS-POST-OK-SW
               MOVE EC-DATE-INVALID TO SU-ERR-CODE
               GO TO P2400-EXIT
           END-IF.
           IF WP-SETTLE-PERIOD NOT = ZERO
               IF ST-SETTLE-PERIOD NOT = WP-SETTLE-PERIOD
                   ADD 1 TO WS-CFWD-CNT
                   MOVE 'N' TO WS-POST-OK-SW
                   MOVE EC-RESTATE-NO-BASIS TO SU-ERR-CODE
               END-IF
           END-IF.

       P2400-EXIT.
           EXIT.

      *****************************************************************
      * S300-IMS-POSTING                                              *
      *****************************************************************
       S300-IMS-POSTING SECTION.

       P3000-POST-IMS.
      * FIRST STORE.  THE ROOT IS INSERTED OR REPLACED.  THE SOURCE
      * FIELD FOR THE SECONDARY INDEX IS BUILT HERE - THE INDEX IS
      * MAINTAINED BY IMS FROM SS-OCN-PERIOD AND IS WHAT THE
      * DISPUTE ENQUIRY IN CABCTL06 READS THROUGH.
           MOVE 'P3000-POST-IMS' TO WS-PARA-NAME.
           MOVE ST-SETTLE-TYPE      TO SSA-ST-TYPE.
           MOVE ST-COUNTERPARTY-OCN TO SSA-ST-OCN.
           MOVE ST-SETTLE-PERIOD    TO SSA-ST-PERIOD.
           MOVE ST-SEQ              TO SSA-ST-SEQ.
           MOVE 'N' TO WS-SEG-FOUND-SW.
           CALL 'CBLTDLI' USING DLI-GHU
                                DB-PCB-SETL
                                WS-SETLSEG-IO
                                SSA-SETLSEG-Q.
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
           MOVE SPACES TO WS-SETLSEG-IO.
           MOVE ST-SETTLE-TYPE      TO SS-SETTLE-TYPE.
           MOVE ST-COUNTERPARTY-OCN TO SS-COUNTERPARTY.
           MOVE ST-SETTLE-PERIOD    TO SS-SETTLE-PERIOD.
           MOVE ST-SEQ              TO SS-SEQ.
           MOVE ST-COUNTERPARTY-OCN TO SS-XI-OCN.
           MOVE ST-SETTLE-PERIOD    TO SS-XI-PERIOD.
           MOVE ST-TOTAL-MOU        TO SS-TOTAL-MOU.
           MOVE ST-BILLABLE-MOU     TO SS-BILLABLE-MOU.
           MOVE ST-CAPPED-MOU       TO SS-CAPPED-MOU.
           MOVE ST-RATE-APPLIED     TO SS-RATE-APPLIED.
           MOVE ST-GROSS-AMT        TO SS-GROSS-AMT.
           MOVE ST-OUR-SHARE        TO SS-OUR-SHARE.
           MOVE ST-THEIR-SHARE      TO SS-THEIR-SHARE.
           MOVE ST-NET-DUE          TO SS-NET-DUE.
           MOVE ST-ROUND-RESIDUE    TO SS-ROUND-RESIDUE.
           MOVE ST-DIRECTION        TO SS-DIRECTION.
           MOVE ST-DISPUTE-SW       TO SS-DISPUTE-SW.
           MOVE ST-EXCH-YYDDD       TO SS-EXCH-YYDDD.
           MOVE ST-RAO-CODE         TO SS-RAO-CODE.
           MOVE WP-CYCLE-YYDDD      TO SS-POST-YYDDD.
           MOVE WS-PGM-NAME         TO SS-POST-PGM.

       P3200-EXIT.
           EXIT.

       P3400-REPLACE-ROOT.
      * A REPLACE OF THE ROOT LEAVES THE DETAIL SEGMENTS FROM THE
      * PREVIOUS POSTING IN PLACE.  THEY ARE NOT DELETED FIRST
      * BECAUSE A RERUN CARRIES THE SAME DETAIL AND THE INSERT WILL
      * BE REJECTED AS A DUPLICATE, WHICH IS THE INTENDED RESULT.
           CALL 'CBLTDLI' USING DLI-REPL
                                DB-PCB-SETL
                                WS-SETLSEG-IO.
           MOVE DBP-STATUS-CODE TO WS-DLI-STATUS.
           IF WS-DLI-OK
               ADD 1 TO WS-IMS-REPL-CNT
           ELSE
               PERFORM P9500-DLI-ERROR THRU P9500-EXIT
           END-IF.

       P3400-EXIT.
           EXIT.

       P3600-INSERT-ROOT.
           CALL 'CBLTDLI' USING DLI-ISRT
                                DB-PCB-SETL
                                WS-SETLSEG-IO
                                SSA-SETLSEG-U.
           MOVE DBP-STATUS-CODE TO WS-DLI-STATUS.
           IF WS-DLI-OK
               ADD 1 TO WS-IMS-ISRT-CNT
               GO TO P3600-EXIT
           END-IF.
           IF WS-DLI-DUP-INSERT
               ADD 1 TO WS-CFWD-CNT
               GO TO P3600-EXIT
           END-IF.
           PERFORM P9500-DLI-ERROR THRU P9500-EXIT.

       P3600-EXIT.
           EXIT.

       P4000-POST-DETAIL.
      * ONE DETAIL SEGMENT PER SETTLEMENT RECORD, CARRYING THE
      * CIRCUIT AND THE TWO PERCENTAGES FOR MEET POINT.  FOR
      * RECIPROCAL COMPENSATION AND CMDS THE CIRCUIT FIELDS ARE
      * SPACES AND THE SEGMENT IS STILL WRITTEN, BECAUSE THE ONLINE
      * ENQUIRY EXPECTS ONE.
           MOVE 'P4000-POST-DETAIL' TO WS-PARA-NAME.
           MOVE SPACES TO WS-SETLDTL-IO.
           MOVE ST-SEQ           TO SD-SEQ.
           MOVE ST-TRUNK-GRP     TO SD-TRUNK-GRP.
           MOVE ST-CIRCUIT-ID    TO SD-CIRCUIT-ID.
           MOVE ST-OUR-PCT       TO SD-OUR-PCT.
           MOVE ST-THEIR-PCT     TO SD-THEIR-PCT.
           MOVE ST-PCT-VARIANCE  TO SD-PCT-VARIANCE.
           MOVE ST-BILLABLE-MOU  TO SD-MOU.
           MOVE ST-GROSS-AMT     TO SD-AMOUNT.
           CALL 'CBLTDLI' USING DLI-ISRT
                                DB-PCB-SETL
                                WS-SETLDTL-IO
                                SSA-SETLSEG-Q
                                SSA-SETLDTL-U.
           MOVE DBP-STATUS-CODE TO WS-DLI-STATUS.
           IF WS-DLI-OK
               ADD 1 TO WS-DTL-ISRT-CNT
               GO TO P4000-EXIT
           END-IF.
           IF WS-DLI-DUP-INSERT
               ADD 1 TO WS-SUMM-CNT
               GO TO P4000-EXIT
           END-IF.
           PERFORM P9500-DLI-ERROR THRU P9500-EXIT.

       P4000-EXIT.
           EXIT.

      *****************************************************************
      * S500-VSAM-POSTING                                             *
      *****************************************************************
       S500-VSAM-POSTING SECTION.

       P5000-POST-VSAM.
      * SECOND STORE.  THE VSAM WRITE IS HARDENED WHEN IT IS ISSUED
      * AND IS NOT INSIDE THE CHECKPOINT SCOPE THAT COVERS THE IMS
      * WORK.  THE RESYNCHRONISATION UTILITY CABSRSYN WAS WRITTEN IN
      * 2009 TO COMPARE THE TWO AND CORRECT THE DIFFERENCE.  IT IS
      * HELD IN TELCABS.SETL.SRCLIB AND IS RUN ON REQUEST.
           MOVE 'P5000-POST-VSAM' TO WS-PARA-NAME.
           MOVE 'N' TO WS-VSAM-FOUND-SW.
           MOVE ST-SETTLE-TYPE      TO SM-SETTLE-TYPE.
           MOVE ST-COUNTERPARTY-OCN TO SM-COUNTERPARTY.
           MOVE ST-SETTLE-PERIOD    TO SM-SETTLE-PERIOD.
           MOVE ST-SEQ              TO SM-SEQ.
           READ SETTLE-MASTER
               INVALID KEY
                   CONTINUE
           END-READ.
           IF WS-FS-OUTPUT = '00'
               MOVE 'Y' TO WS-VSAM-FOUND-SW
           END-IF.
           MOVE CABS-SETTLEMENT-RECORD TO SETTLE-MASTER-RECORD.
           MOVE ST-SETTLE-TYPE      TO SM-SETTLE-TYPE.
           MOVE ST-COUNTERPARTY-OCN TO SM-COUNTERPARTY.
           MOVE ST-SETTLE-PERIOD    TO SM-SETTLE-PERIOD.
           MOVE ST-SEQ              TO SM-SEQ.
           IF WS-VSAM-FOUND
               REWRITE SETTLE-MASTER-RECORD
                   INVALID KEY
                       MOVE 3420 TO CT-RC
                       PERFORM P7000-SUSPEND THRU P7000-EXIT
               END-REWRITE
               IF WS-FS-OUTPUT = '00'
                   ADD 1 TO WS-VSAM-REWR-CNT
               END-IF
           ELSE
               WRITE SETTLE-MASTER-RECORD
                   INVALID KEY
                       MOVE 3421 TO CT-RC
                       PERFORM P7000-SUSPEND THRU P7000-EXIT
               END-WRITE
               IF WS-FS-OUTPUT = '00'
                   ADD 1 TO WS-VSAM-WRIT-CNT
               END-IF
           END-IF.

       P5000-EXIT.
           EXIT.

       P6500-CHECKPOINT.
           ADD 1 TO WS-SINCE-CHKP.
           IF WS-SINCE-CHKP < WP-COMMIT-INT
               GO TO P6500-EXIT
           END-IF.
           MOVE SS-KEY TO WS-CHKP-KEY.
           CALL 'CBLTDLI' USING DLI-CHKP
                                DB-PCB-SETL
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
               MOVE EC-OUT-OF-BALANCE TO SU-ERR-CODE
           END-IF.
           SET SU-ERROR TO TRUE.
           MOVE WS-PGM-NAME  TO SU-DETECT-PGM.
           MOVE WS-PARA-NAME TO SU-DETECT-PARA.
           MOVE WP-RUN-ID    TO SU-RUN-ID.
           MOVE CABS-SETTLEMENT-RECORD TO SU-ORIG-RECORD.
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
           MOVE 050              TO CT-STEP-SEQ.
           MOVE WP-CYCLE-YYDDD   TO CT-CYCLE-YYDDD.
           MOVE WP-BILL-PERIOD   TO CT-BILL-PERIOD.
           MOVE ZERO             TO CT-RERUN-NBR.
           MOVE 'CABJ2500'       TO CT-JOBNAME.
           MOVE 'IMSSTEP'        TO CT-STEPNAME.
           MOVE WS-READ-CNT      TO CT-READ.
           MOVE WS-WRITE-CNT     TO CT-WRITTEN.
           MOVE WS-REJECT-CNT    TO CT-REJECTED.
           MOVE WS-SUMM-CNT      TO CT-SUMMARISED.
           MOVE WS-CFWD-CNT      TO CT-CARRIED-FWD.
           MOVE WS-ACC-NET       TO CT-HASH-MINUTES.
           MOVE WS-ACC-GROSS     TO CT-HASH-AMOUNT.
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
           CLOSE SETTLE-IN.
           CLOSE SETTLE-MASTER.
           CLOSE SUSPENSE-FILE.
           CLOSE CONTROL-FILE.
           CLOSE REPORT-FILE.

       P9000-EXIT.
           EXIT.

       P9200-PRINT-TOTALS.
      * THE TWO POSTING COUNTS ARE COMPARED HERE.  A DIFFERENCE IS
      * PRINTED AS A MESSAGE.  THE STEP STILL ENDS ON RETURN CODE
      * ZERO AND THE NEXT STEP IN THE JOB RUNS.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '1' TO PC-CC.
           MOVE 'CABCTL05' TO PC-COL-001-020.
           MOVE 'SETTLEMENT POSTING' TO PC-COL-021-060.
           MOVE CABS-PRINT-LINE TO REPORT-LINE.
           WRITE REPORT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '0' TO PC-CC.
           MOVE 'IMS ROOTS ISRT' TO PC-COL-001-020.
           MOVE WS-IMS-ISRT-CNT TO WS-ED-COUNT.
           MOVE WS-ED-COUNT TO PC-COL-021-060.
           MOVE CABS-PRINT-LINE TO REPORT-LINE.
           WRITE REPORT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'IMS ROOTS REPL' TO PC-COL-001-020.
           MOVE WS-IMS-REPL-CNT TO WS-ED-COUNT.
           MOVE WS-ED-COUNT TO PC-COL-021-060.
           MOVE CABS-PRINT-LINE TO REPORT-LINE.
           WRITE REPORT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'VSAM RECORDS' TO PC-COL-001-020.
           COMPUTE WS-DIVERGENCE = WS-VSAM-WRIT-CNT
                                 + WS-VSAM-REWR-CNT.
           MOVE WS-DIVERGENCE TO WS-ED-COUNT.
           MOVE WS-ED-COUNT TO PC-COL-021-060.
           MOVE CABS-PRINT-LINE TO REPORT-LINE.
           WRITE REPORT-LINE.
           COMPUTE WS-DIVERGENCE = WS-IMS-ISRT-CNT + WS-IMS-REPL-CNT
                                 - WS-VSAM-WRIT-CNT
                                 - WS-VSAM-REWR-CNT.
           IF WS-DIVERGENCE NOT = ZERO
               MOVE SPACES TO CABS-PRINT-LINE
               MOVE '0' TO PC-CC
               MOVE 'POST COUNTS DIFFER' TO PC-COL-001-020
               MOVE WS-DIVERGENCE TO WS-ED-COUNT
               MOVE WS-ED-COUNT TO PC-COL-021-060
               MOVE 'RUN CABSRSYN' TO PC-COL-061-090
               MOVE CABS-PRINT-LINE TO REPORT-LINE
               WRITE REPORT-LINE
           END-IF.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'NET POSTED' TO PC-COL-001-020.
           MOVE WS-ACC-NET TO WS-ED-AMT.
           MOVE WS-ED-AMT TO PC-COL-021-060.
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
           MOVE 3410 TO CT-RC.
           MOVE 12 TO RETURN-CODE.
           SET WS-EOF TO TRUE.

       P9500-EXIT.
           EXIT.
