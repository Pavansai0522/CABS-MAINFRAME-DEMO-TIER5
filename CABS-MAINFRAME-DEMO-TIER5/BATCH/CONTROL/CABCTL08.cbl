      *****************************************************************
      * CABCTL08 - INBOUND SETTLEMENT ACKNOWLEDGEMENT CONSUMER        *
      * APPLICATION : SETL                                            *
      * COMPILER    : ENTERPRISE COBOL                                *
      * INPUTS      : MQ       CABS.SETTLE.ACK ON CSQ1         NONE   *
      * INPUTS      : SYSIN    INSTREAM                       NONE    *
      * OUTPUTS     : ACKOUT   TELCABS.SETL.ACK.STATUS         NONE   *
      * OUTPUTS     : SUSPOUT  TELCABS.SETL.SUSPENSE(+1)        CABSERR*
      * CONTROL     : CTLOUT                         CABSCTL          *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED               *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                  *
      * COMPILED WITH ENTERPRISE COBOL AND LINK-EDITED WITH THE MQ    *
      * COBOL STUB CSQBSTUB.  SCOPE TERMINATORS PERMITTED HERE.        *
      * REVISION HISTORY                                              *
      *   V1.00  1996-11-18  T.OKONKWO   INITIAL GEN, MQSERIES 2.1     *
      *   V1.03  2001-09-27  D.WASILEWSKI PAIRED WITH THE CABCTL07     *
      *                                  RESEND MODE ADDED UNDER       *
      *                                  CR-2940                       *
      *   V1.05  2005-01-11  A.BUKOWSKI  COMMON MQ FAILURE HANDLING    *
      *                                  CONSOLIDATED UNDER CABS-STD-  *
      *                                  036 SO SUSPENSE, PARTIAL      *
      *                                  COMMIT AND RESTART KEY        *
      *                                  UPDATE HAPPEN THE SAME WAY    *
      *                                  NO MATTER WHERE THE RUN IS    *
      *                                  INTERRUPTED                   *
      *   V2.00  2009-07-30  A.BUKOWSKI  COMMIT FREQUENCY RAISED TO    *
      *                                  100 FROM 50                   *
      *   V2.02  2015-11-12  M.OYELARAN  RECOMPILE, QUEUE MANAGER      *
      *                                  RENAMED CSQ1 (WAS CSQP)       *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CABCTL08.
       AUTHOR.        T.OKONKWO.
       DATE-WRITTEN.  1996-11-18.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * SYSIN - RUN CONTROL CARD, NO DEFAULTS SUPPLIED.
           SELECT PARM-FILE
               ASSIGN TO UT-S-SYSIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-TABLE.
      * RUN CONTROL - BALANCING RECORD, GDG PLUS ONE.
           SELECT CONTROL-FILE
               ASSIGN TO UT-S-CTLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CONTROL.
      * SUSPENSE - ACKNOWLEDGEMENTS THAT COULD NOT BE MATCHED OR
      * POSTED.
           SELECT SUSPENSE-FILE
               ASSIGN TO UT-S-SUSPOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-SUSPENSE.
      * ACKNOWLEDGEMENT STATUS KSDS, KEYED BY SETTLEMENT KEY.
           SELECT ACK-STATUS-FILE
               ASSIGN TO DA-I-ACKOUT
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS AKS-KEY
               FILE STATUS IS WS-FS-OUTPUT.

       DATA DIVISION.
       FILE SECTION.
       FD  PARM-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 80 CHARACTERS
               DATA RECORD IS PRM-RECORD.
       01  PRM-RECORD              PIC X(80).

       FD  CONTROL-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 180 CHARACTERS
               DATA RECORD IS CTL-RECORD.
       01  CTL-RECORD              PIC X(180).

       FD  SUSPENSE-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 300 CHARACTERS
               DATA RECORD IS SUS-RECORD.
       01  SUS-RECORD              PIC X(300).

       FD  ACK-STATUS-FILE
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 100 CHARACTERS
               DATA RECORD IS AKS-RECORD.
       01  AKS-RECORD.
           05  AKS-KEY.
               10  AKS-KEY-TYPE            PIC X(01).
               10  AKS-KEY-OCN             PIC X(04).
               10  AKS-KEY-PERIOD          PIC 9(06).
               10  AKS-KEY-SEQ             PIC 9(09).
           05  AKS-STATUS-CODE         PIC X(02).
           05  AKS-REASON-TEXT         PIC X(40).
           05  AKS-ACK-TS              PIC X(14).
           05  AKS-RUN-ID              PIC X(12).
           05  FILLER                  PIC X(12).

       WORKING-STORAGE SECTION.

      * PROGRAM IDENTIFICATION - MOVED TO THE CONTROL RECORD AND TO
      * EVERY SUSPENSE RECORD RAISED BY THIS MODULE.
       01  WS-PROGRAM-IDENT.
           05  WS-PGM-NAME         PIC X(08)   VALUE 'CABCTL08'.
           05  WS-PGM-VERSION      PIC X(05)   VALUE 'V2.02'.
           05  WS-PGM-APPL         PIC X(04)   VALUE 'SETL'.
           05  WS-PGM-COMPILE      PIC X(08)   VALUE '20151112'.
           05  WS-PARA-NAME        PIC X(30)   VALUE SPACES.

      * RUN CONTEXT.  POPULATED FROM THE SYSIN CARD.
       01  WS-RUN-CONTEXT.
           05  WS-RUN-ID           PIC X(12)   VALUE SPACES.
           05  WS-CYCLE-YYDDD.
               10  WS-CYCLE-YY         PIC 9(02)     VALUE 0.
               10  WS-CYCLE-DDD        PIC 9(03)     VALUE 0.
           05  WS-BILL-PERIOD      PIC 9(06)   VALUE 0.
           05  WS-RERUN-NBR        PIC 9(02)   VALUE 0.
           05  WS-JOBNAME          PIC X(08)   VALUE SPACES.
           05  WS-STEPNAME         PIC X(08)   VALUE SPACES.
           05  WS-RETURN-CODE      PIC 9(04)   VALUE 0.
           05  WS-ERR-CODE         PIC X(04)   VALUE SPACES.
           05  WS-ERR-SEVERITY     PIC X(01)   VALUE 'E'.
           05  WS-SUB-RC           PIC S9(04) COMP  VALUE 0.

      * RESTART KEY.  BROKEN INTO SUBFIELDS SO THE FAILURE HANDLER
      * CAN REBUILD IT FROM THE CORRELID-DERIVED KEY WITHOUT
      * REFERENCE MODIFICATION.
       01  WS-RESTART-KEY.
           05  WS-RK-TYPE          PIC X(01)   VALUE SPACES.
           05  WS-RK-OCN           PIC X(04)   VALUE SPACES.
           05  WS-RK-PERIOD        PIC 9(06)   VALUE 0.
           05  WS-RK-SEQ           PIC 9(09)   VALUE 0.
           05  FILLER              PIC X(06)   VALUE SPACES.

       COPY CABSWRK.

      * SYSIN CONTROL CARD.  COLUMNS 1-2 ARE THE CARD TYPE.
       01  WS-PARM-CARD.
           05  WS-PC-TYPE          PIC X(02)   VALUE SPACES.
           05  WS-PC-REST          PIC X(78)   VALUE SPACES.
       01  WS-PARM-RUN REDEFINES WS-PARM-CARD.
           05  FILLER              PIC X(02).
           05  WS-PC-RUN-ID        PIC X(12).
           05  WS-PC-CYCLE.
               10  WS-PC-CYCLE-YY      PIC 9(02).
               10  WS-PC-CYCLE-DDD     PIC 9(03).
           05  WS-PC-BILL-PERIOD   PIC 9(06).
           05  WS-PC-RERUN         PIC 9(02).
           05  WS-PC-JOBNAME       PIC X(08).
           05  WS-PC-STEPNAME      PIC X(08).
           05  WS-PC-WAIT-INTERVAL PIC 9(05).
           05  WS-PC-COMMIT-FREQ   PIC 9(05).
           05  FILLER              PIC X(27).

      * LOCAL SWITCHES.  THE STANDARD ONES ARRIVE VIA CABSWRK.
       01  WS-LOCAL-SWITCHES.
           05  WS-PARM-EOF-SW          PIC X(01)   VALUE 'N'.
               88  WS-PARM-EOF                 VALUE 'Y'.

      * THE 300-BYTE FIXED ACKNOWLEDGEMENT MESSAGE.  MIRRORS THE
      * GATEWAY MESSAGE BUILT BY CABCTL07.
       01  WS-ACK-MSG.
           05  ACK-REC-TYPE            PIC X(02).
           05  ACK-SETTLE-TYPE         PIC X(01).
           05  ACK-COUNTERPARTY-OCN    PIC X(04).
           05  ACK-SETTLE-PERIOD       PIC 9(06).
           05  ACK-SEQ                 PIC 9(09).
           05  ACK-STATUS-CODE         PIC X(02).
               88  ACK-ACCEPTED                VALUE 'AC'.
               88  ACK-REJECTED                VALUE 'RJ'.
               88  ACK-PENDING                 VALUE 'PN'.
           05  ACK-REASON-TEXT         PIC X(40).
           05  ACK-ORIG-RUN-ID         PIC X(12).
           05  ACK-ACK-TS              PIC X(14).
           05  FILLER                  PIC X(210).

      * CORRELATION ID PARSE AREA.  MQMD-CORRELID IS REDEFINED
      * HERE SO THE ORIGINATING SETTLEMENT KEY CAN BE RECOVERED
      * WITHOUT PARSING THE MESSAGE BODY - CABCTL07 BUILDS IT THE
      * SAME WAY WHEN IT PUTS THE ORIGINAL SETTLEMENT MESSAGE.
       01  WS-CORRELID-BUILD.
           05  WCB-TYPE                PIC X(01).
           05  WCB-OCN                 PIC X(04).
           05  WCB-PERIOD              PIC 9(06).
           05  WCB-SEQ                 PIC 9(09).
           05  FILLER                  PIC X(04).

      * MQ AREAS.  COPY MEMBERS SUPPLY THE CONSTANTS AND THE
      * OBJECT / MESSAGE / OPTIONS STRUCTURES.
           COPY CMQV.
           COPY CMQODV.
           COPY CMQMDV.
           COPY CMQGMOV.
           COPY CMQPMOV.

       01  WS-MQ-AREAS.
           05  W00-QM-NAME             PIC X(48)   VALUE 'CSQ1'.
           05  W00-COMP-CODE           PIC S9(9) COMP  VALUE 0.
           05  W00-REASON              PIC S9(9) COMP  VALUE 0.
           05  W02-OPEN-OPTIONS        PIC S9(9) COMP  VALUE 0.
           05  W02-CLOSE-OPTIONS       PIC S9(9) COMP  VALUE 0.
           05  W03-HCONN               PIC S9(9) COMP  VALUE 0.
           05  W03-HOBJ                PIC S9(9) COMP  VALUE 0.
           05  W04-BUFFLEN             PIC S9(9) COMP  VALUE 300.
           05  W04-DATALEN             PIC S9(9) COMP  VALUE 0.
           05  W04-BUFFER              PIC X(300)  VALUE SPACES.

      * MQ RUN COUNTERS.
       01  WS-MQ-COUNTERS.
           05  WS-MQGET-CNT            PIC S9(09) COMP-3   VALUE 0.
           05  WS-COMMIT-CNT           PIC S9(09) COMP-3   VALUE 0.
           05  WS-SINCE-COMMIT         PIC S9(09) COMP-3   VALUE 0.

      * ABEND COMMUNICATION AREA.  PASSED TO CABABEND WHICH ISSUES
      * A USER ABEND WITH THE CODE IN WS-AB-CODE.
       01  WS-ABEND-AREA.
           05  WS-AB-CODE              PIC 9(04) COMP  VALUE 0.
           05  WS-AB-PGM               PIC X(08)   VALUE SPACES.
           05  WS-AB-PARA              PIC X(30)   VALUE SPACES.
           05  WS-AB-TEXT               PIC X(60)   VALUE SPACES.
           05  WS-AB-KEY                PIC X(26)   VALUE SPACES.

      * ACCEPT AREAS FOR THE ACKNOWLEDGEMENT TIMESTAMP.
       01  WS-ACCEPT-AREAS.
           05  WS-ACCEPT-DATE          PIC 9(06)   VALUE 0.
           05  WS-ACCEPT-TIME          PIC 9(08)   VALUE 0.

      * ORIGINAL RECORD IMAGE FOR THE SUSPENSE WRITER.  AN
      * ACKNOWLEDGEMENT HAS NO NATIVE 200-BYTE CABS RECORD, SO THE
      * MEANINGFUL FIELDS OF THE INBOUND MESSAGE ARE CARRIED
      * INSTEAD, PADDED OUT TO THE STANDARD SUSPENSE WIDTH.
       01  WS-SUSPENSE-ORIG.
           05  SO-ACK-REC-TYPE         PIC X(02).
           05  SO-ACK-SETTLE-TYPE      PIC X(01).
           05  SO-ACK-OCN              PIC X(04).
           05  SO-ACK-PERIOD           PIC 9(06).
           05  SO-ACK-SEQ              PIC 9(09).
           05  SO-ACK-STATUS           PIC X(02).
           05  SO-ACK-REASON           PIC X(40).
           05  SO-ACK-RUN-ID           PIC X(12).
           05  SO-ACK-TS               PIC X(14).
           05  FILLER                  PIC X(110).

       PROCEDURE DIVISION.


      *****************************************************************
      * S000-MAINLINE                                                 *
      * DRIVER.  STRUCTURE IS MANDATED BY CABS-STD-001.               *
      *****************************************************************
       S000-MAINLINE SECTION.

       P0000-MAINLINE.
           PERFORM P1000-INIT     THRU P1000-EXIT.
           PERFORM P2000-PROCESS  THRU P2000-EXIT
               UNTIL WS-EOF.
           PERFORM P8000-CONTROL  THRU P8000-EXIT.
           PERFORM P9000-TERM     THRU P9000-EXIT.


      *****************************************************************
      * S100-INITIALISATION                                           *
      * OPEN, READ THE CONTROL CARD, CONNECT AND OPEN THE QUEUE.      *
      *****************************************************************
       S100-INITIALISATION SECTION.

       P1000-INIT.
           MOVE 'P1000-INIT' TO WS-PARA-NAME.
           ACCEPT WS-ACCEPT-DATE FROM DATE.
           ACCEPT WS-ACCEPT-TIME FROM TIME.
           OPEN INPUT  PARM-FILE
           OPEN OUTPUT CONTROL-FILE
                       SUSPENSE-FILE
           OPEN I-O    ACK-STATUS-FILE
           END-OPEN.
           IF WS-FS-TABLE NOT = '00'
               MOVE 7202 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-SYSIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT
           END-IF.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 7203 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-CTLOUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT
           END-IF.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 7204 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-SUSPOUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT
           END-IF.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 7205 TO WS-AB-CODE
               MOVE 'OPEN FAILED DA-I-ACKOUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT
           END-IF.
           PERFORM P1100-READ-PARM THRU P1100-EXIT.
           PERFORM P1200-EDIT-PARM THRU P1200-EXIT.
           PERFORM P1300-MQ-CONNECT THRU P1300-EXIT.
           MOVE WS-PC-RUN-ID           TO WS-RUN-ID.
           MOVE WS-PC-CYCLE            TO WS-CYCLE-YYDDD.
           MOVE WS-PC-BILL-PERIOD      TO WS-BILL-PERIOD.
           MOVE WS-PC-RERUN            TO WS-RERUN-NBR.
           MOVE WS-PC-JOBNAME          TO WS-JOBNAME.
           MOVE WS-PC-STEPNAME         TO WS-STEPNAME.
           DISPLAY WS-PGM-NAME ' STARTED - RUN ' WS-RUN-ID.

       P1000-EXIT.
           EXIT.

       P1100-READ-PARM.
           MOVE 'P1100-READ-PARM' TO WS-PARA-NAME.
           MOVE SPACES TO WS-PARM-CARD.
           READ PARM-FILE INTO WS-PARM-CARD
               AT END
                   MOVE 'Y' TO WS-PARM-EOF-SW
           END-READ.
           IF WS-PARM-EOF
               MOVE 7206 TO WS-AB-CODE
               MOVE 'NO SYSIN CONTROL CARD SUPPLIED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT
           END-IF.
           IF WS-PC-TYPE NOT = 'AK'
               MOVE 7207 TO WS-AB-CODE
               MOVE 'SYSIN CARD TYPE NOT AK' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT
           END-IF.

       P1100-EXIT.
           EXIT.

       P1200-EDIT-PARM.
           MOVE 'P1200-EDIT-PARM' TO WS-PARA-NAME.
           IF WS-PC-CYCLE NOT NUMERIC
               MOVE 7208 TO WS-AB-CODE
               MOVE 'CYCLE DATE NOT NUMERIC' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT
           END-IF.
           IF WS-PC-BILL-PERIOD NOT NUMERIC
               MOVE 7209 TO WS-AB-CODE
               MOVE 'BILL PERIOD NOT NUMERIC' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT
           END-IF.
           IF WS-PC-RERUN NOT NUMERIC
               MOVE ZERO TO WS-PC-RERUN
           END-IF.
           IF WS-PC-WAIT-INTERVAL NOT NUMERIC
               OR WS-PC-WAIT-INTERVAL = ZERO
               MOVE 30000 TO WS-PC-WAIT-INTERVAL
           END-IF.
           IF WS-PC-COMMIT-FREQ NOT NUMERIC OR WS-PC-COMMIT-FREQ = ZERO
               MOVE 00100 TO WS-PC-COMMIT-FREQ
           END-IF.

       P1200-EXIT.
           EXIT.

       P1300-MQ-CONNECT.
      * CONNECT TO THE SETTLEMENT QUEUE MANAGER AND OPEN THE INBOUND
      * ACKNOWLEDGEMENT QUEUE FOR GET, SHARED SO THE ONLINE MONITOR
      * TRANSACTION CAN BROWSE IT AT THE SAME TIME.
           MOVE 'P1300-MQ-CONNECT' TO WS-PARA-NAME.
           CALL 'MQCONN' USING W00-QM-NAME
                               W03-HCONN
                               W00-COMP-CODE
                               W00-REASON.
           IF W00-COMP-CODE NOT = MQCC-OK
               MOVE 7210 TO WS-AB-CODE
               MOVE 'MQCONN FAILED - SEE REASON IN DISPLAY' TO
                    WS-AB-TEXT
               DISPLAY 'MQCONN REASON ' W00-REASON
               PERFORM P9500-ABEND THRU P9500-EXIT
           END-IF.
           MOVE SPACES TO MQOD-OBJECTNAME.
           MOVE 'CABS.SETTLE.ACK' TO MQOD-OBJECTNAME.
           MOVE MQOT-Q TO MQOD-OBJECTTYPE.
           COMPUTE W02-OPEN-OPTIONS =
                   MQOO-INPUT-SHARED + MQOO-FAIL-IF-QUIESCING.
           CALL 'MQOPEN' USING W03-HCONN
                               MQOD
                               W02-OPEN-OPTIONS
                               W03-HOBJ
                               W00-COMP-CODE
                               W00-REASON.
           IF W00-COMP-CODE NOT = MQCC-OK
               MOVE 7211 TO WS-AB-CODE
               MOVE 'MQOPEN FAILED ON CABS.SETTLE.ACK' TO WS-AB-TEXT
               DISPLAY 'MQOPEN REASON ' W00-REASON
               PERFORM P9500-ABEND THRU P9500-EXIT
           END-IF.

       P1300-EXIT.
           EXIT.


      *****************************************************************
      * S200-CONSUME                                                  *
      * GET, PARSE, MATCH, POST.  RUN ENDS WHEN THE QUEUE DRAINS.    *
      *****************************************************************
       S200-CONSUME SECTION.

       P2000-PROCESS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-GET-MESSAGE THRU P2100-EXIT.
           IF WS-EOF
               GO TO P2000-EXIT
           END-IF.
           PERFORM P3000-PARSE-ACK THRU P3000-EXIT.
           PERFORM P4200-MATCH-ACK THRU P4200-EXIT.
           PERFORM P6300-UPDATE-STATUS THRU P6300-EXIT.
           PERFORM P5000-COMMIT-CHECK THRU P5000-EXIT.
           ADD 1 TO WS-WRITE-CNT.

       P2000-EXIT.
           EXIT.

       P2100-GET-MESSAGE.
      * GET WITH WAIT.  MQRC-NO-MSG-AVAILABLE AFTER THE WAIT
      * INTERVAL EXPIRES IS THE NORMAL END OF RUN - THE QUEUE HAS
      * DRAINED AND THERE IS NOTHING LEFT TO CONSUME THIS CYCLE.
           MOVE 'P2100-GET-MESSAGE' TO WS-PARA-NAME.
           MOVE SPACES TO W04-BUFFER.
           MOVE MQMI-NONE TO MQMD-MSGID.
           MOVE MQCI-NONE TO MQMD-CORRELID.
           COMPUTE W02-CLOSE-OPTIONS = MQGMO-WAIT
                                     + MQGMO-SYNCPOINT
                                     + MQGMO-CONVERT.
           MOVE W02-CLOSE-OPTIONS TO MQGMO-OPTIONS.
           MOVE WS-PC-WAIT-INTERVAL TO MQGMO-WAITINTERVAL.
           CALL 'MQGET' USING W03-HCONN
                              W03-HOBJ
                              MQMD
                              MQGMO
                              W04-BUFFLEN
                              W04-BUFFER
                              W04-DATALEN
                              W00-COMP-CODE
                              W00-REASON.
           IF W00-COMP-CODE = MQCC-OK
               ADD 1 TO WS-MQGET-CNT
               ADD 1 TO WS-READ-CNT
           ELSE
               IF W00-REASON = MQRC-NO-MSG-AVAILABLE
                   MOVE 'Y' TO WS-EOF-SW
               ELSE
                   MOVE 'M101' TO WS-ERR-CODE
                   MOVE 'E' TO WS-ERR-SEVERITY
                   GO TO P9995-MQ-FAILURE
               END-IF
           END-IF.

       P2100-EXIT.
           EXIT.

       P3000-PARSE-ACK.
      * THE BUFFER IS ALREADY THE FIXED 300-BYTE LAYOUT - NO
      * DELIMITERS TO SCAN FOR.
           MOVE 'P3000-PARSE-ACK' TO WS-PARA-NAME.
           MOVE W04-BUFFER TO WS-ACK-MSG.
           MOVE SPACES TO WS-SUSPENSE-ORIG.
           MOVE ACK-REC-TYPE TO SO-ACK-REC-TYPE.
           MOVE ACK-SETTLE-TYPE TO SO-ACK-SETTLE-TYPE.
           MOVE ACK-COUNTERPARTY-OCN TO SO-ACK-OCN.
           MOVE ACK-SETTLE-PERIOD TO SO-ACK-PERIOD.
           MOVE ACK-SEQ TO SO-ACK-SEQ.
           MOVE ACK-STATUS-CODE TO SO-ACK-STATUS.
           MOVE ACK-REASON-TEXT TO SO-ACK-REASON.
           MOVE ACK-ORIG-RUN-ID TO SO-ACK-RUN-ID.
           MOVE ACK-ACK-TS TO SO-ACK-TS.
      * RECOVER THE ORIGINATING SETTLEMENT KEY FROM MQMD-CORRELID,
      * WHICH CABCTL07 BUILT THE SAME WAY WHEN IT PUT THE ORIGINAL
      * SETTLEMENT MESSAGE.
           MOVE MQMD-CORRELID TO WS-CORRELID-BUILD.

       P3000-EXIT.
           EXIT.


      *****************************************************************
      * S400-MATCH                                                    *
      *****************************************************************
       S400-MATCH SECTION.

       P4200-MATCH-ACK.
      * THE CORRELID-DERIVED KEY MUST AGREE WITH THE KEY CARRIED IN
      * THE MESSAGE BODY.  COMMON MQ FAILURE HANDLING - SEE
      * CABS-STD-036 - APPLIES WHEN IT DOES NOT.
           MOVE 'P4200-MATCH-ACK' TO WS-PARA-NAME.
           IF WCB-TYPE = ACK-SETTLE-TYPE
              AND WCB-OCN = ACK-COUNTERPARTY-OCN
              AND WCB-PERIOD = ACK-SETTLE-PERIOD
              AND WCB-SEQ = ACK-SEQ
               CONTINUE
           ELSE
               MOVE 'M102' TO WS-ERR-CODE
               MOVE 'E' TO WS-ERR-SEVERITY
               GO TO P9995-MQ-FAILURE
           END-IF.

       P4200-EXIT.
           EXIT.


      *****************************************************************
      * S500-SYNCPOINT                                                *
      *****************************************************************
       S500-SYNCPOINT SECTION.

       P5000-COMMIT-CHECK.
           ADD 1 TO WS-SINCE-COMMIT.
           IF WS-SINCE-COMMIT < WS-PC-COMMIT-FREQ
               GO TO P5000-EXIT
           END-IF.
           CALL 'MQCMIT' USING W03-HCONN
                               W00-COMP-CODE
                               W00-REASON.
           IF W00-COMP-CODE NOT = MQCC-OK
               MOVE 7240 TO WS-AB-CODE
               MOVE 'MQCMIT FAILED' TO WS-AB-TEXT
               DISPLAY 'MQCMIT REASON ' W00-REASON
               PERFORM P9500-ABEND THRU P9500-EXIT
           END-IF.
           ADD 1 TO WS-COMMIT-CNT.
           MOVE ZERO TO WS-SINCE-COMMIT.

       P5000-EXIT.
           EXIT.


      *****************************************************************
      * S600-POST                                                     *
      *****************************************************************
       S600-POST SECTION.

       P6300-UPDATE-STATUS.
      * POST THE ACKNOWLEDGEMENT STATUS.  ADD IF THIS IS THE FIRST
      * ACKNOWLEDGEMENT FOR THE SETTLEMENT, OTHERWISE REPLACE - A
      * PARTNER GATEWAY CAN SEND A PENDING FOLLOWED LATER BY AN
      * ACCEPT OR REJECT FOR THE SAME KEY.
           MOVE 'P6300-UPDATE-STATUS' TO WS-PARA-NAME.
           MOVE WCB-TYPE TO AKS-KEY-TYPE.
           MOVE WCB-OCN TO AKS-KEY-OCN.
           MOVE WCB-PERIOD TO AKS-KEY-PERIOD.
           MOVE WCB-SEQ TO AKS-KEY-SEQ.
           READ ACK-STATUS-FILE
               INVALID KEY
                   CONTINUE
           END-READ.
           MOVE ACK-STATUS-CODE TO AKS-STATUS-CODE.
           MOVE ACK-REASON-TEXT TO AKS-REASON-TEXT.
           MOVE ACK-ACK-TS TO AKS-ACK-TS.
           MOVE ACK-ORIG-RUN-ID TO AKS-RUN-ID.
           IF WS-FS-OUTPUT = '00'
               REWRITE AKS-RECORD
                   INVALID KEY
                       MOVE 'M103' TO WS-ERR-CODE
                       MOVE 'E' TO WS-ERR-SEVERITY
                       GO TO P9995-MQ-FAILURE
               END-REWRITE
           ELSE
               WRITE AKS-RECORD
                   INVALID KEY
                       MOVE 'M104' TO WS-ERR-CODE
                       MOVE 'E' TO WS-ERR-SEVERITY
                       GO TO P9995-MQ-FAILURE
               END-WRITE
           END-IF.

       P6300-EXIT.
           EXIT.


      *****************************************************************
      * S700-SUSPENSE                                                 *
      *****************************************************************
       S700-SUSPENSE SECTION.

       P7000-SUSPEND.
      * WRITE A SUSPENSE RECORD.  THE CALLER SETS WS-ERR-CODE AND
      * WS-ERR-SEVERITY BEFORE PERFORMING THIS PARAGRAPH.
           MOVE SPACES                 TO CABS-SUSPENSE-RECORD.
           MOVE WS-ERR-CODE            TO SU-ERR-CODE.
           MOVE WS-ERR-SEVERITY        TO SU-ERR-SEVERITY.
           MOVE WS-PGM-NAME            TO SU-DETECT-PGM.
           MOVE WS-PARA-NAME           TO SU-DETECT-PARA.
           MOVE WS-RUN-ID              TO SU-RUN-ID.
           MOVE WS-SUSPENSE-ORIG       TO SU-ORIG-RECORD.
           CALL 'CABERRWR' USING CABS-SUSPENSE-RECORD
                                  WS-SUB-RC.
           WRITE SUS-RECORD FROM CABS-SUSPENSE-RECORD.
           ADD 1 TO WS-REJECT-CNT.
           MOVE 'Y' TO WS-ERROR-SW.

       P7000-EXIT.
           EXIT.


      *****************************************************************
      * S800-CONTROL                                                  *
      * BALANCING.  P8000 IS NOT OPTIONAL.                            *
      *****************************************************************
       S800-CONTROL SECTION.

       P8000-CONTROL.
      * MANDATORY CONTROL RECORD.  THE BALANCING EQUATION IS
      *   CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARISED
      *           + CT-CARRIED-FWD
           MOVE SPACES                 TO CABS-CONTROL-RECORD.
           MOVE WS-RUN-ID              TO CT-RUN-ID.
           MOVE WS-PGM-NAME            TO CT-PROCESS-ID.
           MOVE 720                    TO CT-STEP-SEQ.
           MOVE WS-CYCLE-YYDDD         TO CT-CYCLE-YYDDD.
           MOVE WS-BILL-PERIOD         TO CT-BILL-PERIOD.
           MOVE WS-RERUN-NBR           TO CT-RERUN-NBR.
           MOVE WS-JOBNAME             TO CT-JOBNAME.
           MOVE WS-STEPNAME            TO CT-STEPNAME.
           MOVE WS-READ-CNT            TO CT-READ.
           MOVE WS-WRITE-CNT           TO CT-WRITTEN.
           MOVE WS-REJECT-CNT          TO CT-REJECTED.
           MOVE WS-SUMM-CNT            TO CT-SUMMARISED.
           MOVE WS-CFWD-CNT            TO CT-CARRIED-FWD.
           MOVE WS-ACC-MINUTES         TO CT-HASH-MINUTES.
           MOVE WS-ACC-AMOUNT          TO CT-HASH-AMOUNT.
           MOVE WS-ACC-SEQ-HASH        TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH        TO CT-HASH-OCN.
           COMPUTE WS-SUB-RC =
                   WS-WRITE-CNT + WS-REJECT-CNT
                 + WS-SUMM-CNT  + WS-CFWD-CNT
                 - WS-READ-CNT.
           IF WS-SUB-RC = ZERO
               MOVE 'B' TO CT-BAL-IND
           ELSE
               MOVE 'O' TO CT-BAL-IND
               MOVE EC-OUT-OF-BALANCE TO WS-ERR-CODE
               MOVE 0008 TO WS-RETURN-CODE
               PERFORM P7000-SUSPEND THRU P7000-EXIT
           END-IF.
           MOVE WS-RETURN-CODE         TO CT-RC.
           MOVE WS-RESTART-KEY         TO CT-RESTART-KEY.
           CALL 'CABHASH ' USING CT-HASH-TOTALS
                                  WS-SUB-RC.
           WRITE CTL-RECORD FROM CABS-CONTROL-RECORD.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 7250 TO WS-AB-CODE
               MOVE 'CONTROL RECORD WRITE FAILED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT
           END-IF.

       P8000-EXIT.
           EXIT.


      *****************************************************************
      * S900-TERMINATION                                              *
      *****************************************************************
       S900-TERMINATION SECTION.

       P9000-TERM.
      * NORMAL END OF RUN.  THIS PARAGRAPH IS ALSO THE TARGET OF THE
      * COMMON MQ FAILURE HANDLER BELOW, SO THE RETURN CODE IS SET
      * BEFORE THIS RUNS EITHER WAY.
           DISPLAY '--------------------------------------------'.
           DISPLAY WS-PGM-NAME ' V' WS-PGM-VERSION ' RUN ' WS-RUN-ID.
           DISPLAY 'MESSAGES GOT     ' WS-MQGET-CNT.
           DISPLAY 'COMMITS TAKEN    ' WS-COMMIT-CNT.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY '--------------------------------------------'.
           CALL 'MQCMIT' USING W03-HCONN
                               W00-COMP-CODE
                               W00-REASON.
           MOVE MQCO-NONE TO W02-CLOSE-OPTIONS.
           CALL 'MQCLOSE' USING W03-HCONN
                                W03-HOBJ
                                W02-CLOSE-OPTIONS
                                W00-COMP-CODE
                                W00-REASON.
           CALL 'MQDISC' USING W03-HCONN
                               W00-COMP-CODE
                               W00-REASON.
           CLOSE PARM-FILE
                 CONTROL-FILE
                 SUSPENSE-FILE
                 ACK-STATUS-FILE
           END-CLOSE.
           MOVE WS-RETURN-CODE TO RETURN-CODE.
           STOP RUN.

       P9000-EXIT.
           EXIT.

       P9500-ABEND.
      * UNRECOVERABLE ERROR.  CABABEND ISSUES A USER ABEND SO THAT
      * THE STEP FAILS VISIBLY RATHER THAN COMPLETING WITH BAD DATA.
           MOVE WS-PGM-NAME            TO WS-AB-PGM.
           MOVE WS-PARA-NAME           TO WS-AB-PARA.
           MOVE WS-RESTART-KEY         TO WS-AB-KEY.
           DISPLAY '*** ABEND ' WS-AB-CODE ' IN ' WS-AB-PGM.
           DISPLAY '*** PARAGRAPH ' WS-AB-PARA.
           DISPLAY '*** ' WS-AB-TEXT.
           DISPLAY '*** LAST KEY ' WS-AB-KEY.
           CALL 'CABABEND' USING WS-ABEND-AREA.
           MOVE 16 TO WS-RETURN-CODE.
           STOP RUN.

       P9500-EXIT.
           EXIT.

       P9995-MQ-FAILURE.
      * COMMON MQ FAILURE HANDLING.  SEE CABS-STD-036.  WRITES THE
      * SUSPENSE RECORD FOR WHATEVER MESSAGE WAS IN PROGRESS,
      * COMMITS THE UNIT OF WORK POSTED SO FAR SO NOTHING BUILT
      * BEFORE THE FAILURE IS BACKED OUT ALONG WITH IT, THEN
      * REFRESHES THE RESTART KEY SO THE NEXT SUBMISSION PICKS UP
      * FROM WHERE THIS RUN STOPPED.
           MOVE WCB-TYPE TO WS-RK-TYPE.
           MOVE WCB-OCN TO WS-RK-OCN.
           MOVE WCB-PERIOD TO WS-RK-PERIOD.
           MOVE WCB-SEQ TO WS-RK-SEQ.
           PERFORM P7000-SUSPEND THRU P7000-EXIT.
           CALL 'MQCMIT' USING W03-HCONN
                               W00-COMP-CODE
                               W00-REASON.
           MOVE 0004 TO WS-RETURN-CODE.
           PERFORM P8000-CONTROL THRU P8000-EXIT.
           GO TO P9000-TERM.
