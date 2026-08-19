      *****************************************************************
      * CABCTL07 - OUTBOUND SETTLEMENT GATEWAY                        *
      * APPLICATION : SETL                                            *
      * COMPILER    : ENTERPRISE COBOL                                *
      * INPUTS      : SETLIN   TELCABS.SETL.NET(0)             CABSSETL*
      * INPUTS      : SYSIN    INSTREAM                       NONE    *
      * OUTPUTS     : MQ       CABS.SETTLE.OUT ON CSQ1         NONE    *
      * OUTPUTS     : SUSPOUT  TELCABS.SETL.SUSPENSE(+1)        CABSERR*
      * CONTROL     : CTLOUT                         CABSCTL          *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED               *
      *               + CT-CARRIED-FWD (RESEND-MODE SKIP COUNT)        *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY VIA THE RESEND   *
      *               CARD (SEE P1200-EDIT-PARM)                       *
      * COMPILED WITH ENTERPRISE COBOL AND LINK-EDITED WITH THE MQ    *
      * COBOL STUB CSQBSTUB.  SCOPE TERMINATORS PERMITTED HERE.        *
      * REVISION HISTORY                                              *
      *   V1.00  1996-11-04  T.OKONKWO   INITIAL GEN, MQSERIES 2.1     *
      *   V1.02  1998-05-19  T.OKONKWO   EXPIRY MADE A SYSIN PARAMETER *
      *   V1.05  2001-09-27  D.WASILEWSKI RESEND MODE ADDED UNDER      *
      *                                  CR-2940 AFTER THE OCTOBER     *
      *                                  GATEWAY OUTAGE                *
      *   V1.06  2003-02-14  D.WASILEWSKI REPLYTOQ POPULATED SO THE    *
      *                                  PARTNER GATEWAY CAN ROUTE A   *
      *                                  SYNCHRONOUS NACK BACK TO US   *
      *   V2.00  2009-07-30  A.BUKOWSKI  COMMIT FREQUENCY RAISED TO    *
      *                                  250 FROM 100 - SEE CABS-STD-  *
      *                                  036 CHANGE LOG                *
      *   V2.02  2015-11-12  M.OYELARAN  RECOMPILE, QUEUE MANAGER      *
      *                                  RENAMED CSQ1 (WAS CSQP)       *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CABCTL07.
       AUTHOR.        T.OKONKWO.
       DATE-WRITTEN.  1996-11-04.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * NEW SETTLEMENTS TO BE ANNOUNCED TO THE TRADING PARTNER GATEWAY.
           SELECT SETTLE-NET-FILE
               ASSIGN TO UT-S-SETLIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
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
      * SUSPENSE - SETTLEMENTS THE GATEWAY QUEUE COULD NOT ACCEPT.
           SELECT SUSPENSE-FILE
               ASSIGN TO UT-S-SUSPOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-SUSPENSE.

       DATA DIVISION.
       FILE SECTION.
       FD  SETTLE-NET-FILE
               RECORDING MODE IS F
               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 200 CHARACTERS
               DATA RECORD IS SNI-RECORD.
       01  SNI-RECORD              PIC X(200).

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

       WORKING-STORAGE SECTION.

      * PROGRAM IDENTIFICATION - MOVED TO THE CONTROL RECORD AND TO
      * EVERY SUSPENSE RECORD RAISED BY THIS MODULE.
       01  WS-PROGRAM-IDENT.
           05  WS-PGM-NAME         PIC X(08)   VALUE 'CABCTL07'.
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
           05  WS-RESTART-KEY      PIC X(26)   VALUE SPACES.
           05  WS-SUB-RC           PIC S9(04) COMP  VALUE 0.

       COPY CABSWRK.

       COPY CABSSETL.

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
           05  WS-PC-EXPIRY        PIC 9(05).
           05  WS-PC-COMMIT-FREQ   PIC 9(05).
           05  WS-PC-RESEND-SW     PIC X(01).
           05  WS-PC-RESTART-KEY.
               10  WS-PC-RK-TYPE       PIC X(01).
               10  WS-PC-RK-OCN        PIC X(04).
               10  WS-PC-RK-PERIOD     PIC 9(06).
               10  WS-PC-RK-SEQ        PIC 9(09).
           05  FILLER              PIC X(06).

      * LOCAL SWITCHES.  THE STANDARD ONES ARRIVE VIA CABSWRK.
       01  WS-LOCAL-SWITCHES.
           05  WS-PARM-EOF-SW          PIC X(01)   VALUE 'N'.
               88  WS-PARM-EOF                 VALUE 'Y'.
           05  WS-RESEND-SW            PIC X(01)   VALUE 'N'.
               88  WS-RESEND-MODE              VALUE 'Y'.
           05  WS-RESTART-MATCHED-SW   PIC X(01)   VALUE 'Y'.
               88  WS-RESTART-MATCHED          VALUE 'Y'.

      * THE 300-BYTE FIXED GATEWAY MESSAGE.  ONE PER SETTLEMENT.
      * THE PARTNER GATEWAY SPEC IS TELCABS-GW-014 (SEE DOCS).
       01  WS-GATEWAY-MSG.
           05  GWM-REC-TYPE            PIC X(02)   VALUE 'ST'.
           05  GWM-SETTLE-TYPE         PIC X(01).
           05  GWM-COUNTERPARTY-OCN    PIC X(04).
           05  GWM-SETTLE-PERIOD       PIC 9(06).
           05  GWM-SEQ                 PIC 9(09).
           05  GWM-DIRECTION           PIC X(01).
           05  GWM-TOTAL-MOU           PIC S9(15)V9(02).
           05  GWM-NET-DUE             PIC S9(13)V9(02).
           05  GWM-RUN-ID              PIC X(12).
           05  GWM-SEND-TS             PIC X(14).
           05  GWM-RESEND-IND          PIC X(01).
           05  FILLER                  PIC X(218)  VALUE SPACES.

      * CORRELATION ID BUILD AREA.  MOVED INTO MQMD-CORRELID SO THE
      * ACKNOWLEDGEMENT CONSUMER (CABCTL08) CAN MATCH THE REPLY BACK
      * TO THIS SETTLEMENT WITHOUT PARSING THE MESSAGE BODY.
       01  WS-CORRELID-BUILD.
           05  WCB-TYPE                PIC X(01).
           05  WCB-OCN                 PIC X(04).
           05  WCB-PERIOD              PIC 9(06).
           05  WCB-SEQ                 PIC 9(09).
           05  FILLER                  PIC X(04)   VALUE SPACES.

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

      * MQ RUN COUNTERS AND SWITCHES.
       01  WS-MQ-COUNTERS.
           05  WS-MQPUT-CNT            PIC S9(09) COMP-3   VALUE 0.
           05  WS-COMMIT-CNT           PIC S9(09) COMP-3   VALUE 0.
           05  WS-SINCE-COMMIT         PIC S9(09) COMP-3   VALUE 0.
           05  WS-MQ-OK-SW             PIC X(01)   VALUE 'Y'.
               88  WS-MQ-OK                    VALUE 'Y'.

      * ABEND COMMUNICATION AREA.  PASSED TO CABABEND WHICH ISSUES
      * A USER ABEND WITH THE CODE IN WS-AB-CODE.
       01  WS-ABEND-AREA.
           05  WS-AB-CODE              PIC 9(04) COMP  VALUE 0.
           05  WS-AB-PGM               PIC X(08)   VALUE SPACES.
           05  WS-AB-PARA              PIC X(30)   VALUE SPACES.
           05  WS-AB-TEXT               PIC X(60)   VALUE SPACES.
           05  WS-AB-KEY                PIC X(26)   VALUE SPACES.

      * ACCEPT AREAS FOR THE SEND TIMESTAMP.
       01  WS-ACCEPT-AREAS.
           05  WS-ACCEPT-DATE          PIC 9(06)   VALUE 0.
           05  WS-ACCEPT-TIME          PIC 9(08)   VALUE 0.

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
           STOP RUN.


      *****************************************************************
      * S100-INITIALISATION                                           *
      * OPEN, READ THE CONTROL CARD, CONNECT AND OPEN THE QUEUE.      *
      *****************************************************************
       S100-INITIALISATION SECTION.

       P1000-INIT.
           MOVE 'P1000-INIT' TO WS-PARA-NAME.
           ACCEPT WS-ACCEPT-DATE FROM DATE.
           ACCEPT WS-ACCEPT-TIME FROM TIME.
           OPEN INPUT  SETTLE-NET-FILE
                       PARM-FILE
           OPEN OUTPUT CONTROL-FILE
                       SUSPENSE-FILE
           END-OPEN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 7101 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-SETLIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT
           END-IF.
           IF WS-FS-TABLE NOT = '00'
               MOVE 7102 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-SYSIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT
           END-IF.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 7103 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-CTLOUT' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT
           END-IF.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 7104 TO WS-AB-CODE
               MOVE 'OPEN FAILED UT-S-SUSPOUT' TO WS-AB-TEXT
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
           IF WS-RESEND-MODE
               DISPLAY '  RESEND MODE - RESTART KEY ' WS-PC-RESTART-KEY
               MOVE 'N' TO WS-RESTART-MATCHED-SW
           END-IF.

       P1000-EXIT.
           EXIT.

       P1100-READ-PARM.
      * THE SYSIN CARD CARRIES THE VALUES THE SCHEDULER SUBSTITUTED
      * INTO THE JCL AT SUBMISSION TIME.  THERE ARE NO DEFAULTS.
           MOVE 'P1100-READ-PARM' TO WS-PARA-NAME.
           MOVE SPACES TO WS-PARM-CARD.
           READ PARM-FILE INTO WS-PARM-CARD
               AT END
                   MOVE 'Y' TO WS-PARM-EOF-SW
           END-READ.
           IF WS-PARM-EOF
               MOVE 7105 TO WS-AB-CODE
               MOVE 'NO SYSIN CONTROL CARD SUPPLIED' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT
           END-IF.
           IF WS-PC-TYPE NOT = 'GW'
               MOVE 7106 TO WS-AB-CODE
               MOVE 'SYSIN CARD TYPE NOT GW' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT
           END-IF.

       P1100-EXIT.
           EXIT.

       P1200-EDIT-PARM.
      * EDIT THE CONTROL CARD.  EXPIRY AND COMMIT FREQUENCY DEFAULT
      * WHEN BLANK OR NON-NUMERIC - EVERYTHING ELSE IS MANDATORY.
           MOVE 'P1200-EDIT-PARM' TO WS-PARA-NAME.
           IF WS-PC-CYCLE NOT NUMERIC
               MOVE 7107 TO WS-AB-CODE
               MOVE 'CYCLE DATE NOT NUMERIC' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT
           END-IF.
           IF WS-PC-BILL-PERIOD NOT NUMERIC
               MOVE 7108 TO WS-AB-CODE
               MOVE 'BILL PERIOD NOT NUMERIC' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT
           END-IF.
           IF WS-PC-RERUN NOT NUMERIC
               MOVE ZERO TO WS-PC-RERUN
           END-IF.
           IF WS-PC-EXPIRY NOT NUMERIC OR WS-PC-EXPIRY = ZERO
               MOVE 06000 TO WS-PC-EXPIRY
           END-IF.
           IF WS-PC-COMMIT-FREQ NOT NUMERIC OR WS-PC-COMMIT-FREQ = ZERO
               MOVE 00250 TO WS-PC-COMMIT-FREQ
           END-IF.
           IF WS-PC-RESEND-SW = 'Y'
               MOVE 'Y' TO WS-RESEND-SW
           ELSE
               MOVE 'N' TO WS-RESEND-SW
           END-IF.

       P1200-EXIT.
           EXIT.

       P1300-MQ-CONNECT.
      * CONNECT TO THE SETTLEMENT QUEUE MANAGER AND OPEN THE
      * OUTBOUND QUEUE FOR PUT.  A FAILURE HERE IS ALWAYS FATAL -
      * THERE IS NO DEGRADED MODE FOR THE GATEWAY.
           MOVE 'P1300-MQ-CONNECT' TO WS-PARA-NAME.
           CALL 'MQCONN' USING W00-QM-NAME
                               W03-HCONN
                               W00-COMP-CODE
                               W00-REASON.
           IF W00-COMP-CODE NOT = MQCC-OK
               MOVE 7110 TO WS-AB-CODE
               MOVE 'MQCONN FAILED - SEE REASON IN DISPLAY' TO
                    WS-AB-TEXT
               DISPLAY 'MQCONN REASON ' W00-REASON
               PERFORM P9500-ABEND THRU P9500-EXIT
           END-IF.
           MOVE SPACES TO MQOD-OBJECTNAME.
           MOVE 'CABS.SETTLE.OUT' TO MQOD-OBJECTNAME.
           MOVE MQOT-Q TO MQOD-OBJECTTYPE.
           COMPUTE W02-OPEN-OPTIONS =
                   MQOO-OUTPUT + MQOO-FAIL-IF-QUIESCING.
           CALL 'MQOPEN' USING W03-HCONN
                               MQOD
                               W02-OPEN-OPTIONS
                               W03-HOBJ
                               W00-COMP-CODE
                               W00-REASON.
           IF W00-COMP-CODE NOT = MQCC-OK
               MOVE 7111 TO WS-AB-CODE
               MOVE 'MQOPEN FAILED ON CABS.SETTLE.OUT' TO WS-AB-TEXT
               DISPLAY 'MQOPEN REASON ' W00-REASON
               PERFORM P9500-ABEND THRU P9500-EXIT
           END-IF.

       P1300-EXIT.
           EXIT.


      *****************************************************************
      * S200-GATEWAY                                                  *
      * READ, FILTER FOR RESEND, BUILD THE MESSAGE, PUT IT.           *
      *****************************************************************
       S200-GATEWAY SECTION.

       P2000-PROCESS.
           MOVE 'P2000-PROCESS' TO WS-PARA-NAME.
           PERFORM P2100-READ THRU P2100-EXIT.
           IF WS-EOF
               GO TO P2000-EXIT
           END-IF.
           MOVE SNI-RECORD TO CABS-SETTLEMENT-RECORD.
           MOVE ST-KEY TO WS-RESTART-KEY.
           PERFORM P2150-CHECK-RESTART THRU P2150-EXIT.
           IF NOT WS-RESTART-MATCHED
               GO TO P2000-EXIT
           END-IF.
           PERFORM P2200-BUILD-MSG THRU P2200-EXIT.
           PERFORM P3000-MQ-PUT THRU P3000-EXIT.
           PERFORM P5000-COMMIT-CHECK THRU P5000-EXIT.

       P2000-EXIT.
           EXIT.

       P2100-READ.
      * SEQUENTIAL READ OF THE NET SETTLEMENT FILE.
           READ SETTLE-NET-FILE
               AT END
                   MOVE 'Y' TO WS-EOF-SW
                   GO TO P2100-EXIT
           END-READ.
           IF WS-FS-INPUT NOT = '00'
               MOVE 7120 TO WS-AB-CODE
               MOVE 'READ ERROR UT-S-SETLIN' TO WS-AB-TEXT
               PERFORM P9500-ABEND THRU P9500-EXIT
           END-IF.
           ADD 1 TO WS-READ-CNT.

       P2100-EXIT.
           EXIT.

       P2150-CHECK-RESTART.
      * IN A NORMAL RUN WS-RESTART-MATCHED-SW IS ALREADY 'Y' AND THIS
      * PARAGRAPH IS A NO-OP.  IN RESEND MODE, RECORDS BEFORE THE
      * RESTART KEY WERE ALREADY ACCEPTED BY THE GATEWAY ON THE PRIOR
      * RUN AND ARE CARRIED FORWARD WITHOUT BEING RESENT.
           IF WS-RESTART-MATCHED
               GO TO P2150-EXIT
           END-IF.
           IF ST-SETTLE-TYPE = WS-PC-RK-TYPE
              AND ST-COUNTERPARTY-OCN = WS-PC-RK-OCN
              AND ST-SETTLE-PERIOD = WS-PC-RK-PERIOD
              AND ST-SEQ = WS-PC-RK-SEQ
               MOVE 'Y' TO WS-RESTART-MATCHED-SW
           ELSE
               ADD 1 TO WS-CFWD-CNT
           END-IF.

       P2150-EXIT.
           EXIT.

       P2200-BUILD-MSG.
      * MOVE THE SETTLEMENT INTO THE FIXED GATEWAY LAYOUT.
           MOVE SPACES TO WS-GATEWAY-MSG.
           MOVE 'ST' TO GWM-REC-TYPE.
           MOVE ST-SETTLE-TYPE TO GWM-SETTLE-TYPE.
           MOVE ST-COUNTERPARTY-OCN TO GWM-COUNTERPARTY-OCN.
           MOVE ST-SETTLE-PERIOD TO GWM-SETTLE-PERIOD.
           MOVE ST-SEQ TO GWM-SEQ.
           MOVE ST-DIRECTION TO GWM-DIRECTION.
           MOVE ST-TOTAL-MOU TO GWM-TOTAL-MOU.
           MOVE ST-NET-DUE TO GWM-NET-DUE.
           MOVE WS-RUN-ID TO GWM-RUN-ID.
           STRING WS-ACCEPT-DATE WS-ACCEPT-TIME DELIMITED BY SIZE
               INTO GWM-SEND-TS
           END-STRING.
           IF WS-RESEND-MODE
               MOVE 'Y' TO GWM-RESEND-IND
           ELSE
               MOVE 'N' TO GWM-RESEND-IND
           END-IF.
           MOVE WS-GATEWAY-MSG TO W04-BUFFER.

       P2200-EXIT.
           EXIT.


      *****************************************************************
      * S300-MQPUT                                                    *
      * PUT ONE MESSAGE.  A FULL QUEUE IS TOLERATED AND SUSPENDED -   *
      * EVERYTHING ELSE IS FATAL.                                    *
      *****************************************************************
       S300-MQPUT SECTION.

       P3000-MQ-PUT.
           MOVE 'P3000-MQ-PUT' TO WS-PARA-NAME.
           MOVE SPACES TO MQMD-FORMAT.
           MOVE MQFMT-STRING TO MQMD-FORMAT.
           MOVE MQPER-PERSISTENT TO MQMD-PERSISTENCE.
           MOVE WS-PC-EXPIRY TO MQMD-EXPIRY.
           MOVE ST-SETTLE-TYPE TO WCB-TYPE.
           MOVE ST-COUNTERPARTY-OCN TO WCB-OCN.
           MOVE ST-SETTLE-PERIOD TO WCB-PERIOD.
           MOVE ST-SEQ TO WCB-SEQ.
           MOVE WS-CORRELID-BUILD TO MQMD-CORRELID.
      * REPLYTOQ TELLS THE PARTNER GATEWAY WHERE TO ROUTE A
      * SYNCHRONOUS NACK IF THE MESSAGE FAILS THEIR EDIT.  THE
      * SETTLEMENT ACKNOWLEDGEMENT CONSUMER READS CABS.SETTLE.ACK
      * FOR THE NORMAL ACCEPT/REJECT FLOW - SEE CABS-STD-036.
           MOVE 'CABS.SETTLE.NACK ' TO MQMD-REPLYTOQ.
           COMPUTE W02-CLOSE-OPTIONS = MQPMO-SYNCPOINT
                                     + MQPMO-NEW-MSG-ID.
           MOVE W02-CLOSE-OPTIONS TO MQPMO-OPTIONS.
           CALL 'MQPUT' USING W03-HCONN
                              W03-HOBJ
                              MQMD
                              MQPMO
                              W04-BUFFLEN
                              W04-BUFFER
                              W00-COMP-CODE
                              W00-REASON.
           IF W00-COMP-CODE = MQCC-OK
               ADD 1 TO WS-MQPUT-CNT
               ADD 1 TO WS-WRITE-CNT
               ADD ST-NET-DUE TO WS-ACC-AMOUNT
               ADD ST-TOTAL-MOU TO WS-ACC-MINUTES
           ELSE
               IF W00-REASON = MQRC-Q-FULL
                   MOVE 'M001' TO WS-ERR-CODE
                   MOVE 'E' TO WS-ERR-SEVERITY
                   PERFORM P7000-SUSPEND THRU P7000-EXIT
               ELSE
                   MOVE 7130 TO WS-AB-CODE
                   MOVE 'MQPUT FAILED ON CABS.SETTLE.OUT' TO
                        WS-AB-TEXT
                   DISPLAY 'MQPUT REASON ' W00-REASON
                   PERFORM P9500-ABEND THRU P9500-EXIT
               END-IF
           END-IF.

       P3000-EXIT.
           EXIT.


      *****************************************************************
      * S500-SYNCPOINT                                                *
      *****************************************************************
       S500-SYNCPOINT SECTION.

       P5000-COMMIT-CHECK.
      * SYNCPOINT ON A MESSAGE COUNT.  RAISED TO 250 IN 2009 - SEE
      * REVISION HISTORY.
           ADD 1 TO WS-SINCE-COMMIT.
           IF WS-SINCE-COMMIT < WS-PC-COMMIT-FREQ
               GO TO P5000-EXIT
           END-IF.
           CALL 'MQCMIT' USING W03-HCONN
                               W00-COMP-CODE
                               W00-REASON.
           IF W00-COMP-CODE NOT = MQCC-OK
               MOVE 7140 TO WS-AB-CODE
               MOVE 'MQCMIT FAILED' TO WS-AB-TEXT
               DISPLAY 'MQCMIT REASON ' W00-REASON
               PERFORM P9500-ABEND THRU P9500-EXIT
           END-IF.
           ADD 1 TO WS-COMMIT-CNT.
           MOVE ZERO TO WS-SINCE-COMMIT.

       P5000-EXIT.
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
           MOVE CABS-SETTLEMENT-RECORD TO SU-ORIG-RECORD.
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
      * CARRIED-FWD HERE IS THE COUNT OF RECORDS SKIPPED BECAUSE
      * THEY WERE ALREADY ACCEPTED BY THE GATEWAY BEFORE A RESEND.
           MOVE SPACES                 TO CABS-CONTROL-RECORD.
           MOVE WS-RUN-ID              TO CT-RUN-ID.
           MOVE WS-PGM-NAME            TO CT-PROCESS-ID.
           MOVE 710                    TO CT-STEP-SEQ.
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
               MOVE 7150 TO WS-AB-CODE
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
           DISPLAY '--------------------------------------------'.
           DISPLAY WS-PGM-NAME ' V' WS-PGM-VERSION ' RUN ' WS-RUN-ID.
           DISPLAY 'MESSAGES PUT     ' WS-MQPUT-CNT.
           DISPLAY 'COMMITS TAKEN    ' WS-COMMIT-CNT.
           DISPLAY 'RECORDS READ      ' WS-READ-CNT.
           DISPLAY 'RECORDS WRITTEN   ' WS-WRITE-CNT.
           DISPLAY 'RECORDS REJECTED  ' WS-REJECT-CNT.
           DISPLAY 'CARRIED FORWARD   ' WS-CFWD-CNT.
           DISPLAY 'AMOUNT HASH       ' WS-ACC-AMOUNT.
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
           CLOSE SETTLE-NET-FILE
                 PARM-FILE
                 CONTROL-FILE
                 SUSPENSE-FILE
           END-CLOSE.
           MOVE WS-RETURN-CODE TO RETURN-CODE.

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
