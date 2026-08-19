       IDENTIFICATION DIVISION.
      *****************************************************************
      * CABONL05 - CAB4 DISPUTE ENTRY                                  *
      * APPLICATION : CABS (ONLINE)                                    *
      * COMPILER    : ENTERPRISE COBOL                                 *
      * TRANSACTION : CAB4                                             *
      * MAPSET      : CABM05 / DISPMAP                     CABM0500    *
      * FILES       : DISPUTE   TELCABS.CABS.DISPUTE  VSAM KSDS         *
      *               CREDMEMO  TELCABS.CABS.CREDMEMO VSAM KSDS         *
      *                         (SEE P6000 - DELEGATED AUTHORITY)       *
      * LINKS TO    : CABONL06 (CAB5) TO OBTAIN THE CURRENT SETTLEMENT *
      *               POSITION FOR THE OCN/PERIOD BEING DISPUTED.       *
      * XCTL TO     : CABONL01 (PF3/PF12 RETURN TO MENU)               *
      * RUNNABLE    : REFERENCE-ONLY.  MVS 3.8J / TK4- CARRIES NO      *
      *               CICS REGION.  SEE ONLINE/_MANIFEST.MD.           *
      * REVISION HISTORY                                                *
      *   V1.00  2004-05-14  S.OKONKWO    INITIAL - BUILT UNDER THE     *
      *                                   DISPUTE TRACKING INITIATIVE   *
      *   V1.01  2004-11-30  S.OKONKWO    DELEGATED-AUTHORITY CREDIT     *
      *                                   FACILITY ADDED UNDER CR-2291  *
      *   V1.02  2004-12-18  S.OKONKWO    AUTO-CREDIT SUSPENDED PENDING *
      *                                   TARIFF REVIEW - GATED BEHIND  *
      *                                   CM-AUTO-CREDIT-SW UNTIL THE   *
      *                                   REVIEW CLOSES                 *
      *   V1.03  2009-01-22  S.OKONKWO    CROSS-EDIT AGAINST THE        *
      *                                   MAINTENANCE FLAG BYTE PER     *
      *                                   CABS-STD-014 SECTION 4        *
      *   V1.04  2011-06-09  M.OYELARAN   RECOMPILE ONLY                *
      *****************************************************************
       PROGRAM-ID.    CABONL05.
       AUTHOR.        S.OKONKWO.
       DATE-WRITTEN.  2004-05-14.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-PROGRAM-IDENT.
           05  WS-PGM-NAME              PIC X(08) VALUE 'CABONL05'.
           05  WS-PGM-VERSION           PIC X(05) VALUE 'V1.04'.

       01  WS-DATE-TIME-AREA.
           05  WS-ABSTIME               PIC S9(15) COMP-3.
           05  WS-DISP-DATE             PIC X(08).
           05  WS-YYDDD                 PIC 9(05).

       01  WS-RESP-AREA.
           05  WS-RESP                  PIC S9(08) COMP.
           05  WS-RESP2                 PIC S9(08) COMP.

       01  WS-MSG-AREA                  PIC X(60) VALUE SPACES.
       01  WS-EDIT-OK-SW                PIC X(01) VALUE 'N'.
           88  WS-EDIT-OK                   VALUE 'Y'.
       01  WS-FROM-SETTLEMENT-SW        PIC X(01) VALUE 'N'.
           88  WS-FROM-SETTLEMENT           VALUE 'Y'.

      * WORK FIELDS FOR P6000 - SEE THE HEADER BOX AT P6000 FOR
      * WHAT THIS FACILITY IS.
       01  WS-CREDIT-PCT                PIC 9V99 VALUE 0.
       01  WS-AUTHORITY-LIMIT           PIC S9(11)V9(02) COMP-3 VALUE 0.

      * THE DISPUTE RECORD.  NOT ONE OF THE FROZEN COPYBOOKS -
      * DISPUTE TRACKING IS AN ONLINE-ONLY FILE WITH NO BATCH SIDE.
       01  CABS-DISPUTE-RECORD.
           05  DP-KEY.
               10  DP-BAN               PIC X(13).
               10  DP-BILL-PERIOD       PIC 9(06).
               10  DP-SEQ               PIC 9(04).
           05  DP-OCN                   PIC X(04).
           05  DP-REASON-CD             PIC X(02).
           05  DP-AMOUNT                PIC S9(11)V9(02) COMP-3.
           05  DP-STATUS                PIC X(01).
               88  DP-OPEN                  VALUE 'O'.
               88  DP-CREDITED              VALUE 'C'.
               88  DP-DENIED                VALUE 'D'.
           05  DP-ENTERED-YYDDD         PIC 9(05).
           05  DP-ENTERED-USERID        PIC X(08).
           05  DP-SETL-NET-DUE          PIC S9(13)V9(02) COMP-3.
           05  DP-SETL-DIRECTION        PIC X(01).
           05  DP-FILLER                PIC X(30).

      * CREDIT MEMO RECORD - WRITTEN ONLY BY P6000.  SEE THE HEADER
      * BOX AT P6000 FOR THE AUTHORITY THIS RECORD REPRESENTS.
       01  WS-CREDIT-MEMO-RECORD.
           05  CX-KEY.
               10  CX-BAN               PIC X(13).
               10  CX-BILL-PERIOD       PIC 9(06).
               10  CX-SEQ               PIC 9(04).
           05  CX-OCN                   PIC X(04).
           05  CX-CREDIT-AMT            PIC S9(11)V9(02) COMP-3.
           05  CX-AUTHORITY-CD          PIC X(04) VALUE 'DA01'.
           05  CX-ISSUED-YYDDD          PIC 9(05).
           05  CX-ISSUED-USERID         PIC X(08).
           05  CX-FILLER                PIC X(40).

           COPY DFHAID.
           COPY DFHBMSCA.

      * SYMBOLIC MAP - GENERATED FROM CABM0500 BY THE TYPE=DSECT
      * ASSEMBLY.  NOT PHYSICALLY PRESENT IN SOURCE CONTROL.
           COPY CABM0500.

       LINKAGE SECTION.
           COPY CABSCOMM
               REPLACING ==CABS-COMM-AREA== BY ==DFHCOMMAREA==.

      * LOCAL VIEW OF THE TAIL OF THE COMMAREA USED TO CARRY THE
      * SETTLEMENT POSITION BACK FROM A LINK TO CABONL06.  OFFSET
      * MATCHES CM-FILLER IN CABSCOMM - SEE CABS-STD-014 SECTION 5.
       01  LK-SETL-RESULT REDEFINES DFHCOMMAREA.
           05  FILLER                   PIC X(372).
           05  LK-SETL-NET-DUE          PIC S9(13)V9(02) COMP-3.
           05  LK-SETL-DIRECTION        PIC X(01).
           05  FILLER                   PIC X(11).

       PROCEDURE DIVISION.

       P0000-MAINLINE.
           EXEC CICS HANDLE CONDITION
                DUPREC     (P3950-DUPREC-HANDLER)
                MAPFAIL    (P2900-MAPFAIL-HANDLER)
                ERROR      (P9000-ABEND-HANDLER)
           END-EXEC.

           IF EIBCALEN = ZERO
               PERFORM P1000-FIRST-ENTRY THRU P1000-EXIT
           ELSE
               PERFORM P2000-PROCESS-INPUT THRU P2000-EXIT
           END-IF

           GOBACK.

      *-----------------------------------------------------------*
      * P1000 - FIRST ENTRY.  IN PRACTICE THIS TRANSACTION IS      *
      * ALWAYS REACHED WITH A COMMAREA (VIA THE MENU OR VIA        *
      * CABONL06) - THIS PATH ONLY COVERS AN OPERATOR TYPING       *
      * CAB4 DIRECTLY AT A CLEAR SCREEN.                            *
      *-----------------------------------------------------------*
       P1000-FIRST-ENTRY.
           EXEC CICS GETMAIN
                SET(ADDRESS OF DFHCOMMAREA)
                FLENGTH(LENGTH OF DFHCOMMAREA)
           END-EXEC.
           INITIALIZE DFHCOMMAREA.
           MOVE 'CAB0' TO CM-RETURN-TO.
           PERFORM P1500-BUILD-CLOCK THRU P1500-EXIT.
           MOVE SPACES TO WS-MSG-AREA.
           PERFORM P1800-SEND-ENTRY-SCREEN THRU P1800-EXIT.
           PERFORM P8000-RETURN-SAME THRU P8000-EXIT.
       P1000-EXIT.
           EXIT.

       P1500-BUILD-CLOCK.
           EXEC CICS ASKTIME
                ABSTIME(WS-ABSTIME)
           END-EXEC.
           EXEC CICS FORMATTIME
                ABSTIME(WS-ABSTIME)
                MMDDYYYY(WS-DISP-DATE)
           END-EXEC.
       P1500-EXIT.
           EXIT.

      *-----------------------------------------------------------*
      * P1800 - SEND THE ENTRY SCREEN.  WHEN CM-RETURN-TO IS CAB5  *
      * THE OPERATOR ARRIVED FROM THE SETTLEMENT INQUIRY SCREEN    *
      * WITH AN OCN/PERIOD ALREADY IN HAND - PREFILL THEM AND      *
      * DO NOT REQUIRE A BAN, SINCE A SETTLEMENT-LEVEL DISPUTE     *
      * IS NOT TIED TO A SINGLE INVOICE.  A DISPUTE ENTERED FROM   *
      * THE MAIN MENU (CM-RETURN-TO = CAB0) HAS NO PRIOR CONTEXT   *
      * AND REQUIRES THE FULL BAN/PERIOD/OCN COMBINATION.          *
      *-----------------------------------------------------------*
       P1800-SEND-ENTRY-SCREEN.
           MOVE LOW-VALUES TO DISPMAPO.
           MOVE WS-DISP-DATE TO CDATO.
           MOVE WS-MSG-AREA  TO MSGLO.
           IF CM-RETURN-TO = 'CAB5'
               SET WS-FROM-SETTLEMENT TO TRUE
               MOVE CM-SAVED-KEY-3(1:4) TO OCNNO
               MOVE CM-SAVED-KEY-3(5:6) TO PERDO
           ELSE
               SET WS-FROM-SETTLEMENT TO FALSE
           END-IF.
           EXEC CICS SEND MAP('DISPMAP')
                MAPSET('CABM05')
                FROM(DISPMAPO)
                ERASE
                CURSOR
           END-EXEC.
       P1800-EXIT.
           EXIT.

      *-----------------------------------------------------------*
      * P2000 - AID KEY DISPATCH.                                   *
      *-----------------------------------------------------------*
       P2000-PROCESS-INPUT.
           EVALUATE TRUE
               WHEN EIBAID = DFHCLEAR
                   MOVE SPACES TO WS-MSG-AREA
                   MOVE 'N' TO CM-CONFIRM-SW
                   PERFORM P1500-BUILD-CLOCK THRU P1500-EXIT
                   PERFORM P1800-SEND-ENTRY-SCREEN THRU P1800-EXIT
                   PERFORM P8000-RETURN-SAME THRU P8000-EXIT
               WHEN EIBAID = DFHPF3 OR EIBAID = DFHPF12
                   PERFORM P8500-RETURN-TO-MENU THRU P8500-EXIT
               WHEN CM-CONFIRM-PENDING
                   PERFORM P5500-PROCESS-CONFIRM THRU P5500-EXIT
               WHEN OTHER
                   PERFORM P3000-RECEIVE-AND-VALIDATE THRU P3000-EXIT
           END-EVALUATE.
       P2000-EXIT.
           EXIT.

      *-----------------------------------------------------------*
      * P2900 - MAPFAIL.                                            *
      *-----------------------------------------------------------*
       P2900-MAPFAIL-HANDLER.
           MOVE 'ENTER THE DISPUTE DETAILS' TO WS-MSG-AREA.
           PERFORM P1500-BUILD-CLOCK THRU P1500-EXIT.
           MOVE WS-MSG-AREA TO MSGLO.
           EXEC CICS SEND MAP('DISPMAP')
                MAPSET('CABM05')
                FROM(DISPMAPO)
                DATAONLY
                ALARM
                CURSOR
           END-EXEC.
           PERFORM P8000-RETURN-SAME THRU P8000-EXIT.

      *-----------------------------------------------------------*
      * P3000 - RECEIVE AND VALIDATE THE DISPUTE FIELDS.             *
      * THE CROSS-EDIT AGAINST CM-FLAG-5 BELOW IS THE ONLY PLACE    *
      * THIS TRANSACTION LOOKS AT THAT BYTE - SEE CABS-STD-014       *
      * SECTION 4.                                                   *
      *-----------------------------------------------------------*
       P3000-RECEIVE-AND-VALIDATE.
           EXEC CICS RECEIVE MAP('DISPMAP')
                MAPSET('CABM05')
                INTO(DISPMAPI)
           END-EXEC.

           MOVE 'Y' TO WS-EDIT-OK-SW.
           MOVE SPACES TO WS-MSG-AREA.

           IF CM-RETURN-TO NOT = 'CAB5' AND BANL = ZERO
               MOVE 'BAN IS REQUIRED' TO WS-MSG-AREA
               MOVE 'N' TO WS-EDIT-OK-SW
           END-IF.

           IF WS-EDIT-OK AND (OCNNL = ZERO OR PERDL = ZERO
                   OR RSNL = ZERO OR DAMTL = ZERO)
               MOVE 'OCN, PERIOD, REASON AND AMOUNT ARE ALL REQUIRED'
                    TO WS-MSG-AREA
               MOVE 'N' TO WS-EDIT-OK-SW
           END-IF.

           IF WS-EDIT-OK AND DAMTI NOT > ZERO
               MOVE 'DISPUTE AMOUNT MUST BE GREATER THAN ZERO'
                    TO WS-MSG-AREA
               MOVE 'N' TO WS-EDIT-OK-SW
           END-IF.

           IF WS-EDIT-OK AND CM-FLAG-5 = 'Y'
               MOVE 'FACTOR REVISED THIS CYCLE - DISPUTE NOT PERMITTED'
                    TO WS-MSG-AREA
               MOVE 'N' TO WS-EDIT-OK-SW
           END-IF.

           IF WS-EDIT-OK
               PERFORM P4000-GET-SETTLEMENT-POS THRU P4000-EXIT
           ELSE
               PERFORM P1500-BUILD-CLOCK THRU P1500-EXIT
               MOVE WS-MSG-AREA TO MSGLO
               EXEC CICS SEND MAP('DISPMAP')
                    MAPSET('CABM05')
                    FROM(DISPMAPO)
                    DATAONLY
                    ALARM
                    CURSOR
               END-EXEC
               PERFORM P8000-RETURN-SAME THRU P8000-EXIT
           END-IF.
       P3000-EXIT.
           EXIT.

      *-----------------------------------------------------------*
      * P4000 - LINK TO CABONL06 FOR THE CURRENT SETTLEMENT        *
      * POSITION.  CM-FUNCTION-CD OF 'SP' TELLS CABONL06 IT IS     *
      * BEING ENTERED AS A SUBROUTINE RATHER THAN A FRESH          *
      * TRANSACTION - SEE CABONL06 P0000-MAINLINE.                 *
      *-----------------------------------------------------------*
       P4000-GET-SETTLEMENT-POS.
           MOVE OCNNI TO CM-SAVED-KEY-3(1:4).
           MOVE PERDI TO CM-SAVED-KEY-3(5:6).
           MOVE 'SP' TO CM-FUNCTION-CD.

           EXEC CICS LINK PROGRAM('CABONL06')
                COMMAREA(DFHCOMMAREA)
                LENGTH(LENGTH OF DFHCOMMAREA)
                RESP(WS-RESP)
           END-EXEC.

           MOVE SPACES TO CM-FUNCTION-CD.

           IF WS-RESP = DFHRESP(NORMAL)
               MOVE LK-SETL-NET-DUE   TO SPOSO
               MOVE LK-SETL-DIRECTION TO SDIRO
           ELSE
               MOVE ZERO  TO SPOSO
               MOVE SPACES TO SDIRO
               MOVE 'NO SETTLEMENT POSITION ON FILE FOR THIS OCN/PERIOD'
                    TO WS-MSG-AREA
           END-IF.

           MOVE 'Y' TO CM-CONFIRM-SW.
           MOVE 'CONFIRM SUBMIT DISPUTE (Y/N)?' TO WS-MSG-AREA.
           PERFORM P1500-BUILD-CLOCK THRU P1500-EXIT.
           MOVE WS-DISP-DATE TO CDATO.
           MOVE WS-MSG-AREA  TO MSGLO.
           MOVE DFHBMFSE TO CONFA.
           EXEC CICS SEND MAP('DISPMAP')
                MAPSET('CABM05')
                FROM(DISPMAPO)
                DATAONLY
                CURSOR
           END-EXEC.
           PERFORM P8000-RETURN-SAME THRU P8000-EXIT.
       P4000-EXIT.
           EXIT.

      *-----------------------------------------------------------*
      * P5500 - CONFIRM RESPONSE.  RE-RECEIVES THE SAME FIELDS     *
      * (STILL IN THE 3270 BUFFER FROM THE PRIOR DATAONLY SEND)    *
      * RATHER THAN CARRYING THEM IN THE COMMAREA.                 *
      *-----------------------------------------------------------*
       P5500-PROCESS-CONFIRM.
           EXEC CICS RECEIVE MAP('DISPMAP')
                MAPSET('CABM05')
                INTO(DISPMAPI)
           END-EXEC.

           MOVE 'N' TO CM-CONFIRM-SW.

           IF CONFI = 'Y'
               PERFORM P5600-WRITE-DISPUTE THRU P5600-EXIT
           ELSE
               MOVE 'DISPUTE CANCELLED' TO WS-MSG-AREA
           END-IF.

           PERFORM P1500-BUILD-CLOCK THRU P1500-EXIT.
           PERFORM P1800-SEND-ENTRY-SCREEN THRU P1800-EXIT.
           PERFORM P8000-RETURN-SAME THRU P8000-EXIT.
       P5500-EXIT.
           EXIT.

      *-----------------------------------------------------------*
      * P5600 - WRITE THE DISPUTE RECORD AND SYNCPOINT.  THE       *
      * SEQUENCE NUMBER IS DERIVED FROM THE CICS TASK NUMBER -     *
      * ADEQUATE FOR AN ONLINE-ONLY KEY WITH NO BATCH SIDE TO       *
      * COORDINATE WITH.                                            *
      *-----------------------------------------------------------*
       P5600-WRITE-DISPUTE.
           IF CM-RETURN-TO = 'CAB5'
               MOVE SPACES TO DP-BAN
           ELSE
               MOVE BANI TO DP-BAN
           END-IF.
           MOVE PERDI TO DP-BILL-PERIOD.
           COMPUTE DP-SEQ = FUNCTION MOD(EIBTASKN, 9999).
           MOVE OCNNI TO DP-OCN.
           MOVE RSNI  TO DP-REASON-CD.
           MOVE DAMTI TO DP-AMOUNT.
           SET DP-OPEN TO TRUE.
           EXEC CICS ASKTIME
                ABSTIME(WS-ABSTIME)
           END-EXEC.
           EXEC CICS FORMATTIME
                ABSTIME(WS-ABSTIME)
                YYDDD(WS-YYDDD)
           END-EXEC.
           MOVE WS-YYDDD TO DP-ENTERED-YYDDD.
           MOVE CM-USERID TO DP-ENTERED-USERID.
           MOVE LK-SETL-NET-DUE TO DP-SETL-NET-DUE.
           MOVE LK-SETL-DIRECTION TO DP-SETL-DIRECTION.

           EXEC CICS WRITE DATASET('DISPUTE')
                RIDFLD(DP-KEY)
                FROM(CABS-DISPUTE-RECORD)
           END-EXEC.

           EXEC CICS SYNCPOINT END-EXEC.

           MOVE 'DISPUTE SUBMITTED' TO WS-MSG-AREA.
           PERFORM P6000-AUTO-CREDIT THRU P6000-EXIT.
       P5600-EXIT.
           EXIT.

       P3950-DUPREC-HANDLER.
      * TWO DISPUTES ON THE SAME OCN/PERIOD IN THE SAME CICS TASK
      * NUMBER WOULD COLLIDE HERE - HAS NOT HAPPENED IN PRODUCTION
      * BUT THE HANDLER COSTS NOTHING TO KEEP IN PLACE.
           MOVE 'DUPLICATE DISPUTE SEQUENCE - RESUBMIT' TO WS-MSG-AREA.
           PERFORM P1500-BUILD-CLOCK THRU P1500-EXIT.
           PERFORM P1800-SEND-ENTRY-SCREEN THRU P1800-EXIT.
           PERFORM P8000-RETURN-SAME THRU P8000-EXIT.

      *****************************************************************
      * P6000 - THE DELEGATED-AUTHORITY CREDIT FACILITY, INTRODUCED   *
      * UNDER CR-2291.  WHEN THE SETTLEMENT DEPARTMENT'S DELEGATED    *
      * AUTHORITY SWITCH IS ON FOR A DISPUTE, THE ADJUSTER MAY ISSUE  *
      * AN IMMEDIATE PARTIAL CREDIT WITHOUT WAITING FOR THE MANUAL    *
      * REVIEW CYCLE.  THE AUTHORITY LIMIT AND THE CREDIT PERCENTAGE  *
      * ARE BOTH SET BY THE SETTLEMENT DEPARTMENT AND ARE CARRIED IN  *
      * CM-AUTO-CREDIT-SW FOR THE DURATION OF THE DISPUTE ENTRY       *
      * CONVERSATION.  SEE CR-2291 AND CABS-STD-014 SECTION 7.        *
      *****************************************************************
       P6000-AUTO-CREDIT.
           IF CM-AUTO-CREDIT-SW = 'Y'
               PERFORM P6100-COMPUTE-CREDIT THRU P6100-EXIT
               PERFORM P6200-FORMAT-CREDIT-MEMO THRU P6200-EXIT
               PERFORM P6300-WRITE-CREDIT-MEMO THRU P6300-EXIT
           END-IF.
       P6000-EXIT.
           EXIT.

      *-----------------------------------------------------------*
      * P6100 - COMPUTE THE DELEGATED CREDIT AMOUNT.  THE PERCENT  *
      * OF THE DISPUTED AMOUNT THE ADJUSTER MAY CREDIT WITHOUT     *
      * MANUAL REVIEW VARIES BY REASON CODE - A METERING ERROR     *
      * (RSN 01) CARRIES A HIGHER DELEGATED PERCENTAGE THAN A      *
      * RATE DISAGREEMENT (RSN 04), WHICH USUALLY NEEDS A TARIFF   *
      * LOOKUP THE ADJUSTER CANNOT DO FROM THIS SCREEN.  CAPPED AT *
      * THE AUTHORITY LIMIT ON THE OPERATOR'S PROFILE REGARDLESS   *
      * OF WHAT THE PERCENTAGE TABLE ALLOWS.                       *
      *-----------------------------------------------------------*
       P6100-COMPUTE-CREDIT.
           MOVE ZERO TO CX-CREDIT-AMT.
           MOVE 0.50 TO WS-CREDIT-PCT.
           EVALUATE RSNI
               WHEN '01'
                   MOVE 0.75 TO WS-CREDIT-PCT
               WHEN '02'
                   MOVE 0.60 TO WS-CREDIT-PCT
               WHEN '03'
                   MOVE 0.50 TO WS-CREDIT-PCT
               WHEN '04'
                   MOVE 0.25 TO WS-CREDIT-PCT
               WHEN OTHER
                   MOVE 0.50 TO WS-CREDIT-PCT
           END-EVALUATE.

           COMPUTE CX-CREDIT-AMT ROUNDED = DAMTI * WS-CREDIT-PCT.

           MOVE 5000.00 TO WS-AUTHORITY-LIMIT.
           IF CM-USERID(1:2) = 'SU'
               MOVE 15000.00 TO WS-AUTHORITY-LIMIT
           END-IF.

           IF CX-CREDIT-AMT > WS-AUTHORITY-LIMIT
               MOVE WS-AUTHORITY-LIMIT TO CX-CREDIT-AMT
           END-IF.
       P6100-EXIT.
           EXIT.

      *-----------------------------------------------------------*
      * P6200 - FORMAT THE CREDIT MEMO KEY AND IDENTIFYING FIELDS. *
      *-----------------------------------------------------------*
       P6200-FORMAT-CREDIT-MEMO.
           MOVE DP-BAN          TO CX-BAN.
           MOVE DP-BILL-PERIOD  TO CX-BILL-PERIOD.
           MOVE DP-SEQ          TO CX-SEQ.
           MOVE DP-OCN          TO CX-OCN.
           MOVE 'DA01'          TO CX-AUTHORITY-CD.
           MOVE WS-YYDDD        TO CX-ISSUED-YYDDD.
           MOVE CM-USERID       TO CX-ISSUED-USERID.
       P6200-EXIT.
           EXIT.

      *-----------------------------------------------------------*
      * P6300 - WRITE THE CREDIT MEMO RECORD.                      *
      *-----------------------------------------------------------*
       P6300-WRITE-CREDIT-MEMO.
           EXEC CICS WRITE DATASET('CREDMEMO')
                RIDFLD(CX-KEY)
                FROM(WS-CREDIT-MEMO-RECORD)
                RESP(WS-RESP)
           END-EXEC.
           IF WS-RESP = DFHRESP(NORMAL)
               SET DP-CREDITED TO TRUE
               EXEC CICS REWRITE DATASET('DISPUTE')
                    FROM(CABS-DISPUTE-RECORD)
                    RESP(WS-RESP)
               END-EXEC
           END-IF.
       P6300-EXIT.
           EXIT.

       P8000-RETURN-SAME.
           EXEC CICS RETURN
                TRANSID('CAB4')
                COMMAREA(DFHCOMMAREA)
                LENGTH(LENGTH OF DFHCOMMAREA)
           END-EXEC.
       P8000-EXIT.
           EXIT.

       P8500-RETURN-TO-MENU.
           MOVE SPACES TO CM-MSG-TEXT.
           MOVE 'N' TO CM-CONFIRM-SW.
           EXEC CICS XCTL
                PROGRAM('CABONL01')
                COMMAREA(DFHCOMMAREA)
                LENGTH(LENGTH OF DFHCOMMAREA)
           END-EXEC.
       P8500-EXIT.
           EXIT.

       P9000-ABEND-HANDLER.
           EXEC CICS ABEND
                ABCODE('CB05')
           END-EXEC.
       P9000-EXIT.
           EXIT.
