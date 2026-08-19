       IDENTIFICATION DIVISION.
      *****************************************************************
      * CABONL06 - CAB5 SETTLEMENT INQUIRY                             *
      * APPLICATION : CABS (ONLINE)                                    *
      * COMPILER    : ENTERPRISE COBOL                                 *
      * TRANSACTION : CAB5                                             *
      * MAPSET      : CABM06 / SETLMAP                     CABM0600    *
      * FILE        : SETLMST   TELCABS.CABS.SETTLE   VSAM KSDS         *
      *               RECORD LAYOUT                       CABSSETL    *
      * DUAL MODE   : ENTERED AS A TRANSACTION (CAB5, EIBCALEN > 0     *
      *               WITH CM-FUNCTION-CD SPACES) IT BROWSES THE       *
      *               SETTLEMENT FILE INTERACTIVELY.  ENTERED VIA A    *
      *               LINK FROM CABONL05 (CM-FUNCTION-CD = 'SP') IT    *
      *               RETURNS ONE SETTLEMENT POSITION AND SENDS NO     *
      *               MAP.  SEE P0000-MAINLINE.                        *
      * XCTL TO     : CABONL01 (PF3/PF12) AND CABONL05 (PF4, DISPUTE   *
      *               THE SETTLEMENT CURRENTLY DISPLAYED)              *
      * RUNNABLE    : REFERENCE-ONLY.  MVS 3.8J / TK4- CARRIES NO      *
      *               CICS REGION.  SEE ONLINE/_MANIFEST.MD.           *
      * REVISION HISTORY                                                *
      *   V1.00  2004-05-14  S.OKONKWO    INITIAL - BUILT ALONGSIDE    *
      *                                   CABONL05 UNDER THE DISPUTE   *
      *                                   TRACKING INITIATIVE          *
      *   V1.01  2009-01-22  S.OKONKWO    LINKABLE SUBROUTINE MODE     *
      *                                   ADDED FOR CABONL05            *
      *   V1.02  2011-06-09  M.OYELARAN   RECOMPILE ONLY                *
      *****************************************************************
       PROGRAM-ID.    CABONL06.
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
           05  WS-PGM-NAME              PIC X(08) VALUE 'CABONL06'.
           05  WS-PGM-VERSION           PIC X(05) VALUE 'V1.02'.

       01  WS-DATE-TIME-AREA.
           05  WS-ABSTIME               PIC S9(15) COMP-3.
           05  WS-DISP-DATE             PIC X(08).

       01  WS-RESP-AREA.
           05  WS-RESP                  PIC S9(08) COMP.
           05  WS-RESP2                 PIC S9(08) COMP.

       01  WS-MSG-AREA                  PIC X(60) VALUE SPACES.
       01  WS-BROWSE-KEY                PIC X(16) VALUE SPACES.
       01  WS-SUBR-KEY                  PIC X(16) VALUE SPACES.
       01  WS-SUBR-SCAN-CNT             PIC 9(02) VALUE 0.
       01  WS-SUBR-FOUND-SW             PIC X(01) VALUE 'N'.
           88  WS-SUBR-FOUND                VALUE 'Y'.

           COPY DFHAID.
           COPY DFHBMSCA.
           COPY CABSSETL.

      * SYMBOLIC MAP - GENERATED FROM CABM0600 BY THE TYPE=DSECT
      * ASSEMBLY.  NOT PHYSICALLY PRESENT IN SOURCE CONTROL.  NOT
      * REFERENCED AT ALL WHEN THIS PROGRAM IS RUNNING LINKED.
           COPY CABM0600.

       LINKAGE SECTION.
           COPY CABSCOMM
               REPLACING ==CABS-COMM-AREA== BY ==DFHCOMMAREA==.

      * SAME TAIL-OF-COMMAREA VIEW CABONL05 USES TO RECEIVE THE
      * SETTLEMENT POSITION BACK.  SEE CABONL05 FOR THE OFFSET NOTE.
       01  LK-SETL-RESULT REDEFINES DFHCOMMAREA.
           05  FILLER                   PIC X(372).
           05  LK-SETL-NET-DUE          PIC S9(13)V9(02) COMP-3.
           05  LK-SETL-DIRECTION        PIC X(01).
           05  FILLER                   PIC X(11).

       PROCEDURE DIVISION.

      *-----------------------------------------------------------*
      * P0000 - EIBCALEN TELLS US A COMMAREA CAME IN, BUT NOT      *
      * WHETHER WE WERE XCTL'D/ATTACHED AS THE CAB5 TRANSACTION OR *
      * LINKED AS A SUBROUTINE.  CM-FUNCTION-CD = 'SP' IS THE ONLY *
      * SIGNAL FOR THE LATTER - SET BY CABONL05 IMMEDIATELY BEFORE *
      * THE LINK AND CLEARED IMMEDIATELY AFTER IT RETURNS.          *
      *-----------------------------------------------------------*
       P0000-MAINLINE.
           EXEC CICS HANDLE CONDITION
                NOTFND     (P3100-NOTFND-HANDLER)
                ENDFILE    (P3100-NOTFND-HANDLER)
                MAPFAIL    (P2900-MAPFAIL-HANDLER)
                ERROR      (P9000-ABEND-HANDLER)
           END-EXEC.

           IF EIBCALEN > ZERO AND CM-FUNCTION-CD = 'SP'
               PERFORM P6000-SUBROUTINE-LOOKUP THRU P6000-EXIT
               EXEC CICS RETURN END-EXEC
           ELSE
               IF EIBCALEN = ZERO
                   PERFORM P1000-FIRST-ENTRY THRU P1000-EXIT
               ELSE
                   PERFORM P2000-PROCESS-INPUT THRU P2000-EXIT
               END-IF
           END-IF

           GOBACK.

      *-----------------------------------------------------------*
      * P1000 - FIRST ENTRY AS A TRANSACTION.                       *
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
           MOVE LOW-VALUES TO SETLMAPO.
           MOVE WS-DISP-DATE TO CDATO.
           EXEC CICS SEND MAP('SETLMAP')
                MAPSET('CABM06')
                FROM(SETLMAPO)
                ERASE
                CURSOR
           END-EXEC.
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
      * P2000 - AID KEY DISPATCH.                                   *
      *-----------------------------------------------------------*
       P2000-PROCESS-INPUT.
           EVALUATE TRUE
               WHEN EIBAID = DFHCLEAR
                   MOVE SPACES TO WS-MSG-AREA
                   PERFORM P1500-BUILD-CLOCK THRU P1500-EXIT
                   MOVE LOW-VALUES TO SETLMAPO
                   MOVE WS-DISP-DATE TO CDATO
                   EXEC CICS SEND MAP('SETLMAP')
                        MAPSET('CABM06')
                        FROM(SETLMAPO)
                        ERASE
                        CURSOR
                   END-EXEC
                   PERFORM P8000-RETURN-SAME THRU P8000-EXIT
               WHEN EIBAID = DFHPF3 OR EIBAID = DFHPF12
                   PERFORM P8500-RETURN-TO-MENU THRU P8500-EXIT
               WHEN EIBAID = DFHPF4
                   PERFORM P8600-DISPUTE-THIS-SETTLEMENT THRU P8600-EXIT
               WHEN EIBAID = DFHPF7
                   PERFORM P3600-BROWSE-BACKWARD THRU P3600-EXIT
               WHEN EIBAID = DFHPF8
                   PERFORM P3500-BROWSE-FORWARD THRU P3500-EXIT
               WHEN OTHER
                   PERFORM P2200-RECEIVE-AND-LOOKUP THRU P2200-EXIT
           END-EVALUATE.
       P2000-EXIT.
           EXIT.

      *-----------------------------------------------------------*
      * P2200 - RECEIVE THE SEARCH KEY AND POSITION THE BROWSE.    *
      *-----------------------------------------------------------*
       P2200-RECEIVE-AND-LOOKUP.
           EXEC CICS RECEIVE MAP('SETLMAP')
                MAPSET('CABM06')
                INTO(SETLMAPI)
           END-EXEC.

           IF COCNL = ZERO OR STYPL = ZERO OR PERDL = ZERO
               MOVE 'COUNTERPARTY OCN, TYPE AND PERIOD ARE ALL REQUIRED'
                    TO WS-MSG-AREA
               PERFORM P1500-BUILD-CLOCK THRU P1500-EXIT
               MOVE WS-MSG-AREA TO MSGLO
               EXEC CICS SEND MAP('SETLMAP')
                    MAPSET('CABM06')
                    FROM(SETLMAPO)
                    DATAONLY
                    ALARM
                    CURSOR
               END-EXEC
               PERFORM P8000-RETURN-SAME THRU P8000-EXIT
           END-IF.

           MOVE STYPI TO ST-SETTLE-TYPE.
           MOVE COCNI TO ST-COUNTERPARTY-OCN.
           MOVE PERDI TO ST-SETTLE-PERIOD.
           MOVE 0     TO ST-SEQ.

           EXEC CICS STARTBR DATASET('SETLMST')
                RIDFLD(ST-KEY)
                GTEQ
           END-EXEC.
           EXEC CICS READNEXT DATASET('SETLMST')
                RIDFLD(ST-KEY)
                INTO(CABS-SETTLEMENT-RECORD)
           END-EXEC.
           EXEC CICS ENDBR DATASET('SETLMST') END-EXEC.

           IF ST-SETTLE-TYPE = STYPI AND ST-COUNTERPARTY-OCN = COCNI
                   AND ST-SETTLE-PERIOD = PERDI
               MOVE ST-KEY TO CM-BR-LAST-KEY(1:16)
               MOVE ST-COUNTERPARTY-OCN TO CM-SAVED-KEY-3(1:4)
               MOVE ST-SETTLE-PERIOD    TO CM-SAVED-KEY-3(5:6)
               MOVE 1 TO CM-BR-PAGE-NBR
               SET CM-BR-FORWARD TO TRUE
               MOVE SPACES TO WS-MSG-AREA
               PERFORM P4000-MOVE-RECORD-TO-MAP THRU P4000-EXIT
           ELSE
               MOVE 'NO SETTLEMENT RECORD FOR THAT KEY' TO WS-MSG-AREA
           END-IF.

           PERFORM P1500-BUILD-CLOCK THRU P1500-EXIT.
           MOVE WS-DISP-DATE TO CDATO.
           MOVE WS-MSG-AREA  TO MSGLO.
           EXEC CICS SEND MAP('SETLMAP')
                MAPSET('CABM06')
                FROM(SETLMAPO)
                DATAONLY
                CURSOR
           END-EXEC.
           PERFORM P8000-RETURN-SAME THRU P8000-EXIT.
       P2200-EXIT.
           EXIT.

      *-----------------------------------------------------------*
      * P3500 - BROWSE FORWARD THROUGH THE WHOLE FILE FROM THE     *
      * LAST POSITION SAVED IN THE COMMAREA.                        *
      *-----------------------------------------------------------*
       P3500-BROWSE-FORWARD.
           IF CM-BR-LAST-KEY(1:16) = SPACES
               MOVE 'LOOK UP A SETTLEMENT BEFORE PAGING' TO WS-MSG-AREA
               PERFORM P1500-BUILD-CLOCK THRU P1500-EXIT
               MOVE WS-MSG-AREA TO MSGLO
               EXEC CICS SEND MAP('SETLMAP')
                    MAPSET('CABM06')
                    FROM(SETLMAPO)
                    DATAONLY
                    ALARM
                    CURSOR
               END-EXEC
               PERFORM P8000-RETURN-SAME THRU P8000-EXIT
           END-IF.

           MOVE CM-BR-LAST-KEY(1:16) TO WS-BROWSE-KEY.
           EXEC CICS STARTBR DATASET('SETLMST')
                RIDFLD(WS-BROWSE-KEY)
                RESP(WS-RESP)
           END-EXEC.
           EXEC CICS READNEXT DATASET('SETLMST')
                RIDFLD(WS-BROWSE-KEY)
                INTO(CABS-SETTLEMENT-RECORD)
                RESP(WS-RESP)
           END-EXEC.
           EXEC CICS READNEXT DATASET('SETLMST')
                RIDFLD(WS-BROWSE-KEY)
                INTO(CABS-SETTLEMENT-RECORD)
                RESP(WS-RESP)
           END-EXEC.
           EXEC CICS ENDBR DATASET('SETLMST') END-EXEC.

           IF WS-RESP = DFHRESP(NORMAL)
               MOVE ST-KEY TO CM-BR-LAST-KEY(1:16)
               MOVE ST-COUNTERPARTY-OCN TO CM-SAVED-KEY-3(1:4)
               MOVE ST-SETTLE-PERIOD    TO CM-SAVED-KEY-3(5:6)
               ADD 1 TO CM-BR-PAGE-NBR
               SET CM-BR-FORWARD TO TRUE
               MOVE SPACES TO WS-MSG-AREA
               PERFORM P4000-MOVE-RECORD-TO-MAP THRU P4000-EXIT
           ELSE
               SET CM-BR-AT-END TO TRUE
               MOVE 'END OF SETTLEMENT FILE' TO WS-MSG-AREA
           END-IF.
           PERFORM P1500-BUILD-CLOCK THRU P1500-EXIT.
           MOVE WS-DISP-DATE TO CDATO.
           MOVE WS-MSG-AREA  TO MSGLO.
           EXEC CICS SEND MAP('SETLMAP')
                MAPSET('CABM06')
                FROM(SETLMAPO)
                DATAONLY
                CURSOR
           END-EXEC.
           PERFORM P8000-RETURN-SAME THRU P8000-EXIT.
       P3500-EXIT.
           EXIT.

      *-----------------------------------------------------------*
      * P3600 - BROWSE BACKWARD.  SAME DEPENDENCY AS P3500.         *
      *-----------------------------------------------------------*
       P3600-BROWSE-BACKWARD.
           IF CM-BR-LAST-KEY(1:16) = SPACES
               MOVE 'LOOK UP A SETTLEMENT BEFORE PAGING' TO WS-MSG-AREA
               PERFORM P1500-BUILD-CLOCK THRU P1500-EXIT
               MOVE WS-MSG-AREA TO MSGLO
               EXEC CICS SEND MAP('SETLMAP')
                    MAPSET('CABM06')
                    FROM(SETLMAPO)
                    DATAONLY
                    ALARM
                    CURSOR
               END-EXEC
               PERFORM P8000-RETURN-SAME THRU P8000-EXIT
           END-IF.

           MOVE CM-BR-LAST-KEY(1:16) TO WS-BROWSE-KEY.
           EXEC CICS STARTBR DATASET('SETLMST')
                RIDFLD(WS-BROWSE-KEY)
                RESP(WS-RESP)
           END-EXEC.
           EXEC CICS READPREV DATASET('SETLMST')
                RIDFLD(WS-BROWSE-KEY)
                INTO(CABS-SETTLEMENT-RECORD)
                RESP(WS-RESP)
           END-EXEC.
           EXEC CICS READPREV DATASET('SETLMST')
                RIDFLD(WS-BROWSE-KEY)
                INTO(CABS-SETTLEMENT-RECORD)
                RESP(WS-RESP)
           END-EXEC.
           EXEC CICS ENDBR DATASET('SETLMST') END-EXEC.

           IF WS-RESP = DFHRESP(NORMAL)
               MOVE ST-KEY TO CM-BR-LAST-KEY(1:16)
               MOVE ST-COUNTERPARTY-OCN TO CM-SAVED-KEY-3(1:4)
               MOVE ST-SETTLE-PERIOD    TO CM-SAVED-KEY-3(5:6)
               IF CM-BR-PAGE-NBR > 1
                   SUBTRACT 1 FROM CM-BR-PAGE-NBR
               END-IF
               SET CM-BR-BACKWARD TO TRUE
               MOVE SPACES TO WS-MSG-AREA
               PERFORM P4000-MOVE-RECORD-TO-MAP THRU P4000-EXIT
           ELSE
               SET CM-BR-AT-START TO TRUE
               MOVE 'START OF SETTLEMENT FILE' TO WS-MSG-AREA
           END-IF.
           PERFORM P1500-BUILD-CLOCK THRU P1500-EXIT.
           MOVE WS-DISP-DATE TO CDATO.
           MOVE WS-MSG-AREA  TO MSGLO.
           EXEC CICS SEND MAP('SETLMAP')
                MAPSET('CABM06')
                FROM(SETLMAPO)
                DATAONLY
                CURSOR
           END-EXEC.
           PERFORM P8000-RETURN-SAME THRU P8000-EXIT.
       P3600-EXIT.
           EXIT.

      *-----------------------------------------------------------*
      * P4000 - MOVE THE CURRENT SETTLEMENT RECORD TO THE MAP.     *
      *-----------------------------------------------------------*
       P4000-MOVE-RECORD-TO-MAP.
           MOVE LOW-VALUES     TO SETLMAPO.
           MOVE ST-COUNTERPARTY-OCN TO COCNO.
           MOVE ST-SETTLE-TYPE      TO STYPO.
           MOVE ST-SETTLE-PERIOD    TO PERDO.
           MOVE ST-TOTAL-MOU        TO TMOUO.
           MOVE ST-BILLABLE-MOU     TO BMOUO.
           MOVE ST-GROSS-AMT        TO GAMTO.
           MOVE ST-OUR-SHARE        TO OSHRO.
           MOVE ST-THEIR-SHARE      TO TSHRO.
           MOVE ST-NET-DUE          TO NDUEO.
           MOVE ST-DIRECTION        TO DIRO.
           MOVE ST-DISPUTE-SW       TO DSWO.
       P4000-EXIT.
           EXIT.

      *-----------------------------------------------------------*
      * P6000 - SUBROUTINE MODE.  OCN AND PERIOD ARRIVE IN         *
      * CM-SAVED-KEY-3.  THE FIRST SETTLEMENT RECORD FOUND FOR     *
      * THAT OCN/PERIOD, REGARDLESS OF TYPE, IS RETURNED - A       *
      * DISPUTE IS AGAINST THE OVERALL POSITION, NOT ONE           *
      * SETTLEMENT KIND.  NO MAP I/O HAPPENS IN THIS PATH.         *
      *-----------------------------------------------------------*
       P6000-SUBROUTINE-LOOKUP.
           MOVE LOW-VALUES TO WS-SUBR-KEY.
           MOVE ZERO TO LK-SETL-NET-DUE.
           MOVE SPACES TO LK-SETL-DIRECTION.
           MOVE 'N' TO WS-SUBR-FOUND-SW.

           EXEC CICS STARTBR DATASET('SETLMST')
                RIDFLD(WS-SUBR-KEY)
                GTEQ
                RESP(WS-RESP)
           END-EXEC.

           IF WS-RESP = DFHRESP(NORMAL)
               PERFORM VARYING WS-SUBR-SCAN-CNT FROM 1 BY 1
                       UNTIL WS-SUBR-SCAN-CNT > 50
                          OR WS-SUBR-FOUND
                   EXEC CICS READNEXT DATASET('SETLMST')
                        RIDFLD(WS-SUBR-KEY)
                        INTO(CABS-SETTLEMENT-RECORD)
                        RESP(WS-RESP)
                   END-EXEC
                   IF WS-RESP NOT = DFHRESP(NORMAL)
                       MOVE 51 TO WS-SUBR-SCAN-CNT
                   ELSE
                       IF ST-COUNTERPARTY-OCN = CM-SAVED-KEY-3(1:4)
                          AND ST-SETTLE-PERIOD = CM-SAVED-KEY-3(5:6)
                           MOVE ST-NET-DUE    TO LK-SETL-NET-DUE
                           MOVE ST-DIRECTION  TO LK-SETL-DIRECTION
                           MOVE 'Y' TO WS-SUBR-FOUND-SW
                       END-IF
                   END-IF
               END-PERFORM
               EXEC CICS ENDBR DATASET('SETLMST') END-EXEC
           END-IF.
       P6000-EXIT.
           EXIT.

       P2900-MAPFAIL-HANDLER.
           MOVE 'ENTER THE SEARCH KEY' TO WS-MSG-AREA.
           PERFORM P1500-BUILD-CLOCK THRU P1500-EXIT.
           MOVE WS-MSG-AREA TO MSGLO.
           EXEC CICS SEND MAP('SETLMAP')
                MAPSET('CABM06')
                FROM(SETLMAPO)
                DATAONLY
                ALARM
                CURSOR
           END-EXEC.
           PERFORM P8000-RETURN-SAME THRU P8000-EXIT.

       P3100-NOTFND-HANDLER.
      * ONLY THE DIRECT-LOOKUP READNEXT IN P2200 CAN RAISE NOTFND -
      * THE BROWSE PARAGRAPHS TEST RESP INSTEAD.  TREATED THE SAME
      * AS A KEY THAT SORTS PAST END OF FILE.
           MOVE 'NO SETTLEMENT RECORD FOR THAT KEY' TO WS-MSG-AREA.
           PERFORM P1500-BUILD-CLOCK THRU P1500-EXIT.
           MOVE WS-MSG-AREA TO MSGLO.
           EXEC CICS SEND MAP('SETLMAP')
                MAPSET('CABM06')
                FROM(SETLMAPO)
                DATAONLY
                ALARM
                CURSOR
           END-EXEC.
           PERFORM P8000-RETURN-SAME THRU P8000-EXIT.

       P8000-RETURN-SAME.
           EXEC CICS RETURN
                TRANSID('CAB5')
                COMMAREA(DFHCOMMAREA)
                LENGTH(LENGTH OF DFHCOMMAREA)
           END-EXEC.
       P8000-EXIT.
           EXIT.

       P8500-RETURN-TO-MENU.
           MOVE SPACES TO CM-MSG-TEXT.
           EXEC CICS XCTL
                PROGRAM('CABONL01')
                COMMAREA(DFHCOMMAREA)
                LENGTH(LENGTH OF DFHCOMMAREA)
           END-EXEC.
       P8500-EXIT.
           EXIT.

      *-----------------------------------------------------------*
      * P8600 - PF4.  HAND OFF TO DISPUTE ENTRY WITH THE CURRENTLY *
      * DISPLAYED OCN/PERIOD ALREADY IN CM-SAVED-KEY-3.             *
      *-----------------------------------------------------------*
       P8600-DISPUTE-THIS-SETTLEMENT.
           IF CM-SAVED-KEY-3 = SPACES
               MOVE 'LOOK UP A SETTLEMENT BEFORE DISPUTING IT'
                    TO WS-MSG-AREA
               PERFORM P1500-BUILD-CLOCK THRU P1500-EXIT
               MOVE WS-MSG-AREA TO MSGLO
               EXEC CICS SEND MAP('SETLMAP')
                    MAPSET('CABM06')
                    FROM(SETLMAPO)
                    DATAONLY
                    ALARM
                    CURSOR
               END-EXEC
               PERFORM P8000-RETURN-SAME THRU P8000-EXIT
           END-IF.

           MOVE 'CAB5' TO CM-RETURN-TO.
           EXEC CICS XCTL
                PROGRAM('CABONL05')
                COMMAREA(DFHCOMMAREA)
                LENGTH(LENGTH OF DFHCOMMAREA)
           END-EXEC.
       P8600-EXIT.
           EXIT.

       P9000-ABEND-HANDLER.
           EXEC CICS ABEND
                ABCODE('CB06')
           END-EXEC.
       P9000-EXIT.
           EXIT.
