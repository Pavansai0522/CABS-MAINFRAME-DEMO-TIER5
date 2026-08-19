       IDENTIFICATION DIVISION.
      *****************************************************************
      * CABONL03 - CAB2 INVOICE INQUIRY                                *
      * APPLICATION : CABS (ONLINE)                                    *
      * COMPILER    : ENTERPRISE COBOL                                 *
      * TRANSACTION : CAB2                                             *
      * MAPSET      : CABM03 / BILLMAP                     CABM0300    *
      * FILES       : BILLHDR  TELCABS.CABS.BILLHDR  VSAM KSDS         *
      *                        RECORD LAYOUT               CABSBHDR   *
      *               BILLDTL  TELCABS.CABS.BILLDTL  VSAM KSDS         *
      *                        RECORD LAYOUT               CABSBILL   *
      *                        LOADED NIGHTLY FROM THE VB BATCH        *
      *                        OUTPUT (CABRAT10 BDTLOUT) BY THE         *
      *                        CABSBLD REPRO STEP - SEE JCL/CABS3300.  *
      * XCTL TO     : CABONL01 (PF3/PF12 RETURN TO MENU)               *
      * RUNNABLE    : REFERENCE-ONLY.  MVS 3.8J / TK4- CARRIES NO      *
      *               CICS REGION.  SEE ONLINE/_MANIFEST.MD.           *
      * REVISION HISTORY                                                *
      *   V1.00  1992-02-11  R.KESSLER    INITIAL - BILL HEADER READ   *
      *                                   BY AN ALTERNATE INDEX ON     *
      *                                   BH-INVOICE-NBR, WHICH AT     *
      *                                   THE TIME WAS NOT GUARANTEED  *
      *                                   UNIQUE ACROSS CARRIERS       *
      *   V1.01  1996-04-22  R.KESSLER    INVOICE NUMBERING MADE       *
      *                                   GLOBALLY UNIQUE BY CABRAT10; *
      *                                   PRIMARY KEY (BAN+PERIOD)     *
      *                                   READ SUBSTITUTED FOR THE     *
      *                                   ALTERNATE INDEX PATH         *
      *   V1.02  1998-09-30  D.OYELARAN   JURISDICTION SPLIT ADDED      *
      *                                   TO HEADER DISPLAY             *
      *   V1.03  2011-06-09  M.OYELARAN   RECOMPILE ONLY                *
      *****************************************************************
       PROGRAM-ID.    CABONL03.
       AUTHOR.        R.KESSLER.
       DATE-WRITTEN.  1992-02-11.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-PROGRAM-IDENT.
           05  WS-PGM-NAME              PIC X(08) VALUE 'CABONL03'.
           05  WS-PGM-VERSION           PIC X(05) VALUE 'V1.03'.

       01  WS-DATE-TIME-AREA.
           05  WS-ABSTIME               PIC S9(15) COMP-3.
           05  WS-DISP-DATE             PIC X(08).

       01  WS-MSG-AREA                  PIC X(60) VALUE SPACES.
       01  WS-DTL-LINE-CNT              PIC 9(01) VALUE 0.
       01  WS-DTL-FOUND-SW              PIC X(01) VALUE 'N'.
           88  WS-DTL-FOUND                 VALUE 'Y'.

      * DETAIL BROWSE KEY - PARTIAL KEY (BAN + PERIOD) USED TO
      * POSITION THE BROWSE WITH GTEQ.  THE FULL KEY INCLUDING
      * SECTION AND LINE SEQUENCE IS TAKEN FROM THE RECORD ITSELF
      * ONCE POSITIONED.
       01  WS-DTL-START-KEY.
           05  WS-DSK-BAN               PIC X(13).
           05  WS-DSK-PERIOD             PIC 9(06).
           05  WS-DSK-SECTION           PIC X(02) VALUE LOW-VALUES.
           05  WS-DSK-LINE-SEQ          PIC S9(07) COMP-3 VALUE 0.

           COPY DFHAID.
           COPY DFHBMSCA.
           COPY CABSBHDR.
           COPY CABSBILL.

      * SYMBOLIC MAP - GENERATED FROM CABM0300 BY THE TYPE=DSECT
      * ASSEMBLY.  NOT PHYSICALLY PRESENT IN SOURCE CONTROL.
           COPY CABM0300.

       LINKAGE SECTION.
           COPY CABSCOMM
               REPLACING ==CABS-COMM-AREA== BY ==DFHCOMMAREA==.

       PROCEDURE DIVISION.

       P0000-MAINLINE.
           EXEC CICS HANDLE CONDITION
                NOTFND     (P3100-NOTFND-HANDLER)
                DUPKEY     (P3200-DUPKEY-HANDLER)
                ENDFILE    (P3300-ENDFILE-HANDLER)
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
      * DUPKEY IS TRAPPED FOR HISTORICAL REASONS.  THE ORIGINAL    *
      * 1992 DESIGN READ BH-INVOICE-NBR THROUGH AN ALTERNATE       *
      * INDEX PATH WHERE DUPLICATE INVOICE NUMBERS COULD LEGALLY   *
      * OCCUR ACROSS CARRIERS.  THE HANDLER IS RETAINED IN CASE    *
      * THE ALTERNATE INDEX PATH IS EVER REINSTATED FOR THE        *
      * INVOICE-NUMBER LOOKUP UTILITY DESCRIBED IN CABS-STD-014.   *
      *-----------------------------------------------------------*
       P3200-DUPKEY-HANDLER.
           MOVE 'DUPLICATE INVOICE NUMBER - SEE SUPERVISOR'
                TO WS-MSG-AREA.
           GO TO P3900-COMMON-ERROR-RETURN.

       P3100-NOTFND-HANDLER.
           MOVE 'BAN/PERIOD NOT ON FILE' TO WS-MSG-AREA.
           GO TO P3900-COMMON-ERROR-RETURN.

       P3300-ENDFILE-HANDLER.
      * READPREV/READNEXT PAST THE HIGH OR LOW KEY.  SET BY THE
      * BROWSE PARAGRAPHS BEFORE THIS CAN FIRE - SEE P3500/P3600.
           CONTINUE.

       P3900-COMMON-ERROR-RETURN.
           PERFORM P1500-BUILD-CLOCK THRU P1500-EXIT.
           MOVE WS-MSG-AREA TO MSGLO.
           EXEC CICS SEND MAP('BILLMAP')
                MAPSET('CABM03')
                FROM(BILLMAPO)
                DATAONLY
                ALARM
                CURSOR
           END-EXEC.
           PERFORM P8000-RETURN-SAME THRU P8000-EXIT.

      *-----------------------------------------------------------*
      * P1000 - FIRST ENTRY.                                       *
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
           MOVE LOW-VALUES TO BILLMAPO.
           MOVE WS-DISP-DATE TO CDATO.
           EXEC CICS SEND MAP('BILLMAP')
                MAPSET('CABM03')
                FROM(BILLMAPO)
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
                   MOVE LOW-VALUES TO BILLMAPO
                   MOVE WS-DISP-DATE TO CDATO
                   EXEC CICS SEND MAP('BILLMAP')
                        MAPSET('CABM03')
                        FROM(BILLMAPO)
                        ERASE
                        CURSOR
                   END-EXEC
                   PERFORM P8000-RETURN-SAME THRU P8000-EXIT
               WHEN EIBAID = DFHPF3 OR EIBAID = DFHPF12
                   PERFORM P8500-RETURN-TO-MENU THRU P8500-EXIT
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
      * P2200 - RECEIVE BAN/PERIOD, READ THE HEADER, POSITION THE  *
      * DETAIL BROWSE ON THE FIRST FIVE LINES.                     *
      *-----------------------------------------------------------*
       P2200-RECEIVE-AND-LOOKUP.
           EXEC CICS RECEIVE MAP('BILLMAP')
                MAPSET('CABM03')
                INTO(BILLMAPI)
           END-EXEC.

           IF BANL = ZERO OR PERDL = ZERO
               MOVE 'BAN AND PERIOD ARE BOTH REQUIRED' TO WS-MSG-AREA
               GO TO P3900-COMMON-ERROR-RETURN
           END-IF.

           MOVE BANI  TO BH-BAN.
           MOVE PERDI TO BH-BILL-PERIOD.
           EXEC CICS READ DATASET('BILLHDR')
                RIDFLD(BH-KEY)
                INTO(CABS-BILL-HEADER)
           END-EXEC.

      * NOTFND, IF RAISED, TRANSFERS CONTROL VIA HANDLE CONDITION
      * AND NEVER REACHES THE FOLLOWING LINE.
           MOVE BH-BAN TO CM-SAVED-KEY-2.
           MOVE SPACES TO WS-MSG-AREA.
           PERFORM P4000-MOVE-HEADER-TO-MAP THRU P4000-EXIT.

           MOVE BH-BAN         TO WS-DSK-BAN.
           MOVE BH-BILL-PERIOD TO WS-DSK-PERIOD.
           MOVE LOW-VALUES     TO WS-DSK-SECTION.
           MOVE 0              TO WS-DSK-LINE-SEQ.
           MOVE 1              TO CM-BR-PAGE-NBR.
           SET CM-BR-FORWARD   TO TRUE.
           MOVE 'N'            TO CM-BR-EOF-SW.
           MOVE 'N'            TO CM-BR-BOF-SW.

           PERFORM P5000-FILL-DETAIL-FORWARD THRU P5000-EXIT.
           MOVE WS-DTL-START-KEY TO CM-BR-LAST-KEY.
           PERFORM P1500-BUILD-CLOCK THRU P1500-EXIT.
           MOVE WS-DISP-DATE TO CDATO.
           MOVE WS-MSG-AREA  TO MSGLO.
           EXEC CICS SEND MAP('BILLMAP')
                MAPSET('CABM03')
                FROM(BILLMAPO)
                DATAONLY
                CURSOR
           END-EXEC.
           PERFORM P8000-RETURN-SAME THRU P8000-EXIT.
       P2200-EXIT.
           EXIT.

      *-----------------------------------------------------------*
      * P3500 - PAGE FORWARD.  REQUIRES A DETAIL KEY FROM A PRIOR  *
      * INVOCATION - EITHER THIS TRANSACTION HAS NOT YET LOOKED UP *
      * A BAN/PERIOD THIS SESSION, OR CONTROL ARRIVED HERE VIA A   *
      * FRESH XCTL FROM THE MENU WITH NO CARRIED-FORWARD POSITION. *
      * EITHER WAY, WITHOUT A SAVED KEY THE BROWSE CANNOT RESUME.  *
      *-----------------------------------------------------------*
       P3500-BROWSE-FORWARD.
           IF CM-SAVED-KEY-2 = SPACES
               MOVE 'LOOK UP A BAN/PERIOD BEFORE PAGING'
                    TO WS-MSG-AREA
               GO TO P3900-COMMON-ERROR-RETURN
           END-IF.

      * WORKING STORAGE DOES NOT SURVIVE BETWEEN PSEUDO-CONVERSATIONAL
      * TURNS - THE POSITION HAS TO COME BACK FROM THE COMMAREA THAT
      * THE PRIOR TURN OF THIS SAME TRANSACTION SAVED IT INTO.
           MOVE CM-BR-LAST-KEY TO WS-DTL-START-KEY.
           PERFORM P5000-FILL-DETAIL-FORWARD THRU P5000-EXIT.
           MOVE WS-DTL-START-KEY TO CM-BR-LAST-KEY.
           PERFORM P1500-BUILD-CLOCK THRU P1500-EXIT.
           MOVE WS-DISP-DATE TO CDATO.
           MOVE WS-MSG-AREA  TO MSGLO.
           EXEC CICS SEND MAP('BILLMAP')
                MAPSET('CABM03')
                FROM(BILLMAPO)
                DATAONLY
                CURSOR
           END-EXEC.
           PERFORM P8000-RETURN-SAME THRU P8000-EXIT.
       P3500-EXIT.
           EXIT.

      *-----------------------------------------------------------*
      * P3600 - PAGE BACKWARD.  SAME DEPENDENCY AS P3500, PLUS A   *
      * SECOND GUARD ON CM-BR-BOF-SW SO REPEATED PF7 AT THE START  *
      * OF THE FILE DOES NOT RE-ISSUE THE BROWSE.                  *
      *-----------------------------------------------------------*
       P3600-BROWSE-BACKWARD.
           IF CM-SAVED-KEY-2 = SPACES
               MOVE 'LOOK UP A BAN/PERIOD BEFORE PAGING'
                    TO WS-MSG-AREA
               GO TO P3900-COMMON-ERROR-RETURN
           END-IF.
           IF CM-BR-AT-START
               MOVE 'ALREADY AT THE START OF THIS INVOICE'
                    TO WS-MSG-AREA
               GO TO P3900-COMMON-ERROR-RETURN
           END-IF.

           MOVE CM-BR-LAST-KEY TO WS-DTL-START-KEY.
           PERFORM P5100-FILL-DETAIL-BACKWARD THRU P5100-EXIT.
           MOVE WS-DTL-START-KEY TO CM-BR-LAST-KEY.
           PERFORM P1500-BUILD-CLOCK THRU P1500-EXIT.
           MOVE WS-DISP-DATE TO CDATO.
           MOVE WS-MSG-AREA  TO MSGLO.
           EXEC CICS SEND MAP('BILLMAP')
                MAPSET('CABM03')
                FROM(BILLMAPO)
                DATAONLY
                CURSOR
           END-EXEC.
           PERFORM P8000-RETURN-SAME THRU P8000-EXIT.
       P3600-EXIT.
           EXIT.

      *-----------------------------------------------------------*
      * P4000 - MOVE THE BILL HEADER TO THE MAP.                   *
      *-----------------------------------------------------------*
       P4000-MOVE-HEADER-TO-MAP.
           MOVE LOW-VALUES        TO BILLMAPO.
           MOVE BH-INVOICE-NBR    TO INVNO.
           MOVE BH-BILL-YYDDD     TO BDATO.
           MOVE BH-DUE-YYDDD      TO DDATO.
           MOVE BH-STATUS         TO STATO.
           MOVE BH-PRIOR-BAL      TO PBALO.
           MOVE BH-PAYMENTS       TO PYMTO.
           MOVE BH-ADJUSTMENTS    TO ADJTO.
           MOVE BH-CURR-USAGE     TO CURUO.
           MOVE BH-TOTAL-DUE      TO TOTDO.
       P4000-EXIT.
           EXIT.

      *-----------------------------------------------------------*
      * P5000 - POSITION AND READ FORWARD FOR FIVE DETAIL LINES.   *
      *-----------------------------------------------------------*
       P5000-FILL-DETAIL-FORWARD.
           MOVE SPACES TO DSEC1O DDSC1O DSEC2O DDSC2O DSEC3O DDSC3O
                          DSEC4O DDSC4O DSEC5O DDSC5O.
           MOVE ZERO TO DAMT1O DAMT2O DAMT3O DAMT4O DAMT5O.
           MOVE 0 TO WS-DTL-LINE-CNT.

           EXEC CICS STARTBR DATASET('BILLDTL')
                RIDFLD(WS-DTL-START-KEY)
                GTEQ
           END-EXEC.

           PERFORM VARYING WS-DTL-LINE-CNT FROM 1 BY 1
                   UNTIL WS-DTL-LINE-CNT > 5
               EXEC CICS READNEXT DATASET('BILLDTL')
                    RIDFLD(WS-DTL-START-KEY)
                    INTO(CABS-BILL-DETAIL)
               END-EXEC
               IF BD-BAN = WS-DSK-BAN AND
                  BD-BILL-PERIOD = WS-DSK-PERIOD
                   PERFORM P5200-MOVE-DETAIL-LINE THRU P5200-EXIT
               ELSE
                   SET CM-BR-AT-END TO TRUE
                   MOVE 6 TO WS-DTL-LINE-CNT
               END-IF
           END-PERFORM.

           EXEC CICS ENDBR DATASET('BILLDTL') END-EXEC.
       P5000-EXIT.
           EXIT.

      *-----------------------------------------------------------*
      * P5100 - POSITION AND READ BACKWARD FOR FIVE DETAIL LINES.  *
      *-----------------------------------------------------------*
       P5100-FILL-DETAIL-BACKWARD.
           MOVE SPACES TO DSEC1O DDSC1O DSEC2O DDSC2O DSEC3O DDSC3O
                          DSEC4O DDSC4O DSEC5O DDSC5O.
           MOVE ZERO TO DAMT1O DAMT2O DAMT3O DAMT4O DAMT5O.
           MOVE 0 TO WS-DTL-LINE-CNT.

           EXEC CICS STARTBR DATASET('BILLDTL')
                RIDFLD(WS-DTL-START-KEY)
                GTEQ
           END-EXEC.

           PERFORM VARYING WS-DTL-LINE-CNT FROM 1 BY 1
                   UNTIL WS-DTL-LINE-CNT > 5
               EXEC CICS READPREV DATASET('BILLDTL')
                    RIDFLD(WS-DTL-START-KEY)
                    INTO(CABS-BILL-DETAIL)
               END-EXEC
               IF BD-BAN = WS-DSK-BAN AND
                  BD-BILL-PERIOD = WS-DSK-PERIOD
                   PERFORM P5200-MOVE-DETAIL-LINE THRU P5200-EXIT
               ELSE
                   SET CM-BR-AT-START TO TRUE
                   MOVE 6 TO WS-DTL-LINE-CNT
               END-IF
           END-PERFORM.

           EXEC CICS ENDBR DATASET('BILLDTL') END-EXEC.
           IF CM-BR-PAGE-NBR > 1
               SUBTRACT 1 FROM CM-BR-PAGE-NBR
           END-IF.
       P5100-EXIT.
           EXIT.

      *-----------------------------------------------------------*
      * P5200 - MOVE ONE DETAIL RECORD TO THE APPROPRIATE SCREEN   *
      * LINE AND ADVANCE THE SAVED POSITION KEY.                   *
      *-----------------------------------------------------------*
       P5200-MOVE-DETAIL-LINE.
           MOVE BD-SECTION      TO WS-DSK-SECTION.
           MOVE BD-LINE-SEQ     TO WS-DSK-LINE-SEQ.
           EVALUATE WS-DTL-LINE-CNT
               WHEN 1
                   MOVE BD-SECTION     TO DSEC1O
                   MOVE BD-DESCRIPTION TO DDSC1O
                   MOVE BD-TOT-AMOUNT  TO DAMT1O
               WHEN 2
                   MOVE BD-SECTION     TO DSEC2O
                   MOVE BD-DESCRIPTION TO DDSC2O
                   MOVE BD-TOT-AMOUNT  TO DAMT2O
               WHEN 3
                   MOVE BD-SECTION     TO DSEC3O
                   MOVE BD-DESCRIPTION TO DDSC3O
                   MOVE BD-TOT-AMOUNT  TO DAMT3O
               WHEN 4
                   MOVE BD-SECTION     TO DSEC4O
                   MOVE BD-DESCRIPTION TO DDSC4O
                   MOVE BD-TOT-AMOUNT  TO DAMT4O
               WHEN 5
                   MOVE BD-SECTION     TO DSEC5O
                   MOVE BD-DESCRIPTION TO DDSC5O
                   MOVE BD-TOT-AMOUNT  TO DAMT5O
           END-EVALUATE.
       P5200-EXIT.
           EXIT.

      *-----------------------------------------------------------*
      * P2900 - MAPFAIL HANDLER.                                    *
      *-----------------------------------------------------------*
       P2900-MAPFAIL-HANDLER.
           MOVE 'ENTER A BAN AND PERIOD' TO WS-MSG-AREA.
           GO TO P3900-COMMON-ERROR-RETURN.

       P8000-RETURN-SAME.
           EXEC CICS RETURN
                TRANSID('CAB2')
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

       P9000-ABEND-HANDLER.
           EXEC CICS ABEND
                ABCODE('CB03')
           END-EXEC.
       P9000-EXIT.
           EXIT.
