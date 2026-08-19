       IDENTIFICATION DIVISION.
      *****************************************************************
      * CABONL02 - CAB1 CARRIER (OCN) INQUIRY                          *
      * APPLICATION : CABS (ONLINE)                                    *
      * COMPILER    : ENTERPRISE COBOL                                 *
      * TRANSACTION : CAB1                                             *
      * MAPSET      : CABM02 / CARRMAP                     CABM0200    *
      * FILE        : CARRMST  TELCABS.CABS.CARRIER  VSAM KSDS         *
      *               RECORD LAYOUT                       CABSCARR    *
      * XCTL TO     : CABONL01 (PF3/PF12 RETURN TO MENU)               *
      * RUNNABLE    : REFERENCE-ONLY.  MVS 3.8J / TK4- CARRIES NO      *
      *               CICS REGION.  SEE ONLINE/_MANIFEST.MD.           *
      * REVISION HISTORY                                                *
      *   V1.00  1991-04-09  R.KESSLER    INITIAL                      *
      *   V1.01  1997-01-14  R.KESSLER    ISP CAP / RECIP FIELDS ADDED *
      *   V2.00  2016-08-30  K.VANCE      REWRITTEN TO RESP-STYLE       *
      *                                   CONDITION HANDLING PER        *
      *                                   CABS-STD-014 REV 1 - THE      *
      *                                   FIRST PROGRAM IN THE SUITE    *
      *                                   CONVERTED.  HANDLE CONDITION  *
      *                                   REMOVED ENTIRELY.             *
      *   V2.01  2011-06-09  M.OYELARAN   RECOMPILE ONLY                *
      *****************************************************************
       PROGRAM-ID.    CABONL02.
       AUTHOR.        R.KESSLER.
       DATE-WRITTEN.  1991-04-09.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-PROGRAM-IDENT.
           05  WS-PGM-NAME              PIC X(08) VALUE 'CABONL02'.
           05  WS-PGM-VERSION           PIC X(05) VALUE 'V2.01'.

       01  WS-DATE-TIME-AREA.
           05  WS-ABSTIME               PIC S9(15) COMP-3.
           05  WS-DISP-DATE             PIC X(08).

       01  WS-RESP-AREA.
           05  WS-RESP                  PIC S9(08) COMP.
           05  WS-RESP2                 PIC S9(08) COMP.

       01  WS-BROWSE-KEY                PIC X(04) VALUE SPACES.
       01  WS-MSG-AREA                  PIC X(60) VALUE SPACES.

           COPY DFHAID.
           COPY DFHBMSCA.

      * CARRIER MASTER RECORD - THE FROZEN COPYBOOK.  READ, STARTBR,
      * READNEXT AND READPREV ALL TARGET THIS AREA.
           COPY CABSCARR.

      * SYMBOLIC MAP - GENERATED FROM CABM0200 BY THE TYPE=DSECT
      * ASSEMBLY.  NOT PHYSICALLY PRESENT IN SOURCE CONTROL.
           COPY CABM0200.

       LINKAGE SECTION.
           COPY CABSCOMM
               REPLACING ==CABS-COMM-AREA== BY ==DFHCOMMAREA==.

       PROCEDURE DIVISION.

       P0000-MAINLINE.
           IF EIBCALEN = ZERO
               PERFORM P1000-FIRST-ENTRY THRU P1000-EXIT
           ELSE
               PERFORM P2000-PROCESS-INPUT THRU P2000-EXIT
           END-IF

           GOBACK.

      *-----------------------------------------------------------*
      * P1000 - FIRST ENTRY.  CAN ONLY HAPPEN IF THE TRANSACTION   *
      * WAS ENTERED FROM A CLEAR SCREEN RATHER THAN THE MENU -     *
      * BUILD A MINIMAL COMMAREA AND SEND THE MAP EMPTY.            *
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
           PERFORM P1800-SEND-MAP THRU P1800-EXIT.
           EXEC CICS RETURN
                TRANSID('CAB1')
                COMMAREA(DFHCOMMAREA)
                LENGTH(LENGTH OF DFHCOMMAREA)
           END-EXEC.
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
      * P1800 - SEND THE CARRIER MAP, ERASE.  CALLED ON FIRST      *
      * ENTRY WITH THE SCREEN BLANK.                                *
      *-----------------------------------------------------------*
       P1800-SEND-MAP.
           MOVE LOW-VALUES TO CARRMAPO.
           MOVE WS-DISP-DATE TO CDATO.
           MOVE WS-MSG-AREA  TO MSGLO.
           EXEC CICS SEND MAP('CARRMAP')
                MAPSET('CABM02')
                FROM(CARRMAPO)
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
                   PERFORM P1500-BUILD-CLOCK THRU P1500-EXIT
                   MOVE SPACES TO WS-MSG-AREA
                   PERFORM P1800-SEND-MAP THRU P1800-EXIT
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
      * P2200 - RECEIVE THE OCN AND DO A DIRECT READ.  THIS ALSO   *
      * ESTABLISHES THE BROWSE POSITION FOR SUBSEQUENT PF7/PF8.    *
      *-----------------------------------------------------------*
       P2200-RECEIVE-AND-LOOKUP.
           EXEC CICS RECEIVE MAP('CARRMAP')
                MAPSET('CABM02')
                INTO(CARRMAPI)
                RESP(WS-RESP)
           END-EXEC.

           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE 'ENTER AN OCN' TO WS-MSG-AREA
               PERFORM P1500-BUILD-CLOCK THRU P1500-EXIT
               PERFORM P1900-SEND-DATAONLY THRU P1900-EXIT
               PERFORM P8000-RETURN-SAME THRU P8000-EXIT
           END-IF.

           IF OCNNL = ZERO
               MOVE 'OCN REQUIRED' TO WS-MSG-AREA
               PERFORM P1500-BUILD-CLOCK THRU P1500-EXIT
               PERFORM P1900-SEND-DATAONLY THRU P1900-EXIT
               PERFORM P8000-RETURN-SAME THRU P8000-EXIT
           END-IF.

           MOVE OCNNI TO CR-OCN.
           EXEC CICS READ DATASET('CARRMST')
                RIDFLD(CR-OCN)
                INTO(CABS-CARRIER-RECORD)
                RESP(WS-RESP)
           END-EXEC.

           IF WS-RESP = DFHRESP(NORMAL)
               MOVE CR-OCN         TO CM-SAVED-KEY-1
               MOVE CR-OCN         TO CM-BR-LAST-KEY(1:4)
               SET CM-BR-FORWARD   TO TRUE
               MOVE 1              TO CM-BR-PAGE-NBR
               MOVE 'N'            TO CM-BR-EOF-SW
               MOVE 'N'            TO CM-BR-BOF-SW
               PERFORM P1500-BUILD-CLOCK THRU P1500-EXIT
               MOVE SPACES         TO WS-MSG-AREA
               PERFORM P4000-MOVE-RECORD-TO-MAP THRU P4000-EXIT
               PERFORM P1900-SEND-DATAONLY THRU P1900-EXIT
           ELSE
               IF WS-RESP = DFHRESP(NOTFND)
                   MOVE 'OCN NOT ON FILE' TO WS-MSG-AREA
               ELSE
                   MOVE 'CARRIER MASTER UNAVAILABLE - TRY AGAIN LATER'
                        TO WS-MSG-AREA
               END-IF
               PERFORM P1500-BUILD-CLOCK THRU P1500-EXIT
               PERFORM P1900-SEND-DATAONLY THRU P1900-EXIT
           END-IF.

           PERFORM P8000-RETURN-SAME THRU P8000-EXIT.
       P2200-EXIT.
           EXIT.

      *-----------------------------------------------------------*
      * P3500 - BROWSE FORWARD FROM THE LAST KEY IN CM-BROWSE-     *
      * STATE.  A BLANK LAST KEY MEANS NO INQUIRY HAS BEEN DONE    *
      * YET THIS SESSION - THERE IS NOTHING TO PAGE FROM.          *
      *-----------------------------------------------------------*
       P3500-BROWSE-FORWARD.
           IF CM-BR-LAST-KEY(1:4) = SPACES
               MOVE 'ENTER AN OCN BEFORE PAGING' TO WS-MSG-AREA
               PERFORM P1500-BUILD-CLOCK THRU P1500-EXIT
               PERFORM P1900-SEND-DATAONLY THRU P1900-EXIT
               PERFORM P8000-RETURN-SAME THRU P8000-EXIT
           END-IF.

           MOVE CM-BR-LAST-KEY(1:4) TO WS-BROWSE-KEY.
           EXEC CICS STARTBR DATASET('CARRMST')
                RIDFLD(WS-BROWSE-KEY)
                RESP(WS-RESP)
           END-EXEC.
           EXEC CICS READNEXT DATASET('CARRMST')
                RIDFLD(WS-BROWSE-KEY)
                INTO(CABS-CARRIER-RECORD)
                RESP(WS-RESP)
           END-EXEC.
           EXEC CICS READNEXT DATASET('CARRMST')
                RIDFLD(WS-BROWSE-KEY)
                INTO(CABS-CARRIER-RECORD)
                RESP(WS-RESP)
           END-EXEC.
           EXEC CICS ENDBR DATASET('CARRMST') END-EXEC.

           IF WS-RESP = DFHRESP(NORMAL)
               MOVE CR-OCN TO CM-BR-LAST-KEY(1:4)
               MOVE CR-OCN TO CM-SAVED-KEY-1
               ADD 1 TO CM-BR-PAGE-NBR
               SET CM-BR-FORWARD TO TRUE
               MOVE SPACES TO WS-MSG-AREA
               PERFORM P4000-MOVE-RECORD-TO-MAP THRU P4000-EXIT
           ELSE
               SET CM-BR-AT-END TO TRUE
               MOVE 'END OF CARRIER FILE' TO WS-MSG-AREA
           END-IF.
           PERFORM P1500-BUILD-CLOCK THRU P1500-EXIT.
           PERFORM P1900-SEND-DATAONLY THRU P1900-EXIT.
           PERFORM P8000-RETURN-SAME THRU P8000-EXIT.
       P3500-EXIT.
           EXIT.

      *-----------------------------------------------------------*
      * P3600 - BROWSE BACKWARD.  SAME DEPENDENCY ON A PRIOR       *
      * POSITION AS THE FORWARD PATH.                               *
      *-----------------------------------------------------------*
       P3600-BROWSE-BACKWARD.
           IF CM-BR-LAST-KEY(1:4) = SPACES
               MOVE 'ENTER AN OCN BEFORE PAGING' TO WS-MSG-AREA
               PERFORM P1500-BUILD-CLOCK THRU P1500-EXIT
               PERFORM P1900-SEND-DATAONLY THRU P1900-EXIT
               PERFORM P8000-RETURN-SAME THRU P8000-EXIT
           END-IF.

           MOVE CM-BR-LAST-KEY(1:4) TO WS-BROWSE-KEY.
           EXEC CICS STARTBR DATASET('CARRMST')
                RIDFLD(WS-BROWSE-KEY)
                RESP(WS-RESP)
           END-EXEC.
           EXEC CICS READPREV DATASET('CARRMST')
                RIDFLD(WS-BROWSE-KEY)
                INTO(CABS-CARRIER-RECORD)
                RESP(WS-RESP)
           END-EXEC.
           EXEC CICS READPREV DATASET('CARRMST')
                RIDFLD(WS-BROWSE-KEY)
                INTO(CABS-CARRIER-RECORD)
                RESP(WS-RESP)
           END-EXEC.
           EXEC CICS ENDBR DATASET('CARRMST') END-EXEC.

           IF WS-RESP = DFHRESP(NORMAL)
               MOVE CR-OCN TO CM-BR-LAST-KEY(1:4)
               MOVE CR-OCN TO CM-SAVED-KEY-1
               IF CM-BR-PAGE-NBR > 1
                   SUBTRACT 1 FROM CM-BR-PAGE-NBR
               END-IF
               SET CM-BR-BACKWARD TO TRUE
               MOVE SPACES TO WS-MSG-AREA
               PERFORM P4000-MOVE-RECORD-TO-MAP THRU P4000-EXIT
           ELSE
               SET CM-BR-AT-START TO TRUE
               MOVE 'START OF CARRIER FILE' TO WS-MSG-AREA
           END-IF.
           PERFORM P1500-BUILD-CLOCK THRU P1500-EXIT.
           PERFORM P1900-SEND-DATAONLY THRU P1900-EXIT.
           PERFORM P8000-RETURN-SAME THRU P8000-EXIT.
       P3600-EXIT.
           EXIT.

      *-----------------------------------------------------------*
      * P4000 - MOVE THE CURRENT CARRIER RECORD TO THE OUTPUT MAP. *
      *-----------------------------------------------------------*
       P4000-MOVE-RECORD-TO-MAP.
           MOVE LOW-VALUES     TO CARRMAPO.
           MOVE CR-OCN         TO OCNNO.
           MOVE CR-NAME        TO CNAMO.
           MOVE CR-ACNA        TO ACNAO.
           MOVE CR-CIC         TO CICO.
           MOVE CR-TYPE        TO CTYPO.
           MOVE CR-BILL-CYCLE  TO BCYCO.
           MOVE CR-BILL-MEDIA  TO BMEDO.
           MOVE CR-CURRENCY    TO CURRO.
           MOVE CR-TERMS-DAYS  TO TERMO.
           MOVE CR-CREDIT-LIMIT       TO CLIMO.
           MOVE CR-DEFAULT-PIU        TO DPIUO.
           MOVE CR-DEFAULT-PLU        TO DPLUO.
           MOVE CR-FACTOR-SRC         TO FSRCO.
           MOVE CR-RECIP-RATE         TO RRATO.
           MOVE CR-ISP-CAP-MOU        TO ISCPO.
           MOVE CR-CMDS-RAO           TO RAOO.
           MOVE CR-ACTIVE-SW          TO ASWO.
           MOVE CR-EFF-YYDDD          TO EFDTO.
           MOVE CR-EXP-YYDDD          TO EXDTO.
       P4000-EXIT.
           EXIT.

       P1900-SEND-DATAONLY.
           MOVE WS-DISP-DATE TO CDATO.
           MOVE WS-MSG-AREA  TO MSGLO.
           EXEC CICS SEND MAP('CARRMAP')
                MAPSET('CABM02')
                FROM(CARRMAPO)
                DATAONLY
                CURSOR
           END-EXEC.
       P1900-EXIT.
           EXIT.

       P8000-RETURN-SAME.
           EXEC CICS RETURN
                TRANSID('CAB1')
                COMMAREA(DFHCOMMAREA)
                LENGTH(LENGTH OF DFHCOMMAREA)
           END-EXEC.
       P8000-EXIT.
           EXIT.

      *-----------------------------------------------------------*
      * P8500 - PF3/PF12 RETURNS TO THE MENU.  THE NAV STACK IS    *
      * NOT POPPED HERE - CABONL01 REBUILDS ITS OWN ENTRY ON THE   *
      * NEXT DISPATCH RATHER THAN TRUST A POPPED VALUE.             *
      *-----------------------------------------------------------*
       P8500-RETURN-TO-MENU.
           MOVE SPACES TO CM-MSG-TEXT.
           EXEC CICS XCTL
                PROGRAM('CABONL01')
                COMMAREA(DFHCOMMAREA)
                LENGTH(LENGTH OF DFHCOMMAREA)
           END-EXEC.
       P8500-EXIT.
           EXIT.
