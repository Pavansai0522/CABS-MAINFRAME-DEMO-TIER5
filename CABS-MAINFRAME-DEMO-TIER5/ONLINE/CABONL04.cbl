       IDENTIFICATION DIVISION.
      *****************************************************************
      * CABONL04 - CAB3 FACTOR (PIU/PLU) MAINTENANCE                   *
      * APPLICATION : CABS (ONLINE)                                    *
      * COMPILER    : ENTERPRISE COBOL                                 *
      * TRANSACTION : CAB3                                             *
      * MAPSET      : CABM04 / FCTRMAP                     CABM0400    *
      * FILE        : RATEFCTR  TELCABS.CABS.FACTOR  VSAM KSDS         *
      *               RECORD LAYOUT                       CABSFCTR    *
      * XCTL TO     : CABONL01 (PF3/PF12 RETURN TO MENU)               *
      * RUNNABLE    : REFERENCE-ONLY.  MVS 3.8J / TK4- CARRIES NO      *
      *               CICS REGION.  SEE ONLINE/_MANIFEST.MD.           *
      * REVISION HISTORY                                                *
      *   V1.00  1993-08-05  T.ANSELMO    INITIAL                      *
      *   V1.01  1996-03-19  T.ANSELMO    PSU FIELD ADDED               *
      *   V2.00  2015-11-02  K.VANCE      REWRITTEN TO RESP-STYLE       *
      *                                   CONDITION HANDLING PER        *
      *                                   CABS-STD-014 REV 1            *
      *   V2.01  2018-02-27  K.VANCE      SETS THE MAINTENANCE FLAG     *
      *                                   BYTE ON A SUCCESSFUL UPDATE   *
      *                                   PER CABS-STD-014 SECTION 4    *
      *****************************************************************
       PROGRAM-ID.    CABONL04.
       AUTHOR.        T.ANSELMO.
       DATE-WRITTEN.  1993-08-05.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-PROGRAM-IDENT.
           05  WS-PGM-NAME              PIC X(08) VALUE 'CABONL04'.
           05  WS-PGM-VERSION           PIC X(05) VALUE 'V2.01'.

       01  WS-DATE-TIME-AREA.
           05  WS-ABSTIME               PIC S9(15) COMP-3.
           05  WS-DISP-DATE             PIC X(08).

       01  WS-RESP-AREA.
           05  WS-RESP                  PIC S9(08) COMP.
           05  WS-RESP2                 PIC S9(08) COMP.

       01  WS-MSG-AREA                  PIC X(60) VALUE SPACES.
       01  WS-EDIT-OK-SW                PIC X(01) VALUE 'N'.
           88  WS-EDIT-OK                   VALUE 'Y'.
       01  WS-FOUND-SW                  PIC X(01) VALUE 'N'.
           88  WS-FOUND                     VALUE 'Y'.

           COPY DFHAID.
           COPY DFHBMSCA.
           COPY CABSFCTR.

      * SYMBOLIC MAP - GENERATED FROM CABM0400 BY THE TYPE=DSECT
      * ASSEMBLY.  NOT PHYSICALLY PRESENT IN SOURCE CONTROL.
           COPY CABM0400.

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
      * P1800 - SEND THE ENTRY SCREEN WITH THE CONFIRM FIELD        *
      * PROTECTED.  THIS IS THE STATE THE MAP IS IN UNTIL A         *
      * SUCCESSFUL LOOKUP OR PF5 STAGES AN UPDATE.                  *
      *-----------------------------------------------------------*
       P1800-SEND-ENTRY-SCREEN.
           MOVE LOW-VALUES     TO FCTRMAPO.
           MOVE WS-DISP-DATE   TO CDATO.
           MOVE WS-MSG-AREA    TO MSGLO.
           MOVE DFHBMPRO       TO CONFA.
           EXEC CICS SEND MAP('FCTRMAP')
                MAPSET('CABM04')
                FROM(FCTRMAPO)
                ERASE
                CURSOR
           END-EXEC.
       P1800-EXIT.
           EXIT.

      *-----------------------------------------------------------*
      * P2000 - AID KEY DISPATCH.  IF A CONFIRM IS PENDING, ENTER   *
      * IS ROUTED TO THE CONFIRM PROCESSOR RATHER THAN A FRESH      *
      * EDIT - THIS IS THE ONLY PLACE THE COMMAREA CONFIRM SWITCH   *
      * IS TESTED.                                                  *
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
                   PERFORM P5000-PROCESS-CONFIRM THRU P5000-EXIT
               WHEN EIBAID = DFHPF5
                   PERFORM P3000-RECEIVE-AND-EDIT THRU P3000-EXIT
                   IF WS-EDIT-OK
                       PERFORM P4200-STAGE-INSERT THRU P4200-EXIT
                   END-IF
               WHEN OTHER
                   PERFORM P3000-RECEIVE-AND-EDIT THRU P3000-EXIT
                   IF WS-EDIT-OK
                       PERFORM P4100-STAGE-UPDATE THRU P4100-EXIT
                   END-IF
           END-EVALUATE.
       P2000-EXIT.
           EXIT.

      *-----------------------------------------------------------*
      * P3000 - RECEIVE AND EDIT THE KEY AND FACTOR FIELDS.          *
      * RANGE EDIT: PIU/PLU/PSU ARE FRACTIONS                        *
      * OF TOTAL USAGE AND MUST FALL BETWEEN 0 AND 1 INCLUSIVE -     *
      * A FACTOR OUTSIDE THAT RANGE WOULD OVER OR UNDER STATE THE    *
      * JURISDICTIONAL SPLIT.  SEE CABS-STD-011.                     *
      *-----------------------------------------------------------*
       P3000-RECEIVE-AND-EDIT.
           EXEC CICS RECEIVE MAP('FCTRMAP')
                MAPSET('CABM04')
                INTO(FCTRMAPI)
                RESP(WS-RESP)
           END-EXEC.

           MOVE 'Y' TO WS-EDIT-OK-SW.
           MOVE SPACES TO WS-MSG-AREA.

           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE 'ENTER OCN, STATE, LATA AND EFFECTIVE DATE'
                    TO WS-MSG-AREA
               MOVE 'N' TO WS-EDIT-OK-SW
           END-IF.

           IF WS-EDIT-OK AND (OCNNL = ZERO OR STCDL = ZERO OR
                   LATAL = ZERO OR EFFDL = ZERO)
               MOVE 'OCN, STATE, LATA AND EFF DATE ARE ALL REQUIRED'
                    TO WS-MSG-AREA
               MOVE 'N' TO WS-EDIT-OK-SW
           END-IF.

           IF WS-EDIT-OK AND
              (PIUI < 0 OR PIUI > 1 OR PLUI < 0 OR PLUI > 1
                    OR PSUI < 0 OR PSUI > 1)
               MOVE 'PIU/PLU/PSU MUST BE BETWEEN 0 AND 1' TO WS-MSG-AREA
               MOVE 'N' TO WS-EDIT-OK-SW
           END-IF.

           IF NOT WS-EDIT-OK
               PERFORM P1500-BUILD-CLOCK THRU P1500-EXIT
               MOVE WS-MSG-AREA TO MSGLO
               EXEC CICS SEND MAP('FCTRMAP')
                    MAPSET('CABM04')
                    FROM(FCTRMAPO)
                    DATAONLY
                    ALARM
                    CURSOR
               END-EXEC
               PERFORM P8000-RETURN-SAME THRU P8000-EXIT
           END-IF.
       P3000-EXIT.
           EXIT.

      *-----------------------------------------------------------*
      * P4100 - PLAIN ENTER.  THE QUARTER MUST ALREADY EXIST -      *
      * READ UPDATE HOLDS THE RECORD LOCKED ACROSS THE CONFIRM      *
      * SCREEN UNTIL P5000 EITHER REWRITES OR RELEASES IT.          *
      *-----------------------------------------------------------*
       P4100-STAGE-UPDATE.
           MOVE OCNNI TO FC-OCN.
           MOVE STCDI TO FC-STATE-CD.
           MOVE LATAI TO FC-LATA.
           MOVE EFFDI TO FC-EFF-YYDDD.

           EXEC CICS READ DATASET('RATEFCTR')
                RIDFLD(FC-KEY)
                INTO(CABS-FACTOR-RECORD)
                UPDATE
                RESP(WS-RESP)
           END-EXEC.

           IF WS-RESP = DFHRESP(NORMAL)
               MOVE 'Y' TO CM-CONFIRM-SW
               MOVE 'Y' TO CM-UPDATE-PENDING-SW
               MOVE ZERO TO CM-EDIT-SEQ
               MOVE FC-RESTATE-SW TO RSTSO
               MOVE 'CONFIRM UPDATE OF THIS QUARTER (Y/N)?'
                    TO WS-MSG-AREA
               PERFORM P1500-BUILD-CLOCK THRU P1500-EXIT
               MOVE WS-MSG-AREA TO MSGLO
               MOVE DFHBMFSE TO CONFA
               EXEC CICS SEND MAP('FCTRMAP')
                    MAPSET('CABM04')
                    FROM(FCTRMAPO)
                    DATAONLY
                    CURSOR
               END-EXEC
           ELSE
               MOVE 'N' TO CM-CONFIRM-SW
               MOVE 'FACTOR NOT ON FILE - PF5 ADDS A NEW QUARTER'
                    TO WS-MSG-AREA
               PERFORM P1500-BUILD-CLOCK THRU P1500-EXIT
               MOVE WS-MSG-AREA TO MSGLO
               EXEC CICS SEND MAP('FCTRMAP')
                    MAPSET('CABM04')
                    FROM(FCTRMAPO)
                    DATAONLY
                    ALARM
                    CURSOR
               END-EXEC
           END-IF.
           PERFORM P8000-RETURN-SAME THRU P8000-EXIT.
       P4100-EXIT.
           EXIT.

      *-----------------------------------------------------------*
      * P4200 - PF5.  THE QUARTER MUST NOT ALREADY EXIST.           *
      *-----------------------------------------------------------*
       P4200-STAGE-INSERT.
           MOVE OCNNI TO FC-OCN.
           MOVE STCDI TO FC-STATE-CD.
           MOVE LATAI TO FC-LATA.
           MOVE EFFDI TO FC-EFF-YYDDD.

           EXEC CICS READ DATASET('RATEFCTR')
                RIDFLD(FC-KEY)
                INTO(CABS-FACTOR-RECORD)
                RESP(WS-RESP)
           END-EXEC.

           IF WS-RESP = DFHRESP(NORMAL)
               MOVE 'N' TO CM-CONFIRM-SW
               MOVE 'QUARTER ALREADY ON FILE - PRESS ENTER TO UPDATE'
                    TO WS-MSG-AREA
               PERFORM P1500-BUILD-CLOCK THRU P1500-EXIT
               MOVE WS-MSG-AREA TO MSGLO
               EXEC CICS SEND MAP('FCTRMAP')
                    MAPSET('CABM04')
                    FROM(FCTRMAPO)
                    DATAONLY
                    ALARM
                    CURSOR
               END-EXEC
           ELSE
               MOVE 'Y' TO CM-CONFIRM-SW
               MOVE 'N' TO CM-UPDATE-PENDING-SW
               MOVE 'CONFIRM NEW QUARTER (Y/N)?' TO WS-MSG-AREA
               PERFORM P1500-BUILD-CLOCK THRU P1500-EXIT
               MOVE WS-MSG-AREA TO MSGLO
               MOVE DFHBMFSE TO CONFA
               EXEC CICS SEND MAP('FCTRMAP')
                    MAPSET('CABM04')
                    FROM(FCTRMAPO)
                    DATAONLY
                    CURSOR
               END-EXEC
           END-IF.
           PERFORM P8000-RETURN-SAME THRU P8000-EXIT.
       P4200-EXIT.
           EXIT.

      *-----------------------------------------------------------*
      * P5000 - CONFIRM RESPONSE.  THE 3270 BUFFER STILL HOLDS THE  *
      * KEY AND FACTOR FIELDS ENTERED ON THE PRIOR TURN SINCE ONLY  *
      * DATAONLY WAS SENT - RECEIVE THEM AGAIN RATHER THAN CARRY    *
      * THEM IN THE COMMAREA.  CM-UPDATE-PENDING-SW SAYS WHETHER    *
      * TO REWRITE (SET BY P4100) OR WRITE (LEFT 'N' BY P4200).     *
      *-----------------------------------------------------------*
       P5000-PROCESS-CONFIRM.
           EXEC CICS RECEIVE MAP('FCTRMAP')
                MAPSET('CABM04')
                INTO(FCTRMAPI)
                RESP(WS-RESP)
           END-EXEC.

           MOVE 'N' TO CM-CONFIRM-SW.

           IF CONFI = 'Y'
               MOVE OCNNI TO FC-OCN
               MOVE STCDI TO FC-STATE-CD
               MOVE LATAI TO FC-LATA
               MOVE EFFDI TO FC-EFF-YYDDD
               MOVE PIUI  TO FC-PIU
               MOVE PLUI  TO FC-PLU
               MOVE PSUI  TO FC-PSU
               MOVE SRCI  TO FC-SOURCE

               IF CM-UPDATE-PENDING
                   EXEC CICS REWRITE DATASET('RATEFCTR')
                        FROM(CABS-FACTOR-RECORD)
                        RESP(WS-RESP)
                   END-EXEC
               ELSE
                   EXEC CICS WRITE DATASET('RATEFCTR')
                        RIDFLD(FC-KEY)
                        FROM(CABS-FACTOR-RECORD)
                        RESP(WS-RESP)
                   END-EXEC
               END-IF

               IF WS-RESP = DFHRESP(NORMAL)
      * SET BY THE MAINTENANCE TRANSACTIONS - SEE CABS-STD-014
      * SECTION 4 FOR THE CURRENT BYTE ASSIGNMENTS.
                   MOVE 'Y' TO CM-FLAG-5
                   MOVE 'FACTOR UPDATE COMPLETE' TO WS-MSG-AREA
               ELSE
                   MOVE 'UPDATE FAILED - FILE UNAVAILABLE'
                       TO WS-MSG-AREA
               END-IF
           ELSE
               MOVE 'UPDATE CANCELLED' TO WS-MSG-AREA
           END-IF.

           MOVE 'N' TO CM-UPDATE-PENDING-SW.
           PERFORM P1500-BUILD-CLOCK THRU P1500-EXIT.
           PERFORM P1800-SEND-ENTRY-SCREEN THRU P1800-EXIT.
           PERFORM P8000-RETURN-SAME THRU P8000-EXIT.
       P5000-EXIT.
           EXIT.

       P8000-RETURN-SAME.
           EXEC CICS RETURN
                TRANSID('CAB3')
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
