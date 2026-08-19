      *****************************************************************
      * CABCIRCL - CIRCUIT INVENTORY LOOKUP                           *
      * APPLICATION : CABS                                            *
      * INVOKED BY  : CALL FROM THE RATING PROGRAMS CABRAT04 AND      *
      *               CABRAT05                                        *
      * INPUTS      : LK-CL-CIRCUIT-ID  TWENTY BYTE CIRCUIT ID        *
      *               DDNAME  DSN                          COPYBOOK   *
      *               CIRCMST TELCABS.CABS.CIRCMST        CABSCIRC    *
      * OUTPUTS     : CABS-CIRCUIT-RECORD  THE INVENTORY ROW          *
      *               LK-CL-RC             CONDITION OF THE LOOKUP    *
      * CONTROL     : NONE - SUBPROGRAMS DO NOT WRITE CTLOUT,         *
      *               CABS-STD-041                                    *
      * BALANCE     : NONE - THE CALLING PROGRAM RECORD COUNTS ARE    *
      *               NOT TOUCHED BY THIS MODULE                      *
      * RESTART     : NONE - THE MASTER IS READ ONLY                  *
      * REVISION HISTORY                                              *
      *   V1.00  1991-01-28  R.T.WHEELER   INITIAL RELEASE AS AN      *
      *                      ISAM LOOKUP                              *
      *   V2.00  1996-05-13  T.YAMASHITA   CONVERTED TO VSAM KSDS     *
      *   V2.02  1998-10-07  M.HAAS        SERVICE TYPE DERIVED FROM  *
      *                      THE USOC PREFIX WHEN IT IS BLANK         *
      *   V2.04  2003-03-19  L.FERREIRA    FORTY ENTRY MOST RECENTLY  *
      *                      USED CACHE ADDED AHEAD OF THE READ       *
      *   V2.06  2008-09-30  B.R.HALVORSEN MASTER CLEANSE COMPLETED,  *
      *                      THE DERIVATIONS CAN COME OUT             *
      *   V2.08  2014-11-11  S.MBEKI       HARD STATUS REPORTED WITH  *
      *                      THE STATUS CODE ON SYSOUT                *
      *   V2.09  2019-03-05  G.PETRAKIS    CACHE COUNTERS ADDED TO    *
      *                      THE END OF JOB DISPLAY                   *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABCIRCL.
       AUTHOR. TELCABS APPLICATIONS - CIRCUIT INVENTORY.
      *****************************************************************
      * THE MODULE OWNS THE CIRCMST VSAM ACCESS FOR THE RATING PASS.  *
      * IT READS ONLY.  IT WRITES NO FILE AND KEEPS NO COUNTS FOR     *
      * THE CALLER, SO THE CALLING PROGRAM BALANCE EQUATION IS        *
      * UNAFFECTED BY THIS MODULE.                                    *
      *                                                               *
      * THE CLUSTER IS OPENED INPUT ON THE FIRST CALL AND IS LEFT     *
      * OPEN FOR THE REST OF THE RUN UNIT.  ONE OPEN INSTEAD OF ONE   *
      * PER CIRCUIT IS WHAT MAKES THE RATING PASS AFFORDABLE.  THE    *
      * TERMINATION OF THE RUN UNIT CLOSES IT.                        *
      *                                                               *
      * RETURN CODES SET IN LK-CL-RC                                  *
      *   0000  FOUND - READ FROM THE MASTER                          *
      *   0004  FOUND - SATISFIED FROM THE CACHE                      *
      *   0008  NOT FOUND - THE RECORD IS CLEARED                     *
      *   0012  VSAM HARD STATUS - THE STATUS CODE IS DISPLAYED       *
      *   0016  CIRCUIT ID WAS SPACES - NO READ WAS ATTEMPTED         *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CIRCMST ASSIGN TO CIRCMST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS FD-CI-KEY
               FILE STATUS IS WS-FS-CIRCMST.
       DATA DIVISION.
       FILE SECTION.
      * CIRCMST - VSAM KSDS.  THE FD RECORD CARRIES THE SAME LAYOUT
      * AS CABSCIRC UNDER ITS OWN NAMES SO THE KEY IS UNAMBIGUOUS.
       FD  CIRCMST
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 136 CHARACTERS.
       01  CIRCMST-RECORD.
           05  FD-CI-KEY.
               10  FD-CI-CIRCUIT-ID        PIC X(20).
           05  FD-CI-IDENT.
               10  FD-CI-TRUNK-GRP         PIC X(08).
               10  FD-CI-OCN               PIC X(04).
               10  FD-CI-BAN               PIC X(13).
               10  FD-CI-USOC              PIC X(05).
               10  FD-CI-SERVICE-TYPE      PIC X(02).
           05  FD-CI-LOCATION.
               10  FD-CI-A-CLLI            PIC X(11).
               10  FD-CI-Z-CLLI            PIC X(11).
               10  FD-CI-A-LATA            PIC 9(03).
               10  FD-CI-Z-LATA            PIC 9(03).
               10  FD-CI-STATE-CD          PIC X(02).
           05  FD-CI-MPB.
               10  FD-CI-MPB-SW            PIC X(01).
               10  FD-CI-MPB-OUR-PCT       PIC S9(03)V9(05) COMP-3.
               10  FD-CI-MPB-OTHER-OCN     PIC X(04).
               10  FD-CI-MPB-OTHER-PCT     PIC S9(03)V9(05) COMP-3.
           05  FD-CI-TERM.
               10  FD-CI-INSTALL-YYDDD     PIC 9(05).
               10  FD-CI-TERM-MONTHS       PIC 9(03).
               10  FD-CI-DISC-YYDDD        PIC 9(05).
               10  FD-CI-STATUS            PIC X(01).
           05  FD-CI-FILLER                PIC X(25).
       WORKING-STORAGE SECTION.
      * WORKING STORAGE IN A CALLED SUBPROGRAM SURVIVES FROM ONE CALL
      * TO THE NEXT INSIDE ONE RUN UNIT.  THE OPEN SWITCH, THE CACHE
      * AND THE COUNTERS ALL DEPEND ON THAT.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABCIRCL'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V2.09'.
           05  WS-CACHE-MAX                PIC S9(04) COMP-3 VALUE 40.
       01  WS-FS-CIRCMST                   PIC X(02) VALUE '  '.
       01  WS-SWITCH-AREA.
           05  WS-FIRST-SW                 PIC X(01) VALUE 'Y'.
               88  WS-FIRST-CALL               VALUE 'Y'.
           05  WS-OPEN-FAILED-SW           PIC X(01) VALUE 'N'.
               88  WS-OPEN-FAILED              VALUE 'Y'.
           05  WS-CACHE-HIT-SW             PIC X(01) VALUE 'N'.
               88  WS-CACHE-HIT                VALUE 'Y'.
           05  WS-FOUND-SW                 PIC X(01) VALUE 'N'.
               88  WS-CIRCUIT-FOUND            VALUE 'Y'.
       01  WS-COUNT-AREA.
           05  WS-CALL-CNT                 PIC S9(09) COMP-3 VALUE 0.
           05  WS-CACHE-HIT-CNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-READ-CNT                 PIC S9(09) COMP-3 VALUE 0.
           05  WS-NOTFOUND-CNT             PIC S9(09) COMP-3 VALUE 0.
           05  WS-HARD-STATUS-CNT          PIC S9(09) COMP-3 VALUE 0.
           05  WS-BLANK-KEY-CNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-SERVICE-FIX-CNT          PIC S9(09) COMP-3 VALUE 0.
           05  WS-MPB-FIX-CNT              PIC S9(09) COMP-3 VALUE 0.
       01  WS-SUBSCRIPT-AREA.
           05  WS-SUB-01                   PIC S9(04) COMP-3 VALUE 0.
           05  WS-CACHE-SUB                PIC S9(04) COMP-3 VALUE 0.
           05  WS-CACHE-PTR                PIC S9(04) COMP-3 VALUE 1.
       01  WS-EDIT-AREA.
           05  WS-CNT-EDIT                 PIC ZZZ,ZZZ,ZZ9.
      * FORTY ENTRY CACHE ADDED IN 2003.  IT IS WALKED SERIALLY
      * BECAUSE THE CIRCUITS ARRIVE IN THE ORDER THE SORT LEFT THEM
      * AND NOT IN KEY ORDER, SO THERE IS NO ORDERING TO SEARCH ON.
       01  WS-CACHE-TABLE.
           05  WS-CH-ENTRY OCCURS 40 TIMES.
               10  WS-CH-KEY               PIC X(20).
               10  WS-CH-IMAGE             PIC X(136).
      * USOC PREFIX WORK FOR THE SERVICE TYPE DERIVATION.
       01  WS-USOC-WORK                    PIC X(05) VALUE SPACES.
       01  WS-USOC-PARTS REDEFINES WS-USOC-WORK.
           05  WS-US-PFX                   PIC X(01).
           05  WS-US-REST                  PIC X(04).
       LINKAGE SECTION.
       01  LK-CL-CIRCUIT-ID                PIC X(20).
       COPY CABSCIRC.
       01  LK-CL-RC                        PIC 9(04).
       PROCEDURE DIVISION USING LK-CL-CIRCUIT-ID CABS-CIRCUIT-RECORD
           LK-CL-RC.
      * P0000-ENTRY - ONE PASS PER CALL.
       P0000-ENTRY.
           MOVE 0 TO LK-CL-RC.
           ADD 1 TO WS-CALL-CNT.
           IF WS-FIRST-CALL
               PERFORM P1000-FIRST-CALL THRU P1000-EXIT.
           IF LK-CL-CIRCUIT-ID = '*END'
               PERFORM P8000-SUMMARY THRU P8000-EXIT
               GO TO P0000-RETURN.
           IF LK-CL-CIRCUIT-ID = SPACES
               ADD 1 TO WS-BLANK-KEY-CNT
               PERFORM P6000-CLEAR-RECORD THRU P6000-EXIT
               MOVE 16 TO LK-CL-RC
               GO TO P0000-RETURN.
           PERFORM P2000-LOOK-IN-CACHE THRU P2000-EXIT.
           IF WS-CACHE-HIT
               ADD 1 TO WS-CACHE-HIT-CNT
               MOVE 4 TO LK-CL-RC
               GO TO P0000-RETURN.
           PERFORM P3000-READ-MASTER THRU P3000-EXIT.
       P0000-RETURN.
           GOBACK.
      * S100-INITIALISATION SECTION
       S100-INITIALISATION SECTION.
       P1000-FIRST-CALL.
           MOVE 'N' TO WS-FIRST-SW.
           PERFORM P1100-CLEAR-CACHE THRU P1100-EXIT.
           PERFORM P1200-OPEN-MASTER THRU P1200-EXIT.
       P1000-EXIT.
           EXIT.
       P1100-CLEAR-CACHE.
           MOVE 1 TO WS-CACHE-PTR.
           PERFORM P1110-CLEAR-SLOT THRU P1110-EXIT
               VARYING WS-SUB-01 FROM 1 BY 1
               UNTIL WS-SUB-01 > WS-CACHE-MAX.
       P1100-EXIT.
           EXIT.
       P1110-CLEAR-SLOT.
           MOVE SPACES TO WS-CH-KEY (WS-SUB-01).
           MOVE SPACES TO WS-CH-IMAGE (WS-SUB-01).
       P1110-EXIT.
           EXIT.
       P1200-OPEN-MASTER.
           OPEN INPUT CIRCMST.
           IF WS-FS-CIRCMST NOT = '00'
               MOVE 'Y' TO WS-OPEN-FAILED-SW
               DISPLAY 'CABCIRCL - CIRCMST OPEN STATUS '
                   WS-FS-CIRCMST.
       P1200-EXIT.
           EXIT.
      * S200-CACHE SECTION
       S200-CACHE SECTION.
       P2000-LOOK-IN-CACHE.
           MOVE 'N' TO WS-CACHE-HIT-SW.
           MOVE 1 TO WS-CACHE-SUB.
           PERFORM P2100-TEST-SLOT THRU P2100-EXIT
               UNTIL WS-CACHE-SUB > WS-CACHE-MAX OR WS-CACHE-HIT.
       P2000-EXIT.
           EXIT.
       P2100-TEST-SLOT.
           IF WS-CH-KEY (WS-CACHE-SUB) = LK-CL-CIRCUIT-ID
               MOVE 'Y' TO WS-CACHE-HIT-SW
               MOVE WS-CH-IMAGE (WS-CACHE-SUB) TO
                   CABS-CIRCUIT-RECORD
           ELSE
               ADD 1 TO WS-CACHE-SUB.
       P2100-EXIT.
           EXIT.
      * P2200-ADD-TO-CACHE - THE OLDEST SLOT IS REPLACED THROUGH A
      * WRAP ROUND SLOT INDEX.  THE ROW IS STORED AFTER THE DERIVED
      * FIELDS HAVE BEEN SET SO A LATER HIT RETURNS THE SAME IMAGE.
       P2200-ADD-TO-CACHE.
           IF WS-CACHE-PTR < 1 OR WS-CACHE-PTR > WS-CACHE-MAX
               MOVE 1 TO WS-CACHE-PTR.
           MOVE LK-CL-CIRCUIT-ID TO WS-CH-KEY (WS-CACHE-PTR).
           MOVE CABS-CIRCUIT-RECORD TO WS-CH-IMAGE (WS-CACHE-PTR).
           ADD 1 TO WS-CACHE-PTR.
           IF WS-CACHE-PTR > WS-CACHE-MAX
               MOVE 1 TO WS-CACHE-PTR.
       P2200-EXIT.
           EXIT.
      * S300-MASTER-ACCESS SECTION
       S300-MASTER-ACCESS SECTION.
       P3000-READ-MASTER.
           MOVE 'N' TO WS-FOUND-SW.
           IF WS-OPEN-FAILED
               ADD 1 TO WS-HARD-STATUS-CNT
               PERFORM P6000-CLEAR-RECORD THRU P6000-EXIT
               MOVE 12 TO LK-CL-RC
               GO TO P3000-EXIT.
           MOVE LK-CL-CIRCUIT-ID TO FD-CI-CIRCUIT-ID.
           ADD 1 TO WS-READ-CNT.
           READ CIRCMST.
           IF WS-FS-CIRCMST = '00'
               MOVE 'Y' TO WS-FOUND-SW
               PERFORM P3100-ACCEPT-ROW THRU P3100-EXIT
           ELSE
               PERFORM P3200-BAD-STATUS THRU P3200-EXIT.
       P3000-EXIT.
           EXIT.
       P3100-ACCEPT-ROW.
           MOVE CIRCMST-RECORD TO CABS-CIRCUIT-RECORD.
           PERFORM P4000-DERIVE-FIELDS THRU P4000-EXIT.
           PERFORM P2200-ADD-TO-CACHE THRU P2200-EXIT.
           MOVE 0 TO LK-CL-RC.
       P3100-EXIT.
           EXIT.
       P3200-BAD-STATUS.
           IF WS-FS-CIRCMST = '23'
               ADD 1 TO WS-NOTFOUND-CNT
               PERFORM P6000-CLEAR-RECORD THRU P6000-EXIT
               MOVE 8 TO LK-CL-RC
           ELSE
               ADD 1 TO WS-HARD-STATUS-CNT
               DISPLAY 'CABCIRCL - CIRCMST READ STATUS '
                   WS-FS-CIRCMST ' KEY ' LK-CL-CIRCUIT-ID
               PERFORM P6000-CLEAR-RECORD THRU P6000-EXIT
               MOVE 12 TO LK-CL-RC.
       P3200-EXIT.
           EXIT.
      * S400-DERIVATION SECTION - TWO FIELDS THE MASTER DOES NOT
      * CARRY AND THE RATING MODULES READ.  BOTH WERE PUT IN BEFORE
      * THE MASTER WAS CLEANSED IN 2008 AND BOTH STILL RUN.
       S400-DERIVATION SECTION.
       P4000-DERIVE-FIELDS.
           PERFORM P4100-DERIVE-SERVICE THRU P4100-EXIT.
           PERFORM P4200-CLEAR-MEET-POINT THRU P4200-EXIT.
       P4000-EXIT.
           EXIT.
      * P4100-DERIVE-SERVICE - THE 88 LEVELS ON CI-SERVICE-TYPE ARE
      * CI-SWITCHED, CI-SPECIAL, CI-UNE AND CI-INTERCONNECT.  THE
      * USOC PREFIX SELECTS THE VALUE WHEN THE STORED TYPE IS BLANK.
       P4100-DERIVE-SERVICE.
           IF CI-SERVICE-TYPE NOT = SPACES
               GO TO P4100-EXIT.
           ADD 1 TO WS-SERVICE-FIX-CNT.
           MOVE CI-USOC TO WS-USOC-WORK.
           IF WS-US-PFX = 'T'
               MOVE 'SP' TO CI-SERVICE-TYPE
           ELSE
               IF WS-US-PFX = 'U'
                   MOVE 'UN' TO CI-SERVICE-TYPE
               ELSE
                   IF WS-US-PFX = 'N'
                       MOVE 'IC' TO CI-SERVICE-TYPE
                   ELSE
                       MOVE 'SW' TO CI-SERVICE-TYPE.
       P4100-EXIT.
           EXIT.
      * P4200-CLEAR-MEET-POINT - A ROW THAT IS NOT FLAGGED FOR MEET
      * POINT BILLING RETURNS ZERO PERCENTAGES AND A BLANK PARTNER
      * OCN, WHICH IS WHAT THE RATING MODULES READ.
       P4200-CLEAR-MEET-POINT.
           IF CI-MPB-SW = 'Y'
               GO TO P4200-EXIT.
           ADD 1 TO WS-MPB-FIX-CNT.
           MOVE 0 TO CI-MPB-OUR-PCT.
           MOVE 0 TO CI-MPB-OTHER-PCT.
           MOVE SPACES TO CI-MPB-OTHER-OCN.
       P4200-EXIT.
           EXIT.
      * S600-CLEAR SECTION
       S600-CLEAR SECTION.
       P6000-CLEAR-RECORD.
           MOVE SPACES TO CABS-CIRCUIT-RECORD.
           MOVE LOW-VALUES TO CI-MPB.
           MOVE ZERO TO CI-A-LATA.
           MOVE ZERO TO CI-Z-LATA.
           MOVE ZERO TO CI-INSTALL-YYDDD.
           MOVE ZERO TO CI-TERM-MONTHS.
           MOVE ZERO TO CI-DISC-YYDDD.
       P6000-EXIT.
           EXIT.
      * S800-SUMMARY SECTION - DRIVEN BY A CIRCUIT ID OF *END
      * FOLLOWED BY SPACES.
       S800-SUMMARY SECTION.
       P8000-SUMMARY.
           DISPLAY 'CABCIRCL ' WS-PGM-VERSION
               ' - CIRCUIT LOOKUP SUMMARY'.
           MOVE WS-CALL-CNT TO WS-CNT-EDIT.
           DISPLAY '  CALLS RECEIVED   = ' WS-CNT-EDIT.
           MOVE WS-CACHE-HIT-CNT TO WS-CNT-EDIT.
           DISPLAY '  CACHE HITS       = ' WS-CNT-EDIT.
           MOVE WS-READ-CNT TO WS-CNT-EDIT.
           DISPLAY '  PHYSICAL READS   = ' WS-CNT-EDIT.
           MOVE WS-NOTFOUND-CNT TO WS-CNT-EDIT.
           DISPLAY '  NOT FOUND        = ' WS-CNT-EDIT.
           MOVE WS-HARD-STATUS-CNT TO WS-CNT-EDIT.
           DISPLAY '  HARD STATUSES    = ' WS-CNT-EDIT.
           MOVE WS-BLANK-KEY-CNT TO WS-CNT-EDIT.
           DISPLAY '  BLANK CIRCUIT ID = ' WS-CNT-EDIT.
           MOVE WS-SERVICE-FIX-CNT TO WS-CNT-EDIT.
           DISPLAY '  SERVICE DERIVED  = ' WS-CNT-EDIT.
           MOVE WS-MPB-FIX-CNT TO WS-CNT-EDIT.
           DISPLAY '  MEET POINT BLANK = ' WS-CNT-EDIT.
           MOVE 0 TO LK-CL-RC.
       P8000-EXIT.
           EXIT.
