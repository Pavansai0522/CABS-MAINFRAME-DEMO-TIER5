      *****************************************************************
      * CABERRWR - SUSPENSE AND ERROR LOG WRITER                      *
      * APPLICATION : CABS                                            *
      * INVOKED BY  : CALL FROM BATCH PROGRAMS IN EVERY FAMILY        *
      * INPUTS      : LK-EW-AREA  255 BYTE ERROR STAGING AREA         *
      *               LK-EW-RC    RETURN CODE SLOT                    *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               ERRLOG  TELCABS.CABS.ERRLOG         (LOCAL)     *
      * CONTROL     : NONE - SUBPROGRAMS DO NOT WRITE CTLOUT,         *
      *               CABS-STD-041                                    *
      * BALANCE     : NONE - THE CALLING PROGRAM RECORD COUNTS AND    *
      *               ITS OWN SUSPENSE FILE ARE NOT TOUCHED HERE      *
      * RESTART     : NONE - ERRLOG IS OPENED OUTPUT ON THE FIRST     *
      *               CALL OF EVERY RUN                               *
      * REVISION HISTORY                                              *
      *   V1.00  1988-03-14  R.T.WHEELER   INITIAL RELEASE            *
      *   V1.04  1991-07-22  D.OKONKWO     SEVERITY COUNTS ADDED TO   *
      *                      THE END OF JOB DISPLAY                   *
      *   V2.00  1994-02-28  A.BUKOWSKI    LAYOUT PROBE ADDED WHEN    *
      *                      THE TWO STAGING CONVENTIONS WERE MERGED  *
      *   V2.03  1996-11-05  L.FERREIRA    RUN ID TAKEN FROM THE      *
      *                      CALLER INSTEAD OF THE PARM CARD          *
      *   V2.07  1999-04-19  M.HAAS        SEVERITY DERIVED FROM THE  *
      *                      ERROR CODE WHEN THE CALLER LEAVES IT     *
      *   V2.09  2004-08-02  P.NAIR        UNCLASSIFIED CONDITIONS    *
      *                      WRITTEN INSTEAD OF DISCARDED             *
      *   V2.12  2011-05-16  T.YAMASHITA   BLOCK SIZE SET TO ZERO -   *
      *                      SYSTEM DETERMINED                        *
      *   V2.14  2017-10-09  G.PETRAKIS    PER CODE TALLY TABLE ADDED *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABERRWR.
       AUTHOR. TELCABS APPLICATIONS - CONTROL GROUP.
      *****************************************************************
      * ERRLOG IS A SECOND LOG, KEPT FOR THE CONTROL GROUP, THAT RUNS *
      * IN PARALLEL WITH THE SUSPENSE FILE EACH CALLING PROGRAM OWNS. *
      * THE CALLER STILL ISSUES ITS OWN WRITE TO ITS SUSPENSE FD AND  *
      * STILL STEPS ITS OWN REJECT COUNT AFTER THIS CALL, SO THE      *
      * CALLING PROGRAM BALANCE EQUATION IS UNAFFECTED BY THIS        *
      * MODULE.  THIS MODULE WRITES ERRLOG AND NOTHING ELSE.          *
      *                                                               *
      * TWO STAGING LAYOUTS ARRIVE ON THE FIRST OPERAND.  BOTH ARE    *
      * DESCRIBED IN THE LINKAGE SECTION AND BOTH ARE NORMALISED TO   *
      * ONE 300 BYTE LOG RECORD BEFORE THE WRITE.                     *
      *                                                               *
      * RETURN CODES SET IN LK-EW-RC                                  *
      *   0000  CONDITION LOGGED                                      *
      *   0004  SEVERITY DERIVED - CALLER LEFT IT BLANK OR INVALID    *
      *   0008  ERROR CODE NOT IN THE TABLE - LOGGED UNCLASSIFIED     *
      *   0012  LAYOUT PROBE FELL THROUGH TO THE DEFAULT              *
      *   0016  ERRLOG NOT AVAILABLE - NOTHING WAS LOGGED             *
      *   0020  TALLIES DISPLAYED AND THE LOG CLOSED                  *
      * WHERE MORE THAN ONE APPLIES THE HIGHEST IS THE ONE RETURNED.  *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ERRLOG ASSIGN TO UT-S-ERRLOG
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-ERRLOG.
       DATA DIVISION.
       FILE SECTION.
      * ERRLOG - TELCABS.CABS.ERRLOG, FIXED BLOCKED 300.  THE BLOCK
      * SIZE HAS BEEN SYSTEM DETERMINED SINCE THE 2011 CHANGE.
       FD  ERRLOG
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 300 CHARACTERS.
       01  ERRLOG-RECORD                   PIC X(300).
       WORKING-STORAGE SECTION.
      * WORKING STORAGE IN A CALLED SUBPROGRAM SURVIVES FROM ONE CALL
      * TO THE NEXT INSIDE ONE RUN UNIT.  THE OPEN SWITCH, THE
      * SEQUENCE NUMBER, THE SEVERITY COUNTS AND THE PER CODE TALLY
      * TABLE ALL DEPEND ON THAT AND ARE NOT RESET PER CALL.
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE 'CABERRWR'.
           05  WS-PGM-VERSION              PIC X(05) VALUE 'V2.14'.
           05  WS-TABLE-LIMIT              PIC S9(04) COMP-3 VALUE 15.
       01  WS-FS-ERRLOG                    PIC X(02) VALUE '  '.
       01  WS-SWITCH-AREA.
           05  WS-FIRST-SW                 PIC X(01) VALUE 'Y'.
               88  WS-FIRST-CALL               VALUE 'Y'.
           05  WS-OPEN-FAILED-SW           PIC X(01) VALUE 'N'.
               88  WS-OPEN-FAILED              VALUE 'Y'.
           05  WS-CLOSED-SW                PIC X(01) VALUE 'N'.
               88  WS-LOG-CLOSED               VALUE 'Y'.
           05  WS-FORM-SW                  PIC X(01) VALUE 'A'.
               88  WS-FORM-A                   VALUE 'A'.
               88  WS-FORM-B                   VALUE 'B'.
           05  WS-PROBE-DFLT-SW            PIC X(01) VALUE 'N'.
               88  WS-PROBE-DEFAULTED          VALUE 'Y'.
           05  WS-CODE-FOUND-SW            PIC X(01) VALUE 'N'.
               88  WS-CODE-FOUND               VALUE 'Y'.
           05  WS-SEV-DERIVED-SW           PIC X(01) VALUE 'N'.
               88  WS-SEV-DERIVED              VALUE 'Y'.
       01  WS-COUNT-AREA.
           05  WS-CALL-CNT                 PIC S9(09) COMP-3 VALUE 0.
           05  WS-LOGGED-CNT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-NOT-LOGGED-CNT           PIC S9(09) COMP-3 VALUE 0.
           05  WS-WRITE-ERR-CNT            PIC S9(09) COMP-3 VALUE 0.
           05  WS-FORM-A-CNT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-FORM-B-CNT               PIC S9(09) COMP-3 VALUE 0.
           05  WS-PROBE-DFLT-CNT           PIC S9(09) COMP-3 VALUE 0.
           05  WS-UNCLASS-CNT              PIC S9(09) COMP-3 VALUE 0.
           05  WS-DERIVED-CNT              PIC S9(09) COMP-3 VALUE 0.
           05  WS-WARN-CNT                 PIC S9(09) COMP-3 VALUE 0.
           05  WS-ERROR-CNT                PIC S9(09) COMP-3 VALUE 0.
           05  WS-FATAL-CNT                PIC S9(09) COMP-3 VALUE 0.
           05  WS-SEQ-CNT                  PIC S9(09) COMP-3 VALUE 0.
       01  WS-SUBSCRIPT-AREA.
           05  WS-SUB-01                   PIC S9(04) COMP-3 VALUE 0.
           05  WS-SUB-02                   PIC S9(04) COMP-3 VALUE 0.
       01  WS-EDIT-AREA.
           05  WS-CNT-EDIT                 PIC ZZZ,ZZZ,ZZ9.
      * THE LAYOUT PROBE WORK AREA.  ONLY THE FIRST EIGHT BYTES OF
      * THE PASSED AREA ARE EXAMINED.  THE TWO VIEWS BELOW ARE THE
      * TWO SHAPES THOSE EIGHT BYTES CAN CARRY.
       01  WS-PROBE-AREA                   PIC X(08) VALUE SPACES.
       01  WS-PROBE-V1 REDEFINES WS-PROBE-AREA.
           05  WS-PB-PGM-PFX               PIC X(03).
           05  WS-PB-PGM-REST              PIC X(05).
       01  WS-PROBE-V2 REDEFINES WS-PROBE-AREA.
           05  WS-PB-ERR-ALPHA             PIC X(01).
           05  WS-PB-ERR-DIGITS            PIC 9(03).
           05  WS-PB-ERR-REST              PIC X(04).
      * THE ONE INTERNAL LOG RECORD.  WHICHEVER FORM ARRIVED IS
      * NORMALISED INTO THIS BEFORE THE WRITE.
       01  WS-LOG-RECORD.
           05  WS-LG-ERR-CODE              PIC X(04).
           05  WS-LG-SEVERITY              PIC X(01).
           05  WS-LG-DETECT-PGM            PIC X(08).
           05  WS-LG-DETECT-PARA           PIC X(30).
           05  WS-LG-RUN-ID                PIC X(12).
           05  WS-LG-ORIG-RECORD           PIC X(200).
           05  WS-LG-ERR-TEXT              PIC X(30).
           05  WS-LG-SEQ                   PIC 9(09).
           05  WS-LG-FORM                  PIC X(01).
           05  WS-LG-FILLER                PIC X(05).
      * THE FIFTEEN CONDITIONS CARRIED IN CABSERR, HELD HERE AS
      * LITERALS SO THE MODULE DOES NOT DEPEND ON THE COPYBOOK BEING
      * AVAILABLE TO A SUBPROGRAM COMPILE.
       01  WS-ERR-CODE-LIT.
           05  FILLER  PIC X(34) VALUE
               'E001OCN UNKNOWN'.
           05  FILLER  PIC X(34) VALUE
               'E002BAN UNKNOWN'.
           05  FILLER  PIC X(34) VALUE
               'E003RATE NOT FOUND'.
           05  FILLER  PIC X(34) VALUE
               'E004JURISDICTION INDETERMINATE'.
           05  FILLER  PIC X(34) VALUE
               'E005FACTOR MISSING'.
           05  FILLER  PIC X(34) VALUE
               'E006MPB PERCENT INVALID'.
           05  FILLER  PIC X(34) VALUE
               'E007DATE INVALID'.
           05  FILLER  PIC X(34) VALUE
               'E008MINUTES NEGATIVE'.
           05  FILLER  PIC X(34) VALUE
               'E009DUPLICATE SEQUENCE'.
           05  FILLER  PIC X(34) VALUE
               'E010CIRCUIT UNKNOWN'.
           05  FILLER  PIC X(34) VALUE
               'E011TERM EXPIRED'.
           05  FILLER  PIC X(34) VALUE
               'E012OUT OF BALANCE'.
           05  FILLER  PIC X(34) VALUE
               'E013RECIPROCAL CAP EXCEEDED'.
           05  FILLER  PIC X(34) VALUE
               'E014PIU OUT OF RANGE'.
           05  FILLER  PIC X(34) VALUE
               'E015RESTATE NO BASIS'.
       01  WS-ERR-CODE-TAB REDEFINES WS-ERR-CODE-LIT.
           05  WS-EC-ENTRY OCCURS 15 TIMES
                                       INDEXED BY WS-EC-IX.
               10  WS-EC-CODE              PIC X(04).
               10  WS-EC-TEXT              PIC X(30).
      * PER CODE TALLIES.  NO VALUE CLAUSE IS PERMITTED UNDER AN
      * OCCURS SO THE TABLE IS CLEARED ON THE FIRST CALL.
       01  WS-TALLY-TABLE.
           05  WS-TL-ENTRY OCCURS 15 TIMES.
               10  WS-TL-COUNT             PIC S9(09) COMP-3.
       01  WS-WORK-AREA.
           05  WS-UNCLASS-TEXT             PIC X(30) VALUE
                   'UNCLASSIFIED CONDITION'.
           05  WS-EC-SUB                   PIC S9(04) COMP-3 VALUE 0.
       LINKAGE SECTION.
      * FIRST OPERAND.  TWO CONVENTIONS ARRIVE HERE.  THE PROBE IN
      * P2000 DECIDES WHICH ONE IS PRESENT ON THIS CALL.
       01  LK-EW-AREA                      PIC X(255).
      * FORM A - THE CALLER OWNED STAGING AREA USED BY BILLCALC,
      * JURIS, FORMAT AND REPORT.  THE PROGRAM NAME LEADS.
       01  LK-EW-FORM-A REDEFINES LK-EW-AREA.
           05  LK-EA-PGM                   PIC X(08).
           05  LK-EA-PARA                  PIC X(30).
           05  LK-EA-CODE                  PIC X(04).
           05  LK-EA-SEV                   PIC X(01).
           05  LK-EA-RUN-ID                PIC X(12).
           05  LK-EA-DATA                  PIC X(200).
      * FORM B - THE CABSERR SUSPENSE RECORD USED BY RATING, INGEST
      * AND SETTLE.  THE ERROR CODE LEADS.  THE COPYBOOK RECORD IS
      * 300 BYTES AND ONLY THE FIRST 255 ARE ADDRESSED FROM HERE -
      * THE TRAILING SU-FILLER IS NOT REACHED BY THIS MODULE.
       01  LK-EW-FORM-B REDEFINES LK-EW-AREA.
           05  LK-EB-ERR-CODE              PIC X(04).
           05  LK-EB-ERR-SEVERITY          PIC X(01).
           05  LK-EB-DETECT-PGM            PIC X(08).
           05  LK-EB-DETECT-PARA           PIC X(30).
           05  LK-EB-RUN-ID                PIC X(12).
           05  LK-EB-ORIG-RECORD           PIC X(200).
       01  LK-EW-RC                        PIC 9(04).
       PROCEDURE DIVISION USING LK-EW-AREA LK-EW-RC.
      * P0000-ENTRY - ONE PASS PER CALL.
       P0000-ENTRY.
           MOVE 0 TO LK-EW-RC.
           ADD 1 TO WS-CALL-CNT.
           IF WS-FIRST-CALL
               PERFORM P1000-FIRST-CALL THRU P1000-EXIT.
           PERFORM P2000-PROBE-LAYOUT THRU P2000-EXIT.
           PERFORM P3000-NORMALISE THRU P3000-EXIT.
           IF WS-LG-ERR-CODE = 'ZZZZ' OR LK-EB-ERR-CODE = 'ZZZZ'
               PERFORM P7000-SHUTDOWN THRU P7000-EXIT
           ELSE
               PERFORM P4000-LOG-ONE THRU P4000-EXIT.
           GOBACK.
      * S100-INITIALISATION SECTION
       S100-INITIALISATION SECTION.
       P1000-FIRST-CALL.
           MOVE 'N' TO WS-FIRST-SW.
           PERFORM P1100-CLEAR-TALLIES THRU P1100-EXIT.
           PERFORM P1200-OPEN-LOG THRU P1200-EXIT.
       P1000-EXIT.
           EXIT.
       P1100-CLEAR-TALLIES.
           PERFORM P1110-CLEAR-ONE THRU P1110-EXIT
               VARYING WS-SUB-01 FROM 1 BY 1
               UNTIL WS-SUB-01 > WS-TABLE-LIMIT.
       P1100-EXIT.
           EXIT.
       P1110-CLEAR-ONE.
           MOVE 0 TO WS-TL-COUNT (WS-SUB-01).
       P1110-EXIT.
           EXIT.
      * P1200-OPEN-LOG - ERRLOG IS OPENED OUTPUT ONCE PER RUN UNIT
      * AND STAYS OPEN UNTIL A CODE OF ZZZZ CLOSES IT.
       P1200-OPEN-LOG.
           OPEN OUTPUT ERRLOG.
           IF WS-FS-ERRLOG NOT = '00'
               MOVE 'Y' TO WS-OPEN-FAILED-SW
               DISPLAY 'CABERRWR - ERRLOG OPEN STATUS ' WS-FS-ERRLOG.
       P1200-EXIT.
           EXIT.
      * S200-LAYOUT-PROBE SECTION - THE PROBE INTRODUCED WHEN THE
      * TWO STAGING CONVENTIONS WERE MERGED IN 1994.  FORM A OPENS
      * WITH AN EIGHT CHARACTER PROGRAM NAME AND EVERY CABS PROGRAM
      * NAME OPENS WITH CAB.  FORM B OPENS WITH A FOUR CHARACTER
      * ERROR CODE OF THE SHAPE ENNN.  THE TESTS ARE MADE IN THAT
      * ORDER AND FORM A IS THE DEFAULT WHEN NEITHER MATCHES.
       S200-LAYOUT-PROBE SECTION.
       P2000-PROBE-LAYOUT.
           MOVE 'N' TO WS-PROBE-DFLT-SW.
           MOVE LK-EA-PGM TO WS-PROBE-AREA.
           IF WS-PB-PGM-PFX = 'CAB'
               MOVE 'A' TO WS-FORM-SW
               ADD 1 TO WS-FORM-A-CNT
           ELSE
               PERFORM P2100-PROBE-FORM-B THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-PROBE-FORM-B.
           IF WS-PB-ERR-ALPHA = 'E' AND WS-PB-ERR-DIGITS IS NUMERIC
               MOVE 'B' TO WS-FORM-SW
               ADD 1 TO WS-FORM-B-CNT
           ELSE
               MOVE 'A' TO WS-FORM-SW
               MOVE 'Y' TO WS-PROBE-DFLT-SW
               ADD 1 TO WS-PROBE-DFLT-CNT.
       P2100-EXIT.
           EXIT.
      * S300-NORMALISE SECTION - ONE 300 BYTE RECORD IS BUILT FROM
      * WHICHEVER FORM THE PROBE SELECTED.
       S300-NORMALISE SECTION.
       P3000-NORMALISE.
           MOVE SPACES TO WS-LOG-RECORD.
           IF WS-FORM-B
               PERFORM P3100-FROM-FORM-B THRU P3100-EXIT
           ELSE
               PERFORM P3200-FROM-FORM-A THRU P3200-EXIT.
           MOVE WS-FORM-SW TO WS-LG-FORM.
           MOVE 0 TO WS-LG-SEQ.
       P3000-EXIT.
           EXIT.
       P3100-FROM-FORM-B.
           MOVE LK-EB-ERR-CODE TO WS-LG-ERR-CODE.
           MOVE LK-EB-ERR-SEVERITY TO WS-LG-SEVERITY.
           MOVE LK-EB-DETECT-PGM TO WS-LG-DETECT-PGM.
           MOVE LK-EB-DETECT-PARA TO WS-LG-DETECT-PARA.
           MOVE LK-EB-RUN-ID TO WS-LG-RUN-ID.
           MOVE LK-EB-ORIG-RECORD TO WS-LG-ORIG-RECORD.
       P3100-EXIT.
           EXIT.
       P3200-FROM-FORM-A.
           MOVE LK-EA-CODE TO WS-LG-ERR-CODE.
           MOVE LK-EA-SEV TO WS-LG-SEVERITY.
           MOVE LK-EA-PGM TO WS-LG-DETECT-PGM.
           MOVE LK-EA-PARA TO WS-LG-DETECT-PARA.
           MOVE LK-EA-RUN-ID TO WS-LG-RUN-ID.
           MOVE LK-EA-DATA TO WS-LG-ORIG-RECORD.
       P3200-EXIT.
           EXIT.
      * S400-LOGGING SECTION
       S400-LOGGING SECTION.
       P4000-LOG-ONE.
           PERFORM P4100-VALIDATE-CODE THRU P4100-EXIT.
           PERFORM P4200-DERIVE-SEVERITY THRU P4200-EXIT.
           PERFORM P4300-TALLY THRU P4300-EXIT.
           PERFORM P4400-WRITE-LOG THRU P4400-EXIT.
           PERFORM P6000-SET-RETURN-CODE THRU P6000-EXIT.
       P4000-EXIT.
           EXIT.
      * P4100-VALIDATE-CODE - FIFTEEN ENTRIES, SERIAL SEARCH.  A
      * CODE THAT IS NOT IN THE TABLE IS STILL WRITTEN.
       P4100-VALIDATE-CODE.
           MOVE 'N' TO WS-CODE-FOUND-SW.
           MOVE 0 TO WS-EC-SUB.
           SET WS-EC-IX TO 1.
           SEARCH WS-EC-ENTRY
               AT END
                   MOVE WS-UNCLASS-TEXT TO WS-LG-ERR-TEXT
               WHEN WS-EC-CODE (WS-EC-IX) = WS-LG-ERR-CODE
                   MOVE 'Y' TO WS-CODE-FOUND-SW
                   MOVE WS-EC-TEXT (WS-EC-IX) TO WS-LG-ERR-TEXT
                   SET WS-EC-SUB TO WS-EC-IX.
           IF NOT WS-CODE-FOUND
               ADD 1 TO WS-UNCLASS-CNT.
       P4100-EXIT.
           EXIT.
      * P4200-DERIVE-SEVERITY - THE DERIVATION HAS STOOD AS IT WAS
      * SET IN 1999.  E007, E009 AND E014 ARE WARNINGS, E012 IS
      * FATAL, EVERY OTHER CONDITION IS AN ERROR.
       P4200-DERIVE-SEVERITY.
           MOVE 'N' TO WS-SEV-DERIVED-SW.
           IF WS-LG-SEVERITY = 'W' OR WS-LG-SEVERITY = 'E' OR
              WS-LG-SEVERITY = 'F'
               GO TO P4200-EXIT.
           MOVE 'Y' TO WS-SEV-DERIVED-SW.
           ADD 1 TO WS-DERIVED-CNT.
           IF WS-LG-ERR-CODE = 'E007' OR WS-LG-ERR-CODE = 'E009' OR
              WS-LG-ERR-CODE = 'E014'
               MOVE 'W' TO WS-LG-SEVERITY
           ELSE
               IF WS-LG-ERR-CODE = 'E012'
                   MOVE 'F' TO WS-LG-SEVERITY
               ELSE
                   MOVE 'E' TO WS-LG-SEVERITY.
       P4200-EXIT.
           EXIT.
       P4300-TALLY.
           IF WS-LG-SEVERITY = 'W'
               ADD 1 TO WS-WARN-CNT.
           IF WS-LG-SEVERITY = 'E'
               ADD 1 TO WS-ERROR-CNT.
           IF WS-LG-SEVERITY = 'F'
               ADD 1 TO WS-FATAL-CNT.
           IF WS-EC-SUB > 0
               ADD 1 TO WS-TL-COUNT (WS-EC-SUB).
       P4300-EXIT.
           EXIT.
      * P4400-WRITE-LOG - FILE STATUS IS TESTED ON THE WRITE.  A
      * CALL THAT ARRIVES AFTER THE LOG HAS BEEN CLOSED IS COUNTED
      * AND REPORTED THE SAME WAY AS AN OPEN THAT DID NOT SUCCEED.
       P4400-WRITE-LOG.
           IF WS-OPEN-FAILED OR WS-LOG-CLOSED
               ADD 1 TO WS-NOT-LOGGED-CNT
               GO TO P4400-EXIT.
           ADD 1 TO WS-SEQ-CNT.
           MOVE WS-SEQ-CNT TO WS-LG-SEQ.
           MOVE WS-LOG-RECORD TO ERRLOG-RECORD.
           WRITE ERRLOG-RECORD.
           IF WS-FS-ERRLOG NOT = '00'
               ADD 1 TO WS-WRITE-ERR-CNT
               DISPLAY 'CABERRWR - ERRLOG WRITE STATUS '
                   WS-FS-ERRLOG
           ELSE
               ADD 1 TO WS-LOGGED-CNT.
       P4400-EXIT.
           EXIT.
      * S600-RETURN-CODE SECTION
       S600-RETURN-CODE SECTION.
       P6000-SET-RETURN-CODE.
           MOVE 0 TO LK-EW-RC.
           IF WS-SEV-DERIVED
               MOVE 4 TO LK-EW-RC.
           IF NOT WS-CODE-FOUND
               MOVE 8 TO LK-EW-RC.
           IF WS-PROBE-DEFAULTED
               MOVE 12 TO LK-EW-RC.
           IF WS-OPEN-FAILED OR WS-LOG-CLOSED
               MOVE 16 TO LK-EW-RC.
       P6000-EXIT.
           EXIT.
      * S700-SHUTDOWN SECTION - THE CALLING PROGRAMS SHUT THE LOG
      * DOWN AT END OF JOB BY PASSING AN ERROR CODE OF ZZZZ.
       S700-SHUTDOWN SECTION.
       P7000-SHUTDOWN.
           PERFORM P7100-DISPLAY-TALLIES THRU P7100-EXIT.
           IF NOT WS-OPEN-FAILED
               IF NOT WS-LOG-CLOSED
                   CLOSE ERRLOG
                   MOVE 'Y' TO WS-CLOSED-SW
                   IF WS-FS-ERRLOG NOT = '00'
                       DISPLAY 'CABERRWR - ERRLOG CLOSE STATUS '
                           WS-FS-ERRLOG.
           MOVE 20 TO LK-EW-RC.
       P7000-EXIT.
           EXIT.
       P7100-DISPLAY-TALLIES.
           DISPLAY 'CABERRWR ' WS-PGM-VERSION ' - ERROR LOG SUMMARY'.
           MOVE WS-CALL-CNT TO WS-CNT-EDIT.
           DISPLAY '  CALLS RECEIVED   = ' WS-CNT-EDIT.
           MOVE WS-LOGGED-CNT TO WS-CNT-EDIT.
           DISPLAY '  RECORDS LOGGED   = ' WS-CNT-EDIT.
           MOVE WS-NOT-LOGGED-CNT TO WS-CNT-EDIT.
           DISPLAY '  NOT LOGGED       = ' WS-CNT-EDIT.
           MOVE WS-WRITE-ERR-CNT TO WS-CNT-EDIT.
           DISPLAY '  WRITE STATUSES   = ' WS-CNT-EDIT.
           MOVE WS-FORM-A-CNT TO WS-CNT-EDIT.
           DISPLAY '  FORM A CALLS     = ' WS-CNT-EDIT.
           MOVE WS-FORM-B-CNT TO WS-CNT-EDIT.
           DISPLAY '  FORM B CALLS     = ' WS-CNT-EDIT.
           MOVE WS-PROBE-DFLT-CNT TO WS-CNT-EDIT.
           DISPLAY '  PROBE DEFAULTED  = ' WS-CNT-EDIT.
           MOVE WS-UNCLASS-CNT TO WS-CNT-EDIT.
           DISPLAY '  UNCLASSIFIED     = ' WS-CNT-EDIT.
           MOVE WS-DERIVED-CNT TO WS-CNT-EDIT.
           DISPLAY '  SEVERITY DERIVED = ' WS-CNT-EDIT.
           MOVE WS-WARN-CNT TO WS-CNT-EDIT.
           DISPLAY '  SEVERITY W       = ' WS-CNT-EDIT.
           MOVE WS-ERROR-CNT TO WS-CNT-EDIT.
           DISPLAY '  SEVERITY E       = ' WS-CNT-EDIT.
           MOVE WS-FATAL-CNT TO WS-CNT-EDIT.
           DISPLAY '  SEVERITY F       = ' WS-CNT-EDIT.
           DISPLAY '  BY CONDITION'.
           PERFORM P7200-DISPLAY-ONE THRU P7200-EXIT
               VARYING WS-SUB-02 FROM 1 BY 1
               UNTIL WS-SUB-02 > WS-TABLE-LIMIT.
       P7100-EXIT.
           EXIT.
       P7200-DISPLAY-ONE.
           MOVE WS-TL-COUNT (WS-SUB-02) TO WS-CNT-EDIT.
           DISPLAY '    ' WS-EC-CODE (WS-SUB-02) ' '
               WS-EC-TEXT (WS-SUB-02) ' ' WS-CNT-EDIT.
       P7200-EXIT.
           EXIT.
