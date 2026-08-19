      ******************************************************************
      * CABRAT08 - RATE OVERRIDE HANDLER                               *
      * APPLICATION : CABS                                             *
      * INPUTS      : DDNAME  DSN                     COPYBOOK         *
      *               RATIN   TELCABS.CABS.RATED(0)    (LOCAL)         *
      *               OVRIN   TELCABS.CABS.RATEOVR      (LOCAL)        *
      * OUTPUTS     : DDNAME  DSN                     COPYBOOK         *
      *               OVROUT  TELCABS.CABS.RATED.OVERRIDE(+1)(LOCAL)   *
      *               RPTOUT  SYSOUT CLASS A            CABSPRNT       *
      * CONTROL     : CTLOUT                          CABSCTL          *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +             *
      *               CT-SUMMARISED + CT-CARRIED-FWD                   *
      * RESTART     : FULL RERUN                                       *
      * REVISION HISTORY                                               *
      *   V1.00 1994-09-12 D.OKONKWO    INITIAL - CONTRACT RATE        *
      *                    OVERRIDES ONLY, FLAT FILE, NO PRIORITY      *
      *   V1.03 1998-04-05 J.M.CASTILLO REGULATORY OVERRIDE TYPE       *
      *                    ADDED, ALWAYS WINS OVER CONTRACT            *
      *   V1.06 2002-11-20 P.NAIR       PROMOTIONAL TYPE ADDED,        *
      *                    LOWEST PRIORITY OF THE THREE                *
      *   V1.09 2006-07-01 A.BUKOWSKI   DISPUTE-HOLD TYPE ADDED -      *
      *                    SUSPENDS THE ELEMENT PENDING RESOLUTION     *
      *   V1.12 2010-03-15 S.MARCHETTI  IN-STORAGE TABLE NOW KEPT      *
      *                    SORTED ON LOAD, BINARY SEARCH REPLACES      *
      *                    THE OLD SEQUENTIAL SCAN                     *
      *   V1.14 2015-08-22 L.FERREIRA   DYNAMIC CALL PATH ADDED FOR    *
      *                    CUSTOM CONTRACT RATING MODULES              *
      *   V1.17 2019-05-30 G.PRZYBYLSKI RECOMPILE ONLY - LE V8         *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABRAT08.
       AUTHOR. TELCABS APPLICATIONS - RATING TEAM.
      ******************************************************************
      * RATE OVERRIDE HANDLER.  LOADS OVRIN INTO A SORTED IN-          *
      * STORAGE TABLE (OCCURS 500), THEN RE-RATES ANY RATIN ELEMENT    *
      * THAT MATCHES AN ACTIVE OVERRIDE - REGULATORY BEATS             *
      * CONTRACT BEATS PROMOTIONAL, GATED BY R1-OVERRIDE-PRIORITY.     *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RATIN ASSIGN TO UT-S-RATIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT.
           SELECT OVRIN ASSIGN TO UT-S-OVRIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-TABLE.
           SELECT OVROUT ASSIGN TO UT-S-OVROUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT CTLOUT ASSIGN TO UT-S-CTLOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-CONTROL.
           SELECT RPTOUT ASSIGN TO UT-S-RPTOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUTPUT.
       DATA DIVISION.
       FILE SECTION.
      * RATIN - RATED-ELEMENT INPUT.  SAME 200-BYTE LOCAL SHAPE        *
      * USED ACROSS THE CABRAT07/08/09 FAMILY.                         *
       FD  RATIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       01  CABS-RATIN-RECORD.
           05  RI-OCN                      PIC X(04).
           05  RI-BAN                      PIC X(13).
           05  RI-BILL-PERIOD              PIC 9(06).
           05  RI-SECTION                  PIC X(02).
           05  RI-SEQ-NBR                  PIC 9(09) COMP-3.
           05  RI-CIRCUIT-ID               PIC X(20).
           05  RI-JURIS-CD                 PIC X(01).
           05  RI-STATE-CD                 PIC X(02).
           05  RI-RATE-ELEM                PIC X(06).
           05  RI-QTY                      PIC S9(13)V9(02)
               COMP-3.
           05  RI-RATE                     PIC S9(05)V9(05)
               COMP-3.
           05  RI-AMOUNT                   PIC S9(11)V9(05)
               COMP-3.
           05  RI-ROUND-RULE               PIC X(01).
           05  RI-SRC-PROCESS              PIC X(08).
           05  RI-LINE-TYPE                PIC X(01).
           05  RI-DESCRIPTION              PIC X(60).
           05  RI-AUTH-REF                 PIC X(20).
           05  RI-FILLER                   PIC X(28).
      * OVRIN - OVERRIDE CONTROL CARDS, CARD-IMAGE FB 80.  PARSED      *
      * BY P1330-PARSE-CARD - SEE WORKING-STORAGE FOR THE LAYOUT.      *
       FD  OVRIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  CABS-OVRIN-RECORD              PIC X(80).
      * OVROUT - RE-RATED ELEMENTS PLUS UNCHANGED PASS-THROUGH,        *
      * SAME 200-BYTE SHAPE AS RATIN.                                  *
       FD  OVROUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       01  CABS-OVROUT-RECORD.
           05  OV-OCN                      PIC X(04).
           05  OV-BAN                      PIC X(13).
           05  OV-BILL-PERIOD              PIC 9(06).
           05  OV-SECTION                  PIC X(02).
           05  OV-SEQ-NBR                  PIC 9(09) COMP-3.
           05  OV-CIRCUIT-ID               PIC X(20).
           05  OV-JURIS-CD                 PIC X(01).
           05  OV-STATE-CD                 PIC X(02).
           05  OV-RATE-ELEM                PIC X(06).
           05  OV-QTY                      PIC S9(13)V9(02)
               COMP-3.
           05  OV-RATE                     PIC S9(05)V9(05)
               COMP-3.
           05  OV-AMOUNT                   PIC S9(11)V9(05)
               COMP-3.
           05  OV-ROUND-RULE               PIC X(01).
           05  OV-SRC-PROCESS              PIC X(08).
           05  OV-LINE-TYPE                PIC X(01).
           05  OV-DESCRIPTION              PIC X(60).
           05  OV-AUTH-REF                 PIC X(20).
           05  OV-FILLER                   PIC X(28).
      * CTLOUT - RUN CONTROL / BALANCING RECORD.                       *
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CABS-CTLOUT-RECORD             PIC X(180).
      * RPTOUT - PRINT REPORT.                                         *
       FD  RPTOUT
           RECORDING MODE IS FA
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       COPY CABSPRNT.
       WORKING-STORAGE SECTION.
      * STANDARD SHARED WORKING STORAGE - SEE CABSWRK.                 *
       COPY CABSWRK.
      * RATING FAMILY CONTROL BLOCKS - ONE COPY PULLS ALL FOUR.        *
       COPY CABSRT01.
      * PROGRAM CONSTANTS / LINE-TYPE LITERALS / SYSIN PARM CARD.      *
       01  WS-PROGRAM-CONSTANTS.
           05  WS-PGM-NAME                 PIC X(08) VALUE
               'CABRAT08'.
           05  WS-LT-DETAIL                PIC X(01) VALUE 'D'.
           05  WS-LT-OVERRIDE              PIC X(01) VALUE 'O'.
       01  WS-PARM-CARD                PIC X(80).
       01  WS-PARM-CARD-R1 REDEFINES WS-PARM-CARD.
           05  PC1-CYCLE-YYDDD             PIC 9(05).
           05  PC1-BILL-PERIOD             PIC 9(06).
           05  PC1-TARIFF-CD               PIC X(04).
           05  PC1-RUN-ID                  PIC X(12).
           05  PC1-FILLER                  PIC X(53).
      * OVERRIDE CARD IMAGE, PLUS A CHARACTER-ARRAY REDEFINE FOR THE   *
      * COLUMN-BY-COLUMN SCAN IN P1320 - NO REFERENCE MODIFICATION     *
      * ANYWHERE IN THIS PROGRAM.                                      *
       01  WS-OVR-CARD-IMAGE           PIC X(80).
       01  WS-OVR-CARD-CHARS REDEFINES WS-OVR-CARD-IMAGE.
           05  WS-OVC-CHAR OCCURS 80 TIMES PIC X(01).
      * THE 9 UNSTRING TARGET FIELDS - COMMA-DELIMITED PORTION OF      *
      * THE CARD.  OVR-TYPE AND THE DYNAMIC MODULE NAME OCCUPY         *
      * FIXED TRAILING COLUMNS AFTER THE 9TH COMMA - SEE P1330.        *
       01  WS-CARD-FIELDS.
           05  WS-CF-OCN                   PIC X(04).
           05  WS-CF-OCN-LEN               PIC 9(02) COMP-3.
           05  WS-CF-BAN                   PIC X(13).
           05  WS-CF-BAN-LEN               PIC 9(02) COMP-3.
           05  WS-CF-ELEM                  PIC X(06).
           05  WS-CF-JURIS                 PIC X(01).
           05  WS-CF-STATE                 PIC X(02).
           05  WS-CF-RATE                  PIC 9(05)V9(05).
           05  WS-CF-EFF-FROM              PIC 9(05).
           05  WS-CF-EFF-THRU              PIC 9(05).
           05  WS-CF-AUTH-REF              PIC X(20).
           05  WS-CF-FIELDS-TALLIED        PIC 9(02) COMP-3
               VALUE 0.
           05  WS-CF-OVR-TYPE              PIC X(01).
           05  WS-CF-MODULE-NAME           PIC X(08).
      * CARD VALIDATION COUNTS FROM THE INSPECT/CHARACTER WALK.        *
       01  WS-CARD-SCAN-WORK.
           05  WS-CS-COMMA-CNT             PIC 9(03) VALUE 0.
           05  WS-CS-SPACE-CNT             PIC 9(03) VALUE 0.
           05  WS-CS-NONNUM-CNT            PIC 9(03) VALUE 0.
           05  WS-CS-CX                    PIC 9(02) VALUE 0.
           05  WS-CS-CARD-OK-SW            PIC X(01) VALUE 'Y'.
               88  WS-CS-CARD-OK               VALUE 'Y'.
      * SORTED IN-STORAGE OVERRIDE TABLE.  KEY IS OCN/BAN/ELEM/        *
      * JURIS/STATE - MULTIPLE TYPES CAN SHARE ONE KEY, SO ENTRIES     *
      * WITH THE SAME KEY SIT ADJACENT AND P4100 SCANS THEM FOR        *
      * THE BEST PRIORITY MATCH.                                       *
       01  WS-OVERRIDE-TABLE.
           05  WS-OVR-CNT                  PIC 9(03) VALUE 0.
           05  WS-OVR-ENTRY OCCURS 1 TO 500 TIMES
               DEPENDING ON WS-OVR-CNT
               INDEXED BY WS-OVR-X.
               10  WS-OVR-KEY.
                   15  WS-OVR-OCN                 PIC X(04).
                   15  WS-OVR-BAN                 PIC X(13).
                   15  WS-OVR-ELEM                PIC X(06).
                   15  WS-OVR-JURIS                PIC X(01).
                   15  WS-OVR-STATE                PIC X(02).
               10  WS-OVR-KEY-FLAT REDEFINES WS-OVR-KEY.
                   15  WS-OVR-KEY-VIEW            PIC X(26).
               10  WS-OVR-RATE                 PIC S9(05)V9(05)
                   COMP-3.
               10  WS-OVR-EFF-FROM             PIC 9(05).
               10  WS-OVR-EFF-THRU             PIC 9(05).
               10  WS-OVR-AUTH-REF             PIC X(20).
               10  WS-OVR-TYPE                 PIC X(01).
                   88  WS-OVR-IS-CONTRACT          VALUE 'C'.
                   88  WS-OVR-IS-REGULATORY        VALUE 'R'.
                   88  WS-OVR-IS-DISPUTE           VALUE 'D'.
                   88  WS-OVR-IS-PROMO             VALUE 'P'.
               10  WS-OVR-PRIORITY             PIC 9(01).
               10  WS-OVR-MODULE-NAME          PIC X(08).
      * SEARCH KEY BUILT FROM THE CURRENT RATIN RECORD, COMPARED       *
      * AGAINST WS-OVR-KEY-VIEW (WS-OVR-X) DURING THE BINARY           *
      * SEARCH - SEE P4000/P4050.                                      *
       01  WS-SEARCH-KEY-WORK.
           05  WS-SK-OCN                   PIC X(04).
           05  WS-SK-BAN                   PIC X(13).
           05  WS-SK-ELEM                  PIC X(06).
           05  WS-SK-JURIS                 PIC X(01).
           05  WS-SK-STATE                 PIC X(02).
       01  WS-SEARCH-KEY-FLAT REDEFINES WS-SEARCH-KEY-WORK.
           05  WS-SK-FLAT-VIEW             PIC X(26).
      * AUDIT KEY ASSEMBLY WORK - 5 FRAGMENTS COMBINED BY STRING       *
      * IN P3300 INTO THE AUDIT TRAIL REFERENCE STAMPED ON OVROUT.     *
       01  WS-AUDIT-KEY-WORK.
           05  WS-AK-FRAG1                 PIC X(01).
           05  WS-AK-FRAG2                 PIC X(01).
           05  WS-AK-FRAG3                 PIC X(20).
           05  WS-AK-FRAG4                 PIC X(01).
           05  WS-AK-FRAG5                 PIC X(09).
           05  WS-AK-BUILT                 PIC X(32).
           05  WS-AK-SEQ-TEXT              PIC 9(09).
      * BINARY SEARCH WORK FOR THE OVERRIDE TABLE.                     *
       01  WS-BINARY-SEARCH-WORK.
           05  WS-BS-LOW                   PIC S9(04) COMP-3.
           05  WS-BS-HIGH                  PIC S9(04) COMP-3.
           05  WS-BS-MID                   PIC S9(04) COMP-3.
           05  WS-BS-FOUND-SW              PIC X(01) VALUE 'N'.
               88  WS-BS-FOUND                 VALUE 'Y'.
           05  WS-BS-LANDED-SUB            PIC S9(04) COMP-3.
      * OVERRIDE MATCH RESULT - WHAT P4000 HANDS BACK TO P2200.        *
       01  WS-MATCH-RESULT.
           05  WS-MR-FOUND-SW              PIC X(01) VALUE 'N'.
               88  WS-MR-FOUND                 VALUE 'Y'.
           05  WS-MR-BEST-SUB              PIC S9(04) COMP-3
               VALUE 0.
           05  WS-MR-BEST-PRIORITY         PIC 9(01) VALUE 9.
      * DYNAMIC CALL WORK - P3400.  THE TARGET IS NOT KNOWABLE         *
      * STATICALLY, IT COMES FROM THE OVERRIDE CARD.                   *
       01  WS-DYNCALL-WORK.
           05  WS-DC-TARGET                PIC X(08).
           05  WS-DC-USED-SW               PIC X(01) VALUE 'N'.
               88  WS-DC-USED                  VALUE 'Y'.
           05  WS-DC-COMPUTED-AMT          PIC S9(11)V9(05)
               COMP-3 VALUE 0.
           05  WS-DC-RC                    PIC 9(04) VALUE 0.
      * MISC RUN COUNTERS, ABEND WORK, CALL RETURN CODES.              *
       01  WS-MISC-COUNTERS.
           05  WS-MC-CARDS-READ            PIC S9(05) COMP-3
               VALUE 0.
           05  WS-MC-CARDS-REJECTED        PIC S9(05) COMP-3
               VALUE 0.
           05  WS-MC-OVERRIDES-APPLIED     PIC S9(09) COMP-3
               VALUE 0.
           05  WS-MC-DYNAMIC-CALLS         PIC S9(09) COMP-3
               VALUE 0.
           05  WS-MC-PASSTHRU-CNT          PIC S9(09) COMP-3
               VALUE 0.
           05  WS-MC-BLOCKED-BY-PRIORITY   PIC S9(09) COMP-3
               VALUE 0.
       01  WS-ABEND-WORK.
           05  WS-AB-PGM                   PIC X(08).
           05  WS-AB-PARA                  PIC X(30).
           05  WS-AB-REASON                PIC X(60).
           05  WS-AB-USER-CODE             PIC 9(04) VALUE 9908.
       01  WS-EXT-CALL-RC.
           05  WS-RC-DTCNV                 PIC 9(04) VALUE 0.
           05  WS-RC-HASH                  PIC 9(04) VALUE 0.
           05  WS-RC-PARMR                 PIC 9(04) VALUE 0.
      * MODULE-NAME CHARACTER STAGING - BUILT COLUMN BY COLUMN FROM    *
      * WS-OVC-CHAR DURING P1320, NO REFERENCE MODIFICATION USED.      *
       01  WS-MODULE-SCAN-WORK.
           05  WS-MN-TEXT                  PIC X(08).
       01  WS-MN-CHARS REDEFINES WS-MODULE-SCAN-WORK.
           05  WS-MNC-CHAR OCCURS 8 TIMES PIC X(01).
       01  WS-MN-SUB                   PIC 9(01) VALUE 0.
      * DEDICATED EOF SWITCH FOR THE OVRIN LOAD LOOP - KEPT SEPARATE   *
      * FROM THE STANDARD WS-EOF-SW, WHICH THE MAIN RATIN LOOP OWNS.   *
       01  WS-OVRIN-READ-WORK.
           05  WS-OV-EOF-SW                PIC X(01) VALUE 'N'.
               88  WS-OV-EOF                   VALUE 'Y'.
      * SHIFT/SCAN SUBSCRIPTS FOR THE SORTED-INSERT AND NEIGHBOUR-     *
      * SCAN LOGIC.  PLAIN COMP-3 SUBSCRIPTS, NOT INDEX-NAMES, SO      *
      * THEY CAN BE COMPUTED WITH ARITHMETIC DIRECTLY.                 *
       01  WS-SHIFT-SCAN-WORK.
           05  WS-SHIFT-SUB                PIC S9(04) COMP-3.
           05  WS-SHIFT-FROM               PIC S9(04) COMP-3.
           05  WS-SCAN-SUB                 PIC S9(04) COMP-3.
           05  WS-CAND-OK-SW               PIC X(01) VALUE 'N'.
       PROCEDURE DIVISION.
       P0000-MAINLINE.
           PERFORM P1000-INIT THRU P1000-EXIT.
           PERFORM P2000-PROCESS THRU P2000-EXIT UNTIL WS-EOF.
           PERFORM P8000-CONTROL THRU P8000-EXIT.
           PERFORM P9000-TERM THRU P9000-EXIT.
           STOP RUN.
      * S100-INITIALISATION SECTION                                    *
       S100-INITIALISATION SECTION.
       P1000-INIT.
           PERFORM P1100-OPEN-FILES THRU P1100-EXIT.
           PERFORM P1200-READ-PARM THRU P1200-EXIT.
           PERFORM P1300-LOAD-OVERRIDE-TABLE THRU P1300-EXIT.
           PERFORM P1700-INIT-COUNTERS THRU P1700-EXIT.
           PERFORM P2100-READ-RATIN THRU P2100-EXIT.
       P1000-EXIT.
           EXIT.
       P1100-OPEN-FILES.
           OPEN INPUT RATIN.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RATIN OPEN FAILED' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN INPUT OVRIN.
           IF WS-FS-TABLE NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OVRIN OPEN FAILED' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT OVROUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'OVROUT OPEN FAILED' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT CTLOUT.
           IF WS-FS-CONTROL NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'CTLOUT OPEN FAILED' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
           OPEN OUTPUT RPTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA
               MOVE 'RPTOUT OPEN FAILED' TO WS-AB-REASON
               PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.
       P1100-EXIT.
           EXIT.
       P1200-READ-PARM.
           MOVE SPACES TO WS-PARM-CARD.
           ACCEPT WS-PARM-CARD FROM SYSIN.
           CALL 'CABPARMR' USING WS-PARM-CARD WS-RC-PARMR.
           MOVE PC1-CYCLE-YYDDD TO R1-CYCLE-YYDDD.
           MOVE PC1-BILL-PERIOD TO R1-BILL-PERIOD.
           MOVE PC1-TARIFF-CD TO R1-TARIFF-CD.
           MOVE PC1-RUN-ID TO R1-RUN-ID.
           MOVE PC1-CYCLE-YYDDD TO DW-CURRENT-YYDDD.
           CALL 'CABDTCNV' USING DW-CURRENT-YYDDD DW-GREG-DATE
               WS-RC-DTCNV.
       P1200-EXIT.
           EXIT.
      * P1300-LOAD-OVERRIDE-TABLE - READS OVRIN IN FULL INTO THE       *
      * SORTED IN-STORAGE TABLE.  USES ITS OWN EOF SWITCH, NOT THE     *
      * STANDARD ONE - SEE WS-OVRIN-READ-WORK.                         *
       P1300-LOAD-OVERRIDE-TABLE.
           MOVE 0 TO WS-OVR-CNT.
           MOVE 'N' TO WS-OV-EOF-SW.
           PERFORM P1310-READ-ONE-CARD THRU P1310-EXIT.
           PERFORM P1315-PROCESS-ONE-CARD THRU P1315-EXIT
               UNTIL WS-OV-EOF.
       P1300-EXIT.
           EXIT.
       P1310-READ-ONE-CARD.
           READ OVRIN INTO WS-OVR-CARD-IMAGE
               AT END MOVE 'Y' TO WS-OV-EOF-SW.
           IF NOT WS-OV-EOF
               ADD 1 TO WS-MC-CARDS-READ.
       P1310-EXIT.
           EXIT.
       P1315-PROCESS-ONE-CARD.
           PERFORM P1320-VALIDATE-AND-SCAN-CARD THRU P1320-EXIT.
           IF WS-CS-CARD-OK
               PERFORM P1330-PARSE-CARD THRU P1330-EXIT
               PERFORM P1340-INSERT-SORTED THRU P1340-EXIT
           ELSE
               ADD 1 TO WS-MC-CARDS-REJECTED.
           PERFORM P1310-READ-ONE-CARD THRU P1310-EXIT.
       P1315-EXIT.
           EXIT.
      * P1320 - CABS-STD-023: INSPECT TALLYING FOR ALL PLUS A         **
      * SUBSCRIPTED PERFORM VARYING WALK OVER THE CARD REDEFINED AS    *
      * OCCURS 80 TIMES.  NO REFERENCE MODIFICATION.  THE RATE-        *
      * FIELD COLUMN RANGE (41-50) IS HARDCODED - IF A CARD ARRIVES    *
      * WITH SHORTER OCN/BAN/ELEM TEXT THAN USUAL THE COMMA-           *
      * DELIMITED FIELDS SHIFT LEFT AND THIS CHECK QUIETLY LOOKS AT    *
      * THE WRONG COLUMNS.  KNOWN, NEVER FIXED - CABS-STD-041.         *
       P1320-VALIDATE-AND-SCAN-CARD.
           MOVE 0 TO WS-CS-COMMA-CNT.
           MOVE 0 TO WS-CS-SPACE-CNT.
           MOVE 0 TO WS-CS-NONNUM-CNT.
           MOVE 'Y' TO WS-CS-CARD-OK-SW.
           INSPECT WS-OVR-CARD-IMAGE TALLYING WS-CS-COMMA-CNT
               FOR ALL ','.
           INSPECT WS-OVR-CARD-IMAGE TALLYING WS-CS-SPACE-CNT
               FOR ALL ' '.
           PERFORM P1325-SCAN-ONE-COLUMN THRU P1325-EXIT
               VARYING WS-CS-CX FROM 1 BY 1 UNTIL WS-CS-CX > 80.
           IF WS-CS-COMMA-CNT < 8
               MOVE 'N' TO WS-CS-CARD-OK-SW.
           IF WS-CS-NONNUM-CNT > 3
               MOVE 'N' TO WS-CS-CARD-OK-SW.
           MOVE WS-OVC-CHAR (70) TO WS-CF-OVR-TYPE.
           PERFORM P1327-COPY-MODULE-CHAR THRU P1327-EXIT
               VARYING WS-MN-SUB FROM 1 BY 1 UNTIL WS-MN-SUB > 8.
           MOVE WS-MN-TEXT TO WS-CF-MODULE-NAME.
       P1320-EXIT.
           EXIT.
       P1325-SCAN-ONE-COLUMN.
           IF WS-CS-CX >= 41 AND WS-CS-CX <= 50
               IF WS-OVC-CHAR (WS-CS-CX) NOT NUMERIC AND
                   WS-OVC-CHAR (WS-CS-CX) NOT = ' '
               ADD 1 TO WS-CS-NONNUM-CNT.
       P1325-EXIT.
           EXIT.
       P1327-COPY-MODULE-CHAR.
           COMPUTE WS-CS-CX = 70 + WS-MN-SUB.
           IF WS-CS-CX <= 80
               MOVE WS-OVC-CHAR (WS-CS-CX) TO WS-MNC-CHAR (WS-MN-SUB).
       P1327-EXIT.
           EXIT.
      * P1330 - CABS-STD-020: UNSTRING THE 9 COMMA-DELIMITED CARD     **
      * FIELDS, WITH COUNT IN ON TWO OF THEM AND ONE TALLYING IN       *
      * FOR THE WHOLE STATEMENT.                                       *
       P1330-PARSE-CARD.
           MOVE 0 TO WS-CF-FIELDS-TALLIED.
           UNSTRING WS-OVR-CARD-IMAGE DELIMITED BY ','
               INTO WS-CF-OCN COUNT IN WS-CF-OCN-LEN
                   WS-CF-BAN COUNT IN WS-CF-BAN-LEN
                   WS-CF-ELEM
                   WS-CF-JURIS
                   WS-CF-STATE
                   WS-CF-RATE
                   WS-CF-EFF-FROM
                   WS-CF-EFF-THRU
                   WS-CF-AUTH-REF
               TALLYING IN WS-CF-FIELDS-TALLIED.
       P1330-EXIT.
           EXIT.
      * P1340/P1350/P1355 - REAL BINARY SEARCH FOR THE SORTED          *
      * INSERTION POINT, NOT A LINEAR SCAN.                            *
       P1340-INSERT-SORTED.
           MOVE WS-CF-OCN TO WS-SK-OCN.
           MOVE WS-CF-BAN TO WS-SK-BAN.
           MOVE WS-CF-ELEM TO WS-SK-ELEM.
           MOVE WS-CF-JURIS TO WS-SK-JURIS.
           MOVE WS-CF-STATE TO WS-SK-STATE.
           PERFORM P1350-FIND-INSERT-POINT THRU P1350-EXIT.
           IF WS-OVR-CNT < 500
               PERFORM P1360-SHIFT-ENTRIES-UP THRU P1360-EXIT
               PERFORM P1370-STORE-NEW-ENTRY THRU P1370-EXIT
           ELSE
               ADD 1 TO WS-MC-CARDS-REJECTED.
       P1340-EXIT.
           EXIT.
       P1350-FIND-INSERT-POINT.
           MOVE 1 TO WS-BS-LOW.
           MOVE WS-OVR-CNT TO WS-BS-HIGH.
           IF WS-OVR-CNT = 0
               MOVE 1 TO WS-BS-LANDED-SUB
           ELSE
               PERFORM P1355-NARROW-RANGE THRU P1355-EXIT
                   UNTIL WS-BS-LOW > WS-BS-HIGH
               MOVE WS-BS-LOW TO WS-BS-LANDED-SUB.
       P1350-EXIT.
           EXIT.
       P1355-NARROW-RANGE.
           COMPUTE WS-BS-MID = (WS-BS-LOW + WS-BS-HIGH) / 2.
           SET WS-OVR-X TO WS-BS-MID.
           IF WS-OVR-KEY-VIEW (WS-OVR-X) > WS-SK-FLAT-VIEW
               COMPUTE WS-BS-HIGH = WS-BS-MID - 1
           ELSE
               COMPUTE WS-BS-LOW = WS-BS-MID + 1.
       P1355-EXIT.
           EXIT.
       P1360-SHIFT-ENTRIES-UP.
           ADD 1 TO WS-OVR-CNT.
           IF WS-OVR-CNT > WS-BS-LANDED-SUB
               PERFORM P1365-SHIFT-ONE THRU P1365-EXIT
                   VARYING WS-SHIFT-SUB FROM WS-OVR-CNT BY -1
                   UNTIL WS-SHIFT-SUB <= WS-BS-LANDED-SUB.
       P1360-EXIT.
           EXIT.
       P1365-SHIFT-ONE.
           COMPUTE WS-SHIFT-FROM = WS-SHIFT-SUB - 1.
           MOVE WS-OVR-ENTRY (WS-SHIFT-FROM) TO
               WS-OVR-ENTRY (WS-SHIFT-SUB).
       P1365-EXIT.
           EXIT.
       P1370-STORE-NEW-ENTRY.
           MOVE WS-CF-OCN TO WS-OVR-OCN (WS-BS-LANDED-SUB).
           MOVE WS-CF-BAN TO WS-OVR-BAN (WS-BS-LANDED-SUB).
           MOVE WS-CF-ELEM TO WS-OVR-ELEM (WS-BS-LANDED-SUB).
           MOVE WS-CF-JURIS TO WS-OVR-JURIS (WS-BS-LANDED-SUB).
           MOVE WS-CF-STATE TO WS-OVR-STATE (WS-BS-LANDED-SUB).
           MOVE WS-CF-RATE TO WS-OVR-RATE (WS-BS-LANDED-SUB).
           MOVE WS-CF-EFF-FROM TO WS-OVR-EFF-FROM (WS-BS-LANDED-SUB).
           MOVE WS-CF-EFF-THRU TO WS-OVR-EFF-THRU (WS-BS-LANDED-SUB).
           MOVE WS-CF-AUTH-REF TO WS-OVR-AUTH-REF (WS-BS-LANDED-SUB).
           MOVE WS-CF-OVR-TYPE TO WS-OVR-TYPE (WS-BS-LANDED-SUB).
           MOVE WS-CF-MODULE-NAME TO
               WS-OVR-MODULE-NAME (WS-BS-LANDED-SUB).
           PERFORM P1375-SET-PRIORITY THRU P1375-EXIT.
       P1370-EXIT.
           EXIT.
      * P1375 - R1-OVERRIDE-PRIORITY LOGIC: DISPUTE-HOLD BEATS         *
      * REGULATORY BEATS CONTRACT BEATS PROMOTIONAL (LOWER NUMBER      *
      * WINS).  THE RUN-LEVEL CEILING IS APPLIED LATER, IN P4130.      *
       P1375-SET-PRIORITY.
           MOVE 9 TO WS-OVR-PRIORITY (WS-BS-LANDED-SUB).
           IF WS-CF-OVR-TYPE = 'D'
               MOVE 1 TO WS-OVR-PRIORITY (WS-BS-LANDED-SUB).
           IF WS-CF-OVR-TYPE = 'R'
               MOVE 2 TO WS-OVR-PRIORITY (WS-BS-LANDED-SUB).
           IF WS-CF-OVR-TYPE = 'C'
               MOVE 3 TO WS-OVR-PRIORITY (WS-BS-LANDED-SUB).
           IF WS-CF-OVR-TYPE = 'P'
               MOVE 4 TO WS-OVR-PRIORITY (WS-BS-LANDED-SUB).
       P1375-EXIT.
           EXIT.
       P1700-INIT-COUNTERS.
           MOVE 0 TO WS-READ-CNT WS-WRITE-CNT WS-REJECT-CNT
               WS-SUMM-CNT WS-CFWD-CNT.
           MOVE 0 TO WS-ACC-MINUTES WS-ACC-AMOUNT WS-ACC-SEQ-HASH
               WS-ACC-OCN-HASH.
           MOVE 0 TO WS-MC-OVERRIDES-APPLIED WS-MC-DYNAMIC-CALLS
               WS-MC-PASSTHRU-CNT WS-MC-BLOCKED-BY-PRIORITY.
       P1700-EXIT.
           EXIT.
      * S200-MAIN-PROCESS SECTION - READ-AHEAD LOOP.                   *
       S200-MAIN-PROCESS SECTION.
       P2000-PROCESS.
           PERFORM P2200-PROCESS-RECORD THRU P2200-EXIT.
           PERFORM P2100-READ-RATIN THRU P2100-EXIT.
       P2000-EXIT.
           EXIT.
       P2100-READ-RATIN.
           READ RATIN
               AT END MOVE 'Y' TO WS-EOF-SW.
           IF NOT WS-EOF
               ADD 1 TO WS-READ-CNT
               CALL 'CABHASH' USING RI-OCN WS-ACC-OCN-HASH
                   ON EXCEPTION MOVE 9999 TO WS-RC-HASH
                   NOT ON EXCEPTION MOVE 0 TO WS-RC-HASH
               ADD RI-QTY TO WS-ACC-MINUTES
               ADD RI-AMOUNT TO WS-ACC-AMOUNT
               ADD RI-SEQ-NBR TO WS-ACC-SEQ-HASH.
       P2100-EXIT.
           EXIT.
       P2200-PROCESS-RECORD.
           MOVE RI-OCN TO WS-SK-OCN.
           MOVE RI-BAN TO WS-SK-BAN.
           MOVE RI-RATE-ELEM TO WS-SK-ELEM.
           MOVE RI-JURIS-CD TO WS-SK-JURIS.
           MOVE RI-STATE-CD TO WS-SK-STATE.
           PERFORM P4000-SEARCH-OVERRIDE THRU P4000-EXIT.
           IF WS-MR-FOUND
               PERFORM P3000-APPLY-OVERRIDE THRU P3000-EXIT
           ELSE
               PERFORM P7000-WRITE-PASSTHROUGH THRU P7000-EXIT.
       P2200-EXIT.
           EXIT.
      * S300-OVERRIDE-PROCESSING SECTION - BINARY SEARCH FOR ANY       *
      * ENTRY MATCHING THE CURRENT KEY, THEN A BOUNDED SCAN OF ITS     *
      * NEIGHBOURS (SAME-KEY ENTRIES SIT ADJACENT SINCE THE TABLE      *
      * IS SORTED ON KEY ONLY) FOR THE BEST PRIORITY MATCH.            *
       S300-OVERRIDE-PROCESSING SECTION.
       P4000-SEARCH-OVERRIDE.
           MOVE 'N' TO WS-BS-FOUND-SW.
           MOVE 'N' TO WS-MR-FOUND-SW.
           MOVE 0 TO WS-BS-LANDED-SUB.
           IF WS-OVR-CNT > 0
               MOVE 1 TO WS-BS-LOW
               MOVE WS-OVR-CNT TO WS-BS-HIGH
               PERFORM P4050-TEST-MIDPOINT THRU P4050-EXIT
                   UNTIL WS-BS-LOW > WS-BS-HIGH OR WS-BS-FOUND.
           IF WS-BS-FOUND
               PERFORM P4100-SCAN-NEIGHBOURS THRU P4100-EXIT.
       P4000-EXIT.
           EXIT.
       P4050-TEST-MIDPOINT.
           COMPUTE WS-BS-MID = (WS-BS-LOW + WS-BS-HIGH) / 2.
           SET WS-OVR-X TO WS-BS-MID.
           IF WS-OVR-KEY-VIEW (WS-OVR-X) = WS-SK-FLAT-VIEW
               MOVE 'Y' TO WS-BS-FOUND-SW
               MOVE WS-BS-MID TO WS-BS-LANDED-SUB
           ELSE
               IF WS-OVR-KEY-VIEW (WS-OVR-X) > WS-SK-FLAT-VIEW
                   COMPUTE WS-BS-HIGH = WS-BS-MID - 1
           ELSE
                   COMPUTE WS-BS-LOW = WS-BS-MID + 1.
       P4050-EXIT.
           EXIT.
       P4100-SCAN-NEIGHBOURS.
           MOVE 9 TO WS-MR-BEST-PRIORITY.
           MOVE 0 TO WS-MR-BEST-SUB.
           MOVE WS-BS-LANDED-SUB TO WS-SCAN-SUB.
           PERFORM P4110-EVAL-AND-STEP-BACK THRU P4110-EXIT
               UNTIL WS-SCAN-SUB < 1 OR
                   WS-OVR-KEY-VIEW (WS-SCAN-SUB) NOT = WS-SK-FLAT-VIEW.
           COMPUTE WS-SCAN-SUB = WS-BS-LANDED-SUB + 1.
           PERFORM P4120-EVAL-AND-STEP-FWD THRU P4120-EXIT
               UNTIL WS-SCAN-SUB > WS-OVR-CNT OR
                   WS-OVR-KEY-VIEW (WS-SCAN-SUB) NOT = WS-SK-FLAT-VIEW.
           IF WS-MR-BEST-SUB > 0
               MOVE 'Y' TO WS-MR-FOUND-SW.
       P4100-EXIT.
           EXIT.
       P4110-EVAL-AND-STEP-BACK.
           PERFORM P4130-ASSESS-CANDIDATE THRU P4130-EXIT.
           COMPUTE WS-SCAN-SUB = WS-SCAN-SUB - 1.
       P4110-EXIT.
           EXIT.
       P4120-EVAL-AND-STEP-FWD.
           PERFORM P4130-ASSESS-CANDIDATE THRU P4130-EXIT.
           COMPUTE WS-SCAN-SUB = WS-SCAN-SUB + 1.
       P4120-EXIT.
           EXIT.
      * P4130 - TESTS EFFECTIVE-DATE RANGE AND THE R1-OVERRIDE-        *
      * PRIORITY RUN-LEVEL CEILING; KEEPS THE LOWEST-NUMBERED          *
      * (HIGHEST ACTUAL) PRIORITY SEEN SO FAR.                         *
       P4130-ASSESS-CANDIDATE.
           SET WS-OVR-X TO WS-SCAN-SUB.
           MOVE 'N' TO WS-CAND-OK-SW.
           IF R1-CYCLE-YYDDD NOT < WS-OVR-EFF-FROM (WS-OVR-X) AND
                   R1-CYCLE-YYDDD NOT > WS-OVR-EFF-THRU (WS-OVR-X) AND
                   WS-OVR-PRIORITY (WS-OVR-X) NOT > R1-OVERRIDE-PRIORITY
               MOVE 'Y' TO WS-CAND-OK-SW.
           IF WS-CAND-OK-SW = 'N' AND
                   WS-OVR-PRIORITY (WS-OVR-X) > R1-OVERRIDE-PRIORITY
               ADD 1 TO WS-MC-BLOCKED-BY-PRIORITY.
           IF WS-CAND-OK-SW = 'Y' AND
                   WS-OVR-PRIORITY (WS-OVR-X) < WS-MR-BEST-PRIORITY
               MOVE WS-OVR-PRIORITY (WS-OVR-X) TO WS-MR-BEST-PRIORITY
               MOVE WS-SCAN-SUB TO WS-MR-BEST-SUB.
       P4130-EXIT.
           EXIT.
      * P3000-APPLY-OVERRIDE - RE-RATES USING EITHER THE STANDARD      *
      * QTY TIMES OVERRIDE-RATE COMPUTATION OR A CUSTOM DYNAMIC        *
      * MODULE WHEN ONE IS NAMED ON THE OVERRIDE CARD.                 *
       P3000-APPLY-OVERRIDE.
           SET WS-OVR-X TO WS-MR-BEST-SUB.
           MOVE 'N' TO WS-DC-USED-SW.
           IF WS-OVR-MODULE-NAME (WS-OVR-X) NOT = SPACES
               PERFORM P3400-INVOKE-OVERRIDE-PGM THRU P3400-EXIT
           ELSE
               COMPUTE WS-DC-COMPUTED-AMT ROUNDED =
                   RI-QTY * WS-OVR-RATE (WS-OVR-X).
           PERFORM P3300-BUILD-AUDIT-KEY THRU P3300-EXIT.
           PERFORM P7100-WRITE-OVERRIDE-REC THRU P7100-EXIT.
           ADD 1 TO WS-MC-OVERRIDES-APPLIED.
       P3000-EXIT.
           EXIT.
      * P3400 - CABS-STD-002: DYNAMIC CALL.  WS-OVR-MODULE-NAME        *
      * COMES FROM THE OVERRIDE CARD, NOT KNOWABLE STATICALLY.  A      *
      * MODULE THAT NO LONGER EXISTS AT RUN TIME FALLS BACK TO THE     *
      * STANDARD COMPUTATION RATHER THAN ABENDING THE RUN.             *
       P3400-INVOKE-OVERRIDE-PGM.
           MOVE WS-OVR-MODULE-NAME (WS-OVR-X) TO WS-DC-TARGET.
           MOVE 'Y' TO WS-DC-USED-SW.
           ADD 1 TO WS-MC-DYNAMIC-CALLS.
           CALL WS-DC-TARGET USING RI-QTY WS-OVR-RATE (WS-OVR-X)
               WS-DC-COMPUTED-AMT WS-DC-RC
               ON EXCEPTION
                   MOVE 9999 TO WS-DC-RC
                   COMPUTE WS-DC-COMPUTED-AMT ROUNDED =
                       RI-QTY * WS-OVR-RATE (WS-OVR-X)
               NOT ON EXCEPTION
                   MOVE 0 TO WS-DC-RC.
       P3400-EXIT.
           EXIT.
      * P3300 - CABS-STD-020: STRING REBUILDS THE AUDIT REFERENCE     **
      * KEY FROM 5 FRAGMENTS - TYPE, SEPARATOR, THE CARD AUTH-REF,     *
      * SEPARATOR, THE CDR SEQUENCE NUMBER.                            *
       P3300-BUILD-AUDIT-KEY.
           MOVE WS-OVR-TYPE (WS-OVR-X) TO WS-AK-FRAG1.
           MOVE '-' TO WS-AK-FRAG2.
           MOVE WS-OVR-AUTH-REF (WS-OVR-X) TO WS-AK-FRAG3.
           MOVE '-' TO WS-AK-FRAG4.
           MOVE RI-SEQ-NBR TO WS-AK-SEQ-TEXT.
           MOVE WS-AK-SEQ-TEXT TO WS-AK-FRAG5.
           STRING WS-AK-FRAG1 DELIMITED BY SIZE
                   WS-AK-FRAG2 DELIMITED BY SIZE
                   WS-AK-FRAG3 DELIMITED BY SIZE
                   WS-AK-FRAG4 DELIMITED BY SIZE
                   WS-AK-FRAG5 DELIMITED BY SIZE
               INTO WS-AK-BUILT.
       P3300-EXIT.
           EXIT.
      * P7000/P7100 - OUTPUT.  BOTH COUNT 1:1 AGAINST WS-READ-CNT,     *
      * SO CT-READ = CT-WRITTEN HOLDS FOR THIS PROGRAM.                *
       P7000-WRITE-PASSTHROUGH.
           MOVE SPACES TO CABS-OVROUT-RECORD.
           MOVE RI-OCN TO OV-OCN.
           MOVE RI-BAN TO OV-BAN.
           MOVE RI-BILL-PERIOD TO OV-BILL-PERIOD.
           MOVE RI-SECTION TO OV-SECTION.
           MOVE RI-SEQ-NBR TO OV-SEQ-NBR.
           MOVE RI-CIRCUIT-ID TO OV-CIRCUIT-ID.
           MOVE RI-JURIS-CD TO OV-JURIS-CD.
           MOVE RI-STATE-CD TO OV-STATE-CD.
           MOVE RI-RATE-ELEM TO OV-RATE-ELEM.
           MOVE RI-QTY TO OV-QTY.
           MOVE RI-RATE TO OV-RATE.
           MOVE RI-AMOUNT TO OV-AMOUNT.
           MOVE RI-ROUND-RULE TO OV-ROUND-RULE.
           MOVE RI-SRC-PROCESS TO OV-SRC-PROCESS.
           MOVE WS-LT-DETAIL TO OV-LINE-TYPE.
           MOVE RI-DESCRIPTION TO OV-DESCRIPTION.
           MOVE RI-AUTH-REF TO OV-AUTH-REF.
           WRITE CABS-OVROUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
           ADD 1 TO WS-MC-PASSTHRU-CNT.
       P7000-EXIT.
           EXIT.
       P7100-WRITE-OVERRIDE-REC.
           MOVE SPACES TO CABS-OVROUT-RECORD.
           MOVE RI-OCN TO OV-OCN.
           MOVE RI-BAN TO OV-BAN.
           MOVE RI-BILL-PERIOD TO OV-BILL-PERIOD.
           MOVE RI-SECTION TO OV-SECTION.
           MOVE RI-SEQ-NBR TO OV-SEQ-NBR.
           MOVE RI-CIRCUIT-ID TO OV-CIRCUIT-ID.
           MOVE RI-JURIS-CD TO OV-JURIS-CD.
           MOVE RI-STATE-CD TO OV-STATE-CD.
           MOVE RI-RATE-ELEM TO OV-RATE-ELEM.
           MOVE RI-QTY TO OV-QTY.
           MOVE WS-OVR-RATE (WS-OVR-X) TO OV-RATE.
           MOVE WS-DC-COMPUTED-AMT TO OV-AMOUNT.
           MOVE RI-ROUND-RULE TO OV-ROUND-RULE.
           MOVE WS-PGM-NAME TO OV-SRC-PROCESS.
           MOVE WS-LT-OVERRIDE TO OV-LINE-TYPE.
           MOVE RI-DESCRIPTION TO OV-DESCRIPTION.
           MOVE WS-AK-BUILT TO OV-AUTH-REF.
           WRITE CABS-OVROUT-RECORD.
           ADD 1 TO WS-WRITE-CNT.
       P7100-EXIT.
           EXIT.
      * S800-CONTROL-BALANCE SECTION.                                  *
       S800-CONTROL-BALANCE SECTION.
       P8000-CONTROL.
           PERFORM P8100-BUILD-REPORT THRU P8100-EXIT.
           PERFORM P8400-BUILD-CONTROL-REC THRU P8400-EXIT.
           PERFORM P8500-CHECK-BALANCE THRU P8500-EXIT.
           PERFORM P8600-WRITE-CONTROL-REC THRU P8600-EXIT.
       P8000-EXIT.
           EXIT.
       P8100-BUILD-REPORT.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE '1' TO PC-CC.
           MOVE 'CABRAT08 - RATE OVERRIDE HANDLER' TO PC-TEXT.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE R1-RUN-ID TO PC-TEXT.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'OVERRIDE CARDS READ' TO PC-COL-001-020.
           MOVE WS-MC-CARDS-READ TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'CARDS REJECTED' TO PC-COL-001-020.
           MOVE WS-MC-CARDS-REJECTED TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'OVERRIDE TABLE ENTRIES' TO PC-COL-001-020.
           MOVE WS-OVR-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'RECORDS READ' TO PC-COL-001-020.
           MOVE WS-READ-CNT TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'OVERRIDES APPLIED' TO PC-COL-001-020.
           MOVE WS-MC-OVERRIDES-APPLIED TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'DYNAMIC CALLS' TO PC-COL-001-020.
           MOVE WS-MC-DYNAMIC-CALLS TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
           MOVE SPACES TO CABS-PRINT-LINE.
           MOVE ' ' TO PC-CC.
           MOVE 'BLOCKED BY PRIORITY' TO PC-COL-001-020.
           MOVE WS-MC-BLOCKED-BY-PRIORITY TO PC-COL-021-060.
           WRITE CABS-PRINT-LINE.
       P8100-EXIT.
           EXIT.
       P8400-BUILD-CONTROL-REC.
           MOVE R1-RUN-ID TO CT-RUN-ID.
           MOVE WS-PGM-NAME TO CT-PROCESS-ID.
           MOVE 1 TO CT-STEP-SEQ.
           MOVE R1-CYCLE-YYDDD TO CT-CYCLE-YYDDD.
           MOVE R1-BILL-PERIOD TO CT-BILL-PERIOD.
           MOVE 0 TO CT-RERUN-NBR.
           MOVE SPACES TO CT-JOBNAME.
           MOVE SPACES TO CT-STEPNAME.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE WS-SUMM-CNT TO CT-SUMMARISED.
           MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.
           MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.
           MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.
           MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.
           MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.
           MOVE SPACES TO CT-RESTART-KEY.
           MOVE SPACES TO CT-FILLER.
           MOVE 0 TO CT-RC.
           MOVE SPACES TO CT-ABEND-CD.
       P8400-EXIT.
           EXIT.
       P8500-CHECK-BALANCE.
           IF CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARISED +
               CT-CARRIED-FWD
               MOVE 'B' TO CT-BAL-IND
           ELSE
               MOVE 'O' TO CT-BAL-IND.
       P8500-EXIT.
           EXIT.
       P8600-WRITE-CONTROL-REC.
           MOVE CABS-CONTROL-RECORD TO CABS-CTLOUT-RECORD.
           WRITE CABS-CTLOUT-RECORD.
       P8600-EXIT.
           EXIT.
      ******************************************************************
      * S900-TERMINATION SECTION.                                      *
      ******************************************************************
       S900-TERMINATION SECTION.
       P9000-TERM.
           CLOSE RATIN.
           CLOSE OVRIN.
           CLOSE OVROUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
           DISPLAY 'CABRAT08 - RUN COMPLETE'.
           DISPLAY '  READ       = ' WS-READ-CNT.
           DISPLAY '  WRITTEN    = ' WS-WRITE-CNT.
           DISPLAY '  OVERRIDES  = ' WS-MC-OVERRIDES-APPLIED.
           DISPLAY '  DYN CALLS  = ' WS-MC-DYNAMIC-CALLS.
       P9000-EXIT.
           EXIT.
       P9900-FATAL-OPEN.
           MOVE WS-PGM-NAME TO WS-AB-PGM.
           MOVE 9901 TO WS-AB-USER-CODE.
           DISPLAY 'CABRAT08 FATAL OPEN - ' WS-AB-REASON.
           CALL 'CABABEND' USING WS-AB-PGM WS-AB-PARA WS-AB-REASON
               WS-AB-USER-CODE.
           STOP RUN.
       P9900-EXIT.
           EXIT.
