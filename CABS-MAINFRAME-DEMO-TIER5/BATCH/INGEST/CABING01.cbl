      *****************************************************************
      * CABING01 - RAW EMI-DERIVED ACCESS USAGE EDIT/FORMAT VALIDATE  *
      * APPLICATION : CABS                                            *
      * INPUTS      : DDNAME  DSN                          COPYBOOK   *
      *               RAWIN   TELCABS.CABS.USAGE.RAW(0)    CABSCDR    *
      *               PARMIN  INSTREAM SYSIN                -         *
      * OUTPUTS     : DDNAME  DSN                          COPYBOOK   *
      *               EDTOUT  TELCABS.CABS.USAGE.EDITED(+1) CABSCDR   *
      *               SUSOUT  TELCABS.CABS.USAGE.SUSPENSE   CABSERR   *
      *                       (+1)                                    *
      *               RPTOUT  SYSOUT PRINT                  -         *
      * CONTROL     : CTLOUT  TELCABS.CABS.CONTROL(+1)      CABSCTL   *
      * BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +            *
      *               CT-SUMMARISED + CT-CARRIED-FWD                  *
      *               (CT-SUMMARISED AND CT-CARRIED-FWD ARE ALWAYS    *
      *               ZERO IN THIS PROGRAM.  CT-WRITTEN COUNTS EVERY  *
      *               RECORD STAMPED CLEAN OR SUSPECT (STATUS 0-5)    *
      *               WRITTEN TO EDTOUT.  CT-REJECTED COUNTS FATAL    *
      *               RECORDS (STATUS 6-9) DIVERTED AWAY FROM EDTOUT. *
      *               SUSOUT MAY RECEIVE MORE THAN ONE RECORD PER     *
      *               INPUT RECORD - ONE SUSPENSE RECORD PER FAILED   *
      *               EDIT - AND IS NOT PART OF THE BALANCE EQUATION) *
      * RESTART     : RESTARTABLE FROM CT-RESTART-KEY                 *
      * REVISION HISTORY                                              *
      *   V1.00  1987-04-02  R.T.WHEELER  INITIAL EDIT SUITE - KEY,   *
      *                      RECTYPE AND DATE EDITS ONLY              *
      *   V1.02  1988-09-14  R.T.WHEELER  ADDED VARIANT-AREA EDITS    *
      *                      FOR VOICE MOU RECORDS                    *
      *   V1.05  1990-02-27  D.OKONKWO    ADDED SPECIAL ACCESS EDIT   *
      *                      PATH (USOC / MPB) FOR RDF FEED           *
      *   V1.09  1993-11-03  D.OKONKWO    SUSPENSE FILE WIDENED TO    *
      *                      FB 300 FOR LONGER BUILT MESSAGES         *
      *   V2.00  1996-06-19  J.M.CASTILLO Y2K REMEDIATION - PIVOT     *
      *                      YEAR LOGIC ADDED, DEFAULT PIVOT 70       *
      *   V2.02  1999-01-08  J.M.CASTILLO ADDED CIRCUIT-MASTER LOOKUP *
      *                      VIA CABCIRCL FOR SPECIAL ACCESS RECORDS  *
      *                      (SEE P6400) - VALIDATES CIRCUIT-ID       *
      *   V2.05  2003-08-21  P.NAIR        DATA SERVICE OCTET EDITS   *
      *                      ADDED FOR BROADBAND ACCESS RDF           *
      *   V2.08  2007-05-30  A.BUKOWSKI    HASH TOTAL ACCUMULATION    *
      *                      MOVED TO SHARED CABHASH SUBROUTINE       *
      *   V2.10  2011-10-11  S.MARCHETTI   MPB PERCENT RANGE EDIT     *
      *                      TIGHTENED PER TARIFF FILING 2011-014     *
      *   V2.13  2019-03-06  K.ADEYEMI     RECOMPILE UNDER LE V8,     *
      *                      NO LOGIC CHANGE                          *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CABING01.
       AUTHOR. R-T-WHEELER.
      *****************************************************************
      * SEE HEADER ABOVE FOR FULL REVISION HISTORY.                   *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RAWIN  ASSIGN TO RAWIN
               FILE STATUS IS WS-FS-INPUT.
           SELECT EDTOUT ASSIGN TO EDTOUT
               FILE STATUS IS WS-FS-OUTPUT.
           SELECT SUSOUT ASSIGN TO SUSOUT
               FILE STATUS IS WS-FS-SUSPENSE.
           SELECT CTLOUT ASSIGN TO CTLOUT
               FILE STATUS IS WS-FS-CONTROL.
           SELECT PARMIN ASSIGN TO PARMIN
               FILE STATUS IS WS-FS-PARM.
           SELECT RPTOUT ASSIGN TO RPTOUT
               FILE STATUS IS WS-FS-RPT.
       DATA DIVISION.
       FILE SECTION.
       FD  RAWIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       COPY CABSCDR.
       FD  EDTOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       01  EDT-CDR-OUT-REC             PIC X(200).
       FD  SUSOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 300 CHARACTERS.
       01  SU-SUSPENSE-OUT-REC         PIC X(300).
       FD  CTLOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 180 CHARACTERS.
       01  CT-CONTROL-OUT-REC          PIC X(180).
       FD  PARMIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  PM-PARM-CARD-REC            PIC X(80).
       FD  RPTOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE OMITTED
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       01  RP-REPORT-LINE               PIC X(133).
       WORKING-STORAGE SECTION.
      *****************************************************************
      * STANDARD CABS WORKING STORAGE - SWITCHES, COUNTERS, ACCUMS,   *
      * FILE STATUS, ERROR CODES/SUSPENSE LAYOUT, DATE WORK, CONTROL  *
      * RECORD.  NESTED COPY CHAIN - SEE CABSWRK HEADER.              *
      *****************************************************************
       COPY CABSWRK.
      *****************************************************************
      * WS-PROGRAM-CONSTANTS - LITERALS USED IN SUSPENSE MESSAGES AND *
      * THE ABEND CALL.  KEPT SEPARATE FROM CABSWRK SO A RECOMPILE OF *
      * THIS PROGRAM ALONE DOES NOT FORCE THE OTHER 60-ODD PROGRAMS   *
      * THAT COPY CABSWRK TO RECOMPILE.                                *
      *****************************************************************
       01  WS-PROGRAM-CONSTANTS.
           05  WS-THIS-PROGRAM             PIC X(08) VALUE 'CABING01'.
           05  WS-THIS-RUN-ID               PIC X(12).
           05  WS-ABEND-REASON-CD           PIC X(04) VALUE '0000'.
           05  WS-STEP-SEQ-NBR               PIC 9(03) VALUE 010.
      *****************************************************************
      * WS-ADDL-FILE-STATUS - PARMIN AND RPTOUT ARE NOT PART OF THE   *
      * SHARED CABSWRK FILE STATUS GROUP BECAUSE NOT EVERY PROGRAM    *
      * IN THE ESTATE READS A PARM CARD OR WRITES A REPORT.           *
      *****************************************************************
       01  WS-ADDL-FILE-STATUS.
           05  WS-FS-PARM                   PIC X(02) VALUE '00'.
           05  WS-FS-RPT                    PIC X(02) VALUE '00'.
      *****************************************************************
      * WS-PARM-CARD-WORK - LAYOUT OF THE INSTREAM SYSIN CONTROL      *
      * CARD.  FREE-FORM, SLASH DELIMITED:                            *
      *   RUNID/CYCLE-YYDDD/RERUN-NBR/RESTART-KEY                     *
      * E.G.  CABI0815/26227/00/                                      *
      *****************************************************************
       01  WS-PARM-CARD-WORK.
           05  WS-PARM-RUN-ID               PIC X(12).
           05  WS-PARM-CYCLE-YYDDD           PIC 9(05).
           05  WS-PARM-RERUN-NBR             PIC 9(02).
           05  WS-PARM-RESTART-KEY           PIC X(26).
           05  WS-PARM-SPARE                 PIC X(20).
       01  WS-UNSTRING-PARM-WORK.
           05  WS-UNSTR-PTR                  PIC S9(03) COMP-3.
           05  WS-UNSTR-TALLY                PIC S9(03) COMP-3.
           05  WS-UNSTR-FIELD-CNT            PIC S9(03) COMP-3.
      *****************************************************************
      * WS-RUN-DATE-WORK - THE PROCESSING DATE FOR THIS RUN, DERIVED  *
      * FROM THE PARM CARD CYCLE FIELD AND, IF NOT SUPPLIED, FROM     *
      * CABDTCNV USING THE SYSTEM DATE.                                *
      *****************************************************************
       01  WS-RUN-DATE-WORK.
           05  WS-RUN-YYDDD                  PIC 9(05).
           05  WS-RUN-YY  REDEFINES WS-RUN-YYDDD.
               10  WS-RUN-YY-ONLY             PIC 9(02).
               10  WS-RUN-DDD-ONLY             PIC 9(03).
           05  WS-RUN-CCYY                   PIC 9(04).
           05  WS-RUN-DATE-SRC-SW            PIC X(01) VALUE 'P'.
               88  WS-RUN-DATE-FROM-PARM      VALUE 'P'.
               88  WS-RUN-DATE-FROM-SYS       VALUE 'S'.
      *****************************************************************
      * WS-CALL-PARM-AREA - LINKAGE-STYLE PARAMETERS PASSED TO THE    *
      * STATIC SUBPROGRAMS.  ONE AREA PER SUBPROGRAM, EACH ITS OWN    *
      * 01-LEVEL SO A CHANGE TO ONE SUBPROGRAM'S CALL INTERFACE DOES  *
      * NOT DISTURB THE OTHERS.                                        *
      *****************************************************************
       01  WS-CABDTCNV-PARMS.
           05  CV-FUNCTION-CD                PIC X(01) VALUE '1'.
               88  CV-CONVERT-TO-GREG         VALUE '1'.
               88  CV-CONVERT-TO-JULIAN       VALUE '2'.
           05  CV-YYDDD-IN                    PIC 9(05).
           05  CV-PIVOT-YY                    PIC 9(02).
           05  CV-CCYYMMDD-OUT                PIC 9(08).
           05  CV-CCYYMMDD-SPLIT REDEFINES CV-CCYYMMDD-OUT.
               10  CV-OUT-CCYY                  PIC 9(04).
               10  CV-OUT-MM                    PIC 9(02).
               10  CV-OUT-DD                    PIC 9(02).
           05  CV-RETURN-CD                   PIC X(02).
       01  WS-CABOCNVL-PARMS.
           05  OV-OCN-IN                      PIC X(04).
           05  OV-EFF-YYDDD-IN                PIC 9(05).
           05  OV-FOUND-SW                    PIC X(01).
               88  OV-OCN-FOUND                VALUE 'Y'.
               88  OV-OCN-NOT-FOUND             VALUE 'N'.
           05  OV-EFFECTIVE-SW                PIC X(01).
               88  OV-OCN-EFFECTIVE             VALUE 'Y'.
       01  WS-CABHASH-PARMS.
           05  HS-FIELD-TYPE-CD               PIC X(01).
               88  HS-TYPE-MINUTES              VALUE '1'.
               88  HS-TYPE-AMOUNT                VALUE '2'.
               88  HS-TYPE-SEQ                    VALUE '3'.
               88  HS-TYPE-OCN                     VALUE '4'.
           05  HS-VALUE-IN                    PIC S9(15)V9(05) COMP-3.
           05  HS-ACCUM-INOUT                 PIC S9(17)V9(05) COMP-3.
       01  WS-CABPARMR-PARMS.
           05  PR-CARD-IN                     PIC X(80).
           05  PR-VALID-SW                    PIC X(01).
               88  PR-CARD-VALID                 VALUE 'Y'.
               88  PR-CARD-INVALID                VALUE 'N'.
       01  WS-CABABEND-PARMS.
           05  AB-PROGRAM-ID                  PIC X(08).
           05  AB-ABEND-CD                    PIC X(04).
           05  AB-RUN-ID                      PIC X(12).
      *****************************************************************
      * WS-EDIT-TABLE - LOADED BY P1400 FROM LITERALS (NO EXTERNAL    *
      * TABLE FILE FOR THIS PASS).  DRIVES THE SEVERITY-TO-STATUS     *
      * MAP USED WHEN CD-EDIT-STATUS IS FINALLY STAMPED.               *
      *****************************************************************
       01  WS-EDIT-TABLE.
           05  WS-EDIT-TABLE-ENTRY OCCURS 10 TIMES
                   INDEXED BY WS-ET-IDX.
               10  WS-ET-ERR-CODE              PIC X(04).
               10  WS-ET-SEVERITY               PIC X(01).
               10  WS-ET-STATUS-STAMP            PIC X(01).
       01  WS-EDIT-TABLE-CTL.
           05  WS-ET-LOAD-CNT                 PIC S9(03) COMP-3.
           05  WS-ET-SEARCH-SUB                PIC S9(03) COMP-3.
      *****************************************************************
      * WS-ET-FAIL-COUNTS - PARALLEL ARRAY TO WS-EDIT-TABLE, SAME 10  *
      * SLOTS IN THE SAME ORDER.  INCREMENTED IN P7200 WHEN A         *
      * MATCHING ERROR CODE IS FOUND, PRINTED BY P8260 AT END OF RUN. *
      * A CODE THAT NEVER MATCHES (E.G. EC-CIRCUIT-UNKNOWN - NOTHING  *
      * IN THIS PROGRAM ACTUALLY LOOKS UP A CIRCUIT) STAYS ZERO.      *
      *****************************************************************
       01  WS-ET-FAIL-COUNTS.
           05  WS-ET-FAIL-CNT OCCURS 10 TIMES PIC S9(07) COMP-3.
      *****************************************************************
      * WS-EDIT-RECORD-WORK - WORKING MIRROR OF THE KEY AND CONTROL   *
      * PORTION OF THE RAW RECORD.  LOADED AT THE TOP OF P2200 AND    *
      * REFERENCED BY THE KEY-FAMILY EDIT PARAGRAPHS.  WS-BILL-KEY    *
      * BELOW RENAMES ACROSS WS-EDT-OCN THRU WS-EDT-BAN - THE EDIT    *
      * PARAGRAPHS USE WS-BILL-KEY WHEN BUILDING SUSPENSE TEXT: THE   *
      * REAL ELEMENTARY NAMES ARE WS-EDT-OCN AND WS-EDT-BAN.           *
      *****************************************************************
       01  WS-EDIT-RECORD-WORK.
           05  WS-EDT-OCN                     PIC X(04).
           05  WS-EDT-BAN                     PIC X(13).
           05  WS-EDT-SEQ                     PIC 9(09).
           05  WS-EDT-SEQ-ALPHA REDEFINES WS-EDT-SEQ
                                              PIC X(09).
           05  WS-EDT-REC-TYPE                PIC X(02).
           05  WS-EDT-USAGE-TYPE              PIC X(01).
           05  WS-EDT-JURIS-CD                PIC X(01).
           05  WS-EDT-RATE-ELEM               PIC X(06).
           05  WS-EDT-CONN-YY                 PIC 9(02).
           05  WS-EDT-CONN-DDD                PIC 9(03).
           05  WS-EDT-DISC-YYDDD              PIC 9(05).
      *****************************************************************
      * 66-LEVEL RENAMES - LOGIC ELSEWHERE IN THIS PROGRAM REFERS TO  *
      * WS-BILL-KEY.  THE ACTUAL ELEMENTARY ITEMS ARE WS-EDT-OCN AND  *
      * WS-EDT-BAN (SEE ABOVE).  DO NOT RENAME THE ELEMENTARY ITEMS - *
      * SEE CABS-STD-002.                                              *
      *****************************************************************
           66  WS-BILL-KEY  RENAMES WS-EDT-OCN THRU WS-EDT-BAN.
       01  WS-EDT-SEVERITY-WORK.
      *****************************************************************
      * WS-EDT-SEVERITY - THE 88-LEVELS BELOW OVERLAP ON VALUE '3'.   *
      * WS-SEV-WARN COVERS '1' THRU '3', WS-SEV-HARD COVERS '3' THRU  *
      * '9'.  A VALUE OF '3' SATISFIES BOTH.  WHICHEVER CONDITION IS  *
      * TESTED FIRST IN THE PROCEDURE DIVISION DECIDES THE OUTCOME -  *
      * SEE P2200.                                                     *
      *****************************************************************
           05  WS-EDT-SEVERITY                PIC X(01) VALUE '0'.
               88  WS-SEV-NONE                   VALUE '0'.
               88  WS-SEV-WARN                   VALUE '1' THRU '3'.
               88  WS-SEV-HARD                   VALUE '3' THRU '9'.
           05  WS-EDT-STATUS                  PIC X(01) VALUE '0'.
      *****************************************************************
      * WS-KEY-EDIT-WORK / WS-OCN-FMT-WORK / WS-BAN-FMT-WORK /        *
      * WS-SEQ-EDIT-WORK - ONE WORK AREA PER S300 EDIT PARAGRAPH.     *
      *****************************************************************
       01  WS-KEY-EDIT-WORK.
           05  WS-KEY-SCAN-AREA               PIC X(17).
           05  WS-KEY-BYTE-TABLE REDEFINES WS-KEY-SCAN-AREA.
               10  WS-KEY-BYTE OCCURS 17 TIMES
                       PIC X(01).
           05  WS-KEY-SUB                     PIC S9(03) COMP-3.
           05  WS-KEY-BAD-CNT                 PIC S9(03) COMP-3.
       01  WS-OCN-FMT-WORK.
           05  WS-OCN-SCAN-AREA               PIC X(04).
           05  WS-OCN-NUMERIC-SW              PIC X(01).
               88  WS-OCN-ALL-NUMERIC            VALUE 'Y'.
           05  WS-OCN-SPACE-CNT               PIC S9(03) COMP-3.
       01  WS-BAN-FMT-WORK.
           05  WS-BAN-SCAN-AREA               PIC X(13).
           05  WS-BAN-DASH-CNT                PIC S9(03) COMP-3.
           05  WS-BAN-VALID-SW                PIC X(01) VALUE 'Y'.
               88  WS-BAN-FORMAT-OK              VALUE 'Y'.
       01  WS-SEQ-EDIT-WORK.
           05  WS-SEQ-EDIT-AREA               PIC 9(09).
           05  WS-SEQ-EDIT-ALPHA REDEFINES WS-SEQ-EDIT-AREA
                                              PIC X(09).
           05  WS-SEQ-PRIOR                   PIC 9(09) VALUE 0.
           05  WS-SEQ-GAP-SW                  PIC X(01) VALUE 'N'.
               88  WS-SEQ-GAP-DETECTED           VALUE 'Y'.
      *****************************************************************
      * WS-SEQ-RECENT-CACHE - A SMALL RING BUFFER OF THE LAST 20      *
      * SEQUENCE NUMBERS SEEN THIS RUN.  RAW USAGE FEEDS ARE NOT      *
      * SORTED BEFORE THIS PROGRAM RUNS, SO A NEAR-NEIGHBOUR DUPLICATE*
      * (SAME SEQ REPEATED A FEW RECORDS LATER, NOT NECESSARILY       *
      * ADJACENT) IS OTHERWISE INVISIBLE TO A SINGLE-PASS EDIT.  A    *
      * FULL DUPLICATE CHECK STILL HAPPENS DOWNSTREAM AFTER THE SORT. *
      *****************************************************************
       01  WS-SEQ-RECENT-CACHE.
           05  WS-SEQ-CACHE-ENTRY OCCURS 20 TIMES
                   INDEXED BY WS-SC-IDX.
               10  WS-SC-SEQ-VALUE             PIC 9(09) VALUE 0.
       01  WS-SEQ-CACHE-CTL.
           05  WS-SC-NEXT-SLOT                PIC S9(03) COMP-3
                                              VALUE 1.
           05  WS-SC-SEARCH-SUB               PIC S9(03) COMP-3.
           05  WS-SC-DUP-FOUND-SW             PIC X(01) VALUE 'N'.
               88  WS-SC-DUP-FOUND               VALUE 'Y'.
      *****************************************************************
      * S400 WORK AREAS - RECTYPE, USAGE-TYPE, JURISDICTION, RATE     *
      * ELEMENT.  WS-RECTYPE-EDIT-WORK HOLDS A LOCAL COPY OF THE      *
      * NUMERIC EDIT AREA REDEFINED AS ALPHANUMERIC FOR INSPECT USE.  *
      *****************************************************************
       01  WS-RECTYPE-EDIT-WORK.
           05  WS-NUM-EDIT-AREA               PIC 9(09) VALUE 0.
           05  WS-NUM-EDIT-ALPHA REDEFINES WS-NUM-EDIT-AREA
                                              PIC X(09).
           05  WS-RECTYPE-VALID-SW            PIC X(01) VALUE 'Y'.
               88  WS-RECTYPE-OK                 VALUE 'Y'.
       01  WS-USAGE-TYPE-WORK.
           05  WS-UT-SAVE                     PIC X(01).
           05  WS-UT-VALID-SW                 PIC X(01) VALUE 'Y'.
               88  WS-UT-OK                      VALUE 'Y'.
           05  WS-UT-VOICE-VALUES             PIC X(04) VALUE 'VDMF'.
       01  WS-JURIS-EDIT-WORK.
           05  WS-JUR-CLASS                   PIC X(01) VALUE 'X'.
               88  WS-JUR-INTERSTATE             VALUE 'I'.
               88  WS-JUR-INTRASTATE             VALUE 'S'.
               88  WS-JUR-LOCAL                  VALUE 'L'.
               88  WS-JUR-INDETERMINATE          VALUE 'X'.
           05  WS-JUR-BAD-CNT                 PIC S9(03) COMP-3.
       01  WS-RATE-ELEM-WORK.
           05  WS-RE-SCAN-AREA                PIC X(06).
           05  WS-RE-BYTE-TABLE REDEFINES WS-RE-SCAN-AREA.
               10  WS-RE-BYTE OCCURS 6 TIMES
                       PIC X(01).
           05  WS-RE-PREFIX-ONLY              PIC X(02).
           05  WS-RE-BAND-AREA                PIC X(04).
           05  WS-RE-BAND-TABLE REDEFINES WS-RE-BAND-AREA.
               10  WS-RE-BAND-BYTE OCCURS 4 TIMES
                       PIC X(01).
           05  WS-RE-BAND-SUB                 PIC S9(03) COMP-3.
           05  WS-RE-BAND-BAD-CNT             PIC S9(03) COMP-3.
           05  WS-RE-VALID-TABLE.
               10  FILLER PIC X(24) VALUE 'OASATATLTLTTSTCCUNEOSXX'.
           05  WS-RE-VALID-REDEF REDEFINES WS-RE-VALID-TABLE.
               10  WS-RE-VALID-ENTRY OCCURS 12 TIMES
                       PIC X(02).
           05  WS-RE-FOUND-SW                 PIC X(01) VALUE 'N'.
               88  WS-RE-FOUND                   VALUE 'Y'.
           05  WS-RE-SUB                      PIC S9(03) COMP-3.
      *****************************************************************
      * S500 WORK AREAS - DATE EDITS.  WS-DATE-EDIT-WORK CARRIES THE  *
      * DIGIT-BY-DIGIT SCAN AREA (REDEFINES) USED BY P5000 TO CONFIRM *
      * A YYDDD FIELD IS FULLY NUMERIC BEFORE ANY DATE MATH IS DONE   *
      * AGAINST IT - CABDTCNV IS NOT FORGIVING OF NON-NUMERIC INPUT.  *
      *****************************************************************
       01  WS-DATE-EDIT-WORK.
           05  WS-DATE-SCAN-AREA              PIC X(05).
           05  WS-DATE-DIGIT-TABLE REDEFINES WS-DATE-SCAN-AREA.
               10  WS-DATE-DIGIT OCCURS 5 TIMES
                       PIC X(01).
           05  WS-DATE-SUB                    PIC S9(03) COMP-3.
           05  WS-DATE-BAD-SW                 PIC X(01) VALUE 'N'.
               88  WS-DATE-INVALID               VALUE 'Y'.
       01  WS-CONN-DATE-WORK.
           05  WS-CONN-YYDDD-GRP.
               10  WS-CONN-YY                  PIC 9(02).
               10  WS-CONN-DDD                 PIC 9(03).
           05  WS-CONN-YYDDD REDEFINES WS-CONN-YYDDD-GRP
                                              PIC 9(05).
           05  WS-CONN-GREG                   PIC 9(08).
           05  WS-CONN-VALID-SW               PIC X(01) VALUE 'Y'.
               88  WS-CONN-DATE-OK               VALUE 'Y'.
       01  WS-DISC-DATE-WORK.
           05  WS-DISC-YYDDD                  PIC 9(05).
           05  WS-DISC-GREG                   PIC 9(08).
           05  WS-DISC-VALID-SW               PIC X(01) VALUE 'Y'.
               88  WS-DISC-DATE-OK               VALUE 'Y'.
           05  WS-DISC-BEFORE-CONN-SW         PIC X(01) VALUE 'N'.
               88  WS-DISC-BEFORE-CONN           VALUE 'Y'.
       01  WS-PIVOT-WORK.
           05  WS-PIVOT-CENTURY-CC            PIC 9(02).
           05  WS-PIVOT-COMPARE-YY            PIC 9(02).
       01  WS-DURATION-WORK.
           05  WS-DUR-CONN-SECS               PIC S9(09) COMP-3.
           05  WS-DUR-DISC-SECS               PIC S9(09) COMP-3.
           05  WS-DUR-ELAPSED-SECS            PIC S9(09) COMP-3.
           05  WS-DUR-ELAPSED-MIN             PIC S9(09) COMP-3.
           05  WS-DUR-CHG-MIN-WHOLE           PIC S9(09) COMP-3.
           05  WS-DUR-VARIANCE-MIN            PIC S9(09) COMP-3.
           05  WS-DUR-MAX-SECS                PIC S9(09) COMP-3
                                              VALUE 86400.
           05  WS-DUR-VARIANCE-LIMIT          PIC S9(09) COMP-3
                                              VALUE 5.
           05  WS-DUR-NEGATIVE-SW             PIC X(01) VALUE 'N'.
               88  WS-DUR-IS-NEGATIVE            VALUE 'Y'.
           05  WS-DUR-EXCESSIVE-SW            PIC X(01) VALUE 'N'.
               88  WS-DUR-IS-EXCESSIVE           VALUE 'Y'.
           05  WS-DUR-VARIANCE-SW             PIC X(01) VALUE 'N'.
               88  WS-DUR-VARIANCE-HIGH          VALUE 'Y'.
      *****************************************************************
      * WS-DUR-TIME-WORK - HHMMSS TIMESTAMPS ARE FLAT PIC 9(06)       *
      * FIELDS IN THE FROZEN CABSCDR COPYBOOK.  THIS REDEFINES SPLITS *
      * A WORKING COPY INTO HH/MM/SS SO ELAPSED SECONDS CAN BE        *
      * COMPUTED WITHOUT REFERENCE MODIFICATION AGAINST THE ORIGINAL. *
      *****************************************************************
       01  WS-DUR-TIME-WORK.
           05  WS-DUR-CONN-HHMMSS             PIC 9(06).
           05  WS-DUR-CONN-SPLIT REDEFINES WS-DUR-CONN-HHMMSS.
               10  WS-DUR-CONN-HH               PIC 9(02).
               10  WS-DUR-CONN-MM               PIC 9(02).
               10  WS-DUR-CONN-SS               PIC 9(02).
           05  WS-DUR-DISC-HHMMSS             PIC 9(06).
           05  WS-DUR-DISC-SPLIT REDEFINES WS-DUR-DISC-HHMMSS.
               10  WS-DUR-DISC-HH                PIC 9(02).
               10  WS-DUR-DISC-MM                PIC 9(02).
               10  WS-DUR-DISC-SS                PIC 9(02).
      *****************************************************************
      * S600 WORK AREAS - VARIANT AREA EDITS (VOICE / DATA / SPCL)    *
      * AND THE AUDIT AREA EDIT.  WS-NPA-NXX-COMPOSITE-WORK IS A      *
      * REDEFINES SPLITTING THE 6-DIGIT ORIGINATING NPA-NXX INTO ITS  *
      * TWO 3-DIGIT COMPONENTS FOR SEPARATE RANGE CHECKS.              *
      *****************************************************************
       01  WS-VARIANT-EDIT-WORK.
           05  WS-VAR-REC-TYPE-SAVE           PIC X(02).
           05  WS-VAR-EDIT-SW                 PIC X(01) VALUE 'N'.
               88  WS-VAR-EDIT-FAILED            VALUE 'Y'.
       01  WS-VOICE-VARIANT-WORK.
           05  WS-VC-NPANXX-WORK              PIC 9(06).
           05  WS-VC-NPANXX-SPLIT REDEFINES WS-VC-NPANXX-WORK.
               10  WS-VC-NPA                   PIC 9(03).
               10  WS-VC-NXX                   PIC 9(03).
           05  WS-VC-MIN-VALID-SW             PIC X(01) VALUE 'Y'.
               88  WS-VC-MIN-OK                  VALUE 'Y'.
           05  WS-VC-TANDEM-VALID-SW          PIC X(01) VALUE 'Y'.
               88  WS-VC-TANDEM-OK               VALUE 'Y'.
       01  WS-DATA-VARIANT-WORK.
           05  WS-DT-OCTET-TOTAL              PIC S9(16) COMP-3.
           05  WS-DT-OCTET-MAX                PIC S9(16) COMP-3
                                              VALUE 999999999999999.
           05  WS-DT-OVERFLOW-SW              PIC X(01) VALUE 'N'.
               88  WS-DT-OCTET-OVERFLOW          VALUE 'Y'.
           05  WS-DT-BANDWIDTH-VALID-SW       PIC X(01) VALUE 'Y'.
               88  WS-DT-BANDWIDTH-OK            VALUE 'Y'.
       01  WS-SPCL-VARIANT-WORK.
           05  WS-SP-USOC-VALID-SW            PIC X(01) VALUE 'Y'.
               88  WS-SP-USOC-OK                 VALUE 'Y'.
           05  WS-SP-MPB-PCT-SAVE             PIC S9(03)V9(05)
                                              COMP-3.
           05  WS-SP-MPB-LOW-LIMIT            PIC S9(03)V9(05)
                                              COMP-3 VALUE 0.
           05  WS-SP-MPB-HIGH-LIMIT           PIC S9(03)V9(05)
                                              COMP-3 VALUE 100.00000.
           05  WS-SP-QTY-VALID-SW             PIC X(01) VALUE 'Y'.
               88  WS-SP-QTY-OK                  VALUE 'Y'.
       01  WS-AUDIT-AREA-WORK.
           05  WS-AUD-SRC-VALID-SW            PIC X(01) VALUE 'Y'.
               88  WS-AUD-SRC-OK                 VALUE 'Y'.
           05  WS-AUD-LOAD-DATE-VALID-SW      PIC X(01) VALUE 'Y'.
               88  WS-AUD-LOAD-DATE-OK           VALUE 'Y'.
           05  WS-AUD-PASS-CNT                PIC S9(03) COMP-3
                                              VALUE 0.
           05  WS-AUD-SRC-TABLE.
               10  FILLER PIC X(16) VALUE 'EMI1EMI2RDF1MED1'.
           05  WS-AUD-SRC-REDEF REDEFINES WS-AUD-SRC-TABLE.
               10  WS-AUD-SRC-ENTRY OCCURS 4 TIMES
                       PIC X(04).
           05  WS-AUD-SRC-SUB                 PIC S9(03) COMP-3.
           05  WS-AUD-SRC-FOUND-SW            PIC X(01) VALUE 'N'.
               88  WS-AUD-SRC-FOUND              VALUE 'Y'.
      *****************************************************************
      * S700 WORK AREAS - CHARACTER SCANNING, STRING/UNSTRING, HASH   *
      * ACCUMULATION.  WS-SCAN-RAW-AREA IS THE 200-BYTE REDEFINES     *
      * THAT TURNS THE WHOLE RECORD INTO A BYTE TABLE FOR INSPECT AND *
      * SUBSCRIPTED PERFORM VARYING WALKS.  NO REFERENCE MODIFICATION *
      * IS USED ANYWHERE IN THIS PROGRAM - SEE CABS-STD-014.           *
      *****************************************************************
       01  WS-SCAN-NUMERIC-WORK.
           05  WS-SCAN-RAW-AREA               PIC X(200).
           05  WS-SCAN-BYTE-TABLE REDEFINES WS-SCAN-RAW-AREA.
               10  WS-SCAN-BYTE OCCURS 200 TIMES
                       PIC X(01).
           05  WS-SCAN-SUB                    PIC S9(03) COMP-3.
           05  WS-SCAN-NON-NUMERIC-CNT        PIC S9(03) COMP-3.
       01  WS-SCAN-SPACES-WORK.
           05  WS-EMBEDDED-SPACE-CNT          PIC S9(05) COMP-3.
           05  WS-LOW-VALUE-CNT               PIC S9(05) COMP-3.
           05  WS-REPLACE-CNT                 PIC S9(05) COMP-3.
       01  WS-ERR-MSG-BUILD-WORK.
           05  WS-EMB-PARA-NAME               PIC X(30).
           05  WS-EMB-ERR-CODE                PIC X(04).
           05  WS-EMB-POINTER                 PIC S9(03) COMP-3.
           05  WS-EMB-MSG-AREA                PIC X(200).
       01  WS-SUSPENSE-BUILD-WORK.
           05  WS-SUS-SEVERITY-CHAR           PIC X(01).
           05  WS-SUS-COUNT-THIS-REC          PIC S9(03) COMP-3.
           05  WS-SUS-TOTAL-CNT               PIC S9(09) COMP-3
                                              VALUE 0.
       01  WS-HASH-ACCUM-WORK.
           05  WS-HASH-MIN-ACCUM              PIC S9(17)V9(02)
                                              COMP-3 VALUE 0.
           05  WS-HASH-AMT-ACCUM              PIC S9(17)V9(05)
                                              COMP-3 VALUE 0.
           05  WS-HASH-SEQ-ACCUM              PIC S9(17)
                                              COMP-3 VALUE 0.
           05  WS-HASH-OCN-ACCUM              PIC S9(17)
                                              COMP-3 VALUE 0.
           05  WS-HASH-OCN-NUMERIC            PIC S9(09)
                                              COMP-3 VALUE 0.
           05  WS-OCN-NUMERIC-EDIT             PIC 9(04).
           05  WS-OCN-NUMERIC-ALPHA REDEFINES
               WS-OCN-NUMERIC-EDIT            PIC X(04).
      *****************************************************************
      * S800 WORK AREAS - CONTROL RECORD BUILD, BALANCE CHECK, CLOSE. *
      *****************************************************************
       01  WS-CONTROL-BUILD-WORK.
           05  WS-CTL-CYCLE-SAVE              PIC 9(05).
           05  WS-CTL-STEP-SAVE               PIC X(08) VALUE
                                              'S200PROC'.
       01  WS-BALANCE-CHECK-WORK.
           05  WS-BAL-LEFT-SIDE               PIC S9(11) COMP-3.
           05  WS-BAL-RIGHT-SIDE              PIC S9(11) COMP-3.
           05  WS-BAL-DIFF                    PIC S9(11) COMP-3.
      *****************************************************************
      * WS-EDIT-COUNTS-WORK - BREAKS THE STANDARD CABSWRK COUNTERS    *
      * DOWN FURTHER BY EDIT STATUS FOR THE RPTOUT SUMMARY LINE.      *
      *****************************************************************
       01  WS-EDIT-COUNTS-WORK.
           05  WS-CNT-CLEAN                   PIC S9(09) COMP-3
                                              VALUE 0.
           05  WS-CNT-WARN                    PIC S9(09) COMP-3
                                              VALUE 0.
           05  WS-CNT-HARD                    PIC S9(09) COMP-3
                                              VALUE 0.
           05  WS-CNT-FATAL                   PIC S9(09) COMP-3
                                              VALUE 0.
      *****************************************************************
      * PER-RECORD-TYPE COUNTS FOR THE RPTOUT SUMMARY.  A RECORD CAN  *
      * INCREMENT MORE THAN ONE OF THESE (SEE THE CD-REC-TYPE 88-     *
      * LEVEL OVERLAP NOTE AT P6000) SO THESE WILL NOT SUM TO         *
      * WS-READ-CNT - THAT IS EXPECTED, NOT AN ERROR.                  *
      *****************************************************************
           05  WS-CNT-VOICE                   PIC S9(09) COMP-3
                                              VALUE 0.
           05  WS-CNT-DATA                    PIC S9(09) COMP-3
                                              VALUE 0.
           05  WS-CNT-SPCL                    PIC S9(09) COMP-3
                                              VALUE 0.
           05  WS-CNT-UNE                     PIC S9(09) COMP-3
                                              VALUE 0.
           05  WS-CNT-RECIP                   PIC S9(09) COMP-3
                                              VALUE 0.
      *****************************************************************
      * WS-PRINT-LINE-WORK / WS-REPORT-HEADING-WORK - RPTOUT SUMMARY. *
      *****************************************************************
       01  WS-PRINT-LINE-WORK.
           05  WS-PRT-CARRIAGE-CTL            PIC X(01) VALUE ' '.
           05  WS-PRT-TEXT                    PIC X(132).
       01  WS-REPORT-HEADING-WORK.
           05  WS-RPT-TITLE                   PIC X(40) VALUE
               'CABING01 - RAW USAGE EDIT/FORMAT REPORT'.
           05  WS-RPT-RUN-ID-LIT              PIC X(12).
           05  WS-RPT-LINE-CNT                PIC S9(03) COMP-3
                                              VALUE 0.
       01  WS-RPT-DETAIL-LINE.
           05  WS-RPT-CC                      PIC X(01).
           05  WS-RPT-LABEL                   PIC X(20).
           05  FILLER                         PIC X(02).
           05  WS-RPT-VALUE                   PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                         PIC X(100).
       01  WS-ABEND-WORK.
           05  WS-ABEND-LOCATION              PIC X(30).
           05  WS-ABEND-FILE-STATUS           PIC X(02).
      *****************************************************************
      * WS-MISC-FLAGS - CATCH-ALL SWITCHES THAT DID NOT NATURALLY     *
      * BELONG WITH ANY SINGLE EDIT FAMILY ABOVE.                      *
      *****************************************************************
       01  WS-MISC-FLAGS.
           05  WS-VALID-TYPE-SW               PIC X(01) VALUE 'Y'.
               88  WS-REC-TYPE-VALID             VALUE 'Y'.
           05  WS-ANY-FATAL-SW                PIC X(01) VALUE 'N'.
               88  WS-ANY-FATAL-THIS-REC         VALUE 'Y'.
           05  WS-SUSPENSE-WRITTEN-SW         PIC X(01) VALUE 'N'.
               88  WS-SUSPENSE-ALREADY-WRITTEN   VALUE 'Y'.
           05  WS-DEBUG-SW                    PIC X(01) VALUE 'N'.
               88  WS-DEBUG-ON                   VALUE 'Y'.
       PROCEDURE DIVISION.
      *****************************************************************
      * P0000-MAINLINE - MANDATORY CABS STRUCTURE.  P8000-CONTROL IS  *
      * NOT OPTIONAL - IT WRITES CABS-CONTROL-RECORD TO CTLOUT EVERY  *
      * RUN, CLEAN OR NOT.                                             *
      *****************************************************************
       P0000-MAINLINE.
           PERFORM P1000-INIT THRU P1000-EXIT.
           PERFORM P2000-PROCESS THRU P2000-EXIT
               UNTIL WS-EOF.
           PERFORM P8000-CONTROL THRU P8000-EXIT.
           PERFORM P9000-TERM THRU P9000-EXIT.
           STOP RUN.
      *****************************************************************
       S100-INITIALISATION SECTION.
      *****************************************************************
      * P1000-INIT - OPEN FILES, READ AND PARSE THE PARM CARD, SET    *
      * THE RUN DATE, LOAD THE LITERAL EDIT TABLE, ZEROISE COUNTERS.  *
      *****************************************************************
       P1000-INIT.
           PERFORM P1100-OPEN-FILES THRU P1100-EXIT.
           PERFORM P1200-READ-PARMS THRU P1200-EXIT.
           PERFORM P1300-SET-RUN-DATE THRU P1300-EXIT.
           PERFORM P1400-LOAD-EDIT-TABLE THRU P1400-EXIT.
           MOVE WS-PARM-RUN-ID TO WS-THIS-RUN-ID.
           MOVE WS-PARM-RUN-ID TO CT-RUN-ID.
           MOVE WS-PARM-RUN-ID TO SU-RUN-ID.
           MOVE WS-THIS-PROGRAM TO CT-PROCESS-ID.
           MOVE WS-STEP-SEQ-NBR TO CT-STEP-SEQ.
           MOVE ZERO TO WS-READ-CNT.
           MOVE ZERO TO WS-WRITE-CNT.
           MOVE ZERO TO WS-REJECT-CNT.
           MOVE ZERO TO WS-SUMM-CNT.
           MOVE ZERO TO WS-CFWD-CNT.
           MOVE ZERO TO WS-CNT-CLEAN.
           MOVE ZERO TO WS-CNT-WARN.
           MOVE ZERO TO WS-CNT-HARD.
           MOVE ZERO TO WS-CNT-FATAL.
           MOVE ZERO TO WS-HASH-MIN-ACCUM.
           MOVE ZERO TO WS-HASH-AMT-ACCUM.
           MOVE ZERO TO WS-HASH-SEQ-ACCUM.
           MOVE ZERO TO WS-HASH-OCN-ACCUM.
           MOVE 'N' TO WS-EOF-SW.
           MOVE WS-PARM-RUN-ID TO WS-RPT-RUN-ID-LIT.
           DISPLAY 'CABING01 - EDIT SUITE STARTING RUN '
               WS-THIS-RUN-ID.
       P1000-EXIT.
           EXIT.
      *****************************************************************
      * P1100-OPEN-FILES - NOTE THIS PARAGRAPH DOES NOT ROUTE THROUGH *
      * P9900-FATAL-EXIT ON AN OPEN FAILURE.  THAT HANDLER IS RESERVED*
      * FOR THE THREE MID-RUN CONDITIONS DESCRIBED AT P9900.  AN OPEN *
      * FAILURE HERE IS FATAL BEFORE ANY COUNTERS EXIST TO REPORT, SO *
      * IT ABENDS DIRECTLY.                                            *
      *****************************************************************
       P1100-OPEN-FILES.
           OPEN INPUT RAWIN.
           IF WS-FS-INPUT NOT = '00'
               DISPLAY 'CABING01 - OPEN FAILED RAWIN STATUS '
                   WS-FS-INPUT
               STOP RUN.
           OPEN INPUT PARMIN.
           OPEN OUTPUT EDTOUT.
           IF WS-FS-OUTPUT NOT = '00'
               DISPLAY 'CABING01 - OPEN FAILED EDTOUT STATUS '
                   WS-FS-OUTPUT
               STOP RUN.
           OPEN OUTPUT SUSOUT.
           IF WS-FS-SUSPENSE NOT = '00'
               DISPLAY 'CABING01 - OPEN FAILED SUSOUT STATUS '
                   WS-FS-SUSPENSE
               STOP RUN.
           OPEN OUTPUT CTLOUT.
           OPEN OUTPUT RPTOUT.
       P1100-EXIT.
           EXIT.
      *****************************************************************
      * P1200-READ-PARMS - READS THE SINGLE INSTREAM PARM CARD.  THE  *
      * CARD IS FREE FORM SLASH-DELIMITED:                             *
      *   RUNID/CYCLE-YYDDD/RERUN-NBR/RESTART-KEY                      *
      * CABPARMR VALIDATES THE CARD GENERICALLY (LENGTH, PRINTABLE    *
      * CHARACTERS).  THE ACTUAL FIELD SPLIT IS DONE LOCALLY WITH     *
      * UNSTRING - CABPARMR DOES NOT PARSE, IT ONLY VALIDATES.         *
      *****************************************************************
       P1200-READ-PARMS.
           MOVE SPACES TO PM-PARM-CARD-REC.
           READ PARMIN
               AT END MOVE SPACES TO PM-PARM-CARD-REC.
           MOVE PM-PARM-CARD-REC TO PR-CARD-IN.
           CALL 'CABPARMR' USING PR-CARD-IN PR-VALID-SW.
           IF PR-CARD-INVALID
               DISPLAY 'CABING01 - PARM CARD FAILED CABPARMR '
                   'VALIDATION - DEFAULTS WILL BE USED'
               MOVE SPACES TO PM-PARM-CARD-REC.
      *****************************************************************
      * CABS-STD-020 - STRING ASSEMBLY: THE ONE UNSTRING IN THIS     **
      * PROGRAM.  SPLITS THE PARM CARD ON '/' INTO ITS FOUR FIELDS.   *
      * IF FEWER THAN FOUR FIELDS ARE PRESENT THE TRAILING RECEIVING  *
      * FIELDS SIMPLY KEEP WHATEVER MOVE SPACES LEFT IN THEM ABOVE.   *
      *****************************************************************
           MOVE ZERO TO WS-UNSTR-FIELD-CNT.
           UNSTRING PM-PARM-CARD-REC DELIMITED BY '/'
               INTO WS-PARM-RUN-ID
                    WS-PARM-CYCLE-YYDDD
                    WS-PARM-RERUN-NBR
                    WS-PARM-RESTART-KEY
               TALLYING IN WS-UNSTR-FIELD-CNT.
           IF WS-PARM-CYCLE-YYDDD = ZERO
               MOVE 'S' TO WS-RUN-DATE-SRC-SW
           ELSE
               MOVE 'P' TO WS-RUN-DATE-SRC-SW.
           PERFORM P1250-DEFAULT-RUN-ID THRU P1250-EXIT.
       P1200-EXIT.
           EXIT.
      *****************************************************************
      * P1250-DEFAULT-RUN-ID - IF THE PARM CARD DID NOT SUPPLY A RUN  *
      * ID (BLANK OR ALL SPACES AFTER THE UNSTRING), A DEFAULT IS     *
      * BUILT FROM THE PROGRAM NAME AND THE JULIAN DAY SO EVERY RUN   *
      * STILL PRODUCES A TRACEABLE CT-RUN-ID / SU-RUN-ID VALUE.        *
      *****************************************************************
       P1250-DEFAULT-RUN-ID.
           IF WS-PARM-RUN-ID = SPACES
               ACCEPT WS-PARM-CYCLE-YYDDD FROM DAY
               STRING WS-THIS-PROGRAM DELIMITED BY SPACE
                      '-' DELIMITED BY SIZE
                      WS-PARM-CYCLE-YYDDD DELIMITED BY SIZE
                   INTO WS-PARM-RUN-ID.
       P1250-EXIT.
           EXIT.
      *****************************************************************
      * P1300-SET-RUN-DATE - IF THE PARM CARD SUPPLIED A CYCLE DATE   *
      * USE IT, OTHERWISE FALL BACK TO THE SYSTEM DATE VIA THE JULIAN *
      * DAY SPECIAL REGISTER.  CABDTCNV IS THEN CALLED TO PRODUCE A   *
      * GREGORIAN DATE FOR THE RPTOUT HEADING.                         *
      *****************************************************************
       P1300-SET-RUN-DATE.
           IF WS-RUN-DATE-FROM-PARM
               MOVE WS-PARM-CYCLE-YYDDD TO WS-RUN-YYDDD
           ELSE
               ACCEPT WS-RUN-YYDDD FROM DAY.
           MOVE '1' TO CV-FUNCTION-CD.
           MOVE WS-RUN-YYDDD TO CV-YYDDD-IN.
           MOVE DW-PIVOT-YY TO CV-PIVOT-YY.
           CALL 'CABDTCNV' USING CV-FUNCTION-CD CV-YYDDD-IN
               CV-PIVOT-YY CV-CCYYMMDD-OUT CV-RETURN-CD.
           IF CV-RETURN-CD NOT = '00'
               DISPLAY 'CABING01 - CABDTCNV RC ' CV-RETURN-CD
                   ' ON RUN DATE CONVERSION - CONTINUING'.
           MOVE CV-OUT-CCYY TO WS-RUN-CCYY.
           MOVE WS-RUN-YYDDD TO CT-CYCLE-YYDDD.
           MOVE WS-RUN-YYDDD TO WS-CTL-CYCLE-SAVE.
           MOVE ZERO TO CT-BILL-PERIOD.
           MOVE WS-PARM-RERUN-NBR TO CT-RERUN-NBR.
           PERFORM P1350-VALIDATE-CYCLE-RANGE THRU P1350-EXIT.
       P1300-EXIT.
           EXIT.
      *****************************************************************
      * P1350-VALIDATE-CYCLE-RANGE - DDD MUST BE 1 THRU 366.  THIS IS *
      * A SANITY CHECK ON THE RUN DATE ITSELF (FROM THE PARM CARD OR  *
      * THE SYSTEM CLOCK), NOT ON ANY INPUT RECORD.  IT DOES NOT ROUTE*
      * THROUGH P9900-FATAL-EXIT - THAT HANDLER IS RESERVED FOR THE   *
      * THREE MID-RUN CONDITIONS DESCRIBED AT P9900.  A BAD CYCLE     *
      * DATE IS CAUGHT BEFORE ANY RECORD HAS BEEN READ, SO IT ABENDS  *
      * DIRECTLY, THE SAME WAY P1100-OPEN-FILES DOES.                  *
      *****************************************************************
       P1350-VALIDATE-CYCLE-RANGE.
           IF WS-RUN-DDD-ONLY < 1 OR WS-RUN-DDD-ONLY > 366
               DISPLAY 'CABING01 - INVALID CYCLE DDD '
                   WS-RUN-DDD-ONLY
               PERFORM P9100-CLOSE-FILES THRU P9100-EXIT
               STOP RUN.
       P1350-EXIT.
           EXIT.
      *****************************************************************
      * P1400-LOAD-EDIT-TABLE - LOADS THE LITERAL SEVERITY/STATUS MAP *
      * USED WHEN THE FINAL CD-EDIT-STATUS IS STAMPED IN P2200.  TEN  *
      * ENTRIES, LOADED BY SUBSCRIPT (INDEXED OCCURS - SET/SEARCH).   *
      *****************************************************************
       P1400-LOAD-EDIT-TABLE.
      *****************************************************************
      * SLOT 1 - UNKNOWN OCN.  HARD FAILURE - RATING CANNOT PROCEED   *
      * WITHOUT A CARRIER, SO THIS RECORD IS REJECTED (STATUS 6).      *
      *****************************************************************
           SET WS-ET-IDX TO 1.
           MOVE EC-OCN-UNKNOWN    TO WS-ET-ERR-CODE(WS-ET-IDX).
           MOVE 'E'                TO WS-ET-SEVERITY(WS-ET-IDX).
           MOVE '6'                TO WS-ET-STATUS-STAMP(WS-ET-IDX).
      *****************************************************************
      * SLOT 2 - UNKNOWN/MISSING BAN.  WARNING ONLY - BILLING CAN     *
      * SOMETIMES RESOLVE THE BAN FROM THE OCN AND CIRCUIT LATER.     *
      *****************************************************************
           SET WS-ET-IDX TO 2.
           MOVE EC-BAN-UNKNOWN    TO WS-ET-ERR-CODE(WS-ET-IDX).
           MOVE 'W'                TO WS-ET-SEVERITY(WS-ET-IDX).
           MOVE '2'                TO WS-ET-STATUS-STAMP(WS-ET-IDX).
      *****************************************************************
      * SLOT 3 - INDETERMINATE JURISDICTION.  LOWEST WARNING STATUS - *
      * JURISDICTION CAN OFTEN BE DERIVED DOWNSTREAM FROM NPA-NXX.    *
      *****************************************************************
           SET WS-ET-IDX TO 3.
           MOVE EC-JURIS-INDET    TO WS-ET-ERR-CODE(WS-ET-IDX).
           MOVE 'W'                TO WS-ET-SEVERITY(WS-ET-IDX).
           MOVE '1'                TO WS-ET-STATUS-STAMP(WS-ET-IDX).
      *****************************************************************
      * SLOT 4 - INVALID CONNECT OR DISCONNECT DATE.  HARD FAILURE -  *
      * NO BILL PERIOD CAN BE ASSIGNED WITHOUT A VALID DATE.           *
      *****************************************************************
           SET WS-ET-IDX TO 4.
           MOVE EC-DATE-INVALID   TO WS-ET-ERR-CODE(WS-ET-IDX).
           MOVE 'E'                TO WS-ET-SEVERITY(WS-ET-IDX).
           MOVE '7'                TO WS-ET-STATUS-STAMP(WS-ET-IDX).
      *****************************************************************
      * SLOT 5 - DUPLICATE OR NON-NUMERIC SEQUENCE.  BORDERLINE - A   *
      * DUPLICATE COULD BE A GENUINE RESEND, NOT ALWAYS AN ERROR.     *
      *****************************************************************
           SET WS-ET-IDX TO 5.
           MOVE EC-DUP-SEQ        TO WS-ET-ERR-CODE(WS-ET-IDX).
           MOVE 'E'                TO WS-ET-SEVERITY(WS-ET-IDX).
           MOVE '5'                TO WS-ET-STATUS-STAMP(WS-ET-IDX).
      *****************************************************************
      * SLOT 6 - NEGATIVE MINUTES.  HARD FAILURE - ALWAYS A FEED      *
      * ERROR, NEVER A LEGITIMATE BILLING SITUATION.                   *
      *****************************************************************
           SET WS-ET-IDX TO 6.
           MOVE EC-MIN-NEGATIVE   TO WS-ET-ERR-CODE(WS-ET-IDX).
           MOVE 'F'                TO WS-ET-SEVERITY(WS-ET-IDX).
           MOVE '8'                TO WS-ET-STATUS-STAMP(WS-ET-IDX).
      *****************************************************************
      * SLOT 7 - MPB PERCENT OUT OF RANGE.  WARNING - TIGHTENED IN    *
      * V2.10 PER THE 2011 TARIFF FILING (SEE HEADER).                 *
      *****************************************************************
           SET WS-ET-IDX TO 7.
           MOVE EC-MPB-PCT-INVALID TO WS-ET-ERR-CODE(WS-ET-IDX).
           MOVE 'E'                TO WS-ET-SEVERITY(WS-ET-IDX).
           MOVE '4'                TO WS-ET-STATUS-STAMP(WS-ET-IDX).
      *****************************************************************
      * SLOT 8 - OUT OF BALANCE.  NOT ACTUALLY RAISED BY ANY EDIT     *
      * PARAGRAPH IN THIS PROGRAM - BALANCING IS A P8000 CONCERN, NOT *
      * A PER-RECORD ONE.  THIS SLOT IS LOADED FOR ESTATE-WIDE TABLE  *
      * CONSISTENCY WITH THE OTHER INGEST PROGRAMS AND STAYS AT ZERO. *
      *****************************************************************
           SET WS-ET-IDX TO 8.
           MOVE EC-OUT-OF-BALANCE TO WS-ET-ERR-CODE(WS-ET-IDX).
           MOVE 'F'                TO WS-ET-SEVERITY(WS-ET-IDX).
           MOVE '9'                TO WS-ET-STATUS-STAMP(WS-ET-IDX).
      *****************************************************************
      * SLOT 9 - VALUE OUT OF RANGE.  REUSED ACROSS SEVERAL UNRELATED *
      * RANGE CHECKS (NPA-NXX, MINUTE/ELAPSED VARIANCE) - SEE P6100   *
      * AND P5420.  A LOOSE CODE, BUT THE ONLY GENERIC ONE AVAILABLE. *
      *****************************************************************
           SET WS-ET-IDX TO 9.
           MOVE EC-PIU-OUT-OF-RANGE TO WS-ET-ERR-CODE(WS-ET-IDX).
           MOVE 'W'                TO WS-ET-SEVERITY(WS-ET-IDX).
           MOVE '3'                TO WS-ET-STATUS-STAMP(WS-ET-IDX).
      *****************************************************************
      * SLOT 10 - UNKNOWN CIRCUIT.  LOADED PER THE V2.02 REVISION     *
      * NOTE ABOVE, BUT THIS PROGRAM DOES NOT OPEN CIRCMST AND HAS NO *
      * CIRCUIT MASTER LOOKUP - SEE P6300 AND P6400 FOR WHERE THIS    *
      * CODE ACTUALLY FIRES TODAY (CIRCUIT-ID FORMAT ONLY, NOT A      *
      * MASTER FILE LOOKUP).                                           *
      *****************************************************************
           SET WS-ET-IDX TO 10.
           MOVE EC-CIRCUIT-UNKNOWN TO WS-ET-ERR-CODE(WS-ET-IDX).
           MOVE 'E'                TO WS-ET-SEVERITY(WS-ET-IDX).
           MOVE '6'                TO WS-ET-STATUS-STAMP(WS-ET-IDX).
           MOVE 10 TO WS-ET-LOAD-CNT.
           SET WS-ET-IDX TO 1.
       P1400-EXIT.
           EXIT.
      *****************************************************************
       S200-PROCESS-CONTROL SECTION.
      *****************************************************************
      * P2000-PROCESS - ONE PASS PER INPUT RECORD.  READ, EDIT,       *
      * WRITE (OR REJECT).  THIS PARAGRAPH IS THE UNTIL-WS-EOF LOOP   *
      * BODY IN P0000-MAINLINE.                                        *
      *****************************************************************
       P2000-PROCESS.
           PERFORM P2100-READ-RAW THRU P2100-EXIT.
           IF NOT WS-EOF
               PERFORM P2200-EDIT-RECORD THRU P2200-EXIT
               PERFORM P2300-WRITE-EDITED THRU P2300-EXIT.
       P2000-EXIT.
           EXIT.
      *****************************************************************
      * P2100-READ-RAW - CABS-STD-003: ON A BAD I/O STATUS (ANYTHING  *
      * OTHER THAN 00 NORMAL OR 10 END OF FILE) THIS PARAGRAPH GOES   *
      * TO P9900-FATAL-EXIT, PHYSICALLY AT THE BOTTOM OF THE PROGRAM. *
      *****************************************************************
       P2100-READ-RAW.
           READ RAWIN
               AT END MOVE 'Y' TO WS-EOF-SW.
           IF WS-EOF
               GO TO P2100-EXIT.
           IF WS-FS-INPUT NOT = '00'
               MOVE 'P2100-READ-RAW' TO WS-ABEND-LOCATION
               MOVE WS-FS-INPUT TO WS-ABEND-FILE-STATUS
               GO TO P9900-FATAL-EXIT.
           ADD 1 TO WS-READ-CNT.
       P2100-EXIT.
           EXIT.
      *****************************************************************
      * P2200-EDIT-RECORD - DRIVES THE FULL EDIT BATTERY AGAINST ONE  *
      * CABS-CDR-RECORD, THEN RESOLVES THE ACCUMULATED SEVERITY INTO  *
      * A FINAL CD-EDIT-STATUS AND A KEEP/REJECT DECISION.            *
      *                                                                *
      * CABS-STD-006 - WS-EDT-SEVERITY'S 88-LEVELS OVERLAP AT VALUE   *
      * '3' (WS-SEV-WARN COVERS 1-3, WS-SEV-HARD COVERS 3-9).  WHICH  *
      * ONE FIRES FOR A '3' DEPENDS ENTIRELY ON TEST ORDER BELOW -    *
      * WARN IS TESTED FIRST SO A '3' NEVER REACHES THE HARD BRANCH.  *
      *****************************************************************
       P2200-EDIT-RECORD.
           MOVE '0' TO WS-EDT-SEVERITY.
           MOVE 'N' TO WS-ANY-FATAL-SW.
           MOVE CD-OCN TO WS-EDT-OCN.
           MOVE CD-BAN TO WS-EDT-BAN.
           MOVE CD-SEQ-NBR TO WS-EDT-SEQ.
           MOVE CD-REC-TYPE TO WS-EDT-REC-TYPE.
           MOVE CD-USAGE-TYPE TO WS-EDT-USAGE-TYPE.
           MOVE CD-JURIS-CD TO WS-EDT-JURIS-CD.
           MOVE CD-RATE-ELEM TO WS-EDT-RATE-ELEM.
           MOVE CD-CONN-YY TO WS-EDT-CONN-YY.
           MOVE CD-CONN-DDD TO WS-EDT-CONN-DDD.
           MOVE CD-DISC-YYDDD TO WS-EDT-DISC-YYDDD.
           PERFORM P2250-COUNT-REC-TYPE THRU P2250-EXIT.
           PERFORM P3000-EDIT-KEY THRU P3000-EXIT.
           PERFORM P4000-EDIT-RECTYPE THRU P4000-EXIT.
           PERFORM P5000-EDIT-DATES THRU P5000-EXIT.
           PERFORM P6000-EDIT-VARIANT THRU P6000-EXIT.
           PERFORM P7000-SCAN-NUMERIC THRU P7000-EXIT.
           PERFORM P7100-SCAN-SPACES THRU P7100-EXIT.
           IF WS-SEV-WARN
               MOVE 'N' TO WS-ANY-FATAL-SW
           ELSE
               IF WS-SEV-HARD
                   MOVE 'Y' TO WS-ANY-FATAL-SW
               ELSE
                   MOVE 'N' TO WS-ANY-FATAL-SW.
           MOVE WS-EDT-SEVERITY TO CD-EDIT-STATUS.
           MOVE WS-RUN-YYDDD TO CD-LOAD-YYDDD.
           IF WS-ANY-FATAL-THIS-REC
               ADD 1 TO WS-CNT-FATAL
               ADD 1 TO WS-REJECT-CNT
           ELSE
               IF WS-EDT-SEVERITY = '0'
                   ADD 1 TO WS-CNT-CLEAN
               ELSE
                   IF WS-SEV-WARN
                       ADD 1 TO WS-CNT-WARN
                   ELSE
                       ADD 1 TO WS-CNT-HARD.
           PERFORM P7400-ACCUM-HASH THRU P7400-EXIT.
       P2200-EXIT.
           EXIT.
      *****************************************************************
      * P2250-COUNT-REC-TYPE - INCREMENTS THE PER-RECORD-TYPE         *
      * COUNTERS BEFORE ANY EDIT HAS RUN, SO THE COUNTS REFLECT WHAT  *
      * ARRIVED ON RAWIN, NOT WHAT SURVIVED EDITING.                   *
      *****************************************************************
       P2250-COUNT-REC-TYPE.
           IF CD-VOICE-MOU
               ADD 1 TO WS-CNT-VOICE.
           IF CD-DATA-SVC
               ADD 1 TO WS-CNT-DATA.
           IF CD-SPECIAL-ACC
               ADD 1 TO WS-CNT-SPCL.
           IF CD-UNBUNDLED
               ADD 1 TO WS-CNT-UNE.
           IF CD-RECIP-COMP
               ADD 1 TO WS-CNT-RECIP.
       P2250-EXIT.
           EXIT.
      *****************************************************************
      * P2300-WRITE-EDITED - FATAL RECORDS ARE NOT WRITTEN TO EDTOUT, *
      * THEY WERE ALREADY LOGGED TO SUSOUT BY THE EDIT PARAGRAPH THAT *
      * FAILED THEM.  EVERYTHING ELSE (CLEAN OR SUSPECT) IS WRITTEN.  *
      *****************************************************************
       P2300-WRITE-EDITED.
           IF WS-ANY-FATAL-THIS-REC
               GO TO P2300-EXIT.
           MOVE CABS-CDR-RECORD TO EDT-CDR-OUT-REC.
           WRITE EDT-CDR-OUT-REC.
           IF WS-FS-OUTPUT NOT = '00'
               DISPLAY 'CABING01 - WRITE FAILED EDTOUT STATUS '
                   WS-FS-OUTPUT
               MOVE 'P2300-WRITE-EDITED' TO WS-ABEND-LOCATION
               MOVE WS-FS-OUTPUT TO WS-ABEND-FILE-STATUS
               PERFORM P9100-CLOSE-FILES THRU P9100-EXIT
               CALL 'CABABEND' USING WS-THIS-PROGRAM
                   WS-ABEND-REASON-CD WS-THIS-RUN-ID
               STOP RUN.
           ADD 1 TO WS-WRITE-CNT.
       P2300-EXIT.
           EXIT.
      *****************************************************************
       S300-RECORD-EDITS SECTION.
      *****************************************************************
      * P3000-EDIT-KEY - CABS-STD-005: WS-BILL-KEY (66-LEVEL RENAMES  *
      * OF WS-EDT-OCN THRU WS-EDT-BAN) IS MOVED INTO THE 17-BYTE SCAN *
      * AREA AS A SINGLE UNIT.  THE LOW-VALUES SCAN BELOW WALKS THE   *
      * REDEFINES BYTE TABLE RATHER THAN THE GROUP ITEM DIRECTLY.     *
      *****************************************************************
       P3000-EDIT-KEY.
           MOVE WS-BILL-KEY TO WS-KEY-SCAN-AREA.
           MOVE ZERO TO WS-KEY-BAD-CNT.
           PERFORM P3050-SCAN-KEY-BYTES THRU P3050-EXIT
               VARYING WS-KEY-SUB FROM 1 BY 1
               UNTIL WS-KEY-SUB > 17.
           PERFORM P3100-EDIT-OCN-FMT THRU P3100-EXIT.
           PERFORM P3200-EDIT-BAN-FMT THRU P3200-EXIT.
           PERFORM P3300-EDIT-SEQ THRU P3300-EXIT.
           IF CD-RECIP-COMP
               PERFORM P3400-EDIT-RECIP-COMP THRU P3400-EXIT.
       P3000-EXIT.
           EXIT.
      *****************************************************************
      * P3050-SCAN-KEY-BYTES - CABS-STD-023: WALKS THE REDEFINES     **
      * BYTE TABLE ONE POSITION AT A TIME.  NO REFERENCE MODIFICATION *
      * IS USED - THIS IS A SUBSCRIPTED OCCURS WALK VIA PERFORM       *
      * VARYING ... THRU (NOT INLINE - THIS IS A 1974-STYLE OUT-OF-   *
      * LINE PERFORM, NO END-PERFORM).                                *
      *****************************************************************
       P3050-SCAN-KEY-BYTES.
           IF WS-KEY-BYTE(WS-KEY-SUB) = LOW-VALUE
               ADD 1 TO WS-KEY-BAD-CNT.
       P3050-EXIT.
           EXIT.
      *****************************************************************
      * P3100-EDIT-OCN-FMT - FORMAT CHECK, THEN CABOCNVL FOR CARRIER  *
      * MASTER EXISTENCE AND TARIFF EFFECTIVITY.                       *
      *****************************************************************
       P3100-EDIT-OCN-FMT.
           MOVE WS-EDT-OCN TO WS-OCN-SCAN-AREA.
           MOVE ZERO TO WS-OCN-SPACE-CNT.
           INSPECT WS-OCN-SCAN-AREA TALLYING WS-OCN-SPACE-CNT
               FOR ALL SPACES.
      *****************************************************************
      * WS-KEY-BAD-CNT WAS SET BY P3050 AGAINST THE WHOLE 17-BYTE     *
      * KEY (OCN + BAN).  IF ANY LOW-VALUE BYTE WAS FOUND ANYWHERE IN *
      * THE KEY, TREAT THE OCN AS SUSPECT TOO - NOT JUST THE BAN.      *
      *****************************************************************
           IF WS-KEY-BAD-CNT NOT = ZERO
               ADD 1 TO WS-OCN-SPACE-CNT.
           IF WS-OCN-SPACE-CNT NOT = ZERO
               MOVE 'P3100-EDIT-OCN-FMT' TO WS-EMB-PARA-NAME
               MOVE EC-OCN-UNKNOWN TO WS-EMB-ERR-CODE
               PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
               PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT
           ELSE
               MOVE WS-EDT-OCN TO OV-OCN-IN
               MOVE WS-RUN-YYDDD TO OV-EFF-YYDDD-IN
               CALL 'CABOCNVL' USING OV-OCN-IN OV-EFF-YYDDD-IN
                   OV-FOUND-SW OV-EFFECTIVE-SW
               IF OV-OCN-NOT-FOUND
                   MOVE 'P3100-EDIT-OCN-FMT' TO WS-EMB-PARA-NAME
                   MOVE EC-OCN-UNKNOWN TO WS-EMB-ERR-CODE
                   PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
                   PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT
               ELSE
                   IF NOT OV-OCN-EFFECTIVE
                       MOVE 'P3100-EDIT-OCN-FMT' TO
                           WS-EMB-PARA-NAME
                       MOVE EC-TERM-EXPIRED TO WS-EMB-ERR-CODE
                       PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
                       PERFORM P7300-WRITE-SUSPENSE THRU
                           P7300-EXIT.
       P3100-EXIT.
           EXIT.
      *****************************************************************
      * P3200-EDIT-BAN-FMT - THE BAN MUST NOT BE ALL SPACES AND MUST  *
      * NOT CONTAIN EMBEDDED LOW-VALUES (WS-KEY-BAD-CNT FROM P3050    *
      * COVERS THE WHOLE 17-BYTE KEY, INCLUDING THE BAN PORTION).      *
      *****************************************************************
       P3200-EDIT-BAN-FMT.
           MOVE WS-EDT-BAN TO WS-BAN-SCAN-AREA.
           MOVE 'Y' TO WS-BAN-VALID-SW.
           IF WS-BAN-SCAN-AREA = SPACES
               MOVE 'N' TO WS-BAN-VALID-SW.
           IF WS-KEY-BAD-CNT NOT = ZERO
               MOVE 'N' TO WS-BAN-VALID-SW.
           IF NOT WS-BAN-FORMAT-OK
               MOVE 'P3200-EDIT-BAN-FMT' TO WS-EMB-PARA-NAME
               MOVE EC-BAN-UNKNOWN TO WS-EMB-ERR-CODE
               PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
               PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT.
      *****************************************************************
      * A WHOLESALE BAN IS EXPECTED TO CARRY EXACTLY ONE EMBEDDED     *
      * DASH (NNNNNNNNN-NNN FORMAT).  ZERO DASHES OR MORE THAN ONE IS *
      * A WARNING, NOT A HARD FAILURE - THE DOWNSTREAM RATING STEPS   *
      * DO NOT ACTUALLY DEPEND ON THE DASH POSITION, ONLY ON THE BAN  *
      * BEING NON-SPACE, SO THIS IS BELT-AND-BRACES REPORTING.        *
      *****************************************************************
           MOVE ZERO TO WS-BAN-DASH-CNT.
           INSPECT WS-BAN-SCAN-AREA TALLYING WS-BAN-DASH-CNT
               FOR ALL '-'.
           IF WS-BAN-DASH-CNT NOT = 1
               MOVE 'P3200-EDIT-BAN-FMT' TO WS-EMB-PARA-NAME
               MOVE EC-BAN-UNKNOWN TO WS-EMB-ERR-CODE
               PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
               PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT.
       P3200-EXIT.
           EXIT.
      *****************************************************************
      * P3300-EDIT-SEQ - SEQUENCE NUMBER MUST BE NUMERIC AND MUST NOT *
      * REPEAT THE IMMEDIATELY PRIOR SEQUENCE NUMBER SEEN THIS RUN.   *
      * THIS IS A WEAK DUPLICATE CHECK (ADJACENT ONLY) - CABING01     *
      * DOES NOT SORT, SO A TRUE DUPLICATE CHECK IS NOT POSSIBLE      *
      * HERE.  A FULL CHECK HAPPENS DOWNSTREAM AFTER THE SORT STEP.   *
      *****************************************************************
       P3300-EDIT-SEQ.
           MOVE WS-EDT-SEQ TO WS-SEQ-EDIT-AREA.
           MOVE 'N' TO WS-SEQ-GAP-SW.
           IF WS-SEQ-EDIT-AREA NOT NUMERIC
               MOVE 'P3300-EDIT-SEQ' TO WS-EMB-PARA-NAME
               MOVE EC-DUP-SEQ TO WS-EMB-ERR-CODE
               PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
               PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT
           ELSE
               IF WS-SEQ-EDIT-AREA = WS-SEQ-PRIOR
                       AND WS-SEQ-PRIOR NOT = ZERO
                   MOVE 'Y' TO WS-SEQ-GAP-SW
                   MOVE 'P3300-EDIT-SEQ' TO WS-EMB-PARA-NAME
                   MOVE EC-DUP-SEQ TO WS-EMB-ERR-CODE
                   PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
                   PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT.
           MOVE WS-SEQ-EDIT-AREA TO WS-SEQ-PRIOR.
           IF WS-SEQ-EDIT-AREA NUMERIC
               PERFORM P3350-CHECK-SEQ-CACHE THRU P3350-EXIT.
       P3300-EXIT.
           EXIT.
      *****************************************************************
      * P3350-CHECK-SEQ-CACHE - CABS-STD-023 (SUBSCRIPTED OCCURS     **
      * WALK): SEARCHES THE 20-SLOT RECENT-SEQUENCE RING BUFFER FOR A *
      * NEAR-NEIGHBOUR DUPLICATE, THEN STORES THE CURRENT SEQUENCE    *
      * NUMBER IN THE NEXT SLOT, WRAPPING BACK TO 1 AFTER SLOT 20.     *
      *****************************************************************
       P3350-CHECK-SEQ-CACHE.
           MOVE 'N' TO WS-SC-DUP-FOUND-SW.
           PERFORM P3360-SEARCH-SEQ-CACHE THRU P3360-EXIT
               VARYING WS-SC-SEARCH-SUB FROM 1 BY 1
               UNTIL WS-SC-SEARCH-SUB > 20 OR WS-SC-DUP-FOUND.
           IF WS-SC-DUP-FOUND
               MOVE 'P3350-CHECK-SEQ-CACHE' TO WS-EMB-PARA-NAME
               MOVE EC-DUP-SEQ TO WS-EMB-ERR-CODE
               PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
               PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT.
           MOVE WS-SEQ-EDIT-AREA TO
               WS-SC-SEQ-VALUE(WS-SC-NEXT-SLOT).
           ADD 1 TO WS-SC-NEXT-SLOT.
           IF WS-SC-NEXT-SLOT > 20
               MOVE 1 TO WS-SC-NEXT-SLOT.
       P3350-EXIT.
           EXIT.
       P3360-SEARCH-SEQ-CACHE.
           IF WS-SC-SEQ-VALUE(WS-SC-SEARCH-SUB) = WS-SEQ-EDIT-AREA
                   AND WS-SC-SEQ-VALUE(WS-SC-SEARCH-SUB) NOT = ZERO
               MOVE 'Y' TO WS-SC-DUP-FOUND-SW.
       P3360-EXIT.
           EXIT.
      *****************************************************************
      * P3400-EDIT-RECIP-COMP - RECIPROCAL COMPENSATION RECORDS (CD-  *
      * REC-TYPE '08') ARE EXPECTED TO BE LOCAL JURISDICTION AND      *
      * WITHIN A SANITY CAP ON CHARGED MINUTES - A CAP THAT IN        *
      * PRODUCTION COMES FROM CR-ISP-CAP-MOU ON THE CARRIER MASTER,   *
      * BUT THIS EDIT-STAGE PROGRAM DOES NOT OPEN CARRMST, SO A FIXED *
      * ESTATE-WIDE SANITY CAP IS USED INSTEAD - A LOOSER CHECK THAN  *
      * RATING WILL APPLY LATER.                                      *
      *****************************************************************
       P3400-EDIT-RECIP-COMP.
           IF NOT CD-LOCAL
               MOVE 'P3400-EDIT-RECIP-COMP' TO WS-EMB-PARA-NAME
               MOVE EC-RECIP-CAP-EXCEEDED TO WS-EMB-ERR-CODE
               PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
               PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT.
           IF CD-VC-CHG-MIN > 10000
               MOVE 'P3400-EDIT-RECIP-COMP' TO WS-EMB-PARA-NAME
               MOVE EC-RECIP-CAP-EXCEEDED TO WS-EMB-ERR-CODE
               PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
               PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT.
       P3400-EXIT.
           EXIT.
      *****************************************************************
      * P4000-EDIT-RECTYPE - CD-VALID-TYPE COVERS '01' THRU '08'.     *
      * ANYTHING OUTSIDE THAT RANGE HAS NO BASIS FOR FURTHER EDITING  *
      * SO THE VARIANT-AREA AND DATE EDITS STILL RUN (THIS PROGRAM    *
      * DOES NOT SHORT-CIRCUIT ON A RECTYPE FAILURE - SEE CABS-STD-   *
      * 021, EVERY EDIT ALWAYS RUNS SO THE SUSPENSE RECORD SET IS     *
      * COMPLETE FOR MANUAL REVIEW).                                   *
      *****************************************************************
       P4000-EDIT-RECTYPE.
           MOVE 'Y' TO WS-RECTYPE-VALID-SW.
           IF NOT CD-VALID-TYPE
               MOVE 'N' TO WS-RECTYPE-VALID-SW
               MOVE 'P4000-EDIT-RECTYPE' TO WS-EMB-PARA-NAME
               MOVE EC-RESTATE-NO-BASIS TO WS-EMB-ERR-CODE
               PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
               PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT.
           PERFORM P4100-EDIT-USAGE-TYPE THRU P4100-EXIT.
           PERFORM P4200-EDIT-JURIS THRU P4200-EXIT.
           PERFORM P4300-EDIT-RATE-ELEM THRU P4300-EXIT.
       P4000-EXIT.
           EXIT.
      *****************************************************************
      * P4100-EDIT-USAGE-TYPE - CD-USAGE-TYPE IS A SINGLE CHARACTER   *
      * SUB-CLASSIFIER WITHIN THE RECORD TYPE.  VOICE RECORDS USE     *
      * V/D/M/F (VOICE, DIRECTORY, MISCELLANEOUS, FEATURE).  OTHER    *
      * RECORD TYPES USE THEIR OWN CODE SETS NOT VALIDATED HERE.       *
      *****************************************************************
       P4100-EDIT-USAGE-TYPE.
           MOVE 'Y' TO WS-UT-VALID-SW.
           IF CD-VOICE-MOU
               MOVE CD-USAGE-TYPE TO WS-UT-SAVE
               IF WS-UT-SAVE = SPACE
                   MOVE 'N' TO WS-UT-VALID-SW.
           IF NOT WS-UT-OK
               MOVE 'P4100-EDIT-USAGE-TYPE' TO WS-EMB-PARA-NAME
               MOVE EC-FACTOR-MISSING TO WS-EMB-ERR-CODE
               PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
               PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT.
           IF CD-DATA-SVC
               PERFORM P4150-EDIT-USAGE-DATA THRU P4150-EXIT.
           IF CD-SPECIAL-ACC
               PERFORM P4160-EDIT-USAGE-SPCL THRU P4160-EXIT.
       P4100-EXIT.
           EXIT.
      *****************************************************************
      * P4150-EDIT-USAGE-DATA - DATA SERVICE USAGE TYPE SHOULD BE ONE *
      * OF P (PRIVATE LINE), B (BROADBAND) OR I (INTERNET TRANSIT -   *
      * THE FIELDS ADDED IN V2.07 PER THE HEADER, STILL UNUSED BY     *
      * RATING TODAY).                                                 *
      *****************************************************************
       P4150-EDIT-USAGE-DATA.
           IF CD-USAGE-TYPE NOT = 'P' AND CD-USAGE-TYPE NOT = 'B'
                   AND CD-USAGE-TYPE NOT = 'I'
               MOVE 'P4150-EDIT-USAGE-DATA' TO WS-EMB-PARA-NAME
               MOVE EC-FACTOR-MISSING TO WS-EMB-ERR-CODE
               PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
               PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT.
       P4150-EXIT.
           EXIT.
      *****************************************************************
      * P4160-EDIT-USAGE-SPCL - SPECIAL ACCESS USAGE TYPE SHOULD BE   *
      * ONE OF D (DEDICATED) OR S (SWITCHED SPECIAL ACCESS).          *
      *****************************************************************
       P4160-EDIT-USAGE-SPCL.
           IF CD-USAGE-TYPE NOT = 'D' AND CD-USAGE-TYPE NOT = 'S'
               MOVE 'P4160-EDIT-USAGE-SPCL' TO WS-EMB-PARA-NAME
               MOVE EC-FACTOR-MISSING TO WS-EMB-ERR-CODE
               PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
               PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT.
       P4160-EXIT.
           EXIT.
      *****************************************************************
      * P4200-EDIT-JURIS - CD-JURIS-CD SHOULD BE I/S/L.  A SPACE OR   *
      * 'X' IS INDETERMINATE - A WARNING, NOT A HARD FAILURE.  THIS   *
      * PARAGRAPH IS ALSO WHERE CABS-STD-014 IS VIOLATED: THE CHECK   *
      * BELOW READS CD-JURIS-CD DIRECTLY WITHOUT FIRST CONFIRMING     *
      * CD-USAGE-TYPE, EVEN THOUGH JURISDICTION IS PART OF THE FIXED  *
      * PORTION OF THE RECORD (NOT THE VARIANT AREA) SO THE VIOLATION *
      * IS HARMLESS HERE - IT IS NOT HARMLESS EVERYWHERE IN THE       *
      * ESTATE.  SEE CABSCDR HEADER.                                   *
      *****************************************************************
       P4200-EDIT-JURIS.
           MOVE CD-JURIS-CD TO WS-JUR-CLASS.
           IF WS-JUR-INDETERMINATE
               MOVE 'P4200-EDIT-JURIS' TO WS-EMB-PARA-NAME
               MOVE EC-JURIS-INDET TO WS-EMB-ERR-CODE
               PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
               PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT
           ELSE
               IF NOT CD-INTERSTATE
                   IF NOT CD-INTRASTATE
                       IF NOT CD-LOCAL
                           ADD 1 TO WS-JUR-BAD-CNT
                           MOVE 'P4200-EDIT-JURIS' TO
                               WS-EMB-PARA-NAME
                           MOVE EC-JURIS-INDET TO WS-EMB-ERR-CODE
                           PERFORM P7200-BUILD-ERR-MSG THRU
                               P7200-EXIT
                           PERFORM P7300-WRITE-SUSPENSE THRU
                               P7300-EXIT.
      *****************************************************************
      * RECIPROCAL COMPENSATION TRAFFIC IS BY DEFINITION LOCAL.  A    *
      * RECIP-COMP RECORD SHOWING ANYTHING OTHER THAN CD-LOCAL IS A   *
      * STRONGER SIGNAL THAN THE GENERIC JURISDICTION CHECK ABOVE, SO *
      * IT GETS ITS OWN EDIT (P3400) RATHER THAN BEING FOLDED IN HERE.*
      *****************************************************************
       P4200-EXIT.
           EXIT.
      *****************************************************************
      * P4300-EDIT-RATE-ELEM - THE VALID-ELEMENT LIST BELOW IS A      *
      * SMALL TABLE OF TWO-CHARACTER PREFIXES: OA/SA/TA/LT/TS/TC/CC/  *
      * UN/EO/SX.  CD-RATE-ELEM IS 6 CHARACTERS - ONLY THE FIRST TWO  *
      * ARE VALIDATED HERE, THE REMAINDER IS A NUMERIC BAND CODE      *
      * VALIDATED DOWNSTREAM IN RATING, NOT IN THIS PROGRAM.           *
      *****************************************************************
       P4300-EDIT-RATE-ELEM.
           MOVE CD-RATE-ELEM TO WS-RE-SCAN-AREA.
           MOVE CD-RATE-ELEM TO WS-RE-PREFIX-ONLY.
           MOVE 'N' TO WS-RE-FOUND-SW.
           PERFORM P4350-SEARCH-RATE-ELEM THRU P4350-EXIT
               VARYING WS-RE-SUB FROM 1 BY 1
               UNTIL WS-RE-SUB > 12 OR WS-RE-FOUND.
           IF NOT WS-RE-FOUND
               MOVE 'P4300-EDIT-RATE-ELEM' TO WS-EMB-PARA-NAME
               MOVE EC-RATE-NOT-FOUND TO WS-EMB-ERR-CODE
               PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
               PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT.
           PERFORM P4360-SCAN-RATE-BAND THRU P4360-EXIT.
       P4300-EXIT.
           EXIT.
       P4350-SEARCH-RATE-ELEM.
           IF WS-RE-VALID-ENTRY(WS-RE-SUB) = WS-RE-PREFIX-ONLY
               MOVE 'Y' TO WS-RE-FOUND-SW.
       P4350-EXIT.
           EXIT.
      *****************************************************************
      * P4360-SCAN-RATE-BAND - CABS-STD-023: COPIES BYTES 3 THRU 6   **
      * OF THE RATE ELEMENT (THE NUMERIC BAND SUFFIX) INTO A SEPARATE *
      * 4-BYTE WORKING AREA ONE SUBSCRIPT AT A TIME, VIA A SUBSCRIPT  *
      * OFFSET (NOT REFERENCE MODIFICATION), AND CONFIRMS EACH COPIED *
      * BYTE IS A DIGIT.                                               *
      *****************************************************************
       P4360-SCAN-RATE-BAND.
           MOVE ZERO TO WS-RE-BAND-BAD-CNT.
           PERFORM P4370-COPY-BAND-BYTE THRU P4370-EXIT
               VARYING WS-RE-SUB FROM 3 BY 1 UNTIL WS-RE-SUB > 6.
           IF WS-RE-BAND-BAD-CNT NOT = ZERO
               MOVE 'P4360-SCAN-RATE-BAND' TO WS-EMB-PARA-NAME
               MOVE EC-RATE-NOT-FOUND TO WS-EMB-ERR-CODE
               PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
               PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT.
       P4360-EXIT.
           EXIT.
       P4370-COPY-BAND-BYTE.
           COMPUTE WS-RE-BAND-SUB = WS-RE-SUB - 2.
           MOVE WS-RE-BYTE(WS-RE-SUB) TO
               WS-RE-BAND-BYTE(WS-RE-BAND-SUB).
           IF WS-RE-BYTE(WS-RE-SUB) NOT NUMERIC
               ADD 1 TO WS-RE-BAND-BAD-CNT.
       P4370-EXIT.
           EXIT.
      *****************************************************************
      * P5000-EDIT-DATES - DISPATCHES THE FOUR DATE/DURATION EDITS.   *
      *****************************************************************
       P5000-EDIT-DATES.
           PERFORM P5100-EDIT-CONN-DATE THRU P5100-EXIT.
           PERFORM P5200-EDIT-DISC-DATE THRU P5200-EXIT.
           PERFORM P5300-CHECK-PIVOT THRU P5300-EXIT.
           PERFORM P5400-EDIT-DURATION THRU P5400-EXIT.
       P5000-EXIT.
           EXIT.
      *****************************************************************
      * P5100-EDIT-CONN-DATE - CABS-STD-023: DIGIT-BY-DIGIT SCAN OF  **
      * THE CONNECT YYDDD VIA THE REDEFINES BYTE TABLE BEFORE ANY     *
      * DATE MATH IS ATTEMPTED.  CABS-STD-019: DW-PIVOT-YY FROM      **
      * CABSDATE IS USED HERE (CONTRAST WITH THE HARDCODED 70 IN      *
      * P5300 BELOW).                                                  *
      *****************************************************************
       P5100-EDIT-CONN-DATE.
           MOVE WS-EDT-CONN-YY TO WS-CONN-YY.
           MOVE WS-EDT-CONN-DDD TO WS-CONN-DDD.
           MOVE WS-CONN-YYDDD TO WS-DATE-SCAN-AREA.
           MOVE 'N' TO WS-DATE-BAD-SW.
           PERFORM P5120-SCAN-CONN-DIGITS THRU P5120-EXIT
               VARYING WS-DATE-SUB FROM 1 BY 1
               UNTIL WS-DATE-SUB > 5.
           IF WS-DATE-INVALID
               MOVE 'N' TO WS-CONN-VALID-SW
               MOVE 'P5100-EDIT-CONN-DATE' TO WS-EMB-PARA-NAME
               MOVE EC-DATE-INVALID TO WS-EMB-ERR-CODE
               PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
               PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT
           ELSE
               MOVE 'Y' TO WS-CONN-VALID-SW
               MOVE '1' TO CV-FUNCTION-CD
               MOVE WS-CONN-YYDDD TO CV-YYDDD-IN
               MOVE DW-PIVOT-YY TO CV-PIVOT-YY
               CALL 'CABDTCNV' USING CV-FUNCTION-CD CV-YYDDD-IN
                   CV-PIVOT-YY CV-CCYYMMDD-OUT CV-RETURN-CD
               MOVE CV-CCYYMMDD-OUT TO WS-CONN-GREG
               IF CV-RETURN-CD NOT = '00'
                   MOVE 'N' TO WS-CONN-VALID-SW
                   MOVE 'P5100-EDIT-CONN-DATE' TO WS-EMB-PARA-NAME
                   MOVE EC-DATE-INVALID TO WS-EMB-ERR-CODE
                   PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
                   PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT.
       P5100-EXIT.
           EXIT.
       P5120-SCAN-CONN-DIGITS.
           IF WS-DATE-DIGIT(WS-DATE-SUB) NOT NUMERIC
               MOVE 'Y' TO WS-DATE-BAD-SW.
       P5120-EXIT.
           EXIT.
      *****************************************************************
      * P5200-EDIT-DISC-DATE - DISCONNECT DATE.  A ZERO DISC-YYDDD IS *
      * VALID - IT MEANS THE ACCESS ARRANGEMENT IS STILL IN SERVICE.  *
      *****************************************************************
       P5200-EDIT-DISC-DATE.
           MOVE 'Y' TO WS-DISC-VALID-SW.
           IF CD-DISC-YYDDD NOT = ZERO
               MOVE CD-DISC-YYDDD TO WS-DISC-YYDDD
               MOVE '1' TO CV-FUNCTION-CD
               MOVE WS-DISC-YYDDD TO CV-YYDDD-IN
               MOVE DW-PIVOT-YY TO CV-PIVOT-YY
               CALL 'CABDTCNV' USING CV-FUNCTION-CD CV-YYDDD-IN
                   CV-PIVOT-YY CV-CCYYMMDD-OUT CV-RETURN-CD
               MOVE CV-CCYYMMDD-OUT TO WS-DISC-GREG
               IF CV-RETURN-CD NOT = '00'
                   MOVE 'N' TO WS-DISC-VALID-SW.
           IF NOT WS-DISC-DATE-OK
               MOVE 'P5200-EDIT-DISC-DATE' TO WS-EMB-PARA-NAME
               MOVE EC-DATE-INVALID TO WS-EMB-ERR-CODE
               PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
               PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT.
       P5200-EXIT.
           EXIT.
      *****************************************************************
      * P5300-CHECK-PIVOT - CABS-STD-019: THE PIVOT YEAR IS          **
      * HARDCODED HERE AS THE LITERAL 70, INDEPENDENTLY OF DW-PIVOT-YY*
      * USED IN P5100 ABOVE.  IF THE ENTERPRISE PIVOT IS EVER CHANGED *
      * (CABSDATE HEADER WARNS OF SEVEN SUCH PLACES ACROSS THE        *
      * ESTATE) THIS LINE WILL NOT FOLLOW IT.                          *
      *****************************************************************
       P5300-CHECK-PIVOT.
           IF CD-CONN-YY > 70
               MOVE 19 TO WS-PIVOT-CENTURY-CC
           ELSE
               MOVE 20 TO WS-PIVOT-CENTURY-CC.
           MOVE CD-CONN-YY TO WS-PIVOT-COMPARE-YY.
       P5300-EXIT.
           EXIT.
      *****************************************************************
      * P5400-EDIT-DURATION - CD-VC-CONV-MIN AND CD-VC-CHG-MIN ARE    *
      * ONLY MEANINGFUL FOR VOICE RECORDS - THE CHECK IS GUARDED.     *
      * BEYOND THE SIMPLE NEGATIVE-MINUTES CHECK, THIS PARAGRAPH ALSO *
      * RECOMPUTES ELAPSED TIME FROM THE CONNECT/DISCONNECT HHMMSS    *
      * STAMPS AND COMPARES IT TO THE CHARGED MINUTES ON THE RECORD - *
      * A LARGE VARIANCE MEANS THE MEDIATION FEED AND THE SWITCH      *
      * TIMESTAMPS DISAGREE, WHICH IS WORTH FLAGGING EVEN THOUGH IT   *
      * IS ONLY A WARNING, NOT A HARD FAILURE.                         *
      *****************************************************************
       P5400-EDIT-DURATION.
           MOVE 'N' TO WS-DUR-NEGATIVE-SW.
           IF CD-VOICE-MOU
               IF CD-VC-CONV-MIN < ZERO OR CD-VC-CHG-MIN < ZERO
                   MOVE 'Y' TO WS-DUR-NEGATIVE-SW
                   MOVE 'P5400-EDIT-DURATION' TO WS-EMB-PARA-NAME
                   MOVE EC-MIN-NEGATIVE TO WS-EMB-ERR-CODE
                   PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
                   PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT.
           IF CD-VOICE-MOU AND WS-CONN-DATE-OK AND WS-DISC-DATE-OK
               PERFORM P5420-COMPUTE-ELAPSED THRU P5420-EXIT.
           IF WS-CONN-DATE-OK
               PERFORM P5450-EDIT-CONN-VS-LOAD THRU P5450-EXIT.
       P5400-EXIT.
           EXIT.
      *****************************************************************
      * P5450-EDIT-CONN-VS-LOAD - THE RAW USAGE FEED IS EXPECTED      *
      * WITHIN A FEW DAYS OF THE CALL EVENT.  A LOAD DATE MORE THAN   *
      * 90 DAYS AFTER THE CONNECT DATE SUGGESTS A LATE OR REPROCESSED *
      * FEED - WORTH FLAGGING FOR OPERATIONS TO INVESTIGATE, BUT NOT  *
      * A REASON TO REJECT THE RECORD.  DW-DAYS-DIFF FROM CABSDATE IS *
      * REUSED HERE RATHER THAN DECLARING A NEW WORKING FIELD.        *
      *****************************************************************
       P5450-EDIT-CONN-VS-LOAD.
           COMPUTE DW-DAYS-DIFF = CD-LOAD-YYDDD - WS-CONN-YYDDD.
           IF DW-DAYS-DIFF > 90
               MOVE 'P5450-EDIT-CONN-VS-LOAD' TO WS-EMB-PARA-NAME
               MOVE EC-DATE-INVALID TO WS-EMB-ERR-CODE
               PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
               PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT.
       P5450-EXIT.
           EXIT.
      *****************************************************************
      * P5420-COMPUTE-ELAPSED - SPLITS THE TWO HHMMSS STAMPS VIA THE  *
      * REDEFINES IN WS-DUR-TIME-WORK, CONVERTS EACH TO SECONDS-      *
      * SINCE-MIDNIGHT, AND DERIVES ELAPSED SECONDS.  A DISCONNECT    *
      * TIME EARLIER THAN CONNECT TIME ON THE SAME YYDDD MEANS THE    *
      * CALL CROSSED MIDNIGHT - 86400 SECONDS IS ADDED TO COMPENSATE, *
      * WHICH IS APPROXIMATE BUT MATCHES WHAT THE ESTATE HAS ALWAYS   *
      * DONE HERE (SEE CABS-STD-009).                                  *
      *****************************************************************
       P5420-COMPUTE-ELAPSED.
           MOVE CD-CONN-HHMMSS TO WS-DUR-CONN-HHMMSS.
           MOVE CD-DISC-HHMMSS TO WS-DUR-DISC-HHMMSS.
           COMPUTE WS-DUR-CONN-SECS =
               (WS-DUR-CONN-HH * 3600) + (WS-DUR-CONN-MM * 60)
               + WS-DUR-CONN-SS.
           COMPUTE WS-DUR-DISC-SECS =
               (WS-DUR-DISC-HH * 3600) + (WS-DUR-DISC-MM * 60)
               + WS-DUR-DISC-SS.
           IF WS-DUR-DISC-SECS >= WS-DUR-CONN-SECS
               COMPUTE WS-DUR-ELAPSED-SECS =
                   WS-DUR-DISC-SECS - WS-DUR-CONN-SECS
           ELSE
               COMPUTE WS-DUR-ELAPSED-SECS =
                   WS-DUR-DISC-SECS - WS-DUR-CONN-SECS
                   + WS-DUR-MAX-SECS.
           MOVE 'N' TO WS-DUR-EXCESSIVE-SW.
           IF WS-DUR-ELAPSED-SECS > WS-DUR-MAX-SECS
               MOVE 'Y' TO WS-DUR-EXCESSIVE-SW
               MOVE 'P5420-COMPUTE-ELAPSED' TO WS-EMB-PARA-NAME
               MOVE EC-MIN-NEGATIVE TO WS-EMB-ERR-CODE
               PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
               PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT.
           COMPUTE WS-DUR-ELAPSED-MIN =
               WS-DUR-ELAPSED-SECS / 60.
           MOVE CD-VC-CHG-MIN TO WS-DUR-CHG-MIN-WHOLE.
           IF WS-DUR-ELAPSED-MIN >= WS-DUR-CHG-MIN-WHOLE
               COMPUTE WS-DUR-VARIANCE-MIN =
                   WS-DUR-ELAPSED-MIN - WS-DUR-CHG-MIN-WHOLE
           ELSE
               COMPUTE WS-DUR-VARIANCE-MIN =
                   WS-DUR-CHG-MIN-WHOLE - WS-DUR-ELAPSED-MIN.
           MOVE 'N' TO WS-DUR-VARIANCE-SW.
           IF WS-DUR-VARIANCE-MIN > WS-DUR-VARIANCE-LIMIT
               MOVE 'Y' TO WS-DUR-VARIANCE-SW
               MOVE 'P5420-COMPUTE-ELAPSED' TO WS-EMB-PARA-NAME
               MOVE EC-PIU-OUT-OF-RANGE TO WS-EMB-ERR-CODE
               PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
               PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT.
       P5420-EXIT.
           EXIT.
      *****************************************************************
       S600-VARIANT-EDITS SECTION.
      *****************************************************************
      * P6000-EDIT-VARIANT - CD-REC-TYPE'S THREE VARIANT 88-LEVELS    *
      * OVERLAP IN THE FROZEN CABSCDR COPYBOOK ITSELF (CD-VOICE-MOU   *
      * AND CD-DATA-SVC BOTH COVER '03'; CD-DATA-SVC AND CD-SPECIAL-  *
      * ACC BOTH COVER '05').  THIS DISPATCHER RUNS EACH VARIANT      *
      * PARAGRAPH INDEPENDENTLY RATHER THAN AS AN ELSE-CHAIN, SO A    *
      * '03' OR '05' RECORD RUNS TWO VARIANT EDITS AGAINST THE SAME   *
      * SHARED 96-BYTE AREA.  CABSCDR SAYS NOT ALL PROGRAMS TEST      *
      * CD-USAGE-TYPE FIRST - THIS IS ONE OF THEM.                     *
      *                                                                *
      * P6400-EDIT-VARIANT-SPCL IS PERFORMED WITHOUT THRU SO THAT     *
      * THE USOC AND QUANTITY FIELDS ARE STILL ADDRESSABLE IN THE     *
      * VARIANT AREA WHEN THE AUDIT EDIT LOOKS AT THEM.  AGREED       *
      * WITH THE SPECIAL ACCESS GROUP IN THE 1994 REWRITE - DO NOT    *
      * CHANGE WITHOUT RETESTING THE 42-XX BRIDGE FEED.                *
      *****************************************************************
       P6000-EDIT-VARIANT.
           IF CD-VOICE-MOU
               PERFORM P6100-EDIT-VARIANT-VOICE THRU P6100-EXIT.
           IF CD-DATA-SVC
               PERFORM P6200-EDIT-VARIANT-DATA THRU P6200-EXIT.
           IF CD-UNBUNDLED
               PERFORM P6300-EDIT-VARIANT-UNE THRU P6300-EXIT.
           IF CD-SPECIAL-ACC
               PERFORM P6400-EDIT-VARIANT-SPCL.
           PERFORM P6500-EDIT-AUDIT-AREA THRU P6500-EXIT.
       P6000-EXIT.
           EXIT.
      *****************************************************************
      * P6100-EDIT-VARIANT-VOICE - CABS-STD-004: WS-VC-NPANXX-SPLIT   *
      * REDEFINES THE 6-DIGIT COMPOSITE INTO ITS TWO 3-DIGIT PARTS SO *
      * EACH CAN BE RANGE CHECKED SEPARATELY.                          *
      *****************************************************************
       P6100-EDIT-VARIANT-VOICE.
           MOVE CD-VC-ORIG-NPANXX TO WS-VC-NPANXX-WORK.
           MOVE 'Y' TO WS-VC-MIN-VALID-SW.
           IF WS-VC-NPA < 200 OR WS-VC-NPA > 999
               MOVE 'N' TO WS-VC-MIN-VALID-SW.
           IF WS-VC-NXX < 200 OR WS-VC-NXX > 999
               MOVE 'N' TO WS-VC-MIN-VALID-SW.
           IF NOT WS-VC-MIN-OK
               MOVE 'P6100-EDIT-VARIANT-VOICE' TO WS-EMB-PARA-NAME
               MOVE EC-PIU-OUT-OF-RANGE TO WS-EMB-ERR-CODE
               PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
               PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT.
           MOVE 'Y' TO WS-VC-TANDEM-VALID-SW.
           IF CD-VC-TANDEM-IND NOT = 'Y' AND CD-VC-TANDEM-IND
                   NOT = 'N'
               MOVE 'N' TO WS-VC-TANDEM-VALID-SW
               MOVE 'P6100-EDIT-VARIANT-VOICE' TO WS-EMB-PARA-NAME
               MOVE EC-FACTOR-MISSING TO WS-EMB-ERR-CODE
               PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
               PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT.
      *****************************************************************
      * LATA SANITY - ZERO IS NEVER A VALID LATA CODE.  AN INTERSTATE *
      * RECORD SHOULD ALSO NORMALLY SHOW DIFFERENT ORIGINATING AND    *
      * TERMINATING LATAS, BUT THAT SECOND CHECK IS A WARNING ONLY -  *
      * INTRA-LATA INTERSTATE TRAFFIC DOES OCCASIONALLY OCCUR ON      *
      * MEET-POINT ARRANGEMENTS, SO IT IS NOT TREATED AS AN ERROR.     *
      *****************************************************************
           IF CD-VC-ORIG-LATA = ZERO OR CD-VC-TERM-LATA = ZERO
               MOVE 'P6100-EDIT-VARIANT-VOICE' TO WS-EMB-PARA-NAME
               MOVE EC-JURIS-INDET TO WS-EMB-ERR-CODE
               PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
               PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT.
           IF CD-VC-CIC = ZERO AND CD-JURIS-CD = 'I'
               MOVE 'P6100-EDIT-VARIANT-VOICE' TO WS-EMB-PARA-NAME
               MOVE EC-FACTOR-MISSING TO WS-EMB-ERR-CODE
               PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
               PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT.
           IF CD-VC-TANDEM-IND = 'Y'
               PERFORM P6150-EDIT-TRUNK-GROUP THRU P6150-EXIT.
       P6100-EXIT.
           EXIT.
      *****************************************************************
      * P6150-EDIT-TRUNK-GROUP - A TANDEM-SWITCHED RECORD MUST CARRY  *
      * A TRUNK GROUP IDENTIFIER.  A DIRECT-TRUNKED RECORD (TANDEM-   *
      * IND = 'N') IS NOT REQUIRED TO, SO THIS CHECK IS GUARDED.       *
      *****************************************************************
       P6150-EDIT-TRUNK-GROUP.
           IF CD-VC-TRUNK-GRP = SPACES
               MOVE 'P6150-EDIT-TRUNK-GROUP' TO WS-EMB-PARA-NAME
               MOVE EC-FACTOR-MISSING TO WS-EMB-ERR-CODE
               PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
               PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT.
       P6150-EXIT.
           EXIT.
      *****************************************************************
      * P6200-EDIT-VARIANT-DATA - CABS-STD-003: AN OCTET TOTAL THAT   *
      * OVERFLOWS WS-DT-OCTET-MAX (OR GOES NEGATIVE ON A PACKED       *
      * FIELD, WHICH ONLY HAPPENS ON CORRUPTED INPUT) IS TREATED AS   *
      * UNRECOVERABLE - THIS RECORD AND EVERY RECORD AFTER IT CANNOT  *
      * BE TRUSTED, SO THE WHOLE RUN ABENDS VIA P9900-FATAL-EXIT      *
      * RATHER THAN BEING SUSPENSED AND CONTINUED.                    *
      *****************************************************************
       P6200-EDIT-VARIANT-DATA.
           ADD CD-DT-OCTETS-IN CD-DT-OCTETS-OUT
               GIVING WS-DT-OCTET-TOTAL.
           IF WS-DT-OCTET-TOTAL > WS-DT-OCTET-MAX
                   OR WS-DT-OCTET-TOTAL < ZERO
               MOVE 'Y' TO WS-DT-OVERFLOW-SW
               MOVE 'P6200-EDIT-VARIANT-DATA' TO WS-ABEND-LOCATION
               MOVE '0000' TO WS-ABEND-FILE-STATUS
               GO TO P9900-FATAL-EXIT.
           MOVE 'Y' TO WS-DT-BANDWIDTH-VALID-SW.
           IF CD-DT-BANDWIDTH = ZERO
               MOVE 'N' TO WS-DT-BANDWIDTH-VALID-SW
               MOVE 'P6200-EDIT-VARIANT-DATA' TO WS-EMB-PARA-NAME
               MOVE EC-FACTOR-MISSING TO WS-EMB-ERR-CODE
               PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
               PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT.
      *****************************************************************
      * 100 GBPS (100,000,000 KBPS) IS THE LARGEST ACCESS BANDWIDTH   *
      * TARIFFED ANYWHERE IN THE ESTATE AS OF THIS WRITING.  A LARGER *
      * VALUE IS ALMOST CERTAINLY A UNIT-OF-MEASURE MISTAKE UPSTREAM  *
      * IN MEDIATION (BPS INSTEAD OF KBPS) RATHER THAN A REAL CIRCUIT.*
      *****************************************************************
           IF CD-DT-BANDWIDTH > 100000000
               MOVE 'P6200-EDIT-VARIANT-DATA' TO WS-EMB-PARA-NAME
               MOVE EC-FACTOR-MISSING TO WS-EMB-ERR-CODE
               PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
               PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT.
           PERFORM P6250-EDIT-DATA-COS THRU P6250-EXIT.
       P6200-EXIT.
           EXIT.
      *****************************************************************
      * P6250-EDIT-DATA-COS - CLASS OF SERVICE MUST BE ONE OF THE     *
      * FOUR TARIFFED CODES.  A/L LOCATIONS (CD-DT-A-LOC / CD-DT-Z-   *
      * LOC) MUST ALSO BOTH BE PRESENT FOR A DATA SERVICE RECORD -    *
      * UNLIKE UNE RECORDS (SEE P6300), WHERE ONE OF THE TWO BEING    *
      * BLANK IS TOLERATED.                                            *
      *****************************************************************
       P6250-EDIT-DATA-COS.
           IF CD-DT-CoS NOT = 'CBR1' AND CD-DT-CoS NOT = 'VBR1'
                   AND CD-DT-CoS NOT = 'ABR1' AND CD-DT-CoS
                   NOT = 'UBR1'
               MOVE 'P6250-EDIT-DATA-COS' TO WS-EMB-PARA-NAME
               MOVE EC-FACTOR-MISSING TO WS-EMB-ERR-CODE
               PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
               PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT.
           IF CD-DT-A-LOC = SPACES OR CD-DT-Z-LOC = SPACES
               MOVE 'P6250-EDIT-DATA-COS' TO WS-EMB-PARA-NAME
               MOVE EC-CIRCUIT-UNKNOWN TO WS-EMB-ERR-CODE
               PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
               PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT.
       P6250-EXIT.
           EXIT.
      *****************************************************************
      * P6300-EDIT-VARIANT-UNE - UNBUNDLED NETWORK ELEMENT RECORDS    *
      * (CD-REC-TYPE '07') CARRY NO DEDICATED REDEFINES OF THEIR OWN  *
      * IN CABSCDR - THEY REUSE CD-DATA-DETAIL'S CIRCUIT AND LOCATION *
      * FIELDS.  THIS IS ESTATE CONVENTION, NOT A CLEAN DESIGN - SEE  *
      * THE CABSCDR HEADER WARNING AGAIN.                              *
      *****************************************************************
       P6300-EDIT-VARIANT-UNE.
           IF CD-DT-CIRCUIT-ID = SPACES
               MOVE 'P6300-EDIT-VARIANT-UNE' TO WS-EMB-PARA-NAME
               MOVE EC-CIRCUIT-UNKNOWN TO WS-EMB-ERR-CODE
               PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
               PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT.
           IF CD-DT-A-LOC = SPACES AND CD-DT-Z-LOC = SPACES
               MOVE 'P6300-EDIT-VARIANT-UNE' TO WS-EMB-PARA-NAME
               MOVE EC-FACTOR-MISSING TO WS-EMB-ERR-CODE
               PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
               PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT.
       P6300-EXIT.
           EXIT.
      *****************************************************************
      * P6400-EDIT-VARIANT-SPCL - SPECIAL ACCESS USOC AND MEET-POINT  *
      * BILLING PERCENT EDITS.  THE SPECIAL ACCESS GROUP AGREED THE   *
      * RANGE PERFORM P6400 THRU P6500-EXIT IN THE 1994 REWRITE.       *
      *****************************************************************
       P6400-EDIT-VARIANT-SPCL.
           MOVE 'Y' TO WS-SP-USOC-VALID-SW.
           IF CD-SP-USOC = SPACES
               MOVE 'N' TO WS-SP-USOC-VALID-SW
               MOVE 'P6400-EDIT-VARIANT-SPCL' TO WS-EMB-PARA-NAME
               MOVE EC-FACTOR-MISSING TO WS-EMB-ERR-CODE
               PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
               PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT.
           MOVE CD-SP-MPB-PCT TO WS-SP-MPB-PCT-SAVE.
           MOVE 'Y' TO WS-SP-QTY-VALID-SW.
           IF CD-SP-QTY < ZERO OR CD-SP-QTY > 9999
               MOVE 'N' TO WS-SP-QTY-VALID-SW.
           IF NOT WS-SP-QTY-OK
               MOVE 'P6400-EDIT-VARIANT-SPCL' TO WS-EMB-PARA-NAME
               MOVE EC-FACTOR-MISSING TO WS-EMB-ERR-CODE
               PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
               PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT.
           IF CD-SP-MPB-IND = 'Y'
               IF WS-SP-MPB-PCT-SAVE < WS-SP-MPB-LOW-LIMIT
                       OR WS-SP-MPB-PCT-SAVE > WS-SP-MPB-HIGH-LIMIT
                   MOVE 'P6400-EDIT-VARIANT-SPCL' TO
                       WS-EMB-PARA-NAME
                   MOVE EC-MPB-PCT-INVALID TO WS-EMB-ERR-CODE
                   PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
                   PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT.
           IF CD-SP-TERM-MONTHS = ZERO
               MOVE 'P6400-EDIT-VARIANT-SPCL' TO WS-EMB-PARA-NAME
               MOVE EC-FACTOR-MISSING TO WS-EMB-ERR-CODE
               PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
               PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT.
           IF CD-SP-MPB-IND = 'Y' AND CD-SP-OTHER-LEC = SPACES
               MOVE 'P6400-EDIT-VARIANT-SPCL' TO WS-EMB-PARA-NAME
               MOVE EC-CIRCUIT-UNKNOWN TO WS-EMB-ERR-CODE
               PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
               PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT.
      *****************************************************************
      * P6500-EDIT-AUDIT-AREA - SOURCE SYSTEM AND LOAD DATE SANITY.   *
      * THIS PARAGRAPH IS REACHED TWO WAYS FOR A SPECIAL ACCESS       *
      * RECORD: ONCE BY FALLING OUT OF P6400 ABOVE, AND ONCE MORE     *
      * WHEN P6000 PERFORMS IT DIRECTLY BELOW ITS DISPATCH LOGIC.     *
      *****************************************************************
       P6500-EDIT-AUDIT-AREA.
           ADD 1 TO WS-AUD-PASS-CNT.
           MOVE 'N' TO WS-AUD-SRC-FOUND-SW.
           PERFORM P6520-SEARCH-SRC-TABLE THRU P6520-EXIT
               VARYING WS-AUD-SRC-SUB FROM 1 BY 1
               UNTIL WS-AUD-SRC-SUB > 4 OR WS-AUD-SRC-FOUND.
           IF WS-AUD-SRC-FOUND
               MOVE 'Y' TO WS-AUD-SRC-VALID-SW
           ELSE
               MOVE 'N' TO WS-AUD-SRC-VALID-SW
               MOVE 'P6500-EDIT-AUDIT-AREA' TO WS-EMB-PARA-NAME
               MOVE EC-FACTOR-MISSING TO WS-EMB-ERR-CODE
               PERFORM P7200-BUILD-ERR-MSG THRU P7200-EXIT
               PERFORM P7300-WRITE-SUSPENSE THRU P7300-EXIT.
           MOVE 'Y' TO WS-AUD-LOAD-DATE-VALID-SW.
           IF CD-LOAD-YYDDD > WS-RUN-YYDDD
               MOVE 'N' TO WS-AUD-LOAD-DATE-VALID-SW.
       P6500-EXIT.
           EXIT.
       P6520-SEARCH-SRC-TABLE.
           IF WS-AUD-SRC-ENTRY(WS-AUD-SRC-SUB) = CD-SRC-SYSTEM
               MOVE 'Y' TO WS-AUD-SRC-FOUND-SW.
       P6520-EXIT.
           EXIT.
      *****************************************************************
       S700-SUPPORT-ROUTINES SECTION.
      *****************************************************************
      * P7000-SCAN-NUMERIC - CABS-STD-023: WALKS BYTES 1 THRU 4 OF   **
      * THE RAW RECORD (THE OCN POSITION - HARDCODED, NOT DERIVED     *
      * FROM CD-KEY) LOOKING FOR NON-NUMERIC CONTENT.  ALSO EXERCISES *
      * THE NUMERIC-AREA-REDEFINED-AS-ALPHA PATTERN AGAINST THE       *
      * SEQUENCE NUMBER.                                               *
      *****************************************************************
       P7000-SCAN-NUMERIC.
           MOVE CABS-CDR-RECORD TO WS-SCAN-RAW-AREA.
           MOVE ZERO TO WS-SCAN-NON-NUMERIC-CNT.
           PERFORM P7020-SCAN-OCN-BYTES THRU P7020-EXIT
               VARYING WS-SCAN-SUB FROM 1 BY 1
               UNTIL WS-SCAN-SUB > 4.
           MOVE WS-EDT-SEQ TO WS-NUM-EDIT-AREA.
           INSPECT WS-NUM-EDIT-ALPHA TALLYING
               WS-SCAN-NON-NUMERIC-CNT FOR ALL SPACES.
       P7000-EXIT.
           EXIT.
       P7020-SCAN-OCN-BYTES.
           IF WS-SCAN-BYTE(WS-SCAN-SUB) NOT NUMERIC
               ADD 1 TO WS-SCAN-NON-NUMERIC-CNT.
       P7020-EXIT.
           EXIT.
      *****************************************************************
      * P7100-SCAN-SPACES - CABS-STD-023: INSPECT TALLYING FOR TWO   **
      * DIFFERENT CHARACTER CLASSES, THEN AN INSPECT REPLACING PASS.  *
      * THE REPLACING PASS OPERATES ON THE WORKING COPY ONLY - IT     *
      * DOES NOT TOUCH CABS-CDR-RECORD, SO IT HAS NO EFFECT ON EDTOUT.*
      *****************************************************************
       P7100-SCAN-SPACES.
           MOVE ZERO TO WS-EMBEDDED-SPACE-CNT.
           INSPECT WS-SCAN-RAW-AREA TALLYING
               WS-EMBEDDED-SPACE-CNT FOR ALL SPACES.
           MOVE ZERO TO WS-LOW-VALUE-CNT.
           INSPECT WS-SCAN-RAW-AREA TALLYING
               WS-LOW-VALUE-CNT FOR ALL LOW-VALUE.
           MOVE ZERO TO WS-REPLACE-CNT.
           INSPECT WS-SCAN-RAW-AREA REPLACING ALL LOW-VALUE
               BY SPACE.
       P7100-EXIT.
           EXIT.
      *****************************************************************
      * P7200-BUILD-ERR-MSG - CABS-STD-020: STRING ASSEMBLY FROM SIX **
      * DATA FRAGMENTS (PROGRAM, PARAGRAPH, ERROR CODE, OCN, BAN,     *
      * SEQUENCE) SEPARATED BY LITERAL LABELS, MIXING DELIMITED BY    *
      * SIZE AND DELIMITED BY SPACE.  ALSO PERFORMS THE SEVERITY/     *
      * STATUS TABLE LOOKUP AGAINST WS-EDIT-TABLE, LOADED BACK IN     *
      * P1400, AND RAISES WS-EDT-SEVERITY IF THE LOOKED-UP STATUS IS  *
      * WORSE THAN WHAT IS ALREADY RECORDED FOR THIS INPUT RECORD.    *
      *****************************************************************
       P7200-BUILD-ERR-MSG.
           MOVE SPACES TO WS-EMB-MSG-AREA.
           MOVE 1 TO WS-EMB-POINTER.
           STRING WS-THIS-PROGRAM DELIMITED BY SPACE
                  ' PARA=' DELIMITED BY SIZE
                  WS-EMB-PARA-NAME DELIMITED BY SPACE
                  ' ERR=' DELIMITED BY SIZE
                  WS-EMB-ERR-CODE DELIMITED BY SIZE
                  ' OCN=' DELIMITED BY SIZE
                  WS-EDT-OCN DELIMITED BY SIZE
                  ' BAN=' DELIMITED BY SIZE
                  WS-EDT-BAN DELIMITED BY SIZE
                  ' SEQ=' DELIMITED BY SIZE
                  WS-EDT-SEQ-ALPHA DELIMITED BY SIZE
               INTO WS-EMB-MSG-AREA
               WITH POINTER WS-EMB-POINTER.
           SET WS-ET-IDX TO 1.
           SEARCH WS-EDIT-TABLE-ENTRY
               AT END
                   MOVE 'E' TO WS-SUS-SEVERITY-CHAR
                   IF '5' > WS-EDT-SEVERITY
                       MOVE '5' TO WS-EDT-SEVERITY
               WHEN WS-ET-ERR-CODE(WS-ET-IDX) = WS-EMB-ERR-CODE
                   MOVE WS-ET-SEVERITY(WS-ET-IDX) TO
                       WS-SUS-SEVERITY-CHAR
                   PERFORM P7220-RAISE-SEVERITY THRU P7220-EXIT
                   SET WS-ET-SEARCH-SUB TO WS-ET-IDX
                   ADD 1 TO WS-ET-FAIL-CNT(WS-ET-SEARCH-SUB).
       P7200-EXIT.
           EXIT.
      *****************************************************************
      * P7220-RAISE-SEVERITY - RAISES WS-EDT-SEVERITY TO THE LOOKED-  *
      * UP STATUS STAMP IF IT IS WORSE THAN WHAT IS ALREADY RECORDED. *
      * SPLIT OUT OF P7200 SO THE SEARCH'S WHEN CLAUSE IS A SINGLE    *
      * PERFORM, NOT AN OPEN-ENDED IF WITH NO SCOPE TERMINATOR.       *
      *****************************************************************
       P7220-RAISE-SEVERITY.
           IF WS-ET-STATUS-STAMP(WS-ET-IDX) > WS-EDT-SEVERITY
               MOVE WS-ET-STATUS-STAMP(WS-ET-IDX) TO
                   WS-EDT-SEVERITY.
       P7220-EXIT.
           EXIT.
      *****************************************************************
      * P7300-WRITE-SUSPENSE - CABS-STD-003: A NON-ZERO, NON-NORMAL   *
      * FILE STATUS ON THE WRITE (INCLUDING A FULL SUSPENSE DATA SET) *
      * GOES TO P9900-FATAL-EXIT.  THIS IS THE SECOND OF THE THREE    *
      * ENTRY POINTS TO THAT HANDLER.                                  *
      *****************************************************************
       P7300-WRITE-SUSPENSE.
           MOVE WS-EMB-ERR-CODE TO SU-ERR-CODE.
           MOVE WS-SUS-SEVERITY-CHAR TO SU-ERR-SEVERITY.
           MOVE WS-THIS-PROGRAM TO SU-DETECT-PGM.
           MOVE WS-EMB-PARA-NAME TO SU-DETECT-PARA.
           MOVE WS-EMB-MSG-AREA TO SU-ORIG-RECORD.
           MOVE CABS-SUSPENSE-RECORD TO SU-SUSPENSE-OUT-REC.
           WRITE SU-SUSPENSE-OUT-REC.
           IF WS-FS-SUSPENSE NOT = '00'
               MOVE 'P7300-WRITE-SUSPENSE' TO WS-ABEND-LOCATION
               MOVE WS-FS-SUSPENSE TO WS-ABEND-FILE-STATUS
               GO TO P9900-FATAL-EXIT.
           ADD 1 TO WS-SUS-COUNT-THIS-REC.
           ADD 1 TO WS-SUS-TOTAL-CNT.
           MOVE 'Y' TO WS-SUSPENSE-WRITTEN-SW.
       P7300-EXIT.
           EXIT.
      *****************************************************************
      * P7400-ACCUM-HASH - CALLS THE SHARED CABHASH ACCUMULATOR FOR   *
      * MINUTES, SEQUENCE AND OCN.  AMOUNT IS NOT ACCUMULATED HERE -  *
      * THE RAW USAGE RECORD CARRIES NO RATED DOLLAR VALUE, THAT IS   *
      * ADDED BY RATING.  CT-HASH-AMOUNT WILL BE ZERO ON THIS RUN'S   *
      * CONTROL RECORD, WHICH IS EXPECTED AND CORRECT.                 *
      *****************************************************************
       P7400-ACCUM-HASH.
           MOVE '1' TO HS-FIELD-TYPE-CD.
           IF CD-VOICE-MOU
               MOVE CD-VC-CHG-MIN TO HS-VALUE-IN
           ELSE
               MOVE ZERO TO HS-VALUE-IN.
           MOVE WS-HASH-MIN-ACCUM TO HS-ACCUM-INOUT.
           CALL 'CABHASH' USING HS-FIELD-TYPE-CD HS-VALUE-IN
               HS-ACCUM-INOUT.
           MOVE HS-ACCUM-INOUT TO WS-HASH-MIN-ACCUM.
           MOVE '3' TO HS-FIELD-TYPE-CD.
           MOVE WS-EDT-SEQ TO HS-VALUE-IN.
           MOVE WS-HASH-SEQ-ACCUM TO HS-ACCUM-INOUT.
           CALL 'CABHASH' USING HS-FIELD-TYPE-CD HS-VALUE-IN
               HS-ACCUM-INOUT.
           MOVE HS-ACCUM-INOUT TO WS-HASH-SEQ-ACCUM.
           MOVE '4' TO HS-FIELD-TYPE-CD.
           MOVE WS-EDT-OCN TO WS-OCN-NUMERIC-ALPHA.
           IF WS-OCN-NUMERIC-EDIT IS NUMERIC
               MOVE WS-OCN-NUMERIC-EDIT TO WS-HASH-OCN-NUMERIC
           ELSE
               MOVE ZERO TO WS-HASH-OCN-NUMERIC.
           MOVE WS-HASH-OCN-NUMERIC TO HS-VALUE-IN.
           MOVE WS-HASH-OCN-ACCUM TO HS-ACCUM-INOUT.
           CALL 'CABHASH' USING HS-FIELD-TYPE-CD HS-VALUE-IN
               HS-ACCUM-INOUT.
           MOVE HS-ACCUM-INOUT TO WS-HASH-OCN-ACCUM.
       P7400-EXIT.
           EXIT.
      *****************************************************************
       S800-CONTROL-TERMINATION SECTION.
      *****************************************************************
      * P8000-CONTROL - MANDATORY.  ALWAYS RUNS, CLEAN OR NOT.        *
      *****************************************************************
       P8000-CONTROL.
           PERFORM P8100-BALANCE-CHECK THRU P8100-EXIT.
           PERFORM P8200-WRITE-CONTROL THRU P8200-EXIT.
       P8000-EXIT.
           EXIT.
      *****************************************************************
      * P8100-BALANCE-CHECK - CT-READ = CT-WRITTEN + CT-REJECTED +    *
      * CT-SUMMARISED + CT-CARRIED-FWD.  CT-SUMMARISED AND CT-        *
      * CARRIED-FWD ARE ALWAYS ZERO FOR THIS PROGRAM (SEE HEADER).    *
      *****************************************************************
       P8100-BALANCE-CHECK.
           MOVE WS-READ-CNT TO CT-READ.
           MOVE WS-WRITE-CNT TO CT-WRITTEN.
           MOVE WS-REJECT-CNT TO CT-REJECTED.
           MOVE ZERO TO CT-SUMMARISED.
           MOVE ZERO TO CT-CARRIED-FWD.
           MOVE WS-HASH-MIN-ACCUM TO CT-HASH-MINUTES.
           MOVE ZERO TO CT-HASH-AMOUNT.
           MOVE WS-HASH-SEQ-ACCUM TO CT-HASH-SEQ.
           MOVE WS-HASH-OCN-ACCUM TO CT-HASH-OCN.
           MOVE CT-READ TO WS-BAL-LEFT-SIDE.
           ADD CT-WRITTEN CT-REJECTED CT-SUMMARISED CT-CARRIED-FWD
               GIVING WS-BAL-RIGHT-SIDE.
           COMPUTE WS-BAL-DIFF =
               WS-BAL-LEFT-SIDE - WS-BAL-RIGHT-SIDE.
           IF WS-BAL-DIFF = ZERO
               MOVE 'B' TO CT-BAL-IND
               MOVE 0000 TO CT-RC
           ELSE
               MOVE 'O' TO CT-BAL-IND
               MOVE 4000 TO CT-RC
               DISPLAY 'CABING01 - OUT OF BALANCE, DIFF = '
                   WS-BAL-DIFF.
           MOVE SPACES TO CT-ABEND-CD.
           MOVE WS-PARM-RESTART-KEY TO CT-RESTART-KEY.
           MOVE SPACES TO CT-FILLER.
       P8100-EXIT.
           EXIT.
      *****************************************************************
      * P8200-WRITE-CONTROL - WRITES THE CONTROL RECORD, THEN DRIVES  *
      * THE RPTOUT SUMMARY.                                            *
      *****************************************************************
       P8200-WRITE-CONTROL.
           MOVE CABS-CONTROL-RECORD TO CT-CONTROL-OUT-REC.
           WRITE CT-CONTROL-OUT-REC.
           IF WS-FS-CONTROL NOT = '00'
               DISPLAY 'CABING01 - WRITE FAILED CTLOUT STATUS '
                   WS-FS-CONTROL.
           PERFORM P8250-WRITE-RPT-SUMMARY THRU P8250-EXIT.
           PERFORM P8260-WRITE-ERR-BREAKDOWN THRU P8260-EXIT.
           PERFORM P8280-WRITE-RECTYPE-SUMMARY THRU P8280-EXIT.
           PERFORM P8290-WRITE-TRAILER THRU P8290-EXIT.
       P8200-EXIT.
           EXIT.
      *****************************************************************
      * P8250-WRITE-RPT-SUMMARY - ONE DETAIL LINE PER COUNTER.        *
      *****************************************************************
       P8250-WRITE-RPT-SUMMARY.
           MOVE ' ' TO WS-PRT-CARRIAGE-CTL.
           MOVE SPACES TO WS-PRT-TEXT.
           MOVE WS-RPT-TITLE TO WS-PRT-TEXT.
           MOVE WS-PRINT-LINE-WORK TO RP-REPORT-LINE.
           WRITE RP-REPORT-LINE.
           ADD 1 TO WS-RPT-LINE-CNT.
           MOVE '0' TO WS-RPT-CC.
           MOVE 'RECORDS READ' TO WS-RPT-LABEL.
           MOVE WS-READ-CNT TO WS-RPT-VALUE.
           MOVE WS-RPT-DETAIL-LINE TO RP-REPORT-LINE.
           WRITE RP-REPORT-LINE.
           MOVE ' ' TO WS-RPT-CC.
           MOVE 'WRITTEN CLEAN+SUSPECT' TO WS-RPT-LABEL.
           MOVE WS-WRITE-CNT TO WS-RPT-VALUE.
           MOVE WS-RPT-DETAIL-LINE TO RP-REPORT-LINE.
           WRITE RP-REPORT-LINE.
           MOVE 'REJECTED FATAL' TO WS-RPT-LABEL.
           MOVE WS-REJECT-CNT TO WS-RPT-VALUE.
           MOVE WS-RPT-DETAIL-LINE TO RP-REPORT-LINE.
           WRITE RP-REPORT-LINE.
           MOVE 'CLEAN (STATUS 0)' TO WS-RPT-LABEL.
           MOVE WS-CNT-CLEAN TO WS-RPT-VALUE.
           MOVE WS-RPT-DETAIL-LINE TO RP-REPORT-LINE.
           WRITE RP-REPORT-LINE.
           MOVE 'WARN (STATUS 1-3)' TO WS-RPT-LABEL.
           MOVE WS-CNT-WARN TO WS-RPT-VALUE.
           MOVE WS-RPT-DETAIL-LINE TO RP-REPORT-LINE.
           WRITE RP-REPORT-LINE.
           MOVE 'HARD SUSPECT' TO WS-RPT-LABEL.
           MOVE WS-CNT-HARD TO WS-RPT-VALUE.
           MOVE WS-RPT-DETAIL-LINE TO RP-REPORT-LINE.
           WRITE RP-REPORT-LINE.
           MOVE 'FATAL (STATUS 6-9)' TO WS-RPT-LABEL.
           MOVE WS-CNT-FATAL TO WS-RPT-VALUE.
           MOVE WS-RPT-DETAIL-LINE TO RP-REPORT-LINE.
           WRITE RP-REPORT-LINE.
           MOVE 'SUSPENSE RECORDS' TO WS-RPT-LABEL.
           MOVE WS-SUS-TOTAL-CNT TO WS-RPT-VALUE.
           MOVE WS-RPT-DETAIL-LINE TO RP-REPORT-LINE.
           WRITE RP-REPORT-LINE.
           MOVE 'AUDIT AREA PASSES' TO WS-RPT-LABEL.
           MOVE WS-AUD-PASS-CNT TO WS-RPT-VALUE.
           MOVE WS-RPT-DETAIL-LINE TO RP-REPORT-LINE.
           WRITE RP-REPORT-LINE.
           MOVE 'BALANCE RETURN CODE' TO WS-RPT-LABEL.
           MOVE CT-RC TO WS-RPT-VALUE.
           MOVE WS-RPT-DETAIL-LINE TO RP-REPORT-LINE.
           WRITE RP-REPORT-LINE.
           ADD 9 TO WS-RPT-LINE-CNT.
       P8250-EXIT.
           EXIT.
      *****************************************************************
      * P8260-WRITE-ERR-BREAKDOWN - ONE RPTOUT LINE PER EDIT TABLE    *
      * ENTRY, SHOWING HOW MANY TIMES EACH ERROR CODE FIRED THIS RUN. *
      * A CODE WITH NO OCCURRENCES (E.G. EC-CIRCUIT-UNKNOWN WHEN NO   *
      * UNBUNDLED OR SPECIAL-ACCESS RECORDS WERE PRESENT) PRINTS AS   *
      * ZERO, NOT SUPPRESSED - THE ABSENCE IS ITSELF INFORMATION.      *
      *****************************************************************
       P8260-WRITE-ERR-BREAKDOWN.
           PERFORM P8270-WRITE-ERR-LINE THRU P8270-EXIT
               VARYING WS-ET-SEARCH-SUB FROM 1 BY 1
               UNTIL WS-ET-SEARCH-SUB > 10.
       P8260-EXIT.
           EXIT.
       P8270-WRITE-ERR-LINE.
           MOVE SPACES TO WS-RPT-LABEL.
           STRING 'ERR ' DELIMITED BY SIZE
                  WS-ET-ERR-CODE(WS-ET-SEARCH-SUB) DELIMITED BY
                      SIZE
                  ' CNT' DELIMITED BY SIZE
               INTO WS-RPT-LABEL.
           MOVE ' ' TO WS-RPT-CC.
           MOVE WS-ET-FAIL-CNT(WS-ET-SEARCH-SUB) TO WS-RPT-VALUE.
           MOVE WS-RPT-DETAIL-LINE TO RP-REPORT-LINE.
           WRITE RP-REPORT-LINE.
           ADD 1 TO WS-RPT-LINE-CNT.
       P8270-EXIT.
           EXIT.
      *****************************************************************
      * P8280-WRITE-RECTYPE-SUMMARY - PER-RECORD-TYPE COUNTS.  SEE    *
      * THE WS-CNT-VOICE GROUP COMMENT - THESE OVERLAP AND WILL NOT   *
      * SUM TO WS-READ-CNT.                                            *
      *****************************************************************
       P8280-WRITE-RECTYPE-SUMMARY.
           MOVE ' ' TO WS-RPT-CC.
           MOVE 'VOICE MOU RECORDS' TO WS-RPT-LABEL.
           MOVE WS-CNT-VOICE TO WS-RPT-VALUE.
           MOVE WS-RPT-DETAIL-LINE TO RP-REPORT-LINE.
           WRITE RP-REPORT-LINE.
           MOVE 'DATA SERVICE RECORDS' TO WS-RPT-LABEL.
           MOVE WS-CNT-DATA TO WS-RPT-VALUE.
           MOVE WS-RPT-DETAIL-LINE TO RP-REPORT-LINE.
           WRITE RP-REPORT-LINE.
           MOVE 'SPECIAL ACCESS RECORDS' TO WS-RPT-LABEL.
           MOVE WS-CNT-SPCL TO WS-RPT-VALUE.
           MOVE WS-RPT-DETAIL-LINE TO RP-REPORT-LINE.
           WRITE RP-REPORT-LINE.
           MOVE 'UNE RECORDS' TO WS-RPT-LABEL.
           MOVE WS-CNT-UNE TO WS-RPT-VALUE.
           MOVE WS-RPT-DETAIL-LINE TO RP-REPORT-LINE.
           WRITE RP-REPORT-LINE.
           MOVE 'RECIPROCAL COMP RECORDS' TO WS-RPT-LABEL.
           MOVE WS-CNT-RECIP TO WS-RPT-VALUE.
           MOVE WS-RPT-DETAIL-LINE TO RP-REPORT-LINE.
           WRITE RP-REPORT-LINE.
           ADD 5 TO WS-RPT-LINE-CNT.
       P8280-EXIT.
           EXIT.
      *****************************************************************
      * P8290-WRITE-TRAILER - FINAL RPTOUT LINE.  THE LINE COUNT      *
      * PRINTED HERE IS THE COUNT BEFORE THIS LINE ITSELF, WHICH IS   *
      * THE ESTATE CONVENTION (SEE CABS-STD-031) - A REPORT NEVER     *
      * COUNTS ITS OWN TRAILER OR HEADER LINES IN THE LINE TOTAL.      *
      *****************************************************************
       P8290-WRITE-TRAILER.
           MOVE ' ' TO WS-RPT-CC.
           MOVE 'TOTAL RPT DETAIL LINES' TO WS-RPT-LABEL.
           MOVE WS-RPT-LINE-CNT TO WS-RPT-VALUE.
           MOVE WS-RPT-DETAIL-LINE TO RP-REPORT-LINE.
           WRITE RP-REPORT-LINE.
           MOVE SPACES TO WS-PRT-TEXT.
           MOVE 'END OF CABING01 REPORT' TO WS-PRT-TEXT.
           MOVE WS-PRINT-LINE-WORK TO RP-REPORT-LINE.
           WRITE RP-REPORT-LINE.
       P8290-EXIT.
           EXIT.
      *****************************************************************
      * P9000-TERM - MANDATORY.  NORMAL END OF RUN.                   *
      *****************************************************************
       P9000-TERM.
           DISPLAY 'CABING01 - RUN COMPLETE RUN=' WS-THIS-RUN-ID.
           DISPLAY 'CABING01 - READ=' WS-READ-CNT
               ' WRITTEN=' WS-WRITE-CNT ' REJECTED=' WS-REJECT-CNT.
           DISPLAY 'CABING01 - SUSPENSE RECORDS=' WS-SUS-TOTAL-CNT
               ' BALANCE=' CT-BAL-IND.
           PERFORM P9050-CROSS-CHECK-COUNTS THRU P9050-EXIT.
           PERFORM P9100-CLOSE-FILES THRU P9100-EXIT.
       P9000-EXIT.
           EXIT.
      *****************************************************************
      * P9050-CROSS-CHECK-COUNTS - THE FOUR STATUS-CLASS COUNTERS     *
      * (CLEAN/WARN/HARD/FATAL) SHOULD ALWAYS SUM TO WS-READ-CNT,     *
      * SINCE EVERY RECORD READ FALLS INTO EXACTLY ONE OF THE FOUR IN *
      * P2200.  THIS IS AN OPERATIONAL SANITY CHECK, NOT PART OF THE  *
      * P8100 BALANCING EQUATION - IT ONLY DISPLAYS A WARNING, IT     *
      * DOES NOT SET CT-BAL-IND OR CT-RC.                              *
      *****************************************************************
       P9050-CROSS-CHECK-COUNTS.
           COMPUTE WS-BAL-LEFT-SIDE = WS-CNT-CLEAN + WS-CNT-WARN
               + WS-CNT-HARD + WS-CNT-FATAL.
           IF WS-BAL-LEFT-SIDE NOT = WS-READ-CNT
               DISPLAY 'CABING01 - WARNING: STATUS CLASS COUNTS '
                   'DO NOT SUM TO READ COUNT - REVIEW WS-EDT-'
               DISPLAY 'SEVERITY LOGIC IN P2200'.
       P9050-EXIT.
           EXIT.
      *****************************************************************
      * P9100-CLOSE-FILES - CLOSES EVERYTHING THAT MIGHT BE OPEN.     *
      * SAFE TO CALL FROM THE NORMAL PATH (P9000) OR THE FATAL PATH   *
      * (P9900) - NO CHECK IS MADE FOR WHETHER A GIVEN FILE IS        *
      * ACTUALLY OPEN, WHICH IS HARMLESS ON THIS COMPILER/RUNTIME.    *
      *****************************************************************
       P9100-CLOSE-FILES.
           CLOSE RAWIN.
           CLOSE PARMIN.
           CLOSE EDTOUT.
           CLOSE SUSOUT.
           CLOSE CTLOUT.
           CLOSE RPTOUT.
       P9100-EXIT.
           EXIT.
      *****************************************************************
      * P9900-FATAL-EXIT - CABS-STD-003: PHYSICAL BOTTOM OF THE       *
      * PROGRAM.  NOT PART OF THE PERFORM STRUCTURE ABOVE - IT IS     *
      * REACHED ONLY BY GO TO, FROM THREE WIDELY SEPARATED PLACES:    *
      *   P2100-READ-RAW          (BAD FILE STATUS ON RAWIN)          *
      *   P6200-EDIT-VARIANT-DATA (OCTET TOTAL OVERFLOW)              *
      *   P7300-WRITE-SUSPENSE    (BAD FILE STATUS ON SUSOUT,         *
      *                            INCLUDING SUSPENSE FILE FULL)      *
      * THERE IS NO P9900-EXIT - NOTHING PERFORMS THIS PARAGRAPH,     *
      * AND IT NEVER RETURNS.  IT CLOSES WHATEVER IS OPEN, CALLS      *
      * CABABEND WITH THE CURRENT ABEND REASON CODE, AND STOPS.       *
      *****************************************************************
       P9900-FATAL-EXIT.
           DISPLAY 'CABING01 - FATAL ERROR AT ' WS-ABEND-LOCATION.
           DISPLAY 'CABING01 - FILE STATUS ' WS-ABEND-FILE-STATUS.
           PERFORM P9100-CLOSE-FILES THRU P9100-EXIT.
           MOVE WS-ABEND-FILE-STATUS TO WS-ABEND-REASON-CD.
           CALL 'CABABEND' USING WS-THIS-PROGRAM WS-ABEND-REASON-CD
               WS-THIS-RUN-ID.
           STOP RUN.
