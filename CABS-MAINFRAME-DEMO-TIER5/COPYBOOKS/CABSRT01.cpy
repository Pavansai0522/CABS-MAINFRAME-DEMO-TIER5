      *****************************************************************
      * CABSRT01 - RATING RUN CONTROL BLOCK.  RATING FAMILY ONLY.     *
      * NESTS CABSRT02 WHICH NESTS CABSRT03 WHICH NESTS CABSRT04.     *
      * FOUR LEVELS DEEP.  A CHANGE TO CABSRT04 RECOMPILES THE WHOLE  *
      * RATING SUITE.  THIS WAS FLAGGED IN THE 1999 ARCHITECTURE      *
      * REVIEW AND NOT ACTIONED.                                      *
      *                                                               *
      * REVISION HISTORY                                              *
      *   V1.00  1989-06-02  R.T.WHEELER   INITIAL SWITCHED ACCESS    *
      *   V1.06  1994-10-17  D.OKONKWO     ADDED TANDEM CONTROL SW    *
      *   V2.00  1998-02-09  J.M.CASTILLO  OPERATOR SVCS SW ADDED     *
      *   V2.02  2003-07-21  P.NAIR        BAND CONTROL MOVED TO RT03 *
      *   V2.05  2011-09-14  A.BUKOWSKI    OVERRIDE PRIORITY FIELDS   *
      *****************************************************************
       01  R1-RATING-CONTROL.
           05  R1-RUN-ID                   PIC X(12) VALUE SPACES.
           05  R1-PROCESS-ID               PIC X(08) VALUE SPACES.
           05  R1-CYCLE-YYDDD              PIC 9(05) VALUE ZERO.
           05  R1-BILL-PERIOD              PIC 9(06) VALUE ZERO.
           05  R1-TARIFF-CD                PIC X(04) VALUE 'FCC1'.
           05  R1-MODE-SW                  PIC X(01) VALUE 'P'.
               88  R1-PRODUCTION           VALUE 'P'.
               88  R1-PARALLEL             VALUE 'L'.
               88  R1-SIMULATION           VALUE 'S'.
               88  R1-ANY-LIVE-MODE        VALUE 'P' 'L'.
               88  R1-ANY-TEST-MODE        VALUE 'L' 'S'.
           05  R1-TANDEM-SW                PIC X(01) VALUE 'Y'.
               88  R1-TANDEM-ACTIVE        VALUE 'Y'.
           05  R1-OPR-SVC-SW               PIC X(01) VALUE 'N'.
               88  R1-OPR-SVC-ACTIVE       VALUE 'Y'.
           05  R1-OVERRIDE-PRIORITY        PIC 9(01) VALUE 3.
           05  R1-CALL-TARGET              PIC X(08) VALUE SPACES.
           05  R1-CALL-PREFIX              PIC X(06) VALUE 'CABRAT'.
           05  R1-CALL-SUFFIX              PIC X(02) VALUE SPACES.
           05  R1-ELEM-IN                  PIC X(06) VALUE SPACES.
           05  R1-QTY-IN                   PIC S9(13)V9(02) COMP-3
                                                            VALUE 0.
           05  R1-AMT-OUT                  PIC S9(13)V9(05) COMP-3
                                                            VALUE 0.
           05  R1-RC                       PIC 9(04) VALUE 0.
       COPY CABSRT02.
