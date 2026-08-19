      *****************************************************************
      * CABSWRK - STANDARD WORKING STORAGE FOR ALL CABS BATCH PGMS    *
      * NESTED: PULLS IN CABSERR, CABSDATE AND CABSCTL.               *
      * CHANGING ANY OF THOSE THREE FORCES A RECOMPILE OF EVERY       *
      * PROGRAM THAT COPIES CABSWRK - AT LAST COUNT 60 PLUS.          *
      *****************************************************************
       01  WS-STANDARD-SWITCHES.
           05  WS-EOF-SW                   PIC X(01) VALUE 'N'.
               88  WS-EOF                  VALUE 'Y'.
               88  WS-NOT-EOF              VALUE 'N'.
           05  WS-ERROR-SW                 PIC X(01) VALUE 'N'.
               88  WS-ERROR-FOUND          VALUE 'Y'.
           05  WS-FIRST-TIME-SW            PIC X(01) VALUE 'Y'.
               88  WS-FIRST-TIME           VALUE 'Y'.
           05  WS-RESTART-SW               PIC X(01) VALUE 'N'.
               88  WS-RESTARTING           VALUE 'Y'.
       01  WS-STANDARD-COUNTERS.
           05  WS-READ-CNT                 PIC S9(11) COMP-3 VALUE 0.
           05  WS-WRITE-CNT                PIC S9(11) COMP-3 VALUE 0.
           05  WS-REJECT-CNT               PIC S9(11) COMP-3 VALUE 0.
           05  WS-SUMM-CNT                 PIC S9(11) COMP-3 VALUE 0.
           05  WS-CFWD-CNT                 PIC S9(11) COMP-3 VALUE 0.
       01  WS-STANDARD-ACCUMS.
           05  WS-ACC-MINUTES              PIC S9(15)V9(02) COMP-3
                                                            VALUE 0.
           05  WS-ACC-AMOUNT               PIC S9(13)V9(05) COMP-3
                                                            VALUE 0.
           05  WS-ACC-SEQ-HASH             PIC S9(17)       COMP-3
                                                            VALUE 0.
           05  WS-ACC-OCN-HASH             PIC S9(15)       COMP-3
                                                            VALUE 0.
       01  WS-FILE-STATUS-AREA.
           05  WS-FS-INPUT                 PIC X(02) VALUE '00'.
           05  WS-FS-OUTPUT                PIC X(02) VALUE '00'.
           05  WS-FS-CONTROL               PIC X(02) VALUE '00'.
           05  WS-FS-SUSPENSE              PIC X(02) VALUE '00'.
           05  WS-FS-TABLE                 PIC X(02) VALUE '00'.
       COPY CABSERR.
       COPY CABSDATE.
       COPY CABSCTL.
