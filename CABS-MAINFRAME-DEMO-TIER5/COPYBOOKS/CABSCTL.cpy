      *****************************************************************
      * CABSCTL - RUN CONTROL / BALANCING RECORD                      *
      * WRITTEN BY EVERY PROCESS AT EVERY BOUNDARY.  LRECL 0180 FB.   *
      * THE BALANCING EQUATION FOR EVERY PROCESS IS                   *
      *     CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARISED        *
      *              + CT-CARRIED-FWD                                 *
      * ANY PROCESS WHERE THIS DOES NOT HOLD MUST SET CT-OUT-OF-BAL.  *
      *****************************************************************
       01  CABS-CONTROL-RECORD.
           05  CT-KEY.
               10  CT-RUN-ID               PIC X(12).
               10  CT-PROCESS-ID           PIC X(08).
               10  CT-STEP-SEQ             PIC 9(03).
           05  CT-RUN-CONTEXT.
               10  CT-CYCLE-YYDDD          PIC 9(05).
               10  CT-BILL-PERIOD          PIC 9(06).
               10  CT-RERUN-NBR            PIC 9(02).
               10  CT-JOBNAME              PIC X(08).
               10  CT-STEPNAME             PIC X(08).
           05  CT-COUNTS.
               10  CT-READ                 PIC S9(11) COMP-3.
               10  CT-WRITTEN              PIC S9(11) COMP-3.
               10  CT-REJECTED             PIC S9(11) COMP-3.
               10  CT-SUMMARISED           PIC S9(11) COMP-3.
               10  CT-CARRIED-FWD          PIC S9(11) COMP-3.
           05  CT-HASH-TOTALS.
               10  CT-HASH-MINUTES         PIC S9(15)V9(02) COMP-3.
               10  CT-HASH-AMOUNT          PIC S9(13)V9(05) COMP-3.
               10  CT-HASH-SEQ             PIC S9(17)       COMP-3.
               10  CT-HASH-OCN             PIC S9(15)       COMP-3.
           05  CT-STATUS.
               10  CT-BAL-IND              PIC X(01).
                   88  CT-IN-BALANCE       VALUE 'B'.
                   88  CT-OUT-OF-BAL       VALUE 'O'.
                   88  CT-NOT-CHECKED      VALUE ' '.
               10  CT-RC                   PIC 9(04).
               10  CT-ABEND-CD             PIC X(04).
               10  CT-RESTART-KEY          PIC X(26).
           05  CT-FILLER                   PIC X(27).
