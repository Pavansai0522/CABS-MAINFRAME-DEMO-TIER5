      *****************************************************************
      * CABSFCTR - PIU / PLU JURISDICTIONAL FACTOR RECORD             *
      * FACTORS ARRIVE QUARTERLY AND ARE APPLIED RETROACTIVELY TO     *
      * THE PRIOR QUARTER.  THE RESTATEMENT PROCESS REPRICES USAGE    *
      * ALREADY BILLED AND RAISES AN ADJUSTMENT.  SEE CABSRSTA.       *
      *****************************************************************
       01  CABS-FACTOR-RECORD.
           05  FC-KEY.
               10  FC-OCN                  PIC X(04).
               10  FC-STATE-CD             PIC X(02).
               10  FC-LATA                 PIC 9(03).
               10  FC-EFF-YYDDD            PIC 9(05).
           05  FC-FACTORS.
               10  FC-PIU                  PIC S9(03)V9(05) COMP-3.
               10  FC-PLU                  PIC S9(03)V9(05) COMP-3.
               10  FC-PSU                  PIC S9(03)V9(05) COMP-3.
           05  FC-CONTROL.
               10  FC-SOURCE               PIC X(01).
                   88  FC-FROM-CARRIER     VALUE 'C'.
                   88  FC-FROM-STUDY       VALUE 'S'.
                   88  FC-FROM-DEFAULT     VALUE 'D'.
                   88  FC-DISPUTED         VALUE 'X'.
               10  FC-RESTATE-SW           PIC X(01).
                   88  FC-RESTATE-REQD     VALUE 'Y'.
                   88  FC-NO-RESTATE       VALUE 'N'.
               10  FC-RESTATE-FROM-YYDDD   PIC 9(05).
               10  FC-RESTATE-THRU-YYDDD   PIC 9(05).
               10  FC-PRIOR-PIU            PIC S9(03)V9(05) COMP-3.
               10  FC-PRIOR-PLU            PIC S9(03)V9(05) COMP-3.
               10  FC-RECV-YYDDD           PIC 9(05).
           05  FC-FILLER                   PIC X(20).
