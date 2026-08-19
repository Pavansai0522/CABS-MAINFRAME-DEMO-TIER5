      *****************************************************************
      * CABSPRNT - PRINT LINE.  COLUMN 1 IS ASA CARRIAGE CONTROL AND  *
      * ALSO CARRIES MEANING TO THE DOWNSTREAM FORMATTER - A '4' IN   *
      * PC-CC IS READ BY THE BURST PROCESS AS "START NEW BILL SECTION"*
      * AS WELL AS "SKIP TO CHANNEL 4".  DO NOT REUSE.                *
      *****************************************************************
       01  CABS-PRINT-LINE.
           05  PC-CC                       PIC X(01).
               88  PC-SINGLE-SPACE         VALUE ' '.
               88  PC-DOUBLE-SPACE         VALUE '0'.
               88  PC-TRIPLE-SPACE         VALUE '-'.
               88  PC-NEW-PAGE             VALUE '1'.
               88  PC-NEW-SECTION          VALUE '4'.
               88  PC-NEW-INVOICE          VALUE '7'.
               88  PC-SUPPRESS             VALUE '+'.
           05  PC-BODY.
               10  PC-COL-001-020          PIC X(20).
               10  PC-COL-021-060          PIC X(40).
               10  PC-COL-061-090          PIC X(30).
               10  PC-COL-091-132          PIC X(42).
           05  PC-BODY-R REDEFINES PC-BODY.
               10  PC-TEXT                 PIC X(132).
           05  PC-BODY-A REDEFINES PC-BODY.
               10  PC-AMT-DESC             PIC X(70).
               10  PC-AMT-QTY              PIC Z,ZZZ,ZZZ,ZZ9.99.
               10  PC-AMT-RATE             PIC Z.ZZZZ9.
               10  PC-AMT-VALUE            PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
               10  PC-AMT-FILL             PIC X(20).
