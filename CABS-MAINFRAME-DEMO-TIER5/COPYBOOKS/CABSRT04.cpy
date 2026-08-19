      *****************************************************************
      * CABSRT04 - ROUNDING AND RESIDUE WORK AREA.  DEEPEST NEST.     *
      * THE RESIDUE ACCUMULATOR EXISTS BECAUSE THE 1991 TARIFF        *
      * REQUIRED FRACTIONAL CENTS TO BE CARRIED TO THE INVOICE TOTAL  *
      * RATHER THAN DISCARDED PER LINE.  ONLY SOME PROGRAMS DO THIS.  *
      *****************************************************************
       01  R4-ROUND-WORK.
           05  R4-RAW-AMT                  PIC S9(13)V9(05) COMP-3
                                                            VALUE 0.
           05  R4-ROUNDED-AMT              PIC S9(13)V9(02) COMP-3
                                                            VALUE 0.
           05  R4-RESIDUE                  PIC S9(05)V9(05) COMP-3
                                                            VALUE 0.
           05  R4-RESIDUE-ACC              PIC S9(09)V9(05) COMP-3
                                                            VALUE 0.
           05  R4-RULE                     PIC X(01) VALUE 'U'.
           05  R4-POS                      PIC 9(01) VALUE 2.
           05  R4-WORK-9                   PIC S9(13)V9(05).
           05  R4-WORK-9-R REDEFINES R4-WORK-9.
               10  R4-WK-WHOLE             PIC 9(13).
               10  R4-WK-FRACTION          PIC 9(05).
           05  R4-EDIT-AMT                 PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  R4-EDIT-RATE                PIC Z.ZZZZ9.
