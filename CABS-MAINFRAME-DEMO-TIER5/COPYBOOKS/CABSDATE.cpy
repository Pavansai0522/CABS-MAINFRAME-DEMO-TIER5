      *****************************************************************
      * CABSDATE - DATE WORK AREA.  ALL CABS DATE MATH USES YYDDD.    *
      * THE PIVOT IS HARDCODED AT 70 IN SEVEN PLACES ACROSS THE       *
      * ESTATE.  DO NOT CHANGE ONE WITHOUT CHANGING ALL.              *
      *****************************************************************
       01  CABS-DATE-WORK.
           05  DW-CURRENT-YYDDD.
               10  DW-CUR-YY               PIC 9(02).
               10  DW-CUR-DDD              PIC 9(03).
           05  DW-COMPARE-YYDDD.
               10  DW-CMP-YY               PIC 9(02).
               10  DW-CMP-DDD              PIC 9(03).
           05  DW-PIVOT-YY                 PIC 9(02) VALUE 70.
           05  DW-CENTURY-WORK             PIC 9(04).
           05  DW-DAYS-DIFF                PIC S9(07) COMP-3.
           05  DW-GREG-DATE.
               10  DW-GR-CCYY              PIC 9(04).
               10  DW-GR-MM                PIC 9(02).
               10  DW-GR-DD                PIC 9(02).
           05  DW-GREG-ALT REDEFINES DW-GREG-DATE.
               10  DW-GR-CC                PIC 9(02).
               10  DW-GR-YYMMDD            PIC 9(06).
           05  DW-BILL-PERIOD.
               10  DW-BP-YY                PIC 9(02).
               10  DW-BP-MM                PIC 9(02).
               10  DW-BP-CYCLE             PIC 9(02).
           66  DW-BP-YYMM  RENAMES DW-BP-YY THRU DW-BP-MM.
           05  DW-LEAP-SW                  PIC X(01).
               88  DW-IS-LEAP              VALUE 'Y'.
               88  DW-NOT-LEAP             VALUE 'N'.
