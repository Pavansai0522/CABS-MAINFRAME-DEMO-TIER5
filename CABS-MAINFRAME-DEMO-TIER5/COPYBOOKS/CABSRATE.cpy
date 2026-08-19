      *****************************************************************
      * CABSRATE - ACCESS RATE TABLE RECORD.  VSAM KSDS TELCABS.RATE  *
      * RATES CARRY FIVE DECIMAL PLACES.  FRACTIONAL CENT RATES ARE   *
      * NORMAL FOR SWITCHED ACCESS - DO NOT TRUNCATE ON LOAD.         *
      *****************************************************************
       01  CABS-RATE-RECORD.
           05  RT-KEY.
               10  RT-TARIFF-CD            PIC X(04).
               10  RT-RATE-ELEM            PIC X(06).
               10  RT-JURIS-CD             PIC X(01).
               10  RT-STATE-CD             PIC X(02).
               10  RT-EFF-YYDDD            PIC 9(05).
           05  RT-RATES.
               10  RT-INITIAL-RATE         PIC S9(05)V9(05) COMP-3.
               10  RT-ADDL-RATE            PIC S9(05)V9(05) COMP-3.
               10  RT-SETUP-CHG            PIC S9(07)V9(05) COMP-3.
               10  RT-MIN-CHG              PIC S9(07)V9(02) COMP-3.
               10  RT-MAX-CHG              PIC S9(11)V9(02) COMP-3.
           05  RT-RATE-CTL.
               10  RT-ROUND-RULE           PIC X(01).
                   88  RT-ROUND-HALF-UP    VALUE 'U'.
                   88  RT-ROUND-HALF-EVEN  VALUE 'E'.
                   88  RT-TRUNCATE         VALUE 'T'.
                   88  RT-ROUND-UP-ALWAYS  VALUE 'C'.
               10  RT-ROUND-POS            PIC 9(01).
               10  RT-INIT-PERIOD          PIC 9(04).
               10  RT-ADDL-PERIOD          PIC 9(04).
               10  RT-DISC-ELIGIBLE        PIC X(01).
               10  RT-EXP-YYDDD            PIC 9(05).
           05  RT-BAND-TABLE.
               10  RT-BAND-CNT             PIC 9(02).
               10  RT-BAND OCCURS 1 TO 24 TIMES
                        DEPENDING ON RT-BAND-CNT
                        INDEXED BY RT-BX.
                   15  RT-BAND-FROM        PIC S9(11) COMP-3.
                   15  RT-BAND-THRU        PIC S9(11) COMP-3.
                   15  RT-BAND-RATE        PIC S9(05)V9(05) COMP-3.
                   15  RT-BAND-PCT         PIC S9(03)V9(05) COMP-3.
