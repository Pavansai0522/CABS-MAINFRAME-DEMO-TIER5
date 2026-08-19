      *****************************************************************
      * CABSRT02 - INTERNAL RATE ELEMENT TABLE.  LOADED BY CABRAT01   *
      * FROM TELCABS.CABS.RATE AND PASSED BY REFERENCE TO EVERY       *
      * RATING MODULE.  OCCURS DEPENDING ON - THE TABLE IS SHORTER    *
      * THAN THE MAXIMUM ON EVERY RUN SINCE THE 2007 TARIFF PURGE.    *
      * NESTED VIA CABSRT01.  NESTS CABSRT03.                         *
      *****************************************************************
       01  R2-RATE-TABLE.
           05  R2-ENTRY-CNT                PIC 9(04) VALUE 0.
           05  R2-LOAD-YYDDD               PIC 9(05) VALUE 0.
           05  R2-TABLE-FULL-SW            PIC X(01) VALUE 'N'.
               88  R2-TABLE-FULL           VALUE 'Y'.
           05  R2-ENTRY OCCURS 1 TO 600 TIMES
                    DEPENDING ON R2-ENTRY-CNT
                    INDEXED BY R2-EX.
               10  R2-EN-KEY.
                   15  R2-EN-TARIFF        PIC X(04).
                   15  R2-EN-ELEM          PIC X(06).
                   15  R2-EN-JURIS         PIC X(01).
                   15  R2-EN-STATE         PIC X(02).
                   15  R2-EN-EFF-YYDDD     PIC 9(05).
               10  R2-EN-INITIAL           PIC S9(05)V9(05) COMP-3.
               10  R2-EN-ADDL              PIC S9(05)V9(05) COMP-3.
               10  R2-EN-SETUP             PIC S9(07)V9(05) COMP-3.
               10  R2-EN-MIN-CHG           PIC S9(07)V9(02) COMP-3.
               10  R2-EN-MAX-CHG           PIC S9(11)V9(02) COMP-3.
               10  R2-EN-ROUND-RULE        PIC X(01).
               10  R2-EN-ROUND-POS         PIC 9(01).
               10  R2-EN-INIT-PERIOD       PIC 9(04).
               10  R2-EN-ADDL-PERIOD       PIC 9(04).
               10  R2-EN-EXP-YYDDD         PIC 9(05).
               10  R2-EN-BAND-CNT          PIC 9(02).
               10  R2-EN-BAND-OFFSET       PIC 9(04).
               10  R2-EN-MODULE-SFX        PIC X(02).
       COPY CABSRT03.
