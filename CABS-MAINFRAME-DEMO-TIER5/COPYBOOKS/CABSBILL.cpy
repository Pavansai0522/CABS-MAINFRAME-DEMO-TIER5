      *****************************************************************
      * CABSBILL - BILL DETAIL LINE.  VARIABLE LENGTH RECORD.         *
      * RECFM VB  LRECL 1651.  THE OCCURS DEPENDING ON MEANS THE      *
      * RECORD LENGTH CHANGES WITH THE NUMBER OF RATE ELEMENTS.       *
      * PROGRAMS THAT MOVE THIS TO A FIXED AREA WILL TRUNCATE.        *
      *****************************************************************
       01  CABS-BILL-DETAIL.
           05  BD-KEY.
               10  BD-BAN                  PIC X(13).
               10  BD-BILL-PERIOD          PIC 9(06).
               10  BD-SECTION              PIC X(02).
               10  BD-LINE-SEQ             PIC 9(07) COMP-3.
           05  BD-OCN                      PIC X(04).
           05  BD-JURIS-CD                 PIC X(01).
           05  BD-STATE-CD                 PIC X(02).
           05  BD-DESCRIPTION              PIC X(60).
           05  BD-TOTALS.
               10  BD-TOT-MINUTES          PIC S9(13)V9(02) COMP-3.
               10  BD-TOT-AMOUNT           PIC S9(13)V9(05) COMP-3.
               10  BD-TOT-ROUNDED          PIC S9(13)V9(02) COMP-3.
               10  BD-ROUND-DELTA          PIC S9(05)V9(05) COMP-3.
           05  BD-ELEM-CNT                 PIC 9(03).
           05  BD-ELEMENT OCCURS 1 TO 40 TIMES
                    DEPENDING ON BD-ELEM-CNT
                    INDEXED BY BD-EX.
               10  BD-EL-RATE-ELEM         PIC X(06).
               10  BD-EL-QTY               PIC S9(13)V9(02) COMP-3.
               10  BD-EL-RATE              PIC S9(05)V9(05) COMP-3.
               10  BD-EL-AMOUNT            PIC S9(11)V9(05) COMP-3.
               10  BD-EL-ROUND-RULE        PIC X(01).
               10  BD-EL-SRC-PROCESS       PIC X(08).
