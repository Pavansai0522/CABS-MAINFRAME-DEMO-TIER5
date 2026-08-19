      *****************************************************************
      * CABSBHDR - BILL HEADER / INVOICE SUMMARY.  LRECL 0400 FB.     *
      * THIS IS THE RECORD THE BILL-TO-BILL COMPARISON KEYS ON.       *
      *****************************************************************
       01  CABS-BILL-HEADER.
           05  BH-KEY.
               10  BH-BAN                  PIC X(13).
               10  BH-BILL-PERIOD          PIC 9(06).
           05  BH-OCN                      PIC X(04).
           05  BH-INVOICE-NBR              PIC X(14).
           05  BH-BILL-YYDDD               PIC 9(05).
           05  BH-DUE-YYDDD                PIC 9(05).
           05  BH-AMOUNTS.
               10  BH-PRIOR-BAL            PIC S9(13)V9(02) COMP-3.
               10  BH-PAYMENTS             PIC S9(13)V9(02) COMP-3.
               10  BH-ADJUSTMENTS          PIC S9(13)V9(02) COMP-3.
               10  BH-CURR-USAGE           PIC S9(13)V9(02) COMP-3.
               10  BH-CURR-RECURRING       PIC S9(13)V9(02) COMP-3.
               10  BH-CURR-NONRECUR        PIC S9(13)V9(02) COMP-3.
               10  BH-RESTATEMENT          PIC S9(13)V9(02) COMP-3.
               10  BH-SETTLEMENT-NET       PIC S9(13)V9(02) COMP-3.
               10  BH-TAX                  PIC S9(11)V9(02) COMP-3.
               10  BH-TOTAL-DUE            PIC S9(13)V9(02) COMP-3.
           05  BH-JURIS-SPLIT.
               10  BH-INTERSTATE-AMT       PIC S9(13)V9(02) COMP-3.
               10  BH-INTRASTATE-AMT       PIC S9(13)V9(02) COMP-3.
               10  BH-LOCAL-AMT            PIC S9(13)V9(02) COMP-3.
           05  BH-CONTROL.
               10  BH-DETAIL-LINES         PIC S9(07) COMP-3.
               10  BH-CDR-COUNT            PIC S9(11) COMP-3.
               10  BH-HASH-AMOUNT          PIC S9(15)V9(05) COMP-3.
               10  BH-STATUS               PIC X(01).
                   88  BH-FINAL            VALUE 'F'.
                   88  BH-HELD             VALUE 'H'.
                   88  BH-PENDING          VALUE 'P'.
                   88  BH-CANCELLED        VALUE 'C'.
               10  BH-HOLD-REASON          PIC X(04).
           05  BH-FILLER                   PIC X(224).
