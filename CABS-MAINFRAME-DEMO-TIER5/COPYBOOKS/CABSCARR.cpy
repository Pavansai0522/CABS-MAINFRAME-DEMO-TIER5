      *****************************************************************
      * CABSCARR - CARRIER (OCN) MASTER.  VSAM KSDS TELCABS.CARRIER   *
      *****************************************************************
       01  CABS-CARRIER-RECORD.
           05  CR-KEY.
               10  CR-OCN                  PIC X(04).
           05  CR-IDENT.
               10  CR-NAME                 PIC X(40).
               10  CR-ACNA                 PIC X(03).
               10  CR-CIC                  PIC 9(04).
               10  CR-TYPE                 PIC X(01).
                   88  CR-IXC              VALUE 'I'.
                   88  CR-CLEC             VALUE 'C'.
                   88  CR-ILEC             VALUE 'L'.
                   88  CR-WIRELESS         VALUE 'W'.
                   88  CR-RESELLER         VALUE 'R'.
                   88  CR-SETTLEMENT-PTY   VALUE 'C' 'L' 'W'.
                   88  CR-BILLED-PARTY     VALUE 'I' 'C' 'R'.
           05  CR-BILLING.
               10  CR-BILL-CYCLE           PIC 9(02).
               10  CR-BILL-MEDIA           PIC X(01).
               10  CR-CURRENCY             PIC X(03).
               10  CR-TERMS-DAYS           PIC 9(03).
               10  CR-CREDIT-LIMIT         PIC S9(11)V9(02) COMP-3.
           05  CR-FACTORS.
               10  CR-DEFAULT-PIU          PIC S9(03)V9(05) COMP-3.
               10  CR-DEFAULT-PLU          PIC S9(03)V9(05) COMP-3.
               10  CR-FACTOR-SRC           PIC X(01).
                   88  CR-CARRIER-SUPPLIED VALUE 'C'.
                   88  CR-TARIFF-DEFAULT   VALUE 'T'.
                   88  CR-STUDY-DERIVED    VALUE 'S'.
           05  CR-SETTLEMENT.
               10  CR-RECIP-COMP-ELIG      PIC X(01).
               10  CR-RECIP-RATE           PIC S9(05)V9(05) COMP-3.
               10  CR-ISP-CAP-MOU          PIC S9(13) COMP-3.
               10  CR-CMDS-RAO             PIC X(03).
               10  CR-MPB-ELIGIBLE         PIC X(01).
           05  CR-STATUS.
               10  CR-ACTIVE-SW            PIC X(01).
               10  CR-EFF-YYDDD            PIC 9(05).
               10  CR-EXP-YYDDD            PIC 9(05).
           05  CR-FILLER                   PIC X(30).
