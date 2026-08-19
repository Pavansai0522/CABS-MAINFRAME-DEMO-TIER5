      *****************************************************************
      * CABSSETL - INTER-CARRIER SETTLEMENT RECORD                    *
      * COVERS MEET-POINT BILLING, RECIPROCAL COMPENSATION AND        *
      * CMDS/RAO EXCHANGE.  ONE LAYOUT, THREE SETTLEMENT KINDS.       *
      *****************************************************************
       01  CABS-SETTLEMENT-RECORD.
           05  ST-KEY.
               10  ST-SETTLE-TYPE          PIC X(01).
                   88  ST-MEET-POINT       VALUE 'M'.
                   88  ST-RECIP-COMP       VALUE 'R'.
                   88  ST-CMDS-RAO         VALUE 'C'.
               10  ST-COUNTERPARTY-OCN     PIC X(04).
               10  ST-SETTLE-PERIOD        PIC 9(06).
               10  ST-SEQ                  PIC 9(09) COMP-3.
           05  ST-BASIS.
               10  ST-TOTAL-MOU            PIC S9(15)V9(02) COMP-3.
               10  ST-BILLABLE-MOU         PIC S9(15)V9(02) COMP-3.
               10  ST-CAPPED-MOU           PIC S9(15)V9(02) COMP-3.
               10  ST-RATE-APPLIED         PIC S9(05)V9(05) COMP-3.
           05  ST-MPB-AREA.
               10  ST-OUR-PCT              PIC S9(03)V9(05) COMP-3.
               10  ST-THEIR-PCT            PIC S9(03)V9(05) COMP-3.
               10  ST-PCT-VARIANCE         PIC S9(03)V9(05) COMP-3.
               10  ST-TRUNK-GRP            PIC X(08).
               10  ST-CIRCUIT-ID           PIC X(20).
           05  ST-AMOUNTS.
               10  ST-GROSS-AMT            PIC S9(13)V9(05) COMP-3.
               10  ST-OUR-SHARE            PIC S9(13)V9(05) COMP-3.
               10  ST-THEIR-SHARE          PIC S9(13)V9(05) COMP-3.
               10  ST-NET-DUE              PIC S9(13)V9(02) COMP-3.
               10  ST-ROUND-RESIDUE        PIC S9(05)V9(05) COMP-3.
           05  ST-STATUS.
               10  ST-DIRECTION            PIC X(01).
                   88  ST-RECEIVABLE       VALUE 'R'.
                   88  ST-PAYABLE          VALUE 'P'.
               10  ST-DISPUTE-SW           PIC X(01).
               10  ST-EXCH-YYDDD           PIC 9(05).
               10  ST-RAO-CODE             PIC X(03).
           05  ST-FILLER                   PIC X(40).
