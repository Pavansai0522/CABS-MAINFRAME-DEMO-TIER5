      *****************************************************************
      * CABSCIRC - CIRCUIT / TRUNK GROUP INVENTORY                    *
      *****************************************************************
       01  CABS-CIRCUIT-RECORD.
           05  CI-KEY.
               10  CI-CIRCUIT-ID           PIC X(20).
           05  CI-IDENT.
               10  CI-TRUNK-GRP            PIC X(08).
               10  CI-OCN                  PIC X(04).
               10  CI-BAN                  PIC X(13).
               10  CI-USOC                 PIC X(05).
               10  CI-SERVICE-TYPE         PIC X(02).
                   88  CI-SWITCHED         VALUE 'SW'.
                   88  CI-SPECIAL          VALUE 'SP'.
                   88  CI-UNE              VALUE 'UN'.
                   88  CI-INTERCONNECT     VALUE 'IC'.
           05  CI-LOCATION.
               10  CI-A-CLLI               PIC X(11).
               10  CI-Z-CLLI               PIC X(11).
               10  CI-A-LATA               PIC 9(03).
               10  CI-Z-LATA               PIC 9(03).
               10  CI-STATE-CD             PIC X(02).
           05  CI-MPB.
               10  CI-MPB-SW               PIC X(01).
               10  CI-MPB-OUR-PCT          PIC S9(03)V9(05) COMP-3.
               10  CI-MPB-OTHER-OCN        PIC X(04).
               10  CI-MPB-OTHER-PCT        PIC S9(03)V9(05) COMP-3.
           05  CI-TERM.
               10  CI-INSTALL-YYDDD        PIC 9(05).
               10  CI-TERM-MONTHS          PIC 9(03).
               10  CI-DISC-YYDDD           PIC 9(05).
               10  CI-STATUS               PIC X(01).
           05  CI-FILLER                   PIC X(25).
