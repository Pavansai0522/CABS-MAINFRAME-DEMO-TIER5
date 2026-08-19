      *****************************************************************
      * CABSERR - SHARED ERROR CODE TABLE AND SUSPENSE LAYOUT         *
      * INCLUDED VIA CABSWRK - DO NOT COPY DIRECTLY (SEE CABS-STD-002)*
      * SEVERAL PROGRAMS COPY IT DIRECTLY ANYWAY.                     *
      *****************************************************************
       01  CABS-ERROR-CODES.
           05  EC-OCN-UNKNOWN              PIC X(04) VALUE 'E001'.
           05  EC-BAN-UNKNOWN              PIC X(04) VALUE 'E002'.
           05  EC-RATE-NOT-FOUND           PIC X(04) VALUE 'E003'.
           05  EC-JURIS-INDET              PIC X(04) VALUE 'E004'.
           05  EC-FACTOR-MISSING           PIC X(04) VALUE 'E005'.
           05  EC-MPB-PCT-INVALID          PIC X(04) VALUE 'E006'.
           05  EC-DATE-INVALID             PIC X(04) VALUE 'E007'.
           05  EC-MIN-NEGATIVE             PIC X(04) VALUE 'E008'.
           05  EC-DUP-SEQ                  PIC X(04) VALUE 'E009'.
           05  EC-CIRCUIT-UNKNOWN          PIC X(04) VALUE 'E010'.
           05  EC-TERM-EXPIRED             PIC X(04) VALUE 'E011'.
           05  EC-OUT-OF-BALANCE           PIC X(04) VALUE 'E012'.
           05  EC-RECIP-CAP-EXCEEDED       PIC X(04) VALUE 'E013'.
           05  EC-PIU-OUT-OF-RANGE         PIC X(04) VALUE 'E014'.
           05  EC-RESTATE-NO-BASIS         PIC X(04) VALUE 'E015'.
       01  CABS-SUSPENSE-RECORD.
           05  SU-ERR-CODE                 PIC X(04).
           05  SU-ERR-SEVERITY             PIC X(01).
               88  SU-WARN                 VALUE 'W'.
               88  SU-ERROR                VALUE 'E'.
               88  SU-FATAL                VALUE 'F'.
           05  SU-DETECT-PGM               PIC X(08).
           05  SU-DETECT-PARA              PIC X(30).
           05  SU-RUN-ID                   PIC X(12).
           05  SU-ORIG-RECORD              PIC X(200).
           05  SU-FILLER                   PIC X(45).
