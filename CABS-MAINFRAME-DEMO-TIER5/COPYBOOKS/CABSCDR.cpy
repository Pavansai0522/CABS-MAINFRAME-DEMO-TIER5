      *****************************************************************
      * CABSCDR  - WHOLESALE ACCESS USAGE RECORD (EMI-DERIVED)        *
      * LRECL 0200  RECFM FB                                          *
      *                                                               *
      * REVISION HISTORY                                              *
      *   V1.00  1987-03-11  R.T.WHEELER   INITIAL - MOU ONLY         *
      *   V1.04  1991-08-22  D.OKONKWO     ADDED SPECIAL ACCESS RDF   *
      *   V2.00  1996-01-15  J.M.CASTILLO  Y2K REVIEW - NO IMPACT     *
      *   V2.03  2001-11-06  P.NAIR        DATA SVC REDEFINE ADDED    *
      *   V2.07  2009-04-30  A.BUKOWSKI    IP TRANSIT FIELDS (UNUSED) *
      *   V2.09  2016-02-18  L.FERREIRA    RECOMPILE ONLY - LE V6     *
      *                                                               *
      * WARNING - THE THREE USAGE VARIANTS SHARE THE SAME 96 BYTES.   *
      * CD-USAGE-TYPE MUST BE TESTED BEFORE REFERENCING ANY VARIANT.  *
      * SEE CABS-STD-014.  NOT ALL PROGRAMS COMPLY.                   *
      *****************************************************************
       01  CABS-CDR-RECORD.
           05  CD-KEY.
               10  CD-OCN                  PIC X(04).
               10  CD-BAN                  PIC X(13).
               10  CD-SEQ-NBR              PIC 9(09) COMP-3.
           05  CD-RECORD-CTL.
               10  CD-REC-TYPE             PIC X(02).
                   88  CD-VOICE-MOU        VALUE '01' '02' '03'.
                   88  CD-DATA-SVC         VALUE '03' '04' '05'.
                   88  CD-SPECIAL-ACC      VALUE '05' '06'.
                   88  CD-UNBUNDLED        VALUE '07'.
                   88  CD-RECIP-COMP       VALUE '08'.
                   88  CD-VALID-TYPE       VALUE '01' THRU '08'.
               10  CD-USAGE-TYPE           PIC X(01).
               10  CD-JURIS-CD             PIC X(01).
                   88  CD-INTERSTATE       VALUE 'I'.
                   88  CD-INTRASTATE       VALUE 'S'.
                   88  CD-LOCAL            VALUE 'L'.
                   88  CD-INDETERMINATE    VALUE 'X' ' '.
               10  CD-RATE-ELEM            PIC X(06).
           05  CD-DATE-TIME.
               10  CD-CONN-YYDDD.
                   15  CD-CONN-YY          PIC 9(02).
                   15  CD-CONN-DDD         PIC 9(03).
               10  CD-CONN-HHMMSS          PIC 9(06).
               10  CD-DISC-YYDDD           PIC 9(05).
               10  CD-DISC-HHMMSS          PIC 9(06).
           05  CD-VARIANT-AREA             PIC X(96).
           05  CD-VOICE-DETAIL REDEFINES CD-VARIANT-AREA.
               10  CD-VC-ORIG-NPANXX       PIC 9(06).
               10  CD-VC-TERM-NPANXX       PIC 9(06).
               10  CD-VC-ORIG-LATA         PIC 9(03).
               10  CD-VC-TERM-LATA         PIC 9(03).
               10  CD-VC-CONV-MIN          PIC S9(07)V9(02) COMP-3.
               10  CD-VC-CHG-MIN           PIC S9(07)V9(02) COMP-3.
               10  CD-VC-TANDEM-IND        PIC X(01).
               10  CD-VC-TRUNK-GRP         PIC X(08).
               10  CD-VC-CIC               PIC 9(04).
               10  CD-VC-END-OFFICE        PIC X(11).
               10  CD-VC-FILLER            PIC X(44).
           05  CD-DATA-DETAIL REDEFINES CD-VARIANT-AREA.
               10  CD-DT-CIRCUIT-ID        PIC X(20).
               10  CD-DT-BANDWIDTH         PIC 9(09) COMP-3.
               10  CD-DT-OCTETS-IN         PIC S9(15)    COMP-3.
               10  CD-DT-OCTETS-OUT        PIC S9(15)    COMP-3.
               10  CD-DT-CoS               PIC X(04).
               10  CD-DT-A-LOC             PIC X(11).
               10  CD-DT-Z-LOC             PIC X(11).
               10  CD-DT-FILLER            PIC X(29).
           05  CD-SPCL-DETAIL REDEFINES CD-VARIANT-AREA.
               10  CD-SP-CIRCUIT-ID        PIC X(20).
               10  CD-SP-USOC              PIC X(05).
               10  CD-SP-QTY               PIC S9(05)    COMP-3.
               10  CD-SP-TERM-MONTHS       PIC 9(03).
               10  CD-SP-MPB-IND           PIC X(01).
               10  CD-SP-MPB-PCT           PIC S9(03)V9(05) COMP-3.
               10  CD-SP-OTHER-LEC         PIC X(04).
               10  CD-SP-FILLER            PIC X(55).
           05  CD-AUDIT.
               10  CD-SRC-SYSTEM           PIC X(04).
               10  CD-LOAD-YYDDD           PIC 9(05).
               10  CD-EDIT-STATUS          PIC X(01).
                   88  CD-CLEAN            VALUE ' ' '0'.
                   88  CD-SUSPECT          VALUE '1' THRU '5'.
                   88  CD-FATAL            VALUE '6' THRU '9'.
               10  CD-FILLER               PIC X(40).
