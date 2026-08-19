      ******************************************************************
      * CABSIDX - CABS/SETL DB2 INDEX DEFINITIONS                     *
      * APPLICATION : CABS | SETL                                     *
      * PURPOSE     : UNIQUE INDEX (= TABLE PRIMARY KEY) FOR EACH OF   *
      *               THE SEVEN TABLES IN CABSTBL.DDL, PLUS TWO        *
      *               NON-UNIQUE SECONDARY INDEXES SUPPORTING          *
      *               COUNTERPARTY-LEVEL AND OCN-LEVEL REPORTING.      *
      * REFERENCE-ONLY : SEE CABSTBL.DDL HEADER.                       *
      * REVISION HISTORY                                              *
      *   V1.00  1999-09-02  P.NAIR        UNIQUE INDEXES, THREE       *
      *                                    ORIGINAL TABLES ONLY        *
      *   V1.05  2005-11-08  P.NAIR        RATE/FACTOR HISTORY INDEXES *
      *   V1.06  2006-01-30  P.NAIR        XINVR02 ADDED FOR THE OCN   *
      *                                    EXCEPTION SUMMARY REPORT    *
      *   V1.10  2007-02-19  A.BUKOWSKI    CABSAUDT INDEX ADDED        *
      *   V1.11  2011-09-06  A.BUKOWSKI    XSETT02 ADDED - COUNTERPARTY*
      *                                    POSITION INQUIRY, CICS      *
      *   V1.12  2019-05-14  M.OYELARAN    COLUMN COMMENTS ONLY        *
      ******************************************************************

      ******************************************************************
      * UNIQUE INDEXES - ONE PER TABLE, MATCHES THE STATED PRIMARY KEY *
      ******************************************************************
      CREATE UNIQUE INDEX XRTHS01
             ON CABSRTHS (TARIFF_CD, RATE_ELEM, JURIS_CD, STATE_CD,
                          EFF_YYDDD)
             USING STOGROUP CABSSG01
                   PRIQTY 720
                   SECQTY 360
             BUFFERPOOL BP2
             CLOSE NO;
      COMMIT;

      CREATE UNIQUE INDEX XFCHS01
             ON CABSFCHS (OCN, STATE_CD, LATA, EFF_YYDDD)
             USING STOGROUP CABSSG01
                   PRIQTY 720
                   SECQTY 360
             BUFFERPOOL BP2
             CLOSE NO;
      COMMIT;

      CREATE UNIQUE INDEX XSETT01
             ON SETLTRAN (SETTLE_TYPE, OCN, SETTLE_PERIOD, SEQ_NBR)
             USING STOGROUP CABSSG01
                   PRIQTY 1440
                   SECQTY 720
             BUFFERPOOL BP2
             CLOSE NO;
      COMMIT;

      CREATE UNIQUE INDEX XSPRD01
             ON SETLPERIOD (OCN, SETTLE_PERIOD)
             USING STOGROUP CABSSG01
                   PRIQTY 360
                   SECQTY 180
             BUFFERPOOL BP2
             CLOSE NO;
      COMMIT;

      CREATE UNIQUE INDEX XADJ01
             ON CABSADJ (BAN, BILL_PERIOD, SECTION_CD, LINE_SEQ)
             USING STOGROUP CABSSG01
                   PRIQTY 1440
                   SECQTY 720
             BUFFERPOOL BP2
             CLOSE NO;
      COMMIT;

      CREATE UNIQUE INDEX XINVR01
             ON CABSINVR (BAN, BILL_PERIOD)
             USING STOGROUP CABSSG01
                   PRIQTY 1440
                   SECQTY 720
             BUFFERPOOL BP2
             CLOSE NO;
      COMMIT;

      CREATE UNIQUE INDEX XAUDT01
             ON CABSAUDT (RUN_ID, PROCESS_ID, STEP_SEQ, AUDIT_TS,
                          TABLE_NAME)
             USING STOGROUP CABSSG01
                   PRIQTY 1440
                   SECQTY 720
             BUFFERPOOL BP2
             CLOSE NO;
      COMMIT;

      ******************************************************************
      * NON-UNIQUE SECONDARY INDEXES                                  *
      ******************************************************************

      -- SUPPORTS THE CICS COUNTERPARTY POSITION INQUIRY TRANSACTION,
      -- WHICH LOOKS UP ALL SETTLEMENT KINDS (M/R/C) FOR AN OCN AND
      -- PERIOD WITHOUT KNOWING THE SEQUENCE NUMBER.
      CREATE INDEX XSETT02
             ON SETLTRAN (OCN, SETTLE_PERIOD)
             USING STOGROUP CABSSG01
                   PRIQTY 1440
                   SECQTY 720
             BUFFERPOOL BP2
             CLOSE NO;
      COMMIT;

      -- ADDED 2006-01-30 FOR THE OCN-LEVEL BILLING EXCEPTION SUMMARY
      -- REPORT (RPTCAB22, RUN OUT OF TELCABS.CABS.RPTLIB).  RPTCAB22
      -- WAS RETIRED WHEN THE EXCEPTION SUMMARY MOVED TO THE ON-LINE
      -- BILL INQUIRY SCREEN IN 2009.  RETAINED - CABS-STD-041 REQUIRES
      -- A DROP INDEX TO GO THROUGH THE SAME CHANGE BOARD AS A DDL ADD.
      CREATE INDEX XINVR02
             ON CABSINVR (OCN, BILL_PERIOD)
             USING STOGROUP CABSSG01
                   PRIQTY 1440
                   SECQTY 720
             BUFFERPOOL BP2
             CLOSE NO;
      COMMIT;
