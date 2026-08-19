      ******************************************************************
      * CABSTBL - CABS/SETL DB2 FOR z/OS TABLE DEFINITIONS            *
      * APPLICATION : CABS | SETL                                     *
      * PURPOSE     : DATABASE, STOGROUP, TABLESPACE AND TABLE DDL    *
      *               FOR THE SEVEN TABLES READ OR WRITTEN BY THE     *
      *               DB2 PRECOMPILED MEMBERS OF THE SETTLEMENT AND   *
      *               JURISDICTION FAMILIES, AND BY THE CICS AND      *
      *               PL/I LAYERS THAT MAINTAIN RATE AND FACTOR       *
      *               HISTORY AND THE INVOICE REGISTER.               *
      * REFERENCE-ONLY : THIS SUBSYSTEM DOES NOT RUN UNDER TK4-.      *
      *               DSNP IS THE TARGET SUBSYSTEM ON A FULL Z/OS     *
      *               DB2 FOR z/OS INSTALLATION.  SEE CABS-STD-041.   *
      * REVISION HISTORY                                              *
      *   V1.00  1999-09-02  P.NAIR        INITIAL - SETLPERIOD,      *
      *                                     SETLTRAN, CABSADJ ONLY    *
      *   V1.01  1999-10-04  P.NAIR        ADDED FOR CABSET12 CLOSE   *
      *   V1.03  2001-07-23  P.NAIR        SETLPERIOD COLUMNS ADDED   *
      *   V1.06  2003-04-11  R.OKONKWO     CABSINVR TABLE ADDED       *
      *   V1.08  2005-11-08  P.NAIR        RATE/FACTOR HISTORY ADDED  *
      *                                    FOR THE CICS RATE MAINT    *
      *                                    ON-LINE SET (CABSRTHS,     *
      *                                    CABSFCHS)                  *
      *   V1.10  2007-02-19  A.BUKOWSKI    CABSAUDT AUDIT TABLE ADDED *
      *   V1.12  2011-09-06  A.BUKOWSKI    BUFFERPOOL REASSIGNED BP2  *
      *   V1.13  2015-04-23  L.FERREIRA    SECQTY INCREASED ESTATE-   *
      *                                    WIDE AFTER EXTEND FAILURES *
      *   V1.14  2019-05-14  M.OYELARAN    COLUMN COMMENTS ONLY       *
      ******************************************************************

      ******************************************************************
      * STORAGE GROUP AND DATABASE                                     *
      ******************************************************************
      DROP STOGROUP CABSSG01;

      CREATE STOGROUP CABSSG01
             VOLUMES (DSNVS1, DSNVS2, DSNVS3)
             VCAT TELCABS;
      COMMIT;

      CREATE DATABASE CABSDB01
             STOGROUP CABSSG01
             BUFFERPOOL BP2
             CCSID EBCDIC;
      COMMIT;

      ******************************************************************
      * CABSRTHS - ACCESS RATE HISTORY.  MIRRORS THE VSAM CABSRATE     *
      * KSDS LAYOUT (COPYBOOKS/CABSRATE.CPY) LESS THE OCCURS DEPENDING *
      * ON BAND TABLE, WHICH IS NOT CARRIED INTO DB2.  BAND-LEVEL      *
      * DETAIL REMAINS VSAM-ONLY; THIS TABLE HOLDS THE HEADER PORTION  *
      * FOR HISTORICAL RATE LOOKUP AND FOR THE CICS RATE MAINTENANCE   *
      * TRANSACTION'S BEFORE-IMAGE DISPLAY.                            *
      ******************************************************************
      CREATE TABLESPACE TSRTHS
             IN CABSDB01
             USING STOGROUP CABSSG01
                   PRIQTY 2160
                   SECQTY 720
             ERASE NO
             LOCKSIZE PAGE
             LOCKMAX SYSTEM
             BUFFERPOOL BP2
             SEGSIZE 4
             CLOSE NO;
      COMMIT;

      CREATE TABLE CABSRTHS
           ( TARIFF_CD            CHAR(4)        NOT NULL,
             RATE_ELEM             CHAR(6)        NOT NULL,
             JURIS_CD              CHAR(1)        NOT NULL,
             STATE_CD              CHAR(2)        NOT NULL,
             EFF_YYDDD             DECIMAL(5,0)   NOT NULL,
             EXP_YYDDD             DECIMAL(5,0)   NOT NULL WITH DEFAULT 99365,
             INITIAL_RATE          DECIMAL(10,5)  NOT NULL WITH DEFAULT 0,
             ADDL_RATE             DECIMAL(10,5)  NOT NULL WITH DEFAULT 0,
             ROUND_RULE            CHAR(1)        NOT NULL WITH DEFAULT 'U',
             ROUND_POS             DECIMAL(1,0)   NOT NULL WITH DEFAULT 0,
             BAND_CNT              DECIMAL(2,0)   NOT NULL WITH DEFAULT 0,
             CREATE_TS             TIMESTAMP      NOT NULL WITH DEFAULT,
             CREATE_USERID         CHAR(8)        NOT NULL WITH DEFAULT USER,
             LAST_UPD_TS           TIMESTAMP      NOT NULL WITH DEFAULT,
             LAST_UPD_USERID       CHAR(8)        NOT NULL WITH DEFAULT USER,
             RUN_ID                CHAR(12)       NOT NULL WITH DEFAULT
           )
           IN CABSDB01.TSRTHS
           AUDIT NONE
           CCSID EBCDIC;
      COMMIT;
      -- CABS-STD-014 NOTES THAT A FIELDPROC ON INITIAL_RATE / ADDL_RATE
      -- WAS EVALUATED IN 2005 TO ENFORCE THE FIVE-DECIMAL RATE
      -- CONVENTION AT THE DATA MANAGER LEVEL.  THE DECISION WAS TO RELY
      -- ON THE DECIMAL(10,5) COLUMN DEFINITION INSTEAD.  NO FIELDPROC
      -- OR EDITPROC IS ATTACHED TO THIS TABLE.

      ******************************************************************
      * CABSFCHS - PIU/PLU/PSU JURISDICTIONAL FACTOR HISTORY.  MIRRORS *
      * COPYBOOKS/CABSFCTR.CPY.  THE RESTATEMENT WINDOW AND THE PRIOR  *
      * PIU/PLU VALUES ARE CARRIED SO A RESTATEMENT RUN CAN COMPUTE    *
      * THE DELTA WITHOUT RE-READING THE VSAM SOURCE.                  *
      ******************************************************************
      CREATE TABLESPACE TSFCHS
             IN CABSDB01
             USING STOGROUP CABSSG01
                   PRIQTY 2160
                   SECQTY 720
             ERASE NO
             LOCKSIZE PAGE
             LOCKMAX SYSTEM
             BUFFERPOOL BP2
             SEGSIZE 4
             CLOSE NO;
      COMMIT;

      CREATE TABLE CABSFCHS
           ( OCN                   CHAR(4)        NOT NULL,
             STATE_CD              CHAR(2)        NOT NULL,
             LATA                  DECIMAL(3,0)   NOT NULL,
             EFF_YYDDD             DECIMAL(5,0)   NOT NULL,
             PIU                   DECIMAL(8,5)   NOT NULL WITH DEFAULT 0,
             PLU                   DECIMAL(8,5)   NOT NULL WITH DEFAULT 0,
             PSU                   DECIMAL(8,5)   NOT NULL WITH DEFAULT 0,
             SOURCE_CD             CHAR(1)        NOT NULL WITH DEFAULT 'D',
             RESTATE_SW            CHAR(1)        NOT NULL WITH DEFAULT 'N',
             RESTATE_FROM_YYDDD    DECIMAL(5,0)   NOT NULL WITH DEFAULT 0,
             RESTATE_THRU_YYDDD    DECIMAL(5,0)   NOT NULL WITH DEFAULT 0,
             PRIOR_PIU             DECIMAL(8,5)   NOT NULL WITH DEFAULT 0,
             PRIOR_PLU             DECIMAL(8,5)   NOT NULL WITH DEFAULT 0,
             RECV_YYDDD            DECIMAL(5,0)   NOT NULL WITH DEFAULT 0,
             RUN_ID                CHAR(12)       NOT NULL WITH DEFAULT
           )
           IN CABSDB01.TSFCHS
           AUDIT NONE
           CCSID EBCDIC;
      COMMIT;

      ******************************************************************
      * SETLTRAN - INTER-CARRIER SETTLEMENT LEDGER.  WRITTEN BY         *
      * CABSET13 (SEE BATCH/SETTLE/CABSET13.CBL, PARAGRAPH P3000-      *
      * POST-DB2).  COLUMN NAMES AND TYPES ARE FIXED BY THE HOST        *
      * VARIABLE DECLARATIONS IN THAT PROGRAM - DO NOT RENAME WITHOUT   *
      * CHANGING BOTH.  MIRRORS COPYBOOKS/CABSSETL.CPY.                 *
      ******************************************************************
      CREATE TABLESPACE TSSETT
             IN CABSDB01
             USING STOGROUP CABSSG01
                   PRIQTY 4320
                   SECQTY 1440
             ERASE NO
             LOCKSIZE PAGE
             LOCKMAX SYSTEM
             BUFFERPOOL BP2
             SEGSIZE 4
             CLOSE NO;
      COMMIT;

      CREATE TABLE SETLTRAN
           ( SETTLE_TYPE           CHAR(1)        NOT NULL,
             OCN                   CHAR(4)        NOT NULL,
             SETTLE_PERIOD         INTEGER        NOT NULL,
             SEQ_NBR               INTEGER        NOT NULL,
             TOTAL_MOU             DECIMAL(17,2)  NOT NULL WITH DEFAULT 0,
             BILLABLE_MOU          DECIMAL(17,2)  NOT NULL WITH DEFAULT 0,
             CAPPED_MOU            DECIMAL(17,2)  NOT NULL WITH DEFAULT 0,
             RATE_APPLIED          DECIMAL(10,5)  NOT NULL WITH DEFAULT 0,
             GROSS_AMT             DECIMAL(18,5)  NOT NULL WITH DEFAULT 0,
             OUR_SHARE             DECIMAL(18,5)  NOT NULL WITH DEFAULT 0,
             THEIR_SHARE           DECIMAL(18,5)  NOT NULL WITH DEFAULT 0,
             NET_DUE               DECIMAL(15,2)  NOT NULL WITH DEFAULT 0,
             DIRECTION             CHAR(1)        NOT NULL WITH DEFAULT 'R',
             DISPUTE_SW            CHAR(1)        NOT NULL WITH DEFAULT 'N',
             RUN_ID                CHAR(12)       NOT NULL WITH DEFAULT
           )
           IN CABSDB01.TSSETT
           AUDIT NONE
           CCSID EBCDIC;
      COMMIT;

      ******************************************************************
      * SETLPERIOD - SETTLEMENT PERIOD STATUS.  WRITTEN BY CABSET12    *
      * (SEE BATCH/SETTLE/CABSET12.CBL, PARAGRAPHS P3500-CLOSE-DB2 AND *
      * P3600-INSERT-DB2).  THIS IS THE FIRST OF THE TWO UNCOORDINATED *
      * STORES DESCRIBED IN THAT PROGRAM'S HEADER COMMENT - THE SECOND *
      * IS THE VSAM CLOSE-MASTER KSDS TELCABS.SETL.CLOSE.               *
      ******************************************************************
      CREATE TABLESPACE TSSPRD
             IN CABSDB01
             USING STOGROUP CABSSG01
                   PRIQTY 1440
                   SECQTY 720
             ERASE NO
             LOCKSIZE PAGE
             LOCKMAX SYSTEM
             BUFFERPOOL BP2
             SEGSIZE 4
             CLOSE NO;
      COMMIT;

      CREATE TABLE SETLPERIOD
           ( OCN                   CHAR(4)        NOT NULL,
             SETTLE_PERIOD         INTEGER        NOT NULL,
             RECEIVABLE            DECIMAL(15,2)  NOT NULL WITH DEFAULT 0,
             PAYABLE               DECIMAL(15,2)  NOT NULL WITH DEFAULT 0,
             NET_AMOUNT            DECIMAL(15,2)  NOT NULL WITH DEFAULT 0,
             PERIOD_STATUS         CHAR(1)        NOT NULL WITH DEFAULT 'O',
             CLOSE_DATE            DATE           NOT NULL WITH DEFAULT,
             RUN_ID                CHAR(12)       NOT NULL WITH DEFAULT
           )
           IN CABSDB01.TSSPRD
           AUDIT NONE
           CCSID EBCDIC;
      COMMIT;

      ******************************************************************
      * CABSADJ - RESTATEMENT ADJUSTMENT POSTING.  WRITTEN BY CABJUR10 *
      * (SEE BATCH/JURIS/CABJUR10.CBL, PARAGRAPH P3000-POST-DB2).      *
      * THE DUPLICATE KEY TOLERANCE DESCRIBED IN THAT PROGRAM'S        *
      * COMMENTS DEPENDS ON THE PRIMARY KEY BELOW BEING EXACTLY        *
      * BAN/BILL_PERIOD/SECTION_CD/LINE_SEQ - DO NOT WIDEN IT.         *
      ******************************************************************
      CREATE TABLESPACE TSADJ
             IN CABSDB01
             USING STOGROUP CABSSG01
                   PRIQTY 2880
                   SECQTY 1440
             ERASE NO
             LOCKSIZE PAGE
             LOCKMAX SYSTEM
             BUFFERPOOL BP2
             SEGSIZE 4
             CLOSE NO;
      COMMIT;

      CREATE TABLE CABSADJ
           ( BAN                   CHAR(13)       NOT NULL,
             BILL_PERIOD           INTEGER        NOT NULL,
             SECTION_CD            CHAR(2)        NOT NULL,
             LINE_SEQ              INTEGER        NOT NULL,
             OCN                   CHAR(4)        NOT NULL WITH DEFAULT,
             JURIS_CD              CHAR(1)        NOT NULL WITH DEFAULT,
             STATE_CD              CHAR(2)        NOT NULL WITH DEFAULT,
             DESCRIPTION           CHAR(60)       NOT NULL WITH DEFAULT,
             MINUTES               DECIMAL(15,2)  NOT NULL WITH DEFAULT 0,
             AMOUNT                DECIMAL(18,5)  NOT NULL WITH DEFAULT 0,
             AMOUNT_ROUNDED        DECIMAL(15,2)  NOT NULL WITH DEFAULT 0,
             POST_DATE             DATE           NOT NULL WITH DEFAULT,
             REASON_CD             CHAR(2)        NOT NULL WITH DEFAULT,
             RUN_ID                CHAR(12)       NOT NULL WITH DEFAULT
           )
           IN CABSDB01.TSADJ
           AUDIT NONE
           CCSID EBCDIC;
      COMMIT;

      ******************************************************************
      * CABSINVR - INVOICE REGISTER.  MIRRORS COPYBOOKS/CABSBHDR.CPY.  *
      * POPULATED BY THE ON-LINE BILL INQUIRY CICS TRANSACTION FROM    *
      * THE BILL HEADER SUMMARY WRITTEN BY THE BILLING BATCH STREAM.   *
      ******************************************************************
      CREATE TABLESPACE TSINVR
             IN CABSDB01
             USING STOGROUP CABSSG01
                   PRIQTY 4320
                   SECQTY 1440
             ERASE NO
             LOCKSIZE PAGE
             LOCKMAX SYSTEM
             BUFFERPOOL BP2
             SEGSIZE 4
             CLOSE NO;
      COMMIT;

      CREATE TABLE CABSINVR
           ( BAN                   CHAR(13)       NOT NULL,
             BILL_PERIOD           INTEGER        NOT NULL,
             OCN                   CHAR(4)        NOT NULL WITH DEFAULT,
             INVOICE_NBR           CHAR(14)       NOT NULL WITH DEFAULT,
             BILL_DATE             DATE           NOT NULL WITH DEFAULT,
             DUE_DATE              DATE           NOT NULL WITH DEFAULT,
             PRIOR_BAL             DECIMAL(15,2)  NOT NULL WITH DEFAULT 0,
             PAYMENTS              DECIMAL(15,2)  NOT NULL WITH DEFAULT 0,
             ADJUSTMENTS           DECIMAL(15,2)  NOT NULL WITH DEFAULT 0,
             CURR_USAGE            DECIMAL(15,2)  NOT NULL WITH DEFAULT 0,
             CURR_RECURRING        DECIMAL(15,2)  NOT NULL WITH DEFAULT 0,
             CURR_NONRECUR         DECIMAL(15,2)  NOT NULL WITH DEFAULT 0,
             RESTATEMENT           DECIMAL(15,2)  NOT NULL WITH DEFAULT 0,
             SETTLEMENT_NET        DECIMAL(15,2)  NOT NULL WITH DEFAULT 0,
             TAX                   DECIMAL(13,2)  NOT NULL WITH DEFAULT 0,
             TOTAL_DUE             DECIMAL(15,2)  NOT NULL WITH DEFAULT 0,
             STATUS                CHAR(1)        NOT NULL WITH DEFAULT 'P',
             HOLD_REASON           CHAR(4)        NOT NULL WITH DEFAULT
           )
           IN CABSDB01.TSINVR
           AUDIT NONE
           CCSID EBCDIC;
      COMMIT;

      ******************************************************************
      * CABSAUDT - AUDIT TRAIL.  ONE ROW PER MAINTENANCE ACTION ACROSS *
      * ALL SEVEN TABLES IN THIS DATABASE.  WRITTEN BY THE CICS RATE   *
      * AND FACTOR MAINTENANCE TRANSACTIONS AND BY CABRTMNT.PLI FOR    *
      * BATCH-SIDE RATE TABLE CHANGES.  KEY_VALUE IS VARCHAR BECAUSE   *
      * THE SEVEN TABLES DO NOT SHARE A COMMON KEY LENGTH.             *
      ******************************************************************
      CREATE TABLESPACE TSAUDT
             IN CABSDB01
             USING STOGROUP CABSSG01
                   PRIQTY 4320
                   SECQTY 2160
             ERASE NO
             LOCKSIZE PAGE
             LOCKMAX SYSTEM
             BUFFERPOOL BP2
             SEGSIZE 4
             CLOSE NO;
      COMMIT;

      CREATE TABLE CABSAUDT
           ( RUN_ID                CHAR(12)       NOT NULL,
             PROCESS_ID            CHAR(8)        NOT NULL,
             STEP_SEQ              DECIMAL(3,0)   NOT NULL,
             AUDIT_TS              TIMESTAMP      NOT NULL WITH DEFAULT,
             TABLE_NAME            CHAR(8)        NOT NULL WITH DEFAULT,
             ACTION_CD             CHAR(1)        NOT NULL WITH DEFAULT,
             KEY_VALUE             VARCHAR(60)    NOT NULL WITH DEFAULT,
             BEFORE_AMOUNT         DECIMAL(18,5)  NOT NULL WITH DEFAULT 0,
             AFTER_AMOUNT          DECIMAL(18,5)  NOT NULL WITH DEFAULT 0,
             USERID                CHAR(8)        NOT NULL WITH DEFAULT
           )
           IN CABSDB01.TSAUDT
           AUDIT NONE
           CCSID EBCDIC;
      COMMIT;
