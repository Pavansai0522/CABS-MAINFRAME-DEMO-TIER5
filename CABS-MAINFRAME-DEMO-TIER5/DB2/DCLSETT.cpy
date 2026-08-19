      ******************************************************************
      * DCLGEN TABLE(SETLTRAN)                                        *
      *        LIB(TELCABS.SETL.DCLGEN)                                *
      *        ACTION(REPLACE)                                        *
      *        LANGUAGE(COBOL)                                        *
      *        QUOTE                                                  *
      *        COLSUFFIX(NO)                                          *
      *        STRUCTURE(DCLSETT)                                     *
      * ---------------------------------------------------------------*
      * DCLGEN GENERATED AGAINST TELCABS.SETL.DCLGEN, DSNP SUBSYSTEM.  *
      * CABSET13 DECLARES ITS OWN HOST VARIABLES IN LINE RATHER THAN   *
      * COPYING THIS MEMBER - THE COLUMN NAMES AND TYPES BELOW MUST    *
      * STAY IN STEP WITH THAT PROGRAM'S WS-HV- GROUP BY HAND.  THIS   *
      * DCLGEN EXISTS FOR THE CICS COUNTERPARTY POSITION INQUIRY       *
      * TRANSACTION AND FOR ANY FUTURE BATCH MEMBER THAT NEEDS THE     *
      * TABLE DECLARATION READY-MADE.                                 *
      ******************************************************************
          EXEC SQL DECLARE SETLTRAN TABLE
          ( SETTLE_TYPE                    CHAR(1) NOT NULL,
            OCN                            CHAR(4) NOT NULL,
            SETTLE_PERIOD                  INTEGER NOT NULL,
            SEQ_NBR                        INTEGER NOT NULL,
            TOTAL_MOU                      DECIMAL(17, 2) NOT NULL,
            BILLABLE_MOU                   DECIMAL(17, 2) NOT NULL,
            CAPPED_MOU                     DECIMAL(17, 2) NOT NULL,
            RATE_APPLIED                   DECIMAL(10, 5) NOT NULL,
            GROSS_AMT                      DECIMAL(18, 5) NOT NULL,
            OUR_SHARE                      DECIMAL(18, 5) NOT NULL,
            THEIR_SHARE                    DECIMAL(18, 5) NOT NULL,
            NET_DUE                        DECIMAL(15, 2) NOT NULL,
            DIRECTION                      CHAR(1) NOT NULL,
            DISPUTE_SW                     CHAR(1) NOT NULL,
            RUN_ID                         CHAR(12) NOT NULL
          ) END-EXEC.
      ******************************************************************
      *** DB2 DECLARATION FOR TABLE SETLTRAN
      ******************************************************************
       01  DCLSETT.
      *** SETTLE_TYPE
           10  SETT-SETTLE-TYPE          PIC X(1).
      *** OCN
           10  SETT-OCN                  PIC X(4).
      *** SETTLE_PERIOD
           10  SETT-SETTLE-PERIOD        PIC S9(9) COMP.
      *** SEQ_NBR
           10  SETT-SEQ-NBR              PIC S9(9) COMP.
      *** TOTAL_MOU
           10  SETT-TOTAL-MOU            PIC S9(15)V9(2) COMP-3.
      *** BILLABLE_MOU
           10  SETT-BILLABLE-MOU         PIC S9(15)V9(2) COMP-3.
      *** CAPPED_MOU
           10  SETT-CAPPED-MOU           PIC S9(15)V9(2) COMP-3.
      *** RATE_APPLIED
           10  SETT-RATE-APPLIED         PIC S9(5)V9(5) COMP-3.
      *** GROSS_AMT
           10  SETT-GROSS-AMT            PIC S9(13)V9(5) COMP-3.
      *** OUR_SHARE
           10  SETT-OUR-SHARE            PIC S9(13)V9(5) COMP-3.
      *** THEIR_SHARE
           10  SETT-THEIR-SHARE          PIC S9(13)V9(5) COMP-3.
      *** NET_DUE
           10  SETT-NET-DUE              PIC S9(13)V9(2) COMP-3.
      *** DIRECTION
           10  SETT-DIRECTION            PIC X(1).
      *** DISPUTE_SW
           10  SETT-DISPUTE-SW           PIC X(1).
      *** RUN_ID
           10  SETT-RUN-ID               PIC X(12).
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 15
