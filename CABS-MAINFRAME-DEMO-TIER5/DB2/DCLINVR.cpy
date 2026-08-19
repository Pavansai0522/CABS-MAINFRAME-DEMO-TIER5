      ******************************************************************
      * DCLGEN TABLE(CABSINVR)                                        *
      *        LIB(TELCABS.CABS.DCLGEN)                                *
      *        ACTION(REPLACE)                                        *
      *        LANGUAGE(COBOL)                                        *
      *        QUOTE                                                  *
      *        COLSUFFIX(NO)                                          *
      *        STRUCTURE(DCLINVR)                                     *
      * ---------------------------------------------------------------*
      * DCLGEN GENERATED AGAINST TELCABS.CABS.DCLGEN, DSNP SUBSYSTEM.  *
      * DO NOT EDIT THE EXEC SQL BLOCK BY HAND - REGENERATE AND        *
      * RE-CATALOG THROUGH CABS-STD-041 CHANGE CONTROL.                *
      ******************************************************************
          EXEC SQL DECLARE CABSINVR TABLE
          ( BAN                            CHAR(13) NOT NULL,
            BILL_PERIOD                    INTEGER NOT NULL,
            OCN                            CHAR(4) NOT NULL,
            INVOICE_NBR                    CHAR(14) NOT NULL,
            BILL_DATE                      DATE NOT NULL,
            DUE_DATE                       DATE NOT NULL,
            PRIOR_BAL                      DECIMAL(15, 2) NOT NULL,
            PAYMENTS                       DECIMAL(15, 2) NOT NULL,
            ADJUSTMENTS                    DECIMAL(15, 2) NOT NULL,
            CURR_USAGE                     DECIMAL(15, 2) NOT NULL,
            CURR_RECURRING                 DECIMAL(15, 2) NOT NULL,
            CURR_NONRECUR                  DECIMAL(15, 2) NOT NULL,
            RESTATEMENT                    DECIMAL(15, 2) NOT NULL,
            SETTLEMENT_NET                 DECIMAL(15, 2) NOT NULL,
            TAX                            DECIMAL(13, 2) NOT NULL,
            TOTAL_DUE                      DECIMAL(15, 2) NOT NULL,
            STATUS                         CHAR(1) NOT NULL,
            HOLD_REASON                    CHAR(4) NOT NULL
          ) END-EXEC.
      ******************************************************************
      *** DB2 DECLARATION FOR TABLE CABSINVR
      ******************************************************************
       01  DCLINVR.
      *** BAN
           10  INVR-BAN                  PIC X(13).
      *** BILL_PERIOD
           10  INVR-BILL-PERIOD          PIC S9(9) COMP.
      *** OCN
           10  INVR-OCN                  PIC X(4).
      *** INVOICE_NBR
           10  INVR-INVOICE-NBR          PIC X(14).
      *** BILL_DATE
           10  INVR-BILL-DATE            PIC X(10).
      *** DUE_DATE
           10  INVR-DUE-DATE             PIC X(10).
      *** PRIOR_BAL
           10  INVR-PRIOR-BAL            PIC S9(13)V9(2) COMP-3.
      *** PAYMENTS
           10  INVR-PAYMENTS             PIC S9(13)V9(2) COMP-3.
      *** ADJUSTMENTS
           10  INVR-ADJUSTMENTS          PIC S9(13)V9(2) COMP-3.
      *** CURR_USAGE
           10  INVR-CURR-USAGE           PIC S9(13)V9(2) COMP-3.
      *** CURR_RECURRING
           10  INVR-CURR-RECURRING       PIC S9(13)V9(2) COMP-3.
      *** CURR_NONRECUR
           10  INVR-CURR-NONRECUR        PIC S9(13)V9(2) COMP-3.
      *** RESTATEMENT
           10  INVR-RESTATEMENT          PIC S9(13)V9(2) COMP-3.
      *** SETTLEMENT_NET
           10  INVR-SETTLEMENT-NET       PIC S9(13)V9(2) COMP-3.
      *** TAX
           10  INVR-TAX                  PIC S9(11)V9(2) COMP-3.
      *** TOTAL_DUE
           10  INVR-TOTAL-DUE            PIC S9(13)V9(2) COMP-3.
      *** STATUS
           10  INVR-STATUS               PIC X(1).
      *** HOLD_REASON
           10  INVR-HOLD-REASON          PIC X(4).
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 18
