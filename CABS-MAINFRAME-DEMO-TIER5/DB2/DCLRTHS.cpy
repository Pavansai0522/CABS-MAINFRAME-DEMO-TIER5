      ******************************************************************
      * DCLGEN TABLE(CABSRTHS)                                        *
      *        LIB(TELCABS.CABS.DCLGEN)                                *
      *        ACTION(REPLACE)                                        *
      *        LANGUAGE(COBOL)                                        *
      *        QUOTE                                                  *
      *        COLSUFFIX(NO)                                          *
      *        STRUCTURE(DCLRTHS)                                     *
      * ---------------------------------------------------------------*
      * DCLGEN GENERATED AGAINST TELCABS.CABS.DCLGEN, DSNP SUBSYSTEM.  *
      * DO NOT EDIT THE EXEC SQL BLOCK BY HAND - REGENERATE AND        *
      * RE-CATALOG THROUGH CABS-STD-041 CHANGE CONTROL.                *
      ******************************************************************
          EXEC SQL DECLARE CABSRTHS TABLE
          ( TARIFF_CD                     CHAR(4) NOT NULL,
            RATE_ELEM                     CHAR(6) NOT NULL,
            JURIS_CD                      CHAR(1) NOT NULL,
            STATE_CD                      CHAR(2) NOT NULL,
            EFF_YYDDD                     DECIMAL(5, 0) NOT NULL,
            EXP_YYDDD                     DECIMAL(5, 0) NOT NULL,
            INITIAL_RATE                  DECIMAL(10, 5) NOT NULL,
            ADDL_RATE                     DECIMAL(10, 5) NOT NULL,
            ROUND_RULE                    CHAR(1) NOT NULL,
            ROUND_POS                     DECIMAL(1, 0) NOT NULL,
            BAND_CNT                      DECIMAL(2, 0) NOT NULL,
            CREATE_TS                     TIMESTAMP NOT NULL,
            CREATE_USERID                 CHAR(8) NOT NULL,
            LAST_UPD_TS                   TIMESTAMP NOT NULL,
            LAST_UPD_USERID               CHAR(8) NOT NULL,
            RUN_ID                        CHAR(12) NOT NULL
          ) END-EXEC.
      ******************************************************************
      *** DB2 DECLARATION FOR TABLE CABSRTHS
      ******************************************************************
       01  DCLRTHS.
      *** TARIFF_CD
           10  RTHS-TARIFF-CD            PIC X(4).
      *** RATE_ELEM
           10  RTHS-RATE-ELEM            PIC X(6).
      *** JURIS_CD
           10  RTHS-JURIS-CD             PIC X(1).
      *** STATE_CD
           10  RTHS-STATE-CD             PIC X(2).
      *** EFF_YYDDD
           10  RTHS-EFF-YYDDD            PIC S9(5) COMP-3.
      *** EXP_YYDDD
           10  RTHS-EXP-YYDDD            PIC S9(5) COMP-3.
      *** INITIAL_RATE
           10  RTHS-INITIAL-RATE         PIC S9(5)V9(5) COMP-3.
      *** ADDL_RATE
           10  RTHS-ADDL-RATE            PIC S9(5)V9(5) COMP-3.
      *** ROUND_RULE
           10  RTHS-ROUND-RULE           PIC X(1).
      *** ROUND_POS
           10  RTHS-ROUND-POS            PIC S9(1) COMP-3.
      *** BAND_CNT
           10  RTHS-BAND-CNT             PIC S9(2) COMP-3.
      *** CREATE_TS
           10  RTHS-CREATE-TS            PIC X(26).
      *** CREATE_USERID
           10  RTHS-CREATE-USERID        PIC X(8).
      *** LAST_UPD_TS
           10  RTHS-LAST-UPD-TS          PIC X(26).
      *** LAST_UPD_USERID
           10  RTHS-LAST-UPD-USERID      PIC X(8).
      *** RUN_ID
           10  RTHS-RUN-ID               PIC X(12).
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 16
