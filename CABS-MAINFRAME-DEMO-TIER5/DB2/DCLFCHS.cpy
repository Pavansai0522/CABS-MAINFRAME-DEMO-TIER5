      ******************************************************************
      * DCLGEN TABLE(CABSFCHS)                                        *
      *        LIB(TELCABS.CABS.DCLGEN)                                *
      *        ACTION(REPLACE)                                        *
      *        LANGUAGE(COBOL)                                        *
      *        QUOTE                                                  *
      *        COLSUFFIX(NO)                                          *
      *        STRUCTURE(DCLFCHS)                                     *
      * ---------------------------------------------------------------*
      * DCLGEN GENERATED AGAINST TELCABS.CABS.DCLGEN, DSNP SUBSYSTEM.  *
      * DO NOT EDIT THE EXEC SQL BLOCK BY HAND - REGENERATE AND        *
      * RE-CATALOG THROUGH CABS-STD-041 CHANGE CONTROL.                *
      ******************************************************************
          EXEC SQL DECLARE CABSFCHS TABLE
          ( OCN                            CHAR(4) NOT NULL,
            STATE_CD                       CHAR(2) NOT NULL,
            LATA                           DECIMAL(3, 0) NOT NULL,
            EFF_YYDDD                      DECIMAL(5, 0) NOT NULL,
            PIU                            DECIMAL(8, 5) NOT NULL,
            PLU                            DECIMAL(8, 5) NOT NULL,
            PSU                            DECIMAL(8, 5) NOT NULL,
            SOURCE_CD                      CHAR(1) NOT NULL,
            RESTATE_SW                     CHAR(1) NOT NULL,
            RESTATE_FROM_YYDDD             DECIMAL(5, 0) NOT NULL,
            RESTATE_THRU_YYDDD             DECIMAL(5, 0) NOT NULL,
            PRIOR_PIU                      DECIMAL(8, 5) NOT NULL,
            PRIOR_PLU                      DECIMAL(8, 5) NOT NULL,
            RECV_YYDDD                     DECIMAL(5, 0) NOT NULL,
            RUN_ID                         CHAR(12) NOT NULL
          ) END-EXEC.
      ******************************************************************
      *** DB2 DECLARATION FOR TABLE CABSFCHS
      ******************************************************************
       01  DCLFCHS.
      *** OCN
           10  FCHS-OCN                  PIC X(4).
      *** STATE_CD
           10  FCHS-STATE-CD             PIC X(2).
      *** LATA
           10  FCHS-LATA                 PIC S9(3) COMP-3.
      *** EFF_YYDDD
           10  FCHS-EFF-YYDDD            PIC S9(5) COMP-3.
      *** PIU
           10  FCHS-PIU                  PIC S9(3)V9(5) COMP-3.
      *** PLU
           10  FCHS-PLU                  PIC S9(3)V9(5) COMP-3.
      *** PSU
           10  FCHS-PSU                  PIC S9(3)V9(5) COMP-3.
      *** SOURCE_CD
           10  FCHS-SOURCE-CD            PIC X(1).
      *** RESTATE_SW
           10  FCHS-RESTATE-SW           PIC X(1).
      *** RESTATE_FROM_YYDDD
           10  FCHS-RESTATE-FROM-YYDDD   PIC S9(5) COMP-3.
      *** RESTATE_THRU_YYDDD
           10  FCHS-RESTATE-THRU-YYDDD   PIC S9(5) COMP-3.
      *** PRIOR_PIU
           10  FCHS-PRIOR-PIU            PIC S9(3)V9(5) COMP-3.
      *** PRIOR_PLU
           10  FCHS-PRIOR-PLU            PIC S9(3)V9(5) COMP-3.
      *** RECV_YYDDD
           10  FCHS-RECV-YYDDD           PIC S9(5) COMP-3.
      *** RUN_ID
           10  FCHS-RUN-ID               PIC X(12).
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 15
