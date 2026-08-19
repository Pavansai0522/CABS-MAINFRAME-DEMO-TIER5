      ******************************************************************
      * DCLGEN TABLE(CABSAUDT)                                        *
      *        LIB(TELCABS.CABS.DCLGEN)                                *
      *        ACTION(REPLACE)                                        *
      *        LANGUAGE(COBOL)                                        *
      *        QUOTE                                                  *
      *        COLSUFFIX(NO)                                          *
      *        STRUCTURE(DCLAUDT)                                     *
      * ---------------------------------------------------------------*
      * DCLGEN GENERATED AGAINST TELCABS.CABS.DCLGEN, DSNP SUBSYSTEM.  *
      * DO NOT EDIT THE EXEC SQL BLOCK BY HAND - REGENERATE AND        *
      * RE-CATALOG THROUGH CABS-STD-041 CHANGE CONTROL.                *
      ******************************************************************
          EXEC SQL DECLARE CABSAUDT TABLE
          ( RUN_ID                         CHAR(12) NOT NULL,
            PROCESS_ID                     CHAR(8) NOT NULL,
            STEP_SEQ                       DECIMAL(3, 0) NOT NULL,
            AUDIT_TS                       TIMESTAMP NOT NULL,
            TABLE_NAME                     CHAR(8) NOT NULL,
            ACTION_CD                      CHAR(1) NOT NULL,
            KEY_VALUE                      VARCHAR(60) NOT NULL,
            BEFORE_AMOUNT                  DECIMAL(18, 5) NOT NULL,
            AFTER_AMOUNT                   DECIMAL(18, 5) NOT NULL,
            USERID                         CHAR(8) NOT NULL
          ) END-EXEC.
      ******************************************************************
      *** DB2 DECLARATION FOR TABLE CABSAUDT
      ******************************************************************
       01  DCLAUDT.
      *** RUN_ID
           10  AUDT-RUN-ID               PIC X(12).
      *** PROCESS_ID
           10  AUDT-PROCESS-ID           PIC X(8).
      *** STEP_SEQ
           10  AUDT-STEP-SEQ             PIC S9(3) COMP-3.
      *** AUDIT_TS
           10  AUDT-AUDIT-TS             PIC X(26).
      *** TABLE_NAME
           10  AUDT-TABLE-NAME           PIC X(8).
      *** ACTION_CD
           10  AUDT-ACTION-CD            PIC X(1).
      *** KEY_VALUE
           10  AUDT-KEY-VALUE.
               49  AUDT-KEY-VALUE-LEN    PIC S9(4) COMP.
               49  AUDT-KEY-VALUE-TEXT   PIC X(60).
      *** BEFORE_AMOUNT
           10  AUDT-BEFORE-AMOUNT        PIC S9(13)V9(5) COMP-3.
      *** AFTER_AMOUNT
           10  AUDT-AFTER-AMOUNT         PIC S9(13)V9(5) COMP-3.
      *** USERID
           10  AUDT-USERID               PIC X(8).
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10
