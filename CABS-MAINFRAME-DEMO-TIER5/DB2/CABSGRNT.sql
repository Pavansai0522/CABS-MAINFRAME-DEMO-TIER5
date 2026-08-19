      ******************************************************************
      * CABSGRNT - AUTHORIZATION GRANTS FOR THE CABS/SETL DATABASE     *
      * APPLICATION : CABS | SETL                                     *
      * PURPOSE     : PRODUCTION RUNTIME AUTHORITY FOR THE BATCH       *
      *               EXECUTION IDS AND THE ON-LINE CICS REGION.       *
      * REFERENCE-ONLY : SEE CABSTBL.DDL HEADER.                       *
      * REVISION HISTORY                                              *
      *   V1.00  1999-09-02  P.NAIR        INITIAL GRANTS - SETLBAT    *
      *   V1.02  2001-07-23  P.NAIR        CABSBAT ADDED (CABJUR10)    *
      *   V1.04  1998-11-19  R.OKONKWO     PUBLIC GRANT FOR THE FCC    *
      *                                    PART 61 AUDIT - SEE NOTE    *
      *                                    BELOW.  DATE OUT OF ORDER   *
      *                                    IN THIS HISTORY BECAUSE THE *
      *                                    GRANT WAS BACK-DATED TO     *
      *                                    MATCH THE AUDIT FIELDWORK   *
      *                                    PERIOD WHEN IT WAS ADDED    *
      *                                    TO THIS MEMBER IN 2001.     *
      *   V1.06  2005-11-08  P.NAIR        CABSONL ADDED FOR THE RATE  *
      *                                    MAINTENANCE CICS REGION     *
      *   V1.10  2007-02-19  A.BUKOWSKI    CABSAUDT GRANTS ADDED       *
      *   V1.13  2019-05-14  M.OYELARAN    COLUMN COMMENTS ONLY        *
      ******************************************************************

      ******************************************************************
      * CABSBAT - BATCH EXECUTION ID FOR THE CABS APPLICATION          *
      * (CABJUR07, CABJUR08, CABJUR10 AND THE PL/I RATE UTILITIES).   *
      ******************************************************************
      GRANT SELECT, INSERT, UPDATE, DELETE
            ON TABLE CABSADJ
            TO CABSBAT;

      GRANT SELECT, INSERT, UPDATE
            ON TABLE CABSRTHS
            TO CABSBAT;

      GRANT SELECT, INSERT, UPDATE
            ON TABLE CABSFCHS
            TO CABSBAT;

      GRANT SELECT
            ON TABLE CABSINVR
            TO CABSBAT;

      GRANT SELECT, INSERT
            ON TABLE CABSAUDT
            TO CABSBAT;

      ******************************************************************
      * SETLBAT - BATCH EXECUTION ID FOR THE SETL APPLICATION          *
      * (CABSET12, CABSET13 AND THE SETTLEMENT RECONCILIATION SUITE). *
      ******************************************************************
      GRANT SELECT, INSERT, UPDATE
            ON TABLE SETLPERIOD
            TO SETLBAT;

      GRANT SELECT, INSERT, UPDATE
            ON TABLE SETLTRAN
            TO SETLBAT;

      GRANT SELECT
            ON TABLE CABSADJ
            TO SETLBAT;

      GRANT SELECT, INSERT
            ON TABLE CABSAUDT
            TO SETLBAT;

      ******************************************************************
      * CABSONL - CICS REGION EXECUTION ID FOR THE ON-LINE RATE AND    *
      * FACTOR MAINTENANCE TRANSACTIONS AND THE COUNTERPARTY POSITION  *
      * INQUIRY TRANSACTION.  NO DELETE AUTHORITY - RATE AND FACTOR    *
      * ROWS ARE EXPIRED BY DATE, NOT REMOVED, PER CABS-STD-022.       *
      ******************************************************************
      GRANT SELECT, INSERT, UPDATE
            ON TABLE CABSRTHS
            TO CABSONL;

      GRANT SELECT, INSERT, UPDATE
            ON TABLE CABSFCHS
            TO CABSONL;

      GRANT SELECT
            ON TABLE SETLTRAN
            TO CABSONL;

      GRANT SELECT
            ON TABLE SETLPERIOD
            TO CABSONL;

      GRANT SELECT
            ON TABLE CABSINVR
            TO CABSONL;

      GRANT SELECT, INSERT
            ON TABLE CABSAUDT
            TO CABSONL;

      ******************************************************************
      * PUBLIC GRANT - TEMPORARY FOR THE 1998 FCC PART 61 RATE FILING  *
      * AUDIT.  THE EXTERNAL AUDIT TEAM'S READ-ONLY QUERY TOOL RUNS    *
      * UNDER A POOL OF GENERIC USERIDS THAT COULD NOT BE INDIVIDUALLY *
      * PROVISIONED IN TIME FOR THE FIELDWORK START DATE.  SCOPED TO   *
      * SELECT ONLY, AND TO THE RATE HISTORY TABLE ONLY, SINCE THAT IS *
      * THE ONLY TABLE THE AUDIT SCRIPT TOUCHES.  TO BE REVOKED AT     *
      * FIELDWORK CLOSE PER THE AUDIT ENGAGEMENT LETTER.               *
      ******************************************************************
      GRANT SELECT
            ON TABLE CABSRTHS
            TO PUBLIC;
