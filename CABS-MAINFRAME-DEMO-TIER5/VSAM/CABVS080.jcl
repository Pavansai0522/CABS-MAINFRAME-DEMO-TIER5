//CABVS080 JOB (CABS,DBA),'BILL HEADER KSDS DEFINE',
//             CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=4M,TIME=(10,0)
//*****************************************************************
//* CABVS080 - DEFINE THE BILL HEADER / INVOICE SUMMARY KSDS      *
//*                                                               *
//* DEFINES TELCABS.CABS.BILLHDR, KEYED ON BH-KEY (19 BYTES: BAN  *
//* + BILL PERIOD) PER CABSBHDR.  THIS IS THE RECORD THE BILL-TO- *
//* BILL COMPARISON KEYS ON - THE MOST HEAVILY READ MASTER IN THE *
//* ESTATE OUTSIDE OF BATCH WINDOWS.                               *
//*                                                                *
//* SHAREOPTIONS(4 3) IS CORRECT, NOT A TYPO - IT IS THE ONLY      *
//* CLUSTER IN THE ESTATE AT THIS SETTING.  THE BILLING INQUIRY    *
//* CICS REGION READS THIS CLUSTER WHILE THE NIGHTLY BILL RUN      *
//* WRITES IT; (4 3) WAS CHOSEN OVER (2 3) SO CICS DOES NOT HAVE   *
//* TO WAIT FOR BUFFER INVALIDATION BETWEEN BATCH COMMITS.         *
//*                                                                *
//* REVISION HISTORY                                               *
//*   1990-11-02  D.OKONKWO     INITIAL DEFINITION                 *
//*   1996-01-15  J.M.CASTILLO  Y2K REVIEW - NO IMPACT              *
//*   2005-08-19  A.BUKOWSKI    SHAREOPTIONS CHANGED FROM (2 3)     *
//*                             TO (4 3) FOR THE INQUIRY CICS       *
//*                             REGION GO-LIVE                      *
//*   2012-03-30  S.MARCHETTI   IMBED DROPPED AT RECOMPILE - DASD   *
//*                             CONTROLLER NO LONGER HONOURED IT     *
//*****************************************************************
//STEP010  EXEC PGM=IDCAMS,REGION=4M
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  SET MAXCC = 0
  DELETE TELCABS.CABS.BILLHDR CLUSTER PURGE
  DEFINE CLUSTER -
      (NAME(TELCABS.CABS.BILLHDR) -
       INDEXED -
       KEYS(19 0) -
       RECORDSIZE(400 400) -
       FREESPACE(15 20) -
       SHAREOPTIONS(4 3) -
       VOLUMES(TELV06 TELV07) -
       CYLINDERS(80 40) -
       RECOVERY) -
      DATA -
      (NAME(TELCABS.CABS.BILLHDR.DATA) -
       CISZ(4096)) -
      INDEX -
      (NAME(TELCABS.CABS.BILLHDR.INDEX) -
       CISZ(2048))
  IF LASTCC > 4 THEN -
    SET MAXCC = 12
  ELSE -
    SET MAXCC = 0
/*
//*
//* STEP020 - LISTCAT VERIFICATION.  OPERATIONS CONFIRMS THE      *
//* SHAREOPTIONS VALUE HERE BEFORE THE CICS REGION IS BROUGHT UP  *
//* AGAINST A REBUILT CLUSTER.                                    *
//*
//STEP020  EXEC PGM=IDCAMS,REGION=4M,COND=(4,LT)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  LISTCAT ENTRIES(TELCABS.CABS.BILLHDR) ALL
/*
//
