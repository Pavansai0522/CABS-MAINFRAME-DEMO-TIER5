//CABVS010 JOB (CABS,DBA),'CDR MASTER KSDS DEFINE',
//             CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=4M,TIME=(10,0)
//*****************************************************************
//* CABVS010 - DEFINE THE WHOLESALE ACCESS CDR INPUT MASTER KSDS  *
//*                                                               *
//* DEFINES TELCABS.CABS.CDR.KSDS, THE KEYED USAGE RECORD FILE    *
//* BEHIND THE OVERNIGHT MEDIATION LOAD.  KEY IS THE FULL CD-KEY  *
//* FROM CABSCDR (OCN + BAN + SEQUENCE NUMBER).  THIS IS THE      *
//* LARGEST CLUSTER IN THE ESTATE - SIZE TO THE PEAK BILLING      *
//* MONTH, NOT THE AVERAGE MONTH, PER CABS-STD-041.               *
//*                                                               *
//* THE KEY LENGTH BELOW (26) HAS CARRIED FORWARD UNCHANGED SINCE *
//* THE ORIGINAL 1987 DEFINITION.  DO NOT ADJUST WITHOUT DBA      *
//* SIGN-OFF - DOWNSTREAM RESTART LOGIC ASSUMES IT.               *
//*                                                               *
//* REVISION HISTORY                                              *
//*   1987-03-18  R.T.WHEELER   INITIAL DEFINITION - SINGLE       *
//*                             VOLUME, 400 CYLINDERS             *
//*   1991-09-02  D.OKONKWO     CYLINDER ALLOCATION TRIPLED AND   *
//*                             SPREAD ACROSS THREE VOLUMES FOR   *
//*                             SPECIAL ACCESS VOLUME GROWTH      *
//*   1996-02-01  J.M.CASTILLO  Y2K STORAGE REVIEW - NO CHANGE    *
//*   2004-11-15  A.BUKOWSKI    IMBED RETAINED PENDING DASD       *
//*                             CONTROLLER UPGRADE (STILL         *
//*                             PENDING AS OF THIS REVISION)      *
//*   2013-06-20  S.MARCHETTI   RECOMPILE-EQUIVALENT - VOLSER     *
//*                             STANDARDISED TO THE TELVnn RANGE  *
//*****************************************************************
//STEP010  EXEC PGM=IDCAMS,REGION=4M
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  SET MAXCC = 0
  DELETE TELCABS.CABS.CDR.KSDS CLUSTER PURGE
  DEFINE CLUSTER -
      (NAME(TELCABS.CABS.CDR.KSDS) -
       INDEXED -
       KEYS(26 0) -
       RECORDSIZE(200 200) -
       FREESPACE(10 15) -
       SHAREOPTIONS(2 3) -
       VOLUMES(TELV01 TELV02 TELV03) -
       CYLINDERS(1200 300) -
       IMBED -
       REPLICATE -
       SPEED) -
      DATA -
      (NAME(TELCABS.CABS.CDR.KSDS.DATA) -
       CISZ(4096)) -
      INDEX -
      (NAME(TELCABS.CABS.CDR.KSDS.INDEX) -
       CISZ(2048))
  IF LASTCC > 4 THEN -
    SET MAXCC = 12
  ELSE -
    SET MAXCC = 0
/*
//*
//* STEP020 - LISTCAT VERIFICATION.  OPERATIONS COMPARES THE      *
//* HIGH-USED-RBA AGAINST THE PRIOR CYCLE'S FIGURE BEFORE THE     *
//* INGEST STREAM IS RELEASED FOR SUBMISSION.  COND=(4,LT) SKIPS  *
//* THIS STEP IF THE DEFINE ABOVE FAILED OUTRIGHT.                *
//*
//STEP020  EXEC PGM=IDCAMS,REGION=4M,COND=(4,LT)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  LISTCAT ENTRIES(TELCABS.CABS.CDR.KSDS) ALL
/*
//
