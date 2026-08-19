//CABVS100 JOB (CABS,DBA),'SETTLEMENT MASTER KSDS DEFINE',
//             CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=4M,TIME=(10,0)
//*****************************************************************
//* CABVS100 - DEFINE THE INTER-CARRIER SETTLEMENT MASTER KSDS    *
//*                                                               *
//* DEFINES TELCABS.SETL.MASTER, KEYED ON ST-KEY (SETTLEMENT TYPE *
//* + COUNTERPARTY OCN + SETTLE PERIOD + SEQUENCE) PER CABSSETL.  *
//* ONE LAYOUT COVERS MEET-POINT BILLING, RECIPROCAL COMPENSATION *
//* AND CMDS/RAO EXCHANGE - ST-SETTLE-TYPE DISCRIMINATES.         *
//*                                                                *
//* NOTE THE APPLICATION QUALIFIER IS SETL, NOT CABS - THIS        *
//* CLUSTER SITS UNDER THE SETTLEMENT DBA POOL, NOT THE ACCESS     *
//* BILLING POOL, EVEN THOUGH THE JOB LIVES IN THE SAME VSAM       *
//* DEFINITION LIBRARY AS THE CABS MASTERS.                        *
//*                                                                *
//* REVISION HISTORY                                               *
//*   1992-03-17  D.OKONKWO     INITIAL DEFINITION - MEET-POINT    *
//*                             BILLING ONLY                       *
//*   1998-07-22  J.M.CASTILLO  RECIPROCAL COMPENSATION KIND       *
//*                             ADDED - RECORD LENGTH UNCHANGED     *
//*   2003-09-08  A.BUKOWSKI    CMDS/RAO EXCHANGE KIND ADDED        *
//*   2015-02-18  L.FERREIRA    VOLSER TELV09 RETAINED BELOW       *
//*                             DESPITE THE 2015 DASD CONSOLIDATION*
//*                             - SMS ACS ROUTINES REDIRECT THE    *
//*                             ALLOCATION SO THE VOLSER ON THE    *
//*                             DEFINE IS ADVISORY ONLY             *
//*****************************************************************
//STEP010  EXEC PGM=IDCAMS,REGION=4M
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  SET MAXCC = 0
  DELETE TELCABS.SETL.MASTER CLUSTER PURGE
  DEFINE CLUSTER -
      (NAME(TELCABS.SETL.MASTER) -
       INDEXED -
       KEYS(16 0) -
       RECORDSIZE(186 186) -
       FREESPACE(15 20) -
       SHAREOPTIONS(3 3) -
       VOLUMES(TELV09) -
       CYLINDERS(40 20)) -
      DATA -
      (NAME(TELCABS.SETL.MASTER.DATA) -
       CISZ(4096)) -
      INDEX -
      (NAME(TELCABS.SETL.MASTER.INDEX) -
       CISZ(1024))
  IF LASTCC > 4 THEN -
    SET MAXCC = 12
  ELSE -
    SET MAXCC = 0
/*
//*
//* STEP020 - LISTCAT VERIFICATION.                               *
//*
//STEP020  EXEC PGM=IDCAMS,REGION=4M,COND=(4,LT)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  LISTCAT ENTRIES(TELCABS.SETL.MASTER) ALL
/*
//
