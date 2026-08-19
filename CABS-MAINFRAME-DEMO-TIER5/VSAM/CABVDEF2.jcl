//CABVDEF2 JOB (CABS,VSAM),'DEFINE INVOICE NUMBER CONTROL',
//             CLASS=B,MSGCLASS=X,MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*****************************************************************
//* CABVDEF2 - DEFINE TELCABS.CABS.INVCTL                         *
//*                                                               *
//* THE INVOICE NUMBER CONTROL FILE.  ONE RECORD PER CARRIER WITH *
//* THE LAST NUMBER ISSUED.  CABBIL12 OPENS IT I-O AND REWRITES   *
//* IT AT END OF RUN.  IT IS DEFINED HERE AND NOWHERE ELSE.       *
//* ALLOCATION IS HELD IN THIS MEMBER ONLY - SEE CABS-STD-058.   *
//*                                                               *
//* A NEW CARRIER IS ADDED BY REBUILDING THE FILE WITH THIS JOB   *
//* AND A NEW SEED CARD.  THAT IS WHY THE SEED BELOW IS EMPTY -   *
//* THE LIVE SEED IS KEPT SEPARATELY BY THE BILLING CONTROL TEAM  *
//* AND HAS BEEN SINCE THE 1996 REBUILD LOST THE LAST NUMBERS FOR *
//* FOUR CARRIERS.                                                *
//*****************************************************************
//STEP010  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE TELCABS.CABS.INVCTL CLUSTER PURGE
  SET MAXCC = 0
  DEFINE CLUSTER ( -
         NAME(TELCABS.CABS.INVCTL) -
         INDEXED -
         KEYS(4 0) -
         RECORDSIZE(60 60) -
         SHAREOPTIONS(2 3) -
         VOLUMES(CABS01) -
         TRACKS(15 5) -
         FREESPACE(30 20) ) -
     DATA ( -
         NAME(TELCABS.CABS.INVCTL.DATA) -
         CISZ(2048) ) -
     INDEX ( -
         NAME(TELCABS.CABS.INVCTL.INDEX) -
         CISZ(1024) )
/*
//
