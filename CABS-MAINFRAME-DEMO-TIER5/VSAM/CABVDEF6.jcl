//CABVDEF6 JOB (CABS,VSAM),'DEFINE PERIOD CLOSE MASTER',
//             CLASS=B,MSGCLASS=X,MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*****************************************************************
//* CABVDEF6 - DEFINE TELCABS.CABS.CLOSEMST                       *
//*                                                               *
//* THE PERIOD CLOSE MASTER.  CABRPT08 WRITES ONE RECORD PER      *
//* PERIOD PER LEDGER COMPANY.  THE GENERAL LEDGER INTERFACE      *
//* READS IT AND THAT INTERFACE IS NOT PART OF THIS ESTATE - THE  *
//* CLOSE RECORD IS THE ONLY THING THAT CROSSES THE BOUNDARY.     *
//* ALLOCATION IS HELD IN THIS MEMBER ONLY - SEE CABS-STD-058.   *
//*                                                               *
//* THE CLUSTER IS DEFINED WITH SHAREOPTIONS 3 3 BECAUSE THE      *
//* LEDGER INTERFACE READS IT WHILE THE CLOSE IS STILL RUNNING.   *
//* THAT WAS AGREED IN 1996 AND HAS NEVER BEEN REVISITED.         *
//*****************************************************************
//STEP010  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE CLUSTER ( -
         NAME(TELCABS.CABS.CLOSEMST) -
         INDEXED -
         KEYS(30 0) -
         RECORDSIZE(200 200) -
         SHAREOPTIONS(3 3) -
         VOLUMES(CABS01) -
         TRACKS(30 15) -
         FREESPACE(10 10) ) -
     DATA ( -
         NAME(TELCABS.CABS.CLOSEMST.DATA) -
         CISZ(4096) ) -
     INDEX ( -
         NAME(TELCABS.CABS.CLOSEMST.INDEX) -
         CISZ(2048) )
  IF LASTCC = 0 THEN -
     LISTCAT ENTRIES(TELCABS.CABS.CLOSEMST) ALL
/*
//
