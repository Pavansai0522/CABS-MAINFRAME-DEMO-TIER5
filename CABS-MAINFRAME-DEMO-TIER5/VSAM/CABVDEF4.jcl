//CABVDEF4 JOB (CABS,VSAM),'DEFINE MESSAGE AND CONTRACT FILES',
//             CLASS=B,MSGCLASS=X,MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*****************************************************************
//* CABVDEF4 - DEFINE THE BILL MESSAGE AND CONTRACT MINIMUM FILES *
//*                                                               *
//* TWO DATASETS DEFINED IN ONE JOB BECAUSE THEY WERE BOTH ADDED  *
//* IN THE SAME 1993 RELEASE.  NEITHER IS WRITTEN BY ANY PROGRAM  *
//* IN THE ESTATE - BOTH ARE MAINTAINED THROUGH ISPF EDIT.        *
//* ALLOCATION IS HELD IN THIS MEMBER ONLY - SEE CABS-STD-058.   *
//*                                                               *
//* CABFMT09 READS THE MESSAGE FILE AND CABBIL08 READS THE        *
//* CONTRACT FILE.  BOTH PROGRAMS OPEN THEM AS SEQUENTIAL FILES   *
//* EVEN THOUGH THEY ARE DEFINED HERE AS ENTRY SEQUENCED CLUSTERS.*
//*****************************************************************
//STEP010  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE CLUSTER ( -
         NAME(TELCABS.CABS.BILLMSG) -
         NONINDEXED -
         RECORDSIZE(120 120) -
         SHAREOPTIONS(2 3) -
         VOLUMES(CABS02) -
         TRACKS(10 5) ) -
     DATA ( -
         NAME(TELCABS.CABS.BILLMSG.DATA) -
         CISZ(4096) )
  IF LASTCC = 0 THEN -
     DEFINE CLUSTER ( -
            NAME(TELCABS.CABS.CONTRACT.MMX) -
            NONINDEXED -
            RECORDSIZE(80 80) -
            SHAREOPTIONS(2 3) -
            VOLUMES(CABS02) -
            TRACKS(10 5) ) -
        DATA ( -
            NAME(TELCABS.CABS.CONTRACT.MMX.DATA) -
            CISZ(4096) )
/*
//
