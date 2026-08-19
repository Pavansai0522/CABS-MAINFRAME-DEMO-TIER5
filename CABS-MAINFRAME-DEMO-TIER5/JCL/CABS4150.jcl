//CABS4150 JOB (CABS,BILL),'BILL SECTION SEQUENCING',
//             CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABS4150 - BILL SECTION SEQUENCING                            *
//*                                                               *
//* USES THE SAME PROC AS CABS4100 WITH A DIFFERENT PROGRAM AND   *
//* DIFFERENT SYMBOLIC VALUES, AND OVERRIDES FOUR DD STATEMENTS   *
//* BECAUSE THIS PROGRAM READS AND WRITES VARIABLE LENGTH FILES   *
//* WHERE THE GENERIC STEP EXPECTS FIXED ONES.                    *
//* THE PROC LIBRARY IS CATALOGUED PER CABS-STD-024.              *
//*                                                               *
//* THE SUPPRESSION SWITCH IS SET TO N ON THE REGULATORY RE-RUN   *
//* SO THAT EVERY LINE APPEARS.                                   *
//*****************************************************************
//STEP010  EXEC CABPBDTL,
//             PGMNAME=CABBIL03,
//             CYCLE=%%CYCLDT,
//             BILLPER=%%BILLPR,
//             RUNID=%%RUNID,
//             JOBNM=CABS4150,
//             STEPNM=STEP010,
//             MAXELM=40,
//             SUPZER=%%SUPZER,
//             CONTSW=Y,
//             INGDG='0',
//             OUTGDG='+1'
//DTLSTEP.RATIN    DD DUMMY
//DTLSTEP.TRIGIN   DD DUMMY
//DTLSTEP.BDTLIN   DD DSN=TELCABS.CABS.BILLDTL(0),DISP=SHR
//DTLSTEP.BDTLOUT  DD DUMMY
//DTLSTEP.BDTLSEQ  DD DSN=TELCABS.CABS.BILLDTL.SEQ(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(80,40),RLSE),
//             DCB=(RECFM=VB,LRECL=1651,BLKSIZE=0)
//DTLSTEP.SYSIN    DD *
RN%%RUNID  %%CYCLDT%%BILLPR00CABS4150STEP010NY%%SUPZERY00000.00
/*
//
