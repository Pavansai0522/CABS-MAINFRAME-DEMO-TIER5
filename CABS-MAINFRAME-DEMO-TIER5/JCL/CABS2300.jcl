//CABS2300 JOB (CABS,BILL),'MEET POINT PCT VALIDATION',
//             CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABS2300 - MEET POINT PERCENTAGE VALIDATION                   *
//*                                                               *
//* SECOND USE OF THE MEET POINT FRAGMENT WITH A DIFFERENT        *
//* PROGRAM, A DIFFERENT INPUT AND A DIFFERENT TOLERANCE.         *
//* SHARED MEMBERS ARE CHANGED THROUGH OPERATIONS SUPPORT ONLY.   *
//* DD OVERRIDES ARE AGREED WITH OPERATIONS SUPPORT.              *
//*****************************************************************
//STEP010  EXEC CABPMPBX,
//             PGMNAME=CABSET03,
//             CYCLE=%%CYCLDT,
//             BILLPER=%%BILLPR,
//             RUNID=%%RUNID,
//             SETPER=%%SETPER,
//             INDD=MPBEXT,
//             OUTDD=MPB.VALID,
//             TOLER=%%TOLER
//*
//MPBSTEP.CIRCMAST DD DUMMY
//MPBSTEP.CDRIN    DD DUMMY,DCB=(RECFM=FB,LRECL=200,BLKSIZE=2000)
//MPBSTEP.MPBEXT   DD DSN=TELCABS.SETL.MPB.EXTRACT(0),DISP=SHR
//MPBSTEP.MPBVAL   DD DSN=TELCABS.SETL.MPB.VALID(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(20,10),RLSE),
//             DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//MPBSTEP.SYSIN    DD *
RN%%RUNID  %%CYCLDT%%BILLPR00CABS2300MPBSTEP NN%%TOLER N
/*
//
