//CABS4300 JOB (CABS,BILL),'SETTLEMENT NETTING INTO BILL',
//             CLASS=B,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABS4300 - SETTLEMENT NETTING INTO THE INVOICE                *
//*                                                               *
//* USES THE SAME PROC AS CABS4250 WITH THE SETTLEMENT HALF OF    *
//* THE SYMBOLIC LIST AND THE ADJUSTMENT DD SET TO DUMMY.         *
//* SHARED MEMBERS ARE CHANGED THROUGH OPERATIONS SUPPORT ONLY.   *
//*                                                               *
//* SETLIN POINTS AT TELCABS.SETL.NET, OWNED BY THE SETTLEMENT    *
//* APPLICATION.  THE SETTLEMENT CYCLE RUNS A MONTH BEHIND THE    *
//* BILL CYCLE, SO THE GENERATION READ IS (-1), NOT (0).          *
//* THE RETENTION LIMIT IS SET IN THE DATASET REGISTER.           *
//*****************************************************************
//STEP010  EXEC CABPBADJ,
//             PGMNAME=CABBIL06,
//             CYCLE=%%CYCLDT,
//             BILLPER=%%BILLPR,
//             RUNID=%%RUNID,
//             JOBNM=CABS4300,
//             STEPNM=STEP010,
//             NETSW=%%NETSW,
//             RESIDSW=%%RESIDS,
//             SETPER=%%SETPER,
//             MINNET=%%MINNET,
//             INGDG='0',
//             OUTGDG='+1'
//ADJSTEP.ADJIN    DD DUMMY
//ADJSTEP.SETLIN   DD DSN=TELCABS.SETL.NET(-1),DISP=SHR
//ADJSTEP.BHDRIN   DD DSN=TELCABS.CABS.BILLHDR.ADJ(0),DISP=SHR
//ADJSTEP.BHDROUT  DD DSN=TELCABS.CABS.BILLHDR.SET(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(10,5),RLSE),
//             DCB=(RECFM=FB,LRECL=400,BLKSIZE=0)
//ADJSTEP.SYSIN    DD *
RN%%RUNID  %%CYCLDT%%BILLPR00CABS4300STEP010YY%%NETSW %%RESIDS%%SETPER%%MINNET
/*
//
