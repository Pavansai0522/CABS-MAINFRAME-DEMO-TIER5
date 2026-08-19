//CABJ1650 JOB (CABS,BILL),'RESTATEMENT SIMULATION',
//             CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABJ1650 - RESTATEMENT SIMULATION                             *
//*                                                               *
//* THE SAME PROC AS CABJ1600 WITH SIMSW=Y AND THE ADJUSTMENT     *
//* FILE OVERRIDDEN TO DUMMY.  THE REGISTER IS PRODUCED AND       *
//* NOTHING IS COMMITTED.  RUN BY THE ACCESS MANAGEMENT GROUP     *
//* BEFORE EVERY QUARTERLY RESTATEMENT.                           *
//* SHARED MEMBERS ARE CHANGED THROUGH OPERATIONS SUPPORT ONLY.   *
//* DD OVERRIDES ARE AGREED WITH OPERATIONS SUPPORT.              *
//*****************************************************************
//STEP010  EXEC CABPREST,
//             CYCLE=%%CYCLDT,
//             BILLPER=%%BILLPR,
//             RUNID=%%RUNID,
//             RSTFROM=%%RSTFRM,
//             RSTWIN=%%RSTWIN,
//             REASON=%%REASON,
//             MATRLTY='00000.01',
//             USGGDG='-1',
//             BILGDG='-3',
//             SIMSW=Y,
//             OCNSEL=%%OCNSEL
//*
//RESTATE.ADJOUT  DD DUMMY,DCB=(RECFM=VB,LRECL=1651,BLKSIZE=12040)
//RESTATE.AUDTOUT DD DSN=&&AUDIT,DISP=(NEW,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(50,25)),
//             DCB=(RECFM=FB,LRECL=250,BLKSIZE=0)
//
