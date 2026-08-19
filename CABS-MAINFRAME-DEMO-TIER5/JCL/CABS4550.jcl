//CABS4550 JOB (CABS,BILL),'BILL LEVEL BALANCING',
//             CLASS=B,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABS4550 - BILL LEVEL BALANCING                               *
//*                                                               *
//* PROVES THAT THE BILL DETAIL ADDS UP TO THE INVOICE HEADER FOR *
//* EVERY ACCOUNT AND WRITES THE PROOF FILE THAT THE DAILY        *
//* BALANCING REPORT CONSUMES.                                    *
//*                                                               *
//* THE TOLERANCE ARRIVES AS A SYMBOLIC.  A VALUE OF ZERO MEANS   *
//* USE THE STANDARD FIVE CENTS, NOT A TOLERANCE OF ZERO.         *
//* VALUES ARE SUPPLIED BY THE SCHEDULER PER CABS-STD-022.        *
//*****************************************************************
//STEP010  EXEC CABPBHDR,
//             PGMNAME=CABBIL11,
//             CYCLE=%%CYCLDT,
//             BILLPER=%%BILLPR,
//             RUNID=%%RUNID,
//             JOBNM=CABS4550,
//             STEPNM=STEP010,
//             TOLER=%%TOLER,
//             INGDG='0',
//             OUTGDG='+1',
//             PRIGDG='-1'
//HDRSTEP.PRIORIN  DD DUMMY
//HDRSTEP.HOLDMST  DD DUMMY
//HDRSTEP.INVCTL   DD DUMMY
//HDRSTEP.HOLDOUT  DD DUMMY
//HDRSTEP.BDTLIN   DD DSN=TELCABS.CABS.BILLDTL.SEQ(0),DISP=SHR
//HDRSTEP.BHDRIN   DD DSN=TELCABS.CABS.BILLHDR.AUD(0),DISP=SHR
//HDRSTEP.PROOFOUT DD DSN=TELCABS.CABS.BILLPROOF(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(10,5),RLSE),
//             DCB=(RECFM=FB,LRECL=120,BLKSIZE=0)
//HDRSTEP.SYSIN    DD *
RN%%RUNID  %%CYCLDT%%BILLPR00CABS4550STEP010YN%%TOLER N%%ELMCHK%%LISTSW
/*
//
