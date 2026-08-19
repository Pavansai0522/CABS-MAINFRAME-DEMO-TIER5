//CABS4500 JOB (CABS,BILL),'PRE BILL AUDIT AND HOLD',
//             CLASS=B,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABS4500 - PRE BILL AUDIT AND HOLD                       *
//*                                                               *
//* USES THE GENERIC BILL CALCULATION FRAGMENT WITH FOUR DD       *
//* OVERRIDES BECAUSE THE FRAGMENT ONLY CARRIES THE DD NAMES      *
//* COMMON TO EVERY BILLCALC PROGRAM.                             *
//*                                                               *
//* EVERY VALUE ON THE SYSIN CARD BELOW ARRIVES AS A SCHEDULER    *
//* SYMBOLIC.  NOTHING IS DEFAULTED.                              *
//*****************************************************************
//STEP010  EXEC CABPBILL,
//             PGMNAME=CABBIL10,
//             CYCLE=%%CYCLDT,
//             BILLPER=%%BILLPR,
//             RUNID=%%RUNID,
//             JOBNM=CABS4500,
//             STEPNM=STEP010,
//             INGDG='0',
//             OUTGDG='+1'
//BILLSTEP.BHDRIN  DD DSN=TELCABS.CABS.BILLHDR.MMX(0),DISP=SHR
//BILLSTEP.PRIORIN DD DSN=TELCABS.CABS.BILLHDR.FIN(-1),DISP=SHR
//BILLSTEP.HOLDMST DD DSN=TELCABS.CABS.HOLDRSN,DISP=SHR
//BILLSTEP.BHDROUT DD DSN=TELCABS.CABS.BILLHDR.AUD(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(10,5),RLSE),
//             DCB=(RECFM=FB,LRECL=400,BLKSIZE=0)
//BILLSTEP.HOLDOUT DD DSN=TELCABS.CABS.BILLHOLD(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(30,15),RLSE),
//             DCB=(RECFM=FB,LRECL=400,BLKSIZE=0)
//BILLSTEP.SYSIN   DD *
RN%%RUNID  %%CYCLDT%%BILLPR00CABS4500STEP010YY%%AUDTSW%%VARPCT%%MINLIN%%HLDLIM
/*
//
