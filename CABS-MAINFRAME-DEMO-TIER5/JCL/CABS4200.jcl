//CABS4200 JOB (CABS,BILL),'PRIOR BALANCE AND PAYMENTS',
//             CLASS=B,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABS4200 - PRIOR BALANCE AND PAYMENTS                    *
//*                                                               *
//* USES THE GENERIC BILL CALCULATION FRAGMENT WITH FOUR DD       *
//* OVERRIDES BECAUSE THE FRAGMENT ONLY CARRIES THE DD NAMES      *
//* COMMON TO EVERY BILLCALC PROGRAM.                             *
//*                                                               *
//* EVERY VALUE ON THE SYSIN CARD BELOW ARRIVES AS A SCHEDULER    *
//* SYMBOLIC.  NOTHING IS DEFAULTED.                              *
//*****************************************************************
//STEP010  EXEC CABPBILL,
//             PGMNAME=CABBIL04,
//             CYCLE=%%CYCLDT,
//             BILLPER=%%BILLPR,
//             RUNID=%%RUNID,
//             JOBNM=CABS4200,
//             STEPNM=STEP010,
//             INGDG='0',
//             OUTGDG='+1'
//BILLSTEP.BALIN   DD DSN=TELCABS.CABS.BALANCE(0),DISP=SHR
//BILLSTEP.PAYIN   DD DSN=TELCABS.CABS.PAYMENT(0),DISP=SHR
//BILLSTEP.BHDROUT DD DSN=TELCABS.CABS.BILLHDR(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(10,5),RLSE),
//             DCB=(RECFM=FB,LRECL=400,BLKSIZE=0)
//BILLSTEP.SYSIN   DD *
RN%%RUNID  %%CYCLDT%%BILLPR00CABS4200STEP010%%APPORDY%%AGEBAS%%WOFLIM
/*
//
