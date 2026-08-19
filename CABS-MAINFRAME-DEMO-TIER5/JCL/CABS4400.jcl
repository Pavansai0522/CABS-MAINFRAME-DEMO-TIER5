//CABS4400 JOB (CABS,BILL),'TAX AND SURCHARGE CALCULATION',
//             CLASS=B,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABS4400 - TAX AND SURCHARGE CALCULATION                 *
//*                                                               *
//* USES THE GENERIC BILL CALCULATION FRAGMENT WITH FOUR DD       *
//* OVERRIDES BECAUSE THE FRAGMENT ONLY CARRIES THE DD NAMES      *
//* COMMON TO EVERY BILLCALC PROGRAM.                             *
//*                                                               *
//* EVERY VALUE ON THE SYSIN CARD BELOW ARRIVES AS A SCHEDULER    *
//* SYMBOLIC.  NOTHING IS DEFAULTED.                              *
//*****************************************************************
//STEP010  EXEC CABPBILL,
//             PGMNAME=CABBIL07,
//             CYCLE=%%CYCLDT,
//             BILLPER=%%BILLPR,
//             RUNID=%%RUNID,
//             JOBNM=CABS4400,
//             STEPNM=STEP010,
//             INGDG='0',
//             OUTGDG='+1'
//BILLSTEP.BHDRIN  DD DSN=TELCABS.CABS.BILLHDR.USG(0),DISP=SHR
//BILLSTEP.TAXMST  DD DSN=TELCABS.CABS.TAXRATE,DISP=SHR
//BILLSTEP.BHDROUT DD DSN=TELCABS.CABS.BILLHDR.TAX(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(10,5),RLSE),
//             DCB=(RECFM=FB,LRECL=400,BLKSIZE=0)
//BILLSTEP.SYSIN   DD *
RN%%RUNID  %%CYCLDT%%BILLPR00CABS4400STEP010YN%%TAXSW %%FEDRAT%%EXMPSW%%TAXDT
/*
//
