//CABS4000 JOB (CABS,BILL),'BILL TRIGGER AND CYCLE SELECT',
//             CLASS=B,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABS4000 - BILL TRIGGER AND CYCLE SELECTION                   *
//*                                                               *
//* FIRST STEP OF THE BILL CALCULATION STREAM.  DECIDES WHICH     *
//* ACCOUNTS BILL ON THIS CYCLE DATE.                             *
//*                                                               *
//* THE FORCE SWITCH AND THE OCN RANGE ARE SUBSTITUTED BY THE     *
//* SCHEDULER FROM WHAT THE BILLING SUPERVISOR TYPED ON THE       *
//* NIGHT.  NEITHER HAS A DEFAULT.                                *
//*                                                               *
//* STEP020 SORTS THE ACCOUNT EXTRACT.  THE SELECTION RULE FOR    *
//* THE SORT IS IN CABSRT09 AND NOWHERE ELSE.                     *
//*****************************************************************
//STEP010  EXEC PGM=SORT,REGION=4M
//SORTIN   DD DSN=TELCABS.CABS.ACCOUNT.RAW(0),DISP=SHR
//SORTOUT  DD DSN=TELCABS.CABS.ACCOUNT(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(20,10),RLSE),
//             DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//SORTWK01 DD UNIT=SYSDA,SPACE=(CYL,(20,10))
//SORTWK02 DD UNIT=SYSDA,SPACE=(CYL,(20,10))
//SYSOUT   DD SYSOUT=*
//SYSIN    DD DSN=TELCABS.CABS.CTLCARDS(CABSRT09),DISP=SHR
//*
//STEP020  EXEC CABPBILL,
//             PGMNAME=CABBIL01,
//             CYCLE=%%CYCLDT,
//             BILLPER=%%BILLPR,
//             RUNID=%%RUNID,
//             JOBNM=CABS4000,
//             STEPNM=STEP020,
//             OPT1=%%FORCSW,
//             OPT2=N,
//             INGDG='0',
//             OUTGDG='+1'
//BILLSTEP.ACCTIN  DD DSN=TELCABS.CABS.ACCOUNT(0),DISP=SHR
//BILLSTEP.CARRMST DD DSN=TELCABS.CABS.CARRIER,DISP=SHR
//BILLSTEP.TRIGOUT DD DSN=TELCABS.CABS.BILLTRIG(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(10,5),RLSE),
//             DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//BILLSTEP.SKPOUT  DD DSN=TELCABS.CABS.BILLSKIP(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(10,5),RLSE),
//             DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//BILLSTEP.SYSIN   DD *
RN%%RUNID  %%CYCLDT%%BILLPR00CABS4000STEP020%%FORCSWN%%OCNFRM%%OCNTHR%%DUEDAY
/*
//
