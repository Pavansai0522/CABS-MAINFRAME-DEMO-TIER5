//CABS4350 JOB (CABS,BILL),'MINIMUM AND MAXIMUM BILL',
//             CLASS=B,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABS4350 - MINIMUM AND MAXIMUM BILL ENFORCEMENT               *
//*                                                               *
//* THE TARIFF MINIMUM AND MAXIMUM ARE SUBSTITUTED BY THE         *
//* SCHEDULER FROM THE CURRENT FILING.  NEITHER IS CODED HERE.    *
//* VALUES ARE SUPPLIED BY THE SCHEDULER PER CABS-STD-022.        *
//*****************************************************************
//STEP010  EXEC CABPBILL,
//             PGMNAME=CABBIL08,
//             CYCLE=%%CYCLDT,
//             BILLPER=%%BILLPR,
//             RUNID=%%RUNID,
//             JOBNM=CABS4350,
//             STEPNM=STEP010,
//             INGDG='0',
//             OUTGDG='+1'
//BILLSTEP.BHDRIN  DD DSN=TELCABS.CABS.BILLHDR.TAX(0),DISP=SHR
//BILLSTEP.MMXIN   DD DSN=TELCABS.CABS.CONTRACT.MMX,DISP=SHR
//BILLSTEP.BHDROUT DD DSN=TELCABS.CABS.BILLHDR.MMX(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(10,5),RLSE),
//             DCB=(RECFM=FB,LRECL=400,BLKSIZE=0)
//BILLSTEP.SYSIN   DD *
RN%%RUNID  %%CYCLDT%%BILLPR00CABS4350STEP010YY%%MMXSW %%MINBIL%%MAXBIL%%MKUPSW
/*
//
