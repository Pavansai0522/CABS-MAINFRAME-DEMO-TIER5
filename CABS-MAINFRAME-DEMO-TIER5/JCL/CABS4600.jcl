//CABS4600 JOB (CABS,BILL),'FINAL INVOICE NUMBERING',
//             CLASS=B,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABS4600 - FINAL INVOICE NUMBERING                            *
//*                                                               *
//* THE LAST STEP OF THE BILL CALCULATION STREAM.  ONCE AN        *
//* INVOICE IS NUMBERED IT CANNOT BE RECALCULATED, ONLY CANCELLED *
//* AND REISSUED.                                                 *
//*                                                               *
//* THE PREFIX AND THE STARTING SEQUENCE ARE SUBSTITUTED FROM THE *
//* REGIONAL NUMBERING STANDARD.  AN ABSENT PREFIX ABENDS THE     *
//* STEP.                                                         *
//*                                                               *
//* INVCTL IS A VSAM KSDS DEFINED BY CABVDEF2.  NO PROGRAM IN THE *
//* ESTATE CREATES IT.                                            *
//*****************************************************************
//STEP010  EXEC CABPBHDR,
//             PGMNAME=CABBIL12,
//             CYCLE=%%CYCLDT,
//             BILLPER=%%BILLPR,
//             RUNID=%%RUNID,
//             JOBNM=CABS4600,
//             STEPNM=STEP010,
//             PREFIX=%%PREFIX,
//             SEQSTR=%%SEQSTR,
//             INGDG='0',
//             OUTGDG='+1'
//HDRSTEP.PRIORIN  DD DUMMY
//HDRSTEP.HOLDMST  DD DUMMY
//HDRSTEP.HOLDOUT  DD DUMMY
//HDRSTEP.BHDRIN   DD DSN=TELCABS.CABS.BILLHDR.AUD(0),DISP=SHR
//HDRSTEP.INVCTL   DD DSN=TELCABS.CABS.INVCTL,DISP=OLD
//HDRSTEP.BHDROUT  DD DSN=TELCABS.CABS.BILLHDR.FIN(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(10,5),RLSE),
//             DCB=(RECFM=FB,LRECL=400,BLKSIZE=0)
//HDRSTEP.SYSIN    DD *
RN%%RUNID  %%CYCLDT%%BILLPR00CABS4600STEP010YN%%PREFIX%%SEQSTRY%%RENUM
/*
//
