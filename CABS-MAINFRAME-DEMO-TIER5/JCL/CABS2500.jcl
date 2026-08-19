//CABS2500 JOB (CABS,BILL),'RECIP COMP CALCULATION',
//             CLASS=B,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABS2500 - RECIPROCAL COMPENSATION CALCULATION                *
//*                                                               *
//* APPLIES THE NEGOTIATED RATE AND THE ISP BOUND CAP.  THE CAP   *
//* OVERRIDE ON THE CONTROL CARD IS SUPPLIED BY THE SCHEDULER     *
//* AND A VALUE OF ZERO MEANS USE THE AGREEMENT CAP, NOT A CAP    *
//* OF ZERO MINUTES.                                              *
//*                                                               *
//* RECIPCDR READS TELCABS.CABS.CDR.RECIP - A DATASET OWNED BY    *
//* THE BILLING APPLICATION.                                      *
//*****************************************************************
//STEP010  EXEC CABPRECP,
//             PGMNAME=CABSET05,
//             CYCLE=%%CYCLDT,
//             BILLPER=%%BILLPR,
//             RUNID=%%RUNID,
//             SETPER=%%SETPER,
//             DEFRATE=%%DEFRAT,
//             CAPOVR=%%CAPOVR,
//             STATE=%%STATE,
//             SIMSW=N,
//             INGDG='0'
//*
//RECPSTEP.RECIPIN  DD DSN=TELCABS.SETL.RECIP.AGG(0),DISP=SHR
//RECPSTEP.RECIPCDR DD DSN=TELCABS.CABS.CDR.RECIP(0),DISP=SHR
//RECPSTEP.SETLOUT  DD DSN=TELCABS.SETL.SETTLE.RECIP(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(30,15),RLSE),
//             DCB=(RECFM=FB,LRECL=180,BLKSIZE=0)
//RECPSTEP.CAPOUT   DD DSN=TELCABS.SETL.RECIP.CAPPED(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(30,15),RLSE),
//             DCB=(RECFM=FB,LRECL=120,BLKSIZE=0)
//RECPSTEP.SYSIN    DD *
RN%%RUNID  %%CYCLDT%%BILLPR00CABS2500RECPSTEPNN%%SETPER%%DEFRAT
/*
//
