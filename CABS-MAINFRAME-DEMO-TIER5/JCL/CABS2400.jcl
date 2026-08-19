//CABS2400 JOB (CABS,BILL),'RECIP COMP AGGREGATION',
//             CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABS2400 - RECIPROCAL COMPENSATION MOU AGGREGATION            *
//*                                                               *
//* AGGREGATES TERMINATING MINUTES BY COUNTERPARTY AND SPLITS     *
//* THEM INTO ISP BOUND AND VOICE.  THE SPLIT DECIDES WHICH       *
//* MINUTES ARE SUBJECT TO THE CAP IN CABS2500.                   *
//* SHARED MEMBERS ARE CHANGED THROUGH OPERATIONS SUPPORT ONLY.   *
//*****************************************************************
//STEP010  EXEC CABPRECP,
//             PGMNAME=CABSET04,
//             CYCLE=%%CYCLDT,
//             BILLPER=%%BILLPR,
//             RUNID=%%RUNID,
//             SETPER=%%SETPER,
//             INGDG='0'
//*
//RECPSTEP.RECIPIN DD DSN=TELCABS.SETL.RECIP.USAGE(0),DISP=SHR
//RECPSTEP.SYSIN   DD *
RN%%RUNID  %%CYCLDT%%BILLPR00CABS2400RECPSTEPNN%%SETPERISPBND
/*
//
