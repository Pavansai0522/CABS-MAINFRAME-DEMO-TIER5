//CABS5000 JOB (CABS,BILL),'INVOICE PAGE AND HEADING FORMAT',
//             CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABS5000 - INVOICE PAGE FORMAT AND HEADING INJECTION          *
//*                                                               *
//* TWO STEPS THROUGH THE SAME PROC.  STEP020 OVERRIDES PRTIN TO  *
//* READ WHAT STEP010 JUST WROTE - THE PROC ITSELF HAS NO PRTIN.  *
//* SHARED MEMBERS ARE CHANGED THROUGH OPERATIONS SUPPORT ONLY.   *
//*                                                               *
//* LINES PER PAGE AND THE COMPANY NAME COME FROM THE FORM AND    *
//* OPERATING COMPANY PROFILE IN USE ON THE NIGHT.  NEITHER HAS   *
//* A DEFAULT.                                                    *
//*****************************************************************
//STEP010  EXEC CABPFMTP,
//             PGMNAME=CABFMT01,
//             CYCLE=%%CYCLDT,
//             BILLPER=%%BILLPR,
//             RUNID=%%RUNID,
//             JOBNM=CABS5000,
//             STEPNM=STEP010,
//             LINES=%%LINPGE,
//             MEDIA=%%MEDIA,
//             BURST=%%BURSTS,
//             INGDG='0',
//             OUTGDG='+1'
//FMTSTEP.SYSIN    DD *
RN%%RUNID  %%CYCLDT%%BILLPR00CABS5000STEP010YY%%LINPGE%%MEDIA %%BURSTSY
/*
//*
//STEP020  EXEC CABPFMTP,
//             PGMNAME=CABFMT02,
//             CYCLE=%%CYCLDT,
//             BILLPER=%%BILLPR,
//             RUNID=%%RUNID,
//             JOBNM=CABS5000,
//             STEPNM=STEP020,
//             LINES=%%LINPGE,
//             COMPANY=%%COMPNY,
//             INGDG='0',
//             OUTGDG='+1'
//FMTSTEP.BDTLIN   DD DUMMY
//FMTSTEP.BHDRIN   DD DUMMY
//FMTSTEP.PRTCTL   DD DUMMY
//FMTSTEP.PRTIN    DD DSN=TELCABS.CABS.PRINT.STREAM(0),DISP=SHR
//FMTSTEP.PRTOUT   DD DSN=TELCABS.CABS.PRINT.HEAD(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(100,50),RLSE),
//             DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//FMTSTEP.SYSIN    DD *
RN%%RUNID  %%CYCLDT%%BILLPR00CABS5000STEP020YY%%LINPGEYN%%COMPNY
/*
//
