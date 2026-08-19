//CABS5200 JOB (CABS,BILL),'INVOICE SUMMARY PAGES',
//             CLASS=B,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABS5200 - INVOICE SUMMARY PAGE BUILD                         *
//*                                                               *
//* THE SUMMARY PAGE IS PRODUCED AS A SEPARATE DOCUMENT AND       *
//* MERGED IN FRONT OF THE DETAIL PAGES BY THE INSERTER, WHICH IS *
//* WHY IT IS A JOB OF ITS OWN AND NOT A STEP OF CABS5100.        *
//*                                                               *
//* THE SUMMARY STYLE COMES FROM THE MEDIA PROFILE AND HAS NO     *
//* DEFAULT.                                                      *
//*****************************************************************
//STEP010  EXEC CABPFMTP,
//             PGMNAME=CABFMT05,
//             CYCLE=%%CYCLDT,
//             BILLPER=%%BILLPR,
//             RUNID=%%RUNID,
//             JOBNM=CABS5200,
//             STEPNM=STEP010,
//             SUMSTYL=%%SUMSTL,
//             INGDG='0',
//             OUTGDG='+1'
//FMTSTEP.BDTLIN   DD DUMMY
//FMTSTEP.PRTCTL   DD DUMMY
//FMTSTEP.PRTOUT   DD DSN=TELCABS.CABS.PRINT.SUMM(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(40,20),RLSE),
//             DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//FMTSTEP.SYSIN    DD *
RN%%RUNID  %%CYCLDT%%BILLPR00CABS5200STEP010YY%%SUMSTL%%RECAPS%%TAXDTL%%JURSHW
/*
//
