//CABS5100 JOB (CABS,BILL),'AMOUNT EDITING AND TOTALS',
//             CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABS5100 - AMOUNT EDITING AND SECTION TOTALS                  *
//*                                                               *
//* THE EDIT STYLE IS SUBSTITUTED FROM THE MEDIA PROFILE.  TWO    *
//* CARRIERS TAKE THE CR TAG FORM AND THE REST TAKE THE TRAILING  *
//* MINUS.                                                        *
//*                                                               *
//* STEP020 READS THE OUTPUT OF STEP010 THROUGH A DD OVERRIDE.    *
//* OVERRIDE CONVENTIONS PER CABS-STD-024.                        *
//*****************************************************************
//STEP010  EXEC CABPFMTP,
//             PGMNAME=CABFMT03,
//             CYCLE=%%CYCLDT,
//             BILLPER=%%BILLPR,
//             RUNID=%%RUNID,
//             JOBNM=CABS5100,
//             STEPNM=STEP010,
//             EDSTYL=%%EDSTYL,
//             CRSW=%%CRSW,
//             INGDG='0',
//             OUTGDG='+1'
//FMTSTEP.BHDRIN   DD DUMMY
//FMTSTEP.PRTCTL   DD DUMMY
//FMTSTEP.PRTOUT   DD DSN=TELCABS.CABS.PRINT.EDIT(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(120,60),RLSE),
//             DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//FMTSTEP.SYSIN    DD *
RN%%RUNID  %%CYCLDT%%BILLPR00CABS5100STEP010YY%%EDSTYL%%CRSW  YN
/*
//*
//STEP020  EXEC CABPFMTP,
//             PGMNAME=CABFMT04,
//             CYCLE=%%CYCLDT,
//             BILLPER=%%BILLPR,
//             RUNID=%%RUNID,
//             JOBNM=CABS5100,
//             STEPNM=STEP020,
//             INGDG='0',
//             OUTGDG='+1'
//FMTSTEP.BDTLIN   DD DUMMY
//FMTSTEP.BHDRIN   DD DUMMY
//FMTSTEP.PRTCTL   DD DUMMY
//FMTSTEP.PRTIN    DD DSN=TELCABS.CABS.PRINT.EDIT(0),DISP=SHR
//FMTSTEP.PRTOUT   DD DSN=TELCABS.CABS.PRINT.TOT(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(120,60),RLSE),
//             DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//FMTSTEP.SYSIN    DD *
RN%%RUNID  %%CYCLDT%%BILLPR00CABS5100STEP020YY%%SECTOT%%SUBTOT%%CARRY 02
/*
//
