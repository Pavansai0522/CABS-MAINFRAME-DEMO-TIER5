//CABJ1300 JOB (CABS,BILL),'PIU APPLICATION',
//             CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABJ1300 - PERCENT INTERSTATE USAGE APPLICATION               *
//*                                                               *
//* SAME PROC AS CABJ1200, DIFFERENT PROGRAM AND DIFFERENT FILES. *
//* THE INPUT IS THE OUTPUT OF THE PREVIOUS JOB, PICKED UP BY     *
//* GENERATION POSITION RATHER THAN BY NAME.  IF CABJ1200 RAN     *
//* TWICE THIS JOB READS THE SECOND RUN, NOT THE FIRST.           *
//* GDG RELATIVE NUMBERING PER CABS-STD-026.                      *
//* DD OVERRIDES ARE AGREED WITH OPERATIONS SUPPORT.              *
//*****************************************************************
//STEP010  EXEC CABPJURS,
//             PGMNAME=CABJUR04,
//             CYCLE=%%CYCLDT,
//             BILLPER=%%BILLPR,
//             RUNID=%%RUNID,
//             STEPSEQ=040,
//             OUTDD=CDR.PIU,
//             INGDG='0'
//*
//JURSTEP.CDRIN  DD DSN=TELCABS.CABS.CDR.JURIS(0),DISP=SHR
//JURSTEP.PIUOUT DD DSN=TELCABS.CABS.CDR.PIU(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(300,150),RLSE),
//             DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//JURSTEP.SYSIN  DD *
RN%%RUNID  %%CYCLDT%%BILLPR00CABJ1300JURSTEP  NNFCC1050.00000
/*
//
