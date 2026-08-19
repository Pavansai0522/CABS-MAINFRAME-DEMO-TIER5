//CABS6100 JOB (CABS,BILL),'REVENUE AND RATE ELEMENT STUDY',
//             CLASS=C,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABS6100 - REVENUE REPORT AND RATE ELEMENT STUDY              *
//*                                                               *
//* TWO STEPS THROUGH THE SAME REPORTING PROC WITH DIFFERENT      *
//* PROGRAMS AND DIFFERENT DD OVERRIDES.                          *
//*                                                               *
//* THE STUDY LEVEL IS TYPED BY THE ANALYST AND PASSED IN.  A     *
//* ZERO IS FATAL - IT IS NOT TREATED AS A DEFAULT.               *
//* PARAMETER HANDLING PER CABS-STD-022.                          *
//*****************************************************************
//STEP010  EXEC CABPRPTB,
//             PGMNAME=CABRPT02,
//             CYCLE=%%CYCLDT,
//             BILLPER=%%BILLPR,
//             RUNID=%%RUNID,
//             JOBNM=CABS6100,
//             STEPNM=STEP010,
//             LEVEL=%%RLEVEL,
//             MINREV=%%MINREV,
//             INGDG='0',
//             OUTGDG='+1'
//RPTSTEP.CTLIN    DD DUMMY
//RPTSTEP.PROOFIN  DD DUMMY
//RPTSTEP.REVOUT   DD SYSOUT=*
//RPTSTEP.SYSIN    DD *
RN%%RUNID  %%CYCLDT%%BILLPR00CABS6100STEP010YY%%RLEVEL%%MINREV%%STATSL%%JURSSL
/*
//*
//STEP020  EXEC CABPRPTB,
//             PGMNAME=CABRPT06,
//             CYCLE=%%CYCLDT,
//             BILLPER=%%BILLPR,
//             RUNID=%%RUNID,
//             JOBNM=CABS6100,
//             STEPNM=STEP020,
//             LEVEL=%%SLEVEL,
//             INGDG='0',
//             OUTGDG='+1'
//RPTSTEP.CTLIN    DD DUMMY
//RPTSTEP.PROOFIN  DD DUMMY
//RPTSTEP.CARRMST  DD DUMMY
//RPTSTEP.STUDYEX  DD DSN=TELCABS.CABS.STUDY.EXTRACT(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(60,30),RLSE),
//             DCB=(RECFM=FB,LRECL=400,BLKSIZE=0)
//RPTSTEP.STUDYRP  DD SYSOUT=*
//RPTSTEP.SYSIN    DD *
RN%%RUNID  %%CYCLDT%%BILLPR00CABS6100STEP020YY%%SLEVEL%%MINCNT%%ELMSEL%%EXTRSW
/*
//
