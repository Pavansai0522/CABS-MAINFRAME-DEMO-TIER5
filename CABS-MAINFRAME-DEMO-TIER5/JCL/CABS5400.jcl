//CABS5400 JOB (CABS,BILL),'TAPE AND MEDIA EXTRACT',
//             CLASS=B,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABS5400 - TAPE AND MEDIA EXTRACT                             *
//*                                                               *
//* THE VOLUME SERIAL IS ALLOCATED BY THE TAPE LIBRARY AND        *
//* PASSED IN AT SUBMISSION.  IT HAS NO DEFAULT.                  *
//*                                                               *
//* THE MEDOUT DD IS OVERRIDDEN ONTO A REAL TAPE UNIT.  THE PROC  *
//* CARRIES A DISK DATASET BECAUSE THE EDI STEP THAT SHARES THE   *
//* PROC WRITES TO DISK.                                          *
//*****************************************************************
//STEP010  EXEC CABPFMTM,
//             PGMNAME=CABFMT07,
//             CYCLE=%%CYCLDT,
//             BILLPER=%%BILLPR,
//             RUNID=%%RUNID,
//             JOBNM=CABS5400,
//             STEPNM=STEP010,
//             MEDIA=%%MEDIA,
//             VOLSER=%%VOLSER,
//             LABTXT=%%LABTXT,
//             INGDG='0',
//             OUTGDG='+1'
//MEDSTEP.MSGIN    DD DUMMY
//MEDSTEP.EDIOUT   DD DUMMY
//MEDSTEP.MEDOUT   DD DSN=TELCABS.CABS.MEDIA.EXTRACT(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=TAPE,VOL=SER=%%VOLSER,LABEL=(1,SL),
//             DCB=(RECFM=FB,LRECL=400,BLKSIZE=32000)
//MEDSTEP.SYSIN    DD *
RN%%RUNID  %%CYCLDT%%BILLPR00CABS5400STEP010NN%%MEDIA %%VOLSER%%LABTXT
/*
//
