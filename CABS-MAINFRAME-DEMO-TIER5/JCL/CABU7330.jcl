//CABU7330 JOB (CABS,UTIL),'ACCOUNT TO INVOICE CROSS REF',
//             CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABU7330 - ACCOUNT TO INVOICE CROSS REFERENCE                 *
//*                                                               *
//* STEP010  CABUXR16                                             *
//*                                                               *
//* UTILITY STREAM.  SCHEDULED AFTER THE NIGHTLY                  *
//* ACCESS BILLING STREAM COMPLETES.                              *
//*****************************************************************
//STEP010  EXEC PGM=CABUXR16,REGION=8M,
//             PARM='&CYCLE'
//STEPLIB  DD DSN=TELCABS.CABS.LOADLIB,DISP=SHR
//         DD DSN=TELCABS.COMMON.LOADLIB,DISP=SHR
//         DD DSN=SYS1.COB2LIB,DISP=SHR
//INVIN    DD DSN=TELCABS.CABS.INVIN(0),DISP=SHR
//XRFOUT   DD DSN=TELCABS.CABS.XRFOUT(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(5,5),RLSE),
//             DCB=(RECFM=FB,LRECL=90,BLKSIZE=0)
//CTLOUT   DD DSN=TELCABS.CABS.CONTROL(+1),
//             DISP=(MOD,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(5,5),RLSE),
//             DCB=(RECFM=FB,LRECL=180,BLKSIZE=0)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=D
//SYSIN    DD *
RN%RUNID  %CYCLDT%BILLPRCABU7330STEP010 YY
/*
//*
//
