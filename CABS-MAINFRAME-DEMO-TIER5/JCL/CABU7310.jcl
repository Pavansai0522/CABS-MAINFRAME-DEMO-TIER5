//CABU7310 JOB (CABS,UTIL),'ACCOUNT TO INVOICE CROSS REF',
//             CLASS=B,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABU7310 - ACCOUNT TO INVOICE CROSS REFERENCE                 *
//*                                                               *
//* STEP010  CABUXR21                                             *
//*                                                               *
//* UTILITY STREAM.  SCHEDULED AFTER THE NIGHTLY                  *
//* ACCESS BILLING STREAM COMPLETES.                              *
//*****************************************************************
//STEP010  EXEC PGM=CABUXR21,REGION=6M,
//             PARM='&CYCLE'
//STEPLIB  DD DSN=TELCABS.CABS.LOADLIB,DISP=SHR
//         DD DSN=TELCABS.COMMON.LOADLIB,DISP=SHR
//         DD DSN=SYS1.COB2LIB,DISP=SHR
//MSTIN    DD DSN=TELCABS.CABS.MSTIN(0),DISP=SHR
//GRPOUT   DD DSN=TELCABS.CABS.GRPOUT(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(5,10),RLSE),
//             DCB=(RECFM=FB,LRECL=120,BLKSIZE=0)
//PAIROUT  DD DSN=TELCABS.CABS.PAIROU(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(20,10),RLSE),
//             DCB=(RECFM=FB,LRECL=120,BLKSIZE=0)
//CTLOUT   DD DSN=TELCABS.CABS.CONTROL(+1),
//             DISP=(MOD,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(5,5),RLSE),
//             DCB=(RECFM=FB,LRECL=180,BLKSIZE=0)
//RPTOUT   DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=D
//SYSIN    DD *
RN%RUNID  %CYCLDT%BILLPRCABU7310STEP010 NY
/*
//*
//
