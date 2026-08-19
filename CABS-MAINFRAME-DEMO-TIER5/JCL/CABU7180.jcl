//CABU7180 JOB (CABS,UTIL),'CIRCUIT TO ACCOUNT CROSS REF',
//             CLASS=D,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABU7180 - CIRCUIT TO ACCOUNT CROSS REFERENCE                 *
//*                                                               *
//* STEP010  CABUXR01                                             *
//* STEP020  CABUEX08                                             *
//*                                                               *
//* UTILITY STREAM.  SCHEDULED AFTER THE NIGHTLY                  *
//* ACCESS BILLING STREAM COMPLETES.                              *
//*****************************************************************
//STEP010  EXEC PGM=CABUXR01,REGION=8M,
//             PARM='&CYCLE'
//STEPLIB  DD DSN=TELCABS.CABS.LOADLIB,DISP=SHR
//         DD DSN=TELCABS.COMMON.LOADLIB,DISP=SHR
//         DD DSN=SYS1.COB2LIB,DISP=SHR
//RGTIN    DD DSN=TELCABS.CABS.RGTIN(0),DISP=SHR
//LNKOUT   DD DSN=TELCABS.CABS.LNKOUT(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(20,10),RLSE),
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
//CTLOUT   DD DSN=TELCABS.CABS.CONTROL(+1),
//             DISP=(MOD,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(5,5),RLSE),
//             DCB=(RECFM=FB,LRECL=180,BLKSIZE=0)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=D
//SYSIN    DD *
RN%RUNID  %CYCLDT%BILLPRCABU7180STEP010 YY
/*
//*
//STEP020  EXEC PGM=CABUEX08,REGION=6M,
//             PARM='&CYCLE'
//STEPLIB  DD DSN=TELCABS.CABS.LOADLIB,DISP=SHR
//         DD DSN=TELCABS.COMMON.LOADLIB,DISP=SHR
//         DD DSN=SYS1.COB2LIB,DISP=SHR
//EXTIN    DD DSN=TELCABS.CABS.EXTIN(0),DISP=SHR
//CIROUT   DD DSN=TELCABS.CABS.CIROUT(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(20,10),RLSE),
//             DCB=(RECFM=FB,LRECL=110,BLKSIZE=0)
//SUSOUT   DD DSN=TELCABS.CABS.UTIL.SUSP(+1),
//             DISP=(MOD,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(15,15),RLSE),
//             DCB=(RECFM=FB,LRECL=300,BLKSIZE=0)
//CTLOUT   DD DSN=TELCABS.CABS.CONTROL(+1),
//             DISP=(MOD,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(5,5),RLSE),
//             DCB=(RECFM=FB,LRECL=180,BLKSIZE=0)
//RPTOUT   DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=D
//SYSIN    DD *
RN%RUNID  %CYCLDT%BILLPRCABU7180STEP020 YY
/*
//*
//
