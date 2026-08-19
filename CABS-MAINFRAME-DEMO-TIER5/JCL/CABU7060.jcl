//CABU7060 JOB (CABS,UTIL),'CONTROL RECORD CROSS REFEREN',
//             CLASS=B,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABU7060 - CONTROL RECORD CROSS REFERENCE                     *
//*                                                               *
//* STEP010  CABUXR06                                             *
//* STEP020  CABUEX07                                             *
//*                                                               *
//* UTILITY STREAM.  SCHEDULED AFTER THE NIGHTLY                  *
//* ACCESS BILLING STREAM COMPLETES.                              *
//*****************************************************************
//STEP010  EXEC PGM=CABUXR06,REGION=4M,
//             PARM='&CYCLE'
//STEPLIB  DD DSN=TELCABS.CABS.LOADLIB,DISP=SHR
//         DD DSN=TELCABS.COMMON.LOADLIB,DISP=SHR
//         DD DSN=SYS1.COB2LIB,DISP=SHR
//XRFIN    DD DSN=TELCABS.CABS.XRFIN(0),DISP=SHR
//ORPOUT   DD DSN=TELCABS.CABS.ORPOUT(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(10,10),RLSE),
//             DCB=(RECFM=FB,LRECL=100,BLKSIZE=0)
//CTLOUT   DD DSN=TELCABS.CABS.CONTROL(+1),
//             DISP=(MOD,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(5,5),RLSE),
//             DCB=(RECFM=FB,LRECL=180,BLKSIZE=0)
//RPTOUT   DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=D
//SYSIN    DD *
RN%RUNID  %CYCLDT%BILLPRCABU7060STEP010 NY
/*
//*
//STEP020  EXEC PGM=CABUEX07,REGION=4M,
//             PARM='&CYCLE'
//STEPLIB  DD DSN=TELCABS.CABS.LOADLIB,DISP=SHR
//         DD DSN=TELCABS.COMMON.LOADLIB,DISP=SHR
//         DD DSN=SYS1.COB2LIB,DISP=SHR
//SELIN    DD DSN=TELCABS.CABS.SELIN(0),DISP=SHR
//CIROUT   DD DSN=TELCABS.CABS.CIROUT(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(10,5),RLSE),
//             DCB=(RECFM=FB,LRECL=90,BLKSIZE=0)
//SUSOUT   DD DSN=TELCABS.CABS.UTIL.SUSP(+1),
//             DISP=(MOD,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(15,15),RLSE),
//             DCB=(RECFM=FB,LRECL=300,BLKSIZE=0)
//CTLOUT   DD DSN=TELCABS.CABS.CONTROL(+1),
//             DISP=(MOD,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(5,5),RLSE),
//             DCB=(RECFM=FB,LRECL=180,BLKSIZE=0)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=D
//SYSIN    DD *
RN%RUNID  %CYCLDT%BILLPRCABU7060STEP020 YN
/*
//*
//
