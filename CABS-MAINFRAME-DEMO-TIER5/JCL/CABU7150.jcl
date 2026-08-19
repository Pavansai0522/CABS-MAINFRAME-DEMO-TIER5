//CABU7150 JOB (CABS,UTIL),'RATE OVERRIDE TABLE LOAD',
//             CLASS=D,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABU7150 - RATE OVERRIDE TABLE LOAD                           *
//*                                                               *
//* STEP010  CABURT24                                             *
//* STEP020  CABUCV03                                             *
//*                                                               *
//* UTILITY STREAM.  SCHEDULED AFTER THE NIGHTLY                  *
//* ACCESS BILLING STREAM COMPLETES.                              *
//*****************************************************************
//STEP010  EXEC PGM=CABURT24,REGION=6M,
//             PARM='&CYCLE'
//STEPLIB  DD DSN=TELCABS.CABS.LOADLIB,DISP=SHR
//         DD DSN=TELCABS.COMMON.LOADLIB,DISP=SHR
//         DD DSN=SYS1.COB2LIB,DISP=SHR
//TARIN    DD DSN=TELCABS.CABS.TARIN(0),DISP=SHR
//BNDOUT   DD DSN=TELCABS.CABS.BNDOUT(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(10,10),RLSE),
//             DCB=(RECFM=FB,LRECL=90,BLKSIZE=0)
//CTLOUT   DD DSN=TELCABS.CABS.CONTROL(+1),
//             DISP=(MOD,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(5,5),RLSE),
//             DCB=(RECFM=FB,LRECL=180,BLKSIZE=0)
//RPTOUT   DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=D
//SYSIN    DD *
RN%RUNID  %CYCLDT%BILLPRCABU7150STEP010 NY
/*
//*
//STEP020  EXEC PGM=CABUCV03,REGION=12M,
//             PARM='&CYCLE'
//STEPLIB  DD DSN=TELCABS.CABS.LOADLIB,DISP=SHR
//         DD DSN=TELCABS.COMMON.LOADLIB,DISP=SHR
//         DD DSN=SYS1.COB2LIB,DISP=SHR
//EMIIN    DD DSN=TELCABS.CABS.EMIIN(0),DISP=SHR
//NEWOUT   DD DSN=TELCABS.CABS.NEWOUT(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(10,10),RLSE),
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
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
RN%RUNID  %CYCLDT%BILLPRCABU7150STEP020 YN
/*
//*
//
