//CABU7080 JOB (CABS,UTIL),'RATE ELEMENT DESCRIPTION MAI',
//             CLASS=D,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABU7080 - RATE ELEMENT DESCRIPTION MAINTENANCE               *
//*                                                               *
//* STEP010  CABURT01                                             *
//* STEP020  CABUCV12                                             *
//*                                                               *
//* UTILITY STREAM.  SCHEDULED AFTER THE NIGHTLY                  *
//* ACCESS BILLING STREAM COMPLETES.                              *
//*****************************************************************
//STEP010  EXEC PGM=CABURT01,REGION=12M,
//             PARM='&CYCLE'
//STEPLIB  DD DSN=TELCABS.CABS.LOADLIB,DISP=SHR
//         DD DSN=TELCABS.COMMON.LOADLIB,DISP=SHR
//         DD DSN=SYS1.COB2LIB,DISP=SHR
//TARIN    DD DSN=TELCABS.CABS.TARIN(0),DISP=SHR
//TBLOUT   DD DSN=TELCABS.CABS.TBLOUT(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(5,5),RLSE),
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
//RATOUT   DD DSN=TELCABS.CABS.RATOUT(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(10,5),RLSE),
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
//CTLOUT   DD DSN=TELCABS.CABS.CONTROL(+1),
//             DISP=(MOD,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(5,5),RLSE),
//             DCB=(RECFM=FB,LRECL=180,BLKSIZE=0)
//RPTOUT   DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=D
//SYSIN    DD *
RN%RUNID  %CYCLDT%BILLPRCABU7080STEP010 NY
/*
//*
//STEP020  EXEC PGM=CABUCV12,REGION=6M,
//             PARM='&CYCLE'
//STEPLIB  DD DSN=TELCABS.CABS.LOADLIB,DISP=SHR
//         DD DSN=TELCABS.COMMON.LOADLIB,DISP=SHR
//         DD DSN=SYS1.COB2LIB,DISP=SHR
//OLDIN    DD DSN=TELCABS.CABS.OLDIN(0),DISP=SHR
//CNVOUT   DD DSN=TELCABS.CABS.CNVOUT(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(20,5),RLSE),
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
RN%RUNID  %CYCLDT%BILLPRCABU7080STEP020 NN
/*
//*
//
