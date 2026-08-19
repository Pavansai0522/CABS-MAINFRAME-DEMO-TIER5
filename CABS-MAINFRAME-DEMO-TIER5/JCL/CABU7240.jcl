//CABU7240 JOB (CABS,UTIL),'PACKED TO DISPLAY CONVERSION',
//             CLASS=C,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABU7240 - PACKED TO DISPLAY CONVERSION                       *
//*                                                               *
//* STEP010  CABUCV04                                             *
//*                                                               *
//* UTILITY STREAM.  SCHEDULED AFTER THE NIGHTLY                  *
//* ACCESS BILLING STREAM COMPLETES.                              *
//*****************************************************************
//STEP010  EXEC PGM=CABUCV04,REGION=12M,
//             PARM='&CYCLE'
//STEPLIB  DD DSN=TELCABS.CABS.LOADLIB,DISP=SHR
//         DD DSN=TELCABS.COMMON.LOADLIB,DISP=SHR
//         DD DSN=SYS1.COB2LIB,DISP=SHR
//IXCIN    DD DSN=TELCABS.CABS.IXCIN(0),DISP=SHR
//EMIIN    DD DSN=TELCABS.CABS.EMIIN(0),DISP=SHR
//PCKIN    DD DSN=TELCABS.CABS.PCKIN(0),DISP=SHR
//TGTOUT   DD DSN=TELCABS.CABS.TGTOUT(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(10,10),RLSE),
//             DCB=(RECFM=FB,LRECL=120,BLKSIZE=0)
//UPLOUT   DD DSN=TELCABS.CABS.UPLOUT(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(10,5),RLSE),
//             DCB=(RECFM=FB,LRECL=120,BLKSIZE=0)
//CNVOUT   DD DSN=TELCABS.CABS.CNVOUT(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(10,10),RLSE),
//             DCB=(RECFM=FB,LRECL=120,BLKSIZE=0)
//CTLOUT   DD DSN=TELCABS.CABS.CONTROL(+1),
//             DISP=(MOD,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(5,5),RLSE),
//             DCB=(RECFM=FB,LRECL=180,BLKSIZE=0)
//RPTOUT   DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=D
//SYSIN    DD *
RN%RUNID  %CYCLDT%BILLPRCABU7240STEP010 YY
/*
//*
//
