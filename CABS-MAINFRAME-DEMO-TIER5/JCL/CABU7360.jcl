//CABU7360 JOB (CABS,UTIL),'LEGACY LAYOUT DOWN CONVERSIO',
//             CLASS=C,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABU7360 - LEGACY LAYOUT DOWN CONVERSION                      *
//*                                                               *
//* STEP010  CABUCV08                                             *
//*                                                               *
//* UTILITY STREAM.  SCHEDULED AFTER THE NIGHTLY                  *
//* ACCESS BILLING STREAM COMPLETES.                              *
//*****************************************************************
//STEP010  EXEC PGM=CABUCV08,REGION=8M,
//             PARM='&CYCLE'
//STEPLIB  DD DSN=TELCABS.CABS.LOADLIB,DISP=SHR
//         DD DSN=TELCABS.COMMON.LOADLIB,DISP=SHR
//         DD DSN=SYS1.COB2LIB,DISP=SHR
//IXCIN    DD DSN=TELCABS.CABS.IXCIN(0),DISP=SHR
//SRCIN    DD DSN=TELCABS.CABS.SRCIN(0),DISP=SHR
//REJOUT   DD DSN=TELCABS.CABS.REJOUT(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(20,10),RLSE),
//             DCB=(RECFM=FB,LRECL=130,BLKSIZE=0)
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
RN%RUNID  %CYCLDT%BILLPRCABU7360STEP010 YY
/*
//*
//
