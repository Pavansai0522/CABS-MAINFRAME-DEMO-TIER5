//CABS2100 JOB (CABS,BILL),'MEET POINT CIRCUIT EXTRACT',
//             CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABS2100 - MEET POINT CIRCUIT EXTRACT                         *
//*                                                               *
//* THE FIRST STEP OF THE MONTHLY MEET POINT SETTLEMENT.  READS   *
//* THE SETTLEMENT CIRCUIT INVENTORY AND THE BILLING USAGE FILE.  *
//* THE USAGE FILE BELONGS TO THE CABS APPLICATION.               *
//* THE INTERFACE AGREEMENT IS HELD BY THE APPLICATION OWNER.     *
//* PROC LIBRARY MEMBERS ARE OWNED BY OPERATIONS SUPPORT.         *
//*****************************************************************
//STEP010  EXEC CABPMPBX,
//             PGMNAME=CABSET02,
//             CYCLE=%%CYCLDT,
//             BILLPER=%%BILLPR,
//             RUNID=%%RUNID,
//             SETPER=%%SETPER,
//             INDD=CIRCMAST,
//             OUTDD=MPB.EXTRACT,
//             INGDG='0'
//*
//MPBSTEP.CDRIN DD DSN=TELCABS.CABS.CDR.PLU(0),DISP=SHR
//
