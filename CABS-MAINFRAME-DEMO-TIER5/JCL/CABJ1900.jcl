//CABJ1900 JOB (CABS,BILL),'RESTATEMENT POSTING TO DB2',
//             CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABJ1900 - RESTATEMENT ADJUSTMENT POSTING                     *
//*                                                               *
//* CABJUR10 UPDATES THE CABSADJ DB2 TABLE AND THE VSAM BALANCE   *
//* FILE IN THE SAME LOGICAL UNIT OF WORK WITH NO COORDINATION    *
//* BETWEEN THEM.  A STEP FAILURE BETWEEN THE INSERT AND THE      *
//* REWRITE LEAVES THE TWO STORES OUT OF STEP AND THERE IS NO     *
//* AUTOMATED WAY TO TELL WHICH ONE IS RIGHT.                     *
//* DUAL POSTING AGREED WITH THE SETTLEMENT GROUP, CR-5102.       *
//* CONCATENATION ORDER SET BY THE 1996 LIBRARY REORGANISATION.   *
//*****************************************************************
//STEP010  EXEC CABPDB2P,
//             PGMNAME=CABJUR10,
//             CYCLE=%%CYCLDT,
//             BILLPER=%%BILLPR,
//             RUNID=%%RUNID,
//             SUBSYS=%%DB2SUB,
//             PLAN=%%DB2PLN,
//             COMMIT=%%COMFRQ,
//             INGDG='0'
//*
//DB2STEP.ADJIN   DD DSN=TELCABS.CABS.RESTATE.ADJ.SORT(0),DISP=SHR
//DB2STEP.BALMAST DD DSN=TELCABS.CABS.BALANCE,DISP=SHR
//
