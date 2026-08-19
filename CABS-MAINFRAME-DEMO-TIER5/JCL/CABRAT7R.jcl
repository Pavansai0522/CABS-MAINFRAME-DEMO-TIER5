//CABRAT7R JOB (CABS,RATE),'S.MARCHETTI',CLASS=B,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID,
//             REGION=4M,TIME=(10,0)
//*****************************************************************
//* CABRAT7R - STANDALONE RERUN OF CABRAT07 (MINIMUM / MAXIMUM    *
//* RATE ENFORCEMENT).  FULL RERUN AGAINST THE CURRENT RATED      *
//* GENERATION.                                                   *
//*****************************************************************
//*
//STEP010  EXEC CABPRATE,PGM=CABRAT07,CYCLE=&CYCLE,
//             BILLPER=&BILLPER,RUNID=&RUNID,TARIFF=&TARIFF,
//             MODE=&RERUN
//STEP010.RATIN   DD DSN=TELCABS.CABS.RATED(0),DISP=SHR
//STEP010.RATEMST DD DSN=TELCABS.CABS.RATE,DISP=SHR
//STEP010.RATOUT   DD DSN=TELCABS.CABS.RATED(+1),
//             DISP=(NEW,CATLG,DELETE),
//             SPACE=(CYL,(15,15),RLSE),UNIT=SYSDA,
//             DCB=(RECFM=FB,LRECL=200,BLKSIZE=6000)
//
