//CABJ1400 JOB (CABS,BILL),'PLU APPLICATION',
//             CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABJ1400 - PERCENT LOCAL USAGE APPLICATION                    *
//*                                                               *
//* THIRD USE OF THE SHARED JURISDICTION FRAGMENT.  THE LOCAL     *
//* AND TOLL RATES ARRIVE ON THE CONTROL CARD AS SYMBOLICS -      *
//* THEY ARE NOT ON ANY FILE AND THE JCL DOES NOT SHOW WHAT THEY  *
//* WILL BE.                                                      *
//*****************************************************************
//STEP010  EXEC CABPJURS,
//             PGMNAME=CABJUR05,
//             CYCLE=%%CYCLDT,
//             BILLPER=%%BILLPR,
//             RUNID=%%RUNID,
//             STEPSEQ=050,
//             OUTDD=CDR.PLU,
//             INGDG='0'
//*
//JURSTEP.PIUIN  DD DSN=TELCABS.CABS.CDR.PIU(0),DISP=SHR
//JURSTEP.PLUOUT DD DSN=TELCABS.CABS.CDR.PLU(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(300,150),RLSE),
//             DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//JURSTEP.SYSIN  DD *
RN%%RUNID  %%CYCLDT%%BILLPR00CABJ1400JURSTEP  NN%%LOCRAT%%TOLRAT
/*
//
