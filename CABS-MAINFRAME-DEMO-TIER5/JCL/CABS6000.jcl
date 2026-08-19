//CABS6000 JOB (CABS,BILL),'DAILY BALANCING REPORT',
//             CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABS6000 - DAILY BALANCING REPORT                             *
//*                                                               *
//* READS THE CONTROL RECORD WRITTEN BY EVERY PROCESS IN THE      *
//* CYCLE AND PROVES THE RUN END TO END.  THE VERDICT LINE ON THE *
//* LAST PAGE IS THE ONE OPERATIONS CHECK BEFORE THE PRINT JOBS   *
//* ARE RELEASED.  A RETURN CODE OF 12 STOPS THEM.                *
//*                                                               *
//* EXPPRC IS THE NUMBER OF PROCESSES THE CYCLE DEFINITION SAYS   *
//* SHOULD HAVE REPORTED.  IT DIFFERS BY CYCLE TYPE AND HAS NO    *
//* DEFAULT.                                                      *
//*                                                               *
//* STEP010 CONSOLIDATES THE CONTROL FILE.  THE RULE THAT DROPS   *
//* SUPERSEDED RERUN RECORDS IS IN CABSRT15 AND NOWHERE ELSE.     *
//* THE SORT FIELDS ARE FILED WITH THE DATASET REGISTER.          *
//*****************************************************************
//STEP010  EXEC PGM=SORT,REGION=4M
//SORTIN   DD DSN=TELCABS.CABS.CONTROL(0),DISP=SHR
//SORTOUT  DD DSN=TELCABS.CABS.CONTROL.CONS(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(15,15),RLSE),
//             DCB=(RECFM=FB,LRECL=180,BLKSIZE=0)
//SORTWK01 DD UNIT=SYSDA,SPACE=(CYL,(5,5))
//SYSOUT   DD SYSOUT=*
//SYSIN    DD DSN=TELCABS.CABS.CTLCARDS(CABSRT15),DISP=SHR
//*
//STEP020  EXEC CABPRPTB,
//             PGMNAME=CABRPT01,
//             CYCLE=%%CYCLDT,
//             BILLPER=%%BILLPR,
//             RUNID=%%RUNID,
//             JOBNM=CABS6000,
//             STEPNM=STEP020,
//             HALT=%%HALTSW,
//             CHAIN=%%CHNSW,
//             EXPPRC=%%EXPPRC,
//             INGDG='0',
//             OUTGDG='+1'
//RPTSTEP.BDTLIN   DD DUMMY
//RPTSTEP.CARRMST  DD DUMMY
//RPTSTEP.CTLIN    DD DSN=TELCABS.CABS.CONTROL.CONS(0),DISP=SHR
//RPTSTEP.PROOFIN  DD DSN=TELCABS.CABS.BILLPROOF(0),DISP=SHR
//RPTSTEP.SYSIN    DD *
RN%%RUNID  %%CYCLDT%%BILLPR00CABS6000STEP020YY%%HALTSW%%CHNSW Y N%%EXPPRC
/*
//
