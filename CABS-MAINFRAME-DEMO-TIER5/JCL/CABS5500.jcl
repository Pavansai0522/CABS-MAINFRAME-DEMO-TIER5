//CABS5500 JOB (CABS,BILL),'PRINT CONTROL AND MESSAGE INSERT',
//             CLASS=B,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABS5500 - PRINT CONTROL FILE AND BILL MESSAGE INSERT         *
//*                                                               *
//* TWO STEPS.  STEP010 DESCRIBES THE FINISHED PRINT STREAM FOR   *
//* THE BURST AND INSERT MACHINERY.  STEP020 PRODUCES THE MESSAGE *
//* INSERT PAGE WHEN A MARKETING CAMPAIGN IS RUNNING.             *
//*                                                               *
//* THE INSERT SWITCH AND THE CAMPAIGN CODE COME FROM THE         *
//* CAMPAIGN CALENDAR.  WHEN NO CAMPAIGN IS LIVE THE SCHEDULER    *
//* SUBSTITUTES N AND A BLANK CAMPAIGN AND STEP020 PRODUCES AN    *
//* EMPTY FILE.                                                   *
//*****************************************************************
//STEP010  EXEC CABPFMTM,
//             PGMNAME=CABFMT08,
//             CYCLE=%%CYCLDT,
//             BILLPER=%%BILLPR,
//             RUNID=%%RUNID,
//             JOBNM=CABS5500,
//             STEPNM=STEP010,
//             FORMID=%%FORMID,
//             TRAY=%%TRAY,
//             INGDG='0',
//             OUTGDG='+1'
//MEDSTEP.BDTLIN   DD DUMMY
//MEDSTEP.BHDRIN   DD DUMMY
//MEDSTEP.MSGIN    DD DUMMY
//MEDSTEP.EDIOUT   DD DUMMY
//MEDSTEP.PRTIN    DD DSN=TELCABS.CABS.PRINT.TOT(0),DISP=SHR
//MEDSTEP.DOCCTL   DD DSN=TELCABS.CABS.PRTCTL.DOC(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(30,15),RLSE),
//             DCB=(RECFM=FB,LRECL=90,BLKSIZE=0)
//MEDSTEP.SYSIN    DD *
RN%%RUNID  %%CYCLDT%%BILLPR00CABS5500STEP010NN%%BURSTSY%%FORMID%%TRAY
/*
//*
//STEP020  EXEC CABPFMTM,
//             PGMNAME=CABFMT09,
//             CYCLE=%%CYCLDT,
//             BILLPER=%%BILLPR,
//             RUNID=%%RUNID,
//             JOBNM=CABS5500,
//             STEPNM=STEP020,
//             CAMP=%%CAMPGN,
//             INSSW=%%INSSW,
//             INGDG='0',
//             OUTGDG='+1'
//MEDSTEP.BDTLIN   DD DUMMY
//MEDSTEP.EDIOUT   DD DUMMY
//MEDSTEP.PRTOUT   DD DSN=TELCABS.CABS.PRINT.MSG(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(30,15),RLSE),
//             DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//MEDSTEP.SYSIN    DD *
RN%%RUNID  %%CYCLDT%%BILLPR00CABS5500STEP020NN%%INSSW %%CAMPGN%%MSGFRM%%MSGTHR
/*
//
