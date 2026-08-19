//CABS4250 JOB (CABS,BILL),'ADJUSTMENT APPLICATION',
//             CLASS=B,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABS4250 - ADJUSTMENT AND RESTATEMENT APPLICATION             *
//*                                                               *
//* USES CABPBADJ WITH THE ADJUSTMENT HALF OF THE SYMBOLIC LIST.  *
//* THE SETTLEMENT DD IS OVERRIDDEN TO DUMMY BECAUSE THIS STEP    *
//* DOES NOT READ IT.                                             *
//*                                                               *
//* ADJIN IS READ AT GENERATION (0) AND THE RESTATEMENT FEED FROM *
//* THE JURISDICTION FAMILY IS CONCATENATED BEHIND IT AT (-1)     *
//* BECAUSE THE QUARTERLY RESTATEMENT RUNS A CYCLE AHEAD.         *
//* GDG RELATIVE NUMBERING PER CABS-STD-026.                      *
//*****************************************************************
//STEP010  EXEC CABPBADJ,
//             PGMNAME=CABBIL05,
//             CYCLE=%%CYCLDT,
//             BILLPER=%%BILLPR,
//             RUNID=%%RUNID,
//             JOBNM=CABS4250,
//             STEPNM=STEP010,
//             RSTSW=%%RSTSW,
//             DSPSW=%%DSPSW,
//             MAXADJ=%%MAXADJ,
//             INGDG='0',
//             OUTGDG='+1'
//ADJSTEP.SETLIN   DD DUMMY
//ADJSTEP.ADJIN    DD DSN=TELCABS.CABS.ADJUST(0),DISP=SHR
//         DD DSN=TELCABS.CABS.ADJUST.RESTATE(-1),DISP=SHR
//ADJSTEP.SYSIN    DD *
RN%%RUNID  %%CYCLDT%%BILLPR00CABS4250STEP010YY%%RSTSW %%MAXADJY%%DSPSW
/*
//
