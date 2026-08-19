//CABS4100 JOB (CABS,BILL),'BILL DETAIL LINE ASSEMBLY',
//             CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABS4100 - BILL DETAIL LINE ASSEMBLY                          *
//*                                                               *
//* THE LONGEST STEP IN THE STREAM.  BUILDS THE VARIABLE LENGTH   *
//* BILL DETAIL RECORDS FROM THE RATED AND JURISDICTIONALISED     *
//* USAGE.  RUNS IN CLASS A OVERNIGHT.                            *
//*                                                               *
//* STEP010 PRESORTS THE RATED USAGE.  THE SUMMARISATION RULE     *
//* THAT COLLAPSES DUPLICATE ELEMENT SEQUENCES IS IN CABSRT10     *
//* AND IS NOT KNOWN TO ANY PROGRAM.                              *
//*                                                               *
//* MAXELM IS SUBSTITUTED BY THE SCHEDULER FROM THE CURRENT       *
//* COPYBOOK LIMIT.                                               *
//*****************************************************************
//STEP010  EXEC PGM=SORT,REGION=6M
//SORTIN   DD DSN=TELCABS.CABS.RATED.JUR(0),DISP=SHR
//SORTOUT  DD DSN=TELCABS.CABS.RATED.JUR.SRT(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(120,60),RLSE),
//             DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//SORTWK01 DD UNIT=SYSDA,SPACE=(CYL,(80,40))
//SORTWK02 DD UNIT=SYSDA,SPACE=(CYL,(80,40))
//SORTWK03 DD UNIT=SYSDA,SPACE=(CYL,(80,40))
//SYSOUT   DD SYSOUT=*
//SYSIN    DD DSN=TELCABS.CABS.CTLCARDS(CABSRT10),DISP=SHR
//*
//STEP020  EXEC CABPBDTL,
//             PGMNAME=CABBIL02,
//             CYCLE=%%CYCLDT,
//             BILLPER=%%BILLPR,
//             RUNID=%%RUNID,
//             JOBNM=CABS4100,
//             STEPNM=STEP020,
//             MAXELM=%%MAXELM,
//             SUPZER=%%SUPZER,
//             CONTSW=Y,
//             INGDG='0',
//             OUTGDG='+1'
//DTLSTEP.RATIN    DD DSN=TELCABS.CABS.RATED.JUR.SRT(0),DISP=SHR
//DTLSTEP.SYSIN    DD *
RN%%RUNID  %%CYCLDT%%BILLPR00CABS4100STEP020NY%%MAXELM%%SUPZERYU1Z1
/*
//
