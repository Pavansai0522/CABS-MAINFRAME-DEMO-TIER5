//CABS5300 JOB (CABS,BILL),'EDI 811 INTERCHANGE',
//             CLASS=B,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABS5300 - EDI 811 INTERCHANGE PRODUCTION                     *
//*                                                               *
//* THE EDI VERSION AND SENDER ID DIFFER BY TRADING PARTNER AND   *
//* CHANGE WHENEVER A PARTNER MIGRATES.  BOTH ARE SUBSTITUTED BY  *
//* THE SCHEDULER AND NEITHER HAS A DEFAULT.                      *
//*                                                               *
//* STEP020 TRANSMITS THE INTERCHANGE TO THE VALUE ADDED NETWORK  *
//* GATEWAY.  THE FILTER THAT DECIDES WHICH TRADING PARTNERS ARE  *
//* IN TONIGHT'S INTERCHANGE IS IN CABSRT13 AND IS NOT KNOWN TO   *
//* CABFMT06.                                                     *
//*****************************************************************
//STEP010  EXEC CABPFMTM,
//             PGMNAME=CABFMT06,
//             CYCLE=%%CYCLDT,
//             BILLPER=%%BILLPR,
//             RUNID=%%RUNID,
//             JOBNM=CABS5300,
//             STEPNM=STEP010,
//             EDIVER=%%EDIVER,
//             SENDID=%%SENDID,
//             INGDG='0',
//             OUTGDG='+1'
//MEDSTEP.MSGIN    DD DUMMY
//MEDSTEP.SYSIN    DD *
RN%%RUNID  %%CYCLDT%%BILLPR00CABS5300STEP010NNN%%EDIVER%%SENDID~*
/*
//*
//STEP020  EXEC PGM=SORT,REGION=4M,COND=(4,LT)
//SORTIN   DD DSN=TELCABS.CABS.EDI811(0),DISP=SHR
//SORTOUT  DD DSN=TELCABS.CABS.EDI811.SEND(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(30,15),RLSE),
//             DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//SORTWK01 DD UNIT=SYSDA,SPACE=(CYL,(20,10))
//SYSOUT   DD SYSOUT=*
//SYSIN    DD DSN=TELCABS.CABS.CTLCARDS(CABSRT13),DISP=SHR
//
