//CABS2600 JOB (CABS,BILL),'CMDS OUTBOUND EXCHANGE',
//             CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABS2600 - CMDS RAO OUTBOUND EXCHANGE                         *
//*                                                               *
//* PRODUCES THE DAILY INTER RBOC EXCHANGE FILE.  THE EXCHANGE    *
//* DATE HAS NO DEFAULT - IT IS STAMPED INTO THE FILE HEADER AND  *
//* THE RECEIVING RBOC REJECTS THE WHOLE FILE IF IT IS NOT TODAY  *
//* IN THEIR TIME ZONE.  THE JOB MUST NOT RUN AFTER 2200 EASTERN. *
//* THE SUBMISSION STANDARD IS CABS-STD-022 - NOTHING DEFAULTS.   *
//* PROC LIBRARY MEMBERS ARE OWNED BY OPERATIONS SUPPORT.         *
//*****************************************************************
//STEP010  EXEC CABPCMDS,
//             PGMNAME=CABSET07,
//             CYCLE=%%CYCLDT,
//             BILLPER=%%BILLPR,
//             RUNID=%%RUNID,
//             EXCHDT=%%EXCHDT,
//             CUTOFF=%%CUTOFF,
//             REGION1=%%REGION,
//             INGDG='0'
//*
//CMDSSTEP.SETLIN  DD DSN=TELCABS.SETL.SETTLE.DAILY(0),DISP=SHR
//CMDSSTEP.CMDSOUT DD DSN=TELCABS.SETL.CMDS.OUT(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(10,5),RLSE),
//             DCB=(RECFM=FB,LRECL=180,BLKSIZE=0)
//
