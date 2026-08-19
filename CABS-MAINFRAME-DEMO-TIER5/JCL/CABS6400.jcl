//CABS6400 JOB (CABS,BILL),'MONTH END CLOSE REPORT',
//             CLASS=B,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABS6400 - MONTH END CLOSE REPORT AND LEDGER POSTING          *
//*                                                               *
//* THE CLOSE PERIOD, THE LEDGER COMPANY AND THE SIGN OFF         *
//* INITIALS ARE ALL PROMPTED FOR AT SUBMISSION.  THE STEP WILL   *
//* NOT RUN WITHOUT A NAMED PERSON AGAINST THE CLOSE.             *
//* VALUES ARE SUPPLIED BY THE SCHEDULER PER CABS-STD-022.        *
//*                                                               *
//* THE CONTROL FILE IS CONCATENATED ACROSS THE WHOLE MONTH -     *
//* FIVE WEEKLY GENERATIONS - BECAUSE THE CLOSE HAS TO PROVE THAT *
//* EVERY CYCLE IN THE PERIOD BALANCED, NOT JUST THE LAST ONE.    *
//* GDG RELATIVE NUMBERING PER CABS-STD-026.                      *
//*****************************************************************
//STEP010  EXEC CABPCLOS,
//             PGMNAME=CABRPT08,
//             CYCLE=%%CYCLDT,
//             BILLPER=%%BILLPR,
//             RUNID=%%RUNID,
//             JOBNM=CABS6400,
//             STEPNM=STEP010,
//             CLSPER=%%CLSPER,
//             CLSDT=%%CLSDT,
//             LEDGCO=%%LEDGCO,
//             SIGNOF=%%SIGNOF,
//             INGDG='0',
//             OUTGDG='+1'
//CLSSTEP.CTLIN    DD DSN=TELCABS.CABS.CONTROL(0),DISP=SHR
//         DD DSN=TELCABS.CABS.CONTROL(-1),DISP=SHR
//         DD DSN=TELCABS.CABS.CONTROL(-2),DISP=SHR
//         DD DSN=TELCABS.CABS.CONTROL(-3),DISP=SHR
//         DD DSN=TELCABS.CABS.CONTROL(-4),DISP=SHR
//CLSSTEP.CLOSEMS  DD DSN=TELCABS.CABS.CLOSEMST,DISP=OLD
//CLSSTEP.SYSIN    DD *
RN%%RUNID  %%CYCLDT%%BILLPR00CABS6400STEP010NN%%CLSPER%%CLSDT %%LEDGCO%%SIGNOF
/*
//
