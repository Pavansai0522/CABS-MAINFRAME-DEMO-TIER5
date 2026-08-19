//CABPGDGA PROC CYCLE=000000,BASE=
//*****************************************************************
//* CABPGDGA - GDG ALLOCATION / CLEANUP PROC.                    *
//* SYMBOLICS: CYCLE BASE.  ALLOCATES A SCRATCH WORK DATASET     *
//* FOR THE DAY AND DELETES YESTERDAY'S SCRATCH COPY - THE      *
//* CATALOGUED GDG GENERATIONS THEMSELVES ARE HANDLED BY THE     *
//* DEFINE IN CABGDGDF, NOT BY THIS PROC.                        *
//*****************************************************************
//STEP1    EXEC PGM=IEFBR14
//SCRATCH  DD DSN=TELCABS.CABS.WORK.SCRATCH,
//             DISP=(MOD,DELETE,DELETE),
//             SPACE=(CYL,(1,1)),UNIT=SYSDA
//STEP2    EXEC PGM=IDCAMS,REGION=4M,COND=(0,NE,STEP1)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  SET MAXCC = 0
/*
