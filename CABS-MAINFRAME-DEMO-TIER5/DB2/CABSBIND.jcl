//CABSBIND JOB (CABS,DBA),'BIND CABS/SETL PLANS',
//             CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABSBIND - BIND PACKAGES AND PLANS FOR CABSPLAN AND SETLPLAN  *
//*                                                               *
//* CABSPLAN COVERS THE CABS APPLICATION DBRMS (JURISDICTION AND *
//* ON-LINE RATE/FACTOR MAINTENANCE).  SETLPLAN COVERS THE SETL  *
//* APPLICATION DBRMS (SETTLEMENT PERIOD CLOSE AND POSTING).     *
//*                                                               *
//* BIND ORDER MATTERS.  SETLPLAN MUST BE BOUND AFTER CABSPLAN   *
//* BECAUSE THE SETL PACKAGE LIST INCLUDES CABSADJ COLLECTION    *
//* CABSCOL, WHICH IS CREATED BY THE CABSPLAN STEP BELOW.  A     *
//* FRESH SUBSYSTEM MUST RUN STEP010 BEFORE STEP020 - SEE        *
//* CABS-STD-041 SECTION 6.                                       *
//*                                                               *
//* RUN UNDER A USERID WITH BINDADD AND BIND ON PACKAGE           *
//* AUTHORITY.  DSNTIAD OR SPUFI IS NOT USED HERE - THIS JOB      *
//* CALLS THE DSN COMMAND PROCESSOR DIRECTLY THROUGH IKJEFT01.    *
//*****************************************************************
//*
//STEP010  EXEC PGM=IKJEFT01,DYNAMNBR=20
//STEPLIB  DD DSN=DSNP.SDSNLOAD,DISP=SHR
//SYSTSPRT DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//DBRMLIB  DD DSN=TELCABS.CABS.DBRMLIB,DISP=SHR
//SYSTSIN  DD *
 DSN SYSTEM(DSNP)
 BIND PACKAGE(CABSCOL)                                                -
      MEMBER(CABJUR10)                                                -
      LIBRARY('TELCABS.CABS.DBRMLIB')                                 -
      ACTION(REPLACE)                                                 -
      ISOLATION(CS)                                                   -
      VALIDATE(BIND)                                                  -
      ACQUIRE(USE)                                                    -
      RELEASE(COMMIT)                                                 -
      EXPLAIN(NO)                                                     -
      CURRENTDATA(NO)
 BIND PACKAGE(CABSCOL)                                                -
      MEMBER(CABJUR07)                                                -
      LIBRARY('TELCABS.CABS.DBRMLIB')                                 -
      ACTION(REPLACE)                                                 -
      ISOLATION(CS)                                                   -
      VALIDATE(BIND)                                                  -
      ACQUIRE(USE)                                                    -
      RELEASE(COMMIT)                                                 -
      EXPLAIN(NO)                                                     -
      CURRENTDATA(NO)
 BIND PACKAGE(CABSCOL)                                                -
      MEMBER(CABJUR08)                                                -
      LIBRARY('TELCABS.CABS.DBRMLIB')                                 -
      ACTION(REPLACE)                                                 -
      ISOLATION(CS)                                                   -
      VALIDATE(BIND)                                                  -
      ACQUIRE(USE)                                                    -
      RELEASE(COMMIT)                                                 -
      EXPLAIN(NO)                                                     -
      CURRENTDATA(NO)
 FREE PLAN(CABSPLAN)
 BIND PLAN(CABSPLAN)                                                  -
      PKLIST(CABSCOL.*)                                                -
      ACTION(REPLACE) RETAIN                                          -
      ISOLATION(CS)                                                   -
      VALIDATE(BIND)                                                  -
      ACQUIRE(USE)                                                    -
      RELEASE(COMMIT)                                                 -
      EXPLAIN(NO)                                                     -
      CACHESIZE(1024)
/*
//*
//STEP020  EXEC PGM=IKJEFT01,DYNAMNBR=20,COND=(4,LT,STEP010)
//STEPLIB  DD DSN=DSNP.SDSNLOAD,DISP=SHR
//SYSTSPRT DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//DBRMLIB  DD DSN=TELCABS.SETL.DBRMLIB,DISP=SHR
//SYSTSIN  DD *
 DSN SYSTEM(DSNP)
 BIND PACKAGE(SETLCOL)                                                -
      MEMBER(CABSET12)                                                -
      LIBRARY('TELCABS.SETL.DBRMLIB')                                 -
      ACTION(REPLACE)                                                 -
      ISOLATION(CS)                                                   -
      VALIDATE(BIND)                                                  -
      ACQUIRE(USE)                                                    -
      RELEASE(COMMIT)                                                 -
      EXPLAIN(NO)                                                     -
      CURRENTDATA(NO)
 BIND PACKAGE(SETLCOL)                                                -
      MEMBER(CABSET13)                                                -
      LIBRARY('TELCABS.SETL.DBRMLIB')                                 -
      ACTION(REPLACE)                                                 -
      ISOLATION(CS)                                                   -
      VALIDATE(BIND)                                                  -
      ACQUIRE(USE)                                                    -
      RELEASE(COMMIT)                                                 -
      EXPLAIN(NO)                                                     -
      CURRENTDATA(NO)
 FREE PLAN(SETLPLAN)
 BIND PLAN(SETLPLAN)                                                  -
      PKLIST(SETLCOL.* CABSCOL.CABJUR10)                               -
      ACTION(REPLACE) RETAIN                                          -
      ISOLATION(CS)                                                   -
      VALIDATE(BIND)                                                  -
      ACQUIRE(USE)                                                    -
      RELEASE(COMMIT)                                                 -
      EXPLAIN(NO)                                                     -
      CACHESIZE(1024)
/*
//
