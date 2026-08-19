//CABPVSAM PROC FUNC=LISTCAT,DSN=
//*****************************************************************
//* CABPVSAM - IDCAMS UTILITY PROC (REPRO / PRINT / VERIFY /     *
//* LISTCAT).  THE CALLER SUPPLIES THE SYSIN COMMAND CARD SET    *
//* AS AN OVERRIDE - &FUNC IS DOCUMENTATION ONLY, IT DOES NOT    *
//* DRIVE ANY LOGIC INSIDE THIS PROC.                            *
//*****************************************************************
//STEP1    EXEC PGM=IDCAMS,REGION=4M
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  LISTCAT ENTRIES(&DSN) ALL
/*
