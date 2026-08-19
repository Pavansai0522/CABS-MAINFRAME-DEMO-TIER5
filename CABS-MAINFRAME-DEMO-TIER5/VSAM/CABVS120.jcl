//CABVS120 JOB (CABS,DBA),'RUN CONTROL ESDS DEFINE',
//             CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=4M,TIME=(10,0)
//*****************************************************************
//* CABVS120 - DEFINE THE RUN CONTROL / BALANCING RECORD ESDS      *
//*                                                                *
//* DEFINES TELCABS.CABS.RUNCTL.ES, AN ENTRY-SEQUENCED CLUSTER     *
//* SIZED FOR THE 180-BYTE CABS-CONTROL-RECORD LAYOUT (CABSCTL).   *
//* NOT TO BE CONFUSED WITH TELCABS.CABS.RUNCTL, THE GDG CHAIN     *
//* EVERY INGEST AND RATING STEP WRITES ITS CTLOUT RECORD TO -     *
//* THE NAMES ARE SIMILAR BUT THE DATASETS ARE NOT THE             *
//* SAME.                                                          *
//*                                                                *
//* PRE-DATES THE RUNCTL GDG CHAIN INTRODUCED IN 1998.  THIS       *
//* CLUSTER SUPPORTED THE ORIGINAL BATCH-WINDOW LOCKING SCHEME     *
//* WHERE A SINGLE DIRECTLY ADDRESSABLE CONTROL ROW GATED WHETHER  *
//* A SECOND COPY OF THE NIGHTLY STREAM COULD START.  ALLOCATED    *
//* UNDER CR-4471 FOR THE 1994 CONVERSION.  RERUN ONLY ON A COLD   *
//* START OF A NEW REGION.                                         *
//*                                                                *
//* REVISION HISTORY                                               *
//*   1994-11-01  D.OKONKWO     INITIAL ALLOCATION - CR-4471       *
//*   1998-01-12  J.M.CASTILLO  SUPERSEDED BY THE RUNCTL GDG       *
//*                             CHAIN FOR NEW BATCH-WINDOW LOCKING;*
//*                             CLUSTER LEFT DEFINED PENDING A      *
//*                             FORMAL WITHDRAWAL REQUEST           *
//*****************************************************************
//STEP010  EXEC PGM=IDCAMS,REGION=4M
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  SET MAXCC = 0
  DELETE TELCABS.CABS.RUNCTL.ES CLUSTER PURGE
  DEFINE CLUSTER -
      (NAME(TELCABS.CABS.RUNCTL.ES) -
       NONINDEXED -
       RECORDSIZE(180 180) -
       FREESPACE(0 0) -
       SHAREOPTIONS(3 3) -
       VOLUMES(TELV06) -
       CYLINDERS(10 5) -
       SPEED) -
      DATA -
      (NAME(TELCABS.CABS.RUNCTL.ES.DATA) -
       CISZ(2048))
  IF LASTCC > 4 THEN -
    SET MAXCC = 12
  ELSE -
    SET MAXCC = 0
/*
//*
//* STEP020 - LISTCAT.  NOT ON THE STANDARD OPERATIONS CHECKLIST  *
//* SINCE NO CURRENT SCHEDULE ENTRY OPENS THIS CLUSTER.  KEPT FOR *
//* THE COLD-START REBUILD PROCEDURE ONLY.                        *
//*
//STEP020  EXEC PGM=IDCAMS,REGION=4M,COND=(4,LT)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  LISTCAT ENTRIES(TELCABS.CABS.RUNCTL.ES) ALL
/*
//
