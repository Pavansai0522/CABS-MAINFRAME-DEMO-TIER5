//CABVS070 JOB (CABS,DBA),'CIRCUIT INVENTORY KSDS DEFINE',
//             CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=4M,TIME=(10,0)
//*****************************************************************
//* CABVS070 - DEFINE THE CIRCUIT / TRUNK GROUP INVENTORY KSDS    *
//*                                                               *
//* DEFINES TELCABS.CABS.CIRCUIT, KEYED ON CI-CIRCUIT-ID (20      *
//* BYTES, OFFSET 0) PER CABSCIRC.  ONE ROW PER PROVISIONED       *
//* CIRCUIT OR TRUNK GROUP, INCLUDING MEET-POINT SPLIT DATA USED  *
//* BY THE SETTLEMENT FAMILY.                                     *
//*                                                               *
//* REVISION HISTORY                                              *
//*   1990-06-14  D.OKONKWO     INITIAL DEFINITION                *
//*   1999-10-01  J.M.CASTILLO  MPB FIELDS ADDED TO CI-MPB -       *
//*                             RECORD LENGTH UNCHANGED, FILLER    *
//*                             REDUCED                            *
//*   2007-02-27  A.BUKOWSKI    FREESPACE WIDENED FOR THE UNE      *
//*                             PROVISIONING SURGE                 *
//*   2018-05-11  T.VANCE       SHAREOPTIONS REVIEWED - LEFT AT    *
//*                             (2 3), NO CHANGE MADE               *
//*****************************************************************
//STEP010  EXEC PGM=IDCAMS,REGION=4M
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  SET MAXCC = 0
  DELETE TELCABS.CABS.CIRCUIT CLUSTER PURGE
  DEFINE CLUSTER -
      (NAME(TELCABS.CABS.CIRCUIT) -
       INDEXED -
       KEYS(20 0) -
       RECORDSIZE(150 150) -
       FREESPACE(10 15) -
       SHAREOPTIONS(2 3) -
       VOLUMES(TELV05) -
       CYLINDERS(12 8)) -
      DATA -
      (NAME(TELCABS.CABS.CIRCUIT.DATA) -
       CISZ(4096)) -
      INDEX -
      (NAME(TELCABS.CABS.CIRCUIT.INDEX) -
       CISZ(1024))
  IF LASTCC > 4 THEN -
    SET MAXCC = 12
  ELSE -
    SET MAXCC = 0
/*
//*
//* STEP020 - LISTCAT VERIFICATION.  OPERATIONS CROSS-CHECKS THE  *
//* RECORD COUNT AGAINST THE PRIOR MONTH'S CIRCUIT INVENTORY      *
//* EXTRACT BEFORE THE MEET-POINT BILLING SETTLEMENT STREAM IS    *
//* RELEASED - A LARGE DROP IN COUNT USUALLY MEANS THE PROVISION- *
//* ING FEED TRUNCATED RATHER THAN THAT CIRCUITS WERE ACTUALLY    *
//* DISCONNECTED.                                                 *
//*
//STEP020  EXEC PGM=IDCAMS,REGION=4M,COND=(4,LT)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  LISTCAT ENTRIES(TELCABS.CABS.CIRCUIT) ALL
/*
//
