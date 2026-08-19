//CABVS030 JOB (CABS,DBA),'CARRIER MASTER KSDS DEFINE',
//             CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=4M,TIME=(10,0)
//*****************************************************************
//* CABVS030 - DEFINE THE CARRIER (OCN) MASTER KSDS                *
//*                                                                *
//* DEFINES TELCABS.CABS.CARRIER, KEYED ON CR-OCN (4 BYTES,        *
//* OFFSET 0) PER CABSCARR.  THIS IS THE LIBRARY-STANDARD DEFINE   *
//* FOR THE CLUSTER, MAINTAINED SEPARATELY FROM THE BULK REGION    *
//* PROVISIONING JOB (CABGDGDF) SO THE CARRIER MASTER CAN BE       *
//* REBUILT ON ITS OWN WITHOUT TOUCHING THE OTHER MASTERS.         *
//*                                                                *
//* THE CISZ/FREESPACE VALUES BELOW HAVE DRIFTED SLIGHTLY FROM     *
//* THE CABGDGDF COPY OVER SUCCESSIVE DBA HANDOVERS - NEITHER JOB  *
//* HAS BEEN TREATED AS THE SYSTEM OF RECORD FOR SIZING.           *
//*                                                                *
//* REVISION HISTORY                                               *
//*   1988-05-11  R.T.WHEELER   INITIAL DEFINITION                 *
//*   1993-02-24  D.OKONKWO     ACNA ADDED TO CR-IDENT - RECORD     *
//*                             LENGTH UNCHANGED, FILLER SHRUNK     *
//*   2002-07-19  A.BUKOWSKI    FREESPACE WIDENED FOR THE CLEC      *
//*                             ONBOARDING WAVE                     *
//*   2010-03-08  L.FERREIRA    IMBED KEPT FOR CONSISTENCY WITH     *
//*                             THE 1988 DEFINITION - NOT KNOWN TO  *
//*                             STILL IMPROVE PERFORMANCE ON        *
//*                             CURRENT DASD                        *
//*****************************************************************
//STEP010  EXEC PGM=IDCAMS,REGION=4M
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  SET MAXCC = 0
  DELETE TELCABS.CABS.CARRIER CLUSTER PURGE
  DEFINE CLUSTER -
      (NAME(TELCABS.CABS.CARRIER) -
       INDEXED -
       KEYS(4 0) -
       RECORDSIZE(200 200) -
       FREESPACE(15 15) -
       SHAREOPTIONS(2 3) -
       VOLUMES(TELV02) -
       CYLINDERS(6 6) -
       IMBED -
       REPLICATE) -
      DATA -
      (NAME(TELCABS.CABS.CARRIER.DATA) -
       CISZ(4096)) -
      INDEX -
      (NAME(TELCABS.CABS.CARRIER.INDEX) -
       CISZ(1024))
  IF LASTCC > 4 THEN -
    SET MAXCC = 12
  ELSE -
    SET MAXCC = 0
/*
//*
//* STEP020 - LISTCAT VERIFICATION AGAINST THE CABGDGDF COPY.      *
//* OPERATIONS IS EXPECTED TO EYEBALL BOTH LISTCATS WHEN A NEW     *
//* REGION IS STOOD UP - THERE IS NO AUTOMATED COMPARE.            *
//*
//STEP020  EXEC PGM=IDCAMS,REGION=4M,COND=(4,LT)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  LISTCAT ENTRIES(TELCABS.CABS.CARRIER) ALL
/*
//
