//CABPBKUP PROC DSN=,CYCLE=000000
//*****************************************************************
//* CABPBKUP - BACKUP / ARCHIVE PROC.  DUMPS A CATALOGUED        *
//* GENERATION TO TAPE VIA IDCAMS REPRO FOR OFFSITE RETENTION.   *
//*****************************************************************
//STEP1    EXEC PGM=IDCAMS,REGION=4M
//SYSPRINT DD SYSOUT=*
//BKUPIN   DD DSN=&DSN,DISP=SHR
//BKUPOUT  DD DSN=TELCABS.CABS.BACKUP.D&CYCLE,
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=TAPE,LABEL=(1,SL),
//             DCB=(RECFM=FB,LRECL=200,BLKSIZE=6000)
//SYSIN    DD *
  REPRO INFILE(BKUPIN) OUTFILE(BKUPOUT)
/*
