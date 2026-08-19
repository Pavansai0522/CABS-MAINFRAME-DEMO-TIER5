//CABS2800 JOB (CABS,BILL),'SETTLEMENT NETTING',
//             CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M,TIME=1440
//*****************************************************************
//* CABS2800 - SETTLEMENT NETTING BY COUNTERPARTY                 *
//*                                                               *
//* NETS RECEIVABLES AGAINST PAYABLES FOR EACH COUNTERPARTY.      *
//* THE CARRIER NAME AND THE PAYMENT TERMS COME FROM THE CABS     *
//* CARRIER MASTER - A DATASET OWNED BY THE BILLING APPLICATION.  *
//* THE INTERFACE AGREEMENT IS HELD BY THE APPLICATION OWNER.     *
//* THE PROC LIBRARY IS CATALOGUED PER CABS-STD-024.              *
//*****************************************************************
//STEP010  EXEC CABPNETT,
//             PGMNAME=CABSET09,
//             CYCLE=%%CYCLDT,
//             BILLPER=%%BILLPR,
//             RUNID=%%RUNID,
//             NETPER=%%NETPER,
//             INCDISP=%%INCDSP,
//             INTRATE=%%INTRAT,
//             INGDG='0'
//*
//NETTSTEP.SETLIN   DD DSN=TELCABS.SETL.SETTLE.ALL(0),DISP=SHR
//NETTSTEP.CARRMAST DD DSN=TELCABS.CABS.CARRIER,DISP=SHR
//
