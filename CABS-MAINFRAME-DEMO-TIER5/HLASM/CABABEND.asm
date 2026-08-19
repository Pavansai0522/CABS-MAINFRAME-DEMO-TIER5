*=====================================================================*
*  CABABEND - CONTROLLED ABEND AND DUMP FORMATTER                     *
*  APPLICATION : CABS / SETL                                          *
*  ATTRIBUTES  : AMODE 24, RMODE 24                                   *
*  RESIDES IN  : TELCABS.COMMON.LOADLIB                               *
*  INVOKED BY  : STATIC CALL FROM EVERY BATCH MODULE.  THE COBOL      *
*                SIDE CALLS IT FROM P9500-ABEND OR P9900-FATAL-EXIT   *
*                AFTER SETTING CT-ABEND-CD.                           *
*                                                                     *
*  PURPOSE                                                            *
*    GIVES THE ESTATE ONE PLACE WHERE A CONTROLLED FAILURE IS         *
*    TURNED INTO AN OPERATOR MESSAGE, A FORMATTED SNAP OF THE         *
*    CALLER'S KEY AREAS AND A USER COMPLETION CODE.  WITHOUT IT       *
*    EACH MODULE WOULD PRODUCE ITS OWN DIALECT OF FAILURE AND THE     *
*    NIGHT SHIFT WOULD HAVE NOTHING CONSISTENT TO ACT ON.             *
*                                                                     *
*  WHAT IT DOES                                                       *
*    1.  WTOS A THREE LINE MESSAGE TO THE OPERATOR CONSOLE AND TO     *
*        THE JOB LOG, CARRYING THE MODULE NAME, THE PARAGRAPH NAME,   *
*        THE ABEND CODE AND UP TO FIFTY CHARACTERS OF TEXT.           *
*    2.  FORMATS THE CALLER SUPPLIED DIAGNOSTIC AREA AS SIDE BY       *
*        SIDE HEXADECIMAL AND CHARACTER AND WRITES IT TO SYSPRINT     *
*        IF THAT DD IS ALLOCATED.  IF IT IS NOT, THE FORMATTING IS    *
*        SKIPPED SILENTLY - A MISSING SYSPRINT MUST NOT TURN A        *
*        DIAGNOSABLE ABEND INTO AN 013.                               *
*    3.  ISSUES A SNAP OF THE CALLER'S SAVE AREA CHAIN IF THE         *
*        SNAPDD DD IS ALLOCATED.                                      *
*    4.  ABENDS WITH THE USER CODE SUPPLIED BY THE CALLER, DUMP       *
*        REQUESTED, UNLESS THE CALLER ASKED FOR A RETURN.             *
*                                                                     *
*  THE RETURN OPTION                                                  *
*    A CALLER THAT PASSES ACTION CODE 'R' GETS THE MESSAGES AND THE   *
*    FORMATTED AREA BUT NO ABEND.  THAT IS HOW THE RESTART DRIVER     *
*    RECORDS A FAILURE IT INTENDS TO RECOVER FROM.  A CALLER THAT     *
*    PASSES ANY OTHER VALUE, INCLUDING SPACES, GETS THE ABEND.        *
*                                                                     *
*  LINKAGE CONVENTION                                                 *
*    STANDARD OS LINKAGE.  R1 ADDRESSES A SIX WORD PARAMETER LIST -   *
*      +0   A(ABEND CODE)     CL4  DISPLAY DIGITS, 0001 TO 4095       *
*      +4   A(MODULE NAME)    CL8                                     *
*      +8   A(PARAGRAPH NAME) CL30                                    *
*      +12  A(MESSAGE TEXT)   CL50                                    *
*      +16  A(DIAGNOSTIC AREA) THE FIRST HALFWORD IS THE LENGTH TO    *
*                              FORMAT, MAXIMUM 512, FOLLOWED BY THE   *
*                              DATA.  A LENGTH OF ZERO SUPPRESSES     *
*                              THE FORMATTING.                        *
*      +20  A(ACTION CODE)    CL1  'A' ABEND, 'R' RETURN              *
*                                                                     *
*  REVISION HISTORY                                                   *
*    V1.00  1988-01-25  R.T.WHEELER   INITIAL                         *
*    V1.03  1990-07-12  D.OKONKWO     SYSPRINT MADE OPTIONAL          *
*    V1.06  1994-11-04  J.M.CASTILLO  SNAP ADDED                      *
*    V1.09  1999-03-22  D.OKONKWO     ABEND CODE VALIDATED AND        *
*                                     FORCED TO 4000 IF OUT OF RANGE  *
*    V2.00  2005-11-15  P.NAIR        ACTION CODE R ADDED FOR THE     *
*                                     RESTART DRIVER                  *
*    V2.01  2015-06-18  A.BUKOWSKI    REASSEMBLED - NO SOURCE CHANGE  *
*=====================================================================*
CABABEND CSECT
CABABEND AMODE 24
CABABEND RMODE 24
         USING *,R15
         B     BEGIN
         DC    AL1(16)
         DC    CL16'CABABEND V2.01  '
         DROP  R15
*
BEGIN    STM   R14,R12,12(R13)
         LR    R12,R15
         USING CABABEND,R12
         LA    R11,SAVEAREA
         ST    R13,4(,R11)
         ST    R11,8(,R13)
         LR    R10,R13                 HOLD CALLER SAVE AREA
         LR    R13,R11
         LR    R11,R1
         USING PARMLIST,R11
*
         L     R2,PABCODE
         L     R3,PMODULE
         L     R4,PPARA
         L     R5,PTEXT
         L     R6,PDIAG
         L     R7,PACTION
*
         MVC   HOLDCODE,0(R2)
         MVC   HOLDMOD,0(R3)
         MVC   HOLDPARA,0(R4)
         MVC   HOLDTEXT,0(R5)
         MVC   HOLDACT,0(R7)
*
*        VALIDATE THE ABEND CODE.  ANYTHING THAT IS NOT FOUR
*        DISPLAY DIGITS IN RANGE IS FORCED TO 4000 SO THE ABEND
*        ITSELF CANNOT FAIL.
*
         CLC   HOLDCODE,=CL4'0000'
         BNH   BADCODE
         CLC   HOLDCODE,=CL4'4095'
         BH    BADCODE
         TRT   HOLDCODE,NUMTAB
         BNZ   BADCODE
         B     CODEOK
BADCODE  MVC   HOLDCODE,=CL4'4000'
CODEOK   DS    0H
         PACK  DBLWORK,HOLDCODE
         CVB   R8,DBLWORK
         ST    R8,ABNUM
*
*---------------------------------------------------------------------*
*  BUILD AND ISSUE THE OPERATOR MESSAGES                              *
*---------------------------------------------------------------------*
         MVC   WTOL1+8(8),HOLDMOD
         MVC   WTOL1+20(4),HOLDCODE
         WTO   MF=(E,WTOL1)
         MVC   WTOL2+8(30),HOLDPARA
         WTO   MF=(E,WTOL2)
         MVC   WTOL3+8(50),HOLDTEXT
         WTO   MF=(E,WTOL3)
*
*---------------------------------------------------------------------*
*  FORMAT THE DIAGNOSTIC AREA IF THERE IS ONE AND SYSPRINT IS THERE   *
*---------------------------------------------------------------------*
         LH    R8,0(,R6)
         LTR   R8,R8
         BNP   NODIAG
         C     R8,=F'512'
         BNH   LENOK
         LA    R8,512
LENOK    ST    R8,DIAGLEN
         LA    R9,2(,R6)
         ST    R9,DIAGADR
         BAL   R14,OPENPRT
         CLI   PRTOPEN,C'N'
         BE    NODIAG
         BAL   R14,FMTDIAG
         CLOSE (SYSPRINT)
NODIAG   DS    0H
*
*---------------------------------------------------------------------*
*  SNAP THE SAVE AREA CHAIN IF SNAPDD IS ALLOCATED                    *
*---------------------------------------------------------------------*
         OPEN  (SNAPDD,OUTPUT)
         LTR   R15,R15
         BNZ   NOSNAP
         SNAP  DCB=SNAPDD,ID=1,PDATA=(REGS,SA),                        X
               STORAGE=((R10),(R10))
         CLOSE (SNAPDD)
NOSNAP   DS    0H
*
*---------------------------------------------------------------------*
*  ABEND OR RETURN                                                    *
*---------------------------------------------------------------------*
         CLI   HOLDACT,C'R'
         BE    RETURN
         L     R1,ABNUM
         ABEND (1),DUMP
*
RETURN   DS    0H
         L     R13,4(,R13)
         L     R14,12(,R13)
         LM    R0,R12,20(R13)
         LA    R15,4                   FAILURE RECORDED, NOT ABENDED
         BR    R14
*
*---------------------------------------------------------------------*
*  OPENPRT - OPEN SYSPRINT.  A FAILURE IS TOLERATED.                  *
*---------------------------------------------------------------------*
OPENPRT  DS    0H
         ST    R14,SAVE14A
         MVI   PRTOPEN,C'N'
         OPEN  (SYSPRINT,OUTPUT)
         LTR   R15,R15
         BNZ   OPENXIT
         MVI   PRTOPEN,C'Y'
         MVC   PRTLINE,BLANKS
         MVI   PRTLINE,C'1'
         MVC   PRTLINE+1(31),HDRTEXT
         MVC   PRTLINE+33(8),HOLDMOD
         MVC   PRTLINE+43(4),HOLDCODE
         PUT   SYSPRINT,PRTLINE
         MVC   PRTLINE,BLANKS
         MVC   PRTLINE+1(30),HOLDPARA
         PUT   SYSPRINT,PRTLINE
         MVC   PRTLINE,BLANKS
         MVC   PRTLINE+1(50),HOLDTEXT
         PUT   SYSPRINT,PRTLINE
OPENXIT  L     R14,SAVE14A
         BR    R14
*
*---------------------------------------------------------------------*
*  FMTDIAG - SIXTEEN BYTES PER LINE, HEXADECIMAL AND CHARACTER.       *
*  A BYTE THAT WILL NOT PRINT IS SHOWN AS A FULL STOP.                *
*---------------------------------------------------------------------*
FMTDIAG  DS    0H
         ST    R14,SAVE14B
         L     R9,DIAGADR
         L     R8,DIAGLEN
         SR    R7,R7                   OFFSET
FMTLOOP  DS    0H
         MVC   PRTLINE,BLANKS
         CVD   R7,DBLWORK
         OI    DBLWORK+7,X'0F'
         UNPK  PRTLINE+1(5),DBLWORK+5(3)
         LR    R6,R8
         C     R6,=F'16'
         BNH   FMTSHORT
         LA    R6,16
FMTSHORT DS    0H
         LR    R1,R6
         BCTR  R1,0
         EX    R1,MVCHEX
         UNPK  HEXWORK(33),HEXIN(17)
         TR    HEXWORK(32),HEXTAB-C'0'
         MVC   PRTLINE+8(8),HEXWORK
         MVC   PRTLINE+17(8),HEXWORK+8
         MVC   PRTLINE+26(8),HEXWORK+16
         MVC   PRTLINE+35(8),HEXWORK+24
         MVC   CHRWORK,BLANKS
         EX    R1,MVCCHR
         TR    CHRWORK(16),PRTABLE
         MVC   PRTLINE+46(16),CHRWORK
         PUT   SYSPRINT,PRTLINE
         AR    R9,R6
         AR    R7,R6
         SR    R8,R6
         LTR   R8,R8
         BP    FMTLOOP
         L     R14,SAVE14B
         BR    R14
*
MVCHEX   MVC   HEXIN(0),0(R9)
MVCCHR   MVC   CHRWORK(0),0(R9)
*
*---------------------------------------------------------------------*
*  CONSTANTS AND CONTROL BLOCKS                                       *
*---------------------------------------------------------------------*
WTOL1    WTO   'CABS900I MODULE           ABEND CODE      ',           X
               ROUTCDE=(2,11),DESC=(6),MF=L
WTOL2    WTO   'CABS901I                                       ',      X
               ROUTCDE=(2,11),DESC=(6),MF=L
WTOL3    WTO   'CABS902I                                              *
               ',ROUTCDE=(2,11),DESC=(6),MF=L
*
HDRTEXT  DC    CL31'CABS CONTROLLED ABEND - MODULE '
BLANKS   DC    CL133' '
HEXTAB   DC    C'0123456789ABCDEF'
*
NUMTAB   DC    256X'FF'
         ORG   NUMTAB+C'0'
         DC    10X'00'
         ORG
*
PRTABLE  DC    64X'4B'
         ORG   PRTABLE+X'40'
         DC    X'40'
         ORG   PRTABLE+X'4B'
         DC    X'4B4C4D4E4F50'
         ORG   PRTABLE+X'5B'
         DC    X'5B5C5D5E5F60616263'
         ORG   PRTABLE+X'6B'
         DC    X'6B6C6D6E6F'
         ORG   PRTABLE+X'7A'
         DC    X'7A7B7C7D7E7F'
         ORG   PRTABLE+X'81'
         DC    X'818283848586878889'
         ORG   PRTABLE+X'91'
         DC    X'919293949596979899'
         ORG   PRTABLE+X'A2'
         DC    X'A2A3A4A5A6A7A8A9'
         ORG   PRTABLE+X'C1'
         DC    X'C1C2C3C4C5C6C7C8C9'
         ORG   PRTABLE+X'D1'
         DC    X'D1D2D3D4D5D6D7D8D9'
         ORG   PRTABLE+X'E2'
         DC    X'E2E3E4E5E6E7E8E9'
         ORG   PRTABLE+X'F0'
         DC    X'F0F1F2F3F4F5F6F7F8F9'
         ORG
*
SYSPRINT DCB   DDNAME=SYSPRINT,DSORG=PS,MACRF=(PM),                    X
               RECFM=FBA,LRECL=133,BLKSIZE=1330
SNAPDD   DCB   DDNAME=SNAPDD,DSORG=PS,MACRF=(W),                       X
               RECFM=VBA,LRECL=125,BLKSIZE=1632
*
         LTORG
*
SAVEAREA DS    18F
SAVE14A  DS    F
SAVE14B  DS    F
DBLWORK  DS    D
ABNUM    DS    F
DIAGLEN  DS    F
DIAGADR  DS    A
HOLDCODE DS    CL4
HOLDMOD  DS    CL8
HOLDPARA DS    CL30
HOLDTEXT DS    CL50
HOLDACT  DS    CL1
PRTOPEN  DS    CL1
PRTLINE  DS    CL133
HEXIN    DS    CL17
HEXWORK  DS    CL33
CHRWORK  DS    CL16
*
PARMLIST DSECT
PABCODE  DS    A
PMODULE  DS    A
PPARA    DS    A
PTEXT    DS    A
PDIAG    DS    A
PACTION  DS    A
*
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
         END   CABABEND
