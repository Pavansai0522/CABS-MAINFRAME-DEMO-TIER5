*=====================================================================*
*  CABDATCV - DATE CONVERSION AND DAY ARITHMETIC                      *
*  APPLICATION : CABS / SETL                                          *
*  ATTRIBUTES  : AMODE 24, RMODE 24                                   *
*  RESIDES IN  : TELCABS.COMMON.LOADLIB                               *
*  SOURCE      : TELCABS.CABS.SRCLIB(CABDATCV)                        *
*  INVOKED BY  : STATIC CALL FROM MOST BATCH MODULES.  ALSO ENTERED   *
*                UNDER THE ALIAS CABDTCNV, WHICH IS THE NAME THE      *
*                COBOL SIDE USES.                                     *
*                                                                     *
*  PURPOSE                                                            *
*    EVERY DATE IN CABS IS HELD AS A FIVE DIGIT YYDDD.  THIS MODULE   *
*    CONVERTS BETWEEN YYDDD, GREGORIAN CCYYMMDD AND AN ABSOLUTE DAY   *
*    NUMBER, AND ADDS OR SUBTRACTS DAYS WITH YEAR ROLLOVER.  THE      *
*    ABSOLUTE DAY NUMBER IS DAYS SINCE 1900-01-01 AND IS THE ONLY     *
*    SAFE BASIS FOR COMPARING TWO DATES ACROSS A YEAR BOUNDARY.       *
*                                                                     *
*  CENTURY RESOLUTION                                                 *
*    A TWO DIGIT YEAR OF 70 OR ABOVE IS TAKEN AS NINETEEN HUNDRED.    *
*    BELOW 70 IT IS TAKEN AS TWENTY HUNDRED.  THE PIVOT IS THE        *
*    CONSTANT PIVOTYY BELOW AND MATCHES DW-PIVOT-YY IN CABSDATE.      *
*    IT IS ALSO CODED AS A LITERAL IN SEVERAL COBOL MODULES.  ANY     *
*    CHANGE HERE MUST BE MADE IN ALL OF THEM AT THE SAME TIME - SEE   *
*    THE STANDING NOTE ON CABS-STD-007.                               *
*                                                                     *
*  LINKAGE CONVENTION                                                 *
*    STANDARD OS LINKAGE.  R1 ADDRESSES A FIVE WORD PARAMETER LIST -  *
*      +0   A(FUNCTION CODE)  CL4  'JTOG' 'GTOJ' 'JTOA' 'ATOJ'        *
*                                  'ADDD' 'DIFF' 'LEAP'               *
*      +4   A(INPUT 1)        DEPENDS ON FUNCTION, SEE BELOW          *
*      +8   A(INPUT 2)        SIGNED DAY COUNT PL4 FOR ADDD, OR THE   *
*                             SECOND DATE FOR DIFF                    *
*      +12  A(OUTPUT)         DEPENDS ON FUNCTION                     *
*      +16  A(RETURN AREA)    F    0 OK, 4 INVALID DATE, 8 INVALID    *
*                                  FUNCTION, 12 RESULT OUT OF RANGE   *
*                                                                     *
*      JTOG  IN  YYDDD PL3     OUT CCYYMMDD PL5                       *
*      GTOJ  IN  CCYYMMDD PL5  OUT YYDDD PL3                          *
*      JTOA  IN  YYDDD PL3     OUT ABSOLUTE DAY F                     *
*      ATOJ  IN  ABSOLUTE F    OUT YYDDD PL3                          *
*      ADDD  IN  YYDDD PL3     IN2 SIGNED DAYS PL4  OUT YYDDD PL3     *
*      DIFF  IN  YYDDD PL3     IN2 YYDDD PL3        OUT DAYS PL4      *
*      LEAP  IN  YYDDD PL3     OUT CL1 'Y' OR 'N'                     *
*                                                                     *
*  REVISION HISTORY                                                   *
*    V1.00  1987-06-30  R.T.WHEELER   INITIAL                         *
*    V1.04  1992-01-23  D.OKONKWO     ABSOLUTE DAY FUNCTIONS ADDED    *
*    V1.06  1995-10-11  J.M.CASTILLO  PIVOT INTRODUCED AT 70          *
*    V1.09  1998-12-08  D.OKONKWO     LEAP TEST CORRECTED FOR THE     *
*                                     CENTURY RULE                    *
*    V2.00  2003-04-17  P.NAIR        ADDD REWRITTEN TO GO THROUGH    *
*                                     THE ABSOLUTE DAY RATHER THAN    *
*                                     STEPPING A YEAR AT A TIME       *
*    V2.02  2012-08-20  A.BUKOWSKI    RANGE CHECK ON THE RESULT       *
*    V2.03  2018-02-14  M.HAAS        REASSEMBLED - NO SOURCE CHANGE  *
*=====================================================================*
CABDATCV CSECT
CABDATCV AMODE 24
CABDATCV RMODE 24
         ENTRY CABDTCNV
         USING *,R15
         B     BEGIN
         DC    AL1(16)
         DC    CL16'CABDATCV V2.03  '
         DROP  R15
*
CABDTCNV DS    0H
BEGIN    STM   R14,R12,12(R13)
         LR    R12,R15
         USING CABDATCV,R12
         LA    R11,SAVEAREA
         ST    R13,4(,R11)
         ST    R11,8(,R13)
         LR    R13,R11
         LR    R11,R1
         USING PARMLIST,R11
*
         L     R2,PFUNC
         L     R3,PIN1
         L     R4,PIN2
         L     R5,POUT
         L     R6,PRETRN
         XC    0(4,R6),0(R6)
*
         CLC   0(4,R2),=CL4'JTOG'
         BE    DOJTOG
         CLC   0(4,R2),=CL4'GTOJ'
         BE    DOGTOJ
         CLC   0(4,R2),=CL4'JTOA'
         BE    DOJTOA
         CLC   0(4,R2),=CL4'ATOJ'
         BE    DOATOJ
         CLC   0(4,R2),=CL4'ADDD'
         BE    DOADDD
         CLC   0(4,R2),=CL4'DIFF'
         BE    DODIFF
         CLC   0(4,R2),=CL4'LEAP'
         BE    DOLEAP
         LA    R0,8
         ST    R0,0(,R6)
         B     RETURN
*
*---------------------------------------------------------------------*
*  SPLIT A YYDDD INTO ITS PARTS AND RESOLVE THE CENTURY.              *
*---------------------------------------------------------------------*
SPLITJUL DS    0H
         ZAP   JULWORK,0(3,R3)
         ZAP   DBLWORK,JULWORK
         CVB   R7,DBLWORK
         SR    R8,R8
         LR    R9,R7
         D     R8,=F'1000'             R9 = YY, R8 = DDD
         ST    R9,YEARYY
         ST    R8,DAYDDD
         CLC   YEARYY+3(1),PIVOTYY
         L     R9,YEARYY
         C     R9,PIVOTFW
         BL    SPLIT20
         LA    R7,1900
         AR    R7,R9
         ST    R7,YEARCC
         BR    R10
SPLIT20  LA    R7,2000
         AR    R7,R9
         ST    R7,YEARCC
         BR    R10
*
*---------------------------------------------------------------------*
*  LEAP YEAR TEST ON THE FULL FOUR DIGIT YEAR IN YEARCC.  RESULT IN   *
*  LEAPSW.  DIVISIBLE BY FOUR AND NOT BY ONE HUNDRED, OR DIVISIBLE    *
*  BY FOUR HUNDRED.  THE CENTURY RULE WAS ADDED IN 1998 - BEFORE      *
*  THAT THE MODULE WOULD HAVE TREATED 1900 AS A LEAP YEAR, WHICH      *
*  MATTERED ONLY FOR THE HISTORY FILES.                               *
*---------------------------------------------------------------------*
LEAPTEST DS    0H
         MVI   LEAPSW,C'N'
         L     R7,YEARCC
         SR    R8,R8
         LR    R9,R7
         D     R8,=F'400'
         LTR   R8,R8
         BZ    LEAPYES
         L     R7,YEARCC
         SR    R8,R8
         LR    R9,R7
         D     R8,=F'100'
         LTR   R8,R8
         BZ    LEAPXIT
         L     R7,YEARCC
         SR    R8,R8
         LR    R9,R7
         D     R8,=F'4'
         LTR   R8,R8
         BNZ   LEAPXIT
LEAPYES  MVI   LEAPSW,C'Y'
LEAPXIT  BR    R10
*
*---------------------------------------------------------------------*
*  JULIAN TO GREGORIAN                                                *
*---------------------------------------------------------------------*
DOJTOG   DS    0H
         BAL   R10,SPLITJUL
         BAL   R10,LEAPTEST
         L     R8,DAYDDD
         LTR   R8,R8
         BNP   BADDATE
         C     R8,=F'366'
         BH    BADDATE
         CLI   LEAPSW,C'Y'
         BE    JTOGLEAP
         C     R8,=F'365'
         BH    BADDATE
         LA    R9,MONTAB
         B     JTOGWALK
JTOGLEAP LA    R9,MONTABL
JTOGWALK DS    0H
         SR    R7,R7                   MONTH NUMBER
JTOGLOOP LA    R7,1(,R7)
         LH    R0,0(,R9)
         CR    R8,R0
         BNH   JTOGDONE
         SR    R8,R0
         LA    R9,2(,R9)
         C     R7,=F'12'
         BL    JTOGLOOP
         B     BADDATE
JTOGDONE DS    0H
         L     R0,YEARCC
         CVD   R0,DBLWORK
         ZAP   GREGWK,DBLWORK
         MP    GREGWK,=P'100'
         CVD   R7,DBLWORK
         AP    GREGWK,DBLWORK
         MP    GREGWK,=P'100'
         CVD   R8,DBLWORK
         AP    GREGWK,DBLWORK
         ZAP   0(5,R5),GREGWK
         B     RETURN
*
*---------------------------------------------------------------------*
*  GREGORIAN TO JULIAN                                                *
*---------------------------------------------------------------------*
DOGTOJ   DS    0H
         ZAP   GREGWK,0(5,R3)
         ZAP   DBLWORK,GREGWK
         CVB   R7,DBLWORK
         SR    R8,R8
         LR    R9,R7
         D     R8,=F'100'              R8 = DD
         ST    R8,DAYDD
         SR    R8,R8
         D     R8,=F'100'              R8 = MM
         ST    R8,MONTHMM
         ST    R9,YEARCC
         BAL   R10,LEAPTEST
         L     R7,MONTHMM
         LTR   R7,R7
         BNP   BADDATE
         C     R7,=F'12'
         BH    BADDATE
         CLI   LEAPSW,C'Y'
         BE    GTOJLEAP
         LA    R9,MONTAB
         B     GTOJACC
GTOJLEAP LA    R9,MONTABL
GTOJACC  DS    0H
         SR    R0,R0                   DAY ACCUMULATOR
         BCTR  R7,0
         LTR   R7,R7
         BZ    GTOJADD
GTOJLOOP LH    R1,0(,R9)
         AR    R0,R1
         LA    R9,2(,R9)
         BCT   R7,GTOJLOOP
GTOJADD  A     R0,DAYDD
         ST    R0,DAYDDD
         L     R9,YEARCC
         C     R9,=F'2000'
         BL    GTOJ19
         S     R9,=F'2000'
         B     GTOJBLD
GTOJ19   S     R9,=F'1900'
GTOJBLD  M     R8,=F'1000'
         A     R9,DAYDDD
         CVD   R9,DBLWORK
         ZAP   0(3,R5),DBLWORK
         B     RETURN
*
*---------------------------------------------------------------------*
*  JULIAN TO ABSOLUTE DAY NUMBER.  BASE IS 1900-01-01 = 1.            *
*---------------------------------------------------------------------*
DOJTOA   DS    0H
         BAL   R10,SPLITJUL
         L     R7,YEARCC
         S     R7,=F'1900'
         LTR   R7,R7
         BM    BADDATE
         LR    R9,R7
         M     R8,=F'365'
         ST    R9,ABSWORK
         LR    R8,R7
         BCTR  R8,0
         LTR   R8,R8
         BM    JTOANOLP
         LR    R9,R8
         SRA   R9,2                    LEAP DAYS SINCE 1900
         L     R0,ABSWORK
         AR    R0,R9
         ST    R0,ABSWORK
JTOANOLP DS    0H
         L     R0,ABSWORK
         A     R0,DAYDDD
         ST    R0,0(,R5)
         B     RETURN
*
*---------------------------------------------------------------------*
*  ABSOLUTE DAY NUMBER TO JULIAN                                      *
*---------------------------------------------------------------------*
DOATOJ   DS    0H
         L     R7,0(,R3)
         LTR   R7,R7
         BNP   BADDATE
         SR    R9,R9
         LA    R8,1900
         ST    R8,YEARCC
ATOJLOOP DS    0H
         BAL   R10,LEAPTEST
         LA    R0,365
         CLI   LEAPSW,C'Y'
         BNE   ATOJTEST
         LA    R0,366
ATOJTEST CR    R7,R0
         BNH   ATOJDONE
         SR    R7,R0
         L     R8,YEARCC
         LA    R8,1(,R8)
         ST    R8,YEARCC
         B     ATOJLOOP
ATOJDONE DS    0H
         ST    R7,DAYDDD
         L     R9,YEARCC
         C     R9,=F'2000'
         BL    ATOJ19
         S     R9,=F'2000'
         B     ATOJBLD
ATOJ19   S     R9,=F'1900'
ATOJBLD  M     R8,=F'1000'
         A     R9,DAYDDD
         CVD   R9,DBLWORK
         ZAP   0(3,R5),DBLWORK
         B     RETURN
*
*---------------------------------------------------------------------*
*  ADD OR SUBTRACT DAYS.  DONE THROUGH THE ABSOLUTE DAY NUMBER SO     *
*  THE YEAR ROLLS CORRECTLY IN BOTH DIRECTIONS.                       *
*---------------------------------------------------------------------*
DOADDD   DS    0H
         LA    R1,SUBPARM
         ST    R3,SUBIN1
         LA    R0,ABSHOLD
         ST    R0,SUBOUT
         BAL   R10,CALLJTOA
         ZAP   DBLWORK,0(4,R4)
         CVB   R7,DBLWORK
         L     R0,ABSHOLD
         AR    R0,R7
         LTR   R0,R0
         BNP   OUTRANGE
         ST    R0,ABSHOLD
         LA    R0,ABSHOLD
         ST    R0,SUBIN1
         ST    R5,SUBOUT
         BAL   R10,CALLATOJ
         B     RETURN
*
CALLJTOA DS    0H
         L     R3,SUBIN1
         BAL   R9,JTOAINLN
         BR    R10
CALLATOJ DS    0H
         L     R3,SUBIN1
         BAL   R9,ATOJINLN
         BR    R10
JTOAINLN DS    0H
         L     R5,SUBOUT
         BAL   R10,SPLITJUL
         L     R7,YEARCC
         S     R7,=F'1900'
         LR    R9,R7
         M     R8,=F'365'
         ST    R9,ABSWORK
         LR    R8,R7
         BCTR  R8,0
         SRA   R8,2
         L     R0,ABSWORK
         AR    R0,R8
         A     R0,DAYDDD
         ST    R0,0(,R5)
         BR    R9
ATOJINLN DS    0H
         L     R5,SUBOUT
         B     DOATOJ
*
*---------------------------------------------------------------------*
*  DIFFERENCE IN DAYS BETWEEN TWO YYDDD VALUES.                       *
*---------------------------------------------------------------------*
DODIFF   DS    0H
         BAL   R10,SPLITJUL
         L     R7,YEARCC
         S     R7,=F'1900'
         LR    R9,R7
         M     R8,=F'365'
         LR    R0,R9
         A     R0,DAYDDD
         ST    R0,ABSHOLD
         LR    R3,R4
         BAL   R10,SPLITJUL
         L     R7,YEARCC
         S     R7,=F'1900'
         LR    R9,R7
         M     R8,=F'365'
         LR    R0,R9
         A     R0,DAYDDD
         L     R1,ABSHOLD
         SR    R1,R0
         CVD   R1,DBLWORK
         ZAP   0(4,R5),DBLWORK
         B     RETURN
*
*---------------------------------------------------------------------*
*  LEAP YEAR INDICATOR                                                *
*---------------------------------------------------------------------*
DOLEAP   DS    0H
         BAL   R10,SPLITJUL
         BAL   R10,LEAPTEST
         MVC   0(1,R5),LEAPSW
         B     RETURN
*
BADDATE  LA    R0,4
         ST    R0,0(,R6)
         B     RETURN
OUTRANGE LA    R0,12
         ST    R0,0(,R6)
         B     RETURN
*
RETURN   DS    0H
         L     R13,4(,R13)
         L     R14,12(,R13)
         LM    R0,R12,20(R13)
         SR    R15,R15
         BR    R14
*
*---------------------------------------------------------------------*
*  CONSTANTS                                                          *
*---------------------------------------------------------------------*
PIVOTYY  DC    X'70'                   CENTURY PIVOT, TWO DIGIT
PIVOTFW  DC    F'70'
MONTAB   DC    H'31,28,31,30,31,30,31,31,30,31,30,31'
MONTABL  DC    H'31,29,31,30,31,30,31,31,30,31,30,31'
*
         LTORG
*
SAVEAREA DS    18F
DBLWORK  DS    D
JULWORK  DS    PL3
GREGWK   DS    PL5
YEARYY   DS    F
YEARCC   DS    F
MONTHMM  DS    F
DAYDD    DS    F
DAYDDD   DS    F
ABSWORK  DS    F
ABSHOLD  DS    F
LEAPSW   DS    CL1
         DS    0F
SUBPARM  DS    0F
SUBIN1   DS    A
SUBOUT   DS    A
*
PARMLIST DSECT
PFUNC    DS    A
PIN1     DS    A
PIN2     DS    A
POUT     DS    A
PRETRN   DS    A
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
         END   CABDATCV
