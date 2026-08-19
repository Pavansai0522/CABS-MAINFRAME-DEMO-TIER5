*=====================================================================*
*  CABDATCV - DATE CONVERSION AND DAY ARITHMETIC                      *
*  APPLICATION : SETL                                                 *
*  ATTRIBUTES  : AMODE 24, RMODE 24                                   *
*  RESIDES IN  : TELCABS.SETL.LOADLIB.EMERG                           *
*  SOURCE      : TELCABS.SETL.SRCLIB.EMERG(CABDATCV)                  *
*                                                                     *
*  EMERGENCY FIX LIBRARY                                              *
*    ASSEMBLED AND LINKED UNDER PROBLEM RECORD PR-9964 ON THE NIGHT   *
*    OF 1998-12-08 WHEN THE DECEMBER SETTLEMENT RUN ABENDED IN THE    *
*    AGEING TEST.  THE FIX WAS PLACED IN THE EMERGENCY LIBRARY SO     *
*    THE CYCLE COULD BE RESTARTED WITHOUT WAITING FOR A PROMOTION,    *
*    ON THE UNDERSTANDING THAT IT WOULD BE FOLDED BACK INTO THE       *
*    COMMON LIBRARY AT THE NEXT RELEASE.  THE SETTLEMENT JOBS WERE    *
*    GIVEN A STEPLIB WITH THIS LIBRARY IN FRONT OF THE COMMON ONE     *
*    FOR THE DURATION.  SEE OPERATIONS NOTE OPS-1998-441.             *
*                                                                     *
*  PURPOSE                                                            *
*    CONVERTS BETWEEN YYDDD AND GREGORIAN CCYYMMDD, ADDS DAYS WITH    *
*    YEAR ROLLOVER AND REPORTS LEAP YEARS.  THE SETTLEMENT AGEING     *
*    TEST AND THE CMDS EXCHANGE DATE BOTH DEPEND ON IT.               *
*                                                                     *
*  CENTURY RESOLUTION                                                 *
*    A TWO DIGIT YEAR OF 68 OR ABOVE IS TAKEN AS NINETEEN HUNDRED.    *
*    BELOW 68 IT IS TAKEN AS TWENTY HUNDRED.  THE VALUE WAS SET TO    *
*    MATCH THE INDUSTRY EXCHANGE SPECIFICATION, WHICH THE RECEIVING   *
*    RAOS APPLY TO THE DATES ON THE EXCHANGE HEADER.                  *
*                                                                     *
*  LINKAGE CONVENTION                                                 *
*    STANDARD OS LINKAGE.  R1 ADDRESSES A FIVE WORD PARAMETER LIST -  *
*      +0   A(FUNCTION CODE)  CL4  'JTOG' 'GTOJ' 'ADDD' 'LEAP'        *
*      +4   A(INPUT 1)                                                *
*      +8   A(INPUT 2)        SIGNED DAY COUNT PL4 FOR ADDD           *
*      +12  A(OUTPUT)                                                 *
*      +16  A(RETURN AREA)    F   0 OK, 4 INVALID DATE, 8 INVALID     *
*                                 FUNCTION                            *
*                                                                     *
*  REVISION HISTORY                                                   *
*    V1.00  1987-06-30  R.T.WHEELER   INITIAL                         *
*    V1.06  1995-10-11  J.M.CASTILLO  PIVOT INTRODUCED                *
*    V1.08  1998-12-08  D.OKONKWO     EMERGENCY - ADDD WOULD NOT      *
*                                     CROSS A YEAR END WHEN THE       *
*                                     SPAN EXCEEDED THE REMAINING     *
*                                     DAYS IN THE YEAR                *
*=====================================================================*
CABDATCV CSECT
CABDATCV AMODE 24
CABDATCV RMODE 24
         ENTRY CABDTCNV
         USING *,R15
         B     BEGIN
         DC    AL1(16)
         DC    CL16'CABDATCV V1.08E '
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
         CLC   0(4,R2),=CL4'ADDD'
         BE    DOADDD
         CLC   0(4,R2),=CL4'LEAP'
         BE    DOLEAP
         LA    R0,8
         ST    R0,0(,R6)
         B     RETURN
*
*---------------------------------------------------------------------*
*  SPLIT A YYDDD AND RESOLVE THE CENTURY AGAINST THE PIVOT.           *
*---------------------------------------------------------------------*
SPLITJUL DS    0H
         ZAP   JULWORK,0(3,R3)
         ZAP   DBLWORK,JULWORK
         CVB   R7,DBLWORK
         SR    R8,R8
         LR    R9,R7
         D     R8,=F'1000'
         ST    R9,YEARYY
         ST    R8,DAYDDD
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
*  LEAP YEAR TEST.  DIVISIBLE BY FOUR.                                *
*---------------------------------------------------------------------*
LEAPTEST DS    0H
         MVI   LEAPSW,C'N'
         L     R7,YEARCC
         SR    R8,R8
         LR    R9,R7
         D     R8,=F'4'
         LTR   R8,R8
         BNZ   LEAPXIT
         MVI   LEAPSW,C'Y'
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
         LA    R9,MONTAB
         B     JTOGWALK
JTOGLEAP LA    R9,MONTABL
JTOGWALK DS    0H
         SR    R7,R7
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
         D     R8,=F'100'
         ST    R8,DAYDD
         SR    R8,R8
         D     R8,=F'100'
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
         SR    R0,R0
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
*  ADD DAYS.  THE DAY OF YEAR IS STEPPED AND THE YEAR IS ROLLED       *
*  FORWARD ONE AT A TIME UNTIL THE RESULT FITS INSIDE THE YEAR.  A    *
*  NEGATIVE SPAN ROLLS BACKWARD THE SAME WAY.                         *
*---------------------------------------------------------------------*
DOADDD   DS    0H
         BAL   R10,SPLITJUL
         ZAP   DBLWORK,0(4,R4)
         CVB   R7,DBLWORK
         L     R8,DAYDDD
         AR    R8,R7
         ST    R8,DAYDDD
ADDDFWD  DS    0H
         BAL   R10,LEAPTEST
         LA    R0,365
         CLI   LEAPSW,C'Y'
         BNE   ADDDF2
         LA    R0,366
ADDDF2   L     R8,DAYDDD
         CR    R8,R0
         BNH   ADDDBACK
         SR    R8,R0
         ST    R8,DAYDDD
         L     R9,YEARCC
         LA    R9,1(,R9)
         ST    R9,YEARCC
         B     ADDDFWD
ADDDBACK DS    0H
         L     R8,DAYDDD
         LTR   R8,R8
         BP    ADDDBLD
         L     R9,YEARCC
         BCTR  R9,0
         ST    R9,YEARCC
         BAL   R10,LEAPTEST
         LA    R0,365
         CLI   LEAPSW,C'Y'
         BNE   ADDDB2
         LA    R0,366
ADDDB2   L     R8,DAYDDD
         AR    R8,R0
         ST    R8,DAYDDD
         B     ADDDBACK
ADDDBLD  DS    0H
         L     R9,YEARCC
         C     R9,=F'2000'
         BL    ADDD19
         S     R9,=F'2000'
         B     ADDDPUT
ADDD19   S     R9,=F'1900'
ADDDPUT  M     R8,=F'1000'
         A     R9,DAYDDD
         CVD   R9,DBLWORK
         ZAP   0(3,R5),DBLWORK
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
*
RETURN   DS    0H
         L     R13,4(,R13)
         L     R14,12(,R13)
         LM    R0,R12,20(R13)
         SR    R15,R15
         BR    R14
*
PIVOTYY  DC    X'68'                   CENTURY PIVOT, TWO DIGIT
PIVOTFW  DC    F'68'
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
LEAPSW   DS    CL1
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
