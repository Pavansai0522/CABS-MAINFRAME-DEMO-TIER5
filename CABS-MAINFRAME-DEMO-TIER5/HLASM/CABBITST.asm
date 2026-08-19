*=====================================================================*
*  CABBITST - USAGE STATUS FLAG BIT SERVICE                           *
*  APPLICATION : CABS                                                 *
*  ATTRIBUTES  : AMODE 24, RMODE 24, LINKED INTO                      *
*                TELCABS.COMMON.LOADLIB                               *
*  INVOKED BY  : STATIC CALL FROM THE INTAKE MODULES.  CABING01,      *
*                CABING05, CABING09 AND CABING11 ALL CALL IT.         *
*                                                                     *
*  PURPOSE                                                            *
*    THE EDIT STATUS BYTE ON THE USAGE RECORD IS A CHARACTER FIELD    *
*    IN THE COPYBOOK BUT IT IS USED AS A BIT MAP.  EIGHT INDEPENDENT  *
*    CONDITIONS ARE CARRIED IN ONE BYTE AND THE COBOL SIDE CANNOT     *
*    TEST OR SET THEM WITHOUT A TABLE OF SIXTY FOUR VALUES.  THIS     *
*    MODULE DOES IT WITH TM, OI, NI AND XI.                           *
*                                                                     *
*    BIT ASSIGNMENT, HIGH ORDER BIT FIRST -                           *
*      X'80'  RECORD FAILED THE OCN LOOKUP                            *
*      X'40'  RECORD FAILED THE BAN LOOKUP                            *
*      X'20'  JURISDICTION WAS DERIVED, NOT SUPPLIED                  *
*      X'10'  MINUTES WERE ADJUSTED BY THE CONSOLIDATION              *
*      X'08'  RECORD WAS RECYCLED FROM A PREVIOUS CYCLE               *
*      X'04'  RECORD CARRIES A MEET POINT SPLIT                       *
*      X'02'  DUPLICATE SEQUENCE NUMBER TOLERATED                     *
*      X'01'  RESERVED - SET BY THE 1996 CONVERSION AND NEVER         *
*             CLEARED, SO IT IS ON FOR EVERY RECORD LOADED BEFORE     *
*             THAT DATE AND OFF FOR EVERY RECORD SINCE                *
*                                                                     *
*    THE COPYBOOK 88 LEVELS TEST THE SAME BYTE AS IF IT HELD A        *
*    DISPLAY DIGIT.  A RECORD WITH MORE THAN ONE BIT ON WILL          *
*    THEREFORE SATISFY A DIFFERENT 88 LEVEL FROM THE ONE THE INTAKE   *
*    SET.  THAT BEHAVIOUR IS RELIED ON BY THE SUSPENSE ROUTING AND    *
*    MUST NOT BE ALTERED - SEE CABS-STD-014.                          *
*                                                                     *
*  LINKAGE CONVENTION                                                 *
*    STANDARD OS LINKAGE.  R1 ADDRESSES A FOUR WORD PARAMETER LIST -  *
*      +0   A(FUNCTION CODE)  CL4  'TEST' 'SETB' 'CLRB' 'FLIP'        *
*                                  'CNTB' 'DUMP'                      *
*      +4   A(FLAG BYTE)      XL1  UPDATED IN PLACE BY SETB/CLRB/FLIP *
*      +8   A(BIT MASK)       XL1  X'80' THROUGH X'01', OR A          *
*                                  COMBINATION                        *
*      +12  A(RETURN AREA)    XL8                                     *
*             +0  RESULT      F    TEST - 0 NO BIT ON, 4 SOME BITS    *
*                                  ON, 8 ALL MASK BITS ON             *
*                                  CNTB - NUMBER OF BITS ON           *
*             +4  FLAGS       CL4  PRINTABLE FORM OF THE BYTE,        *
*                                  SET BY DUMP ONLY                   *
*                                                                     *
*  REVISION HISTORY                                                   *
*    V1.00  1989-08-07  R.T.WHEELER   INITIAL                         *
*    V1.02  1993-03-16  D.OKONKWO     CNTB ADDED FOR THE CONTROL      *
*                                     REPORT                          *
*    V1.05  1996-11-21  J.M.CASTILLO  BIT X'01' RESERVED FOR THE      *
*                                     CONVERSION                      *
*    V1.08  2004-06-29  P.NAIR        DUMP FUNCTION FOR THE SUSPENSE  *
*                                     LISTING                         *
*    V1.09  2014-09-03  A.BUKOWSKI    REASSEMBLED FOR THE 31 BIT      *
*                                     LOAD LIBRARY - STILL AMODE 24   *
*=====================================================================*
CABBITST CSECT
CABBITST AMODE 24
CABBITST RMODE 24
         USING *,R15
         B     BEGIN
         DC    AL1(16)
         DC    CL16'CABBITST V1.09  '
         DROP  R15
*
BEGIN    STM   R14,R12,12(R13)
         LR    R12,R15
         USING CABBITST,R12
         LA    R11,SAVEAREA
         ST    R13,4(,R11)
         ST    R11,8(,R13)
         LR    R13,R11
         LR    R11,R1
         USING PARMLIST,R11
*
         L     R2,PFUNC
         L     R3,PFLAG                A(FLAG BYTE)
         L     R4,PMASK                A(MASK BYTE)
         L     R5,PRETRN
         XC    0(8,R5),0(R5)
         MVC   MASKHOLD,0(R4)
         MVC   FLAGHOLD,0(R3)
*
         CLC   0(4,R2),=CL4'TEST'
         BE    DOTEST
         CLC   0(4,R2),=CL4'SETB'
         BE    DOSETB
         CLC   0(4,R2),=CL4'CLRB'
         BE    DOCLRB
         CLC   0(4,R2),=CL4'FLIP'
         BE    DOFLIP
         CLC   0(4,R2),=CL4'CNTB'
         BE    DOCNTB
         CLC   0(4,R2),=CL4'DUMP'
         BE    DODUMP
         LA    R0,12
         ST    R0,0(,R5)
         B     RETURN
*
*---------------------------------------------------------------------*
*  TEST - THE CONDITION CODE AFTER TM DISTINGUISHES ALL THREE CASES.  *
*---------------------------------------------------------------------*
DOTEST   DS    0H
         TM    FLAGHOLD,X'FF'
         EX    R0,TMTEST
         BZ    TESTNONE
         BO    TESTALL
         LA    R0,4
         ST    R0,0(,R5)
         B     RETURN
TESTNONE XC    0(4,R5),0(R5)
         B     RETURN
TESTALL  LA    R0,8
         ST    R0,0(,R5)
         B     RETURN
*
*---------------------------------------------------------------------*
*  SET - TURN THE MASK BITS ON IN PLACE.                              *
*---------------------------------------------------------------------*
DOSETB   DS    0H
         IC    R6,MASKHOLD
         EX    R6,OISET
         MVC   0(1,R3),FLAGHOLD
         LA    R0,4
         ST    R0,0(,R5)
         B     RETURN
*
*---------------------------------------------------------------------*
*  CLEAR - TURN THE MASK BITS OFF.  THE COMPLEMENT OF THE MASK IS     *
*  BUILT WITH XI AGAINST ALL ONES.                                    *
*---------------------------------------------------------------------*
DOCLRB   DS    0H
         MVI   WORKBYTE,X'FF'
         MVC   COMPMASK,MASKHOLD
         XC    COMPMASK,WORKBYTE
         IC    R6,COMPMASK
         EX    R6,NICLEAR
         MVC   0(1,R3),FLAGHOLD
         LA    R0,4
         ST    R0,0(,R5)
         B     RETURN
*
*---------------------------------------------------------------------*
*  FLIP - INVERT THE MASK BITS.                                       *
*---------------------------------------------------------------------*
DOFLIP   DS    0H
         IC    R6,MASKHOLD
         EX    R6,XIFLIP
         MVC   0(1,R3),FLAGHOLD
         LA    R0,4
         ST    R0,0(,R5)
         B     RETURN
*
*---------------------------------------------------------------------*
*  COUNT - HOW MANY BITS ARE ON IN THE FLAG BYTE.  THE MASK IS        *
*  IGNORED.  WALKED ONE BIT AT A TIME RATHER THAN TABLE DRIVEN        *
*  BECAUSE THE ROUTINE IS ENTERED ONCE PER SUSPENDED RECORD, NOT      *
*  ONCE PER RECORD.                                                   *
*---------------------------------------------------------------------*
DOCNTB   DS    0H
         SR    R7,R7                   BIT COUNT
         LA    R8,8                    LOOP COUNT
         MVI   WORKBYTE,X'80'
CNTLOOP  DS    0H
         IC    R6,WORKBYTE
         EX    R6,TMCOUNT
         BZ    CNTNEXT
         LA    R7,1(,R7)
CNTNEXT  DS    0H
         IC    R6,WORKBYTE
         SRL   R6,1
         STC   R6,WORKBYTE
         BCT   R8,CNTLOOP
         ST    R7,0(,R5)
         B     RETURN
*
*---------------------------------------------------------------------*
*  DUMP - PRODUCE A FOUR BYTE PRINTABLE FORM.  THE FIRST TWO BYTES    *
*  ARE THE HEXADECIMAL VALUE, THE LAST TWO ARE THE COUNT OF BITS ON   *
*  IN DISPLAY FORM.                                                   *
*---------------------------------------------------------------------*
DODUMP   DS    0H
         UNPK  UNPKAREA(3),FLAGHOLD(2)
         TR    UNPKAREA(2),HEXTAB-C'0'
         MVC   4(2,R5),UNPKAREA
         BAL   R9,CNTINLIN
         CVD   R7,DBLWORK
         OI    DBLWORK+7,X'0F'
         UNPK  6(2,R5),DBLWORK+6(2)
         ST    R7,0(,R5)
         B     RETURN
*
CNTINLIN DS    0H
         SR    R7,R7
         LA    R8,8
         MVI   WORKBYTE,X'80'
CNTIL2   DS    0H
         IC    R6,WORKBYTE
         EX    R6,TMCOUNT
         BZ    CNTIL3
         LA    R7,1(,R7)
CNTIL3   DS    0H
         IC    R6,WORKBYTE
         SRL   R6,1
         STC   R6,WORKBYTE
         BCT   R8,CNTIL2
         BR    R9
*
RETURN   DS    0H
         L     R13,4(,R13)
         L     R14,12(,R13)
         LM    R0,R12,20(R13)
         SR    R15,R15
         BR    R14
*
*---------------------------------------------------------------------*
*  EXECUTED INSTRUCTIONS                                              *
*---------------------------------------------------------------------*
TMTEST   TM    FLAGHOLD,0
OISET    OI    FLAGHOLD,0
NICLEAR  NI    FLAGHOLD,0
XIFLIP   XI    FLAGHOLD,0
TMCOUNT  TM    FLAGHOLD,0
*
HEXTAB   DC    C'0123456789ABCDEF'
*
         LTORG
*
SAVEAREA DS    18F
DBLWORK  DS    D
FLAGHOLD DS    XL1
MASKHOLD DS    XL1
COMPMASK DS    XL1
WORKBYTE DS    XL1
UNPKAREA DS    CL4
*
PARMLIST DSECT
PFUNC    DS    A
PFLAG    DS    A
PMASK    DS    A
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
         END   CABBITST
