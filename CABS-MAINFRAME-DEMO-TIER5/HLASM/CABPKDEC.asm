*=====================================================================*
*  CABPKDEC - PACKED DECIMAL ARITHMETIC SERVICE                       *
*  APPLICATION : CABS / SETL                                          *
*  ATTRIBUTES  : REENTRANT, AMODE 24, RMODE 24, LINKED NORENT INTO    *
*                TELCABS.COMMON.LOADLIB                               *
*  INVOKED BY  : DYNAMIC CALL FROM THE RATING AND SETTLEMENT          *
*                MODULES.  THE CALLER LOADS THE MODULE NAME INTO A    *
*                WORKING STORAGE FIELD AND ISSUES CALL BY IDENTIFIER, *
*                SO THE BINDER DOES NOT RESOLVE IT.                   *
*                                                                     *
*  PURPOSE                                                            *
*    ALL CABS MONEY FIELDS ARE PACKED DECIMAL WITH FIVE DECIMAL       *
*    PLACES.  THE COBOL COMPILER GENERATES ITS OWN ARITHMETIC FOR     *
*    THOSE FIELDS BUT WILL NOT PRODUCE AN INTERMEDIATE WIDER THAN     *
*    THE RECEIVING FIELD, SO A RATE TIMES A MINUTE COUNT CAN LOSE     *
*    SIGNIFICANCE BEFORE IT IS ROUNDED.  THIS MODULE PERFORMS THE     *
*    MULTIPLY AND THE DIVIDE IN A SIXTEEN BYTE WORK AREA AND ROUNDS   *
*    ONCE, AT THE END.                                                *
*                                                                     *
*  LINKAGE CONVENTION                                                 *
*    STANDARD OS LINKAGE.  ON ENTRY -                                 *
*      R1  = A(PARAMETER LIST)                                        *
*      R13 = A(CALLER SAVE AREA)                                      *
*      R14 = RETURN ADDRESS                                           *
*      R15 = ENTRY POINT ADDRESS                                      *
*                                                                     *
*    THE PARAMETER LIST IS FIVE FULLWORDS.  THE HIGH ORDER BIT OF     *
*    THE LAST ADDRESS IS SET BY THE CALLER TO MARK THE END OF THE     *
*    LIST.  THIS MODULE DOES NOT TEST THAT BIT AND WILL ADDRESS ALL   *
*    FIVE WORDS WHETHER OR NOT IT IS SET.                             *
*                                                                     *
*      +0   A(FUNCTION CODE)   CL4   'ADD ' 'SUB ' 'MUL ' 'DIV '      *
*                                    'RND ' 'ZAP ' 'CMP '             *
*      +4   A(OPERAND 1)       PL8   S9(10)V9(05) COMP-3              *
*      +8   A(OPERAND 2)       PL8   S9(10)V9(05) COMP-3              *
*      +12  A(RESULT)          PL8   S9(10)V9(05) COMP-3              *
*      +16  A(RETURN AREA)     XL8                                    *
*             +0  RETURN CODE  F     0 OK, 4 OVERFLOW, 8 DIVIDE BY    *
*                                    ZERO, 12 BAD FUNCTION CODE       *
*             +4  CONDITION    F     -1 LOW, 0 EQUAL, +1 HIGH, SET    *
*                                    BY THE CMP FUNCTION ONLY         *
*                                                                     *
*    THE RESULT AREA IS ALWAYS SET.  ON ANY NON ZERO RETURN CODE IT   *
*    IS SET TO PACKED ZERO, NOT LEFT ALONE.                           *
*                                                                     *
*  ROUNDING                                                           *
*    THE RESULT OF MUL AND DIV IS CARRIED TO TEN DECIMAL PLACES IN    *
*    THE WORK AREA AND THEN BROUGHT BACK TO FIVE.  THE FIFTH PLACE    *
*    IS ADJUSTED BY ADDING FIVE UNITS OF THE SIXTH PLACE TO THE       *
*    MAGNITUDE BEFORE THE SHIFT, WHICH CARRIES A HALF AWAY FROM       *
*    ZERO IN BOTH DIRECTIONS.  THE SIGN IS PRESERVED ACROSS THE       *
*    ADJUSTMENT BY WORKING ON THE ABSOLUTE VALUE AND RESTORING THE    *
*    SIGN WITH ZAP.  THIS IS THE HOUSE CONVENTION FOR FIVE PLACE      *
*    ARITHMETIC AND IS DOCUMENTED IN CABS-STD-019.                    *
*                                                                     *
*  REVISION HISTORY                                                   *
*    V1.00  1988-04-19  R.T.WHEELER   INITIAL                         *
*    V1.03  1991-09-25  D.OKONKWO     RND FUNCTION SPLIT OUT SO THE   *
*                                     RATING MODULES CAN ROUND        *
*                                     WITHOUT AN ARITHMETIC OPERATION *
*    V1.07  1996-02-11  J.M.CASTILLO  DIVIDE BY ZERO RETURNS 8        *
*                                     INSTEAD OF ABENDING 0CB         *
*    V2.00  2002-05-30  P.NAIR        WORK AREA WIDENED TO 16 BYTES   *
*    V2.02  2010-01-14  A.BUKOWSKI    CMP FUNCTION ADDED FOR THE      *
*                                     BAND SELECTION LOGIC            *
*    V2.03  2017-07-06  M.HAAS        REASSEMBLED - NO SOURCE CHANGE  *
*=====================================================================*
CABPKDEC CSECT
CABPKDEC AMODE 24
CABPKDEC RMODE 24
         USING *,R15
         B     BEGIN
         DC    AL1(16)
         DC    CL16'CABPKDEC V2.03  '
         DROP  R15
*
BEGIN    STM   R14,R12,12(R13)         SAVE CALLER REGISTERS
         LR    R12,R15                 ESTABLISH BASE
         USING CABPKDEC,R12
         LR    R11,R1                  HOLD PARAMETER LIST
         USING PARMLIST,R11
*
*        OBTAIN A WORK AREA.  THE MODULE IS ENTERED FROM MORE THAN
*        ONE TASK IN THE ONLINE REGION SO NOTHING IS HELD IN THE
*        CSECT ITSELF.
*
         GETMAIN R,LV=WORKLEN
         LR    R10,R1
         USING WORKAREA,R10
         ST    R13,WSAVE+4             CHAIN SAVE AREAS
         LA    R14,WSAVE
         ST    R14,8(,R13)
         LR    R13,R14
*
*        ADDRESS THE OPERANDS
*
         L     R2,PFUNC                A(FUNCTION CODE)
         L     R3,POPND1               A(OPERAND 1)
         L     R4,POPND2               A(OPERAND 2)
         L     R5,PRESLT               A(RESULT)
         L     R6,PRETRN               A(RETURN AREA)
         XC    0(8,R6),0(R6)           RETURN CODE AND CONDITION ZERO
         ZAP   RESWORK,=P'0'
*
*        LOAD THE OPERANDS INTO THE SIXTEEN BYTE WORK FIELDS.  BOTH
*        ARE BROUGHT IN AS FIVE PLACE VALUES.
*
         ZAP   OPWORK1,0(8,R3)
         ZAP   OPWORK2,0(8,R4)
*
*        DISPATCH ON THE FUNCTION CODE
*
         CLC   0(4,R2),=CL4'ADD '
         BE    DOADD
         CLC   0(4,R2),=CL4'SUB '
         BE    DOSUB
         CLC   0(4,R2),=CL4'MUL '
         BE    DOMUL
         CLC   0(4,R2),=CL4'DIV '
         BE    DODIV
         CLC   0(4,R2),=CL4'RND '
         BE    DORND
         CLC   0(4,R2),=CL4'ZAP '
         BE    DOZAP
         CLC   0(4,R2),=CL4'CMP '
         BE    DOCMP
         LA    R0,12                   UNRECOGNISED FUNCTION
         ST    R0,0(,R6)
         B     SETZERO
*
*---------------------------------------------------------------------*
*  ADD - BOTH OPERANDS ARE ALREADY AT FIVE PLACES.  NO SHIFT NEEDED.  *
*---------------------------------------------------------------------*
DOADD    ZAP   RESWORK,OPWORK1
         AP    RESWORK,OPWORK2
         BO    OVERFLOW
         B     STORERES
*
*---------------------------------------------------------------------*
*  SUBTRACT                                                           *
*---------------------------------------------------------------------*
DOSUB    ZAP   RESWORK,OPWORK1
         SP    RESWORK,OPWORK2
         BO    OVERFLOW
         B     STORERES
*
*---------------------------------------------------------------------*
*  MULTIPLY - FIVE PLACES TIMES FIVE PLACES GIVES TEN.  THE PRODUCT   *
*  IS BUILT IN THE SIXTEEN BYTE WORK AREA AND THEN BROUGHT BACK TO    *
*  FIVE BY THE ROUNDING ROUTINE.                                      *
*---------------------------------------------------------------------*
DOMUL    ZAP   MULWORK,OPWORK1
         MP    MULWORK,OPWORK2+2       MULTIPLIER MUST BE SHORT
         BO    OVERFLOW
         MVC   SHIFTCNT,=F'5'          TEN PLACES BACK TO FIVE
         BAL   R9,ROUNDIT
         B     STORERES
*
*---------------------------------------------------------------------*
*  DIVIDE - THE DIVIDEND IS SCALED UP BY TEN TO THE FIFTH SO THE      *
*  QUOTIENT COMES OUT AT FIVE PLACES.  A FURTHER FACTOR OF TEN IS     *
*  APPLIED SO THERE IS A SIXTH PLACE FOR THE ROUNDING ROUTINE TO      *
*  WORK ON.                                                          *
*---------------------------------------------------------------------*
DODIV    CP    OPWORK2,=P'0'
         BE    DIVZERO
         ZAP   MULWORK,OPWORK1
         MP    MULWORK,=P'1000000'     SCALE BY TEN TO THE SIXTH
         BO    OVERFLOW
         DP    MULWORK,OPWORK2+2
         ZAP   MULWORK,MULWORK(10)     QUOTIENT ONLY, DISCARD REMAINDER
         MVC   SHIFTCNT,=F'1'          SIX PLACES BACK TO FIVE
         BAL   R9,ROUNDIT
         B     STORERES
*
*---------------------------------------------------------------------*
*  ROUND ONLY - THE CALLER SUPPLIES A VALUE ALREADY CARRYING MORE     *
*  PLACES THAN IT WANTS AND THE NUMBER OF PLACES TO REMOVE IN         *
*  OPERAND 2.                                                        *
*---------------------------------------------------------------------*
DORND    ZAP   MULWORK,OPWORK1
         CVB   R7,DBLWORK
         ZAP   DBLWORK,OPWORK2
         CVB   R7,DBLWORK
         ST    R7,SHIFTCNT
         LTR   R7,R7
         BNP   STORERES                NOTHING TO REMOVE
         C     R7,=F'10'
         BH    BADSHIFT
         BAL   R9,ROUNDIT
         B     STORERES
*
*---------------------------------------------------------------------*
*  ZAP - MOVE WITH SIGN, NO ARITHMETIC.  USED WHERE THE CALLER WANTS  *
*  A CLEAN PACKED FIELD FROM A REDEFINED AREA.                        *
*---------------------------------------------------------------------*
DOZAP    ZAP   RESWORK,OPWORK1
         B     STORERES
*
*---------------------------------------------------------------------*
*  COMPARE - SETS THE CONDITION WORD IN THE RETURN AREA.  THE RESULT  *
*  FIELD IS SET TO THE DIFFERENCE SO THE CALLER CAN USE IT WITHOUT A  *
*  SECOND ENTRY.                                                     *
*---------------------------------------------------------------------*
DOCMP    ZAP   RESWORK,OPWORK1
         SP    RESWORK,OPWORK2
         CP    OPWORK1,OPWORK2
         BL    CMPLOW
         BE    CMPEQ
         LA    R0,1
         ST    R0,4(,R6)
         B     STORERES
CMPLOW   LA    R0,1
         LNR   R0,R0
         ST    R0,4(,R6)
         B     STORERES
CMPEQ    XC    4(4,R6),4(R6)
         B     STORERES
*
*---------------------------------------------------------------------*
*  ROUNDIT - REMOVE SHIFTCNT DECIMAL PLACES FROM MULWORK AND LEAVE    *
*  THE RESULT IN RESWORK.                                             *
*                                                                     *
*  THE ADJUSTMENT IS APPLIED TO THE MAGNITUDE.  THE SIGN IS TAKEN     *
*  OFF FIRST WITH A ZAP TO SIGNHOLD, THE MAGNITUDE IS ADJUSTED AND    *
*  SHIFTED, AND THE SIGN IS PUT BACK.  A NEGATIVE VALUE THEREFORE     *
*  ROUNDS AWAY FROM ZERO IN THE SAME WAY A POSITIVE ONE DOES.        *
*---------------------------------------------------------------------*
ROUNDIT  DS    0H
         ZAP   SIGNHOLD,=P'1'
         CP    MULWORK,=P'0'
         BNL   ROUNDPOS
         ZAP   SIGNHOLD,=P'-1'
         MP    MULWORK,=P'-1'
ROUNDPOS DS    0H
         L     R7,SHIFTCNT
         SLL   R7,2                    FOUR BYTES PER TABLE ENTRY
         LA    R8,HALFTAB
         AR    R8,R7
         L     R7,SHIFTCNT
         SLL   R7,3                    EIGHT BYTES PER ADJUST ENTRY
         LA    R8,ADJTAB
         AR    R8,R7
         AP    MULWORK,0(8,R8)         ADD THE HALF UNIT
         BO    OVERFLOW
         L     R7,SHIFTCNT
         SLL   R7,3
         LA    R8,DIVTAB
         AR    R8,R7
         DP    MULWORK,0(8,R8)
         ZAP   RESWORK,MULWORK(8)      QUOTIENT ONLY
         CP    SIGNHOLD,=P'0'
         BNL   ROUNDXIT
         MP    RESWORK,=P'-1'
ROUNDXIT BR    R9
*
*---------------------------------------------------------------------*
*  RESULT AND ERROR EXITS                                             *
*---------------------------------------------------------------------*
STORERES ZAP   0(8,R5),RESWORK
         B     RETURN
*
OVERFLOW LA    R0,4
         ST    R0,0(,R6)
         B     SETZERO
*
DIVZERO  LA    R0,8
         ST    R0,0(,R6)
         B     SETZERO
*
BADSHIFT LA    R0,12
         ST    R0,0(,R6)
         B     SETZERO
*
SETZERO  ZAP   0(8,R5),=P'0'
         B     RETURN
*
RETURN   DS    0H
         L     R13,WSAVE+4             UNCHAIN
         LR    R1,R10
         FREEMAIN R,LV=WORKLEN,A=(1)
         L     R14,12(,R13)
         LM    R0,R12,20(R13)
         SR    R15,R15                 RC IS IN THE PARAMETER AREA
         BR    R14
*
*---------------------------------------------------------------------*
*  CONSTANTS                                                          *
*                                                                     *
*  ADJTAB AND DIVTAB ARE INDEXED BY THE NUMBER OF PLACES BEING        *
*  REMOVED.  ENTRY ZERO IS NEVER USED BECAUSE A SHIFT COUNT OF ZERO   *
*  RETURNS BEFORE THE ROUTINE IS ENTERED.                             *
*---------------------------------------------------------------------*
         DS    0D
ADJTAB   DC    PL8'0'                  0 PLACES
         DC    PL8'5'                  1 PLACE
         DC    PL8'50'                 2 PLACES
         DC    PL8'500'                3 PLACES
         DC    PL8'5000'               4 PLACES
         DC    PL8'50000'              5 PLACES
         DC    PL8'500000'             6 PLACES
         DC    PL8'5000000'            7 PLACES
         DC    PL8'50000000'           8 PLACES
         DC    PL8'500000000'          9 PLACES
         DC    PL8'5000000000'         10 PLACES
*
         DS    0D
DIVTAB   DC    PL8'1'
         DC    PL8'10'
         DC    PL8'100'
         DC    PL8'1000'
         DC    PL8'10000'
         DC    PL8'100000'
         DC    PL8'1000000'
         DC    PL8'10000000'
         DC    PL8'100000000'
         DC    PL8'1000000000'
         DC    PL8'10000000000'
*
         DS    0D
HALFTAB  DC    F'0,5,50,500,5000,50000,500000,5000000'
*
         LTORG
*
*---------------------------------------------------------------------*
*  PARAMETER LIST MAPPING                                             *
*---------------------------------------------------------------------*
PARMLIST DSECT
PFUNC    DS    A                       A(FUNCTION CODE CL4)
POPND1   DS    A                       A(OPERAND 1  PL8)
POPND2   DS    A                       A(OPERAND 2  PL8)
PRESLT   DS    A                       A(RESULT     PL8)
PRETRN   DS    A                       A(RETURN AREA XL8)
*
*---------------------------------------------------------------------*
*  DYNAMIC WORK AREA                                                  *
*---------------------------------------------------------------------*
WORKAREA DSECT
WSAVE    DS    18F
DBLWORK  DS    D
OPWORK1  DS    PL8
OPWORK2  DS    PL8
RESWORK  DS    PL8
SIGNHOLD DS    PL2
         DS    0D
MULWORK  DS    PL16
SHIFTCNT DS    F
WORKLEN  EQU   *-WORKAREA
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
         END   CABPKDEC
