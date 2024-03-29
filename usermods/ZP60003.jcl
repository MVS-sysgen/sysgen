//ZP60003  JOB (SYSGEN),'J08 M18: ZP60003',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*  ZAP TO ALLOW IFOX00 ACCEPT BLANK RECORDS AS VALID INPUT.
//*
//RECEIVE EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN DD  *
++USERMOD(ZP60003)       /* XF ASSEMBLER */  .
++VER(Z038) FMID(EAS1102) PRE(UZ32460)
 /*
   PROBLEM DESCRIPTION:
     THE XF ASSEMBLER CANNOT ACCEPT BLANK INPUT SOURCE RECORDS.
       MUCH ASSEMBLER CODE WRITTEN FOR THE HIGH-LEVEL ASSEMBLER
       WHICH WOULD OTHERWISE BE FULLY PROCESSABLE BY PUBLICLY
       AVAILABLE ASSEMBLERS CANNOT BE PROCESSED BECAUSE OF
       CHANGES TO RULES FOR ALLOWABLE INPUT.  ONE SUCH RULE IS
       THE REQUIREMENT FOR THE "SPACE" ASSEMBLER INSTRUCTION
       WHENEVER A BLANK LINE IS TO BE PRODUCED IN THE OUTPUT
       LISTING, WHEREAS THE HIGH-LEVEL ASSEMBLER (ASMA90) CAN
       ALSO ACCEPT BLANK INPUT RECORDS.

       THIS USERMOD UPDATES THE XF ASSEMBLER (IFOX00) TO
       ALLOW RECORDS WITH BLANKS IN THE FIRST 72 COLUMNS AS
       VALID INPUT.  NEW LOGIC ADDS THE INTERNAL TEXT FOR THE
       "SPACE" INSTRUCTION IN COLUMN 10 BEFORE THE INPUT RECORD
       IS PARSED WHENEVER A RECORD IS FOUND TO START WITH 72
       BLANKS.

   SPECIAL CONDITIONS:
     NONE.

   COMMENTS:
     PRYCROFT SIX P/L PUBLIC DOMAIN USERMOD FOR MVS 3.8 NO. 3.

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
     MODULES:
       IF0X0F
 */.
++ZAP(IFOX0F) DISTLIB(AOS03).
 NAME IFOX0F00
 EXPAND IFOX0F00(35)
 IDRDATA ZP60003
VER 0076 12B1                  LTR   R11,R1
VER 0078 47F0,C02A             B     RETURN
VER 0100 12BB                  LTR   R11,R11
VER 0102 47F0,C02A             B     RETURN
REP 0078 47F0,C34E             B     NEWCODE
REP 0102 47F0,C34E             B     NEWCODE
REP 0351 1C190A0C0E   SPACE    DC    C'SPACE'
REP 0356 952F,B000    NEWCODE  CLI   0(R11),C' '
REP 035A 4770,C366             BNE   GOBACK
REP 035E D546,B001,B000        CLC   1(71,R11),0(R11)
REP 0364 4770,C366             BNE   GOBACK
REP 0368 D204,B009,C349        MVC   9(5,R11),SPACE
REP 036E 12BB         GOBACK   LTR   R11,R11
REP 0370 47F0,C02A             B     RETURN
/*
//SMPCNTL  DD  *
  RECEIVE
          SELECT(ZP60003)
          .
/*
//*
//APPLY   EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60003)
        CHECK
        .
/*
//*
//APPLYCK EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'

//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60003)
        DIS(WRITE)
        .
/*
//
