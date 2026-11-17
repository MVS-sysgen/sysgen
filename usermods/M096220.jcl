//M096220  JOB (SYSGEN),'M096220',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*********************************************************************
//* INSTALL USERMOD M096220 - FIX HARD WAIT DURING IPL IF THE MASTER  *
//* CATALOG IS ON A 3375/3380/3390                                    *
//*                                                                   *
//* FROM JAY MOSELEY'S INSTALLMVS V8 TAPE/USERMODS.HET                *
//*                                                                   *
//* MVS/CE: ZP60042 (STEP 10, JCL/ZP60041.JCL) REPLACES ALL OF        *
//* IEAVNP12, WHICH WOULD UNDO THIS ZAP. ITS IEAVNP12 SOURCE HAS THE  *
//* SAME FIX IN SUBROUTINE GETTRNM (LINES SEQUENCED M0962201-11) AND  *
//* ZP60042 IS SUP(M096220), SO THE FIX STAYS AFTER STEP 10.          *
//*                                                                   *
//* THE FIX: GETTRNM COMPUTED THE RBA OF THE TRUE NAME CI WITH THE    *
//* INDEX COMPONENT CI SIZE INSTEAD OF THE DATA COMPONENT ONE. THEY   *
//* ARE THE SAME ON 3350, BUT DIFFER WITH THE MASTER CATALOG ON       *
//* 3375/3380/3390: IEA326I LOCATE FAILED FOR SYS1.LINKLIB, WAIT 00A. *
//*********************************************************************
//*
//RECEIVE  EXEC SMPREC,WORK='SYSALLDA',COND=(0,NE)
//SMPPTFIN DD  DATA,DLM='??'
++ USERMOD(M096220)     /* REWORK(20090625) */             .
++ VER (Z038)
   FMID(EDM1102)
   PRE  (UZ72608)
 /*
   PROBLEM DESCRIPTION(S):
     M096220 -
       MsgIEA326I LOCATE FAILED FOR SYS1.LINKLIB and wait00A during
       NIP when master catalog is on D/T3375, 3380 or 3390.

       NIP LOCATE routine IEAVNP12 subroutine GETTRNM is called to
       read the true name record for the target of a LOCATE.  It
       searches the catalog index to find the correct pointer to
       the true name CI in the data component and builds the RBA
       of the target data CI.  When building the RBA, GETTRNM
       incorrectly calculates the value using the index component
       CI size instead of the data component CI size.  No problem
       results as long as index and data component have the same
       CI size, as with a catalog on 3350.  For 3375s, 3380s and
       3390s, VSAM selects different CI sizes for catalog index
       and data components, so the calculated value is wrong, an
       incorrect data record is read and the LOCATE fails.  When
       LOCATE fails for SYS1.LINKLIB, the result is msgIEA326I
       and wait00A.

       IEAVNP12 subroutine GETTRNM is changed to use the correct
       CI size when calcuating the true name CI RBA.

       While this usermod is not required for correct operation
       of IEAVNP12 with a master catalog on devices other than 3375,
       3380 or 3390, it will produce the correct results regardless
       of catalog device type.

   COMPONENT:  5752-SC1DE-EDM1102

   SPECIAL CONDITIONS:
     ACTION:  An IPL is required after installation of this user
       modification.

   COMMENTS:
     LAST CHANGE:  2009/06/25

     REWORK HISTORY:
       2009/06/25: MVS 3.8J DASD extensions 1.02

     CROSS REFERENCE-MODULE/MACRO NAMES TO USERMODS
       IEAVNP12  M096220

     CROSS REFERENCE-USERMODS TO MODULE/MACRO NAMES
       M096220   IEAVNP12

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:

     MODULES
       IEAVNP12

     LISTEND
 */.
++ ZAP      (IEAVNP12) DISTLIB(AOSC5   ).
NAME IEAVNP12
IDRDATA M096220
VER 000004 10                        DC    AL1(16)
VER 000005 C9C5C1E5D5D7F1F2          DC    C'IEAVNP12'
VER 00000D 4040F8F44BF1F3F2          DC    C'  84.132'
VER 000015 00                        DC    X'00'
VER 000016 90EC,D00C                 STM   @14,@12,12(@13)
VER 00001A 05C0                      BALR  @12,0
VER 0009C6 5E50,7004                 AL    @05,IXHBRBA(,@07)
VER 0009CA 5050,A008                 ST    @05,BUFCDDDD(,BUFCPTR) BUFCDDDD(,BUFC
VER 0009CE 5050,B100                 ST    @05,HLDRBA          SAVE HLDRBA
VER 0009F4 5890,B100                 L     @09,HLDRBA          GET HLDRBA
VER 0009F8 5090,A008                 ST    @09,BUFCDDDD(,BUFCPTR) BUFCDDDD(,BUFC
VER 000E42 4040404040404040          DC    38C' '              PATCH AREA
VER 000E4A 4040404040404040
VER 000E52 4040404040404040
VER 000E5A 4040404040404040
VER 000E62 404040404040
REP 0009C6 45E0,CE26                 BAL   @14,PATCH1          LINK TO PATCH ARE
REP 0009CE 50F0,B100                 ST    @15,HLDRBA          SAVE IXHBRBA AS H
REP 0009F4 45E0,CE2E                 BAL   @14,PATCH2          LINK TO PATCH ARE
REP 0009F8 5010,A008                 ST    @01,BUFCDDDD(,BUFCPTR) BUFCDDDD(,BUFC
REP 000E42 58F0,7004        PATCH1   L     @15,IXHBRBA(,@07)   GET IXHBRBA
REP 000E46 1E5F                      ALR   @05,@15             ADD IHXBRBA
REP 000E48 07FE                      BR    @14                 RETURN TO MAINLIN
REP 000E4A 58F0,7014        PATCH2   L     @15,AMBDSB(,@07)    POINT TO DATA AMD
REP 000E4E 5810,B110                 L     @01,IX4             GET IX4
REP 000E52 5C00,F014                 M     @00,AMDCINV(,@15)   TIMES CI SIZE
REP 000E56 5E10,B100                 AL    @01,HLDRBA          PLUS HLDRBA
REP 000E5A 07FE                      BR    @14                 RETURN TO MAINLIN
??
//SMPCNTL  DD  *
  RECEIVE
          SELECT(M096220)
          .
/*
//*
//APPLYCK  EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(M096220)
        CHECK
        .
/*
//*
//APPLY    EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(M096220)
        DIS(WRITE)
        .
/*
//
