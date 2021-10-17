//ZP60007  JOB (SYSGEN),'J03 M22: ZP60007',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*  ADD CONFTXT PARAMETER SUPPORT TO TSO/VTAM INITIALIZATION.
//*
//STEP1   EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
++USERMOD(ZP60007)           /* ADD CONFTXT SUPPORT TO TSO */  .
++VER(Z038) FMID(ETV0108) PRE(UZ28155,UZ28257,UZ33846)
 /*
   PROBLEM DESCRIPTION:
     TSO/VTAM 3270 DATA STREAMS CANNOT BE TRACED BY GTF.
       WHEN ATTEMPTING TO DIAGNOSE PROBLEMS WITH TSO APPLICATIONS
       IT IS A USEFUL TECHNIQUE TO TRACE THE VTAM DATA TRAFFIC
       TO AND FROM THE TSO TERMINAL.  HOWEVER, TSO/VTAM SPECIFIES
       PROC=CONFTXT IN THE VTAM NIB WHICH INSTRUCTS VTAM TO TREAT
       TERMINAL TRAFFIC AS CONFIDENTIAL TEXT WHICH SHOULD NOT BE
       REPORTED BY A TRACE.  IBM HAS ADDED THE CONFTXT PARMLIB
       PARAMETER TO LATER VERSIONS OF VTAM SO THAT AN INSTALLATION
       CAN CONTROL THIS SETTING, BUT THIS FEATURE IS NOT SUPPORTED
       BY THE FREELY AVAILABLE VERSION OF TSO/VTAM.

       THIS USERMOD ADDS SUPPORT TO CORRECTLY PROCESS THE CONFTXT
       PARAMETER IN THE PARMLIB FILE OF THE TSO STARTED TASK,
       WHICH IS USUALLY A TSOKEY__ MEMBER OF SYS1.PARMLIB.
       FURTHER, TSO APPLICATION NIB INITIALISATION WILL ALTER THE
       NIB CONTENTS ACCORDING TO THE PARAMETER SETTING.  THE
       TCAST CONTROL BLOCK DSECT MACRO IS UPDATED TO REFLECT WHERE
       THIS SETTING IS STORED.

   SPECIAL CONDITIONS:
     ACTION:
       A "CLPA" MUST BE PERFORMED AT IPL TIME FOR THIS SYSMOD TO
       BECOME ACTIVE.
     DOC:
       CONFTXT=YES OR CONFTXT=NO CAN NOW BE SPECIFIED IN THE
       TSOKEY__ MEMBER OF SYS1.PARMLIB.  CONFTXT=YES IS THE DEFAULT.
       CONFTXT=YES WILL PREVENT THE TRACING OF TERMINAL DATA STREAMS.
       CONFTXT=NO WILL ALLOW THE TRACING OF TERMINAL DATA STREAMS.

   COMMENTS:
     PRYCROFT SIX P/L PUBLIC DOMAIN USERMOD FOR MVS 3.8 NUMBER 7.

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
     MODULES:
       IKTCAS54
       IKTXINIT
     MACROS:
       IKTTCAST
 */.
++MOD(IKTCAS54) DISTLIB(AOST3).
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(NEW,PASS),UNIT=SYSALLDA,
//             SPACE=(CYL,3),
//             DCB=(DSORG=PS,RECFM=FB,LRECL=80,BLKSIZE=4080)
//SYSIN    DD  DUMMY
//*
//STEP2   EXEC PGM=IFOX00,PARM='OBJECT,NODECK,NOTERM,XREF(SHORT)'
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT2   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT3   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.SMPMTS,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//SYSGO    DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  *
         TITLE '*** IKTCAS54 - PARAMETER PROCESSOR SCANNER'
*
*  MODIFIED BY GREG PRICE 30TH NOVEMBER 2002 FOR USERMOD ZP60007
*           TO SUPPORT THE CONFTXT PARMLIB SETTING
*
IKTCAS54 CSECT ,                                                   0001
@MAINENT DS    0H                                                  0001
         USING *,@15                                               0001
         B     @PROLOG                                             0001
         DC    AL1(16)                                             0001
         DC    C'IKTCAS54  79.255'                                 0001
         DROP  @15
@PROLOG  ST    @14,12(,@13)                                        0001
         STM   @00,@12,20(@13)                                     0001
         BALR  @12,0                                               0001
@PSTART  LA    @08,4095(,@12)                                      0001
         USING @PSTART,@12                                         0001
         USING @PSTART+4095,@08                                    0001
         L     @00,@SIZDATD                                        0001
         GETMAIN  R,LV=(0)
         LR    @10,@01                                             0001
         USING @DATD,@10                                           0001
         ST    @13,@SA00001+4                                      0001
         LM    @00,@01,20(@13)                                     0001
         ST    @10,8(,@13)                                         0001
         LR    @13,@10                                             0001
         EJECT
*                                                                  0119
*/*  *****************************************************************/
*/*                                                                  */
*/*    THIS PROGRAM IS ORGANIZED AS FOLLOWS:                         */
*/*                                                                  */
*/*        A - HOUSEKEEPING                                          */
*/*                                                                  */
*/*        B - MAINLINE, WHICH CONSISTS OF THE FOLLOWING:            */
*/*            1)  IF DATA IS IN A CIB, MOVE IT INTO A WORK BUFFER   */
*/*                AND RELEASE THE CIB WITH THE QEDIT MACRO          */
*/*            2)  VALIDITY CHECKING OF INPUT OPTIONS.  IF ANY       */
*/*                INCONSISTENCIES ARE FOUND, TERMINATE WITH A RETURN*/
*/*                CODE OF 4.                                        */
*/*            3)  SCAN PARAMETERS, FROM EITHER A DATA OR WORK       */
*/*                BUFFER.                                           */
*/*            4)  IF PARAMETERS FROM A PARMLIB ARE DESIRED, READ IN */
*/*                THE DESIRED MEMBER AND PROCESS ITS PARAMETERS.    */
*/*            5)  IF NECESSARY, OVERLAY MEMBER PARAMETERS WITH      */
*/*                BUFFER PARAMETERS.                                */
*/*            6)  IF REQUESTED, UPDATE TCAS TABLE                   */
*/*            7)  IF REQUESTED, WRITE OUT PARAMETERS                */
*/*                                                                  */
*/*        C - CLEANUP AND RETURN                                    */
*/*                                                                  */
*/*  *****************************************************************/
*                                                                  0119
*   SCANERR='0'B;                   /* INITIALIZE SW                 */
*   GOSW1='1'B;                     /* INITIALIZE SW                 */
         OI    GOSW1,B'00100000'                                   0120
         NI    SCANERR,B'01111111'                                 0120
*   PRNTPTR=ADDR(PRNTDAT5);         /* 1ST CHAR IN PRINT LINE        */
         LA    PRNTPTR,PRNTDAT5                                    0121
*   PRNTEND=PRNTPTR+67;             /* LAST CHAR IN PRINT LINE       */
         LA    @15,67                                              0122
         ALR   @15,PRNTPTR                                         0122
         ST    @15,PRNTEND                                         0122
*   SYNADSW1='N';                   /* INITIALIZE                    */
         MVI   SYNADSW1,C'N'                                       0123
*   SYNADSW2='N';                   /* ERROR                         */
         MVI   SYNADSW2,C'N'                                       0124
*   DCBABSW1='N';                   /* HANDLING                      */
         MVI   DCBABSW1,C'N'                                       0125
*   DCBABSW2='N';                   /* SWITCHES                      */
         MVI   DCBABSW2,C'N'                                       0126
*   CALL CHKOPTNS;                  /* CHECK INPUT OPTIONS, ETC      */
         BAL   @14,CHKOPTNS                                        0127
*   IF MLPMINV='1'B THEN            /* ERRORS                        */
         TM    MLPMINV(MLPMPTR),B'01000000'                        0128
         BNO   @RF00128                                            0128
*     RTNCODE=4;                    /* YES - NON-ZERO RTN CODE       */
         MVC   RTNCODE(2),@CH00063                                 0129
*   ELSE                                                           0130
*     DO;                           /* NO - CONTINUE ON WITH         */
         B     @RC00128                                            0130
@RF00128 DS    0H                                                  0131
*       RTNCODE=0;                  /* ZERO RETURN CODE              */
         SLR   @02,@02                                             0131
         STH   @02,RTNCODE                                         0131
*       CALL SCANPARM;              /* SCAN BUFFER PARAMETERS        */
         BAL   @14,SCANPARM                                        0132
*       IF MLPMINV='0'B THEN        /* ALL THE PARMS OK              */
         TM    MLPMINV(MLPMPTR),B'01000000'                        0133
         BNZ   @RF00133                                            0133
*         DO;                       /* YES - CONTINUE ON             */
*           CALL READMBR;           /* READ AND PROCESS PARMLIB      */
         BAL   @14,READMBR                                         0135
*           CALL OVERLAY;           /* MERGE PARMS                   */
         BAL   @14,OVERLAY                                         0136
*           CALL TCASUPDT;          /* UPDATE TCAS TABLE PARMS       */
         BAL   @14,TCASUPDT                                        0137
*           CALL WRITEOUT;          /* PRINT TCAS TABLE PARMS        */
         BAL   @14,WRITEOUT                                        0138
*         END;                                                     0139
*       ELSE                        /* NO - ERROR FOUND              */
*         IF PRINTSW='1'B THEN      /* DOING A PRINTOUT              */
         B     @RC00133                                            0140
@RF00133 TM    PRINTSW,B'00001000'                                 0140
         BNO   @RF00140                                            0140
*           DO;                     /* YES                           */
*             GEN(CLOSE PRINTOUT)   /* CLOSE PRINTOUT                */
*             REFS(R0,R1,R14,R15,PRINTOUT);                        0142
         CLOSE PRINTOUT
*           END;                                                   0143
*     END;                                                         0144
@RF00140 DS    0H                                                  0144
@RC00133 DS    0H                                                  0145
*   RESPECIFY                                                      0145
*    (R15) RESTRICTED;                                             0145
@RC00128 DS    0H                                                  0146
*   R15=RTNCODE;                    /* INSERT RETURN CODE            */
         LH    R15,RTNCODE                                         0146
*   RESPECIFY                                                      0147
*    (R15) UNRESTRICTED;                                           0147
*   RETURN;                         /* FINISHED                      */
@EL00001 L     @13,4(,@13)                                         0148
@EF00001 L     @00,@SIZDATD                                        0148
         LR    @01,@10                                             0148
         FREEMAIN R,LV=(0),A=(1)
@ER00001 L     @14,12(,@13)                                        0148
         LM    @00,@12,20(@13)                                     0148
         BR    @14                                                 0148
         EJECT
*CHKOPTNS:                                                         0149
*   PROC OPTIONS(SAVE(14));                                        0149
CHKOPTNS ST    @14,@SA00002                                        0149
*                                                                  0150
*/*  *****************************************************************/
*/*                                                                  */
*/*    THIS SUBROUTINE DOES THREE THINGS:                            */
*/*                                                                  */
*/*    -   IF PARAMETERS ARE IN A CIB BUFFER, MOVE THEM INTO A WORK  */
*/*        BUFFER AND FREE THE CIB BUFFER                            */
*/*    -   MAKE SURE THE INPUT OPTIONS ARE CONSISTENT.               */
*/*    -   IF A PRINTOUT WANTED, OPEN THE PRINTOUT DCB               */
*/*                                                                  */
*/*  *****************************************************************/
*                                                                  0150
*   CIBUFFER='';                    /* CLEAR WORK BUFFER             */
         MVI   CIBUFFER,C' '                                       0150
         MVC   CIBUFFER+1(47),CIBUFFER                             0150
*   IF MLPMCIB='0'B MLPMSCA=0 THEN                                 0151
         TM    MLPMCIB(MLPMPTR),B'00000001'                        0151
         BZ    @RT00151                                            0151
         L     @15,MLPMSCA(,MLPMPTR)                               0151
         LTR   @15,@15                                             0151
         BZ    @RT00151                                            0151
*     ;                             /* SKIP IF NO CIB BUFFER         */
*   ELSE                            /* A CIB BUFFER                  */
*     DO;                           /* YES                           */
*       RESPECIFY                                                  0154
*        (CIBNAME)                  /* SET UP NEW BASE FOR CIB       */
*             BASED(MLPMSCA);                                      0154
*       IF CIBDATLN>0 THEN          /* IF DATA, MOVE INTO            */
         L     @15,MLPMSCA(,MLPMPTR)                               0155
         LH    @14,CIBDATLN(,@15)                                  0155
         LTR   @14,@14                                             0155
         BNP   @RF00155                                            0155
*         MVC(CIBUFFER(1:CIBDATLN),CIBDATA);/* WORK BUFFER           */
         L     @01,MLPMSCA(,MLPMPTR)                               0156
         LH    @14,CIBDATLN(,@01)                                  0156
         BCTR  @14,0                                               0156
         EX    @14,@SM01645                                        0156
*       RESPECIFY                                                  0157
*        (CIBNAME);                 /* REVERT TO OLD BASE            */
@RF00155 DS    0H                                                  0158
*       RESPECIFY                                                  0158
*        (R2,                                                      0158
*         R3) RESTRICTED;                                          0158
*       R2=ADDR(CHCIBP);            /* ADDR OF CIB PTR               */
         L     R2,CSCBPTR(,TWAPTR)                                 0159
         LA    R2,CHCIBP(,R2)                                      0159
*       R3=MLPMSCA;                 /* ADDR OF CIB TO BE FREED       */
         L     R3,MLPMSCA(,MLPMPTR)                                0160
*       GEN(QEDIT ORIGIN=(R2),BLOCK=(R3))/* FREE CIB                 */
*       REFS(R0,R1,R2,R3,R14,R15);                                 0161
         QEDIT ORIGIN=(R2),BLOCK=(R3)
*       RESPECIFY                                                  0162
*        (R2,                                                      0162
*         R3) UNRESTRICTED;                                        0162
*     END;                                                         0163
*   IF MLPMFL1='00'X                /* CHECK                         */
*       (MLPMBUF='1'B&MLPMCIB='1'B) /* OBVIOUS                       */
*     THEN                                                         0164
@RT00151 CLI   MLPMFL1(MLPMPTR),X'00'                              0164
         BE    @RT00164                                            0164
         TM    MLPMBUF(MLPMPTR),B'00000011'                        0164
         BNO   @RF00164                                            0164
@RT00164 DS    0H                                                  0165
*     DO;                           /* INPUT                         */
*       MLPMINV='1'B;               /* ERRORS                        */
         OI    MLPMINV(MLPMPTR),B'01000000'                        0166
*       RETURN;                     /* .                             */
@EL00002 DS    0H                                                  0167
@EF00002 DS    0H                                                  0167
@ER00002 L     @14,@SA00002                                        0167
         BR    @14                                                 0167
*     END;                          /* .                             */
*   IF MLPMWPM='1'B THEN            /* PRINTOUT WANTED               */
@RF00164 TM    MLPMWPM(MLPMPTR),B'00010000'                        0169
         BNO   @RF00169                                            0169
*     DO;                           /* YES                           */
*       GEN(OPEN  (PRINTOUT,OUTPUT))                               0171
*       REFS(R0,R1,R14,R15,PRINTOUT);                              0171
         OPEN  (PRINTOUT,OUTPUT)
*       DCBPTR=ADDR(PRINTOUT);      /* SETUP PTR                     */
         LA    DCBPTR,PRINTOUT                                     0172
*       IF DCBABSW2='Y' DCBOFOPN='0'B THEN/* GOOD OPEN               */
         CLI   DCBABSW2,C'Y'                                       0173
         BE    @RT00173                                            0173
         TM    DCBOFOPN+40(DCBPTR),B'00010000'                     0173
         BNZ   @RF00173                                            0173
@RT00173 DS    0H                                                  0174
*         DO;                       /* NO                            */
*           RESPECIFY                                              0175
*            (R1) RESTRICTED;                                      0175
*           R1=17;                  /* MESSAGE NUMBER                */
         LA    R1,17                                               0176
*           CALL MSGBLK;            /* ADDR OF LIST FORM OF MSG      */
         L     @15,TWAMSG(,TWAPTR)                                 0177
         BALR  @14,@15                                             0177
*           R1=ADDR(IKT017I);       /* ADDR OF LIST FORM             */
         LA    @00,IKT017I(,R1)                                    0178
         LR    R1,@00                                              0178
*           GEN(WTO   MF=(E,(1)))   /* ISSUE WTO                     */
*           REFS(R0,R1,R14,R15);                                   0179
         WTO   MF=(E,(1))
*           RESPECIFY                                              0180
*            (R1) UNRESTRICTED;                                    0180
*           PRINTSW='0'B;           /* TURN OFF PRINT SW             */
         NI    PRINTSW,B'11110111'                                 0181
*         END;                                                     0182
*       ELSE                                                       0183
*         PRINTSW='1'B;             /* TURN ON PRINT SW              */
         B     @RC00173                                            0183
@RF00173 OI    PRINTSW,B'00001000'                                 0183
*     END;                                                         0184
*   ELSE                                                           0185
*     PRINTSW='0'B;                 /* TURN OFF PRINT SW             */
         B     @RC00169                                            0185
@RF00169 NI    PRINTSW,B'11110111'                                 0185
*   END CHKOPTNS;                                                  0186
         B     @EL00002                                            0186
         EJECT
*SCANPARM:                                                         0187
*   PROC OPTIONS(SAVE(14));                                        0187
SCANPARM ST    @14,@SA00003                                        0187
*                                                                  0188
*/*  *****************************************************************/
*/*                                                                  */
*/*    THIS SUBROUTINE PROCESSES THE PARAMETERS ENTERED BY THE       */
*/*    OPERATOR ON THE VARIOUS COMMANDS.  THE FOLLOWING THINGS ARE   */
*/*    DONE:                                                         */
*/*                                                                  */
*/*    -   INITIALIZE THE BUFFER SAVE AREA                           */
*/*    -   SET UP POINTER TO PROPER INPUT BUFFER                     */
*/*    -   IF BUFFER EMPTY, EXIT                                     */
*/*    -   EXCEPTION CODING                                          */
*/*    -   PROCESS THE PARMS FOUND IN THE BUFFER.  IF ANY PARMS ARE  */
*/*        IN ERROR, SET THE ERROR SWITCH AND EXIT.                  */
*/*    -   IF THE OPERATOR WANTS TO USE A DIFFERENT MEMBER NAME      */
*/*        WHEN ACCESSING PARMLIB, SET UP THE NEW NAME               */
*/*    -   IF CERTAIN OPTIONS ARE SPECIFIED BY THE OPERATOR, SET     */
*/*        SWITCHES TO INDICATE WHAT THEY ARE                        */
*/*                                                                  */
*/*  *****************************************************************/
*                                                                  0188
*   SAVEBUFR=SAVES1;                /* INITIALIZE SAVE AREA          */
         MVC   SAVEBUFR(24),SAVES1                                 0188
*   IF MLPMBUF='1'B THEN            /* DATA BUFFER                   */
         TM    MLPMBUF(MLPMPTR),B'00000010'                        0189
         BNO   @RF00189                                            0189
*     SCANPTR=MLPMSCA;              /* YES                           */
         L     SCANPTR,MLPMSCA(,MLPMPTR)                           0190
*   ELSE                                                           0191
*     SCANPTR=ADDR(CIBUFFER);       /* NO                            */
         B     @RC00189                                            0191
@RF00189 LA    SCANPTR,CIBUFFER                                    0191
*   IF PRINTSW='1'B THEN            /* PRINTOUT WANTED               */
@RC00189 TM    PRINTSW,B'00001000'                                 0192
         BNO   @RF00192                                            0192
*     DO;                           /* YES                           */
*       PRNTCC='1';                 /* SKIP TO TOP OF PAGE           */
         MVI   PRNTCC,C'1'                                         0194
*       PRNTDATA='CONSOLE VALUES ENTERED:';/* HEADER LINE            */
         MVI   PRNTDATA+23,C' '                                    0195
         MVC   PRNTDATA+24(108),PRNTDATA+23                        0195
         MVC   PRNTDATA(23),@CC01526                               0195
*       CALL PRINTIT;               /* PRINT THE HEADER              */
         BAL   @14,PRINTIT                                         0196
*       PRNTLINE='';                /* SKIP TO NEXT LINE             */
         MVI   PRNTLINE,C' '                                       0197
         MVC   PRNTLINE+1(132),PRNTLINE                            0197
*       IF WORKCHAR=' ' THEN        /* DATA                          */
         CLI   WORKCHAR(SCANPTR),C' '                              0198
         BNE   @RF00198                                            0198
*         PRNTDAT5='*NONE ENTERED*';/* FROM                          */
         MVI   PRNTDAT5+14,C' '                                    0199
         MVC   PRNTDAT5+15(113),PRNTDAT5+14                        0199
         MVC   PRNTDAT5(14),@CC01529                               0199
*       ELSE                        /* THE                           */
*         PRNTDAT5=DATABUF;         /* BUFFER                        */
         B     @RC00198                                            0200
@RF00198 MVI   PRNTDAT5+48,C' '                                    0200
         MVC   PRNTDAT5+49(79),PRNTDAT5+48                         0200
         MVC   PRNTDAT5(48),DATABUF(SCANPTR)                       0200
*       CALL PRINTIT;               /* PRINT THE BUFFER              */
@RC00198 BAL   @14,PRINTIT                                         0201
*     END;                                                         0202
*   IF WORKCHAR=' ' THEN            /* FIRST CHARACTER BLANK         */
@RF00192 CLI   WORKCHAR(SCANPTR),C' '                              0203
         BE    @RT00203                                            0203
*     RETURN;                       /* YES - EXIT                    */
*                                                                  0205
*/*  *****************************************************************/
*/*                                                                  */
*/*    THIS SECTION OF CODE HANDLES INPUT DATA THAT IS NOT IN        */
*/*    KEYWORD NAME, EQUAL SIGN, KEYWORD PARAMETER, SEQUENCE.        */
*/*                                                                  */
*/*  *****************************************************************/
*                                                                  0205
*   IF DATA2='U ' DATA7='IGNORE ' THEN/* IGNORE RESPONSE             */
         CLC   DATA2(2,SCANPTR),@CC01531                           0205
         BE    @RT00205                                            0205
         CLC   DATA7(7,SCANPTR),@CC01532                           0205
         BNE   @RF00205                                            0205
@RT00205 DS    0H                                                  0206
*     DO;                           /* YES                           */
*       MLPMIGN='1'B;               /* SET SWITCH                    */
         OI    MLPMIGN(MLPMPTR),B'00000001'                        0207
*       RETURN;                     /* EXIT                          */
@EL00003 DS    0H                                                  0208
@EF00003 DS    0H                                                  0208
@ER00003 L     @14,@SA00003                                        0208
         BR    @14                                                 0208
*     END;                                                         0209
*   IF DATA6='FSTOP ' THEN          /* 'FSTOP' BY ITSELF             */
@RF00205 CLC   DATA6(6,SCANPTR),@CC01533                           0210
         BNE   @RF00210                                            0210
*     DATA11='USER=FSTOP ';         /* YES - ADD KEYWORD NAME        */
         MVC   DATA11(11,SCANPTR),@CC01534                         0211
*   ELSE                                                           0212
*     IF DATA4='SIC ' THEN          /* 'SIC' BY ITSELF               */
         B     @RC00210                                            0212
@RF00210 CLC   DATA4(4,SCANPTR),@CC01536                           0212
         BNE   @RF00212                                            0212
*       DATA9='USER=SIC ';          /* YES - ADD KEYWORD NAME        */
         MVC   DATA9(9,SCANPTR),@CC01537                           0213
*   DO WHILE(WORKCHAR^=' ');        /* PROCESS PARMS                 */
@RF00212 DS    0H                                                  0214
@RC00210 B     @DE00214                                            0214
@DL00214 DS    0H                                                  0215
*     TABLEPTR=ADDR(VERIBUFR);      /* INITIALIZE                    */
         LA    TABLEPTR,VERIBUFR                                   0215
*     SAVEPTR=ADDR(SAVEBUFR);       /* POINTERS                      */
         LA    SAVEPTR,SAVEBUFR                                    0216
*     CALL FNDNMTCH;                /* GET A KEYWORD NAME            */
         BAL   @14,FNDNMTCH                                        0217
*     IF SCANERR='1'B THEN          /* WAS AN ERROR FOUND            */
         TM    SCANERR,B'10000000'                                 0218
         BNO   @RF00218                                            0218
*       DO;                         /* YES                           */
*         MLPMINV='1'B;             /* SHOW IT                       */
         OI    MLPMINV(MLPMPTR),B'01000000'                        0220
*         RETURN;                   /* AND EXIT                      */
         B     @EL00003                                            0221
*       END;                                                       0222
*     CALL PRCSPARM;                /* VERIFY KEYWORD PARAMETER      */
@RF00218 BAL   @14,PRCSPARM                                        0223
*     IF SAVECHAR=HEXFF THEN        /* WAS THE PARM INVALID          */
         CLI   SAVECHAR(SAVEPTR),X'FF'                             0224
         BNE   @RF00224                                            0224
*       DO;                         /* YES                           */
*         MLPMINV='1'B;             /* SHOW IT                       */
         OI    MLPMINV(MLPMPTR),B'01000000'                        0226
*         RETURN;                   /* AND EXIT                      */
         B     @EL00003                                            0227
*       END;                                                       0228
*     IF WORKCHAR^=' ' THEN         /* IF NOT THE END                */
@RF00224 CLI   WORKCHAR(SCANPTR),C' '                              0229
         BE    @RF00229                                            0229
*       SCANPTR=SCANPTR+1;          /* PT PAST THE COMMA             */
         AL    SCANPTR,@CF00057                                    0230
*   END;                                                           0231
@RF00229 DS    0H                                                  0231
@DE00214 CLI   WORKCHAR(SCANPTR),C' '                              0231
         BNE   @DL00214                                            0231
*   CALL GETMBRNM;                  /* SETUP PARMLIB MBR NAME        */
         BAL   @14,GETMBRNM                                        0232
*   IF PRINTSW='1'B THEN            /* PRINTOUT WANTED               */
         TM    PRINTSW,B'00001000'                                 0233
         BNO   @RF00233                                            0233
*     DO;                           /* YES - PRNT ACCPTD VALUES      */
*       PRNTCC='0';                 /* SKIP AN EXTRA LINE            */
         MVI   PRNTCC,C'0'                                         0235
*       PRNTDATA='CONSOLE VALUES ACCEPTED:';/* SUB-HEADER LINE       */
         MVI   PRNTDATA+24,C' '                                    0236
         MVC   PRNTDATA+25(107),PRNTDATA+24                        0236
         MVC   PRNTDATA(24),@CC01543                               0236
*       CALL PRINTIT;               /* PRINT SUB-HEADER              */
         BAL   @14,PRINTIT                                         0237
*       PRNTLINE='';                /* CLEAR PRINT LINE              */
         MVI   PRNTLINE,C' '                                       0238
         MVC   PRNTLINE+1(132),PRNTLINE                            0238
*       TABLEPTR=ADDR(VERIBUFR);    /* SETUP POINTER                 */
         LA    TABLEPTR,VERIBUFR                                   0239
*       L=(LENGTH(SAVEBUFR))/8;     /* NUMBER OF PARMS               */
         MVC   L(2),@CH00118                                       0240
*       DO M=1 TO L;                /* PRINT BUFFER PARMS            */
         LA    @04,1                                               0241
         B     @DE00241                                            0241
@DL00241 DS    0H                                                  0242
*         SAVEPTR=ADDR(SAVEBUFR)+8*(TBLDISP-1);/* SAVE AREA PTR      */
         LA    SAVEPTR,SAVEBUFR                                    0242
         LH    @04,TBLDISP(,TABLEPTR)                              0242
         BCTR  @04,0                                               0242
         SLA   @04,3                                               0242
         ALR   SAVEPTR,@04                                         0242
*         IF SAVECHAR^=HEXBC&SAVECHAR^=HEXFF THEN/* GOOD ENTRY       */
         CLI   SAVECHAR(SAVEPTR),X'BC'                             0243
         BE    @RF00243                                            0243
         CLI   SAVECHAR(SAVEPTR),X'FF'                             0243
         BE    @RF00243                                            0243
*           DO;                     /* YES                           */
*             WORKPARM=SAVAREA;     /* PRINT GOOD                    */
         MVC   WORKPARM(8),SAVAREA(SAVEPTR)                        0245
*             CALL EDITIT;          /* ENTRY                         */
         BAL   @14,EDITIT                                          0246
*           END;                                                   0247
*         ELSE                                                     0248
*           TABLEPTR=TABLEPTR+20;   /* NO - PT TO NEXT ENTRY         */
         B     @RC00243                                            0248
@RF00243 AL    TABLEPTR,@CF00144                                   0248
*       END;                                                       0249
@RC00243 LA    @04,1                                               0249
         AH    @04,M                                               0249
@DE00241 STH   @04,M                                               0249
         CH    @04,L                                               0249
         BNH   @DL00241                                            0249
*       EDFIELD(1)=HEXFF;           /* CAUSE LAST LINE               */
         MVI   EDFIELD,X'FF'                                       0250
*       CALL NSRTNPRT;              /* TO BE PRINTED                 */
         BAL   @14,NSRTNPRT                                        0251
*     END;                                                         0252
*   KEYWDNM='USER    ';             /* KEYWORD NAME WANTED           */
@RF00233 MVC   KEYWDNM(8),@CC00340                                 0253
*   TABLEPTR=ADDR(VERIBUFR);        /* KEYWORD TABLE                 */
         LA    TABLEPTR,VERIBUFR                                   0254
*   DO WHILE(TBLNAME^=KEYWDNM);     /* FIND                          */
         B     @DE00255                                            0255
@DL00255 DS    0H                                                  0256
*     TABLEPTR=TABLEPTR+20;         /* PROPER                        */
         AL    TABLEPTR,@CF00144                                   0256
*   END;                            /* ENTRY                         */
@DE00255 CLC   TBLNAME(8,TABLEPTR),KEYWDNM                         0257
         BNE   @DL00255                                            0257
*   SAVEPTR=ADDR(SAVEBUFR)+8*(TBLDISP-1);/* ADDR OF WORK AREA        */
         LA    SAVEPTR,SAVEBUFR                                    0258
         LH    @04,TBLDISP(,TABLEPTR)                              0258
         BCTR  @04,0                                               0258
         SLA   @04,3                                               0258
         ALR   SAVEPTR,@04                                         0258
*   IF SAVECHAR^=HEXBC THEN         /* USER= ENTERED                 */
         CLI   SAVECHAR(SAVEPTR),X'BC'                             0259
         BE    @RF00259                                            0259
*     IF SAVAREA='FSTOP   ' THEN    /* YES -                         */
         CLC   SAVAREA(8,SAVEPTR),@CC00302                         0260
         BNE   @RF00260                                            0260
*       MLPMFS='1'B;                /* SET                           */
         OI    MLPMFS(MLPMPTR),B'00000100'                         0261
*     ELSE                          /* PROPER                        */
*       MLPMSIC='1'B;               /* SWITCH                        */
         B     @RC00260                                            0262
@RF00260 OI    MLPMSIC(MLPMPTR),B'00000010'                        0262
*   KEYWDNM='USERMAX ';             /* KEYWORD NAME WANTED           */
@RC00260 DS    0H                                                  0263
@RF00259 MVC   KEYWDNM(8),@CC00350                                 0263
*   TABLEPTR=ADDR(VERIBUFR);        /* KEYWORD TABLE                 */
         LA    TABLEPTR,VERIBUFR                                   0264
*   DO WHILE(TBLNAME^=KEYWDNM);     /* FIND                          */
         B     @DE00265                                            0265
@DL00265 DS    0H                                                  0266
*     TABLEPTR=TABLEPTR+20;         /* PROPER                        */
         AL    TABLEPTR,@CF00144                                   0266
*   END;                            /* ENTRY                         */
@DE00265 CLC   TBLNAME(8,TABLEPTR),KEYWDNM                         0267
         BNE   @DL00265                                            0267
*   SAVEPTR=ADDR(SAVEBUFR)+8*(TBLDISP-1);/* ADDR OF WORK AREA        */
         LA    SAVEPTR,SAVEBUFR                                    0268
         LH    @04,TBLDISP(,TABLEPTR)                              0268
         BCTR  @04,0                                               0268
         SLA   @04,3                                               0268
         ALR   SAVEPTR,@04                                         0268
*   IF SAVECHAR^=HEXBC THEN         /* USERMAX= ENTERED              */
         CLI   SAVECHAR(SAVEPTR),X'BC'                             0269
         BE    @RF00269                                            0269
*     IF SAVAREAF=0 THEN            /* YES -                         */
         L     @15,SAVAREAF(,SAVEPTR)                              0270
         LTR   @15,@15                                             0270
         BNZ   @RF00270                                            0270
*       MLPMUZ='1'B;                /* SET                           */
         OI    MLPMUZ(MLPMPTR),B'00010000'                         0271
*     ELSE                          /* PROPER                        */
*       MLPMUNZ='1'B;               /* SWITCH                        */
         B     @RC00270                                            0272
@RF00270 OI    MLPMUNZ(MLPMPTR),B'00001000'                        0272
*   END SCANPARM;                                                  0273
         B     @EL00003                                            0273
         EJECT
*GETMBRNM:                                                         0274
*   PROC OPTIONS(SAVE(14));                                        0274
GETMBRNM ST    @14,12(,@13)                                        0274
*                                                                  0275
*/*  *****************************************************************/
*/*                                                                  */
*/*    THIS SUBROUTINE DETERMINES WHAT MEMBER NAME THE OPERATOR      */
*/*    WANTS USED IN ACCESSING PARMLIB.                              */
*/*                                                                  */
*/*  *****************************************************************/
*                                                                  0275
*   KEYWDNM='MEMBER  ';             /* KEYWORD NAME WANTED           */
         MVC   KEYWDNM(8),@CC00329                                 0275
*   TABLEPTR=ADDR(VERIBUFR);        /* KEYWORD TABLE                 */
         LA    TABLEPTR,VERIBUFR                                   0276
*   DO WHILE(TBLNAME^=KEYWDNM);     /* FIND                          */
         B     @DE00277                                            0277
@DL00277 DS    0H                                                  0278
*     TABLEPTR=TABLEPTR+20;         /* PROPER                        */
         AL    TABLEPTR,@CF00144                                   0278
*   END;                            /* ENTRY                         */
@DE00277 CLC   TBLNAME(8,TABLEPTR),KEYWDNM                         0279
         BNE   @DL00277                                            0279
*   SAVEPTR=ADDR(SAVEBUFR)+8*(TBLDISP-1);/* ADDR OF WORK AREA        */
         LA    SAVEPTR,SAVEBUFR                                    0280
         LH    @04,TBLDISP(,TABLEPTR)                              0280
         BCTR  @04,0                                               0280
         SLA   @04,3                                               0280
         ALR   SAVEPTR,@04                                         0280
*   IF SAVECHAR=HEXBC THEN                                         0281
         CLI   SAVECHAR(SAVEPTR),X'BC'                             0281
         BE    @RT00281                                            0281
*     ;                             /* MEMBER= ENTERED NO - USE NAME
*                                      IN PROC                       */
*   ELSE                                                           0283
*     DO;                           /* YES - GET MEMBER NAME         */
*       WORKPARM=SAVAREA;           /* GET NAME ENTERED BY THE     0284
*                                      OPERATOR IF ITS 2 DIGITS, ADD
*                                      THEM TO 'MEMDEFLT', ELSE USE
*                                      THE NAME AS IS                */
         MVC   WORKPARM(8),SAVAREA(SAVEPTR)                        0284
*       IF(WORKPRM(1)>='0'&WORKPRM(1)<='9')&(WORKPRM(2)>='0'&WORKPRM(2)
*           <='9') THEN                                            0285
         CLI   WORKPRM,C'0'                                        0285
         BL    @RF00285                                            0285
         CLI   WORKPRM,C'9'                                        0285
         BH    @RF00285                                            0285
         CLI   WORKPRM+1,C'0'                                      0285
         BL    @RF00285                                            0285
         CLI   WORKPRM+1,C'9'                                      0285
         BH    @RF00285                                            0285
*         DO;                                                      0286
*           TSO=MEMDEFLT;                                          0287
         MVC   TSO(6),@CC00181                                     0287
*           TSO1=WORKPRM(1);                                       0288
         MVC   TSO1(1),WORKPRM                                     0288
*           TSO2=WORKPRM(2);                                       0289
         MVC   TSO2(1),WORKPRM+1                                   0289
*           SAVAREA=TSOKEYNN;                                      0290
         MVC   SAVAREA(8,SAVEPTR),TSOKEYNN                         0290
*         END;                                                     0291
*     END;                                                         0292
*   END GETMBRNM;                                                  0293
@EL00004 DS    0H                                                  0293
@EF00004 DS    0H                                                  0293
@ER00004 L     @14,12(,@13)                                        0293
         BR    @14                                                 0293
         EJECT
*READMBR:                                                          0294
*   PROC OPTIONS(SAVE(14));                                        0294
READMBR  ST    @14,@SA00005                                        0294
*                                                                  0295
*/*  *****************************************************************/
*/*                                                                  */
*/*    THE PURPOSE OF THIS SUBROUTINE IS TO READ AND PROCESS THE     */
*/*    MEMBER OF PARMLIB WHICH THE OPERATOR WANTS TO USE IN          */
*/*    BRINGING UP TCAS.  THE FOLLOWING THINGS ARE DONE:             */
*/*                                                                  */
*/*    -   INITIALIZE THE MEMBER SAVE AREA                           */
*/*    -   EXIT IF PARMLIB NOT WANTED                                */
*/*    -   OPEN THE PARMLIB WITH THE PROPER MEMBER NAME.  IF AN      */
*/*        ERROR IS ENCOUNTERED, ISSUE AN ERROR MESSAGE AND EXIT     */
*/*    -   PROCESS THE PARMS FOUND IN THE MEMBER.  NOTE THAT THEY    */
*/*        MAY BE CONTINUED FROM ONE CARD TO THE NEXT.  THE ONLY     */
*/*        REQUIREMENT FOR CONTINUING A CARD IS TO HAVE ', ' AFTER   */
*/*        THE LAST PARAMETER ON THE CARD.  THE NEXT CARD CAN START  */
*/*        ANYPLACE.                                                 */
*/*    -   CLOSE PARMLIB                                             */
*/*    -   MAKE ANY CONSISTENCY CHECKS THAT ARE NEEDED               */
*/*                                                                  */
*/*  *****************************************************************/
*                                                                  0295
*   SAVEMBR=SAVES2;                 /* INITIALIZE WORK AREA          */
         MVC   SAVEMBR(208),SAVES2                                 0295
*   IF MLPMRLIB='0'B THEN           /* SHOULD WE READ PARMLIB        */
         TM    MLPMRLIB(MLPMPTR),B'00000100'                       0296
         BZ    @RT00296                                            0296
*     RETURN;                       /* NO - EXIT                     */
*   RESPECIFY                                                      0298
*    (R15) RESTRICTED;                                             0298
*   GEN(RDJFCB PARMLIB)             /* READ THE JFCB INTO CORE       */
*   REFS(R0,R1,R14,R15,PARMLIB);                                   0299
         RDJFCB PARMLIB
*   IF R15=4 THEN                   /* GOOD READ                     */
         C     R15,@CF00063                                        0300
         BNE   @RF00300                                            0300
*     DO;                           /* NO                            */
*       RESPECIFY                                                  0302
*        (R1) RESTRICTED;                                          0302
*       R1=13;                      /* MESSAGE NUMBER                */
         LA    R1,13                                               0303
*       CALL MSGBLK;                /* ADDR OF LIST FORM OF MSG      */
         L     @15,TWAMSG(,TWAPTR)                                 0304
         BALR  @14,@15                                             0304
*       R1=ADDR(IKT013I);           /* ADDR OF LIST FORM             */
         LA    @14,IKT013I(,R1)                                    0305
         LR    R1,@14                                              0305
*       GEN(WTO   MF=(E,(1)))       /* ISSUE WTO                     */
*       REFS(R0,R1,R14,R15);                                       0306
         WTO   MF=(E,(1))
*       RESPECIFY                                                  0307
*        (R1) UNRESTRICTED;                                        0307
*       RETURN;                                                    0308
@EL00005 DS    0H                                                  0308
@EF00005 DS    0H                                                  0308
@ER00005 L     @14,@SA00005                                        0308
         BR    @14                                                 0308
*     END;                                                         0309
*   RESPECIFY                                                      0310
*    (R15) UNRESTRICTED;                                           0310
@RF00300 DS    0H                                                  0311
*   KEYWDNM='MEMBER  ';             /* KEYWORD NAME WANTED           */
         MVC   KEYWDNM(8),@CC00329                                 0311
*   TABLEPTR=ADDR(VERIBUFR);        /* KEYWORD TABLE                 */
         LA    TABLEPTR,VERIBUFR                                   0312
*   DO WHILE(TBLNAME^=KEYWDNM);     /* FIND                          */
         B     @DE00313                                            0313
@DL00313 DS    0H                                                  0314
*     TABLEPTR=TABLEPTR+20;         /* PROPER                        */
         AL    TABLEPTR,@CF00144                                   0314
*   END;                            /* ENTRY                         */
@DE00313 CLC   TBLNAME(8,TABLEPTR),KEYWDNM                         0315
         BNE   @DL00313                                            0315
*   SAVEPTR=ADDR(SAVEBUFR)+8*(TBLDISP-1);/* ADDR OF MBR SAVE AREA    */
         LA    SAVEPTR,SAVEBUFR                                    0316
         LH    @15,TBLDISP(,TABLEPTR)                              0316
         BCTR  @15,0                                               0316
         SLA   @15,3                                               0316
         ALR   SAVEPTR,@15                                         0316
*   JFCBPTR=ADDR(JFCBAREA);         /* ADDR OF JFCB                  */
         LA    JFCBPTR,JFCBAREA                                    0317
*   IF SAVECHAR^=HEXBC THEN         /* MEMBER= ENTERED               */
         CLI   SAVECHAR(SAVEPTR),X'BC'                             0318
         BE    @RF00318                                            0318
*     JFCBELNM=SAVAREA;             /* YES - MOVE IN NEW NAME        */
         MVC   JFCBELNM(8,JFCBPTR),SAVAREA(SAVEPTR)                0319
*   ELSE                            /* NO                            */
*     IF JFCBELNM=BLANK THEN        /* DEFAULT MBR NAME NEEDED       */
         B     @RC00318                                            0320
@RF00318 CLC   JFCBELNM(8,JFCBPTR),@CC00097                        0320
         BNE   @RF00320                                            0320
*       JFCBELNM=MBRDEFLT;          /* YES - SUPPLY IT               */
         MVC   JFCBELNM(8,JFCBPTR),@CC00179                        0321
*   GEN(OPEN  (PARMLIB),TYPE=J)     /* OPEN PARMLIB                  */
*   REFS(R0,R1,R14,R15,PARMLIB);                                   0322
@RF00320 DS    0H                                                  0322
@RC00318 DS    0H                                                  0322
         OPEN  (PARMLIB),TYPE=J
*   DCBPTR=ADDR(PARMLIB);           /* PTR TO DCB                    */
         LA    DCBPTR,PARMLIB                                      0323
*   IF DCBABSW1='Y' DCBOFOPN='0'B THEN/* GOOD OPEN                   */
         CLI   DCBABSW1,C'Y'                                       0324
         BE    @RT00324                                            0324
         TM    DCBOFOPN+40(DCBPTR),B'00010000'                     0324
         BNZ   @RF00324                                            0324
@RT00324 DS    0H                                                  0325
*     DO;                           /* NO                            */
*       RESPECIFY                                                  0326
*        (R1) RESTRICTED;                                          0326
*       R1=13;                      /* MESSAGE NUMBER                */
         LA    R1,13                                               0327
*       CALL MSGBLK;                /* ADDR OF LIST FORM OF MSG      */
         L     @15,TWAMSG(,TWAPTR)                                 0328
         BALR  @14,@15                                             0328
*       R1=ADDR(IKT013I);           /* ADDR OF LIST FORM             */
         LA    @06,IKT013I(,R1)                                    0329
         LR    R1,@06                                              0329
*       GEN(WTO   MF=(E,(1)))       /* ISSUE WTO                     */
*       REFS(R0,R1,R14,R15);                                       0330
         WTO   MF=(E,(1))
*       RESPECIFY                                                  0331
*        (R1) UNRESTRICTED;                                        0331
*       RETURN;                                                    0332
         B     @EL00005                                            0332
*     END;                                                         0333
*   IF PRINTSW='1'B THEN            /* PRINTOUT WANTED               */
@RF00324 TM    PRINTSW,B'00001000'                                 0334
         BNO   @RF00334                                            0334
*     DO;                           /* YES                           */
*       PRNTCC='-';                 /* SKIP                          */
         MVI   PRNTCC,C'-'                                         0336
*       PRNTDATA='';                /* TWO                           */
         MVI   PRNTDATA,C' '                                       0337
         MVC   PRNTDATA+1(131),PRNTDATA                            0337
*       CALL PRINTIT;               /* LINES                         */
         BAL   @14,PRINTIT                                         0338
*       PRNTDATA='PARMLIB VALUES ENTERED:';/* SUB-HEADER LINE        */
         MVI   PRNTDATA+23,C' '                                    0339
         MVC   PRNTDATA+24(108),PRNTDATA+23                        0339
         MVC   PRNTDATA(23),@CC01556                               0339
*       CALL PRINTIT;               /* PRINT THE SUB-HEADER          */
         BAL   @14,PRINTIT                                         0340
*       PRNTLINE='';                /* CLEAR THE LINE                */
         MVI   PRNTLINE,C' '                                       0341
         MVC   PRNTLINE+1(132),PRNTLINE                            0341
*     END;                                                         0342
*   DUMPCNT=0;                      /* INITIALIZE RCFBDUMP KEY- WORD
*                                      COUNTER               @ZM20856*/
@RF00334 SLR   @02,@02                                             0343
         STH   @02,DUMPCNT                                         0343
*   DO WHILE(GOSW1='1'B);           /* PROCESS ALL RECDS IN MBR      */
         B     @DE00344                                            0344
@DL00344 DS    0H                                                  0345
*     READSW1='1'B;                 /* SET READ SW                   */
         OI    READSW1,B'01000000'                                 0345
*     DO WHILE(READSW1='1'B);       /* FIND A NON-BLANK RECD         */
         B     @DE00346                                            0346
@DL00346 DS    0H                                                  0347
*       GEN(GET   PARMLIB,MBRRECD)  /* READ A MBR RECD               */
*       REFS(R0,R1,R14,R15,PARMLIB,MBRRECD);                       0347
         GET   PARMLIB,MBRRECD
*       IF SYNADSW1='Y' THEN        /* I/O ERROR DETECTED            */
         CLI   SYNADSW1,C'Y'                                       0348
         BE    @RT00348                                            0348
*         GOTO SYNAD;               /* YES - IGNORE REST OF MBR      */
*       IF PRINTSW='1'B THEN        /* PRINTOUT WANTED               */
         TM    PRINTSW,B'00001000'                                 0350
         BNO   @RF00350                                            0350
*         DO;                       /* YES                           */
*           PRNTDAT5=MBRRECD;       /* PARMLIB RECORD                */
         MVI   PRNTDAT5+80,C' '                                    0352
         MVC   PRNTDAT5+81(47),PRNTDAT5+80                         0352
         MVC   PRNTDAT5(80),MBRRECD                                0352
*           CALL PRINTIT;           /* PRINT RECORD                  */
         BAL   @14,PRINTIT                                         0353
*         END;                                                     0354
*       MBR7380=BLANK;              /* BLANK OUT SEQUENCE NBR        */
@RF00350 MVC   MBR7380(8),@CC00097                                 0355
*       MBR80=HEXFF;                /* SCAN ENDING CHARACTER         */
         MVI   MBR80,X'FF'                                         0356
*       SCANPTR=ADDR(MBRRECD);      /* START OF RECORD               */
         LA    SCANPTR,MBRRECD                                     0357
*       DO WHILE(WORKCHAR=' ');     /* SKIP BLANKS                   */
         B     @DE00358                                            0358
@DL00358 DS    0H                                                  0359
*         SCANPTR=SCANPTR+1;                                       0359
         AL    SCANPTR,@CF00057                                    0359
*       END;                                                       0360
@DE00358 CLI   WORKCHAR(SCANPTR),C' '                              0360
         BE    @DL00358                                            0360
*       IF WORKCHAR^=HEXFF THEN     /* GOOD RECORD                   */
         CLI   WORKCHAR(SCANPTR),X'FF'                             0361
         BE    @RF00361                                            0361
*         READSW1='0'B;             /* YES                           */
         NI    READSW1,B'10111111'                                 0362
*     END;                                                         0363
@RF00361 DS    0H                                                  0363
@DE00346 TM    READSW1,B'01000000'                                 0363
         BO    @DL00346                                            0363
*     GOSW2='1'B;                   /* SET GO SW                     */
         OI    GOSW2,B'00010000'                                   0364
*     DO WHILE(GOSW2='1'B);         /* PROCESS RECORD                */
         B     @DE00365                                            0365
@DL00365 DS    0H                                                  0366
*       TABLEPTR=ADDR(VERIMBR);     /* SETUP                         */
         LA    TABLEPTR,VERIMBR                                    0366
*       SAVEPTR=ADDR(SAVEMBR);      /* POINTERS                      */
         LA    SAVEPTR,SAVEMBR                                     0367
*       CALL FNDNMTCH;              /* GET A KEYWORD NAME            */
         BAL   @14,FNDNMTCH                                        0368
*       IF SCANERR='1'B THEN        /* ERROR                         */
         TM    SCANERR,B'10000000'                                 0369
         BNO   @RF00369                                            0369
*         CALL MBRERROR;            /* YES                           */
         BAL   @14,MBRERROR                                        0370
*       ELSE                                                       0371
*         DO;                       /* NO                            */
         B     @RC00369                                            0371
@RF00369 DS    0H                                                  0372
*           CALL PRCSPARM;          /* VERIFY KEYWORD PARAMETER SEE
*                                      IF MORE PARMS                 */
         BAL   @14,PRCSPARM                                        0372
*           IF WORKCHAR=' ' DATA2=', ' THEN                        0373
         CLI   WORKCHAR(SCANPTR),C' '                              0373
         BE    @RT00373                                            0373
         CLC   DATA2(2,SCANPTR),@CC01560                           0373
         BNE   @RF00373                                            0373
@RT00373 DS    0H                                                  0374
*             GOSW2='0'B;                                          0374
         NI    GOSW2,B'11101111'                                   0374
*           ELSE                                                   0375
*             SCANPTR=SCANPTR+1;                                   0375
         B     @RC00373                                            0375
@RF00373 AL    SCANPTR,@CF00057                                    0375
*         END;                                                     0376
@RC00373 DS    0H                                                  0377
*     END;                                                         0377
@RC00369 DS    0H                                                  0377
@DE00365 TM    GOSW2,B'00010000'                                   0377
         BO    @DL00365                                            0377
*     IF WORKCHAR=' ' THEN          /* END OF ALL PARMS              */
         CLI   WORKCHAR(SCANPTR),C' '                              0378
         BNE   @RF00378                                            0378
*       GOSW1='0'B;                 /* YES                           */
         NI    GOSW1,B'11011111'                                   0379
*   END;                                                           0380
@RF00378 DS    0H                                                  0380
@DE00344 TM    GOSW1,B'00100000'                                   0380
         BO    @DL00344                                            0380
*   GOTO EOF;                       /* SKIP SYNAD RTN                */
         B     EOF                                                 0381
*SYNAD:                                                            0382
*   ;                                                              0382
SYNAD    DS    0H                                                  0383
*   RESPECIFY                                                      0383
*    (R1) RESTRICTED;                                              0383
*   R1=14;                          /* MESSAGE NUMBER                */
         LA    R1,14                                               0384
*   CALL MSGBLK;                    /* ADDR OF LIST FORM OF MSG      */
         L     @15,TWAMSG(,TWAPTR)                                 0385
         BALR  @14,@15                                             0385
*   R1=ADDR(IKT014I);               /* ADDR OF LIST FORM             */
         LA    @00,IKT014I(,R1)                                    0386
         LR    R1,@00                                              0386
*   GEN(WTO   MF=(E,(1)))           /* ISSUE WTO                     */
*   REFS(R0,R1,R14,R15);                                           0387
         WTO   MF=(E,(1))
*   RESPECIFY                                                      0388
*    (R1) UNRESTRICTED;                                            0388
*EOF:                                                              0389
*   ;                               /* ENTRY FOR EODAD               */
EOF      DS    0H                                                  0390
*   GEN(CLOSE PARMLIB)              /* CLOSE PARMLIB                 */
*   REFS(R0,R1,R14,R15,PARMLIB);                                   0390
         CLOSE PARMLIB
*   KEYWDNM='HIBFREXT';             /* KEYWORD NAME WANTED           */
         MVC   KEYWDNM(8),@CC00394                                 0391
*   TABLEPTR=ADDR(VERIMBR);         /* KEYWORD TABLE                 */
         LA    TABLEPTR,VERIMBR                                    0392
*   DO WHILE(TBLNAME^=KEYWDNM);     /* FIND                          */
         B     @DE00393                                            0393
@DL00393 DS    0H                                                  0394
*     TABLEPTR=TABLEPTR+20;         /* PROPER                        */
         AL    TABLEPTR,@CF00144                                   0394
*   END;                            /* ENTRY                         */
@DE00393 CLC   TBLNAME(8,TABLEPTR),KEYWDNM                         0395
         BNE   @DL00393                                            0395
*   SAVEPTR=ADDR(SAVEMBR)+8*(TBLDISP-1);/* ADDR OF WORK AREA         */
         LA    SAVEPTR,SAVEMBR                                     0396
         LH    @04,TBLDISP(,TABLEPTR)                              0396
         BCTR  @04,0                                               0396
         SLA   @04,3                                               0396
         ALR   SAVEPTR,@04                                         0396
*   IF SAVECHAR^=HEXBC&SAVECHAR^=HEXFF THEN/* VALID ENTRY            */
         CLI   SAVECHAR(SAVEPTR),X'BC'                             0397
         BE    @RF00397                                            0397
         CLI   SAVECHAR(SAVEPTR),X'FF'                             0397
         BE    @RF00397                                            0397
*     DO;                           /* YES                           */
*       WORKPARM=SAVAREA;           /* SAVE VALUE                    */
         MVC   WORKPARM(8),SAVAREA(SAVEPTR)                        0399
*       TEMPPTR=SAVEPTR;            /* SAVE PTR                      */
         LR    TEMPPTR,SAVEPTR                                     0400
*       KEYWDNM='LOBFREXT';         /* KEYWORD NAME WANTED           */
         MVC   KEYWDNM(8),@CC00403                                 0401
*       TABLEPTR=ADDR(VERIMBR);     /* KEYWORD TABLE                 */
         LA    TABLEPTR,VERIMBR                                    0402
*       DO WHILE(TBLNAME^=KEYWDNM); /* FIND                          */
         B     @DE00403                                            0403
@DL00403 DS    0H                                                  0404
*         TABLEPTR=TABLEPTR+20;     /* PROPER                        */
         AL    TABLEPTR,@CF00144                                   0404
*       END;                        /* ENTRY                         */
@DE00403 CLC   TBLNAME(8,TABLEPTR),KEYWDNM                         0405
         BNE   @DL00403                                            0405
*       SAVEPTR=ADDR(SAVEMBR)+8*(TBLDISP-1);/* ADDR OF WORK AREA     */
         LA    SAVEPTR,SAVEMBR                                     0406
         LH    @04,TBLDISP(,TABLEPTR)                              0406
         BCTR  @04,0                                               0406
         SLA   @04,3                                               0406
         ALR   SAVEPTR,@04                                         0406
*       IF SAVECHAR^=HEXBC&SAVECHAR^=HEXFF THEN/* VALID ENTRY        */
         CLI   SAVECHAR(SAVEPTR),X'BC'                             0407
         BE    @RF00407                                            0407
         CLI   SAVECHAR(SAVEPTR),X'FF'                             0407
         BE    @RF00407                                            0407
*         DO;                       /* YES                           */
*           IF WRKPRMF2<=SAVAREAF THEN/* HIBFREXT <= LOBFREXT        */
         L     @15,WRKPRMF2                                        0409
         C     @15,SAVAREAF(,SAVEPTR)                              0409
         BH    @RF00409                                            0409
*             DO;                   /* YES                           */
*               SAVECHAR=HEXFF;     /* FLAG LOBFREXT                 */
         MVI   SAVECHAR(SAVEPTR),X'FF'                             0411
*               TEMPPTR->SAVECHAR=HEXFF;/* FLAG HIBFREXT             */
         MVI   SAVECHAR(TEMPPTR),X'FF'                             0412
*             END;                                                 0413
*         END;                                                     0414
@RF00409 DS    0H                                                  0415
*     END;                                                         0415
@RF00407 DS    0H                                                  0416
*   IF PRINTSW='1'B THEN            /* PRINTOUT WANTED               */
@RF00397 TM    PRINTSW,B'00001000'                                 0416
         BNO   @RF00416                                            0416
*     DO;                           /* YES                           */
*       PRNTCC='0';                 /* SKIP AN EXTRA LINE            */
         MVI   PRNTCC,C'0'                                         0418
*       PRNTDATA='PARMLIB VALUES ACCEPTED:';/* SUB-HEADER            */
         MVI   PRNTDATA+24,C' '                                    0419
         MVC   PRNTDATA+25(107),PRNTDATA+24                        0419
         MVC   PRNTDATA(24),@CC01564                               0419
*       CALL PRINTIT;               /* PRINT SUB-HEADER              */
         BAL   @14,PRINTIT                                         0420
*       PRNTLINE='';                /* CLEAR THE PRINT LINE          */
         MVI   PRNTLINE,C' '                                       0421
         MVC   PRNTLINE+1(132),PRNTLINE                            0421
*       TABLEPTR=ADDR(VERIMBR);     /* SETUP POINTER                 */
         LA    TABLEPTR,VERIMBR                                    0422
*       L=(LENGTH(SAVEMBR))/8;      /* NUMBER OF PARMS               */
         MVC   L(2),@CH00189                                       0423
*       DO M=1 TO L;                /* PRINT PARMLIB PARMS           */
         LA    @04,1                                               0424
         B     @DE00424                                            0424
@DL00424 DS    0H                                                  0425
*         SAVEPTR=ADDR(SAVEMBR)+8*(TBLDISP-1);/* SAVE AREA ADDR      */
         LA    SAVEPTR,SAVEMBR                                     0425
         LH    @04,TBLDISP(,TABLEPTR)                              0425
         BCTR  @04,0                                               0425
         SLA   @04,3                                               0425
         ALR   SAVEPTR,@04                                         0425
*         IF SAVECHAR^=HEXBC&SAVECHAR^=HEXFF THEN/* GOOD ENTRY       */
         CLI   SAVECHAR(SAVEPTR),X'BC'                             0426
         BE    @RF00426                                            0426
         CLI   SAVECHAR(SAVEPTR),X'FF'                             0426
         BE    @RF00426                                            0426
*           DO;                     /* YES                           */
*             IF TBLNAME=RCFBDUMP THEN                             0428
         CLC   TBLNAME(8,TABLEPTR),RCFBDUMP                        0428
         BNE   @RF00428                                            0428
*               DO;                 /* PROCESS DUMP          @ZM20856*/
*                 DUMPPTR=SAVEPTR+5;/* SKIP TO CODES         @ZM21059*/
         LA    @04,5                                               0430
         ALR   @04,SAVEPTR                                         0430
         ST    @04,DUMPPTR                                         0430
*                 CALL EDITIT;      /* PRINT KEYWORD         @ZM20856*/
         BAL   @14,EDITIT                                          0431
*               END;                /* END RCFBDUMP PROCESSING     0432
*                                                            @ZM20856*/
*             ELSE                                                 0433
*               DO;                 /* OTHER KEYWORDS        @ZM20856*/
         B     @RC00428                                            0433
@RF00428 DS    0H                                                  0434
*                 WORKPARM=SAVAREA; /* PRINT GOOD                    */
         MVC   WORKPARM(8),SAVAREA(SAVEPTR)                        0434
*                 CALL EDITIT;      /* ENTRY                         */
         BAL   @14,EDITIT                                          0435
*               END;                /*                       @ZM20856*/
*           END;                                                   0437
*         ELSE                                                     0438
*           TABLEPTR=TABLEPTR+20;   /* NO - PT TO NEXT ENTRY         */
         B     @RC00426                                            0438
@RF00426 AL    TABLEPTR,@CF00144                                   0438
*       END;                                                       0439
@RC00426 LA    @04,1                                               0439
         AH    @04,M                                               0439
@DE00424 STH   @04,M                                               0439
         CH    @04,L                                               0439
         BNH   @DL00424                                            0439
*       EDFIELD(1)=HEXFF;           /* CAUSE LAST LINE               */
         MVI   EDFIELD,X'FF'                                       0440
*       CALL NSRTNPRT;              /* TO BE PRINTED                 */
         BAL   @14,NSRTNPRT                                        0441
*     END;                                                         0442
*   END READMBR;                                                   0443
         B     @EL00005                                            0443
         EJECT
*MBRERROR:                                                         0444
*   PROC OPTIONS(SAVE(14));                                        0444
MBRERROR ST    @14,12(,@13)                                        0444
*                                                                  0445
*/*  *****************************************************************/
*/*                                                                  */
*/*    THIS SUBROUTINE HANDLES THE ERRORS FOUND BY 'FNDNMTCH' WHEN   */
*/*    WORKING WITH A PARMLIB MEMBER.                                */
*/*                                                                  */
*/*  *****************************************************************/
*                                                                  0445
*   SCANERR='0'B;                   /* TURN OFF ERROR SW             */
         NI    SCANERR,B'01111111'                                 0445
*   DO WHILE(GOSW2='1'B);           /* GO THRU ONCE OR TWICE         */
         B     @DE00446                                            0446
@DL00446 DS    0H                                                  0447
*     IF WORKCHAR=' ' DATA2=', ' THEN/* END OF A RECORD              */
         CLI   WORKCHAR(SCANPTR),C' '                              0447
         BE    @RT00447                                            0447
         CLC   DATA2(2,SCANPTR),@CC01560                           0447
         BNE   @RF00447                                            0447
@RT00447 DS    0H                                                  0448
*       DO;                         /* YES                           */
*         GOSW2='0'B;               /* END RECORD LOOP               */
         NI    GOSW2,B'11101111'                                   0449
*         RETURN;                   /* EXIT                          */
@EL00006 DS    0H                                                  0450
@EF00006 DS    0H                                                  0450
@ER00006 L     @14,12(,@13)                                        0450
         BR    @14                                                 0450
*       END;                                                       0451
*     IF WORKCHAR=',' THEN          /* END OF A PARM                 */
@RF00447 CLI   WORKCHAR(SCANPTR),C','                              0452
         BNE   @RF00452                                            0452
*       DO;                         /* YES                           */
*         SCANPTR=SCANPTR+1;        /* POINT PAST ','                */
         AL    SCANPTR,@CF00057                                    0454
*         RETURN;                   /* EXIT                          */
         B     @EL00006                                            0455
*       END;                                                       0456
*     DO WHILE(WORKCHAR^=' '&WORKCHAR^=',');                       0457
@RF00452 B     @DE00457                                            0457
@DL00457 DS    0H                                                  0458
*       SCANPTR=SCANPTR+1;          /* SKIP TO NEXT PARM             */
         AL    SCANPTR,@CF00057                                    0458
*     END;                                                         0459
@DE00457 CLI   WORKCHAR(SCANPTR),C' '                              0459
         BE    @DC00457                                            0459
         CLI   WORKCHAR(SCANPTR),C','                              0459
         BNE   @DL00457                                            0459
@DC00457 DS    0H                                                  0460
*   END;                                                           0460
@DE00446 TM    GOSW2,B'00010000'                                   0460
         BO    @DL00446                                            0460
*   END MBRERROR;                                                  0461
         B     @EL00006                                            0461
         EJECT
*OVERLAY:                                                          0462
*   PROC OPTIONS(SAVE(14));                                        0462
OVERLAY  ST    @14,12(,@13)                                        0462
*                                                                  0463
*/*  *****************************************************************/
*/*                                                                  */
*/*    THIS SUBROUTINE BUILDS A TABLE OF PARM VALUES THAT MAY BE     */
*/*    USED TO UPDATE THE TCAS TABLE.  NOTE THAT THE TABLE 'DEFAULTS'*/
*/*    IS DEPENDENT ON THE ORDER OF THE KEYWORD NAMES AS SET UP IN   */
*/*    'VERIMBR'.                                                    */
*/*                                                                  */
*/*    THE PROCESS OF UPDATTING THE TCAS TABLE WITH NEW VALUES IS    */
*/*    DONE IN THE FOLLOWING MANNER:                                 */
*/*                                                                  */
*/*    -   INITIALIZE THE BUFFER SAVE AREA (DONE IN 'SCANPARM').     */
*/*    -   MOVE VALIDATED PARAMETERS INTO THEIR PROPER SPOT IN THE   */
*/*        BUFFER SAVE AREA (DONE IN 'SCANPARM').                    */
*/*    -   INITIALIZE THE MEMBER SAVE AREA (DONE IN 'READMBR').      */
*/*    -   MOVE VALIDATED PARAMETERS INTO THEIR PROPER SPOT IN THE   */
*/*        MEMBER SAVE AREA (DONE IN 'READMBR').                     */
*/*    -   INITIALIZE THE MERGE AREA.  IF NO PARMLIB WAS READ, THE   */
*/*        AREA IS NULL'ED OUT, OTHERWISE, HARD-CODED DEFAULT VALUES */
*/*        ARE MOVED INTO THE MERGE AREA.  NOTE THAT THE MERGE AREA  */
*/*        HAS THE SAME ORGANIZATION AS THE MEMBER SAVE AREA.        */
*/*    -   SCAN THE MEMBER SAVE AREA.  IF A VALIDATED PARAMETER IS   */
*/*        ENCOUNTERED, MOVE IT TO ITS PROPER SPOT IN THE MERGE AREA.*/
*/*    -   SCAN THE BUFFER SAVE AREA.  IF A VALIDATED PARAMETER IS   */
*/*        ENCOUNTERED, MOVE IT TO ITS PROPER SPOT IN THE MERGE AREA.*/
*/*        NOTE THAT THE ABOVE 3 STATEMENTS IMPLY THAT OPERATOR      */
*/*        ENTERED PARAMETERS OVERRIDE PARMLIB MEMBER PARAMETERS,    */
*/*        WHICH IN TURN OVERRIDE HARD-CODED DEFAULTS.               */
*/*    -   SCAN THE MERGE AREA.  IF A VALIDATED PARAMETER IS         */
*/*        ENCOUNTERED, MOVE IT TO ITS PROPER SPOT IN THE TCAS       */
*/*        TABLE (DONE IN 'TCASUPDT').                               */
*/*                                                                  */
*/*  *****************************************************************/
*                                                                  0463
*   IF MLPMRLIB='0'B THEN           /* PARMLIB MEMBER READ           */
         TM    MLPMRLIB(MLPMPTR),B'00000100'                       0463
         BNZ   @RF00463                                            0463
*     MERGAREA=SAVES2;              /* NO                            */
         MVC   MERGAREA(208),SAVES2                                0464
*   ELSE                                                           0465
*     DO;                           /* YES                           */
         B     @RC00463                                            0465
@RF00463 DS    0H                                                  0466
*       MERGAREA=DEFAULTS;          /* SETUP DEFAULT VALUES          */
         MVI   MERGAREA+80,C' '                                    0466
         MVC   MERGAREA+81(127),MERGAREA+80                        0466
         MVC   MERGAREA(88),DEFAULTS                       ZP60007 0466
*       SAVEPTR=ADDR(SAVEMBR);      /* INITIALIZE PTR                */
         LA    SAVEPTR,SAVEMBR                                     0467
*       J=(LENGTH(SAVEMBR))/8;      /* NUMBER OF PARMS               */
         LA    J,26                                                0468
*       DO I=1 TO J;                /* MOVE IN MBR PARMS             */
         LA    I,1                                                 0469
         B     @DE00469                                            0469
@DL00469 DS    0H                                                  0470
*         IF SAVECHAR^=HEXFF&SAVECHAR^=HEXBC THEN                  0470
         CLI   SAVECHAR(SAVEPTR),X'FF'                             0470
         BE    @RF00470                                            0470
         CLI   SAVECHAR(SAVEPTR),X'BC'                             0470
         BE    @RF00470                                            0470
*           MERGE(I)=SAVAREA;                                      0471
         LR    @05,I                                               0471
         SLA   @05,3                                               0471
         LA    @03,MERGE-8(@05)                                    0471
         MVC   0(8,@03),SAVAREA(SAVEPTR)                           0471
*         SAVEPTR=SAVEPTR+8;                                       0472
@RF00470 AL    SAVEPTR,@CF00034                                    0472
*       END;                                                       0473
         AL    I,@CF00057                                          0473
@DE00469 CR    I,J                                                 0473
         BNH   @DL00469                                            0473
*     END;                                                         0474
*   IF MLPMUZ='1'B MLPMUNZ='1'B THEN/* OPERATOR ENTER USERMAX=       */
@RC00463 TM    MLPMUZ(MLPMPTR),B'00011000'                         0475
         BZ    @RF00475                                            0475
*     DO;                           /* YES                           */
*       KEYWDNM='USERMAX ';         /* KEYWORD NAME WANTED           */
         MVC   KEYWDNM(8),@CC00350                                 0477
*       TABLEPTR=ADDR(VERIBUFR);    /* KEYWORD TABLE                 */
         LA    TABLEPTR,VERIBUFR                                   0478
*       DO WHILE(TBLNAME^=KEYWDNM); /* FIND                          */
         B     @DE00479                                            0479
@DL00479 DS    0H                                                  0480
*         TABLEPTR=TABLEPTR+20;     /* PROPER                        */
         AL    TABLEPTR,@CF00144                                   0480
*       END;                        /* ENTRY                         */
@DE00479 CLC   TBLNAME(8,TABLEPTR),KEYWDNM                         0481
         BNE   @DL00479                                            0481
*       SAVEPTR=ADDR(SAVEBUFR)+8*(TBLDISP-1);/* USERMAX SAVE AREA    */
         LA    SAVEPTR,SAVEBUFR                                    0482
         LH    @06,TBLDISP(,TABLEPTR)                              0482
         BCTR  @06,0                                               0482
         SLA   @06,3                                               0482
         ALR   SAVEPTR,@06                                         0482
*       TABLEPTR=ADDR(VERIMBR);     /* KEYWORD TABLE                 */
         LA    TABLEPTR,VERIMBR                                    0483
*       DO WHILE(TBLNAME^=KEYWDNM); /* FIND                          */
         B     @DE00484                                            0484
@DL00484 DS    0H                                                  0485
*         TABLEPTR=TABLEPTR+20;     /* PROPER                        */
         AL    TABLEPTR,@CF00144                                   0485
*       END;                        /* ENTRY                         */
@DE00484 CLC   TBLNAME(8,TABLEPTR),KEYWDNM                         0486
         BNE   @DL00484                                            0486
*       MERGE(TBLDISP)=SAVAREA;     /* MOVE IN OPERATOR'S VALUE      */
         LH    @06,TBLDISP(,TABLEPTR)                              0487
         SLA   @06,3                                               0487
         LA    @04,MERGE-8(@06)                                    0487
         MVC   0(8,@04),SAVAREA(SAVEPTR)                           0487
*     END;                                                         0488
*   END OVERLAY;                                                   0489
@EL00007 DS    0H                                                  0489
@EF00007 DS    0H                                                  0489
@ER00007 L     @14,12(,@13)                                        0489
         BR    @14                                                 0489
         EJECT
*TCASUPDT:                                                         0490
*   PROC OPTIONS(SAVE(14));                                        0490
TCASUPDT ST    @14,12(,@13)                                        0490
*                                                                  0491
*/*  *****************************************************************/
*/*                                                                  */
*/*    THE PURPOSE OF THIS SUBROUTINE IS TO UPDATE THE TCAS TABLE    */
*/*    WITH ANY VALUES THAT HAVE CHANGED.  NOTE THAT THE CODING IS   */
*/*    DEPENDENT ON THE ORDER OF THE KEYWORD NAMES AS SET UP IN      */
*/*    'VERIMBR'.                                                    */
*/*                                                                  */
*/*  *****************************************************************/
*                                                                  0491
*   IF MLPMUPTB='0'B THEN           /* UPDATE WANTED                 */
         TM    MLPMUPTB(MLPMPTR),B'00001000'                       0491
         BZ    @RT00491                                            0491
*     RETURN;                       /* NO - EXIT                     */
*   SAVEPTR=ADDR(MERGAREA);         /* SET UP PTR                    */
         LA    SAVEPTR,MERGAREA                                    0493
*   IF SAVECHAR^=HEXBC THEN         /* CHANGE 'USERMAX'              */
         CLI   SAVECHAR(SAVEPTR),X'BC'                             0494
         BE    @RF00494                                            0494
*     DO;                           /* YES                           */
*LOOP1:                             /* COMPARE AND SWAP LOOP         */
*       WRKPRMF1=TCASUSER;          /* CURRENT VALUE                 */
LOOP1    L     @06,TCASTPTR(,TWAPTR)                               0496
         L     @03,TCASUSER(,@06)                                  0496
         ST    @03,WRKPRMF1                                        0496
*       WRKPRMF2=WRKPRMF1;          /* VALUE TO WORK WITH            */
         ST    @03,WRKPRMF2                                        0497
*       WRKPRMH=SAVAREAH;           /* NEW 'USERMAX' VALUE           */
         LH    @15,SAVAREAH(,SAVEPTR)                              0498
         STH   @15,WRKPRMH                                         0498
*       CS(WRKPRMF1,WRKPRMF2,TCASUSER);/* 'USERMAX' STILL OK         */
         L     @15,WRKPRMF2                                        0499
         CS    @03,@15,TCASUSER(@06)                               0499
         BZ    @BC00499                                            0499
         ST    @03,WRKPRMF1                                        0499
@BC00499 DS    0H                                                  0500
*       BC(7,LOOP1);                /* NO - TRY AGAIN                */
         BC    7,LOOP1                                             0500
*     END;                                                         0501
*   SAVEPTR=SAVEPTR+8;              /* POINT TO NEXT ENTRY           */
@RF00494 AL    SAVEPTR,@CF00034                                    0502
*   IF SAVECHAR^=HEXBC THEN         /* CHANGE 'ACBPW'                */
         CLI   SAVECHAR(SAVEPTR),X'BC'                             0503
         BE    @RF00503                                            0503
*     TCASACBP=SAVAREA;             /* YES                           */
         L     @06,TCASTPTR(,TWAPTR)                               0504
         MVC   TCASACBP(8,@06),SAVAREA(SAVEPTR)                    0504
*   SAVEPTR=SAVEPTR+8;              /* POINT TO NEXT ENTRY           */
@RF00503 AL    SAVEPTR,@CF00034                                    0505
*   IF SAVECHAR^=HEXBC THEN         /* CHANGE 'RECONLIM'             */
         CLI   SAVECHAR(SAVEPTR),X'BC'                             0506
         BE    @RF00506                                            0506
*     TCASRCON=SAVAREAH;            /* YES                           */
         L     @06,TCASTPTR(,TWAPTR)                               0507
         LH    @03,SAVAREAH(,SAVEPTR)                              0507
         STH   @03,TCASRCON(,@06)                                  0507
*   SAVEPTR=SAVEPTR+8;              /* POINT TO NEXT ENTRY           */
@RF00506 AL    SAVEPTR,@CF00034                                    0508
*   IF SAVECHAR^=HEXBC THEN         /* CHANGE 'BUFRSIZE'             */
         CLI   SAVECHAR(SAVEPTR),X'BC'                             0509
         BE    @RF00509                                            0509
*     TCASCLSZ=SAVAREAH;            /* YES                           */
         L     @06,TCASTPTR(,TWAPTR)                               0510
         LH    @03,SAVAREAH(,SAVEPTR)                              0510
         STH   @03,TCASCLSZ(,@06)                                  0510
*   SAVEPTR=SAVEPTR+8;              /* POINT TO NEXT ENTRY           */
@RF00509 AL    SAVEPTR,@CF00034                                    0511
*   IF SAVECHAR^=HEXBC THEN         /* CHANGE 'HIBFREXT'             */
         CLI   SAVECHAR(SAVEPTR),X'BC'                             0512
         BE    @RF00512                                            0512
*     TCASHBUF=SAVAREAF;            /* YES                           */
         L     @06,TCASTPTR(,TWAPTR)                               0513
         L     @03,SAVAREAF(,SAVEPTR)                              0513
         ST    @03,TCASHBUF(,@06)                                  0513
*   SAVEPTR=SAVEPTR+8;              /* POINT TO NEXT ENTRY           */
@RF00512 AL    SAVEPTR,@CF00034                                    0514
*   IF SAVECHAR^=HEXBC THEN         /* CHANGE 'LOBFREXT'             */
         CLI   SAVECHAR(SAVEPTR),X'BC'                             0515
         BE    @RF00515                                            0515
*     TCASLBUF=SAVAREAF;            /* YES                           */
         L     @06,TCASTPTR(,TWAPTR)                               0516
         L     @03,SAVAREAF(,SAVEPTR)                              0516
         ST    @03,TCASLBUF(,@06)                                  0516
*   SAVEPTR=SAVEPTR+8;              /* POINT TO NEXT ENTRY           */
@RF00515 AL    SAVEPTR,@CF00034                                    0517
*   IF SAVECHAR^=HEXBC THEN         /* CHANGE 'SCRSIZE'              */
         CLI   SAVECHAR(SAVEPTR),X'BC'                             0518
         BE    @RF00518                                            0518
*     TCASCRSZ=SAVAREAH;            /* YES                           */
         L     @06,TCASTPTR(,TWAPTR)                               0519
         LH    @03,SAVAREAH(,SAVEPTR)                              0519
         STH   @03,TCASCRSZ(,@06)                                  0519
*   SAVEPTR=SAVEPTR+8;              /* POINT TO NEXT ENTRY           */
@RF00518 AL    SAVEPTR,@CF00034                                    0520
*   IF SAVECHAR^=HEXBC THEN         /* CHANGE 'CHNLEN'               */
         CLI   SAVECHAR(SAVEPTR),X'BC'                             0521
         BE    @RF00521                                            0521
*     TCASCHNL=SAVAREAC;            /* YES                           */
         L     @06,TCASTPTR(,TWAPTR)                               0522
         IC    @03,SAVAREAC(,SAVEPTR)                              0522
         STC   @03,TCASCHNL(,@06)                                  0522
*   SAVEPTR=SAVEPTR+8;              /* PT TO NEXT ENTY       @G58AK3A*/
@RF00521 AL    SAVEPTR,@CF00034                                    0523
*   IF SAVECHAR^=HEXBC THEN         /* CHANGE 'MODE'         @G58AK3A*/
         CLI   SAVECHAR(SAVEPTR),X'BC'                             0524
         BE    @RF00524                                            0524
*     IF SAVAREA=BREAK THEN         /*                       @G58AK3A*/
         CLC   SAVAREA(8,SAVEPTR),BREAK                            0525
         BNE   @RF00525                                            0525
*       TCASBKMD='1'B;              /* YES                   @G58AK3A*/
         L     @06,TCASTPTR(,TWAPTR)                               0526
         OI    TCASBKMD(@06),B'10000000'                           0526
*     ELSE                                                         0527
*       ;                           /*                       @G58AK3A*/
@RF00525 DS    0H                                                  0528
*   ELSE                                                           0528
*     ;                             /*                       @G58AK3A*/
@RF00524 DS    0H                                                  0529
*   SAVEPTR=SAVEPTR+8;              /* PT TO NEXT ENTY       @G58AK3A*/
         AL    SAVEPTR,@CF00034                                    0529
*   IF SAVECHAR^=HEXBC THEN         /* CHANGE 'MODESW'       @G58AK3A*/
         CLI   SAVECHAR(SAVEPTR),X'BC'                             0530
         BE    @RF00530                                            0530
*     IF SAVAREA=YES THEN           /*                       @G58AK3A*/
         CLC   SAVAREA(8,SAVEPTR),YES                              0531
         BNE   @RF00531                                            0531
*       TCASMDSW='1'B;              /* YES                   @G58AK3A*/
         L     @06,TCASTPTR(,TWAPTR)                               0532
         OI    TCASMDSW(@06),B'01000000'                           0532
*     ELSE                                                         0533
*       ;                           /*                       @G58AK3A*/
@RF00531 DS    0H                                                  0534
*   ELSE                                                           0534
*     ;                             /*                       @G58AK3A*/
@RF00530 DS    0H                                                  0535
*   SAVEPTR=SAVEPTR+8;              /* PT TO NEXT ENTY       @ZM20856*/
         AL    SAVEPTR,@CF00034                                    0535
*                                      CHANGE 'CONFTXT'         ZP60007
         CLC   SAVAREA(8,SAVEPTR),NO  WAS NO SPECIFIED?         ZP60007
         BE    CFTXOKAY               YES, LEAVE FLAG OFF       ZP60007
         L     @06,TCASTPTR(,TWAPTR)  POINT TO THE TCAS TABLE   ZP60007
         OI    TCASCONF(@06),X'01'    YES, SET FLAG ON          ZP60007
CFTXOKAY AL    SAVEPTR,@CF00034       POINT TO NEXT ENTRY       ZP60007
*   IF SAVECHAR^=HEXBC THEN         /* CHANGE RCFBDUMP       @ZM20856*/
         CLI   SAVECHAR(SAVEPTR),X'BC'                             0536
         BE    @RF00536                                            0536
*     DO;                           /* UPDATE RC TABLE       @ZM20856*/
*       DUMPPTR=(TCASDUMP+3);       /* INCREMENT PAST COUNT AND    0538
*                                      RESERVED FIELDS       @ZM20856*/
         L     @06,TCASTPTR(,TWAPTR)                               0538
         LA    @03,3                                               0538
         AL    @03,TCASDUMP(,@06)                                  0538
         ST    @03,DUMPPTR                                         0538
*       RFY                                                        0539
*         DMPCD BASED(DUMPPTR);     /* RESPECIFY PAST COUNT AND    0539
*                                      RESERVED              @ZM20856*/
*       DO I=1 TO DUMPCNT;          /*                       @ZM20856*/
         LA    I,1                                                 0540
         B     @DE00540                                            0540
@DL00540 DS    0H                                                  0541
*         DMPCD(1:3)=SAVAREAI;      /* UPDATE CODE TABL      @ZM20856*/
         L     @03,DUMPPTR                                         0541
         MVC   DMPCD(3,@03),SAVAREAI(SAVEPTR)                      0541
*         DUMPPTR=DUMPPTR+3;        /* NEXT TABLE ENTY       @ZM20856*/
         AL    @03,@CF00118                                        0542
         ST    @03,DUMPPTR                                         0542
*         SAVEPTR=SAVEPTR+8;        /* NEXT SAVE ENTRY       @ZM20856*/
         AL    SAVEPTR,@CF00034                                    0543
*       END;                        /* END PROCESS           @ZM20856*/
         AL    I,@CF00057                                          0544
@DE00540 CH    I,DUMPCNT                                           0544
         BNH   @DL00540                                            0544
*       RFY                                                        0545
*         DMPCD BASED(TCASDUMP);    /* RFY TO TOP TABL       @ZM20856*/
*       DMPRCCT=DUMPCNT;            /* TOTAL COUNT OF KEYWORD      0546
*                                      'RCFBDUMP' SPECFIED.NOTE:   0546
*                                      DUMPCNT WAS UPDATED IN PROC 0546
*                                      SPLITPRM              @ZM20856*/
         L     @06,TCASTPTR(,TWAPTR)                               0546
         L     @06,TCASDUMP(,@06)                                  0546
         LH    @15,DUMPCNT                                         0546
         STC   @15,DMPRCCT(,@06)                                   0546
*     END;                          /* END 'RCFBDUMP'        @ZM20856
*                                      UPDATING              @ZM20856*/
*   ELSE                                                           0548
*     ;                                                            0548
@RF00536 DS    0H                                                  0549
*   END TCASUPDT;                                                  0549
@EL00008 DS    0H                                                  0549
@EF00008 DS    0H                                                  0549
@ER00008 L     @14,12(,@13)                                        0549
         BR    @14                                                 0549
         EJECT
*WRITEOUT:                                                         0550
*   PROC OPTIONS(SAVE(14));                                        0550
WRITEOUT ST    @14,@SA00009                                        0550
*                                                                  0551
*/*  *****************************************************************/
*/*                                                                  */
*/*    THIS SUBROUTINE PRINTS OUT ALL OF THE PARMLIB VALUES FROM THE */
*/*    TCAS TABLE.  NOTE THAT THE CODING IS DEPENDENT ON THE ORDER   */
*/*    OF THE KEYWORD NAMES AS SET UP IN 'VERIMBR':                  */
*/*                                                                  */
*/*  *****************************************************************/
*                                                                  0551
*   IF PRINTSW='0'B THEN            /* PRINTOUT WANTED               */
         TM    PRINTSW,B'00001000'                                 0551
         BZ    @RT00551                                            0551
*     RETURN;                       /* NO - EXIT                     */
*   PRNTCC='-';                     /* SKIP                          */
         MVI   PRNTCC,C'-'                                         0553
*   PRNTDATA='';                    /* TWO                           */
         MVI   PRNTDATA,C' '                                       0554
         MVC   PRNTDATA+1(131),PRNTDATA                            0554
*   CALL PRINTIT;                   /* LINES                         */
         BAL   @14,PRINTIT                                         0555
*   PRNTDATA='CURRENT TCAS PARAMETERS:';/* SUB-HEADER LINE           */
         MVI   PRNTDATA+24,C' '                                    0556
         MVC   PRNTDATA+25(107),PRNTDATA+24                        0556
         MVC   PRNTDATA(24),@CC01585                               0556
*   CALL PRINTIT;                   /* PRINT THE SUB-HEADER          */
         BAL   @14,PRINTIT                                         0557
*   PRNTLINE='';                    /* CLEAR THE LINE                */
         MVI   PRNTLINE,C' '                                       0558
         MVC   PRNTLINE+1(132),PRNTLINE                            0558
*   TABLEPTR=ADDR(VERIMBR);         /* SETUP POINTER                 */
         LA    TABLEPTR,VERIMBR                                    0559
*   WORKPARM=''B;                   /* PRINTOUT                      */
         XC    WORKPARM(8),WORKPARM                                0560
*   WRKPRMH=TCASUMAX;               /* USERMAX=                      */
         L     @04,TCASTPTR(,TWAPTR)                               0561
         LH    @04,TCASUMAX(,@04)                                  0561
         STH   @04,WRKPRMH                                         0561
*   CALL EDITIT;                    /* VALUE                         */
         BAL   @14,EDITIT                                          0562
*   WORKPARM=TCASACBP;              /* PRINTOUT                      */
         L     @04,TCASTPTR(,TWAPTR)                               0563
         MVC   WORKPARM(8),TCASACBP(@04)                           0563
*   CALL EDITIT;                    /* ACBPW= VALUE                  */
         BAL   @14,EDITIT                                          0564
*   WORKPARM=''B;                   /* PRINTOUT                      */
         XC    WORKPARM(8),WORKPARM                                0565
*   WRKPRMH=TCASRCON;               /* RECONLIM=                     */
         L     @04,TCASTPTR(,TWAPTR)                               0566
         LH    @04,TCASRCON(,@04)                                  0566
         STH   @04,WRKPRMH                                         0566
*   CALL EDITIT;                    /* VALUE                         */
         BAL   @14,EDITIT                                          0567
*   WORKPARM=''B;                   /* PRINTOUT                      */
         XC    WORKPARM(8),WORKPARM                                0568
*   WRKPRMH=TCASCLSZ;               /* BUFRSIZE=                     */
         L     @04,TCASTPTR(,TWAPTR)                               0569
         LH    @04,TCASCLSZ(,@04)                                  0569
         STH   @04,WRKPRMH                                         0569
*   CALL EDITIT;                    /* VALUE                         */
         BAL   @14,EDITIT                                          0570
*   WORKPARM=''B;                   /* PRINTOUT                      */
         XC    WORKPARM(8),WORKPARM                                0571
*   WRKPRMF2=TCASHBUF;              /* HIBFREXT=                     */
         L     @04,TCASTPTR(,TWAPTR)                               0572
         L     @04,TCASHBUF(,@04)                                  0572
         ST    @04,WRKPRMF2                                        0572
*   CALL EDITIT;                    /* VALUE                         */
         BAL   @14,EDITIT                                          0573
*   WORKPARM=''B;                   /* PRINTOUT                      */
         XC    WORKPARM(8),WORKPARM                                0574
*   WRKPRMF2=TCASLBUF;              /* LOBFREXT=                     */
         L     @04,TCASTPTR(,TWAPTR)                               0575
         L     @04,TCASLBUF(,@04)                                  0575
         ST    @04,WRKPRMF2                                        0575
*   CALL EDITIT;                    /* VALUE                         */
         BAL   @14,EDITIT                                          0576
*   WORKPARM=''B;                   /* PRINTOUT                      */
         XC    WORKPARM(8),WORKPARM                                0577
*   WRKPRMH=TCASCRSZ;               /* SCRSIZE=                      */
         L     @04,TCASTPTR(,TWAPTR)                               0578
         LH    @04,TCASCRSZ(,@04)                                  0578
         STH   @04,WRKPRMH                                         0578
*   CALL EDITIT;                    /* VALUE                         */
         BAL   @14,EDITIT                                          0579
*   WORKPARM=''B;                   /* PRINTOUT                      */
         XC    WORKPARM(8),WORKPARM                                0580
*   WRKPRMC=TCASCHNL;               /* CHNLEN=                       */
         L     @04,TCASTPTR(,TWAPTR)                               0581
         IC    @02,TCASCHNL(,@04)                                  0581
         STC   @02,WRKPRMC                                         0581
*   CALL EDITIT;                    /* VALUE                         */
         BAL   @14,EDITIT                                          0582
*   IF TCASBKMD='1'B THEN           /* PRINTOUT              @G58AK3A*/
         L     @06,TCASTPTR(,TWAPTR)                               0583
         TM    TCASBKMD(@06),B'10000000'                           0583
         BNO   @RF00583                                            0583
*     WORKPARM=BREAK;               /* MODE=                 @G58AK3A*/
         MVC   WORKPARM(8),BREAK                                   0584
*   ELSE                            /* VALUE                 @G58AK3A*/
*     WORKPARM=NOBREAK;             /*                       @G58AK3A*/
         B     @RC00583                                            0585
@RF00583 MVC   WORKPARM(8),NOBREAK                                 0585
*   CALL EDITIT;                    /*                       @G58AK3A*/
@RC00583 BAL   @14,EDITIT                                          0586
*   IF TCASMDSW='1'B THEN           /* PRINTOUT              @G58AK3A*/
         L     @06,TCASTPTR(,TWAPTR)                               0587
         TM    TCASMDSW(@06),B'01000000'                           0587
         BNO   @RF00587                                            0587
*     WORKPARM=YES;                 /* MODESW=               @G58AK3A*/
         MVC   WORKPARM(8),YES                                     0588
*   ELSE                            /* VALUE                 @G58AK3A*/
*     WORKPARM=NO;                  /*                       @G58AK3A*/
         B     @RC00587                                            0589
@RF00587 MVC   WORKPARM(8),NO                                      0589
*   CALL EDITIT;                    /*                       @G58AK3A*/
@RC00587 BAL   @14,EDITIT                                          0590
*   IF TCASCONF='1'B THEN           /*                       ZP60007 */
         L     @06,TCASTPTR(,TWAPTR)                         ZP60007
         TM    TCASCONF(@06),B'00000001'                     ZP60007
         BNO   CONFOFF                                       ZP60007
*     WORKPARM=YES;                 /* CONFTXT=              ZP60007 */
         MVC   WORKPARM(8),YES                               ZP60007
*   ELSE                            /* VALUE                 ZP60007 */
*     WORKPARM=NO;                  /*                       ZP60007 */
         B     SHOWCONF                                      ZP60007
CONFOFF  MVC   WORKPARM(8),NO                                ZP60007
*   CALL EDITIT;                    /*                       ZP60007 */
SHOWCONF BAL   @14,EDITIT                                    ZP60007
*   DUMPPTR=(TCASDUMP+3);           /* SKIP TO CODES         @ZM20856*/
         L     @04,TCASTPTR(,TWAPTR)                               0591
         LA    @15,3                                               0591
         AL    @15,TCASDUMP(,@04)                                  0591
         ST    @15,DUMPPTR                                         0591
*   DO N=1 TO DUMPCNT;              /* EDIT PARAMETER FOR EACH VALID
*                                      'RCFBDUMP' KEYWORD SPECIFIED
*                                                            @ZM20856*/
         LA    @04,1                                               0592
         B     @DE00592                                            0592
@DL00592 DS    0H                                                  0593
*     CALL EDITIT;                  /* EDIT KEYWORDS         @ZM20856*/
         BAL   @14,EDITIT                                          0593
*     DUMPPTR=DUMPPTR+3;            /* NEXT ENTRY            @ZM20856*/
         LA    @04,3                                               0594
         AL    @04,DUMPPTR                                         0594
         ST    @04,DUMPPTR                                         0594
*   END;                            /*                       @ZM20856*/
         LA    @04,1                                               0595
         AH    @04,N                                               0595
@DE00592 STH   @04,N                                               0595
         CH    @04,DUMPCNT                                         0595
         BNH   @DL00592                                            0595
*   EDFIELD(1)=HEXFF;               /* CAUSE LAST LINE               */
         MVI   EDFIELD,X'FF'                                       0596
*   CALL NSRTNPRT;                  /* TO BE PRINTED                 */
         BAL   @14,NSRTNPRT                                        0597
*   GEN(CLOSE PRINTOUT)                                            0598
*   REFS(R0,R1,R14,R15,PRINTOUT);                                  0598
         CLOSE PRINTOUT
*   END WRITEOUT;                                                  0599
@EL00009 DS    0H                                                  0599
@EF00009 DS    0H                                                  0599
@ER00009 L     @14,@SA00009                                        0599
         BR    @14                                                 0599
         EJECT
*EDITIT:                                                           0600
*   PROC OPTIONS(SAVE(14));                                        0600
EDITIT   ST    @14,@SA00010                                        0600
*                                                                  0601
*/*  *****************************************************************/
*/*                                                                  */
*/*    THIS SUBROUTINE TAKES A KEYWORD NAME, ITS VALUE, PLACES THEM  */
*/*    INTO AN EDITTED FORMAT, AND PRINTS THEM.  UPON ENTRY,         */
*/*    'TABLEPTR' POINTS AT THE TABLE ENTRY, AND 'WORKPARM' CONTAINS */
*/*    THE PARAMETER VALUE, WITH THE EXCEPTION OF THE RCFBDUMP       */
*/*    KEYWORD, WHICH SETS UP 'WORKPARM' HERE.                       */
*/*  *****************************************************************/
*                                                                  0601
*   EDITFLD='';                     /* CLEAR WORK FIELD              */
         MVI   EDITFLD,C' '                                        0601
         MVC   EDITFLD+1(19),EDITFLD                               0601
*   SCANPTR=TABLEPTR;               /* SETUP PTR                     */
         LR    SCANPTR,TABLEPTR                                    0602
*   DO I=1 TO 8 WHILE(WORKCHAR^=' ');/* MOVE IN KEYWORD NAME         */
         LA    I,1                                                 0603
@DL00603 CLI   WORKCHAR(SCANPTR),C' '                              0603
         BE    @DC00603                                            0603
*     EDFIELD(I)=WORKCHAR;                                         0604
         LA    @04,EDFIELD-1(I)                                    0604
         MVC   0(1,@04),WORKCHAR(SCANPTR)                          0604
*     SCANPTR=SCANPTR+1;                                           0605
         AL    SCANPTR,@CF00057                                    0605
*   END;                                                           0606
         AL    I,@CF00057                                          0606
         C     I,@CF00034                                          0606
         BNH   @DL00603                                            0606
@DC00603 DS    0H                                                  0607
*   EDFIELD(I)='=';                 /* ADD '=' SIGN                  */
         LA    @04,EDFIELD-1(I)                                    0607
         MVI   0(@04),C'='                                         0607
*   I=I+1;                          /* POINT PAST '=' SIGN           */
         LA    @04,1                                               0608
         ALR   I,@04                                               0608
*   IF TBLNAME=RCFBDUMP THEN        /* 'RCFBDUMP'            @ZM20856*/
         CLC   TBLNAME(8,TABLEPTR),RCFBDUMP                        0609
         BNE   @RF00609                                            0609
*     DO;                           /* PROCESS PARM          @ZM20856*/
*       RFY                                                        0611
*         DMPCD BASED(DUMPPTR);                                    0611
*       WRKPRMF=''B;                /* ZERO WORK FIELD       @ZM20856*/
         XC    WRKPRMF(8),WRKPRMF                                  0612
*       WORKPARM(8:8)=DMPCD(1:1);   /* EXTRACT RC            @ZM20856*/
         L     @02,DUMPPTR                                         0613
         MVC   WORKPARM+7(1),DMPCD(@02)                            0613
*       TEMPAREA(8)=WORKPARM(8);    /* SAVE RC               @ZM20856*/
         MVC   TEMPAREA+7(1),WORKPARM+7                            0614
*       SLDL(TEMPAREA,4);           /* SHIFT RC TO PROPER POSITION 0615
*                                                            @ZM21059*/
         LM    @00,@01,TEMPAREA                                    0615
         SLDL  @00,4                                               0615
         STM   @00,@01,TEMPAREA                                    0615
*       WORKPARM(8:8)=WORKPARM(8:8)&'0F'X;/* ELIMINATE ZONED PORTION
*                                      OF BYTE RC            @ZM20856*/
         NI    WORKPARM+7,X'0F'                                    0616
*       TEMPAREA(7:8)=TEMPAREA(7:8)&'0F00'X;/* ELIMINATE DIGIT     0617
*                                      PORTION OF BYTE RC    @ZM21059*/
         NC    TEMPAREA+6(2),@CB01595                              0617
*       WRKPRMF=WRKPRMF TEMPAREA;   /* MERGE BYTES           @ZM20856*/
         OC    WRKPRMF(8),TEMPAREA                                 0618
*       TR(WORKPARM(7:8),TABDEC);   /* TO PRINTABLES         @ZM20856*/
         TR    WORKPARM+6(2),TABDEC                                0619
*       EDITFLD(I:(I+1))=WORKPARM(7:8);/* TO EDIT FIELD      @ZM21059*/
         LA    @01,EDITFLD-1(I)                                    0620
         MVC   0(2,@01),WORKPARM+6                                 0620
*       I=I+2;                      /* UPDATE 2 BYTES        @ZM20856*/
         LA    @15,2                                               0621
         ALR   I,@15                                               0621
*       WRKPRMF=''B;                /* ZERO WORK FIELD       @ZM20856*/
         XC    WRKPRMF(8),WRKPRMF                                  0622
*       WORKPARM(8:8)=DMPCD(2:2);   /* EXTRACT FB            @ZM20856*/
         MVC   WORKPARM+7(1),DMPCD+1(@02)                          0623
*       TEMPAREA(8)=WORKPARM(8);    /* SAVE FB               @ZM20856*/
         MVC   TEMPAREA+7(1),WORKPARM+7                            0624
*       SLDL(TEMPAREA,4);           /* SHIFT FB TO PROPER POSITION 0625
*                                                            @ZM21059*/
         LM    @00,@01,TEMPAREA                                    0625
         SLDL  @00,4                                               0625
         STM   @00,@01,TEMPAREA                                    0625
*       WORKPARM(8:8)=WORKPARM(8:8)&'0F'X;/* ELIMINATE ZONED PORTION
*                                      OF BYTE FB            @ZM20856*/
         NI    WORKPARM+7,X'0F'                                    0626
*       TEMPAREA(7:8)=TEMPAREA(7:8)&'0F00'X;/* ELIMINATE DIGIT     0627
*                                      PORTION OF BYTE FB    @ZM21059*/
         NC    TEMPAREA+6(2),@CB01595                              0627
*       WRKPRMF=WRKPRMF TEMPAREA;   /* MERGE BYTES           @ZM20856*/
         OC    WRKPRMF(8),TEMPAREA                                 0628
*       TR(WORKPARM(7:8),TABDEC);   /* TO PRINTABLES         @ZM20856*/
         TR    WORKPARM+6(2),TABDEC                                0629
*       EDITFLD(I:(I+1))=WORKPARM(7:8);/* TO EDIT FIELD      @ZM21059*/
         LA    @01,EDITFLD-1(I)                                    0630
         MVC   0(2,@01),WORKPARM+6                                 0630
*       I=I+2;                      /* UPDATE 2 BYTES        @ZM20856*/
         ALR   I,@15                                               0631
*       WRKPRMF=''B;                /* ZERO WORK FIELD       @ZM20856*/
         XC    WRKPRMF(8),WRKPRMF                                  0632
*       WORKPARM(8:8)=DMPCD(3:3);   /* EXTRACT SDUMP         @ZM20856*/
         MVC   WORKPARM+7(1),DMPCD+2(@02)                          0633
*       CVD(WRKPRMF2,TEMPAREA);     /* TO DECIMAL            @ZM20856*/
         L     @02,WRKPRMF2                                        0634
         CVD   @02,TEMPAREA                                        0634
*       UNPK(WORKPARM,TEMPAREA(8)); /* UNPACK                @ZM20856*/
         UNPK  WORKPARM(8),TEMPAREA+7(1)                           0635
*       WORKPARM(8)=WORKPARM(8) 'F0'X;/* REMOVE SIGN         @ZM20856*/
         OI    WORKPARM+7,X'F0'                                    0636
*       EDFIELD(I)=WORKPARM(8);     /* TO EDIT FIELD         @ZM20856*/
         LA    @02,EDFIELD-1(I)                                    0637
         MVC   0(1,@02),WORKPARM+7                                 0637
*       I=I+1;                      /* UPDATE 1 BYTES        @ZM20856*/
         ALR   I,@04                                               0638
*       RFY                                                        0639
*         DMPCD BASED(TCASDUMP);    /*                       @ZM20856*/
*     END;                          /* END RCFBDUMP          @ZM20856*/
*   ELSE                                                           0641
*     DO;                           /* OTHER KEYWORDS        @ZM20856*/
         B     @RC00609                                            0641
@RF00609 DS    0H                                                  0642
*       IF TBLBINRY='1'B THEN       /* PARM VALUE A NUMBER           */
         TM    TBLBINRY(TABLEPTR),B'00100000'                      0642
         BNO   @RF00642                                            0642
*         CALL PARMMOV;             /* PROCESS ALL OTHER KEYWORDS  0643
*                                                            @ZM20856*/
         BAL   @14,PARMMOV                                         0643
*       ELSE                                                       0644
*         DO;                       /* NO - ALPHANUMERIC             */
         B     @RC00642                                            0644
@RF00642 DS    0H                                                  0645
*           SCANPTR=ADDR(WORKPARM); /* SETUP PTR                     */
         LA    SCANPTR,WORKPARM                                    0645
*           DO J=1 TO 8 WHILE(WORKCHAR^=' ');/* MOVE IN VALUE        */
         LA    J,1                                                 0646
@DL00646 CLI   WORKCHAR(SCANPTR),C' '                              0646
         BE    @DC00646                                            0646
*             EDFIELD(I)=WORKCHAR;                                 0647
         LA    @02,EDFIELD-1(I)                                    0647
         MVC   0(1,@02),WORKCHAR(SCANPTR)                          0647
*             I=I+1;                                               0648
         LA    @02,1                                               0648
         ALR   I,@02                                               0648
*             SCANPTR=SCANPTR+1;                                   0649
         ALR   SCANPTR,@02                                         0649
*           END;                                                   0650
         AL    J,@CF00057                                          0650
         C     J,@CF00034                                          0650
         BNH   @DL00646                                            0650
@DC00646 DS    0H                                                  0651
*         END;                                                     0651
*     END;                          /*                       @ZM20856*/
@RC00642 DS    0H                                                  0653
*   I=I-1;                          /* LENGTH OF EDITTED DATA        */
@RC00609 BCTR  I,0                                                 0653
*   CALL NSRTNPRT;                  /* INSERT & PRINT VALUE          */
         BAL   @14,NSRTNPRT                                        0654
*   TABLEPTR=TABLEPTR+20;           /* PT AT NEXT TABLE ENTRY        */
         AL    TABLEPTR,@CF00144                                   0655
*   END EDITIT;                                                    0656
@EL00010 DS    0H                                                  0656
@EF00010 DS    0H                                                  0656
@ER00010 L     @14,@SA00010                                        0656
         BR    @14                                                 0656
         EJECT
*NSRTNPRT:                                                         0657
*   PROC OPTIONS(SAVE(14));                                        0657
NSRTNPRT ST    @14,@SA00011                                        0657
*                                                                  0658
*/*  *****************************************************************/
*/*                                                                  */
*/*    THIS SUBROUTINE BUILDS UP A PRINT LINE FROM DATA FOUND IN     */
*/*    'EDITFLD', AND WHEN THE PRINT LINE IS FULL, PRINTS IT.        */
*/*                                                                  */
*/*  *****************************************************************/
*                                                                  0658
*   IF EDFIELD(1)=HEXFF THEN        /* PRINT LAST LINE               */
         CLI   EDFIELD,X'FF'                                       0658
         BNE   @RF00658                                            0658
*     DO;                           /* YES                           */
*       CALL PRINTIT;               /* PRINT IT                      */
         BAL   @14,PRINTIT                                         0660
*       PRNTDAT5='';                /* BLANK OUT LINE                */
         MVI   PRNTDAT5,C' '                                       0661
         MVC   PRNTDAT5+1(127),PRNTDAT5                            0661
*       PRNTPTR=ADDR(PRNTDAT5);     /* RE-SET PTR                    */
         LA    PRNTPTR,PRNTDAT5                                    0662
*     END;                                                         0663
*   ELSE                                                           0664
*     DO;                                                          0664
         B     @RC00658                                            0664
@RF00658 DS    0H                                                  0665
*       IF PRNTPTR>ADDR(PRNTDAT5) THEN/* VALUE ALREADY THERE         */
         LA    @04,PRNTDAT5                                        0665
         CR    PRNTPTR,@04                                         0665
         BNH   @RF00665                                            0665
*         DO;                       /* YES                           */
*           PRNTCHAR=',';           /* PUT COMMA AFTER IT            */
         MVI   PRNTCHAR(PRNTPTR),C','                              0667
*           PRNTPTR=PRNTPTR+1;      /* PT PAST COMMA                 */
         AL    PRNTPTR,@CF00057                                    0668
*         END;                                                     0669
*       IF(PRNTPTR+I)>PRNTEND THEN  /* ROOM LEFT ON PRINT LINE       */
@RF00665 LR    @04,PRNTPTR                                         0670
         ALR   @04,I                                               0670
         C     @04,PRNTEND                                         0670
         BNH   @RF00670                                            0670
*         DO;                       /* NO                            */
*           CALL PRINTIT;           /* PRINT IT                      */
         BAL   @14,PRINTIT                                         0672
*           PRNTDAT5='';            /* BLANK OUT LINE                */
         MVI   PRNTDAT5,C' '                                       0673
         MVC   PRNTDAT5+1(127),PRNTDAT5                            0673
*           PRNTPTR=ADDR(PRNTDAT5); /* RE-SET PTR                    */
         LA    PRNTPTR,PRNTDAT5                                    0674
*         END;                                                     0675
*       DO J=1 TO I;                /* MOVE EDITTED                  */
@RF00670 LA    J,1                                                 0676
         B     @DE00676                                            0676
@DL00676 DS    0H                                                  0677
*         PRNTCHAR=EDFIELD(J);      /* DATA                          */
         LA    @02,EDFIELD-1(J)                                    0677
         MVC   PRNTCHAR(1,PRNTPTR),0(@02)                          0677
*         PRNTPTR=PRNTPTR+1;        /* INTO                          */
         AL    PRNTPTR,@CF00057                                    0678
*       END;                        /* PRINT LINE                    */
         AL    J,@CF00057                                          0679
@DE00676 CR    J,I                                                 0679
         BNH   @DL00676                                            0679
*     END;                                                         0680
*   END NSRTNPRT;                                                  0681
@EL00011 DS    0H                                                  0681
@EF00011 DS    0H                                                  0681
@ER00011 L     @14,@SA00011                                        0681
         BR    @14                                                 0681
         EJECT
*PRINTIT:                                                          0682
*   PROC OPTIONS(SAVE(14));                                        0682
PRINTIT  ST    @14,@SA00012                                        0682
*                                                                  0683
*/*  *****************************************************************/
*/*                                                                  */
*/*    THIS SUBROUTINE PRINTS A LINE OF DATA.  IN ADDITION, IF A     */
*/*    SYNAD ERROR IS DETECTED, THE 'PRINTSW' IS TURNED OFF AND THE  */
*/*    FILE IS CLOSED.                                               */
*/*                                                                  */
*/*  *****************************************************************/
*                                                                  0683
*   IF PRINTSW='0'B THEN            /* PRINTOUT WANTED               */
         TM    PRINTSW,B'00001000'                                 0683
         BZ    @RT00683                                            0683
*     RETURN;                       /* NO - EXIT                     */
*   GEN(PUT   PRINTOUT,PRNTLINE)                                   0685
*   REFS(R0,R1,R14,R15,PRINTOUT,PRNTLINE);                         0685
         PUT   PRINTOUT,PRNTLINE
*   IF SYNADSW2='Y' THEN            /* SYNAD ERROR                   */
         CLI   SYNADSW2,C'Y'                                       0686
         BNE   @RF00686                                            0686
*     DO;                           /* YES                           */
*       RESPECIFY                                                  0688
*        (R1) RESTRICTED;                                          0688
*       R1=18;                      /* MESSAGE NUMBER                */
         LA    R1,18                                               0689
*       CALL MSGBLK;                /* ADDR OF LIST FORM OF MSG      */
         L     @15,TWAMSG(,TWAPTR)                                 0690
         BALR  @14,@15                                             0690
*       R1=ADDR(IKT018I);           /* ADDR OF LIST FORM             */
         LA    @04,IKT018I(,R1)                                    0691
         LR    R1,@04                                              0691
*       GEN(WTO   MF=(E,(1)))       /* ISSUE WTO                     */
*       REFS(R0,R1,R14,R15);                                       0692
         WTO   MF=(E,(1))
*       RESPECIFY                                                  0693
*        (R1) UNRESTRICTED;                                        0693
*       PRINTSW='0'B;               /* TURN OFF PRINT SW             */
         NI    PRINTSW,B'11110111'                                 0694
*       GEN(CLOSE PRINTOUT)         /* CLOSE FILE                    */
*       REFS(R0,R1,R14,R15,PRINTOUT);                              0695
         CLOSE PRINTOUT
*     END;                                                         0696
*   END PRINTIT;                                                   0697
@EL00012 DS    0H                                                  0697
@EF00012 DS    0H                                                  0697
@ER00012 L     @14,@SA00012                                        0697
         BR    @14                                                 0697
         EJECT
*                                                                  0698
*/*  *****************************************************************/
*/*                                                                  */
*/*    IN ORDER TO MAKE PARAMETER VALIDATION AS INDEPENDENT AS       */
*/*    POSSIBLE, THE ROUTINES WHICH DO THIS ARE PRIMARILY            */
*/*    TABLE-DRIVEN.  THE MAIN TABLES WHICH ACCOMPLISH THIS ARE      */
*/*    CALLED VERIFY TABLES ('VERIBUFR', 'VERIMBR', ETC.).  THEY     */
*/*    CONTAIN THE CONTROL INFORMATION REGARDING EACH KEYWORD.       */
*/*                                                                  */
*/*    AN ELEMENT IN A VERIFY TABLE IS ORGANIZED AS FOLLOWS (SEE     */
*/*    'SCANTBL'):                                                   */
*/*    -   KEYWORD NAME                                              */
*/*    -   DISPLACEMENT INDEX INTO A SAVE AREA WHERE THE VALUE       */
*/*        ENTERED WITH THE KEYWORD NAME IS SAVED.                   */
*/*    -   FLAGS, WHICH SHOW WHAT KIND OF EDITTING TO DO ON THE      */
*/*        ENTERED VALUE.                                            */
*/*    -   IF THE ENTERED VALUE HAS TO BE ONE OF A SET OF VALUES, A  */
*/*        POINTER TO A LIST OF THE VALUES,                          */
*/*      OR                                                          */
*/*        A LOW NUMBER AND A HIGH NUMBER USED FOR A RANGE CHECK.    */
*/*                                                                  */
*/*    THE SUBROUTINES WHICH ACTUALLY DO THE VALIDATION ARE          */
*/*    'FNDNMTCH', 'PRCSPARM', 'NBRXTRCT', AND 'REGXTRCT'.           */
*/*                                                                  */
*/*  *****************************************************************/
*                                                                  0698
         EJECT
*FNDNMTCH:                                                         0698
*   PROC OPTIONS(SAVE(14));                                        0698
FNDNMTCH ST    @14,12(,@13)                                        0698
*                                                                  0699
*/*  *****************************************************************/
*/*                                                                  */
*/*    THIS SUBROUTINE HAS TWO FUNCTIONS: 1) SCAN A BUFFER LOOKING   */
*/*    FOR A KEYWORD NAME, AND 2) MATCH THAT KEYWORD NAME AGAINST THE*/
*/*    ENTRIES IN A TABLE.  UPON ENTRY, 'SCANPTR' SHOULD POINT AT THE*/
*/*    BUFFER TO BE SCANNED, AND 'TABLEPTR' SHOULD POINT AT THE      */
*/*    TABLE USED FOR MATCHING.  UPON EXIT, 'SCANPTR' WILL POINT AT  */
*/*    THE FIRST CHARACTER PAST THE '=' SIGN, 'WORKPARM' WILL        */
*/*    CONTAIN THE KEYWORD NAME LEFT-JUSTIFIED AND BLANK-FILLED, AND */
*/*    'TABLEPTR' WILL POINT AT EITHER THE MATCHING TABLE ENTRY, OR  */
*/*    THE END OF THE TABLE IF NO MATCH WAS FOUND.                   */
*/*                                                                  */
*/*  *****************************************************************/
*                                                                  0699
*   WORKPARM=BLANK;                 /* BLANK OUT WORK AREA           */
         MVC   WORKPARM(8),@CC00097                                0699
*   I=0;                            /* ZERO OUT COUNTER MOVE KEYWORD
*                                      NAME INTO WORK AREA, AND ALSO
*                                      CHECK ITS LENGTH              */
         SLR   I,I                                                 0700
*   DO WHILE(WORKCHAR^='='&WORKCHAR^=' '&WORKCHAR^=',');           0701
         B     @DE00701                                            0701
@DL00701 DS    0H                                                  0702
*     I=I+1;                                                       0702
         AL    I,@CF00057                                          0702
*     IF I<9 THEN                                                  0703
         C     I,@CF00069                                          0703
         BNL   @RF00703                                            0703
*       WORKPRM(I)=WORKCHAR;                                       0704
         LA    @04,WORKPRM-1(I)                                    0704
         MVC   0(1,@04),WORKCHAR(SCANPTR)                          0704
*     SCANPTR=SCANPTR+1;                                           0705
@RF00703 AL    SCANPTR,@CF00057                                    0705
*   END;                                                           0706
@DE00701 CLI   WORKCHAR(SCANPTR),C'='                              0706
         BE    @DC00701                                            0706
         CLI   WORKCHAR(SCANPTR),C' '                              0706
         BE    @DC00701                                            0706
         CLI   WORKCHAR(SCANPTR),C','                              0706
         BNE   @DL00701                                            0706
@DC00701 DS    0H                                                  0707
*   IF I=0                          /* ERROR IN                      */
*       I>8                         /* LENGTH OR                     */
*       WORKCHAR=' '                /* ENDING                        */
*       WORKCHAR=',' THEN           /* CHARACTER                     */
         LTR   I,I                                                 0707
         BZ    @RT00707                                            0707
         C     I,@CF00034                                          0707
         BH    @RT00707                                            0707
         CLI   WORKCHAR(SCANPTR),C' '                              0707
         BE    @RT00707                                            0707
         CLI   WORKCHAR(SCANPTR),C','                              0707
         BNE   @RF00707                                            0707
@RT00707 DS    0H                                                  0708
*     DO;                           /* YES                           */
*       SCANERR='1'B;               /* SHOW ERROR                    */
         OI    SCANERR,B'10000000'                                 0709
*       RETURN;                     /* EXIT                          */
@EL00013 DS    0H                                                  0710
@EF00013 DS    0H                                                  0710
@ER00013 L     @14,12(,@13)                                        0710
         BR    @14                                                 0710
*     END;                                                         0711
*   ELSE                            /* NO                            */
*     SCANPTR=SCANPTR+1;            /* POINT PAST '=' SIGN SCAN TABLE
*                                      LOOKING FOR A MATCH, OR THE 0712
*                                      END OF THE TABLE              */
@RF00707 AL    SCANPTR,@CF00057                                    0712
*   DO WHILE(TBLNAME1^='FF'X&TBLNAME^=WORKPARM);                   0713
         B     @DE00713                                            0713
@DL00713 DS    0H                                                  0714
*     TABLEPTR=TABLEPTR+20;                                        0714
         AL    TABLEPTR,@CF00144                                   0714
*   END;                                                           0715
@DE00713 CLI   TBLNAME1(TABLEPTR),X'FF'                            0715
         BE    @DC00713                                            0715
         CLC   TBLNAME(8,TABLEPTR),WORKPARM                        0715
         BNE   @DL00713                                            0715
@DC00713 DS    0H                                                  0716
*   IF TBLNAME1='FF'X THEN          /* END OF TABLE                  */
         CLI   TBLNAME1(TABLEPTR),X'FF'                            0716
         BNE   @RF00716                                            0716
*     SCANERR='1'B;                 /* YES - SHOW ERROR              */
         OI    SCANERR,B'10000000'                                 0717
*   END FNDNMTCH;                                                  0718
         B     @EL00013                                            0718
         EJECT
*PRCSPARM:                                                         0719
*   PROC OPTIONS(SAVE(14));                                        0719
PRCSPARM ST    @14,@SA00014                                        0719
*                                                                  0720
*/*  *****************************************************************/
*/*                                                                  */
*/*    THIS SUBROUTINE WILL VERIFY THAT THE PARAMETER FOLLOWING A    */
*/*    KEYWORD NAME IS A LEGAL PARAMETER.  UPON ENTRY, 'SCANPTR'     */
*/*    SHOULD POINT TO THE FIRST CHARACTER OF THE PARAMETER,         */
*/*    'TABLEPTR' SHOULD POINT AT THE TABLE ENTRY WE WILL USE FOR    */
*/*    VALIDATION, AND 'SAVEPTR' SHOULD POINT AT TABLE WHICH WILL BE */
*/*    USED TO SAVE THE EDITED AND VERIFIED PARAMETER.               */
*/*    UPON EXIT, 'SCANPTR' WILL POINT AT THE CHARACTER WHICH STOPPED*/
*/*    THE SCANNING OF THE PARAMETER, 'TABLEPTR' WILL BE UNCHANGED,  */
*/*    AND 'SAVEPTR' WILL POINT AT THE SAVE AREA WHERE THE EDITED    */
*/*    AND VERIFIED PARAMETER IS.  NOTES: - IF THE PARAMETER IS      */
*/*    INVALID, THE FIRST BYTE OF THE SAVE AREA WILL BE 'FF'X.       */
*/*                                       - IF MORE THAN ONE         */
*/*    PARAMETER IS ENTERED FOR A SINGLE KEYWORD, THEN THAT          */
*/*    KEYWORD WILL BE FLAGGED AS AN ERROR, WITH THE EXCEPTION OF    */
*/*    THE 'RCFBDUMP' KEYWORD.                                       */
*/*  *****************************************************************/
*                                                                  0720
*   IF TBLZERO='1'B THEN            /* ZERO-FILL                     */
         TM    TBLZERO(TABLEPTR),B'00001000'                       0720
         BNO   @RF00720                                            0720
*     WORKPARM=ZERO;                /* YES                           */
         MVC   WORKPARM(8),@CC00103                                0721
*   ELSE                                                           0722
*     WORKPARM=BLANK;               /* NO - BLANK-FILL               */
         B     @RC00720                                            0722
@RF00720 MVC   WORKPARM(8),@CC00097                                0722
*   IF TBLNAME=RCFBDUMP THEN        /* 'RCFBDUMP             @ZM20856*/
@RC00720 CLC   TBLNAME(8,TABLEPTR),RCFBDUMP                        0723
         BNE   @RF00723                                            0723
*     DO;                                                          0724
*       SAVEPTR=SAVEPTR+8*(TBLDISP-1+DUMPCNT);/* ADDRESS PROPER WORK
*                                      AREA                  @ZM20856*/
         LH    @04,DUMPCNT                                         0725
         LH    @15,TBLDISP(,TABLEPTR)                              0725
         BCTR  @15,0                                               0725
         ALR   @15,@04                                             0725
         SLA   @15,3                                               0725
         ALR   SAVEPTR,@15                                         0725
*       IF DUMPCNT>16 THEN          /* MAX '16' PARMS        @ZM20856
*                                      NOTE: DUMPCNT INITIALIZED IN
*                                      PROC 'READMBR' TO 0.  @ZM20856*/
         C     @04,@CF00176                                        0726
         BNH   @RF00726                                            0726
*         DO;                       /* POSITION SCAN         @ZM20856*/
*           DO WHILE(WORKPARM^=','&WORKPARM^=' ');/*         @ZM20856*/
         B     @DE00728                                            0728
@DL00728 DS    0H                                                  0729
*             SCANPTR=SCANPTR+1;    /* NEXT CHAR             @ZM20856*/
         AL    SCANPTR,@CF00057                                    0729
*           END;                    /* PT PAST PARM          @ZM20856*/
@DE00728 CLI   WORKPARM,C','                                       0730
         BE    @DC00728                                            0730
         CLI   WORKPARM,C' '                                       0730
         BNE   @DL00728                                            0730
@DC00728 DS    0H                                                  0731
*           RETURN;                 /* IGNORE > 16           @ZM20856*/
@EL00014 DS    0H                                                  0731
@EF00014 DS    0H                                                  0731
@ER00014 L     @14,@SA00014                                        0731
         BR    @14                                                 0731
*         END;                      /*                       @ZM20856*/
*       ELSE                        /* INCREMENT COUNT       @ZM20856*/
*         CALL SPLITPRM;            /* SPLIT PARMS           @ZM20856*/
@RF00726 BAL   @14,SPLITPRM                                        0733
*     END;                          /* END PROCESS           @ZM20856*/
*   ELSE                                                           0735
*     DO;                           /* OTHER KEYWORD         @ZM20856*/
         B     @RC00723                                            0735
@RF00723 DS    0H                                                  0736
*       SAVEPTR=SAVEPTR+8*(TBLDISP-1);/* ADDRESS PROPER WORK AREA  0736
*                                                            @ZM20856*/
         LH    @04,TBLDISP(,TABLEPTR)                              0736
         BCTR  @04,0                                               0736
         SLA   @04,3                                               0736
         ALR   SAVEPTR,@04                                         0736
*       IF SAVECHAR^=HEXBC THEN     /* PARAMETER ALREADY THERE       */
         CLI   SAVECHAR(SAVEPTR),X'BC'                             0737
         BE    @RF00737                                            0737
*         DO;                       /* YES                           */
*           SAVECHAR=HEXFF;         /* FLAG AS AN ERROR              */
         MVI   SAVECHAR(SAVEPTR),X'FF'                             0739
*           RETURN;                 /* EXIT                          */
         B     @EL00014                                            0740
*         END;                                                     0741
*       IF TBLNUMRC='1'B THEN       /* VALUE MUST BE NUMERIC         */
@RF00737 TM    TBLNUMRC(TABLEPTR),B'01000000'                      0742
         BNO   @RF00742                                            0742
*         CALL NBRXTRCT;            /* YES                           */
         BAL   @14,NBRXTRCT                                        0743
*       ELSE                                                       0744
*         CALL REGXTRCT;            /* NO                            */
         B     @RC00742                                            0744
@RF00742 BAL   @14,REGXTRCT                                        0744
*       IF SAVECHAR=HEXFF THEN      /* ERROR DETECTED                */
@RC00742 CLI   SAVECHAR(SAVEPTR),X'FF'                             0745
         BE    @RT00745                                            0745
*         RETURN;                   /* YES                           */
*       IF TBLRIGHT='1'B&I<8 THEN   /* RIGHT-JUSTIFY                 */
         TM    TBLRIGHT(TABLEPTR),B'00010000'                      0747
         BNO   @RF00747                                            0747
         C     I,@CF00034                                          0747
         BNL   @RF00747                                            0747
*         DO;                       /* YES                           */
*           IF TBLZERO='1'B THEN    /* FILL TEMPORARY                */
         TM    TBLZERO(TABLEPTR),B'00001000'                       0749
         BNO   @RF00749                                            0749
*             TEMPAREA=ZERO;        /* AREA WITH                     */
         MVC   TEMPAREA(8),@CC00103                                0750
*           ELSE                    /* PROPER FILL                   */
*             TEMPAREA=BLANK;       /* CHARACTER                     */
         B     @RC00749                                            0751
@RF00749 MVC   TEMPAREA(8),@CC00097                                0751
*           I=9-I;                  /* STARTING PT IN TEMPAREA       */
@RC00749 LA    @04,9                                               0752
         SLR   @04,I                                               0752
         LR    I,@04                                               0752
*           MVC(TEMPAREA(I:8),WORKPARM);/* SHIFT RIGHT INTO AREA     */
         LA    @04,TEMPAREA-1(I)                                   0753
         LA    @15,8                                               0753
         SLR   @15,I                                               0753
         EX    @15,@SM01657                                        0753
*           WORKPARM=TEMPAREA;      /* MOVE BACK                     */
         MVC   WORKPARM(8),TEMPAREA                                0754
*         END;                                                     0755
*       IF TBLRSTD='1'B THEN        /* RESTRICTED VALUES             */
@RF00747 TM    TBLRSTD(TABLEPTR),B'10000000'                       0756
         BNO   @RF00756                                            0756
*         DO;                       /* YES                           */
*           RSTDPTR=TBLPTR;         /* PTR TO RSTD TABLE             */
         L     RSTDPTR,TBLPTR(,TABLEPTR)                           0758
*           DO WHILE(RSTDCHAR^=HEXFF&RSTDNAME^=WORKPARM);/* SEARCH   */
         B     @DE00759                                            0759
@DL00759 DS    0H                                                  0760
*             RSTDPTR=RSTDPTR+8;    /* FOR A                         */
         AL    RSTDPTR,@CF00034                                    0760
*           END;                    /* MATCH                         */
@DE00759 CLI   RSTDCHAR(RSTDPTR),X'FF'                             0761
         BE    @DC00759                                            0761
         CLC   RSTDNAME(8,RSTDPTR),WORKPARM                        0761
         BNE   @DL00759                                            0761
@DC00759 DS    0H                                                  0762
*           IF RSTDCHAR=HEXFF THEN  /* FIND ONE                      */
         CLI   RSTDCHAR(RSTDPTR),X'FF'                             0762
         BNE   @RF00762                                            0762
*             SAVECHAR=HEXFF;       /* NOPE                          */
         MVI   SAVECHAR(SAVEPTR),X'FF'                             0763
*         END;                                                     0764
@RF00762 DS    0H                                                  0765
*       IF SAVECHAR^=HEXFF THEN     /* STILL GOOD VALUE              */
@RF00756 CLI   SAVECHAR(SAVEPTR),X'FF'                             0765
         BE    @RF00765                                            0765
*         IF TBLBINRY='1'B THEN     /* CONVERT TO BINARY             */
         TM    TBLBINRY(TABLEPTR),B'00100000'                      0766
         BNO   @RF00766                                            0766
*           DO;                                                    0767
*             PACK(TEMPAREA,WORKPARM);/* PACK THE NUMBER             */
         PACK  TEMPAREA(8),WORKPARM(8)                             0768
*             WORKPARM=''B;         /* ZERO OUT RECEIVNG FIELD       */
         XC    WORKPARM(8),WORKPARM                                0769
*             CVB(WRKPRMF2,TEMPAREA);/* CVB INTO WORKPARM            */
         CVB   @04,TEMPAREA                                        0770
         ST    @04,WRKPRMF2                                        0770
*             IF TBLRANGE='1'B THEN /* DO A RANGE CHECK              */
         TM    TBLRANGE(TABLEPTR),B'00000100'                      0771
         BNO   @RF00771                                            0771
*               DO;                 /* YES                           */
*                 IF WRKPRMF2<TBLLOW WRKPRMF2>TBLHIGH THEN         0773
         C     @04,TBLLOW(,TABLEPTR)                               0773
         BL    @RT00773                                            0773
         C     @04,TBLHIGH(,TABLEPTR)                              0773
         BNH   @RF00773                                            0773
@RT00773 DS    0H                                                  0774
*                   SAVECHAR=HEXFF;                                0774
         MVI   SAVECHAR(SAVEPTR),X'FF'                             0774
*               END;                                               0775
@RF00773 DS    0H                                                  0776
*           END;                                                   0776
@RF00771 DS    0H                                                  0777
*       IF SAVECHAR^=HEXFF THEN     /* STILL OK                      */
@RF00766 DS    0H                                                  0777
@RF00765 CLI   SAVECHAR(SAVEPTR),X'FF'                             0777
         BE    @RF00777                                            0777
*         SAVAREA=WORKPARM;         /* YES - MOVE IN GOOD VALUE      */
         MVC   SAVAREA(8,SAVEPTR),WORKPARM                         0778
*     END;                          /* ALL OTHER KEYWORDS    @ZM20856*/
*   END PRCSPARM;                                                  0780
         B     @EL00014                                            0780
         EJECT
*NBRXTRCT:                                                         0781
*   PROC OPTIONS(SAVE(14));                                        0781
NBRXTRCT ST    @14,12(,@13)                                        0781
*                                                                  0782
*/*  *****************************************************************/
*/*                                                                  */
*/*    THIS SUBROUTINE MOVES A PARAMETER INTO A WORKAREA, VERIFIES   */
*/*    THAT IT IS NUMERIC, AND CHECKS ITS LENGTH.                    */
*/*                                                                  */
*/*  *****************************************************************/
*                                                                  0782
*   I=0;                            /* ZERO OUT LENGTH COUNTER       */
         SLR   I,I                                                 0782
*   DO WHILE(WORKCHAR^=','&WORKCHAR^=' ');                         0783
         B     @DE00783                                            0783
@DL00783 DS    0H                                                  0784
*     I=I+1;                        /* ADD 1 TO LENGTH               */
         AL    I,@CF00057                                          0784
*     IF I<9 THEN                   /* LENGTH STILL GOOD             */
         C     I,@CF00069                                          0785
         BNL   @RF00785                                            0785
*       DO;                         /* YES                           */
*         WORKPRM(I)=WORKCHAR;      /* MOVE NEXT CHARACTER           */
         LA    @04,WORKPRM-1(I)                                    0787
         MVC   0(1,@04),WORKCHAR(SCANPTR)                          0787
*         IF WORKCHAR<'0'           /* IS IT                         */
*             WORKCHAR>'9' THEN     /* NON-NUMERIC                   */
         CLI   WORKCHAR(SCANPTR),C'0'                              0788
         BL    @RT00788                                            0788
         CLI   WORKCHAR(SCANPTR),C'9'                              0788
         BNH   @RF00788                                            0788
@RT00788 DS    0H                                                  0789
*           I=9;                    /* YES - FORCE ERROR             */
         LA    I,9                                                 0789
*       END;                                                       0790
@RF00788 DS    0H                                                  0791
*     SCANPTR=SCANPTR+1;            /* PT AT NEXT CHARACTER          */
@RF00785 AL    SCANPTR,@CF00057                                    0791
*   END;                                                           0792
@DE00783 CLI   WORKCHAR(SCANPTR),C','                              0792
         BE    @DC00783                                            0792
         CLI   WORKCHAR(SCANPTR),C' '                              0792
         BNE   @DL00783                                            0792
@DC00783 DS    0H                                                  0793
*   IF I=0 I>8 THEN                 /* ANY ERRORS DISCOVERED         */
         LTR   I,I                                                 0793
         BZ    @RT00793                                            0793
         C     I,@CF00034                                          0793
         BNH   @RF00793                                            0793
@RT00793 DS    0H                                                  0794
*     SAVECHAR=HEXFF;               /* YES                           */
         MVI   SAVECHAR(SAVEPTR),X'FF'                             0794
*   END NBRXTRCT;                                                  0795
@EL00015 DS    0H                                                  0795
@EF00015 DS    0H                                                  0795
@ER00015 L     @14,12(,@13)                                        0795
         BR    @14                                                 0795
         EJECT
*REGXTRCT:                                                         0796
*   PROC OPTIONS(SAVE(14));                                        0796
REGXTRCT ST    @14,12(,@13)                                        0796
*                                                                  0797
*/*  *****************************************************************/
*/*                                                                  */
*/*    THIS SUBROUTINE MOVES A PARAMETER INTO A WORKAREA, AND        */
*/*    CHECKS ITS LENGTH.                                            */
*/*                                                                  */
*/*  *****************************************************************/
*                                                                  0797
*   I=0;                            /* ZERO OUT LENGTH COUNTER       */
         SLR   I,I                                                 0797
*   DO WHILE(WORKCHAR^=','&WORKCHAR^=' ');                         0798
         B     @DE00798                                            0798
@DL00798 DS    0H                                                  0799
*     I=I+1;                        /* ADD 1 TO LENGTH               */
         AL    I,@CF00057                                          0799
*     IF I<9 THEN                   /* LENGTH STILL GOOD             */
         C     I,@CF00069                                          0800
         BNL   @RF00800                                            0800
*       WORKPRM(I)=WORKCHAR;        /* YES - MOVE CHARACTER          */
         LA    @04,WORKPRM-1(I)                                    0801
         MVC   0(1,@04),WORKCHAR(SCANPTR)                          0801
*     SCANPTR=SCANPTR+1;            /* PT AT NEXT CHARACTER          */
@RF00800 AL    SCANPTR,@CF00057                                    0802
*   END;                                                           0803
@DE00798 CLI   WORKCHAR(SCANPTR),C','                              0803
         BE    @DC00798                                            0803
         CLI   WORKCHAR(SCANPTR),C' '                              0803
         BNE   @DL00798                                            0803
@DC00798 DS    0H                                                  0804
*   IF I=0 I>8 THEN                 /* IF BAD LENGTH                 */
         LTR   I,I                                                 0804
         BZ    @RT00804                                            0804
         C     I,@CF00034                                          0804
         BNH   @RF00804                                            0804
@RT00804 DS    0H                                                  0805
*     SAVECHAR=HEXFF;               /* SHOW IT                       */
         MVI   SAVECHAR(SAVEPTR),X'FF'                             0805
*   END REGXTRCT;                                                  0806
@EL00016 DS    0H                                                  0806
@EF00016 DS    0H                                                  0806
@ER00016 L     @14,12(,@13)                                        0806
         BR    @14                                                 0806
         EJECT
*SPLITPRM:                                                         0807
*   PROC OPTIONS(SAVE(14));                                        0807
SPLITPRM ST    @14,12(,@13)                                        0807
*                                                                  0808
*/*  *****************************************************************/
*/*                                                                  */
*/*    THIS SUBROUTINE SPLITS THE KEYWORD 'RCFBDUMP' INTO 3 LOGICAL  */
*/*    VALUES AND TRANSLATES THEM TO BINARY. VALUES:                 */
*/*            1. VTAM RETURN CODE                                   */
*/*            2. VTAM FEEDBACK CODE                                 */
*/*            3. DUMP OPTION - 0=LOCAL, 1=ALL                       */
*/*  *****************************************************************/
*                                                                  0808
*   I=0;                            /* ZERO LENGTH COUNT     @ZM20856*/
         SLR   I,I                                                 0808
*   SAVAREAD=SAVAREAD&&SAVAREAD;    /* INITIALIZE AREA       @ZM20856*/
         XC    SAVAREAD(8,SAVEPTR),SAVAREAD(SAVEPTR)               0809
*   DO WHILE(WORKCHAR^=','&WORKCHAR^=' ');/*                 @ZM20856*/
         B     @DE00810                                            0810
@DL00810 DS    0H                                                  0811
*     I=I+1;                        /* 1 TO LENGTH           @ZM20856*/
         AL    I,@CF00057                                          0811
*     IF I<6 THEN                   /* GOOD LENGTH           @ZM20856*/
         C     I,@CF00065                                          0812
         BNL   @RF00812                                            0812
*       WORKPARM(I)=WORKCHAR;       /* MOVE NEXT CHAR        @ZM20856*/
         LA    @01,WORKPARM-1(I)                                   0813
         MVC   0(1,@01),WORKCHAR(SCANPTR)                          0813
*     SCANPTR=SCANPTR+1;            /* PT NEXT CHAR          @ZM20856*/
@RF00812 AL    SCANPTR,@CF00057                                    0814
*   END;                            /* END DO WHILE          @ZM20856*/
@DE00810 CLI   WORKCHAR(SCANPTR),C','                              0815
         BE    @DC00810                                            0815
         CLI   WORKCHAR(SCANPTR),C' '                              0815
         BNE   @DL00810                                            0815
@DC00810 DS    0H                                                  0816
*   IF I^=5 THEN                    /* INVALID PARM          @ZM20856*/
         C     I,@CF00248                                          0816
         BE    @RF00816                                            0816
*     DO;                           /* YES                   @ZM20856*/
*       SAVECHAR=HEXFF;             /* YES                   @ZM20856*/
         MVI   SAVECHAR(SAVEPTR),X'FF'                             0818
*       RETURN;                     /* NEXT KEYWORD          @ZM20856*/
@EL00017 DS    0H                                                  0819
@EF00017 DS    0H                                                  0819
@ER00017 L     @14,12(,@13)                                        0819
         BR    @14                                                 0819
*     END;                          /*                       @ZM20856*/
*   ELSE                                                           0821
*     ;                             /* VALID                 @ZM20856*/
*                                                                  0821
@RF00816 DS    0H                                                  0822
*   /*****************************************************************/
*   /*                                                               */
*   /* PROCESS VTAM RETURN CODE                              @ZM20856*/
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0822
*   SPLIT(5:6)=WORKPARM(1:2);       /* EXTRACT RC            @ZM21059*/
         MVC   SPLIT+4(2),WORKPARM                                 0822
*   TR(SPLIT(5:6),TABBINRY);        /* TRANS TO BINARY-ALSO TRANS  0823
*                                      INVALIDS              @ZM20856*/
         TR    SPLIT+4(2),TABBINRY                                 0823
*   CHKFIELD(5:6)=SPLIT(5:6)&'F0F0'X;/* INVALIDS             @ZM20856*/
         MVC   CHKFIELD+4(2),SPLIT+4                               0824
         NC    CHKFIELD+4(2),@CB01628                              0824
*   IF CHKFIELD(5:6)^=0 THEN        /* INVALID RC            @ZM20856*/
         ICM   @15,3,CHKFIELD+4                                    0825
         BZ    @RF00825                                            0825
*     DO;                           /* YES                   @ZM20856*/
*       SAVECHAR=HEXFF;             /* FLAG ERROR            @ZM20856*/
         MVI   SAVECHAR(SAVEPTR),X'FF'                             0827
*       RETURN;                     /*                       @ZM20856*/
         B     @EL00017                                            0828
*     END;                          /*                       @ZM20856*/
*   ELSE                                                           0830
*     DO;                           /* VALID RC              @ZM20856*/
@RF00825 DS    0H                                                  0831
*       PACK(TEMPAREA(7:8),SPLIT(5:7));/* COMBINE            @ZM21059*/
         PACK  TEMPAREA+6(2),SPLIT+4(3)                            0831
*       VTAMRC=TEMPAREA(7);         /* WORKAREA UPT          @ZM21059*/
         MVC   VTAMRC(1,SAVEPTR),TEMPAREA+6                        0832
*     END;                                                         0833
*                                                                  0833
*   /*****************************************************************/
*   /*                                                               */
*   /* PROCESS VTAM FEEDBACK CODE                            @ZM20856*/
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0834
*   SPLIT(5:6)=WORKPARM(3:4);       /* EXTRACT FEEDBCK       @ZM21059*/
         MVC   SPLIT+4(2),WORKPARM+2                               0834
*   TR(SPLIT(5:6),TABBINRY);        /* TRANS TO BINARY- ALSO TRANS 0835
*                                      INVALIDS              @ZM20856*/
         TR    SPLIT+4(2),TABBINRY                                 0835
*   CHKFIELD(5:6)=SPLIT(5:6)&'F0F0'X;/* INVALIDS             @ZM20856*/
         MVC   CHKFIELD+4(2),SPLIT+4                               0836
         NC    CHKFIELD+4(2),@CB01628                              0836
*   IF CHKFIELD(5:6)^=0 THEN        /* INVALID FB            @ZM20856*/
         ICM   @15,3,CHKFIELD+4                                    0837
         BZ    @RF00837                                            0837
*     DO;                           /* YES                   @ZM20856*/
*       SAVECHAR=HEXFF;             /* FLAG ERROR            @ZM20856*/
         MVI   SAVECHAR(SAVEPTR),X'FF'                             0839
*       RETURN;                     /*                       @ZM20856*/
         B     @EL00017                                            0840
*     END;                          /*                       @ZM20856*/
*   ELSE                                                           0842
*     DO;                           /* VALID FB              @ZM20856*/
@RF00837 DS    0H                                                  0843
*       PACK(TEMPAREA(7:8),SPLIT(5:7));/* COMBINE            @ZM21059*/
         PACK  TEMPAREA+6(2),SPLIT+4(3)                            0843
*       VTAMFB=TEMPAREA(7);         /* WORKAREA UPT          @ZM21059*/
         MVC   VTAMFB(1,SAVEPTR),TEMPAREA+6                        0844
*     END;                                                         0845
*                                                                  0845
*   /*****************************************************************/
*   /*                                                               */
*   /* PROCESS DUMP OPTION CODE                              @ZM20856*/
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0846
*   IF WORKPARM(5)^='F1'X&WORKPARM(5)^='F0'X THEN/* DUMP OPTION HAS
*                                      TO BE EITHER 1 OR 0   @ZM21059*/
         CLI   WORKPARM+4,X'F1'                                    0846
         BE    @RF00846                                            0846
         CLI   WORKPARM+4,X'F0'                                    0846
         BE    @RF00846                                            0846
*     SAVECHAR=HEXFF;               /* FLAG AS ERROR         @ZM20856*/
         MVI   SAVECHAR(SAVEPTR),X'FF'                             0847
*   ELSE                            /* OK PARM               @ZM20856*/
*     DUMPCNT=DUMPCNT+1;            /* BUMP COUNT            @ZM20856*/
         B     @RC00846                                            0848
@RF00846 LH    @01,DUMPCNT                                         0848
         LA    @01,1(,@01)                                         0848
         STH   @01,DUMPCNT                                         0848
*   IF WORKPARM(5)='F1'X THEN       /* '1'                   @ZM21059*/
@RC00846 CLI   WORKPARM+4,X'F1'                                    0849
         BNE   @RF00849                                            0849
*     SDUMP=1;                      /* WORKAREA UPT 1        @ZM20856*/
         MVI   SDUMP(SAVEPTR),X'01'                                0850
*   ELSE                            /*                       @ZM20856*/
*     SDUMP=0;                      /* WORKAREA UPT 0        @ZM20856*/
         B     @RC00849                                            0851
@RF00849 MVI   SDUMP(SAVEPTR),X'00'                                0851
*   END SPLITPRM;                                                  0852
         B     @EL00017                                            0852
         EJECT
*PARMMOV:                                                          0853
*   PROC OPTIONS(SAVE(14));                                        0853
PARMMOV  ST    @14,12(,@13)                                        0853
*                                                                  0854
*/*  *****************************************************************/
*/*                                                                  */
*/*    THIS SUBROUTINE MOVES THE PARMS ENTERED FOR EACH KEYWORD INTO */
*/*    A EDIT FIELD SUITABLE FOR PRINTING. NOTE: THIS PROC IS NOT    */
*/*    INVOKED FOR THE 'RCFBDUMP' KEYWORD.                           */
*/*  *****************************************************************/
*                                                                  0854
*   CVD(WRKPRMF2,TEMPAREA);         /* TO DECIMAL            @ZM20856*/
         L     @04,WRKPRMF2                                        0854
         CVD   @04,TEMPAREA                                        0854
*   UNPK(WORKPARM,TEMPAREA);        /* UNPACK                @ZM20856*/
         UNPK  WORKPARM(8),TEMPAREA(8)                             0855
*   WORKPARM(8)=WORKPARM(8) 'F0'X;  /* REMOVE SIGN           @ZM20856*/
         OI    WORKPARM+7,X'F0'                                    0856
*   SCANPTR=ADDR(WORKPARM);         /* START SCAN            @ZM20856*/
         LA    SCANPTR,WORKPARM                                    0857
*   DO J=1 TO 8 WHILE(WORKCHAR='0');/* FIND LEADING DIGIT            */
         LA    J,1                                                 0858
@DL00858 CLI   WORKCHAR(SCANPTR),C'0'                              0858
         BNE   @DC00858                                            0858
*     SCANPTR=SCANPTR+1;            /* NEXT BYTE             @ZM20856*/
         AL    SCANPTR,@CF00057                                    0859
*   END;                            /*                       @ZM20856*/
         AL    J,@CF00057                                          0860
         C     J,@CF00034                                          0860
         BNH   @DL00858                                            0860
@DC00858 DS    0H                                                  0861
*   IF J=9 THEN                     /* ALL ZERO              @ZM20856*/
         C     J,@CF00069                                          0861
         BNE   @RF00861                                            0861
*     J=8;                          /* PRINT 1 ZERO          @ZM20856*/
         LA    J,8                                                 0862
*   DO K=J TO 8;                    /* MOVE CORRECT          @ZM20856*/
@RF00861 LR    K,J                                                 0863
         B     @DE00863                                            0863
@DL00863 DS    0H                                                  0864
*     EDFIELD(I)=WORKPARM(K);       /* NUMBER TO FIELD       @ZM20856*/
         LA    @01,EDFIELD-1(I)                                    0864
         LA    @15,WORKPARM-1(K)                                   0864
         MVC   0(1,@01),0(@15)                                     0864
*     I=I+1;                        /*                       @ZM20856*/
         AL    I,@CF00057                                          0865
*   END;                            /*                       @ZM20856*/
         AL    K,@CF00057                                          0866
@DE00863 C     K,@CF00034                                          0866
         BNH   @DL00863                                            0866
*   END PARMMOV;                    /*                       @ZM20856*/
@EL00018 DS    0H                                                  0867
@EF00018 DS    0H                                                  0867
@ER00018 L     @14,12(,@13)                                        0867
         BR    @14                                                 0867
         EJECT
*                                                                  0868
*/*  *****************************************************************/
*/*                                                                  */
*/*    THESE 2 PAGES CONTAIN THE ASSEMBLER CODING FOR THE PARMLIB    */
*/*    DCB, THE PRINTOUT DCB, THE SYNAD EXITS, THE DCB ABEND EXITS,  */
*/*    THE EXIT LISTS, AND THE JFCB AREA.                            */
*/*                                                                  */
*/*  *****************************************************************/
*                                                                  0868
*   GENERATE DATA DEFS(PARMLIB,EXITLST1,SYNAD1,DCBABND1,JFCBAREA); 0868
*   GENERATE DATA DEFS(PRINTOUT,EXITLST2,SYNAD2,DCBABND2);         0869
         EJECT
*   END IKTCAS54                                                   0870
*                                                                  0870
*/* THE FOLLOWING INCLUDE STATEMENTS WERE FOUND IN THIS PROGRAM.     */
*/*%INCLUDE SYSLIB  (IEECHAIN)                                       */
*/*%INCLUDE SYSLIB  (IEZCIB  )                                       */
*/*%INCLUDE SYSLIB  (IHADCBDF)                                       */
*/*%INCLUDE SYSLIB  (IHADCB  )                                       */
*/*%INCLUDE SYSLIB  (IEFJFCBN)                                       */
*/*%INCLUDE SYSLIB  (IKTTCAST)                                       */
*/*%INCLUDE SYSLIB  (IKTDMPCD)                                       */
*/*%INCLUDE SYSLIB  (IKTCASWA)                                       */
*                                                                  0870
*       ;                                                          0870
         B     @EL00001                                            0870
@DATA    DS    0H
@CH00189 DC    H'26'
@SM01645 MVC   CIBUFFER(0),CIBDATA(@15)
@SM01657 MVC   0(0,@04),WORKPARM
@DATD    DSECT
         DS    0F
@SA00001 DS    18F
@SA00002 DS    1F
@SA00003 DS    1F
@SA00005 DS    1F
@SA00009 DS    1F
@SA00012 DS    1F
@SA00014 DS    1F
@SA00010 DS    1F
@SA00011 DS    1F
IKTCAS54 CSECT
         DS    0F
@CF00057 DC    F'1'
@CF00118 DC    F'3'
@CH00118 EQU   @CF00118+2
@CF00063 DC    F'4'
@CH00063 EQU   @CF00063+2
@CF00248 DC    F'5'
@CF00065 DC    F'6'
@CF00034 DC    F'8'
@CF00069 DC    F'9'
@CF00176 DC    F'16'
@CF00144 DC    F'20'
@DATD    DSECT
         DS    0D
DUMPPTR  DS    A
PRNTEND  DS    A
L        DS    H
M        DS    H
N        DS    H
RTNCODE  DS    H
DUMPCNT  DS    H
         DS    CL6
TEMPAREA DS    CL8
CHKFIELD DS    CL8
CIBUFFER DS    CL48
KEYWDNM  DS    CL8
SPLIT    DS    CL8
SYNADSW1 DS    CL1
SYNADSW2 DS    CL1
DCBABSW1 DS    CL1
DCBABSW2 DS    CL1
SWITCHES DS    CL1
         ORG   SWITCHES
SCANERR  DS    BL1
READSW1  EQU   SWITCHES+0
GOSW1    EQU   SWITCHES+0
GOSW2    EQU   SWITCHES+0
PRINTSW  EQU   SWITCHES+0
@NM00001 EQU   SWITCHES+0
         ORG   SWITCHES+1
         DS    CL3
WORKPARM DS    CL8
         ORG   WORKPARM
WORKPRM  DS    8CL1
         ORG   WORKPARM+8
MBRRECD  DS    CL80
         ORG   MBRRECD
@NM00004 DS    CL72
MBR7380  DS    CL8
         ORG   MBR7380
@NM00005 DS    CL7
MBR80    DS    CL1
         ORG   MBRRECD+80
PRNTLINE DS    CL133
         ORG   PRNTLINE
PRNTCC   DS    CL1
PRNTDATA DS    CL132
         ORG   PRNTDATA
@NM00006 DS    CL4
PRNTDAT5 DS    CL128
         ORG   PRNTLINE+133
EDITFLD  DS    CL20
         ORG   EDITFLD
EDFIELD  DS    20CL1
         ORG   EDITFLD+20
TSOKEYNN DS    CL8
         ORG   TSOKEYNN
TSO      DS    CL6
TSO1     DS    CL1
TSO2     DS    CL1
         ORG   TSOKEYNN+8
MERGAREA DS    CL208
         ORG   MERGAREA
MERGE    DS    26CL8
         ORG   MERGAREA+208
         DS    CL3
SAVEBUFR DS    CL24
SAVEMBR  DS    CL208
IKTCAS54 CSECT
         DS    0F
@SIZDATD DC    AL1(0)
         DC    AL3(@ENDDATD-@DATD)
         DS    0D
@CC01543 DC    C'CONSOLE VALUES ACCEPTED:'
@CC01564 DC    C'PARMLIB VALUES ACCEPTED:'
@CC01585 DC    C'CURRENT TCAS PARAMETERS:'
@CC01526 DC    C'CONSOLE VALUES ENTERED:'
@CC01556 DC    C'PARMLIB VALUES ENTERED:'
@CC01529 DC    C'*NONE ENTERED*'
@CC01534 DC    C'USER=FSTOP '
@CC01537 DC    C'USER=SIC '
@CC00097 DC    C'        '
@CC00103 DC    C'00000000'
@CC00179 DC    C'TSOKEY00'
@CC00302 DC    C'FSTOP   '
@CC00329 DC    C'MEMBER  '
@CC00340 DC    C'USER    '
@CC00350 DC    C'USERMAX '
@CC00394 DC    C'HIBFREXT'
@CC00403 DC    C'LOBFREXT'
@CC01532 DC    C'IGNORE '
@CC00181 EQU   @CC00179
@CC01533 EQU   @CC00302
@CC01536 DC    C'SIC '
@CC01531 DC    C'U '
@CC01560 DC    C', '
@CB01595 DC    X'0F00'
@CB01628 DC    X'F0F0'
RCFBDUMP DC    CL8'RCFBDUMP'
         DS    CL1
DEFAULTS DS    CL88                                           ZP60007
         ORG   DEFAULTS
@NM00011 DS    CL8
         ORG   @NM00011
@NM00012 DC    F'0'
@NM00013 DC    F'40'
         ORG   DEFAULTS+8
@NM00014 DS    CL8
         ORG   @NM00014
@NM00015 DC    CL8'        '
         ORG   DEFAULTS+16
@NM00016 DS    CL8
         ORG   @NM00016
@NM00017 DC    F'0'
@NM00018 DC    F'3'
         ORG   DEFAULTS+24
@NM00019 DS    CL8
         ORG   @NM00019
@NM00020 DC    F'0'
@NM00021 DC    F'132'
         ORG   DEFAULTS+32
@NM00022 DS    CL8
         ORG   @NM00022
@NM00023 DC    F'0'
@NM00024 DC    F'48000'
         ORG   DEFAULTS+40
@NM00025 DS    CL8
         ORG   @NM00025
@NM00026 DC    F'0'
@NM00027 DC    F'24000'
         ORG   DEFAULTS+48
@NM00028 DS    CL8
         ORG   @NM00028
@NM00029 DC    F'0'
@NM00030 DC    F'480'
         ORG   DEFAULTS+56
@NM00031 DS    CL8
         ORG   @NM00031
@NM00032 DC    F'0'
@NM00033 DC    F'4'
         ORG   DEFAULTS+64
@NM00034 DS    CL8
         ORG   @NM00034
@NM00035 DC    CL8'NOBREAK '
         ORG   DEFAULTS+72
@NM00036 DS    CL8
         ORG   @NM00036
@NM00037 DC    CL8'NO      '
         ORG   DEFAULTS+80
         DC    CL8'YES     '                                   ZP60007
         ORG   DEFAULTS+88                                     ZP60007
SAVES2   DS    CL208
         ORG   SAVES2
SAVES1   DS    CL24
         ORG   SAVES1
@NM00038 DC    24X'BC'
         ORG   SAVES2+24
@NM00039 DS    CL56
         ORG   @NM00039
@NM00040 DC    56X'BC'
         ORG   SAVES2+80
@NM00041 DS    CL128
         ORG   @NM00041
@NM00042 DC    128X'BC'
         ORG   SAVES2+208
         DS    CL4
TABBINRY DS    CL256
         ORG   TABBINRY
@NM00048 DC    X'F0F0F0F0F0F0F0F0'
@NM00049 DC    X'F0F0F0F0F0F0F0F0'
@NM00050 DC    X'F0F0F0F0F0F0F0F0'
@NM00051 DC    X'F0F0F0F0F0F0F0F0'
@NM00052 DC    X'F0F0F0F0F0F0F0F0'
@NM00053 DC    X'F0F0F0F0F0F0F0F0'
@NM00054 DC    X'F0F0F0F0F0F0F0F0'
@NM00055 DC    X'F0F0F0F0F0F0F0F0'
@NM00056 DC    X'F0F0F0F0F0F0F0F0'
@NM00057 DC    X'F0F0F0F0F0F0F0F0'
@NM00058 DC    X'F0F0F0F0F0F0F0F0'
@NM00059 DC    X'F0F0F0F0F0F0F0F0'
@NM00060 DC    X'F0F0F0F0F0F0F0F0'
@NM00061 DC    X'F0F0F0F0F0F0F0F0'
@NM00062 DC    X'F0F0F0F0F0F0F0F0'
@NM00063 DC    X'F0F0F0F0F0F0F0F0'
@NM00064 DC    X'F0F0F0F0F0F0F0F0'
@NM00065 DC    X'F0F0F0F0F0F0F0F0'
@NM00066 DC    X'F0F0F0F0F0F0F0F0'
@NM00067 DC    X'F0F0F0F0F0F0F0F0'
@NM00068 DC    X'F0F0F0F0F0F0F0F0'
@NM00069 DC    X'F0F0F0F0F0F0F0F0'
@NM00070 DC    X'F0F0F0F0F0F0F0F0'
@NM00071 DC    X'F0F0F0F0F0F0F0F0'
@NM00072 DC    X'F00A0B0C0D0E0FF0'
@NM00073 DC    X'F0F0F0F0F0F0F0F0'
@NM00074 DC    X'F0F0F0F0F0F0F0F0'
@NM00075 DC    X'F0F0F0F0F0F0F0F0'
@NM00076 DC    X'F0F0F0F0F0F0F0F0'
@NM00077 DC    X'F0F0F0F0F0F0F0F0'
@NM00078 DC    X'0001020304050607'
@NM00079 DC    X'0809F0F0F0F0F0F0'
         ORG   TABBINRY+256
TABDEC   DS    CL16
         ORG   TABDEC
ALPHANUM DC    X'F0F1F2F3F4F5F6F7F8F9C1C2C3C4C5C6'
         ORG   TABDEC+16
RSTDUSER DS    CL17
         ORG   RSTDUSER
@NM00080 DC    CL8'FSTOP   '
@NM00081 DC    CL8'SIC     '
@NM00082 DC    X'FF'
         ORG   RSTDUSER+17
RSTDSCRN DS    CL17
         ORG   RSTDSCRN
@NM00083 DC    CL8'00000480'
@NM00084 DC    CL8'00001920'
@NM00085 DC    X'FF'
         ORG   RSTDSCRN+17
RSTDMODE DS    CL17
         ORG   RSTDMODE
NOBREAK  DC    CL8'NOBREAK '
BREAK    DC    CL8'BREAK   '
@NM00086 DC    X'FF'
         ORG   RSTDMODE+17
RSTDMDSW DS    CL17
         ORG   RSTDMDSW
NO       DC    CL8'NO      '
YES      DC    CL8'YES     '
@NM00087 DC    X'FF'
         ORG   RSTDMDSW+17
VERIBUFR DS    CL61
         ORG   VERIBUFR
@NM00089 DS    CL20
         ORG   @NM00089
@NM00090 DC    CL8'MEMBER  '
@NM00091 DC    H'1'
@NM00092 DC    CL1' '
@NM00093 DC    B'00000000'
@NM00094 DC    CL4'    '
@NM00095 DC    CL4'    '
         ORG   VERIBUFR+20
@NM00096 DS    CL20
         ORG   @NM00096
@NM00097 DC    CL8'USER    '
@NM00098 DC    H'2'
@NM00099 DC    CL1' '
@NM00100 DC    B'10000000'
@NM00101 DC    AL4(RSTDUSER)
@NM00102 DC    CL4'    '
         ORG   VERIBUFR+40
@NM00103 DS    CL20
         ORG   @NM00103
@NM00104 DC    CL8'USERMAX '
@NM00105 DC    H'3'
@NM00106 DC    CL1' '
@NM00107 DC    B'01111100'
@NM00108 DC    F'0'
@NM00109 DC    F'32767'
         ORG   VERIBUFR+60
@NM00110 DC    X'FF'
         ORG   VERIBUFR+61
         DS    CL3
VERIMBR  DS    CL521
         ORG   VERIMBR
@NM00111 DS    CL20
         ORG   @NM00111
@NM00112 DC    CL8'USERMAX '
@NM00113 DC    H'1'
@NM00114 DC    CL1' '
@NM00115 DC    B'01111100'
@NM00116 DC    F'0'
@NM00117 DC    F'32767'
         ORG   VERIMBR+20
@NM00118 DS    CL20
         ORG   @NM00118
@NM00119 DC    CL8'ACBPW   '
@NM00120 DC    H'2'
@NM00121 DC    CL1' '
@NM00122 DC    B'00000000'
@NM00123 DC    CL4'    '
@NM00124 DC    CL4'    '
         ORG   VERIMBR+40
@NM00125 DS    CL20
         ORG   @NM00125
@NM00126 DC    CL8'RECONLIM'
@NM00127 DC    H'3'
@NM00128 DC    CL1' '
@NM00129 DC    B'01111100'
@NM00130 DC    F'0'
@NM00131 DC    F'32767'
         ORG   VERIMBR+60
@NM00132 DS    CL20
         ORG   @NM00132
@NM00133 DC    CL8'BUFRSIZE'
@NM00134 DC    H'4'
@NM00135 DC    CL1' '
@NM00136 DC    B'01111100'
@NM00137 DC    F'4'
@NM00138 DC    F'4092'
         ORG   VERIMBR+80
@NM00139 DS    CL20
         ORG   @NM00139
@NM00140 DC    CL8'HIBFREXT'
@NM00141 DC    H'5'
@NM00142 DC    CL1' '
@NM00143 DC    B'01111100'
@NM00144 DC    F'1'
@NM00145 DC    F'16777215'
         ORG   VERIMBR+100
@NM00146 DS    CL20
         ORG   @NM00146
@NM00147 DC    CL8'LOBFREXT'
@NM00148 DC    H'6'
@NM00149 DC    CL1' '
@NM00150 DC    B'01111100'
@NM00151 DC    F'0'
@NM00152 DC    F'16777215'
         ORG   VERIMBR+120
@NM00153 DS    CL20
         ORG   @NM00153
@NM00154 DC    CL8'SCRSIZE '
@NM00155 DC    H'7'
@NM00156 DC    CL1' '
@NM00157 DC    B'11111000'
@NM00158 DC    AL4(RSTDSCRN)
@NM00159 DC    CL4'    '
         ORG   VERIMBR+140
@NM00160 DS    CL20
         ORG   @NM00160
@NM00161 DC    CL8'CHNLEN  '
@NM00162 DC    H'8'
@NM00163 DC    CL1' '
@NM00164 DC    B'01111100'
@NM00165 DC    F'1'
@NM00166 DC    F'10'
         ORG   VERIMBR+160
@NM00167 DS    CL20
         ORG   @NM00167
@NM00168 DC    CL8'MODE    '
@NM00169 DC    H'9'
@NM00170 DC    CL1' '
@NM00171 DC    B'10000000'
@NM00172 DC    AL4(RSTDMODE)
@NM00173 DC    CL4'    '
         ORG   VERIMBR+180
@NM00174 DS    CL20
         ORG   @NM00174
@NM00175 DC    CL8'MODESW  '
@NM00176 DC    H'10'
@NM00177 DC    CL1' '
@NM00178 DC    B'10000000'
@NM00179 DC    AL4(RSTDMDSW)
@NM00180 DC    CL4'    '
         ORG   VERIMBR+200
@NM00181 DS    CL20
         ORG   @NM00181
@NM00182 DC    CL8'CONFTXT '                                   ZP60007
@NM00183 DC    H'11'
@NM00184 DC    CL1' '
@NM00185 DC    B'10000000'                                     ZP60007
@NM00186 DC    AL4(RSTDMDSW)                                   ZP60007
@NM00187 DC    CL4'    '
         ORG   VERIMBR+220
@NM00188 DS    CL20
         ORG   @NM00188
@NM00189 DC    CL8'RCFBDUMP'
@NM00190 DC    H'12'
@NM00191 DC    CL1' '
@NM00192 DC    B'00001000'
@NM00193 DC    CL4'    '
@NM00194 DC    CL4'    '
         ORG   VERIMBR+240
@NM00195 DS    CL20
         ORG   @NM00195
@NM00196 DC    CL8'RCFBDUMP'
@NM00197 DC    H'13'
@NM00198 DC    CL1' '
@NM00199 DC    B'00001000'
@NM00200 DC    CL4'    '
@NM00201 DC    CL4'    '
         ORG   VERIMBR+260
@NM00202 DS    CL20
         ORG   @NM00202
@NM00203 DC    CL8'RCFBDUMP'
@NM00204 DC    H'14'
@NM00205 DC    CL1' '
@NM00206 DC    B'00001000'
@NM00207 DC    CL4'    '
@NM00208 DC    CL4'    '
         ORG   VERIMBR+280
@NM00209 DS    CL20
         ORG   @NM00209
@NM00210 DC    CL8'RCFBDUMP'
@NM00211 DC    H'15'
@NM00212 DC    CL1' '
@NM00213 DC    B'00001000'
@NM00214 DC    CL4'    '
@NM00215 DC    CL4'    '
         ORG   VERIMBR+300
@NM00216 DS    CL20
         ORG   @NM00216
@NM00217 DC    CL8'RCFBDUMP'
@NM00218 DC    H'16'
@NM00219 DC    CL1' '
@NM00220 DC    B'00001000'
@NM00221 DC    CL4'    '
@NM00222 DC    CL4'    '
         ORG   VERIMBR+320
@NM00223 DS    CL20
         ORG   @NM00223
@NM00224 DC    CL8'RCFBDUMP'
@NM00225 DC    H'17'
@NM00226 DC    CL1' '
@NM00227 DC    B'00001000'
@NM00228 DC    CL4'    '
@NM00229 DC    CL4'    '
         ORG   VERIMBR+340
@NM00230 DS    CL20
         ORG   @NM00230
@NM00231 DC    CL8'RCFBDUMP'
@NM00232 DC    H'18'
@NM00233 DC    CL1' '
@NM00234 DC    B'00001000'
@NM00235 DC    CL4'    '
@NM00236 DC    CL4'    '
         ORG   VERIMBR+360
@NM00237 DS    CL20
         ORG   @NM00237
@NM00238 DC    CL8'RCFBDUMP'
@NM00239 DC    H'19'
@NM00240 DC    CL1' '
@NM00241 DC    B'00001000'
@NM00242 DC    CL4'    '
@NM00243 DC    CL4'    '
         ORG   VERIMBR+380
@NM00244 DS    CL20
         ORG   @NM00244
@NM00245 DC    CL8'RCFBDUMP'
@NM00246 DC    H'20'
@NM00247 DC    CL1' '
@NM00248 DC    B'00001000'
@NM00249 DC    CL4'    '
@NM00250 DC    CL4'    '
         ORG   VERIMBR+400
@NM00251 DS    CL20
         ORG   @NM00251
@NM00252 DC    CL8'RCFBDUMP'
@NM00253 DC    H'21'
@NM00254 DC    CL1' '
@NM00255 DC    B'00001000'
@NM00256 DC    CL4'    '
@NM00257 DC    CL4'    '
         ORG   VERIMBR+420
@NM00258 DS    CL20
         ORG   @NM00258
@NM00259 DC    CL8'RCFBDUMP'
@NM00260 DC    H'22'
@NM00261 DC    CL1' '
@NM00262 DC    B'00001000'
@NM00263 DC    CL4'    '
@NM00264 DC    CL4'    '
         ORG   VERIMBR+440
@NM00265 DS    CL20
         ORG   @NM00265
@NM00266 DC    CL8'RCFBDUMP'
@NM00267 DC    H'23'
@NM00268 DC    CL1' '
@NM00269 DC    B'00001000'
@NM00270 DC    CL4'    '
@NM00271 DC    CL4'    '
         ORG   VERIMBR+460
@NM00272 DS    CL20
         ORG   @NM00272
@NM00273 DC    CL8'RCFBDUMP'
@NM00274 DC    H'24'
@NM00275 DC    CL1' '
@NM00276 DC    B'00001000'
@NM00277 DC    CL4'    '
@NM00278 DC    CL4'    '
         ORG   VERIMBR+480
@NM00279 DS    CL20
         ORG   @NM00279
@NM00280 DC    CL8'RCFBDUMP'
@NM00281 DC    H'25'
@NM00282 DC    CL1' '
@NM00283 DC    B'00001000'
@NM00284 DC    CL4'    '
@NM00285 DC    CL4'    '
         ORG   VERIMBR+500
@NM00286 DS    CL20
         ORG   @NM00286
@NM00287 DC    CL8'RCFBDUMP'
@NM00288 DC    H'26'
@NM00289 DC    CL1' '
@NM00290 DC    B'00001000'
@NM00291 DC    CL4'    '
@NM00292 DC    CL4'    '
         ORG   VERIMBR+520
@NM00293 DC    X'FF'
         ORG   VERIMBR+521
IKTCAS54 CSECT
PARMLIB  DCB   DSORG=PS,MACRF=GM,EXLST=EXITLST1,EODAD=EOF,            XX
               SYNAD=SYNAD1,EROPT=ACC,DDNAME=PARMLIB
EXITLST1 DS    0F
         DC    X'07'
         DC    AL3(JFCBAREA)
         DC    X'91'
         DC    AL3(DCBABND1)
JFCBAREA DS    0F,CL176
SYNAD1   DS    0F
         MVI   SYNADSW1,C'Y'       TURN SW ON
         RETURN
DCBABND1 DS    0F
         MVI   DCBABSW1,C'Y'      SHOW EXIT ENTERED
         TM    3(1),B'00000100'   IGNORE BIT ON
         BZ    *+12               NO
         MVI   3(1),4             YES - IGNORE ERROR
         B     *+8                EXIT
         MVI   3(1),0             ACCEPT ABEND
         RETURN
IKTCAS54 CSECT
PRINTOUT DCB   DSORG=PS,MACRF=PM,EXLST=EXITLST2,LRECL=133,RECFM=FA,   XX
               SYNAD=SYNAD2,EROPT=ACC,DDNAME=PRINTOUT,BLKSIZE=133
SYNAD2   DS    0F
         MVI   SYNADSW2,C'Y'
         RETURN
EXITLST2 DS    0F
         DC    X'91'
         DC    AL3(DCBABND2)
DCBABND2 DS    0F
         MVI   DCBABSW2,C'Y'
         TM    3(1),B'00000100'
         BZ    *+12
         MVI   3(1),4
         B     *+8
         MVI   3(1),0
         RETURN
@DATD    DSECT
         ORG   *+1-(*-@DATD)/(*-@DATD) INSURE DSECT DATA
@ENDDATD EQU   *
IKTCAS54 CSECT
@00      EQU   00                      EQUATES FOR REGISTERS 0-15
@01      EQU   01
@02      EQU   02
@03      EQU   03
@04      EQU   04
@05      EQU   05
@06      EQU   06
@07      EQU   07
@08      EQU   08
@09      EQU   09
@10      EQU   10
@11      EQU   11
@12      EQU   12
@13      EQU   13
@14      EQU   14
@15      EQU   15
TABLEPTR EQU   @05
RSTDPTR  EQU   @04
SAVEPTR  EQU   @02
K        EQU   @02
J        EQU   @04
I        EQU   @06
SCANPTR  EQU   @03
PRNTPTR  EQU   @07
TEMPPTR  EQU   @03
JFCBPTR  EQU   @04
DCBPTR   EQU   @02
R0       EQU   @00
R1       EQU   @01
R2       EQU   @02
R3       EQU   @03
MLPMPTR  EQU   @09
TWAPTR   EQU   @11
R14      EQU   @14
R15      EQU   @15
PRNTCHAR EQU   0
WORKCHAR EQU   0
DATA2    EQU   0
DATA4    EQU   0
DATA6    EQU   0
DATA7    EQU   0
DATA9    EQU   0
DATA11   EQU   0
DATABUF  EQU   0
@NM00007 EQU   0
MSGDATA  EQU   @NM00007+4
MSGBLK   EQU   0
MLPMPML  EQU   0
MLPMFL1  EQU   MLPMPML
MLPMWPM  EQU   MLPMFL1
MLPMUPTB EQU   MLPMFL1
MLPMRLIB EQU   MLPMFL1
MLPMBUF  EQU   MLPMFL1
MLPMCIB  EQU   MLPMFL1
MLPMFL2  EQU   MLPMPML+1
MLPMINV  EQU   MLPMFL2
MLPMUZ   EQU   MLPMFL2
MLPMUNZ  EQU   MLPMFL2
MLPMFS   EQU   MLPMFL2
MLPMSIC  EQU   MLPMFL2
MLPMIGN  EQU   MLPMFL2
MLPMSCA  EQU   MLPMPML+4
SAVAREA  EQU   0
SAVECHAR EQU   SAVAREA
RSTDNAME EQU   0
RSTDCHAR EQU   RSTDNAME
SCANTBL  EQU   0
TBLNAME  EQU   SCANTBL
TBLNAME1 EQU   TBLNAME
TBLDISP  EQU   SCANTBL+8
TBLFLAG  EQU   SCANTBL+11
TBLRSTD  EQU   TBLFLAG
TBLNUMRC EQU   TBLFLAG
TBLBINRY EQU   TBLFLAG
TBLRIGHT EQU   TBLFLAG
TBLZERO  EQU   TBLFLAG
TBLRANGE EQU   TBLFLAG
TBLPTR   EQU   SCANTBL+12
TBLLOW   EQU   TBLPTR
TBLHIGH  EQU   SCANTBL+16
CHAIN    EQU   0
CHFLG    EQU   CHAIN+4
CHSTS    EQU   CHFLG+2
CHACT    EQU   CHFLG+3
CHKEY    EQU   CHAIN+8
CHPARM   EQU   CHKEY
CHPKE    EQU   CHAIN+28
CHTJID   EQU   CHAIN+30
CHPROCSN EQU   CHAIN+32
CHACT1   EQU   CHPROCSN+3
CHASM    EQU   CHAIN+40
CHCIBP   EQU   CHASM+4
CHRGNSZ  EQU   CHASM+8
CHSTEP   EQU   CHASM+24
CHSWT    EQU   CHSTEP
CHSHORT  EQU   CHASM+32
CIBNAME  EQU   0
CIBASID  EQU   CIBNAME+10
CIBDATLN EQU   CIBNAME+14
CIBDATA  EQU   CIBNAME+16
IHADCB   EQU   0
IHADCS11 EQU   0
DCBRELB  EQU   IHADCS11
DCBREL   EQU   DCBRELB+1
DCBBUFCB EQU   IHADCS11+4
DCBDSORG EQU   IHADCS11+10
DCBDSRG1 EQU   DCBDSORG
DCBDSRG2 EQU   DCBDSORG+1
DCBIOBAD EQU   IHADCS11+12
DCBODEB  EQU   DCBIOBAD
DCBLNP   EQU   DCBODEB
DCBQSLM  EQU   DCBLNP
DCBIOBAA EQU   DCBODEB+1
IHADCS50 EQU   0
DCBSVCXL EQU   IHADCS50
DCBEODAD EQU   IHADCS50+4
DCBBFALN EQU   DCBEODAD
DCBHIARC EQU   DCBBFALN
DCBBFTEK EQU   DCBHIARC
DCBBFT   EQU   DCBBFTEK
DCBEXLST EQU   IHADCS50+8
DCBRECFM EQU   DCBEXLST
DCBRECLA EQU   DCBRECFM
IHADCS24 EQU   0
DCBOFLGS EQU   IHADCS24+8
DCBOFLWR EQU   DCBOFLGS
DCBOFOPN EQU   DCBOFLGS
DCBIFLG  EQU   IHADCS24+9
DCBIBPCT EQU   DCBIFLG
DCBMACR  EQU   IHADCS24+10
DCBMACR1 EQU   DCBMACR
DCBMRFE  EQU   DCBMACR1
DCBMRGET EQU   DCBMRFE
DCBMRAPG EQU   DCBMACR1
DCBMRRD  EQU   DCBMRAPG
DCBMRCI  EQU   DCBMACR1
DCBMRMVG EQU   DCBMRCI
DCBMRLCG EQU   DCBMACR1
DCBMRABC EQU   DCBMACR1
DCBMRPT1 EQU   DCBMRABC
DCBMRSBG EQU   DCBMRPT1
DCBMRCRL EQU   DCBMACR1
DCBMRCHK EQU   DCBMRCRL
DCBMRRDX EQU   DCBMRCHK
DCBMRDMG EQU   DCBMACR1
DCBMACR2 EQU   DCBMACR+1
DCBMRPUT EQU   DCBMACR2
DCBMRWRT EQU   DCBMACR2
DCBMRMVP EQU   DCBMACR2
DCBMR5WD EQU   DCBMACR2
DCBMRLDM EQU   DCBMR5WD
DCBMRLCP EQU   DCBMRLDM
DCBMR4WD EQU   DCBMACR2
DCBMRPT2 EQU   DCBMR4WD
DCBMRTMD EQU   DCBMRPT2
DCBMR3WD EQU   DCBMACR2
DCBMRCTL EQU   DCBMR3WD
DCBMRSTK EQU   DCBMRCTL
DCBMR1WD EQU   DCBMACR2
DCBMRSWA EQU   DCBMR1WD
DCBMRDMD EQU   DCBMRSWA
IHADCS25 EQU   0
DCBMACRF EQU   IHADCS25+2
DCBMACF1 EQU   DCBMACRF
DCBMFFE  EQU   DCBMACF1
DCBMFGET EQU   DCBMFFE
DCBMFAPG EQU   DCBMACF1
DCBMFRD  EQU   DCBMFAPG
DCBMFCI  EQU   DCBMACF1
DCBMFMVG EQU   DCBMFCI
DCBMFLCG EQU   DCBMACF1
DCBMFABC EQU   DCBMACF1
DCBMFPT1 EQU   DCBMFABC
DCBMFSBG EQU   DCBMFPT1
DCBMFCRL EQU   DCBMACF1
DCBMFCHK EQU   DCBMFCRL
DCBMFDMG EQU   DCBMACF1
DCBMACF2 EQU   DCBMACRF+1
DCBMFPUT EQU   DCBMACF2
DCBMFWRT EQU   DCBMACF2
DCBMFMVP EQU   DCBMACF2
DCBMF5WD EQU   DCBMACF2
DCBMFLDM EQU   DCBMF5WD
DCBMFLCP EQU   DCBMFLDM
DCBMF4WD EQU   DCBMACF2
DCBMFPT2 EQU   DCBMF4WD
DCBMFTMD EQU   DCBMFPT2
DCBMF3WD EQU   DCBMACF2
DCBMFCTL EQU   DCBMF3WD
DCBMFSTK EQU   DCBMFCTL
DCBMF1WD EQU   DCBMACF2
DCBMFSWA EQU   DCBMF1WD
DCBMFDMD EQU   DCBMFSWA
DCBDEBAD EQU   IHADCS25+4
DCBIFLGS EQU   DCBDEBAD
DCBIFPCT EQU   DCBIFLGS
IHADCS27 EQU   0
DCBGET   EQU   IHADCS27
DCBPUT   EQU   DCBGET
DCBGETA  EQU   DCBPUT+1
IHADCS36 EQU   0
DCBGERR  EQU   IHADCS36
DCBPERR  EQU   DCBGERR
DCBCHECK EQU   DCBPERR
DCBOPTCD EQU   DCBCHECK
DCBOPTH  EQU   DCBOPTCD
DCBOPTO  EQU   DCBOPTH
DCBOPTZ  EQU   DCBOPTCD
DCBGERRA EQU   DCBCHECK+1
DCBPERRA EQU   DCBGERRA
DCBSYNAD EQU   IHADCS36+4
DCBCIND1 EQU   IHADCS36+8
DCBCIND2 EQU   IHADCS36+9
DCBIOBA  EQU   IHADCS36+16
DCBCICB  EQU   DCBIOBA
IHADCS52 EQU   0
DCBDIRCT EQU   IHADCS52
DCBQSWS  EQU   DCBDIRCT
DCBUSASI EQU   DCBQSWS
DCBQADFS EQU   DCBUSASI
DCBBUFOF EQU   DCBDIRCT+1
IHADCS37 EQU   0
DCBFLAG1 EQU   IHADCS37
IHADCS40 EQU   0
DCBEOBAD EQU   IHADCS40
DCBCCCW  EQU   IHADCS40+4
DCBRECAD EQU   DCBCCCW
DCBRECBT EQU   DCBRECAD
DCBRCREL EQU   DCBRECBT
DCBCNTRL EQU   IHADCS40+12
DCBEROPT EQU   DCBCNTRL
INFMJFCB EQU   0
JFCBDSNM EQU   INFMJFCB
JFCBELNM EQU   INFMJFCB+44
JFCBTSDM EQU   INFMJFCB+52
JFCFCBID EQU   INFMJFCB+56
JFCBFRID EQU   JFCFCBID
JFCBLTYP EQU   INFMJFCB+66
JFCBOTTR EQU   INFMJFCB+67
JFCBUFOF EQU   JFCBOTTR
JFCBFLSQ EQU   JFCBOTTR+1
JFCFUNC  EQU   JFCBFLSQ
JFCBMASK EQU   INFMJFCB+72
JFCBFLG1 EQU   JFCBMASK+5
JFCOPEN  EQU   JFCBFLG1
JFCBFLG2 EQU   JFCBMASK+6
JFCDEFER EQU   JFCBFLG2
JFCBIND1 EQU   INFMJFCB+86
JFCBIND2 EQU   INFMJFCB+87
JFCAMPTR EQU   INFMJFCB+88
JFCBUFRQ EQU   JFCAMPTR
JFCBUFNO EQU   JFCBUFRQ
JFCBGNCP EQU   JFCAMPTR+1
JFCBHIAR EQU   JFCBGNCP
JFCBFALN EQU   JFCBHIAR
JFCBFTEK EQU   JFCBFALN
JFCBBFTA EQU   JFCBFTEK
JFCEROPT EQU   INFMJFCB+92
JFCTRTCH EQU   INFMJFCB+93
TCAST    EQU   0
TCASUSER EQU   TCAST+4
TCASUMAX EQU   TCASUSER+2
TCASACBP EQU   TCAST+8
TCASRCON EQU   TCAST+16
TCASCLSZ EQU   TCAST+18
TCASHBUF EQU   TCAST+20
TCASLBUF EQU   TCAST+24
TCASCRSZ EQU   TCAST+28
TCASCHNL EQU   TCAST+30
TCASFLG1 EQU   TCAST+88
TCASBKMD EQU   TCASFLG1
TCASMDSW EQU   TCASFLG1
TCASCONF EQU   TCASFLG1                                        ZP60007
TCASDUMP EQU   TCAST+132
DMPCD    EQU   0
DMPRCCT  EQU   DMPCD
DMPCD01  EQU   DMPCD+3
DMPCD02  EQU   DMPCD+6
DMPCD03  EQU   DMPCD+9
DMPCD04  EQU   DMPCD+12
DMPCD05  EQU   DMPCD+15
DMPCD06  EQU   DMPCD+18
DMPCD07  EQU   DMPCD+21
DMPCD08  EQU   DMPCD+24
DMPCD09  EQU   DMPCD+27
DMPCD10  EQU   DMPCD+30
DMPCD11  EQU   DMPCD+33
DMPCD12  EQU   DMPCD+36
DMPCD13  EQU   DMPCD+39
DMPCD14  EQU   DMPCD+42
DMPCD15  EQU   DMPCD+45
DMPCD16  EQU   DMPCD+48
TWAR     EQU   0
TWACSCB  EQU   TWAR+16
TWATCAST EQU   TWAR+20
TWAMSG   EQU   TWAR+48
TWAMFL   EQU   TWAR+80
TWAVFL   EQU   TWAR+81
TWAUFL   EQU   TWAR+82
TWACFL   EQU   TWAR+83
TWAM     EQU   TWAR+84
TWAME    EQU   TWAM+68
TWAMEI   EQU   TWAME
TWAV     EQU   TWAR+184
TWAVE    EQU   TWAV+68
TWAVEI   EQU   TWAVE
TWAVI    EQU   TWAR+284
TWAU     EQU   TWAR+300
TWAUE    EQU   TWAU+68
TWAUEI   EQU   TWAUE
TWAUI    EQU   TWAR+400
TWAC     EQU   TWAR+408
TWACE    EQU   TWAC+68
TWACEI   EQU   TWACE
TWACI    EQU   TWAR+508
CIBPTR   EQU   CHCIBP
CSCBPTR  EQU   TWACSCB
TCASTPTR EQU   TWATCAST
WRKPRMF  EQU   WORKPARM
WRKPRMF1 EQU   WRKPRMF
WRKPRMF2 EQU   WRKPRMF+4
WRKPRMH  EQU   WRKPRMF2+2
WRKPRMC  EQU   WRKPRMH+1
IKT013I  EQU   MSGDATA
IKT014I  EQU   MSGDATA
IKT017I  EQU   MSGDATA
IKT018I  EQU   MSGDATA
SAVAREAB EQU   SAVAREA
SAVAREAF EQU   SAVAREAB+4
SAVAREAH EQU   SAVAREAF+2
SAVAREAC EQU   SAVAREAH+1
SAVAREAD EQU   SAVAREA
SAVAREAI EQU   SAVAREAD+5
VTAMRC   EQU   SAVAREAI
VTAMFB   EQU   SAVAREAI+1
SDUMP    EQU   SAVAREAI+2
CHAINS01 EQU   CHASM
CHTYPE   EQU   CHAINS01+124
CHTRSTAT EQU   CHAINS01+125
CHCSYSO  EQU   CHAINS01+134
JFCKEYLE EQU   JFCTRTCH
JFCCODE  EQU   JFCKEYLE
JFCSTACK EQU   JFCTRTCH
JFCMODE  EQU   JFCSTACK
JFCSPPRT EQU   JFCTRTCH
JFCBABFS EQU   JFCSPPRT+2
JFCLIMCT EQU   JFCBABFS
JFCDSORG EQU   JFCSPPRT+5
JFCDSRG1 EQU   JFCDSORG
JFCDSRG2 EQU   JFCDSORG+1
JFCRECFM EQU   JFCSPPRT+7
JFCRCFM  EQU   JFCRECFM
JFCOPTCD EQU   JFCSPPRT+8
JFCWVCSP EQU   JFCOPTCD
JFCWVCIS EQU   JFCWVCSP
JFCWVCBD EQU   JFCWVCIS
JFCALLOW EQU   JFCOPTCD
JFCRSV17 EQU   JFCALLOW
JFCOVER  EQU   JFCRSV17
JFCPCIBT EQU   JFCOPTCD
JFCMAST  EQU   JFCPCIBT
JFCEXT   EQU   JFCMAST
JFCBCKPT EQU   JFCOPTCD
JFCIND   EQU   JFCBCKPT
JFCRSV18 EQU   JFCOPTCD
JFCCYL   EQU   JFCRSV18
JFCACT   EQU   JFCCYL
JFCREDUC EQU   JFCOPTCD
JFCRSV19 EQU   JFCREDUC
JFCRSV20 EQU   JFCRSV19
JFCRSV21 EQU   JFCOPTCD
JFCDEL   EQU   JFCRSV21
JFCOPTJ  EQU   JFCOPTCD
JFCREORG EQU   JFCOPTJ
JFCBLKSI EQU   JFCSPPRT+9
JFCBUFSI EQU   JFCBLKSI
JFCAMSYN EQU   JFCSPPRT+11
JFCNCP   EQU   JFCAMSYN+2
JFCNTM   EQU   JFCAMSYN+3
JFCBFSEQ EQU   JFCNTM
JFCPCI   EQU   JFCBFSEQ
JFCRESRV EQU   JFCAMSYN+4
JFCUCSEG EQU   JFCRESRV
JFCUCSOP EQU   JFCUCSEG+4
JFCOUTLI EQU   JFCUCSEG+5
JFCTHRSH EQU   JFCOUTLI
JFCCPRI  EQU   JFCTHRSH
JFCBVOLS EQU   JFCUCSEG+10
JFCBS001 EQU   JFCBVOLS
JFCBPQTY EQU   JFCBS001+34
JFCBCTRI EQU   JFCBS001+37
JFCBSQTY EQU   JFCBS001+38
JFCFLGS1 EQU   JFCBS001+41
         AGO   .@UNREFD                START UNREFERENCED COMPONENTS
JFCBEND  EQU   JFCBS001+58
JFCBSPTN EQU   JFCBS001+57
JFCBVLCT EQU   JFCBS001+56
JFCBDRLH EQU   JFCBS001+53
JFCBSBNM EQU   JFCBS001+50
JFCBABST EQU   JFCBS001+48
JFCBSPNM EQU   JFCBS001+45
JFCBDQTY EQU   JFCBS001+42
JFCBUAFF EQU   JFCFLGS1
JFCRSV45 EQU   JFCFLGS1
JFCVRDS  EQU   JFCFLGS1
JFCBCEOV EQU   JFCFLGS1
JFCRSV42 EQU   JFCFLGS1
JFCRSV41 EQU   JFCFLGS1
JFCTOPEN EQU   JFCFLGS1
JFCBDLET EQU   JFCFLGS1
JFCRQID  EQU   JFCBSQTY
JFCROUND EQU   JFCBCTRI
JFCALX   EQU   JFCBCTRI
JFCMIXG  EQU   JFCBCTRI
JFCONTIG EQU   JFCBCTRI
JFCRSV29 EQU   JFCBCTRI
JFCBMSGP EQU   JFCBCTRI
JFCBSPAC EQU   JFCBCTRI
JFCRUNIT EQU   JFCBPQTY
JFCBEXAD EQU   JFCBS001+31
JFCBEXTL EQU   JFCBS001+30
JFCMSVGP EQU   JFCBS001+22
@NM00327 EQU   JFCBS001
JFCAVOLS EQU   JFCBVOLS
JFCBNVOL EQU   JFCUCSEG+9
JFCBNTCS EQU   JFCUCSEG+8
JFCSOWA  EQU   JFCOUTLI+1
JFCSEND  EQU   JFCCPRI
JFCEQUAL EQU   JFCCPRI
JFCRECV  EQU   JFCCPRI
JFCRSV34 EQU   JFCCPRI
JFCRSV33 EQU   JFCCPRI
JFCRSV55 EQU   JFCCPRI
JFCRSV54 EQU   JFCCPRI
JFCRSV53 EQU   JFCCPRI
JFCRSV27 EQU   JFCUCSOP
JFCRSV26 EQU   JFCUCSOP
JFCFCBVR EQU   JFCUCSOP
JFCFCBAL EQU   JFCUCSOP
JFCVER   EQU   JFCUCSOP
JFCRSV25 EQU   JFCUCSOP
JFCFOLD  EQU   JFCUCSOP
JFCBEXTP EQU   JFCUCSOP
JFCUCSID EQU   JFCUCSEG
JFCINTVL EQU   JFCSPPRT+19
JFCDBUFN EQU   JFCRESRV+3
JFCCYLOF EQU   JFCRESRV+2
JFCRKP   EQU   JFCRESRV
JFCPCIR2 EQU   JFCPCI
JFCPCIR1 EQU   JFCPCI
JFCPCIN2 EQU   JFCPCI
JFCPCIN1 EQU   JFCPCI
JFCPCIA2 EQU   JFCPCI
JFCPCIA1 EQU   JFCPCI
JFCPCIX2 EQU   JFCPCI
JFCPCIX1 EQU   JFCPCI
JFCBUFMX EQU   JFCNCP
JFCLRECL EQU   JFCAMSYN
JFCBAXBF EQU   JFCBUFSI
JFCREL   EQU   JFCREORG
JFCRSV22 EQU   JFCDEL
JFCSRCHD EQU   JFCRSV20
JFCOPTQ  EQU   JFCACT
JFCFEED  EQU   JFCIND
JFCCBWU  EQU   JFCEXT
JFCWUMSG EQU   JFCOVER
JFCSDNAM EQU   JFCWVCBD
@NM00326 EQU   JFCRECFM
JFCCHAR  EQU   JFCRECFM
JFCRFS   EQU   JFCRECFM
JFCRFB   EQU   JFCRECFM
JFCRFO   EQU   JFCRCFM
JFCFMREC EQU   JFCRCFM
JFCRSV16 EQU   JFCDSRG2
JFCRSV15 EQU   JFCDSRG2
JFCORGTR EQU   JFCDSRG2
JFCORGAM EQU   JFCDSRG2
JFCRSV13 EQU   JFCDSRG2
JFCORGTQ EQU   JFCDSRG2
JFCORGTX EQU   JFCDSRG2
JFCORGGS EQU   JFCDSRG2
JFCORGU  EQU   JFCDSRG1
JFCORGPO EQU   JFCDSRG1
JFCORGMQ EQU   JFCDSRG1
JFCORGCQ EQU   JFCDSRG1
JFCORGCX EQU   JFCDSRG1
JFCORGDA EQU   JFCDSRG1
JFCORGPS EQU   JFCDSRG1
JFCORGIS EQU   JFCDSRG1
JFCTRKBL EQU   JFCLIMCT+1
@NM00325 EQU   JFCLIMCT
JFCDEN   EQU   JFCSPPRT+1
JFCPRTSP EQU   JFCSPPRT
JFCONE   EQU   JFCMODE
JFCTWO   EQU   JFCMODE
JFCRSV07 EQU   JFCMODE
JFCRSV06 EQU   JFCMODE
JFCMODER EQU   JFCMODE
JFCMODEO EQU   JFCMODE
JFCEBCD  EQU   JFCMODE
JFCBIN   EQU   JFCMODE
JFCRSV32 EQU   JFCCODE
JFCTTY   EQU   JFCCODE
JFCASCII EQU   JFCCODE
JFCNCR   EQU   JFCCODE
JFCBUR   EQU   JFCCODE
JFCFRI   EQU   JFCCODE
JFCBCD   EQU   JFCCODE
JFCNOCON EQU   JFCCODE
CHLAST   EQU   CHAINS01+136
CHSPA    EQU   CHAINS01+135
CHUSERID EQU   CHCSYSO
CHJB     EQU   CHCSYSO
CHDUMP   EQU   CHCSYSO
CHQUE    EQU   CHCSYSO
CHHOLD   EQU   CHCSYSO
CHOUT    EQU   CHCSYSO
CHINN    EQU   CHCSYSO
CHALL    EQU   CHCSYSO
CHINC    EQU   CHAINS01+132
CHPEND   EQU   CHAINS01+128
CHARID   EQU   CHAINS01+127
CHCNID   EQU   CHAINS01+126
@NM00300 EQU   CHTRSTAT
CHTSL    EQU   CHTRSTAT
CHTS     EQU   CHTRSTAT
CHINITL  EQU   CHTRSTAT
CHINIT   EQU   CHTRSTAT
CHJOBSL  EQU   CHTRSTAT
CHJOBS   EQU   CHTRSTAT
CHDEF    EQU   CHTYPE
CHHIAR   EQU   CHTYPE
CHARSV29 EQU   CHTYPE
CHARSV28 EQU   CHTYPE
CHARSV27 EQU   CHTYPE
CHARSV26 EQU   CHTYPE
CHARSV25 EQU   CHTYPE
CHDSTAT  EQU   CHTYPE
CHBUF    EQU   CHAINS01
@NM00047 EQU   SAVAREAD
@NM00046 EQU   SAVAREAH
@NM00045 EQU   SAVAREAF
@NM00044 EQU   SAVAREAB
@NM00003 EQU   WRKPRMH
@NM00002 EQU   WRKPRMF2
TWAEND   EQU   TWAR+536
TWAWORKE EQU   TWAR+524
@NM00340 EQU   TWACI+13
TWACSKIP EQU   TWACI+12
TWACMODQ EQU   TWACI+8
TWACSTPQ EQU   TWACI+4
TWACECB  EQU   TWACI
TWACERRS EQU   TWACE+4
TWACERA  EQU   TWACEI+1
TWACEIFC EQU   TWACEI
TWACRTFC EQU   TWAC+60
TWACABFC EQU   TWAC+56
TWACTWA  EQU   TWAC+52
TWACEWA  EQU   TWAC+4
TWACID   EQU   TWAC
TWAUACQH EQU   TWAUI+4
TWAUECB  EQU   TWAUI
TWAUERRS EQU   TWAUE+4
TWAUERA  EQU   TWAUEI+1
TWAUEIFC EQU   TWAUEI
TWAURTFC EQU   TWAU+60
TWAUABFC EQU   TWAU+56
TWAUTWA  EQU   TWAU+52
TWAUEWA  EQU   TWAU+4
TWAUID   EQU   TWAU
TWAVACQH EQU   TWAVI+12
TWAVTHQH EQU   TWAVI+8
TWAVTEQH EQU   TWAVI+4
TWAVECB  EQU   TWAVI
TWAVERRS EQU   TWAVE+4
TWAVERA  EQU   TWAVEI+1
TWAVEIFC EQU   TWAVEI
TWAVRTFC EQU   TWAV+60
TWAVABFC EQU   TWAV+56
TWAVTWA  EQU   TWAV+52
TWAVEWA  EQU   TWAV+4
TWAVID   EQU   TWAV
TWAMERRS EQU   TWAME+4
TWAMERA  EQU   TWAMEI+1
TWAMEIFC EQU   TWAMEI
TWAMRTFC EQU   TWAM+60
TWAMABFC EQU   TWAM+56
TWAMTWA  EQU   TWAM+52
TWAMEWA  EQU   TWAM+4
TWAMID   EQU   TWAM
@NM00339 EQU   TWACFL
TWACFL4  EQU   TWACFL
TWACFL2  EQU   TWACFL
TWACFL1  EQU   TWACFL
@NM00338 EQU   TWAUFL
TWAUFL4  EQU   TWAUFL
TWAUFL3  EQU   TWAUFL
TWAUFL2  EQU   TWAUFL
TWAUFL1  EQU   TWAUFL
@NM00337 EQU   TWAVFL
TWAVFL6  EQU   TWAVFL
TWAVFL5  EQU   TWAVFL
TWAVFL4  EQU   TWAVFL
TWAVFL3  EQU   TWAVFL
TWAVFL2  EQU   TWAVFL
TWAVFL1  EQU   TWAVFL
@NM00336 EQU   TWAMFL
TWAMFL1  EQU   TWAMFL
TWACCOMP EQU   TWAR+76
TWAUCOMP EQU   TWAR+72
TWAVCOMP EQU   TWAR+68
TWAMECB  EQU   TWAR+64
TWACTCB  EQU   TWAR+60
TWAUTCB  EQU   TWAR+56
TWAVTCB  EQU   TWAR+52
TWADEQAS EQU   TWAR+44
TWAEESR  EQU   TWAR+40
TWAPPSR  EQU   TWAR+36
TWATCSR  EQU   TWAR+32
TWATTSR  EQU   TWAR+28
TWAINIT  EQU   TWAR+24
TWAASCB  EQU   TWAR+12
TWAPASQH EQU   TWAR+8
TWASYNQH EQU   TWAR+4
TWARSON  EQU   TWAR+2
TWACOMP  EQU   TWAR
DMPOP16  EQU   DMPCD16+2
DMPFB16  EQU   DMPCD16+1
DMPRC16  EQU   DMPCD16
DMPOP15  EQU   DMPCD15+2
DMPFB15  EQU   DMPCD15+1
DMPRC15  EQU   DMPCD15
DMPOP14  EQU   DMPCD14+2
DMPFB14  EQU   DMPCD14+1
DMPRC14  EQU   DMPCD14
DMPOP13  EQU   DMPCD13+2
DMPFB13  EQU   DMPCD13+1
DMPRC13  EQU   DMPCD13
DMPOP12  EQU   DMPCD12+2
DMPFB12  EQU   DMPCD12+1
DMPRC12  EQU   DMPCD12
DMPOP11  EQU   DMPCD11+2
DMPFB11  EQU   DMPCD11+1
DMPRC11  EQU   DMPCD11
DMPOP10  EQU   DMPCD10+2
DMPFB10  EQU   DMPCD10+1
DMPRC10  EQU   DMPCD10
DMPOP09  EQU   DMPCD09+2
DMPFB09  EQU   DMPCD09+1
DMPRC09  EQU   DMPCD09
DMPOP08  EQU   DMPCD08+2
DMPFB08  EQU   DMPCD08+1
DMPRC08  EQU   DMPCD08
DMPOP07  EQU   DMPCD07+2
DMPFB07  EQU   DMPCD07+1
DMPRC07  EQU   DMPCD07
DMPOP06  EQU   DMPCD06+2
DMPFB06  EQU   DMPCD06+1
DMPRC06  EQU   DMPCD06
DMPOP05  EQU   DMPCD05+2
DMPFB05  EQU   DMPCD05+1
DMPRC05  EQU   DMPCD05
DMPOP04  EQU   DMPCD04+2
DMPFB04  EQU   DMPCD04+1
DMPRC04  EQU   DMPCD04
DMPOP03  EQU   DMPCD03+2
DMPFB03  EQU   DMPCD03+1
DMPRC03  EQU   DMPCD03
DMPOP02  EQU   DMPCD02+2
DMPFB02  EQU   DMPCD02+1
DMPRC02  EQU   DMPCD02
DMPOP01  EQU   DMPCD01+2
DMPFB01  EQU   DMPCD01+1
DMPRC01  EQU   DMPCD01
@NM00335 EQU   DMPCD+1
TCASTEND EQU   TCAST+148
TCASTTYO EQU   TCAST+144
TCAS767O EQU   TCAST+140
TCAS767I EQU   TCAST+136
TCASSCHD EQU   TCAST+128
TCASTPND EQU   TCAST+124
TCASOMJR EQU   TCAST+120
@NM00334 EQU   TCAST+118
TCASBR14 EQU   TCAST+116
TCASATTN EQU   TCAST+112
TCASASCI EQU   TCAST+108
TCASTTQH EQU   TCAST+104
@NM00333 EQU   TCAST+102
@NM00332 EQU   TCAST+100
@NM00331 EQU   TCAST+98
@NM00330 EQU   TCAST+96
TCASASCB EQU   TCAST+92
TCASFLG4 EQU   TCAST+91
TCASFLG3 EQU   TCAST+90
TCASFLG2 EQU   TCAST+89
@NM00329 EQU   TCASFLG1
TCASNACT EQU   TCASFLG1
TCASVSD  EQU   TCASFLG1
TCASABND EQU   TCASFLG1
TCASLTE  EQU   TCAST+84
TCASEXIT EQU   TCAST+80
TCASOQM  EQU   TCAST+76
TCASIQM  EQU   TCAST+72
TCASTSB  EQU   TCAST+68
TCASTTL  EQU   TCAST+64
TCASWA   EQU   TCAST+60
TCASFRR  EQU   TCAST+56
TCASMSGS EQU   TCAST+52
TCASDATO EQU   TCAST+48
TCASDATI EQU   TCAST+44
TCASXECB EQU   TCAST+40
TCASTID  EQU   TCAST+32
@NM00328 EQU   TCAST+31
TCASUSEC EQU   TCASUSER
TCASID   EQU   TCAST
JFCRSV05 EQU   JFCEROPT
JFCRSV04 EQU   JFCEROPT
JFCRSV03 EQU   JFCEROPT
JFCRSV02 EQU   JFCEROPT
JFCTOPT  EQU   JFCEROPT
JFCABN   EQU   JFCEROPT
JFCSKP   EQU   JFCEROPT
JFCACC   EQU   JFCEROPT
JFCBUFL  EQU   JFCAMPTR+2
JFCFWORD EQU   JFCBFTEK
JFCDWORD EQU   JFCBFTEK
JFCHIER1 EQU   JFCBFTEK
JFCDYN   EQU   JFCBFTEK
JFCEXC   EQU   JFCBFTEK
JFCBBFTR EQU   JFCBBFTA
JFCSIM   EQU   JFCBBFTA
JFCHIER  EQU   JFCBFTEK
JFCBFOUT EQU   JFCBUFNO
JFCBUFIN EQU   JFCBUFNO
JFCTEMP  EQU   JFCBIND2
JFCREQ   EQU   JFCBIND2
JFCENT   EQU   JFCBIND2
JFCSHARE EQU   JFCBIND2
JFCSECUR EQU   JFCBIND2
JFCDISP  EQU   JFCBIND2
JFCPDS   EQU   JFCBIND1
JFCGDG   EQU   JFCBIND1
JFCADDED EQU   JFCBIND1
JFCLOC   EQU   JFCBIND1
JFCRLSE  EQU   JFCBIND1
JFCBXPDT EQU   INFMJFCB+83
JFCBCRDT EQU   INFMJFCB+80
JFCBOPS2 EQU   JFCBMASK+7
JFCRCTLG EQU   JFCBFLG2
JFCBBUFF EQU   JFCBFLG2
JFCTRACE EQU   JFCBFLG2
JFCSDRPS EQU   JFCBFLG2
JFCMODNW EQU   JFCBFLG2
JFCNRPS  EQU   JFCDEFER
JFCOUTOP EQU   JFCBFLG2
JFCINOP  EQU   JFCBFLG2
JFCBPWBP EQU   JFCOPEN
@NM00324 EQU   JFCOPEN
JFCDUAL  EQU   JFCBFLG1
JFCSLDES EQU   JFCBFLG1
JFCSLCRE EQU   JFCBFLG1
JFCSTAND EQU   JFCBFLG1
JFCBOPS1 EQU   JFCBMASK
JFCBVLSQ EQU   INFMJFCB+70
JFCRSV31 EQU   JFCFUNC
JFCFNCBT EQU   JFCFUNC
JFCFNCBX EQU   JFCFUNC
JFCFNCBD EQU   JFCFUNC
JFCFNCBW EQU   JFCFUNC
JFCFNCBP EQU   JFCFUNC
JFCFNCBR EQU   JFCFUNC
JFCFNCBI EQU   JFCFUNC
JFCBFOFL EQU   JFCBUFOF
JFCNL    EQU   JFCBLTYP
JFCSL    EQU   JFCBLTYP
JFCNSL   EQU   JFCBLTYP
JFCSUL   EQU   JFCBLTYP
JFCBLP   EQU   JFCBLTYP
JFCBLTM  EQU   JFCBLTYP
JFCBAL   EQU   JFCBLTYP
JFCRSV38 EQU   JFCBLTYP
JFCVINDX EQU   INFMJFCB+64
JFCNLREC EQU   INFMJFCB+62
JFCBADBF EQU   INFMJFCB+60
JFCAMSTR EQU   JFCBFRID+2
JFCAMCRO EQU   JFCBFRID
JFCBDSCB EQU   INFMJFCB+53
JFCPAT   EQU   JFCBTSDM
JFCNDCB  EQU   JFCBTSDM
JFCNDSCB EQU   JFCBTSDM
JFCNWRIT EQU   JFCBTSDM
JFCTTR   EQU   JFCBTSDM
JFCSDS   EQU   JFCBTSDM
JFCVSL   EQU   JFCBTSDM
JFCCAT   EQU   JFCBTSDM
JFCIPLTX EQU   JFCBELNM
JFCBQNAM EQU   JFCBDSNM
DCBEOB   EQU   IHADCS40+20
DCBPRECL EQU   IHADCS40+18
@NM00323 EQU   IHADCS40+16
DCBCNTRA EQU   DCBCNTRL+1
@NM00322 EQU   DCBEROPT
DCBERABE EQU   DCBEROPT
DCBERSKP EQU   DCBEROPT
DCBERACC EQU   DCBEROPT
DCBLRECL EQU   IHADCS40+10
@NM00321 EQU   IHADCS40+9
@NM00320 EQU   IHADCS40+8
DCBRECA  EQU   DCBRECAD+1
@NM00319 EQU   DCBRECBT
DCBRCFGT EQU   DCBRCREL
DCBRCTRU EQU   DCBRCREL
DCBLCCW  EQU   DCBEOBAD
@NM00318 EQU   DCBFLAG1
DCBSTFLS EQU   DCBFLAG1
DCBSTQCK EQU   DCBFLAG1
DCBDIRCQ EQU   DCBBUFOF
DCBQSTRU EQU   DCBUSASI
@NM00317 EQU   DCBUSASI
DCBQADF3 EQU   DCBQADFS
DCBQADF2 EQU   DCBQADFS
DCBQADF1 EQU   DCBQADFS
DCBBLBP  EQU   DCBUSASI
@NM00316 EQU   DCBUSASI
DCBCICBA EQU   DCBCICB+1
@NM00315 EQU   DCBCICB
DCBOFFSW EQU   IHADCS36+15
DCBOFFSR EQU   IHADCS36+14
DCBWCPL  EQU   IHADCS36+13
DCBWCPO  EQU   IHADCS36+12
DCBBLKSI EQU   IHADCS36+10
DCBCNQSM EQU   DCBCIND2
DCBCNFEO EQU   DCBCIND2
DCBCNCHS EQU   DCBCIND2
DCBCNBFP EQU   DCBCIND2
DCBCNIOE EQU   DCBCIND2
DCBCNCLO EQU   DCBCIND2
DCBCNWRO EQU   DCBCIND2
DCBCNSTO EQU   DCBCIND2
DCBCNEXB EQU   DCBCIND1
@NM00314 EQU   DCBCIND1
DCBCNBRM EQU   DCBCIND1
@NM00313 EQU   DCBCIND1
DCBCNEVA EQU   DCBCIND1
DCBCNEVB EQU   DCBCIND1
DCBCNSRD EQU   DCBCIND1
DCBCNTOV EQU   DCBCIND1
DCBSYNA  EQU   DCBSYNAD+1
DCBIOBL  EQU   DCBSYNAD
DCBCHCKA EQU   DCBPERRA
DCBOPTJ  EQU   DCBOPTCD
DCBOPTT  EQU   DCBOPTCD
DCBSRCHD EQU   DCBOPTZ
DCBOPTQ  EQU   DCBOPTCD
DCBBCKPT EQU   DCBOPTO
DCBOPTC  EQU   DCBOPTCD
DCBOPTU  EQU   DCBOPTCD
DCBOPTW  EQU   DCBOPTCD
DCBPUTA  EQU   DCBGETA
@NM00312 EQU   DCBPUT
DCBDEBA  EQU   DCBDEBAD+1
@NM00311 EQU   DCBIFLGS
DCBIFIOE EQU   DCBIFLGS
DCBCH12  EQU   DCBIFPCT
DCBCH9   EQU   DCBIFPCT
DCBIFEC  EQU   DCBIFLGS
DCBMFSTI EQU   DCBMFDMD
DCBMFAWR EQU   DCBMFSTK
DCBMFUIP EQU   DCBMFTMD
DCBMFIDW EQU   DCBMFLCP
DCBMFWRK EQU   DCBMFMVP
DCBMFRDQ EQU   DCBMFWRT
DCBMFGTQ EQU   DCBMFPUT
DCBMFSTL EQU   DCBMACF2
DCBMFCK  EQU   DCBMFDMG
DCBMFRDX EQU   DCBMFCHK
DCBMFDBF EQU   DCBMFSBG
DCBMFRDI EQU   DCBMFLCG
DCBMFRDK EQU   DCBMFMVG
DCBMFWRQ EQU   DCBMFRD
DCBMFPTQ EQU   DCBMFGET
DCBMFECP EQU   DCBMACF1
DCBTIOT  EQU   IHADCS25
DCBMRSTI EQU   DCBMRDMD
DCBMRAWR EQU   DCBMRSTK
DCBMRUIP EQU   DCBMRTMD
DCBMRIDW EQU   DCBMRLCP
DCBMRWRK EQU   DCBMRMVP
DCBMRRDQ EQU   DCBMRWRT
DCBMRGTQ EQU   DCBMRPUT
DCBMRSTL EQU   DCBMACR2
DCBMRCK  EQU   DCBMRDMG
DCBPGFXA EQU   DCBMRRDX
DCBMRDBF EQU   DCBMRSBG
DCBMRRDI EQU   DCBMRLCG
DCBMRRDK EQU   DCBMRMVG
DCBMRWRQ EQU   DCBMRRD
DCBMRPTQ EQU   DCBMRGET
DCBMRECP EQU   DCBMACR1
@NM00310 EQU   DCBIFLG
DCBIBIOE EQU   DCBIFLG
DCBICH12 EQU   DCBIBPCT
DCBICH9  EQU   DCBIBPCT
DCBIBEC  EQU   DCBIFLG
DCBOFIOF EQU   DCBOFLGS
DCBOFUEX EQU   DCBOFLGS
DCBOFTM  EQU   DCBOFLGS
DCBOFPPC EQU   DCBOFLGS
DCBOFEOV EQU   DCBOFLGS
DCBOFLRB EQU   DCBOFLGS
DCBOFIOD EQU   DCBOFLWR
DCBDDNAM EQU   IHADCS24
DCBEXLSA EQU   DCBEXLST+1
DCBRECKL EQU   DCBRECFM
DCBRECCC EQU   DCBRECFM
DCBRECSB EQU   DCBRECFM
DCBRECBR EQU   DCBRECFM
DCBRECTO EQU   DCBRECLA
DCBRECL  EQU   DCBRECLA
DCBEODA  EQU   DCBEODAD+1
DCBBFA   EQU   DCBBFTEK
DCBH0    EQU   DCBBFTEK
DCBBFTKD EQU   DCBBFTEK
DCBBFTE  EQU   DCBBFT
DCBBFTKR EQU   DCBBFT
DCBBFTS  EQU   DCBBFT
DCBH1    EQU   DCBBFTEK
DCBSVCXA EQU   DCBSVCXL+1
@NM00309 EQU   DCBSVCXL
DCBODEBA EQU   DCBIOBAA
@NM00308 EQU   DCBQSLM
DCBUPDBT EQU   DCBQSLM
DCBUPDCM EQU   DCBQSLM
DCB1DVDS EQU   DCBQSLM
@NM00307 EQU   DCBDSRG2
DCBDSGTR EQU   DCBDSRG2
DCBACBM  EQU   DCBDSRG2
@NM00306 EQU   DCBDSRG2
DCBDSGTQ EQU   DCBDSRG2
DCBDSGTX EQU   DCBDSRG2
DCBDSGGS EQU   DCBDSRG2
DCBDSGU  EQU   DCBDSRG1
DCBDSGPO EQU   DCBDSRG1
@NM00305 EQU   DCBDSRG1
@NM00304 EQU   DCBDSRG1
DCBDSGCX EQU   DCBDSRG1
DCBDSGDA EQU   DCBDSRG1
DCBDSGPS EQU   DCBDSRG1
DCBDSGIS EQU   DCBDSRG1
DCBBUFL  EQU   IHADCS11+8
DCBBUFCA EQU   DCBBUFCB+1
DCBBUFNO EQU   DCBBUFCB
DCBDEVT  EQU   DCBREL
DCBKEYLE EQU   DCBRELB
@NM00303 EQU   IHADCB
@NM00302 EQU   CIBNAME+13
CIBCONID EQU   CIBNAME+12
CIBTJID  EQU   CIBASID
@NM00301 EQU   CIBNAME+6
CIBLEN   EQU   CIBNAME+5
CIBVERB  EQU   CIBNAME+4
CIBNEXT  EQU   CIBNAME
CHARSV16 EQU   CHSHORT+100
CHARSV15 EQU   CHSHORT+96
CHJSCBVS EQU   CHSHORT+92
CHUSCVS  EQU   CHSHORT+88
@NM00299 EQU   CHSHORT+84
@NM00298 EQU   CHSHORT+80
CHSQA    EQU   CHSHORT+44
CHQPA    EQU   CHSHORT+8
CHJCL    EQU   CHSHORT+4
CHSPC    EQU   CHSHORT
CHSPB    EQU   CHSTEP+4
CHTCB    EQU   CHSTEP+1
CHARSV23 EQU   CHSWT
CHARSV22 EQU   CHSWT
CHARSV21 EQU   CHSWT
CHAC     EQU   CHSWT
CHPSF    EQU   CHSWT
CHPSD    EQU   CHSWT
CHJCT    EQU   CHSWT
CHARSV20 EQU   CHSWT
CHCECB   EQU   CHASM+20
CHECB    EQU   CHASM+16
CHRGNAD  EQU   CHASM+12
CHARSV18 EQU   CHRGNSZ+1
CHRPRTY  EQU   CHRGNSZ
@NM00297 EQU   CHCIBP
CHEND    EQU   CHCIBP
CHECBP   EQU   CHASM
CHDER    EQU   CHPROCSN+4
CHARSV10 EQU   CHACT1
CHARSV09 EQU   CHACT1
CHARSV08 EQU   CHACT1
CHARSV07 EQU   CHACT1
CHARSV06 EQU   CHACT1
CHARSV05 EQU   CHACT1
CHARSV04 EQU   CHACT1
CHRDWTR  EQU   CHACT1
CHARSV30 EQU   CHPROCSN+2
CHQID    EQU   CHPROCSN
CHASID   EQU   CHTJID
CHUCMP   EQU   CHAIN+29
CHTRKID  EQU   CHPKE
CHCIBCTR EQU   CHAIN+27
CHUNIT   EQU   CHAIN+24
CHCLS    EQU   CHAIN+16
CHPCOI   EQU   CHPARM
CHIFY    EQU   CHACT
CHAIFX   EQU   CHACT
CHCLD    EQU   CHACT
CHCL     EQU   CHACT
CHDSI    EQU   CHACT
CHDISC   EQU   CHACT
CHTERM   EQU   CHACT
CHSWAP   EQU   CHACT
CHABTERM EQU   CHSTS
CHFC     EQU   CHSTS
CHDL     EQU   CHSTS
CHAD     EQU   CHSTS
CHQSPC   EQU   CHSTS
CHSOUT   EQU   CHSTS
CHSYS    EQU   CHSTS
CHAP     EQU   CHSTS
CHSZE    EQU   CHFLG+1
CHVCD    EQU   CHFLG
CHPTR    EQU   CHAIN
@NM00296 EQU   TBLFLAG
@NM00295 EQU   SCANTBL+10
@NM00294 EQU   TBLNAME+1
@NM00088 EQU   RSTDNAME+1
@NM00043 EQU   SAVAREA+1
@NM00010 EQU   MLPMPML+2
MLPMERR  EQU   MLPMFL2
@NM00009 EQU   MLPMFL2
MLPMSTRT EQU   MLPMFL1
@NM00008 EQU   MLPMFL1
MSGLNGTH EQU   @NM00007
.@UNREFD ANOP                          END UNREFERENCED COMPONENTS
@RC00169 EQU   @EL00002
@RT00203 EQU   @EL00003
@RC00270 EQU   @EL00003
@RF00269 EQU   @EL00003
@RT00281 EQU   @EL00004
@RF00285 EQU   @EL00004
@RT00296 EQU   @EL00005
@RT00348 EQU   SYNAD
@RC00428 EQU   @RC00426
@RF00416 EQU   @EL00005
@RF00475 EQU   @EL00007
@RT00491 EQU   @EL00008
@RT00551 EQU   @EL00009
@RC00658 EQU   @EL00011
@RT00683 EQU   @EL00012
@RF00686 EQU   @EL00012
@RF00716 EQU   @EL00013
@RT00745 EQU   @EL00014
@RC00723 EQU   @EL00014
@RF00777 EQU   @EL00014
@RF00793 EQU   @EL00015
@RF00804 EQU   @EL00016
@RC00849 EQU   @EL00017
@RC00173 EQU   @RC00169
@ENDDATA EQU   *
         END   IKTCAS54,(C'PLS2030',0702,79255)
/*
//*
//STEP3   EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
  IDENTIFY IKTCAS54('ZP60007')
++ZAP(IKTXINIT) DISTLIB(AOST4).
 NAME IKTXINIT
 IDRDATA ZP60007
VER 001A 05C0                BALR  @12,0
VER 058E D23F,9000,CB54      MVC   ISTNIB(64,@09),DUMNIB
VER 0594 D207,900C,1068      MVC   NIBSYM(8,@09),TSBTRMID(@01)
VER 059A 58A0,A03C           L     @10,ASCBTSB(,@10)
VER 059E 50A0,9008           ST    @10,NIBUSER(,@09)
VER 05A2 9504,F048           CLI   TSBXTMTP(@15),4
VER 09FC 0000,0000  PATAREA  ***   PATCH AREA   ***
VER 0A00 0000,0000           ***   PATCH AREA   ***
VER 0A04 0000,0000           ***   PATCH AREA   ***
VER 0A08 0000,0000           ***   PATCH AREA   ***
VER 0A0C 0000,0000           ***   PATCH AREA   ***
VER 0A10 0000,0000           ***   PATCH AREA   ***
VER 0A14 0000,0000           ***   PATCH AREA   ***
VER 0B70 D000       DUMNIB   NIB
VER 0B94 0201                (CHECK CONFTXT IS ON)
REP 059E 47F0,C9E0           B     PATAREA
REP 09FC 50A0,9008           ST    @10,NIBUSER(,@09)
REP 0A00 58A0,0010           L     @10,CVTPTR
REP 0A04 58A0,A3F4           L     @10,CVTTCASP
REP 0A08 9101,A058           TM    TCASFLG1,TCASCONF
REP 0A0C 4710,C586           BO    **BACK-TO-INLINE-CODE**
REP 0A10 94FE,9025           NI    PROPROC2,255-PROCFTX
REP 0A14 47F0,C586           B     **BACK-TO-INLINE-CODE**
++MACUPD(IKTTCAST) DISTLIB(ATSOMAC).
./ CHANGE NAME=IKTTCAST
TCASCONF EQU   X'01'              RESTRICTED BUFFERS           ZP60007  04009000
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//*
//STEP4   EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN DD  DSN=&&SMPMCS,DISP=(OLD,DELETE)
//SMPCNTL  DD  *
  RECEIVE
          SELECT(ZP60007)
          .
/*
//*
//STEP5CK EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60007)
        CHECK
        .
/*
//*
//STEP5   EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60007)
        DIS(WRITE)
        .
/*
//
