//ZP60008  JOB (SYSGEN),'J04 M23: ZP60008',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*  ADD EWA AND WSF SUPPORT FOR LOCAL NON-SNA 3270 TO VTAM.
//*
//STEP1   EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
++USERMOD(ZP60008)         /* ADD VTAM EWA AND WSF SUPPORT */  .
++VER(Z038) FMID(EVT0108)
 /*
   PROBLEM DESCRIPTION:
     ERASE/WRITE ALTERNATE AND WRITE STRUCTURED FIELD NOT SUPPORTED.
       VTAM APPLICATIONS CANNOT USE EXTENDED 3270 FACILITIES SINCE
       VTAM DOES NOT SUPPORT THE WRITE STRUCTURED FIELD (WSF)
       COMMAND NECESSARY TO ISSUE A READ PARTITION (QUERY) TO THE
       TERMINAL, AND THE ERASE/WRITE ALTERNATE (EWA) COMMAND
       NECESSARY TO SWITCH A 3270 SCREEN INTO ITS ALTERNATE AND
       USUALLY LARGER SCREEN SIZE.

       THIS USERMOD ADDS SUPPORT TO VTAM FOR THE WSF AND EWA COMMANDS
       FOR LOCAL NON-SNA 3270 TERMINALS ONLY.  THE RELEVANT COMMAND
       CODES (X'7E' FOR EWA AND X'F3' FOR WSF) HAVE NON-ZERO ENTRIES
       INSERTED INTO THE TRANSLATE TABLE OF THE 3270 SEND/RECEIVE LOCAL
       TRANSLATION CONTROLLER ISTZBF0L WHICH ARE DECODED BY THE 3270
       LOCAL WRITE CCW PROCESSOR ISTZBFBA SO THAT THE CORRECT CCW
       OPCODES ARE SET.

   SPECIAL CONDITIONS:
     ACTION:
       A "CLPA" MUST BE PERFORMED AT IPL TIME FOR THIS SYSMOD TO
       BECOME ACTIVE.
     DOC:
       WSF AND EWA ARE NOW VALID VTAM WRITE COMMANDS FOR LOCAL NON-SNA
       3270 TERMINALS.  IT IS UP TO THE VTAM APPLICATION TO DETERMINE
       IF THE USE OF EITHER OF THESE COMMANDS IS APPROPRIATE FOR THE
       3270 HARDWARE (OR EMULATION(S) THEREOF) INVOLVED.

   COMMENTS:
     PRYCROFT SIX P/L PUBLIC DOMAIN USERMOD FOR MVS 3.8 NUMBER 8.
       THE ZAP DOES NOT VERIFY THAT THE TRANSLATE TABLE ENTRIES BEING
       REPLACED ARE NULL, SO THIS USERMOD CAN BE REAPPLIED IF NECESSARY.

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
     MODULES:
       ISTZBFBA
       ISTZBF0L
 */.
++MOD(ISTZBFBA) DISTLIB(AOS24).
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(NEW,PASS),UNIT=SYSALLDA,
//             SPACE=(CYL,3),
//             DCB=(DSORG=PS,RECFM=FB,LRECL=80,BLKSIZE=4080)
//SYSIN    DD  DUMMY
//*
//STEP2   EXEC PGM=IFOX00,PARM='OBJECT,NODECK,NOTERM,XREF(SHORT),RENT'
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT2   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT3   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.SMPMTS,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//SYSGO    DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  *
         TITLE 'ISTZBFBA/ABA-VS1/MBA-VS2 - 3270 LOCAL WRITE CCW PROCESS*
               SOR      '
*
*  MODIFIED BY GREG PRICE 15TH DECEMBER 2002 FOR USERMOD ZP60008
*           TO ADD EWA AND WSF SUPPORT FOR LOCAL NON-SNA 3270
*
ISTZBFBA CSECT ,                                                   0001
@MAINENT DS    0H                                                  0001
         USING *,@15                                               0001
         B     @PROLOG                                             0001
         DC    AL1(16)                                             0001
         DC    C'ISTZBFBA  75.153'                                 0001
         DROP  @15
@PROLOG  DS    0H                                                  0002
*                                                                  0536
*/* TPZSAVE (8,12,14) AC(BCBA)BASE                                   */
*                                                                  0536
*   DO;                                                            0536
*     RFY                                                          0537
*      (GPR01P,                                                    0537
*       GPR12P,                                                    0537
*       GPR13P,                                                    0537
*       GPR14P) RSTD;                                              0537
*     GEN(PRINT OFF);                                              0538
*                                                                  0538
         PRINT OFF
*/* ATBUILD (ISTZSAVE)                                               */
*/* END OF ATBUILD                                                   */
*                                                                  0539
*     GEN(PRINT ON);                                               0539
         PRINT ON
*     RFY                                                          0540
*       ISTZSAVE BASED(GPR13P);                                    0540
*     GEN SETS(ZSAZSVVR)(STM   8,12,ZSAZSVVR+4(13));               0541
         STM   8,12,ZSAZSVVR+4(13)
*     GEN SETS(GPR12P);                                            0542
         BALR  GPR12P,0
@TSTART  DS    0H
         USING @TSTART,GPR12P
*     GEN(PRINT OFF);                                              0543
*                                                                  0543
         PRINT OFF
*/* ATBUILD (ISTZCRR,ISTRPH,ISTCRA) EJECT(NO)                        */
*/* END OF ATBUILD                                                   */
*                                                                  0544
*     GEN(PRINT ON);                                               0544
         PRINT ON
*     GPR01P->RPHCRR->CRACRR->ZCREBCBA='1'B;                       0545
         L     @15,RPHCRR(,GPR01P)                                 0545
         L     @15,CRACRR(,@15)                                    0545
         OI    ZCREBCBA(@15),B'00100000'                           0545
*     ZSAZACTV='1'B;                                               0546
         OI    ZSAZACTV(GPR13P),B'10000000'                        0546
*     ZSAZTRAC='BCBA';                                             0547
         MVC   ZSAZTRAC(4,GPR13P),@CC04296                         0547
*     ZSAZSVRR=GPR14P;                                             0548
         ST    GPR14P,ZSAZSVRR(,GPR13P)                            0548
*     GPR13P=ZSAZSVFC;                                             0549
         L     GPR13P,ZSAZSVFC(,GPR13P)                            0549
*     RFY                                                          0550
*      (GPR01P) UNRSTD;                                            0550
*     RFY                                                          0551
*      (GPR14P) UNRSTD;                                            0551
*   END;                                                           0552
*                                                                  0552
*/* END TPZSAVE                                                      */
*                                                                  0553
*   ZLBLNG2=ZLBLNG1;                /* SET ZLB LENGTH                */
         LH    @09,ZLBLNG1(,PTRZLB)                                0553
         N     @09,@CF04326                                        0553
         STH   @09,ZLBLNG2(,PTRZLB)                                0553
*   FDBDATAA=ADDR(ZLBTEXT1);        /* SET DATA ADDRESS              */
         LA    @09,ZLBTEXT1(,PTRZLB)                               0554
         ST    @09,FDBDATAA(,PTRZLB)                               0554
*   FDBDATCT=0;                     /* SET DATA COUNT TO ZERO        */
         SLR   @09,@09                                             0555
         STH   @09,FDBDATCT(,PTRZLB)                               0555
*   ZLBLSTST=ZCRBFPTR;              /* SET LAST BUFFER POINTER       */
         L     @09,RPHCRR(,PTRRPH)                                 0556
         L     @09,CRACRR(,@09)                                    0556
         L     @09,ZCRBFPTR(,@09)                                  0556
         ST    @09,ZLBLSTST(,PTRZLB)                               0556
*   IF FMCMODE=FMCRCD THEN                                         0557
         CLC   FMCMODE(8,PTRFMC),@CC03533                          0557
         BE    @RT00557                                            0557
*     ;                             /* IF RECORD, IGNORE CHECK     0558
*                                                            #3270S/R*/
*   ELSE                            /* ELSE CHECK DIALOG SEQ #3270S/R*/
*     IF LCCWSI='1'B                /* IF START DIALOG               */
*         &FMCZFA06='1'B THEN       /* & DEVICE IN DIALOG            */
         L     @09,LCPNXLCW(,PTRLCP)                               0559
         TM    LCCWSI(@09),B'10000000'                             0559
         BNO   @RF00559                                            0559
************************************** START OF BLOCK 1 OF 5 ZP60008 **
         TM    LCCWSD(@09),B'01000000' ALSO END DIALOG?               *
         BO    @RF00565                YES, REALLY EWA OR WSF         *
************************************** END OF BLOCK 1 OF 5 ZP60008 ****
         TM    FMCZFA06(PTRFMC),B'00000100'                        0559
         BNO   @RF00559                                            0559
*       DO;                         /* SET CONFLICTING DIALOG ERR    */
*         LCPZRT00='1'B;            /* INDICATE LOGICAL ERROR        */
         OI    LCPZRT00(PTRLCP),B'10000000'                        0561
*         LCPMNRCD=FRMCDITS;        /* INDICATE CONFLICTING DIALOG   */
         MVI   LCPMNRCD(PTRLCP),X'A6'                              0562
*       END;                        /* DIALOG SEQUENCE ERROR         */
*     ELSE                                                         0564
*       ;                           /* NOT DIALOG SEQUENCE ERROR     */
@RF00559 DS    0H                                                  0565
*   IF LCCWSD='1'B THEN             /* IF END DIALOG SPECIFIED       */
@RT00557 L     @09,LCPNXLCW(,PTRLCP)                               0565
         TM    LCCWSD(@09),B'01000000'                             0565
         BNO   @RF00565                                            0565
************************************** START OF BLOCK 2 OF 5 ZP60008 **
         TM    LCCWSI(@09),B'10000000' ALSO START DIALOG?             *
         BO    @RF00565                YES, REALLY EWA OR WSF         *
************************************** END OF BLOCK 2 OF 5 ZP60008 ****
*     IF LCCWMODE=LMM               /* IF MESSAGE MODE               */
*          LCCWRWOP=LWR THEN        /*   WRITE CONVERSATIONAL        */
         TM    LCCWMODE(@09),B'00001000'                           0566
         BNO   @GL00002                                            0566
         TM    LCCWMODE(@09),B'00000100'                           0566
         BZ    @RT00566                                            0566
@GL00002 L     @09,LCPNXLCW(,PTRLCP)                               0566
         TM    LCCWRWOP(@09),B'00000001'                           0566
         BNO   @RF00566                                            0566
         TM    LCCWRWOP(@09),B'00111110'                           0566
         BNZ   @RF00566                                            0566
@RT00566 DS    0H                                                  0567
*       DO;                         /* SET CONFLICTING DIALOG ERR    */
*         LCPZRT00='1'B;            /* INDICATE LOGICAL ERROR        */
         OI    LCPZRT00(PTRLCP),B'10000000'                        0568
*         LCPMNRCD=FRMCDITS;        /* INDICATE CONFLICTING DIALOG   */
         MVI   LCPMNRCD(PTRLCP),X'A6'                              0569
*       END;                        /* DIALOG SEQUENCE ERROR         */
*     ELSE                          /* NOT MSG MODE OR WR CONV.      */
*       DO;                         /* TURN ON THE FLAGS             */
         B     @RC00566                                            0571
@RF00566 DS    0H                                                  0572
*         ZLBRT02='1'B;             /* INDICATE EOB                  */
*         ZLBRT05='1'B;             /* INDICATE DECREMENT COUNT      */
*         ZLBRT06='1'B;             /* INDICATE WRITE REQUEST        */
*         ZLBRT07='1'B;             /* INDICATE SCHEDULE             */
         OI    ZLBRT02(PTRZLB),B'00100111'                         0575
*         ZCRTPDIS='1'B;            /* INDICATE DO DISCONNECT        */
         L     @09,RPHCRR(,PTRRPH)                                 0576
         L     @09,CRACRR(,@09)                                    0576
         OI    ZCRTPDIS(@09),B'00100000'                           0576
*       END;                        /* END FLAG SETTING              */
*   ELSE                            /* END DIALOG NOT SPECIFIED      */
*     DO;                           /* TURN OFF DISCONNECT PENDING   */
         B     @RC00565                                            0578
@RF00565 DS    0H                                                  0579
*       FMCZFD06='0'B;              /* FMCB DISCONNECT NOT PENDING   */
         NI    FMCZFD06(PTRFMC),B'01111111'                        0579
*     END;                          /* DISCONNECT NOT PENDING        */
*   IF LCPZRT00='1'B                /* IF LOGICAL ERROR              */
*        LCPZRT01='1'B THEN                                        0581
@RC00565 TM    LCPZRT00(PTRLCP),B'11000000'                        0581
         BNZ   @RT00581                                            0581
*     ;                             /*   CONTROL BLOCK ERROR THEN NO
*                                      FURTHER WORK REQ'D            */
*   ELSE                            /* ERROR FLAGS NOT SET           */
*     DO;                           /* NO PRELIMINARY ERROR          */
*       IF FMCMODE=FMCRCD THEN                                     0584
         CLC   FMCMODE(8,PTRFMC),@CC03533                          0584
         BNE   @RF00584                                            0584
*         DO;                       /* IF RECORD MODE        #3270S/R*/
*           FMCOSSN=FMCOSSN+1;      /* INCREMENT SEQ NO      #3270S/R*/
         LA    @09,1                                               0586
         LH    @00,FMCOSSN(,PTRFMC)                                0586
         N     @00,@CF04326                                        0586
         ALR   @09,@00                                             0586
         STH   @09,FMCOSSN(,PTRFMC)                                0586
*           LCPSEQ=FMCOSSN;         /* SEQ NO IN LCPB        #3270S/R*/
         STH   @09,LCPSEQ(,PTRLCP)                                 0587
*         END;                      /* END RECORD MODE       #3270S/R*/
*       ELSE                                                       0589
*         ;                         /* IF NOT RECORD MODE    #3270S/R*/
@RF00584 DS    0H                                                  0590
*       IF LCCWRWOP=LEAU THEN       /* IF ERASE ALL UNPROTECTED      */
         L     @09,LCPNXLCW(,PTRLCP)                               0590
         TM    LCCWRWOP(@09),B'00010001'                           0590
         BNO   @RF00590                                            0590
         TM    LCCWRWOP(@09),B'00101110'                           0590
         BNZ   @RF00590                                            0590
*         DO;                       /* PROCESS ERASE ALL UNPROTECT   */
*           PTRCCW=ADDR(ZLBCCW1);   /* ADDRESS CCW                   */
         LA    PTRCCW,ZLBCCW1(,PTRZLB)                             0592
*           CCWCODE=EAU;            /* SET CCW OP CODE               */
         MVI   CCWCODE(PTRCCW),X'0F'                               0593
*           CCWCOUNT=1;             /* SET CCW COUNT                 */
         MVC   CCWCOUNT(2,PTRCCW),@CH00035                         0594
*           ZLBRT02='1'B;           /* INDICATE EOB                  */
*           ZLBRT05='1'B;           /* INDICATE DECREMENT COUNT      */
*           ZLBRT06='1'B;           /* INDICATE WRITE REQUEST        */
*           ZLBRT07='1'B;           /* INDICATE SCHEDULE             */
         OI    ZLBRT02(PTRZLB),B'00100111'                         0598
*           IF LCCWLSI='1'B THEN    /* IF LCCW SYNCH FLAG ON         */
         TM    LCCWLSI(@09),B'00000100'                            0599
         BNO   @RF00599                                            0599
*             CALL ATCZPSVT->ZPSZBFY0;/* CALL LCCW SYNCH ROUTINE     */
         L     @09,ATCZPSVT(,PTRATC)                               0600
         L     @15,ZPSZBFY0(,@09)                                  0600
         BALR  @14,@15                                             0600
*         END;                      /* END ERASE ALL UNPROTECTED     */
*       ELSE                        /* NOT ERASE ALL UNPROTECTED     */
*         IF LCCWMODE=LMB THEN      /* IF BLOCK MODE                 */
         B     @RC00590                                            0602
@RF00590 L     @09,LCPNXLCW(,PTRLCP)                               0602
         TM    LCCWMODE(@09),B'00000100'                           0602
         BNO   @RF00602                                            0602
         TM    LCCWMODE(@09),B'00001000'                           0602
         BNZ   @RF00602                                            0602
*           DO;                     /* SET INVALID OP CODE ERROR     */
*             LCPZRT01='1'B;        /* INDICATE CONTROL BLOCK ERR    */
         OI    LCPZRT01(PTRLCP),B'01000000'                        0604
*             LCPMNRCD=FRMINVL;     /* INDICATE INVALID OP CODE      */
         MVI   LCPMNRCD(PTRLCP),X'A3'                              0605
*           END;                    /* INVALID OPERATION CODE        */
*         ELSE                                                     0607
*           DO;                     /* NOT BLOCK MODE                */
         B     @RC00602                                            0607
@RF00602 DS    0H                                                  0608
*             IF LCCWCNT=0 THEN                                    0608
         L     @09,LCPNXLCW(,PTRLCP)                               0608
         LH    @09,LCCWCNT(,@09)                                   0608
         N     @09,@CF04326                                        0608
         LTR   @09,@09                                             0608
         BNZ   @RF00608                                            0608
*               DO;                 /* IF COUNT IN CURRENT LCCW=0  0609
*                                                            @XL03XFZ*/
*                 ZLBTEXT1(1)='00'X;/* SET ZERO WCC CHARACTER        */
         MVI   ZLBTEXT1(PTRZLB),X'00'                              0610
*                 FDBDATCT=1;       /* SET COUNT TO INCLUDE WCC      */
         MVC   FDBDATCT(2,PTRZLB),@CH00035                         0611
*               END;                /* END CCW COUNT 0       @XL03HFZ*/
*             ELSE                                                 0613
*               ;                   /* NOT CCW COUNT 0       @XL03HFZ*/
*                                                                  0613
@RF00608 DS    0H                                                  0614
*             /*******************************************************/
*             /*                                                     */
*             /* SEGMENT(WRCCW)REQUEST                               */
*             /*                                                     */
*             /*******************************************************/
*                                                                  0614
*             DO;                                                  0614
*               GOTO WRCCW;                                        0615
         B     WRCCW                                               0615
*WRCCW#:                                                           0616
*             END;                  /* MOVE DATA AND FORMAT CCW      */
WRCCW#   DS    0H                                                  0617
*           END;                    /* END NOT BLOCK MODE            */
*       IF LCCWRFI='1'B             /* IF FEEDBACK REQUIRED          */
*           &ZLBRT03='0'B THEN      /* & NOT WRITE CONVERSATIONAL    */
@RC00602 DS    0H                                                  0618
@RC00590 L     @09,LCPNXLCW(,PTRLCP)                               0618
         TM    LCCWRFI(@09),B'00000010'                            0618
         BNO   @RF00618                                            0618
         TM    ZLBRT03(PTRZLB),B'00010000'                         0618
         BNZ   @RF00618                                            0618
*         DO;                       /* SET FLAGS IN REQ TAG          */
*           ZLBRT02='1'B;           /* INDICATE EOB                  */
*           ZLBRT04='1'B;           /* INDICATE FEEDBACK REQUIRED    */
         OI    ZLBRT02(PTRZLB),B'00101000'                         0621
*         END;                      /* FLAGS SET IN REQUEST TAG      */
*       ELSE                                                       0623
*         ;                         /* FEEDBACK NOT REQUIRED         */
@RF00618 DS    0H                                                  0624
*       ZLBECI='1'B;                /* INDICATE ECI                  */
*       ZLBEBI='0'B;                /* CLEAR EBI FLAG                */
*       ZLBEMI='0'B;                /* CLEAR EMI FLAG                */
*       ZLBETI='0'B;                /* CLEAR ETI FLAG                */
*       ZLBFBMI='0'B;               /* CLEAR FBMI FLAG               */
         OI    ZLBECI(PTRZLB),B'01000000'                          0628
         NI    ZLBEBI(PTRZLB),B'11000011'                          0628
*     END;                          /* END NO PRELIMINARY ERROR      */
*/* TPZRETN                                                          */
*                                                                  0630
*   DO;                                                            0630
@RT00581 DS    0H                                                  0631
*     RFY                                                          0631
*      (GPR08P,                                                    0631
*       GPR14P,                                                    0631
*       GPR15P) RSTD;                                              0631
*     GPR13P=ZSAZSVBC;                                             0632
         L     GPR13P,ZSAZSVBC(,GPR13P)                            0632
*     IF GPR13P=0 THEN                                             0633
         LTR   GPR13P,GPR13P                                       0633
         BZ    @RT00633                                            0633
*       ;                           /* TPDVTS ISTZFARR               */
*     ELSE                                                         0635
*       GPR14P=ZSAZSVRR;                                           0635
         L     GPR14P,ZSAZSVRR(,GPR13P)                            0635
*     GEN REFS(ZSAZSVVR)(LM    8,12,ZSAZSVVR+4(13));               0636
@RT00633 DS    0H                                                  0636
         LM    8,12,ZSAZSVVR+4(13)
*     GEN REFS(GPR14P) EXIT NOSEQFLOW(BR    GPR14P);               0637
         BR    GPR14P
*     RFY                                                          0638
*      (GPR08P,                                                    0638
*       GPR14P,                                                    0638
*       GPR15P) UNRSTD;                                            0638
*   END;                                                           0639
*                                                                  0639
*/* END TPZRETN                                                      */
*                                                                  0640
*   GEN NOSETS NOREFS NODEFS(EJECT);                               0640
         EJECT
*                                                                  0641
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT-                                                         */
*/*      WRCCW                                                       */
*/*                                                                  */
*/* FUNCTION-                                                        */
*/*      MOVE DATA AND FORMAT CCW                                    */
*/*                                                                  */
*/********************************************************************/
*                                                                  0641
*   /*****************************************************************/
*   /*                                                               */
*   /* SEGMENT(WRCCW)START                                           */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0641
*   DO;                                                            0641
*     GOTO WRCCW@;                                                 0642
         B     WRCCW@                                              0642
*WRCCW:                                                            0643
*   END;                                                           0643
WRCCW    DS    0H                                                  0644
*   RFY                                                            0644
*     PTRG15 RSTD;                  /* FOR RETURN CODE CHECK         */
*   CALL ATCZPSVT->ZPSZDFC0;        /* CALL MOVE DATA ROUTINE        */
         L     @09,ATCZPSVT(,PTRATC)                               0645
         L     @15,ZPSZDFC0(,@09)                                  0645
         BALR  @14,@15                                             0645
*   IF PTRG15=RC00 THEN             /* IF GOOD RETURN CODE           */
         LTR   PTRG15,PTRG15                                       0646
         BNZ   @RF00646                                            0646
*     CALL FORMCCW;                 /* FORMAT THE CCWS               */
         BAL   @14,FORMCCW                                         0647
*   ELSE                            /* DATA MOVE RC ^= 0             */
*                                                                  0648
*     /***************************************************************/
*     /*                                                             */
*     /* IF USER ERROR FOUND                                 @XL03HQV*/
*     /*                                                             */
*     /***************************************************************/
*                                                                  0648
*     IF PTRG15=12 THEN             /* USER ERROR -          @XL03HFZ*/
         B     @RC00646                                            0648
@RF00646 C     PTRG15,@CF01716                                     0648
         BNE   @RF00648                                            0648
*       IF FMCMODE=FMCRCD THEN                                     0649
         CLC   FMCMODE(8,PTRFMC),@CC03533                          0649
         BNE   @RF00649                                            0649
*         DO;                       /* IF RECORD MODE        @XL03HFZ*/
*           LCPZRT01='1'B;          /* FLAG CB ERROR         @XL03HFZ*/
         OI    LCPZRT01(PTRLCP),B'01000000'                        0651
*           LCPMNRCD=FRMINDTR;      /* SET FRMINDTR RETURN CODE IN 0652
*                                      LCPB                  @XL03HFZ*/
         MVI   LCPMNRCD(PTRLCP),X'17'                              0652
*         END;                      /* END RECORD MODE       @XL03HFZ*/
*       ELSE                                                       0654
*         DO;                       /* NOT RECORD MODE       @XL03HFZ*/
         B     @RC00649                                            0654
@RF00649 DS    0H                                                  0655
*           LCPZRT00='1'B;          /* LOGIC ERROR           @XL03HQV*/
         OI    LCPZRT00(PTRLCP),B'10000000'                        0655
*           LCPMNRCD=FRMINDAT;      /* INVALID DATA          @XL03HQV*/
         MVI   LCPMNRCD(PTRLCP),X'97'                              0656
*         END;                                                     0657
*     ELSE                                                         0658
*       DO;                         /* NOT USER ERROR -      @XL03HQV*/
         B     @RC00648                                            0658
@RF00648 DS    0H                                                  0659
*         LCPZRT00='1'B;            /* INDICATE LOGIC ERROR          */
         OI    LCPZRT00(PTRLCP),B'10000000'                        0659
*         LCPMNRCD=FRMVTAME;        /* INDICATE VTAM ERROR           */
         MVI   LCPMNRCD(PTRLCP),X'A0'                              0660
*       END;                                                       0661
*   PTRZLB=ZCRZLB;                  /* ADDRESS 1ST CHAINED BUFFER    */
@RC00648 DS    0H                                                  0662
@RC00646 L     @14,RPHCRR(,PTRRPH)                                 0662
         L     @14,CRACRR(,@14)                                    0662
         L     PTRZLB,ZCRZLB(,@14)                                 0662
*   ZLBLCCWA=LCPNXLCW;              /* SET LCCW POINTER IN ZLBUF     */
         L     @09,LCPNXLCW(,PTRLCP)                               0663
         ST    @09,ZLBLCCWA(,PTRZLB)                               0663
*   ZLBLSTST=ZCRBFPTR;              /* ADDRESS LAST BUFFER OF SET    */
*                                                                  0664
         L     @14,ZCRBFPTR(,@14)                                  0664
         ST    @14,ZLBLSTST(,PTRZLB)                               0664
*   /*****************************************************************/
*   /*                                                               */
*   /* SEGMENT(WRCCW)FINISH                                          */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0665
*   DO;                                                            0665
*     GOTO WRCCW#;                                                 0666
         B     WRCCW#                                              0666
*WRCCW@:                                                           0667
*   END;                            /* END OF SUBROUTINE             */
*                                                                  0668
*/********************************************************************/
*/*                                                                  */
*/* SUBROUTINE-                                                      */
*/*      FORMCCW                                                     */
*/*                                                                  */
*/* FUNCTION-                                                        */
*/*      FORMAT A WRITE CCW LIST FOR LOCAL BUFFERS.                  */
*/*                                                                  */
*/********************************************************************/
*                                                                  0668
*FORMCCW:                                                          0668
*   PROC OPTIONS(NOSAVE,NOSAVEAREA);                               0668
         B     @PB00002                                            0668
FORMCCW  DS    0H                                                  0669
*   RFY                                                            0669
*     PTRG14 RSTD;                  /* TO SAVE RETURN REG            */
*   ZCRSVWK1=PTRG14;                /* SAVE RETURN REGISTER          */
         L     @09,RPHCRR(,PTRRPH)                                 0670
         L     @09,CRACRR(,@09)                                    0670
         ST    PTRG14,ZCRSVWK1(,@09)                               0670
*   RFY                                                            0671
*     PTRG14 UNRSTD;                /* SAVED                         */
*   PTRCCW=ADDR(ZLBCCW1);           /* ADDRESS CCW                   */
         LA    PTRCCW,ZLBCCW1(,PTRZLB)                             0672
*   PTRG15=FDBDATAA;                /* GET DATA ADDRESS              */
         L     PTRG15,FDBDATAA(,PTRZLB)                            0673
*   GEN;                                                           0674
*                                      /* CONVERT TO REAL ADDRESS    */
         LRA   PTRG15,0(0,PTRG15)      CONVERT TO REAL ADDRESS
*   ZLBCPAD1=PTRG15;                /* SET REAL ADDRESS IN CCW       */
         STCM  PTRG15,7,ZLBCPAD1(PTRZLB)                           0675
*   IF LCCWSOP='1'B THEN            /* IF ERASE WRITE                */
         L     @14,LCPNXLCW(,PTRLCP)                               0676
************************************** START OF BLOCK 3 OF 5 ZP60008 **
         TM    LCCWSOP(@14),B'11000000'                               *
         BO    @ZP68LBL                PROCESS EWA OR WSF             *
************************************** END OF BLOCK 3 OF 5 ZP60008 ****
         TM    LCCWSOP(@14),B'00010000'                            0676
         BNO   @RF00676                                            0676
************************************** START OF BLOCK 4 OF 5 ZP60008 **
@ZP68LBL DS    0H                                                     *
************************************** END OF BLOCK 4 OF 5 ZP60008 ****
*     DO;                           /* PROCESS ERASE WRITE           */
*       LDNZARM=LDNZRM;             /* SET MAX DATA FOR ATTN READ    */
         LH    @14,LDNZRM(,PTRLDN)                                 0678
         N     @14,@CF04326                                        0678
         STH   @14,LDNZARM(,PTRLDN)                                0678
*       CCWCODE=EWRITE;             /* SET CCW OPCODE = ERASE WR     */
         MVI   CCWCODE(PTRCCW),X'05'                               0679
*       CCWCOUNT=FDBDATCT;          /* SET CCW DATA COUNT            */
         LH    @14,FDBDATCT(,PTRZLB)                               0680
         STH   @14,CCWCOUNT(,PTRCCW)                               0680
************************************** START OF BLOCK 5 OF 5 ZP60008 **
         L     @14,LCPNXLCW(,PTRLCP)                                  *
         TM    LCCWSOP(@14),B'11000000'                               *
         BNO   @RC00676                CORRECT CCW OPCODE WAS SET     *
         MVI   CCWCODE(PTRCCW),X'0D'   SET CCW OPCODE = ERASE WR ALT  *
         TM    LCCWSOP(@14),B'00010000'                               *
         BO    @RC00676                CORRECT CCW OPCODE WAS SET     *
         MVI   CCWCODE(PTRCCW),X'11'   SET CCW OPCODE = WR STRUC FLD  *
************************************** END OF BLOCK 5 OF 5 ZP60008 ****
*     END;                          /* END ERASE WRITE               */
*   ELSE                            /* NOT ERASE WRITE               */
*     DO;                           /* PROCESS NON ERASE WRITE       */
         B     @RC00676                                            0682
@RF00676 DS    0H                                                  0683
*       IF ZCRTIC='0'B THEN         /* IF 1ST CCW IN CHAN PGM        */
         L     @14,RPHCRR(,PTRRPH)                                 0683
         L     @14,CRACRR(,@14)                                    0683
         TM    ZCRTIC(@14),B'10000000'                             0683
         BNZ   @RF00683                                            0683
*         DO;                       /* FIRST CCW IN CHANNEL PGM      */
*           ZLBCPAD2=ZLBCPAD1;      /* MOVE ADDRESS TO 2ND CCW       */
         MVC   ZLBCPAD2(3,PTRZLB),ZLBCPAD1(PTRZLB)                 0685
*           ZLBCPTC2=FDBDATCT;      /* MOVE DATA COUNT TO 2ND CCW    */
         LH    @14,FDBDATCT(,PTRZLB)                               0686
         STH   @14,ZLBCPTC2(,PTRZLB)                               0686
*           ZLBCCW1=ZLBSEL;         /* SET 1ST CCW TO SELECT         */
         MVC   ZLBCCW1(8,PTRZLB),@CB04143                          0687
*           PTRCCW=PTRCCW+LENGTH(ISTCCW);/* ADDRESS NEXT CCW         */
         AL    PTRCCW,@CF00072                                     0688
*         END;                      /* END FIRST CCW IN CHAN PGM     */
*       ELSE                        /* NOT FIRST CCW IN CHAN PGM     */
*         CCWCOUNT=FDBDATCT;        /* SET UP DATA COUNT             */
         B     @RC00683                                            0690
@RF00683 LH    @14,FDBDATCT(,PTRZLB)                               0690
         STH   @14,CCWCOUNT(,PTRCCW)                               0690
*       CCWCODE=WRITE;              /* SET CCW OPERATION CODE        */
@RC00683 MVI   CCWCODE(PTRCCW),X'01'                               0691
*     END;                          /* END NON ERASE WRITE           */
*   IF LCCWMODE=LMS THEN            /* IF WRITE CONVERSATIONAL       */
@RC00676 L     @14,LCPNXLCW(,PTRLCP)                               0693
         TM    LCCWMODE(@14),B'00001100'                           0693
         BNZ   @RF00693                                            0693
*     DO;                           /* SET WRITE CONV. FLAGS         */
*       ZLBRT02='1'B;               /* INDICATE EOB                  */
*       ZLBRT03='1'B;               /* INDICATE WRITE/READ           */
*       ZLBRT05='1'B;               /* INDICATE DECREMENT SCHED      */
*       ZLBRT06='1'B;               /* INDICATE WRITE                */
*       ZLBRT07='1'B;               /* INDICATE SCHEDULE             */
         OI    ZLBRT02(PTRZLB),B'00110111'                         0699
*       IF ADDR(PTRDNC->NCBDEVCH)->DEVTCODE=DEV3284 ADDR(PTRDNC->  0700
*           NCBDEVCH)->DEVTCODE=DEV3286 THEN/* IF DEVICE 3284   3286 */
         LA    @14,36                                              0700
         ALR   @14,PTRDNC                                          0700
         CLI   DEVTCODE(@14),X'1A'                                 0700
         BE    @RT00700                                            0700
         CLI   DEVTCODE(@14),X'1B'                                 0700
         BNE   @RF00700                                            0700
@RT00700 DS    0H                                                  0701
*         DO;                       /* PROCESS 3270 PRINTER DEVICE   */
*           LCPZRT00='1'B;          /* FLAG LOGICAL ERROR            */
         OI    LCPZRT00(PTRLCP),B'10000000'                        0702
*           LCPMNRCD=FRMUSELE;      /* FLAG USER ERROR               */
         MVI   LCPMNRCD(PTRLCP),X'A3'                              0703
*         END;                      /* END 3270 PRINTER              */
*       ELSE                                                       0705
*         ;                         /* NOT 3270 PRINTER              */
@RF00700 DS    0H                                                  0706
*     END;                          /* END WRITE CONVERSATIONAL      */
*   ELSE                            /* NOT WRITE CONVERSATIONAL      */
*     IF ZLBWCCSP='1'B THEN         /* IF START PRINT INDICATED      */
         B     @RC00693                                            0707
@RF00693 TM    ZLBWCCSP(PTRZLB),B'00001000'                        0707
         BNO   @RF00707                                            0707
*       DO;                         /* SET START PRINT FLAGS         */
*         ZLBRT02='1'B;             /* INDICATE EOB                  */
*         ZLBRT05='1'B;             /* INDICATE DECREMENT SCHED      */
*         ZLBRT06='1'B;             /* INDICATE WRITE                */
*         ZLBRT07='1'B;             /* INDICATE SCHEDULE             */
         OI    ZLBRT02(PTRZLB),B'00100111'                         0712
*       END;                        /* END START PRINT FLAG SETS     */
*     ELSE                          /* START PRINT BIT OFF           */
*       ZLBRT06='1'B;               /* INDICATE START PRINT          */
         B     @RC00707                                            0714
@RF00707 OI    ZLBRT06(PTRZLB),B'00000010'                         0714
*   DO WHILE(FDBFDBA^=0);           /* CHAIN IN FDB'S TO I/O CHAIN   */
@RC00707 DS    0H                                                  0715
@RC00693 B     @DE00715                                            0715
@DL00715 DS    0H                                                  0716
*     CCWFLAGS=ZLBDC;               /* INDICATE CHAIN DATA           */
         MVI   CCWFLAGS(PTRCCW),X'80'                              0716
*     PTRCCW=PTRCCW+LENGTH(ISTCCW); /* POINT AT NEXT CCW             */
         AL    PTRCCW,@CF00072                                     0717
*     CCWCODE=TIC;                  /* SET COMMAND TO TIC            */
         MVI   CCWCODE(PTRCCW),X'08'                               0718
*     PTRG15=FDBFDBA+ZLBCPOFS;      /* ADDRESS NEXT CCW PACKAGE      */
         LA    PTRG15,4                                            0719
         AL    PTRG15,FDBFDBA(,PTRZLB)                             0719
*     GEN;                                                         0720
*                                      /* CONVERT TO REAL ADDRESS    */
         LRA   PTRG15,0(0,PTRG15)      CONVERT TO REAL ADDRESS
*     CCWADDR=PTRG15;               /* SET REAL ADDRESS IN CCW       */
         STCM  PTRG15,7,CCWADDR(PTRCCW)                            0721
*     PTRZLB=FDBFDBA;               /* ADDRESS NEXT BUFFER           */
         L     PTRZLB,FDBFDBA(,PTRZLB)                             0722
*     PTRCCW=ADDR(ZLBCCW1);         /* ADDRESS CCW IN BUFFER         */
         LA    PTRCCW,ZLBCCW1(,PTRZLB)                             0723
*     PTRG15=FDBDATAA;              /* ADDRESS OF BUFFER DATA        */
         L     PTRG15,FDBDATAA(,PTRZLB)                            0724
*     GEN;                                                         0725
*                                      /* CONVERT TO REAL ADDRESS    */
         LRA   PTRG15,0(0,PTRG15)      CONVERT TO REAL ADDRESS
*     CCWADDR=PTRG15;               /* SET REAL ADDRESS IN CCW       */
         STCM  PTRG15,7,CCWADDR(PTRCCW)                            0726
*     CCWCODE=WRITE;                /* SET CCW OPERATION CODE        */
         MVI   CCWCODE(PTRCCW),X'01'                               0727
*     CCWCOUNT=FDBDATCT;            /* SET DATA COUNT IN CCW         */
         LH    @14,FDBDATCT(,PTRZLB)                               0728
         STH   @14,CCWCOUNT(,PTRCCW)                               0728
*   END;                            /* FDB'S IN CHAIN ARE CHAINED    */
@DE00715 L     @14,FDBFDBA(,PTRZLB)                                0729
         LTR   @14,@14                                             0729
         BNZ   @DL00715                                            0729
*   CCWFLAGS=ZLBSLI;                /* SET SLI FLAG IN LASW CCW      */
         MVI   CCWFLAGS(PTRCCW),X'20'                              0730
*   ZCRBFPTR=PTRZLB;                /* SET BUFFER POINTER IN ZCR     */
         L     @14,RPHCRR(,PTRRPH)                                 0731
         L     @14,CRACRR(,@14)                                    0731
         ST    PTRZLB,ZCRBFPTR(,@14)                               0731
*   ZCRCCWA=PTRCCW;                 /* ADDRESS OF LAST CCW           */
         ST    PTRCCW,ZCRCCWA(,@14)                                0732
*BUG01:                                                            0733
*   ZCRTIC='1'B;                    /* SET ZCR TIC FLAG              */
BUG01    L     @14,RPHCRR(,PTRRPH)                                 0733
         L     @14,CRACRR(,@14)                                    0733
         OI    ZCRTIC(@14),B'10000000'                             0733
*   RFY                                                            0734
*     PTRG14 RSTD;                  /* TO RESTORE RETURN REG         */
*BUG02:                                                            0735
*   PTRG14=ZCRSVWK1;                /* RESTORE RETURN REG            */
BUG02    L     @09,RPHCRR(,PTRRPH)                                 0735
         L     @09,CRACRR(,@09)                                    0735
         L     PTRG14,ZCRSVWK1(,@09)                               0735
*   RFY                                                            0736
*     PTRG14 UNRSTD;                /* RESTORED                      */
*   END FORMCCW;                    /* END OF SUBROUTINE             */
@EL00002 DS    0H                                                  0737
@EF00002 DS    0H                                                  0737
@ER00002 BR    @14                                                 0737
@PB00002 DS    0H                                                  0738
*/* TPEPILOG                                                         */
*                                                                  0738
*   GEN(PRINT OFF);                                                0738
         PRINT OFF
*   GEN(PRINT ON);                                                 0739
*                                                                  0739
         PRINT ON
*/* END OF TPEPILOG                                                  */
*                                                                  0740
*   END ISTZBFBA                    /* END OF COMPILATION            */
*                                                                  0740
*/* THE FOLLOWING INCLUDE STATEMENTS WERE FOUND IN THIS PROGRAM.     */
*/*%INCLUDE SYSLIB  (IECDIOSB)                                       */
*/*%INCLUDE SYSLIB  (IHASRB  )                                       */
*/*%INCLUDE SYSLIB  (CVT     )                                       */
*                                                                  0740
*       ;                                                          0740
@EL00001 DS    0H                                                  0740
@EF00001 DS    0H                                                  0740
@ER00001 BR    @14                                                 0740
@DATA    DS    0H
@CH00035 DC    H'1'
@DATD    DSECT
         DS    0F
ISTZBFBA CSECT
         DS    0F
@CF00072 DC    F'8'
@CF01716 DC    F'12'
@CF04326 DC    XL4'0000FFFF'
@DATD    DSECT
         DS    0D
IOSBPTR  DS    A
SRBPTR   DS    A
CVTPTR   DS    A
LCPBPTR  DS    A
LDNCBPTR DS    A
DNCBPTR  DS    A
FMCBPTR  DS    A
FSBPTR   DS    A
         ORG   *+1-(*-@DATD)/(*-@DATD) INSURE DSECT DATA
@ENDDATD EQU   *
ISTZBFBA CSECT
         DS    0F
@SIZDATD DC    AL1(0)
         DC    AL3(@ENDDATD-@DATD)
         DS    0D
@CC03533 DC    C'RECORD  '
@CC04296 DC    C'BCBA'
@CB04143 DC    X'0B00000060000001'
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
RPHPTR   EQU   @01
GPR01P   EQU   @01
GPR08P   EQU   @08
GPR12P   EQU   @12
GPR13P   EQU   @13
GPR14P   EQU   @14
GPR15P   EQU   @15
PTRRPH   EQU   @01
PTRATC   EQU   @02
PTRLDN   EQU   @03
PTRDNC   EQU   @04
PTRFMC   EQU   @05
PTRZLB   EQU   @06
PTRLCP   EQU   @07
PTRLSC   EQU   @07
PTRCCW   EQU   @08
PTRG12   EQU   @12
PTRG13   EQU   @13
PTRG14   EQU   @14
PTRG15   EQU   @15
GPR00F   EQU   @00
GPR00P   EQU   @00
GPR01F   EQU   @01
GPR02F   EQU   @02
GPR02P   EQU   @02
GPR03F   EQU   @03
GPR03P   EQU   @03
GPR04F   EQU   @04
GPR04P   EQU   @04
GPR05F   EQU   @05
GPR05P   EQU   @05
GPR06F   EQU   @06
GPR06P   EQU   @06
GPR07F   EQU   @07
GPR07P   EQU   @07
GPR08F   EQU   @08
GPR09F   EQU   @09
GPR09P   EQU   @09
GPR10F   EQU   @10
GPR10P   EQU   @10
GPR11F   EQU   @11
GPR11P   EQU   @11
GPR12F   EQU   @12
GPR13F   EQU   @13
GPR14F   EQU   @14
GPR15F   EQU   @15
PTRG00   EQU   @00
PTRG09   EQU   @09
PTRG10   EQU   @10
PTRG11   EQU   @11
ATCLCPTR EQU   1032
ISTZPSVT EQU   0
ZPSZDFC0 EQU   ISTZPSVT+4
ZPSZBFY0 EQU   ISTZPSVT+24
ISTZFSVT EQU   0
ZFSZEFBB EQU   ISTZFSVT
ZFSZEABB EQU   ZFSZEFBB
ZFSZEFAB EQU   ISTZFSVT+4
ZFSZEAAB EQU   ZFSZEFAB
ISTZLBVT EQU   0
ISTCCW   EQU   0
CCWVADDR EQU   ISTCCW
CCWCODE  EQU   CCWVADDR
CCWADDR  EQU   CCWVADDR+1
CCWFLAGS EQU   ISTCCW+4
CCWSPARE EQU   ISTCCW+5
CCWCOUNT EQU   ISTCCW+6
ISTRPH   EQU   0
RPHFLAGS EQU   ISTRPH+2
RPHAPTYP EQU   RPHFLAGS
RPHFSTLC EQU   RPHFLAGS
RPHFLGB  EQU   ISTRPH+3
RPHRPHA  EQU   ISTRPH+4
RPHTSKID EQU   ISTRPH+8
RPHRESMA EQU   ISTRPH+16
RPHWPFLG EQU   RPHRESMA
RPHWEA   EQU   ISTRPH+24
RPHCSPA  EQU   RPHWEA
RPHSRPRM EQU   ISTRPH+28
RPHCRR   EQU   ISTRPH+32
RPHPABQA EQU   ISTRPH+36
RPHWORK  EQU   ISTRPH+40
RPHSAVE1 EQU   RPHWORK
RPHSBITS EQU   RPHSAVE1+1
ISTZSAVE EQU   0
ZSAZSVBC EQU   ISTZSAVE
ZSAZSVMS EQU   ZSAZSVBC
ZSAZACTV EQU   ZSAZSVMS
ZSAZSVFC EQU   ISTZSAVE+4
ZSAZSVRR EQU   ISTZSAVE+8
ZSAZSVVR EQU   ISTZSAVE+12
ZSAZTRAC EQU   ISTZSAVE+36
ISTLCCW  EQU   0
LCCWOP   EQU   ISTLCCW
LCCWSI   EQU   LCCWOP
LCCWSD   EQU   LCCWOP
LCCWRWOP EQU   LCCWOP
LCCWSOP  EQU   LCCWRWOP
LCCWCTL  EQU   LCCWRWOP
LCCWMODE EQU   LCCWCTL
LCCWFLAG EQU   ISTLCCW+1
LCCWLSI  EQU   LCCWFLAG
LCCWRFI  EQU   LCCWFLAG
LCCWCNT  EQU   ISTLCCW+2
ISTLCPB  EQU   0
LCPREQTG EQU   ISTLCPB+2
LCPFLAGS EQU   LCPREQTG
LCPZRT00 EQU   LCPFLAGS
LCPZRT01 EQU   LCPFLAGS
LCPZF001 EQU   LCPFLAGS
LCPZRT02 EQU   LCPZF001
LCPZF002 EQU   LCPFLAGS
LCPZRT03 EQU   LCPZF002
LCPZF003 EQU   LCPFLAGS
LCPZRT04 EQU   LCPZF003
LCPZF004 EQU   LCPFLAGS
LCPZRT05 EQU   LCPZF004
LCPZRT06 EQU   LCPFLAGS
LCPZRT07 EQU   LCPFLAGS
LCPMNRCD EQU   LCPREQTG+1
LCPZRT08 EQU   LCPMNRCD
LCPZRT09 EQU   LCPMNRCD
LCPCHAIN EQU   ISTLCPB+4
LCPCHAI1 EQU   LCPCHAIN
LCPRUCNT EQU   ISTLCPB+8
LCPFLAG2 EQU   ISTLCPB+9
LCPTLBPI EQU   LCPFLAG2
LCPFID0  EQU   LCPFLAG2
LCPSEQ   EQU   ISTLCPB+10
LCPCID   EQU   ISTLCPB+16
LCPNXLCW EQU   ISTLCPB+20
LCPFDBA  EQU   ISTLCPB+24
LCPFLG1  EQU   ISTLCPB+37
LCPFLCCW EQU   ISTLCPB+40
LCPLCCW  EQU   ISTLCPB+48
LCPPABA  EQU   LCPLCCW+4
LCPRPHA  EQU   LCPPABA
ISTZCRR  EQU   0
ZCRCRR   EQU   ISTZCRR
ZCRFLGS1 EQU   ISTZCRR+12
ZCRFLGS2 EQU   ISTZCRR+13
ZCRSWFLD EQU   ISTZCRR+14
ZCRENDSH EQU   ZCRSWFLD
ZCRNDLCP EQU   ZCRSWFLD
ZCRRESET EQU   ZCRSWFLD
ZCRFLGS3 EQU   ISTZCRR+15
ZCRTIC   EQU   ZCRFLGS3
ZCRTPDIS EQU   ZCRFLGS3
ZCRGENSW EQU   ISTZCRR+16
ZCRPFLGS EQU   ISTZCRR+17
ZCRIBPR  EQU   ISTZCRR+18
ZCRSCPSW EQU   ISTZCRR+19
ZCRPARMS EQU   ISTZCRR+24
ZCRFBPTR EQU   ZCRPARMS
ZCRDFRDA EQU   ZCRPARMS+10
ZCRPCOD1 EQU   ZCRPARMS+12
ZCRPLSBF EQU   ZCRPCOD1
ZCRPCOD2 EQU   ZCRPARMS+13
ZCRCCWA  EQU   ISTZCRR+40
ZCRBFPTR EQU   ISTZCRR+44
ZCRZLB   EQU   ISTZCRR+48
ZCRSVRTN EQU   ISTZCRR+56
ZCRWKARA EQU   ISTZCRR+156
ZCRSVWK1 EQU   ZCRWKARA
ZCRCIDSV EQU   ISTZCRR+192
ZCRAUDIT EQU   ISTZCRR+440
ZCRENTER EQU   ZCRAUDIT+5
ZCREBCBA EQU   ZCRENTER+1
ZCRLCPB  EQU   ISTZCRR+480
ZCRRHRU  EQU   ZCRLCPB
ISTPFCRR EQU   0
PFCCRR   EQU   ISTPFCRR
PFCCRRID EQU   PFCCRR
PFCCRRLN EQU   PFCCRR+2
PFCHDR   EQU   ISTPFCRR+4
PFCCRRO  EQU   PFCHDR
PFCFRRO  EQU   PFCHDR+4
ISTAPCRR EQU   0
APCATFLG EQU   ISTAPCRR+16
APCRFLG  EQU   ISTAPCRR+18
APCSAVE  EQU   ISTAPCRR+44
APCCOMF  EQU   ISTAPCRR+116
APCCOMF1 EQU   APCCOMF
APC31FLG EQU   ISTAPCRR+132
APC52FLG EQU   ISTAPCRR+133
APC53FLG EQU   ISTAPCRR+134
APC54FLG EQU   ISTAPCRR+135
APC57FLG EQU   ISTAPCRR+136
ISTDCCRR EQU   0
CRRSPACE EQU   ISTDCCRR+12
CRRSAREA EQU   0
CRRMAS   EQU   CRRSAREA+148
CRRMAS1  EQU   CRRMAS
CRRMAS2  EQU   CRRMAS+1
CRRMAS3  EQU   CRRMAS+2
CRRMAS4  EQU   CRRMAS+3
CRRRMAS1 EQU   CRRMAS+4
CRRRMAS2 EQU   CRRMAS+5
CRRRMAS3 EQU   CRRMAS+6
CRRSMAS  EQU   CRRRMAS3
CRRATS   EQU   CRRSAREA+156
CRRATS1  EQU   CRRATS
CRRATS2  EQU   CRRATS+1
CRRATS3  EQU   CRRATS+2
CRRATS4  EQU   CRRATS+3
CRRRATS1 EQU   CRRATS+4
CRRRATS2 EQU   CRRATS+5
CRRRATS3 EQU   CRRATS+6
CRRSATS  EQU   CRRRATS3
CRRFLGS1 EQU   CRRSAREA+164
CRRFLGS2 EQU   CRRSAREA+165
CRRFLGS3 EQU   CRRSAREA+166
CRRFLGS4 EQU   CRRSAREA+167
CRRFLGS  EQU   CRRSAREA+168
CRRRFLG1 EQU   CRRSAREA+169
CRRRTYPE EQU   CRRRFLG1
CRRFDBK1 EQU   CRRSAREA+170
CRRFDB2  EQU   CRRFDBK1+1
CRRFDB3  EQU   CRRFDBK1+2
CRRFDBK2 EQU   CRRSAREA+176
CRRDSB   EQU   CRRFDBK2
CRRESR   EQU   CRRFDBK2+2
CRRTRBUF EQU   CRRSAREA+204
CRRBFLGS EQU   CRRSAREA+280
ISTASCRR EQU   0
ASCFLAGS EQU   ISTASCRR+16
ASCFLG1  EQU   ASCFLAGS
ASCFLG2  EQU   ASCFLAGS+2
ASCFLG3  EQU   ASCFLAGS+4
ASCUSRGS EQU   ISTASCRR+28
ISTRIA   EQU   0
RIAABRGS EQU   ISTRIA+16
RIAFLGS  EQU   ISTRIA+80
RIAFLG1  EQU   RIAFLGS
RIAFLG2  EQU   RIAFLGS+1
ISTCFCRR EQU   0
CFCAUDIT EQU   ISTCFCRR
CFCNTRD  EQU   CFCAUDIT
CFCCRT   EQU   CFCAUDIT+1
ISTCRA   EQU   0
CRABASIC EQU   ISTCRA
CRAHDR   EQU   CRABASIC
CRACRR   EQU   CRAHDR
CRALKACT EQU   CRABASIC+8
CRASVC   EQU   ISTCRA+848
CRABUFA  EQU   CRASVC+68
ISTPAB   EQU   0
PABWQCHN EQU   ISTPAB
PABWEQA  EQU   PABWQCHN
PABCHAIN EQU   PABWQCHN+4
PABRPHFG EQU   ISTPAB+12
PABFLAGS EQU   PABRPHFG
PABERLCK EQU   PABFLAGS
PABERRLK EQU   PABERLCK
ISTDYPAB EQU   0
ISTLOK   EQU   0
LOKCHN   EQU   ISTLOK+1
LOKCHNG  EQU   LOKCHN
@NM00037 EQU   LOKCHNG+2
ISTATCVT EQU   0
ATCCOM   EQU   ISTATCVT
ATCSTAT  EQU   ATCCOM
ATCSTAT1 EQU   ATCSTAT
ATCSTFLG EQU   ATCSTAT+1
ATCPOFLG EQU   ATCSTAT+2
ATCSTAT4 EQU   ATCSTAT+3
ATCIOTRC EQU   ATCCOM+140
ATCTHTRC EQU   ATCIOTRC
ATCCSMR  EQU   ISTATCVT+296
ATCSHRFG EQU   ISTATCVT+316
ATCHPGM  EQU   ISTATCVT+384
ATCVLCNT EQU   ISTATCVT+772
ATCSOPD  EQU   ISTATCVT+792
ATCSIPD  EQU   ISTATCVT+800
ATCSECST EQU   ISTATCVT+812
ATCAOSM  EQU   ISTATCVT+912
ATCGSRBQ EQU   ATCAOSM+4
ATCGSRBG EQU   ATCGSRBQ
ATCZPSVT EQU   ATCAOSM+72
ATCICAPP EQU   ATCAOSM+100
ATCLDAPP EQU   ATCAOSM+120
ATCDAPVT EQU   ATCAOSM+140
ATCVTFLG EQU   ATCAOSM+392
ATCZFLAG EQU   ATCAOSM+588
ISTDEVCH EQU   0
DEVSHCH  EQU   ISTDEVCH
DEVCHAR  EQU   DEVSHCH
DEVTCODE EQU   ISTDEVCH+1
DEVFLAGS EQU   ISTDEVCH+3
DEVFCCTL EQU   DEVFLAGS
DEVCHAR3 EQU   DEVFLAGS
ISTNCB   EQU   0
NCBFLAGS EQU   ISTNCB+24
NCBFRAS  EQU   NCBFLAGS+1
NCBFTRIO EQU   NCBFRAS
NCBFLAG1 EQU   NCBFLAGS+2
NCBDEVCH EQU   ISTNCB+36
ISTLDNCB EQU   0
LDNCFLAG EQU   ISTLDNCB+54
LDNZRM   EQU   ISTLDNCB+102
LDNZARM  EQU   ISTLDNCB+104
LDNFLAGS EQU   ISTLDNCB+107
LDNFLAG2 EQU   ISTLDNCB+110
LDNZIOLK EQU   ISTLDNCB+184
LDNZERPF EQU   ISTLDNCB+185
LDNZCPAN EQU   LDNZERPF
LDNZEPIN EQU   LDNZERPF
LDNZLPC  EQU   ISTLDNCB+186
LDNDCDEB EQU   ISTLDNCB+304
LDNZSAV3 EQU   ISTLDNCB+376
LDNZSAV4 EQU   LDNZSAV3+8
LDNZH003 EQU   ISTLDNCB+577
ISTDNCB  EQU   0
DNCOS    EQU   ISTDNCB+56
DNCZFLGS EQU   DNCOS
DNCBSCED EQU   DNCOS+2
ISTPROCD EQU   0
PROPROC  EQU   ISTPROCD
PROPROC1 EQU   PROPROC
PROPROC2 EQU   PROPROC+1
PROPROC3 EQU   PROPROC+2
PROPROC4 EQU   PROPROC+3
PROPROCS EQU   0
PROSETMS EQU   PROPROCS+1
PROSETM2 EQU   PROSETMS
PROSETM3 EQU   PROSETMS+1
PROSETM4 EQU   PROSETMS+2
ISTFMCB  EQU   0
FMCSTAT  EQU   ISTFMCB+28
FMCSTAT1 EQU   ISTFMCB+29
FMCCMDFL EQU   ISTFMCB+31
FMCCMPS1 EQU   ISTFMCB+36
FMCMODE  EQU   ISTFMCB+52
FMCSIPA  EQU   ISTFMCB+148
FMCNXOSQ EQU   ISTFMCB+152
FMCNXCFM EQU   FMCNXOSQ
FMCOSSN  EQU   FMCNXCFM
FMCNXOSC EQU   FMCNXOSQ+2
FMCSSTAT EQU   ISTFMCB+159
FMCPACE  EQU   ISTFMCB+160
FMCPACEN EQU   FMCPACE
FMCPACEM EQU   FMCPACE+1
FMCPACPC EQU   FMCPACE+2
FMCCVAL  EQU   FMCPACE+3
FMCOBCE  EQU   FMCCVAL
FMCCLEXT EQU   ISTFMCB+164
FMCRPLA  EQU   FMCCLEXT
FMCFBA   EQU   FMCCLEXT+4
FMCCMPS2 EQU   FMCCLEXT+16
FMCFLGA  EQU   FMCCMPS2
FMCFLGCL EQU   FMCCMPS2+1
FMCPSCMD EQU   FMCCMPS2+2
FMCPSTAT EQU   FMCCMPS2+3
FMCSTAT2 EQU   ISTFMCB+186
FMCMODEB EQU   ISTFMCB+189
FMCCLSEQ EQU   ISTFMCB+190
FMCRRESP EQU   ISTFMCB+195
FMCSSEGQ EQU   ISTFMCB+200
FMCFBAS  EQU   ISTFMCB+216
FMCZV002 EQU   FMCFBAS
FMCSNCTL EQU   ISTFMCB+224
FMCSCFLG EQU   FMCSNCTL
FMCVS1   EQU   ISTFMCB+232
FMCZFLAG EQU   FMCVS1
FMCZF006 EQU   FMCZFLAG
FMCZFA06 EQU   FMCZF006
FMCZFD06 EQU   FMCZF006+1
FMCLBXM  EQU   FMCVS1+12
FMCSHEAQ EQU   FMCLBXM
FMCLCPBQ EQU   FMCVS1+16
FMCLCPAQ EQU   FMCVS1+20
FMCSEQ1  EQU   FMCVS1+34
FMCZFLG2 EQU   FMCVS1+35
FMCCVFLG EQU   FMCVS1+44
FMCFLAG  EQU   0
FMCFLGB  EQU   FMCFLAG
ISTFDB   EQU   0
FDBFDBA  EQU   ISTFDB
FDBDATAA EQU   ISTFDB+4
FDBDATCT EQU   ISTFDB+8
ISTFSB   EQU   0
FSBFSB   EQU   ISTFSB
FSBMNRCD EQU   FSBFSB+2
FSBSFLGS EQU   FSBFSB+3
FSBFSBA  EQU   FSBFSB+4
FSBBTH   EQU   FSBFSB+8
FSBCID   EQU   FSBBTH
FSBRQTAG EQU   FSBBTH+4
FSBSEQID EQU   FSBRQTAG
FSBFLAGS EQU   FSBSEQID
FSBSEQ   EQU   FSBSEQID+1
FSBCSWST EQU   FSBBTH+6
FSBTHFLD EQU   FSBCSWST
FSBTH1   EQU   FSBTHFLD
FSBSYSRS EQU   FSBTH1
FSBTH2   EQU   FSBTHFLD+1
FSBEXTRS EQU   FSBTH2
FSBXFSTS EQU   FSBEXTRS
FSBXCODE EQU   FSBEXTRS
FSBNETRS EQU   FSBFSB+16
FSBSRCAT EQU   FSBNETRS
FSBERC   EQU   FSBNETRS+1
FSBFBMI  EQU   FSBERC
FSBDMGRS EQU   FSBFSB+18
FSBSS1   EQU   FSBDMGRS
FSBSS2   EQU   FSBDMGRS+1
FSBLCPB  EQU   FSBFSB+20
FSBMSGID EQU   FSBLCPB
FSBLCCWA EQU   FSBFSB+24
FSBRLSTS EQU   FSBLCCWA
ISTZLBUF EQU   0
ZLBFSM   EQU   ISTZLBUF+4
ZLBLNG1  EQU   ZLBFSM
ISTLSCB  EQU   0
LSC1WORD EQU   ISTLSCB
LSCREQTG EQU   LSC1WORD+2
LSCZRT02 EQU   LSCREQTG
LSCZRT03 EQU   LSCREQTG
LSCZRT04 EQU   LSCREQTG
LSCZRT05 EQU   LSCREQTG
LSCZRT06 EQU   LSCREQTG
LSCZRT07 EQU   LSCREQTG
LSCZMNRC EQU   LSCREQTG+1
LSCZRT08 EQU   LSCZMNRC
LSCZRT09 EQU   LSCZMNRC
LSCCHAIN EQU   ISTLSCB+4
LSCCHAI1 EQU   LSCCHAIN
LSCRUCNT EQU   ISTLSCB+8
LSCFLGS  EQU   ISTLSCB+9
LSCSEQ   EQU   ISTLSCB+10
LSCFDBA  EQU   ISTLSCB+24
LCPZTIE  EQU   LCPCID
LCPZSNAM EQU   LCPZTIE+8
FSBCOMBN EQU   FSBNETRS
FSBRHFLD EQU   FSBCOMBN+1
FSBRH3   EQU   FSBRHFLD+2
FSBLDFLG EQU   FSBRH3
@NM00057 EQU   FSBRQTAG
ZLBBTRAN EQU   ISTZLBUF
ZLBFDBZ  EQU   ZLBBTRAN
ZLBLNG2  EQU   ZLBBTRAN+14
ZLBAIO   EQU   ZLBBTRAN+16
ZLBDTIO  EQU   ZLBFSM
ZLBCCW1  EQU   ZLBDTIO
ZLBCPAD1 EQU   ZLBCCW1+1
ZLBCCW2  EQU   ZLBDTIO+8
ZLBCPAD2 EQU   ZLBCCW2+1
ZLBCPTC2 EQU   ZLBCCW2+6
ZLBCCW3  EQU   ZLBDTIO+16
ZLBSNS   EQU   ZLBDTIO+28
ZLBERC   EQU   ZLBDTIO+29
ZLBECI   EQU   ZLBERC
ZLBEBI   EQU   ZLBERC
ZLBEMI   EQU   ZLBERC
ZLBETI   EQU   ZLBERC
ZLBFBMI  EQU   ZLBERC
ZLBLCCWA EQU   ZLBDTIO+36
ZLBSAF   EQU   ZLBDTIO+48
ZLBSTAT  EQU   ZLBSAF
ZLBDEVST EQU   ZLBSTAT
ZLBRQTG  EQU   ZLBDTIO+50
ZLBRQFLG EQU   ZLBRQTG
ZLBRT02  EQU   ZLBRQFLG
ZLBRT03  EQU   ZLBRQFLG
ZLBRT04  EQU   ZLBRQFLG
ZLBRT05  EQU   ZLBRQFLG
ZLBRT06  EQU   ZLBRQFLG
ZLBRT07  EQU   ZLBRQFLG
ZLBLSTST EQU   ZLBDTIO+52
ZLBFLAGS EQU   ZLBDTIO+56
ZLBWCCSP EQU   ZLBFLAGS
ZLBTEXTA EQU   ZLBCCW3
ZLBTEXTB EQU   ZLBFLAGS
ZLBTEXT1 EQU   ZLBTEXTB
ZLBTEXTC EQU   ZLBFLAGS
ZLBPPL   EQU   ZLBTEXTC
ZLBFAIO  EQU   ZLBAIO
ZLBFSB   EQU   ZLBFAIO
         AGO   .@UNREFD                START UNREFERENCED COMPONENTS
ZLBFDBS  EQU   ZLBFAIO+32
ZLBPPCSC EQU   ZLBPPL+14
ZLBPPRS1 EQU   ZLBPPL+13
ZLBPPRSV EQU   ZLBPPL+12
ZLBPPCLG EQU   ZLBPPL+12
ZLBPPCC  EQU   ZLBPPL+12
ZLBPPINT EQU   ZLBPPL+12
ZLBPPHIO EQU   ZLBPPL+12
ZLBPP    EQU   ZLBPPL+4
ZLBPPLDB EQU   ZLBPPL+1
ZLBPPLF  EQU   ZLBPPL
ZLBTEXT  EQU   ZLBTEXTA+8
ZLBRSV0A EQU   ZLBTEXTA
ZLBTCP   EQU   ZLBFLAGS+8
ZLBTCSW  EQU   ZLBFLAGS+1
ZLBRSV09 EQU   ZLBFLAGS
ZLBRSV07 EQU   ZLBFLAGS
ZLBRQSEQ EQU   ZLBRQTG+1
ZLBRT0B  EQU   ZLBRQFLG+1
ZLBRT0A  EQU   ZLBRQFLG+1
ZLBRT09  EQU   ZLBRQFLG+1
ZLBRT08  EQU   ZLBRQFLG+1
ZLBRT01  EQU   ZLBRQFLG
ZLBRT00  EQU   ZLBRQFLG
ZLBCHNST EQU   ZLBSTAT+1
ZLBUCST  EQU   ZLBDEVST
ZLBDEST  EQU   ZLBDEVST
ZLBRSV10 EQU   ZLBDEVST
ZLBLSTBF EQU   ZLBDTIO+44
ZLBRSV11 EQU   ZLBDTIO+40
ZLBLCPBA EQU   ZLBDTIO+32
ZLBCOMP  EQU   ZLBDTIO+31
ZLBRSV08 EQU   ZLBDTIO+30
ZLBRSV05 EQU   ZLBERC
ZLBELI   EQU   ZLBERC
ZLBSOC   EQU   ZLBSNS
ZLBSCC   EQU   ZLBSNS
ZLBSUS   EQU   ZLBSNS
ZLBSDC   EQU   ZLBSNS
ZLBSEQCK EQU   ZLBSNS
ZLBSBOL  EQU   ZLBSNS
ZLBSIR   EQU   ZLBSNS
ZLBSCR   EQU   ZLBSNS
ZLBRSV12 EQU   ZLBDTIO+24
ZLBCPTC3 EQU   ZLBCCW3+6
ZLBRSV04 EQU   ZLBCCW3+5
ZLBCPFL3 EQU   ZLBCCW3+4
ZLBCPAD3 EQU   ZLBCCW3+1
ZLBCPCF3 EQU   ZLBCCW3
ZLBRSV03 EQU   ZLBCCW2+5
ZLBCPFL2 EQU   ZLBCCW2+4
ZLBCPCF2 EQU   ZLBCCW2
ZLBCPTC1 EQU   ZLBCCW1+6
ZLBCMOD  EQU   ZLBCCW1+5
ZLBCPFL1 EQU   ZLBCCW1+4
ZLBCPCF1 EQU   ZLBCCW1
ZLBRSV01 EQU   ZLBBTRAN+12
FSBRT0B  EQU   @NM00057+1
FSBRT0A  EQU   @NM00057+1
FSBRT09  EQU   @NM00057+1
FSBRT08  EQU   @NM00057+1
FSBRT07  EQU   @NM00057
FSBRT06  EQU   @NM00057
FSBRT05  EQU   @NM00057
FSBRT04  EQU   @NM00057
FSBRT03  EQU   @NM00057
FSBRT02  EQU   @NM00057
FSBRT01  EQU   @NM00057
FSBRT00  EQU   @NM00057
FSBLDCUF EQU   FSBLDFLG
FSBLDSIF EQU   FSBLDFLG
FSBLDSOF EQU   FSBLDFLG
FSBLDEBF EQU   FSBLDFLG
FSBLDBBF EQU   FSBLDFLG
FSBLDFME EQU   FSBLDFLG
FSBLDSCF EQU   FSBLDFLG
FSBLDRMF EQU   FSBLDFLG
FSBRH2   EQU   FSBRHFLD+1
FSBRH1   EQU   FSBRHFLD
FSBALIGN EQU   FSBCOMBN
LCPZTRV4 EQU   LCPZSNAM+4
LCPZTRV1 EQU   LCPZSNAM
LCPZSNA  EQU   LCPZTIE+4
LCPZFMCA EQU   LCPZTIE
LSCTLBAD EQU   ISTLSCB+28
LSCFDB2  EQU   LSCFDBA+1
LSCFDB1  EQU   LSCFDBA
LSCLCCWA EQU   ISTLSCB+20
LSCLCPBA EQU   ISTLSCB+16
LSCLCCW1 EQU   ISTLSCB+12
LSCRRCNT EQU   LSCSEQ+1
LSCRRCCT EQU   LSCSEQ
LSCATOP  EQU   LSCFLGS
LSCFREE  EQU   LSCFLGS
LSCDSTAT EQU   LSCFLGS
LSCEXRES EQU   LSCFLGS
LSCZRIMM EQU   LSCFLGS
LSCRSV01 EQU   LSCFLGS
LSCSMODE EQU   LSCFLGS
LSCZMIRC EQU   LSCRUCNT
LSCCHAI2 EQU   LSCCHAIN+1
LSCCHNG  EQU   LSCCHAI1
LSCZRTSC EQU   LSCZMNRC
LSCZRT0B EQU   LSCZMNRC
LSCZRT0A EQU   LSCZMNRC
LSCZRTL9 EQU   LSCZRT09
LSCZRTL8 EQU   LSCZRT08
LSCZRTL7 EQU   LSCZRT07
LSCZRTL6 EQU   LSCZRT06
LSCZRTL5 EQU   LSCZRT05
LSCZRTL4 EQU   LSCZRT04
LSCZRTL3 EQU   LSCZRT03
LSCZRTL2 EQU   LSCZRT02
LSCZRT01 EQU   LSCREQTG
LSCZRT00 EQU   LSCREQTG
LSCLNGTH EQU   LSC1WORD+1
LSCTYPE  EQU   LSC1WORD
ZLBCHAIN EQU   ISTZLBUF
FSBFDB   EQU   ISTFSB+32
FSBRSV01 EQU   FSBFSB+30
FSBTHCNT EQU   FSBFSB+28
FSBRLRSV EQU   FSBRLSTS
FSBCFTX  EQU   FSBRLSTS
FSBICNCB EQU   FSBMSGID
FSBSSOC  EQU   FSBSS2
FSBSSCC  EQU   FSBSS2
FSBSSDC  EQU   FSBSS2
FSBSSEC  EQU   FSBSS2
FSBSSIR  EQU   FSBSS2
FSBSSCR  EQU   FSBSS2
@NM00056 EQU   FSBSS2
@NM00055 EQU   FSBSS1
FSBSSDE  EQU   FSBSS1
FSBSSUS  EQU   FSBSS1
@NM00054 EQU   FSBSS1
FSBSOHI  EQU   FSBERC
FSBLGI   EQU   FSBERC
FSBLMRI  EQU   FSBFBMI
FSBETI   EQU   FSBERC
FSBEMI   EQU   FSBERC
FSBEBI   EQU   FSBERC
FSBECI   EQU   FSBERC
FSBELI   EQU   FSBERC
FSBMJRCD EQU   FSBSRCAT
FSBXLCI  EQU   FSBXCODE
FSBXFINS EQU   FSBXCODE
FSBXNORM EQU   FSBXFSTS
FSBSCODE EQU   FSBSYSRS
FSBSPHSE EQU   FSBSYSRS
FSBSERR  EQU   FSBSYSRS
FSBCMD   EQU   FSBSEQ
FSBDTRCE EQU   FSBFLAGS
FSBASBTU EQU   FSBFLAGS
FSBINPET EQU   FSBFLAGS
FSBNONCB EQU   FSBFLAGS
FSBLBIND EQU   FSBFLAGS
FSBFFIND EQU   FSBFLAGS
FSBACIND EQU   FSBFLAGS
FSBRFIND EQU   FSBFLAGS
FSBDAF   EQU   FSBCID+2
FSBSAF   EQU   FSBCID
FSBGATE  EQU   FSBFSBA
FSBRSV02 EQU   FSBSFLGS
FSBPGBUF EQU   FSBSFLGS
FSBMNSDA EQU   FSBMNRCD
FSBMNOA  EQU   FSBMNRCD
FSBMNDIF EQU   FSBMNRCD
FSBMNEOP EQU   FSBMNRCD
FSBMNDNU EQU   FSBMNRCD
FSBMNATN EQU   FSBMNRCD
FSBMNRVI EQU   FSBMNRCD
FSBMNELI EQU   FSBMNRCD
FSBLNGTH EQU   FSBFSB+1
FSBTYPE  EQU   FSBFSB
FDBOFSET EQU   ISTFDB+10
FMCFLRSV EQU   FMCFLGB
FMCFLGA8 EQU   FMCFLGB
FMCCLRPH EQU   FMCVS1+45
FMCRSV0X EQU   FMCCVFLG
FMCVYRPH EQU   FMCCVFLG
FMCZV004 EQU   FMCVS1+40
FMCLOCK2 EQU   FMCVS1+36
@NM00053 EQU   FMCZFLG2
FMCZF027 EQU   FMCZFLG2
FMCZF026 EQU   FMCZFLG2
FMCSEQ1B EQU   FMCSEQ1
FMCSEQ1T EQU   FMCSEQ1
FMCZX006 EQU   FMCVS1+33
FMCZX005 EQU   FMCVS1+32
FMCZX004 EQU   FMCVS1+31
FMCZX003 EQU   FMCVS1+30
FMCZX002 EQU   FMCVS1+29
FMCZX001 EQU   FMCVS1+28
FMCZV001 EQU   FMCVS1+24
FMCAWATQ EQU   FMCLCPAQ+1
@NM00052 EQU   FMCLCPAQ
FMCLCPB2 EQU   FMCLCPBQ+1
FMCLCPB1 EQU   FMCLCPBQ
FMCASCDQ EQU   FMCSHEAQ+1
@NM00051 EQU   FMCSHEAQ
FMCSHEDQ EQU   FMCVS1+8
FMCZFCSQ EQU   FMCVS1+6
FMCZH001 EQU   FMCVS1+4
FMCZF025 EQU   FMCZFLAG+3
FMCZF024 EQU   FMCZFLAG+3
FMCZF023 EQU   FMCZFLAG+3
FMCZF022 EQU   FMCZFLAG+3
FMCZF021 EQU   FMCZFLAG+3
FMCZF020 EQU   FMCZFLAG+3
FMCZF019 EQU   FMCZFLAG+3
FMCZF018 EQU   FMCZFLAG+3
FMCZF017 EQU   FMCZFLAG+2
FMCZF016 EQU   FMCZFLAG+2
FMCZF015 EQU   FMCZFLAG+2
FMCZF014 EQU   FMCZFLAG+2
FMCZF013 EQU   FMCZFLAG+2
FMCZF012 EQU   FMCZFLAG+2
FMCZF011 EQU   FMCZFLAG+2
FMCZF010 EQU   FMCZFLAG+2
FMCZF00F EQU   FMCZFLAG+1
FMCZF00E EQU   FMCZFLAG+1
FMCZF00D EQU   FMCZFLAG+1
FMCZF00C EQU   FMCZFLAG+1
FMCZF00B EQU   FMCZFLAG+1
FMCRSV10 EQU   FMCZFLAG+1
FMCZF009 EQU   FMCZFLAG+1
FMCZFC06 EQU   FMCZF006
FMCZFB06 EQU   FMCZF006
FMCZF005 EQU   FMCZFLAG
FMCZF004 EQU   FMCZFLAG
FMCZF003 EQU   FMCZFLAG
FMCZF002 EQU   FMCZFLAG
FMCZF001 EQU   FMCZFLAG
FMCFRESA EQU   ISTFMCB+228
FMCSCSQN EQU   FMCSNCTL+2
FMCSCCTL EQU   FMCSNCTL+1
FMCSTCTL EQU   FMCSCFLG
FMCSCCLO EQU   FMCSCFLG
FMCAPRO  EQU   FMCSCFLG
FMCLURO  EQU   FMCSCFLG
FMCOBSAC EQU   FMCSCFLG
FMCIBSAC EQU   FMCSCFLG
FMCFBRE  EQU   ISTFMCB+220
FMCFBXM  EQU   FMCZV002
FMCASEGQ EQU   ISTFMCB+212
FMCRDTPT EQU   ISTFMCB+208
FMCLOCK  EQU   ISTFMCB+204
FMCSEGD  EQU   FMCSSEGQ
FMCRSPLM EQU   ISTFMCB+198
FMCPSTRS EQU   ISTFMCB+196
@NM00050 EQU   FMCRRESP
FMCTSLHA EQU   FMCRRESP
FMCFRRN  EQU   FMCRRESP
FMCFME   EQU   FMCRRESP
FMCFEX   EQU   FMCRRESP
FMCFPE   EQU   FMCRRESP
FMCCTLB  EQU   ISTFMCB+194
FMCBSQNO EQU   FMCCLSEQ+2
FMCTSQNO EQU   FMCCLSEQ
FMCOBPRG EQU   FMCMODEB
FMCIBPRG EQU   FMCMODEB
FMCRESOT EQU   FMCMODEB
FMCRSTSR EQU   FMCMODEB
FMCLUQ   EQU   FMCMODEB
FMCAPQ   EQU   FMCMODEB
FMCCLEAR EQU   FMCMODEB
FMCHOLD  EQU   FMCMODEB
FMCIPSEQ EQU   ISTFMCB+188
FMCLUSTR EQU   ISTFMCB+187
FMCRSV87 EQU   FMCSTAT2
FMCSEQI  EQU   FMCSTAT2
FMCCPIP  EQU   FMCSTAT2
FMCALIPR EQU   FMCSTAT2
FMCIAIO  EQU   FMCSTAT2
FMCZF00A EQU   FMCSTAT2
FMCVPIP  EQU   FMCSTAT2
FMCBHSET EQU   ISTFMCB+185
FMCPRMRC EQU   FMCCLEXT+20
FMCRSV89 EQU   FMCPSTAT
FMCPCLSD EQU   FMCPSTAT
FMCDDOCD EQU   FMCPSTAT
FMCIDVFD EQU   FMCPSTAT
FMCELKST EQU   FMCPSTAT
FMCRSV88 EQU   FMCPSCMD
FMCRDBLK EQU   FMCPSCMD
FMCDSEOC EQU   FMCPSCMD
FMCPURGE EQU   FMCPSCMD
FMCRESTI EQU   FMCPSCMD
FMCCNTCT EQU   FMCPSCMD
FMCINVT  EQU   FMCPSCMD
FMCFLGB7 EQU   FMCFLGCL
FMCFLGB6 EQU   FMCFLGCL
FMCFLGB5 EQU   FMCFLGCL
FMCFLGB4 EQU   FMCFLGCL
FMCFLGB3 EQU   FMCFLGCL
FMCFLGB2 EQU   FMCFLGCL
FMCFLGB1 EQU   FMCFLGCL
FMCFLGB0 EQU   FMCFLGCL
FMCFLGA7 EQU   FMCFLGA
FMCFLGA6 EQU   FMCFLGA
FMCFLGA5 EQU   FMCFLGA
FMCFLGA4 EQU   FMCFLGA
FMCFLGA3 EQU   FMCFLGA
FMCFLGA2 EQU   FMCFLGA
FMCFLGA1 EQU   FMCFLGA
FMCFLGA0 EQU   FMCFLGA
FMCDCLOK EQU   FMCCLEXT+12
FMCRAFM  EQU   FMCCLEXT+8
FMCFBPTR EQU   FMCFBA+1
FMCFBRSV EQU   FMCFBA
FMCRPLPT EQU   FMCRPLA+1
FMCRPLRS EQU   FMCRPLA
FMCPRCLS EQU   FMCCVAL
FMCIBCE  EQU   FMCCVAL
FMCONLY  EQU   FMCOBCE
FMCLAST  EQU   FMCOBCE
FMCMIDLE EQU   FMCOBCE
FMCFIRST EQU   FMCOBCE
FMCCPCNT EQU   FMCPACPC
FMCMPCNT EQU   FMCPACEM
FMCNPCNT EQU   FMCPACEN
FMCCONF  EQU   FMCSSTAT
FMCPURCH EQU   FMCSSTAT
FMCNSALU EQU   FMCSSTAT
FMCBIP   EQU   FMCSSTAT
FMCSDTR  EQU   FMCSSTAT
FMCUIPI  EQU   FMCSSTAT
FMCCIPI  EQU   FMCSSTAT
FMCSNBI  EQU   FMCSSTAT
FMCRVD01 EQU   ISTFMCB+158
FMCISSN  EQU   ISTFMCB+156
FMCOASN  EQU   FMCNXOSC
FMCSIP   EQU   FMCSIPA
FMCEXLST EQU   ISTFMCB+144
FMCPAB5  EQU   ISTFMCB+128
FMCPAB4  EQU   ISTFMCB+112
FMCPAB3  EQU   ISTFMCB+96
FMCPAB2  EQU   ISTFMCB+80
FMCPAB1  EQU   ISTFMCB+64
FMCFASYA EQU   ISTFMCB+60
FMCPROCD EQU   ISTFMCB+48
FMCVWT   EQU   ISTFMCB+44
FMCRPLH  EQU   ISTFMCB+40
FMCBUFNB EQU   FMCCMPS1+2
FMCBUFLM EQU   FMCCMPS1
FMCDEBA  EQU   ISTFMCB+32
@NM00049 EQU   FMCCMDFL
FMCSNA   EQU   FMCCMDFL
FMCVSNA  EQU   FMCCMDFL
FMCCMDR  EQU   FMCCMDFL
@NM00048 EQU   ISTFMCB+30
@NM00047 EQU   FMCSTAT1
FMCHPURG EQU   FMCSTAT1
FMCVSP2  EQU   FMCSTAT1
FMCVSP1  EQU   FMCSTAT
FMCLTEAS EQU   FMCSTAT
FMCZF007 EQU   FMCSTAT
FMCDEDSC EQU   FMCSTAT
FMCDEVOF EQU   FMCSTAT
FMCDAPT  EQU   FMCSTAT
FMCPRMTE EQU   FMCSTAT
FMCPRMTR EQU   FMCSTAT
FMCUSFLD EQU   ISTFMCB+24
FMCFMCBA EQU   ISTFMCB+20
FMCDNCBA EQU   ISTFMCB+16
FMCDVTA  EQU   ISTFMCB+12
FMCTSKID EQU   ISTFMCB+8
FMCNXTCB EQU   ISTFMCB+4
FMCSAF   EQU   ISTFMCB+2
FMCLNGTH EQU   ISTFMCB+1
FMCTYPE  EQU   ISTFMCB
PROSRDEF EQU   PROSETM4
PRORSV47 EQU   PROSETM4
PROSCON  EQU   PROSETM4
PRORSV46 EQU   PROSETM3
PROSMONT EQU   PROSETM3
PRORSV45 EQU   PROSETM3
PROSNTO  EQU   PROSETM3
PRORSV44 EQU   PROSETM3
PRORSV43 EQU   PROSETM2
PROSNTFL EQU   PROSETM2
PRORSV42 EQU   PROSETM2
PRORSV41 EQU   PROPROCS
PROMODC  EQU   PROPROC4
PROMODT  EQU   PROPROC4
PROMODM  EQU   PROPROC4
PROMODB  EQU   PROPROC4
PRORSV14 EQU   PROPROC4
PRORSV13 EQU   PROPROC4
PRORSV12 EQU   PROPROC4
PROEIB   EQU   PROPROC4
PRORSV11 EQU   PROPROC3
PROMONT2 EQU   PROPROC3
PROMONIT EQU   PROPROC3
PROSUBLK EQU   PROPROC3
PRONTO   EQU   PROPROC3
PROLGIN  EQU   PROPROC3
PROERPI  EQU   PROPROC3
PRORSV07 EQU   PROPROC3
PROCFTX  EQU   PROPROC2
PROEMLC  EQU   PROPROC2
PRONTFL  EQU   PROPROC2
PRORSV05 EQU   PROPROC2
PRORSV04 EQU   PROPROC2
PROLGOT  EQU   PROPROC2
PROERPO  EQU   PROPROC2
PRORSV03 EQU   PROPROC2
PRORPLC  EQU   PROPROC1
PROCS    EQU   PROPROC1
PROCA    EQU   PROPROC1
PRORESPX EQU   PROPROC1
PRODFASY EQU   PROPROC1
PROXPOPT EQU   PROPROC1
PROTRUNC EQU   PROPROC1
PROASYIP EQU   PROPROC1
DNCZC001 EQU   DNCOS+40
DNCLOCK  EQU   DNCOS+36
DNCZV005 EQU   DNCOS+32
DNCZV004 EQU   DNCOS+28
DNCZV003 EQU   DNCOS+24
DNCZV002 EQU   DNCOS+21
DNCZX001 EQU   DNCOS+20
DNCZV001 EQU   DNCOS+17
DNCZX003 EQU   DNCOS+16
DNCZV000 EQU   DNCOS+12
DNCSESCQ EQU   DNCOS+8
DNCSESIQ EQU   DNCOS+4
DNCACIP  EQU   DNCBSCED+1
DNCLICP  EQU   DNCBSCED
DNCZF008 EQU   DNCZFLGS+1
DNCZPSRD EQU   DNCZFLGS+1
DNCPSOL  EQU   DNCZFLGS+1
DNCZF007 EQU   DNCZFLGS+1
DNCZF006 EQU   DNCZFLGS+1
DNCZF005 EQU   DNCZFLGS+1
DNCZF004 EQU   DNCZFLGS+1
DNCZF003 EQU   DNCZFLGS
DNCZF002 EQU   DNCZFLGS
DNCZRSCH EQU   DNCZFLGS
DNCDISC  EQU   DNCZFLGS
DNCCONT  EQU   DNCZFLGS
DNCINVIT EQU   DNCZFLGS
DNCSESS  EQU   DNCZFLGS
DNCZF001 EQU   DNCZFLGS
DNCMODE  EQU   ISTDNCB+49
DNCBHSET EQU   ISTDNCB+48
DNCNCB   EQU   ISTDNCB
LDNZS00F EQU   LDNZH003+1
LDNZS00E EQU   LDNZH003+1
LDNZS00D EQU   LDNZH003+1
LDNZS00C EQU   LDNZH003+1
LDNZS00B EQU   LDNZH003+1
LDNZS007 EQU   LDNZH003
LDNZS006 EQU   LDNZH003
LDNZS005 EQU   LDNZH003
LDNZS004 EQU   LDNZH003
LDNZS003 EQU   LDNZH003
LDNZS002 EQU   LDNZH003
LDNZS001 EQU   LDNZH003
LDNZS000 EQU   LDNZH003
LDNZX006 EQU   ISTLDNCB+576
LDNZC001 EQU   ISTLDNCB+569
LDNZX005 EQU   ISTLDNCB+568
LDNZV006 EQU   ISTLDNCB+564
LDNSRB   EQU   ISTLDNCB+520
LDNIOSB  EQU   ISTLDNCB+412
@NM00046 EQU   LDNZSAV3+28
LDNRSV06 EQU   LDNZSAV4+16
LDNZSAV6 EQU   LDNZSAV4+12
LDNZSAV5 EQU   LDNZSAV4
LDNZSAVE EQU   LDNZSAV3
LDNZSAV2 EQU   ISTLDNCB+368
LDNZSAV1 EQU   ISTLDNCB+352
LDNZ3RR  EQU   ISTLDNCB+348
LDNZ2RR  EQU   ISTLDNCB+344
LDNZ1RR  EQU   ISTLDNCB+340
LDNRSV0B EQU   ISTLDNCB+336
LDNRSV0A EQU   ISTLDNCB+332
LDNRSV09 EQU   ISTLDNCB+328
LDNRSV08 EQU   ISTLDNCB+324
LDNRSV07 EQU   ISTLDNCB+320
LDNDEAD  EQU   LDNDCDEB+12
LDNUCBAD EQU   LDNDCDEB+8
LDNAPPAD EQU   LDNDCDEB+5
LDNEXSCL EQU   LDNDCDEB+4
LDNDCBAD EQU   LDNDCDEB+1
LDNDEBID EQU   LDNDCDEB
LDNZRIND EQU   ISTLDNCB+300
LDNRPH   EQU   ISTLDNCB+192
LDNZTCP  EQU   ISTLDNCB+188
LDNZCHNC EQU   ISTLDNCB+187
LDNZLPC7 EQU   LDNZLPC
LDNZLPC6 EQU   LDNZLPC
LDNZLPC5 EQU   LDNZLPC
LDNZLPC4 EQU   LDNZLPC
LDNZLPC3 EQU   LDNZLPC
LDNZLPC2 EQU   LDNZLPC
LDNZLPC1 EQU   LDNZLPC
LDNZLPC0 EQU   LDNZLPC
LDNRSV05 EQU   LDNZERPF
LDNZCTOV EQU   LDNZEPIN
LDNZF001 EQU   LDNZEPIN
LDNZFRDW EQU   LDNZCPAN
LDNZFRD  EQU   LDNZCPAN
LDNZNARD EQU   LDNZCPAN
LDNRSV04 EQU   LDNZIOLK
LDNZATBF EQU   LDNZIOLK
LDNZERLK EQU   LDNZIOLK
LDNZDSIC EQU   LDNZIOLK
LDNZPAB1 EQU   ISTLDNCB+168
LDNZV005 EQU   ISTLDNCB+160
LDNZS1VR EQU   ISTLDNCB+136
LDNZS1RR EQU   ISTLDNCB+132
LDNZS1FC EQU   ISTLDNCB+128
LDNZS1BC EQU   ISTLDNCB+124
LDNZLOCK EQU   ISTLDNCB+120
LDNZFBUF EQU   ISTLDNCB+116
LDNZLAST EQU   ISTLDNCB+112
LDNRSV02 EQU   LDNFLAG2
LDNZFF04 EQU   LDNFLAG2
LDNZFF69 EQU   LDNFLAGS+2
LDNTPBIO EQU   LDNFLAGS+2
LDNZFF03 EQU   LDNFLAGS+2
LDNZFF02 EQU   LDNFLAGS+2
LDNZFF01 EQU   LDNFLAGS+2
LDNZFF00 EQU   LDNFLAGS+2
LDNCLOSD EQU   LDNFLAGS+2
LDNZENTR EQU   LDNFLAGS+2
LDNZICCW EQU   LDNFLAGS+1
LDNZREC  EQU   LDNFLAGS+1
LDNZCTXT EQU   LDNFLAGS+1
LDNZATLK EQU   LDNFLAGS+1
LDNZCHNE EQU   LDNFLAGS+1
LDNZATTP EQU   LDNFLAGS+1
LDNZIOBY EQU   LDNFLAGS+1
LDNZF00A EQU   LDNFLAGS+1
LDNZF009 EQU   LDNFLAGS
LDNZF008 EQU   LDNFLAGS
LDNZF007 EQU   LDNFLAGS
LDNZF006 EQU   LDNFLAGS
LDNZF005 EQU   LDNFLAGS
LDNZF004 EQU   LDNFLAGS
LDNZF003 EQU   LDNFLAGS
LDNZF002 EQU   LDNFLAGS
LDNZHDCC EQU   ISTLDNCB+106
LDNZRB   EQU   ISTLDNCB+100
LDNCPQ   EQU   ISTLDNCB+96
LDNSIOQ  EQU   ISTLDNCB+92
LDNZX002 EQU   ISTLDNCB+91
LDNZX001 EQU   ISTLDNCB+90
LDNZH002 EQU   ISTLDNCB+88
LDNZV001 EQU   ISTLDNCB+84
LDNZACSW EQU   ISTLDNCB+76
LDNZC002 EQU   ISTLDNCB+68
LDNCSWSV EQU   ISTLDNCB+60
LDNCHFLD EQU   ISTLDNCB+56
LDNRESRV EQU   ISTLDNCB+55
@NM00045 EQU   LDNCFLAG
LDNRMF   EQU   LDNCFLAG
LDNIBI   EQU   LDNCFLAG
LDNTSAF  EQU   ISTLDNCB+52
LDNCUUA  EQU   ISTLDNCB+48
LDNCB    EQU   ISTLDNCB
NCBRSV01 EQU   ISTNCB+46
NCBSWDAF EQU   ISTNCB+44
NCBFMCBP EQU   ISTNCB+32
NCBLTRC  EQU   ISTNCB+30
NCBRVPT1 EQU   ISTNCB+28
NCBNMLLN EQU   NCBFLAGS+3
@NM00044 EQU   NCBFLAG1
@NM00043 EQU   NCBFLAG1
NCBTTF   EQU   NCBFLAG1
NCBRSYNC EQU   NCBFLAG1
NCBRSTRT EQU   NCBFLAG1
NCBCONF  EQU   NCBFLAG1
NCBSNAI  EQU   NCBFLAG1
NCBFLTIO EQU   NCBFTRIO
NCBCSTAT EQU   NCBFLAGS
NCBNCSL  EQU   ISTNCB+22
NCBNCBA  EQU   ISTNCB+20
NCBFMCBA EQU   ISTNCB+16
NCBNSNA  EQU   ISTNCB+12
NCBTSKID EQU   ISTNCB+8
NCBRDTE  EQU   ISTNCB+4
NCBDAF   EQU   ISTNCB+2
NCBLNGTH EQU   ISTNCB+1
NCBTYPE  EQU   ISTNCB
DEVRSV03 EQU   ISTDEVCH+5
DEVPHYSA EQU   ISTDEVCH+4
DEVCSLPN EQU   DEVCHAR3
DEVCSTCL EQU   DEVCHAR3
DEVCCHEK EQU   DEVCHAR3
DEVCATTN EQU   DEVCHAR3
DEVCSWL  EQU   DEVFCCTL
DEVCRVB  EQU   DEVFCCTL
DEVCSSL  EQU   DEVFCCTL
DEVCBSC  EQU   DEVFCCTL
DEVMCODE EQU   ISTDEVCH+2
DEVCHAR2 EQU   DEVTCODE
DEVRSV01 EQU   DEVCHAR
DEVCCTL  EQU   DEVCHAR
DEVNNSPT EQU   DEVCHAR
DEVSPS   EQU   DEVCHAR
DEVSUBND EQU   DEVCHAR
DEVCONVR EQU   DEVCHAR
DEVOTPUT EQU   DEVCHAR
DEVINPUT EQU   DEVCHAR
ATCAOS   EQU   ISTATCVT+1584
ATCPOPAB EQU   ATCAOSM+640
ATCVIOPM EQU   ATCAOSM+636
ATCTPMMI EQU   ATCAOSM+632
ATCTPMPB EQU   ATCAOSM+600
ATCRPLCT EQU   ATCAOSM+594
ATCCRACT EQU   ATCAOSM+592
ATCSRBCT EQU   ATCAOSM+590
ATCRSV75 EQU   ATCAOSM+589
ATCRSV66 EQU   ATCZFLAG
ATCZF001 EQU   ATCZFLAG
ATCTPCP1 EQU   ATCAOSM+584
ATCTPCP0 EQU   ATCAOSM+580
ATCAICTN EQU   ATCAOSM+576
ATCZR01  EQU   ATCAOSM+572
ATCMPLTA EQU   ATCAOSM+568
ATCASCB  EQU   ATCAOSM+564
ATCAP86  EQU   ATCAOSM+560
ATCAP85  EQU   ATCAOSM+556
ATCAP84  EQU   ATCAOSM+552
ATCAP83  EQU   ATCAOSM+548
ATCAP82  EQU   ATCAOSM+544
ATCAP69  EQU   ATCAOSM+540
ATCAP66  EQU   ATCAOSM+536
ATCAP62  EQU   ATCAOSM+532
ATCAPC59 EQU   ATCAOSM+528
ATCAP56  EQU   ATCAOSM+524
ATCAP55  EQU   ATCAOSM+520
ATCRCFY0 EQU   ATCAOSM+516
ATCAP41  EQU   ATCAOSM+512
ATCAPC25 EQU   ATCAOSM+508
ATCAP23  EQU   ATCAOSM+504
ATCTPSPB EQU   ATCAOSM+472
ATCTRMPB EQU   ATCAOSM+440
ATCPSTA  EQU   ATCAOSM+432
ATCSMABF EQU   ATCAOSM+428
ATCSMAPR EQU   ATCAOSM+424
ATCTPWQA EQU   ATCAOSM+420
ATCSMAID EQU   ATCAOSM+416
ATCSMARA EQU   ATCAOSM+412
ATCCACXA EQU   ATCAOSM+408
ATCZLAAB EQU   ATCAOSM+404
ATCATA10 EQU   ATCAOSM+400
ATCHLTCB EQU   ATCAOSM+396
@NM00042 EQU   ATCVTFLG
ATCVTACT EQU   ATCVTFLG
ATCRPT40 EQU   ATCAOSM+388
ATCRPT39 EQU   ATCAOSM+384
ATCRPT38 EQU   ATCAOSM+380
ATCRPT37 EQU   ATCAOSM+376
ATCRPT36 EQU   ATCAOSM+372
ATCRPT35 EQU   ATCAOSM+368
ATCRPT34 EQU   ATCAOSM+364
ATCRPT33 EQU   ATCAOSM+360
ATCRPT32 EQU   ATCAOSM+356
ATCRPT31 EQU   ATCAOSM+352
ATCRPT30 EQU   ATCAOSM+348
ATCRPT29 EQU   ATCAOSM+344
ATCRPT28 EQU   ATCAOSM+340
ATCRPT27 EQU   ATCAOSM+336
ATCRPT26 EQU   ATCAOSM+332
ATCRPT25 EQU   ATCAOSM+328
ATCRPT24 EQU   ATCAOSM+324
ATCRPT23 EQU   ATCAOSM+320
ATCRPT22 EQU   ATCAOSM+316
ATCRPT21 EQU   ATCAOSM+312
ATCRPT20 EQU   ATCAOSM+308
ATCRPT19 EQU   ATCAOSM+304
ATCRPT18 EQU   ATCAOSM+300
ATCRPT17 EQU   ATCAOSM+296
ATCRPT16 EQU   ATCAOSM+292
ATCRPT15 EQU   ATCAOSM+288
ATCRPT14 EQU   ATCAOSM+284
ATCRPT13 EQU   ATCAOSM+280
ATCRPT12 EQU   ATCAOSM+276
ATCRPT11 EQU   ATCAOSM+272
ATCAP38  EQU   ATCAOSM+268
ATCAP37  EQU   ATCAOSM+264
ATCZDFJ0 EQU   ATCAOSM+260
ATCAP51  EQU   ATCAOSM+256
ATCZPAB2 EQU   ATCAOSM+224
ATCZPAB1 EQU   ATCAOSM+192
ATCZLFVT EQU   ATCAOSM+188
ATCZLPVT EQU   ATCAOSM+184
ATCZDVTX EQU   ATCAOSM+180
ATCZDVTA EQU   ATCAOSM+176
ATCZDVT9 EQU   ATCAOSM+172
ATCZDVT8 EQU   ATCAOSM+168
ATCZDVT7 EQU   ATCAOSM+164
ATCRSV56 EQU   ATCDAPVT+22
ATCDAPDG EQU   ATCDAPVT+20
ATCDXCEA EQU   ATCDAPVT+16
ATCDCEA  EQU   ATCDAPVT+12
ATCDPCIA EQU   ATCDAPVT+8
ATCDSIOA EQU   ATCDAPVT+4
ATCDEOEA EQU   ATCDAPVT
ATCZLAEA EQU   ATCLDAPP+16
ATCZLCEA EQU   ATCLDAPP+12
ATCRSV17 EQU   ATCLDAPP+8
ATCSMFRR EQU   ATCLDAPP+4
ATCCDTID EQU   ATCLDAPP
ATCZAEA  EQU   ATCICAPP+16
ATCZCEA  EQU   ATCICAPP+12
ATCCDTCB EQU   ATCICAPP+8
ATCZSIOA EQU   ATCICAPP+4
ATCCDRPH EQU   ATCICAPP
ATCAP65  EQU   ATCAOSM+96
ATCZDVT6 EQU   ATCAOSM+92
ATCZDVT4 EQU   ATCAOSM+88
ATCZDVT3 EQU   ATCAOSM+84
ATCZDVT1 EQU   ATCAOSM+80
ATCICNCB EQU   ATCAOSM+76
ATCZFSVT EQU   ATCAOSM+68
ATCAP64  EQU   ATCAOSM+64
ATCAP63  EQU   ATCAOSM+60
ATCAP61  EQU   ATCAOSM+56
ATCAP58  EQU   ATCAOSM+52
ATCAP57  EQU   ATCAOSM+48
ATCAP54  EQU   ATCAOSM+44
ATCAP53  EQU   ATCAOSM+40
ATCAP52  EQU   ATCAOSM+36
ATCAP34  EQU   ATCAOSM+32
ATCAP32  EQU   ATCAOSM+28
ATCAP31  EQU   ATCAOSM+24
ATCAP12  EQU   ATCAOSM+20
ATCAP11  EQU   ATCAOSM+16
ATCCMTCB EQU   ATCAOSM+12
ATCPRPH  EQU   ATCAOSM+8
ATCGSRBF EQU   ATCGSRBG
ATCSRBRT EQU   ATCAOSM
ATCRSVBB EQU   ISTATCVT+908
ATCVDLOK EQU   ISTATCVT+904
ATCVFPAB EQU   ISTATCVT+872
ATCVDPAB EQU   ISTATCVT+840
ATCIOECB EQU   ISTATCVT+836
ATCINCW3 EQU   ISTATCVT+832
ATCMODTB EQU   ISTATCVT+828
ATCUSSPT EQU   ISTATCVT+824
ATCSRTAB EQU   ISTATCVT+820
@NM00041 EQU   ISTATCVT+818
ATCNMCTR EQU   ISTATCVT+816
ATCSEC51 EQU   ATCSECST
ATCSEC40 EQU   ATCSIPD+8
ATCSEC30 EQU   ATCSIPD+4
ATCSEC21 EQU   ATCSIPD
ATCSEC10 EQU   ATCSOPD+4
ATCSEC01 EQU   ATCSOPD
ATCPOIA  EQU   ISTATCVT+788
ATCPODVT EQU   ISTATCVT+784
ATCS49XL EQU   ISTATCVT+780
ATCS49XI EQU   ISTATCVT+776
ATCLCLCT EQU   ATCVLCNT+2
ATCRNCNT EQU   ATCVLCNT
ATCCFEAD EQU   ISTATCVT+768
ATCPTR00 EQU   ISTATCVT+736
ATCRSV10 EQU   ISTATCVT+704
ATCTACB  EQU   ISTATCVT+700
ATCVTINM EQU   ISTATCVT+696
ATCTCLIM EQU   ISTATCVT+692
ATCTCLOM EQU   ISTATCVT+688
ATCVTIPB EQU   ISTATCVT+656
ATCTCIPB EQU   ISTATCVT+624
ATCTIPAB EQU   ISTATCVT+592
ATCOCCCB EQU   ISTATCVT+588
ATCOCCOB EQU   ISTATCVT+584
ATCCDLAD EQU   ISTATCVT+580
ATCCCLAD EQU   ISTATCVT+576
ATCDLRPB EQU   ISTATCVT+544
ATCPGPAB EQU   ISTATCVT+512
ATCVPAB1 EQU   ISTATCVT+480
ATCRSV97 EQU   ISTATCVT+476
ATCRDTLK EQU   ISTATCVT+472
ATCVOCLK EQU   ISTATCVT+468
ATCCDFDN EQU   ISTATCVT+464
ATCCDFIN EQU   ISTATCVT+460
ATCDVTLK EQU   ISTATCVT+456
ATCAP36  EQU   ISTATCVT+452
ATCAP35  EQU   ISTATCVT+448
ATCAP33  EQU   ISTATCVT+444
ATCRSV99 EQU   ISTATCVT+412
ATCLDECB EQU   ISTATCVT+408
ATCLDNCS EQU   ISTATCVT+404
ATCCDPTR EQU   ISTATCVT+400
ATCADEL  EQU   ISTATCVT+396
ATCTODVT EQU   ISTATCVT+392
ATCDEBCH EQU   ISTATCVT+388
ATCHBSIZ EQU   ATCHPGM+2
ATCHBFNO EQU   ATCHPGM
ATCCRME  EQU   ISTATCVT+380
ATCRVCHS EQU   ISTATCVT+372
ATCECTLP EQU   ISTATCVT+368
ATCZDVTB EQU   ISTATCVT+364
ATCECPRT EQU   ISTATCVT+360
ATCVTLOD EQU   ISTATCVT+356
ATCTRPAB EQU   ISTATCVT+352
ATCMSGM  EQU   ISTATCVT+348
ATCMSGP  EQU   ISTATCVT+344
ATCEPT   EQU   ISTATCVT+340
ATCDVT   EQU   ISTATCVT+336
ATCTRCPT EQU   ISTATCVT+332
ATCDVLOD EQU   ISTATCVT+328
ATCALERT EQU   ISTATCVT+324
ATCEPA   EQU   ISTATCVT+320
ATCSAF   EQU   ISTATCVT+318
ATCXRANG EQU   ISTATCVT+317
ATCLAST  EQU   ATCSHRFG
ATCSHRTN EQU   ISTATCVT+308
ATCUEP   EQU   ISTATCVT+304
ATCOCRT  EQU   ISTATCVT+300
ATCCSMA  EQU   ATCCSMR
ATCESC02 EQU   ISTATCVT+292
ATCESC01 EQU   ISTATCVT+288
ATCCDFND EQU   ISTATCVT+284
ATCCDDEL EQU   ISTATCVT+280
ATCCDADD EQU   ISTATCVT+276
ATCMNT   EQU   ISTATCVT+272
ATCRSV16 EQU   ISTATCVT+270
ATCMAXID EQU   ISTATCVT+269
ATCMSGSP EQU   ATCCOM+268
ATCCIDM  EQU   ATCCOM+266
ATCACTRM EQU   ATCCOM+264
ATCNERAP EQU   ATCCOM+260
ATCNESAL EQU   ATCCOM+256
ATCRCC65 EQU   ATCCOM+252
ATCRCC26 EQU   ATCCOM+248
ATCRCC63 EQU   ATCCOM+244
ATCRSV11 EQU   ATCCOM+242
ATCVTMID EQU   ATCCOM+240
ATCAPOST EQU   ATCCOM+236
ATCNERFN EQU   ATCCOM+232
ATCNERNE EQU   ATCCOM+228
ATCNERCV EQU   ATCCOM+224
ATCNERST EQU   ATCCOM+220
ATCDCC61 EQU   ATCCOM+216
ATCDCC60 EQU   ATCCOM+212
ATCSRTDF EQU   ATCCOM+208
ATCSRTAD EQU   ATCCOM+204
ATCFIRA  EQU   ATCCOM+200
ATCSMBQ  EQU   ATCCOM+196
ATCSMRC  EQU   ATCCOM+192
ATCSMRS  EQU   ATCCOM+188
ATCSMQU  EQU   ATCCOM+184
ATCSMRQ  EQU   ATCCOM+180
ATCACDA  EQU   ATCCOM+176
ATCSTMA  EQU   ATCCOM+172
ATCFRSTR EQU   ATCCOM+168
ATCGTSTR EQU   ATCCOM+164
ATCREADA EQU   ATCCOM+160
ATCALOAD EQU   ATCCOM+156
ATCBLDLA EQU   ATCCOM+152
ATCFBRPH EQU   ATCCOM+148
ATCBFTRC EQU   ATCCOM+144
ATCTPBUF EQU   ATCTHTRC
ATCREMOV EQU   ATCCOM+136
ATCADD   EQU   ATCCOM+132
ATCBPDA  EQU   ATCCOM+128
ATCDVTPT EQU   ATCCOM+124
ATCFEPT  EQU   ATCCOM+120
ATCFDVT  EQU   ATCCOM+116
ATCCONFT EQU   ATCCOM+112
ATCPTCHA EQU   ATCCOM+108
ATCAUTHA EQU   ATCCOM+104
ATCACCTA EQU   ATCCOM+100
ATCRDTH  EQU   ATCCOM+96
ATCFRSCH EQU   ATCCOM+92
ATCFSB   EQU   ATCCOM+88
ATCHLTMS EQU   ATCCOM+84
ATCECPRM EQU   ATCCOM+80
ATCECNET EQU   ATCCOM+76
ATCECOPC EQU   ATCCOM+72
ATCVPARM EQU   ATCCOM+68
ATCECVRQ EQU   ATCCOM+64
ATCECSES EQU   ATCCOM+60
ATCOCHA  EQU   ATCCOM+56
ATCDSPLQ EQU   ATCCOM+52
ATCECDSP EQU   ATCCOM+48
ATCVARYQ EQU   ATCCOM+44
ATCECVRY EQU   ATCCOM+40
ATCHALTQ EQU   ATCCOM+36
ATCECHLT EQU   ATCCOM+32
ATCMODQ  EQU   ATCCOM+28
ATCECMOD EQU   ATCCOM+24
ATCASCHD EQU   ATCCOM+20
ATCSRT   EQU   ATCCOM+16
ATCRDT   EQU   ATCCOM+12
ATCDCFRR EQU   ATCCOM+8
ATCAPI   EQU   ATCCOM+4
ATCRSV04 EQU   ATCSTAT4
ATCNCSD  EQU   ATCSTAT4
ATCMTST  EQU   ATCSTAT4
ATCTPRT  EQU   ATCSTAT4
ATCPRTAT EQU   ATCSTAT4
@NM00040 EQU   ATCPOFLG
ATCPOUNS EQU   ATCPOFLG
ATCHSDMF EQU   ATCSTFLG
@NM00039 EQU   ATCSTAT1
ATCINHLT EQU   ATCSTAT1
ATCNETSL EQU   ATCSTAT1
ATCQKHLT EQU   ATCSTAT1
ATCSNHLT EQU   ATCSTAT1
ATCACTIV EQU   ATCSTAT1
ATCSTART EQU   ATCSTAT1
LOKXCL   EQU   @NM00037
LOKPRY   EQU   @NM00037
@NM00038 EQU   @NM00037
@NM00036 EQU   LOKCHNG
LOKCT    EQU   ISTLOK
DYPPAB   EQU   ISTDYPAB+16
DYPRSV01 EQU   ISTDYPAB+12
DYPTSKID EQU   ISTDYPAB+8
DYPCHAIN EQU   ISTDYPAB+4
DYPFLGS  EQU   ISTDYPAB+2
DYPLNGTH EQU   ISTDYPAB+1
DYPTYPE  EQU   ISTDYPAB
PABRPHA  EQU   PABRPHFG+1
PABNODQ  EQU   PABFLAGS
PABINHBT EQU   PABFLAGS
PABERLOK EQU   PABERRLK
PABSSN   EQU   PABFLAGS
PABDYNAM EQU   PABFLAGS
PABNORPR EQU   PABFLAGS
PABPRIOR EQU   PABFLAGS
PABAPYP  EQU   PABFLAGS
PABDVTA  EQU   ISTPAB+9
PABOFFST EQU   ISTPAB+8
PABCHNGP EQU   PABCHAIN+1
PABRSV02 EQU   PABCHAIN
PABRUNNG EQU   PABCHAIN
PABRESET EQU   PABCHAIN
PABUNCON EQU   PABCHAIN
PABCDP   EQU   PABCHAIN
PABRESCH EQU   PABCHAIN
PABCHNG  EQU   PABCHAIN
PABWEQP  EQU   PABWEQA+1
PABRSV01 EQU   PABWEQA
PABWEQG  EQU   PABWEQA
CRACHAIN EQU   ISTCRA+928
CRARSV02 EQU   ISTCRA+924
CRARSV01 EQU   ISTCRA+920
CRABFTBA EQU   CRABUFA
CRAMASK  EQU   CRASVC+64
CRAHSIZ  EQU   CRASVC+60
CRAASCRR EQU   CRASVC
CRAPROCR EQU   ISTCRA+312
CRAPSS   EQU   ISTCRA+160
CRARPH   EQU   ISTCRA+52
CRALAPTR EQU   CRABASIC+48
CRAL9PTR EQU   CRABASIC+44
CRAL8PTR EQU   CRABASIC+40
CRAL7PTR EQU   CRABASIC+36
CRAL6PTR EQU   CRABASIC+32
CRAL5PTR EQU   CRABASIC+28
CRAL4PTR EQU   CRABASIC+24
CRAL3PTR EQU   CRABASIC+20
CRAL2PTR EQU   CRABASIC+16
CRAL1PTR EQU   CRABASIC+12
CRALEV01 EQU   CRALKACT+3
CRALEV02 EQU   CRALKACT+3
CRALEV03 EQU   CRALKACT+3
CRALEV04 EQU   CRALKACT+3
CRALEV05 EQU   CRALKACT+3
CRALEV06 EQU   CRALKACT+3
CRALEV07 EQU   CRALKACT+3
CRALEV08 EQU   CRALKACT+3
CRALEV09 EQU   CRALKACT+2
CRALEV10 EQU   CRALKACT+2
CRALEV11 EQU   CRALKACT+2
CRALEV12 EQU   CRALKACT+2
CRALEV13 EQU   CRALKACT+2
CRALEV14 EQU   CRALKACT+2
CRALEV15 EQU   CRALKACT+2
CRALEV16 EQU   CRALKACT+2
CRALEV17 EQU   CRALKACT+1
CRALEV18 EQU   CRALKACT+1
CRALEV19 EQU   CRALKACT+1
CRALEV20 EQU   CRALKACT+1
CRALEV21 EQU   CRALKACT+1
CRALEV22 EQU   CRALKACT+1
CRALEV23 EQU   CRALKACT+1
CRALEV24 EQU   CRALKACT+1
CRALEV25 EQU   CRALKACT
CRALEV26 EQU   CRALKACT
CRALEV27 EQU   CRALKACT
CRALEV28 EQU   CRALKACT
CRALEV29 EQU   CRALKACT
CRALEV30 EQU   CRALKACT
CRALEV31 EQU   CRALKACT
CRALEV32 EQU   CRALKACT
CRAFRR   EQU   CRAHDR+4
CCFRSVD1 EQU   CFCCRT
CCFCR2   EQU   CFCCRT
CCFCRP   EQU   CFCCRT
CCFCR1   EQU   CFCCRT
CCFCS2   EQU   CFCCRT
CCFCS1   EQU   CFCCRT
NCFRSVD  EQU   CFCNTRD
NCFCR2   EQU   CFCNTRD
NCFCRP   EQU   CFCNTRD
NCFCR1   EQU   CFCNTRD
NCFCS2   EQU   CFCNTRD
NCFCS1   EQU   CFCNTRD
RIARSV03 EQU   RIAFLGS+2
RIARSV02 EQU   RIAFLG2
RIABR15  EQU   RIAFLG2
RIARSPCC EQU   RIAFLG2
RIARTPXT EQU   RIAFLG2
RIACONT  EQU   RIAFLG2
RIAFABN  EQU   RIAFLG2
RIARSV01 EQU   RIAFLG1
RIACTIVE EQU   RIAFLG1
RIACTL   EQU   RIAFLG1
RIAABR15 EQU   RIAABRGS+60
RIAABR14 EQU   RIAABRGS+56
RIAABR13 EQU   RIAABRGS+52
RIAABR12 EQU   RIAABRGS+48
RIAABR11 EQU   RIAABRGS+44
RIAABR10 EQU   RIAABRGS+40
RIAABR09 EQU   RIAABRGS+36
RIAABR08 EQU   RIAABRGS+32
RIAABR07 EQU   RIAABRGS+28
RIAABR06 EQU   RIAABRGS+24
RIAABR05 EQU   RIAABRGS+20
RIAABR04 EQU   RIAABRGS+16
RIAABR03 EQU   RIAABRGS+12
RIAABR02 EQU   RIAABRGS+8
RIAABR01 EQU   RIAABRGS+4
RIAABR00 EQU   RIAABRGS
RIARSV04 EQU   ISTRIA+14
RIAABPSW EQU   ISTRIA+4
RIAABCD  EQU   ISTRIA
ASCPSREG EQU   ISTASCRR+44
ASCUSR01 EQU   ASCUSRGS+12
ASCUSR00 EQU   ASCUSRGS+8
ASCUSR15 EQU   ASCUSRGS+4
ASCUSR14 EQU   ASCUSRGS
ASCETRYA EQU   ISTASCRR+24
ASCKEY   EQU   ISTASCRR+23
ASCAUDIT EQU   ASCFLG3
ASCFBDUD EQU   ASCFLG3
ASCLLKF  EQU   ASCFLG3
ASCKEYF  EQU   ASCFLG3
ASCFBDAL EQU   ASCFLG3
ASCFSRGT EQU   ASCFLG3
ASCICR   EQU   ASCFLG2
ASCFSRIC EQU   ASCFLG2
ASCFBDIC EQU   ASCFLG2
ASCFBAIC EQU   ASCFLG2
ASCPASTR EQU   ASCFLG1
ASCFSR   EQU   ASCFLG1
ASCFBD   EQU   ASCFLG1
ASCFBA   EQU   ASCFLG1
ASCPFCRR EQU   ISTASCRR
CRRRVPT5 EQU   CRRSAREA+316
CRRRVPT4 EQU   CRRSAREA+312
CRRRVPT3 EQU   CRRSAREA+308
CRRRVPT2 EQU   CRRSAREA+304
CRRRVPT1 EQU   CRRSAREA+300
CRRRVCR2 EQU   CRRSAREA+292
CRRRVCR1 EQU   CRRSAREA+291
CRRXFNCD EQU   CRRSAREA+290
CRRLDOCT EQU   CRRSAREA+288
CRRBTLEN EQU   CRRSAREA+284
CRRBLEN  EQU   CRRSAREA+282
CRRBRTCD EQU   CRRSAREA+281
@NM00035 EQU   CRRBFLGS
CRRBFLG1 EQU   CRRBFLGS
CRRBLDAD EQU   CRRSAREA+276
CRRBARAD EQU   CRRSAREA+272
CRRRTN12 EQU   CRRSAREA+268
CRRRTN11 EQU   CRRSAREA+264
CRRRTN10 EQU   CRRSAREA+260
CRRRTN9  EQU   CRRSAREA+256
CRRRTN8  EQU   CRRSAREA+252
CRRRTN7  EQU   CRRSAREA+248
CRRRTN6  EQU   CRRSAREA+244
CRRRTN5  EQU   CRRSAREA+240
CRRRTN4  EQU   CRRSAREA+236
CRRRTN3  EQU   CRRSAREA+232
CRRRTN2  EQU   CRRSAREA+228
CRRRTN1  EQU   CRRSAREA+224
CRRRES1  EQU   CRRSAREA+222
CRRLCNT  EQU   CRRSAREA+220
CRRATCVT EQU   CRRSAREA+216
CRRVALCK EQU   CRRSAREA+212
CRRARPL  EQU   CRRTRBUF+4
CRRBIND  EQU   CRRTRBUF+3
CRRTRRES EQU   CRRTRBUF
CRRCLDO  EQU   CRRSAREA+200
CRRUECB  EQU   CRRSAREA+196
CRRLCPB  EQU   CRRSAREA+192
CRRCMDAD EQU   CRRSAREA+188
CRRFMCB  EQU   CRRSAREA+184
CRRCLCCW EQU   CRRSAREA+180
CRRESR2  EQU   CRRESR+1
CRRESR1  EQU   CRRESR
CRRDSB2  EQU   CRRDSB+1
CRRDSB1  EQU   CRRDSB
CRRDTACT EQU   CRRSAREA+174
CRRRSV99 EQU   CRRSAREA+173
CRRRDSOH EQU   CRRFDB3
CRRRLG   EQU   CRRFDB3
CRRLGFRC EQU   CRRFDB3
CRRRDEOT EQU   CRRFDB3
CRRRDEOM EQU   CRRFDB3
CRRRDEOB EQU   CRRFDB3
CRRUNUSD EQU   CRRFDB3
CRRUINPT EQU   CRRFDB3
CRRSTSAV EQU   CRRFDB2
CRRCUERR EQU   CRRFDB2
CRRDLGFL EQU   CRRFDB2
CRRIOERR EQU   CRRFDB2
CRRDVUNS EQU   CRRFDB2
CRRATND  EQU   CRRFDB2
CRRRVID  EQU   CRRFDB2
CRRERLK  EQU   CRRFDB2
CRRRTNCD EQU   CRRFDBK1
CRRNSNA  EQU   CRRRTYPE
CRRDFSYN EQU   CRRRTYPE
CRRRESP  EQU   CRRRTYPE
CRRDFASY EQU   CRRRTYPE
CRRDBLCK EQU   CRRRFLG1
CRRRPSBD EQU   CRRRFLG1
CRRRPLOQ EQU   CRRRFLG1
CRRRPLS  EQU   CRRRFLG1
CRRSW7   EQU   CRRFLGS
CRRSW6   EQU   CRRFLGS
CRRSW5   EQU   CRRFLGS
CRRSW4   EQU   CRRFLGS
CRRSW3   EQU   CRRFLGS
CRRSW2   EQU   CRRFLGS
CRRSW1   EQU   CRRFLGS
CRRSW0   EQU   CRRFLGS
CRRRSV04 EQU   CRRFLGS4
CRRDLCCW EQU   CRRFLGS3
CRRRESET EQU   CRRFLGS3
CRRSUBBK EQU   CRRFLGS3
CRRRSV08 EQU   CRRFLGS3
CRRDEBQ  EQU   CRRFLGS3
CRRSTSPR EQU   CRRFLGS3
CRRDREQ  EQU   CRRFLGS3
CRRDFDBK EQU   CRRFLGS3
CRRDFSB  EQU   CRRFLGS2
CRRDPROC EQU   CRRFLGS2
CRREXTS  EQU   CRRFLGS2
CRRELSRI EQU   CRRFLGS2
CRRPOST  EQU   CRRFLGS2
CRRENDLW EQU   CRRFLGS2
CRRPRGIP EQU   CRRFLGS2
CRRDEXP  EQU   CRRFLGS2
CRRSOLRQ EQU   CRRFLGS1
CRRREQP  EQU   CRRFLGS1
CRRCPROC EQU   CRRFLGS1
CRRFINI  EQU   CRRFLGS1
CRRPREIO EQU   CRRFLGS1
CRRINIO  EQU   CRRFLGS1
CRRFLDOS EQU   CRRFLGS1
CRRDOCUR EQU   CRRFLGS1
CRRRATS4 EQU   CRRATS+7
@NM00034 EQU   CRRSATS
CRRASC51 EQU   CRRSATS
CRRASC40 EQU   CRRSATS
CRRASC30 EQU   CRRSATS
CRRASC21 EQU   CRRSATS
CRRASC10 EQU   CRRSATS
CRRASC01 EQU   CRRSATS
@NM00033 EQU   CRRRATS2
CRRPLIC  EQU   CRRRATS2
CRRRC81  EQU   CRRRATS2
CRRRC80  EQU   CRRRATS2
CRRRC65  EQU   CRRRATS2
CRRRC63  EQU   CRRRATS2
CRRRC55  EQU   CRRRATS2
CRRRC54  EQU   CRRRATS2
CRRRC53  EQU   CRRRATS1
CRRRC52  EQU   CRRRATS1
CRRRC51  EQU   CRRRATS1
CRRRC30  EQU   CRRRATS1
CRRRC26  EQU   CRRRATS1
CRRRC23  EQU   CRRRATS1
CRRRC22  EQU   CRRRATS1
CRRRC21  EQU   CRRRATS1
@NM00032 EQU   CRRATS4
CRRC3X   EQU   CRRATS4
CRRC85   EQU   CRRATS4
CRRC83   EQU   CRRATS4
CRRC82   EQU   CRRATS4
CRRC61   EQU   CRRATS4
CRRC60   EQU   CRRATS4
CRRC81   EQU   CRRATS3
CRRC80   EQU   CRRATS3
CRRC75   EQU   CRRATS3
CRRC51   EQU   CRRATS3
CRRC50   EQU   CRRATS3
CRRC42   EQU   CRRATS3
CRRC41   EQU   CRRATS3
CRRC40   EQU   CRRATS3
CRRC32   EQU   CRRATS2
CRRC31   EQU   CRRATS2
CRRC30   EQU   CRRATS2
CRRC25   EQU   CRRATS2
CRRC24   EQU   CRRATS2
CRRC23   EQU   CRRATS2
CRRC22   EQU   CRRATS2
CRRC21   EQU   CRRATS2
CRRC20   EQU   CRRATS1
CRRC13   EQU   CRRATS1
CRRC12   EQU   CRRATS1
CRRC11   EQU   CRRATS1
CRRC10   EQU   CRRATS1
CRRC02   EQU   CRRATS1
CRRC01   EQU   CRRATS1
CRRC00   EQU   CRRATS1
CRRRMAS4 EQU   CRRMAS+7
@NM00031 EQU   CRRSMAS
CRRMSC51 EQU   CRRSMAS
CRRMSC40 EQU   CRRSMAS
CRRMSC30 EQU   CRRSMAS
CRRMSC21 EQU   CRRSMAS
CRRMSC10 EQU   CRRSMAS
CRRMSC01 EQU   CRRSMAS
@NM00030 EQU   CRRRMAS2
CRRRM81  EQU   CRRRMAS2
CRRRM80  EQU   CRRRMAS2
CRRRM65  EQU   CRRRMAS2
CRRRM63  EQU   CRRRMAS2
CRRRM55  EQU   CRRRMAS2
CRRRM54  EQU   CRRRMAS2
CRRRM53  EQU   CRRRMAS1
CRRRM52  EQU   CRRRMAS1
CRRRM51  EQU   CRRRMAS1
CRRRM30  EQU   CRRRMAS1
CRRRM26  EQU   CRRRMAS1
CRRRM23  EQU   CRRRMAS1
CRRRM22  EQU   CRRRMAS1
CRRRM21  EQU   CRRRMAS1
@NM00029 EQU   CRRMAS4
CRRM3X   EQU   CRRMAS4
CRRM85   EQU   CRRMAS4
CRRM83   EQU   CRRMAS4
CRRM82   EQU   CRRMAS4
CRRM61   EQU   CRRMAS4
CRRM60   EQU   CRRMAS4
CRRM81   EQU   CRRMAS3
CRRM80   EQU   CRRMAS3
CRRM75   EQU   CRRMAS3
CRRM51   EQU   CRRMAS3
CRRM50   EQU   CRRMAS3
CRRM42   EQU   CRRMAS3
CRRM41   EQU   CRRMAS3
CRRM40   EQU   CRRMAS3
CRRM32   EQU   CRRMAS2
CRRM31   EQU   CRRMAS2
CRRM30   EQU   CRRMAS2
CRRM25   EQU   CRRMAS2
CRRM24   EQU   CRRMAS2
CRRM23   EQU   CRRMAS2
CRRM22   EQU   CRRMAS2
CRRM21   EQU   CRRMAS2
CRRM20   EQU   CRRMAS1
CRRM13   EQU   CRRMAS1
CRRM12   EQU   CRRMAS1
CRRM11   EQU   CRRMAS1
CRRM10   EQU   CRRMAS1
CRRM02   EQU   CRRMAS1
CRRM01   EQU   CRRMAS1
CRRM00   EQU   CRRMAS1
CRRFSB   EQU   CRRSAREA+144
CRRSAVE1 EQU   CRRSAREA+72
CRRSAVE  EQU   CRRSAREA
CRRRSV22 EQU   ISTDCCRR+4
CRRCRR   EQU   ISTDCCRR
@NM00028 EQU   ISTAPCRR+137
@NM00027 EQU   APC57FLG
APCNREL  EQU   APC57FLG
APCRECRA EQU   APC57FLG
@NM00026 EQU   APC54FLG
APCRLST  EQU   APC54FLG
APCPR153 EQU   APC53FLG
@NM00025 EQU   APC52FLG
APCWKF   EQU   APC52FLG
APCNWF   EQU   APC52FLG
APCRDQ   EQU   APC52FLG
APCQENQ  EQU   APC52FLG
APCSMSR  EQU   APC52FLG
APCCKRQ  EQU   APC52FLG
@NM00024 EQU   APC31FLG
APCSORT  EQU   APC31FLG
APCRESCH EQU   APC31FLG
APCTPXIT EQU   APC31FLG
APCSWTWD EQU   ISTAPCRR+128
APCSAV2  EQU   ISTAPCRR+124
APCSAV1  EQU   ISTAPCRR+120
APCRETCD EQU   APCCOMF+2
@NM00023 EQU   APCCOMF1+1
APCZLBER EQU   APCCOMF1
APCEXIT  EQU   APCCOMF1
APCRELR  EQU   APCCOMF1
APCIRB2  EQU   APCCOMF1
APCIRB1  EQU   APCCOMF1
APCSMCR  EQU   APCCOMF1
APCUETSK EQU   APCCOMF1
APCSSTSK EQU   APCCOMF1
APCESTAL EQU   APCSAVE
APCFRRPM EQU   ISTAPCRR+40
APCQWFO  EQU   ISTAPCRR+36
APCPABSV EQU   ISTAPCRR+32
APCRTRN  EQU   ISTAPCRR+28
APCTCBA  EQU   ISTAPCRR+24
APCPSTA  EQU   ISTAPCRR+20
@NM00022 EQU   APCRFLG+1
APC31R   EQU   APCRFLG
APC58R   EQU   APCRFLG
APC57R   EQU   APCRFLG
APC56R   EQU   APCRFLG
APC54R   EQU   APCRFLG
APC53R   EQU   APCRFLG
APC52R   EQU   APCRFLG
APC51R   EQU   APCRFLG
@NM00021 EQU   APCATFLG+1
APC31E   EQU   APCATFLG
APC58E   EQU   APCATFLG
APC57E   EQU   APCATFLG
APC56E   EQU   APCATFLG
APC54E   EQU   APCATFLG
APC53E   EQU   APCATFLG
APC52E   EQU   APCATFLG
APC51E   EQU   APCATFLG
APCHDRSV EQU   ISTAPCRR
PFCRSV01 EQU   ISTPFCRR+13
PFCFTA   EQU   ISTPFCRR+12
CRRFRR   EQU   PFCFRRO
CRRCHAIN EQU   PFCCRRO
CRRLEN   EQU   PFCCRRLN
PFCRSV10 EQU   PFCCRR+1
CRRID    EQU   PFCCRRID
ZCRRU    EQU   ZCRRHRU+3
ZCRRH    EQU   ZCRRHRU
ZCRWORDA EQU   ISTZCRR+477
ZCRCMNRC EQU   ISTZCRR+476
ZCRRSV97 EQU   ZCRENTER+18
ZCREIC2B EQU   ZCRENTER+18
ZCREAC2B EQU   ZCRENTER+18
ZCRELCAB EQU   ZCRENTER+18
ZCREDCMF EQU   ZCRENTER+18
ZCREBCIF EQU   ZCRENTER+18
ZCREICSG EQU   ZCRENTER+18
ZCREDCK0 EQU   ZCRENTER+18
ZCRECFY0 EQU   ZCRENTER+17
ZCREKCCB EQU   ZCRENTER+17
ZCREDCNB EQU   ZCRENTER+17
ZCREICOE EQU   ZCRENTER+17
ZCREBCNB EQU   ZCRENTER+17
ZCREBC0L EQU   ZCRENTER+17
ZCREBCAL EQU   ZCRENTER+17
ZCREDCQR EQU   ZCRENTER+17
ZCREDCPR EQU   ZCRENTER+16
ZCREBCSB EQU   ZCRENTER+16
ZCREAC1R EQU   ZCRENTER+16
ZCRECCCI EQU   ZCRENTER+16
ZCRECCAI EQU   ZCRENTER+16
ZCRECCBI EQU   ZCRENTER+16
ZCREBCAR EQU   ZCRENTER+16
ZCREBCDR EQU   ZCRENTER+16
ZCREBCBR EQU   ZCRENTER+15
ZCREDCXR EQU   ZCRENTER+15
ZCREDCWB EQU   ZCRENTER+15
ZCREDCWR EQU   ZCRENTER+15
ZCREKCCN EQU   ZCRENTER+15
ZCREKCBN EQU   ZCRENTER+15
ZCREKCBB EQU   ZCRENTER+15
ZCREJC0B EQU   ZCRENTER+15
ZCREJCDB EQU   ZCRENTER+14
ZCREJCCB EQU   ZCRENTER+14
ZCREJCBB EQU   ZCRENTER+14
ZCREJCAB EQU   ZCRENTER+14
ZCREIC1B EQU   ZCRENTER+14
ZCREIC0B EQU   ZCRENTER+14
ZCREICX0 EQU   ZCRENTER+14
ZCREICWB EQU   ZCRENTER+14
ZCREICVB EQU   ZCRENTER+13
ZCREICUB EQU   ZCRENTER+13
ZCREICTB EQU   ZCRENTER+13
ZCREICSC EQU   ZCRENTER+13
ZCREICSB EQU   ZCRENTER+13
ZCREICRB EQU   ZCRENTER+13
ZCREICQB EQU   ZCRENTER+13
ZCREICQA EQU   ZCRENTER+13
ZCREICOB EQU   ZCRENTER+12
ZCREICOA EQU   ZCRENTER+12
ZCREICNB EQU   ZCRENTER+12
ZCREICNA EQU   ZCRENTER+12
ZCREICMB EQU   ZCRENTER+12
ZCREICLB EQU   ZCRENTER+12
ZCREICKG EQU   ZCRENTER+12
ZCREICKE EQU   ZCRENTER+12
ZCREICKB EQU   ZCRENTER+11
ZCREICJB EQU   ZCRENTER+11
ZCREICID EQU   ZCRENTER+11
ZCREICIB EQU   ZCRENTER+11
ZCREICHB EQU   ZCRENTER+11
ZCREICGB EQU   ZCRENTER+11
ZCREICEB EQU   ZCRENTER+11
ZCREICDB EQU   ZCRENTER+11
ZCREICCI EQU   ZCRENTER+10
ZCREICCB EQU   ZCRENTER+10
ZCREICBI EQU   ZCRENTER+10
ZCREICBB EQU   ZCRENTER+10
ZCREICAB EQU   ZCRENTER+10
ZCREICAA EQU   ZCRENTER+10
ZCREHCAN EQU   ZCRENTER+10
ZCREGC0B EQU   ZCRENTER+10
ZCREGC0A EQU   ZCRENTER+9
ZCREGCLA EQU   ZCRENTER+9
ZCREGCEB EQU   ZCRENTER+9
ZCREGCDB EQU   ZCRENTER+9
@NM00020 EQU   ZCRENTER+9
ZCREGCBB EQU   ZCRENTER+9
ZCREGCAB EQU   ZCRENTER+9
ZCREFCEB EQU   ZCRENTER+9
ZCREFCEA EQU   ZCRENTER+8
ZCREECTA EQU   ZCRENTER+8
ZCREECEA EQU   ZCRENTER+8
ZCREECDA EQU   ZCRENTER+8
ZCREECCB EQU   ZCRENTER+8
ZCREECCA EQU   ZCRENTER+8
ZCREECBB EQU   ZCRENTER+8
ZCREECBA EQU   ZCRENTER+8
ZCREDCMB EQU   ZCRENTER+7
ZCREDCL0 EQU   ZCRENTER+7
ZCREDCKB EQU   ZCRENTER+7
ZCREDCKA EQU   ZCRENTER+7
ZCREDCJ0 EQU   ZCRENTER+7
ZCREDCI0 EQU   ZCRENTER+7
ZCREDCH0 EQU   ZCRENTER+7
ZCREDCF0 EQU   ZCRENTER+7
ZCREDCE0 EQU   ZCRENTER+6
ZCREDCD0 EQU   ZCRENTER+6
ZCREDCC0 EQU   ZCRENTER+6
ZCREDCB0 EQU   ZCRENTER+6
ZCREDCA0 EQU   ZCRENTER+6
ZCRECC2B EQU   ZCRENTER+6
ZCRECC1B EQU   ZCRENTER+6
ZCRECC1A EQU   ZCRENTER+6
ZCRECC0B EQU   ZCRENTER+5
ZCRECC0A EQU   ZCRENTER+5
ZCREBC0B EQU   ZCRENTER+5
ZCREBC0A EQU   ZCRENTER+5
ZCREBCZ0 EQU   ZCRENTER+5
ZCREBCY0 EQU   ZCRENTER+5
ZCREBCX0 EQU   ZCRENTER+5
ZCREBCXA EQU   ZCRENTER+5
ZCREBCSC EQU   ZCRENTER+4
ZCREBCRB EQU   ZCRENTER+4
ZCREBCRA EQU   ZCRENTER+4
ZCREBCQB EQU   ZCRENTER+4
ZCREBCPB EQU   ZCRENTER+4
ZCREBCOB EQU   ZCRENTER+4
ZCREBCNE EQU   ZCRENTER+4
ZCREBCND EQU   ZCRENTER+4
ZCREBCMB EQU   ZCRENTER+3
ZCREBCLB EQU   ZCRENTER+3
ZCREBCLA EQU   ZCRENTER+3
ZCREBCKB EQU   ZCRENTER+3
ZCREBCKA EQU   ZCRENTER+3
ZCREBCJB EQU   ZCRENTER+3
ZCREBCJA EQU   ZCRENTER+3
ZCREBCID EQU   ZCRENTER+3
ZCREBCIB EQU   ZCRENTER+2
ZCREBCIA EQU   ZCRENTER+2
ZCREBCHB EQU   ZCRENTER+2
ZCREBCHA EQU   ZCRENTER+2
ZCREBCGB EQU   ZCRENTER+2
ZCREBCGA EQU   ZCRENTER+2
ZCREBCFB EQU   ZCRENTER+2
ZCREBCFA EQU   ZCRENTER+2
ZCREBCEB EQU   ZCRENTER+1
ZCREBCDI EQU   ZCRENTER+1
ZCREBCCB EQU   ZCRENTER+1
ZCREBCBI EQU   ZCRENTER+1
ZCREBCBB EQU   ZCRENTER+1
ZCREBCAM EQU   ZCRENTER+1
ZCREBCAI EQU   ZCRENTER+1
ZCREBCAG EQU   ZCRENTER
ZCREBCAE EQU   ZCRENTER
ZCREBCAB EQU   ZCRENTER
ZCREBCAA EQU   ZCRENTER
ZCREAC1B EQU   ZCRENTER
ZCREAC0B EQU   ZCRENTER
ZCREAC0A EQU   ZCRENTER
ZCREAC1A EQU   ZCRENTER
ZCRTNAME EQU   ZCRAUDIT+1
ZCRTID   EQU   ZCRAUDIT
ZCRREMIC EQU   ISTZCRR+436
ZCRSAVEZ EQU   ISTZCRR+196
ZCRDAFSV EQU   ZCRCIDSV+2
ZCRSAFSV EQU   ZCRCIDSV
ZCRR13SV EQU   ISTZCRR+188
ZCRR2SV  EQU   ISTZCRR+184
ZCROPCDE EQU   ISTZCRR+183
ZCRPRMTC EQU   ISTZCRR+182
ZCRRCDSV EQU   ISTZCRR+181
ZCRAWQSW EQU   ISTZCRR+180
ZCRTOPQ  EQU   ISTZCRR+180
ZCRLSTFB EQU   ISTZCRR+180
ZCRXSCHD EQU   ISTZCRR+180
ZCRNXTEL EQU   ISTZCRR+180
ZCRSCANQ EQU   ISTZCRR+180
ZCRMOREL EQU   ISTZCRR+180
ZCRFBPRC EQU   ISTZCRR+180
ZCRFSBSV EQU   ISTZCRR+176
ZCRSVWK5 EQU   ZCRWKARA+16
ZCRSVWK4 EQU   ZCRWKARA+12
ZCRSVWK3 EQU   ZCRWKARA+8
ZCRSVWK2 EQU   ZCRWKARA+4
ZCRSVF25 EQU   ZCRSVRTN+96
ZCRSVF24 EQU   ZCRSVRTN+92
ZCRSVF23 EQU   ZCRSVRTN+88
ZCRSVF22 EQU   ZCRSVRTN+84
ZCRSVF21 EQU   ZCRSVRTN+80
ZCRSVF20 EQU   ZCRSVRTN+76
ZCRSVF19 EQU   ZCRSVRTN+72
ZCRSVF18 EQU   ZCRSVRTN+68
ZCRSVF17 EQU   ZCRSVRTN+64
ZCRSVF16 EQU   ZCRSVRTN+60
ZCRSVF15 EQU   ZCRSVRTN+56
ZCRSVF14 EQU   ZCRSVRTN+52
ZCRSVF13 EQU   ZCRSVRTN+48
ZCRSVF12 EQU   ZCRSVRTN+44
ZCRSVF11 EQU   ZCRSVRTN+40
ZCRSVF10 EQU   ZCRSVRTN+36
ZCRSVF9  EQU   ZCRSVRTN+32
ZCRSVF8  EQU   ZCRSVRTN+28
ZCRSVF7  EQU   ZCRSVRTN+24
ZCRSVF6  EQU   ZCRSVRTN+20
ZCRSVF5  EQU   ZCRSVRTN+16
ZCRSVF4  EQU   ZCRSVRTN+12
ZCRSVF3  EQU   ZCRSVRTN+8
ZCRSVF2  EQU   ZCRSVRTN+4
ZCRSVF1  EQU   ZCRSVRTN
ZCRPVZB  EQU   ISTZCRR+52
ZCRPSW   EQU   ZCRPARMS+15
ZCRPCHAR EQU   ZCRPARMS+14
ZCRPRES2 EQU   ZCRPCOD2
ZCRPMFGF EQU   ZCRPCOD2
ZCRPFHDR EQU   ZCRPCOD2
ZCRPPG2  EQU   ZCRPCOD2
ZCRPRES  EQU   ZCRPCOD1
ZCRPCFTX EQU   ZCRPCOD1
ZCRPBUF  EQU   ZCRPCOD1
ZCRPLSCB EQU   ZCRPCOD1
ZCRPLSBQ EQU   ZCRPLSBF
ZCRPFSB  EQU   ZCRPCOD1
ZCRPLNG  EQU   ZCRPCOD1
ZCRPPG   EQU   ZCRPCOD1
ZCRDTODA EQU   ZCRDFRDA
ZCRMLNG  EQU   ZCRPARMS+8
ZCRTBPTR EQU   ZCRPARMS+4
ZCRCBPTR EQU   ZCRFBPTR
ZCRCRLCP EQU   ISTZCRR+20
@NM00019 EQU   ZCRSCPSW
ZCRSKIPT EQU   ZCRSCPSW
ZCRFSBWE EQU   ZCRSCPSW
ZCRPRCMP EQU   ZCRSCPSW
ZCRLREQ  EQU   ZCRSCPSW
ZCRRJLCP EQU   ZCRSCPSW
ZCRTPINV EQU   ZCRSCPSW
ZCRTPEX  EQU   ZCRSCPSW
@NM00018 EQU   ZCRIBPR
ZCR3270R EQU   ZCRIBPR
ZCR3270  EQU   ZCRIBPR
ZCRLCPFD EQU   ZCRIBPR
ZCRLCPFR EQU   ZCRIBPR
ZCRRELK  EQU   ZCRIBPR
ZCRERR   EQU   ZCRIBPR
ZCRNOBUF EQU   ZCRIBPR
@NM00017 EQU   ZCRPFLGS
ZCRQAHED EQU   ZCRPFLGS
ZCRF1RES EQU   ZCRGENSW
ZCRBRIS  EQU   ZCRGENSW
ZCRCLRBD EQU   ZCRGENSW
ZCREXRES EQU   ZCRGENSW
ZCRLCSYN EQU   ZCRGENSW
ZCRRT06  EQU   ZCRGENSW
ZCRTSW2  EQU   ZCRGENSW
ZCRTSW1  EQU   ZCRGENSW
ZCRINLCP EQU   ZCRFLGS3
ZCRWRPTR EQU   ZCRFLGS3
ZCRNOFRE EQU   ZCRFLGS3
ZCREDRIV EQU   ZCRFLGS3
ZCRDISC  EQU   ZCRFLGS3
ZCRTPCON EQU   ZCRFLGS3
ZCRCNLXL EQU   ZCRRESET
ZCRENDAN EQU   ZCRSWFLD
ZCRENDXL EQU   ZCRSWFLD
ZCRENDX  EQU   ZCRSWFLD
ZCROB1   EQU   ZCRNDLCP
ZCRENDPR EQU   ZCRSWFLD
ZCREXTYP EQU   ZCRSWFLD
ZCRSLCP  EQU   ZCRENDSH
ZCRIF0E3 EQU   ZCRFLGS2
ZCRIF0E2 EQU   ZCRFLGS2
ZCRIF0E1 EQU   ZCRFLGS2
ZCROB    EQU   ZCRFLGS2
ZCRFMSED EQU   ZCRFLGS2
ZCRSHEDI EQU   ZCRFLGS2
ZCRLOCAL EQU   ZCRFLGS2
ZCRFEXIT EQU   ZCRFLGS1
ZCRUNSOL EQU   ZCRFLGS1
ZCRENDLP EQU   ZCRFLGS1
ZCRNRMEX EQU   ZCRFLGS1
ZCRWAIT  EQU   ZCRFLGS1
ZCRDEQSW EQU   ZCRFLGS1
ZCRQUESW EQU   ZCRFLGS1
ZCREXSW  EQU   ZCRFLGS1
ZCRFRRA  EQU   ISTZCRR+8
ZCRCHAIN EQU   ISTZCRR+4
ZCRFLAG  EQU   ZCRCRR+3
ZCRLEN   EQU   ZCRCRR+1
ZCRID    EQU   ZCRCRR
LCPFTYPE EQU   LCPRPHA
LCPRSTQH EQU   LCPLCCW
LCPCLREP EQU   ISTLCPB+44
LCPUBFSB EQU   LCPFLCCW
LCPRSV77 EQU   ISTLCPB+39
LCPLCCWC EQU   ISTLCPB+38
LCPRSV78 EQU   LCPFLG1
LCPTRANS EQU   LCPFLG1
LCPSTLB  EQU   LCPFLG1
LCPREDRV EQU   LCPFLG1
LCPDIRPT EQU   LCPFLG1
LCPINTER EQU   LCPFLG1
LCPRESET EQU   LCPFLG1
LCPSTYPE EQU   ISTLCPB+36
LCPFSNCH EQU   ISTLCPB+34
LCPFOSN  EQU   ISTLCPB+32
LCPTLBAD EQU   ISTLCPB+28
LCPFDB2  EQU   LCPFDBA+1
LCPFDB1  EQU   LCPFDBA
LCPDAF   EQU   LCPCID+2
LCPSAF   EQU   LCPCID
LCPLCCW1 EQU   ISTLCPB+12
LCPRRCNT EQU   LCPSEQ+1
LCPRRCCT EQU   LCPSEQ
LCPATOP  EQU   LCPFLAG2
LCPFREE  EQU   LCPFLAG2
LCPDSTAT EQU   LCPFLAG2
LCPEXRES EQU   LCPFID0
LCPZRIMM EQU   LCPTLBPI
LCPRSTRT EQU   LCPFLAG2
LCPASYN  EQU   LCPFLAG2
LCPPRFLG EQU   LCPFLAG2
LCPZMIRC EQU   LCPRUCNT
LCPCHAI2 EQU   LCPCHAIN+1
LCPCHNG  EQU   LCPCHAI1
LCPZRT0B EQU   LCPMNRCD
LCPZRT0A EQU   LCPMNRCD
LCPZRTL9 EQU   LCPZRT09
LCPZRTL8 EQU   LCPZRT08
LCPZRTL7 EQU   LCPZRT07
LCPZRTL6 EQU   LCPZRT06
LCPZRTL5 EQU   LCPZRT05
LCPZRTL4 EQU   LCPZRT04
LCPZRTL3 EQU   LCPZRT03
LCPZRTL2 EQU   LCPZRT02
LCPLNGTH EQU   ISTLCPB+1
LCPTYPE  EQU   ISTLCPB
LCCWDATA EQU   ISTLCCW+4
LCCWACI  EQU   LCCWFLAG
LCCWPER  EQU   LCCWLSI
LCCWIDAT EQU   LCCWFLAG
LCCWNTVC EQU   LCCWFLAG
LCCWCKPT EQU   LCCWFLAG
LCCWCC   EQU   LCCWFLAG
LCCWCD   EQU   LCCWFLAG
LCCWTYPE EQU   LCCWCTL
@NM00016 EQU   LCCWRWOP
RPHNEXPO EQU   ISTRPH+104
RPHSAV16 EQU   RPHWORK+60
RPHSAV15 EQU   RPHWORK+56
RPHSAV14 EQU   RPHWORK+52
RPHSAV13 EQU   RPHWORK+48
RPHSAV12 EQU   RPHWORK+44
RPHSAV11 EQU   RPHWORK+40
RPHSAV10 EQU   RPHWORK+36
RPHSAVE9 EQU   RPHWORK+32
RPHSAVE8 EQU   RPHWORK+28
RPHSAVE7 EQU   RPHWORK+24
RPHSAVE6 EQU   RPHWORK+20
RPHSAVE5 EQU   RPHWORK+16
RPHSAVE4 EQU   RPHWORK+12
RPHSAVE3 EQU   RPHWORK+8
RPHSAVE2 EQU   RPHWORK+4
RPHSHALF EQU   RPHSAVE1+2
RPHSBIT8 EQU   RPHSBITS
RPHSBIT7 EQU   RPHSBITS
RPHSBIT6 EQU   RPHSBITS
RPHSBIT5 EQU   RPHSBITS
RPHSBIT4 EQU   RPHSBITS
RPHSBIT3 EQU   RPHSBITS
RPHSBIT2 EQU   RPHSBITS
RPHSBIT1 EQU   RPHSBITS
RPHSBYTE EQU   RPHSAVE1
RPHPABQP EQU   RPHPABQA+1
RPHPABFG EQU   RPHPABQA
RPHSRP34 EQU   RPHSRPRM+2
RPHSRP12 EQU   RPHSRPRM
RPHWEGT  EQU   RPHCSPA
RPHMAJCB EQU   ISTRPH+21
RPHPABOF EQU   ISTRPH+20
RPHRESUM EQU   RPHRESMA+1
RPHRSV01 EQU   RPHWPFLG
RPHPURGE EQU   RPHWPFLG
RPHPT    EQU   RPHWPFLG
RPHWT    EQU   RPHWPFLG
RPHRSKEY EQU   RPHWPFLG
RPHDVTA  EQU   ISTRPH+12
RPHTIK   EQU   RPHTSKID+3
@NM00015 EQU   RPHTSKID
RPHRPHAP EQU   RPHRPHA+1
@NM00014 EQU   RPHRPHA
RPHGATE  EQU   RPHRPHA
@NM00013 EQU   RPHFLGB
RPHRLCRA EQU   RPHFLGB
RPHNRSAV EQU   RPHFLGB
RPHPGCMP EQU   RPHFLGB
RPHFBAPS EQU   RPHFLGB
RPHBSSP  EQU   RPHFLGB
RPHLOCK  EQU   RPHFLGB
RPHFNFLG EQU   RPHFSTLC
RPHMLTCP EQU   RPHFLAGS
RPHSMCLR EQU   RPHFLAGS
RPHSMTYP EQU   RPHFLAGS
RPHSMQ   EQU   RPHFLAGS
RPHAUTEX EQU   RPHAPTYP
RPHSPGIN EQU   RPHFLAGS
RPHOGIND EQU   RPHFLAGS
RPHLNGTH EQU   ISTRPH+1
RPHTYPE  EQU   ISTRPH
CCWSPRSV EQU   CCWSPARE
CCWCFTX  EQU   CCWSPARE
CCWTRCE  EQU   CCWSPARE
CCWNULL  EQU   CCWFLAGS
CCWPCI   EQU   CCWFLAGS
CCWSKIP  EQU   CCWFLAGS
CCWSLI   EQU   CCWFLAGS
CCWCC    EQU   CCWFLAGS
CCWCD    EQU   CCWFLAGS
ZLBZBFSC EQU   ISTZLBVT+88
ZLBZIFVB EQU   ISTZLBVT+84
ZLBZIFUB EQU   ISTZLBVT+80
ZLBZIFSB EQU   ISTZLBVT+76
ZLBRSV22 EQU   ISTZLBVT+72
ZLBZBFID EQU   ISTZLBVT+68
ZLBZBFX0 EQU   ISTZLBVT+64
ZLBRSV02 EQU   ISTZLBVT+60
ZLBRSV21 EQU   ISTZLBVT+56
ZLBZBFJB EQU   ISTZLBVT+52
ZLBZBFIB EQU   ISTZLBVT+48
ZLBZBFHB EQU   ISTZLBVT+44
ZLBZBFGB EQU   ISTZLBVT+40
ZLBZBFFB EQU   ISTZLBVT+36
ZLBZBFEB EQU   ISTZLBVT+32
ZLBZBFDB EQU   ISTZLBVT+28
ZLBZBFCB EQU   ISTZLBVT+24
ZLBZBFBB EQU   ISTZLBVT+20
ZLBZBFAB EQU   ISTZLBVT+16
ZLBZBFNB EQU   ISTZLBVT+12
ZLBZCFAI EQU   ISTZLBVT+8
ZLBZBF0B EQU   ISTZLBVT+4
ZLBZAF0B EQU   ISTZLBVT
ZFSZWRD4 EQU   ISTZFSVT+36
ZFSZWRD3 EQU   ISTZFSVT+32
ZFSZEFCB EQU   ISTZFSVT+28
ZFSZFFEB EQU   ISTZFSVT+24
ZFSZDFJ0 EQU   ISTZFSVT+20
ZFSZFFFB EQU   ISTZFSVT+16
ZFSZFFDB EQU   ISTZFSVT+12
ZFSZFFCB EQU   ISTZFSVT+8
ZFSZEMAB EQU   ZFSZEAAB
ZFSZEMBB EQU   ZFSZEABB
ZPSZDFQR EQU   ISTZPSVT+80
ZPSZDFPR EQU   ISTZPSVT+76
ZPSZDFXR EQU   ISTZPSVT+72
ZPSZIF2B EQU   ISTZPSVT+68
ZPSZAF2B EQU   ISTZPSVT+64
ZPSZDFMB EQU   ISTZPSVT+60
ZPSZDFMF EQU   ISTZPSVT+56
ZPSZDFNB EQU   ISTZPSVT+52
@NM00012 EQU   ISTZPSVT+48
ZPSZDFF0 EQU   ISTZPSVT+44
ZPSZTRTT EQU   ISTZPSVT+40
ZPSZBFOB EQU   ISTZPSVT+36
ZPSZBFMB EQU   ISTZPSVT+32
ZPSZBFZ0 EQU   ISTZPSVT+28
ZPSZDFI0 EQU   ISTZPSVT+20
ZPSZDFB0 EQU   ISTZPSVT+16
ZPSZDFH0 EQU   ISTZPSVT+12
ZPSZDFD0 EQU   ISTZPSVT+8
ZPSZDFA0 EQU   ISTZPSVT
.@UNREFD ANOP                          END UNREFERENCED COMPONENTS
@RC00566 EQU   @RC00565
@RF00599 EQU   @RC00590
@RC00649 EQU   @RC00648
WRCCW@   EQU   @PB00002
@ENDDATA EQU   *
         END   ISTZBFBA,(C'PLS1957',0603,75153)
/*
//*
//STEP3   EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
  IDENTIFY ISTZBFBA('ZP60008')
++ZAP(ISTZBF0L) DISTLIB(AOS24).
 NAME ISTZBF0L
 IDRDATA ZP60008
VER 082D 0000,0000,0000,0000   ** START OF TRANSLATE TABLE
VER 089C 11                    ** OLD ENTRY FOR HEX 6F
VER 091E 0D16                  ** OLD ENTRIES FOR HEX F1 AND F2
VER 0922 1D12,2D               ** OLD ENTRIES FOR HEX F5, F6 AND F7
REP 08AB DD                    ** NEW ENTRY FOR HEX 7E REPLACING A NULL
REP 0920 CD                    ** NEW ENTRY FOR HEX F3 REPLACING A NULL
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//STEP4   EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN DD  DSN=&&SMPMCS,DISP=(OLD,DELETE)
//SMPCNTL  DD  *
  RECEIVE
          SELECT(ZP60008)
          .
/*
//*
//STEP5CK EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60008)
        CHECK
        .
/*
//*
//STEP5   EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60008)
        DIS(WRITE)
        .
/*
//
