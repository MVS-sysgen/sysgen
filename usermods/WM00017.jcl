//WM00017  JOB (SYSGEN),'J05 M15: WM00017',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*********************************************************************
//* Install USERMOD WM00017 (source: SHARE?) adds 2 JES2 console      *
//* commands: $U and $DP                                              *
//*********************************************************************
//*
//RECV25   EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN DD  *
++USERMOD (WM00017) /* $DP COMMAND AND $U COMMAND */  .
++VER (Z038) FMID(EJE1103) PRE(UZ31176 UZ33158 UZ35334
       UZ37263 UZ54837 UZ57911 UZ63374 UZ65742 UZ71437
       UZ79531)
   /* SOURCE: JES2 MODS (SHARE?)
      TWO NEW COMMANDS HAVE BEEN ADDED TO JES2 TO ENHANCE OUTPUT
      PROCESSING.  THE TWO COMMANDS ARE:- $DP & $U.  THE FORMAT
      OF THE $DP COMMAND IS AS FOLLOWS:-
         $DP   ----  WILL DISPLAY ALL PRINTED OUTPUT.  IT WILL
                     GIVE BOTH THE NUMBER OF LINES AND THE
                     OUTPUT CLASS PLUS THE USUAL JES2 SPOOL
                     UTILIZATION MESSAGE.
         $DPX  ----  WHERE X IS THE SYSOUT CLASS TO BE DISPLAYED.
                     EG. $DPA WILL DISPLAY ALL OUTPUT FOR SYSOUT
                     CLASS=A.  IF THERE IS NO SYSOUT=A THEN THE
                     JES2 SPOOL UTILIZATION MESSAGE IS DISPLAYED.
                     THE $DPX FORMAT DOES NOT SUPPORT MULTIPLE
                     SYSOUT CLASSES.
      THE FORMAT OF THE $U COMMAND IS AS FOLLOWS:-
         $U JOBID,O= FROM CLASS,C= TO CLASS WHERE JOBID CAN BE
                     JOB/TSU/STC NUMBER OR RANGE OF NUMBERS EG
                     J10, S23-25, T51 OR JOBNAME IN QUOTES EG
                     'TSTJOB'.  FROM CLASS CAN BE ONE OR MORE
                     CLASSES EG A, ABV, ABCDEF OR * TO SIGNIFY
                     ALL CLASSES.  TO CLASS IS A SINGLE CLASS
                     SPECIFICATION EG C.
         EXAMPLES OF THE $U COMMAND:-
            $US1,C=P,O=L   WILL RESET 'L' CLASS OUTPUT FOR STARTED
                     TASK 1 TO CLASS 'P'.
            $UJ10,O=V,C=P  WILL RESET 'V' CLASS OUTPUT FOR JOB 10
                     TO CLASS = 'P'.
            $U'TESTJOB',O=2A,C=5  WILL RESET '2' CLASS & 'A' CLASS
                     OUTPUT FOR TESTJOB TO CLASS = '5'.
            $UJ1-999,C=2,O=*  WILL RESET OUTPUT FOR ALL JOBS TO
                     CLASS = '2'.
      POINTS TO NOTE:-
         THE OPERANDS 'O' & 'C' MAY BE IN ANY ORDER.
         OUTPUT CURRENTLY BEING PRINTED CANNOT BE RESET.
         RESETTING OUTPUT TO THE 'Z' QUEUE RE-QUEUES OUTPUT TO A
         'Z' QUEUE BUT DOES NOT AUTOMATICALLY DELETE.
         ('Z' CLASS BEING SYSOUT CLASS NOT PRINTED)
      THE RESPONSES TO THE $U COMMAND WILL BE:-
         $HASP000 SYSOUT CLASS/ES CHANGED
                       OR
         $HASP000 NO OUTPUT FOUND                        */ .
++SRCUPD (HASPCOMM) DISTLIB (HASPSRC).
./       CHANGE NAME=HASPCOMM
*        TO BE INSERTED BETWEEN $DO AND $DQ ENTRIES  ***********   BNSW K1689600
         $COMTAB DP,BNSWCMD        $DP DISPLAY OUTPUT QUEUE(S) *   BNSW K1689700
*                                                    ***********   BNSW K1689800
COMTBLU  $COMTAB U7D,BNSWCMD,REJECT=COMR+COMJ  $U'JOBNAME'     *   BNSW K1720600
         $COMTAB UJ,BNSWCMD,REJECT=COMR+COMJ   $UJ   * CHANGE  *   BNSW K1720602
         $COMTAB US,BNSWCMD,REJECT=COMR+COMJ   $US   * SYSOUT  *   BNSW K1720604
         $COMTAB UT,BNSWCMD,REJECT=COMR+COMJ   $UT   * CLASS.  *   BNSW K1720606
****************************************************************   BNSW K1720608
******** ENTRY FOR 'U' COMMANDS    *****************************   BNSW K1733250
         DC    C'U',AL3(COMTBLU)   $U - CHANGE OUTPUT CLASS    *   BNSW K1733252
****************************************************************   BNSW K1733254
 TITLE '   HASP COMMAND PROCESSOR BNSWCMD - $DP  *** BNSW ***'     BNSW K2577550
         PUSH  USING                                               BNSW K2577551
BNSWCMD  $COMGRUP DP,U7D,UJ,US,UT,DELAY=NO  ADDED COMMANDS         BNSW K2577552
CDP      EQU   *                   DISPLAY OUTPUT QUEUES (FOR A    BNSW K2577553
*                                  PARTICULAR CLASS IF REQUESTED)  BNSW K2577554
         SPACE 2                                                   BNSW K2577555
****************************************************************** BNSW K2577556
*                                                                * BNSW K2577557
*        $DP   OR   $DPX    X=CLASS TO BE DISPLAYED, DEFAULT ALL * BNSW K2577558
*                                                                * BNSW K2577559
****************************************************************** BNSW K2577560
         SPACE 2                                                   BNSW K2577561
         USING JOEDSECT,R1                                         BNSW K2577562
         SPACE 1                                                   BNSW K2577563
         L     R1,0(,WD)           A(1ST OPERAND) I.E. 'P'         BNSW K2577564
         SR    WB,WB               CLASS INDEX - 0=ALL             BNSW K2577565
         CLI   1(R1),C' '          IF NO CLASS SPECIFIED           BNSW K2577566
         BE    CDPALLC             THEN DISPLAY ALL CLASSES        BNSW K2577567
*                                  ELSE CHECK VALIDITY OF CLASS:   BNSW K2577568
         LA    R15,L'CDPCLSES      NUMBER OF VALID CLASSES         BNSW K2577569
CDPFNCLS LA    WB,1(,WB)           1=A , 2=B , ...                 BNSW K2577570
         IC    WC,CDPCLSES-1(WB)   CLASS FROM LIST                 BNSW K2577571
         CLM   WC,1,1(R1)          IF THIS IS THE CLASS            BNSW K2577572
         BE    CDPHAVCL            THEN GO FIND JOE'S              BNSW K2577573
         BCT   R15,CDPFNCLS        ELSE CHECK AGAINST NEXT IN LIST BNSW K2577574
         SPACE 1                                                   BNSW K2577575
         $CFINVO OPERAND=(R1)      MSG 'INVALID OPERAND' + GET OUT BNSW K2577576
         SPACE 1                                                   BNSW K2577577
CDPCLSES DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789' CLASS LIST  BNSW K2577578
         SPACE 1                                                   BNSW K2577579
CDPHAVCL DS    0H                  RUN THRU CLASS QUEUE IN JOT     BNSW K2577580
         BAL   WC,CDPPRC           PROCESS SPECIFIED CLASS         BNSW K2577581
         B     CDPEND              AND GET OUT WITH FINAL MSG.     BNSW K2577582
         SPACE 2                                                   BNSW K2577583
CDPALLC  LA    WB,1(,WB)           1ST/NEXT CLASS TO PROCESS       BNSW K2577584
         BAL   WC,CDPPRC           PROCESS A CLASS                 BNSW K2577585
         LA    WC,L'CDPCLSES       NUMBER OF CLASES                BNSW K2577586
         CR    WB,WC               IF NOT ON LAST ONE              BNSW K2577587
         BNE   CDPALLC             THEN DO NEXT CLASS              BNSW K2577588
         SPACE 1                                                   BNSW K2577589
CDPEND   EQU   *                   ALL DONE - SETUP FINAL MSG AND  BNSW K2577590
*                                  GET OUT. CODE HERE IS EXACTLY   BNSW K2577591
*                                  THE SAME AS AT 'CDQEND'         BNSW K2577592
         L     R15,$SSVT           A(SSVT)                         BNSW K2577593
         L     R1,$SVTGALC-SSVT(,R15) GROUPS ALLOCATED             BNSW K2577594
         M     R0,=F'100'          CALCULATE                       BNSW K2577595
         D     R0,$SVTGTOT-SSVT(,R15) PERCENTAGE                   BNSW K2577596
         $CFCVE VALUE=(R1)         MAKE PRINTABLE                  BNSW K2577597
         MVC   COMMAND(3),COMDWORK+2 INTO MSG                      BNSW K2577598
         MVC   COMMAND+3(26),=C' PERCENT SPOOL UTILIZATION'        BNSW K2577599
         $CRET L=29                GET OUT AND ISSUE MSG           BNSW K2577600
         SPACE 3                                                   BNSW K2577601
CDPPRC   DS    0H                  PROCESS CLASS INDICATED BY WB   BNSW K2577602
         ST    WC,COMWREGS+12      KEEP RETURN ADDR                BNSW K2577603
         IC    R1,CDPCLSES-1(WB)   GET CHAR FOR CLASS              BNSW K2577604
         STC   R1,COMWREGS+4       AND KEEP IT                     BNSW K2577605
         LA    R1,0(WB,WB)         2,4,6,...                       BNSW K2577606
         LA    R1,(JOTCLSQ-JOTDSECT)-(JOENEXT-JOEDSECT+2)(R1) DISP BNSW K2577607
         AL    R1,$JOTABLE         ACTUAL ADDR TO START            BNSW K2577608
         ST    R1,COMWREGS         KEEP STARTING POINT FOR THIS Q  BNSW K2577609
         SPACE 1                                                   BNSW K2577610
CDPNXTJO LH    R1,JOENEXT          1ST/NEXT JOE OFFSET/4           BNSW K2577611
         N     R1,=X'0000FFFF'     ENSURE +                        BNSW K2577612
         BZ    CDPPRCR             GET OUT IF NO MORE JOE'S        BNSW K2577613
         SLL   R1,2                *4 FOR ACTUAL OFFSET            BNSW K2577614
         AL    R1,$JOTABLE         ADD ADDR OF JOT                 BNSW K2577615
         ST    R1,COMWREGS+8       KEEP ADDR OF CURRENT JOE        BNSW K2577616
         LH    R14,JOEJQE          JQE OFFSET/4                    BNSW K2577617
         N     R14,=X'0000FFFF'    ENSURE +                        BNSW K2577618
         SLL   R14,2               *4 FOR ACTUAL OFFSET            BNSW K2577619
         AL    R14,$JOBQPTR        A(JQE)                          BNSW K2577620
         SPACE 1                                                   BNSW K2577621
*        BUILD MSG AND ISSUE $CWTO                                 BNSW K2577622
         SPACE 1                                                   BNSW K2577623
         BAL   WC,CDPIJOB          IDENTIFY JOB                    BNSW K2577624
         BAL   WC,CDPICLAS         IDENTIFY OUTPUT CLASS           BNSW K2577625
         BAL   WC,CDPILNS          NUMBER OF LINES                 BNSW K2577626
         SPACE 1                                                   BNSW K2577627
         $CWTO L=CDPOLEN           ISSUE MSG                       BNSW K2577628
         SPACE 1                                                   BNSW K2577629
*        MAKE SURE THAT THE JOE THAT WE WERE WORKING WITH IS STILL BNSW K2577630
*        ON THE QUEUE.                                             BNSW K2577631
         L     R1,COMWREGS         GET START POINT FOR THE QUEUE   BNSW K2577632
CDPNXTJX LH    R1,JOENEXT          1ST/NEXT JOE                    BNSW K2577633
         N     R1,=X'0000FFFF'     ENSURE +                        BNSW K2577634
         BZ    CDPINCM             NOT FOUND - MSG 'LIST INCOMPLETE' SW K2577635
         SLL   R1,2                *4 FOR ACTUAL OFFSET            BNSW K2577636
         AL    R1,$JOTABLE         A(JOE)                          BNSW K2577637
         C     R1,COMWREGS+8       IF THIS IS THE ONE WE WERE WORKING W K2577638
         BNE   CDPNXTJX            WITH THEN CONTINUE AS NORMAL ELSE SW K2577639
*                                  TRY NEXT JOE ON THE QUEUE.      BNSW K2577640
         B     CDPNXTJO            JOE FOUND - CONTINUE            BNSW K2577641
         SPACE 2                                                   BNSW K2577642
CDPINCM  MVC   COMMAND(L'CDPCLIC),CDPCLIC 'LIST INCOMPLETE'        BNSW K2577643
         MVC   COMMAND+L'CDPCLIC(1),COMWREGS+4 CLASS               BNSW K2577644
         $CWTO L=L'CDPCLIC+1       ISSUE MSG                       BNSW K2577645
         SPACE 1                                                   BNSW K2577646
CDPPRCR  L     WC,COMWREGS+12      RESTORE RETURN ADDR             BNSW K2577647
         BR    WC                                                  BNSW K2577648
         SPACE 3                                                   BNSW K2577649
CDPIJOB  DS    0H                  PUT JOB IDENTITY IN MSG         BNSW K2577650
         MVC   CDPOJNAM,JQEJNAME-JQE(R14) MOVE IN JOBNAME          BNSW K2577651
         LH    R0,JQEJOBNO-JQE(R14) JOB NUMBER                     BNSW K2577652
         MVC   CDPOJID,CDPCJID     ASSUME 'JOB'                    BNSW K2577653
         CH    R0,=H'10000'        IS IT JOB                       BNSW K2577654
         BL    CDPCONJN            IF SO GO CONVERT NUMBER         BNSW K2577655
         MVC   CDPOJID,CDPCTID     TRY FOR 'TSU'                   BNSW K2577656
         SH    R0,=H'20000'        SUBTRACT TSUS BASE              BNSW K2577657
         BNM   CDPCONJN            GO CONVERT IF IT IS TSU         BNSW K2577658
         MVC   CDPOJID,CDPCSID     MUST BE STC                     BNSW K2577659
         AH    R0,=H'10000'        MAKE STC NUMBER +VE             BNSW K2577660
CDPCONJN CVD   R0,COMDWORK                                         BNSW K2577661
         MVC   CDPOJNUM,CDPCJPAT   EDIT PATTERN                    BNSW K2577662
         ED    CDPOJNUM,COMDWORK+(8-L'CDPOJNUM/2)                  BNSW K2577663
         MVC   CDPOJNID,CDPCJNID   '#'                             BNSW K2577664
         BR    WC                                                  BNSW K2577665
         SPACE 2                                                   BNSW K2577666
CDPICLAS DS    0H                  PUT OUTPUT CLASS INTO MSG       BNSW K2577667
         MVC   CDPOCLAS,COMWREGS+4 A,B,C,...                       BNSW K2577668
         MVC   CDPOCLID,CDPCCLID   'CLASS'                         BNSW K2577669
         BR    WC                                                  BNSW K2577670
         SPACE 2                                                   BNSW K2577671
CDPILNS  DS    0H                  NUMBER OF LINES AND INDICATION  BNSW K2577672
         MVC   CDPOPRIN,CDPCBLNK   IF THE JOB IS CURRENTLY PRINTING NSW K2577673
         L     R0,JOERECCT         NUMBER OF LINES                 BNSW K2577674
         TM    JOEFLAG,$JOEPRT     IF JOB IS NOT PRINTING          BNSW K2577675
         BNO   CDPNOTPR            THEN DON'T LOOK FOR CHKPT JOE   BNSW K2577676
         SPACE 1                                                   BNSW K2577677
         MVC   CDPOPRIN,CDPCPRIN   'PRINTING' TO MSG               BNSW K2577678
*        JOB PRINTING, SUBTRACT NUMBER OF LINES PRINTED FROM TOTAL. NSW K2577679
         TM    JOEFLAG,$JOECKV     IF CHKPT NOT VALID              BNSW K2577680
         BNO   CDPNOTPR            THEN DON'T LOOK AT IT           BNSW K2577681
         LH    R1,JOECKPT          OFFSET/4 FOR CHKPT JOE          BNSW K2577682
         N     R1,=X'0000FFFF'     ENSURE +                        BNSW K2577683
         SLA   R1,2                *4 FOR ACTUAL OFFSET            BNSW K2577684
         BZ    CDPNOTPR            SKIP IT IF NOT AVAILABLE        BNSW K2577685
         AL    R1,$JOTABLE         A(CHKPT JOE)                    BNSW K2577686
         S     R0,JOETLNC          TOTAL-(NO. PRINTED)             BNSW K2577687
         L     R1,COMWREGS+8       RESTORE A(WORK JOE)             BNSW K2577688
         SPACE 1                                                   BNSW K2577689
CDPNOTPR CVD   R0,COMDWORK         NUMBER OF LINES                 BNSW K2577690
         MVC   CDPONLIN,CDPCLPAT   EDIT PATTERN                    BNSW K2577691
         ED    CDPONLIN,COMDWORK+(8-L'CDPONLIN/2)                  BNSW K2577692
         MVC   CDPOLINS,CDPCLINS   'LINES'                         BNSW K2577693
         BR    WC                                                  BNSW K2577694
         SPACE 3                                                   BNSW K2577695
*        FIELD 'COMWREGS' USED AS FOLLOWS:                         BNSW K2577696
*                                                                  BNSW K2577697
*              COMWREGS+0(4)  OFFSET OF START OF CURRENT WORK JOE Q NSW K2577698
*              COMWREGS+4(1)  CLASS CURRENTLY BEING PROCESSED      BNSW K2577699
*              COMWREGS+8(4)  ADDR OF CURRENT WORK JOE             BNSW K2577700
*              COMWREGS+12(4) RETURN ADDR FROM 'CDPPRC'            BNSW K2577701
*                                                                  BNSW K2577702
         SPACE 1                                                   BNSW K2577703
CDPCJID  DC    C' JOB '            IDENTIFY 'JOB' IN MSG           BNSW K2577704
CDPCTID  DC    C' TSU '            IDENTIFY 'TSU' IN MSG           BNSW K2577705
CDPCSID  DC    C' STC '            IDENTIFY 'STC' IN MSG           BNSW K2577706
CDPCJNID DC    C'  #'              IDENTIFY JOB NUMBER             BNSW K2577707
CDPCJPAT DC    X'602020202021'     JOB NUMBER PATTERN              BNSW K2577708
CDPCCLID DC    C'  CLASS '         IDENTIFY CLASS IN MSG           BNSW K2577709
CDPCLINS DC    C'     LINES'                                       BNSW K2577710
CDPCLPAT DC    X'4020202020202021' NUMBER OF LINES PATTERN         BNSW K2577711
CDPCPRIN DC    C'  PRINTING'       IF JOB IS PRINTING              BNSW K2577712
CDPCBLNK DC    CL(L'CDPCPRIN)' '   IF JOB NOT PRINTING             BNSW K2577713
CDPCLIC  DC    C'LIST INCOMPLETE FOR CLASS '                       BNSW K2577714
         SPACE 2                                                   BNSW K2577715
*        EQUATES FOR OUTPUT LINE. THESE TOGETHER WITH THE ABOVE    BNSW K2577716
*        CONSTANTS DETERMINE THE FORMAT OF THE MESSAGE.            BNSW K2577717
         SPACE 1                                                   BNSW K2577718
CDPOJID  EQU   COMMAND,L'CDPCJID               'JOB','TSU','STC'   BNSW K2577719
CDPOJNAM EQU   CDPOJID+L'CDPOJID,8             JOBNAME             BNSW K2577720
CDPOJNID EQU   CDPOJNAM+L'CDPOJNAM,L'CDPCJNID  '#'                 BNSW K2577721
CDPOJNUM EQU   CDPOJNID+L'CDPOJNID-1,L'CDPCJPAT JOB NUMBER         BNSW K2577722
CDPOCLID EQU   CDPOJNUM+L'CDPOJNUM,L'CDPCCLID  'CLASS'             BNSW K2577723
CDPOCLAS EQU   CDPOCLID+L'CDPOCLID,1           CLASS BYTE          BNSW K2577724
CDPOLINS EQU   CDPOCLAS+L'CDPOCLAS,L'CDPCLINS  'LINES'             BNSW K2577725
CDPONLIN EQU   CDPOLINS+L'CDPOLINS-1,L'CDPCLPAT NUMBER OF LINES    BNSW K2577726
CDPOPRIN EQU   CDPONLIN+L'CDPONLIN,L'CDPCPRIN  'PRINTING'          BNSW K2577727
CDPOLEN  EQU   CDPOPRIN+L'CDPOPRIN-COMMAND     LENGTH OF MSG       BNSW K2577728
 TITLE '   HASP COMMAND PROCESSOR BNSWCMD - $U   *** BNSW ***'     BNSW K2577729
         EJECT                                                     BNSW K2577730
****************************************************************** BNSW K2577731
*                                                                * BNSW K2577732
*        ROUTINES FOR $U COMMAND:                                * BNSW K2577733
*                                                                * BNSW K2577734
*        EG'S  $U'JOBNAME',O=V,C=Z     (FROM CLASS V TO CLASS Z) * BNSW K2577735
*              $UJ101,O=*,C=A          (ALL CLASSES TO CLASS A)  * BNSW K2577736
*              $UT15-16,O=AB5,C=V      (CLASSES A,B,5 TO CLASS V)* BNSW K2577737
*                                                                * BNSW K2577738
****************************************************************** BNSW K2577739
         SPACE 2                                                   BNSW K2577740
CU7D     DS    0H                  $U'JOBNAME'                     BNSW K2577741
         L     R1,0(,WD)           A(OPERAND) IE 1ST "'"           BNSW K2577742
         L     WC,4(,WD)           END OF OPERAND +2               BNSW K2577743
         BCTR  WC,0                BACK TO END                     BNSW K2577744
         BCTR  WC,0                OF OPERAND                      BNSW K2577745
         CLI   0(WC),C''''         ENDING ' IS OPTIONAL            BNSW K2577746
         BNE   CUNOSUB             IF NOT ' LAST BYTE PART OF NAME BNSW K2577747
         BCTR  WC,0                END OF NAME                     BNSW K2577748
CUNOSUB  SR    WC,R1               LENGTH OF NAME                  BNSW K2577749
         BNP   CUJINVO             NO NAME    - GET OUT            BNSW K2577750
         BCTR  WC,0                LENGTH FOR EX                   BNSW K2577751
         LA    R0,7                MAX LEN                         BNSW K2577752
         CR    WC,R0               TRUNCATE                        BNSW K2577753
         BNH   CUOKL               IF                              BNSW K2577754
         LR    WC,R0               TOO LONG                        BNSW K2577755
CUOKL    MVC   COMJNAME,=CL8' '    INIT JOBNAME                    BNSW K2577756
         EX    WC,CUMVC                                            BNSW K2577757
         SPACE 1                                                   BNSW K2577758
*        LOOK FOR JOB IN JOB QUEUE.                                BNSW K2577759
         USING JQEDSECT,R1                                         BNSW K2577760
         $CFJSCAN PROCESS=CUPROC,NEXT=CUNEXTJ                      BNSW K2577761
         MVC   COMMAND(8),COMJNAME JOB NOT FOUND -                 BNSW K2577762
         MVC   COMMAND+8(14),=C' JOB NOT FOUND' SETUP MSG          BNSW K2577763
         $CRET L=22                AND GET OUT                     BNSW K2577764
         SPACE 1                                                   BNSW K2577765
CUMVC    MVC   COMJNAME(1),1(R1)   TO MOVE JOBNAME FROM COMMAND    BNSW K2577766
         SPACE 1                                                   BNSW K2577767
CUPROC   CLC   COMJNAME,JQEJNAME   IS THIS THE REQD JOB            BNSW K2577768
         BNE   CUNEXTJ             GET NEXT IF NO MATCH            BNSW K2577769
         LH    WC,JQEJOBNO         GET JOB NUMBER                  BNSW K2577770
         STH   WC,CUJWJLO          LO JOB NO                       BNSW K2577771
         STH   WC,CUJWJHI          SAME AS HI JOB NO               BNSW K2577772
         B     CUJHAVJ             PROCESS OTHER OPERANDS          BNSW K2577773
         SPACE 2                                                   BNSW K2577774
         USING JOEDSECT,R1                                         BNSW K2577775
CUS      DS    0H                  $US                             BNSW K2577776
         LH    WA,=H'10000'        STCS LO END                     BNSW K2577777
         B     CUJA                COMMON                          BNSW K2577778
         SPACE 2                                                   BNSW K2577779
CUT      DS    0H                  $UT                             BNSW K2577780
         LH    WA,=H'20000'        TSUS LO END                     BNSW K2577781
         B     CUJA                COMMON                          BNSW K2577782
         SPACE 2                                                   BNSW K2577783
CUJ      DS    0H                  $UJ                             BNSW K2577784
         SR    WA,WA               JOBS LO END                     BNSW K2577785
         SPACE 1                                                   BNSW K2577786
CUJA     $CFCVB POINTER=(WD),NOK=CUJINVO GET JOB NUBER RANGE       BNSW K2577787
         LTR   R0,R0               IF 'HI' JOB NUMBER NOT +VE      BNSW K2577788
         BNP   CUJINVO             THEN GET OUT WITH ERROR MSG     BNSW K2577789
         AR    R0,WA               HI JOB NUMBER                   BNSW K2577790
         STH   R0,CUJWJHI          KEEP HI JOB NUMBER              BNSW K2577791
         AR    R1,WA               LO JOB NUMBER                   BNSW K2577792
         STH   R1,CUJWJLO          KEEP LO JOB NUMBER              BNSW K2577793
CUJHAVJ  MVI   CUJWFLG,0           INIT FLAG                       BNSW K2577794
         SPACE  1                                                  BNSW K2577795
         CLR   WD,WF               IF NO MORE OPERANDS             BNSW K2577796
         BNL   CUJMISS             THEN ISSUE MISSING OP MSG       BNSW K2577797
CUJLOOP  BXH   WD,WE,CUJEND        LOOK AT NEXT OP                 BNSW K2577798
         L     R1,0(,WD)           A(OPERAND)                      BNSW K2577799
         CLI   1(R1),C'='          C=  OR  O=                      BNSW K2577800
         BNE   CUJINVO             GET OUT IF '=' NOT THERE        BNSW K2577801
         $CFSEL (C,CUJCLAS),(O,CUJOUT),OPERAND=(R1) B DEP ON C OR O NSW K2577802
         SPACE 1                                                   BNSW K2577803
CUJINVO  L     R1,0(,WD)           INVALID OPERAND POINTER         BNSW K2577804
         $CFINVO OPERAND=(R1)      GET OUT WITH MSG                BNSW K2577805
         SPACE 1                                                   BNSW K2577806
CUJMISS  $CRET MSG='OPERANDS MISSING FOR $U' GET OUT WITH MSG      BNSW K2577807
         SPACE 2                                                   BNSW K2577808
*        O=CLASSES  OR  O=* FOR ALL CLASSES  EG O=ABV              BNSW K2577809
CUJOUT   OI    CUJWFLG,CUJEOUT     O=  OPERAND FOUND               BNSW K2577810
         XC    CUJWOCLS,CUJWOCLS   CLEAR REQUIRED CLASES           BNSW K2577811
         L     R15,4(,WD)          NEXT OR NULL OPERAND            BNSW K2577812
         BCTR  R15,0               BACK TO ','                     BNSW K2577813
         LA    R1,2(,R1)           1ST OUT CLASS                   BNSW K2577814
         CLI   0(R1),C'*'          IF NOT 'ALL CLASSES'            BNSW K2577815
         BNE   CUJSCLS             THEN GO SCAN LIST               BNSW K2577816
         MVC   CUJWOCLS,CDPCLSES   ELSE MOVE IN LIST OF ALL CLASES BNSW K2577817
         B     CUJLOOP             AND GO GET NEXT OP              BNSW K2577818
         SPACE  1                                                  BNSW K2577819
CUJSCLS  CR    R1,R15              IF AT END OF THIS OPERAND       BNSW K2577820
         BNL   CUJLOOP             THEN GO GET NEXT                BNSW K2577821
         LA    R14,L'CDPCLSES      NUMBER OF VALID CLASSES         BNSW K2577822
CUJFCLSO IC    R0,CDPCLSES-1(R14)  PICK UP CLASS (WORKING BACWARDS) NSW K2577823
         CLM   R0,1,0(R1)          IF CLASS MATCHES                BNSW K2577824
         BE    CUJHCLSO            GO PUT IT IN LIST               BNSW K2577825
         BCT   R14,CUJFCLSO        ELSE TRY NEXT VALID CLASS       BNSW K2577826
         B     CUJINVO             CLASS NOT VALID - ISSUE MSG     BNSW K2577827
         SPACE 1                                                   BNSW K2577828
CUJHCLSO STC   R0,CUJWOCLS-1(R14)  CLASS REQUIRED INTO LIST        BNSW K2577829
         LA    R1,1(,R1)           NEXT IN COMMAND                 BNSW K2577830
         B     CUJSCLS             CHECK FOR MORE                  BNSW K2577831
         SPACE 2                                                   BNSW K2577832
CUJCLAS  OI    CUJWFLG,CUJECLAS    'TO' CLASS OPERAND              BNSW K2577833
         LA    R14,L'CDPCLSES      NUMBER OF VALID CLASSES         BNSW K2577834
CUJFCLSN IC    R0,CDPCLSES-1(R14)  GET A VALID CLASS               BNSW K2577835
         CLM   R0,1,2(R1)          IF CLASS IN LIST                BNSW K2577836
         BE    CUJHCLSN            GO STORE IT                     BNSW K2577837
         BCT   R14,CUJFCLSN        ELSE TRY NEXT                   BNSW K2577838
         B     CUJINVO             INVALID 'TO' CLASS              BNSW K2577839
         SPACE 1                                                   BNSW K2577840
CUJHCLSN STC   R0,CUJWNCLS         STORE NEW CLASS                 BNSW K2577841
         BCTR  R14,0               CLASS NO A=0,B=1,...            BNSW K2577842
         STH   R14,CUJWNCLN        USE LATER TO FIND CLASS Q IN JOT NSW K2577843
         B     CUJLOOP             GET NEXT OPERAND                BNSW K2577844
         SPACE 2                                                   BNSW K2577845
CUJEND   DS    0H                  ALL OPERANDS SCANNED            BNSW K2577846
         TM    CUJWFLG,CUJECLAS+CUJEOUT C=  AND  O=  BOTH REQD     BNSW K2577847
         BNO   CUJMISS             IF NOT BOTH THERE ISSUE MSG     BNSW K2577848
         LH    R14,CUJWNCLN        NEW CLASS NO. 0,1,2,...         BNSW K2577849
         SR    R0,R0               REMOVE ANY REQUEST TO CHANGE    BNSW K2577850
         STC   R0,CUJWOCLS(R14)    CLASS TO WHAT IT WAS (EG C=A,O=A) SW K2577851
         SPACE 2                                                   BNSW K2577852
CUJGETQ  $QSUSE ,                  ENQUEUE                         BNSW K2577853
         SPACE 1                                                   BNSW K2577854
*        FIND ANY OUTPUT WHICH MATCHES THAT SPECIFIED IN THE       BNSW K2577855
*        COMMAND.                                                  BNSW K2577856
*        SCAN JOT CLASS QUEUES FOR THE REQUIRED CLASSES AND CHECK  BNSW K2577857
*        IF THE JOES BELONG TO A REQUESTED JOB.                    BNSW K2577858
         SPACE 1                                                   BNSW K2577859
         SR    WA,WA               1ST CLASS NUMBER                BNSW K2577860
CUJSCNJ  SR    R1,R1                                               BNSW K2577861
         IC    R1,CUJWOCLS(WA)     GET CLASS OR 0 IF THAT CLASS    BNSW K2577862
         LTR   R1,R1               IS NOT REQUIRED.                BNSW K2577863
         BZ    CUJENDCL            GET NEXT CLASS IF NOT REQUIRED. BNSW K2577864
         STC   R1,CUJWCURC         KEEP CURRENT CLASS              BNSW K2577865
         LA    R1,(JOTCLSQ-JOTDSECT)-(JOENEXT-JOEDSECT)(WA,WA) DSP BNSW K2577866
         AL    R1,$JOTABLE         ADDR OF JOT CLASS QUEUE HEAD    BNSW K2577867
         LH    WB,JOENEXT          1ST JOE OFFSET/4                BNSW K2577868
CUJNXTJO LR    WC,R1               KEEP A(PREV JOE) (OR CLS Q HEAD) NSW K2577869
CUJNXTJX LR    R1,WB               NEXT/1ST JOE                    BNSW K2577870
         N     R1,=X'0000FFFF'     ENSURE +VE                      BNSW K2577871
         BZ    CUJENDCL            GET OUT IF NO MORE              BNSW K2577872
         SLL   R1,2                *4 FOR ACTUAL OFFSET            BNSW K2577873
         AL    R1,$JOTABLE         A(JOE)                          BNSW K2577874
         LH    WB,JOENEXT          OFFSET FOR NEXT JOE FOR THIS CLS NSW K2577875
         TM    JOEFLAG,$JOEPRT     IF PRINTING                     BNSW K2577876
         BO    CUJNXTJO            THEN DONT TOUCH IT              BNSW K2577877
         SPACE 1                                                   BNSW K2577878
*        WE HAVE FOUND OUTPUT OF A MATCHING CLASS, NOW CHECK JOB   BNSW K2577879
         LH    R14,JOEJQE          JQE OFFSET/4                    BNSW K2577880
         N     R14,=X'0000FFFF'    ENSURE +                        BNSW K2577881
         SLL   R14,2               ACTUAL OFFSET                   BNSW K2577882
         AL    R14,$JOBQPTR        A(JQE)                          BNSW K2577883
         LH    R15,JQEJOBNO-JQEDSECT(R14) GET JOB NUMBER AND       BNSW K2577884
         CH    R15,CUJWJLO         CHECK                           BNSW K2577885
         BL    CUJNXTJO            RANGE                           BNSW K2577886
         CH    R15,CUJWJHI         OF                              BNSW K2577887
         BH    CUJNXTJO            JOBNUMBERS                      BNSW K2577888
         SPACE 2                                                   BNSW K2577889
*        DO ACTUAL SWAP.                                           BNSW K2577890
         OI    CUJWFLG,CUJEDONE    INDICATE JOT UPDATED            BNSW K2577891
         SPACE 1                                                   BNSW K2577892
*        WC HAS ADDR OF PREV JOE OR CLASS QUEUE HEADER             BNSW K2577893
*        WB HAS NEXT JOE OFFSET                                    BNSW K2577894
         STH   WB,JOENEXT-JOEDSECT(WC) TAKE JOE OFF 'FROM' QUEUE   BNSW K2577895
         LH    R14,CUJWNCLN        NEW ('TO') CLASS NO   0,1,2,... BNSW K2577896
         LA    R14,JOTCLSQ-JOTDSECT(R14,R14) DISP OF CLASS Q       BNSW K2577897
         AL    R14,$JOTABLE        A(CLASS Q HEADER)               BNSW K2577898
         MVC   JOENEXT,0(R14)      HOOK REST OF Q ONTO THIS JOE    BNSW K2577899
         MVC   JOECURCL,CUJWNCLS   NEW CLASS TO JOE                BNSW K2577900
         SL    R1,$JOTABLE         JOE OFFSET                      BNSW K2577901
         SRL   R1,2                /4                              BNSW K2577902
         STH   R1,0(R14)           JOE ONTO 'TO' Q                 BNSW K2577903
         SLL   R1,2                *4 OFFSET AGAIN                 BNSW K2577904
         $#CKPT JOE=(R1),TYPE=D    REQUEST CHKPT OF CURRENT JOE    BNSW K2577905
         $#CKPT JOE=(WC),TYPE=A    REQ CHKPT OF PREV JOE OR Q HEADER SW K2577906
         B     CUJNXTJX            GET NEXT - PREVIOUS REMAINS SAME NSW K2577907
         SPACE 2                                                   BNSW K2577908
CUJENDCL LA    WA,1(,WA)           NEXT CLASS NUMBER               BNSW K2577909
         LA    R1,L'CUJWOCLS       NUMBER OF CLASSES               BNSW K2577910
         CR    WA,R1               IF NOT AT END                   BNSW K2577911
         BNE   CUJSCNJ             DO NEXT CLASS                   BNSW K2577912
         SPACE 1                                                   BNSW K2577913
*        AT END OF SCAN CHECK IF JOT HAS BEEN UPDATED              BNSW K2577914
         TM    CUJWFLG,CUJEDONE    IF JOT NOT UPDATED              BNSW K2577915
         BNO   CUJNOCH             ISSUE MSG + GET OUT             BNSW K2577916
         SPACE 1                                                   BNSW K2577917
         LH    R1,CUJWNCLN         'TO' CLASS NO. 0,1,2...         BNSW K2577918
         LA    R1,JOTCLSQ-JOTDSECT(R1,R1) DISP OF Q HEADER         BNSW K2577919
         $#CKPT JOE=(R1),TYPE=D    REQ CHPKT OF 'TO' Q HEADER      BNSW K2577920
         $POST $HASPECF,(JOT,CKPW) POST JOT AND REQUEST CHKPT WRITE NSW K2577921
         SPACE 1                                                   BNSW K2577922
         $CRET MSG='SYSOUT CLASS/ES CHANGED' ISSUE MSG + RETURN    BNSW K2577923
         SPACE 1                                                   BNSW K2577924
CUJNOCH  $CRET MSG='NO OUTPUT FOUND' RETURN WITH MSG               BNSW K2577925
         SPACE 2                                                   BNSW K2577926
****************************************************************** BNSW K2577927
*                                                                  BNSW K2577928
*        END OF 'COMMAND' USED AS WORK AREA                        BNSW K2577929
*                                                                  BNSW K2577930
CUJWNCLN EQU   COMMAND+80,2               NEW ('TO') CLASS NUMBER  BNSW K2577931
CUJWJLO  EQU   CUJWNCLN+L'CUJWNCLN,2      LO JOB NO.               BNSW K2577932
CUJWJHI  EQU   CUJWJLO+L'CUJWJLO,2        HI JOB NO.               BNSW K2577933
CUJWCURC EQU   CUJWJHI+L'CUJWJHI,1        CURRENT CLASS            BNSW K2577934
CUJWNCLS EQU   CUJWCURC+L'CUJWCURC,1      NEW ('TO') CLASS         BNSW K2577935
CUJWOCLS EQU   CUJWNCLS+L'CUJWNCLS,L'CDPCLSES OLD ('FROM') CLASSES BNSW K2577936
CUJWFLG  EQU   CUJWOCLS+L'CUJWOCLS,1      FLAG BYTE:               BNSW K2577937
CUJECLAS EQU   X'80'                      C= FOUND                 BNSW K2577938
CUJEOUT  EQU   X'40'                      O= FOUND                 BNSW K2577939
CUJEDONE EQU   X'20'                      JOT UPDATED              BNSW K2577940
*                                                                  BNSW K2577941
****************************************************************** BNSW K2577942
         SPACE 2                                                   BNSW K2577943
         POP   USING               BACK TO WHAT IT WAS             BNSW K2577944
         SPACE 2                                                   BNSW K2577945
         LTORG                                                     BNSW K2577946
****************************************************************** BNSW K2577947
./       ENDUP
/*
//SMPCNTL  DD  *
  RECEIVE
          SELECT(WM00017)
          .
/*
//*
//APPLY   EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//HMASMP.SYSUT1 DD UNIT=&WORK,SPACE=(1700,(1000,200))
//HMASMP.SYSUT2 DD UNIT=&WORK,SPACE=(1700,(1000,200))
//HMASMP.SYSUT3 DD UNIT=&WORK,SPACE=(1700,(1000,200))
//SMPCNTL  DD  *
  APPLY
        SELECT(WM00017)
        DIS(WRITE)
        .
/*
//
