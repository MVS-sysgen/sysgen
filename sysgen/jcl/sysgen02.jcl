//SYSGEN02 JOB (SYSGEN),'LINK JES2',                                    
//             CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1)                        
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR                    
//*                                                                     
//********************************************************************* 
//* INSTALL JES2 IN TARGET MVS3.8J                                      
//*   1) LINK JES2 LOAD MODULE AND LPALIB MODULE                        
//*   2) CREATE JES2 PARAMETERS IN SYS1.PARMLIB(JES2PM00)               
//*   3) CREATE JES2 EXECUTION PROCEDURE IN SYS1.PROCLIB(JES2)          
//*   4) ALLOCATE AND CATALOG JES2 CHECKPOINT AND SPOOL DATASETS        
//********************************************************************* 
//*                                                                     
//JES2    EXEC PGM=IEWL,PARM='XREF,LET,LIST,NCAL'                       
//*                                                                     
//* ***************************************************************** * 
//* LINK JES2 FROM DISTRIBUTION LIBRARY                               * 
//* ***************************************************************** * 
//*                                                                     
//SYSPRINT DD  SYSOUT=*                                                 
//SYSUT1   DD  DSN=&&SYSUT1,UNIT=SYSDA,SPACE=(1024,(50,20))             
//SYSLMOD  DD  DSN=SYS1.LINKLIB,DISP=SHR                                
//AOSH3    DD  DSN=SYS1.AOSH3,DISP=SHR                                  
//SYSLIN   DD  *                                                        
       ORDER HASPNUC                                                    
       ORDER HASPRDR                                                    
       ORDER HASPRDRO                                                   
       ORDER HASPRSCN                                                   
       ORDER HASPXEQ                                                    
       ORDER HASPPRPU                                                   
       ORDER HASPACCT                                                   
       ORDER HASPMISC                                                   
       ORDER HASPCON                                                    
       ORDER HASPRTAM                                                   
       ORDER HASPCOMM                                                   
       ORDER HASPCOMA                                                   
       ORDER HASPINIT(P)                                                
       INCLUDE AOSH3(HASPNUC)                                           
       INCLUDE AOSH3(HASPRDR)                                           
       INCLUDE AOSH3(HASPXEQ)                                           
       INCLUDE AOSH3(HASPPRPU)                                          
       INCLUDE AOSH3(HASPACCT)                                          
       INCLUDE AOSH3(HASPMISC)                                          
       INCLUDE AOSH3(HASPCON)                                           
       INCLUDE AOSH3(HASPRTAM)                                          
       INCLUDE AOSH3(HASPCOMM)                                          
       INCLUDE AOSH3(HASPINIT)                                          
       PAGE    HASPINIT                                                 
       NAME    HASJES20(R)                                              
       INCLUDE AOSH3(HASPBLKS)                                          
       NAME    HASPBLKS(R)                                              
       INCLUDE AOSH3(HASPFMT0)                                          
       NAME    HASPFMT0(R)                                              
       INCLUDE AOSH3(HASPFMT1)                                          
       NAME    HASPFMT1(R)                                              
       INCLUDE AOSH3(HASPFMT2)                                          
       NAME    HASPFMT2(R)                                              
       INCLUDE AOSH3(HASPFMT3)                                          
       NAME    HASPFMT3(R)                                              
       INCLUDE AOSH3(HASPFMT4)                                          
       NAME    HASPFMT4(R)                                              
       INCLUDE AOSH3(HASPFMT5)                                          
       NAME    HASPFMT5(R)                                              
//*                                                                     
//SSSM     EXEC PGM=IEWL,PARM='XREF,LIST,LET,NCAL'                      
//SYSPRINT DD  SYSOUT=*                                                 
//SYSUT1   DD  DSN=&&SYSUT1,UNIT=SYSDA,SPACE=(1024,(50,20))             
//SYSLMOD  DD  DSN=SYS1.LPALIB,DISP=SHR                                 
//AOSH3    DD  DSN=SYS1.AOSH3,DISP=SHR                                  
//SYSLIN   DD  *                                                        
       INCLUDE AOSH3(HASPSSSM)                                          
       NAME    HASPSSSM(R)                                              
//*                                                                     
//JES2PARM EXEC PGM=IEBUPDTE,PARM=NEW                                   
//*                                                                     
//* ***************************************************************** * 
//* CREATE JES2 PARAMETERS IN SYS1.PARMLIB(JES2PM00)                    
//* ***************************************************************** * 
//*                                                                     
//SYSPRINT DD  SYSOUT=*                                                 
//SYSUT2   DD  DISP=MOD,DSN=SYS1.PARMLIB                                
//SYSIN    DD  *                                                        
./ ADD NAME=JES2PM00,LIST=ALL                                           
./ NUMBER NEW1=10,INCR=10                                               
&BUFSIZE=3664                  BUFFER SIZE (5 PER TRACK ON 3350)        
&CCOMCHR=$                     OPERATOR COMMAND IDENTIFIER              
&CHKPT=MVS000                  CHECKPOINT DATASET ON MVS000             
&CKPTIME=60                    CHECKPOINT EVERY 60 SECONDS              
&DMNDSET=NO                    DEMAND SETUP OPTION                      
&DSNPRFX=SYS1                  DATA SET PREFIX (SPOOL AND CKPT)         
&ESTIME=0035                   ESTIMATED JOB TIME 35 MINUTES            
&ESTLNCT=50                    ESTIMATED LINE COUNT 50,000 LINES        
&ESTPUN=500                    ESTIMATED CARD COUNT 500 CARDS           
*********************************************************************** 
*                              INITIATORS                             * 
*********************************************************************** 
I1 START,CLASS=A               STANDARD                                 
I2 START,CLASS=BA              QUEUED STANDARD                          
I3 START,CLASS=S               SYSTEMS                                  
I4 DRAIN,CLASS=DCBA                                                     
I5 DRAIN,CLASS=ECBA                                                     
I6 DRAIN,CLASS=FECBA                                                    
INTRDR CLASS=A,NOHOLD,AUTH=0,PRIOINC=0,PRIOLIM=15                       
&JCOPYLM=10                    MAX JOB OUTPUT COPIES                    
&LINECT=61                     LINE PER PAGE LIMIT                      
&MAXCLAS=5                     MAX JOB CLASSES PER INITIATOR            
&MAXJOBS=128                   MAX JOBS IN JOB QUEUE                    
&MAXPART=6                     MAX NUMBER OF BATCH INITIATORS           
&MINJOES=100                   MIN NUMBER OF FREE JOB OUTPUT ELEMENTS   
&MLBFSIZ=400                   MULTI-LEAVE BUFFER SIZE                  
&MSGID=YES                     MSG ID OPTION                            
&NOPRCCW=30                    PRINTER CCW LIMIT                        
&NOPUCCW=45                    PUNCH CCW LIMIT                          
&NUMACE=50                     AUTO COMMAND LIMIT                       
&NUMBUF=128                    I/O BUFFER COUNT                         
&NUMCLAS=3                     PRINTER SYSOUT CLASS LIMIT               
&NUMCMBS=256                   NUMBER OF CONSOLE MESSAGE BUFFERS        
&NUMDA=2                       MAX NUMBER OF SPOOL VOLUMES                  
&NUMINRS=5                     NUMBER OF INTERNAL READERS               
&NUMJOES=1024                  NUMBER OF JOB OUTPUT ELEMENTS            
&NUMPRTS=3                     MAX NUMBER OF LOCAL PRINTERS             
&NUMPUNS=1                     MAX NUMBER OF LOCAL PUNCHES              
&NUMRDRS=1                     MAX NUMBER OF LOCAL READERS              
&NUMSMFB=96                    NUMBER OF SMF BUFFERS                    
&NUMTGV=3330                   TRACK GROUPS PER SPOOL VOLUME (3350)     
&OUTPOPT=0                     ACTION FOR JOBS EXCEEDING OUTPUT         
&OUTXS=5000                    MSG INTERVAL FOR EST OUTPUT              
&PRIDCT=30                     PRINT LINES PER SEPARATOR PAGE           
&PRIHIGH=10                    MAX PRIORITY AGING LIMIT                 
&PRILOW=3                      MIN PRIORITY AGING LIMIT                 
*********************************************************************** 
*                              LOCAL PRINTERS                         * 
*********************************************************************** 
PRINTER1       UNIT=00E,CLASS=A,FCB=6,FORMS=STD1,SEP,AUTO              C
               NOPAUSE,START                                 1403       
PRINTER2       UNIT=00F,CLASS=M,FCB=6,FORMS=STD1,SEP,AUTO              C
               NOPAUSE,UCS=TB,START                          3211       
*********************************************************************** 
&PRIOOPT=YES                   SUPPORT /*PRIORITY CARD                  
&PRIRATE=24                    PRIORITY INCREMENT INTERVAL              
&PRTBOPT=YES                   DOUBLE BUFFER LOCAL PRINTERS             
&PRTFCB=6                      FCB DEFAULT                              
&PRTRANS=NO                    DON'T TRANSLATE LOWER->UPPER CASE        
&PRTYOPT=YES                   PRTY SUPPORT OPTION                      
&PUNBOPT=YES                   DOUBLE BUFFER LOCAL PUNCHES              
*********************************************************************** 
*                              LOCAL PUNCHES                          * 
*********************************************************************** 
PUNCH1         CLASS=B,SEP,AUTO,NOPAUSE,UNIT=00D,START       2540P      
*********************************************************************** 
&RCOMCHR=$                     INSTREAM COMMAND IDENTIFIER              
&RDROPSL=00000300051221E00011  TSU LOGON                                
&RDROPST=00014395951221E00011  STARTED TASK                             
&RDROPSU=30000013051221E00011  BATCH                                    
******** BPPMMMMSSCCCRLAAAAEF      DEFAULT CONVERSION PARAMETER FIELDS  
******** !! !   ! !  !!!   !+-F--- DEFAULT ALLOCATION MESSAGE LEVEL     
******** !! !   ! !  !!!   !         0: None listed unless abend        
******** !! !   ! !  !!!   !         1: Allocation/Termination          
******** !! !   ! !  !!!   !            messages listed                 
******** !! !   ! !  !!!   +-E---  DEFAULT MSGLEVEL parameter           
******** !! !   ! !  !!!             0: JOB statement only              
******** !! !   ! !  !!!             1: input, catalogued procedure     
******** !! !   ! !  !!!                statements, substitutions       
******** !! !   ! !  !!!             2: input statements only           
******** !! !   ! !  !!+-AAAA----  Command Group Authorization          
******** !! !   ! !  !!              E000 = allow all commands          
******** !! !   ! !  !+-L--------  BLP Authorization                    
******** !! !   ! !  !               0: disallow (BLP as NL)            
******** !! !   ! !  !               1: allow (BLP as BLP)              
******** !! !   ! !  +-R---------  Operator Commands in jobstream       
******** !! !   ! !                0=execute without display            
******** !! !   ! !                1=display and execute w/o confirm    
******** !! !   ! !                2=display, require confirm           
******** !! !   ! !                3=ignore                             
******** !! !   ! +-CCC----------  Default step REGION= parameter       
******** !! +---+----------------  Default step TIME= parameter MMMMSS  
******** !+-PP-------------------  Default job priority                 
******** !                           Ignored, specify 00                
******** +-B---------------------  Account/programmer required          
********                           0 = none required                    
********                           1 = Account required                 
********                           2 = Programmer name required         
********                           3 = Both required                    
*********************************************************************** 
*                              LOCAL READERS                          * 
*********************************************************************** 
READER1        AUTH=0,CLASS=A,NOHOLD,MSGCLASS=A,UNIT=00C     2540R      
&RECINCR=2                     RECORD ALTERNATION                       
&RJOBOPT=5                     JOB CARD SCAN OPTION                     
&RPRI(1)=6                     PRTY FOR ESTIMATED TIME                  
&RPRI(2)=5                     PRTY FOR ESTIMATED TIME                  
&RPRI(3)=4                     PRTY FOR ESTIMATED TIME                  
&RPRI(4)=3                     PRTY FOR ESTIMATED TIME                  
&RPRI(5)=2                     PRTY FOR ESTIMATED TIME                  
&RPRI(6)=1                     PRTY FOR ESTIMATED TIME                  
&RPRT(1)=5                     ESTIMATED TIME TABLE ENTRY               
&RPRT(2)=10                    ESTIMATED TIME TABLE ENTRY               
&RPRT(3)=59                    ESTIMATED TIME TABLE ENTRY               
&RPRT(4)=120                   ESTIMATED TIME TABLE ENTRY               
&RPRT(5)=279620                ESTIMATED TIME TABLE ENTRY               
&RPRT(6)=279620                ESTIMATED TIME TABLE ENTRY               
&RPS=YES                       RPS SUPPORT                              
&SPOOL=SPOOL1                  SPOOL VOLUME SERIAL                      
*********************************************************************** 
*                              STC / TSU / BATCH JOB CLASSES          * 
*********************************************************************** 
&STC     NOJOURN,LOG,OUTPUT,NOTYPE6,NOTYPE26,NOUJP,NOUSO,PROCLIB=00,   C
         PERFORM=1                                                      
&TSU     NOJOURN,LOG,OUTPUT,NOTYPE6,NOTYPE26,NOUJP,NOUSO,PROCLIB=00,   C
         PERFORM=2                                                      
&A       NOJOURN,LOG,OUTPUT,PROCLIB=00,PERFORM=4  Low priority batch    
&B       NOJOURN,LOG,OUTPUT,PROCLIB=00,PERFORM=1  Standard batch        
&S       NOJOURN,LOG,OUTPUT,PROCLIB=00,PERFORM=3, Sysprog              C
         CONVPARM=30000013051211E00011                                  
STCMCLAS=Y                     STARTED TASK MESSAGE CLASS               
&STDFORM=STD1                  DEFAULT FORMS ID                         
&TGWARN=80                     THRESHOLD SPOOL UTILIZATION WARNING      
&TIMEOPT=YES                   EXCEEDED EXEC TIME WARN                  
&TIMEXS=1                      EXCEEDED EXEC TIME WARNING INTERVAL      
&XLIN(1)=16777215              OUTPUT SELECT PRIORITY CATEGORY          
&XLIN(2)=1000                                                           
&XLIN(3)=2000                                                           
&XLIN(4)=2500                                                           
&XLIN(5)=6500                                                           
&XLIN(6)=1000000                                                        
&XLIN(7)=16777215                                                       
TSUMCLAS=Y                     TSO USER MESSAGE CLASS                   
*********************************************************************** 
*                            OUTPUT CLASSES                           * 
*********************************************************************** 
$$A  PRINT,SYSOUT,NOHOLD,TRKCEL     STANDARD OUTPUT CLASS               
$$B  PRINT,SYSOUT,NOHOLD,TRKCEL     STANDARD PUNCH OUTPUT CLASS         
$$L  PRINT,SYSOUT,NOHOLD,TRKCEL     SYSLOG                              
$$M  PRINT,SYSOUT,NOHOLD,TRKCEL     DOCUMENTATION                       
$$X  PRINT,SYSOUT,HOLD              HELD BATCH                          
$$Y  PRINT,SYSOUT,HOLD              HELD STC/TSU                        
$$Z  PRINT,DUMMY,NOHOLD             TRASH (DISCARDED)                   
*********************************************************************** 
*                     AUTOMATIC OPERATOR COMMANDS                     * 
*********************************************************************** 
$T OSC3,D=J                                                             
$T OSC3,D=T                                                             
$VS,'MN JOBNAMES,T'                                                     
$VS,'MN SESS,T'                                                         
$VS,'V (100-104),ONLINE'                                                
$VS,'$T PRT1,F=STD1'                                                    
$VS,'$T PRT2,F=STD1'                                                    
$VS,'$SPRT1'                                                            
$VS,'$SPRT2'                                                            
./ ENDUP                                                                
//*                                                                     
//JES2PRC  EXEC PGM=IEBUPDTE,PARM=NEW                                   
//*                                                                     
//* ***************************************************************** * 
//* CREATE JES2 EXECUTION PROCEDURE IN SYS1.PROCLIB(JES2)               
//* ***************************************************************** * 
//*                                                                     
//SYSPRINT DD  SYSOUT=*                                                 
//SYSUT2   DD  DISP=MOD,DSN=SYS1.PROCLIB                                
//SYSIN    DD  DATA,DLM='><'                                            
./ ADD NAME=JES2,LIST=ALL                                               
./ NUMBER NEW1=10,INCR=10                                               
//JES2     PROC M=JES2PM00,                                             
//             N=SYS1,                                                  
//             L=LINKLIB,                                               
//             U=3350,                                                  
//             N1=SYS1,                                                 
//             P=PARMLIB                                                
//IEFPROC  EXEC PGM=HASJES20,                                           
//             TIME=1440,                                               
//             DPRTY=(15,15)                                            
//STEPLIB  DD  UNIT=&U,DISP=SHR,DSN=&N..&L                              
//PROC00   DD  DSN=&N..PROCLIB,DISP=SHR                                 
//HASPPARM DD  DSN=&N1..&P(&M),DISP=SHR                                 
//HASPLIST DD  DDNAME=IEFRDER                                           
./ ENDUP                                                                
><                                                                      
//*                                                                     
//IEFBR14  EXEC PGM=IEFBR14                                             
//*                                                                     
//* ***************************************************************** * 
//* ALLOCATE AND CATALOG JES2 CHECKPOINT AND SPOOL DATASETS           * 
//* ***************************************************************** * 
//*                                                                     
//HASPCKPT DD  DSN=SYS1.HASPCKPT,                                       
//             UNIT=3350,VOL=SER=MVS000,DISP=(,CATLG),                  
//             SPACE=(CYL,(50))                                         
//HASPACE  DD  DSN=SYS1.HASPACE,                                        
//             UNIT=3350,VOL=SER=SPOOL1,DISP=(,CATLG),                  
//             SPACE=(CYL,(554))                                        
//                                                                      
