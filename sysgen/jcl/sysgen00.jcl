//SYSGEN00 JOB (SYSGEN),'INITIALIZE DASD',                              
//             CLASS=A,MSGLEVEL=(1,1),MSGCLASS=A                        
//*                                                                     
//********************************************************************* 
//* PREPARE TO PERFORM MVS3.8J SYSTEM GENERATION                        
//*   1) DELETE ANY EXISTING WORK DATASETS                              
//*   2) INITIALIZE NEW DASD VOLUMES TO RECEIVE SYSTEM DATASETS         
//*   3) SUBMIT 2 CONNTINUATION JOBS TO INTERNAL READER                 
//********************************************************************* 
//*                                                                     
//IEHPROGM EXEC PGM=IEHPROGM                                            
//*                                                                     
//* ***************************************************************** * 
//* SCRATCH DATASETS ON WORK01 FROM PRIOR RUN (IF THEY EXIST)         * 
//* ***************************************************************** * 
//*                                                                     
//WORK01   DD  UNIT=SYSDA,VOL=SER=WORK01,DISP=OLD                       
//SYSPRINT DD  SYSOUT=*                                                 
//SYSIN    DD  *                                                        
  SCRATCH DSNAME=SYS1.OBJPDS01,VOL=SYSDA=WORK01,PURGE                   
  SCRATCH DSNAME=SYS1.OBJPDS02,VOL=SYSDA=WORK01,PURGE                   
  SCRATCH DSNAME=SYS1.OBJPDS03,VOL=SYSDA=WORK01,PURGE                   
  SCRATCH DSNAME=SYS2.LOCAL.LPALIB,VOL=SYSDA=WORK01,PURGE               
//*                                                                     
//*                                                                     
//ICKDSF   EXEC PGM=ICKDSF,REGION=4096K                                 
//*                                                                     
//* ***************************************************************** * 
//* INITIALIZE 3350 DASD ON 149, 14A, 14B, 14C to RECEIVE MVS3.8J     * 
//* SYSTEM DATASETS. IPL LOADER WILL BE WRITTEN ON MVSRES (149).      * 
//* ***************************************************************** * 
//*                                                                     
//IPLTEXT  DD  DSN=SYS1.ASAMPLIB(IEAIPL00),DISP=SHR,                    
//             UNIT=3350,VOL=SER=SMP000                                 
//SYSPRINT DD  SYSOUT=*                                                 
//SYSIN    DD  *                                                        
  INIT UNIT(149) -                                                      
               VERIFY(111111) -                                         
               OWNER(HERCULES) -                                        
               VOLID(MVSRES) -                                          
               IPLDD(IPLTEXT) -                                         
               VTOC(0,1,29)                                             
  INIT UNIT(14A) -                                                      
               VERIFY(222222) -                                         
               OWNER(HERCULES) -                                        
               VOLID(MVS000) -                                          
               VTOC(0,1,29)                                             
  INIT UNIT(14B) -                                                      
               VERIFY(333333) -                                         
               OWNER(HERCULES) -                                        
               VOLID(SPOOL1) -                                          
               VTOC(0,1,1)                                              
  INIT UNIT(14C) -                                                      
               VERIFY(444444) -                                         
               OWNER(HERCULES) -                                        
               VOLID(PAGE00) -                                          
               VTOC(0,1,1)                                              
//*                                                                     
//IEBGENER EXEC PGM=IEBGENER,COND=(0,NE,ICKDSF)                         
//*                                                                     
//* ***************************************************************** * 
//* IF ICKDSF RC=0000, SUBMIT CONTINUATION JOBS TO INTERNAL READER.   * 
//* ***************************************************************** * 
//*                                                                     
//SYSPRINT DD  DUMMY                                                    
//SYSIN    DD  DUMMY                                                    
//SYSUT1   DD  DATA,DLM='><'                                            
//SYSGEN00 JOB (SYSGEN),'MOUNT DASD',                                   
//             CLASS=A,MSGLEVEL=(1,1),MSGCLASS=A                        
//*                                                                     
//********************************************************************* 
//* CONTINUATION SYSGEN00 - THIS JOB WILL BE SUBMITTED TO THE         * 
//* INTERNAL READER IF THE DISK INITIALIZATION SUCCEEDED.               
//********************************************************************* 
//*                                                                     
//********************************************************************* 
//* PRESENT VARY AND MOUNT COMMANDS TO THE OPERATOR FOR CONFIRMATION. * 
//* THE DASD VOLUMES INITIALIZED IN THE PRIOR JOB WILL BE PLACED      * 
//* ONLINE AND MOUNTED WITH STORAGE CLASS OF PRIVATE.  THESE VOLUMES  * 
//* WILL RECEIVE THE GENERATED MVS 3.8H SYSTEM DATASETS.              * 
//********************************************************************* 
//*                                                                     
// V (149,14A,14B,14C),ONLINE                                           
// M 149,VOL=(SL,MVSRES),USE=PRIVATE                                    
// M 14A,VOL=(SL,MVS000),USE=PRIVATE                                    
// M 14B,VOL=(SL,SPOOL1),USE=PRIVATE                                    
// M 14C,VOL=(SL,PAGE00),USE=PRIVATE                                    
//*                                                                     
//IEFBR14  EXEC PGM=IEFBR14                                             
//*         END OF SUBMITTED SYSGEN00 (1 OF 2)                          
//SYSGEN00 JOB (SYSGEN),'SETUP FOR SYSGEN',                             
//             CLASS=A,MSGLEVEL=(1,1),MSGCLASS=A,COND=(0,NE)            
//*                                                                     
//********************************************************************* 
//* CONTINUATION SYSGEN00                                             * 
//********************************************************************* 
//*                                                                     
//DEFCAT   EXEC PGM=IDCAMS,REGION=4096K                                 
//*                                                                     
//* ***************************************************************** * 
//* DEFINE A USER CATALOG ON MVSRES, CATALOGED IN THE STARTER SYSTEM  * 
//* MASTER CATALOG.  IT WILL BE USED DURING THE SYSTEM GENERATION AND * 
//* WILL BECOME THE MASTER CATALOG ON THE TARGET MVS 3.8 SYSTEM.      * 
//* ***************************************************************** * 
//*                                                                     
//SYSPRINT DD  SYSOUT=*                                                 
//SYSUT1   DD  UNIT=3350,VOL=SER=MVSRES,DISP=OLD                        
//SYSIN    DD  *                                                        
                                                                        
  /* REMOVE SYS1.VSAM.MASTER.CATALOG FROM STARTER SYSTEM         */     
                                                                        
  EXPORT SYS1.VSAM.MASTER.CATALOG DISCONNECT                            
                                                                        
  /* SET THE LASTCC AND MAXCC TO 0000 IN CASE THE EXPORT FAILED  */     
                                                                        
  SET LASTCC = 0                                                        
  SET  MAXCC = 0                                                        
                                                                        
  /* DEFINE SYS1.VSAM.MASTER.CATALOG                            */      
                                                                        
  DEFINE USERCATALOG (                 -                                
               NAME(SYS1.VSAM.MASTER.CATALOG) -                         
               FILE(SYSUT1)            -                                
               VOLUME(MVSRES)          -                                
               CYLINDERS(30)           -                                
               BUFFERSPACE(8192) )                                      
                                                                        
//*                                                                     
//ALLOC    EXEC PGM=IEFBR14                                             
//*                                                                     
//* ***************************************************************** * 
//* ALLOCATE AND CATALOG WORK DATASETS USED IN STAGE 2 DECK           * 
//* ***************************************************************** * 
//*                                                                     
//STEPCAT  DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR                    
//OBJPDS01 DD  DSN=SYS1.OBJPDS01,DISP=(NEW,CATLG,DELETE),               
//             UNIT=3350,VOL=SER=WORK01,                                
//             SPACE=(CYL,(20,10,50)),                                  
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120)                     
//OBJPDS02 DD  DSN=SYS1.OBJPDS02,DISP=(NEW,CATLG,DELETE),               
//             UNIT=3350,VOL=SER=WORK01,                                
//             SPACE=(CYL,(20,10,50)),                                  
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120)                     
//OBJPDS03 DD  DSN=SYS1.OBJPDS03,DISP=(NEW,CATLG,DELETE),               
//             UNIT=3350,VOL=SER=WORK01,                                
//             SPACE=(CYL,(20,10,50)),                                  
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120)                     
//*                                                                     
//ASM      EXEC PGM=IEUASM,REGION=1024K,                                
//             PARM='LIST,OBJECT,NODECK,NOXREF'                         
//*                                                                     
//* ***************************************************************** * 
//* ASSEMBLE MODULE FOR LOCAL SVC 244 (AUTHORIZIATION)                * 
//* ***************************************************************** * 
//*                                                                     
//SYSLIB   DD  DSN=SYS1.AMACLIB,UNIT=SYSDA,VOL=SER=SMP000,DISP=SHR      
//SYSUT1   DD  UNIT=SYSDA,SPACE=(1700,(1400,50))                        
//SYSUT2   DD  UNIT=SYSDA,SPACE=(1700,(1400,50))                        
//SYSUT3   DD  UNIT=SYSDA,SPACE=(1700,(1400,50))                        
//SYSPRINT DD  SYSOUT=*                                                 
//SYSGO    DD  UNIT=SYSDA,DISP=(,PASS),DSN=&&SYSGO,                     
//             SPACE=(TRK,45),DCB=BLKSIZE=80                            
//SYSIN    DD  *                                                        
* SOURCE: CBT249-FILE0360                                               
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* 
*                                                                     * 
*  ----------------------S V C   2 4 4 -----------------------------  * 
*  SETJSCB - A NON-AUTHORIZED TYPE 4 SVC THAT TURNS THE JSCB          * 
*  AUTHORIZATION ON OR OFF BASED ON THE ENTRY CODE IN REGISTER 1.     * 
*                                                                     * 
*  ENTRY CODE = 0 - TURN JSCB AUTHORIZATION ON                        * 
*  ENTRY CODE = 4 - TURN JSCB AUTHORIZATION OFF                       * 
*                                                                     * 
*  CODED 3/18/76 BY J. W. RICH (FROM R. MARKEL).                      * 
*  MODED 3/31/81 BY J. A. MARTIN - W.S.R.C.C                          * 
*        ADDED CHECK FOR R1=0 OR R1=4 => SDUMP IF NOT GOOD R1         * 
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* 
IGC0024D CSECT                                                          
         USING *,R6                BASE REGISTER                        
         L     R12,PSATOLD         LOAD CURRENT TCB PTR                 
         L     R12,TCBJSCB(0,R12)  LOAD JSCB PTR                        
         LTR   R1,R1                    CHECK IF ZERO                   
         BZ    DOIT                     R1=0 IS OK - GO DO IT           
         C     R1,F4                    CHECK IF FOUR                   
         BE    DOIT                     R1=4 IS OK - GO DO IT           
         SDUMP HDR='IGC0024D ERROR R1 ^= 0 OR 4 - WSRCC LOCAL SVC'      
         SR    R1,R1                    SET OFF AUTH                    
DOIT     EX    R0,APFON(R1)        "EX"ECUTE APFON OR APFOFF BASED      
*                                       ON REGISTER 1 ENTRY CODE        
         BR    R14                 RETURN                               
         EJECT                                                          
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* 
*  VARIOUS AND SUNDRY "EX"ECUTED INSTRUCTIONS                         * 
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* 
APFON    OI    JSCBOPTS(R12),JSCBAUTH        TURN ON AUTHORIZATION      
APFOFF   NI    JSCBOPTS(R12),X'FF'-JSCBAUTH  TURN OFF AUTHORIZATION     
         EJECT                                                          
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* 
*  VARIOUS AND SUNDRY EQUATES                                           
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* 
F4       DC    F'04'               USED TO COMPARE TO R1                
JSCBAUTH EQU   X'01'               JSCB AUTHORIZATION BIT               
PSATOLD  EQU   X'21C'              OFFSET TO TCBOLD PTR                 
TCBJSCB  EQU   X'B4'               OFFSET TO JSCB PTR                   
JSCBOPTS EQU   X'EC'               OFFSET TO JSCB AUTHORIZATION FIELD   
R0       EQU   0                   REGISTER 0                           
R1       EQU   1                   REGISTER 1                           
R6       EQU   6                   REGISTER 6                           
R12      EQU   12                  REGISTER 12                          
R14      EQU   14                  REGISTER 14                          
         END                                                            
//*                                                                     
//LINK    EXEC PGM=IEWL,PARM=(LIST,MAP,XREF)                            
//*                                                                     
//* ***************************************************************** * 
//* CREATE SYS2.LOCAL.LPALIB AND INSERT MODULES FOR SVCS              * 
//* ***************************************************************** * 
//*                                                                     
//STEPCAT  DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR                    
//SYSPRINT DD  SYSOUT=*                                                 
//SYSLIB   DD  DISP=SHR,DSN=SYS1.LINKLIB                                
//SYSLMOD  DD  DSN=SYS2.LOCAL.LPALIB,DISP=(NEW,CATLG,DELETE),           
//             UNIT=3350,VOL=SER=WORK01,                                
//             SPACE=(CYL,(2,1,10)),                                    
//             DCB=SYS1.LPALIB                                          
//SYSGO    DD  DSN=&&SYSGO,DISP=(OLD,DELETE)                            
//SYSUT1   DD  UNIT=3330,SPACE=(CYL,(5,1))                              
//SYSLIN   DD  *                                                        
 INCLUDE SYSLIB(IEFBR14)         DUMMY SVC 240                          
 NAME IGC0024{(R)                                                       
 INCLUDE SYSLIB(IEFBR14)         DUMMY SVC 241                          
 NAME IGC0024A(R)                                                       
 INCLUDE SYSLIB(IEFBR14)         DUMMY SVC 242                          
 NAME IGC0024B(R)                                                       
 INCLUDE SYSLIB(IEFBR14)         DUMMY SVC 243                          
 NAME IGC0024C(R)                                                       
 INCLUDE SYSGO                   ASSEMBLED SVC 244                      
 NAME IGC0024D(R)                                                       
 INCLUDE SYSLIB(IEFBR14)         DUMMY SVC 245                          
 NAME IGC0024E(R)                                                       
 INCLUDE SYSLIB(IEFBR14)         DUMMY SVC 246                          
 NAME IGC0024F(R)                                                       
 INCLUDE SYSLIB(IEFBR14)         DUMMY SVC 247                          
 NAME IGC0024G(R)                                                       
 INCLUDE SYSLIB(IEFBR14)         DUMMY SVC 248                          
 NAME IGC0024H(R)                                                       
 INCLUDE SYSLIB(IEFBR14)         DUMMY SVC 249                          
 NAME IGC0024I(R)                                                       
 INCLUDE SYSLIB(IEFBR14)         DUMMY SVC 250                          
 NAME IGC0025{(R)                                                       
 INCLUDE SYSLIB(IEFBR14)         DUMMY SVC 251                          
 NAME IGC0025A(R)                                                       
 INCLUDE SYSLIB(IEFBR14)         DUMMY SVC 252                          
 NAME IGC0025B(R)                                                       
 INCLUDE SYSLIB(IEFBR14)         DUMMY SVC 253                          
 NAME IGC0025C(R)                                                       
 INCLUDE SYSLIB(IEFBR14)         DUMMY SVC 254                          
 NAME IGC0025D(R)                                                       
 INCLUDE SYSLIB(IEFBR14)         DUMMY SVC 255                          
 NAME IGC0025E(R)                                                       
//*                                                                     
//RECATLG  EXEC PGM=IDCAMS,REGION=1024K,COND=(0,NE,DEFCAT)              
//*                                                                     
//* ***************************************************************** * 
//* CATALOG SMP DATASETS                                              * 
//* ***************************************************************** * 
//*                                                                     
//STEPCAT  DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR                    
//SYSPRINT DD  SYSOUT=*                                                 
//SYSIN    DD  *                                                        
                                                                        
 DEFINE NONVSAM (NAME (SYS1.ACMDLIB) DEVT(3350) VOLUME(SMP000) )        
 DEFINE NONVSAM (NAME (SYS1.AGENLIB) DEVT(3350) VOLUME(SMP000) )        
 DEFINE NONVSAM (NAME (SYS1.AHELP) DEVT(3350) VOLUME(SMP000) )          
 DEFINE NONVSAM (NAME (SYS1.AIMAGE) DEVT(3350) VOLUME(SMP000) )         
 DEFINE NONVSAM (NAME (SYS1.ALPALIB) DEVT(3350) VOLUME(SMP000) )        
 DEFINE NONVSAM (NAME (SYS1.AMACLIB) DEVT(3350) VOLUME(SMP000) )        
 DEFINE NONVSAM (NAME (SYS1.AMODGEN) DEVT(3350) VOLUME(SMP000) )        
 DEFINE NONVSAM (NAME (SYS1.AOS00) DEVT(3350) VOLUME(SMP000) )          
 DEFINE NONVSAM (NAME (SYS1.AOS03) DEVT(3350) VOLUME(SMP000) )          
 DEFINE NONVSAM (NAME (SYS1.AOS04) DEVT(3350) VOLUME(SMP000) )          
 DEFINE NONVSAM (NAME (SYS1.AOS05) DEVT(3350) VOLUME(SMP000) )          
 DEFINE NONVSAM (NAME (SYS1.AOS06) DEVT(3350) VOLUME(SMP000) )          
 DEFINE NONVSAM (NAME (SYS1.AOS07) DEVT(3350) VOLUME(SMP000) )          
 DEFINE NONVSAM (NAME (SYS1.AOS11) DEVT(3350) VOLUME(SMP000) )          
 DEFINE NONVSAM (NAME (SYS1.AOS12) DEVT(3350) VOLUME(SMP000) )          
 DEFINE NONVSAM (NAME (SYS1.AOS20) DEVT(3350) VOLUME(SMP000) )          
 DEFINE NONVSAM (NAME (SYS1.AOS21) DEVT(3350) VOLUME(SMP000) )          
 DEFINE NONVSAM (NAME (SYS1.AOS24) DEVT(3350) VOLUME(SMP000) )          
 DEFINE NONVSAM (NAME (SYS1.AOS26) DEVT(3350) VOLUME(SMP000) )          
 DEFINE NONVSAM (NAME (SYS1.AOS29) DEVT(3350) VOLUME(SMP000) )          
 DEFINE NONVSAM (NAME (SYS1.AOS32) DEVT(3350) VOLUME(SMP000) )          
 DEFINE NONVSAM (NAME (SYS1.AOSA0) DEVT(3350) VOLUME(SMP000) )          
 DEFINE NONVSAM (NAME (SYS1.AOSA1) DEVT(3350) VOLUME(SMP000) )          
 DEFINE NONVSAM (NAME (SYS1.AOSB0) DEVT(3350) VOLUME(SMP000) )          
 DEFINE NONVSAM (NAME (SYS1.AOSB3) DEVT(3350) VOLUME(SMP000) )          
 DEFINE NONVSAM (NAME (SYS1.AOSBN) DEVT(3350) VOLUME(SMP000) )          
 DEFINE NONVSAM (NAME (SYS1.AOSC2) DEVT(3350) VOLUME(SMP000) )          
 DEFINE NONVSAM (NAME (SYS1.AOSC5) DEVT(3350) VOLUME(SMP000) )          
 DEFINE NONVSAM (NAME (SYS1.AOSC6) DEVT(3350) VOLUME(SMP000) )          
 DEFINE NONVSAM (NAME (SYS1.AOSCA) DEVT(3350) VOLUME(SMP000) )          
 DEFINE NONVSAM (NAME (SYS1.AOSCD) DEVT(3350) VOLUME(SMP000) )          
 DEFINE NONVSAM (NAME (SYS1.AOSCE) DEVT(3350) VOLUME(SMP000) )          
 DEFINE NONVSAM (NAME (SYS1.AOSD0) DEVT(3350) VOLUME(SMP000) )          
 DEFINE NONVSAM (NAME (SYS1.AOSD7) DEVT(3350) VOLUME(SMP000) )          
 DEFINE NONVSAM (NAME (SYS1.AOSD8) DEVT(3350) VOLUME(SMP000) )          
 DEFINE NONVSAM (NAME (SYS1.AOSG0) DEVT(3350) VOLUME(SMP000) )          
 DEFINE NONVSAM (NAME (SYS1.AOSH1) DEVT(3350) VOLUME(SMP000) )          
 DEFINE NONVSAM (NAME (SYS1.AOSH3) DEVT(3350) VOLUME(SMP000) )          
 DEFINE NONVSAM (NAME (SYS1.AOST3) DEVT(3350) VOLUME(SMP000) )          
 DEFINE NONVSAM (NAME (SYS1.AOST4) DEVT(3350) VOLUME(SMP000) )          
 DEFINE NONVSAM (NAME (SYS1.AOSU0) DEVT(3350) VOLUME(SMP000) )          
 DEFINE NONVSAM (NAME (SYS1.APARMLIB) DEVT(3350) VOLUME(SMP000) )       
 DEFINE NONVSAM (NAME (SYS1.APROCLIB) DEVT(3350) VOLUME(SMP000) )       
 DEFINE NONVSAM (NAME (SYS1.ASAMPLIB) DEVT(3350) VOLUME(SMP000) )       
 DEFINE NONVSAM (NAME (SYS1.ATCAMMAC) DEVT(3350) VOLUME(SMP000) )       
 DEFINE NONVSAM (NAME (SYS1.ATSOMAC) DEVT(3350) VOLUME(SMP000) )        
 DEFINE NONVSAM (NAME (SYS1.AUADS) DEVT(3350) VOLUME(SMP000) )          
 DEFINE NONVSAM (NAME (SYS1.HASPSRC) DEVT(3350) VOLUME(SMP000) )        
 DEFINE NONVSAM (NAME (SYS1.SMPACDS) DEVT(3350) VOLUME(SMP000) )        
 DEFINE NONVSAM (NAME (SYS1.SMPACRQ) DEVT(3350) VOLUME(SMP000) )        
 DEFINE NONVSAM (NAME (SYS1.SMPMTS) DEVT(3350) VOLUME(SMP000) )         
 DEFINE NONVSAM (NAME (SYS1.SMPPTS) DEVT(3350) VOLUME(SMP000) )         
 DEFINE NONVSAM (NAME (SYS1.SMPSTS) DEVT(3350) VOLUME(SMP000) )         
                                                                        
//*        END OF SUBMITTED SYSGEN00 (2 OF 2)                           
><                                                                      
//SYSUT2   DD  SYSOUT=(A,INTRDR)                                        
//*        END OF PRIMARY SYSGEN00                                      
