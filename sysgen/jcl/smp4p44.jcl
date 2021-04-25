//SMP4P44  JOB (SYSGEN),'SMP 4.44 FROM MVS38J',                         
//             CLASS=A,                                                 
//             MSGCLASS=A,MSGLEVEL=(1,1)                                
//*                                                                     
//********************************************************************* 
//*                                                                     
//*                       MVS 3.8 SYSGEN                                
//*                       ==============                                
//*                                                                     
//* FUNC: 1. Install SMP 4.44 load modules into SYS1.LINKLIB from the   
//*          MVS 3.8J DLIB tape (VOL=SER=MVS38J; file: zdlib1.het).    
//*       2. Install SMP 4 JCL procedures into SYS1.PROCLIB from the    
//*          MVS 3.8J DTR tape (VOL=SER=T74172; file: smp4b.het).       
//*       3. Install a modified SMP 4 JCL procedure into SYS1.PROCLIB.  
//*          This procedure, based upon those contained on the DTR tape 
//*          correct some errors and include changes that make the      
//*          installation process easier.                               
//*       4. Initialize WORK00, WORK01, and SMP000 volumes              
//*                                                                     
//* NOTE: 1. The level of the SMP 4 load modules supplied on the        
//*          MVS 3.8J DTR tape (4.22) is lower than the level required  
//*          for the installation of MVS 3.8J (4.24).  This job builds  
//*          SMP 4 load modules at a higher level (4.44) from:          
//*          (a) Linkage editor control statements extracted from the   
//*              last of three jobs contained within the JCLIN for      
//*              function ESY1400 contained within the SMPMCS data set  
//*              on the MVS 3.8J DLIB tape.                             
//*          (b) Modules extracted from the ESY1400.F1 data set on the  
//*              MVS 3.8J DLIB tape.                                    
//*       2. An IPL is required to activate the new load modules.       
//*                                                                     
//* HIST: 2002.01.25: Created by Peter Stockdill.                       
//*       2014.12.05: Modified by Jay Moseley                           
//*                                                                     
//********************************************************************* 
//*                                                                     
/*MESSAGE  ************************************************************ 
/*MESSAGE  * An IPL is required after this job has completed!!!       * 
/*MESSAGE  ************************************************************ 
//*-------------------------------------------------------------------- 
//S1       EXEC PGM=IDCAMS,REGION=1024K                                 
//SYSIN    DD  *                                                        
 REPRO INFILE(I1) OUTFILE(O1) SKIP(1607) COUNT(161)                     
//I1       DD  DSN=SMPMCS,DISP=OLD,                                     
//             UNIT=(TAPE,,DEFER),VOL=(,RETAIN,SER=MVS38J),LABEL=(1,SL) 
//O1       DD  DISP=(NEW,PASS),                                         
//             UNIT=SYSDA,SPACE=(TRK,(1,1)),                            
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120)                     
//SYSPRINT DD  SYSOUT=*                                                 
//*-------------------------------------------------------------------- 
//S2       EXEC PGM=IEBCOPY,REGION=1024K                                
//SYSIN    DD  *                                                        
         COPY  OUTDD=O1,INDD=I1                                         
         COPY  OUTDD=O2,INDD=((I2,R))                                   
//I1       DD  DSN=ESY1400.F1,DISP=OLD,                                 
//             VOL=REF=*.S1.I1,LABEL=(4,SL)                             
//I2       DD  DSN=SMPMVS,DISP=OLD,                                     
//             UNIT=AFF=I1,VOL=SER=T74172,LABEL=(1,SL)                  
//O1       DD  DISP=(NEW,PASS),                                         
//             UNIT=SYSDA,SPACE=(TRK,(50,20,30))                        
//O2       DD  DSN=SYS1.PROCLIB,DISP=SHR                                
//SYSPRINT DD  SYSOUT=*                                                 
//*-------------------------------------------------------------------- 
//S3       EXEC LINKS,                                                  
//             PARM='LIST,NCAL,SIZE=(512K,100K)',CLASS='*',             
//             OBJ=,UNIT=,SER=,N=' ',NAME=,P1=,MOD=,P2=                 
//SYSPUNCH DD  DUMMY                                                    
//SYSLMOD  DD  DSN=SYS1.LINKLIB,DISP=SHR                                
//SYSLIN   DD  DSN=*.S1.O1,DISP=(OLD,DELETE)                            
//AOS12    DD  DSN=*.S2.O1,DISP=(OLD,DELETE)                            
//*-------------------------------------------------------------------- 
//S4       EXEC PGM=IEBUPDTE,REGION=1024K,PARM=NEW                      
//SYSUT2    DD DISP=MOD,DSN=SYS1.PROCLIB                                
//SYSPRINT  DD SYSOUT=*                                                 
//SYSIN     DD DATA,DLM='><'                                            
./ ADD NAME=DLBSMP,LIST=ALL                                             
//DLBSMP  PROC TLIB=WORK01             WORK VOLUME SERIAL               
//SMP     EXEC PGM=HMASMP,PARM='DATE=U',REGION=4096K,TIME=999           
//********************************************************************* 
//*             OS/VS2 (MVS) DLIB BUILD PROCEDURE                       
//********************************************************************* 
//SMPLOG   DD  DUMMY                                 LOG                
//SMPOUT   DD  SYSOUT=*                              SUMMARY REPORTS    
//SMPTLIB  DD  UNIT=SYSDA,VOL=SER=&TLIB,DISP=OLD     RELFILE TLIBS      
//SMPWRK1  DD  UNIT=SYSDA,SPACE=(CYL,(5,10,84)),     WORK-COPY/UPDTE    
//             DCB=(RECFM=FB,BLKSIZE=3120,LRECL=80)                     
//SMPWRK2  DD  UNIT=SYSDA,SPACE=(CYL,(5,10,84)),     WORK-COPY/UPDTE    
//             DCB=(RECFM=FB,BLKSIZE=3120,LRECL=80)                     
//SMPWRK3  DD  UNIT=SYSDA,SPACE=(CYL,(5,10,84)),     WORK-OBJECT/ZAP    
//             DCB=(RECFM=FB,BLKSIZE=3120,LRECL=80)                     
//SMPWRK4  DD  UNIT=SYSDA,SPACE=(CYL,(1,10,84)),     WORK-EXPAND/ZAP    
//             DCB=(RECFM=FB,BLKSIZE=3120,LRECL=80)                     
//SMPWRK5  DD  UNIT=SYSDA,SPACE=(CYL,(30,10,250))    WORK-LOAD MODS     
//SYSPRINT DD  SYSOUT=*                              UTILITIES DEFAULT  
//SYSUDUMP DD  SYSOUT=*                              ABEND DUMP         
//SYSUT1   DD  UNIT=SYSDA,SPACE=(1700,(600,100))     WORK               
//SYSUT2   DD  UNIT=SYSDA,SPACE=(1700,(600,100))     WORK               
//SYSUT3   DD  UNIT=SYSDA,SPACE=(1700,(600,100))     WORK               
//SYSUT4   DD  UNIT=SYSDA,SPACE=(80,(2,2))           WORK RETRY         
//********************************************************************* 
//ASMPRINT DD  SYSOUT=*                              ASSEMBLER          
//CMPPRINT DD  SYSOUT=*                              COMPRESS           
//COPPRINT DD  SYSOUT=*                              IEBCOPY            
//LKDPRINT DD  SYSOUT=*                              LINK EDITOR        
//E37PRINT DD  SYSOUT=*                              RETRY              
//UPDPRINT DD  SYSOUT=*                              IEBUPDTE           
//ZAPPRINT DD  SYSOUT=*                              AMASPZAP           
//SYSLIB   DD  DSN=SYS1.AMACLIB,DISP=SHR             ASM MACROS/COPY    
//         DD  DSN=SYS1.AMODGEN,DISP=SHR                                
//         DD  DSN=SYS1.AGENLIB,DISP=SHR                                
//         DD  DSN=SYS1.SMPMTS,DISP=SHR                                 
//********************************************************************* 
//SMPACDS  DD  DSN=SYS1.SMPACDS,DISP=OLD                                
//SMPACRQ  DD  DSN=SYS1.SMPACRQ,DISP=OLD                                
//SMPMTS   DD  DSN=SYS1.SMPMTS,DISP=OLD                                 
//SMPPTS   DD  DSN=SYS1.SMPPTS,DISP=OLD                                 
//SMPSTS   DD  DSN=SYS1.SMPSTS,DISP=OLD                                 
//********************************************************************* 
//ACMDLIB  DD  DSN=SYS1.ACMDLIB,DISP=OLD                                
//AGENLIB  DD  DSN=SYS1.AGENLIB,DISP=OLD                                
//AHELP    DD  DSN=SYS1.AHELP,DISP=OLD                                  
//AIMAGE   DD  DSN=SYS1.AIMAGE,DISP=OLD                                 
//ALPALIB  DD  DSN=SYS1.ALPALIB,DISP=OLD                                
//AMACLIB  DD  DSN=SYS1.AMACLIB,DISP=OLD                                
//AMODGEN  DD  DSN=SYS1.AMODGEN,DISP=OLD                                
//AOS00    DD  DSN=SYS1.AOS00,DISP=OLD                                  
//AOS03    DD  DSN=SYS1.AOS03,DISP=OLD                                  
//AOS04    DD  DSN=SYS1.AOS04,DISP=OLD                                  
//AOS05    DD  DSN=SYS1.AOS05,DISP=OLD                                  
//AOS06    DD  DSN=SYS1.AOS06,DISP=OLD                                  
//AOS07    DD  DSN=SYS1.AOS07,DISP=OLD                                  
//AOS11    DD  DSN=SYS1.AOS11,DISP=OLD                                  
//AOS12    DD  DSN=SYS1.AOS12,DISP=OLD                                  
//AOS20    DD  DSN=SYS1.AOS20,DISP=OLD                                  
//AOS21    DD  DSN=SYS1.AOS21,DISP=OLD                                  
//AOS24    DD  DSN=SYS1.AOS24,DISP=OLD                                  
//AOS26    DD  DSN=SYS1.AOS26,DISP=OLD                                  
//AOS29    DD  DSN=SYS1.AOS29,DISP=OLD                                  
//AOS32    DD  DSN=SYS1.AOS32,DISP=OLD                                  
//AOSA0    DD  DSN=SYS1.AOSA0,DISP=OLD                                  
//AOSA1    DD  DSN=SYS1.AOSA1,DISP=OLD                                  
//AOSB0    DD  DSN=SYS1.AOSB0,DISP=OLD                                  
//AOSB3    DD  DSN=SYS1.AOSB3,DISP=OLD                                  
//AOSBN    DD  DSN=SYS1.AOSBN,DISP=OLD                                  
//AOSC2    DD  DSN=SYS1.AOSC2,DISP=OLD                                  
//AOSC5    DD  DSN=SYS1.AOSC5,DISP=OLD                                  
//AOSC6    DD  DSN=SYS1.AOSC6,DISP=OLD                                  
//AOSCA    DD  DSN=SYS1.AOSCA,DISP=OLD                                  
//AOSCD    DD  DSN=SYS1.AOSCD,DISP=OLD                                  
//AOSCE    DD  DSN=SYS1.AOSCE,DISP=OLD                                  
//AOSD0    DD  DSN=SYS1.AOSD0,DISP=OLD                                  
//AOSD7    DD  DSN=SYS1.AOSD7,DISP=OLD                                  
//AOSD8    DD  DSN=SYS1.AOSD8,DISP=OLD                                  
//AOSG0    DD  DSN=SYS1.AOSG0,DISP=OLD                                  
//AOSH1    DD  DSN=SYS1.AOSH1,DISP=OLD                                  
//AOSH3    DD  DSN=SYS1.AOSH3,DISP=OLD                                  
//AOST3    DD  DSN=SYS1.AOST3,DISP=OLD                                  
//AOST4    DD  DSN=SYS1.AOST4,DISP=OLD                                  
//AOSU0    DD  DSN=SYS1.AOSU0,DISP=OLD                                  
//APARMLIB DD  DSN=SYS1.APARMLIB,DISP=OLD                               
//APROCLIB DD  DSN=SYS1.APROCLIB,DISP=OLD                               
//ASAMPLIB DD  DSN=SYS1.ASAMPLIB,DISP=OLD                               
//ATCAMMAC DD  DSN=SYS1.ATCAMMAC,DISP=OLD                               
//ATSOMAC  DD  DSN=SYS1.ATSOMAC,DISP=OLD                                
//AUADS    DD  DSN=SYS1.AUADS,DISP=OLD                                  
//HASPSRC  DD  DSN=SYS1.HASPSRC,DISP=OLD                                
//********************************************************************* 
><                                                                      
//*                                                                     
//S5       EXEC  PGM=IEHDASDR,REGION=1024K                              
//SYSPRINT DD  SYSOUT=A                                                 
//SYSIN    DD  *                                                        
   ANALYZE TODD=148,VTOC=1,EXTENT=15,NEWVOLID=SMP000                    
   ANALYZE TODD=149,VTOC=1,EXTENT=15,NEWVOLID=WORK00                    
   ANALYZE TODD=14A,VTOC=1,EXTENT=15,NEWVOLID=WORK01                    
//                                                                      
