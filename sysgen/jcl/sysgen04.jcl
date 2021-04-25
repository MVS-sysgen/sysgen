//SYSGEN04 JOB (SYSGEN),'ADD PARMS/PROCS/PGMS',                         
//             CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1)                        
/*JOBPARM LINES=100                                                     
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR                    
//*                                                                     
//* ***************************************************************** * 
//* SOME UPDATES/ADDITIONS ARE NECESSARY TO MAKE USING THE NEW SYSTEM * 
//* EASIER/MORE USER-FRIENDLY. THIS JOB BEGINS MAKING THOSE CHANGES.  * 
//* ***************************************************************** * 
//*                                                                     
//ASMFCL   PROC MAC='SYS1.MACLIB',MAC1='SYS1.MACLIB'                    
//ASM      EXEC PGM=IFOX00,PARM='OBJ,LIST,NOXREF',REGION=1024K          
//SYSLIB   DD  DSN=&MAC,DISP=SHR                                        
//         DD  DSN=&MAC1,DISP=SHR                                       
//SYSUT1   DD  UNIT=SYSDA,SPACE=(1700,(600,100))                        
//SYSUT2   DD  UNIT=SYSDA,SPACE=(1700,(300,50))                         
//SYSUT3   DD  UNIT=SYSDA,SPACE=(1700,(300,50))                         
//SYSPRINT DD  SYSOUT=*                                                 
//SYSTERM  DD  SYSOUT=*                                                 
//SYSPUNCH DD  DUMMY                                                    
//SYSGO    DD  DSN=&&OBJSET,UNIT=SYSDA,SPACE=(80,(200,50)),             
//            DISP=(NEW,PASS)                                           
//LKED     EXEC PGM=IEWL,PARM='XREF,LET,LIST,NCAL',REGION=128K,         
//            COND=(8,LT,ASM)                                           
//SYSLIN   DD  DSN=&&OBJSET,DISP=(OLD,DELETE)                           
//         DD  DDNAME=SYSIN                                             
//SYSLMOD  DD  DSN=&&GOSET(GO),UNIT=SYSDA,SPACE=(1024,(50,20,1)),       
//            DISP=(MOD,PASS)                                           
//SYSUT1   DD  UNIT=SYSDA,SPACE=(1024,(50,20))                          
//SYSPRINT DD  SYSOUT=*                                                 
//         PEND                                                         
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//*                                                                     
//BACKUP01 EXEC PGM=IEHPROGM                                            
//*                                                                     
//* ***************************************************************** * 
//* SYS1.PARMLIB: BACKUP IEASYS00 TO IEASYS99                         * 
//*               BACKUP IEAAPF00 TO IEAAPF99                         * 
//*               BACKUP SMFPRM00 TO SMFPRM99                         * 
//*               BACKUP LNKLST00 TO LNKLST99                         * 
//*               BACKUP IEAPAK00 to IEAPAK99                         * 
//* ***************************************************************** * 
//*                                                                     
//SYSPRINT DD  SYSOUT=*                                                 
//MVSRES   DD  UNIT=3350,VOL=SER=MVSRES,DISP=OLD                        
//SYSIN    DD  *                                                        
  RENAME DSNAME=SYS1.PARMLIB,VOL=3350=MVSRES,                          C
               MEMBER=IEASYS00,NEWNAME=IEASYS99                         
  RENAME DSNAME=SYS1.PARMLIB,VOL=3350=MVSRES,                          C
               MEMBER=IEAAPF00,NEWNAME=IEAAPF99                         
  RENAME DSNAME=SYS1.PARMLIB,VOL=3350=MVSRES,                          C
               MEMBER=SMFPRM00,NEWNAME=SMFPRM99                         
  RENAME DSNAME=SYS1.PARMLIB,VOL=3350=MVSRES,                          C
               MEMBER=LNKLST00,NEWNAME=LNKLST99                         
  RENAME DSNAME=SYS1.PARMLIB,VOL=3350=MVSRES,                          C
               MEMBER=IEAPAK00,NEWNAME=IEAPAK99                         
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -BACKUP01 
//*                                                                     
//UPDATE02 EXEC PGM=IEBUPDTE,PARM=NEW                                   
//*                                                                     
//* ***************************************************************** * 
//* SYS1.PARMLIB: CREATE IEASYS00 (IPL PARMAMETERS)                   * 
//*               CREATE IEAAPF00 (AUTHORIZED LOAD LIBRARIES)         * 
//*               CREATE SMFPRM00 (SMF PARAMETERS)                    * 
//*               CREATE IEALOD00 (LPA MODULES FIXED IN REAL STORAGE) * 
//*               CREATE COMMND00 (AUTOMATIC COMMANDS AT IPL)         * 
//*               CREATE SETPFK00 (SET PFKEYS ON CONSOLES AT IPL)     * 
//*               CREATE LNKLST00 (LINKLIST CONCATENATION)            * 
//*               CREATE VATLST00 (VOLUMES MOUNTED AT IPL TIME)       * 
//*               CREATE PARMTZ (TIME OFFSET FROM GMT)                * 
//* ***************************************************************** * 
//*                                                                     
//SYSUT2   DD  DSN=SYS1.PARMLIB,DISP=OLD                                
//SYSPRINT DD  SYSOUT=*                                                 
//SYSIN    DD  *                                                        
./ ADD NAME=IEASYS00,LIST=ALL                                           
./ NUMBER NEW1=10,INCR=10                                               
APF=00,             IEAAPF00 FOR AUTHORIZED PROGRAM LIBRARIES           
BLDLF=00,           IEABLD00 FOR BLDL MODULES                           
CMD=00,             COMMND00 FOR AUTO COMMANDS                          
CSA=2048,           2048*1K BLOCKS FOR COMMON SERVICE AREA              
CVIO,               CLEAR VIO AT IPL                                    
HARDCPY=(015,       SYSLOG HARDCOPY DEVICE ADDRESS,                     
         ALL,         RECORD ALL WTO/WTOR WITH ROUTE CODES,             
         CMDS),       RECORD ALL COMMANDS AND RESPONSES                 
LNK=00,             LNKLST00 FOR LINKLIST CONCATENATION                 
MAXUSER=32,         SYS TASKS+INITIATORS+TSO USERS                      
PAGNUM=(3,2),       ALLOW ADDITION OF 3 PAGE D/S & 2 SWAP D/S           
PAGE=(SYS1.PAGELPA,                      PAGE DATASETS                 C
      SYS1.PAGECSA,                                                    C
      SYS1.PAGEL00,L),                                                  
REAL=128,           128*1K BLOCKS OF VIRTUAL=REAL SPACE                 
SMF=00,             SMFPRM00 FOR SMP PARAMETERS                         
SQA=3,              3*64K SEGMENTS RESERVED FOR SYSTEM QUEUE SPACE      
VAL=00,             VATLST00 FOR VOLUME ATTRIBUTE LIST                  
VRREGN=64           64 BLOCKS OF V=R ALLOCATED TO VRREGN                
./ ADD NAME=IEAAPF00,LIST=ALL                                           
./ NUMBER NEW1=10,INCR=10                                               
 SYS1.VTAMLIB MVSRES,         REQUIRED BY MVS                           
 SYS1.INDMAC MVSRES,          REQUIRED BY MVS                           
 SYS2.LINKLIB MVS000,         USER BATCH LINKLIB                        
 SYSC.LINKLIB SYSCPK          COMPILER/TOOLS VOLUME                     
./ ADD NAME=SMFPRM00,LIST=ALL                                           
./ NUMBER NEW1=10,INCR=10                                               
    OPT=2,      SYSTEM, JOB AND STEP DATA COLLECTION                    
    EXT=YES,    USER EXITS ARE TO BE TAKEN                              
    JWT=30,     MAXIMUM CONTINUOUS WAIT TIME IS 30 MINS                 
    BUF=2000,   A 2000 BYTE BUFFER IS DEFINED                           
    SID=HMVS,   SYSTEM ID IS HMVS                                       
    OPI=NO,     DO NOT PERMIT OPERATOR INTERVENTION AT IPL              
    MAN=ALL     USER AND SYSTEM RECORDS PERMITTED                       
./ ADD NAME=IEALOD00,LIST=ALL                                           
./ NUMBER NEW1=10,INCR=10                                               
HASPSSSM,                        JES2                                   
IEFW21SD                         IEFACTRT                               
./ ADD NAME=COMMND00,LIST=ALL                                           
./ NUMBER NEW1=10,INCR=10                                               
COM='SEND 'AUTO COMMANDS IN COMMND00 BEING PROCESSED',CN=01'            
COM='START JES2'                                                        
COM='START SETPFKEY,M=00'                                               
COM='START ZTIMER'                                                      
./ ADD NAME=SETPFK00,LIST=ALL                                           
./ NUMBER NEW1=10,INCR=10                                               
* ------------------------------------------------------------------- * 
* PROGRAM FUNCTION KEYS FOR CONSOLE 1                                 * 
* ------------------------------------------------------------------- * 
0101N D PFK                                                             
0102                                                                    
0103                                                                    
0104                                                                    
0105                                                                    
0106                                                                    
0107                                                                    
0108                                                                    
0109N D U,TAPE,ONLINE                                                   
0110N D U,DASD,ONLINE                                                   
0111Y V NET,INACT,ID=CUU0C_0;V NET,ACT,ID=CUU0C_0                       
0112N K S,DEL=RD,SEG=19,RTME=001,RNUM=19,CON=N;K A,10                   
* ------------------------------------------------------------------- * 
* PROGRAM FUNCTION KEYS FOR CONSOLE 2                                 * 
* ------------------------------------------------------------------- * 
0201N D PFK                                                             
0202                                                                    
0203                                                                    
0204                                                                    
0205                                                                    
0206                                                                    
0207                                                                    
0208                                                                    
0209N D U,TAPE,ONLINE                                                   
0210N D U,DASD,ONLINE                                                   
0211Y V NET,INACT,ID=CUU0C_0;V NET,ACT,ID=CUU0C_0                       
0212N K S,DEL=RD,SEG=19,RTME=001,RNUM=19,CON=N;K A,10                   
* ------------------------------------------------------------------- * 
* END OF SETPFK00 MEMBER                                              * 
* ------------------------------------------------------------------- * 
./ ADD NAME=LNKLST00,LIST=ALL                                           
./ NUMBER NEW1=10,INCR=10                                               
SYS1.LINKLIB,               IBM LOAD MODULE LIBRARY                     
SYS2.LINKLIB,               USER LOAD MODULE LIBRARY                    
SYS1.CMDLIB,                IBM TSO COMMAND LIBRARY                     
SYS2.CMDLIB                 USER TSO COMMAND LIBRARY                    
./ ADD NAME=VATLST00,LIST=ALL                                           
./ NUMBER NEW1=10,INCR=10                                               
MVSRES,0,2,3350    ,Y        SYSTEM RESIDENCE (PRIVATE)                 
MVS000,0,2,3350    ,Y        SYSTEM DATASETS (PRIVATE)                  
PAGE00,0,2,3350    ,Y        PAGE DATASETS (PRIVATE)                    
PUB000,1,2,3380    ,N        PUBLIC DATASETS (PRIVATE)                  
PUB001,1,2,3390    ,N        PUBLIC DATASETS (PRIVATE)                  
SMP000,1,2,3350    ,N        DISTRIBUTION LIBRARIES (PRIVATE)           
SORTW1,1,1,2314    ,N        SORT WORK (PUBLIC)                         
SORTW2,1,1,2314    ,N        SORT WORK (PUBLIC)                         
SORTW3,1,1,2314    ,N        SORT WORK (PUBLIC)                         
SORTW4,1,1,2314    ,N        SORT WORK (PUBLIC)                         
SORTW5,1,1,2314    ,N        SORT WORK (PUBLIC)                         
SORTW6,1,1,2314    ,N        SORT WORK (PUBLIC)                         
SPOOL1,0,2,3350    ,Y        JES2 QUEUES (PRIVATE)                      
SYSCPK,1,2,3350    ,N        COMPILER/TOOLS (PRIVATE)                   
WORK00,1,0,3350    ,N        WORK PACK (STORAGE)                        
WORK01,1,0,3350    ,N        WORK PACK (STORAGE)                        
./ ADD NAME=PARMTZ,LIST=ALL                                             
./ NUMBER NEW1=10,INCR=10                                               
W,05                   UNITED STATES, CENTRAL TIME ZONE                 
./ ENDUP                                                                
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE02 
//*                                                                     
//ALLOC03  EXEC PGM=IEFBR14                                             
//*                                                                     
//* ***************************************************************** * 
//* DELETE SYS2.LPALIB FROM SMP000                                    * 
//* ALLOCATE USER LINKLIB (SYS2.LINKLIB),                             * 
//*          USER PROCLIB (SYS2.PROCLIB),                             * 
//*          USER TSO COMMAND LIBRARY (SYS2.CMDLIB),  AND             * 
//*          USER TSO HELP LIBRARY (SYS2.HELP) ON MVS000              * 
//* ***************************************************************** * 
/*                                                                      
//LPALIB2  DD  DSN=SYS2.LOCAL.LPALIB,DISP=(OLD,DELETE)                  
//LINKLIB  DD  DSN=SYS2.LINKLIB,                                        
//             UNIT=3350,VOL=SER=MVS000,DISP=(,CATLG),                  
//             SPACE=(CYL,(20,1,323)),                                  
//             DCB=(SYS1.LINKLIB)                                       
//PROCLIB  DD  DSN=SYS2.PROCLIB,DISP=(,CATLG,),                         
//             UNIT=3350,VOL=SER=MVS000,                                
//             SPACE=(CYL,(10,5,50)),                                   
//             DCB=(SYS1.PROCLIB)                                       
//CMDLIB2  DD  DSN=SYS2.CMDLIB,                                         
//             UNIT=3350,VOL=SER=MVS000,DISP=(,CATLG),                  
//             SPACE=(CYL,(20,,100)),                                   
//             DCB=SYS1.CMDLIB                                          
//HELP2    DD  DSN=SYS2.HELP,                                           
//             UNIT=3350,VOL=SER=MVS000,DISP=(,CATLG),                  
//             SPACE=(CYL,(2,1,20)),                                    
//             DCB=SYS1.HELP                                            
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ALLOC03 
//*                                                                     
//BACKUP04 EXEC PGM=IEBGENER                                            
//*                                                                     
//* ***************************************************************** * 
//* SYS1.PROCLIB: BACKUP JES2 TO JES20099                             * 
//* ***************************************************************** * 
//*                                                                     
//SYSPRINT DD  SYSOUT=*                                                 
//SYSUT1   DD  DSN=SYS1.PROCLIB(JES2),DISP=SHR                          
//SYSUT2   DD  DSN=SYS1.PROCLIB(JES20099),DISP=SHR                      
//SYSIN    DD  DUMMY                                                    
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -BACKUP04 
//*                                                                     
//UPDATE05 EXEC PGM=IEBUPDTE,PARM=NEW                                   
//*                                                                     
//* ***************************************************************** * 
//* SYS1.PROCLIB: CREATE JES2 (JES2 STARTUP PROC) TO ADD SYS2.PROCLIB * 
//* ***************************************************************** * 
//*                                                                     
//SYSUT2   DD  DSN=SYS1.PROCLIB,DISP=MOD                                
//SYSPRINT DD  SYSOUT=*                                                 
//SYSIN    DD  DATA,DLM='><'                                            
./ ADD NAME=JES2,LIST=ALL                                               
./ NUMBER NEW1=10,INCR=10                                               
//JES2     PROC M=JES2PM00,                                             
//             N1=SYS1,                                                 
//             N2=SYS2,                                                 
//             L=LINKLIB,                                               
//             U=3350,                                                  
//             P=PARMLIB                                                
//IEFPROC  EXEC PGM=HASJES20,                                           
//             TIME=1440,                                               
//             DPRTY=(15,15)                                            
//STEPLIB  DD  UNIT=&U,DISP=SHR,DSN=&N1..&L                             
//PROC00   DD  DSN=&N1..PROCLIB,DISP=SHR                                
//         DD  DSN=&N2..PROCLIB,DISP=SHR                                
//         DD  DSN=&N1..PROCLIB,DISP=SHR                                
//HASPPARM DD  DSN=&N1..&P(&M),DISP=SHR                                 
//HASPLIST DD  DDNAME=IEFRDER                                           
./ ENDUP                                                                
><                                                                      
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE05 
//*                                                                     
//UPDATE06 EXEC PGM=IEBUPDTE,PARM=NEW                                   
//*                                                                     
//* ***************************************************************** * 
//* SYS2.PROCLIB: ADD CLEARDMP (CLEAR DUMP DATASETS)                  * 
//*                   CLEARERP (CLEAR HARDWARE ERROR DATASET)         * 
//*                   COMPRESS (COMPRESS PDS)                         * 
//*                   SMPASM (ASSEMBLE USERMODS)                      * 
//*                   SMPASML (ASSEMBLE/LINK USERMODS)                * 
//*                   SMPAPP (REJECT/APPLY USERMODS)                  * 
//*                   SMPREC (RECEIVE USERMODS)                       * 
//* ***************************************************************** * 
//*                                                                     
//SYSUT2   DD  DSN=SYS2.PROCLIB,DISP=MOD                                
//SYSPRINT DD  SYSOUT=*                                                 
//SYSIN    DD  DATA,DLM='><'                                            
./ ADD NAME=CLEARDMP,LIST=ALL                                           
./ NUMBER NEW1=10,INCR=10                                               
//CLEARDMP PROC DD=00         SPECIFY DD={00|01|02}                     
//* ***************************************************************** * 
//* CLEAR DUMP DATASET                                                * 
//* ***************************************************************** * 
//EMPTY    EXEC PGM=IEBGENER                                            
//SYSPRINT DD  SYSOUT=*                                                 
//SYSIN    DD  DUMMY                                                    
//SYSUT1   DD  DUMMY,DCB=(RECFM=U,LRECL=10,BLKSIZE=10)                  
//SYSUT2   DD  DISP=SHR,DSN=SYS1.DUMP&DD                                
./ ADD NAME=CLEARERP,LIST=ALL                                           
./ NUMBER NEW1=10,INCR=10                                               
//CLEARERP PROC                                                         
//* ***************************************************************** * 
//* CLEAR ENVIRONMENTAL ERROR RECORDER DATASET                        * 
//* ***************************************************************** * 
//EREP     EXEC PGM=IFCDIP00                                            
//SERERDS  DD  DISP=SHR,DSN=SYS1.LOGREC                                 
./ ADD NAME=COMPRESS,LIST=ALL                                           
./ NUMBER NEW1=10,INCR=10                                               
//COMPRESS PROC LIB='SYS2.LINKLIB',SOUT='*'                             
//* ***************************************************************** * 
//* COMPRESS LIBRARY IN PLACE                                         * 
//* ***************************************************************** * 
//COPY     EXEC PGM=IEBCOPY                                             
//SYSPRINT DD  SYSOUT=&SOUT                                             
//SYSIN    DD  DUMMY                                                    
//SYSUT1   DD  DISP=SHR,DSN=&LIB                                        
//SYSUT2   DD  DISP=SHR,DSN=&LIB                                        
//SYSUT3   DD  UNIT=SYSDA,SPACE=(CYL,(5,5))                             
./ ADD NAME=SMPASM,LIST=ALL                                             
./ NUMBER NEW1=10,INCR=10                                               
//SMPASM   PROC M=MISSING                                               
//* ***************************************************************** * 
//* ASSEMBLE USER MOD                                                 * 
//* ***************************************************************** * 
//ASM      EXEC PGM=IFOX00,                                             
//             REGION=4096K,                                            
//             PARM='LIST,XREF(SHORT),DECK,NOOBJECT'                    
//SYSPRINT DD  SYSOUT=*                                                 
//SYSTERM  DD  SYSOUT=*                                                 
//SYSPUNCH DD  DISP=SHR,DSN=SYS1.UMODOBJ(&M)                            
//SYSLIB   DD  DISP=SHR,DSN=SYS1.MACLIB,DCB=BLKSIZE=32720               
//         DD  DISP=SHR,DSN=SYS1.AMODGEN                                
//         DD  DISP=SHR,DSN=SYS1.UMODMAC                                
//         DD  DISP=SHR,DSN=SYS1.UMODSRC                                
//         DD  DISP=SHR,DSN=SYS1.HASPSRC                                
//         DD  DISP=SHR,DSN=SYS1.APVTMACS                               
//SYSUT1   DD  UNIT=SYSDA,SPACE=(CYL,(2,1))                             
//SYSUT2   DD  UNIT=SYSDA,SPACE=(CYL,(2,1))                             
//SYSUT3   DD  UNIT=SYSDA,SPACE=(CYL,(2,1))                             
//SYSIN    DD  DISP=SHR,DSN=SYS1.UMODSRC(&M)                            
./ ADD NAME=SMPASML,LIST=ALL                                            
./ NUMBER NEW1=10,INCR=10                                               
//SMPASML  PROC M=MISSING                                               
//* ***************************************************************** * 
//* ASSEMBLE AND LINK USER MOD                                        * 
//* ***************************************************************** * 
//ASM      EXEC PGM=IFOX00,                                             
//             REGION=4096K,                                            
//             PARM='LIST,XREF(SHORT),DECK,NOOBJECT'                    
//SYSPRINT DD  SYSOUT=*                                                 
//SYSTERM  DD  SYSOUT=*                                                 
//SYSPUNCH DD  DISP=(,PASS),                                            
//             UNIT=3350,                                               
//             SPACE=(CYL,(1,1)),                                       
//             DCB=(LRECL=80,BLKSIZE=3120,RECFM=FB)                     
//SYSLIB   DD  DISP=SHR,DSN=SYS1.MACLIB,DCB=BLKSIZE=32720               
//         DD  DISP=SHR,DSN=SYS1.AMODGEN                                
//         DD  DISP=SHR,DSN=SYS1.UMODMAC                                
//         DD  DISP=SHR,DSN=SYS1.UMODSRC                                
//         DD  DISP=SHR,DSN=SYS1.HASPSRC                                
//         DD  DISP=SHR,DSN=SYS1.APVTMACS                               
//SYSUT1   DD  UNIT=SYSDA,SPACE=(CYL,(2,1))                             
//SYSUT2   DD  UNIT=SYSDA,SPACE=(CYL,(2,1))                             
//SYSUT3   DD  UNIT=SYSDA,SPACE=(CYL,(2,1))                             
//SYSIN    DD  DISP=SHR,DSN=SYS1.UMODSRC(&M)                            
//LINK     EXEC PGM=IEWL,PARM='LIST,MAP,XREF',REGION=512K               
//SYSPRINT DD  SYSOUT=*                                                 
//SYSLIN   DD  DISP=(OLD,PASS),DSN=*.ASM.SYSPUNCH                       
//         DD  DDNAME=SYSIN                                             
//SYSLIB   DD  DISP=SHR,DSN=SYS1.UMODOBJ                                
//SYSLMOD  DD  DISP=SHR,DSN=SYS1.UMODLIB(&M)                            
./ ADD NAME=SMPAPP                                                      
./ NUMBER NEW1=10,INCR=10                                               
//* ***************************************************************** * 
//* APPLY/RESTORE USER MOD                                            * 
//* ***************************************************************** * 
//SMPAPP  PROC WORK=3350,              WORK UNIT                        
//             TUNIT=3350,             TLIB UNIT                        
//             TVOL=WORK00             TLIB VOLUME                      
//HMASMP  EXEC PGM=HMASMP,PARM='DATE=U',REGION=5120K,TIME=1439          
//SYSUT1   DD  UNIT=&WORK,SPACE=(1700,(600,100))                        
//SYSUT2   DD  UNIT=&WORK,SPACE=(1700,(600,100))                        
//SYSUT3   DD  UNIT=&WORK,SPACE=(1700,(600,100))                        
//SYSUT4   DD  UNIT=&WORK,SPACE=(80,(2,2))                              
//SYSPRINT DD  SYSOUT=*                                                 
//ASMPRINT DD  SYSOUT=*                                                 
//CMPPRINT DD  SYSOUT=*                                                 
//COPPRINT DD  SYSOUT=*                                                 
//LKDPRINT DD  SYSOUT=*                                                 
//E37PRINT DD  SYSOUT=*                                                 
//UPDPRINT DD  SYSOUT=*                                                 
//ZAPPRINT DD  SYSOUT=*                                                 
//*************************** SMP DATASETS *********************        
//SMPOUT   DD  SYSOUT=*                                                 
//SMPLOG   DD  DUMMY                                                    
//SMPTLIB  DD  DISP=OLD,UNIT=&TUNIT,VOL=SER=&TVOL                       
//SYSLIB   DD  DISP=SHR,DSN=SYS1.SMPMTS,DCB=BLKSIZE=32720               
//         DD  DISP=SHR,DSN=SYS1.SMPSTS                                 
//         DD  DISP=SHR,DSN=SYS1.MACLIB                                 
//         DD  DISP=SHR,DSN=SYS1.AMODGEN                                
//         DD  DISP=SHR,DSN=SYS1.AMACLIB                                
//         DD  DISP=SHR,DSN=SYS1.HASPSRC                                
//         DD  DISP=SHR,DSN=SYS1.APVTMACS                               
//SMPACDS  DD  DISP=SHR,DSN=SYS1.SMPACDS                                
//SMPACRQ  DD  DISP=SHR,DSN=SYS1.SMPACRQ                                
//SMPCDS   DD  DISP=SHR,DSN=SYS1.SMPCDS                                 
//SMPCRQ   DD  DISP=SHR,DSN=SYS1.SMPCRQ                                 
//SMPMTS   DD  DISP=SHR,DSN=SYS1.SMPMTS                                 
//SMPPTS   DD  DISP=SHR,DSN=SYS1.SMPPTS                                 
//SMPSTS   DD  DISP=SHR,DSN=SYS1.SMPSTS                                 
//SMPSCDS  DD  DISP=SHR,DSN=SYS1.SMPSCDS                                
//SMPWRK1  DD  UNIT=&WORK,SPACE=(CYL,(5,10,84)),DCB=(BLKSIZE=3120,      
//             LRECL=80)                                                
//SMPWRK2  DD  UNIT=&WORK,SPACE=(CYL,(5,10,84)),DCB=(BLKSIZE=3120,      
//             LRECL=80)                                                
//SMPWRK3  DD  UNIT=&WORK,SPACE=(CYL,(5,10,84)),DCB=(BLKSIZE=3120,      
//             LRECL=80)                                                
//SMPWRK4  DD  UNIT=&WORK,SPACE=(CYL,(1,10,84)),DCB=(BLKSIZE=3120,      
//             LRECL=80)                                                
//SMPWRK5  DD  UNIT=&WORK,SPACE=(CYL,(30,10,250))                       
//*************************** DLIB DATASETS *********************       
//************************* NEEDED ON RESTORE *******************       
//ACMDLIB  DD  DISP=SHR,DSN=SYS1.ACMDLIB                                
//AGENLIB  DD  DISP=SHR,DSN=SYS1.AGENLIB                                
//AHELP    DD  DISP=SHR,DSN=SYS1.AHELP                                  
//AIMAGE   DD  DISP=SHR,DSN=SYS1.AIMAGE                                 
//ALPALIB  DD  DISP=SHR,DSN=SYS1.ALPALIB                                
//AMACLIB  DD  DISP=SHR,DSN=SYS1.AMACLIB                                
//AMODGEN  DD  DISP=SHR,DSN=SYS1.AMODGEN                                
//AOS00    DD  DISP=SHR,DSN=SYS1.AOS00                                  
//AOS03    DD  DISP=SHR,DSN=SYS1.AOS03                                  
//AOS04    DD  DISP=SHR,DSN=SYS1.AOS04                                  
//AOS05    DD  DISP=SHR,DSN=SYS1.AOS05                                  
//AOS06    DD  DISP=SHR,DSN=SYS1.AOS06                                  
//AOS07    DD  DISP=SHR,DSN=SYS1.AOS07                                  
//AOS11    DD  DISP=SHR,DSN=SYS1.AOS11                                  
//AOS12    DD  DISP=SHR,DSN=SYS1.AOS12                                  
//AOS20    DD  DISP=SHR,DSN=SYS1.AOS20                                  
//AOS21    DD  DISP=SHR,DSN=SYS1.AOS21                                  
//AOS24    DD  DISP=SHR,DSN=SYS1.AOS24                                  
//AOS26    DD  DISP=SHR,DSN=SYS1.AOS26                                  
//AOS29    DD  DISP=SHR,DSN=SYS1.AOS29                                  
//AOS32    DD  DISP=SHR,DSN=SYS1.AOS32                                  
//AOSA0    DD  DISP=SHR,DSN=SYS1.AOSA0                                  
//AOSA1    DD  DISP=SHR,DSN=SYS1.AOSA1                                  
//AOSB0    DD  DISP=SHR,DSN=SYS1.AOSB0                                  
//AOSB3    DD  DISP=SHR,DSN=SYS1.AOSB3                                  
//AOSBN    DD  DISP=SHR,DSN=SYS1.AOSBN                                  
//AOSC2    DD  DISP=SHR,DSN=SYS1.AOSC2                                  
//AOSC5    DD  DISP=SHR,DSN=SYS1.AOSC5                                  
//AOSC6    DD  DISP=SHR,DSN=SYS1.AOSC6                                  
//AOSCA    DD  DISP=SHR,DSN=SYS1.AOSCA                                  
//AOSCD    DD  DISP=SHR,DSN=SYS1.AOSCD                                  
//AOSCE    DD  DISP=SHR,DSN=SYS1.AOSCE                                  
//AOSD0    DD  DISP=SHR,DSN=SYS1.AOSD0                                  
//AOSD7    DD  DISP=SHR,DSN=SYS1.AOSD7                                  
//AOSD8    DD  DISP=SHR,DSN=SYS1.AOSD8                                  
//AOSG0    DD  DISP=SHR,DSN=SYS1.AOSG0                                  
//AOSH1    DD  DISP=SHR,DSN=SYS1.AOSH1                                  
//AOSH3    DD  DISP=SHR,DSN=SYS1.AOSH3                                  
//AOST3    DD  DISP=SHR,DSN=SYS1.AOST3                                  
//AOST4    DD  DISP=SHR,DSN=SYS1.AOST4                                  
//AOSU0    DD  DISP=SHR,DSN=SYS1.AOSU0                                  
//APARMLIB DD  DISP=SHR,DSN=SYS1.APARMLIB                               
//APROCLIB DD  DISP=SHR,DSN=SYS1.APROCLIB                               
//ASAMPLIB DD  DISP=SHR,DSN=SYS1.ASAMPLIB                               
//ATCAMMAC DD  DISP=SHR,DSN=SYS1.ATCAMMAC                               
//ATSOMAC  DD  DISP=SHR,DSN=SYS1.ATSOMAC                                
//AUADS    DD  DISP=SHR,DSN=SYS1.AUADS                                  
//HASPSRC  DD  DISP=SHR,DSN=SYS1.HASPSRC                                
//*************************** TARGET DATASETS *******************       
//*************************** NEEDED FOR APPLY ******************       
//CMDLIB   DD  DISP=SHR,DSN=SYS1.CMDLIB                                 
//HELP     DD  DISP=SHR,DSN=SYS1.HELP                                   
//IMAGELIB DD  DISP=SHR,DSN=SYS1.IMAGELIB                               
//IMAGE    DD  DISP=SHR,DSN=SYS1.IMAGELIB                               
//LPALIB   DD  DISP=SHR,DSN=SYS1.LPALIB                                 
//LINKLIB  DD  DISP=SHR,DSN=SYS1.LINKLIB                                
//NUCLEUS  DD  DISP=SHR,DSN=SYS1.NUCLEUS                                
//MACLIB   DD  DISP=SHR,DSN=SYS1.MACLIB                                 
//PARMLIB  DD  DISP=SHR,DSN=SYS1.PARMLIB                                
//PROCLIB  DD  DISP=SHR,DSN=SYS1.PROCLIB                                
//SAMPLIB  DD  DISP=SHR,DSN=SYS1.SAMPLIB                                
//SVCLIB   DD  DISP=SHR,DSN=SYS1.SVCLIB                                 
//TCOMMAC  DD  DISP=SHR,DSN=SYS1.TCOMMAC                                
//TELCMLIB DD  DISP=SHR,DSN=SYS1.TELCMLIB                               
//UADS     DD  DISP=SHR,DSN=SYS1.UADS                                   
//UMODLIB  DD  DISP=SHR,DSN=SYS1.UMODLIB                                
//UMODOBJ  DD  DISP=SHR,DSN=SYS1.UMODOBJ                                
//VTAMLIB  DD  DISP=SHR,DSN=SYS1.VTAMLIB                                
./ ADD NAME=SMPREC                                                      
./ NUMBER NEW1=10,INCR=10                                               
//SMPREC  PROC WORK=3350,              WORK UNIT                        
//             TUNIT=3350,             TLIB UNIT                        
//             TVOL=WORK00             TLIB VOLUME                      
//* ***************************************************************** * 
//* RECEIVE USER MOD                                                  * 
//* ***************************************************************** * 
//HMASMP  EXEC PGM=HMASMP,PARM='DATE=U',REGION=5120K,TIME=1440          
//SYSUT1   DD  UNIT=&WORK,SPACE=(1700,(600,100))                        
//SYSUT2   DD  UNIT=&WORK,SPACE=(1700,(600,100))                        
//SYSUT3   DD  UNIT=&WORK,SPACE=(1700,(600,100))                        
//SYSUT4   DD  UNIT=&WORK,SPACE=(80,(2,2))                              
//SYSPRINT DD  SYSOUT=*                                                 
//ASMPRINT DD  SYSOUT=*                                                 
//CMPPRINT DD  SYSOUT=*                                                 
//COPPRINT DD  SYSOUT=*                                                 
//LKDPRINT DD  SYSOUT=*                                                 
//E37PRINT DD  SYSOUT=*                                                 
//UPDPRINT DD  SYSOUT=*                                                 
//ZAPPRINT DD  SYSOUT=*                                                 
//*************************** SMP DATASETS *********************        
//SMPOUT   DD  SYSOUT=*                                                 
//SMPLOG   DD  DUMMY                                                    
//SMPTLIB  DD  DISP=OLD,UNIT=&TUNIT,VOL=SER=&TVOL                       
//SYSLIB   DD  DISP=SHR,DSN=SYS1.SMPMTS,DCB=BLKSIZE=32720               
//         DD  DISP=SHR,DSN=SYS1.SMPSTS                                 
//         DD  DISP=SHR,DSN=SYS1.MACLIB                                 
//         DD  DISP=SHR,DSN=SYS1.AMODGEN                                
//         DD  DISP=SHR,DSN=SYS1.AMACLIB                                
//         DD  DISP=SHR,DSN=SYS1.HASPSRC                                
//         DD  DISP=SHR,DSN=SYS1.APVTMACS                               
//SMPACDS  DD  DISP=SHR,DSN=SYS1.SMPACDS                                
//SMPACRQ  DD  DISP=SHR,DSN=SYS1.SMPACRQ                                
//SMPCDS   DD  DISP=SHR,DSN=SYS1.SMPCDS                                 
//SMPCRQ   DD  DISP=SHR,DSN=SYS1.SMPCRQ                                 
//SMPMTS   DD  DISP=SHR,DSN=SYS1.SMPMTS                                 
//SMPPTS   DD  DISP=SHR,DSN=SYS1.SMPPTS                                 
//SMPSTS   DD  DISP=SHR,DSN=SYS1.SMPSTS                                 
//SMPSCDS  DD  DISP=SHR,DSN=SYS1.SMPSCDS                                
//SMPWRK1  DD  UNIT=&WORK,SPACE=(CYL,(5,10,84)),DCB=(BLKSIZE=3120,      
//             LRECL=80)                                                
//SMPWRK2  DD  UNIT=&WORK,SPACE=(CYL,(5,10,84)),DCB=(BLKSIZE=3120,      
//             LRECL=80)                                                
//SMPWRK3  DD  UNIT=&WORK,SPACE=(CYL,(5,10,84)),DCB=(BLKSIZE=3120,      
//             LRECL=80)                                                
//SMPWRK4  DD  UNIT=&WORK,SPACE=(CYL,(1,10,84)),DCB=(BLKSIZE=3120,      
//             LRECL=80)                                                
//SMPWRK5  DD  UNIT=&WORK,SPACE=(CYL,(30,10,250))                       
./ ENDUP                                                                
><                                                                      
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE06 
//*                                                                     
//COPY07   EXEC PGM=IEBCOPY,PARM=NEW                                    
//* ***************************************************************** * 
//* ALLOCATE DATASETS FOR ADDING USER MODS                            * 
//* COPY SMP DATASETS CREATING ENVIRONMENT FOR TARGET SYSTEM          * 
//* ***************************************************************** * 
//UMODMAC  DD  DISP=(NEW,CATLG,DELETE),                                 
//             DSN=SYS1.UMODMAC,                                        
//             UNIT=3350,VOL=SER=SMP000,                                
//             SPACE=(CYL,(1,1,10)),                                    
//             DCB=SYS1.MACLIB                                          
//UMODCNTL DD  DISP=(NEW,CATLG,DELETE),                                 
//             DSN=SYS1.UMODCNTL,                                       
//             UNIT=3350,VOL=SER=SMP000,                                
//             SPACE=(CYL,(1,1,10)),                                    
//             DCB=SYS1.MACLIB                                          
//UMODSRC  DD  DISP=(NEW,CATLG,DELETE),                                 
//             DSN=SYS1.UMODSRC,                                        
//             UNIT=3350,VOL=SER=SMP000,                                
//             SPACE=(CYL,(1,1,10)),                                    
//             DCB=SYS1.MACLIB                                          
//UMODOBJ  DD  DISP=(NEW,CATLG,DELETE),                                 
//             DSN=SYS1.UMODOBJ,                                        
//             UNIT=3350,VOL=SER=SMP000,                                
//             SPACE=(CYL,(1,1,10)),                                    
//             DCB=(LRECL=80,BLKSIZE=3120,RECFM=FB)                     
//UMODLIB  DD  DISP=(NEW,CATLG,DELETE),                                 
//             DSN=SYS1.UMODLIB,                                        
//             UNIT=3350,VOL=SER=SMP000,                                
//             SPACE=(CYL,(1,1,10)),                                    
//             DCB=SYS1.LINKLIB                                         
//SMPACDS  DD  DISP=SHR,DSN=SYS1.SMPACDS                                
//SMPACRQ  DD  DISP=SHR,DSN=SYS1.SMPACRQ                                
//SMPCDS   DD  UNIT=3350,DSN=SYS1.SMPCDS,DISP=(,CATLG),                 
//             VOL=SER=SMP000,SPACE=(TRK,(780,30,4000)),                
//             DCB=SYS1.SMPACDS                                         
//SMPCRQ   DD  UNIT=3350,DSN=SYS1.SMPCRQ,DISP=(,CATLG),                 
//             VOL=SER=SMP000,SPACE=(TRK,(150,30,200)),                 
//             DCB=SYS1.SMPACRQ                                         
//SMPSCDS  DD  UNIT=3350,DSN=SYS1.SMPSCDS,DISP=(,CATLG),                
//             VOL=SER=SMP000,SPACE=(TRK,(30,30,75)),                   
//             DCB=(RECFM=FB,BLKSIZE=3120,LRECL=80)                     
//SYSUT3   DD  UNIT=3350,SPACE=(CYL,(20,10))                            
//SYSUT4   DD  UNIT=3350,SPACE=(CYL,(20,10))                            
//SYSPRINT DD  SYSOUT=*                                                 
//SYSIN    DD  *                                                        
 COPY INDD=SMPACDS,OUTDD=SMPCDS,LIST=NO                                 
 COPY INDD=SMPACRQ,OUTDD=SMPCRQ,LIST=NO                                 
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -COPY07 
//*                                                                     
//UPDATE08 EXEC PGM=IEBUPDTE,PARM=NEW                                   
//* ***************************************************************** * 
//* ADD SMPASM, SMPASML, SMPAPP, AND SMPREC (THAT WERE JUST ADDED TO  * 
//* SYS2.PROCLIB ABOVE) INTO SYS1.PROCLIB ON THE STARTER SYSTEM.      * 
//* ***************************************************************** * 
//SYSUT2   DD  DSN=SYS1.PROCLIB,DISP=MOD,                               
//             UNIT=3330,VOL=SER=START1                                 
//SYSPRINT DD  SYSOUT=*                                                 
//SYSIN    DD  *                                                        
./ ADD NAME=SMPASM,LIST=ALL                                             
//         DD  DISP=SHR,DSN=SYS2.PROCLIB(SMPASM)                        
//         DD  *                                                        
./ ADD NAME=SMPASML,LIST=ALL                                            
//         DD  DISP=SHR,DSN=SYS2.PROCLIB(SMPASML)                       
//         DD  *                                                        
./ ADD NAME=SMPAPP,LIST=ALL                                             
//         DD  DISP=SHR,DSN=SYS2.PROCLIB(SMPAPP)                        
//         DD  *                                                        
./ ADD NAME=SMPREC,LIST=ALL                                             
//         DD  DISP=SHR,DSN=SYS2.PROCLIB(SMPREC)                        
//         DD  *                                                        
./ ENDUP                                                                
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE08 
//*                                                                     
//UPDATE09 EXEC PGM=IEBUPDTE,PARM=NEW                                   
//*                                                                     
//* ***************************************************************** * 
//* Add macro IHADVCT (Device Table Characteristics) to SYS1.MACLIB   * 
//* on MVSRES. The source of this macro is the SYM6-1 dataset from    * 
//* the optional materials. It is necessary for some USERMODs to      * 
//* assemble properly. (Reference:                                    * 
//* http://www.jaymoseley.com/hercules/installMVS/mvssource.htm)      * 
//* ***************************************************************** * 
//*                                                                     
//SYSUT2   DD  DSN=SYS1.MACLIB,DISP=MOD                                 
//SYSPRINT DD  SYSOUT=*                                                 
//SYSIN DD DATA,DLM=')('                                                
./ ADD NAME=IHADVCT                                                     
*%IF DVCIBASE = ''                      /* IF NO DVCTI BASE SPECIFIED*/ 
*%THEN %DVCIBASE = 'BASED(CVTZDTAB)';   /* USE CVTZDTAB              */ 
*%IF DVCTBASE = ''                      /* IF NO DVCT BASE SPECIFIED */ 
*                                       /* USE UCBTYP TO INDEX DVCTI */ 
*                                       /* AND USE CVTZDTAB + OFFSET */ 
*%THEN %DVCTBASE = 'BASED(CVTZDTAB+DVCTIOFF(UCBTYP & DVCTYPMK))';       
*%;/*                                                                   
         MACRO                                                          
         IHADVCT   &DSECT=YES                                           
.* /* START OF SPECIFICATIONS ****                                      
.*                                                                      
.*01  MODULE-NAME = IHADVCT                                             
.*                                                                      
.*01  COPYRIGHT = NONE                                                  
.*                                                                      
.*01  STATUS = OS/VS2 RELEASE 2, LEVEL 0                                
.*                                                                      
.*01  CHANGE-ACTIVITY = NONE                                            
.*                                                                      
.*01  DESCRIPTIVE-NAME = DEVICE CHARACTERISTICS TABLE MAPPING           
.*                                                                      
.*01  FUNCTION = THIS TABLE DESCRIBES PHYSICAL ATTRIBUTES OF EACH DASD  
.*    DEVICE WHICH HAS BEEN SYSGENED                                    
.*                                                                      
.*01  NOTES = THE TABLE IS POINTED TO BY CVTZDTAB.                      
.*                                                                      
.*01  MODULE-TYPE = MACRO                                               
.*02    PROCESSOR = ASSEMBLER-370R                                      
.*                                                                      
.*02    MACRO-SIZE = 200 STATEMENTS                                     
.*                                                                      
.**** END OF SPECIFICATIONS ***/                                        
* /* MACCOMP Y-2 SC1D0/SJD48                                         */ 
* /* MACSTAT Y-2 73226/021160                                        */ 
*/********************************************************************/ 
*/*                 DEVICE CHARACTERISTICS TABLE                     */ 
*/********************************************************************/ 
*/*                                                                  */ 
*/*         THIS TABLE MAY BE USED TO FIND THE CHARACTERISTICS       */ 
*/*         OF DIRECT ACCESS DEVICES.  THE APPLICABLE DEVICES ARE    */ 
*/*         THOSE CONTAINING UCBDACC IN UCBTBYT3 (SEE IEFUCBOB).     */ 
*/*                                                                  */ 
*/*         NOTE: DEVTYPE MAY BE USED TO EXTRACT INFORMATION         */ 
*/*         FROM THIS TABLE.  ITS OUTPUT AREA IS IN A SLIGHTLY       */ 
*/*         DIFFERENT FORMAT, AND IS MAPPED BY IHADVA.               */ 
*/*                                                                  */ 
*/*         THE TABLE IS COMPOSED OF AN INDEX FOLLOWED BY ONE        */ 
*/*         ENTRY FOR EACH DASD DEVICE WHICH HAS BEEN SYSGENED       */ 
*/*                                                                  */ 
*/*         FOR ASSEMBLER USE, TWO SEPARATE DSECTS ARE PROVIDED.     */ 
*/*         A USING ON DVCTI GIVES ADDRESSIBILITY TO THE INDEX,      */ 
*/*         AND A USING ON DVCT GIVES ADDRESSIBILITY TO AN ENTRY.    */ 
*/*         SPECIFYING DSECT=NO SUPPRESSES THE INDEX AND PROVIDES    */ 
*/*         AN ENTRY DESCRIPTION WITHOUT A DSECT STATEMENT           */ 
*/*                                                                  */ 
*/*         FOR PLS USE, TWO STRUCTURES ARE PROVIDED. THEIR STORAGE  */ 
*/*         ATTRIBUTES ARE CONTROLLED BY SETTING STRING MACRO        */ 
*/*         VARIABLES AS FOLLOWS:                                    */ 
*/*         STRUCTURE  MACRO-VAR       DEFAULT SETTING               */ 
*/*         DVCTI      %DVCIBASE  'BASED(CVTZDTAB)'                  */ 
*/*         DVCT       %DVCTBASE  'BASED(CVTZDTAB                    */ 
*/*                                +DVCTIOFF(UCBTYP&DVCTYPMK))'      */ 
*/*                                                                  */ 
*/*         THE DEFAULT SETTINGS WILL PROVIDE ADDRESSIBILITY TO      */ 
*/*         ALL FIELDS, BUT DEPEND ON CVT AND UCB ADDRESSIBILITY.    */ 
*/*                                                                  */ 
*/*               FORMAT OF EACH ENTRY                               */ 
*/*         _____________________________________________            */ 
*/*   0(00) |                     |                     |            */ 
*/*         |       DVCCYL        |       DVCTRK        |            */ 
*/*         |_____________________|_____________________|            */ 
*/*   4(04) |                     |       DVCOVHD       |            */ 
*/*         |       DVCTRKLN      | DVCOVNLB | DVCOVLB  |            */ 
*/*         |_____________________|__________|__________|            */ 
*/*   8(08) |          |          |                     |            */ 
*/*         | DVCOVNK  | DVCFLAGS |       DVCTOL        |            */ 
*/*         |__________|__________|_____________________|            */ 
*/*  12(0C) |                     |______________________            */ 
*/*         |       DVCALT        |                     |            */ 
*/*         |_____________________|       DVCOVR0       |  RPS       */ 
*/*         ______________________|_____________________|  ONLY      */ 
*/*  16(10) |          |          |                        SECTION   */ 
*/*         | DVCSECT  | DVCSECTD |                                  */ 
*/*         |__________|__________|                                  */ 
*/********************************************************************/ 
*%/*                                                                    
         AIF   ('&DSECT' EQ 'NO').NODSECT                               
DVCTI    DSECT ,              INDEX TO DVCT                             
*              THIS INDEX IS LOCATED FROM CVTZDTAB.                     
*              THE PROPER ENTRY IS FOUND BY ADDING THE LOW ORDER        
*              4 BITS OF UCBTYP TO THE ADDRESS IN CVTZDTAB.             
DVCTYPMK EQU   X'0000000F'              TYPICAL USAGE:                  
*              LA    RWRK,DVCTYPMK      MASK FOR UNIT TYPE NUMBER       
*              N     RWRK,UCBTYP        PICK UP UNIT TYPE NUMBER        
*              IC    RWRK,DVCTIOFF(RWRK)  PICK UP OFFSET                
DVCTIOFF DS    AL1                      OFFSET TO DVCT ENTRY            
*********************************************************************** 
         SPACE 3                                                        
DVCT     DSECT ,                        FORMAT OF DVCT ENTRY            
*              THE ENTRY IS LOCATED BY ADDING DVCTIOFF TO CVTZDTAB      
         AGO   .ENTRY                                                   
.NODSECT ANOP                                                           
DVCT     DS    0H                       FORMAT OF DVCT ENTRY            
.ENTRY   ANOP                                                           
*                                                                       
DVCCYL   DS    H                        PHYS NO. CYL PER VOLUME         
DVCTRK   DS    H                        NO. TRACKS PER CYLINDER         
DVCTRKLN DS    H                        NO. OF BYTES PER TRACK          
*                                                                       
DVCOVHD  DS    0H                       BLOCK OVERHEAD IF DVC2BOV=1     
*              USE FOLLOWING TWO CONSTANTS IF DVC2BOV=0                 
DVCOVNLB DS    XL1                      OVERHEAD NOT LAST BLOCK         
DVCOVLB  DS    XL1                      OVERHEAD LAST BLOCK             
*                                                                       
DVCOVNK  DS    XL1                      OVERHEAD DECREMENT NOT KEYED    
*                                                                       
DVCFLAGS DS    BL1                                                      
DVC2BOV  EQU   X'08'                    IF 1, USE DVCOVHD               
*                                       IF 0, USE DVCOVNLB,DVCOVLB      
DVCFTOL  EQU   X'01'                    IF 1, APPLY TOLERANCE FACTOR    
*                                                                       
DVCTOL   DS    H                        TOLERANCE FACTOR                
*              APPLY TOLERANCE FACTOR AS FOLLOWS:                       
*              1. ADD BLOCKSIZE AND KEYLENGTH                           
*              2. MULTIPLY BY DVCTOL                                    
*              3. SHIFT RIGHT DVCTSHFT BITS                             
*              4. ADD APPROPRIATE OVERHEADS                             
DVCTSHFT EQU   9                        SHIFT AMT TO DIVIDE BY 512      
*                                                                       
DVCALT   DS    H                        NUMBER ALTERNATE TRKS/VOLUME    
*                                                                       
DVCENTLG EQU   *-DVCT                   BASIC SIZE OF DEVICE TABLE      
*                                       ENTRY, NOT INCLUDING ADD'L      
*                                       CHARACTERISTICS FOR RPS         
**********************************************************************  
*              THE FOLLOWING SECTION OF THE TABLE IS PRESENT         *  
*              ONLY FOR RPS DEVICES--TEST UCBTBYT2 FOR UCB2OPT3      *  
**********************************************************************  
DVCRPS   DS    0CL4                     RPS SECTION                     
DVCOVR0  DS    H                        OVERHEAD BYTES FOR RECORD 0     
DVCSECT  DS    XL1                      NUMBER SECTORS IN FULL TRACK    
DVCSECTD DS    XL1                      NUMBER DATA SECTORS             
*                                                                       
*              END OF DVCT                                              
         MEND                                                           
**/;                                                                    
*                                                                       
* DCL  1 DVCTI DVCIBASE,                /* INDEX TO DVCT             */ 
*        2 *   PTR(8),                  /* OFFSET TO ENTRY 0         */ 
*        2 DVCTIOFF (*) PTR(8);         /* OFFSETS TO ENTRIES 1 TO N */ 
*/*                                                                  */ 
*/*         USE THE LAST 4 BITS OF UCBTYP TO INDEX DVCTIOFF.         */ 
*/*         DVCTYPMK MAY BE USED AS A MASK TO 'AND' WITH UCBTYP.     */ 
*/*         THE INDEX ENTRIES ARE OFFSETS RELATIVE TO CVTZDTAB.      */ 
*/********************************************************************/ 
*                                                                       
* DCL  1 DVCT DVCTBASE,                 /* FORMAT OF DVCT ENTRY      */ 
*        2 DVCCYL FIXED(15) UNSIGNED,   /* PHYS NO. CYL PER VOLUME   */ 
*        2 DVCTRK FIXED(15) UNSIGNED,   /* NO. TRACKS PER CYLINDER   */ 
*        2 DVCTRKLN FIXED(15) UNSIGNED, /* NO. BYTES PER TRACK       */ 
*                                                                       
*        2 DVCOVHD FIXED(15),           /* BLOCK OVERHD IF DVC2BOV=1 */ 
*          /* USE THE FOLLOWING TWO CONSTANTS IF DVC2BOV=0           */ 
*          3 DVCOVNLB FIXED(8),         /* OVERHEAD NOT LAST BLOCK   */ 
*          3 DVCOVLB FIXED(8),          /* OVERHEAD FOR LAST BLOCK   */ 
*                                                                       
*        2 DVCOVNK FIXED(8),            /* OVERHD DECREMENT NOT KEYED*/ 
*                                                                       
*        2 DVCFLAGS BIT(8),                                             
*          3 * BIT(4),                  /* RESERVED                  */ 
*          3 DVC2BOV BIT(1),            /* IF 1, USE DVCOVHD         */ 
*                                       /* IF 0, USE DVCOVNLB,OVLB   */ 
*          3 * BIT(2),                  /* RESERVED                  */ 
*          3 DVCFTOL BIT(1),            /* IF 1, APPLY TOLER FACTOR  */ 
*                                                                       
*        2 DVCTOL FIXED(15) UNSIGNED,   /* TOLERANCE FACTOR          */ 
*/*         APPLY TOLERANCE AS FOLLOWS:                              */ 
*/*         (BLOCKSIZE+KEYLENGTH) * DVCTOL / DVCTSHFT + OVERHEADS    */ 
*                                                                       
*        2 DVCALT FIXED(15),            /* NO. OF ALTERNATE TRACKS   */ 
*/********************************************************************/ 
*/*         THE FOLLOWING SETION OF THE TABLE IS PRESENT             */ 
*/*         ONLY FOR RPS DEVICES (UCB2OPT3=1)                        */ 
*/********************************************************************/ 
*        2 DVCRPS,                      /*RPS SECTION                */ 
*          3 DVCOVR0 FIXED(15),         /* OVERHD BYTES FOR RECORD 0 */ 
*          3 DVCSECT FIXED(8),          /* NO. SECTORS IN FULL TRACK */ 
*          3 DVCSECTD FIXED(8);         /* NO. OF DATA SECTORS       */ 
*/********************************************************************/ 
*                                                                       
* DCL    DVCTSHFT FIXED(15) CONSTANT(512); /* DENOMINATOR FOR DVCTOL */ 
*/*         THE FOLLOWING CONSTANT CAN BE USED TO MASK OUT ALL BUT   */ 
*/*         THE DVCTIOFF SUBSCRIPT FROM THE UCB WORD UCBTYP          */ 
* DCL    DVCTYPMK BIT(32) CONSTANT('0000000F'X); /* UCBTYP MASK      */ 
*                                                                       
*/*               END OF DVCT                                        */ 
./ ENDUP                                                                
)(                                                                      
//*                                                                     
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE09 
//*                                                                     
//SUBMIT10 EXEC PGM=IEBGENER,COND=(0,NE)                                
//*                                                                     
//* ***************************************************************** * 
//* SUBMIT JOB TO ESTABLISH MVS3.8j SYSTEM CONTENTS IN SMP.           * 
//* ***************************************************************** * 
//*                                                                     
//SYSIN    DD  DUMMY                                                    
//SYSPRINT DD  DUMMY                                                    
//SYSUT1   DD  DATA,DLM='@@'                                            
//SYSGEN04 JOB 'JCLIN-STAGE1/JES2',                                     
//             CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1)                        
/*JOBPARM LINES=100                                                     
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR                    
//*                                                                     
//SMPAPP   EXEC SMPAPP                                                  
//* ***************************************************************** * 
//* RUN JCLIN WITH STAGE2 JOBSTREAM + JES2 LINK JOBSTREAM.            * 
//* ***************************************************************** * 
//SMPCNTL  DD  *                                                        
  JCLIN .                                                               
  LIST CDS SYS XREF .                                                   
//SMPJCLIN DD  DISP=SHR,DSN=SYS1.STAGE1.OUTPUT,                         
//             UNIT=3350,VOL=SER=WORK01                                 
//         DD  DATA,DLM='><'                                            
//JES2    EXEC PGM=IEWL,PARM='XREF,LET,LIST,NCAL'                       
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
><                                                                      
//*                                                                     
//*        END OF SUBMITTED SYSGEN04                                    
@@                                                                      
//SYSUT2   DD  SYSOUT=(A,INTRDR)                                        
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -SUBMIT10 
//*                                                                     
//UPDATE11 EXEC PGM=IEBUPDTE,PARM=NEW                                   
//*                                                                     
//* ***************************************************************** * 
//* INSTALLS 'HELP' FOR TSO COMMAND CLS INTO SYS2.HELP                * 
//* ***************************************************************** * 
//*                                                                     
//SYSPRINT DD  SYSOUT=*                                                 
//SYSUT2   DD  DISP=SHR,DSN=SYS2.HELP                                   
//SYSIN    DD  *                                                        
./ ADD NAME=CLS,LIST=ALL                                                
./ NUMBER NEW1=10,INCR=10                                               
)F CLS  FUNCTIONS -                                                     
  CLEAR THE SCREEN ON A 3270 TYPE TERMINAL                              
)X SYNTAX -                                                             
         CLS                                                            
)O OPERANDS -                                                           
  NO OPERANDS ON CLS COMMAND.                                           
./ ENDUP                                                                
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE11 
//*                                                                     
//ASMFCL12 EXEC ASMFCL,PARM.ASM='LIST,NODECK,OBJ,TERM,NOXREF'           
//*                                                                     
//* ***************************************************************** * 
//* INSTALLS TSO COMMAND CLS INTO SYS2.CMDLIB                         * 
//* ***************************************************************** * 
//*                                                                     
//ASM.SYSIN DD *                                                        
*                              SOURCE: CBT V430 FILE 300                
         TITLE 'CLRSCRN'                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
         SPACE 1                                                        
CLRSCRN  CSECT                                                          
         USING CLRSCRN,R15                                              
         ST    R14,RETURN              SAVE                             
         LR    R14,R15                 BASE REGISTER                    
         DROP  R15                                                      
         USING CLRSCRN,R14                                              
         GTSIZE                                                         
         LA    R15,20                  RETURN CODE 20 - NOT 3270 DEVICE 
         LTR   R0,R0                   Q. 3270 TERMINAL                 
         BZ    EXIT                    ...NO                            
         MR    R0,R0                   SCREEN END = LINES * LINE LENGTH 
         BCTR  R1,0                    ADJUST RELATIVE                  
         D     R0,=F'64'               MODULO 64                        
         N     R1,=X'0000003F'         INSURANCE                        
         STC   R0,SCREND+1             SCREEN OFFSET MODULO 64 COLUMN   
         STC   R1,SCREND               SCREEN OFFSET MODULO 64 ROW      
         TR    SCREND,SCRPOS           TRANSLATE TO 3270 ADDRESS        
         TPUT  SCRCNTL,LSCRCNTL,FULLSCR,,HOLD                           
EXIT     DS    0H                                                       
         L     R14,RETURN                                               
         BR    14                      RETURN                           
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
         TITLE 'CLRSCRN - DATA AREA'                                    
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
         SPACE 1                                                        
RETURN   DS    F                                                        
SCRCNTL  DS    0CL11                   3270 FULL SCREEN CONTROL         
         DC    AL1(WCC)                3270 WRITE CONTROL               
         DC    AL1(SBA)                3270 SET BUFFER ADDRESS COMMAND  
         DC    X'4040'                 3270 ADDRESS 0                   
         DC    AL1(IC)                 INSERT CURSOR                    
         DC    AL1(RA)                 3270 REPEAT TO ADDRESS COMMAND   
SCREND   DC    X'5D7F'                 3270 ADDRESS - REPEAT END        
         DC    X'00'                   REPEAT CHARACTER                 
LSCRCNTL EQU   *-SCRCNTL                                                
SCRPOS   DS    0H                      MODULO 64 TRANSLATE TABLE        
         DC    X'40C1C2C3C4C5C6C7C8C94A4B4C4D4E4F'                      
         DC    X'50D1D2D3D4D5D6D7D8D95A5B5C5D5E5F'                      
         DC    X'6061E2E3E4E5E6E7E8E96A6B6C6D6E6F'                      
         DC    X'F0F1F2F3F4F5F6F7F8F97A7B7C7D7E7F'                      
         LTORG                                                          
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
WCC      EQU   X'C3'                   KEYBOARD RESTORE,                
*                                          RESET MODIFIED DATA TAG      
SF       EQU   X'1D'                   START FIELD                      
SBA      EQU   X'11'                   SET BUFFER ADDRESS               
IC       EQU   X'13'                   INSERT CURSOR                    
PT       EQU   X'05'                   PROGRAM TAB                      
RA       EQU   X'3C'                   REPEATE TO ADDRESS               
EUA      EQU   X'12'                   ERASE UNPROTECTED TO ADDRESS     
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
         END                                                            
//*                                                                     
//LKED.SYSLMOD DD DSN=SYS2.CMDLIB,DISP=SHR                              
//LKED.SYSIN   DD *                                                     
  ALIAS CLS                            12/2014 JLM                      
  NAME CLRSCRN(R)                      12/2014 JLM                      
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -ASMFCL12 
//*                                                                     
//ASMFCL13 EXEC ASMFCL,PARM.ASM='LIST,NODECK,OBJ,TERM,NOXREF',          
//             MAC1='SYS1.AMODGEN'                                      
//*                                                                     
//* ***************************************************************** * 
//* INSTALLS SETPFKEY INTO SYS2.LINKLIB                               * 
//* ***************************************************************** * 
//*                                                                     
//ASM.SYSIN DD *                                                        
*                              SOURCE: CBT V249 FILE 295                
*          DATA SET CBT1115    AT LEVEL 005 AS OF 10/23/80              
         PRINT OFF                                                      
         MACRO                                                          
&NAME    INIT  &RG=1,&PC=,&REQ=,&PATCH=,&SAVE=,&PARM=,&PGM=,&ENTRY      
         GBLB  &ACSC1(8)                                                
         GBLA  &ACSC2(4)                                                
         GBLC  &ACSC3(4)                                                
         LCLA  &A1,&A2,&A3,&A4                                          
         SPACE 2                                                        
*********************************************************************** 
*        INIT MACRO - PROGRAM ENTRY AND HOUSEKEEPING                  * 
*********************************************************************** 
         SPACE 2                                                        
         AIF   (T'&REQ EQ 'O').REGS                                     
         AIF   ('&REQ' EQ 'NO').EREGS                                   
         MNOTE 0,'INVALID ''REQ'' PARAM, NO ASSUMED'                    
         AGO   .EREGS                                                   
.REGS    ANOP                                                           
.* ISSUE EQUR MACRO FOR REGISTER EQUATED SYMBOLS                        
         EQUATE                                                         
.EREGS   ANOP                                                           
.* CHECK PC AND SET APPROPRIATE SWITCH                                  
         AIF   (T'&PC EQ 'O').NOPCX                                     
         AIF   ('&PC' NE 'ARM' AND '&PC' NE 'YES').NOPC                 
&ACSC1(1) SETB 1                                                        
         AGO   .NOPCX                                                   
.NOPC    ANOP                                                           
         MNOTE 0,'INVALID PC, IGNORED'                                  
.NOPCX   AIF   (T'&ENTRY EQ 'O').NOENTRY                                
         AIF   ('&ENTRY' EQ 'ENTRY').ENTOK                              
         MNOTE 4,'INVALID POSITIONAL OPERAND, NO ENTRY GENERATED'       
         AGO   .NOENTRY                                                 
.ENTOK   ANOP                                                           
         ENTRY &NAME                                                    
.NOENTRY ANOP                                                           
&NAME    DS    0H                                                       
*---------------------------------------------------------------------* 
*        ISSUE SAVE MACRO                                             * 
*---------------------------------------------------------------------* 
         SAVE  (14,12),,&SYSECT-&SYSDATE-&SYSTIME                       
         SPACE 2                                                        
*---------------------------------------------------------------------* 
*        SAVE PARM, GET SAVE AREA, SET UP BASE REGS                   * 
*---------------------------------------------------------------------* 
         SPACE 2                                                        
         LR    2,1                      SAVE PASSED PARAMS              
.NPARM1  AIF   (T'&SAVE EQ 'O').NOSAVE                                  
         USING &NAME,15                 SET UP BASE                     
&ACSC3(1) SETC '&SAVE'                  SAVE LENGTH                     
         LA    0,&SAVE+72+&ACSC1(1)*4   SET GETMAIN LENGTH              
* ISSUE GETMAIN FOR SAVE AREA AND WORK SPACE                            
         GETMAIN R,LV=(0)                                               
.CHAIN   ST    13,4(1)                  SAVE BACKWARD POINTER           
         ST    1,8(13)                  SAVE FORWARD POINTER            
         LR    13,1                     SET SAVE AREA                   
         DROP  15                                                       
         AGO   .ADDRS                                                   
.NOSAVE  USING &NAME,15                 SET UP BASE                     
         CNOP  0,4                      SET ON BOUNDRY                  
         BAL   1,*+76+&ACSC1(1)*4       SET REG SAVE PLUS WORK AREA     
         USING *,13                                                     
         DS    18F                      SAVE AREA                       
         AIF   (NOT &ACSC1(1)).CHAIN                                    
         DS    F                        SPIE SAVE AREA                  
         AGO   .CHAIN                                                   
.ADDRS   AIF   (T'&SAVE EQ 'O').NSAV1                                   
         AIF   (T'&RG NE 'O').OKBASE                                    
         MNOTE 4,'YOU REQUESTED NO BASE REGISTERS, WILL GIVE YOU ONE'   
&A1      SETA  1                                                        
         AGO   .NOBASE                                                  
.OKBASE  ANOP                                                           
&A1      SETA  &RG                                                      
.NOBASE  ANOP                                                           
&A2      SETA  11                                                       
&A3      SETA  1                                                        
&A4      SETA  0                                                        
         BALR  12,0                     SET BASE REG                    
         USING *,12                                                     
.ADRLP   ANOP                                                           
&A1      SETA  &A1-1                                                    
         AIF   (&A1 EQ 0).EADDR                                         
         LA    &A2,4095(&A2+1)          SET MORE BASES                  
         USING *+4095*&A3-&A4-4*&A3-&ACSC1(1)*4,&A2                     
&A3      SETA  &A3+1                                                    
&A2      SETA  &A2-1                                                    
         AGO   .ADRLP                                                   
.NSAV1   ANOP                                                           
         AIF   (T'&RG EQ 'O').OKBASE1                                   
         AIF   (T'&RG NE 'O').OKBASE1                                   
         MNOTE 4,'YOU REQUESTED NO BASE REGISTERS, WILL GIVE YOU ONE'   
&A1      SETA  1                                                        
         AGO   .NOBASE1                                                 
.OKBASE1 ANOP                                                           
&A1      SETA  &RG                                                      
.NOBASE1 ANOP                                                           
&A2      SETA  12                                                       
&A3      SETA  1                                                        
&A4      SETA  82                                                       
         AGO   .ADRLP                                                   
.EADDR   AIF   (T'&PARM EQ 'O').PATCHS                                  
         SPACE 2                                                        
*---------------------------------------------------------------------* 
*        SAVE PARM ADDRESS OR INFO                                    * 
*---------------------------------------------------------------------* 
         SPACE 2                                                        
         AIF   ('&PARM(2)' NE 'ADDR').NPARM2                            
         ST    2,&PARM(1)               SAVE PARM ADDRESS POINTER       
         AGO   .PATCHS                                                  
.NPARM2  AIF   ('&PARM(2)' NE 'DATA').NPARM3                            
         L     1,0(0,2)                 GET PARM ADDRESS                
         LH    3,0(0,1)                 GET LENGTH                      
         LA    3,1(0,3)                 SET FOR MVC                     
         EX    3,*+8                    DO THE MOVE                     
         B     *+10                     GO AROUND                       
         MVC   &PARM(1).(0),0(1)        EXECUTED MOVE                   
         AGO   .PATCHS                                                  
.NPARM3  MNOTE 4,'INVALID ''PARM'' PARAM, NO INFO SAVED'                
.PATCHS  AIF   ('&PATCH' EQ 'NO').LEAVE                                 
         AIF   ('&PATCH' NE 'YES').LEAVE                                
         SPACE 2                                                        
*---------------------------------------------------------------------* 
*        PATCH AREA                                                   * 
*---------------------------------------------------------------------* 
         B     *+104                    GO AROUND                       
         NOP   *                        SET UP ADDRESS INDICATOR        
         DC    96X'00'                  CLEAR PATCH AREA                
.LEAVE   AIF   ('&PC' NE 'YES').NPCYES                                  
         SPACE 2                                                        
*---------------------------------------------------------------------* 
*        ISSUE SPIEPC MACRO                                           * 
*---------------------------------------------------------------------* 
         SPACE 2                                                        
         SPIEPC                                                         
.NPCYES ANOP                                                            
         SPACE 2                                                        
         AIF   (T'&PGM EQ 'O').OUT                                      
*        ISSUE WTO FOR PROGRAM NAME                                     
         SPACE                                                          
         WTO   '&PGM EXECUTING',ROUTCDE=2                               
         SPACE                                                          
.OUT     ANOP                                                           
         LR 1,2                         RESTORE PARM INFO               
         SPACE 2                                                        
         MEND                                                           
         MACRO                                                          
&NAME    LEAVE &SAVE=,&RC=0,&RECREG=                                    
*        COPY  ACSCGBLS                                                 
         GBLB  &ACSC1(8)                                                
         GBLA  &ACSC2(4)                                                
         GBLC  &ACSC3(4)                                                
         SPACE 2                                                        
*********************************************************************** 
*        LEAVE MACRO                                                  * 
*********************************************************************** 
         SPACE 2                                                        
&NAME    DS    0H                                                       
         AIF   (NOT &ACSC1(2)).NOSP1                                    
*---------------------------------------------------------------------* 
*        RESET SPIE ROUTINE                                           * 
*---------------------------------------------------------------------* 
         SPACE 2                                                        
         L     1,&ACSC3(1)+72(13)       GET SAVED ADDRESS               
* ISSUE SPIE MACRO                                                      
         SPIE  MF=(E,(1))                                               
         SPACE 2                                                        
.NOSP1   ANOP                                                           
*---------------------------------------------------------------------* 
*        RESET SAVE AREA AND EXIT                                     * 
*---------------------------------------------------------------------* 
         SPACE 2                                                        
         AIF   (T'&SAVE EQ 'O').NSAV                                    
         LR    1,13                     SET FOR RELEASE                 
.NSAV    L     13,4(13)                 UNCHAIN SAVE AREA               
         AIF   (T'&SAVE EQ 'O').NSAV1                                   
* ISSUE FREEMAIN MACRO                                                  
         LA    0,&SAVE+72+&ACSC1(1)*4   GET LENGTH OF AREA              
         FREEMAIN R,LV=(0),A=(1)                                        
.NSAV1   AIF   (T'&RECREG EQ 'O').RET                                   
         AIF   ('&RECREG'(1,1) EQ '(').RECOK                            
         MNOTE 4,'INVALID RECREG, NOT REGISTER NOTATION, IGNORED'       
         AGO   .RET                                                     
.RECOK   ST    &RECREG(1),24(13)        SAVE RECREG IN R1 AREA          
.RET     RETURN (14,12),T,RC=&RC                                        
         SPACE 2                                                        
         MEND                                                           
         MACRO                                                          
         EQUATE                                                         
**                           EQUATES FOR SYMBOLIC REG USAGE             
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
RA       EQU   10                                                       
RB       EQU   11                                                       
RC       EQU   12                                                       
RD       EQU   13                                                       
RE       EQU   14                                                       
RF       EQU   15                                                       
         MEND                                                           
         PRINT ON                                                       
PFK      TITLE 'PFK CREATION - MODIFICATION PROGRAM'                    
****DATE - 10/24/78**************************************************** 
*  PROGRAMMER NAME -                                                  * 
*                                                                     * 
*         BARRY GOLDBERG                                              * 
*         SYSTEMS PROG.                                               * 
*         TECHNICAL SUPPORT                                           * 
*         213-741-4875                                                * 
*         AUTO CLUB OF SOUTHERN CALIFORNIA                            * 
*         2601 S. FIGUEROA                                            * 
*         LOS ANGELES, CA. 90007                                      * 
*  REQUESTED BY -                                                     * 
*                                                                     * 
*         BARRY GOLDBERG                                              * 
*  EFFECTIVE DATE -                                                   * 
*                                                                     * 
*        SOON                                                         * 
*  PARM USAGE -                                                       * 
*                                                                     * 
*        NA                                                           * 
*  REGISTER USAGE -                                                   * 
*                                                                     * 
*        SEE TEXT                                                     * 
*  SWITCHES -                                                         * 
*                                                                     * 
*        NA                                                           * 
*  INPUT FILES -                                                      * 
*                                                                     * 
*        SYSIN - PFK CONTROL                                          * 
*  OUTPUT FILES -                                                     * 
*                                                                     * 
*        NEW PFK AREA IN STORAGE                                      * 
*  MODULES USED -                                                     * 
*                                                                     * 
*        NA                                                           * 
*  ENVIROMENTS  -                                                     * 
*                                                                     * 
*        MVS                                                          * 
*        VS1                                                          * 
*  DESCRIPTION -                                                      * 
*                                                                     * 
*        THIS ROUTINE WILL MODIFY THE PFK AREAS IN MAIN               * 
*        STORAGE.  IN ORDER TO MAKE THEM PERMANENT,                   * 
*        THE OPERATOR WILL HAVE TO MAKE A                             * 
*        REAL PFK DEFINITION.                                         * 
*        NOTE -- THIS PROGRAM MUST RUN AUTHORIZED OR KEY ZERO.        * 
*                                                                     * 
*   PROGRAM DESCRIPTION -                                             * 
*         SSP79PFK WILL LOAD THE RESIDENT PFK AREA FROM CARD IMAGE    * 
*         INPUT.                                                      * 
*         THE OPERATORS MAY MAKE THE CHANGES PERMANENT AND UPDATE     * 
*         SYS1.DCMLIB BY MEARLY UPDATING ANY PFK.                     * 
*         IT IS SUGGESTED THAT THE SIMPLEST ONE BE UPDATED.  THIS WILL* 
*         CAUSE THE IEEPFKEY MEMBER TO BE REWRITTEN WITH ALL PFK'S.   * 
*                                                                     * 
*INPUT RECORD FORMAT:                                                 * 
*                                                                     * 
*   COL               FUNCTION                                        * 
*                                                                     * 
*  1 - 2             CONSOLE ID FOR PFK                               * 
*  3 - 4             PFK #                                            * 
*    5               PFK CONTROL                                      * 
*                    BLANK  =  NULL PFK ENTRY                         * 
*                       N   =  NON CONVERSATIONAL                     * 
*                       Y   =  CONVERSATIONAL                         * 
*  7 - 71            THE COMMAND AS IT WOULD BE ISSUED AND IF         * 
*                     MULTIPLE, SEPARATED WITH A SEMICOLON. ALSO      * 
*                     UNDERSCORE ALLOWED (TAKES A POSITION).          * 
*   72               CONTINUATION COLUMN IF COMMAND(S) REQUIRE        * 
*                     ADDITIONAL SPACE ON ANOTHER CARD.               * 
*   CARD # 2 -                                                        * 
*   16-55            CONTINUATION OF COMMAND IF PREVIOUS CARD         * 
*                     IS NONBLANK IN COL 72.                          * 
*                                                                     * 
*NOTE :                                                               * 
*         1) MAXIMUM LENGTH OF ALL COMMANDS INCLUDING SPECIAL         * 
*               CHARACTERS AND BLANKS IS 105 CHARACTERS.              * 
*         2) THIS PROGRAM MUST RUN AUTHORIZED OR AS A SYSTEM TASK.    * 
*                                                                     * 
*EXAMPLE:                                                             * 
*                                                                     * 
*  //TSGBLGAC JOB 269,GOLDBERG,CLASS=X                                  
*  //PFK   EXEC  PGM=SSP79PFK                                           
*  //SYSIN DD    *                                                      
*  0101N K E,1                                                          
*  0102Y V 020,CONSOLE,ROUT=(1,2,3,4,5,6,8,9,10,11,12,14,15)            
*  0103Y S SSP98LBL.P3;S SSP97LBL.P3                                    
*  0104Y V 520,CONSOLE,ROUT=(1,2,3,4,5)                                 
*  0105Y S LBLREADC.P3;S LBLREAD.P3,ID=_X                               
*  0106Y V 301,CONSOLE,ROUT=7,AUTH=ALL;V 300,CONSOLE,ROUT=3,AUTH=INFO   
*  0107                                                                 
*  0108Y S MSC1010P.P3,RUN=_X                                           
*  0109Y CENOUT C=A,J=_                                                 
*  0110Y S MSC9010P.P3,RUN=_                                            
*  0111Y K S,DEL=RD,SEG=19,RTME=001,RNUM=19,CON=N                       
*  0112Y S SSP93CPY.P3,NEWVOL='SER=_CAPS'                               
*  0201N K E,1                                                          
*  0202                                                                 
*  0203                                                                 
*  0204Y V 020,MSTCONS                                                  
*  0205                                                                 
*  0206                                                                 
*  0207Y K S,DEL=RD,SEG=19,RTME=001,RNUM=19,CON=N;K A,NONE              
*  0208N D A                                                            
*  0209N D N=SOUT                                                       
*  0210N D R                                                            
*  0211N D N                                                            
*  0212N K                                                              
*  0301N K E,1                                                          
*  0302Y S DUMWTR.P                                                     
*  0303Y S IRPT.P3;S IRPTJ.P3                                           
*  0304Y V 002,ONLINE;V 00E,ONLINE;V 011,ONLINE;V 010,ONLINE            
*  0305Y SF PRINT,002,,_;SF PRINT,00E;SF PUNCH,011                      
*  0306Y SF RDR,010                                                     
*  0307Y K S,DEL=RD,SEG=19,RTME=001,RNUM=19,CON=N;K A,NONE              
*  0308N D A                                                            
*  0309N D N                                                            
*  0310Y D Q;D R                                                        
*  0311N D N=SOUT                                                       
*  0312N K                                                              
*  0401N K E,1                                                          
*  0402Y V 017,CONSOLE,ROUT=(1,2,3,4,5,6,8,9,10,11,12,14,15)            
*  0403Y S SSP98LBL.P3;S SSP97LBL.P3                                    
*  0404                                                                 
*  0405Y S LBLREADC.P3;S LBLREAD.P3,ID=_X                               
*  0406Y V 301,CONSOLE,ROUT=7,AUTH=ALL;V 300,CONSOLE,ROUT=3,AUTH=INFO   
*  0407                                                                 
*  0408Y S MSC1010P.P3,RUN=_X                                           
*  0409Y CENOUT C=A,J=_                                                 
*  0410Y S MSC9010P.P3,RUN=_                                            
*  0411Y K S,DEL=RD,SEG=19,RTME=001,RNUM=19,CON=N;K A,NONE              
*  0412Y S SSP93CPY.P3,NEWVOL='SER=_CAPS'                               
*  0501N K E,1                                                          
*  0502                                                                 
*  0503                                                                 
*  0504                                                                 
*  0505                                                                 
*  0506                                                                 
*  0507Y K S,DEL=RD,SEG=19,RTME=001,RNUM=19,CON=N;K A,NONE              
*  0508                                                                 
*  0509                                                                 
*  0510                                                                 
*  0511                                                                 
*  0512                                                                 
*                                                                       
****DATE - 10/24/78**************************************************** 
         EJECT                                                          
SETPFKEY START 0                                                  *JLM* 
XX       INIT  PGM=SETPFKEY                                       *JLM* 
         SPACE                                                          
         OPEN  SYSIN                                                    
         SPACE 2                                                        
         MODESET KEY=ZERO                                               
         SPACE                                                          
G1       EQU   *                                                        
         SPACE                                                          
**********************************************************************  
*        THIS IS THE LOOP START                                       * 
*        INITIALIZE THE PFK AREA AND READ/FILL WITH DATA              * 
**********************************************************************  
         SPACE                                                          
         MVI   CTLLINE,C' '             BLANK OUT IMAGE                 
         MVC   CTLLINE+1(L'CTLLINE-1),CTLLINE                           
         XC    CTLIND,CTLIND            AND INDICATORS                  
G1A      EQU   *                                                  *JLM* 
         GET   SYSIN                    GET A RECORD                    
         CLI   0(R1),C'*'               IS THIS A COMMENT         *JLM* 
         BE    G1A                        YES, IGNORE             *JLM* 
         LR    R3,R1                    POINT TO IT                     
         PACK  DBL,0(2,R3)              GET CONSOLE ID                  
         CVB   R1,DBL                                                   
         STH   R1,CONID                 AND SAVE IT                     
         PACK  DBL,2(2,R3)              GET PFK NUMBER                  
         CVB   R1,DBL                                                   
         STH   R1,PFKNO                 SAVE IT FOR LATER               
         CLI   4(R3),C' '               NULL ENTRY?                     
         BE    SETX                     YES, WE ARE FINISHED            
         OI    CTLIND,ACTIVE            INDICATE ACTIVE ENTRY           
         CLI   4(R3),C'N'               IS IT NON CONVERSATIONAL        
         BE    SET1                     YES                             
         CLI   4(R3),C'Y'               COONVERSATIONAL                 
         BNE   SET1                     TOO BAD, IGNORE                 
         OI    CTLIND,CONY              SET CONVERSATIONAL BIT          
         SPACE 2                                                        
SET1     EQU   *                                                        
         MVC   CTLLINE1,6(R3)           SET COMMAND                     
         CLI   71(R3),C' '              ANY CONTINUATION?               
         BE    SETX                     NO                              
G2A      EQU   *                                                  *JLM* 
         GET   SYSIN                                                    
         CLI   0(R1),C'*'               IS THIS A COMMENT         *JLM* 
         BE    G2A                        YES, IGNORE             *JLM* 
         MVC   CTLLINE2,15(R1)          GET REST OF THE COMMAND         
         SPACE 2                                                        
**********************************************************************  
*        DATA AREA NOW COMPLETE, CHECK AND SET PFK                    * 
**********************************************************************  
         SPACE                                                          
SETX     EQU   *                                                        
         L     R15,CVTPTR               POINT TO CVT                    
         USING CVT,R15                                                  
         L     R15,CVTCUCB                                              
         USING UCM,R15                                                  
         LM    R3,R5,UCMVDATA           SET TO SEARCH                   
         DROP  R15                                                      
         LH    R6,CONID                 GET CON NUMBER                  
SRCH1    EQU   *                                                        
         BCT   R6,SRCH2                 NEXT ONE?                       
         B     SRCH3                    CONTINUE TO LOOK                
SRCH2    EQU   *                                                        
         BXLE  R3,R4,SRCH1              GO GET MORE                     
         B     G1                       NOT HERE, IGNORE                
         SPACE 2                                                        
SRCH3    EQU   *                                                        
**********************************************************************  
*        HAVE FOUND THE ENTRY, NOW MUST GO TO                         * 
*        GET THE PFK AREA                                             * 
**********************************************************************  
         SPACE 2                                                        
         USING UCMLIST,R3                                               
         ICM   R15,B'1111',UCMXB        DCM ADDRESS                     
         BZ    G1                       NOT HERE                        
         DROP  R3                                                       
         USING DCM,R15                                                  
         ICM   R15,B'1111',DCMADPFK                                     
         BZ    G1                       NOT HERE, IGNORE                
         SPACE 2                                                        
**********************************************************************  
*        WE ARE NOW POINTING TO THE PFK AREA                          * 
**********************************************************************  
         SPACE                                                          
         LH    R4,PFKNO                 GET PFK NUMBER                  
PFK1     EQU   *                                                        
         BCT   R4,NXTPFK                GET ANOTHER                     
         CLC   0(1,R15),PFKNO+1         IS IT THE SAME?                 
         BNE   G1                       NO, ERROR                       
         MVC   1(PFKLEN-1,R15),CTLIND   SET UP THE PFK                  
         B     G1                                                       
         SPACE                                                          
NXTPFK   EQU   *                                                        
         LA    R15,PFKLEN(R15)          POINT TO NEXT ONE               
         CLI   0(R15),100               END?                            
         BE    G1                       YES, NOT HERE                   
         B     PFK1                     GO LOOK AGAIN                   
         SPACE 2                                                        
EOFIN    EQU   *                                                        
         MODESET KEY=NZERO                                        *JLM* 
         CLOSE SYSIN                                                    
         LEAVE                                                          
         SPACE 2                                                        
         LTORG                                                          
         SPACE                                                          
DBL      DS    D                        A WORK AREA                     
CONID    DS    H                        REQUESTED CONSOLE               
PFKNO    DS    H                        REQUESTED PFK NUMBER            
         DS    0D                                                       
CTLIND   DS    X                        CONTROL BYTE                    
ACTIVE   EQU   X'80'                                                    
CONY     EQU   X'20'                    PFK IS CONVERSATIONAL           
CTLLINE  DS    0CL108                   LENGTH OF DATA                  
CTLLINE1 DS    CL65                     FIRST CARD IMAGE AMOUNT         
CTLLINE2 DS    CL40                     NEXT AMOUNT (ALLOW ONLY 105)    
         DS    CL3                      THE REST                        
         SPACE 2                                                        
PFKLEN   EQU   110                      LENGTH OF A PFK ANTRY           
         SPACE 2                                                        
SYSIN    DCB   DDNAME=SYSIN,MACRF=GL,EODAD=EOFIN,DSORG=PS               
         SPACE 2                                                        
         CVT   SYS=AOS1,DSECT=YES                                       
         IEECUCM SYS=AOS1,FORMAT=NEW                                    
DCM      DSECT                                                          
         IEECRDCM DSECT=YES                                             
         SPACE 2                                                        
         END                                                            
//*                                                                     
//LKED.SYSLMOD DD DSN=SYS2.LINKLIB,DISP=SHR                             
//LKED.SYSIN   DD *                                                     
  SETCODE AC(1)                        12/2014 JLM                      
  NAME SETPFKEY(R)                     12/2014 JLM                      
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -ASMFCL13 
//*                                                                     
//UPDATE14 EXEC PGM=IEBUPDTE,PARM=NEW                                   
//*                                                                     
//* ***************************************************************** * 
//* INSTALLS SETPFKEY PROC INTO SYS2.PROCLIB                          * 
//* ***************************************************************** * 
//*                                                                     
//SYSUT2   DD  DSN=SYS2.PROCLIB,DISP=MOD                                
//SYSPRINT DD  SYSOUT=*                                                 
//SYSIN    DD  DATA,DLM='><'                                            
./ ADD    LIST=ALL,NAME=SETPFKEY                                        
//*-------------------------------------------------------------------* 
//*            SET CONSOLE PFKEYS FROM SYS1.PARMLIB MEMBER            * 
//*-------------------------------------------------------------------* 
//SETPFKEY PROC M=                                                      
//SETPFKEY EXEC PGM=SETPFKEY                                            
//SYSIN     DD DSN=SYS1.PARMLIB(SETPFK&M),DISP=SHR                      
./ ENDUP                                                                
><                                                                      
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE14 
//*                                                                     
//ASMFCL15 EXEC ASMFCL,PARM.ASM='LIST,NODECK,OBJ,TERM,NOXREF',          
//             MAC1='SYS1.AMODGEN',                                     
//             PARM.LKED='XREF,LET,LIST,NCAL'                           
//*                                                                     
//* ***************************************************************** * 
//* INSTALLS ZTIMER INTO SYS2.LINKLIB                                 * 
//* ***************************************************************** * 
//*                                                                     
//ASM.SYSIN DD *                                                        
         PRINT OFF                                                      
         MACRO                                                          
&LABEL   $PROLOG &LV=,&RENT=NO,&ERRCODE=,&C=,&SP=,&GM=,&LIST=NO         
.********************************************************************** 
.*                                                                    * 
.*                                  CHANEY SYSTEMS SUPPORT, INC.      * 
.*                                                                    * 
.*       THIS MACRO PROVIDES STANDARD LINKAGE AND BASE REGISTER       * 
.*       SPECIFICATIONS FOR MOST MEMBERS OF THE CSS TOOL KIT.         * 
.*                                                                    * 
.*       THE FIRST OPERERAND IS A POSITIONAL LIST OF REGISTERS        * 
.*       TO BE USED AS BASE(S) FOR THE CSECT BEING DEFINED AND        * 
.*       THE LABEL BECOMES THE CSECT NAME.                            * 
.*                                                                    * 
.*       LV=NNN    SPECIFIES AN ADDITIONAL AMOUNT OF STORAGE TO BE    * 
.*                 GOTTEN FOLLOWING THE DYNAMIC SAVE AREA.  THIS      * 
.*                 ADDITIONAL STORAGE IS ADDRESSED VIA REG 13         * 
.*                 (JUST FOLLOWING THE 18 FULLWORD SAVEAREA) AND      * 
.*                 IS LIMITED TO 4023.                                * 
.*                                                                    * 
.*       RENT=YES  IF USED, SPECIFIES THAT THE LENGTH OF ADDITIONAL   * 
.*                 STORAGE IS DETERMINED BASED UPON THE SIZE OF       * 
.*                 THE DYNAMIC STORAGE DEFINED BETWEEN TWO            * 
.*                 LABELS:  "SAVEAREA" AND "WORKEND".  THE WORK       * 
.*                 AREA IS ADDRESSABLE VIA REG 13 THE SAME AS THE     * 
.*                 LV= OPERAND.  THE USER DEFINED CONSTANTS THAT      * 
.*                 EXIST IN THE SOURCE ARE COPIED TO THE NEWLY        * 
.*                 AQUIRED STORAGE AND ARE DIRECTLY ADDRESSABLE.      * 
.*                 CAUTION: THE DYNAMIC AREA MUST BE DEFINED          * 
.*                          "IMMEDIATELY PRIOR TO ANY DSECTS" AND     * 
.*                          YOU MUST SPECIFY "LTORG" PRIOR TO THE     * 
.*                          DEFINITION OF "SAVEAREA".                 * 
.*                                                                    * 
.*                 NOTE: LV= AND RENT=YES ARE MUTUALLY EXCLUSIVE.     * 
.*                                                                    * 
.*       ERRCODE=  SPECIFIES THE RETURN CODE TO BE RETURNED TO        * 
.*                 THE CALLER IN THE EVENT THAT THE CONDITIONAL       * 
.*                 GETMAIN FAILS.  IF SPECIFIED, THE GETMAIN THAT     * 
.*                 IS ISSUED WILL BE CONDITIONAL AND IF IT DOES       * 
.*                 NOT COMPLETE NORMALLY, THIS ERROR CODE WILL        * 
.*                 BE RETURNED TO THE CALLER.  IF ERRCODE IS NOT      * 
.*                 SPECIFIED, THE GETMAIN THAT IS ISSUED WILL BE      * 
.*                 UNCONDITIONAL WITH A RELATED 80A ABEND IN THE      * 
.*                 EVENT OF FAILURE.                                  * 
.*                                                                    * 
.*       SP=       IS USED TO CAUSE AN MVS SPLEVEL MACRO TO BE        * 
.*                 EXECUTED AS PART OF THE ASSEMBLY.  POSSIBLE        * 
.*                 OPTIONS ARE "1" (370) OR "2" (XA).  IF NOT         * 
.*                 SPECIFIED, THE SPLEVEL MACRO IS NOT USED.          * 
.*                                                                    * 
.*       C=        IS A MEANS OF PROVIDING ADDITIONAL DATA IN THE     * 
.*                 EYECATCHER.  IF USED, THE DATA SPECIFIED MUST      * 
.*                 BE ENCLOSED WITHIN QUOTES AND IS LIMITED TO        * 
.*                 46 CHARACTERS.                                     * 
.*                                                                    * 
.*       GM=NO     IS NOT SUPPORTED BY THIS MACRO BUT IS ALLOWED      * 
.*                 FOR COMPATIBILITY OF OLDER VERSIONS.               * 
.*                                                                    * 
.*       LIST=NO   SUPPRESSES GENERATION OF LISTINGS FOR $PROLOG,     * 
.*                 $EPILOG AND $REGS WHEN EXPANSION IS ACTIVE         * 
.*                 (PRINT GEN).                                       * 
.*                                                                    * 
.*       EXAMPLES:                                                    * 
.*                                                                    * 
.*       SECTNAME $PROLOG ,        R12 IS BASE BY DEFAULT             * 
.*       SECTNAME $PROLOG R12,R11  R12 IS 1ST BASE AND R11 IS SECOND  * 
.*       SECTNAME $PROLOG R2,LV=8  R2 IS BASE AND AN ADDITIONAL       * 
.*                                 8 BYTES ARE ADDED TO THE STORAGE   * 
.*                                 GOTTEN FOR THE SAVEAREA.           * 
.*                                                                    * 
.*       SECTNAME $PROLOG RENT=YES R12 IS BASE AND THE ADDITIONAL     * 
.*                                 STORAGE TO BE GOTTEN IS DEFINED    * 
.*                                 BETWEEN "SAVEAREA" AND "WORKEND".  * 
.*       SAVEAREA DS    9D         (SAVE AREA FOR $PROLOG GENERATION) * 
.*       MYFIELD1 DC    CL8'DATA1' (PROGRAM CONSTANTS)                * 
.*       MYFIELD2 DC    CL8'DATA2' (PROGRAM CONSTANTS)                * 
.*       WORKEND  EQU   *          (END OF DYNAMIC WORK AREA)         * 
.*                                                                    * 
.********************************************************************** 
         GBLA  &EPILOG             DEFINE GLOBAL FOR $EPILOG            
         GBLB  &REGS,&LSAVE        DEFINE GLOBALS FOR $REGS/$EPILOG     
         LCLA  &AA,&AB,&BUMP,&X    DEFINE LOCAL VARIABLES               
         LCLC  &GMT,&BASE,&LISTOPT DEFINE LOCAL VARIABLES               
&X       SETA  &SYSNDX             SET LABEL CONSTANT                   
&EPILOG  SETA  0                   RESET LV= GLOBAL                     
&BUMP    SETA  4096                SET FOR BASE REG BUMP                
&LSAVE   SETB  0                   RESET RENT GLOBAL FOR $EPILOG        
&GMT     SETC  'RU'                SET UNCONDITIONAL GETMAIN            
         AIF   ('&LIST' EQ 'YES').LIST1                                 
         PUSH PRINT                                                     
         PRINT OFF                                                      
.LIST1   ANOP                                                           
&LABEL   CSECT                                                          
         AIF   (T'&SP EQ 'O').GO1  IF NO SPLEVEL REQUIRED               
         SPLEVEL SET=&SP           ISSUE USER REQUESTED SPLEVEL MACRO   
.GO1     ANOP                                                           
         USING *,R15               TEMPORARY BASE                       
         B     $&X.A               BRANCH AROUND CONSTANTS              
         DC    CL8'&LABEL'         PROVIDE EYECATCHER                   
         AIF   (T'&C EQ 'O').GO2   COMMENTS ADDITION?                   
         DC    CL46&C                                                   
.GO2     ANOP                                                           
         DC    C'&SYSDATE @ &SYSTIME' DATE/TIME STAMP OBJECT            
         AIF   (T'&LV   EQ 'O').GO3 IF LV= NOT SPECIFIED                
         AIF   ('&RENT' NE 'YES').GO3 RENT NOT ALSO SPECIFIED           
         MNOTE 12,'$PROLOG - RENT=YES AND LV=&LV MUTUALLY EXCLUSIVE'    
         MEXIT                                                          
.GO3     AIF   ('&RENT' EQ 'YES').GO4   RENT=YES SPECIFIED              
         AIF   (T'&LV   NE 'O').GO4   LV= SPECIFIED                     
&LSAVE   SETB  1                   SET NORENT GLOBAL FOR $EPILOG        
$AVE&X   DC    18F'0'              DEFINED SAVE AREA                    
.GO4     ANOP                                                           
         AIF   (T'&LABEL NE 'O').GO5 INSURE CSECT NAME PROVIDED         
         MNOTE 8,'$PROLOG - CSECT NAME NOT SUPPLIED'                    
.GO5     ANOP                                                           
$&X.A    STM   R14,R12,12(R13)     SAVE CALLERS REGISTERS               
&BASE    SETC  'R12'               ASSUME A BASE REGISTER               
         AIF   (N'&SYSLIST EQ 0).GO6 USE DEFAULT IF NOT SPECIFIED       
&BASE    SETC  '&SYSLIST(1)'       SET THE SPECIFIED BASE REGISTER      
.GO6     ANOP                                                           
         LR    &BASE,R15           SET FIRST BASE REGISTER              
         DROP  R15                 FREE THE TEMPORARY BASE              
         USING &LABEL,&BASE        INFORM ASSEMBLER                     
         AIF   (N'&SYSLIST EQ 0).GO7                                    
&AA      SETA  2                   NUMBER TO DEFINE +1                  
.LOOP    ANOP                                                           
         AIF   (&AA GT N'&SYSLIST).GO7                                  
&AB      SETA  &AA-1               NUMBER OF LAST BASE REG DEFINED      
         LA    &SYSLIST(&AA),2048(&SYSLIST(&AB)) SET NEXT BASE REG      
         LA    &SYSLIST(&AA),2048(&SYSLIST(&AA)) SET NEXT BASE REG      
         USING &LABEL+&BUMP,&SYSLIST(&AA) INFORM THE ASSEMBLER          
&BUMP    SETA  &BUMP+4096          BUMP INDEX                           
&AA      SETA  &AA+1               BUMP CONTROL COUNT                   
         AGO   .LOOP                                                    
.GO7     AIF   (T'&ERRCODE EQ 'O').GO8 IF ERROR CODE NOT SPECIFIED      
&GMT     SETC  'RC'                ERROR CODE WAS SPECIFIED             
.GO8     AIF   (T'&LV   NE 'O').GO10 LV= SPECIFIED, DO GETMAIN          
         AIF   ('&RENT' EQ 'YES').GO12 RENT SPECIFIED, DO GETMAIN       
         AIF   (T'&ERRCODE EQ 'O').GO9 IF ERROR CODE NOT SPECIFIED      
      MNOTE 8,'$PROLOG - ERRCODE=&ERRCODE INVALID WITHOUT RENT=YES/LV=' 
.GO9     ANOP                                                           
$&X.B    LA    R2,$AVE&X           ADDRESS OF SAVE AREA                 
         AGO   .COMMON                                                  
.GO10    ANOP                                                           
&EPILOG  SETA  &LV+72              SET SIZE FOR $EPILOG FREEMAIN        
         LA    R0,&LV+72           SET SIZE FOR GETMAIN                 
         GETMAIN &GMT,LV=(0)       GET STORAGE                          
         AIF   (T'&ERRCODE EQ 'O').GO11 IF UNCONDITIONAL                
         LTR   R15,R15             STORAGE GOTTEN?                      
         BZ    $&X.C               YES, CONTINUE                        
         LA    R15,&ERRCODE        SET SPECIFIED ERROR CODE             
         ST    R15,16(R13)         INTO SAVE AREA                       
         LM    R14,R12,12(R13)     RESTORE CALLERS REGS                 
         BR    R14                 AND RETURN                           
$&X.C    DS    0H                                                       
.GO11    ANOP                                                           
         LR    R2,R1               SAVE THE GOTTEN STORAGE ADDRESS      
         LR    R14,R2               MVCL - TARGET ADDR                  
         LA    R15,&LV+72           MVCL - TARGET SIZE                  
         SR    R0,R0                MVCL - SOURCE ADDR (NONE)           
         SR    R1,R1                MVCL - SOURCE SIZE (NONE)           
         MVCL  R14,R0              ZERO GOTTEN STORAGE                  
         AGO   .COMMON                                                  
.GO12    ANOP                                                           
$&X.B    GETMAIN &GMT,LV=WORKEND-SAVEAREA GET THE SAVE AREA STORAGE     
         AIF   (T'&ERRCODE EQ 'O').GO13 IF UNCONDITIONAL                
         LTR   R15,R15             STORAGE GOTTEN?                      
         BZ    $&X.C               YES, CONTINUE                        
         LA    R15,&ERRCODE        SET SPECIFIED ERROR CODE             
         ST    R15,16(R13)         INTO SAVE AREA                       
         LM    R14,R12,12(R13)     RESTORE CALLERS REGS                 
         BR    R14                 AND RETURN                           
$&X.C    DS    0H                                                       
.GO13    ANOP                                                           
         LR    R2,R1               SAVE THE GOTTEN STORAGE ADDRESS      
         LR    R14,R2               MVCL - TARGET ADDR                  
         LA    R15,WORKEND-SAVEAREA MVCL - TARGET SIZE                  
         LA    R0,SAVEAREA          MVCL - SOURCE ADDR                  
         LR    R1,R15               MVCL - SOURCE SIZE                  
         MVCL  R14,R0              COPY TO WORKING STORAGE              
         USING SAVEAREA,R13        PROVIDE ADDRESSABILITY               
.COMMON  ANOP                                                           
         LR    R14,R13             COPY OLD SAVE AREA ADDRESS           
         LR    R13,R2              SET NEW SAVEAREA ADDRESS             
         ST    R14,4(R13)          CHAIN SAVEAREA - BACKWARD            
         ST    R13,8(R14)          CHAIN SAVEAREA - FORWARD             
         LM    R15,R2,16(R14)      RESTORE ENTRY REGISTERS              
         SR    R14,R14             RESET RETURN ADDRESS                 
         AIF   (&REGS).SKIPREG                                          
&LISTOPT SETC  'LIST=&LIST'                                             
         $REGS &LISTOPT            DEFINE STANDARD REG EQUATES          
.SKIPREG AIF   ('&LIST' EQ 'YES').MEXIT                                 
         POP  PRINT                                                     
.MEXIT   ANOP                                                           
         MEND                                                           
         MACRO                                                          
&LABEL   $EPILOG &RETCODE,&LIST=NO                                      
.********************************************************************** 
.*                                                                    * 
.*                                  CHANEY SYSTEMS SUPPORT, INC.      * 
.*                                                                    * 
.*       THIS MACRO PROVIDES STANDARD DE-LINKAGE CONVENTIONS FOR      * 
.*       MOST CSS TOOL KIT MEMBERS.                                   * 
.*                                                                    * 
.********************************************************************** 
         GBLA  &EPILOG                                                  
         GBLB  &LSAVE                                                   
         AIF   ('&LIST' EQ 'YES').LIST1                                 
         PUSH  PRINT                                                    
         PRINT OFF                                                      
.LIST1   ANOP                                                           
&LABEL   DS    0H                                                       
         AIF   (&LSAVE).GO3                                             
         AIF   (&EPILOG EQ 0).GO1                                       
         LA    R0,&EPILOG          GET SAVEAREA LENGTH                  
         AGO   .GO2                                                     
.GO1     LA    R0,WORKEND-SAVEAREA GET SAVEAREA LENGTH                  
.GO2     LR    R1,R13              GET SAVEAREA ADDRESS FOR FREEMAIN    
.GO3     ANOP                                                           
         L     R13,4(R13)          GET BACK CHAIN POINTER               
         ST    R15,16(R13)         SAVE REGISTER 15 (RETCODE)           
         AIF   (&LSAVE).GO4                                             
         FREEMAIN RU,LV=(0),A=(1)  FREE SAVEAREA                        
.GO4     ANOP                                                           
         LM    R14,R12,12(R13)     RESTORE CALLERS REGS                 
         AIF   (T'&RETCODE EQ 'O').GO5                                  
         LA    R15,&RETCODE        SET RETURN CODE                      
.GO5     ANOP                                                           
         BR    R14                 RETURN TO CALLER                     
         AIF   ('&LIST' EQ 'YES').MEXIT                                 
         POP   PRINT                                                    
.MEXIT   ANOP                                                           
         MEND                                                           
         MACRO                                                          
         $REGS &LIST=NO                                                 
.********************************************************************** 
.*                                                                    * 
.*                                  CHANEY SYSTEMS SUPPORT, INC.      * 
.*                                                                    * 
.*       THIS MACRO PROVIDES STANDARD BASE REGISTER EQUATES FOR       * 
.*       ALL "CSS" SUPPORT ROUTINES.                                  * 
.*                                                                    * 
.********************************************************************** 
         GBLB  &REGS                                                    
         AIF   (&REGS).MEXIT                                            
&REGS    SETB  1                                                        
         AIF   ('&LIST' NE 'YES').LIST1                                 
         PUSH  PRINT               SAVE CURRENT PRINT SETTINGS          
         PRINT OFF                 TEMPORARILY TURN OFF PRINT           
.LIST1   ANOP                                                           
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
         AIF   ('&LIST' NE 'YES').MEXIT                                 
         POP   PRINT               RESTORE CURRENT PRINT SETTINGS       
.MEXIT   ANOP                                                           
         MEND                                                           
*                              SOURCE: XEPHON MVS UDPATE 08/1987        
         PRINT ON                                                       
         TITLE '*** ZTIMER - TSG ACTION REMINDER.                 ***'  
*---------------------------------------------------------------------* 
*    THIS PROGRAM PROVIDES A 'REMINDER' FACILITY FOR EVENTS WHICH     * 
*    OCCUR REGULARLY. THIS PROGRAM WILL ISSUE A WRITE-TO-OPERATOR     * 
*    OR A CONSOLE COMMAND DEPENDING ON THE INPUT.                     * 
*    INPUT:  VIA DDNAME SYSIN (CONTROL STATEMENTS)                    * 
*            VIA DDNAME SMFIN (SMFPRMOO MEMBER IN SYS1.PARMLIB)       * 
*    OUTPUT: TO CONSOLE VIA WTO OR MGCR MACRO.                        * 
*---------------------------------------------------------------------* 
* 12/2004 RETRIEVAL OF SYSTEM ID VIA CONTROL BLOCKS               *JLM* 
*---------------------------------------------------------------------* 
ZTIMER   $PROLOG R12,C='ZTIMER'                                         
*---------------------------------------------------------------------* 
*       GET CURRENT TIME AND DATE INTO CHARACTER FORMAT.              * 
*       CHECK FOR LEAP YEAR AND ADJUST.                               * 
*       EVALUATE DAY OF THE WEEK AND WHETHER WEEK-END OR NOT.         * 
*---------------------------------------------------------------------* 
         TIME  DEC                        GET SYSTEM TIME AND DATE      
         ST    R0,PTIM                    STORE PACKED TIME             
         ST    R1,PDAT                    STORE PACKED DATE             
         STCM  R1,4,YEAREND+1             SAVE 'YY' PART OF DATE        
         ED    CURTMSK(6),PTIM            UNPACK TIME                   
         ED    CURDMSK(7),PDAT+1          UNPACK DATE                   
         SLR   R2,R2                      R2 = 0                        
         CVB   R3,PDATDBL                 R3 = YYDDD IN BINARY          
         D     R2,FULL1000                R3 = YY,  R2 = DDD            
         LR    R4,R3                      KEEP YEARS FOR LATER          
         SRL   R4,2                       R4 = R4 / 4                   
         SLL   R4,2                       R4 = R4 * 4                   
         CR    R3,R4                      ARE YEARS EQUAL ?             
         BNE   NOLEAP                     NO ->                         
         MVC   YEAREND+2(2),LEAPDAYS      LEAP YEARS HAVE 366 DAYS      
NOLEAP   EQU   *                                                        
         ST    R3,YEARS                   SAVE BINARY YEARS             
         ST    R2,DAYS                    SAVE BINARY DAYS              
         SLR   R2,R2                      R2 = 0                        
         LA    R3,365                     R3 = 365                      
         M     R2,YEARS                   R3 = YEARS * 365              
         L     R4,YEARS                   R4 = YEARS IN BINARY          
         BCTR  R4,0                       R4 = R4 - 1                   
         SRL   R4,2                       R4 = R4 / 4                   
         AR    R3,R4                      R3 = R3 + R4                  
         A     R3,DAYS                    R3 = R3 + DDD DAYS            
         ST    R3,TEMP                    SAVE IT FOR NOW               
         SLR   R2,R2                      R2 = 0                        
         D     R2,FULL7                   R3 = INTEGER(R3 / 7)          
         M     R2,FULL7                   R3 = R3 * 7                   
         L     R2,TEMP                    GET BACK PREVIOUS VALUE       
         SR    R2,R3                      R2 = 0 TO 6 (SUN TO SAT)      
         SLL   R2,2                       R2 = R2 * 4                   
         LA    R2,WKDAYS(R2)              R2 -> TABLE ENTRY             
         MVC   WKDAY,0(R2)                EXTRACT WEEKDAY               
         CLC   WKDAY,SATURDAY             IS IT A SATURDAY ?            
         BE    WEEKEND                    YES ->                        
         CLC   WKDAY,SUNDAY               IS IT A SUNDAY ?              
         BNE   WEEKDAY                    NO ->                         
WEEKEND  EQU   *                                                        
         MVC   DAYTYPE,WKEND              SET DAYTYPE TO 'WEEK-END'     
WEEKDAY  EQU   *                                                        
*---------------------------------------------------------------------* 
*        CALCULATE NEXT INVOCATION OF ZTIMER                          * 
*        (2 HRS HENCE OR AT NEXT EVENT WHICHEVER COMES EARLIER)       * 
*        GET SYSTEM-ID FROM SMF PARAMETERS                            * 
*---------------------------------------------------------------------* 
         OI    PTIM+3,X'0F'               GIVE A DECENT SIGN TO PTIM    
         SRP   PTIM,64-1,0                SHIFT RIGHT 1 DIGIT POS       
         AP    PTIM,HRS2                  TIME = TIME + 2 HOURS         
         ED    NXTTMSK(7),PTIM            TIME FOR NEXT EXECUTION       
         CP    PTIM,DAYEND                DID WE PASS MIDNIGHT ?        
         BNH   SAMEDAY                    NO ->                         
         MVC   NXTTIM+1(5),TWENTY4        NEXT EXECUTION IS AT MIDNIGHT 
SAMEDAY  EQU   *                                                  *JLM* 
*         OPEN  (SMFIN,(INPUT))            OPEN SMF PARMLIB MEMBER*JLM* 
*SMFREAD  EQU   *                                                 *JLM* 
*         GET   SMFIN,INAREA               READ A RECORD          *JLM* 
*         LA    R2,INAREA                  R2 -> START OF SMF RECO*JLM* 
*         LA    R4,1                       R4 =  INCREMENT FOR BXL*JLM* 
*         LA    R5,INAREA+68               R5 -> END OF SMF RECORD*JLM* 
*SIDSERCH EQU   *                                                 *JLM* 
*         CLC   0(4,R2),SIDKEY             'SID(' KEYWORD FOUND ? *JLM* 
*         BE    SIDFOUND                   YES ->                 *JLM* 
*         BXLE  R2,R4,SIDSERCH             INCR, CHECK AND LOOP   *JLM* 
*         B     SMFREAD                    NOT FOUND, TRY NEXT CAR*JLM* 
*SMFEOF   EQU   *                                                 *JLM* 
*         MVC   SYSID,BLANKS               NO SYSID FOUND.        *JLM* 
*         B     SYSIDOK                                           *JLM* 
*SIDFOUND EQU   *                                                 *JLM* 
*         MVC   SYSID,4(R2)                SAVE CURRENT SYSTEM-ID *JLM* 
*SYSIDOK  EQU   *                                                 *JLM* 
*         CLOSE SMFIN                      CLOSE INPUT FILE       *JLM* 
          USING PSA,R2                     PSA BASE               *JLM* 
          USING CVT,R4                     CVT BASE               *JLM* 
          USING SMCABASE,R5                SMCA BASE              *JLM* 
          XR    R2,R2                      POINT TO PSA (0)       *JLM* 
          L     R4,FLCCVT                  POINT TO CVT           *JLM* 
          L     R5,CVTSMCA                 POINT TO SMCA          *JLM* 
          MVC   SYSID,SMCASID              RETRIEVE SMF-ID        *JLM* 
          DROP  R2,R4,R5                   RELEASE BASES          *JLM* 
*---------------------------------------------------------------------* 
*        READ EACH RECORD, AND CHECK FORMAT, DATE AND SYSTEM-ID       * 
*---------------------------------------------------------------------* 
         OPEN  (SYSIN,(INPUT))            OPEN INPUT FILE               
READ     EQU   *                                                        
         GET   SYSIN,INAREA               READ A RECORD                 
         CLI   INAREA,C'*'                IS THIS A COMMENT CARD ?      
         BE    READ                       YES ->                        
         CLC   INAREA(3),ALL              TO BE DONE ON 'ALL' DAYS ?    
         BE    CHKSYSID                   YES ->                        
         CLC   INAREA(5),DAYTYPE          TO BE DONE ON THIS TYP OF DAY 
         BE    CHKSYSID                   YES ->                        
         CLC   INAREA(3),WKDAY            TO BE DONE ON THIS WEEKDAY ?  
         BE    CHKSYSID                   YES ->                        
         LA    R2,WKDAYS                  R2 -> WEEKDAY TABLE           
         LA    R4,4                       R4 =  LENGTH OF TABLE ENTRIES 
         LA    R5,WKDAYS+24               R5 -> END OF WEEKDAY TABLE    
DAYSERCH EQU   *                                                        
         CLC   INAREA(3),0(R2)            TO BE DONE ON ANOTHER DAY ?   
         BE    READ                       YES ->                        
         BXLE  R2,R4,DAYSERCH             INCR, CHECK AND LOOP          
         CLC   INAREA(5),WKEND            TO BE DONE ON A WEEKEND ?     
         BE    READ                       YES ->                        
         CLC   INAREA(5),WORKDAY          TO BE DONE IN THE WEEK ?      
         BE    READ                       YES ->                        
         CLC   INAREA(5),CURDAT           TO BE DONE ON THIS JULDATE ?  
         BE    CHKSYSID                   YES ->                        
         CLI   INAREA+2,C'/'              VALID DATE SEPARATOR ?        
         BNE   DUFFCARD                   NO ->                         
         CLI   INAREA+5,C'/'              VALID DATE SEPARATOR ?        
         BNE   DUFFCARD                   NO ->                         
         BAL   R14,CALTOJUL               DDMMYY TO YYDDD CONVERSION    
         CLC   JULDAT,CURDAT              TO BE DONE TODAY ?            
         BNE   READ                       NO ->                         
CHKSYSID EQU   *                                                        
         CLC   INSYSID,BLANKS             WAS A SYSTEM-ID SPECIFIED ?   
         BE    CHKTIME                    NO -> GO AND CHECK TIME       
         CLC   INSYSID,SYSID              ACTION FOR THIS SYSTEM ?      
         BNE   READ                       NO ->                         
*---------------------------------------------------------------------* 
*        CHECK TIME OR EXECUTION AND TYPE OF ACTION REQUIRED.         * 
*        PERFORM THE REQUIRED FUNCTION.                               * 
*---------------------------------------------------------------------* 
CHKTIME  EQU   *                                                        
         CLC   INTIME,BLANKS              WAS A TIME SPECIFIED ?        
         BE    EXECUTE                    NO -> GO AND DO IT            
         CLC   INTIME,CURTIM              WHEN IS THE EVENT FOR ?       
         BE    EXECUTE                    NOW -> GO AND DO IT           
         BL    READ                       BEFORE -> IGNORE IT           
         CLC   INTIME,NXTTIM+1            BEFORE THE NEXT EXECUTION ?   
         BH    READ                       NO -> IGNORE IT               
         MVC   NXTTIM+1(5),INTIME         ENSURE WE CATCH HIM           
         B     READ                       GET NEXT CARD                 
EXECUTE  EQU   *                                                        
         CLC   INTYPE,COMMAND             IS IT A COMMAND REQUEST ?     
         BE    ISSUECOM                   YES ->                        
* ADDED 2ND WTO AND COMPARE FOR 'DSH'/'DSN'                       *JLM* 
         CLC   INTYPE,DISPLAYH            IS IT A DISPLAY REQUEST ?     
         BE    ISSUEDSH                   YES ->                        
         CLC   INTYPE,DISPLAYN            IS IT A DISPLAY REQUEST ?     
         BNE   DUFFCARD                   NO ->                         
ISSUEDSN MVC   MESSAGEN+8(49),INACTN      MOVE MESSAGE TO WTO AREA      
MESSAGEN WTO   '....+....1....+....2....+....3....+....4....+....'      
         B     READ                       GO BACK FOR MORE              
ISSUEDSH MVC   MESSAGEH+8(49),INACTN      MOVE MESSAGE TO WTO AREA      
MESSAGEH WTO   '....+....1....+....2....+....3....+....4....+....',    C
               DESC=(2)                   TELL 'EM                      
         B     READ                       GO BACK FOR MORE              
ISSUECOM EQU   *                                                        
         MVC   COMACTN(49),INACTN         MOVE COMMAND TO COM AREA      
         MODESET KEY=ZERO,MODE=SUP        SWITCH TO SUPERVISOR STATE    
         SLR   R0,R0                      PRETEND YOU'RE MVS            
         MGCR  COMAREA                    ISSUE THE COMMAND             
         MODESET KEY=NZERO,MODE=PROB      SWITCH BACK TO NORMAL         
         B     READ                       GO BACK FOR MORE              
*---------------------------------------------------------------------* 
*        END-OF-FILE HANDLING.                                        * 
*        RETURN TO CALLER.                                            * 
*        DUFF INPUT HANDLING.                                         * 
*---------------------------------------------------------------------* 
EOF      EQU   *                                                        
         CLOSE SYSIN                      CLOSE INPUT FILE              
         MVC   AUTOTIME,NXTTIM+1          MOVE NEXT TIME TO AUTO-COMM   
         MODESET KEY=ZERO,MODE=SUP        SWITCH TO SUPERVISOR STATE    
         SLR   R0,R0                      PRETEND YOU'RE MVS            
         MGCR  SCHEDULE                   ISSUE THE AUTOMATIC COMMAND   
         MODESET KEY=NZERO,MODE=PROB      SWITCH BACK TO NORMAL         
         $EPILOG                                                        
DUFFCARD EQU   *                                                        
         WTO   'ZTIMER: INVALID INPUT DETECTED - DUFF CARD READS:'      
         MVC   DUFFMSG+8(54),INAREA                                     
DUFFMSG  WTO   '....+....1....+....2....+....3....+....4....+....5....' 
         B     READ                       IGNORE THIS ONE               
*---------------------------------------------------------------------* 
*        CALENDER DATE (DD/MM/YY) TO JULIAN DATE (YY.DDD) CONVERSION  * 
*        INPUT AREA IS FIELD CALLED 'INDATE'                          * 
*        OUTPUT AREA IS FIELD CALLED 'JULDAT'                         * 
*---------------------------------------------------------------------* 
CALTOJUL EQU   *                                                        
*        PACK  DAY,INDATE(2)              EXTRACT DAY PORTION     *JLM* 
*        PACK  MONTH,INDATE+3(2)          EXTRACT MONTH PORTION   *JLM* 
         PACK  MONTH,INDATE(2)            EXTRACT MONTH PORTION   *JLM* 
         PACK  DAY,INDATE+3(2)            EXTRACT DAY PORTION     *JLM* 
         PACK  YEAR,INDATE+6(2)           EXTRACT YEAR PORTION          
         CVB   R5,MONTHDBL                R5 = MONTH IN BINARY          
         BCTR  R5,0                       R5 = R5 - 1                   
         SLL   R5,1                       R5 = R5 * 2                   
         LH    R2,TABLE(R5)               EXTRACT CUMULATIVE DAYS       
         CVB   R3,YEARDBL                 R3 = YEAR IN BINARY           
         LR    R5,R3                      R5 = YEAR IN BINARY           
         SRL   R3,2                       R3 = R3 / 4                   
         SLL   R3,2                       R3 = R3 * 4                   
         CR    R3,R5                      WAS IT A LEAP YEAR ?          
         BNE   MNTHREDY                   NO ->                         
         CP    MONTH,PACK2                IS IT PAST FEBRUARY ?         
         BNH   MNTHREDY                   NO ->                         
         LA    R2,1(R2)                   ADD 1 FOR THE 29TH            
MNTHREDY EQU   *                                                        
         CVB   R3,DAYDBL                  R3 = DAY IN BINARY            
         AR    R2,R3                      R2 = TOTAL DAYS               
         M     R4,FULL1000                R5 = YEAR * 1000              
         AR    R5,R2                      R5 = R5 + DAYS                
         CVD   R5,DOUBLE                  CONVERT DATE TO PACKED        
         MVC   JULDATF(7),JULDMSK         MOVE EDIT MASK TO FIELD       
         ED    JULDATF(7),DOUBLE+5        FORMAT INTO CHAR FIELD        
         BR    R14                        RETURN TO CALLER              
*---------------------------------------------------------------------* 
*        CONSTANT DATA AREAS.                                         * 
*---------------------------------------------------------------------* 
FULL7    DC    F'7'                       CONSTANT                      
FULL1000 DC    F'1000'                    CONSTANT                      
PACK2    DC    PL2'2'                     CONSTANT                      
HRS2     DC    PL4'020000'                02 HOURS                      
HRS24    DC    PL4'240000'                24 HOURS                      
DAYEND   DC    PL4'235959'                END OF DAY (MIDNIGHT - 1 MIN) 
YEAREND  DC    PL4'365'                   END OF YEAR                   
LEAPDAYS DC    PL2'366'                   NO OF DAYS IN A LEAP YEAR     
CURDMSK  DC    CL1'0'                     FILL CHAR                     
CURDAT   DC    XL6'21204B202020'          CURRENT DATE   (YY.DDD)       
CURTMSK  DC    CL1'0'                     FILL CHAR                     
CURTIM   DC    XL5'21204B2020'            CURRENT TIME  (HH.MM)         
NXTTMSK  DC    CL1'0'                     FILL CHAR                     
NXTTIM   DC    XL6'2120204B2020'          TIME OF NEXT EXECUTION        
JULDMSK  DC    XL7'F021204B202020'        JULIAN DATE EDIT MASK         
JULDATF  DS    XL1                        FILL CHAR                     
JULDAT   DS    XL6                        JULIAN DATE                   
TABLE    DC    H'000',H'031',H'059',H'090',H'120',H'151'                
         DC    H'181',H'212',H'243',H'273',H'304',H'334'                
WKDAYS   DC    C'SUN MON TUE WED THU FRI SAT'  ALL THE WEEKDAYS         
ALL      DC    CL3'ALL'                   CONSTANT                      
TWENTY4  DC    CL5'24.00'                 CONSTANT                      
BLANKS   DC    CL5'     '                 CONSTANT                      
DISPLAYH DC    CL3'DSH'                   CONSTANT                      
DISPLAYN DC    CL3'DSN'                   CONSTANT                      
COMMAND  DC    CL3'COM'                   CONSTANT                      
SATURDAY DC    CL3'SAT'                   CONSTANT                      
SUNDAY   DC    CL3'SUN'                   CONSTANT                      
SIDKEY   DC    CL4'SID('                  CONSTANT                      
WKEND    DC    CL5'WKEND'                 CONSTANT                      
WORKDAY  DC    CL5'WKDAY'                 CONSTANT                      
DAYTYPE  DC    CL5'WKDAY'                 EITHER 'WKDAY' OR 'WKEND'     
COMAREA  DS    0F                         COMMAND AREA                  
         DC    H'70'                      LENGTH                        
         DC    X'0000'                    FLAGS                         
COMACTN  DC    CL80' '                    COMMAND FIELD                 
SCHEDULE DS    0F                         AUTOMATIC COMMAND AREA        
         DC    H'37'                      LENGTH                        
         DC    X'0000'                    FLAGS                         
         DC    CL08'$TA99,T='                                           
AUTOTIME DC    CL05'00.00'                TIME FOR NEXT EXECUTION       
         DC    CL80',''$VS,''''S ZTIMER'''''''                          
*---------------------------------------------------------------------* 
*        OTHER STORAGE REQUIREMENTS                                   * 
*---------------------------------------------------------------------* 
DOUBLE   DS    D                          WORK SPACE FOR CONVERSIONS    
PTIMDBL  DC    XL4'0'                                                   
PTIM     DC    PL4'0'                     PACKED TIME FORMAT HHMMSSCC   
PDATDBL  DC    XL4'0'                                                   
PDAT     DC    PL4'0'                     PACKED DATE FORMAT 00YYDDDF   
DAYDBL   DC    XL5'0'                                                   
DAY      DC    PL3'0'                     WORKSPACE - JULTOCAL          
MONTHDBL DC    XL5'0'                                                   
MONTH    DC    PL3'0'                     WORKSPACE - JULTOCAL          
YEARDBL  DC    XL5'0'                                                   
YEAR     DC    PL3'0'                     WORKSPACE - JULTOCAL          
TEMP     DS    F                          WORKSPACE                     
YEARS    DS    F                          YEARS IN BINARY               
DAYS     DS    F                          DAYS IN BINARY                
WKDAY    DS    CL3                        TODAY'S WEEK DAY              
SYSID    DS    CL4                        SMF SYSID OF CURRENT SYSTEM   
INAREA   DS    0CL80                      INPUT AREA                    
INDATE   DS    CL08                       DATE FORMAT DD/MM/YY          
         DS    CL01                                                     
INTIME   DS    CL05                       TIME FORMAT HH.MM OR BLANK    
         DS    CL01                                                     
INTYPE   DS    CL03                       ACTION TYPE DISPLAY/COMMAND   
INSYSID  DS    CL04                       SMF SYSTEM-ID OF TARGET SYS   
         DS    CL01                                                     
INACTN   DS    CL49                       COMMAND OR MESSAGE            
         DS    CL08                                                     
*---------------------------------------------------------------------* 
*        DATA CONTROL BLOCKS.                                         * 
*---------------------------------------------------------------------* 
SYSIN    DCB   DDNAME=SYSIN,DSORG=PS,MACRF=(GM),EODAD=EOF               
*SMFIN    DCB   DDNAME=SMFIN,DSORG=PS,MACRF=(GM),EODAD=SMFEOF     *JLM* 
         PUSH PRINT                                                     
         PRINT OFF                                                      
         IHAPSA DSECT=YES              PREFIXED SAVE AREA         *JLM* 
         CVT    DSECT=YES              COMM VECT TABLE            *JLM* 
         IEESMCA                       SMF CONTROL TABLE          *JLM* 
         POP PRINT                                                      
*---------------------------------------------------------------------* 
*           STEFAN NEUMANN                                            * 
*           SYSTEMS PROGRAMMER                                        * 
*           SUN ALLIANCE PLC (UK)      XEPHON 1987                    * 
*---------------------------------------------------------------------* 
         END                                                            
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//LKED.SYSLMOD DD DSN=SYS2.LINKLIB,DISP=SHR                             
//LKED.SYSIN   DD *                                                     
  SETCODE AC(1)                        12/2014 JLM                      
  NAME ZTIMER(R)                       12/2014 JLM                      
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -ASMFCL15 
//*                                                                     
//UPDATE16 EXEC PGM=IEBUPDTE,PARM=NEW                                   
//*                                                                     
//* ***************************************************************** * 
//* installs ZTIMER PROC into SYS2.PROCLIB                            * 
//* ***************************************************************** * 
//*                                                                     
//SYSPRINT DD  SYSOUT=*                                                 
//SYSUT2   DD  DISP=MOD,DSN=SYS2.PROCLIB                                
//SYSIN    DD  DATA,DLM='><'                                            
./ ADD NAME=ZTIMER                                                      
./ NUMBER NEW1=10,INCR=10                                               
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
//* INVOKE AUTOMATIC COMMAND/MESSAGE UTILITY                          * 
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
//ZTIMER  PROC MEM=ZTIMER,SOUT=A                                        
//REMINDR EXEC PGM=ZTIMER                                               
//SYSUDUMP DD  SYSOUT=&SOUT                                             
//SYSIN    DD  DISP=SHR,DSN=SYS2.CONTROL(&MEM)                          
./ ENDUP                                                                
><                                                                      
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE16 
//*                                                                     
//UPDATE17 EXEC PGM=IEBUPDTE,PARM=NEW                                   
//*                                                                     
//* ***************************************************************** * 
//* ALLOCATE USER CONTROL CARD LIBRARY (SYS2.CONTROL),                * 
//* INSTALLS ZTIMER COMMANDS INTO SYS2.CONTROL                        * 
//* ***************************************************************** * 
//*                                                                     
//SYSPRINT DD  SYSOUT=*                                                 
//SYSUT2   DD  DSN=SYS2.CONTROL,                                        
//             UNIT=3350,VOL=SER=MVS000,DISP=(,CATLG),                  
//             SPACE=(TRK,(30,5,20)),                                   
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120)                     
//SYSIN    DD  *                                                        
./ ADD NAME=ZTIMER                                                      
./ NUMBER NEW1=10,INCR=10                                               
*----------------------------------------------------------------------*
*             ZTIMER - AUTOMATIC REMINDER/COMMAND FACILITY             *
* COL 1 TO 8:  EITHER DATE IN FORMAT 'DD/MM/YY'                        *
*                 OR DATE IN FORMAT 'YY/DDD'                           *
*                 OR 3-CHAR WEEKDAY 'MON TUE WED THU FRI SAT SUN'      *
*                 OR WORD 'WKDAY' FOR ANY WEEKDAY (MON TO FRI)         *
*                 OR WORD 'WKEND' FOR WEEKEND (SAT AND SUN)            *
*                 OR WORD 'ALL' FOR EVERY DAY EXECUTION                *
*                 OR '*' IN COL 1 FOR A COMMENT CARD.                  *
* COL 10 TO 14:  EITHER TIME IN FORMAT 'HH.MM'                         *
*                 OR BLANKS FOR EXECUTION EVERY TIME.                  *
* COL 16 TO 18:  EITHER 'COM' TO ISSUE COMMAND                         *
*                 OR 'DSH' TO ISSUE A BRIGHT (NON-ROLLABLE) MESSAGE    *
*                 OR 'DSN' TO ISSUE A NORMAL (ROLLABLE) MESSAGE        *
* COL 19 TO 22:  EITHER THE SYSTEM-ID WHERE THE ACTION IS TO BE DONE   *
*                 OR BLANKS FOR ACTION ON ALL SYSTEMS                  *
* COL 24 TO 72:  EITHER THE MESSAGE TO BE DISPLAYED                    *
*                 OR THE COMMAND AS ENTERED AT THE MASTER CONSOLE.     *
*----------------------------------------------------------------------*
*...+....1....+....2....+....3....+....4....+....5....+....6....+....7..
*--------PURGE SYSOUT 'Z' & 'Y' OLDER THAN N DAYS, ALL SYSTEMS          
ALL      00.05 DSN     $OQ,Q=Z,CANCEL,D=5                               
ALL      00.05 COM     $OQ,Q=Z,CANCEL,D=5                               
ALL      00.05 DSN     $OQ,Q=Y,CANCEL,D=5                               
ALL      00.05 COM     $OQ,Q=Y,CANCEL,D=5                               
*                                                                       
./ ENDUP                                                                
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE17 
//*                                                                     
//ASMFCL18 EXEC ASMFCL,PARM.ASM='LIST,LOAD,NODECK,NOXREF',              
//             MAC1='SYS1.AMODGEN',                                     
//             PARM.LKED='XREF,LET,LIST,NCAL'                           
//*                                                                     
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
//* INSTALLS CLIP INTO SYS2.LINKLIB                                   * 
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
//*                                                                     
//ASM.SYSIN DD *                                                        
         PRINT OFF                                                      
         MACRO                                                          
         YREGS                                                          
         GBLA  &REGS                                                    
&REGS    SETA  1                                                        
         SPACE 1                                                        
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
         SPACE 1                                                        
         MEND                                                           
         PRINT ON,NOGEN                                                 
*                              SOURCE: XEPHON MVS UDPATE 12/1987        
         TITLE 'CLIP - CONSOLE CLIP PROGRAM'                            
*=====================================================================* 
*  CLIP - CONSOLE CLIP PROGRAM                                        * 
*     EXEC PGM=CLIP,PARM='UUU/VVVVVV'                                 * 
*        UUU IS UNIT ADDRESS (WHICH MUST BE OFFLINE)                  * 
*     VVVVVV IS NEW VOLUME SERIAL NUMBER                              * 
*     RETURN CODE 28 MEANS: THE PARM WAS IN ERROR                     * 
*                           THE SYSIN FILE DID NOT OPEN               * 
*                           THE SYSPRINT FILE DID NOT OPEN            * 
*     ANY OTHER RETURN CODE IS FROM ICKDSF                            * 
*  THIS PROGRAM MUST BE LINK EDITED WITH AC=1 AND PLACED              * 
*  IN AN APF AUTHORIZED LIBRARY.                                      * 
*=====================================================================* 
         PRINT OFF                                                      
         DCBD  DSORG=PS,DEVD=DA                                         
         PRINT ON,NOGEN                                                 
*   ENTRY LINKAGE                                                       
CLIP     CSECT                                                          
         USING CLIP,R15            SET UP TEMPORARY BASE                
         B     AROUNDID            BRANCH AROUND EYECATCHER             
         DC    AL1(ENDID-*-1)      LENGTH OF EYECATCHER                 
         DC    C'CLIP &SYSDATE. &SYSTIME. '                             
ENDID    EQU   *                                                        
AROUNDID DS    0H                                                       
         STM   R14,R12,12(R13)     SAVE REGISTERS                       
         LR    R12,R15             SET UP NEW BASE                      
         USING CLIP,R12            ...                                  
         DROP  R15                 ...                                  
         LA    R15,SAVEAREA        LINK SAVE AREAS                      
         ST    R15,8(R13)          HSA -> LSA                           
         ST    R13,4(R15)          LSA -> HSA                           
         LR    R13,R15             POINT TO OUR SAVE AREA               
         LR    R2,R1               SAVE PARMLIST POINTER                
*   PARSE THE JCL PARAMETER:  UUU/VVVVVV                                
         L     R4,0(R2)            R4 -> JCL PARM LENGTH                
         LH    R5,0(R4)            R5 = LENGTH OF JCL PARM              
         LA    R4,2(R4)            R4 -> JCL PARM DATA                  
*                                                                       
*   GET THE UNIT ADDRESS                                                
         LR    R1,R4               SAVE START OF PARM                   
UNITLOOP LTR   R5,R5               ARE WE OUT OF PARM?                  
         BNP   UNITLEND            YES...QUIT THE LOOP                  
         CLI   0(R4),C'/'          IS THIS A SLASH?                     
         BE    UNITLEND            YES...QUIT THE LOOP                  
         LA    R4,1(R4)            NO...POINT TO THE NEXT CHARACTER     
         BCTR  R5,0                DOCK THE LENGTH                      
         B     UNITLOOP            AND TRY AGAIN                        
UNITLEND DS    0H                  -                                    
         LR    R15,R4              GET THE LENGTH                       
         SR    R15,R1              OF THE FIRST FIELD                   
         C     R15,=F'3'           IS IT RIGHT?                         
         BNE   BADPARM             NO...BAG IT                          
         MVC   UNIT,0(R1)          SAVE THE UNIT                        
         LA    R4,1(R4)            POINT PAST THE SLASH                 
         BCTR  R5,0                AND TRIM THE LENGTH                  
*   GET THE VOLUME SERIAL NUMBER                                        
         LR    R1,R4               SAVE THE START OF THE FIELD          
VOLLOOP  LTR   R5,R5               ARE WE OUT OF PARM?                  
         BNP   VOLLEND             YES...QUIT THE LOOP                  
         LA    R4,1(R4)            NO...POINT TO THE NEXT CHARACTER     
         BCTR  R5,0                DOCK THE LENGTH                      
         B     VOLLOOP             AND TRY AGAIN                        
VOLLEND  DS    0H                  -                                    
         LR    R15,R4              GET THE LENGTH                       
         SR    R15,R1              OF THE FIRST FIELD                   
         BNP   BADPARM             ZERO LENGTH IS BAD                   
         C     R15,=F'6'           IS IT TOO BIG?                       
         BH    BADPARM             YES...BAG IT                         
         BCTR  R15,0               DECREMENT LENGTH FOR EXECUTE         
         MVI   VOLUME,C' '         BLANK OUT THE VOLSER SLOT            
         MVC   VOLUME+1(L'VOLUME-1),VOLUME  ...                         
MVCINS1  MVC   VOLUME(*-*),0(R1)   *** EXECUTED INSTRUCTION ***         
         EX    R15,MVCINS1         MOVE THE VOLUME SERIAL NUMBER        
         B     CREATEDS            GO CREATE THE DATASET                
*   FLAG  BAD JCL PARAMETER                                             
BADPARM  WTO   'CLIP: THE FORMAT OF THE JCL PARAMETER IS INCORRECT',   X
               ROUTCDE=2                                                
         LA    R15,28              SET BAD RETURN CODE                  
         B     EXIT                GO BAIL OUT                          
CREATEDS DS    0H                  -                                    
         OPEN  (SYSIN,(OUTPUT))    OPEN THE DATASET                     
         TM    SYSIN+(DCBOFLGS-IHADCB),DCBOFOPN  DID IT OPEN?           
         BZ    NOSYSIN             NO...ERROR                           
*   PUT THE ICKDSF CONTROL CARD                                         
         PUT   SYSIN,DSFCARD       PUT THE CARD                         
*   CLOSE THE FILE                                                      
         CLOSE (SYSIN,)            CLOSE IT                             
         B     CALLPROG            GO CALL ICKDSF                       
*   FLAG OPEN ERROR ON SYSIN FILE                                       
NOSYSIN  WTO   'CLIP: THE SYSIN FILE DID NOT OPEN',                    X
               ROUTCDE=2                                                
         LA    R15,28              SET BAD RETURN CODE                  
         B     EXIT                GO BAIL OUT                          
CALLPROG DS    0H                  -                                    
         LA    R1,DUMMYPRM         POINT TO DUMMY PARM                  
         LINK  EP=ICKDSF           CALL THE PROGRAM                     
         LR    R3,R15              SAVE THE RETURN CODE                 
         CVD   R3,DWORK            CONVERT THE RETURN CODE TO DECIMAL   
         MVC   RETCODE,=XL4'40202120'  SET UP RETURN CODE EDIT MASK     
         ED    RETCODE,DWORK+6     EDIT IN THE RETURN CODE              
RCMSG    WTO   'CLIP: ICKDSF RETURN CODE ISXXXX',                      X
               ROUTCDE=2                                                
RETCODE  EQU   RCMSG+8+27,4,C'C'   MESSAGE INSERT FOR RETURN CODE       
SCANSYSP DS    0H                  -                                    
*                                                                       
         OPEN  (SYSPRINT,(INPUT))     OPEN THE DATASET                  
         TM    SYSPRINT+(DCBOFLGS-IHADCB),DCBOFOPN  DID IT OPEN?        
         BZ    NOSYSPR             NO...ERROR                           
*   READ ALL THE SYSPRINT RECORDS                                       
         GET   SYSPRINT            GET THE FIRST RECORD                 
READLOOP DS    0H                  -                                    
         LH    R15,0(R1)           R15 = LENGTH OF RECORD               
         C     R15,=F'9'           IS IT LONG ENOUGH TO BE A MESSAGE?   
         BL    SKIPREC             NO...SKIP IT                         
         LA    R1,5(R1)            POINT PAST THE RDW AND CARRIAGE CTL  
         S     R15,=F'5'           AND ADJUST LENGTH                    
         CLC   =CL3'ICK',0(R1)     IS THIS A POTENTIAL ICK MESSAGE?     
         BNE   SKIPREC             NO...SKIP IT                         
         CLI   3(R1),C'0'          IS IT ICKN, WHERE N IS NUMERIC?      
         BL    SKIPREC             NO...SKIP IT                         
         MVI   DSFMSG,C' '         CLEAR OUT THE MESSAGE SLOT           
         MVC   DSFMSG+1(L'DSFMSG-1),DSFMSG  ...                         
         C     R15,=A(L'DSFMSG)    IS THE LENGTH TOO BIG?               
         BNH   OKLEN               NO...LEAVE IT                        
         L     R15,=A(L'DSFMSG)    YES...MAKE IT RIGHT                  
OKLEN    DS    0H                  -                                    
         BCTR  R15,0               DECREMENT FOR EXECUTE                
MVCINS3  MVC   DSFMSG(*-*),0(R1)   *** EXECUTED INSTRUCTION ***         
         EX    R15,MVCINS3         MOVE THE MESSAGE TO THE WTO          
DSFWTO   WTO   'CLIP: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
               XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX',ROUTCDE=2               
DSFMSG   EQU   DSFWTO+8+6,80,C'C'  MESSAGE INSERT FOR ICKDSF MESSAGE    
SKIPREC  DS    0H                  -                                    
         GET   SYSPRINT            GET THE NEXT                         
         B     READLOOP            AND TRY AGAIN                        
SYSEOF   DS    0H                  -                                    
*   CLOSE THE FILE                                                      
         CLOSE (SYSPRINT,)         CLOSE IT                             
         LR    R15,R3              PUT BACK THE RETURN CODE             
         B     EXIT                GO BAIL OUT                          
*   FLAG OPEN ERROR ON SYSPRINT FILE                                    
NOSYSPR  WTO   'CLIP: THE SYSPRINT FILE DID NOT OPEN',                 X
               ROUTCDE=2                                                
         LA    R15,28              SET BAD RETURN CODE                  
         B     EXIT                GO BAIL OUT                          
EXIT     DS    0H                  -                                    
         L     R13,4(R13)          POINT TO CALLER'S SAVE AREA          
         L     R14,12(R13)         RESTORE REGISTER 14                  
         LM    R0,R12,20(R13)      RESTORE REGISTERS 0 - 12             
         MVI   12(R13),X'FF'       FLAG SAVE AREA                       
         BR    R14                 RETURN TO O.S.                       
DWORK    DS    D                   DOUBLEWORD WORK AREA                 
SAVEAREA DS    18F                 SAVEAREA                             
DSFCARD  DC    CL80' '             ICKDSF CONTROL CARD                  
         ORG   DSFCARD             ORG BACK TO REDEFINE FIELDS          
         DC    C'  REFORMAT UNITADDRESS('                               
UNIT     DC    C'UUU'              UNIT ADDRESS                         
         DC    C') VOLID('                                              
VOLUME   DC    C'VVVVVV'           VOLUME SERIAL NUMBER                 
         DC    C') NOVERIFY'                                            
         ORG   ,                   RESET LOCATION COUNTER               
DUMMYPRM DS    H'0'                DUMMY PARM FOR ICKDSF                
SYSIN    DCB   DDNAME=SYSIN,                                           X
               MACRF=(PM),                                             X
               DSORG=PS,                                               X
               RECFM=F,                                                X
               LRECL=80,                                               X
               BLKSIZE=80                                               
SYSPRINT DCB   DDNAME=SYSPRINT,                                        X
               MACRF=(GL),                                             X
               DSORG=PS,                                               X
               RECFM=VBA,                                              X
               EODAD=SYSEOF                                             
         LTORG ,                                                        
         YREGS                                                          
         END   CLIP                                                     
//*                                                                     
//LKED.SYSLMOD DD DSN=SYS2.LINKLIB,DISP=SHR                             
//LKED.SYSIN   DD *                                                     
  SETCODE AC(1)                        12/2014 JLM                      
  NAME CLIP(R)                         12/2014 JLM                      
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -ASMFCL18 
//*                                                                     
//UPDATE19 EXEC PGM=IEBUPDTE,PARM=NEW                                   
//*                                                                     
//* ***************************************************************** * 
//* INSTALLS CLIP PROCEDURE INTO SYS2.PROCLIB                         * 
//* ***************************************************************** * 
//*                                                                     
//SYSPRINT DD  SYSOUT=*                                                 
//SYSUT2   DD  DISP=MOD,DSN=SYS2.PROCLIB                                
//SYSIN    DD  DATA,DLM='><'                                            
./ ADD NAME=CLIP                                                        
./ NUMBER NEW1=10,INCR=10                                               
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
//* INVOKE CLIP UTILITY WHICH USES ICKDSF TO RE-LABEL OFFLINE DASD    * 
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
//CLIP     PROC U=,V=        U IS THE UNIT, V IS THE NEW VOLSER         
//CLIP     EXEC PGM=CLIP,PARM='&U./&V.'                                 
//SYSIN    DD   UNIT=SYSDA,SPACE=(TRK,(1,1))                            
//SYSPRINT DD   UNIT=SYSDA,SPACE=(TRK,(1,1))                            
./ ENDUP                                                                
><                                                                      
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE19 
//*                                                                     
//UPDATE20 EXEC PGM=IEBGENER                                            
//*                                                                     
//* ***************************************************************** * 
//* CREATE IEAPAK00 FROM IEAPAKBA                                     * 
//* ***************************************************************** * 
//*                                                                     
//SYSPRINT DD  SYSOUT=*                                                 
//SYSUT1   DD  DSN=SYS1.PARMLIB(IEAPAKBA),DISP=SHR                      
//SYSUT2   DD  DSN=SYS1.PARMLIB(IEAPAK00),DISP=SHR                      
//SYSIN    DD  DUMMY                                                    
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE20 
//*        END OF PRIMARY SYSGEN04                                      
