//SYSGEN01 JOB (SYSGEN),'ASSEMBLE STAGE 1',                             
//             CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1)                        
/*JOBPARM  LINES=100                                                    
//*                                                                     
//********************************************************************* 
//* VERIFY STAGE 1 DECK AND PRODUCE STAGE 2 JOBSTREAMS                * 
//*   1) DELETE OUTPUT FILE ON WORK01, IF IT EXISTS                   * 
//*   2) ASSEMBLE STAGE 1 DECK                                        * 
//*   3) IF ASSEMBLY HAS NO ERRORS, SUBMIT SECONDARY JOB TO COPY      * 
//*      OUTPUT FROM STAGE 1 ASSEMBLY ONTO TAPE                       * 
//********************************************************************* 
//*                                                                     
//CLEANUP  EXEC PGM=IEHPROGM                                            
//*                                                                     
//* ***************************************************************** * 
//* DELETE STAGE 1 OUTPUT FROM PRIOR RUN (IF ANY)                     * 
//* ***************************************************************** * 
//*                                                                     
//SYSPRINT DD  SYSOUT=*                                                 
//DD1      DD  UNIT=3350,VOL=SER=WORK01,DISP=OLD                        
//SYSIN    DD  *                                                        
  SCRATCH DSNAME=SYS1.STAGE1.OUTPUT,VOL=3350=WORK01,PURGE               
//*                                                                     
//ASMBLR   EXEC PGM=ASMBLR,PARM='LIST,NOLOAD,DECK,NOXREF',REGION=2056K  
//*                                                                     
//* ***************************************************************** * 
//* ASSEMBLE STAGE 1                                                  * 
//* ***************************************************************** * 
//*                                                                     
//SYSPRINT DD  SYSOUT=*                                                 
//SYSLIB   DD  DSN=SYS1.AMODGEN,DISP=SHR                                
//         DD  DSN=SYS1.AGENLIB,DISP=SHR                                
//SYSPUNCH DD  DSN=SYS1.STAGE1.OUTPUT,DISP=(NEW,KEEP),                  
//             UNIT=3350,VOL=SER=WORK01,                                
//             SPACE=(TRK,(30,30),RLSE),                                
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120)                     
//SYSUT1   DD  UNIT=SYSDA,SPACE=(CYL,(35,10))                           
//SYSUT2   DD  UNIT=SYSDA,SPACE=(CYL,(35,10))                           
//SYSUT3   DD  UNIT=SYSDA,SPACE=(CYL,(50,10))                           
//SYSIN    DD  *                                                        
         PRINT ON,NOGEN,NODATA                                          
*                                                                       
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*                      INPUT/OUTPUT CHANNELS                          * 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*                                                                       
MPLXR    CHANNEL                                                       C
               ADDRESS=(0),                                            C
               TYPE=MULTIPLEXOR                                         
*                                                                       
SELECTOR CHANNEL                                                       C
               ADDRESS=(1,2,3,4,5,6),                                  C
               TYPE=SELECTOR                                            
         EJECT                                                          
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*                         SYSTEM CONSOLES                             * 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*                                                                       
CONSMST  CONSOLE                                                       C
               MCONS=010,                                              C
               ALTCONS=009,                                            C
               AREA=04,                                                C
               PFK=12,                                                 C
               ROUTCDE=ALL                                              
*                                                                       
CONSALT  CONSOLE                                                       C
               SECONS=009,                                             C
               ALTCONS=010,                                            C
               ROUTCDE=ALL                                              
* Removed 'AREA=04,PFK=12,'                                            
*                                                                       
CONSHC   CONSOLE                                                       C
               SECONS=O-015,                                           C
               ALTCONS=009,                                            C
               ROUTCDE=ALL                                              
*                                                                       
MSTCONS  IODEVICE                                                      C
               UNIT=3277,                                              C
               MODEL=2,                                                C
               ADDRESS=010,                                            C
               FEATURE=(EBKY3277,DOCHAR,KB78KEY,AUDALRM,NUMLOCK,       C
               SELPEN,PTREAD)                                           
*                                                                       
ALTCONS  IODEVICE                                                      C
               UNIT=3215,                                              C
               ADDRESS=009                                             
* Removed MODEL and FEATURE
*                                                                       
HCCONS   IODEVICE                                                      C
               UNIT=1403,                                              C
               MODEL=2,                                                C
               ADDRESS=015                                              
         EJECT                                                          
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*                          CHANNEL 0 DEVICES                          * 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*                                                                       
RDR00A   IODEVICE                                                      C
               UNIT=3505,                                              C
               ADDRESS=00A,                                            C
               FEATURE=CARDIMAGE                                        
*                                                                       
PCH00B   IODEVICE                                                      C
               UNIT=3525,                                              C
               ADDRESS=00B,                                            C
               FEATURE=CARDIMAGE                                        
*                                                                       
RDR00C   IODEVICE                                                      C
               UNIT=2540R,                                             C
               MODEL=1,                                                C
               ADDRESS=00C,                                            C
               FEATURE=CARDIMAGE                                        
*                                                                       
PCH00D   IODEVICE                                                      C
               UNIT=2540P,                                             C
               MODEL=1,                                                C
               ADDRESS=00D,                                            C
               FEATURE=CARDIMAGE                                        
*                                                                       
PRT00E   IODEVICE                                                      C
               UNIT=1403,                                              C
               MODEL=N1,                                               C
               ADDRESS=00E,                                            C
               FEATURE=UNVCHSET                                         
*                                                                       
PRT00F   IODEVICE                                                      C
               UNIT=3211,                                              C
               ADDRESS=00F                                              
*                                                                       
RDR01A   IODEVICE                                                      C
               UNIT=2540R,                                             C
               MODEL=1,                                                C
               ADDRESS=01A,                                            C
               FEATURE=CARDIMAGE                                        
*                                                                       
PCH01B   IODEVICE                                                      C
               UNIT=2540P,                                             C
               MODEL=1,                                                C
               ADDRESS=01B,                                            C
               FEATURE=CARDIMAGE                                        
*                                                                       
RDR01C   IODEVICE                                                      C
               UNIT=2540R,                                             C
               MODEL=1,                                                C
               ADDRESS=01C,                                            C
               FEATURE=CARDIMAGE                                        
*                                                                       
PCH01D   IODEVICE                                                      C
               UNIT=2540P,                                             C
               MODEL=1,                                                C
               ADDRESS=01D,                                            C
               FEATURE=CARDIMAGE                                        
*                                                                       
PRT01E   IODEVICE                                                      C
               UNIT=1403,                                              C
               MODEL=N1,                                               C
               ADDRESS=01E,                                            C
               FEATURE=UNVCHSET                                         
*                                                                       
PRT01F   IODEVICE                                                      C
               UNIT=3211,                                              C
               ADDRESS=01F                                              
         EJECT                                                          
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*                          CHANNEL 1 DEVICES                          * 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*                                                                       
T2401@1  IODEVICE                                                      C
               UNIT=2401,                                              C
               MODEL=6,                                                C
               ADDRESS=(100,4),                                        C
               FEATURE=(9-TRACK,DUALDENS)                               
*                                                                       
T3420@1  IODEVICE                                                      C
               UNIT=3420,                                              C
               MODEL=8,                                                C
               ADDRESS=(110,4),                                        C
               FEATURE=(9-TRACK,OPT1600)                                
*                                                                       
D2314@1  IODEVICE                                                      C
               UNIT=2314,                                              C
               ADDRESS=(120,8)                                          
*                                                                       
D33301@1 IODEVICE                                                      C
               MODEL=1,                                                C
               UNIT=3330,                                              C
               ADDRESS=(130,8)                                          
*                                                                       
D33302@1 IODEVICE                                                      C
               MODEL=11,                                               C
               UNIT=3330,                                              C
               ADDRESS=(138,8)                                          
*                                                                       
D3340@1  IODEVICE                                                      C
               UNIT=3340,                                              C
               ADDRESS=(140,8)                                          
*                                                                       
D3350@1  IODEVICE                                                      C
               UNIT=3350,                                              C
               ADDRESS=(150,8)                                          
*                                                                       
D3375@1  IODEVICE                                                      C
               UNIT=3375,                                              C
               ADDRESS=(170,8)                                          
*                                                                       
D3380@1  IODEVICE                                                      C
               UNIT=3380,                                              C
               ADDRESS=(180,8)                                          
*                                                                       
D3390@1  IODEVICE                                                      C
               UNIT=3390,                                              C
               ADDRESS=(190,8)                                          
         EJECT                                                          
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*                          CHANNEL 2 DEVICES                          * 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*                                                                       
T2401@2  IODEVICE                                                      C
               UNIT=2401,                                              C
               MODEL=6,                                                C
               ADDRESS=(200,4),                                        C
               FEATURE=(9-TRACK,DUALDENS)                               
*                                                                       
T3420@2  IODEVICE                                                      C
               UNIT=3420,                                              C
               MODEL=8,                                                C
               ADDRESS=(210,4),                                        C
               FEATURE=(9-TRACK,OPT1600)                                
*                                                                       
D2314@2  IODEVICE                                                      C
               UNIT=2314,                                              C
               ADDRESS=(220,8)                                          
*                                                                       
D33301@2 IODEVICE                                                      C
               MODEL=1,                                                C
               UNIT=3330,                                              C
               ADDRESS=(230,8)                                          
*                                                                       
D33302@2 IODEVICE                                                      C
               MODEL=11,                                               C
               UNIT=3330,                                              C
               ADDRESS=(238,8)                                          
*                                                                       
D3340@2  IODEVICE                                                      C
               UNIT=3340,                                              C
               ADDRESS=(240,8)                                          
*                                                                       
D3350@2  IODEVICE                                                      C
               UNIT=3350,                                              C
               ADDRESS=(250,8)                                          
*                                                                       
D3375@2  IODEVICE                                                      C
               UNIT=3375,                                              C
               ADDRESS=(270,8)                                          
*                                                                       
D3380@2  IODEVICE                                                      C
               UNIT=3380,                                              C
               ADDRESS=(280,8)                                          
*                                                                       
D3390@2  IODEVICE                                                      C
               UNIT=3390,                                              C
               ADDRESS=(290,8)                                          
         EJECT                                                          
*                                                                       
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*                          CHANNEL 3 DEVICES                          * 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*                                                                       
T2401@3  IODEVICE                                                      C
               UNIT=2401,                                              C
               MODEL=6,                                                C
               ADDRESS=(300,4),                                        C
               FEATURE=(9-TRACK,DUALDENS)                               
*                                                                       
T3420@3  IODEVICE                                                      C
               UNIT=3420,                                              C
               MODEL=8,                                                C
               ADDRESS=(310,4),                                        C
               FEATURE=(9-TRACK,OPT1600)                                
*                                                                       
D2314@3  IODEVICE                                                      C
               UNIT=2314,                                              C
               ADDRESS=(320,8)                                          
*                                                                       
D33301@3 IODEVICE                                                      C
               MODEL=1,                                                C
               UNIT=3330,                                              C
               ADDRESS=(330,8)                                          
*                                                                       
D33302@3 IODEVICE                                                      C
               MODEL=11,                                               C
               UNIT=3330,                                              C
               ADDRESS=(338,8)                                          
*                                                                       
D3340@3  IODEVICE                                                      C
               UNIT=3340,                                              C
               ADDRESS=(340,8)                                          
*                                                                       
D3350@3  IODEVICE                                                      C
               UNIT=3350,                                              C
               ADDRESS=(350,8)                                          
*                                                                       
D3375@3  IODEVICE                                                      C
               UNIT=3375,                                              C
               ADDRESS=(370,8)                                          
*                                                                       
D3380@3  IODEVICE                                                      C
               UNIT=3380,                                              C
               ADDRESS=(380,8)                                          
*                                                                       
D3390@3  IODEVICE                                                      C
               UNIT=3390,                                              C
               ADDRESS=(390,8)                                          
         EJECT                                                          
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*                          CHANNEL 4 DEVICES                          * 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
T3278@4  IODEVICE                                                      C
               UNIT=3277,                                              C
               MODEL=2,                                                C
               ADDRESS=(400,32),                                       C
               FEATURE=(EBKY3277,DOCHAR,KB78KEY,AUDALRM,NUMLOCK,PTREAD) 
T3286@4  IODEVICE                                                      C
               UNIT=3286,                                              C
               MODEL=2,                                                C
               ADDRESS=(420,8),                                        C
               FEATURE=(PTREAD,DOCHAR)                                  
         EJECT                                                          
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*                          CHANNEL 5 DEVICES                          * 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
CTC1@5   IODEVICE                                                      C
               UNIT=CTC,                                               C
               ADDRESS=(500,4)                                          
*                                                                       
CTC2@5   IODEVICE                                                      C
               UNIT=CTC,                                               C
               FEATURE=(370),                                          C
               ADDRESS=(510,4)                                          
         EJECT                                                          
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*                          CHANNEL 6 DEVICES                          * 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
C37051@6 IODEVICE                                                      C
               UNIT=3705,                                              C
               ADAPTER=CA1,                                            C
               ADDRESS=600                                              
*                                                                       
C37052@6 IODEVICE                                                      C
               UNIT=3705,                                              C
               ADAPTER=CA2,                                            C
               ADDRESS=601                                              
*                                                                       
BSC1@6   IODEVICE                                                      C
               UNIT=BSC1,                                              C
               ADAPTER=BSCA,                                           C
               TCU=2703,                                               C
               ADDRESS=602                                              
*                                                                       
BSC2@6   IODEVICE                                                      C
               UNIT=BSC2,                                              C
               ADAPTER=BSCA,                                           C
               TCU=2703,                                               C
               FEATURE=(AUTOANSR,AUTOCALL),                            C
               ADDRESS=603                                              
*                                                                       
BSC3@6   IODEVICE                                                      C
               UNIT=BSC3,                                              C
               ADAPTER=BSCA,                                           C
               TCU=2703,                                               C
               ADDRESS=604                                              
*                                                                       
T2740C@6  IODEVICE                                                     C
               UNIT=2740C,                                             C
               ADAPTER=IBM1,                                           C
               TCU=2701,                                               C
               FEATURE=(AUTOANSR,CHECKING),                            C
               ADDRESS=605                                              
*                                                                       
T2740X@6  IODEVICE                                                     C
               UNIT=2740X,                                             C
               ADAPTER=IBM1,                                           C
               TCU=2701,                                               C
               FEATURE=(AUTOANSR,CHECKING),                            C
               ADDRESS=606                                              
*                                                                       
T2741P@6  IODEVICE                                                     C
               UNIT=2741P,                                             C
               ADAPTER=IBM1,                                           C
               TCU=2701,                                               C
               FEATURE=(AUTOANSR),                                     C
               ADDRESS=607                                              
*                                                                       
T2741C@6  IODEVICE                                                     C
               UNIT=2741C,                                             C
               ADAPTER=IBM1,                                           C
               TCU=2701,                                               C
               FEATURE=(AUTOANSR),                                     C
               ADDRESS=608                                              
*                                                                       
T1030@6   IODEVICE                                                     C
               UNIT=1030,                                              C
               ADAPTER=IBM2,                                           C
               TCU=2701,                                               C
               ADDRESS=609                                              
*                                                                       
T115A@6   IODEVICE                                                     C
               UNIT=115A,                                              C
               ADAPTER=TELE1,                                          C
               TCU=2701,                                               C
               FEATURE=(AUTOANSR),                                     C
               ADDRESS=60A                                              
*                                                                       
T83B3@6   IODEVICE                                                     C
               UNIT=83B3,                                              C
               ADAPTER=TELE1,                                          C
               TCU=2701,                                               C
               FEATURE=(AUTOANSR),                                     C
               ADDRESS=60B                                              
*                                                                       
T2265@6   IODEVICE                                                     C
               UNIT=2265,                                              C
               ADAPTER=IBM3,                                           C
               TCU=2701,                                               C
               FEATURE=(AUTOANSR),                                     C
               ADDRESS=60C                                              
*                                                                       
TWX@6    IODEVICE                                                      C
               UNIT=TWX,                                               C
               ADAPTER=TELE2,                                          C
               TCU=2703,                                               C
               FEATURE=(AUTOANSR),                                     C
               ADDRESS=(60D,4)                                          
*                                                                       
WTTA@6   IODEVICE                                                      C
               UNIT=WTTA,                                              C
               ADAPTER=TELEW,                                          C
               TCU=2703,                                               C
               FEATURE=(AUTOANSR),                                     C
               ADDRESS=(611,4)                                          
         EJECT                                                          
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*                      SYMBOLIC DEVICE NAMES                          * 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
SYSRDR   UNITNAME                                                      C
               NAME=SYSRDR,                                            C
               UNIT=(00A)                                               
*                                                                       
SYSDA    UNITNAME                                                      C
               NAME=SYSDA,                                             C
               UNIT=((130,8),(138,8),(140,8),(150,8),                  C
               (170,8),(180,8),(190,8))                                 
*                                                                       
SYSDA    UNITNAME                                                      C
               NAME=SYSDA,                                             C
               UNIT=((230,8),(238,8),(240,8),(250,8),                  C
               (270,8),(280,8),(290,8))                                 
*                                                                       
SYSDA    UNITNAME                                                      C
               NAME=SYSDA,                                             C
               UNIT=((330,8),(338,8),(340,8),(350,8),                  C
               (370,8),(380,8),(390,8))                                 
*                                                                       
SORTDA   UNITNAME                                                      C
               NAME=SORTDA,                                            C
               UNIT=((120,8),(220,8),(320,8))                           
*                                                                       
SYSALLDA UNITNAME                                                      C
               NAME=SYSALLDA,                                          C
               UNIT=((120,8),(220,8),(320,8))                           
*                                                                       
SYSALLDA UNITNAME                                                      C
               NAME=SYSALLDA,                                          C
               UNIT=((130,8),(138,8),(140,8),(150,8),                  C
               (170,8),(180,8),(190,8))                                 
*                                                                       
SYSALLDA UNITNAME                                                      C
               NAME=SYSALLDA,                                          C
               UNIT=((230,8),(238,8),(240,8),(250,8),                  C
               (270,8),(280,8),(290,8))                                 
*                                                                       
SYSALLDA UNITNAME                                                      C
               NAME=SYSALLDA,                                          C
               UNIT=((330,8),(338,8),(340,8),(350,8),                  C
               (370,8),(380,8),(390,8))                                 
*                                                                       
TAPE     UNITNAME                                                      C
               NAME=TAPE,                                              C
               UNIT=((100,4),(110,4),(200,4),(210,4),(300,4),(310,4))   
*                                                                       
SYSSQ    UNITNAME                                                      C
               NAME=SYSSQ,                                             C
               UNIT=((100,4),(110,4),(200,4),(210,4),(300,4),(310,4),  C
               (130,8),(138,8),(140,8),(150,8),                        C
               (170,8),(180,8),(190,8),                                C
               (230,8),(238,8),(240,8),(250,8),                        C
               (270,8),(280,8),(290,8),                                C
               (330,8),(338,8),(340,8),(350,8),                        C
               (370,8),(380,8),(390,8))                                 
*                                                                       
T2401    UNITNAME                                                      C
               NAME=T2401,                                             C
               UNIT=((100,4),(200,4),(300,4))                           
*                                                                       
T3420    UNITNAME                                                      C
               NAME=T3420,                                             C
               UNIT=((110,4),(210,4),(310,4))                           
         EJECT                                                          
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*                         VIRTUAL I/O DEVICES                         * 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
* THE DEVICES SPECIFIED FOR THE VIO UNITNAME ARE 3350.  IF VIO IS USED  
* THE EMULATED DEVICE WILL TAKE ON THE CHARACTERISTICS OF A 3350.       
*                                                                       
VIO      UNITNAME                                                      C
               NAME=VIO,                                               C
               VIO=YES,                                                C
               UNIT=((150,8),(250,8),(350,8))                           
         EJECT                                                          
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*                       CONTROL PROGRAM OPTIONS                       * 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*                                                                       
CTLPG    CTRLPROG ASCII=INCLUDE,     ASCII TRANSLATE ROUTINE           C
               OPTIONS=(RER,         REDUCED ERROR RECOVERY            C
               DEVSTAT,              OFFLINE NOT READY DEVICES AT IPL  C
               RDE,                  LOGREC DATA EXTRACTOR             C
               BLDL),                BLDL IN FIXED STORAGE             C
               SQA=3,                # 64K ADDITIONAL SQA BLOCKS       C
               REAL=128,             # 1K V=R BLOCKS                   C
               STORAGE=0,            DETERMINE MAX REAL DYNAMICALLY    C
               WARN=0,               IGNORE POWER WARN FEATURE         C
               ACRCODE=YES,          ALTERNATE PROCESSOR RECOVERY      C
               APFLIB=(SYS1.VTAMLIB,MVSRES, VTAM REQUIRES              C
               SYS1.INDMAC,MVSRES),         IND=YES REQUIRES           C
               CSA=2048,             # 1K BLOCKS CSA                   C
               VRREGN=64,            DEFAULT V=R REGION                C
               TZ=(W,5)              ONE HOUR WEST OF GMT               
         EJECT                                                          
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*                     JOB SCHEDULER OPTIONS                           * 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*                                                                       
SCHDL    SCHEDULR BCLMT=100,              BROADCAST NOTICE LIMIT       C
               HARDCPY=(015,ALL,CMDS),    RECORD EVERYTHING ON HC LOG  C
               PRISUB=JES2,               JES2 SUBSYSTEM               C
               DEVPREF=(3350,3380,3330-1, DEVICE ALLOCATION PREFERENCE C
               3330,3390,3340)                                          
         EJECT                                                          
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*                           TSO OPTIONS                               * 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*                                                                       
TSOOPTS  TSO CMDS=YES,LOGLINE=4,LOGTIME=50                              
         EJECT                                                          
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*                      OPTIONAL ACCESS METHODS                        * 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*                                                                       
OPTAM    DATAMGT                                                       C
               ACSMETH=(BTAM,     BASIC TELECOMMUNICATIONS             C
               ISAM,              INDEXED SEQUENTIAL                   C
               TCAM,              TELECOMMUNICATIONS                   C
               VTAM,              VIRTUAL TELECOMMUNCATIONS            C
               GAM),              GRAPHICS                             C
               IND=YES,           3270 SUPPORT                         C
               TABLE=ALL,         ALL CHARACTER TABLES FOR 3800        C
               UCSDFLT=ALL        USE DEFAULT UNIV CHAR SET             
         EJECT                                                          
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*                         SYSTEM DATASETS                             * 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*                                                                       
BRODCAST DATASET BRODCAST,VOL=(MVSRES,3350),SPACE=(CYL,(1))             
CMDLIB   DATASET CMDLIB,VOL=(MVSRES,3350),SPACE=(CYL,(3,1,71))          
DCMLIB   DATASET DCMLIB,VOL=(MVSRES,3350),SPACE=(CYL,(1,,35))           
DUMP00   DATASET DUMP00,VOL=(MVSRES,3350),SPACE=(CYL,(30))              
DUMP01   DATASET DUMP01,VOL=(MVSRES,3350),SPACE=(CYL,(30))              
DUMP02   DATASET DUMP02,VOL=(MVSRES,3350),SPACE=(CYL,(30))              
HELP     DATASET HELP,VOL=(MVSRES,3350),SPACE=(CYL,(3,1,71))            
IMAGE    DATASET IMAGELIB,VOL=(MVSRES,3350),SPACE=(CYL,(1,,35))         
INDMAC   DATASET INDMAC,VOL=(MVSRES,3350),SPACE=(CYL,(3,1,71))          
LINKLIB  DATASET LINKLIB,VOL=(MVSRES,3350),SPACE=(CYL,(20,1,323))       
LPALIB   DATASET LPALIB,VOL=(MVSRES,3350),SPACE=(CYL,(20,1,360)),      C
               PDS=SYS2.LOCAL.LPALIB,                                  C
               MEMBERS=(IGC0024{,        SVC 240                       C
               IGC0024A,                 SVC 241                       C
               IGC0024B,                 SVC 242                       C
               IGC0024C,                 SVC 243                       C
               IGC0024D,                 SVC 244                       C
               IGC0024E,                 SVC 245                       C
               IGC0024F,                 SVC 246                       C
               IGC0024G,                 SVC 247                       C
               IGC0024H,                 SVC 248                       C
               IGC0024I,                 SVC 249                       C
               IGC0025{,                 SVC 250                       C
               IGC0025A,                 SVC 251                       C
               IGC0025B,                 SVC 252                       C
               IGC0025C,                 SVC 253                       C
               IGC0025D,                 SVC 254                       C
               IGC0025E)                 SVC 255                        
MACLIB   DATASET MACLIB,VOL=(MVSRES,3350),SPACE=(CYL,(30,1,107))        
MANX     DATASET MANX,VOL=(MVSRES,3350),SPACE=(CYL,(6))                 
MANY     DATASET MANY,VOL=(MVSRES,3350),SPACE=(CYL,(6))                 
NUCLEUS  DATASET NUCLEUS,VOL=(MVSRES,3350),SPACE=(CYL,(10,,20))         
PAGE01   DATASET PAGEDSN=SYS1.PAGELPA,                                 C
               VOL=(PAGE00,3350),SPACE=(CYL,(185))                      
PAGE02   DATASET PAGEDSN=SYS1.PAGECSA,                                 C
               VOL=(PAGE00,3350),SPACE=(CYL,(185))                      
PAGE03   DATASET PAGEDSN=SYS1.PAGEL00,                                 C
               VOL=(PAGE00,3350),SPACE=(CYL,(184))                      
STGINDX  DATASET STGINDEX,VOL=(MVSRES,3350),SPACE=(CYL,(2))             
PARMLIB  DATASET PARMLIB,VOL=(MVSRES,3350),SPACE=(CYL,(8,,40))          
PROCLIB  DATASET PROCLIB,VOL=(MVSRES,3350),SPACE=(CYL,(6,1,71))         
SAMPLIB  DATASET SAMPLIB,VOL=(MVSRES,3350),SPACE=(CYL,(10,1,20))        
SVCLIB   DATASET SVCLIB,VOL=(MVSRES,3350),SPACE=(CYL,(2,1,35))          
TCAMMAC  DATASET TCOMMAC,VOL=(MVSRES,3350),SPACE=(CYL,(3,1,35))         
TELCMLB  DATASET TELCMLIB,VOL=(MVSRES,3350),SPACE=(CYL,(4,1,71))        
UADS     DATASET UADS,VOL=(MVSRES,3350),SPACE=(CYL,(1,1,35))            
VSCATLG  DATASET VSCATLG,NAME=SYS1.VSAM.MASTER.CATALOG                  
VTAMLB   DATASET VTAMLIB,VOL=(MVSRES,3350),SPACE=(CYL,(4,1,35))         
         EJECT                                                          
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*                          USER SVCS                                  * 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*                                                                       
*                  + REQUIRED - SPECIFIES USER SVC AS A DECIMAL NUMBER; 
*                  | MUST BE UNIQUE AND WITHIN THE RANGE 200-255.       
*                  |                                                    
*                  |   + REQUIRED - SPECIFIES SVC TYPE AS 1, 2, 3, 4,   
*                  |   | 5, OR 6.                                       
*                  |   |                                                
*                  |   |  + OPTIONAL - SPECIFIES VALUE(S) TO INDICATE   
*                  |   |  | WHICH LOCKS TO OBTAIN BEFORE ENTRY TO SVC;  
*                  |   |  | L1=LOCAL, L2=CMS, L3=SRM, L4=SALLOC9,       
*                  |   |  | L5=DISPATCHER; INVALID FOR TYPE 6 SVC.      
*                  |   |  |                                             
*                  |   |  |  + OPTIONAL - CODE FC01 RESTRICTS USE OF    
*                  |   |  |  | THE SVC TO AUTHORIZED STEPS; IF NOT      
*                  |   |  |  | SPECIFIED, FC00 IS ASSUMED MAKING SVC    
*                  |   |  |  | UNRESTRICTED.                            
*                  |   |  |  |                                          
*                  |   |  |  |    + OPTIONAL - SPECIFIES THAT SVC RUNS  
*                  |   |  |  |    | NON-PREMPTIBLE FOR I/O INTERRUPTS.  
*                  |   |  |  |    |                                     
*                  V   V  V  V    V                                     
*     SVCTABLE SVC-255-T1-L1-FC00-NP                                    
         SVCTABLE                                                      C
               SVC-255-T3-L1-FC00,     OPEN                            C
               SVC-254-T4-FC00,        OPEN                            C
               SVC-253-T3-FC00,        OPEN                            C
               SVC-252-T4-FC00,        OPEN                            C
               SVC-251-T3-FC00,        OPEN                            C
               SVC-250-T4-FC00,        OPEN                            C
               SVC-249-T3-FC00,        OPEN                            C
               SVC-248-T4-FC00,        FSE                             C
               SVC-247-T3-FC00,        OPEN                            C
               SVC-246-T4-FC00,        OPEN                            C
               SVC-245-T3-FC00,        OPEN                            C
               SVC-244-T4-FC00,        AUTHORIZATION                   C
               SVC-243-T3-FC00,        OPEN                            C
               SVC-242-T4-FC00,        OPEN                            C
               SVC-241-T3-FC00,        OPEN                            C
               SVC-240-T4-FC00         OPEN                             
         EJECT                                                          
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*                           GENERATE                                  * 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*                                                                       
         GENERATE GENTYPE=ALL,        FULL SYSGEN                      C
               INDEX=SYS1,            HIGH LEVEL QUALIFIER FOR DS      C
               JCLASS=A,              JOB CLASS                        C
               OBJPDS=SYS1.OBJPDS,    OBJECT DATASETS                  C
               OCLASS=A,              OUTPUT CLASS                     C
               RESVOL=(MVSRES,3350)   SYSRES VOLUME                     
         END                                                            
/*                                                                      
//*                                                                     
//IEBGENER EXEC PGM=IEBGENER,COND=(0,NE,ASMBLR)                         
//*                                                                     
//* ***************************************************************** * 
//* IF ASMBLR RC=0000, SUBMIT CONTINUATION JOB TO INTERNAL READER.    * 
//* ***************************************************************** * 
//*                                                                     
//SYSPRINT DD  DUMMY                                                    
//SYSIN    DD  DUMMY                                                    
//SYSUT1   DD  DATA,DLM='><'                                            
//SYSGEN01 JOB (SYSGEN),'STAGE2 DECK TO TAPE',                          
//             CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1)                        
/*JOBPARM  CARDS=15000                                                  
//*                                                                     
//* ***************************************************************** * 
//* THIS JOB CONTINUATION SUBMITTED BY SUCCESSFUL SYSGEN01            * 
//* ***************************************************************** * 
//*                                                                     
//PUNCH   EXEC PGM=IEBGENER                                             
//*                                                                     
//* ***************************************************************** * 
//* COPY STAGE 1 OUTPUT DECK TO TAPE                                  * 
//* ***************************************************************** * 
//*                                                                     
//SYSPRINT DD  DUMMY                                                    
//SYSIN    DD  DUMMY                                                    
//SYSUT1   DD  DISP=SHR,DSN=SYS1.STAGE1.OUTPUT,UNIT=3350,VOL=SER=WORK01 
//SYSUT2   DD  UNIT=(TAPE,,DEFER),DSN=STAGE1.OUTPUT,VOL=SER=STAGE1,     
//             LABEL=(1,SL),DISP=(,KEEP)                                
//*        END OF SUBMITTED SYSGEN01                                    
><                                                                      
//SYSUT2   DD  SYSOUT=(A,INTRDR)                                        
//*         END OF PRIMARY SYSGEN01                                     
