//SMPJOB00 JOB (SYSGEN),'PREP FOR RECEIVE',                             
//             CLASS=A,MSGLEVEL=(1,1),MSGCLASS=A                        
//*                                                                     
//********************************************************************* 
//* PREPARE TO RECEIVE MVS3.8J PRODUCTS/PTFS/MODS                       
//*   1) DELETE EXISTING SMP DATASETS AND WORK DATASETS                 
//*   2) ALLOCATE SMP DATASETS AND INITIALIZE SMP TARGET ENVIRONMENT    
//********************************************************************* 
//*                                                                     
//IEHPROGM EXEC PGM=IEHPROGM                                            
//SYSPRINT  DD SYSOUT=*                                                 
//DD1       DD UNIT=SYSDA,VOL=SER=SMP000,DISP=OLD                       
//DD2       DD UNIT=SYSDA,VOL=SER=WORK01,DISP=OLD                       
//SYSIN     DD *                                                        
  UNCATLG DSNAME=SYS1.ACMDLIB                                           
  SCRATCH DSNAME=SYS1.ACMDLIB,VOL=SYSDA=SMP000,PURGE                    
  UNCATLG DSNAME=SYS1.AGENLIB                                           
  SCRATCH DSNAME=SYS1.AGENLIB,VOL=SYSDA=SMP000,PURGE                    
  UNCATLG DSNAME=SYS1.AHELP                                             
  SCRATCH DSNAME=SYS1.AHELP,VOL=SYSDA=SMP000,PURGE                      
  UNCATLG DSNAME=SYS1.AIMAGE                                            
  SCRATCH DSNAME=SYS1.AIMAGE,VOL=SYSDA=SMP000,PURGE                     
  UNCATLG DSNAME=SYS1.ALPALIB                                           
  SCRATCH DSNAME=SYS1.ALPALIB,VOL=SYSDA=SMP000,PURGE                    
  UNCATLG DSNAME=SYS1.AMACLIB                                           
  SCRATCH DSNAME=SYS1.AMACLIB,VOL=SYSDA=SMP000,PURGE                    
  UNCATLG DSNAME=SYS1.AMODGEN                                           
  SCRATCH DSNAME=SYS1.AMODGEN,VOL=SYSDA=SMP000,PURGE                    
  UNCATLG DSNAME=SYS1.AOSA0                                             
  SCRATCH DSNAME=SYS1.AOSA0,VOL=SYSDA=SMP000,PURGE                      
  UNCATLG DSNAME=SYS1.AOSA1                                             
  SCRATCH DSNAME=SYS1.AOSA1,VOL=SYSDA=SMP000,PURGE                      
  UNCATLG DSNAME=SYS1.AOSBN                                             
  SCRATCH DSNAME=SYS1.AOSBN,VOL=SYSDA=SMP000,PURGE                      
  UNCATLG DSNAME=SYS1.AOSB0                                             
  SCRATCH DSNAME=SYS1.AOSB0,VOL=SYSDA=SMP000,PURGE                      
  UNCATLG DSNAME=SYS1.AOSB3                                             
  SCRATCH DSNAME=SYS1.AOSB3,VOL=SYSDA=SMP000,PURGE                      
  UNCATLG DSNAME=SYS1.AOSCA                                             
  SCRATCH DSNAME=SYS1.AOSCA,VOL=SYSDA=SMP000,PURGE                      
  UNCATLG DSNAME=SYS1.AOSCD                                             
  SCRATCH DSNAME=SYS1.AOSCD,VOL=SYSDA=SMP000,PURGE                      
  UNCATLG DSNAME=SYS1.AOSCE                                             
  SCRATCH DSNAME=SYS1.AOSCE,VOL=SYSDA=SMP000,PURGE                      
  UNCATLG DSNAME=SYS1.AOSC2                                             
  SCRATCH DSNAME=SYS1.AOSC2,VOL=SYSDA=SMP000,PURGE                      
  UNCATLG DSNAME=SYS1.AOSC5                                             
  SCRATCH DSNAME=SYS1.AOSC5,VOL=SYSDA=SMP000,PURGE                      
  UNCATLG DSNAME=SYS1.AOSC6                                             
  SCRATCH DSNAME=SYS1.AOSC6,VOL=SYSDA=SMP000,PURGE                      
  UNCATLG DSNAME=SYS1.AOSD0                                             
  SCRATCH DSNAME=SYS1.AOSD0,VOL=SYSDA=SMP000,PURGE                      
  UNCATLG DSNAME=SYS1.AOSD7                                             
  SCRATCH DSNAME=SYS1.AOSD7,VOL=SYSDA=SMP000,PURGE                      
  UNCATLG DSNAME=SYS1.AOSD8                                             
  SCRATCH DSNAME=SYS1.AOSD8,VOL=SYSDA=SMP000,PURGE                      
  UNCATLG DSNAME=SYS1.AOSG0                                             
  SCRATCH DSNAME=SYS1.AOSG0,VOL=SYSDA=SMP000,PURGE                      
  UNCATLG DSNAME=SYS1.AOSH1                                             
  SCRATCH DSNAME=SYS1.AOSH1,VOL=SYSDA=SMP000,PURGE                      
  UNCATLG DSNAME=SYS1.AOSH3                                             
  SCRATCH DSNAME=SYS1.AOSH3,VOL=SYSDA=SMP000,PURGE                      
  UNCATLG DSNAME=SYS1.AOST3                                             
  SCRATCH DSNAME=SYS1.AOST3,VOL=SYSDA=SMP000,PURGE                      
  UNCATLG DSNAME=SYS1.AOST4                                             
  SCRATCH DSNAME=SYS1.AOST4,VOL=SYSDA=SMP000,PURGE                      
  UNCATLG DSNAME=SYS1.AOSU0                                             
  SCRATCH DSNAME=SYS1.AOSU0,VOL=SYSDA=SMP000,PURGE                      
  UNCATLG DSNAME=SYS1.AOS00                                             
  SCRATCH DSNAME=SYS1.AOS00,VOL=SYSDA=SMP000,PURGE                      
  UNCATLG DSNAME=SYS1.AOS03                                             
  SCRATCH DSNAME=SYS1.AOS03,VOL=SYSDA=SMP000,PURGE                      
  UNCATLG DSNAME=SYS1.AOS04                                             
  SCRATCH DSNAME=SYS1.AOS04,VOL=SYSDA=SMP000,PURGE                      
  UNCATLG DSNAME=SYS1.AOS05                                             
  SCRATCH DSNAME=SYS1.AOS05,VOL=SYSDA=SMP000,PURGE                      
  UNCATLG DSNAME=SYS1.AOS06                                             
  SCRATCH DSNAME=SYS1.AOS06,VOL=SYSDA=SMP000,PURGE                      
  UNCATLG DSNAME=SYS1.AOS07                                             
  SCRATCH DSNAME=SYS1.AOS07,VOL=SYSDA=SMP000,PURGE                      
  UNCATLG DSNAME=SYS1.AOS11                                             
  SCRATCH DSNAME=SYS1.AOS11,VOL=SYSDA=SMP000,PURGE                      
  UNCATLG DSNAME=SYS1.AOS12                                             
  SCRATCH DSNAME=SYS1.AOS12,VOL=SYSDA=SMP000,PURGE                      
  UNCATLG DSNAME=SYS1.AOS20                                             
  SCRATCH DSNAME=SYS1.AOS20,VOL=SYSDA=SMP000,PURGE                      
  UNCATLG DSNAME=SYS1.AOS21                                             
  SCRATCH DSNAME=SYS1.AOS21,VOL=SYSDA=SMP000,PURGE                      
  UNCATLG DSNAME=SYS1.AOS24                                             
  SCRATCH DSNAME=SYS1.AOS24,VOL=SYSDA=SMP000,PURGE                      
  UNCATLG DSNAME=SYS1.AOS26                                             
  SCRATCH DSNAME=SYS1.AOS26,VOL=SYSDA=SMP000,PURGE                      
  UNCATLG DSNAME=SYS1.AOS29                                             
  SCRATCH DSNAME=SYS1.AOS29,VOL=SYSDA=SMP000,PURGE                      
  UNCATLG DSNAME=SYS1.AOS32                                             
  SCRATCH DSNAME=SYS1.AOS32,VOL=SYSDA=SMP000,PURGE                      
  UNCATLG DSNAME=SYS1.APARMLIB                                          
  SCRATCH DSNAME=SYS1.APARMLIB,VOL=SYSDA=SMP000,PURGE                   
  UNCATLG DSNAME=SYS1.APROCLIB                                          
  SCRATCH DSNAME=SYS1.APROCLIB,VOL=SYSDA=SMP000,PURGE                   
  UNCATLG DSNAME=SYS1.ASAMPLIB                                          
  SCRATCH DSNAME=SYS1.ASAMPLIB,VOL=SYSDA=SMP000,PURGE                   
  UNCATLG DSNAME=SYS1.ATCAMMAC                                          
  SCRATCH DSNAME=SYS1.ATCAMMAC,VOL=SYSDA=SMP000,PURGE                   
  UNCATLG DSNAME=SYS1.ATSOMAC                                           
  SCRATCH DSNAME=SYS1.ATSOMAC,VOL=SYSDA=SMP000,PURGE                    
  UNCATLG DSNAME=SYS1.AUADS                                             
  SCRATCH DSNAME=SYS1.AUADS,VOL=SYSDA=SMP000,PURGE                      
  UNCATLG DSNAME=SYS1.HASPSRC                                           
  SCRATCH DSNAME=SYS1.HASPSRC,VOL=SYSDA=SMP000,PURGE                    
  UNCATLG DSNAME=SYS1.SMPACDS                                           
  SCRATCH DSNAME=SYS1.SMPACDS,VOL=SYSDA=SMP000,PURGE                    
  UNCATLG DSNAME=SYS1.SMPACRQ                                           
  SCRATCH DSNAME=SYS1.SMPACRQ,VOL=SYSDA=SMP000,PURGE                    
  UNCATLG DSNAME=SYS1.SMPMTS                                            
  SCRATCH DSNAME=SYS1.SMPMTS,VOL=SYSDA=SMP000,PURGE                     
  UNCATLG DSNAME=SYS1.SMPPTS                                            
  SCRATCH DSNAME=SYS1.SMPPTS,VOL=SYSDA=SMP000,PURGE                     
  UNCATLG DSNAME=SYS1.SMPSTS                                            
  SCRATCH DSNAME=SYS1.SMPSTS,VOL=SYSDA=SMP000,PURGE                     
  SCRATCH VTOC,VOL=SYSDA=WORK01,PURGE                                   
//*                                                                     
//********************************************************************* 
//* ALLOCATE DATASETS REQUIRED FOR BUILDING DISTRIBUTION LIBRARIES      
//********************************************************************* 
//*                                                                     
//IEFBR14  EXEC PGM=IEFBR14                                             
//ACMDLIB   DD UNIT=3350,DSN=SYS1.ACMDLIB,DISP=(,CATLG),                
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(600,4,40)),        
//             DCB=(BLKSIZE=6144,RECFM=U)                               
//AGENLIB   DD UNIT=3350,DSN=SYS1.AGENLIB,DISP=(,CATLG),                
//             VOL=(,RETAIN,SER=SMP000),SPACE=(TRK,(300,5,20)),         
//             DCB=(RECFM=FB,BLKSIZE=19040,LRECL=80)                    
//AHELP     DD UNIT=3350,DSN=SYS1.AHELP,DISP=(,CATLG),                  
//             VOL=(,RETAIN,SER=SMP000),SPACE=(TRK,(90,30,17)),         
//             DCB=(RECFM=FB,BLKSIZE=19040,LRECL=80)                    
//AIMAGE    DD UNIT=3350,DSN=SYS1.AIMAGE,DISP=(,CATLG),                 
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6400,(500,30,100)),      
//             DCB=(LRECL=80,RECFM=FB,BLKSIZE=6400)                     
//ALPALIB   DD UNIT=3350,DSN=SYS1.ALPALIB,DISP=(,CATLG),                
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(20,2,15)),         
//             DCB=(BLKSIZE=6144,RECFM=U)                               
//AMACLIB   DD UNIT=3350,DSN=SYS1.AMACLIB,DISP=(,CATLG),                
//             VOL=(,RETAIN,SER=SMP000),SPACE=(1680,(7400,100,50)),     
//             DCB=(RECFM=FB,BLKSIZE=19040,LRECL=80)                    
//AMODGEN   DD UNIT=3350,DSN=SYS1.AMODGEN,DISP=(,CATLG),                
//             VOL=(,RETAIN,SER=SMP000),SPACE=(1680,(4500,100,50)),     
//             DCB=(RECFM=FB,BLKSIZE=19040,LRECL=80)                    
//AOS00     DD UNIT=3350,DSN=SYS1.AOS00,DISP=(,CATLG),                  
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(15,1,15)),         
//             DCB=(BLKSIZE=6144,RECFM=U)                               
//AOS03     DD UNIT=3350,DSN=SYS1.AOS03,DISP=(,CATLG),                  
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(35,1,15)),         
//             DCB=(BLKSIZE=6144,RECFM=U)                               
//AOS04     DD UNIT=3350,DSN=SYS1.AOS04,DISP=(,CATLG),                  
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(30,1,15)),         
//             DCB=(BLKSIZE=6144,RECFM=U)                               
//AOS05     DD UNIT=3350,DSN=SYS1.AOS05,DISP=(,CATLG),                  
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(20,1,7)),          
//             DCB=(BLKSIZE=6144,RECFM=U)                               
//AOS06     DD UNIT=3350,DSN=SYS1.AOS06,DISP=(,CATLG),                  
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(75,1,30)),         
//             DCB=(BLKSIZE=6144,RECFM=U)                               
//AOS07     DD UNIT=3350,DSN=SYS1.AOS07,DISP=(,CATLG),                  
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(32,1,20)),         
//             DCB=(BLKSIZE=6144,RECFM=U)                               
//AOS11     DD UNIT=3350,DSN=SYS1.AOS11,DISP=(,CATLG),                  
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(80,5,15)),         
//             DCB=(BLKSIZE=6144,RECFM=U)                               
//AOS12     DD UNIT=3350,DSN=SYS1.AOS12,DISP=(,CATLG),                  
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(320,10,40)),       
//             DCB=(BLKSIZE=6144,RECFM=U)                               
//AOS20     DD UNIT=3350,DSN=SYS1.AOS20,DISP=(,CATLG),                  
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(50,2,25)),         
//             DCB=(BLKSIZE=6144,RECFM=U)                               
//AOS21     DD UNIT=3350,DSN=SYS1.AOS21,DISP=(,CATLG),                  
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(490,20,167)),      
//             DCB=(BLKSIZE=6144,RECFM=U)                               
//AOS24     DD UNIT=3350,DSN=SYS1.AOS24,DISP=(,CATLG),                  
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(240,25,50)),       
//             DCB=(BLKSIZE=6144,RECFM=U)                               
//AOS26     DD UNIT=3350,DSN=SYS1.AOS26,DISP=(,CATLG),                  
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(550,25,160)),      
//             DCB=(BLKSIZE=6144,RECFM=U)                               
//AOS29     DD UNIT=3350,DSN=SYS1.AOS29,DISP=(,CATLG),                  
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(114,19,27)),       
//             DCB=(BLKSIZE=6144,RECFM=U)                               
//AOS32     DD UNIT=3350,DSN=SYS1.AOS32,DISP=(,CATLG),                  
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(200,38,60)),       
//             DCB=(BLKSIZE=6144,RECFM=U)                               
//AOSA0     DD UNIT=3350,DSN=SYS1.AOSA0,DISP=(,CATLG),                  
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(210,10,70)),       
//             DCB=(BLKSIZE=6144,RECFM=U)                               
//AOSA1     DD UNIT=3350,DSN=SYS1.AOSA1,DISP=(,CATLG),                  
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(10,1,30)),         
//             DCB=(BLKSIZE=6144,RECFM=U)                               
//AOSB0     DD UNIT=3350,DSN=SYS1.AOSB0,DISP=(,CATLG),                  
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(9,1,5)),           
//             DCB=(BLKSIZE=6144,RECFM=U)                               
//AOSB3     DD UNIT=3350,DSN=SYS1.AOSB3,DISP=(,CATLG),                  
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(320,3,100)),       
//             DCB=(BLKSIZE=6144,RECFM=U)                               
//AOSBN     DD UNIT=3350,DSN=SYS1.AOSBN,DISP=(,CATLG),                  
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(130,10,25)),       
//             DCB=(RECFM=U,BLKSIZE=6144)                               
//AOSC2     DD UNIT=3350,DSN=SYS1.AOSC2,DISP=(,CATLG),                  
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(5,2,4)),           
//             DCB=(BLKSIZE=6144,RECFM=U)                               
//AOSC5     DD UNIT=3350,DSN=SYS1.AOSC5,DISP=(,CATLG),                  
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(500,20,150)),      
//             DCB=(BLKSIZE=6144,RECFM=U)                               
//AOSC6     DD UNIT=3350,DSN=SYS1.AOSC6,DISP=(,CATLG),                  
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(20,1,12)),         
//             DCB=(BLKSIZE=6144,RECFM=U)                               
//AOSCA     DD UNIT=3350,DSN=SYS1.AOSCA,DISP=(,CATLG),                  
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(11,1,6)),          
//             DCB=(BLKSIZE=6144,RECFM=U)                               
//AOSCD     DD UNIT=3350,DSN=SYS1.AOSCD,DISP=(,CATLG),                  
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(290,20,51)),       
//             DCB=(BLKSIZE=6144,RECFM=U)                               
//AOSCE     DD UNIT=3350,DSN=SYS1.AOSCE,DISP=(,CATLG),                  
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(55,5,15)),         
//             DCB=(BLKSIZE=6144,RECFM=U)                               
//AOSD0     DD UNIT=3350,DSN=SYS1.AOSD0,DISP=(,CATLG),                  
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(465,10,150)),      
//             DCB=(BLKSIZE=6144,RECFM=U)                               
//AOSD7     DD UNIT=3350,DSN=SYS1.AOSD7,DISP=(,CATLG),                  
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(20,1,15)),         
//             DCB=(BLKSIZE=6144,RECFM=U)                               
//AOSD8     DD UNIT=3350,DSN=SYS1.AOSD8,DISP=(,CATLG),                  
//             VOL=(,RETAIN,SER=SMP000),SPACE=(TRK,(30,15,25)),         
//             DCB=(BLKSIZE=6144,RECFM=U)                               
//AOSG0     DD UNIT=3350,DSN=SYS1.AOSG0,DISP=(,CATLG),                  
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(25,5,35)),         
//             DCB=(BLKSIZE=6144,RECFM=U)                               
//AOSH1     DD UNIT=3350,DSN=SYS1.AOSH1,DISP=(,CATLG),                  
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(5,1,3)),           
//             DCB=(BLKSIZE=6144,RECFM=U)                               
//AOSH3     DD UNIT=3350,DSN=SYS1.AOSH3,DISP=(,CATLG),                  
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(100,5,10)),        
//             DCB=(BLKSIZE=6144,RECFM=U)                               
//AOST3     DD UNIT=3350,DSN=SYS1.AOST3,DISP=(,CATLG),                  
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(75,5,30)),         
//             DCB=(BLKSIZE=6144,RECFM=U)                               
//AOST4     DD UNIT=3350,DSN=SYS1.AOST4,DISP=(,CATLG),                  
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(190,20,35)),       
//             DCB=(BLKSIZE=6144,RECFM=U)                               
//AOSU0     DD UNIT=3350,DSN=SYS1.AOSU0,DISP=(,CATLG),                  
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(710,20,150)),      
//             DCB=(BLKSIZE=6144,RECFM=U)                               
//APARMLIB  DD UNIT=3350,DSN=SYS1.APARMLIB,DISP=(,CATLG),               
//             VOL=(,RETAIN,SER=SMP000),SPACE=(80,(320,10,17)),         
//             DCB=(RECFM=F,BLKSIZE=80)                                 
//APROCLIB  DD UNIT=3350,DSN=SYS1.APROCLIB,DISP=(,CATLG),               
//             VOL=(,RETAIN,SER=SMP000),SPACE=(800,(85,10,17)),         
//             DCB=(RECFM=FB,BLKSIZE=800,LRECL=80)                      
//ASAMPLIB  DD UNIT=3350,DSN=SYS1.ASAMPLIB,DISP=(,CATLG),               
//             VOL=(,RETAIN,SER=SMP000),SPACE=(TRK,(120,5,20)),         
//             DCB=(RECFM=FB,BLKSIZE=19040,LRECL=80)                    
//ATCAMMAC  DD UNIT=3350,DSN=SYS1.ATCAMMAC,DISP=(,CATLG),               
//             VOL=(,RETAIN,SER=SMP000),SPACE=(TRK,(120,30,55)),        
//             DCB=(RECFM=FB,BLKSIZE=19040,LRECL=80)                    
//ATSOMAC   DD UNIT=3350,DSN=SYS1.ATSOMAC,DISP=(,CATLG),                
//             VOL=(,RETAIN,SER=SMP000),SPACE=(TRK,(180,30,17)),        
//             DCB=(RECFM=FB,BLKSIZE=19040,LRECL=80)                    
//AUADS     DD UNIT=3350,DSN=SYS1.AUADS,DISP=(,CATLG),                  
//             VOL=(,RETAIN,SER=SMP000),SPACE=(1680,(2,1,2)),           
//             DCB=(RECFM=FB,BLKSIZE=19040,LRECL=80)                    
//HASPSRC   DD UNIT=3350,DSN=SYS1.HASPSRC,DISP=(,CATLG),                
//             VOL=(,RETAIN,SER=SMP000),SPACE=(TRK,(1020,30,18)),       
//             DCB=(RECFM=FB,BLKSIZE=19040,LRECL=80)                    
//SMPACDS   DD UNIT=3350,DSN=SYS1.SMPACDS,DISP=(,CATLG),                
//             VOL=(,RETAIN,SER=SMP000),SPACE=(TRK,(900,30,4000)),      
//             DCB=(RECFM=FB,BLKSIZE=19040,LRECL=80)                    
//SMPACRQ   DD UNIT=3350,DSN=SYS1.SMPACRQ,DISP=(,CATLG),                
//             VOL=(,RETAIN,SER=SMP000),SPACE=(TRK,(150,30,84)),        
//             DCB=(RECFM=FB,BLKSIZE=19040,LRECL=80)                    
//SMPMTS    DD UNIT=3350,DSN=SYS1.SMPMTS,DISP=(,CATLG),                 
//             VOL=(,RETAIN,SER=SMP000),SPACE=(TRK,(210,30,75)),        
//             DCB=(RECFM=FB,BLKSIZE=19040,LRECL=80)                    
//SMPPTS    DD UNIT=3350,DSN=SYS1.SMPPTS,DISP=(,CATLG),                 
//             VOL=(,RETAIN,SER=SMP000),SPACE=(TRK,(3210,30,4000)),     
//             DCB=(RECFM=FB,BLKSIZE=19040,LRECL=80)                    
//SMPSTS    DD UNIT=3350,DSN=SYS1.SMPSTS,DISP=(,CATLG),                 
//             VOL=(,RETAIN,SER=SMP000),SPACE=(TRK,(480,30,75)),        
//             DCB=(RECFM=FB,BLKSIZE=19040,LRECL=80)                    
//*                                                                     
//* ***************************************************************** * 
//* INITIALIZE SMP ENVIRONMENT                                        * 
//* ***************************************************************** * 
//*                                                                     
//DLBUCL EXEC DLBSMP                                                    
//SMPCNTL  DD  *                                                        
 UCLIN PTS .                                                            
    DEL SYS .                                                           
    ADD SYS ASMNAME(IFOX00)                                             
            ASMPARM(XREF(SHORT),NOLOAD,DECK,LINECOUNT(56))              
            ASMPRINT(ASMPRINT)                                          
            ASMRC(4)                                                    
            COMPNAME(IEBCOPY)                                           
            COMPPARM(SIZE=2048K)                                        
            COMPPRINT(CMPPRINT)                                         
            COMPRC(0)                                                   
            COPYNAME(IEBCOPY)                                           
            COPYPRINT(COPPRINT)                                         
            COPYPARM(SIZE=2048K)                                        
            COPYRC(0)                                                   
            DSPREFIX(MVS.SCRATCH.TLIB)                                  
            DSSPACE(200,200,250)                                        
            LKEDNAME(IEWL)                                              
            LKEDPARM(SIZE=(500K,80K),NCAL,LIST,LET,XREF)                
            LKEDPRINT(LKDPRINT)                                         
            LKEDRC(8)                                                   
            PAGELEN(0061)                                               
            PEMAX(9999)                                                 
            RETRYNAME(IEBCOPY)                                          
            RETRYPARM(SIZE=2048K)                                       
            RETRYPRINT(E37PRINT)                                        
            RETRYRC(0)                                                  
            SREL(Z038)                                                  
            UPDATNAME(IEBUPDTE)                                         
            UPDATPRINT(UPDPRINT)                                        
            UPDATRC(0)                                                  
            ZAPNAME(AMASPZAP)                                           
            ZAPPARM(IGNIDRFULL)                                         
            ZAPPRINT(ZAPPRINT)                                          
            ZAPRC(4)                                                    
            .                                                           
 ENDUCL .                                                               
 RESETRC .                                                              
 LIST PTS SYS .                                                         
 UCLIN ACDS .                                                           
    DEL SYS .                                                           
    ADD SYS CDSID(MVS)                                                  
            NUCID(2)                                                    
            PEMAX(9999)                                                 
            RETRYDDN(ALL)                                               
            SREL(Z038)                                                  
            .                                                           
 ENDUCL .                                                               
 LIST ACDS SYS .                                                        
//*                                                                     
//SMPPTFIN DD  DUMMY                                                    
//                                                                      
