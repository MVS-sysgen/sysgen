//MVS00    JOB (1),'INITIALIZE DASD',                                   
//             CLASS=A,MSGLEVEL=(1,1),MSGCLASS=A                        
//*                                                                     
//ICKDSF   EXEC PGM=ICKDSF13,REGION=4096K                               
//* ***************************************************************** * 
//* Initialize PUB000.3380 on 180, PUB001.3390 on 190, and            * 
//* SORTW1 - SORTW6 on 220 - 225                                      * 
//* ***************************************************************** * 
//SYSPRINT DD  SYSOUT=*                                                 
//SYSIN    DD  *                                                        
  INIT UNIT(180) -                                                      
               VERIFY(AAAAAA) -                                         
               OWNER(HERCULES) -                                        
               VOLID(PUB000) -                                          
               VTOC(0,1,30)                                             
  INIT UNIT(190) -                                                      
               VERIFY(BBBBBB) -                                         
               OWNER(HERCULES) -                                        
               VOLID(PUB001) -                                          
               VTOC(0,1,60)                                             
  INIT UNIT(220) -                                                      
               VERIFY(CCCCCC) -                                         
               OWNER(HERCULES) -                                        
               VOLID(SORTW1) -                                          
               VTOC(0,1,5)                                              
  INIT UNIT(221) -                                                      
               VERIFY(DDDDDD) -                                         
               OWNER(HERCULES) -                                        
               VOLID(SORTW2) -                                          
               VTOC(0,1,5)                                              
  INIT UNIT(222) -                                                      
               VERIFY(EEEEEE) -                                         
               OWNER(HERCULES) -                                        
               VOLID(SORTW3) -                                          
               VTOC(0,1,5)                                              
  INIT UNIT(223) -                                                      
               VERIFY(FFFFFF) -                                         
               OWNER(HERCULES) -                                        
               VOLID(SORTW4) -                                          
               VTOC(0,1,5)                                              
  INIT UNIT(224) -                                                      
               VERIFY(GGGGGG) -                                         
               OWNER(HERCULES) -                                        
               VOLID(SORTW5) -                                          
               VTOC(0,1,5)                                              
  INIT UNIT(225) -                                                      
               VERIFY(HHHHHH) -                                         
               OWNER(HERCULES) -                                        
               VOLID(SORTW6) -                                          
               VTOC(0,1,5)                                              
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//*                                                                     
//IEBGENER EXEC PGM=IEBGENER,COND=(0,NE)                                
//* ***************************************************************** * 
//* If ICKDSF RC=0000, submit MVS00 continuation job to internal      * 
//* reader.                                                           * 
//* ***************************************************************** * 
//SYSPRINT DD  DUMMY                                                    
//SYSIN    DD  DUMMY                                                    
//SYSUT1   DD  DATA,DLM='><'                                            
//MVS00    JOB (1),'PUT NEW DASD ONLINE',CLASS=S,MSGCLASS=A             
//********************************************************************* 
//* Issues Vary and Mount commands to place newly initialized DASD    * 
//* online. IMPORT SYSCPK User Catalog into Master Catalog.           * 
//********************************************************************* 
// V (180,190,220,221,222,223,224,225,253),ONLINE                       
// M 180,VOL=(SL,PUB000),USE=PRIVATE                                    
// M 190,VOL=(SL,PUB001),USE=PRIVATE                                    
// M 220,VOL=(SL,SORTW1),USE=PUBLIC                                     
// M 221,VOL=(SL,SORTW2),USE=PUBLIC                                     
// M 222,VOL=(SL,SORTW3),USE=PUBLIC                                     
// M 223,VOL=(SL,SORTW4),USE=PUBLIC                                     
// M 224,VOL=(SL,SORTW5),USE=PUBLIC                                     
// M 225,VOL=(SL,SORTW6),USE=PUBLIC                                     
// M 253,VOL=(SL,SYSCPK),USE=PRIVATE                                    
//IEFBR14  EXEC PGM=IEFBR14                                             
//IDCAMS01 EXEC PGM=IDCAMS,REGION=4096K                                 
//SYSPRINT DD SYSOUT=*                                                  
//SYSCPK DD UNIT=SYSDA,DISP=OLD,VOL=SER=SYSCPK                          
//SYSIN DD *                                                            
                                                                        
  /* THERE IS A USER CATALOG IN EXISTENCE ON SYSCPK THAT       */       
  /* CONTAINS CATALOG ENTRIES FOR THE DATASETS ON THAT VOLUME. */       
  /* IT IS CONNECTED TO THE MASTER CATALOG AND AN ALIAS TO THE */       
  /* HIGH ORDER INDEX IS DEFINED TO ALLOW ACCESS TO THE        */       
  /* DATASETS CATALOGUED IN THAT USER CATALOG.                 */       
                                                                        
  IMPORT CONNECT OBJECTS ( -                                            
         UCSYSCPK  -                                                    
         DEVICETYPE (3350) -                                            
         VOLUMES (SYSCPK) )                                             
                                                                        
  DEFINE ALIAS ( -                                                      
        NAME (SYSC) -                                                   
        RELATE (UCSYSCPK) )

       
  /* SYSC PACK INSTRUCTIONS DO NOT INCLUDE GCC OR THE CLIB   */ 
  /* THESE TWO DEFINES FIX THAT - SOF                        */ 

  DEFINE ALIAS ( -                                                      
        NAME (GCC) -                                                   
        RELATE (UCSYSCPK) )

                                                                        
  DEFINE ALIAS ( -                                                      
        NAME (PDPCLIB) -                                                   
        RELATE (UCSYSCPK) )
                                                                        
//*    END OF MVS00 CONTINUATION JOB                                    
><                                                                      
//SYSUT2   DD  SYSOUT=(A,INTRDR)                                        
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//BACKUP01 EXEC PGM=IEHPROGM,COND=(0,NE)                                
//* ***************************************************************** * 
//* SYS1.PROCLIB: BACKUP JES2 TO JES20098                             * 
//* ***************************************************************** * 
//SYSPRINT DD  SYSOUT=*                                                 
//MVSRES   DD  UNIT=3350,VOL=SER=MVSRES,DISP=OLD                        
//SYSIN    DD  *                                                        
  RENAME DSNAME=SYS1.PROCLIB,VOL=3350=MVSRES,                          C
               MEMBER=JES2,NEWNAME=JES20098                             
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//UPDATE01 EXEC PGM=IEBUPDTE,PARM=NEW,COND=(0,NE)                       
//* ***************************************************************** * 
//* SYS1.PROCLIB: CREATE JES2 (JES2 STARTUP PROC) TO ADD SYSC.PROCLIB * 
//* ***************************************************************** * 
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
//         DD  UNIT=&U,VOL=SER=SYSCPK,DISP=SHR,DSN=SYSC.PROCLIB         
//         DD  DSN=&N2..PROCLIB,DISP=SHR                                
//         DD  DSN=&N1..PROCLIB,DISP=SHR                                
//HASPPARM DD  DSN=&N1..&P(&M),DISP=SHR                                 
//HASPLIST DD  DDNAME=IEFRDER                                           
./ ENDUP                                                                
><                                                                      
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//*********************************************************************
//* Rename load module IFCRDE03 in SYS1.LINKLIB to eliminate the
//* message IFB010D at IPL (reason for IPL).
//*********************************************************************
//*                                                                    
//PROGM EXEC PGM=IEHPROGM 
//SYSPRINT DD SYSOUT=* 
//DD1 DD UNIT=3350,VOL=SER=MVSRES,DISP=OLD 
//SYSIN DD *
  RENAME DSNAME=SYS1.LINKLIB,VOL=3350=MVSRES,MEMBER=IFCRDE03,          C 
               NEWNAME=IFCRDEX3 
// 
