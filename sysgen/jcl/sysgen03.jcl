//SYSGEN03 JOB (SYSGEN),'LOAD SYS1.APVTMACS',                           
//             CLASS=A,MSGLEVEL=(1,1),MSGCLASS=A                        
/*JOBPARM LINES=100                                                     
//*                                                                     
//********************************************************************* 
//* CREATE/LOAD SYS1.APVTMACS WHICH WAS CREATED FROM OPTIONAL MATERIALS 
//********************************************************************* 
//*                                                                     
//PVTMAC01 EXEC PGM=IEBUPDTE,REGION=1024K,PARM=NEW                      
//STEPCAT  DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR                    
//SYSUT2   DD  DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,VOL=SER=MVS000,       
//             DSN=SYS1.APVTMACS,                                       
//             SPACE=(TRK,(210,,20),RLSE),                              
//             DCB=(SYS1.MACLIB)                                        
//SYSPRINT DD  SYSOUT=*,FREE=CLOSE                                      
//SYSIN    DD  UNIT=TAPE,DISP=OLD,DSN=APVTMACS.OFFLOAD,                 
//             VOL=SER=PVTMAC,LABEL=(1,SL)                              
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -PVTMAC01 
//                                                                      
