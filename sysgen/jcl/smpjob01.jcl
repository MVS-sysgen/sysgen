//SMPJOB01 JOB (SYSGEN),'RECEIVE PRODUCT TAPE',                         
//             CLASS=A,MSGLEVEL=(1,1),MSGCLASS=A                        
/*JOBPARM   LINES=100                                                   
//*                                                                     
//* ***************************************************************** * 
//* RECEIVE MVS COMPONENTS FROM DISTRIBUTION TAPE INTO SMP DATASETS   * 
//* ***************************************************************** * 
//*                                                                     
//DLBUCL   EXEC DLBSMP,TIME.SMP=1439                                    
//SMPCNTL   DD *                                                        
  RECEIVE                                                               
  .                                                                     
//SMPPTFIN  DD UNIT=TAPE,VOL=SER=MVS38J,DISP=(OLD,PASS),DSN=SMPMCS,     
//             LABEL=(1,SL)                                             
//                                                                      
