//SMPJOB02 JOB (SYSGEN),'RECEIVE ALL PTFS',                             
//             CLASS=A,MSGLEVEL=(1,1),MSGCLASS=A                        
/*JOBPARM   LINES=100                                                   
//*                                                                     
//* ***************************************************************** * 
//* RECEIVE ALL PTFS WE HAVE FOUND TO APPLY TO MVS ELEMENTS           * 
//* ***************************************************************** * 
//*                                                                     
//DLBUCL EXEC DLBSMP                                                    
//SMPCNTL  DD  *                                                        
  RECEIVE                                                               
  .                                                                     
/*                                                                      
//SMPPTFIN DD  DSN=PTFS,                                                
//             UNIT=(TAPE,,DEFER),                                      
//             DISP=OLD,                                                
//             LABEL=(,SL),VOL=SER=PTFS20                               
//                                                                      
