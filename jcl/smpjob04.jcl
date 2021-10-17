//SMPJOB04 JOB (SYSGEN),'RECEIVE USERMODS',                             
//             CLASS=A,MSGLEVEL=(1,1),MSGCLASS=A                        
/*JOBPARM   LINES=100                                                   
//*                                                                     
//* ***************************************************************** * 
//* RECEIVE JIM MORRISON'S 3375+3380+3390 USERMODS                    * 
//* ***************************************************************** * 
//*                                                                     
//DLBUCL EXEC DLBSMP,TIME.SMP=1439                                      
//SMPCNTL  DD  *                                                        
  RECEIVE .                                                             
//SMPPTFIN  DD UNIT=TAPE,VOL=SER=J90009,DISP=(OLD,PASS),DSN=SMPMCS,     
//             LABEL=(1,SL)                                             
//                                                                      
