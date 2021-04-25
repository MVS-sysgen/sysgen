//SMPJOB05 JOB (SYSGEN),'ACCEPT USERMODS',                              
//             CLASS=A,MSGLEVEL=(1,1),MSGCLASS=A                        
/*JOBPARM   LINES=100                                                   
//*                                                                     
//* ***************************************************************** * 
//* ACCEPT JIM MORRISON'S 3375+3380+3390 USERMODS                     * 
//* ***************************************************************** * 
//*                                                                     
//DLBUCL EXEC DLBSMP,TIME.SMP=1439                                      
//SMPCNTL  DD  *                                                        
                                                                        
 /* Use the following set of SMP4 control statements         */         
 /* if you have NOT previously installed 3380 + 3390 support */         
                                                                        
  ACCEPT S(M023000                                                      
           M023100                                                      
           M023200 M023201 M023202 M023203 M023204                      
           M023300 M023301 M023302                                      
           M023400 M023401 M023402 M023403 M023404 M023405              
           M024001                                                      
           M024101                                                      
           M024205 M024206 M024207                                      
           M024303 M024304 M024305                                      
           M024406 M024407 M024408                                      
          )                                                             
         USERMODS                                                       
         NOAPPLY                                                        
         DIS(WRITE)                                                     
         COMPRESS(ALL)                                                  
  .                                                                     
//SMPPTFIN DD  DUMMY                                                    
//                                                                      
