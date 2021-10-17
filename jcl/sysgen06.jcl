//SYSGEN06 JOB (SYSGEN),'APPLY NEEDED PTFS',                            
//             CLASS=A,MSGLEVEL=(1,1),MSGCLASS=A                        
/*JOBPARM   LINES=100                                                   
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR                    
//*                                                                     
//* ***************************************************************** * 
//* APPLY ALL PTFS THAT ARE REQUIRED FOR USER MODS TO BE APPLIED NEXT * 
//* PLUS SEVERAL THAT WILL PRECLUDE THE NEED FOR A USERMOD (AZ58122)  * 
//* ***************************************************************** * 
//*                                                                     
//SMPAPP  EXEC SMPAPP                                                   
//SMPCNTL  DD  *                                                        
  APPLY SELECT(                                                         
               UZ57911 UZ60375 UZ63374 UZ65742            /* TJES801 */ 
               UZ68537 UZ71437 UZ76165 UZ77164 UY02859                  
               UZ45157 UZ45158                            /* TMVS816 */ 
               UZ56445                                    /* VS49603 */ 
               UZ79531                                    /* WM00017 */ 
               UY29953                                    /* ZP60002 */ 
               UZ68196                                    /* ZP60005 */ 
               UZ57385 UZ67122 UZ68882                    /* ZP60006 */ 
               UZ71054                                                  
               UY02947 UZ44177 UZ47575 UZ47871 UZ48384    /* ZP60012 */ 
               UZ82014 UZ82941 UZ83396                    /* ZP60012 */ 
               UY01186 UY13091 UY16532 UY17021 UZ48765    /* ZP60014 */ 
               UZ48744                                    /* ZP60016 */ 
               UZ67391                                    /* ZP60019 */ 
               UZ48373 UZ69717                            /* ZP60020 */ 
               UZ75398                                    /* ZP60027 */ 
               UZ44753                                    /* ZP60032 */ 
               UZ42622                                    /* ZP60035 */ 
               UY01301                                    /* ZP60038 */ 
               UZ62088                                    /* ZP60039 */ 
               UY13810                                    /* ZP60040 */ 
               UY07104 UY17588 UZ83530                    /* ZUM0008 */ 
               UZ54484 UZ56250 UZ56759          /* SUPERSEDES        */ 
               UZ57919 UZ61346 UZ61349          /* USERMOD           */ 
               UZ61367                          /* AZ58122           */
          )                                                             
          DIS(WRITE)                                                    
          COMPRESS(ALL)                                                 
  .                                                                     
//*                                                                     
//                                                                      
