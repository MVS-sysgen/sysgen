//SMPJOB06 JOB 'REJECT-CLEANUP',                                        
//             CLASS=A,MSGLEVEL=(1,1),MSGCLASS=A                        
/*JOBPARM   LINES=100                                                   
//*                                                                     
//* ***************************************************************** * 
//* REMOVE WORK DATASETS FROM WORK01 AND CLEAN UP AFTER SMP. EXPECT   * 
//* RC=0004 FROM THIS JOB.                                            * 
//* ***************************************************************** * 
//*                                                                     
//DLBUCL EXEC DLBSMP,TIME.SMP=1439                                      
//SMPCNTL  DD  *                                                        
  REJECT SELECT(EAS1102 EBB1102 EBT1102 EDE1102 EDM1102 EDS1102         
                EER1400 EGA1102 EGS1102 EIP1102 EJE1103 EMF1102         
                EMI1102 EML1102 EMS1102 EPM1102 EST1102 ESU1102         
                ESY1400 ETC0108 ETI1106 ETV0108 EUT1102 EVT0108         
                EXW1102 FBB1221 FDM1133 FDS1122 FDS1133 FDZ1610         
                FUT1133                                                 
                M023000                                                 
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
  .                                                                     
//*                                                                     
//SMPPTFIN DD  DUMMY                                                    
//*                                                                     
//                                                                      
