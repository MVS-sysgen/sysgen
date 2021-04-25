//SMPJOB03 JOB (SYSGEN),'ACCEPT FMIDS/PTFS',                            
//             CLASS=A,MSGLEVEL=(1,1),MSGCLASS=A                        
/*JOBPARM   LINES=100                                                   
//*                                                                     
//* ***************************************************************** * 
//* ACCEPT ALL PRODUCTS PLUS REQUIRED PTFS                            * 
//* ***************************************************************** * 
//*                                                                     
//DLBUCL1 EXEC DLBSMP                                                   
//SMPCNTL  DD  *                                                        
  ACCEPT S(EBT1102   /* BTAM                                         */ 
          )                                                             
         DIS(WRITE)                                                     
         NOAPPLY                                                        
  .                                                                     
//*                                                                     
//SMPPTFIN DD  DUMMY                                                    
//*                                                                     
//DLBUCL2 EXEC DLBSMP                                                   
//SMPCNTL  DD  *                                                        
  ACCEPT G(EAS1102   /* XF ASSEMBLER                                 */ 
           EBB1102   /* BASE CONTROL PROGRAM                         */ 
           EDE1102   /* DISPLAY EXCEPTION MONITORING FACILITY        */ 
           EER1400   /* ENVIRONMENT RECORDING EDITING & PRINTING     */ 
           EGA1102   /* GRAPHIC ACCESS METHOD                        */ 
           EGS1102   /* GRAPHIC SUBROUTINE PACKAGE                   */ 
           EIP1102   /* INTERACTIVE PROBLEM CONTROL SYSTEM           */ 
           EMF1102   /* MF/1                                         */ 
           EMI1102   /* MICR/OCR                                     */ 
           EDM1102   /* DATA MANAGEMENT                              */ 
           EDS1102   /* DATA MANAGEMENT SUPPORT                      */ 
             UZ68825 UZ71903                                            
           EPM1102   /* PROGRAM MANAGEMENT                           */ 
           EST1102   /* SYSTEM SUPPORT                               */ 
           ESU1102   /* SU BIT STRING                                */ 
           ESY1400   /* SYSTEM MODIFICATION PROGRAM 4                */ 
           ETV0108   /* TSO/VTAM                                     */ 
           EUT1102   /* UTILITIES                                    */ 
           EVT0108   /* VTAM                                         */ 
           EXW1102   /* EXTERNAL WRITER                              */ 
          )                                                             
        DIS(WRITE)                                                      
        NOAPPLY                                                         
  .                                                                     
//*                                                                     
//SMPPTFIN DD  DUMMY                                                    
//*                                                                     
//DLBUCL3 EXEC DLBSMP                                                   
//SMPCNTL  DD  *                                                        
  ACCEPT G(EML1102   /* MULTI-LEAVING WORK STATION                   */ 
           EMS1102   /* MASS STORAGE SYSTEM                          */ 
           FDZ1610   /* DEVICE SUPPORT FACILITY                      */ 
           ETI1106   /* TERMINAL I/O CONTROLLER                      */ 
           ETC0108   /* TCAM                                         */ 
          )                                                             
        DIS(WRITE)                                                      
        NOAPPLY                                                         
  .                                                                     
//*                                                                     
//SMPPTFIN DD  DUMMY                                                    
//*                                                                     
//DLBUCL4 EXEC DLBSMP                                                   
//SMPCNTL  DD  *                                                        
  ACCEPT G(FBB1221   /* MVS PROCESSOR SUPPORT 2                      */ 
             UY10657                                                    
             UZ64216                                                    
             UZ67485                                                    
             UZ69168                                                    
             UZ72152                                                    
             UZ72384                                                    
           FDS1122   /* MVS PROCESSOR SUPPORT 2                      */ 
          )                                                             
        DIS(WRITE)                                                      
        NOAPPLY                                                         
  .                                                                     
//*                                                                     
//SMPPTFIN DD  DUMMY                                                    
//*                                                                     
//DLBUCL5 EXEC DLBSMP                                                   
//SMPCNTL  DD  *                                                        
  ACCEPT G(EJE1103   /* JES2 WITH 3800 ENHANCEMENTS                  */ 
           FDM1133   /* 3800 ENHANCEMENTS - DATA MANAGEMENT          */ 
           FDS1133   /* 3800 ENHANCEMENTS - DATA MANAGEMENT SUPPORT  */ 
           FUT1133   /* 3800 ENHANCEMENTS - UTILITIES                */ 
          )                                                             
        DIS(WRITE)                                                      
        NOAPPLY                                                         
        COMPRESS(ALL)                                                   
  .                                                                     
//                                                                      
