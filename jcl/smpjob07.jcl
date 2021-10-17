//SMPJOB07 JOB (SYSGEN),'GENERATE ICKDSF',                              
//             CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1)                        
//*                                                                     
//********************************************************************* 
//*                                                                     
//*         MVS 3.8 SYSGEN                                              
//*                                                                     
//* ICKDSF IS NOT PRESENT IN THE STARTER SYSTEM, BUT IS REQUIRED TO     
//* PLACE THE IPL RECORDS ONTO THE VOLUME WHICH WILL HOLD THE TARGET    
//* MVS 3.8 SYSTEM. THIS JOBSTREAM UTILIZES THE SYSGEN MACROS INSTALLED 
//* DURING THE BUILDING OF THE DISTRIBUTION LIBRARIES TO LINK EDIT      
//* ICKDSF FROM THE DISTRIBUTION LIBRARIES INTO SYS1.LINKLIB ON         
//* THE STARTER SYSTEM VOLUME - START1.                                 
//*                                                                     
//*   !!!  AN IPL IS REQUIRED AFTER THIS JOB COMPLETES  !!!             
//*                                                                     
//********************************************************************* 
//*                                                                     
/*MESSAGE  ************************************************************ 
/*MESSAGE  * AN IPL IS REQUIRED AFTER THIS JOB HAS COMPLETED!!!       * 
/*MESSAGE  ************************************************************ 
//*                                                                     
//********************************************************************* 
//* USE SGIEH404 TO GENERATE LINK EDIT STATEMENTS FOR ICKDSF            
//********************************************************************* 
//*                                                                     
//ASM      EXEC PGM=IEUASM,REGION=1024K,PARM='LIST,NOOBJECT,DECK'       
//SYSLIB   DD  DSN=SYS1.AGENLIB,DISP=SHR,UNIT=SYSDA,VOL=SER=SMP000      
//SYSUT1   DD  UNIT=SYSDA,SPACE=(1700,(1400,50))                        
//SYSUT2   DD  UNIT=SYSDA,SPACE=(1700,(1400,50))                        
//SYSUT3   DD  UNIT=SYSDA,SPACE=(1700,(1400,50))                        
//SYSPRINT DD  SYSOUT=A                                                 
//SYSPUNCH DD  UNIT=SYSDA,DISP=(,PASS),SPACE=(TRK,15),DCB=BLKSIZE=80    
//SYSIN    DD  *                                                        
         PRINT OFF                                                      
         SGIEH404                                                       
         END                                                            
//*                                                                     
//********************************************************************* 
//* BUILD JOBSTREAM TO LINK EDIT IFOX00 and ICKDSF                      
//********************************************************************* 
//*                                                                     
//IDCAMS   EXEC PGM=IDCAMS,REGION=4096K,COND=(0,NE,ASM)                 
//SYSPRINT DD  DUMMY                                                    
//DDIN1    DD  DATA,DLM='><'                                            
//SMPJOB07 JOB (SYSGEN),'LINK IFOX00/ICKDSF',
//             CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1)                        
//* ***************************************************************** * 
//* Re-link IFOX00 from the Distribution Libraries                    * 
//* ***************************************************************** * 
//LINK    EXEC LINKS,PARM=(NCAL,LIST,XREF),                             
//             CLASS=A,OBJ=,UNIT=,SER=,N=' ',NAME=,P1=,MOD=,P2=         
//SYSPUNCH DD  DUMMY                                                    
//SYSLMOD  DD  DSN=SYS1.LINKLIB,DISP=SHR                                
//AOS03    DD  DISP=SHR,VOL=(,RETAIN),DSN=SYS1.AOS03                    
//SYSLIN   DD  *                                                        
  INCLUDE  AOS03(IFOX0A,IFOX0B)                                         
  ENTRY  IFOX0A01                                                       
  ALIAS  ASMBLR,IEUASM                                                  
  NAME  IFOX00(R)                                                       
  INCLUDE  AOS03(IFOX0C)                                                
  ENTRY IFOX0C01                                                        
  NAME  IFOX01(R)                                                       
  INCLUDE  AOS03(IFOX0D,IFOX0J)                                         
  ENTRY IFOX0D01                                                        
  NAME  IFOX02(R)                                                       
  INCLUDE  AOS03(IFOX0E)                                                
  ENTRY  IFOX0E01                                                       
  NAME  IFOX03(R)                                                       
  INCLUDE  AOS03(IFOX0F)                                                
  ENTRY IFOX0F01                                                        
  NAME  IFOX04(R)                                                       
  INCLUDE  AOS03(IFOX0G)                                                
  ENTRY IFOX0G01                                                        
  NAME  IFOX05(R)                                                       
  INCLUDE  AOS03(IFOX0H)                                                
  ENTRY IFOX0H01                                                        
  NAME  IFOX06(R)                                                       
  INCLUDE  AOS03(IFOX0I)                                                
  ENTRY IFOX0I01                                                        
  NAME  IFOX07(R)                                                       
  INCLUDE  AOS03(IFNX1A,IFNX1J,IFNX1K,IFNX1S)                           
  ENTRY  IFNX1A01                                                       
  NAME  IFOX11(R)                                                       
  INCLUDE  AOS03(IFNX2A)                                                
  ENTRY IFNX2A01                                                        
  NAME  IFOX21(R)                                                       
  INCLUDE  AOS03(IFNX3A,IFNX3B,IFNX3K,IFNX3N)                           
  ENTRY  IFNX3A01                                                       
  NAME  IFOX31(R)                                                       
  INCLUDE  AOS03(IFNX4D,IFNX4E,IFNX4M,IFNX4S,IFNX4V)                    
  ENTRY  IFNX4M01                                                       
  NAME  IFOX41(R)                                                       
  INCLUDE  AOS03(IFNX4E,IFNX4N,IFNX4S,IFNX4T,IFNX4V)                    
  ENTRY  IFNX4T01                                                       
  NAME  IFOX42(R)                                                       
  INCLUDE  AOS03(IFNX5A,IFNX5C,IFNX5D,IFNX5F)                           
  INCLUDE  AOS03(IFNX5L,IFNX5M,IFNX5P,IFNX5V)                           
  ENTRY  IFNX5C01                                                       
  NAME  IFOX51(R)                                                       
  INCLUDE  AOS03(IFNX6A)                                                
  ENTRY IFNX6A01                                                        
  NAME  IFOX61(R)                                                       
  INCLUDE  AOS03(IFNX6B,IFNX6C)                                         
  ENTRY  IFNX6B01                                                       
  NAME  IFOX62(R)                                                       
//*                                                                     
//LINK    EXEC LINKS,PARM='NCAL,LIST,XREF,LET,RENT,REFR',               
//             CLASS=A,OBJ=,UNIT=,SER=,N=' ',NAME=,P1=,MOD=,P2=         
//SYSPUNCH DD  DUMMY                                                    
//SYSLMOD  DD  DSN=SYS1.LINKLIB,DISP=SHR                                
><                                                                      
//DDIN2    DD  DSN=*.ASM.SYSPUNCH,DISP=(OLD,DELETE)                     
//DDOUT    DD  UNIT=SYSDA,DISP=(MOD,PASS),                              
//             SPACE=(TRK,15),DCB=BLKSIZE=80                            
//SYSIN    DD  *                                                        
  REPRO INFILE(DDIN1) OUTFILE(DDOUT)                                    
  REPRO INFILE(DDIN2) OUTFILE(DDOUT) SKIP(4)                            
//*                                                                     
//********************************************************************* 
//* WRITE JOBSTEAM TO INTERNAL READER TO LINK EDIT IFOX00/ICKDSF        
//********************************************************************* 
//*                                                                     
//IEBGENER EXEC PGM=IEBGENER,COND=(0,NE,ASM)                            
//SYSIN    DD  DUMMY                                                    
//SYSPRINT DD  DUMMY                                                    
//SYSUT1   DD  DSN=*.IDCAMS.DDOUT,DISP=(OLD,DELETE)                     
//SYSUT2   DD  SYSOUT=(A,INTRDR)                                        
//*                                                                     
//********************************************************************* 
//* COPY SVC REQUIRED BY ICKDSF TO SVCLIB                               
//********************************************************************* 
//*                                                                     
//IEBCOPY EXEC PGM=IEBCOPY,REGION=1024K,COND=(0,NE,ASM)                 
//SYSPRINT DD  SYSOUT=A                                                 
//IN       DD  DSN=SYS1.AOSU0,DISP=SHR,UNIT=SYSDA,VOL=SER=SMP000        
//OUT      DD  DSN=SYS1.SVCLIB,DISP=OLD                                 
//SYSUT3   DD  UNIT=SYSDA,SPACE=(80,(60,45)),DISP=(NEW,DELETE)          
//SYSIN    DD  *                                                        
  COPY INDD=IN,OUTDD=OUT                                                
  SELECT MEMBER=(IGG019P2)                                              
//                                                                      
