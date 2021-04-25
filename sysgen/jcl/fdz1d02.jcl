//FDZ1D02  JOB (SYS),'INSTALL DSF R13',CLASS=A,MSGCLASS=A               
//*                                                                     
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
//* INSTALL ICKDSF (DEVICE SUPPORT FACILITIES) RELEASE 13.0 FROM        
//* MVS3.8J DISTRIBUTION TAPE RENAMING LOAD MODULE TO RETAIN ORIGINAL   
//* ICKDSF FROM BASE INSTALLATION.                                      
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
//*                                                                     
//IEBCOPY  EXEC PGM=IEBCOPY,REGION=1024K                                
//SYSPRINT DD  SYSOUT=*                                                 
//LIBIN1   DD  DSN=FDZ1D02.F1,UNIT=TAPE,DISP=(OLD,PASS),                
//             LABEL=(79,SL),VOL=(PRIVATE,RETAIN,SER=MVS38J)            
//LIBOUT1  DD  DSN=SYS1.FDZ1D02.F1,DISP=(,KEEP,DELETE),                 
//             UNIT=SYSDA,VOL=SER=WORK01,SPACE=(CYL,(1,1,5))            
//LIBIN2   DD  DSN=FDZ1D02.F2,UNIT=TAPE,DISP=OLD,                       
//             LABEL=(80,SL),VOL=(PRIVATE,RETAIN,SER=MVS38J)            
//LIBOUT2  DD  DSN=SYS1.FDZ1D02.F2,DISP=(,KEEP,DELETE),                 
//             UNIT=SYSDA,VOL=SER=WORK01,SPACE=(CYL,(1,1,20))           
//SYSUT3   DD  UNIT=SYSDA,SPACE=(80,(60,45)),DISP=(NEW,DELETE)          
//SYSIN    DD  *                                                        
  COPY INDD=LIBIN1,OUTDD=LIBOUT1                                        
  COPY INDD=LIBIN2,OUTDD=LIBOUT2                                        
//*                                                                     
//IDCAMS   EXEC PGM=IDCAMS,REGION=1024K,COND=(0,NE)                     
//SYSPRINT DD  SYSOUT=*                                                 
//JCLPART1 DD  DATA,DLM='><'                                            
//FDZ1D02  JOB (SYS),'INSTALL DSF R13',CLASS=A,MSGCLASS=A               
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR                    
//IEWL     EXEC PGM=IEWL,REGION=512K,PARM='XREF,LIST,LET,RENT,REFR'     
//SYSPRINT DD  SYSOUT=*                                                 
//SYSUT1   DD  SPACE=(CYL,(5,1)),UNIT=SYSDA                             
//SYSLMOD  DD  DSN=SYS2.LINKLIB,DISP=MOD                                
//AOSU0    DD  DSN=SYS1.FDZ1D02.F2,DISP=SHR,                            
//             UNIT=SYSDA,VOL=SER=WORK01                                
//SYSLIN   DD  *                                                        
><                                                                      
//JCLPART2 DD  DISP=SHR,DSN=SYS1.FDZ1D02.F1(FDZ1D02),                   
//             UNIT=SYSDA,VOL=SER=WORK01                                
//JCLPART3 DD  DATA,DLM='><'                                            
  NAME ICKDSF13(R)                                                      
//IEHPROGM EXEC PGM=IEHPROGM                                            
//SYSPRINT DD  SYSOUT=*                                                 
//DD1      DD  UNIT=SYSDA,VOL=SER=WORK01,DISP=OLD                       
//SYSIN    DD  *                                                        
  SCRATCH DSNAME=SYS1.FDZ1D02.F1,VOL=SYSDA=WORK01,PURGE                 
  SCRATCH DSNAME=SYS1.FDZ1D02.F2,VOL=SYSDA=WORK01,PURGE                 
><                                                                      
//SYSIN    DD  *                                                        
  REPRO INFILE(JCLPART1) OUTFILE(TEMPDD)                                
  REPRO INFILE(JCLPART2) OUTFILE(TEMPDD) SKIP(4) COUNT(93)              
  REPRO INFILE(JCLPART3) OUTFILE(TEMPDD)                                
//TEMPDD   DD  DSN=&&TEMPDD,UNIT=SYSDA,DISP=(MOD,PASS),SPACE=(CYL,2),   
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=80)                       
//*                                                                     
//IEBGENER EXEC PGM=IEBGENER,COND=(0,NE)                                
//SYSPRINT DD  SYSOUT=*                                                 
//SYSIN    DD  DUMMY                                                    
//SYSUT1   DD  DSN=&&TEMPDD,UNIT=SYSDA,DISP=(OLD,DELETE)                
//SYSUT2   DD  SYSOUT=(A,INTRDR)                                        
//                                                                      
