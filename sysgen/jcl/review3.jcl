//REVIEW3  JOB (SYS),'INSTALL REVIEW',CLASS=S,MSGCLASS=X
//RECV370  PROC                                                      
//RECV370  EXEC PGM=RECV370,REGION=1024K                             
//STEPLIB   DD DSN=SYSC.LINKLIB,DISP=SHR                             
//RECVLOG   DD SYSOUT=*                     RECV370 OUTPUT MESSAGES  
//SYSPRINT  DD SYSOUT=*                     IEBCOPY OUTPUT MESSAGES  
//SYSIN     DD DUMMY                        IEBCOPY REQUIRES         
//SYSUT1    DD UNIT=SYSDA,                  WORK DATASET             
//             SPACE=(CYL,(10,10))                                   
//XMITIN    DD DDNAME=XMITIN                INPUT DATASET            
//SYSUT2    DD DDNAME=XMITOUT               OUTPUT DATASET           
//         PEND
//RECV01   EXEC RECV370
//XMITIN    DD UNIT=01C,DCB=(RECFM=FB,LRECL=80,BLKSIZE=80)
//SYSUT2    DD DSN=&&CLIST,DISP=(NEW,PASS),
//             UNIT=SYSDA,SPACE=(TRK,60)
//COPY01   EXEC PGM=IEBCOPY,REGION=1024K
//SYSPRINT  DD SYSOUT=*
//LIBIN     DD DSN=&&CLIST,DISP=OLD
//LIBOUT    DD DSN=SYS1.CMDPROC,DISP=SHR
//SYSUT3    DD UNIT=SYSDA,SPACE=(80,(60,45)),DISP=(,DELETE)
//SYSIN     DD *
  COPY INDD=((LIBIN,R)),OUTDD=LIBOUT
/*
//* ***************************************************************** * 
//* As installed, the REVINIT CLIST is lacking a VOLUME parameter     *
//* which, if present, will allocate the profile dataset on the same  *
//* DASD volume as the other user's datasets.  This step simply       * 
//* modifies the REVINIT member of SYS1.CMDPROC to add a VOLUME       * 
//* parameter when the profile dataset is initially created. This     * 
//* change does not take affect until the TSO user logs on the first  *
//* time after this job is executed.                                  *
//* ***************************************************************** * 
//*                                                                     
//TSO     EXEC PGM=IKJEFT01,REGION=1024K,DYNAMNBR=50                    
//SYSPRINT DD  SYSOUT=*                                                 
//SYSTSPRT DD  SYSOUT=*                                                 
//SYSTERM  DD  SYSOUT=*                                                 
//SYSTSIN  DD  DATA                                                     
EDIT 'SYS1.CMDPROC(REVINIT)' CNTL NONUM                                 
LIST                                                                    
TOP                                                                     
F =ALLOC F(REVPROF) DA('&PROFDSN') NEW USING=                           
C * =TR  =TR +=                                                         
INSERT       VOLUME(PUB000)                                             
LIST                                                                    
END SAVE                                                                
/*
//                                                 
