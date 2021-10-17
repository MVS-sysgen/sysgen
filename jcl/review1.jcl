//REVIEW1  JOB (SYS),'INSTALL REVIEW',CLASS=S,MSGCLASS=X
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
//SYSUT2    DD DSN=&&LOAD,DISP=(NEW,PASS),
//             UNIT=SYSDA,SPACE=(TRK,60)
//COPY01   EXEC PGM=IEBCOPY,REGION=1024K
//SYSPRINT  DD SYSOUT=*
//LIBIN     DD DSN=&&LOAD,DISP=OLD
//LIBOUT    DD DSN=SYS2.CMDLIB,DISP=SHR
//SYSUT3    DD UNIT=SYSDA,SPACE=(80,(60,45)),DISP=(,DELETE)
//SYSIN     DD *
  COPY INDD=((LIBIN,R)),OUTDD=LIBOUT
//