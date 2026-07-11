//*
//* JOBCARD REPLACED BY sysgen.py
//*
//*MVPISPF JOB (SYSGEN),'MVP INSTALL',      
//*             CLASS=A,                     
//*             MSGCLASS=A,                  
//*             MSGLEVEL=(1,1),
//*             USER=IBMUSER,
//*             PASSWORD=SYS1,
//*             REGION=8192K
//* This JCL installs ISPF and REVIEW Front End  
//MVPINST EXEC MVP,INSTALL='UFSD -D'    
//MVPINST EXEC MVP,INSTALL='FTPD -D'    
//MVPINST EXEC MVP,INSTALL='HTTPD -D'    
//MVPINST EXEC MVP,INSTALL='MVSMF -D'   
//MVPINST EXEC MVP,INSTALL='OPNTERSE -D'        