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
//* Extra software installed with MVP. UFSD, FTPD, HTTPD and MVSMF are
//* installed with SMP in step 12a (step_12a_mvslovers), not here.
//MVPINST EXEC MVP,INSTALL='OPNTERSE -D'        