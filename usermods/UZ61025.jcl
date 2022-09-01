//UZ61025  JOB (SYSGEN),'UZ61025',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//********************************************************************
//*                                                                  *
//* Name: CBT.MVS38J.CNTL(CMDSB1)                                    *
//*                                                                  *
//* Type: JCL to install a PTF                                       *
//*                                                                  *
//* Desc: This PTF updates the RESETPL macro.  Without this PTF      *
//*       the assembly of CSCSBMON will fail                         *
//*                                                                  *
//*                                                                  *
//********************************************************************
//ACCCHCK EXEC SMPAPP
//SMPCNTL  DD  *
 ACCEPT GROUP(UZ61025)
       DIS(WRITE)
       CHECK
 .
//ACCEPT EXEC SMPAPP,COND=(0,NE)
//SMPCNTL DD  *
 ACCEPT GROUP(UZ61025)
        DIS(WRITE)
 .