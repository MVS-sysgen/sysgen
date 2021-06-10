//HERC01R JOB  CLASS=A,MSGCLASS=X,REGION=4096K
//C    EXEC GCCCL,
//           PARM.ASM=(DECK,LIST),
//           PARM.LKED=(XREF,LIST,CALL)
//COMP.SYSIN DD *
#include <stdio.h>

#define REGISTER   1
#define DEREGISTER 2
#define WAIT       3
#define GETMSG     4
#define GETECB     5

extern   int NJERLY(int*,int,char*);
int main(void)
{
   int njetkn;
   int njerc;
   int *njeecb;
   char useridÝ9¨;
   char msgÝ121¨;

   /* Register userid HERC01 */
   strcpy(userid,"HERC01  ");
   njerc=NJERLY(&njetkn,REGISTER,userid);
   printf("register rc=%d\n",njerc);
   if (njerc) return njerc;

   /* Get the ECB address to demonstrate, but not used here */
   njeecb=(int*)NJERLY(&njetkn,GETECB,"");
   printf("get ecb  addr=%x \n",njeecb);

   /* Begin message loop; else wait for message or STOP action */
   do
   {
      while (!NJERLY(&njetkn,GETMSG,msg))
      {
         printf("getmsg rc=%d\n",njerc);
         printf("msg txt  =%s\n",msg);
      }
      njerc=NJERLY(&njetkn,WAIT,"");
   } while (!njerc);

   printf("wait rc=%d\n",njerc);

   /* Wait returned non-zero RC, so deregister and exit */
   njerc=NJERLY(&njetkn,DEREGISTER,"");
   printf("deregister rc=%d\n",njerc);
   return njerc;
}
/*
//LKED.SYSIN DD *
  INCLUDE SYSLMOD(NJERLY)
  SETCODE AC(1)
  NAME CRLY(R)
/*
//LKED.SYSLMOD DD DSN=HERC01.AUTHLIB,DISP=SHR
//*
//TESTRUN EXEC PGM=CRLY
//STEPLIB DD DSN=HERC01.AUTHLIB,DISP=SHR
//SYSUDUMP DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSTERM  DD SYSOUT=*
//SYSIN    DD DUMMY
