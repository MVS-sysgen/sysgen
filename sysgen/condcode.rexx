#!/usr/bin/env regina

/* Script:  condcode.rexx
   Purpose: Scan MVS SYSOUT for IEF142I messages
   Author:  Jay Moseley
   Date:    Sat May 2, 2020

   This REXX script scans a text file containing MVS SYSOUT content. Any line
   containing 'IEF142I', the message identifier for step condition code information,
   is processed.

   The first argument to the script specifies the name of the host Operating System 
   file containing the SYSOUT content to process.


   The second argument to the script specifies the name of the job to process. Two
   values trigger special circumstances:

       A value of 'ALL' will cause all IEF142 messages to be processed and printed
       regardless of the name of the job.

       A value of 'USERMODSn' will be treated as a meta job name and is relevant
       to the application of MVS User Modifications during System Generation. The
       single specified meta job name will be interpreted to match the names of
       all jobs expected for the meta job name specified.
*/

PARSE ARG printFile jobName;    /* retrieve the two arguments */

/* verify that a print file name has been specified and exists: */

IF LENGTH(printFile)=0 THEN DO
  SAY 'First argument must be Hercules printer file, for example: prt00e.txt';
  EXIT 1;   /* exit script */
END;
IF LENGTH(STREAM(printFile,'C','QUERY EXISTS'))=0 THEN DO
  SAY 'Hercules printer file specified as first argument ('||printFile||') cannot be opened'
  EXIT 1;   /* exit script */
END;

/* verify that a jobname to be searched for has been specified: */

IF LENGTH(jobName)=0 THEN DO
  SAY 'Second argument must be MVS job name to scan printer file for (or ALL)';
  EXIT 2;   /* exit script */
END;

/* initialize the array below to match USERMODn meta jobs */

metaName='USERMODS1';
USERMOD.metaName.0=9;   /* 9 entries follow */
userMOD.metaName.1='AY12275';
userMOD.metaName.2='JLM0001';
userMOD.metaName.3='JLM0002';
userMOD.metaName.4='JLM0003';
userMOD.metaName.5='JLM0004';
userMOD.metaName.6='SLB0002';
userMOD.metaName.7='SYZM001';
userMOD.metaName.8='TIST801';
userMOD.metaName.9='TJES801';

metaName='USERMODS2';
USERMOD.metaName.0=9;   /* 9 entries follow */
userMOD.metaName.1='TMVS804';
userMOD.metaName.2='TMVS816';
userMOD.metaName.3='TTSO801';
userMOD.metaName.4='VS49603';
userMOD.metaName.5='WM00017';
userMOD.metaName.6='ZP60001';
userMOD.metaName.7='ZP60002';
userMOD.metaName.8='ZP60003';
userMOD.metaName.9='ZP60004';

metaName='USERMODS3';
USERMOD.metaName.0=9;   /* 9 entries follow */
userMOD.metaName.1='ZP60005';
userMOD.metaName.2='ZP60006';
userMOD.metaName.3='ZP60007';
userMOD.metaName.4='ZP60008';
userMOD.metaName.5='ZP60009';
userMOD.metaName.6='ZP60011';
userMOD.metaName.7='ZP60012';
userMOD.metaName.8='ZP60013';
userMOD.metaName.9='ZP60014';

metaName='USERMODS4';
USERMOD.metaName.0=9;   /* 9 entries follow */
userMOD.metaName.1='ZP60015';
userMOD.metaName.2='ZP60016';
userMOD.metaName.3='ZP60017';
userMOD.metaName.4='ZP60018';
userMOD.metaName.5='ZP60019';
userMOD.metaName.6='ZP60020';
userMOD.metaName.7='ZP60021';
userMOD.metaName.8='ZP60022';
userMOD.metaName.9='ZP60026';

metaName='USERMODS5';
USERMOD.metaName.0=9;   /* 9 entries follow */
userMOD.metaName.1='ZP60027';
userMOD.metaName.2='ZP60028';
userMOD.metaName.3='ZP60029';
userMOD.metaName.4='ZP60030';
userMOD.metaName.5='ZP60031';
userMOD.metaName.6='ZP60032';
userMOD.metaName.7='ZP60033';
userMOD.metaName.8='ZP60034';
userMOD.metaName.9='ZP60035';

metaName='USERMODS6';
USERMOD.metaName.0=6;   /* 6 entries follow */
userMOD.metaName.1='ZP60036';
userMOD.metaName.2='ZP60037';
userMOD.metaName.3='ZP60038';
userMOD.metaName.4='ZP60039';
userMOD.metaName.5='ZUM0007';
userMOD.metaName.6='ZUM0008';

rcRecap. = 0;	/* initialize return code summary array */

/* print headings: */

SAY 'Searching' printFile 'for MVS Job Name' jobName;
SAY ' ';
SAY 'Job Name    Step Name    Proc Step Name    Completion Code';
SAY '--------    ---------    --------------    ---------------';

priorJob='NONE';    /* variable to track reported job name change */
ccKey='COND CODE '; /* text preceeding condition code in message line */

/* process all lines in the text file: */

failed=0
DO WHILE LINES(printFile)\=0    /* while there are lines remaining to be processed */

  inpLine=LINEIN(printFile);    /* read a line from the file */

  IF POS('IEF142I',inpLine)>0 THEN DO   /* does the line contain IEF142I? */

    thisJob=WORD(inpLine,2);                /* extract job name from message line */

    CALL jobMatch thisJob, jobName; /* subroutine will determine whether to print this line */

    IF printFLag=='y' THEN DO
      stepName=WORD(inpLine,3);             /* extract step name from message line */
      procName=WORD(inpLine,4);             /* extract proc name from message line */
      IF procName=='-' THEN procName=' ';   /* don't print - when proc name omitted */
      ccPos=POS(ccKey,inpLine)+LENGTH(ccKey); /* location of cc in message line */
      condCode=SUBSTR(inpLine,ccPos,4);     /* extract condition code from message line */

      IF condCode\='0000' THEN DO           /* make non-zero condition codes stand out */
        ccFlag=' <--';
	if condCode\='0004' THEN
	  failed=1
      end
      ELSE
        ccFlag='';

      rcRecap.steps = rcRecap.steps + 1;    /* count this step in total steps */
      rcRecap.condCode = rcRecap.condCode + 1;      /* count ending condcode for summary */

      IF priorJob\=thisJob THEN             /* print blank line between jobs */
        IF priorJob\='NONE' THEN
          SAY ' ';

      SAY LEFT(thisJob,11,' ') LEFT(stepName,12,' ') LEFT(procName,17,' ') condCode||ccFlag;

      priorJob=thisJob;

    END;

  END;

END;

/* At conclusion, print summary of jobs executed and condition codes received */

SAY ' ';
SAY RIGHT(rcRecap.steps,3,' ') 'steps executed in selected jobs';
DO ix=0 BY 1 TO 65535
  thisCC = RIGHT(D2X(ix),4,'0')
  IF rcRecap.thisCC > 0 THEN
    SAY RIGHT(rcRecap.thisCC,3,' ') 'steps received completion code =' thisCC
END;

EXIT failed; /* exit script */

/* The subroutine below will test job name from current message line against
   the job name submitted as an argument to the script. 

   Special 'meta' names of ALL and USERMODSn will be expanded to match
   all jobs for which messages are found or specific jobs that are expected
   in applying USERMODs during System Generation, respectively.
*/

jobMatch: PROCEDURE EXPOSE userMOD. printFlag; /* allow access to main routine's variables */

PARSE UPPER ARG thisJob, selectJob;     /* retrieve arguments to subroutine and make upper case */

printFlag='n';                          /* initialize printFlag to 'n' */

SELECT
  WHEN selectJob=='ALL'                
    THEN printFlag='y';

  WHEN LEFT(selectJob,8)=='USERMODS' THEN
    DO ix=1 BY 1 TO userMOD.selectJob.0
      IF thisJob==userMOD.selectJob.ix THEN DO
        printFlag='y';
        LEAVE;
      END;
    END;

  OTHERWISE
    IF thisJob==selectJob THEN printFlag='y';
END;

RETURN;

