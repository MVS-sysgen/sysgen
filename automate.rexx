/* REXX */
/*
   Sysgen Automation REXX script
   By: Philip Young (Soldier of FORTRAN)

   WARNING: This will only work when called from hercules

   To use:
   1) Start hercules with `hercules -f conf.file > hercules.log`
   2) Then call from hercules: exec automate.rexx file1.txt hercules.log

   License: GPL v2
*/
parse arg commands logfile .

/* **********************************
   Set to 1 to enable  debug messages
   Set to 0 to disable debug messages
   **********************************/
DEBUG=1
/* **********************************
   **********************************/

IF LENGTH(commands)=0 | LENGTH(logfile)=0 THEN DO
  SAY 'Hercules REXX Automation Script'
  SAY '*******************************'
  SAY 'This script is designed for use with MVS/CE build automation'
  SAY 'To use in hercules: "exec automate.rexx file1.txt hercules.log"'
  SAY '*******************************'
  SAY ''
  SAY 'Missing challenge response file or hercules log file'
  SAY 'Usage: ./automate.rexx challenge_response_file.txt hercules_log.log [-debug]'
  EXIT 1;   /* exit script */
END;

/* creating a little lock file */
address 'hercules' 'sh touch failure'
failed = 1


say "[AUTOMATE] Commands file: "||STREAM(commands,'C','QUERY EXISTS')
say "[AUTOMATE] : "||STREAM(logfile,'C','QUERY EXISTS')

/* IF LENGTH(debug)\=0 THEN DO
  debug = 1
END
ELSE DO
  debug = 0
END */

CALL pd 'Verbose messages enabled'

/* read all the lines first */
/* we do this in case we're called after sysgen is ipld */
do while lines(logfile) \= 0
      readline = linein(logfile)
end


do while lines(commands) \= 0
  /* Loop through the commands file */
  t = linein(commands)
  parse var t expect ';' response

  if left(t,1) = '#' then do /* supports comments */
    say "[AUTOMATE]" t
    iterate
  end

  if pos(';', t) = 0 then do
  /* Sometimes we need to run two commands, if we do we just place
  the seccond command after the first on a new line. Since it has
  no ; it will be executed */
    call pd 'Command on its own: '||t
    call slowdown 1
    Address "HERCULES" t
    iterate
  end

  /* Does our expected line start with a reply number?
     if so, remove it because we can't know what the actual number
     will be */
  if left(expect,2) = "/*" then do
    parse var expect . expect
  end

  /* Why do we cut off the last 5 bytes? Cause the script will trigger
     on our message if we print the expected message */
  call pd 'Waiting for: ' ||left(expect,length(expect) - 5)||"..."
  call pd 'Response: ' ||response

  do forever
    /* Read the hercules log file */
    if lines(logfile) \= 0 then do
      /* If there's new lines, read them */
      readline = linein(logfile)
    end

    if wordpos(expect, readline) \= 0 then do
      /*
          If we have a challenge, issue it then leave this while loop
          to get the next challenge response. If there are none we're
          done
      */

      /* remove the date/time column */
      parse var readline . readline

      if left(readline,2) = "/*" then do
        /* We don't know the reply number, get it and use it here */
        parse var readline reply .
        /* call pd "Reply Number: " reply */
        reply = right(reply,length(reply)-2)
        /* call pd "Reply Number: " reply */
        if left(response,2) = "/r" then do
            parse var response s split
            parse var split . ',' reply_string
            response = s||" "reply||","||reply_string
        end
      end

      if left(response,8) = '## CHECK' then do /* Check MAX CC */
        parse var response . . jobNameIn maxccIn .
        call pd "Checking" jobnamein "with maxcc" maxccIn
        maxcc_failed = 0
        maxcc_failed = checkCC(jobnamein maxccIn)
        if maxcc_failed then do
          call pd "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
          call pd "JOB:" jobnamein "failed with max cc greater than" maxccIn
          call pd "              Exiting hercules"
          call pd "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
          x = sleep(5)
          address hercules "quit force"
        end
        else do
          call pd "Job" jobnamein "completed succesfully"
        end

      end

      else do
        call pd 'Found challenge, replying with: ' ||response
      end

      call slowdown 1 /* Without this it was too fast */

      if upper(response) = "QUIT" THEN do
        address 'hercules' 'sh rm -f failure'
        failed = 0
      end

      Address "HERCULES" response
      leave
    end
    /* in some cases the text is read before we read the response */
    call slowdown 0.1
  end
end


/* removing the lock file */
if failed then address 'hercules' 'sh rm -f failure'
EXIT 0

pd: /* Prints if debug is enabled */
  parse arg s
  if debug then say "[AUTOMATE]" s
 return

slowdown: /* While regina has a sleep function it doesnt work in herc */
Parse Arg secs .
/* ADDRESS SYSTEM "sleep " || secs */
x = sleep(secs)
return

checkCC:
  parse arg jobName maxcc
  printFile = "prt00e.txt" /* Hercules printer file */

  IF LENGTH(STREAM(printFile,'C','QUERY EXISTS'))=0 THEN DO
    CALL pd 'Hercules printer file ('||printFile||') cannot be opened'
    EXIT 1;   /* exit script */
  END;

  /* verify that a jobname to be searched for has been specified: */

  IF LENGTH(jobName)=0 THEN DO
    CALL pd 'Missing MVS job (first argument) to scan printer file for (or ALL)';
    EXIT 2;   /* exit script */
  END;

  max = '0004'
  IF LENGTH(maxcc)\=0 THEN
    max = right(maxcc,4,'0')

/* initialize the arrays below to match USERMODn meta jobs */

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

  CALL pd 'Searching' printFile 'for MVS Job Name' jobName;
  CALL pd ' ';
  CALL pd 'Job Name    Step Name    Proc Step Name    Completion Code';
  CALL pd '--------    ---------    --------------    ---------------';

  priorJob='NONE';    /* variable to track reported job name change */
  ccKey='COND CODE '; /* text preceeding condition code in message line */

  /* process all lines in the text file: */

  failed=0
  count=0
  DO WHILE LINES(printFile)\=0    /* while there are lines remaining to be processed */

    inpLine=LINEIN(printFile);    /* read a line from the file */

    IF POS('IEF142I',inpLine)>0 THEN DO   /* does the line contain IEF142I? */

      thisJob=WORD(inpLine,2);                /* extract job name from message line */

      CALL jobMatch thisJob, jobName; /* subroutine will determine whether to print this line */

      IF printFLag=='y' THEN DO
        count = count + 1
        stepName=WORD(inpLine,3);             /* extract step name from message line */
        procName=WORD(inpLine,4);             /* extract proc name from message line */
        IF procName=='-' THEN procName=' ';   /* don't print - when proc name omitted */
        ccPos=POS(ccKey,inpLine)+LENGTH(ccKey); /* location of cc in message line */
        condCode=SUBSTR(inpLine,ccPos,4);     /* extract condition code from message line */

        IF condCode > max THEN DO           /* make non-zero condition codes stand out */
          ccFlag=' <--';
          failed=1
        end
        ELSE
          ccFlag='';

        rcRecap.steps = rcRecap.steps + 1;    /* count this step in total steps */
        rcRecap.condCode = rcRecap.condCode + 1;      /* count ending condcode for summary */

        IF priorJob\=thisJob THEN             /* print blank line between jobs */
          IF priorJob\='NONE' THEN
            CALL pd ' ';

        CALL pd LEFT(thisJob,11,' ') LEFT(stepName,12,' ') LEFT(procName,17,' ') condCode||ccFlag;

        priorJob=thisJob;

      END;

    END;

  END;

  CALL pd ' ';
  CALL pd RIGHT(rcRecap.steps,3,' ') 'steps executed in selected jobs';
  DO ix=0 BY 1 TO 65535
    thisCC = RIGHT(D2X(ix),4,'0')
    IF rcRecap.thisCC > 0 THEN
      CALL pd RIGHT(rcRecap.thisCC,3,' ') 'steps received completion code =' thisCC
  END;

  IF COUNT = 0 THEN failed = 1

  return failed;

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
