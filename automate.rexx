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
  SAY 'Missing challenge response file or hercules log file'
  SAY 'Usage: ./automate.rexx challenge_response_file.txt hercules_log.log [-debug]'
  EXIT 1;   /* exit script */
END;

/* creating a little lock file */
address 'hercules' 'sh touch failure'
failed = 1


say "Commands file: "||STREAM(commands,'C','QUERY EXISTS')
say "Log file: "||STREAM(logfile,'C','QUERY EXISTS')

/* IF LENGTH(debug)\=0 THEN DO
  debug = 1
END
ELSE DO
  debug = 0
END */

CALL pd 'Debug enabled'

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
    say t
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

      call pd 'Found challenge, replying with: ' ||response
      call slowdown 1 /* Without this it was too fast */

      if upper(response) = "QUIT" THEN do
        address 'hercules' 'sh rm -f failure'
        failed = 0
      end

      Address "HERCULES" response
      leave

      /* in some cases the text is read before we read the response */
      call slowdown 0.5
    end
  end
end


/* removing the lock file */
if failed then address 'hercules' 'sh rm -f failure'
EXIT 0

pd: /* Prints if debug is enabled */
  parse arg s
  if debug then say "[AUTOMATE DEBUG]" s
 return

slowdown: /* While regina has a sleep function it doesnt work in herc */
Parse Arg secs .
/* ADDRESS SYSTEM "sleep " || secs */
x = sleep(secs)
return


