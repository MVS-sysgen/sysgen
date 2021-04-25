#!/usr/bin/env regina

/* Script:  check.queue.rexx
   Purpose: Scan MVS SYSOUT for success in restoring queue source elements
   Author:  Jay Moseley
   Date:    Tue September 1, 2020

   This REXX script scans a text file containing MVS SYSOUT content created during
   MVS 3.8j customization; specifically looking for messages generated as the QUEUE
   program source was loaded.  If all expected messages are found, it may be assumed
   that the QUEUE source was successfully loaded to the PDS.

   The first argument to the script specifies the name of the host Operating System 
   file containing the SYSOUT content to process.
*/

PARSE ARG printFile;    /* retrieve the argument */

/* verify that a print file name has been specified and exists: */

IF LENGTH(printFile)=0 THEN DO
  SAY 'Required argument must be Hercules printer file, for example: prt00e.txt';
  EXIT 1;   /* exit script */
END;
IF LENGTH(STREAM(printFile,'C','QUERY EXISTS'))=0 THEN DO
  SAY 'Hercules printer file specified as argument ('||printFile||') cannot be opened'
  EXIT 1;   /* exit script */
END;

/* initialize table of key strings to search for SYSOUT file */
keysTable.1.1='MEMBER $GPDOC ADDED (293 RECORDS)';
keysTable.2.1='MEMBER $JQT ADDED (23 RECORDS)';
keysTable.3.1='MEMBER $RENAME ADDED (45 RECORDS)';
keysTable.4.1='MEMBER $RNBDOC ADDED (170 RECORDS)';
keysTable.5.1='MEMBER $SP ADDED (58 RECORDS)';
keysTable.6.1='MEMBER ACTIVE ADDED (248 RECORDS)';
keysTable.7.1='MEMBER ALLOCATE ADDED (78 RECORDS)';
keysTable.8.1='MEMBER BLD3270 ADDED (181 RECORDS)';
keysTable.9.1='MEMBER C ADDED (78 RECORDS)';
keysTable.10.1='MEMBER CB3270 ADDED (190 RECORDS)';
keysTable.11.1='MEMBER CHCT ADDED (62 RECORDS)';
keysTable.12.1='MEMBER CJCT ADDED (66 RECORDS)';
keysTable.13.1='MEMBER CJOE ADDED (94 RECORDS)';
keysTable.14.1='MEMBER CJOESP ADDED (81 RECORDS)';
keysTable.15.1='MEMBER CJQE ADDED (27 RECORDS)';
keysTable.16.1='MEMBER CKPT ADDED (33 RECORDS)';
keysTable.17.1='MEMBER COMPILE ADDED (67 RECORDS)';
keysTable.18.1='MEMBER CPDDB ADDED (176 RECORDS)';
keysTable.19.1='MEMBER CSAVE ADDED (146 RECORDS)';
keysTable.20.1='MEMBER CSPIN ADDED (430 RECORDS)';
keysTable.21.1='MEMBER CTSO ADDED (211 RECORDS)';
keysTable.22.1='MEMBER DDNAME ADDED (293 RECORDS)';
keysTable.23.1='MEMBER DISPLAY ADDED (543 RECORDS)';
keysTable.24.1='MEMBER FILE53 ADDED (188 RECORDS)';
keysTable.25.1='MEMBER FINDJOB ADDED (224 RECORDS)';
keysTable.26.1='MEMBER FINDPDDB ADDED (101 RECORDS)';
keysTable.27.1='MEMBER FORMAT ADDED (515 RECORDS)';
keysTable.28.1='MEMBER GTTERM ADDED (144 RECORDS)';
keysTable.29.1='MEMBER HELP ADDED (218 RECORDS)';
keysTable.30.1='MEMBER HEXBLK ADDED (82 RECORDS)';
keysTable.31.1='MEMBER HEXDUMP ADDED (171 RECORDS)';
keysTable.32.1='MEMBER INIT ADDED (603 RECORDS)';
keysTable.33.1='MEMBER INITS ADDED (152 RECORDS)';
keysTable.34.1='MEMBER JCL ADDED (23 RECORDS)';
keysTable.35.1='MEMBER JLOG ADDED (90 RECORDS)';
keysTable.36.1='MEMBER JMSG ADDED (23 RECORDS)';
keysTable.37.1='MEMBER LIST ADDED (132 RECORDS)';
keysTable.38.1='MEMBER LISTDS ADDED (404 RECORDS)';
keysTable.39.1='MEMBER NEXT ADDED (56 RECORDS)';
keysTable.40.1='MEMBER PARSE ADDED (241 RECORDS)';
keysTable.41.1='MEMBER PRINT ADDED (538 RECORDS)';
keysTable.42.1='MEMBER QCOMMON ADDED (408 RECORDS)';
keysTable.43.1='MEMBER QHEAD ADDED (22 RECORDS)';
keysTable.44.1='MEMBER QPRBGEN ADDED (18 RECORDS)';
keysTable.45.1='MEMBER QSTART ADDED (222 RECORDS)';
keysTable.46.1='MEMBER QSTOP ADDED (9 RECORDS)';
keysTable.47.1='MEMBER QTILT ADDED (16 RECORDS)';
keysTable.48.1='MEMBER QUEUE ADDED (211 RECORDS)';
keysTable.49.1='MEMBER QUEUECMN ADDED (4 RECORDS)';
keysTable.50.1='MEMBER READSPC ADDED (93 RECORDS)';
keysTable.51.1='MEMBER RELINK ADDED (12 RECORDS)';
keysTable.52.1='MEMBER REPOS ADDED (354 RECORDS)';
keysTable.53.1='MEMBER SEARCH ADDED (651 RECORDS)';
keysTable.54.1='MEMBER SYSLOG ADDED (110 RECORDS)';
keysTable.55.1='MEMBER SYSOUT ADDED (414 RECORDS)';
keysTable.56.1='MEMBER TABLE ADDED (38 RECORDS)';
keysTable.57.1='MEMBER TSOHELP ADDED (12 RECORDS)';
keysTable.58.1='MEMBER XDS ADDED (97 RECORDS)';

/* process all lines in the text file: */

DO WHILE LINES(printFile)\=0    /* while there are lines remaining to be processed */

  inpLine=LINEIN(printFile);    /* read a line from the file */

  DO kX=1 BY 1 TO 58
    IF POS(keysTable.kX.1,inpLine)>0 THEN keysTable.kX.2='OK';
  END;

END;

/* check table for all key lines found */
DO kX=1 BY 1 to 58
  IF keysTable.kX.2 \= 'OK' THEN SAY WORD(keysTable.kX.1,2) 'was not loaded ******** ERROR ********';
  ELSE                           SAY WORD(keysTable.kX.1,2) 'successfully loaded';
END;

EXIT 0;
