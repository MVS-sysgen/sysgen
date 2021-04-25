#!/usr/bin/env regina

/* Script:  stage2.rexx
   Purpose: Convert Stage 1 output into Stage 2 jcl
   Author:  Jay Moseley
   Date:    Mon April 27, 2020

   This REXX script reads the output of SYSGEN01 which has
   been saved in the host OS file stage1.output and makes
   corrections/additions writing the output to the host
   OS file stage2.jcl, ready for submission to MVS/JES

   Summary of changes made to stage1.output statements:
     for each JOB statement -
       if job name is SYSGEN6
         insert job terminator '//' to delimit the previous job
         insert JOBPARM statement
       else
         if job name is not SYSGEN1
           insert job terminator '//' to delimit the previous job
         append comma to end of JOB statement
         insert continuation to JOB statement, adding TYPRUN and TIME
         insert JOBPARM and JOBCAT statements
     for all cards containing EXPDT=99350
       change to EXPDT=00000
     in JOB SYSGEN6, step STEPY
       change program from IDCAMS to IEFBR14
     omit blank cards (expect 2) and punch output separator cards (expect 1)
*/

inpFile='stage1.output'
outFile='jcl/stage2.jcl'
IF LENGTH(STRIP(STREAM(outFile,'C','QUERY EXISTS')))\=0 THEN
  'rm' outFile;

DO WHILE LINES(inpFile)

  inpLine=LINEIN(inpFile);
  
  IF LENGTH(STRIP(inpLine))=0 THEN ITERATE;
  IF SUBSTR(inpLine,1,10)=='||||||||||' THEN ITERATE;
  
  IF POS(' JOB ',inpLine)>0 THEN DO
    jobName=SUBSTR(WORD(inpLine,1),3);
    IF jobName\='SYSGEN1' THEN CALL LINEOUT outFile,'//';
    IF jobName\='SYSGEN6' THEN DO
      CALL LINEOUT outFile,STRIP(inpLine)||',';
      CALL LINEOUT outFile,'//             TYPRUN=HOLD,TIME=1439';
      CALL LINEOUT outFile,'/*JOBPARM LINES=100';
      CALL LINEOUT outFile,'//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR';
      END;
    ELSE
      CALL LINEOUT outFile,inpLine;
    ITERATE;
    END;
    
  IF POS(' EXEC ',inpLine)>0 THEN DO
    stepName=SUBSTR(WORD(inpLine,1),3);
    IF jobName=='SYSGEN6' THEN 
      SELECT
        WHEN stepName=='STEPY' THEN 
          CALL LINEOUT outFile,'//STEPY EXEC PGM=IEFBR14';
        WHEN stepName=='LIST1' THEN DO
          CALL LINEOUT outFile,'/*JOBPARM LINES=100';
          CALL LINEOUT outFile,inpLine;
          END;
        OTHERWISE 
          CALL LINEOUT outFile,inpLine;
      END;
    ELSE
      CALL LINEOUT outFile,inpLine;
    ITERATE;
    END;
    
  IF POS('EXPDT=99350',inpLine)>0 THEN
    inpLine=LEFT(inpLine,POS('EXPDT=',inpLine)-1)||'EXPDT=00000'||SUBSTR(inpLine,POS('EXPDT=',inpLine)+11);
      
  CALL LINEOUT outFile,inpLine;
  
END;

CALL LINEOUT outFile,'//';

EXIT 0;
