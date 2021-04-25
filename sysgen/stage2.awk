#! /bin/awk -f
#
#------------------------------------------------------------------------------
# Script:  stage2.rexx
#  Purpose: Convert Stage 1 output into Stage 2 jcl
#  Author:  Jay Moseley
#  Date:    Mon April 27, 2020
#
#  This AWK script reads the output of SYSGEN01 which has
#  been saved in the host OS file stage1.output and makes
#  corrections/additions writing the output to the host
#  OS file stage2.jcl, ready for submission to MVS/JES
#
#  Summary of changes made to stage1.output statements:
#
# for each JOB JCL statement read:
#   if it is for job SYSGEN1 -
#       print the JOB statement to the output stream, appending a comma
#       insert a job continuation statement with TYPRUN=HOLD,TIME=1439
#       insert a JOBPARM statement with the LINES=100 parameter
#       insert a JOBCAT statement pointing to the target master catalog
#       continue processing the input stream with the next statement
#   if it is for job SYSGEN6 -
#     insert a job terminator statement ("//") in the output stream
#     print the JOB statement to the output stream
#     continue processing the input stream with the next statement
#   for all other than SYSGEN6 -
#       insert a job terminator statement in the output stream
#       print the JOB statement to the output stream, appending a comma
#       insert a job continuation statement with TYPRUN=HOLD,TIME=1439
#       insert a JOBPARM statement with the LINES=100 parameter
#       insert a JOBCAT statement pointing to the target master catalog
#       continue processing the input stream with the next statement
#
# Syntax: gawk -f stage2.awk stage1.output > stage2.jcl
#
#------------------------------------------------------------------------------
function rtrim(s) { sub(/[ \t\r\n]+$/, "", s); return s }
/ JOB / {
  switch ($1) {
    case "//SYSGEN1":
      print rtrim($0)",";
      print "//             TYPRUN=HOLD,TIME=1439";
      print "/*JOBPARM LINES=100";
      print "//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR";
      break
    case "//SYSGEN6":
      print "//";
      print $0;
      break
    default:
      print "//";
      print rtrim($0)",";
      print "//             TYPRUN=HOLD,TIME=1439";     
      print "/*JOBPARM LINES=100";
      print "//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR";
      break
  }
  next;
}
#
#------------------------------------------------------------------------------
# the TYPRUN=HOLD is only present in the generated deck for job SYSGEN6 -
#   print the continuation JOB card and append TIME=1439 to the end
#   insert a JOBPARM statement with the LINES=100 parameter
#   continue processing the input stream with the next statement
#------------------------------------------------------------------------------
/ TYPRUN=HOLD/ {
  print $0;
  print "/*JOBPARM LINES=100";
  next;
}
#
#------------------------------------------------------------------------------
# for all cards containing an EXPDT=99350 -
#   change the expiration date to prior date value ("72001" ought to do)
#   print the changed card to the output stream
#   continue processing the input stream with the next statement
#------------------------------------------------------------------------------
/EXPDT=99350/ {
  sub("99350","00000");
  print $0;
  next;
}
#
#------------------------------------------------------------------------------
# for the STEPY execute statement in job SYSGEN6 -
#   change the program to be executed from IDCAMS to IEFBR14
#   continue processing the input stream with the next statement
#------------------------------------------------------------------------------
/STEPY/ {
  print "//STEPY EXEC PGM=IEFBR14";
  next;
}
#
#------------------------------------------------------------------------------
# for all other cards in the input stream -
#   if the card is blank (there are 2 blank cards produced), omit it
#   if the card is the deck separator (there is one produced), omit it
#   otherwise, copy the input card to the output stream
#------------------------------------------------------------------------------
{
  if ($0=="")
    next;
  if (substr($0,1,10)=="||||||||||")
    next;
  print $0;
}
#
#------------------------------------------------------------------------------
# after all cards have been processed from the input stream -
#   insert a job terminator statement ("//") in the output stream
#------------------------------------------------------------------------------
END {
  print "//";
}
