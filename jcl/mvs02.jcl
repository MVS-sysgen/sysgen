//MVS02    JOB (1),'ADD TSO USERS',CLASS=S,MSGLEVEL=(1,1),
//             MSGCLASS=A
//*
//* ----------------------------------------------------------------- *
//* This job adds a new TSO USER ID and allocates default datasets    *
//* for that user. You may change parameters for any USER ID to fit   *
//* your needs; however you should not change a parameter if you are  *
//* unsure of the results.
//* ----------------------------------------------------------------- *
// EXEC TSONUSER,ID=HMVS01,    This will be the logon ID 
//      PW='*',                No password will be required to logon
//      OP='OPER',             Allow operator authority
//      AC='ACCT'              Allow ACCOUNT TSO COMMAND
// EXEC TSONUSER,ID=HMVS02,    This will be the logon ID
//      PW='*',                No password will be required to logon 
//      OP='OPER',             Allow operator authority
//      AC='ACCT'              Allow ACCOUNT TSO COMMAND
//* ----------------------------------------------------------------- *
//* ID specified must be no longer than 7 characters
//*    first character must be alphabetic or national ($,@,#)
//*    remaining characters may be alphabetic, numeric, or national
//* PW asterisk (*) if no password is required
//*    if 'word' specified, may be no longer than 8 characters
//*    all characters must be alphabetic, numeric, or national ($,@,#)
//* AN asterisk (*) or accounting number to be used for user
//*    will be stored in SMF records for user
//*    maximum length 40 characters
//*    any character except blank, comma, tab, semicolon, apostrophe
//* PR name of logon procedure to be used (from SYS1.PROCLIB)
//* MS limit region size operand at logon (numeric value or NOLIM)
//* OP may be OPER or NOOPER
//*    if OPER specified, operator commands will be allowed from ID
//* AC may be ACCT or NOACCT
//*    if ACCT specified, ACCOUNT TSO COMMAND will be allowed from ID
//* JC may be JCL or NOJCL
//*    if JCL specified, SUBMIT, CANCEL, STATUS, and OUTPUT commands
//*       will be allowed from ID
//* MT may be MOUNT or NOMOUNT
//*    if MOUNT specified, ID may request tape mounts
//* SZ region size at logon
//* UN dynamic allocation unit type
//*
//* >>>>>> if in doubt about parameter, omit to accept default <<<<<<
//
