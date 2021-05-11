This release of IND$FILE includes support for DFT/DDM style transfers using WSF 
data streams.

This IND$FILE software is free for you to use as you wish, however you do so
at your own risk.  If something goes wrong or does not work as you expected,
I accept no responsibility what so ever.  

You are strongly advised to use this IND$FILE on MVS38J with appropriate 
modifications to VTAM and TSO in support of WSF datastreams and increased VTAM 
buffer sizes, such as can be found on the "TK4-" package for MVS38J.

Be sure to make a backup of any existing IND$FILE in your SYS2.CMDLIB dataset. 
(I suggest doing a simple RENAME or COPY of any existing IND$FILE to IND$FIL1 
before submitting the IND$FILE.JCL to install the new release). You may also
want to save or make a copy of any existing CDPG037 or CDPG1047 members in
SYS2.CMDLIB before you install IND$FILE.  The replacement CDPGxxxx members in 
this install tape *should* be the same as those you already have.

To install the new IND$FILE, please edit the IND$FILE.JCL for the appropriate 
unit and submit the JCL to your MVS38J internal reader. Uncomment the USERID 
and PASSWORD and update the JCL with correct values if you need a USERID and 
PASSWORD on job submission.

A volume mount may occur on the MVS system console, reply "/R nn,M" without 
the quotes. 

On the hercules hardware console enter:
DEVINIT 480 path/to/IND$FILE.HET 
Change the unit and path names to suit your environment.

Tape contents are as follows:
SYS2.CMDLIB(IND$FILE,CDPGDFLT,CDPG1047,CDPG037)

What's New in IND$FILE (2.x.x):
1) IND$FILE supports DFT/DDM as well as CUT mode transfers.
   Note: DFT/DDM requires WSF datastream support in VTAM and TSO as well as
   larger VTAM buffers.  MVS38J TK4- or higher is recommended.

2) New transfer options for RECFM=V|VB binary transfers.
   Options:
   RDW     Include big-endian 4 byte RDW before each record 'LL00'.
   RDW2    Include big-endian 2 byte RDW before each record 'LL'.
   RDWPC   Include little-endian 4 byte RDW before each record 'LL00'.
   RDWPC2  Include little-endian 2 byte RDW before each record 'LL'.

   Example, download SYS1.MANX dataset with 4 byte big-endian RDW:
   IND$FILE GET 'SYS1.MANX' RDW

   Binary RECFM=V|VB transfers that omit any of the RDW options will NOT have 
   an RDW before each record (this is the default).
 
   The RDW options have no effect upon non-binary transfers or datasets
   that are RECFM=U or RECFM=F|FB.

Terminal Emulator Notes:
For those using the VISTA TN3270 emulator, please be sure to "check" the DFT 
check box on the file transfer "options button" dialog.  You can adjust the 
buffer size between 2K and 32K as needed.  If the DFT check box is not checked, 
then CUT mode file transfers will occur as was done in previous releases of 
IND$FILE.

Supported Terminal Emulators:
1) VISTA TN3270 1.27 http://www.tombrennansoftware.com/
2) TN3270 Plus 2.1.8 http://www.sdisw.com/

IND$FILE *may* work with other terminal emulators, but if a problem occurs
with something other than those listed above you're on your own as I have no 
means to recreate any issues that may occur.

General support questions should be directed to the H390-MVS group:
https://groups.yahoo.com/neo/groups/H390-MVS

If you prefer to contact me directly:
mailto:mikerayborn@comcast.net

73, KK4WGD
--Mike Rayborn
