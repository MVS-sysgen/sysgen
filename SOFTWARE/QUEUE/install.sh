# Installs QUEUE
# For use with Jay Moseley sysgen MVS only

cd $(dirname $0)

echo_step "Checking for new QUEUE version"
wget "www.prycroft6.com.au/vs2sw/download/queue38j.zip"
unzip -o queue38j.zip
mkdir -p zip
mv queue38j.zip zip
rm queue38j.zip*

cat <<EOF > 01_install_queue.jcl
//QUEUE1   JOB (TSO),
//             'Recieve XMI',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1)
//             USER=IBMUSER,PASSWORD=SYS1
//* First step is to make an alias for QUEUE
//ALIAS1   EXEC PGM=IDCAMS
//SYSIN    DD *
  DEFINE ALIAS(NAME(QUEUE) RELATE(UCPUB000))
//SYSPRINT DD  SYSOUT=*
EOF

# This looks like its doing a lot but its not
# 1) Cut off the jobcard
# 2) Remove the GREG. HLQ
# 3) Rename the JOB step to QUEUEINS
# 4) Add the PDSLOAD steplib
# 5) Add a trailing newline if one is missing
tail -n +2 queue38j.jcl |
sed "s/GREG\.//g" |
sed "s/GREGQ    /QUEUEINS /" |
sed "s/CLASS=A,MSGCLASS=X/CLASS=A,MSGCLASS=A/" |
sed "s/NOTIFY=GREG/NOTIFY=HMVS01/"|
sed "s/SYS1.PPLIB/SYS2.CMDLIB/"|
sed "s/\/\/STEP1   EXEC PGM=PDSLOAD/\/\/STEP1   EXEC PGM=PDSLOAD\n\/\/STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR/" |
sed -e '$a\' >> 01_install_queue.jcl

cat <<EOF >> 01_install_queue.jcl
@@
//SUBMIT1  EXEC PGM=IKJEFT01,REGION=2048K
//SYSPRINT DD  SYSOUT=*
//SYSTSPRT DD  SYSOUT=*
//SYSTERM  DD  SYSOUT=*
//SYSTSIN  DD  DATA
 SUBMIT 'QUEUE.ASM(C)'
/*
EOF





