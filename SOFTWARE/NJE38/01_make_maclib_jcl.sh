# Creates JCL for NJE38 MACLIB

cat << 'END'
//NJE38MAC  JOB (TSO),
//             'Install NJE38 MACLIB',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//*  Installs SYSGEN.NJE38.MACLIB
//*
//STEP1   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYSGEN.NJE38.MACLIB,DISP=(NEW,CATLG),
//             VOL=SER=MVS000,
//             UNIT=3350,SPACE=(CYL,(1,1,5)),
//             DCB=(BLKSIZE=3120,RECFM=FB,LRECL=80)
//SYSUT1   DD  DATA,DLM=@@
END

for i in N38.MACLIB/*;do
    filename=$(basename -- "$i")
    echo "./ ADD NAME=${filename%.*}"
    cat $i
done
echo "@@"
echo "//"