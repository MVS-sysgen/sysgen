# Creates JCL for NJE38 SOURCE

cat << 'END'
//NJE38SRC JOB (TSO),
//             'Install NJE38 SOURCE',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//*  Installs SYSGEN.NJE38.ASMSRC
//*
//STEP1   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYSGEN.NJE38.ASMSRC,DISP=(NEW,CATLG),
//             VOL=SER=MVS000,
//             UNIT=3350,SPACE=(CYL,(2,1,10)),
//             DCB=(BLKSIZE=6160,LRECL=80,RECFM=FB)
//SYSUT1   DD  DATA,DLM=@@
END

for i in N38.ASMSRC/*.txt;do
    filename=$(basename -- "$i")
    echo "./ ADD NAME=${filename%.*}"
    cat $i
done
echo "@@"
echo "//"