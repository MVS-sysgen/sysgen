#!/bin/bash
cd $(dirname $0)
cat << 'END'
//LISTPDSJ JOB (TSO),
//             'Install LISTPDS',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,PASSWORD=SYS1
//*
//* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//* DO NOT EDIT THIS JCL IT IS GENERATED FROM
//* 01_build_jobstream.sh edit that file instead
//* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//*
//* STEP 1: Compile/Link LISTCDS to SYS2.CMDLIB(LISTCDS)
//*
//ASSEM        EXEC ASMFCL,MAC='SYS1.MACLIB',MAC1='SYS1.AMODGEN',
//             MAC2='SYS1.HASPSRC',
//             PARM.LKED='(XREF,LET,LIST,CAL)'
//ASM.SYSIN    DD DATA,DLM='@@'
END

cat LISTCDS.txt

cat << 'END'
@@
//LKED.SYSLMOD DD DISP=SHR,DSN=SYS2.CMDLIB(LISTCDS)
//LKED.SYSLIB   DD  DSN=SYS2.LINKLIB,DISP=SHR
//*
//* STEP 2: Install LISTCDS Help
//*
//STEP1   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS2.HELP,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
END

for i in LISTCDS.hlp; do
    m=${i%.*}
    member=${m##*/}
    echo "./ ADD NAME=$member"
    cat "$i"
done
echo '@@'
echo "//*"


