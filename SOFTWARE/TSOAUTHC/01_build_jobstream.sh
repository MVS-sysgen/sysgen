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
//* STEP 1: Copy TSOAUTHC to SYS2.EXEC
//*
//STEP1   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS2.EXEC,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
END

for i in TSOAUTHC.rexx; do
    m=${i%.*}
    member=${m##*/}
    echo "./ ADD NAME=$member"
    cat "$i"
done
echo '@@'
echo "//*"


