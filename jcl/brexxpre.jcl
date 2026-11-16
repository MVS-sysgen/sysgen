//BRXXPRE  JOB (SYSGEN),'BREXX ALIAS',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1)
//*
//* Catalog the BREXX/370 data sets (BREXX.*) in the user catalog
//* UCPUB001, not in the master catalog (ug-install-pre). Runs before
//* the BREXX/370 SMP allocation job.
//*
//ALIAS    EXEC PGM=IDCAMS
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  *
  DEFINE ALIAS(NAME(BREXX) RELATE(UCPUB001))
/*
//
