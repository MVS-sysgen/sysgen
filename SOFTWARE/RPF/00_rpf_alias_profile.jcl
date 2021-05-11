//RPFPROF0 JOB (SYS),'INSTALL RPF',CLASS=S,MSGCLASS=X,
//         USER=IBMUSER,PASSWORD=SYS1
//*********************************************************************
//*
//* DESC: INSTALL RPF PROFILE CLUSTER.
//*
//*********************************************************************
//CLEANUP EXEC PGM=IDCAMS
//REPROIN  DD  *
99999999    SEED RECORD FOR THE RPF Profile cluster
//SYSPRINT DD  SYSOUT=*
//SYSIN  DD *
 PARM GRAPHICS(CHAIN(SN))
   DEFINE ALIAS(NAME(RPF) RELATE(UCPUB000))
 /***********************************************************/
 /*                                                         */
 /* Note:  you do not need this define alias if you do not  */
 /*        want to use a private user catalog               */
 /*                                                         */
 /* Note:  you need to update the relate parameter to       */
 /*        point to the catalog that you want to use        */
 /*                                                         */
 /***********************************************************/
   DELETE RPF.PROFILE CLUSTER
   SET LASTCC = 0
   SET MAXCC  = 0
 /***********************************************************/
 /*                                                         */
 /* Note:  You will have to modify the volume names         */
 /*        and the dataset high level qualifiers            */
 /*        to reflect your system environment               */
 /*                                                         */
 /***********************************************************/
  DEFINE CLUSTER ( NAME(RPF.PROFILE) -
                   VOL(PUB000) -
                   FREESPACE(20 10) -
                   RECORDSIZE(1750 1750) -
                   INDEXED -
                   IMBED -
                   UNIQUE  -
                   KEYS(8 0) -
                   CYLINDERS(1 1) -
                 ) -
            DATA ( NAME(RPF.PROFILE.DATA) -
                   SHR(3 3) -
                 ) -
           INDEX ( NAME(RPF.PROFILE.INDEX) -
                   SHR(3 3) -
                 )
  IF LASTCC = 0 THEN -
     REPRO INFILE(REPROIN) -
           OUTDATASET(RPF.PROFILE)
