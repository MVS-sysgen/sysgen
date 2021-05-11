//MVS01    JOB (1),'SETUP USER CATS',CLASS=S,MSGLEVEL=(1,1),
//             MSGCLASS=A
//*
//IDCAMS01 EXEC PGM=IDCAMS,REGION=4096K
//* ----------------------------------------------------------------- *
//* This job creates User Catalogs and defines ALIASes to them.       *
//*                                                                   *
//* PUB000 will contain TSO User's NON-VSAM datasets; they will be    *
//* catalogued in a User Catalog defined on PUB000 volume.            *
//* PUB001 will contain a VSAM dataspace to hold suballocated VSAM    *
//* objects; they and NON-VSAM datasets defined on PUB001 will be     *
//* catalogued in a User Catalog defined on PUB001 volume.            *
//* MVS000 will contain a User Catalog for small 'operations'         *
//* related datasets, such as the SMF generation datasets.            *
//* ----------------------------------------------------------------- *
//SYSPRINT DD  SYSOUT=*
//PUB000   DD  UNIT=SYSDA,DISP=OLD,VOL=SER=PUB000   (3380)
//PUB001   DD  UNIT=SYSDA,DISP=OLD,VOL=SER=PUB001   (3390)
//MVS000   DD  UNIT=SYSDA,DISP=OLD,VOL=SER=MVS000   (3350)
//* ----------------------------------------------------------------- *
//* The DD statement below creates a model DSCB on volume MVS000      *
//* for SMF generation datasets created when the SMF data collection  *
//* datasets fill.  No space is taken by the model DSCB, it only      *
//* creates an entry in the VTOC to be used as a pattern when the     *
//* actual datasets are created.                                      *
//* ----------------------------------------------------------------- *
//MODEL    DD  DSN=SYSO.SMF.DATA,DISP=(NEW,KEEP),
//             UNIT=3350,VOL=SER=MVS000,SPACE=(TRK,0),
//             DCB=(RECFM=VBS,LRECL=32756,BLKSIZE=32760)
//SYSIN    DD  *

  /*  PARM GRAPHICS(CHAIN(SN))         */
  /* This User Catalog will contain NON-VSAM datasets that     */
  /* reside on volume PUB000.                                  */

  DEFINE USERCATALOG ( -
         NAME (UCPUB000) -
         VOLUME (PUB000) -
         CYLINDERS (20) -
         FOR (9999) -
         BUFFERSPACE (8192) )

  /* An Alias is defined so that all datasets with a high-     */
  /* level qualifier of PUB000 will be catalogued in the       */
  /* User Catalog UCPUB000.                                    */

  DEFINE ALIAS ( -
         NAME (PUB000) -
         RELATE (UCPUB000) )

  /* This User Catalog will contain NON-VSAM and VSAM objects  */
  /* that reside on volume PUB001.  Half of the volume will be */
  /* allocated at the same time for use as a VSAM Dataspace.   */
  /* The User Catalog is sub-allocated from that Dataspace.    */

  DEFINE USERCATALOG ( -
         NAME (UCPUB001) -
         VOLUME (PUB001) -
         CYLINDERS (556) -
         FOR (9999) -
         BUFFERSPACE (8192) ) -
           DATA (CYLINDERS (30) ) -
           INDEX (CYLINDERS (15) )

  /* An Alias is defined so that all datasets and VSAM objects */
  /* with the high-level qualifier of PUB000 will be           */
  /* catalogued in the User Catalog UCPUB001.                  */

  DEFINE ALIAS ( -
         NAME (PUB001) -
         RELATE (UCPUB001) )

  /* An Alias is defined so that all datasets and VSAM objects */
  /* with the high-level qualifier of SYSGEN will be           */
  /* catalogued in the User Catalog UCPUB001.                  */

  DEFINE ALIAS ( -
         NAME (SYSGEN) -
         RELATE (UCPUB001) )

  /* This User Catalog will contain NON-VSAM objects that      */
  /* reside on volume MVS000.                                  */

  DEFINE USERCATALOG ( -
         NAME (UCMVS000) -
         VOLUME (MVS000) -
         TRACKS (030 0) -
         FOR (9999) )

  /* An Alias is defined so that all datasets with the high-   */
  /* level qualifier of SYSO (operations related) will be      */
  /* catalogued in the User Catalog UCMVS000.                  */

  DEFINE ALIAS ( -
         NAME (SYSO) -
         RELATE (UCMVS000) )

  /* A generation data group is defined to contain the SMF     */
  /* data that is offloaded when the SMF data recording        */
  /* datasets are filled.                                      */

  DEFINE GENERATIONDATAGROUP (                              -
               NAME(SYSO.SMF.DATA)                          -
               LIMIT(5)                                     -
               SCRATCH )

  /* The last major change has been made to the VSAM Master    */
  /* Catalog at this point, so we will add an Update password  */
  /* to make sure that any additions/deletions/changes to      */
  /* the Master Catalog require operator approval.             */

  ALTER SYS1.VSAM.MASTER.CATALOG -
        UPDATEPW(SYSPROG)
//
