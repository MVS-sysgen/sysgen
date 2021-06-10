//HERC01B  JOB CLASS=A,MSGCLASS=X
//*
//* JOB010
//* CREATE THE NJE38 NETSPOOL DATASET
//*
//* VERIFY THE VOLUME NAME, THE DATASET NAME, AND THE NUMBER OF
//* CYLINDERS OF SPACE.
//*
//*
//SPOOL    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//*
//SPLVOL   DD UNIT=SYSDA,DISP=SHR,VOL=SER=PUB002        <== VERIFY
//*
//SYSIN    DD *
          DEF CL (  NAME( NJE38.NETSPOOL )           /* <== VERIFY */ -
                    RECSZ(4089,4089)                                  -
                    CYL(50)                          /* <== VERIFY */ -
                    NUMBERED                                          -
                    CISZ(4096)                                        -
                    SHR(4 4)                                          -
                    FILE( SPLVOL )                                    -
                    VOLUMES( PUB002))                /* <== VERIFY */ -
            DATA (  NAME( NJE38.NETSPOOL.DATA )      /* <== VERIFY */ -
                    UNIQUE )
/*
