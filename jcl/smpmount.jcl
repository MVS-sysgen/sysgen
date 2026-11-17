//SMPMOUNT JOB (SYSGEN),'ISSUE MOUNT COMMANDS',CLASS=A,MSGCLASS=A       
//********************************************************************* 
//*                                                                   * 
//* This job presents a MOUNT command to the operator for confirma-   * 
//* tion that sets the Storage Class to PRIVATE for the volume that   * 
//* is to receive Distribution Library Datasets.                      * 
//*                                                                   * 
//********************************************************************* 
// M 148,VOL=(SL,SMP000),USE=PRIVATE                                    
//S1       EXEC PGM=IEFBR14                                             
//                                                                      
