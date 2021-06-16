TCP/IP for MVS 3.8 Assembler Installation
=========================================

TCP/IP for MVS 3.8 Assembler consists of a set of macros and a single
load module. By default, the *InstallEZASMI.jcl* will copy the macros
into SYS2.MACLIB and the EZASOH03 load module into SYS2.LINKLIB.
EZASOH03 must be linked into any of your programs containing the EZASMI
macro, or calls to the translation routines (EZACIC04 and EZACIC05).

Optional source material for the product is included on the install
tape. As its name implies, its installation is optional. The optional
source material includes the assembler source code for EZASOH03,
EZACIC04 and EZACIC05; also included are two sample programs utilizing
the EZASMI macros: NCAT and NSLOOKUP. The InstallEZASMI.jcl will copy
the source code into SYS2.ASM.

Note: If you are running Jürgen Winkelmann\'s excellent TK4- system, the
steps outlined in this section have already been performed.

Modifying the Installation JCL
------------------------------

Assuming that your MVS system is protected with RAKF, you will need to
modify the JOB card for the installation JCL to specify the userid, and
possibly password, depending upon how you submit the job. You will only
need to modify the IEBCOPY step if you need to specify different
locations for the EZASMI macro set, the EZASOH03 load module, or the
optional source material.

Submitting the Server Installation JCL
--------------------------------------

Before submitting your JCL for execution, you will need to make the
input tape file available to MVS so that the simulated tape volume can
be mounted when it is required by the installation process. The
following steps assume the use of device 480, defined as a tape drive in
the turnkey system:

1.  Enter the following command from the Hercules console:

                        devinit 480 d:\ezasmi\ezasmi.het {READONLY=1}
                        

    where *d:\\ezasmi\\* is the complete path to the Hercules Emulated
    Tape file. Of course your specification will use the path where you
    have stored the *ezasmi.het* simulated tape file. *READONLY=1* may
    optionally be specified if you are concerned that the simulated tape
    file might be overwritten.

2.  Issue the following command from your MVS console:

                        v 480,online
                        

3.  Submit the server installation JCL using whatever means you choose
    (e.g. TSO SUBMIT command).

Following the successful execution of the installation job, you should
be able to successfully assemble and link programs using the EZASMI
macro and related routines.
