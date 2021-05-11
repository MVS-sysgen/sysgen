# REVIEW Front End (RFE) Install

This installs the newest version of REVIEW on to the Automated Sysgen MVS 3.8J.

There are two ways to install this software:

1) From within MVS/Hercules using the `INSTALL` command
2) Using the program `install_review.sh` :warning: if you use this method you MVS system must be shutdown first

- **Install**
  - `install.sh` Script/commands ran to update files before install
  - `install.rc` Hercules script file which runs install.sh
  - `install.txt` semi-colon seperated challenge response used by `automate.rexx`
  - `update_review.sh` updates RFE and generates new jcl files based on RPF version
  - `install_review.sh` IPLs MVS and install REVIEW
- **JCL**
  - `01_review_receive_clist.jcl` installs `revclist.xmi` to `REVIEW.CLIST`
  - `02_review_recieve_help.jcl` installs `revhelp.xmi` to `REVIEW.HELP`
  - `03_review_receive_load.jcl` installs `rev370ld.xmi` to `REVIEW.LOAD`
  - `04_review_receive_data.jcl` installs `revdata.xmi` to `REVIEW.DATA`
  - `05_review_install.jcl` installs REVIEW Front End
  - `06_SOC1_fix.jcl` There's a bug in REVIEW on this sysgen system this fixes it
- **XMI files**
  - `rev370ld.xmi` load libraries (i.e. executables)
  - `revasm.xmi` REVIEW source code, open with XMISSION xmi viewer or RECV370 on MVS
  - `revclist.xmi` REVIEW clists
  - `revhelp.xmi` REVIEW TSO help files
- **Directories**
  - `zip/` Contains zip files of most recent RFE
