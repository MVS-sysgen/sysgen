# Tapes

These files were downloaded from http://www.jaymoseley.com/hercules/installMVS/iPREREQv7.htm

The following is modified from Jay Moseley:

### Starter System

The MVS starter system is simply an MVS 3.7 system that is contained on two 3330 DASD volumes.  The starter system is provided by IBM on two unlabeled tapes (`vs2start.het` and `vs2spool.het`), each of which contains three files:

1) a stand-alone program to initialize a DASD volume,
2) a stand-alone program to restore a tape dump to a DASD volume, and
3) a tape dump of the DASD volume.

* **vs2start.het** Volume image for the START1 3330 DASD volume
* **vs2spool.het** Volume image for the SPOOL0 3330 DASD volume

### Distribution Tapes

The MVS 3.8j distribution tapes provided by IBM is a set of six tapes. Of these six tapes, only three are needed/provided here:

1) **smp4b.het** tape image containing SMP4 load modules and procedures
* * Contains the **System Modification Program** (SMP) load modules and procedures. There is already a version of SMP on the starter system, but it is at a version level that is too early to correctly build the MVS 3.8j system, so you will need the version on this tape. You will also need the procedures from the tape to build the distribution libraries from which MVS 3.8j will be built.
* * The System Modification Program was created by IBM to control the distribution and updating of their MVS Operating System installed in their customer's mainframe systems.  Distributing a completely new copy of the entire Operating System code when changes (corrections) were made to a part of the code base would not work for even a small target customer base.  So SMP was created to allow the management of MVS Operating System code on mainframe computers.  During installation of a completely new system (which is what we are doing) SMP creates a substantial group of files into which it will collect and maintain everything known about the target MVS 3.8j system.  SMP will load the elements of the MVS Operating System into these files from the distribution tape.  Any modifications and patches we apply during this initial installation will also be placed into the files by SMP.  After we complete the System Generation, a copy of the jobstream that was used to generate the system will also be read by SMP and the information about the choices made for how the Operating System will function will also be placed into the files.  Then, whenever a change needs to be applied to the system SMP will be used to apply the change, ensuring that nothing is broken in the target Operating System.  That is a very brief overview of SMP and the functions it provides.  In 1988 Sam Golub wrote an article providing a very concise overview of SMP.  A copy of that article is available at `documentation/SMP_Tutorial.pdf`.  If you really want to learn more about the System Modification Program, you can read the 1980 version of the SMP Programmers Guide: `documentation/GC28-0673-6_SMP_SysPgmrGde_Sep80OCR.pdf`.
2) **zdlib1.het** tape image containing all product elements required to build the distribution libraries for MVS 3.8j
3) **dz1d02.het** tape image containing Device Support Facilities (release 13) components
* * Contains a more recent version of the Device Support Facilities than the one that will be installed with the other components from the second tape. It is nice to have both versions of these programs, as the later version is required for managing the DASD types that were introduced after MVS 3.8j.  As we will be installing user modifications to allow our target MVS 3.8j system to use those devices, we will need the services of the more updated utilities.

The base program directory for MVS 3.8j is an interesting read, and it is available at: `documentation/mvs38bas.pdf`.  This is the documentation that accompanies the product tapes distributed from IBM and contains detailed information about what is contained on the tapes.

The origin of these tapes was a CD-ROM ordered from the CBT web site: *CBT MVS Utilitys Tape CD-ROM 5 dated 2000-12-6.*

### Instalation Tapes

1) **apvtmacs.het** Macros required for some user modifications
1) **j90009.het** Jim Morrison's 3375/3380/3390 modifications
1) **ptfs.het** A large collection (1,482!)  of known PTFs (program Temporary Fixes) for MVS 3.8j
1) **rpf184.het**  	A collection of macros required for some Usermods
1) **stage1.het** A blank, initialized tape used to transfer stage1 output to host computer stage 2 jobstreams


