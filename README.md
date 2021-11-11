# MVS Community Edition Sysgen

:warning: This is a new version of sysgen without any software, if you are looking for the previous version checkout the `original` branch.

To use this version of MVS you can download the current release and run `bash start_mvs.sh`.

Requirements:

- Linux
- hercules SDL >= 4.0 (see below how to build SDL Hercules)
- python3
- git

The following is also recommended:

- c3270/x3270
- ncat

## Building and Installing Hercules

**Ubuntu**: Install m4, make, autoconf, automake, cmake, flex, build-essential, libbz2-dev, libregina3-dev, zlib1g-dev, libtool and, libltdl-dev with:
`sudo apt install m4 make autoconf automake cmake flex build-essential libbz2-dev zlib1g-dev libtool libltdl-dev`

The following script will then autobuild and install hercules for you.

```bash
# MVS3.8 Sysgen Automation - Build hercules
set -e
if [ -d "build" ]; then rm -rf build/; fi
mkdir build
cd build
mkdir ./hercpkgs
mkdir ./WORK

for i in crypto decNumber SoftFloat telnet; do
    git clone https://github.com/SDL-Hercules-390/$i.git
    mkdir ./WORK/$i
    cd ./WORK/$i
    ../../$i/build --pkgname . --all --install ../../hercpkgs
    cd ../../
done

echo "Building Hercules Hyperion SDL"
git clone https://github.com/SDL-Hercules-390/hyperion.git
cd hyperion
./configure --enable-cckd-bzip2 --enable-het-bzip2 --enable-regina-rexx --enable-extpkgs=$(realpath ../hercpkgs) --enable-optimization="-O3 -march=native"

echo "Compiling Hercules"
# thanks Mike Grossman for the CPU/o3
export NUMCPUS=`grep -c '^processor' /proc/cpuinfo`
make -j$NUMCPUS --load-average=$NUMCPUS

echo "Installing Hercules"
sudo make install
sudo ldconfig
cd ..
echo "Done!"
```

## Sysgen

Currently only Debian/Ubuntu based systems are supported. If your system requires a password for sudo commands you may get prompted for your password to install needed software.

:warning: **DO NOT** run this script as root, there is a bug in hercules which will cause it to eat up all your machine resources :warning:

*depending on your system this could take upward of two hours.* If you want to follow along you can use `tail -F sysgen.log`.

Running MVS/CE sysgen will:

- Compile the newest version of SDL Hercules and install it
- Build a modified Jay Moseley sysgen MVS 3.8J
- Install BREXX
- Install RAKF

To build MVS/CE use `sysgen.py`. This python script can take many arguments:

- `--version` This sets the version number displayed at logon and in `SYS1.PARMLIB(RELEASE)`
- `--users` By default sysgen will use the users in the file `users.conf`, you can supply your own with this argument
- `--profiles` By default sysgen will use the RAKF profiles in the file `profiles.conf`, you can supply your own with this argument
- `--username`/`--password` These arguments add an admin user with the username/password supplied
- `--nobrexx` Do not install brexx (this will also prevent RAKF from installing)
- `--norakf ` Do not install RAKF
- `--timeout` Sometimes hercules will end up in a state which can deadlock sysgen, to prevent it from running forever a timeout has been set. The defaul it thirty minutes. Use this argument to change it to something shorter/longer, in seconds.
- `--hercules` Path to a specific hercules binary
- `--no-compress` By default this script will compress DASD files, that is not needed on some file systems, this will disable compression
- `--keep-backup` This script backups after every step then removes the backups when completed, if you'd like to keep the backup DASD images use this flag
- `--keep-temp` This script generates multiple temp files during sysgen and removes the folder when completed, if you wish to keep the temp files pass this flag

### Automation control options

The arguments below are for more granular control of where to start sysgen from. These can be used if a step has failed of if you make changes to a step and start the install from that step instead of starting over. Some steps are atomic, some have multiple sub steps. With the arguments below you can continue/restart from either.

- `-l` or  `--list` This will list all available steps and substeps.
- `--step` Restart sysgen from this step. The install will continue from here.
- `--substep` Restart sysgen from a steps substep.

- `-C` or `--CONTINUE` If sysgen fails for any reason a file (`.step`) is created prior to exit, this argument reads that file and continues building MVS/CE from where it left off. This superscedes the `--step` and `--substep` arguments.


## Usernames/Passwords

**RAKF**/**TSO**

| Username  | Password |
|:---------:|:--------:|
| IBMUSER   | SYS1     |
| MVSCE01   | CUL8TR   |
| MVSCE02   | PASS4U   |

:warning: *IBMUSER* and *MVSCE01* are RAKF and TSO admins.

You can add a admin user using the `--username` flag. To add more users edit the `users.conf` file.

## Changes From Jay Moseley Sysgen

* Added usermod `SYZJ2001` which adds job cc to notification in TSO
* Added RAKF, and BREXX
* Installed usermod `DYNPROC` which allows for dynamic proclibs
* Seperated out usermods to their own JCL for better automation control
* Added `S NET` and changed JES2 startup parms in `sys1.parmlib(COMMND00)`
* Automated startup using HAO hercules
* Adds version to NETSOL
* Added `SYS1.PARMLIB(RELEASE)` which contains release information

And many more. See the branch `original` which tracked changes to the original sysgen

## Info

This repo is heavily based on Jay Moseley sysgen. His writeup is a wonderful resource and you should read the site here: http://www.jaymoseley.com/hercules/installMVS/iMVSintroV7.htm

A lot of the information contained on this repo is directly from his sysgen walkthrough.

There are lots of files and folders. Each folder has a readme explaining from a high level what each file does.

## System Setup Information:

From: http://www.jaymoseley.com/hercules/installMVS/iCUSTv7.htm

Much of the operation of MVS is controlled by JES2 and the parameters that affect JES2 are contained in the member JES2PM00 in SYS1.PARMLIB.  The three main functions are job entry (readers), job execution (job classes), and output (printers and punches).

**Card Readers**

There is one Hercules emulated card reader that is controlled by JES2 - the 2540R at address `x'00c'`. There are other card readers generated into the system and one of those is also defined in the Hercules configuration file - the 2540R at address `x'01c'`.

**Printers**

There are two emulated printers controlled by JES2 - the 1403 at address `x'00e'` and the 3211 at address `x'00f'`. The 1403 printer defined at address `x'015'` is a special case as it defined as, and dedicated to the hardcopy log; it is not controlled by JES2. There are other printers generated into the system, but they are not defined in the configuration file; they are simply 'extra' printers.

**Card Punch**

There is one emulated card punch that is controlled by JES2 - the 2540P at address `x'00d'`.  There are other card punches generated into the system and one of those is also definied in the Hercules configuration file - the 2540P at address `x'01d'`.

**CLASS**

The `CLASS=` parameter on the JOB card determines the class that the job is intended to be run in.

There are six initiators defined to JES2, three are not active when MVS is IPLed, but three of them are automatically started.  The initiators select a job for execution when the `CLASS=` parameter on the JOB card matches one of the CLASSES the initiator is set to process.  Currently the initiators are set to process these classes (listed in order by highest priority first):

| Initiator 1 | Initiator 2 | Initiator 3 | Initiator 4 (not started) | Initiator 5 (not started) | Initiator 6 (not started) |
|:-----------:|:-----------:|:-----------:|:-------------------------:|:-------------------------:|:-------------------------:|
| A           |     B,A     |      S      |         D,C,B,A           |  E,C,B,A                  |   F,E,C,B,A               |


Class **S** is intended for use for System Programming tasks, so some of the control has been loosened on that class, which is why you don't have to 'approve' embedded console commands.  It is not a good idea to simply use S for all of your jobs, however, as there are good reasons for those controls being in place.

The two printers controlled by JES2 are set to select non-held printer output in class **A** (the 1403 at x'00e') and class M (the 3211 at x'00f').

The card punch controlled by JES2 is set to select non-held punch output in class **B**.

Some of the parameters for JES2 may be changed from the MVS console and the changes will only remain in effect until the next IPL.  Some of the parameters must be changed by altering the JES2PM00 member in SYS1.PARMLIB, then stopping and restarting JES2.

