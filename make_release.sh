echo "[+] Creating SYSGEN releases"

rm -rf release
mkdir release
cd release

folders="sysgen sysgen_rakf sysgen_all"

echo "[+] Creating folders"
for j in $folders; do
    mkdir -p ./$j/sysgen/conf
done

echo "[+] Copying files"
for i in automate.rexx start_mvs.sh automate.md; do
    for j in $folders; do
        cp ../$i ./$j
    done
done

for j in $folders; do
    cp ../sysgen/conf/local.cnf ./$j/sysgen/conf
    mkdir ./$j/sysgen/jcl
    echo "//" > ./$j/sysgen/jcl/null.jcl
    for i in autostart.rc autostart.txt submit.sh; do
        cp ../sysgen/$i ./$j/sysgen/
    done
done

sysgen_dasd=$(ls -Art ../sysgen/dasd.04.customization.*.tar | tail -n 1)
sysgen_rakf_dasd=$(ls -Art ../sysgen/dasd.05.rakf.*.tar | tail -n 1)
sysgen_software=$(ls -Art ../sysgen/dasd.06.software.*.tar | tail -n 1)


echo "[+] Copying sysgen_all dasd"
cd ./sysgen_all/sysgen
tar -xvf ../../$sysgen_software
cd ../..


echo "[+] Extracting sysgen $sysgen_dasd"
cd sysgen/sysgen
tar -xvf ../../$sysgen_dasd
cd ../..

echo "[+] Extracting sysgen $sysgen_rakf_dasd"
cd sysgen_rakf/sysgen
tar -xvf ../../$sysgen_rakf_dasd
cd ../..

echo "[+] Copying SOFTWARE and generating README.md"
for i in $folders; do

cp -r ../SOFTWARE $i/

    cat << 'EOF' > ./$i/README.md
# Automated MVS3.8j Sysgen

Welcome to the automated MVS 3.8j build release. This was built using https://github.com/MVS-sysgen/.

To start the system run `./start_mvs.sh`

## Usernames/Passwords

| Username | Password |
|:--------:|:--------:|
| IBMUSER  | SYS1     |
| HMVS01   | CUL8TR   |
| HMVS02   | PASS4U   |

## Available Software

Included with this repo (\* means its already installed):

- BREXX*
- EXTRAS
- INDFILE*
- MDDIAG8*
- OFFLOAD
- QUEUE
- REVIEW*
- RPF

In External Repos:

- [RAKF](https://github.com/MVS-sysgen/RAKF)*
- [FTPD](https://github.com/MVS-sysgen/FTPD)*


To install any of the non-installed software login and run `INSTALL THING` where *THING* is any of the folders in `SOFTWARE`.

In the [SOFTWARE](SOFTWARE) folder is software that comes with this SYSGEN. Other software, such as FTPD and RAKF exist in other repos but can be installed by cloning the repo to the `SOFTWARE` folder and following their instructions.

## Info

This release is heavily based on Jay Moseley sysgen. His writeup is a wonderful resource and you should read the site here: http://www.jaymoseley.com/hercules/installMVS/iMVSintroV7.htm

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
EOF

done


for i in $folders; do
    echo "[+] Compressing $i"
    zip -q -r $i.zip $i
    rm -rf $i
done


echo "[+] Writting RELEASE.md"
cat << 'EOF' > RELEASE.md
This release is made up of three separate options:

- `sysgen.zip` - This is the barebones Jay Moseley sysgen without anything installed other than the SYSC pack.
- `sysgen_rakf.zip` - This is the sysgen system above with RAKF installed
- `sysgen_all.zip` - This is the RAKF system above but with BREXX, REVIEW, MDDIAG8, IND$FILE, and FTPD installed.

Each release has a README with instructions and credentials.

To run ensure you have the newest version of hercules built and installed from https://github.com/SDL-Hercules-390/ (this may work with other versions of hercules but has not been tested) then run `./start_mvs.sh` to IPL the system.

To generate your own sysgen follow the instructions at https://github.com/MVS-sysgen/sysgen
EOF

echo "[+] Done"