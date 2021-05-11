# Build starter system
source ./00_sysgen_functions.sh
trap 'check_return' 0
set -e

echo_step "Using SMP4 to Build the Distribution Libraries"
cd sysgen
echo_step "Removing previously built dasd"
rm -rf dasd
prev_dasd=$(ls -Art dasd.01.build_starter.*.tar | tail -n 1)
echo_step "Untarring $prev_dasd"
tar -xvf $prev_dasd
echo_step "Creating DASD"
bash create.dasd.sh dlibs
echo_step "Starting Hercules: hercules -f conf/smp1.cnf -r ../rc/02_sysgen_build_dist.rc"
hercules -f conf/smp1.cnf -r ../rc/02_sysgen_build_dist.rc > hercules.log
echo_step "Backing up hercules.log to hercules_log.dlib_1.$date_time.log"
mv hercules.log hercules_log.dlib_1.$date_time.log
echo_step "Backing up prt00e.txt to prt00e_dlib_backup_1.$date_time.txt"
cp prt00e.txt prt00e_dlib_backup_1.$date_time.txt
echo_step "Checking Job Return Codes"
tput bold
tput setaf 2
chmod +x ./condcode.rexx
./condcode.rexx prt00e.txt smp4p44
tput sgr0

#######
echo_step "Receive the MVS Product Elements"
echo_step "Starting Hercules: hercules -f conf/smp2.cnf -r ../rc/03_sysgen_build_dist_2.rc"
hercules -f conf/smp2.cnf -r ../rc/03_sysgen_build_dist_2.rc > hercules.log
echo_step "Backing up hercules.log to hercules_log.dlib_2.$date_time.log"
mv hercules.log hercules_log.dlib_2.$date_time.log
echo_step "Backing up prt00e.txt to prt00e_dlib_backup_2.$date_time.txt"
cp prt00e.txt prt00e_dlib_backup_2.$date_time.txt
echo_step "Checking Job Return Codes"
tput bold
tput setaf 2
./condcode.rexx prt00e.txt mount
#./condcode.rexx prt00e.txt smpjob00
./condcode.rexx prt00e.txt smpjob01
./condcode.rexx prt00e.txt smpjob02
./condcode.rexx prt00e.txt smpjob03
./condcode.rexx prt00e.txt smpjob04
./condcode.rexx prt00e.txt smpjob05
./condcode.rexx prt00e.txt smpjob06
./condcode.rexx prt00e.txt smpjob07
tput sgr0
echo_step "backing up DASD folder to dasd.dlib.$date_time.tar"
tar cvf dasd.02.dlib.$date_time.tar ./dasd
cd ..

trap : 0