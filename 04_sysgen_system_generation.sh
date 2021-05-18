# Build starter system
source ./00_sysgen_functions.sh
trap 'check_return' 0
set -e

cd sysgen
rm -rf dasd
prev_dasd=$(ls -Art dasd.02.dlib.*.tar | tail -n 1)
echo_step "Untarring $prev_dasd"
tar -xvf $prev_dasd
echo_step "Create additional DASD Volumes"
bash create.dasd.sh sysgen
chmod +x stage2.rexx
echo_step "Starting Hercules: hercules -f conf/sysgen.cnf -r ../04_sysgen_system_generation.rc"
hercules -f conf/sysgen.cnf -r ../04_sysgen_system_generation.rc -d > hercules.log
echo_step "Backing up hercules.log to hercules_log.sysgen.$date_time.log"
mv hercules.log hercules_log.sysgen.$date_time.log
echo_step "Backing up prt00e.txt to prt00e_sysgen.$date_time.txt"
cp prt00e.txt prt00e_sysgen.$date_time.txt
echo_step "Checking Job Return Codes"
if [[ ! -z "${TERM}" ]]; then
    tput bold
    tput setaf 2
fi
chmod +x ./condcode.rexx
# ./condcode.rexx prt00e.txt sysgen00
./condcode.rexx prt00e.txt mount
# ./condcode.rexx prt00e.txt sysgen01
./condcode.rexx prt00e.txt sysgen1
./condcode.rexx prt00e.txt sysgen2
./condcode.rexx prt00e.txt sysgen3
./condcode.rexx prt00e.txt sysgen4
./condcode.rexx prt00e.txt sysgen5
#./condcode.rexx prt00e.txt sysgen6
./condcode.rexx prt00e.txt sysgen02
./condcode.rexx prt00e.txt sysgen03
./condcode.rexx prt00e.txt sysgen04
./condcode.rexx prt00e.txt sysgen05
./condcode.rexx prt00e.txt sysgen06
./condcode.rexx prt00e.txt usermods1
./condcode.rexx prt00e.txt usermods2
./condcode.rexx prt00e.txt usermods3
./condcode.rexx prt00e.txt usermods4
./condcode.rexx prt00e.txt usermods5
./condcode.rexx prt00e.txt usermods6
./condcode.rexx prt00e.txt fdz1d02
if [[ ! -z "${TERM}" ]]; then
    tput sgr0
fi
echo_step "backing up DASD folder to dasd.sysgen.$date_time.tar"
tar cvf dasd.03.sysgen.$date_time.tar ./dasd
cd ..

trap : 0