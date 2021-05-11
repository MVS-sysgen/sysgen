# Build starter system
source ./00_sysgen_functions.sh
trap 'check_return' 0

set -e

cd sysgen
echo_step "Installing ncat"
sudo apt install -y ncat

rm -rf dasd
prev_dasd=$(ls -Art dasd.03.sysgen.*.tar | tail -n 1)
echo_step "Untarring $prev_dasd"
tar -xvf $prev_dasd
echo_step "Downloading SYSCPK"
ret=0
wget http://www.jaymoseley.com/hercules/downloads/archives/SYSCPK.tar.gz || ret=$?

if [ $ret -ne 0 ]; then
    echo_step "Download of http://www.jaymoseley.com/hercules/downloads/archives/SYSCPK.tar.gz failed"
    echo_step "Using archived copy"
    cp ../gz/SYSCPK.tar.gz ./
fi

echo_step "Extracting SYSCPK.tar.gz"
tar xvzf SYSCPK.tar.gz
rm SYSCPK.tar.gz
mv dasd/syscpk.3350 dasd/syscpk.3350.uncompressed
echo_step "Compressing syscpk.3350"
dasdcopy -z dasd/syscpk.3350.uncompressed dasd/syscpk.3350
rm dasd/syscpk.3350.uncompressed
echo_step "Creating user dasd"
bash ./create.dasd.sh user

chmod +x submit.sh
echo_step "Starting Hercules: hercules -f conf/mvs.cnf -r ../05_sysgen_system_customization.rc"
hercules -f conf/mvs.cnf -r ../05_sysgen_system_customization.rc > hercules.log

echo_step "Backing up hercules.log to hercules_log.customization.$date_time.log"
mv hercules.log hercules_log.customization.$date_time.log
echo_step "Backing up prt00e.txt to prt00e_customization.$date_time.txt"
cp prt00e.txt prt00e_customization.$date_time.txt
echo_step "Checking Job Return Codes"
tput bold
tput setaf 2
chmod +x ./condcode.rexx
./condcode.rexx prt00e.txt mvs00
./condcode.rexx prt00e.txt mvs01
./condcode.rexx prt00e.txt mvs02

echo_step "backing up DASD folder to dasd.customization.$date_time.tar"
tar cvf dasd.04.customization.$date_time.tar ./dasd
cd ..

trap : 0