# Installs SYSGEN Software
# For use with Jay Moseley sysgen MVS only
cd $(dirname $0)

source ../../00_sysgen_functions.sh
trap 'check_return' 0


cd SOFTWARE
echo_step "Downloading FTPD"
git clone https://github.com/MVS-sysgen/FTPD.git
cd ..

cd sysgen
chmod +x submit.sh

echo_step "Starting Hercules: hercules -f conf/local.cnf -r ../06_sysgen_software_install.rc"
hercules -f conf/local.cnf -r ../06_sysgen_software_install.rc > hercules.log

echo_step "Backing up hercules.log to hercules_log.software.$date_time.log"
mv hercules.log hercules_log.software.$date_time.log
echo_step "Backing up prt00e.txt to prt00e_software.$date_time.txt"
cp prt00e.txt prt00e_software.$date_time.txt
echo_step "Checking Job Return Codes"



