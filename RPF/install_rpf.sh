# Installs RPF
# This installs RPF V1R8M8 and up
# For use with Jay Moseley sysgen MVS only

source ../00_sysgen_functions.sh
trap 'check_return' 0

echo_step "Checking for new RPF version"
./update_rpf.sh

set -e

cd ../sysgen
echo_step "Starting Hercules: hercules -f conf/local.cnf -r ../RPF/install_rpf.rc"
hercules -f conf/local.cnf -r ../RPF/install_rpf.rc > hercules.log

echo_step "Backing up hercules.log to hercules_log.rpf_install.$date_time.log"
mv hercules.log hercules_log.rpf_install.$date_time.log
echo_step "Backing up prt00e.txt to prt00e.rpf_install.$date_time.txt"
cp prt00e.txt prt00e.rpf_install.$date_time.txt

trap : 0



