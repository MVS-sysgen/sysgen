# Using SMP4 to Build the Distribution Libraries
source ./00_sysgen_functions.sh
trap 'check_return' 0
set -e
echo_step "Building the MVS Starter System"
cd sysgen
echo_step "Removing previously built dasd"
rm -rf ./dasd
echo_step "Creating DASD"
bash create.dasd.sh starter
echo_step "Starting Hercules: hercules -f conf/ibcdmprs.cnf -r ../01_sysgen_starter.rc -d"
echo_step "Log file: sysgen.build_starter.$date_time.log"
$HERCULES -f conf/ibcdmprs.cnf -r ../01_sysgen_starter.rc > hercules.log
echo_step "Creating dasd.build_starter.$date_time.tar"
tar cvf dasd.01.build_starter.$date_time.tar ./dasd
cd ..

trap : 0