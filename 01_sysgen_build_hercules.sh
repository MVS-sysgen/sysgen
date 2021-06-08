# MVS3.8 Sysgen Automation
source ./00_sysgen_functions.sh

trap 'check_return' 0
set -e

echo_step "Building external packages"
if [ -d "build" ]; then rm -rf build/; fi
mkdir build
cd build
mkdir ./hercpkgs
mkdir ./WORK


for i in crypto decNumber SoftFloat telnet; do
    echo_step "Building $i"
    git clone https://github.com/SDL-Hercules-390/$i.git
    mkdir ./WORK/$i
    cd ./WORK/$i
    ../../$i/build --pkgname . --all --install ../../hercpkgs
    cd ../../
done

echo_step "Building Hercules Hyperion SDL"
git clone https://github.com/SDL-Hercules-390/hyperion.git
cd hyperion
./configure --enable-cckd-bzip2 --enable-het-bzip2 --enable-regina-rexx --enable-extpkgs=$(realpath ../hercpkgs) --enable-optimization="-O3 -march=native"
echo_step "Compiling Hercules"
# thanks Mike Grossman for the CPU/o3
export NUMCPUS=`grep -c '^processor' /proc/cpuinfo`
make -j$NUMCPUS --load-average=$NUMCPUS
echo_step "Installing Hercules"
sudo make install
cd ..

trap : 0
