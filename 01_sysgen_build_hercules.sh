# MVS3.8 Sysgen Automation
source ./00_sysgen_functions.sh

trap 'check_return' 0
set -e

echo_step "Update apt"
sudo apt update -y
echo_step "Installing m4 make autoconf automake cmake flex build-essential regina-rexx libbz2-dev libregina3-dev zlib1g-dev"
sudo apt install m4 make autoconf automake cmake flex build-essential regina-rexx libbz2-dev libregina3-dev zlib1g-dev unzip
echo_step "Building external packages"
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
./configure --enable-cckd-bzip2 --enable-het-bzip2 --enable-regina-rexx --enable-extpkgs=$(realpath ../hercpkgs)
echo_step "Compiling Hercules"
make
echo_step "Installing Hercules"
sudo make install
cd ..

trap : 0