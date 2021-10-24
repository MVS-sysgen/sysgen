
set -e
cd $(dirname $0)
echo "[+] Downloading Brexx"
version=$(python3 ./update.py)
echo "[+] Current BREXX version: '$version'"

echo "[+] Generating JCL"
sed "s/##VERSION##/$version/g" 01_receive_install.template > 01_receive_install.jcl
sed "s/##VERSION##/$version/g" 02_brexx_unpack.template > 02_brexx_unpack.jcl
sed "s/##VERSION##/$version/g" 03_brexx_install.template > 03_brexx_install.jcl

echo "[+] Collecting XMI"
unzip -o BREXX370_$version.zip
mkdir -p zip
mv *.zip zip
mv BREXX370_$version/BREXX_$version.xmit ./BREXX.INSTALL.XMI

echo "[+] Done"
