
set -e
cd $(dirname $0)
echo "[+] Downloading Brexx"
version="NIGHTLY"
filename="$(python3 ./update.py)"
echo "[+] Current BREXX version: '$version'"

echo "[+] Generating JCL"
sed "s/##VERSION##/$version/g" 01_receive_install.template > 01_receive_install.jcl
sed "s/##VERSION##/$version/g" 02_brexx_unpack.template > 02_brexx_unpack.jcl
sed "s/##VERSION##/$version/g" 03_brexx_install.template > 03_brexx_install.jcl

echo "[+] Collecting XMI"
unzip -o $filename
mkdir -p zip
mv *.zip zip
mv BREXX370_$version/BREXX.$version.XMIT ./BREXX.INSTALL.XMI

echo "[+] Done"
