#!/bin/bash
# Downloads newest REVIEW

function echoc {
    echo "$(tput bold)$(tput setaf 3)[+] $1$(tput sgr0)"
}

cd $(dirname $0)

echoc "Getting newest RECV370"
#wget "http://www.prycroft6.com.au/vs2sw/download/rev370.zip"
wget "http://www.prycroft6.com.au/REVIEW/download/revhelp.zip"
wget "http://www.prycroft6.com.au/REVIEW/download/rev370ld.zip"
wget "http://www.prycroft6.com.au/REVIEW/download/revclist.zip"
wget "http://www.prycroft6.com.au/REVIEW/download/revasm.zip"

for i in *.zip; do
    unzip -o $i
done
mkdir -p zip
mv *.zip zip


