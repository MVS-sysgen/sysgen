# Sysgen Functions

command="./sysgen.sh"

function check_return {
    if [[ -z "${TERM}" ]]; then
        echo ""
    else
        echo "$(tput sgr0)"
    fi
    echo >&2 "
        ********************************
        *** !!ERROR!! SYSGEN ABORTED ***
        ********************************
        "
    echo "[!] Error encoutered Sysgen aborted." >&2
    echo "[!] To rerun this step issue the command $command" >&2

    exit 1
}

function git_long {
    if git tag > /dev/null 2>&1; then
        echo $(git log -1 | head -1 | awk '{print $2}')
    fi
}

function git_short {
    SHORTHASH=$( git_long )
    if [ -n "$SHORTHASH" ]; then
        echo ${SHORTHASH::7}
    else
        echo "ZIPPED "
    fi
}


function sysgen_check_return {
    echo ""
    echo "[!] Sysgen failed with the error above. Try rerunning the step."
    echo "[!] If the issue persists please submit an issue with the hercules.log and sysgen/prt00e.txt files attached."
    echo ""
}

function echo_step {
    if [[ -z "${TERM}" ]]; then
        echo "[+] $1"
    else
        echo "$(tput bold)$(tput setaf 4)[+] $1$(tput sgr0)"
    fi
}

function echo_warn {
    if [[ -z "${TERM}" ]]; then
        echo "[+] $1"
    else
        echo "$(tput bold)$(tput setaf 3)[+] $1$(tput sgr0)"
    fi
}


function check_failure {
    echo_step "Checking automate.rexx return code (check sysgen/hercules.log for errors if this check fails)"
    test ! -f failure
}


function ipl_install {

    echo_step "Generating files for IPL Install"
    cat install.rc > install.ipl.rc
    echo "ipl 150" >> install.ipl.rc
    sed -i "s/install.txt/install.ipl.txt/" install.ipl.rc

cat << END > install.ipl.txt
HHC00010A Enter input for console 0:0009;/
/*00 IFB010D ENTER 'IPL REASON,SUBSYSTEM ID' OR 'U';/r 0,u
/*01 $HASP426 SPECIFY OPTIONS - HASP-II, VERSION JES2 4.1;/r 01,noreq
/ $HASP099 ALL AVAILABLE FUNCTIONS COMPLETE;* Installing
END
cat install.txt >> install.ipl.txt
cat << END >> install.ipl.txt
# Shutting down
/ $HASP099 ALL AVAILABLE FUNCTIONS COMPLETE;/$pjes2
/ $HASP085 JES2 TERMINATION COMPLETE;/z eod
/ IEE334I HALT     EOD SUCCESSFUL;/quiesce
HHC00809I Processor CP00: disabled wait state;quit
END

}

function clean_up {

    echo_step "Cleaning Up"
    rm install.ipl.rc
    rm install.ipl.txt

}


date_time=$(date +"%Y-%m-%d-%H%M%S")

if [[ -f /usr/local/bin/hercules ]]
then
    HERCULES=/usr/local/bin/hercules
else
    HERCULES=hercules
fi
