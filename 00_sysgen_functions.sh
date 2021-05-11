# Sysgen Functions


function check_return {

    echo >&2 "$(tput bold)$(tput setaf 1)
    ***************
    *** ABORTED ***
    ***************
    "
    echo "[!] Error encoutered exiting.$(tput sgr0)" >&2
    exit 1
}

function echo_step {
    echo "$(tput bold)$(tput setaf 4)[+] $1$(tput sgr0)"
}

function echo_warn {
    echo "$(tput bold)$(tput setaf 3)[+] $1$(tput sgr0)"
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


