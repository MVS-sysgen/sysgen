echo "Creating NJE38 spool volume"

set -e
cd $(dirname $0)

echo "Generating source and maclib JCL"

bash 01_make_maclib_jcl.sh > 01_make_maclib.jcl
bash 02_make_asmsrc_jcl.sh > 02_make_asmsrc.jcl

echo "Adding NJE38 DASD and BSC line to conf/local.cnf"

cat << 'END' >> ../../sysgen/conf/local.cnf
#############################################
#
# NJE38 Config
#
#############################################
#
# Temporary NJE38 settings below, modify for your environment
#
0602 tcpnje 2703 lnode=SYSA rnode=SYSB lport=1175 rport=1175 rhost=10.10.10.10
#############################################
END
