#!/bin/bash
#
# Purpose:  Copies a text file containing JCL statements to MVS socket reader
# Author:   Jay Moseley
# Date:     Fri April 24, 2020
#
#           Single argument specifies a text file containing JCL statements
#           which are passed to Hercules socket reader for execution by MVS
#           using netcat

echo "This script submits JCL from a text file to an MVS socket reader."

if test "$#" -ne 1; then
    echo "Syntax: ./submit.sh <text file to submit>"
    exit 1
fi

echo "Sending $1"

cat $1 | ncat --send-only -w1 localhost 3505
