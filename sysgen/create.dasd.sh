#!/bin/sh
#
# Purpose:  Creates DASD volumes for MVS 3.8j System Generation
# Author:   Jay Moseley
# Date:     Fri April 24, 2020
#
#           Single argument of starter | dlibs | sysgen | user
#           determines the volumes that will be created for
#           the stage of the generation process based upon
#           the argument

echo "This script creates DASD volumes for MVS 3.8j."

#
# Ensure that valid argument supplied on invocation
# -------------------------------------------------
case $1 in
  starter ) ;;
  dlibs ) ;;
  sysgen ) ;;
  user ) ;;
  * ) echo "Syntax: ./create.dasd.sh {starter | dlibs | sysgen | user}"
  exit 1;;
esac

#
# Prompt for compression
# ----------------------
# while true; do
#     read -p "Do you want to create compressed DASD images? (y or n)" yn
#     case $yn in
#         [Yy]* ) compress='-z'; break;;
#         [Nn]* ) compress=''; break;;
#         * ) echo "Please answer y or n.";;
#     esac
# done
compress='-z'
#
# Ensure that there is a dasd directory below this one
# ----------------------------------------------------
if [ ! -d dasd ]
then
  echo "Creating dasd subdirectory"
  mkdir dasd
fi

#
# Change to the dasd subdirectory where DASD images are to be created
# -------------------------------------------------------------------
cd dasd
echo -n "Changed directory to: "
pwd

case $1 in
  starter )
    #
    # Creating START1 (3330)
    # ----------------------
    if [ -f start1.3330 ]
    then
      rm start1.3330
    fi
    echo "dasdinit -a $compress start1.3330 3330 111111"
    dasdinit -a $compress start1.3330 3330 111111
    #
    # Creating SPOOL0 (3330)
    # ----------------------
    if [ -f spool0.3330 ]
    then
      rm spool0.3330
    fi
    echo "dasdinit -a $compress spool0.3330 3330 222222"
    dasdinit -a $compress spool0.3330 3330 222222
  ;;
  dlibs )
    #
    # Creating WORK00 (3350)
    # ----------------------
    if [ -f work00.3350 ]
    then
      rm work00.3350
    fi
    echo "dasdinit -a $compress work00.3350 3350 111111"
    dasdinit -a $compress work00.3350 3350 111111
    #
    # Creating WORK01 (3350)
    # ----------------------
    if [ -f work01.3350 ]
    then
      rm work01.3350
    fi
    echo "dasdinit -a $compress work01.3350 3350 222222"
    dasdinit -a $compress work01.3350 3350 222222
    #
    # Creating SMP000 (3350)
    # ----------------------
    if [ -f smp000.3350 ]
    then
      rm smp000.3350
    fi
    echo "dasdinit -a $compress smp000.3350 3350 333333"
    dasdinit -a $compress smp000.3350 3350 333333
  ;;
  sysgen )
    #
    # Creating MVSRES (3350)
    # ----------------------
    if [ -f mvsres.3350 ]
    then
      rm mvsres.3350
    fi
    echo "dasdinit -a $compress mvsres.3350 3350 111111"
    dasdinit -a $compress mvsres.3350 3350 111111
    #
    # Creating MVS000 (3350)
    # ----------------------
    if [ -f mvs000.3350 ]
    then
      rm mvs000.3350
    fi
    echo "dasdinit -a $compress mvs000.3350 3350 222222"
    dasdinit -a $compress mvs000.3350 3350 222222
    #
    # Creating SPOOL1 (3350)
    # ----------------------
    if [ -f spool1.3350 ]
    then
      rm spool1.3350
    fi
    echo "dasdinit -a $compress spool1.3350 3350 333333"
    dasdinit -a $compress spool1.3350 3350 333333
    #
    # Creating PAGE00 (3350)
    # ----------------------
    if [ -f page00.3350 ]
    then
      rm page00.3350
    fi
    echo "dasdinit -a $compress page00.3350 3350 444444"
    dasdinit -a $compress page00.3350 3350 444444
  ;;
  user )
    #
    # Creating PUB000 (3380)
    # ----------------------
    if [ -f pub000.3380 ]
    then
      rm pub000.3380
    fi
    echo "dasdinit -a $compress pub000.3380 3380 111111"
    dasdinit -a $compress pub000.3380 3380 111111
    #
    # Creating PUB001 (3390)
    # ----------------------
    if [ -f pub001.3390 ]
    then
      rm pub001.3390
    fi
    echo "dasdinit -a $compress pub001.3390 3390 222222"
    dasdinit -a $compress pub001.3390 3390 222222
    #
    # Creating SORTW1 (2314)
    # ----------------------
    if [ -f sortw1.2314 ]
    then
      rm sortw1.2314
    fi
    echo "dasdinit -a $compress sortw1.2314 2314 333333"
    dasdinit -a $compress sortw1.2314 2314 333333
    #
    # Creating SORTW2 (2314)
    # ----------------------
    if [ -f sortw2.2314 ]
    then
      rm sortw2.2314
    fi
    echo "dasdinit -a $compress sortw2.2314 2314 444444"
    dasdinit -a $compress sortw2.2314 2314 444444
    #
    # Creating SORTW3 (2314)
    # ----------------------
    if [ -f sortw3.2314 ]
    then
      rm sortw3.2314
    fi
    echo "dasdinit -a $compress sortw3.2314 2314 555555"
    dasdinit -a $compress sortw3.2314 2314 555555
    #
    # Creating SORTW4 (2314)
    # ----------------------
    if [ -f sortw4.2314 ]
    then
      rm sortw4.2314
    fi
    echo "dasdinit -a $compress sortw4.2314 2314 666666"
    dasdinit -a $compress sortw4.2314 2314 666666
    #
    # Creating SORTW5 (2314)
    # ----------------------
    if [ -f sortw5.2314 ]
    then
      rm sortw5.2314
    fi
    echo "dasdinit -a $compress sortw5.2314 2314 777777"
    dasdinit -a $compress sortw5.2314 2314 777777
    #
    # Creating SORTW6 (2314)
    # ----------------------
    if [ -f sortw6.2314 ]
    then
      rm sortw6.2314
    fi
    echo "dasdinit -a $compress sortw6.2314 2314 888888"
    dasdinit -a $compress sortw6.2314 2314 888888
  ;;
esac

#
# Return to parent directory
# --------------------------
echo "Returning to parent directory"
cd ..

#
# Finished!
# ---------
echo "Script completed successfully!"
exit 0
