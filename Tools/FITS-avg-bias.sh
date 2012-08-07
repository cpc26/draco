#!/bin/sh
# the following line has to be edited whenever this tool is installed.
Tools="/axolotl/data1/Draco/Tools"
if $Tools/FITS-bias.sh > /dev/null $1 && $Tools/FITS-kval NCOMBINE $1 | fgrep NCOMBINE > /dev/null
then echo "FITS (averaged bias)"
fi
