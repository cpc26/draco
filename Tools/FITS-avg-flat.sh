#!/bin/sh
# the following line has to be edited whenever this tool is installed.
Tools="/axolotl/data1/Draco/Tools"
if $Tools/FITS-flat.sh $1 > /dev/null && $Tools/FITS-kval NCOMBINE $1 | fgrep NCOMBINE > /dev/null
then echo "FITS (averaged flat)"
fi
