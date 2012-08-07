#!/bin/sh
# the following line has to be edited whenever this tool is installed.
Tools="/axolotl/data1/Draco/Tools"
if $Tools/FITS-kval IMAGETYP $1 | fgrep COMPARISON > /dev/null
then echo "FITS (comparison)"
fi
