#!/bin/sh
# the following line has to be edited whenever this tool is installed.
Tools="/axolotl/data1/Draco/Tools"
if $Tools/FITS-kval IMAGETYP $1 | fgrep FLAT > /dev/null
then echo "FITS (flat)"
fi
