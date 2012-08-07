#!/bin/sh
# the following line has to be edited whenever this tool is installed.
Tools="/axolotl/data1/Draco/Tools"
if $Tools/FITS-kval IMAGETYP $1 | egrep '(BIAS)|(DARK)|(FLAT)' > /dev/null
then echo "FITS (calibration)"
fi
