#!/bin/sh
# the following line has to be edited whenever this tool is installed.
Tools="/axolotl/data1/Draco/Tools"
if $Tools/GEIS-kval imagetyp $1 | fgrep KSPOTS > /dev/null
then echo "GEIS (kspots)"
fi
