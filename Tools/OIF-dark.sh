#!/bin/sh
# the following line has to be edited whenever this tool is installed.
Tools="/axolotl/data1/Draco/Tools"
if $Tools/pkval.e imagetyp `$Tools/truename $1` | fgrep DARK > /dev/null
then echo "OIF (dark)"
fi
