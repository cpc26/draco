#!/bin/sh
# the following line has to be edited whenever this tool is installed.
Tools="/axolotl/data1/Draco/Tools"
if $Tools/OIF-dark.sh $1 > /dev/null && $Tools/pkval.e NCOMBINE `$Tools/truename $1` | fgrep NCOMBINE > /dev/null
then echo "OIF (averaged dark)"
fi
