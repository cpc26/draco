#!/bin/sh
#
# Draco's default file-type recognizer
#
# the following line has to be edited whenever this tool is installed.
Tools="/axolotl/data1/Draco/Tools"
file -L -m $Tools/draco-magic $1 | nawk -F':\t' '{print $2}'
