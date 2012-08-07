#!/bin/sh
#
# print auxiliary documents
#
# syntax: lpr-doc [printer]
#
EJECT="$HOME/Bin/eject"
if test $# -eq 1
then
LP=$1
else
LP=$PRINTER
fi
MDS="/marian/data2/mds"
Samples="/axolotl/data1/Draco/Samples"
TEMP="/tmp/lpr-doc.tmp"
cat	$MDS/Draco.sh			$EJECT	> $TEMP
cat	$MDS/Draco*.log			$EJECT	>> $TEMP
cat	$MDS/Draco.inv			$EJECT	>> $TEMP
pr	$MDS/.Draco.lisp			>> $TEMP
pr	$HOME/.Draco.lisp			>> $TEMP
cat	$Samples/packages.lisp		$EJECT	>> $TEMP
cat	$Samples/converters.lisp	$EJECT	>> $TEMP
cat	$Samples/procedures.lisp	$EJECT	>> $TEMP
cat	$Samples/primitives.lisp	$EJECT	>> $TEMP
cat	$Samples/implementations.lisp	$EJECT	>> $TEMP
cat	$Samples/data-types.lisp	$EJECT	>> $TEMP
cat	$Samples/file-types.lisp		>> $TEMP
lpr -P$LP -r $TEMP
