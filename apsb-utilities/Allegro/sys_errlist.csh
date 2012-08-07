#!/bin/csh
#
#	print comment from /usr/include/sys/errno.h for each errno code
#
set errno = 0
while (1)
	@ errno++
	set string = `grep \	$errno\	 /usr/include/sys/errno.h | awk -F"*" '{print $2}'`
	if (! $#string) break
	echo $string
end
