# LOGIN.CL -- User login file for the IRAF command language.

# Identify login.cl version (checked in images.cl).
if (defpar ("logver"))
    logver = "IRAF V2.10EXPORT April 1992 revision 1"

set	home		= "/axolotl/data1/Draco/Samples/Imfort/"
set	imdir		= "HDR$"
set	uparm		= "home$uparm/"
set	userid		= "yen"

# Set the terminal type.
if (envget("TERM") == "sun") {
    if (!access (".hushiraf"))
	print "setting terminal type to gterm..."
    stty gterm
} else if (envget("TERM") == "xterm") {
    if (!access (".hushiraf"))
	print "setting terminal type to xterm..."
    stty xterm nl=44
} else {
    if (!access (".hushiraf"))
	print "setting terminal type to xterm..."
    stty xterm
}

# Uncomment and edit to change the defaults.
#set	editor		= vi
#set	printer		= lw
set	printer		= lp9
#set	stdimage	= imt800
#set	stdimcur	= stdimage
#set	stdplot		= lw
#set	clobber		= no
#set	filewait	= yes
#set	cmbuflen	= 512000
#set	min_lenuserarea	= 24000
#set	imtype		= "imh"

# IMTOOL/XIMAGE stuff.  Set node to the name of your workstation to
# enable remote image display.
#set	node		= ""
set	node		= "axolotl"

# CL parameters you mighth want to change.
#ehinit   = "nostandout eol noverify"
#epinit   = "standout showall"
showtype = yes

# Default USER package; extend or modify as you wish.  Note that this can
# be used to call FORTRAN programs from IRAF.

package user

task	$adb $bc $cal $cat $comm $cp $csh $date $df $diff	= "$foreign"
task	$du $find $finger $ftp $grep $lpq $ls $mail $make	= "$foreign"
task	$man $mon $mv $nm $od $ps $rcp $rlogin $rsh $ruptime	= "$foreign"
task	$rwho $sh $spell $sps $strings $su $telnet $tip $top	= "$foreign"
task	$touch $vi $emacs $w $wc $less $rusers $sync $pwd	= "$foreign"

task	$xc $mkpkg $generic $rtar $wtar $buglog			= "$foreign"
#task	$fc = "$xc -h $* -limfort -lsys -lvops -los"
task	$fc = ("$" // envget("iraf") // "unix/hlib/fc.csh" //
	    " -h $* -limfort -lsys -lvops -los")
task	$nbugs = ("$(setenv EDITOR 'buglog -e';" //
	    "less -Cqm +G " // envget ("iraf") // "local/bugs.*)")
task	$cls = "$clear;ls"

if (access ("home$loginuser.cl"))
    cl < "home$loginuser.cl"
;

keep;   clpackage

prcache directory
cache   directory page type help

# Print the message of the day.
if (access (".hushiraf"))
    menus = no
else {
    clear; type hlib$motd
}

# Delete any old MTIO lock (magtape position) files.
if (deftask ("mtclean"))
    mtclean
else
    delete uparm$mt?.lok,uparm$*.wcs verify-

# List any packages you want loaded at login time, ONE PER LINE.
images          # general image operators
plot            # graphics tasks
dataio          # data conversions, import export
lists           # list processing

# The if(deftask...) is needed for V2.9 compatibility.
if (deftask ("proto"))
    proto       # prototype or ad hoc tasks

tv              # image display
utilities       # miscellaneous utilities
noao            # optical astronomy packages

keep
