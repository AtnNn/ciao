#!/bin/sh
#
# Update local CiaoDE and webpage in our main 'clip' machine
# 
# Authors: Edison Mera, Jose F. Morales

get_mydir() {
  old_dir=`pwd`
  cd `dirname $0`
  dir=`pwd`
  cd ${old_dir}
  echo ${dir}
}
self=`get_mydir`
##

SOURCEDIR=/home/clip/Systems/CiaoDE/trunk
CIAODESVNREP=/home/clip/SvnReps/Systems/CiaoDE
#HTMLDIR=/home/clip/public_html/Local/lpdoc_docs/CiaoDE/
#HTMLURL=/~clip/Local/lpdoc_docs/CiaoDE/
HTMLDIR=/home/clip/public_html/Software/Ciao/New/
HTMLURL=/~clip/Software/Ciao/New/
REQUIRED_USER=clip

check_srcdir() {
    if [ -f "${SOURCEDIR}" ] && [ ! -r "${SOURCEDIR}/ciaosetup" ]; then
	echo "Error: SOURCEDIR=${SOURCEDIR} does not seem to point to the actual source directory"
	exit 1
    fi
}

check_user() {
    USER=`id -nu`
    if [ x${USER} != x${REQUIRED_USER} ]; then
	echo "Error: the script `basename $0` can only be executed by the user ${REQUIRED_USER}"
	exit 1
    fi
}

update_ciaode() {
    # uninstall current CiaoDE, if installed
    if [ -e ${SOURCEDIR} ]; then
	cd ${SOURCEDIR}
	./ciaosetup uninstall
	# update CiaoDE's working copy in clip
	cd ${SOURCEDIR}/..
	rm -rf ${SOURCEDIR}
    fi
    # We need to have a fully functional working copy in order to do some tests:
    /usr/bin/svn co --force file:///${CIAODESVNREP}/trunk ${SOURCEDIR}
    cd ${SOURCEDIR}

    # install CiaoDE's working copy in clip
    # kludge: Enable java interface when clip have javac well configured
    ./ciaosetup realclean
#    ./ciaosetup configure --instype=ins --sysavail=user --compile_ciaoppcl=yes --htmldir=${HTMLDIR} --htmlurl=${HTMLURL} --with_java_interface=no
    # TODO: Changed from 'ins' to 'src'. I had problems with 'ins' installation type
    ./ciaosetup configure --instype=src --sysavail=user --compile_ciaoppcl=yes --htmldir=${HTMLDIR} --htmlurl=${HTMLURL} --with_java_interface=no
    ./ciaosetup build
    ./ciaosetup docs
    ./ciaosetup install
    # regression tests: this is executed only to check that everything is ok
    # ./ciaosetup runtests
}

update_profile() {
    # Include profile info (not included yet)
    . ${SOURCEDIR}/ciao/etc/DOTprofile
    hash
}

update_distweb() {
    # Update distribution web site of the latest packaged revision
    ${SOURCEDIR}/website/create.sh New
    # cd /home/clip/tmp/rotd/ && sudo -H -u clip ./createrotd.sh
}

check_user
check_srcdir
update_ciaode
update_profile
update_distweb
