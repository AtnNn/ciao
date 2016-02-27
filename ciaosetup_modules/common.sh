MAKE='make -s SHELL=/bin/bash'
gmake_ciao_lpmakeopts() {
    LPMAKEOPTS="${LPMAKEOPTS}" ${MAKE} CIAODESRC=${CIAODESRC} ${GMAKEOPTS} -C ciao $*
}

gmake_ciao() {
    ${MAKE} CIAODESRC=${CIAODESRC} ${GMAKEOPTS} -C ciao $*
}

lpmake() {
    eval $LPMAKECMD $LPMAKEOPTS $*
}

exit_on_error() {
    echo "`basename $0`: error: $1"
    exit 1
}

exit_on_error_q() {
    echo "$1"
    exit 1
}

setup_vars() {
    # WARNING: most of those variables are duplicated in ciao/SHARED,
    # so you must have to synchronize it with this function.
    CIAODESRC=${self}
    CIAOSRC=${CIAODESRC}/ciao
    if [ ! -f ${CIAOSRC}/SETTINGS ] ; then
	do__settings_default
    fi
    SRCBINDIR=${CIAODESRC}/build/bin
    LPMAKE=${SRCBINDIR}/lpmake.sta
    CIAOARCH=`${CIAOSRC}/etc/ciao_get_arch`
    SETLOCALCIAO=`cd ${CIAOSRC} ; make -s get_setlocalciao`
    LPMAKECMD="$SETLOCALCIAO $LPMAKE"
    MAKEARG=
    HELP=
    INVALIDOPT=
    CONFIGLEVELOPT="-d CONFIGLEVEL=1"
    RESET=
    CONTINUE=
}

GMAKEOPTS="--no-print-directory"
#GMAKEOPTS=""

