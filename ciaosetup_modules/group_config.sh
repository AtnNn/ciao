# Group of commands for Ciao system configuration

command_group "Configuration"
command_help <<EOF
  configure    Configure the system before build
               (use 'configure --help' for list of options).
EOF
command_help_exp <<EOF
  config_prepare [not documented]
  user_config    [not documented]
  menuconfig     [not documented]
  justmenuconfig [not documented]
  reconfigure    [not documented]
  settings_default [not documented]
  clean_config   [not documented]
EOF
register_command "configure"
do__configure() {
    run_config $*
}
register_command "config_prepare"
do__config_prepare() {
    do__settings_default
    gmake_ciao config_prepare
}
register_command "user_config"
do__user_config() {
    run_config --silent=true --instype=src --sysavail=user $*
}

register_command "menuconfig"
do__menuconfig() {
    ( cd ${CIAODESRC}/ciao; lpmake menuconfig ) || return 1
}
register_command "justmenuconfig"
do__justmenuconfig() {
    ( cd ${CIAODESRC}/ciao; lpmake justmenuconfig ) || return 1
}

register_command "reconfigure"
do__reconfigure() {
    lpmake reconfigure
}

register_command "settings_default"
do__settings_default() {
    cat $CIAOSRC/SETTINGS_DEFAULT > $CIAOSRC/SETTINGS

    cat >> $CIAOSRC/SETTINGS <<EOF
CIAODESRC=${CIAODESRC}
REALLIBDIR=${CIAODESRC}/ciao
ENGINEDIR=${CIAODESRC}/ciao/bin
INSTALL_LOG=${CIAODESRC}/install.log
#ABSSRC=${CIAODESRC}/${CIAOSRC}
RUNTIME_CHECKS=$RUNTIME_CHECKS
UNUSED_PRED_WARNINGS=$UNUSED_PRED_WARNINGS
CFLAGS=${EXTRA_CFLAGS}
LDFLAGS=${EXTRA_LDFLAGS}
EOF
}

run_config() {
    get_config_options $*

    if [ x$HELP = x1 ] ; then
	config_help
	exit 0
    elif [ x$INVALIDOPT != x ]; then
	 exit_on_error "unrecognized configure option: $INVALIDOPT.\nTry \"$0 configure --help\" for usage."
    fi

    bold_message "Configuring Ciao"

    /bin/rm -f $CIAOSRC/SETTINGS

    if [ x$RESET = x1 ] ; then
#	normal_message "Erasing previous configuration"
	rm -f $CIAOSRC/SETTINGS_AUTO
        # if no more commands to process, and no default configuration:
	if [ x$CONTINUE = x0 ] ; then
	    if [ $# = 1 ] ; then
		bold_message "End of configuration"
		exit 0
	    fi
	fi
    fi

    do__settings_default
    
    if [ x$MAKEARG = x ] ; then
	MAKEARG=menuconfig
    else
	normal_message "make argument: $MAKEARG"
    fi

    LPMAKEOPTS="$CONFIGLEVELOPT $LPMAKEOPTS"

    gmake_ciao_lpmakeopts ${MAKEARG}
}

get_config_options() {
    if [ ! $# = 0 ] ; then
	for ARG in "$@" ; do
	    if [ x$ARG = x--menu ] ; then
		CONFIGLEVELOPT=
	    elif [ x$ARG = x--reset ] ; then
		RESET=1
		CONTINUE=0
	    elif [ x$ARG = x--preserve ] ; then
		RESET=0
	    elif [ x$ARG = x--default ] ; then
		RESET=1
		CONTINUE=1
	    elif [ x$ARG = x--help ] ; then
		HELP=1
	    elif expr $ARG : '\(--[^=][^=]*=..*\)' >/dev/null  ; then 
		OPT=`expr $ARG : '--\([^=][^=]*\)=..*'|sed -e s:_:-:g`
		VAL=`expr $ARG : '--[^=][^=]*=\(..*\)'`
                if [ x$OPT = xcflags ] ; then
		    EXTRA_CFLAGS=$VAL
		elif [ x$OPT = xldflags ] ; then
		    EXTRA_LDFLAGS=$VAL
	    # MAKEARG is undocumented because it is only used for debugging.
		elif [ x$OPT = xmakearg ] ; then
		    MAKEARG=$VAL
		elif [ x$OPT = xinstype ] ; then
		    RESET=1
		elif [ x$OPT = xsysavail ] ; then
		    RESET=1
		elif [ x$OPT = xunused-pred-warnings ] ; then
		    UNUSED_PRED_WARNINGS=$VAL
		elif [ x$OPT = xruntime-checks ] ; then
		    RUNTIME_CHECKS=$VAL
		fi
		LPMAKEOPTS="${LPMAKEOPTS} -d ${OPT}=${VAL}"
	    else 
		INVALIDOPT=$ARG
	    fi 
	done
    else
	RESET=1
	CONTINUE=1
    fi
}

get_install_options() {
    for ARG in "$@"
    do
	if expr $ARG : '\(--[^=][^=]*=[^=][^=]*\)' >/dev/null ; then 
	    OPT=`expr $ARG : '--\([^=][^=]*\)=[^=][^=]*'`
	    VAL=`expr $ARG : '--[^=][^=]*=\([^=][^=]*\)'`
	    LPMAKEOPTS="${LPMAKEOPTS} -d ${OPT}=${VAL}"
	else 
	    exit_on_error "incorrect option \"$ARG\". Should be of the form --[option]=[value]".
	fi
    done
}

# Note: This message will be used also in the README file.
# Be sure that if you modify this, the README file is well
# generated.
config_help() {
    cat ${self}/ciaosetup_modules/ciaosetup.opts
}

register_command "clean_config"
do__clean_config() {
    rm -rf ${CIAODESRC}/ciao/SETTINGS ${CIAODESRC}/ciao/SETTINGS_GSL \
	${CIAODESRC}/ciao/SETTINGS_AUTO ${CIAODESRC}/ciao/lib/autoconfig/components \
	${CIAODESRC}/ciao/library/pillow/icon_address.pl
}

