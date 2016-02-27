# Group of commands for MiniProlog

command_group_exp "MiniProlog (a simple WAM for time analysis)"
command_help_exp <<EOF    
  miniprolog   [not documented]
  clean_miniprolog [not documented]
EOF
register_command "miniprolog"
do__miniprolog() {
    ( cd ${CIAODESRC}/ciao; lpmake miniprolog ) || return 1
}
register_command "clean_miniprolog"
do__clean_miniprolog() {
    gmake_miniprolog clean
}
gmake_miniprolog() {
    if [ -d ciao/contrib/miniprolog/miniprolog ] ; then
	make SHELL=/bin/bash CIAODESRC=${CIAODESRC} ${GMAKEOPTS} -C ciao/contrib/miniprolog/miniprolog $*
    fi
}
