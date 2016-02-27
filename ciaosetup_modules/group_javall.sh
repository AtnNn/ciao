# Group of commands for the Javall library

command_group_exp "Javall library"
command_help_exp <<EOF
  clean_java [not documented]
EOF

register_command "clean_java"
do__clean_java() {
    gmake_javall distclean
}
gmake_javall() {
    make SHELL=/bin/bash CIAODESRC=${CIAODESRC} ${GMAKEOPTS} -C ciao/library/javall $*
}

