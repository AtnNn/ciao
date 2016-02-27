# Group of commands for the Ciao emacs-mode

command_group_exp "Emacs mode"
command_help_exp <<EOF
  clean_emacs [not documented]
EOF

register_command "clean_emacs"
do__clean_emacs() {
    gmake_emacs_mode distclean
}

gmake_emacs_mode() {
    make SHELL=/bin/bash CIAODESRC=${CIAODESRC} ${GMAKEOPTS} -C emacs-mode $*
}

