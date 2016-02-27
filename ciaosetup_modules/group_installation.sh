# Group of commands for installation

# todo: When should we print this message?
# "Important: installation modifies the .bashrc/.cshrc files. In order
#  for these changes to be visible you need to log out and back in."

command_group "Installation"
command_help <<EOF
  install      Install the whole CiaoDE system (must have been
               compiled before).

  uninstall    Uninstall CiaoDE.
EOF
command_help_exp <<EOF
  install_to_destroot [not documented]
  install_ciao        [not documented]

  generate_revision [not documented]
  install_extras [not documented]
  fullinstall [not documented]
  justinstall [not documented]
  uninstallciao [not documented]
EOF
register_command "install"
do__install() {
    get_install_options "$@" 
    bold_message "Installing CiaoDE"
    do__install_ciao
    do__install_extras
    # Note: we don't use lpmake install to work around a bug in doc
    # generation that should be solved
    #     lpmake install
    bold_message "CiaoDE installation completed"
}
register_command "uninstall"
do__uninstall() {
    lpmake uninstall
}
register_command "install_to_destroot"
do__install_to_destroot() {
    if echo expr $* : '\(--destdir*=[^=][^=]*\)' >/dev/null  ; then 
	DESTDIR=`expr $* : '--destdir=\([^=][^=]*\)'`
    else
	exit_on_error  "incorrect option \"$ARG\". Should be of the form --destdir=[value]".
    fi
    export BUILD_ROOT=$DESTDIR
    bold_message "Installing CiaoDE into $DESTDIR"
    do__install_ciao

    bold_message "CiaoDE installation completed"
}

register_command "install_ciao"
do__install_ciao() {
    ( cd ${CIAODESRC}/ciao; lpmake install ) || return 1
}

register_command "generate_revision"
do__generate_revision() {
    lpmake generate_revision
}

register_command "install_extras"
do__install_extras() {
    lpmake install_extras
}
register_command "fullinstall"
do__fullinstall() {
    do__build
    do__docs
    do__install
}
register_command "justinstall"
do__justinstall() {
    lpmake justinstall
}

register_command "uninstallciao"
do__uninstallciao() {
    lpmake uninstallciao
}
