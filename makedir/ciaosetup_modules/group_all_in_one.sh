# Group of all-in-one commands.

command_group "All-in-one commands"
command_help <<EOF
  user-install Configure, compile the system to be executed from the
               sources, and install it in your user account. Warnings 
               about unused predicates are enabled.  Used by Ciao developers. 

 devel-install Like user-install, but enables runtime checks by default.
EOF
command_help_exp <<EOF
  user-compile   [not documented]
  system-install [not documented]
  alldocs        [not documented]
EOF
register_command "user_install"
do__user_install() {
    run_config --silent=true --reset --instype=src --sysavail=user --unused-pred-warnings=yes
    do__build
    do__docs
    do__install
}
register_command "user_compile"
do__user_compile() {
    run_config --silent=true --reset --instype=src --sysavail=user --unused-pred-warnings=yes
    do__build
}
register_command "devel_install"
do__devel_install() {
    run_config --unused-pred-warnings=yes \
	--ciaosh-commands="\"set_prolog_flag(runtime_checks,yes)\"" \
	--reset --instype=src --sysavail=user
    do__build
    do__docs
    do__install
}
register_command "system_install"
do__system_install() {
    run_config --silent=true
    do__build
    do__docs
    do__install
}
register_command "alldocs"
do__alldocs() {
    do__build
    do__docs
}

