# Group of commands for command-line help

command_group "Help"
command_help <<EOF 
  help         Display this help and exit
  help-exp     Display advanced help and exit 
EOF
register_command "help"
do__help() {
    show_help_banner
    show_help
}
register_command "help_exp"
do__help_exp() {
    show_help_banner
    show_help_exp
}
show_help_banner() {
    cat <<EOF
usage: `basename $0` <subcommand>
CiaoDE command-line setup tool.

Available subcommands:
EOF
}

# No command
# (this is a hard-wired command for build_command_case)
do__no_command() {
    cat <<EOF
Type '`basename $0` help' for usage.
EOF
}

# Unknown command
# (this is a hard-wired command for build_command_case)
do__unknown() {
    exit_on_error_q "Unknown command: '""$1""'.\nTry '""`basename $0` help""' for usage."
}

