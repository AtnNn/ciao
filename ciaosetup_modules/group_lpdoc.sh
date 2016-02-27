# Group of commands for LpDoc

command_group_exp "LpDoc"
command_help_exp <<EOF
  alllpdoc     [not documented]
EOF
register_command "alllpdoc"
do__alllpdoc() {
    ( cd ${CIAODESRC}/lpdoc; lpmake all ) || return 1
}

