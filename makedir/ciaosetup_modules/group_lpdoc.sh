# Group of commands for LPdoc

command_group_exp "LPdoc"
command_help_exp <<EOF
  all_lpdoc     Perform 'all' command on 'lpdoc' component
EOF
register_command "all_lpdoc"
do__all_lpdoc() {
    ( cd ${CIAODESRC}/lpdoc; lpmake all ) || return 1
}

