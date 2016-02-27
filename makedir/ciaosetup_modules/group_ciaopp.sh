# Group of commands for CiaoPP

command_group_exp "CiaoPP"
command_help_exp <<EOF
  all_ciaopp      Perform 'all' command on 'ciaopp' component
  calibrateciaopp [not documented]
EOF
register_command "all_ciaopp"
do__all_ciaopp() {
    ( cd ${CIAODESRC}/ciaopp; lpmake all ) || return 1
}
register_command "calibrateciaopp"
do__calibrateciaopp() {
    ( cd ${CIAODESRC}/ciaopp; lpmake calibrate ) || return 1
}

