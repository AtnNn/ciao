# Group of commands for CiaoPP

command_group_exp "CiaoPP"
command_help_exp <<EOF
  allciaopp    [not documented]
  calibrateciaopp [not documented]
EOF
register_command "allciaopp"
do__allciaopp() {
    ( cd ${CIAODESRC}/ciaopp; lpmake all ) || return 1
}
register_command "calibrateciaopp"
do__calibrateciaopp() {
    ( cd ${CIAODESRC}/ciaopp; lpmake calibrate ) || return 1
}

