# Group of commands for Profiler

command_group_exp "Profiling configuration"
command_help_exp <<EOF    
  profiler   [not documented]
  clean_profiler [not documented]
EOF
register_command "profiler"
do__profiler() {
    ( cd ${CIAODESRC}/ciao; lpmake profiler ) || return 1
}
register_command "clean_profiler"
do__clean_profiler() {
    do__clean_dir_recursive ${CIAODESRC}/ciao/contrib/profiler
}
