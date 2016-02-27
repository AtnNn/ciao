# Group of commands for benchmarking

command_group "Benchmarking"
command_help <<EOF
  runtests     Execute all benchmarks available in the system.
EOF
register_command "runbenchmarks"
do__runbenchmarks() {
    lpmake runbenchmarks
}
