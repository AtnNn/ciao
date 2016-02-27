# Group of commands for testing

command_group "Testing"
command_help <<EOF
  runtests     Execute all tests (regression, etc.) available in the system.
EOF
register_command "runtests"
do__runtests() {
    lpmake runtests
}
