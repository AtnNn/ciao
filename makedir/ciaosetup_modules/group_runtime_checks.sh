# Group of commands for the full compilation of Ciao with run-time
# checks enabled.

command_group_exp "Run-time Checks"
command_help_exp <<EOF
  rt-compile0 [not documented]
  rt-compile1 [not documented]

    The rt-compile option includes a lot of steps and is not currently
    incremental.  It needs to be improved once we have move the whole
    system to (optionally) placing the compiled modules
    (.po/.asr/.itf) in a single directory (as done by optim_comp).

    To compile CiaoDE with run-time checks in the libraries, run:

      ./ciaosetup rt-compile0
      ./ciaosetup rt-compile1
EOF

#
#  -- EMM

register_command "rt_compile0"
do__rt_compile0() {
    do__realclean
    run_config --silent=true \
        --reset --instype=src --sysavail=user \
        --compile_chr=no \
        --compile_ciaoppcl=no \
	--gen_ciao_asr=no \
	--gen_ciaopp_asr=no
    do__allnolibs
}

register_command "rt_compile1"
do__rt_compile1() {
    run_config --silent=true \
	--compile_ciaoppcl=yes \
	--runtime_checks=yes "$*"
    do__clean_lib
    do__libraries
#   do__calibrateciaopp
}
