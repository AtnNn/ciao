# Group of commands for the compilation of Ciao

command_group "Compilation"
command_help <<EOF    
  build        Compile the whole CiaoDE system (engine, libraries, ...)
               (omitting docs, see 'docs' option below).
EOF
command_help_exp <<EOF    
  promote      Promote bootstrap 'ciaoc' compiler

  justall      [not documented]
  all          [not documented]
  all_extra    Perform 'all' command on all components (except 'ciao')
  engine       [not documented]
  compiler     [not documented]
  applications [not documented]

  all_ciao       Perform 'all' command on 'ciao' component
  all_ciao_gmake 'all' command on gmake of 'ciao' component (internal)
  libraries      [not documented]
  librariesciao  [not documented]
  platdep        [not documented]
  crossengine    [not documented]
  librariesextra [not documented]
  allnolibs      [not documented]
  allnolibsciao  [not documented]
  allnolibsextra [not documented]

  clean_eng [not documented]
  clean_etc [not documented]
  clean_bin [not documented]
  clean_lib [not documented]
  rpdclean [not documented]
  clean_nodistribute [not documented]
EOF
register_command "build"
do__build() {
    bold_message "Compiling CiaoDE"
    if [ ! -f ciao/SETTINGS ]; then
	exit_on_error "$0 configure should be executed before compilation."
    else
	do__justall
    fi
    bold_message "CiaoDE compilation completed"
}
register_command "promote"
do__promote() {
    bold_message "Promoting bootstrap compiler"
    normal_message "The current compiler will become the next bootstrap compiler"
    normal_message "(Warning: do not call it unless you are completely sure)"
    if ask_yesno "Are you sure?" && ask_yesno "Really?"; then
	gmake_ciao promote_internal
	normal_message "Bootstrap compiler promoted"
    else
	normal_message "Promotion canceled"
    fi
}
register_command "justall"
do__justall() {
    do__all
}
register_command "all"
do__all() {
    do__engine
    gmake_ciao config_prepare
    # Kludge because 'lpmake all' breaks installer in Windows:
    # TODO: Why?
    do__all_ciao
    do__all_chr
    do__all_extra
#    do__stateng
}
register_command "all_ciao"
do__all_ciao() {
    # Kludge because 'lpmake all' breaks installer in Windows:
    # TODO: Why?
    ( cd ${CIAODESRC}/ciao; lpmake all ) || return 1
}
register_command "all_extra"
do__all_extra() {
    lpmake all_extra
}
register_command "engine"
do__engine() {
    gmake_ciao engine
}
register_command "stateng"
do__stateng() {
    gmake_ciao stateng
}
register_command "compiler"
do__compiler() {
    gmake_ciao compiler
}
register_command "applications"
do__applications() {
    gmake_ciao applications
    lpmake applicationsextra
}

register_command "libraries"
do__libraries() {
    do__librariesciao
    do__librariesextra
}
register_command "all_ciao_gmake"
do__all_ciao_gmake() {
    # TODO: avoid gmake
    gmake_ciao all
}
register_command "librariesciao"
do__librariesciao() {
    gmake_ciao libraries chr
}
register_command "platdep"
do__platdep() {
    do__engine
    gmake_ciao platdep
    lpmake platdep
#    do__stateng
}
register_command "crossengine"
do__crossengine() {
    gmake_ciao crossengine
}
register_command "librariesextra"
do__librariesextra() {
    lpmake librariesextra
}
register_command "allnolibs"
do__allnolibs() {
    do__allnolibsciao
    do__allnolibsextra
}
register_command "allnolibsciao"
do__allnolibsciao() {
    do__engine
    gmake_ciao allnolibs
#    do__stateng
}
register_command "allnolibsextra"
do__allnolibsextra() {
    lpmake allnolibsextra
}
register_command "clean_eng"
do__clean_eng() {
    gmake_ciao engclean
}

register_command "clean_etc"
do__clean_etc() {
    rm -rf \
	${CIAODESRC}/ciao/etc/DOTprofile \
	${CIAODESRC}/ciao/etc/DOTcshrc
}

register_command "clean_bin"
do__clean_bin() {
    rm -rf ${CIAODESRC}/build/bin \
	${CIAODESRC}/ciao/lib/compiler/header \
	${CIAODESRC}/ciao/lib/compiler/bat_skel \
        ${CIAODESRC}/ciao/Win32/setup_bat.bat
}

register_command "clean_lib"
do__clean_lib() {
    find ${CIAODESRC}/. -name ".svn" -prune -o \( \
	-name "*.po" \
	-o -name "*.itf" \
	-o -name "*.wam" \
	-o -name "*.dep" \
	-o -name "*.asr" \
	-o -name "*.ast" \
	-o -name "*.ass" \
	-o -name "*.o" \
	-o -name "*_glue.c" \
	-o -name "*_inline.c" \
	\
	-o -name "*.aux" \
	-o -name "*.log" \
	-o -name "*.err" \
	\
	-o -name "*_co.pl" \
	-o -name "*_co.java" \) \
	-exec /bin/rm -f {} \;
}

register_command "rpdclean"
do__rpdclean() {
    for i in `cat ciao/makedir/platdep_modules.pl|gawk -F ',' \
	'{print substr($3, 2, length($3)-7)}'`
    do
	/bin/rm -f "${i}.asr" "${i}.ast" "${i}.itf" "${i}.po"  \
	    "${i}_${CIAOARCH}.so" "${i}_${CIAOARCH}.dll"       \
	    "${i}_${CIAOARCH}.dylib" "${i}_${CIAOARCH}_glue.c" \
	    "${i}_${CIAOARCH}_inline.c"
    done
}

register_command "clean_nodistribute"
do__clean_nodistribute() {
    find ${CIAODESRC}/. -name 'NODISTRIBUTE' -exec ${CIAOSRC}/etc/remove_dirname {} \;
}
