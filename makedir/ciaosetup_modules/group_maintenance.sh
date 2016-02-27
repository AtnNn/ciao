# Group of commands for custom maintenance operations

command_group "Maintenance"
command_help <<EOF
  clean        Clean CiaoDE auto-generated files, excluding documentation
               and additional files such as logs, temporary files, etc.

  distclean    Clean CiaoDE auto-generated files, excluding documentation.

  braveclean   Clean CiaoDE auto-generated files, including documentation,
               but keep configuration choices.

  realclean    Clean all CiaoDE  auto-generated files, and also 
               configuration choices. 

  bootclean    Remove binary files.
EOF
command_help_exp <<EOF
  clean_recursive [not documented]
  clean_dir_recursive [not documented]
  clean_java [not documented]
  packageclean [not documented]
  ilciaoclean [not documented]
  clean_platdep [not documented]
  clean_log [not documented]
  clean_build [not documented]
EOF
register_command "clean"
do__clean() {
    do__bootclean
    do__clean_recursive
    do__clean_eng
    do__clean_emacs
    do__clean_java
    do__clean_miniprolog
    do__packageclean
    do__ilciaoclean
    do__clean_build
}
register_command "distclean"
do__distclean() {
    do__clean_config
    do__clean
    do__clean_installeddocs
    do__clean_docstmp
}
register_command "braveclean"
do__braveclean() {
    do__clean
    do__clean_docs
}
register_command "realclean"
do__realclean() {
    do__clean_config
    do__braveclean
}
register_command "bootclean"
do__bootclean() {
    do__clean_bin
    do__clean_chr
    do__clean_etc
}
# Be careful with clean_recursive, it has been optimized and
# must take less than 10 seconds !!!
#
# We can not use -print0 | xargs -0 because -print0 is not compatible
# with Solaris, nor -print | xargs because it is buggy

register_command "clean_dir_recursive"

do__clean_dir_recursive() {
    if [ x$1 = x ] ; then
	DIR=`pwd`
    else
	DIR=$1
    fi
    find ${DIR}/. -name ".svn" -prune -o \( \
	-name "*.po" \
	-o -name "*.itf" \
	-o -name "*.cpx" \
	-o -name "*.wam" \
	-o -name "*.dep" \
	-o -name "*.asr" \
	-o -name "*.ast" \
	-o -name "*.ass" \
	-o -name "*.o" \
	-o -name "*.so" \
	-o -name "*.dll" \
	-o -name "*.dylib" \
	-o -name "*_glue.c" \
	-o -name "*_inline.c" \
	-o -name "*_auto.pl" \
	-o -name "auto" -prune \
	-o -name "*.ascii" \
	-o -name "*.class" \
	\
	-o -name "*.aux" \
	-o -name "*.log" \
	-o -name "*.err" \
	-o -name "tmpciao*" \
	\
	-o -name "*_co.pl" \
	-o -name "*_co.java" \
	-o -name "*.iss" \
	\
	-o -name "*~" \
	-o -name "#*" \) \
	-exec /bin/rm -r -f {} \;
}

register_command "clean_recursive"
do__clean_recursive() {
    do__clean_dir_recursive ${CIAODESRC}
}

register_command "packageclean"
do__packageclean() {
    rm -rf ${CIAODESRC}/package
}
register_command "ilciaoclean"
do__ilciaoclean() {
    rm -rf ${CIAODESRC}/ciaopp/ilciao/examples/Resources/examples.*.pl \
	${CIAODESRC}/ciaopp/ilciao/classes
}
register_command "clean_build"
do__clean_build() {
    rm -rf ${CIAODESRC}/build
}
register_command "clean_platdep"
do__clean_platdep() {
    do__clean_eng
    do__clean_bin
    do__rpdclean ciao
}
register_command "clean_log"
do__clean_log() {
    rm ${CIAODESRC}/install.log
}

