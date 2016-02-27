#!/bin/sh
#
# Generation of setting options for the engine compilation and
# internal script installation commands (that are necessarily not
# encodable in Prolog, since that would not allow us to bootstrap the
# system).
#
# Jose F. Morales (based on previous code)
# ----------------------------------------------------------------------------

# Include the file specified as the first argument (contains
# definitions)
. ${1}

# Define this to be the main file name (with suffix if there is one).
MAIN="ciao"
CIAOSRC="${CIAODESRC}/${MAIN}"
SRC="${CIAOSRC}"

# ===========================================================================
SHELL="/bin/sh"
SYSDEP_FILES="${CIAOSRC}/config-sysdep"
CIAOARCH="`${CIAOSRC}/etc/ciao_get_arch`"

if test x"${CIAOARCH}" = x""; then
    echo "No CIAOARCH" >& 2
    exit 1
fi

# Number of processors: use the real number
PROCESSORS="`${SYSDEP_FILES}/available_processors ${CIAOARCH}`"

ETAGS="etags --ignore-indentation"

# TODO: Used?
ARCHOS="${CIAOARCH}"

if test x"${DEBUG_LEVEL}" != x"nodebug"; then
    CIAODEBUG="-${DEBUG_LEVEL}"
fi

if test x"${AND_PARALLEL_EXECUTION}" = x"yes"; then
    ANDPARALLEL="-DANDPARALLEL"
elif test x"${AND_PARALLEL_EXECUTION}" = x"visandor"; then
    ANDPARALLEL="-DANDPARALLEL -DVISANDOR"
elif test x"${AND_PARALLEL_EXECUTION}" = x"no"; then
    ANDPARALLEL=""
fi

if test x"${PAR_BACK}" = x"yes"; then
    PARBACK="-DPARBACK"
fi

if test x"${TABLED_EXECUTION}" = x"yes"; then
    TABLING="-DTABLING"
fi

# General optimization flags; can be overriden in specific architectures
if test x"${CIAODEBUG}" = x"-debug"; then
    OPTIM_LEVEL="normal"
    if test x"${DEBUG_LEVEL}" = x"nodebug"; then
	DEBUG_LEVEL="debug"
    fi
fi

if test x"${OPTIM_LEVEL}" = x"optimized"; then
    OPTIM_FLAGS="-O2"
elif test x"${OPTIM_LEVEL}" = x"normal"; then
    OPTIM_FLAGS=""
fi

case "${DEBUG_LEVEL}" in
    paranoid-debug)
	DEBUG_FLAGS="-g -Wall -W -Wtraditional -Wshadow -Wpointer-arith -Wbad-function-cast -Wcast-qual -Wcast-align -Wconversion -Waggregate-return -Wstrict-prototypes -Wmissing-prototypes -Wmissing-declarations -Wredundant-decls -Wnested-externs -Winline -DDEBUG"
	;;
    debug)
	DEBUG_FLAGS="-g -Wall -DDEBUG -DGLOBVARS -DINTERNAL_CALLING"
	;;
    nodebug)
	DEBUG_FLAGS=""
	;;
    profile)
	DEBUG_FLAGS="-DDEBUG -DPROFILE"
#    DEBUG_FLAGS="-DPROFILE"
	;;
    profile-debug)
	DEBUG_FLAGS="-g -Wall -DDEBUG -DGLOBVARS -DINTERNAL_CALLING -DPROFILE"
	;;
esac

#INSTALLIBARCHDIR=${INSTALLIBDIR}/${CIAOARCH}${CIAODEBUG}

# ===========================================================================
# Do the actual configuration
# TODO: include this file here? merge with previous code?

. ${SYSDEP_FILES}/config-sysdep.sh

CFLAGS="${OPTIM_FLAGS} ${DEBUG_FLAGS} ${THREAD_FLAG} ${FOREIGN_FILES_FLAG} ${ANDPARALLEL} ${VISANDOR} ${PARBACK} ${TABLING} -D${OSNAME} -D${ARCHNAME} ${ARCHFLAGS} ${MEM_MNG_FLAG} ${EXTRA_CFLAGS}"

LDFLAGS="${LINK_FLAGS} ${EXTRA_LDFLAGS}"

# ===========================================================================

# in ciao/ciaoc/Makefile, ciao/Makefile, ciao/shell/Makefile
VERSION="`cat ${CIAOSRC}/version/GlobalVersion`"
PATCH="`cat ${CIAOSRC}/version/GlobalPatch`"

# local
CIAOBUILDDIR="${CIAODESRC}/build"

# ciao/ciaoc/Makefile ciao/engine/Makefile ciao/shell/Makefile ciao/Makefile
SRCBINDIR="${CIAOBUILDDIR}/bin"

CIAOHDIR="${CIAOHDIRROOT}/${CIAOARCH}${CIAODEBUG}"

case "`uname`" in
    *CYGWIN*)
	ALT="win32"
	EXECSUFFIX=".exe"
	;;
esac

case "${OSTYPE}" in
    cygwin)
	ALT="win32"
	EXECSUFFIX=".exe"
	;;
    cygwin32)
	ALT="win32"
	EXECSUFFIX=".exe"
	;;
esac

# The next is a kludge to use the .cpx extension in Windows
if test x"${ALT}" = x"win32"; then
    CIAOSUFFIX=".cpx"
fi

# ciao/Makefile ciao/ciaoc/Makefile ciao/shell/Makefile
CIAOC="${SRCBINDIR}/ciaoc-${VERSION}${CIAOSUFFIX}"

# ciao/Makefile ciao/ciaoc/Makefile
# local
DISTSTATICCOMP="ciaoc.sta"
# local
BOOTSTRAPDIR="${CIAOSRC}/bootstrap"
BOOTSTRAP_CIAOC="${BOOTSTRAPDIR}/${DISTSTATICCOMP}"

# Default ciaoc options (runtime test enabled)
# TODO: this code should be in Prolog
CIAOCOPTS=""
add_ciaoc_opt() {
    CIAOCOPTS="${CIAOCOPTS} ${1}"
}
test x"${RUNTIME_CHECKS}" = x"yes" && add_ciaoc_opt "-rc"
test x"${RTCHECKS_TRUST}" = x"no" && add_ciaoc_opt "--rtchecks-trust-no"
test x"${RTCHECKS_ENTRY}" = x"no" && add_ciaoc_opt "--rtchecks-entry-no"
test x"${RTCHECKS_EXIT}" = x"no" && add_ciaoc_opt "--rtchecks-exit-no"
test x"${RTCHECKS_TEST}" = x"yes" && add_ciaoc_opt "--rtchecks-test"
test x"${RTCHECKS_LEVEL}" = x"exports" && add_ciaoc_opt "--rtchecks-level-exports"
test x"${RTCHECKS_INLINE}" = x"yes" && add_ciaoc_opt "--rtchecks-inline"
test x"${RTCHECKS_ASRLOC}" = x"no" && add_ciaoc_opt "--rtchecks-asrloc-no"
test x"${RTCHECKS_PREDLOC}" = x"no" && add_ciaoc_opt "--rtchecks-predloc-no"
test x"${RTCHECKS_NAMEFMT}" = x"short" && add_ciaoc_opt "--rtchecks-namefmt-short"
test x"${RTCHECKS_CALLLOC}" = x"no" && add_ciaoc_opt "--rtchecks-callloc-no"
test x"${RTCHECKS_CALLLOC}" = x"literal" && add_ciaoc_opt "--rtchecks-callloc-literal"
test x"${UNUSED_PRED_WARNINGS}" = x"yes" && add_ciaoc_opt "--unused-pred-warnings"

# Default ciaoc options (no runtime test enabled)
CIAOCOPTS_NORTCHECKS=""
add_ciaoc_nort_opt() {
    CIAOCOPTS_NORTCHECKS="${CIAOCOPTS_NORTCHECKS} ${1}"
}
test x"${UNUSED_PRED_WARNINGS}" = x"yes" && add_ciaoc_nort_opt "--unused-pred-warnings"

# ===========================================================================

OBJDIRROOT="${CIAOBUILDDIR}/objs"
OBJDIR="${OBJDIRROOT}/${CIAOARCH}${CIAODEBUG}"

# Relative path from ${OBJDIR} to the ${CIAOSRC}/engine directory
ENG_FROM_OBJDIR="../../../ciao/engine"

# ciao/Makefile ciao/engine/Makefile
SRCINCLUDEDIRROOT="${CIAOBUILDDIR}/include"
SRCINCLUDEDIR="${SRCINCLUDEDIRROOT}/${CIAOARCH}${CIAODEBUG}"

ENGINEBASE="ciaoengine"
ENGINENAME="${ENGINEBASE}${EXECSUFFIX}" # may be patched by config-sysdep.sh

# Custom options for cross Win32
# TODO: move seamlessly into config-sysdep.sh? (by setting the engine extension and adding cc flags in the right variable)
case ${CIAOARCH} in
    crossWin32i86)
       # TODO: why?
       ENGINENAME="${ENGINENAME}.exe"
       # TODO: why?
       # Standard libraries
       CFLAGS="${CFLAGS} -DSTATICENG -DcrossWin32i86 -I/usr/i386-mingw32/include"
       ;;
    *) true
esac    

# TODO: remove from here
SETLOCALCIAO="CIAOALIASPATH=\"\" CIAOLIB=${CIAOSRC} CIAOHDIR=${SRCINCLUDEDIR} CIAOENGINE=${OBJDIR}/${ENGINENAME}"
SETBOOTSTRAPVARS="BOOTSTRAP_CIAOC=${BOOTSTRAP_CIAOC} CIAOC=${CIAOC} CIAOCOPTS=\"${CIAOCOPTS}\" CIAOCOPTS_NORTCHECKS=\"${CIAOCOPTS_NORTCHECKS}\""

# ===========================================================================

# Export settings as Makefile/shell definitions

# The first argument defines the string quoting character for the
# specified output kind. That ensures that values consisting on
# several words are correctly assigned (i.e., V="foo bar" or V=foo
# bar).
emit_CONFIG() { # ${1} is the quotation char
    Q="${1}"
    # TODO: Sure? Configuration of GSL seems to be done too late...
    # TODO: Generalize for other libraries
    CONFIG_GSL_FILE="${CIAOBUILDDIR}/CONFIG_GSL"
    if test -r "${CONFIG_GSL_FILE}"; then
	. "${CONFIG_GSL_FILE}"
	STAT_LIBS="${STAT_LIBS} ${GSL_STAT_LIBS}"
    fi
    echo
    # TODO: Refine the options (not all of them are necessary...
    #       Makefile files should be extinguished.
    cat <<EOF
# (previously named SETTINGS)
CIAODESRC=${Q}${CIAODESRC}${Q}
MAIN=${Q}${MAIN}${Q}
INTERACTIVE_CONFIG=${Q}${INTERACTIVE_CONFIG}${Q}
SILENT=${Q}${SILENT}${Q}
STOP_IF_ERROR=${Q}${STOP_IF_ERROR}${Q}
REGISTRATION_TYPE=${Q}${REGISTRATION_TYPE}${Q}
INSTYPE=${Q}${INSTYPE}${Q}
PREFIX=${Q}${PREFIX}${Q}
BINDIR=${Q}${BINDIR}${Q}
LIBROOT=${Q}${LIBROOT}${Q}
LIBDIR=${Q}${LIBDIR}${Q}
REALLIBDIR=${Q}${REALLIBDIR}${Q}
INCLUDEROOT=${Q}${INCLUDEROOT}${Q}
CIAOHDIRROOT=${Q}${CIAOHDIRROOT}${Q}
ENGINEDIR=${Q}${ENGINEDIR}${Q}
EXECMODE=${Q}${EXECMODE}${Q}
DATAMODE=${Q}${DATAMODE}${Q}
INSTALLGROUP=${Q}${INSTALLGROUP}${Q}
UPDATE_BASHRC=${Q}${UPDATE_BASHRC}${Q}
DOTBASHRC=${Q}${DOTBASHRC}${Q}
UPDATE_CSHRC=${Q}${UPDATE_CSHRC}${Q}
DOTCSHRC=${Q}${DOTCSHRC}${Q}
INSTALL_PROLOG_NAME=${Q}${INSTALL_PROLOG_NAME}${Q}
INSTALL_EMACS_SUPPORT=${Q}${INSTALL_EMACS_SUPPORT}${Q}
INSTALL_XEMACS_SUPPORT=${Q}${INSTALL_XEMACS_SUPPORT}${Q}
EMACS_FOR_CIAO=${Q}${EMACS_FOR_CIAO}${Q}
UPDATE_DOTEMACS=${Q}${UPDATE_DOTEMACS}${Q}
DOTEMACS=${Q}${DOTEMACS}${Q}
EMACSINITDIR=${Q}${EMACSINITDIR}${Q}
EMACSINITFILE=${Q}${EMACSINITFILE}${Q}
HTMLDIR=${Q}${HTMLDIR}${Q}
HTMLURL=${Q}${HTMLURL}${Q}
DOCDIR=${Q}${DOCDIR}${Q}
MANDIR=${Q}${MANDIR}${Q}
INFODIR=${Q}${INFODIR}${Q}
WEB_IMAGES_PATH=${Q}${WEB_IMAGES_PATH}${Q}
WEB_IMAGES_URL=${Q}${WEB_IMAGES_URL}${Q}
WITH_MYSQL=${Q}${WITH_MYSQL}${Q}
WITH_GSL=${Q}${WITH_GSL}${Q}
WITH_PPL=${Q}${WITH_PPL}${Q}
WITH_JAVA_INTERFACE=${Q}${WITH_JAVA_INTERFACE}${Q}
WITH_ANT=${Q}${WITH_ANT}${Q}
CUSTOM_CC=${Q}${CUSTOM_CC}${Q}
CUSTOM_LD=${Q}${CUSTOM_LD}${Q}
EXTRA_CFLAGS=${Q}${EXTRA_CFLAGS}${Q}
EXTRA_LDFLAGS=${Q}${EXTRA_LDFLAGS}${Q}
OPTIMIZING_COMPILER=${Q}${OPTIMIZING_COMPILER}${Q}
USE_THREADS=${Q}${USE_THREADS}${Q}
USE_POSIX_LOCKS=${Q}${USE_POSIX_LOCKS}${Q}
AND_PARALLEL_EXECUTION=${Q}${AND_PARALLEL_EXECUTION}${Q}
PAR_BACK=${Q}${PAR_BACK}${Q}
TABLED_EXECUTION=${Q}${TABLED_EXECUTION}${Q}
OPTIM_LEVEL=${Q}${OPTIM_LEVEL}${Q}
CROSS_COMPILER_HOST=${Q}${CROSS_COMPILER_HOST}${Q}
DEBUG_LEVEL=${Q}${DEBUG_LEVEL}${Q}
#WITH_CHR=${Q}${WITH_CHR}${Q}
WITH_CIAOPPCL=${Q}${WITH_CIAOPPCL}${Q}
GEN_CIAO_ASR=${Q}${GEN_CIAO_ASR}${Q}
GEN_CIAOPP_ASR=${Q}${GEN_CIAOPP_ASR}${Q}
CASE_INSENSITIVE=${Q}${CASE_INSENSITIVE}${Q}
UNUSED_PRED_WARNINGS=${Q}${UNUSED_PRED_WARNINGS}${Q}
SINGLE_VAR_WARNINGS=${Q}${SINGLE_VAR_WARNINGS}${Q}
DISCONTIGUOUS_WARNINGS=${Q}${DISCONTIGUOUS_WARNINGS}${Q}
MULTI_ARITY_WARNINGS=${Q}${MULTI_ARITY_WARNINGS}${Q}
VERBOSE_COMPILATION=${Q}${VERBOSE_COMPILATION}${Q}
ITF_FORMAT=${Q}${ITF_FORMAT}${Q}
COMPRESS_LIB=${Q}${COMPRESS_LIB}${Q}
READ_ASSERTIONS=${Q}${READ_ASSERTIONS}${Q}
USE_COMPILE_PACKAGES=${Q}${USE_COMPILE_PACKAGES}${Q}
KEEP_ASSERTIONS=${Q}${KEEP_ASSERTIONS}${Q}
CHARACTER_ESCAPES=${Q}${CHARACTER_ESCAPES}${Q}
DOCCOMMENTS=${Q}${DOCCOMMENTS}${Q}
READ_HIORD=${Q}${READ_HIORD}${Q}
READ_CURLY_BLOCKS=${Q}${READ_CURLY_BLOCKS}${Q}
READ_POSTFIX_BLOCKS=${Q}${READ_POSTFIX_BLOCKS}${Q}
WRITE_STRINGS=${Q}${WRITE_STRINGS}${Q}
CHECK_CYCLES=${Q}${CHECK_CYCLES}${Q}
PATH_STYLE=${Q}${PATH_STYLE}${Q}
EXECUTABLES=${Q}${EXECUTABLES}${Q}
CHECK_LIBRARIES=${Q}${CHECK_LIBRARIES}${Q}
SELF_CONTAINED=${Q}${SELF_CONTAINED}${Q}
COMPRESS_EXEC=${Q}${COMPRESS_EXEC}${Q}
RUNTIME_CHECKS=${Q}${RUNTIME_CHECKS}${Q}
RTCHECKS_LEVEL=${Q}${RTCHECKS_LEVEL}${Q}
RTCHECKS_TRUST=${Q}${RTCHECKS_TRUST}${Q}
RTCHECKS_ENTRY=${Q}${RTCHECKS_ENTRY}${Q}
RTCHECKS_EXIT=${Q}${RTCHECKS_EXIT}${Q}
RTCHECKS_TEST=${Q}${RTCHECKS_TEST}${Q}
RTCHECKS_INLINE=${Q}${RTCHECKS_INLINE}${Q}
RTCHECKS_ASRLOC=${Q}${RTCHECKS_ASRLOC}${Q}
RTCHECKS_PREDLOC=${Q}${RTCHECKS_PREDLOC}${Q}
RTCHECKS_CALLLOC=${Q}${RTCHECKS_CALLLOC}${Q}
RTCHECKS_NAMEFMT=${Q}${RTCHECKS_NAMEFMT}${Q}
SET_FLAG_OPTIONS=${Q}${SET_FLAG_OPTIONS}${Q}
CIAOSH_COMMANDS=${Q}${CIAOSH_COMMANDS}${Q}
# (previously named SHARED)
# Define this to be the main file name (with suffix if there is one).
MAIN=${Q}${MAIN}${Q}
CIAOSRC=${Q}${CIAOSRC}${Q}
SRC=${Q}${SRC}${Q}
# in ciao/ciaoc/Makefile, ciao/Makefile, ciao/shell/Makefile
VERSION=${Q}${VERSION}${Q}
PATCH=${Q}${PATCH}${Q}
# local
CIAOBUILDDIR=${Q}${CIAOBUILDDIR}${Q}
# ciao/ciaoc/Makefile ciao/engine/Makefile ciao/shell/Makefile ciao/Makefile
SRCBINDIR=${Q}${SRCBINDIR}${Q}
CIAOHDIR=${Q}${CIAOHDIR}${Q}
ALT=${Q}${ALT}${Q}
EXECSUFFIX=${Q}${EXECSUFFIX}${Q}
# The next is a kludge to use the .cpx extension in Windows
CIAOSUFFIX=${Q}${CIAOSUFFIX}${Q}
# ciao/Makefile ciao/ciaoc/Makefile ciao/shell/Makefile
CIAOC=${Q}${CIAOC}${Q}
# ciao/Makefile ciao/ciaoc/Makefile
# local
DISTSTATICCOMP=${Q}${DISTSTATICCOMP}${Q}
# local
BOOTSTRAPDIR=${Q}${BOOTSTRAPDIR}${Q}
BOOTSTRAP_CIAOC=${Q}${BOOTSTRAP_CIAOC}${Q}
CIAOCOPTS_NORTCHECKS=${Q}${CIAOCOPTS_NORTCHECKS}${Q}
CIAOCOPTS=${Q}${CIAOCOPTS}${Q}
# ===========================================================================
SHELL=${Q}${SHELL}${Q}
SYSDEP_FILES=${Q}${SYSDEP_FILES}${Q}
CIAOARCH=${Q}${CIAOARCH}${Q}
# Number of processors: use the real number
PROCESSORS=${Q}${PROCESSORS}${Q}
ETAGS=${Q}${ETAGS}${Q}
# TODO: used?
ARCHOS=${Q}${ARCHOS}${Q}
CIAODEBUG=${Q}${CIAODEBUG}${Q}
ANDPARALLEL=${Q}${ANDPARALLEL}${Q}
PARBACK=${Q}${PARBACK}${Q}
TABLING=${Q}${TABLING}${Q}
OPTIM_LEVEL=${Q}${OPTIM_LEVEL}${Q}
DEBUG_LEVEL=${Q}${DEBUG_LEVEL}${Q}
OPTIM_FLAGS=${Q}${OPTIM_FLAGS}${Q}
DEBUG_FLAGS=${Q}${DEBUG_FLAGS}${Q}
CFLAGS=${Q}${CFLAGS}${Q}
LDFLAGS=${Q}${LDFLAGS}${Q}
# ===========================================================================
# From config-sysdep.sh
# TODO: refine
OSNAME=${Q}${OSNAME}${Q}
ARCHNAME=${Q}${ARCHNAME}${Q}
CC=${Q}${CC}${Q}
LD=${Q}${LD}${Q}
FOREIGN_FILES_FLAG=${Q}${FOREIGN_FILES_FLAG}${Q}
CCSHARED=${Q}${CCSHARED}${Q}
LDSHARED=${Q}${LDSHARED}${Q}
SOSUFFIX=${Q}${SOSUFFIX}${Q}
STAT_LIBS=${Q}${STAT_LIBS}${Q}
LCC=${Q}${LCC}${Q}
LLD=${Q}${LLD}${Q}
INSTALL=${Q}${INSTALL}${Q}
ENGINE_LINK_TYPE=${Q}${ENGINE_LINK_TYPE}${Q}
LIBS=${Q}${LIBS}${Q}
ENGLOCATION=${Q}${ENGLOCATION}${Q}
LDRPATH=${Q}${LDRPATH}${Q}
NOCONSOLEFLAG=${Q}${NOCONSOLEFLAG}${Q}
SDBEGIN=${Q}${SDBEGIN}${Q}
OSREV=${Q}${OSREV}${Q}
# ===========================================================================
OBJDIRROOT=${Q}${OBJDIRROOT}${Q}
OBJDIR=${Q}${OBJDIR}${Q}
# Relative path from ${OBJDIR} to the ${CIAOSRC}/engine directory
ENG_FROM_OBJDIR=${Q}${ENG_FROM_OBJDIR}${Q}
# ciao/Makefile ciao/engine/Makefile
SRCINCLUDEDIRROOT=${Q}${SRCINCLUDEDIRROOT}${Q}
SRCINCLUDEDIR=${Q}${SRCINCLUDEDIR}${Q}
ENGINEBASE=${Q}${ENGINEBASE}${Q}
ENGINENAME=${Q}${ENGINENAME}${Q}
# ciao/shell/Makefile, ciao/Makefile ciao/ciaoc/Makefile
SETLOCALCIAO=${Q}${SETLOCALCIAO}${Q}
SETBOOTSTRAPVARS=${Q}${SETBOOTSTRAPVARS}${Q}
EOF
}

# Emit compile_options_auto, for foreign compilation
# TODO: simplify this code
emit_compile_options_auto() {
    if test x"${CCSHARED}" != x""; then
        CCFORCOMP="['${CCSHARED} ${EXTRA_CFLAGS}']"
    elif test x"${EXTRA_CFLAGS}" != x""; then
        CCFORCOMP="['${EXTRA_CFLAGS}']"
    else
        CCFORCOMP="[]"
    fi

    if test x"${LDSHARED}" != x""; then
        LDFORCOMP="['${LDSHARED} ${EXTRA_LDFLAGS}']"
    elif test x"${EXTRA_LDFLAGS}" != x""; then
        CCFORCOMP="['${EXTRA_LDFLAGS}']"
    else
        LDFORCOMP="[]"
    fi

    # TODO: This is generated in the lib/ directory. I would like to
    # generate it in the build directory instead, but it would be
    # useless now since there is no alias path for that directory, and
    # that file is included from Prolog.
    OPTFILE="${CIAOSRC}/lib/compile_options_auto.pl"
    cat > ${OPTFILE} <<EOF
%%% DO NOT EDIT THIS FILE!
%%% IT HAS BEEN AUTOMATICALLY GENERATED DURING COMPILATION!
foreign_compiler_options('${OSNAME}','${ARCHNAME}','${CC}',${CCFORCOMP}).
foreign_linker_options('${OSNAME}','${ARCHNAME}','${LD}',${LDFORCOMP}).
EOF
    chmod ${DATAMODE} ${OPTFILE}
}

# ===========================================================================

# Export config for 'sh' scripts
emit_CONFIG "'" > ${CIAOBUILDDIR}/CONFIG_sh
# Export config for Makefiles
emit_CONFIG "" > ${CIAOBUILDDIR}/CONFIG_mkf
# Export config (just compile options) for Prolog (the foreign interface)
emit_compile_options_auto

