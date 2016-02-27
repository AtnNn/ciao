#!/bin/sh
#
# Engine build script
# ---------------------------------------------------------------------------

# Follow symbolic links
self_exec=`test -L "$0" && readlink "$0" || echo "$0"`
# Obtain the directory where this script is located
old_dir=`pwd`; cd `dirname ${self_exec}`; self=`pwd`; cd ${old_dir}; old_dir=

# Exit immediately if a simple command exits with a non-zero status
set -e 

# ===========================================================================

. ${self}/../../makedir/terminal_io.sh

if test x"${CIAOBUILDDIR}" = x""; then
    echo "error: CIAOBUILDDIR is undefined" >&2
    echo "Please, do not call this script directly. Use 'ciaosetup' instead." >&2
    exit 1
fi

not_configured() {
    [ ! -f ${CIAOBUILDDIR}/CONFIG_sh ]
}

ensure_config_loaded() {
    if [ x"${SETLOCALCIAO}" != x"" ]; then
	# Exit, bootstrap settings already loaded
	return
    fi
    if not_configured; then
	echo "bug: no configuration found" >&2
        exit 1
    fi
    # TODO: make sure that it is generated (or exit graciously)
    # TODO: during a installation there exist two different configurations here..
    #       one for the bootstrap system and other for the final one
    . ${CIAOBUILDDIR}/CONFIG_sh
}

# ===========================================================================

# TODO: Executables build in this way cannot be moved to machines
#       where Ciao has been installed in other place, unless the CIAO*
#       environment variables are set. Can a nicer solution be found?
do_exec_header() {
    INSTALLED_ENGINE_PATH_TO_CHANGE=installed_engine_path
    HEADER_SKEL=${CIAOSRC}/lib/compiler/header.skel
    HEADER=${CIAOSRC}/lib/compiler/header

    # Override shell clobbering
    /bin/rm -f ${HEADER} # TODO: why?
    sed -e 's:'"${INSTALLED_ENGINE_PATH_TO_CHANGE}"':'"${ENGINEDIR}/${ENGINEBASE}"':g' \
        < ${HEADER_SKEL} > ${HEADER}
    chmod ${DATAMODE} ${HEADER}
}

# ===========================================================================
# Engine build/bootstrap

# Is there a compiled engine?
engine_is_ok() {
    if not_configured; then
	return 1
    else
	ensure_config_loaded
	[ -f ${OBJDIR}/${ENGINENAME} ]
    fi
}

# (called from 'ciaosetup')
do_build_engine() {
    ensure_config_loaded
    # TODO: do_exec_header is for the compiler, not for the engine
    do_exec_header # dowindowsbat
    #
    do_engine_srcdir_and_links
    do_build_engine__2
    do_engine_links
}

# TODO: This command is not symmetric to build_engine! why?
# (called from 'ciaosetup')
do_build_static_engine() {
    ensure_config_loaded
    # TODO: Static engine seem to be required for some profiler code
    ENGINE_LINK_TYPE=dostat do_build_engine__2
}

# These links are needed to run ciao from the sources
# Note: The links are a bit trickly because this should work with the ciao header.
do_engine_links() {
    OLDPWD=`pwd`
    cd ${OBJDIRROOT}
    rm -f ${ENGINEBASE}.${CIAOARCH}${EXECSUFFIX}
    ln -s ${CIAOARCH}${CIAODEBUG}/${ENGINEBASE} ${ENGINEBASE}.${CIAOARCH}${EXECSUFFIX}
    rm -f ${ENGINEBASE}.lnk ${ENGINEBASE}
    ln -s ${ENGINEBASE}.${CIAOARCH}${EXECSUFFIX} ${ENGINEBASE}
    cd ${OLDPWD}
}

ENGINEBASESTAT="${ENGINEBASE}.sta"
ENGINENAMESTAT="${ENGINEBASESTAT}${EXECSUFFIX}"

MFLAGS="-j${PROCESSORS}"

GMAKEOPTS="--no-print-directory"
GMAKE="${MAKE} ${GMAKEOPTS}"
#GMAKE=${MAKE}

do_gmake() {
    ${GMAKE} $*
}

do_build_engine__2() {
    OLDPWD=`pwd`
    cd ${CIAODESRC}/ciao
    gen_engine_build_info
    umask 002
    case "${ENGINE_LINK_TYPE}" in
	crossdyn) 
	    bold_message "Building dynamic engine for ${OSNAME}${ARCHNAME} by cross-compilation"
	    do_gmake -C ${OBJDIR} crossconfigure.h
	    do_gmake_currlibs -C ${OBJDIR} ${MFLAGS} ${ENGINENAME} LCC="${LCC}"
	    ;;
	dyn)
	    bold_message "Building dynamic engine for ${OSNAME}${ARCHNAME}"
	    do_gmake -C ${OBJDIR} configure.h
	    do_gmake_currlibs -C ${OBJDIR} ${MFLAGS} ${ENGINENAME}
	    ;;
	dostat)
	    bold_message "Building static engine for ${OSNAME}${ARCHNAME}"
	    # Nasty trick: rename the old engine
	    if [ -f ${OBJDIR}/${ENGINENAME} ] ; then
		mv ${OBJDIR}/${ENGINENAME} ${OBJDIR}/${ENGINENAME}.tmp ;
	    fi
	    do_gmake -C ${OBJDIR} configure.h
	    do_gmake_currlibs_stat -C ${OBJDIR} ${MFLAGS} ${ENGINENAME}
	    # Nasty trick: rename the engine to ENGINENAMESTAT
	    mv ${OBJDIR}/${ENGINENAME} ${OBJDIR}/${ENGINENAMESTAT}
	    chmod ${EXECMODE} ${OBJDIR}/${ENGINENAMESTAT}
	    # Nasty trick: move saved engine to the original place
	    mv ${OBJDIR}/${ENGINENAME}.tmp ${OBJDIR}/${ENGINENAME}
	    ;;
    esac
    cd ${OLDPWD}
}

#[REMOVED]
# # TODO: Necessary for Windows?
# do_build_libciao() {
#     bold_message "Building 'libciao'"
#     OLDPWD=`pwd`
#     cd ${OBJDIR}
#     do_gmake_currlibs libciao
#     do_gmake_currlibs ciaoobject
#     cd ${OLDPWD}
# }

# TODO: for build_static_engine... useful?
# In Windows we need an executable built without console support 
# ifeq (${OSNAME},Win32)
# 	cd ${OBJDIR} &&	do_gmake clean
# 	do_gmake ${MFLAGS} CONSOLEFLAG="${NOCONSOLEFLAG}" dostateng
# 	/bin/mv ${OBJDIR}/${ENGINAME}.sta ${OBJDIR}/${ENGINAME}_nc.sta
# 	cd ${OBJDIR} &&	do_gmake clean
# endif

do_gmake_currlibs() {
    # make with CURRLIBS set to LIBS
    # TODO: this could be inside the makefile itself...
    ${GMAKE} $* CURRLIBS="${LIBS}"
}

do_gmake_currlibs_stat() {
    # make with CURRLIBS set to LIBS
    # TODO: this could be inside the makefile itself...
    ${GMAKE} $* CURRLIBS="${LIBS} ${STAT_LIBS}" ADDOBJ="`statobj`"
}

# TODO: This implementation is incorrect.
#       Not all C libraries should be part of the static engine.
#         random.o sockets_c.o fastrw.o, ...
statobj() {
    find ${CIAOSRC} -name "*${CIAOARCH}.o" -o -name "*${CIAOARCH}_glue.o" \
    | tr '\n' ' '
}

do_engine_srcdir_and_links() {
    USE_SYMLINKS=yes
    #
    # TODO: USE_SYMLINKS=no could be useful for platforms or
    #       compilation targets where symbolic links are not
    #       appropiate.
    #
    if test -d ${OBJDIR} -a x"${USE_SYMLINKS}" = x"yes"; then
	return
    fi
#    normal_message "Creating engine source directory"
    create_engdir ${OBJDIRROOT} ${OBJDIR}
    if test x"${USE_SYMLINKS}" = x"yes"; then
	OLDPWD=`pwd`
	cd ${OBJDIR}
	ln -s ${ENG_FROM_OBJDIR}/*.[ch] .
	ln -s ${ENG_FROM_OBJDIR}/*.pl .
	ln -s ${ENG_FROM_OBJDIR}/Makefile .
	rm -f configure.h
	cd ${OLDPWD}
    else
	# TODO: This code is not used yet...
	OLDPWD=`pwd`
	cd engine
	for File in *.[ch] *.pl Makefile ; do
	    if [ ! -f ${OBJDIR}/${File} -o ${File} -nt ${OBJDIR}/${File} ]; then
		rm -f ${OBJDIR}/${File}
		cp ${File} ${OBJDIR}/${File}
	    fi
	done
	cd ${OLDPWD}
    fi
    # TODO: check if the directories are correct
#    normal_message "Creating engine include directory"
    create_engdir ${SRCINCLUDEDIRROOT} ${SRCINCLUDEDIR}
}

create_engdir() { # DIRROOT DIR
    if test ! -d ${1}; then
	mkdir ${1};
	chmod ${EXECMODE} ${1};
    fi
    if test ! -d ${2}; then
	mkdir -p ${2};
	chmod ${EXECMODE} ${2};
    fi
}

# Writes the input to FILE. If the input is exactly the same than the
# output, the old file is preserved (thus the modification time is not
# affected)
update_file() { # FILE
    TMP=${1}-tmp
    cat > ${TMP}
    if cmp -s ${1} ${TMP}; then
	# identical (keep the original, thus preserving its
	# creation date)
	rm ${TMP}
    else
	# different, or ${1} does not exist (replace by new file)
	rm -f ${1}
	mv ${TMP} ${1}
	chmod ${DATAMODE} ${1}
    fi
}

# TODO: Make this part of the configuration process? (not the build
#       process) This would make 'update_file' unnecessary, unless you
#       want to reuse that to detect when the configuration changed in
#       some way. (JFMC)

# Generate the build info for the engine:
#   - versions, gcc options, OS suffixes, etc.
# TODO: take the REALLIBDIR as a environment parameter, so that its
# definition is not necessary
gen_engine_build_info() {
    get_commit_info # get COMMIT_*

    if test x"${DEBUG_LEVEL}" = x"nodebug"; then
	SHOW_DEBUG_LEVEL=""
    else
	SHOW_DEBUG_LEVEL=" [${DEBUG_LEVEL}]"
    fi

    # version.h:
    MAJORVER=`echo ${VERSION} | cut -f1 -d'.'`
    MINORVER=`echo ${VERSION} | cut -f2 -d'.'`
    update_file ${OBJDIR}/version.h <<EOF
#define CIAO_MAJOR_VERSION ${MAJORVER}
#define CIAO_MINOR_VERSION ${MINORVER}
#define CIAO_PATCH_VERSION ${PATCH}
#define CIAO_COMMIT_BRANCH "${COMMIT_BRANCH}"
#define CIAO_COMMIT_ID "${COMMIT_ID}"
#define CIAO_COMMIT_DATE "${COMMIT_DATE}"
#define CIAO_COMMIT_DESC "${COMMIT_DESC}"
EOF

    # version.c:
    update_file ${OBJDIR}/version.c <<EOF
char *emulator_version = "Ciao ${COMMIT_DESC}: `date`${SHOW_DEBUG_LEVEL}";
char *emulator_architecture = "${ARCHNAME}";
char *emulator_os =    "${OSNAME}";
char *emulator_debug = "${CIAODEBUG}";
char *emulator_location = "${ENGLOCATION}";
char *ciao_version =   "${VERSION}";
char *ciao_patch =     "${PATCH}";
char *ciao_commit_branch = "${COMMIT_BRANCH}";
char *ciao_commit_id = "${COMMIT_ID}";
char *ciao_commit_date = "${COMMIT_DATE}";
char *ciao_commit_desc = "${COMMIT_DESC}";
char *ciao_suffix =    "${CIAOSUFFIX}";
char *exec_suffix =    "${EXECSUFFIX}";
char *so_suffix =      "${SOSUFFIX}";
char *default_lib_dir = "${REALLIBDIR}";
char *default_c_headers_dir = "${CIAOHDIR}";
EOF
}

# ===========================================================================
# Get commit information from the control version system (Git or SVN) 

# TODO: Combine with 'ciaobot' and 'lpdist' code (probably remove from
# this part).
# TODO: 'branch, id, and date' definitions for SVN are missing

get_commit_info() {
    if `which git > /dev/null 2>&1` && \
       [ -d ${CIAODESRC}/.git ] ; then
	REPO_KIND=git
    elif `which svnversion > /dev/null 2>&1` && \
       [ -d ${CIAODESRC}/.svn ] ; then
	REPO_KIND=svn
    else
	REPO_KIND=none
    fi

    set +e # allow errors here since 'git' or 'svnversion' may fail

    # COMMIT_ID: Git commit id or SVN revision number (0 if everything else fails)
    # COMMIT_BRANCH: branch name (ignored in SVN at this moment)
    # COMMIT_DATE: Git commit date (ignored for SVN)
    # COMMIT_DESC: human-readable description of the commit (including
    #   version, patch, branch, etc. if available)
    if [ x"${REPO_KIND}" = x"svn" ]; then
	COMMIT_BRANCH=""
	COMMIT_ID="`svnversion ${CIAODESRC}/makedir 2>/dev/null`"
	if test x"${COMMIT_ID}" = x"exported"; then
	    COMMIT_ID=""
	fi
	COMMIT_DATE=""
	if [ x"${COMMIT_ID}" = x"" ] ; then
	    COMMIT_DESC="${VERSION}.${PATCH}"
	else
	    COMMIT_DESC="${VERSION}.${PATCH}-${COMMIT_ID}"
	fi
    elif [ x"${REPO_KIND}" = x"git" ]; then
	COMMIT_BRANCH="`cd ${CIAODESRC}; git rev-parse --abbrev-ref HEAD 2>/dev/null`"
	if [ x"${COMMIT_BRANCH}" = x"HEAD" ] ; then
	    COMMIT_BRANCH="" # Detached HEAD!
	fi
	COMMIT_ID="`cd ${CIAODESRC}; git log -1 --format='%H' 2>/dev/null`"
	COMMIT_DATE="`cd ${CIAODESRC}; git log -1 --format='%ci' 2>/dev/null`"
	COMMIT_DESC="`cd ${CIAODESRC}; git describe --tags 2>/dev/null`"
	if [ x"${COMMIT_DESC}" = x"" ] ; then
	    # Create our own human-readable commit description using the
	    # branch name (this should not be needed if we have proper
	    # tags in our commit graph).
	    COMMIT_ID_SHORT="`cd ${CIAODESRC}; git log -1 --format='%h' 2>/dev/null`"
	    COMMIT_DESC="${COMMIT_BRANCH}-g${COMMIT_ID_SHORT}"
	fi
        # Fix COMMIT_DESC (this assumes that we may include version and
        # patch in tag and branch names, and removes redundant
        # information)
	case ${COMMIT_DESC} in
	    # Version and patch in COMMIT_DESC, let us assume that is a
	    # particular code release (a commit with a tag, not a branch).
	    v${VERSION}${PATCH}*) COMMIT_DESC="${VERSION}${PATCH}" ;;
	    ${VERSION}${PATCH}*)  COMMIT_DESC="${VERSION}${PATCH}" ;;
	    # Version but not patch in COMMIT_DESC, just use it (it is a
	    # develoment release).
	    v${VERSION}*)
		# (remove 'v' from the string)
		COMMIT_DESC="`portable_substr "${COMMIT_DESC}" 1 ${#COMMIT_DESC}`"
		;;
	    ${VERSION}*)  true ;;
	    # No version info in commit desc, just append it (also a
	    # development release).
	    *) COMMIT_DESC="${VERSION}-${COMMIT_DESC}" ;;
	esac
    else
	COMMIT_BRANCH=""
	COMMIT_ID=""
	COMMIT_DATE=""
	COMMIT_DESC=""
    fi

    # Finally, get from file if empty
    if test x"${COMMIT_BRANCH}" = x""; then
	if test -f "${CIAOBUILDDIR}/COMMIT_BRANCH"; then \
	    COMMIT_BRANCH=`cat "${CIAOBUILDDIR}/COMMIT_BRANCH"`
	else
	    COMMIT_BRANCH="0"
	fi
    fi
    if test x"${COMMIT_ID}" = x""; then
	if test -f "${CIAOBUILDDIR}/COMMIT_ID"; then \
	    COMMIT_ID=`cat "${CIAOBUILDDIR}/COMMIT_ID"`
	else
	    COMMIT_ID="0"
	fi
    fi
    if test x"${COMMIT_DATE}" = x""; then
	if test -f "${CIAOBUILDDIR}/COMMIT_DATE"; then \
	    COMMIT_DATE=`cat "${CIAOBUILDDIR}/COMMIT_DATE"`
	else
	    COMMIT_DATE="0"
	fi
    fi
    if test x"${COMMIT_DESC}" = x""; then
	if test -f "${CIAOBUILDDIR}/COMMIT_DESC"; then \
	    COMMIT_DESC=`cat "${CIAOBUILDDIR}/COMMIT_DESC"`
	else
	    COMMIT_DESC="0"
	fi
    fi
    set -e
}

# The expression ${V:Offset:Len} is a bashism. Try 'awk' when available.
portable_substr() { # STRING OFFSET LEN
    local STRING
    local OFFSET
    local LEN
    STRING="${1}"
    OFFSET=${2}
    LEN=${3}
    if which awk > /dev/null 2>&1; then
	echo ${STRING} | awk -v offset=${OFFSET} -v len=${LEN} '{ string=substr($0, offset+1, len); print string; }'
    else
	echo "${STRING:${OFFSET}:${LEN}}"
    fi
}

# ===========================================================================
# Cross-compilation of the engine
# TODO: Not working... disabled at the moment.
#
#   It is required a simple way to feed the compiler with a
#   description of the target machine/platform. That is, the active
#   cross-compiler, the way of obtaining the configuration at the
#   target machine, as well as the cross-compiled (C) libraries.

## TODO: Not working
#
# do_crossengwin32_a() {
#     do_engine_srcdir_and_links
#     gen_engine_build_info
# }
# 
# do_crossengwin32() {
#     CIAOARCH=Win32i86 crossengwin32_a
#     OLDUMASK=`umask`
#     umask 002 # TODO: Undo?
#     pushd ${OBJDIRROOT}/Win32i86${CIAODEBUG} > /dev/null
#     do_gmake configure && \
# 	CIAOARCH=crossWin32i86 do_gmake crossconfigure.h  && \
# 	CIAOARCH=crossWin32i86 do_gmake ${MFLAGS} ciaoemulator
#     popd > /dev/null
#     ADDOBJ="`statobj`"
#     CURRLIBS="${LIBS} ${STAT_LIBS}"
#     umask ${OLDUMASK}
# }

# TODO: Do not send ciao_get_arch but all the configuration scripts
#       (configuration may be incorrect otherwise)
# TODO: Need to reload the full settings!
# do_crossengine() {
#     /usr/bin/scp ${CIAOSRC}/etc/ciao_get_arch ${CROSS_COMPILER_HOST}:/tmp && \
# 	ENGINE_LINK_TYPE=crossdyn CIAOARCH=`/usr/bin/ssh ${CROSS_COMPILER_HOST} /tmp/ciao_get_arch` do_doengine
# }

## TODO: Outdated, recover?
#
# do_preparecrosslib() {
#     /bin/rm -rf ../clean-ciao-libs
#     /bin/mkdir ../clean-ciao-libs
#     rsync -a --exclude='.svn*' --exclude='*.asr' lib ../clean-ciao-libs 
# # 	&& \
# # 	rsync -a --exclude='.svn*' --exclude='*.asr' library ../clean-ciao-libs
# }

# ===========================================================================
## TODO: Disabled, enable again?
#
# do_cflow() {
#     pushd ${OBJDIR} > /dev/null
#     cflow -i -D${CIAOARCH} *.c > ${CIAOSRC}/etc/cflow.out
#     popd > /dev/null
# }
# 
# do_cxref() {
#     pushd ${OBJDIR} > /dev/null
#     cxref -xref-function -D${ARCHNAME} -D${OSNAME} \
# 	${THREAD_FLAG} ${FOREIGN_FILES_FLAG} *.[ch] -O${CIAOSRC}/etc/cxref
#     popd > /dev/null
# }

# ===========================================================================
# Other engine commands 
# cflow) do_cflow ;;
# cxref) do_cxref ;;
#
# -----------------------------------------------------------------------
# TODO: Cross-compilation disabled at the moment (see code above)
#
# preparecrosslib) do_preparecrosslib ;; #???
# crossengwin32) do_crossengwin32 ;;
# crossengine) do_crossengine ;; #????
# ===========================================================================

do_clean_engine() {
    if not_configured; then
	# assume that if there is no configuration, then there is no
	# engine to be clean
	true
    else
	ensure_config_loaded
#    bold_message "Removing engine for all architectures"
	rm -rf ${OBJDIRROOT}
	rm -rf ${SRCINCLUDEDIRROOT}
    fi
}

case "${1}" in
    build) do_build_engine ;;
    build_static) do_build_static_engine ;;
    clean) do_clean_engine ;;
    engine_is_ok) engine_is_ok ;;
    *)
	echo "Unknown target '${1}' in build_engine.sh" >&2
	exit 1
	;;
esac
