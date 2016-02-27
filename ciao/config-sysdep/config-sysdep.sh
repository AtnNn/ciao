#!/bin/sh
# Input: CIAOARCH
# TODO: Merge with optim_comp/modules/core/engine/internals.cfg/

# TODO: Add support for new architectures from official branch; update
#       old architectures from optim_comp

# Get OSNAME (operating system) and ARCHNAME (architecture name)
case ${CIAOARCH} in
    SYMMi86)           OSNAME=Dynix; ARCHNAME=i86 ;; #SEEN
    IRIXmips)          OSNAME=IRIX; ARCHNAME=mips ;; #SEEN
    BSDi86)            OSNAME=LINUX; ARCHNAME=i86 ;; # TODO: correct??? #SEEN
    # ! Assume that DARWIN >= 8.0.0 (i.e. MAC OS >= 10.4)
    DARWINppc)         OSNAME=DARWIN; ARCHNAME=ppc ;; #SEEN
    DARWINi86)         OSNAME=DARWIN; ARCHNAME=i86 ;; #SEEN
    DARWINx86_64)      OSNAME=DARWIN; ARCHNAME=x86_64 ;; #NEW in optim_comp
    DARWINx86_64m32)   OSNAME=DARWIN; ARCHNAME=x86_64m32 ;; #NEW in optim_comp
    LINUXalpha)        OSNAME=LINUX; ARCHNAME=alpha ;; #SEEN
    LINUXi86)          OSNAME=LINUX; ARCHNAME=i86 ;;
#    LINUXx86_64)       OSNAME=LINUX; ARCHNAME=x86_64 ;; #NEW in optim_comp
    LINUXi86_64)       OSNAME=LINUX; ARCHNAME=i86_64 ;; #OLD in official (replace by LINUXx86_64)
    LINUXx86_64m32)    OSNAME=LINUX; ARCHNAME=x86_64m32 ;; #NEW in optim_comp
    LINUXppc)          OSNAME=LINUX; ARCHNAME=ppc ;; #SEEN
    LINUXppc64)        OSNAME=LINUX; ARCHNAME=ppc64 ;; #SEEN
    LINUXppc64m32)     OSNAME=LINUX; ARCHNAME=ppc64m32 ;; #NEW in optim_comp
    LINUXSparc)        OSNAME=LINUX; ARCHNAME=Sparc ;; #SEEN
    LINUXSparc64)      OSNAME=LINUX; ARCHNAME=Sparc64 ;; #SEEN
    LINUXarm)          OSNAME=LINUX; ARCHNAME=arm ;; #NEW in official
    LINUXarmv4l)       OSNAME=LINUX; ARCHNAME=armv4l ;; #SEEN
    LINUXarmv5tel)     OSNAME=LINUX; ARCHNAME=armv5tel ;; #SEEN
    # SUNMP use Solaris. Solaris on monoprocessors is returned as SUN4SOL2
    Solarisi86)        OSNAME=Solaris; ARCHNAME=i86 ;; #SEEN
    SolarisSparc)      OSNAME=Solaris; ARCHNAME=Sparc ;; #NEW in optim_comp
    SolarisSparc64)    OSNAME=Solaris; ARCHNAME=Sparc64 ;; #SEEN
    SolarisSparc64m32) OSNAME=Solaris; ARCHNAME=Sparc64m32 ;; #NEW in optim_comp
    SunOS4Sparc)       OSNAME=SunOS4; ARCHNAME=Sparc ;; #SEEN
    Win32alpha)        OSNAME=Win32; ARCHNAME=alpha ;; #SEEN
    Win32i86)          OSNAME=Win32; ARCHNAME=i86 ;; #SEEN
    crossWin32i86)     OSNAME=crossWin32; ARCHNAME=i86 ;; #New in official (useful?)
esac

setup_darwin11_cc() {
    # In MacOS (release 10.7 and above) 'gcc' points to 'llvm-gcc' by default, which 
    # generates wrong code for Ciao (and many other programs).
    #
    # If the user did not provide a custom 'cc' and 'ld', we will
    # assume that the default values are wrong and stop the
    # configuration.
    #
    # TODO: We should run a test to make sure that the C compiler can
    #       generate valid code.
    #
    # TODO: Remove this code when the problem is solved.
    cat <<EOF

** ERROR: Unspecified compiler and linker.

The default compiler on Mac OS release 10.7 and above (llvm-gcc that comes
with XCode-4.2) does not correctly compile CiaoDE.  You have to use an
alternative C-compiler, for instance separately compiled GNU gcc, that
does not depend on the LLVM backend.

Building Ciao on Mac OS 10.7 was succesfully tested with GNU gcc versions 4.2
and 4.6.1. Use the '--cc=CC' option (and '--ld=LD') in 'ciaosetup
configure' to specify the alternative C compiler (and linker) that are
going to be used for compiling Ciao. If you are using GNU gcc, the
linker and compiler can be the same executable.  Example:

  ./ciaosetup configure --cc=/HOME/mygcc/bin/gcc-4.6.1

An illustration of how to compile gcc-4.6.1 on Mac OS 10.7 can be
found on the following web page:

http://solarianprogrammer.com/2011/09/20/compiling-gcc-4-6-1-on-mac-osx-lion/
EOF
    exit 1
}

# Set CC (C compiler) and LD (linker)
if [ x"${CUSTOM_CC}" != x"" ]; then
    # Use custom values for CC and LD
    CC="${CUSTOM_CC}"
    if [ x"${CUSTOM_LD}" != x"" ]; then
	LD="${CUSTOM_LD}"
    else
	LD="${CUSTOM_CC}"
    fi
else
    # Detect based on architecture
    case ${CIAOARCH} in
	SYMMi86)        CC=cc; LD=ld ;;
	SolarisSparc)   CC=gcc; LD=ld ;;
	IRIXmips)       CC=cc; LD=ld ;;
	LINUXarmv5tel)  CC=arm-linux-gcc; LD=arm-linux-gcc ;;
	Solarisi86)     CC=gcc; LD=ld ;;
	SolarisSparc64) CC=gcc; LD=ld ;;
	SunOS4Sparc)    CC=gcc; LD=ld ;;
	crossWin32i86)  CC=i386-mingw32-gcc; LD=i386-mingw32-gcc ;;
	DARWIN*)
	    case "`/usr/bin/uname -r`" in
		11.*|12.*|13.*)  setup_darwin11_cc ;;
		*)     CC=gcc; LD=gcc ;;
	    esac ;;
	*)
	# The rest of the systems just use plain 'gcc'
	    CC=gcc; LD=gcc ;;
#    BSDi86)         CC=gcc; LD=gcc ;;
#    DARWINppc)      CC=gcc; LD=gcc ;;
#    DARWINi86)      CC=gcc; LD=gcc ;;
#    LINUXalpha)     CC=gcc; LD=gcc ;;
#    LINUXi86)       CC=gcc; LD=gcc ;;
#    LINUXi86_64)    CC=gcc; LD=gcc ;;
#    LINUXppc)       CC=gcc; LD=gcc ;;
#    LINUXppc64)     CC=gcc; LD=gcc ;;
#    LINUXSparc)     CC=gcc; LD=gcc ;;
#    LINUXSparc64)   CC=gcc; LD=gcc ;;
#    LINUXarm)       CC=gcc; LD=gcc ;;
#    LINUXarmv4l)    CC=gcc; LD=gcc ;;
#    Win32alpha)     CC=gcc; LD=gcc ;;
#    Win32i86)       CC=gcc; LD=gcc ;;
    esac
fi

# Does this machine has dynamic linking abilities?
case ${CIAOARCH} in
    SYMMi86)       FOREIGN_FILES_FLAG= ;;
    crossWin32i86) FOREIGN_FILES_FLAG= ;;
    *)             FOREIGN_FILES_FLAG="-DFOREIGN_FILES" ;;
esac

# Libraries and flags to use threads
LD_THREAD_LIB=
LD_LOCK_LIB=
THREAD_FLAG=
if test x"${USE_THREADS}" = x"yes"; then
    case ${CIAOARCH} in
	SolarisSparc*)
	    case "`/bin/uname -r`" in
		5.[123456]* ) SOLARIS_VERSION=pre_7 ;;
		5.* )         SOLARIS_VERSION=equal_post_7 ;;
	    esac
	    THREAD_FLAG="-D_REENTRANT -DTHREADS"
	    if test x"${SOLARIS_VERSION}" = x"pre_7"; then
		LD_THREAD_LIB="-lpthread"
	    else
		LD_THREAD_LIB="-lpthread -lrt"
	    fi
	    ;;
        # I do not know how to write locks in MIPS assembler code, so
        # only POSIX locks are accepted at the moment.
	IRIXmips)      LD_THREAD_LIB="-lpthread"; THREAD_FLAG="-DTHREADS" ;;
	BSDi86)        LD_THREAD_LIB="-lpthread"; THREAD_FLAG="-D_REENTRANT -DTHREADS" ;;
	DARWINppc)     THREAD_FLAG="-D_REENTRANT -DTHREADS" ;;
	DARWINi86)     THREAD_FLAG="-D_REENTRANT -DTHREADS" ;;
	LINUXalpha)    LD_THREAD_LIB="-lpthread"; THREAD_FLAG="-D_REENTRANT -DTHREADS" ;;
	LINUXi86)      LD_THREAD_LIB="-lpthread"; THREAD_FLAG="-D_REENTRANT -DTHREADS" ;;
	LINUXi86_64)   LD_THREAD_LIB="-lpthread"; THREAD_FLAG="-D_REENTRANT -DTHREADS" ;;
	LINUXppc)      LD_THREAD_LIB="-lpthread"; THREAD_FLAG="-D_REENTRANT -DTHREADS" ;;
	LINUXppc64)    LD_THREAD_LIB="-lpthread"; THREAD_FLAG="-D_REENTRANT -DTHREADS" ;;
	LINUXSparc)    LD_THREAD_LIB="-lpthread"; THREAD_FLAG="-D_REENTRANT -DTHREADS" ;;
	LINUXSparc64)  LD_THREAD_LIB="-lpthread"; THREAD_FLAG="-D_REENTRANT -DTHREADS" ;;
	LINUXarm)      LD_THREAD_LIB="-lpthread"; THREAD_FLAG="-D_REENTRANT -DTHREADS" ;;
	LINUXarmv4l)   LD_THREAD_LIB="-lpthread"; THREAD_FLAG="-D_REENTRANT -DTHREADS" ;;
	LINUXarmv5tel) LD_THREAD_LIB="-lpthread"; THREAD_FLAG="-D_REENTRANT -DTHREADS" ;;
	Solarisi86)    LD_THREAD_LIB="-lpthread"; THREAD_FLAG="-D_REENTRANT -DTHREADS" ;;
        # Threads and locks in Win32: no threads, no locks so far.
	Win32alpha) THREAD_FLAG="-DTHREADS" ;;
	Win32i86) THREAD_FLAG="-DTHREADS" ;;
    esac    
    # POSIX locks
    if test x"${OSNAME}" = x"Solaris"; then
	if test x"${USE_POSIX_LOCKS}" = x"yes"; then
	    LD_LOCK_LIB="-lposix4"
	fi
    fi
fi

# C compiler options for generating shared libraries code
# Linker options for shared objects
case ${CIAOARCH} in
    IRIXmips)       CCSHARED= ; LDSHARED="-shared" ;;
    BSDi86)         CCSHARED="-fPIC" ; LDSHARED="-shared" ;;
    DARWINppc)      CCSHARED="-fPIC" ; LDSHARED="-flat_namespace -dynamiclib -undefined suppress" ;;
    DARWINi86)      CCSHARED="-m32 -fPIC" ; LDSHARED="-m32 -flat_namespace -dynamiclib -undefined suppress" ;;
    LINUXalpha)     CCSHARED="-fPIC" ; LDSHARED="-shared" ;;
    LINUXi86)       CCSHARED="-fPIC" ; LDSHARED="-shared" ;;
    LINUXi86_64)    CCSHARED="-m32 -fPIC -shared" ; LDSHARED="-m32 -fPIC -shared" ;;
    LINUXppc)       CCSHARED="-fPIC" ; LDSHARED="-shared" ;;
    LINUXppc64)     CCSHARED="-fPIC" ; LDSHARED="-shared" ;;
    LINUXSparc)     CCSHARED="-fPIC" ; LDSHARED="-shared" ;;
    LINUXSparc64)   CCSHARED="-fPIC" ; LDSHARED="-shared" ;;
    LINUXarm)       CCSHARED="-fPIC" ; LDSHARED="-shared" ;;
    LINUXarmv4l)    CCSHARED="-fPIC" ; LDSHARED="-shared" ;;
    LINUXarmv5tel)  CCSHARED="-fPIC" ; LDSHARED="-shared" ;;
    Solarisi86)     CCSHARED= ; LDSHARED="-G" ;;
    SolarisSparc)   CCSHARED= ; LDSHARED="-G" ;;
    SolarisSparc64) CCSHARED= ; LDSHARED="-G" ;;
    SunOS4Sparc)    CCSHARED="-fPIC" ; LDSHARED= ;;
    Win32alpha)     CCSHARED= ; LDSHARED="-shared" ;;
    Win32i86)       CCSHARED= ; LDSHARED="-shared" ;;
    crossWin32i86)  CCSHARED= ; LDSHARED="-c" ;;
esac

# Linker options to combine objects
# TODO: used?
LDCOMBINE=-r

# Extension of shared libraries
case ${CIAOARCH} in
    IRIXmips)  	    SOSUFFIX=".so" ;;
    BSDi86)    	    SOSUFFIX=".so" ;;
    DARWINppc) 	    SOSUFFIX=".dylib" ;;
    DARWINi86) 	    SOSUFFIX=".dylib" ;;
    LINUXalpha)     SOSUFFIX=".so" ;;
    LINUXi86)       SOSUFFIX=".so" ;;
    LINUXi86_64)    SOSUFFIX=".so" ;;
    LINUXppc)       SOSUFFIX=".so" ;;
    LINUXppc64)     SOSUFFIX=".so" ;;
    LINUXSparc)     SOSUFFIX=".so" ;;
    LINUXSparc64)   SOSUFFIX=".so" ;;
    LINUXarm)       SOSUFFIX=".so" ;;
    LINUXarmv4l)    SOSUFFIX=".so" ;;
    LINUXarmv5tel)  SOSUFFIX=".so" ;;
    Solarisi86)     SOSUFFIX=".so" ;;
    SolarisSparc)   SOSUFFIX=".so" ;;
    SolarisSparc64) SOSUFFIX=".so" ;;
    SunOS4Sparc)    SOSUFFIX=".so.1.0" ;;
    Win32alpha)     SOSUFFIX=".dll" ;;
    Win32i86)       SOSUFFIX=".dll" ;;
    crossWin32i86)  SOSUFFIX=".so" ;;
esac

case ${CIAOARCH} in
#    LINUXi86_64)    STAT_LIBS="-lgsl -lgslcblas -lm" ;;
    Solarisi86)     STAT_LIBS="-lsocket" ;;
    SolarisSparc)   STAT_LIBS="-lsocket" ;;
    SolarisSparc64) STAT_LIBS="-lsocket" ;;
esac

# Local C compiler (and local linker)
# TODO: Document
case ${CIAOARCH} in
    LINUXarmv5tel) LCC="gcc"; LLD="ld" ;;
    *)             LCC="${CC}"
esac

# How to install an make directories
case ${CIAOARCH} in
    SYMMi86) MKDIR="mkdir" ;;
    *)       MKDIR="mkdir -p" ;;
esac

# TODO: Used?
case ${CIAOARCH} in
    SYMMi86)        INSTALL="install -c" ;;
    IRIXmips)       INSTALL="/usr/bin/cp" ;;
    Solarisi86)     INSTALL="/usr/bin/cp" ;;
    SolarisSparc)   INSTALL="/usr/bin/cp" ;;
    SolarisSparc64) INSTALL="/usr/bin/cp" ;;
    *)              INSTALL="install" ;;
esac

# TODO: Useful now?
case ${CIAOARCH} in
    Win32alpha)     ENGINE_LINK_TYPE="stat" ;;
    crossWin32i86)  ENGINE_LINK_TYPE="stat" ;;
    *)              ENGINE_LINK_TYPE="dyn" ;;
esac

# Profiling
# Note: OPTIM_LEVEL is switched to 'debug' in profiling mode
if test x"${DEBUG_LEVEL}" = x"profile"; then
    OPTIM_LEVEL=debug
    case ${CIAOARCH} in
	BSDi86)        DEBUG_FLAGS="-pg -DPROFILE -DDEBUG"; PROFILE_LD_FLAGS="-pg" ;;
	DARWINppc)     DEBUG_FLAGS="-pg -DPROFILE -DDEBUG"; PROFILE_LD_FLAGS="-pg" ;;
	DARWINi86)     DEBUG_FLAGS="-pg -DPROFILE -DDEBUG"; PROFILE_LD_FLAGS="-pg" ;;
	LINUXalpha)    PROFILE_LD_FLAGS="-pg" ;;
	LINUXi86)      DEBUG_FLAGS="-DDEBUG -DATOMGC -DPROFILE" ;;
	LINUXi86_64)   DEBUG_FLAGS="-DDEBUG -DATOMGC -DPROFILE" ;;
	LINUXppc)      DEBUG_FLAGS="-pg -DPROFILE -DDEBUG"; PROFILE_LD_FLAGS="-pg" ;;
	LINUXppc64)    DEBUG_FLAGS="-DDEBUG -DATOMGC -DPROFILE" ;;
	LINUXSparc)    DEBUG_FLAGS="-pg -DPROFILE -DDEBUG"; PROFILE_LD_FLAGS="-pg" ;;
	LINUXSparc64)  DEBUG_FLAGS="-pg -DPROFILE -DDEBUG"; PROFILE_LD_FLAGS="-pg" ;;
	LINUXarm)      DEBUG_FLAGS="-DDEBUG -DATOMGC -DPROFILE" ;;
	LINUXarmv4l)   DEBUG_FLAGS="-pg -DPROFILE -DDEBUG"; PROFILE_LD_FLAGS="-pg" ;;
	LINUXarmv5tel) DEBUG_FLAGS="-pg -DPROFILE -DDEBUG"; PROFILE_LD_FLAGS="-pg" ;;
	SunOS4Sparc)   PROFILE_LD_FLAGS="-pg" ;;
	Solaris*)      PROFILE_LD_FLAGS="-pg" ;;
	Win32alpha)    DEBUG_FLAGS="-DDEBUG -DPROFILE" ;;
	Win32i86)      DEBUG_FLAGS="-DDEBUG -DATOMGC -DPROFILE" ;;
    esac
elif test x"${DEBUG_LEVEL}" = x"profile-debug"; then
    OPTIM_LEVEL=debug
    case ${CIAOARCH} in
	LINUXi86)    DEBUG_FLAGS="-g -Wall -DDEBUG -DATOMGC -DGLOBVARS -DINTERNAL_CALLING -DPROFILE" ;;
	LINUXi86_64) DEBUG_FLAGS="-g -Wall -DDEBUG -DATOMGC -DGLOBVARS -DINTERNAL_CALLING -DPROFILE" ;;
	LINUXppc64)  DEBUG_FLAGS="-g -Wall -DDEBUG -DATOMGC -DGLOBVARS -DINTERNAL_CALLING -DPROFILE" ;;
	LINUXarm)    DEBUG_FLAGS="-g -Wall -DDEBUG -DATOMGC -DGLOBVARS -DINTERNAL_CALLING -DPROFILE" ;;
	Win32alpha)  DEBUG_FLAGS="-g -Wall -DDEBUG -DATOMGC -DGLOBVARS -DINTERNAL_CALLING -DPROFILE" ;;
	Win32i86)    DEBUG_FLAGS="-g -Wall -DDEBUG -DATOMGC -DGLOBVARS -DINTERNAL_CALLING -DPROFILE" ;;
    esac
fi

# Optimizations
M_OR_F_OPTIONS="`${SYSDEP_FILES}/gcc_m_or_f_options`"
case ${CIAOARCH} in
    IRIXmips)       OPTIM_FLAGS0="-O" ;;
    BSDi86)         OPTIM_FLAGS0="-fomit-frame-pointer ${M_OR_F_OPTIONS}" ;;
    # does omit-frame-pointer make sense in non-x86 architectures?
    DARWINppc)      OPTIM_FLAGS0="-fomit-frame-pointer ${M_OR_F_OPTIONS}" ;;
    DARWINi86)      OPTIM_FLAGS0="-fomit-frame-pointer ${M_OR_F_OPTIONS}" ;;
    LINUXalpha)     OPTIM_FLAGS0="" ;;
    LINUXi86)       OPTIM_FLAGS0="-fomit-frame-pointer ${M_OR_F_OPTIONS}" ;;
    LINUXi86_64)    OPTIM_FLAGS0="-fomit-frame-pointer ${M_OR_F_OPTIONS}" ;;
    LINUXppc)       OPTIM_FLAGS0="-fomit-frame-pointer ${M_OR_F_OPTIONS}" ;;
    LINUXppc64)     OPTIM_FLAGS0="-fomit-frame-pointer ${M_OR_F_OPTIONS}" ;;
    LINUXSparc)     OPTIM_FLAGS0="-fomit-frame-pointer" ;;
    LINUXSparc64)   OPTIM_FLAGS0="-fomit-frame-pointer" ;;
    LINUXarm)       OPTIM_FLAGS0="-fomit-frame-pointer ${M_OR_F_OPTIONS}" ;;
    LINUXarmv4l)    OPTIM_FLAGS0="-fomit-frame-pointer" ;;
    LINUXarmv5tel)  OPTIM_FLAGS0="-fomit-frame-pointer" ;;
    Solarisi86)     OPTIM_FLAGS0="" ;;
    # We are not using strict-aliasing to avoid problems in SolarisSparc64
    SolarisSparc64) OPTIM_FLAGS0="" ;;
    SunOS4Sparc)    OPTIM_FLAGS0="" ;;
    Win32i86)       OPTIM_FLAGS0="-fomit-frame-pointer ${M_OR_F_OPTIONS}" ;;
    crossWin32i86)  OPTIM_FLAGS0="-fomit-frame-pointer ${M_OR_F_OPTIONS}" ;;
esac
if test x"${OPTIM_LEVEL}" = x"optimized"; then
    OPTIM_FLAGS="-Wall -fno-strict-aliasing -O2 ${OPTIM_FLAGS0}"
else
    # TODO: Why not "-O2" as in optim_comp? which one is wrong?
    OPTIM_FLAGS=""
fi

# Memory management primitives
# See engine/configure.c: HAS_MMAP includes USE_OWN_MALLOC
MEM_MNG_FLAGS=""
case ${CIAOARCH} in
    DARWINppc)      MEM_MNG_FLAG="-DHAS_MMAP -DANONYMOUS_MMAP" ;;
    DARWINppc)      MEM_MNG_FLAG="-DHAS_MMAP -DANONYMOUS_MMAP" ;;
    DARWINi86)      MEM_MNG_FLAG="-DHAS_MMAP -DANONYMOUS_MMAP" ;;
    LINUXi86)       MEM_MNG_FLAG="-DHAS_MMAP -DANONYMOUS_MMAP" ;;
    LINUXi86_64)    MEM_MNG_FLAG="-DHAS_MMAP -DANONYMOUS_MMAP" ;;
    LINUXppc)       MEM_MNG_FLAG="-DHAS_MMAP -DANONYMOUS_MMAP" ;;
    LINUXppc64)     MEM_MNG_FLAG="-DHAS_MMAP -DANONYMOUS_MMAP" ;;
    LINUXSparc64)   MEM_MNG_FLAG="-DUSE_OWN_MALLOC" ;;
    LINUXarm)       MEM_MNG_FLAG="-DHAS_MMAP -DANONYMOUS_MMAP" ;;
    SolarisSparc)   MEM_MNG_FLAG="-DHAS_MMAP -DANONYMOUS_MMAP" ;;
    SolarisSparc64) MEM_MNG_FLAG="-DHAS_MMAP -DANONYMOUS_MMAP" ;;
    Win32i86)       MEM_MNG_FLAG="-DHAS_MMAP -DANONYMOUS_MMAP" ;;
esac

# Architecture options
# TODO: rename to ARCH_FLAGS like in optim_comp?
case ${CIAOARCH} in
    LINUXi86_64)    ARCHFLAGS="-m32" ;;
    DARWINi86)      ARCHFLAGS="-m32" ;;
    LINUXi86)       ARCHFLAGS= ;;
    LINUXi86_64)    ARCHFLAGS="-m32" ;;
    LINUXppc64)     ARCHFLAGS="-m32" ;;
    LINUXSparc64)   ARCHFLAGS="-m32" ;;
    LINUXarm)       ARCHFLAGS= ;;
    SolarisSparc64) ARCHFLAGS="-m32" ;;
    # Symmetry C compiler and linker specific options
    SYMMi86)        ARCHFLAGS="-i" ;;
    # TODO: WHY??
    Solarisi86)     ARCHFLAGS="-fPIC" ;;
    *)              ARCHFLAGS="" ;;
esac

# TODO: Rename LINK_FLAGS by LDFLAGS
# Linker specific options
# Note: LINUX linker specific options
#   "-rdynamic" allows the dynamic libraries symbols to be resolved
#   against
case ${CIAOARCH} in
    IRIXmips)      LDFLAGS0= ;;
    BSDi86)        LDFLAGS0="-rdynamic" ;;
    DARWINppc)     LDFLAGS0="" ;;
    DARWINi86)     LDFLAGS0="-m32" ;;
    LINUXalpha)    LDFLAGS0="-rdynamic" ;;
    LINUXi86)      LDFLAGS0="-rdynamic" ;;
    LINUXi86_64)   LDFLAGS0="-rdynamic -m32" ;;
    LINUXppc)      LDFLAGS0="-rdynamic" ;;
    LINUXppc64)    LDFLAGS0="-rdynamic" ;;
    LINUXSparc)    LDFLAGS0="-rdynamic" ;;
    LINUXSparc64)  LDFLAGS0="-rdynamic" ;;
    LINUXarm)      LDFLAGS0="-rdynamic" ;;
    LINUXarmv4l)   LDFLAGS0="-rdynamic" ;;
    LINUXarmv5tel) LDFLAGS0="-rdynamic" ;;
    Solaris*)      LDFLAGS0= ;;
    SunOS4Sparc)   LDFLAGS0= ;;
    Win32alpha)    LDFLAGS0= ;;
    Win32i86)      LDFLAGS0= ;;
    crossWin32i86) LDFLAGS0="-static" ;;
esac
LINK_FLAGS="${LDFLAGS0} ${PROFILE_LD_FLAGS}"

# Other libs
case ${CIAOARCH} in
    Win32i86)       LIBS0= ;;
    crossWin32i86)  LIBS0= ;;
    SYMMi86)        LIBS0="-lseq -lpps" ;;
    IRIXmips)       LIBS0="-lm" ;;
    BSDi86)         LIBS0="-lm" ;;
    Win32alpha)     LIBS0= ;;
# LIBS=-ldl
    DARWINppc)      LIBS0= ;;
    DARWINi86)      LIBS0= ;;
    Solarisi86)     LIBS0="-ldl -lm -lnsl" ;;
    SolarisSparc)   LIBS0="-ldl -lm -lnsl" ;;
    SolarisSparc64) LIBS0="-ldl -lm -lnsl" ;;
    *) # Linux and SunOS4
	LIBS0="-ldl -lm" ;;
esac

LIBS="${LIBS0} ${LD_THREAD_LIB} ${LD_LOCK_LIB} ${DEBUG_LIBS}"

# ===========================================================================

# Specifies if the engine will be in a shared library (dyn)
# or in an executable (unassigned).  By now is here, but
# should be configurable by the user
case ${CIAOARCH} in
    Win32alpha) ENGLOCATION="dyn" ;;
    Win32i86)   ENGLOCATION="dyn" ;;
esac

# Include path at compile time
case ${CIAOARCH} in
    BSDi86)         LDRPATH="-rpath" ;;
    LINUXi86)       LDRPATH="-rpath" ;;
    LINUXi86_64)    LDRPATH="-rpath" ;;
    LINUXppc64)     LDRPATH="-rpath" ;;
    LINUXarm)       LDRPATH="-rpath" ;;
    SolarisSparc)   LDRPATH="-R" ;;
    SolarisSparc64) LDRPATH="-R" ;;
esac

# Hack not to show a default console in Windows (used by the engine
# Makefile when compiling a static engine)
case ${CIAOARCH} in
    Win32i86)
        #NOCONSOLEFLAG=-mwindows
        # Hack not to use Cygwin's but Microsoft libraries
	NOCONSOLEFLAG="-mno-cygwin" ;;
    *) true
esac

# Custom options for SYMMi86
# TODO: Obsolete architecture?
case ${CIAOARCH} in
    SYMMi86)
        # Symmetry C compiler and linker specific options
	SDBEGIN="-ZO200000"
	# Operating system version
	OSREV="DYNIX"
	;;
    *) true
esac


