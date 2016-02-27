# Copyright (C) 1997, UPM-CLIP
#
# This Makefile *needs* GNU make
#
# make menuconfig       Show the menu for configure the system.  To avoid
#                       problems better use the ./ciaosetup script.
#
# make all              compile the whole Ciao system (engine, libraries, ...)
#                       (but does not regenerate docs: see make docs below)
# make install          install the whole Ciao system (must have been compiled
#                       before)
#
# make fullinstall      compile, generate docs and install the Ciao system
#
# make fullinstalleng	Compile and Install (or only
# make engine           compile) the Ciao engine for this particular arch.
#			This is the only make action needed for using Ciao
#                       executables in several architectures at once.
# 
# make cleanbackups     delete backup files
# make distclean        delete all files which can be automatically generated
# make engclean		delete all engines created
# make totalclean       cleanbackups + distclean
# 
# make docs             regenerate all manuals from the sources. 
#                       NOTE: This can only be done if lpdoc has been 
#                       installed previously.   
#                       It does need not be done during a normal install, 
#                       since the distribution comes with up to date 
#                       documentation.
# 
# make installdoc       a subset of 'make install', which only installs the 
#                       documentation. Useful after 'make docs'. 


#------- You should not change ANYTHING in this file -------------
#------- All customizable options are in the file SETTINGS -------

# Compiling (the engine) in Windows under cygwin:
# You can compile directly on a Windows machine, because CygWin
# allows mounting samba shares as Unix directories: if it is not done,
# just do:
# 
# $ mkdir --parents /home/clip          # Create a path to mount on
# $ mount -f -s //clip/clip /home/clip  # Mount the Samba share
# $ cd /home/clip/Systems/ciao-x.y
# $ make eng
# 
# ... etc.  This makes compiling, developing and testing much easier.
#
#-----------------------------------------------------------------
# Important note: you whould preffer the scheme:
#
# $(LOCALENGINE) [prolog_args] -C -b bootfile
#
# instead of this:
#
# $(SETLOCALCIAO) bootfile [prolog_args]
#
# The reason is that the first one is more portable.  The other
# will not work well in Win32 unless you add the .cpx extension to 
# the bootfile.
#

# CIAODESRC=$(PWD)/..
SRC=$(CIAODESRC)

include ../ciaosetup_modules/makefile__common
-include SETTINGS
-include SHARED

MFLAGS=-j$(PROCESSORS)

GMAKEOPTS="--no-print-directory"
GMAKE=$(MAKE) $(GMAKEOPTS)
#GMAKE=$(MAKE)

.PHONY: docs installdoc engine

default: all$(ALT)

all:
	$(GMAKE) engine
	$(GMAKE) init_library
	$(GMAKE) lpmake
	$(GMAKE) compiler
	$(GMAKE) applications
	$(GMAKE) libraries
	$(GMAKE) chr
	$(LPMAKECMD) environment

allnolibs:
	$(GMAKE) platdep
	$(LPMAKECMD) allnolibs

platdep:
	$(GMAKE) init_library
	$(GMAKE) lpmake
	$(GMAKE) compiler
	$(GMAKE) appsshell

crossengwin32: 
	$(GMAKE) $(OBJDIRROOT)/Win32i86$(CIAODEBUG) CIAOARCH=Win32i86
	$(GMAKE) $(SRCINCLUDEDIRROOT)/Win32i86 CIAOARCH=Win32i86
	$(GMAKE) commoneng CIAOARCH=Win32i86
	@(umask 002; cd $(OBJDIRROOT)/Win32i86$(CIAODEBUG) && \
	$(GMAKE) configure && \
	$(GMAKE) crossconfigure.h CIAORCH=crossWin32i86 && \
	$(GMAKE) $(MFLAGS) ciaoemulator CIAOARCH=crossWin32i86 \
	ADDOBJ='$(STATOBJ)' \
	CURRLIBS='$(LIBS) $(STAT_LIBS)')

crossengine:
	@(/usr/bin/scp $(CIAOSRC)/etc/ciao_get_arch $(CROSS_COMPILER_HOST):/tmp && \
	  $(GMAKE) CIAOARCH=$(shell /usr/bin/ssh $(CROSS_COMPILER_HOST) /tmp/ciao_get_arch) DEFAULTYPE=crossdyn doengine)

preparecrosslib:
	@(/bin/rm -rf ../clean-ciao-libs && \
	  /bin/mkdir ../clean-ciao-libs && \
	  rsync -a --exclude='.svn*' --exclude='*.asr' lib ../clean-ciao-libs) 
# 	&& \
# 	rsync -a --exclude='.svn*' --exclude='*.asr' library ../clean-ciao-libs

allpl:
	$(GMAKE) compiler
	$(GMAKE) applications
	$(GMAKE) libraries

engine:
	$(GMAKE) doengine
	$(GMAKE) englinks

# These links are needed to run ciao from the sources
# Note: The links are a bit trickly because this should work with the ciao header.
englinks:
	@(cd $(OBJDIRROOT) && \
            rm -f $(ENGINEBASE).$(CIAOARCH)$(EXECSUFFIX) && \
	    ln -s $(CIAOARCH)$(CIAODEBUG)/$(ENGINEBASE) \
		  $(ENGINEBASE).$(CIAOARCH)$(EXECSUFFIX) && \
	    rm -f $(ENGINEBASE).lnk $(ENGINEBASE) && \
	    ln -s $(ENGINEBASE).$(CIAOARCH)$(EXECSUFFIX) $(ENGINEBASE))

doengine:
	$(GMAKE) $(OBJDIRROOT)/$(CIAOARCH)$(CIAODEBUG)
	$(GMAKE) $(SRCINCLUDEDIR)
	$(GMAKE) $(DEFAULTYPE)eng
	$(GMAKE) exe_header # dowindowsbat

chr:
ifeq ($(COMPILE_CHR),no)
	@$(BOLD_MESSAGE) "Skipping compilation of CHR Libraries"
else
	@$(BOLD_MESSAGE) "Compiling CHR Libraries"
	@(cd $(CIAOSRC)/library/chr && \
	     $(SETLOCALCIAO) CIAOSH="$(CIAOSHELL)" ./do_bootstrap >> $(INSTALL_LOG); )
endif

libciao:
	@$(BOLD_MESSAGE) "Building 'libciao'"
	@(cd $(OBJDIR) && \
	$(GMAKE) libciao CURRLIBS='$(LIBS)' && \
	$(GMAKE) ciaoobject CURRLIBS='$(LIBS)' )

crossdyneng: commoneng
	@$(BOLD_MESSAGE) "Building dynamic engine for $(OSNAME)$(ARCHNAME) by cross-compilation"
	@( \
		umask 002 ; \
	 	$(GMAKE) -C $(OBJDIR) crossconfigure.h && \
	 	$(GMAKE) -C $(OBJDIR) $(MFLAGS) $(ENGINENAME) LCC=$(LCC) CURRLIBS='$(LIBS)' \
	)

dyneng: commoneng # stateng libciao
	@$(BOLD_MESSAGE) "Building dynamic engine for $(OSNAME)$(ARCHNAME)"
	@( \
		umask 002 ; \
		$(GMAKE) -C $(OBJDIR) configure.h && \
		$(GMAKE) -C $(OBJDIR) $(MFLAGS) $(ENGINENAME) CURRLIBS='$(LIBS)' \
	)

dostateng:
	$(BOLD_MESSAGE) "Building static engine for $(OSNAME)$(ARCHNAME)"
	-if [ -f $(OBJDIR)/$(ENGINENAME) ] ; then \
	  mv $(OBJDIR)/$(ENGINENAME) $(OBJDIR)/$(ENGINENAME).tmp ; \
	fi
	@( \
		umask 002 ; \
		$(GMAKE) -C $(OBJDIR) configure.h && \
		$(GMAKE) -C $(OBJDIR) $(MFLAGS) $(ENGINENAME) CURRLIBS='$(LIBS) $(STAT_LIBS)' \
			ADDOBJ='$(STATOBJ)' \
	)
	mv $(OBJDIR)/$(ENGINENAME) $(OBJDIR)/$(ENGINENAMESTAT) ; \
		chmod $(EXECMODE) $(OBJDIR)/$(ENGINENAMESTAT) ; \
	  mv $(OBJDIR)/$(ENGINENAME).tmp $(OBJDIR)/$(ENGINENAME)
ifneq ($(INSTALLGROUP),)
	-chgrp $(INSTALLGROUP) $(OBJDIR)/$(ENGINENAMESTAT)
endif

stateng: commoneng
# In Windows we need an executable built without console support 
# ifeq ($(OSNAME),Win32)
# 	cd $(OBJDIR) &&	$(GMAKE) clean
# 	$(GMAKE) $(MFLAGS) CONSOLEFLAG='$(NOCONSOLEFLAG)' dostateng
# 	/bin/mv $(OBJDIR)/$(ENGINAME).sta $(OBJDIR)/$(ENGINAME)_nc.sta
# 	cd $(OBJDIR) &&	$(GMAKE) clean
# endif
	$(GMAKE) $(MFLAGS) dostateng

commoneng:
	$(GMAKE) version-ciao

allengs:
	for machine in $(REMOTEMACHINES); do \
		echo ; \
		echo -------------------------------------------- ; \
		echo ; \
		echo Making engine in $$machine; \
		echo "unsetenv CIAODEBUG; cd $(CIAOSRC); $(GMAKE) eng" | rsh $$machine csh; \
		echo "setenv CIAODEBUG -debug; cd $(CIAOSRC); $(GMAKE) eng" | rsh $$machine csh; \
	done

installallengs:
	for machine in $(REMOTEMACHINES); do \
		echo ; \
		echo -------------------------------------------- ; \
		echo ; \
		echo Installing engine in $$machine; \
		echo "unsetenv CIAODEBUG; cd $(CIAOSRC); $(GMAKE) fullinstalleng" | rsh $$machine csh; \
	done

exe_header:
	cd lib/compiler && $(GMAKE) exe_header

NORTCHECKSLIBS=							\
	lib/dcg_expansion					\
	contrib/contextual/calculator				\
	library/toplevel/toplevel__scope			\
	library/menu/examples/menu_0				\
	lib/inliner/inliner_tr					\
	lib/rtchecks/rtchecks_tr

# 	library/menu/examples/menu_0			# Hang the compilation
#	library/class/examples/use_stack.pl		# Halt the compilation

# 	  lib/distutils/packages_access				\
# 	  lib/wp_gen/process_tmpl				\
#	  contrib/contextual/calculator.pl			\ # Halt the compilation

# 	  library/terms_check					\
# 	  contrib/chartlib/chartlib.pl				\
# 	  contrib/chartlib/genbar1.pl				\

#	  lib/engine/term_basic.pl			\ # =../2
# 	  library/toplevel/toplevel__scope.pl		\ # Fix assertions
# 	  lib/dict.pl					\ # Fix assertions
# 	  contrib/jvm_in_ciao/interpreter/binary_operations.pl \ # Fix assertions
# 	  contrib/jvm_in_ciao/interpreter/exception.pl	\ # $rtc0 undefined
# 	  contrib/jvm_in_ciao/interpreter/extended_lists.pl \ # $rtc0 undefined
# 	  contrib/jvm_in_ciao/interpreter/jvml.pl	\ # $rtc0 undefined
# 	  contrib/provrml/generator_util.pl 		\ # Fix assertions

nortcheckslibs: nortcheckslibs_$(RUNTIME_CHECKS)

nortcheckslibs_ nortcheckslibs_no:

nortcheckslibs_yes:
	$(BOLD_MESSAGE) "Compiling not runtime-checkable libraries"
	for file in $(NORTCHECKSLIBS)					\
	  ; do $(RM) $${file}.po $${file}.itf $${file}.asr		\
	; done
	for file in $(NORTCHECKSLIBS)					\
	  ; do $(NORMAL_MESSAGE) "Compiling '$${file}'" 		\
	  ; $(SETLOCALCIAO) $(BOOTSTRAP_CIAOC) -x -c 			\
		$(CIAOCOPTS_NORTCHECKS) $${file} 			\
	; done

compiler: nortcheckslibs
	cd ciaoc && $(GMAKE) compiler
# 	$(GMAKE) sysdep$(ALT)

# sysdep:

lpmake: nortcheckslibs
	@$(BOLD_MESSAGE) "Generating static 'lpmake' (for Ciao installation)"
	cd etc && $(SETLOCALCIAO) $(BOOTSTRAP_CIAOC) -s -x $(CIAOCOPTS) -o $(LPMAKE) lpmake
	chmod $(EXECMODE) $(LPMAKE)

applications: nortcheckslibs
	$(GMAKE) appsshell
	$(GMAKE) appsetc
	$(GMAKE) optimizing_compiler

appsshell:
	cd shell && $(GMAKE) all

appsetc:
	cd etc && $(LPMAKECMD) all

# Promote bootstrap compiler, DO NOT CALL DIRECTLY (only from ciaosetup)
promote_internal:
	cp ${BOOTSTRAP_CIAOC} "$(BOOTSTRAP_CIAOC)-`date +%Y%m%d%H%M%S`"
	cp $(CIAOC) $(BOOTSTRAP_CIAOC)

optimizing_compiler: optimizing_compiler_$(OPTIMIZING_COMPILER)

optimizing_compiler_yes:
	@$(BOLD_MESSAGE) "Building the optimizing compiler"
	cd $(CIAODESRC)/optim_comp && ./ciaotool build

optimizing_compiler_:

optimizing_compiler_no:
	@$(BOLD_MESSAGE) "Skipping compilation of the optimizing compiler"

libraries: nortcheckslibs
	$(LPMAKECMD) libraries

copysrcfiles: createsrcdir
	@(cd engine && for File in *.[ch] *.pl Makefile ; \
	do if [ ! -f $(OBJDIR)/$${File} -o $${File} -nt $(OBJDIR)/$${File} ]; \
             then rm -f $(OBJDIR)/$${File} ; cp $${File} $(OBJDIR)/$${File} ; \
	   fi ; \
	done ;)

$(SRCINCLUDEDIR):
	$(GMAKE) createincludedir

$(OBJDIRROOT)/$(CIAOARCH)$(CIAODEBUG):
	$(GMAKE) createsrcdir
	cd $(OBJDIR) &&	                   \
	   ln -s $(ENG_FROM_OBJDIR)/*.[ch] . &&   \
	   ln -s $(ENG_FROM_OBJDIR)/*.pl . &&   \
	   ln -s $(ENG_FROM_OBJDIR)/Makefile . && \
	   rm -f configure.h

createsrcdir:
	@$(BOLD_MESSAGE) "Creating source directory"
	-if test ! -d $(OBJDIRROOT) ; then \
	  mkdir -p $(OBJDIRROOT) ; \
	  chmod $(EXECMODE) $(OBJDIRROOT) ; \
        fi
	-if test ! -d $(OBJDIR) ; then \
	  mkdir -p $(OBJDIR) ; chmod $(EXECMODE) $(OBJDIR) ; fi

createincludedir:
	@$(BOLD_MESSAGE) "Creating include directory"
	if test ! -d $(SRCINCLUDEDIRROOT) ; then \
	  mkdir $(SRCINCLUDEDIRROOT) ; \
	  chmod $(EXECMODE) $(SRCINCLUDEDIRROOT) ; fi
	if test ! -d $(SRCINCLUDEDIR) ; then \
	  mkdir -p $(SRCINCLUDEDIR) ; chmod $(EXECMODE) $(SRCINCLUDEDIR) ; fi

version-ciao:
	-@rm -f $(OBJDIR)/version.h
	-@rm -f $(OBJDIR)/version.c
ifeq ($(DEBUG_LEVEL),nodebug)
	@(SHOW_DEBUG_LEVEL="" $(GMAKE) version.h version.c)
else
	@(SHOW_DEBUG_LEVEL=" [$(DEBUG_LEVEL)]" $(GMAKE) version.h version.c)
endif

version.h:
	@(umask 002; \
	echo '#define CIAO_MAJOR_VERSION ' `echo $(VERSION) | cut -f1 -d'.'` > $(OBJDIR)/version.h; \
	echo '#define CIAO_MINOR_VERSION ' `echo $(VERSION) | cut -f2 -d'.'` >> $(OBJDIR)/version.h; \
	echo '#define CIAO_PATCH_VERSION $(PATCH)' >> $(OBJDIR)/version.h; \
	echo '#define CIAO_SVNREV $(REVISION)' >> $(OBJDIR)/version.h )

	-@chmod $(DATAMODE) $(OBJDIR)/version.h
ifneq ($(INSTALLGROUP),)
	-@chgrp $(INSTALLGROUP) $(OBJDIR)/version.h
endif

version.c:
	@(umask 002; \
	echo 'char *emulator_version = "Ciao '$(VERSION)'.'$(PATCH)'-'$(REVISION)': '`date`''$(SHOW_DEBUG_LEVEL)'";' \
		> $(OBJDIR)/version.c; \
	echo 'char *emulator_architecture = "$(ARCHNAME)";'\
						       >> $(OBJDIR)/version.c;\
	echo 'char *emulator_os =    "$(OSNAME)";'     >> $(OBJDIR)/version.c;\
	echo 'char *emulator_debug = "$(CIAODEBUG)";'  >> $(OBJDIR)/version.c;\
	echo 'char *emulator_location = "$(ENGLOCATION)";'\
						       >> $(OBJDIR)/version.c;\
	echo 'char *ciao_version =   "$(VERSION)";'    >> $(OBJDIR)/version.c;\
	echo 'char *ciao_patch =     "$(PATCH)";'      >> $(OBJDIR)/version.c;\
	echo 'char *ciao_revision =  "$(REVISION)";'   >> $(OBJDIR)/version.c;\
	echo 'char *ciao_suffix =    "$(CIAOSUFFIX)";' >> $(OBJDIR)/version.c;\
	echo 'char *exec_suffix =    "$(EXECSUFFIX)";' >> $(OBJDIR)/version.c;\
	echo 'char *so_suffix =      "$(SOSUFFIX)";'   >> $(OBJDIR)/version.c;\
	echo 'char *default_lib_dir = "$(REALLIBDIR)";' >> $(OBJDIR)/version.c; \
	echo 'char *default_c_headers_dir = "$(CIAOHDIR)";' >> $(OBJDIR)/version.c )

	-chmod $(DATAMODE) $(OBJDIR)/version.c
ifneq ($(INSTALLGROUP),)
	-chgrp $(INSTALLGROUP) $(OBJDIR)/version.c
endif

fullinstalleng:
	$(GMAKE) engine
	$(GMAKE) installincludes
	$(GMAKE) installeng

installeng:
	$(BOLD_MESSAGE) "Installing engine for $(OSNAME)$(ARCHNAME)"
	-@(umask 002; mkdir -p $(BUILD_ROOT)$(REALLIBDIR) ; \
	 cd $(OBJDIR) && $(GMAKE) install LD=$(LD) \
	 CC=$(CC) CFLAGS='$(CFLAGS)' LDFLAGS='$(LDFLAGS)' LIBS=$(LIBS))

uninstalleng:
	$(BOLD_MESSAGE) "Uninstalling engine for $(OSNAME)$(ARCHNAME)"
#       Recreate the compilation directory in case it has been removed
	$(GMAKE) createsrcdir && \
	cd $(OBJDIR) &&	 \
	rm -f Makefile && \
	ln -s $(ENG_FROM_OBJDIR)/Makefile . && \
	$(GMAKE) uninstall

# todo: check INSTALLEDINCLUDEDIR and INCLUDEROOT
installincludes:
	@$(BOLD_MESSAGE) "Installing C include files for $(OSNAME)$(ARCHNAME)"
	-@(mkdir -p $(BUILDREALLIBDIR)/include; \
	chmod $(EXECMODE) $(BUILDREALLIBDIR)/include; \
	mkdir -p $(BUILD_ROOT)$(INSTALLEDINCLUDEDIR); \
	chmod $(EXECMODE) $(BUILD_ROOT)$(INSTALLEDINCLUDEDIR); \
	cp $(SRCINCLUDEDIR)/* $(BUILD_ROOT)$(INSTALLEDINCLUDEDIR); \
	chmod $(DATAMODE) $(BUILD_ROOT)$(INSTALLEDINCLUDEDIR)/*; \
	mkdir -p $(BUILD_ROOT)$(INCLUDEROOT); \
	rm -f $(BUILD_ROOT)$(INCLUDEROOT)/ciao_prolog.h; \
	ln -s $(INSTALLEDINCLUDEDIR)/ciao_prolog.h $(BUILD_ROOT)$(INCLUDEROOT); \
	)

# uninstallincludes:
# 	@$(BOLD_MESSAGE) "Uninstalling C include files for $(OSNAME)$(ARCHNAME)"
# 	-rm -rf $(BUILD_ROOT)$(INSTALLEDINCLUDEDIR)
# 	-rm  -f $(BUILD_ROOT)$(INCLUDEROOT)/ciao_prolog.h

# This option requires that lpdoc-2.0 are already installed

fullinstall:
	$(GMAKE) all
	$(GMAKE) docs
	$(GMAKE) install

# install:
# 	$(GMAKE) all
# 	$(GMAKE) justinstall

installsrc: reconfiguresrc

init_library:
	$(GMAKE) force_compile_options_auto
	$(GMAKE) config_source_components

config_source_components:
	$(GMAKE) build_config_source_components
	$(GMAKE) run_config_source_components

build_config_source_components:
	mkdir -p $(SRCBINDIR)
	$(SETLOCALCIAO) $(BOOTSTRAP_CIAOC) $(CIAOCOPTS_NORTCHECKS) -s -x -o $(CONFIG_SOURCE_COMPONENTS) \
		$(CIAODESRC)/makedir/config_source_components

run_config_source_components:
	$(SETLOCALCIAO) $(CONFIG_SOURCE_COMPONENTS) $(CIAODESRC)

force_compile_options_auto:
	$(GMAKE) compile_options_auto >> $(INSTALL_LOG)

# note: put $(MAKE) and not $(GMAKE), which may contain options!
compile_options_auto:
	cd lib && $(CIAOSRC)/etc/generate_foreign_compile_options $(MAKE) \
		$(CIAOSRC)/makefile-sysdep $(CIAOSRC)/lib/compile_options_auto.pl
	-chmod $(DATAMODE) $(CIAOSRC)/lib/compile_options_auto.pl

# (for automatic configuration)
config_prepare:
	if [ ! -x $(LPMAKE) ] || [ ! -f $(OBJDIR)/$(ENGINENAME) ] ; then \
		$(GMAKE) just_config_prepare ; \
	fi

# (for automatic configuration)
just_config_prepare:
	$(GMAKE) local_engine
	$(GMAKE) init_library
	$(GMAKE) local_lpmake
	$(BOLD_MESSAGE) "Ciao preparation done"

# (for automatic configuration)
local_lpmake:
	$(NORMAL_MESSAGE) "Creating static lpmake"
	$(GMAKE) lpmake >> $(INSTALL_LOG)

# (for automatic configuration)
local_engine:
	@$(NORMAL_MESSAGE) "Creating local engine"
	$(GMAKE) engine >> $(INSTALL_LOG)

do_menuconfig:
	$(GMAKE) config_source_components
	$(GMAKE) just_menuconfig

just_menuconfig:
	unset USE_THREADS USE_POSIX_LOCKS AND_PARALLEL_EXECUTION PAR_BACK \
                TABLED_EXECUTION OPTIM_LEVEL COMPRESS_LIB EXECMODE \
                DATAMODE DEBUG_LEVEL INSTALLGROUP; \
		$(LPMAKECMD) menuconfig
#	@$(GMAKE) init_library >> $(INSTALL_LOG)

menuconfig:
	$(GMAKE) config_prepare
	$(GMAKE) do_menuconfig
	$(GMAKE) englinks

autoconfig:
	(LPMAKEOPTS="-d CONFIGLEVEL=1" $(GMAKE) do_menuconfig)

reconfigure:
	$(LPMAKECMD) reconfigure

reconfiguresrc:
	$(LPMAKECMD) reconfiguresrc

unreconfigure:
	$(LPMAKECMD) unreconfigure

install:
	$(LPMAKECMD) justinstall reconfigure

justinstall:
	$(LPMAKECMD) justinstall

docs:
#	cd emacs-mode && $(GMAKE) docs
	$(LPMAKECMD) docs

docsemacs:
	$(LPMAKECMD) docsemacs

installdoc:
	$(LPMAKECMD) installdoc

uninstalletc:
	(cd etc && $(LPMAKECMD) uninstall)

uninstall:
	@$(BOLDER_MESSAGE) "Uninstalling Ciao"
	$(LPMAKECMD) uninstall
	$(GMAKE) uninstalleng
#	-rm -rf $(BUILDREALLIBDIR)
#	-rm -r $(LIBDIR)
	@$(BOLDER_MESSAGE) "Ciao uninstallation completed"

uninstalldoc:
	@$(LPMAKECMD) uninstalldoc
#	cd doc && $(GMAKE) uninstall

test:
	(cd examples/misc && $(GMAKE) distclean suite && $(GMAKE) exec_suite)

configclean:
	@rm -rf SETTINGS SETTINGS_AUTO lib/autoconfig/components \
		library/pillow/icon_address.pl
#	cd .. && ciao/etc/config_reset `pwd` ciao ciao

# clean: engclean
# #	cd ciaoc &&          $(GMAKE) clean
# 	cd lib &&            $(GMAKE) clean
# 	cd shell &&          $(GMAKE) clean
# #	cd emacs &&          $(GMAKE) clean
# 	cd emacs-mode &&     $(GMAKE) clean
# 	cd examples &&          $(GMAKE) clean
# 	$(GMAKE) cleangmon

# realclean:
# 	cd ciaoc && $(GMAKE) realclean
# 	cd lib && $(GMAKE) realclean
# 	cd shell && $(GMAKE) realclean
# #	cd emacs && $(GMAKE) realclean
# 	cd emacs-mode && $(GMAKE) realclean
# 	cd examples && $(GMAKE) realclean
# 	$(GMAKE) init_library
# 	$(LPMAKECMD) realcleandoc
# #	cd doc && $(GMAKE) realclean
# 	$(GMAKE) configclean
# 	$(GMAKE) engrealclean
# 	$(GMAKE) recursiveclean

tar:
	(cd .. ; tar cf - $(notdir $(CIAOSRC)) | gzip -c > ciao-$(VERSION).$(PATCH).tar.gz)

totalclean: cleanbackups distclean

engrealclean engclean: # clean_so_libs
	@$(BOLD_MESSAGE) "Removing engine for all architectures"
	-rm -rf $(OBJDIRROOT)
	-rm -rf $(SRCINCLUDEDIRROOT)

engcleanlocal: # clean_so_libs
	@$(BOLD_MESSAGE) "Removing engine for architecture $(CIAOARCH)"
	-rm -rf $(OBJDIR)
	-rm -rf $(SRCINCLUDEDIR)

# clean_so_libs:
# 	(cd library/sockets && $(GMAKE) realclean)
# 	(cd library/random && $(GMAKE) realclean)
# 	(cd library/concurrency && $(GMAKE) realclean)
# 	(cd library/apll && $(GMAKE) realclean)

# cleanbackups:
# 	(cd $(CIAOSRC) && find . -name '*~' -exec /bin/rm -f {} \;)
# 	(cd $(CIAOSRC) && find $(CIAOSRC) -name '#*' -exec /bin/rm -f {} \;)

# cleanmpp:
# 	(cd $(CIAOSRC) && find . -name '*.mpp' -exec /bin/rm -f {} \;)

# cleangmon:
# 	(cd $(CIAOSRC) && find . -name 'gmon.out' -exec /bin/rm -f {} \;)

# cleanasrs:
# 	(cd $(CIAOSRC) && find . -name '*.asr' -exec /bin/rm -f {} \;)

# cleanasts:
# 	(cd $(CIAOSRC) && find . -name '*.ast' -exec /bin/rm -f {} \;)

# clean_co:
# 	(cd $(CIAOSRC) && find . -name '*_co.pl' -exec /bin/rm -f {} \;)

# clean_opt:
# 	(cd $(CIAOSRC) && find . -name '*_opt.pl' -exec /bin/rm -f {} \;)

# clean_glue:
# 	(cd $(CIAOSRC) && find . -name '*_glue.c' -exec /bin/rm -f {} \;)

# clean_inline:
# 	(cd $(CIAOSRC) && find . -name '*_inline.c' -exec /bin/rm -f {} \;)

# clean_nodistribute:
# 	(cd $(CIAOSRC) && find . -name 'NODISTRIBUTE' -exec etc/remove_dirname {} \;)

# recursiveclean: cleangmon clean_co clean_glue clean_inline engclean cleanmpp clean_opt

# Note that to avoid bootstrap complications, the next will not delete
# the lpmake.sta file
# old_distclean: init_library
# 	@$(BOLD_MESSAGE) "Cleaning $(CIAOSRC) distribution tree (unix)"
# 	cd ciaoc && $(GMAKE) distclean
# 	cd shell && $(GMAKE) distclean
# 	cd etc && $(LPMAKECMD) distclean
# 	cd emacs-mode && $(GMAKE) distclean
# 	cd Win32 && $(GMAKE) distclean
# #	cd tests && $(GMAKE) distclean
# 	cd examples && $(GMAKE) distclean
# #	cd doc && $(LPMAKECMD) distclean
# 	$(CIAOSRC)/etc/recursive_make_or_clean $(CIAOSRC)/doc     "$(LPMAKEREADME)" \
# 		"$(LPMAKEINSTALL)" $(GMAKE) distclean
# 	$(CIAOSRC)/etc/recursive_make_or_clean $(CIAOSRC)/library "$(LPMAKEREADME)" \
# 		"$(LPMAKEINSTALL)" $(GMAKE) distclean
# 	$(CIAOSRC)/etc/recursive_make_or_clean $(CIAOSRC)/contrib "$(LPMAKEREADME)" \
# 		"$(LPMAKEINSTALL)" $(GMAKE) distclean
# 	$(CIAOSRC)/etc/recursive_make_or_clean $(CIAOSRC)/lib     "$(LPMAKEREADME)" \
# 		"$(LPMAKEINSTALL)" $(GMAKE) distclean
# 	@rm -f TAGS *.po *.itf *.asr
# 	$(GMAKE) recursiveclean
# 	$(GMAKE) configclean

cflow:
	(cd $(OBJDIR) && cflow -i -D$(CIAOARCH) *.c > $(CIAOSRC)/etc/cflow.out)

cxref:
	(cd $(OBJDIR) && cxref -xref-function -D$(ARCHNAME) -D$(OSNAME) \
		$(THREAD_FLAG) $(FOREIGN_FILES_FLAG) *.[ch] -O$(CIAOSRC)/etc/cxref)

mysqllibs:
	$(LPMAKECMD) mysqllibs

profilerlibs:
	$(LPMAKECMD) profilerlibs

profilerclean:
	(cd contrib/profiler && make -f ../../../Makefile recursiveclean)

get_setlocalciao:
	echo $(SETLOCALCIAO)
