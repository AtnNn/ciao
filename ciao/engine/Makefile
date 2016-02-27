# -*- mode: Makefile; -*-
# Copyright (C) 1996, UPM-CLIP

# Do NOT use tabs for the commands inside if/endif!
#
# ifneq (,$(findstring FOREIGN_FILES, $(CFLAGS)))
#     FOREIGN=foreign.o
# endif

# TODO: put a name to this relative path
CIAOBUILDDIRtemp=../../../build
include $(CIAOBUILDDIRtemp)/CONFIG_mkf

# (Helper defs for messages)
BOLD_MESSAGE=$(CIAODESRC)/makedir/mkf_aux bold_message
NORMAL_MESSAGE=$(NORMAL_MESSAGE_$(SILENT))
NORMAL_MESSAGE_false=$(CIAODESRC)/makedir/mkf_aux normal_message
NORMAL_MESSAGE_true=true

SHELL=/bin/sh
OBJFILES=wam.o format.o inout.o misc.o startgoal.o ciao.o \
	 nondet.o objareas.o streams.o timing.o wambuiltin.o wamfunction.o \
	 unix_utils.o attr.o dynlink.o tasks.o locks.o \
	 own_malloc_bin.o own_mmap.o alloc.o heapgc.o indexing.o initial.o \
	 registers.o stacks.o support.o term_support.o wamsupport.o \
	 qread.o qget.o builtin.o interrupt.o gauge.o bignum.o \
	 float_consts.o float_tostr.o \
	 ciao_prolog.o start.o version.o 

MAIN_FILE=main.o 
#MAIN_EXEC=main_exec.o version.o qinsert.o

DATADEFS=datadefs.h version.h configure.h alloc.h registers.h termdefs.h access.h locks.h objareas.h
SUPPORT=support.h debug.h initial.h compat.h ciao_prolog.h ciao_gluecode.h \
	threads.h task_areas.h timing_defs.h predtyp.h instrdefs.h \
	profile_defs.h alloc_defs.h support_defs.h builtin_defs.h tabling.h

CCOMPILATIONSUPPORT=term_support_defs.h wam.h wam_defs.h tasks_defs.h \
	startgoal_defs.h nondet_defs.h start_defs.h stacks_defs.h \
	startgoal_defs.h streams_defs.h

FIX_SIZE_EXEC:=$(OBJDIR)/fix_size$(EXECSUFFIX)

CIAOLIBBASE:=lib$(MAIN)
# Name of the standard ciao library
CIAOLIBNAME:=$(CIAOLIBBASE)$(SOSUFFIX)

.PHONY: cc_message ld_message

OBJSFORLIB=$(OBJFILES) $(ADDOBJ)
OBJ=$(OBJSFORLIB) $(MAIN_FILE)

CONFIGURESRC = configure_main.c configure.c \
	       compat.h termdefs.h own_malloc_defs.h \
	       own_mmap.h own_mmap.c

#ifeq ($(OSNAME),Solaris)
#  LIBSTA=-lsocket
#else
#  LIBSTA=
#endif

cc_message:
	@$(NORMAL_MESSAGE) "C compiler flags: $(CFLAGS)"
ld_message:
	@$(NORMAL_MESSAGE) "Linker flags: $(CONSOLEFLAG) $(LDFLAGS) $(CURRLIBS)"

.c.o:
	@$(NORMAL_MESSAGE) "Compiling $<"
	@$(CC) $(CFLAGS) -c -o $@ $<

## We produce three files: 
### the combined .o file (for static linking),
### the .so file (for dynamic linking), and
### the executable itself

ENGINE_COMPILATION_FLAGS=$(CIAOSRC)/library/engine_compilation_flags_auto.pl

$(ENGINENAME): configure.h version.h $(ENGINE_COMPILATION_FLAGS) ciaoemulator
	@true

# fix_size: it does not depend on the architecture; it is executed locally,
# so it has to be compiled in the local (host) architecture
fix_size_exec: # $(FIX_SIZE_EXEC)
	-@mkdir -p $(SRCBINDIR)
	-@cd $(CIAOSRC)/etc && $(LCC) -o $(FIX_SIZE_EXEC) fix_size.c


# The emulator is built from all the .o files

ciaoemulator: dociaoemulator$(ENGLOCATION)

dociaoemulator: fix_size_exec cc_message ld_message $(OBJ)
	@$(NORMAL_MESSAGE) "Linking $(ENGINENAME)"
	@$(CC) $(CONSOLEFLAG) $(LDFLAGS) $(OBJ) $(CURRLIBS) -o $(ENGINENAME)
	@$(MAKE) ciaoemulator_common

dociaoemulatordyn: fix_size_exec
	@$(NORMAL_MESSAGE) "Linking dynamic $(ENGINENAME)"
	@$(MAKE) $(CIAOLIBNAME);
	@$(CC) $(CONSOLEFLAG) $(LDFLAGS) $(MAIN_FILE) $(CURRLIBS) -L./ -l $(CIAOLIBBASE) -o $(ENGINENAME);
	@$(MAKE) ciaoemulator_common

ciaoemulator_common:
	@-chmod $(DATAMODE) *.o
	@$(FIX_SIZE_EXEC) $(ENGINENAME)
	@-chmod $(EXECMODE) $(ENGINENAME)
ifneq ($(INSTALLGROUP),)
	@-chgrp $(INSTALLGROUP) *.o
	@-chgrp $(INSTALLGROUP) $(ENGINENAME)
endif
	@if test ! -f $(SRCINCLUDEDIR)/configure.h ; then \
	  for f in $(DATADEFS) $(SUPPORT) $(CCOMPILATIONSUPPORT); do \
	    ln -f -s $(OBJDIR)/$$f $(SRCINCLUDEDIR); \
	  done ; \
        fi

# The archive to be linked with other programs

$(CIAOLIBBASE): $(CIAOLIBNAME) $(CIAOLIBBASE).a

$(CIAOLIBNAME): cc_message ld_message $(OBJ)
	@$(LD) $(LDSHARED) $(OBJFILES) -o $(CIAOLIBNAME) $(CURRLIBS)

$(CIAOLIBBASE).a: $(OBJFILES)
	@ar -r $(CIAOLIBBASE).a $(OBJFILES)
	-@chmod $(DATAMODE) $(CIAOLIBBASE).a

# The big ciaoengine in object format

ciaoobject: $(OBJFILES)
	@$(LD) $(LDCOMBINE) $(OBJFILES) -o $(CIAOLIBBASE).o

clean:
	@/bin/rm -f makecase configure \
		version.c version.h configure.h *.o \
		metaengine metaengine.po metaengine.itf
#	kernel_con kernel.con 
#	def_kernel.c 68k_kernel.s

realclean:
	@/bin/rm -f $(ENGINENAME) $(ENGINENAME).o $(CIAOLIBNAME)

distclean:
	@/bin/rm -f *~
	@/bin/rm -f TAGS
	@/bin/rm -f depends

configure.h: configure 
	@/bin/rm -f configure.h
	@./configure "$(CFLAGS)" > configure.h


crossconfigure.h: configure
	@/bin/rm -f configure.h
	@/usr/bin/scp configure $(CROSS_COMPILER_HOST):/tmp && \
	@/usr/bin/ssh $(CROSS_COMPILER_HOST) /tmp/configure "$(CFLAGS)" > configure.h

configure:
	@$(MAKE) doconfigure$(ENGLOCATION)

$(ENGINE_COMPILATION_FLAGS): compilation_options.c
	@$(CC) compilation_options.c -o compilation_options
	@./compilation_options $(CC) $(CFLAGS) > $(ENGINE_COMPILATION_FLAGS)

doconfigure: $(CONFIGUREOBJ)
	@$(CC) $(CFLAGS) $(LDFLAGS) -o configure configure.c own_mmap.c configure_main.c

# This is a kludge to allow configure get the correct MallocBase if
# the engine is in a dynamic library
doconfiguredyn: $(CONFIGUREOBJ)
	@$(LD) $(LDSHARED) $(CFLAGS) $(LDFLAGS) -c configure.c
	@$(LD) $(LDSHARED) $(CFLAGS) $(LDFLAGS) -c own_mmap.c
	@$(LD) $(LDSHARED) -o libconfigure$(SOSUFFIX) configure.o own_mmap.o
	@$(CC) $(CFLAGS) $(LDFLAGS) -L./ -l libconfigure -o configure configure_main.c

configure.o:

alloc.o: $(DATADEFS) $(SUPPORT) compat.h own_malloc_defs.h alloc_defs.h streams_defs.h wamsupport_defs.h
attr.o: $(DATADEFS)  $(SUPPORT) predtyp.h attr_defs.h stacks_defs.h
builtin.o: $(DATADEFS) $(SUPPORT) predtyp.h builtin_defs.h wam_defs.h profile_defs.h
cmnd.o: $(DATADEFS) instrdefs.h predtyp.h interface.h commands.h compat.h
dynlink.o: $(DATADEFS) $(SUPPORT) dynlink_defs.h
format.o: $(DATADEFS) $(SUPPORT) format_defs.h wamfunction_defs.h term_support_defs.h
float_consts.o: float_consts.h
float_tostr.o: float_tostr.h float_consts.h
heapgc.o: $(DATADEFS) $(SUPPORT) gcdatadefs.h gcsupport.h threads.h heapgc_defs.h stacks_defs.h
help.o: commands.h
indexing.o: $(DATADEFS) $(SUPPORT) predtyp.h indexing_defs.h wamsupport_defs.h support_defs.h alloc_defs.h objareas_defs.h stacks_defs.h bignum_defs.h locks_defs.h start_defs.h initial_defs.h

initial.o: $(DATADEFS) $(SUPPORT) addmodules.c predtyp.h wamfunction.h wambuiltin.h threads.h task_areas.h  initial_defs.h interrupt_defs.h unix_utils_defs.h term_support_defs.h support_defs.h timing_defs.h locks_defs.h streams_defs.h indexing_defs.h

inout.o: $(DATADEFS) $(SUPPORT) ciao.h term_support_defs.h inout_defs.h streams_defs.h tasks_defs.h
interface.o: $(DATADEFS) $(SUPPORT) interface.h predtyp.h
interrupt.o: $(DATADEFS) $(SUPPORT) compat.h interrupt_defs.h tasks_defs.h
kprim.o: $(DATADEFS) kprim.h instrdefs.h
locks.o: $(DATADEFS) $(SUPPORT) alloc.h locks.h locks_defs.h
main.o: main.h debug.h initial.h threads.h locks.h  wam_defs.h tasks_defs.h unix_utils_defs.h initial_defs.h start_defs.h qread_defs.h builtin_defs.h alloc_defs.h support_defs.h own_malloc_defs.h profile_defs.h
main_exec.o: $(DATADEFS)
misc.o: $(DATADEFS) $(SUPPORT) predtyp.h threads.h tasks_defs.h misc_defs.h bignum_defs.h stacks_defs.h term_support_defs.h timing_defs.h profile_defs.h
nondet.o: $(DATADEFS) $(SUPPORT) predtyp.h  nondet_defs.h stacks_defs.h
objareas.o: $(DATADEFS) $(SUPPORT) predtyp.h objareas_defs.h stacks_defs.h indexing_defs.h alloc_defs.h nondet_defs.h
own_malloc_bin.o: configure.h termdefs.h own_malloc_defs.h
own_malloc_linear.o: configure.h termdefs.h own_malloc_defs.h
own_mmap.o : own_mmap.c own_mmap.h
qget.o: $(DATADEFS) $(SUPPORT) threads.h qget_defs.h qread_defs.h bignum_defs.h
qinsert.o: $(DATADEFS) $(SUPPORT) qinstrdefs.h threads.h qinsert_defs.h bignum_defs.h stacks_defs.h
qread.o: $(DATADEFS) $(SUPPORT) qinstrdefs.h qread_defs.h alloc_defs.h qget_defs.h stacks_defs.h
registers.o: $(DATADEFS)
ciao.o: ciao.h ENG_defs.h
stacks.o: $(DATADEFS) $(SUPPORT) threads.h stacks_defs.h heapgc_defs.h
start.o: profile_defs.h
streams.o: $(DATADEFS) $(SUPPORT) alloc_defs.h support_defs.h initial_defs.h
string.o: $(DATADEFS)
support.o: $(DATADEFS) $(SUPPORT) predtyp.h  support_defs.h alloc_defs.h stacks_defs.h bignum_defs.h profile_defs.h
tasks.o: $(DATADEFS) $(SUPPORT) configure.h threads.h task_areas.h  tasks_defs.h initial_defs.h
# threads.o: $(DATADEFS) $(SUPPORT) threads.h
term_support.o: $(DATADEFS) $(SUPPORT) instrdefs.h threads.h task_areas.h term_support_defs.h
timing.o: $(DATADEFS) $(SUPPORT) timing_defs.h
unix_utils.o: $(DATADEFS) $(SUPPORT) compat.h unix_utils_defs.h streams_defs.h stacks_defs.h
version.o:
startgoal.o: $(DATADEFS) $(SUPPORT) wam.h threads.h task_areas.h \
	initial_defs.h inout_defs.h start_defs.h \
	startgoal_defs.h tasks_defs.h term_support_defs.h wam_defs.h
wam.o: $(DATADEFS) $(SUPPORT) \
	wam.h instrdefs.h predtyp.h \
	wamread.c wamwrite.c \
	task_areas.h\
	attr_defs.h builtin_defs.h initial_defs.h \
	inout_defs.h interrupt_defs.h start_defs.h misc_defs.h nondet_defs.h\
	objareas_defs.h stacks_defs.h support_defs.h \
	term_support_defs.h wam_defs.h profile_defs.h
ciao_prolog.o: $(DATADEFS) $(SUPPORT) \
	wam.h instrdefs.h predtyp.h \
	wamread.c wamwrite.c \
	task_areas.h\
	attr_defs.h builtin_defs.h initial_defs.h \
	inout_defs.h interrupt_defs.h start_defs.h misc_defs.h nondet_defs.h\
	objareas_defs.h stacks_defs.h support_defs.h \
	term_support_defs.h wam_defs.h profile_defs.h
wambuiltin.o: $(DATADEFS) $(SUPPORT) wambuiltin.h wambuiltin_defs.h misc_defs.h bignum_defs.h
wamfunction.o: $(DATADEFS) $(SUPPORT) wamfunction.h wamfunction_defs.h
wamsupport.o: $(DATADEFS) $(SUPPORT) instrdefs.h wamsupport_defs.h
gauge.o: $(DATADEFS) $(SUPPORT) predtyp.h gauge_defs.h
bignum.o: $(DATADEFS) $(SUPPORT)

# hashtab.o: hashtab.h recycle.h standard.h lookupa.h
# lookupa.o: lookupa.h standard.h
# recycle.o: recycle.h standard.h

# kernel.o:	$(DATADEFS) kernel_con.c Makefile
# 	$(MAKE) def_kernel.o $(MFLAGS) CC=$(CC) CFLAGS='$(CFLAGS)'
# 	cp def_kernel.o kernel.o;

# def_kernel.c: Makefile
# 	/bin/rm -f def_kernel.c
# 	echo 'static char ignore;' >> def_kernel.c

# def_kernel.o:

# kernel_con: kernel_con.c $(DATADEFS)
# 	$(CC) $(CFLAGS) kernel_con.c -o kernel_con

# kernel.con: kernel_con
# 	/bin/rm -f kernel.con
# 	./kernel_con > kernel.con

# This is for Win32
# sockets_c.o:
# 	rm -rf $(CIAOSRC)/library/sockets/sockets_c_$(CIAOARCH)_glue.o
# 	cd $(CIAOSRC)/library/sockets && $(MAKE) sockets_c_$(CIAOARCH)_glue.o
# 	cp $(CIAOSRC)/library/sockets/sockets_c_$(CIAOARCH)_glue.o ./sockets_c.o

# random.o:
# 	rm -rf $(CIAOSRC)/library/random/random_$(CIAOARCH)_glue.o
# 	cd $(CIAOSRC)/library/random && $(MAKE) random_$(CIAOARCH)_glue.o
# 	cp $(CIAOSRC)/library/random/random_$(CIAOARCH)_glue.o ./random.o

# Needs external libs in windows!
# mysql_client.o:
# 	cp ../../library/persdb_mysql/mysql_client.c .
# 	$(CC) $(CFLAGS) -c mysql_client.c

#fastrw.o:
#	cd ../../library/fastwr ; $(MAKE) fastrw_$(CIAOARCH).o
#	cp ../../library/fastrw/fastrw_$(CIAOARCH).o ./fastrw.o

## JF
#instrdefs.h wamread.c wamwrite.c: metaengine
#	./metaengine
#
#metaengine: metaengine.pl
#	ciaoc metaengine.pl

touch:
	@touch *.[ch]

tags:
	@$(ETAGS) *.[ch]

mode:
	@umask 002
