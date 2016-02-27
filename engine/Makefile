# Copyright (C) 1996, UPM-CLIP

# Do NOT use tabs for the commands inside if/endif!
#
# ifneq (,$(findstring FOREIGN_FILES, $(CFLAGS)))
#     FOREIGN=foreign.o
# endif

SHELL=/bin/sh
DEBUGFILES=cmnd.o help.o interface.o string.o instr2name.o qinstr2name.o
BUILTINS=wam.o kernel.o  format.o inout.o misc.o startgoal.o \
	 nondet.o objareas.o streams.o timing.o wambuiltin.o wamfunction.o \
	 unix_utils.o attr.o dynlink.o tasks.o locks.o prolog_tasks.o \
	 own_malloc_linear.o
SUPPORTS=alloc.o heapgc.o indexing.o initial.o registers.o  \
	stacks.o support.o term_support.o wamsupport.o \
	qread.o qget.o builtin.o interrupt.o gauge.o \
	bignum.o profile.o
CIAOOBJ=ciao.o
MAIN_FILES=main.o version.o
MAIN_EXEC=main_exec.o version.o qinsert.o
DATADEFS=datadefs.h configure.h alloc.h registers.h termdefs.h access.h locks.h objareas.h
SUPPORT=support.h debug.h initial.h compat.h
CCOMPILATIONSUPPORT=

include ../../SETTINGS
include ../../COMMON

include $(SRC)/makefile-sysindep
include $(SYSDEP_FILES)/mkf-$(CIAOARCH)

OBJ_EXEC= $(BUILTINS) $(SUPPORTS) $(CIAOOBJ) $(ADDOBJ) $(MAIN_EXEC)
OBJ= $(BUILTINS) $(SUPPORTS) $(CIAOOBJ) $(ADDOBJ) $(MAIN_FILES)
DOBJ= $(BUILTINS) $(SUPPORTS) $(CIAOOBJ) $(ADDOBJ) $(DEBUGFILES) $(MAIN_FILES)


install: # $(ENGINENAME)
	-mkdir -p $(ENGINEDIR)
	-rm -f $(INSTALLEDENGINE).$(CIAOARCH)
	-rm -f $(INSTALLEDENGINE)
	-cp $(ENGINENAME) $(INSTALLEDENGINE).$(CIAOARCH)
	-ln -s $(INSTALLEDENGINE).$(CIAOARCH) $(INSTALLEDENGINE)
	-chmod $(EXECMODE) $(ENGINEDIR)
	-chmod $(EXECMODE) $(INSTALLEDENGINE)
	-cp $(DATADEFS) $(SUPPORT) $(INSTALLEDINCLUDEDIR)
	-chmod $(DATAMODE) $(INSTALLEDINCLUDEDIR)/*

uninstall:
	-rm -f $(INSTALLEDENGINE)

$(ENGINENAME): configure.h ciaoemulator

#$(STATENGINENAME): configure.h statciaoemulator

ciaoemulator: $(OBJ)
	$(CC)  $(LDFLAGS) $(OBJ) $(CURRLIBS) -o $(ENGINENAME)
	chmod $(EXECMODE) $(ENGINENAME)
	if test ! -f $(SRCINCLUDEDIR)/configure.h ; then \
	  for f in $(DATADEFS) $(SUPPORT) $(CCOMPILATIONSUPPORT); do \
	    ln -s $(OBJDIR)/$$f $(SRCINCLUDEDIR); \
	  done ; \
        fi

clean:
	/bin/rm -f makecase configure qinstr2name.h instr2name.h \
	version.c configure.h *.o kernel_con kernel.con \
	def_kernel.c 68k_kernel.s

realclean:
	/bin/rm -f $(ENGINENAME) $(CIAOLIBNAME)

distclean:
	/bin/rm -f *~

libciao: mode sh_object

sh_object: $(OBJ_EXEC) flinkage.o
	$(LD) $(LDSHARED) -o $(CIAOLIBNAME) $(OBJ_EXEC) flinkage.o $(LIBS)

configure.h: configure
	/bin/rm -f configure.h
	./configure "$(CFLAGS)" > configure.h

crossconfigure.h: configure
	/bin/rm -f configure.h
	./configure " -DSTATICENG -DcrossWin32i86" > configure.h

configure: configure.c compat.h termdefs.h own_malloc_defs.h
	$(CC) $(CFLAGS) $(LDFLAGS) -o configure configure.c

instr2name.h: instrdefs.h makecase
	/bin/rm -f instr2name.h
	./makecase <instrdefs.h > instr2name.h

qinstr2name.h: qinstrdefs.h makecase
	/bin/rm -f qinstr2name.h
	./makecase <qinstrdefs.h >qinstr2name.h

makecase: makecase.c
	$(CC) $(CFLAGS) -o makecase makecase.c


alloc.o: $(DATADEFS) $(SUPPORT) compat.h own_malloc_defs.h alloc_defs.h streams_defs.h wamsupport_defs.h
attr.o: $(DATADEFS)  $(SUPPORT) predtyp.h attr_defs.h stacks_defs.h
builtin.o: $(DATADEFS) $(SUPPORT) predtyp.h builtin_defs.h wam_defs.h
cmnd.o: $(DATADEFS) instrdefs.h predtyp.h interface.h commands.h compat.h
dynlink.o: $(DATADEFS) $(SUPPORT) dynlink_defs.h
# foreign.o: $(DATADEFS) $(SUPPORT) compat.h foreign_defs.h alloc_defs.h
format.o: $(DATADEFS) $(SUPPORT) format_defs.h wamfunction_defs.h term_support_defs.h
heapgc.o: $(DATADEFS) $(SUPPORT) gcdatadefs.h gcsupport.h threads.h heapgc_defs.h stacks_defs.h
help.o: commands.h
indexing.o: $(DATADEFS) $(SUPPORT) predtyp.h indexing_defs.h wamsupport_defs.h support_defs.h alloc_defs.h objareas_defs.h
initial.o: $(DATADEFS) $(SUPPORT) addmodules.c predtyp.h wamfunction.h wambuiltin.h threads.h task_areas.h  initial_defs.h interrupt_defs.h unix_utils_defs.h term_support_defs.h
inout.o: $(DATADEFS) $(SUPPORT) ciao.h term_support_defs.h inout_defs.h streams_defs.h tasks_defs.h
instr2name.o: instr2name.h $(DATADEFS)
interface.o: $(DATADEFS) $(SUPPORT) interface.h predtyp.h
interrupt.o: $(DATADEFS) $(SUPPORT) compat.h interrupt_defs.h tasks_defs.h
kprim.o: $(DATADEFS) kprim.h instrdefs.h
locks.o: $(DATADEFS) $(SUPPORT) alloc.h locks.h locks_defs.h
main.o: main.h debug.h initial.h threads.h locks.h  wam_defs.h tasks_defs.h unix_utils_defs.h initial_defs.h main_defs.h qread_defs.h builtin_defs.h alloc_defs.h support_defs.h own_malloc_defs.h profile_defs.h
main_exec.o: $(DATADEFS)
misc.o: $(DATADEFS) $(SUPPORT) predtyp.h threads.h tasks_defs.h misc_defs.h bignum_defs.h stacks_defs.h term_support_defs.h
nondet.o: $(DATADEFS) $(SUPPORT) predtyp.h  nondet_defs.h stacks_defs.h
objareas.o: $(DATADEFS) $(SUPPORT) predtyp.h objareas_defs.h stacks_defs.h indexing_defs.h alloc_defs.h nondet_defs.h
own_malloc_bin.o: configure.h termdefs.h own_malloc_defs.h
own_malloc_linear.o: configure.h termdefs.h own_malloc_defs.h
prolog_tasks.o: $(DATADEFS) $(SUPPORT) threads.h task_areas.h  wam_defs.h prolog_tasks_defs.h tasks_defs.h
profile.o: $(DATADEFS) initial.h predtyp.h profile_defs.h
qget.o: $(DATADEFS) $(SUPPORT) threads.h qget_defs.h qread_defs.h bignum_defs.h
qinsert.o: $(DATADEFS) $(SUPPORT) qinstrdefs.h threads.h qinsert_defs.h bignum_defs.h stacks_defs.h
qinstr2name.o: qinstr2name.h
qread.o: $(DATADEFS) $(SUPPORT) qinstrdefs.h qread_defs.h alloc_defs.h qget_defs.h stacks_defs.h
registers.o: $(DATADEFS)
ciao.o: ciao.h ENG_defs.h
stacks.o: $(DATADEFS) $(SUPPORT) threads.h stacks_defs.h heapgc_defs.h
streams.o: $(DATADEFS) $(SUPPORT) alloc_defs.h
string.o: $(DATADEFS)
support.o: $(DATADEFS) $(SUPPORT) predtyp.h  support_defs.h alloc_defs.h stacks_defs.h bignum_defs.h
tasks.o: $(DATADEFS) $(SUPPORT) configure.h threads.h task_areas.h  tasks_defs.h initial_defs.h
# threads.o: $(DATADEFS) $(SUPPORT) threads.h
term_support.o: $(DATADEFS) $(SUPPORT) instrdefs.h threads.h task_areas.h term_support_defs.h
timing.o: $(DATADEFS) $(SUPPORT) timing_defs.h
unix_utils.o: $(DATADEFS) $(SUPPORT) compat.h unix_utils_defs.h streams_defs.h stacks_defs.h
version.o:
startgoal.o: $(DATADEFS) $(SUPPORT) wam.h threads.h task_areas.h \
	initial_defs.h inout_defs.h main_defs.h prolog_tasks_defs.h \
	startgoal_defs.h tasks_defs.h term_support_defs.h wam_defs.h
wam.o: $(DATADEFS) $(SUPPORT) \
	wam.h u2defs.h instrdefs.h predtyp.h \
	shdisp_r.c shdisp_w.c \
	u1disp_r.c u2void_r.c u2xvar_r.c u2yvar_r.c u2xval_r.c u2yval_r.c \
	u1disp_w.c u2void_w.c u2xvar_w.c u2yvar_w.c u2xval_w.c u2yval_w.c \
	wamgauge_w.c wamgauge_r.c task_areas.h\
	attr_defs.h builtin_defs.h initial_defs.h \
	inout_defs.h interrupt_defs.h main_defs.h misc_defs.h nondet_defs.h\
	objareas_defs.h stacks_defs.h support_defs.h \
	term_support_defs.h wam_defs.h profile_defs.h
wambuiltin.o: $(DATADEFS) $(SUPPORT) wambuiltin.h wambuiltin_defs.h misc_defs.h bignum_defs.h
wamfunction.o: $(DATADEFS) $(SUPPORT) wamfunction.h wamfunction_defs.h
wamsupport.o: $(DATADEFS) $(SUPPORT) instrdefs.h wamsupport_defs.h
gauge.o: $(DATADEFS) $(SUPPORT) predtyp.h gauge_defs.h
bignum.o: $(DATADEFS) $(SUPPORT)
flinkage.o:

kernel.o:	$(DATADEFS) kernel_con.c Makefile
	$(MAKE) def_kernel.o $(MFLAGS) CC=$(CC) CFLAGS='$(CFLAGS)'
	cp def_kernel.o kernel.o;

def_kernel.c: Makefile
	/bin/rm -f def_kernel.c
	echo 'static char ignore;' >> def_kernel.c

def_kernel.o:

kernel_con: kernel_con.c $(DATADEFS)
	$(CC) $(CFLAGS) kernel_con.c -o kernel_con

kernel.con: kernel_con
	/bin/rm -f kernel.con
	./kernel_con > kernel.con

# This is for Win32
sockets_c.o:
	cd ../../library/sockets ; $(MAKE) sockets_c_$(CIAOARCH).o
	cp ../../library/sockets/sockets_c_$(CIAOARCH).o ./sockets_c.o

random.o:
	cd ../../library/random ; $(MAKE) random_$(CIAOARCH).o
	cp ../../library/random/random_$(CIAOARCH).o ./random.o

touch:
	touch *.[ch]

tags:
	$(ETAGS)

mode:
	umask 002
