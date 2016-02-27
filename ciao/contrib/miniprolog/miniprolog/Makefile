# -*- mode: Makefile; -*-
##
## Makefile
##  
## Made by Edison Mera
## Login   <edison@clip.dia.fi.upm.es>
##
## Started on  Tue Aug 28 16:55:54 2007 Edison Mera
## Last update Mon Dec 28 13:27:21 2009 Edison Mera
## 

ifndef MPARCH
MPARCH := $(shell $(CC) -dumpmachine)
endif

ifeq ($(shell uname -s),SunOS)
TAR=gtar
CMAKE=cmake
else

ifeq ($(shell uname -s),Linux)
CMAKE=cmake
else
ifeq ($(shell uname -s),Darwin)
CMAKE=cmake
else
ifeq ($(shell uname -o),Cygwin)
CMAKE=/usr/bin/cmake
EXECSUFFIX=.exe
else
CMAKE=cmake
endif
endif
endif

ifneq ($(shell which gnutar 2&>/dev/null),)
TAR=gnutar
else
TAR=tar
endif

endif


ifeq ($(MPARCH),win32vc)
BUILDCMD=$(SRCDIR)/buildvc.bat
else
ifeq ($(MPARCH),win32bcc)
BUILDCMD=$(SRCDIR)/buildbcc.bat
else
BUILDCMD=$(CMAKE) ../../src && $(MAKE) $(MFLAGS)
endif
endif

SRCDIR=$(CURDIR)

include SHARED

# HOSTNAME?=$(shell echo $${HOSTNAME})
HOSTNAME?=$(shell hostname)

TARGET?=_$(MPARCH)

MAKEREC=$(MAKE) MPARCH=$(MPARCH) EXECSUFFIX=$(EXECSUFFIX) \
	-f $(CURDIR)/Makefile.mk

default: all

# This hook allow us to remove easily the tests, benchmarks and maintenance 
# part from the distribution
-include Makefile-tests

.PHONY: $(PHONY) default all clean repclean svnclean realclean tests	\
	filelist pack showinfo stats binclean config common bin

all: all_$(CROSSCOMPILE)

all_yes: CROSSCOMPCMD="all binpack"
all_yes: crosscompcmd all2_yes

all2_yes: CROSSCOMPTAR=bin
all2_yes: crosscompget

crosscompcmd:
	echo "scratchbox -d $(CROSSCOMPDIR) $(MAKE) $(MFLAGS) $(CROSSCOMPCMD)" \
	  | $(SSH) $(CROSSCOMPUSER)@$(CROSSCOMPSERVER)

crosscompget:
	mkdir -p $(CROSSCOMPTAR)pack
	scp $(CROSSCOMPUSER)@$(CROSSCOMPSERVER):$(CROSSCOMPHOME)$(CROSSCOMPDIR)/$(CROSSCOMPTAR)pack/* $(CROSSCOMPTAR)pack/
	$(MAKE) $(CROSSCOMPTAR)unpack

all_:
	@echo {generating bin files in $(HOSTNAME), please wait ...}
	mkdir -p bin/_$(MPARCH)
	cd bin/_$(MPARCH) && $(BUILDCMD)
	@echo {finished bin generation in $(HOSTNAME)}

repclean svnclean:
	$(RM) -r `svn st $(CURDIR) --no-ignore | grep "I      " \
	  | sed -e s:"I      "::g`

binclean:
	$(RM) -r bin

commonclean:
	$(RM) -r common

realclean clean: binclean commonclean $(EXTRACLEAN) crossclean_$(CROSSCOMPILE)
	$(RM) -r REVISION miniprolog.tar filelist.txt tests/*.po *.itf
	-find . -name "*~" -exec $(RM) {} \;


crossclean_yes:
	$(MAKE) CROSSCOMPCMD=clean crosscompcmd

crossclean_:

run:
	$(WAM) -t -C tests/ -C tests/calibs/ -C benchcommon/_all/

stats:
	-@echo "WAMSize=`stat -c %s bin/_$(MPARCH)/wam`" > statistics/$(MPARCH).txt || $(RM) statistics/$(MPARCH).txt
	@(echo "consult('tests/fib.pl')."; \
	  echo "hrtime(_T1U,_T1L), fib(20,N), hrtime(_T2U,_T2L), sub2(_T2U,_T2L,_T1U,_T1L,_TU,TimeFib)."; \
	  echo; \
	  echo "consult('tests/hanoi.pl')."; \
	  echo "hrtime(_T1U,_T1L), hanoi(8,a,b,c,_N), hrtime(_T2U,_T2L), sub2(_T2U,_T2L,_T1U,_T1L,_TU,TimeHanoi)."; \
	  echo; \
	  echo "halt.")| \
	bin/_$(MPARCH)/wam -s >> statistics/$(MPARCH).txt
	@(echo "consult('tests/nreverse.pl')."; \
	  echo "hrtime(_T1U,_T1L), length(X,20), nreverse(X,Y),hrtime(_T2U,_T2L), sub2(_T2U,_T2L,_T1U,_T1L,_TU,TimeRev)."; \
	  echo; \
	  echo "halt.")| \
	bin/_$(MPARCH)/wam -s >> statistics/$(MPARCH).txt
	-svn diff statistics/$(MPARCH).txt
#	@echo "time of fib(25,N): " >> Statistics.txt

filelist:
	( for file in `svn ls -R` ; do \
	    if [ -f $${file} ] ; then \
	      echo miniprolog/$${file}; \
	    fi; \
	  done ) > filelist.txt
	svnversion > REVISION
	echo "miniprolog/REVISION" >> filelist.txt

pack: filelist
	$(TAR) --owner=0 --group=0 --directory=.. --files-from=filelist.txt -cf \
	  miniprolog.tar

showinfo:
	@echo MPARCH=$(MPARCH)
	@echo EXECSUFFIX=$(EXECSUFFIX)
	@echo MAKEOPTS=$(MAKEOPTS)
	@$(MAKEREC) TARGET=_$(MPARCH) OBJBASE=bin   showinfo
	@$(MAKEREC) TARGET=_$(MPARCH) OBJBASE=bench showinfo

packdir:
	mkdir -p $(AUTODIR)pack
	$(TAR) --owner=0 --group=0 --directory=$(AUTODIR) -czf \
	   $(AUTODIR)pack/$(TARGET).tar.gz $(TARGET)

unpackdir:
	mkdir -p $(AUTODIR)
	$(TAR) --directory=$(AUTODIR) -xzf $(AUTODIR)pack/$(TARGET).tar.gz

binpack:
	$(MAKE) AUTODIR=bin packdir

binunpack:
	$(MAKE) AUTODIR=bin unpackdir
