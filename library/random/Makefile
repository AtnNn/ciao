MODULE=random

include ../../SETTINGS
include ../../COMMON
include $(SRC)/makefile-sysindep
include $(SYSDEP_FILES)/mkf-$(CIAOARCH)

REQLIBS=

SRCNAME=$(MODULE).c
OBJNAME=$(MODULE)_$(OSNAME)$(ARCHNAME).o
LIBNAME=$(MODULE)_$(OSNAME)$(ARCHNAME).so
HEADERS=common_headers.h

$(LIBNAME): headers $(OBJNAME) 
	$(LD) $(LDSHARED) -o $(LIBNAME) $(OBJNAME) $(REQLIBS)
	chmod $(EXECMODE) $(LIBNAME)

$(OBJNAME): headers $(SRCNAME)
	$(CC) $(CFLAGS) $(CCSHARED) -c $(SRCNAME) -o $(OBJNAME)

headers:
	/bin/rm -f  $(HEADERS)
	echo \#include \"$(SRC)/bin/$(CIAOARCH)/datadefs.h\" > $(HEADERS)
	echo \#include \"$(SRC)/bin/$(CIAOARCH)/support.h\" >> $(HEADERS)
	chmod $(DATAMODE) $(HEADERS)

clean:
	/bin/rm -f $(OBJNAME)

realclean: clean
	/bin/rm -f $(LIBNAME) $(HEADERS)

distclean:
	/bin/rm -f *.~ $(HEADERS) $(MODULE)_*

allarchs:
	@for machine in $(REMOTEMACHINES); do \
		echo ; \
		echo -------------------------------------------- ; \
		echo ; \
		echo Making in $$machine; \
		echo "unsetenv CIAODEBUG; cd $(SRC)/library/random; gmake" | rsh $$machine csh; \
	done
