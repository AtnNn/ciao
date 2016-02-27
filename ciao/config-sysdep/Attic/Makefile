
include $(MKF-OSARCH)

SHELL=/bin/sh

ifdef CCSHARED
	CCFORCOMP=['$(CCSHARED) $(EXTRA_CFLAGS)']
else
ifdef EXTRA_CFLAGS
	CCFORCOMP=['$(EXTRA_CFLAGS)']
else
	CCFORCOMP=[]
endif
endif

ifdef LDSHARED
	LDFORCOMP=['$(LDSHARED) $(EXTRA_LDFLAGS)']
else
ifdef EXTRA_LDFLAGS
	CCFORCOMP=['$(EXTRA_LDFLAGS)']
else
	LDFORCOMP=[]
endif
endif

output_options:
	@(echo "foreign_compiler_options('$(OSNAME)','$(ARCHNAME)','$(CC)',$(CCFORCOMP))." >> $(OUTFILE) ; \
	  echo "foreign_linker_options('$(OSNAME)','$(ARCHNAME)','$(LD)',$(LDFORCOMP))." >> $(OUTFILE) ; \
	)

 
