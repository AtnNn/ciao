
include $(MKF-OSARCH)

SHELL=/bin/sh

ifdef CCSHARED
	CCFORCOMP=['$(CCSHARED)']
else
	CCFORCOMP=[]
endif

ifdef LDSHARED
	LDFORCOMP=['$(LDSHARED)']
else
	LDFORCOMP=[]
endif

output_options:
	@(echo "foreign_compiler_options('$(OSNAME)','$(ARCHNAME)','$(CC)',$(CCFORCOMP))." >> $(OUTFILE) ; \
	  echo "foreign_linker_options('$(OSNAME)','$(ARCHNAME)','$(LD)',$(LDFORCOMP))." >> $(OUTFILE) ; \
	)

 
