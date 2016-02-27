# This is a sample target.mk file, used to jump from a source
# directory to a target directory.  This version allows for an
# architecture specific target directory.
#

.SUFFIXES:

OBJDIR := $(OBJBASE)/$(TARGET)

# Define the rules to build in the target subdirectories.
#
MAKETARGET = $(MAKE) --no-print-directory -C $@ -f $(CURDIR)/Makefile.mk \
	SRCDIR=$(CURDIR) $(MAKECMDGOALS)

.PHONY: $(OBJDIR)
$(OBJDIR):
	+@[ -d $@ ] || mkdir -p $@
	+@$(MAKETARGET)


# These rules keep make from trying to use the match-anything rule
# below to rebuild the makefiles--ouch!  Obviously, if you don't
# follow my convention of using a `.mk' suffix on all non-standard
# makefiles you'll need to change the pattern rule.
#
Makefile : ;
%.mk :: ;


# Anything we don't know how to build will use this rule.  The command
# is a do-nothing command, but the prerequisites ensure that the
# appropriate recursive invocations of make will occur.
#
% :: $(OBJDIR) ;
