# -*- mode: Makefile; -*-
##
## Makefile
##  
## Made by Edison Mera
## Login   <edison@vaioedison>
##
## Started on  Sat Oct 13 19:25:53 2007 Edison Mera
## Last update Sat Oct 13 19:25:53 2007 Edison Mera
## 

# -- Original message from example found in http://make.paulandlesley.org/
#
# This is a sample "general Makefile", showing how a two-step architecture
# invocation might be written.
#
# It jumps to the architecture-specific directory and builds a program
# that prints that version (very trivial, I know, but it should demonstrate
# the salient points.
#

ifeq (,$(filter _%,$(notdir $(CURDIR))))
  include target.mk
else
#----- End Boilerplate

  # This section is common to both target directories

  VPATH	  = $(SRCDIR)

  VERSION = 1.0

ifeq ($(MPARCH),win32vc)
CMDPATH=cygpath -w
else

ifeq ($(MPARCH),win32bcc)
CMDPATH=cygpath -w
else
CMDPATH=echo
endif

endif

ifeq ($(OBJBASE),$(notdir $(shell dirname $(CURDIR))))

include $(SRCDIR)/Makefile-$(OBJBASE)

endif

endif
