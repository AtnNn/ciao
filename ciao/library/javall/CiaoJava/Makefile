# TODO: Port this Makefile to Prolog
CIAOBUILDDIRtemp=../../../../build
-include $(CIAOBUILDDIRtemp)/CONFIG_mkf

JAVASOURCES=*.java
JAVACLASSES=*.class
JAVAC=javac
JAVAFLAGS=

all: classes

classes:
	-$(JAVAC) $(JAVAFLAGS) $(JAVASOURCES)
	-chmod $(DATAMODE) $(JAVACLASSES)

clean realclean:
	/bin/rm -f $(JAVACLASSES)
	/bin/rm -f *.itf *.po *.dep *.asr

distclean: realclean
	/bin/rm -f *~ *.class

tags:
	-etags -l java $(JAVASOURCES)


