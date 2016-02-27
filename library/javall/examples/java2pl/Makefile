include ../../../../SETTINGS
include $(SRC)/SHARED
include $(SYSDEP_FILES)/mkf-$(CIAOARCH)

JAVASOURCES=*.java
PROLOGSOURCES=*.pl
JAVACLASSES=*.class
JAVAC=javac
JAVAEXECUTE=java
JAVAFLAGS=-cp $(SRC)/library/javall:./
JAVACFLAGS=-classpath $(SRC)/library/javall
LOGFILES=plserver.out plserver.err
PROLOGSERVER=$(SRC)/library/javall/plserver

all: classes prolog

run:
	$(JAVAEXECUTE) $(JAVAFLAGS) $(PROLOGSOURCES) ./$(PROLOGSERVER)

classes:
	-$(JAVAC) $(JAVACFLAGS) $(JAVASOURCES)
	-chmod $(DATAMODE) $(JAVACLASSES)

prolog:
	$(SETLOCALCIAO) $(CIAOC) -c $(PROLOGSOURCES)

clean realclean:
	/bin/rm -f $(JAVACLASSES)
	/bin/rm -f *.itf *.po *.dep *.asr
	/bin/rm -f $(LOGFILES)

distclean: realclean
	/bin/rm -f *~ 

tags:
	-etags -l java $(JAVASOURCES)


