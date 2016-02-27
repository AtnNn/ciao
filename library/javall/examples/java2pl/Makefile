include ../../../../SETTINGS
include $(SRC)/SHARED
include $(SYSDEP_FILES)/mkf-$(CIAOARCH)

JAVASOURCES=*.java
PROLOGSOURCES=*.pl
PROLOGSERVER=plserver
JAVACLASSES=*.class
JAVAC=javac
JAVAEXECUTE=java
JAVAFLAGS=-cp $(SRC)/library/javall:./
JAVACFLAGS=-classpath $(SRC)/library/javall
LOGFILES=plserver.out plserver.err

all: classes prolog

run:
	$(JAVAEXECUTE) $(JAVAFLAGS) $(PROLOGSOURCES) ./$(PROLOGSERVER)

classes:
	-$(JAVAC) $(JAVACFLAGS) $(JAVASOURCES)

prolog:
	$(SETLOCALCIAO) $(CIAOC) -c $(PROLOGSOURCES)
	$(SETLOCALCIAO) $(CIAOC) -s $(PROLOGSERVER)
	-chmod $(EXECMODE) $(PROLOGSERVER)

clean realclean:
	/bin/rm -f $(JAVACLASSES)
	/bin/rm -f $(PROLOGSERVER)
	/bin/rm -f *.itf *.po *.dep *.asr plserver
	/bin/rm -f $(LOGFILES)

distclean: realclean
	/bin/rm -f *~ 

tags:
	-etags -l java $(JAVASOURCES)


