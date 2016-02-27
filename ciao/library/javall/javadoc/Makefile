# TODO: Port this Makefile to Prolog
CIAOBUILDDIRtemp=../../../../build
-include $(CIAOBUILDDIRtemp)/CONFIG_mkf

all:
	-javadoc  -sourcepath ../ -d ./ -version -author -overview ../CiaoJava/package.html CiaoJava
	-chmod -f $(DATAMODE) *.html *.css
	-chmod -f $(EXECMODE) CiaoJava
	-chmod -f $(DATAMODE) CiaoJava/*.html

clean distclean:
	$(RM) -r *.html package-list *.cssr CiaoJava gjdoc.properties \
	  resources/*.css resources/*.png resources/gjdoc.js \
	  resources/xhtml11-target10.dtd

