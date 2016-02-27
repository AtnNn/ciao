DIR_MAILMAN = /var/lib/mailman
LIST = ciao-users
INDEX_FILE = "http://ciao-lang.org/index.html"
TMP_DIR = tmp
INDEX = index.html

all: clean get compile gen publish

get:
	rm -f index.html
	wget $(INDEX_FILE)

compile: 
	ciaoc fillcontent_ciao.pl

gen: 
	mkdir $(TMP_DIR)
	./fillcontent_ciao $(INDEX) admlogin all > $(TMP_DIR)/admlogin.html
	./fillcontent_ciao $(INDEX) archtoc all > $(TMP_DIR)/archtoc.html
	./fillcontent_ciao $(INDEX) archtocnombox all > $(TMP_DIR)/archtocnombox.html
	./fillcontent_ciao $(INDEX) article all > $(TMP_DIR)/article.html 
	./fillcontent_ciao $(INDEX) emptyarchive all > $(TMP_DIR)/emptyarchive.html 
	./fillcontent_ciao $(INDEX) listinfo all > $(TMP_DIR)/listinfo.html 
	./fillcontent_ciao $(INDEX) options all > $(TMP_DIR)/options.html 
	./fillcontent_ciao $(INDEX) private all > $(TMP_DIR)/private.html 
	./fillcontent_ciao $(INDEX) roster all > $(TMP_DIR)/roster.html 
	./fillcontent_ciao $(INDEX) subscribe all > $(TMP_DIR)/subscribe.html 
	./fillcontent_ciao $(INDEX) archidxfoot no_head > $(TMP_DIR)/archidxfoot.html
	./fillcontent_ciao $(INDEX) archidxhead no_footer > $(TMP_DIR)/archidxhead.html
	@echo ":: Mailman files generation completed"

dry_publish: 
	@echo "mkdir $(DIR_MAILMAN)/lists/$(LIST)/en"
	@echo "cp $(TMP_DIR)/*.html $(DIR_MAILMAN)/lists/$(LIST)/en"
	@echo "rm -rf $(DIR_MAILMAN)/archives/private/$(LIST)"
	@echo "$(DIR_MAILMAN)/bin/arch $(LIST)"

publish:
	mkdir $(DIR_MAILMAN)/lists/$(LIST)/en
	cp $(TMP_DIR)/*.html $(DIR_MAILMAN)/lists/$(LIST)/en

##        @echo ":: Files copied sucessfully"
	rm -rf $(DIR_MAILMAN)/archives/private/$(LIST)
	$(DIR_MAILMAN)/bin/arch $(LIST)

clean:
	rm -f $(INDEX)
	rm -f *.po *.itf fillcontent_ciao
	rm -rf $(TMP_DIR)


