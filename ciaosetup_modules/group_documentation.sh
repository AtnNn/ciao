# Group of commands for documentation generation

command_group "Documentation"
command_help <<EOF
  docs         (Re)generate all documentation using LPdoc.
               Documentation is normally already in the distribution,
               but this may be needed when checking out of repository
               or after changes in the sources.
EOF
command_help_exp <<EOF
  descfile     [not documented]
  tardocs      [not documented]

  docsreadmes  [not documented]

  clean_docs     [not documented]
  clean_justdocs [not documented]
  clean_installeddocs [not documented]

  clean_justdocs [not documented]
  clean_docstmp  [not documented]
  clean_docspackage [not documented]
EOF
register_command "docs"
do__docs() {
    bold_message "Building CiaoDE documentation"
    lpmake docs
    bold_message "Finished building CiaoDE documentation"
}

register_command "descfile"
do__descfile() {
    lpmake descfile
}
register_command "tardocs"
do__tardocs() {
    lpmake tardocs
}

register_command "docsreadmes"
do__docsreadmes() {
    lpmake docsreadmes
}

register_command "clean_docs"
do__clean_docs() {
    do__clean_installeddocs
    do__clean_justdocs
    do__clean_docstmp
    do__clean_docspackage
}
register_command "clean_installeddocs"
do__clean_installeddocs() {
    rm -rf \
	${CIAODESRC}/build/doc
}
register_command "clean_justdocs"
do__clean_justdocs() {
    rm -rf \
	${CIAODESRC}/HACKING \
	${CIAODESRC}/ciao/NewUser \
	${CIAODESRC}/ciao/README \
	${CIAODESRC}/ciao/INSTALLATION \
	${CIAODESRC}/ciao/INSTALLATION_Win32 \
	${CIAODESRC}/lpdoc/README \
	${CIAODESRC}/lpdoc/INSTALLATION \
	${CIAODESRC}/lpdoc/lib/site/index.html \
	${CIAODESRC}/ciaopp/README \
	${CIAODESRC}/ciaopp/INSTALLATION
}
register_command "clean_docstmp"
do__clean_docstmp() {
    rm -rf \
	${CIAODESRC}/ciao/doc/reference/tmp \
	${CIAODESRC}/ciaopp/doc/internals/tmp \
	${CIAODESRC}/ciaopp/doc/reference/tmp \
	${CIAODESRC}/lpdoc/doc/tmp\
	${CIAODESRC}/doc/readmes/configure_help.tmp
}
register_command "clean_docspackage"
do__clean_docspackage() {
    rm -rf \
	${CIAODESRC}/ciao/doc/reference/package \
	${CIAODESRC}/ciaopp/doc/internals/package \
	${CIAODESRC}/ciaopp/doc/reference/package \
	${CIAODESRC}/lpdoc/doc/package
}
