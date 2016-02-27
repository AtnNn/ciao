# Group of commands for the compilation of CHR

command_group_exp "CHR"
command_help_exp <<EOF    
  all_chr    Perform 'all' action on CHR component
  clean_chr [not documented]
EOF
register_command "all_chr"
do__all_chr() {
    gmake_ciao chr # TODO: It is still not a separated component (and it should be)
}
register_command "clean_chr"
do__clean_chr() {
    rm -rf \
	${CIAODESRC}/ciao/library/chr/chr_translate_bootstrap1.pl \
	${CIAODESRC}/ciao/library/chr/chr_translate_bootstrap2.pl \
	${CIAODESRC}/ciao/library/chr/chr_translate.pl.cmp \
	${CIAODESRC}/ciao/library/chr/guard_entailment.pl \
	${CIAODESRC}/ciao/library/chr/chr_translate.pl
}
