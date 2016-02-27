# Group of commands for the compilation of CHR

command_group_exp "CHR"
command_help_exp <<EOF    
  chr          [not documented]
  clean_chr [not documented]
EOF
register_command "chr"
do__chr() {
    gmake_ciao chr
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
