# Some auxiliary functions to clean results of compilation 
#
# Note: This code is implemented in SH as a safeguard against
#       undesirable accidents. If this code were implemented in
#       Prolog, then it would not be possible to recover from some
#       unstable states (e.g., if the engine or libraries failed to
#       compile and a recompilation from a clean state is necessary).
#
# TODO:
#
#  - It would be possible to generate part of this file automatically
#    (the structure is really simple).
#
#  - The list of suffixes should be given by the system (the previous
#    point would help).
#
#  - Moving the compilation output to a separate directory would make
#    things simpler.
#
#  - Cleaning may clean things we do not want to clean... (e.g.,
#    *_auto.pl?)
#
# (Jose F. Morales, based on previous code)

# Clean the compilation output for just one module
do_clean_mod() { # MOD
    test x"${1}" = x"" && return
    /bin/rm -f \
           "${1}.asr" \
           "${1}.ast" \
           "${1}.itf" \
           "${1}.po" \
	   "${1}_${CIAOARCH}.so" \
           "${1}_${CIAOARCH}.dll" \
	   "${1}_${CIAOARCH}.dylib" \
           "${1}_${CIAOARCH}_glue.c" \
	   "${1}_${CIAOARCH}_inline.c"
}

# Clean the compilation output for all files, recursively
do_clean_dir_rec() {
    # Be careful with clean_recursive, it has been optimized and
    # must take less than 10 seconds !!!
    #
    # We can not use -print0 | xargs -0 because -print0 is not compatible
    # with Solaris, nor -print | xargs because it is buggy
    test x"${1}" = x"" && return
    find ${1}/. -name ".svn" -prune -o \( \
	-name "*.po" \
	-o -name "*.itf" \
	-o -name "*.cpx" \
	-o -name "*.wam" \
	-o -name "*.dep" \
	-o -name "*.asr" \
	-o -name "*.ast" \
	-o -name "*.ass" \
	-o -name "*.o" \
	-o -name "*.so" \
	-o -name "*.dll" \
	-o -name "*.dylib" \
	-o -name "*_glue.c" \
	-o -name "*_inline.c" \
	-o -name "*_auto.pl" \
	-o -name "auto" -prune \
	-o -name "*.ascii" \
	-o -name "*.class" \
	\
	-o -name "*.aux" \
	-o -name "*.log" \
	-o -name "*.err" \
	-o -name "tmpciao*" \
	\
	-o -name "*_co.pl" \
	-o -name "*_co.java" \
	-o -name "*.iss" \
	\
	-o -name "*~" \
	-o -name "#*" \) \
	-exec /bin/rm -r -f {} \;
}

# TODO: subsumed by do_clean_dir_rec?
#
# do_clean_rec() {
#     # TODO: list of suffixes should be given by the system
#     test x"${1}" = x"" && return
#     find ${1}/. -name ".svn" -prune -o \( \
# 	-name "*.po" \
# 	-o -name "*.itf" \
# 	-o -name "*.wam" \
# 	-o -name "*.dep" \
# 	-o -name "*.asr" \
# 	-o -name "*.ast" \
# 	-o -name "*.ass" \
# 	-o -name "*.o" \
# 	-o -name "*_glue.c" \
# 	-o -name "*_inline.c" \
# 	\
# 	-o -name "*.aux" \
# 	-o -name "*.log" \
# 	-o -name "*.err" \
# 	\
# 	-o -name "*_co.pl" \
# 	-o -name "*_co.java" \) \
# 	-exec /bin/rm -f {} \;
# }

