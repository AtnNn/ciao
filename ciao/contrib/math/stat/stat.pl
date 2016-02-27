:- module(stat, [], [assertions]).

:- doc(title, "Statistical Utilities.").

:- doc(author, "Edison Mera").

:- doc(module, "A complete library of statistical utilities.  This
   library will be used in conjunction with the profiler tools.").

:- reexport(library(math(stat(stat_basic)))).
:- reexport(library(math(stat(stat_gsl)))).
:- reexport(library(math(matrix(matrix_stat_gsl)))).
