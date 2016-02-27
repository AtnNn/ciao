:- use_module(library(metaterms), [ instance/2 ]).
:- use_module(library(write), [ write/1 ]).

:- push_prolog_flag(multi_arity_warnings,off).

:- include(library('rtchecks/rtchecks_sys_indep')).
:- include(library('rtchecks/rtchecks_sys_dep')).

:- pop_prolog_flag(multi_arity_warnings).
