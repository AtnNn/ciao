:- module(_, _, [assertions]).

:- use_package(library(resdefs(resexectime))).
:- use_module(library(prolog_sys), [statistics/2]).

resource_usage(exectime, T) :- statistics(walltime, [T|_]).
