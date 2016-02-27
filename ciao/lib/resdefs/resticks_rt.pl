:- module(_, _, [assertions, resdefs]).

:- use_package(library(resdefs(resticks))).
:- use_module(library(hrtime), [hrtime/1]).

resource_usage(ticks, T) :- hrtime(T).
