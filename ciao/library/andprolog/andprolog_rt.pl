:- module(andprolog_rt, _, []).


:- reexport(library(assertions(native_props)), [indep/1, indep/2]).

:- reexport(library(andprolog(det_rt)),                          _).
:- reexport(library(andprolog(nondet_move_top_rt)),              _).
:- reexport(library(andprolog(disj_wait_rt)),                    _).
:- reexport(library(andprolog(callh_rt)),                        _).
:- reexport(library(andprolog(agents_rt)),                       _).

