:- module(c_itf, [], []).

% This module is (will be) a wrapper of c_itf_internal to 'legacy' applications.

:- reexport(library('compiler/c_itf_internal')).

:- export(def_multifile/4).
:- redefining(def_multifile/4).
def_multifile(Base,F,A,DefType) :-
        c_itf_internal:def_multifile(Base,F,A,DefType),
        \+ internal_predicate(F,A).

:- export(clause_of/7).
:- redefining(clause_of/7).
clause_of(Base, Head, Body, VarNames, Source, Line0, Line1):-
	c_itf_internal:clause_of(Base, Head, Body, VarNames, Source, Line0, Line1),
	\+ (number(Head), arg(1,Body,Pred/Arity),internal_predicate(Pred,Arity)).


internal_predicate('$primitive_meta_predicate',2).
internal_predicate('$current_module',1).
internal_predicate('$ldlibs',1).
internal_predicate('$multifile',3).
internal_predicate('$load_libs',0).
internal_predicate('$meta_args',2).
internal_predicate('$u',2).
internal_predicate('$initialization',1).
internal_predicate('$on_abort',1).
internal_predicate('$imports',5).
internal_predicate('$defines',3).



