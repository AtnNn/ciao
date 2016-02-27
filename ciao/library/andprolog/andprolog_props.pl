:- module(_, _, [assertions]).

:- include(library(andprolog(andprolog_ops))).
%%**********************************************************************
%% PROPERTIES
%%**********************************************************************


:- prop andcallable/1.

:- doc(andcallable(Expression), "@var{Expression} is of the form
   @tt{goal&goal}.").

andcallable(A&B) :-
	callable(A),
	callable(B).


:- prop handler/1.

:- doc(handler(Handler), "@var{Handler} is a handler.").

handler(Handler) :- int(Handler).

