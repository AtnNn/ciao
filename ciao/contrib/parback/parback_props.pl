:- module(_, _, [assertions]).

%%**********************************************************************
%% PROPERTIES
%%**********************************************************************

:- prop callable_list/1.

:- doc(callable_list(Expression), "@var{Expression} is a list of
callable goals.").

callable_list([]) :- !.
callable_list([H|R]) :-
	callable(H),
	callable_list(R).


:- prop handler/1.

:- doc(handler(Handler), "@var{Handler} is a handler.").

handler(Handler) :- int(Handler).

