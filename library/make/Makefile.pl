:- module(_,_,[make]).

simple_target <-  :-
	display('This is a simple target'), nl.

dependent_target <- simple_target :-
	display('This is a dependent target'), nl.
