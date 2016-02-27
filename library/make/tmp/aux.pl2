:- module(aux,_,[]).

:- use_module(library('make/system_extra')).
:- use_module(library(compiler),[use_module/1]).

inaux :-
	display('Calling in aux...\n'),
	call_unknown(_:mygoal),
	display('Exit in aux.\n').

loadaux(X) :-
 	use_module(X).


