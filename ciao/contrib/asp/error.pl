:- module(error, [err/4]).

err(_,M,Action,0) :-
	display('Module '), display(M),display(': '),
	display('Error in '), display(Action), nl,
	abortExec(0).

abortExec(0) :-
	display('abort Execution.'),nl,nl,abort.
abortExec(_).
