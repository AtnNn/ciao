
:- module(tk_test_aux,[hello/0]).

:- export(factorial/2).
:- export(name/1).

hello :-
	display('Hello !!!!'),
	nl.

name(X) :-
	display(X),
	nl.


factorial(N2,X) :-
%	number_codes(N,N2),
%	factorial_aux(N,X),
	factorial_aux(N2,X),
	display(X),
	nl.

factorial_aux(0,1).

factorial_aux(N,X1) :-
	N > 0,
	N1 is N - 1,
	factorial_aux(N1,X2),
	X1 is X2 * N.




















