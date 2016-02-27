:- module(basic_nest,
	[
	    a/4
	],[]).

:- use_package(parback).
:- use_module(library(system)).

a(X1,X2,X3,X4) :- 
	parback_exec(2, ['basic_nest:t1'(X1,X2),'basic_nest:t2'(X3,X4)]),
	display('res '), display(t1(X1,X2)), nl,
	display('res '), display(t2(X3,X4)), nl.

t1(X,Y) :-
	pause,
	r(X), p(Y).

t2(X,Y) :-
	pause,
	p(X), r(Y).


p(1) :- pause, display(p1),nl.
p(2) :- display(p2),nl.
 %% p(3) :- display(p3),nl.

q(3) :- pause, display(q3),nl.
q(4) :- display(q4),nl.
 %% q(c) :- display(qc),nl.

r(5) :- pause, display(r5),nl.
 %% r(y) :- display(ry),nl.
 %% r(z) :- display(rz),nl.

pause :- fib(2,_).

fib(0, 0) :- !.
fib(1, 1) :- !.
fib(N, F) :-
        N > 1,
	!,
        N1 is N - 1,
        N2 is N - 2,
        fib(N1, F1),
	fib(N2, F2),
        F is F1 + F2.
