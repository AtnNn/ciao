:- module(basic_nest_choice,
	[
	    a/6
	],[]).

:- use_package(parback).

a(X1,X2,Y1,Y2,Z1,Z2) :- 
	parback_exec(2, ['basic_nest_choice:t1'(X1,Y1,Z1),'basic_nest_choice:t2'(X2,Y2,Z2)]),
	display('res '), display(t1(X1,Y1,Z1)), nl,
	display('res '), display(t2(X2,Y2,Z2)), nl.

t1(8,Y,Z) :-
	parback_exec(2, ['basic_nest_choice:r'(Y),'basic_nest_choice:p'(Z)]).

t1(9,Y,Z) :-
	parback_exec(2, ['basic_nest_choice:p'(Y),'basic_nest_choice:r'(Z)]).

t2(X,6,Z) :-
	parback_exec(2, ['basic_nest_choice:p'(X),'basic_nest_choice:q'(Z)]).

t2(X,7,Z) :-
	parback_exec(2, ['basic_nest_choice:r'(X),'basic_nest_choice:r'(Z)]).


p(1) :- display(p1),nl.
p(2) :- display(p2),nl.
 %% p(3) :- display(p3),nl.

q(3) :- display(q1),nl.
q(4) :- display(q2),nl.
 %% q(c) :- display(qc),nl.

r(5) :- display(r1),nl.
 %% r(y) :- display(ry),nl.
 %% r(z) :- display(rz),nl.
