:- module(basic_choice,
	[
	    m/0,
	    a/4
	],[]).

:- use_package(parback).

m :- a(_,_,_,_), fail.

a(X1,X2,X3,X4) :- 
	parback_exec(2, ['basic_choice:p'(X1,X2),'basic_choice:q'(X3,X4)]),
	display('res '), display(p(X1,X2)), nl,
	display('res '), display(q(X3,X4)), nl.

p(1,X) :- p_(X).
p(2,X) :- p_(X).

q(5,X) :- q_(X).
q(6,X) :- q_(X).

p_(3).
p_(4).

q_(7).
q_(8).