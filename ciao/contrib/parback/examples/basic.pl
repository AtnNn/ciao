:- module(basic,
	[
	    m/0,
	    a/3
	],[]).

:- use_package(parback).

m :- a(_X,_Y,_Z), fail.

a(X,Y,Z) :- 
	parback_exec(3, ['basic:p'(X),'basic:q'(Y),'basic:r'(Z)]),
	display('res '), display(p(X)), nl,
	display('res '), display(q(Y)), nl,
	display('res '), display(r(Z)), nl.

p(1).
p(2).
 %% p(3).

q(3).
q(4).
 %% q(c).

r(5).
r(6).
 %% r(z).
