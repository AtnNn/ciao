:- module(p2, [s/1]).

:- use_package([asp]).
:- use_asp(asp3,'asp3.lp').

a(1).
a(2).

s(X) :- a(X), asp3:model(Q), Q:g(X).

f(_) :- display('in p2.pl\n').