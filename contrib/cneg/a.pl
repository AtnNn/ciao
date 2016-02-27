:- module(_, _, []).
:- use_module( library( 'cneg/dist.pl' ) ,[dist/2]).

a(X,Y) :-
	dist(X,Y).