% This file defines the following utility predicates in aprolog

/*
retractall(X)
numbervars(T, N, NR)
member(X, L)
memberchk(X, L)
append(L1, L2, L3)
select(X, L1, L2)
prefix(L1, L2)
last(L, X) :-
term_variables(T, V)
variant(T1, T2)
between(A, B, C)
*/

%%%% pts %%%%% Dat: abort on Ctrl-<C>
:- initialization((
   '$abolish'('toplevel:interrupt'),
   asserta(('toplevel:interrupt' :- halt(5))) )).

retractall(X) :-
	'database:retractall'(X).

numbervars(T, N, NR) :-
	'term:numbervars'(T, N, NR).

member(X, L) :-
	'lists:member'(X, L).

memberchk(X, L) :-
	'lists:memberchk'(X, L).

append(L1, L2, L3) :-
	'lists:append'(L1, L2, L3).

select(X, L1, L2) :-
	'lists:select'(X, L1, L2).

prefix(L1, L2) :-
	'lists:prefix'(L1, L2).

last(L, X) :-
	'lists:last'(L, X).

term_variables(T, V) :-
	'term:term_variables'(T, V).

variant(T1, T2) :-
	'term:variant'(T1, T2).

%%%% pts %%%%
between(N, _, N).
between(N0, M, N) :-
	N0 < M, N1 is N0+1, between(N1, M, N).
