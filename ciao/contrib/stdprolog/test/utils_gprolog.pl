% This file defines the following utility predicates in aprolog

% !! add user_error

:- set_prolog_flag(strict_iso, on). % !! test !! doc: default

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

term_variables(T, V) :-
	collect_vars(T, V, []).

%% collect_vars(Term, Vs, Vs0): Vs is the list of variables in Term prepended
%%      to Vs0.
collect_vars(V, Vs, Vs0) :-
	var(V),
	\+ var_memberchk(V, Vs0), !,
	Vs = [V|Vs0].
collect_vars(C, Vs, Vs0) :-
	compound(C), !,
	C =.. [_|Args],
	collect_vars0(Args, Vs, Vs0).
collect_vars(_, Vs, Vs).

%% collect_vars0(List, Vs, Vs0): Vs is the list of variables in all of the
%%      terms in List prepended to Vs0.
collect_vars0([], Vs, Vs).
collect_vars0([H|T], Vs, Vs0) :-
	collect_vars(H, Vs1, Vs0),
	collect_vars0(T, Vs, Vs1).

var_memberchk(V0, [V|_]) :-
	V0 == V, !.
var_memberchk(V0, [_|L]) :-
	var_memberchk(V0, L).

% ----------------
subsumes_chk(General, Specific) :-
	\+  (   numbervars(Specific, 0, _), % !! is it correct or approximate?
		\+ General = Specific
	    ).

variant(A, B) :-
	subsumes_chk(A, B),
	subsumes_chk(B, A).


%%%% pts %%%%
between(N, _, N).
between(N0, M, N) :-
	N0 < M, N1 is N0+1, between(N1, M, N).
