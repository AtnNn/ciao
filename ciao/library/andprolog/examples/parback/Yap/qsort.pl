:- module(qsort,
	[
	    speedups/1
	]).

:- use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

speedups(N) :-
	main_seq(10000,N).

ejecuta(L,N) :- N =< 1, !, qsort_seq(L,_).
ejecuta(L,N) :- 
	qsort_seq(L,_),
	N1 is N - 1,
	ejecuta(L,N1).

main_seq(X,N) :-
	gen_list(X,L),
        statistics(walltime, [_,_]),
	ejecuta(L,N),
        statistics(walltime, [_,T2]),
	T is (T2 / N),
	display(time(T)),nl.

qsort_seq([], []) :- !.
qsort_seq([X|L], R) :-
	partition(L, X, L1, L2),
	qsort_seq(L2, R2), 
        qsort_seq(L1, R1), 
        append(R1, [X|R2], R).

partition([], _B, [], []) :- !.
partition([E|R], C, [E|Left1], Right) :- 
	E < C, !,
	partition(R, C, Left1, Right).
partition([E|R], C, Left, [E|Right1]) :-
	E >= C,
	partition(R, C, Left, Right1).

gen_list(0, []) :- !.
gen_list(M, [V|Ns]):- 
        M > 0,
        M1 is M - 1,
        V is M*M*M mod 7919,
        gen_list(M1, Ns), !.
