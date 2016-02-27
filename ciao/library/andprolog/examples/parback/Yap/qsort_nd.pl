:- module(qsort_nd, [speedups/0]).

:- use_module(library(lists)).

speedups :-
	gen_list(12,L1),
	permuta(L1, L),
  	main_seq(L).

just_first(C) :-
	call(C), !.

main_seq(L) :-
 %% 	abolish_all_tables,
        statistics(walltime, [_,_]),
	just_first(qsort_seq(L,_)),
        statistics(walltime, [_,T]),
	display(time_first(T)),nl,
	fail.

main_seq(L) :-
 %% 	abolish_all_tables,
        statistics(walltime, [_,_]),
	(
	    qsort_seq(L,_), fail
	;
	    true
	),
        statistics(walltime, [_,T]),
	display(time_all(T)),nl.

qsort_seq([], []) :- !.
qsort_seq([X|L], R) :-
	partition(L, X, L1, L2),
	qsort_seq(L2, R2), 
        qsort_seq(L1, R1), 
        append(R1, [X|R2], R).

partition(L, P, L1, L2) :-
	union(L1, L2, L),
	check(L1, L2, P).

check([], [], _).
check([], [H|T], P) :-
          \+less_than(H, P),
          check([], T, P).
check([H|T], L2, P) :-
             \+less_than(P, H),
             check(T, L2, P).

union([], S, S).
union(S, [], S) :-
	S \= [].
union([X|TX], [Y|TY], [X|TZ]):-
	union(TX,[Y|TY],TZ).
union([X|TX], [Y|TY], [Y|TZ]):-
	union([X|TX],TY,TZ).

less_than(N,M) :-
	odd(N,1),
        odd(M,1),
	N < M.

less_than(N,M) :-
	odd(N,0),
        odd(M,0),
	N < M.

nextFlag(1,0).
nextFlag(0,1).

odd(1,1).
odd(N,Flag) :-
	N > 1,
	nextFlag(Flag,Flag2),
	N2 is N - 1,
	odd(N2,Flag2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_list(0, []) :- !.
gen_list(N, [N1|L]) :-
	N1 is N - 1,
	gen_list(N1, L).
permuta(L1, L2) :-
	length(L1, N),
	permuta_(L1, N, L2).
permuta_(_, 0, []) :- !.
permuta_(Xs, N, [X|Zs]) :-
	N > 0,
        R is N*N*N*N*N*N*N*N*N*N*N*N*N mod 7919 mod N,
	I is R + 1,
	remove_at(X, Xs, I, Ys),
	N1 is N - 1,
	permuta_(Ys, N1, Zs).
remove_at(X, [X|Xs], 1, Xs).
remove_at(X, [Y|Xs], K, [Y|Ys]) :-
	K > 1,
	K1 is K - 1,
	remove_at(X, Xs, K1, Ys).

