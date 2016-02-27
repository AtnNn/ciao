:- module(nondetqsortex,
	[
	    main_nondet/0,
	    main_nondet_par/4,
	    main_seq/2,
	    gen_list/2,
	    qsort_seq/4,
	    union_gc/5,
	    qsort_nondet_gc/6
	],
	[andprolog]).

:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format), [format/2]).
:- use_module(library(lists), [append/3, length/2, delete/3]).
:- use_module(library(sort)).
:- use_module(library(system)).
:- use_module(library(aggregates)).
:- use_module(library(between)).
:- use_module(library(arithpreds), [floor/2]).

:- use_module(extras).

:- data timeseq/1.
:- data timeseqfinal/1.
:- data timepar/1.

:- data done/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main_nondet :-
	set_prolog_flag(gc, off),
	retractall_fact(timeseq(_)),
	retractall_fact(timeseqfinal(_)),
	retractall_fact(timepar(_)),
	main_seq(8, 10),
	between(1, 8, N),
	main_nondet_par(N, 8, 10, 3),
	fail.
main_nondet.

main_seq(X, Limit) :-
	between(1,3,_),
	main_seq_(X, Limit, _),
	fail.
main_seq(_, _) :-
	findall(SS,retract_fact(timeseq(SS)),LSeq),
	average(LSeq,Seq),
	assertz_fact(timeseqfinal(Seq)).
main_seq_(X, Limit, Exch) :-
	gen_list(X,L1),
	permutation(L1, L),
	nl, display(qsort_seq_todo(X)), nl,
        statistics(walltime, [T1,_]),
	qsort_seq(L,Limit,RR,Exch),
        statistics(walltime, [T2,_]),
	display(qsort_seq_done(X,RR)), nl,
        DeltaSeq is T2 - T1,
	assertz_fact(timeseq(DeltaSeq)),
	fail.

main_nondet_par(A, X, Limit, Gran) :-
	ensure_agents(A),
	between(1,3,_),
	nl, display(qsort_par_todo(num_agents(A))), nl,
	main_nondet_par_(X, Limit, Gran, _),
	fail.
main_nondet_par(A, X, _, _) :-
	current_fact(timeseqfinal(Seq)),
	findall(TP,retract_fact(timepar(TP)),L),
	average(L,Par),
	SpUp is 100*(Seq/Par),
	floor(SpUp,Sp1),
	Sp is Sp1/100,
	format("-- qsort(~f), ~d agents, SpeedUp=~2f~n", [X,A,Sp]),
	fail.

main_nondet_par_(X, Limit, G, Exch) :-
	assertz_fact(done(1)),
	gen_list(X,L1),
	permutation(L1, L),
        statistics(walltime, [T1,_]),
	qsort_nondet_gc(L,X,Limit,G,RR,Exch),
        statistics(walltime, [T2,_]),
	display(qsort_par_done(X,RR)), nl,
        DeltaSeq is T2 - T1,
	assertz_fact(timepar(DeltaSeq)),
	fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Versions without granularity control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

qsort_seq([], _, [], 0) :- !.
qsort_seq([X|L], Limit, R, Exch) :-
	partition(L, X, L1, L2),
	qsort_seq(L2, Limit, R2, _), 
        qsort_seq(L1, Limit, R1, _), 
        append(R1, [X|R2], R),
        exchanges_count([X|L], R, Exch),
	Exch =< Limit.

qsort_par_ndet([], _, [], 0).
qsort_par_ndet([X|L], Limit, R, Exch) :-
        partition(L, X, L1, L2),
        qsort_par_ndet(L2, Limit, R2, _) &
        qsort_par_ndet(L1, Limit, R1, _),
        append(R1, [X|R2], R),
        exchanges_count([X|L], R, Exch),
	Exch =< Limit.

partition(L, P, L1, L2) :-
	union(L1, L2, L),
	check(L1, L2, P).

check([], [], _).
check([], [H|T], P) :-
	\+less_than(H, P),
	check([], T, P).
check([H|T], L2, P) :-
	check_(L2, H, P),
	check(T, L2, P).

check_([], _, _).
check_([H|T], X, P) :-
	\+less_than(H, X),
	\+less_than(P, H),
	check_(T, X, P).

union([], S, S).
union(S, [], S) :-
	S \= [].
union([X|TX], [Y|TY], [X|TZ]):-
	union(TX,[Y|TY],TZ).
union([X|TX], [Y|TY], [Y|TZ]):-
	union([X|TX],TY,TZ).

exchanges_count(L, L, 0) :- !.
exchanges_count(L1, L2, N) :-
	L1 = [H1|T1],
        exchanges_count_(L2, H1, N1),
	replace(L2, H1, L3),
	L3 = [_|T3],
	exchanges_count(T1, T3, N2),
	N is N1 + N2, !.

exchanges_count_([], _, 0) :- !.
exchanges_count_([H|_], H, 0) :- !.
exchanges_count_([_|T], X, N) :-
	exchanges_count_(T, X, N1),
	N is N1 + 1.

replace(L, X, [X|L2]) :-
	delete(L, X, L2).

less_than(N,M) :-
	odd(N,1),
        odd(M,1),
	N < M.

nextFlag(1,0).
nextFlag(0,1).

odd(1,1).
odd(N,Flag) :-
	N > 1,
	nextFlag(Flag,Flag2),
	N2 is N - 1,
	odd(N2,Flag2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Versions with granularity control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

qsort_nondet_gc([], _InputLen, _Limit, _GranLevel, [], 0).
qsort_nondet_gc([X|L], InputLen, Limit, GLev, R, Exch) :-
        (
            InputLen > GLev ->
            partition_gc(L, X, L1, L2, N1, N2),
            qsort_nondet_gc(L1, N1, Limit, GLev, R1, _) &
	    qsort_nondet_gc(L2, N2, Limit, GLev, R2, _),
            append(R1, [X|R2], R),
	    exchanges_count([X|L], R, Exch),
	    Exch =< Limit
        ;
            qsort_seq([X|L], Limit, R, Exch)
        ).

partition_gc(L, P, L1, L2, N1, N2) :-
	union_gc(L1, L2, L, N1, N2),
	check(L1, L2, P).

union_gc([], S, S, 0, N) :-
	length(S, N).
union_gc(S, [], S, N, 0) :-
	S \= [],
	length(S, N).
union_gc([X|TX], [Y|TY], [X|TZ], N1, N2):-
	union_gc(TX,[Y|TY],TZ,N11,N2),
	N1 is N11 + 1.
union_gc([X|TX], [Y|TY], [Y|TZ], N1, N2):-
	union_gc([X|TX],TY,TZ,N1,N21),
	N2 is N21 + 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_list(0, []) :- !.
gen_list(N, [N1|L]) :-
	N1 is N - 1,
	gen_list(N1, L).
permutation(L1, L2) :-
	length(L1, N),
	permutation_(L1, N, L2).
permutation_(_, 0, []) :- !.
permutation_(Xs, N, [X|Zs]) :-
	N > 0,
        R is N*N*N*N*N*N*N*N*N*N*N*N*N mod 7919 mod N,
	I is R + 1,
	remove_at(X, Xs, I, Ys),
	N1 is N - 1,
	permutation_(Ys, N1, Zs).
remove_at(X, [X|Xs], 1, Xs).
remove_at(X, [Y|Xs], K, [Y|Ys]) :-
	K > 1,
	K1 is K - 1,
	remove_at(X, Xs, K1, Ys).

