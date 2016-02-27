% gen_list(10,L1), permutation(L1,L2), qsort_seq(L2,L3).
:- module(qsort_nd, [speedups/0,qsort_seq/2,gen_list/2,permutation/2],[]).

:- use_package(parback).

:- use_module(extras).
:- use_module(library(lists), [append/3,length/2]).
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format), [format/2]).
:- use_module(library(sort)).
:- use_module(library(system)).
:- use_module(library(aggregates)).
:- use_module(library(between)).
:- use_module(library(read)).
:- use_module(library(arithpreds), [floor/2]).

:- data timeseq_first/1.
:- data timeseqfinal_first/1.
:- data timepar_first/1.
 
:- data timeseq/1.
:- data timeseqfinal/1.
:- data timepar/1.
 
speedups :-
	 set_prolog_flag(gc, off),
	 retractall_fact(timeseq(_)),
	 retractall_fact(timeseqfinal(_)),
	 retractall_fact(timepar(_)),
	 gen_list(13,L1),
	 permutation(L1, L),
	 main_seq(L),
 	 between(1, 1, N),
 	 main_par(N, L, 10),
	 fail.
speedups.
 
main_seq(L) :-
	    between(1,1,_),
	    main_seq_(L),
	    fail.
main_seq(_) :-
	    findall(SS1,retract_fact(timeseq(SS1)),LSeq1),
	    findall(SS2,retract_fact(timeseq_first(SS2)),LSeq2),
	    average(LSeq1,Seq1),
	    average(LSeq2,Seq2),
	    assertz_fact(timeseqfinal_first(Seq2)),
	    assertz_fact(timeseqfinal(Seq1)).
 
main_seq_(L) :-
        statistics(walltime, [T1,_]),
	just_first(qsort_seq(L,_L2)),
        statistics(walltime, [T2,_]),
        DeltaSeq is T2 - T1,
	assertz_fact(timeseq_first(DeltaSeq)),
	fail.
 
main_seq_(L) :-
        statistics(walltime, [T1,_]),
	(qsort_seq(L,_),fail;true),
        statistics(walltime, [T2,_]),
        DeltaSeq is T2 - T1,
	assertz_fact(timeseq(DeltaSeq)),
	fail.
 
main_par(A, L, G) :-
 %% 	    ensure_agents(A),
	    between(1,1,_),
	    main_par_(A,L,G),
	    fail.
main_par(A, _L, _G) :-
	    retract_fact(timeseqfinal_first(Seq)),
	    findall(TP,retract_fact(timepar_first(TP)),L),
	    average(L,Par),
	    SpUp is 100*(Seq/Par),
	    floor(SpUp,Sp1),
	    Sp is Sp1/100,
	    format("-- ~d agents, SpeedUp First=~2f~n", [A,Sp]),
	    fail.
main_par(A, _L, _G) :-
	    retract_fact(timeseqfinal(Seq)),
	    findall(TP,retract_fact(timepar(TP)),L),
	    average(L,Par),
	    SpUp is 100*(Seq/Par),
	    floor(SpUp,Sp1),
	    Sp is Sp1/100,
	    format("-- ~d agents, SpeedUp All=~2f~n", [A,Sp]),
	    fail.
 
main_par_(_A,L,_G) :-
        statistics(walltime, [T1,_]),
	just_first(qsort_par(L,_L)),
        statistics(walltime, [T2,_]),
        DeltaSeq is T2 - T1,
	assertz_fact(timepar_first(DeltaSeq)),
	fail.
main_par_(_,L,_G) :-
        statistics(walltime, [T1,_]),
	(qsort_par(L,_L),fail;true),
        statistics(walltime, [T2,_]),
        DeltaSeq is T2 - T1,
	assertz_fact(timepar(DeltaSeq)),
	fail.
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
qsort_seq([], []) :- !.
qsort_seq([X|L], R) :-
	partition(L, X, L1, L2),
	qsort_seq(L2, R2), 
        qsort_seq(L1, R1), 
        append(R1, [X|R2], R).
 
qsort_par([], []).
qsort_par([X|L], R) :-
        partition(L, X, L1, L2),
        parback_exec(2,['qsort_nd:qsort_par'(L2, R2), 'qsort_nd:qsort_par'(L1, R1)]),
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
	M1 is M - 1,
	N = M1,
	!,
	fail.

less_than(N,M) :-
	N < M.
  
nextFlag(1,0).
nextFlag(0,1).
 
odd(1,1) :- !.
odd(N,Flag) :-
	    N > 1,
	    nextFlag(Flag,Flag2),
	    N2 is N - 1,
	    odd(N2,Flag2).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Versions with granularity control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
qsort_par_gc([], _InputLen, _GranLevel, []).
qsort_par_gc([X|L], InputLen, GLev, R) :-
        (
            InputLen > GLev ->
            partition_gc(L, X, L1, L2, N1, N2),
	    parback_exec(2,['qsort_nd:qsort_par_gc'(L1, N1, GLev, R1), 'qsort_nd:qsort_par_gc'(L2, N2, GLev, R2)]),
            append(R1, [X|R2], R)
        ;
            qsort_seq([X|L], R)
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
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
just_first(C) :-
	      call(C), !.
 
 %% :- data timeseq_first/1.
 %% :- data timeseqfinal_first/1.
 %% :- data timepar_first/1.
 %% 
 %% :- data timeseq/1.
 %% :- data timeseqfinal/1.
 %% :- data timepar/1.
 %% 
 %% speedups :-
 %% 	set_prolog_flag(gc, off),
 %% 	retractall_fact(timeseq(_)),
 %% 	retractall_fact(timeseqfinal(_)),
 %% 	retractall_fact(timepar(_)),
 %% 	gen_list(5,L1), 
 %% 	permutation(L1, L),
 %% 	main_seq(L),
 %%  %% 	between(1, 8, N),
 %%  %% 	main_par(N, L, 2),
 %% 	fail.
 %% speedups.
 %% 
 %% main_seq(L) :-
 %% 	between(1,1,_),
 %% 	main_seq_(L),
 %% 	fail.
 %% main_seq(_) :-
 %% 	findall(SS1,retract_fact(timeseq(SS1)),LSeq1),
 %% 	findall(SS2,retract_fact(timeseq_first(SS2)),LSeq2),
 %% 	average(LSeq1,Seq1),
 %% 	average(LSeq2,Seq2),
 %% 	assertz_fact(timeseqfinal_first(Seq2)),
 %% 	assertz_fact(timeseqfinal(Seq1)).
 %% 
 %% main_seq_(L) :-
 %%         statistics(walltime, [T1,_]),
 %% 	just_first(qsort_seq(L,_)),
 %%         statistics(walltime, [T2,_]),
 %%         DeltaSeq is T2 - T1,
 %% 	assertz_fact(timeseq_first(DeltaSeq)),
 %% 	fail.
 %% 
 %% main_seq_(L) :-
 %%         statistics(walltime, [T1,_]),
 %% 	(qsort_seq(L,L2),display(lista(L2)),nl,fail;true),
 %%         statistics(walltime, [T2,_]),
 %%         DeltaSeq is T2 - T1,
 %% 	assertz_fact(timeseq(DeltaSeq)),
 %% 	fail.
 %% 
 %% main_par(A, L, G) :-
 %% 	ensure_agents(A),
 %% 	between(1,1,_),
 %% 	main_par_(A,L,G),
 %% 	fail.
 %% main_par(A, _L, _G) :-
 %% 	retract_fact(timeseqfinal_first(Seq)),
 %% 	findall(TP,retract_fact(timepar_first(TP)),L),
 %% 	average(L,Par),
 %% 	SpUp is 100*(Seq/Par),
 %% 	floor(SpUp,Sp1),
 %% 	Sp is Sp1/100,
 %% 	format("-- ~d agents, SpeedUp First=~2f~n", [A,Sp]),
 %% 	fail.
 %% main_par(A, _L, _G) :-
 %% 	retract_fact(timeseqfinal(Seq)),
 %% 	findall(TP,retract_fact(timepar(TP)),L),
 %% 	average(L,Par),
 %% 	SpUp is 100*(Seq/Par),
 %% 	floor(SpUp,Sp1),
 %% 	Sp is Sp1/100,
 %% 	format("-- ~d agents, SpeedUp All=~2f~n", [A,Sp]),
 %% 	fail.
 %% 
 %% main_par_(_A,L,G) :-
 %%         statistics(walltime, [T1,_]),
 %% 	just_first(qsort_par_gc(L,14,G,_L)),
 %%         statistics(walltime, [T2,_]),
 %%         DeltaSeq is T2 - T1,
 %% 	assertz_fact(timepar_first(DeltaSeq)),
 %% 	fail.
 %% main_par_(_,L,G) :-
 %%         statistics(walltime, [T1,_]),
 %% 	(qsort_par_gc(L,14,G,_L),fail;true),
 %%         statistics(walltime, [T2,_]),
 %%         DeltaSeq is T2 - T1,
 %% 	assertz_fact(timepar(DeltaSeq)),
 %% 	fail.
 %% 
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% 
 %% qsort_seq([], []) :- !.
 %% qsort_seq([X|L], R) :-
 %% 		 partition(L, X, L1, L2),
 %% 		 qsort_seq(L2, R2), 
 %%         qsort_seq(L1, R1), 
 %%         append(R1, [X|R2], R).
 %%  
 %% qsort_par([], []).
 %% qsort_par([X|L], R) :-
 %%         partition(L, X, L1, L2),
 %%         qsort_par(L2, R2), qsort_par(L1, R1),
 %%         append(R1, [X|R2], R).
 %%  
 %% partition(L, P, L1, L2) :-
 %% 	     union(L1, L2, L),
 %% 	     check(L1, L2, P).
 %%  
 %% check([], [], _).
 %% check([], [H|T], P) :-
 %% 	  \+less_than(H, P),
 %% 	  check([], T, P).
 %% check([H|T], L2, P) :-
 %% 	     \+less_than(P, H),
 %% 	     check(T, L2, P).
 %%  
 %% union([], S, S).
 %% union(S, [], S) :-
 %% 	 S \= [].
 %% union([X|TX], [Y|TY], [X|TZ]):-
 %% 	      union(TX,[Y|TY],TZ).
 %% union([X|TX], [Y|TY], [Y|TZ]):-
 %% 	      union([X|TX],TY,TZ).
 %%  
 %% less_than(N,M) :-
 %% 	       odd(N,1),
 %%         odd(M,1),
 %% 	N < M.
 %%  
 %% less_than(N,M) :-
 %% 	       odd(N,0),
 %%         odd(M,0),
 %% 	N < M.
 %%  
 %% nextFlag(1,0).
 %% nextFlag(0,1).
 %%  
 %% odd(1,1) :- !.
 %% odd(N,Flag) :-
 %% 	    N > 1,
 %% 	    nextFlag(Flag,Flag2),
 %% 	    N2 is N - 1,
 %% 	    odd(N2,Flag2).
 %%  
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% %% Versions with granularity control
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %%  
 %% qsort_par_gc([], _InputLen, _GranLevel, []).
 %% qsort_par_gc([X|L], InputLen, GLev, R) :-
 %%         (
 %%             InputLen > GLev ->
 %%             partition_gc(L, X, L1, L2, N1, N2),
 %% 	        qsort_par_gc(L1, N1, GLev, R1), qsort_par_gc(L2, N2, GLev, R2),
 %%             append(R1, [X|R2], R)
 %%         ;
 %%             qsort_seq([X|L], R)
 %%         ).
 %%  
 %% partition_gc(L, P, L1, L2, N1, N2) :-
 %% 		union_gc(L1, L2, L, N1, N2),
 %% 		check(L1, L2, P).
 %%  
 %% union_gc([], S, S, 0, N) :-
 %% 	     length(S, N).
 %% union_gc(S, [], S, N, 0) :-
 %% 	    S \= [],
 %% 	    length(S, N).
 %% union_gc([X|TX], [Y|TY], [X|TZ], N1, N2):-
 %% 		 union_gc(TX,[Y|TY],TZ,N11,N2),
 %% 		 N1 is N11 + 1.
 %% union_gc([X|TX], [Y|TY], [Y|TZ], N1, N2):-
 %% 		 union_gc([X|TX],TY,TZ,N1,N21),
 %% 		 N2 is N21 + 1.
 %%  
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %%  
 %% gen_list(0, []) :- !.
 %% gen_list(N, [N1|L]) :-
 %% 	    N1 is N - 1,
 %% 	    gen_list(N1, L).
 %% permutation(L1, L2) :-
 %% 		length(L1, N),
 %% 		permutation_(L1, N, L2).
 %% permutation_(_, 0, []) :- !.
 %% permutation_(Xs, N, [X|Zs]) :-
 %% 		 N > 0,
 %%         R is N*N*N*N*N*N*N*N*N*N*N*N*N mod 7919 mod N,
 %% 	I is R + 1,
 %% 	remove_at(X, Xs, I, Ys),
 %% 	N1 is N - 1,
 %% 	permutation_(Ys, N1, Zs).
 %% remove_at(X, [X|Xs], 1, Xs).
 %% remove_at(X, [Y|Xs], K, [Y|Ys]) :-
 %% 	     K > 1,
 %% 	     K1 is K - 1,
 %% 	     remove_at(X, Xs, K1, Ys).
 %% 
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% 
 %% just_first(C) :-
 %% 	      call(C), display(C), nl, fail.
 %%  %% just_first(C) :-
 %%  %% 	call(C), !.
 %% 
