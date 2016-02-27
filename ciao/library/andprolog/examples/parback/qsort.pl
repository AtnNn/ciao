:- module(qsort,
	[
	    speedups/0
	],
	[andprolog]).

:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format), [format/2]).
:- use_module(library(lists), [append/3]).
:- use_module(library(sort)).
:- use_module(library(system)).
:- use_module(library(aggregates)).
:- use_module(library(between)).
:- use_module(library(arithpreds), [floor/2]).

:- use_module(extras).

:- data timeseq/1.
:- data timeseqfinal/1.
:- data timepar/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

speedups :-
	set_prolog_flag(gc, off),
	retractall_fact(timeseq(_)),
	retractall_fact(timeseqfinal(_)),
	retractall_fact(timepar(_)),
   	main_seq(10000),
  	between(1,2,N),
  	main_det_par(N,10000),
	fail.
speedups.

main_seq(X) :-
	between(1,10,_),
	gen_list(X,L),
        statistics(walltime, [T1,_]),
	qsort_seq(L,_),
        statistics(walltime, [T2,_]),
        DeltaSeq is T2 - T1,
	assertz_fact(timeseq(DeltaSeq)),
	fail.
main_seq(_) :-
	findall(SS,retract_fact(timeseq(SS)),LSeq),
	average(LSeq,Seq),
	assertz_fact(timeseqfinal(Seq)).

main_det_par(N,X) :-
	ensure_agents(N),
   	pause(1),
	between(1,10,_),
	gen_list(X,L),
        statistics(walltime, [T3,_]),
	qsort_det_gc(L,X,300,_),
        statistics(walltime, [T4,_]),
	DeltaPar is T4 - T3,
	assertz_fact(timepar(DeltaPar)),
	fail.
main_det_par(N,X) :-
	current_fact(timeseqfinal(Seq)),
	findall(TP,retract_fact(timepar(TP)),L),
	average(L,Par),
	SpUp is 100*(Seq/Par),
	floor(SpUp,Sp1),
	Sp is Sp1/100,
	format("-- qsort(~d), ~d agents, SpeedUp=~2f vs Seq=~4f~n", [X,N,Sp,Seq]),
	fail.

qsort_seq([], []) :- !.
qsort_seq([X|L], R) :-
	partition(L, X, L1, L2),
	qsort_seq(L2, R2), 
        qsort_seq(L1, R1), 
        append(R1, [X|R2], R).

qsort_par_det([], []) :- !.
qsort_par_det([X|L], R) :-
	partition(L, X, L1, L2),
	qsort_par_det(L2, R2) '&!' qsort_par_det(L1, R1), 
        append(R1, [X|R2], R).

partition([], _B, [], []) :- !.
partition([E|R], C, [E|Left1], Right) :- 
	E < C, !,
	partition(R, C, Left1, Right).
partition([E|R], C, Left, [E|Right1]) :-
	E >= C,
	partition(R, C, Left, Right1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Versions with granularity control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

qsort_det_gc([], _InputLen, _GranLevel, []) :- !.
qsort_det_gc([X|L], InputLen, GLev, R) :-
        (
            InputLen > GLev ->
            partition_gc(L, X, L1, L2, 0, N1),
            N2 is InputLen - N1,
            qsort_det_gc(L1, N1, GLev, R1) '&!' qsort_det_gc(L2, N2, GLev, R2),
            append(R1, [X|R2], R)
        ;
            qsort_seq([X|L], R)
        ).

partition_gc([], _Piv, [], [], LenL1, LenL1) :- !.
partition_gc([E|R], C, [E|Left1], Right, L1LenIn, L1LenOut) :-
        E<C, !,
        L1LenMid is L1LenIn + 1,
        partition_gc(R, C, Left1, Right, L1LenMid, L1LenOut).
partition_gc([E|R], C, Left, [E|Right1], L1lenIn, L1LenOut) :-
        E>=C,
        partition_gc(R, C, Left, Right1, L1lenIn, L1LenOut).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_list(0, []) :- !.
gen_list(M, [V|Ns]):- 
        M > 0,
        M1 is M - 1,
        V is M*M*M mod 7919,
        gen_list(M1, Ns), !.
