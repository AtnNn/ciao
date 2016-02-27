:- module(hanoi,
	[
	    main_det/0,
	    main_nondet/0,
	    main/1,
	    hanoi_seq/5,
	    hanoi_det_gc/6,
	    hanoi_nondet_gc/6
	],
	[andprolog]).

:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format), [format/2]).
:- use_module(library(lists), [append/3, length/2]).
:- use_module(library(system)).
:- use_module(library(aggregates)).
:- use_module(library(between)).
:- use_module(library(arithpreds), [floor/2]).

:- use_module(extras).

:- data timeseq/1.
:- data timeseqfinal/1.
:- data timepar/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main_det :-
	set_prolog_flag(gc, off),
	retractall_fact(timeseq(_)),
	retractall_fact(timeseqfinal(_)),
	retractall_fact(timepar(_)),
	main_seq(14),
	between(1,8,N),
	main_det_par(N,14),
	fail.
main_det.

main_nondet :-
	set_prolog_flag(gc, off),
	retractall_fact(timeseq(_)),
	retractall_fact(timeseqfinal(_)),
	retractall_fact(timepar(_)),
	main_seq(14),
	between(1,8,N),
	main_nondet_par(N,14),
	fail.
main_nondet.

main_seq(X) :-
	between(1,10,_),
        statistics(walltime, [T1,_]),
	hanoi_seq(X,_,_,_,_),
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
	between(1,10,_),
	pause(1),
        statistics(walltime, [T3,_]),
	hanoi_det_gc(X,_,_,_,7,_),
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
	format("-- hanoi(~f), ~d agents, SpeedUp=~2f~n", [X,N,Sp]),
	fail.

main_nondet_par(N,X) :-
	ensure_agents(N),
	between(1,10,_),
	pause(1),
        statistics(walltime, [T3,_]),
	hanoi_nondet_gc(X,_,_,_,7,_),
        statistics(walltime, [T4,_]),
	DeltaPar is T4 - T3,
	assertz_fact(timepar(DeltaPar)),
	fail.
main_nondet_par(N,X) :-
	current_fact(timeseqfinal(Seq)),
	findall(TP,retract_fact(timepar(TP)),L),
	average(L,Par),
	SpUp is 100*(Seq/Par),
	floor(SpUp,Sp1),
	Sp is Sp1/100,
	format("-- hanoi(~f), ~d agents, SpeedUp=~2f~n", [X,N,Sp]),
	fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main(X) :-
	set_prolog_flag(gc, off),
	ensure_agents(2),
        statistics(walltime, [T1,_]),
	hanoi_det_gc(X,_,_,_,7,_),
        statistics(walltime, [T2,_]),
	Delta is T2 - T1,
	format("-- hanoi(~f), ~f ms.~n", [X,Delta]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Versions without granularity control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hanoi_seq(1,A,_,C,[mv(A,C)]).
hanoi_seq(N,A,B,C,M) :-
	N > 1,
	N1 is N - 1,
	hanoi_seq(N1,A,C,B,M1),
	hanoi_seq(N1,B,A,C,M2),
	append(M1,[mv(A,C)],T),
	append(T,M2,M).

hanoi_det(1,A,_,C,[mv(A,C)]).
hanoi_det(N,A,B,C,M) :-
	N > 1,
	N1 is N - 1,
	hanoi_det(N1,B,A,C,M2) '&!>' H4,
	hanoi_det(N1,A,C,B,M1),
	append(M1,[mv(A,C)],T),
	H4 '<&!',
	append(T,M2,M).

hanoi_nondet(1,A,_,C,[mv(A,C)]).
hanoi_nondet(N,A,B,C,M) :-
	N > 1,
	N1 is N - 1,
	hanoi_det(N1,B,A,C,M2) &> H4,
	hanoi_det(N1,A,C,B,M1),
	append(M1,[mv(A,C)],T),
	H4 <& ,
	append(T,M2,M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Versions with granularity control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hanoi_det_gc(N,A,B,C,GS,M) :-
	(
	    N < GS -> hanoi_seq(N,A,B,C,M)
        ;
	    N1 is N - 1,
	    hanoi_det_gc(N1,B,A,C,GS,M2) '&!>' H4,
	    hanoi_det_gc(N1,A,C,B,GS,M1),
	    append(M1,[mv(A,C)],T),
	    H4 '<&!',
	    append(T,M2,M)
	).

hanoi_nondet_gc(N,A,B,C,GS,M) :-
	(
	    N < GS -> hanoi_seq(N,A,B,C,M)
        ;
	    N1 is N - 1,
	    hanoi_nondet_gc(N1,B,A,C,GS,M2) &> H4,
	    hanoi_nondet_gc(N1,A,C,B,GS,M1),
	    append(M1,[mv(A,C)],T),
	    H4 <& ,
	    append(T,M2,M)
	).

