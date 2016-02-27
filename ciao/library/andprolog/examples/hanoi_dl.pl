:- module(hanoi_dl,
	[
	    main_det/0,
	    main_nondet/0,
	    main/1,
	    hanoidl_seq/5,
	    hanoidl_det_gc/6,
	    hanoidl_nondet_gc/6
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
	hanoidl_seq(X,_,_,_,_),
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
	hanoidl_det_gc(X,_,_,_,7,_),
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
	format("-- hanoidl(~f), ~d agents, SpeedUp=~2f~n", [X,N,Sp]),
	fail.

main_nondet_par(N,X) :-
	ensure_agents(N),
	between(1,10,_),
	pause(1),
        statistics(walltime, [T3,_]),
	hanoidl_nondet_gc(X,_,_,_,7,_),
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
	format("-- hanoidl(~f), ~d agents, SpeedUp=~2f~n", [X,N,Sp]),
	fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main(X) :-
	set_prolog_flag(gc, off),
	ensure_agents(2),
        statistics(walltime, [T1,_]),
	hanoidl_det_gc(X,_,_,_,7,_),
        statistics(walltime, [T2,_]),
	Delta is T2 - T1,
	format("-- hanoidl(~f), ~f ms.~n", [X,Delta]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Versions without granularity control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hanoidl_seq(N, A, B, C, R):-
        hanoidl_seq_(N, A, B, C, R-[]).

hanoidl_seq_(1, A, _, C, [mv(A,C)|X]-X).
hanoidl_seq_(N,A,B,C,M) :-
        N > 1,
        N1 is N - 1,
        hanoidl_seq_(N1,A,C,B,M1),
        hanoidl_seq_(N1,B,A,C,M2),
        append_dl(M1,[mv(A,C)|X]-X,T),
        append_dl(T,M2,M).

hanoidl_det(N, A, B, C, R):-
        hanoidl_det_(N, A, B, C, R-[]).

hanoidl_det_(1, A, _, C, [mv(A,C)|X]-X).
hanoidl_det_(N,A,B,C,M) :-
        N > 1,
        N1 is N - 1,
        hanoidl_det_(N1,A,C,B,M1) '&!'
        hanoidl_det_(N1,B,A,C,M2),
        append_dl(M1,[mv(A,C)|X]-X,T),
        append_dl(T,M2,M).

hanoidl_nondet(N, A, B, C, R):-
        hanoidl_nondet_(N, A, B, C, R-[]).

hanoidl_nondet_(1, A, _, C, [mv(A,C)|X]-X).
hanoidl_nondet_(N,A,B,C,M) :-
        N > 1,
        N1 is N - 1,
        hanoidl_nondet_(N1,A,C,B,M1) &
        hanoidl_nondet_(N1,B,A,C,M2),
        append_dl(M1,[mv(A,C)|X]-X,T),
        append_dl(T,M2,M).

append_dl(A-B, B-C, A-C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Versions with granularity control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hanoidl_det_gc(N, A, B, C, GS, R):-
        hanoidl_det_gc_(N, A, B, C, GS, R-[]).

hanoidl_det_gc_(1, A, _, C, _, [mv(A,C)|X]-X).
hanoidl_det_gc_(N,A,B,C,GS,M) :-
	N > 1,
	N1 is N - 1,
	(
	    N < GS ->
	    hanoidl_det_gc_(N1,A,C,B,GS,M1),
	    hanoidl_det_gc_(N1,B,A,C,GS,M2)
        ;
	    hanoidl_det_gc_(N1,A,C,B,GS,M1) '&!'
            hanoidl_det_gc_(N1,B,A,C,GS,M2)
	),
	append_dl(M1,[mv(A,C)|X]-X,T),
	append_dl(T,M2,M).

hanoidl_nondet_gc(N, A, B, C, GS, R):-
        hanoidl_nondet_gc_(N, A, B, C, GS, R-[]).

hanoidl_nondet_gc_(1, A, _, C, _, [mv(A,C)|X]-X).
hanoidl_nondet_gc_(N,A,B,C,GS,M) :-
	N > 1,
	N1 is N - 1,
	(
	    N < GS ->
	    hanoidl_nondet_gc_(N1,A,C,B,GS,M1),
	    hanoidl_nondet_gc_(N1,B,A,C,GS,M2)
        ;
	    hanoidl_nondet_gc_(N1,A,C,B,GS,M1) &
            hanoidl_nondet_gc_(N1,B,A,C,GS,M2)
	),
	append_dl(M1,[mv(A,C)|X]-X,T),
	append_dl(T,M2,M).

