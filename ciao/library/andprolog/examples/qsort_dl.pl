:- module(qsort_dl,
	[
	    main_det/0,
	    main_nondet/0,
	    main/1,
	    gen_list/2,
	    qsortdl_seq/2,
	    qsortdl_det_gc/4,
	    qsortdl_nondet_gc/4
	], [andprolog]).

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

main_det :-
	set_prolog_flag(gc, off),
	retractall_fact(timeseq(_)),
	retractall_fact(timeseqfinal(_)),
	retractall_fact(timepar(_)),
	main_seq(10000),
	between(1,8,N),
	main_det_par(N,10000),
	fail.
main_det.

main_nondet :-
	set_prolog_flag(gc, off),
	retractall_fact(timeseq(_)),
	retractall_fact(timeseqfinal(_)),
	retractall_fact(timepar(_)),
	main_seq(10000),
	between(1,8,N),
	main_nondet_par(N,10000),
	fail.
main_nondet.

main_seq(X) :-
	between(1,10,_),
	gen_list(X,L),
        statistics(walltime, [T1,_]),
	qsortdl_seq(L,_),
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
	gen_list(X,L),
	pause(1),
        statistics(walltime, [T3,_]),
	qsortdl_det_gc(L,X,1000,_),
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
	format("-- qsort(~f), ~d agents, SpeedUp=~2f~n", [X,N,Sp]),
	fail.

main_nondet_par(N,X) :-
	ensure_agents(N),
	between(1,10,_),
	gen_list(X,L),
	pause(1),
        statistics(walltime, [T3,_]),
	qsortdl_nondet_gc(L,X,1000,_),
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
	format("-- qsort(~f), ~d agents, SpeedUp=~2f~n", [X,N,Sp]),
	fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main(X) :-
	set_prolog_flag(gc, off),
	ensure_agents(2),
	gen_list(X,L),
        statistics(walltime, [T1,_]),
	qsortdl_det_gc(L,X,1000,_),
        statistics(walltime, [T2,_]),
	Delta is T2 - T1,
	format("-- qsort(~f), ~f ms.~n", [X,Delta]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Versions without granularity control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

qsortdl_seq(As,Bs):- 
	qsortdl_seq_(As,Bs,[]).

qsortdl_seq_([X|L],R,R2) :-
	partition(L,X,L1,L2),
        qsortdl_seq_(L2,R1,R2),
	qsortdl_seq_(L1,R,[X|R1]).
qsortdl_seq_([],R,R).

qsortdl_det(As,Bs):- 
	qsortdl_det_(As,Bs,[]).

qsortdl_det_([X|L],R,R2) :-
	partition(L,X,L1,L2),
        qsortdl_det_(L2,R1,R2) '&!'
	qsortdl_det_(L1,R,[X|R1]).
qsortdl_det_([],R,R).

qsortdl_nondet(As,Bs):- 
	qsortdl_nondet_(As,Bs,[]).

qsortdl_nondet_([X|L],R,R2) :-
	partition(L,X,L1,L2),
        qsortdl_nondet_(L2,R1,R2) &
	qsortdl_nondet_(L1,R,[X|R1]).
qsortdl_nondet_([],R,R).

partition([],_,[],[]).
partition([E|R],C,[E|Left1],Right):- E < C, !,
	partition(R,C,Left1,Right).
partition([E|R],C,Left,[E|Right1]):-
	partition(R,C,Left,Right1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Versions with granularity control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

qsortdl_det_gc(As,I,G,Bs):- 
	qsortdl_det_gc_(As,I,G,Bs,[]).

qsortdl_det_gc_([X|L],I,G,R,R2) :-
	(
	    I > G ->
	    partition_gc(L,X,L1,L2,0,N1),
	    N2 is I - N1,
	    qsortdl_det_gc_(L1,N1,G,R,[X|R1]) '&!'
	    qsortdl_det_gc_(L2,N2,G,R1,R2)
	;
	    qsortdl_seq([X|L],R)
	).
qsortdl_det_gc_([],_,_,R,R).

qsortdl_nondet_gc(As,I,G,Bs):- 
	qsortdl_nondet_gc_(As,I,G,Bs,[]).

qsortdl_nondet_gc_([X|L],I,G,R,R2) :-
	(
	    I > G ->
	    partition_gc(L,X,L1,L2,0,N1),
	    N2 is I - N1,
	    qsortdl_nondet_gc_(L1,N1,G,R,[X|R1]) &
	    qsortdl_nondet_gc_(L2,N2,G,R1,R2)
	;
	    qsortdl_seq([X|L],R)
	).
qsortdl_nondet_gc_([],_,_,R,R).

partition_gc([], _Piv, [], [], LenL1, LenL1).
partition_gc([E|R], C, [E|Left1], Right, L1LenIn, L1LenOut) :-
        E<C, !,
        L1LenMid is L1LenIn + 1,
        partition_gc(R, C, Left1, Right, L1LenMid, L1LenOut).
partition_gc([E|R], C, Left, [E|Right1], L1lenIn, L1LenOut) :-
        E>=C,
        partition_gc(R, C, Left, Right1, L1lenIn, L1LenOut).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_list(0, []).
gen_list(M, [V|Ns]):- 
        M > 0,
        M1 is M - 1,
        V is M*M*M mod 7919,
        gen_list(M1, Ns).
