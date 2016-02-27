:- module(tak,
	[
	    main_det/0,
	    main_nondet/0,
	    main/3,
	    tak_seq/4,
	    tak_det_gc/4,
	    tak_nondet_gc/4
	],
	[andprolog]).

:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format), [format/2]).
:- use_module(library(lists), [append/3, length/2]).
:- use_module(library(aggregates)).
:- use_module(library(between)).
:- use_module(library(system)).
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
	main_seq(14,10,3),
	between(1,8,N),
	main_det_par(14,10,3,N),
	fail.
main_det.

main_nondet :-
	set_prolog_flag(gc, off),
	retractall_fact(timeseq(_)),
	retractall_fact(timeseqfinal(_)),
	retractall_fact(timepar(_)),
	main_seq(14,10,3),
	between(1,8,N),
	main_nondet_par(14,10,3,N),
	fail.
main_nondet.

main_seq(X,Y,Z) :-
	between(1,10,_),
        statistics(walltime, [T1,_]),
	tak_seq(X,Y,Z,_),
        statistics(walltime, [T2,_]),
        DeltaSeq is T2 - T1,
	assertz_fact(timeseq(DeltaSeq)),
	fail.
main_seq(_,_,_) :-
	findall(SS,retract_fact(timeseq(SS)),LSeq),
	average(LSeq,Seq),
	assertz_fact(timeseqfinal(Seq)).

main_det_par(X,Y,Z,N) :-
	ensure_agents(N),
	between(1,10,_),
	pause(1),
        statistics(walltime, [T3,_]),
	tak_det_gc(X,Y,Z,_),
        statistics(walltime, [T4,_]),
	DeltaPar is T4 - T3,
	assertz_fact(timepar(DeltaPar)),
	fail.
main_det_par(X,Y,Z,N) :-
	current_fact(timeseqfinal(Seq)),
	findall(TP,retract_fact(timepar(TP)),L),
	average(L,Par),
	SpUp is 100*(Seq/Par),
	floor(SpUp,Sp1),
	Sp is Sp1/100,
	format("-- tak(~f,~f,~f), ~d agents, SpeedUp=~2f~n", [X,Y,Z,N,Sp]),
	fail.

main_nondet_par(X,Y,Z,N) :-
	ensure_agents(N),
	between(1,10,_),
	pause(1),
        statistics(walltime, [T3,_]),
	tak_nondet_gc(X,Y,Z,_),
        statistics(walltime, [T4,_]),
	DeltaPar is T4 - T3,
	assertz_fact(timepar(DeltaPar)),
	fail.
main_nondet_par(X,Y,Z,N) :-
	current_fact(timeseqfinal(Seq)),
	findall(TP,retract_fact(timepar(TP)),L),
	average(L,Par),
	SpUp is 100*(Seq/Par),
	floor(SpUp,Sp1),
	Sp is Sp1/100,
	format("-- tak(~f,~f,~f), ~d agents, SpeedUp=~2f~n", [X,Y,Z,N,Sp]),
	fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main(X,Y,Z) :-
	set_prolog_flag(gc, off),
	ensure_agents(2),
        statistics(walltime, [T1,_]),
	tak_det_gc(X,Y,Z,_),
        statistics(walltime, [T2,_]),
	Delta is T2 - T1,
	format("-- tak(~f,~f,~f), ~f ms.~n", [X,Y,Z,Delta]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tak_seq(X,Y,Z,_1) :-
        X1 is X-1,
        Y1 is Y-1,
        Z1 is Z-1,
        tak_seq_(Z1,X,Y,A3),
        tak_seq_(Y1,Z,X,A2),
        tak_seq_(X1,Y,Z,A1),
        tak_seq_(A1,A2,A3,_A),
        X2 is X-1,
        Y2 is Y-1,
        Z2 is Z-1,
        tak_seq_(Z2,X,Y,A6),
        tak_seq_(Y2,Z,X,A5),
        tak_seq_(X2,Y,Z,A4),
        tak_seq_(A4,A5,A6,_B),
        X3 is X-1,
        Y3 is Y-1,
        Z3 is Z-1,
        tak_seq_(Z3,X,Y,A9),
        tak_seq_(Y3,Z,X,A8),
        tak_seq_(X3,Y,Z,A7),
        tak_seq_(A7,A8,A9,_C),
        X4 is X-1,
        Y4 is Y-1,
        Z4 is Z-1,
        tak_seq_(Z4,X,Y,A12),
        tak_seq_(Y4,Z,X,A11),
        tak_seq_(X4,Y,Z,A10),
        tak_seq_(A10,A11,A12,_D),
	!.

tak_seq_(X,Y,Z,A) :-
        X=<Y,
        !,
        Z=A .
tak_seq_(X,Y,Z,A) :-
        X1 is X-1,
        Y1 is Y-1,
        Z1 is Z-1,
	tak_seq_(Z1,X,Y,A3),
	tak_seq_(Y1,Z,X,A2),
	tak_seq_(X1,Y,Z,A1),
	tak_seq_(A1,A2,A3,A).

tak_det_gc(X,Y,Z,_1) :-
        X1 is X-1,
        Y1 is Y-1,
        Z1 is Z-1,
        tak_det_gc_(Z1,X,Y,A3) '&!>' H4,
        tak_det_gc_(Y1,Z,X,A2) '&!>' H5,
        tak_det_gc_(X1,Y,Z,A1) '&!>' H6,
        X2 is X-1,
        Y2 is Y-1,
        Z2 is Z-1,
        X3 is X-1,
        Y3 is Y-1,
        Z3 is Z-1,
        X4 is X-1,
        Y4 is Y-1,
        Z4 is Z-1,
        tak_det_gc_(Z2,X,Y,A6) '&!>' H11,
        tak_det_gc_(Y2,Z,X,A5) '&!>' H12,
        tak_det_gc_(X2,Y,Z,A4) '&!>' H13,
        tak_det_gc_(Z3,X,Y,A9) '&!>' H18,
        tak_det_gc_(Y3,Z,X,A8) '&!>' H19,
        tak_det_gc_(X3,Y,Z,A7) '&!>' H20,
        tak_det_gc_(Z4,X,Y,A12) '&!>' H25,
        tak_det_gc_(Y4,Z,X,A11) '&!>' H26,
        tak_det_gc_(X4,Y,Z,A10) '&!>' H27,
        H4 '<&!' ,
        H5 '<&!' ,
        H6 '<&!' ,
        tak_det_gc_(A1,A2,A3,_A) '&!>' H7,
        H11 '<&!' ,
        H12 '<&!' ,
        H13 '<&!' ,
        tak_det_gc_(A4,A5,A6,_B) '&!>' H14,
        H18 '<&!' ,
        H19 '<&!' ,
        H20 '<&!' ,
        tak_det_gc_(A7,A8,A9,_C) '&!>' H21,
        H25 '<&!' ,
        H26 '<&!' ,
        H27 '<&!' ,
        tak_det_gc_(A10,A11,A12,_D),
        H7 '<&!' ,
        H14 '<&!' ,
        H21 '<&!' .

tak_det_gc_(X,Y,Z,A) :-
        X=<Y,
        !,
        Z=A .
tak_det_gc_(X,Y,Z,A) :-
        X1 is X-1,
        Y1 is Y-1,
        Z1 is Z-1,
	(
	    (Y1 > 8) ->
	     tak_det_gc_(Z1,X,Y,A3) '&!'
             tak_det_gc_(Y1,Z,X,A2) '&!'
             tak_det_gc_(X1,Y,Z,A1),
	     tak_det_gc_(A1,A2,A3,A)
	;
	     tak_det_gc_(Z1,X,Y,A3),
             tak_det_gc_(Y1,Z,X,A2),
             tak_det_gc_(X1,Y,Z,A1),
	     tak_det_gc_(A1,A2,A3,A)
	).

tak_nondet_gc(X,Y,Z,_1) :-
        X1 is X-1,
        Y1 is Y-1,
        Z1 is Z-1,
        tak_nondet_gc_(Z1,X,Y,A3) &> H4,
        tak_nondet_gc_(Y1,Z,X,A2) &> H5,
        tak_nondet_gc_(X1,Y,Z,A1) &> H6,
        X2 is X-1,
        Y2 is Y-1,
        Z2 is Z-1,
        X3 is X-1,
        Y3 is Y-1,
        Z3 is Z-1,
        X4 is X-1,
        Y4 is Y-1,
        Z4 is Z-1,
        tak_nondet_gc_(Z2,X,Y,A6) &> H11,
        tak_nondet_gc_(Y2,Z,X,A5) &> H12,
        tak_nondet_gc_(X2,Y,Z,A4) &> H13,
        tak_nondet_gc_(Z3,X,Y,A9) &> H18,
        tak_nondet_gc_(Y3,Z,X,A8) &> H19,
        tak_nondet_gc_(X3,Y,Z,A7) &> H20,
        tak_nondet_gc_(Z4,X,Y,A12) &> H25,
        tak_nondet_gc_(Y4,Z,X,A11) &> H26,
        tak_nondet_gc_(X4,Y,Z,A10) &> H27,
        H4 <& ,
        H5 <& ,
        H6 <& ,
        tak_nondet_gc_(A1,A2,A3,_A) &> H7,
        H11 <& ,
        H12 <& ,
        H13 <& ,
        tak_nondet_gc_(A4,A5,A6,_B) &> H14,
        H18 <& ,
        H19 <& ,
        H20 <& ,
        tak_nondet_gc_(A7,A8,A9,_C) &> H21,
        H25 <& ,
        H26 <& ,
        H27 <& ,
        tak_nondet_gc_(A10,A11,A12,_D),
        H7 <& ,
        H14 <& ,
        H21 <& .

tak_nondet_gc_(X,Y,Z,A) :-
        X=<Y,
        !,
        Z=A .
tak_nondet_gc_(X,Y,Z,A) :-
        X1 is X-1,
        Y1 is Y-1,
        Z1 is Z-1,
	(
	    (Y1 > 8) ->
	     tak_nondet_gc_(Z1,X,Y,A3) &
             tak_nondet_gc_(Y1,Z,X,A2) &
             tak_nondet_gc_(X1,Y,Z,A1),
	     tak_nondet_gc_(A1,A2,A3,A)
	;
	     tak_nondet_gc_(Z1,X,Y,A3),
             tak_nondet_gc_(Y1,Z,X,A2),
             tak_nondet_gc_(X1,Y,Z,A1),
	     tak_nondet_gc_(A1,A2,A3,A)
	).

