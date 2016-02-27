:- module(fibo,
	[
	    speedups/0
	],
	[andprolog]).

:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format), [format/2]).
:- use_module(library(system)).
:- use_module(library(aggregates)).
:- use_module(library(between)).
:- use_module(library(odd)).
:- use_module(library(arithpreds), [floor/2]).
:- use_module(library(apll)).

:- use_module(extras).

:- data timeseq/1.
:- data timeseqfinal/1.
:- data timepar/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main_seq(X) :-
	between(1,4,_),
        statistics(walltime, [T1,_]),
	fib_seq(X,_),
        statistics(walltime, [T2,_]),
        DeltaSeq is T2 - T1,
	assertz_fact(timeseq(DeltaSeq)),
	fail.
main_seq(_) :-
	findall(SS,retract_fact(timeseq(SS)),LSeq),
	average(LSeq,Seq),
	assertz_fact(timeseqfinal(Seq)).

speedups :-
	set_prolog_flag(gc, off),
	retractall_fact(timeseq(_)),
	retractall_fact(timeseqfinal(_)),
	retractall_fact(timepar(_)),
	main_seq(25),
	between(1,8,N),
	main_det_par(N,25),
	fail.
speedups.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main_det_par(N,X) :-
	ensure_agents(N),
	between(1,10,_),
        statistics(walltime, [T3,_]),
	fib_det_gc(X,15,_),
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
	format("-- fibo(~f), ~d agents, SpeedUp=~2f~n", [X,N,Sp]),
	fail.

fib_seq(0, 0) :- !.
fib_seq(1, 1) :- !.
fib_seq(N, F) :-
        N > 1,
        N1 is N - 1,
        N2 is N - 2,
        fib_seq(N1, F1),
        fib_seq(N2, F2),
        F is F1 + F2.

fib_det(0, 0) :- !.
fib_det(1, 1) :- !.
fib_det(N, F) :-
        N > 1,
        N1 is N - 1,
        N2 is N - 2,
        fib_det(N1, F1) '&!' fib_det(N2, F2),
        F is F1 + F2.

fib_det_gc(0, _, 0) :- !.
fib_det_gc(1, _, 1) :- !.
fib_det_gc(N, Level, F) :-
        N > 1,
        (
            N =< Level ->
            fib_seq(N, F)
        ;
            N1 is N - 1,
            N2 is N - 2,
	    fib_det_gc(N1, Level, F1) '&!' fib_det_gc(N2, Level, F2),
            F is F1 + F2
        ).

