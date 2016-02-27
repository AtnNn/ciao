:- module(pal,
	[
	    main_det/0,
	    main_nondet/0,
	    main/1,
	    gen_list/2,
	    palindrome_seq/2,
	    palindrome_det_gc/3,
	    palindrome_nondet_gc/3
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
	main_seq(15),
	between(1,8,N),
	main_det_par(N,15),
	fail.
main_det.

main_nondet :-
	set_prolog_flag(gc, off),
	retractall_fact(timeseq(_)),
	retractall_fact(timeseqfinal(_)),
	retractall_fact(timepar(_)),
	main_seq(15),
	between(1,8,N),
	main_nondet_par(N,15),
	fail.
main_nondet.

main_seq(X) :-
	between(1,10,_),
	gen_list(X,L),
        statistics(walltime, [T1,_]),
	palindrome_seq(L,_),
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
	palindrome_det_gc(L,_,7),
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
	format("-- pal(~f), ~d agents, SpeedUp=~2f~n", [X,N,Sp]),
	fail.

main_nondet_par(N,X) :-
	ensure_agents(N),
	between(1,10,_),
	gen_list(X,L),
	pause(1),
        statistics(walltime, [T3,_]),
	palindrome_nondet_gc(L,_,7),
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
	format("-- pal(~f), ~d agents, SpeedUp=~2f~n", [X,N,Sp]),
	fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main(X) :-
	set_prolog_flag(gc, off),
	ensure_agents(2),
	gen_list(X,L),
        statistics(walltime, [T1,_]),
	palindrome_det_gc(L,_,7),
        statistics(walltime, [T2,_]),
	Delta is T2 - T1,
	format("-- pal(~f), ~f ms.~n", [X,Delta]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Versions without granularity control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

palindrome_par_nondet([],[]).
palindrome_par_nondet([First|L1],L2) :-
        palindrome_par_nondet(L1,Ls2) &
        palindrome_par_nondet(L1,Lg2),
        append(Ls2,[First|Lg2],L2).

palindrome_par_det([],[]).
palindrome_par_det([First|L1],L2) :-
        palindrome_par_det(L1,Ls2) '&!'
        palindrome_par_det(L1,Lg2),
        append(Ls2,[First|Lg2],L2).

palindrome_seq([],[]).
palindrome_seq([First|L1],L2) :-
        palindrome_seq(L1,Ls2),
        palindrome_seq(L1,Lg2),
        append(Ls2,[First|Lg2],L2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Versions with granularity control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

palindrome_nondet_gc(X,Y,T) :-
	length(X,L),
	palindrome_nondet_gc_(X,Y,T,L).
palindrome_nondet_gc_([],[],_,_).
palindrome_nondet_gc_([First|L1],L2,T,L) :-
        (
	    L>T  ->
	    L_1 is L-1,
	    L_2 is L-1,
	    palindrome_nondet_gc_(L1,Ls2,T,L_1) &
	    palindrome_nondet_gc_(L1,Lg2,T,L_2)
        ;
	    palindrome_seq(L1,Ls2),
	    palindrome_seq(L1,Lg2)
        ),
	append(Ls2,[First|Lg2],L2).

palindrome_det_gc(X,Y,T) :-
	length(X,L),
	palindrome_det_gc_(X,Y,T,L).
palindrome_det_gc_([],[],_,_).
palindrome_det_gc_([First|L1],L2,T,L) :-
        (
	    L>T  ->
	    L_1 is L-1,
	    L_2 is L-1,
	    palindrome_det_gc_(L1,Ls2,T,L_1) '&!'
	    palindrome_det_gc_(L1,Lg2,T,L_2)
        ;
	    palindrome_seq(L1,Ls2),
	    palindrome_seq(L1,Lg2)
        ),
	append(Ls2,[First|Lg2],L2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_list(0, []).
gen_list(M, [V|Ns]):- 
        M > 0,
        M1 is M - 1,
        V is M*M*M mod 10,
        gen_list(M1, Ns).
