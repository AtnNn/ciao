:- module(mmat,
	[
	    speedups/0
	],
	[fsyntax, parback]).


:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format), [format/2]).
:- use_module(library(lists), [append/3, length/2]).
:- use_module(library(random)).
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
	main_seq(40,40,40),
	between(1,8,N),
	main_det_par(40,40,40,N),
	fail.
speedups.

main_seq(X,Y,Z) :-
	between(1,10,_),
	gen_list(X,Y,L1),
	gen_list(Z,Y,L2),
        statistics(walltime, [T1,_]),
	mmatrix_seq(L1,L2,_),
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
	gen_list(X,Y,L1),
	gen_list(Z,Y,L2),
        statistics(walltime, [T3,_]),
	mmatrix_det(L1,L2,_),
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
	format("-- mmat(~f,~f,~f), ~d agents, SpeedUp=~2f~n", [X,Y,Z,N,Sp]),
	fail.

mmatrix_seq([],_,[]).
mmatrix_seq([R|RR],X,[Res|RRes]):-
        multiply_seq(X,R,Res),
        mmatrix_seq(RR,X,RRes).

multiply_seq([],_,[]).
multiply_seq([V0|Rest], V1, [Result|Others]):-
        vmul(V0,V1,Result),
        multiply_seq(Rest, V1, Others).

vmul([],[],0).
vmul([H1|T1], [H2|T2], Result):-
        vmul(T1,T2, Newresult),
        Product is H1*H2,
        Result is Product+Newresult.

mmatrix_det([],_,[]).
mmatrix_det([R|RR],X,[Res|RRes]):-
        parback_exec(2,['mmat:multiply_det'(X,R,Res), 'mmat:mmatrix_det'(RR,X,RRes)]).

multiply_det([],_,[]).
multiply_det([V0|Rest], V1, [Result|Others]):-
          parback_exec(2,['mmat:vmul'(V0,V1,Result),'mmat:multiply_det'(Rest, V1, Others)]).
 %%         vmul(V0,V1,Result),multiply_det(Rest, V1, Others).

:- fun_eval arith(true).

:- fun_eval gen_list/2.
gen_list(0,_) := [].
gen_list(X,Y) := [~gen_list_(Y)|gen_list(X-1,Y)] :- X > 0.

:- fun_eval gen_list_/1.
gen_list_(0) := [].
gen_list_(X) := [~random(1,1000)|~gen_list_(X-1)] :- X > 0.
