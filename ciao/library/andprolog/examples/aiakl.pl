:- module(aiakl,
	[
	    main_det/0,
	    main_nondet/0,
	    main/1,
	    prepare/3,
	    init_vars_seq/4,
	    init_vars_det/4,
	    init_vars_nondet/4
	],
	[andprolog]).

:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format), [format/2]).
:- use_module(library(lists), [append/3]).
:- use_module(library(system)).
:- use_module(library(sort)).
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
	main_seq(20),
	between(1,8,N),
	main_det_par(N,20),
	fail.
main_det.

main_nondet :-
	set_prolog_flag(gc, off),
	retractall_fact(timeseq(_)),
	retractall_fact(timeseqfinal(_)),
	retractall_fact(timepar(_)),
	main_seq(20),
	between(1,8,N),
	main_nondet_par(N,20),
	fail.
main_nondet.

main_seq(X) :-
	between(1,10,_),
	prepare(X,L1,L2),
        statistics(walltime, [T1,_]),
	init_vars_seq(L1,L2,_,_),
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
	prepare(X,L1,L2),
	pause(1),
        statistics(walltime, [T3,_]),
	init_vars_det(L1,L2,_,_),
        statistics(walltime, [T4,_]),
	DeltaPar is T4 - T3,
	assertz_fact(timepar(DeltaPar)),
	fail.
main_det_par(N,X) :-
	current_fact(timeseqfinal(Seq)),
	findall(TP,retract_fact(timepar(TP)),LDet),
	average(LDet,Par),
	SpUp is 100*(Seq/Par),
	floor(SpUp,Sp1),
	Sp is Sp1/100,
	format("-- aiakl(~f), ~d agents, SpeedUp=~2f~n", [X,N,Sp]),
	fail.

main_nondet_par(N,X) :-
	ensure_agents(N),
	between(1,10,_),
	prepare(X,L1,L2),
	pause(1),
        statistics(walltime, [T3,_]),
	init_vars_nondet(L1,L2,_,_),
        statistics(walltime, [T4,_]),
	DeltaPar is T4 - T3,
	assertz_fact(timepar(DeltaPar)),
	fail.
main_nondet_par(N,X) :-
	current_fact(timeseqfinal(Seq)),
	findall(TP,retract_fact(timepar(TP)),LDet),
	average(LDet,Par),
	SpUp is 100*(Seq/Par),
	floor(SpUp,Sp1),
	Sp is Sp1/100,
	format("-- aiakl(~f), ~d agents, SpeedUp=~2f~n", [X,N,Sp]),
	fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main(X) :-
	set_prolog_flag(gc, off),
	ensure_agents(2),
	prepare(X,L1,L2),
        statistics(walltime, [T1,_]),
	init_vars_det(L1,L2,_,_),
        statistics(walltime, [T2,_]),
	Delta is T2 - T1,
	format("-- aiakl(~f), ~f ms.~n", [X,Delta]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_vars_seq(E1,E2,E1init,E2init) :-
	find_all_vars2(E1,Vars11),
	sort(Vars11,Vars1),
	find_all_vars2(E2,Vars22),
	sort(Vars22,Vars2),
	intersect(Vars1,Vars2,_X,Notin1,Notin2),
	init_vars3(Notin1,E1,Einit11),
	sort(Einit11,E1init),
	init_vars3(Notin2,E2,Einit22),
	sort(Einit22,E2init).

init_vars_det(E1,E2,E1init,E2init) :-
        find_all_vars2_det(E2,Vars22) '&!>' H3,
        find_all_vars2_det(E1,Vars11),
        sort(Vars11,Vars1) '&!>' H2,
        H3 '<&!',
        sort(Vars22,Vars2),
        H2 '<&!',
        intersect(Vars1,Vars2,_X,Notin1,Notin2),
        init_vars3(Notin1,E1,Einit11),
        sort(Einit11,E1init),
        init_vars3(Notin2,E2,Einit22),
        sort(Einit22,E2init) .

init_vars_nondet(E1,E2,E1init,E2init) :-
        find_all_vars2_nondet(E2,Vars22) &> H3,
        find_all_vars2_nondet(E1,Vars11),
        sort(Vars11,Vars1) &> H2,
        H3 <&,
        sort(Vars22,Vars2),
        H2 <&,
        intersect(Vars1,Vars2,_X,Notin1,Notin2),
        init_vars3(Notin1,E1,Einit11),
        sort(Einit11,E1init),
        init_vars3(Notin2,E2,Einit22),
        sort(Einit22,E2init) .

find_all_vars2([],[]).
find_all_vars2([Vars=_Values|Es],AllVars) :-
	find_all_vars2(Es,AllVars1),
	append(Vars,AllVars1,AllVars).

find_all_vars2_det([],[]).
find_all_vars2_det([Vars=_Values|Es],AllVars) :-
	append(Vars,AllVars1,AllVars),
	find_all_vars2_det(Es,AllVars1).

find_all_vars2_nondet([],[]).
find_all_vars2_nondet([Vars=_Values|Es],AllVars) :-
	append(Vars,AllVars1,AllVars) &
	find_all_vars2_nondet(Es,AllVars1).

init_vars3([],E,E).
init_vars3([Var|Vars],E,[[Var]=[unbound]|Es]) :-
        init_vars3(Vars,E,Es) .

intersect(As,[],[],[],As) :- !.
intersect([],Bs,[],Bs,[]) :- !.
intersect([A|As],[B|Bs],Cs,Ds,Es) :-
        A=B,
        !,
        Cs=[A|Cs2],
        intersect(As,Bs,Cs2,Ds,Es) .
intersect([A|As],[B|Bs],Cs,Ds,Es) :-
        A@<B,
        !,
        Es=[A|Es2],
        intersect(As,[B|Bs],Cs,Ds,Es2) .
intersect([A|As],[B|Bs],Cs,Ds,Es) :-
        Ds=[B|Ds2],
        intersect([A|As],Bs,Cs,Ds2,Es) .


% init_vars_par_det(E1,E2,E1init,E2init) :-
%         find_all_vars_par_det(E1,Vars1) '&!'
%         find_all_vars_par_det(E2,Vars2),
%         intersect(Vars1,Vars2,_X,Notin1,Notin2),
%         init_vars2(Notin1,E1,E1init) '&!'
%         init_vars2(Notin2,E2,E2init) .

% init_vars_par_nondet(E1,E2,E1init,E2init) :-
%         find_all_vars_par_nondet(E1,Vars1) &
%         find_all_vars_par_nondet(E2,Vars2),
%         intersect(Vars1,Vars2,_X,Notin1,Notin2),
%         init_vars2(Notin1,E1,E1init) &
%         init_vars2(Notin2,E2,E2init) .

% init_vars_seq(E1,E2,E1init,E2init) :-
%         find_all_vars(E1,Vars1),
%         find_all_vars(E2,Vars2),
%         intersect(Vars1,Vars2,_X,Notin1,Notin2),
%         init_vars2(Notin1,E1,E1init),
%         init_vars2(Notin2,E2,E2init) .

% find_all_vars(E,Vars) :-
%         find_all_vars2(E,Vars0),
%         sort(Vars0,Vars) .

% find_all_vars_par_det(E,Vars) :-
%         find_all_vars2_par_det(E,Vars0),
%         sort(Vars0,Vars) .

% find_all_vars_par_nondet(E,Vars) :-
%         find_all_vars2_par_nondet(E,Vars0),
%         sort(Vars0,Vars) .

% find_all_vars2_par_nondet([],[]).
% find_all_vars2_par_nondet([Vars=_Values|Es],AllVars) :-
%         append(Vars,AllVars1,AllVars) &
%         find_all_vars2_par_nondet(Es,AllVars1).

% find_all_vars2_par_det([],[]).
% find_all_vars2_par_det([Vars=_Values|Es],AllVars) :-
%         append(Vars,AllVars1,AllVars) '&!'
%         find_all_vars2_par_det(Es,AllVars1).

% find_all_vars2([],[]).
% find_all_vars2([Vars=_Values|Es],AllVars) :-
%         append(Vars,AllVars1,AllVars),
%         find_all_vars2(Es,AllVars1) .

% init_vars2(Notin,E,Einit) :-
%         init_vars3(Notin,E,Einit0),
%         sort(Einit0,Einit) .

% init_vars3([],E,E).
% init_vars3([Var|Vars],E,[[Var]=[unbound]|Es]) :-
%         init_vars3(Vars,E,Es) .

% intersect(As,[],[],[],As) :- !.
% intersect([],Bs,[],Bs,[]) :- !.
% intersect([A|As],[B|Bs],Cs,Ds,Es) :-
%         A=B,
%         !,
%         Cs=[A|Cs2],
%         intersect(As,Bs,Cs2,Ds,Es) .
% intersect([A|As],[B|Bs],Cs,Ds,Es) :-
%         A@<B,
%         !,
%         Es=[A|Es2],
%         intersect(As,[B|Bs],Cs,Ds,Es2) .
% intersect([A|As],[B|Bs],Cs,Ds,Es) :-
%         Ds=[B|Ds2],
%         intersect([A|As],Bs,Cs,Ds2,Es) .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prepare(N0,LList1,LList2) :- 
        N is N0*5,
        subst(SList1,SList2),
        mlist(N,SList1,LList1),
        mlist(N,SList2,LList2).

subst(E1,E2):- 
        E1 = [X = [a], X = [a],X = [a], X = [a]],
        E2 = [Y = [a], Y = [a],Y = [a], Y = [a]],
        X = [5,7,8,3,2,4,1,6,9,15,17,18,13,12,14,11,16,19,25,27,28,23,22,24,
             21,26,29],
        Y = [15,17,18,13,12,14,11,16,19,35,37,38,33,32,34,5,7,8,3,2,4,1,6,9,
             31,36,39].

mlist(0,_,[]).
mlist(X,SList,LList) :- 
        X>0,
        Y is X-1,
        mlist(Y,SList,MList),
        append(SList,MList,LList).

