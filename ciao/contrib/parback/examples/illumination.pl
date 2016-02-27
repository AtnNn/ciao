:- module(illumination,
        [
	    get_board/5,
            m_ind/1,
            m_dep/1,
            m_par/1,
	    speedups/0
	],
	[parback]).

:- use_module(library(random)).
:- use_module(library(system)).
:- use_module(extras).
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format), [format/2]).
:- use_module(library(sort)).
:- use_module(library(aggregates)).
:- use_module(library(between)).
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
	R = [[0,1,1,0,0,0,1,0], [1,1,0,0,1,0,1,0],[0,0,1,1,0,0,0,1],[0,0,0,0,1,0,0,0],
	[0,1,1,0,0,1,0,0],[0,0,0,1,1,0,1,0],[0,0,1,1,0,0,0,1],[0,0,1,0,0,1,0,0]],
	main_seq(R),
	between(1, 8, N),
	main_par(N, R).
speedups.

main_seq(R) :-
	between(1,1,_),
	main_seq_(R),
	fail.
main_seq(_) :-
	findall(SS1,retract_fact(timeseq(SS1)),LSeq1),
	findall(SS2,retract_fact(timeseq_first(SS2)),LSeq2),
	display(LSeq1), nl,
	average(LSeq1,Seq1),
	average(LSeq2,Seq2),
	assertz_fact(timeseqfinal_first(Seq2)),
	assertz_fact(timeseqfinal(Seq1)).

main_seq_(R) :-
        statistics(walltime, [T1,_]),
	just_first(find_ind(R,3,_L)),
        statistics(walltime, [T2,_]),
        DeltaSeq is T2 - T1,
	assertz_fact(timeseq_first(DeltaSeq)),
	fail.

main_seq_(R) :-
        statistics(walltime, [T1,_]),
	(find_ind(R,3,_L),fail;true),
        statistics(walltime, [T2,_]),
        DeltaSeq is T2 - T1,
	assertz_fact(timeseq(DeltaSeq)),
	fail.

main_par(A, R) :-
	ensure_agents(A),
	between(1,1,_),
	main_par_(R),
	fail.
main_par(A, _) :-
	current_fact(timeseqfinal_first(Seq)),
	findall(TP,retract_fact(timepar_first(TP)),L),
	average(L,Par),
	SpUp is 100*(Seq/Par),
	floor(SpUp,Sp1),
	Sp is Sp1/100,
	format("-- ~d agents, SpeedUp First=~2f~n", [A,Sp]),
	fail.
main_par(A, _) :-
	current_fact(timeseqfinal(Seq)),
	findall(TP,retract_fact(timepar(TP)),L),
	average(L,Par),
	SpUp is 100*(Seq/Par),
	floor(SpUp,Sp1),
	Sp is Sp1/100,
	format("-- ~d agents, SpeedUp All=~2f~n", [A,Sp]),
	fail.

main_par_(R) :-
        statistics(walltime, [T1,_]),
	just_first(find_par(R,3,_)),
        statistics(walltime, [T2,_]),
        DeltaSeq is T2 - T1,
	assertz_fact(timepar_first(DeltaSeq)),
	fail.
main_par_(R) :-
        statistics(walltime, [T1,_]),
	(find_par(R,3,_),fail;true),
        statistics(walltime, [T2,_]),
        DeltaSeq is T2 - T1,
	assertz_fact(timepar(DeltaSeq)),
	fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% find_xxx(R,D,L): searchs a bit belonging to        %%
%% different columns of the board R such as the       %%
%% line difference between bits of consecutive colums %%
%% is greater than D.                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%sequential independent search
m_ind(L) :- 
	get_board(bin,3,8,8,R), 
	display(R), nl, 
	find_ind(R,3,L).

%sequential dependent search
m_dep(L) :- 
	get_board(bin,3,8,8,R), 
	display(R), nl, 
	find_dep(R,3,L).

%parallel backtracking search
m_par(L) :- 
	get_board(bin,3,8,8,R), 
	display(R), nl, 
	%this is what we have to measure
	find_par(R,3,L).


find_dep(Board,D,R) :-
	D2 is D * -1,
	each_colum_det(Board,R,D,D2).

each_colum_det([],[],_,_) :- !.
each_colum_det([C|RC],[Ind|IndR],D,PrevInd) :- 
	get_pos(C,1,PrevInd,D,Ind),
	each_colum_det(RC,IndR,D,Ind).

get_pos([],_,_PrevInd,_D,_Ind) :- fail.
get_pos([N|_RL],Pos,PrevInd,D,Ind) :-
	pause,
	N == 1,
	Diff is Pos - PrevInd,
	Diff >= D, 
	Ind = Pos.

get_pos([N|_RL],Pos,PrevInd,D,Ind) :-
	N == 1,
	Diff is PrevInd - Pos,
	Diff >= D, 
	Ind = Pos.

get_pos([_|LR],Pos,PrevInd,D,Ind) :-
	Pos1 is Pos + 1,
	get_pos(LR,Pos1,PrevInd,D,Ind).


find_par(Board,D,R) :-
	throw_colum_ndet_par(Board,R,D).
%% check_dist(R,D).

throw_colum_ndet_par([C],[R],_) :- 
	get_colum(C,1,R).

throw_colum_ndet_par([C|RC],[R1,R2|RR],D) :- 
	parback_exec(2,['illumination:get_colum'(C,1,R1), 'illumination:throw_colum_ndet'(RC,[R2|RR],D)]),
	check_value(R1,R2,D).

find_ind(Board,D,R) :-
	throw_colum_ndet(Board,R,D).
%% check_dist(R,D).

throw_colum_ndet([C],[R],_) :- 
	get_colum(C,1,R).

throw_colum_ndet([C|RC],[R1,R2|RR],D) :- 
	get_colum(C,1,R1),
	throw_colum_ndet(RC,[R2|RR],D),
	check_value(R1,R2,D).

get_colum([],_,_) :- !, fail.
get_colum([C|_],Ind,R) :- 
	pause,
	C == 1,
	Ind = R.
get_colum([_|RC],Ind,R) :- 
	Ind1 is Ind + 1,
	get_colum(RC,Ind1,R).

%% check_dist([_],_) :- !.
%% check_dist([R1,R2|RR],D) :-
%% check_value(R1,R2,D) '&'
%% check_dist([R2|RR],D).

check_value(R1,R2,D) :-
	R1 > R2,
	R1 - R2 >= D.

check_value(R1,R2,D) :-
	R2 - R1 >= D.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Generates in R a board of Type numbers, with probability of (0-1)
%equal to P, C colums and L lines.
get_board(Type,P,C,L,R) :- 
	iterate_col(C,Type,P,L,R).


iterate_col(0,_,_,_,[]) :- !.
iterate_col(C,Type,P,L,[R|Rs]) :-
	C1 is C - 1,
	get_line(L,Type,P,R),
	iterate_col(C1,Type,P,L,Rs).

get_line(0,_,_,[]) :- !.
get_line(L,Type,P,[R|Rs]) :-
	L1 is L - 1,
	get_number(Type,P,R),
	get_line(L1,Type,P,Rs).

get_number(bin,P,R) :-
	random(1,10,N),
	N =< P,
	!, R = 1.

get_number(bin,_P,0).

get_number(dec,_,R) :-
	random(1,10,R).

pause :- fib(15,_).

fib(0, 0) :- !.
fib(1, 1) :- !.
fib(N, F) :-
        N > 1,
        N1 is N - 1,
        N2 is N - 2,
        fib(N1, F1),
	fib(N2, F2),
        F is F1 + F2.

just_first(C) :-
	call(C), !.
