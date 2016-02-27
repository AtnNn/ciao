:- module(progeom,
	[
	    main/0,
	    main/1,
	    pds_seq/2,
	    pds_par/3
	],
	[andprolog]).

:- use_module(library(apll)).
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format), [format/2]).
:- use_module(library(aggregates)).
:- use_module(library(between)).
:- use_module(library(system)).
:- use_module(library(arithpreds), [floor/2]).

:- use_module(extras).

:- data timeseq/1.
:- data timeseqfinal/1.
:- data timepar/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main :-
	set_prolog_flag(gc, off),
	retractall_fact(timeseq(_)),
	retractall_fact(timeseqfinal(_)),
	retractall_fact(timepar(_)),
	main_seq(5),
	between(1,8,N),
	clean_measures,
	main_nondet_par(N,5),
	fail.
main :- clean_measures.

main_seq(X) :-
	between(1,10,_),
        statistics(walltime, [T1,_]),
	pds_seq(X,_),
        statistics(walltime, [T2,_]),
        DeltaSeq is T2 - T1,
	assertz_fact(timeseq(DeltaSeq)),
	fail.
main_seq(_) :-
	findall(SS,retract_fact(timeseq(SS)),LSeq),
	average(LSeq,Seq),
	assertz_fact(timeseqfinal(Seq)).

main_nondet_par(N,X) :-
	ensure_agents(N),
	between(1,10,_),
	pause(1),
        statistics(walltime, [T3,_]),
	new_measure,
	pds_par(X,2,_),
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
	print_measures,
	format("-- pds(~f), ~d agents, SpeedUp=~2f, TPar=~f~n",[X,N,Sp,Par]),
	fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main(X) :-
	set_prolog_flag(gc, off),
	ensure_agents(2),
        statistics(walltime, [T1,_]),
	pds_par(X,3,_),
        statistics(walltime, [T2,_]),
        Delta is T2 - T1,
	format("-- pds(~f)=~f ms.~n", [X,Delta]),
	fail.
main(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mymember(X,[X|_Xs]).
mymember(X,[_1|Xs]) :-
        mymember(X,Xs) .

iota(N,List) :-
        iota1(0,N,List) .

iota1(K,K,[]) :- !.
iota1(K,N,[K|List]) :-
        K1 is K+1,
        iota1(K1,N,List) .

dif([],_1,_2,[],[]).
dif([S|Ss],Val,Mod,[D|Ds],[D2|D2s]) :-
        D is Val-S,
        D2 is Mod-D,
        dif(Ss,Val,Mod,Ds,D2s) .

rev([],L,L).
rev([X|Xs],Y,L) :-
        rev(Xs,[X|Y],L) .

mergedelete([],L,L).
mergedelete([D|Ds],[D|R],L2) :-
        mergedelete(Ds,R,L2) .
mergedelete([D|Ds],[X|R],[X|L2]) :-
        D@>X,
        mergedelete([D|Ds],R,L2) .

check_seq([],_1,L,L,_2) :- !.
check_seq(S,Choice,Old,L3,Modulus) :-
        dif(S,Choice,Modulus,Ds,Dds),
        mergedelete(Ds,Old,L2),
        rev(Dds,[],Rds),
        mergedelete(Rds,L2,L3).

check_par([],_1,_2,_3,L,L,_4) :- !.
check_par(S,Size,Gran,Choice,Old,L3,Modulus) :-
        dif(S,Choice,Modulus,Ds,Dds),
	(
	    Size < Gran ->
	    mergedelete(Ds,Old,L2) &
            rev(Dds,[],Rds)
	;
	    mergedelete(Ds,Old,L2),
            rev(Dds,[],Rds)
	),
        mergedelete(Rds,L2,L3).

pds1_seq([],_1,[],_2) :- !.
pds1_seq(Unused,List,[Choice|Rest],Mod) :-
        mymember(Choice,Unused),
        check_seq(List,Choice,Unused,U3,Mod),
        pds1_seq(U3,[Choice|List],Rest,Mod) .

pds1_par([],_1,_2,_3,[],_4) :- !.
pds1_par(Unused,List,Size,Gran,[Choice|Rest],Mod) :-
        mymember(Choice,Unused),
        check_par(List,Size,Gran,Choice,Unused,U3,Mod),
	Size1 is Size + 1,
        pds1_par(U3,[Choice|List],Size1,Gran,Rest,Mod) .

pds_seq(Order,[0|Ans]) :-
        N is Order*(Order+1)+1,
        iota(N,[0|List]),
        pds1_seq(List,[0],Ans,N),
	fail.
pds_seq(_,_).

pds_par(Order,Gran,[0|Ans]) :-
        N is Order*(Order+1)+1,
        iota(N,[0|List]),
        pds1_par(List,[0],1,Gran,Ans,N),
	fail.
pds_par(_,_,_).

