:- module(searchbridge,
	[
	    searchbridge/1
	],[default]).

:-use_module(library(aggregates)).

:- op(1150, fx, [ table ]).

:- dynamic edge/2.
:- dynamic tabl/1.
:- dynamic tmp/1.

searchbridge(File) :-
	open(File,read,F),
	readAll(F),
	findall(X,tabl(X), L),
	nl, display('TABULADOS'), nl,
	display(L), nl,
 	allFordward(L,LF),
	nl, display('ALLFORDWARD'), nl,
	display(LF), nl,
 	allBack(L,LB),
	nl, display('ALLBACKWARD'), nl,
	display(LB), nl,
 	intersecction(LF,LB,LT),
	nl, display('INTERSECCTION'), nl,
	display(LT), nl,
 	difference(LT,L,Bridge),
	nl, display('DIFFERENCE'), nl,
	display(Bridge), nl,
	nl, display('BEGIN'), nl,
 	show(Bridge).
	

readAll(F) :-
	read(F,T),
	reading(T),
	(
	    T = end_of_file ->
	    true
	;
	    readAll(F)
	).

reading(end_of_file) :- !.
reading(':-'(dynamic(A))) :- 
	!, adding(A).

reading(':-'(A,B)) :- 
	!, functor(A,Name,Arity),
	inserting('/'(Name,Arity),B).

reading(_) :- !.

adding(','(A,B)) :- !,
	display(tabl(A)), nl, 
	assert(tabl(A)),
	adding(B).
adding(B) :- !,
	display(tabl(B)), nl, 
	assert(tabl(B)).

inserting(P,','(A,B)) :- !,
	functor(A,Name,Arity),
	display(edge(P,'/'(Name,Arity))), nl, 
	assert(edge(P,'/'(Name,Arity))),
	inserting(P,B).
inserting(P,B) :- !,
	functor(B,Name,Arity),
	display(edge(P,'/'(Name,Arity))), nl,
	assert(edge(P,'/'(Name,Arity))).

allFordward(L,_) :-
	retractall(tmp(_)),
	member(M,L),
	assert(tmp(M)),
	edge(M,M2),
	assert(tmp(M2)),
	fail.

allFordward(L,LF) :-
	setof(X,tmp(X), LT),
	length(L,N1),
	length(LT,N2),
	(
	    N1 = N2 ->
	    LF = LT
	;
	    allFordward(LT,LF)
	).

allBack(L,_) :-
	retractall(tmp(_)),
	member(M,L),
	assert(tmp(M)),
	edge(M2,M),
	assert(tmp(M2)),
	fail.

allBack(L,LF) :-
	setof(X,tmp(X), LT),
	length(L,N1),
	length(LT,N2),
	(
	    N1 = N2 ->
	    LF = LT
	;
	    allBack(LT,LF)
	).

intersecction([],_,[]).
intersecction([H|R],LF, Bridge) :-
	(
	    member(H,LF) ->
	    intersecction(R,LF,BridgeR),
	    Bridge = [H|BridgeR]
	;
	    intersecction(R,LF,Bridge)
	).

difference([],_,[]).
difference([H|R],LF, Bridge) :-
	(
	    member(H,LF) ->
	    difference(R,LF,Bridge)
	;
	    difference(R,LF,BridgeR),
	    Bridge = [H|BridgeR]
	).

show([]) :- display('FIN'), nl.
show([H|R]) :-
	display(':- bridge '), display(H), display('.'), nl,
	show(R).
	
	
