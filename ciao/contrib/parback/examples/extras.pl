:- module(extras, [average/2], []).


:- use_module(library(lists)).


average(List,A) :-
 %% 	remove_lowest(List,List1),
 %% 	remove_highest(List1,List2),
 %% 	average_(List2,0,0,A).
	average_(List,0,0,A).
average_([],C,S,A) :- A is S/C.
average_([X|Xs],C,S,A) :-
	C2 is C+1,
	S2 is S+X,
	average_(Xs,C2,S2,A).

max(X,Y,X) :- X>=Y.
max(X,Y,Y) :- X<Y.

min(X,Y,X) :- X<Y.
min(X,Y,Y) :- X>=Y.

minlist([X],X).
minlist([X,Y|Rest],Min) :-
	minlist([Y|Rest],MinRest),
	min(X,MinRest,Min).
    
maxlist([X],X).
maxlist([X,Y|Rest],Max) :-
	maxlist([Y|Rest],MaxRest),
	max(X,MaxRest,Max).

remove_lowest(L,L2) :-
	minlist(L,X),
	delete(L,X,L2).

remove_highest(L,L2) :-
	maxlist(L,X),
	delete(L,X,L2).

