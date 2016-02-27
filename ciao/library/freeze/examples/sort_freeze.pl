:- module(_, _).

:- use_module(library(freeze)).

% Example from B Prolog

go:-
    L=[12,2,5,13,6,8,14,4,1,11,9,5,3,15,10],
    length(L,N),
    functor(Vec,vec,N),
    Vec=..[_|SortedL], 
    statistics(runtime,[Start|_]),
    psort(L,SortedL),
    statistics(runtime,[End|_]),
    write(SortedL),
    T is End-Start,
    write('execution time is :'),write(T).

psort(X,Y):-sorted(Y),permutation(X,Y).

sorted([X]):-!.
sorted([X,Y|L]):-
    freeze(X,freeze(Y,X=<Y)),
    sorted([Y|L]).

permutation([],Xs):-Xs=[].
permutation([X|Xs],L):-
    L=[Y|Ys],
    myselect([X|Xs],Rest,Y),
    permutation(Rest,Ys).

myselect([X|Xs],Xs,X).
myselect([Y|Ys],[Y|Zs],X) :- myselect(Ys,Zs,X).

