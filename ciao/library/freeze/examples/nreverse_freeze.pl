:- module(_, _).

:- use_module(library(freeze)).

% Example from B Prolog

/* nreverse.pl */

top:-
    mklist(100,L),
    nrev(L,L1).

go:- 
    mklist(500,L),
    statistics(runtime,[Start|_]),
    nrev(L,L1),
    statistics(runtime,[End|_]),
    write(L1),nl,
    T is End-Start,
    write('execution time is :'),write(T).

mklist(N,L):-
    N=:=0,!,
    L=[].
mklist(N,L):-
    L=[N|L1],
    N1 is N-1,
    mklist(N1,L1).

nrev([],L):-
    L=[].
nrev([X|Xs],L):-
    concat(L1,[X],L),
    nrev(Xs,L1).

concat(X,Y,Z):-freeze(X,concat1(X,Y,Z)).

concat1([],L1,L2):-
    L2=L1.
concat1([X|Xs],L1,L2):-
    L2=[X|L3],
    concat(Xs,L1,L3).

