:- module(_, _).

:- use_module(library(freeze)).

% Example from B Prolog

/* queens.pl */
go:-
    statistics(runtime,[Start|_]),
    queens(12),
    statistics(runtime,[End|_]),
    T is End-Start,
    write('execution time is :'),write(T).
    
queens(N):-
    make_list(N,List),
    range(1,N,D),
    constrain_queens(List),
    label(List,D),write(List),nl,fail.
queens(_).

constrain_queens([]).
constrain_queens([X|Y]):-
    safe(X,Y,1),
    constrain_queens(Y).

safe(_,[],_).
safe(X,[Y|T],K):-
    freeze(X,freeze(Y,noattack(X,Y,K))),
    K1 is K+1,
    safe(X,T,K1).

noattack(X,Y,K):-
    X =\= Y,
    X+K =\= Y,
    X-K =\= Y.

make_list(0,[]):-!.
make_list(N,[_|Rest]):-
    N1 is N-1,
    make_list(N1,Rest).

range(N,N,[N]) :- !.
range(M,N,[M|Ns]) :-
	M < N,
	M1 is M+1,
	range(M1,N,Ns).

label([],_D).
label([V|Vs],D):-
    myselect(D,Rest,V),
    label(Vs,Rest).
    
myselect([X|Xs],Xs,X).
myselect([Y|Ys],[Y|Zs],X) :- myselect(Ys,Zs,X).



