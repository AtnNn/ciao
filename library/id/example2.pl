:- module(_,_,[id]).

:- iterative(p/1,0,(_(X,Z) :- Z is X + 1),2).

%:- iterative(p/1,1,(_(X,Z) :- Z is X + 3)). % change the solutions' order 
                                             % to goal p(X). 

p(X) :- q(X).
p(a).

q(X) :- r(X).
q(b).

r(X) :- s(X).
r(c).

s(d).

