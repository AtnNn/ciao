% test program:
% interface module to asp2
#use_asp(asp2,'asp2.lp').
% interface module to asp3
#use_asp(asp3,'asp3.lp').
#import(p2,'p2.pl').

%#const v1=3.

r(X) :- p2:s(X).

% calling asp2 and asp3 ASP modules
d(X) :- not asp2:r(X),asp3:w(X).

a(X) :- d(X),not b(X).
b(X) :- d(X),not a(X).

%g(X) :- 1 { a(X), b(X), c(X)}.
%f(X) :- 2 [ a(X)=v1, b(X)=2, c(X)=2 ] 3.

mm(X) :- asp3:model(Q),Q:g(X).
