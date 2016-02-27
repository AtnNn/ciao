#import(p3,'p3.pl').
r(1..2).
d(X) :- r(X),not p3:a(X).
m(X) :- d(X),not n(X).
n(X) :- d(X),not m(X).
