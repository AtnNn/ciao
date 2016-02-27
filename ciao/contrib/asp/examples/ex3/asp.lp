d(1..2).
a(X) :- d(X), not b(X).
b(X) :- d(X), not a(X).
