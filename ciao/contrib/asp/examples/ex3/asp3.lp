w(2..3).

g(X) :- w(X), not f(X).
f(X) :- w(X), not g(X).
