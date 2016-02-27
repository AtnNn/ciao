:- use_module(library(nwhen)).


e1:-
        when(ground(_A), display(a)). 
e2:-
        when(nonvar(_A), display(a)).
e3:-
        when(ground(A), display(a)), A = f(_).
e4:-
        when(ground(A), display(a)), A = f(1).
e5:-
        when(nonvar(A), display(a)), A = f(_).
e6:-
        when(nonvar(A), display(a)), A = f(1).
e7:-
        when(nonvar(A), display(a)),
        when(nonvar(B), display(b)),
        A = f(B),
        B = f(_).
e8:-
        when(ground(A), display(a)),
        when(nonvar(B), display(b)),
        A = f(B),
        B = f(_).
e9:-
        when(ground(A), display(a)),
        when(ground(B), display(b)),
        A = f(B),
        B = f(J, J),
        J = 1.
e10:-
        when((ground(A), ground(_B)), display(ab)),
        A = 1.
e11:-
        when((ground(A), ground(B)), display(ab)),
        A = 1,
        B = 1.
e12:-
        when((ground(A), ground(B)), display(ab)),
        A = f(B),
        B = 3.
e13:-
        when(ground(A), display(a)),
        when(ground(B), display(b)),
        A = B,
        A = 1.

g(A, B, C):-
        when(ground(A), display(a)),
        when(ground(B), display(b)),
        when(ground(C), display(c)).

e14:-
        g(A, _B, _C), A = 1.
e15:-
        g(_A, B, _C), B = 1.
e16:-
        g(_A, _B, C), C = 1.
e17:-
        g(A, B, C), 
        A = f(B, C),
        B = 1,
        C = 1.
e18:-
        g(A, B, C), 
        A = f(B, C),
        C = 1,
        B = 1.

        
