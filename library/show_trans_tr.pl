:- module(show_trans_tr, [show/3], []).

show(clause(H,B),M,_) :-
        display(M),display(':'),display(H), display(' :-\n'),
        display('  '), display(B), display('.\n'), fail.
