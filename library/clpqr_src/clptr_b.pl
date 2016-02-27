:- module(clptr_b,[translate_clp/2
              % , translate_hash/2
                ],['clpq/ops']).

:- use_module(clpcompiler,[compile_constr/4]).
:- ensure_loaded(clp_attr). % To define attribute hooks

translate_clp({}(Const), Code) :-
        nonvar(Const),
        translate_in_braces(Const, L, []),
        list_to_conjunct(L, Code).

translate_in_braces(A = B, L, R)   :- !, compile_constr(.=.,  A-B, L, R).
translate_in_braces(A =:= B, L, R) :- !, compile_constr(.=.,  A-B, L, R).
translate_in_braces(A =\= B, L, R) :- !, compile_constr(.<>., A-B, L, R).
translate_in_braces(A < B, L, R)   :- !, compile_constr(.<.,  A-B, L, R).
translate_in_braces(A =< B, L, R)  :- !, compile_constr(.=<., A-B, L, R).
translate_in_braces(A > B, L, R)   :- !, compile_constr(.<.,  B-A, L, R).
translate_in_braces(A >= B, L, R)  :- !, compile_constr(.=<., B-A, L, R).
translate_in_braces((C1,C2), L, R) :-
        translate_in_braces(C1, L, L_),
        translate_in_braces(C2, L_, R).

list_to_conjunct([], true).
list_to_conjunct([G|Gs], Goal) :-
        list_to_conjunct_(Gs, G, Goal).

list_to_conjunct_(Gs, G, Goal) :- var(Gs), !,
        Goal = (G,Gs).
list_to_conjunct_([], G, Goal) :- !,
        Goal = G.
list_to_conjunct_([G|Gs], G1, Goal) :- !,
        Goal = (G1,Gs1),
        list_to_conjunct_(Gs, G, Gs1).

% translate_hash(#(p), 3.141592653589793).
% translate_hash(#(e), 2.718281828459045).
% translate_hash(#(zero), Eps) :- arith_eps(Eps).

