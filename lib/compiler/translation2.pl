:- module(translation, [
        expand_term/3,
        expand_goal/3,
        expand_clause/5,
        add_sentence_trans/2,
        del_sentence_trans/1,
        add_term_trans/2,
        del_term_trans/1,
        add_goal_trans/2,
        del_goal_trans/1,
        add_clause_trans/2,
        del_clause_trans/1
        ], [assertions]).

:- meta_predicate
        add_sentence_trans(+, spec),
        add_term_trans(+, spec),
        add_goal_trans(+, spec),
        add_clause_trans(+, spec).

:- use_module(engine(internals), ['$meta_call'/1, term_to_meta/2]).
:- use_module(library('compiler/c_itf'), [meta_args/2, imports/5]).
:- use_module(library(lists), [append/3]).

expand_term(X0, M, X2) :-
        sentence_translation(X0,  M, X1), % not recursive
        term_translation(X1, M, X2).      % recursive

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- data sentence_translations/2.

add_sentence_trans(M, S) :-
        term_to_meta(T/A, S),
        atom(T),
        create_trans(A, T, M, Tr),
        ( retract_fact(sentence_translations(M,Ts)) ->
              append(Ts, [Tr], NTs)
        ; NTs = [Tr]
        ),
        asserta_fact(sentence_translations(M,NTs)).

del_sentence_trans(M) :-
        retractall_fact(sentence_translations(M,_)).

sentence_translation(X, M, Y) :-
        nonvar(X),
        sentence_translations(M, [T|Ts]), !,
        do_sent_trans(Ts, T, X, Y).
sentence_translation(X, _, X).

do_sent_trans([], T, X, Xt) :-
        do_translation(T, X, Xt).
do_sent_trans([T|Ts], T0, X, Y) :-
        do_translation(T0, X, Xt),
        do_sent_trans2(Xt, T, Ts, Y).

do_sent_trans2([], _, _, []) :- !.
do_sent_trans2([S1|S2], T, Ts, St) :- !,
        do_sent_trans(Ts, T, S1, S1t),
        append_clauses(S1t, S2t, St),
        do_sent_trans2(S2, T, Ts, S2t).
do_sent_trans2(S, T, Ts, St) :- do_sent_trans(Ts, T, S, St).

append_clauses([], L, L) :- !.
append_clauses([C|Cs], L, [C|R]) :- !, append(Cs, L, R).
append_clauses(C, L, [C|L]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- data term_translations/2.

add_term_trans(M, S) :-
        term_to_meta(T/A, S),
        atom(T),
        create_trans(A, T, M, Tr),
        ( retract_fact(term_translations(M,Ts)) ->
              append(Ts, [Tr], NTs)
        ; NTs = [Tr]
        ),
        asserta_fact(term_translations(M,NTs)).

del_term_trans(M) :-
        retractall_fact(term_translations(M,_)).

term_translation(X, M, Y) :-
        current_fact(term_translations(M,Ts)), !,
        term_translation_clauses(X, Ts, Y).
term_translation(X, _, X).

term_translation_clauses([], _, []) :- !.
term_translation_clauses([C|Cs], Ts, [D|Ds]) :- !,
        term_translation_t(C, Ts, D),
        term_translation_clauses(Cs, Ts, Ds).
term_translation_clauses(C, Ts, D) :-
        term_translation_t(C, Ts, D).

term_translation_t(X, _, Y) :- var(X), !, Y = X.
term_translation_t(X, Ts, Y) :-
        do_translations(Ts, X, Xt),
        functor(Xt, F, A),
        functor(Y, F, A),
        term_trans_args(A, Xt, Ts, Y).

term_trans_args(0, _, _, _) :- !.
term_trans_args(N, X, Ts, Y) :-
        arg(N, X, Xn),
        arg(N, Y, Yn),
        N1 is N-1,
        term_translation_t(Xn, Ts, Yn),
        term_trans_args(N1, X, Ts, Y).

do_translations([], X, X).
do_translations([T|Ts], X, Y) :-
        do_translation(T, X, Xt),
        do_translations(Ts, Xt, Y).

do_translation(T, X, Y) :-
        comp_goal(T, G),
        arg(1, G, X),
        arg(2, G, Y),
        '$meta_call'(G), !.
do_translation(_, X, X).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

expand_clause(H, B, M, H1, B1) :-
        clause_translations(M, Ts), !,
        do_translations(Ts, clause(H,B), clause(H1,B0)),
        expand_body(B0, M, B1).
expand_clause(H, B,_M, H, B1) :-
        expand_body(B, M, B1).

:- data clause_translations/2.

add_clause_trans(M, S) :-
        term_to_meta(T/A, S),
        atom(T),
        create_trans(A, T, M, Tr),
        ( retract_fact(clause_translations(M,Ts)) ->
              append(Ts, [Tr], NTs)
        ; NTs = [Tr]
        ),
        asserta_fact(clause_translations(M,NTs)).

del_clause_trans(M) :-
        retractall_fact(clause_translations(M,_)).

expand_body(G, M, NG) :-
        nonvar(G),
        current_fact(meta_trans(M,_,_)), !,
        body_translation(G, M, NG).
expand_body(G, _, G).

:- data meta_trans/3.

% add_goal_trans(M, S) == add_meta_trans(M, goal, S)
add_meta_trans(M, Type, S) :-
        term_to_meta(T/A, S),
        atom(T),
        create_trans(A, T, M, Tr),
        assertz_fact(meta_trans(M,Type,Tr)).

del_goal_trans(M) :-
        retractall_fact(goal_trans(M,_)).

sent_trans_of_goals((Head :- Body), M, (Head :- NewBody)) :- !,
        body_translation(Body, M, NewBody).
sent_trans_of_goals(Head, _, Head).

body_translation(V, _, W) :- var(V), !, W = V.
body_translation(Goal, M, NewGoal) :-
        functor(Goal, F, A),
        functor(Meta, F, A),
        construct(Meta), !, % Control predicates
        functor(NewGoal, F, A),
        goal_trans_meta_args(A, Meta, Goal, M, NewGoal).
body_translation(Goal, M, NewGoal) :-
        functor(Goal, F, A),
        functor(Meta, F, A),
        meta_pred(M, Meta), !, % Meta-predicates
        functor(Goal1, F, A),
        goal_trans_meta_args(A, Meta, Goal, M, Goal1),
        simple_goal_trans(Goal1, M, NewGoal).
body_translation(Goal, M, NewGoal) :-
        simple_goal_trans(Goal, M, NewGoal).

goal_trans_meta_args(0, _, _, _, _) :- !.
goal_trans_meta_args(N, Meta, G, M, NG) :-
        arg(N, Meta, K),
        arg(N, G, Gn),
        arg(N, NG, NGn),
        goal_trans_meta_arg(K, Gn, M, NGn),
        N1 is N-1,
        goal_trans_meta_args(N1, Meta, G, M, NG).

goal_trans_meta_arg(goal, G, M, NG) :- !,
        body_translation(G, M, NG).
goal_trans_meta_arg(clause, S, _, S) :- var(S), !.
goal_trans_meta_arg(clause, S, M, NS) :- !,
        sent_trans_of_goals(S, M, NS).
goal_trans_meta_arg(_, A, _, A).

simple_goal_trans(G, M, NG) :-
        goal_trans(M, T),
        do_proper_translation(T, G, Gt), !,
        body_translation(Gt, M, NG).
simple_goal_trans(G, _, G).

do_proper_translation(T, X, Y) :-
        comp_goal(T, G),
        arg(1, G, X),
        arg(2, G, Y),
        '$meta_call'(G), !.

create_trans(2, T,_M, T).
create_trans(3, T, M, Tr) :-
        functor(Tr, T, 1),
        arg(1, Tr, M).

comp_goal(T, G) :- functor(G, T, 2), !.
comp_goal(T, G) :-
        functor(T, F, 1),
        arg(1, T, M),
        functor(G, F, 3),
        arg(3, G, M).

meta_pred(M, Meta) :-
        meta_args(M, Meta), !.
meta_pred(M, Meta) :-
        meta_args(O, Meta),
        functor(Meta, F, A),
        imports(M, _, F, A, O).

construct((goal,goal)).
construct((goal;goal)).
construct((goal->goal)).
construct((\+ goal)).
construct((? ^ goal)).
construct(if(goal,goal,goal)).
construct(? : goal).


% ----------------------------------------------------------------------------

:- comment(version_maintenance,dir('../../version')).

:- comment(version(0*9+2,1999/03/10,21:11*10+'MET'), "Added code to
   handle declaration add_clause_trans/1, to be able to expand clauses
   when information about interface of related modules is needed (so the
   translation is done just before module expansion and compilation).
   (Daniel Cabeza Gras)").

% ----------------------------------------------------------------------------
