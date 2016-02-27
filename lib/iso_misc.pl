:- module(iso_misc, [(\=)/2, once/1, compound/1, sub_atom/5,
                     unify_with_occurs_check/2],
         [assertions]).

:- use_module(library(between)).

X \= X :- !, fail.
_ \= _.

:- meta_predicate(once(goal)).

once(G) :- call(G), !.

compound(T) :-
        nonvar(T),
        functor(T, _, A), A > 0.

sub_atom(Atom, Before, Lenght, After, Sub_atom) :-
        ( atom(Atom) ->
          ( var(Sub_atom) ->
            atom_length(Atom, L),
            between(Before, 0, L),
            L1 is L-Before,
            between(Lenght, 0, L1),
            After is L1-Lenght,
            sub_atom(Atom, Before, Lenght, Sub_atom)
          ; atom(Sub_atom) ->
            atom_length(Atom, L),
            atom_length(Sub_atom, SL),
            Lenght = SL,
            R is L-Lenght,
            R > 0,
            between(Before, 0, R),
            After is R-Before,
            sub_atom(Atom, Before, Lenght, Sub_atom)
          )
        ; var(Atom) ->
          throw(error(instantiation_error, sub_atom/5-1))
        ; throw(error(type_error(atom,Atom), sub_atom/5-1))
        ).

unify_with_occurs_check(X,Y) :- var(X), !, uwoc_var(Y, X).
unify_with_occurs_check(X,Y) :- atomic(X), !, X=Y.
unify_with_occurs_check(X,Y) :- uwoc_struct(Y, X).

uwoc_var(V1, V) :- var(V1), !, V1 = V.
uwoc_var(A, V) :- atomic(A), !, A = V.
uwoc_var(S, V) :- nooccurs(S, V), S = V.

uwoc_struct(V, S) :- var(V), !, nooccurs(S, V), S = V.
uwoc_struct(A,_S) :- atomic(A), !, fail.
uwoc_struct(S1, S2) :-
        functor(S1, F, A),
        functor(S2, F, A),
        uwoc_args(A, S1, S2).

uwoc_args(0, _, _) :- !.
uwoc_args(A, S1, S2) :-
        arg(A, S1, S1a),
        arg(A, S2, S2a),
        unify_with_occurs_check(S1a,S2a),
        A1 is A-1,
        uwoc_args(A1, S1, S2).

nooccurs(S, V) :-
        functor(S, _, A),
        noocurrs_args(A, S, V).

noocurrs_args(0, _, _) :- !.
noocurrs_args(A, S, V) :-
        arg(A, S, Sa),
        noocurrs_(Sa, V),
        A1 is A-1,
        noocurrs_args(A1, S, V).

noocurrs_(V1, V) :- var(V1), !, V1 \== V.
noocurrs_(A, _V) :- atomic(A), !.
noocurrs_(S, V) :-
        functor(S, _, A),
        noocurrs_args(A, S, V).

% -----------------------------------------------------------------------------
%% Version comment prompting control for this file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "../version"
%% End:
% -----------------------------------------------------------------------------

:- comment(version(0*4+5,1998/2/24), "Synchronized file versions with
   global CIAO version.  (Manuel Hermenegildo)").

% -----------------------------------------------------------------------------
