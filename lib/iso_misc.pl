:- module(iso_misc, [(\=)/2, once/1, compound/1, sub_atom/5,
                     unify_with_occurs_check/2],
         [assertions]).

:- use_module(library(between)).

:- comment(title, "Miscelaneus ISO Prolog predicates").

:- comment(author, "The CLIP Group").
:- comment(author, "Documentation by Edison Mera, based on ISO Prolog standar").

:- comment(module, "This module implements some miscelaneous ISO
   Prolog predicates.").

:- comment(X \= Y,"If @var{X} and @var{Y} are NSTO (Not subject to
   occurs-check), then this is true iff @var{X} and @var{Y} are not
   unifiable.").

X \= X :- !, fail.
_ \= _.

:- meta_predicate(once(goal)).

:- comment(once(G),"@pred{once/1} is true iff @pred{call/1} is true.
   Take in mind that @pred{once/1} behaves as @pred{call/1}, but is
   not re-executable.").

once(G) :- call(G), !.

:- comment(compound(T),"Is true iff @var{T} is a member of the set CT.

   CT is a set where an element of CT is defined for each compound
   term, and c is defined as F(X1, ..., Xn) where:

   1) F is the functor name of the compound term, and

   2) n is the arity of the compound term, and

   3) X1, ..., Xn for all n > 0, are the arguments of the compund
      term.

").

compound(T) :-
        nonvar(T),
        functor(T, _, A), A > 0.

:- comment(sub_atom(Atom, Before, Length, After, Sub_atom), "Is true
   iff atom @var{Atom} can be broken into three pieces, @var{AtomL},
   @var{Sub_atom} and @var{AtomR} such that @var{Before} is the number
   of characters of the name of @var{Atom} & @var{Length} is the
   number of characters of the name of @var{Sub_atom} and @var{After}
   is the number of characters of the name of @var{AtomR}").

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

:- comment(unify_with_occurs_check(X, Y), "Attempts to compute and
   apply a most general unifier of the two terms @var{X} and @var{Y}.
   Is true iff @var{X} and @var{Y} are unifiable.").

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

:- comment(version_maintenance,dir('../version')).

:- comment(version(1*11+140,2003/12/31,11:50*14+'CET'), "Added
   documetation.  (Edison Mera)").

:- comment(version(1*11+139,2003/12/31,11:48*35+'CET'), "Changed
   comment to assertion version control.  (Edison Mera)").

:- comment(version(0*4+5,1998/2/24), "Synchronized file versions with
   global CIAO version.  (Manuel Hermenegildo)").

% -----------------------------------------------------------------------------
