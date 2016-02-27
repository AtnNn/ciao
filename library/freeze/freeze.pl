:- module(freeze, [freeze/2, frozen/2],[assertions,isomodes,mattr_global,show_trans]).

:- comment(title,"Delaying predicates (freeze)").
:- comment(author,"Manuel Carro").
:- comment(author,"Daniel Cabeza").

:- comment(module,"This library offers a simple implementation of
   @pred{freeze/2}, @pred{frozen/2},
   etc. @cite{Prologii,MU-Prolog,naish:nu-prolog,Carlsson} based on
   the use of attributed variables
   @cite{holzbaur-plilp92,holzbaur-phd}.").

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Implementation based on Holzbauer's examples
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(engine(internals)).

%% :- ensure_loaded(library(attrdecl)).

:- pred freeze(X, Goal) : callable(Goal) 

# "If @var{X} is free delay @var{Goal} until @var{X} is
   non-variable.".

:- meta_predicate freeze(?, goal).
:- meta_predicate frozen(?, goal).

freeze(X, Goal) :-
        set_attr( V, '$frozen_goals'(Goal)),
        X = V.

:- comment(hide,check_attr/2).


check_attr('$frozen_goals'(Goal), _Value):-
        call(Goal).

:- comment(hide,combine_attributes/3).

combine_attributes('$frozen_goals'(G1), '$frozen_goals'(G2) , V):-
        term_to_meta(T1,G1),
        term_to_meta(T2,G2),
        term_to_meta((T1,T2),G),
        set_attr(V, '$frozen_goals'(G)).


:- pred frozen(X, Goal) => callable(Goal) # "@var{Goal} is currently delayed
   until variable @var{X} becomes bound.".

frozen(Var, Goal):-
        get_attr(Var, '$frozen_goals'(Goal)).

% ----------------------------------------------------------------------------
:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*5+72,2000/03/19,19:09*14+'CET'), "@lib{freeze}
   library now partially documented. Library created on Thu Jul 25
   19:23:32 1996. Last previous modification by MCL on Fri Jul 16
   13:32:01 1999. (Manuel Hermenegildo)").

% ----------------------------------------------------------------------------

