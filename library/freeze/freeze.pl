:- module(freeze, [freeze/2, frozen/2],[assertions,isomodes]).

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

:- meta_predicate
        freeze(?, goal),
        frozen(?, goal).

freeze(X, Goal) :-
        attach_attribute( V, '$frozen_goals'(V,Goal)),
        X = V.

:- comment(hide,verify_attribute/2).

:- multifile verify_attribute/2.

verify_attribute('$frozen_goals'(Var, Goal), Value):-
        detach_attribute(Var),
        Var = Value, 
        call(Goal).

:- comment(hide,combine_attributes/2).

:- multifile combine_attributes/2.

combine_attributes('$frozen_goals'(V1, G1), '$frozen_goals'(V2, G2)):-
        detach_attribute(V1),
        detach_attribute(V2),
        V1 = V2,
        term_to_meta(T1,G1),
        term_to_meta(T2,G2),
        term_to_meta((T1,T2),G),
        attach_attribute(V1, '$frozen_goals'(V1, G)).


:- pred frozen(X, Goal) => callable(Goal) # "@var{Goal} is currently delayed
   until variable @var{X} becomes bound.".

frozen(Var, Goal):-
        get_attribute(Var, '$frozen_goals'(_, Goal)).

% ----------------------------------------------------------------------------
:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*5+72,2000/03/19,19:09*14+'CET'), "@lib{freeze}
   library now partially documented. Library created on Thu Jul 25
   19:23:32 1996. Last previous modification by MCL on Fri Jul 16
   13:32:01 1999. (Manuel Hermenegildo)").

% ----------------------------------------------------------------------------

