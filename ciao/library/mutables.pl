:- module(mutables, [create_mutable/2,
	             get_mutable/2,
		     update_mutable/2,
		     mutable/1], [assertions]).


:- doc(nodoc, assertions).

:- doc(title, "Mutable Terms").
:- doc(author, "R@'{e}my Haemmerl@'{e}").
:- doc(version(2*0+0, 2010/08/09), "Second implementation").

:- doc(module, "This module provides mutable terms i.e. an
abstract datatype provides with efficient backtrackable destructive
assignment. In other words, any destructive assignments are
transparently undone on backtracking. Modifications that are intended
to survive backtracking must be done by asserting or retracting
dynamic program clauses instead. Mutable must be prefered to
destructive assignment of arbitrary terms using @pred{setarg/3} of the
module @module{odd} which does not have safe semantics.
").

% Example of strange behaviour of setarg/3
% test1(L) :-
%	X=t(_), arg(1, X, T), T=[], L = [_|T], setarg(1, X, L).
% test2(L) :-
%	X=t(_), arg(1, X, T), T=[T], L = [_|T], setarg(1, X, L).
% Totaly differents behaviours in gprolog / swi / ciao


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% !!! WARNING !!!  
%
% This version of mutables uses a structure (of principale functor
% '$mutable'/1) to store the imperative data and setarg/3 to assigne
% this latter. To avoid unsafe semantics, we have to ensure that the
% word modifed by setarg/3 is never a free variable. It is why the
% effective datum is '$'(Value) and not Value.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(odd), [setarg/3]).

:- pred create_mutable(Datum, Mutable) # "Unifies @var{Datum} with a
freshly created mutable term with initial value @var{Datum}.".

create_mutable(Datum, X):-
        (  var(X) -> 
             X = '$mutable'('$'(Datum)) 
        ;
             throw(error(type_error(variable), create_mutable/2-1))
        ). 

:- pred get_mutable(Datum, Mutable) # "Unifies @var{Datum} with the
        current value of the mutable term @var{Mutable}. @var{Mutable}
        must be a mutable term.".

get_mutable(Datum, Mutable):-
	(
            mutable(Mutable)
	->
	    Mutable = '$mutable'('$'(Datum))
	;
	    throw(error(instanciation_error, get_mutable/2-2))
	). 

:- pred update_mutable(Datum, Mutable) # "Updates the current value of
	 the mutable term @var{Mutable} to become @var{Datum}.
	 @var{Mutable} must be a mutable term.".

update_mutable(Datum, Mutable):-
	(
	    mutable(Mutable)
	->
            setarg(1, Mutable, '$'(Datum))
	;
	    throw(error(instantiation_error, update_mutable/2-2))
	). 

:- pred mutable(Term) # "Succeeds if @var{Term} is currently
instantiated to a mutable term.".

mutable(Term):-
    functor(Term, '$mutable', 1).
