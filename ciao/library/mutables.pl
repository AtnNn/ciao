:- module(mutables, [create_mutable/2,
	             get_mutable/2,
		     update_mutable/2,
		     mutable/1], [assertions]).

:- doc(nodoc, assertions).

:- doc(title, "Mutable Terms").
:- doc(author, "R@'{e}my Haemmerl@'{e}").

:- doc(module, "This module provides mutable terms i.e. an abstract
   datatype provides with efficient backtrackable destructive
   assignment. In other words, any destructive assignments are
   transparently undone on backtracking. Modifications that are
   intended to survive backtracking must be done by asserting or
   retracting dynamic program clauses instead. Mutable must be
   prefered to destructive assignment of arbitrary terms using
   @pred{setarg/3} of the module @lib{odd} which does not have safe
   semantics.").

% Example of strange behaviour of setarg/3
% test1(L) :-
%	X=t(_), arg(1, X, T), T=[], L = [_|T], setarg(1, X, L).
% test2(L) :-
%	X=t(_), arg(1, X, T), T=[T], L = [_|T], setarg(1, X, L).
% Totaly differents behaviours in gprolog / swi / ciao


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                          !!! WARNING !!!  
%                ***  setarg/3 has unsafe semantics ***
%
% This version of mutables uses a structure (of principale functor
% '$mutable'/1) to store the imperative data and setarg/3 to assign
% this later. To avoid unsafe semantics, we have to ensure that the
% word modifed by setarg/3 is never a free variable. It is why the
% effective datum is '$'(Value) and not Value.
%
% -- Remy Haemmerle
% 
%           EXPLANATION OF THE PROBLEM AND THE WORK-AROUND
%           ==============================================
%
% The problem is related to the representation of unbound arguments of
% structures in the heap, and the binding order of variables. This is
% an example of ill-behaving code:
%
%   ?- Z=s(X), Y=X, setarg(Z,1,a).
%
% We expect Z to be s(a) and Y=X, but Y changes also to 'a'. Lets see
% the heap representation of the terms after each literal
% execution. We denote the heap by (Addr |-> Value), tagged words by
% _(_), and allow address arithmetic.
%
%  1)  Z = str(Zr), Zr |-> [ s/1, ref(Zr+1) ]
%      X = ref(Zr+1)
%
%  2)  Z = str(Zr), Zr |-> [ s/1, ref(Zr+1) ]
%      X = ref(Zr+1)
%      Y = ref(Yr), Yr |-> [ ref(Zr+1) ] % or Y = ref(Zr+1) if optimized
%
%  3)  Z = str(Zr), Zr |-> [ s/1, atm(a) ]
%      X = ref(Zr+1)
%      Y = ref(Yr), Yr |-> [ ref(Zr+1) ]
%
%      Dereferencing Y gives a/0 as value! (which is not what we want)
%
% A work-around consists in making sure that X does not live in the
% region to be mutated, by introducing some intermediate cells between
% the argument of s/1 and X. For example, it is enough to introduce an
% auxiliary structure such as:
%
%      Z = str(Zr), Zr |-> [ s/1, str(Ar) ]
%                   Ar |-> [ aux/1, ref(Ar+1) ]
%      X = ref(Ar+1)
%
% With this change, at step 3 we will get:
%
%      Z = str(Zr), Zr |-> [ s/1, atm(a) ]
%                   Ar |-> [ aux/1, ref(Ar+1) ]
%      X = ref(Ar+1)
%      Y = ref(Yr), Yr |-> [ ref(Ar+1) ]
%
% That is, Z=s(a), X=Y. Another (cheaper) solution would be to make
% sure that in structures that can be mutated, variables are never
% 'inlined' in the same heap region shared with the structure:
% 
%      Z = str(Zr), Zr |-> [ s/1, ref(Xr) ]
%      X = ref(Xr), Xr |-> [ ref(Xr) ]
%
% But this requires some additional checks in the garbage collection
% to avoid shortening of variable reference chains in those
% cases. Dedicated mutable cells could be a much better solution
% (also, to allow conditional trailing).
%
%                         (written by Jose F. Morales, thanks to Remy)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(odd), [setarg/3]).
:- use_module(engine(attributes), [get_attribute/2, attach_attribute/2]).

:- pred create_mutable(Datum, Mutable) # "Unifies @var{Datum} with a
freshly created mutable term with initial value @var{Datum}.".
 
create_mutable(Datum, X):-
        ( var(X), \+ get_attribute(X, _) -> 
             attach_attribute(X, '$mutable'('$'(Datum))) 
        ; throw(error(uninstantiation_error(X), create_mutable/2-1))
        ). 

:- pred get_mutable(Datum, Mutable) # "Unifies @var{Datum} with the
        current value of the mutable term @var{Mutable}. @var{Mutable}
        must be a mutable term.".

get_mutable(Datum, Mutable):-
	( mutable_(Mutable, Datum_) ->
	    Datum_ = '$mutable'('$'(Datum))
	; throw(error(instanciation_error, get_mutable/2-2))
	). 

:- pred update_mutable(Datum, Mutable) # "Updates the current value of
	 the mutable term @var{Mutable} to become @var{Datum}.
	 @var{Mutable} must be a mutable term.".

update_mutable(Datum, Mutable):-
	( mutable_(Mutable, Datum_) ->
            setarg(1, Datum_, '$'(Datum))
	; throw(error(instantiation_error, update_mutable/2-2))
	). 

:- pred mutable(Term) # "Succeeds if @var{Term} is currently
instantiated to a mutable term.".

mutable(Term) :-
	mutable_(Term, _).

mutable_(Term, Attr):-
	get_attribute(Term, Attr),
	functor(Attr, '$mutable', 1).
