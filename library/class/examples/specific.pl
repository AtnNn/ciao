%%----------------------------------%%
%% This class provides additional   %%
%% functionality to the generic     %%
%% class.                           %%
%%----------------------------------%%

:- class(specific).

% establish an inheritance relationship with "generic" class.
:- inherit_class(library('class/examples/generic')).

% override inherited datum/1.
% datum/1 is said to be overriden because there are both
% an inherited definition (from "generic" class) and a local one.

:- data datum/1. 
:- inheritable datum/1.

% Extend the public interface inherited from "generic".
% note that set/1 and a_virtual/0 has been also overriden. 
% undo/0 is a new added functionality.

:- export([set/1,undo/0]).

% Code

set(Value) :-
	inherited datum(OldValue),
	!,
	inherited set(Value),
	asserta_fact(datum(OldValue)).


set(Value) :-
	inherited set(Value).

undo :-
        retract_fact(datum(Last)),
	!,
        asserta_fact(inherited(datum(Last))).

undo :-
	retractall_fact(inherited(datum(_))).

% new implementation of a_virtual. This
% implementation will be used from inherited callme/0
% instead of that version defined at "generic".

a_virtual(specific).

% constructor

specific :-
	generic,
	retractall_fact(inherited(datum(_))),
	display(' specific class constructor '),
	nl.

% destructor

destructor :-
	display(' specific class destructor '),
	nl.
