:- module(condcomp_tr, [condcomp_sentence/3], []).

:- use_module(library(messages), 
	[error_message/2,error_message/3]).

% ---------------------------------------------------------------------------
% State of the translation (private to the module)

% Defined values
:- data defined/2.
% Stack of states
:- data ststack/2.

% The states for conditional compilation indicate whether we are
% inserting sentences or not:
%
%   'enabled':  inserting
%   'else':     not inserting in this block
%               (but will be in the 'else' block if its condition
%                is true)
%   'disabled': not inserting

% Clean all transformation state for this module
clean(Mod) :-
        retractall_fact(defined(_,Mod)),
        retractall_fact(ststack(_,Mod)).

% Get top stack value
ststack__top(Mod, St0) :-
	% Check only first solution
	current_fact(ststack(St, Mod)), !,
	St = St0. 

% Push a new value in the stack
ststack__push(Mod, St) :-
	asserta_fact(ststack(St, Mod)).

% Pop a value from the stack
ststack__pop(Mod) :-
	retract_fact(ststack(_, Mod)), !.

% The ststack stack is empty
ststack__empty(Mod) :-
	\+ current_fact(ststack(_, Mod)).

% ---------------------------------------------------------------------------
% Translation process

condcomp_sentence(0, [], _Mod) :- !.
condcomp_sentence(end_of_file, end_of_file, Mod) :- !,
	% TODO: missing error messages if conditions are left open!
	( ststack__empty(Mod) ->
	    true
	; error_message("End-of-file found before closing conditional compilation block", [])
	),
	clean(Mod).
condcomp_sentence((:- Decl), [], Mod) :- condcomp_directive(Decl), !,
	condcomp_treat(Decl, Mod).
condcomp_sentence(_, [], Mod) :-
	ststack__top(Mod, St),
	\+ St = enabled,
	!.
% If we are here it means that we are inserting
condcomp_sentence((:- define(Name)), [], Mod) :- !,
	define(Name, Mod).
% Not processed sentences are left untouched
%condcomp_sentence(_,_,_) :- fail.

condcomp_directive(if(_)).
condcomp_directive(elif(_)).
condcomp_directive(else).
condcomp_directive(endif).

condcomp_treat(Decl, Mod) :-
	( Decl = if(Cond) ->
	    open_block(Mod, Cond)
	; alternative(Decl, Cond) ->
	    alt_block(Mod, Cond, Decl)
	; % Decl = endif
	  close_block(Mod, Decl)
	).

% Directives that alternate a block
alternative(elif(Cond), Cond).
alternative(else, true).

% ---------------------------------------------------------------------------
% Open and close blocks

% Open a block and evaluate its condition
open_block(Mod, Cond) :-
	current_state(Mod, St0),
	open_block_0(Mod, St0, Cond).

open_block_0(Mod, St0, Cond) :-
	( St0 = enabled ->
	    cond_value(Cond, Mod, Value),
	    ( Value = true -> St = enabled
	    ; Value = false -> St = else
	    )
	; St = disabled
	),
	ststack__push(Mod, St).

% Open an alternative block and evaluate its condition
alt_block(Mod, Cond, Decl) :-
	current_state(Mod, St0),
	close_block(Mod, Decl),
	alt_state(St0, St1),
	open_block_0(Mod, St1, Cond).

% Close a block
close_block(Mod, Decl) :-
	( ststack__empty(Mod) ->
	    functor(Decl, Decl0, _),
	    error_message("`:- ~w' directive without a previous `:- if'", [Decl0])
	; ststack__pop(Mod)
	).

% Get the current state
current_state(Mod, St) :- ststack__top(Mod, St0), !, St = St0.
current_state(_Mod, enabled).

% Get the state for the alternative else branch
alt_state(else, St) :- !, St = enabled.
alt_state(_, disabled).

% ---------------------------------------------------------------------------
% Evaluation of conditions

% Evaluate Cond and obtain true/0, false/0 or unknown/1
cond_value(Cond, Mod, Value) :-
	catch(cond_value__2(Cond, Mod, Value), cannot_eval(G), bad_cond(G, Mod, Value)).

cond_value__2(Cond, Mod, Value) :-
	( eval_cond(Cond, Mod) ->
	    Value = true
	; Value = false
	).

bad_cond(G, Mod, Value) :-
	Value = false, % consider bad case as false
	error_message("Cannot evaluate conditional compilation goal `~w' at compile time in module `~w'", [G, Mod]).

% Interpreter of conditions
eval_cond(X, _Mod) :- var(X), !,
	throw(cannot_eval(X)).
eval_cond((X;Y), Mod) :-
	( eval_cond(X, Mod) -> true ; eval_cond(Y, Mod) ).
eval_cond((X,Y), Mod) :-
	eval_cond(X, Mod), eval_cond(Y, Mod).
eval_cond((\+ X), Mod) :-
	\+ eval_cond(X, Mod).
eval_cond(true, _Mod) :- !.
eval_cond(fail, _Mod) :- !, fail.
eval_cond(false, _Mod) :- !, fail.
eval_cond(defined(X), Mod) :- atom(X), !,
	defined(X, Mod).
eval_cond(current_prolog_flag(Flag, Value), _Mod) :- !,
	current_prolog_flag(Flag, Value).
eval_cond(X, _Mod) :-
	throw(cannot_eval(X)).

define(Name, Mod) :-
	current_fact(defined(Name, Mod)), !.
define(Name, Mod) :-
	asserta_fact(defined(Name, Mod)).
