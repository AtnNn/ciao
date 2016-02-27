:- module(basiccontrol, [
        ','/2, ';'/2, '->'/2, !/0,
	% '^'/2, %% Moved to aggregates M.H.
	(\+)/1, if/3,
        true/0, % This cannot change
        fail/0, repeat/0,
	false/0, otherwise/0,
	'$metachoice'/1, '$metacut'/1, 
	interpret_goal/2,
	interpret_compiled_goal/2,
	undefined_goal/1,
	debug_goal/1
	],
        [assertions, isomodes]).

:- comment(title,"Control constructs/predicates").

:- comment(author,"Daniel Cabeza").
:- comment(author,"Manuel Hermenegildo").

:- comment(module,"This module contains the set of basic control
   predicates, except the predicates dealing with exceptions, which are
   in @ref{Exception handling}.").

:- comment(usage, "These predicates/constructs are builtin in Ciao, so
   nothing special has to be done to use them.  In fact, as they are
   hardwired in some parts of the system, most of them cannot be redefined.").

:- use_module(engine(internals), [term_to_meta/2,'$unknown'/2,'$current_instance'/5,'$unlock_predicate'/1]).
:- use_module(engine(hiord_rt), ['$meta_call'/1]).
:- use_module(library(debugger), [debug_trace/1]).

% Compiled inline -- these are hooks for the interpreter.

:- comment(','(P,Q), "Conjunction (@var{P} @em{and} @var{Q}).").
:- true pred ','(+callable,+callable) + iso.

:- primitive_meta_predicate(','(goal, goal)).
(X, Y) :- undefined_goal((X, Y)).
%(X, Y) :- '$meta_call'(X), '$meta_call'(Y).

:- comment(';'(P,Q), "Disjunction (@var{P} @em{or} @var{Q}).").
:- true pred ';'(+callable,+callable) + iso.

:- primitive_meta_predicate(';'(goal, goal)).
(X;Y) :- undefined_goal((X;Y)).
%(X;_) :- '$meta_call'(X).
%(_;Y) :- '$meta_call'(Y).

:- comment('|'/2, "An alias for disjunction (when appearing outside a
   list). The alias is performed when terms are read in.").

:- comment(doinclude, '|'/2). %% Implemented in reader.

:- comment('->'(P,Q), "If @var{P} then @var{Q} else fail, using first
   solution of @var{P} only.  Also, @tt{(}@var{P} @tt{->} @var{Q}
   @tt{;} @var{R}@tt{)}, if @var{P} then @var{Q} else @var{R}, using
   first solution of @var{P} only.  No cuts are allowed in @var{P}.").

:- true pred '->'(+callable,+callable) + iso.

:- primitive_meta_predicate('->'(goal, goal)).
(X->Y) :- undefined_goal((X->Y)).

:- true pred '!'/0 + iso
   # "Commit to any choices taken in the current predicate.". 

!.						% simple enough

%% Moved to aggregates. MH
%% (X^Y) :- undefined_goal((X^Y)).

:- comment(\+(P), "Goal @var{P} is not provable (negation by failure).
   Fails if @var{P} has a solution, and succeeds otherwise.  No cuts are
   allowed in @var{P}.").

:- true pred \+(+callable) + ( iso, native(not(X)) ).

:- primitive_meta_predicate(\+(goal)).
\+X :- undefined_goal(\+X).

:- comment(if(P,Q,R), "If @var{P} then @var{Q} else @var{R}, exploring
   all solutions of @var{P}.  No cuts are allowed in @var{P}."). 

:- true pred if(+callable,+callable,+callable).

:- primitive_meta_predicate(if(goal, goal, goal)).
if(P, Q, R) :- undefined_goal(if(P,Q,R)).

:- true pred true/0 + ( iso, native ) # "Succeed (noop).".
:- impl_defined([true/0]).

:- impl_defined([otherwise/0]).

:- true pred fail/0 + ( iso, native ) # "Fail, backtrack immediately.".
:- impl_defined([fail/0]).

:- impl_defined([false/0]).

:- true pred repeat/0 + ( iso, native ) # "Generates an infinite sequence of
   backtracking choices.".
:- impl_defined([repeat/0]).

:- impl_defined(['$metachoice'/1]).
:- impl_defined(['$metacut'/1]).

%------ interpreter ------%

% called from within the emulator

interpret_goal(Head, Root) :-
	'CHOICE IDIOM'(Cut),
	'$current_instance'(Head, Body, Root, _, no_block),
        '$unlock_predicate'(Root),
	metacall(Body, Cut, interpret).

interpret_compiled_goal(Head, _) :- debug_trace(Head).

undefined_goal(X) :- 'CHOICE IDIOM'(Cut), metacall(X, Cut, undefined).

debug_goal(X) :- 'CHOICE IDIOM'(Cut), metacall(X, Cut, debug).

metacall(X, _, _) :-
	var(X), !,
        throw(error(instantiation_error, call/1-1)).
metacall('true', _, _) :- !.
metacall('basiccontrol:true', _, _) :- !.
metacall('fail', _, _) :- !, fail.
metacall('basiccontrol:!', ?, _) :- !,
        message(warning, '! illegal in \\+ or if-parts of ->, if; ignored').
metacall('basiccontrol:!', Cut, _) :- !,
	'CUT IDIOM'(Cut).
metacall('!', ?, _) :- !,
        message(warning, '! illegal in \\+ or if-parts of ->, if; ignored').
metacall('!', Cut, _) :- !,
	'CUT IDIOM'(Cut).
% Can be removed??? - the problem is the cut...
metacall('basiccontrol:,'(X, Y), Cut, _) :- !,
	metacall(X, Cut, interpret),
	metacall(Y, Cut, interpret).
metacall(','(X, Y), Cut, _) :- !,
	metacall(X, Cut, interpret),
	metacall(Y, Cut, interpret).
metacall('basiccontrol:;'('basiccontrol:->'(X,Y),Z), Cut, _) :- !,
	(   metacall(X, ?, interpret) ->
	    metacall(Y, Cut, interpret)
	;   metacall(Z, Cut, interpret)
	).
metacall(';'('basiccontrol:->'(X,Y),Z), Cut, _) :- !,
	(   metacall(X, ?, interpret) ->
	    metacall(Y, Cut, interpret)
	;   metacall(Z, Cut, interpret)
	).
metacall('basiccontrol:;'('->'(X,Y),Z), Cut, _) :- !,
	(   metacall(X, ?, interpret) ->
	    metacall(Y, Cut, interpret)
	;   metacall(Z, Cut, interpret)
	).
metacall(';'('->'(X,Y),Z), Cut, _) :- !,
	(   metacall(X, ?, interpret) ->
	    metacall(Y, Cut, interpret)
	;   metacall(Z, Cut, interpret)
	).
metacall('basiccontrol:->'(X,Y), Cut, _) :- !,
	(   metacall(X, ?, interpret) ->
	    metacall(Y, Cut, interpret)
	).
metacall('->'(X,Y), Cut, _) :- !,
	(   metacall(X, ?, interpret) ->
	    metacall(Y, Cut, interpret)
	).
metacall('basiccontrol:;'(X,Y), Cut, _) :- !,
	(   metacall(X, Cut, interpret)
	;   metacall(Y, Cut, interpret)
	).
metacall(';'(X,Y), Cut, _) :- !,
	(   metacall(X, Cut, interpret)
	;   metacall(Y, Cut, interpret)
	).
metacall('basiccontrol:\\+'(X), _, _) :- !,
	\+ metacall(X, ?, interpret).
metacall('\\+'(X), _, _) :- !,
	\+ metacall(X, ?, interpret).
metacall('basiccontrol:if'(P,Q,R), Cut, _) :- !,
	if(metacall(P, ?, interpret),
	   metacall(Q, Cut, interpret),
	   metacall(R, Cut, interpret)).
metacall('if'(P,Q,R), Cut, _) :- !,
	if(metacall(P, ?, interpret),
	   metacall(Q, Cut, interpret),
	   metacall(R, Cut, interpret)).
% Commented out to solve Jesus's bug (Jesus reported it ;) ) bug... remove these lines if you feel that everything works fine (Dec 10 2003) 
%metacall('aggregates:^'(_,G), Cut, _) :- !,
%	metacall(G, Cut, interpret).
%metacall('^'(_,G), Cut, _) :- !,
%	metacall(G, Cut, interpret).
metacall(X, _, _) :-
	number(X), !,
	throw(error(type_error(callable,X), 'in metacall')).
metacall(X, _, Mode) :-
	metacall2(Mode, X).

metacall2(interpret, X) :- '$meta_call'(X).
metacall2(undefined, X) :- '$unknown'(F, F), do_undefined(F, X).
metacall2(debug, X) :- debug_trace(X).

do_undefined(error, X) :-
        functor(X, F, A),
        throw(error(existence_error(procedure, F/A), F/A)).
do_undefined(warning, X) :-
        message(warning, ['The predicate ', X, ' is undefined']),
        fail.
% do_undefined(fail, X) :- fail.

:- impl_defined(['CHOICE IDIOM'/1]).
:- impl_defined(['CUT IDIOM'/1]).

% ----------------------------------------------------------------------------

:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*11+47,2003/09/25,17:34*08+'CEST'), "Added
   native(not(X)) for \+(X).  (Francisco Bueno Carrillo)").

:- comment(version(1*11+2,2003/04/07,13:45*59+'CEST'), "Included
   interpret_goal/2 and related predicates from internals.pl and
   removed call/1.  (Jose Morales)").

:- comment(version(1*7+37,2001/01/02,16:47*03+'CET'), "Higher-order via
   the call/N builtins is detached to the hiord package.  (Daniel Cabeza
   Gras)").

:- comment(version(1*5+66,2000/03/16,17:03*30+'CET'), "Modified 
   srcdbg_spy pred to improve the performance on source-level debugging.
   (Manuel Carlos Rodriguez)").

:- comment(version(1*5+13,1999/12/14,13:37*34+'MET'), "Added
   @pred{srcdbg_spy/6} to support source level debugging.  (M. Carlos 
   Rodriguez)").

:- comment(version(1*3+99,1999/11/12,16:28*06+'MET'), "Added call/2 as
   an exported property (into basic_props).  (Francisco Bueno Carrillo)").

% ----------------------------------------------------------------------------







