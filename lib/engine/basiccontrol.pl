:- module(basiccontrol, [
        ','/2, ';'/2, '->'/2, !/0,
	% '^'/2, %% Moved to aggregates M.H.
	(\+)/1, if/3,
        true/0, % This cannot change
        fail/0, repeat/0, call/1, /* call/N, */
	srcdbg_spy/6
	],
        [assertions, isomodes, .(metadefs)]).

:- comment(title,"Control constructs/predicates").

:- comment(author,"Daniel Cabeza").
:- comment(author,"Manuel Hermenegildo").

:- comment(module,"This module contains the set of basic control
   predicates, except the predicates dealing with exceptions, which are
   in @ref{Exception handling}.").

:- comment(usage, "These predicates/constructs are builtin in CIAO, so
   nothing special has to be done to use them.  In fact, as they are
   hardwired in some parts of the system, most of them cannot be redefined.").

:- use_module(engine(internals), [undefined_goal/1,term_to_meta/2,
	                          '$meta_call'/1,'$debugger_state'/2]).

:- comment(hide,srcdbg_spy/6).

% Compiled inline -- these are hooks for the interpreter.

:- comment(','(P,Q), "Conjunction (@var{P} @em{and} @var{Q}).").
:- true pred ','(+callable,+callable) + iso.

(X, Y) :- undefined_goal((X, Y)).

:- comment(';'(P,Q), "Disjunction (@var{P} @em{or} @var{Q}).").
:- true pred ';'(+callable,+callable) + iso.

(X;Y) :- undefined_goal((X;Y)).

:- comment('|'/2, "An alias for disjunction (when appearing outside a
   list). The alias is performed when terms are read in.").

:- comment(doinclude, '|'/2). %% Implemented in reader.

:- comment('->'(P,Q), "If @var{P} then @var{Q} else fail, using first
   solution of @var{P} only.  Also, @tt{(}@var{P} @tt{->} @var{Q}
   @tt{;} @var{R}@tt{)}, if @var{P} then @var{Q} else @var{R}, using
   first solution of @var{P} only.  No cuts are allowed in @var{P}.").

:- true pred '->'(+callable,+callable) + iso.

(X->Y) :- undefined_goal((X->Y)).

:- true pred '!'/0 + iso
   # "Commit to any choices taken in the current predicate.". 

!.						% simple enough

%% Moved to aggregates. MH
%% (X^Y) :- undefined_goal((X^Y)).

:- comment(\+(P), "Goal @var{P} is not provable (negation by failure).
   Fails if @var{P} has a solution, and succeeds otherwise.  No cuts are
   allowed in @var{P}.").

:- true pred \+(+callable) + iso.

\+X :- undefined_goal(\+X).

:- comment(if(P,Q,R), "If @var{P} then @var{Q} else @var{R}, exploring
   all solutions of @var{P}.  No cuts are allowed in @var{P}."). 

:- true pred if(+callable,+callable,+callable).

if(P, Q, R) :- undefined_goal(if(P,Q,R)).

% Caught explicitly by compiler and interpreter

:- impl_defined([true/0, fail/0, repeat/0, call/1]).

:- true pred true/0 + iso # "Succeed (noop).".

:- true pred fail/0 + iso # "Fail, backtrack immediately.".

:- true pred repeat/0 + iso # "Generates an infinite sequence of
   backtracking choices.".

:- comment(call(G), "Executes goal @var{G}, restricting the scope of the cuts
        to the execution of @var{G}.  Writing a variable @var{G} in a goal
        position is equivalent.").

:- true pred call(+callable) + iso. 

:- primitive_meta_predicate(call(goal)).

:- comment(doinclude, call/2).

:- comment(call(Pred,Arg1), "There exists a set of builtin predicates of
   the form @pred{call/N} such that execute predicate @var{Pred} given
   arguments @var{Arg1} ... @var{ArgX}, if @var{Pred} has already
   arguments the new ones are added to the start. This predicate can be
   written using the special Ciao syntax @tt{Pred(Arg1,...,ArgX)}.").

 :- true pred call(+callable,?). 

% :- primitive_meta_predicate(call(pred(1),?)).

:- pred srcdbg_spy/6 # "Performing source level debugging, all goals
   are expanded to this. This is currenlty done for all interpreted
   code.".

:- meta_predicate srcdbg_spy(goal,?,?,?,?,?).

srcdbg_spy(_,_,_,_,_,_):-
 	'$debugger_state'(State,State),
 	(  
 	    arg(1,State,trace)
 	;
 	    arg(1,State,debug)
 	),!.

srcdbg_spy(Goal,_,_,_,_,_):-
 	'$debugger_state'(State,State),
 	arg(1,State,off),!,
 	term_to_meta(G,Goal),
	'$meta_call'(G).

% ----------------------------------------------------------------------------

:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*5+13,1999/12/14,13:37*34+'MET'), "Added
   @pred{srcdbg_spy/6} to support source level debugging.  (M. Carlos 
   Rodriguez)").

:- comment(version(1*3+99,1999/11/12,16:28*06+'MET'), "Added call/2 as
   an exported property (into basic_props).  (Francisco Bueno Carrillo)").

% ----------------------------------------------------------------------------

