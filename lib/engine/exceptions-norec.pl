:- module(exceptions, [
        catch/3, intercept/3, throw/1, halt/0, halt/1, abort/0],
        [assertions, isomodes, .(metadefs)]).

:- use_module(engine(internals),
        ['$exit'/1,'$metachoice'/1,'$meta_call'/1,'$metacut'/1]).

:- comment(title, "Exception handling").

:- comment(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- comment(module, "This module includes predicates related to
   exceptions, which alter the normal flow of Prolog.").

:- primitive_meta_predicate(catch(goal, ?, goal)).
:- primitive_meta_predicate(intercept(goal, ?, goal)).

:- true pred halt + iso.

:- comment(halt, "Halt the system, exiting to the invoking shell.").

halt :- '$exit'(0).

:- true pred halt(+int) + iso.

:- comment(halt(Code), "Halt the system, exiting to the invoking shell,
   returning exit code @var{Code}.").

halt(E) :- integer(E), !, '$exit'(E).
halt(V) :- var(V), !, throw(error(instantiation_error,halt/1-1)).
halt(N) :- throw(error(type_error(integer, N),halt/1-1)).

:- comment(abort, "Abort the current execution.").

abort :- '$exit'(-32768).

%------ errors ------%

:- data catching/3, thrown/1.

:- data serving/1.  %% Implements the stack of exceptions being served

:- true pred catch(+callable,?term,+callable) + iso.

:- comment(catch(Goal, Error, Handler), "Executes @var{Goal}.  If an
   exception is raised during its execution, @var{Error} is unified with
   the exception, and if the unification succeeds, the entire execution
   derived from @var{Goal} is aborted, and @var{Handler} is executed.
   The execution resumes with the continuation of the catch/3 call.  For
   example, given the code
@begin{verbatim}
p(X) :- throw(error), display('---').
p(X) :- display(X).
@end{verbatim}
   the execution of ""@tt{catch(p(0), E, display(E)), display(.), fail.}""
   results in the output ""@tt{error.}"".").

catch(Goal, Error, _) :-
        retractall_fact(serving(Error)),  %% Clear the stack
        '$metachoice'(Choice),
        asserta_catching(Choice, Error, []),
        '$meta_call'(Goal),
        retract_catching(Choice, Error, []).
catch(_, Error, Handler) :-
        retract_fact(thrown(Error)), !,
        '$meta_call'(Handler).

:- true pred intercept(+callable,?term,+callable).

:- comment(intercept(Goal, Error, Handler), "Executes @var{Goal}.  If an
   exception is raised during its execution, @var{Error} is unified with
   the exception, and if the unification succeeds, @var{Handler} is
   executed and then the execution resumes after the predicate which
   produced the exception.  Note the difference with builtin
   @pred{catch/3}, given the same code defined there, the execution of
   ""@tt{intercept(p(0), E, display(E)), display(.), fail.}"" results in
   the output ""@tt{error---.0.}""."). 

%% If reinstalling an intercepted exception, clear the previous stack
%% This is intended to clear the served interruptions stack when doing
%% "hard" aborts.  on_abort/1 does not work: it is called after it was needed.
intercept(Goal, Error, Handler) :-  
        retractall_fact(serving(Error)), %% Clear the stack
        '$metachoice'(Choice),
        asserta_catching(Choice, Error, Handler),
        '$meta_call'(Goal),
        retract_catching(Choice, Error, Handler).

:- true pred throw(nonvar) + iso.

:- comment(throw(Ball), "Raises an error, throwing the exception
   @var{Ball}, to be caught by an ancestor @pred{catch/3} or
   @pred{intercept/3}.  The closest matching ancestor is chosen.
   Exceptions are also thrown by other builtins in case of error.").

throw(Error) :-
        var(Error), !,
        throw(error(instantiation_error, throw/1-1)).
throw(Error) :-
        current_fact(serving(Error)),  %% Are we already serving this?
        display('{Recursive exception '),
        display(Error),
        display(' --- halting}'),
        nl,
        halt.
throw(Error) :-
        current_fact(catching(C, E, H)),
        E = Error, !,
        asserta_fact(serving(E)),      %% We are into it
        throw_action(H, E, C).
throw(Error) :-
        (
            current_fact(serving(Error)) ->  %% We have seen it already
            display('{Recursive exception '),
            display(Error),
            display(' --- halting}'),
            nl
        ;
            asserta_fact(serving(Error)),    %% We are dealing with the error
            display(user_error, '{ERROR: No handle found for thrown error '),
            display(user_error, Error),
            display(user_error, '}'),
            nl(user_error),
            abort              %% Catch & intercept should clear the stack
        ).


throw_action([], Error, Choice) :-
        asserta_fact(thrown(Error)),
        cut_to(Choice), % This cuts also next clause
        fail.
throw_action(Handler, _Error, _) :-
            '$meta_call'(Handler).

cut_to(Choice) :-
        retract_fact(catching(C,_,_)),
        C = Choice,
        '$metacut'(Choice).

asserta_catching(Ch, Er, Ha) :- asserta_fact(catching(Ch, Er, Ha)).
asserta_catching(Ch, Er, Ha) :- retract_fact(catching(Ch, Er, Ha)), fail.

retract_catching(Ch, Er, Ha) :- retract_fact(catching(Ch, Er, Ha)).
retract_catching(Ch, Er, Ha) :- asserta_fact(catching(Ch, Er, Ha)), fail.

:- comment(version_maintenance,dir('../../version')).

