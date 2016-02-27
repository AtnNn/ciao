
:- module(jtopl,
	[prolog_server/0
	],
	[assertions,regtypes,isomodes]).

:- use_module(library(concurrency)).

:- comment(title,"Low-level Java to Prolog interface").

:- comment(author,"Jes@'{u}s Correas").

:- comment(module, "
@cindex{Low level Java to Prolog interface}
This module defines a low level Java to Prolog interface. This Prolog side
of the Java to Prolog interface only has one public predicate: a server
that listens at the socket connection with Java, and executes the commands
received from the Java side.

In order to evaluate the goals received from the Java side, this module can
work in two ways: executing them in the same engine, or starting a thread
for each goal. The easiest way is to launch them in the same engine, but
the goals must be evaluated sequentially: once a goal provides the first
solution, all the subsequent goals must be finished before this goal can
backtrack to provide another solution. The Prolog side of this interface
works as a top-level, and the goals partially evaluated are not
independent.

The solution of this goal dependence is to evaluate the goals in a
different prolog engine. Although Ciao includes a mechanism to evaluate
goals in different engines, the approach used in this interface is to
launch each goal in a different thread.

The decision of what kind of goal evaluation is selected is done by the
Java side. Each evaluation type has its own command terms, so the Java side
can choose the type it needs.

A Prolog server starts by calling the @tt{prolog_server/0} predicate. The
user predicates and libraries to be called from Java must be
included in the executable file, or be accesible using the built-in
predicates dealing with code loading.

").

:- use_module(engine(internals)).
:- use_module(library(system)).
:- use_module(library(read),[read/1, read/2]).
:- use_module(library(write),[write/1]).
:- use_module(library(dynamic)).
:- use_module(library(lists),[append/3]).
:- use_module(library(format),[format/2]).
:- use_module(library(dynmods),[use_module/1]).
:- use_module(library(atom2term),[string2term/2]).
:- use_module(javasock).

:- concurrent exception_flag/2.
:- concurrent query_solutions/2.
:- concurrent running_queries/2.

:- comment(doinclude,command/1).
:- comment(doinclude,answer/1).
:- comment(doinclude,prolog_query/1).
:- comment(doinclude,shell_s/0).
:- comment(doinclude,process_first_command/1).
:- comment(doinclude,process_next_command/2).
:- comment(doinclude,solve/1).
:- comment(doinclude,solve_on_thread/1).
:- comment(doinclude,get_query_id/1).
:- comment(doinclude,prolog_parse/2).
:- comment(doinclude,read_command/1).
:- comment(doinclude,write_answer/1).

%%------------------------------------------------------------------
%% REGTYPES
%%------------------------------------------------------------------
:- regtype command(X) # "@var{X} is a command received from the java
	client, to be executed by the Prolog process. The command is
	represented as an atom or a functor with arity 1. The command to be
	executed must be one of the following types:
@cindex{Java commands}
@begin{itemize}

@item @tt{prolog_launch_query(Q)} Compound term to create a new query,
	received as single argument of this structure. A reference to the
	new query is returned to Java.

@item @tt{prolog_launch_query_on_thread(Q)} Compound term to evaluate a new
	query using a separate thread. A reference to the new query is
	returned to Java.

@item @tt{prolog_next_solution(ID)} Compound term to get the next solution
	of a goal identified by the single argument of the structure. A term
	representing the goal instantiated with the next solution is
	returned to Java.

@item @tt{prolog_terminate_query(ID)} Compound term to terminate the goal
	identified by the argument. If the thread option is disabled, a cut
	is made in the goal search tree, and the goal is removed from the
	goal table; if the thread option is enabled, the thread evaluating
	the goal is terminated.

@item @tt{prolog_exit} Atom to terminate the current Prolog process.

@end{itemize}
".

command(prolog_launch_query(Query)) :-
	prolog_query(Query).

command(prolog_launch_query_on_thread(Query)) :-
	prolog_query(Query).
command(prolog_next_solution(_)).
command(prolog_terminate_query(_)).
command(prolog_exit).

:- regtype answer(X) # "@var{X} is a response sent from the prolog
	server. Is represented as an atom or a functor with arity 1 or 2,
	depending on the functor name.
@cindex{Prolog answers}

".

answer(prolog_success).  
answer(prolog_fail).
answer(prolog_solution(X, Y)) :- atom(X), nonvar(Y).  
answer(prolog_query_id(X)) :- int(X).
answer(prolog_exception(X)) :- nonvar(X).
answer(prolog_exception(X,Y)) :- int(X), nonvar(Y).

:- regtype prolog_query(X) # "@var{X} is a query to be launched from the
prolog server.".

prolog_query(Query) :-
	callable(Query).

%----------------------------------------------------------------------------
:- pred prolog_server/0
	# "Prolog server entry point. Reads from the standard
	  input the node name and port number where the java
	  client resides, and starts the prolog server
	  listening at the data socket. This predicate acts
	  as a server: it includes an endless read-process loop
	  until the @tt{prolog_exit} command is received.
@cindex{Prolog server}
".
%----------------------------------------------------------------------------
prolog_server :-
	current_host(Node),
	socket_connection(Node, user_input),
	shell_s.

%----------------------------------------------------------------------------
:- pred shell_s/0 # "Command execution loop. This predicate is called
	when the connection to Java is established, and performs an endless
	loop dealing with the following tasks:

@begin{itemize}

@item reads a command from the socket. All the commands are explained in the
	@tt{command} type description,

@item processes the command using the @tt{process_first_command/1} and
	@tt{process_next_command/1} predicates, and

@item returns the results of the command execution.

@end{itemize}

".
%----------------------------------------------------------------------------
shell_s :-
        read_command(Command),
        process_first_command(Command),
	shell_s.

shell_s :-
	write_answer(prolog_exception(jtopl('Read or process failure.'))),
	shell_s.


%----------------------------------------------------------------------------
:- pred process_first_command(+Command) 
	:: command 
        # "Processes the first command of a query. Using the threads option, it
	processes all the commands received from the prolog server.".
%----------------------------------------------------------------------------
process_first_command(prolog_exit) :- 
	%% Kills all the remaining threads and closes the prolog server.  
        eng_killothers, process_next_command(_,prolog_exit).

process_first_command(prolog_launch_query(Q)) :-
	%% Sets a goal for launching. Launching itself is done when the
        %% first prolog_next_solution command is received.
	get_query_id(Id),
	asserta_fact(running_queries(Id, Q)),
	write_answer(prolog_query_id(Id)).

process_first_command(prolog_next_solution(Id)) :-
	%% Launches a query with no threads, to get the first solution.
        %% Further commands are processed by process_next_command/1 predicate,
        %% until this query has finished or has been terminated.
        current_fact(running_queries(Id, Q)),
	solve(Q).

process_first_command(prolog_launch_query_on_thread(Q)) :-
	%% launches a command on a separate thread.
        %% Further commands are processed by this predicate
	eng_call(solve_on_thread(Q), create, create, Id),
	write_answer(prolog_query_id(Id)).

process_first_command(prolog_next_solution_on_thread(QueryId)) :-
	%% Gets and returns the next solution of the query.
	%% Case a: There are more solutions.
	current_fact(query_solutions(QueryId,Solution)),
	retract_fact(query_solutions(QueryId,Solution)),
	write_answer(prolog_solution(QueryId,Solution)).

process_first_command(prolog_next_solution_on_thread(_QueryId)) :-
	%% Gets and returns the next solution of the query
	%% Case b: There are no more solutions.
	write_answer(prolog_fail).

process_first_command(prolog_terminate_query_on_thread(QueryId)) :-
	%% Terminates the query given as argument.
	%% Case a: the query is still running.
	running_queries(QueryId, _),
	eng_kill(QueryId),
	retractall_fact(running_queries(QueryId, _)),
	retractall_fact(query_solutions(QueryId, _)),
	write_answer(prolog_success).

process_first_command(prolog_terminate_query_on_thread(_QueryId)) :-
	%% Terminates the query given as argument.
	%% Case b: The query is not running.
	write_answer(prolog_success).

process_first_command(prolog_use_module(Module)) :-
	%% Loads a module on this thread.
        intercept(use_module(Module), Error, write_answer(prolog_exception(0,Error))),
	(var(Error) -> write_answer(prolog_success); true).

process_first_command(Command) :-
	%% Any other command throws an exception in client side.
        write_answer(prolog_exception(_, jtopl('unexpected command', Command ))).

%----------------------------------------------------------------------------
:- pred solve(+Query)
        :: prolog_query
        # "Runs the query processing the commands received from the java
           side, and handles query nesting. It is used only with the thread
           option disabled.".
%----------------------------------------------------------------------------
%:- meta_predicate solve(goal).

solve(Query) :-
	callable(Query),
        intercept(Query, Error, set_fact(exception_flag(Id, Error))),
	(current_fact_nb(exception_flag(Id, Error)) ->
	 write_answer(prolog_exception(Id, Error)),
	 retract_fact(exception_flag(Id, _))
	;
	 write_answer(prolog_solution(Id, Query))
	),
        read_command(Next),
        process_next_command(Id, Next).

solve(_Query) :-
        write_answer(prolog_fail).

%----------------------------------------------------------------------------
:- pred solve_on_thread(+Query)
        :: prolog_query
        # "Runs the query on a separate thread and stores the solutions 
           on the @tt{query_solutions/2} data predicate.".
%----------------------------------------------------------------------------
solve_on_thread(Query) :-
	eng_self(Id),
	asserta_fact(running_queries(Id, Query)),
	open_predicate(query_solutions(_, _)),
        intercept(Query, Error, error_handler(Id, Error)),
	(current_fact_nb(exception_flag(Id, Error)) ->
	 assertz_fact(query_solutions(Id, prolog_exception(Id, Error))),
	 retract_fact(exception_flag(Id, _))
	;
	 assertz_fact(query_solutions(Id, Query))
	),
	fail.

solve_on_thread(Query) :-
	eng_self(Id),
	retract_fact(running_queries(Id, Query)),
	close_predicate(query_solutions(_, _)),
	eng_kill(Id).

error_handler(Id, Error) :-
	open_predicate(exception_flag(_,_)),
	set_fact(exception_flag(Id, Error)),
	close_predicate(exception_flag(_,_)).


%% Just to avoid complaints

prolog_query_id(X):- integer(X).  

%----------------------------------------------------------------------------
:- pred get_query_id(-Id)
        :: prolog_query_id
        # "Produces the query id for the next query, when is invoked the
	  threads option disabled.".
%----------------------------------------------------------------------------
get_query_id(Id) :-
	running_queries(Id0, _),
	Id is Id0 + 1.

get_query_id(1).

%----------------------------------------------------------------------------
:- pred process_next_command(+Id, +Command)
        :: prolog_query_id * command
        # "Process the commands received from the prolog server when
	   a query is being handled.".
%----------------------------------------------------------------------------
process_next_command(_Id, prolog_exit) :-
	halt.

process_next_command(Id, prolog_next_solution(Id)) :-
	!,
        fail.

process_next_command(Id, prolog_terminate_query(Id)) :-
	write_answer(prolog_success).

process_next_command(Id, prolog_launch_query(Q)) :-
        solve(Q),
        read_command(Next),
        process_next_command(Id, Next).

process_next_command(_Id, prolog_use_module(Module)) :-
	!,
        intercept(use_module(Module)
		 , Error
		 , write_answer(prolog_exception(0,Error))),
	(var(Error) -> write_answer(prolog_success)),
	fail.

process_next_command(Id, _) :-
        write_answer(prolog_exception(Id, jtopl('unexpected command'))),
	read_command(Next),
	process_next_command(Id, Next).

%----------------------------------------------------------------------------
:- pred prolog_parse(+String, -Term)
        :: string * term
        # "Parses the string received as first argument and returns
	   the prolog term as second argument.
           @bf{Important:} This is a private predicate but could be called
           from java side, to parse strings to Prolog terms.".
%----------------------------------------------------------------------------
prolog_parse(S, Term) :-
	string2term(S, Term).

%----------------------------------------------------------------------------
:- pred write_answer(+Answer)
	:: answer
        # "writes to the output socket stream the given answer.".
%----------------------------------------------------------------------------
write_answer(Answer) :-
	java_fast_write(data,Answer).

%----------------------------------------------------------------------------
:- pred read_command(-Command)
	:: command
        # "Reads from the input stream a new prolog server command.".
%----------------------------------------------------------------------------
read_command(Command) :-
	java_fast_read(data,Command).


%%------------------------------------------------------------------------
%% VERSION CONTROL
%%------------------------------------------------------------------------

:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*5+40,2000/02/08,16:32*42+'CET'), "Interface Documentation. (Jesus Correas Fernandez)").



%%------------------------------------------------------------------------
