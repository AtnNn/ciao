
:- module(jtopl,
	[prolog_server/0,
	 prolog_parse/2
	],
	[assertions,regtypes,isomodes]).

:- comment(title,"Low-level Java to Prolog interface").

:- comment(author,"Jes@'{u}s Correas").

:- comment(module,
	"This module defines a low level Java to Prolog interface.").

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

%%------------------------------------------------------------------
%% REGTYPES
%%------------------------------------------------------------------

:- regtype command(X)
	# "@var{X} is a command received from the java client. Is
          represented as an atom or a functor with arity 1.".
command(prolog_launch_query(Query)) :-
	prolog_query(Query).

command(prolog_launch_query_on_thread(Query)) :-
	prolog_query(Query).
command(prolog_next_solution).
command(prolog_terminate_query).
command(prolog_exit).

:- regtype answer(X)
	# "@var{X} is a response sent from the prolog server. Is
          represented as an atom or a functor.".
answer(prolog_success).
answer(prolog_fail).
answer(prolog_solution(_, _)).
answer(prolog_query_id(X)) :-
	int(X).

:- regtype prolog_query(X)
	# "@var{X} is a query to be launched from the prolog server.".
prolog_query(Query) :-
	callable(Query).

%----------------------------------------------------------------------------
:- pred prolog_server/0
	# "Prolog server entry point. Reads from the standard
	  input the node name and port number where the java
	  client resides, and starts the prolog server
	  listening at the data socket. This predicate acts
	  as a server: includes an infinite read-process loop
	  until the prolog_exit command is received.".
%----------------------------------------------------------------------------
prolog_server :-
	current_host(Node),
	socket_connection(Node, user_input),
	shell_s.

%----------------------------------------------------------------------------
:- pred shell_s/0 # "Starts the prolog server.".
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
        # "Process the first command of a query. With threads, process all
	   the commands received from the prolog server.".
%----------------------------------------------------------------------------
process_first_command(prolog_exit) :-
	%% Kills all the remaining threads and closes the prolog server.
        eng_killothers,
        process_next_command(_, prolog_exit).

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
           side, and handles query nesting. Is used only with no threads.".
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
        # "Runs the query on a separate thread and stores the solutions on the
	   query_solutions/2 data predicate.".
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

%----------------------------------------------------------------------------
:- pred get_query_id(-Id)
        :: prolog_query_id
        # "Produces the query id for the next query, when is invoked the
	  no threads version.".
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
           This is a private predicate but is called from java side.".
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

