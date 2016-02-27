:- module(parback_rt, 
	[
	    create_agents/1,
	    ensure_agents/1,
	    parback_exec/2
	],
	[assertions]).

:- use_module(library(apll_parback)).
:- use_module(library(system)).
:- use_module(library(parback(parback_props))).
:- use_module(engine(hiord_rt), ['$meta_call'/1]).
:- use_module(library(odd)).

:- pred goal_scheduling(-atm).
:- doc(goal_scheduling(GS), "Determines the goal scheduling to
   follow by the different agents in the system.").

goal_scheduling(fifo).
% goal_scheduling(lifo).


%%**********************************************************************
%% AGENTS CREATION
%%**********************************************************************


:- pred create_agents(+int).

:- doc(create_agents(N), "Adds @var{N} new agents to the
   system.").

create_agents(0) :- !.
create_agents(N) :-
        N > 0,
        create_agent,
        N1 is N - 1,
        create_agents(N1).


:- pred ensure_agents(+int).

:- doc(ensure_agents(N), "Creates as many agents as necessary to
   ensure @var{N} agents in the system.").

ensure_agents(N) :-
	number_agents(N1),
	(
	    N1 < N ->
	    N2 is N - N1
	;
	    N2 is 0
	),
	create_agents(N2).


:- pred create_agent # "Creates a new agent.".

create_agent :-
	goal_scheduling(GS),
	!,
        start_thread(agent(GS)).


%%**********************************************************************
%%**********************************************************************
%%**********************************************************************
%%**********************************************************************


:- pred agent(+int).

:- doc(agent(GS), "The agent performs some work indefinitely,
   based on the goal scheduling defined by @var{GS}.").

agent(GS) :-
 %% 	pause(1),
	work(GS),
	agent(GS).

agent(GS) :-
	agent(GS).


:- pred work(+int).

:- doc(work(+GS), "Checks whether there is some backtracking to
   perform over a particular goal or searches for a new parallel goal
   in the system to be executed, based on the goal scheduling defined
   by @var{GS}. Otherwise, the agent will suspend.").

work(GS) :-
	(
	    get_handler_to_executed(GS,Handler) ->
	    ( 
		goal_not_executed(Handler) ->
		set_goal_executing(Handler),
		save_init_execution(Handler),
		exit_mutex_self,
		call_handler(Handler)
	    ;
		set_goal_executing(Handler),
		exit_mutex_self,
		move_execution_top(Handler),
		fail
	    )
	;
	    suspend,
	    work(GS)
	).


%%**********************************************************************
%%**********************************************************************
%%**********************************************************************
%%**********************************************************************


:- pred call_handler(+handler).

:- doc(call_handler(+Handler), "Calls the parallel goal associated
   to @var{Handler}.").

call_handler(Handler) :-
	retrieve_goal(Handler,Goal),
	'$meta_call'(Goal),
	enter_mutex_self,
	new_answer(Handler),
	save_end_execution(Handler),
	set_goal_answer_found(Handler),
	release(Handler),
	exit_mutex_self.
call_handler(Handler) :-
	%meter el enter_mutex-exit_mutex en set_goal_failed
	enter_mutex_self,
	set_goal_failed(Handler),
	set_no_more_backtracking(Handler),
	release(Handler),
	exit_mutex_self,
	fail.

call_local_handler(Handler,Goal) :-
	set_goal_executing(Handler),
	save_init_execution(Handler),
	exit_mutex_self,
	'$meta_call'(Goal),
	enter_mutex_self,
	new_answer(Handler),
	save_end_execution(Handler),
	set_goal_answer_found(Handler).

call_local_handler(Handler,_Goal) :-
	%meter el enter_mutex-exit_mutex en set_goal_failed
	enter_mutex_self,
	set_goal_failed(Handler),
	exit_mutex_self,
	fail.


%%**********************************************************************
%%**********************************************************************
%%**********************************************************************
%%**********************************************************************


:- pred parback_exec(+int,+callable_list).

:- doc(parback_exec(N,GoalList), "Performs a parallel execution of
   the @var{N} goals in @var{GoalList}. If some of the goals has not
   been picked up by another agent then it will be executed by the
   publishing agent.").

parback_exec(N,LGoals) :-
	enter_mutex_self,
	fork(PF,N,LGoals,[Handler|LHandler]),
 %% 	display(fork(PF,N,LGoals,[Handler|LHandler])), nl,
	(
	    goal_not_executed(Handler) ->
	    retrieve_goal(Handler,Goal),
	    call_local_handler(Handler,Goal)
	;
	    true
	),
 %% 	display(va_a_llamar_a_wait_remote),nl,
	wait_for_remote_goals(LHandler),
 %% 	display(sale_wait), nl,
	join(PF),
	exit_mutex_self.

%PENDING:HACER RECORRIDO INVERSO A COMO SE VAN ROBANDO OBJETIVOS!!!
wait_for_remote_goals([]) :- !, true.
wait_for_remote_goals([Handler|LHandler]) :- 
	(
 %% 	    display(en_wait_remote(Handler)),nl,
	    goal_available(Handler) ->
 %% 	    display(is_available(Handler)),nl,
	    retrieve_goal(Handler,Goal),
	    call_local_handler(Handler,Goal)
 %% 	    display(sale_local_call),nl
	;
 %% 	    display(no_is_available(Handler)),nl,
	    exit_mutex_self,
	    wait_not_executing(Handler),
	    enter_mutex_self
	),
 %% 	display(sale_local_callII),nl,
	wait_for_remote_goals(LHandler).

wait_not_executing(Handler) :-
	(
	    enter_mutex_remote(Handler),
	    goal_not_executing(Handler) ->
	    set_no_more_backtracking(Handler),
	    exit_mutex_remote(Handler),
	    true
	;
	    suspend,
	    wait_not_executing(Handler)
	).

