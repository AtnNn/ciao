:- module(
        agents_rt,
        [
	    create_agent/0,
	    create_agents/1,
	    ensure_agents/1,
	    work/1
	],
	[assertions, isomodes]
	 ).

:- use_module(library(system)).
:- include(library(andprolog(andprolog_ops))).
:- use_module(library(andprolog(andprolog_sched))).
%:- include(andprolog_props).

:- use_module(library(andprolog(callh_rt))).

:- use_module(library(apll)).


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


:- pred agent(+int).

:- doc(agent(GS), "The agent performs some work indefinitely,
   based on the goal scheduling defined by @var{GS}.").

agent(GS) :-
	repeat,
	agent_(GS).

agent_(GS) :-
	work(GS),
	agent_(GS).

agent_(GS) :-
	agent_(GS).


:- pred work(+int).

:- doc(work(+GS), "Checks whether there is some backtracking to perform
   over a particular goal or searches for a new parallel goal in the system
   to be executed, based on the goal scheduling defined by
   @var{GS}. Otherwise, the agent will suspend.").

work(GS) :-
	enter_mutex_self,
	read_event(Handler) ->
	(
	    exit_mutex_self,
	    enter_mutex(Handler),
	    goal_toreexecute(Handler) ->
	    (
		set_goal_rem_executing(Handler),
		save_init_execution(Handler),
		exit_mutex(Handler),
		call_handler(Handler)
	    ;
		set_goal_rem_executing(Handler),
		move_execution_top(Handler),
		exit_mutex(Handler),
		fail
	    )
	;
	    find_goal(GS,H) ->
	    (
		exit_mutex_self,
		enter_mutex(H),
		goal_det(H) ->
		(
		    exit_mutex(H),
		    call_det_handler(H),
		    work(GS)
		;
		    save_init_execution(H),
		    exit_mutex(H),
		    call_handler(H)
		)
	    ;
		suspend,
		work(GS)
	    )
        ).

