:- module(
        disj_wait_rt,
        [
	    '<?'/1,
	    '&?'/1
	],
	[assertions, isomodes]
	 ).

:- include(library(andprolog(andprolog_ops))).
:- use_module(library(andprolog(andprolog_sched))).
:- use_module(library(andprolog(andprolog_props))).

:- use_module(library(apll)).


%%**********************************************************************
%% HIGH-LEVEL PRIMITIVES FOR DISJUNCTIVE WAIT
%%**********************************************************************

:- pred <?(;(+handler,+int)).

:- doc(<?(';'(Handler,Rest)), "Waits until the execution of some
   of the goals associated to the handlers in the disjunction
   (@var{H};@var{Rest}) finishes.").

Disjunction <? :-
	goal_scheduling(GS),
	!,
	enter_mutex_self,
	disjunctive_wait(Disjunction,GS).


:- pred &?(+handler). 

:- doc(&?(Handler), "Succeeds whether the execution of the goal
   associated to the handler @var{Handler} has finished or failed, and
   fails otherwise.").

Handler &? :-
	enter_mutex(Handler),
	(
	    ( goal_finished(Handler) ; goal_failed(Handler) ) ->
	    exit_mutex(Handler)
	;
	    exit_mutex(Handler),
	    fail
	).


%%**********************************************************************
%% AUXILIARY DEFINITIONS
%%**********************************************************************


:- pred disjunctive_wait(+int,+int).

:- doc(disjunctive_wait(+Disjunction,+GS), "Checks whether the
   execution of some goal associated to some of the handlers in the
   disjunction @var{Disjunction} has finished or failed, and searches
   for some other work to do otherwise.").

disjunctive_wait(Disjunction,GS) :-
	(
	    some_goal_has_finished_or_failed(Disjunction) ->
	    exit_mutex_self
	;
	    (
		read_event(H) ->
		exit_mutex_self,
		move_execution_top(H),
		fail
	    ;
% 		(
% 		    find_goal(GS,H) ->
% 		    exit_mutex_self,
% 		    save_init_execution(H),
% 		    call_handler(H),
% 		    enter_mutex_self
% 		;
		    suspend
% 		),
	    ),
	    disjunctive_wait(Disjunction,GS)
	).


:- pred some_goal_has_finished_or_failed(+int).

:- doc(some_goal_has_finished_or_failed(+Disjunction), "Checks
   whether the execution of some goal associated to some of the
   handlers in the disjunction @var{Disjunction} has finished or
   failed.").

some_goal_has_finished_or_failed(Disjunction) :-
	(
	    Disjunction = (Handler ; Rest) ->
	    (
		( goal_finished(Handler) ; goal_failed(Handler) ) -> true
	    ;
		some_goal_has_finished_or_failed(Rest)
	    )
	;
	    Handler = Disjunction,
	    (
		( goal_finished(Handler) ; goal_failed(Handler) ) -> true
	    ;
		fail
	    )
	).

