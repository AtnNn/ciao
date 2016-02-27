:- module(callh_rt,
	[
	    call_handler/1,
	    call_det_handler/1
	],
	[assertions, isomodes]).

:- include(library(andprolog(andprolog_ops))).
:- use_module(library(andprolog(andprolog_props))).

:- use_module(library(apll)).
:- use_module(library(odd)).


%%**********************************************************************
%% CALL_HANDLER/1
%%**********************************************************************


:- pred call_handler(+handler).

:- doc(call_handler(+Handler), "Calls the parallel goal associated
   to @var{Handler}.").

call_handler(Handler) :-
	retrieve_goal(Handler,Goal),
 %%   	display(agent_stealing(Handler,Goal)),nl,	
	call(Goal),
 %% 	not_measure,
 %%  	retrieve_goal(Handler,Goal),
 %%     display(agent_new_answer(Handler,Goal)), nl,
	enter_mutex(Handler),
	save_end_execution(Handler),
	set_goal_finished(Handler),
	exit_mutex(Handler),
	release(Handler).
call_handler(Handler) :-
 %%    	display(agent_fail(Handler)), nl,
	enter_mutex(Handler),
	set_goal_failed(Handler),
	exit_mutex(Handler),
	release(Handler),
	fail.


:- pred call_det_handler(+handler).

:- doc(call_det_handler(+Handler), "Calls the deterministic
   parallel goal associated to @var{Handler}.").

call_det_handler(Handler) :-
	retrieve_goal(Handler,Goal),
	(
	    call(Goal) ->
	    enter_mutex(Handler),
	    set_goal_finished(Handler),
	    exit_mutex(Handler),
	    release(Handler)
	;
	    enter_mutex(Handler),
	    set_goal_failed(Handler),
	    exit_mutex(Handler),
	    release(Handler),
	    fail
	).

