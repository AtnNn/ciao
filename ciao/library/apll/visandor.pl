:- module(visandor,
	[
	    mark_start_goal_local/0,
	    mark_start_goal/1,
	    mark_finish_goal_local/0,
	    mark_finish_goal/1,
	    mark_join/0,
	    start_event_trace/0,
	    stop_event_trace/0,
	    save_event_trace/1
        ],
	[assertions, isomodes, foreign_interface]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% VISANDOR PRIMITIVES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- true pred '$mark_start_goal_local'
   + foreign_low(apll_mark_start_goal_local).
:- true pred '$mark_start_goal'(+int)
   + foreign_low(apll_mark_start_goal).
:- true pred '$mark_finish_goal_local'
   + foreign_low(apll_mark_finish_goal_local).
:- true pred '$mark_finish_goal'(+int)
   + foreign_low(apll_mark_finish_goal).
:- true pred '$mark_join'
   + foreign_low(apll_mark_join).
:- true pred '$start_event_trace'
   + foreign_low(apll_start_event_trace).
:- true pred '$stop_event_trace'
   + foreign_low(apll_stop_event_trace).
:- true pred '$save_event_trace'(+int)
   + foreign_low(apll_save_event_trace).


:- pred mark_start_goal_local
     # "Marks the execution of the local goal as started.".

mark_start_goal_local:-
	'$mark_start_goal_local'.

:- pred mark_start_goal(+Handler) :
   int
     # "Marks the execution of the goal as started.".

mark_start_goal(Handler):-
	'$mark_start_goal'(Handler).

:- pred mark_finish_goal_local
     # "Marks the execution of the local goal as finished.".

mark_finish_goal_local:-
	'$mark_finish_goal_local'.

:- pred mark_finish_goal(+Handler) :
   int
     # "Marks the execution of the goal as finished.".

mark_finish_goal(Handler):-
	'$mark_finish_goal'(Handler).

:- pred mark_join
     # "Marks the join of goals.".

mark_join:-
	'$mark_join'.

:- pred start_event_trace
     # "Starts the event trace for VisAndOr.".

start_event_trace:-
	'$start_event_trace'.

:- pred stop_event_trace
     # "Stops the event trace for VisAndOr.".

stop_event_trace:-
	'$stop_event_trace'.

:- pred save_event_trace(+Stream)
     # "Saves the event trace for VisAndOr with stream @var{Stream}
        associated to the file.".

save_event_trace(Stream):-
	'$save_event_trace'(Stream).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- use_foreign_source(apll).
:- use_foreign_source(visandor).

:- extra_compiler_opts(['-g']).
:- extra_linker_opts(['-lpthread']).



