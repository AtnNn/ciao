:- module(apll,
	[
	    start_thread/1,
	    number_agents/1,
	    push_goal/1,
	    push_goal/3,
	    find_goal/2,
	    find_det_goal/2,
	    goal_available/1,
	    cancellation/1,
	    retrieve_goal/2,
	    goal_det/1,
	    set_goal_det/1,
	    set_goal_nondet/1,
	    goal_not_executed/1,
	    set_goal_not_executed/1,
	    goal_rem_executing/1,
	    set_goal_rem_executing/1,
	    goal_finished/1,
	    set_goal_finished/1,
	    goal_tobacktrack/1,
	    set_goal_tobacktrack/1,
	    goal_toreexecute/1,
	    set_goal_toreexecute/1,
	    goal_failed/1,
	    set_goal_failed/1,
	    goal_cancelled/1,
	    show_handler/1,
	    set_goal_cancelled/1,

	    send_event/1,
	    read_event/1,
	    save_init_execution/1,
	    save_end_execution/1,
	    more_solutions/1,
	    move_execution_top/1,
% 	    move_pointers_down/1,
% 	    metacut_garbage_slots/1,

	    waiting/1,
	    suspend/0,
	    release/1,
	    release_remote/1,
	    release_some_suspended_thread/0,
	    release_all_for_unwinding/0,
	    enter_mutex/1,
	    enter_mutex_self/0,
	    enter_mutex_remote/1,
	    exit_mutex/1,
	    exit_mutex_self/0,
	    exit_mutex_remote/1,

            clean_measures/0,
	    print_measures/0,
	    new_measure/0,
	    not_measure/0,
	    incr_num_local_backtr/0
        ],
	[assertions, isomodes, foreign_interface]).


:- doc(nodoc, assertions).

:- doc(filetype, package).

:- doc(title, "Low-level concurrency primitives for and-parallelism
                   support").

:- doc(author, "Amadeo Casas"||
                   " (@tt{http://www.cs.unm.edu/~amadeo},"||
                   " University of New Mexico)").

:- doc(module, "This module provides basic mechanisms to start threads,
                    wait for their completion, push goals, search for
                    goals, access to locks, etc. Most of these primitives
                    need to refer to an explicit goal and need to use some
                    information related to its state, stored in the data
                    structure @var{Handler}.

                    This primitives allow to efficiently implement at a
                    higher-level different approaches to exploiting
                    independent and-parallelism.").


:- use_module(engine(internals), [term_to_meta/2]).

% :- use_module(visandor).
:- reexport(library(apll(visandor)), _).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOW-LEVEL SUPPORT PRIMITIVES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- true pred initial
   + foreign_low(init).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOW-LEVEL SUPPORT PRIMITIVES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- true pred '$start_thread'(+int)
   + foreign_low(apll_start_thread).
:- true pred '$number_agents'(-int)
   + foreign_low(apll_number_agents).
% :- true pred '$push_goal'(+int)
%    + foreign_low(apll_push_goal).
:- true pred '$push_goal'(+int,+int,?int)
   + foreign_low(apll_push_goal).
:- true pred '$find_goal'(+int,+int,-int)
   + foreign_low(apll_find_goal).
:- true pred '$goal_available'(+int)
   + foreign_low(apll_goal_available).
:- true pred '$cancellation'(+int)
   + foreign_low(apll_cancellation).
:- true pred '$retrieve_goal'(+int,-int)
   + foreign_low(apll_retrieve_goal).
:- true pred '$goal_det'(+int)
   + foreign_low(apll_goal_det).
:- true pred '$set_goal_det'(+int)
   + foreign_low(apll_set_goal_det).
:- true pred '$set_goal_nondet'(+int)
   + foreign_low(apll_set_goal_nondet).
:- true pred '$goal_not_executed'(+int)
   + foreign_low(apll_goal_not_executed).
:- true pred '$set_goal_not_executed'(+int)
   + foreign_low(apll_set_goal_not_executed).
:- true pred '$goal_rem_executing'(+int)
   + foreign_low(apll_goal_rem_executing).
:- true pred '$set_goal_rem_executing'(+int)
   + foreign_low(apll_set_goal_rem_executing).
:- true pred '$goal_finished'(+int)
   + foreign_low(apll_goal_finished).
:- true pred '$set_goal_finished'(+int)
   + foreign_low(apll_set_goal_finished).
:- true pred '$goal_tobacktrack'(+int)
   + foreign_low(apll_goal_tobacktrack).
:- true pred '$set_goal_tobacktrack'(+int)
   + foreign_low(apll_set_goal_tobacktrack).
:- true pred '$goal_toreexecute'(+int)
   + foreign_low(apll_goal_toreexecute).
:- true pred '$set_goal_toreexecute'(+int)
   + foreign_low(apll_set_goal_toreexecute).
:- true pred '$goal_failed'(+int)
   + foreign_low(apll_goal_failed).
:- true pred '$set_goal_failed'(+int)
   + foreign_low(apll_set_goal_failed).
:- true pred '$goal_cancelled'(+int)
   + foreign_low(apll_goal_cancelled).
:- true pred '$show_handler'(+int)
   + foreign_low(apll_show_handler).
:- true pred '$set_goal_cancelled'(+int)
   + foreign_low(apll_set_goal_cancelled).

:- true pred '$send_event'(+int)
   + foreign_low(apll_send_event).
:- true pred '$read_event'(-int)
   + foreign_low(apll_read_event).
:- true pred '$save_init_execution'(+int)
   + foreign_low(apll_save_init_execution).
:- true pred '$save_end_execution'(+int)
   + foreign_low(apll_save_end_execution).
:- true pred '$more_solutions'(+int)
   + foreign_low(apll_more_solutions).
:- true pred '$move_execution_top'(+int)
   + foreign_low(apll_move_execution_top).
% :- true pred '$move_pointers_down'(+int)
%    + foreign_low(apll_move_pointers_down).
% :- true pred '$metacut_garbage_slots'(+int)
%    + foreign_low(apll_metacut_garbage_slots).

:- true pred '$waiting'(+int)
   + foreign_low(apll_waiting).
:- true pred '$suspend'
   + foreign_low(apll_suspend).
:- true pred '$release'(+int)
   + foreign_low(apll_release).
:- true pred '$release_remote'(+int)
   + foreign_low(apll_release_remote).
:- true pred '$release_some_suspended_thread'
   + foreign_low(apll_release_some_suspended_thread).
:- true pred '$release_all_for_unwinding'
   + foreign_low(apll_release_all_for_unwinding).
:- true pred '$enter_mutex'(+int)
   + foreign_low(apll_enter_mutex).
:- true pred '$enter_mutex_self'
   + foreign_low(apll_enter_mutex_self).
:- true pred '$enter_mutex_remote'(+int)
   + foreign_low(apll_enter_mutex_remote).
:- true pred '$exit_mutex'(+int)
   + foreign_low(apll_exit_mutex).
:- true pred '$exit_mutex_self'
   + foreign_low(apll_exit_mutex_self).
:- true pred '$exit_mutex_remote'(+int)
   + foreign_low(apll_exit_mutex_remote).

:- true pred '$clean_measures'
   + foreign_low(apll_clean_measures).
:- true pred '$print_measures'
   + foreign_low(apll_print_measures).
:- true pred '$new_measure'
   + foreign_low(apll_new_measure).
:- true pred '$not_measure'
   + foreign_low(apll_not_measure).
:- true pred '$incr_num_local_backtr'
   + foreign_low(apll_incr_num_local_backtr).


:- meta_predicate(start_thread(goal)).
:- meta_predicate(push_goal(goal,?,?)).

:- pred start_thread(+Goal) :
   callable
     # "Executes @var{Goal} in a new stack set, using a new thread. Used
        herein to create a number of parallel agents.".

start_thread(Goal):-
        term_to_meta(MetaGoal,Goal),
        '$start_thread'(MetaGoal).

:- pred number_agents(-N) :
   int
     # "Returns in @var{N} the number of agents in the system.".

number_agents(N):-
	'$number_agents'(N).

:- pred push_goal(+Goal,+Det,?Handler) :
   int * int * int
     # "Atomically creates the handler (an arbitrary structure in the
        heap) associated to @var{Goal}, described as deterministic or
        not by @var{Det}, and adds to the goal list a pointer to
        @var{Handler}. @var{Det} states whether the goal is
        deterministic or not.".

push_goal(Goal,Det,Handler):-
        '$push_goal'(Goal,Det,Handler).

:- pred push_goal(+Handler) :
   int
     # "Atomically adds a pointer to the particular handler
        @var{Handler} to the goal list in order to reexecute the goal
        associated to it.".

push_goal(Handler):-
        '$push_goal'(_,_,Handler).

:- pred find_goal(+GS,-Handler) :
   int * int
     # "Searches for a goal handler @var{Handler} published in some
       goal list and succeeds if one is found, failing otherwise. The
       access to each goal list is made atomically.".

find_goal(GS,Handler):-
	'$find_goal'(GS,any,Handler).

:- pred find_det_goal(+GS,-Handler) :
   int * int
     # "Searches for a deterministic goal associated to @var{Handler}
       and succeeds if one is found, failing otherwise. The access to
       each goal list is made atomically.".

find_det_goal(GS,Handler):-
	'$find_goal'(GS,det,Handler).

:- pred goal_available(+Handler) :
   int
     # "Succeeds if @var{Handler} is still in the goal list, and fails
        otherwise.".

goal_available(Handler):- 
        '$goal_available'(Handler).

:- pred cancellation(+Handler) :
   int
     # "Frees the memory used by @var{Handler}.".

cancellation(Handler):-
	'$cancellation'(Handler).

:- pred retrieve_goal(+Handler,-Goal) :
   int * int
     # "Returns in @var{Goal} the parallel goal associated to
        @var{Handler}.".

retrieve_goal(Handler,Goal):-
	'$retrieve_goal'(Handler,Goal).

:- pred goal_det(+Handler) :
   int
     # "Succeeds if the the goal associated to @var{Handler} is
        deterministic, and fails otherwise.".

goal_det(Handler):-
	'$goal_det'(Handler).

:- pred set_goal_det(+Handler) :
   int
     # "Marks the goal associated to @var{Handler} as deterministic.".

set_goal_det(Handler):-
	'$set_goal_det'(Handler).

:- pred set_goal_nondet(+Handler) :
   int
     # "Marks the goal associated to @var{Handler} as
        non-deterministic.".

set_goal_nondet(Handler):-
	'$set_goal_nondet'(Handler).

:- pred goal_not_executed(+Handler) :
   int
     # "Succeeds if the goal associated to @var{Handler} has not been
        executed yet, and fails otherwise.".

goal_not_executed(Handler):-
	'$goal_not_executed'(Handler).

:- pred set_goal_not_executed(+Handler) :
   int
     # "Sets the execution of the goal associated to @var{Handler} to
        never executed.".

set_goal_not_executed(Handler):-
	'$set_goal_not_executed'(Handler).

:- pred goal_rem_executing(+Handler) :
   int
     # "Succeeds if the goal associated to @var{Handler} is remotely
        executing, and fails otherwise.".

goal_rem_executing(Handler):-
	'$goal_rem_executing'(Handler).

:- pred set_goal_rem_executing(+Handler) :
   int
     # "Sets the execution of the goal associated to @var{Handler} to
        being remotely executing.".

set_goal_rem_executing(Handler):-
	'$set_goal_rem_executing'(Handler).

:- pred goal_finished(+Handler) :
   int
     # "Succeeds if the execution of the goal associated to
        @var{Handler} has finished, and fails otherwise.".

goal_finished(Handler):-
	'$goal_finished'(Handler).

:- pred set_goal_finished(+Handler) :
   int
     # "Sets the execution of the goal associated to @var{Handler} to
        finished.".

set_goal_finished(Handler):-
	'$set_goal_finished'(Handler).

:- pred goal_tobacktrack(+Handler) :
   int
     # "Succeeds if the execution of the goal associated to
        @var{Handler} has to backtrack, and fails otherwise.".

goal_tobacktrack(Handler):-
	'$goal_tobacktrack'(Handler).

:- pred set_goal_tobacktrack(+Handler) :
   int
     # "Sets the execution of the goal associated to @var{Handler} to
        backtrack.".

set_goal_tobacktrack(Handler):-
	'$set_goal_tobacktrack'(Handler).

:- pred goal_toreexecute(+Handler) :
   int
     # "Succeeds if the execution of the goal associated to
        @var{Handler} has to backtrack, and fails otherwise.".

goal_toreexecute(Handler):-
	'$goal_toreexecute'(Handler).

:- pred set_goal_toreexecute(+Handler) :
   int
     # "Sets the execution of the goal associated to @var{Handler} to
        be reexecuted.".

set_goal_toreexecute(Handler):-
	'$set_goal_toreexecute'(Handler).

:- pred goal_failed(+Handler) :
   int
     # "Succeeds if the execution of the goal associated to
        @var{Handler} has failed, and fails otherwise.".

goal_failed(Handler):-
	'$goal_failed'(Handler).

:- pred set_goal_failed(+Handler) :
   int
     # "Sets the execution of the goal associated to @var{Handler} to
        failed.".

set_goal_failed(Handler):-
	'$set_goal_failed'(Handler).

:- pred show_handler(+Handler) :
   int
     # "Succeeds if the execution of the goal associated to
        @var{Handler} has cancelled, and fails otherwise.".

show_handler(Handler):-
	'$show_handler'(Handler).

:- pred goal_cancelled(+Handler) :
   int
     # "Succeeds if the execution of the goal associated to
        @var{Handler} has cancelled, and fails otherwise.".

goal_cancelled(Handler):-
	'$goal_cancelled'(Handler).

:- pred set_goal_cancelled(+Handler) :
   int
     # "Sets the execution of the goal associated to @var{Handler} to
        cancelled.".

set_goal_cancelled(Handler):-
	'$set_goal_cancelled'(Handler).

:- pred send_event(+Handler) :
   int
     # "Sends the handler to the agent that picked up the goal
        associated to the handler @var{Handler} in order to perform
        backtracking over it.".

send_event(Handler):-
	'$send_event'(Handler).

:- pred read_event(-Handler) :
   int
     # "Succeeds if the event queue of the agent is not empty and
        unifies @var{Handler} with the handler associated to the goal
        to backtrack over.".

read_event(Handler):-
	'$read_event'(Handler).

:- pred save_init_execution(+Handler) :
   int
     # "Saves the choice point that marks the starting point of the
        execution of the parallel goal associated to the handler
        @var{Handler}.".

save_init_execution(Handler):-
	'$save_init_execution'(Handler).

:- pred save_end_execution(+Handler) :
   int
     # "Saves the choice point that marks the final point of the
        execution of the parallel goal associated to the handler
        @var{Handler}.".

save_end_execution(Handler):-
	'$save_end_execution'(Handler).

:- pred more_solutions(+Handler) :
   int
     # "Succeeds whether the goal associated to @var{Handler} has more
        solutions to compute, and fails otherwise.".

more_solutions(Handler):-
	'$more_solutions'(Handler).

:- pred move_execution_top(+Handler) :
   int
     # "Moves the choice point of the goal saved into the handler
        @var{Handler} to the top of the stack.".

move_execution_top(Handler):-
	'$move_execution_top'(Handler).

% :- pred move_pointers_down(+Handler) :
%    int
%      # "Moves the pointer to top of stack to the end of the goal
%         execution associated to @var{Handler}.".

% move_pointers_down(Handler):-
% 	'$move_pointers_down'(Handler).

% :- pred metacut_garbage_slots(+Handler) :
%    int
%      # "Cleans the stack by removing the choice points that are dead
%         under the execution of the parallel goal associated to the
%         handler @var{Handler}.".

% metacut_garbage_slots(Handler):-
% 	'$metacut_garbage_slots'(Handler).

:- pred waiting(+Handler) :
   int
     # "Succeeds when the execution of the publishing agent associated
        to @var{Handler} is suspended, and fails otherwise.".

waiting(Handler):-
	'$waiting'(Handler).

:- pred suspend
     # "Suspends the execution of the current thread.".

suspend:-
	'$suspend'.

:- pred release(+Handler) :
   int
     # "Releases the execution of the publishing agent associated to
        @var{Handler}.".

release(Handler):-
	'$release'(Handler).

:- pred release_remote(+Handler) :
   int
     # "Releases the execution of the agent that picked up the goal
        associated to @var{Handler}.".

release_remote(Handler):-
	'$release_remote'(Handler).

:- pred release_some_suspended_thread
     # "Selects one out of any suspended threads and sends it a signal
        to resume its execution.".

release_some_suspended_thread:-
	'$release_some_suspended_thread'.

:- pred release_all_for_unwinding
     # "Releases the execution of all agents to perform stack
        unwinding.".

release_all_for_unwinding:-
	'$release_all_for_unwinding'.

:- pred enter_mutex(+Handler) :
   int
     # "Attemps to enter into a mutual exclusion to access shared
        variables of the publishing agent associated to
        @var{Handler}.".

enter_mutex(Handler):-
	'$enter_mutex'(Handler).

:- pred enter_mutex_self
     # "The mutual exclusion is to access the shared variables of the
        calling thread.".

enter_mutex_self:-
	'$enter_mutex_self'.

:- pred enter_mutex_remote(+Handler) :
   int
     # "The mutual exclusion is to access the shared variables of the
        remote thread associated to @var{Handler}.".

enter_mutex_remote(Handler):-
	'$enter_mutex_remote'(Handler).

:- pred exit_mutex(+Handler) :
   int
     # "Exits from the mutual exclusion of the publishing agent
        associated to @var{Handler}.".

exit_mutex(Handler):-
	'$exit_mutex'(Handler).

:- pred exit_mutex_self
     # "Exits from the local mutual exclusion.".

exit_mutex_self:-
	'$exit_mutex_self'.

:- pred exit_mutex_remote(+Handler) :
   int
     # "Exits from the mutual exclusion of the remote agent associated
        to @var{Handler}.".

exit_mutex_remote(Handler):-
	'$exit_mutex_remote'(Handler).

:- pred clean_measures
     # "Restarts the statistical measures for nondeterministic
        parallel programs.".

clean_measures:-
	'$clean_measures'.

:- pred print_measures
     # "Prints the value of the statistical measures for
        nondeterministic parallel goals.".

print_measures:-
	'$print_measures'.

:- pred new_measure
     # "Prepares the statistical values for a new execution.".

new_measure:-
	'$new_measure'.

:- pred not_measure
     # "Avoids measuring.".

not_measure:-
	'$not_measure'.

:- pred incr_num_local_backtr
     # "Increments the number of times that backwards execution has
        been performed over nondeterministic parallel goals.".

incr_num_local_backtr:-
	'$incr_num_local_backtr'.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- use_foreign_source(apll).

:- extra_compiler_opts(['-g']).
:- extra_linker_opts(['-lpthread -lm']).

:- initialization(initial).


