:- module(apll_parback,
	[
	    start_thread/1,
	    number_agents/1,
	    get_handler_to_executed/2,
	    goal_available/1,
	    retrieve_goal/2,
	    goal_not_executing/1,
	    set_goal_answer_found/1,
	    set_goal_executing/1,
	    goal_not_executed/1,
	    set_goal_failed/1,
	    set_no_more_backtracking/1,
	    save_init_execution/1,
	    save_end_execution/1,
	    move_execution_top/1,

	    suspend/0,
	    release/1,
	    enter_mutex/1,
	    enter_mutex_self/0,
	    enter_mutex_remote/1,
	    exit_mutex/1,
	    exit_mutex_self/0,
	    exit_mutex_remote/1,

	    fork/4,
	    new_answer/1,
	    join/1

        ],
	[assertions, isomodes, foreign_interface]).


:- doc(nodoc, assertions).

:- doc(filetype, package).

:- doc(title, "Low-level concurrency primitives for and-parallelism
                   support").

:- doc(author, "Amadeo Casas"||
                   " (@tt{http://www.ece.unm.edu/~amadeo},"||
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOW-LEVEL SUPPORT PRIMITIVES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- true pred initial + foreign_low(init_parback).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOW-LEVEL SUPPORT PRIMITIVES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- true pred apll_start_thread(+int)
   + foreign_low(apll_parback_start_thread).
:- true pred number_agents(-int)
   + foreign_low(apll_parback_number_agents).
:- true pred get_handler_to_executed(+int,+int)
   + foreign_low(apll_parback_get_handler_to_executed).
:- true pred goal_available(+int)
   + foreign_low(apll_parback_goal_available).
:- true pred retrieve_goal(+int,-int)
   + foreign_low(apll_parback_retrieve_goal).
:- true pred goal_not_executing(+int)
   + foreign_low(apll_parback_goal_not_executing).
:- true pred set_goal_answer_found(+int)
   + foreign_low(apll_parback_set_goal_answer_found).
:- true pred set_goal_executing(+int)
   + foreign_low(apll_parback_set_goal_executing).
:- true pred goal_not_executed(+int)
   + foreign_low(apll_parback_goal_not_executed).
:- true pred set_goal_failed(+int)
   + foreign_low(apll_parback_set_goal_failed).
:- true pred set_no_more_backtracking(+int)
   + foreign_low(apll_parback_set_no_more_backtracking).
:- true pred save_init_execution(+int)
   + foreign_low(apll_parback_save_init_execution).
:- true pred save_end_execution(+int)
   + foreign_low(apll_parback_save_end_execution).
:- true pred move_execution_top(+int)
   + foreign_low(apll_parback_move_execution_top).

:- true pred suspend
   + foreign_low(apll_parback_suspend).
:- true pred release(+int)
   + foreign_low(apll_parback_release).
:- true pred enter_mutex(+int)
   + foreign_low(apll_parback_enter_mutex).
:- true pred enter_mutex_self
   + foreign_low(apll_parback_enter_mutex_self).
:- true pred enter_mutex_remote(+int)
   + foreign_low(apll_parback_enter_mutex_remote).
:- true pred exit_mutex(+int)
   + foreign_low(apll_parback_exit_mutex).
:- true pred exit_mutex_self
   + foreign_low(apll_parback_exit_mutex_self).
:- true pred exit_mutex_remote(+int)
   + foreign_low(apll_parback_exit_mutex_remote).

:- true pred fork(-int,+int,+int,-int)
   + foreign_low(apll_parback_fork).
:- true pred join(+int)
   + foreign_low(apll_parback_join).
:- true pred new_answer(+int)
   + foreign_low(apll_parback_new_answer).


:- meta_predicate(start_thread(goal)).

:- pred start_thread(+Goal) :
   callable
     # "Executes @var{Goal} in a new stack set, using a new thread. Used
        herein to create a number of parallel agents.".

start_thread(Goal):-
        term_to_meta(MetaGoal,Goal),
        apll_start_thread(MetaGoal).

:- pred number_agents(-N) :
   int
     # "Returns in @var{N} the number of agents in the system.".


:- pred get_handler_to_executed(+GS,-Handler) :
   int * int
     # "Searches for a goal handler @var{Handler} published in some
       goal list or ready to be backtracked and succeeds if one is
       found, failing otherwise.".

:- pred goal_available(+Handler) :
   int
     # "Succeeds if @var{Handler} is still in the goal list, and fails
        otherwise.".

:- pred retrieve_goal(+Handler,-Goal) :
   int * int
     # "Returns in @var{Goal} the parallel goal associated to
        @var{Handler}.".

:- pred goal_not_executing(+Handler) :
   int
     # "Succeeds if the execution of the goal associated to
        @var{Handler} has finished, and fails otherwise.".

:- pred set_goal_answer_found(+Handler) :
   int
     # "Sets the execution of the goal associated to @var{Handler} to
        answer found.".

:- pred set_goal_executing(+Handler) :
   int
     # "Sets the execution stateof the goal associated to 
        @var{Handler} to executing.".

:- pred goal_not_executed(+Handler) :
   int
     # "Succeeds if the execution of the goal associated to
        @var{Handler} has not been stolen to be executed.".

:- pred set_goal_failed(+Handler) :
   int
     # "Sets the execution of the goal associated to @var{Handler} to
        failed.".

:- pred set_no_more_backtracking(+Handler) :
   int
     # "The goal associated to @var{Handler} can not be backtracked any more.".

:- pred save_init_execution(+Handler) :
   int
     # "Saves the choice point that marks the starting point of the
        execution of the parallel goal associated to the handler
        @var{Handler}.".

:- pred save_end_execution(+Handler) :
   int
     # "Saves the choice point that marks the final point of the
        execution of the parallel goal associated to the handler
        @var{Handler}.".

:- pred move_execution_top(+Handler) :
   int
     # "Moves the choice point of the goal saved into the handler
        @var{Handler} to the top of the stack.".

:- pred suspend
     # "Suspends the execution of the current thread.".

:- pred release(+Handler) :
   int
     # "Releases the execution of the publishing agent associated to
        @var{Handler}.".

:- pred enter_mutex(+Handler) :
   int
     # "Attemps to enter into a mutual exclusion to access shared
        variables of the publishing agent associated to
        @var{Handler}.".

:- pred enter_mutex_self
     # "The mutual exclusion is to access the shared variables of the
        calling thread.".

:- pred enter_mutex_remote(+Handler) :
   int
     # "The mutual exclusion is to access the shared variables of the
        remote thread associated to @var{Handler}.".

:- pred exit_mutex(+Handler) :
   int
     # "Exits from the mutual exclusion of the publishing agent
        associated to @var{Handler}.".

:- pred exit_mutex_self
     # "Exits from the local mutual exclusion.".

:- pred exit_mutex_remote(+Handler) :
   int
     # "Exits from the mutual exclusion of the remote agent associated
        to @var{Handler}.".

:- pred fork(+G1,+G2,+G3,+G4) :
   callable * callable * callable * callable
     # "Executes parallel goals.".

:- pred join(+G1) :
   callable
     # "Joins parallel goals.".

:- pred new_answer(+G1) :
   callable
     # "Memorizes the just founded answer.".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- use_foreign_source(apll_parback).

:- extra_compiler_opts(['-g']).
:- extra_linker_opts(['-lpthread -lm']).

:- initialization(initial).


