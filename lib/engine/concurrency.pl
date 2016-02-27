:- module(concurrency, [
        eng_call/4,
        eng_call/3,
        eng_backtrack/2,
        eng_cut/1,
        eng_release/1,
        eng_wait/1,
        eng_kill/1,
        eng_killothers/0,
        eng_self/1,
        eng_status/0,
%        eng_clean/0,
%        
 %%         launch_goal/1, launch_goal_shv/1,
 %%         launch_goal/2, launch_goal_shv/2,
 %%         launch_goal/3, launch_goal_shv/3,
 %%         launch_goal/4, launch_goal_shv/4,
 %%         join_goal/1, join_goals/1,
 %%         release_goal/1, release_goals/1,
 %%         join_and_release_goal/1, join_and_release_goals/1,
 %%         backtrack_goal/1,
 %%         kill_goal/1, kill_other_goals/0,
 %%         goal_self/1,
 %%         goal_status/0,
        lock_atom/1,
        unlock_atom/1,
        atom_lock_state/2],
        [assertions, isomodes, .(metadefs)]).

:- comment(title,"Concurrency/multithreading primitives").

:- comment(author,"Manuel Carro").

:- comment(usage, "These predicates are builtin in CIAO, so nothing special
   has to be done to use them.").


%% :- set_prolog_flag(multi_arity_warnings, off).

:- impl_defined([
        eng_call/5,
        eng_backtrack/2,
        eng_cut/1,
        eng_release/1,
        eng_wait/1,
        eng_kill/1,
        eng_killothers/0,
        eng_self/1,
        eng_status/0,
%        eng_clean/0,
%
%        launch_goal_plus/3,
%        join_goal/1,
%        release_goal/1,
%        backtrack_goal/1,
%        kill_goal/1,
%        kill_other_goals/0,
%        goal_self/1,
%        goal_status/0,
        lock_atom/1,
        unlock_atom/1,
        atom_lock_state/2]).

 %% :- meta_predicate(           launch_goal(goal)).
 %% :- meta_predicate(           launch_goal_shv(goal)).
 %% :- primitive_meta_predicate( launch_goal(goal, goal, goal)).
 %% :- primitive_meta_predicate( launch_goal(goal, goal, goal, ?)).
 %% :- primitive_meta_predicate( launch_goal(goal, ?)).
 %% :- primitive_meta_predicate( launch_goal_shv(goal, ?)).



:- primitive_meta_predicate(eng_call(goal, ?, ?, ?)).
:- primitive_meta_predicate(eng_call(goal, ?, ?)).

 %% :- primitive_meta_predicate(launch_goal(goal)).
 %% :- primitive_meta_predicate(launch_goal(goal, ?)).
 %% :- primitive_meta_predicate(launch_goal(goal, goal, goal)).
 %% :- primitive_meta_predicate(launch_goal(goal, goal, goal, ?)).
 %% :- primitive_meta_predicate(launch_goal_shv(goal)).
 %% :- primitive_meta_predicate(launch_goal_shv(goal, ?)).
 %% :- primitive_meta_predicate(launch_goal_shv(goal, goal, goal)).
 %% :- primitive_meta_predicate(launch_goal_shv(goal, goal, goal, ?)).



 %% launch_goal(G):- launch_goal_plus([](G), noshare, []).
 %% launch_goal(G, I):- launch_goal_plus([](G), noshare, I).
 %% launch_goal(G1, G2, G3):- launch_goal_plus([](G1,G2,G3), noshare, []).
 %% launch_goal(G1, G2, G3, I):- launch_goal_plus([](G1,G2,G3), noshare, I).
 %% launch_goal_shv(G):- launch_goal_plus([](G), share, []).
 %% launch_goal_shv(G, I):- launch_goal_plus([](G), share, I).
 %% launch_goal_shv(G1, G2, G3):- launch_goal_plus([](G1,G2,G3), share, []).
 %% launch_goal_shv(G1, G2, G3, I):- launch_goal_plus([](G1,G2,G3), share, I).

 %% eng_wait_and_release(Goal):- 
 %%         eng_wait(Goal),
 %%         eng_release(Goal).
 %% 
 %% eng_mult_wait([]).
 %% eng_mult_wait([G|Gs]):-
 %%         eng_wait(G),
 %%         eng_mult_wait(Gs).
 %% 
 %% release_goals([]).
 %% release_goals([G|Gs]):-
 %%         release_goal(G),
 %%         release_goals(Gs).
 %% 
 %% join_and_release_goals(Gs):-
 %%         join_goals(Gs),
 %%         release_goals(Gs).

:- comment(module, "This module provides basic mechanisms for using
concurrency and implementing multi-goal
applications.@cindex{concurrency} Goals can be specified to be run in
a separate stack set; in that case, they are assigned a goal
identifier with which further accesses (e.g., asking for more
solutions) to the goal can be made. Additionally, in some
architectures, these goals can be assigned an O.S. thread, separate
from the one which made the initial call.  On the architectures for
which we were not able to implement it yet, the original thread
switches to the new stack set, executes the goal there, and returns
after it has completed.

As for now, the memory space of the threads (c.f., stack sets) is
separate in the sense that goals are copied to the new stack set, and
bindings of variables are not seen among stack sets which allows
forward and backward execution to proceed independently in each stack
set, at the cost of the initial goal copy.  However, the program space
(including, specially, the @concept{concurrent predicates}) are shared
and seen by all the goals and threads.

Additionally, a small set of lock-related predicates are provided:
locks are associated with atom names. Whereas the concurrent database
facilities are enough to implement locks, semaphores, messages, etc.,
the predicates implementing atom-based locks are faster than the ones
accessing the concurrent database.

").

:- comment(bug, "Not fully available in all architectures yet.").

:- comment(bug, "Some implementation of threads have a limit on the
total number of threads that can be created by a process (not only
threads active at a time, but threads created at all).  Thread
creation, in this case, just hangs. A better solution is planned for
the future.").

:- comment(bug, "The thread-related part of the library is not yet
final, an some characteristics may be revised in a near future.  A
higher level library for concurrency and distribution based on the
low-level primitives in this module is being implemented.  ").


:- pred eng_call(+Goal, +EngineCreation, +ThreadCreation, -GoalId) :
callable * atm * atm * int # "Calls @var{Goal} in a new engine (stack
set), possibly using a new thread, and returns a @var{GoalId} to
designate this new goal henceforth.  @var{EngineCreation} can be
either @tt{wait} or @tt{create}; the distinction is not yet
meaningful. @var{ThreadCreation} can be one of @tt{self}, @tt{wait}, or
@tt{create}.  In the first case the creating thread is used to execute
@var{Goal}, and thus it has to wait until its first result or failure.
In the other two cases, a new thread for the @var{Goal} is created and
assigned the new engine.  Some details still have to be woked out.
@pred{eng_call/4} always succeeds and executes a copy (with new
variables) of the original goal, so local and remote backtracking and
execution do not affect each other.".

eng_call(Goal, Eng, Thr, Id):- eng_call(Goal, Eng, Thr, Id, true).

:- pred eng_call(+Goal, +EngineCreation, +ThreadCreation) : callable *
atm * atm # "Similar to @pred{eng_call/4}, but the thread (if
created) and stack areas are automatically released upon success or
failure of the goal.  No @var{GoalId} is provided for further
interaction with the goal.".

eng_call(Goal, Eng, Thr):- eng_call(Goal, Eng, Thr, _Id, fail).

:- pred eng_backtrack(+GoalId, +ThreadCreation): int * atm # "Performs
backtracking on the goal designed by @var{GoalId}, maybe using a new
thread, according to @var{ThreadCreation} (same as in
@pred{eng_call/4}).  Fails if the goal is backtracked over by the
local thread, and there are no more solutions.  Always succeeds if
executed by a remote thread. The engine is automatically released and
cleaned up upon failure.".

:- pred eng_cut(+GoalId) : int # "Performs a @em{cut} in the execution
of the goal @var{GoalId}.  The next call to @pred{eng_backtrack/2}
will therefore fail.".

:- pred eng_release(+GoalId): int # "Cleans up and releases the engine
executing the goal designed by @var{GoalId}. The engine must be in
@tt{WAITING} state.".

:- pred eng_wait(+GoalId) : int # "Waits for the engine executing the
goal denoted by @var{GoalId} to be in waiting state (i.e., it has
finished searching for a solution, either with success or failure).".

:- pred eng_kill(+GoalId): int # "Kills the thread executing
@var{GoalId} (if any), and frees the memory used up by the stack set.
Usually one should wait (@pred{eng_wait/1}) for a goal, and then
release it, but killing the thread explicitly allows recovering from
error states, or imposing timeouts to a task.  A goal can kill itself;
use this feature with caution.".

:- pred eng_killothers # "Kills threads and releases stack sets of all
active goals, but the one calling @pred{eng_killothers}.  Again, a
safety measure.".

:- pred eng_status #"Prints to standard output the current status of
the stack sets.".

:- pred eng_self(?GoalId) : int #"@var{GoalId} is unified with the
identifier of the goal within which @pred{eng_self/1} is executed.".

 %% :- pred eng_clean # "Removes the stack sets not in use at this moment.
 %% This will probably be left to an extended garbage collection in a
 %% future.".

 %% :- pred launch_goal(+Goal) : callable # "Launch a copy of @var{Goal}
 %% in a separate thread, detached from the current one.  Variables in the
 %% @var{Goal} actually executed are copies of the one in the parent
 %% thread: changes made by the children to the variables in @var{Goal}
 %% will not be seen by the parent.  Upon completion of @var{Goal}, the
 %% thread exits and the memory space used by it (a.k.a. WAM) is left
 %% available for other threads, which may reuse it at will.  If a WAM is
 %% available, use it; otherwise create a new one.  @pred{launch_goal/1}
 %% always suceeds.".
 %% 
 %% %launch_goal(Goal):- launch_goal(Goal, _).
 %% 
 %% :- pred launch_goal_shv(+Goal) : callable # "Its behavior is identical
 %% to that of @pred{launch_goal/1}, except that changes (remotely) made
 %% to @var{Goal} are (immediately) visible by all threads sharing these
 %% variables.".
 %% 
 %% %launch_goal_shv(Goal):- launch_goal_shv(Goal, _).
 %% 
 %% 
 %% :- pred launch_goal(+Goal, -GoalId) : callable * int # "Launch a copy
 %% of @var{Goal} in a separate execution thread, and associate it with
 %% the identifier @var{GoalId}.  Unlike @pred{launch_goal/1}, upon
 %% completion of @var{Goal}, the worker executing it is not automatically
 %% available for other goals, but it must explicitly released using the
 %% @var{release_goal/1} predicate.  It can also be backtracked over by
 %% using the @var{backtrack_goal/1} predicate.".
 %% 
 %% 
 %% :- pred launch_goal_shv(+Goal, -GoalId) : callable * int #"Its
 %% behavior is identical to that of @pred{launch_goal_shv/1}, except that
 %% changes (remotely) made to @var{Goal} are (immediately) visible by all
 %% threads sharing these variables.".
 %% 
 %% 
 %% % Pack 1st & 2nd arg. into a single structure to force variable
 %% % sharing in the remote copy.
 %% 
 %% :- pred launch_goal(+Goal, +OnSuccess, +OnFailure) : callable *
 %% callable * callable #"Copy @var{Goal}, @var{OnSuccess}, and
 %% @var{OnFailure} to the memory areas of a new worker and execute
 %% @var{Goal} in a separate worker. It uses the available WAMs, or
 %% creates a new one if needed.  If @var{Goal} suceeds, execute
 %% @var{OnSuccess}. If @var{Goal} fails, execute @var{Failure}.
 %% @pred{launch_goal/3} always suceeds.  Upon completion (either with
 %% success or with failure), the memory areas used by it are released and
 %% available to be used by other goal.".
 %% 
 %% 
 %% :- pred launch_goal(+Goal, +OnSuccess, +OnFailure, -GoalId) : callable
 %% * callable * callable * int #"Similar to @pred{launch_goal/3}, but it
 %% returns as well the @var{GoalId} of the goal, and the memory areas
 %% used by it are not implicitly recovered or available for other goals
 %% (use @pred{release_goal} for that).  More solutions can be requested
 %% with @pred{backtrack_goal/1}.".
 %% 
 %% :- pred launch_goal_shv(+Goal, +OnSuccess, +OnFailure) : callable *
 %% callable * callable #"Launch @var{Goal} in a separate worker, sharing
 %% variables with those in the launching worker. Uses the available WAMs,
 %% and creates a new one if needed.  If @var{Goal} suceeds, execute
 %% @var{OnSuccess}. If @var{Goal} and @var{OnSuccess} succeed, execute
 %% @var{Failure}.  @pred{launch_goal/3} always suceeds.  Upon completion
 %% (either exit or failure), the memory areas used by it are released and
 %% available to be used by other goal.".
 %% 
 %% :- pred launch_goal_shv(+Goal, +OnSuccess, +OnFailure, -GoalId) :
 %% callable * callable * callable * int #"Launches @var{Goal} in a separate
 %% worker, sharing variables with those in the launching worker,
 %% returning its identifier in @var{GoalId}. Uses the available WAMs, and
 %% creates a new one if needed.  If @var{Goal} suceeds, execute
 %% @var{OnSuccess}. If @var{Goal} and @var{OnSuccess} succeed, execute
 %% @var{Failure}.  @pred{launch_goal/3} always suceeds.  The memory areas
 %% used by it are not released until explicitly requested by using the
 %% @pred{release_goal/1} predicate.".
 %% 
 %% 
 %% :- pred backtrack_goal(+Goal) : int #"Backtracks on the call
 %% associated to @var{Goal}.  @var{Goal} should have returned at least one
 %% solution and be in waiting state (see @pred{goal_status/0}) for the
 %% predicate to perform its task.  If no more solutions are available,
 %% @pred{backtrack_goal/1} fails.".
 %% 
 %% :- pred release_goal(+Goal) : int #"The memory areas used by
 %% @var{Goal} are released and left available for other goals.  The
 %% @var{Goal} to be released must have been joined (see
 %% @pred{join_goal/1}) previously.".
 %% 
 %% :- pred release_goals(+GoalIdList) : list(int) #"The memory areas
 %% related to every goal in @var{GoalIdList} are released and left
 %% available for other goals.".
 %% 
 %% :- pred join_goal(+GoalId) : int #"Wait for the termination of the
 %% goal @var{GoalId}.".
 %% 
 %% :- pred join_goals(+GoalIdList) : list(int) #"Wait for the termination
 %% of every goal in @var{GoalIdList}.".
 %% 
 %% :- pred join_and_release_goal(+Goal) : int #"Wait for the termination
 %% of @var{Goal} and release the memory areas used to execute it.".
 %% 
 %% :- pred join_and_release_goals(+GoalIdList) : int #"Wait for the
 %% termination of all goals in @var{GoalIdList} and release the memory
 %% areas used to execute them.".
 %% 
 %%  %% :- pred kill_goal(+GoalId) : int #"Stops the execution of the goal
 %%  %% referred to by @var{GoalId} and kills the associated thread. The
 %%  %% memory areas this goal was using remain available for other goals.
 %%  %% The state of the computation being performed by the worker which was
 %%  %% executing @var{GoalId} upon its death is undefined (e.g., contents of
 %%  %% shared variables), although the implementation will try to stop the
 %%  %% worker in a non-inconsistent state.".
 %% 
 %% %% What I mean is that if you just kill a thread, operations such a 
 %% %% writing in the database or creating a structure can be half finished!
 %% %% Accessing to the database or the struct may result then in a havoc.
 %%     
 %% :- pred kill_other_goals #"Kill all goals on execution but the one
 %% from where the call is made.  This is intended as a last resort to
 %% fail to a stable state in case of lockout or fast thread creation.".

 %% :- pred goal_self(?GoalId) : int #"@var{GoalId} is unified with the
 %% worker identifier of the current goal within which @pred{goal_self/1}
 %% is executed.".


 %% :- pred goal_status #"Prints to standard output the current status of
 %% the active goals(s) and sleeping WAM(s).".


:- pred lock_atom(+Atom) : atm #"The @concept{semaphore} associated to
@var{Atom} is accessed; if its value is nonzero, it is atomically
decremented and the execution of this thread proceeds.  Otherwise, the
goal waits until a nonzero value is reached.  The semaphore is then
atomically decremented and the execution of this thread proceeds.".


:- pred unlock_atom(+Atom) : atm #"The @concept{semaphore} associated
to @var{Atom} is atomically incremented.".


:- pred atom_lock_state(+Atom, +Value) : atm * int #"Sets the
semaphore associated to @var{Atom} to @var{Value}.  This is usually
done at the beginning of the execution, but can be executed at any
time.  If not called, semaphore associated to atoms are by default
inited to 1.  It should be used with caution: arbitrary use can
transform programs using locks in a mess of internal relations.  The
change of a semaphore value in a place other than the initialization
stage of a program is @bf{not} among the allowed operations as defined
by Dijkstra @cite{dijkstra-semaphores,ben-ari}.".


:- pred atom_lock_state(+Atom, -Value) : atm * int #"Consults the
@var{Value} of the semaphore associated to @var{Atom}.  Use sparingly
and mainly as a medium to check state correctness.  Not among the
operations on semaphore by Djikstra.".



:- comment(version_maintenance,dir('../../version')).

%% Note that the "assertions" library needs to be included in order
%% to support ":- comment(...,...)." declarations such as these.
%% These version comment(s) can be moved elsewhere in the file.
%% Subsequent version comments will be placed above the last one
%% inserted.

:- comment(version(1*3+118,1999/11/25,19:28*06+'MET'), "Added
eng_call/3, which does not return GoalId and releases the thread and
wam upon failure/first solution (MCL)").

:- comment(version(1*3+116,1999/11/24,19:27*13+'MET'),
"Concurrency/multithreading/multiengine predicates completely
revamped!  (MCL)").

:- comment(version(0*9+33,1999/04/06,12:14*13+'MEST'), "added
bibliographic references (MCL)").

:- comment(version(0*8+34,1998/12/26,12:27*00+'MST'), "Added joining
and releasing of a list of goals; updated comments.  (Manuel Carro)").

:- comment(version(0*7+25,1998/10/22,20:13*20+'MEST'), "Added comments
and interface for releasing and backtracking concurrent goals. (MCL)").

:- comment(version(0*7+20,1998/10/12,16:56*21+'MEST'), "Added
launch_goal_plus/3 calls, with atom [] meaning no goal Id was
requested.  (MCL)").

:- comment(version(0*7+10,1998/09/24,11:37*32+'MEST'), "Devised new
    primitives.  Commented.  Not yet implemented.  (MCL)").

:- comment(version(0*7+7,1998/09/22,14:49*27+'MEST'), "Changed locks
to be general semaphores, inited to one by default.  Added primitives
to initialize the value of locks.  (MCL)").

:- comment(version(0*7+5,1998/09/22,13:22*13+'MEST'), "Added
kill_other_goals/0 (MCL)").

:- comment(version(0*7+4,1998/09/21,12:58*08+'MEST'), "Calls without
IDs rewritten in Prolog; volatile C declaration added to prevent C
compiler outsmarting the programmer by removing references to locks.
(MCL)").

:- comment(version(0*1+3,1998/09/18,12:52*21+'MEST'), "Improved locks
by local spinning in cached copy of lock.  (MCL)").

:- comment(version(0*1+2,1998/09/18,12:50*49+'MEST'), "Discovered and
solved a bug in the threads library of Linux kernel 2.1.116: non-null
thread_id argument NEEDED in thread creation.  (MCL)").

:- comment(version(0*1+1,1998/09/18,12:48*51+'MEST'), "Reorganized
comments, added new predicates.  (MCL)").

