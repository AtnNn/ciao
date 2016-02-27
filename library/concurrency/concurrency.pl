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
        goal_id/1,
        eng_goal_id/1,
        eng_status/0,
%        eng_status1/1,
%       eng_clean/0,
        lock_atom/1,
        unlock_atom/1,
        atom_lock_state/2,
        concurrent/1],
        [assertions, isomodes, engine(metadefs)]).


:- use_module(library(prolog_sys), [new_atom/1]).
:- use_module(engine(internals), [
        module_concat/3,
        term_to_meta/2,
        imports/5,
        '$predicate_property'/3,
        '$define_predicate'/2,
        '$set_property'/2]).

:- comment(title,"Low-level concurrency/multithreading primitives").

:- comment(author,"Manuel Carro").

:- comment(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- impl_defined([
        '$eng_call'/6,
        '$eng_backtrack'/2,
        '$eng_cut'/1,
        '$eng_release'/1,
        '$eng_wait'/1,
        '$eng_kill'/1,
        '$eng_killothers'/0,
        '$eng_self'/2,
        '$eng_status'/0,
        '$eng_status1'/1,
%        eng_clean/0,
        lock_atom/1,
        unlock_atom/1,
        atom_lock_state/2]).

:- meta_predicate(eng_call(goal, ?, ?, ?)).
:- meta_predicate(eng_call(goal, ?, ?)).
 %% :- primitive_meta_predicate('$eng_call'(goal, ?, ?, ?, ?, ?)).

:- meta_predicate concurrent(addmodule).


:- comment(module, "This module provides basic mechanisms for using
concurrency and implementing multi-goal
applications.@cindex{concurrency} It provides a means for arbitrary
goals to be specified to be run in a separate stack set; in that case,
they are assigned a goal identifier with which further accesses (e.g.,
asking for more solutions) to the goal can be made. Additionally, in
some architectures, these goals can be assigned an O.S. thread,
separate from the one which made the initial call, thus providing
concurrency and, in multiprocessors, parallelism capabilities.

As for now, the memory space of the threads (c.f., stack sets) is
separate in the sense that goals are copied to the new stack set, and
bindings of variables are not seen among stack sets which allows
forward and backward execution to proceed independently in each stack
set, at the cost of the initial goal copy.  However, the program space
(including, specially, the @concept{concurrent predicates}) are shared
and seen by all the goals and threads, and should be used as the
primary means of communication and synchronization.  Higer level libraries can be built using these basic blocks.

Additionally, a small set of lock primitives are provided.  Locks are
associated with atom names. Whereas the concurrent database facilities
are enough to implement locks, semaphores, messages, etc., the
predicates implementing atom-based locks are faster than the ones
accessing the concurrent database (but they are less powerful).

").

 %% :- comment(bug, "@bf{This library is being rewritten at the moment.
 %% The Prolog interface herein described may not correspond exactly with
 %% the existing implementation. This note will be removed when the
 %% documentation corresponds to the implementation.}").

:- comment(bug, "Available only for Windows 32 environments and for
architectures implementing POSIX threads.").

:- comment(bug, "Some implementation of threads have a limit on the
total number of threads that can be created by a process.  Thread
creation, in this case, just hangs. A better solution is planned for
the future.").

 %% :- comment(bug, "The thread-related part of the library is not yet
 %% final, an some characteristics may be revised in a near future.  A
 %% higher level library for concurrency and distribution based on the
 %% low-level primitives in this module is being implemented.  ").


:- pred eng_call(+Goal, +EngineCreation, +ThreadCreation, -GoalId) :
callable * atm * atm * int # "Calls @var{Goal} in a new engine (stack
set), possibly using a new thread, and returns a @var{GoalId} to
designate this new goal henceforth.  @var{EngineCreation} can be
either @tt{wait} or @tt{create}; the distinction is not yet
meaningful. @var{ThreadCreation} can be one of @tt{self}, @tt{wait},
or @tt{create}.  In the first case the creating thread is used to
execute @var{Goal}, and thus it has to wait until its first result or
failure.  The call will fail if @var{Goal} fails, and succeed
otherwise.  However, the call will always suceed when a remote thread
is started.  The space and identifiers reclaimed for the thread must
be explicitly deallocated by calling @pred{eng_release/1}.
@var{GoalId}s are unique in each execution of a Ciao Prolog program.".


eng_call(Goal, Eng, Thr, Id):- 
        var(Id),
        Id = '$goal_id'(GoalDescriptorId, _UniqueId),
%        Id = GoalDescriptorId,
        term_to_meta(MetaGoal, Goal),
        '$eng_call'(MetaGoal, Eng, Thr, GoalDescriptorId, _UniqueId, true).

:- pred eng_call(+Goal, +EngineCreation, +ThreadCreation) : callable *
atm * atm # "Similar to @pred{eng_call/4}, but the thread (if
created) and stack areas are automatically released upon success or
failure of the goal.  No @var{GoalId} is provided for further
interaction with the goal.".

eng_call(Goal, Eng, Thr):- 
        term_to_meta(MetaGoal, Goal),
        '$eng_call'(MetaGoal, Eng, Thr, _Id, _UniqueId, fail).

:- pred eng_backtrack(+GoalId, +ThreadCreation): int * atm # "Performs
backtracking on the goal designed by @var{GoalId}.  A new thread can
be used to perform backtracking, according to @var{ThreadCreation}
(same as in @pred{eng_call/4}).  Fails if the goal is backtracked over
by the local thread, and there are no more solutions.  Always succeeds
if executed by a remote thread. The engine is @bf{not} automatically
released up upon failure: @pred{eng_release/1} must be called to that
end.".

eng_backtrack(GoalId, ThreadCreation):-
        GoalId = '$goal_id'(GoalDesc, _UniqueId),
%        GoalId = GoalDesc,
        '$eng_backtrack'(GoalDesc, ThreadCreation).


:- pred eng_cut(+GoalId) : int # "Performs a @em{cut} in the execution
of the goal @var{GoalId}.  The next call to @pred{eng_backtrack/2}
will therefore backtrack all the way and fail.".

eng_cut(GoalId):-
        GoalId = '$goal_id'(GoalDesc, _UniqueId),
%        GoalId = GoalDesc,
        '$eng_cut'(GoalDesc).

:- pred eng_release(+GoalId): int # "Cleans up and releases the engine
executing the goal designed by @var{GoalId}. The engine must be idle,
i.e., currently not exedcuting any goal.  @pred{eng_wait/1} can be
used to ensure this.".

eng_release(GoalId):-
        GoalId = '$goal_id'(GoalDesc, _),
%        GoalId = GoalDesc,
        '$eng_release'(GoalDesc).


:- pred eng_wait(+GoalId) : int # "Waits for the engine executing the
goal denoted by @var{GoalId} to finish the computation (i.e., it has
finished searching for a solution, either with success or failure).".

eng_wait(GoalId):-
        GoalId = '$goal_id'(GoalDesc, _),
%        GoalDesc = GoalId,
        '$eng_wait'(GoalDesc).


:- pred eng_kill(+GoalId): int # "Kills the thread executing
@var{GoalId} (if any), and frees the memory used up by the stack set.
Usually one should wait (@pred{eng_wait/1}) for a goal, and then
release it, but killing the thread explicitly allows recovering from
error states.  A goal cannot kill itself.  This feature should be used
with caution, because there are situations where killing a thread
might render the system in an unstable state.  Threads should
cooperate in their killing, but if the killed thread is blocked in a
I/O operation, or inside an internal critical region, this cooperation
is not possible and the system, although stopped, might very well end
up in a incosistent state.".

eng_kill(GoalId):-
        GoalId = '$goal_id'(GoalDesc, _),
%        GoalId = GoalDesc,
        '$eng_kill'(GoalDesc).


:- pred eng_killothers # "Kills threads and releases stack sets of all
active goals, but the one calling @pred{eng_killothers}.  Again, a
safety measure.  The same cautions as with @pred{eng_kill/1} should be
taken.".

eng_killothers:- '$eng_killothers'.


:- pred eng_status #"Prints to standard output the current status of
the stack sets.".

eng_status:- '$eng_status'.


:- pred eng_self(?GoalId) : int #"@var{GoalId} is unified with the
identifier of the goal within which @pred{eng_self/1} is executed.
@pred{eng_self/1} is deprecated, and eng_goal_id/1 should be used
instead.".

eng_self(GoalId):-
        display(user_error, 
               'eng_self/1 is deprecated: use eng_goal_id/1 instead'),
        nl,
        eng_goal_id(GoalId).

:- pred goal_id(?GoalId) : int #"@var{GoalId} is unified with the
identifier of the goal within which @pred{goal_id/1} is executed.
@pred{goal_id/1} is deprecated, and eng_goal_id/1 should be used
instead.".

goal_id(GoalId):- 
        display(user_error, 
               'goal_id/1 is deprecated: use eng_goal_id/1 instead'),
        eng_self(GoalId).


:- pred eng_goal_id(?GoalId) : int #"@var{GoalId} is unified with the
identifier of the goal within which @pred{eng_goal_id/1} is executed.".

eng_goal_id(GoalId):- 
        GoalId = '$goal_id'(GoalDesc, _UniqueId),
%        GoalId = GoalDesc,
       '$eng_self'(GoalDesc, _UniqueId).



 %% goal_id(UniqueId):-
 %%         GoalId = '$goal_id'(GoalDesc, UniqueId),
 %% %        GoalId = UniqueId,
 %%        '$eng_self'(_GoalDesc, UniqueId).


 %% :- pred eng_clean # "Removes the stack sets not in use at this moment.
 %% This will probably be left to an extended garbage collection in a
 %% future.".

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


:- comment(doinclude, concurrent/1).

:- pred concurrent(+Spec).

:- comment(concurrent(F/A), "The predicate named @var{F} with arity
@var{A} is made @concept{concurrent} in the current module at runtime
(useful for predicate names generated on-the-fly). This difficults a
better compile-time analysis, but in turn offers more flexibility to
applications.  It is also faster for some applications: if several
agents have to share data in a stuctured fashion (e.g., the generator
knows and wants to restrict the data generated to a set of other
threads), a possibility is to use the same concurrent fact and emply a
field within the fact to distinguish the receiver/sender.  This can
cause many threads to access and wait on the same fact, which in turns
can create contention problems.  It is much better to create a new
concurrent fact and to use that new name as a channel to communicate
the different threads.  @pred{concurrent/1} can either be given a
@concept{predicate spec} in the form @var{Name/Arity}, with @var{Name}
and @var{Arity} bound, or to give a value only to @var{Arity}, and let
the system choose a new, unused @var{Name} for the fact.").


concurrent(F/A, Mod) :-
        (
            var(F) ->
            new_atom(F)
        ;
            atomic(F)
        ), 
        number(A), !,
        module_concat(Mod, F, MF),
        functor(Head, MF, A), !,
        asserta_fact(imports(Mod, Mod, F, A, Mod)), % defines/3 in not dynamic
	concurrent1(Head, concurrent/2).

concurrent1(F, Goal) :-
	'$predicate_property'(F, _, Prop), !,
	(   Prop/\1 =:= 1 -> true		% concurrent, xref nondet.c
        ;   functor(F, N, A),
            throw(error(permision_error(modify, static_procedure, N/A), Goal))
	).
concurrent1(F, _) :-
	functor(F, Name, Ar),
	'$define_predicate'(Name/Ar, consult),
	'$set_property'(F, (concurrent)).		% xref indexing.c


:- comment(bug, "Creating many concurrent facts may fill up the atom
table, causing Ciao Prolog to abort.").


:- comment(version_maintenance,dir('../../version')).

%% Note that the "assertions" library needs to be included in order
%% to support ":- comment(...,...)." declarations such as these.
%% These version comment(s) can be moved elsewhere in the file.
%% Subsequent version comments will be placed above the last one
%% inserted.

:- comment(version(1*7+114,2001/07/09,13:20*42+'CEST'), "eng_self/1
   deprecation message corrected (JCF) (Jesus Correas Fernandez)").

:- comment(version(1*7+81,2001/03/28,16:39*55+'CEST'), "Goal IDs are
now unique in each execution (MCL)").

:- comment(version(1*7+80,2001/03/28,13:02*11+'CEST'), "Updated GoalId
to be unique in every execution.  (MCL)").

:- comment(version(1*7+78,2001/03/26,19:25*24+'CEST'), "Documented
repetition of goal identifiers.  (MCL)").

:- comment(version(1*7+9,2000/08/16,12:07*48+'CEST'), "Added
concurrent/1 call to generate dynamically concurrent predicates.
(MCL)").

:- comment(version(1*5+145,2000/05/18,16:13*40+'CEST'), "Concurrency
now in a separate library, with a subdirectory of examples.  (MCL)").

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

