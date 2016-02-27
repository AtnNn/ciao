:- module(
        hlconc_rt,
        [(&&>)/2, (<&&)/1, (&>)/2, (<&)/1, (&)/1, (&&)/1],
        [assertions]).


:- use_module(library(concurrency)).
:- use_module(library(prolog_sys), [new_atom/1]).


 %%  %% There should be at least an active thread at the beginning ---
 %%  %% otherwise there is no fairness! 
 %% 
 %% :- initialization(ensure_agent).

:- concurrent ident_and_goal/2.     %% Put requests
:- concurrent ident_and_answer/2.   %% Get answers


:- concurrent sleeping_threads/1.   %% Number of threads waiting for a goal.
sleeping_threads(0).

%% Unused by now, but it will surely be helpful at some point.
:- concurrent total_threads/1.   %% Number of threads waiting for a goal.
total_threads(0).


:- meta_predicate(&&(:)).
:- meta_predicate(&&>(:, ?)).
:- meta_predicate(&(:)).
:- meta_predicate(&>(:, ?)).

&&(Goal):- 
        ensure_agent,
        &(Goal).

&(Goal):- 
        assertz_fact(ident_and_goal(_Ident, Goal)).

&>(Goal, Handle):-
        new_atom(Ident),
        Handle = '$handle'(Ident, Goal),
        assertz_fact(ident_and_goal(Ident, Goal)).

 %%         display(asserted(ident_and_goal(Ident, Goal))),
 %%         nl.

&&>(Goal, Handle):-
        ensure_agent,
        &>(Goal, Handle).



 %% Handle first accesses.  There is an IMPORTANT trick here: we first
 %% access the stored Goal, and then we unify it with the original goal
 %% (the one in the handle).  I have found that if we do that otherwise
 %% and the goal has attributes (e.g., using CLP(X)), then the
 %% retract_fact/1 call just hungs.  Do not know yet the profound reason.

<&&(Handle):-
        Handle = '$handle'(Ident, InitialGoal),
        retract_fact(ident_and_answer(Ident, NewGoal)), !,
        display(NewGoal), nl,
        NewGoal = InitialGoal.

<&(Handle):- <&&(Handle).


ensure_agent:-
        (
            current_fact_nb(sleeping_threads(0)) ->
            eng_call(agent, create, create)
        ;
            true
        ).


agent:-
%% A new agent is born: add it to the agent count
        increase_num_of_agents,
%% Wait for a new goal to execute
        retract_fact(ident_and_goal(Ident, Goal)),
        display(picked_up(Ident, Goal)), nl,
%% Once we have it, execute it --- but there is an agent less to execute
        decrease_active_agents,
        once(Goal),
        assertz_fact(ident_and_answer(Ident, Goal)),
        display(put(Ident, Goal)), nl,
%% And now there is again a new agent to run goals
        increase_num_of_agents,
%% Back to get a new goal
        fail.

once(Goal):- call(Goal), !.


increase_num_of_agents:-
        retract_fact(sleeping_threads(NumAg)), 
        NewNumAg is NumAg + 1,
        asserta_fact(sleeping_threads(NewNumAg)),
        retract_fact(total_threads(NumThreads)),
        NewNumThreads is NumThreads + 1,
        asserta_fact(sleeping_threads(NewNumThreads)), !.

decrease_active_agents:-
        retract_fact(sleeping_threads(NumAg)), !,
        NewNumAg is NumAg - 1,
        asserta_fact(sleeping_threads(NewNumAg)).


:- comment(version_maintenance,dir('../../version')).


%% Note that the "assertions" library needs to be included in order
%% to support ":- comment(...,...)." declarations such as these.
%% These version comment(s) can be moved elsewhere in the file.
%% Subsequent version comments will be placed above the last one
%% inserted.

:- comment(version(1*7+4,2000/07/25,18:05*17+'CEST'), "Higher level
concurrency (hlc) started.  No backtracking at the moment.  (MCL)").

