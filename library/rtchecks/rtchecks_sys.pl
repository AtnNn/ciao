
:- module(rtchecks_sys,
	[ calls/2,
	  check/1,
	  check/2,
	  check_comp/2,
	  prec/3,
	  postc/2
	],
	[
	]).

:- use_module(library(metaterms), [ instance/2 ]).
:- use_module(library(write), [ write/1 ]).

:- include(library('rtchecks/rtchecks_sys_indep')).
:- include(library('rtchecks/rtchecks_sys_dep_new')).

%% :- meta_predicate calls(goal,goal).
%% :- meta_predicate check(goal).
%% :- meta_predicate check(goal,goal).
%% :- meta_predicate check_comp(goal,goal).
%% :- meta_predicate prec(goal,goal,?).
%% :- meta_predicate postc(goal,goal).

:- multifile callme/1.
:- multifile proves/2.
%% :- meta_predicate proves(goal,goal).
:- multifile disproves/2.
%% :- meta_predicate disproves(goal,goal).

