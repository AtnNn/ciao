
:- module(rtchecks_sys,
	[ 
	  true/1,
	  checked/1,
	  trust/1,
	  false/1,
	  check/1,
	  check/2,
	  check_succ/2,
	  check_pred/2,
	  check_comp/2,
 	  calls/2,
 	  prec/3,
 	  c_prec/3,
 	  postc/2
	],
	[
	]).

:- use_module(library(terms_check), [ instance/2 ]).
:- use_module(library(write), [ write/1 ]).

:- include(library('rtchecks/rtchecks_sys_indep')).
:- include(library('rtchecks/rtchecks_sys_indep_pred')).
:- include(library('rtchecks/rtchecks_sys_dep')).

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

