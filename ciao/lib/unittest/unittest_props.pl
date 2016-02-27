:- module(_, _, [assertions]).

:- use_module(library(random)).
:- use_module(library(aggregates)).

:- doc(author, "Edison Mera").

:- doc(module, "This module defines test commands and special
	properties that are random generators.").

:- true prop test_command/1 + sideff(free) # "Defines a unit test
   command.".

:- meta_predicate test_command(goal).

test_command(Goal) :- call(Goal).

:- true prop times/2 + test_command # "The test execution should be
   repeated N times".

:- meta_predicate times(goal, ?).

times(Goal, Times) :-
	Times > 0 ->
	Times1 is Times - 1,
	\+ \+ Goal,
	times(Goal, Times1)
    ;
	true.

:- true prop try_sols/2 + test_command # "The predicate being tested
	will be executed to get at most the specified number of
	solutions.".

:- meta_predicate try_sols(goal, ?).

try_sols(Goal, Sols) :-
	findnsols(Sols, _, Goal, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The following properties are not implemented yet, but are here as a reminder
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- prop user_error/2 # "The predicate should write to the current
   error stream the specified string.".

:- impl_defined(user_error/2).

% time_out(G, T) is equivalent to the property resource(G, ub, time, T) -- EMM

:- prop time_out/2 # "The test should finish in less than the
   specified time.  The time is given in milliseconds.".

:- impl_defined(time_out/2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicates that can be used inside a test to define the values of variables
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- prop bound_random/3 # "Similar to the predicate random:random/3".

bound_random(A, B, C) :-
	random(A, B, C).

:- prop float_random/1 # "Similar to the predicate random:random/1".

float_random(A) :-
	random(A).

:- prop exp_float_random/1 # "Generates any floating point random
   number.".

exp_float_random(A) :-
	random(-324.0, 309.0, X0),
	random(0,      1,     HaveSign),
	X1 is 10 ** X0,
	(
	    HaveSign == 0 ->
	    A is - X1
	;
	    A is X1
	).

:- prop exp_float_random_extended/1 # "Generates any floating point
   random number including special cases like infinite, nan or zero
   whith sign.".

exp_float_random_extended(A) :-
	random(0, 10, SpecialNumbers0),
	(
	    SpecialNumbers0 > 5 ->
	    SpecialNumbers = 5
	;
	    SpecialNumbers = SpecialNumbers0
	),
	exp_float_random_extended_sn(SpecialNumbers, A).

exp_float_random_extended_sn(0, 0.Inf).
exp_float_random_extended_sn(1, -0.Inf).
exp_float_random_extended_sn(2, 0.Nan).
exp_float_random_extended_sn(3, 0.0).
exp_float_random_extended_sn(4, -0.0).
exp_float_random_extended_sn(5, A) :-
	exp_float_random(A).

:- prop near(A, B, Eps) # "Verifies that abs(@var{B} -
   @var{A})/(abs(@var{B}) + (@var{A})) =< @var{Eps}.".

near(A, _, _) :-
	var(A),
	!,
	fail.
near(_, B, _) :-
	var(B),
	!,
	fail.
near(A, B, Eps) :-
	num(A),
	num(B),
	!,
	near_num(A, B, Eps).
near(A, B, Eps) :-
	functor(A, F, N),
	functor(B, F, N),
	near_args(N, A, B, Eps).

near_args(0, _, _, _) :- !.
near_args(N, A, B, Eps) :-
	arg(N, A, ArgA),
	arg(N, B, ArgB),
	!,
	near(ArgA, ArgB, Eps),
	N1 is N - 1,
	near_args(N1, A, B, Eps).
near_args(_, _, _, _).

near_num(0.Inf,  0.Inf,  _) :- !.
near_num(-0.Inf, -0.Inf, _) :- !.
near_num(0.Nan,  0.Nan,  _) :- !.
near_num(A,      B,      Eps) :- A =:= 0, !, abs(B) =< Eps.
near_num(A,      B,      Eps) :- B =:= 0, !, abs(A) =< Eps.
near_num(A,      B,      Eps) :-
	2 * abs(B - A) / (abs(A) + abs(B)) =< Eps.
