:- module(_, _, [assertions]).

:- use_module(library(math(vector))).

:- doc(rss/2, "The residual square sum.").

:- test rss(Residual, RSS) :
	(
	    Residual                    = [-1.841877388349833,
		-6.164058073390942, -4.515536241440712, 1.560836916551507,
		8.273963575107393, -2.680456166744258,
		6.00656902666337, -0.6394416483965236]
	) =>
	(
	    abs(RSS - 176.345479271502) < 0.0000001
	).

rss(Residual, RSS) :-
	vector_multiply(Residual, Residual, RSS).

:- test mrss(RSS, Rows, Cols, MRSS) :
	(
	    RSS = 176.345479271502,
	    Rows = 8,
	    Cols = 3
	) =>
	(
	    MRSS = 35.2690958543004
	).

:- doc(mrss/4, "The Mean of residual square sum.").

mrss(RSS, Rows, Cols, MRSS) :-
	MRSS is RSS / (Rows - Cols).

:- doc(stderror/2, "The standard error.").

stderror(Variance, Error) :-
	vector_sqrt(Variance, Error).

:- doc(tvalue/2, "The relative error.").

tvalue(Error, Vector, TValue) :-
	vector_division(Error, Vector, TValue).

bound_solution(StdError, Z, Solution, BoundSolution) :-
	vector_constant_multiply_addition(StdError, Z, Solution,
	    BoundSolution).
