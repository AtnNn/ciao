:- package(clpq_src).
:- use_module(library(clpq(eval_q))).

:- load_compilation_module(library(clpq(expand_q))). % May be expand_real or expand_rational
:- add_goal_trans(expand_q:expand/2, 750). % TODO: Right priority?

% % A better way is the following --EMM:
% :- use_package(inliner).
% :- use_module(library(clpq(eval_q)), [as_float/2]).
% :- inline_module(library(clpq(eval_q)),
% 	    [arith_zero/1, arith_eval/2, arith_eval/1]).
