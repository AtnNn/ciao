:- include(library('clpq/ops')).
:- use_module(library('clpq/clprt')).
:- use_module(library('clpq/nl_eval'),
        [solve_abs/2,solve_mix/4,solve_mult/3,solve_pow/3,solve_trig/3]).
:- load_compilation_module(library('clpq/clptr')).
% :- add_term_trans(translate_hash/2).
:- add_goal_trans(translate_clp/2).
