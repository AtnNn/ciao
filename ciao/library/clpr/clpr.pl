:- package(clpr).
:- include(library('clpqr-common/ops')).
:- use_module(library(clpr(clpr_rt))).
:- use_module(library(clpr(solver_r)), 
        [solve_abs/2,solve_mix/4,solve_mult/3,solve_pow/3,solve_trig/3,
         inf/2,sup/2,minimize/1,maximize/1]).

%% :- use_module(library(clpr(clpr_dump))).
:- multifile dump_constraints/3.

:- use_module(library(clpr(clpr_meta))).

:- load_compilation_module(library(clpr(clprtr))).
% :- add_term_trans(clprtr:translate_hash/2).
:- add_goal_trans(clprtr:translate_clp/2).
