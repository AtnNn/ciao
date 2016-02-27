:- module(clpr_meta,[clp_meta/1,clp_entailed/1], [assertions, nativeprops, unittestprops]).

:- use_module(library(clpr(solver_r)), 
        [normalize/4, var_with_def/5, solve_lin/2, solve_ineq_lt/2, solve_ineq_le/2]).

:- include('../clpqr-common/clp_meta.pl').

:- load_test_package(clpr).
:- test clp_meta(Constraint) : (Constraint = (A .>=. B, A .<.B)) + fails.
