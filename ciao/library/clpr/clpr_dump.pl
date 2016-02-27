:- module(_, [dump_internal/3], [
        dcg,
        'clpr/clpr_src']).

:- include(library('clpqr-common/ops')).

:- use_module(library(clpr(solver_r))).

:- include(library('clpqr-common/clp_dump')).
:- include(library('clpqr-common/fourier_motzkin')).
