:- module(_, [dump_internal/3], [
        dcg,
        'clpq/clpq_src']).

:- include(library('clpqr-common/ops')).

:- use_module(library(clpq(solver_q))).

:- use_module(engine(attributes)).

:- include(library('clpqr-common/clp_dump')).

:- include(library('clpqr-common/fourier_motzkin')).
