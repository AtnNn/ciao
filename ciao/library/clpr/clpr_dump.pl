:- module(_, [dump_internal/3], [
		dcg,
		'clpr/clpr_src']).

:- include(library('clpqr-common/ops')).

:- use_module(engine(attributes), [attach_attribute/2, get_attribute/2,
		update_attribute/2, detach_attribute/1]).
:- use_module(library(clpr(solver_r))).

:- include(library('clpqr-common/clp_dump')).
:- include(library('clpqr-common/fourier_motzkin')).
