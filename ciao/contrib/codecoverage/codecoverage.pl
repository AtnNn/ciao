:- package(codecoverage).
% Author: Santiago Díez Pérez

:- use_module(library(codecoverage(codecoverage_rt)), [
		'$COVER_TERM'/1,
		init_coverage/1
	    ]).



:- load_compilation_module(library(codecoverage(codecoverage_tr))).
:- add_sentence_trans(put_coverpoints/3).
:- add_clause_trans(damm_assertions/3).
