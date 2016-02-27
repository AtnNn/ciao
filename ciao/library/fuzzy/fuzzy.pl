:- package(fuzzy).
:- use_package(library(clpr(clpr))).
:- include(library(fuzzy(ops))).
:- use_module(library(fuzzy(faggr))).
:- load_compilation_module(library(fuzzy(fuzzy_tr))).
% note: priority after clpr (750)
% TODO: Not compatible with fsyntax (uses ':=')
:- add_sentence_trans(fuzzy_pred/3, 760).
:- add_clause_trans(fuzzy_pred2/3, 760).

:- aggr min.
:- aggr luka.
:- aggr prod.
:- aggr max.
:- aggr dluka.
:- aggr dprod.

:- new_declaration(is_fuzzy/3,on).
