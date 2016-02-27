:- package(expander).
:- load_compilation_module(library(expander(expander_tr))).

:- add_sentence_trans(expand_sentence/4).
:- add_clause_trans(expand_clause/4).
