:- package(id).

:- use_package(hiord).
:- load_compilation_module(library(id(id_tr))).
:- new_declaration(iterative/2, on).
:- add_sentence_trans(id_sentence/3, 750). % TODO: Right priority?
:- add_clause_trans(id_clause/3, 750). % TODO: Right priority?
