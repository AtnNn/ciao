:- package(expander).
:- load_compilation_module(library(expander(expander_tr))).

% TODO: probably the priority should be a parameter of this extension
:- add_sentence_trans(expand_sentence/4, 9910).
:- add_clause_trans(expand_clause/4, 9910).
