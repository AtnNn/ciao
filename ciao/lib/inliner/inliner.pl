:- package(inliner).
:- load_compilation_module(library(inliner(inliner_tr))).

:- add_sentence_trans(inliner_sentence_tr/3).
:- add_goal_trans(inliner_goal_tr/3).

:- use_package(library(inliner(inliner_ops))).
