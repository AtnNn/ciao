:- package(rtchecks).

:- use_package(assertions).
:- use_package(hiord).
:- use_package(library(inliner(inliner_ops))).
:- new_declaration(rtchecked/0).

:- rtchecked.

:- load_compilation_module(library(rtchecks(rtchecks_tr))).

:- add_sentence_trans(rtchecks_sentence_tr/4).
:- add_goal_trans(rtchecks_goal_tr/3).

:- set_prolog_flag(runtime_checks, yes).
