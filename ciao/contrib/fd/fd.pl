:- package(fd).
:- push_prolog_flag(unused_pred_warnings, no).
:- use_module(library(fd(fd_rt))).
:- include(library(fd(fd_syntax))).
:- include(library(fd(fd_translation))).

:- load_compilation_module(library(fd(fd_tr))).
:- add_goal_trans(fd_tr/2).
:- pop_prolog_flag(unused_pred_warnings).
