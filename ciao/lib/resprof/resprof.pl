:- package(resprof).

:- use_package(library(resprof(resprof_decl))).
:- use_package(library(resdefs(resources_decl))).
:- use_module(library(resprof(resprof_rt))).
:- load_compilation_module(library(resprof(resprof_tr))).
% note: priority before rescostcenter, resources, and resdefs
:- add_sentence_trans(resprof_sentence_tr/3, 805).
% :- add_goal_trans(resprof_goal_tr/3).
:- initialization((init_resource_usage, fail;true)).

