:- package(det_hook).
:- use_module(library(det_hook(det_hook_rt))).
:- load_compilation_module(library(det_hook(det_hook_tr))).
% note: after persdb_mysql, etc.
:- add_sentence_trans(det_hook_trans/2, 1150).
