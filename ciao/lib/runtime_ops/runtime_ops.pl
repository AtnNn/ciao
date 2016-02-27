:- package(runtime_ops).
:- use_module(library(operators), [op/3]).
:- load_compilation_module(library(runtime_ops(runtime_ops_tr))).
:- add_sentence_trans(runtime_op/2).
