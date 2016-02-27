:- package(bf).
:- discontiguous '$bfcl'/3, '$bfpred'/1.

:- include(library(bf(ops))).

:- load_compilation_module(library(bf(bftr))).
:- add_sentence_trans(bftr/3).
