:- package(af).
:- discontiguous '$bfcl'/3, '$bfpred'/1.

:- include(library(bf(ops))).

:- load_compilation_module(library(bf(aftr))).
:- add_sentence_trans(aftr/3).
