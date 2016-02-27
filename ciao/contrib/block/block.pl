:- package(block).

:- use_module(library(block(block_rt))).
:- use_module(library(freeze)).

:- op(1150, fx, block).

:- load_compilation_module(library(block(block_tr))).

:- add_sentence_trans(sentence_tr/3).  
:- add_clause_trans(clause_tr/3). 
