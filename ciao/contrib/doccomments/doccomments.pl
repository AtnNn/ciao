:- package(doccomments).
:- load_compilation_module(library(doccomments(doccomments_tr))).
:- add_sentence_trans(doccomments_sentence/3).

% tell the reader to understand '%!' 
:- set_prolog_flag(doccomments, on).






